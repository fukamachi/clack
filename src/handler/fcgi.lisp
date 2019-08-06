(in-package :cl-user)
(defpackage clack.handler.fcgi
  (:use :cl
        :cl-fastcgi)
  (:import-from :quri
                :url-decode)
  (:import-from :flexi-streams
                :make-in-memory-input-stream
                :string-to-octets)
  (:import-from :usocket
                :stream-server-usocket
                :socket-listen
                :socket-close)
  (:import-from :alexandria
                :make-keyword
                :when-let
                :starts-with-subseq)
  (:export :run))
(in-package :clack.handler.fcgi)

(defclass <fcgi-acceptor> ()
  ((port :type integer
         :initarg :port
         :reader acceptor-port)
   (socket :type (or null 'usocket:stream-server-usocket)
           :initarg :socket
           :initform nil
           :accessor acceptor-socket)
   (file-descriptor :type (or null integer)
                    :initarg :file-descriptor
                    :initform nil
                    :accessor acceptor-file-descriptor)))

(defmethod initialize-instance :after ((acceptor <fcgi-acceptor>) &rest args)
  (declare (ignore args))
  (unless (acceptor-file-descriptor acceptor)
    (let ((socket (usocket:socket-listen "127.0.0.1"
                                         (acceptor-port acceptor)
                                         :reuse-address t
                                         :backlog 128)))
      (setf (acceptor-socket acceptor)
            socket)
      (setf (acceptor-file-descriptor acceptor)
            (cl-fastcgi::usocket-to-fd socket)))))

(defun run (app &key (debug t) (port 9000) fd)
  "Start FastCGI server."
  (flet ((main-loop (req)
           (let* ((env (handle-request req))
                  (res (if debug
                           (restart-case
                               (funcall app env)
                             (throw-internal-server-error ()
                               '(500 () ("Internal Server Error"))))
                           (handler-case (funcall app env)
                             (error (error)
                               (princ error *error-output*)
                               '(500 () ("Internal Server Error")))))))
             (etypecase res
               (list (handle-response req res))
               (function (funcall res (lambda (res) (handle-response req res))))))))
    (let ((acceptor
            (make-instance '<fcgi-acceptor>
                           :port port
                           :file-descriptor fd)))
      (unwind-protect
           (cl-fastcgi::server-on-fd
            #'main-loop
            (acceptor-file-descriptor acceptor))
        (stop acceptor))
      acceptor)))

(defun stop (acceptor)
  (when-let (socket (acceptor-socket acceptor))
    (usocket:socket-close socket)))

(defun handle-response (req res)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body)) res
      (fcgx-puts req (format nil "Status: ~D ~A~%" status (http-status-reason status)))
      (loop for (k v) on headers by #'cddr
            with hash = (make-hash-table :test #'eq)
            if (eq k :set-cookie)
              do (fcgx-puts req (format nil "~:(~A~): ~A~%" k v))
            else if (gethash k hash)
              do (setf (gethash k hash)
                       (concatenate 'string (gethash k hash) ", " v))
            else
              do (setf (gethash k hash) v)
            finally
               (loop for k being the hash-keys in hash
                       using (hash-value v)
                     if v
                       do (fcgx-puts req (format nil "~:(~A~): ~A~%" k v))))
      (fcgx-puts req #.(format nil "~%"))

      (when (eq body no-body)
        (return-from handle-response
          (lambda (body &key (start 0 has-start) (end (length body) has-end) (close nil))
            (etypecase body
              (null)
              (string
               (fcgx-puts req
                          (flex:string-to-octets body
                                                 :start start :end end
                                                 :external-format :utf-8)))
              ((vector (unsigned-byte 8))
               (fcgx-puts req
                          (if (or has-start has-end)
                              (subseq body start end)
                              body))))
            (if close
                (fcgx-finish req)
                (fcgx-flush req)))))

      (prog1
          (etypecase body
            (null) ;; nothing to response
            (pathname
             (with-open-file (in body
                                 :direction :input
                                 :element-type '(unsigned-byte 8)
                                 :if-does-not-exist nil)
               (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                 (read-sequence buf in)
                 (fcgx-puts req buf))))
            (list
             (fcgx-puts req
                        (flex:string-to-octets
                         (format nil "~{~A~}" body)
                         :external-format :utf-8)))
            ((vector (unsigned-byte 8))
             (fcgx-puts req body)))
        (fcgx-finish req)))))

(defun handle-request (req)
  "Convert Request from server into a plist
before passing to Clack application."
  (let ((headers (make-hash-table :test 'equal)))
    (flet ((canonicalize (field &key (start 0) (case :upcase))
             (let* ((end (length field))
                    (new (make-string (- end start))))
               (do ((i start (1+ i))
                    (j 0 (1+ j)))
                   ((= i end) new)
                 (let ((char (aref field i)))
                   (cond
                     ((char= #\_ char)
                      (setf (aref new j) #\-))
                     ((and (eq case :downcase)
                           (upper-case-p char))
                      (setf (aref new j) (code-char (+ (char-code char) 32))))
                     ((and (eq case :upcase)
                           (lower-case-p char))
                      (setf (aref new j) (code-char (- (char-code char) 32))))
                     (T
                      (setf (aref new j) char)))))))
           (set-header (k v)
             (multiple-value-bind (current existsp)
                 (gethash k headers)
               (setf (gethash k headers)
                     (if existsp
                         (format nil "~A, ~A" v current)
                         v)))))
      (loop with request-uri = nil
            for (k . v) in (let ((cffi:*default-foreign-encoding* :latin-1))
                             (fcgx-getenv req))
            if (starts-with-subseq "HTTP_" k)
              do (set-header (canonicalize k :start 5 :case :downcase) v)
            if (or (string= k "SERVER_NAME")
                   (string= k "REMOTE_ADDR")
                   (string= k "CONTENT_TYPE")
                   (string= k "QUERY_STRING"))
              append (list (make-keyword (canonicalize k)) v) into env
            if (string= k "REQUEST_URI")
              append (progn
                       (setf request-uri v)
                       (list (make-keyword (canonicalize k)) v)) into env
            else
              if (or (string= k "SERVER_PORT")
                     (string= k "REMOTE_PORT")
                     (string= k "CONTENT_LENGTH"))
                append (list (make-keyword (canonicalize k))
                             (ignore-errors (parse-integer v :junk-allowed t)))
                  into env
            else
              if (or (string= k "SERVER_PROTOCOL")
                     (string= k "REQUEST_METHOD"))
                append (list (make-keyword (canonicalize k))
                             (make-keyword v))
                  into env
            else
              if (string= k "SCRIPT_NAME")
                append (list :script-name
                             (if (string= v "/")
                                 ""
                                 v))
                  into env
            finally
               (return (nconc
                        env
                        (list :headers headers
                              :path-info
                              (quri:url-decode request-uri
                                               :end (position #\? request-uri
                                                              :test #'char=)
                                               :lenient t)
                              :url-scheme "http"
                              :raw-body (loop with buf = (make-array 0 :fill-pointer 0 :adjustable t)
                                              for v in (cdr (fcgx-read-all req))
                                              do (adjust-array buf (+ (length buf) (length v)))
                                                 (loop for val across v
                                                       do (vector-push val buf))
                                              finally
                                                 (return
                                                   (flex:make-in-memory-input-stream buf)))
                              :clack.streaming t)))))))

(defvar *http-status* (make-hash-table :test #'eql))

(macrolet ((def-http-status (code phrase)
             `(setf (gethash ,code *http-status*) ,phrase)))
  (def-http-status 100 "Continue")
  (def-http-status 101 "Switching Protocols")
  (def-http-status 200 "OK")
  (def-http-status 201 "Created")
  (def-http-status 202 "Accepted")
  (def-http-status 203 "Non-Authoritative Information")
  (def-http-status 204 "No Content")
  (def-http-status 205 "Reset Content")
  (def-http-status 206 "Partial Content")
  (def-http-status 207 "Multi-Status")
  (def-http-status 300 "Multiple Choices")
  (def-http-status 301 "Moved Permanently")
  (def-http-status 302 "Moved Temporarily")
  (def-http-status 303 "See Other")
  (def-http-status 304 "Not Modified")
  (def-http-status 305 "Use Proxy")
  (def-http-status 307 "Temporary Redirect")
  (def-http-status 400 "Bad Request")
  (def-http-status 401 "Authorization Required")
  (def-http-status 402 "Payment Required")
  (def-http-status 403 "Forbidden")
  (def-http-status 404 "Not Found")
  (def-http-status 405 "Method Not Allowed")
  (def-http-status 406 "Not Acceptable")
  (def-http-status 407 "Proxy Authentication Required")
  (def-http-status 408 "Request Time-out")
  (def-http-status 409 "Conflict")
  (def-http-status 410 "Gone")
  (def-http-status 411 "Length Required")
  (def-http-status 412 "Precondition Failed")
  (def-http-status 413 "Request Entity Too Large")
  (def-http-status 414 "Request-URI Too Large")
  (def-http-status 415 "Unsupported Media Type")
  (def-http-status 416 "Requested range not satisfiable")
  (def-http-status 417 "Expectation Failed")
  (def-http-status 424 "Failed Dependency")
  (def-http-status 500 "Internal Server Error")
  (def-http-status 501 "Not Implemented")
  (def-http-status 502 "Bad Gateway")
  (def-http-status 503 "Service Unavailable")
  (def-http-status 504 "Gateway Time-out")
  (def-http-status 505 "Version not supported"))

(defun http-status-reason (code)
  (gethash code *http-status*))
