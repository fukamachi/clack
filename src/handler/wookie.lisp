(in-package :cl-user)
(defpackage clack.handler.wookie
  (:use :cl)
  (:import-from :wookie
                :*state*
                :wookie-state
                :start-response
                :send-response
                :finish-response
                :add-hook
                :defroute
                :listener
                :ssl-listener
                :start-server
                :request-headers
                :request-resource
                :request-http
                :request-body
                :request-store-body
                :request-method
                :request-uri
                :request-socket)
  (:import-from :cl-async
                :with-event-loop
                :close-tcp-server
                :async-io-stream
                :socket-data
                :write-socket-data)
  (:import-from :fast-http
                :http-version)
  (:import-from :quri
                :uri-path
                :uri-query
                :parse-uri
                :url-decode)
  (:import-from :flexi-streams
                :make-in-memory-input-stream)
  (:import-from :babel
                :string-to-octets)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :copy-stream)
  (:export :run))
(in-package :clack.handler.wookie)

;; XXX: :store-body keeps the whole POST data in-memory.
(defun parsed-headers-hook (request)
  (setf (wookie:request-store-body request) t))

(defun run (app &rest args
            &key (debug t) (address "127.0.0.1") (port 5000)
              ssl ssl-key-file ssl-cert-file ssl-key-password)
  (cond
    ((asdf::getenv "SERVER_STARTER_PORT")
     (error "Wookie handler doesn't work with Server::Starter."))
    ((getf args :fd)
     (error ":fd is specified though Wookie handler cannot listen on fd")))

  (let ((*state* (make-instance 'wookie:wookie-state)))
    (add-hook :parsed-headers 'parsed-headers-hook :clack-handler-wookie-parsed-headers-hook)
    (defroute (:* ".*" :chunk nil) (req res)
      (let ((env (handle-request req :ssl ssl)))
        (handle-response
         res
         (if debug
	     (restart-case
		 (funcall app env)
	       (throw-internal-server-error ()
		 '(500 nil ("Internal Server Error"))))
             (handler-case (funcall app env)
               (error (error)
                 (princ error *error-output*)
                 '(500 nil ("Internal Server Error"))))))))
    (handler-case
        (as:with-event-loop ()
          (let ((listener
                  (if ssl
                      (make-instance 'wookie:ssl-listener
                                     :bind address
                                     :port port
                                     :key ssl-key-file
                                     :certificate ssl-cert-file
                                     :password ssl-key-password)
                      (make-instance 'wookie:listener
                                     :bind address
                                     :port port))))
            (start-server listener)))
      (as:socket-closed () nil))))

(defun handle-request (req &key ssl)
  (let ((quri (request-uri req))
        (http-version (http-version (request-http req)))
        (headers (request-headers req)))

    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (gethash "host" headers "") :from-end t :count 2)
      (setf (quri:uri-path quri)
            (nth-value 4 (quri:parse-uri (request-resource req))))
      (list :request-method (request-method req)
            :script-name ""
            :server-name server-name
            :server-port (if server-port
                             (parse-integer server-port :junk-allowed t)
                             80)
            :server-protocol (intern (format nil "HTTP/~A" http-version)
                                     :keyword)
            :path-info (quri:url-decode (uri-path quri) :lenient t)
            :query-string (uri-query quri)
            :url-scheme (if ssl "https" "http")
            :request-uri (request-resource req)
            :raw-body (flex:make-in-memory-input-stream (wookie:request-body req))
            :content-length (gethash "content-length" headers)
            :content-type (gethash "content-type" headers)
            :clack.streaming t
            :clack.nonblocking t
            :clack.io (request-socket req)
            :headers headers))))

(defun handle-response (res clack-res)
  (etypecase clack-res
    (list (handle-normal-response res clack-res))
    (function (funcall clack-res (lambda (clack-res)
                                   (handler-case
                                       (handle-normal-response res clack-res)
                                     ;; Ignore when the socket is closed.
                                     (as:socket-closed ())))))))

(defun handle-normal-response (res clack-res)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body)) clack-res
      ;; Returns a writer function for streaming response
      (when (eq body no-body)
        (let ((stream (start-response res
                                      :status status
                                      :headers headers)))
          (return-from handle-normal-response
            (lambda (body &key (start 0) (end (length body)) (close nil))
              (etypecase body
                (null)
                (string (write-sequence (babel:string-to-octets body :start start :end end) stream))
                ((vector (unsigned-byte 8)) (write-sequence body stream :start start :end end)))
              (when close
                (finish-response res))))))

      (etypecase body
        ;; Just send the headers and status.
        (null (send-response res :status status :headers headers))
        (pathname
         (let ((stream (start-response res
                                       :status status
                                       :headers headers)))
           (with-open-file (in body
                               :direction :input
                               :element-type '(unsigned-byte 8))
             (copy-stream in stream))
           (finish-response res)))
        (list
         (send-response res
                        :status status
                        :headers headers
                        :body (if (null (cdr body))
                                  (car body)
                                  (with-fast-output (buffer :vector)
                                    (dolist (str body)
                                      (fast-write-sequence (babel:string-to-octets str) buffer))))))
        ((vector (unsigned-byte 8))
         (send-response res
                        :status status
                        :headers headers
                        :body body))))))

(defmethod clack.socket:read-callback ((socket as:socket))
  (getf (as:socket-data socket) :parser))

(defmethod (setf clack.socket:read-callback) (callback (socket as:socket))
  (setf (getf (as:socket-data socket) :parser) callback))

(defmethod clack.socket:write-sequence-to-socket ((socket as:socket) data &key callback)
  (as:write-socket-data socket data
                        :write-cb
                        (and callback
                             (lambda (socket)
                               (declare (ignore socket))
                               (funcall callback)))))

(defmethod clack.socket:close-socket ((socket as:socket))
  (unless (as:socket-closed-p socket)
    (as:close-socket socket)))

(defmethod clack.socket:socket-async-p ((socket as:socket))
  t)
