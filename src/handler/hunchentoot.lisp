(in-package :cl-user)
(defpackage clack.handler.hunchentoot
  (:use :cl
        :split-sequence)
  (:import-from :hunchentoot
                :acceptor-taskmaster
                :acceptor-process
                :acceptor-shutdown-p)
  (:import-from :flexi-streams
                :make-external-format
                :string-to-octets
                :*substitution-char*)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:import-from :alexandria
                :when-let)
  (:export :run))
(in-package :clack.handler.hunchentoot)

(defvar *client-socket*)

(defclass client ()
  ((stream :initarg :stream
           :reader client-stream)
   (socket :initarg :socket
           :reader client-socket)
   (read-callback :initarg :read-callback
                  :initform nil
                  :accessor client-read-callback)
   (write-lock :initform (bt2:make-lock)
               :reader client-write-lock)))

(defun initialize ()
  (setf hunchentoot:*hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        hunchentoot:*default-content-type* "text/html; charset=utf-8"
        hunchentoot:*catch-errors-p* t
        ;; Not logging 'Broken pipe'
        hunchentoot:*log-lisp-errors-p* nil))

(defclass clack-acceptor (hunchentoot:acceptor)
  ((app :initarg :app
        :initform (error ":app is required")
        :accessor acceptor-app)
   (debug :initarg :debug
          :initform nil
          :accessor acceptor-debug)))

#-hunchentoot-no-ssl
(defclass clack-ssl-acceptor (clack-acceptor hunchentoot:ssl-acceptor) ())

(defgeneric acceptor-handle-request (acceptor req)
  (:method ((acceptor clack-acceptor) req)
    (handle-request req :ssl nil))
  #-hunchentoot-no-ssl
  (:method  ((acceptor clack-ssl-acceptor) req)
    (handle-request req :ssl t)))

(defmethod hunchentoot:acceptor-dispatch-request ((acceptor clack-acceptor) req)
  (let ((app (acceptor-app acceptor))
        (env (acceptor-handle-request acceptor req))
        (hunchentoot:*catch-errors-p* nil))
    (if (acceptor-debug acceptor)
        (handle-response (funcall app env))
        (handler-case (handle-response (funcall app env))
          (error (error)
            (princ error *error-output*)
            (handle-response '(500 () ("Internal Server Error"))))))))

(defmethod hunchentoot:process-connection :around ((acceptor clack-acceptor) socket)
  (let ((flex:*substitution-char* #-(or abcl lispworks) #\Replacement_Character
                                  #+lispworks #\Replacement-Character
                                  #+abcl #\?)
        (*client-socket* socket))
    (call-next-method)))

(defun run (app &rest args
            &key debug (address "127.0.0.1") (port 5000)
              ssl ssl-key-file ssl-cert-file ssl-key-password
              max-thread-count max-accept-count (persistent-connections-p t))
  "Start Hunchentoot server."
  (cond
    ((asdf::getenv "SERVER_STARTER_PORT")
     (error "Hunchentoot handler doesn't work with Server::Starter."))
    ((getf args :fd)
     (error ":fd is specified though Hunchentoot handler cannot listen on fd")))

  (initialize)
  (let* ((taskmaster (when (and max-thread-count max-accept-count)
                       (make-instance 'hunchentoot:one-thread-per-connection-taskmaster
                                      :max-thread-count max-thread-count
                                      :max-accept-count max-accept-count)))
         (acceptor
           (if ssl
               (apply #'make-instance 'clack-ssl-acceptor
                      :app app
                      :debug debug
                      :address address
                      :port port
                      :ssl-certificate-file ssl-cert-file
                      :ssl-privatekey-file ssl-key-file
                      :ssl-privatekey-password ssl-key-password
                      :access-log-destination nil
                      :persistent-connections-p persistent-connections-p
                      (and taskmaster
                           (list :taskmaster taskmaster)))
               (apply #'make-instance 'clack-acceptor
                      :app app
                      :debug debug
                      :address address
                      :port port
                      :access-log-destination nil
                      :error-template-directory nil
                      :persistent-connections-p persistent-connections-p
                      (and taskmaster
                           (list :taskmaster taskmaster))))))
    (let* ((taskmaster (acceptor-taskmaster acceptor))
           (threadedp (typep taskmaster 'hunchentoot:multi-threaded-taskmaster)))
      (setf (hunchentoot:taskmaster-acceptor taskmaster) acceptor)
      (unwind-protect
          (progn
            (hunchentoot:start acceptor)
            #-lispworks
            (when threadedp
              (let ((thread (hunchentoot::acceptor-process taskmaster)))
                (bt2:join-thread
                  (if (typep thread 'bt2:thread)
                      thread
                      (bt2::ensure-thread-wrapper thread)))))
            #+lispworks
            (loop (sleep (expt 2 32))))
        (hunchentoot:stop acceptor)))))

(defun handle-response (res)
  "Convert Response from Clack application into a string
before passing to Hunchentoot."
  (flet ((handle-normal-response (res)
           (destructuring-bind (status headers &optional (body nil body-p)) res
             (setf (hunchentoot:return-code*) status)
             (loop for (k v) on headers by #'cddr
                   if (eq k :set-cookie)
                     do (rplacd (last (hunchentoot:headers-out*))
                                (list (cons k v)))
                   else if (eq k :content-type) do
                     (setf (hunchentoot:content-type*) v)
                   else if (eq k :content-length) do
                     (setf (hunchentoot:content-length*) v)
                   else if (hunchentoot:header-out k) do
                     (setf (hunchentoot:header-out k)
                           (format nil "~A, ~A" (hunchentoot:header-out k) v))
                   else
                     do (setf (hunchentoot:header-out k) v))

             (unless body-p
               (return-from handle-normal-response
                 (let ((out (hunchentoot:send-headers)))
                   (lambda (body &key (start 0) (end (length body)) (close nil))
                     (handler-case
                       (etypecase body
                         (null)
                         (string
                           (write-sequence
                             (flex:string-to-octets body
                                                    :start start :end end
                                                    :external-format hunchentoot:*hunchentoot-default-external-format*)
                             out))
                         ((vector (unsigned-byte 8))
                          (write-sequence body out :start start :end end)))
                       (type-error (e)
                         (format *error-output* "Error when writing to socket: ~a~%" e)))
                     (if close
                         (finish-output out)
                         (force-output out))))))

             (handler-case
               (etypecase body
                 (null) ;; nothing to response
                 (pathname
                   (hunchentoot:handle-static-file body (getf headers :content-type)))
                 (list
                   (let ((out (hunchentoot:send-headers)))
                     (dolist (chunk body)
                       (write-sequence (flex:string-to-octets chunk
                                                              :external-format hunchentoot:*hunchentoot-default-external-format*)
                                       out))))
                 ((vector (unsigned-byte 8))
                  ;; I'm not convinced with this header should be send automatically or not
                  ;; and not sure how to handle same way in other method so comment out
                  ;;(setf (content-length*) (length body))
                  (let ((out (hunchentoot:send-headers)))
                    (write-sequence body out)
                    (finish-output out))))
               (type-error (e)
                 (format *error-output* "Error when writing to socket: ~a~%" e))))))
    (etypecase res
      (list (handle-normal-response res))
      (function (funcall res #'handle-normal-response)))
    (values)))

(defun handle-request (req &key ssl)
  "Convert Request from server into a plist
before passing to Clack application."
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (hunchentoot:host req) :from-end t)
    (list
     :request-method (hunchentoot:request-method* req)
     :script-name ""
     :path-info (hunchentoot:script-name* req)
     :server-name server-name
     :server-port (parse-integer server-port :junk-allowed t)
     :server-protocol (hunchentoot:server-protocol* req)
     :request-uri (hunchentoot:request-uri* req)
     :url-scheme (if ssl "https" "http")
     :remote-addr (hunchentoot:remote-addr* req)
     :remote-port (hunchentoot:remote-port* req)
     ;; Request params
     :query-string (hunchentoot:query-string* req)
     :raw-body (hunchentoot:raw-post-data :request req :want-stream t)
     :content-length (when-let (content-length (hunchentoot:header-in* :content-length req))
                       (parse-integer content-length :junk-allowed t))
     :content-type (hunchentoot:header-in* :content-type req)
     :clack.streaming t
     :clack.io (make-instance 'client
                              :socket *client-socket*
                              :stream (hunchentoot::content-stream req))

     :headers (loop with headers = (make-hash-table :test 'equal)
                    for (k . v) in (hunchentoot:headers-in* req)
                    unless (or (eq k :content-length)
                               (eq k :content-type))
                      do (setf (gethash (string-downcase k) headers) v)
                    finally (return headers)))))

(defmethod clack.socket:read-callback ((client client))
  (client-read-callback client))

(defmethod (setf clack.socket:read-callback) (callback (client client))
  (setf (client-read-callback client) callback))

(defmethod clack.socket:write-sequence-to-socket ((client client) data &key callback)
  (bt2:with-lock-held ((client-write-lock client))
    (let ((stream (client-stream client)))
      (write-sequence data stream)
      (force-output stream)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:close-socket ((client client))
  (bt2:with-lock-held ((client-write-lock client))
    (finish-output (client-stream client))))

(defmethod clack.socket:flush-socket-buffer ((client client) &key callback)
  (bt2:with-lock-held ((client-write-lock client))
    (force-output (client-stream client)))
  (when callback
    (funcall callback)))

(defmethod clack.socket:socket-async-p ((client client))
  nil)

(defmethod clack.socket:socket-stream ((client client))
  (client-stream client))
