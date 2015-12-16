(in-package :cl-user)
(defpackage clack.handler.hunchentoot
  (:use :cl
        :hunchentoot
        :split-sequence)
  (:shadow :handle-request)
  (:import-from :hunchentoot
                :acceptor-taskmaster
                :acceptor-process
                :acceptor-shutdown-p)
  (:import-from :flexi-streams
                :make-external-format
                :string-to-octets)
  (:import-from :alexandria
                :when-let)
  (:export :run))
(in-package :clack.handler.hunchentoot)

(defun initialize ()
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"
        *catch-errors-p* nil))

(defclass clack-acceptor (acceptor)
  ((app :initarg :app
        :initform (error ":app is required")
        :accessor acceptor-app)
   (debug :initarg :debug
          :initform nil
          :accessor acceptor-debug)))

#-hunchentoot-no-ssl
(defclass clack-ssl-acceptor (clack-acceptor ssl-acceptor) ())

(defgeneric acceptor-handle-request (acceptor req)
  (:method ((acceptor clack-acceptor) req)
    (handle-request req :ssl nil))
  (:method  ((acceptor clack-ssl-acceptor) req)
    (handle-request req :ssl t)))

(defmethod acceptor-dispatch-request ((acceptor clack-acceptor) req)
  (let ((app (acceptor-app acceptor))
        (env (acceptor-handle-request acceptor req)))
    (handle-response
     (if (acceptor-debug acceptor)
         (funcall app env)
         (handler-case (funcall app env)
           (error (error)
             (princ error *error-output*)
             '(500 () ("Internal Server Error"))))))))

(defun run (app &rest args
            &key debug (port 5000)
              ssl ssl-key-file ssl-cert-file ssl-key-password
              max-thread-count max-accept-count (persistent-connections-p t))
  "Start Hunchentoot server."
  (cond
    ((asdf::getenv "SERVER_STARTER_PORT")
     (error "Hunchentoot handler doesn't work with Server::Starter."))
    ((getf args :fd)
     (error ":fd is specified though Hunchentoot handler cannot listen on fd")))

  (initialize)
  (let* ((app (let ((stdout *standard-output*)
                    (stderr *error-output*))
                (lambda (env)
                  (let ((*standard-output* stdout)
                        (*error-output* stderr))
                    (funcall app env)))))
         (taskmaster (when (and max-thread-count max-accept-count)
                       (make-instance 'one-thread-per-connection-taskmaster
                                      :max-thread-count max-thread-count
                                      :max-accept-count max-accept-count)))
         (acceptor
           (if ssl
               (apply #'make-instance 'clack-ssl-acceptor
                      :app app
                      :debug debug
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
                      :port port
                      :access-log-destination nil
                      :error-template-directory nil
                      :persistent-connections-p persistent-connections-p
                      (and taskmaster
                           (list :taskmaster taskmaster))))))
    (setf (hunchentoot::acceptor-shutdown-p acceptor) nil)
    (start-listening acceptor)
    (let ((taskmaster (acceptor-taskmaster acceptor)))
      (setf (taskmaster-acceptor taskmaster) acceptor)
      #+thread-support
      (setf (acceptor-process taskmaster) (bt:current-thread))
      (unwind-protect
           (accept-connections acceptor)
        (hunchentoot:stop acceptor)))))

(defun handle-response (res)
  "Convert Response from Clack application into a string
before passing to Hunchentoot."
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (res)
             (destructuring-bind (status headers &optional (body no-body)) res
               (setf (return-code*) status)
               (loop for (k v) on headers by #'cddr
                     if (eq k :set-cookie)
                       do (rplacd (last (headers-out*))
                                  (list (cons k v)))
                     else if (eq k :content-type) do
                       (setf (content-type*) v)
                     else if (eq k :content-length) do
                       (setf (content-length*) v)
                     else if (header-out k) do
                       (setf (header-out k)
                             (format nil "~A, ~A" (header-out k) v))
                     else
                       do (setf (header-out k) v))

               (when (eq body no-body)
                 (return-from handle-normal-response
                   (let ((out (send-headers)))
                     (lambda (body &key (close nil))
                       (write-sequence
                        (if (stringp body)
                            (flex:string-to-octets body
                                                   :external-format *hunchentoot-default-external-format*)
                            body)
                        out)
                       (when close
                         (finish-output out))))))

               (etypecase body
                 (null) ;; nothing to response
                 (pathname
                  (hunchentoot:handle-static-file body (getf headers :content-type)))
                 (list
                  (let ((out (send-headers)))
                    (dolist (chunk body)
                      (write-sequence (flex:string-to-octets chunk
                                                             :external-format *hunchentoot-default-external-format*)
                                      out))))
                 ((vector (unsigned-byte 8))
                  ;; I'm not convinced with this header should be send automatically or not
                  ;; and not sure how to handle same way in other method so comment out
                  ;;(setf (content-length*) (length body))
                  (let ((out (send-headers)))
                    (write-sequence body out)
                    (finish-output out)))))))
      (etypecase res
        (list (handle-normal-response res))
        (function (funcall res #'handle-normal-response)))
      (values))))

(defun handle-request (req &key ssl)
  "Convert Request from server into a plist
before passing to Clack application."
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (host req) :from-end t)
    (list
     :request-method (request-method* req)
     :script-name ""
     :path-info (script-name* req)
     :server-name server-name
     :server-port (parse-integer server-port :junk-allowed t)
     :server-protocol (server-protocol* req)
     :request-uri (request-uri* req)
     :url-scheme (if ssl :https :http)
     :remote-addr (remote-addr* req)
     :remote-port (remote-port* req)
     ;; Request params
     :query-string (query-string* req)
     :raw-body (raw-post-data :request req :want-stream t)
     :content-length (when-let (content-length (header-in* :content-length req))
                       (parse-integer content-length :junk-allowed t))
     :content-type (header-in* :content-type req)
     :clack.streaming t

     :headers (loop with headers = (make-hash-table :test 'equal)
                    for (k . v) in (hunchentoot:headers-in* req)
                    unless (or (eq k :content-length)
                               (eq k :content-type))
                      do (setf (gethash (string-downcase k) headers) v)
                    finally (return headers)))))
