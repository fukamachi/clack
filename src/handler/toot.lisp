(in-package :cl-user)
(defpackage clack.handler.toot
  (:use :cl
        :toot
        :split-sequence
        :ppcre)
  (:shadow :handle-request)
  (:import-from :toot
                :shutdown-p
                :listen-socket
                :listen-backlog
                :acceptor-process
                :accept-connections)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :alexandria
                :if-let)
  (:export :run))
(in-package :clack.handler.toot)

(defun run (app &rest args
            &key debug (port 5000)
              ssl ssl-key-file ssl-cert-file ssl-key-password)
  "Start Toot server."
  (cond
    ((asdf::getenv "SERVER_STARTER_PORT")
     (error "Toot handler doesn't work with Server::Starter."))
    ((getf args :fd)
     (error ":fd is specified though Toot handler cannot listen on fd")))

  (let* ((stdout *standard-output*)
         (errout *error-output*)
         (acceptor (apply #'make-instance 'toot:acceptor
                          :handler (lambda (req)
                                     (let ((env (handle-request req :ssl ssl))
                                           (*standard-output* stdout)
                                           (*error-output* errout))
                                       (handle-response
                                        req
                                        (if debug
                                            (funcall app env)
                                            (handler-case (funcall app env)
                                              (error (error)
                                                (princ error *error-output*)
                                                '(500 () ("Internal Server Error"))))))))
                          :port port
                          :access-logger nil
                          (if ssl
                              (list :ssl-certificate-file ssl-cert-file
                                    :ssl-private-key-file ssl-key-file
                                    :ssl-private-key-password ssl-key-password)
                              '()))))
    (setf (shutdown-p acceptor) nil)
    (setf (listen-socket acceptor)
          (usocket:socket-listen
           (or (address acceptor) usocket:*wildcard-host*) port
           :reuseaddress t
           :backlog (listen-backlog acceptor)
           :element-type '(unsigned-byte 8)))
    (setf (acceptor-process (toot:taskmaster acceptor)) (bt:current-thread))
    (unwind-protect
         (accept-connections acceptor)
      (toot:stop-acceptor acceptor))))

(defun handle-request (req &key ssl)
  "Convert Request from server into a plist
before pass to Clack application."
  (let ((content-length (if-let (content-length (request-header :content-length req))
                          (parse-integer content-length :junk-allowed t)
                          (progn
                            (setf (slot-value req 'request-headers) (acons :content-length "" (slot-value req 'request-headers)))
                            nil))))
    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (cdr (assoc :host (request-headers req))))
      (list
       :request-method (request-method req)
       :script-name ""
       :path-info (url-decode (request-path req))
       :server-name server-name
       :server-port (if server-port
                        (parse-integer server-port)
                        80)
       :server-protocol (server-protocol req)
       :request-uri (request-uri req)
       :url-scheme (if ssl :https :http)
       :remote-addr (remote-addr req)
       :remote-port (remote-port req)
       :query-string (request-query req)
       :content-length content-length
       :content-type (request-header :content-type req)
       :raw-body (toot::request-body-stream req)
       :clack.streaming t
       :clack.handler :toot
       :headers (loop with headers = (make-hash-table :test 'equal)
                      for (k . v) in (toot::request-headers req)
                      unless (or (eq k :content-length)
                                 (eq k :content-type))
                        do (setf (gethash (string-downcase k) headers) v)
                      finally (return headers))))))

(defun handle-response (req res)
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (req res)
             (destructuring-bind (status headers &optional (body no-body)) res
               (when (pathnamep body)
                 (multiple-value-call #'serve-file
                   (values req body (parse-charset (getf headers :content-type))))
                 (return-from handle-normal-response))

               (setf (status-code req) status)
               (loop for (k v) on headers by #'cddr
                     if (eq k :set-cookie)
                       do (rplacd (last (toot::response-headers req))
                                  (list (cons k v)))
                     else if (eq k :content-type) do
                       (multiple-value-bind (v charset)
                           (parse-charset v)
                         (setf (response-header k req) v)
                         (setf (toot::response-charset req) charset))
                     else if (response-header k req) do
                       (setf (response-header k req)
                             (format nil "~A, ~A" (response-header k req) v))
                     else do
                       (setf (response-header k req) v))
               (toot::send-response-headers
                req
                (getf headers :content-length)
                nil
                (toot::response-charset req))
               (let ((out (toot::content-stream req)))
                 (when (eq body no-body)
                   (return-from handle-normal-response
                     (lambda (body &key (close nil))
                       (declare (ignore close))
                       (write-sequence (if (stringp body)
                                           (flex:string-to-octets body :external-format toot::*default-charset*)
                                           body)
                                       out))))

                 (etypecase body
                   (null) ;; nothing to response
                   (list
                    (write-sequence (flex:string-to-octets (format nil "~{~A~}" body)
                                                           :external-format toot::*default-charset*)
                                    out)))))))
      (etypecase res
        (list (handle-normal-response req res))
        (function (funcall res (lambda (res)
                                 (handle-normal-response req res))))))))

(defun parse-charset (content-type)
  (multiple-value-bind (start end reg1 reg2)
      (ppcre:scan "(;\\s*?charset=([-_a-zA-Z0-9]+))" content-type)
    (declare (ignore end))
    (if start
        (values (subseq content-type 0 (aref reg1 0))
                (subseq content-type (aref reg1 1) (aref reg2 1)))
        ;; there is no ";charset="
        (values content-type toot::*default-charset*))))
