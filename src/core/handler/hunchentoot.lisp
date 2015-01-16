#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.handler.hunchentoot
  (:use :cl
        :hunchentoot
        :split-sequence)
  (:shadow :stop
           :handle-request)
  (:import-from :hunchentoot
                :acceptor-taskmaster
                :acceptor-shutdown-p)
  (:import-from :clack.component
                :call)
  (:import-from :flexi-streams
                :make-external-format
                :string-to-octets)
  (:import-from :alexandria
                :when-let))
(in-package :clack.handler.hunchentoot)

(cl-syntax:use-syntax :annot)

(defun initialize ()
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"
        *catch-errors-p* nil))

@export
(defun run (app &key debug (port 5000)
                  ssl ssl-key-file ssl-cert-file ssl-key-password
                  max-thread-count max-accept-count (persistent-connections-p t))
  "Start Hunchentoot server."
  (initialize)
  (setf *dispatch-table*
        (list
         (let ((stdout *standard-output*)
               (errout *error-output*))
           #'(lambda (req)
               (let ((env (handle-request req :ssl ssl)))
                 #'(lambda ()
                     (let ((*standard-output* stdout)
                           (*error-output* errout))
                       (handle-response
                        (if debug
                            (call app env)
                            (handler-case (call app env)
                              (error (error)
                                (princ error *error-output*)
                                '(500 () ("Internal Server Error")))))))))))))
  (let* ((taskmaster (when (and max-thread-count max-accept-count)
                       (make-instance 'one-thread-per-connection-taskmaster
                                      :max-thread-count max-thread-count
                                      :max-accept-count max-accept-count)))
         (acceptor
          (if ssl
              (if (and max-thread-count max-accept-count)
                  (make-instance 'easy-ssl-acceptor
                                 :port port
                                 :ssl-certificate-file ssl-cert-file
                                 :ssl-privatekey-file ssl-key-file
                                 :ssl-privatekey-password ssl-key-password
                                 :access-log-destination nil
                                 :error-template-directory nil
                                 :persistent-connections-p persistent-connections-p
                                 :taskmaster taskmaster)
                  (make-instance 'easy-ssl-acceptor
                                 :port port
                                 :ssl-certificate-file ssl-cert-file
                                 :ssl-privatekey-file ssl-key-file
                                 :ssl-privatekey-password ssl-key-password
                                 :access-log-destination nil
                                 :error-template-directory nil
                                 :persistent-connections-p persistent-connections-p))
              (if (and max-thread-count max-accept-count)
                  (make-instance 'easy-acceptor
                                 :port port
                                 :access-log-destination nil
                                 :error-template-directory nil
                                 :persistent-connections-p persistent-connections-p
                                 :taskmaster taskmaster)
                  (make-instance 'easy-acceptor
                                 :port port
                                 :access-log-destination nil
                                 :error-template-directory nil
                                 :persistent-connections-p persistent-connections-p)))))
    (setf (acceptor-shutdown-p acceptor) nil)
    (start-listening acceptor)
    (let ((taskmaster (acceptor-taskmaster acceptor)))
      (setf (taskmaster-acceptor taskmaster) acceptor)
      (accept-connections acceptor))))

@export
(defun stop (acceptor)
  "Stop Hunchentoot server.
If no acceptor is given, try to stop `*acceptor*' by default."
  (hunchentoot:stop acceptor))

(defun handle-response (res)
  "Convert Response from Clack application into a string
before passing to Hunchentoot."
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (res)
             (destructuring-bind (status headers &optional (body no-body)) res
               (setf (return-code*) status)
               (loop for (k v) on headers by #'cddr
                     with hash = (make-hash-table :test #'eq)
                     if (gethash k hash)
                       do (setf (gethash k hash)
                                (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
                     else do (setf (gethash k hash) v)
                     finally
                        (loop for k being the hash-keys in hash
                                using (hash-value v)
                              do (setf (header-out k) v)))

               (when (eq body no-body)
                 (return-from handle-normal-response
                   (let ((out (send-headers)))
                     (lambda (body &key (close nil))
                       (write-sequence
                        (if (stringp body)
                            (flex:string-to-octets body)
                            body)
                        out)
                       (when close
                         (finish-output out))))))

               (etypecase body
                 (null) ;; nothing to response
                 (pathname
                  (hunchentoot:handle-static-file body (getf headers :content-type)))
                 (list
                  (with-output-to-string (s)
                    (format s "~{~A~}" body)))
                 ((vector (unsigned-byte 8))
                  ;; I'm not convinced with this header should be send automatically or not
                  ;; and not sure how to handle same way in other method so comment out
                  ;;(setf (content-length*) (length body))
                  (let ((out (send-headers)))
                    (write-sequence body out)
                    (finish-output out)))))))
      (etypecase res
        (list (handle-normal-response res))
        (function (funcall res #'handle-normal-response))))))

(defun handle-request (req &key ssl)
  "Convert Request from server into a plist
before passing to Clack application."
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (host req) :from-end t)
    (list
     :request-method (request-method* req)
     :script-name ""
     :path-info (url-decode (script-name* req))
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

(doc:start)

@doc:NAME "
Clack.Handler.Hunchentoot - Clack handler for Hunchentoot.
"

@doc:SYNOPSIS "
    (defpackage clack-sample
      (:use :cl
            :clack.handler.hunchentoot))
    (in-package :clack-sample)

    ;; Start Server
    (run (lambda (env)
           '(200 nil (\"ok\")))
         :port 5000)
"

@doc:DESCRIPTION "
Clack.Handler.Hunchentoot is a Clack handler for the Lisp web server Hunchentoot. This package exports `run' and `stop'.
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"
