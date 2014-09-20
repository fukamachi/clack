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
  (:import-from :clack.component
                :call)
  (:import-from :flexi-streams
                :make-external-format
                :string-to-octets)
  (:import-from :alexandria
                :when-let
                :if-let))
(in-package :clack.handler.hunchentoot)

(cl-syntax:use-syntax :annot)

(defun initialize ()
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"
        *catch-errors-p* nil))

@export
(defun run (app &key debug (port 5000))
  "Start Hunchentoot server."
  (initialize)
  (when debug
    (setf *show-lisp-errors-p* t))
  (setf *dispatch-table*
        (list
         #'(lambda (env)
             #'(lambda ()
                 (handle-response
                  (if debug
                      (call app env)
                      (if-let (res (handler-case (call app env)
                                     (error (error)
                                       (princ error *error-output*)
                                       nil)))
                        res
                        '(500 nil nil))))))))
  (start (make-instance '<debuggable-acceptor>
                        :port port
                        :access-log-destination nil
                        :error-template-directory nil)))

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
                    (format s "~{~A~^~%~}" body)))
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

(defun handle-request (req)
  "Convert Request from server into a plist
before passing to Clack application."
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (host req) :from-end t)
    (append
     (list
      :request-method (request-method* req)
      :script-name ""
      :path-info (url-decode (script-name* req))
      :server-name server-name
      :server-port (parse-integer server-port :junk-allowed t)
      :server-protocol (server-protocol* req)
      :request-uri (request-uri* req)
      ;; FIXME: This handler cannot connect with SSL now.
      :url-scheme :http
      :remote-addr (remote-addr* req)
      :remote-port (remote-port* req)
      ;; Request params
      :query-string (or (query-string* req) "")
      :raw-body (raw-post-data :request req :want-stream t)
      :content-length (when-let (content-length (header-in* :content-length req))
                        (parse-integer content-length :junk-allowed t))
      :content-type (header-in* :content-type req)
      :clack.streaming t)

     (loop for (k . v) in (hunchentoot:headers-in* req)
           unless (find k '(:request-method :script-name :path-info :server-name :server-port :server-protocol :request-uri :remote-addr :remote-port :query-string :content-length :content-type :connection))
             append (list (intern (format nil "HTTP-~:@(~A~)" k) :keyword)
                          v)))))

;; for Debug

;;; Acceptor that provides debugging from the REPL
;;; Based on an email by Andreas Fruchs:
;;; http://common-lisp.net/pipermail/tbnl-devel/2009-April/004688.html
(defclass <debuggable-acceptor> (acceptor)
     ()
  (:documentation "An acceptor that handles errors by invoking the
  debugger."))

(defmethod process-connection ((*acceptor* <debuggable-acceptor>) (socket t))
  (declare (ignore socket))
  ;; Invoke the debugger on any errors except for SB-SYS:IO-TIMEOUT.
  ;; HTTP browsers usually leave the connection open for futher requests,
  ;; and Hunchentoot respects this but sets a timeout so that old connections
  ;; are cleaned up.
  (handler-case (call-next-method)
    #+sbcl (sb-sys:io-timeout (condition) (values nil condition))
    ;; preventing Connection reset by peer
    #+sbcl (sb-int:simple-stream-error (condition) (values nil condition))
    (error (condition) (invoke-debugger condition))))

(defmethod acceptor-request-dispatcher ((*acceptor* <debuggable-acceptor>))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))

(defmethod acceptor-dispatch-request ((this <debuggable-acceptor>) request)
  "Hunchentoot request dispatcher for Clack. Most of this is the same as
list-request-dispatcher, the default one in Hunchentoot, except for converting
Request instances into a plist before passing to Clack application."
  (loop for dispatcher in *dispatch-table*
        for action = (funcall dispatcher (handle-request request))
        when action :return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defmethod acceptor-status-message ((this <debuggable-acceptor>) http-status-code &rest args &key)
  "Disable generating error HTML."
  (declare (ignore http-status-code args))
  nil)

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
