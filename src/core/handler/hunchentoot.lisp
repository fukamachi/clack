#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack handler for Hunchentoot.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.handler.hunchentoot
  (:use :cl
        :clack.component
        :hunchentoot
        :split-sequence
        :alexandria
        :anaphora)
  (:shadow :stop)
  (:export :run :stop))

(in-package :clack.handler.hunchentoot)

(defun initialize ()
  (setf *hunchentoot-default-external-format*
        (flex:make-external-format :utf-8 :eol-style :lf)
        *default-content-type* "text/html; charset=utf-8"))

(defun run (app &key debug (port 8080))
  "Start Hunchentoot server."
  (initialize)
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (setf *dispatch-table*
        (list #'(lambda (req)
                  #'(lambda () (handle-response (call app req))))))
  (start (make-instance '<debuggable-acceptor>
            :port port
            :request-dispatcher 'clack-request-dispatcher)))

(defun stop (acceptor)
  "Stop Hunchentoot server.
If no acceptor given, try to stop `*acceptor*' by default."
  (hunchentoot:stop acceptor))

(defun clack-request-dispatcher (request)
  "Hunchentoot request dispatcher for Clack. Most of this is same as
list-request-dispatcher, default one in Hunchentoot, except for convert
Request instance into just a plist before pass to Clack application."
  (loop for dispatcher in *dispatch-table*
        for action = (funcall dispatcher (request->plist request))
        when action :return (funcall action)
        finally (setf (return-code *reply*) +http-not-found+)))

(defun handle-response (res)
  "Convert Response from Clack application into a string
before pass to Hunchentoot."
  (destructuring-bind (status headers body) res
    (etypecase body
      (pathname
       (hunchentoot:handle-static-file body))
      (list
       (setf (return-code*) status)
       (awhen (getf headers :content-type)
         (setf (content-type*) it))
       (awhen (getf headers :content-length)
         (setf (content-length*) it))
       (with-output-to-string (s)
         (dolist (el body) (write-line el s)))))))

(defun cookie->plist (cookie)
  "Convert Hunchentoot's cookie class into just a plist."
  (list
   :name (cookie-name cookie)
   :value (cookie-value cookie)
   :expires (cookie-expires cookie)
   :path (cookie-path cookie)
   :domain (cookie-domain cookie)
   :secure (cookie-secure cookie)
   :http-only (cookie-http-only cookie)))

(defun request->plist (req)
  "Convert Request from server into a plist
before pass to Clack application."
  (destructuring-bind (server-name server-port)
      (split-sequence #\: (host req) :from-end t)
    (list
     :request-method (request-method* req)
     :script-name ""
     :path-info (script-name* req)
     :query-string (or (query-string* req) "")
     :server-name server-name
     :server-port server-port
     :request-uri (request-uri* req)
     :server-protocol (server-protocol* req)
     :http-user-agent (user-agent req)
     :http-remote-addr (remote-addr* req)
     :http-remote-port (remote-port* req)
     :http-referer (referer req)
     :http-host (host req)
     :http-cookies (alist-plist (cookies-in* req))
     :http-server :hunchentoot
     :%request req)))

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
  (let ((*debugging-p* t))
    (handler-case (call-next-method)
      #+sbcl (sb-sys:io-timeout (condition) (values nil condition))
      (error (condition) (invoke-debugger condition)))))

(defmethod acceptor-request-dispatcher ((*acceptor* <debuggable-acceptor>))
  (let ((dispatcher (call-next-method)))
    (lambda (request)
      (handler-bind ((error #'invoke-debugger))
        (funcall dispatcher request)))))
