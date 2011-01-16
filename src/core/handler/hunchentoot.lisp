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
        :hunchentoot
        :split-sequence
        :alexandria)
  (:export :run))

(in-package :clack.handler.hunchentoot)

(defun run (app &key debug (port 8080))
  "Start Hunchentoot server."
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (push #'(lambda (req)
            #'(lambda () (handle-response (funcall app req)))) *dispatch-table*)
  (start (make-instance 'acceptor
            :port port
            :request-dispatcher 'clack-request-dispatcher)))

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
  (destructuring-bind (status header body) res
    (if (typep body 'pathname)
        (hunchentoot:handle-static-file body)
        (progn
          (setf (return-code*) status)
          (let ((content-type (getf header :content-type)))
            (when content-type
              (setf (content-type*) content-type)))
          (let ((content-length (getf header :content-length)))
            (when content-length
              (setf (content-length*) content-length)))
          (if (consp body)
              (with-output-to-string (s)
                (dolist (el body) (princ el s)))
              body)))))

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
     :script-name (script-name* req)
     :path-info ""
     :request-uri (request-uri* req)
     :query-string (or (query-string* req) "")
     :server-name server-name
     :server-port server-port
     :server-protocol (server-protocol* req)
     :cookies (alist-plist (cookies-in* req))
     :%request req)))
