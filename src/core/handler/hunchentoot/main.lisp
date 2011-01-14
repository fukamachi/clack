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

(in-package :clack.handler.hunchentoot)

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
     :%request req)))

(defun response->string (res)
  "Convert Response from Clack application into a string
before pass to Hunchentoot."
  (destructuring-bind (status header body) res
    (let ((content-type (getf header :content-type)))
      (when content-type
        (setf (content-type*) content-type)))
    (let ((content-length (getf header :content-length)))
      (when content-length
        (setf (content-length*) content-length)))
    (with-output-to-string (s)
      (dolist (str body) (princ str s)))))

(defun clack-request-dispatcher (request)
  "Hunchentoot request dispatcher for Clack. Most of this is same as
list-request-dispatcher, default one in Hunchentoot, except for convert
Request instance into just a plist before pass to Clack application."
  (loop :for dispatcher :in *dispatch-table*
        :for action = (funcall dispatcher (request->plist request))
        :when action :return (funcall action)
        :finally (setf (return-code *reply*) +http-not-found+)))

(defun run (app &key debug (port 8080))
  "Start Hunchentoot server."
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (push (lambda (req) (lambda () (call req app))) *dispatch-table*)
  (start (make-instance 'acceptor
            :port port
            :request-dispatcher 'clack-request-dispatcher)))

(defun call (req app)
  "This function called on each request and returns a string to response
to a browser."
  (response->string (funcall app req)))
