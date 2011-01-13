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

(defun run (app &key debug (port 8080))
  "Start Hunchentoot server."
  (when debug
    (setf *show-lisp-errors-p* t)
    (setf *show-lisp-backtraces-p* t))
  (push (lambda (req) (call req app)) *dispatch-table*)
  (start (make-instance 'acceptor :port port)))

(defun call (req app)
  (let* ((env (make-instance '<environment>
                 :request-method (request-method req)
                 :script-name (script-name req)
                 :path-info (request-uri req)
                 :request-uri (request-uri req)
                 :query-string (query-string req)
                 :server-name (host req)
                 :server-port (remote-port req)
                 :server-protocol (server-protocol req)
                 :request req)))
    (destructuring-bind (status header body)
        (res (call app env))
      (setf (content-type req) (assoc :content-type header))
      (setf (content-length req) (assoc :content-length header))
      (with-output-to-string (s)
        (dolist (str body) (princ str s))))))
