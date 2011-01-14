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

(defun call (raw-req app)
  (let* ((req (make-instance '<request>
                 :request-method (request-method raw-req)
                 :script-name (script-name raw-req)
                 :path-info (request-uri raw-req)
                 :request-uri (request-uri raw-req)
                 :query-string (query-string raw-req)
                 :server-name (host raw-req)
                 :server-port (remote-port raw-req)
                 :server-protocol (server-protocol raw-req)
                 :request raw-req)))
    (destructuring-bind (status header body)
        (res (call app req))
      (setf (content-type raw-req) (assoc :content-type header))
      (setf (content-length raw-req) (assoc :content-length header))
      (with-output-to-string (s)
        (dolist (str body) (princ str s))))))
