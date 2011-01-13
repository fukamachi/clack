#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Functions of Clack Web server.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack)

(defun run (app &key debug (port 8080))
  "Start Hunchentoot server."
  (when debug
    (setf hunchentoot:*show-lisp-errors-p* t)
    (setf hunchentoot:*show-lisp-backtraces-p* t))
  (push app hunchentoot:*dispatch-table*)
  (hunchentoot:start (make-instance 'hunchentoot:acceptor :port port)))
