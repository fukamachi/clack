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

(defun run (app &rest params &allow-other-keys)
  "Start server."
  (apply #'clack.handler.hunchentoot:run app params))

(defmacro builder (&rest app-or-middleware)
  "Wrap Clack application with middlewares and return it as one function."
  (let ((args (gensym "args")))
  `(reduce (lambda (&rest ,args) (wrap (apply #'make-instance ,args)))
           ',(butlast app-or-middleware)
           :initial-value ,(car (last app-or-middleware))
           :from-end t)))
