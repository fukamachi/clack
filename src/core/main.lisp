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

(defun run (app &rest params)
  "Start server."
  (apply #'clack.handler.hunchentoot:run app params))

(defmacro builder (&rest app-or-middleware)
  "Wrap Clack application with middlewares and return it as one function."
  `(reduce #'wrap
           (list ,@(loop :for arg in (butlast app-or-middleware)
                         :if (consp arg)
                           :collect `(make-instance ',(car arg) ,@(cdr arg))
                         :else :collect `(make-instance ',arg)))
           :initial-value ,(car (last app-or-middleware))
           :from-end t))

(defmethod call ((app function) req)
  "Functions should be called like Middleware."
  (funcall app req))
