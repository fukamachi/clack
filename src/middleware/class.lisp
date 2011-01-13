#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Class of Clack Middleware.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.middleware)

(defclass <middleware> () ()
  (:documentation "Base class of Slinky Middleware."))

(defmethod initialize-instance :after ((mw <middleware>) &rest initargs)
  "Collect new instance of middleware into `*middlewares*'."
  (push mw *middlewares*))

(defmethod call ((mw <middleware>) env))
