#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Class for Clack Middleware.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack)

(defclass <middleware> ()
     ((app :initarg :app :reader app))
  (:documentation "Class for Clack Middleware."))

(defmethod call ((mw <middleware>) req)
  "Invoke this Middleware. Designed to override in subclasses.")

(defmethod wrap ((mw <middleware>) app-or-middleware)
  "Compose this and given application or middleware instance into one function.
The function takes request plist."
  (setf (slot-value mw 'app) app-or-middleware)
  (lambda (req) (call mw req)))
