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

(in-package :cl)

(defpackage clack.middleware
  (:use :cl :clack.component)
  (:shadow :call)
  (:export :<middleware>
           :call
           :call-next
           :wrap))

(in-package :clack.middleware)

(defclass <middleware> (<component>)
     ((app :initarg :app :reader app))
  (:documentation "Class for Clack Middleware."))

(defmethod call ((this <middleware>) req)
  "Invoke this Middleware. This shoulb be override in subclasses.")

(defmethod call-next ((this <middleware>) req)
  "Call next middleware or application."
  (call (app this) req))

(defmethod wrap ((this <middleware>) app-or-middleware)
  "Compose this and given application or middleware instance into one function.
The function takes request plist."
  (setf (slot-value this 'app) app-or-middleware)
  #'(lambda (req) (call this req)))
