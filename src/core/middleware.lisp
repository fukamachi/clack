#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware
  (:use :cl)
  (:import-from :clack.component
                :component-designator
                :<component>
                :call))
(in-package :clack.middleware)

(cl-syntax:use-syntax :annot)

@export
(defclass <middleware> (<component>)
     ((app :type component-designator
           :initarg :app
           :reader app))
  (:documentation "Class for Clack Middleware."))

@export
(defgeneric call-next (mw env)
  (:documentation
   "Call next middleware or application.")
  (:method ((this <middleware>) env)
    (call (app this) env)))

@export
(defgeneric wrap (mw app-or-middleware)
  (:documentation
   "Compose `this' and the given application or middleware instance into one function.
This function takes a request plist.")
  (:method ((this <middleware>) app-or-middleware)
    (setf (slot-value this 'app) app-or-middleware)
    #'(lambda (env) (call this env)))
  (:method ((this function) app-or-middleware)
    (funcall this app-or-middleware)))

(doc:start)

@doc:NAME "
Clack.Middleware - Base Class for Clack Middleware.
"

@doc:SYNOPSIS "
    (in-package :cl-user)
    (defpackage clack.middleware.example
      (:use :cl :clack)
      (:export :<clack-middleware-example>))
    (in-package :clack.middleware.example)
    
    (defclass <clack-middleware-example> (<middleware>) ())
    (defmethod call ((this <clack-middleware-example>) env)
      ;; pre-processing `env'
      (let ((res (call-next this env)))
        ;; post-processing `res'
        res))
"

@doc:DESCRIPTION "
Clack.Middleware is the base class to write Clack Middleware.

All you have to do is to inherit from `<middleware>' and then implement the callback `call' method to do the actual work.

Middleware is similar to ':around' method of CLOS. You can delegate some processes to Application (or next Middleware) to call `call-next'.
"

@doc:AUTHOR "
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
