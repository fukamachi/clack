#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware
  (:use :cl)
  (:import-from :clack.component
                :<component>
                :call))

(cl-annot:enable-annot-syntax)

@export
(defclass <middleware> (<component>)
     ((app :initarg :app :reader app))
  (:documentation "Class for Clack Middleware."))

@export
(defmethod call-next ((this <middleware>) req)
  "Call next middleware or application."
  (call (app this) req))

@export
(defmethod wrap ((this <middleware>) app-or-middleware)
  "Compose this and given application or middleware instance into one function.
The function takes request plist."
  (setf (slot-value this 'app) app-or-middleware)
  #'(lambda (req) (call this req)))

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
    (defmethod call ((this <clack-middleware-example>) req)
      ;; pre-processing `req'
      (let ((res (call-next this req)))
        ;; post-processing `res'
        res))
"

@doc:DESCRIPTION "
Clack.Middleware is the base class to write Clack Middleware.

All you have to do is to inherit from `<middleware>' and then implement the callback `call' method to do the actual work.

Middleware is similar to ':around' method of CLOS. You can delegate some processes to Application (or next Middleware) to call `call-next'.
"

@doc:AUTHOR "
  Author: Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
"
