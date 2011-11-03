#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.component
  (:use :cl))

(cl-syntax:use-syntax :annot)

@export
(defclass <component> () ()
  (:documentation
   "Base Class for Clack Component shared between <middleware> and Clack Application."))

@export
(defgeneric call (comp env)
  (:documentation "Invoke component. Designed to override in subclasses."))

@export
(defmethod call ((app function) env)
  "Functions should be called like Component."
  (funcall app env))

@export
(defmethod make-app ((comp <component>))
  "Create a function to call this component."
  #'(lambda (env) (call comp env)))

(doc:start)

@doc:NAME "
Clack.Component - Base Class for Clack Component.
"

@doc:SYNOPSIS "
    (in-package :cl-user)
    (defpackage clack.app.example
      (:use :cl :clack)
      (:export :<clack-app-example>))
    (in-package :clack.app.example)
    
    (defclass <clack-app-example> (<component>) ())
    (defmethod call ((this <clack-app-example>) env)
      (declare (ignore this env))
      `(200 (:content-type \"text/plain\") (\"Hello, World!\")))
"

@doc:DESCRIPTION "
Clack.Component is the base class shared between Clack.Middleware and Clack Application.

You must implement `clack.component:call' as a method which is called when HTTP request coming and returns a response.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware
"
