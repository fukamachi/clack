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

(defclass <middleware> () ()
  (:documentation "Class for Clack Middleware."))

(defmethod call ((mw <middleware>) env)
  "Invoke this Middleware.")

(defgeneric wrap (middleware app-or-middleware &rest args)
  (:documentation
   "Compose this and given application or middleware instance into one function.
The function takes `<environment>'. Default behavior returns given application.
This should be overrided by subclasses."))

(defmethod wrap ((mw <middleware>) app &rest args) app)
(defmethod wrap ((mw1 <middleware>) (mw2 <middleware>) &rest args)
  (lambda (env) (call mw2 env)))
