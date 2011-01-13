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

(in-package :clack.middleware)

(defclass <middleware> () ()
  (:documentation "Class for Clack Middleware."))

(defmethod call ((mw <middleware>) (env <environment>))
  "Invoke this Middleware.")

(defmethod build ((mw <middleware>) app) app)
(defmethod build ((mw1 <middleware>) (mw2 <middleware>))
  (lambda (env) (call mw2 env)))
