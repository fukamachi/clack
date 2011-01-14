#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Base Class for Clack Component shared between <middleware> and
    Clack Application.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack)

(defclass <component> () ()
  (:documentation "Base Class for Clack Component shared between <middleware>
and Clack Application."))

(defmethod call ((comp <component>) req)
  "Invoke component. Designed to override in subclasses.")

(defmethod make-app ((comp <component>))
  "Create a function to call this component."
  (lambda (req) (call comp req)))
