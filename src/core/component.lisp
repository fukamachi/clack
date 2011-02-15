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

(clack.util:namespace clack.component
  (:use :cl))

(cl-annot:enable-annot-syntax)

@export
(defclass <component> () ()
  (:documentation "Base Class for Clack Component shared between <middleware>
and Clack Application."))

@export
(defgeneric call (comp req)
  (:documentation "Invoke component. Designed to override in subclasses."))

@export
(defmethod call ((app function) req)
  "Functions should be called like Component."
  (funcall app req))

@export
(defmethod make-app ((comp <component>))
  "Create a function to call this component."
  #'(lambda (req) (call comp req)))
