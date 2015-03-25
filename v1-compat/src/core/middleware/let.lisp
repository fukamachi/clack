(in-package :cl-user)
(defpackage clack.middleware.let
  (:use :cl
        :clack.component
        :clack.middleware))
(in-package :clack.middleware.let)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-let> (<middleware>)
  ((bindings :initarg :bindings)))

(defmethod call ((this <clack-middleware-let>) env)
  (eval
   `(let ,(slot-value this 'bindings)
      (call-next ,this ',env))))
