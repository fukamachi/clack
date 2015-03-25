(in-package :cl-user)
(defpackage clack.middleware.conditional
  (:nicknames :lack.middleware.condition)
  (:use :cl
        :clack.component
        :clack.middleware))
(in-package :clack.middleware.conditional)

(cl-syntax:use-syntax :annot)

@export
(defparameter *lack-middleware-condition*
  (lambda (app cond &key builder)
    (wrap
     (make-instance '<clack-middleware-conditional>
                    :condition cond
                    :builder builder)
     app)))

@export
(defclass <clack-middleware-conditional> (<middleware>)
     ((condition :type component-designator
                 :initarg :condition)
      (builder :type (or component-designator symbol list)
               :initarg :builder)
      (middleware :type (or function <component>))))

(defmethod initialize-instance :after ((this <clack-middleware-conditional>) &key)
  (with-slots (builder) this
     (setf builder
           (typecase builder
             (symbol (make-instance builder))
             (list (apply #'make-instance builder))
             (t builder)))))

(defmethod wrap ((this <clack-middleware-conditional>) app)
  (with-slots (middleware builder) this
     (setf middleware
           (wrap builder app))
     (call-next-method)))

(defmethod call ((this <clack-middleware-conditional>) env)
  (with-slots (condition middleware) this
     (if (call condition env)
         (call middleware env)
         (call-next this env))))
