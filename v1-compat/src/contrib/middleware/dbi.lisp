(in-package :cl-user)
(defpackage clack.middleware.dbi
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :dbi
                :connect
                :disconnect))
(in-package :clack.middleware.dbi)

(cl-syntax:use-syntax :annot)

@export
(defvar *db* nil)

@export
(defclass <clack-middleware-dbi> (<middleware>)
     ((driver-name :initarg :driver-name
                   :accessor driver-name)
      (connect-args :initarg :connect-args
                    :accessor connect-args)))

(defmethod call ((this <clack-middleware-dbi>) env)
  (let ((*db* (apply #'dbi:connect (cons (driver-name this)
                                         (connect-args this)))))
    (unwind-protect (call-next this env)
      (dbi:disconnect *db*))))
