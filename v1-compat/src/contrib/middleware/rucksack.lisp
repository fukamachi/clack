(in-package :cl-user)
(defpackage clack.middleware.rucksack
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :rucksack
                :with-transaction
                :current-rucksack))
(in-package :clack.middleware.rucksack)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-rucksack> (<middleware>)
     ((rucksack :type rs:rucksack
                :initarg :rucksack
                :initform (current-rucksack)
                :accessor rucksack)))

(defmethod call ((this <clack-middleware-rucksack>) env)
  (with-transaction (:rs (rucksack this))
    (call-next this env)))
