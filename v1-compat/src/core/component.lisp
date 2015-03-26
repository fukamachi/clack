(in-package :cl-user)
(defpackage clack.component
  (:use :cl)
  (:import-from :lack.component
                :lack-component
                :call
                :to-app)
  (:export :<component>
           :call
           :make-app
           :component-designator))
(in-package :clack.component)

(deftype component-designator () '(or function <component>))

(defclass <component> (lack-component) ())

(defgeneric make-app (comp)
  (:method ((comp <component>))
    (to-app comp)))

(let ((symbols '(<component>
                 call
                 make-app)))
  (import symbols (find-package :clack))
  (export symbols (find-package :clack)))
