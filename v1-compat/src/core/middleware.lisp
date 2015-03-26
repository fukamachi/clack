(in-package :cl-user)
(defpackage clack.middleware
  (:use :cl)
  (:import-from :clack.component
                :<component>
                :component-designator
                :call)
  (:export :<middleware>
           :call-next
           :wrap))
(in-package :clack.middleware)

(defclass <middleware> (<component>)
  ((app :type component-designator
        :initarg :app
        :reader app)))

(defgeneric call-next (mw env)
  (:method ((mw <middleware>) env)
    (call (app mw) env)))

(defgeneric wrap (mw app-or-middleware)
  (:method ((mw <middleware>) app-or-middleware)
    (setf (slot-value mw 'app) app-or-middleware)
    (lambda (env) (call mw env)))
  (:method ((mw function) app-or-middleware)
    (funcall mw app-or-middleware)))

(let ((symbols '(<middleware>
                 call-next
                 wrap)))
  (import symbols (find-package :clack))
  (export symbols (find-package :clack)))
