(in-package :cl-user)

(defpackage clack-test.builder
  (:use :cl
        :cl-test-more
        :clack.builder
        :clack.middleware))

(in-package :clack-test.builder)

;; initialize

(defvar app
    (lambda (req)
      (declare (ignore req))
      '(200 (:content-type "text/html") "ok from app")))

(defmethod call ((this <middleware>) req)
  (if (string= "/private" (getf req :path-info))
      '(403 nil ("forbidden"))
      (call-next this req)))

(defclass <simple-middleware> (<middleware>) ()
  (:documentation "Middleware for unit testing."))

(defclass <simple-middleware2> (<middleware>)
     ((one :initarg :one)
      (two :initarg :two))
  (:documentation "Middleware with some slots."))

;; Tests

(plan 3)

(is-type (builder <simple-middleware> app) 'function "builder")
(is-type (builder-lazy <simple-middleware> app) 'function "builder-lazy")
(is-type (builder
          (<simple-middleware2>
           :one "1" :two "2")
          app)
         'function
         "builder with args")

(finalize)
