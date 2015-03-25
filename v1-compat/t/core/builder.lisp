(in-package :cl-user)
(defpackage t.clack.builder
  (:use :cl
        :prove
        :clack.builder
        :clack.component
        :clack.middleware))
(in-package :t.clack.builder)

;; initialize

(defvar app
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/html") "ok from app")))

(defmethod call ((this <middleware>) env)
  (if (string= "/private" (getf env :path-info))
      '(403 nil ("forbidden"))
      (call-next this env)))

(defclass <simple-middleware> (<middleware>) ()
  (:documentation "Middleware for unit testing."))

(defclass <simple-middleware2> (<middleware>)
     ((one :initarg :one)
      (two :initarg :two))
  (:documentation "Middleware with some slots."))

;; Tests

(plan 8)

(is-type (builder <simple-middleware> app) 'function "builder")
(is-type (builder
          (<simple-middleware2>
           :one "1" :two "2")
          app)
         'function
         "builder with args")

(is-type (builder
          (if t
              (make-instance '<simple-middleware>)
              nil)
          app)
         'function
         "CL code can be embed")

(let ((mount-app (builder
                  (:mount "/admin" (lambda (env) `(200 () ("admin" ,(getf env :path-info)))))
                  (lambda (env)
                    `(200 () ("default" ,(getf env :path-info)))))))
  (is (call mount-app '(:path-info "/login"))
      '(200 () ("default" "/login")))
  (is (call mount-app '(:path-info "/admin/login"))
      '(200 () ("admin" "/login")))
  (is (call mount-app '(:path-info "/admin"))
      '(200 () ("admin" "/")))
  (is (call mount-app '(:path-info "/admin/"))
      '(200 () ("admin" "/")))
  (is (call mount-app '(:path-info "/administrators"))
      '(200 () ("default" "/administrators"))))

(finalize)
