(in-package :cl-user)

(defpackage clack-test.middleware
  (:use :cl
        :cl-test-more
        :clack.component
        :clack.middleware))

(in-package :clack-test.middleware)

(plan 5)

(defclass <test-middleware> (<middleware>) ()
  (:documentation "Middleware for unit testing."))

(defvar mw (make-instance '<test-middleware>))

(ok mw "can create")
(is-error (call mw nil) simple-error "simple-error if call it")

;; implement `call'.
(defmethod call ((this <test-middleware>) req)
  (if (string= "/private" (getf req :path-info))
      '(403 nil ("forbidden"))
      (call-next this req)))

(defvar mw2 (wrap mw (lambda (req) (declare (ignore req)) '(200 nil ("ok")))))

(is-type mw2 'function "wrap returns a function.")
(is (call mw2 '(:path-info "/"))
    '(200 nil ("ok"))
    "call-next")
(is (call mw2 '(:path-info "/private"))
    '(403 nil ("forbidden"))
    "response from middleware")

(finalize)
