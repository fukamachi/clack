(in-package :cl-user)
(defpackage t.clack.component
  (:use :cl
        :prove
        :clack.component))
(in-package :t.clack.component)

(plan 5)

(defclass <test-component> (<component>) ()
  (:documentation "Component for unit testing."))

(defvar comp (make-instance '<test-component>))

(ok comp "can create")
(is-error (call comp nil)
          #+ccl-1.9 ccl::no-applicable-method-exists
          #-ccl-1.9 simple-error
          "simple-error if call it")

;; implement `call'.
(defmethod call ((this <test-component>) env)
  `(200 nil (,(getf env :path-info))))

(is (call comp '(:path-info "/")) '(200 nil ("/")) "can call")
(is-type (make-app comp) 'function "make-app")
(is (funcall (make-app comp) '(:path-info "/hoge"))
    '(200 nil ("/hoge")) "funcall a function made by make-app")

(finalize)
