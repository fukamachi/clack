(in-package :cl-user)
(defpackage t-clack-middleware-auth-basic-asd
  (:use :cl :asdf))
(in-package :t-clack-middleware-auth-basic-asd)

(defsystem t-clack-middleware-auth-basic
  :depends-on (:clack-test
               :clack-middleware-auth-basic
               :clack
               :prove
               :drakma)
  :components
  ((:test-file "t/contrib/middleware/auth/basic"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
