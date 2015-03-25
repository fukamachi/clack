(in-package :cl-user)
(defpackage t-clack-v1-compat-asd
  (:use :cl :asdf))
(in-package :t-clack-v1-compat-asd)

(defsystem t-clack-v1-compat
  :depends-on (:clack-test
               :clack-v1-compat
               :prove
               :drakma)
  :components
  ((:module "v1-compat/t/core"
    :components
    ((:test-file "builder")
     (:test-file "request")
     (:test-file "response")
     (:test-file "app/file")
     (:test-file "app/urlmap")
     (:test-file "middleware/conditional")
     (:test-file "middleware/session")
     (:test-file "middleware/static"))))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
