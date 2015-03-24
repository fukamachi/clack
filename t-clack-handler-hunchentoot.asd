(in-package :cl-user)
(defpackage t-clack-handler-hunchentoot-asd
  (:use :cl :asdf))
(in-package :t-clack-handler-hunchentoot-asd)

(defsystem t-clack-handler-hunchentoot
  :depends-on (:clack-handler-hunchentoot
               :clack-test)
  :components
  ((:test-file "t/handler/hunchentoot"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
