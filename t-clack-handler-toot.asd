(in-package :cl-user)
(defpackage t-clack-handler-toot-asd
  (:use :cl :asdf))
(in-package :t-clack-handler-toot-asd)

(defsystem t-clack-handler-toot
  :depends-on (:clack-handler-toot
               :clack-test)
  :components
  ((:test-file "t/handler/toot"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
