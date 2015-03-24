(in-package :cl-user)
(defpackage t-clack-handler-wookie-asd
  (:use :cl :asdf))
(in-package :t-clack-handler-wookie-asd)

(defsystem t-clack-handler-wookie
  :depends-on (;; Some environment cannot load Wookie due to like non FFI support.
               ;; :clack-handler-wookie
               :clack-test)
  :components
  ((:test-file "t/handler/wookie"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
