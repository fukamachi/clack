(in-package :cl-user)
(defpackage t-clack-handler-fcgi-asd
  (:use :cl :asdf))
(in-package :t-clack-handler-fcgi-asd)

(defsystem t-clack-handler-fcgi
  :depends-on (;; Some environment cannot load this due to like non FFI support.
               ;; :clack-handler-fcgi
               :clack-test)
  :components
  ((:test-file "t/handler/fcgi"))
  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove) c)))
