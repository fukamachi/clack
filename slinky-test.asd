(in-package :cl-user)

(defpackage slinky-test-asd
  (:use :cl :asdf))

(defsystem slinky-test
  :depends-on (:slinky :cl-test-more)
  :components ((:module "t"
                        :serial t
                        :components ((:file "package")
                                     (:file "core")))))
