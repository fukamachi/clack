(in-package :cl-user)

(defpackage slinky-asd
  (:use :asdf))

(in-package :slinky-asd)

(defsystem slinky
  :version "1.0"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:hunchentoot :clsql :cl-markup :cl-locale)
  :components ((:module "src"
                        :serial t
                        :components ((:file "package")
                                     (:file "core")))))
