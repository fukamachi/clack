(in-package :cl-user)
(defpackage clack-socket-asd
  (:use :cl :asdf))
(in-package :clack-socket-asd)

(defsystem clack-socket
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :components ((:file "src/socket")))
