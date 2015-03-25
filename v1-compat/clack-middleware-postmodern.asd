
(in-package :cl-user)
(defpackage :clack-middleware-postmodern-asd
  (:use :cl :asdf))
(in-package :clack-middleware-postmodern-asd)

(defsystem clack-middleware-postmodern
  :version "0.1"
  :author "Karl Heinrichmeyer"
  :license "BSD2"
  :depends-on (:clack-v1-compat
               :cl-syntax
               :cl-syntax-annot
               :postmodern)
  :components ((:file "src/contrib/middleware/postmodern"))
  :description "Middleware for POSTMODERN connection management")
