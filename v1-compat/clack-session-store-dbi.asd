(in-package :cl-user)
(defpackage clack-session-store-dbi-asd
  (:use :cl :asdf))
(in-package :clack-session-store-dbi-asd)

(defsystem clack-session-store-dbi
  :version "0.1.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :dbi
               :cl-base64
               :marshal)
  :components ((:file "src/contrib/session/store/dbi"))
  :description "CL-DBI-based session store for Clack.Middleware.Session")
