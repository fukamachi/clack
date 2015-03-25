(in-package :cl-user)
(defpackage clack-v1-compat-asd
  (:use :cl :asdf))
(in-package :clack-v1-compat-asd)

(defsystem clack-v1-compat
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-util
               :clack
               :local-time
               :trivial-backtrace
               :marshal
               :cl-base64
               :cl-ppcre
               :cl-fad
               :quri
               :trivial-mimes
               :ironclad
               :cl-syntax-annot
               :alexandria
               :split-sequence)
  :components ((:module "v1-compat/src/core"
                :components
                ((:file "builder")
                 (:file "component")
                 (:file "middleware" :depends-on ("component"))
                 (:module "middleware-components"
                  :pathname "middleware"
                  :depends-on ("component" "middleware" "app")
                  :components
                  ((:file "accesslog")
                   (:file "backtrace")
                   (:file "conditional")
                   (:file "let")
                   (:file "static")
                   (:module "session-middleware"
                    :pathname "session"
                    :components
                    ((:file "session" :depends-on ("state" "state/cookie" "store"))
                     (:file "session/cookie" :depends-on ("session"))
                     (:file "state")
                     (:file "state/cookie" :depends-on ("state"))
                     (:file "store")))))
                 (:module "app"
                  :depends-on ("component")
                  :components
                  ((:file "directory" :depends-on ("file"))
                   (:file "file")
                   (:file "urlmap")))))))
