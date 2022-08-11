(defsystem "clack"
  :version "2.1.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("lack"
               "lack-middleware-backtrace"
               "lack-util"
               "bordeaux-threads"
               "usocket"
               "swank"
               "alexandria"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "clack" :depends-on ("handler" "util"))
                 (:file "handler" :depends-on ("util"))
                 (:file "util"))))
  :description "Web application environment for Common Lisp")
