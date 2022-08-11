(defsystem "clack-handler-fcgi"
  :version "0.4.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("cl-fastcgi"
               "alexandria"
               "flexi-streams"
               "usocket"
               "quri")
  :components ((:file "src/handler/fcgi"))
  :description "Clack handler for FastCGI.")
