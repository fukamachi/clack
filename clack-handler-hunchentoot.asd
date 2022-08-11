(defsystem "clack-handler-hunchentoot"
  :version "0.5.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("hunchentoot"
               "clack-socket"
               "flexi-streams"
               "bordeaux-threads"
               "split-sequence"
               "alexandria")
  :components ((:file "src/handler/hunchentoot"))
  :description "Clack handler for Hunchentoot.")
