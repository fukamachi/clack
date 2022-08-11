(defsystem "clack-handler-toot"
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("toot"
               "flexi-streams"
               "bordeaux-threads"
               "cl-ppcre"
               "split-sequence"
               "alexandria")
  :components ((:file "src/handler/toot"))
  :description "Clack handler for Toot.")
