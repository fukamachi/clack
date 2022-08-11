(defsystem "clack-handler-wookie"
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("wookie"
               "clack-socket"
               "cl-async"
               "fast-http"
               "quri"
               "flexi-streams"
               "babel"
               "fast-io"
               "split-sequence"
               "alexandria")
  :components ((:file "src/handler/wookie"))
  :description "Clack handler for Wookie.")
