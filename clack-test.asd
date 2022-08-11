(defsystem "clack-test"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("clack"
               "clack-handler-hunchentoot"
               "rove"
               "bordeaux-threads"
               "ironclad"
               "usocket"
               "dexador"
               "flexi-streams"
               "http-body")
  :components ((:file "src/test")
               (:file "src/test/suite" :depends-on ("src/test")))
  :description "Testing Clack Applications.")
