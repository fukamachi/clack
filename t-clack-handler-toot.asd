(defsystem "t-clack-handler-toot"
  :depends-on ("clack-handler-toot"
               "clack-test")
  :components
  ((:file "t/handler/toot"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
