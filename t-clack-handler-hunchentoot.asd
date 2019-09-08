(defsystem "t-clack-handler-hunchentoot"
  :depends-on ("clack-handler-hunchentoot"
               "clack-test")
  :components
  ((:file "t/handler/hunchentoot"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
