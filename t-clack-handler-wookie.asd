(defsystem "t-clack-handler-wookie"
  :depends-on (;; Some environment cannot load Wookie due to like non FFI support.
               ;; :clack-handler-wookie
               "clack-test")
  :components
  ((:file "t/handler/wookie"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
