(defsystem "t-clack-handler-fcgi"
  :depends-on (;; Some environment cannot load this due to like non FFI support.
               ;; :clack-handler-fcgi
               "clack-test")
  :components
  ((:file "t/handler/fcgi"))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
