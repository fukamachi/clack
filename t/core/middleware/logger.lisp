(clack.util:namespace clack-test.middleware.logger
  (:use :cl
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.logger
        :clack.middleware.logger
        :drakma))

(plan nil)

(setf clack.test:*clack-test-port* 4242)

(test-app
 (builder
  (<clack-middleware-logger>)
  (lambda (req)
    (declare (ignore req))
    (log-message :notice "hoge")
    '(200 nil nil)))
 (lambda ()
   (setf *logger-min-level* +notice+)
   (http-request "http://localhost:4242/")
   (like (get-output-stream-string *logger-output*) "\\[NOTICE\\] hoge$" "min level is NOTICE")

   (setf *logger-min-level* +warning+)
   (http-request "http://localhost:4242/")
   (is (get-output-stream-string *logger-output*) "" "min level is WARNING")
   ))

(finalize)
