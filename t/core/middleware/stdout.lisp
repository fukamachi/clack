(clack.util:namespace t.clack.middleware.stdout
  (:use :cl
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.middleware.stdout
        :drakma))

(plan 5)

#+thread-support
(let ((stream (make-string-output-stream)))
  (test-app
   (builder
    (<clack-middleware-stdout>
     :standard-output stream)
    (lambda (env)
      (declare (ignore env))
      (format t "~A" "You've got an access!")
      '(200 nil ("Hi!"))))
   (lambda ()
     (http-request "http://localhost:4242/")
     (is (get-output-stream-string stream)
         "You've got an access!"))))
#-thread-support
(skip 1 "because your lisp doesn't support threads")

(defparameter *output-stream* (make-string-output-stream))

#+thread-support
(let ((stream (make-string-output-stream)))
  (test-app
   (builder
    (<clack-middleware-stdout>
     :standard-output '*output-stream*)
    (lambda (env)
      (declare (ignore env))
      (format t "~A" "You've got an access!")
      '(200 nil ("Hi!"))))
   (lambda ()
     (http-request "http://localhost:4242/")
     (is (get-output-stream-string stream)
         "")
     (is (get-output-stream-string *output-stream*)
         "You've got an access!")
     (setf *output-stream* stream)
     (http-request "http://localhost:4242/")
     (is (get-output-stream-string stream)
         "You've got an access!")
     (is (get-output-stream-string *output-stream*)
         ""))))
#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
