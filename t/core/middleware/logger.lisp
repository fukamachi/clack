(in-package :cl-user)
(defpackage t.clack.middleware.logger
  (:use :cl
        :prove
        :clack.test
        :clack.builder
        :clack.logger
        :clack.middleware.logger
        :clack.logger.stream
        :clack.logger.file
        :drakma)
  (:import-from :cl-fad
                :file-exists-p))
(in-package :t.clack.middleware.logger)

(plan 6)

#+thread-support
(test-app
 (builder
  (lambda (env)
    (declare (ignore env))
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
#-thread-support
(skip 2 "because your lisp doesn't support threads")

#+thread-support
(let ((stream (make-string-output-stream)))
  (test-app
   (builder
    (<clack-middleware-logger>
     :logger #'(lambda (message) (princ message stream)))
    (lambda (env)
      (declare (ignore env))
      (log-message :notice "hoge")
      '(200 nil nil)))
   (lambda ()
     (setf *logger-min-level* +notice+)
     (http-request "http://localhost:4242/")
     (like (get-output-stream-string stream) "\\[NOTICE\\] hoge$"))
   "Can treat a function as a logger."))
#-thread-support
(skip 1 "because your lisp doesn't support threads")

(defvar *log-pathname*
    (asdf:system-relative-pathname (asdf:find-system :clack) #p"t/pongi.log"))

#+thread-support
(test-app
 (builder
  (<clack-middleware-logger>
   :logger (make-instance '<clack-logger-file>
              :output-file *log-pathname*))
  (lambda (env)
    (declare (ignore env))
    (log-message :error "fuga")
    '(200 nil nil)))
 (lambda ()
   (http-request "http://localhost:4242/")
   (ok (file-exists-p *log-pathname*))
   (delete-file *log-pathname*))
 "clack-logger-file")
#-thread-support
(skip 1 "because your lisp doesn't support threads")

#+thread-support
(let ((stream (make-string-output-stream)))
  (test-app
   (builder
    (<clack-middleware-logger>
     :logger (make-instance '<clack-logger-file>
                :output-file *log-pathname*))
    (<clack-middleware-logger>
     :logger (make-instance '<clack-logger-stream>
                :output-stream stream))
    (lambda (env)
      (declare (ignore env))
      (log-message :error "fuga")
      '(200 nil nil)))
   (lambda ()
     (http-request "http://localhost:4242/")
     (ok (file-exists-p *log-pathname*))
     (like (get-output-stream-string stream) "\\[ERROR\\] fuga")
     (delete-file *log-pathname*))
   "multiple loggers"))
#-thread-support
(skip 2 "because your lisp doesn't support threads")

(finalize)
