(in-package :cl-user)

(defpackage clack-test.handler.hunchentoot
  (:use :cl
        :cl-test-more
        :drakma
        :clack.handler.hunchentoot))

(in-package :clack-test.handler.hunchentoot)

(defparameter enable-thread-p nil)

#+(or (and allegro multiprocessing)
      armedbear
      (and cmu mp)
      scl
      corman
      (and digitool ccl-5.1)
      (and ecl threads)
      lispworks
      (and openmcl openmcl-native-threads)
      (and sbcl sb-thread)
      (and clisp mt))
(setf enable-thread-p t)

(if (not enable-thread-p)
    (diag "Your Lisp has no thread support. Skip Handler tests.")
    (progn
      (plan 1)
      (defvar acceptor nil)
      (defvar app
          (lambda (req) (declare (ignore req)) '(200 (:content-type "text/plain") ("ok"))))

      (setf acceptor (run app :port 4242))

      (let ((res (http-request "http://localhost:4242/")))
        (is res (format nil "ok~%") "response body"))

      (stop acceptor)

      (finalize)
      ))
