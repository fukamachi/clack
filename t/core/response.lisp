(in-package :cl-user)

(defpackage clack-test.response
  (:use :cl
        :cl-test-more
        :clack.response)
  (:shadowing-import-from
   :clack.response
   :finalize))

(in-package :clack-test.response)

(defvar res nil)

(plan 6)

(setq res (make-response 200))

(is (headers res) nil "headers")
(is (header res :content-type) nil "content-type")
(setf (header res :content-type) "text/html")
(is (header res :content-type) "text/html" "content-type 2")

(is (body res) nil "body")
(setf (body res) "aiueo")
(is (body res) '("aiueo") "body 2")

(is (finalize res) `(200 (:content-type "text/html") ("aiueo")) "finalize")

(cl-test-more:finalize)
