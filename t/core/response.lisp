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

(plan 14)

(setq res (make-response 200))

(is (headers res) nil "headers")
(is (headers res :content-type) nil "content-type")
(setf (headers res :content-type) "text/html")
(is (headers res :content-type) "text/html" "content-type 2")

(is (body res) nil "body")
(setf (body res) "aiueo")
(is (body res) '("aiueo") "body 2")

(is (finalize res) `(200 (:content-type "text/html") ("aiueo")) "finalize")

(diag "redirect")
(redirect res "http://www.facebook.com/eitarow.fukamachi")
(is (headers res :content-type) "text/html" "content-type")
(is (headers res :location) "http://www.facebook.com/eitarow.fukamachi" "location")
(is (body res) '("aiueo") "body")
(is (status res) 302 "status")

(diag "cookies")
(is (cookies res) nil "cookies")
(setf (cookies res "hoge") "a")
(is (cookies res) '(:|hoge| (:value "a")) "cookies")
(is (cookies res "hoge") "a" "cookie value")
(is (getf (nth 1 (finalize res)) :set-cookie) "hoge=a" "finalize cookie")

(cl-test-more:finalize)
