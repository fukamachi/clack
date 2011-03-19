(clack.util:namespace clack-test.middleware.auth.basic
  (:use :cl
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.middleware.auth.basic
        :drakma))

(plan 4)

#+thread-support
(test-app
 (builder
  (<clack-middleware-auth-basic>
   :authenticator
   #'(lambda (user pass)
       (and (string= user "hoge")
            (string= pass "fuga"))))
  (lambda (req)
    `(200 nil (,(format nil "Hello, ~A" (getf req :remote-user))))))
 (lambda ()
   (is (http-request "http://localhost:4242/")
       "Authorization required")
   (is (http-request "http://localhost:4242/"
                     :basic-authorization '("" ""))
       "Authorization required")
   (is (http-request "http://localhost:4242/"
                     :basic-authorization '("wrong" "auth"))
       "Authorization required")
   (is (http-request "http://localhost:4242/"
                     :basic-authorization '("hoge" "fuga"))
       "Hello, hoge")))

#-thread-support
(skip 2 "because your lisp doesn't support threads")

(finalize)
