(in-package :cl-user)
(defpackage t.clack.middleware.auth.basic
  (:use :cl
        :prove
        :clack.test
        :clack.builder
        :clack.middleware.auth.basic
        :drakma))
(in-package :t.clack.middleware.auth.basic)

(plan 4)

#+thread-support
(subtest-app "auth basic"
    (builder
     (<clack-middleware-auth-basic>
      :authenticator
      #'(lambda (user pass)
          (and (string= user "hoge")
               (string= pass "fuga"))))
     (lambda (env)
       `(200 nil (,(format nil "Hello, ~A" (getf env :remote-user))))))
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
      "Hello, hoge"))

#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
