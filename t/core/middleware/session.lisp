(clack.util:namespace clack-test.middleware.session
  (:use :cl
        :anaphora
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.middleware.session
        :clack.session.state.cookie
        :drakma)
  (:shadowing-import-from :cl-test-more :finalize))

(plan 4)

#+thread-support
(test-app
 (builder
  (<clack-middleware-session> :state (make-instance '<clack-session-state-cookie>))
  (lambda (req)
    (sunless (gethash :counter (getf req :clack.session))
      (setf it 0))
    `(200
      (:content-type "text/plain")
      (,(format nil "Hello, you've been here for ~Ath times!"
                (incf (gethash :counter (getf req :clack.session))))))))
 (lambda ()
   (let ((cookie-jar (make-instance 'cookie-jar)))
     (multiple-value-bind (body status)
         (http-request "http://localhost:4242/"
                       :cookie-jar cookie-jar)
       (diag "1st request")
       (is status 200)
       (is body "Hello, you've been here for 1th times!"))
     (multiple-value-bind (body status)
         (http-request "http://localhost:4242/"
                       :cookie-jar cookie-jar)
       (diag "2nd request")
       (is status 200)
       (is body "Hello, you've been here for 2th times!")))))

#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
