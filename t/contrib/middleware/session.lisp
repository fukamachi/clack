(clack.util:namespace clack-test.middleware.session
  (:use :cl
        :anaphora
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.middleware.session
        :drakma))

(plan nil)

(setf clack.test:*clack-test-port* 4242)

(test-app
 (builder
  (<clack-middleware-session>)
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

(finalize)
