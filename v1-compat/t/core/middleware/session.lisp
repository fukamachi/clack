(in-package :cl-user)
(defpackage t.clack.middleware.session
  (:use :cl
        :prove
        :clack.test
        :clack.builder
        :clack.middleware.session
        :clack.middleware.session.cookie
        :clack.session.state.cookie
        :drakma)
  (:shadowing-import-from :prove :finalize))
(in-package :t.clack.middleware.session)

(plan 2)

#+thread-support
(subtest-app "middleware session"
    (builder
     (<clack-middleware-session> :state (make-instance '<clack-session-state-cookie>))
     (lambda (env)
       (unless (gethash :counter (getf env :clack.session))
         (setf (gethash :counter (getf env :clack.session)) 0))
       `(200
         (:content-type "text/plain")
         (,(format nil "Hello, you've been here for ~Ath times!"
                   (incf (gethash :counter (getf env :clack.session))))))))
  (let ((cookie-jar (make-instance 'cookie-jar)))
    (multiple-value-bind (body status)
        (http-request (localhost)
                      :cookie-jar cookie-jar)
      (diag "1st request")
      (is status 200)
      (is body "Hello, you've been here for 1th times!"))
    (multiple-value-bind (body status)
        (http-request (localhost)
                      :cookie-jar cookie-jar)
      (diag "2nd request")
      (is status 200)
      (is body "Hello, you've been here for 2th times!"))))

#-thread-support
(skip 4 "because your lisp doesn't support threads")

#+thread-support
(subtest-app "middleware session cookie"
    (builder
     <clack-middleware-session-cookie>
     (lambda (env)
       (unless (gethash :counter (getf env :clack.session))
         (setf (gethash :counter (getf env :clack.session)) 0))
       `(200
         (:content-type "text/plain")
         (,(format nil "Hello, you've been here for ~Ath times!"
                   (incf (gethash :counter (getf env :clack.session))))))))
  (let ((cookie-jar (make-instance 'cookie-jar)))
    (multiple-value-bind (body status)
        (http-request (localhost)
                      :cookie-jar cookie-jar)
      (diag "1st request")
      (is status 200)
      (is body "Hello, you've been here for 1th times!"))
    (multiple-value-bind (body status)
        (http-request (localhost)
                      :cookie-jar cookie-jar)
      (diag "2nd request")
      (is status 200)
      (is body "Hello, you've been here for 2th times!"))))

#-thread-support
(skip 2 "because your lisp doesn't support threads")

(finalize)
