(in-package :cl-user)

(defpackage clack-test.middleware.static
  (:use :cl
        :asdf
        :cl-test-more
        :clack.test
        :clack.builder
        :clack.middleware.static
        :drakma))

(in-package :clack-test.middleware.static)

(plan 8)

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

(test-app
 (builder
  (<clack-middleware-static>
   :path "/public/"
   :root (merge-pathnames #p"tmp/" *clack-pathname*))
  (lambda (req)
    (declare (ignore req))
    `(200 (:content-type "text/plain") ("Happy Valentine!"))))
 (lambda ()
   (multiple-value-bind (body status headers)
       (http-request "http://localhost:4242/public/jellyfish.jpg")
     (is status 200)
     (is (cdr (assoc :content-type headers)) "image/jpeg")
     (is (length body) 139616))
   (multiple-value-bind (body status)
       (http-request "http://localhost:4242/public/hoge.png")
     (is status 404)
     (is body "not found"))
   (multiple-value-bind (body status headers)
       (http-request "http://localhost:4242/")
     (is status 200)
     (is (cdr (assoc :content-type headers)) "text/plain")
     (is body "Happy Valentine!"))))

(finalize)
