(in-package :cl-user)
(defpackage t.clack.middleware.static
  (:use :cl
        :asdf
        :prove
        :clack.test
        :clack.builder
        :clack.middleware.static
        :drakma))
(in-package :t.clack.middleware.static)

(plan 1)

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

#+thread-support
(subtest-app "middleware static"
    (builder
     (<clack-middleware-static>
      :path "/public/"
      :root (merge-pathnames #p"tmp/" *clack-pathname*))
     (lambda (env)
       (declare (ignore env))
       `(200 (:content-type "text/plain; charset=utf-8") ("Happy Valentine!"))))
  (multiple-value-bind (body status headers)
      (http-request (localhost "/public/jellyfish.jpg"))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "image/jpeg")
    (is (length body) 139616))
  (multiple-value-bind (body status)
      (http-request (localhost "/public/hoge.png"))
    (is status 404)
    (is body "not found"))
  (multiple-value-bind (body status headers)
      (http-request (localhost))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "text/plain; charset=utf-8")
    (is body "Happy Valentine!")))

#-thread-support
(skip 1 "because your lisp doesn't support threads")

(finalize)
