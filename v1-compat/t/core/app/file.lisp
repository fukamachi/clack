(in-package :cl-user)
(defpackage t.clack.app.file
  (:use :cl
        :asdf
        :prove
        :clack.test
        :clack.app.file
        :lack.component
        :drakma))
(in-package :t.clack.app.file)

(plan 2)

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

#+thread-support
(subtest-app "clack-app-file"
    (to-app
     (make-instance '<clack-app-file>
                    :root (merge-pathnames #p"tmp/" *clack-pathname*)))
  (multiple-value-bind (body status headers)
      (http-request (localhost "/jellyfish.jpg"))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "image/jpeg")
    (is (length body) 139616))
  (multiple-value-bind (body status headers)
      (http-request (localhost "/redhat.png"))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "image/png")
    (is (length body) 12155)))

#+thread-support
(subtest-app "static"
    (to-app
     (make-instance '<clack-app-file>
                    :file "jellyfish.jpg"
                    :root (merge-pathnames #p"tmp/" *clack-pathname*)))
  (multiple-value-bind (body status headers)
      (http-request (localhost "/jellyfish.jpg"))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "image/jpeg")
    (is (length body) 139616))
  (multiple-value-bind (body status headers)
      (http-request (localhost "/redhat.png"))
    (is status 200)
    (is (cdr (assoc :content-type headers)) "image/jpeg")
    (is (length body) 139616)))

#-thread-support
(skip 12 "because your lisp doesn't support threads")

(finalize)
