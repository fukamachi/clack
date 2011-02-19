(in-package :cl-user)
(defpackage clack-test.doc
  (:use :cl
        :cl-test-more
        :clack.doc.class))
(in-package :clack-test.doc)

(defun hoge (a b &optional c)
  "Just for test."
  (+ (* a b) c))

(defvar doc-fn)
(setf doc-fn
      (make-instance '<doc-function>
         :name 'hoge
         :lambda-list '(a b &optional c)))

(is (generate-documentation doc-fn) "")
