#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Testing Clack.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t-clack-asd
  (:use :cl :asdf))
(in-package :t-clack-asd)

(defclass asdf::test-file (asdf:cl-source-file) ())
(defclass run-test-op (asdf:test-op) ())

(defmethod asdf:operation-done-p ((op run-test-op) c)
  (declare (ignorable op c))
  nil)
(defmethod asdf::mark-operation-done ((op run-test-op) c)
  (declare (ignorable op c))
  nil)

#+asdf3
(defmethod component-depends-on ((o run-test-op) (c module))
  `((,o ,@(component-children c)) ,@(call-next-method)))

(defmethod asdf:perform ((op asdf:load-op) (c asdf::test-file))
  ;; do nothing
  )
(defmethod asdf:perform ((op asdf:compile-op) (c asdf::test-file))
  ;; do nothing
  )
(defmethod asdf:perform ((op asdf:test-op) (c asdf::test-file))
  (let ((class (class-of op)))
    (change-class c 'asdf:cl-source-file)
    (asdf:perform (make-instance 'asdf:compile-op) c)
    (asdf:perform (make-instance 'asdf:load-op) c)
    (change-class c class)))

(defsystem t-clack
  :in-order-to ((test-op (run-test-op t-clack)))
  :depends-on (:clack
               :clack-test
               :cl-test-more
               :bordeaux-threads
               :drakma)
  :components
  ((:module "t"
    :components
    ((:module "core"
      :components
      ((:test-file "component")
       (:test-file "middleware")
       (:test-file "builder")
       (:test-file "response")
       (:test-file "request")
       (:test-file "handler/hunchentoot")
       (:test-file "app/file")
       (:test-file "app/urlmap")
       (:test-file "middleware/static")
       (:test-file "middleware/conditional")
       (:test-file "middleware/session")
       (:test-file "middleware/logger")
       (:test-file "middleware/stdout")))
     (:module "util"
      :components
      ((:test-file "route")))))))
