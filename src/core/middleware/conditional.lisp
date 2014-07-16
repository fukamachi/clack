#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.conditional
  (:use :cl
        :clack)
  (:import-from :clack.component
                :component-designator))
(in-package :clack.middleware.conditional)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-conditional> (<middleware>)
     ((condition :type component-designator
                 :initarg :condition)
      (builder :type (or component-designator symbol list)
               :initarg :builder)
      (middleware :type (or function <component>))))

(defmethod initialize-instance :after ((this <clack-middleware-conditional>) &key)
  (with-slots (builder) this
     (setf builder
           (typecase builder
             (symbol (make-instance builder))
             (list (apply #'make-instance builder))
             (t builder)))))

(defmethod wrap ((this <clack-middleware-conditional>) app)
  (with-slots (middleware builder) this
     (setf middleware
           (wrap builder app))
     (call-next-method)))

(defmethod call ((this <clack-middleware-conditional>) env)
  (with-slots (condition middleware) this
     (if (call condition env)
         (call middleware env)
         (call-next this env))))

(doc:start)

@doc:NAME "
Clack.Middleware.Conditional - Conditional wrapper for Clack middleware.
"

@doc:SYNOPSIS "
    (builder
     (:condition (lambda (env)
                   (scan \"WebKit\" (getf env :http-user-agent)))
      :builder '<clack-middleware-something>)
     app)

    (builder
      (<clack-middleware-conditional>
       :condition (lambda (env)
                    (scan \"WebKit\" (getf env :http-user-agent)))
       :builder '(<clack-middleware-static>
                  :path \"/public/\"
                  :root #p\"/static-files/\"))
      app)

    (wrap
     (make-instance '<clack-middleware-conditional>
        :condition (lambda (env)
                     (scan \"WebKit\" (getf env :http-user-agent)))
        :builder '(<clack-middleware-something>
                   :path \"/public/\"
                   :root #p\"/static-files/\"))
     app)
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"
