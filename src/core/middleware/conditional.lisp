#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.conditional
  (:use :cl
        :clack))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-conditional> (<middleware>)
     ((condition :type (or function <component>)
                 :initarg :condition
                 :accessor condition)
      (builder :type (or function <component> symbol list)
               :initarg :builder
               :accessor builder)
      (middleware :type (or function <component>)
                  :accessor middleware)))

(defmethod initialize-instance :after ((this <clack-middleware-conditional>) &key)
  (setf (builder this)
        (typecase (builder this)
          (symbol (make-instance (builder this)))
          (list (apply #'make-instance (builder this)))
          (t (builder this)))))

(defmethod wrap ((this <clack-middleware-conditional>) app)
  (setf (middleware this)
        (wrap (builder this) app))
  (call-next-method))

(defmethod call ((this <clack-middleware-conditional>) env)
  (if (call (condition this) env)
      (call (middleware this) env)
      (call-next this env)))

(doc:start)

@doc:NAME "
Clack.Middleware.Conditional - Conditional wrapper for Clack middleware.
"

@doc:SYNOPSIS "
    (builder
     (:condition (lambda (env)
                   (scan \"WebKit\" (getf env :http-user-agent)))
      :builder (builder
                <clack-middleware-something>
                app)))

    (builder
      (<clack-middleware-conditional>
       :condition (lambda (env)
                    (scan \"WebKit\" (getf env :http-user-agent)))
       :builder '(<clack-middleware-static>
                  :path \"/public/\"
                  :root #p\"/static-files/\")
      app))

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
* Eitarow Fukamachi (e.arrows@gmail.com)
"
