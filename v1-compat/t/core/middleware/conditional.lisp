(in-package :cl-user)
(defpackage t.clack.middleware.conditional
  (:use :cl
        :clack.component
        :clack.middleware
        :clack.builder
        :prove
        :clack.middleware.conditional
        :clack.middleware.static
        :cl-ppcre))
(in-package :t.clack.middleware.conditional)

(plan 5)

(defvar *app* (lambda (env)
                (declare (ignore env))
                '(200 nil ("Hello, Clack"))))

(is-type (builder
          (:condition (lambda (env)
                        (scan "WebKit" (getf env :http-user-agent)))
           :builder '(<clack-middleware-static>
                      :path "/public/"
                      :root #p"/static-files/"))
          *app*)
         'function)

(is-type (builder
          (<clack-middleware-conditional>
           :condition (lambda (env)
                        (scan "WebKit" (getf env :http-user-agent)))
           :builder '(<clack-middleware-static>
                      :path "/public/"
                      :root #p"/static-files/"))
          *app*)
         'function)

(is-type (wrap
          (make-instance '<clack-middleware-conditional>
             :condition (lambda (env)
                          (scan "WebKit" (getf env :http-user-agent)))
             :builder '(<clack-middleware-static>
                        :path "/public/"
                        :root #p"/static-files/"))
          *app*)
         'function)

(defclass <clack-middleware-conditional-test> (<middleware>) ())
(defmethod call ((this <clack-middleware-conditional-test>) env)
  (declare (ignore env))
  '(200 nil ("Hello from Conditional Middleware")))

(defvar *built-app*
    (builder
     (<clack-middleware-conditional>
      :condition (lambda (env)
                   (scan "WebKit" (getf env :http-user-agent)))
      :builder '<clack-middleware-conditional-test>)
     *app*))

(is (call *built-app* '(:http-user-agent "Firefox"))
    '(200 nil ("Hello, Clack")))

(is (call *built-app* '(:http-user-agent "WebKit"))
    '(200 nil ("Hello from Conditional Middleware")))

(finalize)
