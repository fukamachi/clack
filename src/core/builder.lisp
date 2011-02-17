#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.builder
  (:use :cl
        :clack.component
        :clack.middleware))

(cl-annot:enable-annot-syntax)

(defun %builder (&rest app-or-middleware)
  "Wrap Clack application with middlewares and return it as one function."
  `(reduce #'wrap
           (list ,@(loop for arg in (butlast app-or-middleware)
                         if (consp arg)
                           collect `(make-instance ',(car arg) ,@(cdr arg))
                         else collect `(make-instance ',arg)))
           :initial-value ,(car (last app-or-middleware))
           :from-end t))

@export
(defmacro builder (&rest app-or-middleware)
  "Some Middleware and Applications reduce into one function."
  (apply #'%builder app-or-middleware))

@export
(defmacro builder-lazy (&rest app-or-middleware)
  "Some Middleware and Applications reduce into one function. This evals given Components in each HTTP request time."
  (let ((req (gensym "REQ")))
    `(lambda (,req) (call (eval ',(apply #'%builder app-or-middleware)) ,req))))

(doc:start)

@doc:NAME "
Clack.Builder - Clack utility to build up from some Middleware and Application into one function.
"

@doc:SYNOPSIS "
    (builder
     <clack.middleware.logger>
     (<clack.middleware.static>
      :path \"/public/\"
      :root #p\"/static-files/\")
     app)
"

@doc:DESCRIPTION "
Clack.Builder allows you to write middlewares inline. It builds up with calling `wrap' of middlewares sequencially and returns a function also as an Application.

The following example is:

    (builder
     <clack.middleware.logger>
     (<clack.middleware.static>
      :path \"/public/\"
      :root #p\"/static-files/\")
     app)

same as below one.

    (wrap (make-instance '<clack.middleware.logger>)
          (wrap (make-instance '<clack.middleware.static>
                   :path \"/public/\"
                   :root #p\"/static-files/\")
                app))

`builder-lazy' is almost same as `builder', but it builds up every time when the Application calls.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware
"
