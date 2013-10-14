#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2013 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.let
  (:use :cl
        :clack.component
        :clack.middleware))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-let> (<middleware>)
  ((bindings :initarg :bindings)))

(defmethod call ((this <clack-middleware-let>) env)
  (eval
   `(let ,(slot-value this 'bindings)
      (call-next ,this ',env))))

(doc:start)

@doc:NAME "
Clack.Middleware.Let
"

@doc:SYNOPSIS "
    (clackup (builder
              (<clack-middleware-let> :bindings '((*standard-output* *clack-output*)))
              (lambda (env)
                (format t \"You've got an access!~%\")
                '(200 nil (\"Hi!\")))))
"

@doc:DESCRIPTION "
Clack.Middleware.Let allows you to bind variables within each requests like `cl:let'.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
