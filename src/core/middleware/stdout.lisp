#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.stdout
  (:use :cl
        :clack.component
        :clack.middleware))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-stdout> (<middleware>)
     ((standard-output :type (or symbol stream)
                       :initarg :standard-output
                       :initform *standard-output*)))

(defmethod standard-output ((this <clack-middleware-stdout>))
  (if (symbolp (slot-value this 'standard-output))
      (symbol-value (slot-value this 'standard-output))
      (slot-value this 'standard-output)))

(defmethod call ((this <clack-middleware-stdout>) env)
  (let ((*standard-output* (standard-output this)))
    (call-next this env)))

(doc:start)

@doc:NAME "
Clack.Middleware.Stdout - Clack Middleware to set *standard-output*.
"

@doc:SYNOPSIS "
    (clackup (builder
              <clack-middleware-stdout>
              (lambda (env)
                (format nil \"You've got an access!~%\")
                '(200 nil (\"Hi!\")))))
"

@doc:DESCRIPTION "
Clack.Middleware.Stdout allows you to set `*standard-output*` in an each requests.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
