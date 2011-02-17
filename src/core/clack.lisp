#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack
  (:use :cl)
  (:import-from :clack.component
                :<component>
                :call
                :make-app)
  (:import-from :clack.middleware
                :<middleware>
                :call-next
                :wrap)
  (:export :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))

(cl-annot:enable-annot-syntax)

@export
(defun clackup (app &key (handler-name :hunchentoot) (port 8080) debug)
  "
Example:
  (clackup (lambda (req)
             (declare (ignore req))
             '(200 nil (\"ok\")))
           :port 8080
           :debug t)
"
  @ignore (app handler-name port debug)
  (error "TODO"))

(doc:start)

@doc:NAME "
Clack main package just for convenience.
"

@doc:SYNOPSIS "
"

@doc:DESCRIPTION "
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
* Clack.Middleware
"
