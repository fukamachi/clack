#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack main package just for convenience.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
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
