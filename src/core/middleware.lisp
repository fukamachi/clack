#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Class for Clack Middleware.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.middleware
  (:use :cl :clack.component)
  (:export :<middleware>
           :call-next
           :wrap))

(defclass <middleware> (<component>)
     ((app :initarg :app :reader app))
  (:documentation "Class for Clack Middleware."))

(defmethod call-next ((this <middleware>) req)
  "Call next middleware or application."
  (call (app this) req))

(defmethod wrap ((this <middleware>) app-or-middleware)
  "Compose this and given application or middleware instance into one function.
The function takes request plist."
  (setf (slot-value this 'app) app-or-middleware)
  #'(lambda (req) (call this req)))
