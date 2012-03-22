#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler
  (:use :cl)
  (:import-from :clack.util
                :find-handler))

(cl-syntax:use-syntax :annot)

@export
(defclass <handler> ()
     ((server-name :type keyword
                   :initarg :server-name
                   :accessor server-name)
      (acceptor :initarg :acceptor
                :accessor acceptor)))

@export
(defmethod stop ((this <handler>))
  "Stop the Clack server. Currently only works with Hunchentoot."
  (let ((handler-package (find-handler (server-name this))))
    (funcall (intern (string '#:stop) handler-package) (acceptor this))))

(doc:start)

@doc:NAME "
Clack.Handler - Class for Handler
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
