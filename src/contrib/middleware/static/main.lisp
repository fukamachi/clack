#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack Middleware to serve static files.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.middleware.static)

(defclass <clack-static> (<middleware>)
     ((urls :initarg :urls :accessor urls)
      (root :initarg :root :accessor root))
  (:documentation "Clack Middleware to intercept requests for static files."))

(defmethod call ((self <clack-static>) req)
  (let ((path (getf req :path-info)))
    (if (some (lambda (url) (ppcre:scan url path)) (urls self))
        nil ;; serve static file
           ;; locate, detect MIMETYPE, and..
        (funcall (app self) req))))
