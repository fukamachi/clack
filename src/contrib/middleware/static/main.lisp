#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Static.
  Middleware to serve static files.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.middleware.static)

(defclass <clack-middleware-static> (<middleware>)
     ((urls :initarg :urls :accessor urls)
      (root :initarg :root :accessor root))
  (:documentation "Clack Middleware to intercept requests for static files."))

(defmethod call ((self <clack-middleware-static>) req)
  (let* ((path-info (getf req :path-info))
         (path (member-if
                (lambda (url) (= 0 (ppcre:scan url path-info)))
                (urls self))))
    (if path
        (call (make-instance '<clack-app-file>
                 :root (root self))
              (merge-plist `(:path-info ,path) req))
        (call (app self) req))))
