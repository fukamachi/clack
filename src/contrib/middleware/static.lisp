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

(in-package :cl-user)

(defpackage clack.middleware.static
  (:use :cl
        :alexandria
        :cl-ppcre
        :anaphora
        :clack.middleware
        :clack.app.file)
  (:export :<clack-middleware-static>))

(in-package :clack.middleware.static)

(defclass <clack-middleware-static> (<middleware>)
     ((path :initarg :path :accessor static-path
            :documentation "Required. Regex string.")
      (root :initarg :root :initform #p"./" :accessor static-root))
  (:documentation "Clack Middleware to intercept requests for static files."))

(defmethod call ((this <clack-middleware-static>) req)
  (let* ((path-info (getf req :path-info))
         (path (static-path this)))
    (if path
        (etypecase path
          (string
           (if (starts-with-subseq path path-info)
               (progn
                 ;; rewrite :PATH-INFO
                 (setf (getf req :path-info)
                       (subseq path-info (1- (length path))))
                 (call (make-instance '<clack-app-file>
                          :root (static-root this))
                       req))
               (call-next this req)))
          (function
           (aif (funcall path path-info)
                (progn
                  ;; rewrite :PATH-INFO
                  (setf (getf req :path-info) it)
                  (call (make-instance '<clack-app-file>
                           :root (static-root this))
                        req))
                (call-next this req))))
        (call-next this req))))
