(in-package :cl-user)
(defpackage clack.middleware.static
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :clack.app.file
                :<clack-app-file>)
  (:import-from :alexandria
                :starts-with-subseq
                :if-let))
(in-package :clack.middleware.static)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-static> (<middleware>)
     ((path :type (or string function null)
            :initarg :path
            :accessor static-path)
      (root :type pathname
            :initarg :root
            :initform #p"./"
            :accessor static-root))
  (:documentation "Clack Middleware to intercept requests for static files."))

(defmethod call ((this <clack-middleware-static>) env)
  (let* ((path-info (getf env :path-info))
         (path (static-path this)))
    (if (null path)
        (call-next this env)
        (etypecase path
          (string
           (if (starts-with-subseq path path-info)
               ;; Serve static file with Clack.App.File
               (progn
                 (setf (getf env :path-info) ; rewrite :PATH-INFO
                       (subseq path-info (1- (length path))))
                 (call-app-file this env))
               (call-next this env)))
          (function
           (if-let (new-path (funcall path path-info))
             (progn
               (setf (getf env :path-info) new-path) ; rewrite :PATH-INFO
               (call-app-file this env))
             (call-next this env)))))))

(defun call-app-file (mw env)
  "Call Clack.App.File."
  (check-type mw <clack-middleware-static>)
  (clack.component:call
   (make-instance '<clack-app-file>
      :root (static-root mw))
   env))
