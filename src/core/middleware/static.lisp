#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.static
  (:use :cl
        :clack)
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

(doc:start)

@doc:NAME "
Clack.Middleware.Static - Middleware to serve static files.
"

@doc:SYNOPSIS "
    (run
      (builder
       (<clack-middleware-static>
        :path \"/public/\"
        :root #p\"/static-files/\")
       app))
"

@doc:DESCRIPTION "
This is a Clack Middleware component for serving static files.

## Slots

* path (Required, String or Function)

<code>path</code> specifies the prefix of URL or a callback to match with requests to serve static files for.

Notice. Don't forget to add slush \"/\" to the end.

* root (Optional, Pathname)

<code>root</code> specifies the root directory to serve static files from.
"

@doc:EXAMPLE "
The following example code would serve */public/foo.jpg* from */static-files/foo.jpg*.

    (run
      (builder
       (<clack-middleware-static>
        :path \"/public/\"
        :root #p\"/static-files/\")
       app))

You can set any function that returns a mapped filename as <code>:path</code>. The above example can be rewritten as following code.

    (run
      (builder
       (<clack-middleware-static>
        :path (lambda (path)
                (when (ppcre:scan \"^/public/\" path)
                  (subseq path 7)))
        :root #p\"/static-files/\")
       app))
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.App.File
"
