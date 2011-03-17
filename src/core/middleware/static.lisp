#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.static
  (:use :cl
        :clack
        :anaphora)
  (:import-from :clack.app.file
                :<clack-app-file>)
  (:import-from :alexandria
                :starts-with-subseq))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-static> (<middleware>)
     ((path :initarg :path :accessor static-path)
      (root :initarg :root :initform #p"./" :accessor static-root))
  (:documentation "Clack Middleware to intercept requests for static files."))

(defmethod call ((this <clack-middleware-static>) req)
  (let* ((path-info (getf req :path-info))
         (path (static-path this)))
    (if (null path)
        (call-next this req)
        (etypecase path
          (string
           (if (starts-with-subseq path path-info)
               ;; Serve static file with Clack.App.File
               (progn
                 (setf (getf req :path-info) ; rewrite :PATH-INFO
                       (subseq path-info (1- (length path))))
                 (call-app-file this req))
               (call-next this req)))
          (function
           (aif (funcall path path-info)
                (progn
                  (setf (getf req :path-info) it) ; rewrite :PATH-INFO
                  (call-app-file this req))
                (call-next this req)))))))

(defmethod call-app-file ((this <clack-middleware-static>) req)
  "Call Clack.App.File."
  (clack.component:call
   (make-instance '<clack-app-file>
      :root (static-root this))
   req))

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
Following example code would serve */public/foo.jpg* from */static-files/foo.jpg*.

    (run
      (builder
       (<clack-middleware-static>
        :path \"/public/\"
        :root #p\"/static-files/\")
       app))

You can set any function that returns mapped filename, for <code>:path</code>. Above example is able to be rewritten as following code.

    (run
      (builder
       (<clack-middleware-static>
        :path (lambda (path)
                (when (ppcre:scan path \"^/public/\")
                  (subseq path 7)))
        :root #p\"/static-files/\")
       app))
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.App.File
"
