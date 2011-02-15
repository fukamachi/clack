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

(clack.util:namespace clack.middleware.static
  (:use :cl
        :anaphora
        :clack.component
        :clack.middleware)
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
    (if path
        (etypecase path
          (string
           (if (starts-with-subseq path path-info)
               (progn
                 ;; rewrite :PATH-INFO
                 (setf (getf req :path-info)
                       (subseq path-info (1- (length path))))
                 (clack.component:call
                  (make-instance '<clack-app-file>
                     :root (static-root this))
                  req))
               (call-next this req)))
          (function
           (aif (funcall path path-info)
                (progn
                  ;; rewrite :PATH-INFO
                  (setf (getf req :path-info) it)
                  (clack.component:call
                   (make-instance '<clack-app-file>
                      :root (static-root this))
                   req))
                (call-next this req))))
        (call-next this req))))

#|
=markdown

# NAME

clack.middleware.static - Middleware for serving static files.

# SYNOPSIS

    (run
      (builder
       (<clack-middleware-static>
        :path "/public/"
        :root #p"/static-files/")
       app))

# DESCRIPTION

This is a Clack Middleware component for serving static files.

## Slots

* path (Required, String or Function)

<code>path</code> specifies the prefix of URL or a callback to match with requests to serve static files for.

Notice. Don't forget to add slush "/" to the end.

* root (Optional, Pathname)

<code>root</code> specifies the root directory to serve static files from.

## Example

Following example code would serve */public/foo.jpg* from */static-files/foo.jpg*.

    (run
      (builder
       (<clack-middleware-static>
        :path "/public/"
        :root #p"/static-files/")
       app))

You can set any function that returns mapped filename, for <code>:path</code>. Above example is able to be rewritten as following code.

    (run
      (builder
       (<clack-middleware-static>
        :path (lambda (path) (and (ppcre:scan path "^/public/")
                              (subseq path 7)))
        :root #p"/static-files/")
       app))

# AUTHOR

* Eitarow Fukamachi

# COPYRIGHT AND LICENSE

Copyright 2011 (c) Eitarow Fukamachi  
Licensed under the LLGPL License.

|#
