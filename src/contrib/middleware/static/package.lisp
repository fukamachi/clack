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
  (:use :cl :cl-ppcre
        :clack :clack.util
        :clack.app.file)
  (:export :<clack-middleware-static>))
