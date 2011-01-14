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

(in-package :cl-user)

(defpackage clack.middleware.static
  (:use :cl :clack :cl-ppcre))
