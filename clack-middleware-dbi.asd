#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Dbi - Middleware for CL-DBI connection management.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-middleware-dbi-asd
  (:use :cl :asdf))
(in-package :clack-middleware-dbi-asd)

(defsystem clack-middleware-dbi
  :version "0.2.0"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :dbi)
  :components ((:file "src/contrib/middleware/dbi"))
  :description "Middleware for CL-DBI connection management.")
