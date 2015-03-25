#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Clsql - Middleware for CLSQL connection management.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-middleware-clsql-asd
  (:use :cl :asdf))
(in-package :clack-middleware-clsql-asd)

(defsystem clack-middleware-clsql
  :version "0.1.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :cl-syntax
               :cl-syntax-annot
               :clsql)
  :components ((:file "src/contrib/middleware/clsql"))
  :description "Middleware for CLSQL connection management")
