#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Clsql - Middleware for CLSQL connection management.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-middleware-clsql-asd
  (:use :cl :asdf))
(in-package :clack-middleware-clsql-asd)

(defsystem clack-middleware-clsql
  :version "11.09"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :clsql)
  :components ((:file "src/contrib/middleware/clsql"))
  :description "Middleware for CLSQL connection management")
