#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Csrf - Middleware for easy CSRF protection.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-middleware-csrf-asd
  (:use :cl :asdf))
(in-package :clack-middleware-csrf-asd)

(defsystem clack-middleware-csrf
  :version "0.2.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :lack-util
               :cl-syntax
               :cl-syntax-annot
               :alexandria)
  :components ((:file "src/contrib/middleware/csrf"))
  :description "Middleware for easy CSRF protection"
  :in-order-to ((test-op (test-op t-clack-middleware-csrf))))
