#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Csrf - Middleware for easy CSRF protection.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-middleware-csrf-asd
  (:use :cl :asdf))
(in-package :clack-middleware-csrf-asd)

(defsystem clack-middleware-csrf
  :version "11.09"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-annot
               :anaphora)
  :components ((:file "src/contrib/middleware/csrf"))
  :description "Middleware for easy CSRF protection"
  :in-order-to ((test-op (load-op clack-middleware-csrf-test))))
