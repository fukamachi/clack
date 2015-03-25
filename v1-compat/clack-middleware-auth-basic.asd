#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Auth.Basic - Basic Authentication Middleware.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-middleware-auth-basic-asd
  (:use :cl :asdf))
(in-package :clack-middleware-auth-basic-asd)

(defsystem clack-middleware-auth-basic
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :cl-syntax
               :cl-syntax-annot
               :cl-base64
               :cl-ppcre
               :arnesi)
  :components ((:file "src/contrib/middleware/auth/basic"))
  :description "Basic Authentication Middleware."
  :in-order-to ((test-op (test-op t-clack-middleware-auth-basic))))
