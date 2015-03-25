#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.OAuth - Supports authorization mechanism by OAuth.

  Author: Norihisa Fujita (n.fujita@ariel-networks.com)
|#

(in-package :cl-user)
(defpackage :clack-middleware-oauth-asd
  (:use :cl :asdf))
(in-package :clack-middleware-oauth-asd)

(defsystem clack-middleware-oauth
  :version "0.2.0"
  :author "Norihisa Fujita"
  :license "LLGPL"
  :depends-on (:clack-v1-compat
               :cl-syntax
               :cl-syntax-annot
               :cl-oauth)
  :components ((:file "src/contrib/middleware/oauth"))
  :description "Supports authorization mechanism by OAuth")
