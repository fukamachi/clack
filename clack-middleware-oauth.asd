#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

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
  :version "11.09"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :cl-oauth
               :anaphora)
  :components ((:file "src/contrib/middleware/oauth"))
  :description "Supports authorization mechanism by OAuth")
