#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Handler.Apache - Clack handler for Apache2 + mod_lisp.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-handler-apache-asd
  (:use :cl :asdf))
(in-package :clack-handler-apache-asd)

#+(or allegro cmu lispworks sbcl)
(defsystem clack-handler-apache
  :version "0.2.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :modlisp
               :split-sequence
               :alexandria)
  :components ((:file "src/core/handler/apache"))
  :description "Clack handler for Apache2 + mod_lisp.")
#-(or allegro cmu lispworks sbcl)
(error "Clack.Handler.Apache isn't supported your CL implementation.")
