#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Handler.Fcgi - Clack handler for FastCGI.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-handler-fcgi-asd
  (:use :cl :asdf))
(in-package :clack-handler-fcgi-asd)

(defsystem clack-handler-fcgi
  :version "0.3.1"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :cl-fastcgi
               :alexandria
               :flexi-streams
               :cl-ppcre
               :split-sequence
               :usocket
               :quri)
  :components ((:file "src/core/handler/fcgi"))
  :description "Clack handler for FastCGI.")
