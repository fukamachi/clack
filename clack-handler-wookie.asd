#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Handler.Wookie - Clack handler for Wookie.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-handler-wookie-asd
  (:use :cl :asdf))
(in-package :clack-handler-wookie-asd)

(defsystem clack-handler-wookie
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :wookie
               :cl-async
               :http-parse
               :quri
               :flexi-streams
               :babel
               :fast-io
               :split-sequence
               :alexandria)
  :components ((:file "src/core/handler/wookie"))
  :description "Clack handler for Wookie.")
