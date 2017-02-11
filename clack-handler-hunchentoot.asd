#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Handler.Hunchentoot - Clack handler for Hunchentoot.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-handler-hunchentoot-asd
  (:use :cl :asdf))
(in-package :clack-handler-hunchentoot-asd)

(defsystem clack-handler-hunchentoot
  :version "0.4.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:hunchentoot
               :clack-socket
               :flexi-streams
               :bordeaux-threads
               :split-sequence
               :alexandria)
  :components ((:file "src/handler/hunchentoot"))
  :description "Clack handler for Hunchentoot.")
