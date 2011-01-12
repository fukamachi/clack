#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Slinky.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :cl-user)

(defpackage slinky-asd
  (:use :asdf))

(in-package :slinky-asd)

(defsystem slinky
  :version "1.0"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:hunchentoot :clsql :cl-markup :cl-locale)
  :components ((:module "src"
                :serial t
                :components
                ((:module "application"
                  :serial t
                  :components ((:file "package")
                               (:file "special")
                               (:file "class")
                               (:file "main")))
                 (:module "server"
                  :serial t
                  :components ((:file "package")
                               (:file "main")))
                 (:file "package")
                 (:file "core")))))
