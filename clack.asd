#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack is a Web server Interface for Common Lisp.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack-asd
  (:use :asdf))

(in-package :clack-asd)

(defsystem clack
  :version "1.0"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:hunchentoot)
  :components ((:module "src"
                :serial t
                :components
                ((:module "core"
                  :serial t
                  :components ((:file "package")
                               (:file "environment")))
                 (:module "handler"
                  :serial t
                  :components
                  ((:module "hunchentoot"
                    :serial t
                    :components ((:file "package")
                                 (:file "main")))))
                 (:file "package")
                 (:file "middleware")
                 (:file "main")))))
