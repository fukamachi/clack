#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack-doc-asd
  (:use :cl :asdf))
(in-package :clack-doc-asd)

(defsystem clack-doc
  :version "0.1-SNAPSHOT"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:closer-mop
               :split-sequence
               :cl-ppcre
               :cl-annot
               :cl-markdown)
  :components ((:module "src"
                :pathname "src/doc"
                :components
                ((:file "doc" :depends-on ("class" "asdf"))
                 (:file "class" :depends-on ("util" "markdown"))
                 (:file "util")
                 (:file "asdf" :depends-on ("class"))
                 (:file "markdown")))))
