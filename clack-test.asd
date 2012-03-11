#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Test - Testing Clack Applications.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-test-asd
  (:use :cl :asdf))
(in-package :clack-test-asd)

(defsystem clack-test
  :version "12.03"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :cl-test-more
               :flexi-streams
               :drakma)
  :components ((:file "src/contrib/test")
               (:file "src/contrib/test/suite" :depends-on ("src/contrib/test")))
  :description "Testing Clack Applications.")
