#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Rucksack - Middleware for Rucksack connection management.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack-middleware-rucksack-asd
  (:use :cl :asdf))
(in-package :clack-middleware-rucksack-asd)

(defsystem clack-middleware-rucksack
  :version "11.09"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-annot
               :rucksack)
  :components ((:file "src/contrib/middleware/rucksack"))
  :description "Middleware for Rucksack connection management")
