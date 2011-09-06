#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.App.Route - URL dispatcher.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage :clack.app.route-asd
  (:use :cl :asdf))
(in-package :clack.app.route-asd)

(defsystem clack.app.route
  :version "11.09"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (:clack
               :cl-annot
               :cl-ppcre
               :alexandria)
  :components ((:file "src/contrib/app/route"))
  :description "URL dispatcher")
