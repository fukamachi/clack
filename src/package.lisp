#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack top-level package.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack
  (:documentation "Clack top-level package.")
  (:use :cl
        :clack.middleware
        :clack.server)
  (:export :run
           :call
           :<middleware>))
