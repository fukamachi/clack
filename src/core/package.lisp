#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack package.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack
  (:documentation "Clack top-level package.")
  (:use :cl :alexandria)
  (:export :call :call-next
           :wrap :builder :app
           :make-app
           :clackup
           :<component>
           :<middleware>
           :<request>
           :<response>
           :*lazy-load*))
