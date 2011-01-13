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
  (:use :cl :hunchentoot :clack.middleware)
  (:export :run
           :call :build
           :<middleware>
           :<environment>
           :<response>
           :status
           :headers
           :header
           :body
           :content-type
           :content-length
           :content-encoding))
