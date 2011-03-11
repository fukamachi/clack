#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.localtime
  (:use :cl)
  (:import-from :local-time
                :format-timestring
                :unix-to-timestamp
                :+rfc-1123-format+
                :+gmt-zone+))
(in-package :clack.util.localtime)

(cl-annot:enable-annot-syntax)

@export
(defun now ()
  "Returns a timestamp representing the present moment."
  (multiple-value-bind (sec nsec)
      (values (- (get-universal-time)
                 #.(encode-universal-time 0 0 0 1 1 1970 0))
              0)
    (assert (and sec nsec) () "Failed to get the current time from the operating system. How did this happen?")
    (unix-to-timestamp sec :nsec nsec)))

@export
(defun format-rfc1123-timestring (destination timestamp)
  (format-timestring destination timestamp
                     :format +rfc-1123-format+
                     :timezone +gmt-zone+))

(doc:start)

@doc:NAME "
Clack.Util.Localtime - Ported functions from LOCAL-TIME.
"

@doc:DESCRIPTION "
LOCAL-TIME causes some problems sometimes. So I rewrote some functions for Clack.

This package provides following two functions.

* now
* format-rfc1123-timestring
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
