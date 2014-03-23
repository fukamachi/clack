#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Tomohiro Matsuyama <tomo@cx4a.org>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.stream
  (:use :cl :trivial-gray-streams))
(in-package :clack.util.stream)

(cl-syntax:use-syntax :annot)

(deftype octet () '(unsigned-byte 8))

;;; Utilities

@export
(defun slurp-stream (stream)
  (flex:with-output-to-sequence (out)
    (alexandria:copy-stream stream out :finish-output t)))

@export
(defun slurp-stream-to-string (stream &key (external-format :latin1))
  (flex:octets-to-string (slurp-stream stream) :external-format external-format))

@export
(defun ensure-character-input-stream (stream)
  (if (typep stream 'trivial-gray-streams:fundamental-character-input-stream)
      stream
      (flex:make-flexi-stream stream)))

(doc:start)

@doc:NAME "
Clack.Util.Stream - Useul streams
"

@doc:DESCRIPTION "
"

@doc:AUTHOR "
* Tomohiro Matsuyama (tomo@cx4a.org)
"

@doc:CONTRIBUTORS "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
