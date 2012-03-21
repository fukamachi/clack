#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

;;;; -*- mode: LISP; package: RFC2388 -*-
;;;; Copyright (c) 2003 Janis Dzerins
;;;; Modifications for TBNL Copyright (c) 2004 Michael Weber and Dr. Edmund Weitz
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions
;;;; are met:
;;;; 1. Redistributions of source code must retain the above copyright
;;;;    notice, this list of conditions and the following disclaimer.
;;;; 2. Redistributions in binary form must reproduce the above copyright
;;;;    notice, this list of conditions and the following disclaimer in the
;;;;    documentation and/or other materials provided with the distribution.
;;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;;;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;;;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;;;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;;;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;;;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage clack.util.rfc2388
  (:use :cl
        :rfc2388)
  (:shadow :parse-mime)
  (:import-from :rfc2388
                :read-until-next-boundary
                :parse-content-type)
  (:import-from :clack.util
                :make-tmp-file-name))
(in-package :clack.util.rfc2388)

(cl-syntax:use-syntax :annot)

@export
(defgeneric parse-mime (source boundary &key write-content-to-file)
  (:documentation
   "Parses MIME entities, returning them as a list.  Each element in the
    list is of form: (body headers), where BODY is the contents of MIME
    part, and HEADERS are all headers for that part.  BOUNDARY is a string
    used to separate MIME entities."))

@export
(defmethod parse-mime ((input string) separator &key (write-content-to-file t))
  (with-input-from-string (stream input)
    (parse-mime stream separator)))

@export
(defmethod parse-mime ((input stream) boundary &key (write-content-to-file t))
  ;; Find the first boundary.  Return immediately if it is also the last
  ;; one.
  (unless (nth-value 1 (read-until-next-boundary input boundary t))
    (return-from parse-mime nil))

  (let ((result ()))
    (loop
      (let ((headers (loop
                      for header = (parse-header input)
                      while header
                      when (string-equal "CONTENT-TYPE" (header-name header))
                      do (setf (header-value header) (parse-content-type (header-value header)))
                      collect header)))
        (let ((file-name (get-file-name headers)))
          (cond ((and write-content-to-file
                      file-name)
                 (let ((temp-file (make-tmp-file-name)))
                   (multiple-value-bind (text more)
                       (with-open-file (out-file (ensure-directories-exist temp-file)
                                                 :direction :output
                                                 ;; external format for faithful I/O
                                                 ;; see <http://cl-cookbook.sourceforge.net/io.html#faith>
                                                 #+(or :sbcl :lispworks :allegro :openmcl)
                                                 :external-format
                                                 #+sbcl :latin-1
                                                 #+:lispworks '(:latin-1 :eol-style :lf)
                                                 #+:allegro (excl:crlf-base-ef :latin1)
                                                 #+:openmcl '(:character-encoding :iso-8859-1
                                                              :line-termination :unix))
                         (read-until-next-boundary input boundary nil out-file))
                     (declare (ignore text))
                     (when (and (stringp file-name)
                                (plusp (length file-name)))
                       (push (make-mime-part temp-file headers) result))
                     (when (not more)
                       (return)))))
                (t
                 (multiple-value-bind (text more)
                     (read-until-next-boundary input boundary)
                   (push (make-mime-part text headers) result)
                   (when (not more)
                     (return))))))))
    (nreverse result)))

(doc:start)

@doc:NAME "
Clack.Util.Rfc2388 - Ported from RFC2388.
"

@doc:DESCRIPTION "
`rfc2388:parse-mime` requires Hunchentoot package to parse multipart/form-data. This package provides a method `parse-mime` only for Clack.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
