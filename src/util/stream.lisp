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

;;; Replay-Input-Stream

@export
(defclass replay-input-stream (trivial-gray-stream-mixin
                               fundamental-binary-input-stream)
     ((stream :initarg :stream
              :reader replay-stream-stream)
      (buffer :initarg :buffer
              :initform (make-replay-buffer)
              :reader replay-stream-buffer)
      (position :initform 0
                :accessor replay-stream-position)))

@export
(defmethod replay-stream-fetch ((this replay-input-stream))
  (with-slots (stream buffer position) this
     (let ((to-be-fetched (1+ (- position (length buffer)))))
       ;; TODO use read-sequence
       (loop for fetched from 0 below to-be-fetched
             for octet = (read-byte stream nil nil)
             if octet
               do (vector-push-extend octet buffer)
             else
               return fetched
             finally (return fetched)))))

(defmethod stream-element-type ((this replay-input-stream))
  'octet)

(defmethod stream-read-byte ((this replay-input-stream))
  (with-slots (stream buffer position) this
     (if (or (< position (length buffer))
             (plusp (replay-stream-fetch this)))
         (prog1
           (aref buffer position)
           (incf position))
         :eof)))

(defmethod stream-read-char ((this replay-input-stream))
  (code-char (stream-read-byte this)))

(defmethod stream-listen ((this replay-input-stream))
  (with-slots (buffer position) this
     (< position (length buffer))))

(defmethod stream-read-sequence ((this replay-input-stream) sequence start end &key)
  (loop for index from start below end
        for octet = (stream-read-byte this)
        while (typep octet 'octet)
        do (setf (elt sequence index) octet)
        finally (return index)))

(defmethod stream-file-position ((this replay-input-stream))
  (replay-stream-position this))

(defmethod (setf stream-file-position) (position-spec (this replay-input-stream))
  (typecase position-spec
    ((eql :start) 0)
    (integer position-spec)))

@export
(defun make-replay-buffer ()
  (make-array 0
              :element-type 'octet
              :adjustable t
              :fill-pointer 0))

@export
(defun make-replay-input-stream (stream &key (buffer (make-replay-buffer)))
  (make-instance 'replay-input-stream
     :stream stream
     :buffer buffer))

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
