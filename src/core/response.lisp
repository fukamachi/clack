#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Wrapper for Clack response.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack)

;; TODO: cookies, redirect
(defclass <response> ()
     ((status :initarg :status :accessor status)
      (headers :initarg :headers :accessor headers)
      (body :initarg :body :accessor body))
  (:documentation "Wrapper class for Clack response."))

(defun merge-plist (p1 p2)
  "Convert given two plists into one plist."
  (loop :with notfound = '#:notfound
        :for (indicator value) on p1 by #'cddr
        :when (eq (getf p2 indicator notfound) notfound)
        :do (push value p2)
            (push indicator p2)))

(defmethod header ((res <response>) key)
  "Get the header value of given key."
  (cadr (assoc key (headers res))))

(defmacro define-header-method (name)
  (with-gensyms (res val val-supplied-p)
    (let ((name-key (intern (symbol-name name) :keyword)))
      `(defmethod (setf ,name) ((,res <response>) &optional (,val nil ,val-supplied-p))
         (if ,val-supplied-p
             (setf (headers ,res)
                   (merge-plist (headers ,res) `((,,name-key ,,val))))
             (cadr (header ,res ,name-key)))))))

(define-header-method content-type)
(define-header-method content-length)
(define-header-method content-encoding)
