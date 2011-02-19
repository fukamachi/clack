#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.class
  (:use :cl
        :split-sequence)
  (:import-from :clack.doc.util
                :find-method-function
                :class-direct-superclasses
                :external-symbol-p
                :<list-metaclass>))
(in-package :clack.doc.class)

(cl-annot:enable-annot-syntax)

(defvar *package-symbols-hash* (make-hash-table :test #'equal))

(defun gendoc (type summary &optional description)
  (format nil "
- ~:(~A~): ~A~:[~;~:*

~{    ~A~^
~}~]
" type summary (and description
                    (split-sequence #\Newline description))))

(defclass <doc-base> ()
     ((type :initarg :type :initform nil :accessor doc-type)
      (name :initarg :name :accessor doc-name)))

@export
(defmethod generate-documentation ((this <doc-base>)))

@export
(defclass <doc-package> (<doc-base>)
     ((system :initarg :system :accessor package-system))
  (:metaclass <list-metaclass>))

(defmethod initalize-instance :after ((this <doc-package>) &key)
  (setf (doc-type this) :package))

(defmethod find-entity ((this <doc-package>))
  (find-package (doc-name this)))

@export
(defmethod generate-documentation ((this <doc-package>))
  (format nil
          "~2&~A~&# EXTERNAL SYMBOLS~%~{~A~}"
         (or (documentation (find-entity this) t)
             (format nil "# NAME~2%~A" (string-capitalize (doc-name this))))
         (mapcar #'generate-documentation
                 (remove-if-not
                  #'externalp
                  (reverse (gethash (doc-name this) *package-symbols-hash*))))))

@export
(defclass <doc-symbol-base> (<doc-base>)
     ((docstring :initform nil :accessor docstring)
      (package :initarg :package :initform (package-name *package*) :accessor symbol-package*)))

(defmethod initialize-instance :after ((this <doc-symbol-base>) &key)
  (push this
        (gethash (symbol-package* this) *package-symbols-hash*)))

@export
(defmethod externalp ((this <doc-symbol-base>))
  (external-symbol-p (doc-name this)))

@export
(defclass <doc-function> (<doc-symbol-base>)
     ((lambda-list :initarg :lambda-list :initform nil :accessor function-lambda-list)))

(defmethod initialize-instance :after ((this <doc-function>) &key)
  (unless (doc-type this)
    (setf (doc-type this) :function)))

(defmethod find-entity ((this <doc-function>))
  (symbol-function (doc-name this)))

@export
(defmethod generate-documentation ((this <doc-function>))
  (gendoc (doc-type this)
          (format nil "~(~A~)~:[~;~:* [~{~(~A~)~^ ~}]~]"
                  (doc-name this)
                  (function-lambda-list this))
          (documentation (find-entity this) 'function)))

@export
(defclass <doc-method> (<doc-function>)
     ((order :initarg :order :initform nil :accessor method-order)))

(defmethod initialize-instance :after ((this <doc-method>) &key)
  (setf (doc-type this) :method))

(defmethod find-entity ((this <doc-method>))
  (or (find-method-function (doc-name this)
                            (function-lambda-list this))
      (error "Method not found: ~A ~A"
             (doc-name this) (function-lambda-list this))))

@export
(defmethod generate-documentation ((this <doc-method>))
  (gendoc (doc-type this)
          (format nil "~(~A~)~:[~;~:* [~{~(~A~)~^ ~}]~]"
                  (doc-name this)
                  (function-lambda-list this))
          (documentation (find-entity this) t)))

@export
(defclass <doc-class> (<doc-symbol-base>)
     ((slots :initarg :slots :initform nil :accessor class-slots)
      (super-classes :initarg :superclasses :initform nil :accessor class-super-classes)))

(defmethod initialize-instance :after ((this <doc-class>) &key)
  (setf (doc-type this) :class))

(defmethod prepare ((this <doc-class>))
  (let ((class (find-entity this)))
    (setf (class-super-classes this)
          (loop for super in (class-direct-superclasses class)
                unless (eq (type-of super) 'built-in-class)
                  collect (class-name super)))
    (setf (class-slots this)
          (mapcar #'c2mop:slot-definition-name
                  (c2mop:compute-slots class)))))

(defmethod find-entity ((this <doc-class>))
  (find-class (doc-name this)))

@export
(defmethod generate-documentation ((this <doc-class>))
  (prepare this)
  (gendoc (doc-type this)
          (format nil "~(~A~)~:[~;~:* inherits ~(~A~)~]~:[~;~:* [~{~(~A~)~^ ~}]~]"
                  (doc-name this)
                  (class-super-classes this)
                  (class-slots this))
          (documentation (find-entity this) 'class)))

@export
(defclass <doc-variable> (<doc-symbol-base>) ())

(defmethod initialize-instance :after ((this <doc-variable>) &key)
  (setf (doc-type this) :variable))

@export
(defmethod generate-documentation ((this <doc-variable>))
  (gendoc (doc-type this)
          (string-downcase (doc-name this))
          (documentation this 'variable)))
