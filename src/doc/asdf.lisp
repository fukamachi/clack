#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc.asdf
  (:use :cl
        :clack.doc.class))
(in-package :clack.doc.asdf)

(defvar *asdf-system-packages* (make-hash-table :test #'equal))

@export
(defun asdf-component-files (comp)
  (etypecase comp
    (asdf::cl-source-file
     (list (slot-value comp 'asdf::absolute-pathname)))
    (asdf::component
     (loop for c in (slot-value comp 'asdf::components)
           append (asdf-component-files c)))))

@export
(defun asdf-system-reload (system)
  (let (*error-output*)
    (unless (typep system 'asdf::component)
      (setf system (asdf:find-system system)))
    (asdf:oos 'asdf:load-op system :verbose nil)
    (setf (gethash (slot-value system 'asdf::name) *asdf-system-packages*) nil)
    (let ((macroexpand-hook *macroexpand-hook*))
      (setf *macroexpand-hook*
            (lambda (fun form env)
              (when (and (consp form)
                         (ignore-errors (string (second form))))
                (case (first form)
                  (cl:defpackage
                   (make-instance '<doc-package>
                      :name (format nil "~A" (second form))
                      :system (slot-value system 'asdf::name))
                   (push (format nil "~A" (second form))
                    (gethash (slot-value system 'asdf::name) *asdf-system-packages*)))
                  ((cl:defun cl:defmacro)
                   (make-instance '<doc-function>
                      :name (second form)
                      :lambda-list (third form)))
                  (cl:defgeneric
                   (make-instance '<doc-function>
                      :name (second form)
                      :type :generic
                      :lambda-list (third form)))
                  ((cl:defmethod)
                   (make-instance '<doc-method>
                      :name (second form)
                      :order (unless (listp (third form)) (third form))
                      :lambda-list (if (listp (third form))
                                       (third form)
                                       (fourth form))))
                  (cl:defclass
                   (make-instance '<doc-class>
                      :name (second form)
                      :type :class))
                  (cl:defstruct
                   (make-instance '<doc-class>
                      :name (second form)
                      :type :struct))
                  (cl:defconstant
                   (make-instance '<doc-variable>
                      :name (second form)
                      :type :constant))
                  ((cl:defparameter cl:defvar)
                   (make-instance '<doc-variable>
                      :name (second form)
                      :type :variable))))
              (funcall macroexpand-hook fun form env)))
      (map nil #'load (asdf-component-files system))
      (setf *macroexpand-hook* macroexpand-hook)
      t)))

@export
(defun asdf-system-packages (system)
  (let ((packages (gethash (slot-value system 'asdf::name)
                           *asdf-system-packages*
                           :unprepared)))
    (when (eq :unprepared packages)
      (asdf-system-reload system)))
  (gethash (slot-value system 'asdf::name)
           *asdf-system-packages*))
