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

(cl-annot:enable-annot-syntax)

(defvar *asdf-registered-system* nil)

@export
(defun asdf-component-files (comp)
  (etypecase comp
    (asdf::cl-source-file
     (list (slot-value comp 'asdf::absolute-pathname)))
    (asdf::static-file nil)
    (asdf::component
     (loop for c in (slot-value comp 'asdf::components)
           append (asdf-component-files c)))))

(defun asdf-system-reload (system)
  (let (*error-output*)
    #+quicklisp (ql:quickload (slot-value system 'asdf::name) :verbose nil)
    #-quicklisp (asdf:oos 'asdf:load-op system :verbose nil)
    (let ((macroexpand-hook *macroexpand-hook*))
      (setf *macroexpand-hook*
            (lambda (fun form env)
              (when (and (consp form)
                         (ignore-errors (string (second form))))
                (case (first form)
                  (cl:defpackage
                   (register-package-system
                    (princ-to-string (second form))
                    (slot-value system 'asdf::name)))
                  ((cl:defun cl:defmacro)
                   (make-instance '<doc-function>
                      :name (second form)
                      :type (if (eq (first form) 'cl:defun)
                                :function
                                :macro)
                      :lambda-list (third form)))
                  (cl:defgeneric
                   (make-instance '<doc-function>
                      :name (second form)
                      :type :generic
                      :lambda-list (third form)))
                  (cl:defmethod
                   (make-instance '<doc-method>
                      :name (second form)
                      :qualifier (unless (listp (third form)) (third form))
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
(defun ensure-system-loaded (system &key force)
  (if (and (not force)
           (find system *asdf-registered-system* :test #'equal))
      (values t nil)
      (progn
        (format t "Loading system ~A..." system)
        (asdf-system-reload system)
        (push system *asdf-registered-system*)
        (values t t))))
