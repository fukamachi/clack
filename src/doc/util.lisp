(in-package :cl-user)
(defpackage clack.doc.util
  (:use :cl))
(in-package :clack.doc.util)

(cl-annot:enable-annot-syntax)

@export
(defun declared-special-p (symbol)
  "Returns true if SYMBOL is declared special."
  #+lispworks (sys:declared-special-p symbol)
  #+sbcl (eql :special (sb-int:info :variable :kind symbol))
  #+allegro (eq (sys:variable-information symbol) :special)
  #+clozure (ccl:proclaimed-special-p symbol))

@export
(defun external-symbol-p (symbol &optional pkg)
  (let* (exported
         (exported (do-external-symbols (s (or pkg
                                               (symbol-package symbol))
                                           exported)
                     (push s exported))))
    (not (null (member symbol exported :test #'eq)))))

@export
;; FIXME: works only CCL
(defun class-direct-superclasses (class)
  #+ccl
  (slot-value class 'ccl::direct-superclasses)
  #-ccl
  (error "not implemented"))

@export
(defun lambda-list->specializers (lambda-list)
  (loop for arg in lambda-list
        with args = nil
        if (listp arg)
          do (if (and (listp (cadr arg)) (eq (caadr arg) 'eql))
                 ;; FIXME: works only CCL
                 (push (ccl:intern-eql-specializer (eval (cadadr arg))) args)
                 (push (find-class (cadr arg)) args))
        else if (find arg lambda-list-keywords)
               return (nreverse args)
        else do (push t args)
        finally (return (nreverse args))))

@export
(defun map-tree (f tree)
  (mapcar (lambda (e)
            (cond
              ((null e) nil)
              ((listp e) (map-tree f e))
              (t (funcall f e))))
          tree))
