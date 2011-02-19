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
(defun find-method-function (symb lambda-list)
  (if (not lambda-list)
      (symbol-function symb)
      (let ((lambda-list (loop for arg in lambda-list
                           with args = nil
                           if (listp arg)
                             do (push (cadr arg) args)
                           else if (find arg lambda-list-keywords)
                                  return (nreverse args)
                           else do (push t args)
                           finally (return (nreverse args)))))
        (find-if
         #'(lambda (meth)
             (equal (mapcar #'class-name (method-specializers meth))
                    lambda-list))
         (c2mop:generic-function-methods (symbol-function symb))))))

@export
(defun method-specializers (method)
  #+ccl
  (slot-value method 'ccl::specializers)
  #+sbcl
  (slot-value method 'sb-pcl::specializers)
  #+cmu
  (slot-value method 'pcl::specializers)
  #+allegro
  (slot-value method 'excl::specializers)
  #+ecl
  (slot-value method 'clos::specializers)
  #+lispworks
  (slot-value method 'clos::specializers)
  #+clisp
  (slot-value method 'clos::$specializers)
  )

@export
(defun class-direct-superclasses (class)
  #+ccl
  (slot-value class 'ccl::direct-superclasses)
  #-ccl
  (error "not implemented"))

(defun function-lambda-list* (func)
  "Returns the lambda list associated with the definition of the function or
macro function. For example, the lambda list for the common lisp function
`find' is the list:
 (ITEM SEQUENCE &KEY :FROM-END :TEST :TEST-NOT :START :END :KEY)"
  (if (and (functionp func)
           (typep func (find-class 'generic-function)))
      (values (c2mop:generic-function-lambda-list func) t)
      #+sbcl
      (let ((llist (sb-introspect:function-lambda-list func)))
        (if llist
            (values llist t)
            (values nil nil)))
      #+allegro
      (handler-case (values (excl:arglist func) t)
        (simple-error () (values nil nil)))
      #+lispworks
      (let ((arglist (lw:function-lambda-list func)))
        (etypecase arglist
          ((member :dont-know)
           (values nil nil))
          (list
           (values (replace-strings-with-symbols arglist)) t)))
      #+clozure
      (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                               (ccl:arglist func))
        (if binding
            (values arglist t)
            (values nil nil)))
      #+armedbear
      (cond ((symbolp func)
             (multiple-value-bind (arglist present)
                 (sys::arglist func)
               (when (and (not present)
                          (fboundp func)
                          (typep (symbol-function func)
                                 'standard-generic-function))
                 (setq arglist
                       (mop::generic-function-lambda-list
                        (symbol-function func))
                       present
                       t))
               (if present
                   (values arglist t)
                   (values nil nil))))
            (t (values nil nil)))
      #+ecl
      (when (or (functionp func) (fboundp func))
        (multiple-value-bind (name fndef)
            (if (functionp func)
                (values (function-name func) func)
                (values func (fdefinition func)))
          (typecase fndef
            (function
             (let ((fle (function-lambda-expression fndef)))
               (case (car fle)
                 (si:lambda-block
                  (values (caddr fle) t))
                 (t
                  (values nil nil))))))))
      #+cmu
      (let ((llist
             (etypecase func
               (function (cmucl-function-arglist fun))
               (symbol (cmucl-function-arglist (or (macro-function func)
                                                   (symbol-function func)))))))
        (if (eql llist :not-available)
            (values nil nil)
            (values llist t)))
      #+clisp
      (block nil
        (or (ignore-errors
              (return (values (ext:arglist func) t)))
            (ignore-errors
              (let ((exp (function-lambda-expression func)))
                (and exp (return (values (second exp) t)))))
            (values nil nil)))))
