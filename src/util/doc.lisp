#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.doc
  (:nicknames :doc)
  (:use :cl
        :split-sequence)
  (:import-from :closer-mop
                :generic-function-methods
                :generic-function-lambda-list))
(in-package :clack.util.doc)

(cl-annot:enable-annot-syntax)

@export
'#.(defvar *section-plist*
       '(:name "NAME"
         :synopsis "SYNOPSIS"
         :explanation "EXPLANATION"
         :dependencies "DEPENDENCIES"
         :description "DESCRIPTION"
         :example "EXAMPLE"
         :see "SEE ALSO"
         :author "AUTHOR"
         :contributors "CONTRIBUTORS"
         :copyright "COPYRIGHT"
         :license "LICENSE"))

@export
(defun start ()
  "Clear the docstring of `*package*'.
I recommend you to put `(doc:start)' before calling doc functions,
because they append sections duplicately when the packaged is reloaded."
  (setf (documentation *package* t) ""))

@export
(defun doc (header string &optional (level 1))
  "Set documentation to current package"
  (setf (documentation *package* t)
        (concatenate 'string
                     (documentation *package* t)
                     (section header string level))))

@export
(defun section (header string &optional (level 1))
  (format nil "~:[~;~:*~V@{~A~:*~}~* ~A~2&~]~A~2&"
          level "#" header (string-left-trim #(#\Newline) string)))

#.`(progn
     ,@(loop for (fn-name sec) on *section-plist* by #'cddr
             collect
             `@export
               (defun ,(intern (symbol-name fn-name)) (string)
                 (doc ,sec
                      (string-left-trim #(#\Newline) string)))))

;; Generator

(defvar *external-symbols-hash* nil)
(defvar *external-symbols-list* nil)
(defvar *asdf-system-packages* (make-hash-table :test #'equal))

(defun gendoc (name &key type (description "") (arg-list nil))
  (format nil "
- ~:(~A~): ~(~A~)~:[~;~:* [~{~(~A~)~^ ~}]~]~:[~;~:*

~{    ~A~^
~}~]
" type name arg-list (and description
                          (split-sequence #\Newline description))))

(defun type-of* (symb)
  (cond
    ((keywordp symb) (type-of* (intern (symbol-name symb))))
    ((typep (find-class symb nil) 'structure-class) 'struct)
    ((find-class symb nil) 'class)
    ((declared-special-p symb) (if (constantp symb) 'constant 'variable))
    ((fboundp symb) (cond
                      ((typep (symbol-function symb) 'generic-function)
                       'generic)
                      ((macro-function symb) 'macro)
                      (t 'function)))
    ((find-package symb) 'package)
    ((asdf:find-system symb nil) 'system)))

@export
(defmethod generate-documentation ((symb symbol))
  (case (type-of* symb)
    (class (generate-documentation (find-class symb)))
    ((constant variable) (generate-variable-documentation symb))
    ((function generic macro) (generate-function-documentation symb))
    (package (generate-documentation (find-package symb)))
    (system (generate-documentation (asdf:find-system symb)))))

@export
(defmethod generate-documentation ((str string))
  (generate-documentation (intern str)))

@export
(defmethod generate-documentation ((pkg package))
  (let* (symbol-list
         (symbol-list (if *external-symbols-hash*
                          (gethash (package-name pkg) *external-symbols-hash*)
                          (do-external-symbols (symb pkg symbol-list)
                            (push (cons symb nil) symbol-list)))))
    (concatenate 'string
                 (or (documentation pkg t)
                     (section "NAME" (string-capitalize (package-name pkg))))
                 (section "EXTERNAL SYMBOLS"
                          (external-symbols-documentation
                           (reverse symbol-list)
                           pkg)))))

@export
(defmethod generate-documentation ((system asdf:system))
  (let ((packages (asdf-system-packages system)))
    (apply
     #'concatenate
     'string
     (ignore-errors (slot-value system 'asdf::description))
     (mapcar #'generate-documentation (nreverse packages)))))

(defun external-symbols-documentation (symbol-list pkg)
  (let* (exported
         (exported (do-external-symbols (s pkg exported)
                     (push s exported))))
    (apply #'concatenate 'string
           (loop for symb in symbol-list
                 if (member (car symb) exported)
                   collect
                   (ecase (cdr symb)
                     (variable (generate-variable-documentation (car symb)))
                     (function (generate-function-documentation (car symb)))
                     (class (generate-documentation (find-class (car symb))))
                     ((nil) (generate-documentation (car symb))))))))

@export
(defmethod generate-documentation ((class standard-class))
  (gendoc (class-name class)
          :type "Class"
          :description (documentation class 'class)
          :arg-list (mapcar #'c2mop:slot-definition-name
                            (c2mop:compute-slots class))))

(defun generate-variable-documentation (var-symb)
  (gendoc var-symb
          :type (type-of* var-symb)
          :description (documentation var-symb 'variable)))

(defun generate-function-documentation (fn-symb)
  (if (typep (symbol-function fn-symb) 'generic-function)
      (generate-method-documentation fn-symb)
      (gendoc fn-symb
              :type (type-of* fn-symb)
              :arg-list (function-lambda-list fn-symb)
              :description (documentation fn-symb 'function))))

(defun generate-method-documentation (generic-symb)
  (let ((generic (symbol-function generic-symb)))
    (apply #'concatenate 'string
           (if (documentation generic 'function)
               (gendoc generic-symb
                       :type "Generic"
                       :arg-list (function-lambda-list (symbol-function generic-symb))
                       :description (documentation generic 'function))
               "")
           (mapcar #'(lambda (meth)
                       (gendoc generic-symb
                               :type "Method"
                               :arg-list (function-lambda-list meth)
                               :description (documentation meth t)))
                   (c2mop:generic-function-methods generic)))))

(defun asdf-component-files (comp)
  (etypecase comp
    (asdf::cl-source-file
     (list (slot-value comp 'asdf::absolute-pathname)))
    (asdf::component
     (loop for c in (slot-value comp 'asdf::components)
           append (asdf-component-files c)))))

(defun asdf-system-reload (system)
  (let (*error-output*)
    (unless (typep system 'asdf::component)
      (setf system (asdf:find-system system)))
    (asdf:oos 'asdf:load-op system :verbose nil)
    (setf (gethash (slot-value (asdf:find-system :clack) 'asdf::name) *asdf-system-packages*) nil)
    (setf *external-symbols-hash* (make-hash-table :test #'equal))
    (let ((macroexpand-hook *macroexpand-hook*))
      (setf *macroexpand-hook*
            (lambda (fun form env)
              (when (and (consp form)
                         (ignore-errors (string (second form))))
                (case (first form)
                  (cl:defpackage
                   (push (second form) (gethash (slot-value (asdf:find-system :clack) 'asdf::name) *asdf-system-packages*)))
                  ((cl:defun cl:defmacro)
                   (push (cons (second form) 'function)
                         (gethash (package-name *package*) *external-symbols-hash*)))
                  ((cl:defmethod cl:defgeneric)
                   (unless (member (cons (second form) 'function)
                                   (gethash (package-name *package*) *external-symbols-hash*)
                                   :test #'equal)
                     (push (cons (second form) 'function)
                           (gethash (package-name *package*) *external-symbols-hash*))))
                  ((cl:defclass cl:defstruct)
                   (push (cons (second form) 'class)
                         (gethash (package-name *package*) *external-symbols-hash*)))
                  ((cl:defconstant cl:defparameter cl:defvar)
                   (push (cons (second form) 'variable)
                         (gethash (package-name *package*) *external-symbols-hash*)))))
              (funcall macroexpand-hook fun form env)))
      (map nil #'load (asdf-component-files system))
      (setf *macroexpand-hook* macroexpand-hook)
      t)))

@export
(defun asdf-system-packages (system)
  (let ((packages (gethash (slot-value system 'asdf::name)
                           *asdf-system-packages*
                           ':unprepared)))
  (when (eq :unprepared packages)
    (asdf-system-reload system))
  packages))

;; Utility functions

(defun declared-special-p (symbol)
  "Returns true if SYMBOL is declared special."
  #+lispworks (sys:declared-special-p symbol)
  #+sbcl (eql :special (sb-int:info :variable :kind symbol))
  #+allegro (eq (sys:variable-information symbol) :special)
  #+clozure (ccl:proclaimed-special-p symbol))

(defun function-lambda-list (func)
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

(doc::start)

@doc::NAME "
Clack.Util.Doc - For writing Clack documentations.
"

@doc::SYNOPSIS "

    ;; Clear documentation of `*package*'.
    (doc:start)
    
    (doc:NAME \"
    Clack - Web Application Environment for Common Lisp
    \")
    
    (doc:DESCRIPTION \"
    Clack is a Web Application Environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.
    \")

    ;; I recommend to use with cl-annot (https://github.com/arielnetworks/cl-annot).
    ;; It allows you to write docs by annotation-style.
    (cl-annot:enable-annot-syntax)
    
    (doc:start)
    
    @doc:NAME \"
    Clack - Web Application Environment for Common Lisp
    \"
"

@doc::DESCRIPTION "
Clack.Util.Doc enables easy writing package documentations with Markdown.
"

@doc::AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc::SEE "
* [cl-annot](https://github.com/arielnetworks/cl-annot)
"
