(in-package :cl-user)
(defpackage clack.middleware.backtrace
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :trivial-backtrace
                :print-backtrace))
(in-package :clack.middleware.backtrace)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-backtrace> (<middleware>)
  ((output :type (or symbol stream pathname)
           :initarg :output
           :initform '*error-output*
           :accessor output)
   (result-on-error :type (or function t)
                    :initarg :result-on-error
                    :accessor result-on-error)))

(defmethod call ((this <clack-middleware-backtrace>) env)
  (if (slot-boundp this 'result-on-error)
      (handler-case (call-with-backtrace this env)
        (error (condition)
          (if (functionp (result-on-error this))
              (funcall (result-on-error this) condition)
              (result-on-error this))))
      (call-with-backtrace this env)))

(defun call-with-backtrace (mw env)
  (check-type mw <clack-middleware-backtrace>)
  (handler-bind ((error
                   #'(lambda (condition)
                       (etypecase (output mw)
                         (symbol (print-error condition env (symbol-value (output mw))))
                         (stream (print-error condition env (output mw)))
                         ((or pathname string)
                          (with-open-file (out (output mw)
                                               :direction :output
                                               :external-format :utf-8
                                               :if-exists :append
                                               :if-does-not-exist :create)
                            (print-error condition env out)))))))
    (call-next mw env)))

(defun print-error (error env &optional (stream *error-output*))
  (format stream "~3&")
  ;; XXX: Because of a bug of trivial-backtrace.
  ;;   `print-backtrace' treats a string-output-stream as the same as NIL.
  (princ (print-backtrace error :output nil)
         stream)
  (format stream "~2&Request:~%")
  (loop for (k v) on env by #'cddr
        do (format stream
                   "~&    ~A: ~S~%"
                   k v))
  (values))
