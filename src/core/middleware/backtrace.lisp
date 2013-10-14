#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2013 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.backtrace
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :trivial-backtrace
                :print-backtrace))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-backtrace> (<middleware>) ())

(defmethod call ((this <clack-middleware-backtrace>) env)
  (handler-case (call-next this env)
    (error (e)
      (print-error e env *error-output*)
      '(500 () ("500 Internal Server Error")))))

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

(doc:start)

@doc:NAME "
Clack.Middleware.Backtrace
"

@doc:SYNOPSIS "
    (clackup (builder
              <clack-middleware-backtrace>
              (lambda (env)
                (error \"Fatal error! Help!!\")
                '(200 () (\"ok? (probably not)\")))))
"

@doc:DESCRIPTION "
Clack.Middleware.Backtrace catches all errors and outputs their backtraces to `*error-output*`. This aims to be a safty net for production environment.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
