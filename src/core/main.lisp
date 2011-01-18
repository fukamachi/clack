#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Functions of Clack Web server.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack)

(defparameter *lazy-load* nil)

(defmacro builder (&rest app-or-middleware)
  "Wrap Clack application with middlewares and return it as one function."
  (let ((req (gensym "REQ"))
        (form
         `(reduce #'wrap
                  (list ,@(loop for arg in (butlast app-or-middleware)
                                if (consp arg)
                                  collect `(make-instance ',(car arg) ,@(cdr arg))
                                else collect `(make-instance ',arg)))
                  :initial-value ,(car (last app-or-middleware))
                  :from-end t)))
    `(if *lazy-load*
         (lambda (,req) (funcall (eval ',form) ,req))
         ,form)))

(defmethod call ((app function) req)
  "Functions should be called like Middleware."
  (funcall app req))

(defun clackup (filepath &key (server :hunchentoot) port)
  "Load given file and run it as a Clack Application."
  (let ((pkg (intern (string-upcase (pathname-name filepath)) :keyword))
        (handler (intern (format nil "CLACK.HANDLER.~:@(~A~)" server) :keyword)))
    (eval `(progn
             (defpackage ,pkg (:use :cl :clack ,handler))
             (in-package ,pkg)))
    (let ((run (intern "RUN" (find-package handler)))
          (app (intern "APP" (find-package pkg))))
      (eval
       `(progn
          ,(read-from-string (alexandria:read-file-into-string filepath))
          ,(if port
               `(,run ,app :port ,port)
               `(,run ,app)))))))
