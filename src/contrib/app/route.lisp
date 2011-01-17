#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>
  Copyright (c) 2011 Tomohiro Matsuyama <tomo@cx4a.org>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.App.Route.
  URL dispatcher.

  Author: Tomohiro Matsuyama <tomo@cx4a.org>
|#

(in-package :cl-user)

(defpackage clack.app.route
  (:use :cl
        :clack
        :alexandria
        :cl-ppcre)
  (:export :defroutes))

(in-package :clack.app.route)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-path (path)
    (let* (names
           (regex (format nil "^~A$"
                          (regex-replace-all "\\\\:([\\w-]+)"
                                             (quote-meta-chars path)
                                             (lambda (name &rest _)
                                               (declare (ignore _))
                                               (push (string-upcase (subseq name 2)) names)
                                               "(.+?)")
                                             :simple-calls t))))
      (list regex (nreverse names)))))

(defmacro defroutes (name &body routes &aux otherwise)
  (let ((last (last routes)))
    (if (member (car last) '(t otherwise))
        (setf routes (butlast routes)
              otherwise last)))
  (with-gensyms (req request-method request-path matched regs)
    `(defun ,name (,req)
       (let ((,request-method (getf ,req :request-method))
             (,request-path (getf ,req :path-info)))
         (declare (ignorable ,request-method ,request-path))
         (or ,@(loop for (method path form) in routes
                     for (regex names) = (compile-path path)
                     for symbols = (mapcar (lambda (name) (intern name *package*)) names)
                     collect `(and (string= ,request-method ',method)
                                   (multiple-value-bind (,matched ,regs)
                                       (scan-to-strings ,regex ,request-path)
                                     (declare (ignorable ,regs))
                                     (if ,matched
                                         ,(if symbols
                                              `(destructuring-bind ,symbols (coerce ,regs 'list)
                                                 (declare (ignorable ,@symbols))
                                                 (call ,form ,req))
                                              `(call ,form ,req))))))
             ,(if otherwise
                  `(call ,(cadr otherwise) ,req)
                  '(list 404 nil nil)))))))
