#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>
  Copyright (c) 2011 Tomohiro Matsuyama <tomo@cx4a.org>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.app.route
  (:use :cl
        :clack)
  (:import-from :cl-ppcre
                :scan-to-strings
                :split
                :quote-meta-chars)
  (:import-from :alexandria
                :with-gensyms))

(cl-annot:enable-annot-syntax)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun compile-path (path)
    (loop with list = (split ":([\\w-]+)" path :with-registers-p t)
          while list
          for prefix = (pop list)
          for name = (pop list)
          collect (quote-meta-chars prefix) into parts
          if name
            collect (string-upcase name) into names
            and collect "(.+?)" into parts
          finally
       (return (list (format nil "^~{~A~}$"  parts)
                     names)))))

@export
(defmacro defroutes (name &body routes &aux (otherwise (last routes)))
  (if (member (car otherwise) '(t otherwise))
      (setf routes (butlast routes))
      (setf otherwise nil))
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

(doc:start)

@doc:NAME "
Clack.App.Route - URL dispatcher.
"

@doc:SYNOPSIS "
    (defpackage clack-sample
      (:use :cl
            :clack
            :clack.app.route))
    (in-package :clack-sample)
    
    (defroute app (req)
      (GET \"/\" #'index)
      (GET \"/login\" #'login)
      (POST \"/login\" #'authorize)
      (GET \"/member/:id\" #'member))
    
    (clackup #'app)
"

@doc:DESCRIPTION "
Clack.App.Route provides an URL based dispacher, inspired by Ruby's Sinatra.
"

@doc:AUTHOR "
* Tomohiro Matsuyama (tomo@cx4a.org)
"
