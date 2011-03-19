#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.auth.basic
  (:use :cl
        :clack
        :split-sequence)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-base64
                :base64-string-to-string)
  (:import-from :arnesi
                :aand
                :it))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-auth-basic> (<middleware>)
     ((authenticator :type function
                     :initarg :authenticator
                     :initform #'(lambda (user pass)
                                   @ignore (user pass)
                                   t)
                     :accessor authenticator)
      (realm :type string
             :initarg :realm
             :initform "restricted area"
             :accessor realm))
  (:documentation "Clack Middleware to authenticate."))

(defmethod call ((this <clack-middleware-auth-basic>) req)
  (unless (getf req :http-authorization)
    (return-from call (unauthorized this)))

  (destructuring-bind (user &optional (pass ""))
      (parse-user-and-pass (getf req :http-authorization))
    (if (and user
             (funcall (authenticator this) user pass))
        (progn
          (setf (getf req :remote-user) user)
          (call-next this req))
        (unauthorized this))))

(defmethod unauthorized ((this <clack-middleware-auth-basic>))
  `(401
    (:content-type "text/plain"
     :content-length 22
     :www-authenticate ,(format nil "Basic realm=~A" (realm this)))
    ("Authorization required")))

(defun parse-user-and-pass (auth)
  (aand auth
        (nth-value 1 (scan-to-strings "^Basic (.*)$" it))
        (aref it 0)
        (base64:base64-string-to-string it)
        (split-sequence #\: it)))

(doc:start)

@doc:NAME "
Clack.Middleware.Auth.Basic - Basic Authentication Middleware.
"

@doc:SYNOPSIS "
    (clackup
     (builder
      (<clack-middleware-auth-basic>
       :authenticator #'(lambda (user pass)
                          (and (string= user \"hoge\")
                               (string= pass \"fuga\"))))
      app))
"

@doc:DESCRIPTION "
Clack.Middleware.Auth.Basic allows to basic authenticate simply. All you have to do is pass a function to authenticate to `<clack-middleware-auth-basic>`, `:authenticator`.

    (make-instance '<clack-middleware-auth-basic>
       :authenticator
       #'(lambda (user pass)
           ;; Authenticate here and return t or nil.
           ))
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
