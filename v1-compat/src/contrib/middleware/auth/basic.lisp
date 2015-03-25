(in-package :cl-user)
(defpackage clack.middleware.auth.basic
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :cl-ppcre
                :scan-to-strings)
  (:import-from :cl-base64
                :base64-string-to-string)
  (:import-from :arnesi
                :aand
                :it))
(in-package :clack.middleware.auth.basic)

(cl-syntax:use-syntax :annot)

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

(defmethod call ((this <clack-middleware-auth-basic>) env)
  (let ((authorization (gethash "authorization" (getf env :headers))))
    (unless authorization
      (return-from call (unauthorized this)))

    (destructuring-bind (user &optional (pass ""))
        (parse-user-and-pass authorization)
      (if user
          (multiple-value-bind (result returned-user)
              (funcall (authenticator this) user pass)
            (if result
                (progn
                  (setf (getf env :remote-user)
                        (or returned-user
                            user))
                  (call-next this env))
                (unauthorized this)))
          (unauthorized this)))))

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
        (cons (scan-to-strings "[^:]+" it)
              (coerce (nth-value 1 (scan-to-strings ":(.+)" it))
                      'list))))
