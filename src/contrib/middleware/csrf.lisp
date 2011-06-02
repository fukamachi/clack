#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.csrf
  (:use :cl
        :clack
        :anaphora)
  (:import-from :clack.request
                :make-request
                :body-parameter)
  (:import-from :clack.util
                :generate-random-id))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-csrf> (<middleware>)
     ((error-function :initarg :error-function
                    :type function
                    :initform #'return-400
                    :accessor error-function))
  (:documentation "Clack Middleware for easy CSRF protection."))

(defmethod call ((this <clack-middleware-csrf>) env)
  (unless (danger-method-p (getf env :request-method))
    (return-from call (call-next this env)))

  (if (valid-token-p env)
      (call-next this env)
      (funcall (error-function this) env)))

(defun return-400 (env)
  @ignore env
  '(400
    (:content-type "text/plain"
     :content-length 31)
    ("Bad Request: invalid CSRF token")))

(defun danger-method-p (request-method)
  (member request-method
          '(:POST :PUT :DELETE :PATCH)
          :test #'eq))

(defun valid-token-p (env)
  (let ((req (make-request env)))
    (string= (body-parameter req :|_csrf_token|)
             (gethash :csrf-token
                      (getf env :clack.session)))))

@export
(defun csrf-html-tag (session)
  "Return an 'input' tag containing random CSRF token.
Note this has a side-effect, natually. This function stores the generated id into the current session when called."
  @type hash-table session
  (sunless (gethash :csrf-token session)
    (setf it (generate-random-id)))
  (concatenate
   'string
   "<input type=\"hidden\" name=\"_csrf_token\" value=\""
   (gethash :csrf-token session)
   "\" />"))

(doc:start)

@doc:NAME "
Clack.Middleware.Csrf - Middleware for easy CSRF protection.
"

@doc:SYNOPSIS "
    ;; building application.
    (builder <clack-middleware-csrf> app)
    
    ;; in CL-EMB template.
    <form name=\"test-form\" method=\"post\" action=\"/\">
    <input type=\"text\" name=\"name\" />
    <%= (csrf-html-tag session) %>
    <input type=\"submit\" value=\"Send\" />
    </form>
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Session
"
