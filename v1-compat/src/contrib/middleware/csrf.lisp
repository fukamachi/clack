(in-package :cl-user)
(defpackage clack.middleware.csrf
  (:use :cl
        :clack.component
        :clack.middleware)
  (:import-from :clack.request
                :make-request
                :body-parameter)
  (:import-from :lack.util
                :generate-random-id)
  (:import-from :alexandria
                :when-let))
(in-package :clack.middleware.csrf)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-csrf> (<middleware>)
     ((block-app :initarg :block-app
                 :type component-designator
                 :initform #'return-400
                 :accessor block-app)
      (one-time-p :initarg :one-time-p
                  :type boolean
                  :initform nil
                  :accessor one-time-p))
  (:documentation "Clack Middleware for easy CSRF protection."))

(defmethod call ((this <clack-middleware-csrf>) env)
  (unless (danger-method-p (getf env :request-method))
    (return-from call (call-next this env)))

  (unless (getf env :clack.session)
    (error ":clack.session is not found in `env'."))

  (if (valid-token-p env)
      (progn
        ;; delete onetime token
        (when (one-time-p this)
          (remhash :csrf-token (getf env :clack.session)))
        (call-next this env))
      (call (block-app this) env)))

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
    (when-let (csrf-token (gethash :csrf-token
                                   (getf env :clack.session)))
      (string= csrf-token (body-parameter req "_csrf_token")))))

@export
(defun csrf-token (session)
  "Return a random CSRF token."
  (unless (gethash :csrf-token session)
    (setf (gethash :csrf-token session) (generate-random-id)))
  (gethash :csrf-token session))

@export
(defun csrf-html-tag (session)
  "Return an 'input' tag containing random CSRF token.
Note this has a side-effect, natually. This function stores the generated id into the current session when called."
  @type hash-table session
  (concatenate
   'string
   "<input type=\"hidden\" name=\"_csrf_token\" value=\""
   (csrf-token session)
   "\" />"))
