(in-package :cl-user)
(defpackage t.clack.middleware.csrf
  (:use :cl
        :prove
        :clack
        :clack.builder
        :clack.request)
  (:import-from :clack.test
                :test-app)
  (:import-from :drakma
                :http-request
                :cookie-jar)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>)
  (:import-from :clack.middleware.csrf
                :<clack-middleware-csrf>
                :csrf-html-tag))
(in-package :t.clack.middleware.csrf)

(plan 17)

(defun html-form (env)
  (concatenate
   'string
   "
<html>
<body>
<form name=\"test\" method=\"post\" action=\"/\">
<input name=\"name\" value=\"\" />
"
   (csrf-html-tag (getf env :clack.session))
   "
<input type=\"submit\" />
</form>
</body>
</html>
"))

(defvar app)
(setf app
    (builder <clack-middleware-session>
             <clack-middleware-csrf>
             #'(lambda (env)
                 (let ((req (make-request env)))
                   `(200
                     (:content-type "text/html")
                     (,(if (and (eq :post (request-method req))
                                (body-parameter req :|name|))
                           (body-parameter req :|name|)
                           (html-form env))))))))

(defun parse-csrf-token (body)
  (let ((match (nth-value
                1
                (ppcre:scan-to-strings
                 "name=\"_csrf_token\" value=\"(.+?)\"" body))))
    (and match (elt match 0))))

#+thread-support
(subtest-app "csrf"
    app
  (let (csrf-token
        (cookie-jar (make-instance 'cookie-jar)))
    (diag "first POST request")
    (is (nth-value 1 (http-request "http://localhost:4242/"
                                   :method :post
                                   :cookie-jar cookie-jar))
        400)
    (diag "first GET request")
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :cookie-jar cookie-jar)
      (is status 200 "Status is 200")
      (is (cdr (assoc :content-type headers)) "text/html" "Content-Type is text/html")
      (setf csrf-token (parse-csrf-token body))
      (ok csrf-token "can get CSRF token")
      (is-type csrf-token 'string "CSRF token is string")
      (is (length csrf-token) 40 "CSRF token is 40 chars"))
    (diag "bad POST request (no token)")
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :method :post
                      :cookie-jar cookie-jar)
      (is status 400 "Status is 400")
      (is (cdr (assoc :content-type headers)) "text/plain" "Content-Type is text/plain")
      (is body "Bad Request: invalid CSRF token" "Body is 'forbidden'"))
    (diag "bad POST request (wrong token)")
    (is (nth-value
         1
         (http-request "http://localhost:4242/"
                       :method :post
                       :parameters '(("name" . "Eitaro Fukamachi")
                                     ("_csrf_token" . "wrongtokeniknow"))
                       :cookie-jar cookie-jar))
        400)
    (diag "valid POST request")
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :method :post
                      :parameters `(("name" . "Eitaro Fukamachi")
                                    ("_csrf_token" . ,csrf-token))
                      :cookie-jar cookie-jar)
      (is status 200 "Status is 200")
      (is (cdr (assoc :content-type headers)) "text/html" "Content-Type is text/html")
      (is body "Eitaro Fukamachi" "can read body-parameter"))))
#-thread-support
(skip 13 "because your lisp doesn't support threads")

(setf app
      (builder <clack-middleware-session>
               (<clack-middleware-csrf>
                :block-app #'(lambda (env)
                               (declare (ignore env))
                               '(302
                                 (:location "http://en.wikipedia.org/wiki/CSRF")
                                 nil)))
               #'(lambda (env)
                   (declare (ignore env))
                   `(200
                     (:content-type "text/html")
                     ("You look a safety user.")))))

#+thread-support
(subtest-app "change blocking behavior"
    app
  (multiple-value-bind (body status headers)
      (http-request "http://localhost:4242/"
                    :method :post
                    :redirect nil)
    (declare (ignore body))
    (is status 302 "Status is 302")
    (is (cdr (assoc :location headers)) "http://en.wikipedia.org/wiki/CSRF")))
#-thread-support
(skip 2 "because your lisp doesn't support threads")

(setf app
      (builder <clack-middleware-session>
               (<clack-middleware-csrf> :one-time-p t)
               #'(lambda (env)
                   (let ((req (make-request env)))
                     `(200
                       (:content-type "text/html")
                       (,(if (and (eq :post (request-method req))
                                  (body-parameter req :|name|))
                             (body-parameter req :|name|)
                             (html-form env))))))))

#+thread-support
(subtest-app "Enable one-time token"
    app
  (let (csrf-token
        (cookie-jar (make-instance 'cookie-jar)))
    (setf csrf-token
          (parse-csrf-token
           (http-request "http://localhost:4242/"
                         :cookie-jar cookie-jar)))
    (http-request "http://localhost:4242/"
                  :method :post
                  :parameters `(("name" . "Eitaro Fukamachi")
                                ("_csrf_token" . ,csrf-token))
                  :cookie-jar cookie-jar)
    (diag "bad POST request with before token")
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :method :post
                      :parameters `(("name" . "Eitaro Fukamachi")
                                    ("_csrf_token" . ,csrf-token))
                      :cookie-jar cookie-jar)
      (declare (ignore body))
      (is status 400 "Status is 400")
      (is (cdr (assoc :content-type headers)) "text/plain" "Content-Type is text/plain"))))
#-thread-support
(skip 2 "because your lisp doesn't support threads")

(finalize)
