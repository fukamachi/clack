#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.oauth
  (:use :cl
        :clack
        :anaphora
        :metabang-bind
        :clack.request
        :clack.response))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-oauth> (<middleware>)
     ;; TODO: declare types for each slots. -- Eitarow Fukamachi
     ((path :initarg :path :accessor oauth-path)
      (callback-base :initarg :callback-base :accessor oauth-callback-base)
      (consumer-key :initarg :consumer-key :accessor oauth-consumer-key)
      (consumer-secret :initarg :consumer-secret :accessor oauth-consumer-secret)
      (authorize-uri :initarg :authorize-uri :accessor oauth-authorize-uri)
      (request-token-uri :initarg :request-token-uri :accessor oauth-request-token-uri)
      (access-token-uri :initarg :access-token-uri :accessor oauth-access-token-uri)
      (authorized :initarg :authorized :accessor oauth-authorized)
      (state :initform (make-hash-table :test #'equal) :accessor oauth-state)
      (state-expire :initarg :state-expire :initform 60 :accessor oauth-state-expire)))

(defmethod call ((this <clack-middleware-oauth>) req)
  (if (not (string-equal (oauth-path this) (getf req :path-info)))
      (call-next this req)
      (authorize this (make-request req))))

(defmethod obtain-request-token-from-provider ((this <clack-middleware-oauth>))
  (let* ((req-token (cl-oauth:obtain-request-token
                     (oauth-request-token-uri this)
                     (cl-oauth:make-consumer-token
                      :key (oauth-consumer-key this)
                      :secret (oauth-consumer-secret this))
                     :callback-uri (concatenate 'string
                                                (oauth-callback-base this)
                                                (oauth-path this))))
         (state (oauth-state this)))
    (when (gethash (cl-oauth:token-key req-token) state)
      (error "OAuth request token collision is detected."))
    (setf (gethash (cl-oauth:token-key req-token) state)
          (list req-token (get-universal-time)))
    req-token))

(defmethod obtain-request-token ((this <clack-middleware-oauth>) req)
  (bind ((oauth-token (query-parameter req "oauth_token"))
         ((req-token time) (gethash oauth-token (oauth-state this))))
    @ignore time
    req-token))

(defmethod obtain-access-token ((this <clack-middleware-oauth>) req-token)
  (cl-oauth:obtain-access-token (oauth-access-token-uri this) req-token))

(defmethod cleanup-states ((this <clack-middleware-oauth>))
  (let ((now (get-universal-time))
        (state (oauth-state this))
        (expire (oauth-state-expire this)))
    (loop for k being the hash-keys in state using (hash-value v)
          unless (< (- now (second v)) expire)
            do (remhash k state))))

(defmethod is-expired ((this <clack-middleware-oauth>) oauth-token)
  (let ((now (get-universal-time)))
    (not
     (aand (gethash oauth-token (oauth-state this))
           (< (- now (second it)) (oauth-state-expire this))))))

(defmethod is-authorizing ((this <clack-middleware-oauth>) req)
  (let ((oauth-token (query-parameter req "oauth_token"))
        (oauth-verifier (query-parameter req "oauth_verifier")))
    (and oauth-token oauth-verifier
         (not (is-expired this oauth-token)))))

(defmethod authorize ((this <clack-middleware-oauth>) req)
  (cleanup-states this)
  (if (is-authorizing this req)
      (authorize-cont this req)
      (authorize-init this)))

(defmethod authorize-init ((this <clack-middleware-oauth>))
  (let ((req-token (obtain-request-token-from-provider this))
        (res (make-response)))
    (redirect res (cl-oauth:make-authorization-uri (oauth-authorize-uri this) req-token))
    (finalize res)))

(defmethod authorize-cont ((this <clack-middleware-oauth>) req)
  (let ((oauth-token (query-parameter req "oauth_token"))
        (oauth-verifier (query-parameter req "oauth_verifier"))
        (req-token (obtain-request-token this req)))
    (setf (cl-oauth:request-token-verification-code req-token) oauth-verifier)
    (cl-oauth:authorize-request-token req-token)
    (let ((acc-token (obtain-access-token this req-token)))
      (unless (remhash oauth-token (oauth-state this))
        (error "BUG"))
      (funcall (oauth-authorized this) req acc-token))))

(doc:start)

@doc:NAME "
Clack.Middleware.OAuth - Supports authorization mechanism by OAuth.
"

@doc:SYNOPSIS "
    (run
      (builder
       (<clack-middleware-oauth>
         :consumer-key \"XXXXXXXXXXXXXXXXXXXXX\"
         :consumer-secret \"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX\"
         :authorize-uri \"http://localhost/authorize\"
         :request-token-uri \"http://localhost/request\"
         :access-token-uri \"http://localhost/access\"
         :path \"/auth\"
         :callback-base \"http://localhost/\"
         :authorized #'callback)
       app))
"

@doc:DESCRIPTION "
This is a Clack Middleware for OAuth.
Please read rfc5849 for more information.

## Slots

* consumer-key, consumer-secret (Required, String)

OAuth parameter.

* authorize-uri, request-token-uri, access-token-uri (Required, String)

URIs to process OAuth.

* path

A path to be handled by <clack.middleware.oauth>.

* callback-base

A host address will be passed to OAuth Provider.
(format nil \"~a~a\" `callback-base' `path') should be valid URI.

* authorized (Required, function)

A callback function when authorization is suceeded.
It will be called with two parameters: clack request and cl-oauth:access-token.

"

@doc:AUTHOR "
* Norihisa Fujita (n.fujita@ariel-networks.com)
"
