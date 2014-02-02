(in-package :cl-user)
(defpackage clack.middleware.auth.hatena
  (:use :cl
        :clack
        :anaphora)
  (:import-from :alexandria
                :alist-plist)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-sequence
                :ascii-string-to-byte-array)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :drakma
                :http-request)
  (:import-from :yason
                :decode-json))
(in-package :clack.middleware.auth.hatena)

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-auth-hatena> (<middleware>)
     ((consumer-key :type string
                    :initarg :consumer-key
                    :accessor consumer-key)
      (consumer-secret :type string
                       :initarg :consumer-secret
                       :accessor consumer-secret)
      (authorize-uri :type string
                     :initarg :authorize-uri
                     :accessor authorize-uri)
      (cert-uri :type string
                :initarg :cert-uri
                :accessor cert-uri)
      (callback-uri :type string
                    :initarg :callback-uri
                    :accessor callback-uri)
      (auth-path :type string
                 :initarg :auth-path
                 :accessor auth-path)))

(defmethod call ((this <clack-middleware-auth-hatena>) env)
  (if (string-equal (getf env :path-info)
                    (auth-path this))
      (authorize this env)
      (call-next this env)))

(defmethod authorize ((this <clack-middleware-auth-hatena>) env)
  (let ((params
         (clack.request::parameters->plist (getf env :query-string))))
    (aif (getf params :|cert|)
         (progn
           ;; TODO: catch error
           (setf (gethash :hatena.user (getf env :clack.session))
                 (alexandria:alist-plist
                  (cdr (assoc :user (user-info this it)))))
           `(302 (:location ,(callback-uri this)) nil))
         `(302 (:location ,(auth-uri this)) nil))))

(defun md5hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5
    (ironclad:ascii-string-to-byte-array string))))

(defun api-signature (secret params)
  (let ((keys (sort
               (loop for key in params by #'cddr
                     collect key)
               #'string-lessp)))
    (md5hex
     (apply #'concatenate 'string
            secret
            (loop for key in keys
                  collect (concatenate 'string (string key) (getf params key)))))))

(defmethod auth-uri ((this <clack-middleware-auth-hatena>))
  (format nil
          "~A?api_key=~A&api_sig=~A"
          (authorize-uri this)
          (consumer-key this)
          (api-signature
           (consumer-secret this)
           (list :|api_key|
                 (consumer-key this)))))

(defmethod api-uri ((this <clack-middleware-auth-hatena>) params)
  (format nil
          "~A?api_key=~A&cert=~A&api_sig=~A"
          (cert-uri this)
          (consumer-key this)
          (getf params :|cert|)
          (api-signature (consumer-secret this) params)))

(defmethod user-info ((this <clack-middleware-auth-hatena>) cert)
  (with-input-from-string
      (s (flex:octets-to-string
          (drakma:http-request (api-uri this `(:|api_key|
                                               ,(consumer-key this)
                                               :|cert| ,cert)))))
    (decode-json s)))
