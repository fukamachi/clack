(in-package :cl-user)
(defpackage clack.session.state.cookie
  (:use :cl
        :clack.session.state)
  (:import-from :clack.request
                :make-request
                :cookies)
  (:import-from :alexandria
                :remove-from-plist
                :ensure-car)
  (:import-from :clack.response
                :make-response
                :set-cookies)
  (:export :path
           :domain
           :expires
           :secure
           :httponly

           ;; imported from clack.session.state
           :valid-sid-p
           :extract-id
           :generate-id))
(in-package :clack.session.state.cookie)

(cl-syntax:use-syntax :annot)

(defun merge-plist (p1 p2)
  "Merges two plists into one plist.
If there are same keys in the two plists, the one in P2 is adopted.

Example:
  (merge-plist '(:apple 1 :grape 2) '(:banana 3 :apple 4))
  ;;=> (:GRAPE 2 :BANANA 3 :APPLE 4)
"
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound)
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

@export
(defclass <clack-session-state-cookie> (<clack-session-state>)
     ((path :type string
            :initarg :path
            :initform "/"
            :accessor path)
      (domain :type (or string null)
              :initarg :domain
              :initform nil
              :accessor domain)
      (expires :type integer
               :initarg :expires
               :initform (get-universal-time)
               :accessor expires)
      (secure :type boolean
              :initarg :secure
              :initform nil
              :accessor secure)
      (httponly :type boolean
                :initarg :httponly
                :initform nil
                :accessor httponly)))

(defgeneric merge-options (state options)
  (:method ((this <clack-session-state-cookie>) options)
    (setf options (remove-from-plist options :id))
    (merge-plist
     (list
      :path (path this)
      :domain (domain this)
      :secure (secure this)
      :httponly (httponly this)
      :expires (+ (get-universal-time) (expires this)))
     options)))

@export
(defmethod expire ((this <clack-session-state-cookie>)
                              id res &optional options)
  (setf (gethash :expires options) 0)
  (finalize this id res options))

@export
(defmethod session-id ((this <clack-session-state-cookie>) env)
  (let ((req (make-request env)))
    ;; `cookies` may returns a list.
    (ensure-car (cookies req (symbol-name (session-key this))))))

@export
(defmethod finalize ((this <clack-session-state-cookie>) id res options)
  (set-cookie this id res
              (merge-options this options)))

(defgeneric set-cookie (state id res options)
  (:method ((this <clack-session-state-cookie>) id res options)
    (let ((r (apply #'make-response res)))
      (setf (set-cookies r (symbol-name (session-key this)))
            (append `(:value ,id) options))
      (clack.response:finalize r))))
