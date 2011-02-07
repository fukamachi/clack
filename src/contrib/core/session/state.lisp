#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Session.State
  Basic parameter-based session state.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.session.state
  (:use :cl
        :anaphora
        :cl-ppcre
        :ironclad)
  (:shadowing-import-from :cl :null)
  (:export :<clack-session-state>
           :session-key
           :sid-generator
           :sid-validator
           :session-id
           :valid-session-p
           :extract
           :generate
           :finalize
           :expire-session-id))

(defclass <clack-session-state> ()
     ((session-key :initarg :session-key :initform :clack.session
                   :accessor session-key)
      (sid-generator :initarg :sid-generator
                     :initform
                     (lambda (&rest args)
                       (declare (ignore args))
                       (byte-array-to-hex-string
                        (digest-sequence
                         (make-digest :sha1)
                         (format nil "~A~A"
                                 (random 1.0) (get-universal-time)))))
                     :accessor sid-generator)
      (sid-validator :initarg :sid-validator
                     :initform
                     (lambda (sid)
                       (not (null (scan "\\A[0-9a-f]{40}\\Z" sid))))
                     :accessor sid-validator)))

(defmethod expire-session-id ((this <clack-session-state>)
                              id res &optional options)
  (declare (ignore this id res options)))

(defmethod session-id ((this <clack-session-state>) req)
  (getf req (session-key this)))

(defmethod valid-sid-p ((this <clack-session-state>) id)
  (funcall (sid-validator this) id))

(defmethod extract ((this <clack-session-state>) req)
  (aand (session-id this req)
        (valid-sid-p this it)
        it))

(defmethod generate ((this <clack-session-state>) &rest args)
  (apply (sid-generator this) args))

(defmethod finalize ((this <clack-session-state>) id res options)
  (declare (ignore this id options))
  res)
