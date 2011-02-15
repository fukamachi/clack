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
  (:export :session-key
           :sid-generator
           :sid-validator))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-session-state> ()
     ((session-key :initarg :session-key :initform :clack.session
                   :accessor session-key)
      (sid-generator :initarg :sid-generator
                     :initform
                     (lambda (&rest args)
                       (declare (ignore args))
                       (byte-array-to-hex-string
                        (digest-sequence
                         (make-digest :SHA1)
                         (format nil "~A~A"
                                 (random 1.0) (get-universal-time)))))
                     :accessor sid-generator)
      (sid-validator :initarg :sid-validator
                     :initform
                     (lambda (sid)
                       (not (null (scan "\\A[0-9a-f]{40}\\Z" sid))))
                     :accessor sid-validator)))

@export
(defmethod expire ((this <clack-session-state>)
                   id res &optional options)
  (declare (ignore this id res options)))

@export
(defmethod session-id ((this <clack-session-state>) req)
  (getf req (session-key this)))

@export
(defmethod valid-sid-p ((this <clack-session-state>) id)
  (funcall (sid-validator this) id))

@export
(defmethod extract-id ((this <clack-session-state>) req)
  (aand (session-id this req)
        (valid-sid-p this it)
        it))

@export
(defmethod generate-id ((this <clack-session-state>) &rest args)
  (apply (sid-generator this) args))

@export
(defmethod finalize ((this <clack-session-state>) id res options)
  (declare (ignore this id options))
  res)
