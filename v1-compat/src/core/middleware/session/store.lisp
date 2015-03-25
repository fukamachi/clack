(in-package :cl-user)
(defpackage clack.session.store
  (:use :cl))
(in-package :clack.session.store)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-session-store> ()
     ((stash :type hash-table
             :initform (make-hash-table :test 'equal)
             :accessor stash)))

@export
(defgeneric fetch (store sid)
  (:method ((this <clack-session-store>) sid)
    (gethash sid (stash this))))

@export
(defgeneric store-session (store sid session)
  (:method ((this <clack-session-store>) sid session)
    (setf (gethash sid (stash this))
          session)))

@export
(defgeneric remove-session (store sid)
  (:method ((this <clack-session-store>) sid)
    (remhash sid (stash this))))
