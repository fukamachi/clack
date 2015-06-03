(in-package :cl-user)
(defpackage clack.session.store.dbi
  (:use :cl
        :clack.session.store)
  (:import-from :marshal
                :marshal
                :unmarshal)
  (:import-from :base64
                :base64-string-to-string
                :string-to-base64-string))
(in-package :clack.session.store.dbi)

(syntax:use-syntax :annot)

@export
(defclass <clack-session-store-dbi> (<clack-session-store>)
  ((connect-args :type list
                 :initarg :connect-args
                 :reader store-connect-args)
   (connector :type function
              :initarg :connector
              :reader store-connector)
   (table-name :type string
               :initarg :table-name
               :initform "sessions"
               :reader store-table-name)
   (%connection)))

(defmethod initialize-instance :after ((store <clack-session-store-dbi>) &key)
  (unless (or (slot-boundp store 'connector)
              (slot-boundp store 'connect-args))
    (error ":connect-args or :connector is required.")))

(defun store-connection (store)
  (cond
    ((slot-boundp store 'connector)
     (funcall (slot-value store 'connector)))
    ((slot-boundp store '%connection)
     (slot-value store '%connection))
    (T (setf (slot-value store '%connection)
             (apply #'dbi:connect (store-connect-args store))))))

(defmethod fetch ((store <clack-session-store-dbi>) sid)
  (let* ((query (dbi:prepare (store-connection store)
                             (format nil "SELECT session_data FROM ~A WHERE id = ?"
                                     (store-table-name store))))
         (result (dbi:fetch (dbi:execute query sid))))
    (if result
        (ignore-errors
         (unmarshal
          (read-from-string
           (base64-string-to-string
            (getf result :|session_data|)))))
        nil)))

(defmethod store-session ((store <clack-session-store-dbi>) sid session)
  (let* ((conn (store-connection store))
         (query (dbi:prepare conn
                             (format nil "SELECT 1 FROM ~A WHERE id = ?"
                                     (store-table-name store))))
         (existsp (dbi:fetch (dbi:execute query sid)))
         (base64-session (string-to-base64-string (prin1-to-string (marshal session)))))
    (if existsp
        (dbi:do-sql conn (format nil "UPDATE ~A SET session_data = ? WHERE id = ?"
                                 (store-table-name store))
          base64-session
          sid)
        (dbi:do-sql conn (format nil "INSERT INTO ~A (id, session_data) VALUES (?, ?)"
                                 (store-table-name store))
          sid
          base64-session))))

(defmethod remove-session ((store <clack-session-store-dbi>) sid)
  (dbi:do-sql (store-connection store)
    (format nil "DELETE FROM ~A WHERE id = ?"
            (store-table-name store))
    sid))
