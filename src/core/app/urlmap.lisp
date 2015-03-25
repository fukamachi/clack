#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.app.urlmap
  (:use :cl
        :clack)
  (:import-from :cl-ppcre
                :scan
                :scan-to-strings
                :regex-replace)
  (:import-from :alexandria
                :if-let))
(in-package :clack.app.urlmap)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-app-urlmap> (<component>)
     ((%mapping :type list
                :initform nil))
  (:documentation "Class to map multiple apps in different paths."))

@export
(defgeneric mount (app-urlmap location app)
  (:documentation "Register an `app' to the `location'.")
  (:method ((this <clack-app-urlmap>) location app)
    (destructuring-bind (host location)
        (if-let (matches (nth-value 1
                                    (scan-to-strings "^https?://(.*?)(/.*)" location)))
          (coerce matches 'list)
          (list nil location))
      (unless (char= #\/ (aref location 0))
        (error "Paths need to start with /"))
      (push (list host location app)
            (slot-value this '%mapping)))))

(defmethod call ((this <clack-app-urlmap>) env)
  (let ((http-host
         (regex-replace (format nil ":~D" (getf env :server-port))
                        (getf env :http-host)
                        "")))
    (loop for (host location app) in (slot-value this '%mapping)
          if (and (or (not host)
                      (string= http-host host)
                      (string= (getf env :server-name) host))
                  (scan (format nil "^~A" location) (getf env :path-info)))
            do (setf (getf env :path-info)
                     (regex-replace location (getf env :path-info) "/"))
               (setf (getf env :script-name)
                     (concatenate 'string
                                   (getf env :script-name)
                                  location))
               (return (call app env))
          finally (return '(404 (:content-type "text/plain") ("Not Found"))))))

@export
(defmacro builder-urlmap (&rest apps)
  "Useful syntax sugar for building applications."
  (let ((urlmap (gensym "URLMAP")))
    `(let ((,urlmap (make-instance '<clack-app-urlmap>)))
       ,@(mapcar
          (lambda (app) `(mount ,urlmap ,@app))
          apps)
       ,urlmap)))

(doc:start)

@doc:NAME "
Clack.App.URLMap - Map multiple apps in different paths.
"

@doc:SYNOPSIS "
    (defparameter *urlmap* (make-instance '<clack-app-urlmap>))
    (mount *urlmap* \"/pc/\" #'app-for-pc)
    (mount *urlmap* \"/api/\" #'app-for-api)
    (call *urlmap* env)
    
    ;; Useful synonym.
    (builder-urlmap
      (\"/pc/\" #'app-for-pc)
      (\"/api/\" #'app-for-api))
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Builder
"
