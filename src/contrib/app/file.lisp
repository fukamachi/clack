#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.App.File.
  Serve static files.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.app.file
  (:use :cl
        :cl-ppcre
        :cl-fad
        :local-time
        :clack.component
        :anaphora)
  (:export :<clack-app-file>))

(in-package :clack.app.file)

(defclass <clack-app-file> (<component>)
     ((file :initarg :file :initform nil :accessor file)
      (root :initarg :root :initform #p"./" :accessor root)
      (encoding :initarg :encoding :initform "utf-8" :accessor encoding))
  (:documentation "Clack Application to serve static files."))

(defmethod call ((this <clack-app-file>) req)
  (let ((file (locate-file (or (file this) (getf req :path-info))
                           (root this))))
    (if (consp file) ;; some error case
        file
        (serve-file file (encoding this)))))

(defparameter return-403
              '(403 (:content-type "text/plain"
                     :content-length 9)
                ("forbidden")))

(defparameter return-400
              '(400 (:content-type "text/plain"
                     :content-length 11)
                ("Bad Request")))

(defparameter return-404
              '(404 (:content-type "text/plain"
                     :content-length 9)
                ("not found")))

(defun locate-file (path root)
  (let ((file (merge-pathnames path root)))
    (cond
      ((position #\Null (namestring file)) return-400)
      ((not (file-exists-p file)) return-404)
;      ((not (find :user-read (file-permissions file)))
;       return-403)
      (t file))))

(defun text-file-p (content-type)
  (aand (scan "^text" content-type)
        (= it 0)))

(defun serve-file (file encoding)
  (let ((content-type (or "application/octet-stream")) ;FIXME: implement `mime-type'.
        (univ-time (or (file-write-date file) (get-universal-time))))
    (when (text-file-p content-type)
      (setf content-type
            (format nil "~A ;charset=~A"
                    content-type encoding)))
    (with-open-file (stream file
                     :direction :input
                     :if-does-not-exist nil)
      `(200
        (:content-type ,content-type
         :content-length ,(file-length stream)
         :last-modified ,(format-timestring
                          nil
                          (universal-to-timestamp univ-time)
                          :format +rfc-1123-format+))
        ,file))))
