(in-package :cl-user)
(defpackage clack.app.file
  (:use :cl
        :clack.component)
  (:import-from :trivial-mimes
                :mime-lookup)
  (:import-from :cl-ppcre
                :scan)
  (:import-from :local-time
                :format-rfc1123-timestring
                :universal-to-timestamp)
  (:import-from :uiop
                :file-exists-p
                :directory-exists-p)
  (:import-from :alexandria
                :when-let))
(in-package :clack.app.file)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-app-file> (<component>)
     ((file :type (or string null)
            :initarg :file
            :initform nil
            :accessor file)
      (root :type pathname
            :initarg :root
            :initform #p"./"
            :accessor root)
      (encoding :type string
                :initarg :encoding
                :initform "utf-8"
                :accessor encoding))
  (:documentation "Clack Application to serve static files."))

(defmethod call ((this <clack-app-file>) env)
  (let ((file (locate-file this
                           (or (file this)
                               ;; remove "/"
                               (subseq (getf env :path-info) 1))
                           (root this))))
    (if (consp file) ;; some error case
        file
        (serve-path this env file (encoding this)))))

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

@export
(defgeneric should-handle (app file)
  (:method ((this <clack-app-file>) file)
    (and (ignore-errors
          ;; Ignore simple-file-error in a case that
          ;; the file path contains some special characters like "?".
          ;; See https://github.com/fukamachi/clack/issues/111
          (uiop:file-exists-p file))
         (not (uiop:directory-exists-p file)))))

@export
(defgeneric locate-file (app path root)
  (:method ((this <clack-app-file>) path root)
    (when (find :up (pathname-directory path) :test #'eq)
      (return-from locate-file return-400))

    (let ((file (merge-pathnames path root)))
      (cond
        ((position #\Null (namestring file)) return-400)
        ((not (should-handle this file)) return-404)
;      ((not (find :user-read (file-permissions file)))
;       return-403)
        (t file)))))

(defun text-file-p (content-type)
  (when-let (pos (scan "^text" content-type))
    (= pos 0)))

@export
(defgeneric serve-path (app env file encoding)
  (:method ((this <clack-app-file>) env file encoding)
    (let ((content-type (or (mime-lookup file) "text/plain"))
          (univ-time (or (file-write-date file)
                         (get-universal-time))))
      (when (text-file-p content-type)
        (setf content-type
              (format nil "~A;charset=~A"
                      content-type encoding)))
      (with-open-file (stream file
                              :direction :input
                              :if-does-not-exist nil)
        `(200
          (:content-type ,content-type
           :content-length ,(file-length stream)
           :last-modified
           ,(format-rfc1123-timestring nil
                                       (universal-to-timestamp univ-time)))
          ,file)))))
