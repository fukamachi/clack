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

(in-package :clack.app.file)

(defclass <clack-app-file> (<component>)
     ((file :initarg :file :accessor file)
      (root :initarg :root :initform "." :accessor root)
      (encoding :initarg :encoding :initform "utf-8" :accessor encoding))
  (:documentation "Clack Application to serve static files."))

(defmethod call ((self <clack-app-file>) req)
  (let ((file (locate-file (getf req :path-info) (root self))))
    (when file (serve-file file (encoding self)))))

(defparameter return-403
              '(403(:content-type "text/plain"
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

(defun locate-file (path-info &optional (root "."))
  (let ((file (merge-pathnames root path-info)))
    (cond
      ((position #\Null path-info) return-400)
      ((file-exists-p file) return-400)
      ((not (find :user-read (file-permissions file)))
       return-403)
      (t file))))

(defun text-file-p (content-type)
  (= 0 (ppcre:scan "^text" content-type)))

(defun add-charset (content-type encoding)
  (concatenate 'string
               content-type
               "; charset="
               encoding))

(defun serve-file (file encoding)
  (let ((content-type (or (mime-type file) "text/plain"))
        (univ-time (or (file-write-date path) (get-universal-time))))
    (when (text-file-p content-type)
      (setf content-type
            (concatenate 'string
                         content-type "; charset=" encoding)))
    `(200 (:content-type ,content-type
           :content-length ,(file-length file)
           :last-modified ,(format-timestring nil
                                              (universal-to-timestamp univ-time)
                                              :format +rfc-1123-format+))
          ,(open file
                 :direction :input
                 :element-type 'octet
                 :if-does-not-exist))))
