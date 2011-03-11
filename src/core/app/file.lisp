#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.app.file
  (:use :cl
        :clack
        :anaphora)
  (:import-from :clack.util.localtime
                :format-rfc1123-timestring)
  (:import-from :clack.util.hunchentoot
                :mime-type)
  (:import-from :cl-ppcre :scan)
  (:import-from :local-time
                :universal-to-timestamp)
  (:import-from :cl-fad :file-exists-p))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-app-file> (<component>)
     ((file :initarg :file :initform nil :accessor file)
      (root :initarg :root :initform #p"./" :accessor root)
      (encoding :initarg :encoding :initform "utf-8" :accessor encoding))
  (:documentation "Clack Application to serve static files."))

(defmethod call ((this <clack-app-file>) req)
  (let ((file (locate-file (or (file this)
                               ;; remove "/"
                               (subseq (getf req :path-info) 1))
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
      ((not (cl-fad:file-exists-p file)) return-404)
;      ((not (find :user-read (file-permissions file)))
;       return-403)
      (t file))))

(defun text-file-p (content-type)
  (aand (scan "^text" content-type)
        (= it 0)))

(defun serve-file (file encoding)
  (let ((content-type (or (clack.util.hunchentoot:mime-type file)
                          "application/octet-stream"))
        (univ-time (or (file-write-date file)
                       (get-universal-time))))
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
         :last-modified
         ,(format-rfc1123-timestring nil
           (universal-to-timestamp univ-time)))
        ,file))))

(doc:start)

@doc:NAME "
Clack.App.File - Serve static files.
"

@doc:SYNOPSIS "
    ;; THIS IS JUST FOR EXAMPLE
    (clackup (<clack-app-file>
              :root #p\"./static-files/\"))
    
    ;; Then access 'http://localhost/jellyfish.jpg' through into local './static-files/jellyfish.jpg'.
    ;; If the file isn't found, 404 will be returned.
"

@doc:DESCRIPTION "
Clack.App.File serves static files in local directory. This Application should be used in other Middleware or Application (ex. Clack.Middleware.Static).
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Static
"
