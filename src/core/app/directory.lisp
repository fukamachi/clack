#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.app.directory
  (:use :cl
        :clack)
  (:import-from :clack.util
                :html-encode)
  (:import-from :clack.app.file
                :<clack-app-file>
                :should-handle
                :serve-path)
  (:import-from :cl-fad
                :file-exists-p
                :directory-exists-p
                :directory-pathname-p
                :list-directory))
(in-package :clack.app.directory)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-app-directory> (<clack-app-file>) ())

@export
(defun start-server (&key (root (truename ".")) (port 5000))
  (clack:clackup
   (make-instance '<clack-app-directory>
      :root root)
   :port port))

(defmethod should-handle ((this <clack-app-directory>) file)
  (file-exists-p file))

(defmethod serve-path ((this <clack-app-directory>) env file encoding)
  (declare (ignore encoding))
  (if (directory-pathname-p file)
      `(200 nil (,(dir-page
                 (getf env :path-info)
                 (format nil "~A~{~A~}"
                         (dir-file (merge-pathnames "../" file)
                                   :uri ".."
                                   :name "Parent Directory")
                         (mapcar #'dir-file (list-directory file))))))
      (call-next-method)))

(defun dir-file (file &key uri name)
  "Stolen from rack/directory.rb."
  (let* ((dir-p (directory-pathname-p file))
         (uri (or uri
                  (if dir-p
                      (car (last (pathname-directory file)))
                      (file-namestring file)))))
    (format nil "<tr><td class='name'><a href='~A~A'>~A~A</a></td><td class='size'>~:[--~;~:*~:D bytes~]</td><td class='type'>~A</td><td class='mtime'>~A</td></tr>"
            (clack.util.hunchentoot:url-encode uri)
            (if dir-p "/" "")
            (clack.util:html-encode (or name uri))
            (if dir-p "/" "")
            (file-size file)
            (if dir-p
                "directory"
                (or (clack.util.hunchentoot:mime-type file) "text/plain"))
            (clack.util.localtime:format-rfc1123-timestring nil
             (local-time:universal-to-timestamp (file-write-date file))))))

(defun dir-page (path-info body)
  "Stolen from rack/directory.rb."
  (format nil "<html><head>
  <title>Index of ~A</title>
  <meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
  <style type='text/css'>
table { width: 100%; }
tr, td { white-space: nowrap; }
.name { text-align: left; }
.size, .mtime { text-align: right; }
.type { width: 11em; text-align: center; }
.mtime { width: 15em; }
  </style>
</head><body>
<h1>Index of ~:*~A</h1>
<hr />
<table>
  <tr>
    <th class='name'>Name</th>
    <th class='size'>Size</th>
    <th class='type'>Type</th>
    <th class='mtime'>Last Modified</th>
  </tr>
~A
</table>
<hr />
</body></html>"
          (clack.util:html-encode path-info)
          body))

(defun file-size (path)
  "Return the size of the file at `path'. If `path' is a directory, this will return nil."
  (unless (directory-pathname-p path)
    (with-open-file (in path :direction :input
                        :element-type 'unsigned-byte)
      (do ((size 0 (1+ size))
           (byte (read-byte in nil nil)
                 (read-byte in nil nil)))
          ((null byte) size)))))

(doc:start)

@doc:NAME "
Clack.App.Directory - Server static files from document root with directory index.
"

@doc:SYNOPSIS "
    ;; mount /var/www/ to http://localhost:5000/
    (clackup
     (make-instance '<clack-app-directory>
        :root #p\"/var/www/\")
     :port 5000)
    
    ;; same as above
    (clack.app.directory:start-server
       :root #p\"/var/www/\"
       :port 5000)
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.App.File
"
