#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc
  (:use :cl)
  (:import-from :cl-markdown
                :markdown)
  (:import-from :clack.doc.class
                :generate-documentation
                :find-system-packages
                :doc-name)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded))
(in-package :clack.doc)

(cl-annot:enable-annot-syntax)

@export
(defmethod generate-documentation ((system asdf:system))
  (ensure-system-loaded system)
  (let ((packages (find-system-packages system)))
    (loop for pkg in (reverse packages)
          do (with-open-file (stream (format nil "~(~A~).html" (doc-name pkg))
                                     :direction :output
                                     :if-exists :supersede)
               (write-string "<html><head><link href=\"doc.css\" rel=\"stylesheet\" type=\"text/css\"></head><body>" stream)
               (markdown (generate-documentation pkg) :stream stream)
               (write-string "</body></html>" stream)))
    (with-open-file (stream "index.html"
                            :direction :output
                            :if-exists :supersede)
      (write-string "<html><head><link href=\"doc.css\" rel=\"stylesheet\" type=\"text/css\"></head><body>" stream)
      (markdown (ignore-errors (slot-value system 'asdf::description))
                :stream stream)
      (write-string "<h2>API Reference</h2>" stream)
      (write-string "<ul>" stream)
      (loop for pkg in (reverse packages)
            do (format stream "<li><a href=\"~(~A~).html\">~:*~:(~A~)</a></li>" (doc-name pkg)))
      (write-string "</ul></body></html>" stream)))
  t)
