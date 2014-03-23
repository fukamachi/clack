#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2013 Javier Olaechea <pirata@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.json
  (:use :cl
        :clack)
  (:import-from :clack.util.stream
                :ensure-character-input-stream)
  (:import-from :ppcre
                :scan-to-strings)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :alexandria
                :hash-table-plist
                :make-keyword)
  (:import-from :yason :parse)
  (:shadow :finalize :expire))
(in-package :clack.middleware.json)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-json> (<middleware>)
  ())

(defun parse-charset (content-type)
  (let ((charset (aref (nth-value 1 (ppcre:scan-to-strings "^.+?/[^;]+;?(?:\\s*charset=([^; ]+)|$)"
                                                           content-type))
                       0)))
    (and charset
         (make-keyword (string-upcase charset)))))

(defmethod call ((this <clack-middleware-json>) env)
  (when (and (eql (search "application/json" (getf env :content-type) :test #'equalp)
                  0)
             (not (getf (getf env :body-parameters) :json)))
    (let ((json-octets (make-array (getf env :content-length) :element-type '(unsigned-byte 8)))
          (charset (parse-charset (getf env :content-type))))
      (read-sequence json-octets (ensure-character-input-stream
                                  (getf env :raw-body)))
      (let ((json-string (flex:octets-to-string json-octets :external-format (or charset :utf-8))))
        (unless (string= "" json-string)
          (setf (getf env :body-parameters)
                (list :json (yason:parse json-string)))))))
  (call-next this env))
