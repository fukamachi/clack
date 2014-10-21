#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2014 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.accesslog
  (:use :cl
        :clack)
  (:import-from :log
                :category
                :config)
  (:import-from :log4cl
                :logger
                :add-appender
                :this-console-appender
                :pattern-layout)
  (:import-from :local-time
                :now
                :format-timestring))
(in-package :clack.middleware.accesslog)

(syntax:use-syntax :annot)

@export
(defvar *time-format*
  '((:day 2) #\/ :short-month #\/ (:year 4) #\: (:hour 2) #\: (:min 2) #\: (:sec 2) #\Space :gmt-offset))

@export
(defparameter *now* nil)

@export
(defvar *access-logger*
  (let ((logger (log:category '(clack middleware accesslog))))
    (log:config logger :own :trace)
    (log4cl:add-appender logger
                         (make-instance 'log4cl:this-console-appender
                                        :layout (make-instance 'log4cl:pattern-layout
                                                               :conversion-pattern "%m%n")))
    logger))

(defun content-length (res)
  (destructuring-bind (status headers body)
      res
    (declare (ignore status))
    (or (getf headers :content-length)
        (typecase body
          (list (+ (reduce #'+ body :key #'length)
                   (length body)))
          (pathname (with-open-file (in body)
                      (file-length in)))
          ((vector (unsigned-byte 8)) (1+ (length body)))))))

@export
(defclass <clack-middleware-accesslog> (<middleware>)
  ((logger :type log4cl::logger
           :initarg :logger
           :initform *access-logger*)
   (log-level :type keyword
              :initarg :log-level
              :initform :trace)
   (formatter :type function
              :initarg :formatter
              :initform (lambda (env res)
                          (format nil "~A - [~A] \"~A ~A ~A\" ~A ~A \"~:[-~;~:*~A~]\" \"~:[-~;~:*~A~]\""
                                  (getf env :remote-addr)
                                  (local-time:format-timestring nil *now* :format *time-format*)
                                  (getf env :request-method)
                                  (getf env :request-uri)
                                  (getf env :server-protocol)
                                  (car res)
                                  (content-length res)
                                  (getf env :http-referer)
                                  (getf env :http-user-agent))))))

(defmethod call ((mw <clack-middleware-accesslog>) env)
  (let ((*now* (local-time:now))
        (res (call-next mw env)))
    (eval `(,(intern (string (slot-value mw 'log-level)) :log)
            :logger ,(slot-value mw 'logger)
            ,(funcall (slot-value mw 'formatter) env res)))
    res))

(doc:start)

@doc:NAME "
Clack.Middleware.Accesslog
"

@doc:SYNOPSIS "
    (clackup (builder
              <clack-middleware-accesslog>
              (lambda (env)
                '(200 () (\"Hi.\")))))
"

@doc:DESCRIPTION "
Clack.Middleware.Accesslog provides access logging facility.
"

@doc:AUTHOR  "
Eitaro Fukamachi (e.arrows@gmail.com)
"
