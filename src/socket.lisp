(in-package :cl-user)
(defpackage clack.socket
  (:use :cl)
  (:export :set-read-callback
           :write-to-socket))
(in-package :clack.socket)

(defgeneric set-read-callback (socket callback))

(defgeneric write-to-socket (socket message &key callback))
