(in-package :cl-user)
(defpackage clack.socket
  (:use :cl)
  (:export :set-read-callback
           :write-to-socket
           :write-to-socket-buffer
           :flush-socket-buffer))
(in-package :clack.socket)

(defgeneric set-read-callback (socket callback))

(defgeneric write-to-socket (socket message &key callback))

(defgeneric write-to-socket-buffer (socket message)
  (:method (socket message)
    (write-to-socket socket message)))

(defgeneric flush-socket-buffer (socket &key callback)
  (:method (socket &key callback)
    (declare (ignore socket callback))
    nil))
