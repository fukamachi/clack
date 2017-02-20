(in-package :cl-user)
(defpackage clack.socket
  (:use :cl)
  (:export :read-callback
           :close-socket
           :write-sequence-to-socket
           :write-byte-to-socket
           :write-sequence-to-socket-buffer
           :write-byte-to-socket-buffer
           :flush-socket-buffer
           :socket-async-p
           :socket-stream))
(in-package :clack.socket)

;; required
(defgeneric read-callback (socket))

;; required
(defgeneric (setf read-callback) (callback socket))

;; required
(defgeneric close-socket (socket))

;; required.
(defgeneric write-sequence-to-socket (socket data &key callback))

;; optional. fallback to write-sequence-to-socket
(defgeneric write-byte-to-socket (socket byte &key callback)
  (:method (socket byte &key callback)
    (write-sequence-to-socket socket
                              (make-array 1 :element-type '(unsigned-byte 8)
                                            :initial-contents (list byte))
                              :callback callback)))

;; optional. fallback to synchronous version
(defgeneric write-sequence-to-socket-buffer (socket data)
  (:method (socket data)
    (write-sequence-to-socket socket data)))

;; optional. fallback to synchronous version
(defgeneric write-byte-to-socket-buffer (socket byte)
  (:method (socket byte)
    (write-byte-to-socket socket byte)))

;; optional.
(defgeneric flush-socket-buffer (socket &key callback)
  (:method (socket &key callback)
    (write-sequence-to-socket socket
                              #.(make-array 0 :element-type '(unsigned-byte 8))
                              :callback callback)))

;; optional
(defgeneric socket-async-p (socket)
  (:method (socket)
    t))

;; optional
;: required if socket-async-p returns nil
(defgeneric socket-stream (socket))
