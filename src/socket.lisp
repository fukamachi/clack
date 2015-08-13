(in-package :cl-user)
(defpackage clack.socket
  (:use :cl)
  (:export :set-read-callback
           :write-sequence-to-socket
           :write-byte-to-socket
           :write-sequence-to-socket-buffer
           :write-byte-to-socket-buffer
           :flush-socket-buffer))
(in-package :clack.socket)

;; required
(defgeneric set-read-callback (socket callback))

;; required
(defgeneric write-byte-to-socket (socket byte &key callback))

;; optional. fallback to write-byte-to-socket
(defgeneric write-sequence-to-socket (socket data &key callback)
  (:method (socket data &key callback)
    (loop for byte of-type (unsigned-byte 8) across data
          do (write-byte-to-socket socket byte))
    (when callback
      (write-byte-to-socket socket
                            #.(make-array 0 :element-type '(unsigned-byte 8))
                            :callback callback))))

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
    (when callback
      (warn "Calling the callback immediately because the socket (~A) doesn't have asynchronous APIs."
            (type-of socket))
      (funcall callback))))
