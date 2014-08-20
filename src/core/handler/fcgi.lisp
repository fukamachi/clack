#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.handler.fcgi
  (:use :cl
        :cl-fastcgi)
  (:import-from :clack.component
                :call)
  (:import-from :clack.http-status
                :http-status-reason)
  (:import-from :clack.util.hunchentoot
                :url-decode)
  (:import-from :alexandria
                :make-keyword
                :when-let
                :if-let)
  (:import-from :bordeaux-threads
                :make-thread)
  (:import-from :flexi-streams
                :make-flexi-stream
                :make-in-memory-input-stream
                :string-to-octets)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :usocket
                :stream-server-usocket
                :socket-listen
                :socket-close)
  (:import-from :split-sequence
                :split-sequence))
(in-package :clack.handler.fcgi)

(cl-syntax:use-syntax :annot)

(defclass <fcgi-acceptor> ()
  ((port :type integer
         :initarg :port
         :reader acceptor-port)
   (socket :type (or null 'usocket:stream-server-usocket)
           :initarg :socket
           :initform nil
           :accessor acceptor-socket)
   (file-descriptor :type (or null integer)
                    :initarg :file-descriptor
                    :initform nil
                    :accessor acceptor-file-descriptor)))

(defmethod initialize-instance :after ((acceptor <fcgi-acceptor>) &rest args)
  (declare (ignore args))
  (unless (acceptor-file-descriptor acceptor)
    (let ((socket (usocket:socket-listen "0.0.0.0"
                                         (acceptor-port acceptor)
                                         :reuse-address t
                                         :backlog 128)))
      (setf (acceptor-socket acceptor)
            socket)
      (setf (acceptor-file-descriptor acceptor)
            (cl-fastcgi::usocket-to-fd socket)))))

@export
(defun run (app &key (debug t) (port 9000) fd)
  "Start FastCGI server."
  (flet ((main-loop (req)
           (let* ((env (request->plist req))
                  (res (if debug (call app env)
                           (if-let (res (handler-case (call app env)
                                          (error (error)
                                            (princ error *error-output*)
                                            nil)))
                             res
                             '(500 nil nil)))))
             (etypecase res
               (list (handle-response req res))
               (function (funcall res (lambda (res) (handle-response req res))))))))
    (let ((acceptor
            (make-instance '<fcgi-acceptor>
                           :port port
                           :file-descriptor fd)))
      (#+thread-support bt:make-thread
       #-thread-support funcall
       #'(lambda ()
           (cl-fastcgi::server-on-fd
            #'main-loop
            (acceptor-file-descriptor acceptor))))
      acceptor)))

@export
(defun stop (acceptor)
  (when-let (socket (acceptor-socket acceptor))
    (usocket:socket-close socket)))

(defun handle-response (req res)
  (destructuring-bind (status headers &optional body) res
    (fcgx-puts req (format nil "Status: ~D ~A~%" status (http-status-reason status)))
    (loop for (k v) on headers by #'cddr
          with hash = (make-hash-table :test #'eq)
          if (gethash k hash)
            do (setf (gethash k hash)
                     (concatenate 'string (gethash k hash) ", " v))
          else
            do (setf (gethash k hash) v)
          finally
       (loop for k being the hash-keys in hash
             using (hash-value v)
             if v
               do (fcgx-puts req (format nil "~:(~A~): ~A~%" k v))))
    (fcgx-puts req #.(format nil "~%"))
    (etypecase body
      (null
       (lambda (body &key (close nil))
         (fcgx-puts req body)
         (if close
             (fcgx-finish req)
             (fcgx-flush req))))
      (pathname
       (with-open-file (in body
                           :direction :input
                           :element-type '(unsigned-byte 8)
                           :if-does-not-exist nil)
         (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
           (read-sequence buf in)
           (fcgx-puts req buf))))
      (list
       (fcgx-puts req
                  (flex:string-to-octets
                   (format nil "~{~A~^~%~}" body)
                   :external-format :utf-8)))
      ((vector (unsigned-byte 8))
       (fcgx-puts req body)))))

(defun request->plist (req)
  "Convert Request from server into a plist
before passing to Clack application."
  (let ((env
         (loop with env-hash = (make-hash-table :test #'eq)
               for (k . v) in (fcgx-getenv req)
               for key = (make-keyword
                          (with-output-to-string (out)
                            (loop for char across (string k)
                                  do (princ (if (char= #\_ char) #\- char) out))))
               if (gethash key env-hash)
                 do (setf (gethash key env-hash)
                          (concatenate 'string v ", " (gethash key env-hash)))
               else
                 do (setf (gethash key env-hash) v)
               finally (return (alexandria:hash-table-plist env-hash)))))

    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (getf env :http-host) :from-end t)
      (declare (ignore server-port))
      (setf (getf env :server-name) server-name))

    (setf (getf env :clack.streaming) t)

    (loop for key in '(:content-length
                       :server-port
                       :remote-port)
          do (setf (getf env key)
                   (parse-integer (getf env key)
                                  :junk-allowed t)))

    (loop for key in '(:server-protocol
                       :request-method)
          do (setf (getf env key)
                   (make-keyword (getf env key))))

    (when (string= (getf env :script-name) "/")
      (setf (getf env :script-name) ""))

    ;; FIXME: This handler doesn't think about HTTPS.
    (setf (getf env :url-scheme) :http)

    (setf (getf env :raw-body)
          (loop with buf = (make-array 0 :fill-pointer 0 :adjustable t)
                for v in (cdr (fcgx-read-all req))
                do (adjust-array buf (+ (length buf) (length v)))
                   (loop for val across v
                         do (vector-push val buf))
                finally
             (return
               (flex:make-flexi-stream
                (flex:make-in-memory-input-stream buf)
                :external-format :utf-8))))

    (setf (getf env :path-info)
          (clack.util.hunchentoot:url-decode
           (let ((request-uri (getf env :request-uri)))
             (subseq request-uri
                     0
                     (position #\? request-uri
                               :test #'char=)))))

    env))

(doc:start)

@doc:NAME "
Clack.Handler.Fcgi - Clack handler for FastCGI.
"

@doc:SYNOPSIS "
    ;; Start server
    (run (lambda (env)
           '(200 nil (\"ok\")))
         :port 9000)
    
    ;; Using clackup
    (clackup (lambda (env)
               '(200 nil (\"ok\")))
             :server :fcgi
             :port 9000)
"

@doc:DESCRIPTION "
Clack.Handler.Fcgi is a Clack handler to run any Clack application as a FastCGI application. This handler depends on the library \"cl-fastcgi\", so it only works on SBCL, CMUCL, GNU CLISP, Clozure CL, LispWorks and ECL.

Here's an example using Nginx.

    # nginx.conf
    worker_processes  1;
    
    events {
        worker_connections  1024;
    }
    
    http {
        server {
            listen       8080;
            server_name  localhost;
            location / {
                   include_fastcgi_params;
                   fastcgi_pass   127.0.0.1:9000;
            }
        }
    }

This configuration to opens port 8080 to web browsers, and listens for the Lisp process on port 9000.

Run Clack application by `clackup`.

    (clack:clackup (lambda (env) '(200 nil (\"Hello, FastCGI!\")))
      :server :fcgi
      :port 9000)

Now, you can access [http://localhost:8080/](http://localhost:8080/) and Clack should show you \"Hello, FastCGI!\".
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"
