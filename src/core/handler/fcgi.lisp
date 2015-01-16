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
  (:import-from :quri
                :url-decode)
  (:import-from :alexandria
                :make-keyword
                :when-let)
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
           (let* ((env (handle-request req))
                  (res (if debug
                           (call app env)
                           (handler-case (call app env)
                             (error (error)
                               (princ error *error-output*)
                               '(500 () ("Internal Server Error")))))))
             (etypecase res
               (list (handle-response req res))
               (function (funcall res (lambda (res) (handle-response req res))))))))
    (let ((acceptor
            (make-instance '<fcgi-acceptor>
                           :port port
                           :file-descriptor fd)))
      (unwind-protect
           (cl-fastcgi::server-on-fd
            #'main-loop
            (acceptor-file-descriptor acceptor))
        (stop acceptor))
      acceptor)))

@export
(defun stop (acceptor)
  (when-let (socket (acceptor-socket acceptor))
    (usocket:socket-close socket)))

(defun handle-response (req res)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body)) res
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

      (when (eq body no-body)
        (return-from handle-response
          (lambda (body &key (close nil))
            (fcgx-puts req body)
            (if close
                (fcgx-finish req)
                (fcgx-flush req)))))

      (prog1
          (etypecase body
            (null) ;; nothing to response
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
                         (format nil "~{~A~}" body)
                         :external-format :utf-8)))
            ((vector (unsigned-byte 8))
             (fcgx-puts req body)))
        (fcgx-finish req)))))

(defun handle-request (req)
  "Convert Request from server into a plist
before passing to Clack application."
  (let ((headers (make-hash-table :test 'equal)))
    (flet ((canonicalize (field &key (start 0) (case :upcase))
             (let* ((end (length field))
                    (new (make-string (- end start))))
               (do ((i start (1+ i))
                    (j 0 (1+ j)))
                   ((= i end) new)
                 (let ((char (aref field i)))
                   (cond
                     ((char= #\_ char)
                      (setf (aref new j) #\-))
                     ((and (eq case :downcase)
                           (upper-case-p char))
                      (setf (aref new j) (code-char (+ (char-code char) 32))))
                     ((and (eq case :upcase)
                           (lower-case-p char))
                      (setf (aref new j) (code-char (- (char-code char) 32))))
                     (T
                      (setf (aref new j) char)))))))
           (set-header (k v)
             (multiple-value-bind (current existsp)
                 (gethash k headers)
               (setf (gethash k headers)
                     (if existsp
                         (format nil "~A, ~A" v current)
                         v)))))
      (loop with request-uri = nil
            for (k . v) in (fcgx-getenv req)
            if (string= k "HTTP_" :end1 5)
              do (set-header (canonicalize k :start 5 :case :downcase) v)
            if (or (string= k "SERVER_NAME")
                   (string= k "REMOTE_ADDR")
                   (string= k "CONTENT_TYPE")
                   (string= k "QUERY_STRING"))
              append (list (make-keyword (canonicalize k)) v) into env
            if (string= k "REQUEST_URI")
              append (progn
                       (setf request-uri v)
                       (list (make-keyword (canonicalize k)) v)) into env
            else
              if (or (string= k "SERVER_PORT")
                     (string= k "REMOTE_PORT")
                     (string= k "CONTENT_LENGTH"))
                append (list (make-keyword (canonicalize k))
                             (ignore-errors (parse-integer v :junk-allowed t)))
                  into env
            else
              if (or (string= k "SERVER_PROTOCOL")
                     (string= k "REQUEST_METHOD"))
                append (list (make-keyword (canonicalize k))
                             (make-keyword v))
                  into env
            else
              if (string= k "SCRIPT_NAME")
                append (list :script-name
                             (if (string= v "/")
                                 ""
                                 v))
                  into env
            finally
               (return (nconc
                        env
                        (list :headers headers
                              :path-info (let ((path-info (subseq request-uri
                                                                  0
                                                                  (position #\? request-uri
                                                                            :test #'char=))))
                                           (handler-case (quri:url-decode path-info)
                                             (quri:uri-malformed-string ()
                                               path-info)))
                              :url-scheme :http
                              :raw-body (loop with buf = (make-array 0 :fill-pointer 0 :adjustable t)
                                              for v in (cdr (fcgx-read-all req))
                                              do (adjust-array buf (+ (length buf) (length v)))
                                                 (loop for val across v
                                                       do (vector-push val buf))
                                              finally
                                                 (return
                                                   (flex:make-flexi-stream
                                                    (flex:make-in-memory-input-stream buf)
                                                    :external-format :utf-8)))
                              :clack.streaming t)))))))

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
