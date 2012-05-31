#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler.fcgi
  (:use :cl
        :cl-fastcgi
        :anaphora)
  (:import-from :clack.component
                :call)
  (:import-from :clack.util.hunchentoot
                :url-decode)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :bordeaux-threads
                :make-thread)
  (:import-from :flexi-streams
                :make-flexi-stream
                :make-in-memory-input-stream
                :string-to-octets)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :usocket
                :socket-listen
                :socket-close))

(cl-syntax:use-syntax :annot)

@export
(defun run (app &key (debug t) (port 9000))
  "Start FastCGI server."
  (let ((sock (usocket:socket-listen "0.0.0.0" port
               :reuse-address t
               :backlog 128)))
    (prog1 sock
           (bordeaux-threads:make-thread
            (lambda ()
              (cl-fastcgi::server-on-fd
               #'(lambda (req)
                   (let* ((env (request->plist req))
                          (res (if debug (call app env)
                                   (aif (handler-case (call app env)
                                          (condition (error)
                                            (princ error *error-output*)
                                            nil))
                                        it
                                        '(500 nil nil)))))
                     (etypecase res
                       (list (handle-response req res))
                       (function (funcall res (lambda (res) (handle-response req res)))))))
               (cl-fastcgi::usocket-to-fd sock)))))))

@export
(defun stop (acceptor)
  (usocket:socket-close acceptor))

(defun handle-response (req res)
  (destructuring-bind (status headers &optional body) res
    (fcgx-puts req (format nil "Status: ~D ~A~%" status (gethash status *http-status*)))
    (loop for (k v) on headers by #'cddr
          with hash = (make-hash-table :test #'eq)
          do (setf (gethash k hash)
                   (aif (gethash k hash)
                        (concatenate 'string it ", " v)
                        v))
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
             ;; XXX: cl-fastcgi doesn't provide `fcgx-flush' now.
             (cffi:foreign-funcall "FCGX_FFlush"
              :pointer (cffi:foreign-slot-value req 'cl-fastcgi::fcgx-request 'cl-fastcgi::out)
              :int))))
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
                   :external-format :utf-8))))))

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
               do (setf (gethash key env-hash)
                        (aif (gethash key env-hash)
                             (concatenate 'string it ", " v)
                             v))
               finally (return (alexandria:hash-table-plist env-hash)))))

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
                     (position #\? (getf env :request-uri)
                               :test #'char=)))))

    env))

;; TODO: these code should be a part of Clack core package.
(defvar *http-status* (make-hash-table :test #'eql))
(setf (gethash 100 *http-status*) "Continue")
(setf (gethash 101 *http-status*) "Switching Protocols")
(setf (gethash 200 *http-status*) "OK")
(setf (gethash 201 *http-status*) "Created")
(setf (gethash 202 *http-status*) "Accepted")
(setf (gethash 203 *http-status*) "Non-Authoritative Information")
(setf (gethash 205 *http-status*) "Reset Content")
(setf (gethash 206 *http-status*) "Partial Content")
(setf (gethash 207 *http-status*) "Multi-Status")
(setf (gethash 300 *http-status*) "Multiple Choices")
(setf (gethash 301 *http-status*) "Moved Permanently")
(setf (gethash 302 *http-status*) "Moved Temporarily")
(setf (gethash 303 *http-status*) "See Other")
(setf (gethash 304 *http-status*) "Not Modified")
(setf (gethash 305 *http-status*) "Use Proxy")
(setf (gethash 307 *http-status*) "Temporary Redirect")
(setf (gethash 400 *http-status*) "Bad Request")
(setf (gethash 401 *http-status*) "Authorization Required")
(setf (gethash 402 *http-status*) "Payment Required")
(setf (gethash 403 *http-status*) "Forbidden")
(setf (gethash 404 *http-status*) "Not Found")
(setf (gethash 405 *http-status*) "Method Not Allowed")
(setf (gethash 406 *http-status*) "Not Acceptable")
(setf (gethash 407 *http-status*) "Proxy Authentication Required")
(setf (gethash 409 *http-status*) "Conflict")
(setf (gethash 410 *http-status*) "Gone")
(setf (gethash 411 *http-status*) "Length Required")
(setf (gethash 412 *http-status*) "Precondition Failed")
(setf (gethash 413 *http-status*) "Request Entity Too Large")
(setf (gethash 414 *http-status*) "Request-URI Too Large")
(setf (gethash 415 *http-status*) "Unsupported Media Type")
(setf (gethash 416 *http-status*) "Requested range not satisfiable")
(setf (gethash 424 *http-status*) "Failed Dependency")
(setf (gethash 500 *http-status*) "Internal Server Error")
(setf (gethash 501 *http-status*) "Not Implemented")
(setf (gethash 502 *http-status*) "Bad Gateway")
(setf (gethash 503 *http-status*) "Service Unavailable")
(setf (gethash 504 *http-status*) "Gateway Time-out")
(setf (gethash 505 *http-status*) "Version not supported")

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
Eitarow Fukamachi (e.arrows@gmail.com)
"
