#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler.apache
  (:use :cl
        :modlisp
        :split-sequence
        :anaphora)
  (:import-from :alexandria
                :make-keyword)
  (:import-from :clack.component :call)
  (:import-from :clack.util.hunchentoot
                :url-decode))

(cl-syntax:use-syntax :annot)

@export
(defun run (app &key debug (port 3000))
  "Start talking to mod_lisp process.
Note the `port` is for socket, not Apache's."
  (ml:modlisp-start :port port
                    :processor (make-request-dispatcher debug)
                    :processor-args (list app)))

@export
(defun stop (server)
  "Close socket to talk with mod_lisp."
  (ml:modlisp-stop server))

(defun make-request-dispatcher (&optional debug)
  "Return a function which calls `clack-request-dispatcher`."
  (if debug
      #'(lambda (&rest args)
          (handler-bind ((error #'invoke-debugger))
            (apply #'clack-request-dispatcher args)))
      #'(lambda (&rest args)
          (apply #'clack-request-dispatcher args))))

(defun clack-request-dispatcher (command app &key debug)
  "Apache(mod_lisp) request dispatcher for Clack. Process modlisp command alist.
This function is called on each request."
      (handler-bind ((error #'invoke-debugger))
      (handle-response (call app (command->plist command)))))

(defun command->plist (command)
  (let* ((url (ml:header-value command :url))
         (pos (position #\? url)))
    (destructuring-bind (server-name &optional (server-port "80"))
        (split-sequence #\: (ml:header-value command :host)
                        :from-end t)
      (append
       (list
        :request-method (ml:header-value command :method)
        :script-name ""
        :path-info (awhen (subseq url 0 pos)
                     (url-decode it))
        :query-string (subseq url (1+ (or pos 0)))
        :raw-body (awhen (ml:header-value command :posted-content)
                    (flex:make-flexi-stream
                     (flex:make-in-memory-input-stream
                      (flex:string-to-octets it))
                     :external-format :utf-8))
        :content-length (awhen (ml:header-value command :content-length)
                          (parse-integer it :junk-allowed t))
        :content-type (ml:header-value command :content-type)
        :server-name server-name
        :server-port (parse-integer server-port :junk-allowed t)
        :server-protocol (ml:header-value command :server-protocol)
        :request-uri url
        ;; FIXME: always return :http
        :url-scheme :http
        :remote-addr (ml:header-value command :remote-ip-addr)
        :remote-port (ml:header-value command :remote-ip-port)
        :http-server :modlisp)

       ;; NOTE: this code almost same thing of Clack.Handler.Hunchentoot's
       (loop for (k . v) in command
             unless (find k '(:request-method :script-name :path-info :server-name :server-port :server-protocol :request-uri :remote-addr :remote-port :query-string :content-length :content-type :accept :connection))
               append (list (make-keyword (format nil "HTTP-~:@(~A~)" k))
                            v))))))

(defun handle-response (res)
  "Function for managing response. Take response and output it to `ml:*modlisp-socket*'."
  (destructuring-bind (status headers body) res
    (let ((keep-alive-p (getf headers :content-length)))
      (setf (getf headers :status) (write-to-string status))
      (when keep-alive-p
        (setf (getf headers :keep-socket) "1"
              (getf headers :connection) "Keep-Alive"))

      ;; NOTE: This almost same of Clack.Handler.Hunchentoot's.
      ;; Convert plist to alist and make sure the values are strings.
      (setf headers
            (loop for (k v) on headers by #'cddr
                  with hash = (make-hash-table :test #'eq)
                  if (gethash k hash)
                    do (setf (gethash k hash)
                             (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
                  else do (setf (gethash k hash) v)
                  finally
               (return (loop for k being the hash-keys in hash
                             using (hash-value v)
                             if v
                               collect (cons k (princ-to-string v))))))

      (etypecase body
        (pathname
         (with-open-file (file body
                               :direction :input
                               :element-type '(unsigned-byte 8)
                               :if-does-not-exist nil)
           (ml::write-response (:headers headers
                                :len (princ-to-string (file-length file)))
            (loop with buf = (make-array 1024 :element-type '(unsigned-byte 8))
                  for pos = (read-sequence buf file)
                  until (zerop pos)
                  do (write-sequence buf ml:*modlisp-socket* :end pos)))))
        (list
         (ml::write-response (:headers headers)
          (write-sequence (flex:string-to-octets
                           (format nil "~{~A~^~%~}" body)
                           :external-format :utf-8)
           ml:*modlisp-socket*)))))))

(doc:start)

@doc:NAME "
Clack.Handler.Apache - Clack handler for Apache2 + mod_lisp.
"

@doc:SYNOPSIS "
    (clackup #'(lambda (env)
                 '(200
                   (:content-type \"text/plain\")
                   (\"Hello, Clack!\")))
             :server :apache
             :port 3000)
"

@doc:DESCRIPTION "
Clack.Handler.Apache is a Clack handler for Apache2 + mod_lisp. This handler depends on the library \"cl-modlisp\", so it only works on CMUCL, SBCL, AllegroCL and LispWorks.

You should make sure the Apache server and [mod_lisp](http://www.fractalconcept.com/asp/html/mod_lisp.html) are installed on your machine. The configuration of Apache might be like this.

    LoadModule lisp_module /usr/libexec/apache2/mod_lisp2.so
    ...
    Listen 12345
    NameVirtualHost *:12345
    <VirtualHost *:12345>
      ErrorLog \"/tmp/mod-lisp.log\"
      LispServer 127.0.0.1 3000 \"www\"
      <Location />
        SetHandler lisp-handler
      </Location>
    </VirtualHost>

This configuration opens port 12345 to web browsers, and listens for the Lisp process on port 3000.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
