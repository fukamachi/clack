#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler.apache
  (:use :cl
        :modlisp
        :metabang-bind
        :split-sequence)
  (:import-from :clack.component :call)
  (:import-from :alexandria :plist-alist))

(cl-annot:enable-annot-syntax)

@export
(defun run (app &key debug (port 3000))
  "Start talking to mod_lisp process."
  @ignore debug
  (ml:modlisp-start :port port
                    :processor 'clack-request-dispatcher
                    :processor-args (list app)))

@export
(defun stop (server)
  "Close socket to talk with mod_lisp.
If no server given, try to stop `*server*' by default."
  (ml:modlisp-stop server))

(defun clack-request-dispatcher (command app)
  "Apache(mod_lisp) request dispatcher for Clack. Process modlisp command alist.
This is called on each request."
  (handler-bind ((error #'invoke-debugger))
    (handle-response (call app (command->plist command)))))

(defun command->plist (command)
  (bind ((url (ml:header-value command :url))
         (pos (position #\? url))
         ((server-name server-port)
          (split-sequence #\: (ml:header-value command :host))))
    (list
     :request-method (ml:header-value command :method)
     :script-name ""
     :path-info (subseq url 0 pos)
     :query-string (ml:header-value command :url-params)
     :raw-body (ml:header-value command :posted-content)
     :server-name server-name
     :server-port (parse-integer server-port :junk-allowed t)
     :server-protocol (ml:header-value command :server-protocol)
     :request-uri url
     :remote-addr (ml:header-value command :remote-ip-addr)
     :remote-port (ml:header-value command :remote-ip-port)
     :http-user-agent (ml:header-value command :user-agent)
     :http-referer (ml:header-value command :referer)
     :http-host (ml:header-value command :host)
     ;; NOTE: :cookie returns string.
     :http-cookies (ml:header-value command :cookie)
     :http-server :modlisp
     :%request command)))

(defun handle-response (res)
  "Function for managing response. Take response and output it to `ml:*modlisp-socket*'."
  (bind (((status headers body) res)
         (keep-alive-p (getf headers :content-length)))
    (setf (getf headers :status) (write-to-string status))
    (when keep-alive-p
      (setf (getf headers :keep-socket) "1"
            (getf headers :connection) "Keep-Alive"))

    (setf headers (plist-alist headers))

    (etypecase body
      (pathname
       (with-open-file (file body
                             :direction :input
                             :element-type '(unsigned-byte 8)
                             :if-does-not-exist nil)
         (ml::write-response (:headers headers
                              :len (format nil "~A" (file-length file)))
          (loop with buf = (make-array 1024 :element-type '(unsigned-byte 8))
                for pos = (read-sequence buf file)
                until (zerop pos)
                do (write-sequence buf ml:*modlisp-socket* :end pos)))))
      (list
       (ml::write-response (:headers headers)
        (format ml:*modlisp-socket* "~{~A~^~%~}" body))))))

(doc:start)

@doc:NAME "
Clack.Handler.Apache - Clack handler for Apache2 + mod_lisp.
"

@doc:DESCRIPTION "
Clack.Handler.Apache is a Clack handler for Apache2 + mod_lisp.

This is not maintained well. Sorry.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
