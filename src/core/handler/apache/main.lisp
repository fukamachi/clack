#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack handler for Apache2 + mod_lisp.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.handler.apache)

(defun run (app &key debug (port 3000))
  (ml:modlisp-start :port port
                    :processor 'clack-request-dispatcher
                    :processor-args (list app)))

(defun clack-request-dispatcher (command app)
  "Apache(mod_lisp) request dispatcher for Clack. Process modlisp command alist.
This is called on each request."
  (handle-response (funcall app (command->plist command))))

(defun command->plist (command)
  (let* ((url (ml:header-value command :url))
         (pos (position #\? url)))
    (destructuring-bind (server-name server-port)
        (split-sequence #\: (ml:header-value command :host))
      (list
       :request-method (ml:header-value command :method)
       :script-name (ml:header-value command :script-filename)
       :path-info ""
       :request-uri (subseq url 0 pos)
       :query-string (subseq url (1+ pos))
       :server-name server-name
       :server-port server-port
       :server-protocol (ml:header-value command :server-protocol)
       :%request command))))

(defun handle-response (res)
  "Function for managing response. Take response and output it to `ml:*modlisp-socket*'."
  (destructuring-bind (status header body) res
    (setf header
          (merge-plist `(:status ,(write-to-string status)) header))
    (when (getf header :content-length)
      (setf header (merge-plist '(:keep-socket "1"
                                  :connection "Keep-Alive") header)))
    (loop :for (key val) :on header :by #'cddr
          :do (ml:write-header-line (string-capitalize key) val))
    (write-string "end" ml:*modlisp-socket*)
    (write-char #\NewLine ml:*modlisp-socket*)
    (prog1
      (cond
        ((typep body 'pathname)
         (with-open-file (file body
                               :direction :input
                               :element-type 'octet
                               :if-does-not-exist nil)
           (loop with buf = (make-array 1024 :element-type 'octet)
                 for pos = (read-sequence buf file)
                 until (zerop pos)
                 do (write-sequence buf ml:*modlisp-socket* :end pos)
                    (finish-output ml:*modlisp-socket*))))
        ((consp body)
         (dolist (s body) (write-string s ml:*modlisp-socket*)))
        (t (princ s ml:*modlisp-socket*)))
      (if (getf header :content-length)
          (force-output ml:*modlisp-socket*)
          (finish-output ml:*modlisp-socket*))
      (setf ml:*close-modlisp-socket* (not (getf header :content-length))))))
