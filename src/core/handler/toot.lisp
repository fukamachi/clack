#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler.toot
  (:use :cl
        :toot
        :split-sequence
        :ppcre)
  (:shadow :handle-request)
  (:import-from :clack.component
                :<component>
                :call))

(cl-syntax:use-syntax :annot)

@export
(defun run (app &key (port 5000) debug)
  "Start Toot server."
  (declare (ignore debug))
  (toot:start-server
   :handler (lambda (req)
              (handle-response
               req
               (call app (handle-request req))))
   :port port))

@export
(defun stop (acceptor)
  "Stop Toot server."
  (toot:stop-acceptor acceptor))

(defun handle-request (req)
  "Convert Request from server into a plist
before pass to Clack application."
  (let ((content-length (and (content-length req)
                             (parse-integer (content-length req) :junk-allowed t))))
    (append
     (list
      :request-method (request-method req)
      :script-name ""
      :path-info (request-path req)
      :server-name (request-host req)
      :server-port (request-port req)
      :server-protocol (server-protocol req)
      :request-uri (request-uri req)
      :url-scheme (request-scheme req)
      :remote-addr (remote-addr req)
      :remote-port (remote-port req)
      :query-string (request-query req)
      :content-length content-length
      :raw-body (let ((stream (flex:make-flexi-stream (toot::content-stream req)
                               :external-format toot::+latin-1+)))
                  ;(when content-length
                    ;(setf (flex:flexi-stream-bound stream) content-length))
                  stream)
      :clack.uploads nil
      :clack.handler :toot)

     (loop for (k . v) in (toot::request-headers req)
           unless (find k '(:request-method :script-name :path-info :server-name :server-port :server-protocol :request-uri :remote-addr :remote-port :query-string :content-length :content-type :accept :connection))
             append (list (intern (format nil "HTTP-~:@(~A~)" k) :keyword)
                          v)))))

(defun handle-response (req res)
  (destructuring-bind (status headers body) res
    (etypecase body
      (pathname
       (serve-file req body
                   :content-type (getf headers :content-type)))
      (list
       ;; XXX: almost same as Clack.Handler.Hunchentoot's one.
       (setf (status-code req) status)
       (loop for (k v) on headers by #'cddr
             with hash = (make-hash-table :test #'eq)
             if (gethash k hash)
               do (setf (gethash k hash)
                        (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
             else if (eq k :content-type)
               do (multiple-value-bind (v charset)
                      (parse-charset v)
                    (setf (gethash k hash) v)
                    (setf (toot::response-charset req) charset))
             else do (setf (gethash k hash) v)
             finally
          (loop for k being the hash-keys in hash
                using (hash-value v)
                do (setf (response-header k req) v)))
       (with-output-to-string (s)
         (format s "~{~A~^~%~}" body))))))

(defun parse-charset (content-type)
  (multiple-value-bind (start end reg1 reg2)
      (ppcre:scan "(;\\s*?charset=([-_a-zA-Z0-9]+))" content-type)
    (values (subseq content-type 0 (aref reg1 0))
            (subseq content-type (aref reg1 1) (aref reg2 1)))))
