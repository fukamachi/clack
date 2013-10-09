#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.handler.toot
  (:use :cl
        :toot
        :anaphora
        :split-sequence
        :ppcre)
  (:shadow :handle-request)
  (:import-from :clack.component
                :<component>
                :call)
  (:import-from :flexi-streams
		:octets-to-string))

(cl-syntax:use-syntax :annot)

@export
(defun run (app &key debug (port 5000))
  "Start Toot server."
  (toot:start-server
   :handler (lambda (req)
              (handle-response
               req
               (if debug (call app (handle-request req))
		   (aif (handler-case (call app (handle-request req))
			  (condition (error)
			    @ignore error
			    nil))
			it
			'(500 nil nil)))))
   :port port))


@export
(defun stop (acceptor)
  "Stop Toot server."
  (toot:stop-acceptor acceptor))

(defun handle-request (req)
  "Convert Request from server into a plist
before pass to Clack application."
  (let ((content-length (and (request-header :content-length req)
                             (parse-integer (request-header :content-length req) :junk-allowed t)))
	(port-and-host (get-port-and-host req)))
    (append
     (list
      :request-method (request-method req)
      :script-name ""
      :path-info (url-decode (request-path req))
      :server-name (car port-and-host)
      :server-port (cdr port-and-host)
      :server-protocol (server-protocol req)
      :request-uri (request-uri req)
      :url-scheme :HTTP;(request-scheme req)
      :remote-addr (remote-addr req)
      :remote-port (remote-port req)
      :query-string (request-query req)
      :content-length content-length
      :content-type (request-header :content-type req)
      :raw-body (let ((stream (toot::request-body-stream req)))
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
       (multiple-value-call #'serve-file
	 (values req body (parse-charset (getf headers :content-type)))))
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
       (toot::send-response req (with-output-to-string (s)
	  (format s "~{~A~^~%~}" body)))))))

(defun parse-charset (content-type)
  (multiple-value-bind (start end reg1 reg2)
      (ppcre:scan "(;\\s*?charset=([-_a-zA-Z0-9]+))" content-type)
    (if start
	(values (subseq content-type 0 (aref reg1 0))
		(subseq content-type (aref reg1 1) (aref reg2 1)))
	;; there is no ";charset="
	(values content-type toot::*default-charset*))))

(defun get-port-and-host (req)
  (destructuring-bind (server-name &optional (server-port "80"))
      (split-sequence #\: (cdr (assoc :host (request-headers req))))
    (cons server-name (parse-integer server-port))))

(defun url-decode (string &optional (external-format toot::*default-charset*))
  "Decodes a URL-encoded string which is assumed to be encoded using the
external format EXTERNAL-FORMAT, i.e. this is the inverse of
URL-ENCODE. It is assumed that you'll rarely need this function, if
ever. But just in case - here it is. The default for EXTERNAL-FORMAT is
the value of *default-charset*."
  (when (zerop (length string))
    (return-from url-decode ""))
  (let ((vector (make-array (length string) :element-type 'octet :fill-pointer 0))
        (i 0)
        unicodep)
    (loop
      (unless (< i (length string))
        (return))
      (let ((char (aref string i)))
       (labels ((decode-hex (length)
                  (prog1
                      (parse-integer string :start i :end (+ i length) :radix 16)
                    (incf i length)))
                (push-integer (integer)
                  (vector-push integer vector))
                (peek ()
                  (aref string i))
                (advance ()
                  (setq char (peek))
                  (incf i)))
         (cond
          ((char= #\% char)
           (advance)
           (cond
            ((char= #\u (peek))
             (unless unicodep
               (setq unicodep t)
               (upgrade-vector vector '(integer 0 65535)))
             (advance)
             (push-integer (decode-hex 4)))
            (t
             (push-integer (decode-hex 2)))))
          (t
           (push-integer (char-code (case char
                                      ((#\+) #\Space)
                                      (otherwise char))))
           (advance))))))
    (cond (unicodep
           (upgrade-vector vector 'character :converter #'code-char))
          (t (octets-to-string vector :external-format external-format)))))
