(in-package :cl-user)
(defpackage clack.util
  (:use :cl)
  (:import-from :lack.util
                :find-package-or-load)
  (:export :find-handler))
(in-package :clack.util)

(defun find-handler (server)
  (flet ((find-with-prefix (prefix)
           (find-package-or-load (concatenate 'string
                                              prefix
                                              (symbol-name server)))))
    (or (find-with-prefix #.(string '#:clack.handler.))
        (error "~S is unknown handler."
               server))))
