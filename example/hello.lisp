(defvar app
    (lambda (req)
      (declare (ignore req))
      '(200 (:content-type "text/plain") ("Hello, Clack!"))))
