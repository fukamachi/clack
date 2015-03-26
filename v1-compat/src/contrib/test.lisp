(in-package :clack.test)

(defun test-app (app client &optional desc)
  `(subtest-app ,desc ,app (funcall ,client)))
(export 'test-app)
