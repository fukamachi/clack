#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Utilities for Clack Applications.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.util)

(defun merge-plist (p1 p2)
  "Merge two plist and return one plist. If both plist have same key,
p2 will be selected."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)
