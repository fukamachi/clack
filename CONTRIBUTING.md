# Contributing to Clack

Clack is an open source project and we appreciate your help!

## Reporting bugs

Please [open an issue](https://github.com/fukamachi/clack/issues/new) at GitHub.

Good reports must include these informations:

- The full backtrace with your error
- Minimum steps to reproduce it
- Names and versions you are using: OS, Common Lisp implementation, ASDF and Quicklisp dist

You can get informations about your environment by this code:

```common-lisp
(flet ((put (k v &rest vs)
         (format t "~&~A: ~A~{ ~A~}~%" k v vs)))
  (put "Machine" (software-type) (software-version))
  (put "Lisp" (lisp-implementation-type) (lisp-implementation-version)
       #+(and sbcl (not sb-thread)) "(without threads)")
  (put "ASDF" (asdf:asdf-version))
  (let ((qlversion (ql:dist-version "quicklisp")))
    (put "Quicklisp" qlversion
         (if (string= (car (first (ql:available-dist-versions "quicklisp")))
                      qlversion)
             "(latest)"
             "(update available)"))))
;-> Machine: Darwin 15.2.0
;   Lisp: SBCL 1.3.1
;   ASDF: 3.1.5
;   Quicklisp: 2015-10-31 (latest)
;=> NIL
```
