# Clack - Web Application Environment for Common Lisp

[![Build Status](https://travis-ci.org/fukamachi/clack.svg?branch=master)](https://travis-ci.org/fukamachi/clack)
[![Coverage Status](https://coveralls.io/repos/fukamachi/clack/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/clack)

Clack is a web application environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

```common-lisp
(defpackage simple-app
  (:use :cl
        :clack))
(in-package :simple-app)

(defvar *handler*
    (clackup
      #'(lambda (env)
          '(200 (:content-type "text/plain") ("Hello, Clack!")))))
```

Open your web browser and go to [http://localhost:5000/](http://localhost:5000/). You should get "Hello, Clack!".

To stop the server, use `(clack:stop *handler*)`.

## Installation

```common-lisp
(ql:quickload :clack)
```

## Documentation

- [Tutorial](http://clacklisp.org/tutorial/)
- [Quickdocs Page](http://quickdocs.org/clack/)

## Server

* [Hunchentoot](http://weitz.de/hunchentoot/)
* [FastCGI](http://www.fastcgi.com/)
* [Wookie](http://wookie.beeets.com/)
* [Toot](https://github.com/gigamonkey/toot)
* [Woo](https://github.com/fukamachi/woo)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2014 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/clack/graphs/contributors)

## License

Licensed under the LLGPL License.
