# Clack - Web Application Environment for Common Lisp

[![Build Status](https://travis-ci.org/fukamachi/clack.svg?branch=master)](https://travis-ci.org/fukamachi/clack)
[![Coverage Status](https://coveralls.io/repos/fukamachi/clack/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/clack)
[![Quicklisp dist](http://quickdocs.org/badge/clack.svg)](http://quickdocs.org/clack/)

Clack is a web application environment for Common Lisp inspired by Python's WSGI and Ruby's Rack.

## Usage

```common-lisp
(defvar *handler*
    (clack:clackup
      (lambda (env)
        (declare (ignore env))
        '(200 (:content-type "text/plain") ("Hello, Clack!")))))
```

Open your web browser and go to [http://localhost:5000/](http://localhost:5000/). You should get "Hello, Clack!".

To stop the server, use `(clack:stop *handler*)`.

## Command-line interface

Clack provides a script to start a web server. It's useful when you deploy to production environment.

NOTE: Install [Roswell](https://github.com/snmsts/roswell) before as it depends on it.

When you execute `ros install clack`, it copies `clackup` script to `$HOME/.roswell/bin`. Make sure the path is in your shell `$PATH`.

    $ ros install clack
    $ which clackup
    /Users/nitro_idiot/.roswell/bin/clackup

    $ cat <<EOF >> app.lisp
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, Clack!")))
    EOF
    $ clackup app.lisp
    Hunchentoot server is started.
    Listening on localhost:5000.

## Installation

```common-lisp
(ql:quickload :clack)
```

## Documentation

- [Quickdocs Page](http://quickdocs.org/clack/)

## Server

* [Hunchentoot](http://weitz.de/hunchentoot/)
* [FastCGI](http://www.fastcgi.com/)
* [Wookie](http://wookie.beeets.com/)
* [Toot](https://github.com/gigamonkey/toot)
* [Woo](https://github.com/fukamachi/woo)

## How to contribute

See [CONTRIBUTING.md](CONTRIBUTING.md).

## See Also

* [Lack](https://github.com/fukamachi/lack): Clack application builder

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2015 Eitaro Fukamachi & [contributors](https://github.com/fukamachi/clack/graphs/contributors)

## License

Licensed under the LLGPL License.
