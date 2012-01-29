# Clack - Web Application Environment for Common Lisp

Clack is a web application environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

    (defpackage simple-app
      (:use :cl
            :clack))
    (in-package :simple-app)
    
    (defvar *handler*
        (clackup
          #'(lambda (env)
              '(200 (:content-type "text/plain") ("Hello, Clack!")))))

Now access [http://localhost:5000/](http://localhost:5000/) and Clack may show you "Hello, Clack!".

To stop the server, call `(clack:stop *handler*)`.

## Installation

Clack is available on [Quicklisp](http://www.quicklisp.org/beta/).

    (ql:quickload :clack)

## Documentation

- [Tutorial](http://clacklisp.org/tutorial/)
- [API Reference](http://clacklisp.org/doc/)

## Middleware

Middleware is one of the Clack Component. It takes another Application and runs it.

### Bundle Middleware

* Clack.Middleware.Static - Serves static files.
* Clack.Middleware.Logger - Logging in Clack Application or Middleware.
* Clack.Middleware.Session - Session management.
* Clack.Middleware.Auth.Basic - Basic Authentication Middleware.

### Contrib Middleware

* Clack.Middleware.OAuth - Authorization by OAuth.
* Clack.Middleware.Csrf - Provides easy way to protect from CSRF.
* Clack.Middleware.Clsql - For CLSQL connection management.
* Clack.Middleware.Rucksack - For wrapping Rucksack transaction.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)
* Norihisa Fujita (n.fujita@ariel-networks.com)

## License

Licensed under the LLGPL License.
