# Clack - Web Application Environment for Common Lisp

Clack is a Web Application Environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

    (defpackage simple-app
      (:use :cl
            :clack.handler.hunchentoot))
    
    (in-package :simple-app)
    
    (defvar app
      #'(lambda (req)
          '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run app)

Now access [http://localhost:8080/](http://localhost:8080/) and Clack show you "Hello, Clack!".

## Application

Clack Application is a lambda. It takes exactly one argument, the "Request", and returns the "Response" as a list containing exactly three values.

    (defvar app
      #'(lambda (req)
          '(200 (:content-type "text/plain") ("Hello, World"))))

### The Request

Example: http://localhost:4242/sns/member?id=3

    (:request-method :GET
     :script-name ""
     :path-info "/sns/member"
     :query-string "id=3"
     :server-name "localhost"
     :server-port 4242
     :request-uri "/sns/member?id=3"
     :server-protocol :HTTP/1.1
     :http-user-agent "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US) ..."
     :http-remote-addr "127.0.0.1"
     :http-remote-port 26077
     :http-referer nil
     :http-host "localhost:4242"
     :http-cookies nil
     :http-server :hunchentoot
     :%request #<request {11A02249}>)

The Request is a list containing at least the following keys and corresponding values.

* <code>:request-method</code> (Required, Keyword): The HTTP request method, must be one of <code>:GET</code>, <code>:HEAD</code>, <code>:OPTIONS</code>, <code>:PUT</code>, <code>:POST</code>, or <code>:DELETE</code>.
* <code>:script-name</code> (Required, String): The initial portion of the request URL's path, corresponding to the application. This may be an empty string if the application corresponds to the server's root URI. If this key is not empty, it must start with a forward slash (<code>/</code>).
* <code>:path-info</code> (Required, String): The remainder of the request URL's path. This may be an empty string if the request URL targets the application root and does no have a trailing slash.
* <code>:query-string</code> (Optional, String): The portion of the request URL that follows the <code>?</code>, if any. This key may be empty, but must always be present, even if empty.
* <code>:server-name</code> (Required, String): The resolved server name, or the server IP address.
* <code>:server-port</code> (Required, Integer): The port on which the request is being handled.
* <code>:request-uri</code> (Required, String): The request URI. Must start with "/".
* <code>:server-protocol</code> (Required, Keyword)
* <code>:http-server</code> (Required, Keyword): The name of Clack Handler, such as <code>:hunchentoot</code>.
* Other <code>:http-*</code> keys: These keys correspond to the client-supplied HTTP request headers. The presence or absence of these keys should correspond to the presence or absence of the appropriate HTTP header in the request.

### The Response

Example:

    (200
     (:content-type "text/html")
     ("<b>Hello, Lispers!</b>"))

Applications must return a response as a list containing three values.

* Status (Required, Integer): An HTTP status code. This must be an integer greater than or equal to 100, and should be an HTTP status code as documented in RFC 2616.
* Headers (Required, Property List): An HTTP headers. This must be a property list of key/value pairs.
* Body (Required, List or Pathname): The response body. This is either a list or a pathname. If it is a list of strings, Handler should output it with #\NewLine for each elements. The body can instead be a pathname for serving static files.

## Handler

Clack Applications run via Clack Handlers, which are in turn responsible for implementing the HTTP protocol and abstracting the server.

Now Clack Applications works on Hunchentoot and Apache, by using following handler.

* clack.handler.hunchentoot
* clack.handler.apache

If you hope them to run on other server (such as tpd2), you can write a handler for it easily.

## Middleware

Middleware is one of the Clack Component. It takes another Application and runs it.

### Bundle Middleware

#### clack.middleware.static

    (defpackage simple-app
      (:use :cl
            :clack.builder
            :clack.handler.hunchentoot
            :clack.middleware.static))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run
     (builder
      (<clack-middleware-static>
       :path "/static"
       :root #p"/public/")
      app))

### How to write Middleware?

All you have to do is to inherit from clack:<middleware> and then implement the callback <code>call</code> method (or <code>make-app</code> method that would return a function) to do the actual work. You can use <code>call-next</code> to call the original (wrapped) application.

    (defpackage clack.middleware.example
      (:use :cl :clack.middleware)
      (:export :<simple-middleware>))
    
    (in-package :clack.middleware.example)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((this <simple-middleware>) req)
      `(200 (:content-type "text/html")
        ,(cons "Hello, Clack Middleware!<br />"
               (nth 2 (call-next this req)))))

    (defpackage simple-app
      (:use :cl
            :clack.builder
            :clack.handler.hunchentoot
            :clack.middleware.example))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run (builder <simple-middleware> app))

And you should get following response in time.

    Hello, Clack Middleware!
    Hello, Clack!

## Dependency

* CL-PPCRE
* CL-FAD
* Alexandria
* SPLIT-SEQUENCE
* Anaphora
* metabang-bind
* Hunchentoot
* modlisp

## Author

* Eitarow Fukamachi

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## Contributors

* Tomohiro Matsuyama

## License

Licensed under the LLGPL License.
