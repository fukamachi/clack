# Clack - Web Application Environment for Common Lisp

Clack is a Web Application Environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

    (defpackage simple-app
      (:use :cl
            :clack))
    (in-package :simple-app)
    
    (clackup
      #'(lambda (req)
          '(200 (:content-type "text/plain") ("Hello, Clack!"))))

Now access [http://localhost:5000/](http://localhost:5000/) and Clack may show you "Hello, Clack!".

## Installation

Though I want to keep the dependencies of Clack minimum, Clack depends on [cl-annot](https://github.com/arielnetworks/cl-annot) heavily.

It is a really cool product written by [Tomohiro Matsuyama](http://twitter.com/#!/m2ym), my friend. So I recommend you to install it anyway.

Clack and cl-annot isn't in Quicklisp's repository yet ([now on request](https://github.com/quicklisp/quicklisp-projects/issues/120)). Then you have to do following steps.

### 1. Config ASDF

    $ mkdir ~/lisp/systems
    $ vi ~/.config/common-lisp/source-registry.conf.d/01-add-local-lisp.conf

And write the following code.

    (:tree (:home "lisp/systems/"))

Now, ASDF can find .asd from `~/lisp/systems/` or it's sub directories.

### 2. Clone

Download Clack and cl-annot from their repository and make sure they are in `~/lisp/systems/`.

    $ cd ~/lisp/systems
    $ git clone git://github.com/arielnetworks/cl-annot
    $ git clone git://github.com/fukamachi/clack

### 3. Load

In the end, you can load Clack with [Quicklisp](https://www.quicklisp.org/beta/).

    (ql:quickload :clack)

## Application

Clack Application is just a lambda. It takes exactly one argument, the "Request", and returns the "Response" as a list containing exactly three values.

    (defvar app
      #'(lambda (req)
          '(200 (:content-type "text/plain") ("Hello, World"))))

### Clack.App.Route

Clack is not a Web Application Framework. But Clack can also be used as such way.

Clack bundles "Clack.App.Route", written by [Tomohiro Matsuyama](http://twitter.com/#!/m2ym). It allows you to write an URL-based dispatcher, like Ruby's Sinatra.

    (defroutes app (req)
      (GET \"/\" #'index)
      (GET \"/login\" #'login)
      (POST \"/login\" #'authorize)
      (GET \"/member/:id\" #'member))

    (clackup #'app)

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
* <code>:server-protocol</code> (Required, Keyword): The version of the protocol the client used to send the request. Typically this will be something like <code>:HTTP/1.0</code> or <code>:HTTP/1.1</code>.
* <code>:request-uri</code> (Required, String): The request URI. Must start with "/".
* <code>:server-protocol</code> (Required, Keyword)
* <code>:raw-body</code> (Optional, Stream)
* <code>:http-user-agent</code> (Optional, String)
* <code>:http-referer</code> (Optional, String)
* <code>:remote-addr</code> (Required, String)
* <code>:remote-port</code> (Required, Integer)

* <code>:http-server</code> (Required, Keyword): The name of Clack Handler, such as <code>:hunchentoot</code>.

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

* Clack.Handler.Hunchentoot
* Clack.Handler.Apache

If you hope them to run on other server (such as AllegroServe or teepeedee2), you can write a handler for it easily.

## Middleware

Middleware is one of the Clack Component. It takes another Application and runs it.

### Bundle Middleware

* Clack.Middleware.Static - Serves static files.
* Clack.Middleware.Logger - Logging in Clack Application or Middleware.
* Clack.Middleware.Session - Session management.
* Clack.Middleware.OAuth - Authorization by OAuth.

### How to write Middleware?

All you have to do is to inherit from <code>&lt;middleware&gt;</code> and then implement the callback <code>call</code> method (or <code>make-app</code> method that would return a function) to do the actual work. You can use <code>call-next</code> to call the original (wrapped) application.

    (defpackage clack.middleware.example
      (:use :cl :clack)
      (:export :<simple-middleware>))
    (in-package :clack.middleware.example)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((this <simple-middleware>) req)
      `(200 (:content-type "text/html")
        ,(cons "Hello, Clack Middleware!<br />"
               (nth 2 (call-next this req)))))

    (defpackage simple-app
      (:use :cl
            :clack
            :clack.builder
            :clack.middleware.example))
    (in-package :simple-app)
    
    (defvar app
      #'(lambda (req)
          '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (clackup (builder <simple-middleware> app))

And you should get following response in time.

    Hello, Clack Middleware!
    Hello, Clack!

## Dependency

* CL-ANNOT
* CL-PPCRE
* CL-FAD
* Alexandria
* SPLIT-SEQUENCE
* Anaphora
* metabang-bind
* Hunchentoot
* modlisp
* cl-oauth

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)
* Norihisa Fujita (n.fujita@ariel-networks.com)

## License

Licensed under the LLGPL License.
