# CL-Cookie

[![Build Status](https://travis-ci.org/fukamachi/cl-cookie.svg?branch=master)](https://travis-ci.org/fukamachi/cl-cookie)

HTTP cookie manager for Common Lisp.

Key features include:

- Create `cookie` struct instances with `make-cookie` and the following properties: `name`, `value`, `path`, `domain`, `origin-host`, `expires`, `max-age`, `same-site`, `partitioned`, `secure-p`, `httponly-p`. The `creation-timestamp` attribute is set automatically at creation time.
- Compare cookies with `cookie=` and `cookie-equal`.
- Convert `Set-Cookie` header string into `cookie` instance with `parse-set-cookie-headers`.
- Serialize `cookie` instances into header strings with `write-cookie-header` and `write-set-cookie-header`.
- Create `cookie-jar` struct instances with `make-cookie-jar` which allows managing of multiple cookies. You can add multiple cookies to a cookie-jar with `merge-cookies`, or match cookies of certain `host` and `path` with `cookie-jar-host-cookies`.

## Usage

### make-cookie (&key name value path domain origin-host expires max-age same-site partitioned secure-p httponly-p)
```common-lisp
(defparameter *cookie*
  (cl-cookie:make-cookie
    :name "SID"
    :value "31d4d96e407aad42"
    :origin-host "example.com"
    :path "/api/"
    :domain ".example.com"
    :partitioned t
    :httponly-p t
    :secure-p t
    :expires (encode-universal-time 6 22 19 25 1 2002 0)
    :max-age 3600 ;; max-age takes precedence over expires
    :same-site "Strict"))
;=> *COOKIE*

*cookie*
;=> #S(COOKIE :NAME "SID" :VALUE "31d4d96e407aad42" :EXPIRES 3220975326 :PATH "/api/"
;             :DOMAIN ".example.com" :SAME-SITE "Strict" :MAX-AGE 3600 :PARTITIONED T
;	      :SECURE-P T :HTTPONLY-P T :ORIGIN-HOST "example.com" :CREATION-TIMESTAMP 3921465733)
```

### write-set-cookie-header (cookie &optional stream)

```common-lisp
(cl-cookie:write-set-cookie-header *cookie*)
;=> "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Max-age=3600;
     Path=/api/; Domain=.example.com; SameSite=Strict; Partitioned; Secure; HttpOnly"
```

### cookie struct accessor functions

```common-lisp
(cl-cookie:cookie-name *cookie*)
;=> "SID"

(cl-cookie:cookie-value *cookie*)
;=> "31d4d96e407aad42"

(cl-cookie:cookie-expires *cookie*)
;=> 3220975326

(cl-cookie:cookie-path *cookie*)
;=> "/api/"

(cl-cookie:cookie-origin-host *cookie*)
;=> "example.com"
;; host takes precedence over domain, if both are supplied and conflicting.

(cl-cookie:cookie-domain *cookie*)
;=> ".example.com"
;; This cookie is accessible for example.com and all subdomains (e.g. docs.example.com)

(cl-cookie:cookie-same-site *cookie*)
;=> "Strict"

(cl-cookie:cookie-max-age *cookie*)
;=> 3600

(cl-cookie:cookie-partitioned *cookie*)
;=> T

(cl-cookie:cookie-secure-p *cookie*)
;=> T

(cl-cookie:cookie-httponly-p *cookie*)
;=> T
```

### parse-set-cookie-header (set-cookie-string origin-host origin-path)

```common-lisp
(cl-cookie:parse-set-cookie-header
	 "my_cookie=my_value; Domain=.example.com; Expires=Sat, 31 Dec 2024 23:59:59 GMT; HttpOnly; Max-Age=7200; Secure; SameSite=None"
	 "example.com"
	 "/")
;=> #S(COOKIE :NAME "my_cookie" :VALUE "my_value" :EXPIRES 3944678399 :PATH "/" :DOMAIN ".example.com"
;             :SAME-SITE "None" :MAX-AGE 7200 :PARTITIONED NIL :SECURE-P T
;             :HTTPONLY-P T :ORIGIN-HOST "example.com" :CREATION-TIMESTAMP 3921467559)
```

### make-cookie-jar (&key cookies)
```common-lisp
(defparameter *cookie-jar*
  (cl-cookie:make-cookie-jar
   :cookies (list (cl-cookie:make-cookie
	     :name "SID"
	     :value "31d4d96e407aad42"
	     :origin-host "example.com"
	     :path "/api/"
	     :domain ".example.com"
	     :max-age 3600))))
;=> *cookie-jar*
```

### merge-cookies (cookie-jar cookies)
```common-lisp
(cl-cookie:merge-cookies
 *cookie-jar*
 (list (cl-cookie:parse-set-cookie-header
	"SID=31d4d96e407aad42; Path=/; Domain=example.com; Partitioned"
	"www.example.org"
	"/")))
;=> (#S(CL-COOKIE:COOKIE :NAME "SID" :VALUE "31d4d96e407aad42" :PATH "/api/" :DOMAIN ".example.com"
;                        :ORIGIN-HOST "example.com" :EXPIRES NIL :MAX-AGE 3600 :SAME-SITE NIL
;                        :PARTITIONED NIL :SECURE-P NIL :HTTPONLY-P NIL :CREATION-TIMESTAMP 3921572023)
;    #S(CL-COOKIE:COOKIE :NAME "SID" :VALUE "31d4d96e407aad42" :PATH "/" :DOMAIN "example.com"
;                        :ORIGIN-HOST "www.example.org" :EXPIRES NIL :MAX-AGE NIL :SAME-SITE NIL
;                        :PARTITIONED T :SECURE-P NIL :HTTPONLY-P NIL :CREATION-TIMESTAMP 3921572029))
```

### cookie-jar-host-cookies (cookie-jar host path &key securep)
```common-lisp
(cl-cookie:cookie-jar-host-cookies *cookie-jar*
				   "example.com"
				   "/")
;=> (#S(CL-COOKIE:COOKIE :NAME "SID" :VALUE "31d4d96e407aad42" :PATH "/" :DOMAIN "example.com"
;                        :ORIGIN-HOST "www.example.org" :EXPIRES NIL :MAX-AGE NIL :SAME-SITE NIL
;                        :PARTITIONED NIL :SECURE-P NIL :HTTPONLY-P NIL :CREATION-TIMESTAMP 3921566005))
```

## See also

- [RFC 6265](http://tools.ietf.org/html/rfc6265)
- [Set-Cookie](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
