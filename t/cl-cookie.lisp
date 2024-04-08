(in-package :cl-user)
(defpackage cl-cookie-test
  (:use :cl
        :cl-cookie
        :prove)
  (:import-from :cl-cookie
		:expired-cookie-p
                :parse-cookie-date
                :match-cookie-path
                :match-cookie))
(in-package :cl-cookie-test)

(plan nil)

(subtest "parse-cookie-date"
  (loop for (date . rfc3339) in '(("Wed, 06-Feb-2008 21:01:38 GMT" . "2008-02-06T21:01:38+0000")
                                  ("Wed, 06-Feb-08 21:01:38 GMT"   . "2008-02-06T21:01:38+0000")
                                  ("Tue Feb 13 08:00:00 2007 GMT"  . "2007-02-13T08:00:00+0000")
                                  ("Wednesday, 07-February-2027 08:55:23 GMT" . "2027-02-07T08:55:23+0000")
                                  ("Wed, 07-02-2017 10:34:45 GMT"  . "2017-02-07T10:34:45+0000"))
        do (let ((parsed (parse-cookie-date date)))
             (ok parsed (format nil "Can parse ~S" date))
             (is (local-time:universal-to-timestamp parsed)
                 (local-time:parse-timestring rfc3339)
                 :test #'local-time:timestamp=))))

(subtest "parse-set-cookie-header"
  (is (parse-set-cookie-header "SID=31d4d96e407aad42" "example.com" "/")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/")
      :test #'cookie=
      "name and value")
  (is (parse-set-cookie-header "SID=" "example.com" "/")
      (make-cookie :name "SID" :value "" :origin-host "example.com" :path "/")
      :test #'cookie=
      "no value")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; Domain=example.com" "example.com" "/")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :domain "example.com")
      :test #'cookie=
      "path and domain")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Max-age=199999; Path=/"
			       "example.com" "/")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :max-age 199999 :expires (encode-universal-time 6 22 19 25 1 2002 0))
      :test #'cookie-equal
      "expires and max-age")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Max-age=199999; Path=/; SameSite=Lax; Partitioned"
			       "example.com" "/")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :partitioned t :max-age 199999 :same-site "Lax")
      :test #'cookie-equal
      "partitioned max-age and same-site")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; Secure; HttpOnly" "example.com" "/")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :origin-host "example.com" :path "/" :secure-p t :httponly-p t)
      :test #'cookie-equal
      "secure and httponly"))

(subtest "write-cookie-header"
  (is (write-cookie-header nil)
      nil)
  (is (write-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42"))
      "SID=31d4d96e407aad42")
  (is (write-cookie-header (list (make-cookie :name "SID" :value "31d4d96e407aad42")
                                 (make-cookie :name "lang" :value "en-US")))
      "SID=31d4d96e407aad42; lang=en-US"))

(subtest "match-cookie-path"
  (ok (match-cookie-path "/" "/"))
  (ok (match-cookie-path "/" ""))
  (ok (match-cookie-path "" "/"))
  (ok (not (match-cookie-path "/" "/accounts")))
  (ok (match-cookie-path "/accounts" "/"))
  (ok (match-cookie-path "/accounts/nitro_idiot" "/"))
  (ok (not (match-cookie-path "/" "/accounts")))
  (ok (match-cookie-path "/accounts" "/accounts"))
  (ok (match-cookie-path "/accounts/" "/accounts"))
  (ok (not (match-cookie-path "/accounts-page" "/accounts")))
  (ok (match-cookie-path "/accounts/nitro_idiot" "/accounts")))

(subtest "match-cookie"
  (subtest "cookie with domain and path"
    (let ((cookie
            (make-cookie :name "LSID" :value "DQAAAK...Eaem_vYg" :origin-host "docs.foo.com"
                         :domain ".foo.com" :path "/accounts")))
      (diag "path")
      (ok (not (match-cookie cookie "docs.foo.com" "/")))
      (ok (match-cookie cookie "docs.foo.com" "/accounts"))
      (ok (match-cookie cookie "docs.foo.com" "/accounts/"))
      (ok (match-cookie cookie "docs.foo.com" "/accounts/nitro_idiot"))
      (ok (not (match-cookie cookie "docs.foo.com" "/accounts-page" :securep t)))

      (diag "domain")
      (ok (not (match-cookie cookie "foo.com" "/" :securep t))
          "Send only to the origin-host when :host is NIL")
      (ok (not (match-cookie cookie "one.docs.foo.com" "/" :securep t))
          "Send only to the origin-host when :host is NIL")))

  (subtest "cookie with path"
    (let ((cookie
            (make-cookie :name "LSID" :value "DQAAAK...Eaem_vYg" :origin-host "docs.foo.com"
                         :path "/accounts" :secure-p t :httponly-p t)))
      (diag "secure")
      (ok (not (match-cookie cookie "docs.foo.com" "/accounts")))
      (ok (match-cookie cookie "docs.foo.com" "/accounts" :securep t))

      (diag "path")
      (ok (not (match-cookie cookie "docs.foo.com" "/" :securep t)))
      (ok (match-cookie cookie "docs.foo.com" "/accounts" :securep t))
      (ok (match-cookie cookie "docs.foo.com" "/accounts/" :securep t))
      (ok (match-cookie cookie "docs.foo.com" "/accounts/nitro_idiot" :securep t))
      (ok (not (match-cookie cookie "docs.foo.com" "/accounts-page" :securep t)))

      (diag "domain")
      (ok (not (match-cookie cookie "foo.com" "/" :securep t))
          "Send only to the origin-host when :host is NIL")
      (ok (not (match-cookie cookie "one.docs.foo.com" "/" :securep t))
          "Send only to the origin-host when :host is NIL"))))

(subtest "cookie-jar"
  (let ((cookie-jar (make-cookie-jar)))
    (is (length (cookie-jar-cookies cookie-jar)) 0
        "initial cookie jar is empty")
    (merge-cookies cookie-jar
                   (list (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "example.com" :path "/")
                         (make-cookie :name "lang" :value "en-US" :domain "example.com" :path "/accounts")))
    (is (length (cookie-jar-cookies cookie-jar)) 2)
    (merge-cookies cookie-jar
                   (list (make-cookie :name "id" :value "30" :domain "example.com")))
    (is (length (cookie-jar-cookies cookie-jar)) 3)
    (merge-cookies cookie-jar
                   (list (make-cookie :name "lang" :value "ja-JP" :domain "example.com" :path "/accounts")))

    (subtest "can overwrite"
      (is (length (cookie-jar-cookies cookie-jar)) 3)
      (is (cookie-value
           (find "lang" (cookie-jar-cookies cookie-jar) :key #'cookie-name :test #'string=))
          "ja-JP"))

    (subtest "not overwrite other domain cookies"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "lang" :value "fr-FR" :domain "www.example.com")))
      (is (length (cookie-jar-cookies cookie-jar)) 4))

    (subtest "Cross site cooking"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "name" :value "Ultraman" :domain ".com")))
      (is (cookie-jar-host-cookies cookie-jar "hatena.com" "/") nil))))

(subtest "write-set-cookie-header"
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42"))
      "SID=31d4d96e407aad42"
      :test #'string=
      "name and value")
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "www.example.com"))
      "SID=31d4d96e407aad42; Domain=www.example.com"
      :test #'string=
      "name, value, and domain")
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "www.example.com" :path "/users"))
      "SID=31d4d96e407aad42; Path=/users; Domain=www.example.com"
      :test #'string=
      "name, value, domain, and path")
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)))
      "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT"
      :test #'string=
      "name, value, and expires")
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :max-age 3600 :same-site "Strict" :partitioned t))
      "SID=31d4d96e407aad42; Max-age=3600; SameSite=Strict; Partitioned"
      :test #'string=
      "max-age, same-site, partitioned")
  (is (write-set-cookie-header (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)
                                            :secure-p t :httponly-p t))
      "SID=31d4d96e407aad42; Expires=Fri, 25 Jan 2002 19:22:06 GMT; Secure; HttpOnly"
      :test #'string=))

(subtest "expired-cookie-p"
  (is (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0)
		    :secure-p t :httponly-p t))
      t)
  (is (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2002 0) :max-age 10000000000
                                            :secure-p t :httponly-p t))
      nil)
  (is (expired-cookie-p
       (make-cookie :name "SID" :value "31d4d96e407aad42" :expires (encode-universal-time 6 22 19 25 1 2300 0) :max-age 10000000
                                            :secure-p t :httponly-p t))
      nil)
  (is (expired-cookie-p (make-cookie :name "SID" :value "31d4d96e407aad42"))
      nil))

(finalize)
