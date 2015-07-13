(in-package :cl-user)
(defpackage cl-cookie-test
  (:use :cl
        :cl-cookie
        :prove)
  (:import-from :cl-cookie
                :parse-cookie-date))
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
  (is (parse-set-cookie-header "SID=31d4d96e407aad42" "example.com")
      (make-cookie :name "SID" :value "31d4d96e407aad42")
      :test #'cookie=
      "name and value")
  (is (parse-set-cookie-header "SID=" "example.com")
      (make-cookie :name "SID" :value "")
      :test #'cookie=
      "no value")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; Domain=example.com" "example.com")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :path "/" :domain "example.com")
      :test #'cookie=
      "path and domain")
  (is (parse-set-cookie-header "SID=31d4d96e407aad42; Path=/; Secure; HttpOnly" "example.com")
      (make-cookie :name "SID" :value "31d4d96e407aad42" :path "/" :secure-p t :httponly-p t)
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

(subtest "cookie-jar"
  (let ((cookie-jar (make-cookie-jar)))
    (is (length (cookie-jar-cookies cookie-jar)) 0
        "initial cookie jar is empty")
    (merge-cookies cookie-jar
                   (list (make-cookie :name "SID" :value "31d4d96e407aad42" :domain "example.com")
                         (make-cookie :name "lang" :value "en-US" :domain "example.com")))
    (is (length (cookie-jar-cookies cookie-jar)) 2)
    (merge-cookies cookie-jar
                   (list (make-cookie :name "id" :value "30" :domain "example.com")))
    (is (length (cookie-jar-cookies cookie-jar)) 3)
    (merge-cookies cookie-jar
                   (list (make-cookie :name "lang" :value "ja-JP" :domain "example.com")))
    (subtest "can overwrite"
      (is (length (cookie-jar-cookies cookie-jar)) 3)
      (is (cookie-value
           (find "lang" (cookie-jar-cookies cookie-jar) :key #'cookie-name :test #'string=))
          "ja-JP"))
    (subtest "not overwrite other domain cookies"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "lang" :value "fr-FR" :domain "www.example.com")))
      (is (length (cookie-jar-cookies cookie-jar)) 4))
    (subtest "cookie-jar-host-cookies"
      (is (length (cookie-jar-host-cookies cookie-jar "www.example.com")) 4))
    (subtest "Cross site cooking"
      (merge-cookies cookie-jar
                     (list (make-cookie :name "name" :value "Ultraman" :domain ".com")))
      (is (cookie-jar-host-cookies cookie-jar "hatena.com") nil))))

(finalize)
