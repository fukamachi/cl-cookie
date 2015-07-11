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
  (dolist (date (list "Wed, 06-Feb-2008 21:01:38 GMT"
                      "Wed, 06-Feb-08 21:01:38 GMT"
                      "Tue Feb 13 08:00:00 2007 GMT"
                      "Wednesday, 07-February-2027 08:55:23 GMT"
                      "Wed, 07-02-2017 10:34:45 GMT"))
    (ok (parse-cookie-date date) date)))

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
