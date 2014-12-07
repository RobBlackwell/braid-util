(ql:quickload :braid-util)

(defpackage :braid-util-tests
  (:use :cl))

(in-package :braid-util-tests)

(defun my-response ()
  (braid:make-http-response :body (merge-pathnames (asdf:system-source-directory :braid-util) "README.md")))

(defun test1 ()
  "Tests SET-CONTENT-TYPE."
  (let ((response (my-response)))
    (braid-util:set-content-type response "application/json")
    (string= "application/json" (braid:http-message-header response :content-type))))

(defun test2 ()
  "Tests SET-CONTENT-LENGTH."
  (let ((response (braid:make-http-response :body "hello")))
    (braid-util:set-content-length response)
    (eql (braid:http-message-header response :content-length) 5)))

(defun test3 ()
  "Tests MAKE-DIAGNOSTIC-RESPONSE."
  (let ((response (braid-util:make-diagnostic-response (braid:make-http-request :uri "https://www.google.co.uk"))))
    (stringp (braid:http-message-body response))))

(defun test4 ()
  "Tests merge-headers"
  (let ((request (braid:make-http-request :headers (list :foo 1 :bar 2))))
    (braid-util:merge-headers request (list :bar 3 :baz 4))
    (and
     (eq (braid:http-message-header request :foo) 1)
     (eq (braid:http-message-header request :bar) 3)
     (eq (braid:http-message-header request :baz) 4))))

(defun test5 ()
  "Tests SET-LAST-MODIFIED."
  (let ((response (my-response)))
    (braid-util:set-last-modified response)
    (stringp (braid:http-message-header response :last-modified))))

(defun test6 ()
  "Tests SET-BASIC-AUTHORIZATION and GET-BASIC-AUTHORIZATION."
  (let ((message (braid:make-http-response :body "hello world")))
    (braid-util:set-basic-authorization message "username" "password")
    (multiple-value-bind (username password) (braid-util:get-basic-authorization message)
      (and
       (string= username "username")
       (string= password "password")))))

(defun test7 ()
  "Tests PARSE-QUERY-PARAMS."
  (let ((plist (braid-util:parse-query-params (braid:make-http-request :uri "http://foo.bar?a=1&b=hello%20world"))))
    (and
     (string= (getf plist :a) "1")
     (string= (getf plist :b) "hello world"))))

(defun test8 ()
  "Tests ENSURE-RESPONSE."
  (let ((response (braid-util:ensure-http-response 67)))
    (and (stringp (braid:http-message-body response))
	 (eq (braid:http-response-status response) 200))))

(defun test9 ()
  "Tests ENSURE-REQUEST."
  (let ((request (braid-util:ensure-http-request "http://www.google.co.uk")))
    (typep (braid:http-request-uri request) 'puri:uri)))

(defun test10 ()
  (let ((response (my-response)))
    (braid-util:set-body-pathname-to-bytes response)
    (> (length (braid:http-message-body response)) 100)))

(defun run ()
  "Runs all tests."
  (and
   (test1)
   (test2)
   (test3)
   (test4)
   (test5)
   (test6)
   (test7)
   (test8)
   (test9)
   (test10)))

;; (run)
