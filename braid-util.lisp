;;;; braid-util.lisp

(in-package #:braid-util)

(defun set-content-type-response (response content-type)
	"Sets the content-type header of the response."
	(setf (braid:header response :content-type) content-type)
	response)

(defun set-content-length-response (response)
	"Sets the content-length header of the response to the length of the
response body."
	(setf (braid:header response :content-length) (length (braid:body response)))
	response)

(defun plist-html-table (plist)
	"Converts a plist to a simple two-column HTML table."
	(with-output-to-string (s)
		(format s "<table>")
		(loop for (key value) on plist by #'cddr do
				 (format s "<tr><td>~a</td><td>~s</td></tr>" key value))
		(format s "</table>")))

(defun diagnostic-response (request response)
	"A handler that returns diagnostic information."
	(braid:make-response :body (plist-html-table
															(list :request request
																		:response response
																		:room (with-output-to-string (*standard-output*)(room))
																		:machine-instance (machine-instance)
																		:machine-type (machine-type)
																		:machine-version (machine-version)
																		:software-type (software-type)
																		:software-version (software-version)
																		:features *features*))))

(defun diagnostic-handler (request)
	""
	(diagnostic-response request nil))



(defun merge-plists (p1 p2)
	"Merges two property lists with values from p2 overriding those in p1."
;;; Courtesy Rainer Joswig
;;; http://stackoverflow.com/questions/3398602/easy-way-to-merge-plists
	(loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
        do (progn
             (push value p2)
             (push indicator p2))))

(defun merge-headers-response (response headers)
	"Merges HEADERS with the existing headers in RESPONSE."
	(setf (braid:headers response) (merge-plists (braid:headers response) headers))
	response)

(defun default-condition-handler (condition request)
	"Returns a simple, production safe HTTP 500 Internal Server Error."
	(braid:make-response :status 500 :body "Internal Server Error"))

(defun condition-handler (condition request)
	"Returns an HTTP 500 Internal Server Error with some information
about the condition."
	(braid:make-response :status 500
											 :body (format nil "Internal Server Error~%Condition : ~a ~%Request : ~s" condition request)))

(defun rfc-1123-date (&optional (time (get-universal-time)))
  "Generates a time string according to RFC 1123. Default is current
time."
  (multiple-value-bind
        (second minute hour date month year day-of-week)
      (decode-universal-time time 0)
    (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
            (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
            date
            (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
            year
            hour
            minute
            second)))

(defun set-last-modified (response)
	"If RESPONSE body is a pathname to a file, use its file-write-date to set the Last-Modified header."
	(when (typep (braid:body response) 'pathname)
		(setf (braid:header response :last-modified) (rfc-1123-date (file-write-date (braid:body response)))))
	response)

;;; Basic Authorization

(defun set-basic-authorization (request username password)
	"Sets the authorization header for REQUEST using basic auth and the
supplied USERNAME and PASSWORD."
	(setf (braid:header request :authorization)
				(format nil "Basic ~A"
								(base64:string-to-base64-string
								 (format nil "~A:~A"
												 username
												 password))))
	request)

;; Borrowed from Hunchentoot ..

(defun get-basic-authorization (request)
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (braid:header request :authorization))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (cl-ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (cl-ppcre:split ":" (base64:base64-string-to-string (subseq authorization start)) :limit 2)
        (values user password)))))

(defun make-not-authorized-response (realm)
	"Creates an HTTP 401 not-authorized response for the specified REALM."
	(braid:make-response :status 401
											 :headers (list :www-authenticate
																			(format nil "Basic realm=\"~a\"" realm))))

(defun parse-query-params (request)
	"Parses the query string from the REQUEST URI returning a plist of
keys and values."
	(let* ((query-string (puri:uri-query (puri:parse-uri (braid:uri request)))))
		(loop for kv in (cl-ppcre:split "&" query-string) appending
				 (let* ((p (position #\= kv))
								(k (if p (subseq kv 0 p) kv))
								(v (if p (subseq kv (1+ p)) "")))
					 (list (alexandria:make-keyword (string-upcase k)) v)))))

(defun set-query-params (request)
	"Parses the query string from the REQUEST URI and stashes the
resulting paraeters as a plist accessible via the :query-params key."
	(if (eq (getf request :query-params :none) :none)
			(cons :query-params (cons (parse-query-params request) request))
			request))


(defun combine-request-handlers (&rest request-handlers)
	""
	(lambda (request)
		(dolist (request-handler request-handlers)
			(alexandria:when-let (result (funcall request-handler request))
				(return result)))))



(defun ensure-response (response)
	"Turns a shorthand response such as a string or pathname into a full
Braid response."
	(typecase response
			(string (braid:make-response :body response))
			((simple-array (unsigned-byte 8)) (braid:make-response :body response))
			(pathname (braid:make-response :body response))
			(null (braid:make-response :status 404 :body "Not found"))
			(cons response)
			(t (braid:make-response :body (format nil "~a" response)))))

(defun ensure-request (request)
	"Turns a shorthand request such as a string URI into a full Braid
request."
	(typecase request
		(string (braid:make-request :uri request))
		(puri:uri (braid:make-request :uri request))
		(cons request)
		(t (braid:make-request :uri (format nil "~a" request)))))

(defun load-pathname-body (response)
	"Replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
	(when (typep (braid:body response) 'pathname)
		(setf (braid:body response) (alexandria:read-file-into-byte-vector (braid:body response))))
	response)

(defun sanitise-response (response)
	""
	(load-pathname-body
	 (ensure-response response)))

;;; End
