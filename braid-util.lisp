;;;; braid-util.lisp

(in-package #:braid-util)

(defun set-content-type (http-response content-type)
  "Sets the content-type header of HTTP-RESPONSE to CONTENT-TYPE."
  (setf (braid:http-response-header http-response :content-type) content-type))

(defun set-content-length (http-response)
  "Sets the content-length header of HTTP-RESPONSE to the length of the
response body."
  (setf (braid:http-response-header http-response :content-length)
		(length (braid:http-response-body http-response))))

(defun plist-html-table (plist)
  "Converts a plist to a simple two-column HTML table."
  (with-output-to-string (s)
	(format s "<table>")
	(loop for (key value) on plist by #'cddr do
		 (format s "<tr><td>~a</td><td>~s</td></tr>" key value))
	(format s "</table>")))

(defun make-diagnostic-response (http-request &optional (http-response nil))
  "Creates a diagnostic information response."
  (braid:make-http-response :body (plist-html-table
								   (list :http-request http-request
										 :http-response http-response
										 :room (with-output-to-string (*standard-output*)(room))
										 :machine-instance (machine-instance)
										 :machine-type (machine-type)
										 :machine-version (machine-version)
										 :software-type (software-type)
										 :software-version (software-version)
										 :features *features*))))

(defun diagnostic-handler (http-request)
  "A handler that returns diagnostic information in the form of an
http-response."
  (make-diagnostic-response http-request))


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

(defun merge-headers (http-response headers)
	"Merges HEADERS with the existing headers in HTTP-RESPONSE."
	(setf (braid:http-response-headers http-response)
		  (merge-plists (braid:http-response-headers http-response) headers)))

(defun make-default-condition-response (condition http-request)
	"Returns a simple, production safe HTTP 500 Internal Server Error."
	(braid:make-http-response :status 500
							  :body "Internal Server Error"))

(defun make-condition-handler (condition http-request)
  "Returns an HTTP 500 Internal Server Error with some information
about the condition."
  (braid:make-http-response :status 500
							:body (format nil "Internal Server Error~%Condition : ~a ~%Request : ~s" condition http-request)))

;; (defun rfc-1123-date (&optional (time (get-universal-time)))
;;   "Generates a time string according to RFC 1123. Default is current
;; time."
;;   (multiple-value-bind
;;         (second minute hour date month year day-of-week)
;;       (decode-universal-time time 0)
;;     (format nil "~A, ~2,'0d ~A ~4d ~2,'0d:~2,'0d:~2,'0d GMT"
;;             (svref #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun") day-of-week)
;;             date
;;             (svref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") (1- month))
;;             year
;;             hour
;;             minute
;;             second)))

(defun set-last-modified (http-response)
  "If RESPONSE body is a pathname to a file, use its file-write-date to set the Last-Modified header."
  (when (typep (braid:http-response-body http-response) 'pathname)
	(setf (braid:http-response-header http-response :last-modified)
		  (rfc-1123-date (file-write-date (braid:http-response-body http-response))))))

;;; Basic Authorization

(defun set-basic-authorization (http-request username password)
  "Sets the authorization header for REQUEST using basic auth and the
supplied USERNAME and PASSWORD."
  (setf (braid:http-request-header http-request :authorization)
		(format nil "Basic ~A"
				(base64:string-to-base64-string
				 (format nil "~A:~A"
						 username
						 password)))))

;; Borrowed from Hunchentoot ..

(defun get-basic-authorization (request)
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  (let* ((authorization (braid:http-request-header request :authorization))
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
  (braid:make-http-response :status 401
							:headers (list :www-authenticate
										   (format nil "Basic realm=\"~a\"" realm))))

(defun parse-query-params (http-request)
  "Parses the query string from the HTTP-REQUEST URI returning a plist
of keys and values."
  (let* ((query-string (puri:uri-query (puri:parse-uri (braid:http-request-uri http-request)))))
	(loop for kv in (cl-ppcre:split "&" query-string) appending
		 (let* ((p (position #\= kv))
				(k (if p (subseq kv 0 p) kv))
				(v (if p (subseq kv (1+ p)) "")))
		   (list (alexandria:make-keyword (string-upcase k)) v)))))

;; (defun set-query-params (request)
;; 	"Parses the query string from the REQUEST URI and stashes the
;; resulting paraeters as a plist accessible via the :query-params key."
;; 	(if (eq (getf request :query-params :none) :none)
;; 			(cons :query-params (cons (parse-query-params request) request))
;; 			request))


(defun combine-http-request-handlers (&rest http-request-handlers)
  ""
  (lambda (http-request)
	(dolist (http-request-handler http-request-handlers)
	  (alexandria:when-let (result (funcall http-request-handler http-request))
		(return result)))))



(defun ensure-response (http-response)
  "Turns a shorthand response such as a string or pathname into a full
Braid response."
  (typecase http-response
	(string (braid:make-http-response :body http-response))
	((simple-array (unsigned-byte 8)) (braid:make-http-response :body http-response))
	(pathname (braid:make-http-response :body http-response))
	(null (braid:make-http-response :status 404 :body "Not found"))
	(braid:http-response http-response)
	(t (braid:make-http-response :body (format nil "~a" http-response)))))

(defun ensure-request (http-request)
  "Turns a shorthand request such as a string URI into a full Braid
request."
  (typecase http-request
	(string (braid:make-http-request :uri http-request))
	(puri:uri (braid:make-http-request :uri http-request))
	(braid:http-request http-request)
	(t (braid:make-http-request :uri (format nil "~a" http-request)))))

(defun load-pathname-body (http-response)
  "Replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
  (when (typep (braid:http-response-body http-response) 'pathname)
	(setf (braid:http-response-body http-response)
		  (alexandria:read-file-into-byte-vector (braid:http-response-body http-response)))))

(defun utf-8-bytes-to-string (vector)
  (flexi-streams:octets-to-string vector :external-format braid-util::+utf-8+))

(defun utf-8-bytes-to-string-body (http-response)
  (when (typep (braid:http-response-body http-response) '(simple-array (unsigned-byte 8)))
	(setf (braid:http-response-body http-response) (utf-8-byes-to-string (braid:http-response-body http-response)))))

;;;

(defun my-file-exists-p (pathspec)
  "Returns T if PATHSPEC exists and doesn't designate a directory."
  (let ((pathname (cl-fad:file-exists-p pathspec)))
	(and pathname (not (cl-fad:directory-pathname-p pathname)))))

(defun file-response (pathspec &key (default-mime-type "text/plain"))
  "Returns a Braid response being the file from PATHSPEC or NIL if PATHSPEC does not exist."
  (when (my-file-exists-p pathspec)
	(braid:make-http-response :body pathspec
							  :headers (list :content-type (or (mime-type pathspec) default-mime-type)))))

(defun make-file-request-handler (pathspec)
  "Middleware that always responds with the static file at PATHSPEC or
NIL if the file does not exist."
  (lambda (http-request)
	(file-response pathspec)))

(defun directory-response (path root-pathspec)
  ""
  (file-response (merge-pathnames (parse-path path) root-pathspec)))
	
(defun make-directory-request-handler (root-pathspec)
  "Returns a Braid request handler that serves static files from
ROOT-PATHSPEC."
  (lambda (http-request)
	(let ((path (subseq (puri:uri-path (puri:parse-uri (braid:http-request-uri http-request))) 1)))
	  (directory-response path root-pathspec))))


;;;

(defun uri-to-string (uri)
  "Converts a PURI URI to a STRING."
  (with-output-to-string (s)
	(puri:render-uri uri s)))

;;; End
