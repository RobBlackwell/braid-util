;;;; braid-util.lisp

(in-package #:braid-util)

(defun set-content-type (http-message string)
  "Sets the content-type header of the HTTP-MESSAGE to STRING."
  (setf (braid:http-message-header http-message :content-type) string))

(defun set-content-length (http-message)
  "Sets the content-length header of the HTTP-MESSAGE to the length of
the message body."
  (setf (braid:http-message-header http-message :content-length)
	(length (braid:http-message-body http-message))))

(defun plist-html-table (plist)
  "Converts the property list PLIST to a STRING containing a simple
two-column HTML table."
  (with-output-to-string (s)
    (format s "<table>")
    (loop for (key value) on plist by #'cddr do
	 (format s "<tr><td>~a</td><td>~s</td></tr>" key value))
    (format s "</table>")))

(defun make-diagnostic-response (http-request &optional (http-response nil))
  "Returns an HTTP-RESPONSE with diagnostic information."
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
  "An HTTP-REQUEST handler that returns a diagnostic HTTP-RESPONSE."
  (make-diagnostic-response http-request))

(defun merge-plists (p1 p2)
  "Merges two property lists with values from p2 overriding those in p1."
  ;; Courtesy Rainer Joswig
  ;; http://stackoverflow.com/questions/3398602/easy-way-to-merge-plists
  (loop with notfound = '#:notfound
     for (indicator value) on p1 by #'cddr
     when (eq (getf p2 indicator notfound) notfound) 
     do (progn
	  (push value p2)
	  (push indicator p2))))

(defun merge-headers (http-message headers)
  "Merges HEADERS with the existing headers in the HTTP-MESSAGE."
  (setf (braid:http-message-headers http-message)
	(merge-plists (braid:http-message-headers http-message) headers)))
 
(defun make-default-condition-response (condition http-request)
  "Returns a simple, production safe, HTTP 500 Internal Server Error
HTTP-RESPONSE."
  (braid:make-http-response :status 500
			    :body "Internal Server Error"))

(defun make-condition-response (condition http-request)
  "Returns an HTTP 500 Internal Server Error with some information
about the condition."
  (braid:make-http-response :status 500
			    :body (format nil "Internal Server Error~%Condition : ~a ~%Request : ~s" condition http-request)))

(defun set-last-modified (http-message)
  "If HTTP-MESSAGE body is a pathname to a file, use its
file-write-date to set the Last-Modified header."
  (when (typep (braid:http-message-body http-message) 'pathname)
    (setf (braid:http-message-header http-message :last-modified)
	  (rfc-1123-date (file-write-date (braid:http-message-body http-message))))))

;;; Basic Authorization

(defun set-basic-authorization (http-message username password)
  "Sets the authorization header for HTTP-MESSAGE using basic auth and
the supplied USERNAME and PASSWORD."
  (setf (braid:http-message-header http-message :authorization)
	(format nil "Basic ~A"
		(base64:string-to-base64-string
		 (format nil "~A:~A"
			 username
			 password)))))



(defun get-basic-authorization (http-message)
  "Returns as two values the user and password \(if any) as encoded in
the 'AUTHORIZATION' header.  Returns NIL if there is no such header."
  ;; Based on Hunchentoot "authorization", See Hunchentoot copyright
  ;; in request.lisp
  (let* ((authorization (braid:http-message-header http-message :authorization))
         (start (and authorization
                     (> (length authorization) 5)
                     (string-equal "Basic" authorization :end2 5)
                     (cl-ppcre:scan "\\S" authorization :start 5))))
    (when start
      (destructuring-bind (&optional user password)
          (cl-ppcre:split ":" (base64:base64-string-to-string (subseq authorization start)) :limit 2)
        (values user password)))))

(defun make-not-authorized-response (realm)
  "Creates an HTTP 401 not-authorized response with the specified REALM."
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

;; set-body-pathname-to-bytes?
(defun load-pathname-body (http-message)
  "Replaces a pathname body with a byte vector being
the contents of the file designated by the pathname. "
  (when (typep (braid:http-message-body http-message) 'pathname)
    (setf (braid:http-message-body http-message)
	  (alexandria:read-file-into-byte-vector (braid:http-message-body http-message)))))

(defun utf-8-bytes-to-string (sequence)
  (flexi-streams:octets-to-string sequence :external-format braid-util::+utf-8+))

;; set-body-utf-8-bytes-to-string?
(defun utf-8-bytes-to-string-body (http-message)
  (when (typep (braid:http-message-body http-message) '(simple-array (unsigned-byte 8)))
	(setf (braid:http-message-body http-message) (utf-8-byes-to-string (braid:http-message-body http-message)))))

(defun my-file-exists-p (pathspec)
  "Returns T if PATHSPEC exists and doesn't designate a directory."
  (let ((pathname (cl-fad:file-exists-p pathspec)))
    (and pathname (not (cl-fad:directory-pathname-p pathname)))))

(defun file-response (pathspec &key (default-mime-type "text/plain"))
  "Returns an HTTP-RESPONSE being the file from PATHSPEC or NIL if
PATHSPEC does not exist. Sets the Content Type header based on the
type of the file it can be detrmined, or DEFAULT-MIME-TYPE otherwise."
  (when (my-file-exists-p pathspec)
    (braid:make-http-response :body pathspec
			      :headers (list :content-type (or (mime-type pathspec) default-mime-type)))))

(defun make-file-request-handler (pathspec)
  "Middleware that always responds with the static file at PATHSPEC or
NIL if the file does not exist."
  (lambda (http-request)
    (file-response pathspec)))

(defun directory-response (path root-pathspec &key (default-mime-type "text/plain"))
  "Combines PATH and ROOT-PATHSPEC safely to return an HTTP-RESPONSE
being the designated file or NIL if the file does not exist. Sets the
Content Type header based on the type of the file it can be detrmined,
or DEFAULT-MIME-TYPE otherwise."
  (file-response (merge-pathnames (parse-path path) root-pathspec)))
	
(defun make-directory-request-handler (root-pathspec)
  "Returns an HTTP-REQUEST handler that serves static files from
ROOT-PATHSPEC."
  (lambda (http-request)
    (let ((path (subseq (url-decode (puri:uri-path (puri:parse-uri (braid:http-request-uri http-request)))) 1)))
      (directory-response path root-pathspec))))

(defun uri-to-string (uri)
  "Converts a PURI URI to a STRING."
  (with-output-to-string (s)
	(puri:render-uri uri s)))

;;; End
