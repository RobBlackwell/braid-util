;;;; braid-util.asd

(asdf:defsystem #:braid-util
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "Utilities for working with the Braid HTTP abstraction."
  :serial t
  :depends-on (#:alexandria
	       #:cl-ppcre
	       #:cl-fad
	       #:cl-base64
	       #:flexi-streams
	       #:braid)
  :components ((:file "package")
	       (:file "request")
	       (:file "mime-types")
	       (:file "util")
	       (:file "braid-util")))





