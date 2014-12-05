;;;; braid-util.asd

(asdf:defsystem #:braid-util
  :version "0.0.1"
  :author "Rob Blackwell"
  :description "Utilities for working with the Braid HTTP abstraction."
  :serial t
	:depends-on (#:alexandria
							 #:flexi-streams
							 #:braid)
  :components ((:file "package")
							 (:file "util")
							 (:file "braid-util")))





