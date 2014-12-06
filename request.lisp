;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: HUNCHENTOOT; Base: 10 -*-

;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; parse-path borrowed from Hunchentoot with grateful thanks by Rob
;;; Blackwell.

;;;(in-package :hunchentoot)

(in-package #:braid-util)

(defun parse-path (path)
  "Return a relative pathname that has been verified to not contain
  any directory traversals or explicit device or host fields.  Returns
  NIL if the path is not acceptable."
  (when (every #'graphic-char-p path)
    (let* ((pathname (pathname (remove #\\ (regex-replace "^/*" path ""))))
           (directory (pathname-directory pathname)))
      (when (and (or (null (pathname-host pathname))
                     (equal (pathname-host pathname) (pathname-host *default-pathname-defaults*)))
                 (or (null (pathname-device pathname))
                     (equal (pathname-device pathname) (pathname-device *default-pathname-defaults*)))
                 (or (null directory)
                     (and (eql (first directory) :relative)
                          (every #'stringp (rest directory))))) ; only string components, no :UP traversals
        pathname))))
