;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Jan 28 06:41:46 2004
;;;; Contains: Tests of FORCE-OUTPUT

(in-package :cl-test)

(deftest force-output.1
  (force-output)
  nil)

(deftest force-output.2
  (force-output t)
  nil)

(deftest force-output.3
  (force-output nil)
  nil)

(deftest force-output.4
  (loop for s in (list *debug-io* *error-output* *query-io*
		       *standard-output* *trace-output* *terminal-io*)
	for results = (multiple-value-list (force-output s))
	unless (equal results '(nil))
	collect s)
  nil)

;;; Error tests

(deftest force-output.error.1
  (signals-error (force-output nil nil) program-error)
  t)

(deftest force-output.error.2
  (signals-error (force-output t nil) program-error)
  t)

(deftest force-output.error.3
  (loop for x in *mini-universe*
	unless (or (member x '(nil t))
		   (typep x 'stream)
		   (equalt
		    (eval `(multiple-value-list
			    (signals-error (force-output ',x) type-error)))
		    '(t)))
	collect x)
  nil)

