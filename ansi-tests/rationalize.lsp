;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Sep  1 14:00:45 2003
;;;; Contains: Tests of RATIONALIZE

(in-package :cl-test)

(deftest rationalize.error.1
  (classify-error (rationalize))
  program-error)

(deftest rationalize.error.2
  (classify-error (rationalize 0 nil))
  program-error)

(deftest rationalize.error.3
  (classify-error (rationalize 0 0))
  program-error)

(deftest rationalize.error.4
  (loop for x in *mini-universe*
	unless (or (realp x)
		   (eq (eval `(classify-error (rationalize (quote ,x))))
		       'type-error))
	collect x)
  nil)

(deftest rationalize.1
  (loop for x in *reals*
	for r = (rationalize x)
	unless (and (rationalp r)
		    (if (floatp x)
			(= (float r x) x)
		      (eql x r)))
	collect (list x r))
  nil)

(deftest rationalize.2
  (loop for type in '(short-float single-float double-float long-float)
	collect
	(loop for i from -10000 to 10000
	      for x = (coerce i type)
	      for r = (rationalize x)
	      count (not (eql r i))))
  (0 0 0 0))

(deftest rationalize.3
  (loop for type in '(short-float single-float double-float long-float)
	for bound in '(1.0s5 1.0f10 1.0d20 1.0l30)
	nconc
	(loop for x = (random-from-interval bound)
	      for r = (rationalize x)
	      for x2 = (float r x)
	      repeat 1000
	      unless (and (rationalp r) (= x x2))
	      collect (list x r x2)))
  nil)