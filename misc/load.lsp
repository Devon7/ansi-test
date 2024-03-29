;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jun 23 20:14:32 2005
;;;; Contains: Load misc. tests

;;; Miscellaneous tests, mostly tests that failed in random testing
;;; on various implementations
(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-truename*))))
  (load "misc.lsp")

;;; Misc. tests dealing with type propagation in CMUCL
  (load "misc-cmucl-type-prop.lsp")
)
