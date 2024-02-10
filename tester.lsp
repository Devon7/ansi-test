;;; LispWorks has no obvious RT package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *default-pathname-defaults* #P"~/quicklisp/local-projects/ansi-test/")
  (hcl:change-directory *default-pathname-defaults*)
  (load "rt-package.lsp"))

(defmacro tr (test-name)
  `(tr* ,(if (symbolp test-name)
	     (string-downcase (symbol-name test-name))
	     test-name)))

(defun tr* (test-name)
  "LispWorks Personal Edition ansi-test TEST-NAME"

  (setf *handle-warn-on-redefinition* nil)

  ;;	doit1.lsp
  ;; (trace compile-file-pathname pathname-type make-pathname directory delete-file load in-package truename time)

  (pushnew :ansi-tests-strict-initial-element *features*)

  ;; Remove compiled files
  (let* ((fn (compile-file-pathname "doit.lsp"))
	 (type (pathname-type fn))
	 (dir-pathname (make-pathname :name :wild
				      :type type))
	 (files (directory dir-pathname)))
    (assert type)
    (assert (not (string-equal type "lsp")))
    (mapc #'delete-file files))

  (load "gclload1.lsp")

  ;;	*/load.lsp
  ;; (trace rt:disable-note rt:rem-test rt:do-tests)

  (handler-bind ((warning (lambda (w)
			    (when (string= "Redefining test ~:@(~S~)"
					   (simple-condition-format-control w))
			      (muffle-warning)))))
    (load (make-pathname :directory `(:relative ,test-name)
			 :name "load"
			 :type "lsp")))

  ;;	doit2.lsp
  (mapc #'rt:disable-note '(:allow-nil-arrays
			    :nil-vectors-are-strings))

  (in-package :cl-test)

  (when *load-pathname*
    (mapc #'rt:rem-test '(load-pathname.1
			  load-truename.1)))

  (setf *default-pathname-defaults* (truename #P"sandbox/"))

  (time (rt:do-tests))

  (let* ((f (length rt:*failed-tests*))
	 (p (length rt:*passed-tests*))
	 (f+p (+ f p))
	 (d (/ f+p 100)))
    (format t "~&~D (~D%) failed, ~D (~D%) passed, ~D total ~@:(~A~) tests.~%"
	    f (round f d) p (round p d) f+p test-name))

  #+allegro (cl-user::exit)
  #+(or cmu sbcl ccl gcl armedbear) (cl-user::quit))