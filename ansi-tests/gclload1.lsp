(load "compile-and-load.lsp")
(load "rt-package.lsp")
(compile-and-load "rt.lsp")
;;; (unless (probe-file "rt.o") (compile-file "rt.lsp"))
;;; (load "rt.o")
(load "cl-test-package.lsp")
(in-package :cl-test)
(load "universe.lsp")
(compile-and-load "ansi-aux.lsp")
;;; (unless (probe-file "ansi-aux.o") (compile-file "ansi-aux.lsp"))
;;; (load "ansi-aux.o")
