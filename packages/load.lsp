;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct  6 00:32:56 2002
;;;; Contains: Loader for files containing package tests

(compile-and-load* "packages00-aux.lsp")
(compile-and-load* "package-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-truename*))))
  (load "find-symbol.lsp")
  (load "find-all-symbols.lsp")
  (load "find-package.lsp")
  (load "list-all-packages.lsp")
  (load "package-name.lsp")
  (load "package-nicknames.lsp")
  (load "intern.lsp")
  (load "export.lsp")
  (load "rename-package.lsp")
  (load "shadow.lsp")
  (load "shadowing-import.lsp")
  (load "delete-package.lsp")
  (load "make-package.lsp")
  (load "with-package-iterator.lsp")
  (load "unexport.lsp")
  (load "unintern.lsp")
  (load "in-package.lsp")
  (load "unuse-package.lsp")
  (load "use-package.lsp")
  (load "defpackage.lsp")
  (load "do-symbols.lsp")
  (load "do-external-symbols.lsp")
  (load "do-all-symbols.lsp")
  (load "packagep.lsp")
  (load "package-error.lsp")
  (load "package-error-package.lsp")
  (load "keyword.lsp")
  (load "package-shadowing-symbols.lsp")
  (load "package-use-list.lsp")
  (load "package-used-by-list.lsp")
  (load "import.lsp")
)
