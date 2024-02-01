;;;; Character tests
(compile-and-load* "char-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-truename*))))
  (load "character.lsp")
  (load "char-compare.lsp")
  (load "name-char.lsp")
)
