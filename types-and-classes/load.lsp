;;; Tests of types and classes
(compile-and-load* "types-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-truename*))))
  (load "types-and-class.lsp")
  (load "types-and-class-2.lsp")
  (load "coerce.lsp")
  (load "subtypep.lsp")
  (load "subtypep-integer.lsp")
  (load "subtypep-float.lsp")
  (load "subtypep-rational.lsp")
  (load "subtypep-real.lsp")
  #-lispworks (load "subtypep-cons.lsp")
  (load "subtypep-member.lsp")
  (load "subtypep-eql.lsp")
  (load "subtypep-array.lsp")
  (load "subtypep-function.lsp")
  (load "subtypep-complex.lsp")

  (load "deftype.lsp")
  (load "standard-generic-function.lsp")
  (load "type-of.lsp")
  (load "typep.lsp")
  (load "class-precedence-lists.lsp"))
