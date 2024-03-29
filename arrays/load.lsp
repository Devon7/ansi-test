;;; Tests on arrays
(compile-and-load* "array-aux.lsp")
(compile-and-load* "bit-aux.lsp")

(in-package #:cl-test)

(let ((*default-pathname-defaults*
       (make-pathname
        :directory (pathname-directory *load-truename*))))
  (load "aref.lsp")
  (load "array.lsp")
  (load "array-t.lsp")
  (load "array-as-class.lsp")
  (load "simple-array.lsp")
  (load "simple-array-t.lsp")
  (load "bit-vector.lsp")
  (load "simple-bit-vector.lsp")
  (load "make-array.lsp")
  (load "adjust-array.lsp")
  (load "adjustable-array-p.lsp")
  (load "array-displacement.lsp")
  (load "array-dimension.lsp")
  (load "array-dimensions.lsp")
  (load "array-element-type.lsp")
  (load "array-has-fill-pointer-p.lsp")
  (load "array-in-bounds-p.lsp")
  (load "array-misc.lsp")
  (load "array-rank.lsp")
  (load "array-row-major-index.lsp")
  (load "array-total-size.lsp")
  (load "arrayp.lsp")
  (load "fill-pointer.lsp")
  (load "row-major-aref.lsp")
  (load "simple-vector-p.lsp")
  (load "svref.lsp")
  (load "upgraded-array-element-type.lsp")
  (load "vector.lsp")
  (load "vector-pop.lsp")
  (load "vector-push.lsp")
  (load "vector-push-extend.lsp")
  (load "vectorp.lsp")
  (load "bit.lsp")
  (load "sbit.lsp")
  (load "bit-and.lsp")
  (load "bit-andc1.lsp")
  (load "bit-andc2.lsp")
  (load "bit-eqv.lsp")
  (load "bit-ior.lsp")
  (load "bit-nand.lsp")
  (load "bit-nor.lsp")
  (load "bit-orc1.lsp")
  (load "bit-orc2.lsp")
  (load "bit-xor.lsp")
  (load "bit-not.lsp")
  (load "bit-vector-p.lsp")
  (load "simple-bit-vector-p.lsp"))
