;;; Tests of sequences

(load "copy-seq.lsp")
(load "elt.lsp")
(load "fill.lsp")
(load "fill-strings.lsp")
(load "make-sequence.lsp")
(load "map.lsp")
(load "map-into.lsp")
(load "reduce.lsp")
(load "count.lsp")
(load "count-if.lsp")
(load "count-if-not.lsp")
(load "reverse.lsp")
(load "nreverse.lsp")
(load "sort.lsp")
(load "find.lsp")
(load "find-if.lsp")
(load "find-if-not.lsp")
(load "position.lsp")
(compile-and-load "search-aux.lsp")
(load "search-list.lsp")
(load "search-vector.lsp")
(load "search-bitvector.lsp")
(load "search-string.lsp")
(load "mismatch.lsp")
(load "replace.lsp")
(compile-and-load "subseq-aux.lsp")
(load "subseq.lsp")
(load "substitute.lsp")
(load "substitute-if.lsp")
(load "substitute-if-not.lsp")
(load "nsubstitute.lsp")
(load "nsubstitute-if.lsp")
(load "nsubstitute-if-not.lsp")
(load "concatenate.lsp")
(load "merge.lsp")
(compile-and-load "remove-aux.lsp")
(load "remove.lsp")  ;; also related funs
(compile-and-load "remove-duplicates-aux.lsp")
(load "remove-duplicates.lsp")  ;; also delete-duplicates
