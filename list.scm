;;;; List utilities (Library definition)

(define-library (wgscheme list)
  (import (scheme base) (scheme case-lambda) (wgscheme match))
  (export
    repeat
    iota
    filter
    reduce-left
    reduce-right
    remove-index
    find-opt
    find
    take
    drop)
  (include "list.body.scm"))
