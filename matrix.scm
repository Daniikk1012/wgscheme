;;;; Matrix utilities (Library definition)

(define-library (wgscheme matrix)
  (import
    (rename
      (scheme base)
      (+ scheme+)
      (- scheme-)
      (* scheme*)
      (/ scheme/)
      (abs scheme-abs))
    (scheme case-lambda)
    (scheme inexact)
    (wgscheme list)
    (wgscheme match))
  (export + - * / abs transpose)
  (include "matrix.body.scm"))
