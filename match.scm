;;;; General-purpose pattern matching (Library definition)

(define-library (wgscheme match)
  (import (scheme base))
  (export match)
  (include "match.body.scm"))
