;;;; Hashmap implementation (Library definition)

(define-library (wgscheme hashmap)
  (import (scheme base) (scheme case-lambda) (scheme char) (wgscheme match))
  (export hash hashmap hashmap? hashmap-hash hashmap-equal?  hashmap-ref-opt
          hashmap-ref hashmap-contains? hashmap-for-each hashmap-keys
          hashmap-values hashmap-pairs hashmap-insert!-opt hashmap-insert!
          hashmap-replace! hashmap-delete!-opt hashmap-delete! hashmap-remove!)
  (include "hashmap.body.scm"))
