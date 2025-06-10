;;;; General-purpose pattern matching (Library body)

;;; Actual implementation of `match` macro. `match` is a wrapper around this
;;; that first assigns the result of the input expression to a variable so that
;;; we don't have to worry about avoiding executing the input expression
;;; multiple times

(define-syntax match-variable
  (syntax-rules (if ? map ?map and or not quote unquote else _)
    ((match-variable value (() body ...) clause ...)
     (if (null? value) (let () body ...) (match-variable value clause ...)))
    ((match-variable value ((if condition) body ...) clause ...)
     (if condition (let () body ...) (match-variable value clause ...)))
    ((match-variable value ((if pattern condition) body ...) clause ...)
     (let ((otherwise (lambda () (match-variable value clause ...))))
       (match-variable value
         (pattern (if condition (let () body ...) (otherwise)))
         (else (otherwise)))))
    ((match-variable value ((? predicate) body ...) clause ...)
     (if (predicate value) (let () body ...) (match-variable value clause ...)))
    ((match-variable value ((? predicate pattern) body ...) clause ...)
     (let ((otherwise (lambda () (match-variable value clause ...))))
       (if (predicate value)
         (match-variable value (pattern body ...) (else (otherwise)))
         (otherwise))))
    ((match-variable value ((map function pattern) body ...) clause ...)
     (let ((mapped (function value)))
       (match-variable mapped
         (pattern body ...)
         (else (match-variable value clause ...)))))
    ((match-variable value
       ((?map predicate function pattern) body ...)
       clause ...)
     (match-variable value
       ((and (? predicate) (map function pattern)) body ...)
       clause ...))
    ((match-variable value ((and pattern pattern* ...) body ...) clause ...)
     (let ((otherwise (lambda () (match-variable value clause ...))))
       (match-variable value
         (pattern (match-variable value
                    ((and pattern* ...) body ...)
                    (else (otherwise))))
           (else (otherwise)))))
    ((match-variable value ((and) body ...) clause ...) (let () body ...))
    ((match-variable value ((or pattern ...) . body) clause ...)
     (match-variable value (pattern . body) ...  clause ...))
    ((match-variable value ((not pattern) body ...) clause ...)
     (match-variable value
       (pattern (match-variable value clause ...))
       (else body ...)))
    ((match-variable value ('pattern body ...) clause ...)
     (if (equal? value 'pattern)
       (let () body ...)
       (match-variable value clause ...)))
    ((match-variable value (,pattern body ...) clause ...)
     (if (equal? value pattern)
       (let () body ...)
       (match-variable value clause ...)))
    ((match-variable value ((pattern . pattern*) body ...) clause ...)
     (let ((otherwise (lambda () (match-variable value clause ...))))
       (if (pair? value)
         (match-variable (car value)
           (pattern (match-variable (cdr value)
                      (pattern* body ...)
                      (else (otherwise))))
           (else (otherwise)))
         (otherwise))))
    ((match-variable value (#(pattern ...) body ...) clause ...)
     (match-variable value
       ((?map vector? vector->list (pattern ...)) body ...)
       clause ...))
    ((match-variable value (else body ...) clause ...) (let () body ...))
    ((match-variable value (_ body ...) clause ...) (let () body ...))
    ((match-variable value (ident-or-const body ...) clause ...)
     ;; This trick is stolen from
     ;; https://github.com/SaitoAtsushi/pattern-match-lambda
     (let-syntax
       ((test
          (syntax-rules ()
            ((test ident-or-const) (let ((ident-or-const value)) body ...))
            ((test ident)
             (match-variable value (,ident-or-const body ...) clause ...)))))
       (test test)))
    ((match-variable value (name body ...) clause ...)
     (let ((name value)) body ...))
    ((match-variable value) (error "no pattern matched"))))

;;; The pattern matching macro. It looks similar to Shinn pattern matcher, but
;;; has significant differences due to me not knowing how to implement such
;;; things as only matching if subpatterns bound to the same name are equal, or
;;; ellipses. Here's the list of available patterns:
;;; `<name>` - matches anything and binds its value to `<name>`;
;;; `<constant>` - matches only if the value is `equal?` to `<constant>`;
;;; `,<expr>` - matches only if the value is `equal?` to result of `<expr>`;
;;; `,<literal>` - matches only if the value is `equal?` to the literal data;
;;; `(<pattern-a> . <pattern-b>)` - only matches pairs, then matches `car` and
;;;   `cdr` recursively;
;;; `#(<pattern> ...)` - only matches vectors, then matches each element
;;;   recursively. Unfortunately, due to the absence of ellipses, it cannot be
;;;   used for variable-length vectors;
;;; `(if <condition>)` - matches only if `<condition>` is `#t`
;;; `(if <pattern> <condition>)` - matches against <pattern>, then also checks
;;;   if `<condition>` is `#t`;
;;; `(? <predicate>)` - matches only if `(<predicate> <matched-value>)` is `#t`;
;;; `(? <predicate> <pattern>)` - matches only if
;;;   `(<predicate> <matched-value>)` is `#t`, then also matches against
;;;   `<pattern>`;
;;; `(map <function> <pattern>)` - first calls `<function>` with the matched
;;;   value, then matches the result against `<pattern>`. Useful for extending
;;;   pattern matching to custom types;
;;; `(?map <predicate> <function> <pattern>) - first checks the matching value
;;;   against the predicate, then, if it matches, works like the `map` pattern;
;;; `(and <pattern> ...)` - matches only if it matches against all the patterns.
;;;   Only useful for binding names to subpatterns that you also want to
;;;   destructure further, maybe there are other niche uses for it;
;;; `(or <pattern> ...)` - matches the first pattern that works;
;;; `(not <pattern>)` - only matches if `<pattern>` doesn't match;
;;; `else` - matches anything, without binding values. To be used in the last
;;;   clause;
;;; `_` - matches anything, without binding values. To be used in subpatterns.

(define-syntax match
  (syntax-rules ()
    ((match expr clause ...)
     (let ((value expr))
       (match-variable value clause ...)))))
