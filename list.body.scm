;;;; List utilities (Library body)

;;; Returns a list that contains `x` repeated `n` times

(define (repeat n x)
  (do ((r '() (cons x r)) (i 0 (+ i 1))) ((>= i n) r)))

;;; Returns a list of consecutive integers

(define iota
  (case-lambda
    ;; Returns numbers in range [0; n)
    ((n) (iota 0 n))
    ;; Returns numbers in range [a; b)
    ((a b) (do ((r '() (cons i r)) (i (- b 1) (- i 1))) ((< i a) r)))))

;;; Your standard `filter`

(define (filter p? xs)
  (match xs
    ((if (x . xs) (p? x)) (cons x (filter p? xs)))
    ((x . xs) (filter p? xs))
    (else xs)))

;;; Left-associative reduction

(define (reduce-left f xs)
  (match xs
    ((x) x)
    ((x y . xs) (reduce-left f (cons (f x y) xs)))))

;;; Right-associative reduction

(define (reduce-right f xs)
  (match xs
    ((x) x)
    ((x . xs) (f x (reduce-right f xs)))))

;;; Constructs a list with `i`th element removed

(define (remove-index i xs)
  (match xs
    ((if (_ . xs) (zero? i)) xs)
    ((x . xs) (cons x (remove-index (- i 1) xs)))))

;;; Finds and returns from the list that satisfies the predicate, wrapped into
;;; a 1-element list. If no such element was found, returns an empty list

(define (find-opt p? xs)
  (match xs
    ((if (x . _) (p? x)) (list x))
    ((_ . xs) (find-opt p? xs))
    (else '())))

;;; Same as `find-opt`, but returns the element it finds as-is, and fails if
;;; nothing is found

(define (find p? xs) (car (find-opt p? xs)))

;;; Constructs a list that consists of `n` first elements of `xs`, or same
;;; elements as `xs` if its length is <= `n`

(define (take n xs)
  (match xs
    ((if (x . xs) (positive? n)) (cons x (take (- n 1) xs)))
    (else '())))

;;; Constructs a list that consists of elements of `xs` with the first `n`
;;; removed, or all of them removed if `xs` has less that `n` elements

(define (drop n xs)
  (match xs
    ((if (_ . xs) (positive? n)) (drop (- n 1) xs))
    (else xs)))
