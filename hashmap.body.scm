;;;; Hashmap implementation (Library body)
;;;; This is a linear probing hashmap that doesn't perform any manipulations on
;;;; generated hash values, so its performance depends heavily on the quality of
;;;; the hash function

;;; Default hash function. Supports booleans, numbers (there are probably bugs
;;; with those, however, the Scheme numeric tower is tricky), nulls, pairs,
;;; chars, strings, symbols, vectors and bytevectors. For everything else,
;;; returns a technically correct, but useless hash of 1

(define (hash x)
  (define (multihash a b) (+ (* (hash a) 31) (hash b)))
  (match x
    ((? boolean?) (if x 1 0))
    ((? integer?) (abs x))
    ((? rational?) (multihash (numerator x) (denominator x)))
    ((? complex?) (multihash (real-part x) (imag-part x)))
    (() 0)
    ((a . b) (multihash a b))
    ((? char?) (hash (char->integer x)))
    ((? string?) (hash (string->list x)))
    ((? symbol?) (hash (symbol->string x)))
    ((? vector?) (hash vector->list x))
    ((? bytevector?) (hash (do ((i (- (bytevector-length x) 1) (- i 1))
                                (r '() (cons (bytevector-u8-ref x i) r)))
                             ((negative? i) r))))
    (else 1)))

(define-record-type <hashmap>
  (hashmap-impl h eq v l)
  hashmap? ; Checks if the input is a hashmap
  (h hashmap-hash) ; Returns the hash function for the hashmap
  (eq hashmap-equal?) ; Returns the equality function for the hashmap
  (v hashmap-vector hashmap-vector-set!)
  (l hashmap-load hashmap-load-set!))

;;; Constructs a new hashmap from hash and equality functions, using default
;;; ones if they aren't passed

(define hashmap (case-lambda (() (hashmap hash))
                             ((h) (hashmap h equal?))
                             ((h eq) (hashmap-impl h eq #(()) 0))))

;;; Returns the value under key `k` wrapped in a single-element list, or null if
;;; it isn't contained within the hashmap

(define (hashmap-ref-opt m k)
  (define mequal? (hashmap-equal? m))
  (define v (hashmap-vector m))
  (define n (vector-length v))
  (let loop ((i ((hashmap-hash m) k)))
    (define j (remainder i n))
    (match (vector-ref v j)
      ((if (x . y) (mequal? x k)) (list y))
      (() '())
      (else (loop (+ j 1))))))

;;; Returns the value under key `k`. Errors if no such key is contained

(define (hashmap-ref m k) (car (hashmap-ref-opt m k)))

;;; Returns `#t` if the key exists in the hashmap

(define (hashmap-contains? m k) (pair? (hashmap-ref-opt m k)))

;;; Iterates over all pairs in the hashmap, in no particular order. Returns the
;;; hashmap itself

(define (hashmap-for-each f m)
  (vector-for-each (lambda (x) (when (pair? x) (f (car x) (cdr x))))
                   (hashmap-vector m))
  m)

;;; Returns all keys contained in the hashmap, in no particular order

(define (hashmap-keys m)
  (define result '())
  (hashmap-for-each (lambda (k x) (set! result (cons k result))) m)
  result)

;;; Returns all values contained in the hashmap, in no particular order

(define (hashmap-keys m)
  (define result '())
  (hashmap-for-each (lambda (k x) (set! result (cons x result))) m)
  result)

;;; Returns all pairs contained in the hashmap, in no particular order. The
;;; pairs are copies of the internal pairs, so that their mutation doesn't break
;;; the hashmap

(define (hashmap-pairs m)
  (define result '())
  (hashmap-for-each (lambda (k x) (set! result (cons (cons k x) result))) m)
  result)

;;; Insert a key-value pair into the hashmap. Returns previous value wrapped
;;; in a single-element list, if any, otherwise returns empty list

(define (hashmap-insert!-opt m k x)
  (define mequal? (hashmap-equal? m))
  (define v (hashmap-vector m))
  (define n (vector-length v))
  (when (> (* 2 (+ (hashmap-load m) 1)) n)
    (hashmap-vector-set! m (make-vector (* n 2) '()))
    (hashmap-load-set! m 0)
    (vector-for-each
      (lambda (x) (when (pair? x) (hashmap-insert!-opt m (car x) (cdr x))))
      v))
  (hashmap-load-set! m (+ (hashmap-load m) 1))
  (let* ((v (hashmap-vector m)) (n (vector-length v)))
    (let loop ((i ((hashmap-hash m) k)))
      (define j (remainder i n))
      (match (vector-ref v j)
        ((or () 'deleted) (vector-set! v j (cons k x)) '())
        (((if y (mequal? y k)) . r) (vector-set! v j (cons k x)) (list r))
        (else (loop (+ j 1)))))))

;;; Insert a key-value pair into the hashmap. Returns hashmap itself

(define (hashmap-insert! m k x) (hashmap-insert!-opt m k x) m)

;;; Insert a key-value pair into the hashmap. Returns previous value contained
;;; at that key, or errors if there was no such value

(define (hashmap-replace! m k x) (car (hashmap-insert!-opt m k x)))

;;; Deletes the pair with given key and returns the value at it wrapped in
;;; single-element list if it existed. If it didn't exist, returns empty list

(define (hashmap-delete!-opt m k)
  (define mequal? (hashmap-equal? m))
  (define v (hashmap-vector m))
  (define n (vector-length v))
  (let loop ((i ((hashmap-hash m) k)))
    (define j (remainder i n))
    (match (vector-ref v j)
      (((if x (mequal? x k)) . y)
       (vector-set! v j 'deleted)
       (if (null? (vector-ref v (remainder (+ j 1) n)))
         (do ((i j (modulo (- i 1) n)))
           ((not (symbol? (vector-ref v i))) (list y))
           (vector-set! v i '()))
         (list y)))
      (() '())
      (else (loop (+ j 1))))))

;;; Deletes the pair with given key. Does nothing if it didn't exist. Returns
;;; the hashmap itself

(define (hashmap-delete! m k) (hashmap-delete!-opt m k) m)

;;; Deletes the pair with given key and returns the value at it. Errors if it
;;; didn't exist

(define (hashmap-remove! m k) (car (hashmap-delete!-opt m k)) m)
