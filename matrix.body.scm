;;;; Matrix utilities (Library body)
;;;; Vectors are represented as lists, and matrices as lists of lists, where
;;;; each sublist corresponds to a row of the matrix. The operators whose names
;;;; overlap with standard ones are designed to be drop-in replacements for
;;;; them, extended to work with lists as vectors, and lists of lists as
;;;; matrices. If you give invalid input, the behavior is undefined

;;; Universal `+` for numbers, vectors, and matrices

(define +
  (case-lambda
    (() 0)
    ((x . xs)
     (if (number? x)
       (apply scheme+ x xs)
       (apply map + x xs)))))

;;; Universal `-` for numbers, vectors, and matrices

(define (- x . xs)
  (if (number? x)
    (apply scheme- x xs)
    (apply map - x xs)))

;;; Universal `-` for numbers, vectors, and matrices

(define (scale k x)
  (if (number? x)
    (scheme* k x)
    (map (lambda (y) (scale k y)) x)))

;;; Universal `*` for numbers, vectors, and matrices. Uses dot products for
;;; vectors

(define *
  (case-lambda
    (() 1)
    ((m . ms)
     (match (cons m (apply * ms))
       ((or (() . m) (m . ())) m)
       ((and (m . n) (((_ . _) . _) . ((_ . _) . _)))
        (map
          (lambda (row)
            (apply map (lambda col (* row col)) n))
          m))
       ((and (m . n) (((_ . _) . _) . (_ . _)))
        (map (lambda (row) (* row n)) m))
       ((and (m . n) ((_ . _) . ((_ . _) . _)))
        (apply map (lambda col (* m col)) n))
       ((and (m . n) ((_ . _) . (_ . _)))
        (apply scheme+ (map scheme* m n)))
       ((or (k . (and m (_ . _))) ((and m (_ . _)) . k))
        (scale k m))
       ((m . n) (scheme* m n))))))

;;; Universal `/` for numbers, vectors, and matrices. Left-associative, where
;;; `(/ A B)` is `(* (/ B) A)`, so that it can be used to solve matrix equations
;;; of form `AX = B`. Inverse is only defined for numbers and matrices, so
;;; vectors can only be used as the first argument in the multi-argument form

(define /
  (case-lambda
    ((m)
     (if (number? m)
       (scheme/ m)
       (let ((k (scheme/ (abs m)))
             (rows-range (iota (length m))))
         (map
           (lambda (j)
             (map
               (lambda (i)
                 (scheme*
                   k
                   (if (even? (scheme+ i j)) 1 -1)
                   (abs
                     (map
                       (lambda (row) (remove-index j row))
                       (remove-index i m)))))
               rows-range))
           (iota (length (car m)))))))
    ((m n) (* (/ n) m))
    (ms (reduce-left / ms))))

;;; Universal `abs` for numbers, vectors, and matrices. For vectors computes
;;; the length, for matrices the determinant

(define (abs m)
  (match m
    (() 1)
    (((_ . _) . _)
     (apply
       scheme+
       (map
         (lambda (i row)
           (scheme*
             (if (even? i) 1 -1)
             (car row)
             (abs (map cdr (remove-index i m)))))
         (iota (length m))
         m)))
    ((_ . _) (sqrt (* m m)))
    (else (scheme-abs m))))

;;; Constructs a transposed matrix

(define (transpose m) (apply map list m))
