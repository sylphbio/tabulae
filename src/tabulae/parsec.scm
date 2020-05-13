(module tabulae.parsec *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.pretty-print)
(import srfi-4)

(import (prefix srfi-1 srfi1-))
(import tabulae.base)
(import tabulae.monad)

; XXX move these into monad when I impl stuff like this
(define <maybe>-mzero (<maybe>-fail))
(define (<parser>-mzero _) <maybe>-mzero)

; mplus defined as alternative, not concatination
(define <parser>-mplus
  (lambda (p) (lambda (q) (lambda (ll)
    (let ((r (p ll)))
         (if (not (equal? r <maybe>-mzero)) r (q ll)))))))

; n-ary alternative operator
(define (<?> a . bs)
  (define ret (foldl (lambda (acc i) ((<parser>-mplus acc) i)) a bs))
  ret)

; parser that consumes one token, else fails
(define (item ll)
  (if (= (length* ll) 0)
      <maybe>-mzero
      (<maybe>-return `(,(head* ll) . ,(tail* ll)))))

; parser that succeeds when nothing to consume
(define (eom ll)
  (if (= (length* ll) 0)
      (<maybe>-return `(,(zero* ll) . ,(zero* ll)))
      <maybe>-mzero))

; parser that matches one token to a predicate
(define (sat p?)
  (do/m <parser>
    (i <- item)
    (if (p? i)
        (return i)
        <parser>-mzero)))

; parser that matches one token to a particular value
(define (value v)
  (sat (lambda (i) (equal? i v))))

; parser for numeric in u8 range
(define byte
  (sat (lambda (b) (and (fixnum? b) (>= b 0) (< b 256)))))

; handful of trivial char parsers
(define char (sat char?))
(define alpha (sat char-alphabetic?))
(define num (sat char-numeric?))
(define alphanum (sat (lambda (c) (or (char-alphabetic? c) (char-numeric? c)))))
(define upper (sat char-upper-case?))
(define lower (sat char-lower-case?))
(define whitespace (sat char-whitespace?))

; slightly nontrivial
(define hex
  (do/m <parser>
    (c <- char)
    (cond ((char-numeric? c) (return c))
          ((and (char>=? c #\a) (char<=? c #\f)) (return c))
          ((and (char>=? c #\A) (char<=? c #\F)) (return (integer->char (+ (char->integer c) #x20))))
          (else <parser>-mzero))))

; these are purely so vim paren-matching works
(define lparen (value #\())
(define rparen (value #\)))
(define lsbracket (value #\[))
(define rsbracket (value #\]))
(define lcbracket (value #\{))
(define rcbracket (value #\}))

; parser that matches a given listlike sequence token-for-token
(define (listlike ll)
  (if (= (length* ll) 0)
      (<parser>-return (zero* ll))
      (do/m <parser>
        (value (head* ll))
        (listlike (tail* ll))
        (return ll))))

; combinator that maps over a listlike
(define (over p) (lambda (ll)
  (if (null*? ll)
      (<parser>-return (zero* ll))
  (if (not (listlike? (head* ll)))
      <maybe>-mzero
      (do/m <maybe>
        (r <- (p (head* ll)))
        (rr <- ((over p) (tail* ll)))
        (return (cons (cons* (car r)
                             (car rr))
                      (cdr rr))))))))

; combinator that applies inside one level of list nesting
(define (into p) (lambda (ll)
  (if (or (null*? ll) (not (listlike? (head* ll))))
      <maybe>-mzero
      (do/m <maybe>
        (r <- (p (head* ll)))
        (return (cons (car r)
                      (tail* ll)))))))

; combinator, zero-or-more
(define (many p) (lambda (ll)
  (let ((m (p ll)))
       (if (equal? m <maybe>-mzero)
           (<maybe>-return `(,(zero* ll) . ,ll))
           (do/m <maybe>
             (r <- m)
             (rr <- ((many p) (cdr r)))
             (return (cons (cons* (car r)
                                  (car rr))
                           (cdr rr))))))))

; combinator, one-or-more
(define (many1 p) (lambda (ll)
  (do/m <maybe>
    (r <- ((many p) ll))
    (if (null*? (car r))
        <maybe>-mzero
        (return r)))))

; combinator, matches exactly n times then stops
(define (precisely n p) (lambda (ll)
  (if (= n 0)
      (<maybe>-return `(,(zero* ll) . ,ll))
      (do/m <maybe>
        (r <- (p ll))
        (rr <- ((precisely (- n 1) p) (cdr r)))
        (return (cons (cons* (car r)
                             (car rr))
                      (cdr rr)))))))

; combinator, zero-or-more that fails on mismatch
(define (exclusively p) (lambda (ll)
  (if (null*? ll)
      (<parser>-return (zero* ll))
      (do/m <maybe>
        (r <- (p ll))
        (rr <- ((exclusively p) (cdr r)))
        (return (cons (cons* (car r)
                             (car rr))
                      (cdr rr)))))))

; combinator, zero-or-one
(define (perhaps p) (lambda (ll)
  (let* ((m (p ll)))
       (if (equal? m <maybe>-mzero)
           (<maybe>-return `(,(zero* ll) . ,ll))
           (do/m <maybe>
             (r <- m)
             (return (cons ((if (listlike? (car r)) append* cons*) (car r) (zero* ll))
                           (cdr r))))))))

; combinator, zero-or-more, collects as list
; XXX I should maybe have a generic impl and wrappers
(define (many-l p) (lambda (ll)
  (let ((m (p ll)))
       (if (equal? m <maybe>-mzero)
           (<maybe>-return `(() . ,ll))
           (do/m <maybe>
             (r <- m)
             (rr <- ((many-l p) (cdr r)))
             (return (cons (cons (car r)
                                 (car rr))
                           (cdr rr))))))))

; combinator, one-or-more, collects as list
(define (many1-l p) (lambda (ll)
  (do/m <maybe>
    (r <- ((many-l p) ll))
    (if (null*? (car r))
        <maybe>-mzero
        (return r)))))

)
