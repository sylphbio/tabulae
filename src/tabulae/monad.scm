(module tabulae.monad *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)

(import tabulae.base)

; trivial checked utility functions for maybe
(define (just? m) (and (list? m) (not (null? m)) (eq? (car m) 'Just)))
(define (nothing? m) (eq? m 'Nothing))
(define (maybe? m) (or (just? m) (nothing? m)))
(define (from-just m) (if (just? m) (cadr m) (error "from-just applied to non-just value" m)))

; maybe monad implementation
(define (<maybe>-return v) `(Just ,v))
(define (<maybe>-bind m) (lambda (f) (cond ((just? m) (f (from-just m)))
                                           ((nothing? m) 'Nothing)
                                           (else (error "bind applied to non-maybe value" m)))))
(define (<maybe>-fail) 'Nothing)
(define (<maybe>-fmap f) (lambda (m) (cond ((just? m) (<maybe>-return (f (from-just m))))
                                           ((nothing? m) 'Nothing)
                                           (else (error "fmap applied to non-maybe value" m)))))

; maybe return that fails on #f and only #f
(define (to-maybe v) (if v (<maybe>-return v) (<maybe>-fail)))

; trivial checked utility functions for either
(define (left? e) (and (list? e) (not (null? e)) (eq? (car e) 'Left)))
(define (right? e) (and (list? e) (not (null? e)) (eq? (car e) 'Right)))
(define (either? e) (or (left? e) (right? e)))
(define (from-right e) (if (right? e) (cadr e) (error "from-right applied to non-right value" e)))

; either monad implementation
(define (<either>-return v) `(Right ,v))
(define (<either>-bind m) (lambda (f) (cond ((right? m) (f (from-right m)))
                                            ((left? m) m)
                                            (else (error "bind applied to non-either value" m)))))
(define (<either>-fail e) `(Left ,e))
(define (<either>-fmap f) (lambda (m) (cond ((right? m) (<either>-return (f (from-right m))))
                                            ((left? m) m)
                                            (else (error "fmap applied to non-either value" m)))))

; either return that fails on #f and only #f
; I define conversions to eithers as macros to short-circuit the error case
(define-syntax to-either
  (syntax-rules ()
    ((to-either expression alternative)
     (let ((res expression))
          (if res
              (<either>-return res)
              (<either>-fail alternative))))))

; likewise as above
(define-syntax maybe->either
  (syntax-rules ()
    ((maybe->either expression alternative)
     (let ((res expression))
          (cond ((just? res) (<either>-return (from-just res)))
                ((nothing? res) (<either>-fail alternative))
                (else (error "maybe->either applied to non-maybe value" res)))))))

; straightforward
(define (either->maybe e)
  (cond ((right? e) (<maybe>-return (from-right e)))
        ((left? e) (<maybe>-fail))
        (else (error "either->maybe applied to non-either value" e))))

; parser monad. a parser is a function that accepts text (any listlike) and returns maybe pair: result/remaining text
; return v is the parser that unconditionally results in v and leaves text unchanged
; bind p f is the function that applies a parser to an input and a combinator to the result, producing a new parser
(define (<parser>-return v) (lambda (ll) (<maybe>-return `(,v . ,ll))))
(define (<parser>-bind p) (lambda (f) (lambda (ll) ((<maybe>-bind (p ll)) (lambda (r) ((f (car r)) (cdr r)))))))
(define (<parser>-fail _) (<maybe>-fail))
(define (<parser>-fmap _) (error "<parser>-fmap not implemented"))

; note this is a function on a parse result, not a parser
(define (parse->maybe p)
  ((<maybe>-bind p) (lambda (r) (<maybe>-return (car r)))))

; as above
(define-syntax parse->either
  (syntax-rules ()
    ((parse->either expression alternative)
     (let ((res expression))
          (cond ((and (just? res) (pair? (from-just res))) (<either>-return (car (from-just res))))
                ((nothing? res) (<either>-fail alternative))
                (else (error "parse->either applied to non-parse value" res)))))))

; monad implementation. this just resolves my overloaded function names and puts them in scope
; bind and fmap are curried, <$> is uncurried, >>= and <&> are n-ary
(define-syntax do/m
  (er-macro-transformer (lambda (e r c)
    (let* ((m (cadr e))
           (%let (r 'let))
           (%lambda (r 'lambda))
           (%foldl (r 'foldl))
           (return (symbol-append m '-return))
           (bind (symbol-append m '-bind))
           (fail (symbol-append m '-fail))
           (fmap (symbol-append m '-fmap)))
          `(,%let ((return ,return)
                   (bind ,bind)
                   (fail ,fail)
                   (fmap ,fmap)
                   (>>= (,%lambda (m . fs) (,%foldl (,%lambda (m f) ((,bind m) f)) m fs)))
                   (<$> (,%lambda (f m) ((,fmap f) m)))
                   (<&> (,%lambda (m . fs) (,%foldl (,%lambda (m f) ((,fmap f) m)) m fs))))
                  (do-monad ,@(cddr e)))))))

; this does the actual syntax transforms
; the three syntactic forms we allow are
; (x <- a)
; (declare y b)
; (c)
(define-syntax do-monad
  (syntax-rules (<- declare)
    ((do-monad final)
     final)
    ((do-monad (name <- val) next ...)
     ((bind val) (lambda (name) (do-monad next ...))))
    ((do-monad (declare name val) next ...)
     ((bind (return val)) (lambda (name) (do-monad next ...))))
    ((do-monad val next ...)
     ((bind val) (lambda (_) (do-monad next ...))))))

; XXX define this more principled later. in haskell it's typed (Traversable t, Monad m) => t (m a) -> m (t a)
; it would be cool to have traversable for other cons cell structures like trees if such a thing is feasible
(define (sequence ll)
  (cond ((any* nothing? ll) 'Nothing)
        ((any* left? ll) (find* left? ll))
        ((all* just? ll) `(Just ,(map from-just ll)))
        ((all* right? ll) `(Right ,(map from-right ll)))
        (else #f)))

)
