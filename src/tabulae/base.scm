(module tabulae.base *

(import scheme)
(import chicken.base)
(import chicken.type)
(import chicken.string)
(import chicken.format)
(import chicken.foreign)
(import chicken.irregex)
(import chicken.process)
(import chicken.process-context)
(import chicken.port)
(import chicken.file)
(import chicken.io)
(import srfi-4)

(import (prefix srfi-1 srfi1:))

(define (listlike? ll)
  (or (list? ll) (string? ll) (u8vector? ll)))

(define (byte? v)
  (and (fixnum? v) (>= v 0) (< v 256)))

; fake-polymorphic "listlike" functions
; so I can apply the parsers to strings and bytes alike
; XXX consider making head/tail/etc total
; all my listlikes are at least monoidal functors
; traversable too. so I can define fmap and fold
; zero and append I already have, tho if I can unify the syntax with my mplus shit that'd be cool
; I'm being a bit fast and loose with terms because string/bytevec are monomorphic
; but they're basically specific degenerate lists and should have all the same structural functions
; I want to eventually define everything I like in the Data.List repitoire for listlikes
; and then drop the asterisks and replace the functions in scheme/base
; XXX also everything cleanly admits a ->list function
; XXX functions to add: chop, last
; XXX want to support vectors. also I realized my thing is actually like, stringlike and listlike
; the difference is between like, a structure of containers whose types cannot change, vs one which can
; so you could actually have a string map, but it must be a function Char -> Char) *not* a -> b
; maybe call this sequential and traversible? idk it doesn't matter until I have real typeclasses
(define (length* ll)
  (cond ((list? ll) (length ll))
        ((string? ll) (string-length ll))
        ((u8vector? ll) (u8vector-length ll))
        (else (error "no length* impl for type" ll))))

; XXX change these to return maybes and have bang versions once I drop the star
(define (index* i ll)
  (cond ((list? ll) (list-ref ll i))
        ((string? ll) (string-ref ll i))
        ((u8vector? ll) (u8vector-ref ll i))
        (else (error "no index* impl for type" ll))))

(define (head* ll) (index* 0 ll))
(define (first* ll) (index* 0 ll))
(define (second* ll) (index* 1 ll))
(define (third* ll) (index* 2 ll))
(define (fourth* ll) (index* 3 ll))
(define (fifth* ll) (index* 4 ll))
(define (sixth* ll) (index* 5 ll))
(define (seventh* ll) (index* 6 ll))
(define (eighth* ll) (index* 7 ll))
(define (ninth* ll) (index* 8 ll))
(define (tenth* ll) (index* 9 ll))

(define (tail* ll)
  (cond ((list? ll) (cdr ll))
        ((string? ll) (substring ll 1))
        ((u8vector? ll) (subu8vector ll 1 (u8vector-length ll)))
        (else (error "no tail* impl for type" ll))))

(define (cons* v ll)
  (cond ((list? ll) (cons v ll))
        ((and (char? v) (string? ll)) (conc (->string v) ll))
        ; XXX do this less ugly, I'd rather not use set! but also don't wnat multiple copies
        ((and (byte? v) (u8vector? ll)) (list->u8vector (cons v (u8vector->list ll))))
        (else (error "no cons* impl for types" v ll))))

(define (null*? ll)
  (= (length* ll) 0))

; I now have two competing monadplus syntaxes, kill me
(define (zero* ll)
  (cond ((list? ll) '())
        ((string? ll) "")
        ((u8vector? ll) (make-u8vector 0))
        (else (error "no zero* impl for type" ll))))

(define (append* lla llb)
  (cond ((and (list? lla) (list? llb)) (append lla llb))
        ((and (string? lla) (string? llb)) (string-append lla llb))
        ; XXX as with cons*, this should be one realloc and one memcpy
        ((and (u8vector? lla) (u8vector? llb) (list->u8vector (append (u8vector->list lla) (u8vector->list llb)))))
        (else (error "no append* impl for types" lla llb))))

; this is very inefficient
(define (reverse* ll)
  (if (null*? ll)
      (zero* ll)
      (append* (reverse* (tail* ll)) (cons* (head* ll) (zero* ll)))))

(define (take* n ll)
  (cond ((= n 0) (zero* ll))
        ((null*? ll) (zero* ll))
        (else (cons* (head* ll) (take* (- n 1) (tail* ll))))))

(define (drop* n ll)
  (cond ((= n 0) ll)
        ((null*? ll) (zero* ll))
        (else (drop* (- n 1) (tail* ll)))))

; lol lazy
(define (take-right* n ll) (reverse* (take* n (reverse* ll))))
(define (drop-right* n ll) (reverse* (drop* n (reverse* ll))))

(define (find* p? ll)
  (cond ((null*? ll) #f)
        ((p? (head* ll)) (head* ll))
        (else (find* p? (tail* ll)))))

(define (findi* p? ll)
  (define (f^ p? ll n)
    (cond ((null*? ll) #f)
          ((p? (head* ll)) n)
          (else (f^ p? (tail* ll) (+ n 1)))))
  (f^ p? ll 0))

(define (filter* p? ll)
  (cond ((null*? ll) (zero* ll))
        ((p? (head* ll)) (cons* (head* ll) (filter* p? (tail* ll))))
        (else (filter* p? (tail* ll)))))

; XXX TODO FIXME using pairs is fundamentally a mistake and this should be a two-item list
(define (zip* ll1 ll2)
  (if (or (null*? ll1) (null*? ll2))
      '()
      (cons `(,(head* ll1) . ,(head* ll2)) (zip* (tail* ll1) (tail* ll2)))))

(define (any* p? ll)
  (cond ((null*? ll) #f)
        ((p? (head* ll)) #t)
        (else (any* p? (tail* ll)))))

(define (all* p? ll)
  (cond ((null*? ll) #t)
        ((p? (head* ll)) (all* p? (tail* ll)))
        (else #f)))

; I don't use pair because having a pair of two lists confuses scheme
; because it also represents a list where the first element is a list
(define (partition* p? ll)
  (letrec ((partition^ (lambda (ll ts fs)
             (cond ((null*? ll) `(,(reverse* ts) ,(reverse* fs)))
                   ((p? (head* ll)) (partition^ (tail* ll) (cons* (head* ll) ts) fs))
                   (else (partition^ (tail* ll) ts (cons* (head* ll) fs)))))))
          (partition^ ll '() '())))

; keeping star just for consistency ig
(define iota* srfi1:iota)

; XXX fix these to actually work across listlikes lol
; also obviously make them more efficient if/when we have an ord typeclass
; actually I should probably just have an eq typeclass that uses the most efficient value equality for a type
; and a separate function like same-object? that eqs everything
; XXX we follow haskell's convention that sortBy takes a binary function to Ordering for items without an Ord constraint
; whereas sortWith takes a unary function that takes an item to something with an Ord constraint
; XXX be very careful to inspect callers if I do an nlogn version, I (wrongly) depend in various places on order preservation
(define (nub* ll) (srfi1:delete-duplicates ll))
(define (nub-by* cmp? ll) (srfi1:delete-duplicates ll cmp?))
(define (union* ll . lls) (apply srfi1:lset-union `(,equal? ,(nub* ll) ,@lls)))
(define (union-by* cmp? ll . lls) (apply srfi1:lset-union `(,cmp? ,(nub-by* cmp? ll) ,@lls)))
(define (intersect* ll . lls) (apply srfi1:lset-intersection `(,equal? ,(nub* ll) ,@lls)))
(define (intersect-by* cmp? ll . lls) (apply srfi1:lset-intersection `(,cmp? ,(nub-by* cmp? ll) ,@lls)))
(define (difference* ll . lls) (apply srfi1:lset-difference `(,equal? ,(nub* ll) ,@lls)))
(define (difference-by* cmp? ll . lls) (apply srfi1:lset-difference `(,cmp? ,(nub-by* cmp? ll) ,@lls)))

(define (curry* f)
  (lambda (a) (lambda (b) (f a b))))

(define (uncurry* f)
  (lambda (a b) ((f a) b)))

; zzz I'm getting lazy
(define (<> a . bs)
  (foldl append* a bs))

; rename reexport, so I don't need to import chicken.irregex anywhere
(define regex irregex)

; takes compiled regex and string
; returns alist with 0 the full match and 1..n all captures
(define (regex-capture r s)
  (let ((m (irregex-match r s)))
       (if m
           (map (lambda (n) (cons n (irregex-match-substring m n)))
                (srfi1:iota (+ (irregex-match-num-submatches m) 1)))
           '())))

; chicken only has seconds since epoch or milliseconds since boot
; and srfi-19 has ten million dependencies
(define current-microseconds (foreign-lambda* unsigned-integer64 () #<<EOM
#include <time.h>
struct timespec t;
clock_gettime(CLOCK_REALTIME, &t);
C_return(t.tv_sec * 1000000 + t.tv_nsec / 1000);
EOM
))

; similar to process-run: with args treats cmd as a binary to invoke, without args treats it as a shellout
; also vars is an alist to match the chicken functions
; unlike process-run, we *always* use execve rather than execvp after the fork
; we *merge* environments rather than overwrite 
; and we *always* use /bin/sh rather than ever going through the user shell
; together the goal is that process-create covers all uses and behaves much more uniformly than process-run
(define (process-create cmd #!optional args (vars '()))
  (let ((env (foldl (lambda (acc e) (alist-update (car e) (cdr e) acc equal?))
                    (get-environment-variables)
                    vars))
        (pid (process-fork)))
       (cond ((not (= pid 0)) pid)
             (args (process-execute cmd args env))
             (else (process-execute "/bin/sh" `("-c" ,cmd) env)))))

; very simple, writes a string to disk truncating on open
; XXX nice thing would be to write to a temp file and relink but need to decide how to impl mv
(define (save-file path str)
  (call-with-output-file path (lambda (p) (write-string str #f p))))

; reads a file into a string and returns it, properly handling the empty file
(define (load-file path)
  (and (file-exists? path)
       (file-readable? path)
       (let ((s (call-with-input-file path (lambda (p) (read-string #f p)))))
            (if (eof-object? s) "" s))))

)
