#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../sources/include/cs151-core.rkt")

(: large-int (-> Integer Integer Integer Integer))
;; Return the largest integer out of the three integers
(define (large-int a b c)
  (cond
    [(and (>= a b) (>= a c)) a]
    [(and (>= b a) (>= b c)) b]
    [(and (>= c a) (>= c b)) c]
    [else (error "ERROR")]))
(check-expect (large-int 1 2 3) 3)
(check-expect (large-int 5 5 5) 5)


(: small-int (-> Integer Integer Integer Integer))
;; Return the smallest integer out ofthe three inputs
(define (small-int a b c)
  (cond
    [(and (<= a b) (<= a c)) a]
    [(and (<= b a) (<= b c)) b]
    [(and (<= c a) (<= c b)) c]
    [else (error "ERROR")]))
(check-expect (small-int 1 2 3) 1)
(check-expect (small-int 0 0 0) 0)


(: med-int (-> Integer Integer Integer Integer))
;;Return the median among the three integers
(define (med-int a b c)
  (cond
    [(and (not (= (small-int a b c) a)) (not (= (large-int a b c) a))) a]
    [(and (not (= (small-int a b c) b)) (not (= (large-int a b c) b))) b]
    [(and (not (= (small-int a b c) c)) (not (= (large-int a b c) c))) c]
    [else a]))
(check-expect (med-int 1 2 3) 2)
(check-expect (med-int 10 10 10) 10)


(: py-tri (-> Integer Integer Integer Boolean))
;;Check if the three integer is a phthagorean triple
(define (py-tri a b c)
  (cond
    [(= (* (large-int a b c) (large-int a b c)) (+ (* (med-int a b c) (med-int a b c)) (* (small-int a b c) (small-int a b c)))) #t]
    [else #f]))
(check-expect (py-tri 5 12 13) #t)
(check-expect (py-tri 1 2 3) #f)


(: ex-or (-> Boolean Boolean Boolean))
;;Check if the two input boolean values are the same
(define (ex-or p q)
  (cond
    [(and p q) #f]
    [(and (not p) (not q)) #f]
    [else #t]))
(check-expect (ex-or #t #t) #f)
(check-expect (ex-or #t #f) #t)
(check-expect (ex-or #f #f) #f)


(: leap-year (-> Integer Boolean))
;;Check if the input is a leap year
(define (leap-year y)
  (cond
    [(integer? (/ y 4)) #t]
    [else #f]))
(check-expect (leap-year 2020) #t)
(check-expect (leap-year 1999) #f)


(test)
