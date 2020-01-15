#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct TaxReturn
  ([num-adults : Integer]
   [num-children : Integer]
   [num-dogs : Integer]
   [income : Integer]
   [charity : Integer]))


(: adjusted-income : TaxReturn -> Integer)
;;calculate the adjusted income of the family
(define (adjusted-income T)
  (cond
    [(> (TaxReturn-income T) (TaxReturn-charity T))
     (- (TaxReturn-income T) (TaxReturn-charity T))]
    [(= (TaxReturn-income T) (TaxReturn-charity T)) 0]
    [else (error "not plausible")]))
(check-expect (adjusted-income (TaxReturn 2 3 1 60000 5000)) 55000)

(: round : Real -> Integer)
;;rounding real number to the closest integer
(define
(: income-tax : Integer -> Integer)
;;determine the income tax based on adjusted income but before any deductible
(define (income-tax t)
  (cond
    [(< t 10000) 0]
    [(and (> t 10000) (< t 20000))
      (* (- t 10000) 0.1)]
    [(and (> t 20000) (< t 30000))
      (+ (* (- t 20000) 0.2) 1000)]
    [(and (> t 30000) (< t 40000))
      (+ (* (- t 30000) 0.3) 3000)]
    [(> t 40000) (+ (* (- t 40000) 0.4) 6000)]
    [else (error "not plausible")]))

(check-expect (income-tax 55000) 12000)
;(: child-deduction : TaxReturn -> Integer)
;;compute the deduction based on the number of child
  
;(: plausible? : TaxReturn -> Boolean)
;; determine if the tax return is reasonable
;; i.e. it is not negative and there is atleast one adult
;(define (plausible? T)
  ;(cond
   ; [(> (TaxReturn-num-adults T) 0)]
    ;[(> (- (TaxReturn-income T) (TaxReturn-charity T)) 0)]


(test)