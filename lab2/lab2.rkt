#lang typed/racket
(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct TaxReturn
  ([num-adults : Integer]
   [num-children : Integer]
   [num-dogs : Integer]
   [income : Integer]
   [charity : Integer]))


(: plausible? : TaxReturn -> Boolean)
;; determine if the tax return is reasonable
;; i.e. all quantities are not negative and there is atleast one adult
(define (plausible? T)
  (cond
    [(and (> (TaxReturn-num-adults T) 0)
          (>= (TaxReturn-num-children T) 0)
          (>= (TaxReturn-num-dogs T) 0)
          (>= (TaxReturn-income T) 0)
          (>= (TaxReturn-charity T) 0)) #t]
    [else #f]))

(check-expect (plausible? (TaxReturn 2 3 1 60000 5000)) #t)
(check-expect (plausible? (TaxReturn -1 3 1 60000 5000)) #f)
(check-expect (plausible? (TaxReturn 2 -3 1 60000 5000)) #f)
(check-expect (plausible? (TaxReturn 2 3 -1 60000 5000)) #f)
(check-expect (plausible? (TaxReturn 2 3 1 -60000 5000)) #f)
(check-expect (plausible? (TaxReturn 2 3 1 60000 -5000)) #f)


(: adjusted-income : TaxReturn -> Integer)
;;calculate the adjusted income of the family, if the charity is greater than
;;income, then the adjusted income is zero
(define (adjusted-income T)
  (cond
    [(> (TaxReturn-income T) (TaxReturn-charity T))
     (- (TaxReturn-income T) (TaxReturn-charity T))]
    [else 0]))

(check-expect (adjusted-income (TaxReturn 2 3 1 60000 5000)) 55000)
(check-expect (adjusted-income (TaxReturn 2 3 1 60000 60000)) 0)
(check-expect (adjusted-income (TaxReturn 2 3 1 60000 80000)) 0)

(: income-tax : Integer -> Integer)
;;determine the income tax based on adjusted income but before any deductible
;; the cut off of each interval is 0~10000, 10001~20000 ... etc
(define (income-tax t)
  (cond
    [(< t 0) (error "not plausible")]
    [(< t 10000) 0]
    [(and (> t 10000) (<= t 20000))
      (exact-round (* (- t 10000) 0.1))]
    [(and (> t 20000) (<= t 30000))
      (exact-round (+ (* (- t 20000) 0.2) 1000))]
    [(and (> t 30000) (<= t 40000))
      (exact-round(+ (* (- t 30000) 0.3) 3000))]
    [else (exact-round (+ (* (- t 40000) 0.4) 6000))]))
    

(check-expect (income-tax 55000) 12000)
(check-expect (income-tax 9999) 0)
(check-expect (income-tax 20000) 1000)
(check-expect (income-tax 35000) 4500)
(check-error (income-tax -40000) "not plausible")


(: child-deduction : TaxReturn -> Integer)
;;compute the deduction based on the number of child
(define (child-deduction T)
  (cond
    [(< (TaxReturn-num-children T) 0) (error "not plausible")]
    [(= (TaxReturn-num-children T) 0) 0]
    [(>= (TaxReturn-num-children T) 12) (income-tax (adjusted-income T))]
    [else
     (exact-round (* (income-tax (adjusted-income T))
                     (* 1/12 (TaxReturn-num-children T))))]))

(check-expect (child-deduction (TaxReturn 2 2 1 60000 38000)) 233)
(check-expect (child-deduction (TaxReturn 2 15 1 60000 38000)) 1400)
(check-expect (child-deduction (TaxReturn 2 0 1 60000 38000)) 0)
(check-error (child-deduction (TaxReturn 2 -5 1 60000 38000)) "not plausible")


(: dog-deduction : TaxReturn -> Integer)
;;Compute the deduction based on the number of dogs
(define (dog-deduction T)
  (cond
    [(> (* (TaxReturn-num-dogs T) 100) (income-tax (adjusted-income T)))
     (income-tax (adjusted-income T))]
    [(>= (TaxReturn-num-dogs T) 0)
     (* (TaxReturn-num-dogs T) 100)]
    [else (error "not plausible")]))

(check-expect (dog-deduction (TaxReturn 2 3 100 60000 38000)) 1400)
(check-expect (dog-deduction (TaxReturn 2 3 0 60000 38000)) 0)
(check-expect (dog-deduction (TaxReturn 2 3 12 60000 38000)) 1200)
(check-error (dog-deduction (TaxReturn 2 3 -2 60000 38000)) "not plausible")


(: tax-owed : TaxReturn -> Integer)
;; Determine the total tax to be collected after deduction
(define (tax-owed T)
  (cond
    [(or (= (adjusted-income T) 0)
         (< (income-tax (adjusted-income T))
            (+ (dog-deduction T) (child-deduction T)))) 0]
    [else (- (income-tax (adjusted-income T))
             (dog-deduction T) (child-deduction T))]))

(check-expect (tax-owed (TaxReturn 2 3 1 60000 5000)) 8900)
(check-expect (tax-owed (TaxReturn 2 100 100 60000 5000)) 0)

(: effective-tax-rate : TaxReturn -> Exact-Rational)
;;compute the effective tax rate based on the taxreturn input by
;;the equation
;;effective-tax-rate = (tax-owed/adjusted-income)
(define (effective-tax-rate T)
  (cond
    [(>= (tax-owed T) 0)
     (/ (tax-owed T) (adjusted-income T))]
    [else (error "not plausible")]))

(check-within (effective-tax-rate (TaxReturn 2 3 1 60000 5000)) 0.16 0.1)


(test)