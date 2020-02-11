#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Point
  ([x : Real]
   [y : Real]))

(define-type Dataset
  (Listof Point))

(define-struct Line
  ([slope : Real] 
   [intercept : Real]))

(define-struct Analysis
  ([best-fit : Line]
   [r-squared : Real]))


(: sum-data : (Listof Real) -> Real)
;;define a sumationa function that sums all the values of a real list
(define (sum-data s)
  (match s
    [ '() 0]
    [(cons head tail) (+ head (sum-data tail))]))

(check-expect (sum-data (list 1 2 3 5)) 11)
(check-expect (sum-data (list 1 1 1 1 )) 4)



(: prod-xy : Point -> Real)
;;given a point, return the product of the x, y
(define (prod-xy p)
  (* (Point-x p) (Point-y p)))

(check-expect (prod-xy (Point 3 4)) 12)
(check-expect (prod-xy (Point 0 0)) 0)



(: list-x : Dataset -> (Listof Real))
;;generate a list of real values based on the x component of a given dataset
(define (list-x set)
  (map (lambda (point) (Point-x point)) set))

(check-expect (list-x (list (Point 1 2) (Point 2 3) (Point 3 4))) (list 1 2 3))
(check-expect (list-x (list (Point 1 1) (Point 1 1) (Point 1 1))) (list 1 1 1))



(: list-y : Dataset -> (Listof Real))
;;generate a list of real values based on the y component of a given dataset 
(define (list-y set)
  (map (lambda (point) (Point-y point)) set))

(check-expect (list-y (list (Point 1 2) (Point 2 3) (Point 3 4))) (list 2 3 4))
(check-expect (list-y (list (Point 1 1) (Point 1 1) (Point 1 1))) (list 1 1 1))



(: list-x-sq : Dataset -> (Listof Real))
;;generate the list of x^2 based on the points from a given dataset
(define (list-x-sq set)
  (map sqr (list-x set)))

(check-expect
 (list-x-sq (list (Point 1 2) (Point 2 3) (Point 3 4))) (list 1 4 9))
(check-expect
 (list-x-sq (list (Point 1 1) (Point 1 1) (Point 1 1))) (list 1 1 1))



(: sum-xy : Dataset -> Real)
;;sum up the values of the xy product list based on a given dataset
(define (sum-xy s)
  (sum-data (map prod-xy s)))
  
(check-expect
 (sum-xy (list (Point 3 4) (Point 1 2) (Point 0 0) (Point 3 4))) 26)
(check-expect
 (sum-xy (list (Point 1 1) (Point 1 1) (Point 1 1))) 3)



(: sum-x : Dataset -> Real)
;;sum up the values of the x component based on a given dataset
(define (sum-x s)
  (sum-data (list-x s)))

(check-expect (sum-x (list (Point 1 2) (Point 2 3) (Point 3 4))) 6)
(check-expect (sum-x (list (Point 1 1) (Point 1 1) (Point 1 1))) 3)



(: sum-y : Dataset -> Real)
;;sum up the values of the y component based on a given dataset
(define (sum-y s)
  (sum-data (list-y s)))

(check-expect (sum-y (list (Point 1 2) (Point 2 3) (Point 3 4))) 9)
(check-expect (sum-y (list (Point 1 1) (Point 1 1) (Point 1 1))) 3)



(: list-mean : (Listof Real) -> Real)
;;compute the mean value of a list
(define (list-mean l)
  (/ (sum-data l) (length l)))

(check-expect (list-mean (list 1 2 3 4 5)) 3)
(check-expect (list-mean (list 1 1 1)) 1)



(: reg-slope : Dataset -> Real)
;;compute the slope of linear regression line based on a given dataset
(define (reg-slope set)
  (/ (- (* (length set) (sum-xy set)) (* (sum-x set) (sum-y set)))
     (- (* (length set) (sum-data (list-x-sq set))) (sqr (sum-x set)))))

(check-within (reg-slope
               (list (Point 1 2) (Point 4 6) (Point 7 9)
                     (Point 11 8) (Point 19 22))) 1.0256 0.001)
(check-expect (reg-slope (list (Point 1 1) (Point 2 2) (Point 3 3))) 1)



(: reg-intercept : Dataset -> Real)
;;compute the intercept of a linear regression based on the given dataset
(define (reg-intercept set)
  (/ (- (* (sum-y set) (sum-data (list-x-sq set)))
        (* (sum-x set) (sum-xy set)))
     (- (* (length set) (sum-data (list-x-sq set)))
        (sqr (sum-x set)))))

(check-within (reg-intercept
               (list (Point 1 2) (Point 4 6) (Point 7 9)
                     (Point 11 8) (Point 19 22))) 0.7848 0.001)
(check-expect (reg-intercept (list (Point 1 1) (Point 2 2) (Point 3 3))) 0)



(: stdev : (Listof Real) -> Real)
;;compute the standard deviation of a given list of real number
(define (stdev set)
  (sqrt (- (/ (sum-data (map sqr set)) (length set))
           (sqr (list-mean set)))))

(check-within (stdev (list 1 2 3 4 5)) 1.414 0.001)
(check-within (stdev (list 1 1 1 1 1)) 0 0.001) 



(: cor-eff : Dataset -> Real)
;;compute the correlation factor of a given dataset
(define (cor-eff set)
  (/ (- (/ (sum-xy set) (length set))
        (* (/ (sum-x set) (length set)) (/ (sum-y set) (length set))))
     (* (stdev (list-x set)) (stdev (list-y set)))))

(check-within (cor-eff
               (list (Point 1 2) (Point 4 6) (Point 7 9)
                     (Point 11 8) (Point 19 22))) 0.9506 0.001)
(check-within (cor-eff (list (Point 1 1) (Point 2 2) (Point 3 3))) 1 0.001)



(: linreg : Dataset -> Analysis)
;; compute the linear regression and relavant parameter
;; based on a given dataset
(define (linreg set)
  (Analysis (Line (reg-slope set) (reg-intercept set))
            (cor-eff set)))

(check-within (linreg
               (list (Point 1 2) (Point 4 6) (Point 7 9)
                     (Point 11 8) (Point 19 22)))
              (Analysis (Line 1.0256 0.7848) 0.9506) 0.001)
(check-within (linreg
               (list (Point 1 1) (Point 2 2) (Point 3 3)))
              (Analysis (Line 1 0) 1) 0.001)



(test)


  