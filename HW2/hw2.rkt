#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")

(define-struct Point
  ([x : Real]
   [y : Real]))

(define-struct Quad
  ([a : Real]
   [b : Real]
   [c : Real]))


(: point$ : Point -> String)
;; build a string of the form "(2,3)"
(define (point$ p)
  (match p
    [ _ (cat "(" (num$ (Point-x p))
             "," (num$ (Point-y p)) ")")]))

(check-expect (point$ (Point 2 3)) "(2,3)")


(: quad$ : Quad -> String)
;; build a string of the form "2x^2+5x+1"
(define (quad$ q)
  (match q
    [ _ (cat (num$ (Quad-a q)) "x^2+" 
             (num$ (Quad-b q)) "x+"
             (num$ (Quad-c q)))]))

(check-expect (quad$ (Quad -2 1 3)) "-2x^2+1x+3")

(: +x : Real Point -> Point)
;; move the point in the x direction
(define (+x a p)
  (match p
    [ _ (Point (+ (Point-x p) a) (Point-y p))]))

(check-expect (+x 2 (Point 3 4)) (Point 5 4))
  
(: +y : Real Point -> Point)
;; move the point in the y direction
(define (+y a p)
  (match p
    [ _ (Point (Point-x p) (+ (Point-y p) a))]))

(check-expect (+y 2 (Point 3 4)) (Point 3 6))


(: distance : Point Point -> Real)
;; compute the Euclidean distance between the points
(define (distance p q)
  (sqrt (+ (sqr (- (Point-x p) (Point-x q)))
           (sqr (- (Point-y p) (Point-y q))))))
  
(check-expect (distance (Point 0 0) (Point 3 4)) 5)


(: midpoint : Point Point -> Point)
;; compute the midpoint between the points
(define (midpoint p q)
  (Point (/ (+ (Point-x p) (Point-x q)) 2)
         (/ (+ (Point-y p) (Point-y q)) 2)))

(check-expect (midpoint (Point 0 0) (Point 4 4)) (Point 2 2))

(: inside-unit-circle? : Point -> Bool)
;; test whether the point lies inside the unit circle
(define (inside-unit-circle? p)
  (if (< (+ (sqr (Point-x p)) (sqr (Point-y p))) 1) #t #f))

(check-expect (inside-unit-circle? (Point 0.5 0.5)) #t)
(check-expect (inside-unit-circle? (Point 2 3)) #f)


(: discriminant : Quad -> Real)
;; compute the discriminant of the quadratic equation
(define (discriminant q)
  (- (sqr (Quad-b q)) (* 4 (Quad-a q) (Quad-c q))))

(check-expect (discriminant (Quad 1 2 3)) -8)


(: num-real-solutions : Quad -> Int)
;; compute whether there are 0, 1, or 2 solutions
(define (num-real-solutions q)
  (cond
    [(> (discriminant q) 0) 2]
    [(= (discriminant q) 0) 1]
    [else  0]))

(check-expect (num-real-solutions (Quad 1 2 3)) 0)
(check-expect (num-real-solutions (Quad 1 0 0)) 1)
(check-expect (num-real-solutions (Quad 1 0 -1)) 2)

(: concave-up? : Quad -> Bool)
;; test whether the parabola is concave up
(define (concave-up? q)
  (cond
    [(> (Quad-a q) 0) #t]
    [else #f]))

(check-expect (concave-up? (Quad 1 2 3)) #t)
(check-expect (concave-up? (Quad -1 2 3)) #f)


(: value-at : Real Quad -> Point)
;; given an x value and a quadratic, find the point at that x
(define (value-at a q)
  (Point a (+ (* (Quad-a q) (sqr a)) (* (Quad-b q) a) (Quad-c q))))

(check-expect (value-at 1 (Quad 1 1 1)) (Point 1 3))
(check-expect (value-at 2 (Quad 1 2 3)) (Point 2 11))

(: vertex : Quad -> Point)
;; find the vertex of the parabola
(define (vertex q)
  (Point (/ (Quad-b q) (Quad-a q) -2)
         (- (Quad-c q) (/ (sqr (Quad-b q)) 4 (Quad-a q)))))

(check-expect (vertex (Quad 1 0 0)) (Point 0 0))
(check-expect (vertex (Quad 3 12 -12)) (Point -2 -24))


(: vertical-distance : Point Quad -> Real)
;; measure the vertical distance between the point and the parabola
;; Since I don't know how is the distance defined
;;(i.e. (point - parabola) or (parabola - point), so I return the value in abs 
(define (vertical-distance p q)
  (abs (- (Point-y p) (Point-y (value-at (Point-x p) q)))))

(check-expect (vertical-distance (Point 0 2) (Quad 1 0 0)) 2)
(check-expect (vertical-distance (Point 9 99) (Quad 1 0 0)) 18) 


(: lies-on? : Point Quad -> Bool)
;; test whether the point lies on the parabola
(define (lies-on? p q)
  (match (vertical-distance p q)
    [ 0 #t]
    [ _ #f]))

(check-expect (lies-on? (Point 0 0) (Quad 1 0 0)) #t)
(check-expect (lies-on? (Point 0 200) (Quad 1 0 0)) #f)


(: intersect? : Quad Quad -> Bool)
;; test whether the two parabolas intersect
(define (intersect? q1 q2)
  (cond
    [(= (- (Quad-a q1) (Quad-a q2)) 0)
     (if (not (= (- (Quad-b q1) (Quad-b q2)) 0)) #t
         (if (= (- (Quad-c q1 ) (Quad-c q2)) 0) #t #f))]
    [(>= (discriminant (Quad (- (Quad-a q1) (Quad-a q2))
                             (- (Quad-b q1) (Quad-b q2))
                             (- (Quad-c q1) (Quad-c q2)))) 0) #t]
    [else #f]))

(check-expect (intersect? (Quad 1 0 0) (Quad 1 0 5)) #f)
(check-expect (intersect? (Quad 1 2 3) (Quad 1 0 3)) #t)

(test)