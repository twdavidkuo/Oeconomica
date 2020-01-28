#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

;; ================================ data definitions

(define-type IBT
  (U 'IEmpty IntNode))

(define-struct IntNode
  ([root : Integer]
   [lsub : IBT]
   [rsub : IBT]))

(define-type RBT
  (U 'REmpty RationalNode))

(define-struct RationalNode
  ([root : Exact-Rational]
   [lsub : RBT]
   [rsub : RBT]))


(: sum-upto : Integer -> Integer)
;; Compute the sum from 0 up to the given integer.
;; Return 0 if the given integer is not positive.
(define (sum-upto n)
  (cond
    [(<= n 0) 0]
    [else (+ (sum-upto (- n 1)) n)]))

(check-expect (sum-upto 3) 6)
(check-expect (sum-upto 5) 15)
(check-expect (sum-upto 100) 5050)


(: sum-from : Integer Integer -> Integer)
;; Compute the sum in the inclusive interval from one integer to another.
;; ex: (sum-from 10 12) --> 33
;; The order of integers must not matter, 
;;   so (sum-from 12 10) must equal (sum-from 10 12).
;; Negatives and/or 0 are OK.
(define (sum-from a b)
  (cond
    [(= a b) a]
    [(> a b) (+ (sum-from a (+ b 1)) b)]
    [else (+ (sum-from (+ a 1) b) a)]))

(check-expect (sum-from 3 5) 12)
(check-expect (sum-from 5 3) 12)
(check-expect (sum-from -2 1) -2)
(check-expect (sum-from 1 -2) -2)


(: product-from : Integer Integer -> Integer)
;; Similar to sum-from, but with multiplication.
;; Negatives and/or 0 are OK.
(define (product-from a b)
  (cond
    [(= a b) (* b b)]
    [(< a b) (* (product-from (+ a 1) b) a)]
    [else (* (product-from a (+ b 1)) b)]))

(check-expect (product-from 1 3) 6)
(check-expect (product-from 3 1) 6)
(check-expect (product-from -4 -2) -24)
(check-expect (product-from -2 -4) -24)
(check-expect (product-from -5 2) 0)


(: tower : Integer Integer Integer Image-Color -> Image)
;; given: level width, level height, width delta, and color
;; each level of the tower must have a black outline around it
(define (tower a b c col)
  (cond
    [(<= a 0) empty-image]
    [else (above (tower (- a c) b c col)
                 (overlay (rectangle a b 'solid col)
                          (rectangle a b 'outline 'black)))]))
    
            
;test image:(tower 100 10 10 "orange")         

(: cc : Integer Integer Image-Color -> Image)
;; concentric circles
;; given: radius, delta, color
(define (cc a b col)
  (cond
    [(<= a 0) empty-image]
    [else (overlay (circle a 'outline col)
                   (cc (- a b) b col))]))

;test image:(cc 100 20 'red)


(: cc3 : Integer Integer Image-Color Image-Color Image-Color -> Image)
;; concentric colors in three colors
;; given: radius, delta, and a cycle of three colors
(define (cc3 a b col1 col2 col3)
  (cond
    [(<= a 0) empty-image]
    [(>= a (* 3 b)) (overlay (cc3 (- a (* 3 b)) b col1 col2 col3)
                            (circle (- a (* 2 b)) 'solid col3)
                            (circle (- a b) 'solid col2)
                            (circle a 'solid col1))]
    [(>= a (* 2 b)) (overlay (circle (- a b) 'solid col2)
                             (circle a 'solid col1))]
    [(>= a b) (circle a 'solid col1)]
    [else empty-image]))

;test image:(cc3 100 4 "blue" "lightblue" "dodgerblue")

(: duplicate-beside : Integer Image -> Image)
;; given: number of duplicates, image to duplicate
(define (duplicate-beside a img)
  (cond
    [(<= a 0) empty-image]
    [else (beside img (duplicate-beside (- a 1) img))]))

;test image:(duplicate-beside 5 (circle 10 'solid 'black))
    

(: vanishing : Exact-Rational Image -> Image)
;; draw vanishing row of images extending to the right
;; given: scaling factor, image
;; raise an error if the factor is not strictly between 0 and 1
;; you can measure the width of an image with built-in "image-width" and 
;;   the height with built-in "image-height"
(define (vanishing r img)
  (cond
    [(or (>= r 1) (<= r 0)) (error "scaling factor out of range")]
    [(< (image-width img) 1) empty-image]
    [else (beside img (scale r img) (vanishing r (scale (* r r) img)))]))

;test image:(vanishing 2/3 (cc3 50 4 "blue" "black" "silver"))


(: divide-tree : IBT Integer -> RBT)
;; divide every item in the IBT by the integer to get an RBT
;; the resulting RBT should have the same shape as the original tree;
(define (divide-tree itree n)
  (match itree
    ['IEmpty'REmpty]
    [(IntNode root lsub rsub)
     (RationalNode (/ (IntNode-root itree) n)
                   (if (symbol? (IntNode-lsub itree)) 'REmpty
                       (divide-tree (IntNode-lsub itree) n))
                   (if (symbol? (IntNode-rsub itree)) 'REmpty
                       (divide-tree (IntNode-rsub itree) n)))]))

(check-expect (divide-tree (IntNode 5 (IntNode 7 'IEmpty 'IEmpty) 'IEmpty) 2)
              (RationalNode 5/2 (RationalNode 7/2 'REmpty 'REmpty) 'REmpty))


(: floor-tree : RBT -> IBT)
;; take exact-floor of every item in the RBT to get an IBT
;; the resulting IBT should have the same shape as the original tree
(define (floor-tree rtree)
  (match rtree
    ['REmpty 'IEmpty]
    [(RationalNode root lsub rsub)
     (IntNode (exact-floor (RationalNode-root rtree))
              (if (symbol? (RationalNode-lsub rtree)) 'IEmpty
                  (floor-tree (RationalNode-lsub rtree)))
              (if (symbol? (RationalNode-rsub rtree)) 'IEmpty
                  (floor-tree (RationalNode-rsub rtree))))]))

(check-expect (floor-tree
               (RationalNode 5/2 (RationalNode 7/2 'REmpty 'REmpty) 'REmpty))
              (IntNode 2 (IntNode 3 'IEmpty 'IEmpty) 'IEmpty))



(: contains? : Integer IBT -> Boolean)
;; return true if the given integer appears somewhere (anywhere)
;;   in the IBT
(define (contains? n itree)
  (match itree 
    ['IEmpty #f]
    [(IntNode m lsub rsub)
     (if (= m n) #t (or (contains? n (IntNode-lsub itree))
                        (contains? n (IntNode-rsub itree))))]))

(check-expect (contains? 5 (IntNode 6 (IntNode 5 'IEmpty 'IEmpty) 'IEmpty)) #t)
(check-expect (contains? 10 (IntNode 6 (IntNode 7 'IEmpty 'IEmpty) 'IEmpty)) #f)


(: leaf? : RBT -> Bool)
;; a leaf is a nonempty tree with two empty subtrees
(define (leaf? rtree)
  (match rtree
    [(RationalNode root 'REmpty 'REmpty) #t]
    [_ #f]))

(check-expect (leaf? (RationalNode 5 'REmpty 'REmpty)) #t)
(check-expect (leaf? (RationalNode 7
                                   (RationalNode 6 'REmpty 'REmpty) 'REmpty))#f)

(: num-leaves : RBT -> Integer)
;; count the number of leaves in an RBT
(define (num-leaves rtree)
  (match rtree
    ['REmpty 0]
    [(RationalNode root 'REmpty 'REmpty) 1]
    [(RationalNode _ lsub rsub)
     (+ (num-leaves (RationalNode-lsub rtree))
        (num-leaves (RationalNode-rsub rtree)))]))

(check-expect (num-leaves
               (RationalNode 7 (RationalNode 5 'REmpty 'REmpty)
                             (RationalNode 10 'REmpty 'REmpty))) 2)
(check-expect (num-leaves (RationalNode 7 'REmpty 'REmpty)) 1)

  
(: node-sum : RBT -> Exact-Rational)
;; add all the rational values together in all nodes
(define (node-sum rtree)
  (match rtree
    ['REmpty 0]
    [(RationalNode _ lsub rsub)
     (+ (RationalNode-root rtree)
        (node-sum (RationalNode-lsub rtree))
        (node-sum (RationalNode-rsub rtree)))]))

(check-expect (node-sum (RationalNode 7 'REmpty 'REmpty)) 7)
(check-expect (node-sum (RationalNode 7 (RationalNode 5 'REmpty 'REmpty)
                                      (RationalNode 10 'REmpty 'REmpty))) 22)

(: leaf-sum : RBT -> Exact-Rational)
;; add just the rational values that occupy leaves
(define (leaf-sum rtree)
  (cond
    [(symbol? rtree) 0]
    [(leaf? rtree) (RationalNode-root rtree)]
    [else (+ (leaf-sum (RationalNode-lsub rtree))
             (leaf-sum (RationalNode-rsub rtree)))]))

(check-expect (leaf-sum (RationalNode 7 'REmpty 'REmpty)) 7)
(check-expect (leaf-sum (RationalNode 2
                                      (RationalNode 6 'REmpty 'REmpty)
                                      (RationalNode 7 'REmpty 'REmpty))) 13)


(test)