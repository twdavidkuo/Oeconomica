#lang typed/racket

(require typed/test-engine/racket-tests)
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(: scale-down-power-2 : Image -> Image)
;; scale down the size of image to the closest power of 2
(define (scale-down-power-2 img)
  (cond
    [(integer? (/ (log (image-width img)) (log 2))) img]
    [else (scale-down-power-2
           (scale (/ (- (image-width img) 1) (image-width img)) img))]))

(check-expect (scale-down-power-2 (square 40 'solid 'red)) (square 32 'solid 'red))


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

(: duplicate-above : Integer Image -> Image)
;; given: number of duplicates, image to duplicate above the image
(define (duplicate-above a img)
  (cond
    [(<= a 0) empty-image]
    [else (above img (duplicate-above (- a 1) img))]))

(: define base : Image -> Image)
(define (base img)
  (

(: sicp : Image -> Image)
;;performing the sicp algorithm to a given picture
(define (sicp img)
  (local {define


(test)