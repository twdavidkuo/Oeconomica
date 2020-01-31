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

;(check-expect (scale-down-power-2 (square 40 'solid 'red))
 ;             (square 32 'solid 'red))


(: thing64 Image)
(define thing64
  (overlay/xy (circle 10 'solid 'red)
	      -40
	      -10
	      (square 64 'outline 'black)))

(: vertical : Image -> Image)
;;define the vertical part of a recursive rectangle
(define (vertical img)
  (cond
    [(< (image-height img) 2) empty-image]
    [else (above (beside (vertical (scale 1/2 img)) (vertical (scale 1/2 img)))
                 img)]))


(: L-shape : Image -> Image)
;;turn the recursive rectangle into a l-shape graph
(define (L-shape img)
  (underlay/align 'left 'bottom
                 (rotate -90 (flip-horizontal (vertical img)))
                 (vertical img)))

(: base : Image -> Image)
;;turn the l-shape into a recursive square
(define (base img)
  (cond
    [(< (image-width img) 2) empty-image]
    [else (overlay/align/offset 'right 'top
                         (L-shape img)
                         (/ 2 (image-width (L-shape img)))
                         (/ 2 (image-width (L-shape img)))
                         (base (scale 1/2 img)))]))
                         

 


(: sicp : Image -> Image)
;;performing the sicp algorithm to a given picture
(define (sicp img)
  (above (beside (flip-horizontal (base thing64)) (base thing64))
         (flip-vertical
          (beside (flip-horizontal (base thing64)) (base thing64)))))

   
(test)