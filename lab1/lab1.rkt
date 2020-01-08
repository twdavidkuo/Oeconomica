#lang typed/racket
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")

(: sears-tower Image)
(define sears-tower
  (above (beside (rectangle 2 11 "solid" "black")
                 (rectangle 3 1 "solid" "white")
                 (rectangle 2 11 "solid" "black"))
         (rectangle 10 30 "solid" "black")
         (rectangle 20 100 "solid" "black")))