#lang racket/base

(define (some-function x y z)
  (+ x y z))

(define x 1)
(define y 2)
(define z 3)


(define (if a b c)
  (displayln "You thought I was an if expression? Fool!"))

(if (some-function x y z)
    #t
    #f)
