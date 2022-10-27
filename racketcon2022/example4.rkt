#lang racket/base


(define (f x)
  (let ([y (* x 2)])
    (+ y 1)))

(define (g x)
  (let ([x (* x 2)])
    (+ x 1)))
