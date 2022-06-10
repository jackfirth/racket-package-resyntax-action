#lang racket/base


(define (pwease-refactow-me?)
  (or 1 2 (or 3 4)))


(define (pwease-refactow-me2?)
    (or 1 2 (or 3 4)))


(define (pwease-refactow-me3?)
(or 1 2 (or 3 4)))


(define (pwease-refactow-me4?)
  (let ([x 1])
    (+ x 5)))
