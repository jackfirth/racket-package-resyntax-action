#lang racket/base


(define (distance x1 y1 x2 y2)
  (let ([dx (- x1 x2)]
        [dy (- y1 y2)])
    (sqrt (+ (* dx dx) (* dy dy)))))


(or 1 (or 2 3))


(if 'cond 'then (if 'cond2 'then2 'else))


(if 'a
    (println "true branch")
    #f)
