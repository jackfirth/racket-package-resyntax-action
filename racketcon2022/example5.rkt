#lang racket/base

(define populations (make-hash))


(hash-set! populations
           'portugal
           (+ (hash-ref populations 'portugal 0) 125))
