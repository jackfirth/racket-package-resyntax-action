#lang info

(define collection "resyntax-github-action")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "resyntax-github-action")))

(define deps
  (list "base"
        "resyntax"
        "fancy-app"
        "rebellion"))

(define build-deps
  (list "racket-doc"
        "rackunit-lib"
        "scribble-lib"))

(define racket-launcher-names
  (list "resyntax-github-action"))

(define racket-launcher-libraries
  (list "main.rkt"))