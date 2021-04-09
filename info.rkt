#lang info

(define collection "racket-package-resyntax-action")

(define scribblings
  (list (list "main.scrbl"
              (list 'multi-page)
              (list 'library)
              "racket-package-resyntax-action")))

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
  (list "racket-package-resyntax-action"))

(define racket-launcher-libraries
  (list "main.rkt"))
