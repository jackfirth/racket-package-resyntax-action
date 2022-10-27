#lang racket/base

(require racket/list)


(define words (list 'the 'quick 'brown 'fox))

(for-each (λ (c)
            (printf "Letter: ~a\n" c)
            (printf "Letter code: ~a\n\n" (char->integer c)))
          (append-map (λ (word) (string->list (symbol->string word)))
                      words))
