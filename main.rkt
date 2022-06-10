#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [resyntax-github-run
   (-> #:git-base-ref string?
       #:github-repository (and/c string? (lambda (s) (string-contains? s "/")))
       #:branch-ref (and/c string? git-pr-ref?)
       void)]
  [git-diff-names (-> string? (listof string?))]
  [git-path (-> (or/c string? path?) (listof string?))]
  [git-pr-ref-regexp regexp?]
  [git-pr-ref? predicate/c]
  [git-ref->pr-number (-> string? exact-nonnegative-integer?)]))


(require racket/list
         racket/match
         racket/pretty
         racket/string
         rebellion/collection/list
         rebellion/streaming/transducer
         rebellion/private/guarded-block
         resyntax
         resyntax/refactoring-result
         resyntax/code-snippet
         resyntax/default-recommendations
         resyntax/file-group
         resyntax/line-replacement
         resyntax/source
         racket-package-resyntax-action/command
         racket-package-resyntax-action/github)


(define (git-diff-names commitish)
  (string-split (run-command "git" "diff" "--name-only" "-z" commitish)
                "\0"))

(define (git-path path)
  (string-split (run-command "git" "ls-tree" "-r" "-z" "--name-only" "HEAD" path)
                "\0"))

(define git-pr-ref-regexp #rx"^refs/pull/([0-9]+)/merge$")
(define (git-pr-ref? ref)
  (regexp-match git-pr-ref-regexp ref))

(define (git-ref->pr-number ref)
  (match ref
    [(regexp git-pr-ref-regexp (list _ num))
     (string->number num)]
    [_
     (error (format "ref ~a doesn't represent a pull request" ref))]))

(define (resyntax-analyze-files file-groups)
  (define files (file-groups-resolve file-groups))
  (transduce files
             (append-mapping (lambda (file) (refactor-file file #:suite default-recommendations)))
             #:into into-list))

(define (refactoring-result->github-review-comment result)
  (define path (file-source-path (refactoring-result-source result)))
  (define replacement (refactoring-result-line-replacement result))
  (define body
    (format #<<EOS
**`~a`** ~a

```suggestion
~a
```

Debugging details below:

<details>
  <summary>Textual replacement</summary>

  ```scheme
~a
  ```
</details>

<details>
  <summary>Syntactic replacement</summary>

  ```scheme
~a
  ```
</details>
EOS
            (refactoring-result-rule-name result)
            (refactoring-result-message result)
            (line-replacement-new-text replacement)
            (string-indent (pretty-format replacement) #:amount 2)
            (string-indent (pretty-format (refactoring-result-replacement result)) #:amount 2)))
  (define comment
    (github-review-comment #:path (first (git-path path))
                           #:body body
                           #:start-line (line-replacement-start-line replacement)
                           #:end-line (line-replacement-original-end-line replacement)
                           #:start-side "RIGHT"
                           #:end-side "RIGHT"))
  (printf "DEBUG: comment = ~v\n" comment)
  comment)


(define (github-review-body comments? file-count)
  (format "[Resyntax](https://docs.racket-lang.org/resyntax/) analyzed ~a in this pull request and ~a"
          (if (= file-count 1) "1 file" (format "~a files" file-count))
          (if comments? "has added suggestions." "found no issues.")))


(define/guard (string-indent s #:amount amount)
  (guard (zero? amount) then s)
  (define indent-string (make-string amount #\space))
  (define lines
    (for/list ([line (in-lines (open-input-string s))])
      (string-append indent-string line)))
  (string-join lines "\n"))


(define (resyntax-github-run #:git-base-ref git-base-ref
                             #:github-repository github-repository
                             #:branch-ref branch-ref)
  (define files (git-diff-names git-base-ref))
  (define results (resyntax-analyze-files (map single-file-group files)))
  (define comments (map refactoring-result->github-review-comment results))
  (define req
    (github-review-request
     #:owner-repo github-repository
     #:pull-number (git-ref->pr-number branch-ref)
     #:body (github-review-body (not (null? comments)) (length files))
     #:event (if (null? comments) "APPROVE" "REQUEST_CHANGES")
     #:comments comments))
  (define resp (github-review-request-send req))
  (printf "Response: ~a\n" resp))


(define (pwease-refactow-me?)
  (or 1 2 (or 3 4)))


(define (pwease-refactow-me2?)
    (or 1 2 (or 3 4)))


(define (pwease-refactow-me3?)
(or 1 2 (or 3 4)))


(define (pwease-refactow-me4?)
  (let ([x 1])
    (+ x 5)))


(module+ main
  (resyntax-github-run #:git-base-ref (getenv "GITHUB_BASE_REF")
                       #:github-repository (getenv "GITHUB_REPOSITORY")
                       #:branch-ref (getenv "GITHUB_REF")))
