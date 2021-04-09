#lang racket/base

(require racket/list
         racket/match
         racket/string
         racket/port
         net/url
         net/url-connect
         json
         rebellion/type/record
         rebellion/collection/list
         rebellion/streaming/transducer
         rebellion/private/guarded-block
         resyntax
         resyntax/refactoring-result
         resyntax/code-snippet
         resyntax/default-recommendations
         resyntax/file-group
         resyntax/source)

; https://docs.github.com/en/actions/reference/environment-variables#default-environment-variables

; TODO: I implemented the github-* parameters as parameters bc that's easy and makes sense, but
; several of these don't really make sense as parameters -- some of these are specific to runs for pull
; requests, for example, and this code would ideally be more generic.

; https://docs.github.com/en/actions/reference/authentication-in-a-workflow#about-the-github_token-secret
(define github-token
  (make-parameter (getenv "GITHUB_TOKEN") #f 'github-token))

; The owner and repository name. For example, `octocat/Hello-World`.
(define github-repository
  (make-parameter (getenv "GITHUB_REPOSITORY") #f 'github-repository))

; The GitHub workspace directory path. The workspace directory is a copy of
; your repository if your workflow uses the `actions/checkout` action. If you
; don't use the actions/checkout action, the directory will be empty. For
; example, `/home/runner/work/my-repo-name/my-repo-name`.
(define github-workspace
  (make-parameter (getenv "GITHUB_WORKSPACE") #f 'github-workspace))

; The commit SHA that triggered the workflow. For example,
; `ffac537e6cbbf934b08745a378932722df287a53`.
(define github-sha
  (make-parameter (getenv "GITHUB_SHA") #f 'github-sha))

; The branch or tag ref that triggered the workflow. For example,
; `refs/heads/feature-branch-1`. If neither a branch or tag is available for
; the event type, the variable will not exist.
(define github-ref
  (make-parameter (getenv "GITHUB_REF") #f 'github-ref))

; Only set for pull request events. The name of the head branch.
(define github-head-ref
  (make-parameter (getenv "GITHUB_HEAD_REF") #f 'github-head-ref))

; Only set for pull request events. The name of the base branch.
(define github-base-ref
  (make-parameter (getenv "GITHUB_BASE_REF") #f 'github-base-ref))

; Returns the API URL. For example: `https://api.github.com`.
(define github-api-url
  (make-parameter (getenv "GITHUB_API_URL") #f 'github-api-url))

; This doesn't support anything but getting stdout -- but that's OK for now!
(define/guard (run-cmd cmd-name . args)
  (define cmd-path (or (find-executable-path cmd-name)
                       ; Racket doesn't know about $PATHEXT:
                       (find-executable-path (string-append ".exe"))))
  (guard (not cmd-path) then
         (error (format "couldn't find ~a executable in PATH" cmd-name)))
  (define-values (proc stdout stdin stderr)
    (apply subprocess `(#f #f #f ,cmd-path ,@args)))
  (close-output-port stdin)
  (subprocess-wait proc)
  (define exit-code (subprocess-status proc))
  (define stdout-string (port->string stdout))
  (define stderr-string (string-trim (port->string stderr)))
  (close-input-port stdout)
  (close-input-port stderr)
  (guard (zero? exit-code) else
         (error (format "command '~a ~a' exited with code ~a"
                        cmd-path
                        (string-join args " ")
                        exit-code)))
  (guard (zero? (string-length stderr-string)) else
         (error (format "command '~a ~a' wrote to stderr: ~a"
                        cmd-path
                        (string-join args " ")
                        stderr-string)))
  stdout-string)

(define (git-diff-names commitish)
  (string-split (run-cmd "git" "diff" "--name-only" "-z" commitish)
                "\0"))

(define (git-path path)
  (string-split (run-cmd "git" "ls-tree" "-r" "-z" "--name-only" "HEAD" path)
                "\0"))

(define (git-ref->pr-number ref)
  (match ref
    [(regexp #rx"^refs/pull/([0-9]+)/merge$" (list _ num))
     (string->number num)]
    [_
     (error (format "ref ~a doesn't represent a pull request"))]))

(define-record-type github-review-request
  (owner-repo pull-number commit-id body event comments))

(define (github-review-request-body-jsexpr req)
  (match req
    [(github-review-request #:commit-id commit-id
                            #:body body
                            #:event event
                            #:comments comments)
     (hash 'body body
           'event event
           'comments (map github-review-comment-jsexpr
                          comments))]))

(define-record-type github-review-comment
  (path body start-line end-line start-side end-side))

(define (github-review-comment-jsexpr comment)
  (match comment
    [(github-review-comment #:path path
                            #:body body
                            #:start-line start-line
                            #:end-line end-line
                            #:start-side start-side
                            #:end-side end-side)
     (if (= start-line end-line)
         (hash 'path path
               'body body
               'line end-line
               'side end-side)
         (hash 'path path
               'body body
               'start_line start-line
               'line end-line
               'start_side start-side
               'side end-side))]))

; https://docs.github.com/en/rest/reference/pulls#create-a-review-for-a-pull-request
(define (github-review-request-url req)
  (string->url (format "~a/repos/~a/pulls/~a/reviews"
                       (github-api-url)
                       (github-repository)
                       (github-review-request-pull-number req))))

(define (github-review-request-send req)
  (parameterize ([current-https-protocol 'secure])
    (define response-port
      (post-pure-port (github-review-request-url req)
                      (jsexpr->bytes (github-review-request-body-jsexpr req))
                      ; https://docs.github.com/en/rest/reference/pulls#list-review-comments-in-a-repository-preview-notices
                      `("Accept: application/vnd.github.comfort-fade-preview+json"
                        ,(format "Authorization: Bearer ~a" (github-token)))))
    (define response (port->string response-port))
    (close-input-port response-port)
    response))

; thank you jack for this code in the resyntax cli.rkt :)
(define/guard (string-indent s #:amount amount)
  (guard (zero? amount) then s)
  (define indent-string (make-string amount #\space))
  (define lines
    (for/list ([line (in-lines (open-input-string s))])
      (string-append indent-string line)))
  (string-join lines "\n"))

(define (resyntax-github-run)
  (define filenames (git-diff-names (github-base-ref)))
  (define files (file-groups-resolve (map single-file-group filenames)))
  (printf "resyntax: --- analyzing code ---\n")
  (define results
    (transduce files
               (append-mapping (λ (file) (refactor-file file #:suite default-recommendations)))
               #:into into-list))
  
  (define comments
    (for/list ([result (in-list results)])
      (define path (file-source-path (refactoring-result-source result)))
      (define old-code-snippet (refactoring-result-original-code result))
      (define new-code-snippet (refactoring-result-new-code result))
      (define start-line (code-snippet-start-line old-code-snippet))
      (define end-line (sub1 (code-snippet-end-line old-code-snippet)))
      (define start-col (code-snippet-start-column new-code-snippet))
      (define new-code (code-snippet-raw-text new-code-snippet))
      (define body (format "```suggestion\n~a\n```\n\n~a [`~a`]"
                           (string-indent new-code #:amount start-col)
                           (refactoring-result-message result)
                           (refactoring-result-rule-name result)))
      (github-review-comment #:path (first (git-path path))
                             #:body body
                             #:start-line start-line
                             #:end-line end-line
                             #:start-side "RIGHT"
                             #:end-side "RIGHT")))
  
  (define resyntax-markdown-link "[Resyntax](https://docs.racket-lang.org/resyntax/)")
  (define req
    (github-review-request #:owner-repo (github-repository)
                           #:pull-number (git-ref->pr-number (github-ref))
                           #:commit-id (github-sha) 
                           #:body (string-append resyntax-markdown-link
                                                 " analyzed this pull request and "
                                                 (if (null? comments)
                                                     "found no issues."
                                                     "has added suggestions."))
                           #:event (if (null? comments)
                                       "APPROVE"
                                       "REQUEST_CHANGES")
                           #:comments comments))
  
  (printf "Request struct: ~a\nSending request!\n" req)
  (define resp (github-review-request-send req))
  (printf "Response: ~a\n" resp))

(module+ main
  (resyntax-github-run))
