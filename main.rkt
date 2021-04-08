#lang racket/base

(require fancy-app
         racket/match
         racket/string
         racket/port
         net/url
         net/url-connect
         net/head
         json
         rebellion/collection/list
         ;  rebellion/collection/vector/builder
         ;  rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         ;  rebellion/type/tuple
         rebellion/private/guarded-block
         resyntax
         resyntax/code-snippet
         resyntax/default-recommendations
         resyntax/file-group
         resyntax/refactoring-suite
         resyntax/source)

; https://docs.github.com/en/actions/reference/environment-variables#default-environment-variables

; files: from git
; `git diff ?? --name-only -z` outputs a list of changed files nul-separated
; refactoring suite: whatever -- cli args?

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

(define/guard (git-diff-names commitish)
  (define git (or (find-executable-path "git")
                  ; Racket doesn't know about $PATHEXT:
                  (find-executable-path "git.exe")))
  (guard (not git) then
         (error "couldn't find git executable in PATH"))
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f git "diff" commitish "--name-only" "-z"))
  (close-output-port stdin)
  (subprocess-wait proc)
  (define stdout-string (port->string stdout))
  (define stderr-string (string-trim (port->string stderr)))
  (close-input-port stdout)
  (close-input-port stderr)
  (if (zero? (string-length stderr-string))
      (string-split stdout-string "\0")
      (error stderr-string)))

(define/guard (git-path path)
  (define git (or (find-executable-path "git")
                  ; Racket doesn't know about $PATHEXT:
                  (find-executable-path "git.exe")))
  (guard (not git) then
         (error "couldn't find git executable in PATH"))
  (define-values (proc stdout stdin stderr)
    (subprocess #f #f #f git "ls-tree" "-r" "-z" "--name-only" "HEAD" path))
  (close-output-port stdin)
  (subprocess-wait proc)
  (define stdout-string (port->string stdout))
  (define stderr-string (string-trim (port->string stderr)))
  (close-input-port stdout)
  (close-input-port stderr)
  (if (zero? (string-length stderr-string))
      (string-split stdout-string "\0")
      (error stderr-string)))

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
     (make-immutable-hash `((commit_id . ,commit-id)
                            (body . ,body)
                            (event . ,event)
                            (comments . ,(map github-review-comment-jsexpr comments))))]))

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
     (make-immutable-hash `((path . ,path)
                            (body . ,body)
                            (start_line . ,start-line)
                            (line . ,end-line)
                            (start_side . ,start-side)
                            (side . ,end-side)))]))

; https://docs.github.com/en/rest/reference/pulls#create-a-review-for-a-pull-request
(define (github-review-request-url req)
  ; header: `accept`: `application/vnd.github.v3+json`
  ;                | application/vnd.github.comfort-fade-preview+json
  ; body:
  ;   commit_id: (github-sha)
  ;   body: body text...
  ;   event: REQUEST_CHANGES
  ;   comments: array
  (string->url (format "~a/repos/~a/pulls/~a/reviews"
                       (github-api-url)
                       (github-repository)
                       (github-review-request-pull-number req))))

(define (hash->headers table)
  (for/fold ([headers empty-header])
            ([(k v) (in-hash table)])
    (insert-field k v headers)))

(define (github-review-request-send req)
  (define github-review-request-headers
    (hash->headers (make-immutable-hash
                    `(("Accept" . "application/vnd.github.comfort-fade-preview+json")
                      ("Authorization" . ,(format "Bearer ~a" (github-token)))))))
  (parameterize ([current-https-protocol 'secure])
    (define response-port
      (post-pure-port (github-review-request-url req)
                      (jsexpr->bytes (github-review-request-body-jsexpr req))
                      github-review-request-headers))
    (define response (string-trim response-port))
    (close-input-port response-port)
    response))


(define/guard (resyntax-github-run)
  (define filenames (git-diff-names (github-base-ref)))
  (define files (file-groups-resolve (map single-file-group filenames)))
  (printf "resyntax: --- analyzing code ---\n")
  (define results
    (transduce files
               (append-mapping (refactor-file _ #:suite default-recommendations))
               #:into into-list))
  
  (define comments
    (for/list ([result (in-list results)])
      (define path (file-source-path (refactoring-result-source result)))
      (define old-code-snippet (refactoring-result-original-code result))
      (define new-code-snippet (refactoring-result-new-code result))
      (define start-line (code-snippet-start-line old-code-snippet))
      (define end-line (code-snippet-end-line old-code-snippet))
      (define new-code (code-snippet-raw-text new-code-snippet))
      (define body (format "```suggestion\n~a\n```\n\nRule: `~a`\n~a"
                           new-code
                           (refactoring-result-rule-name result)
                           (refactoring-result-message result)))
      (github-review-comment #:path (git-path path)
                             #:body body
                             #:start-line start-line
                             #:end-line end-line
                             #:start-side "RIGHT"
                             #:end-side "RIGHT")))
  
  (define req
    (github-review-request #:owner-repo (github-repository)
                           #:pull-number (git-ref->pr-number (github-ref))
                           #:commit-id (github-sha) 
                           #:body (if (null? comments)
                                      "LGTM!"
                                      "Hello! I've found some potential refactoring ideas :)")
                           #:event (if (null? comments)
                                       "APPROVE"
                                       "REQUEST_CHANGES")
                           #:comments comments))
  
  (println (format "~a" req)))

(module+ main
  (parameterize ([github-ref "refs/pull/385/merge"]
                 [github-base-ref "main"])
    (resyntax-github-run)))
