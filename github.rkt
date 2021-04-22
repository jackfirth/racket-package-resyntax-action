#lang racket/base

(require racket/contract)

(provide (contract-out
          [github-review-request (-> #:owner-repo string?
                                     #:pull-number exact-positive-integer?
                                     #:body string?
                                     #:event (or/c "APPROVE" "REQUEST_CHANGES")
                                     #:comments (listof github-review-comment?)
                                     github-review-request?)]
          [github-review-comment (-> #:path string?
                                     #:body string?
                                     #:start-line exact-positive-integer?
                                     #:end-line exact-positive-integer?
                                     #:start-side "RIGHT"
                                     #:end-side "RIGHT"
                                     github-review-comment?)]
          [github-review-request-jsexpr (-> github-review-request?
                                            jsexpr?)]
          [github-review-comment-jsexpr (-> github-review-comment?
                                            jsexpr?)]
          [github-review-request-url (-> github-review-request?
                                         url?)]
          [github-review-request-send (-> github-review-request?
                                          jsexpr?)]
          [github-token (parameter/c string?)]
          [github-api-url (parameter/c string?)]))

(require racket/match
         net/url
         net/url-connect
         json
         rebellion/type/record)

; https://docs.github.com/en/actions/reference/authentication-in-a-workflow#about-the-github_token-secret
(define github-token
  (make-parameter (getenv "GITHUB_TOKEN") #f 'github-token))

; Returns the API URL. For example: `https://api.github.com`.
(define github-api-url
  (make-parameter (getenv "GITHUB_API_URL") #f 'github-api-url))


(define-record-type github-review-request
  (owner-repo pull-number body event comments))

(define (github-review-request-jsexpr req)
  (match req
    [(github-review-request #:body body
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
     (if (= start-line (sub1 end-line))
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
                       (github-review-request-owner-repo req)
                       (github-review-request-pull-number req))))

(define (github-review-request-send req)
  (parameterize ([current-https-protocol 'secure])
    (define response-port
      (post-pure-port (github-review-request-url req)
                      (jsexpr->bytes (github-review-request-jsexpr req))
                      ; https://docs.github.com/en/rest/reference/pulls#list-review-comments-in-a-repository-preview-notices
                      `("Accept: application/vnd.github.comfort-fade-preview+json"
                        ,(format "Authorization: Bearer ~a" (github-token)))))
    (define response-or-eof (read-json response-port))
    (if (eof-object? response-or-eof)
        (error (format "No response data for request to ~a"
                       (github-review-request-url req)))
        response-or-eof)))
