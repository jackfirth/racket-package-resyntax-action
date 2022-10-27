#lang racket/base


(syntax-disarm #'(foo a b c) (current-inspector))
