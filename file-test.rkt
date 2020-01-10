#lang racket
(require rackunit "othello.rkt")

(check equal? (avanza '(0 0) 2) '(1 -1))