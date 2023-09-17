#lang play
(require "T2.rkt")

(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 1 2)) (sub (num 1) (num 2)))
(test (parse '(* 1 2)) (mul (num 1) (num 2)))
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse '(<= 3 4)) (leq (num 3) (num 4)))
(test (parse '(if (<= 3 4) 3 4)) (ifc (leq (num 3) (num 4)) (num 3) (num 4)))

(print-only-errors #t)
