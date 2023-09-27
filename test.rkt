#lang play
(require "T2.rkt")

;; Testing Parse
; Test sum
(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(+ 1 (+ x 2))) (add (num 1) (add (id 'x) (num 2))))
(test (parse '(+ (- 2 4) (* 5 6))) (add (sub (num 2) (num 4)) (mul (num 5) (num 6))))

; Test sub
(test (parse '(- 1 2)) (sub (num 1) (num 2)))
(test (parse '(- (fun (x y) (+ x y)) 2)) (sub (fun (list 'x 'y) (add (id 'x) (id 'y))) (num 2)))

; Test mul
(test (parse '(* 1 2)) (mul (num 1) (num 2)))
(test (parse '(* (if (<= 3 4) 5 8) 9)) (mul (ifc (leq (num 3) (num 4)) (num 5) (num 8)) (num 9)))

; Test true
(test (parse 'true) (tt))
(test (parse '(if (<= 3 4) true true)) (ifc (leq (num 3) (num 4)) (tt) (tt)))

; Test false
(test (parse 'false) (ff))
(test (parse '(if (<= 3 4) false false)) (ifc (leq (num 3) (num 4)) (ff) (ff)))

; Test less or equal
(test (parse '(<= 3 4)) (leq (num 3) (num 4)))
(test (parse '(fun (x y z) (if (<= x y) (+ x y) (+ x z)))) (fun '(x y z) (ifc (leq (id 'x) (id 'y)) (add (id 'x) (id 'y)) (add (id 'x) (id 'z)))))

; Test ifc
(test (parse '(if (<= 3 4) 3 4)) (ifc (leq (num 3) (num 4)) (num 3) (num 4)))

; Test parse id
(test (parse 'x) (id 'x))

; Test parse fun
(test (parse ' (fun (x y) (+ x y))) (fun '(x y) (add (id 'x) (id 'y))))

; Test app fun
(test (parse '(my-function 2 3 4)) (app (id 'my-function) (list (num 2) (num 3) (num 4))))
(test (parse '(alo)) (app (id 'alo) (list)))

; Testing parse tuple
(test (parse ' (tuple 1 2 3)) (tupl ( list (num 1) (num 2) (num 3))))
;Testing parse projection
(test (parse '(proj (tuple 10 20 30) 1)) (proj (tupl (list (num 10) (num 20) (num 30))) (num 1)))



(test (eval (parse '(fun (x) x)) (mtEnv)) (closureV (list 'x) (parse 'x) (mtEnv)))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y z))) (mtEnv)) (mtEnv))) (numV 6))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y (+ z w)))) (aEnv 'w (numV 6) mtEnv)) (mtEnv))) (numV 12))


;; Part 2

; Test swap*
;(test (swap* (fun (list 'x 'y) (parse '(- x y)))) (closureV (list 'y 'x) (parse '(- x y)) (mtEnv)))
;(test (swap* (fun (list 'x 'y) (parse '(+ x y)))) (fun (list 'y 'x) (parse '(+ x y))))

(define globals ( list
(cons 'swap swap*)
(cons 'curry curry*)
;(cons 'uncurry uncurry*)
(cons ' partial partial* )))


(print-only-errors #t)
