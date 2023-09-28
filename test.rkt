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

; Test env-lookup
(test/exn (env-lookup 'x (aEnv 'a (numV 0) (mtEnv))) "free identifier: x")

; Test num2num-op
(test/exn (num+ 4 2) "num-op: invalid operands")

; Test num2bool-op
(test/exn (num<= 4 2) "bool-op: invalid operands")

; Test extend-env-lst
(test/exn (extend-env-lst '(x y z) '(1 2) (mtEnv) (mtEnv)) "not the amount of parameters needed")

;; Test eval
(test (eval (parse '15) (mtEnv)) (numV 15))
(test (eval (parse '(+ 2 4)) (mtEnv)) (numV 6))
(test (eval (parse '(- 2 4)) (mtEnv)) (numV -2))
(test (eval (parse '(* 2 4)) (mtEnv)) (numV 8))
(test (eval (parse '(if (<= 8 x) (* 2 x) (+ x 1))) (aEnv 'x (numV 20) (mtEnv))) (numV 40))
(test (eval (parse '(if (<= 8 x) true false)) (aEnv 'x (numV 4) (mtEnv))) (boolV #f))
(test (eval (parse '(if (<= 8 x) true false)) (aEnv 'x (numV 8) (mtEnv))) (boolV #t))
(test (eval (parse '(fun (x, y) (if (<= x y) true false))) (aEnv 'x (numV -4) (aEnv 'y (numV -1) (mtEnv))))
      (closureV '(x ,y) (ifc (leq (id 'x) (id 'y)) (tt) (ff)) (aEnv 'x (numV -4) (aEnv 'y (numV -1) (mtEnv)))))
(test (eval (parse '(fun (x) x)) (mtEnv)) (closureV (list 'x) (parse 'x) (mtEnv)))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y z))) (mtEnv)) (mtEnv))) (numV 6))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y (+ z w)))) (aEnv 'w (numV 6) mtEnv)) (mtEnv))) (numV 12))
(test (eval (parse '(f (g (h 1 2 3) 3))) (aEnv 'h (closureV (list 'h1 'h2 'h3) (parse '(+ h1 (+ h2 h3))) (mtEnv))
                                                (aEnv 'g (closureV (list 'g1 'g2) (parse '(+ g1 g2)) (mtEnv))
                                                      (aEnv 'f (closureV (list 'f1) (parse '(* f1 4)) (mtEnv)) (mtEnv)))))
      (numV 36))
(test (eval (parse '(fun (x) x)) (mtEnv)) (closureV (list 'x) (parse 'x) (mtEnv)))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y z))) (mtEnv)) (mtEnv))) (numV 6))
(test (eval (parse '(f 2 2 2)) (aEnv 'f (closureV (list 'x 'y 'z) (parse '(+ x (+ y (+ z w)))) (aEnv 'w (numV 6) mtEnv)) (mtEnv))) (numV 12))


(test (eval (parse '(proj (tuple 10 20 30) 1)) (mtEnv)) (numV 20)) 
(test (eval (parse '(tuple 1 2 3)) (mtEnv)) (tupleV (list (numV 1) (numV 2) (numV 3))))
(test (eval (parse '(tuple (+ 1 2) (- 9 6) (* 5 8))) (mtEnv)) (tupleV (list (numV 3) (numV 3) (numV 40))))
(test (eval (parse '(proj (tuple (+ 1 2) (* x y) (fun (w z) (<= w z))) (if (<= x y) 0 1))) (aEnv 'x (numV 8)
                                                                               (aEnv 'y (numV 16)
                                                                                     (aEnv 'w (numV 2)
                                                                                           (aEnv 'z (numV 99)
                                                                                                 (mtEnv))))))
      (numV 3))
(test (eval (parse '(proj (tuple (+ 1 2) (* x y) (fun (w z) (<= w z))) (if (<= x y) 0 1))) (aEnv 'x (numV 20)
                                                                               (aEnv 'y (numV 16)
                                                                                     (aEnv 'w (numV 2)
                                                                                           (aEnv 'z (numV 99)
                                                                                                 (mtEnv))))))
      (numV 320))
(test (eval (parse '(proj (tuple true false true false) 2)) (mtEnv)) (boolV #t))
(test (eval (parse '(proj (tuple true false true false) 3)) (mtEnv)) (boolV #f))
(test (eval (parse '(proj (tuple (f (g (h 1 2 3) 3))) 0)) (aEnv 'h (closureV (list 'h1 'h2 'h3) (parse '(+ h1 (+ h2 h3))) (mtEnv))
                                                (aEnv 'g (closureV (list 'g1 'g2) (parse '(+ g1 g2)) (mtEnv))
                                                      (aEnv 'f (closureV (list 'f1) (parse '(* f1 4)) (mtEnv)) (mtEnv)))))
      (numV 36))

;; Part 2



(define globals ( list
(cons 'swap swap*)
(cons 'curry curry*)
(cons 'uncurry uncurry*)
(cons ' partial partial* )))


(test (run '(((curry (fun (x y) (<= x y))) 1) 2) globals) (boolV #t))
(test (run '(((curry (fun (x y) (<= x y))) 2) 1) globals) (boolV #f))
(test (run '((uncurry (curry (fun (x y) (<= x y)))) 1 2) globals) (boolV #t))
(test (run '((uncurry (curry (fun (x y) (<= x y)))) 2 1) globals) (boolV #f))
(test (run '((swap (uncurry (curry (fun (x y) (<= x y))))) 2 1) globals) (boolV #t))
(test (run '((swap (fun (x y) (+ x y))) 1 2) globals) (numV 3))
(test (run '((swap (fun (x y) (- x y))) 1 2) globals) (numV 1))
(test (run '((partial (fun (x y) (if (<=  x y) true false)) 1) 2) globals) (boolV #t))
(test (run '(+ 1 2) globals) (numV 3))
(test (run '(((curry (uncurry (fun (x) (fun (y) (<= x y))))) 1) 2) globals) (boolV #t))
(test (run '((partial (swap (uncurry (curry (fun (x y) (<= x y))))) 2) 1) globals) (boolV #t))


(print-only-errors #t)

