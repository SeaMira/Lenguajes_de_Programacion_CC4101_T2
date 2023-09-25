#lang play
(print-only-errors)

;; PARTE 1A, 1B, 1F

#|
  Expr ::= (num <num>)         
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (mul <expr> <expr>)
         | (tt)
         | (ff)
         | (leq <expr> <expr>)
         | (ifc <expr> <expr> <expr>)
         | (id <sym>)
         | (fun (list <id>) <expr>)
         | (app <sym> (list <expr>))
         | (tuple (list <expr>))
         | (proj <tuple> <num>)
|#
(deftype Expr
  ;; core
  (num n)
  (add l r)
  (sub l r)
  (mul l r)
  (tt)
  (ff)
  (leq l r)
  (ifc c t e)
  (id x)
  (fun args body)
  (app fun args)
  (tupl expr)
  (proj exprs expr)
  )

;; parse :: s-expr -> Expr
;; converts s-exprs into Exprs
(define (parse s-expr)
  (match s-expr
    [n #:when (number? n) (num n)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list '* l r) (mul (parse l) (parse r))]
    ['true (tt)]
    ['false (ff)]
    [(list '<= l r) (leq (parse l) (parse r))]
    [(list 'if c t e) (ifc (parse c) (parse t) (parse e))]
    [x #:when (and (symbol? x) (not (eq? x 'true)) (not (eq? x 'false))) (id x)]
    [(list 'fun (list elems ...) body) (fun elems (parse body))]
    [(list name args ...) #:when (and (not (eq? name 'tuple)) (not (eq? name 'proj)) (not (eq? name 'fun))) (app (parse name) (map parse args))]
    [(list 'tuple expr ...) (tupl (map parse expr))]
    [(list 'proj tupl expr) (proj (parse tupl) (parse expr))]
    ))

(test (parse '(+ 1 2)) (add (num 1) (num 2)))
(test (parse '(- 1 2)) (sub (num 1) (num 2)))
(test (parse '(* 1 2)) (mul (num 1) (num 2)))
(test (parse 'true) (tt))
(test (parse 'false) (ff))
(test (parse '(<= 3 4)) (leq (num 3) (num 4)))
(test (parse '(if (<= 3 4) 3 4)) (ifc (leq (num 3) (num 4)) (num 3) (num 4)))
(test (parse 'x) (id 'x))
(test (parse ' (fun (x y) (+ x y))) (fun '(x y) (add (id 'x) (id 'y))))
(test (parse '(my-function 2 3 4)) (app (id 'my-function) (list (num 2) (num 3) (num 4))))
;; PARTE 1C, 1G

;; values of expressions
;; <Val> ::= (numV <number>)
;;         | (boolV <boolean>)
;;         | (closureV <id> <expr> <env>) 

(deftype Val
  (numV n)
  (boolV b)
  (tupleV tvs)
  (closureV id body env)
  )

;; ambiente de sustitución diferida
(deftype Env
  (mtEnv)
  (aEnv id val env))

;; interface ADT (abstract data type) del ambiente
(define empty-env (mtEnv))

;; "Simplemente" asigna un nuevo identificador para aEnv
;(define extend-env aEnv)
;;
;; es lo mismo que definir extend-env así:
;; (concepto técnico 'eta expansion')
(define (extend-env id val env) (aEnv id val env))

(define (env-lookup x env)
  (match env
    [(mtEnv) (error 'env-lookup "free identifier: ~a" x)]
    [(aEnv id val rest) (if (symbol=? id x) val (env-lookup x rest))]))

;; PARTE 1D

;; num2num-op :: (Number Number -> Number)-> (Val Val -> Val)
;; Recieves a binary operation between two numbers returning a number, and returns a binary operation between
;; two numV-s that return a numV.

(define (num2num-op op)
  (lambda (x y) 
    (if (or (not (numV? x)) (not (numV? y))) (error "num-op: invalid operands") (void))
        (def (numV a) x) (def (numV b) y)
        (numV (op a b))
         ))

;; num2bool-op :: (Number Number -> Boolean)-> (Val Val -> Val)
;; Recieves a binary operation between two numbers returning a boolean, and returns a binary operation between
;; two numV-s that return a boolV.
(define (num2bool-op op)
  (lambda (x y) 
  (if (or (not (numV? x)) (not (numV? y))) (error "bool-op: invalid operands") (void))
        (def (numV a) x) (def (numV b) y)
        (boolV (op a b))
         ))


(define num+ (num2num-op +))
(define num- (num2num-op -))
(define num* (num2num-op *))
(define num<= (num2bool-op <=))

;; PARTE 1E, 1G

(define (num-leq? n)
  (def (boolV b) n) b)

(define (extend-env-lst args arg-values env ext-env)
  (if (not (eq? (length args) (length arg-values))) (error "not the amount of parameters needed") (void))
  (match args
    [(list) ext-env]
    [(cons arg rest-args)
     (def (cons val rest-arg-values) arg-values)
     (extend-env-lst rest-args rest-arg-values env (extend-env arg (eval val env) ext-env))]
  ))

(define (eval-tuple lst env)
  (match lst
    [(list) '()]
    [(cons n rest) (cons (eval n env) (eval-tuple rest env))]
    )
  )

(define (tupl-ref tuplV nV)
  (def (tupleV lst) tuplV)
  (def (numV i) nV)
  (list-ref lst i)
  )

;; eval :: ...
(define (eval expr env)
  (match expr
    [(num n) (numV n)]
    [(tt) (boolV #t)]
    [(ff) (boolV #f)]
    [(fun ids body) (closureV ids body env)]
    [(id x) (env-lookup x env)]
    [(add l r) (num+ (eval l env) (eval r env))]
    [(sub l r) (num- (eval l env) (eval r env))]
    [(mul l r) (num* (eval l env) (eval r env))]
    [(leq l r) (num<= (eval l env) (eval r env))]
    [(ifc c t e) (if (num-leq? (eval c env)) (eval t env) (eval e env))]
    
    [(app f e) (def (closureV the-args the-body the-claus-env) (eval f env))
               (def the-ext-env (extend-env-lst the-args e env the-claus-env))
               (eval the-body the-ext-env)]

    [(tupl tvs) (tupleV (eval-tuple tvs env))]
    [(proj tv i) (tupl-ref (eval tv env) (eval i env))]
    ))

;; PARTE 2A

(define swap* '???)
(define curry* '???)
(define uncurry* '???)
(define partial* '???)

;; PARTE 2B

;; run :: ...
(define (run) '???)
