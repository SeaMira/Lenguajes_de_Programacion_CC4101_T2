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
         | (fun (list <id>*) <expr>)
         | (app <sym> (list <expr>*))
         | (tuple (list <expr>*))
         | (proj <tuple> <num>)
|#
(deftype Expr
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

;; s-expressions used as concrete syntax for our programs
#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list '*  <s-expr> <s-expr>)
           | ('true)
           | ('false)
           | (list '<=  <s-expr>  <s-expr>)
           | (list 'ifc  <s-expr> <s-expr> <s-expr>)
           | (<sym>)
           | (list 'fun (list <sym>*) <s-expr>)
           | (list <s-expr> <s-expr>*)
           | (list 'tuple <s-expr>*)
           | (list 'proj <s-expr> <s-expr>)
|#

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


;; PARTE 1C, 1G

;; values of expressions
;; <Val> ::= (numV <number>)
;;         | (boolV <boolean>)
;;         | (tupleV (list <Val>))
;;         | (closureV <sym> <expr> <env>) 

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

;; num-leq? :: (boolV -> boolean)
;; Recieves a boolV and returns its boolean value.
(define (num-leq? n)
  (def (boolV b) n) b)

;; extend-env-lst :: listof(id) listof(expr) env env -> env
;; Extends an env with a list of id's with their own values (Expr) on another list.
(define (extend-env-lst args arg-values env ext-env)
  (if (not (eq? (length args) (length arg-values))) (error "not the amount of parameters needed") (void))
  (match args
    [(list) ext-env]
    [(cons arg rest-args)
     (def (cons val rest-arg-values) arg-values)
     (extend-env-lst rest-args rest-arg-values env (extend-env arg (eval val env) ext-env))]
  ))

;; eval-tuple :: list(expr) env -> list(Val)
;; Turns a tuple values into Val's according to a env.
(define (eval-tuple lst env)
  (match lst
    [(list) '()]
    [(cons n rest) (cons (eval n env) (eval-tuple rest env))]
    )
  )

;; tupl-ref :: tupleV numV -> Val
;; Recieves a tupleV and a numV and takes the projection that the value of the numV indicates as a position
;; on the values of the tupleV.
(define (tupl-ref tuplV nV)
  (def (tupleV lst) tuplV)
  (def (numV i) nV)
  (list-ref lst i)
  )

;; eval :: Expr Env -> Value
;; evaluates an expression in a given
;; environment using static scoping
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



;; swap* :: (expr expr -> expr) -> (expr expr -> expr)
;; recieves a function that takes parameters "a" and "b" to return a "c" value,
;; returns a function that evaluates the parameter function with parameters "b" and "a" to return a "c" value
(define swap* (closureV (list 'f)  (fun (list 'x 'y) (app (id 'f) (list (parse 'y) (parse 'x)))) (mtEnv)))

;; curry* :: (expr expr -> expr) -> (expr -> (expr -> expr))
;; recieves a function that takes parameters "a" and "b" to return a "c" value,
;; and curryfies it into a function that takes an "a" value to generate a function that
;; recieves a "b" value to return a "c" value.
(define curry* (closureV (list 'f) (fun (list 'x) (fun (list 'y) (app (id 'f) (list (parse 'x) (parse 'y))))) (mtEnv)))

;; uncurry* :: (Val Val -> Val) -> 
(define uncurry* (closureV (list 'f) (fun (list 'x 'y) (app (app (id 'f) (list (parse 'x))) (list (parse 'y)))) (mtEnv)))

;; partial* :: (fun expr -> expr)  
(define partial* (closureV (list 'f 'a) (fun (list 'b) (app (id 'f) (list (parse 'a) (parse 'b)))) (mtEnv)))

;; PARTE 2B

(define (add-f global env)
  (match global
    [(list) env]
    [(cons f rest) (def (cons s v) f) (add-f rest (extend-env s v env))]
    )
  )

;; run :: s-expr listof(pairs (id Val))-> Val
;; evaluates an expression with functions defined in a list of pairs of "id of function" and function. 
(define (run s-expr gl)
  (def new-env (add-f gl (mtEnv)))
  (eval (parse s-expr) new-env))


