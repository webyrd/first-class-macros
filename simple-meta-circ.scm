(define (eval exp env)
  (cond
    ((symbol? exp)    (env-lookup env exp))
    ((number? exp)    exp)
    ((boolean? exp)   exp)
    ((string? exp)    exp)
    
    ; 3D-syntax is invoked to produce a captured value:
    ((procedure? exp) (exp))
    
    ((app? exp)       (perform-apply (eval (app->fun exp) env)
                                     exp env))))


(define (perform-apply fun app-exp env)
  (let ((args (app->args app-exp)))
    (cond
      ((macro? fun)       (eval (apply-proc (macro->proc fun) args) env))
      ((syntax-prim? fun) ((syntax-primitive->eval fun) app-exp env))
      (else                 (let ((arg-values (eval* args env)))
                              (apply-proc fun arg-values))))))


(define eval*
  (lambda (exp* env)
    (map (lambda (exp) (eval exp env)) exp*)))

(define apply-proc
  (lambda (p args)
    (p args)))





;; ((y . #t) (x . 5) (z . 7))
(define env-lookup
  (lambda (env x)
    (cond
      [(null? env) (error 'env-lookup "unbound variable")]
      [(eq? (caar env) x) (cdar env)]
      [else (env-lookup (cdr env) x)])))

;; (e1 e2* ...)
(define app?
  (lambda (exp)
    (and (list? exp) (>= (length exp) 1))))

;; (e1 e2* ...)
(define app->fun
  (lambda (exp)
    (car exp)))
(define app->args
  (lambda (exp)
    (cdr exp)))



(define macro->proc
  (lambda (s-exp)
    (cadr s-exp)))

(define syntax-primitive->eval
  (lambda (s-exp)
    (cadr s-exp)))


;; `(macro ,proc)
(define macro?
  (lambda (exp)
    (and (list? exp) (eq? (car exp) 'macro))))

;; (list 'syntax-primitive eval-if))
(define syntax-prim?
  (lambda (fun)
    (and (list? fun) (= (length fun) 2) (eq? (car fun) 'syntax-primitive))))


;; (lambda (x ...) body)
;;
;; (lambda (x y) (+ x y)) => (lambda args
;;                        (eval '(+ x y)
;;                              (ext-env 'x (car args)
;;                                (ext-env 'y (cadr args)
;;                                  env))))
(define eval-lambda
  (lambda (exp env)
    (let ((formal* (cadr exp))
          (body (caddr exp)))
      (lambda (args)
        (eval body (ext-env* formal* args env))))))

;; (quote datum)
(define eval-quote
  (lambda (exp env)
    (let ((datum (cadr exp)))
      datum)))

;; (if e1 e2 e3)
(define eval-if
  (lambda (exp env)
    (let ((e1 (cadr exp))
          (e2 (caddr exp))
          (e3 (cadddr exp)))
      (if (eval e1 env)
          (eval e2 env)
          (eval e3 env)))))


; eval-macro : macro-exp env -> value
(define eval-macro
  (lambda (exp env)
    (list 'macro (eval (macro->proc exp) env))))


;; using association lists for environments
(define ext-env*
  (lambda (formal* args env)
    (cond
      [(and (null? formal*)
            (null? args))
       env]
      [else
       (cons
         (cons (car formal*)
               (car args))
         (ext-env* (cdr formal*) (cdr args) env))])))

(define (prim-proc proc)
  (lambda (args)
    (apply proc args)))

(define initial-env
  `((apply   .   ,(prim-proc apply-proc))
    (cons    .   ,(prim-proc cons))
    (car     .   ,(prim-proc car))    
    (cdr     .   ,(prim-proc cdr))
    (cadr     .  ,(prim-proc cadr))
    (list    .   ,(prim-proc list))
    (null?   .   ,(prim-proc null?))
    (pair?   .   ,(prim-proc pair?))
    (+       .   ,(prim-proc +))
    (*       .   ,(prim-proc *))
    
    (quote   .   ,(list 'syntax-primitive eval-quote))
    (if      .   ,(list 'syntax-primitive eval-if))
    (lambda       .   ,(list 'syntax-primitive eval-lambda))
    (macro   .   ,(list 'syntax-primitive eval-macro))))
