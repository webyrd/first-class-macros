(define (eval exp env)
  (cond
    ((symbol? exp)    (env-lookup env exp))
    ((number? exp)    exp)
    ((boolean? exp)   exp)
    ((string? exp)    exp)

    ((if? exp)        (eval-if exp env))    
    ((quote? exp)     (eval-quote exp env))
    ((lambda? exp)    (eval-lambda exp env))
    
    ((app? exp)       (apply-proc (eval (app->fun exp) env) 
                                  (map (lambda (arg) (eval arg env)) 
                                       (app->args exp))))))

(define apply-proc
  (lambda (p args)
    (p args)))


;; (e1 e2* ...)
(define app->fun
  (lambda (exp)
    (car exp)))
(define app->args
  (lambda (exp)
    (cdr exp)))


(define empty-env '())

;; ((y . #t) (x . 5) (z . 7))
(define env-lookup
  (lambda (env x)
    (cond
      [(null? env) (error 'env-lookup "unbound variable")]
      [(eq? (caar env) x) (cdar env)]
      [else (env-lookup (cdr env) x)])))

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

;; (lambda (x ...) body)

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



;; (if e1 e2 e3)
(define if?
  (lambda (exp)
    (and (list? exp) (= (length exp) 4) (eq? (car exp) 'if))))

;; (quote datum)
(define quote?
  (lambda (exp)
    (and (list? exp) (= (length exp) 2) (eq? (car exp) 'quote))))

;; (lambda (x ...) body)
(define lambda?
  (lambda (exp)
    (and (list? exp) (= (length exp) 3) (eq? (car exp) 'lambda))))

;; (e1 e2* ...)
(define app?
  (lambda (exp)
    (and (list? exp) (>= (length exp) 1))))
