#lang racket
(provide (all-defined-out))

(struct var (string) #:transparent)
(struct int (num) #:transparent)
(struct add (e1 e2) #:transparent)
(struct ifgreater (e1 e2 e3 e4) #:transparent)
(struct fun (nameopt formal body) #:transparent)
(struct call (funexp actual) #:transparent)
(struct mlet (var e body) #:transparent)
(struct apair (e1 e2) #:transparent)
(struct fst(e) #:transparent)
(struct snd (e) #:transparent)
(struct aunit () #:transparent)
(struct isaunit (e) #:transparent)
(struct closure (env fun) #:transparent)

(define (racketlist->mupllist xs)
  (if (null? xs)
      (aunit)
      (apair (car xs) (racketlist->mupllist (cdr xs)))))

(define (mupllist->racketlist xs)
  (if (aunit? xs)
      null
      (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))))

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

(define (eval-under-env e env)
  (cond [(var? e)
         (envlookup env (var-string e))]

        [(add? e)
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1)
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]

        [(aunit? e) e]

        [(isaunit? e)
         (if (aunit? (eval-under-env (isaunit-e e) env))
             (int 1)
             (int 0))]

        [(int? e) e]

        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number in the first two arguments")))]

        [(closure? e) e]

        [(fun? e) (closure env e)]

        [(call? e)
         (let ([funexp (eval-under-env (call-funexp e) env)])
           (if (not (closure? funexp))
               (error "MUPL call applied to non-closure in the first argument")
               (let* ([funexp-fun (closure-fun funexp)]
                      [fun-name (fun-nameopt funexp-fun)]
                      [param-name (fun-formal funexp-fun)]
                      [param-value (eval-under-env (call-actual e) env)]
                      [cl-env (closure-env funexp)]
                      [funbody (fun-body funexp-fun)])
                 (if fun-name
                     (eval-under-env funbody (cons (cons param-name param-value)
                                                   (cons (cons fun-name funexp) cl-env)))
                     (eval-under-env funbody (cons (cons param-name param-value) cl-env))))))]

        [(apair? e)
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]

        [(fst? e)
         (let ([v (eval-under-env (fst-e e) env)])
           (if (apair? v)
               (apair-e1 v)
               (error "MUPL fst applied to non-apair")))]

        [(snd? e)
         (let ([v (eval-under-env (snd-e e) env)])
           (if (apair? v)
               (apair-e2 v)
               (error "MUPL fst applied to non-apair")))]

        [(mlet? e)
         (let ([name (mlet-var e)]
               [value (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons name value) env)))]

        [#t (error (format "bad MUPL expression: ~v" e))]))

(define (eval-exp e)
  (eval-under-env e null))

(define (ifaunit e1 e2 e3)
  (call (fun #f "flag" (ifgreater (isaunit (var "flag")) (int 0) e2 e3)) e1))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst)) (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4)
  (mlet "_x" e1
        (mlet "_y" e2
              (ifgreater (var "_x") (var "_y")
                         e4
                         (ifgreater (var "_y") (var "_x")
                                    e4
                                    e3)))))

(define mupl-map
  (fun #f "processor"
       (fun "map-rec" "mupl-list"
            (ifaunit (var "mupl-list")
                     (aunit)
                     (apair (call (var "processor") (fst (var "mupl-list")))
                            (call (var "map-rec") (snd (var "mupl-list"))))))))

(define mupl-mapAddN
  (mlet "map" mupl-map
        (fun #f "n"
             (call (var "map") (fun #f "x" (add (var "n") (var "x")))))))
