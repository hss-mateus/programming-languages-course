#lang racket
(provide (all-defined-out))

(define x 3)
(define y (+ x 2))

(define (cube x)
    (* x x x))

(define (factorial x)
  (if (= x 0)
      1
      (* x (factorial (- x 1)))))

(define (pow x y)
  (if (= y 0)
      1
      (* x (pow x (- y 1)))))

(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

(define (sum2 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum2 (cdr xs)))]
        [(list? (car xs)) (+ (sum2 (car xs)) (sum2 (cdr xs)))]
        [#t (sum2 (cdr xs))]))

(define (count-falses xs)
  (cond [(null? xs) 0]
        [(car xs) (count-falses (cdr xs))]
        [#t (+ 1 (count-falses (cdr xs)))]))

(define (ones)
  (cons 1 ones))

(define (naturals)
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (f 1)))

(define (powers-of-two)
  (letrec ([f (lambda (x) (cons x (lambda () (f (* x 2)))))])
    (f 2)))

(define (stream-maker fn arg)
  (letrec ([f (lambda (x) (cons x (lambda () (f (fn x arg)))))])
    (f arg)))

(define fibonacci
  (letrec ([memo null]
           [f (lambda (x)
                (let ([ans (assoc x memo)])
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 1) (= x 2))
                                         1
                                         (+ (f (- x 1))
                                            (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))
