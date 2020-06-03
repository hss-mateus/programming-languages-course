#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (if (or (< high low) (> low high))
      null
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: negative number")
      (if (empty? xs) (error "list-nth-mod: empty list")
          (let ([i (remainder n (length xs))])
            (car (list-tail xs i))))))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number-stream)
  (letrec ([aux (lambda (i)
                  (lambda ()
                    (if (= (modulo i 5) 0)
                        (cons (- i) (aux (+ i 1)))
                        (cons i (aux (+ i 1))))))])
    ((aux 1))))

(define (dan-then-dog)
  (let ([aux (lambda () (cons "dog.jpg" dan-then-dog))])
    (cons "dan.jpg" aux)))

(define (stream-add-zero s)
  (lambda ()
    (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (lambda ()
    (cons (cons (car xs) (car ys)) (cycle-lists
                                    (append (cdr xs) (list (car xs)))
                                    (append (cdr ys) (list (car ys)))))))

(define (vector-assoc v vec)
  (letrec ([aux (lambda (i)
                  (if (>= i (vector-length vec))
                      #f
                  (let ([x (vector-ref vec i)])
                    (if (and (pair? x) (= v (car x)))
                        x
                        (aux (+ i 1))))))])
    (aux 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)])
    (lambda (v)
      (let ([res (vector-assoc v cache)])
        (if res
            (cdr res)
            (begin
              (vector-map! (lambda (x)
                             (if (not x) (cons v (assoc v xs)) x))
                           cache)
              (if (not (vector-assoc v cache))
                  #f
                  (cdr (vector-assoc v cache)))))))))
