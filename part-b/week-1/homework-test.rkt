#lang racket

(require "homework.rkt")

(require rackunit)

(define ones (lambda () (cons 1 ones)))
(define a 2)

(define tests
  (test-suite
   "Sample tests for Assignment 4"

   (check-equal? (sequence 0 5 1) (list 0 1 2 3 4 5) "Sequence test")

   (check-equal? (sequence -5 5 10) (list -5 5) "Sequence test")

   (check-equal? (string-append-map
                  (list "dan" "dog" "curry" "dog2")
                  ".jpg") '("dan.jpg" "dog.jpg" "curry.jpg" "dog2.jpg") "string-append-map test")

   (check-equal? (list-nth-mod (list 0 1 2 3 4) 2) 2 "list-nth-mod test")

   (check-equal? (stream-for-n-steps ones 2) (list 1 1) "stream-for-n-steps test")

   (check-equal? (stream-for-n-steps funny-number-stream 16) (list 1 2 3 4 -5 6 7 8 9 -10 11 12 13 14 -15 16) "funny-number-stream test")

   (check-equal? (stream-for-n-steps dan-then-dog 1) (list "dan.jpg") "dan-then-dog test")

   (check-equal? (stream-for-n-steps (stream-add-zero ones) 1) (list (cons 0 1)) "stream-add-zero test")

   (check-equal? (stream-for-n-steps (cycle-lists (list 1 2 3) (list "a" "b")) 3) (list (cons 1 "a") (cons 2 "b") (cons 3 "a"))
                 "cycle-lists test")

   (check-equal? (vector-assoc 4 (vector (cons 2 1) (cons 3 1) (cons 4 1) (cons 5 1))) (cons 4 1) "vector-assoc test")

   (check-equal? ((cached-assoc (list (cons 1 2) (cons 3 4)) 3) 3) (cons 3 4) "cached-assoc test")

   ;; (check-equal? (while-less 7 do (begin (set! a (+ a 1)) a)) #t "while-less test")

   ))

(require rackunit/text-ui)
(run-tests tests)
