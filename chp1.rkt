#lang sicp

#| Chapter definitions |#

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#| Exercise 1.3 |#

(define (sum-largest-squares x y z)
  (sum-of-squares (max x y) (max (min x y) z)))

#| Exercise 1.4 |#

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

#| Exercise 1.5 |#

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

#| Exercise 1.6 |#

