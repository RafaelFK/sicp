#lang sicp

#| Chapter definitions |#

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (abs x)
  (if (> x 0)
      x
      (- x)))



(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2.0))
  
  (define (good-enough? guess)
  (< (abs (- (square guess) x)) 0.001))
  
  (sqrt-iter 1.0))

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

(define (new-if predicate
               then-clause
               else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

#| Being an ordinary procedure (and not a special form) `then-clause` and
   `else-clause` in `new-if` will both be evaluated before its definition is applied,
   independent of the boolean value of `predicate`. In the special form `if`, its
   then-clause is only evaluated if its predicate evaluates to `true`. When we define
   `sqrt-iter`in terms of `new-if`, a recursion without a base case arises |#

#|
(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
|#

#| Exercise 1.7 |#

(define (new-sqrt x)
  (define (new-sqrt-iter guess previous-guess)
    (if (new-good-enough? guess previous-guess)
        guess
        (new-sqrt-iter (improve guess) guess)))

  (define (new-good-enough? guess previous-guess)
    (< (abs (- guess previous-guess)) .0000000001))

  (define (improve guess)
    (average guess (/ x guess)))

  (define (average x y)
    (/ (+ x y) 2.0))
    
  (new-sqrt-iter 1.0 0.0))

#| Exercise 1.8 |#

(define (cube-root x)
  (define (cube-root-iter guess previous-guess)
    (if (new-good-enough? guess previous-guess)
        guess
        (cube-root-iter (improve-cube-root guess) guess)))

  (define (new-good-enough? guess previous-guess)
  (< (abs (- guess previous-guess)) .0000000001))
  
  (define (improve-cube-root guess x)
    (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

  (cube-root-iter 1.0 0.0 x))