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

#| Linear recursive implementation |#
#|
(define (factorial n)
  (if (= n 1)
      n
      (* n (factorial (- n 1)))))
|#

#| Linear iterative implementation |#
(define (factorial n)
  (define (fact-iter product counter)
    (if (> counter n)
        product
        (fact-iter (* product counter) (+ counter 1))))

  (fact-iter 1 1))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

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
  
  (define (improve-cube-root guess)
    (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))

  (cube-root-iter 1.0 0.0))

#| Exercise 1.9 |#

#|

(define (+ a b)
  (if (= a 0)
       b
       (inc (+ (dec a) b))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9

Linear recursive process
|#

#|

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9

Linear iterative process
|#

#| Exercise 1.10 |#

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

#|

f(n) = 2n
g(n) = 2^n
h(n) = 2^...^2 (n times)
k(n) = 5n^2

|#
