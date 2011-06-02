;;; Rational numbers.
;;;
;;; Thanks to built-in bignums, can do arbitrary precision arithmetic.
(defmodule math_rational 
  (export (new 1) (new 2) (is_rational 1) 
          (fraction 1) (mixed 1)
          (+ 2) (- 2) (* 2) (/ 2)
          (pi 0))
  (behaviour math_number)
  (import (rename erlang ((+ 2) :+) ((- 2) :-) ((* 2) :*))))

(defsyntax rat ((a b) (tuple 'math_rational a b)))


;;; Constructors/destructors:

;; Construct rational from integer or float.
(defun new
  ((n) (when (is_integer n)) 
       (rat n 1))
  ((f) (when (is_float f)) 
       (cond ((?= (cons num den) 
                  (: math_alg fraction f #xFFFFFF)) ; what's a good precision?
              (rat num den)))))                     ; error cap instead?

;; Construct rational from numerator + denominator.
(defun new ((num den) (when (/= 0 den))
  (reduce (rat num den))))

;; Fast type test.
(defun is_rational 
  (((rat _ _)) 'true)
  ((_) 'false))

;; Fraction a/b, as tuple.
(defun fraction (((rat n d)) (tuple n d)))

;; Mixed number a+(b/c), as 3-tuple.
;; If proper fraction, a is 0.
(defun mixed (((rat n d)) (tuple (div n d) (rem n d) d)))

;; Zu's ratio.
;;
;; Shortest <4 digit 6 decimal place approximation of pi.
;; See http://en.wikipedia.org/wiki/Mil%C3%BC for history.
(defun pi () 
  (rat 355 113))


;;; Operators:

;; Rational field ops:
(defun + (((rat a b) (rat x y))
  (reduce (rat (:+ (:* a y) (:* x b)) (:* x y)))))
(defun - (((rat a b) (rat x y))
  (reduce (rat (:- (:* a y) (:* x b)) (:* x y)))))
(defun * (((rat a b) (rat x y))
  (reduce (rat (:* a x) (:* b y)))))
(defun / (((rat a b) (rat x y))
  (reduce (rat (:* x a) (:* y b)))))


;;; Internals:

;; Simplify a fraction by factoring out any GCD.
(defun reduce (((rat a b)) ; TODO: why's there a compile warning here?
  (cond ((== 0 a) (rat 0 b))  
        ((?= gcd (: math_alg gcd a b))
         (rat (div a gcd) (div b gcd))))))

