;;; Rational numbers.
;;;
;;; Thanks to built-in bignums, can do arbitrary precision arithmetic.
(defmodule math_rational 
  (export (new 1) (new 2)
          (+ 2) (- 2) (* 2) (/ 2)
          (pi 0))
  (behaviour math_number)
  (import (rename erlang ((+ 2) :+) ((- 2) :-) ((* 2) :*))))

(defmacro rat (a b) `(tuple 'math_rational ,a ,b))

;; Construct rational from integer or float.
(defun new
  ((n) (when (is_integer n)) 
       (rat n 1))
  ((f) (when (is_float f)) 
       (cond ((?= (cons num den) 
                  (: math_alg fraction f #xFFFFFF)) ; what's a good precision?
              (rat num den))))                      ; error cap instead?
  ((e) (: erlang error 'badarg `(,e))))

;; Construct rational from numerator + denominator.
(defun new 
  ((num den) (when (/= 0 den) (is_integer num) (is_integer den))
             (rat num den))
  ((num den) (: erlang error 'badarg `(,num ,den))))

;; Rational field ops:

(defun + (((rat a b) (rat x y))
  (reduce (rat (:+ (:* a y) (:* x b)) (:* x y)))))
(defun - (((rat a b) (rat x y))
  (reduce (rat (:- (:* a y) (:* x b)) (:* x y)))))
(defun * (((rat a b) (rat x y))
  (reduce (rat (:* a x) (:* b y)))))
(defun / (((rat a b) (rat x y))
  (reduce (rat (:* x a) (:* y b)))))

;; Simplify a fraction by factoring out any GCD.
(defun reduce (((rat a b)) ; TODO: why's there a compile warning here?
  (cond ((== 0 a) (rat 0 b))  
        ((?= gcd (: math_alg gcd a b))
         (rat (div a gcd) (div b gcd))))))

;; Zu's ratio.
;;
;; Shortest <4 digit 6 decimal place approximation of pi.
;; See http://en.wikipedia.org/wiki/Mil%C3%BC for history.
(defun pi () 
  (rat 355 113))
