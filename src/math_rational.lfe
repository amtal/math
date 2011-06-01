;;; Rational numbers.
;;;
;;; Thanks to built-in bignums, can do arbitrary precision arithmetic.
(defmodule math_rational 
  (export (new 1) (new 2)
          (+ 2) (- 2) (* 2) (/ 2)
          (pi 0))
  (behaviour math_number)
  (import (rename erlang ((+ 2) :+) ((- 2) :-) ((* 2) :*) ((/ 2) :/))))

(defmacro rat (a b) `(tuple 'math_rational ,a ,b))

;; Construct rational from integer or float.
(defun new
  ((n) (when (is_integer n)) 
       (rat n 1))
  ((f) (when (is_float f)) 
       (cond ((?= (cons num den) 
                  (: math_alg fraction f)) 
              (rat num den)))))

;; Construct rational from numerator + denominator.
(defun new (num den) 
  (rat num den)) 

(defun + (((rat na da) (rat nb db)) ;(+ na nb)))
  (rat (:+ (:* na db) (:* nb da)) 
       (:* da db))))
(defun - (a b) (: erlang - a b))
(defun * (a b) (: erlang * a b))
(defun / (a b) (: erlang / a b))

;; Zu's ratio.
;;
;; Shortest <4 digit 6 decimal place approximation of pi.
;; See http://en.wikipedia.org/wiki/Mil%C3%BC for history.
(defun pi () 
  (rat 355 113))

