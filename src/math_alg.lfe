(defmodule math_alg
  (export (gcd 2) (rat_approx 2)))

;; Euclid's Algorithm
(defun gcd
  ((0 n) n) ; GCF of two numbers doesn't change if the
  ((n 0) n) ; smaller is subtracted from the larger
  ((a b) (if (> a b) (gcd (- a b) b)
                     (gcd (- b a) a))))


;; Rational approximation to given real number.
;;
;; Port of David Eppstein's frap.c: www.ics.uci.edu/~eppstein/numth/frap.c
;;
;; Iterates over a 2x2 matrix storing two partial, truncated products of the
;; continuous fraction representation of the target number.
;;
;; Max denominator size affects readability and accuracy.
(defun rat_approx (flt max-den) 
  (case (catch (rat_approx 1 0 0 1 flt (trunc flt) max-den))
    ((tuple 'approx a b) (if (< (get-err flt a) 
                                (get-err flt b)) 
                           a b))
    ((tuple 'perf x) x)))

(defun rat_approx 
  ((a b c d x xi max-den) 
   (when (or (> x #x7FFFFFFF) ; got too close, floating point arithmetic went nuts
             (> (+ (* c xi) d) max-den))) ; denominator limit will be exceeded
   (let* ((xi' (trunc (/ (- max-den d) c)))
          (a' (lin a xi' b))
          (c' (lin c xi' d)))
     (tuple 'approx (cons a c) (cons a' c'))))
  ((a b c d x xi max-den) 
   (let* ((a' (lin a xi b))
          (c' (lin c xi d))
          (x' (if (== x xi) 
                ; perfect match found, exit before we divide by zero
                (throw (tuple 'perf (cons a' c')) )
                (/ 1 (- x xi)))))
     (rat_approx a' a c' c x' (trunc x') max-den))))
  
;; ax+b
(defun lin (a x b) (+ (* a x) b))

;; Error of rational approximation.
(defun get-err ((flt (cons n d)) 
  (abs (- flt (/ n d)))))
