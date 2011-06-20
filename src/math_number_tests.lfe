;; Eunit doesn't compile .lfe files in test folder, will use manual method.
;; Whatever, this lets me run tests by hand while developing.
(defmodule math_rational_tests 
  (export all)
  (import (from math_number (+ 2) (- 2) (/ 2) (* 2))))


(defrecord meanvar (n 0) mean m2) 
  
(defun new [type]
  (make-meanvar mean (call type 'new 0)
                m2 (call type 'new 0)))

;; Online algorithm for tracking mean+variance.
;;
;; Has an interesting parallel version.
;; (update new-x meanvar-state)
(defun update ([x st]
  (let* (((match-meanvar n n
                        mean mean
                        m2 m2) st)
         (n (+ 1 n))
         (delta (- x mean))
         (mean (+ mean (/ delta n)))
         (m2 (+ m2 (* delta (- x mean)))))
    (tuple 'meanvar n mean m2))))

;; (read meanvar-state) -> (cons mean variance)
(defun read ([(match-meanvar n n m2 m2 mean mean)]
   (list (tuple 'mean mean)
         (tuple 'var (/ m2 (- n 1))))))
  
(defun test []
  (let* ((a (update -8 (update 4 (update 16 (new 'math_number)))))
         ((list #(mean 4.0) #(var 144.0)) (read a))
         (b (update -8 (update 4 (update 16 (new 'math_rational)))))
         ((list _ _) (read b)))
    'ok))
