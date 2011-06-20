;;; The field of Erlang values for which is_number/1 is true.
;;;
;;; With this wrapper, math-code can be written that isn't
;;; aware of the underlying data type. This might not be a waste of
;;; time: gonna have to wait and see.
(defmodule math_number
  (export (+ 2) (- 2) (* 2) (/ 2)
          (is_number 1) (new 1))
  (import (rename erlang ((is_number 1) is_num))))


;; Multi-argument and macro... Think this exists in the dev branch
;; of LFE, need to update?
(defmacro and (es
  (fletrec ((self [es] (if (== '() (cdr es)) 
                        (car es)
                        `(andalso ,(car es) ,(self (cdr es))))))
           (self es))))

;; Module dispatch for 2-arity operators, based on atom-tagged tuple.
;;
;; Handles "numbers" using the 'erlang module, coercing them
;; to other types if operated on with a tagged tuple.
(defmacro op [name]
  `(defun ,name [a b]
           ; normal numbers work as usual
     (cond ((and (is_num a) (is_num b))
            (: erlang ,name a b))
           ; atom-tagged tuples can also be used, if the
           ; tag is a module name that implements the interface
           ((and (is_tuple a) (is_tuple b) 
                 (== (element 1 a) (element 1 b)))
            (call (element 1 a) ',name a b))
           ; when arguments mismatch, but at least one tagged
           ; tuple is present the other argument is coerced
           ((and (is_tuple a) (is_num b))
            (let* ((mod (element 1 a))
                   (b (call mod 'new b)))
              (call mod ',name a b)))
           ((and (is_tuple b) (is_num a))
            (let* ((mod (element 1 b))
                   (a (call mod 'new a)))
              (call mod ',name a b)))
           ('true (error 'badarg `(,a ,b))))))


(op +) (op -) (op *) (op /) 

(defun is_number [n] 
  (orelse (is_num n)
          (andalso (is_tuple n) 
                   (call (element 1 n) 'is_number n))))

; Not sure if necessary:
(defun new [n] n) 


