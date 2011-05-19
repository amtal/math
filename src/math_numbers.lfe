;;; The field of Erlang values for which is_number/1 is true.
;;;
;;; With this wrapper, math-code can be written that isn't
;;; aware of the underlying data type. This might not be a waste of
;;; time: gonna have to wait and see.
(defmodule math_numbers (export (+ 2) (- 2) (* 2) (/ 2)))

(defun + (a b) (: erlang + a b))
(defun - (a b) (: erlang - a b))
(defun * (a b) (: erlang * a b))
(defun / (a b) (: erlang / a b))
