;;; Monoid (merging) behaviour.
;;;
;;;
;;; Monoids are structures that support merging and simple
;;; initialization, with some guaranteed intuitive properties.
;;;
;;; They show up everywhere - numbers, strings, and executable code are all
;;; monoids. They're espcially common in distributed and parallel systems. (Did
;;; I mention their properties are intuitive and easy to reason about?)
;;;
;;; By explicitly stating your type is a monoid, you make it easier to
;;; understand and reason about. You also get a nifty math_monoid:concat/1
;;; operation for free.
;;; 
;;;
;;; A module defining an abstract data type stored as a standard tagged tuple
;;; {mod_name, ...} can become a monoid by providing:
;;; 1. an "identity" instance of the type, via mempty/0 
;;; 2. a mappend/2 operation, that merges two instances into one.
;;;
;;; To be a monoid, these properties must hold:
;;; 1. mappend(A,mappend(B,C)) == mappend(mappend(A,B),C)
;;; 2. mappend(A,mempty()) == mappend(mempty(),A) == A
;;;
;;; In other words, associativity and an identity element. As a result:
;;; - monoids can be merged in any order
;;; - 'empty' values have no effect when merged in, and can be used to
;;;   represent 'no data'/failed computations/'do nothing' operations
;;;
;;; The resulting code follows well-defined convetions, and can be manipulated
;;; in an abstract fashion.
(defmodule math_monoid (export 
  (empty 1)  ; identity value of specific type
  (append 2) ; merge two monoids into one
  (is 1)     ; tests whether structure belongs to monoid-module
  (concat 1) ; collapse a list of monoids into one
  (behaviour_info 1)))

;;; Construct an identity instance.
;;;
;;; Equivalent to (mod):mempty(), included for completeness. Could try and
;;; implement monoid extensions for lists, dict, sets, orddicts, etc..?
(defun empty (mod) (call mod 'mempty))

;;; Merge two monoids.
;;;
;;; It is good practice for monoid implementation of mappend/2 to fail to match
;;; a function clause, if arguments are the wrong type.
(defun append (a b) (call (element 1 a) 'mappend a b))

;;; Monoid test.
(defun is 
  ((tup) (when (is_tuple tup))
   (let* ((mname (element 1 tup))
          (exports (call mname 'module_info 'exports)))
     (and (: lists member #(mempty 0) exports)
          (: lists member #(mappend 2) exports))))
  ((_) 'false))

;;; Merge a list of monoids into a singleton. 
;;;
;;; Errors out on empty list (can't resolve type). If worried about empty
;;; lists, cons on an identity element.
(defun concat 
  (('[]) (error 'badarg))
  (((cons m ms)) (: lists foldl (fun append 2) m ms)))

(defun behaviour_info 
  (('callbacks) '[#(mempty 0) #(mappend 2)])
  ((_) 'undefined))
