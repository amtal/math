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
  (concat 1) ; collapse a list of monoids into one
  (behaviour_info 1)))

;;; Construct an identity instance.
;;;
;;; Equivalent to (mod):mempty(), included for completeness. Could try and
;;; implement monoid extensions for lists, dict, sets, orddicts, etc..?
(defun empty (mod) (call mod 'mempty))

;;; Merge two monoids.
(defun append (a b) 
  (let ((ma (element 1 a))
        (mb (element 1 b)))
    (orelse (== ma mb) (error #('mismatching_types a b)))
    (call ma 'mappend a b)))

;;; Merge a list of monoids into a singleton. Returns 'empty on empty list.
(defun concat 
  (('[]) 'empty)
  (((cons m ms)) (: lists foldl (fun append 2) m ms)))

(defun behaviour_info 
  (('callbacks) '[#(mempty 0) #(mappend 2)])
  ((_) 'undefined))
