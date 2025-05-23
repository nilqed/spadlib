;;; -*- Package: User; Syntax: Common-Lisp; Mode: Lisp; Base: 10 -*-
;;; unify.lisp

(in-package "CL-USER")

;;; ****************************************************************
;;; Unification Algorithm ******************************************
;;; ****************************************************************
;;;
;;; Random implementations of unification.
;;;
;;; Written by Mark Kantrowitz, mkant@cs.cmu.edu, October 15, 1990.
;;;
;;; Sorry, no documentation. 
;;; 
;;; To do:
;;;    lazy unification

;;; ********************************
;;; Global Variables ***************
;;; ********************************
(defvar *failure* 'failed)

;;; ********************************
;;; Macros *************************
;;; ********************************
(defmacro xor (a b)
  `(or (and ,a (not ,b))
       (and ,b (not ,a))))

(defmacro nor (a b)
  `(and (not ,a) (not ,b)))

;;; ********************************
;;; Primitives *********************
;;; ********************************
(defun occurs (elt lst)
  "Returns t if elt occurs somewhere in lst"
  (cond ((null lst)
	 nil)
	((listp lst)
	 (or (occurs elt (car lst))
	     (occurs elt (cdr lst))))
	((atom lst)
	 (eq lst elt))))

;;; ********************************
;;; Variables **********************
;;; ********************************
(defun make-variable (x)
  (make-symbol (format nil "?~a" x)))

(defun variablep (item)
  "A variable is of the form ?name, where name is a symbol."
  (and (symbolp item)
       (char= (char (symbol-name item) 0)
	      #\?)))

(defun variable-lookup (var env)
  (let* ((binding (assoc var env))
	 (val (cdr binding)))
    (cond ((variablep val)
	   (variable-lookup val env))
	  ((null binding)
	   ;; Unbound variable, so the variable itself is returned.
	   var)
	  ((null val)
	   ;; Null variable value.
	   nil)
	  ((and (not (occurs var val))
		(apply-substitutions env val)))
	  (t val))))

(defun apply-substitutions (substitutions elt)
  (cond ((null elt)
	 nil)
	((listp elt)
	 (cons (apply-substitutions substitutions (car elt))
	       (apply-substitutions substitutions (cdr elt))))
	((variablep elt)
	 (variable-lookup elt substitutions))
	(t elt)))


;;; ********************************
;;; Recursive Unification **********
;;; ********************************
;;; See Rich & Knight, p. 181
(defun recursive-unify (l1 l2)
  (cond ((or (atom l1) (atom l2))
	 (cond ((eq l1 l2)
		nil)
	       ((variablep l1)
		(if (occurs l1 l2)
		    *failure*
		    (list (cons l1 l2))))
	       ((variablep l2)
		(if (occurs l2 l1)
		    *failure*
		    (list (cons l2 l1))))
	       (t
		*failure*)))
	((not (= (length l1) (length l2)))
	 *failure*)
	(t
	 (let ((subst nil))
	   (do* ((sl1 l1 (cdr sl1))
		 (e1 (car sl1) (car sl1))
		 (sl2 l2 (cdr sl2))
		 (e2 (car sl2) (car sl2)))
	       ((null sl1)
		subst)
	     (let ((s (recursive-unify e1 e2)))
	       (cond ((eq s *failure*)
		      (return *failure*))
		     (s
		      (setf sl1 (apply-substitutions s sl1))
		      (setf sl2 (apply-substitutions s sl2))
		      (setq subst (append s subst))))))))))


;;; ********************************
;;; Iterative Unfication ***********
;;; ********************************
(defun unify (pattern data &optional env trace)
  "This is a fast iterative implementation of unification. It eliminates
   the need for a stack in a manner similar to tail-recursion. We model the
   flow of control in unification by saving untested pattern and data elements
   on a \"continuation stack\". At any point of the program, we are either
   updating the iteration variables or testing a pattern element against
   a data element (which must then be either atoms or variables). If this
   test fails, we return *failure* immediately. Otherwise, we accumulate
   any substitutions in the environment, which will ultimately be returned." 
  (let ((rest-pattern nil)		; these act as continuations
	(rest-data nil)
	binding)
    (loop
     (when trace
       ;; For debugging.
       (format t "~&Pattern:~T~A  ~&Data:~T~A  ~&Environment:~T~A"
	       pattern data env))
     (cond ((or (and pattern (atom pattern))
		(and data (atom data)))
	    ;; We have a pattern and a data to match, at least one
	    ;; of which is a non-nil atom.
	    (cond ((eq pattern data)
		   ;; If pattern and data are identical, test next elements.
		   (setf data nil pattern nil))
		  ;; Note: we aren't doing any sort of occurrence check
		  ;; to see if variable lookup will lead to infinite
		  ;; loops. For example, (?a ?b) against (?b ?a), or
		  ;; even ?a against (b ?a).
		  ((variablep data)
		   ;; Lookup the variable, if possible.
		   (setf binding (assoc data env))
		   (if binding
		       ;; If there's a data binding, substitute and try again.
		       (setf data (cdr binding))
		     ;; If no data binding, add one and move on. 
		     (setf env (acons data pattern env)
			   data nil pattern nil)))
		  ((variablep pattern)
		   (setf binding (assoc pattern env))
		   (if binding
		       (setf pattern (cdr binding))
		     (setf env (acons pattern data env)
			   data nil pattern nil)))
		  (t 
		   ;; Match failed. Probably because of data-pattern mismatch.
		   (return *failure*))))
	   ((nor pattern data)
	    ;; If we've run out of pattern and data (both nil), check the 
	    ;; rest-pattern and rest-data.
	    (cond ((xor rest-pattern rest-data)
		   ;; If we have a mismatch, fail.
		   (return  *failure*))
		  ((nor rest-pattern rest-data)
		   ;; If we've run out there too, exit with the bindings.
		   (return env))
		  (t 
		   ;; Otherwise, pop from the remainder to get the next pair.
		   (setf pattern (pop rest-pattern))
		   (setf data    (pop rest-data)))))
	   ((and (listp pattern) (listp data))
	    ;; We have two lists, one of which isn't nil.
	    ;; Break it apart into bite-size chunks.
	    (push (rest pattern) rest-pattern)
	    (setf pattern (first pattern))
	    (push (rest data) rest-data)
	    (setf data (first data)))))))


;;; ********************************
;;; Examples ***********************
;;; ********************************
#|
* (unify '(a ?v (c e)) '(a b (?d e)))

((?D . C) (?V . B))
* (unify '(a ?v (c e)) '(a b (?d f)))

FAILED
* (time (unify '(a ?v (c e)) '(a b (?d e))))

Evaluation took:
  0.0 seconds of real time,
  0.0 seconds of user run time,
  0.0 seconds of system run time,
  0 page faults, and
  144 bytes consed.
((?D . C) (?V . B))
* (time (recursive-unify '(a ?v (c e)) '(a b (?d e))))

Evaluation took:
  0.0 seconds of real time,
  0.0 seconds of user run time,
  0.0 seconds of system run time,
  0 page faults, and
  280 bytes consed.
((?D . C) (?V . B))

* (recursive-unify '(?d ?c ?e) '(?c ?d ?c))

((?E . ?C) (?D . ?C))
* (recursive-unify '(?c ?d ?c) '(?d ?c ?e))

((?D . ?E) (?C . ?D))
* (unify '(?c ?d ?c) '(?d ?c ?e))

((?E . ?C) (?C . ?D) (?D . ?C))
* (unify '(?d ?c ?e) '(?c ?d ?c) nil t)

Pattern: (?D ?C ?E)  
Data: (?C ?D ?C)  
Environment: NIL
Pattern: ?D  
Data: ?C  
Environment: NIL
Pattern: NIL  
Data: NIL  
Environment: ((?C . ?D))
Pattern: (?C ?E)  
Data: (?D ?C)  
Environment: ((?C . ?D))
Pattern: ?C  
Data: ?D  
Environment: ((?C . ?D))
Pattern: NIL  
Data: NIL  
Environment: ((?D . ?C) (?C . ?D))
Pattern: (?E)  
Data: (?C)  
Environment: ((?D . ?C) (?C . ?D))
Pattern: ?E  
Data: ?C  
Environment: ((?D . ?C) (?C . ?D))
Pattern: ?E  
Data: ?D  
Environment: ((?D . ?C) (?C . ?D))
Pattern: ?E  
Data: ?C  
Environment: ((?D . ?C) (?C . ?D))
...
|#
