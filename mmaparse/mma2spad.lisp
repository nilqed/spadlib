;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-
;; Mock MMA (A Lisp language mathematica-like system)
;; (c) copyright 1990, 1991 by Richard J. Fateman and Univ. of California
;;
;; https://people.eecs.berkeley.edu/~fateman/lisp/mma4max/more/copyright
;; Copyright (c) 1990-1992 Richard J. Fateman
;; Copyright (c) 1990-1992 The Regents of the University of California.
;; All rights reserved.
;; Permission to use, copy, modify, and distribute this software and its
;; documentation for any purpose, without fee, and without written agreement is
;; hereby granted, provided that the above copyright notice and the following
;; two paragraphs appear in all copies of this software.

;; IN NO EVENT SHALL RICHARD J. FATEMAN OR THE UNIVERSITY OF CALIFORNIA
;; BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR
;; CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OF THIS SOFTWARE AND ITS
;; DOCUMENTATION, EVEN IF RICHARD J. FATEMAN AND THE UNIVERSITY OF
;; CALIFORNIA HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; RICHARD J. FATEMAN AND THE UNIVERSITY OF CALIFORNIA SPECIFICALLY
;; DISCLAIM ANY WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
;; THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS, AND RICHARD J.
;; FATEMAN AND THE UNIVERSITY OF CALIFORNIA HAVE NO OBLIGATION TO PROVIDE
;; MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.

;; ===============================================
;; mma2cl -- MMA to Common-Lisp parser (SBCL ONLY)
;; https://github.com/nilqed/mma2cl
;; --------------------------------
;; Make: load this file 
;;   $ sbcl --load mma2cl
;;   * (make-image "mma2cl")
;;     [undoing binding stack and other enclosing state... done]
;;     [performing final GC... done]
;;     [saving current Lisp image into mma2cl:
;;     writing 5968 bytes from the read-only space at 0000000020000000
;;     writing 1712 bytes from the static space at 0000000020110000
;;     writing 45481984 bytes from the dynamic space at 0000001000000000
;;     done]
;;     Process sbcl exited with code 0 
;; Usage: mma2cl <mma-file>
;; mma2cl-main, make-image at the end of this file.
;;

;; Usage: 
;;  (p) ............ parse interactively
;;  (ps file) ...... parse from file
;;  (pstring str) .. parse from string
;;

;; Examples:
#|
* (mma::pstring "")
NIL
* (mma::pstring "X+Y")
(MMA::|Plus| X Y)
* (mma::pstring "X*Y")
(MMA::|Times| X Y)
* (mma::pstring "X*Y+Z")
(MMA::|Plus| (MMA::|Times| X Y) Z)
*
(mma::pstring "Exp[X*Y+Z]")
(|Exp| (MMA::|Plus| (MMA::|Times| X Y) Z))
* (mma::pstring "D[Exp[X*Y+Z],X]")
(D (|Exp| (MMA::|Plus| (MMA::|Times| X Y) Z)) X)
*

* (mma::p)
Hello
|Hello|
* (mma::p)
Sin[x*y]+X*(a+b-c)
(MMA::|Plus| (|Sin| (MMA::|Times| |x| |y|))
 (MMA::|Times| X (MMA::|Plus| |a| |b| (MMA::|Times| -1 |c|))))
*
|#

#-sbcl (progn (format t "~a~%" 
  "Only for SBCL at the moment5, sorry ;)")(cl-user::quit))

(sb-ext:unlock-package :common-lisp)

(defpackage :mma (:nicknames "MockMMA") (:use :common-lisp))

;; keep pattern vars and maybe local function names away from user

;(defpackage :loc (:nicknames "MockMMAlocal") 	    (:use :common-lisp ))
;(defpackage :pat (:nicknames "MockMMApatvars") 	    (:use :common-lisp ))

(in-package :mma)
;; this next line is not enough.. need to have these macros
;; available at compile time.
;;(declaim (ftype macro ulist uconsm))
;;(load "ucons1")
(defparameter  built-in-syms
    ;; these are the atoms used by the parser, evaluator, display,
    ;; etc.  They must be the same in each of the separate packages,
    ;; and so each package should be in this package ( :mma).
	  
  '(|$Line| |Abs| |AddTo| |Alias| |Alternatives| |And| |Apply|
  |ArcCos| |ArcCosh| |ArcSec| |ArcSech| |ArcSin| |ArcSinh| |ArcTan|
  |ArcTanh| |Attributes| |Batch| |Blank| |BlankNullSequence| |BlankSequence|
  |Block| |Clear| |Comparison| |Complex| |CompoundExpression|
  |Condition| |Constant| |Cos| |Cot| |D| |Decrement| |Default|
  |Delayed| |Derivative| |Display| |DivideBy| |Do| |Dot| |E| |Equal| |Erf| |Every|
  |Exit| |Exp| |ExpIntegralEi| |Factorial| |Factorial2| |First| |Flat|
    |FullForm| |Function| |Gradef| |Greater| |GreaterEqual| |HoldAll|
    |HoldFirst| |HoldRest| |If| |Im|
  |In| |Increment| |Inequality| |Int| |Integer| |IntegerQ| |Less|
  |LessEqual| |List| |ListQ| |Log| |Map| |MapAll| |MessageName|
  |Module| |N| |NonCommutativeMultiply| |Not| |Null| |Null| |NumericQ|
  |Optional| |Or| |Orderless| |Out| |Part| |Pattern| |PatternTest|
  |Pi| |Plus| |Plus| |PossibleZeroQ| |Power| |Power| |PreDecrement|
  |PreIncrement| |Precision| |Print| |PutAppend| |Quote| |Rat| |RatExpand|
  |Rational| |Re| |Real| |Repeated| |RepeatedNull| |Replace|
  |ReplaceAll| |ReplaceRepeated| |Rest| |Rest| |Return| |Rule|
  |RuleDelayed| |SameQ| |Scan| |Sec| |Sequence| |Set| |SetAttributes|
  |SetDelayed| |Simp| |Sin| |Sinh| |Slot| |SlotSequence| |Some| |Sqrt|
  |SubtractFrom| |Table| |TagSet| |TagSetDelayed| |Tan| |Times| 
  |Timing| |TimesBy| |UnAlias| |UnRat| |UnSameQ| |UnSet| |Unequal|
  |UpSet| |UpSetDelayed| |mPut| ) )


;;(eval-when (:load-toplevel :compile-top-level)

  (defparameter caps-built-in-syms 
    (map 'list    #'(lambda(r) (intern (map 'string #'char-upcase (symbol-name r)) :mma))
	 built-in-syms))
  (export '(tl mread1))
  
;;  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ucons1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-kfp (provide 'ucons1)

;; (c) 1990, 1991, Richard J. Fateman

(in-package :mma)
;; alternative to ucons1 file
;; for non-Allegro CL.  This is a much inferior version in
;; efficiency of the unique-ification, and any CL could do
;; better. But maybe not the same way.

;;Simplest way to make the substitution would be to rename this
;; file ucons1.lisp.



(defvar *uniq-table* (make-hash-table :test #'eq))
(defvar *uniq-atom-table* (make-hash-table :test #'equal))

(defun uniq (x)
  "Return a canonical representation that is EQUAL to x,
  such that (equal x y) => (eq (uniq x) (uniq y))"
  (typecase x
    ((or fixnum symbol) x)
    (atom (or (gethash x *uniq-atom-table*)
              (setf (gethash x *uniq-atom-table*) x)))
    (cons (ucons (uniq (car x))   ; this could check in 
                                  ; *uniq-table* first...
                 (uniq (cdr x))))))

(defun ucons (x y)
  "Unique cons: (eq (ucons x y) (ucons x y)) is always true."
;; Look up the car, x, in the hash-table *uniq-table*.
;; If there a table there, then we have already hashed an
;; item with this car in the table.
;; If it is missing, create a hash-table for the purpose of
;; storing the new (cons x y) in the next step.

  (let ((car-table (or (gethash x *uniq-table*)
	               (setf (gethash x *uniq-table*)
                             (make-hash-table :test #'eq :size 10)))))

;;  At this point, car-table is a hash-table that either has
;;  (cons x y) in it, hashed under the key y, or we create 
;;  such an item and store it.

    (or (gethash y car-table)
        (setf (gethash y car-table) (cons x y)))))



(defun umapcar(f x)(cond((null x)nil)
			(t (ucons (funcall f (car x))(umapcar f (cdr x))))))

(defmacro ulist(&rest l)(cond ((null l)nil)
			      (t `(ucons ,(car l) (ulist ,@(cdr l))))))

(defun uappend(r s)(cond ((null r)s)
			 (t (ucons (car r)(uappend (cdr r) s)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -*- Mode:Common-Lisp;Package:mma; Base:10 -*-

;; Lisp-mathematica (Lmath) parser for Mathematica (tm)-like language.
;;(c) copyright 1990, 1991 by Richard J. Fateman
;; Last revised 5/29/91 by RJF
;; Mathematica is described in S. Wolfram: Mathematica, a
;; System for Doing Mathematics By Computer, (Addison-Wesley).
;; this line is not quite enough. Need to do, prior to compiling this
;; file, (set-case-mode :case-sensitive-lower)

;; parts of syntax in version 7 currently missing in this parser:
;;  span  e[[i;;j]]  which is Part[e,Span[i,j]]
;;  stringjoin  a<>b
;; "scientific notation"  34*^10
;; base..  b^^nnn
;; symbol in context vv`nn   [requires introducing name spaces?]
;; continuing at end of line \
;; The rule for ending a line iff the expression is apparently complete
;; is unreliable.


#+ignore ;; just use default case
(eval-when (compile load eval)
	   #+Allegro(cond((eq *current-case-mode* :case-sensitive-lower))
			 (t (set-case-mode :case-sensitive-lower))))

(declaim (optimize (speed 3)(safety 0)))



;;(provide 'math-parser)
;;(eval-when (:compile-toplevel) (load "mma")) ;; get all the symbols from this file
(in-package :mma)

;;(export '(p  pc rc))

(defvar mathbuffer nil) 
(defvar stream t) ;; if needed
(defmacro rt()`(cond((null mathbuffer)(mread1))
		(t (prog1 mathbuffer (setq mathbuffer nil )))))

;; The first section consists of readtable hacking for mathematica parser.
;; We set up a separate readtable for
;; mathematica input, and utilize it when scanning.
;; We use lisp atoms to store information on tokens.
;; For production, this could all be put in a Lisp package.

(defvar mathrt (copy-readtable nil))
#-GCL (setf (readtable-case mathrt) :preserve) ;gcl currently has no readtable-case


(defvar si (make-synonym-stream '*standard-input*))



(setq *print-level* nil *print-length* nil *print-pretty* t)

(defun mysignal(&rest f) (apply #'format (cons t (cdr f)))(signal (car f)))

(defun pc()(peek-char nil stream nil 'e-o-l nil))
;;(defun pc()(peek-char nil stream nil  #\newlineq))
(defun rc()(read-char stream nil 'e-o-l))
;;an idea that won't work to echo because we read with read-preserving-white-space, mostly,  not rc
#+ignore (defun rc()(princ (read-char stream nil 'e-o-l))) 

(defun char-to-int (c)  ;; return the integer 0-9 corresponding to
			;; the character c, #\0 - #\9
  ;; will not work in larger bases though..
  (let ((h (char-int c)))
    (cond ((< h 48)(- h 7))  ;; #\A=17
	  ((< h 58)  (- h  48)) ; #\0 is 48 in ascii.
	  (t (- h 87)) ; #\a=97
	     )))
(defun collect-integer (val r)
  (cond ((member (pc) '(e-o-l #\newline):test 'eql) val)
	((digit-char-p (pc) r)	;r is radix

	 (collect-integer (+ (char-to-int (rc))(* r val)) r))
;;	((eql (pc) #\`)(rc)(collect-integer val r)) ;;option 123`456 is 123456.
	(t val)))

;; to test scanner, try typing
;;  (mreadl)  

;; most of these read-table entries were generated by macro expansion
(set-macro-character #\/
  #'(lambda 
     (stream char)
     (declare (ignore char))
;;     (format t "processing slash")

     (case (pc)
	   (#\newline '/)
	   (#\: (rc) '|/:|)
	   (#\. (rc) '/.)
	   (#\@ (rc) '/@)
	   (#\; (rc) '|/;|)
	   (#\= (rc) '/=)
	   (#\/ (rc)
		(case (pc) (#\newline '//) (#\@ (rc) '//@) (#\. (rc) '//.) (t '//)))
	   (t '/  )))
  nil mathrt)

(set-macro-character #\^
  #'(lambda 
     (stream char)
     (declare (ignore char))
     (case (pc)
	   (#\newline '^)
	   (#\= (rc) '^=)
	   (#\^ (rc) '^^)
	   (#\: (rc)
		(case (pc) (#\newline '|^:|) (#\= (rc) '|^:=|) (t '|^:|)))
	   (t '^)))
  nil mathrt)

(set-macro-character #\&
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '&) (#\& (rc) '&&) (t '&)))
  nil mathrt)

(set-macro-character #\|
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '\|) (#\| (rc) '\|\|) (t '\|)))
  nil mathrt)

(set-macro-character #\+
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '+) (#\+ (rc) '++) (#\= (rc) '+=) (t '+)))
  nil mathrt)

(set-macro-character #\*
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '*) (#\* (rc) '**) (#\= (rc) '*=) (t '*)))
  nil mathrt) 

(set-macro-character #\-
  #'(lambda 
     (stream char)
     (declare (ignore char))
     (case (pc)
	   (#\newline '-) (#\> (rc) '->) (#\= (rc) '-=) (#\- (rc) '--) (t '-)))
  nil mathrt)

#+ignore ;; don't allow [[ as a token
(set-macro-character #\[
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '[) (#\[ (rc) '[[) (t '[))) 
  nil mathrt)

(set-macro-character #\[
  #'(lambda (stream char)
      (declare (ignore char))
      '[) 
  nil mathrt)

#+ignore
(set-macro-character #\]
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline ']) (#\] (rc) ']]) (t '])))
  nil mathrt)

(set-macro-character #\]
  #'(lambda (stream char)
      (declare (ignore char))
      ']) 
  nil mathrt)

(set-macro-character #\{
  #'(lambda (stream char)
      (declare (ignore char)) 
      '{) ; fixed 2/21/91 lvi@ida.liu.se
  nil mathrt) 

(set-macro-character #\<
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '<) (#\= (rc) '<=) (t '<)))
  nil mathrt) 

(set-macro-character #\>
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '>)
		  (#\= (rc) '>=)
		  (#\> (rc)
		       (case (pc) (#\newline '>>) (#\> (rc) '>>>) (t '>>)))
		  (t '>)))
  nil mathrt) 

(set-macro-character #\!
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '!) (#\! (rc) '!!) (#\= (rc) '!=) (t '!)))
  nil mathrt) 

(set-macro-character #\#
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc) (#\newline '|#|) (#\# (rc) '|##|) (t '|#|)))
  nil mathrt)

(set-macro-character #\\ 
#'(lambda(stream char)
    (declare (ignore char))
    (case (pc)
	(#\newline (rc) (mread1)) ;; \ at end of line -> splice
	(t (intern (make-string 1 :initial-element (rc))))
	; \ within line, ignore the \ and return the next char
	))
nil mathrt)

(set-macro-character #\= 
  #'(lambda(stream char)
      (declare (ignore char))
      (case (pc)
		 (#\newline '|=|)
		 (#\= (rc) 
		      (case(pc) (#\newline '|==|) (#\= (rc) '|===|) (t '|==|)))
		 (#\! (rc) (case(pc)
				(#\newline '|=!|) ;unused
				(#\= (rc) '|=!=|)
				(t '|=!|)))
		 (t '|=|))) nil mathrt)

(set-macro-character #\. 
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '|.|)
		  (#\. (rc) 
		       (case (pc)
			     (#\newline '|..|) (#\. (rc) '|...|) (t '|..|)))
		  (t '|.|)))
  nil mathrt)

(set-macro-character #\:
  #'(lambda (stream char)
      (declare (ignore char))
      (case (pc)
		  (#\newline '|:|)
		  (#\> (rc) '|:>|)
		  (#\: (rc)
		       (case (pc) 
			     (#\newline '|::|) (#\= (rc) '|::=|) (t '|::|)))
		  (#\= (rc) '|:=|)
		  (t '|:|)))
  nil mathrt)

(set-macro-character #\' #'(lambda (stream char)
			     (declare (ignore char))
			     '|'|) nil mathrt) 

(set-macro-character #\@ #'(lambda (stream char)
			     (declare (ignore char))
			     (case (pc) (#\newline '@)(#\@ (rc) '@@)(t '@)))
			     nil mathrt) 
;; above fixed by lvi@ida.liu 3/20/92

(set-macro-character #\~ #'(lambda (stream char)  (declare (ignore char))
			     '~) nil mathrt) 
(set-macro-character #\? #'(lambda (stream char)  (declare (ignore char))
			     '?) nil mathrt) 
(set-macro-character #\) #'(lambda (stream char)  (declare (ignore char))
			     '|)|) nil mathrt) 
(set-macro-character #\} #'(lambda (stream char)  (declare (ignore char))
			     '}) nil mathrt) 
(set-macro-character #\; #'(lambda (stream char)  (declare (ignore char))
			     '|;|) nil mathrt) 
(set-macro-character #\, #'(lambda (stream char)   (declare (ignore char))
			     '|,|) nil mathrt) 
(set-macro-character #\newline #'(lambda(stream char)  (declare (ignore char))
				   'e-o-l) nil mathrt)

(mapc #'(lambda(x) (setf (get x 'mathtoken) t))
      '(/ |/:| /. /@ |/;| /= // //@ //.
	  ^ ^= ^^ |^:=| |^:| 
	  & && \| \|\| + ++ +=  ** *= 
	  - -> -= -- [  ]  { } > >= >> >>> < <=
	  ! !! != 
	  |#| |##|
	  |:=| |:>|  |::| |::=| |:|
	  |=| |==| |===| |=!=|
	  |.| |..| |...| \\ 
	  e-o-l |(| |)|
	  |'| @ ~ ? |;| |,|))

;; |# +kfp --> end gray comment ;)	  
;;  Extension.  This allows us to use foo[*,1]*bar[1,*] notationally.
;; also a * *  means  (Times a *)
;;(setf (get '* 'mathtoken t))

(set-macro-character #\_
		     #'(lambda (stream char &aux next)
			 (declare (ignore char))
	    (case
	     (pc)
	     (#\Newline '(|Blank|))  ; _
	     (#\. (rc)
		  '(|Optional| (|Blank|)))  ;_.
	     (#\_ (rc)
		  (case
		   (pc)
		   (#\Newline '(|BlankSequence|))  ;__
		   (#\_
		    (rc)  ;___ (3 of em)
		    (cond ((and (typep (pc) 'character)(alpha-char-p (pc))
				(setq next(rt)))
			   `(|BlankNullSequence| ,next))
			  (t '(|BlankNullSequence|)))) 
		   (t  ;; __ (2 of em)
		       (cond ((and (typep (pc) 'character)(alpha-char-p (pc))
				   (setq next(rt)))
			      `(|BlankSequence| ,next))
			     (t '(|BlankSequence|))) 
		       )))  
	     (t ; _ (1 of em)
		(cond ((and (typep (pc) 'character)(alpha-char-p (pc))
			    (setq next(rt)))
		       `(|Blank| ,next))
		      (t '(|Blank|))))))
  nil
  mathrt)

;; left paren could start a comment


(defun sawlpar (stream char)  ;; comments are (* any text *)
  (declare (ignore char))
  (case (pc)
	(#\* ;skip to end of comment
	     (rc)
	     (commentskip stream))
	(t '\())) ;)

(set-macro-character #\( #'sawlpar nil mathrt)  ;)

;; the use of the % character is peculiar.
(set-macro-character #\% 
  #'(lambda(stream char)
      (declare (ignore char))
      (cond((eq(pc) #\%) (parse-outform1 2))
		((and (typep (pc) 'character)(digit-char-p (pc))) ;in case (pc) is e-o-l
		 `(|Out|,(collect-integer 0 10)))
		(t '(|Out|))))
  nil mathrt)

(defun parse-outform1(counter) ; saw more than one % 
	 (rc)
	 (cond ((equal (pc) #\%) (parse-outform1 (+ 1 counter))) ;another %
	       (t `(|Out| ,(- counter)))))
  

(defun commentskip (stream &aux x )
  (loop
   (setq x (rc))
    (cond 
	  ((eql x #\( ) (sawlpar stream x))  
	 ((and (eql x #\* )
	       (eql (pc) #\) ))
	  (rc)				; flush the last leftpar
	 (return(mread1)))   ;return next item
	 )))



 
;;; end of the lexical analysis part
;;----------------------------------------------------------
;;; 			The Parser
;; You can use (p)  to try out the parser by typing in from the
;; keyboard. It sets up the readtable and calls parse-comp.

;; Reading from lines is set up so that if a sentence ends at
;; an end-of-line, the parse is completed. Otherwise, the e-o-l
;; is absorbed and the reading continued.  A continuation line
;; can be forced by a \.  (This is Mathematica's usual operation)

(defvar interactive t) ; t means 2 eol's ends a command. not for files.



;;  ps will read from a Mathematica stream  // print to std output
;; e.g.  (ps (open "foo.text"))

(defun ps(stream  &aux (interactive nil) 
		       res 
		       (*readtable* mathrt)
		       (mathbuffer nil)
		       z)

;  (rt)
  
  (loop (setq res (catch 'endofparse(parse-comp t)))  ;; end=t means a #\newline will end expr.
  (print (cond ;((null res) (return 'done))
	      ((eq #\newline (pc)) 
	       (rc) 
	       res) ;; proper ending
	       ((setq z(rt))
	       (cond ((equal z 'e-o-l))  ;;may also be proper ending
		     (t(format t "~%garbage at end of expression:~s~%" z )))
	       res)))
    ;; test for eof on stream
    (if (eq 'eof (peek-char nil stream nil 'eof))(return 'done))
    ))

;; Starting from a character string,
;; parse it into a single (Mock) Mathematica expression
;; and return a lisp expression.

(defun pstring(string  &aux (interactive nil) 
		       (*readtable* mathrt)
		       (mathbuffer nil)
		       )
  ;;;+kfp format with a newline ~%
  (let ((stream  (make-string-input-stream (format nil "~A~%" string))))
    ;(ps stream)))
    (parse-comp t)))

;; example: (defvar r (mma::pstring "Sin[x]")) ==> (|Sin| |x|)
;;          (defvar r2 (mma::pstring "D[Sin[x],x]")) ==> (D (|Sin| |x|) |x|)
;; 


;;; same as ps, but translate to macsyma.
;;; not really useful except as toy demo

(defun pst(stream  &aux (interactive nil) 
		       res 
		       (*readtable* mathrt)
		       (mathbuffer nil)
			z)

;  (rt)
  (loop (setq res (catch 'endofparse(parse-comp t)))  ;; end=t means a #\newline will end expr.
    (print (mma2max
	    (cond			;((null res) (return 'done))
	      ((eq #\newline (pc)) 
	       (rc) 
	       res) ;; proper ending
	       ((setq z(rt))
	       (cond ((equal z 'e-o-l))  ;;may also be proper ending
		     (t(format t "~%garbage at end of expression:~s~%" z )))
	       res))))
    ;; test for eof on stream
    (if (eq 'eof (peek-char nil stream nil 'eof))(return 'done))
    ))


#+ignore ;; see mma2max in file mma2maxfun.lisp
(defun tomacsyma(r)
  (cond ((numberp r) r)
	((symbolp r)
	 (let ((l (assoc r macsubs)))
	   (cond (l (cdr l)) ;found a translation
		 (t (intern (format nil"$~a"r)))))) ;; a symbol foo -> $foo or |$foo| perhaps
	;; here we should check for while, for, other complex stuff
	(t (mapcar #'tomacsyma r))))

;; . while, for, ordinary function calls.

;; while n>0 do (print(n),n:n-1) looks like this...
; ((MDO) NIL NIL NIL NIL NIL ((MNOT) ((MGREATERP) |$n| 0))
; ((MPROGN) (($PRINT) |$n|) ((MSETQ) |$n| ((MPLUS) |$n| ((MMINUS) 1)))))

;mreadl is a debugging loop that just reads lexemes until it reads done

#+ignore
(defun mreadl(&aux (stream *standard-input* ) next (*readtable* mathrt))
  (loop 
   (setq next (mread1))
   (when (eq  next 'e-o-l) (return 'done))
   (print next)))


(defmacro eolp(end) ;;used all over to see if we've reached an end of line
  `(and ,end (eq 'e-o-l (peek-token))))

;; this function reads a token. Although it looks like it
;; just reads a lisp s-expression or number, it uses a different
;; read-table. If mread1 encounters a #\newline, it returns the
;; atom e-o-l, as specified in the read-table.

;; +KFP , avoiding using batch.lisp
(defun chash (m) m)

(defun mread1()
;    (format t "~% next char = ~s" (pc)) ;; debug
  (cond ((eq 'e-o-l (pc) ) '|Null|) ;;???
	((digit-char-p (pc));; next character is a digit 0-9
	 (collect-integer 
	  (char-to-int(rc)) 10) )
				;radix 10 default
	(t (let* ((rr (read-preserving-whitespace stream nil 'e-o-l))
		 (c(chash #+:allegro rr
			  #-:allegro (recase rr))))	  
	    c)
	   ;; nil reads as False
	   )))

(defun p (&optional(stream *standard-input*) 
		  &aux (interactive t)
		  res
		  (*readtable* mathrt)
		  (mathbuffer nil))
;  (rt) ;;get something in mathbuffer
  (setq res (catch 'endofparse (parse-comp t)))  ;; end=t means a #\newline will end expr.
  (cond((eq mathbuffer 'e-o-l)  (if res res '|Null|)) ;; proper ending
       (t (format t "~%Unexpected token at end of expression: ~a~%" mathbuffer)
	  (mysignal	   'syntax-error)
	  res)))
      

(defun peek-token() (cond(mathbuffer)
			 (t (setq mathbuffer(mread1)))))

(defun parse-nary1 (res tag)
  (cond ((null(cdr res))(car res))
	(t (cons tag (nreverse res)))))

(defun guess-token (guess &aux (tok (peek-token)))
  (cond((eql guess tok) t)
       ((eql 'e-o-l tok)(rt)
	(if (and interactive (eql'e-o-l (peek-token))) ;; if two in-a-row; get outta here
	    (throw 'endofparse nil)))))

;; a variable is any symbol that looks like a lisp symbol and
;; is not an integer or string, or a pattern-var

(defun var-p(token)
  (or (consp token) ;; case of (blank)
      (and 
       (not (integerp token))
       (not (eql token 'e-o-l))
       (or (stringp token) (not (get token 'mathtoken))))))

;; is Head one of the pattern items "blank..."
(defun blankp(token)
  (and(not (atom token))
      (member (car token) '(|Blank| |BlankSequence|  |BlankNullSequence|) :test #'eql)))


;; parse a number
(defun parse-number(end &aux (x (parse-int end)) afterdot) ;; reads floats and radix nums also
  (cond (x
	 (cond 
	  ((equal (pc) #\.); is the very next character a "."?
	   (rc) ;; remove exactly that character.
	   ;; note: in Mathematica, 1. 2 is 1.0*2 = 2.0
	   ;; 1 .2  is 1*0.2 = 0.2
	   ;; 1 . 2 is Dot[1,2]
	   ;;Now check: Is there a digit next?
	   (cond((eq (p) 'e-o-l) (make-real x 0))
		 ((digit-char-p (pc))
		 (setq afterdot (parse-frac end))
		 (cond (afterdot (make-real x afterdot)) ;;like 12.34
		       (t x)));      not a float -> return integer
		(t (make-real x 0)) ;a float of the form 1. 
		))
	  (t x))) ;;x is an integer, but no "." follows
	;; still, we must check for  a number of the form .123
	((guess-token '|.|)
	 (rt)
	 ;;is there a digit next?
	 (cond((eq (p) 'e-o-l) (make-real x 0))
	      ((digit-char-p (pc))
	       (setq afterdot (parse-frac end))
	       (cond (afterdot (make-real 0 afterdot)) ;;like 0.34
		     (t "what's a dot doing here?")))));; we could make it 0?
	(t nil) ))



;;parse an integer, including radix

(defun parse-int(end &aux (x (peek-token))) 
  (cond 
   ((integerp x)
	 (cond
	  ((eolp end) x)
	  ((and (rt) (eql (pc) #\^) ;; don't sop up extra spaces here. what if 1 .2
		(guess-token '|^^|)) ;; see if it is, e.g. 8^^101 =65
	   (rt)
	   (cond((or (> x 10)
		     (< x 2))
		 (format t "radix ~s ?~%" x)))
	   (collect-integer 0 x))
	  (t x))) ;; ok, no radix stuff -- just return x
	(t nil)))

;; parse the fraction part of a decimal number .123

(defun parse-frac(end &aux x (num 0)(den 1))
  (declare(ignore end))
 (loop
   ;; since all of the line termination chars are not digits, all we
   ;; need to check is for digits..  but digit-char-p errs out for non-characters. ugh. e-o-l not char
   (if  (or (eql (pc) 'e-o-l)(not(setq x(digit-char-p (pc))))) (return (/ num den)))
   (rc) ;; read past the char
   (setq den (* den 10))
   (setq num (+ (* 10 num) x))
))

;; this is a stub until we decide what to really do here
(defun make-real (x y) `(|Real| ,x ,y))

;; parse lists delimited by [] [[]]{}  tricky to  handle f[g[x]].

;; there are two possibilities here  f [ [ X ] ]  which is (Part f X)
;; and f [ X ]   which is (f X)
(defun parse-list (&optional op &aux next)
  (setq next (peek-token))
  (cond  ((equal next '\[)
         (rt)
         (parselist1 (list op) '\]))
        ((equal next '\{)
         (rt)
         (parselist1 (list '|List|) '\}))))

(defun parselist1 (sofar endmark &aux next) ;; we want to find an expression
  (setq next (peek-token ))
  (cond ((eq next '[) ;;create a Part, and absorb an extra '] at the end
	 (rt)
	 (prog1 (cons '|Part|
		      (parselist1  sofar
				   endmark))
	 ;  (format t "~%sofar =~s" sofar)
	   (if (eq(peek-token) '])
	       (rt)
	     (mysignal 'syntax-error "unmatched ]] at ~s" (car sofar)))))
	 ((eq next '\,)
	 (rt);; get past the comma
	 (parselist1 (cons nil sofar) endmark))
	 ((eq next endmark)
	  (rt);; get past the endmark [a,b,]
	  (cond ((null (cdr sofar)) 
;;		 (print 'xx)
		 sofar ) ;; f[] -> (f). 
		(t(nreverse (cons '|Null| sofar)))))
	 ((setq next (parse-comp nil)) ;; end=nil; can't end with just #\newline
	  (parselist2 (cons next sofar) endmark))
	 (t (mysignal 'syntax-error "parse-list: looking for a comma, expression or endmark"))
	 ))

(defun parselist2 (sofar endmark &aux next) ;; we want to find , or close mark
  (setq next (peek-token))
  (cond ((equal next '\,)
	 (rt);; get past the comma
	 (parselist1 sofar endmark))
	((equal next endmark) (rt) (nreverse sofar ))
	((equal endmark '\]) (rt)(nreverse sofar))
	(t (mysignal 'syntax-error "parse-list: looking for a comma, expression or endmark"))
	))

;;comparison operators
(setf (get '== 'compop) '|Equal|)
(setf (get '!= 'compop) '|Unequal|)
(setf (get '< 'compop) '|Less|)
(setf (get '<= 'compop) '|LessEqual|)
(setf (get '> 'compop) '|Greater|)
(setf (get '>= 'compop) '|GreaterEqual|)
(setf (get '=== 'sameop) '|SameQ|)
(setf (get '=!= 'sameop) '|UnSameQ|)

;; sample parses.  All comparisons of 3 or more items are questionable,
;; but this is what Mockmma does
;; a<b<c  (Comparison(a, Less b Less c)
;; a>b<c  (Comparison a Greater b Less c) 
;; a>b==c (Comparison a Greater b Equal c)
;; a+b==c  (Comparison (Plus a b) Equal c)

(defun parse-or (end &aux (temp (parse-and end)) res)  ; E::=e1||e2  n-ary
  (cond ((eolp end) temp)
	(temp
	 (cond ((guess-token '\|\|) ;;check first to avoid consing
		(setq res (cons temp nil))
		(loop
		 (cond ((eolp end) (return(parse-nary1 res '|Or|)))
		       ((guess-token  '\|\|)
			(rt)
			(setq res (cons (parse-and end) res)))
		       (t (return(parse-nary1 res '|Or|)))
		       )))
	       (t temp)))
	(t nil) ; not an or-expression
	))

(defun parse-and (end &aux (temp (parse-not end)) res)  ; E::=e1 && e2  n-ary  (And)
  (cond ((eolp end) temp)
	(temp
	 (cond ((guess-token '&&) ;;check first to avoid consing
		(setq res (cons temp nil))
		(loop
		 (cond ((eolp end)(return(parse-nary1 res '|And|)))
		       ((guess-token  '&&)
			(rt)
			(setq res (cons (parse-not end) res)))
		       (t (return(parse-nary1 res '|And|)))
		       )))
	       (t temp)))
        (t nil) ; not an and-expression
))

(defun parse-not(end)
  (cond((eolp end) nil)
       ((guess-token '|!|) ;; Not
	 (rt)
	 `(Not ,(parse-not end)))
	(t (parse-same end))))

;; this definition does not handle 3-way or more comparisons quite
;; the same as Mathematica. 
;; a===b is (SameQ a b) but a=!=b===c is (Comparison a SameQ b SameQ c)
;; rather than (Sameq (UnSameQ a b) c).
;; reason: probably Mathematica is wrong; probably the feature is unused
;; and hence un-noticed.

(defun parse-same (end &aux (temp (parse-equal end))res op)  ; E::=e1 ===e2 etc
  (cond ((eolp end) temp)
	(temp
	 (setq op (peek-token))
	 (cond ((and (atom op)(get op 'sameop)) ;; check before cons
		;;SameQ
		(setq res (cons temp nil))
		(loop
		 (cond ((eolp end) 
			(return (patch-equal(parse-nary1 res '|Comparison|))))
		       ((and (atom (setq op (peek-token)))
			     (setq op (get op 'sameop)))
			(rt)
			(setq res (cons (parse-equal end) (cons op res))))
		       (t (return (patch-equal(parse-nary1 res '|Comparison|))))
		       )))
	       (t temp)))
	(t nil) ; not a SameQ  or UnSameQ
	))

(defun parse-equal (end &aux (temp (parse-plus end))res op)  ; E::=e1 compop e2  n-ary  (==, etc)
  (cond ((eolp end) temp)
	(temp
	 (setq op (peek-token))
	 (cond ((and (atom op)(get op 'compop)) ;; check before cons
		;;Unequal, for example
		(setq res (cons temp nil))
		(loop
		 (cond ((eolp end) (return (patch-equal(parse-nary1 res '|Comparison|))))
		       ((and (atom (setq op (peek-token)))
			     (setq op (get op 'compop)))
			(rt)
			(setq res (cons (parse-plus end) (cons op res))))
		       (t (return (patch-equal(parse-nary1 res '|Comparison|))))
		       )))
	       (t temp)))
	(t nil) ; not an equal  or inequal -expression
	))
(defun patch-equal(h)
  #+ignore ;; change (Comparison a OP b) to (Op a b). e.g. (Greater a b)
  (if (= (length h) 4)
      (list (caddr h) (cadr h)(cadddr h)) h)
  h ;; leave Comparison, e.g. (Comparison a Greater b)
  )
;; arithmetic expression

(defun parse-plus (end &aux (temp (parse-times end)) res); E::=T1{+T2} | T1{-T2}
  (cond (temp
	 (cond 
	  ((eolp end) temp)
	  ((or (guess-token '+)(guess-token '-))
	   (setq res (cons temp nil))  
	   (loop
	    (cond ((eolp end) (return (parse-nary1 res '|Plus|)))
		  ((guess-token '+)
		   (rt)
		   (setq res (cons (parse-times end) res)))
		  ((guess-token '-)
		   (rt)
		   (setq res (cons 

			      (let ((h (parse-times end)))
				(if (numberp h) (- h)
				  `(|Times|   -1 ,h)
				     )) res)))
		  (t (return(parse-nary1 res '|Plus|))))))
	  (t temp)))
	(t nil)) ; not a  Plus expr
  )

(defun parse-comp (end &aux temp res )  ; E::=E;E;  | E;
  (cond ((setq temp (parse-put end))
	 (cond ((eolp end) temp)
	       ((guess-token '|;|) ;;check first to avoid consing
		(setq res (cons (if temp temp '|Null|) nil))
		(loop
		 (cond ((eolp end) (return(parse-nary1 res '|CompoundExpression|)))
		       ((guess-token  '|;|)
			(rt)
			(setq res (cons (or(parse-put end) '|Null|) res)))
		       (t(return (parse-nary1 res '|CompoundExpression|))))))
	       (t temp)))
	(t nil)) ; not a compound expr -- something wrong --
  )



(defun parse-put( end &aux (temp (parse-set end))) ; e >> file or e>>>file
  (cond(temp
	(cond((eolp end) temp)
	     ((guess-token '>>)(rt)`(|Put| ,temp ,(rt)))
	     ((guess-token '>>>)(rt)`(|PutAppend| ,temp ,(rt)))
	     (t temp)))
       (t nil)))

;;replace is left-assoc    e /. e   |  e//.e

(defun parse-replace( end &aux(temp(parse-rule end)))
  (cond (temp (parse-replace1 temp end))
	(t nil)))

(defun parse-replace1(temp end)
  (cond ((eolp end) temp)
	((guess-token '|/.|)
	 (rt)
	 (parse-replace1 `(|ReplaceAll| ,temp ,(parse-replace end)) end))
	((guess-token '|//.|)
	 (rt)
	 (parse-replace1 `(|ReplaceRepeated| ,temp ,(parse-replace end)) end))
	(t temp)))

(defun parse-rule(end &aux (temp (parse-condition end)))  ;e->(e->e) etc
  (cond(temp (cond ((eolp end) temp)
		   ((guess-token '|->|)
		    (rt)
		    `(|Rule| ,temp ,(parse-rule end)))
		   ((guess-token '|:>|)
		    (rt)
		    `(|RuleDelayed| ,temp ,(parse-rule end)))
		   (t temp)))
       (t nil)))

;;condition is left-assoc
(defun parse-condition( end &aux(temp(parse-alternatives end)))
  (cond (temp (parse-condition1 temp end))
	(t nil)))

(defun parse-alternatives( end &aux(temp(parse-repeated end)))
  (cond (temp (parse-alternatives1 temp end))
	(t nil)))


(defun parse-alternatives1(temp end)
  (cond ((eolp end) temp)
	((guess-token '\| )
	 (rt)
	 (parse-alternatives1 `(|Alternatives| ,temp ,(parse-repeated
					      end)) end))
	(t temp)))


(defun parse-condition1(temp end)
  (cond ((eolp end) temp)
	((guess-token '|/;|)
	 (rt)
	 (parse-condition1 `(|Condition| ,temp ,(parse-repeated
					      end)) end))
	(t temp)))

(defun parse-repeated(end &aux (temp (parse-or end)))
  (cond (temp 
	 (cond((eolp end) temp)
	      ((guess-token '|..|)(rt)`(|Repeated| ,temp))
	      ((guess-token '|...|)(rt)`(|RepeatedNull| ,temp))
	      (t temp)))
	(t nil)))


(defun parse-addto(end &aux (temp (parse-replace end)))
  ;; bug noticed by /fixed by lvi@ida.liu.se
  (cond (temp
	 (cond 
	  ((eolp end) temp)
	  ((guess-token '|+=|)(rt)`(|AddTo| ,temp ,(parse-addto end)))
	  ((guess-token '|*=|)(rt)`(|TimesBy| ,temp ,(parse-addto end)))
	  ((guess-token '|-=|)(rt)`(|SubtractFrom| ,temp ,(parse-addto end)))
	  ((guess-token '|/=|)(rt)`(|DivideBy| ,temp ,(parse-addto end)))
	  (t temp)))
	(t nil)))


(defun parse-set(end &aux (temp (parse-// end)) )
  (cond (temp
	 (cond ((eolp end) temp)
	       ((guess-token '=)(rt)
		(cond ((guess-token '|.|)(rt)`(|UnSet| ,temp))
		      (t`(|Set|,temp ,(parse-set end)))))
	       ((guess-token '|:=|)(rt)`(|SetDelayed| ,temp ,(parse-set end)))
	       ((guess-token '^= ) (rt)`(|UpSet| ,temp ,(parse-set end)))
	       ((guess-token '|^:=| ) (rt)`(|UpSetDelayed| ,temp ,(parse-set end)))
	       ((guess-token '|/:| ) (rt)`(|TagSet| ,temp ,(parse-set end)))
	       ;;actually, Mathematica uses TagSet Delayed, Un. 
	       ((guess-token '|::=| ) (rt)
		(cond 
		 ((guess-token '|.|)(rt)`(|UnAlias| ,temp))
		 (t`(|Alias| ,temp ,(parse-set end)))))
	       (t temp)))
	(t nil)))

;; f&[a,b] --> ((Function f) a b)
(defun parse-ampersand(end &aux temp)
  (cond((setq temp (parse-addto end))
	(cond ((eolp end) temp)
	      ((eq (peek-token) '\&) (rt)(parse-fun1 `(|Function| ,temp) end))
	      (t temp)))
       (t nil)))

;;left associative  e1//e2  
(defun parse-//(end &aux (temp (parse-ampersand end)))
  (cond (temp
	 (cond ((eolp end) temp)
	       ((guess-token '|//|)(rt)
		(parse-//1 `(,(parse-ampersand end) ,temp) end))
	       (t temp)))
	(t nil)))

(defun parse-//1(sofar end) 
  (cond ((eolp end) sofar)
	((guess-token '|//|) 
	 (rt)
	 (parse-//1 `(,(parse-ampersand end) ,sofar) end))
	(t sofar)))
  

;;; hacked to eliminate 1/a -> (* 1 (expt a -1)) in favor of
;;; just (expt a -1).  4/26/96 RJF Fixed 5/3/96 to really work. I hope.


(defun parse-times(end &aux (temp (parse-unary end))res)  ;
  ;  t::=f1{*f2} | f1{/f2} |  f1 <space> f2   
  (cond ((eolp end) temp)
	(temp 
	 (setq res (cons temp nil))
	 (loop
	  (cond ((eolp end)(return (fixtimes1(parse-nary1 res '|Times|))))
		((guess-token '*)
		 (rt)
		 ;; a * !b+c is (Times a (Not (Plus b c)))
		 (setq res (cons (parse-unary end)res)))
		((guess-token '/)
		 (rt)
		 ;; patch 1/11/96 RJF to make 1/2 come out as 1/2
		 ;; rather than (Times 1(Power 2 -1)).
		 ;; This helped in a pattern matching application
		 ;; so I put it in here too.
		 (let ((denom (parse-unary end)))
		   (setf res
		     (if (numberp denom)
			 (if (numberp (car res))
			     ;; combine numerator and denominator, numerically
			     (cons (/ (car res) denom) (cdr res))
			   ;; just tack on number like 1/2
			   (cons (/ 1 denom) res))
		       (cons  `(|Power| ,denom -1) res))))
		 ;; previously I just did this...
		 ;;(setq res (cons `(Power ,(parse-unary end) -1) res))
		 )
		;; note that a / b c  = (a * b^-1 *c) not (a* (b*c)^-1)
		
		;; this implements the kludge a x = a*x
		;; can't tolerate  a +b ==> (Times a b), and +b is b...
		;; hence use parse-power, not parse-not
		
		((setq temp (parse-power end)) (setq res (cons temp res)))
		(t (return (fixtimes1(parse-nary1 res '|Times|)))))))
	(t nil) ; not a term
	))
;; this hack below is mostly to make (Times 1 (Power x -1)) ==> (Power x -1)
(defun fixtimes1(r)
  (cond ((and (consp r)(eq (car r) '|Times|))
	 (cond((eql (cadr r) 1)  ;; (Times 1 x y ) ==> (Times x y)
	       (setf r (ucons (car r)(cddr r)))
	       r))
	 (cond((null (cddr r))  ;; (Times 1 x) ==> x
	       (setf r (cadr r))))))
  r)

  
			 
(defun parse-unary (end &aux)  ; E::=+T | -T
  (cond ((guess-token '+)(rt)(parse-unary end)) ;unary +
	((guess-token '-)(rt)
	 (let ((h (parse-unary end)))
	   (if (numberp h) (- h)
	     `(|Times| -1 ,h))))
	((guess-token '|!| )(parse-not end))
	;;; extra added attraction!!  'foo  -> (Quote foo)
	((guess-token '|'|) (rt)`(|Quote|, (parse-unary end)))
	(t (parse-power end))))
  
(defun parse-power (end &aux (temp (parse-dot end)))  ; f ::= p^f | p
  (cond ;((eolp end) temp)
   (temp
    (cond ((eolp end) temp)
	  ((guess-token '^)
	   (rt)
	   `(|Power| ,temp ,(parse-unary end))) ;;really going up the precedence
	  (t temp)))
   (t nil)))

(defun parse-dot (end &aux (temp (parse-ncm end))res)  ; E::=e1 . e2  n-ary  dot
  (cond (temp
	 (cond ((eolp end) temp)
	       ((guess-token '|.|) ;;check first to avoid consing
		(setq res (cons temp nil))
		(loop
		 (cond ((eolp end) (return (parse-nary1 res 'Dot)))
		       ((guess-token  '|.|)
			(rt)
			(setq res (cons (parse-ncm end) res)))
		       (t (return (parse-nary1 res '|Dot|))))))
	       (t temp)))
	(t nil) ; not a dot-expression
	))

(defun parse-ncm (end &aux (temp (parse-fact end)) res) ; E::=e1 ** e2  n-ary  
  (cond (temp
	 (cond ((eolp end) temp)
	       ((guess-token '**) ;;check first to avoid consing
		(setq res (cons temp nil))
		(loop
		 (cond 
		  ((eolp end)
		   (return 
		    (parse-nary1 res
				 '|NonCommutativeMultiply|)))
		  ((guess-token  '**)
			(rt)
			(setq res (cons (parse-fact end) res)))
		  (t (return (parse-nary1 res '|NonCommutativeMultiply|))))))
	       (t temp)))
        (t nil) ; not a **-expression
))


;;factorial is left-associative  a ! !  means (a!)!

(defun parse-fact (end &aux (temp (parse-map end))) ;  d ::= m | m! | m!!
  (cond (temp (parse-fact1 temp end))
	(t nil)))

(defun parse-fact1 (temp end) ;  d ::= m | m! | m!!
  (cond((eolp end) temp)
       ((guess-token '|!|)
	(rt)
	(parse-fact1 `(|Factorial| ,temp) end))
       ((guess-token '|!!|)
	(rt)
	(parse-fact1 `(|Factorial2| ,temp) end))
       (t temp)))

(defun parse-map 
  (end &aux (temp (parse-tilde (parse-at end) end))) ;  d ::= t | t /@ expr
  (cond ((eolp end) temp)
	(temp
	 (cond ((guess-token '|/@|)
		(rt)
		`(|Map|  ,temp ,(parse-map end)))
	       ((guess-token '|//@|)
		(rt)
		`(|MapAll|  ,temp ,(parse-map end))) 
	       ((guess-token '|@@|)
		(rt)
		`(|Apply|  ,temp ,(parse-map end)))
	       (t temp)))
	(t nil)))


(defun parse-tilde(sofar end &aux op last )
  (cond ((null sofar)nil)
	((eolp end) sofar)
	(t(cond ((and 
		  (guess-token '|~|) (rt)
		  (setq op (parse-at nil))
		  (guess-token '|~|)(rt)
		  (setq last (parse-at end)))
		 (parse-tilde `(,op ,sofar ,last) end))
		(t sofar)))))

(defun parse-precrement(end);; look for ++a or --a  ;lvi fix for ++ ++ a
	 (cond ;((eolp end) nil)
	       ((guess-token '|++|)(rt) `(|PreIncrement| ,(parse-precrement end)))
	       ((guess-token '|--|)(rt) `(|PreDecrement| ,(parse-precrement end)))
	       (t (parse-fun end))))

(defun parse-pattest(end &aux (temp (parse-var end))) ; patterntest  is e1?e2
  (cond (temp
	 (cond ((eolp end) temp)
	       ((guess-token '\?)
		(rt)
		`(|PatternTest| ,temp ,(parse-var end)))
	       (t temp)))
	(t nil)))

(defvar rpar '\) )
(defvar lpar '\( )

;;parse-optional looks for Optional   a_:v is (Optional(Pattern a (Blank)) v)
;; also,  a_. is (Optional (Pattern a (Blank))) ;; 2/2011 RJF; fixed in parse-var

(defun parse-optional (end &aux (temp (parse-pattest end)))

  (cond (temp 
	 (cond
	       ((eolp end) temp)
	       ((guess-token '\:)
		(rt)
		(list '|Optional| temp (parse-comp end)))
	       (t temp)))
      (t temp)))



  ; var ::=  var_ etc| #var | _ | __ | ___ | patternstuff  | var :: string
  ;( stuff ) |  ( a , ....) | { a , ...} | number 

(defun parse-var (end &aux (next (peek-token)))  
  (cond ((eql next 'e-o-l)
	 (rt)
	 (setq next (peek-token))
	 (cond ((eql next 'e-o-l) nil)
	       (t (parse-var end))))
	((var-p next)
	 (rt)
	 (cond ((eolp end) next)
	       ((blankp (peek-token))
		(list '|Pattern| next (rt)))
	     
	       ((guess-token '|::|) (rt) (list '|MessageName| next (rt)))
	       ((guess-token '|:|)(rt)(list '|Pattern| next (parse-repeated end)))
	      	      
	       ((equal (peek-token)'(|Optional| (|Blank|)))
		(rt)  ;;example x_.   3/30/92 ; fixed 2/5/2011
		`(|Optional|(|Pattern| ,next (|Blank|))))
	       
	       (t next)))
	((equal next lpar) ;; look for (expr)
	 ;; actually  (a,b,..), a Sequence is not accepted in 2.0, but in 1.2
	 (rt)
	 (setq next (parse-comp nil))
	 (cond ((guess-token rpar)
		(rt)
		next)
	       ((parselist2 (list next '|Sequence|) rpar))
	       (t (mysignal 'syntax-error "too few rpars"))))
	((equal next '{) (rt) ;; look for List
	 (cond ((guess-token '})
		(rt)
		(list '|List|))
	       ((setq next (parse-comp nil)) ;lvi 8/29
		(parselist2 (list next '|List|) '}))
	       (t
		(mysignal 'syntax-error "too few right-}"))))
	((equal next '|#|)  ;;; |# +kfp comment fix
	 (parse-slotform '|Slot| end))
	((equal next '|##|) ;;; |# +kfp comment fix
	 (parse-slotform '|SlotSequence| end))
	 
	((setq next (parse-number end))
	 ;;(if (atom next) (list 'Integer next) next);;tags integers specifically
	 next;; just leaves integers as self-declared, exact.
	 )
	(t nil)))

;; # means (Slot 1) ## means (SlotSequence 1)
;; #2 means (Slot 2) etc.

(defun parse-slotform(head end &aux var)
  (rt) ;; sop up # or ##
  (cond((null (setq var(parse-int end)))`(,head 1))
       (t `(,head ,var))))


(defun parse-at (end &aux (var (parse-precrement end))) 
  ;; collect e1 @ e2 | e++ | e--
  (cond (var 
	 (cond ((eolp end) var) 
	       ((guess-token `|@|) (rt) `(,var ,(parse-at end)))
	       ((guess-token '|++|) (rt) `(|Increment| ,var)) 
	       ((guess-token '|--|) (rt) `(|Decrement| ,var))
	       (t var)))
	(t nil)))

;; parse-fun collects f[x] or similar; also a++
;; it is left-assoc.  f[x]=(f x);  f[x][y] = ((f x) y)

(defun parse-fun(end &aux (temp(parse-optional end)))
  (cond (temp (parse-fun1 temp end))
	(t nil)))

;; parser must handle the following cases:
;; f'    --> ((Derivative 1) f)
;; f'x   --> (Times ((Derivative 1) f) x)
;; f'[x] --> (((Derivative 1) f) x)
;; f''  --> ((Derivative 2) f)



(defun parse-fun1(sofar end)
  (cond((eolp end) sofar)
       ;; handle the derivative cases
       ((eq (peek-token) '|'|)
	(do ((i 0 (1+ i)))
	    ((or (eolp end)(not (guess-token '|'| )))
	     (parse-fun1  `((|Derivative| ,i) ,sofar) end))
	    (rt)))
       ;; handle the function invocation f[x] and part .. f[[1]]
       ((eql(peek-token)'[)
	(parse-fun1(parse-list sofar) end));; f[], f[x] or maybe (f[x])[y]  etc.

       (t sofar)))


;;     some extensions/ modifications
;; 1. we parse a==b>c as (Comparison a |Equal| b Greater c)  
;; 2. integers are parsed as (for example) 4, not (Integer 4) ;;optional
;;  (we could do this so we can eventually tag integers with other info
;;  like precision, accuracy, base)
;; 3. integer args to % and # are just lisp integers.
;; 4. real numbers like 1.20 are simply (|Real| 1 20) for the
;;   same reason as for integers.
;;   (Mathematica has such info stashed away in secret)
;; 5. within " " we allow any number of newlines even interactively. M allows 2
;; 6. we count lines consisting only of (*comments*) as newlines
;; 7 optional.. (commented out) 123`456`789 syntax for long bignumber input

;;known bugs or features(?)  1/90

;; we support radix only between 2 and 10; blame it on laziness
;; we do not support non-decimal radix flt. pt; blame it on ditto.
;; we do TagSet slightly differently; ditto


;; fixed bugs/new features  1/91 -- RJF

;; typing nil  provides the symbol False, not nil. I don't know if
;; this is a bug or a feature, though. It means that the parser will
;; not think it has failed to parse a subexpression when it merely 
;; has parsed the symbol nil, so it is convenient, anyway.
;; Mma has the symbol Null, perhaps for similar reasons.

;; fixed 1/28/91
;; fixed the parsing a_:v of which is now
;; (Optional (Pattern a (Blank)) v).
;; fixed the parsing of #1+#2&[a,b] to ((Function (Plus (Slot 1)(Slot 2))) a b)
;; fixed 2/15/91 parsing of a**b followed by eol

;; added 2/3/91

;;   'a is same as Quote[a].   f' is derivative, though.  'f'a is
;;  (Times (Quote ((Derivative 1)f)) a).  This is not in conflict with mma.

;; added 2/15/91
;; the symbol * can be used, in some circumstances, as a variable name.
;; In those circumstances where it cannot be confused with an operator,
;; it can be used as a symbol.  In some cases it can be used as a symbol
;; even if YOU confuse it.  Advantages: you can use it as a regular-expression
;; tag like  foo[*,3] to denote the 3rd column of a matrix.

;; You can use * * *  to mean  (Times * *)  although  *^2 (Power * 2) also
;; works.  The expressions x * * y and x * * * y mean (Times x * y).
;; The expression ( * * ) means (Times * *)  

;; BUT NOTE  THAT  (* ANYTHING  *)  is A COMMENT !!!!  :) 

;; fixed 5/29/91 from lvi@ida.liu.se 
;; fixed parsing of a+=b;c from a=+(b;c) to (a=+b);c.
;; fixed ++ ++ a also.
;; 8/29/91 bug fix from lars viklund (lvi@ida,liu.se)
;; in parse var, replace parse-set by parse-comp (twice)
;; 11/23/91 bug fix to repair parsing of 1.004 (was same as 1.4) using 
;;   parse-frac. This was pointed out by gotoda@is.s.u-tokyo.ac.jp
;; 2/7/2011 RJF fixed parsing of Optional in  n_.

;; this next item allows one to do, in lisp, (setq r #mx^2-1
;;                                              )
(set-dispatch-macro-character #\# #\m
      #'(lambda (stream sub-char infix-argument)
         (declare (ignore sub-char infix-argument))
         (list 'quote(mma::p stream) )))



(defparameter trans-to-2case (make-hash-table))

(defun setupcases()
  (map nil #'(lambda (capcase bothcase)
	       (setf (gethash capcase trans-to-2case)
		 bothcase))
       caps-built-in-syms
       built-in-syms))

(defun recase(x)(gethash x trans-to-2case x))

;; really there are 2 situations with GCL 
;; 1. Identifier is typed in in whatever cases are available on the keyboard, 
;; e.g. Cos, COS, cos and appears to be "COS" because
;; the lisp system is not so clever.  

;; 2. Similarly, Foo, fOO, FOO, foo are all "FOO". 
;; What to do?

;;  a. If the parser sees COS, it looks up on trans-to-2case, and returns |Cos|.
;;  b. There is no entry for FOO so the parser may return just FOO. This is
;;     ok until  one of two situations: 
;;     1. It comes time to display FOO, at which point we may want Foo, or foo, or...
;;     2. The user types in two different names that are converted to FOO. That is,
;;        Foo and foo are DIFFERENT. Tough luck.


;; With an ANSI CL that has a readtable-case setting there are 2 situations. 
;; 1. Identifier is typed in in whatever cases are available on the keyboard, 
;; e.g. Cos, COS, cos and appear in the correct case. Great.

;; 2. Similarly, Foo, foo, etc are distinct.  The name print is read in as print.
;;  Unfortunately, the lisp program that prints expressions is called PRINT.
;;   What to do?

;;  a. If the parser sees print, all in lower case, it converts it to PRINT.
;;  b. If the display is about to display PRINT, all in upper case, it converts it to print.
;;    This works.


;;;+kfp
(in-package :mma)
;;;
;;; Get argument: for *.m file
;;;
(defun get-argv1 ()
  (cdr sb-ext:*posix-argv*))


;;;
;;; mma2cl main function
;;;
(defun mma2cl-main ()
  (if (not (get-argv1))
      (progn (format t "~a~%" "MMA to Common-Lisp parser based on")
        (format t "~a~%" "Mock MMA, (c) by Richard J. Fateman") 
        (format t "~a~%" "Usage: mma2cl <mma-file>") 
        (cl-user::quit)))
  (let ((mma-file (car (last (get-argv1)))))
     (if (probe-file mma-file)
       (ps (open mma-file))
       (format t "Error: bad file ~a, or not found. ~%" mma-file))))

;;;
;;; create a standalone image
;;; e.g. (mma::make-image "mma2cl")
;;;
(defun make-image (img-name)
    (sb-ext:save-lisp-and-die img-name
      ;:compression t  ;; runtime needs zlib support
       :toplevel #'mma::mma2cl-main
      ;:save-runtime-options t
       :executable t))

;;;EOF