
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : fricas-menus.scm
;; DESCRIPTION : Fricas menus
;; COPYRIGHT   : (C) 1999, 2012  Joris van der Hoeven and Andrey Grozin
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (fricas-menus)
  (:use (texmacs texmacs tm-files)
	;(doc help-funcs)
	(dynamic scripts-edit)))

(define fricas-apply script-apply)

(menu-bind fricas-menu
  (if (not-in-session?)
      (link scripts-eval-menu)
      ---)
  (-> "Calculus"
      ("Differentiate" (fricas-apply "D" 2))
      ("Integrate"     (fricas-apply "integrate" 2))
      ("Limit"         (fricas-apply "limit" 3))
      ---
      ("Sum"     (fricas-apply "sum" 2))
      ("Product" (fricas-apply "product" 2))
      ---
      ("Solve an equation" (fricas-apply "solve" 2)))
  (if (not-in-session?)
      ---
      (link scripts-eval-toggle-menu)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(menu-extend texmacs-extra-menu
    (=> "Fricas"
    
      ("Last result(s)... % or %%(+-n)" (insert "%"  ))
      ("Assignment ... x:=1"            (insert ":=" ))
      ("Type declaration ... y:Integer" (insert ":"  ))
      ("Type conversion ... y::Float"   (insert "::" ))
      ("Line continuation ... abc_"     (insert "_"  ))
      ("Comments ... -- double dash"    (insert "--" ))
      ("Function ... f(x) == x^2"       (insert "==" ))
      ("Macro    ... macro df == D"     (insert "macro ? ==" ))
       
      ---   
      (-> "Predefined macros"
         ("The square root of -1 ... %i"      (insert "%i"  ))
         ("Base of natural log ... %e"        (insert "%e"  ))
         ("Number pi ... %pi"                 (insert "%pi" ))
         ("Infinity       ... %infinity"      (insert "%infinity" ))
         ("Plus infinity  ... %plusInfinity"  (insert "%plusInfinity" ))
         ("Minus infinity ... %minusInfinity" (insert "%minusInfinity" )))
         
       
      (-> "Numbers"
         ("Factor an integer n"         (fricas-apply "factor"     1))
         ("Number n in base x"          (fricas-apply "radix"      2))
         ("Integer n to Roman"          (fricas-apply "roman"      1))
         ("Represent x as T ... x@T"    (insert       "?@?"         ))
         ("Precision #digits"           (fricas-apply "digits"     1))
         ("Complex number ... 1+%i*2"   (insert       "?+%i*?"      ))
         ("Complex symbolic ... a+%i*b" (fricas-apply "complex"    2))
         ("Complex conjugate"           (fricas-apply "conjugate"  1))
         ("Rational -> Decimal"         (fricas-apply "decimal"    1))
         ("Continued fraction"          (fricas-apply "continuedFraction" 1))
         ("Partial fraction"            (fricas-apply "partialFraction"   2))
         ("Padic fraction"              (fricas-apply "padicFraction"     1))
         ("Root of polynomial"          (fricas-apply "rootOf"            2)))
         
         
         
         
         
          
         
      (-> "Calculus"
    
         ("Differentiate [D]"       (fricas-apply "D" 2))
         ("Integrate"               (fricas-apply "integrate" 2))
         ("Limit"                   (fricas-apply "limit" 3))
         ---
         ("Sum"                     (fricas-apply "sum" 2))
         ("Product"                 (fricas-apply "product" 2))
         ---
         ("Solve an equation"       (fricas-apply "solve" 2)))
                        
      (-> "Algebra"
          ("Matrices" ()  )
          ("Tensors" ()  ))
     )
)
