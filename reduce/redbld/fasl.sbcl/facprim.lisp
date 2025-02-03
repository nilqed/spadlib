(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FACPRIM)) 
(FLUID
 '(*FORCE-ZERO-SET *OVERSHOOT *OVERVIEW *TRFAC ALPHALIST ALPHAVEC BAD-CASE
   BEST-FACTOR-COUNT BEST-KNOWN-FACTORS BEST-MODULUS BEST-SET-POINTER
   CHOSEN-PRIME CURRENT-FACTOR-PRODUCT DELTAM F-NUMVEC FACTOR-LEVEL
   FACTOR-TRACE-LIST FACTORED-LC FACTORVEC FACVEC FHATVEC FORBIDDEN-PRIMES
   SMALLEST-PRIME FORBIDDEN-SETS FULL-GCD HENSEL-GROWTH-SIZE IMAGE-CONTENT
   IMAGE-FACTORS IMAGE-LC IMAGE-MOD-P IMAGE-POLY IMAGE-SET IMAGE-SET-MODULUS
   INPUT-LEADING-COEFFICIENT INPUT-POLYNOMIAL INVERTED INVERTED-SIGN
   IRREDUCIBLE KNOWN-FACTORS KORD* M-IMAGE-VARIABLE MODFVEC MODULAR-INFO
   MULTIVARIATE-FACTORS MULTIVARIATE-INPUT-POLY NO-OF-BEST-SETS
   NO-OF-PRIMES-TO-TRY NO-OF-RANDOM-SETS NON-MONIC NULL-SPACE-BASIS
   NUMBER-OF-FACTORS ONE-COMPLETE-DEG-ANALYSIS-DONE OTHERVARS POLY-MOD-P
   POLYNOMIAL-TO-FACTOR PREVIOUS-DEGREE-MAP PRIME-BASE RECONSTRUCTING-GCD
   REDUCTION-COUNT SAVE-ZSET SPLIT-LIST TARGET-FACTOR-COUNT
   TRUE-LEADING-COEFFTS USABLE-SET-FOUND VALID-IMAGE-SETS VARS-TO-KILL
   ZERO-SET-TRIED ZEROVARSET ZSET)) 
(GLOBAL '(LARGEST-SMALL-MODULUS)) 
(PUT 'FACTORIZE-PRIMITIVE-POLYNOMIAL 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE-PRIMITIVE-POLYNOMIAL 'DEFINED-ON-LINE '164) 
(PUT 'FACTORIZE-PRIMITIVE-POLYNOMIAL 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'FACTORIZE-PRIMITIVE-POLYNOMIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE-PRIMITIVE-POLYNOMIAL (U)
    (COND ((NOT (EQ M-IMAGE-VARIABLE (CAAAR U))) (ERRACH "factorize variable"))
          ((EQUAL (DEGREE-IN-VARIABLE U M-IMAGE-VARIABLE) 1) (LIST U))
          ((EQUAL (DEGREE-IN-VARIABLE U M-IMAGE-VARIABLE) 2)
           (FACTORIZE-QUADRATIC U))
          ((FAC-UNIVARIATEP U) (UNIVARIATE-FACTORIZE U))
          (T
           (PROG (VALID-IMAGE-SETS FACTORED-LC IMAGE-FACTORS PRIME-BASE
                  ONE-COMPLETE-DEG-ANALYSIS-DONE ZSET ZEROVARSET OTHERVARS
                  MULTIVARIATE-INPUT-POLY BEST-SET-POINTER REDUCTION-COUNT
                  TRUE-LEADING-COEFFTS NUMBER-OF-FACTORS INVERTED-SIGN
                  IRREDUCIBLE INVERTED VARS-TO-KILL FORBIDDEN-SETS
                  ZERO-SET-TRIED NON-MONIC NO-OF-BEST-SETS NO-OF-RANDOM-SETS
                  BAD-CASE TARGET-FACTOR-COUNT MODULAR-INFO
                  MULTIVARIATE-FACTORS HENSEL-GROWTH-SIZE ALPHALIST
                  PREVIOUS-DEGREE-MAP IMAGE-SET-MODULUS BEST-KNOWN-FACTORS
                  RECONSTRUCTING-GCD FULL-GCD)
             (PROG (STREAM)
               (COND
                ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                 (SETQ STREAM (CONS NIL NIL)))
                (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
               (COND
                (STREAM
                 (PROGN
                  (SETQ STREAM (WRS (CDR STREAM)))
                  (PROGN
                   (PROGN
                    (PRIN2*
                     "From now on we shall refer to this polynomial as U.")
                    (TERPRI* NIL))
                   (PROGN
                    (PRIN2*
                     "We now create an image of U by picking suitable values ")
                    (TERPRI* NIL))
                   (PROGN
                    (PRIN2* "for all but one of the variables in U.")
                    (TERPRI* NIL))
                   (PRIN2* "The variable preserved in the image is ")
                   (PRIN2* M-IMAGE-VARIABLE)
                   (TERPRI* NIL))
                  (WRS STREAM)))))
             (INITIALIZE-FLUIDS U)
            TRYAGAIN
             (GET-SOME-RANDOM-SETS)
             (CHOOSE-THE-BEST-SET)
             (COND (IRREDUCIBLE (RETURN (LIST U)))
                   (BAD-CASE
                    (PROGN
                     (COND (*OVERSHOOT (PRIN2T "Bad image sets - loop")))
                     (SETQ BAD-CASE NIL)
                     (GO TRYAGAIN))))
             (RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS)
             (COND (IRREDUCIBLE (RETURN (LIST U)))
                   (BAD-CASE
                    (PROGN
                     (COND (*OVERSHOOT (PRIN2T "Bad image factors - loop")))
                     (SETQ BAD-CASE NIL)
                     (GO TRYAGAIN))))
             (DETERMINE.LEADING.COEFFTS)
             (COND (IRREDUCIBLE (RETURN (LIST U)))
                   (BAD-CASE
                    (PROGN
                     (COND
                      (*OVERSHOOT
                       (PRIN2T "Bad split shown by LC distribution")))
                     (SETQ BAD-CASE NIL)
                     (GO TRYAGAIN))))
             (COND
              ((EQUAL (DETERMINE-MORE-COEFFTS) 'DONE)
               (PROGN (RETURN (CHECK-INVERTED MULTIVARIATE-FACTORS)))))
             (RECONSTRUCT-MULTIVARIATE-FACTORS NIL)
             (COND
              ((AND BAD-CASE (NOT IRREDUCIBLE))
               (PROGN
                (COND (*OVERSHOOT (PRIN2T "Multivariate overshoot - restart")))
                (SETQ BAD-CASE NIL)
                (GO TRYAGAIN))))
             (COND (IRREDUCIBLE (RETURN (LIST U))))
             (RETURN (CHECK-INVERTED MULTIVARIATE-FACTORS)))))) 
(PUT 'CHECK-INVERTED 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-INVERTED 'DEFINED-ON-LINE '259) 
(PUT 'CHECK-INVERTED 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'CHECK-INVERTED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-INVERTED (MULTI-FACLIST)
    (PROG (INV.SIGN L)
      (COND
       (INVERTED
        (PROGN
         (SETQ INV.SIGN 1)
         (SETQ MULTI-FACLIST
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X MULTI-FACLIST)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (PROGN
                                        (SETQ L
                                                (INVERT.POLY X
                                                             M-IMAGE-VARIABLE))
                                        (SETQ INV.SIGN
                                                (TIMES (CAR L) INV.SIGN))
                                        (CDR L)))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (PROGN
                                (SETQ L (INVERT.POLY X M-IMAGE-VARIABLE))
                                (SETQ INV.SIGN (TIMES (CAR L) INV.SIGN))
                                (CDR L)))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((NOT (EQUAL INV.SIGN INVERTED-SIGN))
           (ERRORF (LIST "INVERSION HAS LOST A SIGN" INV.SIGN)))))))
      (RETURN (SETQ MULTIVARIATE-FACTORS MULTI-FACLIST)))) 
(PUT 'GETCOF 'NUMBER-OF-ARGS 3) 
(PUT 'GETCOF 'DEFINED-ON-LINE '272) 
(PUT 'GETCOF 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'GETCOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETCOF (P V N)
    (COND ((OR (ATOM P) (ATOM (CAR P))) (COND ((EQUAL N 0) P) (T NIL)))
          ((EQUAL (CAAAR P) V)
           (COND ((EQUAL (CDAAR P) N) (CDAR P)) (T (GETCOF (CDR P) V N))))
          (T
           (ADDF
            ((LAMBDA (G573 G574)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF G573 G574))
                     (T (POLY-MULTF G573 G574))))
             (CONS (CONS (CAAR P) 1) NIL) (GETCOF (CDAR P) V N))
            (GETCOF (CDR P) V N))))) 
(PUT 'FACTORIZE-QUADRATIC 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE-QUADRATIC 'DEFINED-ON-LINE '282) 
(PUT 'FACTORIZE-QUADRATIC 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'FACTORIZE-QUADRATIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE-QUADRATIC (U)
    (PROG (A B C DISCR F1 F2 X)
      (SETQ A (GETCOF U M-IMAGE-VARIABLE 2))
      (SETQ B (GETCOF U M-IMAGE-VARIABLE 1))
      (SETQ C (GETCOF U M-IMAGE-VARIABLE 0))
      (COND
       ((AND (EQUAL DMODE* '|:MOD:|) (EQUAL CURRENT-MODULUS 2))
        (COND ((AND (EQUAL B 1) (EQUAL C 1)) (RETURN (LIST U))))))
      (SETQ DISCR
              (ADDF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF B B)) (T (POLY-MULTF B B)))
               ((LAMBDA (G578)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G578))
                        (T (POLY-MULTF A G578))))
                ((LAMBDA (G575)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G575 C))
                         (T (POLY-MULTF G575 C))))
                 (MINUS 4)))))
      (SETQ DISCR (SQRTF2 DISCR))
      (COND ((EQUAL DISCR (MINUS 1)) (RETURN (LIST U))))
      (SETQ X
              (ADDF
               ((LAMBDA (G582)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G582))
                        (T (POLY-MULTF A G582))))
                ((LAMBDA (G580)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 G580))
                         (T (POLY-MULTF 2 G580))))
                 (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1))))
               B))
      (SETQ F1 (ADDF X DISCR))
      (SETQ F2 (ADDF X (NEGF DISCR)))
      (SETQ F1
              ((LAMBDA (*EXP)
                 (QUOTF1 F1
                         (CDR (CONTENTS-WITH-RESPECT-TO F1 M-IMAGE-VARIABLE))))
               T))
      (SETQ F2
              ((LAMBDA (*EXP)
                 (QUOTF1 F2
                         (CDR (CONTENTS-WITH-RESPECT-TO F2 M-IMAGE-VARIABLE))))
               T))
      (RETURN (LIST F1 F2)))) 
(PUT 'SQRTD2 'NUMBER-OF-ARGS 1) 
(PUT 'SQRTD2 'DEFINED-ON-LINE '308) 
(PUT 'SQRTD2 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'SQRTD2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRTD2 (D)
    (COND ((EQUAL D NIL) NIL) ((OR (NOT (FIXP D)) (LESSP D 0)) (MINUS 1))
          (T
           (PROG (Q R ROLD)
             (SETQ Q (PMAM-SQRT D))
             (SETQ R (DIFFERENCE (TIMES Q Q) D))
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ ROLD (ABS R))
                (SETQ Q (DIFFERENCE Q (QUOTIENT (PLUS R Q) (TIMES 2 Q))))
                (SETQ R (DIFFERENCE (TIMES Q Q) D)))
               (COND ((NOT (GEQ (ABS R) ROLD)) (GO REPEATLABEL))))
             (COND ((EQUAL R 0) (RETURN Q)) (T (RETURN (MINUS 1)))))))) 
(PUT 'PMAM-SQRT 'NUMBER-OF-ARGS 1) 
(PUT 'PMAM-SQRT 'DEFINED-ON-LINE '330) 
(PUT 'PMAM-SQRT 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'PMAM-SQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PMAM-SQRT (N)
    (PROG (S TEN**6 TEN**12 TEN**14)
      (SETQ S 0)
      (SETQ TEN**6 (EXPT 10 6))
      (SETQ TEN**12 (EXPT TEN**6 2))
      (SETQ TEN**14 (TIMES 100 TEN**12))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N TEN**14)) (RETURN NIL)))
        (PROGN (SETQ S (IADD1 S)) (SETQ N (PLUS 1 (QUOTIENT N TEN**12))))
        (GO WHILELABEL))
      (RETURN (TIMES (PLUS (FIX (SQRT (FLOAT N))) 1) (EXPT 10 (TIMES 6 S)))))) 
(PUT 'SQRTF2 'NUMBER-OF-ARGS 1) 
(PUT 'SQRTF2 'DEFINED-ON-LINE '343) 
(PUT 'SQRTF2 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'SQRTF2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRTF2 (P)
    (COND ((OR (ATOM P) (ATOM (CAR P))) (SQRTD2 P))
          (T
           (PROG (V D QLC Q R W)
             (COND
              ((OR (NOT (EVENP (SETQ D (CDAAR P))))
                   (EQUAL (SETQ QLC (SQRTF2 (CDAR P))) (MINUS 1)))
               (RETURN (MINUS 1))))
             (SETQ D (QUOTIENT D 2))
             (SETQ V (CAAAR P))
             (SETQ Q (CONS (CONS (GETPOWER (FKERN V) D) QLC) NIL))
             (SETQ R
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 Q))
                           (T (POLY-MULTF 2 Q))))
             (SETQ P (CDR P))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (NOT (OR (ATOM P) (ATOM (CAR P)))) (EQUAL (CAAAR P) V)
                       (GEQ (CDAAR P) D)
                       (NEQ
                        (SETQ W
                                ((LAMBDA (*EXP) (QUOTF1 (CONS (CAR P) NIL) R))
                                 T))
                        NIL)))
                 (RETURN NIL)))
               (PROGN
                (SETQ P
                        (ADDF P
                              ((LAMBDA (G583 G584)
                                 (COND
                                  (*PHYSOP-LOADED (PHYSOP-MULTF G583 G584))
                                  (T (POLY-MULTF G583 G584))))
                               (NEGF W)
                               (ADDF
                                (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 Q))
                                      (T (POLY-MULTF 2 Q)))
                                W))))
                (SETQ Q (ADDF Q W)))
               (GO WHILELABEL))
             (COND ((NULL P) (RETURN Q)) (T (RETURN (MINUS 1)))))))) 
(PUT 'INITIALIZE-FLUIDS 'NUMBER-OF-ARGS 1) 
(PUT 'INITIALIZE-FLUIDS 'DEFINED-ON-LINE '365) 
(PUT 'INITIALIZE-FLUIDS 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'INITIALIZE-FLUIDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INITIALIZE-FLUIDS (U)
    (PROG (W W1)
      (COND
       (*FORCE-ZERO-SET
        (PROGN (SETQ NO-OF-RANDOM-SETS 1) (SETQ NO-OF-BEST-SETS 1)))
       (T (PROGN (SETQ NO-OF-RANDOM-SETS 9) (SETQ NO-OF-BEST-SETS 5) NIL)))
      (SETQ IMAGE-SET-MODULUS 5)
      (SETQ VARS-TO-KILL (VARIABLES-TO-KILL (CDAR U)))
      (SETQ MULTIVARIATE-INPUT-POLY U)
      (SETQ NO-OF-PRIMES-TO-TRY 5)
      (SETQ TARGET-FACTOR-COUNT (DEGREE-IN-VARIABLE U M-IMAGE-VARIABLE))
      (COND
       ((NOT
         ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
          (CDAR MULTIVARIATE-INPUT-POLY)))
        (COND
         (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
           (SETQ W (TRAILING.COEFFT MULTIVARIATE-INPUT-POLY M-IMAGE-VARIABLE)))
          (PROGN
           (SETQ INVERTED T)
           (SETQ W1 (INVERT.POLY MULTIVARIATE-INPUT-POLY M-IMAGE-VARIABLE))
           (SETQ MULTIVARIATE-INPUT-POLY (CDR W1))
           (SETQ INVERTED-SIGN (CAR W1))
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PRIN2* "The trailing coefficient of U wrt ")
                 (PRIN2* M-IMAGE-VARIABLE)
                 (PRIN2* "(=")
                 (PRIN2* W)
                 (PROGN
                  (PRIN2* ") is purely numeric so we 'invert' U to give: ")
                  (TERPRI* NIL))
                 (PRIN2* "  U <- ")
                 (PRINTSF MULTIVARIATE-INPUT-POLY)
                 (PROGN
                  (PRIN2* "This simplifies any problems with the leading ")
                  (TERPRI* NIL))
                 (PROGN (PRIN2* "coefficient of U.") (TERPRI* NIL)))
                (WRS STREAM)))))))
         (T
          (PROGN
           (SETQ FACTORED-LC
                   (FACTORIZE-FORM-RECURSION (CDAR MULTIVARIATE-INPUT-POLY)))
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PROGN
                  (PRIN2*
                   "The leading coefficient of U is non-trivial so we must ")
                  (TERPRI* NIL))
                 (PROGN
                  (PRIN2*
                   "factor it before we can decide how it is distributed")
                  (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "over the leading coefficients of the factors of U.")
                  (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "So the factors of this leading coefficient are:")
                  (TERPRI* NIL))
                 (FAC-PRINTFACTORS FACTORED-LC))
                (WRS STREAM))))))))))
      (MAKE-ZEROVARSET VARS-TO-KILL)
      (COND ((NULL ZEROVARSET) (SETQ ZERO-SET-TRIED T))
            (T
             (PROGN
              (SETQ ZSET (MAKE-ZEROSET-LIST (LENGTH ZEROVARSET)))
              (SETQ SAVE-ZSET ZSET)))))) 
(PUT 'VARIABLES-TO-KILL 'NUMBER-OF-ARGS 1) 
(PUT 'VARIABLES-TO-KILL 'DEFINED-ON-LINE '429) 
(PUT 'VARIABLES-TO-KILL 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'VARIABLES-TO-KILL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARIABLES-TO-KILL (LC-U)
    (PROG (W FORALL-RESULT FORALL-ENDPTR)
      (SETQ W (CDR KORD*))
      (COND ((NULL W) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (W)
                          (COND
                           ((OR (OR (ATOM LC-U) (ATOM (CAR LC-U)))
                                (NULL
                                 ((LAMBDA (*EXP)
                                    (QUOTF1 LC-U (LIST (CONS (CONS W 1) 1))))
                                  T)))
                            (CONS W NIL))
                           (T (CONS W T))))
                        (CAR W))
                       NIL)))
     LOOPLABEL
      (SETQ W (CDR W))
      (COND ((NULL W) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (W)
                  (COND
                   ((OR (OR (ATOM LC-U) (ATOM (CAR LC-U)))
                        (NULL
                         ((LAMBDA (*EXP)
                            (QUOTF1 LC-U (LIST (CONS (CONS W 1) 1))))
                          T)))
                    (CONS W NIL))
                   (T (CONS W T))))
                (CAR W))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(FLUID '(USABLE-SET-FOUND)) 
(PUT 'GET-SOME-RANDOM-SETS 'NUMBER-OF-ARGS 0) 
(PUT 'GET-SOME-RANDOM-SETS 'DEFINED-ON-LINE '444) 
(PUT 'GET-SOME-RANDOM-SETS 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'GET-SOME-RANDOM-SETS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GET-SOME-RANDOM-SETS NIL
    (PROG (IMAGE-SET CHOSEN-PRIME IMAGE-LC IMAGE-MOD-P IMAGE-CONTENT IMAGE-POLY
           F-NUMVEC FORBIDDEN-PRIMES SMALLEST-PRIME I J USABLE-SET-FOUND)
      (SETQ VALID-IMAGE-SETS (MKVECT NO-OF-RANDOM-SETS))
      (SETQ SMALLEST-PRIME
              (PLUS 1 (MAX-COEFFICIENT-DEGREE MULTIVARIATE-INPUT-POLY)))
      (SETQ I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP I NO-OF-RANDOM-SETS)) (RETURN NIL)))
        (PROGN
         (GENERATE-AN-IMAGE-SET-WITH-PRIME
          (COND ((LESSP I (IDIFFERENCE NO-OF-RANDOM-SETS 3)) NIL) (T T)))
         (SETQ I (IADD1 I))
         (PUTV VALID-IMAGE-SETS I
               (LIST IMAGE-SET CHOSEN-PRIME IMAGE-LC IMAGE-MOD-P IMAGE-CONTENT
                     IMAGE-POLY F-NUMVEC))
         (SETQ FORBIDDEN-SETS (CONS IMAGE-SET FORBIDDEN-SETS))
         (SETQ FORBIDDEN-PRIMES (LIST CHOSEN-PRIME))
         (SETQ J 1)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND (LESSP J 3) (LESSP I NO-OF-RANDOM-SETS))) (RETURN NIL)))
           (PROGN
            (SETQ IMAGE-MOD-P
                    (FIND-A-VALID-PRIME IMAGE-LC IMAGE-POLY
                     (NOT (NUMBERP IMAGE-CONTENT))))
            (COND
             ((NOT (EQUAL IMAGE-MOD-P 'NOT-SQUARE-FREE))
              (PROGN
               (SETQ I (IADD1 I))
               (PUTV VALID-IMAGE-SETS I
                     (LIST IMAGE-SET CHOSEN-PRIME IMAGE-LC IMAGE-MOD-P
                           IMAGE-CONTENT IMAGE-POLY F-NUMVEC))
               (SETQ FORBIDDEN-PRIMES (CONS CHOSEN-PRIME FORBIDDEN-PRIMES)))))
            (SETQ J (IADD1 J)))
           (GO WHILELABEL)))
        (GO WHILELABEL)))) 
(PUT 'CHOOSE-THE-BEST-SET 'NUMBER-OF-ARGS 0) 
(PUT 'CHOOSE-THE-BEST-SET 'DEFINED-ON-LINE '486) 
(PUT 'CHOOSE-THE-BEST-SET 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'CHOOSE-THE-BEST-SET 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHOOSE-THE-BEST-SET NIL
    (PROG (SPLIT-LIST POLY-MOD-P NULL-SPACE-BASIS KNOWN-FACTORS W N FNUM
           REMAINING-SPLIT-LIST)
      (SETQ MODULAR-INFO (MKVECT NO-OF-RANDOM-SETS))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NO-OF-RANDOM-SETS I)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV VALID-IMAGE-SETS I))
         (GET-FACTOR-COUNT-MOD-P I (CADDR (CDR W)) (CADR W)
          (NOT (NUMBERP (CADDR (CDDR W))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ SPLIT-LIST (SORT SPLIT-LIST (FUNCTION LESSPPAIR)))
      (COND
       ((EQUAL (CAAR SPLIT-LIST) 1) (PROGN (SETQ IRREDUCIBLE T) (RETURN NIL))))
      (SETQ W NIL)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NO-OF-BEST-SETS I)) (RETURN NIL)))
        (PROGN
         (SETQ N (CDAR SPLIT-LIST))
         (GET-FACTORS-MOD-P N (CADR (GETV VALID-IMAGE-SETS N)))
         (SETQ W (CONS (CAR SPLIT-LIST) W))
         (SETQ SPLIT-LIST (CDR SPLIT-LIST)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ REMAINING-SPLIT-LIST SPLIT-LIST)
      (SETQ SPLIT-LIST (REVERSIP W))
      (CHECK-DEGREE-SETS NO-OF-BEST-SETS T)
      (COND
       (BAD-CASE
        (PROGN
         (SETQ BAD-CASE NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT REMAINING-SPLIT-LIST) (RETURN NIL)))
           (PROGN
            (SETQ N (CDAR REMAINING-SPLIT-LIST))
            (GET-FACTORS-MOD-P N (CADR (GETV VALID-IMAGE-SETS N)))
            (SETQ W (CONS (CAR REMAINING-SPLIT-LIST) W))
            (SETQ REMAINING-SPLIT-LIST (CDR REMAINING-SPLIT-LIST)))
           (GO WHILELABEL))
         (SETQ SPLIT-LIST (REVERSIP W))
         (CHECK-DEGREE-SETS (DIFFERENCE NO-OF-RANDOM-SETS NO-OF-BEST-SETS) T)
         NIL)))
      (SETQ ONE-COMPLETE-DEG-ANALYSIS-DONE T)
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROGN
            (SETQ W (GETV VALID-IMAGE-SETS BEST-SET-POINTER))
            (PRIN2* "The chosen image set is:  ")
            (PROG (X)
              (SETQ X (CAR W))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (PROGN
                  (PRIN2* (CAR X))
                  (PRIN2* "=")
                  (PRIN2* (CDR X))
                  (PRIN2* "; ")))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (TERPRI* NIL)
            (PRIN2* "and chosen prime is ")
            (PROGN (PRIN2* (CADR W)) (TERPRI* NIL))
            (PROGN
             (PRIN2* "Image polynomial (made primitive) = ")
             (TERPRI* NIL))
            (PRINTSF (CADDR (CDDDR W)))
            (COND
             ((NOT (EQUAL (CADDR (CDDR W)) 1))
              (PROGN
               (PRIN2* " with (extracted) content of ")
               (PRINTSF (CADDR (CDDR W))))))
            (PRIN2* "The image polynomial mod ")
            (PRIN2* (CADR W))
            (PROGN (PRIN2* ", made monic, is:") (TERPRI* NIL))
            (PRINTSF (CADDR (CDR W)))
            (PROGN
             (PRIN2* "and factors of the primitive image mod this prime are:")
             (TERPRI* NIL))
            (PROG (X)
              (SETQ X (GETV MODULAR-INFO BEST-SET-POINTER))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (PRINTSF X)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (COND
             ((AND (SETQ FNUM (CADR (CDDR (CDDDR W)))) (NOT *OVERVIEW))
              (PROGN
               (PROGN
                (PRIN2* "The numeric images of each (square-free) factor of")
                (TERPRI* NIL))
               (PROGN
                (PRIN2* "the leading coefficient of the polynomial are as")
                (TERPRI* NIL))
               (PRIN2* "follows (in order):")
               (PRIN2* "  ")
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (LENGTH (CDR FACTORED-LC)) I))
                   (RETURN NIL)))
                 (PROGN (PRIN2* (GETV FNUM I)) (PRIN2* "; "))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (TERPRI* NIL)))))
           (WRS STREAM))))))) 
(PUT 'RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS 'NUMBER-OF-ARGS 0) 
(PUT 'RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS 'DEFINED-ON-LINE '582) 
(PUT 'RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS 'DEFINED-IN-FILE
     'FACTOR/FACPRIM.RED) 
(PUT 'RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS 'PROCEDURE_TYPE
     '(ARROW UNIT GENERAL)) 
(DE RECONSTRUCT-IMAGE-FACTORS-OVER-INTEGERS NIL
    (PROG (BEST-MODULUS BEST-FACTOR-COUNT INPUT-POLYNOMIAL
           INPUT-LEADING-COEFFICIENT BEST-KNOWN-FACTORS S W I X-IS-FACTOR
           X-FACTOR)
      (SETQ S (GETV VALID-IMAGE-SETS BEST-SET-POINTER))
      (SETQ BEST-KNOWN-FACTORS (GETV MODULAR-INFO BEST-SET-POINTER))
      (SETQ BEST-MODULUS (CADR S))
      (SETQ BEST-FACTOR-COUNT (LENGTH BEST-KNOWN-FACTORS))
      (SETQ INPUT-POLYNOMIAL (CADDR (CDDDR S)))
      (COND
       ((EQUAL (CDAAR INPUT-POLYNOMIAL) 1)
        (COND
         ((NOT (SETQ X-IS-FACTOR (NOT (NUMBERP (CADDR (CDDR S))))))
          (ERRORF
           (LIST "Trying to factor a linear image poly: " INPUT-POLYNOMIAL)))
         (T
          (PROG (BRECIP WW OM X-MOD-P)
            (SETQ NUMBER-OF-FACTORS 2)
            (SETQ PRIME-BASE BEST-MODULUS)
            (SETQ X-FACTOR (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1)))
            (PUTV VALID-IMAGE-SETS BEST-SET-POINTER
                  (LIST (CAR S) (CADR S) (CADDR S) (CADDR (CDR S))
                        (CDAR (CADDR (CDDR S)))
                        ((LAMBDA (G587)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF X-FACTOR G587))
                                 (T (POLY-MULTF X-FACTOR G587))))
                         (CADDR (CDDDR S)))
                        (CADDR (CDDDR (CDR S)))))
            (SETQ OM (SET-MODULUS BEST-MODULUS))
            (SETQ BRECIP
                    (MODULAR-RECIPROCAL
                     (CDR (SETQ WW (REDUCE-MOD-P INPUT-POLYNOMIAL)))))
            (SETQ X-MOD-P X-FACTOR)
            (SETQ ALPHALIST
                    (LIST (CONS X-MOD-P BRECIP)
                          (CONS WW
                                ((LAMBDA (A)
                                   (COND ((EQUAL A 0) A)
                                         (T (IDIFFERENCE CURRENT-MODULUS A))))
                                 (REMAINDER (TIMES BRECIP (CDAR WW))
                                            CURRENT-MODULUS)))))
            (DO-QUADRATIC-GROWTH (LIST X-FACTOR INPUT-POLYNOMIAL)
             (LIST X-MOD-P WW) BEST-MODULUS)
            (SETQ W (LIST INPUT-POLYNOMIAL))
            (SET-MODULUS OM)))))
       (T
        (PROGN
         (SETQ INPUT-LEADING-COEFFICIENT (CDAR INPUT-POLYNOMIAL))
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              (PROGN
               (PROGN
                (PRIN2*
                 "Next we use the Hensel Construction to grow these modular")
                (TERPRI* NIL))
               (PROGN
                (PRIN2* "factors into factors over the integers.")
                (TERPRI* NIL)))
              (WRS STREAM)))))
         (SETQ W (RECONSTRUCT.OVER.INTEGERS))
         (COND (IRREDUCIBLE (RETURN T)))
         (COND
          ((SETQ X-IS-FACTOR (NOT (NUMBERP (CADDR (CDDR S)))))
           (PROGN
            (SETQ NUMBER-OF-FACTORS (PLUS (LENGTH W) 1))
            (SETQ X-FACTOR (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1)))
            (PUTV VALID-IMAGE-SETS BEST-SET-POINTER
                  (LIST (CAR S) (CADR S) (CADDR S) (CADDR (CDR S))
                        (CDAR (CADDR (CDDR S)))
                        ((LAMBDA (G592)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF X-FACTOR G592))
                                 (T (POLY-MULTF X-FACTOR G592))))
                         (CADDR (CDDDR S)))
                        (CADDR (CDDDR (CDR S)))))
            (FIX-ALPHAS)))
          (T (SETQ NUMBER-OF-FACTORS (LENGTH W))))
         (COND ((EQUAL NUMBER-OF-FACTORS 1) (RETURN (SETQ IRREDUCIBLE T)))))))
      (COND
       ((GREATERP NUMBER-OF-FACTORS TARGET-FACTOR-COUNT)
        (RETURN (SETQ BAD-CASE (LIST (CAR S))))))
      (SETQ IMAGE-FACTORS (MKVECT NUMBER-OF-FACTORS))
      (SETQ I 1)
      (PROG (STREAM)
        (COND
         ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
          (SETQ STREAM (CONS NIL NIL)))
         (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
        (COND
         (STREAM
          (PROGN
           (SETQ STREAM (WRS (CDR STREAM)))
           (PROGN
            (PRIN2* "The full factors of the image polynomial are:")
            (TERPRI* NIL))
           (WRS STREAM)))))
      (PROG (IM-FACTOR)
        (SETQ IM-FACTOR W)
       LAB
        (COND ((NULL IM-FACTOR) (RETURN NIL)))
        ((LAMBDA (IM-FACTOR)
           (PROGN
            (PUTV IMAGE-FACTORS I IM-FACTOR)
            (PROG (STREAM)
              (COND
               ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                (SETQ STREAM (CONS NIL NIL)))
               (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
              (COND
               (STREAM
                (PROGN
                 (SETQ STREAM (WRS (CDR STREAM)))
                 (PRINTSF IM-FACTOR)
                 (WRS STREAM)))))
            (SETQ I (IADD1 I))))
         (CAR IM-FACTOR))
        (SETQ IM-FACTOR (CDR IM-FACTOR))
        (GO LAB))
      (COND
       (X-IS-FACTOR
        (PROGN
         (PUTV IMAGE-FACTORS I X-FACTOR)
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              (PROGN
               (PRINTSF X-FACTOR)
               (PRINTSF
                (CADDR (CDDR (GETV VALID-IMAGE-SETS BEST-SET-POINTER)))))
              (WRS STREAM)))))))))) 
(PUT 'DO-QUADRATIC-GROWTH 'NUMBER-OF-ARGS 3) 
(PUT 'DO-QUADRATIC-GROWTH 'DEFINED-ON-LINE '651) 
(PUT 'DO-QUADRATIC-GROWTH 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'DO-QUADRATIC-GROWTH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DO-QUADRATIC-GROWTH (FLIST MODFLIST P)
    (PROG (FHATVEC ALPHAVEC FACTORVEC MODFVEC FACVEC CURRENT-FACTOR-PRODUCT I
           DELTAM M)
      (SETQ FHATVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ ALPHAVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ FACTORVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ MODFVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ FACVEC (MKVECT NUMBER-OF-FACTORS))
      (SETQ CURRENT-FACTOR-PRODUCT 1)
      (SETQ I 0)
      (PROG (FF)
        (SETQ FF FLIST)
       LAB
        (COND ((NULL FF) (RETURN NIL)))
        ((LAMBDA (FF)
           (PROGN
            (PUTV FACTORVEC (SETQ I (IADD1 I)) FF)
            (SETQ CURRENT-FACTOR-PRODUCT
                    ((LAMBDA (G597)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF FF G597))
                             (T (POLY-MULTF FF G597))))
                     CURRENT-FACTOR-PRODUCT))))
         (CAR FF))
        (SETQ FF (CDR FF))
        (GO LAB))
      (SETQ I 0)
      (PROG (MODFF)
        (SETQ MODFF MODFLIST)
       LAB
        (COND ((NULL MODFF) (RETURN NIL)))
        ((LAMBDA (MODFF)
           (PROGN
            (PUTV MODFVEC (SETQ I (IADD1 I)) MODFF)
            (PUTV ALPHAVEC I (CDR (GET-ALPHA MODFF)))))
         (CAR MODFF))
        (SETQ MODFF (CDR MODFF))
        (GO LAB))
      (SETQ DELTAM P)
      (SETQ M (TIMES DELTAM DELTAM))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP M LARGEST-SMALL-MODULUS)) (RETURN NIL)))
        (PROGN (QUADRATIC-STEP M NUMBER-OF-FACTORS) (SETQ M (TIMES M DELTAM)))
        (GO WHILELABEL))
      (SETQ HENSEL-GROWTH-SIZE DELTAM)
      (SETQ ALPHALIST NIL)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS J)) (RETURN NIL)))
        (SETQ ALPHALIST
                (CONS
                 (CONS (REDUCE-MOD-P (GETV FACTORVEC J)) (GETV ALPHAVEC J))
                 ALPHALIST))
        (SETQ J (PLUS2 J 1))
        (GO LAB)))) 
(PUT 'FIX-ALPHAS 'NUMBER-OF-ARGS 0) 
(PUT 'FIX-ALPHAS 'DEFINED-ON-LINE '680) 
(PUT 'FIX-ALPHAS 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'FIX-ALPHAS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIX-ALPHAS NIL
    (PROG (OM F1 X-FACTOR A ARECIP B)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ F1 (REDUCE-MOD-P INPUT-POLYNOMIAL))
      (SETQ X-FACTOR (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1)))
      (SETQ ARECIP
              (MODULAR-RECIPROCAL
               (SETQ A (EVALUATE-MOD-P F1 M-IMAGE-VARIABLE 0))))
      (SETQ B
              (TIMES-MOD-P
               (COND ((EQUAL ARECIP 0) ARECIP)
                     (T (IDIFFERENCE CURRENT-MODULUS ARECIP)))
               (QUOTFAIL-MOD-P (DIFFERENCE-MOD-P F1 A) X-FACTOR)))
      (SETQ ALPHALIST
              (CONS (CONS X-FACTOR ARECIP)
                    (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                      (SETQ AA ALPHALIST)
                      (COND ((NULL AA) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (AA)
                                          (CONS (CAR AA)
                                                (REMAINDER-MOD-P
                                                 (TIMES-MOD-P B (CDR AA))
                                                 (CAR AA))))
                                        (CAR AA))
                                       NIL)))
                     LOOPLABEL
                      (SETQ AA (CDR AA))
                      (COND ((NULL AA) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (AA)
                                  (CONS (CAR AA)
                                        (REMAINDER-MOD-P
                                         (TIMES-MOD-P B (CDR AA)) (CAR AA))))
                                (CAR AA))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SET-MODULUS OM))) 
(PUT 'DETERMINE.LEADING.COEFFTS 'NUMBER-OF-ARGS 0) 
(PUT 'DETERMINE.LEADING.COEFFTS 'DEFINED-ON-LINE '704) 
(PUT 'DETERMINE.LEADING.COEFFTS 'DEFINED-IN-FILE 'FACTOR/FACPRIM.RED) 
(PUT 'DETERMINE.LEADING.COEFFTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DETERMINE.LEADING.COEFFTS NIL
    (PROG (DELTA C S)
      (SETQ S (GETV VALID-IMAGE-SETS BEST-SET-POINTER))
      (SETQ DELTA (CADDR (CDDR S)))
      (COND
       ((NOT
         ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
          (CDAR MULTIVARIATE-INPUT-POLY)))
        (PROGN
         (SETQ TRUE-LEADING-COEFFTS
                 (DISTRIBUTE.LC NUMBER-OF-FACTORS IMAGE-FACTORS S FACTORED-LC))
         (COND
          (BAD-CASE
           (PROGN
            (SETQ BAD-CASE (LIST (CAR S)))
            (SETQ TARGET-FACTOR-COUNT (DIFFERENCE NUMBER-OF-FACTORS 1))
            (COND ((EQUAL TARGET-FACTOR-COUNT 1) (SETQ IRREDUCIBLE T)))
            (RETURN BAD-CASE))))
         (SETQ DELTA (CAR TRUE-LEADING-COEFFTS))
         (SETQ TRUE-LEADING-COEFFTS (CDR TRUE-LEADING-COEFFTS))
         (COND
          ((NOT *OVERVIEW)
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PROGN
                  (PRIN2* "We now determine the leading coefficients of the ")
                  (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "factors of U by using the factors of the leading")
                  (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "coefficient of U and their (square-free) images")
                  (TERPRI* NIL))
                 (PROGN (PRIN2* "referred to earlier:") (TERPRI* NIL))
                 (PROG (I)
                   (SETQ I 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                   (PROGN
                    (PRINSF (GETV IMAGE-FACTORS I))
                    (PRIN2* " with l.c.: ")
                    (PRINTSF (GETV TRUE-LEADING-COEFFTS I)))
                   (SETQ I (PLUS2 I 1))
                   (GO LAB)))
                (WRS STREAM)))))))
         (COND
          ((NOT (ONEP DELTA))
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (COND
                  (*OVERVIEW
                   (PROGN
                    (PROGN
                     (PRIN2*
                      "In determining the leading coefficients of the factors")
                     (TERPRI* NIL))
                    (PRIN2* "of U, "))))
                 (PRIN2* "We have an integer factor, ")
                 (PRIN2* DELTA)
                 (PROGN (PRIN2* ", left over that we ") (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "cannot yet distribute correctly.")
                  (TERPRI* NIL)))
                (WRS STREAM)))))))))
       (T
        (PROGN
         (SETQ TRUE-LEADING-COEFFTS (MKVECT NUMBER-OF-FACTORS))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PUTV TRUE-LEADING-COEFFTS I (CDAR (GETV IMAGE-FACTORS I)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((NOT (ONEP DELTA))
           (PROG (STREAM)
             (COND
              ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
               (SETQ STREAM (CONS NIL NIL)))
              (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
             (COND
              (STREAM
               (PROGN
                (SETQ STREAM (WRS (CDR STREAM)))
                (PROGN
                 (PRIN2* "U has a leading coefficient = ")
                 (PRIN2* DELTA)
                 (PROGN (PRIN2* " which we cannot ") (TERPRI* NIL))
                 (PROGN
                  (PRIN2* "yet distribute correctly over the image factors.")
                  (TERPRI* NIL)))
                (WRS STREAM))))))))))
      (COND
       ((NOT (ONEP DELTA))
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PROGN
            (PUTV IMAGE-FACTORS I
                  ((LAMBDA (G599)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF DELTA G599))
                           (T (POLY-MULTF DELTA G599))))
                   (GETV IMAGE-FACTORS I)))
            (PUTV TRUE-LEADING-COEFFTS I
                  ((LAMBDA (G601)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF DELTA G601))
                           (T (POLY-MULTF DELTA G601))))
                   (GETV TRUE-LEADING-COEFFTS I))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (DIVIDE-ALL-ALPHAS DELTA)
         (SETQ C (EXPT DELTA (ISUB1 NUMBER-OF-FACTORS)))
         (SETQ MULTIVARIATE-INPUT-POLY
                 ((LAMBDA (G603)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF C G603))
                          (T (POLY-MULTF C G603))))
                  MULTIVARIATE-INPUT-POLY))
         (SETQ NON-MONIC T)
         (PROG (STREAM)
           (COND
            ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
             (SETQ STREAM (CONS NIL NIL)))
            (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
           (COND
            (STREAM
             (PROGN
              (SETQ STREAM (WRS (CDR STREAM)))
              (PROGN
               (PROGN
                (PRIN2* "(a) We multiply each of the image factors by the ")
                (TERPRI* NIL))
               (PROGN
                (PRIN2* "absolute value of this constant and multiply")
                (TERPRI* NIL))
               (PRIN2* "U by ")
               (COND
                ((NOT (EQUAL NUMBER-OF-FACTORS 2))
                 (PROGN
                  (PRIN2* DELTA)
                  (PRIN2* "**")
                  (PRIN2* (ISUB1 NUMBER-OF-FACTORS))))
                (T (PRIN2* DELTA)))
               (PROGN (PRIN2* " giving new image factors") (TERPRI* NIL))
               (PROGN (PRIN2* "as follows: ") (TERPRI* NIL))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                 (PRINTSF (GETV IMAGE-FACTORS I))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB)))
              (WRS STREAM)))))))))) 
(ENDMODULE) 