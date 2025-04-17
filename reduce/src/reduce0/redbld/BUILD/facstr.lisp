(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FACSTR)) 
(FLUID
 '(*TRFAC ALPHAVEC BAD-CASE BEST-KNOWN-FACTORS BEST-SET-POINTER CURRENT-MODULUS
   DEGREE-BOUNDS FACTOR-LEVEL FACTOR-TRACE-LIST FHATVEC FULL-GCD
   HENSEL-GROWTH-SIZE IMAGE-FACTORS IRREDUCIBLE M-IMAGE-VARIABLE
   MULTIVARIATE-FACTORS MULTIVARIATE-INPUT-POLY NON-MONIC NUMBER-OF-FACTORS
   PREDICTIONS PRIME-BASE RECONSTRUCTING-GCD TARGET-FACTOR-COUNT
   VALID-IMAGE-SETS)) 
(PUT 'RECONSTRUCT-MULTIVARIATE-FACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'RECONSTRUCT-MULTIVARIATE-FACTORS 'DEFINED-ON-LINE '57) 
(PUT 'RECONSTRUCT-MULTIVARIATE-FACTORS 'DEFINED-IN-FILE 'FACTOR/FACSTR.RED) 
(PUT 'RECONSTRUCT-MULTIVARIATE-FACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RECONSTRUCT-MULTIVARIATE-FACTORS (VSET-MOD-P)
    ((LAMBDA (FACTOR-LEVEL)
       (PROG (S OM U0 ALPHAVEC PREDICTIONS BEST-FACTORS-MOD-P FHATVEC W1
              FVEC-MOD-P D DEGREE-BOUNDS LC-VEC)
         (SETQ ALPHAVEC (MKVECT NUMBER-OF-FACTORS))
         (SETQ BEST-FACTORS-MOD-P (MKVECT NUMBER-OF-FACTORS))
         (SETQ LC-VEC (MKVECT NUMBER-OF-FACTORS))
         (COND
          ((NOT
            (LESSP (SETQ D (MAX-DEGREE MULTIVARIATE-INPUT-POLY 0)) PRIME-BASE))
           (SETQ FVEC-MOD-P (CHOOSE-LARGER-PRIME D))))
         (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
         (COND
          ((NULL FVEC-MOD-P)
           (PROGN
            (SETQ FVEC-MOD-P (MKVECT NUMBER-OF-FACTORS))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
              (PUTV FVEC-MOD-P I (REDUCE-MOD-P (GETV IMAGE-FACTORS I)))
              (SETQ I (PLUS2 I 1))
              (GO LAB)))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PROGN
            (PUTV ALPHAVEC I (CDR (GET-ALPHA (GETV FVEC-MOD-P I))))
            (PUTV BEST-FACTORS-MOD-P I
                  (REDUCE-MOD-P (GETV BEST-KNOWN-FACTORS I)))
            (PUTV LC-VEC I (CDAR (GETV BEST-KNOWN-FACTORS I))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((NOT RECONSTRUCTING-GCD)
           (PROGN
            (SETQ S (GETV VALID-IMAGE-SETS BEST-SET-POINTER))
            (SETQ VSET-MOD-P
                    (PROG (V FORALL-RESULT FORALL-ENDPTR)
                      (SETQ V (CAR S))
                      (COND ((NULL V) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (V)
                                          (CONS (CAR V)
                                                (MODULAR-NUMBER (CDR V))))
                                        (CAR V))
                                       NIL)))
                     LOOPLABEL
                      (SETQ V (CDR V))
                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (V)
                                  (CONS (CAR V) (MODULAR-NUMBER (CDR V))))
                                (CAR V))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))
         (SETQ U0 (REDUCE-MOD-P MULTIVARIATE-INPUT-POLY))
         (SETQ S 1)
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (SETQ S
                   ((LAMBDA (G567)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF S G567))
                            (T (POLY-MULTF S G567))))
                    (GETV BEST-KNOWN-FACTORS I)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SET-DEGREE-BOUNDS VSET-MOD-P MULTIVARIATE-INPUT-POLY S)
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
                 "We use the Hensel Construction to grow univariate modular")
                (TERPRI* NIL))
               (PROGN
                (PRIN2*
                 "factors into multivariate modular factors, which will in")
                (TERPRI* NIL))
               (PROGN
                (PRIN2* "turn be used in the later Hensel construction.  The")
                (TERPRI* NIL))
               (PROGN (PRIN2* "starting modular factors are:") (TERPRI* NIL))
               (EZGCD_PRINTVEC " f(" NUMBER-OF-FACTORS ")=" BEST-FACTORS-MOD-P)
               (PRIN2* "The modulus is ")
               (PROGN (PRIN2* CURRENT-MODULUS) (TERPRI* NIL)))
              (WRS STREAM)))))
         (FIND-MULTIVARIATE-FACTORS-MOD-P U0 BEST-FACTORS-MOD-P VSET-MOD-P)
         (COND
          (BAD-CASE
           (PROGN
            (SETQ TARGET-FACTOR-COUNT (DIFFERENCE NUMBER-OF-FACTORS 1))
            (COND ((EQUAL TARGET-FACTOR-COUNT 1) (SETQ IRREDUCIBLE T)))
            (SET-MODULUS OM)
            (RETURN BAD-CASE))))
         (SETQ FHATVEC
                 (MAKE-MULTIVARIATE-HATVEC-MOD-P BEST-FACTORS-MOD-P
                  NUMBER-OF-FACTORS))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PUTV FVEC-MOD-P I (GETV BEST-FACTORS-MOD-P I))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (MAKE-VEC-MODULAR-SYMMETRIC BEST-FACTORS-MOD-P NUMBER-OF-FACTORS)
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PROGN
            (PUTV BEST-KNOWN-FACTORS I
                  (FORCE-LC (GETV BEST-FACTORS-MOD-P I) (GETV LC-VEC I)))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ W1
                 (HENSEL-MOD-P MULTIVARIATE-INPUT-POLY FVEC-MOD-P
                  BEST-KNOWN-FACTORS
                  (GET.COEFFT.BOUND MULTIVARIATE-INPUT-POLY
                   (TOTAL-DEGREE-IN-POWERS MULTIVARIATE-INPUT-POLY NIL))
                  VSET-MOD-P HENSEL-GROWTH-SIZE))
         (COND
          ((EQUAL (CAR W1) 'OVERSHOT)
           (PROGN
            (SETQ TARGET-FACTOR-COUNT (DIFFERENCE NUMBER-OF-FACTORS 1))
            (COND ((EQUAL TARGET-FACTOR-COUNT 1) (SETQ IRREDUCIBLE T)))
            (SET-MODULUS OM)
            (RETURN (SETQ BAD-CASE T)))))
         (COND ((NOT (EQUAL (CAR W1) 'OK)) (ERRORF W1)))
         (COND
          (RECONSTRUCTING-GCD
           (PROGN
            (SETQ FULL-GCD
                    (COND
                     (NON-MONIC
                      (CAR
                       (PRIMITIVE.PARTS (LIST (GETV (CDR W1) 1))
                        M-IMAGE-VARIABLE NIL)))
                     (T (GETV (CDR W1) 1))))
            (SET-MODULUS OM)
            (RETURN FULL-GCD))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (GETV (CDR W1) 0) I)) (RETURN NIL)))
           (SETQ MULTIVARIATE-FACTORS
                   (CONS (GETV (CDR W1) I) MULTIVARIATE-FACTORS))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          (NON-MONIC
           (SETQ MULTIVARIATE-FACTORS
                   (PRIMITIVE.PARTS MULTIVARIATE-FACTORS M-IMAGE-VARIABLE
                    NIL))))
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
                (PRIN2* "The full multivariate factors are:")
                (TERPRI* NIL))
               (PROG (X)
                 (SETQ X MULTIVARIATE-FACTORS)
                LAB
                 (COND ((NULL X) (RETURN NIL)))
                 ((LAMBDA (X) (PRINTSF X)) (CAR X))
                 (SETQ X (CDR X))
                 (GO LAB)))
              (WRS STREAM)))))
         (SET-MODULUS OM)))
     (TIMES FACTOR-LEVEL 100))) 
(ENDMODULE) 