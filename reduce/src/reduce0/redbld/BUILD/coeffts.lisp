(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'COEFFTS)) 
(FLUID
 '(*TRFAC ALPHALIST BEST-KNOWN-FACTOR-LIST BEST-KNOWN-FACTORS COEFFT-VECTORS
   DEG-OF-UNKNOWN DIFFERENCE-FOR-UNKNOWN DIVISOR-FOR-UNKNOWN FACTOR-LEVEL
   FACTOR-TRACE-LIST FULL-GCD HENSEL-GROWTH-SIZE IMAGE-FACTORS M-IMAGE-VARIABLE
   MULTIVARIATE-FACTORS MULTIVARIATE-INPUT-POLY NON-MONIC NUMBER-OF-FACTORS
   POLYZERO RECONSTRUCTING-GCD TRUE-LEADING-COEFFTS UNKNOWN UNKNOWNS-LIST)) 
(PUT 'DETERMINE-MORE-COEFFTS 'NUMBER-OF-ARGS 0) 
(PUT 'DETERMINE-MORE-COEFFTS 'DEFINED-ON-LINE '60) 
(PUT 'DETERMINE-MORE-COEFFTS 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'DETERMINE-MORE-COEFFTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DETERMINE-MORE-COEFFTS NIL
    (PROG (UNKNOWNS-LIST UV R W BEST-KNOWN-FACTOR-LIST)
      (SETQ BEST-KNOWN-FACTORS (MKVECT NUMBER-OF-FACTORS))
      (SETQ UV (MKVECT NUMBER-OF-FACTORS))
      (PROG (I)
        (SETQ I NUMBER-OF-FACTORS)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PUTV UV I
              (CONVERT-FACTOR-TO-TERMVECTOR (GETV IMAGE-FACTORS I)
               (GETV TRUE-LEADING-COEFFTS I)))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SETQ R (CDR MULTIVARIATE-INPUT-POLY))
      (COND
       ((OR
         (NOT
          ((LAMBDA (|##Z|)
             (AND (NOT (OR (ATOM |##Z|) (ATOM (CAR |##Z|))))
                  (EQUAL (CAAAR |##Z|) M-IMAGE-VARIABLE)))
           R))
         (NULL
          (SETQ W (TRY-FIRST-COEFFT (CDAAR R) (CDAR R) UNKNOWNS-LIST UV))))
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PUTV BEST-KNOWN-FACTORS I
                 (FORCE-LC (GETV IMAGE-FACTORS I)
                  (GETV TRUE-LEADING-COEFFTS I)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ COEFFT-VECTORS UV)
         (RETURN NIL))))
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
             (PRIN2* "By exploiting any sparsity wrt the main variable in the")
             (TERPRI* NIL))
            (PROGN
             (PRIN2* "factors, we can try guessing some of the multivariate")
             (TERPRI* NIL))
            (PROGN (PRIN2* "coefficients.") (TERPRI* NIL)))
           (WRS STREAM)))))
      (TRY-OTHER-COEFFTS R UNKNOWNS-LIST UV)
      (SETQ W (CONVERT-AND-TRIAL-DIVIDE UV))
      (RETURN (SET-UP-GLOBALS UV W)))) 
(PUT 'CONVERT-FACTOR-TO-TERMVECTOR 'NUMBER-OF-ARGS 2) 
(PUT 'CONVERT-FACTOR-TO-TERMVECTOR 'DEFINED-ON-LINE '91) 
(PUT 'CONVERT-FACTOR-TO-TERMVECTOR 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'CONVERT-FACTOR-TO-TERMVECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONVERT-FACTOR-TO-TERMVECTOR (U TLC)
    (PROG (TERMLIST RES N SLIST)
      (SETQ TERMLIST
              (CONS (CONS (CDAAR U) TLC) (LIST-TERMS-IN-FACTOR (CDR U))))
      (SETQ RES (MKVECT (SETQ N (LENGTH TERMLIST))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ SLIST (CONS (CONS (CAAR TERMLIST) I) SLIST))
         (PUTV RES I (CAR TERMLIST))
         (SETQ TERMLIST (CDR TERMLIST)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PUTV RES 0 (CONS N (IDIFFERENCE N 1)))
      (SETQ UNKNOWNS-LIST (CONS (REVERSIP SLIST) UNKNOWNS-LIST))
      (RETURN RES))) 
(PUT 'TRY-FIRST-COEFFT 'NUMBER-OF-ARGS 4) 
(PUT 'TRY-FIRST-COEFFT 'DEFINED-ON-LINE '105) 
(PUT 'TRY-FIRST-COEFFT 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'TRY-FIRST-COEFFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRY-FIRST-COEFFT (N C SLIST UV)
    (PROG (COMBNS UNKNOWN W L D V M)
      (SETQ COMBNS (GET-TERM N SLIST))
      (COND ((OR (EQUAL COMBNS 'NO) (NOT (NULL (CDR COMBNS)))) (RETURN NIL)))
      (SETQ L (CAR COMBNS))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV (GETV UV I) (CAR L)))
         (COND
          ((NULL (CDR W))
           (PROGN
            (COND
             (UNKNOWN (PROGN (SETQ C NIL) (SETQ I (PLUS NUMBER-OF-FACTORS 1))))
             (T (PROGN (SETQ UNKNOWN (CONS I (CAR L))) (SETQ D (CAR W)))))))
          (T
           (PROGN
            (SETQ C ((LAMBDA (*EXP) (QUOTF1 C (CDR W))) T))
            (COND ((NULL C) (SETQ I (PLUS NUMBER-OF-FACTORS 1)))))))
         (SETQ L (CDR L)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((NULL C) (RETURN NIL)))
      (PUTV (SETQ V (GETV UV (CAR UNKNOWN))) (CDR UNKNOWN) (CONS D C))
      (SETQ M (GETV V 0))
      (PUTV V 0 (CONS (CAR M) (IDIFFERENCE (CDR M) 1)))
      (COND ((AND (EQUAL (CDR M) 1) (FACTORS-COMPLETE UV)) (RETURN 'COMPLETE)))
      (RETURN C))) 
(PUT 'SOLVE-NEXT-COEFFT 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVE-NEXT-COEFFT 'DEFINED-ON-LINE '128) 
(PUT 'SOLVE-NEXT-COEFFT 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'SOLVE-NEXT-COEFFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-NEXT-COEFFT (N C SLIST UV)
    (PROG (COMBNS W UNKNOWN DEG-OF-UNKNOWN DIVISOR-FOR-UNKNOWN
           DIFFERENCE-FOR-UNKNOWN V)
      (SETQ DIFFERENCE-FOR-UNKNOWN POLYZERO)
      (SETQ DIVISOR-FOR-UNKNOWN POLYZERO)
      (SETQ COMBNS (GET-TERM N SLIST))
      (COND ((EQUAL COMBNS 'NO) (RETURN 'NOGOOD)))
      (PROG ()
       WHILELABEL
        (COND ((NOT COMBNS) (RETURN NIL)))
        (PROGN
         (SETQ W (SPLIT-TERM-LIST (CAR COMBNS) UV))
         (COND ((EQUAL W 'NOGOOD) (SETQ COMBNS NIL))
               (T (SETQ COMBNS (CDR COMBNS)))))
        (GO WHILELABEL))
      (COND ((EQUAL W 'NOGOOD) (RETURN W)))
      (COND ((NULL UNKNOWN) (RETURN NIL)))
      (SETQ W
              ((LAMBDA (*EXP)
                 (QUOTF1 (ADDF C (NEGF DIFFERENCE-FOR-UNKNOWN))
                         DIVISOR-FOR-UNKNOWN))
               T))
      (COND ((NULL W) (RETURN 'NOGOOD)))
      (PUTV (SETQ V (GETV UV (CAR UNKNOWN))) (CDR UNKNOWN)
            (CONS DEG-OF-UNKNOWN W))
      (SETQ N (GETV V 0))
      (PUTV V 0 (CONS (CAR N) (IDIFFERENCE (CDR N) 1)))
      (COND ((AND (EQUAL (CDR N) 1) (FACTORS-COMPLETE UV)) (RETURN 'COMPLETE)))
      (RETURN W))) 
(PUT 'SPLIT-TERM-LIST 'NUMBER-OF-ARGS 2) 
(PUT 'SPLIT-TERM-LIST 'DEFINED-ON-LINE '151) 
(PUT 'SPLIT-TERM-LIST 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'SPLIT-TERM-LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPLIT-TERM-LIST (TERM-COMBN UV)
    (PROG (A V W)
      (SETQ A 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV (GETV UV I) (CAR TERM-COMBN)))
         (COND
          ((NULL (CDR W))
           (COND
            ((OR V
                 (AND UNKNOWN (NOT (EQUAL (CONS I (CAR TERM-COMBN)) UNKNOWN))))
             (PROGN (SETQ V 'NOGOOD) (SETQ I (PLUS NUMBER-OF-FACTORS 1))))
            (T
             (PROGN
              (SETQ UNKNOWN (CONS I (CAR TERM-COMBN)))
              (SETQ DEG-OF-UNKNOWN (CAR W))
              (SETQ V UNKNOWN)))))
          (T
           (SETQ A
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF A (CDR W)))
                         (T (POLY-MULTF A (CDR W)))))))
         (COND ((NOT (EQ V 'NOGOOD)) (SETQ TERM-COMBN (CDR TERM-COMBN)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((EQUAL V 'NOGOOD) (RETURN V)))
      (COND (V (SETQ DIVISOR-FOR-UNKNOWN (ADDF DIVISOR-FOR-UNKNOWN A)))
            (T (SETQ DIFFERENCE-FOR-UNKNOWN (ADDF DIFFERENCE-FOR-UNKNOWN A))))
      (RETURN 'OK))) 
(PUT 'FACTORS-COMPLETE 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORS-COMPLETE 'DEFINED-ON-LINE '172) 
(PUT 'FACTORS-COMPLETE 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'FACTORS-COMPLETE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORS-COMPLETE (UV)
    (PROG (FACTOR-NOT-DONE R)
      (SETQ R T)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (COND
         ((NOT (EQUAL (CDR (GETV (GETV UV I) 0)) 0))
          (COND
           (FACTOR-NOT-DONE
            (PROGN (SETQ R NIL) (SETQ I (PLUS NUMBER-OF-FACTORS 1))))
           (T (SETQ FACTOR-NOT-DONE T)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN R))) 
(PUT 'CONVERT-AND-TRIAL-DIVIDE 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT-AND-TRIAL-DIVIDE 'DEFINED-ON-LINE '183) 
(PUT 'CONVERT-AND-TRIAL-DIVIDE 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'CONVERT-AND-TRIAL-DIVIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT-AND-TRIAL-DIVIDE (UV)
    (PROG (W R FDONE-PRODUCT-MOD-P OM)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ FDONE-PRODUCT-MOD-P 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV UV I))
         (SETQ W
                 (COND ((EQUAL (CDR (GETV W 0)) 0) (TERMVECTOR2SF W))
                       (T (MERGE-TERMS (GETV IMAGE-FACTORS I) W))))
         (SETQ R ((LAMBDA (*EXP) (QUOTF1 MULTIVARIATE-INPUT-POLY W)) T))
         (COND
          ((NULL R)
           (SETQ BEST-KNOWN-FACTOR-LIST
                   (CONS (CONS I W) BEST-KNOWN-FACTOR-LIST)))
          ((AND RECONSTRUCTING-GCD (EQUAL I 1))
           (PROGN
            (SETQ FULL-GCD
                    (COND
                     (NON-MONIC
                      (CAR (PRIMITIVE.PARTS (LIST W) M-IMAGE-VARIABLE NIL)))
                     (T W)))
            (SETQ I (PLUS NUMBER-OF-FACTORS 1))))
          (T
           (PROGN
            (SETQ MULTIVARIATE-FACTORS (CONS W MULTIVARIATE-FACTORS))
            (SETQ FDONE-PRODUCT-MOD-P
                    (TIMES-MOD-P (REDUCE-MOD-P (GETV IMAGE-FACTORS I))
                     FDONE-PRODUCT-MOD-P))
            (SETQ MULTIVARIATE-INPUT-POLY R)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND (FULL-GCD (RETURN NIL)))
      (COND
       ((NULL BEST-KNOWN-FACTOR-LIST)
        (SETQ MULTIVARIATE-FACTORS
                (PRIMITIVE.PARTS MULTIVARIATE-FACTORS M-IMAGE-VARIABLE NIL)))
       ((NULL (CDR BEST-KNOWN-FACTOR-LIST))
        (PROGN
         (COND
          (RECONSTRUCTING-GCD
           (COND
            ((NOT (EQUAL (CAAR BEST-KNOWN-FACTOR-LIST) 1))
             (ERRORF "gcd is jiggered in determining other coeffts"))
            (T
             (SETQ FULL-GCD
                     (COND
                      (NON-MONIC
                       (CAR
                        (PRIMITIVE.PARTS (LIST MULTIVARIATE-INPUT-POLY)
                         M-IMAGE-VARIABLE NIL)))
                      (T MULTIVARIATE-INPUT-POLY))))))
          (T
           (SETQ MULTIVARIATE-FACTORS
                   (PRIMITIVE.PARTS
                    (CONS MULTIVARIATE-INPUT-POLY MULTIVARIATE-FACTORS)
                    M-IMAGE-VARIABLE NIL))))
         (SETQ BEST-KNOWN-FACTOR-LIST NIL))))
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
             ((NULL BEST-KNOWN-FACTOR-LIST)
              (PROGN
               (PRIN2*
                "We have completely determined all the factors this way")
               (TERPRI* NIL)))
             (MULTIVARIATE-FACTORS
              (PROGN
               (PRIN2* "We have completely determined the following factor")
               (PROGN
                (PRIN2*
                 (COND ((EQUAL (LENGTH MULTIVARIATE-FACTORS) 1) ":") (T "s:")))
                (TERPRI* NIL))
               (PROG (WW)
                 (SETQ WW MULTIVARIATE-FACTORS)
                LAB
                 (COND ((NULL WW) (RETURN NIL)))
                 ((LAMBDA (WW) (PRINTSF WW)) (CAR WW))
                 (SETQ WW (CDR WW))
                 (GO LAB))))))
           (WRS STREAM)))))
      (SET-MODULUS OM)
      (RETURN FDONE-PRODUCT-MOD-P))) 
(PUT 'SET-UP-GLOBALS 'NUMBER-OF-ARGS 2) 
(PUT 'SET-UP-GLOBALS 'DEFINED-ON-LINE '232) 
(PUT 'SET-UP-GLOBALS 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'SET-UP-GLOBALS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SET-UP-GLOBALS (UV F-PRODUCT)
    (COND ((OR (NULL BEST-KNOWN-FACTOR-LIST) FULL-GCD) 'DONE)
          (T
           (PROG (I R N K FLIST-MOD-P IMF OM SAVEK)
             (SETQ N (LENGTH BEST-KNOWN-FACTOR-LIST))
             (SETQ BEST-KNOWN-FACTORS (MKVECT N))
             (SETQ COEFFT-VECTORS (MKVECT N))
             (SETQ R (MKVECT N))
             (SETQ K (COND (RECONSTRUCTING-GCD 1) (T 0)))
             (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
             (PROG (W)
               (SETQ W BEST-KNOWN-FACTOR-LIST)
              LAB
               (COND ((NULL W) (RETURN NIL)))
               ((LAMBDA (W)
                  (PROGN
                   (SETQ I (CAR W))
                   (SETQ W (CDR W))
                   (COND
                    ((AND RECONSTRUCTING-GCD (EQUAL I 1))
                     (PROGN (SETQ SAVEK K) (SETQ K 1)))
                    (T (SETQ K (IPLUS2 K 1))))
                   (PUTV R K (SETQ IMF (GETV IMAGE-FACTORS I)))
                   (SETQ FLIST-MOD-P (CONS (REDUCE-MOD-P IMF) FLIST-MOD-P))
                   (PUTV BEST-KNOWN-FACTORS K W)
                   (PUTV COEFFT-VECTORS K (GETV UV I))
                   (COND ((AND RECONSTRUCTING-GCD (EQUAL K 1)) (SETQ K SAVEK)))
                   NIL))
                (CAR W))
               (SETQ W (CDR W))
               (GO LAB))
             (COND
              ((NOT (EQUAL N NUMBER-OF-FACTORS))
               (PROGN
                (SETQ ALPHALIST
                        (PROG (MODF FORALL-RESULT FORALL-ENDPTR)
                          (SETQ MODF FLIST-MOD-P)
                          (COND ((NULL MODF) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (MODF)
                                              (CONS MODF
                                                    (REMAINDER-MOD-P
                                                     (TIMES-MOD-P F-PRODUCT
                                                      (CDR (GET-ALPHA MODF)))
                                                     MODF)))
                                            (CAR MODF))
                                           NIL)))
                         LOOPLABEL
                          (SETQ MODF (CDR MODF))
                          (COND ((NULL MODF) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (MODF)
                                      (CONS MODF
                                            (REMAINDER-MOD-P
                                             (TIMES-MOD-P F-PRODUCT
                                              (CDR (GET-ALPHA MODF)))
                                             MODF)))
                                    (CAR MODF))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ NUMBER-OF-FACTORS N))))
             (SET-MODULUS OM)
             (SETQ IMAGE-FACTORS R)
             (RETURN '|NEED TO RECONSTRUCT|))))) 
(PUT 'GET-TERM 'NUMBER-OF-ARGS 2) 
(PUT 'GET-TERM 'DEFINED-ON-LINE '265) 
(PUT 'GET-TERM 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'GET-TERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET-TERM (N L)
    (COND ((ILESSP N 0) 'NO) ((NULL (CDR L)) (GET-TERM-N N (CAR L)))
          (T
           (PROG (W RES)
             (PROG (FTERM)
               (SETQ FTERM (CAR L))
              LAB
               (COND ((NULL FTERM) (RETURN NIL)))
               ((LAMBDA (FTERM)
                  (PROGN
                   (SETQ W (GET-TERM (IDIFFERENCE N (CAR FTERM)) (CDR L)))
                   (COND
                    ((NOT (EQUAL W 'NO))
                     (SETQ RES
                             (APPEND
                              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                (SETQ V W)
                                (COND ((NULL V) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (V)
                                                    (CONS (CDR FTERM) V))
                                                  (CAR V))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ V (CDR V))
                                (COND ((NULL V) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (V) (CONS (CDR FTERM) V))
                                          (CAR V))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))
                              RES))))))
                (CAR FTERM))
               (SETQ FTERM (CDR FTERM))
               (GO LAB))
             (RETURN (COND ((NULL RES) 'NO) (T RES))))))) 
(PUT 'GET-TERM-N 'NUMBER-OF-ARGS 2) 
(PUT 'GET-TERM-N 'DEFINED-ON-LINE '277) 
(PUT 'GET-TERM-N 'DEFINED-IN-FILE 'FACTOR/COEFFTS.RED) 
(PUT 'GET-TERM-N 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET-TERM-N (N U)
    (COND ((OR (NULL U) (IGREATERP N (CAAR U))) 'NO)
          ((EQUAL (CAAR U) N) (LIST (CONS (CDAR U) NIL)))
          (T (GET-TERM-N N (CDR U))))) 
(ENDMODULE) 