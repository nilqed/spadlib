(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MULTIHEN)) 
(FLUID
 '(*OVERSHOOT *TRFAC ALPHAVEC BAD-CASE FACTOR-LEVEL FACTOR-TRACE-LIST FHATVEC
   HENSEL-GROWTH-SIZE MAX-UNKNOWNS NUMBER-OF-FACTORS NUMBER-OF-UNKNOWNS
   PREDICTIONS)) 
(PUT 'FIND-MULTIVARIATE-FACTORS-MOD-P 'NUMBER-OF-ARGS 3) 
(PUT 'FIND-MULTIVARIATE-FACTORS-MOD-P 'DEFINED-ON-LINE '45) 
(PUT 'FIND-MULTIVARIATE-FACTORS-MOD-P 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MULTIVARIATE-FACTORS-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-MULTIVARIATE-FACTORS-MOD-P (POLY BEST-FACTORS VARIABLE-SET)
    (COND ((NULL VARIABLE-SET) BEST-FACTORS)
          (T
           ((LAMBDA (FACTOR-LEVEL)
              (PROG (GROWTH-FACTOR B0S RES V BHAT0S W DEGBD FIRST-TIME REDPOLY
                     PREDICTED-FORMS NUMBER-OF-UNKNOWNS SOLVE-COUNT
                     CORRECTION-VECTORS SOLN-MATRICES MAX-UNKNOWNS
                     UNKNOWNS-COUNT-LIST POLY-REMAINING PREDICTION-RESULTS
                     ONE-PREDICTION-FAILED)
                (SETQ V (CAR VARIABLE-SET))
                (SETQ DEGBD (GET-DEGREE-BOUND (CAR V)))
                (SETQ FIRST-TIME T)
                (SETQ GROWTH-FACTOR (MAKE-GROWTH-FACTOR V))
                (SETQ POLY-REMAINING POLY)
                (SETQ PREDICTION-RESULTS (MKVECT NUMBER-OF-FACTORS))
                (FIND-MSG1 BEST-FACTORS GROWTH-FACTOR POLY)
                (SETQ B0S
                        (REDUCE-VEC-BY-ONE-VAR-MOD-P BEST-FACTORS V
                         NUMBER-OF-FACTORS))
                (PROG (I)
                  (SETQ I 1)
                 LAB
                  (COND
                   ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                  (PUTV BEST-FACTORS I
                        (DIFFERENCE-MOD-P (GETV BEST-FACTORS I) (GETV B0S I)))
                  (SETQ I (PLUS2 I 1))
                  (GO LAB))
                (SETQ REDPOLY (EVALUATE-MOD-P POLY (CAR V) (CDR V)))
                (FIND-MSG2 V VARIABLE-SET)
                (FIND-MULTIVARIATE-FACTORS-MOD-P REDPOLY B0S
                 (CDR VARIABLE-SET))
                (COND (BAD-CASE (RETURN NIL)))
                (PROG (I)
                  (SETQ I 1)
                 LAB
                  (COND
                   ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                  (PUTV BEST-FACTORS I
                        (PLUS-MOD-P (GETV B0S I) (GETV BEST-FACTORS I)))
                  (SETQ I (PLUS2 I 1))
                  (GO LAB))
                (FIND-MSG3 BEST-FACTORS V)
                (SETQ RES
                        (DIFF-OVER-K-MOD-P
                         (DIFFERENCE-MOD-P POLY
                          (TIMES-VECTOR-MOD-P BEST-FACTORS NUMBER-OF-FACTORS))
                         1 (CAR V)))
                (PROG (STREAM)
                  (COND
                   ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                    (SETQ STREAM (CONS NIL NIL)))
                   (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                  (COND
                   (STREAM
                    (PROGN
                     (SETQ STREAM (WRS (CDR STREAM)))
                     (PROGN (PRINTSF RES) (TERPRI* NIL))
                     (WRS STREAM)))))
                (COND
                 ((AND (NOT (NULL RES)) (CDR VARIABLE-SET)
                       (NOT (ZEROP (CDR V))))
                  (PROGN
                   (SETQ PREDICTED-FORMS
                           (MAKE-BIVARIATE-VEC-MOD-P BEST-FACTORS
                            (CDR VARIABLE-SET) (CAR V) NUMBER-OF-FACTORS))
                   (FIND-MULTIVARIATE-FACTORS-MOD-P
                    (MAKE-BIVARIATE-MOD-P POLY (CDR VARIABLE-SET) (CAR V))
                    PREDICTED-FORMS (LIST V))
                   (FIND-MSG4 PREDICTED-FORMS V)
                   (MAKE-PREDICTED-FORMS PREDICTED-FORMS (CAR V))
                   (FIND-MSG5)
                   (SETQ UNKNOWNS-COUNT-LIST NUMBER-OF-UNKNOWNS)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT
                        (AND UNKNOWNS-COUNT-LIST
                             (EQUAL (CAR (SETQ W (CAR UNKNOWNS-COUNT-LIST)))
                                    1)))
                       (RETURN NIL)))
                     (PROG (I R)
                       (SETQ UNKNOWNS-COUNT-LIST (CDR UNKNOWNS-COUNT-LIST))
                       (SETQ I (CDR W))
                       (SETQ W
                               (QUOTIENT-MOD-P POLY-REMAINING
                                (SETQ R (GETV BEST-FACTORS I))))
                       (COND
                        ((OR (NULL W)
                             (NOT
                              (NULL
                               (DIFFERENCE-MOD-P POLY-REMAINING
                                (TIMES-MOD-P W R)))))
                         (COND
                          (ONE-PREDICTION-FAILED
                           (PROGN
                            (PROG (STREAM)
                              (COND
                               ((OR *TRALLFAC
                                    (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                                (SETQ STREAM (CONS NIL NIL)))
                               (T
                                (SETQ STREAM
                                        (ASSOC FACTOR-LEVEL
                                               FACTOR-TRACE-LIST))))
                              (COND
                               (STREAM
                                (PROGN
                                 (SETQ STREAM (WRS (CDR STREAM)))
                                 (PROGN
                                  (PRIN2* "Predictions are no good")
                                  (TERPRI* NIL))
                                 (WRS STREAM)))))
                            (SETQ MAX-UNKNOWNS NIL)))
                          (T
                           (PROGN
                            (PROG (STREAM)
                              (COND
                               ((OR *TRALLFAC
                                    (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                                (SETQ STREAM (CONS NIL NIL)))
                               (T
                                (SETQ STREAM
                                        (ASSOC FACTOR-LEVEL
                                               FACTOR-TRACE-LIST))))
                              (COND
                               (STREAM
                                (PROGN
                                 (SETQ STREAM (WRS (CDR STREAM)))
                                 (PROGN
                                  (PRIN2* "Guess for f(")
                                  (PRIN2* I)
                                  (PROGN (PRIN2* ") was bad.") (TERPRI* NIL)))
                                 (WRS STREAM)))))
                            (SETQ ONE-PREDICTION-FAILED I)))))
                        (T
                         (PROGN
                          (PUTV PREDICTION-RESULTS I R)
                          (PROG (STREAM)
                            (COND
                             ((OR *TRALLFAC
                                  (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                              (SETQ STREAM (CONS NIL NIL)))
                             (T
                              (SETQ STREAM
                                      (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                            (COND
                             (STREAM
                              (PROGN
                               (SETQ STREAM (WRS (CDR STREAM)))
                               (PROGN
                                (PRIN2* "Prediction for f(")
                                (PRIN2* I)
                                (PRIN2* ") worked: ")
                                (PRINTSF R))
                               (WRS STREAM)))))
                          (SETQ POLY-REMAINING W)))))
                     (GO WHILELABEL))
                   (SETQ W (LENGTH UNKNOWNS-COUNT-LIST))
                   (COND
                    ((AND (EQUAL W 1) (NOT ONE-PREDICTION-FAILED))
                     (PROGN
                      (PUTV BEST-FACTORS (CDAR UNKNOWNS-COUNT-LIST)
                            POLY-REMAINING)
                      (GO EXIT)))
                    ((AND NIL (EQUAL W 0) ONE-PREDICTION-FAILED)
                     (PROGN
                      (PUTV BEST-FACTORS ONE-PREDICTION-FAILED POLY-REMAINING)
                      (GO EXIT))))
                   (SETQ SOLVE-COUNT 1)
                   (COND
                    (MAX-UNKNOWNS
                     (SETQ CORRECTION-VECTORS
                             (MAKE-CORRECTION-VECTORS BEST-FACTORS
                              MAX-UNKNOWNS)))))))
                (SETQ BHAT0S
                        (MAKE-MULTIVARIATE-HATVEC-MOD-P B0S NUMBER-OF-FACTORS))
                (RETURN
                 (MULTIHEN1
                  (LIST RES GROWTH-FACTOR FIRST-TIME BHAT0S B0S VARIABLE-SET
                        SOLVE-COUNT CORRECTION-VECTORS UNKNOWNS-COUNT-LIST
                        BEST-FACTORS V DEGBD SOLN-MATRICES PREDICTED-FORMS
                        POLY-REMAINING PREDICTION-RESULTS
                        ONE-PREDICTION-FAILED)
                  NIL))
               EXIT
                (MULTIHEN-EXIT FIRST-TIME BEST-FACTORS NIL)))
            (PLUS FACTOR-LEVEL 1))))) 
(PUT 'MULTIHEN1 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIHEN1 'DEFINED-ON-LINE '168) 
(PUT 'MULTIHEN1 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'MULTIHEN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIHEN1 (U ZZ)
    (PROG (RES TEST-PREDICTION GROWTH-FACTOR FIRST-TIME HAT0S X0S VARIABLE-SET
           SOLVE-COUNT CORRECTION-VECTORS UNKNOWNS-COUNT-LIST CORRECTION-FACTOR
           FRVEC V DEGBD SOLN-MATRICES PREDICTED-FORMS POLY-REMAINING FVEC
           PREVIOUS-PREDICTION-HOLDS PREDICTION-RESULTS ONE-PREDICTION-FAILED
           BOOL D X1 K KK SUBSTRES W)
      (SETQ RES (CAR U))
      (SETQ U (CDR U))
      (SETQ GROWTH-FACTOR (CAR U))
      (SETQ U (CDR U))
      (SETQ FIRST-TIME (CAR U))
      (SETQ U (CDR U))
      (SETQ HAT0S (CAR U))
      (SETQ U (CDR U))
      (SETQ X0S (CAR U))
      (SETQ U (CDR U))
      (SETQ VARIABLE-SET (CAR U))
      (SETQ U (CDR U))
      (SETQ SOLVE-COUNT (CAR U))
      (SETQ U (CDR U))
      (SETQ CORRECTION-VECTORS (CAR U))
      (SETQ U (CDR U))
      (SETQ UNKNOWNS-COUNT-LIST (CAR U))
      (SETQ U (CDR U))
      (SETQ FRVEC (CAR U))
      (SETQ U (CDR U))
      (SETQ V (CAR U))
      (SETQ U (CDR U))
      (SETQ DEGBD (CAR U))
      (SETQ U (CDR U))
      (SETQ SOLN-MATRICES (CAR U))
      (SETQ U (CDR U))
      (SETQ PREDICTED-FORMS (CAR U))
      (SETQ U (CDR U))
      (SETQ POLY-REMAINING (CAR U))
      (SETQ U (CDR U))
      (SETQ PREDICTION-RESULTS (CAR U))
      (SETQ U (CDR U))
      (COND
       (ZZ
        (PROGN
         (SETQ FVEC (CAR U))
         (SETQ U (CDR U))
         (SETQ PREVIOUS-PREDICTION-HOLDS (CAR U))
         (SETQ U (CDR U)))))
      (SETQ ONE-PREDICTION-FAILED (CAR U))
      (SETQ CORRECTION-FACTOR GROWTH-FACTOR)
      (SETQ X1 (MKVECT NUMBER-OF-FACTORS))
      (SETQ K 1)
      (SETQ KK 0)
     TEMPLOOP
      (SETQ BOOL NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT BOOL) (NOT (NULL RES))
                (OR (NULL MAX-UNKNOWNS) (NULL TEST-PREDICTION))))
          (RETURN NIL)))
        (COND
         ((GREATERP K DEGBD)
          (PROGN
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
                 (PRIN2* "We have overshot the degree bound for ")
                 (PROGN (PRIN2* (CAR V)) (TERPRI* NIL)))
                (WRS STREAM)))))
           (COND
            (*OVERSHOOT
             (PRIN2T "Multivariate degree bound overshoot -> restart")))
           (SETQ BAD-CASE (SETQ BOOL T))))
         ((NULL (SETQ SUBSTRES (EVALUATE-MOD-P RES (CAR V) (CDR V))))
          (PROGN
           (SETQ K (IADD1 K))
           (SETQ RES (DIFF-OVER-K-MOD-P RES K (CAR V)))
           (SETQ CORRECTION-FACTOR
                   (TIMES-MOD-P CORRECTION-FACTOR GROWTH-FACTOR))))
         (T
          (PROG ()
            (MULTIHEN-MSG GROWTH-FACTOR FIRST-TIME K KK SUBSTRES ZZ)
            (COND
             ((NULL ZZ)
              (PROGN
               (SETQ KK (IPLUS2 KK 1))
               (COND (FIRST-TIME (SETQ FIRST-TIME NIL))))))
            (SOLVE-FOR-CORRECTIONS SUBSTRES HAT0S X0S X1 (CDR VARIABLE-SET))
            (COND (BAD-CASE (RETURN (SETQ BOOL T))))
            (COND
             (MAX-UNKNOWNS
              (PROGN
               (SETQ SOLVE-COUNT (IADD1 SOLVE-COUNT))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                 (PUTV (GETV CORRECTION-VECTORS I) SOLVE-COUNT (GETV X1 I))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (COND
                ((EQUAL SOLVE-COUNT (CAAR UNKNOWNS-COUNT-LIST))
                 (SETQ TEST-PREDICTION T))))))
            (COND
             (ZZ
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                (PUTV FRVEC I
                      (PLUS-MOD-P (GETV FRVEC I)
                       (TIMES-MOD-P (GETV X1 I) CORRECTION-FACTOR)))
                (SETQ I (PLUS2 I 1))
                (GO LAB))))
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
                  (PROGN (PRIN2* "   Giving:") (TERPRI* NIL))
                  (COND
                   ((NULL ZZ)
                    (EZGCD_PRINTVEC "     f(" NUMBER-OF-FACTORS ",1) = " X1))
                   (T
                    (PROGN
                     (EZGCD_PRINTVEC "     a(" NUMBER-OF-FACTORS ",1) = " X1)
                     (PROGN (PRIN2* "   New a's are now:") (TERPRI* NIL))
                     (EZGCD_PRINTVEC "     a(" NUMBER-OF-FACTORS ") = "
                                     FRVEC)))))
                 (WRS STREAM)))))
            (SETQ D
                    (TIMES-MOD-P CORRECTION-FACTOR
                     (COND
                      (ZZ
                       (FORM-SUM-AND-PRODUCT-MOD-P X1 FHATVEC
                        NUMBER-OF-FACTORS))
                      (T (TERMS-DONE-MOD-P FRVEC X1 CORRECTION-FACTOR)))))
            (COND
             ((GREATERP (DEGREE-IN-VARIABLE D (CAR V)) DEGBD)
              (PROGN
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
                     (PRIN2* "We have overshot the degree bound for ")
                     (PROGN (PRIN2* (CAR V)) (TERPRI* NIL)))
                    (WRS STREAM)))))
               (COND
                (*OVERSHOOT
                 (PRIN2T "Multivariate degree bound overshoot -> restart")))
               (SETQ BAD-CASE T)
               (RETURN (SETQ BOOL T)))))
            (SETQ D (DIFF-K-TIMES-MOD-P D K (CAR V)))
            (COND
             ((NULL ZZ)
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                (PUTV FRVEC I
                      (PLUS-MOD-P (GETV FRVEC I)
                       (TIMES-MOD-P (GETV X1 I) CORRECTION-FACTOR)))
                (SETQ I (PLUS2 I 1))
                (GO LAB))))
            (SETQ K (IADD1 K))
            (SETQ RES (DIFF-OVER-K-MOD-P (DIFFERENCE-MOD-P RES D) K (CAR V)))
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
                   ((NULL ZZ)
                    (PROGN
                     (PROGN (PRIN2* "   New factors are now:") (TERPRI* NIL))
                     (EZGCD_PRINTVEC "     f(" NUMBER-OF-FACTORS ") = "
                                     FRVEC))))
                  (PRIN2* "   and residue = ")
                  (PRINTSF RES)
                  (PROGN (PRIN2* "-------------") (TERPRI* NIL)))
                 (WRS STREAM)))))
            (SETQ CORRECTION-FACTOR
                    (TIMES-MOD-P CORRECTION-FACTOR GROWTH-FACTOR)))))
        (GO WHILELABEL))
      (COND
       ((AND (NOT (NULL RES)) (NOT BAD-CASE))
        (PROGN
         (COND
          ((OR (NULL ZZ) (NULL SOLN-MATRICES))
           (SETQ SOLN-MATRICES
                   (CONSTRUCT-SOLN-MATRICES PREDICTED-FORMS (CDR V)))))
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
                ((NULL ZZ)
                 (PROGN
                  (PROGN
                   (PRIN2* "We use the results from the Hensel growth to")
                   (TERPRI* NIL))
                  (PROGN
                   (PRIN2* "produce a set of linear equations to solve")
                   (TERPRI* NIL))
                  (PROGN
                   (PRIN2* "for coefficients in the relevant factors:")
                   (TERPRI* NIL))))
                (T
                 (PROGN
                  (PROGN
                   (PRIN2*
                    "The Hensel growth so far allows us to test some of")
                   (TERPRI* NIL))
                  (PROGN (PRIN2* "our predictions:") (TERPRI* NIL))))))
              (WRS STREAM)))))
         (SETQ BOOL NIL)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (NOT BOOL) UNKNOWNS-COUNT-LIST
                   (EQUAL (CAR (SETQ W (CAR UNKNOWNS-COUNT-LIST)))
                          SOLVE-COUNT)))
             (RETURN NIL)))
           (PROGN
            (SETQ UNKNOWNS-COUNT-LIST (CDR UNKNOWNS-COUNT-LIST))
            (PROG (STREAM)
              (COND
               ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                (SETQ STREAM (CONS NIL NIL)))
               (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
              (COND
               (STREAM
                (PROGN
                 (SETQ STREAM (WRS (CDR STREAM)))
                 (PRINT-LINEAR-SYSTEM (CDR W) SOLN-MATRICES CORRECTION-VECTORS
                  PREDICTED-FORMS (CAR V))
                 (WRS STREAM)))))
            (SETQ W
                    (TRY-PREDICTION SOLN-MATRICES CORRECTION-VECTORS
                     PREDICTED-FORMS (CAR W) (CDR W) POLY-REMAINING (CAR V)
                     (COND (ZZ FVEC) (T NIL)) (COND (ZZ FHATVEC) (T NIL))))
            (COND
             ((OR (EQUAL (CAR W) 'SINGULAR) (EQUAL (CAR W) 'BAD-PREDICTION))
              (COND
               (ONE-PREDICTION-FAILED
                (PROGN
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
                       (PRIN2* "Predictions were no help.")
                       (TERPRI* NIL))
                      (WRS STREAM)))))
                 (SETQ MAX-UNKNOWNS NIL)
                 (SETQ BOOL T)))
               ((NULL ZZ) (SETQ ONE-PREDICTION-FAILED (CDR W)))
               (T
                (PROGN
                 (COND
                  (PREVIOUS-PREDICTION-HOLDS
                   (PROGN
                    (SETQ PREDICTIONS (DELASC (CAR V) PREDICTIONS))
                    (SETQ PREVIOUS-PREDICTION-HOLDS NIL))))
                 (SETQ ONE-PREDICTION-FAILED (CDR W))))))
             (T
              (PROGN
               (PUTV PREDICTION-RESULTS (CAR W) (CADR W))
               (SETQ POLY-REMAINING (CADDR W))))))
           (GO WHILELABEL))
         (COND
          ((NULL MAX-UNKNOWNS)
           (PROGN
            (COND
             ((AND ZZ PREVIOUS-PREDICTION-HOLDS)
              (SETQ PREDICTIONS (DELASC (CAR V) PREDICTIONS))))
            (GO TEMPLOOP))))
         (SETQ W (LENGTH UNKNOWNS-COUNT-LIST))
         (COND
          ((OR (GREATERP W 1) (AND (EQUAL W 1) ONE-PREDICTION-FAILED))
           (PROGN (SETQ TEST-PREDICTION NIL) (GO TEMPLOOP))))
         (COND
          ((OR (EQUAL W 1) ONE-PREDICTION-FAILED)
           (PROGN
            (SETQ W
                    (COND (ONE-PREDICTION-FAILED ONE-PREDICTION-FAILED)
                          (T (CDAR UNKNOWNS-COUNT-LIST))))
            (PUTV PREDICTION-RESULTS W
                  (COND ((NULL ZZ) POLY-REMAINING)
                        (T
                         (QUOTFAIL-MOD-P POLY-REMAINING (GETV FHATVEC W))))))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
           (PUTV FRVEC I (GETV PREDICTION-RESULTS I))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((AND (OR (NOT PREVIOUS-PREDICTION-HOLDS) (NULL ZZ))
                (NOT ONE-PREDICTION-FAILED))
           (SETQ PREDICTIONS
                   (CONS
                    (CONS (CAR V)
                          (LIST SOLN-MATRICES PREDICTED-FORMS MAX-UNKNOWNS
                                NUMBER-OF-UNKNOWNS))
                    PREDICTIONS)))))))
      (MULTIHEN-EXIT FIRST-TIME FRVEC ZZ))) 
(PUT 'MULTIHEN-MSG 'NUMBER-OF-ARGS 6) 
(PUT 'MULTIHEN-MSG 'DEFINED-ON-LINE '340) 
(PUT 'MULTIHEN-MSG 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'MULTIHEN-MSG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTIHEN-MSG (GROWTH-FACTOR FIRST-TIME K KK SUBSTRES ZZ)
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
          (PRIN2* "Hensel Step ")
          (PROGN (PRIN2* (SETQ KK (IPLUS2 KK 1))) (TERPRI* NIL))
          (PRIN2* "-------------")
          (COND ((GREATERP KK 10) (PROGN (PRIN2* "-") (TERPRI* NIL)))
                (T (TERPRI* T)))
          (PRIN2* "Next corrections are for (")
          (PRINSF GROWTH-FACTOR)
          (COND ((NOT (EQUAL K 1)) (PROGN (PRIN2* ") ** ") (PRIN2* K)))
                (T (PRIN2* '|)|)))
          (PROGN (PRIN2* ". To find these we solve:") (TERPRI* NIL))
          (COND (ZZ (PRIN2* "     sum over i [ a(i,1)*fhat(i,0) ] = "))
                (T (PRIN2* "     sum over i [ f(i,1)*fhat(i,0) ] = ")))
          (PRINSF SUBSTRES)
          (PRIN2* " mod ")
          (PRIN2* HENSEL-GROWTH-SIZE)
          (COND (ZZ (PROGN (PRIN2* " for a(i,1). ") (TERPRI* NIL)))
                (T (PROGN (PRIN2* " for f(i,1), ") (TERPRI* NIL))))
          (COND
           ((AND (NULL ZZ) FIRST-TIME)
            (PROGN
             (PRIN2* "       where fhat(i,0) = product over j [ f(j,0) ]")
             (PRIN2* " / f(i,0) mod ")
             (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL)))))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(PUT 'MULTIHEN-EXIT 'NUMBER-OF-ARGS 3) 
(PUT 'MULTIHEN-EXIT 'DEFINED-ON-LINE '366) 
(PUT 'MULTIHEN-EXIT 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'MULTIHEN-EXIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTIHEN-EXIT (FIRST-TIME FRVEC ZZ)
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
           ((NOT BAD-CASE)
            (COND
             (FIRST-TIME
              (COND
               (ZZ
                (PROGN
                 (PRIN2* "But these a's are already correct.")
                 (TERPRI* NIL)))
               (T
                (PROGN
                 (PRIN2* "Therefore these factors are already correct.")
                 (TERPRI* NIL)))))
             (T
              (PROGN
               (COND
                (ZZ
                 (PROGN
                  (PROGN (PRIN2* "Correct a's are:") (TERPRI* NIL))
                  (EZGCD_PRINTVEC "  a(" NUMBER-OF-FACTORS ") = " FRVEC)))
                (T
                 (PROGN
                  (PROGN (PRIN2* "Correct factors are:") (TERPRI* NIL))
                  (EZGCD_PRINTVEC "  f(" NUMBER-OF-FACTORS ") = "
                                  FRVEC)))))))))
          (TERPRI* NIL)
          (PROGN
           (PRIN2* "**************************************************")
           (TERPRI* NIL))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(PUT 'FIND-MSG1 'NUMBER-OF-ARGS 3) 
(PUT 'FIND-MSG1 'DEFINED-ON-LINE '381) 
(PUT 'FIND-MSG1 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MSG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-MSG1 (BEST-FACTORS GROWTH-FACTOR POLY)
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
          (PROGN (PRIN2* "Want f(i) s.t.") (TERPRI* NIL))
          (PRIN2* "  product over i [ f(i) ] = ")
          (PRINSF POLY)
          (PRIN2* " mod ")
          (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL))
          (TERPRI* NIL)
          (PROGN (PRIN2* "We know f(i) as follows:") (TERPRI* NIL))
          (EZGCD_PRINTVEC "  f(" NUMBER-OF-FACTORS ") = " BEST-FACTORS)
          (PRIN2* " and we shall put in powers of ")
          (PRINSF GROWTH-FACTOR)
          (PROGN (PRIN2* " to find them fully.") (TERPRI* NIL)))
         (WRS STREAM)))))) 
(PUT 'FIND-MSG2 'NUMBER-OF-ARGS 2) 
(PUT 'FIND-MSG2 'DEFINED-ON-LINE '396) 
(PUT 'FIND-MSG2 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MSG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND-MSG2 (V VARIABLE-SET)
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
          (PRIN2* "First solve the problem in one less variable by putting ")
          (PRIN2* (CAR V))
          (PRIN2* "=")
          (PROGN (PRIN2* (CDR V)) (TERPRI* NIL))
          (COND
           ((CDR VARIABLE-SET)
            (PROGN
             (PRIN2* "and growing wrt ")
             (PROGN (PRIN2* (CAADR VARIABLE-SET)) (TERPRI* NIL)))))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(PUT 'FIND-MSG3 'NUMBER-OF-ARGS 2) 
(PUT 'FIND-MSG3 'DEFINED-ON-LINE '408) 
(PUT 'FIND-MSG3 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MSG3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND-MSG3 (BEST-FACTORS V)
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
          (PRIN2* "After putting back any knowledge of ")
          (PRIN2* (CAR V))
          (PROGN (PRIN2* ", we have the") (TERPRI* NIL))
          (PROGN (PRIN2* "factors so far as:") (TERPRI* NIL))
          (EZGCD_PRINTVEC "  f(" NUMBER-OF-FACTORS ") = " BEST-FACTORS)
          (PROGN
           (PRIN2* "Subtracting the product of these from the polynomial")
           (TERPRI* NIL))
          (PRIN2* " and differentiating wrt ")
          (PRIN2* (CAR V))
          (PROGN (PRIN2* " gives a residue:") (TERPRI* NIL)))
         (WRS STREAM)))))) 
(PUT 'FIND-MSG4 'NUMBER-OF-ARGS 2) 
(PUT 'FIND-MSG4 'DEFINED-ON-LINE '420) 
(PUT 'FIND-MSG4 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MSG4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND-MSG4 (PREDICTED-FORMS V)
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
           (PRIN2* "To help reduce the number of Hensel steps we try")
           (TERPRI* NIL))
          (PRIN2* " predicting how many terms each factor will have wrt ")
          (PRIN2* (CAR V))
          (PROGN (PRIN2* ".") (TERPRI* NIL))
          (PROGN
           (PRIN2* "Predictions are based on the bivariate factors :")
           (TERPRI* NIL))
          (EZGCD_PRINTVEC "     f(" NUMBER-OF-FACTORS ") = " PREDICTED-FORMS))
         (WRS STREAM)))))) 
(PUT 'FIND-MSG5 'NUMBER-OF-ARGS 0) 
(PUT 'FIND-MSG5 'DEFINED-ON-LINE '430) 
(PUT 'FIND-MSG5 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'FIND-MSG5 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIND-MSG5 NIL
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
          (TERPRI* NIL)
          (PROGN (PRIN2* "We predict :") (TERPRI* NIL))
          (PROG (W)
            (SETQ W NUMBER-OF-UNKNOWNS)
           LAB
            (COND ((NULL W) (RETURN NIL)))
            ((LAMBDA (W)
               (PROGN
                (PRIN2* (CAR W))
                (PRIN2* " terms in f(")
                (PRIN2* (CDR W))
                (PROGN (PRIN2* '|)|) (TERPRI* NIL))))
             (CAR W))
            (SETQ W (CDR W))
            (GO LAB))
          (COND
           ((EQUAL (CAAR NUMBER-OF-UNKNOWNS) 1)
            (PROGN
             (PRIN2* "Since we predict only one term for f(")
             (PRIN2* (CDAR NUMBER-OF-UNKNOWNS))
             (PROGN (PRIN2* "), we can try") (TERPRI* NIL))
             (PROGN (PRIN2* "dividing it out now:") (TERPRI* NIL))))
           (T
            (PROGN
             (PRIN2* "So we shall do at least ")
             (PRIN2* (ISUB1 (CAAR NUMBER-OF-UNKNOWNS)))
             (PRIN2* " Hensel step")
             (COND
              ((EQUAL (CAAR NUMBER-OF-UNKNOWNS) 2)
               (PROGN (PRIN2* ".") (TERPRI* NIL)))
              (T (PROGN (PRIN2* "s.") (TERPRI* NIL)))))))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(PUT 'SOLVE-FOR-CORRECTIONS 'NUMBER-OF-ARGS 5) 
(PUT 'SOLVE-FOR-CORRECTIONS 'DEFINED-ON-LINE '450) 
(PUT 'SOLVE-FOR-CORRECTIONS 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'SOLVE-FOR-CORRECTIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-FOR-CORRECTIONS (C FHATVEC FVEC RESVEC VSET)
    (COND
     ((NULL VSET)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PUTV RESVEC I
              (REMAINDER-MOD-P (TIMES-MOD-P C (GETV ALPHAVEC I))
               (GETV FVEC I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB)))
     (T
      ((LAMBDA (FACTOR-LEVEL)
         (PROG (RESIDUE GROWTH-FACTOR F0S FHAT0S V DEGBD FIRST-TIME REDC
                PREDICTED-FORMS MAX-UNKNOWNS SOLVE-COUNT NUMBER-OF-UNKNOWNS
                CORRECTION-VECTORS SOLN-MATRICES W PREVIOUS-PREDICTION-HOLDS
                UNKNOWNS-COUNT-LIST POLY-REMAINING PREDICTION-RESULTS
                ONE-PREDICTION-FAILED)
           (SETQ V (CAR VSET))
           (SETQ DEGBD (GET-DEGREE-BOUND (CAR V)))
           (SETQ FIRST-TIME T)
           (SETQ GROWTH-FACTOR (MAKE-GROWTH-FACTOR V))
           (SETQ POLY-REMAINING C)
           (SETQ PREDICTION-RESULTS (MKVECT NUMBER-OF-FACTORS))
           (SETQ REDC (EVALUATE-MOD-P C (CAR V) (CDR V)))
           (SOLVE-MSG1 C FVEC V)
           (SOLVE-FOR-CORRECTIONS REDC
            (SETQ FHAT0S
                    (REDUCE-VEC-BY-ONE-VAR-MOD-P FHATVEC V NUMBER-OF-FACTORS))
            (SETQ F0S (REDUCE-VEC-BY-ONE-VAR-MOD-P FVEC V NUMBER-OF-FACTORS))
            RESVEC (CDR VSET))
           (COND (BAD-CASE (RETURN NIL)))
           (SOLVE-MSG2 RESVEC V)
           (SETQ RESIDUE
                   (DIFF-OVER-K-MOD-P
                    (DIFFERENCE-MOD-P C
                     (FORM-SUM-AND-PRODUCT-MOD-P RESVEC FHATVEC
                      NUMBER-OF-FACTORS))
                    1 (CAR V)))
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
                 (PRINTSF RESIDUE)
                 (PRIN2* " Now we shall put in the powers of ")
                 (PRINSF GROWTH-FACTOR)
                 (PROGN (PRIN2* " to find the a's fully.") (TERPRI* NIL)))
                (WRS STREAM)))))
           (COND
            ((AND (NOT (NULL RESIDUE)) (NOT (ZEROP (CDR V))))
             (PROGN
              (SETQ W (ATSOC (CAR V) PREDICTIONS))
              (COND
               (W
                (PROGN
                 (SETQ PREVIOUS-PREDICTION-HOLDS T)
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
                         "We shall use the previous prediction for the form of")
                        (TERPRI* NIL))
                       (PRIN2* "polynomials wrt ")
                       (PROGN (PRIN2* (CAR V)) (TERPRI* NIL)))
                      (WRS STREAM)))))
                 (SETQ W (CDR W))
                 (SETQ SOLN-MATRICES (CAR W))
                 (SETQ PREDICTED-FORMS (CADR W))
                 (SETQ MAX-UNKNOWNS (CADDR W))
                 (SETQ NUMBER-OF-UNKNOWNS (CADR (CDDR W)))))
               (T
                (PROGN
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
                         "We shall use a new prediction for the form of polynomials ")
                        (TERPRI* NIL))
                       (PRIN2* "wrt ")
                       (PROGN (PRIN2* (CAR V)) (TERPRI* NIL)))
                      (WRS STREAM)))))
                 (SETQ PREDICTED-FORMS (MKVECT NUMBER-OF-FACTORS))
                 (PROG (I)
                   (SETQ I 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
                   (PUTV PREDICTED-FORMS I (GETV FVEC I))
                   (SETQ I (PLUS2 I 1))
                   (GO LAB))
                 (MAKE-PREDICTED-FORMS PREDICTED-FORMS (CAR V))
                 NIL)))
              (SOLVE-MSG3)
              (SETQ UNKNOWNS-COUNT-LIST NUMBER-OF-UNKNOWNS)
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND UNKNOWNS-COUNT-LIST
                        (EQUAL (CAR (SETQ W (CAR UNKNOWNS-COUNT-LIST))) 1)))
                  (RETURN NIL)))
                (PROG (I R WR FI)
                  (SETQ UNKNOWNS-COUNT-LIST (CDR UNKNOWNS-COUNT-LIST))
                  (SETQ I (CDR W))
                  (SETQ W
                          (QUOTIENT-MOD-P
                           (SETQ WR
                                   (DIFFERENCE-MOD-P POLY-REMAINING
                                    (TIMES-MOD-P (SETQ R (GETV RESVEC I))
                                     (GETV FHATVEC I))))
                           (SETQ FI (GETV FVEC I))))
                  (COND
                   ((OR (NULL W)
                        (NOT (NULL (DIFFERENCE-MOD-P WR (TIMES-MOD-P W FI)))))
                    (COND
                     (ONE-PREDICTION-FAILED
                      (PROGN
                       (PROG (STREAM)
                         (COND
                          ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                           (SETQ STREAM (CONS NIL NIL)))
                          (T
                           (SETQ STREAM
                                   (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                         (COND
                          (STREAM
                           (PROGN
                            (SETQ STREAM (WRS (CDR STREAM)))
                            (PROGN
                             (PRIN2* "Predictions are no good.")
                             (TERPRI* NIL))
                            (WRS STREAM)))))
                       (SETQ MAX-UNKNOWNS NIL)))
                     (T
                      (PROGN
                       (PROG (STREAM)
                         (COND
                          ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                           (SETQ STREAM (CONS NIL NIL)))
                          (T
                           (SETQ STREAM
                                   (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                         (COND
                          (STREAM
                           (PROGN
                            (SETQ STREAM (WRS (CDR STREAM)))
                            (PROGN
                             (PRIN2* "Guess for a(")
                             (PRIN2* I)
                             (PROGN (PRIN2* ") was bad.") (TERPRI* NIL)))
                            (WRS STREAM)))))
                       (SETQ ONE-PREDICTION-FAILED I)))))
                   (T
                    (PROGN
                     (PUTV PREDICTION-RESULTS I R)
                     (PROG (STREAM)
                       (COND
                        ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                         (SETQ STREAM (CONS NIL NIL)))
                        (T
                         (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
                       (COND
                        (STREAM
                         (PROGN
                          (SETQ STREAM (WRS (CDR STREAM)))
                          (PROGN
                           (PRIN2* "Prediction for a(")
                           (PRIN2* I)
                           (PRIN2* ") worked: ")
                           (PRINTSF R))
                          (WRS STREAM)))))
                     (SETQ POLY-REMAINING WR)))))
                (GO WHILELABEL))
              (SETQ W (LENGTH UNKNOWNS-COUNT-LIST))
              (COND
               ((AND (EQUAL W 1) (NOT ONE-PREDICTION-FAILED))
                (PROGN
                 (PUTV RESVEC (CDAR UNKNOWNS-COUNT-LIST)
                       (QUOTFAIL-MOD-P POLY-REMAINING
                        (GETV FHATVEC (CDAR UNKNOWNS-COUNT-LIST))))
                 (GO EXIT)))
               ((AND (EQUAL W 0) ONE-PREDICTION-FAILED MAX-UNKNOWNS)
                (PROGN
                 (PUTV RESVEC ONE-PREDICTION-FAILED
                       (QUOTFAIL-MOD-P POLY-REMAINING
                        (GETV FHATVEC ONE-PREDICTION-FAILED)))
                 (GO EXIT))))
              (SETQ SOLVE-COUNT 1)
              (COND
               (MAX-UNKNOWNS
                (SETQ CORRECTION-VECTORS
                        (MAKE-CORRECTION-VECTORS RESVEC MAX-UNKNOWNS)))))))
           (COND ((NOT (NULL RESIDUE)) (SETQ FIRST-TIME NIL)))
           (RETURN
            (MULTIHEN1
             (LIST RESIDUE GROWTH-FACTOR FIRST-TIME FHAT0S F0S VSET SOLVE-COUNT
                   CORRECTION-VECTORS UNKNOWNS-COUNT-LIST RESVEC V DEGBD
                   SOLN-MATRICES PREDICTED-FORMS POLY-REMAINING
                   PREDICTION-RESULTS FVEC PREVIOUS-PREDICTION-HOLDS
                   ONE-PREDICTION-FAILED)
             T))
          EXIT
           (MULTIHEN-EXIT FIRST-TIME RESVEC T)))
       (PLUS FACTOR-LEVEL 1))))) 
(PUT 'SOLVE-MSG1 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVE-MSG1 'DEFINED-ON-LINE '586) 
(PUT 'SOLVE-MSG1 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'SOLVE-MSG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-MSG1 (C FVEC V)
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
          (PROGN (PRIN2* "Want a(i) s.t.") (TERPRI* NIL))
          (PRIN2* "(*)  sum over i [ a(i)*fhat(i) ] = ")
          (PRINSF C)
          (PRIN2* " mod ")
          (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL))
          (PRIN2* "    where fhat(i) = product over j [ f(j) ]")
          (PRIN2* " / f(i) mod ")
          (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL))
          (PROGN (PRIN2* "    and") (TERPRI* NIL))
          (EZGCD_PRINTVEC "      f(" NUMBER-OF-FACTORS ") = " FVEC)
          (TERPRI* NIL)
          (PRIN2* "First solve the problem in one less variable by putting ")
          (PRIN2* (CAR V))
          (PRIN2* '=)
          (PROGN (PRIN2* (CDR V)) (TERPRI* NIL))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(PUT 'SOLVE-MSG2 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-MSG2 'DEFINED-ON-LINE '605) 
(PUT 'SOLVE-MSG2 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'SOLVE-MSG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-MSG2 (RESVEC V)
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
          (PROGN (PRIN2* "Giving:") (TERPRI* NIL))
          (EZGCD_PRINTVEC "  a(" NUMBER-OF-FACTORS ",0) = " RESVEC)
          (PROGN
           (PRIN2* "Subtracting the contributions these give in (*) from")
           (TERPRI* NIL))
          (PRIN2* "the R.H.S. of (*) ")
          (PRIN2* "and differentiating wrt ")
          (PRIN2* (CAR V))
          (PROGN (PRIN2* " gives a residue:") (TERPRI* NIL)))
         (WRS STREAM)))))) 
(PUT 'SOLVE-MSG3 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVE-MSG3 'DEFINED-ON-LINE '615) 
(PUT 'SOLVE-MSG3 'DEFINED-IN-FILE 'FACTOR/MULTIHEN.RED) 
(PUT 'SOLVE-MSG3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVE-MSG3 NIL
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
          (TERPRI* NIL)
          (PROGN (PRIN2* "We predict :") (TERPRI* NIL))
          (PROG (W)
            (SETQ W NUMBER-OF-UNKNOWNS)
           LAB
            (COND ((NULL W) (RETURN NIL)))
            ((LAMBDA (W)
               (PROGN
                (PRIN2* (CAR W))
                (PRIN2* " terms in a(")
                (PRIN2* (CDR W))
                (PROGN (PRIN2* '|)|) (TERPRI* NIL))))
             (CAR W))
            (SETQ W (CDR W))
            (GO LAB))
          (COND
           ((EQUAL (CAAR NUMBER-OF-UNKNOWNS) 1)
            (PROGN
             (PRIN2* "Since we predict only one term for a(")
             (PRIN2* (CDAR NUMBER-OF-UNKNOWNS))
             (PROGN (PRIN2* "), we can test it right away:") (TERPRI* NIL))))
           (T
            (PROGN
             (PRIN2* "So we shall do at least ")
             (PRIN2* (ISUB1 (CAAR NUMBER-OF-UNKNOWNS)))
             (PRIN2* " Hensel step")
             (COND
              ((EQUAL (CAAR NUMBER-OF-UNKNOWNS) 2)
               (PROGN (PRIN2* ".") (TERPRI* NIL)))
              (T (PROGN (PRIN2* "s.") (TERPRI* NIL)))))))
          (TERPRI* NIL))
         (WRS STREAM)))))) 
(ENDMODULE) 