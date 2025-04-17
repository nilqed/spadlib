(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINT0)) 
(GLOBAL '(UNKNOWN_TST PRODUCT_TST TRANSFORM_TST TRANSFORM_LST)) 
(SETQ TRANSFORM_LST 'NIL) 
(FLUID '(*PRECISE *TRDEFINT)) 
(GLOBAL '(SPEC_COND)) 
(SWITCH (LIST 'TRDEFINT)) 
(PUT 'INTGGGG 'SIMPFN 'SIMPINTGGGG) 
(PUT 'NEW_DEFINT 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_DEFINT 'DEFINED-ON-LINE '90) 
(PUT 'NEW_DEFINT 'DEFINED-IN-FILE 'DEFINT/DEFINT0.RED) 
(PUT 'NEW_DEFINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_DEFINT (LST)
    (PROG (NN DD X Y NCOEF DCOEF COEFF VAR VARPOW VAREXP MATCHFORM RESULT N1 N2
           N3 *PRECISE)
      (SETQ X (SIMP* (CAR LST)))
      (SETQ VAR (CADR LST))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Entering new_defint with integrand")
         (PRINTSQ X)
         (PRIN2* "w.r.t. variable >")
         (MAPRIN VAR)
         (PRIN2* "<")
         (TERPRI* T)
         NIL)))
      (SETQ NN (FCTRF (CAR X)))
      (SETQ DD (FCTRF (CDR X)))
      (SETQ NCOEF (CAR NN))
      (SETQ DCOEF (CAR DD))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "After factorization, the numerator has factors")
         (TERPRI* T)
         (COND ((NEQ NCOEF 1) (PROGN (PRINTSF NCOEF) (TERPRI* T) NIL)))
         (PROG (FCTR)
           (SETQ FCTR (CDR NN))
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR)
              (PROGN (PRINTSF (MKSP* (CAR FCTR) (CDR FCTR))) (TERPRI* T)))
            (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         (PRIN2T "and the denominator has factors")
         (TERPRI* T)
         (COND ((NEQ DCOEF 1) (PROGN (PRINTSF DCOEF) (TERPRI* T) NIL)))
         (PROG (FCTR)
           (SETQ FCTR (CDR DD))
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR)
              (PROGN (PRINTSF (MKSP* (CAR FCTR) (CDR FCTR))) (TERPRI* T)))
            (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         NIL)))
      (SETQ VARPOW (CONS 1 1))
      (SETQ VAREXP (CONS NIL 1))
      (PROG (FFF)
        (SETQ FFF (CDR NN))
       LAB
        (COND ((NULL FFF) (RETURN NIL)))
        ((LAMBDA (FFF)
           (PROGN
            (COND
             ((NOT (DEPENDS (CAR FFF) VAR))
              (SETQ NCOEF
                      ((LAMBDA (G138)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF NCOEF G138))
                               (T (POLY-MULTF NCOEF G138))))
                       (COND ((EQUAL (CDR FFF) 1) (CAR FFF))
                             (T (MKSP* (CAR FFF) (CDR FFF)))))))
             ((AND (NULL (CDR (CAR FFF))) (EQUAL (CDAR (CAR FFF)) 1)
                   (EQUAL (CDAAR (CAR FFF)) 1)
                   (OR (EQUAL (CAAAR (CAR FFF)) VAR)
                       (EQUAL (CAAAR (CAR FFF)) (LIST 'SQRT VAR))
                       (AND (EQCAR (CAAAR (CAR FFF)) 'EXPT)
                            (EQUAL (CADR (CAAAR (CAR FFF))) VAR)
                            (NOT (DEPENDS (CADDR (CAAAR (CAR FFF))) VAR)))))
              (PROGN
               (SETQ VARPOW (MULTSQ VARPOW (MKSQ (CAR FFF) (CDR FFF))))
               (COND
                ((EQUAL (CAAAR (CAR FFF)) VAR)
                 (SETQ VAREXP (ADDSQ VAREXP (CONS (CDR FFF) 1))))
                ((EQUAL (CAR FFF) (LIST 'SQRT VAR))
                 (SETQ VAREXP
                         (ADDSQ VAREXP
                                (COND
                                 ((EVENP (CDR FFF))
                                  (CONS (QUOTIENT (CDR FFF) 2) 1))
                                 (T (CONS (CDR FFF) 2))))))
                (T
                 (SETQ VAREXP
                         (ADDSQ VAREXP
                                (SIMP*
                                 (LIST 'TIMES (CADDR (CAAAR (CAR FFF)))
                                       (CDR FFF)))))))))
             (T
              (SETQ Y
                      (CONS
                       (COND ((EQUAL (CDR FFF) 1) (PREPF (CAR FFF)))
                             (T (PREPF (MKSP* (CAR FFF) (CDR FFF)))))
                       Y))))))
         (CAR FFF))
        (SETQ FFF (CDR FFF))
        (GO LAB))
      (PROG (FFF)
        (SETQ FFF (CDR DD))
       LAB
        (COND ((NULL FFF) (RETURN NIL)))
        ((LAMBDA (FFF)
           (COND
            ((NOT (DEPENDS (CAR FFF) VAR))
             (SETQ DCOEF
                     ((LAMBDA (G140)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF DCOEF G140))
                              (T (POLY-MULTF DCOEF G140))))
                      (COND ((EQUAL (CDR FFF) 1) (CAR FFF))
                            (T (MKSP* (CAR FFF) (CDR FFF)))))))
            ((AND (NULL (CDR (CAR FFF))) (EQUAL (CDAR (CAR FFF)) 1)
                  (EQUAL (CDAAR (CAR FFF)) 1)
                  (OR (EQUAL (CAAAR (CAR FFF)) VAR)
                      (EQUAL (CAAAR (CAR FFF)) (LIST 'SQRT VAR))
                      (AND (EQCAR (CAAAR (CAR FFF)) 'EXPT)
                           (EQUAL (CADR (CAAAR (CAR FFF))) VAR)
                           (NOT (DEPENDS (CADDR (CAAAR (CAR FFF))) VAR)))))
             (PROGN
              (SETQ VARPOW (MULTSQ VARPOW (MKSQ (CAR FFF) (MINUS (CDR FFF)))))
              (COND
               ((EQUAL (CAAAR (CAR FFF)) VAR)
                (SETQ VAREXP (ADDSQ VAREXP (NEGSQ (CONS (CDR FFF) 1)))))
               ((EQUAL (CAAAR (CAR FFF)) (LIST 'SQRT VAR))
                (SETQ VAREXP
                        (ADDSQ VAREXP
                               (NEGSQ
                                (COND
                                 ((EVENP (CDR FFF))
                                  (CONS (QUOTIENT (CDR FFF) 2) 1))
                                 (T (CONS (CDR FFF) 2)))))))
               (T
                (SETQ VAREXP
                        (ADDSQ VAREXP
                               (NEGSQ
                                (SIMP*
                                 (LIST 'TIMES (CADDR (CAAAR (CAR FFF)))
                                       (CDR FFF))))))))))
            (T
             (SETQ Y (CONS (PREPSQ (CONS 1 (MKSP* (CAR FFF) (CDR FFF)))) Y)))))
         (CAR FFF))
        (SETQ FFF (CDR FFF))
        (GO LAB))
      (SETQ COEFF (MK*SQ (MULTSQ (CONS NCOEF 1) (INVSQ (CONS DCOEF 1)))))
      (SETQ Y (REVERSIP Y))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "After separating the factors into classes, we have the following factors:")
         (COND
          ((NEQ COEFF '(1 . 1))
           (PROGN
            (TERPRI* T)
            (PRIN2T "Constant w.r.t. integration variable:")
            (MATHPRINT COEFF)
            NIL)))
         (COND
          ((NEQ VARPOW '(1 . 1))
           (PROGN
            (PRIN2T "Powers of integration variable:")
            (PRINTSQ VARPOW)
            NIL)))
         (PROG (FCTR)
           (SETQ FCTR Y)
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR) (MATHPRINT FCTR)) (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         NIL)))
      (COND
       ((NULL Y)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "The integrand is constant or a power of the integration variable:")
            (PRIN2T "Integral is divergent!")
            NIL)))
         (RETURN 'UNKNOWN)
         NIL)))
      (COND ((GREATERP (LENGTH Y) 2) (SETQ Y (LIST (REVAL1 (RETIMES Y) T)))))
      (COND ((NEQ VARPOW '(1 . 1)) (SETQ Y (CONS (PREPSQ VARPOW) Y))))
      (SETQ LST (NCONC Y (CDR LST)))
      (SETQ UNKNOWN_TST NIL)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (PROG (I)
        (SETQ I LST)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I) (SPECFN_TEST I)) (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND
       ((EQUAL (LENGTH LST) 4)
        (PROGN
         (SETQ N1 (CAR LST))
         (SETQ N2 (CADR LST))
         (SETQ N3 (CADDR LST))
         (SETQ MATCHFORM (LIST 'DEFINT2 N1 N2 N3 VAR))
         NIL))
       ((EQUAL (LENGTH LST) 3)
        (PROGN
         (SETQ N1 (CAR LST))
         (SETQ N2 (CADR LST))
         (SETQ MATCHFORM (LIST 'DEFINT2 N1 N2 VAR))))
       ((EQUAL (LENGTH LST) 2)
        (PROGN (SETQ N1 (CAR LST)) (SETQ MATCHFORM (LIST 'DEFINT2 N1 VAR))))
       ((EQUAL (LENGTH LST) 1) (SETQ MATCHFORM (LIST 'DEFINT2 1 VAR)))
       (T (RETURN 'UNKNOWN)))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Expression to pass to algebraic simplifier is:")
         (MATHPRINT MATCHFORM)
         NIL)))
      (SETQ RESULT (REVAL1 MATCHFORM T))
      (COND
       (*TRDEFINT
        (PROGN (PRIN2T "Expression returned is:") (MATHPRINT RESULT) NIL)))
      (SETK 'TRANSFORM_TST (AEVAL 'NIL))
      (COND
       ((SMEMQ 'DEFINT2 RESULT)
        (PROGN
         (COND
          (*TRDEFINT (PROGN (PRIN2* "Pattern match failed!") (TERPRI* T) NIL)))
         (RETURN 'UNKNOWN)
         NIL))
       ((SMEMQ 'UNKNOWN RESULT)
        (PROGN
         (COND (*TRDEFINT (PROGN (PRIN2* "Method failed") (TERPRI* T) NIL)))
         (RETURN 'UNKNOWN)
         NIL)))
      (SETQ RESULT (REVAL1 (LIST 'TIMES COEFF RESULT) T))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "After multiplying with the coefficient we return:")
         (MATHPRINT RESULT)
         NIL)))
      (RETURN RESULT))) 
(PUT 'SPECFN_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'SPECFN_TEST 'DEFINED-ON-LINE '251) 
(PUT 'SPECFN_TEST 'DEFINED-IN-FILE 'DEFINT/DEFINT0.RED) 
(PUT 'SPECFN_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPECFN_TEST (N)
    (PROG ()
      (COND
       ((AND (LISTP N) (EQUAL (CAR N) 'TIMES))
        (PROGN
         (COND
          ((AND (LISTP (CADDR N))
                (OR (EQUAL (CAR (CADDR N)) 'M_GEGENBAUERP)
                    (EQUAL (CAR (CADDR N)) 'M_JACOBIP)))
           (OFF (LIST 'EXP))))
         NIL))))) 
(PUT 'TEST_PROD 'NUMBER-OF-ARGS 2) 
(PUT 'TEST_PROD 'DEFINED-ON-LINE '260) 
(PUT 'TEST_PROD 'DEFINED-IN-FILE 'DEFINT/DEFINT0.RED) 
(PUT 'TEST_PROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TEST_PROD (LST VAR)
    (PROG (TEMP LS)
      (SETQ TEMP (CAAR LST))
      (COND
       ((EQUAL TEMP 'TIMES)
        (PROGN
         (COND
          ((LISTP (CADDAR LST))
           (PROGN
            (COND
             ((AND (NEQ (CAR (CADDAR LST)) 'M_CHEBYSHEVT)
                   (NEQ (CAR (CADDAR LST)) 'M_CHEBYSHEVU)
                   (NEQ (CAR (CADDAR LST)) 'M_GEGENBAUERP)
                   (NEQ (CAR (CADDAR LST)) 'M_JACOBIP))
              (SETQ LS (APPEND (CDAR LST) (LIST VAR))))
             (T (SETQ LS LST)))
            NIL))
          (T (SETQ LS (APPEND (CDAR LST) (LIST VAR)))))
         NIL))
       ((AND (EQUAL TEMP 'MINUS) (EQUAL (CAADAR LST) 'TIMES))
        (PROGN
         (COND
          ((EQUAL (LENGTH (CADAR LST)) 3)
           (SETQ LS
                   (LIST (LIST 'MINUS (CAR (CDADAR LST))) (CADR (CDADAR LST))
                         VAR)))
          ((EQUAL (LENGTH (CADAR LST)) 4)
           (SETQ LS
                   (LIST (LIST 'MINUS (CAR (CDADAR LST))) (CADR (CDADAR LST))
                         (CADDR (CDADAR LST)) VAR))))))
       (T (SETQ LS LST)))
      (RETURN LS))) 
(SETK 'HEAVISIDE_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'HEAVISIDE (LIST '~ 'X))
                   (LIST 'WHEN 1
                         (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'GEQ 'X 0))))
             (LIST 'REPLACEBY (LIST 'HEAVISIDE (LIST '~ 'X))
                   (LIST 'WHEN 0
                         (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'LESSP 'X 0))))))) 
(AEVAL (LET '(HEAVISIDE_RULES))) 
(AEVAL (OPERATOR (LIST 'DEFINT2 'DEFINT_CHOOSE))) 
(AEVAL (SHARE (LIST 'MELLINCOEF))) 
(SETK 'DEFINT2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'DIFFERENCE
                               (LIST 'COS
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'A)))
                                           (LIST '~ (LIST '~ 'C))))
                               (LIST 'COS
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ (LIST '~ 'D)))))
                         (LIST '~ 'X))
                   (LIST 'MINUS
                         (LIST 'TIMES 2
                               (LIST 'DEFINT2
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'QUOTIENT 'A 'C)
                                                       (LIST 'QUOTIENT 'B 'D))
                                                 (LIST 'QUOTIENT 'X 2)))
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'QUOTIENT 'A 'C)
                                                       (LIST 'QUOTIENT 'B 'D))
                                                 (LIST 'QUOTIENT 'X 2)))
                                     'X))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'QUOTIENT (LIST '~ 'B) (LIST '~ 'F1))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'B
                               (LIST 'DEFINT2 (LIST 'QUOTIENT 1 'F1) 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'NOT (LIST 'EQUAL 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST '~ (LIST '~ 'B)) (LIST '~ 'F1))
                         (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES 'B (LIST 'DEFINT2 'F1 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'NOT (LIST 'EQUAL 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT (LIST '~ 'F1) (LIST '~ (LIST '~ 'B)))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'B)
                               (LIST 'DEFINT2 'F1 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'NOT (LIST 'EQUAL 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F2)
                                     (LIST '~ (LIST '~ 'F1)))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST '|DEFINT:ADDX|
                               (LIST 'DEFINT2 (LIST 'QUOTIENT 'F2 'F3) 'X)
                               (LIST 'DEFINT2 (LIST 'QUOTIENT 'F1 'F3) 'X))
                         (LIST 'NOT (LIST 'EQUAL 'F1 0))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'MINUS (LIST '~ 'F1)) (LIST '~ 'X))
                   (LIST 'MINUS (LIST 'DEFINT2 'F1 'X)))
             (LIST 'REPLACEBY (LIST 'DEFINT2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0 0 'X))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'DIFFERENCE
                               (LIST 'COS
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'A)))
                                           (LIST '~ (LIST '~ 'C))))
                               (LIST 'COS
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ (LIST '~ 'D)))))
                         (LIST '~ 'X))
                   (LIST 'MINUS
                         (LIST 'TIMES 2
                               (LIST 'DEFINT2 'N
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'PLUS
                                                       (LIST 'QUOTIENT 'A 'C)
                                                       (LIST 'QUOTIENT 'B 'D))
                                                 (LIST 'QUOTIENT 'X 2)))
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'QUOTIENT 'A 'C)
                                                       (LIST 'QUOTIENT 'B 'D))
                                                 (LIST 'QUOTIENT 'X 2)))
                                     'X))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST '~ (LIST '~ 'B)) (LIST '~ 'F1))
                         (LIST 'TIMES (LIST '~ (LIST '~ 'C)) (LIST '~ 'F2))
                         (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES 'B 'C (LIST 'DEFINT2 'F1 'F2 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'QUOTIENT (LIST '~ 'B) (LIST '~ 'F1))
                         (LIST 'QUOTIENT (LIST '~ 'C) (LIST '~ 'F2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'C 'B
                               (LIST 'DEFINT2 (LIST 'QUOTIENT 1 'F1)
                                     (LIST 'QUOTIENT 1 'F2) 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST '~ (LIST '~ 'B)) (LIST '~ 'F1))
                         (LIST 'QUOTIENT (LIST '~ 'C) (LIST '~ 'F2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'C 'B
                               (LIST 'DEFINT2 'F1 (LIST 'QUOTIENT 1 'F2) 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'QUOTIENT (LIST '~ 'B) (LIST '~ 'F1))
                         (LIST 'TIMES (LIST '~ (LIST '~ 'C)) (LIST '~ 'F2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'C 'B
                               (LIST 'DEFINT2 (LIST 'QUOTIENT 1 'F1) 'F2 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT (LIST '~ 'F1) (LIST '~ (LIST '~ 'B)))
                         (LIST 'TIMES (LIST '~ (LIST '~ 'C)) (LIST '~ 'F2))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'C 'B)
                               (LIST 'DEFINT2 'F1 'F2 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X) (LIST 'FREEOF 'C 'X)
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'EQUAL 'B 1)
                                           (LIST 'EQUAL 'C 1))))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F2) (LIST '~ 'F1))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'N) (LIST '~ 'X))
                   (LIST '|DEFINT:ADDX|
                         (LIST 'DEFINT2 (LIST 'QUOTIENT 'F2 'F3) 'N 'X)
                         (LIST 'DEFINT2 (LIST 'QUOTIENT 'F1 'F3) 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'MINUS (LIST '~ 'F1)) (LIST '~ 'N)
                         (LIST '~ 'X))
                   (LIST 'MINUS (LIST 'DEFINT2 'F1 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F2) (LIST '~ 'F1))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'X))
                   (LIST '|DEFINT:ADDX|
                         (LIST 'DEFINT2 'N (LIST 'QUOTIENT 'F2 'F3) 'X)
                         (LIST 'DEFINT2 'N (LIST 'QUOTIENT 'F1 'F3) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST 'MINUS (LIST '~ 'F1))
                         (LIST '~ 'X))
                   (LIST 'MINUS (LIST 'DEFINT2 'N 'F1 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X)
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                               (LIST 'MINUS 'A) 'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                         (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 'X))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'A)))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                               (LIST 'DIFFERENCE
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 'A)
                               'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0 'A 'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                         (LIST 'QUOTIENT 1 2) 'X))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X)
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                               (LIST 'PLUS (LIST 'QUOTIENT 1 2) 'A) 'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES 'B (LIST 'DEFINT2 'F1 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                         (LIST 'DEFINT_CHOOSE 'F2 'X) 0 'X))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0 0
                                     'X))
                         (LIST 'NUMBERP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE (LIST '~ 'F1) (LIST '~ 'F2))
                               (LIST '~ 'F3))
                         (LIST '~ 'F4) (LIST '~ 'X))
                   (LIST '|DEFINT:SUBTRACT|
                         (LIST 'DEFINT2 (LIST 'QUOTIENT 'F1 'F3) 'F4 'X)
                         (LIST 'DEFINT2 (LIST 'QUOTIENT 'F2 'F3) 'F4 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'F2)
                         (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES 'B (LIST 'DEFINT2 'F1 'F2 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F2) (LIST '~ 'F1))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'NN) (LIST '~ 'X))
                   (LIST '|DEFINT:ADDX|
                         (LIST 'DEFINT2 'N (LIST 'QUOTIENT 'F2 'F3) 'NN 'X)
                         (LIST 'DEFINT2 'N (LIST 'QUOTIENT 'F1 'F3) 'NN 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST 'MINUS (LIST '~ 'F1))
                         (LIST '~ 'NN) (LIST '~ 'X))
                   (LIST 'MINUS (LIST 'DEFINT2 'N 'F1 'NN 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST '~ 'NN)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'F2) (LIST '~ 'F1))
                               (LIST '~ (LIST '~ 'F3)))
                         (LIST '~ 'X))
                   (LIST '|DEFINT:ADDX|
                         (LIST 'DEFINT2 'N 'NN (LIST 'QUOTIENT 'F2 'F3) 'X)
                         (LIST 'DEFINT2 'N 'NN (LIST 'QUOTIENT 'F1 'F3) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST '~ 'NN)
                         (LIST 'MINUS (LIST '~ 'F1)) (LIST '~ 'X))
                   (LIST 'MINUS (LIST 'DEFINT2 'N 'NN 'F1 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (MINUS 1) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'MINUS 'A) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'MINUS (LIST 'QUOTIENT 3 2)) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'DIFFERENCE
                                           (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                                           'A)
                                     'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0 'A
                                     'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST '~ 'X) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0 1
                                     'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N) (LIST 'SQRT (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'QUOTIENT 1 2) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'QUOTIENT 3 2) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST '~ 'N)
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES 'N
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X) 0
                                     (LIST 'PLUS (LIST 'QUOTIENT 1 2) 'A) 'X))
                         (LIST 'FREEOF 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'A)))
                                     (LIST '~ (LIST '~ 'C))))
                         (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'B 'C)
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                                     (LIST 'DEFINT_CHOOSE 'F2 'X) 'A 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT (LIST '~ 'B)
                               (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'A)))))
                         (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'B 'C)
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                                     (LIST 'DEFINT_CHOOSE 'F2 'X)
                                     (LIST 'MINUS 'A) 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                         (LIST 'DEFINT_CHOOSE 'F2 'X) (LIST 'QUOTIENT 1 2) 'X))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X)
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                               (LIST 'DEFINT_CHOOSE 'F2 'X)
                               (LIST 'PLUS (LIST 'QUOTIENT 1 2) 'A) 'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT (LIST '~ 'B)
                               (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                     (LIST 'SQRT (LIST '~ 'X))))
                         (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'B 'C)
                               (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                                     (LIST 'DEFINT_CHOOSE 'F2 'X)
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 'X))
                         (LIST 'AND (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X))))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'A)))))
                         (LIST '~ 'F1) (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'INTGGGG (LIST 'DEFINT_CHOOSE 'F1 'X)
                               (LIST 'DEFINT_CHOOSE 'F2 'X)
                               (LIST 'DIFFERENCE
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 'A)
                               'X)
                         (LIST 'FREEOF 'A 'X)))
             (LIST 'REPLACEBY
                   (LIST 'DEFINT2 (LIST 'MINUS (LIST '~ 'B)) (LIST '~ 'F1)
                         (LIST '~ 'F2) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES 'B (LIST 'DEFINT2 'F1 'F2 'X)))
                         (LIST 'FREEOF 'B 'X)))))) 
(AEVAL (LET '(DEFINT2_RULES))) 
(AEVAL 'NIL) 
(ENDMODULE) 