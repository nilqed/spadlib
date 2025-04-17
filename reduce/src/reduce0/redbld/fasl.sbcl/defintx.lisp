(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTX)) 
(LOAD_PACKAGE (LIST 'SOLVE 'MISC)) 
(FLUID '(*ALLPOLY RDON* *NORATIONALGI)) 
(SWITCH (LIST 'ALLPOLY)) 
(GLOBAL '(DOMAINLIST* POLES*)) 
(SETK 'LOGCOMPLEX
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'PLUS (LIST '~ 'X) 'I))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'LOG
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'TIMES 'X 'X) 1)))
                               (LIST 'TIMES 'I
                                     (LIST 'ATAN2
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             1)))
                                           (LIST 'QUOTIENT 'X
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             1))))))
                         (LIST 'EQUAL (LIST 'REPART 'X) 'X)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'DIFFERENCE (LIST '~ 'X) 'I))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'LOG
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'TIMES 'X 'X) 1)))
                               (LIST 'TIMES 'I
                                     (LIST 'ATAN2
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             1)))
                                           (LIST 'QUOTIENT 'X
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             1))))))
                         (LIST 'EQUAL (LIST 'REPART 'X) 'X)))
             (LIST 'REPLACEBY
                   (LIST 'LOG
                         (LIST 'PLUS (LIST '~ 'X)
                               (LIST 'TIMES 'I (LIST '~ 'Y))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'LOG
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'TIMES 'X 'X)
                                                 (LIST 'TIMES 'Y 'Y))))
                               (LIST 'TIMES 'I
                                     (LIST 'ATAN2
                                           (LIST 'QUOTIENT 'Y
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y))))
                                           (LIST 'QUOTIENT 'X
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y)))))))
                         (LIST 'AND (LIST 'EQUAL (LIST 'REPART 'X) 'X)
                               (LIST 'EQUAL (LIST 'REPART 'Y) 'Y))))
             (LIST 'REPLACEBY
                   (LIST 'LOG
                         (LIST 'DIFFERENCE (LIST '~ 'X)
                               (LIST 'TIMES 'I (LIST '~ 'Y))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'LOG
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'TIMES 'X 'X)
                                                 (LIST 'TIMES 'Y 'Y))))
                               (LIST 'TIMES 'I
                                     (LIST 'ATAN2
                                           (LIST 'QUOTIENT 'Y
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y))))
                                           (LIST 'QUOTIENT 'X
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y)))))))
                         (LIST 'AND (LIST 'EQUAL (LIST 'REPART 'X) 'X)
                               (LIST 'EQUAL (LIST 'REPART 'Y) 'Y))))
             (LIST 'REPLACEBY
                   (LIST 'LOG (LIST 'QUOTIENT (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'WHEN (LIST 'DIFFERENCE (LIST 'LOG 'X) (LIST 'LOG 'Y))
                         (LIST 'EQUAL (LIST 'REPART 'Y) 'Y)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'SQRT (LIST '~ 'X)))
                   (LIST 'QUOTIENT (LIST 'LOG 'X) 2))
             (LIST 'REPLACEBY (LIST 'LOG (MINUS 1)) (LIST 'TIMES 'I 'PI))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'MINUS 'I))
                   (LIST 'MINUS (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))))
             (LIST 'REPLACEBY (LIST 'LOG 'I)
                   (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'WHEN (LIST 'PLUS (LIST 'TIMES 'I 'PI) (LIST 'LOG 'X))
                         (LIST 'AND (LIST 'EQUAL (LIST 'REPART 'X) 'X)
                               (LIST 'NUMBERP 'X) (LIST 'GREATERP 'X 0))))
             (LIST 'REPLACEBY
                   (LIST 'LOG (LIST 'MINUS (LIST 'TIMES 'I (LIST '~ 'X))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2)))
                               (LIST 'LOG 'X))
                         (LIST 'AND (LIST 'EQUAL (LIST 'REPART 'X) 'X)
                               (LIST 'NUMBERP 'X) (LIST 'GREATERP 'X 0))))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'TIMES 'I (LIST '~ 'X)))
                   (LIST 'WHEN
                         (LIST 'PLUS (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))
                               (LIST 'LOG 'X))
                         (LIST 'AND (LIST 'EQUAL (LIST 'REPART 'X) 'X)
                               (LIST 'NUMBERP 'X) (LIST 'GREATERP 'X 0))))))) 
(SETK 'ATAN2EVAL
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT (LIST 'SQRT 3) 2)
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'QUOTIENT 'PI 3))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 3)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT (LIST 'SQRT 3) 2)
                         (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                   (LIST 'TIMES 2 (LIST 'QUOTIENT 'PI 3)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
                         (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                   (LIST 'MINUS (LIST 'TIMES 2 (LIST 'QUOTIENT 'PI 3))))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2
                         (LIST 'QUOTIENT 3 (LIST 'TIMES 2 (LIST 'SQRT 3)))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'QUOTIENT 'PI 3))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2
                         (LIST 'MINUS
                               (LIST 'QUOTIENT 3
                                     (LIST 'TIMES 2 (LIST 'SQRT 3))))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 3)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2
                         (LIST 'QUOTIENT 3 (LIST 'TIMES 2 (LIST 'SQRT 3)))
                         (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                   (LIST 'TIMES 2 (LIST 'QUOTIENT 'PI 3)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2
                         (LIST 'MINUS
                               (LIST 'QUOTIENT 3
                                     (LIST 'TIMES 2 (LIST 'SQRT 3))))
                         (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                   (LIST 'MINUS (LIST 'TIMES 2 (LIST 'QUOTIENT 'PI 3))))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT 1 2)
                         (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
                   (LIST 'QUOTIENT 'PI 6))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST 'QUOTIENT (LIST 'SQRT 3) 2))
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 6)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT 1 2)
                         (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 3) 2)))
                   (LIST 'TIMES 5 (LIST 'QUOTIENT 'PI 6)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 3) 2)))
                   (LIST 'MINUS (LIST 'TIMES 5 (LIST 'QUOTIENT 'PI 6))))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT 1 2)
                         (LIST 'QUOTIENT 3 (LIST 'TIMES 2 (LIST 'SQRT 3))))
                   (LIST 'QUOTIENT 'PI 6))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST 'QUOTIENT 3 (LIST 'TIMES 2 (LIST 'SQRT 3))))
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 6)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT 1 2)
                         (LIST 'MINUS
                               (LIST 'QUOTIENT 3
                                     (LIST 'TIMES 2 (LIST 'SQRT 3)))))
                   (LIST 'TIMES 5 (LIST 'QUOTIENT 'PI 6)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                         (LIST 'MINUS (LIST 'TIMES 3 2 (LIST 'SQRT 3))))
                   (LIST 'MINUS (LIST 'TIMES 5 (LIST 'QUOTIENT 'PI 6))))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT (LIST 'SQRT 2) 2)
                         (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
                   (LIST 'QUOTIENT 'PI 4))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
                         (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 4)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'QUOTIENT (LIST 'SQRT 2) 2)
                         (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 2) 2)))
                   (LIST 'TIMES 3 (LIST 'QUOTIENT 'PI 4)))
             (LIST 'REPLACEBY
                   (LIST 'ATAN2 (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 2) 2))
                         (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SQRT 2) 2)))
                   (LIST 'MINUS (LIST 'TIMES 3 (LIST 'QUOTIENT 'PI 4))))
             (LIST 'REPLACEBY (LIST 'ATAN2 0 (MINUS 1)) 'PI)
             (LIST 'REPLACEBY (LIST 'ATAN2 0 1) 0)
             (LIST 'REPLACEBY (LIST 'ATAN2 1 0) (LIST 'QUOTIENT 'PI 2))
             (LIST 'REPLACEBY (LIST 'ATAN2 (MINUS 1) 0)
                   (LIST 'MINUS (LIST 'QUOTIENT 'PI 2)))))) 
(SETK 'IPOWER
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT 'I (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                         (LIST 'TIMES 'I
                               (LIST 'SIN
                                     (LIST 'TIMES 'N
                                           (LIST 'QUOTIENT 'PI 2))))))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'MINUS 'I) (LIST '~ 'N))
                   (LIST 'DIFFERENCE
                         (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                         (LIST 'TIMES 'I
                               (LIST 'SIN
                                     (LIST 'TIMES 'N
                                           (LIST 'QUOTIENT 'PI 2))))))))) 
(AEVAL 'NIL) 
(PROG (OLDMODE)
  (COND (DMODE* (SETQ OLDMODE (SETDMODE DMODE* NIL))))
  (AEVAL (LET '(ATAN2EVAL)))
  (COND (OLDMODE (SETDMODE OLDMODE T)))) 
(FLUID '(*DIFFSOLN ZPLIST! |POLES#| *MSG *ROUNDED *COMPLEX ZLIST)) 
(SWITCH (LIST 'DIFFSOLN)) 
(LOAD_PACKAGE (LIST 'INT)) 
(PUT 'DEFINT0 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT0 'DEFINED-ON-LINE '113) 
(PUT 'DEFINT0 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINT0 (U)
    (PROG (RDON* *MSG C *NONEGLOGS FAC *COMBINELOGS *RATIONALIZE)
      (COND ((NOT (GETD 'SOLVESQ)) (LOAD_PACKAGE (LIST 'SOLVE))))
      (COND
       ((NEQ (LENGTH U) 4) (REDERR "defint called with wrong number of args")))
      (SETQ C *COMPLEX)
      (OFF (LIST 'COMPLEX))
      (SETQ *NONEGLOGS T)
      (AEVAL (LET '(IPOWER LOGCOMPLEX)))
      (SETQ FAC *FACTOR)
      (ON (LIST 'FACTOR))
      ((LAMBDA (*NORATIONALGI)
         (SETQ U (ERRORSET2 (LIST 'DEFINT1 (MKQUOTE U)))))
       T)
      (COND ((ERRORP U) (PROGN (SETQ U 'FAILED) (GO RET)))
            (T (SETQ U (CAR U))))
      (OFF (LIST 'FACTOR))
      (COND
       (*ROUNDED
        ((LAMBDA (RL IM EPS)
           (PROGN
            (OFF (LIST 'COMPLEX))
            (COND
             ((AND (OR (ATOM (CAR U)) (ATOM (CAR (CAR U)))) (EQUAL (CDR U) 1))
              (COND
               ((EVALGREATERP (LIST 'ABS (PREPSQ RL))
                              (LIST 'TIMES (LIST 'ABS (PREPSQ IM)) EPS))
                (SETQ U RL))
               ((EVALGREATERP (LIST 'ABS (PREPSQ IM))
                              (LIST 'TIMES (LIST 'ABS (PREPSQ RL)) EPS))
                (SETQ U (ADDSQ U (NEGSQ RL)))))))
            (SETQ U (MK*SQ U))
            (COND (RDON* (OFF (LIST 'ROUNDED))))
            (OFF (LIST 'COMPLEX))
            (GO RET2)))
         (REPARTSQ U) (IMPARTSQ U) (EXPT 10.0 (DIFFERENCE 2 (PRECISION 0))))))
      (SETQ *RATIONALIZE T)
      (SETQ U (REVAL1 (PREPSQ U) NIL))
      (ON (LIST 'COMPLEX))
      (SETQ U (SIMP* U))
     RET
      (ONOFF 'FACTOR FAC)
      (OFF (LIST 'COMPLEX))
      (AEVAL (CLEARRULES (LIST 'IPOWER 'LOGCOMPLEX)))
      (COND ((NEQ U 'FAILED) (SETQ U (PREPSQ U))))
      (OFF (LIST 'COMPLEX))
      (ON (LIST 'COMBINELOGS))
      (COND ((NEQ U 'FAILED) (SETQ U (REVAL1 U NIL))))
     RET2
      (COND (C (ON (LIST 'COMPLEX))))
      (RETURN U))) 
(PUT 'DEFINT1 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT1 'DEFINED-ON-LINE '160) 
(PUT 'DEFINT1 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINT1 (U) (DEFINT11S (CAR U) (CADR U) (CADDR U) (CADDDR U))) 
(PUT 'DEFINT11S 'NUMBER-OF-ARGS 4) 
(PUT 'DEFINT11S 'DEFINED-ON-LINE '170) 
(PUT 'DEFINT11S 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT11S 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT11S (EXP VAR LLIM ULIM)
    (PROG (SPLIT_EXP)
      (SETQ SPLIT_EXP (SPLITFACTORS (SIMP* (REVAL1 EXP NIL)) VAR))
      (ON (LIST 'COMPLEX))
      (RETURN
       (COND
        (((LAMBDA (X) (OR (EQ X 'I) (AND (EQCAR X 'TIMES) (MEMQ 'I X))))
          (CAR SPLIT_EXP))
         (DEFINT11 EXP VAR LLIM ULIM T))
        (T
         (MULTSQ (SIMP* (CAR SPLIT_EXP))
                 (DEFINT11 (CADR SPLIT_EXP) VAR LLIM ULIM T))))))) 
(PUT 'FXINFINITY 'NUMBER-OF-ARGS 1) 
(PUT 'FXINFINITY 'DEFINED-ON-LINE '185) 
(PUT 'FXINFINITY 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'FXINFINITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FXINFINITY (X)
    (COND ((EQ X 'INFINITY) 'INF) ((EQUAL X '(MINUS INFINITY)) 'MINF) (T X))) 
(PUT 'DEFINT11 'NUMBER-OF-ARGS 5) 
(PUT 'DEFINT11 'DEFINED-ON-LINE '189) 
(PUT 'DEFINT11 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT11 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT11 (EXP VAR LLIM ULIM DTST)
    (COND
     ((OR
       (EVALEQUAL (SETQ LLIM (FXINFINITY LLIM)) (SETQ ULIM (FXINFINITY ULIM)))
       (EVALEQUAL EXP 0))
      (CONS NIL 1))
     (T
      (PROG (*NORATIONALGI R P Q POLES RLRTS CMPRTS Q1 M N)
        (COND
         ((OR (EQUAL ULIM 'MINF) (EQUAL LLIM 'INF))
          (RETURN (DEFINT11 (LIST 'MINUS EXP) VAR ULIM LLIM DTST))))
        (COND
         ((EQUAL ULIM 'INF)
          (PROGN
           (SETQ EXP (SIMP* EXP))
           (COND
            ((NOT (MEMBER LLIM '(0 MINF)))
             (PROGN
              (SETQ EXP (SUBSQ EXP (LIST (CONS VAR (LIST 'PLUS VAR LLIM)))))
              (SETQ LLIM 0))))
           (GO C0))))
        (COND
         ((EQUAL LLIM 'MINF)
          (PROGN
           (SETQ EXP (SIMP* EXP))
           (OFF (LIST 'COMPLEX))
           (SETQ EXP
                   (REVAL1
                    (PREPSQ (SUBSQ EXP (LIST (CONS VAR (LIST 'MINUS VAR)))))
                    T))
           (SETQ LLIM (REVAL1 (LIST 'MINUS ULIM) T))
           (ON (LIST 'COMPLEX))
           (RETURN (DEFINT11 EXP VAR LLIM 'INF DTST)))))
        (SETQ R (SIMPINT (LIST EXP VAR)))
        (COND ((EQCAR (PREPSQ R) 'INT) (GO C1)))
        (SETQ P
                (ERRORSET2
                 (LIST 'SUBSQ (MKQUOTE R) (MKQUOTE (LIST (CONS VAR ULIM))))))
        (SETQ Q
                (ERRORSET2
                 (LIST 'SUBSQ (MKQUOTE R) (MKQUOTE (LIST (CONS VAR LLIM))))))
        (COND
         ((ERRORP P) (SETQ P (SIMPLIMIT (LIST 'LIMIT- (MK*SQ R) VAR ULIM))))
         (T (SETQ P (CAR P))))
        (COND
         ((ERRORP Q) (SETQ Q (SIMPLIMIT (LIST 'LIMIT+ (MK*SQ R) VAR LLIM))))
         (T (SETQ Q (CAR Q))))
        (RETURN (SETQ Q1 (ADDSQ P (NEGSQ Q))))
       C1
        (REDERR "special forms for finite limits not implemented")
       C0
        (SETQ R EXP)
        (SETQ P (CAR R))
        (SETQ Q (CDR R))
        (SETQ M (DEGREEOF P VAR))
        (SETQ N (DEGREEOF Q VAR))
        (COND
         ((OR (SMEMQL '(FAILED INFINITY) M) (SMEMQL '(FAILED INFINITY) N))
          (RETURN (ERROR 99 'FAILED))))
        (COND
         ((NOT
           (EVALGREATERP (PREPSQ (ADDSQ (REPARTSQ N) (NEGSQ (REPARTSQ M)))) 1))
          (GO DIV)))
        (COND ((SETQ Q1 (SPECFORMTESTINT Q P VAR LLIM ULIM)) (RETURN Q1)))
        (COND
         ((OR (NOT (SETQ M (SQ2INT M))) (NOT (SETQ N (SQ2INT N))))
          (PROGN
           (PROGN (PRIN2 "this irrational function case not handled") NIL)
           (TERPRI)
           (ERROR 99 'FAILED))))
        (COND ((LESSP (DIFFERENCE N M) 2) (GO DIV)))
        (COND
         ((AND DTST *DIFFSOLN)
          (COND ((SETQ Q1 (DIFFSOL Q P M N VAR LLIM ULIM)) (RETURN Q1)))))
        (OFF (LIST 'FACTOR))
        (SETQ *NORATIONALGI NIL)
        (SETQ POLES (GETPOLES Q VAR LLIM))
        (SETQ RLRTS (APPEND (CAR POLES) (CADR POLES)))
        (SETQ CMPRTS (CADDR POLES))
        (SETQ *NORATIONALGI T)
        (SETQ Q1 (DIFFF Q VAR))
        (SETQ Q (CONS Q 1))
        (SETQ P (CONS P 1))
        (RETURN
         (COND ((EQUAL LLIM 0) (DEFINT2 P Q Q1 VAR RLRTS CMPRTS))
               (T (DEFINT3 P Q Q1 VAR RLRTS CMPRTS))))
       DIV
        (ERROR 99 'FAILED))))) 
(PUT 'ZPSUBSQ 'NUMBER-OF-ARGS 1) 
(PUT 'ZPSUBSQ 'DEFINED-ON-LINE '256) 
(PUT 'ZPSUBSQ 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'ZPSUBSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZPSUBSQ (X)
    (SUBSQ X
           (PROG (V FORALL-RESULT FORALL-ENDPTR)
             (SETQ V ZPLIST!)
             (COND ((NULL V) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (V) (CONS V 0)) (CAR V)) NIL)))
            LOOPLABEL
             (SETQ V (CDR V))
             (COND ((NULL V) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (V) (CONS V 0)) (CAR V)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))) 
(PUT 'DEGREEOF 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREEOF 'DEFINED-ON-LINE '259) 
(PUT 'DEGREEOF 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEGREEOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREEOF (P VAR)
    (COND
     ((SMEMQL '(BESSELJ BESSELK BESSELY BESSELI FRESNEL_S |,| FRESNEL_C) P)
      (CONS (LIST (CONS (CONS 'FAILED 1) 1)) 1))
     (T
      ((LAMBDA (D DE)
         (COND ((NULL (CAR DE)) DE)
               (T
                (PROGN
                 (COND (D (SETDMODE (GET D 'DNAME) NIL)))
                 (SETQ P
                         (SIMP*
                          (LIMIT
                           (LIST 'QUOTIENT (LIST 'TIMES VAR (PREPSQ DE))
                                 (PREPF P))
                           VAR 'INFINITY)))
                 (COND (D (SETDMODE D T)))
                 P))))
       DMODE* (DIFFF P VAR))))) 
(PUT 'GENMINUSP 'NUMBER-OF-ARGS 1) 
(PUT 'GENMINUSP 'DEFINED-ON-LINE '274) 
(PUT 'GENMINUSP 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'GENMINUSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENMINUSP (X)
    (COND ((OR (ATOM X) (ATOM (CAR X))) (|:MINUSP| X))
          (T (|:MINUSP| (TOPEVAL (PREPF X)))))) 
(PUT 'SQ2INT 'NUMBER-OF-ARGS 1) 
(PUT 'SQ2INT 'DEFINED-ON-LINE '277) 
(PUT 'SQ2INT 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'SQ2INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ2INT (X)
    ((LAMBDA (Y)
       ((LAMBDA (Z)
          (COND
           ((AND (NULL (CAR (IMPARTSQ X))) (EQUAL (CDR Y) 1))
            (COND ((NULL Z) 0) ((NUMBERP Z) Z) (T NIL)))))
        (CAR Y)))
     (REPARTSQ X))) 
(PUT 'TOPEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'TOPEVAL 'DEFINED-ON-LINE '282) 
(PUT 'TOPEVAL 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'TOPEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOPEVAL (U)
    ((LAMBDA (R C *MSG)
       (PROGN
        (COND ((NOT R) (ON (LIST 'ROUNDED))))
        (COND ((NOT C) (ON (LIST 'COMPLEX))))
        (SETQ U (CAR (SIMP* (REVAL1 U NIL))))
        (COND ((NOT R) (OFF (LIST 'ROUNDED))))
        (COND ((NOT C) (OFF (LIST 'COMPLEX))))
        U))
     *ROUNDED *COMPLEX NIL)) 
(PUT 'FIRSTATOM 'NUMBER-OF-ARGS 1) 
(PUT 'FIRSTATOM 'DEFINED-ON-LINE '288) 
(PUT 'FIRSTATOM 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'FIRSTATOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIRSTATOM (X) (COND ((ATOM X) X) (T (FIRSTATOM (CAR X))))) 
(PUT 'VALUEOF 'NUMBER-OF-ARGS 1) 
(PUT 'VALUEOF 'DEFINED-ON-LINE '291) 
(PUT 'VALUEOF 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'VALUEOF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VALUEOF (U) ((LAMBDA (X) (COND ((NEQ (FIRSTATOM X) 'ROOT_OF) X))) (CAAR U))) 
(PUT 'RDSOLVESQ 'NUMBER-OF-ARGS 1) 
(PUT 'RDSOLVESQ 'DEFINED-ON-LINE '294) 
(PUT 'RDSOLVESQ 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'RDSOLVESQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDSOLVESQ (U)
    ((LAMBDA (X)
       (SOLVESQ (SUBF (CAR (SIMP* (CADR X))) (LIST (CONS (CADDR X) (CAADR U))))
                (CAADR U) (CADDR U)))
     (CAAAAR (CAAR U)))) 
(PUT 'DEFINT2 'NUMBER-OF-ARGS 6) 
(PUT 'DEFINT2 'DEFINED-ON-LINE '299) 
(PUT 'DEFINT2 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT2 (P Q Q1 VAR RLRTS CMPRTS)
    (PROG (INT)
      (ERROR 99 "THIS IS WRONG, Stanley!!")
      (SETQ P
              (SIMP*
               (REVAL1 (LIST 'TIMES (LIST 'LOG (LIST 'MINUS VAR)) (PREPSQ P))
                       NIL)))
      (SETQ INT (CONS NIL 1))
      (PROG (R)
        (SETQ R (APPEND RLRTS CMPRTS))
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (SETQ INT
                   (ADDSQ INT (RESIDUUM P Q Q1 VAR (PREPSQ (CAR R)) (CDR R)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN (NEGSQ INT)))) 
(PUT 'DEFINT3 'NUMBER-OF-ARGS 6) 
(PUT 'DEFINT3 'DEFINED-ON-LINE '310) 
(PUT 'DEFINT3 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DEFINT3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT3 (P Q Q1 VAR RLRTS CMPRTS)
    (PROG (INT INT2)
      (SETQ INT (SETQ INT2 (CONS NIL 1)))
      (PROG (R)
        (SETQ R CMPRTS)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (SETQ INT
                   (ADDSQ INT (RESIDUUM P Q Q1 VAR (PREPSQ (CAR R)) (CDR R)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (SETQ INT (ADDSQ INT INT))
      (PROG (R)
        (SETQ R RLRTS)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (SETQ INT2
                   (ADDSQ INT2
                          (RESIDUUM P Q Q1 VAR (PREPSQ (CAR R)) (CDR R)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (SETQ INT (ADDSQ INT INT2))
      (RETURN (MULTSQ (SIMP* '(TIMES PI I)) INT)))) 
(PUT 'DIFFSQN 'NUMBER-OF-ARGS 3) 
(PUT 'DIFFSQN 'DEFINED-ON-LINE '323) 
(PUT 'DIFFSQN 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DIFFSQN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFFSQN (SQ VAR N)
    (PROGN
     (COND
      ((GREATERP N 0)
       (PROG (J)
         (SETQ J 1)
        LAB
         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
         (SETQ SQ (DIFFSQ SQ VAR))
         (SETQ J (PLUS2 J 1))
         (GO LAB))))
     SQ)) 
(PUT 'POLYPWRP 'NUMBER-OF-ARGS 2) 
(PUT 'POLYPWRP 'DEFINED-ON-LINE '327) 
(PUT 'POLYPWRP 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'POLYPWRP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYPWRP (EXP VAR)
    (PROG (POL FL S PWR)
      (SETQ S 0)
      (SETQ PWR 0)
      (COND
       ((EQCAR EXP 'EXPT)
        (PROGN
         (SETQ POL (CADR EXP))
         (COND ((LESSP (SETQ PWR (CADDR EXP)) 2) (RETURN NIL)))
         (COND ((ATOM POL) (COND ((EQ VAR POL) (SETQ S 1)) (T (RETURN NIL))))
               ((NOT (EQCAR POL 'PLUS)) (RETURN NIL))
               (T
                (PROG (P)
                  (SETQ P (CDR POL))
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P) (SETQ S (MAX S (TERMVPWR P VAR)))) (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))))
         (RETURN (COND ((EQUAL S 0) NIL) (T (LIST POL S PWR))))))
       ((EQCAR EXP 'TIMES)
        (PROGN
         (SETQ EXP
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (CDR EXP))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P) (POLYPWRP P VAR)) (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (P) (POLYPWRP P VAR)) (CAR P)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (P)
           (SETQ P EXP)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (PROGN
               (COND ((NULL P) (SETQ FL T)))
               (COND ((NOT FL) (SETQ PWR (GCDN PWR (CADDR P)))))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (COND (FL (RETURN NIL)))
         (SETQ S
                 (QUOTIENT
                  (PROG (P FORALL-RESULT)
                    (SETQ P EXP)
                    (SETQ FORALL-RESULT 0)
                   LAB1
                    (COND ((NULL P) (RETURN FORALL-RESULT)))
                    (SETQ FORALL-RESULT
                            (PLUS
                             ((LAMBDA (P) (TIMES (CADR P) (CADDR P))) (CAR P))
                             FORALL-RESULT))
                    (SETQ P (CDR P))
                    (GO LAB1))
                  PWR))
         (SETQ POL
                 (CONS 'TIMES
                       (PROG (P FORALL-RESULT FORALL-ENDPTR)
                         (SETQ P EXP)
                         (COND ((NULL P) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (P)
                                             (LIST 'EXPT (CAR P)
                                                   (QUOTIENT (CADDR P) PWR)))
                                           (CAR P))
                                          NIL)))
                        LOOPLABEL
                         (SETQ P (CDR P))
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (P)
                                     (LIST 'EXPT (CAR P)
                                           (QUOTIENT (CADDR P) PWR)))
                                   (CAR P))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (RETURN (LIST POL S PWR))))))) 
(PUT 'TERMVPWR 'NUMBER-OF-ARGS 2) 
(PUT 'TERMVPWR 'DEFINED-ON-LINE '347) 
(PUT 'TERMVPWR 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'TERMVPWR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TERMVPWR (P VAR)
    (COND ((FREEOF P VAR) 0) ((ATOM P) 1)
          ((AND (EQCAR P 'EXPT) (EQUAL (CADR P) VAR) (NUMBERP (CADDR P)))
           (CADDR P))
          ((EQCAR P 'TIMES)
           (PROG (Q FORALL-RESULT)
             (SETQ Q (CDR P))
             (SETQ FORALL-RESULT 0)
            LAB1
             (COND ((NULL Q) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT
                     (PLUS ((LAMBDA (Q) (TERMVPWR Q VAR)) (CAR Q))
                           FORALL-RESULT))
             (SETQ Q (CDR Q))
             (GO LAB1)))
          (T 0))) 
(PUT 'DIFFSOL 'NUMBER-OF-ARGS 7) 
(PUT 'DIFFSOL 'DEFINED-ON-LINE '355) 
(PUT 'DIFFSOL 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'DIFFSOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE DIFFSOL (Q P MM NN VAR LLIM ULIM)
    (AND (SETQ Q (POLYPWRP (PREPF Q) VAR))
         (PROG (N S M R ZPLIST!)
           (SETQ N MM)
           (SETQ S (CADR Q))
           (SETQ M (CADDR Q))
           (COND
            ((OR (GREATERP S 2) (NEQ (TIMES M S) NN) (LEQ (DIFFERENCE NN N) 2))
             (RETURN NIL)))
           (SETQ R (QUOTIENT (PLUS N 2) S))
           (COND ((LESSP (TIMES R S) (PLUS N 2)) (SETQ R (PLUS R 1))))
           (COND ((EQUAL M R) (RETURN NIL)))
           (SETQ Q (LIST 'PLUS (CAR Q) 'ZP!))
           (SETQ ZPLIST! '(ZP!))
           (SETQ Q (CAR (SIMP* (LIST 'EXPT Q R))))
           (SETQ NN
                   (CONS
                    (TIMES (EXPT (MINUS 1) (DIFFERENCE M R))
                           (FACTORIAL (DIFFERENCE R 1)))
                    (FACTORIAL (DIFFERENCE M 1))))
           (SETQ P (DEFINT11 (PREPSQ (CONS P Q)) VAR LLIM ULIM NIL))
           (SETQ P (ZPSUBSQ (DIFFSQN P 'ZP! (DIFFERENCE M R))))
           (RETURN (MULTSQ NN P))))) 
(PUT 'RESIDUUM 'NUMBER-OF-ARGS 6) 
(PUT 'RESIDUUM 'DEFINED-ON-LINE '375) 
(PUT 'RESIDUUM 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'RESIDUUM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESIDUUM (P Q Q1 VAR POLE M)
    (COND ((EQUAL M 1) (SUBSQ (MULTSQ P (INVSQ Q1)) (LIST (CONS VAR POLE))))
          (T
           (PROG (N)
             (SETQ N 0)
             (SETQ Q1 NIL)
             (PROG (R)
               (SETQ R POLES*)
              LAB
               (COND ((NULL R) (RETURN NIL)))
               ((LAMBDA (R)
                  (PROGN
                   (SETQ N (CDR R))
                   (SETQ R (PREPSQ (CAR R)))
                   (COND
                    ((NOT (EVALEQUAL POLE R))
                     (SETQ Q1
                             (CONS (LIST 'EXPT (LIST 'DIFFERENCE VAR R) N)
                                   Q1))))))
                (CAR R))
               (SETQ R (CDR R))
               (GO LAB))
             (SETQ N ((LAMBDA (*FACTOR) (CDAR (CAR (SIMP* (PREPSQ Q))))) NIL))
             (SETQ Q1 (CONS 'TIMES (CONS N Q1)))
             (RETURN
              (COND
               (((LAMBDA (*FACTOR)
                   (EQUAL (CAR (CAR (SIMP* (PREPSQ Q))))
                          (CAR
                           (CAR
                            (SIMP*
                             (LIST 'TIMES
                                   (LIST 'EXPT (LIST 'DIFFERENCE VAR POLE) M)
                                   Q1))))))
                 NIL)
                (PROGN
                 (SETQ Q (MULTSQ P (INVSQ (SIMP* Q1))))
                 (SETQ Q (DIFFSQN Q VAR (DIFFERENCE M 1)))
                 (SETQ Q (SUBSQ Q (LIST (CONS VAR POLE))))
                 (SETQ Q
                         (COND ((NULL (CAR Q)) Q)
                               (T
                                (MULTSQ Q
                                        (INVSQ
                                         (CONS (FACTORIAL (DIFFERENCE M 1))
                                               1))))))))
               (T
                (SETQ Q1
                        (SIMP*
                         (SETQ P
                                 (LIMIT
                                  (PREPSQ
                                   (MULTSQ
                                    (DIFFSQN
                                     (MULTSQ (MULTSQ P (INVSQ Q))
                                             (SIMP*
                                              (LIST 'EXPT
                                                    (LIST 'DIFFERENCE VAR POLE)
                                                    M)))
                                     VAR (DIFFERENCE M 1))
                                    (INVSQ
                                     (CONS (FACTORIAL (DIFFERENCE M 1)) 1))))
                                  VAR POLE))))))))))) 
(PUT 'SPLITFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'SPLITFACTORS 'DEFINED-ON-LINE '401) 
(PUT 'SPLITFACTORS 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'SPLITFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPLITFACTORS (U VAR)
    (PROG (N D NI ND DI DD FLI FLD)
      (SETQ N (PREPF (CAR U)))
      (COND ((EQUAL N 0) (RETURN (LIST 0 0))))
      (SETQ D (PREPF (CDR U)))
      (SETQ NI (SETQ ND (SETQ DI (SETQ DD 1))))
      (COND
       ((SIMPTERMP N)
        (PROGN (COND ((FREEOF N VAR) (SETQ NI N)) (T (SETQ ND N))) (GO D))))
      (PROG (X)
        (SETQ X (CDR N))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((FREEOF X VAR)
             (SETQ NI
                     (COND ((EQUAL NI 1) (LIST X))
                           (T (PROGN (SETQ FLI T) (CONS X NI))))))
            (T
             (SETQ ND
                     (COND ((EQUAL ND 1) (LIST X))
                           (T (PROGN (SETQ FLD T) (CONS X ND))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ NI (FLESHOUTT NI FLI))
      (SETQ ND (FLESHOUTT ND FLD))
      (SETQ FLI (SETQ FLD NIL))
     D
      (COND
       ((SIMPTERMP D)
        (PROGN (COND ((FREEOF D VAR) (SETQ DI D)) (T (SETQ DD D))) (GO RET))))
      (PROG (X)
        (SETQ X (CDR D))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((FREEOF X VAR)
             (SETQ DI
                     (COND ((EQUAL DI 1) (LIST X))
                           (T (PROGN (SETQ FLI T) (CONS X DI))))))
            (T
             (SETQ DD
                     (COND ((EQUAL DD 1) (LIST X))
                           (T (PROGN (SETQ FLD T) (CONS X DD))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ DI (FLESHOUTT DI FLI))
      (SETQ DD (FLESHOUTT DD FLD))
     RET
      (RETURN (LIST (FLESHOUT NI DI) (FLESHOUT ND DD))))) 
(PUT 'SIMPTERMP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTERMP 'DEFINED-ON-LINE '426) 
(PUT 'SIMPTERMP 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'SIMPTERMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTERMP (X)
    (OR (ATOM X)
        ((LAMBDA (Y)
           (OR (AND (EQUAL Y 'MINUS) (SIMPTERMP (CADR X))) (NEQ Y 'TIMES)))
         (CAR X)))) 
(PUT 'FLESHOUT 'NUMBER-OF-ARGS 2) 
(PUT 'FLESHOUT 'DEFINED-ON-LINE '430) 
(PUT 'FLESHOUT 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'FLESHOUT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FLESHOUT (N D) (COND ((EQUAL D 1) N) (T (LIST 'QUOTIENT N D)))) 
(PUT 'FLESHOUTT 'NUMBER-OF-ARGS 2) 
(PUT 'FLESHOUTT 'DEFINED-ON-LINE '432) 
(PUT 'FLESHOUTT 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'FLESHOUTT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FLESHOUTT (N D) (COND ((EQUAL N 1) N) (D (CONS 'TIMES N)) (T (CAR N)))) 
(PUT 'SPECFORMTESTINT 'NUMBER-OF-ARGS 5) 
(PUT 'SPECFORMTESTINT 'DEFINED-ON-LINE '435) 
(PUT 'SPECFORMTESTINT 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'SPECFORMTESTINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPECFORMTESTINT (DEN NUM VAR LLIM ULIM)
    (PROG (A B D M N P Q1 Q K Z FF)
      (SETQ DEN (PREPF DEN))
      (SETQ NUM (PREPF NUM))
      (COND ((AND (NOT (EQUAL LLIM 0)) (EQUAL ULIM 'INF)) (GO T2)))
      (COND ((EQUAL NUM 1) (SETQ Q1 0))
            ((EQUAL (SETQ Q1 (VARPWRTST NUM VAR)) 0) (GO T2)))
      (COND ((ATOM DEN) (GO T2))
            ((NOT (EQCAR DEN 'TIMES))
             (COND ((SETQ K (AYNBMTST DEN VAR)) (GO SEP4)) (T (GO T2))))
            ((NEQ (LENGTH DEN) 3) (GO T2)))
      (SETQ D (CDR DEN))
      (COND
       ((NOT
         (OR
          (AND (SETQ K (AYNBMTST (CADR D) VAR))
               (EQCAR (SETQ Q (VARPWRTST (CAR D) VAR)) 'QUOTIENT))
          (AND (SETQ K (AYNBMTST (CAR D) VAR))
               (EQCAR (SETQ Q (VARPWRTST (CADR D) VAR)) 'QUOTIENT))))
        (GO T2)))
     SEP4
      (SETQ N (CADDR K))
      (COND ((NOT (NUMBERP N)) (GO T3)))
      (SETQ Q (PREPSQ (SIMP* (LIST 'PLUS 1 Q1 (LIST 'MINUS Q)))))
      (SETQ P (PREPSQ (SIMP* (LIST 'QUOTIENT Q N))))
      (SETQ M (CADDDR K))
      (COND ((OR (NOT (NUMBERP M)) (LESSP M 1)) (GO T3)))
      (SETQ A (CAR K))
      (SETQ B (CADR K))
      (SETQ Z (PREPSQ (SIMP* (LIST 'QUOTIENT B A))))
      (COND ((CAR (IMPARTSQ (SIMP* Z))) (GO T2)))
      (SETQ FF
              (PREPSQ
               (SIMP*
                (REVAL1 (LIST 'QUOTIENT 1 (LIST 'TIMES N (LIST 'EXPT A M)))
                        NIL))))
      (COND
       ((EVALGREATERP Z 0)
        (COND
         ((NOT
           (AND (EVALGREATERP (SETQ K (PREPSQ (REPARTSQ (SIMP* P)))) 0)
                (EVALGREATERP M K)))
          (GO T3))
         (T
          (PROGN
           (SETQ K
                   (PREPSQ
                    (SIMP*
                     (REVAL1
                      (LIST 'TIMES (LIST 'EXPT (MINUS 1) (PLUS M 1)) 'PI
                            (LIST 'EXPT Z (LIST 'DIFFERENCE P M)))
                      NIL))))
           (SETQ N 1)
           (PROG (C)
             (SETQ C 1)
            LAB
             (COND ((MINUSP (DIFFERENCE (DIFFERENCE M 1) C)) (RETURN NIL)))
             (SETQ N
                     (PREPSQ
                      (SIMP*
                       (REVAL1 (LIST 'TIMES N (LIST 'DIFFERENCE P C)) NIL))))
             (SETQ C (PLUS2 C 1))
             (GO LAB))
           (SETQ Q
                   (PREPSQ
                    (SIMP*
                     (REVAL1
                      (LIST 'TIMES (LIST 'FACTORIAL (DIFFERENCE M 1))
                            (LIST 'SIN (LIST 'TIMES P 'PI)))
                      NIL))))
           (RETURN
            (SIMP* (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES K N FF) Q) NIL))))))))
      (COND ((NEQ M 1) (GO T3)))
      (PROGN (PRIN2 "Cauchy principal value") NIL)
      (TERPRI)
      (SETQ K
              (PREPSQ
               (SIMP*
                (REVAL1
                 (LIST 'MINUS
                       (LIST 'EXPT (LIST 'QUOTIENT (MINUS 1) Z)
                             (LIST 'DIFFERENCE 1 P)))
                 NIL))))
      (SETQ Q
              (PREPSQ
               (SIMP*
                (REVAL1
                 (LIST 'TIMES FF (LIST 'QUOTIENT 'PI (LIST 'TIMES A N))
                       (LIST 'COT (LIST 'TIMES 'PI P)))
                 NIL))))
      (RETURN (SIMP* (REVAL1 (LIST 'TIMES K Q) NIL)))
     T3
      (RETURN NIL)
     T2
      (RETURN (SPECFORMTESTINT2 DEN NUM VAR LLIM ULIM)))) 
(PUT 'AYNBMTST 'NUMBER-OF-ARGS 2) 
(PUT 'AYNBMTST 'DEFINED-ON-LINE '499) 
(PUT 'AYNBMTST 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'AYNBMTST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AYNBMTST (EXP VAR)
    (PROG (A B M N)
      (COND ((NOT (EQCAR EXP 'EXPT)) (PROGN (SETQ M 1) (GO A2))))
      (SETQ M (CADDR EXP))
      (SETQ EXP (CADR EXP))
     A2
      (COND ((OR (NOT (EQCAR EXP 'PLUS)) (NEQ (LENGTH EXP) 3)) (RETURN NIL)))
      (SETQ B (CADDR EXP))
      (COND
       ((EQCAR (CADR EXP) 'TIMES)
        (PROGN
         (SETQ EXP (CDADR EXP))
         (COND
          ((OR (NEQ (LENGTH EXP) 2)
               (NOT
                (OR
                 (AND (NUMBERP (SETQ A (CAR EXP)))
                      (NEQ (SETQ N (VARPWRTST (CADR EXP) VAR)) 0))
                 (AND (NUMBERP (SETQ A (CADR EXP)))
                      (NEQ (SETQ N (VARPWRTST (CAR EXP) VAR)) 0)))))
           (RETURN NIL)))))
       (T
        (PROGN
         (SETQ A 1)
         (COND ((EQUAL (SETQ N (VARPWRTST (CADR EXP) VAR)) 0) (RETURN NIL))))))
      (RETURN (LIST A B N M)))) 
(FLUID '(*FULLPOLY)) 
(SWITCH (LIST 'FULLPOLY)) 
(PUT 'GETPOLES 'NUMBER-OF-ARGS 3) 
(PUT 'GETPOLES 'DEFINED-ON-LINE '525) 
(PUT 'GETPOLES 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'GETPOLES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETPOLES (Q VAR LLIM)
    (PROG (POLES RT M RLRT CMPRT RTV RTVZ CPV PRLRTS NRLRTS RLRTS CMPRTS
           *MULTIPLICITIES *FULLROOTS *NORATIONALGI)
      (OFF (LIST 'FACTOR))
      (SETQ *NORATIONALGI (SETQ POLES* NIL))
      (SETQ *MULTIPLICITIES T)
      (COND (*FULLPOLY (SETQ *FULLROOTS T)))
      (SETQ POLES (SOLVESQ (SIMP* (PREPF Q)) VAR 1))
      (SETQ *NORATIONALGI T)
     LP
      (COND ((NULL POLES) (GO INT)))
      (SETQ RT (CAR POLES))
      (SETQ POLES (CDR POLES))
      (SETQ M (CADDR RT))
      (SETQ RLRT (SETQ CMPRT NIL))
      (COND
       ((SETQ RTV (VALUEOF RT))
        (PROGN
         (SETQ POLES* (CONS (CONS RTV M) POLES*))
         (SETQ RTVZ (ZPSUBSQ RTV))
         (SETQ RT (CAR (IMPARTSQ RTVZ)))
         (COND
          ((OR (NULL RT)
               (AND (SETQ RT (TOPEVALSETSQ (PREPF RT)))
                    (EVALEQUAL 0 (PREPSQ RT))))
           (SETQ RLRT RTV))
          (T (SETQ CMPRT RTV)))
         (COND
          ((EQUAL LLIM 0)
           (PROGN
            (COND
             (RLRT
              (PROGN
               (COND ((NULL (CAR RTVZ)) (GO DIV))
                     ((NOT (GENMINUSP (CAR RTVZ)))
                      (PROGN
                       (COND ((GREATERP M 1) (GO DIV)) (T (SETQ CPV T)))
                       (SETQ PRLRTS (CONS (CONS RLRT M) PRLRTS))))
                     (T (SETQ NRLRTS (CONS (CONS RLRT M) NRLRTS))))))
             (T (SETQ CMPRTS (CONS (CONS CMPRT M) CMPRTS))))
            (GO LP)))
          (T
           (PROGN
            (COND
             (RLRT
              (PROGN
               (COND ((GREATERP M 1) (GO DIV)) (T (SETQ CPV T)))
               (SETQ RLRTS (CONS (CONS RLRT M) RLRTS))))
             ((NOT (GENMINUSP (CAR (IMPARTSQ RTVZ))))
              (SETQ CMPRTS (CONS (CONS CMPRT M) CMPRTS)))))))
         (GO LP))))
     UNA
      (COND (*ROUNDED (REDERR "unable to find poles approximately")))
      (COND
       ((NOT *ALLPOLY)
        (PROGN
         (PROGN
          (PRIN2
           "Denominator degree > 4.  Approx integral requires ON ALLPOLY")
          NIL)
         (TERPRI)
         (ERROR 99 "failed")))
       (T (PROGN (PROGN (PRIN2 "approximate integral only") NIL) (TERPRI))))
      (ON (LIST 'ROUNDED))
      (SETQ RDON* T)
      (COND
       ((VALUEOF (CAR (SETQ RT (RDSOLVESQ RT))))
        (PROGN (SETQ POLES (APPEND RT POLES)) (GO LP))))
      (GO UNA)
     DIV
      (ERROR 99 'FAILED)
     INT
      (COND
       (CPV (PROGN (PROGN (PRIN2 "Cauchy principal value") NIL) (TERPRI))))
      (RETURN
       (COND ((EQUAL LLIM 0) (LIST PRLRTS NRLRTS CMPRTS))
             (T (LIST RLRTS NIL CMPRTS)))))) 
(PUT 'SPECFORMTESTINT2 'NUMBER-OF-ARGS 5) 
(PUT 'SPECFORMTESTINT2 'DEFINED-ON-LINE '574) 
(PUT 'SPECFORMTESTINT2 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'SPECFORMTESTINT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPECFORMTESTINT2 (DEN NUM VAR LLIM ULIM)
    (PROG (D K K1 M P P1 Q Q1 POLES PRPOLES S1 S2)
      (COND ((AND (NOT (EQUAL LLIM 0)) (EQUAL ULIM 'INF)) (GO T2)))
      (SETQ P1 (POLANALYZ NUM VAR))
      (SETQ K1 (POLANALYZ DEN VAR))
      (COND ((NOT (OR P1 K1)) (GO T2)))
      (SETQ K (PREPSQ (SIMP* (REVAL1 (LIST 'DIFFERENCE P1 K1) NIL))))
      (COND
       ((OR (NUMBERP K)
            (NOT (AND (EVALGREATERP K (MINUS 1)) (EVALGREATERP 1 K))))
        (GO T2)))
      (COND ((SETQ D DMODE*) (ONOFF (SETQ D (GET D 'DNAME)) NIL)))
      (SETQ M
              (PREPSQ
               (SIMP*
                (REVAL1 (LIST 'QUOTIENT (LIST 'TIMES VAR NUM) DEN) NIL))))
      (COND
       ((OR (CAR (SIMP* (LIMIT+ M VAR 0)))
            (CAR (SIMP* (LIMIT M VAR 'INFINITY))))
        (GO T3)))
      (COND (D (ONOFF D T)))
      (SETQ P
              (SIMP*
               (REVAL1
                (LIST 'TIMES NUM (LIST 'EXPT VAR (LIST 'TIMES (MINUS 1) P1))
                      (LIST 'EXPT (LIST 'MINUS VAR) K))
                NIL)))
      (SETQ Q
              (SIMP*
               (REVAL1
                (LIST 'TIMES DEN (LIST 'EXPT VAR (LIST 'TIMES (MINUS 1) K1)))
                NIL)))
      (SETQ Q1 (DIFFSQ Q VAR))
      (SETQ POLES (GETPOLES (CAR Q) VAR LLIM))
      (SETQ PRPOLES (CAR POLES))
      (SETQ POLES (APPEND (CADR POLES) (CADDR POLES)))
      (SETQ S1 (SETQ S2 (CONS NIL 1)))
      (SETQ K1 (LIST 'TIMES 'PI (LIST 'PLUS K 1)))
      (COND
       (POLES
        (PROGN
         (PROG (R)
           (SETQ R POLES)
          LAB
           (COND ((NULL R) (RETURN NIL)))
           ((LAMBDA (R)
              (SETQ S1
                      (ADDSQ S1
                             (RESIDUUM P Q Q1 VAR (PREPSQ (CAR R)) (CDR R)))))
            (CAR R))
           (SETQ R (CDR R))
           (GO LAB))
         (SETQ S1
                 (LIST 'QUOTIENT (LIST 'TIMES 'PI (PREPSQ S1))
                       (LIST 'SIN K1)))))
       (T (SETQ S1 0)))
      (COND
       (PRPOLES
        (PROGN
         (PROG (R)
           (SETQ R PRPOLES)
          LAB
           (COND ((NULL R) (RETURN NIL)))
           ((LAMBDA (R)
              (SETQ S2
                      (ADDSQ S2
                             (RESIDUUM P Q Q1 VAR (PREPSQ (CAR R)) (CDR R)))))
            (CAR R))
           (SETQ R (CDR R))
           (GO LAB))
         (SETQ S2 (LIST 'TIMES 'PI (PREPSQ S2) (LIST 'COT K1)))))
       (T (SETQ S2 0)))
      (RETURN (SIMP* (REVAL1 (LIST 'DIFFERENCE S1 S2) NIL)))
     T2
      (RETURN NIL)
     T3
      (ERROR 99 'FAILED))) 
(PUT 'POLANALYZ 'NUMBER-OF-ARGS 2) 
(PUT 'POLANALYZ 'DEFINED-ON-LINE '618) 
(PUT 'POLANALYZ 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'POLANALYZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLANALYZ (EXP VAR)
    (COND
     ((POLTSTP EXP)
      ((LAMBDA (EX2)
         (COND
          ((EQCAR
            (SETQ EXP
                    (VARPWRTST (COND ((EQCAR EX2 'TIMES) (CADR EX2)) (T EX2))
                     VAR))
            'QUOTIENT)
           EXP)
          (T 0)))
       (COND ((EQCAR EXP 'MINUS) (CADR EXP)) (T EXP)))))) 
(PUT 'POLTSTP 'NUMBER-OF-ARGS 1) 
(PUT 'POLTSTP 'DEFINED-ON-LINE '626) 
(PUT 'POLTSTP 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'POLTSTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLTSTP (EXP)
    (OR (AND (ATOM EXP) EXP) (MEMBER (CAR EXP) DOMAINLIST*)
        (AND (MEMBER (CAR EXP) '(PLUS TIMES QUOTIENT MINUS EXPT SQRT))
             (PROG (FG)
               (PROG (C)
                 (SETQ C (CDR EXP))
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C) (COND ((NOT (POLTSTP C)) (SETQ FG T)))) (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB))
               (RETURN (NULL FG)))))) 
(PUT 'EVALMAX 'NUMBER-OF-ARGS 2) 
(PUT 'EVALMAX 'DEFINED-ON-LINE '636) 
(PUT 'EVALMAX 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'EVALMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALMAX (A B)
    (COND ((AND (NUMBERP A) (NUMBERP B)) (MAX A B)) ((EVALGREATERP A B) A)
          (T B))) 
(PUT 'EVALPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'EVALPLUS 'DEFINED-ON-LINE '640) 
(PUT 'EVALPLUS 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'EVALPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALPLUS (A B)
    (COND ((AND (NUMBERP A) (NUMBERP B)) (PLUS A B))
          (T (PREPSQ (SIMP* (REVAL1 (LIST 'PLUS A B) NIL)))))) 
(PUT 'VARPWRTST 'NUMBER-OF-ARGS 2) 
(PUT 'VARPWRTST 'DEFINED-ON-LINE '644) 
(PUT 'VARPWRTST 'DEFINED-IN-FILE 'DEFINT/DEFINTX.RED) 
(PUT 'VARPWRTST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VARPWRTST (EXP VAR)
    (COND ((ATOM EXP) (COND ((EQUAL EXP VAR) 1) (T 0)))
          ((EQ (CAR EXP) 'MINUS) (VARPWRTST (CADR EXP) VAR))
          ((MEMBER (CAR EXP) '(PLUS |,| DIFFERENCE))
           ((LAMBDA (Q)
              (PROGN
               (PROG (C)
                 (SETQ C (CDR EXP))
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C) (SETQ Q (EVALMAX Q (VARPWRTST C VAR)))) (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB))
               Q))
            0))
          ((EQCAR EXP 'EXPT)
           (PREPSQ
            (SIMP*
             (REVAL1 (LIST 'TIMES (VARPWRTST (CADR EXP) VAR) (CADDR EXP))
                     NIL))))
          ((EQCAR EXP 'SQRT)
           (PREPSQ
            (SIMP*
             (REVAL1
              (LIST 'TIMES (VARPWRTST (CADR EXP) VAR) (LIST 'QUOTIENT 1 2))
              NIL))))
          ((EQCAR EXP 'TIMES)
           ((LAMBDA (Q)
              (PROGN
               (PROG (C)
                 (SETQ C (CDR EXP))
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C) (SETQ Q (EVALPLUS Q (VARPWRTST C VAR)))) (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB))
               Q))
            0))
          (T 0))) 
(ENDMODULE) 