(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFCONSTS)) 
(FLUID '(|INTLOG:REM|)) 
(PUT 'RD_CATALAN* 'NUMBER-OF-ARGS 0) 
(PUT 'RD_CATALAN* 'DEFINED-ON-LINE '83) 
(PUT 'RD_CATALAN* 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT 'RD_CATALAN* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RD_CATALAN* NIL
    (PROG (II J BJ P TT S)
      (SETQ II 1)
      (SETQ J 1)
      (SETQ S (SETQ TT (SETQ P BFHALF*)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (|GREATERP:| TT RD-TOLERANCE*)) (RETURN NIL)))
        (PROGN
         (SETQ J (PLUS J 2))
         (SETQ BJ (CONS '|:RD:| (CONS J 0)))
         (SETQ P
                 (NORMBF
                  (|DIVIDE:| (|TIMES:| P (CONS '|:RD:| (CONS II 0))) BJ
                             |:BPREC:|)))
         (SETQ TT
                 (NORMBF
                  (|DIVIDE:|
                   (|PLUS:| (|TIMES:| TT (CONS '|:RD:| (CONS II 0))) P) BJ
                   |:BPREC:|)))
         (SETQ S (|PLUS:| S TT))
         (SETQ II (PLUS II 1)))
        (GO WHILELABEL))
      (RETURN S))) 
(PUT 'CR_CATALAN* 'NUMBER-OF-ARGS 0) 
(PUT 'CR_CATALAN* 'DEFINED-ON-LINE '100) 
(PUT 'CR_CATALAN* 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT 'CR_CATALAN* 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CR_CATALAN* NIL (MKCR (RD_CATALAN*) (RDZERO*))) 
(AEVAL (FLAG '(|COMPUTE:INTLOG|) 'OPFN)) 
(PUT '|COMPUTE:KHINCHIN1| 'NUMBER-OF-ARGS 0) 
(FLAG '(|COMPUTE:KHINCHIN1|) 'OPFN) 
(PUT '|COMPUTE:KHINCHIN1| 'DEFINED-ON-LINE '121) 
(PUT '|COMPUTE:KHINCHIN1| 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT '|COMPUTE:KHINCHIN1| 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE |COMPUTE:KHINCHIN1| NIL
    (PROG (TERM SUMM ACC K LN2 LN3 OLDPREC ZP)
      (COND
       ((BOOLVALUE*
         (REVALX (LIST 'EVENP (SETQ OLDPREC (REVALX (LIST 'PRECISION 0))))))
        (AEVAL (LIST 'PRECISION (LIST 'PLUS OLDPREC 13))))
       (T (AEVAL (LIST 'PRECISION (LIST 'PLUS OLDPREC 12)))))
      (SETQ ACC
              (AEVAL
               (LIST 'EXPT 10 (LIST 'DIFFERENCE (LIST 'MINUS OLDPREC) 3))))
      (SETQ LN2 (AEVAL (LIST 'LOG 2)))
      (SETQ LN3 (AEVAL (LIST 'LOG 3)))
      (SETQ K (AEVAL 2))
      (SETQ TERM (AEVAL 1))
      (SETQ SUMM (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACC))
             (PROGN
              (SETQ ZP (AEVAL* (LIST '|COMPUTE:ZETAPRIME| K)))
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES (LIST 'EXPT (MINUS 1) K)
                             (LIST 'QUOTIENT
                                   (LIST 'DIFFERENCE (LIST 'TIMES 2 ZP)
                                         (LIST 'TIMES (LIST 'EXPT 2 K)
                                               (LIST 'PLUS ZP
                                                     (LIST 'QUOTIENT LN2
                                                           (LIST 'EXPT 2 K))
                                                     (LIST 'QUOTIENT LN3
                                                           (LIST 'EXPT 3 K)))))
                                   K))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
      (SETQ SUMM
              (AEVAL
               (LIST 'EXPT 'E
                     (LIST 'PLUS (LIST 'QUOTIENT SUMM LN2)
                           (LIST 'TIMES (LIST 'QUOTIENT LN3 LN2)
                                 (LIST 'DIFFERENCE
                                       (LIST 'QUOTIENT '(|:RD:| 2 . 0) 3)
                                       (LIST 'LOG (LIST 'QUOTIENT 5 3))))
                           (LIST 'DIFFERENCE 1 LN2)))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL SUMM)))) 
(PUT '|COMPUTE:ZETAPRIME| 'NUMBER-OF-ARGS 1) 
(FLAG '(|COMPUTE:ZETAPRIME|) 'OPFN) 
(PUT '|COMPUTE:ZETAPRIME| 'DEFINED-ON-LINE '143) 
(PUT '|COMPUTE:ZETAPRIME| 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT '|COMPUTE:ZETAPRIME| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |COMPUTE:ZETAPRIME| (U)
    (PROG (TERM SUMM N ACC F J LOGM M OLDPREC)
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'PLUS OLDPREC 5)))
      (SETQ N (AEVAL U))
      (SETQ |INTLOG:REM| NIL)
      (SETQ F
              (AEVAL
               (LIST 'MINUS
                     (LIST 'QUOTIENT
                           (LIST 'DF
                                 (LIST 'QUOTIENT (LIST 'LOG 'X)
                                       (LIST 'EXPT 'X N))
                                 'X)
                           2))))
      (SETQ M (AEVAL (LIST 'QUOTIENT (LIST 'PLUS OLDPREC 5) 2)))
      (SETQ LOGM (AEVAL (LIST 'LOG M)))
      (SETQ TERM (AEVAL LOGM))
      (SETQ ACC
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'EXPT 10
                           (LIST 'DIFFERENCE 1 (LIST 'PLUS OLDPREC 5)))
                     2)))
      (SETQ J (AEVAL 1))
      (SETQ SUMM
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'DIFFERENCE
                           (LIST 'MINUS
                                 (PROG (II FORALL-RESULT)
                                   (SETQ II 2)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE
                                            (AEVAL*
                                             (LIST 'DIFFERENCE (LIST 'FIX M)
                                                   1))
                                            II))
                                     (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (AEVAL*
                                            (LIST 'PLUS
                                                  (AEVAL*
                                                   (LIST 'QUOTIENT
                                                         (LIST
                                                          '|COMPUTE:INTLOG| II)
                                                         (LIST 'EXPT II N)))
                                                  FORALL-RESULT)))
                                   (SETQ II
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            II))
                                   (GO LAB1)))
                           (LIST 'QUOTIENT
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS LOGM
                                             (LIST 'QUOTIENT 1
                                                   (LIST 'DIFFERENCE N 1)))
                                       (LIST 'EXPT M (LIST 'DIFFERENCE N 1)))
                                 (LIST 'DIFFERENCE N 1)))
                     (LIST 'QUOTIENT (LIST 'QUOTIENT LOGM 2)
                           (LIST 'EXPT M N)))))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACC))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES (LIST 'BERNOULLI (LIST 'TIMES 2 J))
                             (LIST 'SUB (LIST 'EQUAL (LIST 'LOG 'X) LOGM)
                                   (LIST 'EQUAL 'X M) F))))
              (SETQ F
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'DF F 'X 'X)
                             (LIST 'PLUS
                                   (LIST 'TIMES
                                         (LIST 'PLUS (LIST 'TIMES 4 J) 6) J)
                                   2))))
              (SETQ SUMM (AEVAL* (LIST 'DIFFERENCE SUMM TERM)))
              (SETQ J (AEVAL* (LIST 'PLUS J 1)))
              (AEVAL* 'NIL)))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL SUMM)))) 
(PUT '|COMPUTE:INTLOG| 'NUMBER-OF-ARGS 1) 
(PUT '|COMPUTE:INTLOG| 'DEFINED-ON-LINE '168) 
(PUT '|COMPUTE:INTLOG| 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT '|COMPUTE:INTLOG| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |COMPUTE:INTLOG| (N)
    ((LAMBDA (FOUND)
       (COND ((SETQ FOUND (ATSOC N |INTLOG:REM|)) (CDR FOUND))
             (T
              (PROGN
               (SETQ FOUND (|COMPUTE:INTLOG1| N))
               (SETQ |INTLOG:REM| (CONS (CONS N FOUND) |INTLOG:REM|))
               FOUND))))
     NIL)) 
(PUT '|COMPUTE:INTLOG1| 'NUMBER-OF-ARGS 1) 
(PUT '|COMPUTE:INTLOG1| 'DEFINED-ON-LINE '175) 
(PUT '|COMPUTE:INTLOG1| 'DEFINED-IN-FILE 'SPECFN/SFCONSTS.RED) 
(PUT '|COMPUTE:INTLOG1| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |COMPUTE:INTLOG1| (N)
    ((LAMBDA (DIV)
       (COND
        ((OR (EQUAL N 2) (EQUAL N 3) (EQUAL N 4) (EQUAL N 5) (EQUAL N 7))
         (RDLOG* (CONS '|:RD:| (CONS N 0))))
        ((IEQUAL (CDR (SETQ DIV (DIVIDE N 2))) 0)
         (|RD:PLUS| (|COMPUTE:INTLOG| 2) (|COMPUTE:INTLOG| (CAR DIV))))
        ((IEQUAL (CDR (SETQ DIV (DIVIDE N 3))) 0)
         (|RD:PLUS| (|COMPUTE:INTLOG| 3) (|COMPUTE:INTLOG| (CAR DIV))))
        ((IEQUAL (CDR (SETQ DIV (DIVIDE N 5))) 0)
         (|RD:PLUS| (|COMPUTE:INTLOG| 5) (|COMPUTE:INTLOG| (CAR DIV))))
        ((IEQUAL (CDR (SETQ DIV (DIVIDE N 7))) 0)
         (|RD:PLUS| (|COMPUTE:INTLOG| 7) (|COMPUTE:INTLOG| (CAR DIV))))
        (T (RDLOG* (CONS '|:RD:| (CONS N 0))))))
     NIL)) 
(AEVAL 'NIL) 
(ENDMODULE) 