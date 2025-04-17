(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(SETK 'LOGCOMPLEX
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'PLUS (LIST '~ 'X) 'I))
                   (LIST 'PLUS
                         (LIST 'LOG
                               (LIST 'SQRT (LIST 'PLUS (LIST 'EXPT 'X 2) 1)))
                         (LIST 'TIMES 'I
                               (LIST 'ATAN
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'EXPT 'X 2)
                                                             1)))
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT 'X
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST 'EXPT
                                                                         'X 2)
                                                                   1)))))))))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'DIFFERENCE (LIST '~ 'X) 'I))
                   (LIST 'DIFFERENCE
                         (LIST 'LOG
                               (LIST 'SQRT (LIST 'PLUS (LIST 'EXPT 'X 2) 1)))
                         (LIST 'TIMES 'I
                               (LIST 'ATAN
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'EXPT 'X 2)
                                                             1)))
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT 'X
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST 'EXPT
                                                                         'X 2)
                                                                   1)))))))))
             (LIST 'REPLACEBY
                   (LIST 'LOG
                         (LIST 'PLUS (LIST '~ 'X)
                               (LIST 'TIMES 'I (LIST '~ 'Y))))
                   (LIST 'PLUS
                         (LIST 'LOG
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'TIMES 'X 'X)
                                           (LIST 'TIMES 'Y 'Y))))
                         (LIST 'TIMES 'I
                               (LIST 'ATAN
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT 'Y
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y))))
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT 'X
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST 'TIMES
                                                                         'X 'X)
                                                                   (LIST 'TIMES
                                                                         'Y
                                                                         'Y))))))))))
             (LIST 'REPLACEBY
                   (LIST 'LOG
                         (LIST 'DIFFERENCE (LIST '~ 'X)
                               (LIST 'TIMES 'I (LIST '~ 'Y))))
                   (LIST 'DIFFERENCE
                         (LIST 'LOG
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'TIMES 'X 'X)
                                           (LIST 'TIMES 'Y 'Y))))
                         (LIST 'TIMES 'I
                               (LIST 'ATAN
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT 'Y
                                                 (LIST 'SQRT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'X
                                                                   'X)
                                                             (LIST 'TIMES 'Y
                                                                   'Y))))
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT 'X
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST 'TIMES
                                                                         'X 'X)
                                                                   (LIST 'TIMES
                                                                         'Y
                                                                         'Y))))))))))
             (LIST 'REPLACEBY
                   (LIST 'LOG (LIST 'QUOTIENT (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'DIFFERENCE (LIST 'LOG 'X) (LIST 'LOG 'Y)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'SQRT (LIST '~ 'X)))
                   (LIST 'QUOTIENT (LIST 'LOG 'X) 2))
             (LIST 'REPLACEBY (LIST 'LOG (MINUS 1)) (LIST 'TIMES 'I 'PI))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'MINUS 'I))
                   (LIST 'MINUS (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))))
             (LIST 'REPLACEBY (LIST 'LOG 'I)
                   (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'MINUS (LIST '~ 'X)))
                   (LIST 'PLUS (LIST 'TIMES 'I 'PI) (LIST 'LOG 'X)))
             (LIST 'REPLACEBY
                   (LIST 'LOG (LIST 'MINUS (LIST 'TIMES 'I (LIST '~ 'X))))
                   (LIST 'PLUS
                         (LIST 'MINUS (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2)))
                         (LIST 'LOG 'X)))
             (LIST 'REPLACEBY (LIST 'LOG (LIST 'TIMES 'I (LIST '~ 'X)))
                   (LIST 'PLUS (LIST 'TIMES 'I (LIST 'QUOTIENT 'PI 2))
                         (LIST 'LOG 'X)))))) 
(AEVAL 'NIL) 
(PUT 'EVALPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'EVALPLUS 'DEFINED-ON-LINE '83) 
(PUT 'EVALPLUS 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'EVALPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALPLUS (A B)
    (COND ((AND (NUMBERP A) (NUMBERP B)) (PLUS A B))
          (T (PREPSQ (SIMP* (REVAL1 (LIST 'PLUS A B) NIL)))))) 
(PUT 'MY_PLUS 'NUMBER-OF-ARGS 2) 
(FLAG '(MY_PLUS) 'OPFN) 
(PUT 'MY_PLUS 'DEFINED-ON-LINE '87) 
(PUT 'MY_PLUS 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'MY_PLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MY_PLUS (A B) (EVALPLUS A B)) 
(PUT 'EVALMAX 'NUMBER-OF-ARGS 2) 
(PUT 'EVALMAX 'DEFINED-ON-LINE '89) 
(PUT 'EVALMAX 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'EVALMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALMAX (A B)
    (COND ((AND (NUMBERP A) (NUMBERP B)) (MAX A B)) ((EVALGREATERP A B) A)
          (T B))) 
(PUT 'MY_MAX 'NUMBER-OF-ARGS 2) 
(FLAG '(MY_MAX) 'OPFN) 
(PUT 'MY_MAX 'DEFINED-ON-LINE '93) 
(PUT 'MY_MAX 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'MY_MAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MY_MAX (A B) (EVALMAX A B)) 
(PUT 'CONVERT 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT 'DEFINED-ON-LINE '98) 
(PUT 'CONVERT 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'CONVERT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT (EXP)
    (PROG (TEMP SOLUTION ANSWER)
      (COND ((FREEOF EXP 'LOG_SUM) (RETURN EXP))
            (T
             (PROGN
              (COND ((NULL EXP) (RETURN NIL))
                    (T
                     (PROGN
                      (COND
                       ((EQUAL (CAR EXP) 'LOG_SUM)
                        (PROGN
                         (SETQ TEMP (CADDR EXP))
                         (COND
                          ((NOT (FREEOF TEMP 'BETA))
                           (PROGN
                            (SETQ TEMP
                                    (SUBEVAL
                                     (LIST (LIST 'EQUAL 'BETA 'ALPHA) TEMP)))
                            (SETQ EXP
                                    (SUBEVAL
                                     (LIST (LIST 'EQUAL 'BETA 'ALPHA) EXP)))
                            NIL)))
                         (SETQ TEMP (REVAL1 TEMP T))
                         (SETQ EXP (REVAL1 EXP T))
                         (COND
                          ((GREATERP (DEG TEMP 'ALPHA) 2)
                           (PROGN
                            (REDERR "cannot convert to radicals, degree > 2")
                            NIL))
                          (T
                           (PROGN
                            (COND
                             ((EQUAL (DEG TEMP 'ALPHA) 2)
                              (PROGN
                               (SETQ SOLUTION
                                       (AEVAL
                                        (LIST 'SOLVE (LIST 'EQUAL TEMP 0)
                                              'ALPHA)))
                               (SETQ ANSWER
                                       (SUBEVAL
                                        (LIST (REVAL1 (CADR SOLUTION) T)
                                              (AEVAL
                                               (LIST 'REVAL
                                                     (LIST 'PART EXP 4))))))
                               (SETQ ANSWER
                                       (LIST 'PLUS ANSWER
                                             (SUBEVAL
                                              (LIST (REVAL1 (CADDR SOLUTION) T)
                                                    (AEVAL
                                                     (LIST 'REVAL
                                                           (LIST 'PART EXP
                                                                 4)))))))
                               (SETQ ANSWER (REVAL1 ANSWER T))
                               NIL))))))))
                       (T
                        (PROGN
                         (COND ((NULL (CDR EXP)) (RETURN (CONVERT (CAR EXP))))
                               (T (RETURN (CONVERT (CDR EXP)))))
                         NIL)))))))))
      (RETURN ANSWER))) 
(FLAG '(CONVERT) 'OPFN) 
(PUT 'SEPARATE_REAL 'NUMBER-OF-ARGS 1) 
(PUT 'SEPARATE_REAL 'DEFINED-ON-LINE '151) 
(PUT 'SEPARATE_REAL 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'SEPARATE_REAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEPARATE_REAL (EXP)
    (PROG (RE IM ANS)
      (SETQ RE (AEVAL (LIST 'REPART EXP)))
      (SETQ IM (AEVAL (LIST 'IMPART EXP)))
      (SETQ ANS (CONS 'LIST (LIST RE IM)))
      (SETQ ANS (REVAL1 ANS T))
      (RETURN ANS))) 
(PUT 'SEP 'NUMBER-OF-ARGS 1) 
(FLAG '(SEP) 'OPFN) 
(PUT 'SEP 'DEFINED-ON-LINE '160) 
(PUT 'SEP 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'SEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEP (EXP) (SEPARATE_REAL EXP)) 
(PUT 'CONVERT_LOG 'NUMBER-OF-ARGS 2) 
(PUT 'CONVERT_LOG 'DEFINED-ON-LINE '163) 
(PUT 'CONVERT_LOG 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'CONVERT_LOG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONVERT_LOG (EXP VAR)
    (PROG (SEPP RE IM ANSWER)
      (SETK (LIST 'REPART VAR) VAR)
      (SETK (LIST 'IMPART VAR) 0)
      (COND
       ((EQUAL (CAR EXP) 'LOG)
        (PROGN
         (SETQ SEPP (SEPARATE_REAL EXP))
         (SETQ RE (CAR EXP))
         (SETQ IM (CADR EXP))
         (SETQ ANSWER (LOGTOTAN1 RE IM VAR))
         (RETURN ANSWER)
         NIL))
       (T NIL)))) 
(FLAG '(CONVERT_LOG) 'OPFN) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'LOGTOATAN 'NUMBER-OF-ARGS 3) 
(FLAG '(LOGTOATAN) 'OPFN) 
(PUT 'LOGTOATAN 'DEFINED-ON-LINE '198) 
(PUT 'LOGTOATAN 'DEFINED-IN-FILE 'RATINT/CONVERT.RED) 
(PUT 'LOGTOATAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOGTOATAN (EXP1 EXP2 X)
    (PROG (D C G TEMP REM)
      (SETQ REM (AEVAL (LIST 'PSEUDOREM EXP1 EXP2 X)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART REM 1)) 0)
        (RETURN
         (AEVAL (LIST 'TIMES 2 (LIST 'ATAN (LIST 'QUOTIENT EXP1 EXP2))))))
       (T
        (PROGN
         (COND
          ((EVALLESSP (AEVAL (LIST 'DEG EXP1 X)) (AEVAL (LIST 'DEG EXP2 X)))
           (RETURN (AEVAL (LIST 'LOGTOATAN (LIST 'MINUS EXP2) EXP1 X))))
          (T
           (PROGN
            (SETQ TEMP (AEVAL (LIST 'GCD_EX EXP2 (LIST 'MINUS EXP1) X)))
            (SETQ D (AEVAL (LIST 'PART TEMP 1)))
            (SETQ C (AEVAL (LIST 'PART TEMP 2)))
            (SETQ G
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'TIMES EXP2 D)
                           (LIST 'TIMES EXP1 C))))
            (RETURN
             (AEVAL
              (LIST 'PLUS
                    (LIST 'TIMES 2
                          (LIST 'ATAN
                                (LIST 'QUOTIENT
                                      (LIST 'PLUS (LIST 'TIMES EXP1 D)
                                            (LIST 'TIMES EXP2 C))
                                      G)))
                    (LIST 'LOGTOATAN D C X))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))))) 