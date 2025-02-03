(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(CREATE-PACKAGE '(RATINT CONVERT) NIL) 
(GLOBAL '(*TRACERATINT)) 
(SWITCH (LIST 'TRACERATINT)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'MAKE_MON 'NUMBER-OF-ARGS 2) 
(FLAG '(MAKE_MON) 'OPFN) 
(PUT 'MAKE_MON 'DEFINED-ON-LINE '82) 
(PUT 'MAKE_MON 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'MAKE_MON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_MON (LI VAR)
    (PROG (CURRENT LI2)
      (SETQ LI2 (AEVAL (LIST 'LIST)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH LI)) K))
          (RETURN NIL)))
        (PROGN
         (SETQ CURRENT (AEVAL* (LIST 'PART LI K)))
         (SETQ CURRENT (AEVAL* (LIST 'MONIC CURRENT VAR)))
         (COND
          ((NOT (MEMBER (REVALX CURRENT) (REVALX LI2)))
           (SETQ LI2 (AEVAL* (LIST 'CONS CURRENT LI2)))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL (LIST 'REVERSE LI2))))) 
(PUT 'MONIC 'NUMBER-OF-ARGS 2) 
(FLAG '(MONIC) 'OPFN) 
(PUT 'MONIC 'DEFINED-ON-LINE '96) 
(PUT 'MONIC 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'MONIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MONIC (EXP VAR)
    (PROG (LECOF TEMP)
      (SETQ LECOF (AEVAL (LIST 'LCOF EXP VAR)))
      (SETQ EXP (AEVAL (LIST 'QUOTIENT EXP LECOF)))
      (RETURN (AEVAL EXP)))) 
(AEVAL 'NIL) 
(PUT 'GCD_EX 'NUMBER-OF-ARGS 3) 
(FLAG '(GCD_EX) 'OPFN) 
(PUT 'GCD_EX 'DEFINED-ON-LINE '120) 
(PUT 'GCD_EX 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'GCD_EX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GCD_EX (A B VAR)
    (PROG (C C1 C2 D D1 D2 Q R M R1 R2 G S)
      (AEVAL (ON (LIST 'RATIONAL)))
      (SETQ C (AEVAL (LIST 'NORM A VAR)))
      (SETQ D (AEVAL (LIST 'NORM B VAR)))
      (SETQ C1 (AEVAL 1))
      (SETQ D1 (AEVAL 0))
      (SETQ C2 (AEVAL 0))
      (SETQ D2 (AEVAL 1))
      (WHILE (EVALNEQ (AEVAL* D) 0)
             (PROGN
              (AEVAL* (ON (LIST 'RATIONAL)))
              (SETQ M (AEVAL* (LIST 'PSEUDOREM C D VAR)))
              (SETQ Q
                      (AEVAL*
                       (LIST 'QUOTIENT (LIST 'PART (LIST 'PSEUDOREM C D VAR) 3)
                             (LIST 'PART (LIST 'PSEUDOREM C D VAR) 2))))
              (SETQ R (AEVAL* (LIST 'DIFFERENCE C (LIST 'TIMES Q D))))
              (SETQ R1 (AEVAL* (LIST 'DIFFERENCE C1 (LIST 'TIMES Q D1))))
              (SETQ R2 (AEVAL* (LIST 'DIFFERENCE C2 (LIST 'TIMES Q D2))))
              (SETQ C (AEVAL* D))
              (SETQ C1 (AEVAL* D1))
              (SETQ C2 (AEVAL* D2))
              (SETQ D (AEVAL* R))
              (SETQ D1 (AEVAL* R1))
              (SETQ D2 (AEVAL* R2))
              (AEVAL* 'NIL)))
      (SETQ S
              (AEVAL
               (LIST 'QUOTIENT C1
                     (LIST 'TIMES (LIST 'RATINT_U A VAR)
                           (LIST 'RATINT_U C VAR)))))
      (SETQ B
              (AEVAL
               (LIST 'QUOTIENT C2
                     (LIST 'TIMES (LIST 'RATINT_U B VAR)
                           (LIST 'RATINT_U C VAR)))))
      (RETURN (AEVAL (LIST 'LIST S B))))) 
(PUT 'RATINT_U 'NUMBER-OF-ARGS 2) 
(FLAG '(RATINT_U) 'OPFN) 
(PUT 'RATINT_U 'DEFINED-ON-LINE '144) 
(PUT 'RATINT_U 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'RATINT_U 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATINT_U (EXP VAR)
    (COND ((EVALNUMBERP (AEVAL EXP)) (AEVAL EXP))
          (T (AEVAL (LIST 'LCOF EXP VAR))))) 
(PUT 'REM_ZERO 'NUMBER-OF-ARGS 1) 
(FLAG '(REM_ZERO) 'OPFN) 
(PUT 'REM_ZERO 'DEFINED-ON-LINE '160) 
(PUT 'REM_ZERO 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'REM_ZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REM_ZERO (LI)
    (PROG (K J L LI1 LI2)
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH LI)) K))
          (RETURN NIL)))
        (PROGN
         (SETQ J (AEVAL* (LIST 'PART LI K)))
         (COND ((EVALNEQ (AEVAL* J) 0) (AEVAL* 'NIL))
               (T
                (PROGN
                 (SETQ LI1
                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L 1)
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE K 1))
                                    L))
                             (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS (AEVAL* (LIST 'PART LI L))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ L
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    L))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE K 1))
                                    L))
                             (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS (AEVAL* (LIST 'PART LI L)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ LI2
                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L (AEVAL* (LIST 'PLUS K 1)))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH LI))
                                    L))
                             (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS (AEVAL* (LIST 'PART LI L))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ L
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    L))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH LI))
                                    L))
                             (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS (AEVAL* (LIST 'PART LI L)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ LI (AEVAL* (LIST 'APPEND LI1 LI2)))
                 (AEVAL* 'NIL))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL LI)))) 
(PUT 'HR_MONIC_DEN 'NUMBER-OF-ARGS 2) 
(FLAG '(HR_MONIC_DEN) 'OPFN) 
(PUT 'HR_MONIC_DEN 'DEFINED-ON-LINE '179) 
(PUT 'HR_MONIC_DEN 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'HR_MONIC_DEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HR_MONIC_DEN (LI X)
    (PROG (*EXP *FACTOR Q LC)
      (AEVAL (ON (LIST 'EXP)))
      (SETQ LI
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R (GETRLIST (AEVAL LI)))
                (COND ((NULL R) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R)
                                    (PROGN
                                     (SETQ LC
                                             (AEVAL
                                              (LIST 'LCOF (LIST 'DEN R) X)))
                                     (AEVAL
                                      (LIST 'LIST
                                            (LIST 'QUOTIENT (LIST 'NUM R) LC)
                                            (LIST 'QUOTIENT (LIST 'DEN R)
                                                  LC)))))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (PROGN
                             (SETQ LC (AEVAL (LIST 'LCOF (LIST 'DEN R) X)))
                             (AEVAL
                              (LIST 'LIST (LIST 'QUOTIENT (LIST 'NUM R) LC)
                                    (LIST 'QUOTIENT (LIST 'DEN R) LC)))))
                          (CAR R))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL (ON (LIST 'FACTOR)))
      (SETQ LI
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R (GETRLIST (AEVAL LI)))
                (COND ((NULL R) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R)
                                    (PROGN
                                     (SETQ Q (AEVAL (LIST 'PART R 2)))
                                     (COND
                                      ((AND
                                        (EVALGREATERP
                                         (AEVAL (LIST 'ARGLENGTH Q)) (MINUS 1))
                                        (EVALEQUAL (AEVAL (LIST 'PART Q 0))
                                                   (AEVAL 'EXPT)))
                                       (AEVAL
                                        (LIST 'LIST (LIST 'PART R 1)
                                              (LIST 'PART Q 1)
                                              (LIST 'PART Q 2))))
                                      (T
                                       (AEVAL
                                        (LIST 'LIST (LIST 'PART R 1)
                                              (LIST 'PART Q 1) 1))))))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (PROGN
                             (SETQ Q (AEVAL (LIST 'PART R 2)))
                             (COND
                              ((AND
                                (EVALGREATERP (AEVAL (LIST 'ARGLENGTH Q))
                                              (MINUS 1))
                                (EVALEQUAL (AEVAL (LIST 'PART Q 0))
                                           (AEVAL 'EXPT)))
                               (AEVAL
                                (LIST 'LIST (LIST 'PART R 1) (LIST 'PART Q 1)
                                      (LIST 'PART Q 2))))
                              (T
                               (AEVAL
                                (LIST 'LIST (LIST 'PART R 1) (LIST 'PART Q 1)
                                      1))))))
                          (CAR R))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (AEVAL LI)))) 
(OPERATOR (LIST 'C 'RTOF 'V 'ALPHA)) 
(LOAD_PACKAGE (LIST 'ARNUM)) 
(PUT 'NOT_NUMBERP 'NUMBER-OF-ARGS 1) 
(FLAG '(NOT_NUMBERP) 'OPFN) 
(PUT 'NOT_NUMBERP 'DEFINED-ON-LINE '208) 
(PUT 'NOT_NUMBERP 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'NOT_NUMBERP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOT_NUMBERP (X)
    (COND ((NOT (EVALNUMBERP (AEVAL X))) (AEVAL 'T)) (T (AEVAL 'NIL)))) 
(PUT 'PREM 'NUMBER-OF-ARGS 3) 
(PUT 'PREM 'DEFINED-ON-LINE '300) 
(PUT 'PREM 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'PREM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREM (R V VAR)
    (PROG (D DR DV L N TT RULE_LIST M Q INPUT1 INPUT2 RR VV)
      (ON (LIST 'RATIONAL))
      (OFF (LIST 'FACTOR))
      (SETQ RR R)
      (SETQ VV V)
      (SETQ DR (DEG R VAR))
      (SETQ DV (DEG V VAR))
      (COND
       ((LEQ DV DR)
        (PROGN
         (SETQ L (REVAL1 (COEFFN V VAR DV) T))
         (SETQ V
                 (REVAL1
                  (LIST 'PLUS V
                        (LIST 'MINUS (LIST 'TIMES L (LIST 'EXPT VAR DV))))
                  T))
         NIL))
       (T (SETQ L 1)))
      (SETQ D (PLUS (DIFFERENCE DR DV) 1))
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (LEQ DV DR) (NEQ R 0))) (RETURN NIL)))
        (PROGN
         (SETQ TT
                 (REVAL1
                  (LIST 'TIMES (LIST 'EXPT VAR (DIFFERENCE DR DV)) V
                        (COEFFN R VAR DR))
                  T))
         (COND ((EQUAL DR 0) (SETQ R 0))
               (T
                (PROGN
                 (SETQ RULE_LIST (LIST 'REPLACEBY (LIST 'EXPT VAR DR) 0))
                 (UNWIND-PROTECT
                     (PROGN (LET (LIST RULE_LIST)) (SETQ R (REVAL1 R T)))
                   (CLEARRULES (LIST RULE_LIST)))
                 NIL)))
         (SETQ R (REVAL1 (LIST 'PLUS (LIST 'TIMES L R) (LIST 'MINUS TT)) T))
         (SETQ DR (DEG R VAR))
         (SETQ N (PLUS N 1))
         NIL)
        (GO WHILELABEL))
      (SETQ R (REVAL1 (LIST 'TIMES (LIST 'EXPT L (DIFFERENCE D N)) R) T))
      (SETQ M (REVAL1 (LIST 'EXPT L D) T))
      (SETQ INPUT1
              (REVAL1
               (LIST 'PLUS (LIST 'TIMES (LIST 'EXPT L D) RR) (LIST 'MINUS R))
               T))
      (SETQ INPUT2 VV)
      (SETQ Q (REVAL1 (LIST 'QUOTIENT INPUT1 INPUT2) T))
      (RETURN (LIST R M Q)))) 
(PUT 'PSEUDOREM 'NUMBER-OF-ARGS 3) 
(FLAG '(PSEUDOREM) 'OPFN) 
(PUT 'PSEUDOREM 'DEFINED-ON-LINE '344) 
(PUT 'PSEUDOREM 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'PSEUDOREM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PSEUDOREM (X Y VAR) (CONS 'LIST (PREM X Y VAR))) 
(OPERATOR (LIST 'A 'NEIL)) 
(PUT 'HOWY 'NUMBER-OF-ARGS 3) 
(FLAG '(HOWY) 'OPFN) 
(PUT 'HOWY 'DEFINED-ON-LINE '363) 
(PUT 'HOWY 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'HOWY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HOWY (P Q X)
    (PROG (PSEUDO QUO REM PP POLY_PART D MM B NN J K AA CC PSEUDO3 I QUO3 R
           PSEUDO2 EQN L NEIL1 SOL VAR1 TEMP VAR2 VAR3 TEST OUTPUT)
      (PROGN
       (REMPROP 'A 'KLIST)
       (REMPROP 'A 'KVALUE)
       (REMPROP 'C 'KLIST)
       (REMPROP 'C 'KVALUE)
       NIL)
      (SETQ PSEUDO (AEVAL (LIST 'PSEUDOREM P Q X)))
      (SETQ QUO
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART PSEUDO 3) (LIST 'PART PSEUDO 2))))
      (SETQ REM
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART PSEUDO 1) (LIST 'PART PSEUDO 2))))
      (SETQ POLY_PART (AEVAL QUO))
      (SETQ PP (AEVAL REM))
      (SETQ D (AEVAL (LIST 'GCD Q (LIST 'DF Q X))))
      (SETQ PSEUDO2 (AEVAL (LIST 'PSEUDOREM Q D X)))
      (SETQ B
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART PSEUDO2 3) (LIST 'PART PSEUDO2 2))))
      (SETQ MM (AEVAL (LIST 'DEG B X)))
      (SETQ NN (AEVAL (LIST 'DEG D X)))
      (SETQ AA
              (PROG (K FORALL-RESULT)
                (SETQ K 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE MM 1)) K))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES (LIST 'A K) (LIST 'EXPT X K)))
                               FORALL-RESULT)))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB1)))
      (SETQ CC
              (PROG (J FORALL-RESULT)
                (SETQ J 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE NN 1)) J))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES (LIST 'C J) (LIST 'EXPT X J)))
                               FORALL-RESULT)))
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (GO LAB1)))
      (SETQ VAR1
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE MM 1)) I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (AEVAL* (LIST 'A I)) NIL)))
               LOOPLABEL
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE MM 1)) I))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'A I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VAR2
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K 0)
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE NN 1)) K))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (AEVAL* (LIST 'C K)) NIL)))
               LOOPLABEL
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE NN 1)) K))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'C K)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VAR3 (AEVAL (LIST 'APPEND VAR1 VAR2)))
      (AEVAL (ON (LIST 'RATIONAL)))
      (SETQ PSEUDO3
              (AEVAL (LIST 'PSEUDOREM (LIST 'TIMES B (LIST 'DF D X)) D X)))
      (SETQ QUO3
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART PSEUDO3 3) (LIST 'PART PSEUDO3 2))))
      (SETQ TEMP (AEVAL (LIST 'TIMES B (LIST 'QUOTIENT (LIST 'DF D X) D))))
      (SETQ TEMP (AEVAL (LIST 'PSEUDOREM (LIST 'NUM TEMP) (LIST 'DEN TEMP) X)))
      (SETQ TEMP
              (AEVAL (LIST 'QUOTIENT (LIST 'PART TEMP 3) (LIST 'PART TEMP 2))))
      (SETQ R
              (AEVAL
               (LIST 'PLUS
                     (LIST 'DIFFERENCE (LIST 'TIMES B (LIST 'DF CC X))
                           (LIST 'TIMES CC TEMP))
                     (LIST 'TIMES D AA))))
      (PROG (K)
        (SETQ K 0)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS MM (LIST 'DIFFERENCE NN 1)))
                 K))
          (RETURN NIL)))
        (PROGN
         (SETK (LIST 'NEIL K)
               (AEVAL*
                (LIST 'DIFFERENCE (LIST 'COEFFN PP X K) (LIST 'COEFFN R X K))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ NEIL1
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K 0)
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE
                         (AEVAL* (LIST 'PLUS MM (LIST 'DIFFERENCE NN 1))) K))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (LIST 'EQUAL (LIST 'NEIL K) 0))
                                      NIL)))
               LOOPLABEL
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE
                         (AEVAL* (LIST 'PLUS MM (LIST 'DIFFERENCE NN 1))) K))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS (AEVAL* (LIST 'EQUAL (LIST 'NEIL K) 0)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SOL (AEVAL (LIST 'SOLVE NEIL1 VAR3)))
      (SETQ SOL (AEVAL (LIST 'FIRST SOL)))
      (SETQ AA (AEVAL (LIST 'SUB SOL AA)))
      (SETQ CC (AEVAL (LIST 'SUB SOL CC)))
      (SETK 'ANS1 (AEVAL (LIST 'QUOTIENT CC D)))
      (SETK 'ANS2 (AEVAL (LIST 'INT POLY_PART X)))
      (SETK 'ANS3 (AEVAL (LIST 'QUOTIENT AA B)))
      (SETQ OUTPUT (AEVAL (LIST 'LIST 'ANS1 'ANS2 'ANS3)))
      (RETURN (AEVAL OUTPUT)))) 
(PUT 'RI_NEWTON 'NUMBER-OF-ARGS 5) 
(FLAG '(RI_NEWTON) 'OPFN) 
(PUT 'RI_NEWTON 'DEFINED-ON-LINE '430) 
(PUT 'RI_NEWTON 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'RI_NEWTON 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RI_NEWTON (A P U1 W1 B)
    (PROG (ALPHA GAMMA EEA_RESULT S TT U W EF MODULUS C SIGMA SIGMA_TILDE TAU
           TAU_TILDE RE R QUO)
      (SETQ ALPHA (AEVAL (LIST 'LCOF A 'X)))
      (SETQ A (AEVAL (LIST 'TIMES ALPHA A)))
      (SETQ GAMMA (AEVAL ALPHA))
      (SETQ U1 (AEVAL (LIST 'RATINT_PHI U1 'X P)))
      (PROGN
       (ASSGNPRI (AEVAL "u1 is ") NIL 'FIRST)
       (ASSGNPRI (AEVAL U1) NIL 'LAST))
      (AEVAL (OFF (LIST 'MODULAR)))
      (SETQ W1 (AEVAL (LIST 'RATINT_PHI (LIST 'TIMES ALPHA W1) 'X P)))
      (AEVAL (OFF (LIST 'MODULAR)))
      (SETQ EEA_RESULT (AEVAL (LIST 'GCD_EX U1 W1 'X)))
      (AEVAL (ON (LIST 'MODULAR)))
      (AEVAL (LIST 'SETMOD P))
      (SETQ S (AEVAL (LIST 'PART EEA_RESULT 1)))
      (SETQ TT (AEVAL (LIST 'PART EEA_RESULT 2)))
      (AEVAL (ON (LIST 'MODULAR)))
      (AEVAL (LIST 'SETMOD P))
      (SETQ U (AEVAL (LIST 'REPLACE_LC U1 'X GAMMA)))
      (SETQ W (AEVAL (LIST 'REPLACE_LC W1 'X ALPHA)))
      (AEVAL (OFF (LIST 'MODULAR)))
      (SETQ EF (AEVAL (LIST 'DIFFERENCE A (LIST 'TIMES U W))))
      (AEVAL (OFF (LIST 'MODULAR)))
      (SETQ MODULUS (AEVAL P))
      (AEVAL (ON (LIST 'MODULAR)))
      (AEVAL (LIST 'SETMOD P))
      (WHILE
       (AND (EVALNEQ (AEVAL* EF) 0)
            (EVALLESSP (AEVAL* MODULUS) (AEVAL* (LIST 'TIMES 2 B GAMMA))))
       (PROGN
        (SETQ C (AEVAL* (LIST 'QUOTIENT EF MODULUS)))
        (SETQ SIGMA_TILDE (AEVAL* (LIST 'RATINT_PHI (LIST 'TIMES S C) 'X P)))
        (AEVAL* (OFF (LIST 'MODULAR)))
        (SETQ TAU_TILDE (AEVAL* (LIST 'RATINT_PHI (LIST 'TIMES TT C) 'X P)))
        (AEVAL* (OFF (LIST 'MODULAR)))
        (SETQ RE (AEVAL* (LIST 'PSEUDOREM SIGMA_TILDE W1 'X)))
        (SETQ R (AEVAL* (LIST 'QUOTIENT (LIST 'PART RE 1) (LIST 'PART RE 2))))
        (SETQ QUO
                (AEVAL* (LIST 'QUOTIENT (LIST 'PART RE 3) (LIST 'PART RE 2))))
        (SETQ SIGMA (AEVAL* RE))
        (SETQ TAU
                (AEVAL*
                 (LIST 'RATINT_PHI (LIST 'PLUS TAU_TILDE (LIST 'TIMES QUO U1))
                       'X P)))
        (AEVAL* (OFF (LIST 'MODULAR)))
        (SETQ U (AEVAL* (LIST 'PLUS U (LIST 'TIMES TAU MODULUS))))
        (SETQ W (AEVAL* (LIST 'PLUS W (LIST 'TIMES SIGMA MODULUS))))
        (SETQ EF (AEVAL* (LIST 'DIFFERENCE A (LIST 'TIMES U W))))
        (SETQ MODULUS (AEVAL* (LIST 'TIMES MODULUS P)))
        (AEVAL* 'NIL)))
      (COND
       ((EVALEQUAL (AEVAL EF) 0)
        (PROGN
         (SETQ U (AEVAL U))
         (SETQ W (AEVAL (LIST 'QUOTIENT W GAMMA)))
         (AEVAL 'NIL)))
       (T (AEVAL (REDERR (REVALX "nsfe")))))
      (RETURN (AEVAL (LIST 'LIST U W))))) 
(CLEAR (LIST 'P)) 
(PUT 'REPLACE_LC 'NUMBER-OF-ARGS 3) 
(FLAG '(REPLACE_LC) 'OPFN) 
(PUT 'REPLACE_LC 'DEFINED-ON-LINE '494) 
(PUT 'REPLACE_LC 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'REPLACE_LC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPLACE_LC (EXP VAR VAL)
    (PROG (LEAD_TERM NEW_LEAD_TERM RED)
      (SETQ LEAD_TERM (AEVAL (LIST 'LTERM EXP VAR)))
      (SETQ RED (AEVAL (LIST 'REDUCT EXP VAR)))
      (SETQ NEW_LEAD_TERM
              (AEVAL (LIST 'QUOTIENT LEAD_TERM (LIST 'LCOF EXP VAR))))
      (SETQ NEW_LEAD_TERM (AEVAL (LIST 'TIMES NEW_LEAD_TERM VAL)))
      (SETK 'NEW_EXP (AEVAL (LIST 'PLUS NEW_LEAD_TERM RED)))
      (RETURN (AEVAL 'NEW_EXP)))) 
(PUT 'POLYDI 'NUMBER-OF-ARGS 4) 
(FLAG '(POLYDI) 'OPFN) 
(PUT 'POLYDI 'DEFINED-ON-LINE '510) 
(PUT 'POLYDI 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'POLYDI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYDI (A B C X)
    (PROG (Q R SIGMA TAU S TT SOL SIGMA_TILDE TAU_TILDE G)
      (AEVAL (ON (LIST 'RATIONAL)))
      (SETQ G (AEVAL (LIST 'GCD A B)))
      (SETQ S (AEVAL (LIST 'PART (LIST 'GCD_EX A B X) 1)))
      (SETQ TT (AEVAL (LIST 'PART (LIST 'GCD_EX A B X) 2)))
      (SETQ SOL
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES (LIST 'TIMES S (LIST 'QUOTIENT C G)) A)
                     (LIST 'TIMES (LIST 'TIMES TT (LIST 'QUOTIENT C G)) B))))
      (SETQ SIGMA_TILDE (AEVAL (LIST 'TIMES S (LIST 'QUOTIENT C G))))
      (SETQ TAU_TILDE (AEVAL (LIST 'TIMES TT (LIST 'QUOTIENT C G))))
      (SETK 'RESULT
            (AEVAL (LIST 'PSEUDOREM SIGMA_TILDE (LIST 'QUOTIENT B G) X)))
      (SETQ Q
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART 'RESULT 3) (LIST 'PART 'RESULT 2))))
      (SETQ R
              (AEVAL
               (LIST 'QUOTIENT (LIST 'PART 'RESULT 1) (LIST 'PART 'RESULT 2))))
      (SETQ SIGMA (AEVAL R))
      (SETQ TAU
              (AEVAL
               (LIST 'PLUS TAU_TILDE (LIST 'TIMES Q (LIST 'QUOTIENT A G)))))
      (RETURN (AEVAL (LIST 'LIST SIGMA TAU))))) 
(PUT 'RATINT_PHI 'NUMBER-OF-ARGS 3) 
(FLAG '(RATINT_PHI) 'OPFN) 
(PUT 'RATINT_PHI 'DEFINED-ON-LINE '538) 
(PUT 'RATINT_PHI 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'RATINT_PHI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RATINT_PHI (EXP VAR P)
    (PROG (PRIME)
      (SETQ PRIME (AEVAL P))
      (COND
       ((PRIMEP (REVALX P))
        (PROGN
         (AEVAL (ON (LIST 'MODULAR)))
         (AEVAL (LIST 'SETMOD P))
         (SETQ EXP (AEVAL (LIST 'MOD EXP P)))
         (AEVAL 'NIL)))
       (T (AEVAL (REDERR (REVALX "p should be prime")))))
      (RETURN (AEVAL EXP)))) 
(PUT 'NN 'NUMBER-OF-ARGS 3) 
(FLAG '(NN) 'OPFN) 
(PUT 'NN 'DEFINED-ON-LINE '549) 
(PUT 'NN 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'NN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NN (EXP VAR P)
    (PROG (LCOEF)
      (SETQ LCOEF (AEVAL (LIST 'LCOF EXP VAR)))
      (COND
       ((PRIMEP (REVALX P))
        (PROGN
         (AEVAL (ON (LIST 'MODULAR)))
         (AEVAL (LIST 'SETMOD P))
         (SETQ EXP (AEVAL (LIST 'QUOTIENT EXP LCOEF)))
         (AEVAL 'NIL)))
       (T (AEVAL (REDERR (REVALX "p should be prime")))))
      (RETURN (AEVAL EXP)))) 
(LOAD_PACKAGE (LIST 'ARNUM)) 
(PUT 'LOG_SUM 'SIMPFN 'SIMPIDEN) 
(PUT 'RT 'NUMBER-OF-ARGS 3) 
(FLAG '(RT) 'OPFN) 
(PUT 'RT 'DEFINED-ON-LINE '568) 
(PUT 'RT 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'RT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RT (A B X)
    (PROG (VV J K I CURRENT SOL RES CC B_PRIME EXTRA_TERM CURRENT1 VVV INTEGRAL
           EQN D V_LIST SOL1 SOL2 TEMP TEMP2)
      (SETQ B_PRIME (AEVAL (LIST 'DF B X)))
      (SETQ V_LIST (AEVAL (LIST 'LIST)))
      (AEVAL (ON (LIST 'RATIONAL)))
      (SETQ RES
              (AEVAL
               (LIST 'RESULTANT (LIST 'DIFFERENCE A (LIST 'TIMES 'Z B_PRIME)) B
                     X)))
      (AEVAL (ON (LIST 'IFACTOR)))
      (SETQ RES (AEVAL (LIST 'OLD_FACTORIZE RES)))
      (WHILE
       (AND (EVALGREATERP (AEVAL* (LIST 'LENGTH RES)) 0)
            (EVALNUMBERP (AEVAL* (LIST 'FIRST RES))))
       (SETQ RES (AEVAL* (LIST 'REST RES))))
      (SETQ RES (AEVAL (LIST 'MAKE_MON RES 'Z)))
      (SETQ INTEGRAL (AEVAL 0))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH RES)) K))
          (RETURN NIL)))
        (PROGN
         (SETQ CURRENT (AEVAL* (LIST 'PART RES K)))
         (SETQ D (AEVAL* (LIST 'DEG CURRENT 'Z)))
         (COND
          ((EVALEQUAL (AEVAL* D) 1)
           (PROGN
            (SETQ SOL (AEVAL* (LIST 'SOLVE (LIST 'EQUAL CURRENT 0) 'Z)))
            (SETQ SOL (AEVAL* (LIST 'PART SOL 1)))
            (SETQ CC (AEVAL* (LIST 'PART SOL 2)))
            (SETQ VV
                    (AEVAL*
                     (LIST 'GCD (LIST 'DIFFERENCE A (LIST 'TIMES CC B_PRIME))
                           B)))
            (SETQ VV (AEVAL* (LIST 'QUOTIENT VV (LIST 'LCOF VV X))))
            (SETQ EXTRA_TERM
                    (AEVAL*
                     (LIST 'APPEND (LIST 'LIST CC)
                           (LIST 'LIST (LIST 'LOG VV)))))
            (SETQ EXTRA_TERM
                    (AEVAL*
                     (LIST 'TIMES (LIST 'PART EXTRA_TERM 1)
                           (LIST 'PART EXTRA_TERM 2))))
            (SETQ INTEGRAL (AEVAL* (LIST 'PLUS EXTRA_TERM INTEGRAL)))
            (COND
             ((BOOLVALUE* (REVALX *TRACERATINT))
              (PROGN
               (ASSGNPRI (AEVAL* "integral in Rothstein T is ") NIL 'FIRST)
               (ASSGNPRI (AEVAL* INTEGRAL) NIL 'LAST))))
            (AEVAL* 'NIL)))
          (T
           (PROGN
            (SETQ CURRENT (AEVAL* (LIST 'SUB (LIST 'EQUAL 'Z 'ALPHA) CURRENT)))
            (SETQ CURRENT1
                    (AEVAL* (LIST 'SUB (LIST 'EQUAL 'ALPHA 'ALP) CURRENT)))
            (AEVAL* (OFF (LIST 'MCD)))
            (SETQ CURRENT (AEVAL* CURRENT))
            (AEVAL* (DEFPOLY (LIST CURRENT)))
            (AEVAL* (ON (LIST 'MCD)))
            (SETQ A (AEVAL* (LIST 'SUB (LIST 'EQUAL X 'Z) A)))
            (SETQ B (AEVAL* (LIST 'SUB (LIST 'EQUAL X 'Z) B)))
            (SETQ B_PRIME (AEVAL* (LIST 'SUB (LIST 'EQUAL X 'Z) B_PRIME)))
            (SETQ VV
                    (AEVAL*
                     (LIST 'GCD
                           (LIST 'DIFFERENCE A (LIST 'TIMES 'ALPHA B_PRIME))
                           B)))
            (AEVAL* (OFF (LIST 'ARNUM)))
            (AEVAL* (ON (LIST 'FULLROOTS)))
            (SETQ VV (AEVAL* (LIST 'SUB (LIST 'EQUAL 'Z X) VV)))
            (AEVAL* (ON (LIST 'RATIONAL)))
            (AEVAL* (ON (LIST 'RATARG)))
            (COND
             ((EVALGREATERP (AEVAL* (LIST 'DEG VV X)) 2)
              (PROGN
               (SETQ INTEGRAL
                       (AEVAL*
                        (LIST 'PLUS INTEGRAL
                              (LIST 'LOG_SUM 'ALPHA_A CURRENT1 0
                                    (LIST 'TIMES 'ALPHA_A (LIST 'LOG VV)) X))))
               (SETQ INTEGRAL
                       (AEVAL*
                        (LIST 'SUB (LIST 'EQUAL 'ALPHA_A 'ALPHA) INTEGRAL)))
               (SETQ INTEGRAL
                       (AEVAL* (LIST 'SUB (LIST 'EQUAL 'ALP 'ALPHA) INTEGRAL)))
               (COND
                ((BOOLVALUE* (REVALX *TRACERATINT))
                 (PROGN
                  (ASSGNPRI (AEVAL* "integral in Rothstein T is ") NIL 'FIRST)
                  (ASSGNPRI (AEVAL* INTEGRAL) NIL 'LAST))))
               (AEVAL* 'NIL)))
             (T
              (PROGN
               (SETQ CURRENT
                       (AEVAL* (LIST 'SUB (LIST 'EQUAL 'ALPHA 'BETA) CURRENT)))
               (SETQ VV (AEVAL* (LIST 'SUB (LIST 'EQUAL 'ALPHA 'BETA) VV)))
               (SETQ INTEGRAL
                       (AEVAL*
                        (LIST 'PLUS INTEGRAL
                              (LIST 'LOG_SUM 'BETA CURRENT 0
                                    (LIST 'TIMES 'BETA (LIST 'LOG VV))))))
               (COND
                ((EVALEQUAL (AEVAL* D) 2)
                 (PROGN
                  (SETQ SOL
                          (AEVAL* (LIST 'SOLVE (LIST 'EQUAL CURRENT1 0) 'ALP)))
                  (SETQ SOL1 (AEVAL* (LIST 'PART SOL 1)))
                  (SETQ SOL2 (AEVAL* (LIST 'PART SOL 2)))
                  (SETK (LIST 'C 1) (AEVAL* (LIST 'PART SOL1 2)))
                  (SETK (LIST 'C 2) (AEVAL* (LIST 'PART SOL2 2)))
                  (PROG (J)
                    (SETQ J 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE 2 J)) (RETURN NIL)))
                    (PROGN
                     (SETK (LIST 'V J)
                           (AEVAL*
                            (LIST 'SUB (LIST 'EQUAL 'ALP (LIST 'C J)) EQN)))
                     (AEVAL* 'NIL))
                    (SETQ J (PLUS2 J 1))
                    (GO LAB))
                  (AEVAL* 'NIL)))
                (T
                 (PROGN
                  (SETQ K (AEVAL* 1))
                  (WHILE (EVALLEQ (AEVAL* K) (AEVAL* D))
                         (PROGN
                          (SETK (LIST 'C K) (AEVAL* (LIST 'RTOF CURRENT1)))
                          (SETK (LIST 'V K)
                                (AEVAL*
                                 (LIST 'SUB
                                       (LIST 'LIST
                                             (LIST 'EQUAL 'ALP (LIST 'C K)))
                                       EQN)))
                          (SETQ K (AEVAL* (LIST 'PLUS K 1)))
                          (AEVAL* 'NIL)))
                  (AEVAL* 'NIL))))
               (AEVAL* 'NIL))))
            (AEVAL* 'NIL))))
         (AEVAL* (NULL (REMPROP 'ALPHA 'CURREP)))
         (AEVAL* (NULL (REMPROP 'ALPHA 'IDVALFN)))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL INTEGRAL)))) 
(PUT 'DEPENDP 'NUMBER-OF-ARGS 2) 
(FLAG '(DEPENDP) 'OPFN) 
(PUT 'DEPENDP 'DEFINED-ON-LINE '678) 
(PUT 'DEPENDP 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'DEPENDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPENDP (EXP X)
    (COND
     ((AND (FREEOF (REVALX EXP) (REVALX X)) (NOT (EVALNUMBERP (AEVAL EXP))))
      (AEVAL 'NIL))
     (T (AEVAL 'T)))) 
(PUT 'RATINT 'NUMBER-OF-ARGS 3) 
(FLAG '(RATINT) 'OPFN) 
(PUT 'RATINT 'DEFINED-ON-LINE '685) 
(PUT 'RATINT 'DEFINED-IN-FILE 'RATINT/RATINT.RED) 
(PUT 'RATINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RATINT (P Q X)
    (PROG (S_LIST FIRST_TERM SECOND_TERM R_PART ANSWER)
      (COND
       ((AND (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDP P X))))
             (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDP Q X)))))
        (RETURN (AEVAL (LIST 'TIMES (LIST 'QUOTIENT P Q) X))))
       (T
        (PROGN
         (COND
          ((AND (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDP P X))))
                (BOOLVALUE* (REVALX (LIST 'DEPENDP Q X))))
           (RETURN (AEVAL (LIST 'TIMES P (LIST 'RATINT 1 Q X)))))
          (T
           (PROGN
            (COND
             ((AND (BOOLVALUE* (REVALX (LIST 'DEPENDP P X)))
                   (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDP Q X)))))
              (RETURN
               (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 1 Q) (LIST 'INT P X))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALNUMBERP (AEVAL P)) (EVALNUMBERP (AEVAL Q)))
        (RETURN (AEVAL (LIST 'TIMES (LIST 'QUOTIENT P Q) X)))))
      (COND
       ((BOOLVALUE* (REVALX *TRACERATINT))
        (PROGN
         (ASSGNPRI (AEVAL "performing Howoritz reduction on ") NIL 'FIRST)
         (ASSGNPRI (AEVAL (LIST 'QUOTIENT P Q)) NIL 'LAST))))
      (SETQ S_LIST (AEVAL (LIST 'HOWY P Q X)))
      (COND
       ((BOOLVALUE* (REVALX *TRACERATINT))
        (PROGN
         (ASSGNPRI (AEVAL "Howoritz gives: ") NIL 'FIRST)
         (ASSGNPRI (AEVAL S_LIST) NIL 'LAST))))
      (SETQ FIRST_TERM (AEVAL (LIST 'PART S_LIST 1)))
      (SETQ SECOND_TERM (AEVAL (LIST 'PART S_LIST 2)))
      (SETQ R_PART (AEVAL (LIST 'PART S_LIST 3)))
      (COND
       ((BOOLVALUE* (REVALX *TRACERATINT))
        (PROGN
         (ASSGNPRI (AEVAL "computing Rothstein Trager on ") NIL 'FIRST)
         (ASSGNPRI (AEVAL R_PART) NIL 'LAST))))
      (SETQ R_PART (AEVAL (LIST 'RT (LIST 'NUM R_PART) (LIST 'DEN R_PART) X)))
      (SETQ ANSWER
              (AEVAL (LIST 'LIST (LIST 'PLUS FIRST_TERM SECOND_TERM) R_PART)))
      (RETURN (AEVAL ANSWER)))) 