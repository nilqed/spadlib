(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LINREC)) 
(SWITCH (LIST 'TRLINREC)) 
(PUT 'RSOLVE 'NUMBER-OF-ARGS 2) 
(FLAG '(RSOLVE) 'OPFN) 
(PUT 'RSOLVE 'DEFINED-ON-LINE '44) 
(PUT 'RSOLVE 'DEFINED-IN-FILE 'SPECFN/LINREC.RED) 
(PUT 'RSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RSOLVE (REC IC)
    (PROG (MVAR1 INDE CONSTPART LOWESTIND MODIC MODREC INDIS)
      (SETQ MVAR1 (AEVAL (LIST 'PART (LIST 'MAINVAR REC) 0)))
      (SETQ INDE (AEVAL (LIST 'MAINVAR (LIST 'PART (LIST 'MAINVAR REC) 1))))
      (SETQ INDIS
              (AEVAL
               (LIST 'THE_INDICES
                     (PROG (KK FORALL-RESULT FORALL-ENDPTR)
                       (SETQ KK (GETRLIST (AEVAL IC)))
                       (COND ((NULL KK) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (KK) (AEVAL (LIST 'LHS KK)))
                                         (CAR KK))
                                        NIL)))
                      LOOPLABEL
                       (SETQ KK (CDR KK))
                       (COND ((NULL KK) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (KK) (AEVAL (LIST 'LHS KK))) (CAR KK))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     MVAR1)))
      (SETK 'HIGHESTIND
            (COND
             ((EVALNEQ (AEVAL INDIS) (AEVAL (LIST 'LIST)))
              (AEVAL (LIST 'MAX INDIS)))
             (T (AEVAL 'NIL))))
      (SETK 'NINDIS (AEVAL (LIST 'THE_INDICES REC MVAR1)))
      (SETQ CONSTPART
              (REVAL1
               (LIST 'WHEREEXP
                     (LIST 'LIST (LIST 'REPLACEBY (LIST MVAR1 (LIST '~ 'N)) 0))
                     REC)
               NIL))
      (COND
       ((NOT (FREEOF (REVALX CONSTPART) (REVALX INDE)))
        (AEVAL
         (REDERR
          (REVALX
           "Cant solve recurrence equations with non-constant coefficients")))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EQN CONSTPART 0)))
        (RETURN (AEVAL (LIST 'SOLVE_LIN_REC REC IC))))
       (T
        (PROGN
         (SETQ MODREC
                 (AEVAL
                  (LIST 'SUB (LIST 'EQUAL INDE (LIST 'PLUS INDE 1)) REC)))
         (SETQ MODREC (AEVAL (LIST 'DIFFERENCE MODREC REC)))
         (COND
          ((BOOLVALUE* (REVALX 'HIGHESTIND))
           (PROGN
            (SETQ MODIC
                    (AEVAL
                     (LIST 'SUB
                           (LIST 'EQUAL INDE
                                 (LIST 'PLUS
                                       (LIST 'DIFFERENCE INDE
                                             (LIST 'MAX 'NINDIS))
                                       'HIGHESTIND 1))
                           REC)))
            (PROG (AA)
              (SETQ AA (GETRLIST (AEVAL IC)))
             LAB
              (COND ((NULL AA) (RETURN NIL)))
              ((LAMBDA (AA) (SETQ MODIC (AEVAL (LIST 'SUB AA MODIC))))
               (CAR AA))
              (SETQ AA (CDR AA))
              (GO LAB))
            (SETQ MODIC
                    (AEVAL
                     (LIST 'CONS
                           (LIST 'FIRST
                                 (LIST 'SOLVE MODIC (LIST 'MAINVAR MODIC)))
                           IC)))))
          (T (SETQ MODIC (AEVAL IC))))
         (RETURN (AEVAL (LIST 'SOLVE_LIN_REC MODREC MODIC)))
         (AEVAL 'NIL)))))) 
(FLUID '(LINRECX* LINRECVAR*)) 
(PUT 'SOLVE_LIN_REC 'NUMBER-OF-ARGS 2) 
(FLAG '(SOLVE_LIN_REC) 'OPFN) 
(PUT 'SOLVE_LIN_REC 'DEFINED-ON-LINE '81) 
(PUT 'SOLVE_LIN_REC 'DEFINED-IN-FILE 'SPECFN/LINREC.RED) 
(PUT 'SOLVE_LIN_REC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE_LIN_REC (REC IC)
    (PROG (LREC SOL MSOL GSOL J FLAGG C LINRECVAR* ERRFLAG NSAVE)
      (AEVAL (CLEAR (LIST 'N)))
      (SETQ LINRECVAR* (AEVAL (LIST 'PART (LIST 'MAINVAR REC) 0)))
      (SETK 'LINRECX* (AEVAL (GENSYM)))
      (SETQ C (AEVAL (MKQUOTE (GENSYM))))
      (AEVAL (OPERATOR (LIST C)))
      (COND
       ((AND (EQ (REVALX (LIST 'PART REC 0)) (REVALX LINRECVAR*))
             (EVALEQUAL (AEVAL (LIST 'ARGLENGTH IC)) 1)
             (EVALEQUAL
              (AEVAL
               (LIST 'PART (LIST 'MAINVAR (LIST 'LHS (LIST 'FIRST IC))) 0))
              (AEVAL LINRECVAR*)))
        (RETURN (AEVAL (LIST 'RHS (LIST 'FIRST IC))))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'PART REC 0)) (AEVAL 'PLUS))
        (RETURN
         (AEVAL
          (REDERR
           (REVALX
            "Cant solve recurrence equations with non-constant coefficients"))))))
      (SETQ LREC (AEVAL (LIST 'ARGLENGTH REC)))
      (SETQ LREC (AEVAL (LIST 'PART REC LREC)))
      (AEVAL
       (FORALL
        (LIST '(N) 'T
              '(LET00
                (LIST
                 (LIST 'EQUAL (LIST LINRECVAR* 'N)
                       (LIST 'EXPT 'LINRECX* 'N)))))))
      (SETQ LREC (AEVAL LREC))
      (SETQ REC (AEVAL (LIST 'QUOTIENT REC LREC)))
      (AEVAL (FORALL (LIST '(N) 'T '(CLEAR (LIST (LIST LINRECVAR* 'N))))))
      (SETQ REC (AEVAL (LIST 'NUM REC)))
      (PROG (J)
        (SETQ J (GETRLIST (AEVAL (LIST 'COEFF REC 'LINRECX*))))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((NOT
              (FREEOF (REVALX J)
                      (REVALX (LIST 'PART (LIST 'PART (LIST 'PART REC) 1) 1))))
             (SETQ ERRFLAG (AEVAL 17)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL ERRFLAG) 17)
        (RETURN
         (AEVAL
          (REDERR
           (REVALX
            "Cant solve recurrence equations with non-constant coefficients"))))))
      (SETQ J (AEVAL 1))
      (PROG (A)
        (SETQ A (GETRLIST (AEVAL (LIST 'SOLVE REC 'LINRECX*))))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ A (AEVAL (LIST 'EXPT (LIST 'RHS A) 'N)))
            (SETQ GSOL (AEVAL (LIST 'PLUS GSOL (LIST 'TIMES (LIST C J) A))))
            (SETQ J (AEVAL (LIST 'PLUS J 1)))
            (SETQ MSOL (AEVAL (LIST 'FIRST MULTIPLICITIES*)))
            (SETQ MULTIPLICITIES*
                    (PROGN
                     (SETQ ALGLIST* (CONS NIL NIL))
                     (AEVAL (LIST 'REST MULTIPLICITIES*))))
            (WHILE (EVALGREATERP (AEVAL* MSOL) 1)
                   (PROGN
                    (SETQ A (AEVAL* (LIST 'TIMES 'N A)))
                    (SETQ GSOL
                            (AEVAL*
                             (LIST 'PLUS GSOL (LIST 'TIMES (LIST C J) A))))
                    (SETQ J (AEVAL* (LIST 'PLUS J 1)))
                    (SETQ MSOL (AEVAL* (LIST 'DIFFERENCE MSOL 1)))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX *TRLINREC))
        (PROGN
         (ASSGNPRI (AEVAL "General solution: ") NIL 'FIRST)
         (ASSGNPRI (AEVAL LINRECVAR*) NIL NIL)
         (ASSGNPRI (AEVAL "(N) := ") NIL NIL)
         (ASSGNPRI (AEVAL GSOL) NIL 'LAST))))
      (COND
       ((EVALEQUAL (AEVAL IC) (AEVAL (LIST 'LIST)))
        (SETQ SOL (AEVAL (LIST 'LIST))))
       (T
        (SETQ SOL
                (AEVAL
                 (LIST 'SOLVE
                       (PROG (A FORALL-RESULT FORALL-ENDPTR)
                         (SETQ A (GETRLIST (AEVAL IC)))
                         (COND ((NULL A) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (A)
                                             (AEVAL
                                              (LIST 'EQUAL
                                                    (LIST 'SUB
                                                          (LIST 'EQUAL 'N
                                                                (LIST 'PART
                                                                      (LIST
                                                                       'LHS A)
                                                                      1))
                                                          GSOL)
                                                    (LIST 'RHS A))))
                                           (CAR A))
                                          NIL)))
                        LOOPLABEL
                         (SETQ A (CDR A))
                         (COND ((NULL A) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (A)
                                     (AEVAL
                                      (LIST 'EQUAL
                                            (LIST 'SUB
                                                  (LIST 'EQUAL 'N
                                                        (LIST 'PART
                                                              (LIST 'LHS A) 1))
                                                  GSOL)
                                            (LIST 'RHS A))))
                                   (CAR A))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND
                          ((|AMINUSP:|
                            (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH IC)) I))
                           (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS (AEVAL* (LIST C I)) NIL)))
                        LOOPLABEL
                         (SETQ I
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  I))
                         (COND
                          ((|AMINUSP:|
                            (LIST 'DIFFERENCE (AEVAL* (LIST 'ARGLENGTH IC)) I))
                           (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST C I)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (SETQ SOL (AEVAL (SUBLA '((EQUAL . REPLACEBY)) SOL)))
      (SETQ SOL (AEVAL (SUBLA '((EQUAL . REPLACEBY)) SOL)))
      (AEVAL (LET (LIST SOL)))
      (SETQ GSOL (AEVAL GSOL))
      (AEVAL (CLEARRULES (LIST SOL)))
      (AEVAL (LET '(MOIVRE_EXPT)))
      (SETQ GSOL (AEVAL GSOL))
      (AEVAL (CLEARRULES (LIST 'MOIVRE_EXPT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* J) I)) (RETURN NIL)))
        (COND
         ((EVALEQUAL (AEVAL* (LIST 'COEFF GSOL (LIST C I)))
                     (AEVAL* (LIST 'LIST GSOL)))
          (AEVAL* 'NIL))
         (T
          (SETQ GSOL
                  (AEVAL*
                   (LIST 'SUB (LIST 'EQUAL (LIST C I) (CAAAR (MAKEARBCOMPLEX)))
                         GSOL)))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL GSOL)))) 
(SETK 'MOIVRE_EXPT
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST '~ 'Z) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MOIVRE 'Z 'K)
                         (LIST 'NOT (LIST 'FREEOF 'Z 'I))))))) 
(PUT 'MOIVRE 'NUMBER-OF-ARGS 2) 
(FLAG '(MOIVRE) 'OPFN) 
(PUT 'MOIVRE 'DEFINED-ON-LINE '155) 
(PUT 'MOIVRE 'DEFINED-IN-FILE 'SPECFN/LINREC.RED) 
(PUT 'MOIVRE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOIVRE (Z K)
    (PROG (RHO PHI)
      (SETQ RHO
              (AEVAL
               (LIST 'SQRT
                     (LIST 'PLUS (LIST 'EXPT (LIST 'REPART Z) 2)
                           (LIST 'EXPT (LIST 'IMPART Z) 2)))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART Z)) 0)
        (SETQ PHI (AEVAL (LIST 'QUOTIENT 'PI 2))))
       (T
        (SETQ PHI
                (AEVAL
                 (LIST 'ATAN
                       (LIST 'QUOTIENT (LIST 'IMPART Z) (LIST 'REPART Z)))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES (LIST 'EXPT RHO K)
              (LIST 'PLUS (LIST 'COS (LIST 'TIMES K PHI))
                    (LIST 'TIMES 'I (LIST 'SIN (LIST 'TIMES K PHI))))))))) 
(PUT 'THE_INDICES 'NUMBER-OF-ARGS 2) 
(FLAG '(THE_INDICES) 'OPFN) 
(PUT 'THE_INDICES 'DEFINED-ON-LINE '164) 
(PUT 'THE_INDICES 'DEFINED-IN-FILE 'SPECFN/LINREC.RED) 
(PUT 'THE_INDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE THE_INDICES (EX MVAR)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'PART EX 0)) (AEVAL 'LIST))
      (PROG (KK FORALL-RESULT FORALL-ENDPTR)
        (SETQ KK (GETRLIST (AEVAL EX)))
       STARTOVER
        (COND ((NULL KK) (RETURN (MAKELIST NIL))))
        (SETQ FORALL-RESULT
                ((LAMBDA (KK) (AEVAL (LIST 'THE_INDICES KK MVAR))) (CAR KK)))
        (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
        (SETQ KK (CDR KK))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL KK) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (GETRLIST
                 ((LAMBDA (KK) (AEVAL (LIST 'THE_INDICES KK MVAR))) (CAR KK))))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ KK (CDR KK))
        (GO LOOPLABEL)))
     (T
      (PROG (EQQ L1 L2 KERN)
        (SETQ EQQ (AEVAL EX))
        (SETQ KERN
                (UNION (KERNELS (*Q2F (CONS (CAR (SIMP EQQ)) 1)))
                       (KERNELS (*Q2F (CONS (CDR (SIMP EQQ)) 1)))))
        (SETQ L1
                (AEVAL
                 (CONS 'LIST
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K KERN)
                        STARTOVER
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 ((LAMBDA (K)
                                    (COND ((ATOM K) NIL)
                                          ((EQCAR K MVAR) (LIST (CADR K)))
                                          (T NIL)))
                                  (CAR K)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                         (SETQ K (CDR K))
                         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                        LOOPLABEL
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 ((LAMBDA (K)
                                    (COND ((ATOM K) NIL)
                                          ((EQCAR K MVAR) (LIST (CADR K)))
                                          (T NIL)))
                                  (CAR K)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                         (SETQ K (CDR K))
                         (GO LOOPLABEL)))))
        (RETURN (AEVAL L1)))))) 
(ENDMODULE) 