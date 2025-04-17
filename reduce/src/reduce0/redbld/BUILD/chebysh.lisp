(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CHEBYSH)) 
(PUT 'CHEBYSHEV_FIT 'PSOPFN '(LAMBDA (U) (CHEBYSHEVEVAL U 'FIT))) 
(PUT 'CHEBYSHEV_EVAL 'PSOPFN '(LAMBDA (U) (CHEBYSHEVEVAL U 'EVAL))) 
(PUT 'CHEBYSHEV_INT 'PSOPFN '(LAMBDA (U) (CHEBYSHEVEVAL U 'INT))) 
(PUT 'CHEBYSHEV_DF 'PSOPFN '(LAMBDA (U) (CHEBYSHEVEVAL U 'DF))) 
(PUT 'CHEBYSHEVEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'CHEBYSHEVEVAL 'DEFINED-ON-LINE '39) 
(PUT 'CHEBYSHEVEVAL 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBYSHEVEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHEBYSHEVEVAL (U MODE)
    (PROG (W X C E Y R OLDMODE N)
      (SETQ N 0)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (ACCURACYCONTROL U 3 20))
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (SETQ W (CAR U))
      (COND ((NOT (EQCAR W 'EQUAL)) (TYPERR W "interval bounds")))
      (SETQ OLDMODE (SWITCH-MODE-RD NIL))
      (SETQ Y (REVALNUMINTERVAL (CADDR W) T))
      (SETQ X (CADR W))
      (COND
       ((EQUAL MODE 'FIT)
        (PROGN
         (SETQ N (COND ((CDR U) (IEVAL (CADR U))) (T 20)))
         (SETQ C (CHEBCOEFFS E X (CAR Y) (CADR Y) N))
         (SETQ R (LIST 'LIST (CHEBPOL C X (CAR Y) (CADR Y)) (CONS 'LIST C)))
         NIL))
       ((EQUAL MODE 'EVAL)
        (PROGN
         (COND
          ((OR (NULL (CDR U)) (NOT (EQCAR (CADR U) 'EQUAL)))
           (REDERR "Chebyshev_eval: point missing")))
         (SETQ E (CDR (LISTEVAL E T)))
         (SETQ W (CAR (SIMP (CADDR (CADR U)))))
         (SETQ R (PREPF (CHEBEVAL E X (CAR Y) (CADR Y) W)))
         NIL))
       ((OR (EQUAL MODE 'INT) (EQUAL MODE 'DF))
        (PROGN
         (SETQ E (CDR (LISTEVAL E T)))
         (SETQ R
                 (COND ((EQUAL MODE 'INT) (CHEBINT E X (CAR Y) (CADR Y)))
                       (T (CHEBDF E X (CAR Y) (CADR Y)))))
         (SETQ R
                 (CONS 'LIST
                       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Q R)
                         (COND ((NULL Q) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (Q) (PREPF Q)) (CAR Q))
                                               NIL)))
                        LOOPLABEL
                         (SETQ Q (CDR Q))
                         (COND ((NULL Q) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (Q) (PREPF Q)) (CAR Q)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         NIL)))
      (SWITCH-MODE-RD OLDMODE)
      (RETURN R))) 
(PUT 'CHEBCOEFFS 'NUMBER-OF-ARGS 5) 
(PUT 'CHEBCOEFFS 'DEFINED-ON-LINE '80) 
(PUT 'CHEBCOEFFS 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBCOEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBCOEFFS (FUNC X A B N)
    (PROG (K J N1 FAC BPA BMA F 1PI C Y SU NN HALF W)
      (SETQ K 0)
      (SETQ J 0)
      (SETQ N1 0)
      (SETQ X (LIST X))
      (SETQ 1PI (RDPI*))
      (SETQ NN (|::QUOTIENT| 1 N))
      (SETQ N1 (DIFFERENCE N 1))
      (SETQ HALF (|::QUOTIENT| 1 2))
      (PROGN
       (SETQ BMA (|::QUOTIENT| (|:DIFFERENCE| B A) 2))
       (SETQ BPA (|::QUOTIENT| (|:PLUSN| B A) 2)))
      (SETQ W (|:TIMESN| 1PI NN))
      (SETQ F
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K 0)
                (COND ((MINUSP (DIFFERENCE N1 K)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ Y
                                          (|:PLUSN|
                                           (|:TIMESN|
                                            (RDCOS*
                                             (|:TIMESN| W (|:PLUSN| K HALF)))
                                            BMA)
                                           BPA))
                                  (EVALUATE FUNC X (LIST Y)))
                                 NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND ((MINUSP (DIFFERENCE N1 K)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ Y
                                  (|:PLUSN|
                                   (|:TIMESN|
                                    (RDCOS* (|:TIMESN| W (|:PLUSN| K HALF)))
                                    BMA)
                                   BPA))
                          (EVALUATE FUNC X (LIST Y)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROGN (SETQ FAC (|:TIMESN| 2 NN)))
      (SETQ C
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 0)
                (COND ((MINUSP (DIFFERENCE N1 J)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ W (|:TIMESN| 1PI J NN))
                                  (SETQ SU NIL)
                                  (PROG (K)
                                    (SETQ K 0)
                                   LAB
                                    (COND
                                     ((MINUSP (DIFFERENCE N1 K)) (RETURN NIL)))
                                    (SETQ SU
                                            (|:PLUSN| SU
                                                      (|:TIMESN|
                                                       (NTH F (IADD1 K))
                                                       (RDCOS*
                                                        (|:TIMESN| W
                                                                   (|:PLUSN| K
                                                                             HALF))))))
                                    (SETQ K (PLUS2 K 1))
                                    (GO LAB))
                                  (|:TIMESN| FAC SU))
                                 NIL)))
               LOOPLABEL
                (SETQ J (PLUS2 J 1))
                (COND ((MINUSP (DIFFERENCE N1 J)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ W (|:TIMESN| 1PI J NN))
                          (SETQ SU NIL)
                          (PROG (K)
                            (SETQ K 0)
                           LAB
                            (COND ((MINUSP (DIFFERENCE N1 K)) (RETURN NIL)))
                            (SETQ SU
                                    (|:PLUSN| SU
                                              (|:TIMESN| (NTH F (IADD1 K))
                                                         (RDCOS*
                                                          (|:TIMESN| W
                                                                     (|:PLUSN|
                                                                      K
                                                                      HALF))))))
                            (SETQ K (PLUS2 K 1))
                            (GO LAB))
                          (|:TIMESN| FAC SU))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN C))) 
(PUT 'CHEBPOL 'NUMBER-OF-ARGS 4) 
(PUT 'CHEBPOL 'DEFINED-ON-LINE '110) 
(PUT 'CHEBPOL 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBPOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBPOL (C X A B)
    (PROG (D DD SV FAC CNST N)
      (SETQ N 0)
      (SETQ N (LENGTH C))
      (SETQ D
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NIL NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DD
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NIL NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR DD (NTH C N))
      (PROG (J)
        (SETQ J (DIFFERENCE N 2))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
        (PROGN
         (PROG (K)
           (SETQ K (DIFFERENCE N J))
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 K))) (RETURN NIL)))
           (PROGN
            (SETQ SV (NTH D (PLUS K 1)))
            (SETCAR (PNTH D (PLUS K 1))
                    (|:DIFFERENCE| (|:TIMESN| 2 (NTH D K)) (NTH DD (ADD1 K))))
            (SETCAR (PNTH DD (PLUS K 1)) SV))
           (SETQ K (PLUS2 K (MINUS 1)))
           (GO LAB))
         (SETQ SV (CAR D))
         (SETCAR D (|:PLUSN| (|:MINUS| (CAR DD)) (NTH C (ADD1 J))))
         (SETCAR DD SV)
         NIL)
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (PROG (J)
        (SETQ J (DIFFERENCE N 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
        (SETCAR (PNTH D (PLUS J 1))
                (|:DIFFERENCE| (NTH D J) (NTH DD (ADD1 J))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (SETCAR D (|:PLUSN| (|:MINUS| (CAR DD)) (|::QUOTIENT| (CAR C) 2)))
      (SETQ CNST (|::QUOTIENT| 2 (|:DIFFERENCE| B A)))
      (SETQ FAC CNST)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) J)) (RETURN NIL)))
        (PROGN
         (SETCAR (PNTH D (ADD1 J)) (|:TIMESN| (NTH D (ADD1 J)) FAC))
         (SETQ FAC (|:TIMESN| FAC CNST))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ CNST (|::QUOTIENT| (|:PLUSN| A B) 2))
      (PROG (J)
        (SETQ J 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 2) J)) (RETURN NIL)))
        (PROG (K)
          (SETQ K (DIFFERENCE N 2))
         LAB
          (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE J K))) (RETURN NIL)))
          (SETCAR (PNTH D (ADD1 K))
                  (|:DIFFERENCE| (NTH D (ADD1 K))
                                 (|:TIMESN| CNST (NTH D (ADD1 (ADD1 K))))))
          (SETQ K (PLUS2 K (MINUS 1)))
          (GO LAB))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN
       (REVAL1
        (CONS 'PLUS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (LIST 'TIMES (NTH D I)
                                       (LIST 'EXPT X (DIFFERENCE I 1)))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (LIST 'TIMES (NTH D I)
                               (LIST 'EXPT X (DIFFERENCE I 1)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
        T)))) 
(PUT 'CHEBEVAL 'NUMBER-OF-ARGS 5) 
(PUT 'CHEBEVAL 'DEFINED-ON-LINE '147) 
(PUT 'CHEBEVAL 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBEVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBEVAL (C XX A B X)
    (PROG (D DD Y Y2 SV M)
      (SETQ M 0)
      (SETQ XX NIL)
      (SETQ C
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q C)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q))
                                      NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ M (LENGTH C))
      (SETQ Y2
              (|:TIMESN| 2
                         (SETQ Y
                                 (|::QUOTIENT|
                                  (|:DIFFERENCE|
                                   (|:DIFFERENCE| (|:TIMESN| 2 X) A) B)
                                  (|:DIFFERENCE| B A)))))
      (PROG (J)
        (SETQ J (DIFFERENCE M 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
        (PROGN
         (SETQ SV D)
         (SETQ D
                 (|:PLUSN| (|:DIFFERENCE| (|:TIMESN| Y2 D) DD)
                           (NTH C (ADD1 J))))
         (SETQ DD SV)
         NIL)
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (RETURN
       (|:PLUSN| (|:DIFFERENCE| (|:TIMESN| Y D) DD) (|::QUOTIENT| (CAR C) 2))))) 
(PUT 'CHEBINT 'NUMBER-OF-ARGS 4) 
(PUT 'CHEBINT 'DEFINED-ON-LINE '162) 
(PUT 'CHEBINT 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBINT (C XX A B)
    (PROG (SU FAC CON CINT W N JJ)
      (SETQ N 0)
      (SETQ JJ 0)
      (SETQ XX NIL)
      (SETQ N (LENGTH C))
      (SETQ C
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q C)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q))
                                      NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FAC 1)
      (SETQ CON (|::QUOTIENT| (|:DIFFERENCE| B A) 4))
      (SETQ CINT
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 2) J)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ JJ (PLUS J 2))
                                  (SETQ W
                                          (|:TIMESN| CON
                                                     (|::QUOTIENT|
                                                      (|:DIFFERENCE| (NTH C J)
                                                                     (NTH C
                                                                          JJ))
                                                      J)))
                                  (SETQ SU (|:PLUSN| SU (|:TIMESN| FAC W)))
                                  (SETQ FAC (|:MINUS| FAC))
                                  W)
                                 NIL)))
               LOOPLABEL
                (SETQ J (PLUS2 J 1))
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE N 2) J))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ JJ (PLUS J 2))
                          (SETQ W
                                  (|:TIMESN| CON
                                             (|::QUOTIENT|
                                              (|:DIFFERENCE| (NTH C J)
                                                             (NTH C JJ))
                                              J)))
                          (SETQ SU (|:PLUSN| SU (|:TIMESN| FAC W)))
                          (SETQ FAC (|:MINUS| FAC))
                          W)
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CINT
              (APPEND CINT
                      (LIST
                       (|:TIMESN| CON
                                  (|::QUOTIENT| (NTH C (SUB1 N))
                                   (|:DIFFERENCE| N 1))))))
      (SETQ SU (|:PLUSN| SU (|:TIMESN| FAC (NTH C N))))
      (SETQ CINT (CONS (|:PLUSN| SU SU) CINT))
      (RETURN CINT))) 
(PUT 'CHEBDF 'NUMBER-OF-ARGS 4) 
(PUT 'CHEBDF 'DEFINED-ON-LINE '183) 
(PUT 'CHEBDF 'DEFINED-IN-FILE 'NUMERIC/CHEBYSH.RED) 
(PUT 'CHEBDF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBDF (C XX A B)
    (PROG (CON CDER N JJ)
      (SETQ N 0)
      (SETQ JJ 0)
      (SETQ XX NIL)
      (SETQ N (LENGTH C))
      (SETQ C
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q C)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q))
                                      NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Q) (CAR (SIMP Q))) (CAR Q)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CDER
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NIL NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETCAR (PNTH CDER (DIFFERENCE N 1))
              (|:TIMESN| 2 (|:DIFFERENCE| N 1) (NTH C N)))
      (PROG (J)
        (SETQ J (DIFFERENCE N 3))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 J))) (RETURN NIL)))
        (PROGN
         (SETQ JJ (PLUS J 3))
         (SETCAR (PNTH CDER (PLUS J 1))
                 (|:PLUSN| (NTH CDER JJ)
                           (|:TIMESN| 2 (|:PLUSN| J 1)
                                      (NTH C (ADD1 (ADD1 J))))))
         NIL)
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (SETQ CON (|::QUOTIENT| 2 (|:DIFFERENCE| B A)))
      (RETURN
       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
         (SETQ Q CDER)
         (COND ((NULL Q) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (Q) (|:TIMESN| CON Q)) (CAR Q)) NIL)))
        LOOPLABEL
         (SETQ Q (CDR Q))
         (COND ((NULL Q) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (Q) (|:TIMESN| CON Q)) (CAR Q)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(ENDMODULE) 