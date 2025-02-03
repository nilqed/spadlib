(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'STEEPSTD)) 
(FLUID '(*NOEQUIV ACCURACY*)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC)) 
(PUT 'RDMINEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RDMINEVAL 'DEFINED-ON-LINE '36) 
(PUT 'RDMINEVAL 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'RDMINEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDMINEVAL (U)
    (PROG (E VARS X Y Z OLDMODE P *NOEQUIV)
      (SETQ OLDMODE (STEEPDECEDMODE NIL NIL))
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
      (SETQ U (ACCURACYCONTROL U 6 40))
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (COND
       ((EQCAR (CAR U) 'LIST)
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CDAR U))
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
                  (GO LOOPLABEL)))))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((EQCAR X 'EQUAL) (PROGN (SETQ Z (CADR X)) (SETQ Y (CADDR X))))
             (T (PROGN (SETQ Z X) (SETQ Y (RANDOM 100)))))
            (SETQ VARS (CONS Z VARS))
            (SETQ P (CONS Y P))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VARS (REVERSIP VARS))
      (SETQ P (REVERSIP P))
      (SETQ X (STEEPDECEVAL1 E VARS P 'NUM_MIN))
      (STEEPDECEDMODE T OLDMODE)
      (RETURN
       (LIST 'LIST (CAR X)
             (CONS 'LIST
                   (PROG (P FORALL-RESULT FORALL-ENDPTR)
                     (SETQ P (PAIR VARS (CDR X)))
                     (COND ((NULL P) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (P)
                                         (LIST 'EQUAL (CAR P) (CDR P)))
                                       (CAR P))
                                      NIL)))
                    LOOPLABEL
                     (SETQ P (CDR P))
                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                               (CAR P))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))))) 
(PUT 'NUM_MIN 'PSOPFN 'RDMINEVAL) 
(PUT 'RDSOLVESTDEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RDSOLVESTDEVAL 'DEFINED-ON-LINE '58) 
(PUT 'RDSOLVESTDEVAL 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'RDSOLVESTDEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDSOLVESTDEVAL (U)
    (PROG (E VARS X Y Z OLDMODE P Q *NOEQUIV)
      (SETQ OLDMODE (STEEPDECEDMODE NIL NIL))
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
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (SETQ E (COND ((EQCAR E 'LIST) (CDR E)) (T (LIST E))))
      (SETQ Q
              (CONS 'PLUS
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F E)
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F)
                                          (LIST 'EXPT
                                                (COND ((EQEXPR F) (*EQN2A F))
                                                      (T F))
                                                2))
                                        (CAR F))
                                       NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (F)
                                  (LIST 'EXPT
                                        (COND ((EQEXPR F) (*EQN2A F)) (T F))
                                        2))
                                (CAR F))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ Q (PREPSQ (SIMP Q)))
      (COND
       ((EQCAR (CAR U) 'LIST)
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CDAR U))
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
                  (GO LOOPLABEL)))))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((EQCAR X 'EQUAL) (PROGN (SETQ Z (CADR X)) (SETQ Y (CADDR X))))
             (T (PROGN (SETQ Z X) (SETQ Y (RANDOM 100)))))
            (SETQ VARS (CONS Z VARS))
            (SETQ P (CONS Y P))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VARS (REVERSIP VARS))
      (SETQ P (REVERSIP P))
      (SETQ X (STEEPDECEVAL1 Q VARS P 'ROOT))
      (STEEPDECEDMODE T OLDMODE)
      (COND ((NULL X) (REDERR "no solution found")))
      (RETURN
       (CONS 'LIST
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P (PAIR VARS (CDR X)))
               (COND ((NULL P) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                                 (CAR P))
                                NIL)))
              LOOPLABEL
               (SETQ P (CDR P))
               (COND ((NULL P) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P))) (CAR P))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'STEEPDECEDMODE 'NUMBER-OF-ARGS 2) 
(PUT 'STEEPDECEDMODE 'DEFINED-ON-LINE '86) 
(PUT 'STEEPDECEDMODE 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'STEEPDECEDMODE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STEEPDECEDMODE (M OLDMODE)
    (COND
     ((NOT M)
      (PROGN
       (COND
        (*COMPLEX
         (REDERR "steepest descent method not applicable under complex")))
       (COND
        ((NOT (EQUAL DMODE* '|:RD:|))
         (PROGN (SETQ OLDMODE T) (SETDMODE 'ROUNDED T))))
       (LIST OLDMODE (PRECISION 0) *ROUNDBF)))
     (T
      (PROGN
       (COND ((CAR OLDMODE) (SETDMODE 'ROUNDED NIL)))
       (PRECISION (CADR OLDMODE))
       (SETQ *ROUNDBF (CADDR OLDMODE)))))) 
(PUT 'STEEPDECEVAL1 'NUMBER-OF-ARGS 4) 
(PUT 'STEEPDECEVAL1 'DEFINED-ON-LINE '98) 
(PUT 'STEEPDECEVAL1 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'STEEPDECEVAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE STEEPDECEVAL1 (F VARS X MODE)
    (PROG (E G R ACC N)
      (SETQ N 0)
      (SETQ N (LENGTH VARS))
      (SETQ E (PREPSQ (SIMP F)))
      (COND (*TRNUMERIC (LPRIM "computing symbolic gradient")))
      (SETQ G
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VARS)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (PREPSQ (SIMP (LIST 'DF F V))))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (PREPSQ (SIMP (LIST 'DF F V)))) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ *NOEQUIV T)
      (COND
       (*TRNUMERIC
        (LPRIM "starting Fletcher Reeves steepest descent iteration")))
      (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
      (SETQ X
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U X)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (FORCE-TO-DM (CAR (SIMP U))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U) (FORCE-TO-DM (CAR (SIMP U)))) (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R (STEEPDEC2 E G VARS ACC X MODE))
      (SETQ R
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X R)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (PREPF X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (PREPF X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN R))) 
(PUT 'STEEPDEC2 'NUMBER-OF-ARGS 6) 
(PUT 'STEEPDEC2 'DEFINED-ON-LINE '116) 
(PUT 'STEEPDEC2 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'STEEPDEC2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE STEEPDEC2 (F GRAD VARS ACC X MODE)
    (PROG (E0 E00 E1 E2 A A1 A1A1 A2 A2A2 X1 X2 G H DX DELTA LIMIT GOLD GNORM
           GOLDNORM MULTI COUNT K N)
      (SETQ COUNT 0)
      (SETQ K 0)
      (SETQ N 0)
      (SETQ N (LENGTH GRAD))
      (SETQ MULTI ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) N 1))
      (SETQ N (|:TIMESN| 10 N))
      (SETQ E00 (SETQ E0 (EVALUATE F VARS X)))
      (SETQ A1 1)
     INIT
      (SETQ K 0)
      (SETQ G
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V GRAD)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (|:MINUS| (EVALUATE V VARS X)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (|:MINUS| (EVALUATE V VARS X))) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GNORM (NORMLIST G))
      (SETQ H G)
     LOOP
      (SETQ COUNT (ADD1 COUNT))
      (SETQ K (ADD1 K))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) COUNT ITERATIONS*)
        (PROGN
         (LPRIM "requested accuracy not reached within iteration limit")
         (GO READY))))
      (SETQ A2 NIL)
     L1
      (SETQ X1 (LIST+LIST X (SCAL*LIST A1 H)))
      (SETQ E1 (EVALUATE F VARS X1))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) E1 E0)
        (PROGN
         (SETQ A2 A1)
         (SETQ X2 X1)
         (SETQ E2 E1)
         (SETQ A1 (|::QUOTIENT| A1 2))
         (GO L1))))
      (COND (A2 (GO ALPH)))
     L2
      (SETQ A2 (|:PLUSN| A1 A1))
      (SETQ X2 (LIST+LIST X (SCAL*LIST A2 H)))
      (SETQ E2 (EVALUATE F VARS X2))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) E2 E1)
        (PROGN (SETQ A1 A2) (SETQ E1 E2) (GO L2))))
     ALPH
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B)))
         (ABSF (|:DIFFERENCE| E1 E2)) ACC)
        (GO READY)))
      (SETQ A1A1 (|:TIMESN| A1 A1))
      (SETQ A2A2 (|:TIMESN| A2 A2))
      (SETQ A
              (|:PLUSN| (|:TIMESN| (|:DIFFERENCE| A1A1 A2A2) E0)
                        (|:DIFFERENCE| (|:TIMESN| A2A2 E1)
                                       (|:TIMESN| A1A1 E2))))
      (SETQ A
              (|::QUOTIENT| A
               (|:PLUSN| (|:TIMESN| (|:DIFFERENCE| A1 A2) E0)
                         (|:DIFFERENCE| (|:TIMESN| A2 E1) (|:TIMESN| A1 E2)))))
      (SETQ A (|::QUOTIENT| A 2))
      (SETQ DX (SCAL*LIST A H))
      (SETQ X (LIST+LIST X DX))
      (SETQ E0 (EVALUATE F VARS X))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) E0 E1)
        (PROGN
         (SETQ DX (SCAL*LIST (|:DIFFERENCE| A1 A) H))
         (SETQ X (LIST+LIST X DX))
         (SETQ E0 E1)
         (SETQ DX (SCAL*LIST A1 H)))))
      (SETQ DELTA (NORMLIST DX))
      (STEEPDECPRINTPOINT COUNT X DELTA E0 GNORM)
      (UPDATE-PRECISION (LIST DELTA E0 GNORM))
      (COND (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) K N) (GO INIT)))
      (SETQ GOLD G)
      (SETQ GOLDNORM GNORM)
      (SETQ G
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V GRAD)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V) (|:MINUS| (EVALUATE V VARS X)))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (|:MINUS| (EVALUATE V VARS X))) (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GNORM (NORMLIST G))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) GNORM LIMIT)
        (GO READY)))
      (SETQ H
              (COND ((NOT MULTI) G)
                    (T
                     (LIST+LIST G
                      (SCAL*LIST (|::QUOTIENT| GNORM GOLDNORM) H)))))
      (COND
       ((OR
         (AND (EQUAL MODE 'NUM_MIN)
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) GNORM ACC))
         (AND (EQUAL MODE 'ROOT)
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) E0 ACC)))
        (GO LOOP)))
     READY
      (COND
       ((AND (EQUAL MODE 'ROOT)
             (NOT
              ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) (ABSF E0) ACC)))
        (PROGN
         (LPRIM "probably fallen into local minimum of f^2")
         (RETURN NIL))))
      (RETURN (CONS E0 X)))) 
(PUT 'STEEPDECPRINTPOINT 'NUMBER-OF-ARGS 5) 
(PUT 'STEEPDECPRINTPOINT 'DEFINED-ON-LINE '193) 
(PUT 'STEEPDECPRINTPOINT 'DEFINED-IN-FILE 'NUMERIC/STEEPSTD.RED) 
(PUT 'STEEPDECPRINTPOINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE STEEPDECPRINTPOINT (COUNT X D E0 NG)
    (COND
     (*TRNUMERIC
      (PROG (N)
        (SETQ N 0)
        (WRITEPRI COUNT 'FIRST)
        (WRITEPRI ". residue=" NIL)
        (WRITEPRI (MKQUOTE (PREPF E0)) NIL)
        (WRITEPRI ", gradient length=" NIL)
        (WRITEPRI (MKQUOTE (PREPF NG)) 'LAST)
        (WRITEPRI " at (" NIL)
        (PROG (Y)
          (SETQ Y X)
         LAB
          (COND ((NULL Y) (RETURN NIL)))
          ((LAMBDA (Y)
             (PROGN
              (COND ((GREATERP N 0) (WRITEPRI " , " NIL)))
              (SETQ N (PLUS N 1))
              (WRITEPRI (MKQUOTE (PREPF Y)) NIL)))
           (CAR Y))
          (SETQ Y (CDR Y))
          (GO LAB))
        (WRITEPRI ")" NIL)
        (WRITEPRI ", stepsize=" NIL)
        (WRITEPRI (MKQUOTE (PREPF D)) NIL)
        (WRITEPRI "" 'LAST))))) 
(ENDMODULE) 