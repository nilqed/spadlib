(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LIQSIMP2)) 
(FLUID '(INFINITIES*)) 
(PUT 'LIQSIMP2-MAXMIN 'NUMBER-OF-ARGS 1) 
(PUT 'LIQSIMP2-MAXMIN 'DEFINED-ON-LINE '32) 
(PUT 'LIQSIMP2-MAXMIN 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2-MAXMIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIQSIMP2-MAXMIN (W)
    (PROG (R)
      (SETQ INFINITIES* (LIST (SIMP 'INFINITY) (SIMP '(MINUS INFINITY))))
      (SETQ W
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q W)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Q)
                                    (LIST (CAR Q) (MINMAX2QL (CADR Q))
                                          (MINMAX2QL (CADDR Q))))
                                  (CAR Q))
                                 NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Q)
                            (LIST (CAR Q) (MINMAX2QL (CADR Q))
                                  (MINMAX2QL (CADDR Q))))
                          (CAR Q))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R (LIQSIMP2-MAXMIN1 W))
      (RETURN
       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
         (SETQ Q R)
         (COND ((NULL Q) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (Q)
                             (LIST (CAR Q) (QL2MINMAX 'MAX (CADR Q))
                                   (QL2MINMAX 'MIN (CADDR Q))))
                           (CAR Q))
                          NIL)))
        LOOPLABEL
         (SETQ Q (CDR Q))
         (COND ((NULL Q) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (Q)
                     (LIST (CAR Q) (QL2MINMAX 'MAX (CADR Q))
                           (QL2MINMAX 'MIN (CADDR Q))))
                   (CAR Q))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'QL2MINMAX 'NUMBER-OF-ARGS 2) 
(PUT 'QL2MINMAX 'DEFINED-ON-LINE '47) 
(PUT 'QL2MINMAX 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'QL2MINMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QL2MINMAX (M L)
    (PROGN
     (SETQ L
             (PROG (Q FORALL-RESULT FORALL-ENDPTR)
               (SETQ Q L)
               (COND ((NULL Q) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (Q) (PREPSQ Q)) (CAR Q)) NIL)))
              LOOPLABEL
               (SETQ Q (CDR Q))
               (COND ((NULL Q) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (Q) (PREPSQ Q)) (CAR Q)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (COND ((CDR L) (CONS M L)) (T (CAR L))))) 
(PUT 'MINMAX2QL 'NUMBER-OF-ARGS 1) 
(PUT 'MINMAX2QL 'DEFINED-ON-LINE '51) 
(PUT 'MINMAX2QL 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'MINMAX2QL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINMAX2QL (L)
    (COND
     ((AND (PAIRP L) (MEMQ (CAR L) '(MIN MAX)))
      (PROG (Q FORALL-RESULT FORALL-ENDPTR)
        (SETQ Q (CDR L))
        (COND ((NULL Q) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (Q) (SIMP Q)) (CAR Q)) NIL)))
       LOOPLABEL
        (SETQ Q (CDR Q))
        (COND ((NULL Q) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (Q) (SIMP Q)) (CAR Q)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T (LIST (SIMP L))))) 
(PUT 'LIQSIMP2-MAXMIN1 'NUMBER-OF-ARGS 1) 
(PUT 'LIQSIMP2-MAXMIN1 'DEFINED-ON-LINE '55) 
(PUT 'LIQSIMP2-MAXMIN1 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2-MAXMIN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIQSIMP2-MAXMIN1 (W)
    (COND ((NULL W) NIL)
          (T (LIQSIMP2-REDUCECASES (CAR W) (LIQSIMP2-MAXMIN1 (CDR W)))))) 
(PUT 'LIQSIMP2-REDUCECASES 'NUMBER-OF-ARGS 2) 
(PUT 'LIQSIMP2-REDUCECASES 'DEFINED-ON-LINE '60) 
(PUT 'LIQSIMP2-REDUCECASES 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2-REDUCECASES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIQSIMP2-REDUCECASES (W LL)
    (PROG (X L U T1 E1 E2 PTS EQNS Y)
      (SETQ X (CAAR W))
      (SETQ L (CADR W))
      (SETQ U (CADDR W))
      (COND ((AND (NULL (CDR L)) (NULL (CDR U))) (RETURN (CONS W LL))))
      (SETQ EQNS
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q
                        (DELETE (CAR INFINITIES*)
                                (DELETE (CADR INFINITIES*) (APPEND L U))))
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Q) (LIST Q)) (CAR Q)) NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Q) (LIST Q)) (CAR Q)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ T1 EQNS)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND T1 (CDR T1))) (RETURN NIL)))
        (PROGN
         (SETQ E1 (CAR T1))
         (SETQ T1 (CDR T1))
         (PROG (E2)
           (SETQ E2 T1)
          LAB
           (COND ((NULL E2) (RETURN NIL)))
           ((LAMBDA (E2)
              (PROGN
               (SETQ PTS (LIQSIMP2_MK_EDGES X (CAR E1) (CAR E2) L U LL))
               (SETCDR E1 (APPEND (CDR E1) PTS))
               (SETCDR E2 (APPEND (CDR E2) PTS))
               NIL))
            (CAR E2))
           (SETQ E2 (CDR E2))
           (GO LAB)))
        (GO WHILELABEL))
      (SETQ L
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q L)
               STARTOVER
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (Q)
                           (COND
                            ((OR (NULL (SETQ Y (ASSOC Q EQNS))) (CDR Y))
                             (LIST Q))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ Q (CDR Q))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (Q)
                           (COND
                            ((OR (NULL (SETQ Y (ASSOC Q EQNS))) (CDR Y))
                             (LIST Q))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ Q (CDR Q))
                (GO LOOPLABEL)))
      (SETQ U
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q U)
               STARTOVER
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (Q)
                           (COND
                            ((OR (NULL (SETQ Y (ASSOC Q EQNS))) (CDR Y))
                             (LIST Q))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ Q (CDR Q))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (Q)
                           (COND
                            ((OR (NULL (SETQ Y (ASSOC Q EQNS))) (CDR Y))
                             (LIST Q))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ Q (CDR Q))
                (GO LOOPLABEL)))
      (RETURN (CONS (LIST (CAR W) L U) LL)))) 
(PUT 'LIQSIMP2_MK_EDGES 'NUMBER-OF-ARGS 6) 
(PUT 'LIQSIMP2_MK_EDGES 'DEFINED-ON-LINE '90) 
(PUT 'LIQSIMP2_MK_EDGES 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2_MK_EDGES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIQSIMP2_MK_EDGES (X E1 E2 L U LL)
    (PROG (FORM PTS PL)
      (SETQ FORM (ADDSQ E1 (NEGSQ E2)))
      (SETQ PL (LIQSIMP2_MK_EDGES1 FORM LL))
      (SETQ PTS (LIQSIMP2_MK_EDGES_CHECK PL X E1 L U))
      (RETURN PTS))) 
(PUT 'SFNEGATIVEP 'NUMBER-OF-ARGS 1) 
(PUT 'SFNEGATIVEP 'DEFINED-ON-LINE '104) 
(PUT 'SFNEGATIVEP 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'SFNEGATIVEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFNEGATIVEP (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (MINUSF U))
          ((EQUAL (CAAAR U) 'INFINITY) (SFNEGATIVEP (CDAR U)))
          (T (TYPERR (PREPF U) "numeric expression")))) 
(PUT 'LIQSIMP2_MK_EDGES1 'NUMBER-OF-ARGS 2) 
(PUT 'LIQSIMP2_MK_EDGES1 'DEFINED-ON-LINE '109) 
(PUT 'LIQSIMP2_MK_EDGES1 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2_MK_EDGES1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIQSIMP2_MK_EDGES1 (F LL)
    (COND ((AND (NULL LL) (NULL (CAR F))) '(NIL))
          ((NULL LL) (TYPERR (PREPSQ F) "soll nicht vorkommen"))
          (T
           (PROG (FX FXX T1 X L U POINTS PL)
             (SETQ X (CAAAR LL))
             (SETQ L (CADAR LL))
             (SETQ U (CADDAR LL))
             (SETQ LL (CDR LL))
             (SETQ T1
                     (DELETE (CAR INFINITIES*)
                             (DELETE (CADR INFINITIES*) (APPEND L U))))
             (COND ((NULL T1) (SETQ T1 '((NIL . 1)))))
             (SETQ FX (LIQSIMP2_MK_EDGES2 F X))
             (SETQ FXX '(NIL . 1))
             (SETQ POINTS
                     (COND
                      ((NULL FX)
                       (PROGN
                        (SETQ PL (LIQSIMP2_MK_EDGES1 FXX LL))
                        (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                          (SETQ Q T1)
                         STARTOVER
                          (COND ((NULL Q) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  ((LAMBDA (Q)
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P PL)
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P)
                                                           (CONS
                                                            (CONS X
                                                                  (PREPSQ
                                                                   (SUBSQ Q
                                                                          P)))
                                                            P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P)
                                                   (CONS
                                                    (CONS X
                                                          (PREPSQ (SUBSQ Q P)))
                                                    P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR Q)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                          (SETQ Q (CDR Q))
                          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                         LOOPLABEL
                          (COND ((NULL Q) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  ((LAMBDA (Q)
                                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ P PL)
                                       (COND ((NULL P) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (P)
                                                           (CONS
                                                            (CONS X
                                                                  (PREPSQ
                                                                   (SUBSQ Q
                                                                          P)))
                                                            P))
                                                         (CAR P))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ P (CDR P))
                                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (P)
                                                   (CONS
                                                    (CONS X
                                                          (PREPSQ (SUBSQ Q P)))
                                                    P))
                                                 (CAR P))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR Q)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                          (SETQ Q (CDR Q))
                          (GO LOOPLABEL))))
                      ((OR (ATOM (CAR FX)) (ATOM (CAR (CAR FX))))
                       (PROGN
                        (SETQ PL (LIQSIMP2_MK_EDGES1 FXX LL))
                        (SETQ PL (LIQSIMP2_MK_EDGES_CHECK PL X FX L U))
                        PL))
                      (T
                       (PROG (P FORALL-RESULT FORALL-ENDPTR)
                         (SETQ P T1)
                        STARTOVER
                         (COND ((NULL P) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ FXX (ADDSQ FX (NEGSQ P)))
                                     (SETQ PL (LIQSIMP2_MK_EDGES1 FXX LL))
                                     (SETQ PL
                                             (LIQSIMP2_MK_EDGES_CHECK PL X FX L
                                              U))
                                     PL))
                                  (CAR P)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                         (SETQ P (CDR P))
                         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                        LOOPLABEL
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ FXX (ADDSQ FX (NEGSQ P)))
                                     (SETQ PL (LIQSIMP2_MK_EDGES1 FXX LL))
                                     (SETQ PL
                                             (LIQSIMP2_MK_EDGES_CHECK PL X FX L
                                              U))
                                     PL))
                                  (CAR P)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                         (SETQ P (CDR P))
                         (GO LOOPLABEL)))))
             (RETURN POINTS))))) 
(PUT 'LIQSIMP2_MK_EDGES_CHECK 'NUMBER-OF-ARGS 5) 
(PUT 'LIQSIMP2_MK_EDGES_CHECK 'DEFINED-ON-LINE '155) 
(PUT 'LIQSIMP2_MK_EDGES_CHECK 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2_MK_EDGES_CHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIQSIMP2_MK_EDGES_CHECK (PL X FX L U)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P PL)
     STARTOVER
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (P)
                 (PROG (FINE X1)
                   (SETQ FINE T)
                   (SETQ X1 (SUBSQ FX P))
                   (PROG (L1)
                     (SETQ L1 L)
                    LAB
                     (COND ((NULL L1) (RETURN NIL)))
                     ((LAMBDA (L1)
                        (COND
                         ((AND FINE
                               (SFNEGATIVEP
                                (CAR (ADDSQ X1 (NEGSQ (SUBSQ L1 P))))))
                          (SETQ FINE NIL))))
                      (CAR L1))
                     (SETQ L1 (CDR L1))
                     (GO LAB))
                   (PROG (U1)
                     (SETQ U1 U)
                    LAB
                     (COND ((NULL U1) (RETURN NIL)))
                     ((LAMBDA (U1)
                        (COND
                         ((AND FINE
                               (SFNEGATIVEP
                                (CAR (ADDSQ (SUBSQ U1 P) (NEGSQ X1)))))
                          (SETQ FINE NIL))))
                      (CAR U1))
                     (SETQ U1 (CDR U1))
                     (GO LAB))
                   (RETURN
                    (COND (FINE (LIST (CONS (CONS X (PREPSQ X1)) P)))))))
               (CAR P)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ P (CDR P))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (P)
                 (PROG (FINE X1)
                   (SETQ FINE T)
                   (SETQ X1 (SUBSQ FX P))
                   (PROG (L1)
                     (SETQ L1 L)
                    LAB
                     (COND ((NULL L1) (RETURN NIL)))
                     ((LAMBDA (L1)
                        (COND
                         ((AND FINE
                               (SFNEGATIVEP
                                (CAR (ADDSQ X1 (NEGSQ (SUBSQ L1 P))))))
                          (SETQ FINE NIL))))
                      (CAR L1))
                     (SETQ L1 (CDR L1))
                     (GO LAB))
                   (PROG (U1)
                     (SETQ U1 U)
                    LAB
                     (COND ((NULL U1) (RETURN NIL)))
                     ((LAMBDA (U1)
                        (COND
                         ((AND FINE
                               (SFNEGATIVEP
                                (CAR (ADDSQ (SUBSQ U1 P) (NEGSQ X1)))))
                          (SETQ FINE NIL))))
                      (CAR U1))
                     (SETQ U1 (CDR U1))
                     (GO LAB))
                   (RETURN
                    (COND (FINE (LIST (CONS (CONS X (PREPSQ X1)) P)))))))
               (CAR P)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ P (CDR P))
      (GO LOOPLABEL))) 
(PUT 'LIQSIMP2_MK_EDGES2 'NUMBER-OF-ARGS 2) 
(PUT 'LIQSIMP2_MK_EDGES2 'DEFINED-ON-LINE '172) 
(PUT 'LIQSIMP2_MK_EDGES2 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LIQSIMP2_MK_EDGES2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIQSIMP2_MK_EDGES2 (F X)
    (COND ((NOT (SMEMQ X F)) NIL)
          (T
           (PROG (W)
             ((LAMBDA (KORD*) (SETQ W (REORDER (CAR F)))) (LIST X))
             (RETURN
              (MULTSQ (CONS (NEGF (CDR W)) 1) (INVSQ (CONS (CDAR W) 1)))))))) 
(PUT 'LININEQPRINT1 'NUMBER-OF-ARGS 3) 
(PUT 'LININEQPRINT1 'DEFINED-ON-LINE '183) 
(PUT 'LININEQPRINT1 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LININEQPRINT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LININEQPRINT1 (TEXT LH RH)
    (PROGN
     (WRITEPRI TEXT 'FIRST)
     (WRITEPRI (MKQUOTE (PREPSQ LH)) NIL)
     (WRITEPRI " >= " NIL)
     (WRITEPRI (MKQUOTE (PREPSQ RH)) 'LAST))) 
(PUT 'LININEQPRINT2 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQPRINT2 'DEFINED-ON-LINE '190) 
(PUT 'LININEQPRINT2 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LININEQPRINT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQPRINT2 (TEXT PROB)
    (PROGN
     (PRIN2T "--------------------------------")
     (COND ((ATOM TEXT) (SETQ TEXT (LIST TEXT))))
     (PROG (U)
       (SETQ U TEXT)
      LAB
       (COND ((NULL U) (RETURN NIL)))
       ((LAMBDA (U) (PRIN2 U)) (CAR U))
       (SETQ U (CDR U))
       (GO LAB))
     (TERPRI)
     (WRITEPRI
      (MKQUOTE
       (CONS 'LIST
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P PROB)
               (COND ((NULL P) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P)
                                   (LIST 'GEQ (PREPSQ (CAR P))
                                         (PREPSQ (CDR P))))
                                 (CAR P))
                                NIL)))
              LOOPLABEL
               (SETQ P (CDR P))
               (COND ((NULL P) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P)
                           (LIST 'GEQ (PREPSQ (CAR P)) (PREPSQ (CDR P))))
                         (CAR P))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      'LAST))) 
(PUT 'LININEQPRINT3 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQPRINT3 'DEFINED-ON-LINE '199) 
(PUT 'LININEQPRINT3 'DEFINED-IN-FILE 'SOLVE/LIQSIMP2.RED) 
(PUT 'LININEQPRINT3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQPRINT3 (TEXT RES)
    (PROGN
     (WRITEPRI TEXT 'FIRST)
     (WRITEPRI
      (MKQUOTE
       (CONS 'LIST
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P RES)
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
               (GO LOOPLABEL))))
      'LAST)
     NIL)) 
(ENDMODULE) 