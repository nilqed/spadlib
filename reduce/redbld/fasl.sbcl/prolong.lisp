(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PROLONG)) 
(FLUID
 '(*EDSVERBOSE *EDSDEBUG *ARBVARS *VAROPT *GROEBOPT *SOLVEINCONSISTENT DEPL*
   *EDSSLOPPY PULLBACK_MAPS)) 
(PUT 'GRASSMANN_VARIETY 'RTYPEFN 'GETRTYPECAR) 
(PUT 'GRASSMANN_VARIETY 'EDSFN 'GRASSMANNVARIETY) 
(FLAG '(GRASSMANNVARIETY GRASSMANNVARIETYSOLUTION GRASSMANNVARIETYTORSION)
      'HIDDEN) 
(PUT 'GRASSMANNVARIETY 'NUMBER-OF-ARGS 1) 
(PUT 'GRASSMANNVARIETY 'DEFINED-ON-LINE '45) 
(PUT 'GRASSMANNVARIETY 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'GRASSMANNVARIETY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRASSMANNVARIETY (S)
    (PROG (P G S0 V)
      (COND ((SETQ G (GETEDS S 'GRASSMANNVARIETY)) (RETURN G)))
      (SETQ S0 (CLOSURE S))
      (SETQ G (GBSYS S0))
      (SETQ P (SOLVEDPART (PFAFFPART (CADR S0))))
      (PROG (F)
        (SETQ F (SETDIFF (CADR S0) P))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (SETQ V
                   (UNION
                    (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Q (XCOEFFS (XREDUCE F (CADR G))))
                      (COND ((NULL Q) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Q) (CONS (CONS 1 Q) NIL))
                                        (CAR Q))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Q (CDR Q))
                      (COND ((NULL Q) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Q) (CONS (CONS 1 Q) NIL)) (CAR Q))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    V)))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ G (AUGMENTSYS G (APPEND V P)))
      (PUTEDS S 'GRASSMANNVARIETY G)
      (RETURN G))) 
(PUT 'PROLONG 'RTYPEFN 'QUOTEEDS) 
(PUT 'PROLONG 'EDSFN 'PROLONGEDS) 
(PUT 'PROLONGEDS 'NUMBER-OF-ARGS 1) 
(PUT 'PROLONGEDS 'DEFINED-ON-LINE '71) 
(PUT 'PROLONGEDS 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'PROLONGEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROLONGEDS (S)
    (PROG ()
      (SETQ PULLBACK_MAPS
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST (LIST))))
      (RETURN
       (COND ((NOT (EDSP S)) (TYPERR S 'EDS))
             (T
              (MKXEDS
               (CONS 'LIST
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (PROLONG S))
                      STARTOVER
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (X) (COND ((CDR X) (LIST (CDR X)))))
                                (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ X (CDR X))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (X) (COND ((CDR X) (LIST (CDR X)))))
                                (CAR X)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ X (CDR X))
                       (GO LOOPLABEL))))))))) 
(PUT 'PROLONG 'NUMBER-OF-ARGS 1) 
(PUT 'PROLONG 'DEFINED-ON-LINE '83) 
(PUT 'PROLONG 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'PROLONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROLONG (S)
    (PROG (G V S1)
      (SETQ G (COPYEDS (GRASSMANNVARIETY S)))
      (UPDKORDL (EDSCOB G))
      (SETCAR (CDR G) (SETDIFF (CADR G) (SCALARPART (CADR G))))
      (SETQ V (DECOMPOSEGRASSMANNVARIETY S))
      (RETURN
       (COND
        ((NULL V)
         (PROGN
          (EDSVERBOSE "Prolongation inconsistent" NIL NIL)
          (SETCAR (CDR G) (LIST (CONS (CONS 1 (CONS 1 1)) NIL)))
          (LIST (CONS 'INCONSISTENT G))))
        (T
         (PROG (STRATA FORALL-RESULT FORALL-ENDPTR)
           (SETQ STRATA V)
          STARTOVER
           (COND ((NULL STRATA) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   ((LAMBDA (STRATA)
                      (COND
                       ((EQUAL (CAR STRATA) 'FAILED)
                        (PROGN
                         (EDSVERBOSE "Prolongation failed - solution variety:"
                          (CDR STRATA) 'SQ)
                         (LIST
                          (CONS 'FAILED
                                (AUGMENTSYS G
                                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Q (CDR STRATA))
                                   (COND ((NULL Q) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (Q)
                                                       (CONS (CONS 1 Q) NIL))
                                                     (CAR Q))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ Q (CDR Q))
                                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Q) (CONS (CONS 1 Q) NIL))
                                             (CAR Q))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))))))
                       ((EQUAL (CAR STRATA) 'BASE)
                        (PROGN
                         (EDSVERBOSE "Reduction using new equations:"
                          (CDR STRATA) 'RMAP)
                         (SETQ PULLBACK_MAPS
                                 (PROGN
                                  (SETQ ALGLIST* (CONS NIL NIL))
                                  (APPEND PULLBACK_MAPS
                                          (LIST (*RMAP2A (CDR STRATA))))))
                         (SETQ S1
                                 (EDSPROTECT (LIST 'PULLBACK0 S (CDR STRATA))))
                         (COND
                          ((SCALARPART (CADR S1))
                           (SETQ S1 (EDSPROTECT (LIST 'POSITIVEEDS S1)))))
                         (COND ((EMPTYEDSP S1) (LIST (CONS 'INCONSISTENT S1)))
                               ((EDSP S1) (LIST (CONS 'REDUCED S1)))
                               (T
                                (PROG (S2 FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ S2 (GETRLIST S1))
                                  (COND ((NULL S2) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (S2)
                                                      (CONS 'REDUCED S2))
                                                    (CAR S2))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ S2 (CDR S2))
                                  (COND ((NULL S2) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S2) (CONS 'REDUCED S2))
                                            (CAR S2))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))))
                       ((EQUAL (CAR STRATA) 'FIBRE)
                        (PROGN
                         (COND
                          ((CADR STRATA)
                           (EDSVERBOSE "Prolongation using new equations:"
                            (CDR STRATA) 'RMAP))
                          (T
                           (EDSVERBOSE "Prolongation (no new equations)" NIL
                            NIL)))
                         (SETQ PULLBACK_MAPS
                                 (PROGN
                                  (SETQ ALGLIST* (CONS NIL NIL))
                                  (APPEND PULLBACK_MAPS
                                          (LIST (*RMAP2A (CDR STRATA))))))
                         (SETQ S1
                                 (EDSPROTECT (LIST 'PULLBACK0 G (CDR STRATA))))
                         (LIST (CONS 'PROLONGED S1))))))
                    (CAR STRATA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
           (SETQ STRATA (CDR STRATA))
           (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
          LOOPLABEL
           (COND ((NULL STRATA) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   ((LAMBDA (STRATA)
                      (COND
                       ((EQUAL (CAR STRATA) 'FAILED)
                        (PROGN
                         (EDSVERBOSE "Prolongation failed - solution variety:"
                          (CDR STRATA) 'SQ)
                         (LIST
                          (CONS 'FAILED
                                (AUGMENTSYS G
                                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Q (CDR STRATA))
                                   (COND ((NULL Q) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (Q)
                                                       (CONS (CONS 1 Q) NIL))
                                                     (CAR Q))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ Q (CDR Q))
                                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Q) (CONS (CONS 1 Q) NIL))
                                             (CAR Q))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))))))
                       ((EQUAL (CAR STRATA) 'BASE)
                        (PROGN
                         (EDSVERBOSE "Reduction using new equations:"
                          (CDR STRATA) 'RMAP)
                         (SETQ PULLBACK_MAPS
                                 (PROGN
                                  (SETQ ALGLIST* (CONS NIL NIL))
                                  (APPEND PULLBACK_MAPS
                                          (LIST (*RMAP2A (CDR STRATA))))))
                         (SETQ S1
                                 (EDSPROTECT (LIST 'PULLBACK0 S (CDR STRATA))))
                         (COND
                          ((SCALARPART (CADR S1))
                           (SETQ S1 (EDSPROTECT (LIST 'POSITIVEEDS S1)))))
                         (COND ((EMPTYEDSP S1) (LIST (CONS 'INCONSISTENT S1)))
                               ((EDSP S1) (LIST (CONS 'REDUCED S1)))
                               (T
                                (PROG (S2 FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ S2 (GETRLIST S1))
                                  (COND ((NULL S2) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (S2)
                                                      (CONS 'REDUCED S2))
                                                    (CAR S2))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ S2 (CDR S2))
                                  (COND ((NULL S2) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S2) (CONS 'REDUCED S2))
                                            (CAR S2))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))))
                       ((EQUAL (CAR STRATA) 'FIBRE)
                        (PROGN
                         (COND
                          ((CADR STRATA)
                           (EDSVERBOSE "Prolongation using new equations:"
                            (CDR STRATA) 'RMAP))
                          (T
                           (EDSVERBOSE "Prolongation (no new equations)" NIL
                            NIL)))
                         (SETQ PULLBACK_MAPS
                                 (PROGN
                                  (SETQ ALGLIST* (CONS NIL NIL))
                                  (APPEND PULLBACK_MAPS
                                          (LIST (*RMAP2A (CDR STRATA))))))
                         (SETQ S1
                                 (EDSPROTECT (LIST 'PULLBACK0 G (CDR STRATA))))
                         (LIST (CONS 'PROLONGED S1))))))
                    (CAR STRATA)))
           (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
           (SETQ STRATA (CDR STRATA))
           (GO LOOPLABEL))))))) 
(PUT 'DECOMPOSEGRASSMANNVARIETY 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSEGRASSMANNVARIETY 'DEFINED-ON-LINE '130) 
(PUT 'DECOMPOSEGRASSMANNVARIETY 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'DECOMPOSEGRASSMANNVARIETY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSEGRASSMANNVARIETY (S)
    (PROG (G V C B)
      (SETQ G (GRASSMANNVARIETY S))
      (SETQ C (REVERSE (SETDIFF (EDSCRD G) (EDSINDCRD G))))
      (SETQ C (EDSGRADECOORDS C (GETEDS G 'JET0)))
      (COND ((NULL (SETDIFF (EDSCRD G) (EDSCRD S))) (SETQ C (CONS (LIST) C))))
      (COND
       ((SEMILINEARP S)
        (COND
         ((SETQ V (GRASSMANNVARIETYTORSION S))
          (COND
           ((NULL
             (SETQ V
                     (EDSSOLVEGRADED V (CDR C)
                      (CADDR (CDDR (CADDR (CDR S)))))))
            (RETURN (LIST (CONS 'INCONSISTENT NIL))))
           (T
            (RETURN
             (PROG (M FORALL-RESULT FORALL-ENDPTR)
               (SETQ M V)
               (COND ((NULL M) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (M)
                                   (COND ((CAR M) (CONS 'BASE (CDR M)))
                                         (T (CONS 'FAILED (CDR M)))))
                                 (CAR M))
                                NIL)))
              LOOPLABEL
               (SETQ M (CDR M))
               (COND ((NULL M) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (M)
                           (COND ((CAR M) (CONS 'BASE (CDR M)))
                                 (T (CONS 'FAILED (CDR M)))))
                         (CAR M))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))
         ((SETQ V (PARTSOLVEGRASSMANNVARIETY S))
          (RETURN
           (LIST
            (CONS 'FIBRE
                  (*MAP2RMAP
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (CAR V))
                    STARTOVER
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (X)
                                (COND
                                 ((NOT (MEMQ (CAR X) (CADR V)))
                                  (LIST
                                   (CONS (CAR X)
                                         (MK*SQ
                                          (SUBSQ (SIMP* (CDR X))
                                                 (CADDR V))))))))
                              (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ X (CDR X))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (X)
                                (COND
                                 ((NOT (MEMQ (CAR X) (CADR V)))
                                  (LIST
                                   (CONS (CAR X)
                                         (MK*SQ
                                          (SUBSQ (SIMP* (CDR X))
                                                 (CADDR V))))))))
                              (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ X (CDR X))
                     (GO LOOPLABEL)))))))
         (T (ERRDHH "Bad solution to semilinear system"))))
       (T
        (PROGN
         (SETQ V
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F (SCALARPART (CADR G)))
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (F) (CDAR F)) (CAR F)) NIL)))
                  LOOPLABEL
                   (SETQ F (CDR F))
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (F) (CDAR F)) (CAR F)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((NULL (SETQ V (EDSSOLVEGRADED V C (CADDR (CDDR (CADDR (CDR S)))))))
           (RETURN (LIST (CONS 'INCONSISTENT NIL))))
          (T
           (RETURN
            (PROG (M FORALL-RESULT FORALL-ENDPTR)
              (SETQ M V)
              (COND ((NULL M) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (M)
                                  (COND ((NULL (CAR M)) (CONS 'FAILED (CDR M)))
                                        ((SETQ B
                                                 (PROG (S FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ S (CADR M))
                                                  STARTOVER
                                                   (COND
                                                    ((NULL S) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           ((LAMBDA (S)
                                                              (COND
                                                               ((NOT
                                                                 (MEMQ (CAR S)
                                                                       (CAR
                                                                        C)))
                                                                (LIST S))))
                                                            (CAR S)))
                                                   (SETQ FORALL-ENDPTR
                                                           (LASTPAIR
                                                            FORALL-RESULT))
                                                   (SETQ S (CDR S))
                                                   (COND
                                                    ((ATOM FORALL-ENDPTR)
                                                     (GO STARTOVER)))
                                                  LOOPLABEL
                                                   (COND
                                                    ((NULL S)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           ((LAMBDA (S)
                                                              (COND
                                                               ((NOT
                                                                 (MEMQ (CAR S)
                                                                       (CAR
                                                                        C)))
                                                                (LIST S))))
                                                            (CAR S)))
                                                   (SETQ FORALL-ENDPTR
                                                           (LASTPAIR
                                                            FORALL-ENDPTR))
                                                   (SETQ S (CDR S))
                                                   (GO LOOPLABEL)))
                                         (CONS 'BASE
                                               (LIST B
                                                     (PROG (P FORALL-RESULT
                                                            FORALL-ENDPTR)
                                                       (SETQ P (CADDR M))
                                                      STARTOVER
                                                       (COND
                                                        ((NULL P)
                                                         (RETURN NIL)))
                                                       (SETQ FORALL-RESULT
                                                               ((LAMBDA (P)
                                                                  (COND
                                                                   ((FREEOFL P
                                                                             (CAR
                                                                              C))
                                                                    (LIST P))))
                                                                (CAR P)))
                                                       (SETQ FORALL-ENDPTR
                                                               (LASTPAIR
                                                                FORALL-RESULT))
                                                       (SETQ P (CDR P))
                                                       (COND
                                                        ((ATOM FORALL-ENDPTR)
                                                         (GO STARTOVER)))
                                                      LOOPLABEL
                                                       (COND
                                                        ((NULL P)
                                                         (RETURN
                                                          FORALL-RESULT)))
                                                       (RPLACD FORALL-ENDPTR
                                                               ((LAMBDA (P)
                                                                  (COND
                                                                   ((FREEOFL P
                                                                             (CAR
                                                                              C))
                                                                    (LIST P))))
                                                                (CAR P)))
                                                       (SETQ FORALL-ENDPTR
                                                               (LASTPAIR
                                                                FORALL-ENDPTR))
                                                       (SETQ P (CDR P))
                                                       (GO LOOPLABEL)))))
                                        (T (CONS 'FIBRE (CDR M)))))
                                (CAR M))
                               NIL)))
             LOOPLABEL
              (SETQ M (CDR M))
              (COND ((NULL M) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (M)
                          (COND ((NULL (CAR M)) (CONS 'FAILED (CDR M)))
                                ((SETQ B
                                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ S (CADR M))
                                          STARTOVER
                                           (COND ((NULL S) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   ((LAMBDA (S)
                                                      (COND
                                                       ((NOT
                                                         (MEMQ (CAR S)
                                                               (CAR C)))
                                                        (LIST S))))
                                                    (CAR S)))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR FORALL-RESULT))
                                           (SETQ S (CDR S))
                                           (COND
                                            ((ATOM FORALL-ENDPTR)
                                             (GO STARTOVER)))
                                          LOOPLABEL
                                           (COND
                                            ((NULL S) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   ((LAMBDA (S)
                                                      (COND
                                                       ((NOT
                                                         (MEMQ (CAR S)
                                                               (CAR C)))
                                                        (LIST S))))
                                                    (CAR S)))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR FORALL-ENDPTR))
                                           (SETQ S (CDR S))
                                           (GO LOOPLABEL)))
                                 (CONS 'BASE
                                       (LIST B
                                             (PROG (P FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ P (CADDR M))
                                              STARTOVER
                                               (COND ((NULL P) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       ((LAMBDA (P)
                                                          (COND
                                                           ((FREEOFL P (CAR C))
                                                            (LIST P))))
                                                        (CAR P)))
                                               (SETQ FORALL-ENDPTR
                                                       (LASTPAIR
                                                        FORALL-RESULT))
                                               (SETQ P (CDR P))
                                               (COND
                                                ((ATOM FORALL-ENDPTR)
                                                 (GO STARTOVER)))
                                              LOOPLABEL
                                               (COND
                                                ((NULL P)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       ((LAMBDA (P)
                                                          (COND
                                                           ((FREEOFL P (CAR C))
                                                            (LIST P))))
                                                        (CAR P)))
                                               (SETQ FORALL-ENDPTR
                                                       (LASTPAIR
                                                        FORALL-ENDPTR))
                                               (SETQ P (CDR P))
                                               (GO LOOPLABEL)))))
                                (T (CONS 'FIBRE (CDR M)))))
                        (CAR M))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))
         NIL))))) 
(PUT 'PARTSOLVEGRASSMANNVARIETY 'NUMBER-OF-ARGS 1) 
(PUT 'PARTSOLVEGRASSMANNVARIETY 'DEFINED-ON-LINE '173) 
(PUT 'PARTSOLVEGRASSMANNVARIETY 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'PARTSOLVEGRASSMANNVARIETY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARTSOLVEGRASSMANNVARIETY (S)
    (PROG (V C)
      (COND ((SETQ V (GETEDS S 'GRASSMANNVARIETYSOLUTION)) (RETURN V)))
      (SETQ V (GRASSMANNVARIETY S))
      (SETQ C (REVERSE (SETDIFF (EDSCRD V) (EDSCRD S))))
      (SETQ V
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (SCALARPART (CADR V)))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (CDAR F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (CDAR F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V (EDSPARTSOLVE V C))
      (PUTEDS S 'GRASSMANNVARIETYSOLUTION V)
      (RETURN V))) 
(PUT 'DIM_GRASSMANN_VARIETY 'SIMPFN 'DIMGRASSMANNVARIETYEVAL) 
(PUT 'DIMGRASSMANNVARIETYEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'DIMGRASSMANNVARIETYEVAL 'DEFINED-ON-LINE '190) 
(PUT 'DIMGRASSMANNVARIETYEVAL 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'DIMGRASSMANNVARIETYEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIMGRASSMANNVARIETYEVAL (U)
    (COND
     ((OR (LESSP (LENGTH U) 1) (GREATERP (LENGTH U) 2))
      (RERROR 'EDS 0 "Wrong number of arguments to dim_grassmann_variety"))
     ((EDSP (CAR (SETQ U (REVLIS U))))
      (CONS
       (EDSPROTECT
        (LIST 'DIMGRASSMANNVARIETY (CAR U) (COND ((CDR U) (*A2SYS (CADR U))))))
       1))
     (T (TYPERR (CAR U) "EDS")))) 
(PUT 'DIMGRASSMANNVARIETY 'NUMBER-OF-ARGS 2) 
(PUT 'DIMGRASSMANNVARIETY 'DEFINED-ON-LINE '201) 
(PUT 'DIMGRASSMANNVARIETY 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'DIMGRASSMANNVARIETY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIMGRASSMANNVARIETY (S X)
    (PROG (V C)
      (COND
       ((NOT (QUASILINEARP S))
        (COND
         ((NULL X)
          (RERROR 'EDS 0 "Integral element required for nonlinear EDS"))
         (T (SETQ S (LINEARISE S X))))))
      (SETQ V (GRASSMANNVARIETY S))
      (SETQ C (LENGTH (SETDIFF (EDSCRD V) (EDSCRD S))))
      (SETQ V (PARTSOLVEGRASSMANNVARIETY S))
      (SETQ C
              (DIFFERENCE C
                          (PROG (X FORALL-RESULT)
                            (SETQ X (CAR V))
                            (SETQ FORALL-RESULT 0)
                           LAB1
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (SETQ FORALL-RESULT
                                    (PLUS
                                     ((LAMBDA (X)
                                        (COND ((MEMQ (CAR X) (CADR V)) 0)
                                              (T 1)))
                                      (CAR X))
                                     FORALL-RESULT))
                            (SETQ X (CDR X))
                            (GO LAB1))))
      (RETURN C))) 
(PUT 'TORSION 'RTYPEFN 'QUOTELIST) 
(PUT 'TORSION 'LISTFN 'TORSIONEVAL) 
(PUT 'TORSIONEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'TORSIONEVAL 'DEFINED-ON-LINE '223) 
(PUT 'TORSIONEVAL 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'TORSIONEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TORSIONEVAL (U V)
    (COND
     ((NOT (EDSPROTECT (LIST 'SEMILINEARP (SETQ U (REVAL1 (CAR U) T)))))
      (RERROR 'EDS 0 "TORSION available for semi-linear systems only"))
     (T
      (CONS 'LIST
            (PROG (Q FORALL-RESULT FORALL-ENDPTR)
              (SETQ Q (EDSPROTECT (LIST 'GRASSMANNVARIETYTORSION U)))
              (COND ((NULL Q) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (Q) (*Q2A1 Q V)) (CAR Q)) NIL)))
             LOOPLABEL
              (SETQ Q (CDR Q))
              (COND ((NULL Q) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (Q) (*Q2A1 Q V)) (CAR Q)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT 'GRASSMANNVARIETYTORSION 'NUMBER-OF-ARGS 1) 
(PUT 'GRASSMANNVARIETYTORSION 'DEFINED-ON-LINE '232) 
(PUT 'GRASSMANNVARIETYTORSION 'DEFINED-IN-FILE 'EDS/PROLONG.RED) 
(PUT 'GRASSMANNVARIETYTORSION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRASSMANNVARIETYTORSION (S)
    (PROG (V)
      (COND ((SETQ V (GETEDS S 'GRASSMANNVARIETYTORSION)) (RETURN V)))
      (SETQ V (PARTSOLVEGRASSMANNVARIETY S))
      (SETQ V
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CAR V))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND
                            ((AND (MEMQ (CAR X) (CADR V))
                                  (CAR
                                   (PROGN
                                    (SETQ X
                                            (ADDSQ
                                             (NEGSQ
                                              (CONS
                                               (LIST (CONS (CONS (CAR X) 1) 1))
                                               1))
                                             (SIMP* (CDR X))))
                                    (SETQ X (SUBSQ X (CADDR V))))))
                             (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND
                            ((AND (MEMQ (CAR X) (CADR V))
                                  (CAR
                                   (PROGN
                                    (SETQ X
                                            (ADDSQ
                                             (NEGSQ
                                              (CONS
                                               (LIST (CONS (CONS (CAR X) 1) 1))
                                               1))
                                             (SIMP* (CDR X))))
                                    (SETQ X (SUBSQ X (CADDR V))))))
                             (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (PUTEDS S 'GRASSMANNVARIETYTORSION V)
      (RETURN V))) 
(ENDMODULE) 