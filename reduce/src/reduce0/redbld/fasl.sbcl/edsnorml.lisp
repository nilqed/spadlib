(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSNORMAL)) 
(FLUID '(CFRMRSX* *EDSSLOPPY PULLBACK_MAPS XVARS* KORD*)) 
(PUT 'NORMALEDS 'NUMBER-OF-ARGS 1) 
(PUT 'NORMALEDS 'DEFINED-ON-LINE '54) 
(PUT 'NORMALEDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'NORMALEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMALEDS (S)
    (COND ((NORMALEDSP S) S) (T (SORTEDS (REDUCEDEDS (SOLVEDEDS S)))))) 
(PUT 'NORMALEDSP 'NUMBER-OF-ARGS 1) 
(PUT 'NORMALEDSP 'DEFINED-ON-LINE '63) 
(PUT 'NORMALEDSP 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'NORMALEDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMALEDSP (S) (AND (SOLVEDEDSP S) (REDUCEDEDSP S))) 
(PUT 'LIFT 'EDSFN 'POSITIVEEDS) 
(PUT 'LIFT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'POSITIVEEDS 'NUMBER-OF-ARGS 1) 
(PUT 'POSITIVEEDS 'DEFINED-ON-LINE '73) 
(PUT 'POSITIVEEDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'POSITIVEEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POSITIVEEDS (S)
    (PROG (V C S1)
      (SETQ V
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (SCALARPART (CADR S)))
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
      (COND ((NULL V) (RETURN S)))
      (EDSVERBOSE "Solving 0-forms" NIL NIL)
      (SETCAR (CDR S) (SETDIFF (CADR S) V))
      (SETQ C (REVERSE (SETDIFF (EDSCRD S) (EDSINDCRD S))))
      (SETQ C (EDSGRADECOORDS C (GETEDS S 'JET0)))
      (SETQ V (EDSSOLVEGRADED V C (CADDR (CDDR (CADDR (CDR S))))))
      (SETQ S
              (PURGEXEDS
               (CONS 'LIST
                     (COND
                      ((NULL V)
                       (PROGN
                        (EDSVERBOSE "System inconsistent" NIL NIL)
                        (LIST)))
                      (T
                       (PROG (STRATA FORALL-RESULT FORALL-ENDPTR)
                         (SETQ STRATA V)
                         (COND ((NULL STRATA) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (STRATA)
                                             (COND
                                              ((NULL (CAR STRATA))
                                               (PROGN
                                                (EDSVERBOSE
                                                 "Couldn't solve 0-forms"
                                                 (CDR STRATA) 'SQ)
                                                (SETQ STRATA
                                                        (PROG (Q FORALL-RESULT
                                                               FORALL-ENDPTR)
                                                          (SETQ Q (CDR STRATA))
                                                          (COND
                                                           ((NULL Q)
                                                            (RETURN NIL)))
                                                          (SETQ FORALL-RESULT
                                                                  (SETQ FORALL-ENDPTR
                                                                          (CONS
                                                                           ((LAMBDA
                                                                                (
                                                                                 Q)
                                                                              (CONS
                                                                               (CONS
                                                                                1
                                                                                Q)
                                                                               NIL))
                                                                            (CAR
                                                                             Q))
                                                                           NIL)))
                                                         LOOPLABEL
                                                          (SETQ Q (CDR Q))
                                                          (COND
                                                           ((NULL Q)
                                                            (RETURN
                                                             FORALL-RESULT)))
                                                          (RPLACD FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (Q)
                                                                      (CONS
                                                                       (CONS 1
                                                                             Q)
                                                                       NIL))
                                                                    (CAR Q))
                                                                   NIL))
                                                          (SETQ FORALL-ENDPTR
                                                                  (CDR
                                                                   FORALL-ENDPTR))
                                                          (GO LOOPLABEL)))
                                                (AUGMENTSYS S STRATA)))
                                              (T
                                               (PROGN
                                                (EDSVERBOSE "New equations:"
                                                 (CADR STRATA) 'MAP)
                                                (SETQ S1
                                                        (PULLBACK0 S
                                                         (CDR STRATA)))
                                                (COND
                                                 ((NULL (SCALARPART (CADR S1)))
                                                  S1)
                                                 (T
                                                  (EDSPROTECT
                                                   (LIST 'POSITIVEEDS
                                                         S1))))))))
                                           (CAR STRATA))
                                          NIL)))
                        LOOPLABEL
                         (SETQ STRATA (CDR STRATA))
                         (COND ((NULL STRATA) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (STRATA)
                                     (COND
                                      ((NULL (CAR STRATA))
                                       (PROGN
                                        (EDSVERBOSE "Couldn't solve 0-forms"
                                         (CDR STRATA) 'SQ)
                                        (SETQ STRATA
                                                (PROG (Q FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ Q (CDR STRATA))
                                                  (COND
                                                   ((NULL Q) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (Q)
                                                                      (CONS
                                                                       (CONS 1
                                                                             Q)
                                                                       NIL))
                                                                    (CAR Q))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ Q (CDR Q))
                                                  (COND
                                                   ((NULL Q)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (Q)
                                                              (CONS (CONS 1 Q)
                                                                    NIL))
                                                            (CAR Q))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL)))
                                        (AUGMENTSYS S STRATA)))
                                      (T
                                       (PROGN
                                        (EDSVERBOSE "New equations:"
                                         (CADR STRATA) 'MAP)
                                        (SETQ S1 (PULLBACK0 S (CDR STRATA)))
                                        (COND
                                         ((NULL (SCALARPART (CADR S1))) S1)
                                         (T
                                          (EDSPROTECT
                                           (LIST 'POSITIVEEDS S1))))))))
                                   (CAR STRATA))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (RETURN S))) 
(FLAG '(REDUCED SOLVED) 'HIDDEN) 
(PUT 'REDUCEDEDS 'NUMBER-OF-ARGS 1) 
(PUT 'REDUCEDEDS 'DEFINED-ON-LINE '105) 
(PUT 'REDUCEDEDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'REDUCEDEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDUCEDEDS (S)
    (COND ((KNOWNTRUEEDS S 'REDUCED) S)
          (T
           (PROG (M P Q)
             (SETQ M (SETCFRM (EDS_CFRM* S)))
             (SETQ P (SOLVEDPART (PFAFFPART (CADR S))))
             (SETQ Q
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F (SETDIFF (CADR S) P))
                      STARTOVER
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (F)
                                  (COND
                                   ((SETQ F (XREDUCE F P))
                                    (LIST
                                     (COND
                                      ((CFRMNOWHEREZERO (CAR (CDAR F)))
                                       (XNORMALISE F))
                                      (T F))))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ F (CDR F))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (F)
                                  (COND
                                   ((SETQ F (XREDUCE F P))
                                    (LIST
                                     (COND
                                      ((CFRMNOWHEREZERO (CAR (CDAR F)))
                                       (XNORMALISE F))
                                      (T F))))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ F (CDR F))
                       (GO LOOPLABEL)))
             (SETCAR (CDR S) (APPEND P Q))
             (FLAGTRUEEDS S 'REDUCED)
             (RETURN S))))) 
(PUT 'REDUCEDEDSP 'NUMBER-OF-ARGS 1) 
(PUT 'REDUCEDEDSP 'DEFINED-ON-LINE '122) 
(PUT 'REDUCEDEDSP 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'REDUCEDEDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDUCEDEDSP (S) (KNOWNTRUEEDS S 'REDUCED)) 
(PUT 'SOLVEDEDS 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEDEDS 'DEFINED-ON-LINE '127) 
(PUT 'SOLVEDEDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'SOLVEDEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEDEDS (S)
    (COND ((KNOWNEDS S 'SOLVED) S)
          (T
           (PROG (M N P Q Z I IK DK PK KL)
             (SETQ M (COPYCFRM (EDS_CFRM* S)))
             (SETQ I (XAUTOREDUCE (CADDR S)))
             (SETQ I
                     (COND
                      ((OR *EDSSLOPPY (SINGLETERMS I)) (REVERSE (LPOWS I)))
                      (T (LIST))))
             (SETQ KL (APPEND (SETDIFF (CADR M) I) I))
             (SETCAR (CDR M) KL)
             (SETQ N (SETCFRM M))
             (EDSDEBUG "Solving Pfaffian subsystem" NIL NIL)
             (SETQ Q
                     (SOLVEPFSYS1 (PFAFFPART (CADR S))
                      (COND (*EDSSLOPPY (SETDIFF (CADR M) I)))))
             (SETQ P (CAR Q))
             (SETQ DK (LPOWS P))
             (EDSDEBUG "Solving independence forms" NIL NIL)
             (SETQ I
                     (SOLVEPFSYS1
                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                        (SETQ F (CADDR S))
                       STARTOVER
                        (COND ((NULL F) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (F)
                                   (COND
                                    ((SETQ F (XREDUCE (XREORDER F) P))
                                     (LIST F))))
                                 (CAR F)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ F (CDR F))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (F)
                                   (COND
                                    ((SETQ F (XREDUCE (XREORDER F) P))
                                     (LIST F))))
                                 (CAR F)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ F (CDR F))
                        (GO LOOPLABEL))
                      (COND (*EDSSLOPPY I))))
             (COND
              ((GREATERP (LENGTH (CADDR S))
                         (PLUS (LENGTH (CAR I)) (LENGTH (CADR I))))
               (RETURN
                (PROGN
                 (EDSVERBOSE "System inconsistent" NIL NIL)
                 (SETCFRM N)
                 (EMPTYEDS)))))
             (SETQ IK (LPOWS (CAR I)))
             (SETQ Q
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F (CADR Q))
                      STARTOVER
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (F)
                                  (COND
                                   ((XREDUCE (SETQ F (XREORDER F)) (CAR I))
                                    (LIST F))
                                   (T
                                    (PROGN
                                     (SETQ Z
                                             (UNION
                                              (PROG (W FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ W IK)
                                               STARTOVER
                                                (COND ((NULL W) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        ((LAMBDA (W)
                                                           (COND
                                                            ((SETQ W
                                                                     (XCOEFF F
                                                                      (WEDGEFAX
                                                                       W)))
                                                             (LIST W))))
                                                         (CAR W)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-RESULT))
                                                (SETQ W (CDR W))
                                                (COND
                                                 ((ATOM FORALL-ENDPTR)
                                                  (GO STARTOVER)))
                                               LOOPLABEL
                                                (COND
                                                 ((NULL W)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        ((LAMBDA (W)
                                                           (COND
                                                            ((SETQ W
                                                                     (XCOEFF F
                                                                      (WEDGEFAX
                                                                       W)))
                                                             (LIST W))))
                                                         (CAR W)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-ENDPTR))
                                                (SETQ W (CDR W))
                                                (GO LOOPLABEL))
                                              Z))
                                     NIL))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ F (CDR F))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (F)
                                  (COND
                                   ((XREDUCE (SETQ F (XREORDER F)) (CAR I))
                                    (LIST F))
                                   (T
                                    (PROGN
                                     (SETQ Z
                                             (UNION
                                              (PROG (W FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ W IK)
                                               STARTOVER
                                                (COND ((NULL W) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        ((LAMBDA (W)
                                                           (COND
                                                            ((SETQ W
                                                                     (XCOEFF F
                                                                      (WEDGEFAX
                                                                       W)))
                                                             (LIST W))))
                                                         (CAR W)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-RESULT))
                                                (SETQ W (CDR W))
                                                (COND
                                                 ((ATOM FORALL-ENDPTR)
                                                  (GO STARTOVER)))
                                               LOOPLABEL
                                                (COND
                                                 ((NULL W)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        ((LAMBDA (W)
                                                           (COND
                                                            ((SETQ W
                                                                     (XCOEFF F
                                                                      (WEDGEFAX
                                                                       W)))
                                                             (LIST W))))
                                                         (CAR W)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-ENDPTR))
                                                (SETQ W (CDR W))
                                                (GO LOOPLABEL))
                                              Z))
                                     NIL))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ F (CDR F))
                       (GO LOOPLABEL)))
             (COND (Z (EDSVERBOSE "New 0-form conditions detected" Z 'SYS)))
             (SETQ PK (SETDIFF KL (APPEND DK IK)))
             (SETQ KL (APPEND DK (APPEND PK IK)))
             (UPDKORDL KL)
             (SETQ M (COPYCFRM (CADDR (CDR S))))
             (SETQ S (COPYEDS S))
             (SETCAR (CDR S)
                     (XREORDERSYS
                      (APPEND Z
                              (APPEND P (APPEND Q (NONPFAFFPART (CADR S)))))))
             (SETCAR (CDDR S) (XREORDERSYS (APPEND (CAR I) (CADR I))))
             (SETCAR (CDR M) KL)
             (SETCAR (CDDR (CDR S)) M)
             (COND
              (*EDSSLOPPY (SETCAR (CDDR (CDR S)) (UPDATERSX (CADDR (CDR S))))))
             (COND ((OR Q (CADR I)) (FLAGFALSEEDS S 'SOLVED))
                   (T (FLAGTRUEEDS S 'SOLVED)))
             (REMPROPEDS S 'REDUCED)
             (COND (Z (REMTRUEEDS S 'CLOSED)))
             (REMKRNS S)
             (SETCFRM N)
             (RETURN S))))) 
(PUT 'XREORDERSYS 'NUMBER-OF-ARGS 1) 
(PUT 'XREORDERSYS 'DEFINED-ON-LINE '199) 
(PUT 'XREORDERSYS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'XREORDERSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XREORDERSYS (P)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F P)
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (F) (XREORDER F)) (CAR F)) NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (F) (XREORDER F)) (CAR F)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SOLVEDEDSP 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEDEDSP 'DEFINED-ON-LINE '204) 
(PUT 'SOLVEDEDSP 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'SOLVEDEDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEDEDSP (S) (KNOWNTRUEEDS S 'SOLVED)) 
(PUT 'REORDEREDS 'NUMBER-OF-ARGS 1) 
(PUT 'REORDEREDS 'DEFINED-ON-LINE '209) 
(PUT 'REORDEREDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'REORDEREDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REORDEREDS (S)
    (PROG (R K)
      (SETQ R (COPYEDS S))
      (SETQ K (RIGHTUNION KORD* (EDSCOB R)))
      (SETCAR (CDR R) (SORTSYS (XREORDERSYS (CADR R)) K))
      (SETCAR (CDDR R) (SORTSYS (XREORDERSYS (CADDR R)) K))
      (SETCAR (CDDR (CDR R)) (REORDERCFRM (CADDR (CDR R))))
      (RETURN (COND ((EQUAL R S) S) (T (NORMALEDS R)))))) 
(PUT 'SORTEDS 'NUMBER-OF-ARGS 1) 
(PUT 'SORTEDS 'DEFINED-ON-LINE '222) 
(PUT 'SORTEDS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'SORTEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORTEDS (S)
    (PROG (K)
      (SETQ S (COPYEDS S))
      (SETQ K (EDSCOB S))
      (SETCAR (CDR S) (SORTSYS (CADR S) K))
      (SETCAR (CDDR S) (SORTSYS (CADDR S) K))
      (RETURN S))) 
(PUT 'SORTSYS 'NUMBER-OF-ARGS 2) 
(PUT 'SORTSYS 'DEFINED-ON-LINE '233) 
(PUT 'SORTSYS 'DEFINED-IN-FILE 'EDS/EDSNORML.RED) 
(PUT 'SORTSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORTSYS (S C)
    ((LAMBDA (KORD*) (REVERSIP (SORT S (FUNCTION PFORDP)))) (REVERSE C))) 
(ENDMODULE) 