(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DISJOIN)) 
(FLUID
 '(*EDSVERBOSE *EDSDEBUG *ARBVARS *VAROPT *GROEBOPT *SOLVEINCONSISTENT DEPL*
   CFRMCRD* CFRMRSX* XVARS* *EDSSLOPPY *EDSDISJOINT)) 
(FLAG '(DISJOIN) 'OPFN) 
(PUT 'DISJOIN 'NUMBER-OF-ARGS 1) 
(PUT 'DISJOIN 'DEFINED-ON-LINE '46) 
(PUT 'DISJOIN 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'DISJOIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DISJOIN (U)
    ((LAMBDA (*EDSDISJOINT)
       (CONS 'LIST
             (PROG (RM FORALL-RESULT FORALL-ENDPTR)
               (SETQ RM
                       (EDSDISJOIN
                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                          (SETQ S (GETRLIST U))
                          (COND ((NULL S) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S) (*A2RMAP S)) (CAR S))
                                           NIL)))
                         LOOPLABEL
                          (SETQ S (CDR S))
                          (COND ((NULL S) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (S) (*A2RMAP S)) (CAR S))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
               (COND ((NULL RM) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (RM) (*RMAP2A RM)) (CAR RM))
                                     NIL)))
              LOOPLABEL
               (SETQ RM (CDR RM))
               (COND ((NULL RM) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (RM) (*RMAP2A RM)) (CAR RM)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
     T)) 
(PUT 'EDSDISJOIN 'NUMBER-OF-ARGS 1) 
(PUT 'EDSDISJOIN 'DEFINED-ON-LINE '52) 
(PUT 'EDSDISJOIN 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'EDSDISJOIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSDISJOIN (U) (COND (*EDSDISJOINT (REVERSE (EDSDISJOIN1 U (LIST)))) (T U))) 
(PUT 'EDSDISJOIN1 'NUMBER-OF-ARGS 2) 
(PUT 'EDSDISJOIN1 'DEFINED-ON-LINE '57) 
(PUT 'EDSDISJOIN1 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'EDSDISJOIN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSDISJOIN1 (U V)
    (COND ((NULL U) V) (T (EDSDISJOIN1 (CDR U) (EDSDISJOIN2 (CAR U) V))))) 
(PUT 'EDSDISJOIN2 'NUMBER-OF-ARGS 2) 
(PUT 'EDSDISJOIN2 'DEFINED-ON-LINE '64) 
(PUT 'EDSDISJOIN2 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'EDSDISJOIN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSDISJOIN2 (X V)
    (COND ((NULL V) (LIST X))
          (T
           (PROG (Y Z)
             (SETQ Y (CAR V))
             (RETURN
              (COND
               ((SETQ Z (RMAPINTERSECTION X Y))
                (APPEND (RMAPDIFFERENCE Y X)
                        (APPEND Z (EDSDISJOIN1 (RMAPDIFFERENCE X Y) (CDR V)))))
               (T (CONS Y (EDSDISJOIN2 X (CDR V)))))))))) 
(PUT 'RMAPINTERSECTION 'NUMBER-OF-ARGS 2) 
(PUT 'RMAPINTERSECTION 'DEFINED-ON-LINE '80) 
(PUT 'RMAPINTERSECTION 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'RMAPINTERSECTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RMAPINTERSECTION (X Y)
    (PROG (LHX XL Z RSX RSY MX MY)
      (SETQ RSY (PULLBACKRSX (CADR Y) (CAR X)))
      (COND ((MEMBER 0 RSY) (RETURN NIL)))
      (SETQ RSX (PULLBACKRSX (CADR X) (CAR Y)))
      (COND ((MEMBER 0 RSX) (RETURN NIL)))
      (RETURN (RMAPEVAL (LIST (APPEND (CAR X) (CAR Y)) (APPEND RSX RSY)))))) 
(PUT 'RMAPDIFFERENCE 'NUMBER-OF-ARGS 2) 
(PUT 'RMAPDIFFERENCE 'DEFINED-ON-LINE '93) 
(PUT 'RMAPDIFFERENCE 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'RMAPDIFFERENCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RMAPDIFFERENCE (X Z)
    (PROG (M)
      (SETQ M
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (CAR Z))
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (COND
                            ((SETQ S
                                     (CAR
                                      (SUBSQ
                                       (ADDSQ (SIMP* (CAR S))
                                              (NEGSQ (SIMP* (CDR S))))
                                       (CAR X))))
                             (RMAPEVAL (LIST (CAR X) (ADDRSX S (CADR X)))))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (COND
                            ((SETQ S
                                     (CAR
                                      (SUBSQ
                                       (ADDSQ (SIMP* (CAR S))
                                              (NEGSQ (SIMP* (CDR S))))
                                       (CAR X))))
                             (RMAPEVAL (LIST (CAR X) (ADDRSX S (CADR X)))))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETQ M
              (APPEND M
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S (CADR Z))
                       STARTOVER
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (S)
                                   (RMAPEVAL
                                    (LIST (CONS (CONS 0 S) (CAR X)) (CADR X))))
                                 (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ S (CDR S))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (S)
                                   (RMAPEVAL
                                    (LIST (CONS (CONS 0 S) (CAR X)) (CADR X))))
                                 (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ S (CDR S))
                        (GO LOOPLABEL))))
      (RETURN (EDSDISJOIN (PURGE M))))) 
(PUT 'RMAPEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RMAPEVAL 'DEFINED-ON-LINE '107) 
(PUT 'RMAPEVAL 'DEFINED-IN-FILE 'EDS/DISJOIN.RED) 
(PUT 'RMAPEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMAPEVAL (X)
    (PROG (XL VL)
      (SETQ VL
              (PURGE
               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                 (SETQ S (CAR X))
                 (COND ((NULL S) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (S) (CAR S)) (CAR S)) NIL)))
                LOOPLABEL
                 (SETQ S (CDR S))
                 (COND ((NULL S) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (S) (CAR S)) (CAR S)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ VL (LIST (INTERSECTION CFRMCRD* VL) (SETDIFF CFRMCRD* VL)))
      (SETQ XL
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (CAR X))
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (ADDSQ (SIMP* (CAR S))
                                           (NEGSQ (SIMP* (CDR S)))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (ADDSQ (SIMP* (CAR S)) (NEGSQ (SIMP* (CDR S)))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (PROG (S FORALL-RESULT FORALL-ENDPTR)
         (SETQ S (EDSSOLVEGRADED XL VL (CADR X)))
         (COND ((NULL S) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (S)
                             (COND ((CAR S) (CDR S))
                                   (T
                                    (PROGN
                                     (LPRIM
                                      "Could not resolve decomposition entirely")
                                     (LIST
                                      (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ Q (CDR S))
                                        (COND ((NULL Q) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (Q)
                                                            (CONS 0 (MK*SQ Q)))
                                                          (CAR Q))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ Q (CDR Q))
                                        (COND
                                         ((NULL Q) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q)
                                                    (CONS 0 (MK*SQ Q)))
                                                  (CAR Q))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))
                                      (CADR X))))))
                           (CAR S))
                          NIL)))
        LOOPLABEL
         (SETQ S (CDR S))
         (COND ((NULL S) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (S)
                     (COND ((CAR S) (CDR S))
                           (T
                            (PROGN
                             (LPRIM "Could not resolve decomposition entirely")
                             (LIST
                              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                (SETQ Q (CDR S))
                                (COND ((NULL Q) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q)
                                                    (CONS 0 (MK*SQ Q)))
                                                  (CAR Q))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ Q (CDR Q))
                                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Q) (CONS 0 (MK*SQ Q)))
                                          (CAR Q))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))
                              (CADR X))))))
                   (CAR S))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(ENDMODULE) 