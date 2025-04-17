(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TABLEAUX)) 
(PUT '*A2TAB 'NUMBER-OF-ARGS 1) 
(PUT '*A2TAB 'DEFINED-ON-LINE '44) 
(PUT '*A2TAB 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT '*A2TAB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2TAB (U)
    (COND ((NOT (EQCAR U 'MAT)) (TYPERR U 'MATRIX))
          (T
           (MKTAB
            (PROG (R FORALL-RESULT FORALL-ENDPTR)
              (SETQ R (CDR U))
              (COND ((NULL R) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (R)
                                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ F R)
                                    (COND ((NULL F) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (F)
                                                        (XPARTITOP F))
                                                      (CAR F))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ F (CDR F))
                                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (F) (XPARTITOP F))
                                              (CAR F))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR R))
                               NIL)))
             LOOPLABEL
              (SETQ R (CDR R))
              (COND ((NULL R) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (R)
                          (PROG (F FORALL-RESULT FORALL-ENDPTR)
                            (SETQ F R)
                            (COND ((NULL F) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (F) (XPARTITOP F))
                                              (CAR F))
                                             NIL)))
                           LOOPLABEL
                            (SETQ F (CDR F))
                            (COND ((NULL F) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                        (CAR R))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT '*TAB2A 'NUMBER-OF-ARGS 1) 
(PUT '*TAB2A 'DEFINED-ON-LINE '52) 
(PUT '*TAB2A 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT '*TAB2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *TAB2A (U)
    (CONS 'MAT
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R (CDR U))
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (R)
                                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ F R)
                                  (COND ((NULL F) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (F) (*PF2A F))
                                                    (CAR F))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ F (CDR F))
                                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (F) (*PF2A F)) (CAR F))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                              (CAR R))
                             NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (R)
                        (PROG (F FORALL-RESULT FORALL-ENDPTR)
                          (SETQ F R)
                          (COND ((NULL F) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (F) (*PF2A F)) (CAR F))
                                           NIL)))
                         LOOPLABEL
                          (SETQ F (CDR F))
                          (COND ((NULL F) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (F) (*PF2A F)) (CAR F)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR R))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'MKTAB 'NUMBER-OF-ARGS 1) 
(PUT 'MKTAB 'DEFINED-ON-LINE '58) 
(PUT 'MKTAB 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT 'MKTAB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKTAB (U) (CONS 'TAB U)) 
(PUT 'TABLEAUP 'NUMBER-OF-ARGS 1) 
(PUT 'TABLEAUP 'DEFINED-ON-LINE '63) 
(PUT 'TABLEAUP 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT 'TABLEAUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TABLEAUP (U) (EQCAR U 'TAB)) 
(PUT 'TABLEAU 'PSOPFN 'TABLEAUEVAL) 
(PUT 'TABLEAUEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'TABLEAUEVAL 'DEFINED-ON-LINE '70) 
(PUT 'TABLEAUEVAL 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT 'TABLEAUEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TABLEAUEVAL (S)
    (COND ((NOT (EDSP (SETQ S (REVAL1 (CAR S) T)))) (TYPERR S 'EDS))
          (T (*TAB2A (EDSPROTECT (LIST 'TABLEAU S)))))) 
(PUT 'TABLEAU 'NUMBER-OF-ARGS 1) 
(PUT 'TABLEAU 'DEFINED-ON-LINE '76) 
(PUT 'TABLEAU 'DEFINED-IN-FILE 'EDS/TABLEAUX.RED) 
(PUT 'TABLEAU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TABLEAU (S)
    (PROG (PRL IND)
      (COND
       ((OR (NOT (PFAFFIAN S)) (NOT (QUASILINEARP S)))
        (RERROR 'EDS 0 "Tableau only works for quasilinear Pfaffian systems")))
      (SETQ S (CAR (TMPIND (CLOSURE S))))
      (SETQ PRL (PRLKRNS S))
      (SETQ IND (INDKRNS S))
      (SETQ S
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (NONPFAFFPART (CADR S)))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND ((SETQ F (LINEARPART F PRL)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND ((SETQ F (LINEARPART F PRL)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (COND ((NULL S) (RETURN (MKTAB (LIST (NLIST NIL (LENGTH IND)))))))
      (RETURN
       (MKTAB
        (PROG (F FORALL-RESULT FORALL-ENDPTR)
          (SETQ F S)
          (COND ((NULL F) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F)
                              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                (SETQ I IND)
                                (COND ((NULL I) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (I)
                                                    (MULTPFSQ
                                                     (XCOEFF F (WEDGEFAX I))
                                                     (CONS (MINUS 1) 1)))
                                                  (CAR I))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ I (CDR I))
                                (COND ((NULL I) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (I)
                                            (MULTPFSQ (XCOEFF F (WEDGEFAX I))
                                                      (CONS (MINUS 1) 1)))
                                          (CAR I))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR F))
                           NIL)))
         LOOPLABEL
          (SETQ F (CDR F))
          (COND ((NULL F) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (F)
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I IND)
                        (COND ((NULL I) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (I)
                                            (MULTPFSQ (XCOEFF F (WEDGEFAX I))
                                                      (CONS (MINUS 1) 1)))
                                          (CAR I))
                                         NIL)))
                       LOOPLABEL
                        (SETQ I (CDR I))
                        (COND ((NULL I) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I)
                                    (MULTPFSQ (XCOEFF F (WEDGEFAX I))
                                              (CONS (MINUS 1) 1)))
                                  (CAR I))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                    (CAR F))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(ENDMODULE) 