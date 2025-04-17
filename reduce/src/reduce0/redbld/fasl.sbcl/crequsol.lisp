(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EQUIVALENCE)) 
(PUT 'EXTRFUN 'NUMBER-OF-ARGS 1) 
(FLAG '(EXTRFUN) 'OPFN) 
(PUT 'EXTRFUN 'DEFINED-ON-LINE '35) 
(PUT 'EXTRFUN 'DEFINED-IN-FILE 'CRACK/CREQUSOL.RED) 
(PUT 'EXTRFUN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTRFUN (SOL)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F (GETRLIST (AEVAL (LIST 'THIRD SOL))))
      (COND ((NULL F) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (F)
                          (PROGN
                           (PROG (H)
                             (SETQ H (GETRLIST (AEVAL (LIST 'REST F))))
                            LAB
                             (COND ((NULL H) (RETURN NIL)))
                             ((LAMBDA (H)
                                (AEVAL (DEPEND (LIST (LIST 'FIRST F) H))))
                              (CAR H))
                             (SETQ H (CDR H))
                             (GO LAB))
                           (AEVAL (LIST 'FIRST F))))
                        (CAR F))
                       NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN (CONS 'LIST FORALL-RESULT))))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (F)
                  (PROGN
                   (PROG (H)
                     (SETQ H (GETRLIST (AEVAL (LIST 'REST F))))
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H) (AEVAL (DEPEND (LIST (LIST 'FIRST F) H))))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (AEVAL (LIST 'FIRST F))))
                (CAR F))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'EQUIVSOL 'NUMBER-OF-ARGS 3) 
(FLAG '(EQUIVSOL) 'OPFN) 
(PUT 'EQUIVSOL 'DEFINED-ON-LINE '44) 
(PUT 'EQUIVSOL 'DEFINED-IN-FILE 'CRACK/CREQUSOL.RED) 
(PUT 'EQUIVSOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQUIVSOL (SOL1 SOL2 VL)
    (PROG (F1 F2 S1 S2 FF2 F H S V MM_ OLDTIME)
      (FLUID '(TIME_))
      (COND
       ((OR (EVALNEQ (AEVAL (LIST 'FIRST SOL1)) (AEVAL (LIST 'LIST)))
            (EVALNEQ (AEVAL (LIST 'FIRST SOL2)) (AEVAL (LIST 'LIST)))
            (EVALNEQ (AEVAL (LIST 'LENGTH (LIST 'SECOND SOL1)))
                     (AEVAL (LIST 'LENGTH (LIST 'SECOND SOL2))))
            (EVALNEQ (AEVAL (LIST 'LENGTH (LIST 'THIRD SOL1)))
                     (AEVAL (LIST 'LENGTH (LIST 'THIRD SOL2)))))
        (RETURN (AEVAL 'NIL))))
      (SETQ F1 (AEVAL (LIST 'EXTRFUN SOL1)))
      (SETQ F2 (AEVAL (LIST 'EXTRFUN SOL2)))
      (SETQ S2 (AEVAL (LIST 'SECOND SOL2)))
      (SETQ FF2
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (AEVAL F2)))
                (COND ((NULL F) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (PROGN
                                     (SETQ H (AEVAL (GENSYM)))
                                     (SETQ S2
                                             (AEVAL
                                              (LIST 'SUB (LIST 'EQUAL F H)
                                                    S2)))
                                     (SETQ S (AEVAL (LIST 'FARGS F)))
                                     (PROG (V)
                                       (SETQ V (GETRLIST (AEVAL S)))
                                      LAB
                                       (COND ((NULL V) (RETURN NIL)))
                                       ((LAMBDA (V)
                                          (AEVAL (DEPEND (LIST H V))))
                                        (CAR V))
                                       (SETQ V (CDR V))
                                       (GO LAB))
                                     (AEVAL H)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (PROGN
                             (SETQ H (AEVAL (GENSYM)))
                             (SETQ S2 (AEVAL (LIST 'SUB (LIST 'EQUAL F H) S2)))
                             (SETQ S (AEVAL (LIST 'FARGS F)))
                             (PROG (V)
                               (SETQ V (GETRLIST (AEVAL S)))
                              LAB
                               (COND ((NULL V) (RETURN NIL)))
                               ((LAMBDA (V) (AEVAL (DEPEND (LIST H V))))
                                (CAR V))
                               (SETQ V (CDR V))
                               (GO LAB))
                             (AEVAL H)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ S1
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (GETRLIST (AEVAL (LIST 'SECOND SOL1))))
                (COND ((NULL S) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (AEVAL
                                     (LIST 'DIFFERENCE (LIST 'LHS S)
                                           (LIST 'RHS S))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (AEVAL
                             (LIST 'DIFFERENCE (LIST 'LHS S) (LIST 'RHS S))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ S1 (AEVAL (LIST 'SUB S2 S1)))
      (PROGN (SETQ OLDTIME TIME_) (SETQ TIME_ NIL))
      (SETQ H (AEVAL (LIST 'CRACK S1 (LIST 'LIST) F1 VL)))
      (SETQ TIME_ OLDTIME)
      (COND
       ((OR (EVALEQUAL (AEVAL H) (AEVAL (LIST 'LIST)))
            (EVALNEQ (AEVAL (LIST 'LENGTH H)) 1))
        (RETURN (AEVAL 'NIL)))
       (T (SETQ H (AEVAL (LIST 'FIRST H)))))
      (COND
       ((OR (EVALNEQ (AEVAL (LIST 'FIRST H)) (AEVAL (LIST 'LIST)))
            (EVALNEQ (AEVAL (LIST 'THIRD H)) (AEVAL (LIST 'LIST))))
        (RETURN (AEVAL 'NIL))))
      (SETQ S2 (AEVAL (LIST 'SECOND H)))
      (SETQ H (AEVAL (LIST 'LENGTH F1)))
      (AEVAL (MATRIX (LIST (LIST 'M__ H H))))
      (PROG (F)
        (SETQ F 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* H) F)) (RETURN NIL)))
        (PROG (S)
          (SETQ S 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* H) S)) (RETURN NIL)))
          (SETK (LIST 'M__ F S)
                (AEVAL*
                 (LIST 'DF (LIST 'RHS (LIST 'PART S2 F)) (LIST 'PART FF2 S))))
          (SETQ S
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   S))
          (GO LAB))
        (SETQ F
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 F))
        (GO LAB))
      (PROG (H)
        (SETQ H (GETRLIST (AEVAL FF2)))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ S (AEVAL (LIST 'FARGS H)))
            (PROG (V)
              (SETQ V (GETRLIST (AEVAL S)))
             LAB
              (COND ((NULL V) (RETURN NIL)))
              ((LAMBDA (V) (AEVAL (NODEPEND (LIST H V)))) (CAR V))
              (SETQ V (CDR V))
              (GO LAB))
            (AEVAL 'NIL)))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN
       (COND ((EVALEQUAL (AEVAL (LIST 'DET 'M__)) 0) (AEVAL 'NIL))
             (T (AEVAL 'T)))))) 
(PUT 'COMPLETESOL 'NUMBER-OF-ARGS 1) 
(FLAG '(COMPLETESOL) 'OPFN) 
(PUT 'COMPLETESOL 'DEFINED-ON-LINE '102) 
(PUT 'COMPLETESOL 'DEFINED-IN-FILE 'CRACK/CREQUSOL.RED) 
(PUT 'COMPLETESOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPLETESOL (SOL)
    (LIST 'LIST (LIST 'FIRST SOL) (LIST 'SECOND SOL)
          (PROG (F FORALL-RESULT FORALL-ENDPTR)
            (SETQ F (GETRLIST (AEVAL (LIST 'THIRD SOL))))
            (COND ((NULL F) (RETURN (MAKELIST NIL))))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (F)
                                (AEVAL (LIST 'CONS F (LIST 'FARGS F))))
                              (CAR F))
                             NIL)))
           LOOPLABEL
            (SETQ F (CDR F))
            (COND ((NULL F) (RETURN (CONS 'LIST FORALL-RESULT))))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (F) (AEVAL (LIST 'CONS F (LIST 'FARGS F))))
                      (CAR F))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (COND
           ((EVALGREATERP (AEVAL (LIST 'LENGTH SOL)) 3)
            (AEVAL (LIST 'PART SOL 4)))
           (T (AEVAL (LIST 'LIST)))))) 
(ENDMODULE) 