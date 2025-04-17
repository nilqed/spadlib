(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GLSOLVE)) 
(FLUID '(*SOLVESINGULAR VARS*)) 
(GLOBAL '(!ARBINT ASSUMPTIONS REQUIREMENTS)) 
(PUT 'GLNRSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'GLNRSOLVE 'DEFINED-ON-LINE '40) 
(PUT 'GLNRSOLVE 'DEFINED-IN-FILE 'SOLVE/GLSOLVE.RED) 
(PUT 'GLNRSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLNRSOLVE (U V)
    (PROG (SGN X Y CNDS)
      (COND ((NULL U) (GO B)))
     A
      (SETQ X (*SF2EX (CAR U) V))
      (COND ((NULL X) (SETQ U (CDR U)))
            ((INCONSISTENCY-CHK X)
             (PROGN
              (SETQ CNDS (CONS (CAR U) CNDS))
              (SETQ X NIL)
              (SETQ U (CDR U)))))
      (COND ((AND U (NULL X)) (GO A)))
     B
      (COND
       ((NULL U)
        (COND (CNDS (GO D)) (T (RETURN (CONS T (LIST (LIST NIL NIL 1))))))))
      (COND ((NULL (SETQ U (CDR U))) (GO D)))
     C
      (COND
       ((SETQ Y (SUBS2CHKEX (EXTMULT (*SF2EX (CAR U) V) X)))
        (COND
         ((INCONSISTENCY-CHK Y)
          (SETQ CNDS (CONS (CAR (CANCEL (CONS (CDAR Y) (CDAR X)))) CNDS)))
         (T
          (PROGN
           (SETQ ASSUMPTIONS
                   (PROGN
                    (SETQ ALGLIST* (CONS NIL NIL))
                    (CONS 'LIST
                          (CONS (MK*SQ (CONS (CDAR Y) 1))
                                (AND (PAIRP ASSUMPTIONS) (CDR ASSUMPTIONS))))))
           (SETQ X Y))))))
      (COND ((SETQ U (CDR U)) (GO C)))
     D
      (PROG (J)
        (SETQ J CNDS)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (SETQ REQUIREMENTS
                   (PROGN
                    (SETQ ALGLIST* (CONS NIL NIL))
                    (CONS 'LIST
                          (CONS (MK*SQ (CONS J 1))
                                (AND (PAIRP REQUIREMENTS)
                                     (CDR REQUIREMENTS)))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND (CNDS (RETURN (CONS 'INCONSISTENT NIL))))
      (COND
       ((AND (SETDIFF V (CAAR X)) (NOT *SOLVESINGULAR))
        (RETURN (CONS 'SINGULAR (LIST)))))
      (COND
       ((NULL (CDR X))
        (RETURN
         (CONS T
               (LIST
                (LIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (CAAR X))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (CONS NIL 1)) (CAR J))
                                         NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (CONS NIL 1)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (CAAR X) 1))))))
      (SETQ Y (CDAR X))
      (SETQ SGN (EVENP (LENGTH (CAAR X))))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CAAR X))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    ((LAMBDA (F)
                                       (COND ((SETQ SGN (NOT SGN)) (NEGF F))
                                             (T F)))
                                     (*EX2SF
                                      (INNPRODPEX (DELETE J (CAAR X))
                                                  (CDR X)))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            ((LAMBDA (F)
                               (COND ((SETQ SGN (NOT SGN)) (NEGF F)) (T F)))
                             (*EX2SF
                              (INNPRODPEX (DELETE J (CAAR X)) (CDR X)))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS T
             (LIST
              (LIST
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F U)
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (F) (CANCEL (CONS F Y))) (CAR F))
                                  NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (CANCEL (CONS F Y))) (CAR F)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (CAAR X) 1)))))) 
(PUT 'INCONSISTENCY-CHK 'NUMBER-OF-ARGS 1) 
(PUT 'INCONSISTENCY-CHK 'DEFINED-ON-LINE '81) 
(PUT 'INCONSISTENCY-CHK 'DEFINED-IN-FILE 'SOLVE/GLSOLVE.RED) 
(PUT 'INCONSISTENCY-CHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INCONSISTENCY-CHK (U)
    (OR (NULL U) (AND (MEMQ NIL (CAAR U)) (INCONSISTENCY-CHK (CDR U))))) 
(ENDMODULE) 