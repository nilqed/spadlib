(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFNF)) 
(REVISION 'PASFNF "$Id: pasfnf.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'PASFNF
           "(c) 2001-2009 A. Dolzmann, A. Lasaruk, A. Seidl, T. Sturm, 2010-2020 T. Sturm") 
(PUT 'PASF_PNF 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PNF 'DEFINED-ON-LINE '35) 
(PUT 'PASF_PNF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_PNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PNF (PHI) (PASF_PNF1 (RL_NNF PHI))) 
(PUT 'PASF_PNF1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PNF1 'DEFINED-ON-LINE '40) 
(PUT 'PASF_PNF1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_PNF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PNF1 (PHI)
    ((LAMBDA (ERG)
       (PROGN
        (COND
         ((OR (NULL (CDR ERG))
              (LESSP (PASF_QB (CAR ERG)) (PASF_QB (CADR ERG))))
          (CAR ERG))
         (T (CADR ERG)))))
     (PASF_PNF2 (CL_RENAME-VARS PHI)))) 
(PUT 'PASF_PNF2 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PNF2 'DEFINED-ON-LINE '51) 
(PUT 'PASF_PNF2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_PNF2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PNF2 (PHI)
    (PROG (OP)
      (SETQ OP (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (COND
       ((OR (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
        (RETURN (PASF_PNF2-QUANTIFIER PHI))))
      (COND ((OR (EQ OP 'OR) (EQ OP 'AND)) (RETURN (PASF_PNF2-JUNCTOR PHI))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN (LIST PHI))))
      (COND
       ((OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
            (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
            (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
        (REDERR (LIST "pasf_pnf2():" OP "invalid as operator"))))
      (RETURN (LIST PHI)))) 
(PUT 'PASF_PNF2-QUANTIFIER 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PNF2-QUANTIFIER 'DEFINED-ON-LINE '68) 
(PUT 'PASF_PNF2-QUANTIFIER 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_PNF2-QUANTIFIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PNF2-QUANTIFIER (PHI)
    (PROG (PNFMAT TP)
      (SETQ PNFMAT (PASF_PNF2 (CADDR PHI)))
      (RETURN
       (COND
        ((OR (NULL (CDR PNFMAT))
             (AND (MEMQ (COND ((ATOM PHI) PHI) (T (CAR PHI))) '(ALL BALL))
                  (MEMQ
                   (COND ((ATOM (CAR PNFMAT)) (CAR PNFMAT))
                         (T (CAR (CAR PNFMAT))))
                   '(ALL BALL)))
             (AND (MEMQ (COND ((ATOM PHI) PHI) (T (CAR PHI))) '(EX BEX))
                  (MEMQ
                   (COND ((ATOM (CAR PNFMAT)) (CAR PNFMAT))
                         (T (CAR (CAR PNFMAT))))
                   '(EX BEX))))
         (COND
          (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
            (COND ((ATOM PHI) PHI) (T (CAR PHI))))
           (LIST
            (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI) (CAR PNFMAT)
                  (RL_PNF (CADDDR PHI)))))
          (T
           (LIST
            (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI)
                  (CAR PNFMAT))))))
        (T
         (COND
          (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
            (COND ((ATOM PHI) PHI) (T (CAR PHI))))
           (LIST
            (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI)
                  (CADR PNFMAT) (RL_PNF (CADDDR PHI)))))
          (T
           (LIST
            (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI)
                  (CADR PNFMAT)))))))))) 
(PUT 'PASF_PNF2-JUNCTOR 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_PNF2-JUNCTOR 'DEFINED-ON-LINE '90) 
(PUT 'PASF_PNF2-JUNCTOR 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_PNF2-JUNCTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_PNF2-JUNCTOR (PHI)
    (PROG (ARGS JUNCTOR E L1 L2 ONLYEX ONLYALL PHI1 PHI2 M QB)
      (SETQ M 0)
      (SETQ QB 0)
      (SETQ JUNCTOR (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (SETQ ARGS (CDR PHI))
      (SETQ E
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F ARGS)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (PASF_PNF2 F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (PASF_PNF2 F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ONLYEX T)
      (SETQ ONLYALL T)
      (PROG (EJ)
        (SETQ EJ E)
       LAB
        (COND ((NULL EJ) (RETURN NIL)))
        ((LAMBDA (EJ)
           (PROGN
            (SETQ QB (PASF_QB (CAR EJ)))
            (COND
             ((GREATERP QB M)
              (PROGN (SETQ M QB) (SETQ ONLYEX T) (SETQ ONLYALL T))))
            (COND
             ((CDR EJ)
              (PROGN
               (SETQ L1 (CONS (CAR EJ) L1))
               (SETQ L2 (CONS (CADR EJ) L2))))
             (T
              (PROGN
               (SETQ L1 (CONS (CAR EJ) L1))
               (SETQ L2 (CONS (CAR EJ) L2)))))
            (COND
             ((EQN M QB)
              (PROGN
               (COND
                ((OR
                  (EQ (COND ((ATOM (CAR L1)) (CAR L1)) (T (CAR (CAR L1))))
                      'ALL)
                  (EQ (COND ((ATOM (CAR L1)) (CAR L1)) (T (CAR (CAR L1))))
                      'BALL))
                 (SETQ ONLYEX NIL)))
               (COND
                ((OR
                  (EQ (COND ((ATOM (CAR L2)) (CAR L2)) (T (CAR (CAR L2)))) 'EX)
                  (EQ (COND ((ATOM (CAR L1)) (CAR L1)) (T (CAR (CAR L1))))
                      'BEX))
                 (SETQ ONLYALL NIL))))))))
         (CAR EJ))
        (SETQ EJ (CDR EJ))
        (GO LAB))
      (SETQ L1 (REVERSIP L1))
      (SETQ L2 (REVERSIP L2))
      (COND ((EQN M 0) (RETURN (LIST PHI))))
      (COND
       ((NEQ ONLYEX ONLYALL)
        (COND (ONLYEX (RETURN (LIST (PASF_INTERCHANGE L1 JUNCTOR 'EX))))
              (T (RETURN (LIST (PASF_INTERCHANGE L2 JUNCTOR 'ALL)))))))
      (SETQ PHI1 (PASF_INTERCHANGE L1 JUNCTOR 'EX))
      (SETQ PHI2 (PASF_INTERCHANGE L2 JUNCTOR 'ALL))
      (COND ((EQ (CAR PHI1) (CAR PHI2)) (RETURN (LIST PHI1)))
            (T (RETURN (LIST PHI1 PHI2)))))) 
(PUT 'PASF_QB 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_QB 'DEFINED-ON-LINE '137) 
(PUT 'PASF_QB 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_QB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_QB (PHI)
    (PROG (Q TP QB)
      (SETQ QB 0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (OR
            ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
             (COND ((ATOM PHI) PHI) (T (CAR PHI))))
            ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
             (COND ((ATOM PHI) PHI) (T (CAR PHI))))))
          (RETURN NIL)))
        (PROGN
         (SETQ TP
                 (COND
                  ((MEMQ (COND ((ATOM PHI) PHI) (T (CAR PHI))) '(BALL ALL))
                   'ALL)
                  (T 'EX)))
         (COND
          ((NEQ TP Q)
           (PROGN
            (SETQ QB (PLUS QB 1))
            (SETQ Q
                    (COND
                     ((MEMQ (COND ((ATOM PHI) PHI) (T (CAR PHI))) '(BALL ALL))
                      'ALL)
                     (T 'EX))))))
         (SETQ PHI (CADDR PHI)))
        (GO WHILELABEL))
      (RETURN QB))) 
(PUT 'PASF_INTERCHANGE 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_INTERCHANGE 'DEFINED-ON-LINE '155) 
(PUT 'PASF_INTERCHANGE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_INTERCHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_INTERCHANGE (L JUNCTOR A)
    (PROG (QL B RESULT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (PASF_CONTAINS-QUANTIFIER L)) (RETURN NIL)))
        (PROGN
         (SETQ L
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F L)
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (F)
                                       (PROGN
                                        (PROG ()
                                         WHILELABEL
                                          (COND
                                           ((NOT
                                             (OR
                                              (AND (EQ A 'ALL)
                                                   (MEMQ
                                                    (COND ((ATOM F) F)
                                                          (T (CAR F)))
                                                    '(BALL ALL)))
                                              (AND (EQ A 'EX)
                                                   (MEMQ
                                                    (COND ((ATOM F) F)
                                                          (T (CAR F)))
                                                    '(BEX EX)))))
                                            (RETURN NIL)))
                                          (PROGN
                                           (SETQ B
                                                   (CONS
                                                    (LIST
                                                     (COND ((ATOM F) F)
                                                           (T (CAR F)))
                                                     (CADR F)
                                                     (COND
                                                      (((LAMBDA (X)
                                                          (OR (EQ X 'BEX)
                                                              (EQ X 'BALL)))
                                                        (COND ((ATOM F) F)
                                                              (T (CAR F))))
                                                       (CADDDR F))
                                                      (T NIL)))
                                                    B))
                                           (SETQ F (CADDR F)))
                                          (GO WHILELABEL))
                                        F))
                                     (CAR F))
                                    NIL)))
                  LOOPLABEL
                   (SETQ F (CDR F))
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (F)
                               (PROGN
                                (PROG ()
                                 WHILELABEL
                                  (COND
                                   ((NOT
                                     (OR
                                      (AND (EQ A 'ALL)
                                           (MEMQ
                                            (COND ((ATOM F) F) (T (CAR F)))
                                            '(BALL ALL)))
                                      (AND (EQ A 'EX)
                                           (MEMQ
                                            (COND ((ATOM F) F) (T (CAR F)))
                                            '(BEX EX)))))
                                    (RETURN NIL)))
                                  (PROGN
                                   (SETQ B
                                           (CONS
                                            (LIST
                                             (COND ((ATOM F) F) (T (CAR F)))
                                             (CADR F)
                                             (COND
                                              (((LAMBDA (X)
                                                  (OR (EQ X 'BEX)
                                                      (EQ X 'BALL)))
                                                (COND ((ATOM F) F)
                                                      (T (CAR F))))
                                               (CADDDR F))
                                              (T NIL)))
                                            B))
                                   (SETQ F (CADDR F)))
                                  (GO WHILELABEL))
                                F))
                             (CAR F))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ QL (CONS B QL))
         (SETQ B NIL)
         (SETQ A (CL_FLIP A)))
        (GO WHILELABEL))
      (SETQ RESULT (CONS JUNCTOR L))
      (PROG (B)
        (SETQ B QL)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (PROG (V)
              (SETQ V B)
             LAB
              (COND ((NULL V) (RETURN NIL)))
              ((LAMBDA (V)
                 (COND
                  ((NULL (CADDR V))
                   (SETQ RESULT (LIST (CAR V) (CADR V) RESULT)))
                  (T (SETQ RESULT (LIST (CAR V) (CADR V) RESULT (CADDR V))))))
               (CAR V))
              (SETQ V (CDR V))
              (GO LAB))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (RETURN RESULT))) 
(PUT 'PASF_CONTAINS-QUANTIFIER 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CONTAINS-QUANTIFIER 'DEFINED-ON-LINE '187) 
(PUT 'PASF_CONTAINS-QUANTIFIER 'DEFINED-IN-FILE 'REDLOG/PASF/PASFNF.RED) 
(PUT 'PASF_CONTAINS-QUANTIFIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CONTAINS-QUANTIFIER (L)
    (AND L
         (OR
          ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
           (COND ((ATOM (CAR L)) (CAR L)) (T (CAR (CAR L)))))
          ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
           (COND ((ATOM (CAR L)) (CAR L)) (T (CAR (CAR L)))))
          (PASF_CONTAINS-QUANTIFIER (CDR L))))) 
(ENDMODULE) 