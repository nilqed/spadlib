(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLNF)) 
(REVISION 'CLNF "$Id: clnf.red 5988 2021-08-28 17:06:17Z thomas-sturm $") 
(COPYRIGHT 'CLNF "(c) 1995-2021 A. Dolzmann, T. Sturm") 
(PUT 'CL_EXPAND-EXTBOOL 'NUMBER-OF-ARGS 1) 
(PUT 'CL_EXPAND-EXTBOOL 'DEFINED-ON-LINE '32) 
(PUT 'CL_EXPAND-EXTBOOL 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_EXPAND-EXTBOOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_EXPAND-EXTBOOL (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (CL_EXPAND-EXTBOOL (CADDR F))))))
      (COND
       ((OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
        (RETURN
         (CONS OP
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR F))
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SUBF) (CL_EXPAND-EXTBOOL SUBF))
                                   (CAR SUBF))
                                  NIL)))
                LOOPLABEL
                 (SETQ SUBF (CDR SUBF))
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (SUBF) (CL_EXPAND-EXTBOOL SUBF)) (CAR SUBF))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND
       ((EQ OP 'IMPL)
        (RETURN
         (CL_EXPAND-EXTBOOL (LIST 'OR (LIST 'NOT (CADR F)) (CADDR F))))))
      (COND
       ((EQ OP 'REPL)
        (RETURN
         (CL_EXPAND-EXTBOOL (LIST 'OR (CADR F) (LIST 'NOT (CADDR F)))))))
      (COND
       ((EQ OP 'EQUIV)
        (RETURN
         (CL_EXPAND-EXTBOOL
          (CONS 'AND
                (LIST (LIST 'IMPL (CADR F) (CADDR F))
                      (LIST 'REPL (CADR F) (CADDR F))))))))
      (RETURN F))) 
(PUT 'CL_NNF 'NUMBER-OF-ARGS 1) 
(DE CL_NNF (F) (CL_NNF1 F T)) 
(PUT 'CL_NNFNOT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_NNFNOT 'DEFINED-ON-LINE '58) 
(PUT 'CL_NNFNOT 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_NNFNOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_NNFNOT (F) (CL_NNF1 F NIL)) 
(PUT 'CL_NNF1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_NNF1 'DEFINED-ON-LINE '64) 
(PUT 'CL_NNF1 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_NNF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_NNF1 (F FLAG)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((EQ OP 'NOT) (RETURN (CL_NNF1 (CADR F) (NOT FLAG)))))
      (COND
       ((EQ OP 'IMPL)
        (RETURN
         (CONS (CL_CFLIP 'OR FLAG)
               (LIST (CL_NNF1 (CADR F) (NOT FLAG))
                     (CL_NNF1 (CADDR F) FLAG))))))
      (COND
       ((EQ OP 'REPL)
        (RETURN
         (CONS (CL_CFLIP 'OR FLAG)
               (LIST (CL_NNF1 (CADR F) FLAG)
                     (CL_NNF1 (CADDR F) (NOT FLAG)))))))
      (COND
       ((EQ OP 'EQUIV)
        (RETURN
         (CONS (CL_CFLIP 'OR FLAG)
               (LIST
                (CONS (CL_CFLIP 'AND FLAG)
                      (LIST (CL_NNF1 (CADR F) FLAG) (CL_NNF1 (CADDR F) FLAG)))
                (CONS (CL_CFLIP 'AND FLAG)
                      (LIST (CL_NNF1 (CADR F) (NOT FLAG))
                            (CL_NNF1 (CADDR F) (NOT FLAG)))))))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN (CL_CFLIP F FLAG))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST (CL_CFLIP OP FLAG) (CADR F) (CL_NNF1 (CADDR F) FLAG)))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQNNF1 F FLAG))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (RETURN
         (CONS (CL_CFLIP OP FLAG)
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR F))
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SUBF) (CL_NNF1 SUBF FLAG))
                                   (CAR SUBF))
                                  NIL)))
                LOOPLABEL
                 (SETQ SUBF (CDR SUBF))
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (SUBF) (CL_NNF1 SUBF FLAG)) (CAR SUBF))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (COND (FLAG F) (T (RL_NEGATEAT F)))))) 
(PUT 'CL_PNF 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PNF 'DEFINED-ON-LINE '93) 
(PUT 'CL_PNF 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_PNF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PNF (PHI) (CL_PNF1 (RL_NNF PHI))) 
(PUT 'CL_PNF1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PNF1 'DEFINED-ON-LINE '100) 
(PUT 'CL_PNF1 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_PNF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PNF1 (PHI)
    ((LAMBDA (ERG)
       (PROGN
        (COND
         ((OR (NULL (CDR ERG)) (LESSP (CL_QB (CAR ERG)) (CL_QB (CADR ERG))))
          (CAR ERG))
         (T (CADR ERG)))))
     (CL_PNF2 (CL_RENAME-VARS PHI)))) 
(PUT 'CL_PNF2 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PNF2 'DEFINED-ON-LINE '114) 
(PUT 'CL_PNF2 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_PNF2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PNF2 (PHI)
    (PROG (OP W)
      (SETQ OP (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (COND ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN (CL_PNF2-QUANTIFIER PHI))))
      (COND ((OR (EQ OP 'OR) (EQ OP 'AND)) (RETURN (CL_PNF2-JUNCTOR PHI))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN (LIST PHI))))
      (COND
       ((OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
            (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
            (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
        (REDERR (LIST "cl_pnf2():" OP "invalid as operator"))))
      (RETURN (LIST PHI)))) 
(PUT 'CL_PNF2-QUANTIFIER 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PNF2-QUANTIFIER 'DEFINED-ON-LINE '128) 
(PUT 'CL_PNF2-QUANTIFIER 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_PNF2-QUANTIFIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PNF2-QUANTIFIER (PHI)
    ((LAMBDA (E)
       (PROGN
        (COND
         ((OR (NULL (CDR E))
              (EQ (COND ((ATOM PHI) PHI) (T (CAR PHI)))
                  (COND ((ATOM (CAR E)) (CAR E)) (T (CAR (CAR E))))))
          (LIST
           (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI) (CAR E))))
         (T
          (LIST
           (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI)
                 (CADR E)))))))
     (CL_PNF2 (CADDR PHI)))) 
(PUT 'CL_PNF2-JUNCTOR 'NUMBER-OF-ARGS 1) 
(PUT 'CL_PNF2-JUNCTOR 'DEFINED-ON-LINE '136) 
(PUT 'CL_PNF2-JUNCTOR 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_PNF2-JUNCTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_PNF2-JUNCTOR (PHI)
    (PROG (JUNCTOR E L1 L2 ONLYEX ONLYALL PHI1 PHI2 M QB)
      (SETQ M 0)
      (SETQ QB 0)
      (SETQ JUNCTOR (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (SETQ E
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CDR PHI))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (CL_PNF2 F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (CL_PNF2 F)) (CAR F)) NIL))
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
            (SETQ QB (CL_QB (CAR EJ)))
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
                ((EQ (COND ((ATOM (CAR L1)) (CAR L1)) (T (CAR (CAR L1)))) 'ALL)
                 (SETQ ONLYEX NIL)))
               (COND
                ((EQ (COND ((ATOM (CAR L2)) (CAR L2)) (T (CAR (CAR L2)))) 'EX)
                 (SETQ ONLYALL NIL))))))
            NIL))
         (CAR EJ))
        (SETQ EJ (CDR EJ))
        (GO LAB))
      (SETQ L1 (REVERSIP L1))
      (SETQ L2 (REVERSIP L2))
      (COND ((EQN M 0) (RETURN (LIST PHI))))
      (COND
       ((NEQ ONLYEX ONLYALL)
        (COND (ONLYEX (RETURN (LIST (CL_INTERCHANGE L1 JUNCTOR 'EX))))
              (T (RETURN (LIST (CL_INTERCHANGE L2 JUNCTOR 'ALL)))))))
      (SETQ PHI1 (CL_INTERCHANGE L1 JUNCTOR 'EX))
      (SETQ PHI2 (CL_INTERCHANGE L2 JUNCTOR 'ALL))
      (COND ((EQ (CAR PHI1) (CAR PHI2)) (RETURN (LIST PHI1)))
            (T (RETURN (LIST PHI1 PHI2)))))) 
(PUT 'CL_QB 'NUMBER-OF-ARGS 1) 
(PUT 'CL_QB 'DEFINED-ON-LINE '174) 
(PUT 'CL_QB 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_QB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_QB (PHI)
    (PROG (Q SCAN-Q QB)
      (SETQ QB 0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
            (SETQ SCAN-Q (COND ((ATOM PHI) PHI) (T (CAR PHI))))))
          (RETURN NIL)))
        (PROGN
         (COND ((NEQ SCAN-Q Q) (PROGN (SETQ QB (PLUS QB 1)) (SETQ Q SCAN-Q))))
         (SETQ PHI (CADDR PHI)))
        (GO WHILELABEL))
      (RETURN QB))) 
(PUT 'CL_INTERCHANGE 'NUMBER-OF-ARGS 3) 
(PUT 'CL_INTERCHANGE 'DEFINED-ON-LINE '186) 
(PUT 'CL_INTERCHANGE 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_INTERCHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_INTERCHANGE (L JUNCTOR A)
    (PROG (QL B RESULT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CL_CONTAINS-QUANTIFIER L)) (RETURN NIL)))
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
                                             (EQ
                                              (COND ((ATOM F) F) (T (CAR F)))
                                              A))
                                            (RETURN NIL)))
                                          (PROGN
                                           (SETQ B (CONS (CADR F) B))
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
                                     (EQ (COND ((ATOM F) F) (T (CAR F))) A))
                                    (RETURN NIL)))
                                  (PROGN
                                   (SETQ B (CONS (CADR F) B))
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
      (SETQ A (CL_FLIP A))
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
              ((LAMBDA (V) (SETQ RESULT (LIST A V RESULT))) (CAR V))
              (SETQ V (CDR V))
              (GO LAB))
            (SETQ A (CL_FLIP A))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (RETURN RESULT))) 
(PUT 'CL_CONTAINS-QUANTIFIER 'NUMBER-OF-ARGS 1) 
(PUT 'CL_CONTAINS-QUANTIFIER 'DEFINED-ON-LINE '209) 
(PUT 'CL_CONTAINS-QUANTIFIER 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_CONTAINS-QUANTIFIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_CONTAINS-QUANTIFIER (L)
    (AND L
         (OR
          ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
           (COND ((ATOM (CAR L)) (CAR L)) (T (CAR (CAR L)))))
          (CL_CONTAINS-QUANTIFIER (CDR L))))) 
(PUT 'CL_RENAME-VARS 'NUMBER-OF-ARGS 1) 
(DE CL_RENAME-VARS (F) (CAR (CL_RENAME-VARS1 F (CL_REPLACE-VARL F)))) 
(PUT 'CL_REPLACE-VARL 'NUMBER-OF-ARGS 1) 
(DE CL_REPLACE-VARL (F)
    (PROG (FVL BVL AVL X REPLACEL)
      (PROG (G135)
        (SETQ G135 (CL_VARL1 F))
        (SETQ FVL (CAR G135))
        (SETQ BVL (CDR G135))
        (RETURN G135))
      (SETQ AVL (APPEND FVL BVL))
      (PROG ()
       WHILELABEL
        (COND ((NOT BVL) (RETURN NIL)))
        (PROGN
         (SETQ X (PROG1 (CAR BVL) (SETQ BVL (CDR BVL))))
         (COND
          ((MEMQ X FVL)
           (PROG (W1)
             (SETQ W1 (CONS X 1))
             (SETQ REPLACEL (CONS W1 REPLACEL))
             (RETURN W1)))
          (T
           (PROG (W1)
             (SETQ W1 (CONS X 0))
             (SETQ REPLACEL (CONS W1 REPLACEL))
             (RETURN W1)))))
        (GO WHILELABEL))
      (RETURN (CONS AVL REPLACEL)))) 
(PUT 'CL_RENAME-VARS1 'NUMBER-OF-ARGS 2) 
(DE CL_RENAME-VARS1 (F VL)
    (PROG (OP W RNF RNB NVAR)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (PROGN
         (PROG (FF)
           (SETQ FF (CDR F))
          LAB
           (COND ((NULL FF) (RETURN NIL)))
           ((LAMBDA (FF)
              (PROGN
               (PROG (G136)
                 (SETQ G136 (CL_RENAME-VARS1 FF VL))
                 (SETQ RNF (CAR G136))
                 (SETQ VL (CDR G136))
                 (RETURN G136))
               (PROGN (SETQ W (CONS RNF W)) RNF)))
            (CAR FF))
           (SETQ FF (CDR FF))
           (GO LAB))
         (RETURN (CONS (CONS OP (REVERSIP W)) VL)))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (PROG (G137)
           (SETQ G137 (CL_RENAME-VARS1 (CADDR F) VL))
           (SETQ RNF (CAR G137))
           (SETQ VL (CDR G137))
           (RETURN G137))
         (SETQ W (ASSOC (CADR F) (CDR VL)))
         (COND
          (W
           (PROGN
            (COND
             ((EQN (CDR W) 0)
              (PROGN (SETCDR W 1) (RETURN (CONS (LIST OP (CADR F) RNF) VL)))))
            (PROG ()
             REPEATLABEL
              (PROGN
               (SETQ NVAR (MKID (CAR W) (CDR W)))
               (SETCDR W (PLUS (CDR W) 1)))
              (COND
               ((NOT (NOT (OR (MEMQ NVAR (CAR VL)) (GET NVAR 'AVALUE))))
                (GO REPEATLABEL))))
            (PROGN (SETCAR VL (CONS NVAR (CAR VL))) NVAR)
            (SETQ RNF (CL_APPLY2ATS1 RNF 'RL_VARSUBSTAT (LIST NVAR (CAR W))))
            (RETURN (CONS (LIST OP NVAR RNF) VL)))))
         (RETURN (CONS (LIST OP (CADR F) RNF) VL)))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQRENAME-VARS1 F VL))))
      (RETURN (CONS F VL)))) 
(PUT 'CL_FVARL 'NUMBER-OF-ARGS 1) 
(PUT 'CL_FVARL 'DEFINED-ON-LINE '266) 
(PUT 'CL_FVARL 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_FVARL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_FVARL (F) (SORT (CL_FVARL1 F) 'ORDP)) 
(PUT 'CL_FVARL1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_FVARL1 'DEFINED-ON-LINE '271) 
(PUT 'CL_FVARL1 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_FVARL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_FVARL1 (F) (CAR (CL_VARL1 F))) 
(PUT 'CL_BVARL 'NUMBER-OF-ARGS 1) 
(PUT 'CL_BVARL 'DEFINED-ON-LINE '276) 
(PUT 'CL_BVARL 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_BVARL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_BVARL (F) (SORT (CL_BVARL1 F) 'ORDP)) 
(PUT 'CL_BVARL1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_BVARL1 'DEFINED-ON-LINE '281) 
(PUT 'CL_BVARL1 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_BVARL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_BVARL1 (F) (CDR (CL_VARL1 F))) 
(PUT 'CL_VARL 'NUMBER-OF-ARGS 1) 
(DE CL_VARL (F)
    ((LAMBDA (W) (CONS (SORT (CAR W) 'ORDP) (SORT (CDR W) 'ORDP)))
     (CL_VARL1 F))) 
(PUT 'CL_VARL1 'NUMBER-OF-ARGS 1) 
(DE CL_VARL1 (F) (CL_VARL2 F NIL NIL NIL)) 
(PUT 'CL_VARL2 'NUMBER-OF-ARGS 4) 
(DE CL_VARL2 (F FVL CBVL BVL)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN (CONS FVL BVL))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (PROGN
         (PROG (S)
           (SETQ S (CDR F))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (PROG (G138)
                (SETQ G138 (CL_VARL2 S FVL CBVL BVL))
                (SETQ FVL (CAR G138))
                (SETQ BVL (CDR G138))
                (RETURN G138)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (RETURN (CONS FVL BVL)))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (CL_VARL2 (CADDR F) FVL (LTO_INSERTQ (CADR F) CBVL) BVL))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQVARL2 F FVL CBVL BVL))))
      (PROG (V)
        (SETQ V (RL_VARLAT F))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (COND ((MEMQ V CBVL) (SETQ BVL (LTO_INSERTQ V BVL)))
                 (T (SETQ FVL (LTO_INSERTQ V FVL)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS FVL BVL)))) 
(PUT 'CL_QVARL 'NUMBER-OF-ARGS 1) 
(DE CL_QVARL (F) (SORT (CL_QVARL1 F) (FUNCTION ORDP))) 
(PUT 'CL_QVARL1 'NUMBER-OF-ARGS 1) 
(DE CL_QVARL1 (F)
    (PROG (OP QVL)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LTO_INSERTQ (CADR F) (CL_QVARL1 (CADDR F))))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQQVARL1 F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (PROGN
         (PROG (S)
           (SETQ S (CDR F))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S) (SETQ QVL (UNION QVL (CL_QVARL1 S)))) (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (RETURN QVL))))
      (RETURN NIL))) 
(RL_PROVIDESERVICE 'RL_MINISCOPE 'CL_APNF '(RL_VARLAT)) 
(PUT 'CL_APNF 'NUMBER-OF-ARGS 1) 
(DE CL_APNF (PHI)
    (PROG (OP)
      (SETQ PHI (RL_NNF PHI))
      (SETQ OP (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (COND ((EQ OP 'EX) (RETURN (CL_APNF1 (CADR PHI) (CL_APNF (CADDR PHI))))))
      (COND
       ((EQ OP 'ALL)
        (RETURN
         (RL_NNFNOT (CL_APNF1 (CADR PHI) (CL_APNF (CADDR (RL_NNFNOT PHI))))))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (RETURN
         (CONS OP
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR PHI))
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SUBF) (CL_APNF SUBF)) (CAR SUBF))
                                  NIL)))
                LOOPLABEL
                 (SETQ SUBF (CDR SUBF))
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (SUBF) (CL_APNF SUBF)) (CAR SUBF))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN PHI))) 
(PUT 'CL_APNF1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_APNF1 'DEFINED-ON-LINE '362) 
(PUT 'CL_APNF1 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_APNF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_APNF1 (VAR PHI)
    (PROG (OP NF OCCURL NOCCURL)
      (SETQ OP (COND ((ATOM PHI) PHI) (T (CAR PHI))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN PHI)))
      (COND
       ((EQ OP 'EX) (RETURN (LIST 'EX (CADR PHI) (CL_APNF1 VAR (CADDR PHI))))))
      (COND
       ((EQ OP 'ALL)
        (RETURN (COND ((CL_FREEVP VAR PHI) (LIST 'EX VAR PHI)) (T PHI)))))
      (COND
       ((EQ OP 'OR)
        (PROGN
         (SETQ NF
                 (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SUBF (CDR PHI))
                   (COND ((NULL SUBF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SUBF) (CL_APNF1 VAR SUBF))
                                     (CAR SUBF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SUBF (CDR SUBF))
                   (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBF) (CL_APNF1 VAR SUBF)) (CAR SUBF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN (CONS 'OR NF)))))
      (COND
       ((EQ OP 'AND)
        (PROGN
         (PROG (SUBF)
           (SETQ SUBF (CDR PHI))
          LAB
           (COND ((NULL SUBF) (RETURN NIL)))
           ((LAMBDA (SUBF)
              (COND ((CL_FREEVP VAR SUBF) (SETQ OCCURL (CONS SUBF OCCURL)))
                    (T (SETQ NOCCURL (CONS SUBF NOCCURL)))))
            (CAR SUBF))
           (SETQ SUBF (CDR SUBF))
           (GO LAB))
         (COND
          (OCCURL
           (PROGN
            (SETQ NF
                    (COND
                     ((CDR OCCURL)
                      (LIST 'EX VAR (CONS 'AND (REVERSIP OCCURL))))
                     (T (CL_APNF1 VAR (CAR OCCURL)))))
            (SETQ NOCCURL (CONS NF NOCCURL)))))
         (RETURN
          ((LAMBDA (G140)
             (COND ((AND G140 (CDR G140)) (CONS 'AND G140))
                   ((NULL G140) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                   (T (CAR G140))))
           (REVERSIP NOCCURL))))))
      (COND ((MEMQ VAR (RL_VARLAT PHI)) (RETURN (LIST 'EX VAR PHI))))
      (RETURN PHI))) 
(PUT 'CL_FREEVP 'NUMBER-OF-ARGS 2) 
(PUT 'CL_FREEVP 'DEFINED-ON-LINE '404) 
(PUT 'CL_FREEVP 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_FREEVP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_FREEVP (VAR PHI)
    (PROG (ARGL FLAG)
      (COND
       (((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
         (COND ((ATOM PHI) PHI) (T (CAR PHI))))
        (PROGN
         (COND ((EQ VAR (CADR PHI)) (RETURN NIL)))
         (RETURN (CL_FREEVP VAR (CADDR PHI))))))
      (COND ((CL_ATFP PHI) (RETURN (MEMQ VAR (RL_VARLAT PHI)))))
      (SETQ ARGL (CDR PHI))
      (PROG ()
       WHILELABEL
        (COND ((NOT ARGL) (RETURN NIL)))
        (COND
         ((CL_FREEVP VAR (CAR ARGL)) (PROGN (SETQ FLAG T) (SETQ ARGL NIL)))
         (T (SETQ ARGL (CDR ARGL))))
        (GO WHILELABEL))
      (RETURN FLAG))) 
(RL_PROVIDESERVICE 'RL_TNF 'CL_TNF '(RL_T2CDL)) 
(PUT 'CL_TNF 'NUMBER-OF-ARGS 2) 
(PUT 'CL_TNF 'DEFINED-ON-LINE '427) 
(PUT 'CL_TNF 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_TNF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_TNF (F TERML) (COND (*RLTNFT (CL_TNFT F TERML)) (T (CL_TNFF F TERML)))) 
(PUT 'CL_TNFF 'NUMBER-OF-ARGS 2) 
(PUT 'CL_TNFF 'DEFINED-ON-LINE '433) 
(PUT 'CL_TNFF 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_TNFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_TNFF (F TERML)
    (PROG (W THEOL RESL DPTH)
      (SETQ THEOL
              (CL_BNF-CARTPROD
               (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                 (SETQ TERM TERML)
                 (COND ((NULL TERM) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (TERM) (RL_T2CDL TERM)) (CAR TERM))
                                  NIL)))
                LOOPLABEL
                 (SETQ TERM (CDR TERM))
                 (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (TERM) (RL_T2CDL TERM)) (CAR TERM))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND (*RLVERBOSE (SETQ DPTH (LENGTH THEOL))))
      (PROG (THEO)
        (SETQ THEO THEOL)
       LAB
        (COND ((NULL THEO) (RETURN NIL)))
        ((LAMBDA (THEO)
           (PROGN
            (COND
             (*RLVERBOSE
              (PROGN
               (IOTO_PRIN2 (LIST "[" DPTH))
               (SETQ DPTH (DIFFERENCE DPTH 1)))))
            (SETQ W (RL_SIMPL F THEO (MINUS 1)))
            (COND
             ((EQ W 'TRUE)
              (PROGN
               (SETQ RESL
                       (CONS
                        (COND ((AND THEO (CDR THEO)) (CONS 'AND THEO))
                              ((NULL THEO)
                               (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                              (T (CAR THEO)))
                        RESL))
               (COND (*RLVERBOSE (IOTO_PRIN2 "+] ")))))
             ((EQ W 'INCTHEO) (COND (*RLVERBOSE (IOTO_PRIN2 "!] "))))
             ((NEQ W 'FALSE)
              (PROGN
               (SETQ RESL
                       (CONS
                        ((LAMBDA (G142)
                           (COND ((AND G142 (CDR G142)) (CONS 'AND G142))
                                 ((NULL G142)
                                  (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                 (T (CAR G142))))
                         (CONS W THEO))
                        RESL))
               (COND (*RLVERBOSE (IOTO_PRIN2 ".] ")))))
             (*RLVERBOSE (IOTO_PRIN2 "-] ")))))
         (CAR THEO))
        (SETQ THEO (CDR THEO))
        (GO LAB))
      (RETURN
       (COND ((AND RESL (CDR RESL)) (CONS 'OR RESL))
             ((NULL RESL) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR RESL)))))) 
(PUT 'CL_TNFT 'NUMBER-OF-ARGS 2) 
(PUT 'CL_TNFT 'DEFINED-ON-LINE '460) 
(PUT 'CL_TNFT 'DEFINED-IN-FILE 'REDLOG/CL/CLNF.RED) 
(PUT 'CL_TNFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_TNFT (F TERML)
    (PROG (W CDL CD RVL)
      (COND ((NULL TERML) (RETURN F)))
      (SETQ CDL (RL_T2CDL (CAR TERML)))
      (PROG ()
       WHILELABEL
        (COND ((NOT CDL) (RETURN NIL)))
        (PROGN
         (SETQ CD (CAR CDL))
         (SETQ CDL (CDR CDL))
         (SETQ W (RL_SIMPL (LIST 'AND CD F) NIL (MINUS 1)))
         (COND ((EQ W 'TRUE) (PROGN (SETQ RVL '(TRUE)) (SETQ CDL NIL)))
               ((NEQ W 'FALSE) (SETQ RVL (CONS (CL_TNFT W (CDR TERML)) RVL)))))
        (GO WHILELABEL))
      (RETURN
       (RL_SIMPL
        (COND ((AND RVL (CDR RVL)) (CONS 'OR RVL))
              ((NULL RVL) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
              (T (CAR RVL)))
        NIL (MINUS 1))))) 
(ENDMODULE) 