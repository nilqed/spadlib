(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLMISC)) 
(REVISION 'CLMISC "$Id: clmisc.red 6085 2021-10-06 06:32:09Z thomas-sturm $") 
(COPYRIGHT 'CLMISC "(c) 1995-2021 A. Dolzmann, T. Sturm") 
(PUT 'CL_APPLY2ATS 'NUMBER-OF-ARGS 2) 
(DE CL_APPLY2ATS (F CLIENT) (CL_APPLY2ATS1 F CLIENT NIL)) 
(PUT 'CL_APPLY2ATS1 'NUMBER-OF-ARGS 3) 
(DE CL_APPLY2ATS1 (F CLIENT XARGS)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (CL_APPLY2ATS1 (CADDR F) CLIENT XARGS)))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (RETURN (RL_BQAPPLY2ATS1 F CLIENT XARGS))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                 (SETQ S (CDR F))
                 (COND ((NULL S) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (S) (CL_APPLY2ATS1 S CLIENT XARGS))
                                   (CAR S))
                                  NIL)))
                LOOPLABEL
                 (SETQ S (CDR S))
                 (COND ((NULL S) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (S) (CL_APPLY2ATS1 S CLIENT XARGS)) (CAR S))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND (NIL NIL))
      (RETURN (APPLY CLIENT (CONS F XARGS))))) 
(PUT 'CL_APPLY2ATS2 'NUMBER-OF-ARGS 4) 
(DE CL_APPLY2ATS2 (F CLIENT XARGS OMEGA)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (CL_APPLY2ATS2 (CADDR F) CLIENT XARGS OP)))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                 (SETQ S (CDR F))
                 (COND ((NULL S) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (S)
                                     (CL_APPLY2ATS2 S CLIENT XARGS OP))
                                   (CAR S))
                                  NIL)))
                LOOPLABEL
                 (SETQ S (CDR S))
                 (COND ((NULL S) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (S) (CL_APPLY2ATS2 S CLIENT XARGS OP))
                           (CAR S))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (COND (NIL NIL))
      (RETURN (APPLY CLIENT (CONS F (CONS OMEGA XARGS)))))) 
(PUT 'CL_ATNUM 'NUMBER-OF-ARGS 1) 
(DE CL_ATNUM (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (PROG (SUBF FORALL-RESULT)
           (SETQ SUBF (CDR F))
           (SETQ FORALL-RESULT 0)
          LAB1
           (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
           (SETQ FORALL-RESULT
                   (PLUS ((LAMBDA (SUBF) (CL_ATNUM SUBF)) (CAR SUBF))
                         FORALL-RESULT))
           (SETQ SUBF (CDR SUBF))
           (GO LAB1)))))
      (COND ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN (CL_ATNUM (CADDR F)))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQATNUM F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN 0)))
      (COND (NIL NIL))
      (RETURN 1))) 
(PUT 'CL_QNUM 'NUMBER-OF-ARGS 1) 
(DE CL_QNUM (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (PROG (S FORALL-RESULT)
           (SETQ S (CDR F))
           (SETQ FORALL-RESULT 0)
          LAB1
           (COND ((NULL S) (RETURN FORALL-RESULT)))
           (SETQ FORALL-RESULT
                   (PLUS ((LAMBDA (S) (CL_QNUM S)) (CAR S)) FORALL-RESULT))
           (SETQ S (CDR S))
           (GO LAB1)))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN (PLUS 1 (CL_QNUM (CADDR F))))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQQNUM F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN 0)))
      (COND (NIL NIL))
      (RETURN 0))) 
(PUT 'CL_DEPTH 'NUMBER-OF-ARGS 1) 
(DE CL_DEPTH (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
        (RETURN
         (PLUS 1
               (LTO_MAX
                (PROG (SF FORALL-RESULT FORALL-ENDPTR)
                  (SETQ SF (CDR F))
                  (COND ((NULL SF) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (SF) (CL_DEPTH SF)) (CAR SF))
                                        NIL)))
                 LOOPLABEL
                  (SETQ SF (CDR SF))
                  (COND ((NULL SF) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (SF) (CL_DEPTH SF)) (CAR SF)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN (PLUS 1 (CL_DEPTH (CADDR F))))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQDEPTH F))))
      (COND ((EQ OP 'NOT) (RETURN (PLUS 1 (CL_DEPTH (CADR F))))))
      (COND
       ((OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV))
        (RETURN (PLUS 1 (MAX (CL_DEPTH (CADR F)) (CL_DEPTH (CADDR F)))))))
      (COND (NIL NIL))
      (RETURN 0))) 
(PUT 'CL_PRENEXP 'NUMBER-OF-ARGS 1) 
(DE CL_PRENEXP (F)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
            (COND ((ATOM F) F) (T (CAR F)))))
          (RETURN NIL)))
        (SETQ F (CADDR F))
        (GO WHILELABEL))
      (RETURN (EQUAL (CL_QNUM F) 0)))) 
(PUT 'CL_F2ML 'NUMBER-OF-ARGS 2) 
(DE CL_F2ML (F CLIENT)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN NIL)))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (LTO_ALMERGE
          (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
            (SETQ SUBF (CDR F))
            (COND ((NULL SUBF) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (SUBF) (CL_F2ML SUBF CLIENT)) (CAR SUBF))
                             NIL)))
           LOOPLABEL
            (SETQ SUBF (CDR SUBF))
            (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (SUBF) (CL_F2ML SUBF CLIENT)) (CAR SUBF))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          'PLUS2))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN (CL_F2ML (CADDR F) CLIENT))))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN (RL_BQF2ML F CLIENT))))
      (COND (NIL NIL))
      (RETURN (APPLY CLIENT (LIST F))))) 
(RL_PROVIDESERVICE 'RL_ATML 'CL_ATML '(RL_ORDATP)) 
(PUT 'CL_ATML 'NUMBER-OF-ARGS 1) 
(DE CL_ATML (F)
    (SORT (CL_ATML1 F) (FUNCTION (LAMBDA (X Y) (RL_ORDATP (CAR X) (CAR Y)))))) 
(PUT 'CL_ATML1 'NUMBER-OF-ARGS 1) 
(DE CL_ATML1 (F) (CL_F2ML F 'CL_ATMLC)) 
(PUT 'CL_ATMLC 'NUMBER-OF-ARGS 1) 
(DE CL_ATMLC (ATF) (LIST (CONS ATF 1))) 
(RL_PROVIDESERVICE 'RL_ATL 'CL_ATL '(RL_ORDATP)) 
(PUT 'CL_ATL 'NUMBER-OF-ARGS 1) 
(DE CL_ATL (F) (SORT (CL_ATL1 F) 'RL_ORDATP)) 
(PUT 'CL_ATL1 'NUMBER-OF-ARGS 1) 
(DE CL_ATL1 (F)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (CL_ATML1 F))
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(RL_PROVIDESERVICE 'RL_IFACML 'CL_IFACML '(RL_TORDP RL_FCTRAT)) 
(PUT 'CL_IFACML 'NUMBER-OF-ARGS 1) 
(DE CL_IFACML (F)
    (SORT (CL_IFACML1 F) (FUNCTION (LAMBDA (X Y) (RL_TORDP (CAR X) (CAR Y)))))) 
(PUT 'CL_IFACML1 'NUMBER-OF-ARGS 1) 
(DE CL_IFACML1 (F) (CL_F2ML F 'RL_FCTRAT)) 
(RL_PROVIDESERVICE 'RL_IFACL 'CL_IFACL '(RL_TORDP RL_FCTRAT)) 
(PUT 'CL_IFACL 'NUMBER-OF-ARGS 1) 
(DE CL_IFACL (F) (SORT (CL_IFACL1 F) 'RL_TORDP)) 
(PUT 'CL_IFACL1 'NUMBER-OF-ARGS 1) 
(DE CL_IFACL1 (F)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (CL_IFACML1 F))
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(RL_PROVIDESERVICE 'RL_IFACDEGL 'CL_IFACDEGL '(RL_TORDP)) 
(PUT 'CL_IFACDEGL 'NUMBER-OF-ARGS 1) 
(DE CL_IFACDEGL (F)
    (PROG (FVARL BVARL FACL FAL BAL D)
      (SETQ D 0)
      (PROG (G169)
        (SETQ G169 (CL_VARL1 F))
        (SETQ FVARL (CAR G169))
        (SETQ BVARL (CDR G169))
        (RETURN G169))
      (SETQ FACL (CL_IFACL1 F))
      (PROG (V)
        (SETQ V FVARL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ D 0)
            (PROG (P)
              (SETQ P FACL)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P) (SETQ D (MAX D (DEGREEF P V)))) (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ FAL (CONS (CONS V D) FAL))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (V)
        (SETQ V BVARL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ D 0)
            (PROG (P)
              (SETQ P FACL)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P) (SETQ D (MAX D (DEGREEF P V)))) (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ BAL (CONS (CONS V D) BAL))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ FAL
              (SORT FAL (FUNCTION (LAMBDA (X Y) (RL_TORDP (CAR X) (CAR Y))))))
      (SETQ BAL
              (SORT BAL (FUNCTION (LAMBDA (X Y) (RL_TORDP (CAR X) (CAR Y))))))
      (RETURN (CONS FAL BAL)))) 
(RL_PROVIDESERVICE 'RL_MATRIX 'CL_MATRIX NIL) 
(PUT 'CL_MATRIX 'NUMBER-OF-ARGS 1) 
(DE CL_MATRIX (F)
    (COND
     (((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
       (COND ((ATOM F) F) (T (CAR F))))
      (CL_MATRIX (CADDR F)))
     (T F))) 
(RL_PROVIDESERVICE 'RL_ALL 'CL_ALL NIL) 
(PUT 'CL_ALL 'NUMBER-OF-ARGS 2) 
(DE CL_ALL (F EXCLUDE) (CL_CLOSURE 'ALL F EXCLUDE)) 
(RL_PROVIDESERVICE 'RL_EX 'CL_EX NIL) 
(PUT 'CL_EX 'NUMBER-OF-ARGS 2) 
(DE CL_EX (F EXCLUDE) (CL_CLOSURE 'EX F EXCLUDE)) 
(PUT 'CL_CLOSURE 'NUMBER-OF-ARGS 3) 
(DE CL_CLOSURE (Q F EXCLUDE)
    (PROG (FREEVARL RESULT)
      (SETQ FREEVARL (REVERSIP (CAR (CL_VARL F))))
      (PROG (V)
        (SETQ V EXCLUDE)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ FREEVARL (LTO_DELQIP V FREEVARL))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ RESULT F)
      (PROG (X)
        (SETQ X FREEVARL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ RESULT (LIST Q X RESULT))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN RESULT))) 
(RL_PROVIDESERVICE 'RL_TERMML 'CL_TERMML '(RL_TORDP RL_TERMMLAT)) 
(PUT 'CL_TERMML 'NUMBER-OF-ARGS 1) 
(PUT 'CL_TERMML 'DEFINED-ON-LINE '270) 
(PUT 'CL_TERMML 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_TERMML 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_TERMML (F)
    (SORT (CL_TERMML1 F) (FUNCTION (LAMBDA (X Y) (RL_TORDP (CAR X) (CAR Y)))))) 
(PUT 'CL_TERMML1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_TERMML1 'DEFINED-ON-LINE '276) 
(PUT 'CL_TERMML1 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_TERMML1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_TERMML1 (F) (CL_F2ML F 'RL_TERMMLAT)) 
(RL_PROVIDESERVICE 'RL_TERML 'CL_TERML '(RL_TORDP)) 
(PUT 'CL_TERML 'NUMBER-OF-ARGS 1) 
(PUT 'CL_TERML 'DEFINED-ON-LINE '284) 
(PUT 'CL_TERML 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_TERML 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_TERML (F) (SORT (CL_TERML1 F) 'RL_TORDP)) 
(PUT 'CL_TERML1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_TERML1 'DEFINED-ON-LINE '290) 
(PUT 'CL_TERML1 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_TERML1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_TERML1 (F)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (CL_TERMML1 F))
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(RL_PROVIDESERVICE 'RL_STRUCT 'CL_STRUCT '(RL_IFSTRUCTAT RL_STRUCTAT)) 
(PUT 'CL_STRUCT 'NUMBER-OF-ARGS 3) 
(DE CL_STRUCT (F FAC V) (COND (FAC (CL_IFSTRUCT F V)) (T (CL_STRUCT0 F V)))) 
(PUT 'CL_STRUCT0 'NUMBER-OF-ARGS 2) 
(DE CL_STRUCT0 (F V)
    (PROG (W J)
      (SETQ J 0)
      (SETQ W (CL_TERML F))
      (SETQ W
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S W)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS S (MKID V (SETQ J (PLUS J 1)))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S) (CONS S (MKID V (SETQ J (PLUS J 1)))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS (CL_STRUCT1 F W)
             (PROG (PR FORALL-RESULT FORALL-ENDPTR)
               (SETQ PR W)
               (COND ((NULL PR) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (PR) (CONS (CDR PR) (CAR PR)))
                                 (CAR PR))
                                NIL)))
              LOOPLABEL
               (SETQ PR (CDR PR))
               (COND ((NULL PR) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (PR) (CONS (CDR PR) (CAR PR))) (CAR PR))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'CL_STRUCT1 'NUMBER-OF-ARGS 2) 
(DE CL_STRUCT1 (F AL) (CL_APPLY2ATS1 F 'RL_STRUCTAT (LIST AL))) 
(PUT 'CL_IFSTRUCT 'NUMBER-OF-ARGS 2) 
(DE CL_IFSTRUCT (F V)
    (PROG (W J)
      (SETQ J 0)
      (SETQ W (CL_IFACL F))
      (SETQ W
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S W)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS S (MKID V (SETQ J (PLUS J 1)))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S) (CONS S (MKID V (SETQ J (PLUS J 1)))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS (CL_IFSTRUCT1 F W)
             (PROG (PR FORALL-RESULT FORALL-ENDPTR)
               (SETQ PR W)
               (COND ((NULL PR) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (PR) (CONS (CDR PR) (CAR PR)))
                                 (CAR PR))
                                NIL)))
              LOOPLABEL
               (SETQ PR (CDR PR))
               (COND ((NULL PR) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (PR) (CONS (CDR PR) (CAR PR))) (CAR PR))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'CL_IFSTRUCT1 'NUMBER-OF-ARGS 2) 
(DE CL_IFSTRUCT1 (F AL) (CL_APPLY2ATS1 F 'RL_IFSTRUCTAT (LIST AL))) 
(RL_PROVIDESERVICE 'RL_SUREP 'CL_SUREP '(RL_MULTSUREP)) 
(PUT 'CL_SUREP 'NUMBER-OF-ARGS 2) 
(DE CL_SUREP (AT ATL)
    (COND (*RLSPGS (OR (EQ (RL_GSN AT ATL 'DNF) 'TRUE) (RL_MULTSUREP AT ATL)))
          (T
           (OR (EQ (RL_SIMPL AT ATL (MINUS 1)) 'TRUE) (RL_MULTSUREP AT ATL))))) 
(PUT 'CL_FLIP 'NUMBER-OF-ARGS 1) 
(DE CL_FLIP (OP)
    (COND ((EQ OP 'AND) 'OR) ((EQ OP 'OR) 'AND) ((EQ OP 'ALL) 'EX)
          ((EQ OP 'EX) 'ALL) ((EQ OP 'BALL) 'BEX) ((EQ OP 'BEX) 'BALL)
          ((EQ OP 'TRUE) 'FALSE) ((EQ OP 'FALSE) 'TRUE)
          (T (REDERR (LIST "cl_flip(): don't know" OP))))) 
(PUT 'CL_CFLIP 'NUMBER-OF-ARGS 2) 
(DE CL_CFLIP (OP FLAG) (COND (FLAG OP) (T (CL_FLIP OP)))) 
(PUT 'CL_SPLT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_SPLT 'DEFINED-ON-LINE '364) 
(PUT 'CL_SPLT 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_SPLT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_SPLT (F)
    (PROG (W Q VL QBLKL BVL V)
      (SETQ Q (COND ((ATOM F) F) (T (CAR F))))
      (COND ((NOT (MEMQ Q '(EX ALL))) (RETURN (LIST NIL F NIL))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (MEMQ (SETQ W (COND ((ATOM F) F) (T (CAR F)))) '(EX ALL)))
          (RETURN NIL)))
        (PROGN
         (SETQ V (CADR F))
         (SETQ BVL (CONS V BVL))
         (COND ((EQ W Q) (SETQ VL (CONS V VL)))
               (T
                (PROGN
                 (SETQ QBLKL (CONS (CONS Q VL) QBLKL))
                 (SETQ Q W)
                 (SETQ VL (LIST V)))))
         (SETQ F (CADDR F)))
        (GO WHILELABEL))
      (SETQ QBLKL (CONS (CONS Q VL) QBLKL))
      (RETURN (LIST QBLKL F BVL)))) 
(PUT 'CL_ATL2B 'NUMBER-OF-ARGS 1) 
(PUT 'CL_ATL2B 'DEFINED-ON-LINE '390) 
(PUT 'CL_ATL2B 'DEFINED-IN-FILE 'REDLOG/CL/CLMISC.RED) 
(PUT 'CL_ATL2B 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_ATL2B (ATL)
    (COND ((NULL ATL) 'FALSE)
          (T
           (COND ((AND ATL (CDR ATL)) (CONS 'AND ATL))
                 ((NULL ATL) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR ATL)))))) 
(FLAG '(EX2) 'OPFN) 
(PUT 'EX2 'NUMBER-OF-ARGS 2) 
(DE EX2 (VARS F)
    (PROGN
     (SETQ VARS (COND ((EQCAR VARS 'LIST) (CDR VARS)) (T (LIST VARS))))
     (RL_MK*FOF (CL_EX21 VARS (RL_SIMP F))))) 
(RL_PROVIDESERVICE 'RL_EX2 'CL_EX2 '(RL_MKEQUATION)) 
(PUT 'CL_EX2 'NUMBER-OF-ARGS 2) 
(DE CL_EX2 (F PL)
    (PROG (FVL BVL VL)
      (PROG (G170)
        (SETQ G170 (RL_VARL F))
        (SETQ FVL (CAR G170))
        (SETQ BVL (CDR G170))
        (RETURN G170))
      (SETQ VL (LTO_SETMINUS FVL PL))
      (RETURN (CL_EX21 VL F)))) 
(PUT 'CL_EX21 'NUMBER-OF-ARGS 2) 
(DE CL_EX21 (VL F)
    (PROG (W FVL BVL QL SL EQS RES)
      (PROG (G171)
        (SETQ G171 (RL_VARL F))
        (SETQ FVL (CAR G171))
        (SETQ BVL (CDR G171))
        (RETURN G171))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ W V)
            (PROG ()
             REPEATLABEL
              (SETQ W (MKID W '|#|))
              (COND
               ((NOT (AND (NOT (MEMQ W FVL)) (NOT (MEMQ W BVL))))
                (GO REPEATLABEL))))
            (SETQ QL (CONS W QL))
            (SETQ SL (CONS (CONS V W) SL))
            (SETQ EQS (CONS (RL_MKEQUATION V W) EQS))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ QL (NCONC QL (REVERSIP VL)))
      (SETQ EQS
              (LIST 'NOT
                    ((LAMBDA (G173)
                       (COND ((AND G173 (CDR G173)) (CONS 'AND G173))
                             ((NULL G173)
                              (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                             (T (CAR G173))))
                     (REVERSIP EQS))))
      (SETQ RES (CONS 'AND (LIST F (CL_SUBFOF SL F) EQS)))
      (PROG (Q)
        (SETQ Q QL)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q) (SETQ RES (LIST 'EX Q RES))) (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (RETURN RES))) 
(PUT 'CL_DIVIDE 'NUMBER-OF-ARGS 1) 
(DE CL_DIVIDE (F)
    (PROG (QL VARLL M OP Q VARL L W)
      (PROG (G174 G175)
        (SETQ G174 (CL_SPLIT (CL_PNF F)))
        (SETQ G175 G174)
        (SETQ QL (CAR G174))
        (SETQ G174 (CDR G174))
        (SETQ VARLL (CAR G174))
        (SETQ G174 (CDR G174))
        (SETQ M (CAR G174))
        (SETQ G174 (CDR G174))
        (RETURN G175))
      (COND ((OR (NOT QL) (CDR QL)) (RETURN (CONS 'OR (LIST F)))))
      (SETQ Q (CAR QL))
      (SETQ OP (COND ((ATOM M) M) (T (CAR M))))
      (COND
       ((AND (NOT (AND (EQ Q 'EX) (EQ OP 'OR)))
             (NOT (AND (EQ Q 'ALL) (EQ OP 'AND))))
        (RETURN (CONS 'OR (LIST F)))))
      (SETQ VARL (CAR VARLL))
      (SETQ L
              (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                (SETQ SUBF (CDR M))
                (COND ((NULL SUBF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (SUBF)
                                    (PROGN
                                     (SETQ W SUBF)
                                     (PROG (V)
                                       (SETQ V VARL)
                                      LAB
                                       (COND ((NULL V) (RETURN NIL)))
                                       ((LAMBDA (V) (SETQ W (LIST Q V SUBF)))
                                        (CAR V))
                                       (SETQ V (CDR V))
                                       (GO LAB))
                                     W))
                                  (CAR SUBF))
                                 NIL)))
               LOOPLABEL
                (SETQ SUBF (CDR SUBF))
                (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (SUBF)
                            (PROGN
                             (SETQ W SUBF)
                             (PROG (V)
                               (SETQ V VARL)
                              LAB
                               (COND ((NULL V) (RETURN NIL)))
                               ((LAMBDA (V) (SETQ W (LIST Q V SUBF))) (CAR V))
                               (SETQ V (CDR V))
                               (GO LAB))
                             W))
                          (CAR SUBF))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS OP L)))) 
(PUT 'CL_EVAL 'NUMBER-OF-ARGS 3) 
(DE CL_EVAL (F SUBAL EVALAT)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN OP)))
      (COND
       ((EQ OP 'AND) (RETURN (CL_EVAL-GAND 'AND (CDR F) SUBAL EVALAT 'TRUE))))
      (COND
       ((EQ OP 'OR) (RETURN (CL_EVAL-GAND 'OR (CDR F) SUBAL EVALAT 'FALSE))))
      (RETURN (APPLY EVALAT (LIST F SUBAL))))) 
(PUT 'CL_EVAL-GAND 'NUMBER-OF-ARGS 5) 
(DE CL_EVAL-GAND (GAND ARGL SUBAL EVALAT GTRUE)
    (PROG (RES)
      (SETQ RES GTRUE)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (EQ RES GTRUE) ARGL)) (RETURN NIL)))
        (SETQ RES
                (CL_EVAL (PROG1 (CAR ARGL) (SETQ ARGL (CDR ARGL))) SUBAL
                 EVALAT))
        (GO WHILELABEL))
      (RETURN RES))) 
(RL_PROVIDESERVICE 'RL_SIGN 'CL_SIGN '(RL_SIGNAT)) 
(PUT 'CL_SIGN 'NUMBER-OF-ARGS 1) 
(DE CL_SIGN (F) (CL_APPLY2ATS F 'RL_SIGNAT)) 
(RL_PROVIDESERVICE 'CL_SIADDATL 'CL_SIADDATL NIL) 
(PUT 'CL_SIADDATL 'NUMBER-OF-ARGS 2) 
(DE CL_SIADDATL (ATL C)
    (PROG (W SICD)
      (COND ((EQ C 'FALSE) (RETURN 'FALSE)))
      (SETQ ATL (CL_SIMPLIFYTHEORY ATL))
      (COND ((EQ ATL 'INCTHEO) (RETURN 'FALSE)))
      (SETQ SICD
              (COND ((EQ C 'TRUE) NIL)
                    ((CL_CXFP C) (PROGN (COND (NIL NIL)) (CDR C)))
                    (T (LIST C))))
      (SETQ W (RL_SMUPDKNOWL 'AND (NCONC ATL SICD) NIL 1))
      (COND ((EQ W 'FALSE) (RETURN 'FALSE)))
      (SETQ W (RL_SMMKATL 'AND NIL W 1))
      (COND ((EQ W 'FALSE) (RETURN 'FALSE)))
      (COND (*RLSISO (SETQ W (SORT W 'RL_ORDATP))))
      (RETURN
       (COND ((AND W (CDR W)) (CONS 'AND W))
             ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE))) (T (CAR W)))))) 
(RL_PROVIDESERVICE 'RL_QESIL 'CL_QESIL NIL) 
(PUT 'CL_QESIL 'NUMBER-OF-ARGS 2) 
(DE CL_QESIL (FL THEO)
    (PROG (PREM TEST SOL RES N)
      (SETQ N 0)
      (COND (*RLVERBOSE (PROGN (SETQ N (PLUS (LENGTH FL) 1)) (IOTO_CTERPRI))))
      (SETQ RES
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FL)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (PROGN
                            (SETQ PREM
                                    (CONS 'AND
                                          (LIST
                                           (COND
                                            ((AND THEO (CDR THEO))
                                             (CONS 'AND THEO))
                                            ((NULL THEO)
                                             (COND ((EQ 'AND 'AND) 'TRUE)
                                                   (T 'FALSE)))
                                            (T (CAR THEO)))
                                           ((LAMBDA (G177)
                                              (COND
                                               ((AND G177 (CDR G177))
                                                (CONS 'AND G177))
                                               ((NULL G177)
                                                (COND ((EQ 'AND 'AND) 'TRUE)
                                                      (T 'FALSE)))
                                               (T (CAR G177))))
                                            (DELETE F FL)))))
                            (SETQ TEST (RL_ALL (LIST 'IMPL PREM F) NIL))
                            (COND
                             (*RLVERBOSE
                              (IOTO_PRIN2
                               (LIST "[" (SETQ N (DIFFERENCE N 1))))))
                            ((LAMBDA (*RLVERBOSE) (SETQ SOL (RL_QE TEST NIL)))
                             NIL)
                            (COND
                             (*RLVERBOSE
                              (IOTO_PRIN2
                               (LIST (COND ((EQ SOL 'TRUE) "!") (T ""))
                                     "] "))))
                            (COND ((NEQ SOL 'TRUE) (LIST F)))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (PROGN
                            (SETQ PREM
                                    (CONS 'AND
                                          (LIST
                                           (COND
                                            ((AND THEO (CDR THEO))
                                             (CONS 'AND THEO))
                                            ((NULL THEO)
                                             (COND ((EQ 'AND 'AND) 'TRUE)
                                                   (T 'FALSE)))
                                            (T (CAR THEO)))
                                           ((LAMBDA (G177)
                                              (COND
                                               ((AND G177 (CDR G177))
                                                (CONS 'AND G177))
                                               ((NULL G177)
                                                (COND ((EQ 'AND 'AND) 'TRUE)
                                                      (T 'FALSE)))
                                               (T (CAR G177))))
                                            (DELETE F FL)))))
                            (SETQ TEST (RL_ALL (LIST 'IMPL PREM F) NIL))
                            (COND
                             (*RLVERBOSE
                              (IOTO_PRIN2
                               (LIST "[" (SETQ N (DIFFERENCE N 1))))))
                            ((LAMBDA (*RLVERBOSE) (SETQ SOL (RL_QE TEST NIL)))
                             NIL)
                            (COND
                             (*RLVERBOSE
                              (IOTO_PRIN2
                               (LIST (COND ((EQ SOL 'TRUE) "!") (T ""))
                                     "] "))))
                            (COND ((NEQ SOL 'TRUE) (LIST F)))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (COND (*RLVERBOSE (IOTO_CTERPRI)))
      (RETURN RES))) 
(ENDMODULE) 