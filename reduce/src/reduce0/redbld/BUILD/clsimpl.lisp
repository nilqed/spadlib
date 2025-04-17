(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLSIMPL)) 
(REVISION 'CLSIMPL "$Id: clsimpl.red 6081 2021-10-05 12:19:46Z thomas-sturm $") 
(COPYRIGHT 'CLSIMPL "(c) 1995-2021 A. Dolzmann, T. Sturm") 
(RL_PROVIDESERVICE 'RL_SIMPL 'CL_SIMPL
                   '(RL_ORDATP RL_SMMKATL RL_SMCPKNOWL RL_SMRMKNOWL
                               RL_SMUPDKNOWL RL_SIMPLAT1 RL_NEGATEAT)) 
(PUT 'CL_SIMPL 'NUMBER-OF-ARGS 3) 
(DE CL_SIMPL (F ATL N)
    (PROG (W)
      (SETQ ATL (CL_SIMPLIFYTHEORY ATL))
      (COND ((RL_EXCP ATL) (RETURN ATL)))
      (SETQ W (RL_SMUPDKNOWL 'AND ATL NIL (PLUS N 1)))
      (COND ((EQ W 'FALSE) (RETURN (RL_EXC "inconsistent theory"))))
      (RETURN (CL_SIMPL1 F W N NIL)))) 
(PUT 'CL_SIMPLIFYTHEORY 'NUMBER-OF-ARGS 1) 
(DE CL_SIMPLIFYTHEORY (ATL)
    (PROG (ATF W NATL *RLSIEXPLA)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ ATF (PROG1 (CAR ATL) (SETQ ATL (CDR ATL))))
         (SETQ W (CL_SIMPLAT ATF NIL))
         (COND ((EQ W 'FALSE) (PROGN (SETQ ATF 'INCTHEO) (SETQ ATL NIL)))
               ((NEQ W 'TRUE) (SETQ NATL (CONS W NATL)))))
        (GO WHILELABEL))
      (COND ((EQ ATF 'INCTHEO) (RETURN (RL_EXC "inconsistent theory"))))
      (RETURN NATL))) 
(PUT 'CL_SIMPL1 'NUMBER-OF-ARGS 4) 
(DE CL_SIMPL1 (F KNOWL N SOP)
    (PROG (OP)
      (COND ((EQN N 0) (RETURN F)))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN F)))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND)) (RETURN (CL_SIMPLIFYANDOR F KNOWL N))))
      (COND ((EQ OP 'NOT) (RETURN (CL_SIMPLIFYNOT F KNOWL N))))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (CL_SIMPLIFYQUANTIFIER F KNOWL N))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (RETURN (RL_SIMPLIFYBOUNDEDQUANTIFIER F KNOWL N))))
      (COND
       ((EQ OP 'IMPL)
        (RETURN (CL_SIMPLIFYIMPLICATION (CADR F) (CADDR F) KNOWL N))))
      (COND
       ((EQ OP 'REPL)
        (RETURN (CL_SIMPLIFYIMPLICATION (CADDR F) (CADR F) KNOWL N))))
      (COND
       ((EQ OP 'EQUIV)
        (RETURN (CL_SIMPLIFYEQUIVALENCE (CADR F) (CADDR F) KNOWL N))))
      (COND (NIL NIL))
      (RETURN (CL_SIMPLIFYATOM F KNOWL N SOP)))) 
(PUT 'CL_SIMPLIFYATOM 'NUMBER-OF-ARGS 4) 
(DE CL_SIMPLIFYATOM (F KNOWL N SOP)
    (PROG (W OP NEWKNOWL)
      (SETQ W (CL_SIMPLAT F SOP))
      (SETQ OP (COND ((ATOM W) W) (T (CAR W))))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND)) (RETURN (CL_SIMPLIFYANDOR W KNOWL N))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN W)))
      (COND (NIL NIL))
      (SETQ NEWKNOWL (RL_SMUPDKNOWL 'AND (LIST W) (RL_SMCPKNOWL KNOWL) N))
      (COND ((EQ NEWKNOWL 'FALSE) (RETURN 'FALSE)))
      (SETQ W (RL_SMMKATL 'AND KNOWL NEWKNOWL N))
      (COND ((EQ W 'FALSE) (RETURN 'FALSE)))
      (RETURN
       (COND ((AND W (CDR W)) (CONS 'AND W))
             ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE))) (T (CAR W)))))) 
(PUT 'CL_SIMPLIFYQUANTIFIER 'NUMBER-OF-ARGS 3) 
(DE CL_SIMPLIFYQUANTIFIER (F KNOWL N)
    (PROG (Q X W)
      (SETQ Q (COND ((ATOM F) F) (T (CAR F))))
      (SETQ X (CADR F))
      (SETQ KNOWL (RL_SMRMKNOWL KNOWL X))
      (SETQ W (CL_SIMPL1 (CADDR F) KNOWL (DIFFERENCE N 1) Q))
      (COND ((NOT (MEMQ X (CL_FVARL1 W))) (RETURN W)))
      (RETURN (LIST Q X W)))) 
(PUT 'CL_SIMPLIFYNOT 'NUMBER-OF-ARGS 3) 
(DE CL_SIMPLIFYNOT (F KNOWL N)
    (PROG (RESULT)
      (SETQ RESULT (CL_SIMPL1 (CADR F) KNOWL (DIFFERENCE N 1) 'NOT))
      (COND
       ((OR (EQ RESULT 'TRUE) (EQ RESULT 'FALSE)) (RETURN (CL_FLIP RESULT))))
      (COND ((CL_ATFP RESULT) (RETURN (RL_NEGATEAT RESULT))))
      (RETURN (CL_INVOLUTIVENOT RESULT)))) 
(PUT 'CL_INVOLUTIVENOT 'NUMBER-OF-ARGS 1) 
(DE CL_INVOLUTIVENOT (F)
    (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'NOT) (CADR F))
          (T (LIST 'NOT F)))) 
(PUT 'CL_SIMPLIFYANDOR 'NUMBER-OF-ARGS 3) 
(DE CL_SIMPLIFYANDOR (F KNOWL N)
    (PROG (OP JUNCT BREAK W ATL COL NEWKNOWL A WOP ARGL SICOL NATL)
      (SETQ NEWKNOWL (RL_SMCPKNOWL KNOWL))
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (SETQ JUNCT (CDR F))
      (SETQ BREAK (CL_CFLIP 'FALSE (EQ OP 'AND)))
      (PROG (SUBF)
        (SETQ SUBF JUNCT)
       LAB
        (COND ((NULL SUBF) (RETURN NIL)))
        ((LAMBDA (SUBF)
           (PROGN
            (SETQ W (COND ((CL_ATFP SUBF) (CL_SIMPLAT SUBF OP)) (T SUBF)))
            (COND ((CL_ATFP W) (SETQ ATL (CONS W ATL)))
                  (T (SETQ COL (CONS W COL))))))
         (CAR SUBF))
        (SETQ SUBF (CDR SUBF))
        (GO LAB))
      (SETQ NEWKNOWL (RL_SMUPDKNOWL OP ATL NEWKNOWL N))
      (COND ((EQ NEWKNOWL 'FALSE) (RETURN BREAK)))
      (SETQ COL (REVERSIP COL))
      (PROG ()
       WHILELABEL
        (COND ((NOT COL) (RETURN NIL)))
        (PROGN
         (SETQ A (PROG1 (CAR COL) (SETQ COL (CDR COL))))
         (SETQ W (CL_SIMPL1 A NEWKNOWL (DIFFERENCE N 1) OP))
         (SETQ WOP (COND ((ATOM W) W) (T (CAR W))))
         (COND ((EQ WOP BREAK) (PROGN (SETQ A BREAK) (SETQ COL NIL)))
               ((EQ WOP OP)
                (PROGN
                 (SETQ ARGL (CDR W))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND ARGL (CL_ATFP (CAR ARGL)))) (RETURN NIL)))
                   (PROGN
                    (SETQ NATL (CONS (CAR ARGL) NATL))
                    (SETQ ARGL (CDR ARGL)))
                   (GO WHILELABEL))
                 (COND
                  (NATL
                   (PROGN
                    (SETQ COL (NCONC (REVERSIP SICOL) COL))
                    (SETQ SICOL NIL))))
                 (SETQ SICOL (NCONC SICOL (REVERSE ARGL)))))
               ((OR (OR (EQ WOP 'TRUE) (EQ WOP 'FALSE))
                    (OR (OR (OR (EQ WOP 'OR) (EQ WOP 'AND)) (EQ WOP 'NOT))
                        (OR (EQ WOP 'IMPL) (EQ WOP 'REPL) (EQ WOP 'EQUIV)))
                    (OR (EQ WOP 'EX) (EQ WOP 'ALL))
                    (OR (EQ WOP 'BEX) (EQ WOP 'BALL)))
                (COND ((NEQ WOP (CL_FLIP BREAK)) (SETQ SICOL (CONS W SICOL)))))
               (T
                (PROGN
                 (COND (NIL NIL))
                 (SETQ COL (NCONC (REVERSIP SICOL) COL))
                 (SETQ SICOL NIL)
                 (SETQ NATL (LIST W)))))
         (COND
          (NATL
           (PROGN
            (SETQ NEWKNOWL (RL_SMUPDKNOWL OP NATL NEWKNOWL N))
            (COND ((EQ NEWKNOWL 'FALSE) (PROGN (SETQ A BREAK) (SETQ COL NIL))))
            (SETQ NATL NIL)))))
        (GO WHILELABEL))
      (COND ((EQ A BREAK) (RETURN BREAK)))
      (SETQ ATL (RL_SMMKATL OP KNOWL NEWKNOWL N))
      (COND ((EQ ATL BREAK) (RETURN BREAK)))
      (SETQ W SICOL)
      (SETQ SICOL NIL)
      (PROG (X)
        (SETQ X W)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ SICOL (LTO_INSERT X SICOL))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (*RLSISO
        (PROGN
         (SETQ ATL (SORT ATL 'RL_ORDATP))
         (SETQ SICOL (SORT SICOL 'CL_ORDP)))))
      (RETURN
       ((LAMBDA (G128)
          (COND ((AND G128 (CDR G128)) (CONS OP G128))
                ((NULL G128) (COND ((EQ OP 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G128))))
        (NCONC ATL SICOL))))) 
(PUT 'CL_SIMPLIFYIMPLICATION 'NUMBER-OF-ARGS 4) 
(DE CL_SIMPLIFYIMPLICATION (PREM CONCL KNOWL N)
    (PROG (W NEWKNOWL)
      (COND ((EQ CONCL 'TRUE) (RETURN 'TRUE)))
      (SETQ PREM (CL_SIMPL1 PREM KNOWL (DIFFERENCE N 1) 'PREM))
      (COND ((EQ PREM 'FALSE) (RETURN 'TRUE)))
      (SETQ CONCL (CL_SIMPL1 CONCL KNOWL (DIFFERENCE N 1) 'CONCL))
      (COND ((EQ CONCL 'TRUE) (RETURN 'TRUE)))
      (COND ((EQ PREM 'TRUE) (RETURN CONCL)))
      (COND ((EQ CONCL 'FALSE) (RETURN (CL_INVOLUTIVENOT PREM))))
      (COND (NIL NIL))
      (COND ((EQUAL PREM CONCL) (RETURN 'TRUE)))
      (COND
       ((AND (CL_ATFP PREM) (CL_ATFP CONCL))
        (PROGN
         (SETQ W
                 (CL_SIMPL1 (RL_NNF (LIST 'IMPL PREM CONCL)) KNOWL
                  (DIFFERENCE N 1) NIL))
         (COND ((OR (OR (EQ W 'TRUE) (EQ W 'FALSE)) (CL_ATFP W)) (RETURN W)))
         (RETURN (LIST 'IMPL PREM CONCL))))
       ((AND (CL_ATFP PREM) (CL_CXFP CONCL))
        (PROGN
         (SETQ NEWKNOWL
                 (RL_SMUPDKNOWL 'AND (LIST PREM) (RL_SMCPKNOWL KNOWL) N))
         (COND ((EQ NEWKNOWL 'FALSE) (RETURN 'TRUE)))
         (RETURN
          (LIST 'IMPL PREM
                (CL_SIMPL1 CONCL NEWKNOWL (DIFFERENCE N 1) 'CONCL)))))
       ((AND (CL_CXFP PREM) (CL_ATFP CONCL))
        (PROGN
         (SETQ NEWKNOWL
                 (RL_SMUPDKNOWL 'OR (LIST CONCL) (RL_SMCPKNOWL KNOWL) N))
         (COND ((EQ NEWKNOWL 'FALSE) (RETURN 'TRUE)))
         (RETURN
          (LIST 'IMPL (CL_SIMPL1 PREM NEWKNOWL (DIFFERENCE N 1) 'CONCL)
                CONCL))))
       (T (PROGN (COND (NIL NIL)) (RETURN (LIST 'IMPL PREM CONCL))))))) 
(PUT 'CL_SIMPLIFYEQUIVALENCE 'NUMBER-OF-ARGS 4) 
(DE CL_SIMPLIFYEQUIVALENCE (LHS RHS KNOWL N)
    (PROG (W)
      (SETQ LHS (CL_SIMPL1 LHS KNOWL (DIFFERENCE N 1) 'EQUIV))
      (SETQ RHS (CL_SIMPL1 RHS KNOWL (DIFFERENCE N 1) 'EQUIV))
      (COND ((EQ LHS 'TRUE) (RETURN RHS)))
      (COND ((EQ RHS 'TRUE) (RETURN LHS)))
      (COND ((EQ LHS 'FALSE) (RETURN (CL_INVOLUTIVENOT RHS))))
      (COND ((EQ RHS 'FALSE) (RETURN (CL_INVOLUTIVENOT LHS))))
      (COND ((EQUAL LHS RHS) (RETURN 'TRUE)))
      (COND
       ((AND (CL_ATFP LHS) (CL_ATFP RHS))
        (PROGN
         (SETQ W (CL_SIMPL1 (RL_NNF (LIST 'EQUIV LHS RHS)) KNOWL N NIL))
         (COND
          ((OR (OR (EQ W 'TRUE) (EQ W 'FALSE)) (CL_ATFP W)) (RETURN W))))))
      (RETURN
       (COND ((CL_ORDP LHS RHS) (LIST 'EQUIV LHS RHS))
             (T (LIST 'EQUIV RHS LHS)))))) 
(PUT 'CL_ORDP 'NUMBER-OF-ARGS 2) 
(DE CL_ORDP (F1 F2)
    (PROG (OP1 OP2)
      (SETQ OP1 (COND ((ATOM F1) F1) (T (CAR F1))))
      (SETQ OP2 (COND ((ATOM F2) F2) (T (CAR F2))))
      (COND
       ((AND
         (NOT
          (OR (OR (EQ OP1 'TRUE) (EQ OP1 'FALSE))
              (OR (OR (OR (EQ OP1 'OR) (EQ OP1 'AND)) (EQ OP1 'NOT))
                  (OR (EQ OP1 'IMPL) (EQ OP1 'REPL) (EQ OP1 'EQUIV)))
              (OR (EQ OP1 'EX) (EQ OP1 'ALL))
              (OR (EQ OP1 'BEX) (EQ OP1 'BALL))))
         (NOT
          (OR (OR (EQ OP2 'TRUE) (EQ OP2 'FALSE))
              (OR (OR (OR (EQ OP2 'OR) (EQ OP2 'AND)) (EQ OP2 'NOT))
                  (OR (EQ OP2 'IMPL) (EQ OP2 'REPL) (EQ OP2 'EQUIV)))
              (OR (EQ OP2 'EX) (EQ OP2 'ALL))
              (OR (EQ OP2 'BEX) (EQ OP2 'BALL)))))
        (RETURN (RL_ORDATP F1 F2))))
      (COND
       ((AND
         (NOT
          (OR (OR (EQ OP1 'TRUE) (EQ OP1 'FALSE))
              (OR (OR (OR (EQ OP1 'OR) (EQ OP1 'AND)) (EQ OP1 'NOT))
                  (OR (EQ OP1 'IMPL) (EQ OP1 'REPL) (EQ OP1 'EQUIV)))
              (OR (EQ OP1 'EX) (EQ OP1 'ALL))
              (OR (EQ OP1 'BEX) (EQ OP1 'BALL))))
         (OR (OR (EQ OP2 'TRUE) (EQ OP2 'FALSE))
             (OR (OR (OR (EQ OP2 'OR) (EQ OP2 'AND)) (EQ OP2 'NOT))
                 (OR (EQ OP2 'IMPL) (EQ OP2 'REPL) (EQ OP2 'EQUIV)))
             (OR (EQ OP2 'EX) (EQ OP2 'ALL))
             (OR (EQ OP2 'BEX) (EQ OP2 'BALL))))
        (RETURN T)))
      (COND
       ((AND
         (OR (OR (EQ OP1 'TRUE) (EQ OP1 'FALSE))
             (OR (OR (OR (EQ OP1 'OR) (EQ OP1 'AND)) (EQ OP1 'NOT))
                 (OR (EQ OP1 'IMPL) (EQ OP1 'REPL) (EQ OP1 'EQUIV)))
             (OR (EQ OP1 'EX) (EQ OP1 'ALL)) (OR (EQ OP1 'BEX) (EQ OP1 'BALL)))
         (NOT
          (OR (OR (EQ OP2 'TRUE) (EQ OP2 'FALSE))
              (OR (OR (OR (EQ OP2 'OR) (EQ OP2 'AND)) (EQ OP2 'NOT))
                  (OR (EQ OP2 'IMPL) (EQ OP2 'REPL) (EQ OP2 'EQUIV)))
              (OR (EQ OP2 'EX) (EQ OP2 'ALL))
              (OR (EQ OP2 'BEX) (EQ OP2 'BALL)))))
        (RETURN NIL)))
      (COND ((NEQ OP1 OP2) (RETURN (CL_ORDOPP OP1 OP2))))
      (COND ((OR (EQ OP1 'TRUE) (EQ OP1 'FALSE)) (RETURN T)))
      (COND
       ((OR (EQ OP1 'EX) (EQ OP1 'ALL))
        (RETURN
         (COND
          ((NEQ (CADR F1) (CADR F2))
           (NOT (AND (ORDP (CADR F1) (CADR F2)) (NEQ (CADR F1) (CADR F2)))))
          (T (CL_ORDP (CADDR F1) (CADDR F2)))))))
      (COND ((OR (EQ OP1 'BEX) (EQ OP1 'BALL)) (RETURN (RL_BQORDP F1 F2))))
      (COND (NIL NIL))
      (RETURN (CL_ORDPL (CDR F1) (CDR F2))))) 
(PUT 'CL_ORDPL 'NUMBER-OF-ARGS 2) 
(DE CL_ORDPL (FL1 FL2)
    (COND ((NOT FL2) NIL) ((NOT FL1) T)
          ((NEQ (CAR FL1) (CAR FL2)) (CL_ORDP (CAR FL1) (CAR FL2)))
          (T (CL_ORDPL (CDR FL1) (CDR FL2))))) 
(PUT 'CL_ORDOPP 'NUMBER-OF-ARGS 2) 
(DE CL_ORDOPP (OP1 OP2)
    (MEMQ OP2
          (CDR
           (MEMQ OP1
                 '(AND OR NOT IMPL REPL EQUIV BEX BALL EX ALL TRUE FALSE))))) 
(PUT 'CL_SMCPKNOWL 'NUMBER-OF-ARGS 1) 
(DE CL_SMCPKNOWL (KNOWL)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P KNOWL)
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (P) P) (CAR P)) NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) P) (CAR P)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_SMRMKNOWL 'NUMBER-OF-ARGS 2) 
(DE CL_SMRMKNOWL (KNOWL V) NIL) 
(PUT 'CL_SMUPDKNOWL 'NUMBER-OF-ARGS 4) 
(DE CL_SMUPDKNOWL (OP ATL KNOWL N)
    (PROG (AT)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ AT (CAR ATL))
         (SETQ ATL (CDR ATL))
         (SETQ KNOWL (CL_SMUPDKNOWL1 OP AT KNOWL N))
         (COND ((EQ KNOWL 'FALSE) (PROGN (SETQ ATL NIL) (SETQ AT 'BREAK)))))
        (GO WHILELABEL))
      (COND ((EQ AT 'BREAK) (RETURN 'FALSE)) (T (RETURN KNOWL))))) 
(PUT 'CL_SMUPDKNOWL1 'NUMBER-OF-ARGS 4) 
(DE CL_SMUPDKNOWL1 (OP AT KNOWL N)
    (PROG (ENT CONTRA)
      (COND ((EQ OP 'OR) (PROGN (SETQ ENT (RL_NEGATEAT AT)) (SETQ CONTRA AT)))
            (T (PROGN (SETQ ENT AT) (SETQ CONTRA (RL_NEGATEAT AT)))))
      (COND ((ASSOC CONTRA KNOWL) (RETURN 'FALSE)))
      (COND ((ASSOC ENT KNOWL) (RETURN KNOWL)))
      (RETURN (SETQ KNOWL (CONS (CONS ENT N) KNOWL))))) 
(PUT 'CL_SMMKATL 'NUMBER-OF-ARGS 4) 
(DE CL_SMMKATL (OP KNOWL NEWKNOWL N)
    (PROG (RES)
      (SETQ RES
              (PROG (PAIR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PAIR NEWKNOWL)
               STARTOVER
                (COND ((NULL PAIR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PAIR)
                           (COND ((EQUAL (CDR PAIR) N) (LIST (CAR PAIR)))))
                         (CAR PAIR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PAIR (CDR PAIR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PAIR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PAIR)
                           (COND ((EQUAL (CDR PAIR) N) (LIST (CAR PAIR)))))
                         (CAR PAIR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PAIR (CDR PAIR))
                (GO LOOPLABEL)))
      (COND
       ((EQ OP 'OR)
        (SETQ RES
                (PROG (AT FORALL-RESULT FORALL-ENDPTR)
                  (SETQ AT RES)
                  (COND ((NULL AT) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (AT) (RL_NEGATEAT AT)) (CAR AT))
                                   NIL)))
                 LOOPLABEL
                  (SETQ AT (CDR AT))
                  (COND ((NULL AT) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (AT) (RL_NEGATEAT AT)) (CAR AT)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN RES))) 
(ENDMODULE) 