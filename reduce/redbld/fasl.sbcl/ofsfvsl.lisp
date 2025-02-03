(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFVSL)) 
(REVISION 'OFSFVSL "$Id: ofsfvsl.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFVSL "(c) 2014-2017 M. Kosta, T. Sturm") 
(SWITCH (LIST 'VSLGREATERPLEM)) 
(FLAG '(VOID) 'ASSERT_DYNTYPE) 
(PUT 'VSLSTATE 'ASSERT_DYNTYPECHK 'VSLSTATEP) 
(FLAG '(VSLSTATE) 'ASSERT_DYNTYPE) 
(PUT 'VSLSTACKELEM 'ASSERT_DYNTYPECHK 'VSLSTACKELEMP) 
(FLAG '(VSLSTACKELEM) 'ASSERT_DYNTYPE) 
(PUT 'VSLNEGLEMMA 'ASSERT_DYNTYPECHK 'VSLNEGLEMMAP) 
(FLAG '(VSLNEGLEMMA) 'ASSERT_DYNTYPE) 
(PUT 'VSLNEGLEMMAL 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(VSLNEGLEMMAL) 'ASSERT_DYNTYPE) 
(PUT 'VSLSTATEP 'NUMBER-OF-ARGS 1) 
(PUT 'VSLSTATEP 'DEFINED-ON-LINE '43) 
(PUT 'VSLSTATEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'VSLSTATEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSLSTATEP (S) (EQCAR S 'VSLSTATE)) 
(PUT 'VSLSTACKELEMP 'NUMBER-OF-ARGS 1) 
(PUT 'VSLSTACKELEMP 'DEFINED-ON-LINE '46) 
(PUT 'VSLSTACKELEMP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'VSLSTACKELEMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSLSTACKELEMP (S) (EQCAR S 'VSLSTACKELEM)) 
(PUT 'VSLNEGLEMMAP 'NUMBER-OF-ARGS 1) 
(PUT 'VSLNEGLEMMAP 'DEFINED-ON-LINE '49) 
(PUT 'VSLNEGLEMMAP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'VSLNEGLEMMAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSLNEGLEMMAP (S) (AND (LISTP S) (EQCAR S 'OR))) 
(PUT 'VSLS_IL 'NUMBER-OF-ARGS 1) 
(DE VSLS_IL (S) (NTH S 2)) 
(PUT 'VSLS_SETIL 'NUMBER-OF-ARGS 2) 
(DE VSLS_SETIL (S IL) (SETCAR (PNTH S 2) IL)) 
(PUT 'VSLS_SL 'NUMBER-OF-ARGS 1) 
(DE VSLS_SL (S) (NTH S 3)) 
(PUT 'VSLS_SETSL 'NUMBER-OF-ARGS 2) 
(DE VSLS_SETSL (S SL) (SETCAR (PNTH S 3) SL)) 
(PUT 'VSLS_NL 'NUMBER-OF-ARGS 1) 
(DE VSLS_NL (S) (NTH S 4)) 
(PUT 'VSLS_SETNL 'NUMBER-OF-ARGS 2) 
(DE VSLS_SETNL (S NL) (SETCAR (PNTH S 4) NL)) 
(PUT 'VSLS_PROP 'NUMBER-OF-ARGS 1) 
(DE VSLS_PROP (S) (NTH S 5)) 
(PUT 'VSLS_PUT 'NUMBER-OF-ARGS 3) 
(DE VSLS_PUT (S KEY VALUE)
    (PROG (H W)
      (SETQ H (CDDDR (CDR S)))
      (SETQ W (ATSOC KEY (CAR H)))
      (COND (W (SETCDR W VALUE))
            (T (SETCAR H (CONS (CONS KEY VALUE) (CAR H)))))
      (RETURN VALUE))) 
(PUT 'VSLS_KVGET 'NUMBER-OF-ARGS 2) 
(DE VSLS_KVGET (S KEY) (ATSOC KEY (VSLS_PROP S))) 
(PUT 'VSLS_MK 'NUMBER-OF-ARGS 3) 
(DE VSLS_MK (IL SAL NL) (LIST 'VSLSTATE IL SAL NL NIL)) 
(SWITCH (LIST 'VSLSPRINTIL)) 
(SWITCH (LIST 'VSLSPRINTSL)) 
(SWITCH (LIST 'VSLSPRINTNL)) 
(OFF1 'VSLSPRINTIL) 
(ON1 'VSLSPRINTSL) 
(ON1 'VSLSPRINTNL) 
(PUT 'VSLS_PRINT 'NUMBER-OF-ARGS 1) 
(DE VSLS_PRINT (S)
    (PROG (SL IL NL)
      (COND
       (*VSLSPRINTIL
        (PROGN
         (IOTO_PRIN2 "   input: ")
         (SETQ IL (VSLS_IL S))
         (COND ((NULL IL) (IOTO_PRIN2T "nil"))
               (T
                (PROGN
                 (IOTO_PRIN2T (LIST "|" (IOTO_FORM2STR (RL_PREPFOF (CAR IL)))))
                 (PROG (ATF)
                   (SETQ ATF (CDR IL))
                  LAB
                   (COND ((NULL ATF) (RETURN NIL)))
                   ((LAMBDA (ATF)
                      (PROGN
                       (IOTO_TPRIN2 "          |")
                       (IOTO_PRIN2T (IOTO_FORM2STR (RL_PREPFOF ATF)))))
                    (CAR ATF))
                   (SETQ ATF (CDR ATF))
                   (GO LAB))))))))
      (COND
       (*VSLSPRINTSL
        (PROGN
         (IOTO_PRIN2 "   stack: ")
         (SETQ SL (VSLS_SL S))
         (COND ((NULL SL) (PROGN (IOTO_PRIN2T "nil") (RETURN NIL)))
               (T
                (PROGN
                 (VSLSE_PRINT (CAR SL))
                 (PROG (SE)
                   (SETQ SE (CDR SL))
                  LAB
                   (COND ((NULL SE) (RETURN NIL)))
                   ((LAMBDA (SE)
                      (PROGN (IOTO_TPRIN2 "          ") (VSLSE_PRINT SE)))
                    (CAR SE))
                   (SETQ SE (CDR SE))
                   (GO LAB))))))))
      (COND
       (*VSLSPRINTNL
        (PROGN
         (IOTO_PRIN2 "  lemmas: ")
         (SETQ NL (VSLS_NL S))
         (COND ((NULL NL) (IOTO_PRIN2T "nil"))
               (T
                (PROGN
                 (IOTO_PRIN2T
                  (LIST "|"
                        (IOTO_FORM2STR
                         (RL_PREPFOF
                          (COND
                           ((AND (CAR NL) (CDR (CAR NL))) (CONS 'OR (CAR NL)))
                           ((NULL (CAR NL))
                            (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR (CAR NL))))))))
                 (PROG (L)
                   (SETQ L (CDR NL))
                  LAB
                   (COND ((NULL L) (RETURN NIL)))
                   ((LAMBDA (L)
                      (PROGN
                       (IOTO_TPRIN2 "          |")
                       (IOTO_PRIN2T
                        (IOTO_FORM2STR
                         (RL_PREPFOF
                          (COND ((AND L (CDR L)) (CONS 'OR L))
                                ((NULL L)
                                 (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                (T (CAR L))))))))
                    (CAR L))
                   (SETQ L (CDR L))
                   (GO LAB)))))))))) 
(PUT 'VSLSE_V 'NUMBER-OF-ARGS 1) 
(DE VSLSE_V (SE) (NTH SE 2)) 
(PUT 'VSLSE_ETERM 'NUMBER-OF-ARGS 1) 
(DE VSLSE_ETERM (SE) (NTH SE 3)) 
(PUT 'VSLSE_ORIG 'NUMBER-OF-ARGS 1) 
(DE VSLSE_ORIG (SE) (NTH SE 4)) 
(PUT 'VSLSE_ILS 'NUMBER-OF-ARGS 1) 
(DE VSLSE_ILS (SE) (NTH SE 5)) 
(PUT 'VSLSE_NLS 'NUMBER-OF-ARGS 1) 
(DE VSLSE_NLS (SE) (NTH SE 6)) 
(PUT 'VSLSE_MK 'NUMBER-OF-ARGS 5) 
(DE VSLSE_MK (V E O I L) (LIST 'VSLSTACKELEM V E O I L)) 
(SWITCH (LIST 'VSLSEPRINTNLS)) 
(ON1 'VSLSEPRINTNLS) 
(PUT 'VSLSE_PRINT 'NUMBER-OF-ARGS 1) 
(DE VSLSE_PRINT (SE)
    (PROG (NLS)
      (COND
       (*VSLSEPRINTNLS
        (PROGN
         (IOTO_PRIN2 "|  nls = ")
         (SETQ NLS (VSLSE_NLS SE))
         (COND ((NULL NLS) (IOTO_PRIN2T "nil"))
               (T
                (PROGN
                 (IOTO_PRIN2T
                  (LIST "|"
                        (IOTO_FORM2STR
                         (RL_PREPFOF
                          (COND
                           ((AND (CAR NLS) (CDR (CAR NLS)))
                            (CONS 'OR (CAR NLS)))
                           ((NULL (CAR NLS))
                            (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR (CAR NLS))))))))
                 (PROG (ATF)
                   (SETQ ATF (CDR NLS))
                  LAB
                   (COND ((NULL ATF) (RETURN NIL)))
                   ((LAMBDA (ATF)
                      (PROGN
                       (IOTO_PRIN2 "        ")
                       (IOTO_PRIN2T
                        (LIST "|"
                              (IOTO_FORM2STR
                               (RL_PREPFOF
                                (COND ((AND ATF (CDR ATF)) (CONS 'OR ATF))
                                      ((NULL ATF)
                                       (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                      (T (CAR ATF)))))))))
                    (CAR ATF))
                   (SETQ ATF (CDR ATF))
                   (GO LAB)))))
         (IOTO_TPRIN2 "          "))))
      (IOTO_PRIN2T
       (LIST "|" (VSLSE_V SE) "="
             (IOTO_FORM2STR
              (COND ((IDP (VSLSE_ETERM SE)) (VSLSE_ETERM SE))
                    (T (PREPSQ (VSLSE_ETERM SE))))))))) 
(FLUID '(RLALPHASTAT*)) 
(FLUID '(RLUDSC*)) 
(PUT 'VSL_VSL 'NUMBER-OF-ARGS 1) 
(DE VSL_VSL (INPUTL)
    (PROG (STATE FREEVARL SL KGEQ0 RLALPHASTAT* RLUDSC* SUBSTC)
      (SETQ RLUDSC* 0)
      (SETQ SUBSTC 0)
      (SETQ STATE (VSLS_MK (VSL_NORMALIZE INPUTL) NIL NIL))
      (COND
       ((AND *RLVERBOSE *RLVSLLOG)
        (PROGN (IOTO_TPRIN2T "<normalize>") (VSLS_PRINT STATE))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (NOT
            ((LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))
             (CAR (VSLS_IL STATE)))))
          (RETURN NIL)))
        (PROGN
         (SETQ FREEVARL (VSL_FREEVARL STATE))
         (COND (FREEVARL (VSL_DECIDE STATE FREEVARL)))
         (VSL_SUBSTITUTE STATE)
         (SETQ SL (VSLS_SL STATE))
         (COND
          ((AND *RLVERBOSE (NOT (VSLSE_EOESETP (CAR SL))))
           (SETQ SUBSTC (PLUS SUBSTC 1))))
         (COND
          ((VSLSE_EOESETP (CAR SL))
           (COND ((CDR SL) (VSL_IBACKTRACK STATE)) (T (VSL_FAIL STATE))))
          ((SETQ KGEQ0 (VSL_TINCONSISTENTP STATE))
           (PROGN
            (COND (*RLVSLLEARN (VSL_ACONFLICT STATE KGEQ0)))
            (VSL_LBACKTRACK STATE)))
          ((NULL (VSL_FREEVARL STATE)) (VSL_SUCCEED STATE))))
        (GO WHILELABEL))
      (COND (*RLVERBOSE (VSL_STAT SUBSTC)))
      (RETURN (CAR (VSLS_IL STATE))))) 
(PUT 'VSL_STAT 'NUMBER-OF-ARGS 1) 
(PUT 'VSL_STAT 'DEFINED-ON-LINE '226) 
(PUT 'VSL_STAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'VSL_STAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSL_STAT (SUBSTC)
    (PROG (ALPHL NALPHL LEN SNA)
      (SETQ LEN 0)
      (SETQ SNA 0)
      (SETQ LEN (LENGTH RLALPHASTAT*))
      (IOTO_TPRIN2T (LIST "computed nodes: " SUBSTC))
      (IOTO_TPRIN2T (LIST "solves: " LEN))
      (IOTO_TPRIN2T (LIST "underdetermined solves: " RLUDSC*))
      (PROG (PR)
        (SETQ PR RLALPHASTAT*)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (PROG (W1)
              (SETQ W1 (CAR PR))
              (SETQ NALPHL (CONS W1 NALPHL))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CDR PR))
              (SETQ ALPHL (CONS W1 ALPHL))
              (RETURN W1))
            (COND ((EQN (CAR PR) 0) (SETQ SNA (PLUS SNA 1))))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (IOTO_TPRIN2T (LIST "solves without negative alpha (<=1): " SNA))
      (COND
       ((GREATERP LEN 0)
        (PROGN
         (IOTO_TPRIN2T
          (LIST "alphas: " "min = " (LTO_MIN ALPHL) ", " "max = "
                (LTO_MAX ALPHL) ", " "avg = " (LTO_RAVG ALPHL) ", " "median = "
                (LTO_RMEDIAN ALPHL)))
         (IOTO_TPRIN2T
          (LIST "negative alphas: " "min = " (LTO_MIN NALPHL) ", " "max = "
                (LTO_MAX NALPHL) ", " "avg = " (LTO_RAVG NALPHL) ", "
                "median = " (LTO_RMEDIAN NALPHL)))))))) 
(PUT 'VSL_NORMALIZE 'NUMBER-OF-ARGS 1) 
(DE VSL_NORMALIZE (INPUTL)
    (PROG (OP)
      (RETURN
       (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
         (SETQ ATF INPUTL)
        STARTOVER
         (COND ((NULL ATF) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (ATF)
                    (PROGN
                     (SETQ OP (CAR ATF))
                     (COND ((EQ OP 'GEQ) (LIST ATF))
                           ((EQ OP 'LEQ)
                            (LIST (LIST 'GEQ (NEGF (CADR ATF)) NIL)))
                           ((EQ OP 'EQUAL)
                            (LIST (LIST 'GEQ (NEGF (CADR ATF)) NIL)
                                  (LIST 'GEQ (CADR ATF) NIL)))
                           (T
                            (REDERR (LIST "Illegal relation" OP "in" ATF))))))
                  (CAR ATF)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ ATF (CDR ATF))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL ATF) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (ATF)
                    (PROGN
                     (SETQ OP (CAR ATF))
                     (COND ((EQ OP 'GEQ) (LIST ATF))
                           ((EQ OP 'LEQ)
                            (LIST (LIST 'GEQ (NEGF (CADR ATF)) NIL)))
                           ((EQ OP 'EQUAL)
                            (LIST (LIST 'GEQ (NEGF (CADR ATF)) NIL)
                                  (LIST 'GEQ (CADR ATF) NIL)))
                           (T
                            (REDERR (LIST "Illegal relation" OP "in" ATF))))))
                  (CAR ATF)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ ATF (CDR ATF))
         (GO LOOPLABEL))))) 
(PUT 'VSL_FREEVARL 'NUMBER-OF-ARGS 1) 
(DE VSL_FREEVARL (S)
    (CL_FVARL
     ((LAMBDA (G490)
        (COND ((AND G490 (CDR G490)) (CONS 'AND G490))
              ((NULL G490) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
              (T (CAR G490))))
      (VSL_ILS S)))) 
(PUT 'VSLSE_EOESETP 'NUMBER-OF-ARGS 1) 
(DE VSLSE_EOESETP (SE) (EQ (VSLSE_ETERM SE) 'BOTTOM)) 
(SWITCH (LIST 'VSLFAST)) 
(PUT 'VSL_ETERM 'NUMBER-OF-ARGS 2) 
(DE VSL_ETERM (S X)
    (PROG (ESET SB B BORIG)
      (SETQ ESET (VSLS_ESETGET S X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ESET (NOT SB))) (RETURN NIL)))
        (PROGN
         (PROG (G491)
           (SETQ G491 (PROG1 (CAR ESET) (SETQ ESET (CDR ESET))))
           (SETQ B (CAR G491))
           (SETQ BORIG (CDR G491))
           (RETURN G491))
         (COND ((VSL_INFP B) (SETQ SB (VSL_ETERMI S X B)))
               (T (SETQ SB (VSL_ETERMSQ S X B BORIG)))))
        (GO WHILELABEL))
      (VSLS_ESETPUT S X ESET)
      (RETURN (OR SB (VSLSE_MK X 'BOTTOM NIL NIL NIL))))) 
(PUT 'VSL_ETERMI 'NUMBER-OF-ARGS 3) 
(DE VSL_ETERMI (S X INF)
    (PROG (AP NILS SB)
      (SETQ AP (VSL_ADMISSIBLEP (VSL_NLS S) X INF))
      (COND
       (AP
        (PROGN
         (VSLS_PUT S 'INCONSISTENT NIL)
         (SETQ NILS
                 (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ATF (VSL_ILS S))
                   (COND ((NULL ATF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ATF) (VSL_SUBATI ATF X INF))
                                     (CAR ATF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ATF (CDR ATF))
                   (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ATF) (VSL_SUBATI ATF X INF)) (CAR ATF))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ SB (VSLSE_MK X INF NIL NILS (CAR AP)))))
       ((AND *RLVERBOSE *RLVSLLOG)
        (IOTO_TPRIN2T (LIST "   dropped test term by learning: " X "=" INF))))
      (RETURN SB))) 
(PUT 'VSL_ETERMSQ 'NUMBER-OF-ARGS 4) 
(DE VSL_ETERMSQ (S X B BORIG)
    (PROG (LHS Q AP NILS SB)
      (SETQ LHS (CADR B))
      (SETQ Q (MULTSQ (CONS (NEGF (CDR LHS)) 1) (INVSQ (CONS (CDAR LHS) 1))))
      (SETQ AP (VSL_ADMISSIBLEP (VSL_NLS S) X Q))
      (COND
       (AP
        (PROGN
         (COND (*VSLFAST (SETQ NILS (VSL_SUBILSSQ S (VSL_ILS S) X Q)))
               (T
                (SETQ NILS
                        (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                          (SETQ ATF (VSL_ILS S))
                          (COND ((NULL ATF) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (ATF)
                                              (VSL_SUBATSQ ATF X Q))
                                            (CAR ATF))
                                           NIL)))
                         LOOPLABEL
                          (SETQ ATF (CDR ATF))
                          (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (ATF) (VSL_SUBATSQ ATF X Q))
                                    (CAR ATF))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))))
         (SETQ SB (VSLSE_MK X Q BORIG NILS (CAR AP)))))
       ((AND *RLVERBOSE *RLVSLLOG)
        (IOTO_TPRIN2T
         (LIST "   dropped test term by learning: " X "="
               (IOTO_FORM2STR (PREPSQ Q))))))
      (RETURN SB))) 
(PUT 'VSL_INFP 'NUMBER-OF-ARGS 1) 
(DE VSL_INFP (S) (OR (EQ S 'MINF) (EQ S 'PINF))) 
(PUT 'VSLS_ESETGET 'NUMBER-OF-ARGS 2) 
(DE VSLS_ESETGET (S X)
    (PROG (W)
      (SETQ W (VSLS_KVGET S 'ESET))
      (COND (NIL NIL))
      (SETQ W (ATSOC X (CDR W)))
      (COND (NIL NIL))
      (RETURN (CDR W)))) 
(PUT 'VSLS_ESETPUT 'NUMBER-OF-ARGS 3) 
(DE VSLS_ESETPUT (S X ESET)
    (PROG (W ESETAL)
      (SETQ W (VSLS_KVGET S 'ESET))
      (COND ((NULL W) (RETURN (VSLS_PUT S 'ESET (LIST (CONS X ESET))))))
      (SETQ ESETAL (CDR W))
      (SETQ W (ATSOC X ESETAL))
      (COND ((NULL W) (RETURN (VSLS_PUT S 'ESET (CONS (CONS X ESET) ESETAL)))))
      (SETCDR W ESET)
      (RETURN ESET))) 
(PUT 'VSL_ESET 'NUMBER-OF-ARGS 2) 
(DE VSL_ESET (S X)
    (PROG (IL BORIG LHS B LBL UBL)
      (SETQ IL (VSLS_IL S))
      (PROG (B)
        (SETQ B (VSL_ILS S))
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ BORIG (PROG1 (CAR IL) (SETQ IL (CDR IL))))
            (SETQ LHS (SFTO_REORDER (SFTO_DPRPARTKSF (CADR B)) X))
            (COND
             ((AND (NOT (OR (ATOM LHS) (ATOM (CAR LHS)))) (EQ (CAAAR LHS) X))
              (PROGN
               (SETQ B (LIST 'GEQ LHS NIL))
               (COND
                ((GREATERP (CDAR LHS) 0)
                 (COND
                  ((NOT (ASSOC B LBL))
                   (PROG (W1)
                     (SETQ W1 (CONS B BORIG))
                     (SETQ LBL (CONS W1 LBL))
                     (RETURN W1)))))
                ((NOT (ASSOC B UBL))
                 (PROG (W1)
                   (SETQ W1 (CONS B BORIG))
                   (SETQ UBL (CONS W1 UBL))
                   (RETURN W1)))))))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (COND ((AND (NULL LBL) (NULL UBL)) (RETURN NIL)))
      (RETURN (VSL_ESET1 LBL UBL)))) 
(PUT 'VSL_ESET1 'NUMBER-OF-ARGS 2) 
(DE VSL_ESET1 (LBL UBL)
    (COND ((NULL LBL) (LIST (CONS 'MINF NIL)))
          ((NULL UBL) (LIST (CONS 'PINF NIL)))
          ((GREATERP (LENGTH LBL) (LENGTH UBL)) UBL) (T LBL))) 
(PUT 'VSL_ILS 'NUMBER-OF-ARGS 1) 
(DE VSL_ILS (S)
    ((LAMBDA (SL) (COND (SL (VSLSE_ILS (CAR SL))) (T (VSLS_IL S))))
     (VSLS_SL S))) 
(PUT 'VSL_NLS 'NUMBER-OF-ARGS 1) 
(DE VSL_NLS (S)
    ((LAMBDA (SL) (COND (SL (VSLSE_NLS (CAR SL))) (T (VSLS_NL S))))
     (VSLS_SL S))) 
(PUT 'VSL_ADMISSIBLEP 'NUMBER-OF-ARGS 3) 
(DE VSL_ADMISSIBLEP (NLS X ETERM)
    (PROG (C NLEM NNLS)
      (COND ((NOT *RLVSLLEARN) (RETURN (LIST NIL))))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C NLS)) (RETURN NIL)))
        (PROGN
         (SETQ NLEM
                 (VSL_SUBNLEM (PROG1 (CAR NLS) (SETQ NLS (CDR NLS))) X ETERM))
         (COND
          ((OR (NULL NLEM) (NEQ (CADR (CAR NLEM)) 1))
           (COND ((NULL NLEM) (SETQ C NIL))
                 (T (PROGN (SETQ NNLS (CONS NLEM NNLS)) NLEM))))))
        (GO WHILELABEL))
      (RETURN (COND (C (LIST (REVERSIP NNLS))))))) 
(PUT 'VSL_SUBSTACK 'NUMBER-OF-ARGS 2) 
(DE VSL_SUBSTACK (F SL) (CL_APPLY2ATS1 F (FUNCTION VSL_SUBSTACKAT) (LIST SL))) 
(PUT 'VSL_SUBSTACKAT 'NUMBER-OF-ARGS 2) 
(DE VSL_SUBSTACKAT (ATF SL)
    (COND ((NULL SL) ATF)
          (T
           (VSL_SUBAT (VSL_SUBSTACKAT ATF (CDR SL)) (VSLSE_V (CAR SL))
            (VSLSE_ETERM (CAR SL)))))) 
(PUT 'VSL_SUBNLEM 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBNLEM (NLEM X ETERM)
    (PROGN
     (COND (NIL NIL))
     (COND ((OR (EQ ETERM 'MINF) (EQ ETERM 'PINF)) (VSL_SUBNLEMI NLEM X ETERM))
           (T (VSL_SUBNLEMSQ NLEM X ETERM))))) 
(PUT 'VSL_NLEMP 'NUMBER-OF-ARGS 1) 
(DE VSL_NLEMP (NLEM)
    (COND
     (*VSLGREATERPLEM
      (OR (NULL NLEM)
          (AND (EQ (CAR (CAR NLEM)) 'GREATERP) (VSL_NLEMP (CDR NLEM)))))
     (T
      (OR (NULL NLEM)
          (AND (EQ (CAR (CAR NLEM)) 'NEQ) (VSL_NLEMP (CDR NLEM))))))) 
(PUT 'VSL_SUBNLEMI 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBNLEMI (NLEM X INF)
    (PROG (C ATF LHS NNLEM)
      (COND
       (*VSLGREATERPLEM
        (PROGN
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C NLEM)) (RETURN NIL)))
           (PROGN
            (SETQ ATF (PROG1 (CAR NLEM) (SETQ NLEM (CDR NLEM))))
            (SETQ LHS (CADR ATF))
            (COND
             ((NOT (SFTO_KMEMBERF X LHS))
              (PROGN (SETQ NNLEM (CONS ATF NNLEM)) ATF))
             ((OR (AND (EQ INF 'PINF) (GREATERP (VSL_FASTCOEFF LHS X) 0))
                  (AND (EQ INF 'MINF) (LESSP (VSL_FASTCOEFF LHS X) 0)))
              (SETQ C NIL))))
           (GO WHILELABEL))
         (RETURN
          (COND (C (REVERSIP NNLEM)) (T (LIST (LIST 'GREATERP 1 NIL)))))))
       (T
        (PROGN
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C NLEM)) (RETURN NIL)))
           (PROGN
            (SETQ ATF (PROG1 (CAR NLEM) (SETQ NLEM (CDR NLEM))))
            (SETQ LHS (CADR ATF))
            (COND
             ((NOT (SFTO_KMEMBERF X LHS))
              (PROGN (SETQ NNLEM (CONS ATF NNLEM)) ATF))
             (T (SETQ C NIL))))
           (GO WHILELABEL))
         (RETURN (COND (C (REVERSIP NNLEM)) (T (LIST (LIST 'NEQ 1 NIL)))))))))) 
(PUT 'VSL_FASTCOEFF 'NUMBER-OF-ARGS 2) 
(DE VSL_FASTCOEFF (F X)
    (COND ((EQ (CAAAR F) X) (CDAR F)) (T (FVSL_FASTCOEFF (CDR F) X)))) 
(PUT 'VSL_SUBNLEMSQ 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBNLEMSQ (NLEM X Q)
    (PROG (C ATF NNLEM LHS)
      (COND
       (*VSLGREATERPLEM
        (PROGN
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C NLEM)) (RETURN NIL)))
           (PROGN
            (SETQ ATF
                    (VSL_SUBATSQ (PROG1 (CAR NLEM) (SETQ NLEM (CDR NLEM))) X
                     Q))
            (SETQ LHS (CADR ATF))
            (COND
             ((OR (NOT (OR (ATOM LHS) (ATOM (CAR LHS))))
                  (AND (NOT (NULL LHS)) (GREATERP LHS 0)))
              (COND
               ((OR (ATOM LHS) (ATOM (CAR LHS)))
                (PROGN
                 (SETQ NNLEM (LIST (LIST 'GREATERP 1 NIL)))
                 (SETQ C NIL)))
               (T (PROGN (SETQ NNLEM (CONS ATF NNLEM)) ATF))))))
           (GO WHILELABEL))
         (RETURN (REVERSIP NNLEM))))
       (T
        (PROGN
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C NLEM)) (RETURN NIL)))
           (PROGN
            (SETQ ATF
                    (VSL_SUBATSQ (PROG1 (CAR NLEM) (SETQ NLEM (CDR NLEM))) X
                     Q))
            (SETQ LHS (CADR ATF))
            (COND
             ((OR (NOT (OR (ATOM LHS) (ATOM (CAR LHS)))) (NOT (NULL LHS)))
              (COND
               ((OR (ATOM LHS) (ATOM (CAR LHS)))
                (PROGN
                 (SETQ NNLEM (LIST (LIST 'GREATERP 1 NIL)))
                 (SETQ C NIL)))
               (T (PROGN (SETQ NNLEM (CONS ATF NNLEM)) ATF))))))
           (GO WHILELABEL))
         (RETURN (REVERSIP NNLEM))))))) 
(PUT 'VSL_SUBAT 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBAT (ATF X ETERM)
    (PROGN
     (COND (NIL NIL))
     (COND ((OR (EQ ETERM 'MINF) (EQ ETERM 'PINF)) (VSL_SUBATI ATF X ETERM))
           (T (VSL_SUBATSQ ATF X ETERM))))) 
(PUT 'VSL_SUBATI 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBATI (ATF X INF)
    (PROG (RES LHS)
      (COND (NIL NIL))
      (SETQ LHS (CADR ATF))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT RES) (NOT (OR (ATOM LHS) (ATOM (CAR LHS))))))
          (RETURN NIL)))
        (PROGN
         (COND
          ((EQ (CAAAR LHS) X)
           (PROGN (COND (NIL NIL)) (SETQ RES (LIST 'GEQ NIL NIL)))))
         (SETQ LHS (CDR LHS)))
        (GO WHILELABEL))
      (RETURN (OR RES ATF)))) 
(PUT 'VSL_SUBIMATCH 'NUMBER-OF-ARGS 2) 
(DE VSL_SUBIMATCH (F B)
    (OR (AND (MINUSF F) (EQ B 'MINF)) (AND (NOT (MINUSF F)) (EQ B 'PINF)))) 
(PUT 'VSL_SUBILSSQ 'NUMBER-OF-ARGS 4) 
(DE VSL_SUBILSSQ (S ILS X Q)
    (PROG (C IL ORIG ATF NATF NLHS NILS)
      (SETQ IL (VSLS_IL S))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C ILS)) (RETURN NIL)))
        (PROGN
         (SETQ ATF (PROG1 (CAR ILS) (SETQ ILS (CDR ILS))))
         (SETQ ORIG (PROG1 (CAR IL) (SETQ IL (CDR IL))))
         (SETQ NATF (VSL_SUBATSQ ATF X Q))
         (SETQ NLHS (CADR NATF))
         (COND
          ((OR (NOT (OR (ATOM NLHS) (ATOM (CAR NLHS)))) (NOT (MINUSF NLHS)))
           (PROGN (SETQ NILS (CONS NATF NILS)) NATF))
          (T (PROGN (SETQ C NIL)))))
        (GO WHILELABEL))
      (VSLS_PUT S 'INCONSISTENT (COND ((NULL C) ORIG)))
      (RETURN (REVERSIP NILS)))) 
(PUT 'VSL_SUBATSQ 'NUMBER-OF-ARGS 3) 
(DE VSL_SUBATSQ (ATF X Q) (LIST (CAR ATF) (CAR (OFSF_SUBF (CADR ATF) X Q)) NIL)) 
(PUT 'VSL_DECIDE 'NUMBER-OF-ARGS 2) 
(DE VSL_DECIDE (S FREEVARL)
    (PROG (IL LHSL W LHS A VARL V)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2 "<decide>")))
      (SETQ IL (VSLS_IL S))
      (SETQ LHSL
              (PROG (B FORALL-RESULT FORALL-ENDPTR)
                (SETQ B (VSL_ILS S))
                (COND ((NULL B) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (B) (CADR B)) (CAR B)) NIL)))
               LOOPLABEL
                (SETQ B (CDR B))
                (COND ((NULL B) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (B) (CADR B)) (CAR B)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LHSL (NOT W))) (RETURN NIL)))
        (PROGN
         (SETQ LHS (NEGF (PROG1 (CAR LHSL) (SETQ LHSL (CDR LHSL)))))
         (SETQ A (PROG1 (CAR IL) (SETQ IL (CDR IL))))
         (COND
          ((AND (NOT (OR (ATOM LHS) (ATOM (CAR LHS)))) (MEMBER LHS LHSL))
           (SETQ W (LIST (CONS (LIST 'GEQ LHS NIL) A))))))
        (GO WHILELABEL))
      (COND
       (W
        (PROGN
         (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_PRIN2 " *gauss*")))
         (SETQ V (CAAAR LHS))
         (VSLS_ESETPUT S V W)))
       (T
        (PROGN
         (PROG (G492)
           (SETQ G492 (VSL_VARSEL FREEVARL (VSL_ILS S)))
           (SETQ V (CAR G492))
           (SETQ VARL (CDR G492))
           (RETURN G492))
         (VSLS_ESETPUT S V (VSL_ESET S V)))))
      (VSLS_SETSL S (CONS (VSLSE_MK V NIL NIL NIL NIL) (VSLS_SL S)))
      (COND ((AND *RLVERBOSE *RLVSLLOG) (PROGN (TERPRI) (VSLS_PRINT S)))))) 
(PUT 'VSL_UNDECIDEDP 'NUMBER-OF-ARGS 1) 
(DE VSL_UNDECIDEDP (S)
    (PROG (SL)
      (SETQ SL (VSLS_SL S))
      (RETURN (OR (NULL SL) (NOT (NULL (VSLSE_ETERM (CAR SL)))))))) 
(PUT 'VSL_VARSEL 'NUMBER-OF-ARGS 2) 
(DE VSL_VARSEL (VARL ILS)
    (PROG (BT BESTV UBN LBN BESTN THISN)
      (SETQ UBN 0)
      (SETQ LBN 0)
      (SETQ BESTN 0)
      (SETQ THISN 0)
      (COND (NIL NIL))
      (COND (NIL NIL))
      (COND ((NOT *RLQEVARSEL) (RETURN VARL)))
      (SETQ BESTN (MINUS 1))
      (PROG (X)
        (SETQ X VARL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (PROG (ATF)
              (SETQ ATF ILS)
             LAB
              (COND ((NULL ATF) (RETURN NIL)))
              ((LAMBDA (ATF)
                 (PROGN
                  (SETQ BT (VSL_BOUNDTYPE ATF X))
                  (COND ((EQ BT 'UB) (SETQ UBN (PLUS UBN 1)))
                        ((EQ BT 'LB) (SETQ LBN (PLUS LBN 1))))))
               (CAR ATF))
              (SETQ ATF (CDR ATF))
              (GO LAB))
            (SETQ THISN (MIN UBN LBN))
            (COND
             ((OR (EQN BESTN (MINUS 1)) (LESSP THISN BESTN))
              (PROGN (SETQ BESTN THISN) (SETQ BESTV X))))
            (SETQ UBN 0)
            (SETQ LBN 0)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS BESTV (LTO_DELQ BESTV VARL))))) 
(PUT 'VSL_BOUNDTYPE 'NUMBER-OF-ARGS 2) 
(DE VSL_BOUNDTYPE (ATF X) (VSL_BOUNDTYPE1 (CADR ATF) X)) 
(PUT 'VSL_BOUNDTYPE1 'NUMBER-OF-ARGS 2) 
(DE VSL_BOUNDTYPE1 (F X)
    (COND ((OR (ATOM F) (ATOM (CAR F))) NIL)
          ((EQ (CAAAR F) X) (COND ((MINUSF (CDAR F)) 'UB) (T 'LB)))
          (T (VSL_BOUNDTYPE1 (CDR F) X)))) 
(PUT 'VSL_SUBSTITUTE 'NUMBER-OF-ARGS 1) 
(DE VSL_SUBSTITUTE (S)
    (PROG (SL V)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2 "<substitute>")))
      (SETQ SL (VSLS_SL S))
      (COND (NIL NIL))
      (SETQ V (VSLSE_V (PROG1 (CAR SL) (SETQ SL (CDR SL)))))
      (VSLS_SETSL S SL)
      (PROG (W1) (SETQ W1 (VSL_ETERM S V)) (SETQ SL (CONS W1 SL)) (RETURN W1))
      (VSLS_SETSL S SL)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (PROGN (TERPRI) (VSLS_PRINT S)))))) 
(PUT 'VSL_ACONFLICT 'NUMBER-OF-ARGS 2) 
(DE VSL_ACONFLICT (S KGEQ0)
    (PROG (SL NLEM W)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2T "<aconflict>")))
      (SETQ SL (VSLS_SL S))
      (COND (NIL NIL))
      (SETQ NLEM
              (VSL_ANALYZE
               (PROG (SE FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SE SL)
                STARTOVER
                 (COND ((NULL SE) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (SE)
                            (COND ((SETQ W (VSLSE_ORIG SE)) (LIST W))))
                          (CAR SE)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ SE (CDR SE))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL SE) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (SE)
                            (COND ((SETQ W (VSLSE_ORIG SE)) (LIST W))))
                          (CAR SE)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ SE (CDR SE))
                 (GO LOOPLABEL))
               KGEQ0))
      (VSLS_SETNL S (CONS NLEM (VSLS_NL S)))
      (COND
       ((AND *RLVERBOSE *RLVSLLOG)
        (IOTO_PRIN2T
         (LIST "   nlearned: "
               (IOTO_FORM2STR
                (RL_PREPFOF
                 (COND ((AND NLEM (CDR NLEM)) (CONS 'OR NLEM))
                       ((NULL NLEM) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR NLEM))))))))))) 
(PUT 'VSL_LBACKTRACK 'NUMBER-OF-ARGS 1) 
(DE VSL_LBACKTRACK (S)
    (PROG (NSL NNLEM RSL C SE)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2 "<lbacktrack> ")))
      (COND
       ((NOT *RLVSLLEARN)
        (PROGN
         (SETQ NSL (VSLS_SL S))
         (SETQ SE (PROG1 (CAR NSL) (SETQ NSL (CDR NSL))))
         (SETQ NSL (CONS (VSLSE_MK (VSLSE_V SE) NIL NIL NIL NIL) NSL))
         (VSLS_SETSL S NSL)
         (RETURN NIL))))
      (SETQ NNLEM (CAR (VSLS_NL S)))
      (SETQ RSL (REVERSE (VSLS_SL S)))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C RSL)) (RETURN NIL)))
        (PROGN
         (SETQ SE (PROG1 (CAR RSL) (SETQ RSL (CDR RSL))))
         (SETQ NNLEM (VSL_SUBNLEM NNLEM (VSLSE_V SE) (VSLSE_ETERM SE)))
         (COND
          (NNLEM
           (PROG (W1)
             (SETQ W1
                     (VSLSE_MK (VSLSE_V SE) (VSLSE_ETERM SE) (VSLSE_ORIG SE)
                      (VSLSE_ILS SE) (CONS NNLEM (VSLSE_NLS SE))))
             (SETQ NSL (CONS W1 NSL))
             (RETURN W1)))
          (T (SETQ C NIL))))
        (GO WHILELABEL))
      (COND (NIL NIL))
      (PROG (W1)
        (SETQ W1 (VSLSE_MK (VSLSE_V SE) NIL NIL NIL NIL))
        (SETQ NSL (CONS W1 NSL))
        (RETURN W1))
      (VSLS_SETSL S NSL)
      (COND
       ((AND *RLVERBOSE *RLVSLLOG)
        (PROGN
         (IOTO_PRIN2 (LIST "(" (PLUS (LENGTH RSL) 1) ")"))
         (VSLS_PRINT S)))))) 
(PUT 'VSL_TINCONSISTENTP 'NUMBER-OF-ARGS 1) 
(DE VSL_TINCONSISTENTP (S)
    (PROG (IL ILS C W SLHS)
      (COND (*VSLFAST (RETURN (CDR (VSLS_KVGET S 'INCONSISTENT)))))
      (SETQ IL (VSLS_IL S))
      (SETQ ILS (VSL_ILS S))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C ILS)) (RETURN NIL)))
        (PROGN
         (SETQ W (PROG1 (CAR IL) (SETQ IL (CDR IL))))
         (SETQ SLHS (CADR (PROG1 (CAR ILS) (SETQ ILS (CDR ILS)))))
         (COND
          ((AND (OR (ATOM SLHS) (ATOM (CAR SLHS))) (MINUSF SLHS))
           (SETQ C NIL))))
        (GO WHILELABEL))
      (RETURN (COND ((NOT C) W))))) 
(PUT 'VSL_ANALYZE 'NUMBER-OF-ARGS 2) 
(DE VSL_ANALYZE (L KGEQ0)
    (PROG (XL Y YL HUGO RHUGO SYSL SOLAL W NLEARNL ALPHAC NALPHAC)
      (SETQ ALPHAC 0)
      (SETQ NALPHAC 0)
      (SETQ XL
              (CL_FVARL
               ((LAMBDA (G494)
                  (COND ((AND G494 (CDR G494)) (CONS 'AND G494))
                        ((NULL G494) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                        (T (CAR G494))))
                (CONS KGEQ0 L))))
      (PROG (C)
        (SETQ C L)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ Y (GENSYM))
            (PROGN (SETQ YL (CONS Y YL)) Y)
            (SETQ HUGO
                    (ADDF HUGO
                          ((LAMBDA (G495)
                             (COND
                              (*PHYSOP-LOADED (PHYSOP-MULTF G495 (CADR C)))
                              (T (POLY-MULTF G495 (CADR C)))))
                           (LIST (CONS (CONS Y 1) 1)))))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (SETQ HUGO (ADDF HUGO (CADR KGEQ0)))
      (SETQ RHUGO (SFTO_LREORDER HUGO XL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM RHUGO) (ATOM (CAR RHUGO))))
                (MEMQ (CAAAR RHUGO) XL)))
          (RETURN NIL)))
        (PROGN
         (PROG (W1)
           (SETQ W1 (CDAR RHUGO))
           (SETQ SYSL (CONS W1 SYSL))
           (RETURN W1))
         (SETQ RHUGO (CDR RHUGO)))
        (GO WHILELABEL))
      (SETQ SOLAL (VSL_SOLVE SYSL YL))
      (SETQ HUGO (SFTO_LREORDER HUGO YL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM HUGO) (ATOM (CAR HUGO))))
                (SETQ W (ATSOC (CAAAR HUGO) SOLAL))))
          (RETURN NIL)))
        (PROGN
         (COND (*RLVERBOSE (SETQ ALPHAC (PLUS ALPHAC 1))))
         (COND
          ((MINUSF (CAR (CDR W)))
           (PROGN
            (COND (*RLVERBOSE (SETQ NALPHAC (PLUS NALPHAC 1))))
            (COND
             (*VSLGREATERPLEM
              (PROG (W1)
                (SETQ W1 (LIST 'GREATERP (CDAR HUGO) NIL))
                (SETQ NLEARNL (CONS W1 NLEARNL))
                (RETURN W1)))
             (T
              (PROG (W1)
                (SETQ W1 (LIST 'NEQ (CDAR HUGO) NIL))
                (SETQ NLEARNL (CONS W1 NLEARNL))
                (RETURN W1)))))))
         (SETQ HUGO (CDR HUGO)))
        (GO WHILELABEL))
      (COND
       (*RLVERBOSE
        (SETQ RLALPHASTAT* (CONS (CONS NALPHAC ALPHAC) RLALPHASTAT*))))
      (RETURN NLEARNL))) 
(PUT 'VSL_SOLVE 'NUMBER-OF-ARGS 2) 
(DE VSL_SOLVE (SYSL YL)
    (PROG (TSL VL SL PLUGAL SUBAL RESAL S)
      (SETQ YL (SORT YL 'ORDOP))
      (SETQ TSL
              (COND (*CRAMER (SOLVECRAMER SYSL YL))
                    (T (SOLVEBAREISS SYSL YL))))
      (COND (NIL NIL))
      (SETQ TSL (CDR TSL))
      (COND (NIL NIL))
      (SETQ TSL (CAR TSL))
      (SETQ VL (CADR TSL))
      (SETQ SL (CAR TSL))
      (SETQ PLUGAL (VSL_PLUGIN (LTO_SETMINUS YL VL)))
      (SETQ SUBAL
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR PLUGAL)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (CAR PR) (PREPSQ (CDR PR))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR) (CONS (CAR PR) (PREPSQ (CDR PR))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RESAL PLUGAL)
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ S (SUBSQ (PROG1 (CAR SL) (SETQ SL (CDR SL))) SUBAL))
            (SETQ RESAL (CONS (CONS V S) RESAL))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (SORT RESAL (FUNCTION ORDOPCAR))))) 
(PUT 'VSL_PLUGIN 'NUMBER-OF-ARGS 1) 
(DE VSL_PLUGIN (YL)
    (PROGN
     (COND ((AND *RLVERBOSE YL) (SETQ RLUDSC* (PLUS RLUDSC* 1))))
     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
       (SETQ Y YL)
       (COND ((NULL Y) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (Y) (CONS Y (CONS NIL 1))) (CAR Y))
                             NIL)))
      LOOPLABEL
       (SETQ Y (CDR Y))
       (COND ((NULL Y) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (Y) (CONS Y (CONS NIL 1))) (CAR Y)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'VSL_IBACKTRACK 'NUMBER-OF-ARGS 1) 
(DE VSL_IBACKTRACK (S)
    (PROG (SL SE)
      (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2T "<ibacktrack>")))
      (SETQ SL (VSLS_SL S))
      (COND (NIL NIL))
      (PROG1 (CAR SL) (SETQ SL (CDR SL)))
      (SETQ SE (PROG1 (CAR SL) (SETQ SL (CDR SL))))
      (VSLS_SETSL S (CONS (VSLSE_MK (VSLSE_V SE) NIL NIL NIL NIL) SL))
      (COND ((AND *RLVERBOSE *RLVSLLOG) (VSLS_PRINT S))))) 
(PUT 'VSL_FAIL 'NUMBER-OF-ARGS 1) 
(DE VSL_FAIL (S)
    (PROGN
     (COND ((AND *RLVERBOSE *RLVSLLOG) (IOTO_TPRIN2T "<fail>")))
     (VSLS_SETIL S (LIST 'FALSE)))) 
(PUT 'VSL_SUCCEED 'NUMBER-OF-ARGS 1) 
(DE VSL_SUCCEED (S)
    (PROG (ILS)
      (COND
       ((AND *RLVERBOSE *RLVSLLOG)
        (PROGN
         (IOTO_TPRIN2T "<succeed>")
         (SETQ ILS (VSL_ILS S))
         (IOTO_TPRIN2
          (LIST "   F/S: |"
                (IOTO_FORM2STR
                 (RL_PREPFOF (PROG1 (CAR ILS) (SETQ ILS (CDR ILS)))))))
         (PROG (ATF)
           (SETQ ATF ILS)
          LAB
           (COND ((NULL ATF) (RETURN NIL)))
           ((LAMBDA (ATF)
              (IOTO_TPRIN2T
               (LIST "        |" (IOTO_FORM2STR (RL_PREPFOF ATF)))))
            (CAR ATF))
           (SETQ ATF (CDR ATF))
           (GO LAB)))))
      (VSLS_SETIL S (LIST 'TRUE)))) 
(PUT 'VSL_TRPRINTER 'NUMBER-OF-ARGS 1) 
(PUT 'VSL_TRPRINTER 'DEFINED-ON-LINE '810) 
(PUT 'VSL_TRPRINTER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'VSL_TRPRINTER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSL_TRPRINTER (S)
    (COND ((EQCAR S 'VSLSTATE) (VSLS_PRINT S))
          ((EQCAR S 'VSLSTACKELEM) (VSLSE_PRINT S))
          ((SQP S) (PRINTX (IOTO_FORM2STR (PREPSQ S)))) (T (PRINTX S)))) 
(PUT 'ISUB 'NUMBER-OF-ARGS 2) 
(FLAG '(ISUB) 'OPFN) 
(PUT 'ISUB 'DEFINED-ON-LINE '820) 
(PUT 'ISUB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSL.RED) 
(PUT 'ISUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ISUB (EQL FORM)
    (COND ((EVALEQUAL (AEVAL EQL) (AEVAL (LIST 'LIST))) (AEVAL FORM))
          (T
           (AEVAL
            (LIST 'ISUB (LIST 'REST EQL) (LIST 'SUB (LIST 'FIRST EQL) FORM)))))) 
(ENDMODULE) 