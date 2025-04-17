(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TALPSISM)) 
(REVISION 'TALPSISM
          "$Id: talpsism.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'TALPSISM "(c) 2004-2009 A. Dolzmann, T. Sturm, 2016 T. Sturm") 
(PUT 'TALP_SMUPDKNOWL 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_SMUPDKNOWL 'DEFINED-ON-LINE '32) 
(PUT 'TALP_SMUPDKNOWL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SMUPDKNOWL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SMUPDKNOWL (OP ATL KNOWL N)
    (PROG (AT)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ AT (CAR ATL))
         (SETQ ATL (CDR ATL))
         (SETQ KNOWL (TALP_SMUPDKNOWL1 OP AT KNOWL N))
         (COND ((EQ KNOWL 'FALSE) (PROGN (SETQ ATL NIL) (SETQ AT 'BREAK)))))
        (GO WHILELABEL))
      (COND ((EQ AT 'BREAK) (RETURN 'FALSE)) (T (RETURN KNOWL))))) 
(PUT 'TALP_SMUPDKNOWL1 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_SMUPDKNOWL1 'DEFINED-ON-LINE '55) 
(PUT 'TALP_SMUPDKNOWL1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SMUPDKNOWL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SMUPDKNOWL1 (OP AT KNOWL N)
    (PROG (ENT CONTRA)
      (COND
       ((EQ OP 'OR) (PROGN (SETQ ENT (RL_NEGATEAT AT)) (SETQ CONTRA AT) NIL))
       (T (PROGN (SETQ ENT AT) (SETQ CONTRA (RL_NEGATEAT AT)) NIL)))
      (COND ((ASSOC CONTRA KNOWL) (RETURN 'FALSE)))
      (COND ((TALP_CHKKNOWL ENT KNOWL) (RETURN 'FALSE)))
      (COND ((ASSOC ENT KNOWL) (RETURN KNOWL)))
      (RETURN (SETQ KNOWL (CONS (CONS ENT N) KNOWL))))) 
(PUT 'TALP_CHKKNOWL 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_CHKKNOWL 'DEFINED-ON-LINE '80) 
(PUT 'TALP_CHKKNOWL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_CHKKNOWL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_CHKKNOWL (ATF KNOWL)
    (PROG (INVT FS TVAR STOP AT ATOP TMP RESULT)
      (COND
       ((TALP_CANDP ATF)
        (PROGN
         (SETQ INVT
                 (COND
                  ((ATOM (CADR ATF))
                   (PROGN (SETQ TVAR (CADR ATF)) (CADDR ATF)))
                  (T (PROGN (SETQ TVAR (CADDR ATF)) (CADR ATF)))))
         (SETQ FS (TALP_INVF INVT))
         (SETQ ATOP (CAR ATF))
         (SETQ TMP KNOWL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND TMP (NOT STOP))) (RETURN NIL)))
           (PROGN
            (SETQ AT (CAAR TMP))
            (COND
             ((TALP_CANDP AT)
              (PROGN
               (SETQ INVT
                       (COND ((TALP_INVP (CADR AT)) (CADR AT)) (T (CADDR AT))))
               (COND
                ((EQ (CADR INVT) TVAR)
                 (COND
                  ((AND (EQ (COND ((ATOM AT) AT) (T (CAR AT))) ATOP)
                        (EQ ATOP 'NEQ))
                   (COND
                    ((NEQ (TALP_INVF INVT) FS)
                     (PROGN (SETQ RESULT 'TRUE) (SETQ STOP T)))))))))))
            (SETQ TMP (CDR TMP)))
           (GO WHILELABEL))
         NIL))
       (T (RETURN NIL)))
      (RETURN RESULT))) 
(FLAG '(RLTRYGS) 'OPFN) 
(PUT 'RLTRYGS 'NUMBER-OF-ARGS 1) 
(PUT 'RLTRYGS 'DEFINED-ON-LINE '117) 
(PUT 'RLTRYGS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'RLTRYGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLTRYGS (F) (TALP_TRY F)) 
(PUT 'TALP_TRY 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_TRY 'DEFINED-ON-LINE '127) 
(PUT 'TALP_TRY 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TRY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_TRY (F) (TALP_TRY1 (TALP_LSSIMPL (TALP_INVTSCSIMPL (RL_PNF F))))) 
(PUT 'TALP_TRY1 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_TRY1 'DEFINED-ON-LINE '131) 
(PUT 'TALP_TRY1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TRY1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_TRY1 (F)
    (PROG (TMP OP)
      (COND ((ATOM F) (RETURN F)))
      (SETQ TMP (TALP_RNF F))
      (COND
       ((OR (OR (EQ TMP 'TRUE) (EQ TMP 'FALSE)) (TALP_ATFP TMP)) (RETURN TMP)))
      (SETQ OP (CAR TMP))
      (RETURN
       (COND
        ((EQ OP 'OR)
         (CL_SIMPL
          (CL_NNFNOT
           (TALP_TRY2
            (CONS 'AND
                  (PROG (SF FORALL-RESULT FORALL-ENDPTR)
                    (SETQ SF (CDR TMP))
                    (COND ((NULL SF) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (SF) (TALP_TRY1 (CL_NNFNOT SF)))
                                      (CAR SF))
                                     NIL)))
                   LOOPLABEL
                    (SETQ SF (CDR SF))
                    (COND ((NULL SF) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (SF) (TALP_TRY1 (CL_NNFNOT SF)))
                              (CAR SF))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
          NIL (MINUS 1)))
        ((EQ OP 'AND)
         (CL_SIMPL
          (TALP_TRY2
           (CONS OP
                 (PROG (SF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SF (CDR TMP))
                   (COND ((NULL SF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SF) (TALP_TRY1 SF)) (CAR SF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SF (CDR SF))
                   (COND ((NULL SF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (SF) (TALP_TRY1 SF)) (CAR SF)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          NIL (MINUS 1)))
        (T
         (CL_SIMPL (CONS OP (CONS (CADR TMP) (LIST (TALP_TRY1 (CADDR TMP)))))
                   NIL (MINUS 1))))))) 
(PUT 'TALP_TRY2 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_TRY2 'DEFINED-ON-LINE '148) 
(PUT 'TALP_TRY2 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TRY2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_TRY2 (F)
    (PROG (BVARS FVARS VARS)
      (SETQ BVARS (CL_BVARL F))
      (SETQ FVARS (CL_FVARL F))
      (SETQ VARS (APPEND BVARS FVARS))
      (RETURN (COND (VARS (TALP_TRY3 F VARS)) (T F))))) 
(PUT 'TALP_TRY3 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_TRY3 'DEFINED-ON-LINE '157) 
(PUT 'TALP_TRY3 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TRY3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_TRY3 (F VARS)
    (PROG (RES SUBPAIRS EQUS EXTOBJ CONTLVAR CONTRVAR LHS RHS)
      (COND ((OR (OR (EQ F 'TRUE) (EQ F 'FALSE)) (TALP_ATFP F)) (RETURN F)))
      (SETQ RES F)
      (COND
       ((EQ (COND ((ATOM RES) RES) (T (CAR RES))) 'AND)
        (PROG (SUBF)
          (SETQ SUBF (CDR RES))
         LAB
          (COND ((NULL SUBF) (RETURN NIL)))
          ((LAMBDA (SUBF)
             (COND
              ((AND (NULL (ATOM SUBF)) (EQ (CAR SUBF) 'EQUAL))
               (PROGN
                (SETQ LHS (CADR SUBF))
                (SETQ RHS (CADDR SUBF))
                (PROG (X)
                  (SETQ X VARS)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (PROGN
                      (COND
                       ((OR (AND (ATOM LHS) (TALP_CONTAINS RHS X))
                            (LESSP (TALP_TD LHS) (TALP_TD RHS)))
                        (SETQ CONTRVAR T)))
                      (COND
                       ((OR (AND (ATOM RHS) (TALP_CONTAINS LHS X))
                            (LESSP (TALP_TD RHS) (TALP_TD LHS)))
                        (SETQ CONTLVAR T)))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (COND
                 (CONTLVAR
                  (PROGN
                   (SETQ EQUS (CONS SUBF EQUS))
                   (SETQ SUBPAIRS
                           (CONS (CONS (CADR SUBF) (CADDR SUBF)) SUBPAIRS)))))
                (COND
                 (CONTRVAR
                  (PROGN
                   (SETQ EQUS (CONS SUBF EQUS))
                   (SETQ SUBPAIRS
                           (CONS (CONS (CADDR SUBF) (CADR SUBF)) SUBPAIRS)))))
                (SETQ CONTLVAR NIL)
                (SETQ CONTRVAR NIL)
                NIL))))
           (CAR SUBF))
          (SETQ SUBF (CDR SUBF))
          (GO LAB))))
      (COND
       (SUBPAIRS
        (PROGN
         (SETQ EXTOBJ (TALP_EXTLFTRS SUBPAIRS EQUS VARS))
         (SETQ SUBPAIRS (CAR EXTOBJ))
         (SETQ EQUS (CDR EXTOBJ))
         NIL)))
      (RETURN
       (COND
        (SUBPAIRS
         ((LAMBDA (X) (COND ((CDR X) (TALP_TRY1 (CAR X))) (T (CAR X))))
          (TALP_CHSBSTRES RES SUBPAIRS EQUS)))
        (T (TALP_RNF RES)))))) 
(PUT 'TALP_SPECSUB 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_SPECSUB 'DEFINED-ON-LINE '195) 
(PUT 'TALP_SPECSUB 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SPECSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_SPECSUB (P F)
    (PROG (RES OP)
      (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) (RETURN F)))
      (COND ((TALP_ATFP F) (RETURN (TALP_SPECSUBAT (CAR P) (CDR P) F))))
      (SETQ OP (CAR F))
      (SETQ RES
              (CONS OP
                    (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                      (SETQ SUBF (CDR F))
                      (COND ((NULL SUBF) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (SUBF) (TALP_SPECSUB P SUBF))
                                        (CAR SUBF))
                                       NIL)))
                     LOOPLABEL
                      (SETQ SUBF (CDR SUBF))
                      (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (SUBF) (TALP_SPECSUB P SUBF))
                                (CAR SUBF))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN RES))) 
(PUT 'TALP_SPECSUBAT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SPECSUBAT 'DEFINED-ON-LINE '209) 
(PUT 'TALP_SPECSUBAT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SPECSUBAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SPECSUBAT (OLD NEW ATF)
    (TALP_SIMPAT
     (LIST (CAR ATF) (TALP_SPECSUBT OLD NEW (CADR ATF))
           (TALP_SPECSUBT OLD NEW (CADDR ATF))))) 
(PUT 'TALP_SPECSUBT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_SPECSUBT 'DEFINED-ON-LINE '218) 
(PUT 'TALP_SPECSUBT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SPECSUBT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_SPECSUBT (OLD NEW TERM)
    (PROG (TMP)
      (COND ((ATOM TERM) (RETURN (COND ((EQ TERM OLD) NEW) (T TERM)))))
      (COND ((TALP_EQTP OLD TERM) (RETURN NEW)))
      (SETQ TMP
              (CONS (CAR TERM)
                    (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELEM (CDR TERM))
                      (COND ((NULL ELEM) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELEM)
                                          (TALP_SPECSUBT OLD NEW ELEM))
                                        (CAR ELEM))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELEM (CDR ELEM))
                      (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELEM) (TALP_SPECSUBT OLD NEW ELEM))
                                (CAR ELEM))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN TMP))) 
(PUT 'TALP_EQTP 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_EQTP 'DEFINED-ON-LINE '230) 
(PUT 'TALP_EQTP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_EQTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_EQTP (T1 T2)
    (COND ((OR (ATOM T1) (ATOM T2)) (COND ((EQ T1 T2) T) (T NIL)))
          ((AND (ATOM (CAR T1)) (ATOM (CAR T2)))
           (COND ((EQ (CAR T1) (CAR T2)) (TALP_EQTP (CDR T1) (CDR T2)))
                 (T NIL)))
          (T
           (AND (TALP_EQTP (CAR T1) (CAR T2)) (TALP_EQTP (CDR T1) (CDR T2)))))) 
(PUT 'TALP_CHSBSTRES 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_CHSBSTRES 'DEFINED-ON-LINE '240) 
(PUT 'TALP_CHSBSTRES 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_CHSBSTRES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_CHSBSTRES (F LST EQUS)
    (PROG (ATNUM CURRATNUM CURRDEPTH STOP CURRSUM MAXD CHOSEN SUMD CURR)
      (SETQ ATNUM (TALP_ATNUM F))
      (SETQ MAXD (TALP_MAXD F))
      (SETQ SUMD (TALP_SUMD F))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LST (NOT STOP))) (RETURN NIL)))
        (PROGN
         (SETQ CURR
                 (CONS 'AND
                       (CONS (CAR EQUS) (LIST (TALP_SPECSUB (CAR LST) F)))))
         (SETQ CURR (TALP_RNF CURR))
         (SETQ EQUS (CDR EQUS))
         (SETQ LST (CDR LST))
         (COND
          ((OR (EQ CURR 'TRUE) (EQ CURR 'FALSE))
           (PROGN (SETQ STOP T) (SETQ CHOSEN CURR)))
          (T
           (PROGN
            (SETQ CURRSUM (TALP_SUMD CURR))
            (COND
             ((LESSP CURRSUM SUMD)
              (PROGN (SETQ CHOSEN CURR) (SETQ SUMD CURRSUM)))
             ((EQUAL CURRSUM SUMD)
              (PROGN
               (SETQ CURRDEPTH (TALP_MAXD CURR))
               (COND
                ((LESSP CURRDEPTH MAXD)
                 (PROGN (SETQ CHOSEN CURR) (SETQ MAXD CURRDEPTH)))
                ((EQUAL CURRDEPTH MAXD)
                 (PROGN
                  (SETQ CURRATNUM (TALP_ATNUM CURR))
                  (COND
                   ((LESSP CURRATNUM ATNUM)
                    (PROGN
                     (SETQ CHOSEN CURR)
                     (SETQ ATNUM CURRATNUM))))))))))))))
        (GO WHILELABEL))
      (RETURN (COND (CHOSEN (CONS CHOSEN T)) (T (CONS F NIL)))))) 
(PUT 'TALP_EXTLFTRS 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_EXTLFTRS 'DEFINED-ON-LINE '281) 
(PUT 'TALP_EXTLFTRS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_EXTLFTRS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_EXTLFTRS (SUBL EQL FVARS)
    (PROG (PW2V RST NEWSUBL NEWEQL TRANSL)
      (PROG (PAIR)
        (SETQ PAIR SUBL)
       LAB
        (COND ((NULL PAIR) (RETURN NIL)))
        ((LAMBDA (PAIR)
           (COND
            ((AND (MEMQ (CAR PAIR) FVARS) (MEMQ (CDR PAIR) FVARS))
             (SETQ PW2V (CONS PAIR PW2V)))
            (T (SETQ RST (CONS PAIR RST)))))
         (CAR PAIR))
        (SETQ PAIR (CDR PAIR))
        (GO LAB))
      (COND ((NOT (AND PW2V RST)) (RETURN (CONS SUBL EQL))))
      (SETQ NEWSUBL SUBL)
      (SETQ NEWEQL EQL)
      (PROG (PAIR)
        (SETQ PAIR RST)
       LAB
        (COND ((NULL PAIR) (RETURN NIL)))
        ((LAMBDA (PAIR)
           (PROGN
            (SETQ TRANSL (TALP_GETTRANSL (CAR PAIR) PW2V NIL))
            (PROG (ELEM)
              (SETQ ELEM TRANSL)
             LAB
              (COND ((NULL ELEM) (RETURN NIL)))
              ((LAMBDA (ELEM)
                 (PROGN
                  (SETQ NEWSUBL (CONS (CONS ELEM (CDR PAIR)) NEWSUBL))
                  (SETQ NEWEQL
                          (CONS (TALP_SIMPAT (LIST 'EQUAL ELEM (CDR PAIR)))
                                NEWEQL))))
               (CAR ELEM))
              (SETQ ELEM (CDR ELEM))
              (GO LAB))))
         (CAR PAIR))
        (SETQ PAIR (CDR PAIR))
        (GO LAB))
      (RETURN (CONS NEWSUBL NEWEQL)))) 
(PUT 'TALP_GETTRANSL 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_GETTRANSL 'DEFINED-ON-LINE '307) 
(PUT 'TALP_GETTRANSL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_GETTRANSL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_GETTRANSL (VAR PL RESULT)
    (PROG (VARL NEWPL)
      (PROG (X)
        (SETQ X PL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((EQ (CAR X) VAR) (SETQ VARL (CONS X VARL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X VARL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((AND (NEQ (CDR X) VAR) (NOT (MEMQ (CDR X) RESULT)))
             (SETQ RESULT (CONS (CDR X) RESULT)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (VARL
        (PROG (X)
          (SETQ X PL)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (COND ((NOT (TALP_CTNS X VARL)) (SETQ NEWPL (CONS X NEWPL)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))))
      (RETURN
       (COND (NEWPL (TALP_GETTRANSL (CAAR NEWPL) NEWPL RESULT)) (T RESULT))))) 
(PUT 'TALP_SUMD 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_SUMD 'DEFINED-ON-LINE '326) 
(PUT 'TALP_SUMD 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_SUMD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_SUMD (F)
    (PROG (TMP SD)
      (SETQ SD 0)
      (COND ((ATOM F) (RETURN 0)))
      (SETQ TMP (RL_ATL F))
      (PROG ()
       WHILELABEL
        (COND ((NOT TMP) (RETURN NIL)))
        (PROGN
         (SETQ SD
                 (PLUS SD (TALP_TD (CADR (CAR TMP)))
                       (TALP_TD (CADDR (CAR TMP)))))
         (SETQ TMP (CDR TMP))
         NIL)
        (GO WHILELABEL))
      (RETURN SD))) 
(PUT 'TALP_CTNS 'NUMBER-OF-ARGS 2) 
(PUT 'TALP_CTNS 'DEFINED-ON-LINE '339) 
(PUT 'TALP_CTNS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_CTNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TALP_CTNS (PAIR PAIRL)
    (PROG (FOUND)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PAIRL (NOT FOUND))) (RETURN NIL)))
        (COND
         ((AND (EQ (CAAR PAIRL) (CAR PAIR)) (EQ (CDAR PAIRL) (CDR PAIR)))
          (SETQ FOUND T))
         (T (SETQ PAIRL (CDR PAIRL))))
        (GO WHILELABEL))
      (RETURN FOUND))) 
(FLAG '(TALP_LSSIMPL) 'OPFN) 
(PUT 'TALP_LSSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_LSSIMPL 'DEFINED-ON-LINE '353) 
(PUT 'TALP_LSSIMPL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_LSSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_LSSIMPL (F)
    (PROG (OP)
      (SETQ F (TALP_RNF F))
      (COND ((OR (ATOM F) (TALP_ATFP F)) (RETURN F)))
      (SETQ OP (CAR F))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (RETURN
         (TALP_RNF
          (TALP_LSSIMPL1
           (CONS OP
                 (PROG (SF FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SF (CDR F))
                   (COND ((NULL SF) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (SF) (TALP_LSSIMPL SF)) (CAR SF))
                                    NIL)))
                  LOOPLABEL
                   (SETQ SF (CDR SF))
                   (COND ((NULL SF) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (SF) (TALP_LSSIMPL SF)) (CAR SF))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))
       ((MEMQ OP '(EX ALL))
        (RETURN
         (TALP_RNF
          (CONS OP (CONS (CADR F) (LIST (TALP_LSSIMPL (CADDR F))))))))))) 
(PUT 'TALP_LSSIMPL1 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_LSSIMPL1 'DEFINED-ON-LINE '368) 
(PUT 'TALP_LSSIMPL1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_LSSIMPL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_LSSIMPL1 (F)
    (PROG (TMP TMP2 KNOWL OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (PROG (SUBF)
        (SETQ SUBF F)
       LAB
        (COND ((NULL SUBF) (RETURN NIL)))
        ((LAMBDA (SUBF)
           (COND
            ((TALP_ATFP SUBF)
             (COND ((TALP_CANDP SUBF) (SETQ KNOWL (CONS SUBF KNOWL)))))))
         (CAR SUBF))
        (SETQ SUBF (CDR SUBF))
        (GO LAB))
      (SETQ TMP
              (CONS (CAR F)
                    (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                      (SETQ SUBF (CDR F))
                      (COND ((NULL SUBF) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (SUBF)
                                          (PROGN
                                           (COND
                                            ((OR (EQ SUBF 'TRUE)
                                                 (EQ SUBF 'FALSE))
                                             SUBF)
                                            ((TALP_ATFP SUBF)
                                             (COND
                                              ((TALP_CANDP SUBF)
                                               (TALP_TCANDT SUBF KNOWL OP))
                                              (T SUBF)))
                                            (T
                                             (PROGN
                                              (SETQ TMP2 (TALP_LSSIMPL1 SUBF))
                                              (COND
                                               ((TALP_ATFP TMP2)
                                                (COND
                                                 ((TALP_CANDP TMP2)
                                                  (PROGN
                                                   (SETQ KNOWL
                                                           (CONS TMP2 KNOWL))
                                                   (TALP_TCANDT TMP2 KNOWL
                                                    OP)))
                                                 (T TMP2)))
                                               (T TMP2)))))))
                                        (CAR SUBF))
                                       NIL)))
                     LOOPLABEL
                      (SETQ SUBF (CDR SUBF))
                      (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (SUBF)
                                  (PROGN
                                   (COND
                                    ((OR (EQ SUBF 'TRUE) (EQ SUBF 'FALSE))
                                     SUBF)
                                    ((TALP_ATFP SUBF)
                                     (COND
                                      ((TALP_CANDP SUBF)
                                       (TALP_TCANDT SUBF KNOWL OP))
                                      (T SUBF)))
                                    (T
                                     (PROGN
                                      (SETQ TMP2 (TALP_LSSIMPL1 SUBF))
                                      (COND
                                       ((TALP_ATFP TMP2)
                                        (COND
                                         ((TALP_CANDP TMP2)
                                          (PROGN
                                           (SETQ KNOWL (CONS TMP2 KNOWL))
                                           (TALP_TCANDT TMP2 KNOWL OP)))
                                         (T TMP2)))
                                       (T TMP2)))))))
                                (CAR SUBF))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN (TALP_RNF TMP)))) 
(PUT 'TALP_CANDP 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_CANDP 'DEFINED-ON-LINE '396) 
(PUT 'TALP_CANDP 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_CANDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_CANDP (ATF)
    (PROG (LHS RHS)
      (SETQ LHS (CADR ATF))
      (SETQ RHS (CADDR ATF))
      (COND ((AND (ATOM LHS) (ATOM RHS)) (RETURN NIL)))
      (COND
       ((ATOM LHS)
        (COND ((NULL (TALP_INVP RHS)) (RETURN NIL))
              (T (COND ((NEQ LHS (CADR RHS)) (RETURN NIL))))))
       ((ATOM RHS)
        (COND ((NULL (TALP_INVP LHS)) (RETURN NIL))
              (T (COND ((NEQ RHS (CADR LHS)) (RETURN NIL))))))
       (T (RETURN NIL)))
      (RETURN 'TRUE))) 
(PUT 'TALP_TCANDT 'NUMBER-OF-ARGS 3) 
(PUT 'TALP_TCANDT 'DEFINED-ON-LINE '417) 
(PUT 'TALP_TCANDT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TCANDT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TCANDT (CAND KNOWL OP)
    (COND ((NULL KNOWL) CAND) (T (TALP_TCANDT1 CAND (CAR CAND) KNOWL OP)))) 
(PUT 'TALP_TCANDT1 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_TCANDT1 'DEFINED-ON-LINE '425) 
(PUT 'TALP_TCANDT1 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TCANDT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TCANDT1 (CAND COP KNOWL OP)
    (PROG (INVT TVAR CFS RESULT SCOP)
      (SETQ INVT
              (COND
               ((ATOM (CADR CAND))
                (PROGN (SETQ TVAR (CADR CAND)) (CADDR CAND)))
               (T (PROGN (SETQ TVAR (CADDR CAND)) (CADR CAND)))))
      (SETQ SCOP (COND ((EQ COP 'EQUAL) 'NEQ) (T 'EQUAL)))
      (SETQ CFS (TALP_INVF INVT))
      (COND
       ((EQ OP 'AND)
        (COND
         ((EQ COP 'EQUAL)
          (COND
           ((TALP_TESTKNOWL INVT TVAR COP KNOWL)
            (SETQ RESULT
                    ((LAMBDA (G124)
                       (COND ((AND G124 (CDR G124)) (CONS 'OR G124))
                             ((NULL G124)
                              (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                             (T (CAR G124))))
                     (PROG (C FORALL-RESULT FORALL-ENDPTR)
                       (SETQ C (TALP_GETCTS))
                       (COND ((NULL C) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (C)
                                           (TALP_SIMPAT (LIST 'EQUAL TVAR C)))
                                         (CAR C))
                                        NIL)))
                      LOOPLABEL
                       (SETQ C (CDR C))
                       (COND ((NULL C) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (C)
                                   (TALP_SIMPAT (LIST 'EQUAL TVAR C)))
                                 (CAR C))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
           (T (SETQ RESULT CAND))))
         (T (SETQ RESULT CAND))))
       ((TALP_TESTKNOWL INVT TVAR COP KNOWL)
        (SETQ RESULT
                ((LAMBDA (G126)
                   (COND ((AND G126 (CDR G126)) (CONS 'AND G126))
                         ((NULL G126) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                         (T (CAR G126))))
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C (TALP_GETCTS))
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (TALP_SIMPAT (LIST 'NEQ TVAR C)))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C) (TALP_SIMPAT (LIST 'NEQ TVAR C)))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T (SETQ RESULT CAND)))
      (RETURN (COND (RESULT RESULT) (T CAND))))) 
(PUT 'TALP_TESTKNOWL 'NUMBER-OF-ARGS 4) 
(PUT 'TALP_TESTKNOWL 'DEFINED-ON-LINE '452) 
(PUT 'TALP_TESTKNOWL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_TESTKNOWL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TALP_TESTKNOWL (TERM VAR ATOP KNOWL)
    (PROG (TMP INVFS ATF INVT)
      (SETQ TMP KNOWL)
      (SETQ INVFS (TALP_GETINVFTS))
      (SETQ INVFS (DELETE (CAR TERM) INVFS))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMP INVFS)) (RETURN NIL)))
        (PROGN
         (SETQ ATF (CAR TMP))
         (COND
          ((TALP_CANDP ATF)
           (PROGN
            (SETQ INVT
                    (COND ((TALP_INVP (CADR ATF)) (CADR ATF)) (T (CADDR ATF))))
            (COND
             ((AND (EQ ATOP (CAR ATF)) (EQ (CADR INVT) VAR))
              (SETQ INVFS (DELETE (CAR INVT) INVFS)))))))
         (SETQ TMP (CDR TMP)))
        (GO WHILELABEL))
      (COND ((NULL INVFS) (RETURN 'TRUE)))
      (RETURN NIL))) 
(PUT 'TALP_GETINVFTS 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_GETINVFTS 'DEFINED-ON-LINE '476) 
(PUT 'TALP_GETINVFTS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_GETINVFTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_GETINVFTS NIL
    (PROG (TMP INVFSET)
      (SETQ TMP (SETDIFF TALP_EXTLANG* TALP_LANG*))
      (SETQ INVFSET
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TMP)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND
                            ((EQ (TALP_GETINVN (CAR X)) 1) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (COND
                            ((EQ (TALP_GETINVN (CAR X)) 1) (LIST (CAR X)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN INVFSET))) 
(PUT 'TALP_GETINVN 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_GETINVN 'DEFINED-ON-LINE '486) 
(PUT 'TALP_GETINVN 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_GETINVN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_GETINVN (FSYM) (COMPRESS (LIST (CADR (CDDDDR (EXPLODE2 FSYM)))))) 
(FLAG '(TALP_INVTSCSIMPL) 'OPFN) 
(PUT 'TALP_INVTSCSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_INVTSCSIMPL 'DEFINED-ON-LINE '494) 
(PUT 'TALP_INVTSCSIMPL 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_INVTSCSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_INVTSCSIMPL (F)
    (PROG (OP)
      (SETQ F (TALP_RNF F))
      (COND ((ATOM F) (RETURN F)))
      (COND
       ((TALP_ATFP F)
        (COND
         ((TALP_INVTSCC F)
          (RETURN (TALP_RNF (TALP_INVTSCSIMPLAT (TALP_SIMPAT F)))))
         (T (RETURN (TALP_SIMPAT F))))))
      (SETQ OP (CAR F))
      (COND
       ((OR (EQ OP 'OR) (EQ OP 'AND))
        (RETURN
         (TALP_RNF
          (CONS OP
                (PROG (SF FORALL-RESULT FORALL-ENDPTR)
                  (SETQ SF (CDR F))
                  (COND ((NULL SF) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (SF) (TALP_INVTSCSIMPL SF))
                                    (CAR SF))
                                   NIL)))
                 LOOPLABEL
                  (SETQ SF (CDR SF))
                  (COND ((NULL SF) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (SF) (TALP_INVTSCSIMPL SF)) (CAR SF))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))))
       ((MEMQ OP '(EX ALL))
        (RETURN
         (TALP_RNF
          (CONS OP (CONS (CADR F) (LIST (TALP_INVTSCSIMPL (CADDR F))))))))))) 
(PUT 'TALP_INVTSCSIMPLAT 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_INVTSCSIMPLAT 'DEFINED-ON-LINE '514) 
(PUT 'TALP_INVTSCSIMPLAT 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_INVTSCSIMPLAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_INVTSCSIMPLAT (ATF)
    (PROG (LEN RES OP FCTSYML VAR FCTSYM NEXTFCTSYM INVT CANDIDATE PURE)
      (SETQ CANDIDATE (TALP_INVTSCC ATF))
      (COND ((NOT CANDIDATE) (RETURN ATF)))
      (SETQ INVT (CDR CANDIDATE))
      (SETQ VAR (CAR CANDIDATE))
      (SETQ OP (CAR ATF))
      (SETQ PURE T)
      (SETQ FCTSYM (TALP_INVF INVT))
      (SETQ FCTSYML (CONS FCTSYM FCTSYML))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ATOM (CADR INVT)))) (RETURN NIL)))
        (PROGN
         (SETQ INVT (CADR INVT))
         (SETQ NEXTFCTSYM (TALP_INVF INVT))
         (COND
          ((NOT (MEMQ NEXTFCTSYM FCTSYML))
           (PROGN (SETQ FCTSYML (CONS NEXTFCTSYM FCTSYML)) (SETQ PURE NIL)))))
        (GO WHILELABEL))
      (SETQ LEN (LENGTH FCTSYML))
      (COND
       ((AND PURE (GREATERP LEN 1))
        (RETURN
         (TALP_SIMPAT
          (LIST OP (CONS (TALP_GETINVFSYM FCTSYM 1) (LIST VAR)) VAR)))))
      (SETQ RES
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ FCTSYM (CAR FCTSYML))
                                  (SETQ FCTSYML (CDR FCTSYML))
                                  (TALP_SIMPAT
                                   (LIST OP
                                         (CONS (TALP_GETINVFSYM FCTSYM 1)
                                               (LIST VAR))
                                         VAR)))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ FCTSYM (CAR FCTSYML))
                          (SETQ FCTSYML (CDR FCTSYML))
                          (TALP_SIMPAT
                           (LIST OP
                                 (CONS (TALP_GETINVFSYM FCTSYM 1) (LIST VAR))
                                 VAR)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (COND
        ((EQ (TALP_NOFFCTS) (LENGTH RES))
         (CONS (COND ((EQ OP 'EQUAL) 'OR) (T 'AND))
               (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ELEM (TALP_GETCTS))
                 (COND ((NULL ELEM) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELEM)
                                     (TALP_SIMPAT (LIST OP VAR ELEM)))
                                   (CAR ELEM))
                                  NIL)))
                LOOPLABEL
                 (SETQ ELEM (CDR ELEM))
                 (COND ((NULL ELEM) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ELEM) (TALP_SIMPAT (LIST OP VAR ELEM)))
                           (CAR ELEM))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        ((EQ OP 'EQUAL) (CONS 'AND RES)) (T (CONS 'OR RES)))))) 
(PUT 'TALP_INVTSCC 'NUMBER-OF-ARGS 1) 
(PUT 'TALP_INVTSCC 'DEFINED-ON-LINE '553) 
(PUT 'TALP_INVTSCC 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_INVTSCC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TALP_INVTSCC (ATF)
    (PROG (VAR INVT TMP NOF)
      (COND ((ATOM (CADR ATF)) (SETQ VAR (CADR ATF)))
            ((ATOM (CADDR ATF)) (SETQ VAR (CADDR ATF))) (T (RETURN NIL)))
      (SETQ INVT (COND ((ATOM (CADR ATF)) (CADDR ATF)) (T (CADR ATF))))
      (SETQ NOF (TALP_NOFFCTS))
      (COND
       ((OR (NOT (TALP_INVP INVT))
            (NOT (OR (GREATERP (TALP_TD INVT) 1) (EQUAL NOF 1))))
        (RETURN NIL)))
      (SETQ TMP INVT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ATOM (CADR TMP)))) (RETURN NIL)))
        (SETQ TMP (CADR TMP))
        (GO WHILELABEL))
      (COND ((NEQ VAR (CADR TMP)) (RETURN NIL)))
      (RETURN (CONS VAR INVT)))) 
(PUT 'TALP_NOFFCTS 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_NOFFCTS 'DEFINED-ON-LINE '576) 
(PUT 'TALP_NOFFCTS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_NOFFCTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_NOFFCTS NIL
    (PROG (NOF)
      (SETQ NOF 0)
      (PROG (X)
        (SETQ X TALP_LANG*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((GREATERP (CDR X) 0) (SETQ NOF (PLUS NOF 1)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN NOF))) 
(PUT 'TALP_GETCTS 'NUMBER-OF-ARGS 0) 
(PUT 'TALP_GETCTS 'DEFINED-ON-LINE '585) 
(PUT 'TALP_GETCTS 'DEFINED-IN-FILE 'REDLOG/TALP/TALPSISM.RED) 
(PUT 'TALP_GETCTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TALP_GETCTS NIL
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X TALP_LANG*)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X) (COND ((EQ (CDR X) 0) (LIST (CAR X))))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X) (COND ((EQ (CDR X) 0) (LIST (CAR X))))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(ENDMODULE) 