(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFQE)) 
(REVISION 'OFSFQE "$Id: ofsfqe.red 6618 2023-10-06 06:18:51Z thomas-sturm $") 
(COPYRIGHT 'OFSFQE
           "(c) 1995-2009 A. Dolzmann, T. Sturm, 2010-2016 T. Sturm, 2017 M. Kosta, T. Sturm") 
(PUT 'OFSF_POSQE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_POSQE 'DEFINED-ON-LINE '35) 
(PUT 'OFSF_POSQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POSQE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_POSQE (F THEO)
    (PROG (*RLPOS POSCONDS RES)
      (SETQ *RLPOS T)
      (SETQ POSCONDS (OFSF_POSCONDS (CL_FVARL F) NIL))
      (SETQ RES (OFSF_QE (OFSF_POSQE-PREP F) (NCONC POSCONDS THEO)))
      (RETURN
       (CL_SIMPL
        ((LAMBDA (G166)
           (COND ((AND G166 (CDR G166)) (CONS 'AND G166))
                 ((NULL G166) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR G166))))
         (CONS RES POSCONDS))
        NIL (MINUS 1))))) 
(PUT 'OFSF_POSQEA 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_POSQEA 'DEFINED-ON-LINE '43) 
(PUT 'OFSF_POSQEA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POSQEA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_POSQEA (F THEO)
    (PROG (*RLPOS)
      (SETQ *RLPOS T)
      (RETURN (OFSF_QEA (OFSF_POSQE-PREP F) THEO)))) 
(PUT 'OFSF_POSGQE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_POSGQE 'DEFINED-ON-LINE '49) 
(PUT 'OFSF_POSGQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POSGQE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_POSGQE (F THEO XVL)
    (PROG (*RLPOS POSCONDS RES)
      (SETQ *RLPOS T)
      (SETQ POSCONDS (OFSF_POSCONDS (CL_FVARL F) NIL))
      (SETQ RES (CL_GQE (OFSF_POSQE-PREP F) (NCONC POSCONDS THEO) XVL))
      (RETURN
       (CONS (CAR RES)
             (CL_SIMPL
              ((LAMBDA (G168)
                 (COND ((AND G168 (CDR G168)) (CONS 'AND G168))
                       ((NULL G168) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G168))))
               (CONS (CDR RES) POSCONDS))
              NIL (MINUS 1)))))) 
(PUT 'OFSF_POSGQEA 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_POSGQEA 'DEFINED-ON-LINE '57) 
(PUT 'OFSF_POSGQEA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POSGQEA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_POSGQEA (F THEO XVL)
    (PROG (*RLPOS)
      (SETQ *RLPOS T)
      (RETURN (CL_GQEA (OFSF_POSQE-PREP F) THEO XVL)))) 
(PUT 'OFSF_POSQE-PREP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_POSQE-PREP 'DEFINED-ON-LINE '63) 
(PUT 'OFSF_POSQE-PREP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POSQE-PREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_POSQE-PREP (F) (OFSF_POSPREP F NIL)) 
(PUT 'OFSF_QE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QE 'DEFINED-ON-LINE '66) 
(PUT 'OFSF_QE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QE (F THEO)
    (COND
     ((AND *RLXOPT (NULL THEO) (OFSF_XOPT-CHECK F))
      (PROGN
       (COND (*RLVERBOSE (IOTO_TPRIN2T "++++ Entering xopt-qe")))
       (OFSF_XOPT-QE F)))
     (T
      (PROGN
       (COND (*RLVERBOSE (IOTO_TPRIN2T "++++ Entering cl_qe")))
       (CL_QE F THEO))))) 
(PUT 'OFSF_QEA 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QEA 'DEFINED-ON-LINE '77) 
(PUT 'OFSF_QEA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEA (F THEO)
    (COND
     ((AND *RLXOPT (NULL THEO) (OFSF_XOPT-CHECK F))
      (PROGN
       (COND (*RLVERBOSE (IOTO_TPRIN2T "++++ Entering xopt-qea")))
       (OFSF_XOPT-QEA F)))
     (T
      (PROGN
       (COND (*RLVERBOSE (IOTO_TPRIN2T "++++ Entering cl_qea")))
       (CL_QEA F THEO))))) 
(PUT 'OFSF_LQE 'NUMBER-OF-ARGS 3) 
(DE OFSF_LQE (F THEO PT)
    (PROG (W ER *RLQELOCAL *RLQEVARSELTRY *RLSIPW *RLSIPO CL_PAL* CL_LPS*
           CL_THEO* *CLQENEW)
      (SETQ *RLQELOCAL T)
      (SETQ *RLQEVARSELTRY NIL)
      (SETQ *RLSIPW T)
      (SETQ *RLSIPO NIL)
      (SETQ CL_PAL* PT)
      (SETQ CL_LPS*
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X PT)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CL_THEO* NIL)
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X THEO)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (RL_SUBAT CL_PAL* X)) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (RL_SUBAT CL_PAL* X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (RL_SIMPL
               (COND ((AND W (CDR W)) (CONS 'AND W))
                     ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                     (T (CAR W)))
               NIL (MINUS 1)))
      (COND ((EQ W 'FALSE) (REDERR "rllqe: inconsistent theory")))
      (SETQ ER (CL_QE1 F THEO NIL))
      (SETQ THEO (NCONC CL_THEO* THEO))
      (SETQ CL_PAL* (SETQ CL_LPS* (SETQ CL_THEO* NIL)))
      (COND ((RL_EXCEPTIONP ER) (RETURN ER)))
      (RETURN
       (CONS (RL_LTHSIMPL THEO) (RL_SIMPL (CAAR (CDR ER)) THEO (MINUS 1)))))) 
(PUT 'OFSF_VARSEL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_VARSEL 'DEFINED-ON-LINE '125) 
(PUT 'OFSF_VARSEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_VARSEL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_VARSEL (F VL THEO)
    (COND (*RLQEVARSELTRY (OFSF_VARSEL-TRY F VL THEO))
          (T (LIST (OFSF_VARSEL-CLASSIC F VL THEO))))) 
(PUT 'OFSF_VARSEL-TRY 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_VARSEL-TRY 'DEFINED-ON-LINE '134) 
(PUT 'OFSF_VARSEL-TRY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_VARSEL-TRY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_VARSEL-TRY (F VL THEO)
    (PROG (ATL CANDVL IFACL TERML)
      (SETQ ATL (CL_ATL1 F))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A)
                           (COND ((OFSF_LINP ATL A (LTO_DELQ A VL)) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A)
                           (COND ((OFSF_LINP ATL A (LTO_DELQ A VL)) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A) (COND ((OFSF_QSCP ATL A) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A) (COND ((OFSF_QSCP ATL A) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (SETQ TERML
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X ATL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP TERML A 1) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP TERML A 1) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP TERML A 2) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP TERML A 2) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 "(SVF")))
      (SETQ IFACL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X ATL)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (PROG (P FORALL-RESULT FORALL-ENDPTR)
                             (SETQ P (CDR (SFTO_FCTRF (CADR X))))
                             (COND ((NULL P) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P) (CAR P)) (CAR P))
                                              NIL)))
                            LOOPLABEL
                             (SETQ P (CDR P))
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (PROG (P FORALL-RESULT FORALL-ENDPTR)
                             (SETQ P (CDR (SFTO_FCTRF (CADR X))))
                             (COND ((NULL P) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P) (CAR P)) (CAR P))
                                              NIL)))
                            LOOPLABEL
                             (SETQ P (CDR P))
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 ")")))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP IFACL A 1) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP IFACL A 1) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (SETQ CANDVL
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A VL)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP IFACL A 2) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A) (COND ((OFSF_PSEUDP IFACL A 2) (LIST A))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (COND (CANDVL (RETURN CANDVL)))
      (RETURN VL))) 
(PUT 'OFSF_VARSEL-CLASSIC 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_VARSEL-CLASSIC 'DEFINED-ON-LINE '168) 
(PUT 'OFSF_VARSEL-CLASSIC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_VARSEL-CLASSIC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_VARSEL-CLASSIC (F VL THEO)
    (PROG (V A SCVL ATL IFACL TERML)
      (SETQ ATL (CL_ATL1 F))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_LINP ATL A (LTO_DELQ A VL)) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_QSCP ATL A) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (SETQ TERML
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X ATL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_PSEUDP TERML A 1) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_PSEUDP TERML A 2) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 "(SVF")))
      (SETQ IFACL
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X ATL)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (PROG (P FORALL-RESULT FORALL-ENDPTR)
                             (SETQ P (CDR (SFTO_FCTRF (CADR X))))
                             (COND ((NULL P) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P) (CAR P)) (CAR P))
                                              NIL)))
                            LOOPLABEL
                             (SETQ P (CDR P))
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X)
                           (PROG (P FORALL-RESULT FORALL-ENDPTR)
                             (SETQ P (CDR (SFTO_FCTRF (CADR X))))
                             (COND ((NULL P) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P) (CAR P)) (CAR P))
                                              NIL)))
                            LOOPLABEL
                             (SETQ P (CDR P))
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 ")")))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_PSEUDP IFACL A 1) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (SETQ SCVL VL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT V))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (COND ((OFSF_PSEUDP IFACL A 2) (SETQ V A))))
        (GO WHILELABEL))
      (COND (V (RETURN V)))
      (RETURN (CAR VL)))) 
(PUT 'OFSF_LINP 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_LINP 'DEFINED-ON-LINE '226) 
(PUT 'OFSF_LINP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_LINP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_LINP (ATL V VL)
    (PROG (LINP W U G)
      (SETQ LINP T)
      (SETQ W (SETKORDER (LIST V)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ATL LINP)) (RETURN NIL)))
        (PROGN
         (SETQ U (REORDER (CADR (CAR ATL))))
         (SETQ ATL (CDR ATL))
         (SETQ G (DEGR U V))
         (COND
          ((OR (GREATERP G 1)
               (AND (EQUAL G 1) (INTERSECTION (KERNELS (CDAR U)) VL)))
           (SETQ LINP NIL))))
        (GO WHILELABEL))
      (SETKORDER W)
      (RETURN LINP))) 
(PUT 'OFSF_QSCP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QSCP 'DEFINED-ON-LINE '247) 
(PUT 'OFSF_QSCP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QSCP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QSCP (ATL V)
    (PROG (A HIT D)
      (COND ((NOT *RLQEQSC) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR ATL))
         (SETQ ATL (CDR ATL))
         (SETQ D (DEGREEF (CADR A) V))
         (COND ((GREATERP D 2) (SETQ ATL (SETQ HIT NIL)))
               ((AND (EQUAL D 2) (MEMQ (CAR A) '(GREATERP LESSP GEQ LEQ NEQ)))
                (COND (HIT (SETQ ATL (SETQ HIT NIL))) (T (SETQ HIT T))))))
        (GO WHILELABEL))
      (RETURN HIT))) 
(PUT 'OFSF_PSEUDP 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PSEUDP 'DEFINED-ON-LINE '270) 
(PUT 'OFSF_PSEUDP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_PSEUDP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PSEUDP (IFACL V N)
    (PROG (OK)
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND IFACL OK)) (RETURN NIL)))
        (COND ((GREATERP (DEGREEF (CAR IFACL) V) N) (SETQ OK NIL))
              (T (SETQ IFACL (CDR IFACL))))
        (GO WHILELABEL))
      (RETURN OK))) 
(PUT 'OFSF_QESUBCR1 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCR1 'DEFINED-ON-LINE '292) 
(PUT 'OFSF_QESUBCR1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCR1 (BVL THEO F V CO U)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBR F V U))))))) 
(PUT 'OFSF_QESUBCR2 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_QESUBCR2 'DEFINED-ON-LINE '308) 
(PUT 'OFSF_QESUBCR2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCR2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_QESUBCR2 (BVL THEO F V CO U1 U2)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN
       (CONS (CAR W)
             (CONS 'AND
                   (LIST (CDR W)
                         (CONS 'OR
                               (LIST (OFSF_QESUBR F V U1)
                                     (OFSF_QESUBR F V U2))))))))) 
(PUT 'OFSF_QESUBR 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBR 'DEFINED-ON-LINE '326) 
(PUT 'OFSF_QESUBR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBR (F V U)
    (COND
     ((EQUAL (CADDR U) 1)
      (CL_APPLY2ATS1 F 'OFSF_QESUBQAT
                     (LIST V
                           (MULTSQ (CONS (ADDF (CAR U) (CADR U)) 1)
                                   (INVSQ (CONS (CADDDR U) 1))))))
     (T (CL_APPLY2ATS1 F 'OFSF_QESUBRAT (LIST V U))))) 
(PUT 'OFSF_QESUBRAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBRAT 'DEFINED-ON-LINE '337) 
(PUT 'OFSF_QESUBRAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRAT (ATF V U)
    (COND ((NOT (MEMQ V (OFSF_VARLAT ATF))) ATF)
          (T (OFSF_QESUBRAT1 (CAR ATF) (CADR ATF) V U)))) 
(PUT 'OFSF_QESUBRAT1 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QESUBRAT1 'DEFINED-ON-LINE '348) 
(PUT 'OFSF_QESUBRAT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRAT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRAT1 (R F X RFORM)
    (PROG (W DD)
      (SETQ W (OFSF_GETSUBRCOEFFS F X RFORM))
      (COND
       ((OR (EQ R 'EQUAL) (EQ R 'NEQ))
        (RETURN (OFSF_QESUBREQ R (CAR W) (CADR W) (CADDR W)))))
      (SETQ DD (CADDDR W))
      (COND
       ((OFSF_SUREP (LIST 'GEQ DD NIL) NIL)
        (RETURN (OFSF_QESUBRORD R (CAR W) (CADR W) (CADDR W) 1))))
      (SETQ DD (CAR (SFTO_PDECF DD)))
      (COND
       ((OFSF_SUREP (LIST 'GEQ DD NIL) NIL)
        (RETURN (OFSF_QESUBRORD R (CAR W) (CADR W) (CADDR W) 1))))
      (RETURN (OFSF_QESUBRORD R (CAR W) (CADR W) (CADDR W) DD)))) 
(PUT 'OFSF_QESUBREQ 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QESUBREQ 'DEFINED-ON-LINE '367) 
(PUT 'OFSF_QESUBREQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBREQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBREQ (R AA BB C)
    ((LAMBDA (W) (COND ((EQ R 'EQUAL) W) (T (CL_NNFNOT W))))
     (OFSF_QESUBREQ1 AA BB C))) 
(PUT 'OFSF_QESUBREQ1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBREQ1 'DEFINED-ON-LINE '376) 
(PUT 'OFSF_QESUBREQ1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBREQ1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBREQ1 (AA BB C)
    (COND ((NULL BB) (LIST 'EQUAL AA NIL))
          (T
           (CONS 'AND
                 (LIST
                  (LIST 'LEQ
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF AA BB))
                              (T (POLY-MULTF AA BB)))
                        NIL)
                  (LIST 'EQUAL
                        (ADDF (EXPTF AA 2)
                              (NEGF
                               ((LAMBDA (G169)
                                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G169 C))
                                        (T (POLY-MULTF G169 C))))
                                (EXPTF BB 2))))
                        NIL)))))) 
(PUT 'OFSF_QESUBRORD 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QESUBRORD 'DEFINED-ON-LINE '387) 
(PUT 'OFSF_QESUBRORD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRORD (R AA BB C DD)
    (COND ((OR (EQ R 'LEQ) (EQ R 'LESSP)) (OFSF_QESUBRORD1 R AA BB C DD))
          (T (CL_NNFNOT (OFSF_QESUBRORD1 (OFSF_LNEGREL R) AA BB C DD))))) 
(PUT 'OFSF_QESUBRORD1 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QESUBRORD1 'DEFINED-ON-LINE '399) 
(PUT 'OFSF_QESUBRORD1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRORD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRORD1 (R AA BB C DD)
    (PROG (AD A2B2C W)
      (SETQ AD
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF AA DD))
                    (T (POLY-MULTF AA DD))))
      (COND ((NULL BB) (RETURN (LIST R AD NIL))))
      (SETQ A2B2C
              (ADDF (EXPTF AA 2)
                    (NEGF
                     ((LAMBDA (G171)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G171 C))
                              (T (POLY-MULTF G171 C))))
                      (EXPTF BB 2)))))
      (SETQ W
              (COND ((EQ R 'LEQ) (LIST 'LEQ A2B2C NIL))
                    (T
                     (CONS 'OR
                           (LIST (LIST 'LESSP AD NIL)
                                 (LIST 'LESSP A2B2C NIL))))))
      (RETURN
       (CONS 'OR
             (LIST
              (CONS 'AND
                    (LIST (LIST R AD NIL) (LIST (OFSF_ANEGREL R) A2B2C NIL)))
              (CONS 'AND
                    (LIST
                     (LIST 'LEQ
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF BB DD))
                                 (T (POLY-MULTF BB DD)))
                           NIL)
                     W))))))) 
(PUT 'OFSF_GETSUBRCOEFFS 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_GETSUBRCOEFFS 'DEFINED-ON-LINE '420) 
(PUT 'OFSF_GETSUBRCOEFFS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_GETSUBRCOEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_GETSUBRCOEFFS (F X RFORM)
    (PROG (W RPOL AA BB DD A B C D)
      (SETQ A (PREPF (CAR RFORM)))
      (SETQ B (PREPF (CADR RFORM)))
      (SETQ C (CADDR RFORM))
      (SETQ D (PREPF (CADDDR RFORM)))
      (SETQ RPOL (LIST 'QUOTIENT (LIST 'PLUS A (LIST 'TIMES B 'OFSF_SQRT)) D))
      (SETQ W (SUBF F (LIST (CONS X RPOL))))
      (SETQ DD (CDR W))
      (SETQ W (SFTO_REORDER (CAR W) 'OFSF_SQRT))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM W) (ATOM (CAR W)))) (EQ (CAAAR W) 'OFSF_SQRT)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((EVENP (CDAAR W))
           (SETQ AA
                   (ADDF AA
                         ((LAMBDA (G173 G174)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G173 G174))
                                  (T (POLY-MULTF G173 G174))))
                          (REORDER (CDAR W))
                          (EXPTF C (QUOTIENT (CDAAR W) 2))))))
          (T
           (SETQ BB
                   (ADDF BB
                         ((LAMBDA (G175 G176)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G175 G176))
                                  (T (POLY-MULTF G175 G176))))
                          (REORDER (CDAR W))
                          (EXPTF C (QUOTIENT (CDAAR W) 2)))))))
         (SETQ W (CDR W)))
        (GO WHILELABEL))
      (SETQ AA (ADDF AA (REORDER W)))
      (RETURN (LIST AA BB C DD)))) 
(PUT 'OFSF_QESUBCQ 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCQ 'DEFINED-ON-LINE '446) 
(PUT 'OFSF_QESUBCQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCQ (BVL THEO F V CO U)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBQ F V U))))))) 
(PUT 'OFSF_QESUBQ 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBQ 'DEFINED-ON-LINE '462) 
(PUT 'OFSF_QESUBQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBQ (F V U) (CL_APPLY2ATS1 F 'OFSF_QESUBQAT (LIST V U))) 
(PUT 'OFSF_QESUBQAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBQAT 'DEFINED-ON-LINE '469) 
(PUT 'OFSF_QESUBQAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBQAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBQAT (ATF V U)
    (PROG (W OP DD)
      (COND ((NOT (MEMQ V (OFSF_VARLAT ATF))) (RETURN ATF)))
      (SETQ W
              (COND (*RLQESUBF (SUBF (CADR ATF) (LIST (CONS V (PREPSQ U)))))
                    (T (OFSF_SUBF (CADR ATF) V U))))
      (SETQ OP (CAR ATF))
      (COND (*RLQELOCAL (RETURN (OFSF_QESUBQAT-LOCAL OP W))))
      (COND
       ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ)
            (OFSF_SUREP (LIST 'GEQ (CDR W) NIL) NIL))
        (RETURN (LIST OP (CAR W) NIL))))
      (SETQ DD (CAR (SFTO_PDECF (CDR W))))
      (COND
       ((OFSF_SUREP (LIST 'GEQ DD NIL) NIL) (RETURN (LIST OP (CAR W) NIL))))
      (RETURN
       (LIST OP
             (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR W) DD))
                   (T (POLY-MULTF (CAR W) DD)))
             NIL)))) 
(PUT 'OFSF_SUBF 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SUBF 'DEFINED-ON-LINE '494) 
(PUT 'OFSF_SUBF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUBF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBF (F V U)
    (PROG (NRED)
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RETURN (CONS F 1))))
      (SETQ NRED (OFSF_SUBF (CDR F) V U))
      (COND
       ((EQ (CAAAR F) V)
        (RETURN (ADDSQ (MULTSQ (CONS (CDAR F) 1) (EXPTSQ U (CDAAR F))) NRED))))
      (RETURN
       (ADDSQ
        (MULTSQ (OFSF_SUBF (CDAR F) V U) (OFSF_POW2Q (CAAAR F) (CDAAR F)))
        NRED)))) 
(PUT 'OFSF_SUBQ 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SUBQ 'DEFINED-ON-LINE '505) 
(PUT 'OFSF_SUBQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUBQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBQ (Q V U)
    (MULTSQ (OFSF_SUBF (CAR Q) V U) (INVSQ (OFSF_SUBF (CDR Q) V U)))) 
(PUT 'OFSF_POW2Q 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_POW2Q 'DEFINED-ON-LINE '509) 
(PUT 'OFSF_POW2Q 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_POW2Q 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_POW2Q (V D) (CONS (CONS (CONS (CONS V D) 1) NIL) 1)) 
(PUT 'OFSF_QESUBQAT-LOCAL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QESUBQAT-LOCAL 'DEFINED-ON-LINE '512) 
(PUT 'OFSF_QESUBQAT-LOCAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBQAT-LOCAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBQAT-LOCAL (OP W)
    (PROGN
     (COND ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ)) (LIST OP (CAR W) NIL))
           ((MEMBER (LIST 'GREATERP (CDR W) NIL) CL_THEO*)
            (LIST OP (CAR W) NIL))
           ((MEMBER (LIST 'LESSP (CDR W) NIL) CL_THEO*)
            (LIST (OFSF_ANEGREL OP) (CAR W) NIL))
           (T
            (LIST OP
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR W) (CDR W)))
                        (T (POLY-MULTF (CAR W) (CDR W))))
                  NIL))))) 
(PUT 'OFSF_QESUBI 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QESUBI 'DEFINED-ON-LINE '524) 
(PUT 'OFSF_QESUBI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBI (BVL THEO F V INF)
    (CONS NIL (CL_APPLY2ATS1 F 'OFSF_QESUBIAT (LIST V INF)))) 
(PUT 'OFSF_QESUBIAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBIAT 'DEFINED-ON-LINE '535) 
(PUT 'OFSF_QESUBIAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBIAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBIAT (ATF V INF)
    (PROG (OP LHS)
      (COND ((NOT (MEMQ V (OFSF_VARLAT ATF))) (RETURN ATF)))
      (SETQ OP (CAR ATF))
      (SETQ LHS (CADR ATF))
      (COND
       ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ))
        (RETURN (OFSF_QESUBTRANSEQ OP LHS V))))
      (RETURN (OFSF_QESUBIORD OP LHS V INF)))) 
(PUT 'OFSF_QESUBTRANSEQ 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBTRANSEQ 'DEFINED-ON-LINE '551) 
(PUT 'OFSF_QESUBTRANSEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBTRANSEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBTRANSEQ (OP LHS V)
    (COND ((EQ OP 'EQUAL) (OFSF_QESUBTRANSEQUAL LHS V))
          (T (CL_NNFNOT (OFSF_QESUBTRANSEQUAL LHS V))))) 
(PUT 'OFSF_QESUBTRANSEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QESUBTRANSEQUAL 'DEFINED-ON-LINE '562) 
(PUT 'OFSF_QESUBTRANSEQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBTRANSEQUAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBTRANSEQUAL (LHS V) (OFSF_QESUBTRANSEQUAL1 (SFTO_REORDER LHS V) V)) 
(PUT 'OFSF_QESUBTRANSEQUAL1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QESUBTRANSEQUAL1 'DEFINED-ON-LINE '569) 
(PUT 'OFSF_QESUBTRANSEQUAL1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBTRANSEQUAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBTRANSEQUAL1 (LHS V)
    (PROG (CL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (ATOM LHS) (ATOM (CAR LHS)))) (EQ (CAAAR LHS) V)))
          (RETURN NIL)))
        (PROGN
         (SETQ CL (CONS (LIST 'EQUAL (REORDER (CDAR LHS)) NIL) CL))
         (SETQ LHS (CDR LHS)))
        (GO WHILELABEL))
      (SETQ CL (CONS (LIST 'EQUAL (REORDER LHS) NIL) CL))
      (RETURN
       (COND ((AND CL (CDR CL)) (CONS 'AND CL))
             ((NULL CL) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR CL)))))) 
(PUT 'OFSF_QESUBIORD 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QESUBIORD 'DEFINED-ON-LINE '584) 
(PUT 'OFSF_QESUBIORD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBIORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBIORD (OP F V INF) (OFSF_QESUBIORD1 OP (SFTO_REORDER F V) V INF)) 
(PUT 'OFSF_QESUBIORD1 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QESUBIORD1 'DEFINED-ON-LINE '593) 
(PUT 'OFSF_QESUBIORD1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBIORD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBIORD1 (OP F V INF)
    (PROG (AN)
      (COND
       ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) V))
        (RETURN (LIST OP (REORDER F) NIL))))
      (SETQ AN
              (COND
               ((AND (EQ INF 'MINF) (NOT (EVENP (CDAAR F))))
                (NEGF (REORDER (CDAR F))))
               (T (REORDER (CDAR F)))))
      (RETURN
       (CONS 'OR
             (LIST (LIST (OFSF_MKSTRICT OP) AN NIL)
                   (CONS 'AND
                         (LIST (LIST 'EQUAL AN NIL)
                               (OFSF_QESUBIORD1 OP (CDR F) V INF)))))))) 
(PUT 'OFSF_QESUBCRPE1 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCRPE1 'DEFINED-ON-LINE '612) 
(PUT 'OFSF_QESUBCRPE1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCRPE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCRPE1 (BVL THEO F V CO R)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBRPE F V R))))))) 
(PUT 'OFSF_QESUBCRME1 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCRME1 'DEFINED-ON-LINE '628) 
(PUT 'OFSF_QESUBCRME1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCRME1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCRME1 (BVL THEO F V CO R)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBRME F V R))))))) 
(PUT 'OFSF_QESUBCRPE2 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_QESUBCRPE2 'DEFINED-ON-LINE '644) 
(PUT 'OFSF_QESUBCRPE2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCRPE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_QESUBCRPE2 (BVL THEO F V CO R1 R2)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN
       (CONS (CAR W)
             (CONS 'AND
                   (LIST (CDR W)
                         (CONS 'OR
                               (LIST (OFSF_QESUBRPE F V R1)
                                     (OFSF_QESUBRPE F V R2))))))))) 
(PUT 'OFSF_QESUBCRME2 'NUMBER-OF-ARGS 7) 
(PUT 'OFSF_QESUBCRME2 'DEFINED-ON-LINE '662) 
(PUT 'OFSF_QESUBCRME2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCRME2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_QESUBCRME2 (BVL THEO F V CO R1 R2)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN
       (CONS (CAR W)
             (CONS 'AND
                   (LIST (CDR W)
                         (CONS 'OR
                               (LIST (OFSF_QESUBRME F V R1)
                                     (OFSF_QESUBRME F V R2))))))))) 
(PUT 'OFSF_QESUBRPE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBRPE 'DEFINED-ON-LINE '680) 
(PUT 'OFSF_QESUBRPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRPE (F V R)
    (CL_APPLY2ATS1 F 'OFSF_QESUBPMEAT (LIST V R 'OFSF_QESUBR T))) 
(PUT 'OFSF_QESUBRME 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBRME 'DEFINED-ON-LINE '687) 
(PUT 'OFSF_QESUBRME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBRME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBRME (F V R)
    (CL_APPLY2ATS1 F 'OFSF_QESUBPMEAT (LIST V R 'OFSF_QESUBR NIL))) 
(PUT 'OFSF_QESUBCQPE 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCQPE 'DEFINED-ON-LINE '694) 
(PUT 'OFSF_QESUBCQPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCQPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCQPE (BVL THEO F V CO Q)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBQPE F V Q))))))) 
(PUT 'OFSF_QESUBCQME 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_QESUBCQME 'DEFINED-ON-LINE '710) 
(PUT 'OFSF_QESUBCQME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBCQME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBCQME (BVL THEO F V CO Q)
    (PROG (W)
      (SETQ W (OFSF_SUBSIMPL BVL CO THEO))
      (COND ((EQ (CDR W) 'FALSE) (RETURN (CONS (CAR W) 'FALSE))))
      (RETURN (CONS (CAR W) (CONS 'AND (LIST (CDR W) (OFSF_QESUBQME F V Q))))))) 
(PUT 'OFSF_QESUBQPE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBQPE 'DEFINED-ON-LINE '726) 
(PUT 'OFSF_QESUBQPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBQPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBQPE (F V Q)
    (CL_APPLY2ATS1 F 'OFSF_QESUBPMEAT (LIST V Q 'OFSF_QESUBQ T))) 
(PUT 'OFSF_QESUBQME 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QESUBQME 'DEFINED-ON-LINE '734) 
(PUT 'OFSF_QESUBQME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBQME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBQME (F V Q)
    (CL_APPLY2ATS1 F 'OFSF_QESUBPMEAT (LIST V Q 'OFSF_QESUBQ NIL))) 
(PUT 'OFSF_QESUBPMEAT 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QESUBPMEAT 'DEFINED-ON-LINE '742) 
(PUT 'OFSF_QESUBPMEAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBPMEAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBPMEAT (ATF V U FINSUB PLE)
    (PROG (OP LHS)
      (COND ((NOT (MEMQ V (OFSF_VARLAT ATF))) (RETURN ATF)))
      (SETQ OP (CAR ATF))
      (SETQ LHS (CADR ATF))
      (COND
       ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ))
        (RETURN (OFSF_QESUBTRANSEQ OP LHS V))))
      (RETURN (APPLY FINSUB (LIST (OFSF_QESUBPMEORD OP LHS V PLE) V U))))) 
(PUT 'OFSF_QESUBPMEORD 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QESUBPMEORD 'DEFINED-ON-LINE '760) 
(PUT 'OFSF_QESUBPMEORD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QESUBPMEORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QESUBPMEORD (OP F V PLE)
    (COND ((EQ (DEGREEF F V) 0) (LIST OP F NIL))
          (T
           (CONS 'OR
                 (LIST (LIST (OFSF_MKSTRICT OP) F NIL)
                       (CONS 'AND
                             (LIST (LIST 'EQUAL F NIL)
                                   (OFSF_QESUBPMEORD OP
                                    (COND (PLE (DIFF F V))
                                          (T (NEGF (DIFF F V))))
                                    V PLE)))))))) 
(PUT 'OFSF_SUBSIMPL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SUBSIMPL 'DEFINED-ON-LINE '775) 
(PUT 'OFSF_SUBSIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUBSIMPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBSIMPL (BVL F TH)
    (PROG (NTH)
      (SETQ F (CL_SIMPL F TH (MINUS 1)))
      (COND ((NOT *RLQEGEN) (RETURN (CONS NIL F))))
      (SETQ NTH
              (PROG (ATF FORALL-RESULT FORALL-ENDPTR)
                (SETQ ATF (CL_ATL1 F))
               STARTOVER
                (COND ((NULL ATF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (ATF)
                           (COND
                            ((AND (EQUAL (CAR ATF) 'EQUAL)
                                  (OFSF_VALASSP BVL (CADR ATF)))
                             (LIST (LIST 'NEQ (CADR ATF) NIL)))))
                         (CAR ATF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ ATF (CDR ATF))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL ATF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (ATF)
                           (COND
                            ((AND (EQUAL (CAR ATF) 'EQUAL)
                                  (OFSF_VALASSP BVL (CADR ATF)))
                             (LIST (LIST 'NEQ (CADR ATF) NIL)))))
                         (CAR ATF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ ATF (CDR ATF))
                (GO LOOPLABEL)))
      (COND
       (NTH
        (PROGN
         (COND
          ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
           (IOTO_PRIN2 "!")))
         (RETURN (CONS NTH (CL_SIMPL F (APPEND NTH TH) (MINUS 1)))))))
      (RETURN (CONS NIL F)))) 
(PUT 'OFSF_VALASSP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_VALASSP 'DEFINED-ON-LINE '797) 
(PUT 'OFSF_VALASSP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_VALASSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_VALASSP (BVL SF)
    (AND (OR *RLQEGENCT (SFTO_MONFP SF))
         (NULL (INTERSECTION BVL (KERNELS SF))))) 
(DE OFSF_MKALP (TAG L) (CONS (LIST (CONS TAG L)) (LIST (CONS TAG 1)))) 
(PUT 'OFSF_MKALP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKALP 'DEFINED-ON-LINE '824) 
(PUT 'OFSF_MKALP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKALP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_MKALP 'INLINE
      '(LAMBDA (TAG L) (CONS (LIST (CONS TAG L)) (LIST (CONS TAG 1))))) 
(DE OFSF_CETERM1A (M U) (LIST (LIST 'NEQ M NIL) U)) 
(PUT 'OFSF_CETERM1A 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CETERM1A 'DEFINED-ON-LINE '829) 
(PUT 'OFSF_CETERM1A 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CETERM1A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_CETERM1A 'INLINE '(LAMBDA (M U) (LIST (LIST 'NEQ M NIL) U))) 
(DE OFSF_CETERM2A (A M U)
    (COND (A (LIST (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'NEQ M NIL))) U))
          (T (LIST (LIST 'NEQ M NIL) U)))) 
(PUT 'OFSF_CETERM2A 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_CETERM2A 'DEFINED-ON-LINE '835) 
(PUT 'OFSF_CETERM2A 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CETERM2A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_CETERM2A 'INLINE
      '(LAMBDA (A M U)
         (COND
          (A (LIST (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'NEQ M NIL))) U))
          (T (LIST (LIST 'NEQ M NIL) U))))) 
(DE OFSF_CETERM1L (A L) (CONS (LIST 'NEQ A NIL) L)) 
(PUT 'OFSF_CETERM1L 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CETERM1L 'DEFINED-ON-LINE '844) 
(PUT 'OFSF_CETERM1L 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CETERM1L 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_CETERM1L 'INLINE '(LAMBDA (A L) (CONS (LIST 'NEQ A NIL) L))) 
(DE OFSF_CETERM2L (A D L)
    (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) (LIST 'GEQ D NIL))) L)) 
(PUT 'OFSF_CETERM2L 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_CETERM2L 'DEFINED-ON-LINE '849) 
(PUT 'OFSF_CETERM2L 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CETERM2L 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_CETERM2L 'INLINE
      '(LAMBDA (A D L)
         (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) (LIST 'GEQ D NIL))) L))) 
(DE OFSF_MKTAG1 (X) (INTERN (COMPRESS (NCONC (EXPLODE X) '(|1|))))) 
(PUT 'OFSF_MKTAG1 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_MKTAG1 'DEFINED-ON-LINE '855) 
(PUT 'OFSF_MKTAG1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKTAG1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'OFSF_MKTAG1 'INLINE
      '(LAMBDA (X) (INTERN (COMPRESS (NCONC (EXPLODE X) '(|1|)))))) 
(DE OFSF_MKTAG2 (X Y)
    (INTERN (COMPRESS (NCONC (EXPLODE X) (CONS '|2| (EXPLODE Y)))))) 
(PUT 'OFSF_MKTAG2 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKTAG2 'DEFINED-ON-LINE '860) 
(PUT 'OFSF_MKTAG2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKTAG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'OFSF_MKTAG2 'INLINE
      '(LAMBDA (X Y)
         (INTERN (COMPRESS (NCONC (EXPLODE X) (CONS '|2| (EXPLODE Y))))))) 
(SWITCH (LIST 'RLELIMSETOPTMAREK)) 
(OFF1 'RLELIMSETOPTMAREK) 
(PUT 'OFSF_TRANSLAT 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLAT 'DEFINED-ON-LINE '868) 
(PUT 'OFSF_TRANSLAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT (ATF V THEO POS ANS)
    (COND (*RLELIMSETOPTMAREK (OFSF_TRANSLAT_MAREK ATF V THEO POS ANS))
          (T (OFSF_TRANSLAT_ORIG ATF V THEO POS ANS)))) 
(PUT 'OFSF_TRANSLAT_MAREK 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLAT_MAREK 'DEFINED-ON-LINE '874) 
(PUT 'OFSF_TRANSLAT_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT_MAREK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT_MAREK (ATF V THEO POS ANS)
    (PROG (RES)
      (COND
       ((MEMQ V (OFSF_VARLAT ATF))
        (PROGN
         (SETQ RES
                 (COND (POS (OFSF_TRANSLAT_MAREK1 ATF V THEO))
                       (T (OFSF_TRANSLAT_MAREK1 (OFSF_NEGATEAT ATF) V THEO))))
         (COND
          ((EQUAL RES '(NIL))
           (SETQ RES
                   (CONS (LIST (CONS 'ANYPOINT NIL))
                         (LIST (CONS 'ANYPOINT 1))))))
         (RETURN RES))))
      (RETURN (CONS NIL NIL)))) 
(PUT 'OFSF_TRANSLAT_MAREK1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TRANSLAT_MAREK1 'DEFINED-ON-LINE '903) 
(PUT 'OFSF_TRANSLAT_MAREK1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT_MAREK1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT_MAREK1 (ATF V THEO)
    (PROG (OP P W A B C RES D)
      (SETQ D 0)
      (SETQ OP (CAR ATF))
      (SETQ P (CADR ATF))
      (SETQ W (SETKORDER (LIST V)))
      (SETQ P (REORDER P))
      (SETQ D (CDAAR P))
      (COND ((GREATERP D 2) (SETQ RES (CONS 'FAILED (LIST V "^" D))))
            ((EQUAL D 2)
             (PROGN
              (SETQ A (CDAR P))
              (SETQ P (CDR P))
              (COND
               ((OR (OR (ATOM P) (ATOM (CAR P))) (NEQ (CAAAR P) V))
                (PROGN (SETQ B NIL) (SETQ C P)))
               (T (PROGN (SETQ B (CDAR P)) (SETQ C (CDR P)))))
              (SETQ RES (OFSF_CMPALPQUA_MAREK OP A B C V THEO))))
            (T
             (PROGN
              (SETQ B (CDAR P))
              (SETQ C (CDR P))
              (SETQ RES (OFSF_CMPALPLIN_MAREK OP B C V THEO)))))
      (SETKORDER W)
      (RETURN RES))) 
(PUT 'OFSF_CMPALPLIN_MAREK 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_CMPALPLIN_MAREK 'DEFINED-ON-LINE '934) 
(PUT 'OFSF_CMPALPLIN_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CMPALPLIN_MAREK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CMPALPLIN_MAREK (OP B C V THEO)
    (PROG (W GUARD LGUARD UGUARD)
      (SETQ W (CONS 'QUOT (MULTSQ (CONS (NEGF C) 1) (INVSQ (CONS B 1)))))
      (COND
       ((MEMQ OP '(EQUAL NEQ))
        (PROGN
         (SETQ GUARD (LIST 'NEQ B NIL))
         (RETURN (OFSF_MKALP_MAREK OP (LIST (CONS GUARD W)))))))
      (COND
       ((OFSF_SUREP (LIST 'GREATERP B NIL) THEO)
        (PROGN
         (SETQ GUARD 'TRUE)
         (COND
          ((EQ OP 'LESSP)
           (RETURN (OFSF_MKALP_MAREK 'UBME (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'GREATERP)
           (RETURN (OFSF_MKALP_MAREK 'LBPE (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'LEQ) (RETURN (OFSF_MKALP_MAREK 'UB (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'GEQ)
           (RETURN (OFSF_MKALP_MAREK 'LB (LIST (CONS GUARD W))))))))
       ((OFSF_SUREP (LIST 'LESSP B NIL) THEO)
        (PROGN
         (SETQ GUARD 'TRUE)
         (COND
          ((EQ OP 'LESSP)
           (RETURN (OFSF_MKALP_MAREK 'LBPE (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'GREATERP)
           (RETURN (OFSF_MKALP_MAREK 'UBME (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'LEQ) (RETURN (OFSF_MKALP_MAREK 'LB (LIST (CONS GUARD W))))))
         (COND
          ((EQ OP 'GEQ)
           (RETURN (OFSF_MKALP_MAREK 'UB (LIST (CONS GUARD W))))))))
       (T
        (PROGN
         (COND
          ((EQ OP 'LESSP)
           (PROGN
            (SETQ LGUARD (LIST 'LESSP B NIL))
            (SETQ UGUARD (LIST 'GREATERP B NIL))
            (RETURN
             (CL_ALPUNION
              (LIST (OFSF_MKALP_MAREK 'UBME (LIST (CONS UGUARD W)))
                    (OFSF_MKALP_MAREK 'LBPE (LIST (CONS LGUARD W)))))))))
         (COND
          ((EQ OP 'GREATERP)
           (PROGN
            (SETQ LGUARD (LIST 'GREATERP B NIL))
            (SETQ UGUARD (LIST 'LESSP B NIL))
            (RETURN
             (CL_ALPUNION
              (LIST (OFSF_MKALP_MAREK 'UBME (LIST (CONS UGUARD W)))
                    (OFSF_MKALP_MAREK 'LBPE (LIST (CONS LGUARD W)))))))))
         (COND
          ((EQ OP 'LEQ)
           (PROGN
            (SETQ LGUARD (LIST 'LESSP B NIL))
            (SETQ UGUARD (LIST 'GREATERP B NIL))
            (RETURN
             (CL_ALPUNION
              (LIST (OFSF_MKALP_MAREK 'UB (LIST (CONS UGUARD W)))
                    (OFSF_MKALP_MAREK 'LB (LIST (CONS LGUARD W)))))))))
         (COND
          ((EQ OP 'GEQ)
           (PROGN
            (SETQ LGUARD (LIST 'GREATERP B NIL))
            (SETQ UGUARD (LIST 'LESSP B NIL))
            (RETURN
             (CL_ALPUNION
              (LIST (OFSF_MKALP_MAREK 'UB (LIST (CONS UGUARD W)))
                    (OFSF_MKALP_MAREK 'LB (LIST (CONS LGUARD W)))))))))))))) 
(PUT 'OFSF_CMPALPQUA_MAREK 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_CMPALPQUA_MAREK 'DEFINED-ON-LINE '990) 
(PUT 'OFSF_CMPALPQUA_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CMPALPQUA_MAREK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_CMPALPQUA_MAREK (OP A B C V THEO)
    (PROG (DISCR DGEQ0 MROOT PROOT QUOT GUARD ROOTL ROOTUL ROOTLL)
      (SETQ DISCR
              (ADDF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF B B)) (T (POLY-MULTF B B)))
               (NEGF
                ((LAMBDA (G178)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF 4 G178))
                         (T (POLY-MULTF 4 G178))))
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF A C))
                       (T (POLY-MULTF A C)))))))
      (SETQ DGEQ0 (LIST 'GEQ DISCR NIL))
      (SETQ MROOT
              (CONS 'REXP
                    (LIST (NEGF B) (MINUS 1) DISCR
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 A))
                                (T (POLY-MULTF 2 A))))))
      (SETQ PROOT
              (CONS 'REXP
                    (LIST (NEGF B) 1 DISCR
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 A))
                                (T (POLY-MULTF 2 A))))))
      (COND
       ((MEMQ OP '(EQUAL NEQ))
        (PROGN
         (COND
          ((NOT (OFSF_SUREP (LIST 'EQUAL B NIL) THEO))
           (PROGN
            (SETQ QUOT
                    (CONS 'QUOT (MULTSQ (CONS (NEGF C) 1) (INVSQ (CONS B 1)))))
            (SETQ GUARD
                    (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'NEQ B NIL))))
            (SETQ ROOTL (CONS (CONS GUARD QUOT) ROOTL)))))
         (SETQ GUARD (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)))
         (SETQ ROOTL (CONS (CONS GUARD MROOT) ROOTL))
         (SETQ ROOTL (CONS (CONS GUARD PROOT) ROOTL))
         (RETURN (OFSF_MKALP_MAREK OP ROOTL)))))
      (COND
       ((NOT (NULL B))
        (SETQ QUOT
                (CONS 'QUOT (MULTSQ (CONS (NEGF C) 1) (INVSQ (CONS B 1)))))))
      (COND
       ((EQ OP 'LESSP)
        (PROGN
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'GREATERP B NIL) THEO)))
           (SETQ ROOTLL
                   (LIST
                    (CONS
                     (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'LESSP B NIL)))
                     QUOT)))))
         (SETQ ROOTLL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) MROOT)
                       ROOTLL))
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'LESSP B NIL) THEO)))
           (SETQ ROOTUL
                   (LIST
                    (CONS
                     (CONS 'AND
                           (LIST (LIST 'EQUAL A NIL) (LIST 'GREATERP B NIL)))
                     QUOT)))))
         (SETQ ROOTUL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) PROOT)
                       ROOTUL))
         (RETURN
          (CL_ALPUNION
           (LIST (OFSF_MKALP_MAREK 'LBPE ROOTLL)
                 (OFSF_MKALP_MAREK 'UBME ROOTUL)))))))
      (COND
       ((EQ OP 'GREATERP)
        (PROGN
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'LESSP B NIL) THEO)))
           (SETQ ROOTLL
                   (LIST
                    (CONS
                     (CONS 'AND
                           (LIST (LIST 'EQUAL A NIL) (LIST 'GREATERP B NIL)))
                     QUOT)))))
         (SETQ ROOTLL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) PROOT)
                       ROOTLL))
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'GREATERP B NIL) THEO)))
           (SETQ ROOTUL
                   (LIST
                    (CONS
                     (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'LESSP B NIL)))
                     QUOT)))))
         (SETQ ROOTUL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) MROOT)
                       ROOTUL))
         (RETURN
          (CL_ALPUNION
           (LIST (OFSF_MKALP_MAREK 'LBPE ROOTLL)
                 (OFSF_MKALP_MAREK 'UBME ROOTUL)))))))
      (COND
       ((EQ OP 'LEQ)
        (PROGN
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'GREATERP B NIL) THEO)))
           (SETQ ROOTLL
                   (LIST
                    (CONS
                     (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'LESSP B NIL)))
                     QUOT)))))
         (SETQ ROOTLL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) MROOT)
                       ROOTLL))
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'LESSP B NIL) THEO)))
           (SETQ ROOTUL
                   (LIST
                    (CONS
                     (CONS 'AND
                           (LIST (LIST 'EQUAL A NIL) (LIST 'GREATERP B NIL)))
                     QUOT)))))
         (SETQ ROOTUL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) PROOT)
                       ROOTUL))
         (RETURN
          (CL_ALPUNION
           (LIST (OFSF_MKALP_MAREK 'LB ROOTLL)
                 (OFSF_MKALP_MAREK 'UB ROOTUL)))))))
      (COND
       ((EQ OP 'GEQ)
        (PROGN
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'LESSP B NIL) THEO)))
           (SETQ ROOTLL
                   (LIST
                    (CONS
                     (CONS 'AND
                           (LIST (LIST 'EQUAL A NIL) (LIST 'GREATERP B NIL)))
                     QUOT)))))
         (SETQ ROOTLL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) PROOT)
                       ROOTLL))
         (COND
          ((AND (NOT (NULL B)) (NOT (OFSF_SUREP (LIST 'GREATERP B NIL) THEO)))
           (SETQ ROOTUL
                   (LIST
                    (CONS
                     (CONS 'AND (LIST (LIST 'EQUAL A NIL) (LIST 'LESSP B NIL)))
                     QUOT)))))
         (SETQ ROOTUL
                 (CONS (CONS (CONS 'AND (LIST (LIST 'NEQ A NIL) DGEQ0)) MROOT)
                       ROOTUL))
         (RETURN
          (CL_ALPUNION
           (LIST (OFSF_MKALP_MAREK 'LB ROOTLL)
                 (OFSF_MKALP_MAREK 'UB ROOTUL))))))))) 
(PUT 'OFSF_MKALP_MAREK 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKALP_MAREK 'DEFINED-ON-LINE '1048) 
(PUT 'OFSF_MKALP_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKALP_MAREK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKALP_MAREK (TAG ROOTL)
    (CONS (LIST (CONS TAG ROOTL)) (LIST (CONS TAG (LENGTH ROOTL))))) 
(PUT 'OFSF_TRANSLAT_ORIG 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLAT_ORIG 'DEFINED-ON-LINE '1051) 
(PUT 'OFSF_TRANSLAT_ORIG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT_ORIG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT_ORIG (ATF V THEO POS ANS)
    (PROG (SVRLQESR RES)
      (COND
       ((OR ANS *RLQEPRECISE *RLQEAPRECISE)
        (PROGN (SETQ SVRLQESR *RLQESR) (ON1 'RLQESR))))
      (COND
       ((MEMQ V (OFSF_VARLAT ATF))
        (PROGN
         (SETQ RES
                 (COND (POS (OFSF_TRANSLAT1 ATF V THEO))
                       (T (OFSF_TRANSLAT1 (OFSF_NEGATEAT ATF) V THEO))))
         (COND
          ((EQUAL RES '(NIL))
           (SETQ RES
                   (CONS (LIST (CONS 'ANYPOINT NIL))
                         (LIST (CONS 'ANYPOINT 1))))))))
       (T (SETQ RES (CONS NIL NIL))))
      (COND ((AND ANS (NULL SVRLQESR)) (OFF1 'RLQESR)))
      (RETURN RES))) 
(PUT 'OFSF_TRANSLAT1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TRANSLAT1 'DEFINED-ON-LINE '1080) 
(PUT 'OFSF_TRANSLAT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT1 (ATF V THEO)
    (PROG (W REL KL C K)
      (COND
       (*RLBRKCXK
        (PROGN
         ((LAMBDA (*RLBRKCXK) (SETQ KL (OFSF_VARLAT ATF))) NIL)
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C KL)) (RETURN NIL)))
           (PROGN
            (SETQ K (PROG1 (CAR KL) (SETQ KL (CDR KL))))
            (COND ((AND (PAIRP K) (MEMQ V (LTO_LPVARL K))) (SETQ C NIL))))
           (GO WHILELABEL))
         (COND
          ((NOT C)
           (PROGN
            (LPRIM (LIST "ignoring quantified variable" V "in" K))
            (RETURN (CONS NIL NIL))))))))
      (SETQ W (OFSF_MKTRIPLEL (CADR ATF) V))
      (COND ((EQ (CAR W) 'FAILED) (RETURN W)))
      (SETQ REL (CAR ATF))
      (COND ((NULL (CAR W)) (RETURN (OFSF_TRANSLAT2 REL (CADR W) THEO))))
      (RETURN
       (CL_ALPUNION
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X (CDR W))
         STARTOVER
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  ((LAMBDA (X)
                     (COND
                      ((MEMQ REL '(GEQ LEQ LESSP GREATERP))
                       (LIST (OFSF_TRANSLAT2 REL X THEO)
                             (OFSF_TRANSLAT2 (OFSF_ANEGREL REL) X THEO)))
                      (T (LIST (OFSF_TRANSLAT2 REL X THEO)))))
                   (CAR X)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
          (SETQ X (CDR X))
          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
         LOOPLABEL
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  ((LAMBDA (X)
                     (COND
                      ((MEMQ REL '(GEQ LEQ LESSP GREATERP))
                       (LIST (OFSF_TRANSLAT2 REL X THEO)
                             (OFSF_TRANSLAT2 (OFSF_ANEGREL REL) X THEO)))
                      (T (LIST (OFSF_TRANSLAT2 REL X THEO)))))
                   (CAR X)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
          (SETQ X (CDR X))
          (GO LOOPLABEL)))))) 
(PUT 'OFSF_TRANSLAT2 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TRANSLAT2 'DEFINED-ON-LINE '1111) 
(PUT 'OFSF_TRANSLAT2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLAT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLAT2 (REL TRIP THEO)
    (COND
     ((NULL (CAR TRIP))
      (OFSF_TRANSLATLIN REL (CADR TRIP) (CADDR TRIP) THEO NIL))
     (T (OFSF_TRANSLATQUA REL (CAR TRIP) (CADR TRIP) (CADDR TRIP) THEO)))) 
(PUT 'OFSF_TRANSLATLIN 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLATLIN 'DEFINED-ON-LINE '1120) 
(PUT 'OFSF_TRANSLATLIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLATLIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLATLIN (R M B THEO XC)
    (COND
     ((AND *RLQELOCAL (NULL (SETDIFF (KERNELS M) CL_LPS*)))
      (OFSF_TRANSLATLIN-LOCAL R M B THEO XC))
     (T
      ((LAMBDA (G182)
         (CONS
          (LIST
           (CONS G182
                 (LIST
                  ((LAMBDA (G181)
                     (COND
                      (XC
                       (LIST
                        (CONS 'AND
                              (LIST (LIST 'EQUAL XC NIL) (LIST 'NEQ M NIL)))
                        G181))
                      (T (LIST (LIST 'NEQ M NIL) G181))))
                   (OFSF_MKSOL1 M B)))))
          (LIST (CONS G182 1))))
       (OFSF_TLLTAG R M THEO))))) 
(PUT 'OFSF_TRANSLATLIN-LOCAL 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLATLIN-LOCAL 'DEFINED-ON-LINE '1130) 
(PUT 'OFSF_TRANSLATLIN-LOCAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLATLIN-LOCAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLATLIN-LOCAL (R M B THEO XC)
    (PROG (W)
      (SETQ W (OFSF_TLLTAG-LOCAL1 R M THEO))
      (COND ((NULL (CAR W)) (RETURN (CONS NIL NIL))))
      (RETURN
       (CONS
        (LIST
         (CONS (CDR W)
               (LIST
                ((LAMBDA (G186)
                   (COND
                    (XC
                     (LIST
                      (CONS 'AND
                            (LIST (LIST 'EQUAL XC NIL)
                                  (LIST 'NEQ (CAR W) NIL)))
                      G186))
                    (T (LIST (LIST 'NEQ (CAR W) NIL) G186))))
                 (OFSF_MKSOL1 M B)))))
        (LIST (CONS (CDR W) 1)))))) 
(PUT 'OFSF_TLLTAG 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TLLTAG 'DEFINED-ON-LINE '1138) 
(PUT 'OFSF_TLLTAG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TLLTAG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TLLTAG (R M THEO)
    (COND
     ((OR (EQ R 'EQUAL) (EQ R 'NEQ))
      (INTERN (COMPRESS (NCONC (EXPLODE R) '(|1|)))))
     ((OR (OFSF_SUREP (LIST 'GEQ M NIL) THEO) (AND *RLPOS (SFTO_VARP M)))
      (INTERN (COMPRESS (NCONC (EXPLODE R) '(|1|)))))
     ((OFSF_SUREP (LIST 'LEQ M NIL) THEO)
      (INTERN (COMPRESS (NCONC (EXPLODE (OFSF_ANEGREL R)) '(|1|)))))
     ((AND *RLQELOCAL (NULL (SETDIFF (KERNELS M) CL_LPS*)))
      (OFSF_TLLTAG-LOCAL R M THEO))
     ((OR (EQ R 'LESSP) (EQ R 'GREATERP)) 'SO1) (T 'WO1))) 
(PUT 'OFSF_TLLTAG-LOCAL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TLLTAG-LOCAL 'DEFINED-ON-LINE '1156) 
(PUT 'OFSF_TLLTAG-LOCAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TLLTAG-LOCAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TLLTAG-LOCAL (R M THEO) (CDR (OFSF_TLLTAG-LOCAL1 R M THEO))) 
(PUT 'OFSF_TLLTAG-LOCAL1 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_TLLTAG-LOCAL1 'DEFINED-ON-LINE '1159) 
(PUT 'OFSF_TLLTAG-LOCAL1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TLLTAG-LOCAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TLLTAG-LOCAL1 (R M THEO)
    (PROG (W)
      (SETQ W (CAR (SUBF M CL_PAL*)))
      (COND
       ((NULL W)
        (PROGN
         (SETQ CL_THEO* (CONS (LIST 'EQUAL M NIL) CL_THEO*))
         (RETURN (CONS NIL (INTERN (COMPRESS (NCONC (EXPLODE R) '(|1|)))))))))
      (COND
       ((MINUSF W)
        (PROGN
         (SETQ CL_THEO* (CONS (LIST 'LESSP M NIL) CL_THEO*))
         (RETURN
          (CONS (MINUS 1)
                (INTERN
                 (COMPRESS (NCONC (EXPLODE (OFSF_ANEGREL R)) '(|1|)))))))))
      (SETQ CL_THEO* (CONS (LIST 'GREATERP M NIL) CL_THEO*))
      (RETURN (CONS 1 (INTERN (COMPRESS (NCONC (EXPLODE R) '(|1|)))))))) 
(PUT 'OFSF_TRANSLATQUA 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_TRANSLATQUA 'DEFINED-ON-LINE '1174) 
(PUT 'OFSF_TRANSLATQUA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_TRANSLATQUA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_TRANSLATQUA (R A B C THEO)
    (PROG (W TAGBASE TAG ESET)
      (SETQ W (OFSF_MKSOL2 A B C))
      (COND ((EQ W 'FAILED) (RETURN (CONS NIL NIL))))
      (SETQ TAGBASE
              (COND ((MEMQ R '(LESSP GREATERP)) 'SO) ((MEMQ R '(LEQ GEQ)) 'WO)
                    (T R)))
      (COND
       ((EQ (CAR W) 'ONEQUOT)
        (PROGN
         (SETQ TAG
                 (INTERN
                  (COMPRESS
                   (NCONC (EXPLODE TAGBASE) (CONS '|2| (EXPLODE '|1Q|))))))
         (SETQ ESET (LIST (LIST (LIST 'NEQ A NIL) (CDR W))))))
       ((EQ (CAR W) 'TWOROOT)
        (PROGN
         (COND
          (*RLQESR
           (PROGN
            (SETQ TAG
                    (INTERN
                     (COMPRESS
                      (NCONC (EXPLODE TAGBASE) (CONS '|2| (EXPLODE '|1R|))))))
            (SETQ ESET
                    (LIST
                     (CONS
                      (CONS 'AND
                            (LIST (LIST 'NEQ A NIL) (LIST 'GEQ (CADR W) NIL)))
                      (LIST (CADDR W)))
                     (CONS
                      (CONS 'AND
                            (LIST (LIST 'NEQ A NIL) (LIST 'GEQ (CADR W) NIL)))
                      (LIST (CADDDR W)))))))
          (T
           (PROGN
            (SETQ TAG
                    (INTERN
                     (COMPRESS
                      (NCONC (EXPLODE TAGBASE) (CONS '|2| (EXPLODE '|2R|))))))
            (SETQ ESET
                    (LIST
                     (CONS
                      (CONS 'AND
                            (LIST (LIST 'NEQ A NIL) (LIST 'GEQ (CADR W) NIL)))
                      (LIST (CADDR W) (CADDDR W)))))))))))
      (COND
       ((AND *RLQELOCAL (NULL (SETDIFF (KERNELS A) CL_LPS*)))
        (PROGN
         (SETQ W (CAR (SUBF A CL_PAL*)))
         (COND
          ((NOT (NULL W))
           (PROGN
            (SETQ CL_THEO* (CONS (LIST 'NEQ A NIL) CL_THEO*))
            (RETURN (CONS (LIST (CONS TAG ESET)) (LIST (CONS TAG 1)))))))
         (SETQ CL_THEO* (CONS (LIST 'EQUAL A NIL) CL_THEO*))
         (RETURN (OFSF_TRANSLATLIN R B C THEO A)))))
      (COND
       ((NOT (NULL B))
        (PROGN
         (SETQ W (OFSF_TRANSLATLIN R B C THEO A))
         (RETURN
          (CONS (LIST (CONS TAG ESET) (CAAR W))
                (LIST (CONS TAG 1) (CADR W)))))))
      (RETURN (CONS (LIST (CONS TAG ESET)) (LIST (CONS TAG 1)))))) 
(PUT 'OFSF_SUREP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SUREP 'DEFINED-ON-LINE '1218) 
(PUT 'OFSF_SUREP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUREP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUREP (F THEO) (EQ (CL_SIMPL F THEO (MINUS 1)) 'TRUE)) 
(PUT 'OFSF_MKTRIPLEL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKTRIPLEL 'DEFINED-ON-LINE '1224) 
(PUT 'OFSF_MKTRIPLEL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKTRIPLEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKTRIPLEL (U V)
    (PROG (W G FL A UL)
      (SETQ W (SETKORDER (LIST V)))
      (SETQ U (REORDER U))
      (COND
       ((LEQ (CDAAR U) 2)
        (PROGN
         (SETKORDER W)
         (RETURN (CONS NIL (LIST (OFSF_REOTRIP (OFSF_MKTRIPLE U))))))))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 (LIST "."))))
      (SETQ FL (CDR (SFTO_FCTRF U)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR FL))
         (SETQ FL (CDR FL))
         (SETQ G (DEGR (CAR A) V))
         (COND
          ((GREATERP G 2)
           (PROGN (SETQ UL (CONS 'FAILED (LIST V "^" G))) (SETQ FL NIL)))
          ((GREATERP G 0) (SETQ UL (CONS (CAR A) UL)))))
        (GO WHILELABEL))
      (SETKORDER W)
      (COND ((EQUAL (CAR UL) 'FAILED) (RETURN UL)))
      (RETURN
       (CONS 'FAC
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X UL)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X) (OFSF_REOTRIP (OFSF_MKTRIPLE X)))
                                 (CAR X))
                                NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X) (OFSF_REOTRIP (OFSF_MKTRIPLE X))) (CAR X))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'OFSF_MKTRIPLE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_MKTRIPLE 'DEFINED-ON-LINE '1263) 
(PUT 'OFSF_MKTRIPLE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKTRIPLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_MKTRIPLE (X)
    (PROG (A V)
      (SETQ V (CAAAR X))
      (COND ((EQ (CDAAR X) 2) (PROGN (SETQ A (CDAR X)) (SETQ X (CDR X)))))
      (RETURN
       (COND
        ((AND (NOT (OR (ATOM X) (ATOM (CAR X)))) (EQ (CAAAR X) V))
         (LIST A (CDAR X) (CDR X)))
        (T (LIST A NIL X)))))) 
(PUT 'OFSF_REOTRIP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_REOTRIP 'DEFINED-ON-LINE '1279) 
(PUT 'OFSF_REOTRIP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_REOTRIP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_REOTRIP (TRIP)
    (LIST (REORDER (CAR TRIP)) (REORDER (CADR TRIP)) (REORDER (CADDR TRIP)))) 
(PUT 'OFSF_MKSOL1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKSOL1 'DEFINED-ON-LINE '1285) 
(PUT 'OFSF_MKSOL1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKSOL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKSOL1 (M B) (MULTSQ (CONS (NEGF B) 1) (INVSQ (CONS M 1)))) 
(PUT 'OFSF_MKSOL2 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MKSOL2 'DEFINED-ON-LINE '1290) 
(PUT 'OFSF_MKSOL2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKSOL2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKSOL2 (A B C)
    (PROG (DISC W WW)
      (SETQ DISC
              (ADDF (EXPTF B 2)
                    (NEGF
                     ((LAMBDA (G190)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF 4 G190))
                              (T (POLY-MULTF 4 G190))))
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF A C))
                            (T (POLY-MULTF A C)))))))
      (COND
       ((AND (OR (ATOM DISC) (ATOM (CAR DISC))) (MINUSF DISC))
        (RETURN 'FAILED)))
      (SETQ A (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 A)) (T (POLY-MULTF 2 A))))
      (SETQ B (NEGF B))
      (COND
       ((NULL DISC)
        (RETURN (CONS 'ONEQUOT (MULTSQ (CONS B 1) (INVSQ (CONS A 1)))))))
      (COND
       ((AND *RLQELOCAL (NULL (SETDIFF (KERNELS DISC) CL_LPS*)))
        (PROGN
         (SETQ WW (CAR (SUBF DISC CL_PAL*)))
         (COND
          ((MINUSF WW)
           (PROGN
            (SETQ CL_THEO* (CONS (LIST 'LESSP DISC NIL) CL_THEO*))
            (RETURN 'FAILED))))
         (COND
          ((NULL WW)
           (PROGN
            (SETQ CL_THEO* (CONS (LIST 'EQUAL DISC NIL) CL_THEO*))
            (RETURN
             (CONS 'ONEQUOT (MULTSQ (CONS B 1) (INVSQ (CONS A 1)))))))))))
      (SETQ W (SFTO_SQRTF DISC))
      (COND (W (RETURN (CONS 'TWOROOT (CONS NIL (OFSF_MKSOL21Q B W A))))))
      (RETURN (CONS 'TWOROOT (CONS DISC (OFSF_MKSOL21R B DISC A)))))) 
(PUT 'OFSF_MKSOL21Q 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MKSOL21Q 'DEFINED-ON-LINE '1322) 
(PUT 'OFSF_MKSOL21Q 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKSOL21Q 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKSOL21Q (MB DISCR TA)
    (LIST (LIST MB (NEGF DISCR) 1 TA) (LIST MB DISCR 1 TA))) 
(PUT 'OFSF_MKSOL21R 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MKSOL21R 'DEFINED-ON-LINE '1328) 
(PUT 'OFSF_MKSOL21R 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKSOL21R 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKSOL21R (MB DISC TA)
    (LIST (LIST MB (MINUS 1) DISC TA) (LIST MB 1 DISC TA))) 
(FLAG '(RLQELOG) 'OPFN) 
(PUT 'RLQELOG 'NUMBER-OF-ARGS 0) 
(PUT 'RLQELOG 'DEFINED-ON-LINE '1350) 
(PUT 'RLQELOG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'RLQELOG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RLQELOG NIL
    (PROG (W)
      (SETQ W
              (CONS 'LIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (REVERSE RLQELOG*))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ RLQELOG* NIL)
      (RETURN W))) 
(PUT 'OFSF_ELIMSET 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSET 'DEFINED-ON-LINE '1357) 
(PUT 'OFSF_ELIMSET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSET (V ALP)
    (COND (*RLELIMSETOPTMAREK (OFSF_ELIMSET_MAREK V ALP))
          (T (OFSF_ELIMSET_ORIG V ALP)))) 
(PUT 'OFSF_ELIMSET_MAREK 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSET_MAREK 'DEFINED-ON-LINE '1363) 
(PUT 'OFSF_ELIMSET_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSET_MAREK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSET_MAREK (V ALP)
    (PROG (W TAKEU NUMAL RESL NLB NUB NLBPE NUBME NUML NUMU)
      (SETQ NLB 0)
      (SETQ NUB 0)
      (SETQ NLBPE 0)
      (SETQ NUBME 0)
      (SETQ NUML 0)
      (SETQ NUMU 0)
      (SETQ NUMAL (CDR ALP))
      (COND ((SETQ W (ATSOC 'LB NUMAL)) (SETQ NLB (CDR W))))
      (COND ((SETQ W (ATSOC 'UB NUMAL)) (SETQ NUB (CDR W))))
      (COND ((SETQ W (ATSOC 'LBPE NUMAL)) (SETQ NLBPE (CDR W))))
      (COND ((SETQ W (ATSOC 'UBME NUMAL)) (SETQ NUBME (CDR W))))
      (SETQ NUML (PLUS NLB NLBPE))
      (SETQ NUMU (PLUS NUB NUBME))
      (COND
       ((OR (LESSP NUMU NUML) (AND (EQUAL NUMU NUML) (LESSP NUBME NLBPE)))
        (SETQ TAKEU T)))
      (SETQ RESL
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E (CAR ALP))
               STARTOVER
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (E)
                           (OFSF_ROOTL2ETERML_MAREK (CAR E) (CDR E) TAKEU))
                         (CAR E)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ E (CDR E))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (E)
                           (OFSF_ROOTL2ETERML_MAREK (CAR E) (CDR E) TAKEU))
                         (CAR E)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ E (CDR E))
                (GO LOOPLABEL)))
      (COND (TAKEU (SETQ RESL (CONS (LIST 'OFSF_QESUBI (LIST 'PINF)) RESL)))
            (T (SETQ RESL (CONS (LIST 'OFSF_QESUBI (LIST 'MINF)) RESL))))
      (RETURN RESL))) 
(PUT 'OFSF_ROOTL2ETERML_MAREK 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_ROOTL2ETERML_MAREK 'DEFINED-ON-LINE '1389) 
(PUT 'OFSF_ROOTL2ETERML_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ROOTL2ETERML_MAREK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_ROOTL2ETERML_MAREK (BTYPE ROOTL TAKEU)
    (PROG (RES GUARD RDESC RTYPE ROOT)
      (PROG (E FORALL-RESULT FORALL-ENDPTR)
        (SETQ E ROOTL)
        (COND ((NULL E) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E)
                            (PROGN
                             (PROG (G191)
                               (SETQ G191 E)
                               (SETQ GUARD (CAR G191))
                               (SETQ RDESC (CDR G191))
                               (RETURN G191))
                             (PROG (G192)
                               (SETQ G192 RDESC)
                               (SETQ RTYPE (CAR G192))
                               (SETQ ROOT (CDR G192))
                               (RETURN G192))
                             (COND
                              ((EQ BTYPE 'EQUAL)
                               (PROGN
                                (COND
                                 ((EQ RTYPE 'REXP)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCR1
                                                 (LIST GUARD ROOT))
                                           RES))))
                                (COND
                                 ((EQ RTYPE 'QUOT)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCQ
                                                 (LIST GUARD ROOT))
                                           RES)))))))
                             (COND
                              ((EQ BTYPE 'NEQ)
                               (PROGN
                                (COND
                                 ((EQ RTYPE 'REXP)
                                  (SETQ RES
                                          (CONS
                                           (LIST
                                            (COND (TAKEU 'OFSF_QESUBCRME1)
                                                  (T 'OFSF_QESUBCRPE1))
                                            (LIST GUARD ROOT))
                                           RES))))
                                (COND
                                 ((EQ RTYPE 'QUOT)
                                  (SETQ RES
                                          (CONS
                                           (LIST
                                            (COND (TAKEU 'OFSF_QESUBCQME)
                                                  (T 'OFSF_QESUBCQPE))
                                            (LIST GUARD ROOT))
                                           RES)))))))
                             (COND
                              ((AND (EQ BTYPE 'LBPE) (NOT TAKEU))
                               (PROGN
                                (COND
                                 ((EQ RTYPE 'REXP)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCRPE1
                                                 (LIST GUARD ROOT))
                                           RES))))
                                (COND
                                 ((EQ RTYPE 'QUOT)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCQPE
                                                 (LIST GUARD ROOT))
                                           RES)))))))
                             (COND
                              ((AND (EQ BTYPE 'UBME) TAKEU)
                               (PROGN
                                (COND
                                 ((EQ RTYPE 'REXP)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCRME1
                                                 (LIST GUARD ROOT))
                                           RES))))
                                (COND
                                 ((EQ RTYPE 'QUOT)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCQME
                                                 (LIST GUARD ROOT))
                                           RES)))))))
                             (COND
                              ((OR (AND (EQ BTYPE 'LB) (NOT TAKEU))
                                   (AND (EQ BTYPE 'UB) TAKEU))
                               (PROGN
                                (COND
                                 ((EQ RTYPE 'REXP)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCR1
                                                 (LIST GUARD ROOT))
                                           RES))))
                                (COND
                                 ((EQ RTYPE 'QUOT)
                                  (SETQ RES
                                          (CONS
                                           (LIST 'OFSF_QESUBCQ
                                                 (LIST GUARD ROOT))
                                           RES)))))))))
                          (CAR E))
                         NIL)))
       LOOPLABEL
        (SETQ E (CDR E))
        (COND ((NULL E) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (E)
                    (PROGN
                     (PROG (G191)
                       (SETQ G191 E)
                       (SETQ GUARD (CAR G191))
                       (SETQ RDESC (CDR G191))
                       (RETURN G191))
                     (PROG (G192)
                       (SETQ G192 RDESC)
                       (SETQ RTYPE (CAR G192))
                       (SETQ ROOT (CDR G192))
                       (RETURN G192))
                     (COND
                      ((EQ BTYPE 'EQUAL)
                       (PROGN
                        (COND
                         ((EQ RTYPE 'REXP)
                          (SETQ RES
                                  (CONS (LIST 'OFSF_QESUBCR1 (LIST GUARD ROOT))
                                        RES))))
                        (COND
                         ((EQ RTYPE 'QUOT)
                          (SETQ RES
                                  (CONS (LIST 'OFSF_QESUBCQ (LIST GUARD ROOT))
                                        RES)))))))
                     (COND
                      ((EQ BTYPE 'NEQ)
                       (PROGN
                        (COND
                         ((EQ RTYPE 'REXP)
                          (SETQ RES
                                  (CONS
                                   (LIST
                                    (COND (TAKEU 'OFSF_QESUBCRME1)
                                          (T 'OFSF_QESUBCRPE1))
                                    (LIST GUARD ROOT))
                                   RES))))
                        (COND
                         ((EQ RTYPE 'QUOT)
                          (SETQ RES
                                  (CONS
                                   (LIST
                                    (COND (TAKEU 'OFSF_QESUBCQME)
                                          (T 'OFSF_QESUBCQPE))
                                    (LIST GUARD ROOT))
                                   RES)))))))
                     (COND
                      ((AND (EQ BTYPE 'LBPE) (NOT TAKEU))
                       (PROGN
                        (COND
                         ((EQ RTYPE 'REXP)
                          (SETQ RES
                                  (CONS
                                   (LIST 'OFSF_QESUBCRPE1 (LIST GUARD ROOT))
                                   RES))))
                        (COND
                         ((EQ RTYPE 'QUOT)
                          (SETQ RES
                                  (CONS
                                   (LIST 'OFSF_QESUBCQPE (LIST GUARD ROOT))
                                   RES)))))))
                     (COND
                      ((AND (EQ BTYPE 'UBME) TAKEU)
                       (PROGN
                        (COND
                         ((EQ RTYPE 'REXP)
                          (SETQ RES
                                  (CONS
                                   (LIST 'OFSF_QESUBCRME1 (LIST GUARD ROOT))
                                   RES))))
                        (COND
                         ((EQ RTYPE 'QUOT)
                          (SETQ RES
                                  (CONS
                                   (LIST 'OFSF_QESUBCQME (LIST GUARD ROOT))
                                   RES)))))))
                     (COND
                      ((OR (AND (EQ BTYPE 'LB) (NOT TAKEU))
                           (AND (EQ BTYPE 'UB) TAKEU))
                       (PROGN
                        (COND
                         ((EQ RTYPE 'REXP)
                          (SETQ RES
                                  (CONS (LIST 'OFSF_QESUBCR1 (LIST GUARD ROOT))
                                        RES))))
                        (COND
                         ((EQ RTYPE 'QUOT)
                          (SETQ RES
                                  (CONS (LIST 'OFSF_QESUBCQ (LIST GUARD ROOT))
                                        RES)))))))))
                  (CAR E))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (RETURN RES))) 
(PUT 'OFSF_ELIMSET_ORIG 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSET_ORIG 'DEFINED-ON-LINE '1430) 
(PUT 'OFSF_ELIMSET_ORIG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSET_ORIG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSET_ORIG (V ALP)
    (PROG (ATFAL W LPART QPART NPART)
      (COND (*RLQELOG (SETQ RLQELOG* (CONS (LIST V 0 0 0 0) RLQELOG*))))
      (SETQ ATFAL (CAR ALP))
      (COND
       ((AND (NULL (CDR ATFAL)) (EQUAL (CAAR ATFAL) 'ANYPOINT))
        (RETURN '((OFSF_QESUBCQ (TRUE (NIL . 1)))))))
      (COND
       ((OR (AND *RLQEANS *RLQEAPRECISE) (AND (NOT *RLQEANS) *RLQEPRECISE))
        (RETURN (OFSF_ELIMSET-PRECISE V ALP))))
      (SETQ W (OFSF_ELIMSETSCQ ATFAL))
      (COND
       (W
        (PROGN
         (COND
          ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
           (IOTO_PRIN2 "#q")))
         (RETURN W))))
      (SETQ W (OFSF_ELIMSETSCL ATFAL))
      (COND
       (W
        (PROGN
         (COND
          ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
           (IOTO_PRIN2 "#l")))
         (RETURN W))))
      (SETQ W (OFSF_ELIMSETLIN1S ATFAL))
      (SETQ LPART (CDR W))
      (SETQ QPART (OFSF_ELIMSETQUA ATFAL (CAR W)))
      (SETQ NPART (OFSF_ELIMSETNEQ ATFAL (CAR W)))
      (RETURN (LTO_NCONCN (LIST LPART QPART NPART))))) 
(PUT 'OFSF_ELIMSETSCQ 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETSCQ 'DEFINED-ON-LINE '1460) 
(PUT 'OFSF_ELIMSETSCQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETSCQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETSCQ (ATFAL)
    (PROG (W L A NZF ZERO D DFZERO HL)
      (COND ((NOT *RLQEQSC) (RETURN NIL)))
      (SETQ L '(NEQ21Q NEQ22R WO21Q WO22R SO21Q SO22R NEQ21R WO21R SO21R))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR L))
         (SETQ L (CDR L))
         (COND
          ((SETQ W (LTO_CATSOC A ATFAL))
           (COND
            ((OR NZF (AND (MEMQ A '(NEQ21R WO21R SO21R)) (CDDR W))
                 (AND (MEMQ A '(NEQ21Q NEQ22R WO21Q WO22R SO21Q SO22R))
                      (CDR W)))
             (PROGN (SETQ L NIL) (SETQ A 'FAILED)))
            (T
             (PROGN
              (SETQ ZERO (CAR W))
              (SETQ NZF (CAR (REVERSIP (EXPLODE A))))))))))
        (GO WHILELABEL))
      (COND ((OR (EQ A 'FAILED) (NULL NZF)) (RETURN NIL)))
      (COND ((EQUAL NZF 'Q) (SETQ DFZERO ZERO))
            (T
             (PROGN
              (SETQ ZERO (CADR ZERO))
              (SETQ D (CADDDR ZERO))
              (SETQ DFZERO
                      (LIST (LIST 'NEQ D NIL)
                            (OFSF_MKSOL1 D (NEGF (CAR ZERO))))))))
      (SETQ HL
              (LIST
               (CONS 'OFSF_QESUBCQ (CONS DFZERO (LTO_CATSOC 'EQUAL21Q ATFAL)))
               (CONS 'OFSF_QESUBCR2 (LTO_CATSOC 'EQUAL22R ATFAL))
               '(OFSF_QESUBI (PINF) (MINF))))
      (RETURN
       (LTO_NCONCN
        (LIST HL (OFSF_ELIMSETLINBS ATFAL) (OFSF_ELIMSETNEQBS ATFAL)))))) 
(PUT 'OFSF_SETVLIN 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_SETVLIN 'DEFINED-ON-LINE '1501) 
(PUT 'OFSF_SETVLIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SETVLIN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'OFSF_SETVLIN 'SMACRO
      '(LAMBDA ()
         (PROGN
          (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
          (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
          (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
          (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
          (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
          (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
          (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL))))) 
(PUT 'OFSF_ELIMSETLINBS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETLINBS 'DEFINED-ON-LINE '1514) 
(PUT 'OFSF_ELIMSETLINBS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETLINBS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETLINBS (ATFAL)
    (PROG (EQUAL1 LEQ1 GEQ1 GREATERP1 LESSP1 WO1 SO1 QESUBCQL QESUBCQMEL
           QESUBCQPEL)
      (PROGN
       (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
       (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
       (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
       (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
       (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
       (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
       (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL)))
      (SETQ QESUBCQL
              (CONS 'OFSF_QESUBCQ (LTO_NCONCN (LIST EQUAL1 LEQ1 GEQ1 WO1))))
      (SETQ QESUBCQMEL (CONS 'OFSF_QESUBCQME (LTO_NCONCN (LIST SO1 LESSP1))))
      (SETQ QESUBCQPEL
              (CONS 'OFSF_QESUBCQPE (LTO_NCONCN (LIST SO1 GREATERP1))))
      (RETURN (LIST QESUBCQL QESUBCQMEL QESUBCQPEL)))) 
(PUT 'OFSF_ELIMSETNEQBS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETNEQBS 'DEFINED-ON-LINE '1527) 
(PUT 'OFSF_ELIMSETNEQBS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETNEQBS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETNEQBS (ATFAL)
    (PROG (NEQ1 NEQ21Q NEQ21R NEQ22R)
      (SETQ NEQ1 (LTO_CATSOC 'NEQ1 ATFAL))
      (SETQ NEQ21Q (LTO_CATSOC 'NEQ21Q ATFAL))
      (SETQ NEQ22R (LTO_CATSOC 'NEQ22R ATFAL))
      (SETQ NEQ21R (LTO_CATSOC 'NEQ21R ATFAL))
      (RETURN
       (LIST (CONS 'OFSF_QESUBCQME (NCONC NEQ1 NEQ21Q))
             (CONS 'OFSF_QESUBCRME2 NEQ22R) (CONS 'OFSF_QESUBCRME1 NEQ21R)
             (CONS 'OFSF_QESUBCRPE1 NEQ21R)
             (CONS 'OFSF_QESUBCQPE (NCONC NEQ1 NEQ21Q))
             (CONS 'OFSF_QESUBCRPE2 NEQ22R))))) 
(PUT 'OFSF_SETVSCL 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_SETVSCL 'DEFINED-ON-LINE '1539) 
(PUT 'OFSF_SETVSCL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SETVSCL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'OFSF_SETVSCL 'SMACRO
      '(LAMBDA ()
         (PROGN
          (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
          (SETQ EQUAL21Q (LTO_CATSOC 'EQUAL21Q ATFAL))
          (SETQ EQUAL21R (LTO_CATSOC 'EQUAL21R ATFAL))
          (SETQ EQUAL22R (LTO_CATSOC 'EQUAL22R ATFAL))
          (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
          (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
          (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
          (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
          (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
          (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL))
          (SETQ O2P
                  (OR (LTO_CATSOC 'WO21Q ATFAL) (LTO_CATSOC 'WO21R ATFAL)
                      (LTO_CATSOC 'WO22R ATFAL) (LTO_CATSOC 'SO21Q ATFAL)
                      (LTO_CATSOC 'SO21R ATFAL) (LTO_CATSOC 'SO22R ATFAL)))))) 
(PUT 'OFSF_ELIMSETSCL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETSCL 'DEFINED-ON-LINE '1558) 
(PUT 'OFSF_ELIMSETSCL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETSCL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETSCL (ATFAL)
    (PROG (EQUAL1 EQUAL21Q EQUAL21R EQUAL22R LEQ1 GEQ1 GREATERP1 LESSP1 O2P NUB
           NLB INFSUBL WO1 SO1)
      (PROGN
       (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
       (SETQ EQUAL21Q (LTO_CATSOC 'EQUAL21Q ATFAL))
       (SETQ EQUAL21R (LTO_CATSOC 'EQUAL21R ATFAL))
       (SETQ EQUAL22R (LTO_CATSOC 'EQUAL22R ATFAL))
       (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
       (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
       (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
       (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
       (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
       (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL))
       (SETQ O2P
               (OR (LTO_CATSOC 'WO21Q ATFAL) (LTO_CATSOC 'WO21R ATFAL)
                   (LTO_CATSOC 'WO22R ATFAL) (LTO_CATSOC 'SO21Q ATFAL)
                   (LTO_CATSOC 'SO21R ATFAL) (LTO_CATSOC 'SO22R ATFAL))))
      (COND (O2P (RETURN NIL)))
      (SETQ NUB (NULL (OR LEQ1 LESSP1)))
      (SETQ NLB (NULL (OR GEQ1 GREATERP1)))
      (COND
       ((NULL (OR WO1 SO1))
        (COND (NUB (SETQ INFSUBL '(OFSF_QESUBI (PINF))))
              (NLB (SETQ INFSUBL '(OFSF_QESUBI (MINF))))))
       ((AND NUB NLB
             (OR (AND (NULL WO1) (NULL (CDR SO1)))
                 (AND (NULL SO1) (NULL (CDR WO1)))))
        (SETQ INFSUBL '(OFSF_QESUBI (PINF) (MINF)))))
      (COND
       (INFSUBL
        (RETURN
         (LIST INFSUBL (CONS 'OFSF_QESUBCR1 EQUAL21R)
               (CONS 'OFSF_QESUBCQ (NCONC EQUAL1 EQUAL21Q))
               (CONS 'OFSF_QESUBCR2 EQUAL22R))))))) 
(PUT 'OFSF_ELIMSETLIN1S 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETLIN1S 'DEFINED-ON-LINE '1589) 
(PUT 'OFSF_ELIMSETLIN1S 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETLIN1S 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETLIN1S (ATFAL)
    (PROG (EQUAL1 LEQ1 GEQ1 GREATERP1 LESSP1 WO1 SO1 QESUBCQL QESUBIL ESUBL L1N
           G1N)
      (SETQ L1N 0)
      (SETQ G1N 0)
      (PROGN
       (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
       (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
       (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
       (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
       (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
       (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
       (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL)))
      (COND
       (*RLQEFILTERBOUNDS
        (PROGN
         (SETQ LEQ1 (OFSF_FILTERBOUNDS LEQ1))
         (SETQ GEQ1 (OFSF_FILTERBOUNDS GEQ1))
         (SETQ LESSP1 (OFSF_FILTERBOUNDS LESSP1))
         (SETQ GREATERP1 (OFSF_FILTERBOUNDS GREATERP1)))))
      (SETQ L1N (PLUS (LENGTH LEQ1) (LENGTH LESSP1)))
      (SETQ G1N (PLUS (LENGTH GEQ1) (LENGTH GREATERP1)))
      (COND
       (*RLQELOG
        (PROGN
         (SETCAR (CDR (CAR RLQELOG*)) (PLUS (LENGTH GREATERP1) (LENGTH SO1)))
         (SETCAR (CDDR (CAR RLQELOG*)) (PLUS (LENGTH LESSP1) (LENGTH SO1)))
         NIL)))
      (COND
       ((LEQ L1N G1N)
        (PROGN
         (SETQ QESUBCQL
                 (CONS 'OFSF_QESUBCQ (LTO_NCONCN (LIST EQUAL1 LEQ1 WO1))))
         (SETQ ESUBL (CONS 'OFSF_QESUBCQME (NCONC SO1 LESSP1)))
         (COND
          (*RLQELOG (SETCAR (CDDDR (CAR RLQELOG*)) (CADDR (CAR RLQELOG*)))))
         (SETQ QESUBIL '(OFSF_QESUBI (PINF)))
         (RETURN (CONS NIL (LIST QESUBCQL ESUBL QESUBIL))))))
      (SETQ QESUBCQL (CONS 'OFSF_QESUBCQ (LTO_NCONCN (LIST EQUAL1 GEQ1 WO1))))
      (SETQ ESUBL (CONS 'OFSF_QESUBCQPE (NCONC SO1 GREATERP1)))
      (COND (*RLQELOG (SETCAR (CDDDR (CAR RLQELOG*)) (CADR (CAR RLQELOG*)))))
      (SETQ QESUBIL '(OFSF_QESUBI (MINF)))
      (RETURN (CONS T (LIST QESUBCQL ESUBL QESUBIL))))) 
(PUT 'OFSF_FILTERBOUNDS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_FILTERBOUNDS 'DEFINED-ON-LINE '1626) 
(PUT 'OFSF_FILTERBOUNDS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_FILTERBOUNDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_FILTERBOUNDS (L)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X)
                 (PROGN
                  (COND
                   ((OFSF_SUREP (LIST 'NOT (CAR X)) NIL)
                    (COND
                     ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
                      (IOTO_PRIN2 "(FB)"))))
                   (T (LIST X)))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X)
                 (PROGN
                  (COND
                   ((OFSF_SUREP (LIST 'NOT (CAR X)) NIL)
                    (COND
                     ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
                      (IOTO_PRIN2 "(FB)"))))
                   (T (LIST X)))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'OFSF_ELIMSETQUA 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSETQUA 'DEFINED-ON-LINE '1635) 
(PUT 'OFSF_ELIMSETQUA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETQUA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSETQUA (ATFAL PLE)
    (PROG (EQUAL21Q EQUAL22R WO21Q WO22R SO21Q SO22R QESUBCQL QESUBCR1L
           QESUBCR2L ESUBCQL ESUBCR1L ESUBCR2L EQUAL21R WO21R SO21R)
      (SETQ EQUAL21Q (LTO_CATSOC 'EQUAL21Q ATFAL))
      (SETQ EQUAL21R (LTO_CATSOC 'EQUAL21R ATFAL))
      (SETQ EQUAL22R (LTO_CATSOC 'EQUAL22R ATFAL))
      (SETQ WO21Q (LTO_CATSOC 'WO21Q ATFAL))
      (SETQ WO21R (LTO_CATSOC 'WO21R ATFAL))
      (SETQ WO22R (LTO_CATSOC 'WO22R ATFAL))
      (SETQ SO21Q (LTO_CATSOC 'SO21Q ATFAL))
      (SETQ SO21R (LTO_CATSOC 'SO21R ATFAL))
      (SETQ SO22R (LTO_CATSOC 'SO22R ATFAL))
      (COND
       (PLE
        (PROGN
         (SETQ ESUBCQL (CONS 'OFSF_QESUBCQPE SO21Q))
         (SETQ ESUBCR1L (CONS 'OFSF_QESUBCRPE1 SO21R))
         (SETQ ESUBCR2L (CONS 'OFSF_QESUBCRPE2 SO22R))))
       (T
        (PROGN
         (SETQ ESUBCQL (CONS 'OFSF_QESUBCQME SO21Q))
         (SETQ ESUBCR1L (CONS 'OFSF_QESUBCRME1 SO21R))
         (SETQ ESUBCR2L (CONS 'OFSF_QESUBCRME2 SO22R)))))
      (SETQ QESUBCQL (CONS 'OFSF_QESUBCQ (NCONC EQUAL21Q WO21Q)))
      (SETQ QESUBCR1L (CONS 'OFSF_QESUBCR1 (NCONC EQUAL21R WO21R)))
      (SETQ QESUBCR2L (CONS 'OFSF_QESUBCR2 (NCONC EQUAL22R WO22R)))
      (RETURN (LIST QESUBCQL QESUBCR1L QESUBCR2L ESUBCQL ESUBCR1L ESUBCR2L)))) 
(PUT 'OFSF_SETVNEQ 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_SETVNEQ 'DEFINED-ON-LINE '1667) 
(PUT 'OFSF_SETVNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SETVNEQ 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(PUTC 'OFSF_SETVNEQ 'SMACRO
      '(LAMBDA ()
         (PROGN
          (SETQ NEQ1 (LTO_CATSOC 'NEQ1 ATFAL))
          (SETQ NEQ21Q (LTO_CATSOC 'NEQ21Q ATFAL))
          (SETQ NEQ21R (LTO_CATSOC 'NEQ21R ATFAL))
          (SETQ NEQ22R (LTO_CATSOC 'NEQ22R ATFAL))
          (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
          (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
          (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
          (SETQ WO21Q (LTO_CATSOC 'WO21Q ATFAL))
          (SETQ WO21R (LTO_CATSOC 'WO21R ATFAL))
          (SETQ WO22R (LTO_CATSOC 'WO22R ATFAL))))) 
(PUT 'OFSF_ELIMSETNEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSETNEQ 'DEFINED-ON-LINE '1683) 
(PUT 'OFSF_ELIMSETNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETNEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSETNEQ (ATFAL PLE)
    (PROG (NEQ1 NEQ21Q NEQ21R NEQ22R LEQ1 GEQ1 WO1 WO21Q WO21R WO22R NEQN WBN
           ESUBCQ ESUBCR1 ESUBCR2 WB1)
      (PROGN
       (SETQ NEQ1 (LTO_CATSOC 'NEQ1 ATFAL))
       (SETQ NEQ21Q (LTO_CATSOC 'NEQ21Q ATFAL))
       (SETQ NEQ21R (LTO_CATSOC 'NEQ21R ATFAL))
       (SETQ NEQ22R (LTO_CATSOC 'NEQ22R ATFAL))
       (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
       (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
       (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
       (SETQ WO21Q (LTO_CATSOC 'WO21Q ATFAL))
       (SETQ WO21R (LTO_CATSOC 'WO21R ATFAL))
       (SETQ WO22R (LTO_CATSOC 'WO22R ATFAL)))
      (SETQ NEQN
              (PLUS (LENGTH NEQ1) (LENGTH NEQ21Q) (LENGTH NEQ21R)
                    (TIMES 2 (LENGTH NEQ22R))))
      (COND ((EQUAL NEQN 0) (RETURN NIL)))
      (SETQ WBN
              (PLUS (LENGTH WO1) (LENGTH WO21Q) (LENGTH WO21R)
                    (TIMES 2 (LENGTH WO22R))))
      (COND
       (PLE
        (PROGN
         (SETQ ESUBCQ 'OFSF_QESUBCQPE)
         (SETQ ESUBCR1 'OFSF_QESUBCRPE1)
         (SETQ ESUBCR2 'OFSF_QESUBCRPE2)
         (SETQ WB1 GEQ1)
         (SETQ WBN (PLUS WBN (LENGTH GEQ1)))))
       (T
        (PROGN
         (SETQ ESUBCQ 'OFSF_QESUBCQME)
         (SETQ ESUBCR1 'OFSF_QESUBCRME1)
         (SETQ ESUBCR2 'OFSF_QESUBCRME2)
         (SETQ WB1 LEQ1)
         (SETQ WBN (PLUS WBN (LENGTH LEQ1))))))
      (COND
       ((LESSP NEQN WBN)
        (RETURN
         (LIST (CONS ESUBCQ (NCONC NEQ1 NEQ21Q)) (CONS ESUBCR1 NEQ21R)
               (CONS ESUBCR2 NEQ22R)))))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 (LIST "(ANEQ:" NEQN "|" WBN ")"))))
      (RETURN
       (LIST (CONS ESUBCQ (LTO_NCONCN (LIST WB1 WO1 WO21Q)))
             (CONS ESUBCR1 WO21R) (CONS ESUBCR2 WO22R))))) 
(PUT 'OFSF_BETTERGAUSSP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_BETTERGAUSSP 'DEFINED-ON-LINE '1718) 
(PUT 'OFSF_BETTERGAUSSP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_BETTERGAUSSP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_BETTERGAUSSP (GRV1 GRV2)
    (PROG (W1 W2)
      (COND ((EQ (CAR GRV1) 'FAILED) (RETURN NIL)))
      (COND ((EQ (CAR GRV2) 'FAILED) (RETURN T)))
      (SETQ W1 (CADAR GRV1))
      (SETQ W2 (CADAR GRV2))
      (COND
       ((NEQ W1 W2)
        (RETURN (MEMQ W1 (CDR (MEMQ W2 '(FAC QUAR QUA2Q QUAQ LIN)))))))
      (SETQ W1 (CADDAR GRV1))
      (SETQ W2 (CADDAR GRV2))
      (COND ((NEQ W1 W2) (RETURN (MEMQ W1 (CDR (MEMQ W2 '(GEN TD CON)))))))
      (SETQ W1 (OFSF_ESETLENGTH (CADR GRV1)))
      (SETQ W2 (OFSF_ESETLENGTH (CADR GRV2)))
      (COND ((NEQ W1 W2) (RETURN (LESSP W1 W2))))
      (SETQ W1 (CADDAR GRV1))
      (SETQ W2 (CADDAR GRV2))
      (RETURN (MEMQ W1 (CDR (MEMQ W2 '(GEN TD CON))))))) 
(PUT 'OFSF_ESETLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ESETLENGTH 'DEFINED-ON-LINE '1745) 
(PUT 'OFSF_ESETLENGTH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ESETLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ESETLENGTH (E)
    (PROG (P FORALL-RESULT)
      (SETQ P E)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS
               ((LAMBDA (P)
                  (PROG (X FORALL-RESULT)
                    (SETQ X P)
                    (SETQ FORALL-RESULT 0)
                   LAB1
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (SETQ FORALL-RESULT
                            (PLUS ((LAMBDA (X) (LENGTH (CDR P))) (CAR X))
                                  FORALL-RESULT))
                    (SETQ X (CDR X))
                    (GO LAB1)))
                (CAR P))
               FORALL-RESULT))
      (SETQ P (CDR P))
      (GO LAB1))) 
(PUT 'OFSF_ESETUNION 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ESETUNION 'DEFINED-ON-LINE '1752) 
(PUT 'OFSF_ESETUNION 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ESETUNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ESETUNION (E1 E2) (LTO_ALUNION (LIST E1 E2))) 
(PUT 'OFSF_BESTGAUSSP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_BESTGAUSSP 'DEFINED-ON-LINE '1757) 
(PUT 'OFSF_BESTGAUSSP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_BESTGAUSSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_BESTGAUSSP (GRV)
    (AND (NOT (EQ (CAR GRV) 'FAILED)) (NOT (EQ (CAR GRV) 'GIGNORE))
         (EQ (CADAR GRV) 'LIN) (EQ (CADDAR GRV) 'CON) (NULL (CDR (CADR GRV)))
         (NULL (CDDAR (CADR GRV))))) 
(PUT 'OFSF_QEFSOLSET 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEFSOLSET 'DEFINED-ON-LINE '1765) 
(PUT 'OFSF_QEFSOLSET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEFSOLSET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEFSOLSET (A V THEO ANS BVL)
    (PROG (W K C)
      (COND ((NEQ (CAR A) 'EQUAL) (RETURN '(FAILED))))
      (COND
       (*RLBRKCXK
        (PROGN
         ((LAMBDA (*RLBRKCXK) (SETQ W (OFSF_VARLAT A))) NIL)
         (SETQ C T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND W C)) (RETURN NIL)))
           (PROGN
            (SETQ K (PROG1 (CAR W) (SETQ W (CDR W))))
            (COND ((AND (PAIRP K) (MEMQ V (LTO_LPVARL K))) (SETQ C NIL))))
           (GO WHILELABEL))
         (COND ((NOT C) (RETURN '(FAILED)))))))
      (SETQ W (OFSF_VARLAT A))
      (COND ((MEMQ V W) (RETURN (OFSF_FINDEQSOL A V THEO ANS BVL))))
      (COND
       ((AND *RLQEGEN (OFSF_VALASSP BVL (CADR A)))
        (RETURN (CONS 'GIGNORE (CONS NIL (LIST (LIST 'NEQ (CADR A) NIL)))))))
      (RETURN '(FAILED)))) 
(PUT 'OFSF_FINDEQSOL 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_FINDEQSOL 'DEFINED-ON-LINE '1791) 
(PUT 'OFSF_FINDEQSOL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_FINDEQSOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_FINDEQSOL (A V THEO ANS BVL)
    (PROG (W D THEOP TAG)
      (SETQ W (OFSF_PNONTRIVIAL (CADR A) V THEO BVL))
      (SETQ TAG (CAR W))
      (COND ((NOT TAG) (RETURN '(FAILED))))
      (COND ((CDR W) (SETQ THEOP (LIST (CDR W)))))
      (SETQ D (DEGREEF (CADR A) V))
      (SETQ W (OFSF_GELIMSET (OFSF_TRANSLAT A V THEO T ANS)))
      (COND ((EQ W 'FAILED) (RETURN '(FAILED))))
      (RETURN (CONS (OFSF_MKGTAG D TAG W THEO) (CONS W THEOP))))) 
(PUT 'OFSF_MKGTAG 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_MKGTAG 'DEFINED-ON-LINE '1814) 
(PUT 'OFSF_MKGTAG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKGTAG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKGTAG (D TAG ESET THEO)
    (PROG (W V)
      (SETQ W
              (COND ((EQUAL D 1) 'LIN) ((EQUAL D 2) (OFSF_MKGTAGQ ESET THEO))
                    (T 'FAC)))
      (SETQ V
              (COND ((EQUAL D 1) (SETQ V (CONS "l" V)))
                    ((EQUAL D 2) (SETQ V (CONS "q" V)))))
      (COND ((EQ TAG 'GEN) (SETQ V (CONS "!" V))))
      (RETURN (LIST V W TAG)))) 
(PUT 'OFSF_MKGTAGQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MKGTAGQ 'DEFINED-ON-LINE '1825) 
(PUT 'OFSF_MKGTAGQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MKGTAGQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MKGTAGQ (ESET THEO)
    (PROG (A)
      (SETQ A (OR (ATSOC 'OFSF_QESUBCR2 ESET) (ATSOC 'OFSF_QESUBCR1 ESET)))
      (COND
       (A
        (PROGN
         (COND ((NOT (NULL (CADR (CADR (CADR A))))) (RETURN 'QUAR)))
         (RETURN 'QUA2Q))))
      (COND ((NULL (CDR ESET)) (RETURN 'QUAQ)))
      (RETURN 'QUA2Q))) 
(PUT 'OFSF_GELIMSET 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GELIMSET 'DEFINED-ON-LINE '1841) 
(PUT 'OFSF_GELIMSET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_GELIMSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GELIMSET (ALP)
    (COND (*RLELIMSETOPTMAREK (OFSF_GELIMSET_MAREK ALP))
          (T (OFSF_GELIMSET_ORIG ALP)))) 
(PUT 'OFSF_GELIMSET_MAREK 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GELIMSET_MAREK 'DEFINED-ON-LINE '1847) 
(PUT 'OFSF_GELIMSET_MAREK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_GELIMSET_MAREK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GELIMSET_MAREK (ALP)
    (PROG (W RES)
      (SETQ W (CAR ALP))
      (COND ((EQUAL W 'FAILED) (RETURN 'FAILED)))
      (SETQ RES
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E W)
               STARTOVER
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (E)
                           (COND
                            ((EQ (CAR E) 'EQUAL)
                             (OFSF_ROOTL2ETERML_MAREK (CAR E) (CDR E) NIL))
                            (T (REDERR "BUG IN ofsf_gelimset_marek"))))
                         (CAR E)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ E (CDR E))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (E)
                           (COND
                            ((EQ (CAR E) 'EQUAL)
                             (OFSF_ROOTL2ETERML_MAREK (CAR E) (CDR E) NIL))
                            (T (REDERR "BUG IN ofsf_gelimset_marek"))))
                         (CAR E)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ E (CDR E))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'OFSF_GELIMSET_ORIG 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GELIMSET_ORIG 'DEFINED-ON-LINE '1859) 
(PUT 'OFSF_GELIMSET_ORIG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_GELIMSET_ORIG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GELIMSET_ORIG (ALP)
    (PROG (ESET)
      (SETQ ESET (CAR ALP))
      (COND ((EQUAL ESET 'FAILED) (RETURN 'FAILED)))
      (COND
       ((AND (NULL (CDR ESET)) (EQUAL (CAAR ESET) 'ANYPOINT))
        (RETURN (LIST (CONS 'OFSF_QESUBCQ (LIST '(TRUE (NIL . 1))))))))
      (PROG (X)
        (SETQ X ESET)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((MEMQ (CAR X) '(EQUAL1 EQUAL21Q)) (SETCAR X 'OFSF_QESUBCQ))
                 ((EQUAL (CAR X) 'EQUAL21R) (SETCAR X 'OFSF_QESUBCR1))
                 ((EQUAL (CAR X) 'EQUAL22R) (SETCAR X 'OFSF_QESUBCR2))
                 (T (REDERR "BUG IN ofsf_gelimset"))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN ESET))) 
(PUT 'OFSF_PNONTRIVIAL 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PNONTRIVIAL 'DEFINED-ON-LINE '1879) 
(PUT 'OFSF_PNONTRIVIAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_PNONTRIVIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PNONTRIVIAL (U V THEO BVL)
    (PROG (VCOEFFS)
      (SETQ VCOEFFS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (COEFFS (SFTO_REORDER U V)))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (REORDER X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (REORDER X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (OFSF_MAYBENONZEROL VCOEFFS THEO BVL)))) 
(PUT 'OFSF_MAYBENONZEROL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MAYBENONZEROL 'DEFINED-ON-LINE '1892) 
(PUT 'OFSF_MAYBENONZEROL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MAYBENONZEROL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MAYBENONZEROL (L THEO BVL)
    (PROG (W RESULT)
      (SETQ RESULT '(NIL))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ W (OFSF_MAYBENONZERO (CAR L) THEO BVL))
         (SETQ L (CDR L))
         (COND ((CAR W) (PROGN (SETQ RESULT W) (SETQ L NIL)))))
        (GO WHILELABEL))
      (RETURN RESULT))) 
(PUT 'OFSF_MAYBENONZERO 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MAYBENONZERO 'DEFINED-ON-LINE '1913) 
(PUT 'OFSF_MAYBENONZERO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MAYBENONZERO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MAYBENONZERO (U THEO BVL)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (COND ((NULL U) '(NIL)) (T '(CON))))
          ((EQ (CL_SIMPL (LIST 'EQUAL U NIL) THEO (MINUS 1)) 'FALSE) '(TD))
          ((AND *RLQELOCAL (NULL (SETDIFF (KERNELS U) CL_LPS*)))
           (OFSF_MAYBENONZERO-LOCAL U THEO BVL))
          ((AND *RLQEGEN (OFSF_VALASSP BVL U)) (CONS 'GEN (LIST 'NEQ U NIL)))
          (T '(NIL)))) 
(PUT 'OFSF_MAYBENONZERO-LOCAL 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_MAYBENONZERO-LOCAL 'DEFINED-ON-LINE '1934) 
(PUT 'OFSF_MAYBENONZERO-LOCAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MAYBENONZERO-LOCAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_MAYBENONZERO-LOCAL (U THEO BVL)
    (PROG (W)
      (SETQ W (CAR (SUBF U CL_PAL*)))
      (COND ((NULL W) (RETURN '(NIL))))
      (SETQ CL_THEO* (CONS (LIST 'NEQ U NIL) CL_THEO*))
      (RETURN (CONS 'GEN (LIST 'NEQ U NIL))))) 
(PUT 'OFSF_QEMKANS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QEMKANS 'DEFINED-ON-LINE '1943) 
(PUT 'OFSF_QEMKANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QEMKANS (AN)
    (PROG (RES TIME GCTIME)
      (SETQ TIME 0)
      (SETQ GCTIME 0)
      (COND (*RLVERBOSE (PROGN (SETQ TIME (TIME)) (SETQ GCTIME (GCTIME)))))
      (COND ((AND *RLQESTDANS (NOT *RLQEGEN)) (SETQ AN (OFSF_QEMKSTDANS AN))))
      (SETQ RES (OFSF_QEMKANS1 AN))
      (SETQ RES
              (COND (*RLQEBACKSUB (OFSF_QEBACKSUB RES))
                    (T (OFSF_QENOBACKSUB RES))))
      (COND (*RLQEBACKSUB (SETQ RES (SORT RES (FUNCTION ORDPCAR)))))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_PRIN2
          (LIST " ("
                (PLUS (DIFFERENCE (TIME) TIME) (DIFFERENCE (GCTIME) GCTIME))
                " ms)"))
         NIL)))
      (RETURN RES))) 
(PUT 'OFSF_QEMKSTDANS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QEMKSTDANS 'DEFINED-ON-LINE '1963) 
(PUT 'OFSF_QEMKSTDANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QEMKSTDANS (AN)
    (PROG (Y YY F V SUB XARGL NAN ANUNAN)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "++++ Determining standard real numbers for the answers "
               (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                 (SETQ Y AN)
                 (COND ((NULL Y) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL)))
                LOOPLABEL
                 (SETQ Y (CDR Y))
                 (COND ((NULL Y) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               "..."))))
      (PROG ()
       WHILELABEL
        (COND ((NOT AN) (RETURN NIL)))
        (PROGN
         (SETQ Y (PROG1 (CAR AN) (SETQ AN (CDR AN))))
         (PROG (G193 G194)
           (SETQ G193 Y)
           (SETQ G194 G193)
           (SETQ V (CAR G193))
           (SETQ G193 (CDR G193))
           (SETQ SUB (CAR G193))
           (SETQ G193 (CDR G193))
           (SETQ XARGL (CAR G193))
           (SETQ G193 (CDR G193))
           (SETQ F (CAR G193))
           (SETQ G193 (CDR G193))
           (RETURN G194))
         (COND
          ((EQ SUB 'ARBITRARY)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = arbitrary -> 0"))))
            (PROG (W1)
              (SETQ W1 (LIST V 'OFSF_QESUBCQ (LIST 'TRUE (CONS NIL 1))))
              (SETQ NAN (CONS W1 NAN))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_ARBITRARY2ANU)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_SHIFT-INDICATOR)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = shift"))))
            (PROG (W1)
              (SETQ W1 (LIST V SUB XARGL))
              (SETQ NAN (CONS W1 NAN))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1
                      (CONS V
                            (OFSF_SHIFT2ANU V (OFSF_EXTRACTID (CADR XARGL))
                             (CADDR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCQ)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = quotient"))))
            (SETQ YY (OFSF_QEMKSTDANSQFQ F NAN V XARGL ANUNAN))
            (COND
             (YY
              (PROGN
               (PROG (G195 G196)
                 (SETQ G195 YY)
                 (SETQ G196 G195)
                 (SETQ V (CAR G195))
                 (SETQ G195 (CDR G195))
                 (SETQ SUB (CAR G195))
                 (SETQ G195 (CDR G195))
                 (SETQ XARGL (CAR G195))
                 (SETQ G195 (CDR G195))
                 (RETURN G196))
               (COND
                (*RLVERBOSE
                 (IOTO_PRIN2
                  (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL))))))))))
            (PROG (W1)
              (SETQ W1 (LIST V SUB XARGL))
              (SETQ NAN (CONS W1 NAN))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCR1)
           (PROGN
            (COND (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = root"))))
            (SETQ YY (OFSF_QEMKSTDANSQFR F NAN V XARGL ANUNAN))
            (COND
             (YY
              (PROGN
               (PROG (G197 G198)
                 (SETQ G197 YY)
                 (SETQ G198 G197)
                 (SETQ V (CAR G197))
                 (SETQ G197 (CDR G197))
                 (SETQ SUB (CAR G197))
                 (SETQ G197 (CDR G197))
                 (SETQ XARGL (CAR G197))
                 (SETQ G197 (CDR G197))
                 (RETURN G198))
               (COND
                (*RLVERBOSE
                 (IOTO_PRIN2
                  (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
               (PROGN (SETQ NAN (CONS YY NAN)) YY)
               (PROG (W1)
                 (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
                 (SETQ ANUNAN (CONS W1 ANUNAN))
                 (RETURN W1))))
             (T
              (PROGN
               (PROG (W1)
                 (SETQ W1 (LIST V SUB XARGL))
                 (SETQ NAN (CONS W1 NAN))
                 (RETURN W1))
               (PROG (W1)
                 (SETQ W1 (CONS V (OFSF_R2ANU (CADR XARGL) ANUNAN)))
                 (SETQ ANUNAN (CONS W1 ANUNAN))
                 (RETURN W1)))))))
          ((EQ SUB 'OFSF_QESUBI)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = " (CAR XARGL)))))
            (PROG (G199 G200)
              (SETQ G199 (SETQ Y (OFSF_QEMKSTDANSINF F NAN V SUB XARGL)))
              (SETQ G200 G199)
              (SETQ V (CAR G199))
              (SETQ G199 (CDR G199))
              (SETQ SUB (CAR G199))
              (SETQ G199 (CDR G199))
              (SETQ XARGL (CAR G199))
              (SETQ G199 (CDR G199))
              (RETURN G200))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2
               (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
            (PROGN (SETQ NAN (CONS Y NAN)) Y)
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCQPE)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = quotient + epsilon"))))
            (PROG (G201 G202)
              (SETQ G201 (SETQ Y (OFSF_QEMKSTDANSQPE F NAN V XARGL ANUNAN)))
              (SETQ G202 G201)
              (SETQ V (CAR G201))
              (SETQ G201 (CDR G201))
              (SETQ SUB (CAR G201))
              (SETQ G201 (CDR G201))
              (SETQ XARGL (CAR G201))
              (SETQ G201 (CDR G201))
              (RETURN G202))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2
               (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
            (PROGN (SETQ NAN (CONS Y NAN)) Y)
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCQME)
           (PROGN
            (COND
             (*RLVERBOSE
              (IOTO_TPRIN2 (LIST "++++ " V " = quotient - epsilon"))))
            (PROG (G203 G204)
              (SETQ G203 (OFSF_QEMKSTDANSQME F NAN V XARGL ANUNAN))
              (SETQ G204 G203)
              (SETQ V (CAR G203))
              (SETQ G203 (CDR G203))
              (SETQ SUB (CAR G203))
              (SETQ G203 (CDR G203))
              (SETQ XARGL (CAR G203))
              (SETQ G203 (CDR G203))
              (RETURN G204))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2
               (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
            (PROG (W1)
              (SETQ W1 (LIST V SUB XARGL))
              (SETQ NAN (CONS W1 NAN))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCRPE1)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = root + epsilon"))))
            (PROG (G205 G206)
              (SETQ G205 (SETQ Y (OFSF_QEMKSTDANSRPE F NAN V XARGL ANUNAN)))
              (SETQ G206 G205)
              (SETQ V (CAR G205))
              (SETQ G205 (CDR G205))
              (SETQ SUB (CAR G205))
              (SETQ G205 (CDR G205))
              (SETQ XARGL (CAR G205))
              (SETQ G205 (CDR G205))
              (RETURN G206))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2
               (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
            (PROGN (SETQ NAN (CONS Y NAN)) Y)
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          ((EQ SUB 'OFSF_QESUBCRME1)
           (PROGN
            (COND
             (*RLVERBOSE (IOTO_TPRIN2 (LIST "++++ " V " = root - epsilon"))))
            (PROG (G207 G208)
              (SETQ G207 (OFSF_QEMKSTDANSRME F NAN V XARGL ANUNAN))
              (SETQ G208 G207)
              (SETQ V (CAR G207))
              (SETQ G207 (CDR G207))
              (SETQ SUB (CAR G207))
              (SETQ G207 (CDR G207))
              (SETQ XARGL (CAR G207))
              (SETQ G207 (CDR G207))
              (RETURN G208))
            (COND
             (*RLVERBOSE
              (IOTO_PRIN2
               (LIST " -> " (IOTO_FORM2STR (PREPSQ (CADR XARGL)))))))
            (PROG (W1)
              (SETQ W1 (LIST V SUB XARGL))
              (SETQ NAN (CONS W1 NAN))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS V (OFSF_Q2ANU (CADR XARGL) ANUNAN)))
              (SETQ ANUNAN (CONS W1 ANUNAN))
              (RETURN W1))))
          (T (REDERR "BUG IN ofsf_qemkstdans"))))
        (GO WHILELABEL))
      (RETURN (REVERSIP NAN)))) 
(PUT 'OFSF_QEMKSTDANSQFQ 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSQFQ 'DEFINED-ON-LINE '2056) 
(PUT 'OFSF_QEMKSTDANSQFQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSQFQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSQFQ (F NAN V XARGL ANUNAN)
    (PROG (W Q)
      (COND ((NOT *RLQESTDANSQ) (RETURN NIL)))
      (COND ((FLAGP V 'RL_QEANSVAR) (RETURN NIL)))
      (SETQ Q (OFSF_QEAPPLYNANQ (CADR XARGL) NAN))
      (COND
       ((AND (OR (ATOM (CAR Q)) (ATOM (CAR (CAR Q))))
             (OR (ATOM (CAR Q)) (ATOM (CAR (CAR Q)))))
        (RETURN NIL)))
      (SETQ W
              (OFSF_QEAPPLYNAN NAN
               (OFSF_QEAPPLYSUB 'OFSF_QESUBCQPE F V XARGL)))
      (COND (NIL NIL))
      (COND ((EQ W 'TRUE) (RETURN (OFSF_QEMKSTDANSQPE F NAN V XARGL ANUNAN))))
      (SETQ W
              (OFSF_QEAPPLYNAN NAN
               (OFSF_QEAPPLYSUB 'OFSF_QESUBCQME F V XARGL)))
      (COND (NIL NIL))
      (COND ((EQ W 'TRUE) (RETURN (OFSF_QEMKSTDANSQME F NAN V XARGL ANUNAN))))
      (RETURN NIL))) 
(PUT 'OFSF_QEMKSTDANSQFR 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSQFR 'DEFINED-ON-LINE '2076) 
(PUT 'OFSF_QEMKSTDANSQFR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSQFR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSQFR (F NAN V XARGL ANUNAN)
    (PROG (W Q)
      (COND ((NOT *RLQESTDANSQ) (RETURN NIL)))
      (COND ((FLAGP V 'RL_QEANSVAR) (RETURN NIL)))
      (SETQ Q (OFSF_QEAPPLYNANQ (OFSF_PREPREXPR (CADR XARGL)) NAN))
      (COND
       ((AND (OR (ATOM (CAR Q)) (ATOM (CAR (CAR Q))))
             (OR (ATOM (CAR Q)) (ATOM (CAR (CAR Q)))))
        (RETURN NIL)))
      (SETQ W
              (OFSF_QEAPPLYNAN NAN
               (OFSF_QEAPPLYSUB 'OFSF_QESUBCRPE1 F V XARGL)))
      (COND (NIL NIL))
      (COND ((EQ W 'TRUE) (RETURN (OFSF_QEMKSTDANSRPE F NAN V XARGL ANUNAN))))
      (SETQ W
              (OFSF_QEAPPLYNAN NAN
               (OFSF_QEAPPLYSUB 'OFSF_QESUBCRME1 F V XARGL)))
      (COND (NIL NIL))
      (COND ((EQ W 'TRUE) (RETURN (OFSF_QEMKSTDANSRME F NAN V XARGL ANUNAN))))
      (RETURN NIL))) 
(PUT 'OFSF_QEAPPLYNANQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QEAPPLYNANQ 'DEFINED-ON-LINE '2096) 
(PUT 'OFSF_QEAPPLYNANQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEAPPLYNANQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEAPPLYNANQ (Q NAN)
    (PROG (W NV NSUB NXARGL)
      (PROG (Y)
        (SETQ Y NAN)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (PROG (G209 G210)
              (SETQ G209 Y)
              (SETQ G210 G209)
              (SETQ NV (CAR G209))
              (SETQ G209 (CDR G209))
              (SETQ NSUB (CAR G209))
              (SETQ G209 (CDR G209))
              (SETQ NXARGL (CAR G209))
              (SETQ G209 (CDR G209))
              (RETURN G210))
            (COND (NIL NIL))
            (SETQ W
                    (COND
                     ((EQ NSUB 'OFSF_QESUBCR1)
                      (PREPSQ (OFSF_PREPREXPR (CADR NXARGL))))
                     (T (PREPSQ (CADR NXARGL)))))
            (SETQ Q (SUBSQ Q (LIST (CONS NV W))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN Q))) 
(PUT 'OFSF_MIRROR 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MIRROR 'DEFINED-ON-LINE '2110) 
(PUT 'OFSF_MIRROR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MIRROR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MIRROR (F V)
    (PROG (*RLPOS)
      (RETURN
       (CL_SIMPL (CL_APPLY2ATS1 F 'OFSF_MIRRORAT (LIST V)) NIL (MINUS 1))))) 
(PUT 'OFSF_MIRRORAT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MIRRORAT 'DEFINED-ON-LINE '2115) 
(PUT 'OFSF_MIRRORAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_MIRRORAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MIRRORAT (ATF V)
    (LIST (CAR ATF)
          (CAR
           (OFSF_SUBF (CADR ATF) V
            (NEGSQ (CONS (LIST (CONS (CONS V 1) 1)) 1))))
          NIL)) 
(PUT 'OFSF_ARBITRARY2ANU 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_ARBITRARY2ANU 'DEFINED-ON-LINE '2118) 
(PUT 'OFSF_ARBITRARY2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ARBITRARY2ANU 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OFSF_ARBITRARY2ANU NIL
    (ANU_MK (AEX_FROMSF (LIST (CONS (CONS (OFSF_MKSMALLID) 1) 1)))
     (IV_MK (CONS (MINUS 1) 1) (CONS 1 1)))) 
(PUT 'OFSF_SHIFT2ANU 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_SHIFT2ANU 'DEFINED-ON-LINE '2121) 
(PUT 'OFSF_SHIFT2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SHIFT2ANU 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SHIFT2ANU (V BASE DGCD ANUNAN)
    (PROG (W BASEVAR AVAR SGN AEX CB)
      (SETQ W (ATSOC BASE ANUNAN))
      (COND (NIL NIL))
      (SETQ BASE (CDR W))
      (SETQ BASEVAR (AEX_MVAR (ANU_DP BASE)))
      (SETQ AVAR (OFSF_MKSMALLID))
      (SETQ SGN
              (AEX_SGN
               (AEX_FROMSFIAL (LIST (CONS (CONS BASEVAR 1) 1))
                (LIST (CONS BASEVAR BASE)))))
      (COND
       ((EQN SGN 0)
        (RETURN
         (ANU_MK (AEX_FROMSF (LIST (CONS (CONS BASEVAR 1) 1)))
          (IV_MK (CONS (MINUS 1) 1) (CONS 1 1))))))
      (SETQ AEX
              (AEX_FROMSFIAL
               (ADDF (EXPTF (LIST (CONS (CONS AVAR 1) 1)) DGCD)
                     (NEGF (LIST (CONS (CONS BASEVAR 1) 1))))
               (LIST (CONS BASEVAR BASE))))
      (SETQ CB (ADDSQ (AEX_CAUCHYBOUND AEX AVAR) (CONS 1 1)))
      (COND (NIL NIL))
      (COND ((EQN SGN 1) (RETURN (ANU_MK AEX (IV_MK (CONS NIL 1) CB)))))
      (RETURN (ANU_MK AEX (IV_MK (NEGSQ CB) (CONS NIL 1)))))) 
(PUT 'OFSF_Q2ANU 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_Q2ANU 'DEFINED-ON-LINE '2140) 
(PUT 'OFSF_Q2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_Q2ANU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_Q2ANU (Q ANUNAN)
    (PROG (N D VL W ANUV SUBAL IAL AVAR AEX CB)
      (SETQ N (CAR Q))
      (SETQ D (CDR Q))
      (SETQ VL (UNION (KERNELS N) (KERNELS D)))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ W (ATSOC V ANUNAN))
            (COND (NIL NIL))
            (SETQ ANUV (AEX_MVAR (ANU_DP (CDR W))))
            (PROG (W1)
              (SETQ W1 (CONS V ANUV))
              (SETQ SUBAL (CONS W1 SUBAL))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS ANUV (CDR W)))
              (SETQ IAL (CONS W1 IAL))
              (RETURN W1))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ N (SFTO_RENAMEALF N SUBAL))
      (SETQ D (SFTO_RENAMEALF D SUBAL))
      (SETQ AVAR (OFSF_MKSMALLID))
      (SETQ AEX
              (AEX_PRPART
               (AEX_FROMSFIAL
                (ADDF
                 ((LAMBDA (G212)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF D G212))
                          (T (POLY-MULTF D G212))))
                  (LIST (CONS (CONS AVAR 1) 1)))
                 (NEGF N))
                IAL)))
      (COND (NIL NIL))
      (SETQ CB (AEX_CAUCHYBOUND AEX AVAR))
      (SETQ CB (ADDSQ CB (CONS 1 1)))
      (RETURN (ANU_MK AEX (IV_MK (NEGSQ CB) CB))))) 
(PUT 'OFSF_R2ANU 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_R2ANU 'DEFINED-ON-LINE '2162) 
(PUT 'OFSF_R2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_R2ANU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_R2ANU (R ANUNAN)
    (PROG (A B C D VL W ANUV SUBAL IAL SGN AVAR AVARF P AEX ROOTS SGND)
      (PROG (G213 G214)
        (SETQ G213 R)
        (SETQ G214 G213)
        (SETQ A (CAR G213))
        (SETQ G213 (CDR G213))
        (SETQ B (CAR G213))
        (SETQ G213 (CDR G213))
        (SETQ C (CAR G213))
        (SETQ G213 (CDR G213))
        (SETQ D (CAR G213))
        (SETQ G213 (CDR G213))
        (RETURN G214))
      (SETQ VL
              (LTO_UNIONN
               (LIST (KERNELS A) (KERNELS B) (KERNELS C) (KERNELS D))))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ W (ATSOC V ANUNAN))
            (COND (NIL NIL))
            (SETQ ANUV (AEX_MVAR (ANU_DP (CDR W))))
            (PROG (W1)
              (SETQ W1 (CONS V ANUV))
              (SETQ SUBAL (CONS W1 SUBAL))
              (RETURN W1))
            (PROG (W1)
              (SETQ W1 (CONS ANUV (CDR W)))
              (SETQ IAL (CONS W1 IAL))
              (RETURN W1))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ C (SFTO_RENAMEALF C SUBAL))
      (SETQ SGN (AEX_SGN (AEX_FROMSFIAL C IAL)))
      (COND
       ((EQN SGN 0)
        (RETURN (OFSF_Q2ANU (MULTSQ (CONS A 1) (INVSQ (CONS D 1))) ANUNAN))))
      (COND (NIL NIL))
      (SETQ B (SFTO_RENAMEALF B SUBAL))
      (SETQ SGN (AEX_SGN (AEX_FROMSFIAL B IAL)))
      (COND
       ((EQN SGN 0)
        (RETURN (OFSF_Q2ANU (MULTSQ (CONS A 1) (INVSQ (CONS D 1))) ANUNAN))))
      (SETQ A (SFTO_RENAMEALF A SUBAL))
      (SETQ D (SFTO_RENAMEALF D SUBAL))
      (SETQ AVAR (OFSF_MKSMALLID))
      (SETQ AVARF (LIST (CONS (CONS AVAR 1) 1)))
      (SETQ P
              (ADDF
               ((LAMBDA (G215 G216)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G215 G216))
                        (T (POLY-MULTF G215 G216))))
                (EXPTF D 2) (EXPTF AVARF 2))
               (ADDF
                (NEGF
                 ((LAMBDA (G220)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 G220))
                          (T (POLY-MULTF 2 G220))))
                  ((LAMBDA (G218)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF A G218))
                           (T (POLY-MULTF A G218))))
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF D AVARF))
                         (T (POLY-MULTF D AVARF))))))
                (ADDF (EXPTF A 2)
                      (NEGF
                       ((LAMBDA (G221)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G221 C))
                                (T (POLY-MULTF G221 C))))
                        (EXPTF B 2)))))))
      (SETQ AEX (AEX_PRPART (AEX_FROMSFIAL P IAL)))
      (SETQ ROOTS (AEX_FINDROOTS AEX AVAR))
      (COND (NIL NIL))
      (SETQ SGND (AEX_SGN (AEX_FROMSFIAL D IAL)))
      (COND ((LESSP (TIMES SGN SGND) 0) (RETURN (CAR ROOTS))))
      (RETURN (CADR ROOTS)))) 
(PUT 'AEX_FROMSFIAL 'NUMBER-OF-ARGS 2) 
(PUT 'AEX_FROMSFIAL 'DEFINED-ON-LINE '2199) 
(PUT 'AEX_FROMSFIAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'AEX_FROMSFIAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AEX_FROMSFIAL (F IAL)
    (PROG (RIAL AEX)
      (SETQ RIAL (REVERSE (SORT IAL (FUNCTION ORDOPCAR))))
      (SETQ AEX (AEX_FROMSF F))
      (PROG (PR)
        (SETQ PR RIAL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR) (SETQ AEX (AEX_BIND AEX (CAR PR) (CDR PR)))) (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (RETURN AEX))) 
(PUT 'AEX_PRPART 'NUMBER-OF-ARGS 1) 
(PUT 'AEX_PRPART 'DEFINED-ON-LINE '2208) 
(PUT 'AEX_PRPART 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'AEX_PRPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AEX_PRPART (AEX)
    (AEX_MK (CONS (SFTO_DPRPARTF (CAR (AEX_EX AEX))) 1) (AEX_CTX AEX))) 
(PUT 'ANU_FROMAEX 'NUMBER-OF-ARGS 1) 
(PUT 'ANU_FROMAEX 'DEFINED-ON-LINE '2211) 
(PUT 'ANU_FROMAEX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'ANU_FROMAEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANU_FROMAEX (AEX)
    (PROG (AVAR CB)
      (COND (NIL NIL))
      (SETQ AVAR (OFSF_MKSMALLID))
      (SETQ AEX
              (AEX_MK
               (ADDSQ (CONS (LIST (CONS (CONS AVAR 1) 1)) 1)
                      (NEGSQ (AEX_EX AEX)))
               (AEX_CTX AEX)))
      (COND (NIL NIL))
      (SETQ CB (AEX_CAUCHYBOUND AEX (AEX_MVAR AEX)))
      (SETQ CB (ADDSQ CB (CONS 1 1)))
      (RETURN (ANU_MK AEX (IV_MK (NEGSQ CB) CB))))) 
(PUT 'OFSF_QEAPPLYNAN 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QEAPPLYNAN 'DEFINED-ON-LINE '2222) 
(PUT 'OFSF_QEAPPLYNAN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEAPPLYNAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEAPPLYNAN (NAN F)
    (PROG (V SUB XARGL)
      (PROG (Y)
        (SETQ Y NAN)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (PROG (G223 G224)
              (SETQ G223 Y)
              (SETQ G224 G223)
              (SETQ V (CAR G223))
              (SETQ G223 (CDR G223))
              (SETQ SUB (CAR G223))
              (SETQ G223 (CDR G223))
              (SETQ XARGL (CAR G223))
              (SETQ G223 (CDR G223))
              (RETURN G224))
            (SETQ F (OFSF_QEAPPLYSUB SUB F V XARGL))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN F))) 
(PUT 'OFSF_QEAPPLYSUB 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QEAPPLYSUB 'DEFINED-ON-LINE '2231) 
(PUT 'OFSF_QEAPPLYSUB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEAPPLYSUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEAPPLYSUB (SUB F V XARGL)
    (PROG (RES)
      (SETQ RES
              (COND
               ((EQ SUB 'OFSF_SHIFT-INDICATOR)
                (OFSF_ANSSHIFT F V (OFSF_EXTRACTID (CADR XARGL))
                 (CADDR XARGL)))
               (T
                (CDR
                 (APPLY SUB (CONS NIL (CONS NIL (CONS F (CONS V XARGL)))))))))
      (RETURN (CL_SIMPL RES NIL (MINUS 1))))) 
(PUT 'OFSF_EXTRACTID 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_EXTRACTID 'DEFINED-ON-LINE '2240) 
(PUT 'OFSF_EXTRACTID 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_EXTRACTID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_EXTRACTID (Q) (CADR (CAAAR (CAR Q)))) 
(PUT 'OFSF_ANSSHIFT 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_ANSSHIFT 'DEFINED-ON-LINE '2243) 
(PUT 'OFSF_ANSSHIFT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ANSSHIFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_ANSSHIFT (F V ANSVAR DGCD)
    (OFSF_RENAME (OFSF_RETRANSFORM F V DGCD) V ANSVAR)) 
(PUT 'OFSF_RENAME 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_RENAME 'DEFINED-ON-LINE '2246) 
(PUT 'OFSF_RENAME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_RENAME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_RENAME (F V ANSVAR) (CL_APPLY2ATS1 F 'OFSF_RENAMEAT (LIST V ANSVAR))) 
(PUT 'OFSF_RENAMEAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_RENAMEAT 'DEFINED-ON-LINE '2249) 
(PUT 'OFSF_RENAMEAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_RENAMEAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_RENAMEAT (F VOLD VNEW)
    (LIST (COND ((ATOM F) F) (T (CAR F))) (SFTO_RENAMEF (CADR F) VOLD VNEW)
          NIL)) 
(PUT 'OFSF_QEMKSTDANSINF 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSINF 'DEFINED-ON-LINE '2252) 
(PUT 'OFSF_QEMKSTDANSINF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSINF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSINF (F NAN V SUB XARGL)
    (PROG (W)
      (SETQ F (OFSF_QEAPPLYNAN NAN F))
      (COND ((EQUAL (CAR XARGL) 'PINF) (SETQ W (OFSF_QEMKSTDANSPINF F V)))
            ((EQUAL (CAR XARGL) 'MINF)
             (SETQ W (NEGSQ (OFSF_QEMKSTDANSPINF (OFSF_MIRROR F V) V)))))
      (RETURN (LIST V 'OFSF_QESUBCQ (LIST 'TRUE W))))) 
(PUT 'OFSF_QEMKSTDANSPINF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_QEMKSTDANSPINF 'DEFINED-ON-LINE '2262) 
(PUT 'OFSF_QEMKSTDANSPINF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSPINF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSPINF (CSVF V)
    (PROG (NEQL OP NEEDSQ MAXSQ ONEQL NE)
      (PROG (ATF)
        (SETQ ATF (CL_ATL CSVF))
       LAB
        (COND ((NULL ATF) (RETURN NIL)))
        ((LAMBDA (ATF)
           (PROGN
            (SETQ OP (COND ((ATOM ATF) ATF) (T (CAR ATF))))
            (COND ((EQ OP 'NEQ) (PROGN (SETQ NEQL (CONS ATF NEQL)) ATF))
                  ((MEMQ OP '(GEQ GREATERP))
                   (PROGN
                    (SETQ NEEDSQ (OFSF_GUESSPINF ATF V))
                    (COND
                     ((OR (NOT MAXSQ) (SFTO_GREATERQ NEEDSQ MAXSQ))
                      (SETQ MAXSQ NEEDSQ))))))))
         (CAR ATF))
        (SETQ ATF (CDR ATF))
        (GO LAB))
      (COND ((NOT MAXSQ) (SETQ MAXSQ (CONS NIL 1))))
      (COND (*RLQESTDANSINT (SETQ MAXSQ (SFTO_CEILQ MAXSQ))))
      (SETQ ONEQL NEQL)
      (PROG ()
       WHILELABEL
        (COND ((NOT NEQL) (RETURN NIL)))
        (PROGN
         (SETQ NE (PROG1 (CAR NEQL) (SETQ NEQL (CDR NEQL))))
         (COND
          ((NULL (CAR (OFSF_SUBF (CADR NE) V MAXSQ)))
           (PROGN (SETQ MAXSQ (ADDSQ MAXSQ (CONS 1 1))) (SETQ NEQL ONEQL)))))
        (GO WHILELABEL))
      (RETURN MAXSQ))) 
(PUT 'OFSF_GUESSPINF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_GUESSPINF 'DEFINED-ON-LINE '2289) 
(PUT 'OFSF_GUESSPINF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_GUESSPINF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_GUESSPINF (ATF V)
    (PROG (OP LHS W)
      (SETQ OP (COND ((ATOM ATF) ATF) (T (CAR ATF))))
      (SETQ LHS (CADR ATF))
      (COND (NIL NIL))
      (SETQ W
              (COND
               ((EQN (CDAAR LHS) 1)
                (MULTSQ (CONS (NEGF (CDR LHS)) 1) (INVSQ (CONS (CDAR LHS) 1))))
               (T (SFTO_LMQ LHS))))
      (COND ((EQ OP 'GEQ) (RETURN W)))
      (COND ((EQ OP 'GREATERP) (RETURN (ADDSQ W (CONS 1 1)))))
      (REDERR (LIST "ofsf_guesspinf:" OP)))) 
(PUT 'OFSF_QEMKSTDANSQPE 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSQPE 'DEFINED-ON-LINE '2305) 
(PUT 'OFSF_QEMKSTDANSQPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSQPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSQPE (F NAN V XARGL ANUNAN)
    (PROG (Q ANU)
      (SETQ F (OFSF_QEAPPLYNAN NAN F))
      (SETQ Q (CADR XARGL))
      (SETQ ANU (OFSF_Q2ANU Q ANUNAN))
      (RETURN (OFSF_QEMKSTDANSAEXPE F V ANU)))) 
(PUT 'OFSF_QEMKSTDANSQME 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSQME 'DEFINED-ON-LINE '2313) 
(PUT 'OFSF_QEMKSTDANSQME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSQME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSQME (F NAN V XARGL ANUNAN)
    (PROG (FM Q ANU NV NSUB NXARGL)
      (SETQ F (OFSF_QEAPPLYNAN NAN F))
      (SETQ FM (OFSF_MIRROR F V))
      (SETQ Q (CADR XARGL))
      (SETQ ANU (OFSF_Q2ANU (NEGSQ Q) ANUNAN))
      (PROG (G225 G226)
        (SETQ G225 (OFSF_QEMKSTDANSAEXPE FM V ANU))
        (SETQ G226 G225)
        (SETQ NV (CAR G225))
        (SETQ G225 (CDR G225))
        (SETQ NSUB (CAR G225))
        (SETQ G225 (CDR G225))
        (SETQ NXARGL (CAR G225))
        (SETQ G225 (CDR G225))
        (RETURN G226))
      (SETQ NXARGL
              (CONS (CAR NXARGL) (CONS (NEGSQ (CADR NXARGL)) (CDDR NXARGL))))
      (RETURN (LIST NV NSUB NXARGL)))) 
(PUT 'OFSF_QEMKSTDANSRPE 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSRPE 'DEFINED-ON-LINE '2324) 
(PUT 'OFSF_QEMKSTDANSRPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSRPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSRPE (F NAN V XARGL ANUNAN)
    (OFSF_QEMKSTDANSRPE1 (OFSF_QEAPPLYNAN NAN F) V XARGL ANUNAN)) 
(PUT 'OFSF_QEMKSTDANSRPE1 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QEMKSTDANSRPE1 'DEFINED-ON-LINE '2327) 
(PUT 'OFSF_QEMKSTDANSRPE1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSRPE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSRPE1 (F V XARGL ANUNAN)
    (PROG (R ANU)
      (SETQ R (CADR XARGL))
      (SETQ ANU (OFSF_R2ANU R ANUNAN))
      (RETURN (OFSF_QEMKSTDANSAEXPE F V ANU)))) 
(PUT 'OFSF_QEMKSTDANSRME 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_QEMKSTDANSRME 'DEFINED-ON-LINE '2334) 
(PUT 'OFSF_QEMKSTDANSRME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSRME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSRME (F NAN V XARGL ANUNAN)
    (OFSF_QEMKSTDANSRME1 (OFSF_QEAPPLYNAN NAN F) V XARGL ANUNAN)) 
(PUT 'OFSF_QEMKSTDANSRME1 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_QEMKSTDANSRME1 'DEFINED-ON-LINE '2337) 
(PUT 'OFSF_QEMKSTDANSRME1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSRME1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSRME1 (F V XARGL ANUNAN)
    (PROG (A B C D ANU NV NSUB NXARGL)
      (SETQ F (OFSF_MIRROR F V))
      (PROG (G227 G228)
        (SETQ G227 (CADR XARGL))
        (SETQ G228 G227)
        (SETQ A (CAR G227))
        (SETQ G227 (CDR G227))
        (SETQ B (CAR G227))
        (SETQ G227 (CDR G227))
        (SETQ C (CAR G227))
        (SETQ G227 (CDR G227))
        (SETQ D (CAR G227))
        (SETQ G227 (CDR G227))
        (RETURN G228))
      (SETQ ANU (OFSF_R2ANU (LIST (NEGF A) (NEGF B) C D) ANUNAN))
      (PROG (G229 G230)
        (SETQ G229 (OFSF_QEMKSTDANSAEXPE F V ANU))
        (SETQ G230 G229)
        (SETQ NV (CAR G229))
        (SETQ G229 (CDR G229))
        (SETQ NSUB (CAR G229))
        (SETQ G229 (CDR G229))
        (SETQ NXARGL (CAR G229))
        (SETQ G229 (CDR G229))
        (RETURN G230))
      (SETQ NXARGL
              (CONS (CAR NXARGL) (CONS (NEGSQ (CADR NXARGL)) (CDDR NXARGL))))
      (RETURN (LIST NV NSUB NXARGL)))) 
(PUT 'OFSF_QEMKSTDANSAEXPE_OLD 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QEMKSTDANSAEXPE_OLD 'DEFINED-ON-LINE '2347) 
(PUT 'OFSF_QEMKSTDANSAEXPE_OLD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSAEXPE_OLD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSAEXPE_OLD (F V ANU)
    (PROG (ANUV CANUV MANUV QCA QMC Q AEX MANU OP LHS CANU C ROOTL FLAG)
      (COND (NIL NIL))
      (SETQ ANUV (AEX_MVAR (ANU_DP ANU)))
      (SETQ CANUV (OFSF_MKSMALLID))
      (SETQ MANUV (OFSF_MKSMALLID))
      (SETQ QCA
              (ADDSQ (CONS (LIST (CONS (CONS CANUV 1) 1)) 1)
                     (NEGSQ (CONS (LIST (CONS (CONS ANUV 1) 1)) 1))))
      (SETQ QMC
              (ADDSQ (CONS (LIST (CONS (CONS MANUV 1) 1)) 1)
                     (NEGSQ (CONS (LIST (CONS (CONS CANUV 1) 1)) 1))))
      (SETQ Q (IV_RB (ANU_IV ANU)))
      (SETQ AEX
              (AEX_FROMSF (CONS (CONS (CONS MANUV 1) (CDR Q)) (NEGF (CAR Q)))))
      (SETQ MANU
              (ANU_MK AEX
               (IV_MK (ADDSQ Q (NEGSQ (CONS 1 1))) (ADDSQ Q (CONS 1 1)))))
      (PROG (ATF)
        (SETQ ATF (CL_ATL F))
       LAB
        (COND ((NULL ATF) (RETURN NIL)))
        ((LAMBDA (ATF)
           (PROGN
            (SETQ OP (COND ((ATOM ATF) ATF) (T (CAR ATF))))
            (SETQ LHS (CADR ATF))
            (COND
             ((NOT (OR (ATOM LHS) (ATOM (CAR LHS))))
              (COND
               ((AND (MEMQ OP '(LESSP LEQ NEQ)) (EQN (CDAAR LHS) 1))
                (PROGN
                 (SETQ CANU (OFSF_LINSF2ANU LHS CANUV))
                 (SETQ MANU
                         (CDR
                          (OFSF_QEMKSTDANSUPDMIN QMC QCA MANUV MANU CANUV CANU
                           ANUV ANU)))))
               ((AND (MEMQ OP '(GREATERP GEQ NEQ)) (EQN (CDAAR LHS) 2))
                (PROGN
                 (SETQ CANU (OFSF_QUASF2ANU LHS CANUV 1))
                 (SETQ MANU
                         (CDR
                          (OFSF_QEMKSTDANSUPDMIN QMC QCA MANUV MANU CANUV CANU
                           ANUV ANU)))))
               ((AND (MEMQ OP '(LESSP LEQ)) (EQN (CDAAR LHS) 2))
                (PROGN
                 (SETQ CANU (OFSF_QUASF2ANU LHS CANUV 2))
                 (SETQ MANU
                         (CDR
                          (OFSF_QEMKSTDANSUPDMIN QMC QCA MANUV MANU CANUV CANU
                           ANUV ANU)))))
               ((GREATERP (CDAAR LHS) 2)
                (PROGN
                 (SETQ ROOTL
                         (AEX_FINDROOTS (AEX_FROMSF (SFTO_RENAMEF LHS V CANUV))
                          CANUV))
                 (SETQ C T)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND C ROOTL)) (RETURN NIL)))
                   (PROGN
                    (SETQ CANU (PROG1 (CAR ROOTL) (SETQ ROOTL (CDR ROOTL))))
                    (PROG (G231)
                      (SETQ G231
                              (OFSF_QEMKSTDANSUPDMIN QMC QCA MANUV MANU CANUV
                               CANU ANUV ANU))
                      (SETQ FLAG (CAR G231))
                      (SETQ MANU (CDR G231))
                      (RETURN G231))
                    (COND ((OR (EQ FLAG 'HIT) (EQ FLAG 'GMIN)) (SETQ C NIL))))
                   (GO WHILELABEL))
                 NIL)))))))
         (CAR ATF))
        (SETQ ATF (CDR ATF))
        (GO LAB))
      (RETURN (LIST V 'OFSF_QESUBCQ (LIST 'TRUE (OFSF_FINDRAT ANU MANU)))))) 
(PUT 'OFSF_QEMKSTDANSAEXPE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QEMKSTDANSAEXPE 'DEFINED-ON-LINE '2385) 
(PUT 'OFSF_QEMKSTDANSAEXPE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSAEXPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QEMKSTDANSAEXPE (F V ANU)
    (PROG (AEX SC Q Z AEXZ)
      (COND (NIL NIL))
      (SETQ AEX (ANU_DP ANU))
      (SETQ SC (AEX_STDSTURMCHAIN AEX (AEX_MVAR AEX)))
      (COND
       (*RLQESTDANSINT
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (SFTO_GREATERQ
               (ADDSQ (IV_RB (ANU_IV ANU)) (NEGSQ (IV_LB (ANU_IV ANU))))
               (CONS 1 2)))
             (RETURN NIL)))
           (ANU_REFINEIP ANU SC)
           (GO WHILELABEL))
         (SETQ Z (SFTO_CEILQ (IV_LB (ANU_IV ANU))))
         (COND
          ((NEQ Z (SFTO_CEILQ (IV_RB (ANU_IV ANU))))
           (COND
            ((NOT (EQN (AEX_SGN (AEX_SUBRAT AEX (AEX_MVAR AEX) Z)) 0))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (NEQ (SFTO_CEILQ (IV_LB (ANU_IV ANU)))
                       (SFTO_CEILQ (IV_RB (ANU_IV ANU)))))
                 (RETURN NIL)))
               (ANU_REFINEIP ANU SC)
               (GO WHILELABEL))))))
         (SETQ Q (SFTO_CEILQ (IV_RB (ANU_IV ANU))))))
       (T (SETQ Q (IV_RB (ANU_IV ANU)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQ (OFSF_SUBCONSTQ F V Q) 'FALSE)) (RETURN NIL)))
        (PROGN (ANU_REFINEIP ANU SC) (SETQ Q (IV_RB (ANU_IV ANU))))
        (GO WHILELABEL))
      (RETURN (LIST V 'OFSF_QESUBCQ (LIST 'TRUE Q))))) 
(PUT 'OFSF_SUBCONSTQ 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SUBCONSTQ 'DEFINED-ON-LINE '2408) 
(PUT 'OFSF_SUBCONSTQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUBCONSTQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBCONSTQ (F V Q)
    (CL_EVAL F (LIST (CONS V Q)) (FUNCTION OFSF_SUBCONSTAT))) 
(PUT 'OFSF_SUBCONSTAT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SUBCONSTAT 'DEFINED-ON-LINE '2412) 
(PUT 'OFSF_SUBCONSTAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SUBCONSTAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SUBCONSTAT (AT SUBAL)
    (COND
     ((OFSF_EVALATP (CAR AT)
       (CAR (OFSF_SUBF (CADR AT) (CAAR SUBAL) (CDAR SUBAL))))
      'TRUE)
     (T 'FALSE))) 
(PUT 'OFSF_FINDRAT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_FINDRAT 'DEFINED-ON-LINE '2415) 
(PUT 'OFSF_FINDRAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_FINDRAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_FINDRAT (ANU1 ANU2)
    (PROG (AEX1 AEX2 SC1 SC2)
      (COND
       ((SFTO_LESSQ (IV_RB (ANU_IV ANU1)) (IV_LB (ANU_IV ANU2)))
        (RETURN (IV_RB (ANU_IV ANU1)))))
      (SETQ AEX1 (ANU_DP ANU1))
      (SETQ AEX2 (ANU_DP ANU2))
      (SETQ SC1 (AEX_STDSTURMCHAIN AEX1 (AEX_MVAR AEX1)))
      (SETQ SC2 (AEX_STDSTURMCHAIN AEX2 (AEX_MVAR AEX2)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (SFTO_GEQQ (IV_RB (ANU_IV ANU1)) (IV_LB (ANU_IV ANU2))))
          (RETURN NIL)))
        (PROGN (ANU_REFINEIP ANU1 SC1) (ANU_REFINEIP ANU2 SC2))
        (GO WHILELABEL))
      (RETURN (IV_RB (ANU_IV ANU1))))) 
(PUT 'OFSF_QEMKSTDANSUPDMIN 'NUMBER-OF-ARGS 8) 
(PUT 'OFSF_QEMKSTDANSUPDMIN 'DEFINED-ON-LINE '2431) 
(PUT 'OFSF_QEMKSTDANSUPDMIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKSTDANSUPDMIN 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE OFSF_QEMKSTDANSUPDMIN (QMC QCA MANUV MANU CANUV CANU ANUV ANU)
    (PROG (AEX CDP IV MDP)
      (SETQ AEX
              (AEX_MK QCA
               (CTX_FROMIAL (LIST (CONS CANUV CANU) (CONS ANUV ANU)))))
      (COND ((LESSP (AEX_SGN AEX) 1) (RETURN (CONS 'LANU MANU))))
      (SETQ AEX
              (AEX_MK QMC
               (CTX_FROMIAL (LIST (CONS MANUV MANU) (CONS CANUV CANU)))))
      (COND ((LESSP (AEX_SGN AEX) 1) (RETURN (CONS 'GMIN MANU))))
      (SETQ CDP (AEX_EX (ANU_DP CANU)))
      (SETQ IV (ANU_IV CANU))
      (SETQ MDP (CONS (SFTO_RENAMEF (CAR CDP) CANUV MANUV) (CDR CDP)))
      (RETURN (CONS 'HIT (ANU_MK (AEX_FROMRP MDP) IV))))) 
(PUT 'OFSF_LINSF2ANU 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_LINSF2ANU 'DEFINED-ON-LINE '2447) 
(PUT 'OFSF_LINSF2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_LINSF2ANU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_LINSF2ANU (F V)
    (PROG (AEX Q)
      (SETQ F (SFTO_DPRPARTF F))
      (SETQ AEX (AEX_FROMSF (CONS (CONS (CONS V 1) (CDAR F)) (CDR F))))
      (SETQ Q (MULTSQ (CONS (NEGF (CDR F)) 1) (INVSQ (CONS (CDAR F) 1))))
      (RETURN
       (ANU_MK AEX (IV_MK (ADDSQ Q (NEGSQ (CONS 1 1))) (ADDSQ Q (CONS 1 1))))))) 
(PUT 'OFSF_QUASF2ANU 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_QUASF2ANU 'DEFINED-ON-LINE '2455) 
(PUT 'OFSF_QUASF2ANU 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QUASF2ANU 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_QUASF2ANU (F V ROOTNO)
    (PROG (AEX MID A B LB UB)
      (SETQ F (SFTO_SQFPARTF F))
      (COND ((EQN (CDAAR F) 1) (RETURN (OFSF_LINSF2ANU F V))))
      (SETQ A (CDAR F))
      (SETQ B
              (COND
               ((NOT (OR (ATOM (CDR F)) (ATOM (CAR (CDR F)))))
                (CDAR (CDR F)))))
      (SETQ MID
              (MULTSQ (CONS (NEGF B) 1)
                      (INVSQ
                       (CONS
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 A))
                              (T (POLY-MULTF 2 A)))
                        1))))
      (SETQ LB (NEGSQ (SFTO_LMQ (SFTO_MIRROR F))))
      (SETQ UB (SFTO_LMQ F))
      (SETQ AEX (AEX_FROMSF (SFTO_RENAMEF F (CAAAR F) V)))
      (COND ((EQN ROOTNO 1) (RETURN (ANU_MK AEX (IV_MK LB MID)))))
      (RETURN (ANU_MK AEX (IV_MK MID UB))))) 
(PUT 'OFSF_QEMKANS1 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QEMKANS1 'DEFINED-ON-LINE '2471) 
(PUT 'OFSF_QEMKANS1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEMKANS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QEMKANS1 (AN)
    (PROG (V SUB XARGL W IC EC AC)
      (SETQ IC 0)
      (SETQ EC 0)
      (SETQ AC 0)
      (RETURN
       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
         (SETQ Y AN)
         (COND ((NULL Y) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (Y)
                             (PROGN
                              (SETQ V (CAR Y))
                              (SETQ SUB (CADR Y))
                              (SETQ XARGL (CADDR Y))
                              (SETQ W
                                      (COND
                                       ((EQ SUB 'OFSF_QESUBI)
                                        (PROGN
                                         (COND
                                          ((EQUAL (CAR XARGL) 'PINF)
                                           (SIMP
                                            (OFSF_NEWINFINITY
                                             (SETQ IC (PLUS IC 1)))))
                                          ((EQUAL (CAR XARGL) 'MINF)
                                           (NEGSQ
                                            (SIMP
                                             (OFSF_NEWINFINITY
                                              (SETQ IC (PLUS IC 1)))))))))
                                       ((OR (EQ SUB 'OFSF_QESUBCQ)
                                            (EQ SUB 'OFSF_SHIFT-INDICATOR))
                                        (CADR XARGL))
                                       ((EQ SUB 'OFSF_QESUBCR1)
                                        (OFSF_PREPREXPR (CADR XARGL)))
                                       ((EQ SUB 'OFSF_QESUBCQME)
                                        (ADDSQ (CADR XARGL)
                                               (NEGSQ
                                                (SIMP
                                                 (OFSF_NEWEPSILON
                                                  (SETQ EC (PLUS EC 1)))))))
                                       ((EQ SUB 'OFSF_QESUBCQPE)
                                        (ADDSQ (CADR XARGL)
                                               (SIMP
                                                (OFSF_NEWEPSILON
                                                 (SETQ EC (PLUS EC 1))))))
                                       ((EQ SUB 'OFSF_QESUBCRME1)
                                        (ADDSQ (OFSF_PREPREXPR (CADR XARGL))
                                               (NEGSQ
                                                (SIMP
                                                 (OFSF_NEWEPSILON
                                                  (SETQ EC (PLUS EC 1)))))))
                                       ((EQ SUB 'OFSF_QESUBCRPE1)
                                        (ADDSQ (OFSF_PREPREXPR (CADR XARGL))
                                               (SIMP
                                                (OFSF_NEWEPSILON
                                                 (SETQ EC (PLUS EC 1))))))
                                       ((EQ SUB 'ARBITRARY)
                                        (SIMP
                                         (OFSF_NEWARBITRARY
                                          (SETQ AC (PLUS AC 1)))))
                                       (T (REDERR "BUG IN ofsf_qemkans"))))
                              (CONS V W)))
                           (CAR Y))
                          NIL)))
        LOOPLABEL
         (SETQ Y (CDR Y))
         (COND ((NULL Y) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (Y)
                     (PROGN
                      (SETQ V (CAR Y))
                      (SETQ SUB (CADR Y))
                      (SETQ XARGL (CADDR Y))
                      (SETQ W
                              (COND
                               ((EQ SUB 'OFSF_QESUBI)
                                (PROGN
                                 (COND
                                  ((EQUAL (CAR XARGL) 'PINF)
                                   (SIMP
                                    (OFSF_NEWINFINITY (SETQ IC (PLUS IC 1)))))
                                  ((EQUAL (CAR XARGL) 'MINF)
                                   (NEGSQ
                                    (SIMP
                                     (OFSF_NEWINFINITY
                                      (SETQ IC (PLUS IC 1)))))))))
                               ((OR (EQ SUB 'OFSF_QESUBCQ)
                                    (EQ SUB 'OFSF_SHIFT-INDICATOR))
                                (CADR XARGL))
                               ((EQ SUB 'OFSF_QESUBCR1)
                                (OFSF_PREPREXPR (CADR XARGL)))
                               ((EQ SUB 'OFSF_QESUBCQME)
                                (ADDSQ (CADR XARGL)
                                       (NEGSQ
                                        (SIMP
                                         (OFSF_NEWEPSILON
                                          (SETQ EC (PLUS EC 1)))))))
                               ((EQ SUB 'OFSF_QESUBCQPE)
                                (ADDSQ (CADR XARGL)
                                       (SIMP
                                        (OFSF_NEWEPSILON
                                         (SETQ EC (PLUS EC 1))))))
                               ((EQ SUB 'OFSF_QESUBCRME1)
                                (ADDSQ (OFSF_PREPREXPR (CADR XARGL))
                                       (NEGSQ
                                        (SIMP
                                         (OFSF_NEWEPSILON
                                          (SETQ EC (PLUS EC 1)))))))
                               ((EQ SUB 'OFSF_QESUBCRPE1)
                                (ADDSQ (OFSF_PREPREXPR (CADR XARGL))
                                       (SIMP
                                        (OFSF_NEWEPSILON
                                         (SETQ EC (PLUS EC 1))))))
                               ((EQ SUB 'ARBITRARY)
                                (SIMP
                                 (OFSF_NEWARBITRARY (SETQ AC (PLUS AC 1)))))
                               (T (REDERR "BUG IN ofsf_qemkans"))))
                      (CONS V W)))
                   (CAR Y))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_NEWINFINITY 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NEWINFINITY 'DEFINED-ON-LINE '2507) 
(PUT 'OFSF_NEWINFINITY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_NEWINFINITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NEWINFINITY (IC) (MKID 'INFINITY IC)) 
(PUT 'OFSF_NEWEPSILON 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NEWEPSILON 'DEFINED-ON-LINE '2510) 
(PUT 'OFSF_NEWEPSILON 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_NEWEPSILON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NEWEPSILON (EC)
    (PROG (EPS)
      (SETQ EPS (MKID 'EPSILON EC))
      (FLAG (LIST EPS) 'CONSTANT)
      (PUT EPS '|:RD:| 'RDZERO*)
      (RETURN EPS))) 
(PUT 'OFSF_NEWARBITRARY 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_NEWARBITRARY 'DEFINED-ON-LINE '2518) 
(PUT 'OFSF_NEWARBITRARY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_NEWARBITRARY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_NEWARBITRARY (AC) (MKID 'ARBITRARY AC)) 
(PUT 'OFSF_QEBACKSUB 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QEBACKSUB 'DEFINED-ON-LINE '2521) 
(PUT 'OFSF_QEBACKSUB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QEBACKSUB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QEBACKSUB (EQL)
    (PROG (SUBL E)
      (RETURN
       (PROG (W FORALL-RESULT FORALL-ENDPTR)
         (SETQ W EQL)
        STARTOVER
         (COND ((NULL W) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (W)
                    (PROGN
                     (SETQ E (CONS (CAR W) (PREPSQ (SUBSQ (CDR W) SUBL))))
                     (PROGN (SETQ SUBL (CONS E SUBL)) E)
                     (COND
                      ((OR *RLQEFULLANS (NOT (FLAGP (CAR W) 'RL_QEANSVAR)))
                       (LIST E)))))
                  (CAR W)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ W (CDR W))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL W) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (W)
                    (PROGN
                     (SETQ E (CONS (CAR W) (PREPSQ (SUBSQ (CDR W) SUBL))))
                     (PROGN (SETQ SUBL (CONS E SUBL)) E)
                     (COND
                      ((OR *RLQEFULLANS (NOT (FLAGP (CAR W) 'RL_QEANSVAR)))
                       (LIST E)))))
                  (CAR W)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ W (CDR W))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_QENOBACKSUB 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_QENOBACKSUB 'DEFINED-ON-LINE '2532) 
(PUT 'OFSF_QENOBACKSUB 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_QENOBACKSUB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_QENOBACKSUB (EQL)
    (PROG (W FORALL-RESULT FORALL-ENDPTR)
      (SETQ W EQL)
     STARTOVER
      (COND ((NULL W) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (W)
                 (COND
                  ((OR *RLQEFULLANS (NOT (FLAGP (CAR W) 'RL_QEANSVAR)))
                   (LIST (CONS (CAR W) (PREPSQ (CDR W)))))))
               (CAR W)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ W (CDR W))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL W) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (W)
                 (COND
                  ((OR *RLQEFULLANS (NOT (FLAGP (CAR W) 'RL_QEANSVAR)))
                   (LIST (CONS (CAR W) (PREPSQ (CDR W)))))))
               (CAR W)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ W (CDR W))
      (GO LOOPLABEL))) 
(PUT 'OFSF_CROOT 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CROOT 'DEFINED-ON-LINE '2541) 
(PUT 'OFSF_CROOT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CROOT (U N)
    (COND ((EQN N 1) U) (T (REVAL1 (LIST 'EXPT U (LIST 'QUOTIENT 1 N)) T)))) 
(PUT 'OFSF_PREPREXPR 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PREPREXPR 'DEFINED-ON-LINE '2544) 
(PUT 'OFSF_PREPREXPR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_PREPREXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_PREPREXPR (R)
    (MULTSQ
     (CONS
      (ADDF (CAR R)
            ((LAMBDA (G233)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CADR R) G233))
                     (T (POLY-MULTF (CADR R) G233))))
             (CAR (SIMP (LIST 'SQRT (PREPF (CADDR R)))))))
      1)
     (INVSQ (CONS (CADDDR R) 1)))) 
(PUT 'OFSF_THSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THSIMPL 'DEFINED-ON-LINE '2548) 
(PUT 'OFSF_THSIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THSIMPL (ATL)
    (PROG (*RLSIEXPLA *RLSIPO)
      (SETQ *RLSIEXPLA T)
      (RETURN
       (SORT
        (OFSF_THREGEN
         (CL_SIMPL
          (COND ((AND ATL (CDR ATL)) (CONS 'AND ATL))
                ((NULL ATL) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR ATL)))
          NIL (MINUS 1)))
        'RL_ORDATP)))) 
(PUT 'OFSF_THREGEN 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THREGEN 'DEFINED-ON-LINE '2557) 
(PUT 'OFSF_THREGEN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THREGEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THREGEN (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND
       ((EQUAL OP 'AND)
        (RETURN
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X (CDR F))
           (COND ((NULL X) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (OFSF_THREGEN-OR X)) (CAR X))
                                 NIL)))
          LOOPLABEL
           (SETQ X (CDR X))
           (COND ((NULL X) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (X) (OFSF_THREGEN-OR X)) (CAR X)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (COND ((EQUAL OP 'OR) (RETURN (LIST (OFSF_THREGEN-OR F)))))
      (COND ((EQUAL OP 'TRUE) (RETURN NIL)))
      (COND ((EQUAL OP 'FALSE) (LIST 'FALSE)))
      (RETURN (LIST F)))) 
(PUT 'OFSF_THREGEN-AND 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THREGEN-AND 'DEFINED-ON-LINE '2576) 
(PUT 'OFSF_THREGEN-AND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THREGEN-AND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THREGEN-AND (F) (CL_NNFNOT (OFSF_THREGEN-OR (CL_NNFNOT F)))) 
(PUT 'OFSF_THREGEN-OR 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THREGEN-OR 'DEFINED-ON-LINE '2582) 
(PUT 'OFSF_THREGEN-OR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THREGEN-OR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THREGEN-OR (F)
    (PROG (W)
      (COND ((CL_ATFP F) (RETURN F)))
      (SETQ W (CAR (CDR F)))
      (COND
       ((EQUAL (COND ((ATOM W) W) (T (CAR W))) 'AND)
        (SETQ W (OFSF_THREGEN-AND W))))
      (COND
       ((EQUAL (COND ((ATOM W) W) (T (CAR W))) 'EQUAL)
        (RETURN (OFSF_THREGEN-EQUAL (CONS W (CDR (CDR F)))))))
      (COND
       ((EQUAL (COND ((ATOM W) W) (T (CAR W))) 'NEQ)
        (RETURN (OFSF_THREGEN-NEQ (CONS W (CDR (CDR F)))))))
      (REDERR "BUG IN ofsf_thregen!-or"))) 
(PUT 'OFSF_THREGEN-EQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THREGEN-EQUAL 'DEFINED-ON-LINE '2599) 
(PUT 'OFSF_THREGEN-EQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THREGEN-EQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THREGEN-EQUAL (EQL)
    (PROG (W)
      (SETQ W 1)
      (PROG (X)
        (SETQ X EQL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((EQUAL (COND ((ATOM X) X) (T (CAR X))) 'AND)
              (SETQ X (OFSF_THREGEN-AND X))))
            (COND
             ((NEQ (COND ((ATOM X) X) (T (CAR X))) 'EQUAL)
              (REDERR "BUG IN ofsf_thregen!-equal")))
            (SETQ W
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF W (CADR X)))
                          (T (POLY-MULTF W (CADR X)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (LIST 'EQUAL W NIL)))) 
(PUT 'OFSF_THREGEN-NEQ 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_THREGEN-NEQ 'DEFINED-ON-LINE '2618) 
(PUT 'OFSF_THREGEN-NEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_THREGEN-NEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_THREGEN-NEQ (NEQL)
    (PROG (W)
      (PROG (X)
        (SETQ X NEQL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((EQUAL (COND ((ATOM X) X) (T (CAR X))) 'AND)
              (SETQ X (OFSF_THREGEN-AND X))))
            (COND
             ((NEQ (COND ((ATOM X) X) (T (CAR X))) 'NEQ)
              (REDERR "BUG IN ofsf_thregen!-neq")))
            (SETQ W (ADDF W (EXPTF (CADR X) 2)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (LIST 'NEQ W NIL)))) 
(PUT 'OFSF_SPECELIM 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_SPECELIM 'DEFINED-ON-LINE '2636) 
(PUT 'OFSF_SPECELIM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SPECELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SPECELIM (F VL THEO ANS BVL)
    (COND ((OR (NOT *RLQESQSC) ANS *RLQEGEN) 'FAILED)
          (T (OFSF_SQSC F VL THEO ANS BVL)))) 
(PUT 'OFSF_SQSC 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_SQSC 'DEFINED-ON-LINE '2643) 
(PUT 'OFSF_SQSC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SQSC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SQSC (F VL THEO ANS BVL)
    (PROG (ATL SCVL LIN A AT)
      (SETQ ATL (CL_ATL1 F))
      (SETQ SCVL (COND (*RLQEVARSEL VL) (T (LIST (CAR VL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT LIN))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (SETQ LIN (OFSF_LINP ATL A (LTO_DELQ A VL))))
        (GO WHILELABEL))
      (COND (LIN (RETURN 'FAILED)))
      (SETQ SCVL (COND (*RLQEVARSEL VL) (T (LIST (CAR VL)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCVL (NOT AT))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SCVL))
         (SETQ SCVL (CDR SCVL))
         (SETQ AT (OFSF_SQSC-TEST ATL A)))
        (GO WHILELABEL))
      (COND ((NOT AT) (RETURN 'FAILED)))
      (COND
       ((AND *RLVERBOSE *RLQEVB (OR (NOT *RLQEDFS) *RLQEVBOLD))
        (IOTO_PRIN2 "#Q")))
      (SETQ VL (LTO_DELQ A VL))
      (SETQ F (CL_SIMPL (OFSF_SQSC1 F AT A THEO) THEO (MINUS 1)))
      (RETURN (CONS (CONS T (LIST (CL_MKCE VL F NIL NIL))) THEO)))) 
(PUT 'OFSF_SQSC1 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_SQSC1 'DEFINED-ON-LINE '2670) 
(PUT 'OFSF_SQSC1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SQSC1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SQSC1 (F AT V THEO)
    (COND
     ((CL_CXFP F)
      (CONS (COND ((ATOM F) F) (T (CAR F)))
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (CDR F))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X) (OFSF_SQSC1 X AT V THEO)) (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (OFSF_SQSC1 X AT V THEO)) (CAR X))
                            NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     ((EQ F AT) (OFSF_SQSC1AT AT V THEO)) (T F))) 
(PUT 'OFSF_SQSC1AT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_SQSC1AT 'DEFINED-ON-LINE '2678) 
(PUT 'OFSF_SQSC1AT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SQSC1AT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_SQSC1AT (AT V THEO)
    (PROG (OP W A B C DISCR)
      (SETQ OP (CAR AT))
      (SETQ W (OFSF_MKTRIPLE (SFTO_REORDER (CADR AT) V)))
      (SETQ A (REORDER (CAR W)))
      (SETQ B (REORDER (CADR W)))
      (SETQ C (REORDER (CADDR W)))
      (COND
       ((EQ OP 'NEQ)
        (RETURN
         (CONS 'OR
               (LIST (LIST 'NEQ A NIL) (LIST 'NEQ B NIL) (LIST 'NEQ C NIL))))))
      (SETQ DISCR
              (ADDF (EXPTF B 2)
                    (NEGF
                     ((LAMBDA (G235)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF 4 G235))
                              (T (POLY-MULTF 4 G235))))
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF A C))
                            (T (POLY-MULTF A C)))))))
      (COND
       ((EQ OP 'EQUAL)
        (PROGN
         (COND
          ((OFSF_SUREP (LIST 'NEQ A NIL) THEO) (RETURN (LIST 'GEQ DISCR NIL))))
         (RETURN
          (CONS 'OR
                (LIST (LIST 'GREATERP DISCR NIL) (LIST 'EQUAL C NIL)
                      (CONS 'AND
                            (LIST (LIST 'EQUAL DISCR NIL)
                                  (LIST 'NEQ B NIL)))))))))
      (COND
       ((EQ OP 'LEQ)
        (PROGN
         (COND
          ((OFSF_SUREP (LIST 'GREATERP A NIL) THEO)
           (RETURN (LIST 'GEQ DISCR NIL))))
         (RETURN
          (CONS 'OR
                (LIST (LIST 'LESSP A NIL) (LIST 'LEQ C NIL)
                      (CONS 'AND
                            (LIST (LIST 'GEQ DISCR NIL)
                                  (LIST 'NEQ B NIL)))))))))
      (COND
       ((EQ OP 'GEQ)
        (PROGN
         (COND
          ((OFSF_SUREP (LIST 'LESSP A NIL) THEO)
           (RETURN (LIST 'GEQ DISCR NIL))))
         (RETURN
          (CONS 'OR
                (LIST (LIST 'GREATERP A NIL) (LIST 'GEQ C NIL)
                      (CONS 'AND
                            (LIST (LIST 'GEQ DISCR NIL)
                                  (LIST 'NEQ B NIL)))))))))
      (COND
       ((EQ OP 'LESSP)
        (PROGN
         (COND
          ((OFSF_SUREP (LIST 'GREATERP A NIL) THEO)
           (RETURN (LIST 'GREATERP DISCR NIL))))
         (RETURN
          (CONS 'OR
                (LIST (LIST 'GREATERP DISCR NIL) (LIST 'LESSP A NIL)
                      (LIST 'LESSP C NIL)))))))
      (COND
       ((EQ OP 'GREATERP)
        (PROGN
         (COND
          ((OFSF_SUREP (LIST 'LESSP A NIL) THEO)
           (RETURN (LIST 'GREATERP DISCR NIL))))
         (RETURN
          (CONS 'OR
                (LIST (LIST 'GREATERP DISCR NIL) (LIST 'GREATERP A NIL)
                      (LIST 'GREATERP C NIL)))))))
      (REDERR (LIST "ofsf_sqsc1at: unknown operator " OP)))) 
(PUT 'OFSF_SQSC-TEST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SQSC-TEST 'DEFINED-ON-LINE '2725) 
(PUT 'OFSF_SQSC-TEST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_SQSC-TEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SQSC-TEST (ATL V)
    (PROG (HIT A D)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR ATL))
         (SETQ ATL (CDR ATL))
         (SETQ D (DEGREEF (CADR A) V))
         (COND ((EQUAL D 1) (SETQ ATL (SETQ HIT NIL)))
               ((EQUAL D 2)
                (COND (HIT (SETQ ATL (SETQ HIT NIL))) (T (SETQ HIT A))))))
        (GO WHILELABEL))
      (RETURN HIT))) 
(PUT 'OFSF_LTHSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_LTHSIMPL 'DEFINED-ON-LINE '2742) 
(PUT 'OFSF_LTHSIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_LTHSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_LTHSIMPL (L)
    (PROG (W)
      (SETQ W
              (COND ((AND L (CDR L)) (CONS 'AND L))
                    ((NULL L) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR L))))
      ((LAMBDA (*RLSIEXPL *RLSIEXPLA *RLSIATADV *RLSIFAC)
         (SETQ W (RL_SIMPL W NIL (MINUS 1))))
       NIL NIL NIL NIL)
      (RETURN (OFSF_CJ2ATL W)))) 
(PUT 'OFSF_CJ2ATL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_CJ2ATL 'DEFINED-ON-LINE '2751) 
(PUT 'OFSF_CJ2ATL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_CJ2ATL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_CJ2ATL (F)
    (COND ((EQ F 'TRUE) (LIST))
          ((EQ F 'FALSE) (REDERR "ofsf_cj2atl: inconsistent theory"))
          ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND) (CDR F)) (T (LIST F)))) 
(PUT 'OFSF_FBQE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_FBQE 'DEFINED-ON-LINE '2762) 
(PUT 'OFSF_FBQE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_FBQE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_FBQE (F THEO)
    (COND
     (*RLQEFBQEPCAD
      (CONS THEO
            (OFSF_FBEXTERNAL F (FUNCTION QEPCAD_QEPCAD) (LIST 100 200)
             "Qepcad B")))
     (*RLQEFBMMA
      (CONS THEO
            (OFSF_FBEXTERNAL F (FUNCTION MMA_MATHEMATICA) NIL "Mathematica")))
     (T
      (PROGN
       (COND
        (*RLVERBOSE
         (IOTO_PRIN2T "ofsf_cad with optimization of projection order")))
       (OFSF_CAD1 F (OFSF_CADPORDER F) THEO))))) 
(PUT 'OFSF_FBEXTERNAL 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_FBEXTERNAL 'DEFINED-ON-LINE '2776) 
(PUT 'OFSF_FBEXTERNAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_FBEXTERNAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_FBEXTERNAL (F CALL ARGL NAME)
    (PROG (QL VARLL MTX MTX1 J L SUCCL FAILL RES VL W N M VN AN)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ VN 0)
      (SETQ AN 0)
      (COND
       (*RLQEFBSLFQ
        (PROGN
         (PROG (G236 G237)
           (SETQ G236 (CL_SPLIT F))
           (SETQ G237 G236)
           (SETQ QL (CAR G236))
           (SETQ G236 (CDR G236))
           (SETQ VARLL (CAR G236))
           (SETQ G236 (CDR G236))
           (SETQ MTX (CAR G236))
           (SETQ G236 (CDR G236))
           (RETURN G237))
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++ SLFQ ..."))))
         (SETQ MTX1 (QEPCAD_SLFQ MTX 100 200))
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_TPRIN2
             (LIST "+++ SLFQ simplification: " (CL_ATNUM MTX) " -> "))
            (IOTO_PRIN2T (COND (MTX1 (CL_ATNUM MTX1)) (T "failed"))))))
         (SETQ F (OR MTX1 MTX))
         (PROG (Q)
           (SETQ Q QL)
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q)
              (PROG (V)
                (SETQ V (PROG1 (CAR VARLL) (SETQ VARLL (CDR VARLL))))
               LAB
                (COND ((NULL V) (RETURN NIL)))
                ((LAMBDA (V)
                   (COND ((MEMQ V (CL_FVARL1 F)) (SETQ F (LIST Q V F)))))
                 (CAR V))
                (SETQ V (CDR V))
                (GO LAB)))
            (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB)))))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN F)))
      (PROG (G238)
        (SETQ G238 (CL_DIVIDE F))
        (SETQ J (CAR G238))
        (SETQ L (CDR G238))
        (RETURN G238))
      (COND
       (*RLVERBOSE
        (PROGN
         (SETQ N (LENGTH L))
         (IOTO_PRIN2T
          (LIST "+++ " NAME " on " N (IOTO_CPLU " subproblem" (GREATERP N 1))
                " ...")))))
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (COND
             (*RLVERBOSE
              (PROGN
               (SETQ VL (CL_VARL S))
               (SETQ VN (PLUS (LENGTH (CAR VL)) (LENGTH (CDR VL))))
               (SETQ AN (CL_ATNUM S))
               (IOTO_TPRIN2T
                (LIST "+++ Subproblem " (SETQ M (PLUS M 1)) " of " N ": " VN
                      (IOTO_CPLU " variable" (NOT (EQN VN 1))) ", " AN
                      (IOTO_CPLU " atomic formula" (NOT (EQN AN 1))))))))
            (SETQ W (APPLY CALL (CONS S ARGL)))
            (COND (W (SETQ SUCCL (CONS W SUCCL)))
                  (T (SETQ FAILL (CONS S FAILL))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ RES
              ((LAMBDA (G240)
                 (COND ((AND G240 (CDR G240)) (CONS J G240))
                       ((NULL G240) (COND ((EQ J 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G240))))
               (NCONC SUCCL FAILL)))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2
         (LIST "+++ Final simplification ... " (CL_ATNUM RES) " -> "))))
      (SETQ RES (RL_SIMPL RES NIL (MINUS 1)))
      (COND (*RLVERBOSE (IOTO_PRIN2T (CL_ATNUM RES))))
      (SETQ W (LENGTH FAILL))
      (COND
       ((GREATERP W 0)
        (LPRIM
         (LIST "quantifier elimination failed for" W
               (IOTO_CPLU "subproblem" (GREATERP W 1)) "out of" N))))
      (RETURN RES))) 
(PUT 'OFSF_ELIMSET-PRECISE 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_ELIMSET-PRECISE 'DEFINED-ON-LINE '2831) 
(PUT 'OFSF_ELIMSET-PRECISE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSET-PRECISE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_ELIMSET-PRECISE (V ALP)
    (LTO_NCONCN
     (LIST (OFSF_ELIMSETQ-PRECISE (CAR ALP))
           (OFSF_ELIMSETR-PRECISE (CAR ALP))))) 
(PUT 'OFSF_ELIMSETQ-PRECISE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETQ-PRECISE 'DEFINED-ON-LINE '2839) 
(PUT 'OFSF_ELIMSETQ-PRECISE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETQ-PRECISE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETQ-PRECISE (ATFAL)
    (PROG (EQUAL1 LEQ1 GEQ1 GREATERP1 LESSP1 WO1 SO1 NEQ1 EQUAL21Q WO21Q SO21Q
           NEQ21Q QESUBCQL SO PSLB1 PSUB1)
      (SETQ EQUAL1 (LTO_CATSOC 'EQUAL1 ATFAL))
      (SETQ LEQ1 (LTO_CATSOC 'LEQ1 ATFAL))
      (SETQ GEQ1 (LTO_CATSOC 'GEQ1 ATFAL))
      (SETQ GREATERP1 (LTO_CATSOC 'GREATERP1 ATFAL))
      (SETQ LESSP1 (LTO_CATSOC 'LESSP1 ATFAL))
      (SETQ WO1 (LTO_CATSOC 'WO1 ATFAL))
      (SETQ SO1 (LTO_CATSOC 'SO1 ATFAL))
      (SETQ NEQ1 (LTO_CATSOC 'NEQ1 ATFAL))
      (SETQ NEQ21Q (LTO_CATSOC 'NEQ21Q ATFAL))
      (SETQ WO21Q (LTO_CATSOC 'WO21Q ATFAL))
      (SETQ SO21Q (LTO_CATSOC 'SO21Q ATFAL))
      (SETQ QESUBCQL (LTO_NCONCN (LIST EQUAL1 LEQ1 GEQ1 WO1 EQUAL21Q WO21Q)))
      (SETQ SO (LTO_NCONCN (LIST SO1 NEQ1 SO21Q NEQ21Q)))
      (SETQ PSLB1 (NCONC GREATERP1 SO))
      (SETQ PSUB1 (NCONC LESSP1 SO))
      (COND
       (*RLQELOG
        (PROGN
         (SETCAR (CDR (CAR RLQELOG*)) (LENGTH PSLB1))
         (SETCAR (CDDR (CAR RLQELOG*)) (LENGTH PSUB1)))))
      (PROG (X)
        (SETQ X PSLB1)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ QESUBCQL
                    (CONS (LIST (CAR X) (ADDSQ (CADR X) (CONS 1 1))) QESUBCQL))
            (PROG (Y)
              (SETQ Y PSUB1)
             LAB
              (COND ((NULL Y) (RETURN NIL)))
              ((LAMBDA (Y)
                 (COND
                  ((NEQ X Y)
                   (PROGN
                    (SETQ QESUBCQL (CONS (OFSF_AVGQ X Y) QESUBCQL))
                    (COND
                     (*RLQELOG
                      (SETCAR (CDDDDR (CAR RLQELOG*))
                              (PLUS (CAR (CDDDDR (CAR RLQELOG*))) 1))))))))
               (CAR Y))
              (SETQ Y (CDR Y))
              (GO LAB))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X PSUB1)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ QESUBCQL
                   (CONS (LIST (CAR X) (ADDSQ (CADR X) (NEGSQ (CONS 1 1))))
                         QESUBCQL)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ QESUBCQL (CONS (LIST 'TRUE (CONS NIL 1)) QESUBCQL))
      (RETURN (LIST (CONS 'OFSF_QESUBCQ QESUBCQL))))) 
(PUT 'OFSF_AVGQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_AVGQ 'DEFINED-ON-LINE '2881) 
(PUT 'OFSF_AVGQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_AVGQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_AVGQ (X Y)
    (LIST (CONS 'AND (LIST (CAR X) (CAR Y)))
          (MULTSQ (ADDSQ (CADR X) (CADR Y)) (INVSQ (CONS 2 1))))) 
(PUT 'OFSF_ELIMSETR-PRECISE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_ELIMSETR-PRECISE 'DEFINED-ON-LINE '2887) 
(PUT 'OFSF_ELIMSETR-PRECISE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFQE.RED) 
(PUT 'OFSF_ELIMSETR-PRECISE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_ELIMSETR-PRECISE (ATFAL)
    (PROG (EQUAL21R WO21R SO21R NEQ21R SO)
      (SETQ EQUAL21R (LTO_CATSOC 'EQUAL21R ATFAL))
      (SETQ WO21R (LTO_CATSOC 'WO21R ATFAL))
      (SETQ SO21R (LTO_CATSOC 'SO21R ATFAL))
      (SETQ NEQ21R (LTO_CATSOC 'NEQ21R ATFAL))
      (SETQ SO (NCONC SO21R NEQ21R))
      (RETURN
       (LIST (CONS 'OFSF_QESUBCRME1 SO) (CONS 'OFSF_QESUBCRPE1 SO)
             (CONS 'OFSF_QESUBCR1 (NCONC EQUAL21R WO21R)))))) 
(PUT 'OFSF_QEG 'NUMBER-OF-ARGS 1) 
(DE OFSF_QEG (F)
    (PROG (*RLQEGENCT ASS GRES RES W)
      (SETQ GRES (CL_GQE F NIL NIL))
      (SETQ RES
              (CONS GRES
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH (CAR GRES)) I))
                        (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (PROGN
                                        (SETQ ASS (NTH (CAR GRES) I))
                                        (SETQ W
                                                (PROG (FAC FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ FAC
                                                          (CDR
                                                           (SFTO_FCTRF
                                                            (CADR ASS))))
                                                  (COND
                                                   ((NULL FAC) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA
                                                                        (FAC)
                                                                      (CAR
                                                                       FAC))
                                                                    (CAR FAC))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ FAC (CDR FAC))
                                                  (COND
                                                   ((NULL FAC)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (FAC)
                                                              (CAR FAC))
                                                            (CAR FAC))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL)))
                                        (COND
                                         ((CDR W)
                                          (REDERR
                                           "ofsf_qeg: uexpected nonvariable assumption")))
                                        (CONS
                                         (CONS (LIST 'EQUAL (CADR ASS) NIL)
                                               (LTO_DELQ ASS (CAR GRES)))
                                         (CL_QE
                                          (CL_SUBFOF
                                           (LIST (CONS (PREPF (CAR W)) 0)) F)
                                          NIL)))
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH (CAR GRES)) I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               (PROGN
                                (SETQ ASS (NTH (CAR GRES) I))
                                (SETQ W
                                        (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ FAC
                                                  (CDR
                                                   (SFTO_FCTRF (CADR ASS))))
                                          (COND ((NULL FAC) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (FAC)
                                                              (CAR FAC))
                                                            (CAR FAC))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ FAC (CDR FAC))
                                          (COND
                                           ((NULL FAC) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (FAC) (CAR FAC))
                                                    (CAR FAC))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                (COND
                                 ((CDR W)
                                  (REDERR
                                   "ofsf_qeg: uexpected nonvariable assumption")))
                                (CONS
                                 (CONS (LIST 'EQUAL (CADR ASS) NIL)
                                       (LTO_DELQ ASS (CAR GRES)))
                                 (CL_QE
                                  (CL_SUBFOF (LIST (CONS (PREPF (CAR W)) 0)) F)
                                  NIL)))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN
       (CL_SIMPL
        ((LAMBDA (G242)
           (COND ((AND G242 (CDR G242)) (CONS 'OR G242))
                 ((NULL G242) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR G242))))
         (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
           (SETQ CASE RES)
           (COND ((NULL CASE) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (CASE)
                               (CONS 'AND (CONS (CDR CASE) (CAR CASE))))
                             (CAR CASE))
                            NIL)))
          LOOPLABEL
           (SETQ CASE (CDR CASE))
           (COND ((NULL CASE) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (CASE) (CONS 'AND (CONS (CDR CASE) (CAR CASE))))
                     (CAR CASE))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
        NIL (MINUS 1))))) 
(PUT 'OFSF_PREQE 'NUMBER-OF-ARGS 3) 
(DE OFSF_PREQE (F THEO EXACT)
    (PROG (QL VARLL BVL EVL EQL G)
      (SETQ F (RL_SIMPL (RL_PNF F) NIL (MINUS 1)))
      (COND
       ((NOT
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (RETURN (CONS NIL F))))
      (PROG (G243 G244)
        (SETQ G243 (CL_SPLIT F))
        (SETQ G244 G243)
        (SETQ QL (CAR G243))
        (SETQ G243 (CDR G243))
        (SETQ VARLL (CAR G243))
        (SETQ G243 (CDR G243))
        (SETQ G (CAR G243))
        (SETQ G243 (CDR G243))
        (SETQ BVL (CAR G243))
        (SETQ G243 (CDR G243))
        (RETURN G244))
      (COND ((NEQ QL '(EX)) (RETURN (CONS NIL F))))
      (COND ((NEQ (COND ((ATOM G) G) (T (CAR G))) 'AND) (RETURN (CONS NIL F))))
      (PROG (G245)
        (SETQ G245 (OFSF_PREQEGOODEQL G (CAR VARLL) EXACT))
        (SETQ EVL (CAR G245))
        (SETQ EQL (CDR G245))
        (RETURN G245))
      (COND ((NULL EVL) (RETURN (CONS NIL F))))
      (SETQ EVL (SORT EVL 'ORDP))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH EVL) (LENGTH EQL)) I))
          (RETURN NIL)))
        (SETQ EVL (CDR EVL))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (V)
        (SETQ V (REVERSE EVL))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ G (LIST 'EX V G))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T "+++ starting qe, which should do only Gauss steps")))
      (PROG (G246)
        (SETQ G246 (CL_GQE G THEO NIL))
        (SETQ THEO (CAR G246))
        (SETQ G (CDR G246))
        (RETURN G246))
      (PROG (V)
        (SETQ V (REVERSE (SORT (LTO_SETMINUS (CAR VARLL) EVL) 'ORDP)))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ G (LIST 'EX V G))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS THEO G)))) 
(PUT 'OFSF_VCREDUCE 'NUMBER-OF-ARGS 5) 
(DE OFSF_VCREDUCE (F VARIABLES THEO NOGEN EXACT)
    (PROG (ANS LINL MISL RES REST THEO0 V W *RLSIEXPLA)
      (SETQ F (RL_SIMPL F THEO (MINUS 1)))
      (SETQ THEO0 THEO)
      (PROG (G247 G248)
        (SETQ G247 (OFSF_VCREDUCEPARTITIONVARS F VARIABLES EXACT))
        (SETQ G248 G247)
        (SETQ LINL (CAR G247))
        (SETQ G247 (CDR G247))
        (SETQ MISL (CAR G247))
        (SETQ G247 (CDR G247))
        (SETQ REST (CAR G247))
        (SETQ G247 (CDR G247))
        (RETURN G248))
      (SETQ LINL (SORT LINL 'ORDP))
      (SETQ MISL (SORT MISL 'ORDP))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ Eliminating " LINL ", " MISL ": "))))
      (PROG (V)
        (SETQ V (APPEND LINL MISL))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" V ":"))))
            (COND
             ((OFSF_VCREDUCESTILLGOOD V F VARIABLES)
              (PROGN
               ((LAMBDA (*RLVERBOSE)
                  (SETQ W (CL_GQEA (LIST 'EX V F) THEO NIL)))
                NIL)
               (SETQ THEO (CAR W))
               (SETQ F (CAAR (CDR W)))
               (SETQ ANS (CONS (CADAR (CDR W)) ANS))
               (COND (*RLVERBOSE (IOTO_PRIN2 "ok] "))))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ RES (LIST F (OFSF_VCREDUCEBACKSUB ANS) (LTO_SETMINUS THEO THEO0)))
      (COND (*RLVERBOSE (TERPRI)))
      (RETURN RES))) 
(PUT 'OFSF_VCREDUCEPARTITIONVARS 'NUMBER-OF-ARGS 3) 
(DE OFSF_VCREDUCEPARTITIONVARS (F VARIABLES EXACT)
    (PROG (ARGL TERML ALL MISL VL EL VCL)
      (SETQ ARGL
              (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND) (CDR F))
                    (T (LIST F))))
      (PROG (S)
        (SETQ S ARGL)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((EQ (COND ((ATOM S) S) (T (CAR S))) 'EQUAL)
             (PROGN
              (SETQ TERML (OFSF_PREQEGOODPOLY (CADR S) VARIABLES))
              (COND
               (TERML
                (PROGN
                 (PROG (TERM)
                   (SETQ TERM TERML)
                  LAB
                   (COND ((NULL TERM) (RETURN NIL)))
                   ((LAMBDA (TERM)
                      (PROGN
                       (SETQ ALL (UNION ALL TERM))
                       (COND
                        ((CDR TERM)
                         (PROGN
                          (SETQ VL (UNION VL TERM))
                          (PROG (RVL1)
                            (SETQ RVL1 TERM)
                           LAB
                            (COND ((NULL RVL1) (RETURN NIL)))
                            (PROG (V)
                              (SETQ V (CDR RVL1))
                             LAB
                              (COND ((NULL V) (RETURN NIL)))
                              ((LAMBDA (V)
                                 (SETQ EL (LTO_INSERT (CONS (CAR RVL1) V) EL)))
                               (CAR V))
                              (SETQ V (CDR V))
                              (GO LAB))
                            (SETQ RVL1 (CDR RVL1))
                            (GO LAB)))))))
                    (CAR TERM))
                   (SETQ TERM (CDR TERM))
                   (GO LAB)))))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ computing minimum vertex cover of " EL ": "))))
      ((LAMBDA (*RLVERBOSE) (SETQ VCL (LTO_VERTEXCOVER EL EXACT))) NIL)
      (COND (*RLVERBOSE (IOTO_PRIN2T (LIST VCL))))
      (SETQ MISL (LTO_SETMINUS VL VCL))
      (RETURN (LIST (LTO_SETMINUS ALL VL) MISL (LTO_SETMINUS VARIABLES ALL))))) 
(PUT 'OFSF_VCREDUCESTILLGOOD 'NUMBER-OF-ARGS 3) 
(DE OFSF_VCREDUCESTILLGOOD (V F VARIABLES)
    (PROG (ARGL AT P RP LCKL FOUNDV FOUNDPOLY)
      (COND
       ((EQ F 'TRUE)
        (PROGN (COND (*RLVERBOSE (IOTO_PRIN2 "gone] "))) (RETURN NIL))))
      (SETQ ARGL
              (COND ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND) (CDR F))
                    (T (LIST F))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT FOUNDPOLY) ARGL)) (RETURN NIL)))
        (PROGN
         (SETQ AT (PROG1 (CAR ARGL) (SETQ ARGL (CDR ARGL))))
         (SETQ P (CADR AT))
         (COND
          ((MEMQ V (KERNELS P))
           (PROGN
            (SETQ FOUNDV T)
            (COND
             ((EQ (COND ((ATOM AT) AT) (T (CAR AT))) 'EQUAL)
              (PROGN
               (SETQ RP (SFTO_REORDER P V))
               (COND
                ((EQUAL (CDAAR RP) 1)
                 (PROGN
                  (SETQ LCKL (INTERSECTION (KERNELS (CDAR RP)) VARIABLES))
                  (COND
                   ((NOT (AND LCKL (CDR LCKL))) (SETQ FOUNDPOLY T)))))))))))))
        (GO WHILELABEL))
      (COND (FOUNDPOLY (RETURN T)))
      (COND
       (*RLVERBOSE
        (COND (FOUNDV (IOTO_PRIN2 "failed] ")) (T (IOTO_PRIN2 "gone] ")))))
      (RETURN NIL))) 
(PUT 'OFSF_VCREDUCEBACKSUB 'NUMBER-OF-ARGS 1) 
(DE OFSF_VCREDUCEBACKSUB (ANS)
    (PROG (SUBL W)
      (COND (*RLVERBOSE (IOTO_TPRIN2 "+++ Back-substitution: ")))
      (SETQ W
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR ANS)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (PROGN
                                     (COND
                                      (*RLVERBOSE
                                       (IOTO_PRIN2 (LIST "[" (CAR PR)))))
                                     (SETQ PR
                                             (CONS (CAR PR)
                                                   (PREPSQ
                                                    (SUBSQ (SIMP (CDR PR))
                                                           SUBL))))
                                     (PROGN (SETQ SUBL (CONS PR SUBL)) PR)
                                     (COND (*RLVERBOSE (IOTO_PRIN2 "] ")))
                                     PR))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (PROGN
                             (COND
                              (*RLVERBOSE (IOTO_PRIN2 (LIST "[" (CAR PR)))))
                             (SETQ PR
                                     (CONS (CAR PR)
                                           (PREPSQ
                                            (SUBSQ (SIMP (CDR PR)) SUBL))))
                             (PROGN (SETQ SUBL (CONS PR SUBL)) PR)
                             (COND (*RLVERBOSE (IOTO_PRIN2 "] ")))
                             PR))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (SORT W (FUNCTION ORDPCAR))))) 
(PUT 'OFSF_PREQEGOODEQL 'NUMBER-OF-ARGS 3) 
(DE OFSF_PREQEGOODEQL (F QVL EXACT)
    (PROG (EQL VLL SVL VL EL VCL EVL)
      (PROG (S)
        (SETQ S (CDR F))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((EQ (COND ((ATOM S) S) (T (CAR S))) 'EQUAL)
             (PROGN
              (SETQ VLL (OFSF_PREQEGOODPOLY (CADR S) QVL))
              (COND
               (VLL
                (PROGN
                 (SETQ SVL NIL)
                 (PROG (TERM)
                   (SETQ TERM VLL)
                  LAB
                   (COND ((NULL TERM) (RETURN NIL)))
                   ((LAMBDA (TERM)
                      (PROGN
                       (PROG (V)
                         (SETQ V TERM)
                        LAB
                         (COND ((NULL V) (RETURN NIL)))
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ SVL (LTO_INSERTQ V SVL))
                             (SETQ VL (LTO_INSERTQ V VL))))
                          (CAR V))
                         (SETQ V (CDR V))
                         (GO LAB))
                       (PROG (RVL1)
                         (SETQ RVL1 TERM)
                        LAB
                         (COND ((NULL RVL1) (RETURN NIL)))
                         (PROG (V)
                           (SETQ V (CDR RVL1))
                          LAB
                           (COND ((NULL V) (RETURN NIL)))
                           ((LAMBDA (V)
                              (SETQ EL (LTO_INSERT (CONS (CAR RVL1) V) EL)))
                            (CAR V))
                           (SETQ V (CDR V))
                           (GO LAB))
                         (SETQ RVL1 (CDR RVL1))
                         (GO LAB))))
                    (CAR TERM))
                   (SETQ TERM (CDR TERM))
                   (GO LAB))
                 (PROG (W1)
                   (SETQ W1 (CONS S SVL))
                   (SETQ EQL (CONS W1 EQL))
                   (RETURN W1)))))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2 (LIST "+++ computing minimum vertex cover of " EL ": "))))
      ((LAMBDA (*RLVERBOSE) (SETQ VCL (LTO_VERTEXCOVER EL EXACT))) NIL)
      (COND (*RLVERBOSE (IOTO_PRIN2T (LIST VCL))))
      (SETQ EVL (LTO_SETMINUS VL VCL))
      (RETURN (CONS EVL EQL)))) 
(PUT 'OFSF_PREQEGOODPOLY 'NUMBER-OF-ARGS 2) 
(DE OFSF_PREQEGOODPOLY (P QVL)
    (PROG (AVL ML C EV SCVL V VL VLL N D E)
      (SETQ N 0)
      (SETQ D 0)
      (SETQ E 0)
      (PROG (G249)
        (SETQ G249 (SFTO_SF2MONL (SFTO_REORDER P QVL)))
        (SETQ AVL (CAR G249))
        (SETQ ML (CDR G249))
        (RETURN G249))
      (SETQ N (LENGTH (LTO_SETMINUS AVL QVL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ AVL (CDR AVL))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C ML)) (RETURN NIL)))
        (PROGN
         (SETQ EV (CAR (PROG1 (CAR ML) (SETQ ML (CDR ML)))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
           (SETQ EV (CDR EV))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ SCVL AVL)
         (SETQ D 0)
         (SETQ VL NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C SCVL)) (RETURN NIL)))
           (PROGN
            (SETQ V (PROG1 (CAR SCVL) (SETQ SCVL (CDR SCVL))))
            (SETQ E (PROG1 (CAR EV) (SETQ EV (CDR EV))))
            (SETQ D (PLUS D E))
            (COND ((OR (GREATERP E 1) (GREATERP D 2)) (SETQ C NIL))
                  ((EQN E 1) (PROGN (SETQ VL (CONS V VL)) V))))
           (GO WHILELABEL))
         (COND (VL (PROGN (SETQ VLL (CONS VL VLL)) VL))))
        (GO WHILELABEL))
      (RETURN (AND C VLL)))) 
(ENDMODULE) 