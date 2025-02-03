(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFQE)) 
(REVISION 'PASFQE "$Id: pasfqe.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'PASFQE
           "(c) 2002-2009 A. Dolzmann, A. Seidl, T. Sturm, 2010-2016 T. Sturm") 
(PUT 'PASF_QE 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_QE 'DEFINED-ON-LINE '34) 
(PUT 'PASF_QE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_QE (PHI THEO)
    (COND
     ((NULL (PASF_UPRAP PHI)) (PASF_EXPAND (PASF_GQE PHI THEO NIL (SIMP 1))))
     (T (REDERR (LIST "Only weak quantifier elimination possible"))))) 
(PUT 'PASF_WQE 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_WQE 'DEFINED-ON-LINE '43) 
(PUT 'PASF_WQE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_WQE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_WQE (PHI THEO) (PASF_GQE PHI THEO NIL (SIMP 1))) 
(PUT 'PASF_QEA 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_QEA 'DEFINED-ON-LINE '49) 
(PUT 'PASF_QEA 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QEA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_QEA (PHI THEO)
    (COND ((NULL (PASF_UPRAP PHI)) (PASF_EXPANDA (PASF_WQEA PHI THEO) PHI))
          (T (REDERR (LIST "Only weak quantifier elimination possible"))))) 
(PUT 'PASF_WQEA 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_WQEA 'DEFINED-ON-LINE '58) 
(PUT 'PASF_WQEA 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_WQEA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_WQEA (PHI THEO)
    (PROG (RES RET)
      (SETQ RES (PASF_GQE PHI THEO T (SIMP 1)))
      (PROG (R)
        (SETQ R RES)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (SETQ RET
                   (CONS
                    (LIST (ANSW_F R)
                          (PROG (B FORALL-RESULT FORALL-ENDPTR)
                            (SETQ B (ANSW_BL R))
                            (COND ((NULL B) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS ((LAMBDA (B) B) (CAR B))
                                                  NIL)))
                           LOOPLABEL
                            (SETQ B (CDR B))
                            (COND ((NULL B) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (B) B) (CAR B)) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          (PROG (EQN FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EQN (ANSW_TL R))
                            (COND ((NULL EQN) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EQN)
                                                (CONS (PREPF (CADR EQN))
                                                      (PREPSQ (CADDR EQN))))
                                              (CAR EQN))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EQN (CDR EQN))
                            (COND ((NULL EQN) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EQN)
                                        (CONS (PREPF (CADR EQN))
                                              (PREPSQ (CADDR EQN))))
                                      (CAR EQN))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                    RET)))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN RET))) 
(PUT 'PASF_PQE 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_PQE 'DEFINED-ON-LINE '71) 
(PUT 'PASF_PQE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_PQE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_PQE (PHI P THEO)
    (COND ((NULL (PASF_UPRAP PHI)) (PASF_GQE PHI THEO NIL P))
          (T
           (REDERR (LIST "Probabilistic quantifier elimination impossible"))))) 
(PUT 'PASF_PQEA 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_PQEA 'DEFINED-ON-LINE '81) 
(PUT 'PASF_PQEA 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_PQEA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_PQEA (PHI P THEO)
    (COND ((NULL (PASF_UPRAP PHI)) (PASF_EXPANDA (PASF_PQEA1 PHI THEO P) PHI))
          (T
           (REDERR (LIST "Probabilistic quantifier elimination impossible"))))) 
(PUT 'PASF_PQEA1 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_PQEA1 'DEFINED-ON-LINE '91) 
(PUT 'PASF_PQEA1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_PQEA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_PQEA1 (PHI THEO P)
    (PROG (RES RET)
      (SETQ RES (PASF_GQE PHI THEO T P))
      (PROG (R)
        (SETQ R RES)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (SETQ RET
                   (CONS
                    (LIST (ANSW_F R)
                          (PROG (B FORALL-RESULT FORALL-ENDPTR)
                            (SETQ B (ANSW_BL R))
                            (COND ((NULL B) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS ((LAMBDA (B) B) (CAR B))
                                                  NIL)))
                           LOOPLABEL
                            (SETQ B (CDR B))
                            (COND ((NULL B) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (B) B) (CAR B)) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          (PROG (EQN FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EQN (ANSW_TL R))
                            (COND ((NULL EQN) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EQN)
                                                (LIST 'EQUAL (PREPF (CADR EQN))
                                                      (PREPSQ (CADDR EQN))))
                                              (CAR EQN))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EQN (CDR EQN))
                            (COND ((NULL EQN) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EQN)
                                        (LIST 'EQUAL (PREPF (CADR EQN))
                                              (PREPSQ (CADDR EQN))))
                                      (CAR EQN))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                    RET)))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN RET))) 
(PUT 'PASF_GQE 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_GQE 'DEFINED-ON-LINE '105) 
(PUT 'PASF_GQE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_GQE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_GQE (PHI THEO ANSW P)
    (PROG (RSLT PT RETN TMP BL TL)
      (COND (*RLVERBOSE (IOTO_TPRIN2 "++++ Entering pasf_qe")))
      (SETQ PHI (CL_SIMPL PHI THEO (MINUS 1)))
      (SETQ RSLT (COND (*RLPASFDNFFIRST (PASF_DNF PHI)) (T (PASF_PNF PHI))))
      (COND
       ((OR (EQ (COND ((ATOM RSLT) RSLT) (T (CAR RSLT))) 'EX) PHI)
        (SETQ PT 'EXISTENTIAL))
       ((OR (EQ (COND ((ATOM RSLT) RSLT) (T (CAR RSLT))) 'ALL) PHI)
        (SETQ PT 'UNIVERSAL))
       (ANSW (REDERR (LIST "QE with answers impossible"))))
      (SETQ RSLT (PASF_INPLACEQE RSLT THEO ANSW P))
      (COND
       ((AND ANSW (OR (EQ RSLT 'TRUE) (EQ RSLT 'FALSE)))
        (SETQ RSLT (LIST (ANSW_NEW RSLT NIL NIL)))))
      (COND
       (ANSW
        (PROGN
         (PROG (AN)
           (SETQ AN RSLT)
          LAB
           (COND ((NULL AN) (RETURN NIL)))
           ((LAMBDA (AN)
              (PROGN
               (SETQ TMP (CL_SIMPL (ANSW_F AN) THEO (MINUS 1)))
               (SETQ BL (ANSW_BL AN))
               (SETQ TL (ANSW_TL AN))
               (COND
                ((OR (AND (EQ PT 'EXISTENTIAL) (NEQ TMP 'FALSE))
                     (AND (EQ PT 'UNIVERSAL) (NEQ TMP 'TRUE)))
                 (SETQ RETN (LTO_INSERT (ANSW_NEW TMP BL TL) RETN))))))
            (CAR AN))
           (SETQ AN (CDR AN))
           (GO LAB))
         (COND ((NULL RETN) (SETQ RETN (LIST (ANSW_NEW 'FALSE NIL NIL)))))
         (RETURN RETN)))
       (T (RETURN (CL_SIMPL RSLT THEO (MINUS 1))))))) 
(PUT 'PASF_INPLACEQE 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_INPLACEQE 'DEFINED-ON-LINE '154) 
(PUT 'PASF_INPLACEQE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_INPLACEQE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_INPLACEQE (PHI THEO ANSW P)
    (PROG (RES)
      (SETQ RES (PASF_INPLACEQE1 PHI THEO P))
      (COND
       ((CDR RES)
        (RETURN
         (PASF_QEBLOCK (CADR RES) (CDDR RES) (CAR RES) THEO
          (COND (ANSW (ANSW_NEW 'TRUE NIL NIL)) (T NIL)) P))))
      (RETURN (CAR RES)))) 
(PUT 'PASF_INPLACEQE1 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_INPLACEQE1 'DEFINED-ON-LINE '168) 
(PUT 'PASF_INPLACEQE1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_INPLACEQE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_INPLACEQE1 (PHI THEO P)
    (PROG (TMP F)
      (COND
       (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
         (COND ((ATOM PHI) PHI) (T (CAR PHI))))
        (PROGN
         (SETQ TMP (PASF_INPLACEQE1 (CADDR PHI) THEO P))
         (COND
          ((CDR TMP)
           (SETQ F (PASF_QEBLOCK (CADR TMP) (CDDR TMP) (CAR TMP) THEO NIL P)))
          (T (SETQ F (CAR TMP))))
         (RETURN
          (CONS
           (LIST (COND ((ATOM PHI) PHI) (T (CAR PHI))) (CADR PHI) F
                 (CADDDR PHI))
           NIL)))))
      (COND
       (((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
         (COND ((ATOM PHI) PHI) (T (CAR PHI))))
        (PROGN
         (SETQ TMP (PASF_INPLACEQE1 (CADDR PHI) THEO P))
         (RETURN
          (COND
           ((CDR TMP)
            (COND
             ((NEQ (CADR TMP) (COND ((ATOM PHI) PHI) (T (CAR PHI))))
              (CONS (PASF_QEBLOCK (CADR TMP) (CDDR TMP) (CAR TMP) THEO NIL P)
                    (CONS (COND ((ATOM PHI) PHI) (T (CAR PHI)))
                          (LIST (CADR PHI)))))
             (T
              (CONS (CAR TMP)
                    (CONS (CADR TMP) (CONS (CADR PHI) (CDDR TMP)))))))
           (T
            (CONS (CAR TMP)
                  (CONS (COND ((ATOM PHI) PHI) (T (CAR PHI)))
                        (LIST (CADR PHI))))))))))
      (RETURN (CONS PHI NIL)))) 
(SWITCH (LIST 'RLQEINFIRST)) 
(OFF1 'RLQEINFIRST) 
(PUT 'PASF_QEBLOCK 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_QEBLOCK 'DEFINED-ON-LINE '204) 
(PUT 'PASF_QEBLOCK 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QEBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QEBLOCK (THETA VARL PSI THEO ANSW P)
    (PROG (RES DPTH VLV)
      (SETQ DPTH 0)
      (SETQ VLV 0)
      (COND (*RLQEINFIRST (SETQ VARL (REVERSE VARL))))
      (COND
       (*RLVERBOSE
        (COND (*RLQEINFIRST (IOTO_TPRIN2 (LIST "---- " (CONS THETA VARL))))
              (T (IOTO_TPRIN2 (LIST "---- " (CONS THETA (REVERSE VARL))))))))
      (COND
       ((EQ THETA 'EX)
        (SETQ RES (PASF_QEEXBLOCK VARL PSI DPTH VLV THEO ANSW P)))
       (T
        (PROGN
         (SETQ RES (PASF_QEEXBLOCK VARL (CL_NNFNOT PSI) DPTH VLV THEO ANSW P))
         (SETQ RES
                 (COND
                  (ANSW
                   (PROG (AN FORALL-RESULT FORALL-ENDPTR)
                     (SETQ AN RES)
                     (COND ((NULL AN) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (AN)
                                         (ANSW_NEW (CL_NNFNOT (ANSW_F AN))
                                          (ANSW_BL AN) (ANSW_TL AN)))
                                       (CAR AN))
                                      NIL)))
                    LOOPLABEL
                     (SETQ AN (CDR AN))
                     (COND ((NULL AN) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (AN)
                                 (ANSW_NEW (CL_NNFNOT (ANSW_F AN)) (ANSW_BL AN)
                                  (ANSW_TL AN)))
                               (CAR AN))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                  (T (CL_NNFNOT RES)))))))
      (RETURN RES))) 
(PUT 'PASF_QEEXBLOCK 'NUMBER-OF-ARGS 7) 
(PUT 'PASF_QEEXBLOCK 'DEFINED-ON-LINE '234) 
(PUT 'PASF_QEEXBLOCK 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QEEXBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PASF_QEEXBLOCK (VARL PSI DPTH VLV THEO ANSW P)
    (PROG (CO CVL W COE F NEWJ V ANS WW C DELC OLDCOL COUNT COMAX COMAXN)
      (SETQ C 0)
      (SETQ DELC 0)
      (SETQ OLDCOL 0)
      (SETQ COUNT 0)
      (SETQ COMAX 0)
      (SETQ COMAXN 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (COND
          (*RLQEDFS
           (PROGN
            (IOTO_PRIN2 (LIST " [DFS"))
            (COND (*RLQEDYN (IOTO_PRIN2 (LIST " DYN"))))
            (COND
             (*RLQEVBOLD
              (PROGN
               (SETQ DPTH (LENGTH VARL))
               (SETQ VLV (QUOTIENT DPTH 4))
               (IOTO_PRIN2T
                (LIST ": depth " DPTH ", watching " (DIFFERENCE DPTH VLV)
                      "]"))))
             (T (IOTO_PRIN2T (LIST "]"))))))
          (T (IOTO_PRIN2T (LIST " [BFS: depth " DPTH "]")))))))
      (SETQ CVL VARL)
      (SETQ CO (CO_NEW))
      (COND (*RLPASFDNFQEEXBLOCK (SETQ PSI (PASF_DNF PSI))))
      (COND
       ((EQ (COND ((ATOM PSI) PSI) (T (CAR PSI))) 'OR)
        (PROG (X)
          (SETQ X (CDR PSI))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (SETQ CO (CO_SAVE CO (LIST (LIST 'CE CVL X NIL NIL ANSW)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB)))
       (T (SETQ CO (CO_SAVE CO (LIST (LIST 'CE CVL PSI NIL NIL ANSW))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR CO)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND *RLVERBOSE (NOT *RLQEVBOLD))
           (COND
            (*RLQEDFS
             (PROGN
              (SETQ WW (CAR (CO_STAT CO)))
              (COND
               ((OR (EQUAL COMAX 0) (LESSP (CAR WW) COMAX)
                    (AND (EQUAL (CAR WW) COMAX) (LESSP (CDR WW) COMAXN)))
                (PROGN
                 (SETQ COMAX (CAR WW))
                 (SETQ COMAXN (CDR WW))
                 (IOTO_PRIN2 (LIST "[" COMAX ":" COMAXN "] "))))))))))
         (PROG (G159)
           (SETQ G159 (CO_GET CO))
           (SETQ COE (CAR G159))
           (SETQ CO (CDR G159))
           (RETURN G159))
         (SETQ CVL (CAR (CDR COE)))
         (SETQ COUNT (PLUS COUNT 1))
         (COND
          (*RLVERBOSE
           (COND
            (*RLQEDFS
             (COND
              (*RLQEVBOLD
               (PROGN
                (COND
                 ((EQUAL VLV (LENGTH CVL))
                  (IOTO_TPRIN2T (LIST "-- crossing: " (DIFFERENCE DPTH VLV)))))
                (IOTO_PRIN2 (LIST "[" (DIFFERENCE DPTH (LENGTH CVL))))))))
            (T
             (PROGN
              (COND
               ((EQUAL C 0)
                (PROGN
                 (IOTO_TPRIN2T (LIST "-- left: " (LENGTH CVL)))
                 (SETQ C (PLUS (CO_LENGTH CO) 1)))))
              (IOTO_NTERPRI (PLUS (LENGTH (EXPLODE C)) 4))
              (IOTO_PRIN2 (LIST "[" C))
              (SETQ C (DIFFERENCE C 1)))))))
         (SETQ CVL (PASF_VARSEL CVL PSI))
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "----" CVL))))
         (SETQ V (PROG1 (CAR CVL) (SETQ CVL (CDR CVL))))
         (SETQ ANS (PASF_QEEX (CADR (CDR COE)) V THEO (NTH (CDR COE) 5) CVL P))
         (COND
          (CVL
           (PROGN
            (COND (*RLVERBOSE (SETQ OLDCOL (CO_LENGTH CO))))
            (SETQ CO (CO_SAVE CO ANS))
            (COND
             (*RLVERBOSE
              (SETQ DELC
                      (PLUS DELC OLDCOL
                            (DIFFERENCE (LENGTH ANS) (CO_LENGTH CO))))))))
          (T
           (PROGN
            (COND
             (ANSW
              (PROG (AN)
                (SETQ AN ANS)
               LAB
                (COND ((NULL AN) (RETURN NIL)))
                ((LAMBDA (AN) (SETQ NEWJ (LTO_INSERT (NTH (CDR AN) 5) NEWJ)))
                 (CAR AN))
                (SETQ AN (CDR AN))
                (GO LAB)))
             (T
              (PROG (AN)
                (SETQ AN ANS)
               LAB
                (COND ((NULL AN) (RETURN NIL)))
                ((LAMBDA (AN) (SETQ NEWJ (LTO_INSERT (CADR (CDR AN)) NEWJ)))
                 (CAR AN))
                (SETQ AN (CDR AN))
                (GO LAB)))))))
         (COND
          ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD))
           (PROGN
            (IOTO_PRIN2 "] ")
            (COND ((AND *RLQEDFS (NULL CVL)) (IOTO_PRIN2 ". ")))))))
        (GO WHILELABEL))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[DEL:" DELC "/" COUNT "]"))))
      (RETURN
       (COND (ANSW NEWJ)
             (T
              (COND ((AND NEWJ (CDR NEWJ)) (CONS 'OR NEWJ))
                    ((NULL NEWJ) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR NEWJ)))))))) 
(PUT 'PASF_VARSEL 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_VARSEL 'DEFINED-ON-LINE '327) 
(PUT 'PASF_VARSEL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_VARSEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_VARSEL (VARL PSI)
    (PROG (OVARL ATL BESTV BESTW W)
      (SETQ BESTW 0)
      (SETQ W 0)
      (COND ((NOT *RLQEVARSEL) (RETURN VARL)))
      (COND ((NULL (CDR VARL)) (RETURN VARL)))
      (SETQ OVARL VARL)
      (SETQ ATL (CL_ATL PSI))
      (SETQ BESTV (CAR VARL))
      (SETQ BESTW (PASF_VARWEIGHT BESTV ATL))
      (SETQ VARL (CDR VARL))
      (PROG ()
       WHILELABEL
        (COND ((NOT VARL) (RETURN NIL)))
        (PROGN
         (SETQ W (PASF_VARWEIGHT (CAR VARL) ATL))
         (COND
          ((LESSP W BESTW) (PROGN (SETQ BESTV (CAR VARL)) (SETQ BESTW W))))
         (SETQ VARL (CDR VARL)))
        (GO WHILELABEL))
      (RETURN (CONS BESTV (DELETE BESTV OVARL))))) 
(PUT 'PASF_VARWEIGHT 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_VARWEIGHT 'DEFINED-ON-LINE '352) 
(PUT 'PASF_VARWEIGHT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_VARWEIGHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_VARWEIGHT (X ATL)
    (PROG (ATF M XOCCUR WGT)
      (SETQ M 0)
      (SETQ XOCCUR 0)
      (SETQ WGT 0)
      (SETQ M 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT ATL) (RETURN NIL)))
        (PROGN
         (SETQ ATF (CAR ATL))
         (COND
          ((MEMQ X (CAR (CL_VARL ATF)))
           (PROGN
            (COND
             ((AND (PAIRP ATF) (PAIRP (CAR ATF))
                   (MEMQ (CAAR ATF) '(CONG NCONG)))
              (PROGN (SETQ M (TIMES M (CDAR ATF)))))
             ((NEQ (COND ((ATOM ATF) ATF) (T (CAR ATF))) 'EQUAL)
              (SETQ WGT (PLUS WGT (PASF_ABSLC (CADR ATF) X)))))
            (SETQ XOCCUR (PLUS XOCCUR 1)))))
         (SETQ ATL (CDR ATL)))
        (GO WHILELABEL))
      (SETQ WGT (TIMES WGT M XOCCUR))
      (RETURN WGT))) 
(PUT 'PASF_ABSLC 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_ABSLC 'DEFINED-ON-LINE '374) 
(PUT 'PASF_ABSLC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_ABSLC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_ABSLC (F X)
    (PROG (OLDO RES)
      (SETQ OLDO (SETKORDER (LIST X)))
      (SETQ RES (ABS (CDAR (REORDER F))))
      (SETKORDER OLDO)
      (RETURN RES))) 
(PUT 'PASF_QEEX 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_QEEX 'DEFINED-ON-LINE '383) 
(PUT 'PASF_QEEX 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QEEX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QEEX (PSI X THEO ANSW CVLM P)
    (PROG (ESET DEC F RES PCC TMP)
      (SETQ PSI (PASF_PNF PSI))
      (COND
       ((NOT (MEMQ X (CL_FVARL1 PSI)))
        (PROGN
         (COND
          ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD)) (IOTO_PRIN2 "*")))
         (RETURN
          (LIST
           (LIST 'CE CVLM PSI NIL NIL
                 (ANSW_NEW PSI NIL
                  (COND
                   (ANSW
                    (CONS (LIST 'EQUAL (CAR (SIMP X)) (SIMP 0))
                          (ANSW_TL ANSW)))
                   (T NIL)))))))))
      (COND ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD)) (IOTO_PRIN2 "e")))
      (SETQ DEC
              (COND (*RLPASFGAUSS (PASF_GAUSSDEC PSI X THEO))
                    (T (CONS NIL PSI))))
      (COND
       ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD) (CAR DEC))
        (IOTO_PRIN2 "g")))
      (SETQ F (CL_SIMPL (CDR DEC) THEO (MINUS 1)))
      (COND
       ((NOT (MEMQ X (CL_FVARL1 F)))
        (PROGN
         (COND
          ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD)) (IOTO_PRIN2 "#")))))
       (T (SETQ ESET (PASF_ELIMSET F X THEO P))))
      (SETQ PCC 0)
      (SETQ RES
              (APPEND
               (COND
                ((AND (NULL ESET) (NEQ F 'FALSE))
                 (LIST
                  (ANSW_NEW F NIL
                   (COND
                    (ANSW
                     (CONS (LIST 'EQUAL (CAR (SIMP X)) (SIMP 0))
                           (ANSW_TL ANSW)))
                    (T NIL)))))
                (T
                 (PROG (ELIMPT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELIMPT ESET)
                   (COND ((NULL ELIMPT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELIMPT)
                                       (PASF_VS
                                        (COND
                                         (*RLPASFSC
                                          (PROGN
                                           (SETQ TMP
                                                   (PASF_CONDENSE F
                                                    (ELIMPT_POS ELIMPT)))
                                           (SETQ PCC (PLUS PCC (CDR TMP)))
                                           (CAR TMP)))
                                         (T F))
                                        X ELIMPT))
                                     (CAR ELIMPT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELIMPT (CDR ELIMPT))
                   (COND ((NULL ELIMPT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELIMPT)
                               (PASF_VS
                                (COND
                                 (*RLPASFSC
                                  (PROGN
                                   (SETQ TMP
                                           (PASF_CONDENSE F
                                            (ELIMPT_POS ELIMPT)))
                                   (SETQ PCC (PLUS PCC (CDR TMP)))
                                   (CAR TMP)))
                                 (T F))
                                X ELIMPT))
                             (CAR ELIMPT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
               (PROG (ELIMPT FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ELIMPT (CAR DEC))
                 (COND ((NULL ELIMPT) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ELIMPT)
                                     (PASF_VS
                                      (COND
                                       (*RLPASFGC
                                        (PROGN
                                         (SETQ TMP
                                                 (PASF_CONDENSE PSI
                                                  (ELIMPT_POS ELIMPT)))
                                         (SETQ PCC (PLUS PCC (CDR TMP)))
                                         (CAR TMP)))
                                       (T PSI))
                                      X ELIMPT))
                                   (CAR ELIMPT))
                                  NIL)))
                LOOPLABEL
                 (SETQ ELIMPT (CDR ELIMPT))
                 (COND ((NULL ELIMPT) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ELIMPT)
                             (PASF_VS
                              (COND
                               (*RLPASFGC
                                (PROGN
                                 (SETQ TMP
                                         (PASF_CONDENSE PSI
                                          (ELIMPT_POS ELIMPT)))
                                 (SETQ PCC (PLUS PCC (CDR TMP)))
                                 (CAR TMP)))
                               (T PSI))
                              X ELIMPT))
                           (CAR ELIMPT))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD) (GREATERP PCC 0))
        (PROGN (IOTO_PRIN2 "c") (IOTO_PRIN2 PCC))))
      (SETQ RES
              (PROG (RS FORALL-RESULT FORALL-ENDPTR)
                (SETQ RS RES)
                (COND ((NULL RS) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (RS)
                                    (ANSW_NEW
                                     (COND
                                      (*RLPASFSIMPLIFY
                                       (CL_SIMPL (ANSW_F RS) THEO (MINUS 1)))
                                      (T (ANSW_F RS)))
                                     (ANSW_BL RS) (ANSW_TL RS)))
                                  (CAR RS))
                                 NIL)))
               LOOPLABEL
                (SETQ RS (CDR RS))
                (COND ((NULL RS) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (RS)
                            (ANSW_NEW
                             (COND
                              (*RLPASFSIMPLIFY
                               (CL_SIMPL (ANSW_F RS) THEO (MINUS 1)))
                              (T (ANSW_F RS)))
                             (ANSW_BL RS) (ANSW_TL RS)))
                          (CAR RS))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (PROG (AN FORALL-RESULT FORALL-ENDPTR)
         (SETQ AN RES)
         (COND ((NULL AN) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (AN)
                             (LIST 'CE CVLM (ANSW_F AN) NIL NIL
                                   (ANSW_BACKSUBST AN ANSW)))
                           (CAR AN))
                          NIL)))
        LOOPLABEL
         (SETQ AN (CDR AN))
         (COND ((NULL AN) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (AN)
                     (LIST 'CE CVLM (ANSW_F AN) NIL NIL
                           (ANSW_BACKSUBST AN ANSW)))
                   (CAR AN))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(SWITCH (LIST 'RLQEEXPAND)) 
(OFF1 'RLQEEXPAND) 
(PUT 'PASF_VS 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_VS 'DEFINED-ON-LINE '461) 
(PUT 'PASF_VS 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_VS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_VS (F X ELIMPT)
    (PROG (RES TF BVL SF W)
      (SETQ SF
              (CL_APPLY2ATS1 F 'PASF_VSUBSTATF
                             (LIST X (ELIMPT_DEN ELIMPT) (ELIMPT_NOM ELIMPT)
                                   (ELIMPT_UNIF ELIMPT))))
      (SETQ TF
              ((LAMBDA (G161)
                 (COND ((AND G161 (CDR G161)) (CONS 'AND G161))
                       ((NULL G161) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G161))))
               (LIST SF (ELIMPT_GUARD ELIMPT))))
      (COND
       ((ELIMPT_BVL ELIMPT)
        (PROGN
         (SETQ BVL (ELIMPT_BVL ELIMPT))
         (PROG (BV)
           (SETQ BV BVL)
          LAB
           (COND ((NULL BV) (RETURN NIL)))
           ((LAMBDA (BV)
              (PROGN
               (SETQ TF (LIST 'BEX (CDR BV) TF (CAR BV)))
               (SETQ W (CL_FVARL (CAR BV)))
               (COND
                ((AND *RLQEEXPAND W (NULL (CDR W)))
                 (SETQ TF (PASF_EXPAND TF))))))
            (CAR BV))
           (SETQ BV (CDR BV))
           (GO LAB)))))
      (SETQ RES
              (ANSW_NEW TF
               (PROG (BV FORALL-RESULT FORALL-ENDPTR)
                 (SETQ BV BVL)
                 (COND ((NULL BV) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (BV) (CAR BV)) (CAR BV)) NIL)))
                LOOPLABEL
                 (SETQ BV (CDR BV))
                 (COND ((NULL BV) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (BV) (CAR BV)) (CAR BV)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (LIST
                (LIST 'EQUAL (CAR (SIMP X))
                      (CONS (ELIMPT_NOM ELIMPT) (ELIMPT_DEN ELIMPT))))))
      (RETURN RES))) 
(PUT 'PASF_VSUBSTATF 'NUMBER-OF-ARGS 5) 
(PUT 'PASF_VSUBSTATF 'DEFINED-ON-LINE '488) 
(PUT 'PASF_VSUBSTATF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_VSUBSTATF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_VSUBSTATF (ATF X N_J A_J UNIF)
    (PROG (N_I A_I DC D DEGR)
      (SETQ DC (REPR_ATFNEW ATF X NIL))
      (SETQ DEGR (REPR_LDEG DC))
      (COND
       ((AND (GREATERP DEGR 1) (NOT UNIF))
        (RETURN (PASF_VSUBSTCATF ATF X N_J A_J))))
      (COND
       ((LEQ DEGR 1)
        (PROGN
         (SETQ N_I (REPR_N DC))
         (SETQ A_I (REPR_A DC))
         (COND ((NULL N_I) (RETURN ATF)))
         (SETQ D (PASF_PDP N_J))
         (RETURN
          (COND
           ((AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG)))
            (LIST
             (PASF_MKOP
              (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
                    ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR ATF) N_J))
                    (T (POLY-MULTF (CDAR ATF) N_J))))
             (ADDF
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_I A_J))
                    (T (POLY-MULTF N_I A_J)))
              (NEGF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J A_I))
                     (T (POLY-MULTF N_J A_I)))))
             NIL))
           ((MEMQ (CAR ATF) '(LEQ LESSP GEQ GREATERP))
            (COND
             ((MEMQ D '(PDEF PSDEF))
              (LIST (REPR_OP DC)
                    (ADDF
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_I A_J))
                           (T (POLY-MULTF N_I A_J)))
                     (NEGF
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J A_I))
                            (T (POLY-MULTF N_J A_I)))))
                    NIL))
             ((MEMQ D '(NDEF NSDEF))
              (LIST (ANEGREL (REPR_OP DC))
                    (ADDF
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_I A_J))
                           (T (POLY-MULTF N_I A_J)))
                     (NEGF
                      ((LAMBDA (G162)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G162 A_I))
                               (T (POLY-MULTF G162 A_I))))
                       (NEGF N_J))))
                    NIL))
             ((EQ D 'INDEF)
              (LIST (REPR_OP DC)
                    (ADDF
                     ((LAMBDA (G164)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G164 A_J))
                              (T (POLY-MULTF G164 A_J))))
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_I N_J))
                            (T (POLY-MULTF N_I N_J))))
                     (NEGF
                      ((LAMBDA (G166)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G166 A_I))
                               (T (POLY-MULTF G166 A_I))))
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J N_J))
                             (T (POLY-MULTF N_J N_J))))))
                    NIL))))
           (T
            (LIST (REPR_OP DC)
                  (ADDF
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_I A_J))
                         (T (POLY-MULTF N_I A_J)))
                   (NEGF
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J A_I))
                          (T (POLY-MULTF N_J A_I)))))
                  NIL))))))
       (T (PROGN (RETURN (PASF_SUBAT (LIST (CONS X (PREPF A_J))) ATF)) NIL))))) 
(PUT 'PASF_VSUBSTCATF 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_VSUBSTCATF 'DEFINED-ON-LINE '531) 
(PUT 'PASF_VSUBSTCATF 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_VSUBSTCATF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_VSUBSTCATF (ATF X N_J A_J)
    (PROG (CL CB CBADD LCOEFF)
      (SETQ CL (PASF_COEFLST (CADR ATF) X))
      (SETQ CB (PASF_CAUCHYBNDCL CL))
      (SETQ CBADD
              ((LAMBDA (G168)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G168 CB))
                       (T (POLY-MULTF G168 CB))))
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J N_J))
                     (T (POLY-MULTF N_J N_J)))))
      (SETQ LCOEFF (CAR CL))
      (COND
       ((AND (OR (ATOM (CAR LCOEFF)) (ATOM (CAR (CAR LCOEFF))))
             (EQUAL (REMAINDER (CDR LCOEFF) 2) 0))
        (RETURN
         ((LAMBDA (G175)
            (COND ((AND G175 (CDR G175)) (CONS 'OR G175))
                  ((NULL G175) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G175))))
          (LIST
           ((LAMBDA (G171)
              (COND ((AND G171 (CDR G171)) (CONS 'AND G171))
                    ((NULL G171) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G171))))
            (LIST (LIST 'LEQ (ADDF A_J CBADD) NIL)
                  (LIST (CAR ATF) (CAR LCOEFF) NIL)))
           ((LAMBDA (G173)
              (COND ((AND G173 (CDR G173)) (CONS 'AND G173))
                    ((NULL G173) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G173))))
            (LIST (LIST 'GEQ (ADDF A_J (NEGF CBADD)) NIL)
                  (LIST (CAR ATF) (CAR LCOEFF) NIL))))))))
      (COND
       (*RLQESUBI
        (RETURN
         ((LAMBDA (G181)
            (COND ((AND G181 (CDR G181)) (CONS 'OR G181))
                  ((NULL G181) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G181))))
          (LIST
           ((LAMBDA (G177)
              (COND ((AND G177 (CDR G177)) (CONS 'AND G177))
                    ((NULL G177) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G177))))
            (LIST (LIST 'LEQ (ADDF A_J CBADD) NIL)
                  (PASF_QESUBIAT ATF X 'MINF)))
           ((LAMBDA (G179)
              (COND ((AND G179 (CDR G179)) (CONS 'AND G179))
                    ((NULL G179) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G179))))
            (LIST (LIST 'GEQ (ADDF A_J (NEGF CBADD)) NIL)
                  (PASF_QESUBIAT ATF X 'PINF)))))))
       (T
        (RETURN
         ((LAMBDA (G187)
            (COND ((AND G187 (CDR G187)) (CONS 'OR G187))
                  ((NULL G187) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G187))))
          (LIST
           ((LAMBDA (G183)
              (COND ((AND G183 (CDR G183)) (CONS 'AND G183))
                    ((NULL G183) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G183))))
            (LIST (LIST 'LEQ (ADDF A_J CBADD) NIL)
                  (PASF_SUBAT (LIST (CONS X (PREPF (NEGF CB)))) ATF)))
           ((LAMBDA (G185)
              (COND ((AND G185 (CDR G185)) (CONS 'AND G185))
                    ((NULL G185) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR G185))))
            (LIST (LIST 'GEQ (ADDF A_J (NEGF CBADD)) NIL)
                  (PASF_SUBAT (LIST (CONS X (PREPF CB))) ATF)))))))))) 
(PUT 'PASF_QESUBI 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_QESUBI 'DEFINED-ON-LINE '564) 
(PUT 'PASF_QESUBI 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBI (F V INF) (CL_APPLY2ATS1 F 'PASF_QESUBIAT (LIST V INF))) 
(PUT 'PASF_QESUBIAT 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_QESUBIAT 'DEFINED-ON-LINE '575) 
(PUT 'PASF_QESUBIAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBIAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBIAT (ATF V INF)
    (PROG (OP LHS)
      (COND ((NOT (MEMQ V (PASF_VARLAT ATF))) (RETURN ATF)))
      (SETQ OP (CAR ATF))
      (SETQ LHS (CADR ATF))
      (COND
       ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ))
        (RETURN (PASF_QESUBTRANSEQ OP LHS V))))
      (RETURN (PASF_QESUBIORD OP LHS V INF)))) 
(PUT 'PASF_QESUBTRANSEQ 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_QESUBTRANSEQ 'DEFINED-ON-LINE '591) 
(PUT 'PASF_QESUBTRANSEQ 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBTRANSEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBTRANSEQ (OP LHS V)
    (COND ((EQ OP 'EQUAL) (PASF_QESUBTRANSEQUAL LHS V))
          (T (CL_NNFNOT (PASF_QESUBTRANSEQUAL LHS V))))) 
(PUT 'PASF_QESUBTRANSEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_QESUBTRANSEQUAL 'DEFINED-ON-LINE '602) 
(PUT 'PASF_QESUBTRANSEQUAL 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBTRANSEQUAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBTRANSEQUAL (LHS V) (PASF_QESUBTRANSEQUAL1 (SFTO_REORDER LHS V) V)) 
(PUT 'PASF_QESUBTRANSEQUAL1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_QESUBTRANSEQUAL1 'DEFINED-ON-LINE '609) 
(PUT 'PASF_QESUBTRANSEQUAL1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBTRANSEQUAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBTRANSEQUAL1 (LHS V)
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
(PUT 'PASF_QESUBIORD 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_QESUBIORD 'DEFINED-ON-LINE '624) 
(PUT 'PASF_QESUBIORD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBIORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBIORD (OP F V INF) (PASF_QESUBIORD1 OP (SFTO_REORDER F V) V INF)) 
(PUT 'PASF_QESUBIORD1 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_QESUBIORD1 'DEFINED-ON-LINE '633) 
(PUT 'PASF_QESUBIORD1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_QESUBIORD1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_QESUBIORD1 (OP F V INF)
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
             (LIST (LIST (PASF_MKSTRICT OP) AN NIL)
                   (CONS 'AND
                         (LIST (LIST 'EQUAL AN NIL)
                               (PASF_QESUBIORD1 OP (CDR F) V INF)))))))) 
(PUT 'PASF_CONDENSE 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_CONDENSE 'DEFINED-ON-LINE '654) 
(PUT 'PASF_CONDENSE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_CONDENSE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_CONDENSE (F PL)
    (PROG (R C TMP CM)
      (COND ((NULL PL) (RETURN (CONS F 0))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (PROGN
         (SETQ C 0)
         (PROG (SF)
           (SETQ SF (CDR F))
          LAB
           (COND ((NULL SF) (RETURN NIL)))
           ((LAMBDA (SF)
              (PROGN
               (COND ((EQUAL C (CAR PL)) (SETQ R (PASF_CONDENSE SF (CDR PL)))))
               (SETQ C (PLUS C 1))))
            (CAR SF))
           (SETQ SF (CDR SF))
           (GO LAB))
         (COND
          ((EQUAL C 0)
           (REDERR (LIST "Bug in pasf_condense, reference leads to nothing"))))
         (RETURN (CONS (CAR R) (PLUS (CDR R) (DIFFERENCE C 1)))))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
        (PROGN
         (SETQ C 0)
         (SETQ CM 0)
         (PROG (SF)
           (SETQ SF (CDR F))
          LAB
           (COND ((NULL SF) (RETURN NIL)))
           ((LAMBDA (SF)
              (PROGN
               (COND
                ((EQUAL C (CAR PL))
                 (PROGN
                  (SETQ TMP (PASF_CONDENSE SF (CDR PL)))
                  (SETQ R (CONS (CAR TMP) R))
                  (SETQ CM (CDR TMP))))
                (T (SETQ R (CONS SF R))))
               (SETQ C (PLUS C 1))))
            (CAR SF))
           (SETQ SF (CDR SF))
           (GO LAB))
         (RETURN
          (CONS
           (COND ((AND R (CDR R)) (CONS 'AND R))
                 ((NULL R) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                 (T (CAR R)))
           CM)))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'BEX)
        (PROGN
         (SETQ TMP (PASF_CONDENSE (CADDR F) (CDR PL)))
         (RETURN
          (CONS
           (LIST (COND ((ATOM F) F) (T (CAR F))) (CADR F) (CAR TMP) (CADDDR F))
           (CDR TMP))))))
      (RETURN (CONS F 0)))) 
(PUT 'PASF_ELIMSET 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_ELIMSET 'DEFINED-ON-LINE '708) 
(PUT 'PASF_ELIMSET 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_ELIMSET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_ELIMSET (F X THEO P)
    (PROG (REPRL REPRLS M TEMPM PDP RL RES VL TZ TOC)
      (COND
       ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD) (NEQ P (SIMP 1)))
        (IOTO_PRIN2 "p")))
      (SETQ REPRLS (PASF_REP F X))
      (SETQ VL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND
                 ((MINUSP
                   (DIFFERENCE (PLUS (LENGTH (FDEC_BVL (CAR REPRLS))) 1) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W)
                                    (PROGN
                                     (COND (*RLGENSYMINTERN (INTERN W))
                                           (T (REMOB W)))
                                     W))
                                  (COMPRESS
                                   (CONS '!
                                         (CONS '_
                                               (CONS 'K
                                                     (EXPLODE
                                                      (SETCDR RLGENSYMFAST*
                                                              (PLUS
                                                               (CDR
                                                                RLGENSYMFAST*)
                                                               1))))))))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP
                   (DIFFERENCE (PLUS (LENGTH (FDEC_BVL (CAR REPRLS))) 1) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (W)
                            (PROGN
                             (COND (*RLGENSYMINTERN (INTERN W)) (T (REMOB W)))
                             W))
                          (COMPRESS
                           (CONS '!
                                 (CONS '_
                                       (CONS 'K
                                             (EXPLODE
                                              (SETCDR RLGENSYMFAST*
                                                      (PLUS (CDR RLGENSYMFAST*)
                                                            1))))))))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD)
             (GREATERP (LENGTH (CDR REPRLS)) 1))
        (PROGN (IOTO_PRIN2 "s") (IOTO_PRIN2 (LENGTH (CDR REPRLS))))))
      (PROG (REPRL)
        (SETQ REPRL (CDR REPRLS))
       LAB
        (COND ((NULL REPRL) (RETURN NIL)))
        ((LAMBDA (REPRL)
           (PROGN
            (SETQ M 1)
            (SETQ RL NIL)
            (SETQ TOC T)
            (PROG (REPR)
              (SETQ REPR REPRL)
             LAB
              (COND ((NULL REPR) (RETURN NIL)))
              ((LAMBDA (REPR)
                 (COND
                  ((REPR_N REPR)
                   (PROGN
                    (COND
                     ((AND (PAIRP (REPR_OP REPR))
                           (MEMQ (CAR (REPR_OP REPR)) '(CONG NCONG)))
                      (PROGN
                       (SETQ TEMPM (CDR (REPR_OP REPR)))
                       (SETQ PDP (PASF_PDP TEMPM))
                       (SETQ M
                               (COND ((EQ PDP 'PDEF) (LCM M TEMPM))
                                     ((EQ PDP 'NDEF) (LCM M (NEGF TEMPM)))
                                     ((EQ PDP 'PSDEF) (LCM M (ADDF TEMPM 1)))
                                     ((EQ PDP 'NSDEF)
                                      (LCM M (ADDF (NEGF TEMPM) 1)))
                                     (T
                                      (LCM M
                                           (ADDF
                                            (COND
                                             (*PHYSOP-LOADED
                                              (PHYSOP-MULTF TEMPM TEMPM))
                                             (T (POLY-MULTF TEMPM TEMPM)))
                                            1)))))
                       (COND
                        ((NOT (MEMQ PDP '(PDEF NDEF)))
                         (PROGN (SETQ TOC T) (SETQ RL (CONS REPR RL)))))))
                     (T (SETQ RL (CONS REPR RL))))))))
               (CAR REPR))
              (SETQ REPR (CDR REPR))
              (GO LAB))
            (SETQ RES
                    (APPEND (PASF_TESTPT (FDEC_BVL (CAR REPRLS)) RL M VL TOC P)
                            RES))))
         (CAR REPRL))
        (SETQ REPRL (CDR REPRL))
        (GO LAB))
      (SETQ TZ (LENGTH RES))
      (SETQ RES (COND (*RLPASFCONF (PASF_CONFLATE RES)) (T RES)))
      (COND
       ((AND *RLVERBOSE (OR (NOT *RLQEDFS) *RLQEVBOLD) *RLPASFCONF
             (GREATERP (DIFFERENCE TZ (LENGTH RES)) 0))
        (PROGN (IOTO_PRIN2 "t") (IOTO_PRIN2 (DIFFERENCE TZ (LENGTH RES))))))
      (COND ((NULL RES) (REDERR (LIST "error in elimination set creation"))))
      (RETURN
       (COND
        ((AND (PASF_UPRAP F) (NOT (PASF_UNIVNLFP F X)))
         (CONS (ELIMPT_NEW NIL 'TRUE NIL 1 NIL NIL) RES))
        (T RES))))) 
(PUT 'PASF_TESTPT 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_TESTPT 'DEFINED-ON-LINE '780) 
(PUT 'PASF_TESTPT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_TESTPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_TESTPT (B L M VL TOC P)
    (PROG (V RES CP NSV RND RNG N)
      (SETQ V (CAR VL))
      (SETQ NSV (CAR (SIMP V)))
      (SETQ RES
              (COND
               ((OR (NULL L) (NULL TOC))
                (COND ((NEQ P (SIMP 1)) (PASF_TESTPTPQE NIL 0 1 0 M P NIL))
                      (T
                       (LIST
                        (ELIMPT_NEW NIL 'TRUE (CAR (SIMP V)) 1
                         (LIST
                          (CONS
                           ((LAMBDA (G189)
                              (COND ((AND G189 (CDR G189)) (CONS 'OR G189))
                                    ((NULL G189)
                                     (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                    (T (CAR G189))))
                            (LIST (PASF_MKRNG (CAR (SIMP V)) NIL M)
                                  (PASF_MKRNG NSV NIL (NEGF M))))
                           V))
                         NIL)))))))
      (PROG (REPR)
        (SETQ REPR L)
       LAB
        (COND ((NULL REPR) (RETURN NIL)))
        ((LAMBDA (REPR)
           (PROGN
            (COND
             ((EQUAL (REPR_LDEG REPR) 0)
              (REDERR
               (LIST "pasf_testpt: representant with leading degree 0"))))
            (COND
             ((AND (NEQ P (SIMP 1)) (EQUAL (REPR_LDEG REPR) 1))
              (SETQ RES
                      (PASF_TESTPTPQE (REPR_POS REPR) (REPR_R REPR)
                       (REPR_N REPR) (MINUS (TIMES M (REPR_N REPR)))
                       (TIMES M (REPR_N REPR)) P T)))
             ((EQUAL (REPR_LDEG REPR) 1)
              (SETQ RES
                      (CONS
                       (ELIMPT_NEW (REPR_POS REPR)
                        ((LAMBDA (G191)
                           (COND ((AND G191 (CDR G191)) (CONS 'AND G191))
                                 ((NULL G191)
                                  (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                 (T (CAR G191))))
                         (LIST (LIST 'NEQ (REPR_N REPR) NIL)
                               (LIST (CONS 'CONG (REPR_N REPR))
                                     (ADDF (REPR_R REPR) NSV) NIL)))
                        (ADDF (REPR_R REPR) NSV) (REPR_N REPR)
                        (PASF_SUBSTB B (REPR_T REPR) V M (REPR_N REPR)
                         (CDR VL))
                        NIL)
                       RES)))
             (T
              (PROGN
               (SETQ CP (ADDF (PASF_CAUCHYBNDCL (REPR_CL REPR)) M))
               (SETQ RES
                       (CONS
                        (ELIMPT_NEW (REPR_POS REPR) 'TRUE NSV 1
                         (LIST (CONS (PASF_MKRNG NSV (NEGF CP) CP) V)) T)
                        RES))
               NIL)))))
         (CAR REPR))
        (SETQ REPR (CDR REPR))
        (GO LAB))
      (RETURN RES))) 
(PUT 'PASF_TESTPTPQE 'NUMBER-OF-ARGS 7) 
(PUT 'PASF_TESTPTPQE 'DEFINED-ON-LINE '837) 
(PUT 'PASF_TESTPTPQE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_TESTPTPQE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PASF_TESTPTPQE (POS NOM DEN A B P G)
    (COND (*RLPQEOLD (PASF_TESTPTPQEOLD POS NOM DEN A B P G))
          (T (PASF_TESTPTPQENEW POS NOM DEN A B P G)))) 
(PUT 'PASF_TESTPTPQENEW 'NUMBER-OF-ARGS 7) 
(PUT 'PASF_TESTPTPQENEW 'DEFINED-ON-LINE '849) 
(PUT 'PASF_TESTPTPQENEW 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_TESTPTPQENEW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PASF_TESTPTPQENEW (POS NOM DEN A B P G)
    (PROG (N R RES)
      (SETQ R
              (PASF_MKRNDF B
                           ((LAMBDA (W)
                              (PROGN
                               (COND (*RLGENSYMINTERN (INTERN W))
                                     (T (REMOB W)))
                               W))
                            (COMPRESS
                             (CONS '!
                                   (CONS '_
                                         (CONS 'K
                                               (EXPLODE
                                                (SETCDR RLGENSYMFAST*
                                                        (PLUS
                                                         (CDR RLGENSYMFAST*)
                                                         1))))))))))
      (SETQ RES
              (LIST
               (ELIMPT_NEW POS
                (COND
                 (G
                  ((LAMBDA (G193)
                     (COND ((AND G193 (CDR G193)) (CONS 'AND G193))
                           ((NULL G193)
                            (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR G193))))
                   (LIST (LIST 'NEQ DEN NIL)
                         (LIST (CONS 'CONG DEN) (ADDF NOM R) NIL))))
                 (T 'TRUE))
                (ADDF NOM R) DEN NIL NIL)
               (ELIMPT_NEW POS
                (COND
                 (G
                  ((LAMBDA (G195)
                     (COND ((AND G195 (CDR G195)) (CONS 'AND G195))
                           ((NULL G195)
                            (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                           (T (CAR G195))))
                   (LIST (LIST 'NEQ DEN NIL)
                         (LIST (CONS 'CONG DEN) (ADDF NOM (NEGF R)) NIL))))
                 (T 'TRUE))
                (ADDF NOM (NEGF R)) DEN NIL NIL)))
      (RETURN RES))) 
(PUT 'PASF_TESTPTPQEOLD 'NUMBER-OF-ARGS 7) 
(PUT 'PASF_TESTPTPQEOLD 'DEFINED-ON-LINE '873) 
(PUT 'PASF_TESTPTPQEOLD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_TESTPTPQEOLD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PASF_TESTPTPQEOLD (POS NOM DEN A B P G)
    (PROG (N RND RES)
      (SETQ N
              (MAX2
               (CEILING
                (DIFFERENCE
                 (QUOTIENT
                  (LN (DIFFERENCE 1.0 (TIMES (CAR P) (QUOTIENT 1.0 (CDR P)))))
                  (LN
                   (DIFFERENCE 1.0 (QUOTIENT 1.0 (PLUS (DIFFERENCE B A) 1)))))
                 1))
               1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ RND (CAR (SIMP (PLUS (RANDOM (PLUS (DIFFERENCE B A) 1)) A))))
         (SETQ RES
                 (CONS
                  (ELIMPT_NEW POS
                   (COND
                    (G
                     ((LAMBDA (G197)
                        (COND ((AND G197 (CDR G197)) (CONS 'AND G197))
                              ((NULL G197)
                               (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                              (T (CAR G197))))
                      (LIST (LIST 'NEQ DEN NIL)
                            (LIST (CONS 'CONG DEN) (ADDF NOM RND) NIL))))
                    (T 'TRUE))
                   (ADDF NOM RND) DEN NIL NIL)
                  RES)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN RES))) 
(PUT 'PASF_SUBSTB 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_SUBSTB 'DEFINED-ON-LINE '895) 
(PUT 'PASF_SUBSTB 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_SUBSTB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SUBSTB (B TERM V M N_J VL)
    (PROG (NB NV NT1 NT2 RES SB NBB TMP PDP)
      (PROG (BND)
        (SETQ BND B)
       LAB
        (COND ((NULL BND) (RETURN NIL)))
        ((LAMBDA (BND)
           (PROGN
            (SETQ SB (CONS (CONS (CDR BND) (CAR VL)) SB))
            (SETQ VL (CDR VL))))
         (CAR BND))
        (SETQ BND (CDR BND))
        (GO LAB))
      (SETQ TERM (CAR (SUBF TERM SB)))
      (PROG (BND)
        (SETQ BND B)
       LAB
        (COND ((NULL BND) (RETURN NIL)))
        ((LAMBDA (BND)
           (PROGN
            (SETQ NBB (CAR BND))
            (SETQ NV NIL)
            (PROG (S)
              (SETQ S SB)
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S)
                 (PROGN
                  (COND ((EQ (CAR S) (CDR BND)) (SETQ NV (CDR S))))
                  (SETQ NBB (PASF_SUBFOF (CAR S) (CDR S) NBB))))
               (CAR S))
              (SETQ S (CDR S))
              (GO LAB))
            (COND ((NULL NV) (REDERR (LIST "bug in bound substitution"))))
            (SETQ NB (CONS (CONS NBB NV) NB))))
         (CAR BND))
        (SETQ BND (CDR BND))
        (GO LAB))
      (COND
       (*RLPASFBAPPROX
        (PROGN
         (SETQ TMP (PASF_BAPPROX NB TERM V M N_J))
         (COND (TMP (RETURN TMP))))))
      (SETQ NT1
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J M))
                    (T (POLY-MULTF N_J M))))
      (SETQ NT2
              ((LAMBDA (G198)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G198 M))
                       (T (POLY-MULTF G198 M))))
               (NEGF N_J)))
      (SETQ PDP (PASF_PDP N_J))
      (SETQ RES
              ((LAMBDA (G201)
                 (COND ((AND G201 (CDR G201)) (CONS 'OR G201))
                       ((NULL G201) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G201))))
               (COND
                ((EQ PDP 'PDEF)
                 (LIST
                  (PASF_MKRNG (ADDF (CAR (SIMP V)) (NEGF TERM)) (NEGF NT1)
                              NT1)))
                ((EQ PDP 'NDEF)
                 (LIST
                  (PASF_MKRNG (ADDF (CAR (SIMP V)) (NEGF TERM)) (NEGF NT2)
                              NT2)))
                (T
                 (LIST
                  (PASF_MKRNG (ADDF (CAR (SIMP V)) (NEGF TERM)) (NEGF NT1) NT1)
                  (PASF_MKRNG (ADDF (CAR (SIMP V)) (NEGF TERM)) (NEGF NT2)
                              NT2))))))
      (RETURN (CONS (CONS RES V) (REVERSE NB))))) 
(PUT 'PASF_BAPPROX 'NUMBER-OF-ARGS 5) 
(PUT 'PASF_BAPPROX 'DEFINED-ON-LINE '944) 
(PUT 'PASF_BAPPROX 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_BAPPROX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_BAPPROX (B TERM V L N_J)
    (PROG (TMIN TMAX TMP FLAG TPOOL TNPOOL RES FVL)
      (COND ((NULL (OR (ATOM L) (ATOM (CAR L)))) (RETURN NIL)))
      (COND ((NULL (OR (ATOM N_J) (ATOM (CAR N_J)))) (RETURN NIL)))
      (COND ((PASF_TERMP TERM NIL) (RETURN NIL)))
      (SETQ TPOOL (LIST TERM))
      (PROG (BND)
        (SETQ BND B)
       LAB
        (COND ((NULL BND) (RETURN NIL)))
        ((LAMBDA (BND)
           (PROGN
            (SETQ FVL (CL_FVARL (CAR BND)))
            (COND ((GREATERP (LENGTH FVL) 1) (SETQ FLAG T)))
            (COND
             ((AND (EQUAL (LENGTH FVL) 1) (NEQ (CAR FVL) (CDR BND)))
              (REDERROR (LIST "bug in bound approximation"))))
            (COND
             ((NULL FLAG)
              (PROGN
               (SETQ TMP (PASF_BRNG (CAR BND) (CDR BND)))
               (SETQ TNPOOL NIL)
               (PROG (TM)
                 (SETQ TM TPOOL)
                LAB
                 (COND ((NULL TM) (RETURN NIL)))
                 ((LAMBDA (TM)
                    (PROGN
                     (SETQ TNPOOL
                             (CONS
                              (CAR (SUBF TM (LIST (CONS (CDR BND) (CAR TMP)))))
                              TNPOOL))
                     (SETQ TNPOOL
                             (CONS
                              (CAR (SUBF TM (LIST (CONS (CDR BND) (CDR TMP)))))
                              TNPOOL))))
                  (CAR TM))
                 (SETQ TM (CDR TM))
                 (GO LAB)))))
            (SETQ TPOOL TNPOOL)))
         (CAR BND))
        (SETQ BND (CDR BND))
        (GO LAB))
      (COND (FLAG (RETURN NIL)))
      (SETQ TMAX 'MINF)
      (SETQ TMIN 'PINF)
      (PROG (TM)
        (SETQ TM TPOOL)
       LAB
        (COND ((NULL TM) (RETURN NIL)))
        ((LAMBDA (TM)
           (PROGN
            (COND ((PASF_LEQP TM TMIN) (SETQ TMIN TM)))
            (COND ((PASF_LEQP TMAX TM) (SETQ TMAX TM)))))
         (CAR TM))
        (SETQ TM (CDR TM))
        (GO LAB))
      (COND ((MINUSF N_J) (SETQ N_J (NEGF N_J))))
      (COND ((MINUSF L) (SETQ L (NEGF L))))
      (SETQ RES
              (PASF_MKRNG (CAR (SIMP V))
                          (ADDF TMIN
                                (NEGF
                                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J L))
                                       (T (POLY-MULTF N_J L)))))
                          (ADDF TMAX
                                (COND (*PHYSOP-LOADED (PHYSOP-MULTF N_J L))
                                      (T (POLY-MULTF N_J L))))))
      (RETURN (LIST (CONS RES V))))) 
(PUT 'PASF_CONFLATE 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_CONFLATE 'DEFINED-ON-LINE '990) 
(PUT 'PASF_CONFLATE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_CONFLATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_CONFLATE (ELSL)
    (PROG (TMP RES)
      (PROG ()
       WHILELABEL
        (COND ((NOT ELSL) (RETURN NIL)))
        (PROGN
         (SETQ TMP (PASF_CONFLATE1 (CDR ELSL) (CAR ELSL)))
         (SETQ RES (CONS (CAR TMP) RES))
         (SETQ ELSL (CDR TMP)))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'PASF_CONFLATE1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_CONFLATE1 'DEFINED-ON-LINE '1003) 
(PUT 'PASF_CONFLATE1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_CONFLATE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_CONFLATE1 (ELSL ELS1)
    (PROG (R REV1 REV2)
      (PROG (ELS2)
        (SETQ ELS2 ELSL)
       LAB
        (COND ((NULL ELS2) (RETURN NIL)))
        ((LAMBDA (ELS2)
           (PROGN
            (COND
             ((AND (EQUAL (ELIMPT_NOM ELS1) (ELIMPT_NOM ELS2))
                   (EQUAL (ELIMPT_DEN ELS1) (ELIMPT_DEN ELS2))
                   (EQUAL (ELIMPT_GUARD ELS1) (ELIMPT_GUARD ELS2))
                   (EQUAL (ELIMPT_UNIF ELS1) (ELIMPT_UNIF ELS2)))
              (PROGN
               (SETQ REV1 (ELIMPT_BVL ELS1))
               (SETQ REV2 (ELIMPT_BVL ELS2))
               (SETQ ELS1
                       (ELIMPT_NEW (ELIMPT_CPOS ELS1 ELS2) (ELIMPT_GUARD ELS1)
                        (ELIMPT_NOM ELS1) (ELIMPT_DEN ELS1)
                        (COND
                         ((AND REV1 REV2)
                          (CONS
                           (CONS (PASF_SSMK2 'OR (CAAR REV1) (CAAR REV2))
                                 (CDAR REV1))
                           (CDR REV1)))
                         (REV1 REV1) (T REV2))
                        (ELIMPT_UNIF ELS1)))))
             (T (SETQ R (CONS ELS2 R))))))
         (CAR ELS2))
        (SETQ ELS2 (CDR ELS2))
        (GO LAB))
      (RETURN (CONS ELS1 R)))) 
(PUT 'PASF_SSMK2 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_SSMK2 'DEFINED-ON-LINE '1030) 
(PUT 'PASF_SSMK2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_SSMK2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SSMK2 (OP A1 A2)
    (COND ((EQUAL A1 A2) A1)
          ((AND (EQ (COND ((ATOM A1) A1) (T (CAR A1))) OP)
                (EQ (COND ((ATOM A2) A2) (T (CAR A2))) OP))
           (CONS OP (APPEND (CDR A1) (CDR A2))))
          ((EQ (COND ((ATOM A1) A1) (T (CAR A1))) OP)
           (CONS OP (CONS A2 (CDR A1))))
          ((EQ (COND ((ATOM A2) A2) (T (CAR A2))) OP)
           (CONS OP (CONS A1 (CDR A2))))
          (T (CONS OP (LIST A1 A2))))) 
(PUT 'PASF_REP 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_REP 'DEFINED-ON-LINE '1045) 
(PUT 'PASF_REP 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_REP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_REP (F X)
    (PROG (FDEC BALL)
      (SETQ FDEC (FDEC_NEW F X))
      (PROG (B)
        (SETQ B (FDEC_BOPL FDEC))
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B) (COND ((EQ B 'BALL) (SETQ BALL T)))) (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (RETURN
       (COND
        ((AND *RLPASFSES (NULL BALL))
         (CONS FDEC
               (PASF_SES (FDEC_MAT FDEC) X (FDEC_POS FDEC) (FDEC_BVL FDEC))))
        (T
         (CONS FDEC
               (LIST
                (PASF_REP1 (FDEC_MAT FDEC) X (FDEC_POS FDEC)
                 (FDEC_BVL FDEC))))))))) 
(PUT 'PASF_REP1 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_REP1 'DEFINED-ON-LINE '1063) 
(PUT 'PASF_REP1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_REP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_REP1 (F X POS BVL)
    (PROG (N RES)
      (SETQ N 0)
      (COND
       ((OR
         ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
          (COND ((ATOM F) F) (T (CAR F))))
         ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (REDERR
         (LIST "pasf_canrep : quantifier illegal inside a formula's matrix"))))
      (COND
       (((LAMBDA (X)
           (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
               (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV))))
         (COND ((ATOM F) F) (T (CAR F))))
        (PROGN
         (PROG (ARG)
           (SETQ ARG (CDR F))
          LAB
           (COND ((NULL ARG) (RETURN NIL)))
           ((LAMBDA (ARG)
              (PROGN
               (SETQ RES (APPEND (PASF_REP1 ARG X NIL BVL) RES))
               (SETQ N (PLUS N 1))))
            (CAR ARG))
           (SETQ ARG (CDR ARG))
           (GO LAB))
         (RETURN RES))))
      (COND
       ((AND (AND (PAIRP F) (PAIRP (CAR F)) (MEMQ (CAAR F) '(CONG NCONG)))
             (MEMQ X (KERNELS (CDAR F))))
        (REDERR (LIST "Quantified variable " X " is not allowed in modulus"))))
      (RETURN (LIST (REPR_ATFBNEW F X NIL BVL))))) 
(PUT 'PASF_SES 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_SES 'DEFINED-ON-LINE '1090) 
(PUT 'PASF_SES 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_SES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_SES (F X POS BVL)
    (PROG (N RES TMP LMAX SMAX)
      (SETQ N 0)
      (COND
       ((OR
         ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
          (COND ((ATOM F) F) (T (CAR F))))
         ((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
          (COND ((ATOM F) F) (T (CAR F)))))
        (REDERR (LIST "bug in pasf_canrep"))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
        (PROGN
         (SETQ LMAX 0)
         (PROG (ARG)
           (SETQ ARG (CDR F))
          LAB
           (COND ((NULL ARG) (RETURN NIL)))
           ((LAMBDA (ARG)
              (PROGN
               (SETQ TMP (PASF_SES ARG X (APPEND POS (LIST N)) BVL))
               (COND
                ((GREATERP (LENGTH TMP) LMAX)
                 (PROGN
                  (PROG (SM)
                    (SETQ SM SMAX)
                   LAB
                    (COND ((NULL SM) (RETURN NIL)))
                    ((LAMBDA (SM) (SETQ RES (APPEND SM RES))) (CAR SM))
                    (SETQ SM (CDR SM))
                    (GO LAB))
                  (SETQ LMAX (LENGTH TMP))
                  (SETQ SMAX TMP)))
                (T
                 (PROG (SM)
                   (SETQ SM TMP)
                  LAB
                   (COND ((NULL SM) (RETURN NIL)))
                   ((LAMBDA (SM) (SETQ RES (APPEND SM RES))) (CAR SM))
                   (SETQ SM (CDR SM))
                   (GO LAB))))
               (SETQ N (PLUS N 1))))
            (CAR ARG))
           (SETQ ARG (CDR ARG))
           (GO LAB))
         (RETURN
          (PROG (ESL FORALL-RESULT FORALL-ENDPTR)
            (SETQ ESL SMAX)
            (COND ((NULL ESL) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ESL)
                                (APPEND ESL
                                        (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ R RES)
                                          (COND ((NULL R) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (R)
                                                              (REPR_SETPOS R
                                                               (REPR_POS
                                                                (CAR ESL))))
                                                            (CAR R))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ R (CDR R))
                                          (COND
                                           ((NULL R) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (R)
                                                      (REPR_SETPOS R
                                                       (REPR_POS (CAR ESL))))
                                                    (CAR R))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))
                              (CAR ESL))
                             NIL)))
           LOOPLABEL
            (SETQ ESL (CDR ESL))
            (COND ((NULL ESL) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (ESL)
                        (APPEND ESL
                                (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ R RES)
                                  (COND ((NULL R) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (R)
                                                      (REPR_SETPOS R
                                                       (REPR_POS (CAR ESL))))
                                                    (CAR R))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ R (CDR R))
                                  (COND ((NULL R) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (R)
                                              (REPR_SETPOS R
                                               (REPR_POS (CAR ESL))))
                                            (CAR R))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))
                      (CAR ESL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (PROGN
         (PROG (ARG)
           (SETQ ARG (CDR F))
          LAB
           (COND ((NULL ARG) (RETURN NIL)))
           ((LAMBDA (ARG)
              (PROGN
               (SETQ RES
                       (APPEND (PASF_SES ARG X (APPEND POS (LIST N)) BVL) RES))
               (SETQ N (PLUS N 1))))
            (CAR ARG))
           (SETQ ARG (CDR ARG))
           (GO LAB))
         (RETURN RES))))
      (COND
       ((AND (AND (PAIRP F) (PAIRP (CAR F)) (MEMQ (CAAR F) '(CONG NCONG)))
             (MEMQ X (KERNELS (CDAR F))))
        (REDERR (LIST "Quantified variable" X "is not allowed in modulus"))))
      (RETURN (LIST (LIST (REPR_ATFBNEW F X POS BVL)))))) 
(PUT 'PASF_GAUSSDEC 'NUMBER-OF-ARGS 3) 
(PUT 'PASF_GAUSSDEC 'DEFINED-ON-LINE '1132) 
(PUT 'PASF_GAUSSDEC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_GAUSSDEC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_GAUSSDEC (F X THEO)
    (PROG (R FDEC OPL STP VL)
      (SETQ FDEC (FDEC_NEW F X))
      (COND ((PASF_UNIVNLFP (FDEC_MAT FDEC) X) (RETURN (CONS NIL F))))
      (SETQ OPL (FDEC_BOPL FDEC))
      (PROG (OP)
        (SETQ OP OPL)
       LAB
        (COND ((NULL OP) (RETURN NIL)))
        ((LAMBDA (OP) (COND ((EQ OP 'BALL) (SETQ STP T)))) (CAR OP))
        (SETQ OP (CDR OP))
        (GO LAB))
      (COND (STP (RETURN (CONS NIL F))))
      (SETQ VL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND
                 ((MINUSP (DIFFERENCE (PLUS (LENGTH (FDEC_BVL FDEC)) 1) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W)
                                    (PROGN
                                     (COND (*RLGENSYMINTERN (INTERN W))
                                           (T (REMOB W)))
                                     W))
                                  (COMPRESS
                                   (CONS '!
                                         (CONS '_
                                               (CONS 'K
                                                     (EXPLODE
                                                      (SETCDR RLGENSYMFAST*
                                                              (PLUS
                                                               (CDR
                                                                RLGENSYMFAST*)
                                                               1))))))))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (PLUS (LENGTH (FDEC_BVL FDEC)) 1) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (W)
                            (PROGN
                             (COND (*RLGENSYMINTERN (INTERN W)) (T (REMOB W)))
                             W))
                          (COMPRESS
                           (CONS '!
                                 (CONS '_
                                       (CONS 'K
                                             (EXPLODE
                                              (SETCDR RLGENSYMFAST*
                                                      (PLUS (CDR RLGENSYMFAST*)
                                                            1))))))))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R
              (PASF_GAUSSDEC1 (FDEC_MAT FDEC) X THEO (FDEC_POS FDEC)
               (FDEC_BVL FDEC) VL))
      (SETQ F (CADDR R))
      (PROG (BV)
        (SETQ BV (FDEC_BVL FDEC))
       LAB
        (COND ((NULL BV) (RETURN NIL)))
        ((LAMBDA (BV)
           (PROGN
            (SETQ F (LIST (CAR OPL) (CDR BV) F (CAR BV)))
            (SETQ OPL (CDR OPL))))
         (CAR BV))
        (SETQ BV (CDR BV))
        (GO LAB))
      (RETURN (CONS (CADR R) F)))) 
(PUT 'PASF_GAUSSDEC1 'NUMBER-OF-ARGS 6) 
(PUT 'PASF_GAUSSDEC1 'DEFINED-ON-LINE '1160) 
(PUT 'PASF_GAUSSDEC1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_GAUSSDEC1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_GAUSSDEC1 (F X THEO POS BVAR VL)
    (PROG (C TMP R)
      (COND ((EQ F 'FALSE) (RETURN (LIST T NIL F))))
      (COND ((EQ F 'TRUE) (RETURN (LIST NIL NIL F))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'AND)
        (PROGN
         (SETQ C 0)
         (SETQ TMP (LIST NIL NIL NIL))
         (PROG (SF)
           (SETQ SF (CDR F))
          LAB
           (COND ((NULL SF) (RETURN NIL)))
           ((LAMBDA (SF)
              (PROGN
               (SETQ R
                       (PASF_GAUSSDEC1 SF X THEO (APPEND POS (LIST C)) BVAR
                        VL))
               (COND
                ((CAR R)
                 (SETQ TMP
                         (LIST T (PASF_GAUSSESORD (CADR TMP) (CADR R))
                               'FALSE)))
                ((NULL (CAR TMP))
                 (SETQ TMP
                         (LIST NIL (APPEND (CADR TMP) (CADR R))
                               (CONS (CADDR R) (CADDR TMP))))))
               (SETQ C (PLUS C 1))))
            (CAR SF))
           (SETQ SF (CDR SF))
           (GO LAB))
         (COND ((CAR TMP) (RETURN TMP))
               (T
                (RETURN
                 (LIST NIL (CADR TMP)
                       ((LAMBDA (G203)
                          (COND ((AND G203 (CDR G203)) (CONS 'AND G203))
                                ((NULL G203)
                                 (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                (T (CAR G203))))
                        (CADDR TMP)))))))))
      (COND
       ((EQ (COND ((ATOM F) F) (T (CAR F))) 'OR)
        (PROGN
         (SETQ C 0)
         (SETQ TMP (LIST T NIL NIL))
         (PROG (SF)
           (SETQ SF (CDR F))
          LAB
           (COND ((NULL SF) (RETURN NIL)))
           ((LAMBDA (SF)
              (PROGN
               (SETQ R
                       (PASF_GAUSSDEC1 SF X THEO (APPEND POS (LIST C)) BVAR
                        VL))
               (COND
                ((CAR R)
                 (SETQ TMP
                         (LIST (CAR TMP) (APPEND (CADR TMP) (CADR R))
                               (CONS (CADDR R) (CADDR TMP)))))
                (T
                 (SETQ TMP
                         (LIST NIL (APPEND (CADR TMP) (CADR R))
                               (CONS (CADDR R) (CADDR TMP))))))
               (SETQ C (PLUS C 1))))
            (CAR SF))
           (SETQ SF (CDR SF))
           (GO LAB))
         (COND ((CAR TMP) (RETURN (LIST T (CADR TMP) 'FALSE)))
               (T
                (RETURN
                 (LIST NIL (CADR TMP)
                       ((LAMBDA (G205)
                          (COND ((AND G205 (CDR G205)) (CONS 'OR G205))
                                ((NULL G205)
                                 (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                (T (CAR G205))))
                        (CADDR TMP)))))))))
      (COND
       (((LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))
         (COND ((ATOM F) F) (T (CAR F))))
        (REDERR (LIST "Bug in gauss decomposition"))))
      (COND
       ((MEMQ
         (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) F) ((PAIRP (CAR F)) (CAAR F))
               (T (CAR F)))
         '(EQUAL NEQ LEQ GEQ LESSP GREATERP CONG NCONG))
        (COND
         ((EQ
           (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) F) ((PAIRP (CAR F)) (CAAR F))
                 (T (CAR F)))
           'EQUAL)
          (RETURN (PASF_GAUSSDEC2 F X BVAR POS VL)))
         (T (RETURN (LIST NIL NIL F))))))
      (REDERR (LIST "Bug in gauss decomposition. Code assumed dead reached")))) 
(PUT 'PASF_GAUSSDEC2 'NUMBER-OF-ARGS 5) 
(PUT 'PASF_GAUSSDEC2 'DEFINED-ON-LINE '1236) 
(PUT 'PASF_GAUSSDEC2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_GAUSSDEC2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_GAUSSDEC2 (ATF X BVAR POS VL)
    (PROG (REPR A_I B)
      (SETQ REPR (REPR_ATFBNEW ATF X POS BVAR))
      (SETQ A_I (REPR_R REPR))
      (SETQ B (PASF_SUBSTB BVAR (REPR_T REPR) (CAR VL) NIL NIL (CDR VL)))
      (COND (BVAR (SETQ A_I (ADDF A_I (CAR (SIMP (CAR VL)))))))
      (COND
       ((AND (REPR_N REPR)
             ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (REPR_N REPR)))
        (RETURN
         (LIST T
               (LIST
                (ELIMPT_NEW POS
                 (CONS 'AND
                       (LIST (LIST (CONS 'CONG (REPR_N REPR)) A_I NIL)
                             (LIST 'NEQ (REPR_N REPR) NIL)))
                 A_I (REPR_N REPR) (COND (BVAR B) (T NIL)) NIL))
               'FALSE))))
      (RETURN (LIST NIL NIL ATF)))) 
(PUT 'PASF_GAUSSESORD 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_GAUSSESORD 'DEFINED-ON-LINE '1263) 
(PUT 'PASF_GAUSSESORD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFQE.RED) 
(PUT 'PASF_GAUSSESORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_GAUSSESORD (A B)
    (PROG ()
      (COND ((AND (NULL A) B) (RETURN B))
            ((AND (NULL A) (NULL B)) (RETURN NIL))
            ((AND A (NULL B)) (RETURN A))
            ((LESSP (LENGTH (CDAR B)) (LENGTH (CDAR A))) (RETURN B))
            ((GREATERP (LENGTH (CDAR B)) (LENGTH (CDAR A))) (RETURN A)))
      (RETURN B))) 
(ENDMODULE) 