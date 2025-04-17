(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFSMTQE)) 
(REVISION 'OFSFSMTQE
          "$Id: ofsfsmtqe.red 6013 2021-09-09 08:22:53Z thomas-sturm $") 
(COPYRIGHT 'OFSFSMTQE "(c) 2017 M. Kosta, T. Sturm") 
(PUT 'SMTQE_SETFNAL 'NUMBER-OF-ARGS 0) 
(DE SMTQE_SETFNAL NIL
    (PROGN
     (VS_PATCHFNAL 'FN_PC-DECOMPOSITION 'SMTQE_PC-DECOMPOSITION)
     (VS_PATCHFNAL 'FN_APPLYVSTS 'SMTQE_APPLYVSTS))) 
(PUT 'SMTQE_PC-DECOMPOSITION 'NUMBER-OF-ARGS 1) 
(DE SMTQE_PC-DECOMPOSITION (DE)
    (PROG (F GL CGL ATL GPOSL PC PCL N)
      (SETQ N 0)
      (SETQ F (VSDE_F DE))
      (COND (NIL NIL))
      (SETQ GL
              (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                (SETQ SUBF (CDR F))
               STARTOVER
                (COND ((NULL SUBF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (SUBF)
                           (PROGN
                            (SETQ N (PLUS N 1))
                            (QFF_GAUSSPOSL (VSDE_V DE) SUBF (LIST N)
                             (VSDE_BVL DE) (VSDE_CURTHEO DE))))
                         (CAR SUBF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ SUBF (CDR SUBF))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (SUBF)
                           (PROGN
                            (SETQ N (PLUS N 1))
                            (QFF_GAUSSPOSL (VSDE_V DE) SUBF (LIST N)
                             (VSDE_BVL DE) (VSDE_CURTHEO DE))))
                         (CAR SUBF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ SUBF (CDR SUBF))
                (GO LOOPLABEL)))
      (COND ((VSDE_FAILEDALP GL) (SETQ GL NIL))
            (T
             (SETQ F
                     (QFF_REPLACEL F
                      (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                        (SETQ PR GL)
                        (COND ((NULL PR) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR))
                                              NIL)))
                       LOOPLABEL
                        (SETQ PR (CDR PR))
                        (COND ((NULL PR) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      'FALSE))))
      (SETQ N 0)
      (SETQ CGL
              (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                (SETQ SUBF (CDR F))
               STARTOVER
                (COND ((NULL SUBF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (SUBF)
                           (PROGN
                            (SETQ N (PLUS N 1))
                            (QFF_COGAUSSPOSL (VSDE_V DE) SUBF (LIST N)
                             (VSDE_BVL DE) (VSDE_CURTHEO DE))))
                         (CAR SUBF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ SUBF (CDR SUBF))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (SUBF)
                           (PROGN
                            (SETQ N (PLUS N 1))
                            (QFF_COGAUSSPOSL (VSDE_V DE) SUBF (LIST N)
                             (VSDE_BVL DE) (VSDE_CURTHEO DE))))
                         (CAR SUBF)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ SUBF (CDR SUBF))
                (GO LOOPLABEL)))
      (COND ((VSDE_FAILEDALP CGL) (PROGN (VSDE_PUTPCL DE NIL) (RETURN NIL))))
      (SETQ GL (POS_DELSUBPOSAL CGL GL))
      (SETQ F
              (QFF_REPLACEL F
               (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                 (SETQ PR CGL)
                 (COND ((NULL PR) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
                LOOPLABEL
                 (SETQ PR (CDR PR))
                 (COND ((NULL PR) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               'FALSE))
      (SETQ ATL (QFF_ATPOSL (VSDE_V DE) F NIL (VSDE_BVL DE) (VSDE_CURTHEO DE)))
      (COND ((VSDE_FAILEDALP ATL) (PROGN (VSDE_PUTPCL DE NIL) (RETURN NIL))))
      (PROG (PR)
        (SETQ PR ATL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ PC (VSPC_MK (CAR PR) 'AT (CDR PR) GPOSL NIL))
            (PROGN (SETQ PCL (CONS PC PCL)) PC)))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (PROG (PR)
        (SETQ PR CGL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ PC (VSPC_MK (CAR PR) 'COGAUSS (CDR PR) GPOSL NIL))
            (PROGN (SETQ PCL (CONS PC PCL)) PC)))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (PROG (PR)
        (SETQ PR GL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ PC (VSPC_MK (CAR PR) 'GAUSS (CDR PR) GPOSL NIL))
            (PROGN (SETQ PCL (CONS PC PCL)) PC)
            (PROG (W1)
              (SETQ W1 (CAR PR))
              (SETQ GPOSL (CONS W1 GPOSL))
              (RETURN W1))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (VSDE_PUTPCL DE PCL))) 
(PUT 'SMTQE_APPLYVSTS 'NUMBER-OF-ARGS 1) 
(DE SMTQE_APPLYVSTS (DS)
    (PROG (VS F TP TTHEO THEO G)
      (SETQ VS (VSDS_VS DS))
      (SETQ F (VSDS_F DS))
      (SETQ TP (VSTS_TP VS))
      (SETQ TTHEO (VSDS_TTHEO DS))
      (SETQ THEO (APPEND (VSDS_PTHEO DS) TTHEO))
      (PROG (G647)
        (SETQ G647 (VSDS_G2GTT (VSTP_GUARD TP) THEO TTHEO))
        (SETQ G (CAR G647))
        (SETQ TTHEO (CDR G647))
        (RETURN G647))
      (VSDS_PUTTTHEO DS TTHEO)
      (SETQ F (QFF_REPLACEL F (VSTP_GPL TP) 'FALSE))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2 (LIST "+++++ VSUB " (VSVS_V VS) " "))
         (IOTO_PRIN2 "[condense"))))
      (SETQ F (QFF_CONDENSE F (VSTP_P TP)))
      (COND (*RLVERBOSE (PROGN (IOTO_PRIN2 "]") (IOTO_PRIN2 "[substitute"))))
      (SETQ F (CL_APPLY2ATS1 F 'VSDS_APPLYVSTS-AT (LIST DS)))
      (COND (*RLVERBOSE (PROGN (IOTO_PRIN2 "]") (IOTO_PRIN2 "[simplify"))))
      (COND (NIL NIL))
      (SETQ F
              ((LAMBDA (G649)
                 (COND ((AND G649 (CDR G649)) (CONS 'AND G649))
                       ((NULL G649) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G649))))
               (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
                 (SETQ SUBF (CDR F))
                 (COND ((NULL SUBF) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (SUBF) (CONS 'AND (LIST G SUBF)))
                                   (CAR SUBF))
                                  NIL)))
                LOOPLABEL
                 (SETQ SUBF (CDR SUBF))
                 (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (SUBF) (CONS 'AND (LIST G SUBF)))
                           (CAR SUBF))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ F (SMTQE_SIMPL F THEO))
      (COND (*RLVERBOSE (IOTO_PRIN2T "]")))
      (VSDS_PUTRES DS F))) 
(RL_PROVIDESERVICE 'RL_SMTQE 'SMTQE_SMTQE NIL) 
(PUT 'SMTQE_SMTQE 'NUMBER-OF-ARGS 1) 
(DE SMTQE_SMTQE (L)
    (PROG (FORMULA DB)
      (SMTQE_SETFNAL)
      (SETQ FORMULA
              (COND ((AND L (CDR L)) (CONS 'AND L))
                    ((NULL L) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                    (T (CAR L))))
      (SETQ FORMULA (SMTQE_SIMPL FORMULA NIL))
      (SETQ DB (VSDB_MK (CL_FVARL FORMULA) FORMULA NIL NIL T))
      (VS_BLOCKMAINLOOP DB)
      (COND (*OFSFVSQETREE2GML (VSDB_2GML DB RLQETREEGMLFILE*)))
      (RETURN (SMTQE_COLLECTRESULT DB)))) 
(PUT 'SMTQE_SIMPL 'NUMBER-OF-ARGS 2) 
(DE SMTQE_SIMPL (F THEO)
    ((LAMBDA (G651)
       (COND ((AND G651 (CDR G651)) (CONS 'AND G651))
             ((NULL G651) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G651))))
     (PROG (SUBF FORALL-RESULT FORALL-ENDPTR)
       (SETQ SUBF (CDR F))
       (COND ((NULL SUBF) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (SUBF) (CL_SIMPL SUBF THEO (MINUS 1)))
                         (CAR SUBF))
                        NIL)))
      LOOPLABEL
       (SETQ SUBF (CDR SUBF))
       (COND ((NULL SUBF) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (SUBF) (CL_SIMPL SUBF THEO (MINUS 1))) (CAR SUBF))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'SMTQE_COLLECTRESULT 'NUMBER-OF-ARGS 1) 
(DE SMTQE_COLLECTRESULT (DB)
    (PROG (FL VL W UC N)
      (SETQ N 0)
      (COND (NIL NIL))
      (PROG (ND)
        (SETQ ND (VSDB_SC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (PROGN
            (COND (NIL NIL))
            (PROG (W1)
              (SETQ W1 (VSND_F ND))
              (SETQ FL (CONS W1 FL))
              (RETURN W1))))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (SETQ UC (SMTQE_FL2UC FL))
      (RETURN (LIST (CONS 'OR FL) NIL UC)))) 
(PUT 'SMTQE_FL2UC 'NUMBER-OF-ARGS 1) 
(DE SMTQE_FL2UC (FL)
    (PROG (MTX1 MTX ROW C SCMTX UC INP OTIME N)
      (SETQ N 0)
      (COND (*RLVERBOSE (IOTO_TPRIN2 "+++++ unsat core: ")))
      (COND ((NULL FL) (RETURN NIL)))
      (SETQ MTX1
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FL)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (PROG (TV FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ TV (CDR F))
                                      (COND ((NULL TV) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (TV)
                                                          (COND
                                                           ((EQ TV 'FALSE) 1)
                                                           (T 0)))
                                                        (CAR TV))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ TV (CDR TV))
                                      (COND ((NULL TV) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (TV)
                                                  (COND ((EQ TV 'FALSE) 1)
                                                        (T 0)))
                                                (CAR TV))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (PROG (TV FORALL-RESULT FORALL-ENDPTR)
                              (SETQ TV (CDR F))
                              (COND ((NULL TV) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (TV)
                                                  (COND ((EQ TV 'FALSE) 1)
                                                        (T 0)))
                                                (CAR TV))
                                               NIL)))
                             LOOPLABEL
                              (SETQ TV (CDR TV))
                              (COND ((NULL TV) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (TV)
                                          (COND ((EQ TV 'FALSE) 1) (T 0)))
                                        (CAR TV))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N (LENGTH (CAR MTX1)))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST N ", order preprocessing: "))))
      (SETQ MTX1
              (CONS
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I 0)
                 (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (LTO_INT2ID I) NIL)))
                LOOPLABEL
                 (SETQ I (PLUS2 I 1))
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (LTO_INT2ID I) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               MTX1))
      (SETQ MTX1 (LTO_TRANSPOSIP MTX1))
      (PROG (ROW)
        (SETQ ROW MTX1)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (SETQ SCMTX MTX)
            (SETQ C T)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND C SCMTX)) (RETURN NIL)))
              (PROGN
               (COND
                ((LTO_ORDPROD (CDR ROW) (CDR (CAR SCMTX)) 'GEQ)
                 (PROGN (SETCAR SCMTX ROW) (SETQ C NIL)))
                ((LTO_ORDPROD (CDR ROW) (CDR (CAR SCMTX)) 'LEQ) (SETQ C NIL))
                (T (PROG1 (CAR SCMTX) (SETQ SCMTX (CDR SCMTX))))))
              (GO WHILELABEL))
            (COND (C (PROGN (SETQ MTX (CONS ROW MTX)) ROW)))))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (COND (*RLVERBOSE (IOTO_PRIN2 (LIST (LENGTH MTX) ", set cover: "))))
      ((LAMBDA (*RLVERBOSE) (SETQ UC (LTO_SETCOVER MTX))) NIL)
      (COND (*RLVERBOSE (IOTO_PRIN2T (LENGTH UC))))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X UC)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (LTO_ID2INT X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (LTO_ID2INT X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(ENDMODULE) 