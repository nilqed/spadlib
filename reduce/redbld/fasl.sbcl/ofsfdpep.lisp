(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFDPEP)) 
(REVISION 'OFSFDPEP
          "$Id: ofsfdpep.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFDPEP "(c) 1995-2013 A. Dolzmann, T. Sturm") 
(SWITCH (LIST 'RLDPEPVERBOSE 'RLDPEPIVERBOSE)) 
(ON1 'RLDPEPVERBOSE) 
(OFF1 'RLDPEPIVERBOSE) 
(FLUID '(*OFSF_EXPF *RLPEPEVAL)) 
(PUT 'OFSF_DPEPVERBOSEP 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_DPEPVERBOSEP 'DEFINED-ON-LINE '39) 
(PUT 'OFSF_DPEPVERBOSEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DPEPVERBOSEP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OFSF_DPEPVERBOSEP NIL (AND *RLVERBOSE *RLDPEPVERBOSE)) 
(PUT 'OFSF_DPEPIVERBOSEP 'NUMBER-OF-ARGS 0) 
(PUT 'OFSF_DPEPIVERBOSEP 'DEFINED-ON-LINE '43) 
(PUT 'OFSF_DPEPIVERBOSEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DPEPIVERBOSEP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OFSF_DPEPIVERBOSEP NIL (AND *RLVERBOSE *RLDPEPIVERBOSE)) 
(PUT 'OFSF_DPEP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DPEP 'DEFINED-ON-LINE '47) 
(PUT 'OFSF_DPEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DPEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DPEP (OPHI K)
    (PROG (QEXP OPHIEXP PHI PSIPRIME PHIPRIME)
      (SETQ *OFSF_EXPF (INTERN (GENSYM)))
      (SETQ *RLPEPEVAL NIL)
      (COND ((MINUSF K) (REDERR "Accuracy value has to be positive.")))
      (OFSF_PEPCHECK OPHI)
      (COND ((OFSF_EXPFREE OPHI) (RETURN (OFSF_CAD OPHI NIL NIL))))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (IOTO_TPRIN2T "++++ DPEP Preparation Phase")))
      (SETQ QEXP (CONS (CADR OPHI) (COND ((ATOM OPHI) OPHI) (T (CAR OPHI)))))
      (SETQ OPHIEXP
              (CL_APPLY2ATS1 OPHI
                             (FUNCTION
                              (LAMBDA (X QEXP)
                                (LIST (CAR X)
                                      (OFSF_PEPSUBF (CADR X)
                                       (LIST 'EXPT 'E (CAR QEXP)) *OFSF_EXPF)
                                      NIL)))
                             (LIST QEXP)))
      (SETQ PHI (CADDR OPHIEXP))
      (COND
       ((CL_QVARL1 PHI)
        (PROGN
         (COND
          ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
           (PROGN
            (IOTO_TPRIN2T "++++ DPEP QE by CAD")
            (SETQ PSIPRIME (OFSF_CAD PHI NIL NIL))))
          (T
           (PROGN
            (COND
             ((AND *RLVERBOSE *RLCADVERBOSE)
              (PROGN
               (OFF1 'RLVERBOSE)
               (OFF1 'RLCADVERBOSE)
               (SETQ PSIPRIME (OFSF_CAD PHI NIL NIL))
               (ON1 'RLVERBOSE)
               (ON1 'RLCADVERBOSE)))
             (T (SETQ PSIPRIME (OFSF_CAD PHI NIL NIL))))
            NIL)))))
       (T (SETQ PSIPRIME PHI)))
      (SETQ PHIPRIME (LIST (CDR QEXP) (CAR QEXP) PSIPRIME))
      (COND
       ((NULL
         (OR (EQ (LENGTH (CL_QVARL1 PHIPRIME)) 1) (EQ (CADDR PHIPRIME) 'FALSE)
             (EQ (CADDR PHIPRIME) 'TRUE)))
        (REDERR "QE by CAD and decision procedure failed.")))
      (RETURN
       (COND
        ((OR (EQ (CADDR PHIPRIME) 'FALSE) (EQ (CADDR PHIPRIME) 'TRUE))
         (CL_SIMPL (CADDR PHIPRIME) NIL (MINUS 1)))
        (T (OFSF_DUPEP PHIPRIME K)))))) 
(PUT 'OFSF_DUPEP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DUPEP 'DEFINED-ON-LINE '125) 
(PUT 'OFSF_DUPEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DUPEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DUPEP (PHIPRIME K)
    (PROG (QEXP CCR PPR CSB CBASES CONTSB PPRTSB PBASES PSB ILIST ISOL HATLIST
           SPLIST CELLSTOGO TV)
      (SETQ QEXP
              (CONS (CADR PHIPRIME)
                    (COND ((ATOM PHIPRIME) PHIPRIME) (T (CAR PHIPRIME)))))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN (TERPRI) (IOTO_PRIN2T (LIST "++++ Decide UPEP")))))
      (SETKORDER (LIST *OFSF_EXPF (CAR QEXP)))
      (SETQ PHIPRIME
              (CL_APPLY2ATS PHIPRIME
                            (FUNCTION
                             (LAMBDA (X)
                               (LIST (CAR X) (REORDER (CADR X)) NIL)))))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN
         (MAPRINT "P := " 0)
         (MAPRINT
          (CONS 'LIST
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F (CL_TERML PHIPRIME))
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F)
                                      (PREPF
                                       (OFSF_PEPSUBF F *OFSF_EXPF
                                        (LIST 'EXPT 'E (CAR QEXP)))))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F)
                              (PREPF
                               (OFSF_PEPSUBF F *OFSF_EXPF
                                (LIST 'EXPT 'E (CAR QEXP)))))
                            (CAR F))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
          0)
         (TERPRI* NIL))))
      (SETQ CCR
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C (CL_TERML PHIPRIME))
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C) (OFSF_CONTENTY C *OFSF_EXPF))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (C) (OFSF_CONTENTY C *OFSF_EXPF)) (CAR C))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PPR
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CL_TERML PHIPRIME))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (OFSF_PRPARTY P *OFSF_EXPF))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P) (OFSF_PRPARTY P *OFSF_EXPF)) (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CSB
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C CCR)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (C) (SFTO_SQFDECF C)) (CAR C))
                                      NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (C) (SFTO_SQFDECF C)) (CAR C)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PSB
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PPR)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (SFTO_SQFDECF P)) (CAR P))
                                      NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (SFTO_SQFDECF P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (C)
        (SETQ C CSB)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROG (I)
             (SETQ I C)
            LAB
             (COND ((NULL I) (RETURN NIL)))
             ((LAMBDA (I) (SETQ CONTSB (CONS (CAR I) CONTSB))) (CAR I))
             (SETQ I (CDR I))
             (GO LAB)))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (PROG (P)
        (SETQ P PSB)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROG (I)
             (SETQ I P)
            LAB
             (COND ((NULL I) (RETURN NIL)))
             ((LAMBDA (I) (SETQ PPRTSB (CONS (CAR I) PPRTSB))) (CAR I))
             (SETQ I (CDR I))
             (GO LAB)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT CONTSB) (RETURN NIL)))
        (COND ((MEMBER (CAR CONTSB) (CDR CONTSB)) (SETQ CONTSB (CDR CONTSB)))
              (T
               (PROGN
                (SETQ CBASES (CONS (CAR CONTSB) CBASES))
                (SETQ CONTSB (CDR CONTSB)))))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT PPRTSB) (RETURN NIL)))
        (COND ((MEMBER (CAR PPRTSB) (CDR PPRTSB)) (SETQ PPRTSB (CDR PPRTSB)))
              (T
               (PROGN
                (SETQ PBASES (CONS (CAR PPRTSB) PBASES))
                (SETQ PPRTSB (CDR PPRTSB)))))
        (GO WHILELABEL))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN
         (MAPRINT "K := " 0)
         (MAPRINT
          (CONS 'LIST
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F CBASES)
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F)
                                      (PREPF
                                       (OFSF_PEPSUBF F *OFSF_EXPF
                                        (LIST 'EXPT 'E (CAR QEXP)))))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F)
                              (PREPF
                               (OFSF_PEPSUBF F *OFSF_EXPF
                                (LIST 'EXPT 'E (CAR QEXP)))))
                            (CAR F))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
          0)
         (TERPRI* NIL)
         (MAPRINT "Q := " 0)
         (MAPRINT
          (CONS 'LIST
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F PBASES)
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F)
                                      (PREPF
                                       (OFSF_PEPSUBF F *OFSF_EXPF
                                        (LIST 'EXPT 'E (CAR QEXP)))))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F)
                              (PREPF
                               (OFSF_PEPSUBF F *OFSF_EXPF
                                (LIST 'EXPT 'E (CAR QEXP)))))
                            (CAR F))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
          0)
         (TERPRI* NIL))))
      (PROG (C)
        (SETQ C CBASES)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (COND
             ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
              (PROGN
               (TERPRI)
               (MAPRINT "+++ ISOL(" 0)
               (MAPRINT
                (PREPF (OFSF_PEPSUBF C *OFSF_EXPF (LIST 'EXPT 'E (CAR QEXP))))
                0)
               (MAPRINT ")" 0)
               (TERPRI* NIL))))
            (SETQ ILIST (OFSF_PEPISOLATE C (CAR QEXP) *OFSF_EXPF K))
            (SETQ C
                    (AEX_FROMSF
                     (OFSF_PEPSUBF C *OFSF_EXPF (LIST 'EXPT 'E (CAR QEXP)))))
            (SETQ ILIST
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I ILIST)
                     STARTOVER
                      (COND ((NULL I) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (I) (LIST (LIST 'ANU C I))) (CAR I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ I (CDR I))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL I) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (I) (LIST (LIST 'ANU C I))) (CAR I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ I (CDR I))
                      (GO LOOPLABEL)))
            (PROG (I)
              (SETQ I ILIST)
             LAB
              (COND ((NULL I) (RETURN NIL)))
              ((LAMBDA (I) (SETQ ISOL (CONS I ISOL))) (CAR I))
              (SETQ I (CDR I))
              (GO LAB))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (PROG (P)
        (SETQ P PBASES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (COND
             ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
              (PROGN
               (TERPRI)
               (MAPRINT "+++ ISOL(" 0)
               (MAPRINT
                (PREPF (OFSF_PEPSUBF P *OFSF_EXPF (LIST 'EXPT 'E (CAR QEXP))))
                0)
               (MAPRINT ")" 0)
               (TERPRI* NIL))))
            (SETQ ILIST (OFSF_PEPISOLATE P (CAR QEXP) *OFSF_EXPF K))
            (SETQ P
                    (AEX_FROMSF
                     (OFSF_PEPSUBF P *OFSF_EXPF (LIST 'EXPT 'E (CAR QEXP)))))
            (SETQ ILIST
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I ILIST)
                     STARTOVER
                      (COND ((NULL I) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (I) (LIST (LIST 'ANU P I))) (CAR I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ I (CDR I))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL I) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (I) (LIST (LIST 'ANU P I))) (CAR I)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ I (CDR I))
                      (GO LOOPLABEL)))
            (PROG (I)
              (SETQ I ILIST)
             LAB
              (COND ((NULL I) (RETURN NIL)))
              ((LAMBDA (I) (SETQ ISOL (CONS I ISOL))) (CAR I))
              (SETQ I (CDR I))
              (GO LAB))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ ISOL (REVERSE ISOL))
      (COND (ISOL (SETQ ISOL (ANU_SORTLIST ISOL))))
      (COND (ISOL (SETQ HATLIST (ANU_REFINELIST ISOL (CAR QEXP) K))))
      (COND (HATLIST (SETQ SPLIST (OFSF_PEPSPLIST HATLIST)))
            (T
             (SETQ SPLIST
                     (LIST
                      (LIST 'ANU (AEX_FROMSF NIL)
                            (IV_MK (NEGSQ (RAT_1)) (RAT_1)))))))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN
         (TERPRI)
         (IOTO_PRIN2T
          (LIST "+++ Cell Decomposition: " (LENGTH SPLIST) " cells"))
         (PROG (SP)
           (SETQ SP SPLIST)
          LAB
           (COND ((NULL SP) (RETURN NIL)))
           ((LAMBDA (SP) (IV_PEPPRINT (ANU_IV SP))) (CAR SP))
           (SETQ SP (CDR SP))
           (GO LAB))
         (TERPRI* NIL))))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN
         (TERPRI)
         (IOTO_PRIN2T
          (LIST "+++ Sign-Evaluation for " (CL_ATNUM PHIPRIME)
                " polynomials in " (LENGTH SPLIST) " cells")))))
      (SETQ *RLPEPEVAL T)
      (SETQ PHIPRIME
              (CL_APPLY2ATS1 PHIPRIME
                             (FUNCTION
                              (LAMBDA (X QEXP)
                                (LIST (CAR X)
                                      (OFSF_PEPSUBF (CADR X) *OFSF_EXPF
                                       (LIST 'EXPT 'E (CAR QEXP)))
                                      NIL)))
                             (LIST QEXP)))
      (SETQ CELLSTOGO (LENGTH SPLIST))
      (PROG (SP)
        (SETQ SP SPLIST)
       LAB
        (COND ((NULL SP) (RETURN NIL)))
        ((LAMBDA (SP)
           (PROGN
            (COND
             ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
              (PROGN
               (MAPRINT "[" 0)
               (MAPRINT CELLSTOGO 0)
               (MAPRINT "sgn(" 0))))
            (SETQ TV
                    (CONS TV
                          (OFSF_PEPEVALQFF (CL_NNF (CADDR PHIPRIME)) SP
                           (CAR QEXP) K)))
            (SETQ CELLSTOGO (DIFFERENCE CELLSTOGO 1))))
         (CAR SP))
        (SETQ SP (CDR SP))
        (GO LAB))
      (COND
       (TV
        (COND
         ((EQ (CDR QEXP) 'ALL)
          (COND ((SMEMBER 'FALSE TV) (RETURN (CL_SIMPL 'FALSE NIL (MINUS 1))))
                (T (RETURN (CL_SIMPL 'TRUE NIL (MINUS 1))))))
         ((SMEMBER 'TRUE TV) (RETURN (CL_SIMPL 'TRUE NIL (MINUS 1))))
         (T (RETURN (CL_SIMPL 'FALSE NIL (MINUS 1)))))))
      (RETURN (CL_SIMPL PHIPRIME NIL (MINUS 1))))) 
(PUT 'OFSF_PEPEVALQFF 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPEVALQFF 'DEFINED-ON-LINE '322) 
(PUT 'OFSF_PEPEVALQFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALQFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALQFF (F SP ID K)
    (PROG (R)
      (SETQ R
              (CL_SIMPL
               (CL_APPLY2ATS1 F (FUNCTION OFSF_PEPSUBSIGNAT) (LIST SP ID K))
               NIL (MINUS 1)))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (COND ((EQ R 'TRUE) (MAPRINT ") tt]" 0)) (T (MAPRINT ") ff]" 0)))))
      (RETURN R))) 
(PUT 'OFSF_PEPSUBSIGNAT 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPSUBSIGNAT 'DEFINED-ON-LINE '343) 
(PUT 'OFSF_PEPSUBSIGNAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSUBSIGNAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPSUBSIGNAT (AT SP ID K)
    (LIST (CAR AT) (OFSF_PEPEVALSIGNF (CADR AT) SP ID K) NIL)) 
(PUT 'OFSF_PEPEVALSIGNF 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPEVALSIGNF 'DEFINED-ON-LINE '353) 
(PUT 'OFSF_PEPEVALSIGNF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALSIGNF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALSIGNF (F SP ID K)
    (PROG (SGNF SQFDECF DECF PRODF)
      (SETQ SGNF (RAT_1))
      (SETQ PRODF (CAR (SIMP 1)))
      (COND
       ((AND (IV_CONTAINSZERO (ANU_IV SP))
             (NULL
              (CAR
               (SIMP
                (RAT_SGN
                 (OFSF_PEPSUBSQ (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))) ID
                  (RAT_0) K)))))
             (NULL (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F ID (RAT_0) K))))))
        (PROGN
         (COND
          ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP)) (MAPRINT "0 " 0)))
         (RETURN (CAR (SIMP 0))))))
      (COND
       ((NULL (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))))
        (PROGN
         (SETQ SGNF
                 (CAR
                  (SIMP (RAT_SGN (OFSF_PEPSUBSQ F ID (IV_RB (ANU_IV SP)) K)))))
         (COND
          ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
           (PROGN (MAPRINT SGNF 0) (MAPRINT " " 0))))
         (RETURN SGNF))))
      (SETQ SQFDECF (SFTO_SQFDECF F))
      (SETQ DECF
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I SQFDECF)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (I) (CAR I)) (CAR I)) NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (CAR I)) (CAR I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I DECF)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (SETQ PRODF
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF PRODF I))
                         (T (POLY-MULTF PRODF I)))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND
       ((OR (MEMBER (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))) DECF)
            (MEMBER
             ((LAMBDA (G469 G470)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G469 G470))
                      (T (POLY-MULTF G469 G470))))
              (NEGF (CAR (SIMP 1))) (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))))
             DECF))
        (PROGN
         (COND
          ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP)) (MAPRINT "0 " 0)))
         (RETURN (CAR (SIMP 0)))))
       (T
        (PROGN
         (SETQ DECF
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P DECF)
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P) (OFSF_PEPEVALSGNP P SP ID K))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P) (OFSF_PEPEVALSGNP P SP ID K)) (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (I)
           (SETQ I SQFDECF)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ SGNF (MULTSQ SGNF (EXPTSQ (CONS (CAR DECF) 1) (CDR I))))
               (SETQ DECF (CDR DECF))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (COND
          ((MINUSF (QUOTFX F PRODF))
           (PROGN
            (SETQ SGNF (CAR (SIMP (PREPSQ (MULTSQ SGNF (NEGSQ (RAT_1)))))))
            (COND
             ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
              (PROGN (MAPRINT SGNF 0) (MAPRINT " " 0))))
            (RETURN SGNF)))
          (T
           (PROGN
            (SETQ SGNF (CAR (SIMP (PREPSQ SGNF))))
            (COND
             ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
              (PROGN (MAPRINT SGNF 0) (MAPRINT " " 0))))
            (RETURN SGNF))))))))) 
(PUT 'OFSF_PEPEVALSGNP 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPEVALSGNP 'DEFINED-ON-LINE '430) 
(PUT 'OFSF_PEPEVALSGNP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALSGNP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALSGNP (F SP ID K)
    (PROG ()
      (COND
       ((AND (NOT (MEMBER (LIST 'EXPT 'E ID) (KERNELS F)))
             (NOT
              (MEMBER (LIST 'EXPT 'E ID)
                      (KERNELS (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP)))))))))
        (RETURN (OFSF_EVALSIGNF F (LIST SP) (LIST ID)))))
      (COND
       ((MEMBER (LIST 'EXPT 'E ID)
                (KERNELS (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP)))))))
        (RETURN (OFSF_PEPEVALSIGNTRANS F SP ID K)))
       (T (RETURN (OFSF_PEPEVALSIGNALG F SP ID K)))))) 
(PUT 'OFSF_PEPEVALSIGNTRANS 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPEVALSIGNTRANS 'DEFINED-ON-LINE '452) 
(PUT 'OFSF_PEPEVALSIGNTRANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALSIGNTRANS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALSIGNTRANS (F SP ID K)
    (PROG (IV SQFF IVREFINED)
      (SETQ IV (ANU_IV SP))
      (COND ((NULL F) (RETURN (CAR (SIMP 0)))))
      (COND
       ((EQ (CDR (QREMF F (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))))) NIL)
        (RETURN (CAR (SIMP 0)))))
      (SETQ SQFF
              (OFSF_SQFPARTY (OFSF_PEPSUBF F (LIST 'EXPT 'E ID) *OFSF_EXPF)
               *OFSF_EXPF))
      (SETQ SQFF (OFSF_PEPSUBF SQFF *OFSF_EXPF (LIST 'EXPT 'E ID)))
      (SETQ IVREFINED
              (OFSF_PEPREFINE SQFF (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP)))))
               ID IV K))
      (RETURN (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F ID (IV_RB IVREFINED) K))))))) 
(PUT 'OFSF_PEPEVALSIGNALG 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPEVALSIGNALG 'DEFINED-ON-LINE '473) 
(PUT 'OFSF_PEPEVALSIGNALG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALSIGNALG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALSIGNALG (F SP ID K)
    (PROG (IV SQFF IVREFINED EF)
      (SETQ IV (ANU_IV SP))
      (COND ((NULL F) (RETURN (CAR (SIMP 0)))))
      (SETQ EF (OFSF_PEPSUBF F (LIST 'EXPT 'E ID) *OFSF_EXPF))
      (SETKORDER (LIST *OFSF_EXPF ID))
      (SETQ EF (REORDER EF))
      (COND ((OFSF_PEPEVALSIGN0ALG EF SP ID) (RETURN (CAR (SIMP 0)))))
      (SETQ SQFF
              (OFSF_SQFPARTY (OFSF_PEPSUBF F (LIST 'EXPT 'E ID) *OFSF_EXPF)
               *OFSF_EXPF))
      (SETQ SQFF (OFSF_PEPSUBF SQFF *OFSF_EXPF (LIST 'EXPT 'E ID)))
      (SETQ IVREFINED
              (OFSF_PEPREFINE SQFF (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP)))))
               ID IV K))
      (RETURN (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F ID (IV_RB IVREFINED) K))))))) 
(PUT 'OFSF_PEPEVALSIGN0ALG 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PEPEVALSIGN0ALG 'DEFINED-ON-LINE '498) 
(PUT 'OFSF_PEPEVALSIGN0ALG 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEVALSIGN0ALG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEVALSIGN0ALG (F SP ID)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) ID))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (NULL F))
            (T
             (NULL
              (CDR (QREMF F (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP)))))))))))
     (T
      (AND
       (NULL (CDR (QREMF (CDAR F) (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP SP))))))))
       (OFSF_PEPSGN0RAT (CDR F) SP ID))))) 
(PUT 'OFSF_PEPSPLIST 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PEPSPLIST 'DEFINED-ON-LINE '515) 
(PUT 'OFSF_PEPSPLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSPLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_PEPSPLIST (ANUL)
    (PROG (SPL)
      (SETQ SPL
              (LIST
               (LIST 'ANU (AEX_FROMSF NIL)
                     (IV_MK (ADDSQ (IV_LB (ANU_IV (CAR ANUL))) (NEGSQ (RAT_1)))
                      (IV_LB (ANU_IV (CAR ANUL)))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT ANUL) (RETURN NIL)))
        (PROGN
         (SETQ SPL (CONS (CAR ANUL) SPL))
         (COND
          ((CDR ANUL)
           (SETQ SPL
                   (CONS
                    (LIST 'ANU (AEX_FROMSF NIL)
                          (IV_MK (IV_RB (ANU_IV (CAR ANUL)))
                           (IV_LB (ANU_IV (CADR ANUL)))))
                    SPL)))
          (T
           (SETQ SPL
                   (CONS
                    (LIST 'ANU (AEX_FROMSF NIL)
                          (IV_MK (IV_RB (ANU_IV (CAR ANUL)))
                           (ADDSQ (IV_RB (ANU_IV (CAR ANUL))) (RAT_1))))
                    SPL))))
         (SETQ ANUL (CDR ANUL)))
        (GO WHILELABEL))
      (RETURN (REVERSE SPL)))) 
(PUT 'OFSF_PEPSUBSQ 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPSUBSQ 'DEFINED-ON-LINE '543) 
(PUT 'OFSF_PEPSUBSQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSUBSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPSUBSQ (F ID R K)
    (PROG (UB LB EF UBEXPF LBEXPF)
      (SETQ EF (OFSF_PEPSUBF F (LIST 'EXPT 'E ID) *OFSF_EXPF))
      (SETKORDER (LIST *OFSF_EXPF ID))
      (SETQ EF (REORDER EF))
      (COND ((OFSF_PEPSGN0RAT EF ID R) (RETURN (RAT_0)))
            (T
             (PROGN
              (SETQ UBEXPF (OFSF_PEPUBOUNDEXPF R K))
              (SETQ LBEXPF (OFSF_PEPLBOUNDEXPF R K))
              (SETQ UB (OFSF_PEPUBOUNDEPOLY EF ID R UBEXPF LBEXPF))
              (SETQ LB (OFSF_PEPLBOUNDEPOLY EF ID R UBEXPF LBEXPF))
              (COND
               ((NEQ (RAT_SGN UB) (RAT_SGN LB))
                (PROGN
                 (REDERR
                  (LIST
                   "Approximation error too high. Accuracy has to be higher than"
                   K)))))
              (RETURN UB)))))) 
(PUT 'OFSF_PEPSGN0RAT 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PEPSGN0RAT 'DEFINED-ON-LINE '571) 
(PUT 'OFSF_PEPSGN0RAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSGN0RAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPSGN0RAT (F X V)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) X))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (NULL F))
            (T (NULL (CAR (OFSF_SUBF F X V))))))
     (T
      (AND (NULL (CAR (OFSF_SUBF (CDAR F) X V)))
           (OFSF_PEPSGN0RAT (CDR F) X V))))) 
(PUT 'OFSF_PEPUBOUNDEPOLY 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PEPUBOUNDEPOLY 'DEFINED-ON-LINE '588) 
(PUT 'OFSF_PEPUBOUNDEPOLY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPUBOUNDEPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPUBOUNDEPOLY (F ID R LBEXPF UBEXPF)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) ID))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (CONS F 1)) (T (OFSF_SUBF F ID R))))
     (T
      (PROG (TMP)
        (SETQ TMP (OFSF_SUBF (CDAR F) ID R))
        (COND
         ((MINUSF (CAR (SIMP (RAT_SGN TMP))))
          (RETURN
           (ADDSQ (MULTSQ TMP (EXPTSQ LBEXPF (CDAAR F)))
                  (OFSF_PEPUBOUNDEPOLY (CDR F) ID R LBEXPF UBEXPF))))
         (T
          (RETURN
           (ADDSQ (MULTSQ TMP (EXPTSQ UBEXPF (CDAAR F)))
                  (OFSF_PEPUBOUNDEPOLY (CDR F) ID R LBEXPF UBEXPF))))))))) 
(PUT 'OFSF_PEPLBOUNDEPOLY 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PEPLBOUNDEPOLY 'DEFINED-ON-LINE '614) 
(PUT 'OFSF_PEPLBOUNDEPOLY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPLBOUNDEPOLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPLBOUNDEPOLY (F ID R LBEXPF UBEXPF)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) ID))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (CONS F 1)) (T (OFSF_SUBF F ID R))))
     (T
      (PROG (TMP)
        (SETQ TMP (OFSF_SUBF (CDAR F) ID R))
        (COND
         ((MINUSF (CAR (SIMP (RAT_SGN TMP))))
          (RETURN
           (ADDSQ (MULTSQ TMP (EXPTSQ UBEXPF (CDAAR F)))
                  (OFSF_PEPUBOUNDEPOLY (CDR F) ID R LBEXPF UBEXPF))))
         (T
          (RETURN
           (ADDSQ (MULTSQ TMP (EXPTSQ LBEXPF (CDAAR F)))
                  (OFSF_PEPUBOUNDEPOLY (CDR F) ID R LBEXPF UBEXPF))))))))) 
(PUT 'OFSF_PEPSUBF 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PEPSUBF 'DEFINED-ON-LINE '641) 
(PUT 'OFSF_PEPSUBF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSUBF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPSUBF (F ID R) (CAR (SIMP (PREPSQ (SUBF F (LIST (CONS ID R))))))) 
(PUT 'OFSF_PEPISOLATE 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPISOLATE 'DEFINED-ON-LINE '648) 
(PUT 'OFSF_PEPISOLATE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPISOLATE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPISOLATE (F X Y K)
    (PROG (PSDEG PREDIFF S LPRIME POSB NEGB F0 F1 ISOL LPREFINED EF ES)
      (SETQ PSDEG (OFSF_PSDEGREE F X Y))
      (COND
       ((AND (EQ (CAR PSDEG) 0) (EQ (CDR PSDEG) 0))
        (PROGN
         (COND
          ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
           (PROGN
            (MAPRINT "L(" 0)
            (MAPRINT (PREPF (OFSF_PEPSUBF F *OFSF_EXPF (LIST 'EXPT 'E X))) 0)
            (MAPRINT ") := " 0)
            (MAPRINT "{}" 0)
            (TERPRI* NIL))))
         (RETURN NIL))))
      (SETQ PREDIFF (OFSF_DIFF F X Y))
      (COND ((GREATERP (CDR PSDEG) 0) (SETQ S (OFSF_SQFPARTY PREDIFF Y)))
            (T
             (SETQ S
                     (OFSF_SQFPARTY
                      ((LAMBDA (*EXP) (QUOTF1 PREDIFF (CAR (SIMP Y)))) T) Y))))
      (SETQ LPRIME (OFSF_PEPISOLATE S X Y K))
      (COND
       ((EQ (CAAAR F) Y)
        (PROGN
         (SETQ POSB (CONS (OFSF_PEPPOSITIVEBOUND F K) 1))
         (SETQ NEGB (CONS (OFSF_PEPNEGATIVEBOUND F K) 1))))
       ((OR (ATOM F) (ATOM (CAR F))) (SETQ POSB (SETQ NEGB 0)))
       (T
        (PROGN
         (SETQ POSB
                 (ADDSQ (AEX_CAUCHYBOUND (AEX_FROMSF F) (CAAAR F)) (SIMP 1)))
         (SETQ NEGB (NEGSQ POSB)))))
      (COND
       ((OFSF_DPEPIVERBOSEP)
        (PROGN
         (MAPRINT "++ ISOL(" 0)
         (MAPRINT (PREPF (OFSF_PEPSUBF F *OFSF_EXPF (LIST 'EXPT 'E X))) 0)
         (MAPRINT ")" 0)
         (TERPRI* NIL)
         (MAPRINT "+ Real root bounds: " 0)
         (MAPRINT (CAR (SIMP (PREPSQ NEGB))) 0)
         (MAPRINT ", " 0)
         (MAPRINT (CAR (SIMP (PREPSQ POSB))) 0)
         (TERPRI* NIL))))
      (SETQ F1 (OFSF_PEPSUBF F *OFSF_EXPF 1))
      (COND ((OR (ATOM F1) (ATOM (CAR F1))) (SETQ F0 F1))
            (T (SETQ F0 (OFSF_PEPSUBF F1 (CAAAR F1) 0))))
      (SETQ EF (OFSF_PEPSUBF F *OFSF_EXPF (LIST 'EXPT 'E X)))
      (SETQ ES (OFSF_PEPSUBF S *OFSF_EXPF (LIST 'EXPT 'E X)))
      (SETQ LPREFINED
              (PROG (IV FORALL-RESULT FORALL-ENDPTR)
                (SETQ IV LPRIME)
               STARTOVER
                (COND ((NULL IV) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (IV)
                           (COND
                            ((AND (IV_CONTAINSZERO IV) (NULL F0))
                             (LIST
                              (IV_MK (CONS (MINUS 1) 1024) (CONS 1 1024))))
                            (T (LIST (OFSF_PEPREFINE EF ES X IV K)))))
                         (CAR IV)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ IV (CDR IV))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL IV) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (IV)
                           (COND
                            ((AND (IV_CONTAINSZERO IV) (NULL F0))
                             (LIST
                              (IV_MK (CONS (MINUS 1) 1024) (CONS 1 1024))))
                            (T (LIST (OFSF_PEPREFINE EF ES X IV K)))))
                         (CAR IV)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ IV (CDR IV))
                (GO LOOPLABEL)))
      (SETQ ISOL (REVERSE (OFSF_PEPCOMPLETION EF X LPREFINED NEGB POSB K)))
      (COND
       ((OR (OFSF_DPEPVERBOSEP) (OFSF_DPEPIVERBOSEP))
        (PROGN
         (MAPRINT "L(" 0)
         (MAPRINT (PREPF (OFSF_PEPSUBF F *OFSF_EXPF (LIST 'EXPT 'E X))) 0)
         (MAPRINT ") := " 0)
         (COND ((NULL ISOL) (PROGN (MAPRINT "{}" 0) (TERPRI* NIL)))
               (T
                (PROG (I)
                  (SETQ I ISOL)
                 LAB
                  (COND ((NULL I) (RETURN NIL)))
                  ((LAMBDA (I) (IV_PEPPRINT I)) (CAR I))
                  (SETQ I (CDR I))
                  (GO LAB))))
         (TERPRI* NIL))))
      (RETURN ISOL))) 
(PUT 'IV_PEPPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'IV_PEPPRINT 'DEFINED-ON-LINE '747) 
(PUT 'IV_PEPPRINT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'IV_PEPPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IV_PEPPRINT (IV)
    (PROGN
     (MAPRINT "]" 0)
     (RAT_PEPPRINT (IV_LB IV))
     (MAPRINT "," 0)
     (RAT_PEPPRINT (IV_RB IV))
     (MAPRINT "[" 0))) 
(PUT 'RAT_PEPPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'RAT_PEPPRINT 'DEFINED-ON-LINE '755) 
(PUT 'RAT_PEPPRINT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'RAT_PEPPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAT_PEPPRINT (R)
    (COND
     ((CAR R) (PROGN (MAPRINT (CAR R) 0) (MAPRINT "/" 0) (MAPRINT (CDR R) 0)))
     (T (MAPRINT "0" 0)))) 
(PUT 'OFSF_PEPCOMPLETION 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_PEPCOMPLETION 'DEFINED-ON-LINE '764) 
(PUT 'OFSF_PEPCOMPLETION 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPCOMPLETION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPCOMPLETION (F X IVL B A K)
    (PROG (F0 IVLIST R)
      (SETQ F0 (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F X (RAT_0) K)))))
      (COND
       ((EQUAL IVL NIL)
        (PROGN
         (COND
          ((EQ (OFSF_PEPSGNCH F X B A K) 'TRUE)
           (SETQ IVLIST (CONS (IV_MK B A) IVLIST))))
         (RETURN IVLIST))))
      (COND
       ((EQUAL (CDR IVL) NIL)
        (PROGN
         (COND
          ((EQ (OFSF_PEPSGNCH F X B (IV_LB (CAR IVL)) K) 'TRUE)
           (SETQ IVLIST (CONS (IV_MK B (IV_LB (CAR IVL))) IVLIST))))
         (COND
          ((AND (IV_CONTAINSZERO (CAR IVL)) (NULL F0))
           (SETQ IVLIST
                   (CONS (IV_MK (IV_LB (CAR IVL)) (IV_RB (CAR IVL))) IVLIST))))
         (COND
          ((EQ (OFSF_PEPSGNCH F X (IV_RB (CAR IVL)) A K) 'TRUE)
           (SETQ IVLIST (CONS (IV_MK (IV_RB (CAR IVL)) A) IVLIST))))
         (RETURN IVLIST))))
      (COND
       ((EQ (OFSF_PEPSGNCH F X B (IV_LB (CAR IVL)) K) 'TRUE)
        (SETQ IVLIST (CONS (IV_MK B (IV_LB (CAR IVL))) IVLIST))))
      (COND
       ((AND (IV_CONTAINSZERO (CAR IVL)) (NULL F0))
        (SETQ IVLIST
                (CONS (IV_MK (IV_LB (CAR IVL)) (IV_RB (CAR IVL))) IVLIST))))
      (SETQ R
              (CONS (OFSF_PEPCOMPLETION F X (CDR IVL) (IV_RB (CAR IVL)) A K)
                    IVLIST))
      (SETQ R
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I R)
               STARTOVER
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (I)
                           (COND
                            ((LISTP I)
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J I)
                              STARTOVER
                               (COND ((NULL J) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (J) (LIST J)) (CAR J)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ J (CDR J))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL J) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (J) (LIST J)) (CAR J)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ J (CDR J))
                               (GO LOOPLABEL)))
                            (T (LIST I))))
                         (CAR I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (CDR I))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (I)
                           (COND
                            ((LISTP I)
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J I)
                              STARTOVER
                               (COND ((NULL J) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (J) (LIST J)) (CAR J)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ J (CDR J))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL J) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (J) (LIST J)) (CAR J)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ J (CDR J))
                               (GO LOOPLABEL)))
                            (T (LIST I))))
                         (CAR I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (CDR I))
                (GO LOOPLABEL)))
      (RETURN R))) 
(PUT 'OFSF_PEPSGNCH 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PEPSGNCH 'DEFINED-ON-LINE '811) 
(PUT 'OFSF_PEPSGNCH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPSGNCH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPSGNCH (F X V1 V2 K)
    (COND
     ((MINUSF
       ((LAMBDA (G471 G472)
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF G471 G472))
                (T (POLY-MULTF G471 G472))))
        (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F X V1 K))))
        (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ F X V2 K))))))
      'TRUE)
     (T 'FALSE))) 
(PUT 'OFSF_PEPREFINE 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PEPREFINE 'DEFINED-ON-LINE '825) 
(PUT 'OFSF_PEPREFINE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPREFINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPREFINE (F S X IV K)
    (PROG (D M IVL IVR FIVL FIVR EPSILON DIFF DELTA SGNSM SGNSIVL SGNSS
           MAXCOEFFL)
      (COND
       ((AND (OFSF_DPEPIVERBOSEP) (NULL *RLPEPEVAL))
        (PROGN
         (MAPRINT "+ Interval refinement of " 0)
         (IV_PEPPRINT IV)
         (TERPRI* NIL))))
      (SETQ D (CAR (SIMP (PREPSQ (DIFFSQ (CONS F 1) X)))))
      (SETQ D (OFSF_PEPSUBF D (LIST 'EXPT 'E X) *OFSF_EXPF))
      (SETKORDER (LIST *OFSF_EXPF X))
      (SETQ D (REORDER D))
      (SETQ MAXCOEFFL (OFSF_MAXCOEFFLIST D X))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND ((AND (OFSF_DPEPIVERBOSEP) (NULL *RLPEPEVAL)) (IV_PEPPRINT IV)))
         (SETQ M (IV_MED IV))
         (SETQ SGNSM (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ S X M K)))))
         (SETQ SGNSIVL (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ S X (IV_LB IV) K)))))
         (SETQ SGNSS
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF SGNSIVL SGNSM))
                       (T (POLY-MULTF SGNSIVL SGNSM))))
         (COND
          ((NULL SGNSM)
           (PROGN
            (SETQ IVL
                    (ADDSQ (IV_LB IV)
                           (MULTSQ (ADDSQ (IV_RB IV) (NEGSQ (IV_LB IV)))
                                   (INVSQ (SIMP 4)))))
            (SETQ IVR
                    (ADDSQ (IV_LB IV)
                           (MULTSQ
                            (MULTSQ (SIMP 3)
                                    (ADDSQ (IV_RB IV) (NEGSQ (IV_LB IV))))
                            (INVSQ (SIMP 4)))))
            (SETQ IV (IV_MK IVL IVR))))
          ((MINUSF SGNSS) (SETQ IV (IV_MK (IV_LB IV) M)))
          (T (SETQ IV (IV_MK M (IV_RB IV)))))
         (SETQ FIVL (RAT_ABS (OFSF_PEPSUBSQ F X (IV_LB IV) K)))
         (SETQ FIVR (RAT_ABS (OFSF_PEPSUBSQ F X (IV_RB IV) K)))
         (SETQ EPSILON (MULTSQ (RAT_MIN FIVL FIVR) (INVSQ (SIMP 2))))
         (SETQ DIFF (ADDSQ (IV_RB IV) (NEGSQ (IV_LB IV))))
         (SETQ DELTA (MULTSQ EPSILON (OFSF_PEPMOC D X IV MAXCOEFFL K))))
        (COND
         ((NOT (AND (RAT_LEQ DIFF DELTA) (RAT_GREATER EPSILON (RAT_0))))
          (GO REPEATLABEL))))
      (COND
       ((AND (OFSF_DPEPIVERBOSEP) (NULL *RLPEPEVAL))
        (PROGN
         (TERPRI* NIL)
         (MAPRINT "+ Refined interval: " 0)
         (IV_PEPPRINT IV)
         (TERPRI* NIL))))
      (RETURN (IV_MK (IV_LB IV) (IV_RB IV))))) 
(PUT 'OFSF_PEPMOC 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PEPMOC 'DEFINED-ON-LINE '900) 
(PUT 'OFSF_PEPMOC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPMOC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPMOC (F X IV CL K)
    (PROG (M MBOUND BOUND)
      (SETQ MBOUND (OFSF_PEPMOCBOUND F X IV CL))
      (COND
       ((MEMBER *OFSF_EXPF (KERNELS F))
        (PROGN
         (SETQ M (CONS (CDAAR F) 1))
         (SETQ BOUND
                 (MULTSQ (ADDSQ M (RAT_1))
                         (OFSF_PEPUBOUNDEXPF (MULTSQ M (IV_RB IV)) K)))
         (SETQ BOUND (MULTSQ BOUND MBOUND))))
       (T (SETQ BOUND MBOUND)))
      (RETURN (MULTSQ (RAT_1) (INVSQ BOUND))))) 
(PUT 'OFSF_PEPMOCBOUND 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PEPMOCBOUND 'DEFINED-ON-LINE '922) 
(PUT 'OFSF_PEPMOCBOUND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPMOCBOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPMOCBOUND (F X IV CL)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) X))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RAT_ABS (CONS F 1)))
            (T
             (PROG (A C SUM)
               (SETQ A (CONS (CAR CL) 1))
               (SETQ C (RAT_MAX (RAT_ABS (IV_LB IV)) (RAT_ABS (IV_RB IV))))
               (SETQ SUM (RAT_1))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE (CDAAR F) I)) (RETURN NIL)))
                 (SETQ SUM (ADDSQ SUM (EXPTSQ C I)))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (RETURN (MULTSQ A SUM))))))
     (T
      (PROG (A C DEGREE SUM)
        (COND ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F)))) (SETQ DEGREE 0))
              (T (SETQ DEGREE (CDAAR (CDAR F)))))
        (SETQ A (CONS (CAR CL) 1))
        (SETQ CL (CDR CL))
        (SETQ C (RAT_MAX (RAT_ABS (IV_LB IV)) (RAT_ABS (IV_RB IV))))
        (SETQ SUM (RAT_1))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE DEGREE I)) (RETURN NIL)))
          (SETQ SUM (ADDSQ SUM (EXPTSQ C I)))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (RETURN (RAT_MAX (MULTSQ A SUM) (OFSF_PEPMOCBOUND (CDR F) X IV CL))))))) 
(PUT 'OFSF_MAXCOEFFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_MAXCOEFFLIST 'DEFINED-ON-LINE '960) 
(PUT 'OFSF_MAXCOEFFLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_MAXCOEFFLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_MAXCOEFFLIST (F X)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) X))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (LIST (ABSF F)))
            (T (LIST (OFSF_MAXCOEFF F)))))
     (T
      (APPEND (LIST (OFSF_MAXCOEFF (CDAR F))) (OFSF_MAXCOEFFLIST (CDR F) X))))) 
(PUT 'OFSF_MAXCOEFF 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_MAXCOEFF 'DEFINED-ON-LINE '976) 
(PUT 'OFSF_MAXCOEFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_MAXCOEFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_MAXCOEFF (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) (COND ((EQ F NIL) 0) (T (ABSF F))))
          (T (MAX (ABSF (CDAR F)) (ABSF (OFSF_MAXCOEFF (CDR F))))))) 
(PUT 'OFSF_PEPPOSITIVEBOUND 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPPOSITIVEBOUND 'DEFINED-ON-LINE '991) 
(PUT 'OFSF_PEPPOSITIVEBOUND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPPOSITIVEBOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPPOSITIVEBOUND (F K)
    (PROG (C1 AC2 A C2 C3)
      (SETQ C1 (OFSF_PEPBOUND1 (CDAR F)))
      (SETQ AC2 (OFSF_PEPBOUND2 (CDR F) *OFSF_EXPF (CDAAR F)))
      (SETQ A (ADDF 1 (CAR AC2)))
      (SETQ C2 (CDR AC2))
      (SETQ C3 (OFSF_PEPBOUND3 A K))
      (RETURN (MAX C1 (MAX C2 C3))))) 
(PUT 'OFSF_PEPNEGATIVEBOUND 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPNEGATIVEBOUND 'DEFINED-ON-LINE '1015) 
(PUT 'OFSF_PEPNEGATIVEBOUND 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPNEGATIVEBOUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPNEGATIVEBOUND (F K)
    (PROG (F0 C1 AC2 A C2 C3)
      (SETQ F0 (OFSF_PEPLOWESTCOEFF F *OFSF_EXPF))
      (SETQ C1 (OFSF_PEPBOUND1 F0))
      (SETQ AC2
              (OFSF_PEPBOUND2 (ADDF (ADDF F (NEGF F0)) 1) *OFSF_EXPF
               (CDAAR F)))
      (SETQ A (ADDF 1 (CAR AC2)))
      (SETQ C2 (CDR AC2))
      (SETQ C3 (OFSF_PEPBOUND3 A K))
      (RETURN (NEGF (MAX C1 (MAX C2 C3)))))) 
(PUT 'OFSF_PEPLOWESTCOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPLOWESTCOEFF 'DEFINED-ON-LINE '1034) 
(PUT 'OFSF_PEPLOWESTCOEFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPLOWESTCOEFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPLOWESTCOEFF (F Y)
    (COND ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) Y)) F)
          (T (OFSF_PEPLOWESTCOEFF (CDR F) Y)))) 
(PUT 'OFSF_PEPBOUND1 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PEPBOUND1 'DEFINED-ON-LINE '1045) 
(PUT 'OFSF_PEPBOUND1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPBOUND1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_PEPBOUND1 (F)
    (PROG (FP1 AEFP1 CBP1 FM1 AEFM1 CBM1)
      (SETQ FP1 (ADDF F 1))
      (SETQ AEFP1 (AEX_FROMSF FP1))
      (COND ((OR (ATOM FP1) (ATOM (CAR FP1))) (SETQ CBP1 0))
            (T
             (SETQ CBP1
                     (CAR
                      (SIMP (PREPSQ (AEX_CAUCHYBOUND AEFP1 (CAAAR FP1))))))))
      (SETQ FM1 (ADDF F (NEGF 1)))
      (SETQ AEFM1 (AEX_FROMSF FM1))
      (COND ((OR (ATOM FM1) (ATOM (CAR FM1))) (SETQ CBM1 0))
            (T
             (SETQ CBM1
                     (CAR
                      (SIMP (PREPSQ (AEX_CAUCHYBOUND AEFM1 (CAAAR FM1))))))))
      (RETURN (MIN CBP1 CBM1)))) 
(PUT 'OFSF_PEPBOUND2 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PEPBOUND2 'DEFINED-ON-LINE '1068) 
(PUT 'OFSF_PEPBOUND2 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPBOUND2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPBOUND2 (F Y MDEG)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) Y))
      (COND
       ((OR (ATOM F) (ATOM (CAR F)))
        (PROGN
         (COND ((EQ F NIL) (SETQ F 0)))
         (CONS 0
               ((LAMBDA (G473 G474)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G473 G474))
                        (T (POLY-MULTF G473 G474))))
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 MDEG))
                      (T (POLY-MULTF 2 MDEG)))
                (ABSF* F)))))
       (T
        (CONS (CDAAR F)
              ((LAMBDA (G475 G476)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G475 G476))
                       (T (POLY-MULTF G475 G476))))
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 MDEG))
                     (T (POLY-MULTF 2 MDEG)))
               (ABSF* (CDAR F)))))))
     (T
      (PROG (AEPM CB B M PMDEG)
        (SETQ AEPM (AEX_FROMSF (CDAR F)))
        (COND
         ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F))))
          (PROGN
           (SETQ CB 0)
           (SETQ B
                   ((LAMBDA (G477 G478)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G477 G478))
                            (T (POLY-MULTF G477 G478))))
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 MDEG))
                          (T (POLY-MULTF 2 MDEG)))
                    (ABSF* (CDAR F))))
           (SETQ PMDEG 0)))
         (T
          (PROGN
           (SETQ CB
                   (CAR
                    (SIMP (PREPSQ (AEX_CAUCHYBOUND AEPM (CAAAR (CDAR F)))))))
           (SETQ B
                   ((LAMBDA (G479 G480)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G479 G480))
                            (T (POLY-MULTF G479 G480))))
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 MDEG))
                          (T (POLY-MULTF 2 MDEG)))
                    (ABSF* (CDAR (CDAR F)))))
           (SETQ PMDEG (CDAAR (CDAR F))))))
        (COND ((LESSP CB B) (SETQ M B)) (T (SETQ M CB)))
        (RETURN
         (CONS (MAX PMDEG (CAR (OFSF_PEPBOUND2 (CDR F) Y MDEG)))
               (MAX M (CDR (OFSF_PEPBOUND2 (CDR F) Y MDEG))))))))) 
(PUT 'OFSF_PEPBOUND3 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPBOUND3 'DEFINED-ON-LINE '1104) 
(PUT 'OFSF_PEPBOUND3 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPBOUND3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPBOUND3 (A K)
    (PROG (B BA BEXP)
      (SETQ B 0)
      (SETQ BA 0)
      (SETQ BEXP 0)
      (SETQ B (SIMP 2))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ B (ADDSQ B (SIMP 1)))
         (SETQ BA (EXPTSQ B A))
         (SETQ BEXP (OFSF_PEPUBOUNDEXPF (MULTSQ B (INVSQ (SIMP 2))) K)))
        (COND ((NOT (EQ (RAT_MAX BA BEXP) BEXP)) (GO REPEATLABEL))))
      (RETURN (CAR (SIMP (PREPSQ B)))))) 
(PUT 'OFSF_DIFF 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_DIFF 'DEFINED-ON-LINE '1120) 
(PUT 'OFSF_DIFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DIFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_DIFF (F X Y)
    (COND
     ((OR (OR (ATOM F) (ATOM (CAR F))) (EQ (CAAAR F) X))
      (COND ((OR (ATOM F) (ATOM (CAR F))) NIL) (T (DIFF F (CAAAR F)))))
     (T
      (PROG (D L K M)
        (COND ((OR (ATOM (CDAR F)) (ATOM (CAR (CDAR F)))) (SETQ D 0))
              (T
               (SETQ D
                       (CAR
                        (SIMP
                         (PREPSQ
                          (DIFFSQ (CONS (CDAR F) 1) (CAAAR (CDAR F)))))))))
        (SETQ L
                (ADDF D
                      ((LAMBDA (G481)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G481 (CDAR F)))
                               (T (POLY-MULTF G481 (CDAR F)))))
                       (CDAAR F))))
        (COND
         ((EQ (CAAAR F) Y)
          (PROGN
           (SETQ K (LIST (CONS (CONS (CAAAR F) 1) 1)))
           (SETQ M
                   ((LAMBDA (G483)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G483 L))
                            (T (POLY-MULTF G483 L))))
                    (EXPTF K (CDAAR F))))))
         (T (SETQ M L)))
        (RETURN (ADDF M (OFSF_DIFF (CDR F) X Y))))))) 
(PUT 'OFSF_PSDEGREE 'NUMBER-OF-ARGS 3) 
(PUT 'OFSF_PSDEGREE 'DEFINED-ON-LINE '1151) 
(PUT 'OFSF_PSDEGREE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PSDEGREE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PSDEGREE (F X Y)
    (COND ((OR (ATOM F) (ATOM (CAR F))) (CONS 0 0))
          (T
           (PROG (M N)
             (COND ((EQ (CAAAR F) Y) (SETQ M (CDAAR F))) (T (SETQ M 0)))
             (SETQ N (OFSF_DEGX F X))
             (RETURN (CONS M N)))))) 
(PUT 'OFSF_DEGX 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_DEGX 'DEFINED-ON-LINE '1169) 
(PUT 'OFSF_DEGX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_DEGX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_DEGX (F X)
    (COND ((OR (ATOM F) (ATOM (CAR F))) 0) ((EQ (CAAAR F) X) (CDAAR F))
          (T (OFSF_DEGX (CDR F) X)))) 
(PUT 'OFSF_CONTENTY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_CONTENTY 'DEFINED-ON-LINE '1182) 
(PUT 'OFSF_CONTENTY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_CONTENTY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_CONTENTY (F Y)
    (COND ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) Y)) F)
          (T (SFTO_GCDF* (CDAR F) (OFSF_CONTENTY (CDR F) Y))))) 
(PUT 'OFSF_PRPARTY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PRPARTY 'DEFINED-ON-LINE '1192) 
(PUT 'OFSF_PRPARTY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PRPARTY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PRPARTY (F Y) ((LAMBDA (*EXP) (QUOTF1 F (OFSF_CONTENTY F Y))) T)) 
(PUT 'OFSF_SQFPARTY 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SQFPARTY 'DEFINED-ON-LINE '1198) 
(PUT 'OFSF_SQFPARTY 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_SQFPARTY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SQFPARTY (F Y)
    (PROG (C PP CDEC1 CDEC2 PPDEC1 PPDEC2)
      (SETQ CDEC2 (SETQ PPDEC2 1))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RETURN 1)))
      (SETQ C (OFSF_CONTENTY F Y))
      (SETQ CDEC1 (SFTO_SQFDECF C))
      (SETQ CDEC1
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C CDEC1)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (C) (CAR C)) (CAR C)) NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (C) (CAR C)) (CAR C)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT CDEC1) (RETURN NIL)))
        (PROGN
         (SETQ CDEC2
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF CDEC2 (CAR CDEC1)))
                       (T (POLY-MULTF CDEC2 (CAR CDEC1)))))
         (SETQ CDEC1 (CDR CDEC1)))
        (GO WHILELABEL))
      (SETQ PP (OFSF_PRPARTY F Y))
      (SETQ PPDEC1 (SFTO_SQFDECF PP))
      (SETQ PPDEC1
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PPDEC1)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT PPDEC1) (RETURN NIL)))
        (PROGN
         (SETQ PPDEC2
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF PPDEC2 (CAR PPDEC1)))
                       (T (POLY-MULTF PPDEC2 (CAR PPDEC1)))))
         (SETQ PPDEC1 (CDR PPDEC1)))
        (GO WHILELABEL))
      (RETURN
       (COND (*PHYSOP-LOADED (PHYSOP-MULTF CDEC2 PPDEC2))
             (T (POLY-MULTF CDEC2 PPDEC2)))))) 
(PUT 'OFSF_PEPCHECK 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PEPCHECK 'DEFINED-ON-LINE '1226) 
(PUT 'OFSF_PEPCHECK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPCHECK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_PEPCHECK (OPHI)
    (PROG (IDEXP PSI KERNS)
      (COND
       ((NULL (CAR (CL_SPLT OPHI))) (REDERR "Input is not a prenex sentence")))
      (SETQ IDEXP (CADR OPHI))
      (SETQ PSI (CADDR OPHI))
      (PROG (TERM)
        (SETQ TERM (CL_TERML PSI))
       LAB
        (COND ((NULL TERM) (RETURN NIL)))
        ((LAMBDA (TERM)
           (PROGN
            (SETQ KERNS (KERNELS TERM))
            (PROG (K)
              (SETQ K KERNS)
             LAB
              (COND ((NULL K) (RETURN NIL)))
              ((LAMBDA (K)
                 (COND
                  ((AND (MEMBER 'EXPT K) (MEMBER 'E K))
                   (COND
                    ((NEQ IDEXP (CADDR K))
                     (REDERR
                      (LIST "Exponential function has to occur in" IDEXP)))))))
               (CAR K))
              (SETQ K (CDR K))
              (GO LAB))))
         (CAR TERM))
        (SETQ TERM (CDR TERM))
        (GO LAB))
      (COND
       ((NULL
         (AND (EQ (LENGTH (CL_FVARL1 OPHI)) 1)
              (SMEMBER (LIST 'EXPT 'E IDEXP) (CL_FVARL1 OPHI))))
        (COND
         ((NULL (OFSF_EXPFREE OPHI))
          (REDERR "Input is not a prenex sentence")))))
      (RETURN NIL))) 
(PUT 'OFSF_EXPFREE 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_EXPFREE 'DEFINED-ON-LINE '1257) 
(PUT 'OFSF_EXPFREE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_EXPFREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_EXPFREE (F)
    (PROG (ESF EXPFREE)
      (SETQ EXPFREE 'TRUE)
      (SETQ ESF (LIST 'EXPT 'E (CADR F)))
      (PROG (TERM)
        (SETQ TERM (CL_TERML F))
       LAB
        (COND ((NULL TERM) (RETURN NIL)))
        ((LAMBDA (TERM)
           (COND ((MEMBER ESF (KERNELS TERM)) (SETQ EXPFREE NIL))))
         (CAR TERM))
        (SETQ TERM (CDR TERM))
        (GO LAB))
      (RETURN EXPFREE))) 
(PUT 'OFSF_PEPUBOUNDEXPF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPUBOUNDEXPF 'DEFINED-ON-LINE '1273) 
(PUT 'OFSF_PEPUBOUNDEXPF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPUBOUNDEXPF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPUBOUNDEXPF (X K)
    (COND
     ((RAT_LESS X (RAT_0))
      (MULTSQ (RAT_1) (INVSQ (OFSF_PEPEXPF (RAT_ABS X) K))))
     (T
      (ADDSQ (OFSF_PEPEXPF X K)
             (MULTSQ
              (MULTSQ (EXPTSQ (SIMP 3) (OFSF_PEPCEILING (RAT_ABS X)))
                      (INVSQ (SIMP (OFSF_FACTSF (ADDF K 1)))))
              (EXPTSQ X (ADDF K 1))))))) 
(PUT 'OFSF_PEPLBOUNDEXPF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPLBOUNDEXPF 'DEFINED-ON-LINE '1288) 
(PUT 'OFSF_PEPLBOUNDEXPF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPLBOUNDEXPF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPLBOUNDEXPF (X K)
    (COND
     ((RAT_LESS X (RAT_0))
      (MULTSQ (RAT_1)
              (INVSQ
               (ADDSQ (OFSF_PEPEXPF (RAT_ABS X) K)
                      (MULTSQ
                       (MULTSQ (EXPTSQ (SIMP 3) (OFSF_PEPCEILING (RAT_ABS X)))
                               (INVSQ (SIMP (OFSF_FACTSF (ADDF K 1)))))
                       (EXPTSQ X (ADDF K 1)))))))
     (T (OFSF_PEPEXPF X K)))) 
(PUT 'OFSF_PEPEXPF 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_PEPEXPF 'DEFINED-ON-LINE '1304) 
(PUT 'OFSF_PEPEXPF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPEXPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_PEPEXPF (X K) (EXPAPPROXRED X K)) 
(PUT 'EXPAPPROXRED 'NUMBER-OF-ARGS 2) 
(PUT 'EXPAPPROXRED 'DEFINED-ON-LINE '1313) 
(PUT 'EXPAPPROXRED 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'EXPAPPROXRED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPAPPROXRED (X K)
    (PROG (SR)
      (COND ((RAT_LEQ X (LNAPPROX (SIMP 1))) (RETURN (EXPAPPROX X K)))
            (T
             (PROGN
              (SETQ SR (FINDSR X))
              (RETURN
               (MULTSQ (EXPTSQ (SIMP 2) (CAR SR))
                       (EXPAPPROXRED (CDR SR) K)))))))) 
(PUT 'FINDSR 'NUMBER-OF-ARGS 1) 
(PUT 'FINDSR 'DEFINED-ON-LINE '1325) 
(PUT 'FINDSR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'FINDSR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDSR (X) (FINDSR_HELPER X 0)) 
(PUT 'FINDSR_HELPER 'NUMBER-OF-ARGS 2) 
(PUT 'FINDSR_HELPER 'DEFINED-ON-LINE '1328) 
(PUT 'FINDSR_HELPER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'FINDSR_HELPER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDSR_HELPER (X S)
    (COND ((RAT_LEQ X (LNAPPROX (SIMP 1))) (CONS S X))
          (T (FINDSR_HELPER (ADDSQ X (NEGSQ (LNAPPROX (SIMP 1)))) (ADDF S 1))))) 
(PUT 'LNAPPROX 'NUMBER-OF-ARGS 1) 
(PUT 'LNAPPROX 'DEFINED-ON-LINE '1336) 
(PUT 'LNAPPROX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'LNAPPROX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LNAPPROX (X)
    (PROG (N G SUM)
      (SETQ N 20)
      (SETQ G 0)
      (SETQ SUM X)
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (COND
         ((EQ G 0)
          (PROGN
           (SETQ SUM
                   (ADDSQ SUM (NEGSQ (MULTSQ (EXPTSQ X I) (INVSQ (SIMP I))))))
           (SETQ G 1)))
         (T
          (PROGN
           (SETQ SUM (ADDSQ SUM (MULTSQ (EXPTSQ X I) (INVSQ (SIMP I)))))
           (SETQ G 0))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN SUM))) 
(PUT 'EXPAPPROX 'NUMBER-OF-ARGS 2) 
(PUT 'EXPAPPROX 'DEFINED-ON-LINE '1353) 
(PUT 'EXPAPPROX 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'EXPAPPROX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPAPPROX (X K)
    (PROG (SUM XI FI)
      (COND ((RAT_NULLP X) (RETURN (SIMP 1))))
      (SETQ SUM (SIMP 1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
        (PROGN
         (SETQ XI (EXPTSQ (RAT_ABS X) I))
         (SETQ FI (OFSF_FACTSF I))
         (SETQ SUM (ADDSQ SUM (MULTSQ XI (INVSQ (SIMP FI))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((RAT_LESS X (RAT_0)) (SETQ SUM (MULTSQ (RAT_1) (INVSQ SUM)))))
      (RETURN SUM))) 
(PUT 'OFSF_FACTSF 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_FACTSF 'DEFINED-ON-LINE '1371) 
(PUT 'OFSF_FACTSF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_FACTSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_FACTSF (X)
    (COND ((LEQ X 1) 1)
          (T
           ((LAMBDA (G486)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF X G486))
                    (T (POLY-MULTF X G486))))
            (OFSF_FACTSF (ADDF X (NEGF 1))))))) 
(PUT 'OFSF_PEPCEILING 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_PEPCEILING 'DEFINED-ON-LINE '1380) 
(PUT 'OFSF_PEPCEILING 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'OFSF_PEPCEILING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_PEPCEILING (X)
    (PROG ()
      (SETQ *ROUNDED T)
      (SETDMODE 'ROUNDED T)
      (SETQ X (CAR (SIMP (PREPSQ X))))
      (COND ((PAIRP X) (SETQ X (CEILING (CDR X)))) ((NULL X) (SETQ X 0))
            (T (SETQ X (CEILING X))))
      (SETDMODE 'ROUNDED NIL)
      (SETQ *ROUNDED NIL)
      (RETURN X))) 
(PUT 'ANU_REFINELIST 'NUMBER-OF-ARGS 3) 
(PUT 'ANU_REFINELIST 'DEFINED-ON-LINE '1402) 
(PUT 'ANU_REFINELIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'ANU_REFINELIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ANU_REFINELIST (ANUL X K)
    (PROG (CANU A L)
      (SETQ CANU (CAR ANUL))
      (SETQ L (LIST CANU))
      (SETQ ANUL (CDR ANUL))
      (PROG ()
       WHILELABEL
        (COND ((NOT ANUL) (RETURN NIL)))
        (PROGN
         (COND
          ((EQ (IV_COMP (ANU_IV CANU) (ANU_IV (CAR ANUL))) 0)
           (PROGN
            (COND
             ((NOT
               (AND (IV_CONTAINSZERO (ANU_IV CANU))
                    (IV_CONTAINSZERO (ANU_IV (CAR ANUL)))
                    (NULL
                     (CAR
                      (SIMP
                       (RAT_SGN
                        (OFSF_PEPSUBSQ
                         (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP CANU))))) X (RAT_0)
                         K)))))
                    (NULL
                     (CAR
                      (SIMP
                       (RAT_SGN
                        (OFSF_PEPSUBSQ
                         (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP (CAR ANUL)))))) X
                         (RAT_0) K)))))))
              (PROGN
               (SETQ A (DPEP_ANU_REFINE CANU (CAR ANUL) X K))
               (SETQ L (ANU_DELETE CANU L))
               (COND
                ((EQ (IV_COMP (ANU_IV (CAR A)) (ANU_IV (CDR A))) (MINUS 1))
                 (PROGN (SETQ L (CONS (CAR A) L)) (SETQ L (CONS (CDR A) L))))
                (T
                 (PROGN
                  (SETQ L (CONS (CDR A) L))
                  (SETQ L (CONS (CAR A) L))))))))))
          (T (SETQ L (CONS (CAR ANUL) L))))
         (SETQ CANU (CAR L))
         (SETQ ANUL (CDR ANUL)))
        (GO WHILELABEL))
      (RETURN (REVERSE L)))) 
(PUT 'DPEP_ANU_REFINE 'NUMBER-OF-ARGS 4) 
(PUT 'DPEP_ANU_REFINE 'DEFINED-ON-LINE '1447) 
(PUT 'DPEP_ANU_REFINE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'DPEP_ANU_REFINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DPEP_ANU_REFINE (ANU1 ANU2 X K)
    (PROG (IV1 IV2 M1 M2 P1 P2 SGNP1M SGNP2M SGNP1IVL SGNP2IVL SGNPP1 SGNPP2
           IVL IVR)
      (SETQ IV1 (ANU_IV ANU1))
      (SETQ IV2 (ANU_IV ANU2))
      (SETQ P1 (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP ANU1))))))
      (SETQ P2 (CAR (SIMP (PREPSQ (AEX_EX (ANU_DP ANU2))))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ M1 (IV_MED IV1))
         (SETQ M2 (IV_MED IV2))
         (SETQ SGNP1M (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ P1 X M1 K)))))
         (SETQ SGNP1IVL
                 (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ P1 X (IV_LB IV1) K)))))
         (SETQ SGNPP1
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF SGNP1IVL SGNP1M))
                       (T (POLY-MULTF SGNP1IVL SGNP1M))))
         (SETQ SGNP2M (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ P2 X M2 K)))))
         (SETQ SGNP2IVL
                 (CAR (SIMP (RAT_SGN (OFSF_PEPSUBSQ P2 X (IV_LB IV2) K)))))
         (SETQ SGNPP2
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF SGNP2IVL SGNP2M))
                       (T (POLY-MULTF SGNP2IVL SGNP2M))))
         (COND
          ((NULL SGNP1M)
           (PROGN
            (SETQ IVL
                    (ADDSQ (IV_LB IV1)
                           (MULTSQ (ADDSQ (IV_RB IV1) (NEGSQ (IV_LB IV1)))
                                   (INVSQ (SIMP 4)))))
            (SETQ IVR
                    (ADDSQ (IV_LB IV1)
                           (MULTSQ
                            (MULTSQ (SIMP 3)
                                    (ADDSQ (IV_RB IV1) (NEGSQ (IV_LB IV1))))
                            (INVSQ (SIMP 4)))))
            (SETQ IV1 (IV_MK IVL IVR))))
          ((MINUSF SGNPP1) (SETQ IV1 (IV_MK (IV_LB IV1) M1)))
          (T (SETQ IV1 (IV_MK M1 (IV_RB IV1)))))
         (COND
          ((NULL SGNP2M)
           (PROGN
            (SETQ IVL
                    (ADDSQ (IV_LB IV2)
                           (MULTSQ (ADDSQ (IV_RB IV2) (NEGSQ (IV_LB IV2)))
                                   (INVSQ (SIMP 4)))))
            (SETQ IVR
                    (ADDSQ (IV_LB IV2)
                           (MULTSQ
                            (MULTSQ (SIMP 3)
                                    (ADDSQ (IV_RB IV2) (NEGSQ (IV_LB IV2))))
                            (INVSQ (SIMP 4)))))
            (SETQ IV2 (IV_MK IVL IVR))))
          ((MINUSF SGNPP2) (SETQ IV2 (IV_MK (IV_LB IV2) M2)))
          (T (SETQ IV2 (IV_MK M2 (IV_RB IV2))))))
        (COND ((NOT (NEQ (IV_COMP IV1 IV2) 0)) (GO REPEATLABEL))))
      (SETQ ANU1 (LIST 'ANU (AEX_EX ANU1) IV1))
      (SETQ ANU2 (LIST 'ANU (AEX_EX ANU2) IV2))
      (RETURN (CONS ANU1 ANU2)))) 
(PUT 'ANU_SORTLIST 'NUMBER-OF-ARGS 1) 
(PUT 'ANU_SORTLIST 'DEFINED-ON-LINE '1516) 
(PUT 'ANU_SORTLIST 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'ANU_SORTLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANU_SORTLIST (ANUL)
    (PROG (ANUMIN SORT)
      (PROG ()
       WHILELABEL
        (COND ((NOT ANUL) (RETURN NIL)))
        (PROGN
         (SETQ ANUMIN (ANU_MIN ANUL))
         (SETQ SORT (CONS ANUMIN SORT))
         (SETQ ANUL (ANU_DELETE ANUMIN ANUL)))
        (GO WHILELABEL))
      (SETQ SORT (REVERSE SORT))
      (RETURN SORT))) 
(PUT 'ANU_DELETE 'NUMBER-OF-ARGS 2) 
(PUT 'ANU_DELETE 'DEFINED-ON-LINE '1530) 
(PUT 'ANU_DELETE 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'ANU_DELETE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ANU_DELETE (ANU ANUL)
    (PROGN
     (SETQ ANUL
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I ANUL)
              STARTOVER
               (COND ((NULL I) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (I) (COND ((NEQ I ANU) (LIST I)))) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ I (CDR I))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL I) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (I) (COND ((NEQ I ANU) (LIST I)))) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ I (CDR I))
               (GO LOOPLABEL)))
     ANUL)) 
(PUT 'ANU_MIN 'NUMBER-OF-ARGS 1) 
(PUT 'ANU_MIN 'DEFINED-ON-LINE '1541) 
(PUT 'ANU_MIN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFDPEP.RED) 
(PUT 'ANU_MIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANU_MIN (ANUL)
    (PROG (ANUMIN)
      (SETQ ANUMIN (CAR ANUL))
      (SETQ ANUL (CDR ANUL))
      (PROG ()
       WHILELABEL
        (COND ((NOT ANUL) (RETURN NIL)))
        (PROGN
         (COND
          ((EQ (IV_COMP (ANU_IV (CAR ANUL)) (ANU_IV ANUMIN)) (MINUS 1))
           (SETQ ANUMIN (CAR ANUL))))
         (COND
          ((EQ (IV_COMP (ANU_IV (CAR ANUL)) (ANU_IV ANUMIN)) 0)
           (COND
            ((RAT_LESS (IV_LB (ANU_IV (CAR ANUL))) (IV_LB (ANU_IV ANUMIN)))
             (SETQ ANUMIN (CAR ANUL))))))
         (SETQ ANUL (CDR ANUL))
         NIL)
        (GO WHILELABEL))
      (RETURN ANUMIN))) 
(ENDMODULE) 