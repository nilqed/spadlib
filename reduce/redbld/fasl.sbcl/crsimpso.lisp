(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPSOLUTION)) 
(FLAG '(DROPREDUNDANT) 'OPFN) 
(PUT 'DROPREDUNDANT 'NUMBER-OF-ARGS 4) 
(PUT 'DROPREDUNDANT 'DEFINED-ON-LINE '36) 
(PUT 'DROPREDUNDANT 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'DROPREDUNDANT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROPREDUNDANT (EX FL VL UNEQU)
    (PROG (A)
      (SETQ VL (UNION (REVERSE (ARGSET (CDR FL))) (CDR VL)))
      (COND ((NULL FTEM_) (SETQ FTEM_ (CDR FL))))
      (SETQ A
              (DROPREDUND
               (LIST
                (PROG (A FORALL-RESULT FORALL-ENDPTR)
                  (SETQ A (CDR EX))
                  (COND ((NULL A) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A)
                                      (SIMP
                                       (COND
                                        ((AND (PAIRP A) (EQUAL (CAR A) 'EQUAL))
                                         (CADDR A))
                                        (T A))))
                                    (CAR A))
                                   NIL)))
                 LOOPLABEL
                  (SETQ A (CDR A))
                  (COND ((NULL A) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (A)
                              (SIMP
                               (COND
                                ((AND (PAIRP A) (EQUAL (CAR A) 'EQUAL))
                                 (CADDR A))
                                (T A))))
                            (CAR A))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))
                (LIST) (CDR FL)
                (PROG (A FORALL-RESULT FORALL-ENDPTR)
                  (SETQ A (CDR UNEQU))
                  (COND ((NULL A) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (A) (SIMP A)) (CAR A)) NIL)))
                 LOOPLABEL
                  (SETQ A (CDR A))
                  (COND ((NULL A) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (A) (SIMP A)) (CAR A)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               NIL VL))
      (RETURN
       (COND
        (A
         (LIST 'LIST (CONS 'LIST (CAR A))
               (CONS 'LIST
                     (PROG (B FORALL-RESULT FORALL-ENDPTR)
                       (SETQ B (CADDR A))
                       (COND ((NULL B) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (B)
                                           (LIST 'EQUAL (CADR B)
                                                 (PREPSQ (CADDR B))))
                                         (CAR B))
                                        NIL)))
                      LOOPLABEL
                       (SETQ B (CDR B))
                       (COND ((NULL B) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (B)
                                   (LIST 'EQUAL (CADR B) (PREPSQ (CADDR B))))
                                 (CAR B))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               (CONS 'LIST (CADDDR A))))
        (T NIL))))) 
(PUT 'DEL_REDUNDANT_FC 'NUMBER-OF-ARGS 1) 
(PUT 'DEL_REDUNDANT_FC 'DEFINED-ON-LINE '63) 
(PUT 'DEL_REDUNDANT_FC 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'DEL_REDUNDANT_FC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEL_REDUNDANT_FC (ARGLIST)
    (PROG (P F FLI NOFL FRED REDU DROPPED OLDPDES NEWPDES NEWPVAL BAK PROLIBAK
           NEWFORG)
      (SETQ BAK FSUB_)
      (PROG (F)
        (SETQ F (SETQ NEWFORG (SUB_FSUB_IN_ITSELF_AND_IN_FORG (CADR ARGLIST))))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND ((NOT (PAIRP F)) (SETQ NOFL (CONS F NOFL)))
                 ((EQUAL (CAR F) 'EQUAL) (SETQ FLI (CONS F FLI)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ FRED (SETDIFF FTEM_ NOFL))
      (COND
       ((NULL FRED)
        (RETURN (COND ((NULL BAK) NIL) (T (LIST (CAR ARGLIST) NEWFORG))))))
      (SETQ FSUB_ BAK)
      (SETQ BAK (BACKUP_PDES (CAR ARGLIST) (CADR ARGLIST)))
      (SETQ PROLIBAK PROC_LIST_)
      (SETQ PROC_LIST_ DEFAULT_PROC_LIST_)
      (COND
       (EXPERT_MODE
        (PROGN
         (PROGN
          (PRIN2 "Which functions shall be checked for redundancy? ")
          NIL)
         (SETQ F (SELECT_FROM_LIST FRED NIL))
         (COND
          (F (PROGN (SETQ NOFL (APPEND NOFL (SETDIFF FRED F))) (SETQ FRED F))))
         NIL)))
      (SETQ REDU
              (DROPREDUND
               (LIST
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P (CAR ARGLIST))
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (P) (GET P 'SQVAL)) (CAR P))
                                        NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (P) (GET P 'SQVAL)) (CAR P)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))
                FLI FRED INEQ_)
               NOFL VL_))
      (SETQ OLDPDES (CAR (RESTORE_PDES BAK)))
      (SETQ PROC_LIST_ PROLIBAK)
      (COND
       ((AND REDU (CAR REDU))
        (PROGN
         (PROG (F)
           (SETQ F (CAR REDU))
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (PROGN
               (SETQ DROPPED (CONS (CADR F) DROPPED))
               (DROP_FCT (CADR F))))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (SETQ FTEM_ (SETDIFF FTEM_ DROPPED))
         (SETQ FLIN_ (SETDIFF FLIN_ DROPPED))
         (SETQ NEWPVAL (CADR REDU))
         (PROG ()
          WHILELABEL
           (COND ((NOT NEWPVAL) (RETURN NIL)))
           (PROGN
            (SETQ NEWPDES
                    (COND
                     ((EQUAL (GET (CAR OLDPDES) 'SQVAL) (CAR NEWPVAL))
                      (EQINSERT (CAR OLDPDES) NEWPDES))
                     (T
                      (PROGN
                       (PROG (F)
                         (SETQ F ALLFLAGS_)
                        LAB
                         (COND ((NULL F) (RETURN NIL)))
                         ((LAMBDA (F) (FLAG (LIST (CAR OLDPDES)) F)) (CAR F))
                         (SETQ F (CDR F))
                         (GO LAB))
                       (SETQ P
                               (UPDATESQ (CAR OLDPDES) (CAR NEWPVAL) NIL NIL
                                (GET (CAR OLDPDES) 'FCTS)
                                (GET (CAR OLDPDES) 'VARS) T (LIST 0) NEWPDES))
                       (COND
                        ((NULL P)
                         (PROGN
                          (DROP_PDE_FROM_IDTIES (CAR OLDPDES) (CAR ARGLIST)
                           NIL)
                          (DROP_PDE_FROM_PROPERTIES (CAR OLDPDES)
                           (CAR ARGLIST))))
                        (T (EQINSERT P NEWPDES)))))))
            (SETQ NEWPVAL (CDR NEWPVAL))
            (SETQ OLDPDES (CDR OLDPDES)))
           (GO WHILELABEL))
         (PROG (F)
           (SETQ F DROPPED)
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (PROG (P)
                (SETQ P (CADDR REDU))
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P)
                   (PROGN
                    (COND
                     ((AND (PAIRP P) (EQUAL (CAR P) 'EQUAL))
                      (PUT (CADR P) 'FCTS (DELETE F (GET (CADR P) 'FCTS)))))))
                 (CAR P))
                (SETQ P (CDR P))
                (GO LAB)))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (RETURN
          (LIST NEWPDES
                (APPEND (CADDR REDU)
                        (SETDIFF NOFL (SETDIFF NOFL (CADR ARGLIST))))))))))) 
(PUT 'DROPREDUND 'NUMBER-OF-ARGS 3) 
(PUT 'DROPREDUND 'DEFINED-ON-LINE '155) 
(PUT 'DROPREDUND 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'DROPREDUND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROPREDUND (A NOFL VL)
    (COND
     ((CADR A)
      (COND
       ((GREATERP (LENGTH (CADR A)) 0)
        (COND
         ((CADDR A)
          (COND
           ((GREATERP (LENGTH (CADDR A)) 0)
            (PROG (SOL ARBIT FL EL1 EL2 EL3 CORRES B B1 B2 CONDI OLDCON REDUND
                   FLSTART FLDROP FLDUPLI2 NEWFU NEWCORRES UNEQU UNSOLVED VLF
                   VLA POTOLD NEWNEWFU FLDUPLI NOFL_ARBIT INEQ_BAK INEQ_OR_BAK
                   VL_BAK FTEM_BAK PROC_LIST_BAK SESSION_BAK ADJUST_FNC_BAK
                   LEVEL_BAK COLLECT_SOL_BAK BATCH_MODE_OLD PRINTOLD
                   BATCHCOUNT_BAK STEPCOUNTER_BAK FLIN_BAK OLD_HIST_BAK)
              (SETQ PRINTOLD PRINT_)
              (COND
               ((AND (NULL PRINT_) (NULL BATCH_MODE_SUB)) (SETQ PRINT_ 8)))
              (COND ((AND PRINT_ BATCH_MODE_SUB) (SETQ PRINT_ NIL)))
              (SETQ BATCH_MODE_OLD *BATCH_MODE)
              (SETQ *BATCH_MODE BATCH_MODE_SUB)
              (COND
               ((NOT *BATCH_MODE)
                (PROGN
                 (PROGN
                  (PRIN2
                   "-------------------------------------------------------------")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   " A new CRACK computation starts to find redundand functions. ")
                  NIL)
                 (TERPRI)
                 NIL)))
              (PROG (EL1)
                (SETQ EL1 (APPEND (CAR A) (CADR A)))
               LAB
                (COND ((NULL EL1) (RETURN NIL)))
                ((LAMBDA (EL1)
                   (COND
                    (EL1
                     (SETQ B1
                             (CONS
                              (COND
                               ((PAIRP EL1)
                                (COND ((EQUAL (CAR EL1) 'EQUAL) (CADDR EL1))
                                      (T EL1)))
                               (T EL1))
                              B1)))))
                 (CAR EL1))
                (SETQ EL1 (CDR EL1))
                (GO LAB))
              (SETQ B2 B1)
              (SETQ ARBIT (CADDR A))
              (SETQ FLIN_BAK FLIN_)
              (PROG (EL1)
                (SETQ EL1 ARBIT)
               LAB
                (COND ((NULL EL1) (RETURN NIL)))
                ((LAMBDA (EL1)
                   (COND
                    ((NOT (FREEOF NOFL EL1))
                     (SETQ NOFL_ARBIT (CONS EL1 NOFL_ARBIT)))
                    ((NOT (MY_FREEOF B1 EL1))
                     (PROGN
                      (SETQ FLSTART (CONS EL1 FLSTART))
                      (SETQ EL2 (NEWFCT FNAME_ (FCTARGS EL1) NFCT_))
                      (COND
                       ((NOT (FREEOF FLIN_ EL1))
                        (SETQ FLIN_ (CONS EL2 FLIN_))))
                      (SETQ FLDUPLI (CONS EL2 FLDUPLI))
                      (SETQ NFCT_ (ADD1 NFCT_))
                      (SETQ B2
                              (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                (SETQ B B2)
                                (COND ((NULL B) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (B)
                                                    (SUBSQ B
                                                           (LIST
                                                            (CONS EL1 EL2))))
                                                  (CAR B))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ B (CDR B))
                                (COND ((NULL B) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (B)
                                            (SUBSQ B (LIST (CONS EL1 EL2))))
                                          (CAR B))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ FL (CONS EL1 (CONS EL2 FL)))
                      (SETQ CORRES (CONS (CONS EL1 EL2) CORRES))
                      NIL))
                    (T (SETQ FLDROP (CONS EL1 FLDROP)))))
                 (CAR EL1))
                (SETQ EL1 (CDR EL1))
                (GO LAB))
              (PROG ()
               WHILELABEL
                (COND ((NOT B1) (RETURN NIL)))
                (PROGN
                 (SETQ CONDI (CONS (ADDSQ (CAR B1) (NEGSQ (CAR B2))) CONDI))
                 (SETQ B1 (CDR B1))
                 (SETQ B2 (CDR B2)))
                (GO WHILELABEL))
              (SETQ B1 NIL)
              (SETQ B2 NIL)
              (SETQ POTOLD POTINT_)
              (SETQ POTINT_ T)
              (SETQ SESSION_BAK SESSION_)
              (SETQ SESSION_ NIL)
              (SETQ INEQ_BAK INEQ_)
              (SETQ INEQ_ NIL)
              (SETQ INEQ_OR_BAK INEQ_OR)
              (SETQ INEQ_OR NIL)
              (SETQ VL_BAK VL_)
              (SETQ VL_ VL)
              (SETQ OLD_HIST_BAK OLD_HISTORY)
              (SETQ OLD_HISTORY NIL)
              (SETQ FTEM_BAK FTEM_)
              (SETQ FTEM_ NIL)
              (SETQ LEVEL_BAK LEVEL_)
              (SETQ LEVEL_ NIL)
              (PROG (B)
                (SETQ B FLDUPLI)
               LAB
                (COND ((NULL B) (RETURN NIL)))
                ((LAMBDA (B) (SETQ FTEM_ (FCTINSERT B FTEM_))) (CAR B))
                (SETQ B (CDR B))
                (GO LAB))
              (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
              (COND
               ((NOT (FREEOF PROC_LIST_ 'STOP_BATCH))
                (PROGN
                 (SETQ PROC_LIST_BAK PROC_LIST_)
                 (SETQ PROC_LIST_ (DELETE 'STOP_BATCH PROC_LIST_)))))
              (SETQ ADJUST_FNC_BAK ADJUST_FNC)
              (SETQ ADJUST_FNC NIL)
              (SETQ COLLECT_SOL_BAK COLLECT_SOL)
              (SETQ COLLECT_SOL T)
              (SETQ BATCHCOUNT_BAK BATCHCOUNT_)
              (SETQ BATCHCOUNT_ (MINUS 1))
              (SETQ STEPCOUNTER_BAK STEPCOUNTER_)
              (SETQ STEPCOUNTER_ 0)
              (SETQ B
                      (CRACKMAIN
                       (MKEQSQLIST CONDI NIL NIL FL VL_ ALLFLAGS_ T (LIST 0)
                        NIL)
                       FL))
              (SETQ BATCHCOUNT_ BATCHCOUNT_BAK)
              (SETQ STEPCOUNTER_ STEPCOUNTER_BAK)
              (SETQ COLLECT_SOL COLLECT_SOL_BAK)
              (SETQ FLIN_ FLIN_BAK)
              (SETQ ADJUST_FNC ADJUST_FNC_BAK)
              (SETQ LEVEL_ LEVEL_BAK)
              (SETQ INEQ_ INEQ_BAK)
              (SETQ INEQ_OR INEQ_OR_BAK)
              (SETQ VL_ VL_BAK)
              (SETQ FTEM_ FTEM_BAK)
              (COND (PROC_LIST_BAK (SETQ PROC_LIST_ PROC_LIST_BAK)))
              (SETQ OLD_HISTORY OLD_HIST_BAK)
              (PROG (B1)
                (SETQ B1 B)
               LAB
                (COND ((NULL B1) (RETURN NIL)))
                ((LAMBDA (B1)
                   (COND
                    ((PAIRP B1)
                     (PROGN
                      (SETQ EL1 T)
                      (PROG (EL2)
                        (SETQ EL2 (CADR B1))
                       LAB
                        (COND ((NULL EL2) (RETURN NIL)))
                        ((LAMBDA (EL2)
                           (COND
                            ((AND (PAIRP EL2) (EQUAL (CAR EL2) 'EQUAL)
                                  (NULL (SMEMBERL FL (CADDR EL2)))
                                  (NULL (SMEMBERL (CADDR B1) (CADDR EL2))))
                             (SETQ EL1 NIL))))
                         (CAR EL2))
                        (SETQ EL2 (CDR EL2))
                        (GO LAB))
                      (COND (EL1 (SETQ B2 (CONS B1 B2))))
                      NIL))))
                 (CAR B1))
                (SETQ B1 (CDR B1))
                (GO LAB))
              (SETQ POTINT_ POTOLD)
              (SETQ SESSION_ SESSION_BAK)
              (SETQ PRINT_ PRINTOLD)
              (COND
               ((NOT *BATCH_MODE)
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   " The CRACK computation to find redundand functions finished.")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "------------------------------------------------------------")
                  NIL)
                 (TERPRI)
                 NIL)))
              (SETQ *BATCH_MODE BATCH_MODE_OLD)
              (COND
               ((NULL B2)
                (RETURN
                 (PROGN
                  (PROG (EL1)
                    (SETQ EL1 (APPEND FLDUPLI FLDROP))
                   LAB
                    (COND ((NULL EL1) (RETURN NIL)))
                    ((LAMBDA (EL1) (DROP_FCT EL1)) (CAR EL1))
                    (SETQ EL1 (CDR EL1))
                    (GO LAB))
                  (COND ((NULL FLDROP) NIL)
                        (T
                         (PROGN
                          (SETQ REDUND
                                  (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ EL1 FLDROP)
                                    (COND ((NULL EL1) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (EL1)
                                                        (LIST 'EQUAL EL1 0))
                                                      (CAR EL1))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ EL1 (CDR EL1))
                                    (COND ((NULL EL1) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL1)
                                                (LIST 'EQUAL EL1 0))
                                              (CAR EL1))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                          (SETQ EL2
                                  (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ EL1 FLDROP)
                                    (COND ((NULL EL1) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (EL1)
                                                        (CONS EL1 0))
                                                      (CAR EL1))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ EL1 (CDR EL1))
                                    (COND ((NULL EL1) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL1) (CONS EL1 0))
                                              (CAR EL1))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                          (SETQ OLDCON
                                  (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ EL3 (CAR A))
                                    (COND ((NULL EL3) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (EL3)
                                                        (SUBSQ EL3 EL2))
                                                      (CAR EL3))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ EL3 (CDR EL3))
                                    (COND ((NULL EL3) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL3) (SUBSQ EL3 EL2))
                                              (CAR EL3))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                          (SETQ SOL
                                  (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ EL3 (CADR A))
                                    (COND ((NULL EL3) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (EL3)
                                                        (LIST 'EQUAL (CADR EL3)
                                                              (SUBSQ
                                                               (CADDR EL3)
                                                               EL2)))
                                                      (CAR EL3))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ EL3 (CDR EL3))
                                    (COND ((NULL EL3) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL3)
                                                (LIST 'EQUAL (CADR EL3)
                                                      (SUBSQ (CADDR EL3) EL2)))
                                              (CAR EL3))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                          (SETQ UNEQU
                                  (PROG (EL3 FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ EL3 (CADDDR A))
                                    (COND ((NULL EL3) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (EL3)
                                                        (SUBSQ EL3 EL2))
                                                      (CAR EL3))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ EL3 (CDR EL3))
                                    (COND ((NULL EL3) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL3) (SUBSQ EL3 EL2))
                                              (CAR EL3))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                          (LIST REDUND OLDCON SOL (UNION NOFL_ARBIT FLSTART)
                                UNEQU)))))))
               (T (SETQ B (CAR B2))))
              (SETQ UNSOLVED (CAR B))
              (SETQ ARBIT (CADDR B))
              (PROG (EL1)
                (SETQ EL1 (CADR B))
               LAB
                (COND ((NULL EL1) (RETURN NIL)))
                ((LAMBDA (EL1)
                   (COND
                    ((NOT (AND (PAIRP EL1) (EQUAL (CAR EL1) 'EQUAL)))
                     (SETQ ARBIT (CONS EL1 ARBIT)))
                    (T (SETQ NEWFU (CONS EL1 NEWFU)))))
                 (CAR EL1))
                (SETQ EL1 (CDR EL1))
                (GO LAB))
              (SETQ OLDCON (CAR A))
              (SETQ SOL (CADR A))
              (SETQ UNEQU (CADDDR A))
              (SETQ FLDUPLI2 FLDUPLI)
              (PROG (EL1)
                (SETQ EL1 CORRES)
               LAB
                (COND ((NULL EL1) (RETURN NIL)))
                ((LAMBDA (EL1)
                   (COND
                    ((AND (MEMBER (CAR EL1) ARBIT) (MEMBER (CDR EL1) ARBIT)
                          (FREEOF UNSOLVED (CAR EL1))
                          (FREEOF UNSOLVED (CDR EL1)))
                     (PROGN
                      (SETQ REDUND (CONS (LIST 'EQUAL (CAR EL1) 0) REDUND))
                      (SETQ FLDROP (CONS (CAR EL1) FLDROP))
                      (SETQ OLDCON
                              (PROG (EL2 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL2 OLDCON)
                                (COND ((NULL EL2) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL2)
                                                    (SUBSQ EL2
                                                           (LIST
                                                            (CONS (CAR EL1)
                                                                  0))))
                                                  (CAR EL2))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL2 (CDR EL2))
                                (COND ((NULL EL2) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL2)
                                            (SUBSQ EL2
                                                   (LIST (CONS (CAR EL1) 0))))
                                          (CAR EL2))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ SOL
                              (PROG (EL2 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL2 SOL)
                                (COND ((NULL EL2) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL2)
                                                    (PROGN
                                                     (COND
                                                      ((AND (PAIRP EL2)
                                                            (EQUAL (CAR EL2)
                                                                   'EQUAL))
                                                       (PUT (CADR EL2) 'FCTS
                                                            (DELETE (CAR EL1)
                                                                    (GET
                                                                     (CADR EL2)
                                                                     'FCTS)))))
                                                     (LIST 'EQUAL (CADR EL2)
                                                           (SUBSQ (CADDR EL2)
                                                                  (LIST
                                                                   (CONS
                                                                    (CAR EL1)
                                                                    0))))))
                                                  (CAR EL2))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL2 (CDR EL2))
                                (COND ((NULL EL2) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL2)
                                            (PROGN
                                             (COND
                                              ((AND (PAIRP EL2)
                                                    (EQUAL (CAR EL2) 'EQUAL))
                                               (PUT (CADR EL2) 'FCTS
                                                    (DELETE (CAR EL1)
                                                            (GET (CADR EL2)
                                                                 'FCTS)))))
                                             (LIST 'EQUAL (CADR EL2)
                                                   (SUBSQ (CADDR EL2)
                                                          (LIST
                                                           (CONS (CAR EL1)
                                                                 0))))))
                                          (CAR EL2))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ UNEQU
                              (PROG (EL2 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL2 UNEQU)
                                (COND ((NULL EL2) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL2)
                                                    (SUBSQ EL2
                                                           (LIST
                                                            (CONS (CAR EL1)
                                                                  0))))
                                                  (CAR EL2))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL2 (CDR EL2))
                                (COND ((NULL EL2) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL2)
                                            (SUBSQ EL2
                                                   (LIST (CONS (CAR EL1) 0))))
                                          (CAR EL2))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ ARBIT (DELETE (CAR EL1) ARBIT))
                      (SETQ ARBIT (DELETE (CDR EL1) ARBIT))
                      (SETQ FL (DELETE (CAR EL1) FL))
                      (SETQ FL (DELETE (CDR EL1) FL))
                      (SETQ FLSTART (DELETE (CAR EL1) FLSTART))
                      (SETQ FLDUPLI2 (DELETE (CDR EL1) FLDUPLI2))
                      (SETQ NEWFU
                              (PROG (EL2 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL2 NEWFU)
                                (COND ((NULL EL2) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL2)
                                                    (LIST 'EQUAL (CADR EL2)
                                                          (SUBSQ (CADDR EL2)
                                                                 (LIST
                                                                  (CONS
                                                                   (CAR EL1) 0)
                                                                  (CONS
                                                                   (CDR EL1)
                                                                   0)))))
                                                  (CAR EL2))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL2 (CDR EL2))
                                (COND ((NULL EL2) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL2)
                                            (LIST 'EQUAL (CADR EL2)
                                                  (SUBSQ (CADDR EL2)
                                                         (LIST
                                                          (CONS (CAR EL1) 0)
                                                          (CONS (CDR EL1)
                                                                0)))))
                                          (CAR EL2))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))
                    (T (SETQ NEWCORRES (CONS EL1 NEWCORRES)))))
                 (CAR EL1))
                (SETQ EL1 (CDR EL1))
                (GO LAB))
              (PROG ()
               WHILELABEL
                (COND ((NOT NEWFU) (RETURN NIL)))
                (PROGN
                 (SETQ EL1 (CAR NEWFU))
                 (SETQ EL2 (CADR EL1))
                 (SETQ B NEWCORRES)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND B (NEQ EL2 (CDAR B)))) (RETURN NIL)))
                   (SETQ B (CDR B))
                   (GO WHILELABEL))
                 (COND
                  (B
                   (COND
                    ((NOT (FREEOF EL1 (CAAR B)))
                     (SETQ NEWNEWFU (CONS EL1 NEWNEWFU)))
                    (T
                     (PROGN
                      (SETQ EL3 NEWFU)
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (AND EL3 (NEQ (CADAR EL3) (CAAR B))))
                          (RETURN NIL)))
                        (SETQ EL3 (CDR EL3))
                        (GO WHILELABEL))
                      (COND
                       (EL3
                        (PROGN
                         (SETQ NEWNEWFU
                                 (CONS
                                  (LIST 'EQUAL EL2
                                        (ADDSQ
                                         (ADDSQ (CADDR EL1) (SIMP (CADAR EL3)))
                                         (NEGSQ (CADDAR EL3))))
                                  NEWNEWFU))
                         (SETQ NEWFU (DELETE (CAR EL3) NEWFU))))
                       (T (SETQ NEWNEWFU NIL)))
                      NIL))))
                  (T
                   (PROGN
                    (SETQ B NEWCORRES)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT (NEQ EL2 (CAAR B))) (RETURN NIL)))
                      (SETQ B (CDR B))
                      (GO WHILELABEL))
                    (COND
                     ((NOT (FREEOF EL1 (CDAR B)))
                      (SETQ NEWNEWFU
                              (CONS
                               (LIST 'EQUAL (CDAR B)
                                     (ADDSQ (ADDSQ (SIMP (CDAR B)) (SIMP EL2))
                                            (NEGSQ (CADDR EL1))))
                               NEWNEWFU)))
                     (T
                      (PROGN
                       (SETQ EL3 NEWFU)
                       (PROG ()
                        WHILELABEL
                         (COND
                          ((NOT (AND EL3 (NEQ (CADAR EL3) (CDAR B))))
                           (RETURN NIL)))
                         (SETQ EL3 (CDR EL3))
                         (GO WHILELABEL))
                       (COND
                        (EL3
                         (PROGN
                          (SETQ NEWNEWFU
                                  (CONS
                                   (LIST 'EQUAL (CDAR B)
                                         (ADDSQ
                                          (ADDSQ (CADDAR EL3)
                                                 (SIMP (CADR EL1)))
                                          (NEGSQ (CADDR EL1))))
                                   NEWNEWFU))
                          (SETQ NEWFU (DELETE (CAR EL3) NEWFU))))
                        (T (SETQ NEWNEWFU NIL)))
                       NIL))))))
                 (SETQ NEWFU (CDR NEWFU)))
                (GO WHILELABEL))
              (SETQ NEWFU NEWNEWFU)
              (COND
               ((EQUAL (LENGTH FLDUPLI2) (LENGTH NEWFU))
                (PROG ()
                 WHILELABEL
                  (COND
                   ((NOT
                     (AND NEWNEWFU (FREEOFLIST (CADDAR NEWNEWFU) FLDUPLI2)))
                    (RETURN NIL)))
                  (SETQ NEWNEWFU (CDR NEWNEWFU))
                  (GO WHILELABEL))))
              (COND
               ((AND NEWFU (NOT NEWNEWFU))
                (PROGN
                 (SETQ ARBIT
                         (SETDIFF (SETDIFF (UNION ARBIT FL) FLDUPLI2) FLSTART))
                 (SETQ NEWFU
                         (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL1 NEWFU)
                           (COND ((NULL EL1) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (EL1)
                                               (PROGN
                                                (SETQ B (CADR EL1))
                                                (SETQ EL2 NEWCORRES)
                                                (PROG ()
                                                 WHILELABEL
                                                  (COND
                                                   ((NOT (NEQ B (CDAR EL2)))
                                                    (RETURN NIL)))
                                                  (SETQ EL2 (CDR EL2))
                                                  (GO WHILELABEL))
                                                (LIST 'EQUAL (CAAR EL2)
                                                      (CADDR EL1))))
                                             (CAR EL1))
                                            NIL)))
                          LOOPLABEL
                           (SETQ EL1 (CDR EL1))
                           (COND ((NULL EL1) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL1)
                                       (PROGN
                                        (SETQ B (CADR EL1))
                                        (SETQ EL2 NEWCORRES)
                                        (PROG ()
                                         WHILELABEL
                                          (COND
                                           ((NOT (NEQ B (CDAR EL2)))
                                            (RETURN NIL)))
                                          (SETQ EL2 (CDR EL2))
                                          (GO WHILELABEL))
                                        (LIST 'EQUAL (CAAR EL2) (CADDR EL1))))
                                     (CAR EL1))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ ARBIT (FCTSORT ARBIT))
                 (PROG (EL1)
                   (SETQ EL1 ARBIT)
                  LAB
                   (COND ((NULL EL1) (RETURN NIL)))
                   ((LAMBDA (EL1)
                      (COND
                       ((FREEOF UNSOLVED EL1)
                        (PROGN
                         (SETQ VLA (FCTARGS EL1))
                         (SETQ EL2 NEWFU)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT EL2) (RETURN NIL)))
                           (COND ((FREEOF (CAR EL2) EL1) (SETQ EL2 (CDR EL2)))
                                 (T
                                  (PROGN
                                   (SETQ VLF (FCTARGS (CADAR EL2)))
                                   (COND
                                    ((AND (NULL (NOT_INCLUDED VLA VLF))
                                          (NULL (NOT_INCLUDED VLF VLA)))
                                     (PROGN
                                      (SETQ B
                                              (LDERIV
                                               (REVAL1
                                                (LIST '*SQ (CADDAR EL2) T) T)
                                               EL1 VLA))
                                      (COND
                                       ((EQUAL (CDR B) 1)
                                        (PROGN
                                         (COND
                                          ((AND (NEQ (CAR B) EL1) PRINT_)
                                           (PROGN
                                            (TERPRI)
                                            (PROGN
                                             (PRIN2
                                              " It is assumed that the equation:")
                                             NIL)
                                            (TERPRI)
                                            (DEPRINT
                                             (LIST (LIST '*SQ (CADDAR EL2) T)))
                                            (PROGN
                                             (PRIN2
                                              " has always a solution in ")
                                             (PRIN2 EL1)
                                             NIL)
                                            (TERPRI)
                                            (PROGN (PRIN2 " functions: ") NIL)
                                            (SETQ EL3 (APPEND FLSTART ARBIT))
                                            (SETQ B NIL)
                                            (PROG ()
                                             WHILELABEL
                                              (COND ((NOT EL3) (RETURN NIL)))
                                              (PROGN
                                               (COND
                                                ((NOT
                                                  (FREEOF (CADDAR EL2)
                                                          (CAR EL3)))
                                                 (SETQ B (CONS (CAR EL3) B))))
                                               (SETQ EL3 (CDR EL3)))
                                              (GO WHILELABEL))
                                            (FCTPRINT B)
                                            (SETQ B NIL))))
                                         (SETQ REDUND
                                                 (CONS
                                                  (LIST 'EQUAL (CADAR EL2) 0)
                                                  REDUND))
                                         (SETQ FLDROP
                                                 (CONS (CADAR EL2) FLDROP))
                                         (SETQ OLDCON
                                                 (PROG (EL3 FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EL3 OLDCON)
                                                   (COND
                                                    ((NULL EL3) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EL3)
                                                                       (SUBSQ
                                                                        EL3
                                                                        (LIST
                                                                         (CONS
                                                                          (CADAR
                                                                           EL2)
                                                                          0))))
                                                                     (CAR EL3))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EL3 (CDR EL3))
                                                   (COND
                                                    ((NULL EL3)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EL3)
                                                               (SUBSQ EL3
                                                                      (LIST
                                                                       (CONS
                                                                        (CADAR
                                                                         EL2)
                                                                        0))))
                                                             (CAR EL3))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                         (SETQ SOL
                                                 (PROG (EL3 FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EL3 SOL)
                                                   (COND
                                                    ((NULL EL3) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EL3)
                                                                       (PROGN
                                                                        (COND
                                                                         ((AND
                                                                           (PAIRP
                                                                            EL3)
                                                                           (EQUAL
                                                                            (CAR
                                                                             EL3)
                                                                            'EQUAL))
                                                                          (PUT
                                                                           (CADR
                                                                            EL3)
                                                                           'FCTS
                                                                           (DELETE
                                                                            (CADAR
                                                                             EL2)
                                                                            (GET
                                                                             (CADR
                                                                              EL3)
                                                                             'FCTS)))))
                                                                        (LIST
                                                                         'EQUAL
                                                                         (CADR
                                                                          EL3)
                                                                         (SUBSQ
                                                                          (CADDR
                                                                           EL3)
                                                                          (LIST
                                                                           (CONS
                                                                            (CADAR
                                                                             EL2)
                                                                            0))))))
                                                                     (CAR EL3))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EL3 (CDR EL3))
                                                   (COND
                                                    ((NULL EL3)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EL3)
                                                               (PROGN
                                                                (COND
                                                                 ((AND
                                                                   (PAIRP EL3)
                                                                   (EQUAL
                                                                    (CAR EL3)
                                                                    'EQUAL))
                                                                  (PUT
                                                                   (CADR EL3)
                                                                   'FCTS
                                                                   (DELETE
                                                                    (CADAR EL2)
                                                                    (GET
                                                                     (CADR EL3)
                                                                     'FCTS)))))
                                                                (LIST 'EQUAL
                                                                      (CADR
                                                                       EL3)
                                                                      (SUBSQ
                                                                       (CADDR
                                                                        EL3)
                                                                       (LIST
                                                                        (CONS
                                                                         (CADAR
                                                                          EL2)
                                                                         0))))))
                                                             (CAR EL3))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                         (SETQ UNEQU
                                                 (PROG (EL3 FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EL3 UNEQU)
                                                   (COND
                                                    ((NULL EL3) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EL3)
                                                                       (SUBSQ
                                                                        EL3
                                                                        (LIST
                                                                         (CONS
                                                                          (CADAR
                                                                           EL2)
                                                                          0))))
                                                                     (CAR EL3))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EL3 (CDR EL3))
                                                   (COND
                                                    ((NULL EL3)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EL3)
                                                               (SUBSQ EL3
                                                                      (LIST
                                                                       (CONS
                                                                        (CADAR
                                                                         EL2)
                                                                        0))))
                                                             (CAR EL3))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                         (SETQ FLSTART
                                                 (DELETE (CADAR EL2) FLSTART))
                                         (SETQ NEWFU (DELETE (CAR EL2) NEWFU))
                                         (SETQ EL2 NIL)
                                         NIL))))))
                                   (COND (EL2 (SETQ EL2 (CDR EL2)))))))
                           (GO WHILELABEL))))))
                    (CAR EL1))
                   (SETQ EL1 (CDR EL1))
                   (GO LAB))
                 (SETQ EL2
                         (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL1 ARBIT)
                           (COND ((NULL EL1) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (EL1) (CONS EL1 0))
                                             (CAR EL1))
                                            NIL)))
                          LOOPLABEL
                           (SETQ EL1 (CDR EL1))
                           (COND ((NULL EL1) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL1) (CONS EL1 0)) (CAR EL1))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ NEWFU
                         (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL1 NEWFU)
                           (COND ((NULL EL1) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (EL1)
                                               (LIST 'EQUAL (CADR EL1)
                                                     (SUBSQ (CADDR EL1) EL2)))
                                             (CAR EL1))
                                            NIL)))
                          LOOPLABEL
                           (SETQ EL1 (CDR EL1))
                           (COND ((NULL EL1) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL1)
                                       (LIST 'EQUAL (CADR EL1)
                                             (SUBSQ (CADDR EL1) EL2)))
                                     (CAR EL1))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))
              (COND
               ((AND FLDROP PRINT_)
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "non-essential dropped constant(s) or function(s): ")
                  NIL)
                 (FCTPRINT FLDROP))))
              (PROG (EL1)
                (SETQ EL1 (APPEND FLDUPLI FLDROP))
               LAB
                (COND ((NULL EL1) (RETURN NIL)))
                ((LAMBDA (EL1) (SETQ DEPL* (DELETE (ASSOC EL1 DEPL*) DEPL*)))
                 (CAR EL1))
                (SETQ EL1 (CDR EL1))
                (GO LAB))
              (RETURN
               (COND ((NULL FLDROP) NIL)
                     (T
                      (LIST REDUND OLDCON SOL (UNION NOFL_ARBIT FLSTART)
                            UNEQU)))))))))))))) 
(FLAG '(NCONTENT) 'OPFN) 
(PUT 'NCONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'NCONTENT 'DEFINED-ON-LINE '586) 
(PUT 'NCONTENT 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'NCONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NCONTENT (P)
    (PROGN
     (SETQ P (SIMP* P))
     (COND ((EQUAL P (CONS 'NIL 1)) 0)
           (T
            (MK*SQ
             (CONS (NUMERIC-CONTENT (CAR P)) (NUMERIC-CONTENT (CDR P)))))))) 
(PUT 'ABSORBCONST 'NUMBER-OF-ARGS 2) 
(FLAG '(ABSORBCONST) 'OPFN) 
(PUT 'ABSORBCONST 'DEFINED-ON-LINE '595) 
(PUT 'ABSORBCONST 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'ABSORBCONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ABSORBCONST (EXLIST FLIST)
    (PROG (E1 E2 N N1 N2 NU SB CS1 CS2 *RATIONAL_BAK)
      (SETQ *RATIONAL_BAK (AEVAL '*RATIONAL))
      (COND ((BOOLVALUE* (REVALX '*RATIONAL)) (AEVAL (OFF (LIST 'RATIONAL)))))
      (SETQ SB (AEVAL (LIST 'LIST)))
      (PROG (E1)
        (SETQ E1 (GETRLIST (AEVAL FLIST)))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ N1 (AEVAL 'NIL))
            (SETQ CS1 (AEVAL 'NIL))
            (SETQ CS2 (AEVAL 'T))
            (PROG (E2)
              (SETQ E2 (GETRLIST (AEVAL EXLIST)))
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (PROGN
                  (SETQ N (AEVAL (LIST 'COEFFN E2 E1 1)))
                  (COND
                   ((EVALNEQ (AEVAL N) 0)
                    (PROGN
                     (COND
                      ((AND (EVALNUMBERP (AEVAL N)) (EVALLESSP (AEVAL N) 0))
                       (SETQ CS1 (AEVAL 'T)))
                      ((BOOLVALUE* (REVALX (PAIRP (REVAL1 (REVALX N) T))))
                       (PROGN
                        (COND
                         ((EVALEQUAL (AEVAL (LIST 'PART N 0)) (AEVAL 'MINUS))
                          (SETQ CS1 (AEVAL 'T)))
                         ((EVALEQUAL (AEVAL (LIST 'PART N 0))
                                     (AEVAL 'QUOTIENT))
                          (PROGN
                           (SETQ NU (AEVAL (LIST 'PART N 1)))
                           (COND
                            ((AND
                              (BOOLVALUE*
                               (REVALX (PAIRP (REVAL1 (REVALX NU) T))))
                              (EVALEQUAL (AEVAL (LIST 'PART NU 0))
                                         (AEVAL 'MINUS)))
                             (SETQ CS1 (AEVAL 'T)))
                            ((AND (EVALNUMBERP (AEVAL NU))
                                  (EVALLESSP (AEVAL NU) 0))
                             (SETQ CS1 (AEVAL 'T)))
                            ((AND (EVALGREATERP (AEVAL (LIST 'ARGLENGTH NU)) 0)
                                  (EVALEQUAL (AEVAL (LIST 'PART NU 0))
                                             (AEVAL 'PLUS))
                                  (EVALGREATERP
                                   (AEVAL (LIST 'ARGLENGTH (LIST 'PART NU 1)))
                                   0)
                                  (EVALEQUAL
                                   (AEVAL (LIST 'PART (LIST 'PART NU 1) 0))
                                   (AEVAL 'MINUS)))
                             (SETQ CS1 (AEVAL 'T)))
                            (T (SETQ CS2 (AEVAL 'NIL))))))
                         ((AND
                           (EVALEQUAL (AEVAL (LIST 'PART N 0)) (AEVAL 'PLUS))
                           (EVALGREATERP
                            (AEVAL (LIST 'ARGLENGTH (LIST 'PART N 1))) 0)
                           (EVALEQUAL (AEVAL (LIST 'PART (LIST 'PART N 1) 0))
                                      (AEVAL 'MINUS)))
                          (SETQ CS1 (AEVAL 'T)))
                         (T (SETQ CS2 (AEVAL 'NIL)))))))
                     (SETQ N (AEVAL (LIST 'NCONTENT N)))
                     (COND
                      ((EVALEQUAL (AEVAL N1) (AEVAL 'NIL))
                       (PROGN
                        (SETQ N1 (AEVAL (LIST 'NUM N)))
                        (SETQ N2 (AEVAL (LIST 'DEN N)))))
                      (T
                       (PROGN
                        (SETQ N1 (AEVAL (LIST 'GCD N1 (LIST 'NUM N))))
                        (SETQ N2
                                (AEVAL
                                 (LIST 'TIMES N2
                                       (LIST 'QUOTIENT (LIST 'DEN N)
                                             (LIST 'GCD N2
                                                   (LIST 'DEN N))))))))))))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (COND
             ((AND (BOOLVALUE* N1)
                   (OR (EVALNEQ (AEVAL N1) 1) (EVALNEQ (AEVAL N2) 1)))
              (PROGN
               (COND
                ((AND (BOOLVALUE* CS1) (BOOLVALUE* CS2))
                 (SETQ N2 (AEVAL (LIST 'MINUS N2)))))
               (SETQ SB
                       (AEVAL
                        (LIST 'CONS
                              (LIST 'EQUAL E1
                                    (LIST 'TIMES E1 (LIST 'QUOTIENT N2 N1)))
                              SB)))
               (AEVAL 'NIL)))
             ((AND (BOOLVALUE* CS1) (BOOLVALUE* CS2))
              (SETQ SB
                      (AEVAL
                       (LIST 'CONS (LIST 'EQUAL E1 (LIST 'MINUS E1)) SB)))))
            (AEVAL 'NIL)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND ((BOOLVALUE* *RATIONAL_BAK) (AEVAL (ON (LIST 'RATIONAL)))))
      (RETURN
       (COND ((EVALEQUAL (AEVAL SB) (AEVAL (LIST 'LIST))) (AEVAL 'NIL))
             (T (AEVAL SB)))))) 
(PUT 'DROP_CONST 'NUMBER-OF-ARGS 3) 
(FLAG '(DROP_CONST) 'OPFN) 
(PUT 'DROP_CONST 'DEFINED-ON-LINE '655) 
(PUT 'DROP_CONST 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'DROP_CONST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROP_CONST (OLDSOLN VARS ADDITIVE)
    (PROG (SOLN SL FNCN H1 H2 NEWFL VCOPY CONSTNT V FCOPY F1 CO MCDOLD)
      (SETQ SOLN (AEVAL (LIST 'LIST)))
      (SETQ MCDOLD (AEVAL *MCD))
      (AEVAL (OFF (LIST 'MCD)))
      (WHILE (EVALNEQ (AEVAL* OLDSOLN) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ SL (AEVAL* (LIST 'FIRST OLDSOLN)))
              (SETQ OLDSOLN (AEVAL* (LIST 'REST OLDSOLN)))
              (SETQ FNCN (AEVAL* (LIST 'SECOND SL)))
              (SETQ H1 (AEVAL* (LIST 'THIRD SL)))
              (SETQ NEWFL (AEVAL* (LIST 'LIST)))
              (PROG (H2)
                (SETQ H2 (GETRLIST (AEVAL* H1)))
               LAB
                (COND ((NULL H2) (RETURN NIL)))
                ((LAMBDA (H2)
                   (PROGN
                    (SETQ VCOPY (AEVAL* VARS))
                    (SETQ CONSTNT (AEVAL* 'T))
                    (WHILE
                     (AND (BOOLVALUE* CONSTNT)
                          (EVALNEQ (AEVAL* VCOPY) (AEVAL* (LIST 'LIST))))
                     (PROGN
                      (SETQ V (AEVAL* (LIST 'FIRST VCOPY)))
                      (SETQ VCOPY (AEVAL* (LIST 'REST VCOPY)))
                      (COND
                       ((NOT (BOOLVALUE* (REVALX (LIST 'MY_FREEOF CO V))))
                        (SETQ CONSTNT (AEVAL* 'NIL))))))
                    (COND
                     ((BOOLVALUE* CONSTNT)
                      (COND
                       ((OR
                         (NOT
                          (BOOLVALUE*
                           (REVALX (LIST 'MY_FREEOF (LIST 'FIRST SL) H2))))
                         (BOOLVALUE* (REVALX (LIST 'MY_FREEOF FNCN H2))))
                        (SETQ CONSTNT (AEVAL* 'NIL))))))
                    (COND
                     ((BOOLVALUE* CONSTNT)
                      (PROGN
                       (SETQ FCOPY (AEVAL* FNCN))
                       (WHILE
                        (AND (BOOLVALUE* CONSTNT)
                             (EVALNEQ (AEVAL* FCOPY) (AEVAL* (LIST 'LIST))))
                        (PROGN
                         (SETQ F1 (AEVAL* (LIST 'RHS (LIST 'FIRST FCOPY))))
                         (SETQ FCOPY (AEVAL* (LIST 'REST FCOPY)))
                         (AEVAL* (ON (LIST 'MCD)))
                         (SETQ CO (AEVAL* (LIST 'COEFFN F1 H2 1)))
                         (COND
                          ((OR
                            (NOT (BOOLVALUE* (REVALX (LIST 'MY_FREEOF CO H2))))
                            (AND (BOOLVALUE* ADDITIVE)
                                 (NOT
                                  (BOOLVALUE*
                                   (REVALX
                                    (LIST 'MY_FREEOF
                                          (LIST 'DIFFERENCE F1
                                                (LIST 'TIMES CO H2))
                                          H2)))))
                            (AND (NOT (BOOLVALUE* ADDITIVE))
                                 (EVALNEQ
                                  (AEVAL*
                                   (LIST 'DIFFERENCE F1 (LIST 'TIMES CO H2)))
                                  0)))
                           (SETQ CONSTNT (AEVAL* 'NIL))))
                         (AEVAL* (OFF (LIST 'MCD)))
                         (COND
                          ((AND (BOOLVALUE* CONSTNT) (BOOLVALUE* ADDITIVE))
                           (PROGN
                            (SETQ VCOPY (AEVAL* VARS))
                            (WHILE
                             (AND (BOOLVALUE* CONSTNT)
                                  (EVALNEQ (AEVAL* VCOPY)
                                           (AEVAL* (LIST 'LIST))))
                             (PROGN
                              (SETQ V (AEVAL* (LIST 'FIRST VCOPY)))
                              (SETQ VCOPY (AEVAL* (LIST 'REST VCOPY)))
                              (COND
                               ((NOT
                                 (BOOLVALUE* (REVALX (LIST 'MY_FREEOF CO V))))
                                (SETQ CONSTNT (AEVAL* 'NIL))))))))))))))
                    (COND
                     ((BOOLVALUE* CONSTNT)
                      (COND
                       ((BOOLVALUE* ADDITIVE)
                        (SETQ FNCN
                                (AEVAL* (LIST 'SUB (LIST 'EQUAL H2 0) FNCN))))
                       (T
                        (SETQ FNCN
                                (AEVAL*
                                 (LIST 'SUB (LIST 'EQUAL H2 1) FNCN))))))
                     (T (SETQ NEWFL (AEVAL* (LIST 'CONS H2 NEWFL)))))))
                 (CAR H2))
                (SETQ H2 (CDR H2))
                (GO LAB))
              (SETQ SOLN
                      (AEVAL*
                       (LIST 'CONS (LIST 'LIST (LIST 'FIRST SL) FNCN NEWFL)
                             SOLN)))))
      (COND ((BOOLVALUE* MCDOLD) (AEVAL (ON (LIST 'MCD)))))
      (RETURN (AEVAL SOLN)))) 
(PUT 'SOL_DEFINE1 'NUMBER-OF-ARGS 0) 
(FLAG '(SOL_DEFINE1) 'OPFN) 
(PUT 'SOL_DEFINE1 'DEFINED-ON-LINE '737) 
(PUT 'SOL_DEFINE1 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOL_DEFINE1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOL_DEFINE1 NIL (PROG (A B ASSI))) 
(PUT 'SOLUTION_CHECK1 'NUMBER-OF-ARGS 1) 
(PUT 'SOLUTION_CHECK1 'DEFINED-ON-LINE '780) 
(PUT 'SOLUTION_CHECK1 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOLUTION_CHECK1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLUTION_CHECK1 (ARGLIST)
    (PROG (PDES FORG A FSUB SOLU L1 L2 BATCH_BAK SESSION_BAK PROC_LIST_BAK
           PRINT_BAK OLD_HISTORY_BAK BATCHCOUNT_BAK STEPCOUNTER_BAK
           REPEAT_MODE_BAK FREEF)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (SETQ BATCH_BAK *BATCH_MODE)
      (SETQ *BATCH_MODE BATCH_MODE_SUB)
      (BACKUP_TO_FILE PDES FORG NIL)
      (SETQ SOLU (AEVAL (LIST 'SOL_DEFINE1)))
      (SETQ FREEF (CDR (REVAL1 (CADDR SOLU) T)))
      (SETQ SOLU (CADR SOLU))
      (COND
       ((NULL SOLU)
        (RETURN
         (PROGN
          (SETQ *BATCH_MODE BATCH_BAK)
          (SETQ PRINT_ PRINT_BAK)
          (PROGN
           (PRIN2 "##### Before calling this module a solution must be read")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "##### into the procedure sol_define1. Please have a look")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "##### at this procedure in file crsimpso.red .") NIL)
          (TERPRI)
          NIL))))
      (SETQ FORG (SUB_FSUB_IN_ITSELF_AND_IN_FORG FORG))
      (PROG (A)
        (SETQ A FORG)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((AND (PAIRP A) (EQUAL (CAR A) 'EQUAL))
             (SETQ FSUB
                     (CONS (LIST 'EQUAL (CADR A) (LIST '*SQ (CADDR A) T))
                           FSUB)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (COND (FSUB (SETQ SOLU (AEVAL (LIST 'SUB (CONS 'LIST FSUB) SOLU)))))
      (SETQ PRINT_BAK PRINT_)
      (COND ((NULL *BATCH_MODE) (SETQ PRINT_ 8)) (T (SETQ PRINT_ NIL)))
      (SETQ SESSION_BAK SESSION_)
      (SETQ SESSION_ NIL)
      (START_LEVEL 1 (LIST "Check whether a solution is contained."))
      (COND
       (PRINT_
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "CRACK is now called to check whether a given") NIL)
         (TERPRI)
         (PROGN (PRIN2 "solution is included in the current system.") NIL))))
      (SETQ RECYCLE_FCTS NIL)
      (SETQ SOLU
              (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ L1 (CDR (REVAL1 SOLU NIL)))
                (COND ((NULL L1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (L1)
                                    (COND
                                     ((AND (PAIRP L1) (EQUAL (CAR L1) '*SQ))
                                      (CADR L1))
                                     (T (SIMP L1))))
                                  (CAR L1))
                                 NIL)))
               LOOPLABEL
                (SETQ L1 (CDR L1))
                (COND ((NULL L1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (L1)
                            (COND
                             ((AND (PAIRP L1) (EQUAL (CAR L1) '*SQ)) (CADR L1))
                             (T (SIMP L1))))
                          (CAR L1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FTEM_ (SETDIFF FTEM_ FREEF))
      (SETQ A FTEM_)
      (PROG (L1)
        (SETQ L1 FORG)
       LAB
        (COND ((NULL L1) (RETURN NIL)))
        ((LAMBDA (L1)
           (COND
            ((NOT (ATOM L1))
             (PROG (L2)
               (SETQ L2 (GET (CADR L1) 'FCTS))
              LAB
               (COND ((NULL L2) (RETURN NIL)))
               ((LAMBDA (L2)
                  (COND
                   ((AND (FREEOF A L2) (FREEOF FREEF L2))
                    (SETQ A (FCTINSERT L2 A)))
                   (T NIL)))
                (CAR L2))
               (SETQ L2 (CDR L2))
               (GO LAB)))
            ((AND (FREEOF A L1) (FREEOF FREEF L1)) (SETQ A (FCTINSERT L1 A)))))
         (CAR L1))
        (SETQ L1 (CDR L1))
        (GO LAB))
      (COND
       (FREEF
        (PROGN
         (SETQ A (SETDIFF A FREEF))
         (PROG (L1)
           (SETQ L1 PDES)
          LAB
           (COND ((NULL L1) (RETURN NIL)))
           ((LAMBDA (L1) (SETQ SOLU (CONS (GET L1 'SQVAL) SOLU))) (CAR L1))
           (SETQ L1 (CDR L1))
           (GO LAB))
         (SETQ PDES NIL)
         (SETQ PDES (MKEQSQLIST SOLU NIL NIL A VL_ ALLFLAGS_ T (LIST 0) PDES))
         NIL))
       (T
        (SETQ PDES
                (APPEND
                 (MKEQSQLIST SOLU NIL NIL A VL_ ALLFLAGS_ T (LIST 0) PDES)
                 PDES))))
      (SETQ PRINT_MORE NIL)
      (SETQ PROC_LIST_ (DELETE 'SOLUTION_CHECK1 PROC_LIST_))
      (SETQ PROC_LIST_BAK PROC_LIST_)
      (SETQ PROC_LIST_ DEFAULT_PROC_LIST_)
      (COND (CONTRADICTION_ (PROGN (SETQ LEVEL_ (CDR LEVEL_)) (SETQ L1 NIL)))
            (T
             (PROGN
              (SETQ OLD_HISTORY_BAK OLD_HISTORY)
              (SETQ OLD_HISTORY NIL)
              (SETQ BATCHCOUNT_BAK BATCHCOUNT_)
              (SETQ BATCHCOUNT_ (MINUS 1))
              (SETQ STEPCOUNTER_BAK STEPCOUNTER_)
              (SETQ STEPCOUNTER_ 0)
              (SETQ REPEAT_MODE_BAK REPEAT_MODE)
              (SETQ REPEAT_MODE NIL)
              (SETQ L1 (CRACKMAIN PDES FORG))
              (SETQ OLD_HISTORY OLD_HISTORY_BAK)
              (SETQ BATCHCOUNT_ BATCHCOUNT_BAK)
              (SETQ STEPCOUNTER_ STEPCOUNTER_BAK)
              (SETQ REPEAT_MODE REPEAT_MODE_BAK))))
      (SETQ SESSION_ SESSION_BAK)
      (SETQ PROC_LIST_ PROC_LIST_BAK)
      (SETQ PRINT_ PRINT_BAK)
      (COND
       ((AND L1 (NOT CONTRADICTION_))
        (PROGN (PRIN2 "+++++ Solution IS included.") NIL))
       (T (PROGN (PRIN2 "+++++ Solution is NOT included.") NIL)))
      (TERPRI)
      (SETQ CONTRADICTION_ NIL)
      (SETQ SESSION_ SESSION_BAK)
      (SETQ L1 (RESTORE_BACKUP_FROM_FILE PDES FORG NIL))
      (DELETE_BACKUP)
      (SETQ *BATCH_MODE BATCH_BAK))) 
(PUT 'SOL_DEFINE2 'NUMBER-OF-ARGS 0) 
(FLAG '(SOL_DEFINE2) 'OPFN) 
(PUT 'SOL_DEFINE2 'DEFINED-ON-LINE '892) 
(PUT 'SOL_DEFINE2 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOL_DEFINE2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOL_DEFINE2 NIL (PROG ())) 
(PUT 'SOLUTION_CHECK2 'NUMBER-OF-ARGS 1) 
(PUT 'SOLUTION_CHECK2 'DEFINED-ON-LINE '913) 
(PUT 'SOLUTION_CHECK2 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOLUTION_CHECK2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLUTION_CHECK2 (ARGLIST)
    (PROG (G H K P INEQ_CP HSUB)
      (SETQ G (CADR ARGLIST))
      (SETQ HSUB (AEVAL (LIST 'SOL_DEFINE2)))
      (COND
       ((NULL HSUB)
        (RETURN
         (PROGN
          (PROGN
           (PRIN2 "##### Before calling this module a solution must be read")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "##### into the procedure sol_define2. Please have a look")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "##### at this procedure in file crsimpso.red .") NIL)
          (TERPRI)
          NIL))))
      (SETQ INEQ_CP INEQ_)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND INEQ_CP
                (NOT
                 (ZEROP
                  (SETQ H
                          (AEVAL*
                           (LIST 'SUB HSUB (LIST '*SQ (CAR INEQ_CP) T))))))))
          (RETURN NIL)))
        (SETQ INEQ_CP (CDR INEQ_CP))
        (GO WHILELABEL))
      (COND
       ((NULL INEQ_CP)
        (PROGN
         (SETQ INEQ_CP INEQ_OR)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND INEQ_CP
                   (PROGN
                    (SETQ G (CAR INEQ_CP))
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND G
                              (ZEROP
                               (SETQ H
                                       (AEVAL*
                                        (LIST 'SUB HSUB
                                              (PROGN
                                               (SETQ P (CAAR G))
                                               (PROG (K)
                                                 (SETQ K (CDAR G))
                                                LAB
                                                 (COND ((NULL K) (RETURN NIL)))
                                                 ((LAMBDA (K)
                                                    (SETQ P (MULTSQ P K)))
                                                  (CAR K))
                                                 (SETQ K (CDR K))
                                                 (GO LAB))
                                               (LIST '*SQ P T))))))))
                        (RETURN NIL)))
                      (SETQ G (CDR G))
                      (GO WHILELABEL))
                    G)))
             (RETURN NIL)))
           (SETQ INEQ_CP (CDR INEQ_CP))
           (GO WHILELABEL))
         NIL)))
      (COND
       ((NULL INEQ_CP)
        (PROGN
         (PRIN2 ".......... No inequality was violated. ............")
         NIL))
       (T
        (PROGN
         (PROGN
          (PRIN2 "########## At least one inequality was violated: ")
          NIL)
         (MATHPRINT
          (CONS 'LIST
                (PROG (G FORALL-RESULT FORALL-ENDPTR)
                  (SETQ G (CAR INEQ_CP))
                  (COND ((NULL G) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (G)
                                      (CONS 'LIST
                                            (PROG (K FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ K G)
                                              (COND ((NULL K) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (K)
                                                                  (LIST '*SQ K
                                                                        T))
                                                                (CAR K))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ K (CDR K))
                                              (COND
                                               ((NULL K)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (LIST '*SQ K T))
                                                        (CAR K))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL))))
                                    (CAR G))
                                   NIL)))
                 LOOPLABEL
                  (SETQ G (CDR G))
                  (COND ((NULL G) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (G)
                              (CONS 'LIST
                                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ K G)
                                      (COND ((NULL K) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (LIST '*SQ K T))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K) (LIST '*SQ K T))
                                                (CAR K))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL))))
                            (CAR G))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (TERPRI))) 
(PUT 'SOL_DEFINE3 'NUMBER-OF-ARGS 0) 
(FLAG '(SOL_DEFINE3) 'OPFN) 
(PUT 'SOL_DEFINE3 'DEFINED-ON-LINE '965) 
(PUT 'SOL_DEFINE3 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOL_DEFINE3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOL_DEFINE3 NIL (PROG (A B C H))) 
(PUT 'SOLUTION_CHECK3 'NUMBER-OF-ARGS 1) 
(PUT 'SOLUTION_CHECK3 'DEFINED-ON-LINE '996) 
(PUT 'SOLUTION_CHECK3 'DEFINED-IN-FILE 'CRACK/CRSIMPSO.RED) 
(PUT 'SOLUTION_CHECK3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLUTION_CHECK3 (ARGLIST)
    (PROG (SOLU A B P F H CPA)
      (SETQ SOLU (AEVAL (LIST 'SOL_DEFINE3)))
      (COND
       ((NULL SOLU)
        (RETURN
         (PROGN
          (PROGN
           (PRIN2 "##### Before calling this module a solution must be read")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2 "##### into the procedure sol_define3. Please have a look")
           NIL)
          (TERPRI)
          (PROGN (PRIN2 "##### at this procedure in file crsimpso.red .") NIL)
          (TERPRI)
          NIL))))
      (SETQ A (CADDR SOLU))
      (SETQ B (CDADR SOLU))
      (SETQ SOLU NIL)
      (PROG (P)
        (SETQ P (CAR ARGLIST))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ H (AEVAL (LIST 'SUB A (LIST '*SQ (GET P 'SQVAL) T))))
            (COND
             ((OR (NULL H) (NOT (ZEROP H)))
              (PROGN
               (PROGN
                (PRIN2 "***** Equation ")
                (PRIN2 P)
                (PRIN2 " is violated! *****")
                NIL)
               (TERPRI)
               (PROGN
                (ASSGNPRI (AEVAL "0 = ") NIL 'FIRST)
                (ASSGNPRI (AEVAL H) NIL 'LAST)))))
            NIL))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (F)
        (SETQ F (CADR ARGLIST))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((PAIRP F)
             (PROGN
              (COND
               ((EQUAL (CDR (CADDR F)) 1)
                (SETQ H
                        (AEVAL
                         (LIST 'SUB A
                               (LIST '*SQ
                                     (ADDSQ (CADDR F) (NEGSQ (SIMP (CADR F))))
                                     T)))))
               (T
                (PROGN
                 (SETQ H
                         (LIST '*SQ (ADDSQ (CADDR F) (NEGSQ (SIMP (CADR F))))
                               T))
                 (SETQ CPA (CDR A))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND CPA
                           (SETQ H
                                   (ERR_CATCH_SUB (CADAR CPA) (CADDAR CPA)
                                    H))))
                     (RETURN NIL)))
                   (SETQ CPA (CDR CPA))
                   (GO WHILELABEL))
                 NIL)))
              (COND
               ((OR (NULL H) (NOT (ZEROP H)))
                (PROGN
                 (PROGN
                  (PRIN2 "***** Function ")
                  (PRIN2 (CADR F))
                  (PRIN2 " value is contradicted! *****")
                  NIL)
                 (TERPRI))))
              NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB)))) 
(ENDMODULE) 