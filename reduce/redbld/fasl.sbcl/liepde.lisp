(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(CREATE-PACKAGE '(LIEPDE) NIL) 
(FLUID
 '(PRINT_ LOGOPRINT_ ADJUST_FNC PROC_LIST_ PRELIM_ INDIVIDUAL_ PROLONG_ORDER
   *BATCH_MODE COLLECT_SOL FLIN_ LIN_PROBLEM DONE_TRAFO ETAMN_AL MAX_GC_FAC
   BATCH_MODE_SUB INVERSE_TRAFO_LIST_INCOMPLETE)) 
(SETQ *BATCH_MODE T) 
(SETQ PRELIM_ NIL) 
(SETQ INDIVIDUAL_ NIL) 
(SETQ PROLONG_ORDER 0) 
(SETQ ETAMN_AL NIL) 
(PUT 'EQU_TO_EXPR 'NUMBER-OF-ARGS 1) 
(FLAG '(EQU_TO_EXPR) 'OPFN) 
(PUT 'EQU_TO_EXPR 'DEFINED-ON-LINE '50) 
(PUT 'EQU_TO_EXPR 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'EQU_TO_EXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQU_TO_EXPR (A)
    (PROG (LDE)
      (RETURN
       (COND ((EVALEQUAL (AEVAL A) (AEVAL 'NIL)) (AEVAL A))
             (T
              (PROGN
               (SETQ LDE (REVAL1 (AEVAL A) T))
               (COND ((BOOLVALUE* (REVALX (ATOM LDE))) (AEVAL A))
                     (T
                      (AEVAL
                       (LIST 'NUM
                             (COND
                              ((BOOLVALUE* (REVALX (EQUAL (CAR LDE) 'EQUAL)))
                               (AEVAL
                                (LIST 'DIFFERENCE (LIST 'LHS A)
                                      (LIST 'RHS A))))
                              (T (AEVAL A))))))))))))) 
(MODULE (LIST 'PDESYMMETRY)) 
(PUT 'COMPAREDIF3 'NUMBER-OF-ARGS 3) 
(PUT 'COMPAREDIF3 'DEFINED-ON-LINE '78) 
(PUT 'COMPAREDIF3 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'COMPAREDIF3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPAREDIF3 (DU1 U2 U2LIST)
    (PROG (U1L)
      (SETQ U1L (COMBIDIF DU1))
      (COND ((NEQ (CAR U1L) U2) (RETURN NIL))
            (T (RETURN (COMPAREDIF1 (CDR U1L) U2LIST)))))) 
(PUT 'MERGEDEPLI 'NUMBER-OF-ARGS 2) 
(PUT 'MERGEDEPLI 'DEFINED-ON-LINE '96) 
(PUT 'MERGEDEPLI 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'MERGEDEPLI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MERGEDEPLI (LI1 LI2)
    (PROG (NEWDEP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND LI1 LI2)) (RETURN NIL)))
        (PROGN
         (SETQ NEWDEP (CONS (UNION (CAR LI1) (CAR LI2)) NEWDEP))
         (SETQ LI1 (CDR LI1))
         (SETQ LI2 (CDR LI2)))
        (GO WHILELABEL))
      (RETURN
       (COND (LI1 (NCONC (REVERSIP NEWDEP) LI1))
             (LI2 (NCONC (REVERSIP NEWDEP) LI2)) (T (REVERSIP NEWDEP)))))) 
(PUT 'ADDDEPLI 'NUMBER-OF-ARGS 2) 
(PUT 'ADDDEPLI 'DEFINED-ON-LINE '110) 
(PUT 'ADDDEPLI 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'ADDDEPLI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDDEPLI (EX REVDYLIST)
    (PROG (A B C D)
      (PROG (A)
        (SETQ A REVDYLIST)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ C NIL)
            (PROG (B)
              (SETQ B A)
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (COND ((NOT (MY_FREEOF EX B)) (SETQ C (CONS B C)))))
               (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (COND ((OR C D) (SETQ D (CONS C D))))
            NIL))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (RETURN (LIST EX D)))) 
(PUT 'ADD_XI_ETA_DEPLI 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_XI_ETA_DEPLI 'DEFINED-ON-LINE '123) 
(PUT 'ADD_XI_ETA_DEPLI 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'ADD_XI_ETA_DEPLI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_XI_ETA_DEPLI (XILIST ETALIST REVDYLIST)
    (PROG (E1 G H)
      (PROG (E1)
        (SETQ E1 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH XILIST) E1)) (RETURN NIL)))
        (PROGN
         (SETQ G (NTH XILIST E1))
         (SETQ H (PNTH G 4))
         (RPLACA H (CADR (ADDDEPLI (CAR G) REVDYLIST))))
        (SETQ E1 (PLUS2 E1 1))
        (GO LAB))
      (PROG (E1)
        (SETQ E1 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH ETALIST) E1)) (RETURN NIL)))
        (PROGN
         (SETQ G (NTH ETALIST E1))
         (SETQ H (PNTH G 3))
         (RPLACA H (CADR (ADDDEPLI (CAR G) REVDYLIST))))
        (SETQ E1 (PLUS2 E1 1))
        (GO LAB)))) 
(PUT 'SUBTEST 'NUMBER-OF-ARGS 5) 
(PUT 'SUBTEST 'DEFINED-ON-LINE '140) 
(PUT 'SUBTEST 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'SUBTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBTEST (UIK SB XLIST ORDOK SUBORDINC)
    (PROG (EL5 EL6 EL7 EL8 EL9 EL10 SBC)
      (SETQ EL5 (COMBIDIF UIK))
      (SETQ EL6 (CAR EL5))
      (SETQ EL5 (CDR EL5))
      (SETQ EL7 NIL)
      (SETQ EL8 100)
      (SETQ EL9 NIL)
      (SETQ SBC SB)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND SBC
                (OR (NEQ (CAAAR SBC) EL6)
                    (NEQ 0
                         (PROGN
                          (SETQ EL7 (COMPAREDIF1 (CDAAR SBC) EL5))
                          (COND
                           ((AND EL7 (NOT (ZEROP EL7))
                                 (LESSP (LENGTH EL7) EL8))
                            (PROGN
                             (SETQ EL8 (LENGTH EL7))
                             (SETQ EL9 EL7)
                             (SETQ EL10 (CAR SBC))))
                           (T EL7)))))))
          (RETURN NIL)))
        (SETQ SBC (CDR SBC))
        (GO WHILELABEL))
      (RETURN
       (COND (SBC (CONS (SIMP* (CADAR SBC)) (CADDAR SBC)))
             (EL9
              (PROGN
               (SETQ UIK (CONS (SIMP* (CADR EL10)) (CADDR EL10)))
               (PROG ()
                WHILELABEL
                 (COND ((NOT EL9) (RETURN NIL)))
                 (PROGN
                  (SETQ UIK
                          (TOTDF3 (CAR UIK) (CDR UIK) (NTH XLIST (CAR EL9))
                           (CAR EL9) SB XLIST ORDOK SUBORDINC))
                  (SETQ EL9 (CDR EL9)))
                 (GO WHILELABEL))
               UIK))
             (T NIL))))) 
(PUT 'TOTDF3 'NUMBER-OF-ARGS 8) 
(PUT 'TOTDF3 'DEFINED-ON-LINE '178) 
(PUT 'TOTDF3 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'TOTDF3 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TOTDF3 (S DEPLI X N SB XLIST ORDOK SUBORDINC)
    (PROG (TDF EL1 EL2 EL3 EL4 EL5 NEWDEPLI NEWDY DY DDY)
      (SETQ NEWDEPLI NIL)
      (SETQ NEWDY NIL)
      (SETQ DDY NIL)
      (COND
       ((GREATERP ORDOK 0)
        (PROGN
         (SETQ TDF (SIMP* 0))
         (SETQ DEPLI (COPY DEPLI))
         (SETQ EL2 (LENGTH DEPLI))
         (COND ((LESSP EL2 (DIFFERENCE ORDOK SUBORDINC)) (SETQ DEPLI NIL))
               (T
                (PROG (EL1)
                  (SETQ EL1 1)
                 LAB
                  (COND
                   ((MINUSP
                     (DIFFERENCE (DIFFERENCE (DIFFERENCE ORDOK 1) SUBORDINC)
                                 EL1))
                    (RETURN NIL)))
                  (PROGN (SETQ DY (PNTH DEPLI EL1)) (RPLACA DY NIL) NIL)
                  (SETQ EL1 (PLUS2 EL1 1))
                  (GO LAB))))))
       (T (SETQ TDF (DIFFSQ S X))))
      (PROG (EL1)
        (SETQ EL1 DEPLI)
       LAB
        (COND ((NULL EL1) (RETURN NIL)))
        ((LAMBDA (EL1)
           (PROGN
            (SETQ DY (UNION DDY EL1))
            (SETQ DDY NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT EL1) (RETURN NIL)))
              (PROGN
               (SETQ EL2 (CAR EL1))
               (SETQ EL1 (CDR EL1))
               (SETQ EL3 (DIFFSQ S EL2))
               (COND ((NULL (CAR EL3)) NIL)
                     (T
                      (PROGN
                       (SETQ EL4 (DIF EL2 N))
                       (COND
                        ((SETQ EL5 (SUBTEST EL4 SB XLIST ORDOK SUBORDINC))
                         (PROGN
                          (SETQ EL4 (CAR EL5))
                          (SETQ NEWDEPLI (MERGEDEPLI NEWDEPLI (CDR EL5)))))
                        (T
                         (PROGN
                          (SETQ DDY (CONS EL4 DDY))
                          (SETQ EL4 (SIMP* EL4)))))
                       (SETQ TDF (ADDSQ TDF (MULTSQ EL4 EL3)))))))
              (GO WHILELABEL))
            (SETQ NEWDY (CONS DY NEWDY))))
         (CAR EL1))
        (SETQ EL1 (CDR EL1))
        (GO LAB))
      (COND (DDY (SETQ NEWDY (CONS DDY NEWDY))))
      (SETQ NEWDEPLI (MERGEDEPLI (REVERSIP NEWDY) NEWDEPLI))
      (RETURN (CONS TDF NEWDEPLI)))) 
(PUT 'JOINSUBLISTS 'NUMBER-OF-ARGS 1) 
(PUT 'JOINSUBLISTS 'DEFINED-ON-LINE '253) 
(PUT 'JOINSUBLISTS 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'JOINSUBLISTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE JOINSUBLISTS (A)
    (COND ((NULL A) NIL) (T (APPEND (CAR A) (JOINSUBLISTS (CDR A)))))) 
(PUT 'TRANSEQ 'NUMBER-OF-ARGS 4) 
(FLAG '(TRANSEQ) 'OPFN) 
(PUT 'TRANSEQ 'DEFINED-ON-LINE '261) 
(PUT 'TRANSEQ 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'TRANSEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANSEQ (EQN XLIST YLIST SB)
    (PROGN
     (PROG (EL1)
       (SETQ EL1 (GETRLIST (AEVAL SB)))
      LAB
       (COND ((NULL EL1) (RETURN NIL)))
       ((LAMBDA (EL1) (SETQ EQN (AEVAL (LIST 'SUB EL1 EQN)))) (CAR EL1))
       (SETQ EL1 (CDR EL1))
       (GO LAB))
     (PROG (EL1)
       (SETQ EL1 (GETRLIST (AEVAL YLIST)))
      LAB
       (COND ((NULL EL1) (RETURN NIL)))
       ((LAMBDA (EL1)
          (PROG (EL2)
            (SETQ EL2 (GETRLIST (AEVAL XLIST)))
           LAB
            (COND ((NULL EL2) (RETURN NIL)))
            ((LAMBDA (EL2) (AEVAL (NODEPEND (LIST EL1 EL2)))) (CAR EL2))
            (SETQ EL2 (CDR EL2))
            (GO LAB)))
        (CAR EL1))
       (SETQ EL1 (CDR EL1))
       (GO LAB))
     (AEVAL EQN))) 
(FLAG '(DROP) 'OPFN) 
(PUT 'DROP 'NUMBER-OF-ARGS 2) 
(PUT 'DROP 'DEFINED-ON-LINE '270) 
(PUT 'DROP 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'DROP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DROP (A VL)
    (PROG (B)
      (COND ((NOT (AND (PAIRP A) (EQUAL (CAR A) 'PLUS))) (SETQ B A))
            (T
             (PROGN
              (SETQ VL (CDR VL))
              (PROG (C)
                (SETQ C (CDR A))
               LAB
                (COND ((NULL C) (RETURN NIL)))
                ((LAMBDA (C)
                   (COND ((NOT (FREEOFLIST C VL)) (SETQ B (CONS C B)))))
                 (CAR C))
                (SETQ C (CDR C))
                (GO LAB))
              (COND (B (SETQ B (CONS 'PLUS (REVERSE B))))))))
      (RETURN B))) 
(PUT 'ETAMN 'NUMBER-OF-ARGS 8) 
(PUT 'ETAMN 'DEFINED-ON-LINE '284) 
(PUT 'ETAMN 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'ETAMN 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE ETAMN (U INDXLIST XILIST ETALIST ORDOK TRUESUB SUBORDINC XLIST)
    (PROG (ETAM X H1 H2 H3 H4 H5 ULIST EL R CPLIST DEPLI)
      (COND
       ((EQUAL ORDOK 0)
        (PROGN
         (SETQ H5 U)
         (SETQ H2 INDXLIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT H2) (RETURN NIL)))
           (PROGN (SETQ H5 (MKID H5 (CAR H2))) (SETQ H2 (CDR H2)))
           (GO WHILELABEL))
         (SETQ H3 (ASSOC H5 ETAMN_AL)))))
      (COND (H3 (RETURN (CDR H3))))
      (COND
       ((OR (NULL INDXLIST) (EQUAL (LENGTH INDXLIST) 1))
        (PROGN
         (SETQ CPLIST ETALIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ U (CADAR CPLIST))) (RETURN NIL)))
           (SETQ CPLIST (CDR CPLIST))
           (GO WHILELABEL))
         (SETQ ETAM (CONS (CONS (CAAR CPLIST) (CADDAR CPLIST)) NIL))
         NIL))
       (T
        (SETQ ETAM
                (ETAMN U (CDR INDXLIST) XILIST ETALIST ORDOK TRUESUB SUBORDINC
                 XLIST))))
      (RETURN
       (COND ((NULL INDXLIST) ETAM)
             (T
              (PROGN
               (SETQ ULIST NIL)
               (SETQ X (CDR (NTH XILIST (CAR INDXLIST))))
               (SETQ R
                       (COND
                        ((NULL (CAR (CAAR ETAM)))
                         (PROGN (SETQ DEPLI NIL) (CAAR ETAM)))
                        (T
                         (PROGN
                          (SETQ H2
                                  (TOTDF3 (CAAR ETAM) (CDAR ETAM) (CAR X)
                                   (CADR X) TRUESUB XLIST ORDOK SUBORDINC))
                          (SETQ DEPLI (CDR H2))
                          (CAR H2)))))
               (SETQ ETAM (CDR ETAM))
               (SETQ CPLIST XILIST)
               (SETQ H3 NIL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT CPLIST) (RETURN NIL)))
                 (PROGN
                  (SETQ EL (CAR CPLIST))
                  (SETQ CPLIST (CDR CPLIST))
                  (COND
                   ((EQUAL (LENGTH INDXLIST) 1) (SETQ H1 (DIF U (CADDR EL))))
                   (T
                    (PROGN
                     (SETQ H1 (DIF (CAR ETAM) (CADR INDXLIST)))
                     (SETQ ETAM (CDR ETAM))
                     NIL)))
                  (SETQ ULIST (CONS H1 ULIST))
                  (COND
                   ((NOT (SQZEROP (CAR EL)))
                    (PROGN
                     (COND
                      ((SETQ H4 (SUBTEST H1 TRUESUB XLIST ORDOK SUBORDINC))
                       (PROGN
                        (SETQ H1 (CAR H4))
                        (SETQ DEPLI (MERGEDEPLI DEPLI (CDR H4)))))
                      (T (SETQ H1 (SIMP* H1))))
                     (SETQ R
                             (ADDSQ R
                                    (NEGSQ
                                     (MULTSQ H1
                                             (PROGN
                                              (SETQ H2
                                                      (TOTDF3 (CAR EL)
                                                       (CADDDR EL) (CAR X)
                                                       (CADR X) TRUESUB XLIST 0
                                                       0))
                                              (COND
                                               ((CAR (CAR H2))
                                                (PROGN
                                                 (COND
                                                  (H4
                                                   (SETQ DEPLI
                                                           (MERGEDEPLI DEPLI
                                                            (CDR H4))))
                                                  (T
                                                   (SETQ H3
                                                           (CONS (PREPSQ H1)
                                                                 H3))))
                                                 (SETQ DEPLI
                                                         (MERGEDEPLI DEPLI
                                                          (CDR H2)))
                                                 NIL)))
                                              (CAR H2))))))
                     NIL))))
                 (GO WHILELABEL))
               (COND
                (H3
                 (PROGN
                  (SETQ H3 (LIST H3))
                  (PROG (H2)
                    (SETQ H2 1)
                   LAB
                    (COND
                     ((MINUSP (DIFFERENCE (LENGTH INDXLIST) H2)) (RETURN NIL)))
                    (SETQ H3 (CONS NIL H3))
                    (SETQ H2 (PLUS2 H2 1))
                    (GO LAB))
                  (SETQ DEPLI (MERGEDEPLI DEPLI H3))
                  NIL)))
               (SETQ H1 (CONS (CONS R DEPLI) (REVERSE ULIST)))
               (COND
                ((EQUAL ORDOK 0) (SETQ ETAMN_AL (CONS (CONS H5 H1) ETAMN_AL))))
               H1)))))) 
(PUT 'CALLCRACK 'NUMBER-OF-ARGS 11) 
(PUT 'CALLCRACK 'DEFINED-ON-LINE '369) 
(PUT 'CALLCRACK 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'CALLCRACK 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE CALLCRACK
    (*TIME CPU GC LIETRACE_ SYMCON FLIST VL XILIST ETALIST INEQU LAST_CALL)
    (PROG (G H OLDBATCH_MODE PRINT_OLD)
      (COND
       (*TIME
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "time to formulate conditions: ")
          (PRIN2 (DIFFERENCE (TIME) CPU))
          (PRIN2 " ms    GC time : ")
          (PRIN2 (DIFFERENCE (GCTIME) GC))
          (PRIN2 " ms")
          NIL)
         NIL)))
      (COND
       (LIETRACE_
        (PROGN
         (ASSGNPRI (AEVAL "Symmetry conditions before CRACK: ") NIL 'ONLY)
         (ASSGNPRI (AEVAL (CONS 'LIST SYMCON)) NIL 'ONLY)
         (AEVAL 'NIL))))
      (SETQ OLDBATCH_MODE *BATCH_MODE)
      (SETQ *BATCH_MODE BATCH_MODE_SUB)
      (SETQ PRINT_OLD PRINT_)
      (COND ((AND (NULL BATCH_MODE_SUB) (NULL PRINT_)) (SETQ PRINT_ 8)))
      (COND
       ((FREEOF PROC_LIST_ 'TRY_OTHER_ORDERING)
        (SETQ PROC_LIST_ (APPEND PROC_LIST_ (LIST 'TRY_OTHER_ORDERING)))))
      (SETQ H
              (SQ*CRACK
               (LIST (CONS 'LIST SYMCON) (CONS 'LIST INEQU) (CONS 'LIST FLIST)
                     (CONS 'LIST VL))))
      (SETQ *BATCH_MODE OLDBATCH_MODE)
      (SETQ PRINT_ PRINT_OLD)
      (COND (LAST_CALL (RETURN H)))
      (COND
       ((NEQ H (LIST 'LIST))
        (PROGN
         (SETQ H (CADR H))
         (SETQ SYMCON (CDADR H))
         (PROG (G)
           (SETQ G (CDADDR H))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (PROGN
               (SETQ XILIST
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K XILIST)
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K)
                                             (CONS
                                              (SUBSQ (CAR K)
                                                     (LIST
                                                      (CONS (CADR G)
                                                            (CADDR G))))
                                              (CDR K)))
                                           (CAR K))
                                          NIL)))
                        LOOPLABEL
                         (SETQ K (CDR K))
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K)
                                     (CONS
                                      (SUBSQ (CAR K)
                                             (LIST (CONS (CADR G) (CADDR G))))
                                      (CDR K)))
                                   (CAR K))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ ETALIST
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K ETALIST)
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K)
                                             (CONS
                                              (SUBSQ (CAR K)
                                                     (LIST
                                                      (CONS (CADR G)
                                                            (CADDR G))))
                                              (CDR K)))
                                           (CAR K))
                                          NIL)))
                        LOOPLABEL
                         (SETQ K (CDR K))
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K)
                                     (CONS
                                      (SUBSQ (CAR K)
                                             (LIST (CONS (CADR G) (CADDR G))))
                                      (CDR K)))
                                   (CAR K))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ INEQU (SUBST (CADDR G) (CADR G) INEQU))
               NIL))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (COND
          (LIETRACE_
           (PROGN
            (PROGN (PRIN2 "symcon nachher: ") (PRIN2 SYMCON) NIL)
            (PROGN (PRIN2 "xilist=") (PRIN2 XILIST) NIL)
            (PROGN (PRIN2 "etalist=") (PRIN2 ETALIST) NIL)
            NIL)))
         (SETQ FLIST (CDR (REVAL1 (CADDDR H) T)))
         (COND
          (PRINT_
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2 "Remaining free functions after the last CRACK-run:")
             NIL)
            (TERPRI)
            (FCTPRINT FLIST)
            (TERPRI)
            (TERPRI))))
         NIL)))
      (RETURN (LIST SYMCON XILIST ETALIST FLIST INEQU)))) 
(PUT 'LIEPDE 'PSOPFN 'LIEPDE_PSOPFN) 
(PUT 'LIEPDE_PSOPFN 'NUMBER-OF-ARGS 1) 
(PUT 'LIEPDE_PSOPFN 'DEFINED-ON-LINE '436) 
(PUT 'LIEPDE_PSOPFN 'DEFINED-IN-FILE 'CRACK/LIEPDE.RED) 
(PUT 'LIEPDE_PSOPFN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIEPDE_PSOPFN (INP)
    (PROG (CPU GC LIETRACE_ OLDADJ EQLIST YLIST XLIST POINTP CONTACTP GENERALP
           ANSATZP SYMORD E1 E2 ORDR SB DYLIST REVDYLIST XI ETA EQORDR
           EQORDRCOP NO EQCOPY1 TRUESUB DEPLIST XILIST ETALIST DYCOPY FREELIST
           EQLEN DYLEN TRUESUBNO MINORDR N1 N2 N3 N4 N5 N H JETORD ALLSUB SUBDY
           LHSLIST SYMCON SUBORDINC EQN DEPLI VL OCCLI REVDYCOPY SUBORDINCLIST
           XICOP ETACOP FLCOP ETAPQLIST ETAPQCOP ETAPQ OLDBATCH_MODE ALLSYM
           SYMCON_S XILIST_S ETALIST_S INEQU_S FLIST_S TRUESUB_S OLDCOLLECT_SOL
           OLDPRINT_ FLIST_SLIN FLIST_SNLI RETURN_LIST LAST_CALL PARALIST
           PROC_LIST_BAK MAX_GC_FAC_BAK FLISTORG PROBLEM SYMTYPE FLIST INEQU)
      (BACKUP_REDUCE_FLAGS)
      (SETQ CPU (TIME))
      (SETQ GC (GCTIME))
      (SETQ OLDADJ ADJUST_FNC)
      (SETQ ADJUST_FNC NIL)
      (SETQ OLDCOLLECT_SOL COLLECT_SOL)
      (SETQ COLLECT_SOL T)
      (SETQ PROBLEM (REVAL1 (CAR INP) NIL))
      (SETQ SYMTYPE (REVAL1 (CADR INP) T))
      (SETQ FLIST (REVAL1 (CADDR INP) T))
      (SETQ INEQU (REVAL1 (CADDDR INP) T))
      (SETQ EQLIST
              (COND ((ATOM (CADR PROBLEM)) (LIST (CADR PROBLEM)))
                    ((EQUAL (CAR (CADR PROBLEM)) 'LIST) (CDR (CADR PROBLEM)))
                    (T (LIST (CADR PROBLEM)))))
      (SETQ YLIST (REVAL1 (MAKLIST (CADDR PROBLEM)) T))
      (SETQ XLIST (REVAL1 (MAKLIST (CADDDR PROBLEM)) T))
      (COND (INEQU (SETQ INEQU (CDR INEQU))))
      (COND (FLIST (SETQ FLIST (CDR FLIST))))
      (SETQ E1 FLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND E1 (FREEOF EQLIST (CAR E1)))) (RETURN NIL)))
        (SETQ E1 (CDR E1))
        (GO WHILELABEL))
      (SETQ LIN_PROBLEM (COND (E1 NIL) (T T)))
      (SETQ EQLEN (LENGTH EQLIST))
      (PROG (E1)
        (SETQ E1 (CDR YLIST))
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROG (E2)
             (SETQ E2 (CDR XLIST))
            LAB
             (COND ((NULL E2) (RETURN NIL)))
             ((LAMBDA (E2)
                (COND
                 ((MY_FREEOF E1 E2)
                  (REDERR "Not all functions do depend on all variables."))))
              (CAR E2))
             (SETQ E2 (CDR E2))
             (GO LAB)))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       ((ATOM (CADR SYMTYPE))
        (COND
         ((EQUAL (CADR SYMTYPE) "point")
          (PROGN (SETQ POINTP T) (SETQ SYMORD 0)))
         ((EQUAL (CADR SYMTYPE) "contact")
          (PROGN (SETQ CONTACTP T) (SETQ SYMORD 1) NIL))
         ((EQUAL (CADR SYMTYPE) "general")
          (PROGN
           (SETQ GENERALP T)
           (SETQ SYMORD (CADDR SYMTYPE))
           (COND
            ((OR (NOT (FIXP SYMORD)) (LESSP SYMORD 1))
             (REDERR
              "The order of the generalized symmetry must be an integer > 0.")))))
         (T (REDERR "Inconclusive symmetry type."))))
       (T
        (PROGN
         (SETQ ANSATZP T)
         (SETQ SYMORD 0)
         (PROG (E1)
           (SETQ E1 (CDR SYMTYPE))
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROG (E2)
                (SETQ E2 (CDR YLIST))
               LAB
                (COND ((NULL E2) (RETURN NIL)))
                ((LAMBDA (E2)
                   (PROGN
                    (SETQ N (TOTDEG (CADDR E1) E2))
                    (COND ((GREATERP N SYMORD) (SETQ SYMORD N)))))
                 (CAR E2))
                (SETQ E2 (CDR E2))
                (GO LAB)))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (PROG (E1)
           (SETQ E1 FLIST)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (SETQ E2 (FCTARGS E1))
               (PROG (H)
                 (SETQ H E2)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (SETQ N2 (PLUS (MINUS 1) (LENGTH (COMBIDIF H))))
                     (COND ((GREATERP N2 SYMORD) (SETQ SYMORD N2)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               NIL))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (COND ((EQUAL SYMORD 0) (SETQ POINTP T))
               ((AND (EQUAL SYMORD 1) (EQUAL (LENGTH YLIST) 2))
                (SETQ CONTACTP T))
               (T (SETQ GENERALP T)))
         (SETQ SB NIL)
         (PROG (E1)
           (SETQ E1 FLIST)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (COND
               ((FREEOF EQLIST E1) (SETQ SB (CONS (LIST 'EQUAL E1 0) SB)))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (SETQ SB (CONS 'LIST SB))
         (SETQ H NIL)
         (PROG (E1)
           (SETQ E1 (CDR SYMTYPE))
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (SETQ N1 (AEVAL (LIST 'SUB SB (CADDR E1))))
               (COND
                ((AND (NOT (ZEROP N1))
                      (OR (NOT (PAIRP N1))
                          (AND (PAIRP N1)
                               (OR (NEQ (CAR N1) 'EQUAL)
                                   (NOT (ZEROP (CADDR N1)))))))
                 (PROGN
                  (SETQ H 0)
                  (PROGN
                   (PRIN2
                    "Your ansatz for the symmetry needs to be homogeneous, i.e. ")
                   (PRIN2
                    "substituting all unknown functions and constants to be computed ")
                   (PRIN2
                    "(which do not occur in the equation) to zero needs to make the ")
                   (PRIN2 "symmetry to zero. In your ansatz this is not ")
                   (PRIN2 "the case because the list of substitutions:")
                   NIL)
                  (ASSGNPRI (AEVAL SB) NIL 'ONLY)
                  (PROGN (PRIN2 "leaves this right hand side non-zero:") NIL)
                  (ASSGNPRI (AEVAL (LIST 'EQUAL (CADR E1) N1)) NIL 'ONLY)
                  (PROGN
                   (PRIN2
                    "To fix your ansatz you could, for example, simply multiply all ")
                   (PRIN2
                    "non-vanishing parts on all right hand sides in your ansatz with one ")
                   (PRIN2
                    "and the same unknown constant, say cc, and add cc to the list of unknowns ")
                   (PRIN2
                    "to be computed and to the list of non-vanishing expressions.")
                   NIL))))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB)))))
      (COND (H (RETURN NIL)))
      (SETQ PROBLEM 0)
      (SETQ EQCOPY1 EQLIST)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND EQCOPY1 (PAIRP (CAR EQCOPY1)) (EQUAL (CAAR EQCOPY1) 'EQUAL)
                (PAIRP (CADAR EQCOPY1)) (EQUAL (CAADAR EQCOPY1) 'DF)))
          (RETURN NIL)))
        (SETQ EQCOPY1 (CDR EQCOPY1))
        (GO WHILELABEL))
      (COND ((NULL EQCOPY1) (SETQ TRUESUB EQLIST)))
      (SETQ EQCOPY1 NIL)
      (COND
       ((AND PRINT_ LOGOPRINT_)
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "-----------------------------------------------")
          (PRIN2 "---------------------------")
          NIL)
         (TERPRI)
         (TERPRI)
         (PROGN
          (PRIN2 "This is LIEPDE - a program for calculating infinitesimal")
          (PRIN2 " symmetries")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "of single differential equations or systems of de's")
          NIL)
         NIL)))
      (TERPRI)
      (TERPRI)
      (COND ((EQUAL (LENGTH XLIST) 2) (PROGN (PRIN2 "The ODE") NIL))
            (T (PROGN (PRIN2 "The PDE") NIL)))
      (COND ((GREATERP (LENGTH YLIST) 2) (PROGN (PRIN2 "-system") NIL)))
      (PROGN (PRIN2 " under investigation is :") NIL)
      (TERPRI)
      (PROG (E1)
        (SETQ E1 EQLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (ASSGNPRI E1 NIL 'ONLY)) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (TERPRI)
      (PROGN (PRIN2 "for the function(s) : ") NIL)
      (TERPRI)
      (TERPRI)
      (FCTPRINT (CDR (REVAL1 YLIST T)))
      (TERPRI)
      (TERPRI)
      (SETQ EQLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 EQLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1) (AEVAL (LIST 'EQU_TO_EXPR E1)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1) (AEVAL (LIST 'EQU_TO_EXPR E1)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((GREATERP EQLEN 1) (SETQ EQLIST (DESORT EQLIST))))
      (COND
       (*TIME
        (PROGN
         (TERPRI)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "=============== Initializations") NIL)
         NIL)))
      (SETQ ORDR 0)
      (PROG (E1)
        (SETQ E1 EQLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (PROGN
            (SETQ H 0)
            (PROG (E2)
              (SETQ E2 (CDR YLIST))
             LAB
              (COND ((NULL E2) (RETURN NIL)))
              ((LAMBDA (E2)
                 (PROGN
                  (SETQ N (TOTDEG E1 E2))
                  (COND ((GREATERP N H) (SETQ H N)))))
               (CAR E2))
              (SETQ E2 (CDR E2))
              (GO LAB))
            (SETQ EQORDR (CONS H EQORDR))
            (COND ((GREATERP H ORDR) (SETQ ORDR H)))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ EQORDR (REVERSIP EQORDR))
      (COND ((GREATERP ORDR SYMORD) (SETQ JETORD ORDR))
            (T (SETQ JETORD SYMORD)))
      (SETQ SB (SUBDIF1 XLIST YLIST JETORD))
      (SETQ EQLIST (CONS 'LIST EQLIST))
      (COND (ANSATZP (SETQ EQLIST (LIST 'LIST SYMTYPE EQLIST))))
      (COND (TRUESUB (SETQ EQLIST (LIST 'LIST (CONS 'LIST TRUESUB) EQLIST))))
      (COND (INEQU (SETQ EQLIST (LIST 'LIST (CONS 'LIST INEQU) EQLIST))))
      (ON (LIST 'EVALLHSEQP))
      (SETQ EQLIST (TRANSEQ EQLIST XLIST YLIST SB))
      (OFF (LIST 'EVALLHSEQP))
      (COND
       (INEQU
        (PROGN (SETQ INEQU (CDADR EQLIST)) (SETQ EQLIST (CADDR EQLIST)))))
      (COND
       (TRUESUB
        (PROGN (SETQ TRUESUB (CDADR EQLIST)) (SETQ EQLIST (CADDR EQLIST)))))
      (COND
       (ANSATZP
        (PROGN (SETQ SYMTYPE (CDADR EQLIST)) (SETQ EQLIST (CDADDR EQLIST))))
       (T (SETQ EQLIST (CDR EQLIST))))
      (SETQ YLIST (CDR YLIST))
      (SETQ XLIST (CDR XLIST))
      (COND
       ((AND LIETRACE_ ANSATZP) (PROGN (PRIN2 "ansatz=") (PRIN2 SYMTYPE) NIL)))
      (SETQ DYLIST
              (CONS YLIST
                    (REVERSE
                     (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E1 (CDR SB))
                       (COND ((NULL E1) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (E1)
                                           (PROG (E2 FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ E2 (CDR E1))
                                             (COND ((NULL E2) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (E2)
                                                                 (CADDR E2))
                                                               (CAR E2))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ E2 (CDR E2))
                                             (COND
                                              ((NULL E2)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (E2) (CADDR E2))
                                                       (CAR E2))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))
                                         (CAR E1))
                                        NIL)))
                      LOOPLABEL
                       (SETQ E1 (CDR E1))
                       (COND ((NULL E1) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (E1)
                                   (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ E2 (CDR E1))
                                     (COND ((NULL E2) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (E2) (CADDR E2))
                                                       (CAR E2))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ E2 (CDR E2))
                                     (COND ((NULL E2) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (E2) (CADDR E2))
                                               (CAR E2))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR E1))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ REVDYLIST (REVERSE DYLIST))
      (SETQ VL XLIST)
      (PROG (E1)
        (SETQ E1 DYLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (SETQ VL (APPEND E1 VL))) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (SETQ VL (CONS 'LIST VL))
      (COND
       ((NOT ANSATZP)
        (SETQ DEPLIST
                (PROG (N FORALL-RESULT FORALL-ENDPTR)
                  (SETQ N 0)
                  (COND ((MINUSP (DIFFERENCE SYMORD N)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS (NTH DYLIST (PLUS N 1)) NIL)))
                 LOOPLABEL
                  (SETQ N (PLUS2 N 1))
                  (COND
                   ((MINUSP (DIFFERENCE SYMORD N)) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR (CONS (NTH DYLIST (PLUS N 1)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ XI (REVAL1 (AEVAL 'XI_) T))
      (SETQ ETA (REVAL1 (AEVAL 'ETA_) T))
      (SETQ N 0)
      (SETQ XILIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 XLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (PROGN
                                     (SETQ N (PLUS N 1))
                                     (COND
                                      ((OR POINTP ANSATZP)
                                       (PROGN
                                        (SETQ H (MKID 'XI_ E1))
                                        (COND
                                         ((NOT ANSATZP)
                                          (PROGN
                                           (NODEPENDLIST (LIST H))
                                           (DEPND H (CONS XLIST DEPLIST))
                                           (SETQ FLIST (CONS H FLIST))
                                           (SETQ FLIN_ (CONS H FLIN_))
                                           (SETQ DEPLI DEPLIST)
                                           NIL))
                                         (T (SETQ DEPLI NIL)))))
                                      (T (PROGN (SETQ H 0) (SETQ DEPLI NIL))))
                                     (LIST (SIMP H) E1 N DEPLI)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (PROGN
                             (SETQ N (PLUS N 1))
                             (COND
                              ((OR POINTP ANSATZP)
                               (PROGN
                                (SETQ H (MKID 'XI_ E1))
                                (COND
                                 ((NOT ANSATZP)
                                  (PROGN
                                   (NODEPENDLIST (LIST H))
                                   (DEPND H (CONS XLIST DEPLIST))
                                   (SETQ FLIST (CONS H FLIST))
                                   (SETQ FLIN_ (CONS H FLIN_))
                                   (SETQ DEPLI DEPLIST)
                                   NIL))
                                 (T (SETQ DEPLI NIL)))))
                              (T (PROGN (SETQ H 0) (SETQ DEPLI NIL))))
                             (LIST (SIMP H) E1 N DEPLI)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DEPLI (COND ((AND (NOT ANSATZP) (NOT GENERALP)) DEPLIST) (T NIL)))
      (SETQ N 0)
      (SETQ ETALIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 YLIST)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (PROGN
                                     (SETQ N (PLUS N 1))
                                     (SETQ H (MKID 'ETA_ E1))
                                     (COND
                                      ((NOT ANSATZP)
                                       (PROGN
                                        (COND
                                         ((NOT GENERALP)
                                          (PROGN
                                           (NODEPENDLIST (LIST H))
                                           (DEPND H (CONS XLIST DEPLIST)))))
                                        (SETQ FLIST (CONS H FLIST))
                                        (SETQ FLIN_ (CONS H FLIN_))
                                        NIL)))
                                     (LIST (SIMP H) E1 DEPLI)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (PROGN
                             (SETQ N (PLUS N 1))
                             (SETQ H (MKID 'ETA_ E1))
                             (COND
                              ((NOT ANSATZP)
                               (PROGN
                                (COND
                                 ((NOT GENERALP)
                                  (PROGN
                                   (NODEPENDLIST (LIST H))
                                   (DEPND H (CONS XLIST DEPLIST)))))
                                (SETQ FLIST (CONS H FLIST))
                                (SETQ FLIN_ (CONS H FLIN_))
                                NIL)))
                             (LIST (SIMP H) E1 DEPLI)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FLISTORG FLIST)
      (COND
       (ANSATZP
        (PROGN
         (PROG (E1)
           (SETQ E1 SYMTYPE)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (SETQ XILIST
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K XILIST)
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K)
                                             (CONS
                                              (SUBSQ (CAR K)
                                                     (LIST
                                                      (CONS (CADR E1)
                                                            (CADDR E1))))
                                              (CDR K)))
                                           (CAR K))
                                          NIL)))
                        LOOPLABEL
                         (SETQ K (CDR K))
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K)
                                     (CONS
                                      (SUBSQ (CAR K)
                                             (LIST
                                              (CONS (CADR E1) (CADDR E1))))
                                      (CDR K)))
                                   (CAR K))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ ETALIST
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K ETALIST)
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K)
                                             (CONS
                                              (SUBSQ (CAR K)
                                                     (LIST
                                                      (CONS (CADR E1)
                                                            (CADDR E1))))
                                              (CDR K)))
                                           (CAR K))
                                          NIL)))
                        LOOPLABEL
                         (SETQ K (CDR K))
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K)
                                     (CONS
                                      (SUBSQ (CAR K)
                                             (LIST
                                              (CONS (CADR E1) (CADDR E1))))
                                      (CDR K)))
                                   (CAR K))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               NIL))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (ADD_XI_ETA_DEPLI XILIST ETALIST REVDYLIST)
         NIL)))
      (COND
       (LIETRACE_
        (PROGN
         (PRIN2 "xilist=")
         (PRIN2 XILIST)
         (PRIN2 "  etalist=")
         (PRIN2 ETALIST)
         NIL)))
      (COND
       (TRUESUB
        (PROGN
         (SETQ DYCOPY (CAR REVDYLIST))
         (PROG ()
          WHILELABEL
           (COND ((NOT DYCOPY) (RETURN NIL)))
           (PROGN
            (SETQ E1 (CAR DYCOPY))
            (SETQ DYCOPY (CDR DYCOPY))
            (SETQ EQCOPY1 EQLIST)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND EQCOPY1 (MY_FREEOF (CAR EQCOPY1) E1))) (RETURN NIL)))
              (SETQ EQCOPY1 (CDR EQCOPY1))
              (GO WHILELABEL))
            (COND ((NULL EQCOPY1) (SETQ FREELIST (CONS E1 FREELIST)))))
           (GO WHILELABEL))))
       (T
        (PROGN
         (SETQ NO 0)
         (SETQ EQORDRCOP (COPY EQORDR))
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ NO (PLUS NO 1))
            (COND ((NULL TRUESUB) (SETQ TRUESUBNO 0))
                  (T (SETQ TRUESUBNO (LENGTH TRUESUB))))
            (SETQ MINORDR 1000)
            (PROG (E1)
              (SETQ E1 EQORDRCOP)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (COND
                  ((AND (NEQ E1 0) (LESSP E1 MINORDR)) (SETQ MINORDR E1))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (SETQ DYCOPY (COPY (NTH DYLIST (PLUS MINORDR 1))))
            (SETQ DYLEN (LENGTH DYCOPY))
            (SETQ ALLSUB NIL)
            (PROG (N1)
              (SETQ N1 1)
             LAB
              (COND ((MINUSP (DIFFERENCE DYLEN N1)) (RETURN NIL)))
              (PROGN
               (SETQ E1 (NTH DYCOPY N1))
               (SETQ H (COMBIDIF E1))
               (SETQ N (CAR H))
               (SETQ H (CDR H))
               (SETQ E2 TRUESUB)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND E2 (NULL (COMPAREDIF3 (CADAR E2) N H))))
                   (RETURN NIL)))
                 (SETQ E2 (CDR E2))
                 (GO WHILELABEL))
               (COND
                ((NULL E2)
                 (PROGN
                  (SETQ N2 0)
                  (SETQ SUBDY NIL)
                  (PROG (N3)
                    (SETQ N3 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE EQLEN N3)) (RETURN NIL)))
                    (COND
                     ((NOT (MY_FREEOF (NTH EQLIST N3) E1))
                      (PROGN
                       (SETQ N2 (PLUS N2 1))
                       (COND
                        ((EQUAL (NTH EQORDRCOP N3) MINORDR)
                         (PROGN
                          (SETQ E2
                                  (CDR
                                   (AEVAL* (LIST 'COEFF (NTH EQLIST N3) E1))))
                          (COND
                           ((EQUAL HIPOW* 1)
                            (SETQ SUBDY
                                    (CONS
                                     (LIST N1 N3
                                           (LIST 'EQUAL E1
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT (CAR E2)
                                                             (CADR E2)))))
                                     SUBDY))))))))))
                    (SETQ N3 (PLUS2 N3 1))
                    (GO LAB))
                  (COND
                   ((EQUAL N2 0)
                    (COND ((EQUAL NO 1) (SETQ FREELIST (CONS E1 FREELIST)))
                          (T NIL)))
                   (T
                    (PROGN
                     (COND
                      (SUBDY
                       (COND
                        ((EQUAL N2 1)
                         (PROGN
                          (SETQ H (CAR SUBDY))
                          (SETQ TRUESUB (CONS (CADDR H) TRUESUB))
                          (SETQ N (PNTH DYCOPY (CAR H)))
                          (RPLACA N 0)
                          (SETQ N (PNTH EQORDRCOP (CADR H)))
                          (RPLACA N 0)
                          NIL))
                        (T (SETQ ALLSUB (NCONC ALLSUB SUBDY))))))
                     NIL)))))))
              (SETQ N1 (PLUS2 N1 1))
              (GO LAB))
            (SETQ H (SETQ SUBDY 0))
            (PROG (H)
              (SETQ H ALLSUB)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (COND
                  ((AND (NEQ (NTH DYCOPY (CAR H)) 0)
                        (NEQ (NTH EQORDRCOP (CADR H)) 0))
                   (PROGN
                    (SETQ TRUESUB (CONS (CADDR H) TRUESUB))
                    (SETQ N (PNTH DYCOPY (CAR H)))
                    (RPLACA N 0)
                    (SETQ N (PNTH EQORDRCOP (CADR H)))
                    (RPLACA N 0)
                    NIL))))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            NIL)
           (COND
            ((NOT
              (OR (AND TRUESUB (EQUAL (LENGTH TRUESUB) EQLEN))
                  (EQUAL TRUESUBNO (LENGTH TRUESUB))))
             (GO REPEATLABEL))))
         (SETQ ALLSUB (SETQ EQORDRCOP (SETQ DYCOPY NIL)))
         (COND
          ((OR (NULL TRUESUB) (NEQ EQLEN (LENGTH TRUESUB)))
           (REDERR
            "Unable to find all substitutions. Input equations as df(..,..)=..!")))
         NIL)))
      (SETQ LHSLIST
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 TRUESUB)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (E1) (CADR E1)) (CAR E1)) NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E1) (CADR E1)) (CAR E1)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (CHKFLIST (CONS 'LIST FLIST) (CONS 'LIST LHSLIST))
      (SETQ TRUESUB
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 TRUESUB)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (CONS (COMBIDIF (CADR E1))
                                          (ADDDEPLI (CADDR E1) REVDYLIST)))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (CONS (COMBIDIF (CADR E1))
                                  (ADDDEPLI (CADDR E1) REVDYLIST)))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ H T)
      (SETQ NO T)
      (PROG (E1)
        (SETQ E1 TRUESUB)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1)
           (COND
            ((AND H NO)
             (PROGN
              (SETQ N1 (CAAR E1))
              (SETQ N2 (CDAR E1))
              (SETQ DYLEN (LENGTH N2))
              (PROG (E2)
                (SETQ E2 TRUESUB)
               LAB
                (COND ((NULL E2) (RETURN NIL)))
                ((LAMBDA (E2)
                   (PROGN
                    (COND
                     ((AND (NOT (EQ E1 E2)) (EQUAL N1 (CAAR E2))
                           (COMPAREDIF1 N2 (CDAR E2)))
                      (SETQ H NIL)))
                    (SETQ DYCOPY (CADDR E2))
                    (PROG (N)
                      (SETQ N 1)
                     LAB
                      (COND ((MINUSP (DIFFERENCE DYLEN N)) (RETURN NIL)))
                      (COND (DYCOPY (SETQ DYCOPY (CDR DYCOPY))))
                      (SETQ N (PLUS2 N 1))
                      (GO LAB))
                    (PROG (E3)
                      (SETQ E3 DYCOPY)
                     LAB
                      (COND ((NULL E3) (RETURN NIL)))
                      ((LAMBDA (E3)
                         (PROG (E4)
                           (SETQ E4 E3)
                          LAB
                           (COND ((NULL E4) (RETURN NIL)))
                           ((LAMBDA (E4)
                              (COND ((COMPAREDIF2 N1 N2 E4) (SETQ NO NIL))))
                            (CAR E4))
                           (SETQ E4 (CDR E4))
                           (GO LAB)))
                       (CAR E3))
                      (SETQ E3 (CDR E3))
                      (GO LAB))
                    NIL))
                 (CAR E2))
                (SETQ E2 (CDR E2))
                (GO LAB))))))
         (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (COND
       ((NULL H)
        (REDERR
         "One substitution can be made in the lhs of another substitution!")))
      (COND
       ((NULL NO)
        (REDERR
         "One substitution can be made in the rhs of another substitution!")))
      (SETQ SUBORDINC 0)
      (SETQ SUBORDINCLIST
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H TRUESUB)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (PROGN
                                     (SETQ N
                                             (DIFFERENCE (LENGTH (CADDR H))
                                                         (LENGTH (CAR H))))
                                     (COND
                                      ((GREATERP N SUBORDINC)
                                       (SETQ SUBORDINC N)))
                                     N))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (PROGN
                             (SETQ N
                                     (DIFFERENCE (LENGTH (CADDR H))
                                                 (LENGTH (CAR H))))
                             (COND ((GREATERP N SUBORDINC) (SETQ SUBORDINC N)))
                             N))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (LIETRACE_
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "truesub=") (PRIN2 TRUESUB) NIL)
         (TERPRI)
         (PROGN (PRIN2 "freelist=") (PRIN2 FREELIST) NIL)
         NIL)))
      (COND
       ((AND GENERALP (NULL ANSATZP))
        (PROGN
         (SETQ DEPLIST
                 (CONS YLIST
                       (PROG (DYCOPY FORALL-RESULT FORALL-ENDPTR)
                         (SETQ DYCOPY (CDR DEPLIST))
                         (COND ((NULL DYCOPY) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (DYCOPY)
                                             (PROGN
                                              (PROG (H)
                                                (SETQ H LHSLIST)
                                               LAB
                                                (COND ((NULL H) (RETURN NIL)))
                                                ((LAMBDA (H)
                                                   (SETQ DYCOPY
                                                           (LISTDIFDIF1 H
                                                            DYCOPY)))
                                                 (CAR H))
                                                (SETQ H (CDR H))
                                                (GO LAB))
                                              DYCOPY))
                                           (CAR DYCOPY))
                                          NIL)))
                        LOOPLABEL
                         (SETQ DYCOPY (CDR DYCOPY))
                         (COND ((NULL DYCOPY) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (DYCOPY)
                                     (PROGN
                                      (PROG (H)
                                        (SETQ H LHSLIST)
                                       LAB
                                        (COND ((NULL H) (RETURN NIL)))
                                        ((LAMBDA (H)
                                           (SETQ DYCOPY
                                                   (LISTDIFDIF1 H DYCOPY)))
                                         (CAR H))
                                        (SETQ H (CDR H))
                                        (GO LAB))
                                      DYCOPY))
                                   (CAR DYCOPY))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (PROG (E1)
           (SETQ E1 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (LENGTH ETALIST) E1)) (RETURN NIL)))
           (PROGN
            (SETQ H (NTH ETALIST E1))
            (SETQ E2 (PREPSQ (CAR H)))
            (NODEPENDLIST (LIST E2))
            (DEPND E2 (CONS XLIST DEPLIST))
            (SETQ H (PNTH H 3))
            (RPLACA H DEPLIST))
           (SETQ E1 (PLUS2 E1 1))
           (GO LAB)))))
      (SETQ PROC_LIST_ (DELETE 'MULTINTFAC PROC_LIST_))
      (COND
       (*TIME
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "time for initializations: ")
          (PRIN2 (DIFFERENCE (TIME) CPU))
          (PRIN2 " ms    GC time : ")
          (PRIN2 (DIFFERENCE (GCTIME) GC))
          (PRIN2 " ms")
          NIL)
         (SETQ CPU (TIME))
         (SETQ GC (GCTIME))
         NIL)))
      (COND
       ((OR PRELIM_ INDIVIDUAL_)
        (PROGN
         (SETQ PROC_LIST_BAK PROC_LIST_)
         (SETQ PROC_LIST_
                 '(TO_DO SEPARATION SUBST_LEVEL_0 SUBST_LEVEL_03
                   QUICK_INTEGRATION SUBST_LEVEL_33 GEN_SEPARATION))
         (SETQ MAX_GC_FAC_BAK MAX_GC_FAC)
         (SETQ MAX_GC_FAC 0)
         (SETQ INVERSE_TRAFO_LIST_INCOMPLETE NIL))))
      (SETQ SYMCON NIL)
      (SETQ N1 0)
      (COND
       ((AND PRELIM_ LIN_PROBLEM)
        (PROG (EQN)
          (SETQ EQN EQLIST)
         LAB
          (COND ((NULL EQN) (RETURN NIL)))
          ((LAMBDA (EQN)
             (PROGN
              (SETQ N1 (PLUS N1 1))
              (COND
               (*TIME
                (PROGN
                 (TERPRI)
                 (TERPRI)
                 (TERPRI)
                 (PROGN
                  (PRIN2 "=============== Preconditions for the ")
                  (PRIN2 N1)
                  (PRIN2 ". equation")
                  NIL)
                 NIL)))
              (SETQ REVDYCOPY REVDYLIST)
              (PROG (E1)
                (SETQ E1 (PLUS (NTH EQORDR N1) 1))
               LAB
                (COND ((MINUSP (DIFFERENCE ORDR E1)) (RETURN NIL)))
                (SETQ REVDYCOPY (CDR REVDYCOPY))
                (SETQ E1 (PLUS2 E1 1))
                (GO LAB))
              (SETQ N2 (CADR (ADDDEPLI EQN REVDYCOPY)))
              (SETQ VL N2)
              (SETQ OCCLI (LASTCAR N2))
              (SETQ FREELIST (SETDIFF (CAR REVDYCOPY) OCCLI))
              (COND
               ((AND POINTP (EQUAL SUBORDINC 0)) (SETQ EQN (DROP EQN OCCLI)))
               (T (SETQ OCCLI (JOINSUBLISTS N2))))
              (SETQ FREELIST (SETDIFF FREELIST LHSLIST))
              (PROG (N4)
                (SETQ N4 FREELIST)
               LAB
                (COND ((NULL N4) (RETURN NIL)))
                ((LAMBDA (N4)
                   (COND
                    ((NOT (FREEOF DEPL* N4))
                     (SETQ FREELIST (DELETE N4 FREELIST)))))
                 (CAR N4))
                (SETQ N4 (CDR N4))
                (GO LAB))
              (COND
               (FREELIST
                (PROGN
                 (SETQ N (NTH EQORDR N1))
                 (SETQ H (SIMP 0))
                 (PROG (E1)
                   (SETQ E1 XILIST)
                  LAB
                   (COND ((NULL E1) (RETURN NIL)))
                   ((LAMBDA (E1)
                      (COND
                       ((AND (CADDDR E1) (GREATERP (LENGTH (CADDDR E1)) N))
                        (SETQ H
                                (ADDSQ H
                                       (COND ((SQZEROP (CAR E1)) (SIMP 0))
                                             (T
                                              (PROGN
                                               (SETQ N3
                                                       (MERGEDEPLI N3
                                                        (CADDDR E1)))
                                               (MULTSQ (CAR E1)
                                                       (SIMPDF
                                                        (LIST EQN
                                                              (CADR
                                                               E1))))))))))))
                    (CAR E1))
                   (SETQ E1 (CDR E1))
                   (GO LAB))
                 (PROG (E2)
                   (SETQ E2 OCCLI)
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (SETQ H
                              (ADDSQ H
                                     (MULTSQ
                                      (PROGN
                                       (SETQ N5 (COMBIDIF E2))
                                       (SETQ N4
                                               (CAR
                                                (ETAMN (CAR N5) (CDR N5) XILIST
                                                 ETALIST (NTH EQORDR N1)
                                                 TRUESUB SUBORDINC XLIST)))
                                       (SETQ VL (MERGEDEPLI VL (CDR N4)))
                                       (CAR N4))
                                      (SIMPDF (LIST EQN E2))))))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB))
                 (SETQ VL (JOINSUBLISTS (CONS XLIST VL)))
                 (COND
                  ((GREATERP N1 1)
                   (PROG (E1)
                     (SETQ E1 (REVERSE FLISTORG))
                    LAB
                     (COND ((NULL E1) (RETURN NIL)))
                     ((LAMBDA (E1)
                        (COND
                         ((NOT (FREEOF SYMCON E1))
                          (SETQ FLIST (UNION (LIST E1) FLIST)))))
                      (CAR E1))
                     (SETQ E1 (CDR E1))
                     (GO LAB))))
                 (PROG (E2)
                   (SETQ E2 FREELIST)
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (PROGN
                       (SETQ E1 (CONS (CAR (DIFFSQ H E2)) 1))
                       (PROG (N2)
                         (SETQ N2 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE EQLEN N2)) (RETURN NIL)))
                         (PROGN
                          (SETQ N4 (NTH LHSLIST N2))
                          (COND
                           ((NOT (MY_FREEOF EQN N4))
                            (SETQ E1
                                    (CONS
                                     (CAR
                                      (SUBSQ E1
                                             (LIST
                                              (CONS N4
                                                    (CADR (NTH TRUESUB N2))))))
                                     1))))
                          (SETQ VL (DELETE N4 VL)))
                         (SETQ N2 (PLUS2 N2 1))
                         (GO LAB))
                       (COND
                        ((AND (CAR E1) (NEQ (CAR E1) 0))
                         (PROGN
                          (SETQ E1
                                  (CDR
                                   (SPLIT_SIMPLIFY
                                    (LIST (LIST 'LIST (MK*SQ E1))
                                          (CONS 'LIST NIL) (CONS 'LIST FLIST)
                                          (CONS 'LIST VL) NIL))))
                          (SETQ SYMCON (NCONC E1 SYMCON)))))))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB))
                 (COND
                  ((AND SYMCON (OR INDIVIDUAL_ (EQUAL N1 EQLEN)))
                   (PROGN
                    (SETQ H
                            (CALLCRACK *TIME CPU GC LIETRACE_ SYMCON FLIST VL
                             XILIST ETALIST INEQU NIL))
                    (SETQ SYMCON (CAR H))
                    (SETQ XILIST (CADR H))
                    (SETQ ETALIST (CADDR H))
                    (SETQ FLIST (CADDDR H))
                    (SETQ INEQU (CADDDR (CDR H)))
                    (SETQ H NIL)
                    (SETQ CPU (TIME))
                    (SETQ GC (GCTIME))
                    NIL))))))))
           (CAR EQN))
          (SETQ EQN (CDR EQN))
          (GO LAB))))
      (SETQ N1 0)
      (SETQ VL NIL)
      (PROG (EQN)
        (SETQ EQN EQLIST)
       LAB
        (COND ((NULL EQN) (RETURN NIL)))
        ((LAMBDA (EQN)
           (PROGN
            (SETQ N1 (PLUS N1 1))
            (COND
             (*TIME
              (PROGN
               (TERPRI)
               (TERPRI)
               (TERPRI)
               (PROGN
                (PRIN2 "=============== Full conditions for the ")
                (PRIN2 N1)
                (PRIN2 ". equation")
                NIL)
               NIL)))
            (SETQ N2 (CADR (ADDDEPLI EQN REVDYLIST)))
            (SETQ N3 N2)
            (SETQ H (SIMP 0))
            (PROG (E1)
              (SETQ E1 XILIST)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (SETQ H
                         (ADDSQ H
                                (COND ((SQZEROP (CAR E1)) (SIMP 0))
                                      (T
                                       (PROGN
                                        (SETQ N3 (MERGEDEPLI N3 (CADDDR E1)))
                                        (MULTSQ (CAR E1)
                                                (SIMPDF
                                                 (LIST EQN (CADR E1))))))))))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (PROG (E1)
              (SETQ E1 N2)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROG (E2)
                   (SETQ E2 E1)
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (SETQ H
                              (ADDSQ H
                                     (MULTSQ
                                      (PROGN
                                       (SETQ N5 (COMBIDIF E2))
                                       (SETQ N4
                                               (CAR
                                                (ETAMN (CAR N5) (CDR N5) XILIST
                                                 ETALIST 0 TRUESUB 0 XLIST)))
                                       (SETQ N3 (MERGEDEPLI N3 (CDR N4)))
                                       (CAR N4))
                                      (SIMPDF (LIST EQN E2))))))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB)))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (SETQ H (CONS (CAR H) 1))
            (SETQ N3 (JOINSUBLISTS (CONS XLIST N3)))
            (PROG (N2)
              (SETQ N2 1)
             LAB
              (COND ((MINUSP (DIFFERENCE EQLEN N2)) (RETURN NIL)))
              (PROGN
               (SETQ N4 (NTH LHSLIST N2))
               (COND
                ((NOT (MY_FREEOF EQN N4))
                 (SETQ H
                         (CONS
                          (CAR
                           (SUBSQ H (LIST (CONS N4 (CADR (NTH TRUESUB N2))))))
                          1))))
               (SETQ N3 (DELETE N4 N3)))
              (SETQ N2 (PLUS2 N2 1))
              (GO LAB))
            (SETQ VL (UNION VL N3))
            (COND
             ((OR PRELIM_ (GREATERP N1 1))
              (PROG (E1)
                (SETQ E1 (REVERSE FLISTORG))
               LAB
                (COND ((NULL E1) (RETURN NIL)))
                ((LAMBDA (E1)
                   (COND
                    ((NOT (FREEOF SYMCON E1))
                     (SETQ FLIST (UNION (LIST E1) FLIST)))))
                 (CAR E1))
                (SETQ E1 (CDR E1))
                (GO LAB))))
            (COND
             ((AND (CAR H) (NEQ (CAR H) 0))
              (PROGN
               (SETQ H
                       (CDR
                        (SPLIT_SIMPLIFY
                         (LIST (LIST 'LIST (MK*SQ H)) (CONS 'LIST NIL)
                               (CONS 'LIST FLIST) (CONS 'LIST VL) NIL))))
               (SETQ SYMCON (NCONC H SYMCON)))))
            (SETQ LAST_CALL (COND ((EQUAL N1 EQLEN) T) (T NIL)))
            (COND
             ((OR (AND INDIVIDUAL_ LIN_PROBLEM) LAST_CALL)
              (PROGN
               (COND
                (LAST_CALL
                 (PROGN
                  (SETQ ETAMN_AL NIL)
                  (COND
                   ((OR PRELIM_ INDIVIDUAL_)
                    (PROGN
                     (SETQ PROC_LIST_ PROC_LIST_BAK)
                     (SETQ MAX_GC_FAC MAX_GC_FAC_BAK)))))))
               (SETQ ALLSYM
                       (CALLCRACK *TIME CPU GC LIETRACE_ SYMCON FLIST VL XILIST
                        ETALIST INEQU LAST_CALL))
               (SETQ CPU (TIME))
               (SETQ GC (GCTIME))
               (COND (LAST_CALL (SETQ FLIST FLISTORG))
                     (T
                      (PROGN
                       (SETQ SYMCON (CAR ALLSYM))
                       (SETQ XILIST (CADR ALLSYM))
                       (SETQ ETALIST (CADDR ALLSYM))
                       (SETQ FLIST (CADDDR ALLSYM))
                       (SETQ INEQU (CADDDR (CDR ALLSYM)))
                       (SETQ ALLSYM NIL)
                       NIL))))))))
         (CAR EQN))
        (SETQ EQN (CDR EQN))
        (GO LAB))
      (SETQ EQN
              (SETQ SB
                      (SETQ E1
                              (SETQ E2
                                      (SETQ N
                                              (SETQ H
                                                      (SETQ DYLIST
                                                              (SETQ DEPLIST
                                                                      (SETQ SYMORD
                                                                              NIL)))))))))
      (COND
       ((AND DONE_TRAFO (CDR DONE_TRAFO))
        (PROGN
         (TERPRI)
         (COND
          ((CDDR DONE_TRAFO)
           (PROGN
            (PRIN2 "The following transformations reverse the transformations")
            NIL))
          (T
           (PROGN
            (PRIN2 "The following transformation reverses the transformation")
            NIL)))
         (TERPRI)
         (PROGN (PRIN2 "performed in the computation:") NIL)
         (ASSGNPRI (AEVAL DONE_TRAFO) NIL 'ONLY)
         (COND
          (INVERSE_TRAFO_LIST_INCOMPLETE
           (PROGN
            (PRIN2 "***** The list 'done_trafo' of inverse transformations")
            NIL)))
         (TERPRI)
         (PROGN
          (PRIN2 "      is not complete as at least one transformation")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "      could not be inverted") NIL)
         (TERPRI))))
      (SETQ N1 0)
      (COND
       ((AND ALLSYM (EQUAL (CAR ALLSYM) 'LIST)) (SETQ ALLSYM (CDR ALLSYM))))
      (SETQ H
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 ALLSYM)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E1)
                                    (CONS
                                     (PLUS (LENGTH (CDADR E1))
                                           (LENGTH (CDADDR E1)))
                                     E1))
                                  (CAR E1))
                                 NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E1)
                            (CONS
                             (PLUS (LENGTH (CDADR E1)) (LENGTH (CDADDR E1)))
                             E1))
                          (CAR E1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ H (IDX_SORT H))
      (SETQ ALLSYM
              (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ E1 H)
                (COND ((NULL E1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (E1) (CDR E1)) (CAR E1)) NIL)))
               LOOPLABEL
                (SETQ E1 (CDR E1))
                (COND ((NULL E1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E1) (CDR E1)) (CAR E1)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT ALLSYM) (RETURN NIL)))
        (PROGN
         (NODEPENDLIST YLIST)
         (SETQ SYMCON_S (CDADAR ALLSYM))
         (SETQ XILIST_S XILIST)
         (SETQ ETALIST_S ETALIST)
         (SETQ INEQU_S INEQU)
         (SETQ TRUESUB_S TRUESUB)
         (SETQ PARALIST NIL)
         (PROG (G)
           (SETQ G (CDADDR (CAR ALLSYM)))
          LAB
           (COND ((NULL G) (RETURN NIL)))
           ((LAMBDA (G)
              (COND
               ((NOT (FREEOF EQLIST (CADR G)))
                (PROGN
                 (SETQ TRUESUB_S (SUBST (CADDR G) (CADR G) TRUESUB_S))
                 (SETQ PARALIST (CONS G PARALIST))))
               (T
                (PROGN
                 (SETQ XILIST_S
                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                           (SETQ K XILIST_S)
                           (COND ((NULL K) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (K)
                                               (CONS
                                                (SUBSQ (CAR K)
                                                       (LIST
                                                        (CONS (CADR G)
                                                              (CADDR G))))
                                                (CDR K)))
                                             (CAR K))
                                            NIL)))
                          LOOPLABEL
                           (SETQ K (CDR K))
                           (COND ((NULL K) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (CONS
                                        (SUBSQ (CAR K)
                                               (LIST
                                                (CONS (CADR G) (CADDR G))))
                                        (CDR K)))
                                     (CAR K))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ ETALIST_S
                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                           (SETQ K ETALIST_S)
                           (COND ((NULL K) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (K)
                                               (CONS
                                                (SUBSQ (CAR K)
                                                       (LIST
                                                        (CONS (CADR G)
                                                              (CADDR G))))
                                                (CDR K)))
                                             (CAR K))
                                            NIL)))
                          LOOPLABEL
                           (SETQ K (CDR K))
                           (COND ((NULL K) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (K)
                                       (CONS
                                        (SUBSQ (CAR K)
                                               (LIST
                                                (CONS (CADR G) (CADDR G))))
                                        (CDR K)))
                                     (CAR K))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (SETQ INEQU_S (SUBST (CADDR G) (CADR G) INEQU_S))
                 NIL))))
            (CAR G))
           (SETQ G (CDR G))
           (GO LAB))
         (COND
          (LIETRACE_
           (PROGN
            (PROGN (PRIN2 "final symcon : ") (PRIN2 SYMCON_S) NIL)
            (TERPRI)
            (PROGN (PRIN2 "final xilist = ") (PRIN2 XILIST_S) NIL)
            (TERPRI)
            (PROGN (PRIN2 "final etalist= ") (PRIN2 ETALIST_S) NIL)
            (TERPRI)
            NIL)))
         (SETQ FLIST_S (CDR (REVAL1 (CADDDR (CAR ALLSYM)) T)))
         (SETQ ALLSYM (CDR ALLSYM))
         (SETQ OLDPRINT_ PRINT_)
         (SETQ PRINT_ NIL)
         (COND
          (PRINT_
           (PROGN
            (TERPRI)
            (PROGN
             (PRIN2 "Remaining free functions after the last CRACK-run:")
             NIL)
            (TERPRI)
            (FCTPRINT FLIST_S)
            (TERPRI)
            (TERPRI))))
         (SETQ H
                 (APPEND
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL XILIST_S)
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (MK*SQ (CAR EL))) (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (EL) (MK*SQ (CAR EL))) (CAR EL))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL ETALIST_S)
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (MK*SQ (CAR EL))) (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (EL) (MK*SQ (CAR EL))) (CAR EL))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
         (COND
          (SYMCON_S
           (PROG (EL)
             (SETQ EL SYMCON_S)
            LAB
             (COND ((NULL EL) (RETURN NIL)))
             ((LAMBDA (EL) (SETQ H (CONS EL H))) (CAR EL))
             (SETQ EL (CDR EL))
             (GO LAB))))
         (SETQ H (CONS 'LIST H))
         (SETQ FLIST_SLIN NIL)
         (SETQ FLIST_SNLI NIL)
         (PROG (E1)
           (SETQ E1 FLIST_S)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (COND
               ((FREEOF PARALIST E1) (SETQ FLIST_SLIN (CONS E1 FLIST_SLIN)))
               (T (SETQ FLIST_SNLI (CONS E1 FLIST_SNLI)))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (SETQ OLDBATCH_MODE *BATCH_MODE)
         (SETQ *BATCH_MODE NIL)
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "***** START OF A COMPUTATION TO DROP REDUNDANT *****")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "*****  CONSTANTS AND FUNCTIONS OF INTEGRATION  *****")
             NIL)
            (TERPRI)
            NIL)))
         (SETQ SB
                 (REVAL1
                  (DROPREDUNDANT H (CONS 'LIST FLIST_SLIN) (CONS 'LIST VL)
                   (LIST 'LIST))
                  T))
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "***** THE COMPUTATION TO DROP REDUNDANT CONSTANTS *****")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "*****    AND FUNCTIONS OF INTEGRATION FINISHED    *****")
             NIL)
            (TERPRI)
            NIL)))
         (SETQ *BATCH_MODE OLDBATCH_MODE)
         (COND
          (SB
           (PROGN
            (SETQ FLIST_SLIN (CDR (CADDDR SB)))
            (SETQ H (CADDR SB))
            (SETQ SB (CADR SB))
            (SETQ E1 NIL))))
         (COND
          ((OR (NOT (FREEOFLIST XILIST_S FLIST))
               (NOT (FREEOFLIST ETALIST_S FLIST)))
           (SETQ H NIL))
          (T (SETQ H (REVAL1 (ABSORBCONST H (CONS 'LIST FLIST_SLIN)) T))))
         (COND
          (H
           (COND (SB (SETQ SB (APPEND SB (CDR H))))
                 (T (SETQ SB (CONS 'LIST (CDR H)))))))
         (COND
          (SB
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "Free constants and/or functions have been rescaled. ")
                NIL))))
            (PROG (E1)
              (SETQ E1 (CDR SB))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROGN
                  (SETQ XILIST_S
                          (PROG (K FORALL-RESULT FORALL-ENDPTR)
                            (SETQ K XILIST_S)
                            (COND ((NULL K) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (K)
                                                (CONS
                                                 (SUBSQ (CAR K)
                                                        (LIST
                                                         (CONS
                                                          (REVAL1 (CADR E1) T)
                                                          (CADDR E1))))
                                                 (CDR K)))
                                              (CAR K))
                                             NIL)))
                           LOOPLABEL
                            (SETQ K (CDR K))
                            (COND ((NULL K) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (K)
                                        (CONS
                                         (SUBSQ (CAR K)
                                                (LIST
                                                 (CONS (REVAL1 (CADR E1) T)
                                                       (CADDR E1))))
                                         (CDR K)))
                                      (CAR K))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                  (SETQ ETALIST_S
                          (PROG (K FORALL-RESULT FORALL-ENDPTR)
                            (SETQ K ETALIST_S)
                            (COND ((NULL K) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (K)
                                                (CONS
                                                 (SUBSQ (CAR K)
                                                        (LIST
                                                         (CONS
                                                          (REVAL1 (CADR E1) T)
                                                          (CADDR E1))))
                                                 (CDR K)))
                                              (CAR K))
                                             NIL)))
                           LOOPLABEL
                            (SETQ K (CDR K))
                            (COND ((NULL K) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (K)
                                        (CONS
                                         (SUBSQ (CAR K)
                                                (LIST
                                                 (CONS (REVAL1 (CADR E1) T)
                                                       (CADDR E1))))
                                         (CDR K)))
                                      (CAR K))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                  (SETQ SYMCON_S
                          (CDR
                           (REVAL1
                            (CONS 'LIST
                                  (SUBST (CADDR E1) (REVAL1 (CADR E1) T)
                                         SYMCON_S))
                            T)))
                  NIL))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            NIL)))
         (SETQ SYMCON_S
                 (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E1 SYMCON_S)
                   (COND ((NULL E1) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (E1)
                                       (CAR
                                        (SIMPLIFYPDESQ (SIMP E1)
                                         (APPEND FLIST_SLIN FLIST_SNLI) T NIL
                                         NIL)))
                                     (CAR E1))
                                    NIL)))
                  LOOPLABEL
                   (SETQ E1 (CDR E1))
                   (COND ((NULL E1) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (E1)
                               (CAR
                                (SIMPLIFYPDESQ (SIMP E1)
                                 (APPEND FLIST_SLIN FLIST_SNLI) T NIL NIL)))
                             (CAR E1))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ PRINT_ OLDPRINT_)
         (SETQ ETAPQLIST NIL)
         (COND
          ((AND (FIXP PROLONG_ORDER) (GREATERP PROLONG_ORDER 0))
           (PROGN
            (SETQ SB
                    (SUBDIF1 (CONS 'LIST XLIST) (CONS 'LIST YLIST)
                     PROLONG_ORDER))
            (PROG (E1)
              (SETQ E1 (CDR SB))
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (PROG (E2)
                   (SETQ E2 (CDR E1))
                  LAB
                   (COND ((NULL E2) (RETURN NIL)))
                   ((LAMBDA (E2)
                      (PROGN
                       (SETQ H (COMBIDIF (CADDR E2)))
                       (SETQ N4 (MKID 'ETA_ (CAR H)))
                       (PROG (N2)
                         (SETQ N2 (CDR H))
                        LAB
                         (COND ((NULL N2) (RETURN NIL)))
                         ((LAMBDA (N2) (SETQ N4 (MKID N4 (NTH XLIST N2))))
                          (CAR N2))
                         (SETQ N2 (CDR N2))
                         (GO LAB))
                       (SETQ H
                               (CAR
                                (ETAMN (CAR H) (CDR H) XILIST_S ETALIST_S 0
                                 TRUESUB_S 0 XLIST)))
                       (SETQ N3 (DIFFERENCE (LENGTH (CDR H)) 1))
                       (COND ((GREATERP N3 JETORD) (SETQ JETORD N3)))
                       (SETQ ETAPQLIST
                               (CONS (LIST 'EQUAL N4 (MK*SQ (CAR H)))
                                     ETAPQLIST))
                       NIL))
                    (CAR E2))
                   (SETQ E2 (CDR E2))
                   (GO LAB)))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB)))))
         (SETQ REVDYLIST NIL)
         (COND ((GREATERP (LENGTH FLIST_SLIN) 1) (SETQ N T)) (T (SETQ N NIL)))
         (SETQ XILIST_S
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL XILIST_S)
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL)
                                       (PROGN
                                        (SETQ E1 (MKID 'XI_ (CADR EL)))
                                        (SETQ E1
                                                (LIST 'EQUAL E1
                                                      (PREPSQ (CAR EL))))
                                        E1))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL)
                               (PROGN
                                (SETQ E1 (MKID 'XI_ (CADR EL)))
                                (SETQ E1 (LIST 'EQUAL E1 (PREPSQ (CAR EL))))
                                E1))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ ETALIST_S
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL ETALIST_S)
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL)
                                       (PROGN
                                        (SETQ E1 (MKID 'ETA_ (CADR EL)))
                                        (SETQ E1
                                                (LIST 'EQUAL E1
                                                      (PREPSQ (CAR EL))))
                                        E1))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL)
                               (PROGN
                                (SETQ E1 (MKID 'ETA_ (CADR EL)))
                                (SETQ E1 (LIST 'EQUAL E1 (PREPSQ (CAR EL))))
                                E1))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (E1)
           (SETQ E1 YLIST)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1) (DEPND E1 (LIST XLIST))) (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (ON (LIST 'EVALLHSEQP))
         (SETQ SB (SUBDIF1 (CONS 'LIST XLIST) (CONS 'LIST YLIST) JETORD))
         (SETQ SB
                 (PROG (E1 FORALL-RESULT FORALL-ENDPTR)
                   (SETQ E1 (GETRLIST (AEVAL* SB)))
                  STARTOVER
                   (COND ((NULL E1) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (E1)
                              (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                (SETQ E2 (GETRLIST (AEVAL* E1)))
                                (COND ((NULL E2) (RETURN (MAKELIST NIL))))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (E2)
                                                    (AEVAL*
                                                     (LIST 'EQUAL
                                                           (LIST 'RHS E2)
                                                           (LIST 'LHS E2))))
                                                  (CAR E2))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ E2 (CDR E2))
                                (COND
                                 ((NULL E2)
                                  (RETURN (CONS 'LIST FORALL-RESULT))))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (E2)
                                            (AEVAL*
                                             (LIST 'EQUAL (LIST 'RHS E2)
                                                   (LIST 'LHS E2))))
                                          (CAR E2))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR E1)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ E1 (CDR E1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL E1) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (E1)
                               (PROG (E2 FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ E2 (GETRLIST (AEVAL* E1)))
                                 (COND ((NULL E2) (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (E2)
                                                     (AEVAL*
                                                      (LIST 'EQUAL
                                                            (LIST 'RHS E2)
                                                            (LIST 'LHS E2))))
                                                   (CAR E2))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ E2 (CDR E2))
                                 (COND
                                  ((NULL E2)
                                   (RETURN (CONS 'LIST FORALL-RESULT))))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (E2)
                                             (AEVAL*
                                              (LIST 'EQUAL (LIST 'RHS E2)
                                                    (LIST 'LHS E2))))
                                           (CAR E2))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR E1))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ E1 (CDR E1))
                   (GO LOOPLABEL)))
         (OFF (LIST 'EVALLHSEQP))
         (SETQ XILIST_S (CDR (AEVAL* (LIST 'SUB SB (CONS 'LIST XILIST_S)))))
         (SETQ ETALIST_S (CDR (AEVAL* (LIST 'SUB SB (CONS 'LIST ETALIST_S)))))
         (SETQ ETAPQLIST (CDR (AEVAL* (LIST 'SUB SB (CONS 'LIST ETAPQLIST)))))
         (SETQ XICOP XILIST_S)
         (SETQ ETACOP ETALIST_S)
         (SETQ ETAPQCOP ETAPQLIST)
         (SETQ SB NIL)
         (SETQ FLCOP FLIST_SLIN)
         (PROG (E1)
           (SETQ E1 FLIST_SLIN)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (COND ((NOT (FREEOF EQLIST E1)) (SETQ FLCOP (DELETE E1 FLCOP)))))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (PROG (E1)
           (SETQ E1 FLCOP)
          LAB
           (COND ((NULL E1) (RETURN NIL)))
           ((LAMBDA (E1)
              (PROGN
               (COND
                ((FREEOF SYMCON_S E1)
                 (PROGN
                  (SETQ N1 (PLUS N1 1))
                  (SETQ XI XICOP)
                  (SETQ ETA ETACOP)
                  (SETQ ETAPQ ETAPQCOP)
                  (PROG (E2)
                    (SETQ E2 FLCOP)
                   LAB
                    (COND ((NULL E2) (RETURN NIL)))
                    ((LAMBDA (E2)
                       (COND
                        ((NEQ E2 E1)
                         (PROGN
                          (SETQ XI (SUBST 0 E2 XI))
                          (SETQ ETA (SUBST 0 E2 ETA))
                          (SETQ ETAPQ (SUBST 0 E2 ETAPQ))))
                        ((NULL (CDR (FARGS E1)))
                         (PROGN
                          (SETQ XI (SUBST 1 E2 XI))
                          (SETQ ETA (SUBST 1 E2 ETA))
                          (SETQ ETAPQ (SUBST 1 E2 ETAPQ))))))
                     (CAR E2))
                    (SETQ E2 (CDR E2))
                    (GO LAB))
                  (TERPRI)
                  (PROGN
                   (PRIN2 "-------- ")
                   (PRIN2 N1)
                   (PRIN2 ". Symmetry:")
                   NIL)
                  (TERPRI)
                  (PROG (E2)
                    (SETQ E2 PARALIST)
                   LAB
                    (COND ((NULL E2) (RETURN NIL)))
                    ((LAMBDA (E2)
                       (ASSGNPRI
                        (AEVAL* (LIST 'EQUAL (CADR E2) (REVAL1 (CADDR E2) T)))
                        NIL 'ONLY))
                     (CAR E2))
                    (SETQ E2 (CDR E2))
                    (GO LAB))
                  (PROG (E2)
                    (SETQ E2 XI)
                   LAB
                    (COND ((NULL E2) (RETURN NIL)))
                    ((LAMBDA (E2)
                       (ASSGNPRI
                        (AEVAL* (LIST 'EQUAL (CADR E2) (REVAL1 (CADDR E2) T)))
                        NIL 'ONLY))
                     (CAR E2))
                    (SETQ E2 (CDR E2))
                    (GO LAB))
                  (PROG (E2)
                    (SETQ E2 ETA)
                   LAB
                    (COND ((NULL E2) (RETURN NIL)))
                    ((LAMBDA (E2)
                       (ASSGNPRI
                        (AEVAL* (LIST 'EQUAL (CADR E2) (REVAL1 (CADDR E2) T)))
                        NIL 'ONLY))
                     (CAR E2))
                    (SETQ E2 (CDR E2))
                    (GO LAB))
                  (PROG (E2)
                    (SETQ E2 ETAPQ)
                   LAB
                    (COND ((NULL E2) (RETURN NIL)))
                    ((LAMBDA (E2)
                       (ASSGNPRI
                        (AEVAL* (LIST 'EQUAL (CADR E2) (REVAL1 (CADDR E2) T)))
                        NIL 'ONLY))
                     (CAR E2))
                    (SETQ E2 (CDR E2))
                    (GO LAB))
                  (COND
                   ((CDR (FARGS E1))
                    (PROGN
                     (TERPRI)
                     (PROGN (PRIN2 "with ") NIL)
                     (FCTPRINT (LIST E1))
                     (TERPRI))))
                  (SETQ XICOP (SUBST 0 E1 XICOP))
                  (SETQ ETACOP (SUBST 0 E1 ETACOP))
                  (SETQ ETAPQCOP (SUBST 0 E1 ETAPQCOP))
                  (SETQ FLCOP (DELETE E1 FLCOP))
                  NIL)))
               NIL))
            (CAR E1))
           (SETQ E1 (CDR E1))
           (GO LAB))
         (COND
          (FLCOP
           (PROGN
            (COND
             ((GREATERP (PLUS (LENGTH FLCOP) (LENGTH FLIST_SNLI)) 1)
              (SETQ N T))
             (T (SETQ N NIL)))
            (TERPRI)
            (COND ((EQUAL FLCOP FLIST_SLIN) (PROGN (PRIN2 "-------- S") NIL))
                  (T (PROGN (PRIN2 "-------- Further s") NIL)))
            (PROGN (PRIN2 "ymmetr") (PRIN2 (COND (N "ies:") (T "y:"))) NIL)
            (TERPRI)
            (PROG (E1)
              (SETQ E1 PARALIST)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (ASSGNPRI
                  (AEVAL* (LIST 'EQUAL (CADR E1) (REVAL1 (CADDR E1) T))) NIL
                  'ONLY))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (PROG (E1)
              (SETQ E1 XICOP)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (ASSGNPRI
                  (AEVAL* (LIST 'EQUAL (CADR E1) (REVAL1 (CADDR E1) T))) NIL
                  'ONLY))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (PROG (E1)
              (SETQ E1 ETACOP)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (ASSGNPRI
                  (AEVAL* (LIST 'EQUAL (CADR E1) (REVAL1 (CADDR E1) T))) NIL
                  'ONLY))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB))
            (PROG (E1)
              (SETQ E1 ETAPQCOP)
             LAB
              (COND ((NULL E1) (RETURN NIL)))
              ((LAMBDA (E1)
                 (ASSGNPRI
                  (AEVAL* (LIST 'EQUAL (CADR E1) (REVAL1 (CADDR E1) T))) NIL
                  'ONLY))
               (CAR E1))
              (SETQ E1 (CDR E1))
              (GO LAB)))))
         (TERPRI)
         (COND
          (FLCOP
           (PROGN
            (PROGN (PRIN2 "with ") NIL)
            (FCTPRINT
             (CDR (REVAL1 (CONS 'LIST (APPEND FLCOP FLIST_SNLI)) T))))))
         (COND
          ((NULL SYMCON_S)
           (COND
            (FLCOP
             (PROGN
              (PROGN
               (PRIN2 " which ")
               (PRIN2 (COND (N "are") (T "is")))
               (PRIN2 " free. ")
               NIL)
              (TERPRI)))
            (T NIL)))
          (T
           (PROGN
            (SETQ H PRINT_)
            (SETQ PRINT_ 50)
            (SETQ SYMCON_S
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A SYMCON_S)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (A) (LIST '*SQ A T)) (CAR A))
                                       NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (A) (LIST '*SQ A T)) (CAR A))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "which still ")
                (PRIN2 (COND (N "have") (T "has")))
                (PRIN2 " to satisfy: ")
                NIL)
               (TERPRI)
               (DEPRINT SYMCON_S)
               NIL))
             (T
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "which ")
                (PRIN2 (COND (N "have") (T "has")))
                (PRIN2 " to satisfy conditions. To see them set ")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "lisp(print_= max. number of terms of an equation to print);")
                NIL)
               (TERPRI)
               NIL)))
            (SETQ PRINT_ H))))
         (SETQ RETURN_LIST
                 (CONS
                  (LIST 'LIST (CONS 'LIST SYMCON_S)
                        (CONS 'LIST
                              (APPEND PARALIST (APPEND XILIST_S ETALIST_S)))
                        (CONS 'LIST (APPEND FLIST_SLIN FLIST_SNLI)))
                  RETURN_LIST))
         (NODEPENDLIST YLIST))
        (GO WHILELABEL))
      (TERPRI)
      (COND
       ((AND (EQUAL N1 0) (NULL SYMCON_S))
        (PROGN
         (COND
          ((EQUAL (LENGTH EQLIST) 1)
           (PROGN (PRIN2 "The equation has no symmetry.") NIL))
          (T (PROGN (PRIN2 "The equations have no symmetry.") NIL)))
         (TERPRI))))
      (PROGN (PRIN2 "-------- ") NIL)
      (TERPRI)
      (PROG (E1)
        (SETQ E1 YLIST)
       LAB
        (COND ((NULL E1) (RETURN NIL)))
        ((LAMBDA (E1) (DEPND E1 (LIST XLIST))) (CAR E1))
        (SETQ E1 (CDR E1))
        (GO LAB))
      (RECOVER_REDUCE_FLAGS)
      (SETQ COLLECT_SOL OLDCOLLECT_SOL)
      (SETQ ADJUST_FNC OLDADJ)
      (RETURN (CONS 'LIST RETURN_LIST)))) 
(ENDMODULE) 