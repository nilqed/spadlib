(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SEPARATION)) 
(PUT 'GET_SEPAR_PDE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_SEPAR_PDE 'DEFINED-ON-LINE '33) 
(PUT 'GET_SEPAR_PDE 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'GET_SEPAR_PDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_SEPAR_PDE (PDES)
    (PROG (P M)
      (SETQ M (MINUS 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (COND
          ((AND
            (OR (FLAGP (CAR PDES) 'TO_SEP)
                (AND FORCE_SEP (FLAGP (CAR PDES) 'TO_CASESEP)))
            (GET (CAR PDES) 'STARDE) (ZEROP (CAAR (GET (CAR PDES) 'STARDE)))
            (OR (NULL P) (LESSP (GET (CAR PDES) 'NVARS) M)))
           (PROGN (SETQ P (CAR PDES)) (SETQ M (GET P 'NVARS)))))
         (SETQ PDES (CDR PDES))
         NIL)
        (GO WHILELABEL))
      (RETURN P))) 
(PUT 'SEPARATE 'NUMBER-OF-ARGS 2) 
(PUT 'SEPARATE 'DEFINED-ON-LINE '56) 
(PUT 'SEPARATE 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SEPARATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEPARATE (DE PDES)
    (COND
     ((AND (OR (FLAGP DE 'TO_SEP) (AND FORCE_SEP (FLAGP DE 'TO_CASESEP)))
           (GET DE 'STARDE))
      (PROG (L)
        (COND
         ((SETQ L (SPLITSQ DE))
          (RETURN
           (PROGN
            (SETQ L
                    (MKEQSQLIST
                     (PROG (A FORALL-RESULT FORALL-ENDPTR)
                       (SETQ A L)
                       (COND ((NULL A) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (A) (CONS A 1)) (CAR A))
                                             NIL)))
                      LOOPLABEL
                       (SETQ A (CDR A))
                       (COND ((NULL A) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (A) (CONS A 1)) (CAR A)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     NIL NIL (GET DE 'FCTS) (GET DE 'VARS)
                     (DELETE 'TO_SEP ALLFLAGS_) T (GET DE 'ORDERINGS) PDES))
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "Separation of ")
                (PRIN2 DE)
                (PRIN2 " yields ")
                (PRIN2 L)
                NIL)
               (TERPRI))))
            L))))
        (CP_SQ2P_VAL DE)
        (SETQ L
                (SEPAR (GET DE 'PVAL) (GET DE 'FCTS) (GET DE 'VARS)
                 (GET DE 'NONRATIONAL) (COND (FORCE_SEP PDES) (T NIL))))
        (COND
         ((EQUAL L 1)
          (PROGN
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 "Separation of ")
               (PRIN2 DE)
               (PRIN2 " leads to one or more case distinctions.")
               NIL)
              (TERPRI))))
           (RETURN 1)))
         ((OR (GREATERP (LENGTH L) 1)
              (AND (EQUAL (LENGTH L) 1) (NEQ (CAAR L) 1)))
          (PROGN
           (SETQ L
                   (MKEQSQLIST NIL NIL
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A L)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (A) (CDR A)) (CAR A))
                                            NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    (GET DE 'FCTS) (GET DE 'VARS) (DELETE 'TO_SEP ALLFLAGS_) T
                    (GET DE 'ORDERINGS) PDES))
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 "Separation of ")
               (PRIN2 DE)
               (PRIN2 " yields ")
               (PRIN2 L)
               NIL)
              (TERPRI))))
           (RETURN L)))
         (T
          (PROGN
           (REMFLAG (LIST DE) 'TO_SEP)
           (COND (FORCE_SEP (REMFLAG (LIST DE) 'TO_CASESEP)))))))))) 
(PUT 'SPLITSQ 'NUMBER-OF-ARGS 1) 
(PUT 'SPLITSQ 'DEFINED-ON-LINE '101) 
(PUT 'SPLITSQ 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SPLITSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLITSQ (DE)
    (COND ((NEQ 0 (CAAR (GET DE 'STARDE))) NIL)
          (T
           (PROG (SPLITVAR FN NRK SV V NRKCP KE K)
             (SETQ SV (GET DE 'STARDE))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND SV (ZEROP (CAAR SV)))) (RETURN NIL)))
               (PROGN
                (SETQ SPLITVAR (CONS (CDAR SV) SPLITVAR))
                (SETQ SV (CDR SV)))
               (GO WHILELABEL))
             (SETQ FN (GET DE 'FCTS))
             (SETQ NRK (GET DE 'NON_RAT_KERN))
             (COND
              (NRK
               (PROGN
                (SETQ SV NIL)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT SPLITVAR) (RETURN NIL)))
                  (PROGN
                   (SETQ V (CAR SPLITVAR))
                   (SETQ SPLITVAR (CDR SPLITVAR))
                   (SETQ NRKCP NRK)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT
                        (AND NRKCP
                             (OR (MY_FREEOF (CAR NRKCP) V)
                                 (FREEOFLIST (CAR NRKCP) FN))))
                       (RETURN NIL)))
                     (SETQ NRKCP (CDR NRKCP))
                     (GO WHILELABEL))
                   (COND ((NULL NRKCP) (SETQ SV (CONS V SV)))))
                  (GO WHILELABEL))
                (SETQ SPLITVAR SV))))
             (COND ((NULL SPLITVAR) (RETURN NIL)))
             (SETQ KE (GET DE 'KERN))
             (PROG (K)
               (SETQ K KE)
              LAB
               (COND ((NULL K) (RETURN NIL)))
               ((LAMBDA (K)
                  (COND
                   ((AND (NOT (FREEOFLIST K SPLITVAR)) (FREEOFLIST K FN))
                    (SETQ SPLITVAR (UNION (LIST K) SPLITVAR)))))
                (CAR K))
               (SETQ K (CDR K))
               (GO LAB))
             (COND ((CDR SPLITVAR) (SETQ SPLITVAR (KERNEL_SORT SPLITVAR))))
             (SETQ K (SETKORDER SPLITVAR))
             (SETQ SV (ITERCOEFF (REORDER (CAR (GET DE 'SQVAL))) SPLITVAR))
             (SETKORDER K)
             (RETURN SV))))) 
(PUT 'TERMSEP 'NUMBER-OF-ARGS 3) 
(PUT 'TERMSEP 'DEFINED-ON-LINE '145) 
(PUT 'TERMSEP 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'TERMSEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMSEP (A X NONRAT)
    (PROG (L P Q SIG L1 L2 CARL)
      (COND ((MY_FREEOF A X) (SETQ L (LIST 1 A)))
            ((ATOM A) (SETQ L (LIST A 1)))
            (T
             (PROGN
              (COND
               ((EQUAL (CAR A) 'MINUS)
                (PROGN (SETQ A (CADR A)) (SETQ SIG (NOT SIG)))))
              (COND ((AND (PAIRP A) (EQUAL (CAR A) 'TIMES)) (SETQ L (CDR A)))
                    (T (SETQ L (LIST A))))
              (SETQ P NIL)
              (SETQ Q NIL)
              (PROG ()
               WHILELABEL
                (COND ((NOT L) (RETURN NIL)))
                (PROGN
                 (COND ((MY_FREEOF (CAR L) X) (SETQ Q (CONS (CAR L) Q)))
                       (T
                        (PROGN
                         (SETQ CARL (CAR L))
                         (COND
                          ((AND (PAIRP CARL) (EQUAL (CAR CARL) 'SQRT))
                           (SETQ CARL
                                   (LIST 'EXPT (CADR CARL)
                                         (LIST 'QUOTIENT 1 2)))))
                         (COND
                          ((AND (PAIRP CARL) (EQUAL (CAR CARL) 'EXPT)
                                (MY_FREEOF (CADR CARL) X) (PAIRP (CADDR CARL))
                                (EQUAL (CAR (CADDR CARL)) 'PLUS))
                           (PROGN
                            (PROG (S)
                              (SETQ S (CDR (CADDR CARL)))
                             LAB
                              (COND ((NULL S) (RETURN NIL)))
                              ((LAMBDA (S)
                                 (COND ((MY_FREEOF S X) (SETQ L1 (CONS S L1)))
                                       (T (SETQ L2 (CONS S L2)))))
                               (CAR S))
                              (SETQ S (CDR S))
                              (GO LAB))
                            (COND
                             (L1
                              (PROGN
                               (COND ((CDR L1) (SETQ L1 (CONS 'PLUS L1)))
                                     (T (SETQ L1 (CAR L1))))
                               (SETQ Q (CONS (LIST 'EXPT (CADR CARL) L1) Q)))))
                            (COND ((AND L2 (CDR L2)) (SETQ L2 (CONS 'PLUS L2)))
                                  (T (SETQ L2 (CAR L2))))
                            (SETQ P (CONS (LIST 'EXPT (CADR CARL) L2) P))))
                          (T (SETQ P (CONS CARL P))))
                         NIL)))
                 (SETQ L (CDR L)))
                (GO WHILELABEL))
              (COND
               (P
                (COND
                 ((AND (NULL FORCE_SEP) (NOT (FREEOFLIST P NONRAT)))
                  (PROGN (SETQ P (SETQ Q (SETQ L NIL)))))
                 (T
                  (SETQ P
                          (COND ((GREATERP (LENGTH P) 1) (CONS 'TIMES P))
                                (T (CAR P))))))))
              (COND
               (Q
                (COND ((GREATERP (LENGTH Q) 1) (SETQ Q (CONS 'TIMES Q)))
                      (T (SETQ Q (CAR Q))))))
              (COND
               ((OR P Q)
                (COND
                 ((NULL P)
                  (COND (SIG (SETQ L (LIST 1 (LIST 'MINUS Q))))
                        (T (SETQ L (LIST 1 Q)))))
                 (Q
                  (COND (SIG (SETQ L (LIST P (LIST 'MINUS Q))))
                        (T (SETQ L (LIST P Q)))))
                 (SIG (SETQ L (LIST P (LIST 'MINUS 1))))
                 (T (SETQ L (LIST P 1)))))))))
      (RETURN L))) 
(PUT 'SUMSEP 'NUMBER-OF-ARGS 4) 
(PUT 'SUMSEP 'DEFINED-ON-LINE '201) 
(PUT 'SUMSEP 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SUMSEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUMSEP (L X NONRAT PDES)
    (PROG (CL P Q S)
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (COND
         ((AND (PAIRP (CAR L)) (EQUAL (CAAR L) 'QUOTIENT))
          (PROGN
           (SETQ P (TERMSEP (CADAR L) X NONRAT))
           (COND ((NOT Q) (SETQ Q (TERMSEP (CADDAR L) X NONRAT))))
           (COND
            ((AND P Q)
             (PROGN
              (SETQ L (CDR L))
              (COND ((EQUAL (CAR Q) 1) (SETQ S (CAR P)))
                    (T (SETQ S (LIST 'QUOTIENT (CAR P) (CAR Q)))))
              (COND ((EQUAL (CADR Q) 1) (SETQ P (LIST S (CADR P))))
                    (T (SETQ P (LIST S (LIST 'QUOTIENT (CADR P) (CADR Q))))))
              (SETQ CL (TERMSORT CL P))))
            (T (PROGN (SETQ L NIL) (SETQ CL NIL))))))
         (T
          (PROGN
           (SETQ P (TERMSEP (CAR L) X NONRAT))
           (COND (P (PROGN (SETQ L (CDR L)) (SETQ CL (TERMSORT CL P))))
                 (T (PROGN (SETQ L NIL) (SETQ CL NIL)))))))
        (GO WHILELABEL))
      (COND
       ((AND CL (GREATERP (LENGTH CL) 1))
        (COND
         ((AND NONRAT
               (PROGN
                (SETQ S CL)
                (PROG ()
                 WHILELABEL
                  (COND
                   ((NOT (AND S (FREEOFLIST (CAAR S) NONRAT))) (RETURN NIL)))
                  (SETQ S (CDR S))
                  (GO WHILELABEL))
                S))
          (PROGN
           (COND
            ((AND (PAIRP (CAAR CL)) (EQUAL (CAAAR CL) 'QUOTIENT))
             (SETQ L
                     (PROG (S FORALL-RESULT FORALL-ENDPTR)
                       (SETQ S CL)
                       (COND ((NULL S) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (S) (CADAR S)) (CAR S))
                                             NIL)))
                      LOOPLABEL
                       (SETQ S (CDR S))
                       (COND ((NULL S) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (S) (CADAR S)) (CAR S)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T
             (SETQ L
                     (PROG (S FORALL-RESULT FORALL-ENDPTR)
                       (SETQ S CL)
                       (COND ((NULL S) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (S) (CAR S)) (CAR S))
                                             NIL)))
                      LOOPLABEL
                       (SETQ S (CDR S))
                       (COND ((NULL S) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (S) (CAR S)) (CAR S)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
           (SETQ S (EQUALITY_ASSUMPTION L X NONRAT PDES))
           (COND ((NULL S) (SETQ CL NIL)) ((NEQ S 1) (SETQ CL 1)))))
         (T
          (PROGN
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 "separation w.r.t. ") NIL)
              (FCTPRINT (LIST X))
              (PROGN (PRIN2 " . ") NIL))))
           (COND
            ((AND (PAIRP (CAAR CL)) (EQUAL (CAAAR CL) 'QUOTIENT))
             (SETQ L
                     (PROG (S FORALL-RESULT FORALL-ENDPTR)
                       (SETQ S CL)
                       (COND ((NULL S) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (S) (CADAR S)) (CAR S))
                                             NIL)))
                      LOOPLABEL
                       (SETQ S (CDR S))
                       (COND ((NULL S) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (S) (CADAR S)) (CAR S)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (T
             (SETQ L
                     (PROG (S FORALL-RESULT FORALL-ENDPTR)
                       (SETQ S CL)
                       (COND ((NULL S) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (S) (CAR S)) (CAR S))
                                             NIL)))
                      LOOPLABEL
                       (SETQ S (CDR S))
                       (COND ((NULL S) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (S) (CAR S)) (CAR S)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
           (COND ((NOT (LINEARINDEPTEST L (LIST X))) (SETQ CL NIL))))))))
      (RETURN CL))) 
(PUT 'EXPON 'NUMBER-OF-ARGS 2) 
(PUT 'EXPON 'DEFINED-ON-LINE '259) 
(PUT 'EXPON 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'EXPON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPON (H X)
    (COND ((EQUAL H 1) 0) ((EQUAL H X) 1)
          ((AND (PAIRP H) (EQUAL (CAR H) 'EXPT) (EQUAL (CADR H) X)) (CADDR H))
          ((AND (PAIRP H) (EQUAL (CAR H) 'TIMES))
           (PROG (G S K)
             (SETQ S
                     (PROG (G FORALL-RESULT FORALL-ENDPTR)
                       (SETQ G (CDR H))
                       (COND ((NULL G) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (G) (EXPON G X)) (CAR G))
                                             NIL)))
                      LOOPLABEL
                       (SETQ G (CDR G))
                       (COND ((NULL G) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (G) (EXPON G X)) (CAR G)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (PROG (G)
               (SETQ G S)
              LAB
               (COND ((NULL G) (RETURN NIL)))
               ((LAMBDA (G) (COND ((NULL G) (SETQ K T)))) (CAR G))
               (SETQ G (CDR G))
               (GO LAB))
             (RETURN (COND (K NIL) (T (CONS 'PLUS S))))))
          (T NIL))) 
(PUT 'EQUALITY_ASSUMPTION 'NUMBER-OF-ARGS 4) 
(PUT 'EQUALITY_ASSUMPTION 'DEFINED-ON-LINE '272) 
(PUT 'EQUALITY_ASSUMPTION 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'EQUALITY_ASSUMPTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQUALITY_ASSUMPTION (L X NONRAT PDES)
    (PROG (A EA B EB LCP H S PRINT_BAK IQ NR)
      (SETQ S 1)
      (COND
       ((PAIRP L)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (CDR L) (OR (NULL S) (EQUAL S 1)))) (RETURN NIL)))
          (PROGN
           (SETQ H L)
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND H (FREEOFLIST (CAR H) NONRAT))) (RETURN NIL)))
             (SETQ H (CDR H))
             (GO WHILELABEL))
           (COND ((NULL H) (SETQ L (LIST 1)))
                 (T
                  (PROGN
                   (SETQ A (CAR H))
                   (SETQ L (DELETE A L))
                   (SETQ EA (EXPON A X))
                   (COND ((NULL EA) (SETQ S NIL))
                         (T
                          (PROGN
                           (SETQ LCP L)
                           (PROG ()
                            WHILELABEL
                             (COND
                              ((NOT (AND LCP (OR (NULL S) (EQUAL S 1))))
                               (RETURN NIL)))
                             (PROGN
                              (SETQ B (CAR LCP))
                              (SETQ LCP (CDR LCP))
                              (SETQ EB (EXPON B X))
                              (COND
                               ((NULL EB)
                                (COND ((FREEOFLIST B NONRAT) NIL)
                                      (T (SETQ S NIL))))
                               (T
                                (PROGN
                                 (SETQ H (REVAL1 (LIST 'DIFFERENCE EA EB) T))
                                 (SETQ PRINT_BAK PRINT_)
                                 (SETQ PRINT_ NIL)
                                 (SETQ H
                                         (SIMPLIFYPDESQ (SIMP H) NONRAT T NIL
                                          NIL))
                                 (SETQ PRINT_ PRINT_BAK)
                                 (COND
                                  ((FREEOFLIST (CAR H) NONRAT)
                                   (SETQ CONTRADICTION_ T)))
                                 (COND
                                  (CONTRADICTION_ (SETQ CONTRADICTION_ NIL))
                                  ((FOLLOWS_FROMSQ
                                    (COND ((PAIRP (CDR H)) (CDR H))
                                          (T (LIST (CAR H))))
                                    PDES)
                                   (PROGN
                                    (SETQ S NIL)
                                    (SETQ L (LIST 1))
                                    (SETQ LCP NIL)))
                                  (T
                                   (PROGN
                                    (SETQ H (CAR H))
                                    (SETQ S NIL)
                                    (SETQ NR NONRAT)
                                    (PROG ()
                                     WHILELABEL
                                      (COND
                                       ((NOT (AND NR (NULL S))) (RETURN NIL)))
                                      (PROGN
                                       (SETQ S
                                               (ERR_CATCH_SOLVE
                                                (LIST 'LIST (LIST '*SQ H T))
                                                (LIST 'LIST (CAR NR))))
                                       (COND
                                        (S
                                         (PROGN
                                          (SETQ IQ INEQ_)
                                          (PROG ()
                                           WHILELABEL
                                            (COND
                                             ((NOT
                                               (AND IQ
                                                    (NOT
                                                     (ZEROP
                                                      (REVAL1
                                                       (SUBST (CADDAR S)
                                                              (CADAR S)
                                                              (PREPSQ
                                                               (CAR IQ)))
                                                       T)))))
                                              (RETURN NIL)))
                                            (SETQ IQ (CDR IQ))
                                            (GO WHILELABEL))
                                          (COND (IQ (SETQ S NIL))))))
                                       (COND ((NULL S) (SETQ NR (CDR NR)))))
                                      (GO WHILELABEL))
                                    (COND
                                     (S
                                      (SETQ TO_DO_LIST
                                              (CONS (LIST 'SPLIT_INTO_CASES H)
                                                    TO_DO_LIST)))))))))))
                             (GO WHILELABEL)))))))))
          (GO WHILELABEL))))
      (RETURN S))) 
(PUT 'LINEARINDEPTEST 'NUMBER-OF-ARGS 2) 
(PUT 'LINEARINDEPTEST 'DEFINED-ON-LINE '357) 
(PUT 'LINEARINDEPTEST 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'LINEARINDEPTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINEARINDEPTEST (L VL)
    (PROG (L1 FLAG PRINT_BAK)
      (SETQ L1 L)
      (SETQ FLAG T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FLAG (PAIRP L1))) (RETURN NIL)))
        (COND ((FREEOFLIST (CAR L1) VL) (SETQ L1 (CDR L1)))
              ((MEMBER (CAR L1) VL) (SETQ L1 (CDR L1)))
              ((AND (PAIRP (CAR L1)) (EQUAL (CAAR L1) 'EXPT)
                    (NUMBERP (CADDAR L1)) (MEMBER (CADAR L1) VL))
               (SETQ L1 (CDR L1)))
              (T (SETQ FLAG NIL)))
        (GO WHILELABEL))
      (COND
       ((NOT FLAG)
        (PROGN
         (COND
          (INDEPENDENCE_ (PROGN (SETQ PRINT_BAK PRINT_) (SETQ PRINT_ 10000))))
         (COND
          (PRINT_
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "linear independent expressions : ") NIL))))
         (PROG (X)
           (SETQ X L)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X) (EQPRINT X)) (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (COND
          (INDEPENDENCE_
           (PROGN
            (COND
             ((YESP "Are the expressions linear independent? ") (SETQ FLAG T))
             (T (SETQ FLAG NIL)))
            (SETQ PRINT_ PRINT_BAK)))
          (T (SETQ FLAG T))))))
      (RETURN FLAG))) 
(PUT 'TERMSORT 'NUMBER-OF-ARGS 2) 
(PUT 'TERMSORT 'DEFINED-ON-LINE '380) 
(PUT 'TERMSORT 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'TERMSORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TERMSORT (CL P)
    (COND ((NULL CL) (LIST P))
          ((EQUAL (CAAR CL) (CAR P))
           (CONS (CONS (CAR P) (CONS (CADR P) (CDAR CL))) (CDR CL)))
          (T (CONS (CAR CL) (TERMSORT (CDR CL) P))))) 
(PUT 'EQSEP 'NUMBER-OF-ARGS 4) 
(PUT 'EQSEP 'DEFINED-ON-LINE '389) 
(PUT 'EQSEP 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'EQSEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EQSEP (EQL FTEM NONRAT PDES)
    (PROG (VLIST1 VLIST2 A X L EQL1)
      (PROG ()
       WHILELABEL
        (COND ((NOT EQL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR EQL))
         (SETQ VLIST1 (CADR A))
         (SETQ VLIST2 (CADDR A))
         (SETQ EQL (CDR EQL))
         (COND ((NULL VLIST2) (SETQ EQL1 (CONS A EQL1)))
               (T
                (PROGN
                 (SETQ X (CAR VLIST2))
                 (SETQ VLIST2 (CDR VLIST2))
                 (COND
                  ((MY_FREEOF (CDAR A) X)
                   (COND
                    (VLIST2 (SETQ EQL (CONS (LIST (CAR A) VLIST1 VLIST2) EQL)))
                    (T (SETQ EQL1 (CONS A EQL1)))))
                  ((MEMBER X (ARGSET (SMEMBERL FTEM (LIST (CDAR A)))))
                   (SETQ EQL (CONS (LIST (CAR A) (CONS X VLIST1) VLIST2) EQL)))
                  (T
                   (PROGN
                    (SETQ L (SUMSEP (CDAR A) X NONRAT PDES))
                    (COND ((EQUAL L 1) (PROGN (SETQ EQL NIL) (SETQ EQL1 1)))
                          (L
                           (COND
                            ((OR VLIST1 VLIST2)
                             (SETQ EQL
                                     (APPEND
                                      (VARAPP L (CAAR A) NIL
                                       (APPEND VLIST2 VLIST1))
                                      EQL)))
                            (T
                             (SETQ EQL1
                                     (APPEND (VARAPP L (CAAR A) NIL NIL)
                                             EQL1)))))
                          (VLIST2
                           (SETQ EQL
                                   (CONS (LIST (CAR A) (CONS X VLIST1) VLIST2)
                                         EQL)))
                          (T (SETQ EQL1 (CONS A EQL1)))))))))))
        (GO WHILELABEL))
      (RETURN EQL1))) 
(PUT 'VARAPP 'NUMBER-OF-ARGS 4) 
(PUT 'VARAPP 'DEFINED-ON-LINE '433) 
(PUT 'VARAPP 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'VARAPP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARAPP (L A V1 V2)
    (COND ((NULL L) NIL)
          (T
           (CONS (LIST (CONS (CONS (CAAR L) A) (CDAR L)) V1 V2)
                 (VARAPP (CDR L) A V1 V2))))) 
(PUT 'SEP 'NUMBER-OF-ARGS 5) 
(PUT 'SEP 'DEFINED-ON-LINE '439) 
(PUT 'SEP 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEP (P FTEM VARL NONRAT PDES)
    (PROG (EQL EQLIST A Q)
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT))
        (PROGN
         (SETQ Q (CDR (ERR_CATCH_FAC (CADDR P))))
         (COND ((GREATERP (LENGTH Q) 1) (SETQ Q (CONS 'TIMES Q)))
               (T (SETQ Q (CAR Q))))
         (SETQ P (CADR P)))))
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'PLUS))
        (SETQ A
                (CONS NIL
                      (COND ((NOT Q) (CDR P))
                            (T
                             (PROG (B FORALL-RESULT FORALL-ENDPTR)
                               (SETQ B (CDR P))
                               (COND ((NULL B) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (B)
                                                   (LIST 'QUOTIENT B Q))
                                                 (CAR B))
                                                NIL)))
                              LOOPLABEL
                               (SETQ B (CDR B))
                               (COND ((NULL B) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (B) (LIST 'QUOTIENT B Q))
                                         (CAR B))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))))
       ((NOT Q) (SETQ A (LIST NIL P)))
       (T (SETQ A (LIST NIL (LIST 'QUOTIENT P Q)))))
      (SETQ EQL (LIST (LIST A NIL VARL)))
      (SETQ EQL (EQSEP EQL FTEM NONRAT PDES))
      (COND ((EQUAL EQL 1) (RETURN 1)))
      (PROG ()
       WHILELABEL
        (COND ((NOT EQL) (RETURN NIL)))
        (PROGN
         (SETQ A (CAAR EQL))
         (COND ((CDDR A) (SETQ A (CONS (CAR A) (CONS 'PLUS (CDR A)))))
               (T (SETQ A (CONS (CAR A) (CADR A)))))
         (COND
          ((CAR A)
           (COND ((CDAR A) (SETQ A (CONS (CONS 'TIMES (CAR A)) (CDR A))))
                 (T (SETQ A (CONS (CAAR A) (CDR A))))))
          (T (SETQ A (CONS 1 (CDR A)))))
         (SETQ EQLIST (CONS A EQLIST))
         (SETQ EQL (CDR EQL)))
        (GO WHILELABEL))
      (RETURN EQLIST))) 
(PUT 'SEPAR2 'NUMBER-OF-ARGS 3) 
(PUT 'SEPAR2 'DEFINED-ON-LINE '484) 
(PUT 'SEPAR2 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SEPAR2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEPAR2 (P FTEM VARL)
    (PROG (EQLIST)
      (COND
       ((AND P (NOT (ZEROP P)))
        (COND
         ((NOT
           (AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT)
                (INTERSECTION (ARGSET (SMEMBERL FTEM (CADR P))) VARL)))
          (PROGN
           (SETQ EQLIST (SEP P FTEM VARL NIL NIL))
           (COND
            ((PAIRP EQLIST)
             (SETQ EQLIST (UNION (CDR EQLIST) (LIST (CAR EQLIST))))))
           NIL)))))
      (RETURN EQLIST))) 
(PUT 'SEPAR 'NUMBER-OF-ARGS 5) 
(PUT 'SEPAR 'DEFINED-ON-LINE '505) 
(PUT 'SEPAR 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SEPAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEPAR (P FTEM VARL NONRAT PDES)
    (PROG (EQL EQLIST A B L S)
      (COND ((OR (NULL P) (ZEROP P)) (SETQ EQLIST (LIST (CONS 0 0))))
            ((AND (PAIRP P) (EQUAL (CAR P) 'QUOTIENT)
                  (INTERSECTION (ARGSET (SMEMBERL FTEM (CADDR P))) VARL))
             (SETQ EQLIST (LIST (CONS 1 P))))
            (T
             (PROGN
              (COND
               ((AND (PAIRP P) (EQUAL (CAR P) 'TIMES)) (SETQ P (REVAL1 P T))))
              (SETQ EQLIST (SEP P FTEM VARL NONRAT PDES))
              (COND
               ((NEQ EQLIST 1)
                (PROGN
                 (COND
                  (EQLIST (SETQ EQL (UNION (CDR EQLIST) (LIST (CAR EQLIST))))))
                 (SETQ EQLIST NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT EQL) (RETURN NIL)))
                   (PROGN
                    (SETQ A (CAR EQL))
                    (SETQ L (SETQ EQL (CDR EQL)))
                    (PROG (B)
                      (SETQ B L)
                     LAB
                      (COND ((NULL B) (RETURN NIL)))
                      ((LAMBDA (B)
                         (PROGN
                          (SETQ S (REVAL1 (LIST 'QUOTIENT (CDR B) (CDR A)) T))
                          (COND
                           ((NOT (SMEMBERL (APPEND VARL FTEM) S))
                            (PROGN
                             (SETQ EQL (DELETE B EQL))
                             (SETQ A
                                     (CONS
                                      (REVAL1
                                       (LIST 'PLUS (CAR A)
                                             (LIST 'TIMES S (CAR B)))
                                       T)
                                      (CDR A))))))))
                       (CAR B))
                      (SETQ B (CDR B))
                      (GO LAB))
                    (SETQ EQLIST (CONS A EQLIST)))
                   (GO WHILELABEL))))))))
      (RETURN EQLIST))) 
(PUT 'ITERCOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'ITERCOEFF 'DEFINED-ON-LINE '548) 
(PUT 'ITERCOEFF 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'ITERCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ITERCOEFF (SF SPLITVAR)
    (COND ((NOT (PAIRP SF)) (LIST SF))
          ((OR (EQUAL (CAR SF) '|:RN:|) (EQUAL (CAR SF) '|:GI:|)) (LIST SF))
          ((NOT (MEMBER (CAAAR SF) SPLITVAR)) (LIST SF))
          (T
           (NCONC (ITERCOEFF (CDAR SF) SPLITVAR)
                  (ITERCOEFF (CDR SF) SPLITVAR))))) 
(PUT 'SPLIT_SIMP 'PSOPFN 'SPLIT_SIMPLIFY) 
(PUT 'SPLIT_SIMPLIFY 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT_SIMPLIFY 'DEFINED-ON-LINE '559) 
(PUT 'SPLIT_SIMPLIFY 'DEFINED-IN-FILE 'CRACK/CRSEP.RED) 
(PUT 'SPLIT_SIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT_SIMPLIFY (INP)
    (PROG (LE K H P Q INEQ_BAK W NOPOWERSBAK M R S A INDEPVAR SPLITVAR EQNS INE
           FLL CPU TO_SIMPLIFY MAX_GC_FAC_BAK)
      (COND (*TIME (SETQ CPU (TIME))))
      (COND
       ((NEQ (LENGTH INP) 5)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "SPLIT_SIMPLIFY DOES NOT HAVE 5 ARGUMENTS.") NIL)
         (TERPRI)
         (PROGN (PRIN2 "SOME PARAMETER IS MISSING OR IS TOO MUCH.") NIL)
         NIL)))
      (COND
       (PRINT_
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Start of splitting equations") NIL)
         (TERPRI)
         NIL)))
      (SETQ EQNS (CDR (REVAL1 (CAR INP) NIL)))
      (SETQ INE (CDR (REVAL1 (CADR INP) NIL)))
      (SETQ FLL (CDR (REVAL1 (CADDR INP) T)))
      (SETQ INDEPVAR (CDR (REVAL1 (CADDDR INP) T)))
      (SETQ TO_SIMPLIFY (REVAL1 (CAR (CDDDDR INP)) T))
      (SETQ Q 1)
      (PROG (P)
        (SETQ P EQNS)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (COND
             ((AND (NOT (FIXP P)) (OR (NOT (PAIRP P)) (NEQ (CAR P) '*SQ)))
              (PROGN
               (PROGN
                (PRIN2 "THE ")
                (PRIN2 Q)
                (PRIN2
                 ". ELEMENT OF THE INPUT LIST OF EQUATIONS IS NOT IN STANDARD")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "quotient FORM! THIS MAY HAVE BEEN CAUSED BY USING COMMANDS LIKE cons")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "IN ALGEBRAIC MODE. IN THAT CASE, USE sqcons, sqrest, sqfirst, sqsecond,")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "sqthird, sqpart INSTEAD.") NIL)
               (TERPRI)
               NIL)))
            (SETQ Q (ADD1 Q))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       (*TIME
        (PROGN
         (PROGN
          (PRIN2 (QUOTIENT (DIFFERENCE (SETQ K (TIME)) CPU) 1000))
          (PRIN2 " s: Determination of the splitting variables")
          NIL)
         (TERPRI)
         (SETQ CPU K))))
      (SETQ SPLITVAR NIL)
      (SETQ Q NIL)
      (PROG (H)
        (SETQ H INDEPVAR)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((MY_FREEOF FLL H)
             (PROGN
              (SETQ SPLITVAR (UNION (LIST H) SPLITVAR))
              (PROG (P)
                (SETQ P DEPL*)
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P)
                   (COND
                    ((AND (FREEOF FLL (CAR P)) (NOT (FREEOF (CDR P) H)))
                     (SETQ Q (UNION (LIST (CAR P)) Q)))))
                 (CAR P))
                (SETQ P (CDR P))
                (GO LAB))
              NIL))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (COND
       (Q
        (PROGN
         (SETQ K NIL)
         (PROG (P)
           (SETQ P EQNS)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((AND (PAIRP P) (EQUAL (CAR P) '*SQ))
                (SETQ K (UNION (KERNELS (CAR (CADR P))) K)))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (PROG (P)
           (SETQ P K)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((AND (OR (NOT (FREEOFLIST P Q)) (NOT (FREEOFLIST P SPLITVAR)))
                     (MY_FREEOF FLL P))
                (SETQ SPLITVAR (UNION (LIST P) SPLITVAR)))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB)))))
      (COND
       ((NULL SPLITVAR)
        (COND ((NULL TO_SIMPLIFY) (RETURN (CONS 'LIST EQNS)))
              (T
               (PROGN
                (SETQ H
                        (PROG (P FORALL-RESULT FORALL-ENDPTR)
                          (SETQ P EQNS)
                          (COND ((NULL P) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (P)
                                              (COND ((PAIRP P) (CAR (CADR P)))
                                                    (T P)))
                                            (CAR P))
                                           NIL)))
                         LOOPLABEL
                          (SETQ P (CDR P))
                          (COND ((NULL P) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P)
                                      (COND ((PAIRP P) (CAR (CADR P))) (T P)))
                                    (CAR P))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ LE (LENGTH H))))))
       (T
        (PROGN
         (COND ((CDR SPLITVAR) (SETQ SPLITVAR (KERNEL_SORT SPLITVAR))))
         (COND
          ((NOT (LINEARINDEPTEST SPLITVAR INDEPVAR))
           (RETURN (CONS 'LIST EQNS))))
         (SETQ K (SEARCH_LI2 EQNS 'EXPT))
         (SETQ H SPLITVAR)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND H (MY_FREEOF K (CAR H)))) (RETURN NIL)))
           (SETQ H (CDR H))
           (GO WHILELABEL))
         (COND (H (RETURN (CONS 'LIST EQNS))))
         (COND
          (*TIME
           (PROGN
            (PROGN
             (PRIN2 (QUOTIENT (DIFFERENCE (SETQ K (TIME)) CPU) 1000))
             (PRIN2 " s: Variables to be used for splitting: ")
             (PRIN2 SPLITVAR)
             NIL)
            (TERPRI)
            (SETQ CPU K)
            (PROGN
             (PRIN2 (QUOTIENT (DIFFERENCE (SETQ K (TIME)) CPU) 1000))
             (PRIN2 " s: Start of splitting equations")
             NIL)
            (TERPRI)
            (SETQ CPU K))))
         (SETQ K (SETKORDER SPLITVAR))
         (SETQ H
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P EQNS)
                  STARTOVER
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (P)
                              (ITERCOEFF
                               (COND ((NOT (PAIRP P)) P)
                                     ((EQUAL (CAR P) '*SQ)
                                      (REORDER (CAR (CADR P))))
                                     (T (CAR (SIMP P))))
                               SPLITVAR))
                            (CAR P)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ P (CDR P))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (P)
                              (ITERCOEFF
                               (COND ((NOT (PAIRP P)) P)
                                     ((EQUAL (CAR P) '*SQ)
                                      (REORDER (CAR (CADR P))))
                                     (T (CAR (SIMP P))))
                               SPLITVAR))
                            (CAR P)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ P (CDR P))
                   (GO LOOPLABEL)))
         (SETKORDER K)
         (SETQ LE (LENGTH H))
         (COND
          (PRINT_
           (PROGN
            (PROGN (PRIN2 LE) (PRIN2 " equations result") NIL)
            (TERPRI)))))))
      (COND
       (TO_SIMPLIFY
        (PROGN
         (COND
          (*TIME
           (PROGN
            (PROGN
             (PRIN2 (QUOTIENT (DIFFERENCE (SETQ K (TIME)) CPU) 1000))
             (PRIN2 " s: Now simplifying equations and dropping multiple ones")
             NIL)
            (TERPRI)
            (SETQ CPU K)
            NIL)))
         (SETQ W NIL)
         (SETQ NOPOWERSBAK *NOPOWERS)
         (AEVAL (OFF (LIST 'NOPOWERS)))
         (SETQ EQNS NIL)
         (SETQ INEQ_BAK INEQ_)
         (SETQ INEQ_
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A INE)
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (CADR A)) (CAR A)) NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (A) (CADR A)) (CAR A)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ M NIL)
         (SETQ R 0)
         (SETQ MAX_GC_FAC_BAK MAX_GC_FAC)
         (SETQ MAX_GC_FAC 0)
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ R (ADD1 R))
            (COND
             (PRINT_
              (PROGN
               (PROGN (PRIN2 R) (PRIN2 ". simplification run") NIL)
               (TERPRI))))
            (COND
             (PRINT_
              (PROGN
               (PROGN (PRIN2 "Substitution of vanishing unknowns") NIL)
               (TERPRI))))
            (SETQ S
                    (PROG (A FORALL-RESULT FORALL-ENDPTR)
                      (SETQ A M)
                      (COND ((NULL A) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (A) (CONS A 0)) (CAR A))
                                            NIL)))
                     LOOPLABEL
                      (SETQ A (CDR A))
                      (COND ((NULL A) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (A) (CONS A 0)) (CAR A)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND (M (PROGN (SETQ W (NCONC M W)) (SETQ M NIL) NIL)))
            (PROG (P)
              (SETQ P H)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (PROGN
                  (COND (S (SETQ Q (CAR (SUBF P S)))) (T (SETQ Q P)))
                  (COND ((AND S (EQUAL P Q)) (SETQ EQNS (UNION (LIST P) EQNS)))
                        (T
                         (PROGN
                          (SETQ P (CDR (ERR_CATCH_FAC3 Q)))
                          (COND
                           ((NULL P)
                            (SETQ EQNS
                                    (UNION
                                     (LIST
                                      (CAR
                                       (CAR
                                        (SIMPLIFYSQ (CONS Q 1) FLL NIL NIL
                                         NIL))))
                                     EQNS)))
                           (T
                            (PROGN
                             (SETQ Q NIL)
                             (PROG ()
                              WHILELABEL
                               (COND ((NOT P) (RETURN NIL)))
                               (PROGN
                                (COND
                                 ((AND (NOT (FREEOFLIST (CAAR P) FLL))
                                       (NOT (MEMBER (CONS (CAAR P) 1) INEQ_)))
                                  (SETQ Q (CONS (CAAR P) Q))))
                                (SETQ P (CDR P)))
                               (GO WHILELABEL))
                             (COND ((NULL Q) (SETQ EQNS (CONS 1 EQNS)))
                                   ((CDR Q)
                                    (PROGN
                                     (SETQ H 1)
                                     (PROG (K)
                                       (SETQ K Q)
                                      LAB
                                       (COND ((NULL K) (RETURN NIL)))
                                       ((LAMBDA (K)
                                          (SETQ H
                                                  (COND
                                                   (*PHYSOP-LOADED
                                                    (PHYSOP-MULTF K H))
                                                   (T (POLY-MULTF K H)))))
                                        (CAR K))
                                       (SETQ K (CDR K))
                                       (GO LAB))
                                     (SETQ EQNS (UNION (LIST H) EQNS))))
                                   (T
                                    (PROGN
                                     (SETQ Q
                                             (CAR
                                              (CAR
                                               (SIMPLIFYSQ (CONS (CAR Q) 1) FLL
                                                NIL NIL NIL))))
                                     (COND
                                      ((PAIRP Q)
                                       (COND
                                        ((AND (NULL (CDR Q)) (EQUAL (CDAR Q) 1)
                                              (EQUAL (CDAAR Q) 1)
                                              (ATOM (CAAAR Q)))
                                         (SETQ M (UNION (LIST (CAAAR Q)) M)))
                                        (T
                                         (SETQ EQNS
                                                 (UNION (LIST Q)
                                                        EQNS)))))))))))))))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ H EQNS)
            (SETQ EQNS NIL))
           (COND ((NOT (NULL M)) (GO REPEATLABEL))))
         (SETQ MAX_GC_FAC MAX_GC_FAC_BAK)
         (SETQ INEQ_ INEQ_BAK)
         (SETQ K (LENGTH W))
         (COND
          (PRINT_
           (PROGN
            (PROGN (PRIN2 K) (PRIN2 " coefficients found to be zero: ") NIL)
            (COND ((LESSP K 100) (LISTPRINT W)))
            (TERPRI))))
         (SETQ H (APPEND H W))
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 (REVAL1 (DIFFERENCE LE (LENGTH H)) T))
             (PRIN2 " redundant equations have been dropped.")
             NIL)
            (TERPRI))))
         NIL)))
      (SETQ EQNS
              (SETQ P
                      (SETQ Q
                              (SETQ INEQ_BAK
                                      (SETQ W
                                              (SETQ NOPOWERSBAK
                                                      (SETQ M
                                                              (SETQ R
                                                                      (SETQ S
                                                                              (SETQ A
                                                                                      (SETQ SPLITVAR
                                                                                              (SETQ FLL
                                                                                                      (SETQ LE
                                                                                                              NIL)))))))))))))
      (COND (NOPOWERSBAK (AEVAL (ON (LIST 'NOPOWERS)))))
      (COND
       (*TIME
        (PROGN
         (PROGN
          (PRIN2 (QUOTIENT (DIFFERENCE (TIME) CPU) 1000))
          (PRIN2 " s: The system is formulated.")
          NIL)
         (TERPRI))))
      (RETURN
       (CONS 'LIST
             (PROG (K FORALL-RESULT FORALL-ENDPTR)
               (SETQ K H)
               (COND ((NULL K) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (K) (LIST '*SQ (CONS K 1) NIL))
                                 (CAR K))
                                NIL)))
              LOOPLABEL
               (SETQ K (CDR K))
               (COND ((NULL K) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (K) (LIST '*SQ (CONS K 1) NIL)) (CAR K))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(ENDMODULE) 