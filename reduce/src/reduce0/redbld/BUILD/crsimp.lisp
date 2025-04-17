(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMPLIFICATIONS)) 
(PUT 'SIGNCHANGE 'NUMBER-OF-ARGS 1) 
(PUT 'SIGNCHANGE 'DEFINED-ON-LINE '34) 
(PUT 'SIGNCHANGE 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SIGNCHANGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIGNCHANGE (G)
    (COND
     ((PAIRP G)
      (COND ((EQUAL (CAR G) 'MINUS) (CADR G))
            ((AND (EQUAL (CAR G) 'PLUS) (PAIRP (CADR G))
                  (EQUAL (CAADR G) 'MINUS))
             (REVAL1 (LIST 'MINUS G) T))
            (T G)))
     (T G))) 
(PUT 'RAISE_CONTRADICTION 'NUMBER-OF-ARGS 2) 
(PUT 'RAISE_CONTRADICTION 'DEFINED-ON-LINE '45) 
(PUT 'RAISE_CONTRADICTION 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'RAISE_CONTRADICTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RAISE_CONTRADICTION (G TEXT)
    (PROGN
     (SETQ CONTRADICTION_ T)
     (COND
      (PRINT_
       (PROGN
        (TERPRI)
        (COND (TEXT (PROGN (PRIN2 TEXT) NIL))
              (T (PROGN (PRIN2 "contradiction : ") NIL)))
        (DEPRINT (LIST G))))))) 
(PUT 'FCTEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'FCTEVAL 'DEFINED-ON-LINE '54) 
(PUT 'FCTEVAL 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'FCTEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTEVAL (P)
    (COND
     ((FLAGP P 'TO_EVAL)
      (COND ((PAIRP (GET P 'FAC)) (REMFLAG (LIST P) 'TO_EVAL))
            (T
             (PROG (FT A FL LI NC NL F CPF FV FC F_IS_IN_FLIN FLIN_SUB_FOUND)
               (COND
                ((AND (NOT (GET P 'FCTEVAL_LIN)) (NOT (GET P 'FCTEVAL_NCA))
                      (NOT (GET P 'FCTEVAL_NLI)))
                 (PROGN
                  (SETQ FT (GET P 'ALLVARFCTS))
                  (COND
                   ((NULL FT)
                    (PROGN
                     (SETQ FT (SETDIFF (GET P 'RATIONAL) (GET P 'NOT_TO_EVAL)))
                     (COND
                      (VL_
                       (PROGN
                        (PROG (F)
                          (SETQ F FT)
                         LAB
                          (COND ((NULL F) (RETURN NIL)))
                          ((LAMBDA (F)
                             (PROGN
                              (SETQ CPF (GET P 'FCTS))
                              (SETQ FV (FCTARGS F))
                              (PROG ()
                               WHILELABEL
                                (COND
                                 ((NOT
                                   (AND CPF
                                        (OR
                                         (NOT_INCLUDED FV
                                          (SETQ FC (FCTARGS (CAR CPF))))
                                         (GEQ (LENGTH FV) (LENGTH FC)))))
                                  (RETURN NIL)))
                                (SETQ CPF (CDR CPF))
                                (GO WHILELABEL))
                              (COND ((NULL CPF) (SETQ FL (CONS F CPF))))))
                           (CAR F))
                          (SETQ F (CDR F))
                          (GO LAB))
                        (SETQ FT FL)))))))
                  (SETQ LI (SETQ NC (SETQ NL NIL)))
                  (PROG (F)
                    (SETQ F FT)
                   LAB
                    (COND ((NULL F) (RETURN NIL)))
                    ((LAMBDA (F)
                       (COND
                        ((NULL (MEMBER F (GET P 'NOT_TO_EVAL)))
                         (COND
                          ((ALG_LINEAR_FCT P F)
                           (COND
                            ((OR CONFIRM_SUBST (NULL FLIN_)
                                 (PROGN
                                  (SETQ F_IS_IN_FLIN (MEMBER F FLIN_))
                                  (COND
                                   ((AND FLIN_SUB_FOUND (NULL F_IS_IN_FLIN))
                                    NIL)
                                   (T
                                    (PROGN
                                     (COND
                                      ((AND F_IS_IN_FLIN (NULL FLIN_SUB_FOUND))
                                       (PROGN
                                        (SETQ LI (SETQ NC (SETQ NL NIL)))
                                        (SETQ FLIN_SUB_FOUND T))))
                                     T)))))
                             (PROGN
                              (SETQ A
                                      (COEFFN (LIST '*SQ (GET P 'SQVAL) T) F
                                              1))
                              (SETQ A
                                      (COND
                                       ((AND (PAIRP A) (EQUAL (CAR A) '*SQ))
                                        (CADR A))
                                       (T (SIMP A))))
                              (COND
                               ((NULL
                                 (SETQ FL
                                         (SMEMBERL (DELETE F (GET P 'FCTS))
                                          A)))
                                (SETQ LI (CONS (CONS A F) LI)))
                               ((CAN_NOT_BECOME_ZEROSQ A FL)
                                (SETQ NC (CONS (CONS A F) NC)))
                               (T (SETQ NL (CONS (CONS A F) NL))))))))))))
                     (CAR F))
                    (SETQ F (CDR F))
                    (GO LAB))
                  (COND
                   ((AND FLIN_ (NULL FLIN_SUB_FOUND) VL_
                         (SETQ FL (INTERSECTION FT FLIN_)))
                    (PROGN (PUT P 'FCTEVAL_N2L T))))
                  (COND (LI (PUT P 'FCTEVAL_LIN (REVERSE LI))))
                  (COND (NC (PUT P 'FCTEVAL_NCA (REVERSE NC))))
                  (COND (NL (PUT P 'FCTEVAL_NLI (REVERSE NL))))
                  (COND ((NOT (OR LI NC NL)) (REMFLAG (LIST P) 'TO_EVAL))))))
               (RETURN
                (OR (GET P 'FCTEVAL_LIN) (GET P 'FCTEVAL_NCA)
                    (GET P 'FCTEVAL_NLI))))))))) 
(PUT 'BEST_MDU 'NUMBER-OF-ARGS 1) 
(PUT 'BEST_MDU 'DEFINED-ON-LINE '164) 
(PUT 'BEST_MDU 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'BEST_MDU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BEST_MDU (P)
    (COND ((GET P 'FCTEVAL_LIN) 1) ((GET P 'FCTEVAL_NCA) 2)
          ((GET P 'FCT_NLI_LIN) 3) ((GET P 'FCT_NLI_NCA) 4)
          ((GET P 'FCT_NLI_NLI) 5) ((GET P 'FCT_NLI_NUS) 6) (T 7))) 
(PUT 'GET_SUBST 'NUMBER-OF-ARGS 6) 
(PUT 'GET_SUBST 'DEFINED-ON-LINE '174) 
(PUT 'GET_SUBST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'GET_SUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GET_SUBST (PDES L LENGTH_LIMIT LESS_VARS NO_DF NO_CASES)
    (PROG (P Q H L1 L2 M NTMS MDU INEQ_CP RTN LCOP FCTEVAL_COP NECOUNT
           FOUND_NON_INEQ_SUB FOUND_NON_N2L_SUB IS_NON_INEQ_SUB IS_NON_N2L_SUB)
      (SETQ LCOP L)
      (COND
       (LESS_VARS
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (COND ((EQUAL (GET (CAR L) 'NVARS) 0) (SETQ L1 (CONS (CAR L) L1)))
                  (T
                   (PROGN
                    (SETQ L2 (GET (CAR L) 'ALLVARFCTS))
                    (COND
                     ((AND L2 (NULL (CDR L2))) (SETQ L1 (CONS (CAR L) L1))))
                    NIL)))
            (SETQ L (CDR L)))
           (GO WHILELABEL))
         (SETQ L (REVERSE L1)))))
      (COND
       (LENGTH_LIMIT
        (PROGN
         (SETQ L1 NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (COND
            ((GREATERP (SETQ H (GET (CAR L) 'LENGTH)) LENGTH_LIMIT)
             (COND ((GREATERP H (TIMES 2 LENGTH_LIMIT)) (SETQ L NIL))
                   (T (SETQ L (CDR L)))))
            (T (PROGN (SETQ L1 (CONS (CAR L) L1)) (SETQ L (CDR L)))))
           (GO WHILELABEL))
         (SETQ L (REVERSE L1)))))
      (COND
       (NO_DF
        (PROGN
         (SETQ L1 NIL)
         (PROG (S)
           (SETQ S L)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (PROGN
               (SETQ L2 (GET S 'DERIVS))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L2) (RETURN NIL)))
                 (COND
                  ((PAIRP (CDAAR L2))
                   (PROGN (SETQ L2 NIL) (SETQ L1 (CONS S L1))))
                  (T (SETQ L2 (CDR L2))))
                 (GO WHILELABEL))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ L (SETDIFF L L1)))))
      (SETQ L1 NIL)
      (SETQ NECOUNT 0)
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((FCTEVAL S)
             (COND
              ((OR (GET S 'FCTEVAL_LIN) (GET S 'FCTEVAL_NCA))
               (SETQ L1 (CONS S L1)))
              ((NULL NO_CASES)
               (COND
                ((SETQ H (GET S 'FCTEVAL_NLI))
                 (PROGN
                  (COND
                   ((AND (NULL (GET S 'FCT_NLI_LIN))
                         (NULL (GET S 'FCT_NLI_NCA))
                         (NULL (GET S 'FCT_NLI_NLI))
                         (NULL (GET S 'FCT_NLI_NUS)))
                    (PROGN
                     (SETQ INEQ_CP INEQ_)
                     (SETQ INEQ_ NIL)
                     (PROG (L2)
                       (SETQ L2 H)
                      LAB
                       (COND ((NULL L2) (RETURN NIL)))
                       ((LAMBDA (L2)
                          (PROGN
                           (SETQ Q
                                   (MKEQSQ (CAR L2) NIL NIL (GET S 'FCTS)
                                    (GET S 'VARS) ALLFLAGS_ T (LIST 0) NIL
                                    NIL))
                           (SETQ NECOUNT (ADD1 NECOUNT))
                           (FCTEVAL Q)
                           (COND
                            ((GET Q 'FCTEVAL_LIN)
                             (PUT S 'FCT_NLI_LIN
                                  (CONS L2 (GET S 'FCT_NLI_LIN))))
                            ((GET Q 'FCTEVAL_NCA)
                             (PUT S 'FCT_NLI_NCA
                                  (CONS L2 (GET S 'FCT_NLI_NCA))))
                            ((GET Q 'FCTEVAL_NLI)
                             (PUT S 'FCT_NLI_NLI
                                  (CONS L2 (GET S 'FCT_NLI_NLI))))
                            (T
                             (PUT S 'FCT_NLI_NUS
                                  (CONS L2 (GET S 'FCT_NLI_NUS)))))
                           (DROP_PDE Q NIL NIL)
                           (COND
                            ((GREATERP NECOUNT 100)
                             (PROGN
                              (CLEAN_PROP_LIST PDES)
                              (SETQ NECOUNT 0))))))
                        (CAR L2))
                       (SETQ L2 (CDR L2))
                       (GO LAB))
                     (SETQ INEQ_ INEQ_CP))))
                  (SETQ L1 (CONS S L1))
                  NIL))))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ L L1)
      (SETQ M (MINUS 1))
      (SETQ MDU 8)
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ L1 (GET S 'NVARS))
            (COND ((GET S 'STARDE) (SETQ L1 (SUB1 L1))))
            (COND ((GREATERP L1 M) (SETQ M L1)))
            NIL))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ M 0)) (RETURN NIL)))
        (PROGN
         (SETQ L1 L)
         (SETQ NTMS 10000000)
         (SETQ FOUND_NON_INEQ_SUB NIL)
         (SETQ FOUND_NON_N2L_SUB NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT L1) (RETURN NIL)))
           (COND
            ((AND
              (EQUAL
               (DIFFERENCE (GET (CAR L1) 'NVARS)
                           (COND ((GET (CAR L1) 'STARDE) 1) (T 0)))
               M)
              (SETQ Q (FCTEVAL (CAR L1)))
              (OR (SETQ IS_NON_INEQ_SUB (NULL (MEMBER (SIMP (CDAR Q)) INEQ_)))
                  (NULL FOUND_NON_INEQ_SUB))
              (OR (SETQ IS_NON_N2L_SUB (NULL (GET (CAR L1) 'FCTEVAL_N2L)))
                  (NULL FOUND_NON_N2L_SUB))
              (OR (LESSP (GET (CAR L1) 'TERMS) NTMS)
                  (AND (EQUAL (GET (CAR L1) 'TERMS) NTMS)
                       (LESSP (BEST_MDU (CAR L1)) MDU))))
             (PROGN
              (SETQ P (CAR L1))
              (SETQ L1 (CDR L1))
              (SETQ NTMS (GET P 'TERMS))
              (SETQ MDU (BEST_MDU P))
              (COND
               ((NULL FOUND_NON_INEQ_SUB)
                (SETQ FOUND_NON_INEQ_SUB IS_NON_INEQ_SUB)))
              (COND
               ((NULL FOUND_NON_N2L_SUB)
                (SETQ FOUND_NON_N2L_SUB IS_NON_N2L_SUB)))))
            (T (SETQ L1 (CDR L1))))
           (GO WHILELABEL))
         (SETQ M (COND (P (MINUS 1)) (T (SUB1 M)))))
        (GO WHILELABEL))
      (COND
       (P
        (RETURN
         (PROGN
          (SETQ FCTEVAL_COP
                  (COND ((EQUAL MDU 1) (GET P 'FCTEVAL_LIN))
                        ((EQUAL MDU 2) (GET P 'FCTEVAL_NCA))
                        ((EQUAL MDU 3) (GET P 'FCT_NLI_LIN))
                        ((EQUAL MDU 4) (GET P 'FCT_NLI_NCA))
                        ((EQUAL MDU 5) (GET P 'FCT_NLI_NLI))
                        ((EQUAL MDU 6) (GET P 'FCT_NLI_NUS))))
          (SETQ RTN (LIST MDU P (PICK_FCTEVAL PDES MDU FCTEVAL_COP)))
          (COND
           ((AND RTN FHOM_
                 (SETDIFF
                  (PROG (H FORALL-RESULT FORALL-ENDPTR)
                    (SETQ H (GET P 'FCTS))
                    (COND ((NULL H) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (H) (SIMP H)) (CAR H))
                                          NIL)))
                   LOOPLABEL
                    (SETQ H (CDR H))
                    (COND ((NULL H) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (H) (SIMP H)) (CAR H)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  INEQ_)
                 (CDR PDES) (GREATERP (GET P 'TERMS) 1))
            (PROGN
             (COND ((NOT (MEMBER (SIMP (CDADDR RTN)) INEQ_)) RTN)
                   ((NULL (CDR FCTEVAL_COP))
                    (COND
                     ((CDR LCOP)
                      (PROGN
                       (SETQ H
                               (GET_SUBST PDES (DELETE P LCOP) LENGTH_LIMIT
                                LESS_VARS NO_DF NO_CASES))
                       (COND ((NULL H) RTN) (T H))))
                     (T RTN)))
                   (T
                    (PROGN
                     (SETQ FCTEVAL_COP (DELETE (CADDR RTN) FCTEVAL_COP))
                     (LIST MDU P (PICK_FCTEVAL PDES MDU FCTEVAL_COP)))))))
           (T RTN)))))))) 
(PUT 'TOTAL_DEG_OF_FIRST_TM 'NUMBER-OF-ARGS 1) 
(PUT 'TOTAL_DEG_OF_FIRST_TM 'DEFINED-ON-LINE '366) 
(PUT 'TOTAL_DEG_OF_FIRST_TM 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'TOTAL_DEG_OF_FIRST_TM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOTAL_DEG_OF_FIRST_TM (SF)
    (COND ((OR (ATOM SF) (ATOM (CAR SF))) 0)
          (T (PLUS (CDAAR SF) (TOTAL_DEG_OF_FIRST_TM (CDAR SF)))))) 
(PUT 'NCO_SF 'NUMBER-OF-ARGS 1) 
(PUT 'NCO_SF 'DEFINED-ON-LINE '370) 
(PUT 'NCO_SF 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'NCO_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NCO_SF (SF)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT
          (AND (PAIRP SF) (NOT (OR (ATOM SF) (ATOM (CAR SF))))
               (NOT (OR (ATOM (CDAR SF)) (ATOM (CAR (CDAR SF)))))))
         (RETURN NIL)))
       (SETQ SF (CDAR SF))
       (GO WHILELABEL))
     SF)) 
(PUT 'PICK_FCTEVAL 'NUMBER-OF-ARGS 3) 
(PUT 'PICK_FCTEVAL 'DEFINED-ON-LINE '374) 
(PUT 'PICK_FCTEVAL 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'PICK_FCTEVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PICK_FCTEVAL (PDES MDU FCTLI)
    (COND
     (FCTLI
      (COND
       ((OR (NOT EXPERT_MODE) (EQUAL (LENGTH FCTLI) 1))
        (COND ((NULL (CDR FCTLI)) (CAR FCTLI))
              ((LESSP MDU 3)
               (PROG (BEST BESTH1 BESTH2 BESTH3 H1 H2 H3)
                 (SETQ BESTH1 100000000)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT FCTLI) (RETURN NIL)))
                   (PROGN
                    (COND
                     ((GREATERP BESTH1
                                (SETQ H1 (NO_OF_TM_SF (CAR (CAAR FCTLI)))))
                      (PROGN
                       (SETQ BEST (CAR FCTLI))
                       (SETQ BESTH1 H1)
                       (COND
                        ((EQUAL H1 1)
                         (PROGN
                          (SETQ BESTH2
                                  (TOTAL_DEG_OF_FIRST_TM (CAR (CAAR FCTLI))))
                          (SETQ BESTH3 (NCO_SF (CAR (CAAR FCTLI)))))))))
                     ((EQUAL BESTH1 H1)
                      (COND
                       ((GREATERP H1 1)
                        (COND
                         ((NEQ (WHICH_FIRST (CDR BEST) (CDAR FCTLI) FTEM_)
                               (CDR BEST))
                          (SETQ BEST (CAR FCTLI)))
                         (T NIL)))
                       ((GREATERP BESTH2
                                  (SETQ H2
                                          (TOTAL_DEG_OF_FIRST_TM
                                           (CAR (CAAR FCTLI)))))
                        (PROGN
                         (SETQ BEST (CAR FCTLI))
                         (SETQ BESTH2 H2)
                         (SETQ BESTH3 (NCO_SF (CAR (CAAR FCTLI))))))
                       ((EQUAL BESTH2 H2)
                        (COND
                         ((OR
                           (AND (FIXP (SETQ H3 (NCO_SF (CAR (CAAR FCTLI)))))
                                (NOT (FIXP BESTH3)))
                           (AND (FIXP H3) (FIXP BESTH3)
                                (LESSP (ABS H3) (ABS BESTH3))))
                          (PROGN
                           (SETQ BEST (CAR FCTLI))
                           (SETQ BESTH3 H3))))))))
                    (SETQ FCTLI (CDR FCTLI)))
                   (GO WHILELABEL))
                 (RETURN BEST)))
              (T
               (PROG (CO MINFINCO MINNOFINCO FINCO NOFINCO FCTLILEN N MAXNOPDES
                      NOPDES F BESTN)
                 (SETQ FCTLILEN (LENGTH FCTLI))
                 (SETQ MINNOFINCO 10000)
                 (PROG (N)
                   (SETQ N 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE FCTLILEN N)) (RETURN NIL)))
                   (PROGN
                    (SETQ CO (NTH FCTLI N))
                    (SETQ FINCO (SMEMBERL FTEM_ (CAR CO)))
                    (SETQ NOFINCO (LENGTH FINCO))
                    (COND
                     ((LESSP NOFINCO MINNOFINCO)
                      (PROGN
                       (SETQ MINFINCO (LIST (CONS N FINCO)))
                       (SETQ MINNOFINCO NOFINCO)))
                     ((EQUAL NOFINCO MINNOFINCO)
                      (SETQ MINFINCO (CONS (CONS N FINCO) MINFINCO))))
                    NIL)
                   (SETQ N (PLUS2 N 1))
                   (GO LAB))
                 (COND
                  ((OR (EQUAL (LENGTH MINFINCO) 1) (GREATERP MINNOFINCO 1))
                   (RETURN (NTH FCTLI (CAAR MINFINCO))))
                  (T
                   (RETURN
                    (PROGN
                     (SETQ MAXNOPDES 1000000)
                     (PROG (SU)
                       (SETQ SU MINFINCO)
                      LAB
                       (COND ((NULL SU) (RETURN NIL)))
                       ((LAMBDA (SU)
                          (PROGN
                           (SETQ F (CADR SU))
                           (SETQ NOPDES 0)
                           (PROG (P)
                             (SETQ P PDES)
                            LAB
                             (COND ((NULL P) (RETURN NIL)))
                             ((LAMBDA (P)
                                (COND
                                 ((NOT (FREEOF (GET P 'FCTS) F))
                                  (SETQ NOPDES (ADD1 NOPDES)))))
                              (CAR P))
                             (SETQ P (CDR P))
                             (GO LAB))
                           (COND
                            ((LESSP NOPDES MAXNOPDES)
                             (PROGN
                              (SETQ MAXNOPDES NOPDES)
                              (SETQ BESTN (CAR SU)))))
                           NIL))
                        (CAR SU))
                       (SETQ SU (CDR SU))
                       (GO LAB))
                     (NTH FCTLI BESTN)))))))))
       (T
        (PROG (FL A H)
          (SETQ FL
                  (PROG (A FORALL-RESULT FORALL-ENDPTR)
                    (SETQ A FCTLI)
                    (COND ((NULL A) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL)))
                   LOOPLABEL
                    (SETQ A (CDR A))
                    (COND ((NULL A) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (PROGN (PRIN2 "Choose a function to be substituted from ") NIL)
          (LISTPRINT FL)
          (TERPRI)
          (CHANGE_PROMPT_TO "")
          (PROG ()
           REPEATLABEL
            (SETQ H (TERMREAD))
            (COND ((NOT (NOT (FREEOF FL H))) (GO REPEATLABEL))))
          (RESTORE_INTERACTIVE_PROMPT)
          (PROG ()
           WHILELABEL
            (COND ((NOT (NEQ H (CDAR FCTLI))) (RETURN NIL)))
            (SETQ FCTLI (CDR FCTLI))
            (GO WHILELABEL))
          (RETURN (CAR FCTLI)))))))) 
(PUT 'DO_ONE_SUBST 'NUMBER-OF-ARGS 8) 
(PUT 'DO_ONE_SUBST 'DEFINED-ON-LINE '452) 
(PUT 'DO_ONE_SUBST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DO_ONE_SUBST 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE DO_ONE_SUBST (EX F A FTEM VL LEVEL EQN PDES)
    (PROG (L L1 P OLDSTARDE)
      (SETQ L (GET A 'FAC))
      (COND ((NOT (PAIRP L)) (SETQ L (LIST (GET A 'SQVAL)))))
      (SETQ OLDSTARDE (GET A 'STARDE))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (COND
          ((SMEMBER F (CAR L))
           (PROGN
            (SETQ P (SIMP* (LIST '*SQ (SUBSQ (CAR L) (LIST (CONS F EX))) NIL)))
            (COND ((SQZEROP P) (PROGN (SETQ L (LIST NIL)) (SETQ L1 (LIST 0))))
                  (T (PROGN (SETQ L1 (CONS P L1)))))))
          (T (SETQ L1 (CONS (CAR L) L1))))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (SETQ L NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT L1) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (MEMBER (CAR L1) (CDR L1)))
           (SETQ L (UNION (SIMPLIFYSQ (CAR L1) FTEM T NIL NIL) L))))
         (SETQ L1 (CDR L1)))
        (GO WHILELABEL))
      (SETQ L (DELETE (CONS 1 1) L))
      (COND
       ((NULL L)
        (PROGN
         (COND
          (PRINT_
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "Substitution of ") NIL)
            (FCTPRINT (LIST F))
            (COND
             ((CDR (GET EQN 'FCTS))
              (PROGN
               (PROGN (PRIN2 " by an expression in ") NIL)
               (TERPRI)
               (FCTPRINT (DELETE F (GET EQN 'FCTS))))))
            (PROGN (PRIN2 " found in ") (PRIN2 EQN) (PRIN2 " : ") NIL)
            (EQPRINT (LIST 'EQUAL F EX)))))
         (RAISE_CONTRADICTION (LIST '*SQ (GET A 'SQVAL) T)
          "leads to a contradiction in : ")
         (SETQ A NIL)))
       ((MEMBER (CONS NIL 1) L) (PROGN (DROP_PDE A NIL 0) (SETQ A NIL)))
       (T
        (PROGN
         (COND
          ((NEQ (GET A 'LEVEL) LEVEL)
           (PROGN
            (SETQ PDES (DROP_PDE A PDES 0))
            (SETQ A
                    (MKEQSQ NIL L NIL FTEM VL ALLFLAGS_ NIL (LIST 0) NIL
                     PDES))))
          (T
           (PROGN
            (SETQ P (GET A 'DERIVS))
            (COND (P (SETQ P (CAAR P))))
            (PROG (B)
              (SETQ B ALLFLAGS_)
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (FLAG (LIST A) B)) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (COND
             ((NULL (UPDATESQ A NIL L NIL FTEM VL NIL (LIST 0) PDES))
              (PROGN (DROP_PDE A PDES 0) (SETQ A NIL)))
             (T
              (PROGN
               (SETQ L1 (GET A 'DERIVS))
               (COND (L1 (SETQ L1 (CAAR L1))))
               (COND
                ((NEQ L1 P)
                 (PROGN
                  (PUT A 'DEC_WITH NIL)
                  (PUT A 'DEC_WITH_RL NIL)
                  (PROG (L1)
                    (SETQ L1 PDES)
                   LAB
                    (COND ((NULL L1) (RETURN NIL)))
                    ((LAMBDA (L1)
                       (COND
                        ((NEQ L1 A)
                         (PROGN
                          (DROP_DEC_WITH A L1 T)
                          (DROP_DEC_WITH A L1 NIL)))))
                     (CAR L1))
                    (SETQ L1 (CDR L1))
                    (GO LAB)))))
               (DROP_PDE_FROM_IDTIES A PDES NIL)
               (PUT A 'RL_WITH NIL)
               (PROG (L1)
                 (SETQ L1 PDES)
                LAB
                 (COND ((NULL L1) (RETURN NIL)))
                 ((LAMBDA (L1) (COND ((NEQ L1 A) (DROP_RL_WITH A L1))))
                  (CAR L1))
                 (SETQ L1 (CDR L1))
                 (GO LAB))))))))
         (PUT A 'LEVEL LEVEL))))
      (COND ((AND OLDSTARDE (NOT (GET A 'STARDE))) (PUT A 'DEC_WITH NIL)))
      (RETURN A))) 
(PUT 'SUB_IN_FORG 'NUMBER-OF-ARGS 3) 
(PUT 'SUB_IN_FORG 'DEFINED-ON-LINE '537) 
(PUT 'SUB_IN_FORG 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUB_IN_FORG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUB_IN_FORG (F EX FORG)
    (PROG (FL H WAS_SUBST DNR)
      (SETQ FL
              (SMEMBERL
               (APPEND FTEM_
                       (PROG (H FORALL-RESULT FORALL-ENDPTR)
                         (SETQ H FSUB_)
                         (COND ((NULL H) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (H) (CAR H)) (CAR H))
                                               NIL)))
                        LOOPLABEL
                         (SETQ H (CDR H))
                         (COND ((NULL H) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (H) (CAR H)) (CAR H)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               EX))
      (SETQ FORG
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H FORG)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (COND
                                     ((ATOM H)
                                      (COND
                                       ((EQUAL F H)
                                        (PROGN
                                         (PUT F 'FCTS FL)
                                         (SETQ WAS_SUBST T)
                                         (LIST 'EQUAL F
                                               (COND
                                                ((AND (PAIRP EX)
                                                      (EQUAL (CAR EX) '*SQ))
                                                 (CADR EX))
                                                (T (SIMP* EX))))))
                                       (T H)))
                                     ((AND (EQUAL (CAR H) 'EQUAL)
                                           (MEMBER F (GET (CADR H) 'FCTS)))
                                      (PROGN
                                       (SETQ WAS_SUBST T)
                                       (SETQ DNR
                                               (SIMP*
                                                (LIST '*SQ
                                                      (SUBF (CDR (CADDR H))
                                                            (LIST (CONS F EX)))
                                                      NIL)))
                                       (COND
                                        ((SQZEROP DNR)
                                         (PROGN
                                          (SETQ CONTRADICTION_ T)
                                          (TERPRI)
                                          (PROGN
                                           (PRIN2
                                            "##### ERROR: When substituting ")
                                           (PRIN2 F)
                                           (PRIN2 " by ")
                                           (PRIN2 EX)
                                           (PRIN2
                                            " in the denominator of the forg entry: ")
                                           (PRIN2 H)
                                           (PRIN2
                                            " then the denominator becomes zero!! #####")
                                           NIL)
                                          (TERPRI)
                                          NIL))
                                        (T
                                         (PROGN
                                          (SETQ H
                                                  (LIST 'EQUAL (CADR H)
                                                        (SIMP*
                                                         (LIST '*SQ
                                                               (MULTSQ
                                                                (SUBF
                                                                 (CAR
                                                                  (CADDR H))
                                                                 (LIST
                                                                  (CONS F EX)))
                                                                (INVSQ DNR))
                                                               NIL))))
                                          (PUT (CADR H) 'FCTS
                                               (SMEMBERL
                                                (UNION FL
                                                       (DELETE F
                                                               (GET (CADR H)
                                                                    'FCTS)))
                                                (CADDR H)))
                                          H)))))
                                     (T H)))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (COND
                             ((ATOM H)
                              (COND
                               ((EQUAL F H)
                                (PROGN
                                 (PUT F 'FCTS FL)
                                 (SETQ WAS_SUBST T)
                                 (LIST 'EQUAL F
                                       (COND
                                        ((AND (PAIRP EX) (EQUAL (CAR EX) '*SQ))
                                         (CADR EX))
                                        (T (SIMP* EX))))))
                               (T H)))
                             ((AND (EQUAL (CAR H) 'EQUAL)
                                   (MEMBER F (GET (CADR H) 'FCTS)))
                              (PROGN
                               (SETQ WAS_SUBST T)
                               (SETQ DNR
                                       (SIMP*
                                        (LIST '*SQ
                                              (SUBF (CDR (CADDR H))
                                                    (LIST (CONS F EX)))
                                              NIL)))
                               (COND
                                ((SQZEROP DNR)
                                 (PROGN
                                  (SETQ CONTRADICTION_ T)
                                  (TERPRI)
                                  (PROGN
                                   (PRIN2 "##### ERROR: When substituting ")
                                   (PRIN2 F)
                                   (PRIN2 " by ")
                                   (PRIN2 EX)
                                   (PRIN2
                                    " in the denominator of the forg entry: ")
                                   (PRIN2 H)
                                   (PRIN2
                                    " then the denominator becomes zero!! #####")
                                   NIL)
                                  (TERPRI)
                                  NIL))
                                (T
                                 (PROGN
                                  (SETQ H
                                          (LIST 'EQUAL (CADR H)
                                                (SIMP*
                                                 (LIST '*SQ
                                                       (MULTSQ
                                                        (SUBF (CAR (CADDR H))
                                                              (LIST
                                                               (CONS F EX)))
                                                        (INVSQ DNR))
                                                       NIL))))
                                  (PUT (CADR H) 'FCTS
                                       (SMEMBERL
                                        (UNION FL
                                               (DELETE F (GET (CADR H) 'FCTS)))
                                        (CADDR H)))
                                  H)))))
                             (T H)))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS FORG WAS_SUBST)))) 
(PUT 'DO_SUBST 'NUMBER-OF-ARGS 8) 
(PUT 'DO_SUBST 'DEFINED-ON-LINE '572) 
(PUT 'DO_SUBST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DO_SUBST 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE DO_SUBST (MD P L PDE FORG VL PLIM KEEP_EQN)
    (PROG (F FL H EX RES SLIM TOO_LARGE WAS_SUBST NEW_USER_RULES RULI ISE CF
           NOF STDE PARTIAL_SUBS INEQ_BAK INEQ_OR_BAK)
      (SETQ F (CDR L))
      (SETQ CF (CAR L))
      (COND
       ((AND FLIN_ VL_ (FREEOF FLIN_ F) (NULL (ADD2FLIN PDE F)))
        (PROGN
         (PROG (H)
           (SETQ H (GET P 'FCTS))
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND ((NOT (FREEOF FLIN_ H)) (SETQ FLIN_ (DELETE H FLIN_)))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB)))))
      (COND ((GET P 'STARDE) (SETQ ISE T)))
      (SETQ SLIM (GET P 'LENGTH))
      (SETQ RULI (START_LET_RULES))
      (COND (MODULAR_COMP (ON (LIST 'MODULAR))))
      (SETQ EX
              (LIST '*SQ
                    (SUBS2
                     (MULTSQ
                      (ADDSQ (MULTSQ CF (SIMP F)) (NEGSQ (GET P 'SQVAL)))
                      (INVSQ CF)))
                    T))
      (PROG (H)
        (SETQ H (CDR USERRULES_))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((AND (NOT (FREEOF (CDR H) F))
                  (EQUAL (LIST (CONS 1 1))
                         (SIMPLIFYSQ
                          (SUBSQ
                           (ADDSQ (SIMP (CADR H)) (NEGSQ (SIMP (CADDR H))))
                           (LIST (CONS F EX)))
                          FTEM_ T NIL NIL)))
             (PROGN
              (SETQ CONTRADICTION_ T)
              (COND
               (PRINT_
                (PROGN
                 (PROGN (PRIN2 "The current substitution") NIL)
                 (TERPRI)
                 (PROGN
                  (ASSGNPRI (AEVAL F) NIL 'FIRST)
                  (ASSGNPRI (AEVAL " = ") NIL NIL)
                  (ASSGNPRI (AEVAL EX) NIL 'LAST))
                 (PROGN
                  (PRIN2 "results in a contradiction in the LET rule:")
                  NIL)
                 (ASSGNPRI (AEVAL (LIST 'LIST H)) NIL 'ONLY)
                 NIL)))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (COND
       ((NOT CONTRADICTION_)
        (COND
         (EXPERT_MODE
          (PROGN
           (TERPRI)
           (PROGN
            (PRIN2
             "Enter a list of equations in which substitution should take place.")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             "Substitution into the expressions for the original functions and")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2
             "the inequalities is only done if you select all equations with `;' .")
            NIL)
           (SETQ L (SELECT_FROM_LIST PDE NIL))
           (COND
            (L
             (PROGN
              (COND ((NOT_INCLUDED PDE L) (SETQ PARTIAL_SUBS T))
                    (T (SETQ PARTIAL_SUBS NIL)))
              (SETQ L (DELETE P L)))))
           (COND
            (PARTIAL_SUBS
             (PROGN
              (CHANGE_PROMPT_TO "")
              (TERPRI)
              (PROGN
               (PRIN2
                "Should substitutions be done in the inequalities? (y/nil)")
               NIL)
              (PROG ()
               REPEATLABEL
                (SETQ H (REVAL1 (TERMREAD) T))
                (COND ((NOT (OR (EQUAL H T) (EQUAL H NIL))) (GO REPEATLABEL))))
              (RESTORE_INTERACTIVE_PROMPT))))))
         (T (SETQ L (DELETE P PDE))))))
      (COND
       ((NOT CONTRADICTION_)
        (PROGN
         (PROG (H)
           (SETQ H (CDR USERRULES_))
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND
               ((FREEOF (CADR H) F)
                (COND
                 ((FREEOF (CADDR H) F)
                  (SETQ NEW_USER_RULES (CONS H NEW_USER_RULES)))
                 (T
                  (PROGN
                   (COND
                    (PRINT_
                     (PROGN
                      (TERPRI)
                      (PROGN (PRIN2 "The current substitution") NIL)
                      (TERPRI)
                      (PROGN
                       (ASSGNPRI (AEVAL F) NIL 'FIRST)
                       (ASSGNPRI (AEVAL " = ") NIL NIL)
                       (ASSGNPRI (AEVAL EX) NIL 'LAST))
                      (PROGN (PRIN2 "affects the following LET rule:") NIL)
                      (TERPRI)
                      (ASSGNPRI (AEVAL (LIST 'LIST H)) NIL 'ONLY))))
                   (AEVAL (CLEARRULES (LIST (LIST 'LIST H))))
                   (SETQ H
                           (LIST (CAR H) (CADR H)
                                 (MK*SQ
                                  (SUBSQ (SIMP (CADDR H))
                                         (LIST (CONS F EX))))))
                   (COND
                    (PRINT_
                     (PROGN
                      (PROGN (PRIN2 "which therefore is modified to:") NIL)
                      (ASSGNPRI (AEVAL (LIST 'LIST H)) NIL 'ONLY)
                      NIL)))
                   (SETQ NEW_USER_RULES (CONS H NEW_USER_RULES))
                   (AEVAL (LET (LIST (LIST 'LIST H))))
                   NIL))))
               (T
                (PROGN
                 (COND
                  (PRINT_
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "The current substitution affects the following LET rule")
                     NIL)
                    (TERPRI)
                    (PROGN (PRIN2 "which therefore has to be deleted:") NIL)
                    (ASSGNPRI (AEVAL (LIST 'LIST H)) NIL 'ONLY))))
                 (SETQ PDE (MOVERULE2EQN H PDE))
                 NIL))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (SETQ USERRULES_ (CONS 'LIST (REVERSE NEW_USER_RULES)))
         NIL)))
      (COND
       ((NOT CONTRADICTION_)
        (PROGN
         (SETQ INEQ_BAK INEQ_)
         (SETQ INEQ_OR_BAK INEQ_OR)
         (COND
          ((AND (NOT ISE) (OR (NOT PARTIAL_SUBS) H))
           (SIMP_ALL_INEQ_WITH_SUBST_SQ EX F PDE)))
         NIL)))
      (COND
       ((NOT CONTRADICTION_)
        (PROGN
         (COND
          ((AND (NOT ISE) (NOT PARTIAL_SUBS))
           (COND
            ((AND LAZY_EVAL (NULL LIN_PROBLEM))
             (PROGN
              (SETQ WAS_SUBST T)
              (SETQ FSUB_ (CONS (CONS F EX) FSUB_))
              NIL))
            (T
             (PROGN
              (SETQ WAS_SUBST (SUB_IN_FORG F EX FORG))
              (SETQ FORG (CAR WAS_SUBST))
              (SETQ WAS_SUBST (CDR WAS_SUBST)))))))
         (COND
          ((AND ISE (NOT CONTRADICTION_))
           (PROGN
            (SETQ H NIL)
            (SETQ VL (GET P 'VARS))
            (SETQ FL (GET P 'FCTS))
            (SETQ NOF (CAAR (GET P 'STARDE)))
            (PROG ()
             WHILELABEL
              (COND ((NOT L) (RETURN NIL)))
              (PROGN
               (COND
                ((AND (SETQ STDE (GET (CAR L) 'STARDE)) (LEQ NOF (CAAR STDE))
                      (NOT (NOT_INCLUDED VL (GET (CAR L) 'VARS)))
                      (NOT (NOT_INCLUDED FL (GET (CAR L) 'FCTS))))
                 (SETQ H (CONS (CAR L) H))))
               (SETQ L (CDR L)))
              (GO WHILELABEL))
            (SETQ L H)
            NIL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND L (NOT CONTRADICTION_))) (RETURN NIL)))
           (PROGN
            (COND
             ((MEMBER F (GET (CAR L) 'FCTS))
              (COND
               ((AND (NOT EXPERT_MODE) PLIM
                     (GREATERP (TIMES SLIM (GET (CAR L) 'LENGTH)) PLIM))
                (SETQ TOO_LARGE T))
               (T
                (PROGN
                 (SETQ PDE
                         (EQINSERT
                          (DO_ONE_SUBST EX F (CAR L) FTEM_
                           (UNION VL (GET (CAR L) 'VARS)) (GET P 'LEVEL) P PDE)
                          (DELETE (CAR L) PDE)))
                 (PROG (H)
                   (SETQ H PDE)
                  LAB
                   (COND ((NULL H) (RETURN NIL)))
                   ((LAMBDA (H) (DROP_RL_WITH (CAR L) H)) (CAR H))
                   (SETQ H (CDR H))
                   (GO LAB))
                 (PUT (CAR L) 'RL_WITH NIL)
                 (PROG (H)
                   (SETQ H PDE)
                  LAB
                   (COND ((NULL H) (RETURN NIL)))
                   ((LAMBDA (H) (DROP_DEC_WITH (CAR L) H 'DEC_WITH_RL))
                    (CAR H))
                   (SETQ H (CDR H))
                   (GO LAB))
                 (PUT (CAR L) 'DEC_WITH_RL NIL)
                 (FLAG (LIST (CAR L)) 'TO_INT)
                 (SETQ WAS_SUBST T))))))
            (SETQ L (CDR L)))
           (GO WHILELABEL))
         (COND
          ((AND PRINT_ (NOT CONTRADICTION_) WAS_SUBST)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "Substitution of ") NIL)
            (FCTPRINT (LIST F))
            (COND
             ((CDR (GET P 'FCTS))
              (PROGN
               (PROGN (PRIN2 " by an ") NIL)
               (COND (ISE (PROGN (PRIN2 "(separable) ") NIL)))
               (PROGN (PRIN2 "expression in ") NIL)
               (TERPRI)
               (FCTPRINT (DELETE F (GET P 'FCTS))))))
            (PROGN (PRIN2 " found in ") (PRIN2 P) (PRIN2 " : ") NIL)
            (EQPRINT (LIST 'EQUAL F EX)))))
         (COND
          ((AND ISE (NOT CONTRADICTION_))
           (PROGN
            (PUT P 'FCTEVAL_LIN NIL)
            (PUT P 'FCTEVAL_NCA NIL)
            (PUT P 'FCTEVAL_NLI NIL)
            (REMFLAG (LIST P) 'TO_EVAL)
            (SETQ MD MD)
            NIL)))
         (COND
          ((AND (NOT ISE) (NOT KEEP_EQN) (NOT TOO_LARGE) (NOT PARTIAL_SUBS)
                (NOT CONTRADICTION_))
           (PROGN
            (COND
             ((OR (NULL LAZY_EVAL) LIN_PROBLEM)
              (PROGN
               (SETQ H T)
               (PROG (L)
                 (SETQ L FORG)
                LAB
                 (COND ((NULL L) (RETURN NIL)))
                 ((LAMBDA (L)
                    (COND
                     ((PAIRP L)
                      (COND ((EQUAL (CADR L) F) (SETQ H NIL)) (T NIL)))
                     ((EQUAL L F) (SETQ H NIL))))
                  (CAR L))
                 (SETQ L (CDR L))
                 (GO LAB))
               (COND (H (DROP_FCT F)))
               NIL)))
            (SETQ WAS_SUBST T)
            (SETQ FTEM_ (DELETE F FTEM_))
            (SETQ FLIN_ (DELETE F FLIN_))
            (SETQ PDE (DROP_PDE P PDE 0))
            NIL)))
         (SETQ RES (LIST PDE FORG P)))))
      (COND (MODULAR_COMP (OFF (LIST 'MODULAR))))
      (STOP_LET_RULES RULI)
      (COND
       ((NULL WAS_SUBST)
        (PROGN (SETQ INEQ_ INEQ_BAK) (SETQ INEQ_OR INEQ_OR_BAK))))
      (COND ((NOT CONTRADICTION_) (RETURN (CONS WAS_SUBST RES)))))) 
(PUT 'MAKE_SUBST 'NUMBER-OF-ARGS 14) 
(PUT 'MAKE_SUBST 'DEFINED-ON-LINE '832) 
(PUT 'MAKE_SUBST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'MAKE_SUBST 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE MAKE_SUBST
    (PDES FORG VL L1 LENGTH_LIMIT PDELIMIT LESS_VARS NO_DF NO_CASES LIN_SUBST
     MIN_GROWTH COST_LIMIT KEEP_EQN SUB_FC)
    (PROG (P Q R L H HH H3 CASES_ W MD TEMPCHNG PLIM MOD_SWITCHED)
      (PROG (H)
        (SETQ H L1)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H) (COND ((NOT (FREEOF PDES H)) (SETQ L (CONS H L)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (COND ((AND L1 (NULL L)) (RETURN NIL)) (T (SETQ L1 (REVERSE L))))
      (SETQ L NIL)
      (COND
       (EXPERT_MODE
        (PROGN
         (PROGN (PRIN2 "Which PDE should be used for substitution?") NIL)
         (TERPRI)
         (SETQ L1 (SELECTPDES PDES 1))
         NIL)))
      (COND
       ((AND SUB_FC L1 (NULL (CDR L1)))
        (PROGN
         (SETQ P (CAR L1))
         (COND
          ((NULL (FCTEVAL P))
           (COND
            (PRINT_
             (PROGN
              (PROGN
               (PRIN2 "##### Strange: equation ")
               (PRIN2 P)
               (PRIN2 " was to be solved for ")
               (PRIN2 SUB_FC)
               (PRIN2 " but facteval says that is not possible.")
               NIL)
              (TERPRI))))))
         (SETQ H (GET P 'FCTEVAL_LIN))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND H (NEQ SUB_FC (CDAR H)))) (RETURN NIL)))
           (SETQ H (CDR H))
           (GO WHILELABEL))
         (COND (H (SETQ HH 1))
               (T
                (PROGN
                 (SETQ H (GET P 'FCTEVAL_NCA))
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND H (NEQ SUB_FC (CDAR H)))) (RETURN NIL)))
                   (SETQ H (CDR H))
                   (GO WHILELABEL))
                 (COND (H (SETQ HH 2))
                       (T
                        (PROGN
                         (SETQ H (GET P 'FCTEVAL_NLI))
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT (AND H (NEQ SUB_FC (CDAR H)))) (RETURN NIL)))
                           (SETQ H (CDR H))
                           (GO WHILELABEL))
                         (COND (H (SETQ HH 3)))))))))
         (COND (H (SETQ W (LIST HH (CAR L1) (CAR H))))))))
      (COND ((AND SUB_FC (NULL W)) (RETURN NIL)))
     AGAIN
      (COND
       ((OR (AND SUB_FC W)
            (AND MIN_GROWTH
                 (SETQ W (ERR_CATCH_MINSUB PDES L1 COST_LIMIT NO_CASES)))
            (AND (NULL MIN_GROWTH)
                 (SETQ W
                         (GET_SUBST PDES L1 LENGTH_LIMIT LESS_VARS NO_DF
                          NO_CASES))))
        (COND
         ((AND (NULL *BATCH_MODE) (NULL EXPERT_MODE) CONFIRM_SUBST
               (PROGN
                (COND
                 (PRINT_
                  (PROGN
                   (TERPRI)
                   (PROGN
                    (PRIN2 "Proposal: Substitution of  ")
                    (PRIN2 (CDADDR W))
                    NIL)
                   (TERPRI)
                   (PROGN
                    (PRIN2 "          using equation ")
                    (PRIN2 (CADR W))
                    (PRIN2 ": ")
                    NIL)
                   NIL)))
                (COND
                 ((AND PRINT_ (LEQ (GET (CADR W) 'PRINTLENGTH) PRINT_))
                  (PRINT_STARS (CADR W))))
                (COND (PRINT_ (PROGN (TYPEEQ (CADR W)) (TERPRI))))
                (COND
                 ((LEQ (CAR W) 2)
                  (PROGN
                   (PRIN2 "No case distinctions will be necessary.")
                   NIL))
                 (T
                  (PROGN (PRIN2 "Case distinctions will be necessary.") NIL)))
                (TERPRI)
                (PROGN (PRIN2 "The coefficient is:") NIL)
                (MATHPRINT (FACTORIZE (LIST '*SQ (CAADDR W) T)))
                (PROGN
                 (PRIN2
                  "Accept? (Enter y or n or p for stopping substitution) ")
                 NIL)
                (CHANGE_PROMPT_TO "")
                (PROG ()
                 REPEATLABEL
                  (SETQ H (TERMREAD))
                  (COND
                   ((NOT (OR (EQUAL H 'Y) (EQUAL H 'N) (EQUAL H 'P)))
                    (GO REPEATLABEL))))
                (RESTORE_INTERACTIVE_PROMPT)
                (COND
                 ((EQUAL H 'N)
                  (PROGN
                   (SETQ TEMPCHNG (CONS W TEMPCHNG))
                   (COND
                    ((EQUAL (CAR W) 1)
                     (PROGN
                      (SETQ HH (GET (CADR W) 'FCTEVAL_LIN))
                      (SETQ HH (DELETE (CADDR W) HH))
                      (PUT (CADR W) 'FCTEVAL_LIN HH)))
                    ((EQUAL (CAR W) 2)
                     (PROGN
                      (SETQ HH (GET (CADR W) 'FCTEVAL_NCA))
                      (SETQ HH (DELETE (CADDR W) HH))
                      (PUT (CADR W) 'FCTEVAL_NCA HH)))
                    (T
                     (PROGN
                      (SETQ HH (GET (CADR W) 'FCTEVAL_NLI))
                      (SETQ HH (DELETE (CADDR W) HH))
                      (PUT (CADR W) 'FCTEVAL_NLI HH)
                      (COND
                       ((EQUAL (CAR W) 3)
                        (PROGN
                         (SETQ HH (GET (CADR W) 'FCT_NLI_LIN))
                         (SETQ HH (DELETE (CADDR W) HH))
                         (PUT (CADR W) 'FCT_NLI_LIN HH)))
                       ((EQUAL (CAR W) 4)
                        (PROGN
                         (SETQ HH (GET (CADR W) 'FCT_NLI_NCA))
                         (SETQ HH (DELETE (CADDR W) HH))
                         (PUT (CADR W) 'FCT_NLI_NCA HH)))
                       ((EQUAL (CAR W) 5)
                        (PROGN
                         (SETQ HH (GET (CADR W) 'FCT_NLI_NLI))
                         (SETQ HH (DELETE (CADDR W) HH))
                         (PUT (CADR W) 'FCT_NLI_NLI HH)))
                       ((EQUAL (CAR W) 6)
                        (PROGN
                         (SETQ HH (GET (CADR W) 'FCT_NLI_NUS))
                         (SETQ HH (DELETE (CADDR W) HH))
                         (PUT (CADR W) 'FCT_NLI_NUS HH)))))))
                   (COND
                    ((AND (NULL HH) (NULL (GET (CADR W) 'FCTEVAL_LIN))
                          (NULL (GET (CADR W) 'FCTEVAL_NCA))
                          (NULL (GET (CADR W) 'FCTEVAL_NLI)))
                     (REMFLAG (LIST (CADR W)) 'TO_EVAL))))))
                (COND ((EQUAL H 'P) (SETQ L1 NIL)))
                (COND ((OR (EQUAL H 'N) (EQUAL H 'P)) T) (T NIL))))
          (GO AGAIN))
         ((OR (EQUAL (CAR W) 1)
              (AND (EQUAL LIN_SUBST NIL)
                   (OR (EQUAL (CAR W) 2)
                       (AND (GREATERP (CAR W) 2) (MEMBER (CAADDR W) INEQ_)))))
          (PROGN
           (COND
            ((AND PDELIMIT
                  (IN_CYCLE
                   (LIST 'SUBST STEPCOUNTER_ (CDADDR W)
                         (GET (CADR W) 'PRINTLENGTH) PDELIMIT)))
             (SETQ PLIM NIL))
            (T (SETQ PLIM PDELIMIT)))
           (SETQ L
                   (DO_SUBST (CAR W) (CADR W) (CADDR W) PDES FORG VL PLIM
                    KEEP_EQN))
           (COND
            ((AND L (NULL (CAR L)))
             (PROGN
              (SETQ L1 (DELETE (CADR W) L1))
              (COND
               (L1
                (PROGN
                 (SETQ PDES (CADR L))
                 (SETQ FORG (CADDR L))
                 (SETQ L NIL)
                 (GO AGAIN)))
               (T (SETQ L NIL))))))
           (COND
            (L
             (PROGN
              (SETQ L (CDR L))
              (ADD_TO_LAST_STEPS
               (LIST 'SUBST STEPCOUNTER_ (CDADDR W) (GET (CADR W) 'PRINTLENGTH)
                     PDELIMIT)))))))
         ((AND (NULL LIN_SUBST) (NULL NO_CASES))
          (PROGN
           (SETQ MD (CAR W))
           (SETQ P (CADR W))
           (SETQ W (CADDR W))
           (COND
            ((AND PDELIMIT
                  (IN_CYCLE
                   (LIST 'SUBST STEPCOUNTER_ W (GET P 'PRINTLENGTH) PDELIMIT)))
             (SETQ PDELIMIT NIL)))
           (BACKUP_TO_FILE PDES FORG NIL)
           (SETQ Q
                   (MKEQSQ (CAR W) NIL NIL (GET P 'FCTS) (GET P 'VARS)
                    ALLFLAGS_ T (LIST 0) NIL PDES))
           (SETQ R NIL)
           (COND
            ((NOT CONTRADICTION_)
             (PROGN
              (SETQ HH
                      (ADDSQ (GET P 'SQVAL)
                             (NEGSQ (MULTSQ (CAR W) (SIMP (CDR W))))))
              (COND
               ((NOT (SQZEROP HH))
                (SETQ R
                        (MKEQSQ HH NIL NIL (GET P 'FCTS) (GET P 'VARS)
                         ALLFLAGS_ T (LIST 0) NIL PDES)))))))
           (COND
            (CONTRADICTION_
             (PROGN
              (COND
               (PRINT_
                (PROGN
                 (PROGN
                  (PRIN2 "Therefore no special investigation whether the ")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "coefficient of a function to be substituted is zero.")
                  NIL)
                 NIL)))
              (SETQ CONTRADICTION_ NIL)
              (SETQ H (RESTORE_BACKUP_FROM_FILE PDES FORG NIL))
              (SETQ PDES (CAR H))
              (SETQ FORG (CADR H))
              (DELETE_BACKUP)
              (COND ((NULL Q) (SETQ H (LIST (CAR W))))
                    (T
                     (PROGN
                      (SETQ H (GET Q 'FAC))
                      (COND
                       ((NOT (PAIRP H)) (SETQ H (LIST (GET Q 'SQVAL))))))))
              (PROG (L)
                (SETQ L H)
               LAB
                (COND ((NULL L) (RETURN NIL)))
                ((LAMBDA (L) (ADDSQINEQ PDES L (COND (Q NIL) (T T)))) (CAR L))
                (SETQ L (CDR L))
                (GO LAB))
              (DROP_PDE Q NIL NIL)
              (COND (R (DROP_PDE R NIL NIL)))
              (SETQ L (DO_SUBST MD P W PDES FORG VL PDELIMIT KEEP_EQN))
              (COND
               ((AND L (NULL (CAR L)))
                (PROGN
                 (SETQ L1 (DELETE P L1))
                 (COND
                  (L1
                   (PROGN
                    (SETQ PDES (CADR L))
                    (SETQ FORG (CADDR L))
                    (SETQ L NIL)
                    (GO AGAIN)))))))
              (COND
               (L
                (PROGN
                 (SETQ L (CDR L))
                 (ADD_TO_LAST_STEPS
                  (LIST 'SUBST STEPCOUNTER_ (CDR W) (GET P 'PRINTLENGTH)
                        PDELIMIT)))))))
            (T
             (PROGN
              (REMFLAG (LIST P) 'TO_EVAL)
              (COND
               (PRINT_
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "for the substitution of ")
                  (PRIN2 (CDR W))
                  (PRIN2 " by ")
                  (PRIN2 P)
                  NIL)
                 (PROGN
                  (PRIN2 " we have to consider the case 0=")
                  (PRIN2 Q)
                  (PRIN2 ": ")
                  NIL)
                 (EQPRINT (LIST 'EQUAL 0 (LIST '*SQ (CAR W) T))))))
              (SETQ PDES (EQINSERT Q (DROP_PDE P PDES NIL)))
              (COND
               ((FREEOF PDES Q)
                (PROGN
                 (COND
                  (PRINT_
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (PRIN2 "It turns out that the coefficient of ")
                     (PRIN2 (CDR W))
                     (PRIN2 " in ")
                     (PRIN2 P)
                     (PRIN2 " is zero due")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "to other equations. Therefore no substitution is made and")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2 "equation ")
                     (PRIN2 P)
                     (PRIN2 " will be updated instead.")
                     NIL)
                    (TERPRI))))
                 (SETQ H (RESTORE_BACKUP_FROM_FILE PDES FORG NIL))
                 (SETQ PDES (CAR H))
                 (SETQ FORG (CADR H))
                 (DELETE_BACKUP)
                 (COND
                  ((AND MODULAR_COMP (NULL *MODULAR))
                   (PROGN (ON (LIST 'MODULAR)) (SETQ MOD_SWITCHED T))))
                 (SETQ HH
                         (ADDSQ (GET P 'SQVAL)
                                (NEGSQ (MULTSQ (CAR W) (SIMP (CDR W))))))
                 (COND (MOD_SWITCHED (OFF (LIST 'MODULAR))))
                 (COND
                  ((SQZEROP HH)
                   (PROGN (DROP_PDE P PDES NIL) (SETQ PDES (DELETE P PDES))))
                  (T
                   (PROGN
                    (UPDATESQ P HH NIL NIL (GET P 'FCTS) (GET P 'VARS) T
                     (LIST 0) PDES)
                    (DROP_PDE_FROM_IDTIES P PDES NIL)
                    (DROP_PDE_FROM_PROPERTIES P PDES))))
                 (DROP_PDE Q PDES NIL)
                 (COND (R (DROP_PDE R PDES NIL)))
                 (SETQ L (LIST PDES FORG P))))
               (T
                (PROGN
                 (COND (R (SETQ PDES (EQINSERT R PDES))))
                 (COND
                  (PRINT_
                   (PROGN
                    (PROGN
                     (PRIN2
                      "The coefficient to be set = 0 in the first subcase is:")
                     NIL)
                    (SETQ HH PRINT_)
                    (SETQ PRINT_ 30)
                    (TYPEEQLIST (LIST Q))
                    (SETQ PRINT_ HH))))
                 (SETQ TO_DO_LIST
                         (CONS (LIST 'SUBST_LEVEL_35 (LIST Q)) TO_DO_LIST))
                 (CP_SQ2P_VAL Q)
                 (SETQ H3 (GET Q 'PVAL))
                 (START_LEVEL 1 (LIST (LIST 'EQUAL 0 H3)))
                 (SETQ H (GET Q 'FAC))
                 (COND ((NOT (PAIRP H)) (SETQ H (LIST (GET Q 'SQVAL)))))
                 (SETQ RECYCLE_FCTS NIL)
                 (SETQ L (CRACKMAIN_IF_POSSIBLE_REMOTE PDES FORG))
                 (SETQ HH (RESTORE_AND_MERGE L PDES FORG))
                 (SETQ PDES (CAR HH))
                 (SETQ FORG (CADR HH))
                 (DELETE_BACKUP)
                 (START_LEVEL 2 (LIST (LIST 'INEQ 0 H3)))
                 (COND
                  (PRINT_
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (PRIN2 "now back to the substitution of ")
                     (PRIN2 (CDR W))
                     (PRIN2 " by ")
                     (PRIN2 P)
                     NIL)
                    NIL)))
                 (PROG (H3)
                   (SETQ H3 H)
                  LAB
                   (COND ((NULL H3) (RETURN NIL)))
                   ((LAMBDA (H3) (ADDSQINEQ PDES H3 NIL)) (CAR H3))
                   (SETQ H3 (CDR H3))
                   (GO LAB))
                 (FCTEVAL P)
                 (COND
                  (CONTRADICTION_
                   (PROGN (SETQ CASES_ T) (SETQ CONTRADICTION_ NIL)))
                  (T
                   (PROGN
                    (SETQ HH (COEFFN (LIST '*SQ (GET P 'SQVAL) T) (CDR W) 1))
                    (SETQ W
                            (CONS
                             (COND
                              ((AND (PAIRP HH) (EQUAL (CAR HH) '*SQ))
                               (CADR HH))
                              (T (SIMP HH)))
                             (CDR W)))
                    (SETQ TO_DO_LIST
                            (CONS
                             (CONS 'SUBST_LEVEL_35
                                   (COND (SUB_FC (LIST (LIST P) SUB_FC))
                                         (T (LIST (LIST P)))))
                             TO_DO_LIST))
                    (COND
                     ((GREATERP MD 2)
                      (PROGN
                       (SETQ H (GET P 'FCTEVAL_NLI))
                       (COND
                        ((MEMBER W H)
                         (PROGN
                          (SETQ H (DELETE W H))
                          (PUT P 'FCTEVAL_NLI H)
                          (PUT P 'FCTEVAL_NCA (CONS W (GET P 'FCTEVAL_NCA)))
                          (COND
                           ((EQUAL MD 3)
                            (PROGN
                             (SETQ H (GET P 'FCT_NLI_LIN))
                             (SETQ H (DELETE W H))
                             (PUT P 'FCT_NLI_LIN H)
                             NIL))
                           ((EQUAL MD 4)
                            (PROGN
                             (SETQ H (GET P 'FCT_NLI_NCA))
                             (SETQ H (DELETE W H))
                             (PUT P 'FCT_NLI_NCA H)
                             NIL))
                           ((EQUAL MD 5)
                            (PROGN
                             (SETQ H (GET P 'FCT_NLI_NLI))
                             (SETQ H (DELETE W H))
                             (PUT P 'FCT_NLI_NLI H)
                             NIL))
                           ((EQUAL MD 6)
                            (PROGN
                             (SETQ H (GET P 'FCT_NLI_NUS))
                             (SETQ H (DELETE W H))
                             (PUT P 'FCT_NLI_NUS H)
                             NIL)))))))))
                    (SETQ CASES_ T)
                    (SETQ H (CRACKMAIN_IF_POSSIBLE_REMOTE PDES FORG))
                    (COND (CONTRADICTION_ (SETQ CONTRADICTION_ NIL))
                          (T (SETQ L (MERGE_CRACK_RETURNS H L)))))))))))))
           NIL)))))
      (COND
       ((AND (NULL *BATCH_MODE) (NULL EXPERT_MODE) CONFIRM_SUBST)
        (PROG ()
         WHILELABEL
          (COND ((NOT TEMPCHNG) (RETURN NIL)))
          (PROGN
           (SETQ W (CAR TEMPCHNG))
           (SETQ TEMPCHNG (CDR TEMPCHNG))
           (COND
            ((EQUAL (CAR W) 1)
             (PROGN
              (SETQ HH (GET (CADR W) 'FCTEVAL_LIN))
              (SETQ HH (CONS (CADDR W) HH))
              (PUT (CADR W) 'FCTEVAL_LIN HH)))
            ((EQUAL (CAR W) 2)
             (PROGN
              (SETQ HH (GET (CADR W) 'FCTEVAL_NCA))
              (SETQ HH (CONS (CADDR W) HH))
              (PUT (CADR W) 'FCTEVAL_NCA HH)))
            (T
             (PROGN
              (SETQ HH (GET (CADR W) 'FCTEVAL_NLI))
              (SETQ HH (CONS (CADDR W) HH))
              (PUT (CADR W) 'FCTEVAL_NLI HH)
              (COND
               ((EQUAL (CAR W) 3)
                (PROGN
                 (SETQ HH (GET (CADR W) 'FCT_NLI_LIN))
                 (SETQ HH (CONS (CADDR W) HH))
                 (PUT (CADR W) 'FCT_NLI_LIN HH)))
               ((EQUAL (CAR W) 4)
                (PROGN
                 (SETQ HH (GET (CADR W) 'FCT_NLI_NCA))
                 (SETQ HH (CONS (CADDR W) HH))
                 (PUT (CADR W) 'FCT_NLI_NCA HH)))
               ((EQUAL (CAR W) 5)
                (PROGN
                 (SETQ HH (GET (CADR W) 'FCT_NLI_NLI))
                 (SETQ HH (CONS (CADDR W) HH))
                 (PUT (CADR W) 'FCT_NLI_NLI HH)))
               ((EQUAL (CAR W) 6)
                (PROGN
                 (SETQ HH (GET (CADR W) 'FCT_NLI_NUS))
                 (SETQ HH (CONS (CADDR W) HH))
                 (PUT (CADR W) 'FCT_NLI_NUS HH)))))))
           (FLAG (LIST (CADR W)) 'TO_EVAL))
          (GO WHILELABEL))))
      (RETURN (COND (CONTRADICTION_ NIL) (CASES_ (LIST L)) (T L))))) 
(PUT 'SUBST_LEVEL_0 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_0 'DEFINED-ON-LINE '1231) 
(PUT 'SUBST_LEVEL_0 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_0 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_0 TARGET_LIMIT_3 T NIL T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_03 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_03 'DEFINED-ON-LINE '1248) 
(PUT 'SUBST_LEVEL_03 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_03 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_03 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_0 TARGET_LIMIT_3 NIL T T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_04 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_04 'DEFINED-ON-LINE '1265) 
(PUT 'SUBST_LEVEL_04 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_04 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_04 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_1 TARGET_LIMIT_1 NIL T T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_05 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_05 'DEFINED-ON-LINE '1282) 
(PUT 'SUBST_LEVEL_05 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_05 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_05 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL T T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_1 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_1 'DEFINED-ON-LINE '1299) 
(PUT 'SUBST_LEVEL_1 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_1 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_1 TARGET_LIMIT_2 T NIL NIL NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_2 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_2 'DEFINED-ON-LINE '1316) 
(PUT 'SUBST_LEVEL_2 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_2 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 T NIL T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_3 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_3 'DEFINED-ON-LINE '1333) 
(PUT 'SUBST_LEVEL_3 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_3 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_2 TARGET_LIMIT_1 NIL NIL NIL NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_33 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_33 'DEFINED-ON-LINE '1350) 
(PUT 'SUBST_LEVEL_33 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_33 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_33 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL NIL T T NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_35 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_35 'DEFINED-ON-LINE '1367) 
(PUT 'SUBST_LEVEL_35 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_35 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_35 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL NIL T NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_4 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_4 'DEFINED-ON-LINE '1384) 
(PUT 'SUBST_LEVEL_4 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_4 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL NIL NIL NIL NIL NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_45 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_45 'DEFINED-ON-LINE '1401) 
(PUT 'SUBST_LEVEL_45 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_45 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_45 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL NIL T NIL T COST_LIMIT5 NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'SUBST_LEVEL_5 'NUMBER-OF-ARGS 1) 
(PUT 'SUBST_LEVEL_5 'DEFINED-ON-LINE '1418) 
(PUT 'SUBST_LEVEL_5 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBST_LEVEL_5 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBST_LEVEL_5 (ARGLIST)
    (MAKE_SUBST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDDR ARGLIST)
     SUBST_3 TARGET_LIMIT_3 NIL NIL NIL NIL T NIL NIL
     (COND ((GREATERP (LENGTH ARGLIST) 4) (NTH ARGLIST 5)) (T NIL)))) 
(PUT 'BEST_FAC_PDE 'NUMBER-OF-ARGS 1) 
(PUT 'BEST_FAC_PDE 'DEFINED-ON-LINE '1436) 
(PUT 'BEST_FAC_PDE 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'BEST_FAC_PDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BEST_FAC_PDE (PDES)
    (PROG (P MD MDGR MTM F DGR TM BESTP)
      (SETQ MD 1000)
      (SETQ MTM 100000)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ MDGR 0)
            (PROG (F)
              (SETQ F (GET P 'FAC))
             LAB
              (COND ((NULL F) (RETURN NIL)))
              ((LAMBDA (F)
                 (PROGN
                  (SETQ DGR (PDE_DEGREE_SQ F (SMEMBERL (GET P 'RATIONAL) F)))
                  (COND ((GREATERP DGR MDGR) (SETQ MDGR DGR)))))
               (CAR F))
              (SETQ F (CDR F))
              (GO LAB))
            (SETQ TM (GET P 'LENGTH))
            (COND
             ((OR (LESSP MDGR MD) (AND (EQUAL MDGR MD) (LESSP TM MTM)))
              (PROGN (SETQ BESTP P) (SETQ MD MDGR) (SETQ MTM TM))))
            NIL))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (LIST BESTP MD MTM)))) 
(PUT 'START_LET_RULES 'NUMBER-OF-ARGS 0) 
(FLAG '(START_LET_RULES) 'OPFN) 
(PUT 'START_LET_RULES 'DEFINED-ON-LINE '1454) 
(PUT 'START_LET_RULES 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'START_LET_RULES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE START_LET_RULES NIL
    (PROG (RULI)
      (SETQ OLDRULES* NIL)
      (SETQ RULI (AEVAL (LIST 'LIST)))
      (COND
       ((EVALNEQ (AEVAL (LIST 'COT '%X))
                 (AEVAL (LIST 'QUOTIENT 1 (LIST 'TAN '%X))))
        (AEVAL (LET '(EXPLOG_)))))
      (COND
       ((EVALNEQ (AEVAL USERRULES_) (AEVAL (LIST 'LIST)))
        (AEVAL (LET (LIST USERRULES_)))))
      (COND
       ((EVALNEQ
         (AEVAL
          (LIST 'PLUS (LIST 'EXPT (LIST 'SIN '%X) 2)
                (LIST 'EXPT (LIST 'COS '%X) 2)))
         1)
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG1_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'EXPT (LIST 'COSH '%X) 2))
                 (AEVAL (LIST 'PLUS (LIST 'EXPT (LIST 'SINH '%X) 2) 1)))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG2_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ
         (AEVAL
          (LIST 'PLUS
                (LIST 'TIMES (LIST 'SIN '%X)
                      (LIST 'TAN (LIST 'QUOTIENT '%X 2)))
                (LIST 'COS '%X)))
         1)
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG3_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ
         (AEVAL
          (LIST 'DIFFERENCE
                (LIST 'TIMES (LIST 'SIN '%X)
                      (LIST 'COT (LIST 'QUOTIENT '%X 2)))
                (LIST 'COS '%X)))
         1)
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG4_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ
         (AEVAL
          (LIST 'PLUS (LIST 'COS (LIST 'TIMES 2 '%X))
                (LIST 'TIMES 2 (LIST 'EXPT (LIST 'SIN '%X) 2))))
         1)
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG5_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SIN (LIST 'TIMES 2 '%X)))
                 (AEVAL (LIST 'TIMES 2 (LIST 'COS '%X) (LIST 'SIN '%X))))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG6_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SINH (LIST 'TIMES 2 '%X)))
                 (AEVAL (LIST 'TIMES 2 (LIST 'SINH '%X) (LIST 'COSH '%X))))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG7_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'COSH (LIST 'TIMES 2 '%X)))
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES 2 (LIST 'EXPT (LIST 'COSH '%X) 2)) 1)))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(TRIG8_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SQRT (LIST 'TIMES '%X '%Y)))
                 (AEVAL (LIST 'TIMES (LIST 'SQRT '%X) (LIST 'SQRT '%Y))))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(SQRT1_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'SQRT (LIST 'QUOTIENT '%X '%Y)))
                 (AEVAL (LIST 'QUOTIENT (LIST 'SQRT '%X) (LIST 'SQRT '%Y))))
        (PROGN
         (SETQ RULI (AEVAL (LIST 'CONS 1 RULI)))
         (AEVAL (LET '(SQRT2_)))))
       (T (SETQ RULI (AEVAL (LIST 'CONS 0 RULI)))))
      (RETURN (AEVAL RULI)))) 
(PUT 'STOP_LET_RULES 'NUMBER-OF-ARGS 1) 
(FLAG '(STOP_LET_RULES) 'OPFN) 
(PUT 'STOP_LET_RULES 'DEFINED-ON-LINE '1473) 
(PUT 'STOP_LET_RULES 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'STOP_LET_RULES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STOP_LET_RULES (RULI)
    (PROG ()
      (AEVAL (CLEARRULES (LIST 'EXPLOG_)))
      (COND
       ((AND (EVALNEQ (AEVAL USERRULES_) (AEVAL (LIST 'LIST)))
             (BOOLVALUE*
              (REVALX
               (ZEROP
                (REVAL1
                 (LIST 'DIFFERENCE (CAR (CDADR USERRULES_))
                       (CADR (CDADR USERRULES_)))
                 T)))))
        (AEVAL (CLEARRULES (LIST USERRULES_)))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'SQRT2_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'SQRT1_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG8_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG7_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG6_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG5_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG4_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG3_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG2_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FIRST RULI)) 1)
        (AEVAL (CLEARRULES (LIST 'TRIG1_)))))
      (SETQ RULI (AEVAL (LIST 'REST RULI))))) 
(PUT 'FBTS 'NUMBER-OF-ARGS 2) 
(PUT 'FBTS 'DEFINED-ON-LINE '1497) 
(PUT 'FBTS 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'FBTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FBTS (A B)
    (AND (LEQ (CADR A) (CADR B)) (LEQ (CADDR A) (CADDR B))
         (LEQ (CADDDR A) (CADDDR B)))) 
(PUT 'LIST_SUBS 'NUMBER-OF-ARGS 4) 
(PUT 'LIST_SUBS 'DEFINED-ON-LINE '1504) 
(PUT 'LIST_SUBS 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'LIST_SUBS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIST_SUBS (P FEVL FLI MDU)
    (PROG (A F NCO NTE CPY CC NTRY)
      (PROG (A)
        (SETQ A FEVL)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ F (CDR A))
            (SETQ NCO (NO_OF_TM_SF (CAR (CAR A))))
            (SETQ NTE (GET P 'TERMS))
            (SETQ NTE (COND ((EQUAL NTE 1) 0) (T (DIFFERENCE NTE NCO))))
            (SETQ NTRY (LIST P NCO NTE MDU))
            (SETQ CPY FLI)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND CPY (NEQ F (CAAR CPY)))) (RETURN NIL)))
              (SETQ CPY (CDR CPY))
              (GO WHILELABEL))
            (COND ((NULL CPY) (SETQ FLI (CONS (LIST F NTRY) FLI)))
                  (T
                   (PROGN
                    (SETQ CC (CDAR CPY))
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (AND CC (NULL (FBTS (CAR CC) NTRY))))
                        (RETURN NIL)))
                      (SETQ CC (CDR CC))
                      (GO WHILELABEL))
                    (COND
                     ((NULL CC)
                      (PROGN
                       (RPLACA CPY (CONS F (CONS NTRY (CDAR CPY))))
                       (SETQ CC (CDAR CPY))
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT (CDR CC)) (RETURN NIL)))
                         (COND ((FBTS NTRY (CADR CC)) (RPLACD CC (CDDR CC)))
                               (T (SETQ CC (CDR CC))))
                         (GO WHILELABEL))
                       NIL))))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (RETURN FLI))) 
(PUT 'CWRNO 'NUMBER-OF-ARGS 2) 
(PUT 'CWRNO 'DEFINED-ON-LINE '1546) 
(PUT 'CWRNO 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'CWRNO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CWRNO (N R)
    (PROGN
     (SETQ N (PLUS N (DIFFERENCE R 1)))
     (COND ((GREATERP (TIMES 2 R) N) (SETQ R (DIFFERENCE N R))))
     (QUOTIENT
      (PROG (I FORALL-RESULT)
        (SETQ I 1)
        (SETQ FORALL-RESULT 1)
       LAB1
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT (TIMES (PLUS N (DIFFERENCE 1 I)) FORALL-RESULT))
        (SETQ I (PLUS2 I 1))
        (GO LAB1))
      (PROG (I FORALL-RESULT)
        (SETQ I 1)
        (SETQ FORALL-RESULT 1)
       LAB1
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT (TIMES I FORALL-RESULT))
        (SETQ I (PLUS2 I 1))
        (GO LAB1))))) 
(PUT 'BESU 'NUMBER-OF-ARGS 4) 
(PUT 'BESU 'DEFINED-ON-LINE '1557) 
(PUT 'BESU 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'BESU 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE BESU (IC1 MDU1 IC2 MDU2)
    (OR (AND (LESSP MDU1 MDU2) (LEQ IC1 IC2))
        (AND (EQUAL MDU1 MDU2) (LESSP IC1 IC2))
        (AND (EQUAL MDU1 2) (LESSP IC1 (PLUS IC2 4)))
        (AND (EQUAL MDU1 3) (LESSP IC1 (PLUS IC2 25))))) 
(PUT 'SEARCH_SUBS 'NUMBER-OF-ARGS 4) 
(PUT 'SEARCH_SUBS 'DEFINED-ON-LINE '1567) 
(PUT 'SEARCH_SUBS 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SEARCH_SUBS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEARCH_SUBS (PDES SBPDES COST_LIMIT NO_CASES)
    (PROG (FLI P EL F FPL DV DRF D FFL HP FF NCO BE S NTE IC FP RM MC SUBLI MDU
           TR_SEARCH H)
      (PROG (P)
        (SETQ P SBPDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (FCTEVAL P)) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ FP SBPDES)
      (SETQ H NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FP (NULL H))) (RETURN NIL)))
        (COND ((GREATERP (GET (CAR FP) 'TERMS) 2) (SETQ FP NIL))
              ((NULL (SETQ H (GET (CAR FP) 'FCTEVAL_LIN))) (SETQ FP (CDR FP))))
        (GO WHILELABEL))
      (COND (FP (RETURN (LIST 1 (CAR FP) (CAR H)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT SBPDES) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR SBPDES))
         (SETQ SBPDES (CDR SBPDES))
         (SETQ FLI (LIST_SUBS P (GET P 'FCTEVAL_LIN) FLI 1))
         (SETQ FLI (LIST_SUBS P (GET P 'FCTEVAL_NCA) FLI 2))
         (COND
          ((NULL NO_CASES)
           (SETQ FLI (LIST_SUBS P (GET P 'FCTEVAL_NLI) FLI 3))))
         (COND
          (S
           (COND ((GREATERP (GET P 'LENGTH) (TIMES 3 S)) (SETQ SBPDES NIL))
                 (T NIL)))
          (FLI (SETQ S (GET P 'LENGTH)))))
        (GO WHILELABEL))
      (COND
       (TR_SEARCH
        (PROGN
         (PROGN
          (PRIN2
           "equations substitution: (eqn, no of coeff. t., no of other t., mdu)")
          NIL)
         (TERPRI)
         (PROG (EL)
           (SETQ EL FLI)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL) (PROGN (PROGN (PRIN2 EL) NIL) (TERPRI))) (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         NIL)))
      (COND
       (FLI
        (COND
         ((AND (NULL (CDR FLI)) (NULL (CDDAR FLI)))
          (RETURN
           (PROGN
            (SETQ FLI (CADAR FLI))
            (SETQ MDU (CADDDR FLI))
            (LIST MDU (CAR FLI)
                  (CAR
                   (GET (CAR FLI)
                        (COND ((EQUAL MDU 1) 'FCTEVAL_LIN)
                              ((EQUAL MDU 2) 'FCTEVAL_NCA)
                              (T 'FCTEVAL_NLI))))))))
         (T
          (PROG (EL)
            (SETQ EL FLI)
           LAB
            (COND ((NULL EL) (RETURN NIL)))
            ((LAMBDA (EL)
               (PROGN
                (SETQ F (CAR EL))
                (SETQ EL (CDR EL))
                (SETQ FPL NIL)
                (PROG (P)
                  (SETQ P PDES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (PROGN
                      (SETQ DV (GET P 'DERIVS))
                      (SETQ DRF NIL)
                      (PROG (D)
                        (SETQ D DV)
                       LAB
                        (COND ((NULL D) (RETURN NIL)))
                        ((LAMBDA (D)
                           (COND ((EQUAL (CAAR D) F) (SETQ DRF (CONS D DRF)))))
                         (CAR D))
                        (SETQ D (CDR D))
                        (GO LAB))
                      (SETQ FFL NIL)
                      (COND
                       (DRF
                        (PROGN
                         (SETQ HP 0)
                         (PROG (D)
                           (SETQ D DRF)
                          LAB
                           (COND ((NULL D) (RETURN NIL)))
                           ((LAMBDA (D)
                              (PROGN
                               (COND ((CDAR D) (SETQ FF (CONS 'DF (CAR D))))
                                     (T (SETQ FF (CAAR D))))
                               (SETQ NCO
                                       (COEFFN (LIST '*SQ (GET P 'SQVAL) T) FF
                                               (CDR D)))
                               (SETQ NCO
                                       (COND
                                        ((AND (PAIRP NCO)
                                              (EQUAL (CAR NCO) '*SQ))
                                         (NO_OF_TM_SF (CAR (CADR NCO))))
                                        (T 1)))
                               (COND ((GREATERP (CDR D) HP) (SETQ HP (CDR D))))
                               (SETQ FFL (CONS (LIST FF (CDR D) NCO) FFL))
                               NIL))
                            (CAR D))
                           (SETQ D (CDR D))
                           (GO LAB)))))
                      (COND (DRF (SETQ FPL (CONS (CONS P (CONS HP FFL)) FPL))))
                      NIL))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))
                (SETQ BE NIL)
                (PROG (S)
                  (SETQ S EL)
                 LAB
                  (COND ((NULL S) (RETURN NIL)))
                  ((LAMBDA (S)
                     (PROGN
                      (SETQ NCO (CADR S))
                      (SETQ NTE (CADDR S))
                      (SETQ IC (MINUS (GET (CAR S) 'TERMS)))
                      (PROG (FP)
                        (SETQ FP FPL)
                       LAB
                        (COND ((NULL FP) (RETURN NIL)))
                        ((LAMBDA (FP)
                           (COND
                            ((NEQ (CAR S) (CAR FP))
                             (PROGN
                              (SETQ RM (GET (CAR FP) 'TERMS))
                              (SETQ HP (CADR FP))
                              (SETQ IC (DIFFERENCE IC RM))
                              (PROG (FF)
                                (SETQ FF (CDDR FP))
                               LAB
                                (COND ((NULL FF) (RETURN NIL)))
                                ((LAMBDA (FF)
                                   (PROGN
                                    (SETQ IC
                                            (PLUS IC
                                                  (TIMES (CADDR FF)
                                                         (CWRNO NTE (CADR FF))
                                                         (CWRNO NCO
                                                          (DIFFERENCE HP
                                                                      (CADR
                                                                       FF))))))
                                    (SETQ RM (DIFFERENCE RM (CADDR FF)))
                                    NIL))
                                 (CAR FF))
                                (SETQ FF (CDR FF))
                                (GO LAB))
                              (SETQ IC (PLUS IC (TIMES RM (CWRNO NCO HP))))))))
                         (CAR FP))
                        (SETQ FP (CDR FP))
                        (GO LAB))
                      (COND
                       ((OR (NULL BE) (BESU IC (CADDDR S) MC MDU))
                        (PROGN
                         (SETQ BE (CAR S))
                         (SETQ MC IC)
                         (SETQ MDU (CADDDR S)))))
                      NIL))
                   (CAR S))
                  (SETQ S (CDR S))
                  (GO LAB))
                (COND
                 ((AND TR_SEARCH (GREATERP (LENGTH EL) 1))
                  (PROGN
                   (TERPRI)
                   (PROGN
                    (PRIN2 "Best substitution for ")
                    (PRIN2 F)
                    (PRIN2 " : ")
                    (PRIN2 (LIST IC F BE MDU))
                    NIL)
                   NIL)))
                (COND
                 ((OR (NULL COST_LIMIT) (LESSP IC COST_LIMIT))
                  (SETQ SUBLI (CONS (LIST IC MDU F BE) SUBLI))))
                NIL))
             (CAR EL))
            (SETQ EL (CDR EL))
            (GO LAB))))))
      (COND
       (SUBLI
        (PROGN
         (SETQ S (CAR SUBLI))
         (SETQ SUBLI (CDR SUBLI))
         (PROG (EL)
           (SETQ EL SUBLI)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (COND ((BESU (CAR EL) (CADR EL) (CAR S) (CADR S)) (SETQ S EL))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         (COND
          (TR_SEARCH
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "Optimal substitution:") NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "  replace ")
             (PRIN2 (CADDR S))
             (PRIN2 " with the help of ")
             (PRIN2 (CADDDR S))
             (PRIN2 ",")
             NIL)
            (TERPRI)
            (COND
             ((LESSP (CAR S) 0)
              (PROGN
               (PRIN2 "  saving ")
               (PRIN2 (MINUS (CAR S)))
               (PRIN2 " terms, ")
               NIL))
             (T
              (PROGN
               (PRIN2 "  with a cost of ")
               (PRIN2 (CAR S))
               (PRIN2 " additional terms, ")
               NIL)))
            (TERPRI)
            (PROGN
             (PRIN2
              (COND ((EQUAL (CADR S) 1) "  linear substitution")
                    ((EQUAL (CADR S) 2)
                     "  nonlinearity inceasing substitution")
                    (T "  with case distinction")))
             NIL)
            NIL)))
         (SETQ EL
                 (GET (CADDDR S)
                      (COND ((EQUAL (CADR S) 1) 'FCTEVAL_LIN)
                            ((EQUAL (CADR S) 2) 'FCTEVAL_NCA)
                            (T 'FCTEVAL_NLI))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ (CADDR S) (CDAR EL))) (RETURN NIL)))
           (SETQ EL (CDR EL))
           (GO WHILELABEL))
         (RETURN (LIST (CADR S) (CADDDR S) (CAR EL)))))))) 
(PUT 'BOTTOM_UP_SUBST 'NUMBER-OF-ARGS 1) 
(PUT 'BOTTOM_UP_SUBST 'DEFINED-ON-LINE '1737) 
(PUT 'BOTTOM_UP_SUBST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'BOTTOM_UP_SUBST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOTTOM_UP_SUBST (ARGLIST)
    (COND ((EQUAL CURRENTLY_TO_BE_SUBSTITUTED_IN '*SUBST_DONE*) NIL)
          ((OR (NULL (CAR ARGLIST)) (NULL (CDAR ARGLIST)))
           (PROGN (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN '*SUBST_DONE*) NIL))
          (T
           (PROG (PCP FOUND PDES FNS P A H)
             (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN NIL)
             (COND
              ((NULL CURRENTLY_TO_BE_SUBSTITUTED_IN)
               (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN (CADAR ARGLIST))))
             (SETQ FNS (GET CURRENTLY_TO_BE_SUBSTITUTED_IN 'FCTS))
             (SETQ PCP (CAR ARGLIST))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (NEQ CURRENTLY_TO_BE_SUBSTITUTED_IN '*SUBST_DONE*)
                       (NULL FOUND)))
                 (RETURN NIL)))
               (COND
                ((EQUAL (CAR PCP) CURRENTLY_TO_BE_SUBSTITUTED_IN)
                 (COND
                  ((NULL (CDR PCP))
                   (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN '*SUBST_DONE*))
                  (T
                   (PROGN
                    (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN (CADR PCP))
                    (SETQ FNS (GET CURRENTLY_TO_BE_SUBSTITUTED_IN 'FCTS))
                    (SETQ PCP (CAR ARGLIST))))))
                ((PROGN
                  (SETQ P (CAR PCP))
                  (COND ((GET P 'STARDE) NIL)
                        (T
                         (PROGN
                          (SETQ A (GET P 'FCTEVAL_LIN))
                          (COND
                           ((OR (NULL A) (FREEOF FNS (CDAR A)))
                            (PROGN
                             (SETQ A (GET P 'FCTEVAL_NCA))
                             (COND
                              ((AND A (FREEOF FNS (CDAR A))) (SETQ A NIL))))))
                          A))))
                 (PROGN
                  (SETQ PDES (CAR ARGLIST))
                  (SETQ PDES
                          (EQINSERT
                           (DO_ONE_SUBST (CAR A) (CDR A)
                            CURRENTLY_TO_BE_SUBSTITUTED_IN FTEM_
                            (GET CURRENTLY_TO_BE_SUBSTITUTED_IN 'VARS)
                            (GET P 'LEVEL) P PDES)
                           (DELETE CURRENTLY_TO_BE_SUBSTITUTED_IN PDES)))
                  (PROG (H)
                    (SETQ H PDES)
                   LAB
                    (COND ((NULL H) (RETURN NIL)))
                    ((LAMBDA (H)
                       (DROP_RL_WITH CURRENTLY_TO_BE_SUBSTITUTED_IN H))
                     (CAR H))
                    (SETQ H (CDR H))
                    (GO LAB))
                  (PUT CURRENTLY_TO_BE_SUBSTITUTED_IN 'RL_WITH NIL)
                  (PROG (H)
                    (SETQ H PDES)
                   LAB
                    (COND ((NULL H) (RETURN NIL)))
                    ((LAMBDA (H)
                       (DROP_DEC_WITH CURRENTLY_TO_BE_SUBSTITUTED_IN H
                        'DEC_WITH_RL))
                     (CAR H))
                    (SETQ H (CDR H))
                    (GO LAB))
                  (PUT CURRENTLY_TO_BE_SUBSTITUTED_IN 'DEC_WITH_RL NIL)
                  (FLAG (LIST CURRENTLY_TO_BE_SUBSTITUTED_IN) 'TO_INT)
                  (SETQ FOUND T)))
                (T (SETQ PCP (CDR PCP))))
               (GO WHILELABEL))
             (RETURN
              (COND ((EQUAL CURRENTLY_TO_BE_SUBSTITUTED_IN '*SUBST_DONE*) NIL)
                    (T (LIST PDES (CADR ARGLIST))))))))) 
(PUT 'CHECK_SUBST_DF 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_SUBST_DF 'DEFINED-ON-LINE '1805) 
(PUT 'CHECK_SUBST_DF 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'CHECK_SUBST_DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_SUBST_DF (PDES FORG)
    (PROG (L L1 L2 N CP NOT_TO_SUBSTDF)
      (COND
       (PDES
        (PROGN
         (PROG (S)
           (SETQ S PDES)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (SETQ L
                      (UNION
                       (PROG (A FORALL-RESULT FORALL-ENDPTR)
                         (SETQ A (GET S 'DERIVS))
                         (COND ((NULL A) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (A) (CAR A)) (CAR A))
                                               NIL)))
                        LOOPLABEL
                         (SETQ A (CDR A))
                         (COND ((NULL A) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (A) (CAR A)) (CAR A)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       L)))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S FORG)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((PAIRP S)
                (SETQ L
                        (UNION
                         (PROG (A FORALL-RESULT FORALL-ENDPTR)
                           (SETQ A (ALL_DERIV_SEARCH_SF (CAR (CADDR S)) FTEM_))
                           (COND ((NULL A) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS ((LAMBDA (A) (CAR A)) (CAR A))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ A (CDR A))
                           (COND ((NULL A) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (CAR A)) (CAR A)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         (UNION
                          (PROG (A FORALL-RESULT FORALL-ENDPTR)
                            (SETQ A
                                    (ALL_DERIV_SEARCH_SF (CDR (CADDR S))
                                     FTEM_))
                            (COND ((NULL A) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (A) (CAR A)) (CAR A))
                                             NIL)))
                           LOOPLABEL
                            (SETQ A (CDR A))
                            (COND ((NULL A) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (A) (CAR A)) (CAR A)) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          L))))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG (S)
           (SETQ S FSUB_)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (SETQ L
                      (UNION
                       (PROG (A FORALL-RESULT FORALL-ENDPTR)
                         (SETQ A
                                 (ALL_DERIV_SEARCH_SF (CAR (CADR (CDR S)))
                                  FTEM_))
                         (COND ((NULL A) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (A) (CAR A)) (CAR A))
                                               NIL)))
                        LOOPLABEL
                         (SETQ A (CDR A))
                         (COND ((NULL A) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (A) (CAR A)) (CAR A)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (UNION
                        (PROG (A FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A
                                  (ALL_DERIV_SEARCH_SF (CDR (CADR (CDR S)))
                                   FTEM_))
                          (COND ((NULL A) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS ((LAMBDA (A) (CAR A)) (CAR A))
                                                NIL)))
                         LOOPLABEL
                          (SETQ A (CDR A))
                          (COND ((NULL A) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (A) (CAR A)) (CAR A)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        L))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (SETQ L1 (DF_MIN_LIST L))
         (SETQ L NIL)
         (PROG (S)
           (SETQ S L1)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((AND (PAIRP S) (NOT (MEMBER (CAR S) NOT_TO_SUBSTDF)))
                (PROGN
                 (SETQ L (CONS (CONS 'DF S) L))
                 (SETQ NOT_TO_SUBSTDF (CONS (CAR S) NOT_TO_SUBSTDF))))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ N 0)
            (SETQ CP PDES)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND CP (LESSP N 2))) (RETURN NIL)))
              (PROGN
               (COND
                ((MEMBER (CADAR L) (GET (CAR CP) 'FCTS)) (SETQ N (ADD1 N))))
               (SETQ CP (CDR CP)))
              (GO WHILELABEL))
            (SETQ CP FORG)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND CP (LESSP N 2))) (RETURN NIL)))
              (PROGN
               (COND
                ((AND (PAIRP (CAR CP)) (EQUAL (CAAR CP) 'EQUAL)
                      (MEMBER (CADAR L) (GET (CADR (CAR CP)) 'FCTS)))
                 (SETQ N (ADD1 N))))
               (SETQ CP (CDR CP)))
              (GO WHILELABEL))
            (COND ((EQUAL N 2) (SETQ L2 (CONS (CAR L) L2))))
            (SETQ L (CDR L)))
           (GO WHILELABEL)))))
      (RETURN L2))) 
(PUT 'DF_MIN_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'DF_MIN_LIST 'DEFINED-ON-LINE '1853) 
(PUT 'DF_MIN_LIST 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DF_MIN_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DF_MIN_LIST (DFLIST)
    (COND
     (DFLIST
      (PROG (L D M LMAX)
        (PROG ()
         WHILELABEL
          (COND ((NOT DFLIST) (RETURN NIL)))
          (PROGN
           (SETQ M (CAR DFLIST))
           (SETQ DFLIST (CDR DFLIST))
           (SETQ L NIL)
           (PROG ()
            WHILELABEL
             (COND ((NOT DFLIST) (RETURN NIL)))
             (PROGN
              (COND ((SETQ D (DF_MIN (CAR DFLIST) M)) (SETQ M D))
                    (T (SETQ L (CONS (CAR DFLIST) L))))
              (SETQ DFLIST (CDR DFLIST))
              NIL)
             (GO WHILELABEL))
           (COND
            ((AND (PAIRP M) (NULL (CDR M))) (SETQ LMAX (CONS (CAR M) LMAX)))
            (T (SETQ LMAX (CONS M LMAX))))
           (SETQ DFLIST L)
           NIL)
          (GO WHILELABEL))
        (RETURN LMAX))))) 
(PUT 'DF_MIN 'NUMBER-OF-ARGS 2) 
(PUT 'DF_MIN 'DEFINED-ON-LINE '1876) 
(PUT 'DF_MIN 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DF_MIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DF_MIN (DF1 DF2)
    (PROGN
     (COND ((NOT (PAIRP DF1)) (SETQ DF1 (LIST DF1))))
     (COND ((NOT (PAIRP DF2)) (SETQ DF2 (LIST DF2))))
     (COND
      ((EQUAL (CAR DF1) (CAR DF2))
       (COND ((SETQ DF1 (DF_MIN1 (CDR DF1) (CDR DF2))) (CONS (CAR DF2) DF1))
             (T (CAR DF2))))))) 
(PUT 'DF_MIN1 'NUMBER-OF-ARGS 2) 
(PUT 'DF_MIN1 'DEFINED-ON-LINE '1886) 
(PUT 'DF_MIN1 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DF_MIN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DF_MIN1 (DF1 DF2)
    (PROG (L A)
      (PROG ()
       WHILELABEL
        (COND ((NOT DF1) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR DF1))
         (COND
          ((NOT
            (ZEROP (SETQ A (MIN (DFDEG DF1 (CAR DF1)) (DFDEG DF2 (CAR DF1))))))
           (SETQ L (CONS (CAR DF1) L))))
         (COND ((GREATERP A 1) (SETQ L (CONS A L))))
         (SETQ DF1 (CDR DF1))
         (COND ((AND DF1 (NUMBERP (CAR DF1))) (SETQ DF1 (CDR DF1)))))
        (GO WHILELABEL))
      (RETURN (REVERSE L)))) 
(PUT 'DFSUBST_FORG 'NUMBER-OF-ARGS 4) 
(PUT 'DFSUBST_FORG 'DEFINED-ON-LINE '1898) 
(PUT 'DFSUBST_FORG 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'DFSUBST_FORG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFSUBST_FORG (P G D FORG)
    (PROG (H FORALL-RESULT FORALL-ENDPTR)
      (SETQ H FORG)
      (COND ((NULL H) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (H)
                          (COND
                           ((AND (PAIRP H) (MEMBER D (GET (CADR H) 'FCTS)))
                            (PROGN
                             (PUT (CADR H) 'FCTS
                                  (FCTINSERT P
                                   (DELETE D (GET (CADR H) 'FCTS))))
                             (LIST 'EQUAL (CADR H)
                                   (SIMP
                                    (LIST '*SQ
                                          (SUBSQ (CADDR H)
                                                 (LIST
                                                  (CONS D (LIST '*SQ G T))))
                                          NIL)))))
                           (T H)))
                        (CAR H))
                       NIL)))
     LOOPLABEL
      (SETQ H (CDR H))
      (COND ((NULL H) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (H)
                  (COND
                   ((AND (PAIRP H) (MEMBER D (GET (CADR H) 'FCTS)))
                    (PROGN
                     (PUT (CADR H) 'FCTS
                          (FCTINSERT P (DELETE D (GET (CADR H) 'FCTS))))
                     (LIST 'EQUAL (CADR H)
                           (SIMP
                            (LIST '*SQ
                                  (SUBSQ (CADDR H)
                                         (LIST (CONS D (LIST '*SQ G T))))
                                  NIL)))))
                   (T H)))
                (CAR H))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'EXPAND_INT 'NUMBER-OF-ARGS 2) 
(PUT 'EXPAND_INT 'DEFINED-ON-LINE '1910) 
(PUT 'EXPAND_INT 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'EXPAND_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPAND_INT (P VARLIST)
    (COND ((NULL VARLIST) P)
          (T
           (PROG (V N)
             (SETQ V (CAR VARLIST))
             (SETQ VARLIST (CDR VARLIST))
             (COND
              ((AND (PAIRP VARLIST) (NUMBERP (CAR VARLIST)))
               (PROGN (SETQ N (CAR VARLIST)) (SETQ VARLIST (CDR VARLIST))))
              (T (SETQ N 1)))
             (PROG (I)
               (SETQ I 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (SETQ P (LIST 'INT P V))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN (EXPAND_INT P VARLIST)))))) 
(PUT 'RATIONAL_LESS 'NUMBER-OF-ARGS 2) 
(PUT 'RATIONAL_LESS 'DEFINED-ON-LINE '1923) 
(PUT 'RATIONAL_LESS 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'RATIONAL_LESS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATIONAL_LESS (A B)
    (COND
     ((AND (PAIRP A) (EQUAL (CAR A) 'QUOTIENT))
      (RATIONAL_LESS (CADR A) (REVAL1 (LIST 'TIMES (CADDR A) B) T)))
     ((AND (PAIRP B) (EQUAL (CAR B) 'QUOTIENT))
      (RATIONAL_LESS (REVAL1 (LIST 'TIMES (CADDR B) A) T) (CADR B)))
     ((AND (PAIRP A) (EQUAL (CAR A) 'MINUS))
      (COND
       ((AND (PAIRP B) (EQUAL (CAR B) 'MINUS)) (GREATERP (CADR A) (CADR B)))
       (T (NOT (RATIONAL_LESS (CADR A) (REVAL1 (LIST 'MINUS B) T))))))
     ((AND (PAIRP B) (EQUAL (CAR B) 'MINUS))
      (COND
       ((LESSP A 0) (NOT (RATIONAL_LESS (REVAL1 (LIST 'MINUS A) T) (CADR B))))
       (T NIL)))
     (T (LESSP A B)))) 
(PUT 'SUBSTITUTION_WEIGHT 'NUMBER-OF-ARGS 6) 
(PUT 'SUBSTITUTION_WEIGHT 'DEFINED-ON-LINE '1939) 
(PUT 'SUBSTITUTION_WEIGHT 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'SUBSTITUTION_WEIGHT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBSTITUTION_WEIGHT (K L M N P Q)
    (REVAL1
     (LIST 'QUOTIENT (LIST 'TIMES L N Q)
           (LIST 'TIMES P (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 K) M) 2)))
     T)) 
(PUT 'TEST_FACTORS_FOR_SUBSTITUTION 'NUMBER-OF-ARGS 2) 
(PUT 'TEST_FACTORS_FOR_SUBSTITUTION 'DEFINED-ON-LINE '1951) 
(PUT 'TEST_FACTORS_FOR_SUBSTITUTION 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'TEST_FACTORS_FOR_SUBSTITUTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TEST_FACTORS_FOR_SUBSTITUTION (P PV)
    (PROG (H H1 H4 H3)
      (SETQ H (GET P 'SPLIT_TEST))
      (COND
       ((NULL H)
        (PROGN
         (SETQ H1 PV)
         (SETQ H4 T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND H1 H4)) (RETURN NIL)))
           (PROGN
            (SETQ H3
                    (MKEQSQ (CAR H1) NIL NIL (GET P 'FCTS) (GET P 'VARS)
                     ALLFLAGS_ T (LIST 0) NIL NIL))
            (SETQ CONTRADICTION_ NIL)
            (SETQ H1 (CDR H1))
            (FCTEVAL H3)
            (COND
             ((NOT (OR (GET H3 'FCTEVAL_LIN) (GET H3 'FCTEVAL_NCA)))
              (SETQ H4 NIL)))
            (DROP_PDE H3 NIL NIL))
           (GO WHILELABEL))
         (SETQ H (COND (H4 1) (T 0)))
         (PUT P 'SPLIT_TEST H))))
      (RETURN H))) 
(PUT 'GET_FACT_PDE 'NUMBER-OF-ARGS 2) 
(PUT 'GET_FACT_PDE 'DEFINED-ON-LINE '1978) 
(PUT 'GET_FACT_PDE 'DEFINED-IN-FILE 'CRACK/CRSIMP.RED) 
(PUT 'GET_FACT_PDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_FACT_PDE (PDES AIM_AT_SUBST)
    (PROG (P PV F FCL FCC H H1 H2 H3 H4 H5 H6 H7 H8 EQL TR_GF)
      (SETQ H1 PDES)
      (COND
       ((NULL AIM_AT_SUBST)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND H1 (NULL H2))) (RETURN NIL)))
          (PROGN
           (SETQ H3 (GET (CAR H1) 'CASE2SEP))
           (COND
            (H3
             (COND ((NOT (MEMBER H3 INEQ_)) (SETQ H2 (CAR H1)))
                   (T
                    (PROGN
                     (PUT (CAR H1) 'CASE2SEP NIL)
                     (SETQ H4
                             (STARDEP3 (GET (CAR H1) 'VARS)
                              (GET (CAR H1) 'KERN) (GET (CAR H1) 'DERIVS)))
                     (COND
                      (H4
                       (PROGN
                        (PUT (CAR H1) 'STARDE (LIST (CONS 0 (CAR H4))))
                        (FLAG (LIST (CAR H1)) 'TO_SEP))))
                     (SETQ H1 (CDR H1))))))
            (T (SETQ H1 (CDR H1)))))
          (GO WHILELABEL))))
      (COND (H2 (RETURN H2)))
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ PV (GET P 'FAC))
            (COND
             ((PAIRP PV)
              (PROGN
               (SETQ H1 PV)
               (PROG ()
                WHILELABEL
                 (COND ((NOT H1) (RETURN NIL)))
                 (PROGN
                  (SETQ F (CAR H1))
                  (SETQ H1 (CDR H1))
                  (SETQ FCC FCL)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND FCC (NEQ (CAAR FCC) F))) (RETURN NIL)))
                    (SETQ FCC (CDR FCC))
                    (GO WHILELABEL))
                  (COND
                   (FCC
                    (PROGN
                     (SETQ H (CONS F (CONS (ADD1 (CADAR FCC)) (CDDAR FCC))))
                     (RPLACA FCC H)
                     NIL))
                   (T
                    (PROGN
                     (COND
                      (FHOM_
                       (PROGN
                        (SETQ H2 (FIND_HOM_DEG_SF (CAR F)))
                        (SETQ H2 (PLUS (CAR H2) (CADR H2)))))
                      (T (SETQ H2 1)))
                     (COND
                      ((NO_NUMBER_ATOM_SQ F)
                       (PROGN
                        (SETQ H5 (CAAAR (CAR F)))
                        (SETQ H3 0)
                        (SETQ H4 PDES)
                        (PROG ()
                         WHILELABEL
                          (COND ((NOT H4) (RETURN NIL)))
                          (PROGN
                           (COND
                            ((NOT (FREEOF (GET (CAR H4) 'FCTS) H5))
                             (SETQ H3 (ADD1 H3))))
                           (SETQ H4 (CDR H4)))
                          (GO WHILELABEL))))
                      (T (SETQ H3 1)))
                     (SETQ H4 (NO_OF_TM_SF (CAR F)))
                     (SETQ H5 1)
                     (SETQ H6 INEQ_OR)
                     (PROG ()
                      WHILELABEL
                       (COND ((NOT H6) (RETURN NIL)))
                       (PROGN
                        (SETQ H7 (LENGTH H6))
                        (COND
                         ((LESSP H7 9)
                          (COND
                           ((MEMBER (LIST F) (CAR H6))
                            (SETQ H5
                                    (LIST 'TIMES H5
                                          (LIST 'EXPT 2
                                                (LIST 'DIFFERENCE 9 H7))))))))
                        (SETQ H6 (CDR H6)))
                       (GO WHILELABEL))
                     (SETQ H5 (REVAL1 H5 T))
                     (COND ((AND FLIN_ (NOT (FREEOFLIST F FLIN_))) (SETQ H6 1))
                           (T (SETQ H6 0)))
                     (SETQ FCL (CONS (LIST F 1 H2 H3 H4 H5 H6) FCL))))))
                 (GO WHILELABEL))
               (COND ((NULL AIM_AT_SUBST) (SETQ H 1))
                     (T (SETQ H (TEST_FACTORS_FOR_SUBSTITUTION P PV))))
               (COND ((NOT (ZEROP H)) (SETQ EQL (CONS P EQL)))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND ((NULL EQL) (RETURN NIL)))
      (SETQ FCL
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H FCL)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (CONS (CAR H)
                                          (CONS
                                           (SUBSTITUTION_WEIGHT (CADR H)
                                            (CADDR H) (CADDDR H)
                                            (CAR (CDDDDR H)) (CADR (CDDDDR H))
                                            (CADDR (CDDDDR H)))
                                           (CDR H))))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (CONS (CAR H)
                                  (CONS
                                   (SUBSTITUTION_WEIGHT (CADR H) (CADDR H)
                                    (CADDDR H) (CAR (CDDDDR H))
                                    (CADR (CDDDDR H)) (CADDR (CDDDDR H)))
                                   (CDR H))))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ H2 NIL)
      (PROG (P)
        (SETQ P EQL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ PV (GET P 'FAC))
            (SETQ H8 (LENGTH PV))
            (SETQ H5 NIL)
            (SETQ H6 1)
            (PROG ()
             WHILELABEL
              (COND ((NOT PV) (RETURN NIL)))
              (PROGN
               (SETQ H (ASSOC (CAR PV) FCL))
               (COND
                (TR_GF
                 (PROGN (PROGN (PRIN2 "h assoc= ") (PRIN2 H) NIL) (TERPRI))))
               (SETQ H5 (CONS (CONS (REVAL1 (CADR H) T) (CAR H)) H5))
               (SETQ H6 (LIST 'TIMES H6 (CADR H)))
               (COND
                ((AND (NULL LIN_PROBLEM) (NOT (ZEROP (NTH H 8))))
                 (SETQ H6
                         (LIST 'TIMES
                               (LIST 'EXPT 2 (LIST 'PLUS 4 (GET P 'NFCT_LIN)))
                               H6))))
               (SETQ PV (CDR PV)))
              (GO WHILELABEL))
            (SETQ H6 (REVAL1 (LIST 'TIMES (LIST 'EXPT 2 H8) H6) T))
            (COND
             ((OR (NULL H2) (RATIONAL_LESS H6 H3))
              (PROGN (SETQ H2 P) (SETQ H3 H6) (SETQ H4 H5))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ H4 (RAT_IDX_SORT H4))
      (COND
       (TR_GF
        (PROGN
         (PROGN (PRIN2 "h4(rat_idx_sort'ed)= ") (PRIN2 H4) NIL)
         (TERPRI))))
      (PUT H2 'FAC
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A H4)
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
      (RETURN H2))) 
(ENDMODULE) 