(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'UTILITIES)) 
(PUT 'DROP_DEC_WITH 'NUMBER-OF-ARGS 3) 
(PUT 'DROP_DEC_WITH 'DEFINED-ON-LINE '39) 
(PUT 'DROP_DEC_WITH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_DEC_WITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROP_DEC_WITH (DE1 DE2 RL)
    (PROG (A B C)
      (SETQ A (COND (RL (GET DE2 'DEC_WITH_RL)) (T (GET DE2 'DEC_WITH))))
      (PROG (B)
        (SETQ B A)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ B (DELETE DE1 B))
            (COND ((GREATERP (LENGTH B) 1) (SETQ C (CONS B C))))
            NIL))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (COND (RL (PUT DE2 'DEC_WITH_RL C)) (T (PUT DE2 'DEC_WITH C))))) 
(PUT 'ADD_DEC_WITH 'NUMBER-OF-ARGS 4) 
(PUT 'ADD_DEC_WITH 'DEFINED-ON-LINE '53) 
(PUT 'ADD_DEC_WITH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_DEC_WITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_DEC_WITH (ORDERING DE1 DE2 RL)
    (PROG (A B)
      (SETQ A (COND (RL (GET DE2 'DEC_WITH_RL)) (T (GET DE2 'DEC_WITH))))
      (SETQ B (ASSOC ORDERING A))
      (SETQ A (DELETE B A))
      (COND (B (SETQ B (CONS ORDERING (CONS DE1 (CDR B)))))
            (T (SETQ B (LIST ORDERING DE1))))
      (COND (RL (PUT DE2 'DEC_WITH_RL (CONS B A)))
            (T (PUT DE2 'DEC_WITH (CONS B A)))))) 
(PUT 'ADD_BOTH_DEC_WITH 'NUMBER-OF-ARGS 4) 
(PUT 'ADD_BOTH_DEC_WITH 'DEFINED-ON-LINE '66) 
(PUT 'ADD_BOTH_DEC_WITH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_BOTH_DEC_WITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_BOTH_DEC_WITH (ORDERING DE1 DE2 RL)
    (PROG ()
      (ADD_DEC_WITH ORDERING DE1 DE2 RL)
      (ADD_DEC_WITH ORDERING DE2 DE1 RL))) 
(PUT 'DROP_RL_WITH 'NUMBER-OF-ARGS 2) 
(PUT 'DROP_RL_WITH 'DEFINED-ON-LINE '74) 
(PUT 'DROP_RL_WITH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_RL_WITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DROP_RL_WITH (DE1 DE2) (PUT DE2 'RL_WITH (DELETE DE1 (GET DE2 'RL_WITH)))) 
(PUT 'ADD_RL_WITH 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_RL_WITH 'DEFINED-ON-LINE '78) 
(PUT 'ADD_RL_WITH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_RL_WITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_RL_WITH (DE1 DE2)
    (PROGN
     (PUT DE2 'RL_WITH (CONS DE1 (GET DE2 'RL_WITH)))
     (PUT DE1 'RL_WITH (CONS DE2 (GET DE1 'RL_WITH))))) 
(PUT 'PREVENT_SIMP 'NUMBER-OF-ARGS 3) 
(PUT 'PREVENT_SIMP 'DEFINED-ON-LINE '83) 
(PUT 'PREVENT_SIMP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PREVENT_SIMP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PREVENT_SIMP (V DE1 DE2)
    (PROG (A B)
      (SETQ A (LIST 0))
      (PROG (B)
        (SETQ B A)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (COND
            ((MEMBER V (FCTARGS B))
             (PROGN (ADD_DEC_WITH B DE2 DE1 NIL) (ADD_DEC_WITH B DE2 DE1 T)))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (SETQ A (LIST 0))
      (PROG (B)
        (SETQ B A)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (COND
            ((MEMBER V (FCTARGS B))
             (PROGN (ADD_DEC_WITH B DE1 DE2 NIL) (ADD_DEC_WITH B DE1 DE2 T)))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB)))) 
(PUT 'TERMREAD 'NUMBER-OF-ARGS 0) 
(PUT 'TERMREAD 'DEFINED-ON-LINE '98) 
(PUT 'TERMREAD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TERMREAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TERMREAD NIL
    (PROG (VAL *ECHO)
     AGAIN
      (COND
       ((NOT (NULL OLD_HISTORY))
        (PROGN
         (SETQ VAL (CAR OLD_HISTORY))
         (COND
          ((EQUAL VAL 'IG)
           (PROGN (SETQ OLD_HISTORY (CDDR OLD_HISTORY)) (GO AGAIN))))
         (COND
          (PRINT_
           (PROGN (PROGN (PRIN2 "old input: ") (PRIN2 VAL) NIL) (TERPRI))))
         (COND (OLD_HISTORY (SETQ OLD_HISTORY (CDR OLD_HISTORY))))))
       (T
        (PROGN
         (RDS NIL)
         (WRS NIL)
         (SETQ VAL (READ))
         (COND (IFL* (RDS (CADR IFL*))))
         (COND (OFL* (WRS (CDR OFL*))))
         NIL)))
      (SETQ HISTORY_ (CONS VAL HISTORY_))
      (RETURN VAL))) 
(PUT 'TERMXREAD 'NUMBER-OF-ARGS 0) 
(PUT 'TERMXREAD 'DEFINED-ON-LINE '126) 
(PUT 'TERMXREAD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TERMXREAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TERMXREAD NIL
    (PROG (VAL *ECHO)
     AGAIN
      (COND
       ((NOT (NULL OLD_HISTORY))
        (PROGN
         (SETQ VAL (CAR OLD_HISTORY))
         (COND
          ((EQUAL VAL 'IG)
           (PROGN (SETQ OLD_HISTORY (CDDR OLD_HISTORY)) (GO AGAIN))))
         (COND
          (PRINT_
           (PROGN (PROGN (PRIN2 "old input: ") (PRIN2 VAL) NIL) (TERPRI))))
         (SETQ OLD_HISTORY (CDR OLD_HISTORY))))
       (T
        (PROGN
         (RDS NIL)
         (WRS NIL)
         (SETQ VAL (XREAD NIL))
         (COND (IFL* (RDS (CADR IFL*))))
         (COND (OFL* (WRS (CDR OFL*))))
         NIL)))
      (SETQ HISTORY_ (CONS VAL HISTORY_))
      (RETURN VAL))) 
(PUT 'TERMLISTREAD 'NUMBER-OF-ARGS 0) 
(PUT 'TERMLISTREAD 'DEFINED-ON-LINE '148) 
(PUT 'TERMLISTREAD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TERMLISTREAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TERMLISTREAD NIL
    (PROG (L)
      (SETQ L (TERMXREAD))
      (COND
       ((AND (NOT (NULL L))
             (OR (ATOM L) (AND (PAIRP L) (NEQ (CAR L) '*COMMA*))))
        (SETQ L (LIST '*COMMA* L))))
      (COND
       ((AND L (OR (NOT (PAIRP L)) (NEQ (CAR L) '*COMMA*)))
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Error: not a legal list of elements.") NIL)
         (TERPRI)
         (SETQ L NIL)))
       ((PAIRP L) (SETQ L (CDR L))))
      (RETURN L))) 
(FLUID '(PROMPTSTRING*)) 
(PUT 'CHANGE_PROMPT 'NUMBER-OF-ARGS 0) 
(PUT 'CHANGE_PROMPT 'DEFINED-ON-LINE '164) 
(PUT 'CHANGE_PROMPT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHANGE_PROMPT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHANGE_PROMPT NIL
    (PROG (*USERMODE)
      (COND ((NULL PROMPTSTRING*) (SETQ PROMPTSTRING* "")))
      (SETPCHAR PROMPTSTRING*)
      (SETQ PROMPTEXP* PROMPTSTRING*))) 
(PUT 'CHANGE_PROMPT_TO 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE_PROMPT_TO 'DEFINED-ON-LINE '171) 
(PUT 'CHANGE_PROMPT_TO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHANGE_PROMPT_TO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE_PROMPT_TO (U)
    (PROG (OLDPROMPT *REDEFMSG *USERMODE)
      (SETQ OLDPROMPT PROMPTSTRING*)
      (SETQ PROMPTSTRING* U)
      (COPYD 'RESTORE_UPDATE_PROMPT 'UPDATE_PROMPT)
      (COPYD 'UPDATE_PROMPT 'CHANGE_PROMPT)
      (UPDATE_PROMPT)
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN OLDPROMPT))) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'NUMBER-OF-ARGS 0) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'DEFINED-ON-LINE '182) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RESTORE_INTERACTIVE_PROMPT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESTORE_INTERACTIVE_PROMPT NIL
    (PROG (*REDEFMSG *USERMODE) (COPYD 'UPDATE_PROMPT 'RESTORE_UPDATE_PROMPT))) 
(PUT 'RESTORE_INPUT_FILE 'NUMBER-OF-ARGS 0) 
(PUT 'RESTORE_INPUT_FILE 'DEFINED-ON-LINE '187) 
(PUT 'RESTORE_INPUT_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RESTORE_INPUT_FILE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESTORE_INPUT_FILE NIL
    (COND
     ((OR (EQUAL EQUATIONS_FILE "") (EQUAL EQN_INPUT 'DONE)
          (AND (NULL EQN_INPUT) (ZEROP EQN_NO)))
      NIL)
     (T
      (PROG (H OLDINPU INTBAK)
        (SETQ INTBAK *INT)
        (SETQ *INT NIL)
        (SETQ EQN_INPUT (OPEN EQUATIONS_FILE 'INPUT))
        (SETQ OLDINPU (RDS EQN_INPUT))
        (PROG (H)
          (SETQ H 1)
         LAB
          (COND ((MINUSP (DIFFERENCE EQN_NO H)) (RETURN NIL)))
          (XREAD T)
          (SETQ H (PLUS2 H 1))
          (GO LAB))
        (RDS OLDINPU)
        (SETQ *INT INTBAK))))) 
(PUT 'READ_EQUATION 'NUMBER-OF-ARGS 1) 
(PUT 'READ_EQUATION 'DEFINED-ON-LINE '244) 
(PUT 'READ_EQUATION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'READ_EQUATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE READ_EQUATION (ARGLIST)
    (PROG (H OLDINPU EX PDES FORG SUBLI START_NO INTBAK)
      (COND
       ((OR (EQUAL EQN_INPUT 'DONE)
            (AND (EQUAL EQN_INPUT NIL)
                 (OR (EQUAL EQUATIONS_FILE "") (NULL EQUATIONS_FILE))))
        (RETURN NIL)))
      (COND ((NULL EQN_INPUT) (SETQ EQN_INPUT (OPEN EQUATIONS_FILE 'INPUT))))
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (SETQ OLDINPU (RDS EQN_INPUT))
      (PROG (H)
        (SETQ H FORG)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((AND (PAIRP H) (EQUAL (CAR H) 'EQUAL))
             (SETQ SUBLI
                     (CONS (CONS (CADR H) (LIST '*SQ (CADDR H) T)) SUBLI)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ START_NO EQN_NO)
      (SETQ INTBAK *INT)
      (SETQ *INT NIL)
     ONCEMORE
      (SETQ EX (XREAD T))
      (COND
       ((NULL EX)
        (RETURN
         (PROGN
          (SETQ EX (XREAD T))
          (CLOSE EQN_INPUT)
          (SETQ EQN_INPUT 'DONE)
          (RDS OLDINPU)
          (SETQ *INT INTBAK)
          NIL))))
      (SETQ EQN_NO (ADD1 EQN_NO))
      (SETQ *UNCACHED T)
      (SETQ EX (CONS (CAR (SUBSQ (SIMP EX) SUBLI)) 1))
      (COND
       (CONTRADICTION_
        (RETURN
         (PROGN
          (CLOSE EQN_INPUT)
          (SETQ EQN_INPUT 'DONE)
          (RDS OLDINPU)
          (SETQ *INT INTBAK)
          NIL))))
      (COND
       ((SQZEROP EX)
        (PROGN
         (COND (PRINT_ (PROGN (PRIN2 EQN_NO) (PRIN2 " ") NIL)))
         (GO ONCEMORE)))
       (T
        (PROGN
         (SETQ EX (MKEQSQ EX NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL PDES))
         (SETQ H (EQINSERT2 EX PDES))
         (COND
          ((NULL H)
           (PROGN
            (COND
             ((NULL (CAR RECYCLE_EQNS))
              (SETQ RECYCLE_EQNS
                      (CONS (LIST (CADR RECYCLE_EQNS)) (CDDR RECYCLE_EQNS)))))
            (COND (PRINT_ (PROGN (PRIN2 " (") (PRIN2 EQN_NO) (PRIN2 ")") NIL)))
            (GO ONCEMORE)))
          (T
           (PROGN
            (SETQ PDES H)
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "Reading ")
                (PRIN2 EQN_NO)
                (PRIN2 ".equation. ")
                NIL))))))))))
      (RDS OLDINPU)
      (SETQ *INT INTBAK)
      (RETURN (LIST PDES FORG)))) 
(PUT 'MKEQSQLIST 'NUMBER-OF-ARGS 9) 
(PUT 'MKEQSQLIST 'DEFINED-ON-LINE '304) 
(PUT 'MKEQSQLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MKEQSQLIST 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE MKEQSQLIST
    (SQVALLIST FACLIST PVALLIST FTEM VL FLAGLIST SIMP_FLAG ORDERL PDES)
    (PROG (L0 L1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (OR SQVALLIST FACLIST PVALLIST) (NULL CONTRADICTION_)))
          (RETURN NIL)))
        (PROGN
         (SETQ L0
                 (MKEQSQ (COND (SQVALLIST (CAR SQVALLIST)) (T NIL))
                  (COND (FACLIST (CAR FACLIST)) (T NIL))
                  (COND (PVALLIST (CAR PVALLIST)) (T NIL)) FTEM VL FLAGLIST
                  SIMP_FLAG ORDERL NIL (APPEND L1 PDES)))
         (COND (L0 (SETQ L1 (EQINSERT L0 L1))))
         (COND (SQVALLIST (SETQ SQVALLIST (CDR SQVALLIST))))
         (COND (FACLIST (SETQ FACLIST (CDR FACLIST))))
         (COND (PVALLIST (SETQ PVALLIST (CDR PVALLIST)))))
        (GO WHILELABEL))
      (RETURN L1))) 
(PUT 'MKEQSQ 'NUMBER-OF-ARGS 10) 
(PUT 'MKEQSQ 'DEFINED-ON-LINE '331) 
(PUT 'MKEQSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MKEQSQ 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE MKEQSQ (SQVAL FAC PVAL FTEM VL FLAGLIST SIMP_FLAG ORDERL HIST PDES)
    (COND
     ((OR (AND SQVAL (NOT (SQZEROP SQVAL))) FAC (NOT (ZEROP PVAL)))
      (PROG (S)
        (SETQ S (NEW_PDE))
        (COND ((AND RECORD_HIST HIST) (PUT S 'HISTRY_ (REVAL1 HIST T))))
        (PROG (A)
          (SETQ A FLAGLIST)
         LAB
          (COND ((NULL A) (RETURN NIL)))
          ((LAMBDA (A) (FLAG (LIST S) A)) (CAR A))
          (SETQ A (CDR A))
          (GO LAB))
        (COND
         ((NOT (UPDATESQ S SQVAL FAC PVAL FTEM VL SIMP_FLAG ORDERL PDES))
          (PROGN (DROP_PDE S NIL NIL) (SETQ S NIL))))
        (COND ((AND RECORD_HIST (NULL HIST) S) (PUT S 'HISTRY_ S)))
        (RETURN S))))) 
(PUT 'NO_OF_DERIVS 'NUMBER-OF-ARGS 1) 
(PUT 'NO_OF_DERIVS 'DEFINED-ON-LINE '359) 
(PUT 'NO_OF_DERIVS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_DERIVS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_OF_DERIVS (EQU)
    (COND (ALG_POLY 0)
          (T
           (PROG (H DL)
             (SETQ H 0)
             (SETQ DL (GET EQU 'DERIVS))
             (PROG ()
              WHILELABEL
               (COND ((NOT DL) (RETURN NIL)))
               (PROGN
                (COND ((AND (PAIRP (CAAR DL)) (CDAAR DL)) (SETQ H (ADD1 H))))
                (SETQ DL (CDR DL)))
               (GO WHILELABEL))
             (RETURN H))))) 
(PUT 'UPDATESQ 'NUMBER-OF-ARGS 9) 
(PUT 'UPDATESQ 'DEFINED-ON-LINE '371) 
(PUT 'UPDATESQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UPDATESQ 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE UPDATESQ (EQU SQVAL FAC PVAL FTEM VL SIMP_FLAG ORDERL PDES)
    (PROG (L H H2 H3 H4 NVARS RATIONAL NONRATIONAL ALLVARFCTS DROPED_FACTORS
           CARL RATI)
      (COND
       ((AND (PAIRP SQVAL) (EQUAL (CAR SQVAL) '*SQ))
        (COND ((EQUAL (CADDR SQVAL) T) (SETQ SQVAL (CADR SQVAL)))
              (T (SETQ SQVAL (SIMP* SQVAL)))))
       (SQVAL (SETQ SQVAL (SUBS2 SQVAL))))
      (PUT EQU 'TERMS NIL)
      (PUT EQU 'SQVAL NIL)
      (PUT EQU 'FAC NIL)
      (PUT EQU 'PVAL NIL)
      (COND
       ((NULL SQVAL)
        (COND ((NULL FAC) (PROGN (SETQ SQVAL (SIMP* PVAL)) (PUT EQU 'FAC NIL)))
              (T
               (PROGN
                (COND
                 ((NULL (CDR FAC))
                  (PROGN (PUT EQU 'FAC 2) (SETQ SQVAL (SUBS2 (CAR FAC)))))
                 (T
                  (PROGN
                   (SETQ L NIL)
                   (PROG (V)
                     (SETQ V FAC)
                    LAB
                     (COND ((NULL V) (RETURN NIL)))
                     ((LAMBDA (V)
                        (PROGN
                         (SETQ H
                                 (CDR
                                  (ERR_CATCH_FAC2
                                   (LIST '*SQ (CONS (CAR V) 1) T))))
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT H) (RETURN NIL)))
                           (PROGN
                            (COND
                             ((NULL
                               ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                                (CAR (SIMP (CADAR H)))))
                              (PROGN
                               (COND
                                ((GREATERP (CADDAR H) 1)
                                 (SETQ DROPED_FACTORS T)))
                               (SETQ H2
                                       (SIMPLIFYSQ (CADR (CADAR H)) FTEM NIL
                                        NIL T))
                               (PROG (H3)
                                 (SETQ H3 H2)
                                LAB
                                 (COND ((NULL H3) (RETURN NIL)))
                                 ((LAMBDA (H3)
                                    (COND
                                     ((MEMBER H3 L) (SETQ DROPED_FACTORS T))
                                     (T (SETQ L (CONS (CAR H3) L)))))
                                  (CAR H3))
                                 (SETQ H3 (CDR H3))
                                 (GO LAB))
                               NIL)))
                            (SETQ H (CDR H)))
                           (GO WHILELABEL))))
                      (CAR V))
                     (SETQ V (CDR V))
                     (GO LAB))
                   (COND
                    ((NULL L)
                     (PROGN
                      (SETQ SQVAL NIL)
                      (PUT EQU 'FAC NIL)
                      (SETQ FAC NIL)))
                    ((NULL (CDR L))
                     (PROGN
                      (SETQ SQVAL (CONS (CAR L) 1))
                      (PUT EQU 'FAC 2)
                      (SETQ FAC 2)))
                    (T
                     (PROGN
                      (PUT EQU 'FAC
                           (PROG (H FORALL-RESULT FORALL-ENDPTR)
                             (SETQ H L)
                             (COND ((NULL H) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (H) (CONS H 1)) (CAR H))
                                              NIL)))
                            LOOPLABEL
                             (SETQ H (CDR H))
                             (COND ((NULL H) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (H) (CONS H 1)) (CAR H))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                      (SETQ SQVAL (CONS (CAR L) 1))
                      (SETQ L (CDR L))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT L) (RETURN NIL)))
                        (PROGN
                         (SETQ SQVAL (MULTSQ SQVAL (CONS (CAR L) 1)))
                         (SETQ L (CDR L)))
                        (GO WHILELABEL))
                      (SETQ SQVAL (SUBS2 SQVAL))
                      (SETQ FAC (GET EQU 'FAC)))))))))))))
      (COND
       ((AND SQVAL (NOT (SQZEROP SQVAL)))
        (PROGN
         (COND
          ((AND (NULL SIMP_FLAG) (OR (NULL FAC) (NULL (CDR FAC))))
           (PROGN
            (COND
             ((MEMBER SQVAL INEQ_)
              (RAISE_CONTRADICTION (LIST '*SQ SQVAL T) NIL)))
            (COND
             ((NULL FAC)
              (PROGN
               (PUT EQU 'TERMS (NO_OF_TM_SF (CAR SQVAL)))
               (COND
                ((OR
                  (AND (NULL *COMPLEX)
                       (GREATERP (GET EQU 'TERMS) MAX_TERM_TO_FAC_REAL))
                  (AND *COMPLEX
                       (GREATERP (GET EQU 'TERMS) MAX_TERM_TO_FAC_COMPLEX)))
                 (SETQ L (SFFAC (CAR SQVAL))))
                (T
                 (PROGN
                  (SETQ H
                          (CDR
                           (ERR_CATCH_FAC2
                            (LIST '*SQ (CONS (CAR SQVAL) 1) T))))
                  (SETQ L NIL)
                  (COND
                   ((OR (CDR H) (GREATERP (CADDAR H) 1))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT H) (RETURN NIL)))
                      (PROGN
                       (COND
                        ((NULL
                          ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                           (CAR (SIMP (CADAR H)))))
                         (PROGN
                          (COND
                           ((GREATERP (CADDAR H) 1) (SETQ DROPED_FACTORS T)))
                          (SETQ H2
                                  (SIMPLIFYSQ (CADR (CADAR H)) FTEM NIL NIL T))
                          (PROG (H3)
                            (SETQ H3 H2)
                           LAB
                            (COND ((NULL H3) (RETURN NIL)))
                            ((LAMBDA (H3)
                               (COND ((MEMBER H3 L) (SETQ DROPED_FACTORS T))
                                     (T (SETQ L (CONS (CAR H3) L)))))
                             (CAR H3))
                            (SETQ H3 (CDR H3))
                            (GO LAB))
                          NIL)))
                       (SETQ H (CDR H)))
                      (GO WHILELABEL)))))))
               (COND
                ((AND L (CDR L))
                 (PUT EQU 'FAC
                      (PROG (H FORALL-RESULT FORALL-ENDPTR)
                        (SETQ H L)
                        (COND ((NULL H) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (H) (CONS H 1)) (CAR H))
                                              NIL)))
                       LOOPLABEL
                        (SETQ H (CDR H))
                        (COND ((NULL H) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (H) (CONS H 1)) (CAR H)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
                (T
                 (PUT EQU 'FAC
                      (COND
                       ((OR
                         (AND (NULL *COMPLEX)
                              (GREATERP (GET EQU 'TERMS) MAX_TERM_TO_FAC_REAL))
                         (AND *COMPLEX
                              (GREATERP (GET EQU 'TERMS)
                                        MAX_TERM_TO_FAC_COMPLEX)))
                        1)
                       (T 2))))))))
            (COND ((AND PVAL (NULL DROPED_FACTORS)) (PUT EQU 'PVAL PVAL)))))
          (T
           (PROGN
            (COND ((NULL FTEM) (SETQ FTEM FTEM_)))
            (COND ((NULL FAC) (SETQ L (SIMPLIFYSQ SQVAL FTEM T EQU T)))
                  (T
                   (PROGN
                    (SETQ L NIL)
                    (PROG (F)
                      (SETQ F FAC)
                     LAB
                      (COND ((NULL F) (RETURN NIL)))
                      ((LAMBDA (F)
                         (PROGN
                          (SETQ H (SIMPLIFYSQ F FTEM T EQU NIL))
                          (COND
                           ((EQUAL H (LIST (CONS 1 1))) (ADDSQINEQ PDES F T))
                           (T (SETQ L (UNION H L))))))
                       (CAR F))
                      (SETQ F (CDR F))
                      (GO LAB))
                    (COND ((NULL L) (SETQ L (LIST (CONS 1 1))))))))
            (COND
             ((EQUAL L (LIST (CONS 1 1)))
              (RAISE_CONTRADICTION (LIST '*SQ SQVAL T) NIL)))
            (SETQ SQVAL (CAR L))
            (COND ((NULL (CDR L)) (PUT EQU 'FAC 1))
                  (T
                   (PROGN
                    (PUT EQU 'FAC L)
                    (SETQ L (CDR L))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT L) (RETURN NIL)))
                      (PROGN
                       (SETQ SQVAL (MULTSQ SQVAL (CAR L)))
                       (SETQ L (CDR L)))
                      (GO WHILELABEL)))))
            (PUT EQU 'TERMS (NO_OF_TM_SF (CAR SQVAL)))
            NIL))))))
      (SETQ DEPL* (DELETE (ASSOC (REVAL1 EQU T) DEPL*) DEPL*))
      (COND
       ((NULL CONTRADICTION_)
        (COND ((OR (NULL SQVAL) (SQZEROP SQVAL)) (RETURN NIL))
              (T
               (PROGN
                (PUT EQU 'SQVAL SQVAL)
                (PUT EQU 'KERN
                     (UNION (KERNELS (CDR SQVAL)) (KERNELS (CAR SQVAL))))
                (SETQ L NIL)
                (PROG (V)
                  (SETQ V (GET EQU 'KERN))
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V)
                     (COND
                      ((AND (PAIRP V)
                            (OR (NEQ (CAR V) 'DF)
                                (AND (EQUAL (CAR V) 'DF) (PAIRP (CADR V))))
                            (MEMBER (CAR V) REDUCEFUNCTIONS_))
                       (SETQ L (CONS V L)))))
                   (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB))
                (PUT EQU 'NON_RAT_KERN L)
                (PUT EQU 'FCT_KERN_LIN NIL)
                (PUT EQU 'FCT_KERN_NLI NIL)
                (SETQ FTEM
                        (SORT_ACCORDING_TO (SMEMBERL FTEM (GET EQU 'KERN))
                         FTEM_))
                (PUT EQU 'FCTS FTEM)
                (PUT EQU 'FCT_HOM (SMEMBERL FTEM FHOM_))
                (SETQ L NIL)
                (PROG (V)
                  (SETQ V VL)
                 LAB
                  (COND ((NULL V) (RETURN NIL)))
                  ((LAMBDA (V)
                     (COND
                      ((NOT (MY_FREEOF (GET EQU 'KERN) V))
                       (SETQ L (CONS V L)))))
                   (CAR V))
                  (SETQ V (CDR V))
                  (GO LAB))
                (SETQ VL (SORT_ACCORDING_TO L VL_))
                (PUT EQU 'VARS VL)
                (COND ((NULL VL) (REMFLAG (LIST EQU) 'TO_DIFF)))
                (COND (VL (SETQ DEPL* (CONS (CONS EQU VL) DEPL*))))
                (PUT EQU 'NVARS (LENGTH VL))
                (PUT EQU 'LEVEL LEVEL_)
                (PUT EQU 'DERIVS
                     (SORT_DERIVS
                      (COND
                       ((PAIRP (CDR SQVAL))
                        (UNION (ALL_DERIV_SEARCH_SF (CDR SQVAL) FTEM)
                               (ALL_DERIV_SEARCH_SF (CAR SQVAL) FTEM)))
                       (T (ALL_DERIV_SEARCH_SF (CAR SQVAL) FTEM)))
                      FTEM VL))
                (COND (STRUC_EQN (PUT EQU 'NO_DERIVS (NO_OF_DERIVS EQU))))
                (PUT EQU 'FCTEVAL_LIN NIL)
                (PUT EQU 'FCTEVAL_NCA NIL)
                (PUT EQU 'FCTEVAL_NLI NIL)
                (PUT EQU 'FCTEVAL_N2L NIL)
                (PUT EQU 'FCT_NLI_LIN NIL)
                (PUT EQU 'FCT_NLI_NCA NIL)
                (PUT EQU 'FCT_NLI_NLI NIL)
                (PUT EQU 'FCT_NLI_NUS NIL)
                (COND
                 ((NULL (GET EQU 'TERMS))
                  (PUT EQU 'TERMS (NO_OF_TM_SF (CAR SQVAL)))))
                (PUT EQU 'LENGTH (GET EQU 'TERMS))
                (PUT EQU 'PRINTLENGTH (DELENGTHSQ SQVAL))
                (PUT EQU 'ORDERINGS ORDERL)
                (SETQ NVARS (GET EQU 'NVARS))
                (COND
                 (ALG_POLY
                  (PROGN
                   (SETQ RATIONAL FTEM)
                   (SETQ NONRATIONAL NIL)
                   (SETQ ALLVARFCTS FTEM)))
                 (T
                  (PROGN
                   (PROG (F)
                     (SETQ F (REVERSE FTEM))
                    LAB
                     (COND ((NULL F) (RETURN NIL)))
                     ((LAMBDA (F) (SETQ RATIONAL (CONS F RATIONAL))) (CAR F))
                     (SETQ F (CDR F))
                     (GO LAB))
                   (SETQ RATI (CONS 1 RATIONAL))
                   (SETQ NONRATIONAL NIL)
                   (SETQ ALLVARFCTS NIL)
                   (SETQ L (GET EQU 'KERN))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT L) (RETURN NIL)))
                     (PROGN
                      (SETQ CARL (CAR L))
                      (SETQ L (CDR L))
                      (COND
                       ((OR (ATOM CARL)
                            (AND (PAIRP CARL) (EQUAL (CAR CARL) 'DF)
                                 (ATOM (CADR CARL))))
                        T)
                       (T
                        (PROGN
                         (SETQ H RATI)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT (CDR H)) (RETURN NIL)))
                           (COND
                            ((NOT (FREEOF CARL (CADR H)))
                             (PROGN
                              (SETQ NONRATIONAL (CONS (CADR H) NONRATIONAL))
                              (SETQ H (RPLACD H (CDDR H)))))
                            (T (SETQ H (CDR H))))
                           (GO WHILELABEL))))))
                     (GO WHILELABEL))
                   (SETQ NONRATIONAL (REVERSE NONRATIONAL))
                   (COND ((EQUAL NVARS 0) (SETQ ALLVARFCTS RATIONAL))
                         (T
                          (PROG (F)
                            (SETQ F (REVERSE RATIONAL))
                           LAB
                            (COND ((NULL F) (RETURN NIL)))
                            ((LAMBDA (F)
                               (COND
                                ((EQUAL (FCTLENGTH F) NVARS)
                                 (SETQ ALLVARFCTS (CONS F ALLVARFCTS)))))
                             (CAR F))
                            (SETQ F (CDR F))
                            (GO LAB))))
                   NIL)))
                (PROG (L)
                  (SETQ L NONRATIONAL)
                 LAB
                  (COND ((NULL L) (RETURN NIL)))
                  ((LAMBDA (L) (SETQ RATIONAL (DELETE L RATIONAL))) (CAR L))
                  (SETQ L (CDR L))
                  (GO LAB))
                (PUT EQU 'RATIONAL RATIONAL)
                (PUT EQU 'NONRATIONAL NONRATIONAL)
                (PUT EQU 'ALLVARFCTS ALLVARFCTS)
                (PUT EQU 'PARTITIONED NIL)
                (PUT EQU 'CASE2SEP NIL)
                (SETQ L
                        (STARDEP3 (GET EQU 'VARS) (GET EQU 'KERN)
                         (GET EQU 'DERIVS)))
                (COND
                 (L
                  (PROGN
                   (SETQ H (CDR L))
                   (SETQ L (SIMP (CAR L)))
                   (COND
                    ((AND (MEMBER L INEQ_) (MEMBER (DIFFSQ L H) INEQ_))
                     (PUT EQU 'STARDE (LIST (CONS 0 (PREPSQ L)))))
                    (T
                     (PROGN
                      (PUT EQU 'CASE2SEP (DIFFSQ L H))
                      (PUT EQU 'STARDE (SEP_VAR FTEM VL)))))))
                 (T (PUT EQU 'STARDE (SEP_VAR FTEM VL))))
                (FLAG (LIST EQU) 'TO_EVAL)
                (COND
                 ((SETQ L (GET EQU 'STARDE))
                  (PROGN
                   (REMFLAG (LIST EQU) 'TO_INT)
                   (REMFLAG (LIST EQU) 'TO_FULLINT)
                   (COND
                    ((AND SIMP_FLAG (ZEROP (CAAR L)))
                     (PROGN
                      (FLAG (LIST EQU) 'TO_SEP)
                      (FLAG (LIST EQU) 'TO_CASESEP))))
                   NIL))
                 (T
                  (PROGN
                   (REMFLAG (LIST EQU) 'TO_GENSEP)
                   (REMFLAG (LIST EQU) 'TO_CASEGENSEP))))
                (COND
                 ((AND (SETQ L (GET EQU 'STARDE)) (ZEROP (CAAR L)))
                  (REMFLAG (LIST EQU) 'TO_EVAL))
                 (T
                  (PROGN
                   (REMFLAG (LIST EQU) 'TO_SEP)
                   (REMFLAG (LIST EQU) 'TO_CASESEP))))
                (COND
                 ((GET EQU 'NONRATIONAL)
                  (PROGN
                   (COND
                    ((NULL
                      (SETDIFF (GET EQU 'ALLVARFCTS) (GET EQU 'NONRATIONAL)))
                     (REMFLAG (LIST EQU) 'TO_EVAL))))))
                (COND
                 ((NOT (GET EQU 'RATIONAL)) (REMFLAG (LIST EQU) 'TO_EVAL)))
                (COND
                 (FHOM_
                  (PROGN
                   (SETQ L (FIND_HOM_DEG_SF (CAR SQVAL)))
                   (PUT EQU 'HOM_DEG L)
                   NIL)))
                (PUT EQU 'SPLIT_TEST NIL)
                (PUT EQU 'LINEAR_
                     (COND (NONRATIONAL NIL) (LIN_PROBLEM T)
                           ((NOT (FREEOF (CDR SQVAL) FTEM)) NIL)
                           ((LIN_CHECK_SQ (CONS (FIRST_TERM_SF (CAR SQVAL)) 1)
                             FTEM)
                            (COND ((LIN_CHECK_SQ SQVAL FTEM) T) (T NIL)))
                           (T NIL)))
                (PUT EQU 'NOT_TO_EVAL NIL)
                (COND
                 (PDES
                  (PROGN
                   (NEW_INEQ_FROM_EQU_SQ EQU PDES)
                   (COND
                    ((NULL (CDR PDES)) (NEW_INEQ_FROM_EQU_SQ (CAR PDES) PDES)))
                   (COND
                    ((NULL CONTRADICTION_)
                     (SIMP_ALL_INEQ_WITH_EQU_SQ EQU PDES)))
                   (SETQ H (GET EQU 'ALLVARFCTS))
                   (COND
                    ((AND H (NULL (CDR H)) (GET EQU 'VARS)
                          (CDR (GET EQU 'FCTS)))
                     (PROGN
                      (SETQ H4 NIL)
                      (SETQ H3 T)
                      (SETQ L (GET EQU 'DERIVS))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT (AND H3 L)) (RETURN NIL)))
                        (PROGN
                         (COND
                          ((EQUAL (CAAAR L) (CAR H))
                           (COND ((NULL H4) (SETQ H4 (CAAR L)))
                                 ((NEQ H4 (CAAR L)) (SETQ H3 NIL)))))
                         (SETQ L (CDR L)))
                        (GO WHILELABEL))
                      (COND
                       (H3
                        (PROGN
                         (SETQ H4 (COND ((CDR H4) (CONS 'DF H4)) (T (CAR H4))))
                         (SETQ L
                                 (COND
                                  ((FREEOF (GET EQU 'NONRATIONAL) (CAR H))
                                   (GET EQU 'VARS))
                                  (T NIL)))
                         (COND
                          (L
                           (PROGN
                            (SETQ H2 NIL)
                            (PROG (H3)
                              (SETQ H3 L)
                             LAB
                              (COND ((NULL H3) (RETURN NIL)))
                              ((LAMBDA (H3)
                                 (COND
                                  ((NOT (MEMBER H3 (GET EQU 'KERN)))
                                   (SETQ H2 (CONS H3 H2)))))
                               (CAR H3))
                              (SETQ H3 (CDR H3))
                              (GO LAB))
                            (SETQ L H2)
                            NIL)))
                         (COND
                          (L
                           (PROGN
                            (SETQ H2 (SETDIFF (GET EQU 'FCTS) H))
                            (PROG (H3)
                              (SETQ H3 H2)
                             LAB
                              (COND ((NULL H3) (RETURN NIL)))
                              ((LAMBDA (H3) (SETQ L (SETDIFF L (FCTARGS H3))))
                               (CAR H3))
                              (SETQ H3 (CDR H3))
                              (GO LAB))
                            NIL)))
                         (COND
                          (L
                           (PROGN
                            (PROG ()
                             WHILELABEL
                              (COND
                               ((NOT
                                 (AND L
                                      (NULL
                                       (MEMBER (SIMP (LIST 'DF H4 (CAR L)))
                                               INEQ_))))
                                (RETURN NIL)))
                              (SETQ L (CDR L))
                              (GO WHILELABEL))
                            (COND
                             (L
                              (PROGN
                               (COND
                                (PRINT_
                                 (PROGN
                                  (PROGN
                                   (PRIN2
                                    "Next comes a separation of equation ")
                                   (PRIN2 EQU)
                                   (PRIN2 " wrt. ")
                                   (PRIN2 H4)
                                   NIL)
                                  (TERPRI))))
                               (SETQ H
                                       (CDR
                                        (AEVAL
                                         (LIST 'COEFF
                                               (LIST '*SQ (GET EQU 'SQVAL) T)
                                               H4))))
                               (SETQ TO_DO_LIST
                                       (CONS
                                        (LIST 'ADD_EQNS
                                              (PROG (G FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ G H)
                                                (COND ((NULL G) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (G)
                                                                    (COND
                                                                     ((AND
                                                                       (PAIRP
                                                                        G)
                                                                       (EQUAL
                                                                        (CAR G)
                                                                        '*SQ))
                                                                      (CADR G))
                                                                     (T
                                                                      (SIMP
                                                                       G))))
                                                                  (CAR G))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ G (CDR G))
                                                (COND
                                                 ((NULL G)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (G)
                                                            (COND
                                                             ((AND (PAIRP G)
                                                                   (EQUAL
                                                                    (CAR G)
                                                                    '*SQ))
                                                              (CADR G))
                                                             (T (SIMP G))))
                                                          (CAR G))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL)))
                                        TO_DO_LIST))))
                             (T
                              (SETQ TO_DO_LIST
                                      (CONS
                                       (LIST 'ADD_DIFFERENTIATED_PDES
                                             (LIST EQU))
                                       TO_DO_LIST)))))))))))))
                   (COND
                    ((AND REAL_VALUED (NON_NEGATIVE (CAR SQVAL))
                          (NON_NEGATIVE (CDR SQVAL)))
                     (PROGN
                      (COND
                       (PRINT_
                        (PROGN
                         (PROGN
                          (PRIN2
                           "Because of real_valued=t all variables, unknowns and parameters")
                          NIL)
                         (TERPRI)
                         (PROGN
                          (PRIN2
                           "are supposed to be real and therefore each term of equation ")
                          (PRIN2 EQU)
                          NIL)
                         (TERPRI)
                         (PROGN (PRIN2 "must vanish on its own.") NIL)
                         (TERPRI)
                         (EQPRINT (LIST 'EQUAL EQU (LIST '*SQ SQVAL T))))))
                      (SETQ H (CAR SQVAL))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT H) (RETURN NIL)))
                        (PROGN
                         (SETQ L (FIRST_TERM_SF H))
                         (SETQ H (ADDF H (NEGF L)))
                         (SETQ TO_DO_LIST
                                 (CONS
                                  (LIST 'REPLACE_EQUATION
                                        (LIST NIL NIL (CONS L 1) NIL))
                                  TO_DO_LIST))
                         NIL)
                        (GO WHILELABEL)))))
                   NIL)))
                (RETURN EQU)))))))) 
(PUT 'ADD_EQNS 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_EQNS 'DEFINED-ON-LINE '815) 
(PUT 'ADD_EQNS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_EQNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_EQNS (ARGLIST)
    (PROG (PDES EQNS Q)
      (SETQ PDES (CAR ARGLIST))
      (SETQ EQNS (CADDDR ARGLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND EQNS (NULL CONTRADICTION_))) (RETURN NIL)))
        (PROGN
         (COND ((ZEROP (CAR EQNS)) (SETQ Q NIL))
               (T
                (SETQ Q
                        (MKEQSQ (CAR EQNS) NIL NIL FTEM_ VL_ ALLFLAGS_ T
                         (LIST 0) NIL NIL))))
         (COND (Q (SETQ PDES (EQINSERT Q PDES))))
         (SETQ EQNS (CDR EQNS)))
        (GO WHILELABEL))
      (RETURN (LIST PDES (CADR ARGLIST))))) 
(PUT 'PDE_DEGREE_SQ 'NUMBER-OF-ARGS 2) 
(PUT 'PDE_DEGREE_SQ 'DEFINED-ON-LINE '890) 
(PUT 'PDE_DEGREE_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PDE_DEGREE_SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PDE_DEGREE_SQ (PV FL)
    (PROG (F SB K)
      (SETQ K (SETKORDER (LIST LIN_TEST_CONST)))
      (SETQ SB
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FL)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (CONS F
                                          (LIST '*SQ
                                                (SIMP
                                                 (LIST 'TIMES LIN_TEST_CONST
                                                       F))
                                                T)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (CONS F
                                  (LIST '*SQ
                                        (SIMP (LIST 'TIMES LIN_TEST_CONST F))
                                        T)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PV (SUBF (CAR PV) SB))
      (SETKORDER K)
      (RETURN (CDAAR (CAR PV))))) 
(PUT 'DFSUBST_UPDATE 'NUMBER-OF-ARGS 3) 
(PUT 'DFSUBST_UPDATE 'DEFINED-ON-LINE '901) 
(PUT 'DFSUBST_UPDATE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFSUBST_UPDATE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFSUBST_UPDATE (F DER EQU)
    (PROG (L H)
      (PROG (D)
        (SETQ D (GET EQU 'DERIVS))
       LAB
        (COND ((NULL D) (RETURN NIL)))
        ((LAMBDA (D)
           (COND ((NOT (MEMBER (CADR DER) (CAR D))) (SETQ L (CONS D L)))
                 (T
                  (PROGN
                   (SETQ L
                           (CONS
                            (CONS (CONS F (DF_INT (CDAR D) (CDDR DER)))
                                  (CDR D))
                            L))
                   (PUT EQU 'KERN
                        (SUBST (REVAL1 (CONS 'DF (CAAR L)) T)
                               (REVAL1 (CONS 'DF (CAR D)) T) (GET EQU 'KERN)))
                   (SETQ H (GET EQU 'PVAL))
                   (COND
                    (H
                     (PUT EQU 'PVAL
                          (SUBST (REVAL1 (CONS 'DF (CAAR L)) T)
                                 (CONS 'DF (CAR D)) H))))
                   (SETQ H (GET EQU 'FAC))
                   (COND
                    ((PAIRP H)
                     (PUT EQU 'FAC
                          (PROG (F FORALL-RESULT FORALL-ENDPTR)
                            (SETQ F H)
                            (COND ((NULL F) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (F)
                                                (SUBSQ F
                                                       (LIST
                                                        (CONS
                                                         (CAAAR
                                                          (CAR
                                                           (MKSQ
                                                            (CONS 'DF (CAR D))
                                                            1)))
                                                         (REVAL1
                                                          (CONS 'DF (CAAR L))
                                                          T)))))
                                              (CAR F))
                                             NIL)))
                           LOOPLABEL
                            (SETQ F (CDR F))
                            (COND ((NULL F) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (F)
                                        (SUBSQ F
                                               (LIST
                                                (CONS
                                                 (CAAAR
                                                  (CAR
                                                   (MKSQ (CONS 'DF (CAR D))
                                                         1)))
                                                 (REVAL1 (CONS 'DF (CAAR L))
                                                         T)))))
                                      (CAR F))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
                   (PUT EQU 'PARTITIONED NIL)
                   (PUT EQU 'SQVAL
                        (SUBSQ (GET EQU 'SQVAL)
                               (LIST
                                (CONS (CAAAR (CAR (MKSQ (CONS 'DF (CAR D)) 1)))
                                      (REVAL1 (CONS 'DF (CAAR L)) T)))))
                   NIL))))
         (CAR D))
        (SETQ D (CDR D))
        (GO LAB))
      (PUT EQU 'FCTS
           (SORT_ACCORDING_TO (SUBST F (CADR DER) (GET EQU 'FCTS)) FTEM_))
      (PUT EQU 'ALLVARFCTS
           (SORT_ACCORDING_TO (SUBST F (CADR DER) (GET EQU 'ALLVARFCTS))
            FTEM_))
      (COND ((GET EQU 'ALLVARFCTS) (FLAG (LIST EQU) 'TO_EVAL)))
      (PUT EQU 'RATIONAL (SUBST F (CADR DER) (GET EQU 'RATIONAL)))
      (PUT EQU 'NONRATIONAL (SUBST F (CADR DER) (GET EQU 'NONRATIONAL)))
      (PUT EQU 'DERIVS (SORT_DERIVS L (GET EQU 'FCTS) (GET EQU 'VARS)))
      (RETURN EQU))) 
(PUT 'INSERT_IN_EQLIST 'NUMBER-OF-ARGS 2) 
(PUT 'INSERT_IN_EQLIST 'DEFINED-ON-LINE '937) 
(PUT 'INSERT_IN_EQLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'INSERT_IN_EQLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSERT_IN_EQLIST (S L)
    (COND ((NULL L) (LIST S))
          (T
           (PROG (L1 M N FOUND1 FOUND2)
             (SETQ N (GET S 'PRINTLENGTH))
             (RETURN
              (COND
               ((LEQ N (GET (CAR L) 'PRINTLENGTH))
                (PROGN
                 (SETQ LARGEST_FULLY_SHORTENED NIL)
                 (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN (CAR L))
                 (CONS S L)))
               (T
                (PROGN
                 (SETQ L1 L)
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND (CDR L)
                           (OR (NULL (SETQ M (GET (CADR L) 'PRINTLENGTH)))
                               (GREATERP N M))))
                     (RETURN NIL)))
                   (PROGN
                    (COND
                     ((NULL M)
                      (PROGN
                       (PRIN2 "### The equation ")
                       (PRIN2 (CADR L))
                       (PRIN2 " has no length! ###")
                       NIL)))
                    (COND
                     ((EQUAL (CAR L) LARGEST_FULLY_SHORTENED) (SETQ FOUND1 T)))
                    (COND
                     ((EQUAL (CAR L) CURRENTLY_TO_BE_SUBSTITUTED_IN)
                      (SETQ FOUND2 T)))
                    (SETQ L (CDR L)))
                   (GO WHILELABEL))
                 (COND
                  ((AND LARGEST_FULLY_SHORTENED (NULL FOUND1))
                   (SETQ LARGEST_FULLY_SHORTENED (CAR L))))
                 (COND
                  ((AND CURRENTLY_TO_BE_SUBSTITUTED_IN (NULL FOUND2))
                   (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN (CAR L))))
                 (RPLACD L (CONS S (CDR L)))
                 L1)))))))) 
(PUT 'EQINSERT 'NUMBER-OF-ARGS 2) 
(PUT 'EQINSERT 'DEFINED-ON-LINE '969) 
(PUT 'EQINSERT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'EQINSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQINSERT (S L)
    (COND
     ((OR (NOT (OR S (GET S 'SQVAL))) (ZEROP (GET S 'LENGTH)) (MEMBER S L)) L)
     ((NOT L) (LIST S))
     (T
      (PROG (L1)
        (SETQ L1 (PRODDEL_SQ S L))
        (COND ((CAR L1) (PROGN (SETQ L1 (INSERT_IN_EQLIST S (CADR L1)))))
              (L1 (SETQ L1 (CADR L1))) (T (SETQ L1 L)))
        (RETURN L1))))) 
(PUT 'EQINSERT2 'NUMBER-OF-ARGS 2) 
(PUT 'EQINSERT2 'DEFINED-ON-LINE '990) 
(PUT 'EQINSERT2 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'EQINSERT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQINSERT2 (S L)
    (COND
     ((OR (NOT (OR S (GET S 'SQVAL))) (ZEROP (GET S 'LENGTH)) (MEMBER S L))
      NIL)
     ((NOT L) (LIST S))
     (T
      (PROG (L1 N FOUND1 FOUND2)
        (SETQ L1 (PRODDEL_SQ S L))
        (COND
         ((CAR L1)
          (PROGN
           (SETQ N (GET S 'LENGTH))
           (SETQ L (CADR L1))
           (SETQ L1 NIL)
           (PROG ()
            WHILELABEL
             (COND
              ((NOT (AND L (GREATERP N (GET (CAR L) 'LENGTH)))) (RETURN NIL)))
             (PROGN
              (SETQ L1 (CONS (CAR L) L1))
              (COND ((EQUAL (CAR L) LARGEST_FULLY_SHORTENED) (SETQ FOUND1 T)))
              (COND
               ((EQUAL (CAR L) CURRENTLY_TO_BE_SUBSTITUTED_IN)
                (SETQ FOUND2 T)))
              (SETQ L (CDR L)))
             (GO WHILELABEL))
           (COND
            ((AND LARGEST_FULLY_SHORTENED (NULL FOUND1))
             (SETQ LARGEST_FULLY_SHORTENED
                     (COND ((NULL L1) NIL) (T (CAR L1))))))
           (COND
            ((AND CURRENTLY_TO_BE_SUBSTITUTED_IN (NULL FOUND2))
             (SETQ LARGEST_FULLY_SHORTENED S)))
           (SETQ L1 (APPEND (REVERSE L1) (CONS S L)))
           NIL))
         ((NOT_INCLUDED L (CADR L1)) (SETQ L1 (CADR L1))) (T (SETQ L1 NIL)))
        (RETURN L1))))) 
(PUT 'NOT_INCLUDED 'NUMBER-OF-ARGS 2) 
(PUT 'NOT_INCLUDED 'DEFINED-ON-LINE '1023) 
(PUT 'NOT_INCLUDED 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NOT_INCLUDED 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NOT_INCLUDED (A B)
    (PROG (C)
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A C)) (RETURN NIL)))
        (PROGN
         (SETQ C B)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND C (NEQ (CAR A) (CAR C)))) (RETURN NIL)))
           (SETQ C (CDR C))
           (GO WHILELABEL))
         (SETQ A (CDR A))
         NIL)
        (GO WHILELABEL))
      (RETURN (COND (C NIL) (T T))))) 
(PUT 'FOLLOWS_FROMSQ 'NUMBER-OF-ARGS 2) 
(PUT 'FOLLOWS_FROMSQ 'DEFINED-ON-LINE '1057) 
(PUT 'FOLLOWS_FROMSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FOLLOWS_FROMSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FOLLOWS_FROMSQ (PFAC PDES)
    (PROG (P1 FOLLOWS)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (PAIRP (SETQ P1 (GET (CAR PDES) 'FAC))))
           (SETQ P1 (LIST (GET (CAR PDES) 'SQVAL)))))
         (COND
          ((NULL (NOT_INCLUDED P1 PFAC))
           (PROGN (SETQ FOLLOWS T) (SETQ PDES NIL)))
          (T (SETQ PDES (CDR PDES)))))
        (GO WHILELABEL))
      (RETURN FOLLOWS))) 
(PUT 'PRODDEL_SQ 'NUMBER-OF-ARGS 2) 
(PUT 'PRODDEL_SQ 'DEFINED-ON-LINE '1112) 
(PUT 'PRODDEL_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRODDEL_SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRODDEL_SQ (S L)
    (PROG (L1 L2 L3 N LNEW PDES S_HIST)
      (COND
       ((NOT (PAIRP (SETQ LNEW (GET S 'FAC))))
        (SETQ LNEW (LIST (GET S 'SQVAL)))))
      (SETQ N (LENGTH LNEW))
      (SETQ PDES L)
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (PAIRP (SETQ L1 (GET (CAR L) 'FAC))))
           (SETQ L1 (LIST (GET (CAR L) 'SQVAL)))))
         (COND
          ((LESSP N (LENGTH L1))
           (COND ((NOT_INCLUDED LNEW L1) (SETQ L2 (CONS (CAR L) L2)))
                 (T
                  (PROGN
                   (SETQ L3 (CONS (CAR L) L3))
                   (DROP_PDE (CAR L) NIL
                    (REVAL1
                     (LIST '*SQ
                           (MULTSQ (MULTSQ (SIMP S) (GET (CAR L) 'SQVAL))
                                   (INVSQ (GET S 'SQVAL)))
                           T)
                     T))))))
          (T
           (PROGN
            (COND
             ((NULL (NOT_INCLUDED L1 LNEW))
              (PROGN
               (COND
                (PRINT_
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2 S)
                   (PRIN2 " is a consequence of ")
                   (PRIN2 (CAR L))
                   (PRIN2 ".")
                   NIL))))
               (COND
                ((NULL S_HIST)
                 (SETQ S_HIST
                         (MULTSQ (MULTSQ (SIMP (CAR L)) (GET S 'SQVAL))
                                 (INVSQ (GET (CAR L) 'SQVAL))))))
               NIL)))
            (COND
             ((OR (NULL L3) (NEQ (CAR L3) (CAR L)))
              (SETQ L2 (CONS (CAR L) L2))))
            NIL)))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (COND
       ((AND PRINT_ L3)
        (PROGN
         (LISTPRINT L3)
         (COND ((CDR L3) (PROGN (PRIN2 " are consequences of ") (PRIN2 S) NIL))
               (T (PROGN (PRIN2 " is a consequence of ") (PRIN2 S) NIL)))
         (TERPRI)
         NIL)))
      (COND
       (S_HIST
        (PROGN (DROP_PDE S NIL (REVAL1 (LIST '*SQ S_HIST T) T)) (SETQ S NIL))))
      (RETURN (LIST S (REVERSE L2))))) 
(PUT 'CLEAN_HIST 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAN_HIST 'DEFINED-ON-LINE '1153) 
(PUT 'CLEAN_HIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CLEAN_HIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAN_HIST NIL
    (PROG (H NEWH)
      (SETQ H (REVERSE HISTORY_))
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (COND
         ((OR (EQUAL (CAR H) 'S) (EQUAL (CAR H) 'PH) (EQUAL (CAR H) 'PO))
          (SETQ H (CDR H)))
         ((AND (EQUAL (CAR H) 'T) (CDR H) (EQUAL (CADR H) 'T))
          (SETQ H (CDDR H)))
         ((AND (EQUAL (CAR H) 'T) (CDR H) (EQUAL (CADR H) 'E) (CDDR H)
               (EQUAL (CADDR H) 'T))
          (SETQ H (CDDDR H)))
         (T (PROGN (SETQ NEWH (CONS (CAR H) NEWH)) (SETQ H (CDR H)))))
        (GO WHILELABEL))
      (RETURN NEWH))) 
(PUT 'UNSUCC 'NUMBER-OF-ARGS 1) 
(PUT 'UNSUCC 'DEFINED-ON-LINE '1175) 
(PUT 'UNSUCC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNSUCC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNSUCC (S)
    (PROGN
     (SETQ S (REVERSE (EXPLODE S)))
     (COND
      ((AND (EQUAL (CAR S) '|"|) (EQUAL (CADR S) '|.|) (CDDR S)
            (EQUAL (CADDR S) 'C) (CDDDR S) (EQUAL (CADDDR S) 'C))
       T)
      (T NIL)))) 
(PUT 'PRI_HIST 'NUMBER-OF-ARGS 1) 
(PUT 'PRI_HIST 'DEFINED-ON-LINE '1186) 
(PUT 'PRI_HIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRI_HIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRI_HIST (L)
    (PROG (W J)
      (SETQ L (REVERSE L))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ W NIL)
         (COND (J (SETQ J (NOT J)))
               ((OR (EQUAL (CAR L) 'CM) (EQUAL (CAR L) 'GS) (EQUAL (CAR L) 'R)
                    (EQUAL (CAR L) '44)
                    (AND (EQUAL (CAR L) 'IG)
                         (NULL (AND (CDR L) (UNSUCC (CADR L)))))
                    (PROGN
                     (COND ((NULL (CDR L)) NIL) ((NULL (CDDR L)) NIL)
                           (T (UNSUCC (CADDR L))))))
                (PROGN (SETQ J T) (TERPRI)))
               (T (SETQ J NIL)))
         NIL
         (PRIN1 (CAR L))
         (COND ((UNSUCC (CAR L)) (PROGN (SETQ J T) (TERPRI)))
               (T (PROGN (SETQ J NIL) (PRIN2 " "))))
         (SETQ L (CDR L)))
        (GO WHILELABEL)))) 
(PUT 'MYPRIN2L 'NUMBER-OF-ARGS 2) 
(PUT 'MYPRIN2L 'DEFINED-ON-LINE '1206) 
(PUT 'MYPRIN2L 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MYPRIN2L 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MYPRIN2L (L TRENN)
    (COND
     (L
      (PROGN
       (COND
        ((PAIRP L)
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (PROGN (PRIN2 (CAR L)) NIL)
            (SETQ L (CDR L))
            (COND (L (PROGN (PRIN2 TRENN) NIL))))
           (GO WHILELABEL)))
        (T (PROGN (PRIN2 L) NIL))))))) 
(PUT 'PRINT_STARS 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_STARS 'DEFINED-ON-LINE '1216) 
(PUT 'PRINT_STARS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_STARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_STARS (S)
    (PROG (B STAR PV CS)
      (SETQ PV (PAIRP (GET S 'FAC)))
      (SETQ CS (GET S 'CASE2SEP))
      (SETQ STAR (GET S 'STARDE))
      (COND
       ((OR STAR PV CS)
        (PROGN
         (PROGN (PRIN2 "(") NIL)
         (COND (PV (PROGN (PRIN2 "#") NIL)))
         (COND (CS (PROGN (PRIN2 "!") NIL)))
         (COND
          (STAR
           (PROG (B)
             (SETQ B 1)
            LAB
             (COND ((MINUSP (DIFFERENCE (PLUS 1 (CAAR STAR)) B)) (RETURN NIL)))
             (PROGN (PRIN2 "*") NIL)
             (SETQ B (PLUS2 B 1))
             (GO LAB))))
         (PROGN (PRIN2 ")") NIL)
         NIL))))) 
(PUT 'TYPEEQ 'NUMBER-OF-ARGS 1) 
(PUT 'TYPEEQ 'DEFINED-ON-LINE '1230) 
(PUT 'TYPEEQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TYPEEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPEEQ (S)
    (COND
     ((OR (NULL PRINT_) (GREATERP (GET S 'PRINTLENGTH) PRINT_))
      (PROG (A B)
        (PRINT_STARS S)
        (PROGN
         (PRIN2 " ")
         (PRIN2 (SETQ A (GET S 'TERMS)))
         (PRIN2 " terms,")
         NIL)
        (COND
         ((NEQ A (SETQ B (GET S 'LENGTH)))
          (PROGN (PRIN2 " ") (PRIN2 B) (PRIN2 " factors,") NIL)))
        (PROGN (PRIN2 " with ") NIL)
        (COND ((GET S 'VARS) (PROGN (PRIN2 "derivatives") NIL))
              (T (PROGN (PRIN2 "powers: ") NIL)))
        (COND
         ((GET S 'STARDE)
          (PROGN (PROGN (PRIN2 ": ") NIL) (TERPRI) (PRINT_DERIVS S NIL) NIL))
         (T
          (PROGN
           (COND
            ((SETQ A (GET S 'VARS))
             (PROGN
              (PROGN
               (PRIN2 " of functions of all ")
               (PRIN2 (LENGTH A))
               (PRIN2 " variables: ")
               NIL)
              (LISTPRINT (GET S 'VARS)))))
           (TERPRI)
           (PRINT_DERIVS S T)
           NIL)))))
     (T (MATHPRINT (LIST 'EQUAL 0 (LIST '*SQ (GET S 'SQVAL) T)))))) 
(PUT 'PRINT_DERIVS 'NUMBER-OF-ARGS 2) 
(PUT 'PRINT_DERIVS 'DEFINED-ON-LINE '1252) 
(PUT 'PRINT_DERIVS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_DERIVS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINT_DERIVS (P ALLVARF)
    (PROG (A D DL AVF)
      (SETQ DL (GET P 'DERIVS))
      (COND
       (ALLVARF
        (PROGN
         (SETQ AVF (GET P 'ALLVARFCTS))
         (PROG (D)
           (SETQ D DL)
          LAB
           (COND ((NULL D) (RETURN NIL)))
           ((LAMBDA (D) (COND ((NOT (FREEOFLIST D AVF)) (SETQ A (CONS D A)))))
            (CAR D))
           (SETQ D (CDR D))
           (GO LAB))
         (SETQ DL (REVERSE A)))))
      (SETQ DL
              (PROG (D FORALL-RESULT FORALL-ENDPTR)
                (SETQ D DL)
                (COND ((NULL D) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (D)
                                    (PROGN
                                     (SETQ A
                                             (COND ((NULL (CDAR D)) (CAAR D))
                                                   (T (CONS 'DF (CAR D)))))
                                     (COND ((EQUAL (CDR D) 1) A)
                                           (T (LIST 'EXPT A (CDR D))))))
                                  (CAR D))
                                 NIL)))
               LOOPLABEL
                (SETQ D (CDR D))
                (COND ((NULL D) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (D)
                            (PROGN
                             (SETQ A
                                     (COND ((NULL (CDAR D)) (CAAR D))
                                           (T (CONS 'DF (CAR D)))))
                             (COND ((EQUAL (CDR D) 1) A)
                                   (T (LIST 'EXPT A (CDR D))))))
                          (CAR D))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (MATHPRINT (CONS BLANK DL))
      (SETQ DL (GET P 'NON_RAT_KERN))
      (COND (DL (MATHPRINT (CONS 'LIST DL)))))) 
(PUT 'TYPE_PRE_EX 'NUMBER-OF-ARGS 1) 
(PUT 'TYPE_PRE_EX 'DEFINED-ON-LINE '1273) 
(PUT 'TYPE_PRE_EX 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TYPE_PRE_EX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPE_PRE_EX (P)
    (COND
     (PRINT_
      (MATHPRINT
       (COND
        ((AND (PAIRP P)
              (OR (AND (EQUAL (CAR P) 'PLUS) (GREATERP (LENGTH P) PRINT_))
                  (AND (EQUAL (CAR P) 'QUOTIENT)
                       (OR (GREATERP (LENGTH (CADR P)) PRINT_)
                           (GREATERP (LENGTH (CADDR P)) PRINT_)))))
         (BLDMSG_INTERNAL "%w%d%w" (LIST " " (NO_OF_TM_SF (CAR P)) " terms ")))
        (T P)))))) 
(PUT 'TYPE_SQ_EX 'NUMBER-OF-ARGS 1) 
(PUT 'TYPE_SQ_EX 'DEFINED-ON-LINE '1283) 
(PUT 'TYPE_SQ_EX 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TYPE_SQ_EX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPE_SQ_EX (P)
    (COND
     (PRINT_
      (MATHPRINT
       (COND
        ((GREATERP (DELENGTHSQ P) PRINT_)
         (BLDMSG_INTERNAL "%w%d%w" (LIST " " (NO_OF_TM_SF (CAR P)) " terms ")))
        (T (LIST '*SQ P T))))))) 
(PUT 'TYPEEQLIST 'NUMBER-OF-ARGS 1) 
(PUT 'TYPEEQLIST 'DEFINED-ON-LINE '1290) 
(PUT 'TYPEEQLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TYPEEQLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TYPEEQLIST (L)
    (PROGN
     (TERPRI)
     (PROG (S)
       (SETQ S L)
      LAB
       (COND ((NULL S) (RETURN NIL)))
       ((LAMBDA (S)
          (PROGN
           (TERPRI)
           (PROGN (PRIN2 S) (PRIN2 " : ") NIL)
           (COND ((NOT PRINT_ALL) (TYPEEQ S))
                 ((OR (NULL PRINT_) (GREATERP (GET S 'PRINTLENGTH) PRINT_))
                  (PROGN
                   (PROGN (PRIN2 (GET S 'TERMS)) (PRIN2 " terms") NIL)
                   (TERPRI)))
                 (T (MATHPRINT (LIST 'EQUAL 0 (LIST '*SQ (GET S 'SQVAL) T)))))
           (COND
            (PRINT_ALL
             (PROGN
              (PROGN (PRIN2 "   derivs        : ") NIL)
              (TERPRI)
              (PRINT_DERIVS S NIL)
              (TERPRI)
              (PROGN (PRIN2 "   derivs(raw)   : ") (PRIN2 (GET S 'DERIVS)) NIL)
              (TERPRI)
              (PROGN (PRIN2 "   fac           : ") NIL)
              (COND
               ((PAIRP (GET S 'FAC))
                (PROGN
                 (TERPRI)
                 (MATHPRINT
                  (CONS 'LIST
                        (PROG (F FORALL-RESULT FORALL-ENDPTR)
                          (SETQ F (GET S 'FAC))
                          (COND ((NULL F) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (F)
                                              (COND
                                               ((OR (NULL PRINT_)
                                                    (GREATERP (DELENGTHSQ F)
                                                              PRINT_))
                                                (BLDMSG_INTERNAL "%w%d%w"
                                                                 (LIST " "
                                                                       (NO_OF_TM_SF
                                                                        (CAR
                                                                         F))
                                                                       " terms ")))
                                               (T (LIST '*SQ F T))))
                                            (CAR F))
                                           NIL)))
                         LOOPLABEL
                          (SETQ F (CDR F))
                          (COND ((NULL F) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F)
                                      (COND
                                       ((OR (NULL PRINT_)
                                            (GREATERP (DELENGTHSQ F) PRINT_))
                                        (BLDMSG_INTERNAL "%w%d%w"
                                                         (LIST " "
                                                               (NO_OF_TM_SF
                                                                (CAR F))
                                                               " terms ")))
                                       (T (LIST '*SQ F T))))
                                    (CAR F))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
                 NIL))
               (T (PROGN (PRIN2 (GET S 'FAC)) NIL)))
              (TERPRI)
              (PROGN (PRIN2 "   pval          : ") (PRIN2 (GET S 'PVAL)) NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   partitioned   : ")
               (PRIN2 (COND ((GET S 'PARTITIONED) "not nil") (T "nil")))
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "   kern          : ") (PRIN2 (GET S 'KERN)) NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   non_rat_kern  : ")
               (PRIN2 (GET S 'NON_RAT_KERN))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_kern_lin  : ")
               (PRIN2 (GET S 'FCT_KERN_LIN))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_kern_nli  : ")
               (PRIN2 (GET S 'FCT_KERN_NLI))
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "   fcts          : ") (PRIN2 (GET S 'FCTS)) NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_hom       : ")
               (PRIN2 (GET S 'FCT_HOM))
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "   vars          : ") (PRIN2 (GET S 'VARS)) NIL)
              (TERPRI)
              (PROGN (PRIN2 "   nvars         : ") (PRIN2 (GET S 'NVARS)) NIL)
              (TERPRI)
              (PROGN (PRIN2 "   level         : ") (PRIN2 (GET S 'LEVEL)) NIL)
              (TERPRI)
              (PROGN (PRIN2 "   terms         : ") (PRIN2 (GET S 'TERMS)) NIL)
              (TERPRI)
              (PROGN (PRIN2 "   length        : ") (PRIN2 (GET S 'LENGTH)) NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   printlength   : ")
               (PRIN2 (GET S 'PRINTLENGTH))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   rational      : ")
               (PRIN2 (GET S 'RATIONAL))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   nonrational   : ")
               (PRIN2 (GET S 'NONRATIONAL))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   allvarfcts    : ")
               (PRIN2 (GET S 'ALLVARFCTS))
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "   starde        : ") (PRIN2 (GET S 'STARDE)) NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fcteval_lin   : ")
               (PRIN2 (GET S 'FCTEVAL_LIN))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fcteval_nca   : ")
               (PRIN2 (GET S 'FCTEVAL_NCA))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fcteval_nli   : ")
               (PRIN2 (GET S 'FCTEVAL_NLI))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fcteval_n2l   : ")
               (PRIN2 (GET S 'FCTEVAL_N2L))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_nli_lin   : ")
               (PRIN2 (GET S 'FCT_NLI_LIN))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_nli_nca   : ")
               (PRIN2 (GET S 'FCT_NLI_NCA))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_nli_nli   : ")
               (PRIN2 (GET S 'FCT_NLI_NLI))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   fct_nli_nus   : ")
               (PRIN2 (GET S 'FCT_NLI_NUS))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   rl_with       : ")
               (PRIN2 (GET S 'RL_WITH))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   dec_with      : ")
               (PRIN2 (GET S 'DEC_WITH))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   dec_with_rl   : ")
               (PRIN2 (GET S 'DEC_WITH_RL))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   res_with      : ")
               (PRIN2 (GET S 'RES_WITH))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_int        : ")
               (PRIN2 (FLAGP S 'TO_INT))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_fullint    : ")
               (PRIN2 (FLAGP S 'TO_FULLINT))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_sep        : ")
               (PRIN2 (FLAGP S 'TO_SEP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_casesep    : ")
               (PRIN2 (FLAGP S 'TO_CASESEP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_gensep     : ")
               (PRIN2 (FLAGP S 'TO_GENSEP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_casegensep : ")
               (PRIN2 (FLAGP S 'TO_CASEGENSEP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_decoup     : ")
               (PRIN2 (FLAGP S 'TO_DECOUP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_drop       : ")
               (PRIN2 (FLAGP S 'TO_DROP))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_eval       : ")
               (PRIN2 (FLAGP S 'TO_EVAL))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_diff       : ")
               (PRIN2 (FLAGP S 'TO_DIFF))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_under      : ")
               (PRIN2 (FLAGP S 'TO_UNDER))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   to_separant   : ")
               (PRIN2 (FLAGP S 'TO_SEPARANT))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   not_to_eval   : ")
               (PRIN2 (GET S 'NOT_TO_EVAL))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   used_         : ")
               (PRIN2 (FLAGP S 'USED_))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   orderings     : ")
               (PRIN2 (GET S 'ORDERINGS))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   split_test    : ")
               (PRIN2 (GET S 'SPLIT_TEST))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   linear_       : ")
               (PRIN2 (GET S 'LINEAR_))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   histry_       : ")
               (PRIN2 (GET S 'HISTRY_))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   hom_deg       : ")
               (PRIN2 (GET S 'HOM_DEG))
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "   case2sep      : ")
               (PRIN2 (GET S 'CASE2SEP))
               NIL)
              (TERPRI))))))
        (CAR S))
       (SETQ S (CDR S))
       (GO LAB)))) 
(PUT 'RATIONALP 'NUMBER-OF-ARGS 2) 
(PUT 'RATIONALP 'DEFINED-ON-LINE '1384) 
(PUT 'RATIONALP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RATIONALP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATIONALP (P F)
    (OR (NOT (PAIRP P))
        (AND (EQUAL (CAR P) 'QUOTIENT) (POLYNOP (CADR P) F)
             (POLYNOP (CADDR P) F))
        (AND (EQUAL (CAR P) 'EQUAL) (RATIONALP (CADR P) F)
             (RATIONALP (CADDR P) F))
        (POLYNOP P F))) 
(PUT 'RATEXP 'NUMBER-OF-ARGS 2) 
(PUT 'RATEXP 'DEFINED-ON-LINE '1398) 
(PUT 'RATEXP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RATEXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATEXP (P FTEM)
    (COND ((NULL FTEM) T) ((RATIONALP P (CAR FTEM)) (RATEXP P (CDR FTEM)))
          (T NIL))) 
(PUT 'POLYNOP 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOP 'DEFINED-ON-LINE '1406) 
(PUT 'POLYNOP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'POLYNOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOP (P F)
    (COND ((ATOM P) T) ((AND (PAIRP P) (EQUAL (CAR P) 'DF) (ATOM (CADR P))) T)
          ((MY_FREEOF P F) T)
          (T
           (PROG (A)
             (COND
              ((MEMBER (CAR P) (LIST 'EXPT 'PLUS 'MINUS 'TIMES 'QUOTIENT 'DF))
               (PROGN
                (COND
                 ((OR (EQUAL (CAR P) 'PLUS) (EQUAL (CAR P) 'TIMES))
                  (PROGN
                   (SETQ P (CDR P))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT P) (RETURN NIL)))
                     (COND ((SETQ A (POLYNOP (CAR P) F)) (SETQ P (CDR P)))
                           (T (SETQ P NIL)))
                     (GO WHILELABEL))))
                 ((EQUAL (CAR P) 'MINUS) (SETQ A (POLYNOP (CADR P) F)))
                 ((EQUAL (CAR P) 'QUOTIENT)
                  (PROGN
                   (COND
                    ((FREEOF (CADDR P) F) (SETQ A (POLYNOP (CADR P) F))))))
                 ((EQUAL (CAR P) 'EXPT)
                  (PROGN
                   (COND
                    ((FIXP (CADDR P))
                     (COND
                      ((GREATERP (CADDR P) 0)
                       (SETQ A (POLYNOP (CADR P) F))))))))
                 ((EQUAL (CAR P) 'DF)
                  (COND
                   ((OR (EQUAL (CADR P) F) (FREEOF (CADR P) F))
                    (SETQ A T)))))))
              (T (SETQ A (EQUAL P F))))
             (RETURN A))))) 
(PUT 'STARDEP3 'NUMBER-OF-ARGS 3) 
(PUT 'STARDEP3 'DEFINED-ON-LINE '1434) 
(PUT 'STARDEP3 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'STARDEP3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STARDEP3 (VL KER DRV)
    (PROG (V DFC DFCP DRVCP CAA)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (NULL DFC))) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR VL))
         (SETQ VL (CDR VL))
         (COND
          ((FREEOF KER V)
           (PROGN
            (SETQ DRVCP DRV)
            (PROG ()
             WHILELABEL
              (COND ((NOT DRVCP) (RETURN NIL)))
              (PROGN
               (SETQ CAA (CAAR DRVCP))
               (COND
                ((EQUAL CAA DFC)
                 (COND ((GREATERP (CDAR DRVCP) DFCP) (SETQ DFCP (CDAR DRVCP)))
                       (T NIL)))
                ((MEMBER V (FCTARGS (CAR CAA)))
                 (COND
                  ((NULL DFC) (PROGN (SETQ DFC CAA) (SETQ DFCP (CDAR DRVCP))))
                  (T (PROGN (SETQ DRVCP (LIST 1)) (SETQ DFC NIL))))))
               (SETQ DRVCP (CDR DRVCP))
               NIL)
              (GO WHILELABEL))))))
        (GO WHILELABEL))
      (RETURN
       (COND ((OR (NULL DFC) (EQUAL DFCP 1)) NIL)
             ((NULL (CDR DFC)) (CONS (CAR DFC) V))
             (T (CONS (CAAAR (CAR (MKSQ (CONS 'DF DFC) 1))) V)))))) 
(PUT 'STARP 'NUMBER-OF-ARGS 2) 
(PUT 'STARP 'DEFINED-ON-LINE '1460) 
(PUT 'STARP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'STARP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STARP (FT N)
    (PROG (B)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT B) FT)) (RETURN NIL)))
        (COND ((EQUAL (FCTLENGTH (CAR FT)) N) (SETQ B T))
              (T (SETQ FT (CDR FT))))
        (GO WHILELABEL))
      (RETURN (NOT B)))) 
(PUT 'SEP_VAR 'NUMBER-OF-ARGS 2) 
(PUT 'SEP_VAR 'DEFINED-ON-LINE '1497) 
(PUT 'SEP_VAR 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SEP_VAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEP_VAR (FTEM VL)
    (COND
     (VL
      (PROG (N F FV V S)
        (COND ((NULL (STARP FTEM (LENGTH VL))) (RETURN NIL)))
        (SETQ FV
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F FTEM)
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (F) (FCTARGS F)) (CAR F))
                                        NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (F) (FCTARGS F)) (CAR F)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROG (V)
          (SETQ V VL)
         LAB
          (COND ((NULL V) (RETURN NIL)))
          ((LAMBDA (V)
             (PROGN
              (SETQ N
                      (PROG (F FORALL-RESULT)
                        (SETQ F FV)
                        (SETQ FORALL-RESULT 0)
                       LAB1
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (SETQ FORALL-RESULT
                                (PLUS
                                 ((LAMBDA (F) (COND ((MEMBER V F) 1) (T 0)))
                                  (CAR F))
                                 FORALL-RESULT))
                        (SETQ F (CDR F))
                        (GO LAB1)))
              (SETQ S (CONS (CONS N V) S))))
           (CAR V))
          (SETQ V (CDR V))
          (GO LAB))
        (RETURN (IDX_SORT S)))))) 
(PUT 'PARTI_FN 'PSOPFN 'PARTI_FNCTS) 
(PUT 'PARTI_FNCTS 'NUMBER-OF-ARGS 1) 
(PUT 'PARTI_FNCTS 'DEFINED-ON-LINE '1530) 
(PUT 'PARTI_FNCTS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PARTI_FNCTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARTI_FNCTS (INP)
    (COND
     ((NEQ (LENGTH INP) 2)
      (PROGN
       (TERPRI)
       (PROGN (PRIN2 "PARTI_FNCTS DOES NOT HAVE 2 ARGUMENTS.") NIL)
       NIL))
     (T
      (PROG (FL F1 F2 F3 F4 F5 EL E1 E2)
        (SETQ FL (CDR (REVAL1 (CAR INP) T)))
        (SETQ EL (CDR (REVAL1 (CADR INP) NIL)))
        (PROG ()
         WHILELABEL
          (COND ((NOT FL) (RETURN NIL)))
          (PROGN
           (SETQ F1 NIL)
           (SETQ F2 (LIST (CAR FL)))
           (SETQ FL (CDR FL))
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND F2 FL)) (RETURN NIL)))
             (PROGN
              (SETQ F3 (CAR F2))
              (SETQ F2 (CDR F2))
              (SETQ F1 (CONS F3 F1))
              (PROG (F4)
                (SETQ F4
                        (SMEMBERL FL
                         (PROGN
                          (SETQ E1 NIL)
                          (PROG (E2)
                            (SETQ E2 EL)
                           LAB
                            (COND ((NULL E2) (RETURN NIL)))
                            ((LAMBDA (E2)
                               (COND ((SMEMBER F3 E2) (SETQ E1 (CONS E2 E1)))))
                             (CAR E2))
                            (SETQ E2 (CDR E2))
                            (GO LAB))
                          E1)))
               LAB
                (COND ((NULL F4) (RETURN NIL)))
                ((LAMBDA (F4)
                   (PROGN (SETQ F2 (CONS F4 F2)) (SETQ FL (DELETE F4 FL))))
                 (CAR F4))
                (SETQ F4 (CDR F4))
                (GO LAB)))
             (GO WHILELABEL))
           (COND (F2 (SETQ F1 (APPEND F1 F2))))
           (SETQ F5 (CONS (CONS 'LIST F1) F5)))
          (GO WHILELABEL))
        (RETURN (CONS 'LIST F5)))))) 
(PUT 'PLOT_DEPENDENCIES 'NUMBER-OF-ARGS 1) 
(PUT 'PLOT_DEPENDENCIES 'DEFINED-ON-LINE '1575) 
(PUT 'PLOT_DEPENDENCIES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLOT_DEPENDENCIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOT_DEPENDENCIES (PDES)
    (PROG (FL)
      (CHANGE_PROMPT_TO "")
      (SETQ FL FTEM_)
      (COND
       ((AND FLIN_
             (YESP
              "Shall only functions from the linear list flin_ be considered? "))
        (SETQ FL (SETDIFF FL (SETDIFF FL FLIN_)))))
      (RESTORE_INTERACTIVE_PROMPT)
      (PLOT_DEP_MATRIX PDES FL))) 
(FLUID '(*GC)) 
(PUT 'PLOT_DEP_MATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'PLOT_DEP_MATRIX 'DEFINED-ON-LINE '1588) 
(PUT 'PLOT_DEP_MATRIX 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLOT_DEP_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLOT_DEP_MATRIX (PDES ALLF)
    (PROG (F ML LF FL H LH LCO N M LL GCBAK)
      (SETQ GCBAK *GC)
      (COND (GCBAK (AEVAL (OFF (LIST 'GC)))))
      (SETQ ML 0)
      (SETQ LF (LENGTH ALLF))
      (PROG (F)
        (SETQ F (REVERSE ALLF))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ H (EXPLODE F))
            (SETQ LH (LENGTH H))
            (COND ((GREATERP LH ML) (SETQ ML LH)))
            (SETQ LCO (CONS H LCO))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ LL (LINELENGTH (PLUS LF 6)))
      (TERPRI)
      (PROGN
       (PRIN2 "Horizontally: function names (each vertical),  ")
       (PRIN2 "Vertically: equation indices")
       NIL)
      (TERPRI)
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ML N)) (RETURN NIL)))
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "     ") NIL)
         (PROG (M)
           (SETQ M 1)
          LAB
           (COND ((MINUSP (DIFFERENCE LF M)) (RETURN NIL)))
           (PROGN
            (PRIN2
             (PROGN
              (SETQ H (NTH LCO M))
              (COND ((GREATERP N (LENGTH H)) " ") (T (NTH (NTH LCO M) N)))))
            NIL)
           (SETQ M (PLUS2 M 1))
           (GO LAB)))
        (SETQ N (PLUS2 N 1))
        (GO LAB))
      (SETQ M (ADD1 (ADD1 ML)))
      (TERPRI)
      (TERPRI)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((GEQ M 0)
             (PROGN
              (SETQ H (EXPLODE P))
              (PROG (N)
                (SETQ N 3)
               LAB
                (COND ((MINUSP (DIFFERENCE (LENGTH H) N)) (RETURN NIL)))
                (PROGN (PRIN2 (NTH H N)) NIL)
                (SETQ N (PLUS2 N 1))
                (GO LAB))
              (PROG (N)
                (SETQ N (SUB1 (LENGTH H)))
               LAB
                (COND ((MINUSP (DIFFERENCE 5 N)) (RETURN NIL)))
                (PROGN (PRIN2 " ") NIL)
                (SETQ N (PLUS2 N 1))
                (GO LAB))
              (SETQ FL (GET P 'FCTS))
              (COND
               ((AND (NOT (GET P 'FCTEVAL_LIN)) (NOT (GET P 'FCTEVAL_NCA))
                     (NOT (GET P 'FCTEVAL_NLI)))
                (FCTEVAL P)))
              (PROG (F)
                (SETQ F ALLF)
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F)
                   (COND ((FREEOF FL F) (PROGN (PRIN2 " ") NIL))
                         ((OR (SOLVABLE_CASE P F 'FCTEVAL_LIN)
                              (SOLVABLE_CASE P F 'FCTEVAL_NCA))
                          (PROGN (PRIN2 "s") NIL))
                         (T (PROGN (PRIN2 "+") NIL))))
                 (CAR F))
                (SETQ F (CDR F))
                (GO LAB))
              (TERPRI)
              (SETQ M (ADD1 M))
              NIL))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (GCBAK (AEVAL (ON (LIST 'GC)))))
      (LINELENGTH LL))) 
(PUT 'PLOT_STATISTICS 'NUMBER-OF-ARGS 1) 
(PUT 'PLOT_STATISTICS 'DEFINED-ON-LINE '1644) 
(PUT 'PLOT_STATISTICS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLOT_STATISTICS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOT_STATISTICS (SIZE_HISTORY)
    (PROG (S H K N PL SF TL PROLI PLCP NEWPLCP TIME_OFFSET NEXT_TIME OLD_TIME
           MINT MAXT QUICK MAXMETH MAXFL MAXPDES MAXTERMS MAXFACTPERTERM
           MAXCELLS A SAVE OFL*BAK)
      (CHANGE_PROMPT_TO "")
      (SETQ H SIZE_HISTORY)
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROGN
         (SETQ K (CAR H))
         (SETQ H (CDR H))
         (COND
          ((EQUAL (CAR K) 'CP)
           (COND ((NULL PLCP) (SETQ PLCP (CDR K)))
                 (T
                  (PROGN
                   (SETQ NEWPLCP NIL)
                   (SETQ K (CDR K))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (OR K PLCP)) (RETURN NIL)))
                     (PROGN
                      (COND
                       ((AND K (NOT (FREEOF NEWPLCP (CAR K))))
                        (SETQ K (CDR K)))
                       ((AND PLCP (NOT (FREEOF NEWPLCP (CAR PLCP))))
                        (SETQ PLCP (CDR PLCP)))
                       ((NULL K)
                        (PROGN
                         (SETQ NEWPLCP (CONS (CAR PLCP) NEWPLCP))
                         (SETQ PLCP (CDR PLCP))))
                       ((NULL PLCP)
                        (PROGN
                         (SETQ NEWPLCP (CONS (CAR K) NEWPLCP))
                         (SETQ K (CDR K))))
                       ((EQUAL (CAR K) (CAR PLCP))
                        (PROGN
                         (SETQ NEWPLCP (CONS (CAR K) NEWPLCP))
                         (SETQ K (CDR K))
                         (SETQ PLCP (CDR PLCP))))
                       ((FREEOF K (CAR PLCP))
                        (PROGN
                         (SETQ NEWPLCP (CONS (CAR PLCP) NEWPLCP))
                         (SETQ PLCP (CDR PLCP))))
                       ((FREEOF PLCP (CAR K))
                        (PROGN
                         (SETQ NEWPLCP (CONS (CAR K) NEWPLCP))
                         (SETQ K (CDR K))))
                       (T
                        (PROGN
                         (SETQ NEWPLCP
                                 (CONS (CAR K) (CONS (CAR PLCP) NEWPLCP)))
                         (SETQ K (CDR K))
                         (SETQ PLCP (CDR PLCP))))))
                     (GO WHILELABEL))
                   (SETQ PLCP (REVERSE NEWPLCP))))))))
        (GO WHILELABEL))
      (SETQ S 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT PLCP) (RETURN NIL)))
        (PROGN
         (SETQ S (ADD1 S))
         (SETQ PROLI (CONS (CONS (CAR PLCP) S) PROLI))
         (SETQ PLCP (CDR PLCP)))
        (GO WHILELABEL))
      (SETQ MAXMETH 0)
      (SETQ MAXFL 0)
      (SETQ MAXPDES 0)
      (SETQ MAXTERMS 0)
      (SETQ MAXFACTPERTERM 0)
      (SETQ MAXCELLS 0)
      (SETQ PROLI (REVERSE PROLI))
      (SETQ TIME_OFFSET 0)
      (SETQ OLD_TIME (MINUS 1))
      (SETQ S "schrott.tmp")
      (SETQ A (OPEN S 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* S)
      (SETQ SAVE (WRS A))
      (PROG (H)
        (SETQ H (REVERSE SIZE_HISTORY))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((AND (FIXP (CAR H)) (CDDDR (CDDDR H)))
             (PROGN
              (COND ((EQUAL OLD_TIME (MINUS 1)) (SETQ OLD_TIME (CADDR H))))
              (SETQ NEXT_TIME (PLUS TIME_OFFSET (CADDR H)))
              (COND
               ((LESSP NEXT_TIME OLD_TIME)
                (PROGN
                 (SETQ TIME_OFFSET
                         (PLUS TIME_OFFSET (DIFFERENCE OLD_TIME NEXT_TIME)))
                 (SETQ NEXT_TIME OLD_TIME))))
              (PROGN
               (PRIN2 NEXT_TIME)
               (PRIN2 " ")
               (PRIN2 (COND ((SETQ N (ASSOC (CAR H) PROLI)) (CDR N)) (T 0)))
               (PRIN2 " ")
               (PRIN2 (CADDDR H))
               (PRIN2 " ")
               (PRIN2 (CADDDR (CDR H)))
               (PRIN2 " ")
               (PRIN2 (CADDDR (CDDR H)))
               (PRIN2 " ")
               (PRIN2 (CADDDR (CDDDR H)))
               (PRIN2 " ")
               (PRIN2 (CADDDR (CDDDDR H)))
               NIL)
              (SETQ OLD_TIME NEXT_TIME)
              (COND
               ((AND N (GREATERP (CDR N) MAXMETH)) (SETQ MAXMETH (CDR N))))
              (COND ((GREATERP (CADDDR H) MAXFL) (SETQ MAXFL (CADDDR H))))
              (COND
               ((GREATERP (CADDDR (CDR H)) MAXPDES)
                (SETQ MAXPDES (CADDDR (CDR H)))))
              (COND
               ((GREATERP (CADDDR (CDDR H)) MAXTERMS)
                (SETQ MAXTERMS (CADDDR (CDDR H)))))
              (COND
               ((GREATERP (TIMES 100 (CADDDR (CDDDR H)))
                          (TIMES MAXFACTPERTERM (CADDDR (CDDR H))))
                (SETQ MAXFACTPERTERM
                        (QUOTIENT (TIMES 100 (CADDDR (CDDDR H)))
                                  (CADDDR (CDDR H))))))
              (COND
               ((AND (FIXP (CADDDR (CDDDDR H)))
                     (GREATERP (CADDDR (CDDDDR H)) MAXCELLS))
                (SETQ MAXCELLS (CADDDR (CDDDDR H)))))
              (TERPRI)
              NIL))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (SETQ PL NIL)
      (COND
       ((YESP "Do you want a quick overview on the screen? ") (SETQ QUICK T)))
      (COND
       (QUICK
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "Here are the maximal values scaled to 1 in the diagram:")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "max # of unknows:      ") (PRIN2 MAXFL) NIL)
         (TERPRI)
         (PROGN (PRIN2 "max # of equations:    ") (PRIN2 MAXPDES) NIL)
         (TERPRI)
         (PROGN (PRIN2 "max # of terms:        ") (PRIN2 MAXTERMS) NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "max # of factors/term: ")
          (PRIN2 MAXFACTPERTERM)
          (PRIN2 "/100")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "max # of free cells:   ") (PRIN2 MAXCELLS) NIL)
         (TERPRI)
         (SETQ PL (BLDMSG_INTERNAL "%w" (LIST "plot '")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%d%w"
                                  (LIST PL S "' using ($1/60000):($3/" MAXFL
                                        ") title \"unknowns    :\" with lines")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                  (LIST PL ", '" S "' using ($1/60000):($4/"
                                        MAXPDES
                                        ") title \"equations   :\" with lines")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                  (LIST PL ", '" S "' using ($1/60000):($5/"
                                        MAXTERMS
                                        ") title \"all terms   :\" with lines")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                  (LIST PL ", '" S
                                        "' using ($1/60000):(100*$6/$5/"
                                        MAXFACTPERTERM
                                        ") title \"factors/term:\" with lines")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                  (LIST PL ", '" S "' using ($1/60000):($7/"
                                        MAXCELLS
                                        ") title \"free cells  :\" with lines")))
         (SETQ PL
                 (BLDMSG_INTERNAL "%w%w%w%w"
                                  (LIST PL ", '" S
                                        "' using ($1/60000):(0) title \"step        :\"")))
         NIL))
       (T
        (PROG ()
         REPEATLABEL
          (PROGN
           (PROGN
            (PRIN2 "Do you want to add to the plot a graph for the ")
            NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - method used at each step:   1") NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - number of unknowns:         2") NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - number of pdes:             3") NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - number of terms:            4") NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - number of factors/term:     5") NIL)
           (TERPRI)
           (PROGN (PRIN2 "  - number of last free cells:  6") NIL)
           (TERPRI)
           (PROGN (PRIN2 "or add no further graphs:       n          ") NIL)
           (SETQ H (TERMREAD))
           (COND
            ((OR (EQUAL H 1) (EQUAL H 2) (EQUAL H 3) (EQUAL H 4) (EQUAL H 5)
                 (EQUAL H 6))
             (PROGN
              (PROGN (PRIN2 "What is the scaling factor for this graph? ") NIL)
              (PROG ()
               REPEATLABEL
                (SETQ SF (TERMREAD))
                (COND ((NOT (FIXP SF)) (GO REPEATLABEL))))
              (COND ((NULL PL) (SETQ PL "plot "))
                    (T (SETQ PL (BLDMSG_INTERNAL "%w%w" (LIST PL ",")))))
              (COND
               ((EQUAL H 1)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$2) title \"method      :\""))))
               ((EQUAL H 2)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$3) title \"unknowns    :\""))))
               ((EQUAL H 3)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$4) title \"equations   :\""))))
               ((EQUAL H 4)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$5) title \"all terms   :\""))))
               ((EQUAL H 5)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$6/$5) title \"factors/term:\""))))
               ((EQUAL H 6)
                (SETQ PL
                        (BLDMSG_INTERNAL "%w%w%w%w%d%w"
                                         (LIST PL "'" S "' using ($1/60000):("
                                               SF
                                               "*$7) title \"free cells  :\"")))))
              NIL)))
           NIL)
          (COND ((NOT (EQUAL H 'N)) (GO REPEATLABEL))))))
      (SETQ TL "set title \"Modules in order of their priority: ")
      (PROG (H)
        (SETQ H PROLI)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (SETQ TL (BLDMSG_INTERNAL "%w%d%w" (LIST TL (CAR H) " "))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ TL (BLDMSG_INTERNAL "%w%w" (LIST TL "\" ")))
      (AEVAL (LIST 'GNUPLOT TL))
      (COND
       ((OR QUICK
            (YESP "Do you want the x-range to be determined automatically? "))
        (PROGN
         (AEVAL (LIST 'GNUPLOT "set autoscale x"))
         (AEVAL (LIST 'GNUPLOT "set autoscale y"))
         NIL))
       (T
        (PROGN
         (PROGN
          (PRIN2 "What is the minimal value of x (time in minutes) ? ")
          NIL)
         (SETQ MINT (TERMREAD))
         (PROGN
          (PRIN2 "What is the maximal value of x (time in minutes) ? ")
          NIL)
         (SETQ MAXT (TERMREAD))
         (SETQ TL
                 (BLDMSG_INTERNAL "%w%d%w%d%w"
                                  (LIST "set xrange [" MINT ":" MAXT "]")))
         (AEVAL (LIST 'GNUPLOT "set noautoscale"))
         (AEVAL (LIST 'GNUPLOT "set autoscale y"))
         (AEVAL (LIST 'GNUPLOT TL))
         NIL)))
      (AEVAL (LIST 'GNUPLOT "set key Left"))
      (COND
       ((OR QUICK (YESP "Do you want to display the plot on the screen? "))
        (PROGN NIL))
       ((YESP "Do you want to print the plot? ")
        (PROGN
         (SETQ PLOTHEADER* "")
         (AEVAL (LIST 'GNUPLOT "set output '|lpr -Pmath4'"))
         (AEVAL (LIST 'GNUPLOT "set terminal postscript eps 22"))
         NIL))
       (T
        (PROGN
         (PROGN
          (PRIN2 "Give the file name in which to save the plot in \" \": ")
          NIL)
         (SETQ TL (TERMREAD))
         (SETQ TL (BLDMSG_INTERNAL "%w%w%w" (LIST "set output '" TL "'")))
         (SETQ PLOTHEADER* "")
         (AEVAL (LIST 'GNUPLOT TL))
         (AEVAL (LIST 'GNUPLOT "set terminal postscript eps 22"))
         NIL)))
      (AEVAL (LIST 'GNUPLOT PL))
      (AEVAL (NULL (EVAL '(PLOTSHOW))))
      (RESTORE_INTERACTIVE_PROMPT))) 
(FLAG '(PLOT_STAT) 'OPFN) 
(PUT 'PLOT_STAT 'NUMBER-OF-ARGS 0) 
(PUT 'PLOT_STAT 'DEFINED-ON-LINE '1861) 
(PUT 'PLOT_STAT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLOT_STAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PLOT_STAT NIL
    (PROG (S ASK)
      (CHANGE_PROMPT_TO "")
      (COND ((NULL SESSION_) (SETQ ASK T))
            (T
             (PROGN
              (PROGN
               (PRIN2 "Do you want to plot statistics of this session,")
               NIL)
              (TERPRI)
              (COND
               ((NOT (YESP "i.e. since loading CRACK the last time? "))
                (SETQ ASK T))))))
      (COND
       (ASK
        (PROGN
         (ASK_FOR_SESSION)
         (SETQ S (BLDMSG_INTERNAL "%w.%w" (LIST SESSION_ "size_hist")))
         (IN (LIST S)))))
      (PLOT_STATISTICS SIZE_HIST)
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'LIST_CASES 'NUMBER-OF-ARGS 1) 
(PUT 'LIST_CASES 'DEFINED-ON-LINE '1878) 
(PUT 'LIST_CASES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_CASES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIST_CASES (SIZE_HISTORY)
    (PROG (S M N P H CNTR LASTSTEP LASTP LL SH)
      (SETQ LL (LINELENGTH 250))
      (CHANGE_PROMPT_TO "")
      (AEVAL (OFF (LIST 'NAT)))
      (COND
       ((NEQ SIZE_WATCH T)
        (PROGN
         (PROGN
          (PRIN2 "Warning: Because the parameter size_watch was set to ")
          (PRIN2 SIZE_WATCH)
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "(to save memory in long computations) only the last ")
          (PRIN2 SIZE_WATCH)
          (PRIN2 " steps")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "are recorded, i.e. early cases may be missing in this listing.")
          NIL)
         (TERPRI)
         (TERPRI)
         NIL)))
      (PROGN (PRIN2 "Start") NIL)
      (SETQ CNTR 0)
      (SETQ LASTSTEP 0)
      (SETQ LASTP NIL)
      (SETQ N 0)
      (SETQ SH (REVERSE SIZE_HISTORY))
      (PROG ()
       WHILELABEL
        (COND ((NOT SH) (RETURN NIL)))
        (PROGN
         (SETQ P (CAAR SH))
         (COND
          ((EQUAL P 'A)
           (PROGN
            (SETQ H (DIFFERENCE LASTSTEP CNTR))
            (PROGN
             (PRIN2 " : ")
             (PRIN2 H)
             (PRIN2 (COND ((EQUAL H 1) " step") (T " steps")))
             NIL)
            (TERPRI)
            (SETQ CNTR LASTSTEP)
            (SETQ N (ADD1 N))
            (SETQ H (CADDDR (CAR SH)))
            (SETQ S "")
            (PROG (M)
              (SETQ M (CADDR (CAR SH)))
             LAB
              (COND ((NULL M) (RETURN NIL)))
              ((LAMBDA (M) (SETQ S (BLDMSG_INTERNAL "%w%d" (LIST S M))))
               (CAR M))
              (SETQ M (CDR M))
              (GO LAB))
            (PROGN (PRIN2 S) NIL)
            (COND
             (H
              (COND ((ATOM H) (PROGN (PRIN2 H) NIL))
                    (T
                     (PROG ()
                      REPEATLABEL
                       (PROGN
                        (COND
                         ((EQUAL (CAAR H) 'EQUAL)
                          (PROGN
                           (PROGN (PRIN2 " 0=") NIL)
                           (MAPRIN (CADDR (CAR H)))))
                         ((EQUAL (CAAR H) 'INEQ)
                          (PROGN
                           (PROGN (PRIN2 " 0<>") NIL)
                           (MAPRIN (CADDR (CAR H))))))
                        (SETQ H (CDR H))
                        (COND
                         (H
                          (PROGN
                           (SETQ S "")
                           (PROG (M)
                             (SETQ M (CADDR (CAR SH)))
                            LAB
                             (COND ((NULL M) (RETURN NIL)))
                             ((LAMBDA (M)
                                (SETQ S (BLDMSG_INTERNAL "%w%w" (LIST S " "))))
                              (CAR M))
                             (SETQ M (CDR M))
                             (GO LAB))
                           (PROGN (PRIN2 S) NIL)))))
                       (COND ((NOT (NULL H)) (GO REPEATLABEL))))))))))
          ((EQUAL P 'Z)
           (PROGN
            (SETQ N (SUB1 N))
            (COND
             ((NEQ LASTP 'Z)
              (PROGN
               (PRIN2 ", ")
               (PRIN2 (CADDDR (CAR SH)))
               (PRIN2 " soln")
               NIL)))))
          ((NUMBERP (CAAR SH)) (SETQ LASTSTEP (CADAR SH))))
         (COND
          ((AND (EQUAL SIZE_WATCH T) (OR (EQUAL P 'A) (EQUAL P 'Z))
                (NEQ N (LENGTH (CADDAR SH))))
           (PROGN
            (PROGN
             (PRIN2 "Somthing is wrong with level counting in size_hist")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "n=")
             (PRIN2 N)
             (PRIN2 " level:")
             (PRIN2 (CADDAR SH))
             NIL)
            (TERPRI)
            NIL)))
         (SETQ LASTP P)
         (SETQ SH (CDR SH)))
        (GO WHILELABEL))
      (TERPRI)
      (AEVAL (ON (LIST 'NAT)))
      (RESTORE_INTERACTIVE_PROMPT)
      (LINELENGTH LL))) 
(PUT 'LIST_GLOBAL_CRACK_VARIABLES 'NUMBER-OF-ARGS 0) 
(PUT 'LIST_GLOBAL_CRACK_VARIABLES 'DEFINED-ON-LINE '1943) 
(PUT 'LIST_GLOBAL_CRACK_VARIABLES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_GLOBAL_CRACK_VARIABLES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIST_GLOBAL_CRACK_VARIABLES NIL
    (PROG (H)
      (SETQ H GLOBAL_VAR)
     LAB
      (COND ((NULL H) (RETURN NIL)))
      ((LAMBDA (H)
         (PROGN
          (TERPRI)
          (PROGN (PRIN2 "variable: ") (PRIN2 H) NIL)
          (TERPRI)
          (PROGN (PRIN2 "value: ") NIL)
          (COND
           ((OR (EQUAL H 'BACKUP_) (AND (EQUAL H 'SIZE_HIST) (EVAL H)))
            (PROGN
             (PRIN2 " as this value might be
  large, please print it in a separate command:  pv ")
             (PRIN2 H)
             NIL))
           (T (PROGN (PRIN2 (EVAL H)) NIL)))
          (TERPRI)
          (PROGN (PRIN2 "description: ") (PRIN2 (GET H 'DESCRIPTION)) NIL)
          (TERPRI)
          (COND
           ((AND (FREEOF NOT_PASSED_BACK H) (FREEOF PASSED_BACK H))
            (PROGN (PROGN (PRIN2 "not in not_passed_back, passed_back") NIL))))
          (TERPRI)))
       (CAR H))
      (SETQ H (CDR H))
      (GO LAB))) 
(PUT 'DESCRIBE_ID 'NUMBER-OF-ARGS 0) 
(PUT 'DESCRIBE_ID 'DEFINED-ON-LINE '1957) 
(PUT 'DESCRIBE_ID 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DESCRIBE_ID 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DESCRIBE_ID NIL
    (PROG (H HH)
      (CHANGE_PROMPT_TO "")
      (PROGN (PRIN2 "Please enter the interactive command or ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "             the number of a module or ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "             the global variable: ") NIL)
      (TERPRI)
      (SETQ H (TERMREAD))
      (COND
       ((FIXP H)
        (COND
         ((OR (LEQ H 0) (GREATERP H (LENGTH FULL_PROC_LIST_)))
          (PROGN
           (PROGN
            (PRIN2 "The number must be in 1 .. ")
            (PRIN2 (LENGTH FULL_PROC_LIST_))
            (PRIN2 " .")
            NIL)
           (TERPRI)))
         (T
          (PROGN
           (SETQ HH (NTH FULL_PROC_LIST_ H))
           (COND ((LESSP H 10) (PROGN (PRIN2 " ") NIL)))
           (PROGN (PRIN2 H) (PRIN2 " : procedure:   ") (PRIN2 HH) NIL)
           (TERPRI)
           (PROGN (PRIN2 "     description: ") NIL)
           (SETQ HH (GET HH 'DESCRIPTION))
           (PROG (H)
             (SETQ H HH)
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H) (PROGN (PRIN2 H) NIL)) (CAR H))
             (SETQ H (CDR H))
             (GO LAB))))))
       ((MEMBER H GLOBAL_VAR)
        (PROGN
         (PRIN2 H)
         (PRIN2 " (global variable): ")
         (PRIN2 (CAR (GET H 'DESCRIPTION)))
         NIL))
       (T
        (PROGN
         (SETQ HH (MKID 'I_ H))
         (COND
          ((MEMBER HH GLOBAL_VAR)
           (PROGN
            (PRIN2 H)
            (PRIN2 " (interactive command): ")
            (PRIN2 (CAR (GET HH 'DESCRIPTION)))
            NIL))
          (T
           (PROGN
            (PROGN
             (PRIN2 H)
             (PRIN2 " is not a global variable and not a command.")
             NIL)
            (TERPRI))))))))) 
(FLAG '(PRINT_TREE) 'OPFN) 
(PUT 'PRINT_TREE 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_TREE 'DEFINED-ON-LINE '1982) 
(PUT 'PRINT_TREE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_TREE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_TREE NIL
    (PROG (S ASK)
      (CHANGE_PROMPT_TO "")
      (COND ((NULL SESSION_) (SETQ ASK T))
            (T
             (PROGN
              (PROGN
               (PRIN2
                "Do you want to print the tree of cases of this session,")
               NIL)
              (TERPRI)
              (COND
               ((NOT (YESP "i.e. since loading CRACK the last time? "))
                (SETQ ASK T)))
              (TERPRI))))
      (COND
       (ASK
        (PROGN
         (ASK_FOR_SESSION)
         (SETQ S (BLDMSG_INTERNAL "%w.%w" (LIST SESSION_ "size_hist")))
         (IN (LIST S)))))
      (LIST_CASES SIZE_HIST)
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'MODIFY_PROC_LIST 'NUMBER-OF-ARGS 2) 
(PUT 'MODIFY_PROC_LIST 'DEFINED-ON-LINE '2003) 
(PUT 'MODIFY_PROC_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MODIFY_PROC_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODIFY_PROC_LIST (METHOD REVSL)
    (PROG (PLBAK PLCOP OK)
      (SETQ PLBAK PROC_LIST_)
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) METHOD)))
          (RETURN NIL)))
        (COND
         ((NOT (FREEOF REVSL (CAR PROC_LIST_)))
          (PROGN
           (PROGN (PRIN2 "*** Mis-use of ") (PRIN2 METHOD) NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "*** ")
            (PRIN2 (CAR PROC_LIST_))
            (PRIN2 " came before ")
            (PRIN2 METHOD)
            (PRIN2 " in proc_list_ !")
            NIL)
           (TERPRI)
           (SETQ PROC_LIST_ PLBAK)
           (SETQ OK NIL)))
         (T
          (PROGN
           (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
           (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
        (GO WHILELABEL))
      (COND
       (OK
        (PROGN
         (SETQ PLCOP (CONS METHOD PLCOP))
         (COND (PROC_LIST_ (SETQ PROC_LIST_ (CDR PROC_LIST_))))
         (SETQ PLCOP (APPEND REVSL PLCOP))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND PROC_LIST_ (MEMBER (CAR PROC_LIST_) REVSL)))
             (RETURN NIL)))
           (SETQ PROC_LIST_ (CDR PROC_LIST_))
           (GO WHILELABEL))
         (PROG ()
          WHILELABEL
           (COND ((NOT PROC_LIST_) (RETURN NIL)))
           (PROGN
            (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
            (SETQ PROC_LIST_ (CDR PROC_LIST_)))
           (GO WHILELABEL))
         (SETQ PROC_LIST_ (REVERSE PLCOP))
         (COND
          (PRINT_MORE
           (PROGN
            (PROGN (PRIN2 "New proc_list_ based on ") (PRIN2 METHOD) NIL)
            (TERPRI))))))))) 
(PUT 'CHOOSE_6_20 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_6_20 'DEFINED-ON-LINE '2051) 
(PUT 'CHOOSE_6_20 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_6_20 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_6_20 (ARGLIST)
    (COND
     ((FREEOF PROC_LIST_ 'SUBST_LEVEL_35)
      (PROG (ALLTERMS UNKN PLBAK PLCOP P OK SHCOP)
        (COND
         (SIZE_WATCH
          (PROGN
           (SETQ SHCOP SIZE_HIST)
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND SHCOP (NOT (FIXP (CAAR SHCOP))))) (RETURN NIL)))
             (SETQ SHCOP (CDR SHCOP))
             (GO WHILELABEL)))))
        (COND
         ((NULL SHCOP)
          (PROGN
           (SETQ UNKN (LENGTH FTEM_))
           (SETQ ALLTERMS
                   (PROG (P FORALL-RESULT)
                     (SETQ P (CAR ARGLIST))
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS ((LAMBDA (P) (GET P 'TERMS)) (CAR P))
                                   FORALL-RESULT))
                     (SETQ P (CDR P))
                     (GO LAB1)))))
         (T
          (PROGN
           (SETQ UNKN (CADDDR (CAR SHCOP)))
           (SETQ ALLTERMS (CADDDR (CDDAR SHCOP))))))
        (COND
         ((AND (LEQ UNKN CHOOSE_6_20_MAX_FTEM)
               (LEQ ALLTERMS CHOOSE_6_20_MAX_TERMS))
          (PROGN
           (SETQ PLBAK PROC_LIST_)
           (SETQ OK T)
           (PROG ()
            WHILELABEL
             (COND
              ((NOT (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) 'CHOOSE_6_20)))
               (RETURN NIL)))
             (COND
              ((EQUAL (CAR PROC_LIST_) 'SUBST_LEVEL_45)
               (PROGN
                (PROGN (PRIN2 "*** Mis-use of choose_6_20") NIL)
                (TERPRI)
                (PROGN
                 (PRIN2
                  "*** subst_level_45 came before choose_6_20 in proc_list_ !")
                 NIL)
                (TERPRI)
                (SETQ PROC_LIST_ PLBAK)
                (SETQ OK NIL)))
              (T
               (PROGN
                (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
                (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
             (GO WHILELABEL))
           (COND
            (OK
             (PROGN
              (COND (PROC_LIST_ (SETQ PROC_LIST_ (CDDR PROC_LIST_))))
              (SETQ PLCOP (CONS 'SUBST_LEVEL_35 PLCOP))
              (PROG ()
               WHILELABEL
                (COND ((NOT PROC_LIST_) (RETURN NIL)))
                (PROGN
                 (COND
                  ((FREEOF PLCOP (CAR PROC_LIST_))
                   (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))))
                 (SETQ PROC_LIST_ (CDR PROC_LIST_)))
                (GO WHILELABEL))
              (SETQ PROC_LIST_ (REVERSE PLCOP))
              (COND
               (PRINT_MORE
                (PROGN
                 (PROGN
                  (PRIN2 "proc_list_ has been automatically changed.")
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "6 is changed to 20.") NIL)
                 (TERPRI)))))))))))))) 
(PUT 'CHOOSE_27_8_16 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_27_8_16 'DEFINED-ON-LINE '2144) 
(PUT 'CHOOSE_27_8_16 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_27_8_16 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_27_8_16 (ARGLIST)
    (PROG (TOO_MUCH_27 SHCP SH1 SH2 N H PLBAK PLCOP OK)
      (COND
       ((NULL SIZE_WATCH)
        (PROGN
         (PROGN (PRIN2 "*** choose_27_8_16 needs size_watch=t !") NIL)
         (TERPRI)))
       (T
        (PROGN
         (SETQ SHCP SIZE_HIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
           (SETQ SHCP (CDR SHCP))
           (GO WHILELABEL))
         (COND
          ((AND SHCP (EQUAL (CAAR SHCP) 27))
           (PROGN
            (SETQ SH1 (CAR SHCP))
            (SETQ SHCP (CDR SHCP))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
              (SETQ SHCP (CDR SHCP))
              (GO WHILELABEL))
            (COND
             ((AND SHCP (EQUAL (CAAR SHCP) 27))
              (PROGN
               (SETQ SH2 (CAR SHCP))
               (SETQ SHCP (CDR SHCP))
               (SETQ N 0)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND SH2 (EQUAL (CAR SH2) 27))) (RETURN NIL)))
                 (PROGN
                  (SETQ H (DIFFERENCE (CADDR SH1) (CADDR SH2)))
                  (SETQ N (PLUS N (QUOTIENT H 60000)))
                  (COND ((GEQ N CHOOSE_27_8_16_MAX) (SETQ TOO_MUCH_27 T)))
                  (SETQ SH1 SH2)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
                    (SETQ SHCP (CDR SHCP))
                    (GO WHILELABEL))
                  (COND ((NULL SHCP) (SETQ SH2 NIL))
                        (T
                         (PROGN
                          (SETQ SH2 (CAR SHCP))
                          (SETQ SHCP (CDR SHCP))))))
                 (GO WHILELABEL))))))))
         NIL)))
      (SETQ PLBAK PROC_LIST_)
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) 'CHOOSE_27_8_16)))
          (RETURN NIL)))
        (COND
         ((NOT
           (FREEOF
            (LIST 'DIFF_LENGTH_REDUCTION 'SUBST_LEVEL_3
                  'FACTORIZE_TO_SUBSTITUTE)
            (CAR PROC_LIST_)))
          (PROGN
           (PROGN (PRIN2 "*** Mis-use of choose_27_8_16") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "*** ")
            (PRIN2 (CAR PROC_LIST_))
            (PRIN2 " came before choose_27_8_16 in proc_list_ !")
            NIL)
           (TERPRI)
           (SETQ PROC_LIST_ PLBAK)
           (SETQ OK NIL)))
         (T
          (PROGN
           (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
           (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
        (GO WHILELABEL))
      (COND
       (OK
        (PROGN
         (SETQ PLCOP (CONS 'CHOOSE_27_8_16 PLCOP))
         (COND (PROC_LIST_ (SETQ PROC_LIST_ (CDR PROC_LIST_))))
         (COND
          (TOO_MUCH_27
           (SETQ PLCOP
                   (APPEND
                    (LIST 'DIFF_LENGTH_REDUCTION 'SUBST_LEVEL_3
                          'FACTORIZE_TO_SUBSTITUTE)
                    PLCOP)))
          (T
           (SETQ PLCOP
                   (APPEND
                    (LIST 'SUBST_LEVEL_3 'FACTORIZE_TO_SUBSTITUTE
                          'DIFF_LENGTH_REDUCTION)
                    PLCOP))))
         (PROG ()
          WHILELABEL
           (COND ((NOT PROC_LIST_) (RETURN NIL)))
           (PROGN
            (COND
             ((FREEOF PLCOP (CAR PROC_LIST_))
              (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))))
            (SETQ PROC_LIST_ (CDR PROC_LIST_)))
           (GO WHILELABEL))
         (SETQ PROC_LIST_ (REVERSE PLCOP))
         (COND
          (PRINT_MORE
           (PROGN
            (PROGN (PRIN2 "proc_list_ has been automatically changed.") NIL)
            (TERPRI)
            (COND (TOO_MUCH_27 (PROGN (PRIN2 "The order is 8,16,27.") NIL))
                  (T (PROGN (PRIN2 "The order is 27,8,16.") NIL)))
            (TERPRI)))))))
      (SETQ ARGLIST NIL))) 
(PUT 'CHOOSE_30_47_21 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_30_47_21 'DEFINED-ON-LINE '2264) 
(PUT 'CHOOSE_30_47_21 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_30_47_21 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_30_47_21 (ARGLIST)
    (PROG (TOO_MUCH_30 SHCP SH1 SH2 N H PLBAK PLCOP OK SHCOP UNKN ALLTERMS P)
      (COND
       ((NULL SIZE_WATCH)
        (PROGN
         (PROGN (PRIN2 "*** choose_30_47_21 needs size_watch=t !") NIL)
         (TERPRI)))
       (T
        (PROGN
         (SETQ SHCP SIZE_HIST)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
           (SETQ SHCP (CDR SHCP))
           (GO WHILELABEL))
         (COND
          ((AND SHCP (EQUAL (CAAR SHCP) 30))
           (PROGN
            (SETQ SH1 (CAR SHCP))
            (SETQ SHCP (CDR SHCP))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
              (SETQ SHCP (CDR SHCP))
              (GO WHILELABEL))
            (COND
             ((AND SHCP (EQUAL (CAAR SHCP) 30))
              (PROGN
               (SETQ SH2 (CAR SHCP))
               (SETQ SHCP (CDR SHCP))
               (SETQ N 0)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND SH2 (EQUAL (CAR SH2) 30))) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((GEQ (CADDDR (CDR SH1)) (CADDDR (CDR SH2)))
                    (PROGN
                     (SETQ N (ADD1 N))
                     (COND
                      ((GREATERP (CADDDR (CDR SH1)) (CADDDR (CDR SH2)))
                       (SETQ N (ADD1 N))))
                     (SETQ H (DIFFERENCE (CADDR SH1) (CADDR SH2)))
                     (SETQ N (PLUS N (QUOTIENT H 60000)))
                     (COND
                      ((GEQ N CHOOSE_30_47_21_MAX) (SETQ TOO_MUCH_30 T))))))
                  (SETQ SH1 SH2)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
                    (SETQ SHCP (CDR SHCP))
                    (GO WHILELABEL))
                  (COND ((NULL SHCP) (SETQ SH2 NIL))
                        (T
                         (PROGN
                          (SETQ SH2 (CAR SHCP))
                          (SETQ SHCP (CDR SHCP))))))
                 (GO WHILELABEL))))))))
         NIL)))
      (SETQ PLBAK PROC_LIST_)
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) 'CHOOSE_30_47_21)))
          (RETURN NIL)))
        (COND
         ((NOT
           (FREEOF (LIST 'DECOUPLING 'SUBST_LEVEL_4 'FACTORIZE_ANY)
                   (CAR PROC_LIST_)))
          (PROGN
           (PROGN (PRIN2 "*** Mis-use of choose_30_47_21") NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "*** ")
            (PRIN2 (CAR PROC_LIST_))
            (PRIN2 " came before choose_30_47_21 in proc_list_ !")
            NIL)
           (TERPRI)
           (SETQ PROC_LIST_ PLBAK)
           (SETQ OK NIL)))
         (T
          (PROGN
           (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
           (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
        (GO WHILELABEL))
      (COND
       (OK
        (PROGN
         (SETQ PLCOP (CONS 'CHOOSE_30_47_21 PLCOP))
         (COND
          ((MEMBER 'EXTERNAL_GROEBNER PROC_LIST_)
           (PROGN
            (SETQ PROC_LIST_ (DELETE 'EXTERNAL_GROEBNER PROC_LIST_))
            (SETQ H (LENGTH FTEM_))
            (COND
             ((LEQ H GROEB_DIFF_MAX)
              (SETQ PLCOP (CONS 'EXTERNAL_GROEBNER PLCOP)))))))
         (COND (PROC_LIST_ (SETQ PROC_LIST_ (CDR PROC_LIST_))))
         (COND
          (TOO_MUCH_30
           (PROGN
            (COND
             (SIZE_WATCH
              (PROGN
               (SETQ SHCOP SIZE_HIST)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND SHCOP (NOT (FIXP (CAAR SHCOP))))) (RETURN NIL)))
                 (SETQ SHCOP (CDR SHCOP))
                 (GO WHILELABEL)))))
            (COND
             ((NULL SHCOP)
              (PROGN
               (SETQ UNKN (LENGTH FTEM_))
               (SETQ ALLTERMS
                       (PROG (P FORALL-RESULT)
                         (SETQ P (CAR ARGLIST))
                         (SETQ FORALL-RESULT 0)
                        LAB1
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (SETQ FORALL-RESULT
                                 (PLUS ((LAMBDA (P) (GET P 'TERMS)) (CAR P))
                                       FORALL-RESULT))
                         (SETQ P (CDR P))
                         (GO LAB1)))))
             (T
              (PROGN
               (SETQ UNKN (CADDDR (CAR SHCOP)))
               (SETQ ALLTERMS (CADDDR (CDDAR SHCOP))))))
            (COND
             ((AND (LEQ UNKN CHOOSE_6_20_MAX_FTEM)
                   (LEQ ALLTERMS CHOOSE_6_20_MAX_TERMS))
              (SETQ PLCOP
                      (APPEND (LIST 'DECOUPLING 'SUBST_LEVEL_4 'FACTORIZE_ANY)
                              PLCOP)))
             (T
              (SETQ PLCOP
                      (APPEND (LIST 'SUBST_LEVEL_4 'DECOUPLING 'FACTORIZE_ANY)
                              PLCOP))))))
          (T
           (SETQ PLCOP
                   (APPEND (LIST 'SUBST_LEVEL_4 'FACTORIZE_ANY 'DECOUPLING)
                           PLCOP))))
         (PROG ()
          WHILELABEL
           (COND ((NOT PROC_LIST_) (RETURN NIL)))
           (PROGN
            (COND
             ((FREEOF PLCOP (CAR PROC_LIST_))
              (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))))
            (SETQ PROC_LIST_ (CDR PROC_LIST_)))
           (GO WHILELABEL))
         (SETQ PROC_LIST_ (REVERSE PLCOP))
         (COND
          (PRINT_MORE
           (PROGN
            (PROGN (PRIN2 "proc_list_ has been automatically changed.") NIL)
            (TERPRI)
            (COND (TOO_MUCH_30 (PROGN (PRIN2 "The order is 47,21,30.") NIL))
                  (T (PROGN (PRIN2 "The order is 30,47,21.") NIL)))
            (TERPRI)))))))
      (SETQ ARGLIST NIL))) 
(PUT 'CHOOSE_70_65_8_47 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_70_65_8_47 'DEFINED-ON-LINE '2410) 
(PUT 'CHOOSE_70_65_8_47 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_70_65_8_47 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_70_65_8_47 (ARGLIST)
    (PROG (CSH PLBAK OK PLCOP DO_SPLIT SL SHCP)
      (SETQ SHCP SIZE_HIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
        (SETQ SHCP (CDR SHCP))
        (GO WHILELABEL))
      (COND
       (SHCP
        (PROGN
         (SETQ SL
                 (LIST 'PRE_DETERMINED_CASE_SPLITS 'CASE_ON_MOST_FREQU_FNC
                       'FACTORIZE_TO_SUBSTITUTE 'FACTORIZE_ANY))
         (SETQ CSH (CAR SIZE_HIST))
         (COND
          ((OR
            (GREATERP (TIMES 100 (CADR (CDDDDR CSH)))
                      (TIMES CHOOSE_70_65_8_47_RATIOTERMS
                             CHOOSE_70_65_8_47_ORIGTERMS))
            (LESSP (TIMES 100 (CADDDR (CDDDDR CSH)))
                   (TIMES CHOOSE_70_65_8_47_RATIOMEM
                          CHOOSE_70_65_8_47_ORIGMEM)))
           (SETQ DO_SPLIT T)))
         (SETQ PLBAK PROC_LIST_)
         (SETQ OK T)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) 'CHOOSE_70_65_8_47)))
             (RETURN NIL)))
           (COND
            ((NOT (FREEOF SL (CAR PROC_LIST_)))
             (PROGN
              (PROGN (PRIN2 "*** Mis-use of choose_70_65_8_47") NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "*** ")
               (PRIN2 (CAR PROC_LIST_))
               (PRIN2 " came before choose_70_65_8_47 in proc_list_ !")
               NIL)
              (TERPRI)
              (SETQ PROC_LIST_ PLBAK)
              (SETQ OK NIL)))
            (T
             (PROGN
              (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
              (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
           (GO WHILELABEL))
         (COND
          (OK
           (PROGN
            (SETQ PLCOP (CONS 'CHOOSE_70_65_8_47 PLCOP))
            (COND (PROC_LIST_ (SETQ PROC_LIST_ (CDR PROC_LIST_))))
            (COND (DO_SPLIT (SETQ PLCOP (APPEND SL PLCOP))))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND PROC_LIST_ (MEMBER (CAR PROC_LIST_) SL)))
                (RETURN NIL)))
              (SETQ PROC_LIST_ (CDR PROC_LIST_))
              (GO WHILELABEL))
            (PROG ()
             WHILELABEL
              (COND ((NOT PROC_LIST_) (RETURN NIL)))
              (PROGN
               (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
               (SETQ PROC_LIST_ (CDR PROC_LIST_)))
              (GO WHILELABEL))
            (SETQ PROC_LIST_ (REVERSE PLCOP))
            (COND
             ((AND PRINT_MORE DO_SPLIT)
              (PROGN
               (PROGN (PRIN2 "proc_list_ has been automatically changed.") NIL)
               (TERPRI)
               (PROGN (PRIN2 "70,8,47 has been inserted.") NIL)
               (TERPRI))))))))))
      (SETQ ARGLIST NIL))) 
(PUT 'CHOOSE_30_47_72 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_30_47_72 'DEFINED-ON-LINE '2500) 
(PUT 'CHOOSE_30_47_72 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_30_47_72 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_30_47_72 (ARGLIST)
    (PROG (SHCP CSH REVSL)
      (SETQ SHCP SIZE_HIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SHCP (NOT (FIXP (CAAR SHCP))))) (RETURN NIL)))
        (SETQ SHCP (CDR SHCP))
        (GO WHILELABEL))
      (COND
       (SHCP
        (PROGN
         (SETQ CSH (CAR SIZE_HIST))
         (COND
          ((EQUAL (CAR (CDDDDR CSH)) 0) (SETQ REVSL (LIST 'READ_EQUATION)))
          ((LESSP (CAR (CDDDDR CSH)) CHOOSE_30_47_72_EQN)
           (SETQ REVSL (LIST 'DECOUPLING 'FACTORIZE_ANY 'READ_EQUATION)))
          ((AND (NEQ (CAR CSH) 30) (PAIRP (CDR SHCP)) (NEQ (CAADR SHCP) 30))
           (SETQ REVSL (LIST 'READ_EQUATION 'FACTORIZE_ANY 'DECOUPLING)))
          (T (SETQ REVSL (LIST 'READ_EQUATION 'DECOUPLING 'FACTORIZE_ANY))))
         (MODIFY_PROC_LIST 'CHOOSE_30_47_72 REVSL)
         NIL)))
      (SETQ ARGLIST NIL))) 
(PUT 'CHOOSE_11_30 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE_11_30 'DEFINED-ON-LINE '2559) 
(PUT 'CHOOSE_11_30 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHOOSE_11_30 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE_11_30 (ARGLIST)
    (COND
     (SIZE_WATCH
      (PROG (SHCOP N11 N30 OK PLBAK PLCOP LAST_11_TIME LAST_30_TIME LAST_SIZE
             STEPS_AGO)
        (SETQ SHCOP SIZE_HIST)
        (SETQ N11 0)
        (SETQ N30 0)
        (SETQ STEPS_AGO 0)
        (SETQ LAST_SIZE
                (GET_STATISTIC (CAR ARGLIST)
                 (APPEND (CADR ARGLIST) (SETDIFF FTEM_ (CADR ARGLIST)))))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (AND SHCOP
                  (OR (NEQ (CAAR SHCOP) 72) (NULL LAST_11_TIME)
                      (NULL LAST_30_TIME))
                  (OR (NOT (FIXP (CAAR SHCOP))) (LESSP N11 CHOOSE_11_30_MAX_11)
                      (LEQ N30 CHOOSE_11_30_MAX_30))))
            (RETURN NIL)))
          (PROGN
           (COND
            ((FIXP (CAAR SHCOP))
             (PROGN
              (SETQ STEPS_AGO (ADD1 STEPS_AGO))
              (COND
               ((EQUAL (CAAR SHCOP) 11)
                (PROGN
                 (SETQ N11 (ADD1 N11))
                 (COND
                  ((NULL LAST_11_TIME)
                   (SETQ LAST_11_TIME
                           (TIMES (DIFFERENCE (CADR LAST_SIZE) (CADDAR SHCOP))
                                  (QUOTIENT 50 (PLUS 50 STEPS_AGO))))))
                 NIL))
               ((EQUAL (CAAR SHCOP) 30)
                (PROGN
                 (SETQ N30 (ADD1 30))
                 (COND
                  ((NULL LAST_30_TIME)
                   (SETQ LAST_30_TIME
                           (TIMES (DIFFERENCE (CADR LAST_SIZE) (CADDAR SHCOP))
                                  (QUOTIENT 50 (PLUS 50 STEPS_AGO))))))
                 NIL)))
              (SETQ LAST_SIZE (CDAR SHCOP)))))
           (SETQ SHCOP (CDR SHCOP)))
          (GO WHILELABEL))
        (COND ((NULL LAST_11_TIME) (SETQ LAST_11_TIME 0)))
        (COND ((NULL LAST_30_TIME) (SETQ LAST_30_TIME 0)))
        (SETQ PLBAK PROC_LIST_)
        (SETQ OK T)
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (AND OK PROC_LIST_ (NEQ (CAR PROC_LIST_) 'CHOOSE_11_30)))
            (RETURN NIL)))
          (COND
           ((OR (EQUAL (CAR PROC_LIST_) 'ALG_LENGTH_REDUCTION)
                (EQUAL (CAR PROC_LIST_) 'DECOUPLING))
            (PROGN
             (PROGN (PRIN2 "*** Mis-use of choose_11_30") NIL)
             (TERPRI)
             (PROGN
              (PRIN2 "*** alg_length_reduction (11) or decoupling (30)")
              NIL)
             (TERPRI)
             (PROGN (PRIN2 "*** came before choose_11_30 in proc_list_ !") NIL)
             (TERPRI)
             (TERPRI)
             (SETQ PROC_LIST_ PLBAK)
             (SETQ OK NIL)))
           (T
            (PROGN
             (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
             (SETQ PROC_LIST_ (CDR PROC_LIST_)))))
          (GO WHILELABEL))
        (COND
         (OK
          (PROGN
           (PROG ()
            WHILELABEL
             (COND
              ((NOT
                (AND PROC_LIST_
                     (OR (EQUAL (CAR PROC_LIST_) 'CHOOSE_11_30)
                         (EQUAL (CAR PROC_LIST_) 'ALG_LENGTH_REDUCTION)
                         (EQUAL (CAR PROC_LIST_) 'DECOUPLING))))
               (RETURN NIL)))
             (SETQ PROC_LIST_ (CDR PROC_LIST_))
             (GO WHILELABEL))
           (SETQ PLCOP (CONS 'CHOOSE_11_30 PLCOP))
           (COND
            ((AND (LESSP LAST_11_TIME 4000) (LESSP N11 CHOOSE_11_30_MAX_11))
             (SETQ PLCOP (CONS 'ALG_LENGTH_REDUCTION PLCOP))))
           (COND
            ((AND (LESSP LAST_30_TIME 2000) (LESSP N30 CHOOSE_11_30_MAX_30))
             (SETQ PLCOP (CONS 'DECOUPLING PLCOP))))
           (PROG ()
            WHILELABEL
             (COND ((NOT PROC_LIST_) (RETURN NIL)))
             (PROGN
              (COND
               ((FREEOF PLCOP (CAR PROC_LIST_))
                (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))))
              (SETQ PROC_LIST_ (CDR PROC_LIST_)))
             (GO WHILELABEL))
           (SETQ PROC_LIST_ (REVERSE PLCOP))
           (COND
            (PRINT_MORE
             (PROGN
              (PROGN (PRIN2 "proc_list_ has been automatically updated.") NIL)
              (TERPRI)
              NIL))))))
        (SETQ ARGLIST NIL))))) 
(PUT 'TRY_OTHER_ORDERING 'NUMBER-OF-ARGS 1) 
(PUT 'TRY_OTHER_ORDERING 'DEFINED-ON-LINE '2665) 
(PUT 'TRY_OTHER_ORDERING 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TRY_OTHER_ORDERING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRY_OTHER_ORDERING (ARGLIST)
    (PROG (PLCOP PDES S)
      (SETQ PDES (CAR ARGLIST))
      (COND ((OR (NULL PDES) (NULL (CDR PDES))) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND PROC_LIST_ (NEQ (CAR PROC_LIST_) 'TRY_OTHER_ORDERING)))
          (RETURN NIL)))
        (PROGN
         (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
         (SETQ PROC_LIST_ (CDR PROC_LIST_)))
        (GO WHILELABEL))
      (COND
       (PROC_LIST_
        (PROGN
         (COND
          ((NOT LEX_DF)
           (PROGN
            (SETQ LEX_DF T)
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 "From now on lexicographic ordering of derivatives.")
                NIL))))
            (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
            NIL))
          (T
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2
                 "The current variable ordering is going to be reversed.")
                NIL))))
            (SETQ VL_ (REVERSE VL_))
            (PROG (S)
              (SETQ S PDES)
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S) (PUT S 'VARS (SORT_ACCORDING_TO (GET S 'VARS) VL_)))
               (CAR S))
              (SETQ S (CDR S))
              (GO LAB))
            NIL)))
         (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL_))
         (SETQ PROC_LIST_ (CDR PROC_LIST_))
         NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT PROC_LIST_) (RETURN NIL)))
        (PROGN
         (SETQ PLCOP (CONS (CAR PROC_LIST_) PLCOP))
         (SETQ PROC_LIST_ (CDR PROC_LIST_)))
        (GO WHILELABEL))
      (SETQ PROC_LIST_ (REVERSE PLCOP))
      (RETURN (CONS PDES (CDR ARGLIST))))) 
(PUT 'SOLVABLE_CASE 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVABLE_CASE 'DEFINED-ON-LINE '2708) 
(PUT 'SOLVABLE_CASE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SOLVABLE_CASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVABLE_CASE (P F CASE)
    (PROG (FE)
      (SETQ FE (GET P CASE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FE (NEQ (CDAR FE) F))) (RETURN NIL)))
        (SETQ FE (CDR FE))
        (GO WHILELABEL))
      (RETURN FE))) 
(PUT 'ADD2FLIN 'NUMBER-OF-ARGS 2) 
(PUT 'ADD2FLIN 'DEFINED-ON-LINE '2725) 
(PUT 'ADD2FLIN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD2FLIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD2FLIN (PDES F)
    (PROG (PCP NONLI H P FL F0)
      (PROG ()
       WHILELABEL
        (COND ((NOT PDES) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR PDES))
         (COND ((FREEOF (GET P 'FCTS) F) (SETQ PDES (CDR PDES)))
               (T
                (PROGN
                 (SETQ PCP (CONS P PCP))
                 (SETQ H (GET P 'DERIVS))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT (AND H (OR (EQUAL (CDAR H) 1) (NEQ (CAAAR H) F))))
                     (RETURN NIL)))
                   (SETQ H (CDR H))
                   (GO WHILELABEL))
                 (COND (H (PROGN (SETQ NONLI T) (SETQ PDES NIL)))
                       (T (SETQ PDES (CDR PDES))))))))
        (GO WHILELABEL))
      (COND
       ((AND (NULL NONLI) FLIN_)
        (PROGN
         (SETQ F0
                 (PROG (FL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FL FLIN_)
                   (COND ((NULL FL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (FL) (CONS FL 0)) (CAR FL))
                                         NIL)))
                  LOOPLABEL
                   (SETQ FL (CDR FL))
                   (COND ((NULL FL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (FL) (CONS FL 0)) (CAR FL)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT PCP) (RETURN NIL)))
           (COND
            ((NOT (FREEOF (CDR (GET (CAR PCP) 'SQVAL)) F))
             (PROGN (SETQ NONLI T) (SETQ PCP NIL)))
            (T
             (PROGN
              (SETQ H
                      (ADDSQ (GET (CAR PCP) 'SQVAL)
                             (NEGSQ (SUBSQ (GET (CAR PCP) 'SQVAL) F0))))
              (COND ((NOT (FREEOF H F)) (PROGN (SETQ NONLI T) (SETQ PCP NIL)))
                    (T (SETQ PCP (CDR PCP)))))))
           (GO WHILELABEL)))))
      (COND
       ((NULL NONLI) (SETQ FLIN_ (SORT_ACCORDING_TO (CONS F FLIN_) FTEM_))))
      (RETURN (NULL NONLI)))) 
(PUT 'LIN_CHECK_SQ 'NUMBER-OF-ARGS 2) 
(PUT 'LIN_CHECK_SQ 'DEFINED-ON-LINE '2764) 
(PUT 'LIN_CHECK_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIN_CHECK_SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIN_CHECK_SQ (SQVAL FL)
    (COND ((AND (NEQ (CDR SQVAL) 1) (NOT (FREEOFLIST SQVAL FL))) NIL)
          (T
           (PROG (K F NU SB)
             (SETQ K (SETKORDER (LIST LIN_TEST_CONST)))
             (SETQ SB
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F FL)
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (F)
                                           (CONS F
                                                 (LIST 'TIMES LIN_TEST_CONST
                                                       F)))
                                         (CAR F))
                                        NIL)))
                      LOOPLABEL
                       (SETQ F (CDR F))
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (F)
                                   (CONS F (LIST 'TIMES LIN_TEST_CONST F)))
                                 (CAR F))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ NU (CAR (SUBF (CAR SQVAL) SB)))
             (SETKORDER K)
             (RETURN
              (COND
               ((OR (OR (ATOM NU) (ATOM (CAR NU)))
                    (NEQ LIN_TEST_CONST (CAAAR NU)) (GREATERP 2 (CDAAR NU)))
                T)
               (T NIL))))))) 
(PUT 'LIN_CHECK 'NUMBER-OF-ARGS 2) 
(PUT 'LIN_CHECK 'DEFINED-ON-LINE '2779) 
(PUT 'LIN_CHECK 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIN_CHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIN_CHECK (PDE FL)
    (PROG (INHOM F)
      (SETQ INHOM PDE)
      (PROG (F)
        (SETQ F FL)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (SETQ INHOM (ERR_CATCH_SUB F 0 INHOM))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN
       (PROGN
        (PROG (F)
          (SETQ F FL)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F) (SETQ PDE (SUBST (LIST 'TIMES LIN_TEST_CONST F) F PDE)))
           (CAR F))
          (SETQ F (CDR F))
          (GO LAB))
        (FREEOF
         (REVAL1 (LIST 'QUOTIENT (LIST 'DIFFERENCE PDE INHOM) LIN_TEST_CONST)
                 T)
         LIN_TEST_CONST))))) 
(PUT 'SYMBOL_EXPLANATION 'NUMBER-OF-ARGS 0) 
(PUT 'SYMBOL_EXPLANATION 'DEFINED-ON-LINE '2791) 
(PUT 'SYMBOL_EXPLANATION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SYMBOL_EXPLANATION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SYMBOL_EXPLANATION NIL
    (PROGN
     (PROGN
      (PRIN2
       "+------------------------------------------------------------------------------+")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "|CHARACTERIZING FUNCTIONS:                                                     |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "|flin_: The function occurs linear and is element of the global list flin_.    |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "|fhom_: The function is one of a set of homogeneously occuring functions fhom_.|")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| <>0 : The function is known to be non-zero, i.e. it is an element of ineq_.  |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| n2l : The function is not linearly occuring but the equation involves        |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "|       linearly occuring functions.                                           |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "|CHARACTERIZING SUBSTITUTIONS:                                                 |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| (+) : a favourable substitution                                              |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| (-) : an unfavourable substitution                                           |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| const coeff     : substitution generates no cases                            |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| no cases        : no cases but coefficient involves unknowns                 |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "| case generating : substitution generates cases                               |")
      NIL)
     (TERPRI)
     (PROGN
      (PRIN2
       "+------------------------------------------------------------------------------+")
      NIL)
     (TERPRI)
     NIL)) 
(PUT 'LIST_ALL_SUBS 'NUMBER-OF-ARGS 3) 
(PUT 'LIST_ALL_SUBS 'DEFINED-ON-LINE '2808) 
(PUT 'LIST_ALL_SUBS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_ALL_SUBS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIST_ALL_SUBS (TXT SL S)
    (COND
     (SL
      (PROG (H)
        (PROGN (PRIN2 TXT) (PRIN2 ": ") NIL)
        (TERPRI)
        (PROG ()
         WHILELABEL
          (COND ((NOT SL) (RETURN NIL)))
          (PROGN
           (PROGN (PRIN2 (CDAR SL)) (PRIN2 " :") NIL)
           (COND ((MEMBER (CDAR SL) FLIN_) (PROGN (PRIN2 " flin_(+)") NIL))
                 ((NOT (FREEOFLIST FLIN_ (GET S 'FCTS)))
                  (PROGN (PRIN2 " n2l(-)") NIL)))
           (COND ((MEMBER (CDAR SL) FHOM_) (PROGN (PRIN2 " fhom_(+)") NIL)))
           (COND
            ((MEMBER (SIMP (CDAR SL)) INEQ_) (PROGN (PRIN2 " <>0(-)") NIL)))
           (COND
            ((GREATERP
              (SETQ H
                      (PLUS (DELENGTHSF (CAR (CAAR SL)))
                            (DELENGTHSF (CDR (CAAR SL)))))
              PRINT_)
             (PROGN
              (PRIN2 " coeff: (print length = ")
              (PRIN2 H)
              (PRIN2 ")")
              NIL))
            (T (PROGN (PRIN2 " coeff: ") (PRIN2 (PREPSQ (CAAR SL))) NIL)))
           (TERPRI)
           (SETQ SL (CDR SL)))
          (GO WHILELABEL)))))) 
(PUT 'LIST_POSSIBLE_SUBS 'NUMBER-OF-ARGS 1) 
(PUT 'LIST_POSSIBLE_SUBS 'DEFINED-ON-LINE '2827) 
(PUT 'LIST_POSSIBLE_SUBS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_POSSIBLE_SUBS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIST_POSSIBLE_SUBS (S)
    (PROG ()
      (FCTEVAL S)
      (TERPRI)
      (LIST_ALL_SUBS "const coeff substitutions" (GET S 'FCTEVAL_LIN) S)
      (LIST_ALL_SUBS "no cases substitutions" (GET S 'FCTEVAL_NCA) S)
      (LIST_ALL_SUBS "case generating substitutions" (GET S 'FCTEVAL_NLI) S))) 
(PUT 'PLOT_NON0_SEPARANTS 'NUMBER-OF-ARGS 1) 
(PUT 'PLOT_NON0_SEPARANTS 'DEFINED-ON-LINE '2836) 
(PUT 'PLOT_NON0_SEPARANTS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLOT_NON0_SEPARANTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLOT_NON0_SEPARANTS (S)
    (PROG (DV DL DLC DR FDL AVF UR)
      (COND
       ((AND (NEQ USERRULES_ (LIST 'LIST))
             (ZEROP
              (REVAL1
               (LIST 'DIFFERENCE (CAR (CDADR USERRULES_))
                     (CADR (CDADR USERRULES_)))
               T)))
        (PROGN (SETQ UR T) (AEVAL (CLEARRULES (LIST USERRULES_))))))
      (SETQ DV (GET S 'DERIVS))
      (SETQ AVF (GET S 'ALLVARFCTS))
      (PROG ()
       WHILELABEL
        (COND ((NOT DV) (RETURN NIL)))
        (PROGN
         (SETQ DR (CAAR DV))
         (SETQ DV (CDR DV))
         (COND
          ((MEMBER (CAR DR) AVF)
           (PROGN
            (SETQ DLC DL)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND DLC
                      (OR (NEQ (CAAR DLC) (CAR DR))
                          (WHICH_DERIV (CAR DLC) DR))))
                (RETURN NIL)))
              (SETQ DLC (CDR DLC))
              (GO WHILELABEL))
            (COND ((NULL DLC) (SETQ DL (CONS DR DL))))
            NIL))))
        (GO WHILELABEL))
      (PROG (DR)
        (SETQ DR DL)
       LAB
        (COND ((NULL DR) (RETURN NIL)))
        ((LAMBDA (DR)
           (PROGN
            (SETQ DR (COND ((NULL (CDR DR)) (CAR DR)) (T (CONS 'DF DR))))
            (SETQ DR (CAAAR (CAR (MKSQ DR 1))))
            (COND
             ((OR (GET S 'LINEAR_)
                  (CAN_NOT_BECOME_ZEROSQ (DIFFSQ (GET S 'SQVAL) DR)
                   (GET S 'FCTS)))
              (SETQ FDL (CONS DR FDL))))))
         (CAR DR))
        (SETQ DR (CDR DR))
        (GO LAB))
      (TERPRI)
      (COND
       (FDL
        (PROGN
         (PROGN (PRIN2 "Leading derivatives with non-zero separant: ") NIL)
         (PROGN (PRIN2 (CDR (REVAL1 (CONS 'LIST FDL) T))) NIL)
         NIL))
       (T
        (PROGN (PRIN2 "No leading derivative with non-zero separant. ") NIL)))
      (COND (UR (AEVAL (LET (LIST USERRULES_))))))) 
(PUT 'RULE_FROM_PDE 'NUMBER-OF-ARGS 1) 
(PUT 'RULE_FROM_PDE 'DEFINED-ON-LINE '2880) 
(PUT 'RULE_FROM_PDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RULE_FROM_PDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RULE_FROM_PDE (S)
    (PROG (DV DL DLC DR FDL AVF L)
      (SETQ DV (GET S 'DERIVS))
      (SETQ AVF (GET S 'ALLVARFCTS))
      (PROG ()
       WHILELABEL
        (COND ((NOT DV) (RETURN NIL)))
        (PROGN
         (SETQ DR (CAAR DV))
         (COND
          ((MEMBER (CAR DR) AVF)
           (PROGN
            (SETQ DLC DL)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND DLC
                      (OR (NEQ (CAAAR DLC) (CAR DR))
                          (WHICH_DERIV (CAAR DLC) DR))))
                (RETURN NIL)))
              (SETQ DLC (CDR DLC))
              (GO WHILELABEL))
            (COND ((NULL DLC) (SETQ DL (CONS (CAR DV) DL))))
            NIL)))
         (SETQ DV (CDR DV)))
        (GO WHILELABEL))
      (PROG (DV)
        (SETQ DV DL)
       LAB
        (COND ((NULL DV) (RETURN NIL)))
        ((LAMBDA (DV)
           (PROGN
            (SETQ DR
                    (COND ((NULL (CDAR DV)) (CAAR DV))
                          (T (CONS 'DF (CAR DV)))))
            (SETQ DR (CAAAR (CAR (MKSQ DR 1))))
            (COND
             ((OR (GET S 'LINEAR_)
                  (CAN_NOT_BECOME_ZEROSQ
                   (PROGN
                    (SETQ L (COEFFN (MK*SQ (GET S 'SQVAL)) DR (CDR DV)))
                    (COND ((PAIRP L) (CADR L)) (T (SIMP L))))
                   (GET S 'FCTS)))
              (COND ((EQUAL (CDR DV) 1) (SETQ FDL (CONS DR FDL)))
                    (T (SETQ FDL (CONS (LIST 'EXPT DR (CDR DV)) FDL))))))))
         (CAR DV))
        (SETQ DV (CDR DV))
        (GO LAB))
      (COND
       ((NULL FDL)
        (PROGN
         (PROGN
          (PRIN2 "No leading derivative has a non-zero coefficient.")
          NIL)
         (TERPRI)))
       ((CDR FDL)
        (PROGN
         (PROGN (PRIN2 "Which term shall be substituted by the rule?") NIL)
         (TERPRI)
         (MATHPRINT (CONS BLANK FDL))
         (PROGN (PRIN2 "Input its number + Enter: ") NIL)
         (SETQ L (TERMREAD))
         (COND
          ((NOT (FIXP L))
           (PROGN
            (SETQ FDL NIL)
            (PROGN (PRIN2 "Input is not a number!") NIL)
            (TERPRI)))
          ((GREATERP L (LENGTH FDL))
           (PROGN
            (SETQ FDL NIL)
            (PROGN (PRIN2 "This number is too big.") NIL)
            (TERPRI)))
          (T (SETQ FDL (LIST (NTH FDL L))))))))
      (COND
       (FDL
        (PROGN
         (SETQ L (GET S 'SQVAL))
         (COND
          ((AND (PAIRP (CAR FDL)) (EQUAL (CAAR FDL) 'EXPT))
           (SETQ DV (COEFFN (LIST '*SQ L T) (CADAR FDL) (CADDAR FDL))))
          (T (SETQ DV (COEFFN (LIST '*SQ L T) (CAR FDL) 1))))
         (SETQ DV (COND ((PAIRP DV) (CADR DV)) (T (SIMP DV))))
         (SETQ USERRULES_
                 (CONS 'LIST
                       (CONS
                        (LIST 'REPLACEBY (CAR FDL)
                              (LIST '*SQ
                                    (MULTSQ
                                     (ADDSQ (MULTSQ (SIMP (CAR FDL)) DV)
                                            (NEGSQ L))
                                     (INVSQ DV))
                                    T))
                        (CDR USERRULES_))))
         (PROGN
          (ASSGNPRI (AEVAL "The new list of user defined rules: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL USERRULES_) NIL 'LAST))
         (TERPRI)))))) 
(PUT 'HOW_OFTEN 'NUMBER-OF-ARGS 1) 
(PUT 'HOW_OFTEN 'DEFINED-ON-LINE '2951) 
(PUT 'HOW_OFTEN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'HOW_OFTEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HOW_OFTEN (PDES)
    (PROG (F N EQUN)
      (SETQ BACKUP_ NIL)
      (PROG (F)
        (SETQ F FTEM_)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ N 0)
            (PROG (P)
              (SETQ P PDES)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P)
                 (SETQ N
                         (PLUS N
                               (DIFFERENCE (GET P 'TERMS)
                                           (NO_OF_TM_SF
                                            (CAR
                                             (SUBF (CAR (GET P 'SQVAL))
                                                   (LIST (CONS F 0)))))))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ BACKUP_ (CONS (CONS N F) BACKUP_))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ BACKUP_ (REV_IDX_SORT BACKUP_))
      (PROG (F)
        (SETQ F FTEM_)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ N 0)
            (PROG (P)
              (SETQ P PDES)
             LAB
              (COND ((NULL P) (RETURN NIL)))
              ((LAMBDA (P) (COND ((MEMBER F (GET P 'FCTS)) (SETQ N (ADD1 N)))))
               (CAR P))
              (SETQ P (CDR P))
              (GO LAB))
            (SETQ EQUN (CONS (CONS N F) EQUN))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ EQUN (REV_IDX_SORT EQUN))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2
           "Total number of occurences of all unknowns in all equations:")
          NIL)
         (TERPRI)
         (PRETTYPRINT BACKUP_)
         (PROGN
          (PRIN2 "Total number of equations in which unknowns occur:")
          NIL)
         (TERPRI)
         (PRETTYPRINT EQUN)
         NIL)))
      (RETURN BACKUP_))) 
(PUT 'CASE_ON_MOST_FREQU_FNC 'NUMBER-OF-ARGS 1) 
(PUT 'CASE_ON_MOST_FREQU_FNC 'DEFINED-ON-LINE '2984) 
(PUT 'CASE_ON_MOST_FREQU_FNC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CASE_ON_MOST_FREQU_FNC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASE_ON_MOST_FREQU_FNC (ARGLIST)
    (PROG (H)
      (SETQ H (HOW_OFTEN (CAR ARGLIST)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND H (MEMBER (SIMP (CDAR H)) INEQ_) (NOT (ZEROP (CAAR H)))))
          (RETURN NIL)))
        (SETQ H (CDR H))
        (GO WHILELABEL))
      (RETURN
       (COND
        ((AND H (NOT (ZEROP (CAAR H))))
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CDAR H))))
        (T NIL))))) 
(PUT 'PRE_DETERMINED_CASE_SPLITS 'NUMBER-OF-ARGS 1) 
(PUT 'PRE_DETERMINED_CASE_SPLITS 'DEFINED-ON-LINE '2994) 
(PUT 'PRE_DETERMINED_CASE_SPLITS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRE_DETERMINED_CASE_SPLITS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRE_DETERMINED_CASE_SPLITS (ARGLIST)
    (PROG (H CARH)
      (SETQ H (CDR CASE_LIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROGN
         (SETQ CARH (SIMP (CAR H)))
         (COND
          ((OR (FREEOFLIST CARH FTEM_) (MEMBER CARH INEQ_))
           (PROGN (SETQ CARH NIL) (SETQ H (CDR H))))
          (T (SETQ H NIL)))
         (SETQ CASE_LIST (CONS 'LIST (CDDR CASE_LIST))))
        (GO WHILELABEL))
      (RETURN
       (COND
        (CARH
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) CARH)))
        (T NIL))))) 
(PUT 'FTEM_SORTED_BY_INDEX 'NUMBER-OF-ARGS 0) 
(PUT 'FTEM_SORTED_BY_INDEX 'DEFINED-ON-LINE '3008) 
(PUT 'FTEM_SORTED_BY_INDEX 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FTEM_SORTED_BY_INDEX 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FTEM_SORTED_BY_INDEX NIL
    (PROG (H H1 H2 H3 H4 H5)
      (COND
       (FLIN_
        (PROG ()
         WHILELABEL
          (COND ((NOT BACKUP_) (RETURN NIL)))
          (PROGN
           (COND
            ((MEMBER (CDAR BACKUP_) FLIN_) (SETQ H1 (CONS (CAR BACKUP_) H1)))
            (T (SETQ H2 (CONS (CAR BACKUP_) H2))))
           (SETQ BACKUP_ (CDR BACKUP_)))
          (GO WHILELABEL)))
       (T
        (PROG ()
         WHILELABEL
          (COND ((NOT BACKUP_) (RETURN NIL)))
          (PROGN
           (SETQ H1 (CONS (CAR BACKUP_) H1))
           (SETQ BACKUP_ (CDR BACKUP_)))
          (GO WHILELABEL))))
      (PROG (H3)
        (SETQ H3 INEQ_)
       LAB
        (COND ((NULL H3) (RETURN NIL)))
        ((LAMBDA (H3)
           (COND
            ((AND (ATOM H3) (MEMBER H3 FTEM_))
             (COND
              ((MEMBER H3 FLIN_)
               (PROGN
                (SETQ H H1)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND H (NEQ (CDAR H) H3))) (RETURN NIL)))
                  (SETQ H (CDR H))
                  (GO WHILELABEL))
                (COND
                 (H
                  (PROGN
                   (SETQ H1 (DELETE (CAR H) H1))
                   (SETQ H4 (CONS (CAR H) H4)))))))
              (T
               (PROGN
                (SETQ H H2)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND H (NEQ (CDAR H) H3))) (RETURN NIL)))
                  (SETQ H (CDR H))
                  (GO WHILELABEL))
                (COND
                 (H
                  (PROGN
                   (SETQ H2 (DELETE (CAR H) H2))
                   (SETQ H5 (CONS (CAR H) H5)))))))))))
         (CAR H3))
        (SETQ H3 (CDR H3))
        (GO LAB))
      (SETQ H3 (APPEND (APPEND H1 (IDX_SORT H4)) (APPEND H2 (IDX_SORT H5))))
      (RETURN
       (PROG (H FORALL-RESULT FORALL-ENDPTR)
         (SETQ H H3)
         (COND ((NULL H) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL)))
        LOOPLABEL
         (SETQ H (CDR H))
         (COND ((NULL H) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (H) (CDR H)) (CAR H)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MAXMUM 'NUMBER-OF-ARGS 2) 
(PUT 'MAXMUM 'DEFINED-ON-LINE '3044) 
(PUT 'MAXMUM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MAXMUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXMUM (A B) (COND ((GREATERP A B) A) (T B))) 
(PUT 'DEGREE_SF 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREE_SF 'DEFINED-ON-LINE '3047) 
(PUT 'DEGREE_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DEGREE_SF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREE_SF (SF F)
    (COND ((NULL (PAIRP SF)) 0) ((EQUAL F (CAAAR SF)) (CDAAR SF))
          (T (MAXMUM (DEGREE_SF (CDAR SF) F) (DEGREE_SF (CDR SF) F))))) 
(PUT 'LISTREL 'NUMBER-OF-ARGS 3) 
(PUT 'LISTREL 'DEFINED-ON-LINE '3055) 
(PUT 'LISTREL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LISTREL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LISTREL (A B L) (MEMBER B (MEMBER A L))) 
(PUT 'ABS_DFREL 'NUMBER-OF-ARGS 3) 
(PUT 'ABS_DFREL 'DEFINED-ON-LINE '3059) 
(PUT 'ABS_DFREL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ABS_DFREL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ABS_DFREL (P Q VL)
    (PROG (A)
      (RETURN
       (COND (LEX_DF (DFREL2 P Q VL))
             ((ZEROP
               (SETQ A (DIFFERENCE (ABSODEG (CDAR P)) (ABSODEG (CDAR Q)))))
              (DFREL2 P Q VL))
             (T (LESSP A 0)))))) 
(PUT 'MULT_DERIVS 'NUMBER-OF-ARGS 2) 
(PUT 'MULT_DERIVS 'DEFINED-ON-LINE '3073) 
(PUT 'MULT_DERIVS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MULT_DERIVS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULT_DERIVS (A B)
    (PROG (L)
      (RETURN
       (COND ((NOT B) A) ((NOT A) B)
             (T
              (PROGN
               (PROG (S)
                 (SETQ S A)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PROG (R)
                      (SETQ R B)
                     LAB
                      (COND ((NULL R) (RETURN NIL)))
                      ((LAMBDA (R)
                         (COND
                          ((EQUAL (CAR S) (CAR R))
                           (SETQ L
                                   (UNION
                                    (LIST
                                     (CONS (CAR R) (PLUS (CDR R) (CDR S))))
                                    L)))
                          (T (SETQ L (UNION (LIST R S) L)))))
                       (CAR R))
                      (SETQ R (CDR R))
                      (GO LAB)))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))
               L)))))) 
(PUT 'ALL_POWER_SEARCH_SF 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_POWER_SEARCH_SF 'DEFINED-ON-LINE '3109) 
(PUT 'ALL_POWER_SEARCH_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ALL_POWER_SEARCH_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_POWER_SEARCH_SF (P)
    (COND
     ((AND (PAIRP P) (PAIRP (CAR P)) (NOT (OR (ATOM P) (ATOM (CAR P)))))
      (PROG (A B LCP)
        (SETQ A (CAAAR P))
        (SETQ LCP (ALL_POWER_SEARCH_SF (CDAR P)))
        (SETQ B
                (COND ((ATOM A) (CONS (LIST A) (CDAAR P)))
                      ((AND (PAIRP A) (EQUAL (CAR A) 'DF))
                       (CONS (CDR A) (CDAAR P)))))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT
             (AND (PAIRP (CDR P)) (PAIRP (CAR (CDR P)))
                  (NOT (OR (ATOM (CDR P)) (ATOM (CAR (CDR P)))))))
            (RETURN NIL)))
          (COND
           ((EQ A (CAAAR (CDR P)))
            (PROGN
             (COND (B (SETQ LCP (CONS (CONS (CAR B) (CDAAR (CDR P))) LCP))))
             (SETQ LCP (UNION (ALL_POWER_SEARCH_SF (CDAR (CDR P))) LCP))
             (SETQ P (CDR P))))
           (T
            (PROGN
             (SETQ LCP (UNION (ALL_POWER_SEARCH_SF (CDR P)) LCP))
             (SETQ P (LIST (CONS NIL NIL))))))
          (GO WHILELABEL))
        (RETURN (COND (B (CONS B LCP)) (T LCP))))))) 
(PUT 'ALL_DERIV_SEARCH_SF 'NUMBER-OF-ARGS 2) 
(PUT 'ALL_DERIV_SEARCH_SF 'DEFINED-ON-LINE '3126) 
(PUT 'ALL_DERIV_SEARCH_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ALL_DERIV_SEARCH_SF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALL_DERIV_SEARCH_SF (P FTEM)
    (PROG (H AD)
      (PROG (H)
        (SETQ H (ALL_POWER_SEARCH_SF P))
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H) (COND ((MEMBER (CAAR H) FTEM) (SETQ AD (CONS H AD)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN AD))) 
(PUT 'ALL_DERIV_SEARCH 'NUMBER-OF-ARGS 2) 
(PUT 'ALL_DERIV_SEARCH 'DEFINED-ON-LINE '3133) 
(PUT 'ALL_DERIV_SEARCH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ALL_DERIV_SEARCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALL_DERIV_SEARCH (P FTEM)
    (PROG (A)
      (COND
       ((NOT (PAIRP P))
        (PROGN (COND ((MEMBER P FTEM) (SETQ A (LIST (CONS (LIST P) 1)))))))
       (T
        (PROGN
         (COND
          ((MEMBER (CAR P) '(PLUS QUOTIENT EQUAL))
           (PROG (Q)
             (SETQ Q (CDR P))
            LAB
             (COND ((NULL Q) (RETURN NIL)))
             ((LAMBDA (Q) (SETQ A (UNION (ALL_DERIV_SEARCH Q FTEM) A)))
              (CAR Q))
             (SETQ Q (CDR Q))
             (GO LAB)))
          ((EQUAL (CAR P) 'MINUS) (SETQ A (ALL_DERIV_SEARCH (CADR P) FTEM)))
          ((EQUAL (CAR P) 'TIMES)
           (PROG (Q)
             (SETQ Q (CDR P))
            LAB
             (COND ((NULL Q) (RETURN NIL)))
             ((LAMBDA (Q) (SETQ A (MULT_DERIVS (ALL_DERIV_SEARCH Q FTEM) A)))
              (CAR Q))
             (SETQ Q (CDR Q))
             (GO LAB)))
          ((AND (EQUAL (CAR P) 'EXPT) (NUMBERP (CADDR P)))
           (PROG (B)
             (SETQ B (ALL_DERIV_SEARCH (CADR P) FTEM))
            LAB
             (COND ((NULL B) (RETURN NIL)))
             ((LAMBDA (B)
                (PROGN
                 (COND
                  ((NUMBERP (CDR B))
                   (SETQ A
                           (CONS (CONS (CAR B) (TIMES (CADDR P) (CDR B)))
                                 A))))))
              (CAR B))
             (SETQ B (CDR B))
             (GO LAB)))
          ((AND (EQUAL (CAR P) 'DF) (MEMBER (CADR P) FTEM))
           (SETQ A (LIST (CONS (CDR P) 1))))))))
      (RETURN A))) 
(PUT 'ABS_LD_DERIV 'NUMBER-OF-ARGS 1) 
(PUT 'ABS_LD_DERIV 'DEFINED-ON-LINE '3153) 
(PUT 'ABS_LD_DERIV 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ABS_LD_DERIV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ABS_LD_DERIV (P)
    (COND ((GET P 'DERIVS) (REVAL1 (CONS 'DF (CAAR (GET P 'DERIVS))) T)))) 
(PUT 'ABS_LD_DERIV_POW 'NUMBER-OF-ARGS 1) 
(PUT 'ABS_LD_DERIV_POW 'DEFINED-ON-LINE '3156) 
(PUT 'ABS_LD_DERIV_POW 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ABS_LD_DERIV_POW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ABS_LD_DERIV_POW (P) (COND ((GET P 'DERIVS) (CDAR (GET P 'DERIVS))) (T 0))) 
(PUT 'WHICH_FIRST 'NUMBER-OF-ARGS 3) 
(PUT 'WHICH_FIRST 'DEFINED-ON-LINE '3160) 
(PUT 'WHICH_FIRST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WHICH_FIRST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WHICH_FIRST (A B L)
    (COND ((NULL L) NIL) ((EQUAL A (CAR L)) A) ((EQUAL B (CAR L)) B)
          (T (WHICH_FIRST A B (CDR L))))) 
(PUT 'TOTAL_LESS_DFREL 'NUMBER-OF-ARGS 4) 
(PUT 'TOTAL_LESS_DFREL 'DEFINED-ON-LINE '3166) 
(PUT 'TOTAL_LESS_DFREL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TOTAL_LESS_DFREL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TOTAL_LESS_DFREL (A B FTEM VL)
    (PROG (FA AD AL BL)
      (SETQ FA (CAAR A))
      (RETURN
       (COND ((EQUAL A B) 0)
             (LEX_FC
              (COND
               ((EQUAL FA (CAAR B))
                (COND
                 ((EQUAL (SETQ AD (ABS_DFREL A B VL)) 0)
                  (COND ((LESSP (CDR A) (CDR B)) T) (T NIL)))
                 (AD T) (T NIL)))
               ((EQUAL FA (WHICH_FIRST FA (CAAR B) FTEM)) NIL) (T T)))
             ((GREATERP (SETQ AL (FCTLENGTH FA))
                        (SETQ BL (FCTLENGTH (CAAR B))))
              NIL)
             ((GREATERP BL AL) T)
             ((EQUAL (SETQ AD (ABS_DFREL A B VL)) 0)
              (COND
               ((EQUAL FA (CAAR B)) (COND ((LESSP (CDR A) (CDR B)) T) (T NIL)))
               ((EQUAL FA (WHICH_FIRST FA (CAAR B) FTEM)) NIL) (T T)))
             (AD T) (T NIL))))) 
(PUT 'SORT_DERIVS 'NUMBER-OF-ARGS 3) 
(PUT 'SORT_DERIVS 'DEFINED-ON-LINE '3201) 
(PUT 'SORT_DERIVS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SORT_DERIVS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SORT_DERIVS (L FTEM VL)
    (PROG (L1 L2 A)
      (RETURN
       (COND ((NULL L) NIL)
             (T
              (PROGN
               (SETQ A (CAR L))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((NEQ A (CAR L))
                    (COND
                     ((TOTAL_LESS_DFREL A (CAR L) FTEM VL)
                      (SETQ L1 (CONS (CAR L) L1)))
                     (T (SETQ L2 (CONS (CAR L) L2))))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (SORT_DERIVS L1 FTEM VL)
                       (CONS A (SORT_DERIVS L2 FTEM VL))))))))) 
(PUT 'DFMAX 'NUMBER-OF-ARGS 3) 
(PUT 'DFMAX 'DEFINED-ON-LINE '3218) 
(PUT 'DFMAX 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFMAX (P Q VL) (COND ((DFREL P Q VL) Q) (T P))) 
(PUT 'DFREL 'NUMBER-OF-ARGS 3) 
(PUT 'DFREL 'DEFINED-ON-LINE '3225) 
(PUT 'DFREL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFREL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFREL (P Q VL)
    (COND (LEX_DF (DFREL1 P Q VL))
          (T
           (PROG (A)
             (RETURN
              (COND
               ((ZEROP
                 (SETQ A (DIFFERENCE (ABSODEG (CAR P)) (ABSODEG (CAR Q)))))
                (DFREL1 P Q VL))
               ((LESSP A 0) T) (T NIL))))))) 
(PUT 'DIFFRELP 'NUMBER-OF-ARGS 3) 
(PUT 'DIFFRELP 'DEFINED-ON-LINE '3236) 
(PUT 'DIFFRELP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DIFFRELP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFFRELP (P Q V)
    (COND ((EQUAL (CDR P) 'INFINITY) NIL) ((EQUAL (CDR Q) 'INFINITY) T)
          (T (DFREL P Q V)))) 
(PUT 'DFREL1 'NUMBER-OF-ARGS 3) 
(PUT 'DFREL1 'DEFINED-ON-LINE '3249) 
(PUT 'DFREL1 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFREL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFREL1 (P Q V)
    (COND
     ((NULL V)
      (COND ((EQUAL (CDR P) 'INFINITY) NIL) ((EQUAL (CDR Q) 'INFINITY) T)
            ((GREATERP (CDR P) (CDR Q)) NIL) ((LESSP (CDR P) (CDR Q)) T)
            (T 0)))
     (T
      (PROG (A B)
        (SETQ A (DFDEG (CAR P) (CAR V)))
        (SETQ B (DFDEG (CAR Q) (CAR V)))
        (RETURN
         (COND ((LESSP A B) T) ((LESSP B A) NIL) (T (DFREL1 P Q (CDR V))))))))) 
(PUT 'DFREL2 'NUMBER-OF-ARGS 3) 
(PUT 'DFREL2 'DEFINED-ON-LINE '3267) 
(PUT 'DFREL2 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFREL2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DFREL2 (P Q V)
    (COND ((NULL V) 0)
          (T
           (PROG (A B)
             (SETQ A (DFDEG (CAR P) (CAR V)))
             (SETQ B (DFDEG (CAR Q) (CAR V)))
             (RETURN
              (COND ((LESSP A B) T) ((LESSP B A) NIL)
                    (T (DFREL2 P Q (CDR V))))))))) 
(PUT 'ABSODEG 'NUMBER-OF-ARGS 1) 
(PUT 'ABSODEG 'DEFINED-ON-LINE '3279) 
(PUT 'ABSODEG 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ABSODEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ABSODEG (P)
    (COND ((NOT (PAIRP P)) 0)
          (T
           (EVAL
            (CONS 'PLUS
                  (PROG (V FORALL-RESULT FORALL-ENDPTR)
                    (SETQ V P)
                    (COND ((NULL V) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (V)
                                        (COND ((FIXP V) (SUB1 V)) (T 1)))
                                      (CAR V))
                                     NIL)))
                   LOOPLABEL
                    (SETQ V (CDR V))
                    (COND ((NULL V) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (V) (COND ((FIXP V) (SUB1 V)) (T 1)))
                              (CAR V))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))) 
(PUT 'MAXDERIVS 'NUMBER-OF-ARGS 3) 
(PUT 'MAXDERIVS 'DEFINED-ON-LINE '3284) 
(PUT 'MAXDERIVS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MAXDERIVS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAXDERIVS (NUMBERLIST DERIV VARLIST)
    (COND
     ((NULL NUMBERLIST)
      (PROG (V FORALL-RESULT FORALL-ENDPTR)
        (SETQ V VARLIST)
        (COND ((NULL V) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (DFDEG DERIV V)) (CAR V)) NIL)))
       LOOPLABEL
        (SETQ V (CDR V))
        (COND ((NULL V) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (V) (DFDEG DERIV V)) (CAR V)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (PROG (L)
        (PROG (V)
          (SETQ V VARLIST)
         LAB
          (COND ((NULL V) (RETURN NIL)))
          ((LAMBDA (V)
             (PROGN
              (SETQ L (CONS (MAX (CAR NUMBERLIST) (DFDEG DERIV V)) L))
              (SETQ NUMBERLIST (CDR NUMBERLIST))))
           (CAR V))
          (SETQ V (CDR V))
          (GO LAB))
        (RETURN (REVERSE L)))))) 
(PUT 'DFDEG 'NUMBER-OF-ARGS 2) 
(PUT 'DFDEG 'DEFINED-ON-LINE '3294) 
(PUT 'DFDEG 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DFDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DFDEG (P V)
    (COND ((NULL (SETQ P (MEMBER V P))) 0)
          ((OR (NULL (CDR P)) (NOT (FIXP (CADR P)))) 1) (T (CADR P)))) 
(PUT 'LOWER_DEG 'NUMBER-OF-ARGS 2) 
(PUT 'LOWER_DEG 'DEFINED-ON-LINE '3302) 
(PUT 'LOWER_DEG 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LOWER_DEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOWER_DEG (P V)
    (PROG (NEWP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P (NEQ (CAR P) V))) (RETURN NIL)))
        (PROGN (SETQ NEWP (CONS (CAR P) NEWP)) (SETQ P (CDR P)))
        (GO WHILELABEL))
      (COND
       (P
        (COND ((OR (NULL (CDR P)) (NOT (FIXP (CADR P)))) (SETQ P (CDR P)))
              (T
               (PROGN
                (SETQ NEWP (CONS (SUB1 (CADR P)) (CONS (CAR P) NEWP)))
                (SETQ P (CDDR P))))))
       (T (SETQ NEWP NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT P) (RETURN NIL)))
        (PROGN (SETQ NEWP (CONS (CAR P) NEWP)) (SETQ P (CDR P)))
        (GO WHILELABEL))
      (RETURN (REVERSE NEWP)))) 
(PUT 'DF_INT 'NUMBER-OF-ARGS 2) 
(PUT 'DF_INT 'DEFINED-ON-LINE '3319) 
(PUT 'DF_INT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DF_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DF_INT (D1 D2)
    (PROG (N L)
      (RETURN
       (COND
        (D1
         (COND
          (D2
           (PROGN
            (SETQ N (DIFFERENCE (DFDEG D1 (CAR D1)) (DFDEG D2 (CAR D1))))
            (SETQ L
                    (DF_INT
                     (COND ((AND (CDR D1) (NUMBERP (CADR D1))) (CDDR D1))
                           (T (CDR D1)))
                     D2))
            (COND ((LEQ N 0) L) ((EQUAL N 1) (CONS (CAR D1) L))
                  (T (CONS (CAR D1) (CONS N L))))))
          (T D1))))))) 
(PUT 'ALG_LINEAR_FCT 'NUMBER-OF-ARGS 2) 
(PUT 'ALG_LINEAR_FCT 'DEFINED-ON-LINE '3334) 
(PUT 'ALG_LINEAR_FCT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ALG_LINEAR_FCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALG_LINEAR_FCT (P F)
    (PROG (L)
      (SETQ L (LD_DERIV P F))
      (RETURN (AND L (EQUAL (CAR L) F) (EQUAL (CDR L) 1))))) 
(PUT 'LD_DERIV 'NUMBER-OF-ARGS 2) 
(PUT 'LD_DERIV 'DEFINED-ON-LINE '3358) 
(PUT 'LD_DERIV 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LD_DERIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LD_DERIV (P F)
    (PROG (L)
      (RETURN
       (COND
        ((SETQ L (GET P 'DERIVS))
         (PROGN
          (PROG ()
           WHILELABEL
            (COND ((NOT (AND L (NEQ (CAAAR L) F))) (RETURN NIL)))
            (SETQ L (CDR L))
            (GO WHILELABEL))
          (COND (L (CONS (REVAL1 (CONS 'DF (CAAR L)) T) (CDAR L))))))
        (T (CONS NIL 0)))))) 
(PUT 'LDIFFP 'NUMBER-OF-ARGS 2) 
(PUT 'LDIFFP 'DEFINED-ON-LINE '3369) 
(PUT 'LDIFFP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LDIFFP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LDIFFP (P F) (LD_DERIV_SEARCH P F (FCTARGS F))) 
(PUT 'LD_DERIV_SEARCH 'NUMBER-OF-ARGS 3) 
(PUT 'LD_DERIV_SEARCH 'DEFINED-ON-LINE '3374) 
(PUT 'LD_DERIV_SEARCH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LD_DERIV_SEARCH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LD_DERIV_SEARCH (P F VL)
    (PROG (A)
      (COND ((EQUAL P F) (SETQ A (CONS NIL 1)))
            (T
             (PROGN
              (SETQ A (CONS NIL 0))
              (COND
               ((PAIRP P)
                (COND
                 ((MEMBER (CAR P) '(PLUS TIMES QUOTIENT EQUAL))
                  (PROGN
                   (SETQ P (CDR P))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT P) (RETURN NIL)))
                     (PROGN
                      (SETQ A (DFMAX (LD_DERIV_SEARCH (CAR P) F VL) A VL))
                      (SETQ P (CDR P)))
                     (GO WHILELABEL))))
                 ((EQUAL (CAR P) 'MINUS)
                  (SETQ A (LD_DERIV_SEARCH (CADR P) F VL)))
                 ((EQUAL (CAR P) 'EXPT)
                  (PROGN
                   (SETQ A (LD_DERIV_SEARCH (CADR P) F VL))
                   (COND
                    ((NUMBERP (CDR A))
                     (COND
                      ((NUMBERP (CADDR P))
                       (SETQ A (CONS (CAR A) (TIMES (CADDR P) (CDR A)))))
                      ((NOT (ZEROP (CDR A))) (SETQ A (CONS NIL 'INFINITY)))
                      ((NOT (MY_FREEOF (CADDR P) F))
                       (SETQ A (CONS NIL 'INFINITY))))))))
                 ((EQUAL (CAR P) 'DF)
                  (COND ((EQUAL (CADR P) F) (SETQ A (CONS (CDDR P) 1)))
                        ((MY_FREEOF (CADR P) F) (SETQ A (CONS NIL 0)))
                        (T (SETQ A (CONS NIL 'INFINITY)))))
                 ((MY_FREEOF P F) (SETQ A (CONS NIL 0)))
                 ((MEMBER (CAR P) ONE_ARGUMENT_FUNCTIONS_)
                  (SETQ A
                          (CONS (CAR (LD_DERIV_SEARCH (CADR P) F VL))
                                'INFINITY)))
                 (T (SETQ A (CONS NIL 'INFINITY)))))))))
      (RETURN A))) 
(PUT 'LDERIV 'NUMBER-OF-ARGS 3) 
(PUT 'LDERIV 'DEFINED-ON-LINE '3415) 
(PUT 'LDERIV 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LDERIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LDERIV (P F VL)
    (PROG (L)
      (SETQ L (LD_DERIV_SEARCH P F VL))
      (RETURN
       (CONS
        (COND ((CAR L) (CONS 'DF (CONS F (CAR L)))) ((ZEROP (CDR L)) NIL)
              (T F))
        (CDR L))))) 
(PUT 'SPLITINHOM 'NUMBER-OF-ARGS 3) 
(PUT 'SPLITINHOM 'DEFINED-ON-LINE '3425) 
(PUT 'SPLITINHOM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SPLITINHOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPLITINHOM (Q FTEM VL)
    (PROG (QHOM QINHOM DENM)
      (SETQ VL (VARSLIST Q FTEM VL))
      (COND
       ((AND (PAIRP Q) (EQUAL (CAR Q) 'QUOTIENT))
        (COND
         ((STARP (SMEMBERL FTEM (CADDR Q)) (LENGTH VL))
          (PROGN (SETQ DENM (CADDR Q)) (SETQ Q (CADR Q))))
         (T (RETURN (CONS Q 0)))))
       (T (SETQ DENM 1)))
      (COND ((AND (PAIRP Q) (EQUAL (CAR Q) 'PLUS)) (SETQ Q (CDR Q)))
            (T (SETQ Q (LIST Q))))
      (PROG ()
       WHILELABEL
        (COND ((NOT Q) (RETURN NIL)))
        (PROGN
         (COND
          ((STARP (SMEMBERL FTEM (CAR Q)) (LENGTH VL))
           (SETQ QINHOM (CONS (CAR Q) QINHOM)))
          (T (SETQ QHOM (CONS (CAR Q) QHOM))))
         (SETQ Q (CDR Q)))
        (GO WHILELABEL))
      (COND ((NULL QINHOM) (SETQ QINHOM 0))
            ((GREATERP (LENGTH QINHOM) 1) (SETQ QINHOM (CONS 'PLUS QINHOM)))
            (T (SETQ QINHOM (CAR QINHOM))))
      (COND ((NULL QHOM) (SETQ QHOM 0))
            ((GREATERP (LENGTH QHOM) 1) (SETQ QHOM (CONS 'PLUS QHOM)))
            (T (SETQ QHOM (CAR QHOM))))
      (COND
       ((NEQ DENM 1)
        (PROGN
         (SETQ QHOM (LIST 'QUOTIENT QHOM DENM))
         (SETQ QINHOM (LIST 'QUOTIENT QINHOM DENM)))))
      (RETURN (CONS QHOM QINHOM)))) 
(PUT 'SEARCH_DEN 'NUMBER-OF-ARGS 1) 
(PUT 'SEARCH_DEN 'DEFINED-ON-LINE '3455) 
(PUT 'SEARCH_DEN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SEARCH_DEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEARCH_DEN (L)
    (PROG (L1)
      (COND
       ((PAIRP L)
        (COND
         ((EQUAL (CAR L) 'QUOTIENT)
          (SETQ L1
                  (UNION (CDDR L)
                         (UNION (SEARCH_DEN (CADR L))
                                (SEARCH_DEN (CADDR L))))))
         ((MEMBER (CAR L) '(LOG LN LOGB LOG10))
          (COND
           ((AND (PAIRP (CADR L)) (EQUAL (CAADR L) 'QUOTIENT))
            (SETQ L1 (UNION (LIST (CADADR L)) (SEARCH_DEN (CADR L)))))
           (T (SETQ L1 (UNION (CDR L) (SEARCH_DEN (CADR L)))))))
         (T (SETQ L1 (UNION (SEARCH_DEN (CAR L)) (SEARCH_DEN (CDR L))))))))
      (RETURN L1))) 
(PUT 'ZERO_DEN 'NUMBER-OF-ARGS 2) 
(PUT 'ZERO_DEN 'DEFINED-ON-LINE '3469) 
(PUT 'ZERO_DEN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ZERO_DEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZERO_DEN (L FTEM)
    (PROG (CASES CARL)
      (SETQ L (SEARCH_DEN L))
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ CARL (SIMP (CAR L)))
         (COND
          ((NULL (CAN_NOT_BECOME_ZEROSQ CARL FTEM))
           (SETQ CASES (CONS CARL CASES))))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN CASES))) 
(PUT 'FORG_INT 'NUMBER-OF-ARGS 2) 
(PUT 'FORG_INT 'DEFINED-ON-LINE '3483) 
(PUT 'FORG_INT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORG_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORG_INT (FORG FGES)
    (PROG (EX FORALL-RESULT FORALL-ENDPTR)
      (SETQ EX FORG)
      (COND ((NULL EX) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (EX)
                          (COND
                           ((AND (PAIRP EX) (PAIRP (CADR EX)))
                            (FORG_INT_F EX (SMEMBERL FGES EX)))
                           (T EX)))
                        (CAR EX))
                       NIL)))
     LOOPLABEL
      (SETQ EX (CDR EX))
      (COND ((NULL EX) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (EX)
                  (COND
                   ((AND (PAIRP EX) (PAIRP (CADR EX)))
                    (FORG_INT_F EX (SMEMBERL FGES EX)))
                   (T EX)))
                (CAR EX))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'FORG_INT_F 'NUMBER-OF-ARGS 2) 
(PUT 'FORG_INT_F 'DEFINED-ON-LINE '3488) 
(PUT 'FORG_INT_F 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORG_INT_F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORG_INT_F (EX FGES)
    (PROG (P H F)
      (SETQ P (LIST '*SQ (CADDR EX) T))
      (SETQ F (CADADR EX))
      (COND
       ((AND (PAIRP P) (EQUAL (CAR P) 'PLUS))
        (SETQ P
                (REVAL1 (CONS 'PLUS (CONS (LIST 'MINUS (CADR EX)) (CDR P)))
                        T)))
       (T (SETQ P (REVAL1 (LIST 'DIFFERENCE P (CADR EX)) T))))
      (SETQ P (INTEGRATEPDE P (CONS F FGES) NIL NIL NIL))
      (COND
       ((AND P (CAR P) (NOT (CDR P)))
        (PROGN
         (SETQ H (CAR (LDERIV (CAR P) F (FCTARGS F))))
         (SETQ P (REVAL1 (LIST 'PLUS (CAR P) H) T))
         (PROG (FF)
           (SETQ FF FNEW_)
          LAB
           (COND ((NULL FF) (RETURN NIL)))
           ((LAMBDA (FF)
              (COND
               ((NOT (MEMBER FF FTEM_)) (SETQ FTEM_ (FCTINSERT FF FTEM_)))))
            (CAR FF))
           (SETQ FF (CDR FF))
           (GO LAB))
         (SETQ EX (LIST 'EQUAL H P)))))
      (RETURN EX))) 
(PUT 'TOTAL_ALG_MODE_DERIV 'PSOPFN 'TOT_ALG_DERI) 
(PUT 'TOT_ALG_DERI 'NUMBER-OF-ARGS 1) 
(PUT 'TOT_ALG_DERI 'DEFINED-ON-LINE '3525) 
(PUT 'TOT_ALG_DERI 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'TOT_ALG_DERI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOT_ALG_DERI (INP)
    (PROG (S)
      (RETURN
       (LIST '*SQ
             (DIFFSQ
              (PROGN
               (SETQ S (REVAL1 (CAR INP) NIL))
               (COND ((AND (PAIRP S) (EQUAL (CAR S) '*SQ)) (CADR S))
                     (T (SIMP S))))
              (REVAL1 (CADR INP) T))
             T)))) 
(PUT 'NO_OF_V 'NUMBER-OF-ARGS 2) 
(PUT 'NO_OF_V 'DEFINED-ON-LINE '3533) 
(PUT 'NO_OF_V 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_V 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NO_OF_V (V L)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND ((NOT (AND L (NEQ (CAR L) V))) (RETURN NIL)))
       (SETQ L (CDR L))
       (GO WHILELABEL))
     (COND ((NULL L) 0)
           ((OR (NULL (CDR L)) (NOT (FIXP (CADR L))) (EQUAL (CADR L) 1)) 1)
           (T (CADR L))))) 
(PUT 'MULTIPLE_DIFFSQ 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIPLE_DIFFSQ 'DEFINED-ON-LINE '3542) 
(PUT 'MULTIPLE_DIFFSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MULTIPLE_DIFFSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIPLE_DIFFSQ (P H)
    (PROG (V M N)
      (PROG ()
       WHILELABEL
        (COND ((NOT H) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR H))
         (SETQ H (CDR H))
         (SETQ V (CAAAR (CAR (MKSQ V 1))))
         (COND ((NULL H) (SETQ N 1))
               ((FIXP (CAR H)) (PROGN (SETQ N (CAR H)) (SETQ H (CDR H))))
               (T (SETQ N 1)))
         (PROG (M)
           (SETQ M 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N M)) (RETURN NIL)))
           (SETQ P (DIFFSQ P V))
           (SETQ M (PLUS2 M 1))
           (GO LAB)))
        (GO WHILELABEL))
      (RETURN P))) 
(PUT 'CP_SQ2P_VAL 'NUMBER-OF-ARGS 1) 
(PUT 'CP_SQ2P_VAL 'DEFINED-ON-LINE '3555) 
(PUT 'CP_SQ2P_VAL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CP_SQ2P_VAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CP_SQ2P_VAL (P)
    (COND ((NULL (GET P 'PVAL)) (PUT P 'PVAL (PREPSQ (GET P 'SQVAL)))))) 
(PUT 'SQZEROP 'NUMBER-OF-ARGS 1) 
(PUT 'SQZEROP 'DEFINED-ON-LINE '3562) 
(PUT 'SQZEROP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQZEROP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQZEROP (P)
    (COND ((ATOM P) (ZEROP P))
          ((NEQ (CAR P) '*SQ) (OR (NULL (CAR P)) (ZEROP (CAR P))))
          (T
           (OR (NULL (CAR (CADR P)))
               (AND ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CAADR P))
                    (NOT (ATOM (CAADR P)))
                    (APPLY1 (GET (CAR (CAADR P)) 'ZEROP) (CAADR P))))))) 
(PUT 'MEMBERL 'NUMBER-OF-ARGS 2) 
(PUT 'MEMBERL 'DEFINED-ON-LINE '3586) 
(PUT 'MEMBERL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MEMBERL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MEMBERL (A B)
    (COND
     ((AND A B)
      (COND ((MEMBER (CAR A) B) (CONS (CAR A) (MEMBERL (CDR A) B)))
            (T (MEMBERL (CDR A) B)))))) 
(PUT 'SMEMBERL 'NUMBER-OF-ARGS 2) 
(PUT 'SMEMBERL 'DEFINED-ON-LINE '3592) 
(PUT 'SMEMBERL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SMEMBERL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SMEMBERL (FL EX)
    (COND
     ((AND FL EX)
      (COND ((SMEMBER (CAR FL) EX) (CONS (CAR FL) (SMEMBERL (CDR FL) EX)))
            (T (SMEMBERL (CDR FL) EX)))))) 
(FLAG '(MY_FREEOF) 'OPFN) 
(PUT 'MY_FREEOF 'NUMBER-OF-ARGS 2) 
(PUT 'MY_FREEOF 'DEFINED-ON-LINE '3599) 
(PUT 'MY_FREEOF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MY_FREEOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MY_FREEOF (U V) (AND (NOT (SMEMBER V U)) (FREEOFDEPL DEPL* U V))) 
(FLAG '(MY_FREEOF) 'BOOLEAN) 
(PUT 'FREEOFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'FREEOFLIST 'DEFINED-ON-LINE '3604) 
(PUT 'FREEOFLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FREEOFLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FREEOFLIST (L M)
    (COND ((NULL M) T) ((FREEOF L (CAR M)) (FREEOFLIST L (CDR M))) (T NIL))) 
(PUT 'FREEOFDEPL 'NUMBER-OF-ARGS 3) 
(PUT 'FREEOFDEPL 'DEFINED-ON-LINE '3610) 
(PUT 'FREEOFDEPL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FREEOFDEPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FREEOFDEPL (DE U V)
    (COND ((NULL DE) T) ((AND (SMEMBER V (CDAR DE)) (SMEMBER (CAAR DE) U)) NIL)
          (T (FREEOFDEPL (CDR DE) U V)))) 
(PUT 'FCTINS 'NUMBER-OF-ARGS 3) 
(PUT 'FCTINS 'DEFINED-ON-LINE '3615) 
(PUT 'FCTINS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTINS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FCTINS (F FLEN FTEM)
    (COND ((NULL FTEM) (LIST F))
          ((LESSP (FCTLENGTH (CAR FTEM)) FLEN) (CONS F FTEM))
          (T (CONS (CAR FTEM) (FCTINSERT F (CDR FTEM)))))) 
(PUT 'FCTINSERT 'NUMBER-OF-ARGS 2) 
(PUT 'FCTINSERT 'DEFINED-ON-LINE '3620) 
(PUT 'FCTINSERT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTINSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FCTINSERT (F FTEM)
    (COND ((FREEOF FTEM F) (FCTINS F (FCTLENGTH F) FTEM)) (T FTEM))) 
(PUT 'NEWFCT 'NUMBER-OF-ARGS 3) 
(PUT 'NEWFCT 'DEFINED-ON-LINE '3625) 
(PUT 'NEWFCT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NEWFCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWFCT (ID L NFCT)
    (PROG (F)
      (COND
       ((AND (NULL LEVEL_) (EQUAL ID FNAME_) RECYCLE_FCTS)
        (PROGN
         (SETQ F (CAR RECYCLE_FCTS))
         (SETQ RECYCLE_FCTS (CDR RECYCLE_FCTS))))
       (T (SETQ F (MKID ID NFCT))))
      (SETQ DEPL* (DELETE (ASSOC F DEPL*) DEPL*))
      (COND ((PAIRP L) (SETQ DEPL* (CONS (CONS F L) DEPL*))))
      (COND
       (PRINT_
        (PROGN
         (TERPRI)
         (COND
          ((PAIRP L)
           (PROGN (PROGN (PRIN2 "new function: ") NIL) (FCTPRINT (LIST F))))
          (T (PROGN (PRIN2 "new constant: ") (PRIN2 F) NIL))))))
      (RETURN F))) 
(PUT 'DROP_FCT 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_FCT 'DEFINED-ON-LINE '3649) 
(PUT 'DROP_FCT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_FCT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_FCT (F)
    (COND
     ((NULL COLLECT_SOL)
      (PROGN
       (COND (DO_RECYCLE_FNC (SETQ RECYCLE_FCTS (CONS F RECYCLE_FCTS))))
       (SETQ DEPL* (DELETE (ASSOC (REVAL1 F T) DEPL*) DEPL*)))))) 
(PUT 'VARSLIST 'NUMBER-OF-ARGS 3) 
(PUT 'VARSLIST 'DEFINED-ON-LINE '3658) 
(PUT 'VARSLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'VARSLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VARSLIST (P FTEM VL)
    (PROG (L)
      (SETQ FTEM (ARGSET (SMEMBERL FTEM P)))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (COND
            ((OR (NOT (MY_FREEOF P V)) (MEMBER V FTEM)) (SETQ L (CONS V L)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (REVERSE L)))) 
(PUT 'VAR_LIST 'NUMBER-OF-ARGS 3) 
(PUT 'VAR_LIST 'DEFINED-ON-LINE '3666) 
(PUT 'VAR_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'VAR_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VAR_LIST (PDES FORG VL)
    (PROG (L L1)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ L (UNION (GET P 'VARS) L))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (COND
            ((OR (MEMBER V L) (NOT (MY_FREEOF FORG V)))
             (SETQ L1 (CONS V L1)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (REVERSE L1)))) 
(PUT 'F_UPDATE 'NUMBER-OF-ARGS 2) 
(PUT 'F_UPDATE 'DEFINED-ON-LINE '3674) 
(PUT 'F_UPDATE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'F_UPDATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE F_UPDATE (PDES FORG)
    (PROG (FDROP P F)
      (SETQ FDROP FTEM_)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ FDROP (SETDIFF_ACCORDING_TO FDROP (GET P 'FCTS) FTEM_)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (F)
        (SETQ F FTEM_)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND ((NOT (FREEOF FORG F)) (SETQ FDROP (DELETE F FDROP)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       (FSUB_
        (PROG (F)
          (SETQ F FTEM_)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F)
             (COND ((NOT (FREEOF FSUB_ F)) (SETQ FDROP (DELETE F FDROP)))))
           (CAR F))
          (SETQ F (CDR F))
          (GO LAB))))
      (PROG (F)
        (SETQ F FDROP)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (DROP_FCT F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ FLIN_ (SETDIFF_ACCORDING_TO FLIN_ FDROP FTEM_))
      (SETQ FTEM_ (SETDIFF_ACCORDING_TO FTEM_ FDROP FTEM_)))) 
(FLAG '(FARGS) 'OPFN) 
(PUT 'FARGS 'NUMBER-OF-ARGS 1) 
(PUT 'FARGS 'DEFINED-ON-LINE '3692) 
(PUT 'FARGS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FARGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FARGS (F)
    (CONS 'LIST
          (FCTARGS
           (COND ((AND (PAIRP F) (EQUAL (CAR F) '*SQ)) (REVAL1 F T)) (T F))))) 
(PUT 'FCTARGS 'NUMBER-OF-ARGS 1) 
(PUT 'FCTARGS 'DEFINED-ON-LINE '3695) 
(PUT 'FCTARGS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTARGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTARGS (F) (COND ((SETQ F (ASSOC F DEPL*)) (CDR F)))) 
(PUT 'FCTLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'FCTLENGTH 'DEFINED-ON-LINE '3699) 
(PUT 'FCTLENGTH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTLENGTH (F) (LENGTH (FCTARGS F))) 
(PUT 'FCTSORT 'NUMBER-OF-ARGS 1) 
(PUT 'FCTSORT 'DEFINED-ON-LINE '3703) 
(PUT 'FCTSORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTSORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTSORT (L)
    (PROG (L1 L2 L3 M N)
      (RETURN
       (COND ((NULL L) NIL)
             (T
              (PROGN
               (SETQ N (FCTLENGTH (CAR L)))
               (SETQ L2 (LIST (CAR L)))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (SETQ M (FCTLENGTH (CAR L)))
                  (COND ((LESSP M N) (SETQ L1 (CONS (CAR L) L1)))
                        ((GREATERP M N) (SETQ L3 (CONS (CAR L) L3)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (FCTSORT (REVERSIP L3))
                       (APPEND (REVERSIP L2) (FCTSORT (REVERSIP L1)))))))))) 
(FLAG '(CHKFLIST) 'OPFN) 
(PUT 'CHKFLIST 'NUMBER-OF-ARGS 2) 
(PUT 'CHKFLIST 'DEFINED-ON-LINE '3722) 
(PUT 'CHKFLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHKFLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHKFLIST (FLIST SB)
    (PROG (F H)
      (PROG (F)
        (SETQ F (CDR FLIST))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND
            ((SETQ H (MEMBERL (CDR SB) (FCTARGS F)))
             (PROGN
              (PROGN
               (PRIN2 "##### The function ")
               (PRIN2 F)
               (PRIN2 " that is to be computed depends on ")
               (PRIN2 H)
               (PRIN2 " which is a left hand side of the input system or a ")
               (PRIN2 "derivative of a left hand side of the input system")
               NIL)
              NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB)))) 
(PUT 'LISTPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'LISTPRINT 'DEFINED-ON-LINE '3732) 
(PUT 'LISTPRINT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LISTPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTPRINT (L)
    (COND
     ((PAIRP L)
      (PROGN
       (PRIN1 (CAR L))
       (PROG (V)
         (SETQ V (CDR L))
        LAB
         (COND ((NULL V) (RETURN NIL)))
         ((LAMBDA (V) (PROGN (PRIN2 ",") (PRIN1 V))) (CAR V))
         (SETQ V (CDR V))
         (GO LAB)))))) 
(PUT 'FCTPRINT1 'NUMBER-OF-ARGS 1) 
(PUT 'FCTPRINT1 'DEFINED-ON-LINE '3739) 
(PUT 'FCTPRINT1 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTPRINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTPRINT1 (F)
    (PROG (VL)
      (COND
       (F
        (COND
         ((PAIRP F)
          (PROGN
           (PROGN (PRIN2 (CAR F)) NIL)
           (COND
            ((PAIRP (CDR F))
             (PROGN
              (PROG (A)
                (SETQ A VL_)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A)
                   (COND ((NOT (FREEOF (CDR F) A)) (SETQ VL (CONS A VL)))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB))
              (PROGN (PRIN2 "(") NIL)
              (LISTPRINT (APPEND (SETDIFF (CDR F) VL) (REVERSE VL)))
              (PROGN (PRIN2 ")") NIL))))))
         (T (PROGN (PRIN2 F) NIL))))))) 
(PUT 'FCTPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'FCTPRINT 'DEFINED-ON-LINE '3756) 
(PUT 'FCTPRINT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTPRINT (FL)
    (PROG (L F A N NN)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT FL) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FL))
         (SETQ FL (CDR FL))
         (COND
          ((PAIRP F)
           (COND
            ((EQUAL (CAR F) 'EQUAL)
             (PROGN
              (SETQ N
                      (COND
                       ((AND (PAIRP (CADDR F)) (EQUAL (CAR (CADDR F)) '*SQ))
                        (NO_OF_TM_SQ (CADR (CADDR F))))
                       (T (NO_OF_TERMS (CADDR F)))))
              (COND
               ((OR (NULL PRINT_) (GREATERP N PRINT_))
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 (CADR F))
                  (PRIN2 "= expr. with ")
                  (PRIN2 N)
                  (PRIN2 " terms")
                  NIL)
                 (COND
                  ((SETQ L (GET (CADR F) 'FCTS))
                   (PROGN (PROGN (PRIN2 " in ") NIL) (MYPRIN2L L ", "))))
                 (TERPRI)))
               (T (MATHPRINT F)))
              (SETQ N 0)))
            (T
             (PROGN
              (COND ((EQUAL N 4) (PROGN (TERPRI) (SETQ N 0))))
              (FCTPRINT1 F)
              (COND (FL (PROGN (PRIN2 ", ") NIL)))
              (SETQ N (ADD1 N))))))
          (T
           (PROGN
            (SETQ NN
                    (REVAL1
                     (LIST 'PLUS 4 (LENGTH (EXPLODE F))
                           (PROG (A FORALL-RESULT)
                             (SETQ A (FCTARGS F))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND ((NULL A) (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS
                                      ((LAMBDA (A) (ADD1 (LENGTH (EXPLODE A))))
                                       (CAR A))
                                      FORALL-RESULT))
                             (SETQ A (CDR A))
                             (GO LAB1)))
                     T))
            (COND ((GREATERP (PLUS NN N) 79) (PROGN (TERPRI) (SETQ N 0))))
            (SETQ L (ASSOC F DEPL*))
            (FCTPRINT1 (COND (L L) (T F)))
            (COND (FL (PROGN (PRIN2 ", ") NIL)))
            (SETQ N (PLUS NN N))))))
        (GO WHILELABEL)))) 
(PUT 'FCTPRINT_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'FCTPRINT_SQ 'DEFINED-ON-LINE '3797) 
(PUT 'FCTPRINT_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FCTPRINT_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTPRINT_SQ (FL)
    (PROG (L F A N NN)
      (SETQ N 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT FL) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FL))
         (SETQ FL (CDR FL))
         (COND
          ((PAIRP F)
           (COND
            ((EQUAL (CAR F) 'EQUAL)
             (PROGN
              (SETQ N (NO_OF_TM_SQ (CADDR F)))
              (COND
               ((OR (NULL PRINT_) (GREATERP N PRINT_))
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 (CADR F))
                  (PRIN2 "= expr. with ")
                  (PRIN2 N)
                  (PRIN2 " terms")
                  NIL)
                 (COND
                  ((SETQ L (GET (CADR F) 'FCTS))
                   (PROGN (PROGN (PRIN2 " in ") NIL) (MYPRIN2L L ", "))))
                 (TERPRI)))
               (T
                (MATHPRINT
                 (LIST 'EQUAL (CADR F)
                       (COND ((NULL (CAR (CADDR F))) 0)
                             (T (LIST '*SQ (CADDR F) T)))))))
              (SETQ N 0)))
            (T
             (PROGN
              (COND ((EQUAL N 4) (PROGN (TERPRI) (SETQ N 0))))
              (FCTPRINT1 F)
              (COND (FL (PROGN (PRIN2 ", ") NIL)))
              (SETQ N (ADD1 N))))))
          (T
           (PROGN
            (SETQ NN
                    (REVAL1
                     (LIST 'PLUS 4 (LENGTH (EXPLODE F))
                           (PROG (A FORALL-RESULT)
                             (SETQ A (FCTARGS F))
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND ((NULL A) (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS
                                      ((LAMBDA (A) (ADD1 (LENGTH (EXPLODE A))))
                                       (CAR A))
                                      FORALL-RESULT))
                             (SETQ A (CDR A))
                             (GO LAB1)))
                     T))
            (COND ((GREATERP (PLUS NN N) 79) (PROGN (TERPRI) (SETQ N 0))))
            (SETQ L (ASSOC F DEPL*))
            (FCTPRINT1 (COND (L L) (T F)))
            (COND (FL (PROGN (PRIN2 ", ") NIL)))
            (SETQ N (PLUS NN N))))))
        (GO WHILELABEL)))) 
(FLAG '(FDEP) 'OPFN) 
(PUT 'FDEP 'NUMBER-OF-ARGS 1) 
(PUT 'FDEP 'DEFINED-ON-LINE '3839) 
(PUT 'FDEP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FDEP (FL)
    (PROG (L F)
      (TERPRI)
      (SETQ FL (CDR (REVAL1 FL T)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FL) (RETURN NIL)))
        (PROGN
         (SETQ F (CAR FL))
         (SETQ FL (CDR FL))
         (SETQ L (ASSOC F DEPL*))
         (PROGN (PRIN2 F) NIL)
         (COND
          ((AND L (CDR L))
           (PROGN (PROGN (PRIN2 "=") NIL) (FCTPRINT1 (COND (L L) (T F))))))
         (COND (FL (PROGN (PRIN2 ", ") NIL))))
        (GO WHILELABEL))
      (TERPRI))) 
(PUT 'DEPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'DEPRINT 'DEFINED-ON-LINE '3854) 
(PUT 'DEPRINT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DEPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEPRINT (L)
    (COND
     ((AND L PRINT_)
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (EQPRINT (LIST 'EQUAL 0 X))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))))) 
(PUT 'EQPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'EQPRINT 'DEFINED-ON-LINE '3858) 
(PUT 'EQPRINT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'EQPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQPRINT (E)
    (COND
     (PRINT_
      (PROG (N)
        (SETQ N
                (COND ((NOT (PAIRP E)) 1)
                      ((EQUAL (CAR E) '*SQ) (DELENGTHSQ (CADR E)))
                      ((EQUAL (CAR E) 'EQUAL)
                       (COND ((NOT (PAIRP (CADDR E))) 1)
                             ((EQUAL (CAADDR E) '*SQ)
                              (DELENGTHSQ (CADR (CADDR E))))
                             (T (DELENGTH (CADDR E)))))
                      (T (DELENGTH E))))
        (COND
         ((GREATERP N PRINT_)
          (PROGN
           (PROGN
            (PRIN2 N)
            (PRIN2 " factors in ")
            (PRIN2
             (COND ((NOT (PAIRP E)) 1)
                   ((EQUAL (CAR E) '*SQ) (NO_OF_TM_SQ (CADR E)))
                   ((EQUAL (CAR E) 'EQUAL)
                    (COND ((NOT (PAIRP (CADDR E))) 1)
                          ((EQUAL (CAADDR E) '*SQ)
                           (NO_OF_TM_SQ (CADR (CADDR E))))
                          (T (NO_OF_TERMS (CADDR E)))))
                   (T (NO_OF_TERMS E))))
            (PRIN2 " terms")
            NIL)
           (TERPRI)))
         ((SQZEROP E) (MATHPRINT 0))
         ((AND (PAIRP E) (EQUAL (CAR E) 'EQUAL) (SQZEROP (CADDR E)))
          (MATHPRINT (LIST 'EQUAL (CADR E) 0)))
         (T (MATHPRINT E))))))) 
(PUT 'PRINT_LEVEL 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_LEVEL 'DEFINED-ON-LINE '3888) 
(PUT 'PRINT_LEVEL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_LEVEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_LEVEL (MODE)
    (COND
     ((AND PRINT_ LEVEL_)
      (PROGN
       (TERPRI)
       (COND ((EQUAL MODE 2) (PROGN (PRIN2 "New level :     ") NIL))
             ((EQUAL MODE 1) (PROGN (PRIN2 "Current level : ") NIL))
             (T (PROGN (PRIN2 "Back to level : ") NIL)))
       (PROG (M)
         (SETQ M (REVERSE LEVEL_))
        LAB
         (COND ((NULL M) (RETURN NIL)))
         ((LAMBDA (M) (PROGN (PRIN2 M) (PRIN2 ".") NIL)) (CAR M))
         (SETQ M (CDR M))
         (GO LAB))
       (TERPRI))))) 
(PUT 'START_LEVEL 'NUMBER-OF-ARGS 2) 
(PUT 'START_LEVEL 'DEFINED-ON-LINE '3898) 
(PUT 'START_LEVEL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'START_LEVEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE START_LEVEL (N NEW_ASSUMPTION)
    (PROGN
     (SETQ LEVEL_ (CONS N LEVEL_))
     (CASETREE (COND ((NULL NEW_ASSUMPTION) (LIST NIL)) (T NEW_ASSUMPTION)))
     (PRINT_LEVEL 2)
     (COND
      ((AND SIZE_WATCH (NOT (FIXP SIZE_WATCH)))
       (SETQ HISTORY_
               (CONS
                (BLDMSG_INTERNAL "%w%w"
                                 (LIST "Start of level " (LEVEL_STRING NIL)))
                (CONS 'IG HISTORY_)))))
     (COND
      (SIZE_WATCH
       (SETQ SIZE_HIST
               (CONS (LIST 'A "Start of " (REVERSE LEVEL_) NEW_ASSUMPTION)
                     SIZE_HIST))))
     NIL)) 
(PUT 'FINISH_LEVEL 'NUMBER-OF-ARGS 1) 
(PUT 'FINISH_LEVEL 'DEFINED-ON-LINE '3911) 
(PUT 'FINISH_LEVEL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FINISH_LEVEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINISH_LEVEL (NO_OF_SOL)
    (PROG (S)
      (DELETE_BACKUP)
      (CASETREE NO_OF_SOL)
      (COND
       ((AND SIZE_WATCH (NOT (FIXP SIZE_WATCH)))
        (PROGN
         (SETQ S (LEVEL_STRING NIL))
         (SETQ S
                 (BLDMSG_INTERNAL "End of level %w, %d solution(s)"
                                  (LIST S NO_OF_SOL)))
         (SETQ HISTORY_ (CONS S (CONS 'IG HISTORY_))))))
      (SETQ LEVEL_ (CDR LEVEL_))
      (PRINT_LEVEL 0)
      (COND
       (SIZE_WATCH
        (SETQ SIZE_HIST
                (CONS (LIST 'Z "Back to " (REVERSE LEVEL_) NO_OF_SOL)
                      SIZE_HIST))))
      (SETQ S SWITCH_LIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT S) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (LENGTH (CAR S)) (LENGTH LEVEL_))
           (PROGN
            (COND ((CADDAR S) (ON1 (CADAR S))) (T (OFF1 (CADAR S))))
            (SETQ S (CDR S))
            (SETQ SWITCH_LIST (CDR SWITCH_LIST))))
          (T (SETQ S NIL))))
        (GO WHILELABEL)))) 
(PUT 'PRINT_STATISTIC 'NUMBER-OF-ARGS 2) 
(PUT 'PRINT_STATISTIC 'DEFINED-ON-LINE '3936) 
(PUT 'PRINT_STATISTIC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_STATISTIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINT_STATISTIC (PDES FCTS)
    (COND
     (PRINT_
      (PROG (J K LE R S N M P EL FL VL PL ST PDECP)
        (SETQ J 0)
        (SETQ K 0)
        (SETQ LE 0)
        (SETQ R 0)
        (SETQ S 0)
        (SETQ PDECP PDES)
        (COND
         (PDES
          (PROGN
           (COND
            ((NEQ EQUATIONS_FILE "")
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 "equations read from disk : ") (PRIN2 EQN_NO) NIL)
              (SETQ ST "     ")
              NIL))
            ((NULL LIN_PROBLEM) (SETQ ST "     ")) (T (SETQ ST "")))
           (TERPRI)
           (PROGN
            (PRIN2 "number of equations ")
            (PRIN2 ST)
            (PRIN2 ": ")
            (PRIN2 (LENGTH PDES))
            NIL)
           (COND
            ((NULL LIN_PROBLEM)
             (PROGN
              (SETQ J 0)
              (PROG (P)
                (SETQ P PDES)
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P) (COND ((GET P 'LINEAR_) (SETQ J (ADD1 J)))))
                 (CAR P))
                (SETQ P (CDR P))
                (GO LAB))
              (TERPRI)
              (PROGN (PRIN2 "number of lin. equations : ") (PRIN2 J) NIL)
              NIL)))
           (TERPRI)
           (PROGN
            (PRIN2 "total no of terms   ")
            (PRIN2 ST)
            (PRIN2 ": ")
            (PRIN2
             (SETQ J
                     (PROG (P FORALL-RESULT)
                       (SETQ P PDES)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (PLUS ((LAMBDA (P) (GET P 'TERMS)) (CAR P))
                                     FORALL-RESULT))
                       (SETQ P (CDR P))
                       (GO LAB1))))
            NIL)
           (SETQ K
                   (PROG (P FORALL-RESULT)
                     (SETQ P PDES)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS ((LAMBDA (P) (GET P 'LENGTH)) (CAR P))
                                   FORALL-RESULT))
                     (SETQ P (CDR P))
                     (GO LAB1)))
           (COND
            ((NEQ K J)
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 "total no of factors ")
               (PRIN2 ST)
               (PRIN2 ": ")
               (PRIN2 K)
               NIL))))
           (PROG ()
            WHILELABEL
             (COND ((NOT PDES) (RETURN NIL)))
             (PROGN
              (SETQ J 0)
              (SETQ EL NIL)
              (PROG (P)
                (SETQ P PDES)
               LAB
                (COND ((NULL P) (RETURN NIL)))
                ((LAMBDA (P)
                   (PROGN
                    (SETQ VL (GET P 'VARS))
                    (COND (VL (SETQ LE (LENGTH VL))) (T (SETQ LE 0)))
                    (COND
                     ((OR (AND (EQUAL J 0) (NULL VL)) (EQUAL J LE))
                      (SETQ EL (CONS P EL)))
                     ((LESSP J LE) (PROGN (SETQ J LE) (SETQ EL (LIST P)))))))
                 (CAR P))
                (SETQ P (CDR P))
                (GO LAB))
              (SETQ PDES (SETDIFF PDES EL))
              (COND
               (EL
                (PROGN
                 (SETQ N (LENGTH EL))
                 (TERPRI)
                 (PROGN (PRIN2 N) (PRIN2 " equation") NIL)
                 (COND ((GREATERP N 1) (PROGN (PRIN2 "s") NIL)))
                 (PROGN (PRIN2 " in ") (PRIN2 J) (PRIN2 " variable") NIL)
                 (COND ((NEQ J 1) (PROGN (PRIN2 "s") NIL)))
                 (PROGN (PRIN2 ": ") NIL)
                 (COND (STRUC_EQN (SETQ EL (SORT_DERIV_PDES EL))))
                 (PROG ()
                  REPEATLABEL
                   (PROGN
                    (COND
                     (STRUC_EQN
                      (PROGN
                       (SETQ PL (FIRST EL))
                       (SETQ EL (CDR EL))
                       (TERPRI)
                       (PROGN
                        (PRIN2 (LENGTH (CDR PL)))
                        (PRIN2 " equations with ")
                        (PRIN2 (CAR PL))
                        (PRIN2 " derivative")
                        (PRIN2 (COND ((EQUAL (CAR PL) 1) ":") (T "s:")))
                        NIL)
                       (SETQ PL (CDR PL))))
                     (T (PROGN (SETQ PL EL) (SETQ EL NIL))))
                    (SETQ K 29)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT PL) (RETURN NIL)))
                      (PROGN
                       (COND
                        ((GEQ K 70)
                         (PROGN (SETQ K 0) (TERPRI) (PROGN (PRIN2 "  ") NIL))))
                       (SETQ K
                               (PLUS K 4 (LENGTH (EXPLODE (CAR PL)))
                                     (LENGTH (EXPLODE (GET (CAR PL) 'TERMS)))))
                       (PROGN
                        (PRIN2 (CAR PL))
                        (PRIN2 "(")
                        (PRIN2 (GET (CAR PL) 'TERMS))
                        NIL)
                       (COND
                        ((SETQ S (GET (CAR PL) 'STARDE))
                         (PROGN
                          (PROG (R)
                            (SETQ R 1)
                           LAB
                            (COND
                             ((MINUSP (DIFFERENCE (PLUS 1 (CAAR S)) R))
                              (RETURN NIL)))
                            (PROGN (PRIN2 "*") NIL)
                            (SETQ R (PLUS2 R 1))
                            (GO LAB))
                          (SETQ K (PLUS K 1 (CAAR S)))
                          NIL)))
                       (COND
                        ((PAIRP (GET (CAR PL) 'FAC)) (PROGN (PRIN2 "#") NIL)))
                       (COND
                        ((GET (CAR PL) 'CASE2SEP) (PROGN (PRIN2 "!") NIL)))
                       (COND
                        ((AND FLIN_ (GET (CAR PL) 'ALLVARFCTS)
                              (FREEOFLIST (GET (CAR PL) 'ALLVARFCTS) FLIN_))
                         (PROGN (PRIN2 "a") NIL)))
                       (COND
                        ((AND (NULL LIN_PROBLEM) (GET (CAR PL) 'LINEAR_))
                         (PROGN (PRIN2 "l") NIL)))
                       (PROGN (PRIN2 ")") NIL)
                       (SETQ PL (CDR PL))
                       (COND (PL (PROGN (PRIN2 ",") NIL)))
                       NIL)
                      (GO WHILELABEL))
                    NIL)
                   (COND ((NOT (NULL EL)) (GO REPEATLABEL))))
                 NIL)))
              (SETQ J (ADD1 J))
              NIL)
             (GO WHILELABEL))))
         (T (PROGN (TERPRI) (PROGN (PRIN2 "no equations") NIL))))
        (PROG (F)
          (SETQ F FCTS)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F) (COND ((NOT (PAIRP F)) (SETQ FL (CONS F FL))))) (CAR F))
          (SETQ F (CDR F))
          (GO LAB))
        (PROG (F)
          (SETQ F FSUB_)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F) (SETQ FL (DELETE (CAR F) FL))) (CAR F))
          (SETQ F (CDR F))
          (GO LAB))
        (COND
         (FL
          (PROGN
           (SETQ FL (FCTSORT FL))
           (SETQ M (FCTLENGTH (CAR FL)))
           (PROG ()
            WHILELABEL
             (COND ((NOT (GEQ M 0)) (RETURN NIL)))
             (PROGN
              (SETQ N 0)
              (SETQ EL NIL)
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND FL (EQUAL (FCTLENGTH (CAR FL)) M))) (RETURN NIL)))
                (PROGN
                 (SETQ N (ADD1 N))
                 (SETQ EL (CONS (CAR FL) EL))
                 (SETQ FL (CDR FL)))
                (GO WHILELABEL))
              (COND
               ((GREATERP N 0)
                (COND
                 ((GREATERP M 0)
                  (PROGN
                   (TERPRI)
                   (PROGN (PRIN2 N) (PRIN2 " function") NIL)
                   (COND ((GREATERP N 1) (PROGN (PRIN2 "s") NIL)))
                   (PROGN
                    (PRIN2 " with ")
                    (PRIN2 M)
                    (PRIN2 " argument")
                    (PRIN2 (COND ((GREATERP M 1) "s : ") (T "  : ")))
                    NIL)))
                 (T
                  (PROGN
                   (TERPRI)
                   (PROGN (PRIN2 N) (PRIN2 " constant") NIL)
                   (COND ((GREATERP N 1) (PROGN (PRIN2 "s") NIL)))
                   (PROGN (PRIN2 " : ") NIL))))))
              (SETQ K 5)
              (SETQ EL (SORT_ACCORDING_TO EL FTEM_))
              (PROG ()
               WHILELABEL
                (COND ((NOT EL) (RETURN NIL)))
                (PROGN
                 (COND
                  ((EQUAL K 8)
                   (PROGN (SETQ K 0) (TERPRI) (PROGN (PRIN2 "  ") NIL)))
                  (T (SETQ K (ADD1 K))))
                 (PROGN (PRIN2 (CAR EL)) NIL)
                 (SETQ N 0)
                 (PROG (P)
                   (SETQ P PDECP)
                  LAB
                   (COND ((NULL P) (RETURN NIL)))
                   ((LAMBDA (P)
                      (COND
                       ((MEMBER (CAR EL) (GET P 'FCTS)) (SETQ N (ADD1 N)))))
                    (CAR P))
                   (SETQ P (CDR P))
                   (GO LAB))
                 (PROGN (PRIN2 "(") (PRIN2 N) (PRIN2 ")") NIL)
                 (SETQ EL (CDR EL))
                 (COND (EL (PROGN (PRIN2 ",") NIL)))
                 NIL)
                (GO WHILELABEL))
              (SETQ M (COND (FL (FCTLENGTH (CAR FL))) (T (MINUS 1)))))
             (GO WHILELABEL))))
         (T (PROGN (TERPRI) (PROGN (PRIN2 "no functions or constants") NIL))))
        (TERPRI))))) 
(PUT 'GET_STATISTIC 'NUMBER-OF-ARGS 2) 
(PUT 'GET_STATISTIC 'DEFINED-ON-LINE '4068) 
(PUT 'GET_STATISTIC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'GET_STATISTIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_STATISTIC (PDES FCTS)
    (COND (CONTRADICTION_ "contradiction")
          (T
           (PROG (J LE N P EL FL VL LI STATS)
             (SETQ J 0)
             (SETQ LE 0)
             (SETQ STATS
                     (LIST LAST_FREE_CELLS
                           (PROG (P FORALL-RESULT)
                             (SETQ P PDES)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS
                                      ((LAMBDA (P) (GET P 'LENGTH)) (CAR P))
                                      FORALL-RESULT))
                             (SETQ P (CDR P))
                             (GO LAB1))
                           (PROG (P FORALL-RESULT)
                             (SETQ P PDES)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS
                                      ((LAMBDA (P) (GET P 'TERMS)) (CAR P))
                                      FORALL-RESULT))
                             (SETQ P (CDR P))
                             (GO LAB1))
                           (LENGTH PDES) (LENGTH FTEM_) (TIME) STEPCOUNTER_))
             (COND ((NULL VL_) (RETURN (REVERSE STATS))))
             (PROG ()
              WHILELABEL
               (COND ((NOT PDES) (RETURN NIL)))
               (PROGN
                (SETQ J 0)
                (SETQ EL NIL)
                (PROG (P)
                  (SETQ P PDES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (PROGN
                      (SETQ VL (GET P 'VARS))
                      (COND (VL (SETQ LE (LENGTH VL))) (T (SETQ LE 0)))
                      (COND
                       ((OR (AND (EQUAL J 0) (NULL VL)) (EQUAL J LE))
                        (SETQ EL (CONS P EL)))
                       ((LESSP J LE) (PROGN (SETQ J LE) (SETQ EL (LIST P)))))))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))
                (SETQ PDES (SETDIFF PDES EL))
                (SETQ LI (CONS (LIST (LENGTH EL) J) LI)))
               (GO WHILELABEL))
             (SETQ STATS (CONS LI STATS))
             (SETQ LI NIL)
             (PROG (F)
               (SETQ F FCTS)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F) (COND ((NOT (PAIRP F)) (SETQ FL (CONS F FL)))))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (COND
              (FL
               (PROGN
                (SETQ FL (FCTSORT (REVERSE FL)))
                (SETQ J (FCTLENGTH (CAR FL)))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (GEQ J 0)) (RETURN NIL)))
                  (PROGN
                   (SETQ N 0)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT (AND FL (EQUAL (FCTLENGTH (CAR FL)) J)))
                       (RETURN NIL)))
                     (PROGN (SETQ N (ADD1 N)) (SETQ FL (CDR FL)))
                     (GO WHILELABEL))
                   (SETQ LI (CONS (LIST N J) LI))
                   (SETQ J (COND (FL (FCTLENGTH (CAR FL))) (T (MINUS 1)))))
                  (GO WHILELABEL)))))
             (SETQ STATS (CONS LI STATS))
             (RETURN (REVERSE STATS)))))) 
(PUT 'SORT_DERIV_PDES 'NUMBER-OF-ARGS 1) 
(PUT 'SORT_DERIV_PDES 'DEFINED-ON-LINE '4136) 
(PUT 'SORT_DERIV_PDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SORT_DERIV_PDES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORT_DERIV_PDES (PDES)
    (PROG (MAX_NO_DERI CP PL RES)
      (SETQ MAX_NO_DERI 0)
      (SETQ CP PDES)
      (PROG ()
       WHILELABEL
        (COND ((NOT CP) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (GET (CAR CP) 'NO_DERIVS) MAX_NO_DERI)
           (SETQ MAX_NO_DERI (GET (CAR CP) 'NO_DERIVS))))
         (SETQ CP (CDR CP)))
        (GO WHILELABEL))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ PL NIL)
         (SETQ CP PDES)
         (PROG ()
          WHILELABEL
           (COND ((NOT CP) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (GET (CAR CP) 'NO_DERIVS) MAX_NO_DERI)
              (SETQ PL (CONS (CAR CP) PL))))
            (SETQ CP (CDR CP)))
           (GO WHILELABEL))
         (COND (PL (SETQ RES (CONS (CONS MAX_NO_DERI (REVERSE PL)) RES))))
         (SETQ PDES (SETDIFF PDES PL))
         (SETQ MAX_NO_DERI
                 (COND ((ZEROP MAX_NO_DERI) NIL) (T (SUB1 MAX_NO_DERI))))
         NIL)
        (COND ((NOT (OR (NULL MAX_NO_DERI) (NULL PDES))) (GO REPEATLABEL))))
      (RETURN RES))) 
(PUT 'PRINT_PDES 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_PDES 'DEFINED-ON-LINE '4160) 
(PUT 'PRINT_PDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_PDES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_PDES (PDES)
    (PROG (PL N PDECP)
      (TERPRI)
      (COND
       (PDES
        (PROGN
         (COND
          ((AND (NULL *BATCH_MODE) (LESSP BATCHCOUNT_ STEPCOUNTER_) (CDR PDES))
           (PROGN
            (SETQ N 1000000000)
            (COND
             (NIL
              (PROG ()
               REPEATLABEL
                (PROGN
                 (PROGN
                  (PRIN2
                   "What is the maximal number of terms of equations to be shown? ")
                  NIL)
                 (CHANGE_PROMPT_TO "")
                 (TERPRI)
                 (SETQ N (TERMREAD))
                 (RESTORE_INTERACTIVE_PROMPT))
                (COND ((NOT (FIXP N)) (GO REPEATLABEL))))))
            (PROG (PL)
              (SETQ PL PDES)
             LAB
              (COND ((NULL PL) (RETURN NIL)))
              ((LAMBDA (PL)
                 (COND ((LEQ (GET PL 'TERMS) N) (SETQ PDECP (CONS PL PDECP)))))
               (CAR PL))
              (SETQ PL (CDR PL))
              (GO LAB))
            (SETQ PDECP (REVERSE PDECP))
            NIL))
          (T (SETQ PDECP PDES)))
         (PROGN (PRIN2 "equations : ") NIL)
         (COND
          (STRUC_EQN
           (PROGN
            (SETQ PL (SORT_DERIV_PDES PDECP))
            (PROG ()
             WHILELABEL
              (COND ((NOT PL) (RETURN NIL)))
              (PROGN
               (TERPRI)
               (PROGN
                (PRIN2 (LENGTH (CDAR PL)))
                (PRIN2 " equations with ")
                (PRIN2 (CAAR PL))
                (PRIN2 " derivatives:")
                NIL)
               (TYPEEQLIST (CDAR PL))
               (SETQ PL (CDR PL)))
              (GO WHILELABEL))))
          (T (TYPEEQLIST PDECP)))))
       (T (PROGN (PROGN (PRIN2 "no equations") NIL) (TERPRI)))))) 
(PUT 'PRINT_INEQ 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_INEQ 'DEFINED-ON-LINE '4194) 
(PUT 'PRINT_INEQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_INEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_INEQ (INEQS)
    (PROG (A B C D H)
      (TERPRI)
      (COND
       ((CAR INEQS)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Non-vanishing expressions: ") NIL)
         (PROG (A)
           (SETQ A (CAR INEQS))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (COND ((NO_NUMBER_ATOM_SQ A) (SETQ C (CONS (CAAAR (CAR A)) C)))
                    (T (SETQ B (CONS (LIST '*SQ A T) B)))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (LISTPRINT C)
         (TERPRI)
         (PROG (A)
           (SETQ A B)
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (EQPRINT A)) (CAR A))
           (SETQ A (CDR A))
           (GO LAB)))))
      (COND
       ((CDR INEQS)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Lists with at least one non-vanishing sub-list ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "(ie. a sub-list of which no element vanishes.): ") NIL)
         (TERPRI)
         (PROG (A)
           (SETQ A (CDR INEQS))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (PROGN
               (PROGN (PRIN2 "{") NIL)
               (PROG (H)
                 (SETQ H A)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (PROGN (PRIN2 "{") NIL)
                     (SETQ C NIL)
                     (SETQ B NIL)
                     (PROG (D)
                       (SETQ D H)
                      LAB
                       (COND ((NULL D) (RETURN NIL)))
                       ((LAMBDA (D)
                          (COND
                           ((NO_NUMBER_ATOM_SQ D)
                            (SETQ C (CONS (CAAAR (CAR D)) C)))
                           (T (SETQ B (CONS (LIST '*SQ D T) B)))))
                        (CAR D))
                       (SETQ D (CDR D))
                       (GO LAB))
                     (LISTPRINT C)
                     (COND
                      ((NOT (NULL B))
                       (PROGN
                        (COND (C (PROGN (PROGN (PRIN2 ",") NIL) (TERPRI))))
                        (PROG (D)
                          (SETQ D B)
                         LAB
                          (COND ((NULL D) (RETURN NIL)))
                          ((LAMBDA (D) (EQPRINT D)) (CAR D))
                          (SETQ D (CDR D))
                          (GO LAB)))))
                     (PROGN (PRIN2 "}") NIL)
                     NIL))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (PROGN (PRIN2 "}") NIL)
               (TERPRI)
               NIL))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))))))) 
(PUT 'PRINT_FCTS 'NUMBER-OF-ARGS 2) 
(PUT 'PRINT_FCTS 'DEFINED-ON-LINE '4231) 
(PUT 'PRINT_FCTS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_FCTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINT_FCTS (PDES FCTS)
    (PROG (DFLIST DFS F P CP H HH SHOWCOEF)
      (PROG (H)
        (SETQ H FCTS)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H) (COND ((NOT (PAIRP H)) (SETQ HH (CONS H HH))))) (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (CHANGE_PROMPT_TO "")
      (SETQ FCTS (SELECT_FROM_LIST HH NIL))
      (SETQ PDES (SELECT_FROM_LIST PDES NIL))
      (PROGN
       (PRIN2
        "Do you want to see the coefficients of all derivatives in all equations")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "in factorized form which may take relatively much time? y/n")
       NIL)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (SETQ H (TERMREAD))
        (COND ((NOT (OR (EQUAL H 'Y) (EQUAL H 'N))) (GO REPEATLABEL))))
      (COND ((EQUAL H 'N) (SETQ SHOWCOEF NIL)) (T (SETQ SHOWCOEF T)))
      (RESTORE_INTERACTIVE_PROMPT)
      (PROG ()
       WHILELABEL
        (COND ((NOT FCTS) (RETURN NIL)))
        (COND ((PAIRP (CAR FCTS)) (SETQ FCTS (CDR FCTS)))
              (T
               (PROGN
                (SETQ F (CAR FCTS))
                (SETQ FCTS (CDR FCTS))
                (SETQ DFLIST NIL)
                (PROG (P)
                  (SETQ P PDES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (COND
                      ((NOT (FREEOF (GET P 'FCTS) F))
                       (PROGN
                        (SETQ DFS (GET P 'DERIVS))
                        (PROG ()
                         WHILELABEL
                          (COND ((NOT DFS) (RETURN NIL)))
                          (PROGN
                           (COND
                            ((EQUAL (CAAAR DFS) F)
                             (PROGN
                              (SETQ CP DFLIST)
                              (PROG ()
                               WHILELABEL
                                (COND
                                 ((NOT (AND CP (NEQ (CAAR CP) (CAAR DFS))))
                                  (RETURN NIL)))
                                (SETQ CP (CDR CP))
                                (GO WHILELABEL))
                              (COND
                               ((CDAAR DFS) (SETQ H (CONS 'DF (CAAR DFS))))
                               (T (SETQ H (CAAAR DFS))))
                              (COND
                               (SHOWCOEF
                                (COND
                                 ((NULL CP)
                                  (SETQ DFLIST
                                          (CONS
                                           (LIST (CAAR DFS)
                                                 (LIST 'LIST P
                                                       (ERR_CATCH_FAC
                                                        (COEFFN
                                                         (LIST '*SQ
                                                               (GET P 'SQVAL)
                                                               T)
                                                         H 1))))
                                           DFLIST)))
                                 (T
                                  (RPLACA CP
                                          (CONS (CAAR CP)
                                                (CONS
                                                 (LIST 'LIST P
                                                       (ERR_CATCH_FAC
                                                        (COEFFN
                                                         (LIST '*SQ
                                                               (GET P 'SQVAL)
                                                               T)
                                                         H 1)))
                                                 (CDAR CP)))))))
                               ((NULL CP)
                                (SETQ DFLIST
                                        (CONS (LIST (CAAR DFS) P) DFLIST)))
                               (T
                                (RPLACA CP
                                        (CONS (CAAR CP)
                                              (CONS P (CDAR CP)))))))))
                           (SETQ DFS (CDR DFS)))
                          (GO WHILELABEL))
                        NIL))))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT DFLIST) (RETURN NIL)))
                  (PROGN
                   (SETQ DFS (CAR DFLIST))
                   (SETQ DFLIST (CDR DFLIST))
                   (COND ((CDAR DFS) (SETQ H (CONS 'DF (CAR DFS))))
                         (T (SETQ H (CAAR DFS))))
                   (COND
                    (SHOWCOEF
                     (PROGN
                      (PROGN
                       (ASSGNPRI (AEVAL* H) NIL 'FIRST)
                       (ASSGNPRI (AEVAL* ": ") NIL NIL)
                       (ASSGNPRI (AEVAL* (CONS 'LIST (CDR DFS))) NIL 'LAST))))
                    (T
                     (PROGN
                      (PROGN (PRIN2 H) (PRIN2 ": ") NIL)
                      (PRINT (CDR DFS))
                      (TERPRI)))))
                  (GO WHILELABEL))
                NIL)))
        (GO WHILELABEL)))) 
(PUT 'PRINT_FORG 'NUMBER-OF-ARGS 2) 
(PUT 'PRINT_FORG 'DEFINED-ON-LINE '4290) 
(PUT 'PRINT_FORG 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_FORG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRINT_FORG (FCTS VL)
    (PROGN
     (COND
      (FSUB_
       (PROGN
        (TERPRI)
        (PROGN (PRIN2 "Eliminations not yet used for substitutions : ") NIL)
        (TERPRI)
        (PROG (P)
          (SETQ P FSUB_)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (PROGN
              (ASSGNPRI (AEVAL (CAR P)) NIL 'FIRST)
              (ASSGNPRI (AEVAL " = ") NIL NIL)
              (ASSGNPRI (REVAL1 (CDR P) T) NIL 'LAST)))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB)))))
     (COND
      (FCTS
       (PROGN
        (TERPRI)
        (PROGN (PRIN2 "Functions : ") NIL)
        (FCTPRINT_SQ FCTS)
        (TERPRI)
        (PROGN
         (PRIN2 "with ")
         (PRIN2
          (PROG (P FORALL-RESULT)
            (SETQ P FCTS)
            (SETQ FORALL-RESULT 0)
           LAB1
            (COND ((NULL P) (RETURN FORALL-RESULT)))
            (SETQ FORALL-RESULT
                    (PLUS
                     ((LAMBDA (P)
                        (COND
                         ((AND (PAIRP P) (EQUAL (CAR P) 'EQUAL))
                          (NO_OF_TM_SQ (CADDR P)))
                         (T 1)))
                      (CAR P))
                     FORALL-RESULT))
            (SETQ P (CDR P))
            (GO LAB1)))
         (PRIN2 " terms")
         NIL)
        (TERPRI)
        NIL)))
     (COND
      (VL (PROGN (TERPRI) (PROGN (PRIN2 "Variables : ") NIL) (FCTPRINT VL))))
     NIL)) 
(PUT 'PRINT_PDE_FORG_INEQ 'NUMBER-OF-ARGS 4) 
(PUT 'PRINT_PDE_FORG_INEQ 'DEFINED-ON-LINE '4308) 
(PUT 'PRINT_PDE_FORG_INEQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_PDE_FORG_INEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINT_PDE_FORG_INEQ (PDES INEQS FCTS VL)
    (COND
     (PRINT_
      (PROG ()
        (PRINT_PDES PDES)
        (PRINT_INEQ INEQS)
        (PRINT_FORG FCTS VL)
        (PRINT_STATISTIC PDES FCTS))))) 
(PUT 'NO_OF_TERMS 'NUMBER-OF-ARGS 1) 
(PUT 'NO_OF_TERMS 'DEFINED-ON-LINE '4317) 
(PUT 'NO_OF_TERMS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_TERMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_OF_TERMS (D)
    (COND ((NOT (PAIRP D)) (COND ((OR (NULL D) (ZEROP D)) 0) (T 1)))
          ((EQUAL (CAR D) 'PLUS) (DIFFERENCE (LENGTH D) 1))
          ((EQUAL (CAR D) 'EQUAL)
           (PLUS (NO_OF_TERMS (CADR D)) (NO_OF_TERMS (CADDR D))))
          ((OR (EQUAL (CAR D) 'MINUS) (EQUAL (CAR D) 'QUOTIENT))
           (NO_OF_TERMS (CADR D)))
          ((EQUAL (CAR D) 'EXPT)
           (COND ((OR (NOT (FIXP (CADDR D))) (LESSP (CADDR D) 2)) 1)
                 (T
                  (PROG (H M Q)
                    (SETQ M (DIFFERENCE (NO_OF_TERMS (CADR D)) 1))
                    (SETQ H 1)
                    (PROG (Q)
                      (SETQ Q 1)
                     LAB
                      (COND ((MINUSP (DIFFERENCE (CADDR D) Q)) (RETURN NIL)))
                      (SETQ H (TIMES H (QUOTIENT (PLUS M Q) Q)))
                      (SETQ Q (PLUS2 Q 1))
                      (GO LAB))
                    (RETURN H)))))
          ((EQUAL (CAR D) 'TIMES)
           (PROG (H R)
             (SETQ H 1)
             (PROG (R)
               (SETQ R (CDR D))
              LAB
               (COND ((NULL R) (RETURN NIL)))
               ((LAMBDA (R) (SETQ H (TIMES H (NO_OF_TERMS R)))) (CAR R))
               (SETQ R (CDR R))
               (GO LAB))
             (RETURN H)))
          (T 1))) 
(PUT 'NO_OF_TM_SF 'NUMBER-OF-ARGS 1) 
(PUT 'NO_OF_TM_SF 'DEFINED-ON-LINE '4340) 
(PUT 'NO_OF_TM_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_TM_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_OF_TM_SF (S)
    (COND ((NULL S) 0) ((OR (NOT (PAIRP S)) (NOT (PAIRP (CAR S)))) 1)
          (T (PLUS (NO_OF_TM_SF (CDAR S)) (NO_OF_TM_SF (CDR S)))))) 
(PUT 'NO_OF_TM_SF_LIMITED 'NUMBER-OF-ARGS 2) 
(PUT 'NO_OF_TM_SF_LIMITED 'DEFINED-ON-LINE '4348) 
(PUT 'NO_OF_TM_SF_LIMITED 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_TM_SF_LIMITED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NO_OF_TM_SF_LIMITED (S X)
    (COND ((NULL S) 0) ((OR (NOT (PAIRP S)) (NOT (PAIRP (CAR S)))) 1)
          (T
           (PROG (R)
             (SETQ R (NO_OF_TM_SF_LIMITED (CDAR S) X))
             (RETURN
              (COND ((GREATERP R X) R)
                    (T (PLUS R (NO_OF_TM_SF_LIMITED (CDR S) X))))))))) 
(PUT 'MORE_THAN_X_TERMS 'NUMBER-OF-ARGS 2) 
(PUT 'MORE_THAN_X_TERMS 'DEFINED-ON-LINE '4361) 
(PUT 'MORE_THAN_X_TERMS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MORE_THAN_X_TERMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MORE_THAN_X_TERMS (S X)
    (PROG (Y)
      (RETURN
       (COND ((NULL S) NIL)
             ((OR (NOT (PAIRP S)) (NOT (PAIRP (CAR S))))
              (COND ((EQUAL X 0) T) (T NIL)))
             (T
              (PROGN
               (SETQ Y (NO_OF_TM_SF_LIMITED (CDAR S) X))
               (COND
                ((LEQ Y X) (SETQ Y (PLUS Y (NO_OF_TM_SF_LIMITED (CDR S) X)))))
               (GREATERP Y X))))))) 
(PUT 'NO_OF_TM_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'NO_OF_TM_SQ 'DEFINED-ON-LINE '4388) 
(PUT 'NO_OF_TM_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_OF_TM_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_OF_TM_SQ (S)
    (PLUS (NO_OF_TM_SF (CAR S))
          (COND ((EQUAL (CDR S) 1) 0) (T (NO_OF_TM_SF (CDR S)))))) 
(PUT 'NO_NUMBER_ATOM_SF 'NUMBER-OF-ARGS 1) 
(PUT 'NO_NUMBER_ATOM_SF 'DEFINED-ON-LINE '4394) 
(PUT 'NO_NUMBER_ATOM_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_NUMBER_ATOM_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_NUMBER_ATOM_SF (SF)
    (COND
     ((AND (PAIRP SF) (NULL (CDR SF)) (EQUAL (CDAR SF) 1) (EQUAL (CDAAR SF) 1)
           (NULL (PAIRP (CAAAR SF))))
      T)
     (T NIL))) 
(PUT 'NO_NUMBER_ATOM_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'NO_NUMBER_ATOM_SQ 'DEFINED-ON-LINE '4402) 
(PUT 'NO_NUMBER_ATOM_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_NUMBER_ATOM_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_NUMBER_ATOM_SQ (SQ) (NO_NUMBER_ATOM_SF (CAR SQ))) 
(PUT 'ONE_TERMPSF 'NUMBER-OF-ARGS 1) 
(PUT 'ONE_TERMPSF 'DEFINED-ON-LINE '4405) 
(PUT 'ONE_TERMPSF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ONE_TERMPSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ONE_TERMPSF (SF)
    (COND ((OR (ATOM SF) (ATOM (CAR SF))) T) ((CDR SF) NIL)
          (T (ONE_TERMPSF (CDAR SF))))) 
(PUT 'FIRST_TERM_SF 'NUMBER-OF-ARGS 1) 
(PUT 'FIRST_TERM_SF 'DEFINED-ON-LINE '4410) 
(PUT 'FIRST_TERM_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FIRST_TERM_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIRST_TERM_SF (SF)
    (COND ((OR (ATOM SF) (ATOM (CAR SF))) SF)
          (T (LIST (CONS (CAAR SF) (FIRST_TERM_SF (CDAR SF))))))) 
(PUT 'NUM_TERM_SF 'NUMBER-OF-ARGS 1) 
(PUT 'NUM_TERM_SF 'DEFINED-ON-LINE '4417) 
(PUT 'NUM_TERM_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NUM_TERM_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_TERM_SF (SF)
    (COND
     (SF
      (COND ((OR (ATOM SF) (ATOM (CAR SF))) SF) (T (NUM_TERM_SF (CDR SF))))))) 
(PUT 'LMON_SF 'NUMBER-OF-ARGS 1) 
(PUT 'LMON_SF 'DEFINED-ON-LINE '4424) 
(PUT 'LMON_SF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LMON_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LMON_SF (SF)
    (COND ((OR (ATOM SF) (ATOM (CAR SF))) 1)
          (T (LIST (CONS (CAAR SF) (LMON_SF (CDAR SF))))))) 
(PUT 'NCO_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'NCO_SQ 'DEFINED-ON-LINE '4431) 
(PUT 'NCO_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NCO_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NCO_SQ (H)
    (PROG (D)
      (SETQ D (CDR H))
      (SETQ H (CAR H))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (PAIRP H) (NOT (OR (ATOM (CAR H)) (ATOM (CAR (CAR H)))))))
          (RETURN NIL)))
        (SETQ H (CDAR H))
        (GO WHILELABEL))
      (COND ((PAIRP H) (SETQ H (LIST 'QUOTIENT (CADR H) (CDDR H)))))
      (COND ((NEQ D 1) (SETQ H (LIST 'QUOTIENT H D))))
      (RETURN H))) 
(PUT 'NUMCOEFF 'PSOPFN 'NUMCO) 
(PUT 'NUMCO 'NUMBER-OF-ARGS 1) 
(PUT 'NUMCO 'DEFINED-ON-LINE '4445) 
(PUT 'NUMCO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NUMCO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMCO (H)
    (PROG ()
      (SETQ H (CAR (CADR (REVAL1 (CAR H) NIL))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (PAIRP H) (NOT (OR (ATOM (CAR H)) (ATOM (CAR (CAR H)))))))
          (RETURN NIL)))
        (SETQ H (CDAR H))
        (GO WHILELABEL))
      (COND ((PAIRP H) (SETQ H (LIST 'QUOTIENT (CADR H) (CDDR H)))))
      (RETURN H))) 
(PUT 'NON_NEGATIVE 'NUMBER-OF-ARGS 1) 
(PUT 'NON_NEGATIVE 'DEFINED-ON-LINE '4454) 
(PUT 'NON_NEGATIVE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NON_NEGATIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NON_NEGATIVE (EXF)
    (OR (NULL EXF) (AND (OR (ATOM EXF) (ATOM (CAR EXF))) (PLUSP EXF))
        (AND (NULL (OR (ATOM EXF) (ATOM (CAR EXF))))
             (AND (FIXP (CDAAR EXF)) (EVENP (CDAAR EXF)))
             (OR
              (AND (OR (ATOM (CDAR EXF)) (ATOM (CAR (CDAR EXF))))
                   (PLUSP (CDAR EXF)))
              (AND (NULL (OR (ATOM (CDAR EXF)) (ATOM (CAR (CDAR EXF)))))
                   (NON_NEGATIVE (CDAR EXF))))
             (NON_NEGATIVE (CDR EXF))))) 
(PUT 'MYMEMQ 'NUMBER-OF-ARGS 3) 
(PUT 'MYMEMQ 'DEFINED-ON-LINE '4466) 
(PUT 'MYMEMQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MYMEMQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MYMEMQ (U V V1)
    (COND ((NOT (PAIRP V)) NIL)
          ((EQ U (CAR V)) (PROGN (COND (V1 (RPLACD V1 NIL))) V))
          (T (MYMEMQ U (CDR V) V)))) 
(PUT 'SQ*CONS 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*CONS 'DEFINED-ON-LINE '4479) 
(PUT 'SQ*CONS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*CONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*CONS (X)
    (PROGN
     (CONS 'LIST (CONS (REVAL1 (CAR X) NIL) (CDR (REVAL1 (CADR X) NIL)))))) 
(PUT 'SQCONS 'PSOPFN 'SQ*CONS) 
(PUT 'SQ*LENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*LENGTH 'DEFINED-ON-LINE '4484) 
(PUT 'SQ*LENGTH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*LENGTH (X) (DIFFERENCE (LENGTH (REVAL1 (CAR X) NIL)) 1)) 
(PUT 'SQLENGTH 'PSOPFN 'SQ*LENGTH) 
(PUT 'SQ*REST 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*REST 'DEFINED-ON-LINE '4489) 
(PUT 'SQ*REST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*REST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*REST (X) (PROGN (CONS 'LIST (CDDR (REVAL1 (CAR X) NIL))))) 
(PUT 'SQREST 'PSOPFN 'SQ*REST) 
(PUT 'SQ*FIRST 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*FIRST 'DEFINED-ON-LINE '4494) 
(PUT 'SQ*FIRST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*FIRST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*FIRST (X) (CADR (REVAL1 (CAR X) NIL))) 
(PUT 'SQFIRST 'PSOPFN 'SQ*FIRST) 
(PUT 'SQ*SECOND 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*SECOND 'DEFINED-ON-LINE '4499) 
(PUT 'SQ*SECOND 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*SECOND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*SECOND (X) (CADDR (REVAL1 (CAR X) NIL))) 
(PUT 'SQSECOND 'PSOPFN 'SQ*SECOND) 
(PUT 'SQ*THIRD 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*THIRD 'DEFINED-ON-LINE '4504) 
(PUT 'SQ*THIRD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*THIRD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*THIRD (X) (CADDDR (REVAL1 (CAR X) NIL))) 
(PUT 'SQTHIRD 'PSOPFN 'SQ*THIRD) 
(PUT 'SQ*PART 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*PART 'DEFINED-ON-LINE '4509) 
(PUT 'SQ*PART 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*PART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*PART (X)
    (PROG (C1 C2)
      (SETQ C1 (REVAL1 (CAR X) NIL))
      (SETQ C2 (REVAL1 (CADR X) NIL))
      (RETURN
       (COND ((AND (EQUAL C2 0) (NOT (PAIRP C1))) (MINUS 1))
             (T (NTH C1 (ADD1 C2))))))) 
(PUT 'SQPART 'PSOPFN 'SQ*PART) 
(PUT 'SQ*REVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*REVERSE 'DEFINED-ON-LINE '4523) 
(PUT 'SQ*REVERSE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*REVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*REVERSE (X) (PROGN (CONS 'LIST (REVERSE (CDR (REVAL1 (CAR X) NIL)))))) 
(PUT 'SQREVERSE 'PSOPFN 'SQ*REVERSE) 
(PUT 'SQ*APPEND 'NUMBER-OF-ARGS 1) 
(PUT 'SQ*APPEND 'DEFINED-ON-LINE '4528) 
(PUT 'SQ*APPEND 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SQ*APPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ*APPEND (X)
    (PROGN
     (CONS 'LIST
           (APPEND (CDR (REVAL1 (CAR X) NIL)) (CDR (REVAL1 (CADR X) NIL)))))) 
(PUT 'SQAPPEND 'PSOPFN 'SQ*APPEND) 
(PUT 'DELENGTHSF 'NUMBER-OF-ARGS 1) 
(PUT 'DELENGTHSF 'DEFINED-ON-LINE '4534) 
(PUT 'DELENGTHSF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELENGTHSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELENGTHSF (D)
    (COND
     ((OR (NOT (PAIRP D)) (NOT (PAIRP (CAR D))) (NOT (PAIRP (CAAR D))))
      (COND ((OR (ATOM D) (ATOM (CAR D))) 0) (T 1)))
     (T (PLUS (CDAAR D) (DELENGTHSF (CDAR D)) (DELENGTHSF (CDR D)))))) 
(PUT 'DELENGTHSQ 'NUMBER-OF-ARGS 1) 
(PUT 'DELENGTHSQ 'DEFINED-ON-LINE '4541) 
(PUT 'DELENGTHSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELENGTHSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELENGTHSQ (D)
    (PLUS (COND ((EQUAL (CAR D) 1) 0) (T (DELENGTHSF (CAR D))))
          (COND ((EQUAL (CDR D) 1) 0) (T (DELENGTHSF (CDR D)))))) 
(PUT 'DELENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'DELENGTH 'DEFINED-ON-LINE '4546) 
(PUT 'DELENGTH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELENGTH (D)
    (COND ((NOT (PAIRP D)) (COND (D 1) (T 0)))
          ((OR (EQUAL (CAR D) 'PLUS) (EQUAL (CAR D) 'TIMES)
               (EQUAL (CAR D) 'QUOTIENT) (EQUAL (CAR D) 'MINUS)
               (EQUAL (CAR D) 'EQUAL))
           (PROG (A FORALL-RESULT)
             (SETQ A (CDR D))
             (SETQ FORALL-RESULT 0)
            LAB1
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT
                     (PLUS ((LAMBDA (A) (DELENGTH A)) (CAR A)) FORALL-RESULT))
             (SETQ A (CDR A))
             (GO LAB1)))
          (T 1))) 
(PUT 'PDEWEIGHTSF 'NUMBER-OF-ARGS 2) 
(PUT 'PDEWEIGHTSF 'DEFINED-ON-LINE '4557) 
(PUT 'PDEWEIGHTSF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PDEWEIGHTSF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PDEWEIGHTSF (D FTEM)
    (COND ((OR (NULL D) (EQUAL D 1) (EQUAL D 0)) 0)
          ((OR (NOT (PAIRP D)) (NOT (PAIRP (CAR D))) (NOT (PAIRP (CAAR D)))) 1)
          ((FREEOFLIST (CAAAR D) FTEM)
           (PLUS (PDEWEIGHTSF (CDAR D) FTEM) (PDEWEIGHTSF (CDR D) FTEM)))
          (T
           (PLUS (CDAAR D) (PDEWEIGHTSF (CDAR D) FTEM)
                 (PDEWEIGHTSF (CDR D) FTEM))))) 
(PUT 'PDEWEIGHT 'NUMBER-OF-ARGS 2) 
(PUT 'PDEWEIGHT 'DEFINED-ON-LINE '4570) 
(PUT 'PDEWEIGHT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PDEWEIGHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PDEWEIGHT (D FTEM)
    (COND ((NOT (SMEMBERL FTEM D)) 0) ((NOT (PAIRP D)) 1)
          ((OR (EQUAL (CAR D) 'PLUS) (EQUAL (CAR D) 'TIMES)
               (EQUAL (CAR D) 'EQUAL) (EQUAL (CAR D) 'QUOTIENT))
           (PROG (A FORALL-RESULT)
             (SETQ A (CDR D))
             (SETQ FORALL-RESULT 0)
            LAB1
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT
                     (PLUS ((LAMBDA (A) (PDEWEIGHT A FTEM)) (CAR A))
                           FORALL-RESULT))
             (SETQ A (CDR A))
             (GO LAB1)))
          ((EQUAL (CAR D) 'EXPT)
           (COND
            ((NUMBERP (CADDR D)) (TIMES (CADDR D) (PDEWEIGHT (CADR D) FTEM)))
            (T (PLUS (PDEWEIGHT (CADDR D) FTEM) (PDEWEIGHT (CADR D) FTEM)))))
          ((EQUAL (CAR D) 'MINUS) (PDEWEIGHT (CADR D) FTEM)) (T 1))) 
(PUT 'DESORT 'NUMBER-OF-ARGS 1) 
(PUT 'DESORT 'DEFINED-ON-LINE '4585) 
(PUT 'DESORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DESORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DESORT (L)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A
              (IDX_SORT
               (PROG (B FORALL-RESULT FORALL-ENDPTR)
                 (SETQ B L)
                 (COND ((NULL B) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (B) (CONS (DELENGTH B) B)) (CAR B))
                                  NIL)))
                LOOPLABEL
                 (SETQ B (CDR B))
                 (COND ((NULL B) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (B) (CONS (DELENGTH B) B)) (CAR B))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL)))
     LOOPLABEL
      (SETQ A (CDR A))
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (A) (CDR A)) (CAR A)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'IDX_SORT 'NUMBER-OF-ARGS 1) 
(PUT 'IDX_SORT 'DEFINED-ON-LINE '4591) 
(PUT 'IDX_SORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'IDX_SORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDX_SORT (L)
    (COND ((NULL L) NIL)
          (T
           (PROG (L1 L2 L3 M N)
             (RETURN
              (PROGN
               (SETQ N (CAAR L))
               (SETQ L2 (LIST (CAR L)))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (SETQ M (CAAR L))
                  (COND ((LESSP M N) (SETQ L1 (CONS (CAR L) L1)))
                        ((GREATERP M N) (SETQ L3 (CONS (CAR L) L3)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (IDX_SORT L1) (APPEND L2 (IDX_SORT L3))))))))) 
(PUT 'REV_IDX_SORT 'NUMBER-OF-ARGS 1) 
(PUT 'REV_IDX_SORT 'DEFINED-ON-LINE '4610) 
(PUT 'REV_IDX_SORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'REV_IDX_SORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REV_IDX_SORT (L)
    (COND ((NULL L) NIL)
          (T
           (PROG (L1 L2 L3 M N)
             (RETURN
              (PROGN
               (SETQ N (CAAR L))
               (SETQ L2 (LIST (CAR L)))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (SETQ M (CAAR L))
                  (COND ((GREATERP M N) (SETQ L1 (CONS (CAR L) L1)))
                        ((LESSP M N) (SETQ L3 (CONS (CAR L) L3)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (REV_IDX_SORT L1) (APPEND L2 (REV_IDX_SORT L3))))))))) 
(PUT 'RAT_IDX_SORT 'NUMBER-OF-ARGS 1) 
(PUT 'RAT_IDX_SORT 'DEFINED-ON-LINE '4629) 
(PUT 'RAT_IDX_SORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RAT_IDX_SORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAT_IDX_SORT (L)
    (PROG (L1 L2 L3 M N)
      (RETURN
       (COND ((NULL L) NIL)
             (T
              (PROGN
               (SETQ N (CAAR L))
               (SETQ L2 (LIST (CAR L)))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (SETQ M (CAAR L))
                  (COND ((RATIONAL_LESS M N) (SETQ L1 (CONS (CAR L) L1)))
                        ((RATIONAL_LESS N M) (SETQ L3 (CONS (CAR L) L3)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (APPEND (RAT_IDX_SORT L1) (APPEND L2 (RAT_IDX_SORT L3))))))))) 
(PUT 'SORT_EQ_BY_LENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'SORT_EQ_BY_LENGTH 'DEFINED-ON-LINE '4649) 
(PUT 'SORT_EQ_BY_LENGTH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SORT_EQ_BY_LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SORT_EQ_BY_LENGTH (PDES)
    (PROGN
     (SETQ LARGEST_FULLY_SHORTENED NIL)
     (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN NIL)
     (PROG (P FORALL-RESULT FORALL-ENDPTR)
       (SETQ P
               (IDX_SORT
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P PDES)
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P) (CONS (GET P 'TERMS) P))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (P) (CONS (GET P 'TERMS) P)) (CAR P))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (COND ((NULL P) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL)))
      LOOPLABEL
       (SETQ P (CDR P))
       (COND ((NULL P) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'UPDATE_EQ_SORT_BY_LENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'UPDATE_EQ_SORT_BY_LENGTH 'DEFINED-ON-LINE '4656) 
(PUT 'UPDATE_EQ_SORT_BY_LENGTH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UPDATE_EQ_SORT_BY_LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UPDATE_EQ_SORT_BY_LENGTH (PDES)
    (COND ((OR (NULL PDES) (NULL (CDR PDES))) PDES)
          (T
           (PROG (P Q CARPT CADRPT CADRP)
             (SETQ P PDES)
             (SETQ CARPT (GET (CAR P) 'TERMS))
             (PROG ()
              WHILELABEL
               (COND ((NOT (CDR P)) (RETURN NIL)))
               (PROGN
                (SETQ CADRPT (GET (CADR P) 'TERMS))
                (COND
                 ((LEQ CARPT CADRPT)
                  (PROGN (SETQ CARPT CADRPT) (SETQ P (CDR P))))
                 (T
                  (PROGN
                   (SETQ CADRP (CADR P))
                   (RPLACD P (CDDR P))
                   (COND
                    ((LEQ CADRPT (GET (CAR PDES) 'TERMS))
                     (SETQ PDES (CONS CADRP PDES)))
                    (T
                     (PROGN
                      (SETQ Q PDES)
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT
                           (AND (CDR Q)
                                (GREATERP CADRPT (GET (CADR Q) 'TERMS))))
                          (RETURN NIL)))
                        (SETQ Q (CDR Q))
                        (GO WHILELABEL))
                      (RPLACD Q (CONS CADRP (CDR Q)))
                      NIL)))))))
               (GO WHILELABEL))
             (RETURN PDES))))) 
(PUT 'KERNEL_SORT 'NUMBER-OF-ARGS 1) 
(PUT 'KERNEL_SORT 'DEFINED-ON-LINE '4682) 
(PUT 'KERNEL_SORT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'KERNEL_SORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KERNEL_SORT (L)
    (COND ((NULL L) NIL) ((NULL (CDR L)) L)
          (T
           (PROG (N L1 L2)
             (RETURN
              (PROGN
               (SETQ N (CAR L))
               (SETQ L2 (LIST N))
               (SETQ L (CDR L))
               (PROG ()
                WHILELABEL
                 (COND ((NOT L) (RETURN NIL)))
                 (PROGN
                  (COND ((ORDP (CAR L) N) (SETQ L1 (CONS (CAR L) L1)))
                        (T (SETQ L2 (CONS (CAR L) L2))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (NCONC (KERNEL_SORT L1) (KERNEL_SORT L2)))))))) 
(PUT 'ARGSET 'NUMBER-OF-ARGS 1) 
(PUT 'ARGSET 'DEFINED-ON-LINE '4701) 
(PUT 'ARGSET 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ARGSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ARGSET (FTEM)
    (COND (FTEM (UNION (REVERSE (FCTARGS (CAR FTEM))) (ARGSET (CDR FTEM))))
          (T NIL))) 
(PUT 'NO_FNC_OF_V 'NUMBER-OF-ARGS 0) 
(PUT 'NO_FNC_OF_V 'DEFINED-ON-LINE '4706) 
(PUT 'NO_FNC_OF_V 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NO_FNC_OF_V 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NO_FNC_OF_V NIL
    (PROG (VL V NOFU F NV)
      (SETQ VL (ARGSET FTEM_))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ NOFU 0)
            (PROG (F)
              (SETQ F FTEM_)
             LAB
              (COND ((NULL F) (RETURN NIL)))
              ((LAMBDA (F)
                 (COND ((NOT (FREEOF (FCTARGS F) V)) (SETQ NOFU (ADD1 NOFU)))))
               (CAR F))
              (SETQ F (CDR F))
              (GO LAB))
            (SETQ NV (CONS (CONS V NOFU) NV))
            NIL))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN NV))) 
(PUT 'PUSH_VARS 'NUMBER-OF-ARGS 1) 
(PUT 'PUSH_VARS 'DEFINED-ON-LINE '4719) 
(PUT 'PUSH_VARS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PUSH_VARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PUSH_VARS (LISTE)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X LISTE)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (COND ((NOT (BOUNDP X)) X) (T (EVAL X))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X) (COND ((NOT (BOUNDP X)) X) (T (EVAL X)))) (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'BACKUP_PDES 'NUMBER-OF-ARGS 2) 
(PUT 'BACKUP_PDES 'DEFINED-ON-LINE '4723) 
(PUT 'BACKUP_PDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'BACKUP_PDES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BACKUP_PDES (PDES FORG)
    (PROG (ALLFL)
      (RETURN
       (LIST (PUSH_VARS NOT_PASSED_BACK)
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P PDES)
               (COND ((NULL P) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P)
                                   (LIST P
                                         (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ Q PROP_LIST)
                                           (COND ((NULL Q) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (Q)
                                                               (CONS Q
                                                                     (GET P
                                                                          Q)))
                                                             (CAR Q))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ Q (CDR Q))
                                           (COND
                                            ((NULL Q) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (Q)
                                                       (CONS Q (GET P Q)))
                                                     (CAR Q))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))
                                         (PROGN
                                          (SETQ ALLFL NIL)
                                          (PROG (Q)
                                            (SETQ Q ALLFLAGS_)
                                           LAB
                                            (COND ((NULL Q) (RETURN NIL)))
                                            ((LAMBDA (Q)
                                               (COND
                                                ((FLAGP P Q)
                                                 (SETQ ALLFL (CONS Q ALLFL)))))
                                             (CAR Q))
                                            (SETQ Q (CDR Q))
                                            (GO LAB))
                                          ALLFL)))
                                 (CAR P))
                                NIL)))
              LOOPLABEL
               (SETQ P (CDR P))
               (COND ((NULL P) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P)
                           (LIST P
                                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Q PROP_LIST)
                                   (COND ((NULL Q) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (Q)
                                                       (CONS Q (GET P Q)))
                                                     (CAR Q))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ Q (CDR Q))
                                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Q) (CONS Q (GET P Q)))
                                             (CAR Q))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 (PROGN
                                  (SETQ ALLFL NIL)
                                  (PROG (Q)
                                    (SETQ Q ALLFLAGS_)
                                   LAB
                                    (COND ((NULL Q) (RETURN NIL)))
                                    ((LAMBDA (Q)
                                       (COND
                                        ((FLAGP P Q)
                                         (SETQ ALLFL (CONS Q ALLFL)))))
                                     (CAR Q))
                                    (SETQ Q (CDR Q))
                                    (GO LAB))
                                  ALLFL)))
                         (CAR P))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F FORG)
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (F)
                                   (COND
                                    ((PAIRP F) (CONS F (GET (CADR F) 'FCTS)))
                                    (T (CONS F (GET F 'FCTS)))))
                                 (CAR F))
                                NIL)))
              LOOPLABEL
               (SETQ F (CDR F))
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (F)
                           (COND ((PAIRP F) (CONS F (GET (CADR F) 'FCTS)))
                                 (T (CONS F (GET F 'FCTS)))))
                         (CAR F))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (PROG (ID FORALL-RESULT FORALL-ENDPTR)
               (SETQ ID IDNTIES_)
               (COND ((NULL ID) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ID)
                                   (LIST ID (GET ID 'VAL) (FLAGP ID 'TO_INT)
                                         (FLAGP ID 'TO_SUBST)))
                                 (CAR ID))
                                NIL)))
              LOOPLABEL
               (SETQ ID (CDR ID))
               (COND ((NULL ID) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (ID)
                           (LIST ID (GET ID 'VAL) (FLAGP ID 'TO_INT)
                                 (FLAGP ID 'TO_SUBST)))
                         (CAR ID))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'POP_VARS 'NUMBER-OF-ARGS 2) 
(PUT 'POP_VARS 'DEFINED-ON-LINE '4763) 
(PUT 'POP_VARS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'POP_VARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POP_VARS (LISTE ALTEWERTE)
    (PROG (X)
      (SETQ X LISTE)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X)
         (PROGN (SET X (CAR ALTEWERTE)) (SETQ ALTEWERTE (CDR ALTEWERTE))))
       (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'RESTORE_PDES 'NUMBER-OF-ARGS 1) 
(PUT 'RESTORE_PDES 'DEFINED-ON-LINE '4767) 
(PUT 'RESTORE_PDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RESTORE_PDES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESTORE_PDES (BAK)
    (PROG (PDES FORG *COMPLEX_BAK MODULAR_COMP_BAK)
      (SETQ *COMPLEX_BAK *COMPLEX)
      (SETQ MODULAR_COMP_BAK MODULAR_COMP)
      (POP_VARS NOT_PASSED_BACK (CAR BAK))
      (COND
       ((AND *COMPLEX_BAK (NULL *COMPLEX))
        (PROGN
         (PROGN
          (PRIN2
           "### WARNING: You were currently in a session with ON COMPLEX and")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    now loaded a backed up session with OFF COMPLEX. If you want")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    to do anything with the data/solutions just computed under")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    ON COMPLEX in the loaded session with OFF COMPLEX then better")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "    switch ON COMPLEX now.") NIL)
         (TERPRI))))
      (COND
       ((AND MODULAR_COMP_BAK (NULL MODULAR_COMP))
        (PROGN
         (PROGN
          (PRIN2
           "### WARNING: You were currently in a session which did computations")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    with ON MODULAR and now loaded a backed up session with OFF MODULAR.")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    If you want to do anything with the data/solutions just computed")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    under ON MODULAR in the loaded session with calculations done under")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "    OFF MODULAR then better do the interactive crack command MO now.")
          NIL)
         (TERPRI))))
      (COND (*COMPLEX (ON (LIST 'COMPLEX))) (T (OFF (LIST 'COMPLEX))))
      (COND (MODULAR_COMP (SETMOD MODULAR_COMP)))
      (COND
       ((AND *COMPLEX_BAK (NULL *COMPLEX))
        (PROGN (SETQ *COMPLEX T) (AEVAL (ON (LIST 'COMPLEX))) NIL)))
      (COND
       ((AND MODULAR_COMP_BAK (NULL MODULAR_COMP))
        (PROGN (SETMOD MODULAR_COMP))))
      (PROG (C)
        (SETQ C (CADR BAK))
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ PDES (CONS (CAR C) PDES))
            (PROG (S)
              (SETQ S (CADR C))
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S) (PUT (CAR C) (CAR S) (CDR S))) (CAR S))
              (SETQ S (CDR S))
              (GO LAB))
            (PROG (S)
              (SETQ S (CADDR C))
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S) (FLAG (LIST (CAR C)) S)) (CAR S))
              (SETQ S (CDR S))
              (GO LAB))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (PROG (C)
        (SETQ C (CADDR BAK))
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ FORG (CONS (CAR C) FORG))
            (COND ((PAIRP (CAR C)) (PUT (CADAR C) 'FCTS (CDR C))))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (COND
       ((CDDDR BAK)
        (PROG (C)
          (SETQ C (CADDDR BAK))
         LAB
          (COND ((NULL C) (RETURN NIL)))
          ((LAMBDA (C)
             (PROGN
              (PUT (CAR C) 'VAL (CADR C))
              (COND ((CADDR C) (FLAG (LIST (CAR C)) 'TO_INT))
                    ((FLAGP (CAR C) 'TO_INT) (REMFLAG (CAR C) 'TO_INT)))
              (COND ((CADDR C) (FLAG (LIST (CAR C)) 'TO_SUBST))
                    ((FLAGP (CAR C) 'TO_SUBST) (REMFLAG (CAR C) 'TO_SUBST)))
              NIL))
           (CAR C))
          (SETQ C (CDR C))
          (GO LAB))))
      (UNIQUIFYALL PDES FORG)
      (RETURN (LIST (REVERSE PDES) (REVERSE FORG))))) 
(PUT 'DELETEPDE 'NUMBER-OF-ARGS 1) 
(PUT 'DELETEPDE 'DEFINED-ON-LINE '4849) 
(PUT 'DELETEPDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETEPDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETEPDE (PDES)
    (PROG (S L)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN (PRIN2 "Equations to be deleted: ") NIL)
      (SETQ L (SELECT_FROM_LIST PDES NIL))
      (RESTORE_INTERACTIVE_PROMPT)
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND ((MEMBER S PDES) (SETQ PDES (DROP_PDE S PDES NIL)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (F_UPDATE PDES NIL)
      (RETURN PDES))) 
(PUT 'NEW_PDE 'NUMBER-OF-ARGS 0) 
(PUT 'NEW_PDE 'DEFINED-ON-LINE '4862) 
(PUT 'NEW_PDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NEW_PDE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEW_PDE NIL
    (PROG (S)
      (COND
       ((AND (NULL (CAR RECYCLE_EQNS)) (CDR RECYCLE_EQNS))
        (CLEAN_PROP_LIST (CDR RECYCLE_EQNS))))
      (COND
       ((NULL (CAR RECYCLE_EQNS))
        (PROGN (SETQ S (MKID EQNAME_ NEQU_)) (SETQ NEQU_ (ADD1 NEQU_)) NIL))
       (T
        (PROGN
         (SETQ S (CAAR RECYCLE_EQNS))
         (SETQ RECYCLE_EQNS (CONS (CDAR RECYCLE_EQNS) (CDR RECYCLE_EQNS))))))
      (SETPROP S NIL)
      (RETURN S))) 
(PUT 'DROP_PDE_FROM_PROPERTIES 'NUMBER-OF-ARGS 2) 
(PUT 'DROP_PDE_FROM_PROPERTIES 'DEFINED-ON-LINE '4879) 
(PUT 'DROP_PDE_FROM_PROPERTIES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_PDE_FROM_PROPERTIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DROP_PDE_FROM_PROPERTIES (P PDES)
    (PROG ()
      (PUT P 'DEC_WITH NIL)
      (PUT P 'DEC_WITH_RL NIL)
      (PUT P 'RL_WITH NIL)
      (PROG (Q)
        (SETQ Q PDES)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (COND
            ((NEQ Q P)
             (PROGN
              (DROP_DEC_WITH P Q T)
              (DROP_DEC_WITH P Q NIL)
              (DROP_RL_WITH P Q)))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB)))) 
(PUT 'DROP_PDE_FROM_IDTIES 'NUMBER-OF-ARGS 3) 
(PUT 'DROP_PDE_FROM_IDTIES 'DEFINED-ON-LINE '4891) 
(PUT 'DROP_PDE_FROM_IDTIES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_PDE_FROM_IDTIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROP_PDE_FROM_IDTIES (P PDES PHIST)
    (PROG (Q NEWIDVAL IDNT)
      (PROG (Q)
        (SETQ Q PDES)
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (COND
            ((NEQ Q P)
             (COND
              ((NOT (FREEOF (GET Q 'HISTRY_) P))
               (PUT Q 'HISTRY_
                    (COND ((NULL PHIST) Q)
                          (T (SUBST PHIST P (GET Q 'HISTRY_))))))))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (COND
       ((AND RECORD_HIST (GETD 'SHOW_ID))
        (PROGN
         (SETQ IDNT IDNTIES_)
         (PROG ()
          WHILELABEL
           (COND ((NOT IDNT) (RETURN NIL)))
           (PROGN
            (COND
             ((NOT (FREEOF (GET (CAR IDNT) 'VAL) P))
              (COND ((NULL PHIST) (DROP_IDTY (CAR IDNT)))
                    (T
                     (PROGN
                      (SETQ NEWIDVAL
                              (REVAL1 (SUBST PHIST P (GET (CAR IDNT) 'VAL)) T))
                      (COND
                       ((TRIVIAL_IDTY PDES NEWIDVAL) (DROP_IDTY (CAR IDNT)))
                       (T
                        (PROGN
                         (PUT (CAR IDNT) 'VAL NEWIDVAL)
                         (FLAG (LIST (CAR IDNT)) 'TO_SUBST)
                         (FLAG (LIST (CAR IDNT)) 'TO_INT)))))))))
            (SETQ IDNT (CDR IDNT)))
           (GO WHILELABEL))
         (COND
          ((AND PHIST (NOT (ZEROP PHIST)) (NEQ P (GET P 'HISTRY_)))
           (PROGN
            (SETQ IDNT
                    (REVAL1 (LIST 'PLUS (GET P 'HISTRY_) (LIST 'MINUS PHIST))
                            T))
            (COND
             ((AND (PAIRP IDNT) (EQUAL (CAR IDNT) 'QUOTIENT))
              (SETQ IDNT (CADR IDNT))))
            (COND
             ((NOT (ZEROP IDNT))
              (NEW_IDTY IDNT PDES (COND (PDES T) (T NIL))))))))))))) 
(PUT 'DROP_PDE 'NUMBER-OF-ARGS 3) 
(PUT 'DROP_PDE 'DEFINED-ON-LINE '4934) 
(PUT 'DROP_PDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_PDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DROP_PDE (P PDES PHIST)
    (COND
     (P
      (PROG (L)
        (COND
         ((AND DO_RECYCLE_EQN (FREEOF (CAR RECYCLE_EQNS) P))
          (SETQ RECYCLE_EQNS
                  (CONS (CAR RECYCLE_EQNS)
                        (UNION (LIST P) (CDR RECYCLE_EQNS))))))
        (SETQ DEPL* (DELETE (ASSOC (REVAL1 P T) DEPL*) DEPL*))
        (DROP_PDE_FROM_IDTIES P PDES PHIST)
        (SETPROP P NIL)
        (COND
         ((OR (EQUAL P LARGEST_FULLY_SHORTENED)
              (EQUAL P CURRENTLY_TO_BE_SUBSTITUTED_IN))
          (COND
           ((OR (NULL PDES) (EQUAL P (CAR PDES)))
            (PROGN
             (COND
              ((EQUAL P LARGEST_FULLY_SHORTENED)
               (SETQ LARGEST_FULLY_SHORTENED NIL)))
             (COND
              ((EQUAL P CURRENTLY_TO_BE_SUBSTITUTED_IN)
               (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN NIL)))))
           (T
            (PROGN
             (SETQ L PDES)
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND (CDR L) (NEQ P (CADR L)))) (RETURN NIL)))
               (SETQ L (CDR L))
               (GO WHILELABEL))
             (COND
              ((EQUAL P LARGEST_FULLY_SHORTENED)
               (SETQ LARGEST_FULLY_SHORTENED (CAR L))))
             (COND
              ((EQUAL P CURRENTLY_TO_BE_SUBSTITUTED_IN)
               (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN
                       (COND ((AND (CDR L) (CDDR L)) (CADDR L))
                             (T (CAR L)))))))))))
        (RETURN (DELETE P PDES)))))) 
(PUT 'DROP_ALL_PDES 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_ALL_PDES 'DEFINED-ON-LINE '4963) 
(PUT 'DROP_ALL_PDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DROP_ALL_PDES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_ALL_PDES (PDES)
    (PROG (P)
      (COND
       (DO_RECYCLE_EQN
        (SETQ RECYCLE_EQNS
                (CONS (UNION PDES (CAR RECYCLE_EQNS))
                      (SETDIFF (CDR RECYCLE_EQNS) PDES)))))
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ DEPL* (DELETE (ASSOC (REVAL1 P T) DEPL*) DEPL*))
            (SETPROP P NIL)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT IDNTIES_) (RETURN NIL)))
        (DROP_IDTY (CAR IDNTIES_))
        (GO WHILELABEL)))) 
(PUT 'CHANGE_PDE_FLAG 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE_PDE_FLAG 'DEFINED-ON-LINE '4976) 
(PUT 'CHANGE_PDE_FLAG 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHANGE_PDE_FLAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE_PDE_FLAG (PDES)
    (PROG (P TY H)
      (TERPRI)
      (PROGN (PRIN2 "At first we need the list of equations for which ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "you want to change properties.") NIL)
      (SETQ PDES (SELECT_FROM_LIST PDES NIL))
      (TERPRI)
      (PROGN
       (PRIN2 "Type in one of the following flags that is to be flipped ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "(e.g. to_int <ENTER>): ") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 ALLFLAGS_) NIL)
      (TERPRI)
      (TERPRI)
      (PROGN
       (PRIN2
        "or type in one of the following properties that is to be changed")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "(e.g. vars <ENTER>): ") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 PROP_LIST) NIL)
      (TERPRI)
      (TERPRI)
      (CHANGE_PROMPT_TO "")
      (SETQ TY (TERMREAD))
      (COND
       ((MEMBER TY ALLFLAGS_)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Shall the flag be set (Y) ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "or be removed ?       (N) ") NIL)
         (SETQ H (TERMREAD))
         (PROG (P)
           (SETQ P PDES)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND ((EQUAL H 'Y) (FLAG (LIST P) TY))
                    (T (REMFLAG (LIST P) TY))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))))
       ((MEMBER TY PROP_LIST)
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2
           "Shall the property list for all selected equations be set to nil (Y/N) ")
          NIL)
         (SETQ H (TERMREAD))
         (COND
          ((EQUAL H 'Y)
           (PROG (P)
             (SETQ P PDES)
            LAB
             (COND ((NULL P) (RETURN NIL)))
             ((LAMBDA (P) (PUT P TY NIL)) (CAR P))
             (SETQ P (CDR P))
             (GO LAB)))
          (T
           (PROG (P)
             (SETQ P PDES)
            LAB
             (COND ((NULL P) (RETURN NIL)))
             ((LAMBDA (P)
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "current value for ")
                  (PRIN2 P)
                  (PRIN2 ": ")
                  (PRIN2 (GET P TY))
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "new value (e.g. '(x y z); ENTER): ") NIL)
                 (TERPRI)
                 (SETQ H (TERMREAD))
                 (PUT P TY H)
                 (PROGN
                  (PRIN2 "The new value of ")
                  (PRIN2 TY)
                  (PRIN2 ": ")
                  (PRIN2 (GET P TY))
                  NIL)))
              (CAR P))
             (SETQ P (CDR P))
             (GO LAB))))
         (COND ((EQUAL TY 'RL_WITH) (SETQ LARGEST_FULLY_SHORTENED NIL)))))
       (T (PROGN (PRIN2 "Input not recognized.") NIL)))
      (TERPRI)
      (RESTORE_INTERACTIVE_PROMPT))) 
(PUT 'RESTORE_BACKUP_FROM_FILE 'NUMBER-OF-ARGS 3) 
(PUT 'RESTORE_BACKUP_FROM_FILE 'DEFINED-ON-LINE '5023) 
(PUT 'RESTORE_BACKUP_FROM_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RESTORE_BACKUP_FROM_FILE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESTORE_BACKUP_FROM_FILE (PDES FORG NME)
    (PROG (S P ECHO_BAK SEMIC_BAK FLIST N H FI OLDSESSION OLD_SOL_LI)
      (COND
       ((EQUAL NME T)
        (PROGN
         (CHANGE_PROMPT_TO "")
         (TERPRI)
         (PROGN
          (PRIN2 "Please give the name of the file in double quotes")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "without `;' : ") NIL)
         (SETQ S (TERMREAD))
         (RESTORE_INPUT_FILE)
         (SETQ P (EXPLODE S))
         (COND
          ((OR (MEMBER '* P) (MEMBER '? P))
           (PROGN
            (SETQ P (PIPE-OPEN (BLDMSG_INTERNAL "ls %w" (LIST S)) 'INPUT))
            (SETQ FI "")
            (PROG ()
             REPEATLABEL
              (PROGN
               (SETQ H (CHANNELREADCHAR P))
               (COND
                ((EQUAL H 10)
                 (PROGN (SETQ FLIST (CONS FI FLIST)) (SETQ FI "")))
                ((NEQ H 4)
                 (SETQ FI (BLDMSG_INTERNAL "%w%w" (LIST FI (INT2ID H)))))))
              (COND ((NOT (EQUAL H 4)) (GO REPEATLABEL))))
            (COND ((NEQ FI "") (SETQ FLIST (CONS FI FLIST))))
            (CLOSE P)
            (COND
             (FLIST
              (PROGN
               (SETQ N 0)
               (SETQ P FLIST)
               (PROG ()
                WHILELABEL
                 (COND ((NOT P) (RETURN NIL)))
                 (PROGN
                  (SETQ N (ADD1 N))
                  (PROGN (PRIN2 N) (PRIN2 ": ") (PRIN2 (CAR P)) NIL)
                  (TERPRI)
                  (SETQ P (CDR P)))
                 (GO WHILELABEL))
               (TERPRI)
               (PROGN (PRIN2 "Indicate the file you want to load by") NIL)
               (TERPRI)
               (PROGN (PRIN2 "entering the corresponding number: ") NIL)
               (SETQ P (TERMREAD))
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT
                    (OR (NOT (NUMBERP P)) (LESSP P 0)
                        (GREATERP P (LENGTH FLIST))))
                   (RETURN NIL)))
                 (PROGN
                  (PROGN
                   (PRIN2 "This is not a number >0 and <=")
                   (PRIN2 (LENGTH FLIST))
                   (PRIN2 "! Try again: ")
                   NIL)
                  (SETQ P (TERMREAD)))
                 (GO WHILELABEL))
               (SETQ S (NTH FLIST P))))))))
         (RESTORE_INTERACTIVE_PROMPT)))
       (NME (SETQ S NME)) (T (SETQ S (LEVEL_STRING SESSION_))))
      (COND
       ((NULL SOL_LIST)
        (PROGN
         (SETQ OLD_SOL_LI (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
         (COND ((FILEP OLD_SOL_LI) (SETQ OLDSESSION SESSION_))))))
      (SETQ ECHO_BAK *ECHO)
      (SETQ SEMIC_BAK SEMIC*)
      (SETQ SEMIC* '$)
      (IN (LIST S))
      (SETQ *ECHO ECHO_BAK)
      (SETQ SEMIC* SEMIC_BAK)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETPROP P NIL)) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (P)
        (SETQ P FORG)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (COND ((PAIRP P) (PUT (CADR P) 'FCTS NIL)))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (POP_VARS PASSED_BACK (CAR BACKUP_))
      (UNIQUIFYKORD KORD*)
      (UNIQUIFYDEPL DEPL*)
      (UNIQUIFYASYMPLIS ASYMPLIS*)
      (COND ((AND EQN_INPUT (NEQ EQN_INPUT 'DONE)) (CLOSE EQN_INPUT)))
      (SETQ S (RESTORE_PDES (CDR BACKUP_)))
      (SETQ BACKUP_ NIL)
      (COND
       ((AND OLDSESSION (NEQ OLDSESSION SESSION_))
        (SYSTEM (BLDMSG_INTERNAL "rm %w" (LIST OLD_SOL_LI)))))
      (RETURN S))) 
(PUT 'LEVEL_STRING 'NUMBER-OF-ARGS 1) 
(PUT 'LEVEL_STRING 'DEFINED-ON-LINE '5105) 
(PUT 'LEVEL_STRING 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LEVEL_STRING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEVEL_STRING (S)
    (PROG (M)
      (PROG (M)
        (SETQ M (REVERSE LEVEL_))
       LAB
        (COND ((NULL M) (RETURN NIL)))
        ((LAMBDA (M)
           (SETQ S
                   (COND
                    (S
                     (COND
                      ((FIXP M)
                       (COND ((LESSP M 10) (BLDMSG_INTERNAL "%w%d" (LIST S M)))
                             (T (BLDMSG_INTERNAL "%w.%d." (LIST S M)))))
                      (T (BLDMSG_INTERNAL "%w%w." (LIST S M)))))
                    ((FIXP M)
                     (COND ((LESSP M 10) (BLDMSG_INTERNAL "%d" (LIST M)))
                           (T (BLDMSG_INTERNAL ".%d." (LIST M)))))
                    (T (BLDMSG_INTERNAL "%w." (LIST M))))))
         (CAR M))
        (SETQ M (CDR M))
        (GO LAB))
      (RETURN S))) 
(PUT 'BACKUP_TO_FILE 'NUMBER-OF-ARGS 3) 
(PUT 'BACKUP_TO_FILE 'DEFINED-ON-LINE '5117) 
(PUT 'BACKUP_TO_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'BACKUP_TO_FILE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BACKUP_TO_FILE (PDES FORG NME)
    (PROG (S A SAVE OFL*BAK *NATBAT)
      (COND
       ((EQUAL NME T)
        (PROGN
         (CHANGE_PROMPT_TO "")
         (TERPRI)
         (PROGN
          (PRIN2 "Please give the name of the file in double quotes")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "without `;' : ") NIL)
         (SETQ S (TERMREAD))
         (RESTORE_INTERACTIVE_PROMPT)))
       (NME (SETQ S NME)) (T (SETQ S (LEVEL_STRING SESSION_))))
      (SETQ A (OPEN S 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* S)
      (SETQ SAVE (WRS A))
      (SETQ *NATBAT *NAT)
      (OFF (LIST 'NAT))
      (PROGN (PRIN2 "off echo$") NIL)
      (PROGN (PRIN2 "backup_:='") NIL)
      (TERPRI)
      (PRINT (CONS (PUSH_VARS PASSED_BACK) (BACKUP_PDES PDES FORG)))
      (PROGN (PRIN2 "$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (COND ((NEQ *NAT *NATBAT) (ON (LIST 'NAT)))))) 
(PUT 'DELETE_BACKUP 'NUMBER-OF-ARGS 0) 
(PUT 'DELETE_BACKUP 'DEFINED-ON-LINE '5154) 
(PUT 'DELETE_BACKUP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETE_BACKUP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DELETE_BACKUP NIL
    (PROG (S)
      (COND ((NULL SESSION_) (RETURN NIL)))
      (SETQ S (LEVEL_STRING SESSION_))
      (DELETE-FILE-EXACT S)
      (SETQ S (EXPLODE S))
      (SETQ S (REVERSE (CONS '|"| (CONS '* (CDR (REVERSE S))))))
      (SETQ S (CONS '|"| (CONS 'C (CONS 'D (CDDDR S)))))
      (DELETE-FILE-MATCH (COMPRESS S))
      (SETQ S (CONS '|"| (CONS 'I (CONS 'E (CDDDR S)))))
      (DELETE-FILE-MATCH (COMPRESS S)))) 
(PUT 'MERGE_CRACK_RETURNS 'NUMBER-OF-ARGS 2) 
(PUT 'MERGE_CRACK_RETURNS 'DEFINED-ON-LINE '5174) 
(PUT 'MERGE_CRACK_RETURNS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MERGE_CRACK_RETURNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MERGE_CRACK_RETURNS (R1 R2)
    (COND
     ((AND (NULL COLLECT_SOL) (OR (NULL R1) (FIXP (CAR R1)))
           (OR (NULL R2) (FIXP (CAR R2))))
      (COND ((NULL R1) R2) ((NULL R2) R1) (T (LIST (PLUS (CAR R1) (CAR R2))))))
     (T (UNION R1 R2)))) 
(PUT 'RESTORE_AND_MERGE 'NUMBER-OF-ARGS 3) 
(PUT 'RESTORE_AND_MERGE 'DEFINED-ON-LINE '5182) 
(PUT 'RESTORE_AND_MERGE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RESTORE_AND_MERGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RESTORE_AND_MERGE (SOLN PDES FORG)
    (PROG (BAK NEWFDEP SOL F H)
      (SETQ NEWFDEP NIL)
      (PROG (SOL)
        (SETQ SOL SOLN)
       LAB
        (COND ((NULL SOL) (RETURN NIL)))
        ((LAMBDA (SOL)
           (COND
            ((PAIRP SOL)
             (PROGN
              (PROG (F)
                (SETQ F (CADDR SOL))
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F)
                   (COND
                    ((SETQ H (ASSOC F DEPL*))
                     (SETQ NEWFDEP (UNION (LIST H) NEWFDEP)))))
                 (CAR F))
                (SETQ F (CDR F))
                (GO LAB))
              NIL))))
         (CAR SOL))
        (SETQ SOL (CDR SOL))
        (GO LAB))
      (SETQ BAK (LIST (PUSH_VARS PASSED_BACK) NEWFDEP))
      (SETQ H (RESTORE_BACKUP_FROM_FILE PDES FORG NIL))
      (POP_VARS PASSED_BACK (CAR BAK))
      (SETQ DEPL* (UNION (CADR BAK) DEPL*))
      (RETURN H))) 
(FLAG '(WRITE_STAT_IN_FILE) 'OPFN) 
(PUT 'WRITE_STAT_IN_FILE 'NUMBER-OF-ARGS 0) 
(PUT 'WRITE_STAT_IN_FILE 'DEFINED-ON-LINE '5210) 
(PUT 'WRITE_STAT_IN_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITE_STAT_IN_FILE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE WRITE_STAT_IN_FILE NIL
    (COND
     ((NULL SIZE_WATCH)
      (PROGN
       (PROGN (PRIN2 "No statistical history is recorded.") NIL)
       (TERPRI)
       (PROGN (PRIN2 "To record one enter: as {size_watch,t};") NIL)
       (TERPRI)
       NIL))
     (T
      (PROG (S A SAVE OFL*BAK)
        (CHANGE_PROMPT_TO "")
        (SETQ S (BLDMSG_INTERNAL "%w.%w" (LIST SESSION_ "size_hist")))
        (SETQ A (OPEN S 'OUTPUT))
        (SETQ OFL*BAK OFL*)
        (SETQ OFL* S)
        (SETQ SAVE (WRS A))
        (PROGN (PRIN2 "size_hist:='") NIL)
        (PRETTYPRINT SIZE_HIST)
        (PROGN (PRIN2 "$end$") NIL)
        (TERPRI)
        (WRS SAVE)
        (SETQ OFL* OFL*BAK)
        (CLOSE A)
        (RESTORE_INTERACTIVE_PROMPT))))) 
(PUT 'WRITE_IN_FILE 'NUMBER-OF-ARGS 2) 
(PUT 'WRITE_IN_FILE 'DEFINED-ON-LINE '5233) 
(PUT 'WRITE_IN_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITE_IN_FILE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WRITE_IN_FILE (PDES FORG)
    (PROG (P PL S H WN VL V LL A SAVE OFL*BAK *NATBAK)
      (SETQ LL (LINELENGTH 79))
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN (PRIN2 "Enter a list of equations, like e2,e5,e35; from: ") NIL)
      (TERPRI)
      (LISTPRINT PDES)
      (TERPRI)
      (PROGN (PRIN2 "To write all equations just enter ; ") NIL)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ S (TERMLISTREAD))
         (SETQ H S)
         (COND ((EQUAL S NIL) (SETQ PL PDES))
               (T
                (PROGN
                 (SETQ PL NIL)
                 (SETQ H NIL)
                 (COND
                  ((OR (NULL S) (PAIRP S))
                   (PROGN
                    (PROG (P)
                      (SETQ P S)
                     LAB
                      (COND ((NULL P) (RETURN NIL)))
                      ((LAMBDA (P)
                         (COND ((MEMBER P PDES) (SETQ PL (CONS P PL)))))
                       (CAR P))
                      (SETQ P (CDR P))
                      (GO LAB))
                    (SETQ H (SETDIFF PL PDES))
                    NIL))
                  (T (SETQ H S)))
                 NIL)))
         (COND
          (H
           (PROGN
            (PROGN
             (PRIN2 "These are no equations: ")
             (PRIN2 H)
             (PRIN2 "   Try again.")
             NIL)
            (TERPRI))))
         NIL)
        (COND ((NOT (NULL H)) (GO REPEATLABEL))))
      (PROGN (PRIN2 "Shall the name of the equation be written? (y/n) ") NIL)
      (PROG ()
       REPEATLABEL
        (SETQ S (TERMREAD))
        (COND
         ((NOT (OR (EQUAL S 'Y) (EQUAL S 'Y) (EQUAL S 'N) (EQUAL S 'N)))
          (GO REPEATLABEL))))
      (COND ((OR (EQUAL S 'Y) (EQUAL S 'Y)) (SETQ WN T)))
      (PROGN (PRIN2 "Please give the name of the file in double quotes") NIL)
      (TERPRI)
      (PROGN (PRIN2 "without `;' : ") NIL)
      (SETQ S (TERMREAD))
      (SETQ A (OPEN S 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* S)
      (SETQ SAVE (WRS A))
      (SETQ *NATBAK *NAT)
      (OFF (LIST 'NAT))
      (PROGN (PRIN2 "% Modify the following load command by adding the") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "% directory name in which crack is stored, for example:")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "% load \"~/crack/crack\"$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "load crack$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "lisp(nfct_:=") (PRIN2 NFCT_) (PRIN2 ")$") NIL)
      (TERPRI)
      (COND (WN (PROGN (PRIN2 "lisp(nequ_:=") (PRIN2 NEQU_) (PRIN2 ")$") NIL)))
      (TERPRI)
      (PROGN (PRIN2 "off batch_mode$") NIL)
      (TERPRI)
      (PROG (P)
        (SETQ P PL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN (SETQ H (GET P 'VARS)) (COND (H (SETQ VL (UNION H VL))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROGN (PRIN2 "list_of_variables:=") NIL)
      (ASSGNPRI (AEVAL (CONS 'LIST VL)) NIL 'ONLY)
      (PROGN (PRIN2 "list_of_functions:=") NIL)
      (ASSGNPRI (AEVAL (CONS 'LIST FTEM_)) NIL 'ONLY)
      (COND
       (FLIN_
        (PROGN
         (PROGN (PRIN2 "% linearly occuring functions:") NIL)
         (TERPRI)
         (PROGN (PRIN2 "lisp(flin_:='(") NIL)
         (TERPRI)
         (PROG (H)
           (SETQ H FLIN_)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H) (PROGN (PROGN (PRIN2 H) NIL) (TERPRI))) (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (PROGN (PRIN2 "))$") NIL)
         (TERPRI))))
      (COND
       (FHOM_
        (PROGN
         (PROGN (PRIN2 "% homogeneous functions:") NIL)
         (TERPRI)
         (PROGN (PRIN2 "lisp(fhom_:='(") NIL)
         (TERPRI)
         (PROG (H)
           (SETQ H FHOM_)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H) (PROGN (PROGN (PRIN2 H) NIL) (TERPRI))) (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (PROGN (PRIN2 "))$") NIL)
         (TERPRI))))
      (PROG (H)
        (SETQ H FTEM_)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND
            ((ASSOC H DEPL*)
             (PROGN
              (PROGN (PRIN2 "depend ") (PRIN2 H) NIL)
              (PROG (V)
                (SETQ V (CDR (ASSOC H DEPL*)))
               LAB
                (COND ((NULL V) (RETURN NIL)))
                ((LAMBDA (V) (PROGN (PROGN (PRIN2 ",") NIL) (PRINT V)))
                 (CAR V))
                (SETQ V (CDR V))
                (GO LAB))
              (PROGN (PRIN2 "$") NIL)
              (TERPRI)
              NIL))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (COND
       (WN
        (PROGN
         (PROG (H)
           (SETQ H PL)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (PROGN
               (ASSGNPRI H NIL 'FIRST)
               (ASSGNPRI (AEVAL ":=") NIL NIL)
               (ASSGNPRI (AEVAL (LIST '*SQ (GET H 'SQVAL) T)) NIL 'LAST)))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (PROGN (PRIN2 "list_of_equations:=") NIL)
         (ASSGNPRI (AEVAL (CONS 'LIST PL)) NIL 'ONLY)))
       (T
        (PROGN
         (PROGN (PRIN2 "list_of_equations:=") NIL)
         (ASSGNPRI
          (AEVAL
           (CONS 'LIST
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H PL)
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (H) (LIST '*SQ (GET H 'SQVAL) T))
                                     (CAR H))
                                    NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (H) (LIST '*SQ (GET H 'SQVAL) T)) (CAR H))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          NIL 'ONLY)
         NIL)))
      (PROGN (PRIN2 "list_of_inequalities:=") NIL)
      (ASSGNPRI
       (AEVAL
        (CONS 'LIST
              (APPEND
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P INEQ_)
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (P) (LIST '*SQ P T)) (CAR P))
                                       NIL)))
                LOOPLABEL
                 (SETQ P (CDR P))
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (LIST '*SQ P T)) (CAR P)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (COND ((NULL INEQ_OR) NIL)
                     (T
                      (PROG (H FORALL-RESULT FORALL-ENDPTR)
                        (SETQ H INEQ_OR)
                        (COND ((NULL H) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (H)
                                            (CONS 'LIST
                                                  (PROG (P FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ P H)
                                                    (COND
                                                     ((NULL P) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (P)
                                                                        (LIST
                                                                         '*SQ
                                                                         (COND
                                                                          ((NULL
                                                                            (CDR
                                                                             P))
                                                                           (CAR
                                                                            P))
                                                                          (T
                                                                           (PROGN
                                                                            (SETQ V
                                                                                    (CAR
                                                                                     P))
                                                                            (SETQ P
                                                                                    (CDR
                                                                                     P))
                                                                            (PROG ()
                                                                             WHILELABEL
                                                                              (COND
                                                                               ((NOT
                                                                                 P)
                                                                                (RETURN
                                                                                 NIL)))
                                                                              (PROGN
                                                                               (SETQ V
                                                                                       (MULTSQ
                                                                                        V
                                                                                        (CAR
                                                                                         P)))
                                                                               (SETQ P
                                                                                       (CDR
                                                                                        P)))
                                                                              (GO
                                                                               WHILELABEL))
                                                                            V)))
                                                                         T))
                                                                      (CAR P))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ P (CDR P))
                                                    (COND
                                                     ((NULL P)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (P)
                                                                (LIST '*SQ
                                                                      (COND
                                                                       ((NULL
                                                                         (CDR
                                                                          P))
                                                                        (CAR
                                                                         P))
                                                                       (T
                                                                        (PROGN
                                                                         (SETQ V
                                                                                 (CAR
                                                                                  P))
                                                                         (SETQ P
                                                                                 (CDR
                                                                                  P))
                                                                         (PROG ()
                                                                          WHILELABEL
                                                                           (COND
                                                                            ((NOT
                                                                              P)
                                                                             (RETURN
                                                                              NIL)))
                                                                           (PROGN
                                                                            (SETQ V
                                                                                    (MULTSQ
                                                                                     V
                                                                                     (CAR
                                                                                      P)))
                                                                            (SETQ P
                                                                                    (CDR
                                                                                     P)))
                                                                           (GO
                                                                            WHILELABEL))
                                                                         V)))
                                                                      T))
                                                              (CAR P))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL))))
                                          (CAR H))
                                         NIL)))
                       LOOPLABEL
                        (SETQ H (CDR H))
                        (COND ((NULL H) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (CONS 'LIST
                                          (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ P H)
                                            (COND ((NULL P) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (P)
                                                                (LIST '*SQ
                                                                      (COND
                                                                       ((NULL
                                                                         (CDR
                                                                          P))
                                                                        (CAR
                                                                         P))
                                                                       (T
                                                                        (PROGN
                                                                         (SETQ V
                                                                                 (CAR
                                                                                  P))
                                                                         (SETQ P
                                                                                 (CDR
                                                                                  P))
                                                                         (PROG ()
                                                                          WHILELABEL
                                                                           (COND
                                                                            ((NOT
                                                                              P)
                                                                             (RETURN
                                                                              NIL)))
                                                                           (PROGN
                                                                            (SETQ V
                                                                                    (MULTSQ
                                                                                     V
                                                                                     (CAR
                                                                                      P)))
                                                                            (SETQ P
                                                                                    (CDR
                                                                                     P)))
                                                                           (GO
                                                                            WHILELABEL))
                                                                         V)))
                                                                      T))
                                                              (CAR P))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ P (CDR P))
                                            (COND
                                             ((NULL P) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (P)
                                                        (LIST '*SQ
                                                              (COND
                                                               ((NULL (CDR P))
                                                                (CAR P))
                                                               (T
                                                                (PROGN
                                                                 (SETQ V
                                                                         (CAR
                                                                          P))
                                                                 (SETQ P
                                                                         (CDR
                                                                          P))
                                                                 (PROG ()
                                                                  WHILELABEL
                                                                   (COND
                                                                    ((NOT P)
                                                                     (RETURN
                                                                      NIL)))
                                                                   (PROGN
                                                                    (SETQ V
                                                                            (MULTSQ
                                                                             V
                                                                             (CAR
                                                                              P)))
                                                                    (SETQ P
                                                                            (CDR
                                                                             P)))
                                                                   (GO
                                                                    WHILELABEL))
                                                                 V)))
                                                              T))
                                                      (CAR P))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                  (CAR H))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))))
       NIL 'ONLY)
      (TERPRI)
      (PROGN (PRIN2 "solution_:=crack(list_of_equations,") NIL)
      (TERPRI)
      (PROGN (PRIN2 "                 list_of_inequalities,") NIL)
      (TERPRI)
      (PROGN (PRIN2 "                 list_of_functions,") NIL)
      (TERPRI)
      (PROGN (PRIN2 "                 list_of_variables)$") NIL)
      (TERPRI)
      (PROG (H)
        (SETQ H FORG)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (COND
             ((AND (PAIRP H) (EQUAL (CAR H) 'EQUAL))
              (PROGN
               (TERPRI)
               (PROGN
                (ASSGNPRI (AEVAL (CADR H)) NIL 'FIRST)
                (ASSGNPRI (AEVAL " := sub(second first solution_,") NIL NIL)
                (ASSGNPRI (AEVAL (LIST '*SQ (CADDR H) T)) NIL NIL)
                (ASSGNPRI (AEVAL ")") NIL 'LAST)))))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (TERPRI)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 "These data were produced with the following input:") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 "lisp( old_history := ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "'") (PRIN2 (REVERSE HISTORY_)) (PRIN2 ")$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (COND ((NEQ *NAT *NATBAK) (ON (LIST 'NAT))))
      (RESTORE_INTERACTIVE_PROMPT)
      (LINELENGTH LL))) 
(PUT 'GIVE_LOW_PRIORITY 'NUMBER-OF-ARGS 2) 
(PUT 'GIVE_LOW_PRIORITY 'DEFINED-ON-LINE '5360) 
(PUT 'GIVE_LOW_PRIORITY 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'GIVE_LOW_PRIORITY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GIVE_LOW_PRIORITY (PDES F)
    (PROG (FTEMCP ANO H S FLI)
      (SETQ FTEMCP FTEM_)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FTEMCP (NEQ (CAR FTEMCP) F))) (RETURN NIL)))
        (PROGN (SETQ H (CONS (CAR FTEMCP) H)) (SETQ FTEMCP (CDR FTEMCP)))
        (GO WHILELABEL))
      (COND
       (FTEMCP
        (PROGN
         (SETQ FTEMCP (CDR FTEMCP))
         (SETQ ANO (FCTLENGTH F))
         (COND ((MEMBER F FLIN_) (SETQ FLI T)))
         (PROG ()
          WHILELABEL
           (COND ((NOT FTEMCP) (RETURN NIL)))
           (COND
            ((OR (GREATERP ANO (FCTLENGTH (CAR FTEMCP)))
                 (AND FLI (NOT (MEMBER (CAR FTEMCP) FLIN_))))
             (SETQ FTEMCP NIL))
            (T
             (PROGN
              (SETQ H (CONS (CAR FTEMCP) H))
              (SETQ FTEMCP (CDR FTEMCP))
              (COND
               ((NOT (MEMBER (SIMP (CAR H)) INEQ_))
                (PROGN
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND FTEMCP (EQUAL ANO (FCTLENGTH (CAR FTEMCP)))
                           (NOT (MEMBER (SIMP (CAR FTEMCP)) INEQ_))
                           (OR (NOT FLI) (MEMBER (CAR FTEMCP) FLIN_))))
                     (RETURN NIL)))
                   (PROGN
                    (SETQ H (CONS (CAR FTEMCP) H))
                    (SETQ FTEMCP (CDR FTEMCP)))
                   (GO WHILELABEL))
                 (COND
                  ((OR PRINT_ TR_ORDERINGS)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "The lexicographical ordering of unknowns is changed")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2 "because ")
                     (PRIN2 F)
                     (PRIN2 " has to be non-zero, giving ")
                     (PRIN2 F)
                     (PRIN2 " a low priority.")
                     NIL)
                    (TERPRI)
                    (PROGN (PRIN2 "Old ordering: ") NIL)
                    (SETQ S FTEM_)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT S) (RETURN NIL)))
                      (PROGN
                       (PROGN (PRIN2 (CAR S)) NIL)
                       (SETQ S (CDR S))
                       (COND (S (PROGN (PRIN2 ",") NIL))))
                      (GO WHILELABEL))
                    (TERPRI)
                    (PROGN (PRIN2 "New ordering: ") NIL)
                    (SETQ S (APPEND (REVERSE H) (CONS F FTEMCP)))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT S) (RETURN NIL)))
                      (PROGN
                       (PROGN (PRIN2 (CAR S)) NIL)
                       (SETQ S (CDR S))
                       (COND (S (PROGN (PRIN2 ",") NIL))))
                      (GO WHILELABEL))
                    (TERPRI)
                    NIL)))
                 (CHANGE_FCTS_ORDERING (APPEND (REVERSE H) (CONS F FTEMCP))
                  PDES VL_)
                 (SETQ FTEMCP NIL)))))))
           (GO WHILELABEL))))))) 
(PUT 'UPDATESQFCTEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'UPDATESQFCTEVAL 'DEFINED-ON-LINE '5436) 
(PUT 'UPDATESQFCTEVAL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UPDATESQFCTEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPDATESQFCTEVAL (PDES NEWINEQ)
    (PROG (P PV PS HIST H1 MOD_SWITCHED)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((NULL CONTRADICTION_)
             (COND
              ((EQUAL NEWINEQ (GET P 'SQVAL))
               (RAISE_CONTRADICTION (LIST '*SQ NEWINEQ T) NIL))
              (T
               (PROGN
                (SETQ PV (GET P 'FAC))
                (COND
                 ((AND (PAIRP PV) (MEMBER NEWINEQ PV))
                  (PROGN
                   (COND
                    (RECORD_HIST
                     (SETQ HIST
                             (REVAL1
                              (LIST 'QUOTIENT (GET P 'HISTRY_)
                                    (REVAL1 (LIST '*SQ NEWINEQ NIL) T))
                              T))))
                   (PROG (H1)
                     (SETQ H1 ALLFLAGS_)
                    LAB
                     (COND ((NULL H1) (RETURN NIL)))
                     ((LAMBDA (H1) (FLAG (LIST P) H1)) (CAR H1))
                     (SETQ H1 (CDR H1))
                     (GO LAB))
                   (COND
                    ((AND MODULAR_COMP (NULL *MODULAR))
                     (PROGN (ON (LIST 'MODULAR)) (SETQ MOD_SWITCHED T))))
                   (SETQ H1 (MULTSQ (GET P 'SQVAL) (INVSQ NEWINEQ)))
                   (COND (MOD_SWITCHED (OFF (LIST 'MODULAR))))
                   (UPDATESQ P H1 NIL NIL (GET P 'FCTS) (GET P 'VARS) T
                    (LIST 0) PDES)
                   (DROP_PDE_FROM_IDTIES P PDES HIST)
                   (DROP_PDE_FROM_PROPERTIES P PDES)))
                 (T
                  (PROGN
                   (SETQ PS (GET P 'FCTEVAL_NLI))
                   (COND
                    ((AND PS (SETQ H1 (SMEMBERL (GET P 'FCTS) NEWINEQ)))
                     (PROGN
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (AND PS (FREEOFLIST (CAAR PS) H1)))
                          (RETURN NIL)))
                        (SETQ PS (CDR PS))
                        (GO WHILELABEL))
                      (COND
                       (PS
                        (PROGN
                         (FLAG (LIST P) 'TO_EVAL)
                         (PUT P 'FCTEVAL_LIN NIL)
                         (PUT P 'FCTEVAL_NCA NIL)
                         (PUT P 'FCTEVAL_NLI NIL)
                         (PUT P 'FCTEVAL_N2L NIL)
                         (PUT P 'FCT_NLI_LIN NIL)
                         (PUT P 'FCT_NLI_NCA NIL)
                         (PUT P 'FCT_NLI_NLI NIL)
                         (PUT P 'FCT_NLI_NUS NIL)
                         NIL)))))))))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB)))) 
(PUT 'ADDFUNCTION 'NUMBER-OF-ARGS 1) 
(PUT 'ADDFUNCTION 'DEFINED-ON-LINE '5501) 
(PUT 'ADDFUNCTION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADDFUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADDFUNCTION (FT)
    (PROG (F FF L OK)
      (CHANGE_PROMPT_TO "")
      (SETQ FF (MKID FNAME_ NFCT_))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ OK T)
         (TERPRI)
         (PROGN (PRIN2 "What is the name of the new function?") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "If the name is ")
          (PRIN2 FNAME_)
          (PRIN2 "+digits then use ")
          (PRIN2 FF)
          (PRIN2 ". Terminate with <ENTER>: ")
          NIL)
         (SETQ F (TERMREAD))
         (COND ((EQUAL F FF) (SETQ NFCT_ (ADD1 NFCT_)))
               ((MEMBER F FT)
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "Choose another name. ")
                  (PRIN2 F)
                  (PRIN2 " is already in use.")
                  NIL)
                 (SETQ OK NIL))))
         NIL)
        (COND ((NOT OK) (GO REPEATLABEL))))
      (SETQ DEPL* (DELETE (ASSOC F DEPL*) DEPL*))
      (TERPRI)
      (PROGN
       (PRIN2 "Give a list of variables ")
       (PRIN2 F)
       (PRIN2 " depends on, for example x,y,z;  ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "For constant ") (PRIN2 F) (PRIN2 " input a `;'  ") NIL)
      (SETQ L (TERMXREAD))
      (COND ((AND (PAIRP L) (EQUAL (CAR L) '*COMMA*)) (SETQ L (CDR L))))
      (COND ((PAIRP L) (SETQ DEPL* (CONS (CONS F L) DEPL*)))
            (L (SETQ DEPL* (CONS (LIST F L) DEPL*))))
      (SETQ FT (FCTINSERT F FT))
      (SETQ FTEM_ (FCTINSERT F FTEM_))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN (CONS FT F)))) 
(PUT 'REDUCEPDE 'NUMBER-OF-ARGS 3) 
(PUT 'REDUCEPDE 'DEFINED-ON-LINE '5534) 
(PUT 'REDUCEPDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'REDUCEPDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDUCEPDE (PDES FTEM VL)
    (PROG (P Q EX)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN (PRIN2 "Which equation is to be simplified? ") NIL)
      (SETQ P (TERMREAD))
      (COND
       ((NOT (MEMBER P PDES))
        (PROGN (PRIN2 "This is not the name of an equation!") NIL))
       (T
        (PROGN
         (SETQ EX (GET P 'SQVAL))
         (SETQ PDES (DROP_PDE P PDES NIL))
         (SETQ Q (MKEQSQ EX NIL NIL FTEM VL ALLFLAGS_ T (LIST 0) NIL PDES))
         (TERPRI)
         (PROGN (PRIN2 Q) (PRIN2 " replaces ") (PRIN2 P) NIL)
         (SETQ PDES (EQINSERT Q PDES))
         (COND
          ((MEMBER Q PDES)
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 Q) (PRIN2 " : ") NIL)
            (TYPEEQ Q)
            (PLOT_NON0_SEPARANTS Q)))))))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN (LIST PDES FTEM)))) 
(PUT 'REPLACE_EQUATION 'NUMBER-OF-ARGS 1) 
(PUT 'REPLACE_EQUATION 'DEFINED-ON-LINE '5554) 
(PUT 'REPLACE_EQUATION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'REPLACE_EQUATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPLACE_EQUATION (ARGLIST)
    (PROG (PDES FORG S NFL Q)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (SETQ S (CAR (CADDDR ARGLIST)))
      (COND (S (SETQ PDES (DROP_PDE S PDES NIL))))
      (SETQ NFL (CADR (CADDDR ARGLIST)))
      (PROG (F)
        (SETQ F NFL)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (COND ((CDR F) (SETQ DEPL* (CONS F DEPL*))))
            (SETQ FTEM_ (FCTINSERT (CAR F) FTEM_))
            NIL))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ Q
              (MKEQSQ (CADDR (CADDDR ARGLIST)) NIL NIL FTEM_ VL_ ALLFLAGS_ T
               (LIST 0) (CADDDR (CADDDR ARGLIST)) PDES))
      (SETQ PDES (EQINSERT Q PDES))
      (TERPRI)
      (COND
       ((FREEOF PDES Q)
        (COND
         (S (PROGN (PRIN2 "Equation ") (PRIN2 S) (PRIN2 " is deleted.") NIL))
         (T
          (PROGN
           (PRIN2
            "A new equation turned out to be a consequence of known ones.")
           NIL))))
       (S
        (PROGN
         (PRIN2 "Equation ")
         (PRIN2 Q)
         (PRIN2 " replaces ")
         (PRIN2 S)
         (PRIN2 ".")
         NIL))
       (T (PROGN (PRIN2 "Equation ") (PRIN2 Q) (PRIN2 " is added.") NIL)))
      (RETURN (LIST PDES FORG)))) 
(PUT 'REPLACEPDE 'NUMBER-OF-ARGS 3) 
(PUT 'REPLACEPDE 'DEFINED-ON-LINE '5597) 
(PUT 'REPLACEPDE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'REPLACEPDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPLACEPDE (PDES FTEM VL)
    (PROG (P Q EX H NEWFT AGAIN)
      (CHANGE_PROMPT_TO "")
      (PROG ()
       REPEATLABEL
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Is there a") NIL)
         (COND (AGAIN (PROGN (PRIN2 " further") NIL)))
         (PROGN (PRIN2 " new function in the changed/new PDE that") NIL)
         (TERPRI)
         (PROGN (PRIN2 "is to be calculated (y/n)? ") NIL)
         (SETQ P (TERMREAD))
         (COND
          ((OR (EQUAL P 'Y) (EQUAL P 'Y))
           (PROGN
            (SETQ H (ADDFUNCTION FTEM))
            (SETQ FTEM (CAR H))
            (COND ((CDR H) (SETQ NEWFT (CONS (CDR H) NEWFT)))))))
         (SETQ AGAIN T))
        (COND ((NOT (OR (EQUAL P 'N) (EQUAL P 'N))) (GO REPEATLABEL))))
      (TERPRI)
      (PROGN
       (PRIN2
        "If you want to replace a pde then type its name, e.g. e_23 <ENTER>.")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "If you want to add a pde then type `new_pde' <ENTER>. ")
       NIL)
      (SETQ P (TERMREAD))
      (COND
       ((OR (EQUAL P 'NEW_PDE) (MEMBER P PDES))
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "Input of a value for ") NIL)
         (COND ((EQUAL P 'NEW_PDE) (PROGN (PRIN2 "the new pde.") NIL))
               (T (PROGN (PRIN2 P) (PRIN2 ".") NIL)))
         (TERPRI)
         (PROGN
          (PRIN2 "You can use names of other pds, e.g. 3*e_12 - df(e_13,x); ")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "Terminate the expression with ; or $ : ") NIL)
         (TERPRI)
         (SETQ EX (TERMXREAD))
         (PROG (A)
           (SETQ A PDES)
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A)
              (COND
               ((NOT (FREEOF EX A))
                (PROGN
                 (COND
                  ((NULL (GET A 'VAL)) (PUT A 'VAL (PREPSQ (GET A 'SQVAL)))))
                 (SETQ EX (SUBST (GET A 'VAL) A EX))
                 NIL))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ EX (SIMP EX))
         (TERPRI)
         (PROGN (PRIN2 "Do you want the equation to be") NIL)
         (TERPRI)
         (PROGN (PRIN2 "- simplified (e.g. e**log(x) -> x) without") NIL)
         (TERPRI)
         (PROGN (PRIN2 "  dropping non-zero factors and denominators") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "  (e.g. to introduce integrating factors)       (1)")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "- simplified completely                         (2) ")
          NIL)
         (SETQ H (TERMREAD))
         (COND ((EQUAL H 1) (SETQ H NIL)) (T (SETQ H T)))
         (COND ((NEQ P 'NEW_PDE) (SETQ PDES (DROP_PDE P PDES NIL))))
         (COND
          (FLIN_
           (PROG (Q)
             (SETQ Q NEWFT)
            LAB
             (COND ((NULL Q) (RETURN NIL)))
             ((LAMBDA (Q)
                (COND
                 ((LIN_CHECK_SQ EX (LIST Q))
                  (SETQ FLIN_ (SORT_ACCORDING_TO (CONS Q FLIN_) FTEM_)))))
              (CAR Q))
             (SETQ Q (CDR Q))
             (GO LAB))))
         (SETQ Q (MKEQSQ EX NIL NIL FTEM VL ALLFLAGS_ H (LIST 0) NIL PDES))
         (COND ((AND (EQUAL P 'NEW_PDE) NEWFT) (PUT Q 'NOT_TO_EVAL NEWFT)))
         (TERPRI)
         (PROGN (PRIN2 Q) NIL)
         (COND ((EQUAL P 'NEW_PDE) (PROGN (PRIN2 " is added") NIL))
               (T (PROGN (PRIN2 " replaces ") (PRIN2 P) NIL)))
         (SETQ PDES (EQINSERT Q PDES))))
       (T
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 "A pde ")
          (PRIN2 P)
          (PRIN2 " does not exist! (Back to previous menu)")
          NIL))))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN (LIST PDES FTEM)))) 
(PUT 'SELECT_FROM_LIST 'NUMBER-OF-ARGS 2) 
(PUT 'SELECT_FROM_LIST 'DEFINED-ON-LINE '5685) 
(PUT 'SELECT_FROM_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SELECT_FROM_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SELECT_FROM_LIST (LISTE N)
    (PROG (S)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (COND
       (N (PROGN (PRIN2 "Pick ") (PRIN2 N) (PRIN2 " from this list:") NIL))
       (T (PROGN (PRIN2 "Pick from this list") NIL)))
      (TERPRI)
      (LISTPRINT LISTE)
      (PROGN (PRIN2 ";") NIL)
      (TERPRI)
      (COND
       ((NULL N)
        (PROGN
         (PROGN
          (PRIN2
           "a sublist and input it in the same form. Enter ; to choose all.")
          NIL)
         (TERPRI)
         NIL)))
      (SETQ S (TERMLISTREAD))
      (COND
       ((AND N (NEQ N (LENGTH S)))
        (PROGN
         (PROGN (PRIN2 "Wrong number picked.") NIL)
         (TERPRI)
         (SETQ S NIL)
         NIL))
       ((NULL S) (SETQ S LISTE))
       ((NOT_INCLUDED S LISTE)
        (PROGN
         (PROGN (PRIN2 (SETDIFF S LISTE)) (PRIN2 " is not allowed.") NIL)
         (TERPRI)
         (SETQ S NIL)
         NIL)))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN S))) 
(PUT 'SELECTPDES 'NUMBER-OF-ARGS 2) 
(PUT 'SELECTPDES 'DEFINED-ON-LINE '5711) 
(PUT 'SELECTPDES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SELECTPDES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SELECTPDES (PDES N)
    (COND
     (PDES
      (PROG (L S M)
        (CHANGE_PROMPT_TO "")
        (TERPRI)
        (COND
         ((NULL N)
          (PROGN
           (PROGN (PRIN2 "How many equations do you want to select? ") NIL)
           (TERPRI)
           (PROGN (PRIN2 "(number <ENTER>) : ") NIL)
           (TERPRI)
           (SETQ N (TERMREAD))
           NIL)))
        (PROGN (PRIN2 "Please select ") (PRIN2 N) (PRIN2 " equation") NIL)
        (COND ((GREATERP N 1) (PROGN (PRIN2 "s") NIL)))
        (PROGN (PRIN2 " from: ") NIL)
        (PROGN (PRIN2 PDES) NIL)
        (TERPRI)
        (SETQ M 0)
        (SETQ S T)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (LESSP M N) S)) (RETURN NIL)))
          (PROGN
           (SETQ M (ADD1 M))
           (COND ((GREATERP N 1) (PROGN (PRIN2 M) (PRIN2 ". ") NIL)))
           (PROGN (PRIN2 "pde: ") NIL)
           (SETQ S (TERMREAD))
           (PROG ()
            WHILELABEL
             (COND ((NOT (NOT (MEMBER S PDES))) (RETURN NIL)))
             (PROGN
              (COND
               ((AND SIZE_WATCH (NOT (FIXP SIZE_WATCH)))
                (SETQ HISTORY_
                        (CONS "*** Invalid input." (CONS 'IG HISTORY_)))))
              (PROGN (PRIN2 "Error!!! Please select a pde from: ") NIL)
              (PROGN (PRIN2 PDES) NIL)
              (TERPRI)
              (COND ((GREATERP N 1) (PROGN (PRIN2 M) (PRIN2 ". ") NIL)))
              (PROGN (PRIN2 "pde: ") NIL)
              (SETQ S (TERMREAD)))
             (GO WHILELABEL))
           (COND (S (PROGN (SETQ PDES (DELETE S PDES)) (SETQ L (CONS S L))))))
          (GO WHILELABEL))
        (RESTORE_INTERACTIVE_PROMPT)
        (RETURN (REVERSE L)))))) 
(PUT 'DEPND 'NUMBER-OF-ARGS 2) 
(PUT 'DEPND 'DEFINED-ON-LINE '5752) 
(PUT 'DEPND 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DEPND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPND (Y XLIST)
    (PROG (XX)
      (SETQ XX XLIST)
     LAB
      (COND ((NULL XX) (RETURN NIL)))
      ((LAMBDA (XX)
         (PROG (X)
           (SETQ X XX)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X) (DEPEND (LIST Y X))) (CAR X))
           (SETQ X (CDR X))
           (GO LAB)))
       (CAR XX))
      (SETQ XX (CDR XX))
      (GO LAB))) 
(FLAG '(NODEPENDLIST) 'OPFN) 
(PUT 'NODEPENDLIST 'NUMBER-OF-ARGS 1) 
(PUT 'NODEPENDLIST 'DEFINED-ON-LINE '5758) 
(PUT 'NODEPENDLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'NODEPENDLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NODEPENDLIST (FL)
    (PROG (F)
      (SETQ F FL)
     LAB
      (COND ((NULL F) (RETURN NIL)))
      ((LAMBDA (F)
         (COND
          ((NEQ F 'LIST)
           (PROGN
            (SETQ F (REVAL1 F T))
            (SETQ DEPL* (DELETE (ASSOC F DEPL*) DEPL*))
            (SETQ F (MKID F '_))
            (SETQ DEPL* (DELETE (ASSOC F DEPL*) DEPL*))))))
       (CAR F))
      (SETQ F (CDR F))
      (GO LAB))) 
(PUT 'DEPENDLIST 'NUMBER-OF-ARGS 2) 
(FLAG '(DEPENDLIST) 'OPFN) 
(PUT 'DEPENDLIST 'DEFINED-ON-LINE '5767) 
(PUT 'DEPENDLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DEPENDLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPENDLIST (Y XLIST)
    (PROG (XX)
      (SETQ XX (GETRLIST (AEVAL XLIST)))
     LAB
      (COND ((NULL XX) (RETURN NIL)))
      ((LAMBDA (XX)
         (PROG (X)
           (SETQ X (GETRLIST (AEVAL XX)))
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X) (AEVAL (DEPEND (LIST Y X)))) (CAR X))
           (SETQ X (CDR X))
           (GO LAB)))
       (CAR XX))
      (SETQ XX (CDR XX))
      (GO LAB))) 
(PUT 'ERR_CATCH_GROEB 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_GROEB 'DEFINED-ON-LINE '5773) 
(PUT 'ERR_CATCH_GROEB 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_GROEB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_GROEB (ARGLIST)
    (COND
     ((AND (CADDDR ARGLIST) (GREATERP (LENGTH (CADDDR ARGLIST)) 1))
      (PROG (H LL)
        (SETQ LL (LINELENGTH 10000000))
        ((LAMBDA (*PROTFG)
           (SETQ H
                   (ERRORSET (LIST 'COMP_GROEBNER_BASIS (MKQUOTE ARGLIST)) NIL
                             NIL)))
         T)
        (LINELENGTH LL)
        (SETQ ERFG* NIL)
        (RETURN (COND ((OR (NULL H) (ERRORP H)) NIL) (T (CAR H)))))))) 
(FLAG '(ERR_CATCH_READIN) 'OPFN) 
(PUT 'ERR_CATCH_READIN 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_READIN 'DEFINED-ON-LINE '5789) 
(PUT 'ERR_CATCH_READIN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_READIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_READIN (FNAME IN_MODE)
    (COND ((NULL (FILEP FNAME)) NIL)
          (T
           (PROG (H MODE_BAK ECHO_BAK SEMIC_BAK)
             (SETQ MODE_BAK *MODE)
             (SETQ ECHO_BAK *ECHO)
             (SETQ SEMIC_BAK SEMIC*)
             (SETQ SEMIC* '$)
             (SETQ *MODE
                     (PROGN
                      (SETQ ALGLIST* (CONS NIL NIL))
                      (COND ((EQUAL IN_MODE 'ALGEBRAIC) 'ALGEBRAIC)
                            (T 'SYMBOLIC))))
             ((LAMBDA (*PROTFG)
                (SETQ H (ERRORSET (LIST 'IN (MKQUOTE (LIST FNAME))) NIL NIL)))
              T)
             (SETQ *ECHO ECHO_BAK)
             (SETQ SEMIC* SEMIC_BAK)
             (SETQ ERFG* NIL)
             (SETQ *MODE (PROGN (SETQ ALGLIST* (CONS NIL NIL)) MODE_BAK))
             (RETURN (NOT (ERRORP H))))))) 
(PUT 'ERR_CATCH_SOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_SOLVE 'DEFINED-ON-LINE '5803) 
(PUT 'ERR_CATCH_SOLVE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_SOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_SOLVE (EQS FL)
    (PROG (H)
      ((LAMBDA (*PROTFG)
         (SETQ H (ERRORSET (LIST 'SOLVEEVAL (MKQUOTE (LIST EQS FL))) NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (RETURN (COND ((ERRORP H) NIL) (T (CDAR H)))))) 
(PUT 'ERR_CATCH_ODESOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'ERR_CATCH_ODESOLVE 'DEFINED-ON-LINE '5813) 
(PUT 'ERR_CATCH_ODESOLVE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_ODESOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_ODESOLVE (EQS Y X)
    (PROG (H K BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_ODE))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_ODE)
      (SETQ K (SETKORDER NIL))
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'ODESOLVE (MKQUOTE (REVAL1 EQS T))
                        (MKQUOTE (REVAL1 Y T)) (MKQUOTE (REVAL1 X T)))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETKORDER K)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) (LIST 'LIST NIL)) (T (CAR H)))))) 
(PUT 'ERR_CATCH_MINSUB 'NUMBER-OF-ARGS 4) 
(PUT 'ERR_CATCH_MINSUB 'DEFINED-ON-LINE '5829) 
(PUT 'ERR_CATCH_MINSUB 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_MINSUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_MINSUB (PDES L1 COST_LIMIT NO_CASES)
    (PROG (H BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_MINSUB))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_MINSUB)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'SEARCH_SUBS (MKQUOTE PDES) (MKQUOTE L1)
                        (MKQUOTE COST_LIMIT) (MKQUOTE NO_CASES))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'ERR_CATCH_GB 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_GB 'DEFINED-ON-LINE '5844) 
(PUT 'ERR_CATCH_GB 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_GB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_GB (PDES)
    (PROG (H P BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_GB))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_GB)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'GROEBNERFEVAL
                        (MKQUOTE
                         (LIST
                          (CONS 'LIST
                                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ P PDES)
                                  (COND ((NULL P) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (LIST '*SQ (GET P 'SQVAL)
                                                            T))
                                                    (CAR P))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ P (CDR P))
                                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (P)
                                              (LIST '*SQ (GET P 'SQVAL) T))
                                            (CAR P))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                          (CONS 'LIST FTEM_)
                          (CONS 'LIST
                                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ P INEQ_)
                                  (COND ((NULL P) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (LIST '*SQ P T))
                                                    (CAR P))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ P (CDR P))
                                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (P) (LIST '*SQ P T))
                                            (CAR P))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(FLAG '(ERR_CATCH_SUB) 'OPFN) 
(PUT 'ERR_CATCH_SUB 'NUMBER-OF-ARGS 3) 
(PUT 'ERR_CATCH_SUB 'DEFINED-ON-LINE '5863) 
(PUT 'ERR_CATCH_SUB 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_SUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_SUB (H2 H6 H3)
    (PROG (H4 H5)
      (SETQ H4 (LIST 'EQUAL H2 H6))
      ((LAMBDA (*PROTFG)
         (SETQ H5
                 (ERRORSET
                  (LIST 'SUBEVAL (MKQUOTE (LIST (REVAL1 H4 T) (REVAL1 H3 T))))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (RETURN (COND ((ERRORP H5) NIL) (T (CAR H5)))))) 
(PUT 'ERR_CATCH_SUB_SQ 'PSOPFN 'ECS_SQ) 
(PUT 'ECS_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'ECS_SQ 'DEFINED-ON-LINE '5878) 
(PUT 'ECS_SQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ECS_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ECS_SQ (INP)
    (PROG (H2 H3 H5 H6)
      (COND
       ((NEQ (LENGTH INP) 3)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "SPLIT_SIMPLIFY DOES NOT HAVE 3 ARGUMENTS.") NIL)
         NIL)))
      (SETQ H2 (REVAL1 (CAR INP) T))
      (SETQ H6 (REVAL1 (CADR INP) NIL))
      (SETQ H3 (CADR (REVAL1 (CADDR INP) NIL)))
      ((LAMBDA (*PROTFG)
         (SETQ H5
                 (ERRORSET
                  (LIST 'SUBSQ (MKQUOTE H3) (MKQUOTE (LIST (CONS H2 H6)))) NIL
                  NIL)))
       T)
      (SETQ ERFG* NIL)
      (RETURN (COND ((ERRORP H5) NIL) (T (LIST '*SQ (CAR H5) T)))))) 
(FLAG '(ERR_CATCH_INT) 'OPFN) 
(PUT 'ERR_CATCH_INT 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_INT 'DEFINED-ON-LINE '5902) 
(PUT 'ERR_CATCH_INT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_INT (H2 H3)
    (PROG (H5 BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_INT))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_INT)
      ((LAMBDA (*PROTFG)
         (SETQ H5
                 (ERRORSET
                  (LIST 'SIMPINT (MKQUOTE (LIST (REVAL1 H2 T) (REVAL1 H3 T))))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H5) NIL) (T (PREPSQ (CAR H5))))))) 
(PUT 'ERR_CATCH_REVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_REVAL 'DEFINED-ON-LINE '5927) 
(PUT 'ERR_CATCH_REVAL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_REVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_REVAL (H)
    (PROG (H2 BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_REVAL))
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_REVAL)
      ((LAMBDA (*PROTFG)
         (SETQ H2 (ERRORSET (LIST 'REVAL (MKQUOTE H)) NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H2) NIL) (T (CAR H2)))))) 
(PUT 'CHECK_STOP 'NUMBER-OF-ARGS 0) 
(PUT 'CHECK_STOP 'DEFINED-ON-LINE '5942) 
(PUT 'CHECK_STOP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHECK_STOP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHECK_STOP NIL
    (COND
     ((FILEP "stop_now")
      (PROGN
       (SETQ *BATCH_MODE NIL)
       (SETQ OLD_HISTORY NIL)
       (SETQ BATCHCOUNT_ (SUB1 STEPCOUNTER_))
       (SETQ REPEAT_MODE 1)
       NIL)))) 
(PUT 'AFTERGCUSERHOOK1 'NUMBER-OF-ARGS 0) 
(PUT 'AFTERGCUSERHOOK1 'DEFINED-ON-LINE '5953) 
(PUT 'AFTERGCUSERHOOK1 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'AFTERGCUSERHOOK1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE AFTERGCUSERHOOK1 NIL
    (PROG (LI)
      (SETQ LI
              (LIST 'MAX_GC_ELIMIN 'MAX_GC_FAC 'MAX_GC_GB 'MAX_GC_INT
                    'MAX_GC_MINSUB 'MAX_GC_ODE 'MAX_GC_RED_LEN 'MAX_GC_SHORT
                    'MAX_GC_REVAL 'MAX_GC_SS))
      (SETQ MY_GC_COUNTER (ADD1 MY_GC_COUNTER))
      (COND
       ((AND *GC (MEMBER BACKUP_ LI))
        (PROGN
         (PROGN
          (PRIN2 BACKUP_)
          (PRIN2 " : ")
          (PRIN2
           (COND ((EQUAL BACKUP_ 'MAX_GC_ELIMIN) MAX_GC_ELIMIN)
                 ((EQUAL BACKUP_ 'MAX_GC_FAC) MAX_GC_FAC)
                 ((EQUAL BACKUP_ 'MAX_GC_GB) MAX_GC_GB)
                 ((EQUAL BACKUP_ 'MAX_GC_INT) MAX_GC_INT)
                 ((EQUAL BACKUP_ 'MAX_GC_MINSUB) MAX_GC_MINSUB)
                 ((EQUAL BACKUP_ 'MAX_GC_ODE) MAX_GC_ODE)
                 ((EQUAL BACKUP_ 'MAX_GC_RED_LEN) MAX_GC_RED_LEN)
                 ((EQUAL BACKUP_ 'MAX_GC_SHORT) MAX_GC_SHORT)
                 ((EQUAL BACKUP_ 'MAX_GC_REVAL) MAX_GC_REVAL)
                 ((EQUAL BACKUP_ 'MAX_GC_SS) MAX_GC_SS)))
          (PRIN2 "  max # of GC's left to do: ")
          (PRIN2 (PLUS 1 (DIFFERENCE MAX_GC_COUNTER MY_GC_COUNTER)))
          NIL)
         (TERPRI))))
      (COND
       ((AND (MEMBER BACKUP_ LI)
             (OR (GREATERP MY_GC_COUNTER MAX_GC_COUNTER)
                 (LESSP LAST_FREE_CELLS 100000)))
        (PROGN
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "Stop of ")
             (PRIN2
              (COND ((EQUAL BACKUP_ 'MAX_GC_ELIMIN) "an elimination")
                    ((EQUAL BACKUP_ 'MAX_GC_FAC) "a factorization")
                    ((EQUAL BACKUP_ 'MAX_GC_GB) "a groebner basis computation")
                    ((EQUAL BACKUP_ 'MAX_GC_INT) "an integration")
                    ((EQUAL BACKUP_ 'MAX_GC_MINSUB)
                     "a minimal growth substitution")
                    ((EQUAL BACKUP_ 'MAX_GC_ODE) "solving an ODE")
                    ((EQUAL BACKUP_ 'MAX_GC_RED_LEN)
                     "a length reducing decoupling step")
                    ((EQUAL BACKUP_ 'MAX_GC_SHORT) "a shortening step")
                    ((EQUAL BACKUP_ 'MAX_GC_REVAL) "a simplification")
                    ((EQUAL BACKUP_ 'MAX_GC_SS) "searching a sub-system")
                    (T "an unknown step")))
             (PRIN2 " due to ")
             (PRIN2
              (COND
               ((LESSP LAST_FREE_CELLS 100000) "less than 100000 free cells.")
               (T "reaching the limit of garbage collections.")))
             NIL)
            (TERPRI)
            NIL)))
         (REDERR "Heidadeife ")))
       ((AND PRINT_ (LESSP LAST_FREE_CELLS 100000))
        (PROGN
         (PRIN2 "Memory seems to run out. Less than 100000 free cells!")
         NIL))))) 
(FLAG '(ERR_CATCH_FAC) 'OPFN) 
(PUT 'ERR_CATCH_FAC 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_FAC 'DEFINED-ON-LINE '6046) 
(PUT 'ERR_CATCH_FAC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_FAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_FAC (A)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK MODULAR_BAK NO_POWERS_CHANGED
           RATIONAL_CHANGED)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_FAC))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_FAC)
      (COND
       ((NULL *NOPOWERS)
        (PROGN (AEVAL (ON (LIST 'NOPOWERS))) (SETQ NO_POWERS_CHANGED T))))
      (COND
       ((AND (NULL *RATIONAL) (NOT (FREEOF A '|:RN:|))
             (OR (NULL *COMPLEX) (NOT (FREEOF A '|:GI:|))))
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (ON (LIST 'RATIONAL)))
         (ON (LIST 'MSG))
         (SETQ RATIONAL_CHANGED T))))
      (COND
       ((OR (AND MODULAR_COMP (NOT (FREEOF A '|:MOD:|))) *MODULAR)
        (PROGN
         (SETQ MODULAR_BAK *MODULAR)
         (COND (*MODULAR (OFF (LIST 'MODULAR))))
         ((LAMBDA (*PROTFG)
            (SETQ H
                    (ERRORSET
                     (LIST 'REVAL
                           (LIST 'FACTORIZE
                                 (MKQUOTE (MK*SQ (RESIMP (SIMP A))))))
                     NIL NIL)))
          T)
         (COND (MODULAR_BAK (ON (LIST 'MODULAR))))))
       (T
        ((LAMBDA (*PROTFG)
           (SETQ H
                   (ERRORSET (LIST 'REVAL (LIST 'FACTORIZE (MKQUOTE A))) NIL
                             NIL)))
         T)))
      (COND (MODULAR_BAK (ON (LIST 'MODULAR))))
      (COND
       (RATIONAL_CHANGED
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (OFF (LIST 'RATIONAL)))
         (ON (LIST 'MSG)))))
      (COND (NO_POWERS_CHANGED (AEVAL (OFF (LIST 'NOPOWERS)))))
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN
       (COND
        ((OR (ERRORP H)
             (AND (PAIRP H) (PAIRP (CAR H)) (CDAR H) (NULL (CADAR H))))
         (LIST 'LIST A))
        (T (CAR H)))))) 
(PUT 'ERR_CATCH_FAC2 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_FAC2 'DEFINED-ON-LINE '6093) 
(PUT 'ERR_CATCH_FAC2 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_FAC2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_FAC2 (A)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK NO_POWERS_CHANGED
           RATIONAL_CHANGED MODULAR_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_FAC))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_FAC)
      (COND
       (*NOPOWERS
        (PROGN (AEVAL (OFF (LIST 'NOPOWERS))) (SETQ NO_POWERS_CHANGED T))))
      (COND
       ((AND (NULL *RATIONAL) (NOT (FREEOF A '|:RN:|))
             (OR (NULL *COMPLEX) (NOT (FREEOF A '|:GI:|))))
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (ON (LIST 'RATIONAL)))
         (ON (LIST 'MSG))
         (SETQ RATIONAL_CHANGED T))))
      (COND
       ((OR (AND MODULAR_COMP (NOT (FREEOF A '|:MOD:|))) *MODULAR)
        (PROGN
         (SETQ MODULAR_BAK *MODULAR)
         (COND (*MODULAR (OFF (LIST 'MODULAR))))
         ((LAMBDA (*PROTFG)
            (SETQ H
                    (ERRORSET
                     (LIST 'FACTORIZE (MKQUOTE (MK*SQ (RESIMP (SIMP A))))) NIL
                     NIL)))
          T)
         (COND (MODULAR_BAK (ON (LIST 'MODULAR))))))
       (T
        ((LAMBDA (*PROTFG)
           (SETQ H (ERRORSET (LIST 'FACTORIZE (MKQUOTE A)) NIL NIL)))
         T)))
      (COND
       (RATIONAL_CHANGED
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (OFF (LIST 'RATIONAL)))
         (ON (LIST 'MSG)))))
      (COND (NO_POWERS_CHANGED (AEVAL (ON (LIST 'NOPOWERS)))))
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN
       (COND
        ((OR (ERRORP H)
             (AND (PAIRP H) (PAIRP (CAR H)) (CDAR H) (NULL (CADAR H))))
         (LIST 'LIST (LIST 'LIST A 1)))
        (T (CAR H)))))) 
(PUT 'ERR_CATCH_FAC3 'NUMBER-OF-ARGS 1) 
(PUT 'ERR_CATCH_FAC3 'DEFINED-ON-LINE '6139) 
(PUT 'ERR_CATCH_FAC3 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_FAC3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERR_CATCH_FAC3 (A)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK NO_POWERS_CHANGED
           RATIONAL_CHANGED MODULAR_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_FAC))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_FAC)
      (COND
       (*NOPOWERS
        (PROGN (AEVAL (OFF (LIST 'NOPOWERS))) (SETQ NO_POWERS_CHANGED T))))
      (COND
       ((AND (NULL *RATIONAL) (NOT (FREEOF A '|:RN:|))
             (OR (NULL *COMPLEX) (NOT (FREEOF A '|:GI:|))))
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (ON (LIST 'RATIONAL)))
         (ON (LIST 'MSG))
         (SETQ RATIONAL_CHANGED T))))
      (COND
       ((OR (AND MODULAR_COMP (NOT (FREEOF A '|:MOD:|))) *MODULAR)
        (PROGN
         (SETQ MODULAR_BAK *MODULAR)
         (COND (*MODULAR (OFF (LIST 'MODULAR))))
         ((LAMBDA (*PROTFG)
            (SETQ H
                    (ERRORSET (LIST 'FCTRF (MKQUOTE (CAR (RESIMP (CONS A 1)))))
                              NIL NIL)))
          T)
         (COND (MODULAR_BAK (ON (LIST 'MODULAR))))))
       (T
        ((LAMBDA (*PROTFG)
           (SETQ H (ERRORSET (LIST 'FCTRF (MKQUOTE A)) NIL NIL)))
         T)))
      (COND
       (RATIONAL_CHANGED
        (PROGN
         (OFF (LIST 'MSG))
         (AEVAL (OFF (LIST 'RATIONAL)))
         (ON (LIST 'MSG)))))
      (COND (NO_POWERS_CHANGED (AEVAL (ON (LIST 'NOPOWERS)))))
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) (CONS 1 NIL)) (T (CAR H)))))) 
(PUT 'ERR_CATCH_GCD 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_GCD 'DEFINED-ON-LINE '6183) 
(PUT 'ERR_CATCH_GCD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_GCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_GCD (A B)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_FAC))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_FAC)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'AEVAL (LIST 'LIST ''GCD (MKQUOTE A) (MKQUOTE B))) NIL
                  NIL)))
       T)
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) (LIST '*SQ (CONS 1 1) T)) (T (CAR H)))))) 
(PUT 'ERR_CATCH_PREDUCE 'NUMBER-OF-ARGS 2) 
(PUT 'ERR_CATCH_PREDUCE 'DEFINED-ON-LINE '6206) 
(PUT 'ERR_CATCH_PREDUCE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ERR_CATCH_PREDUCE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_PREDUCE (A B)
    (PROG (H K)
      (SETQ K (SETKORDER NIL))
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'AEVAL
                        (MKQUOTE (LIST 'PREDUCE (MKQUOTE A) (MKQUOTE B))))
                  NIL NIL)))
       T)
      (SETQ ERFG* NIL)
      (SETKORDER K)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'FIND_FACTORIZATION 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_FACTORIZATION 'DEFINED-ON-LINE '6218) 
(PUT 'FIND_FACTORIZATION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FIND_FACTORIZATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_FACTORIZATION (ARGLIST)
    (PROG (L G H K M NEW_SQVAL FS DROPPED_FACTORS MB PDES PDECP DROPPED_EQN)
      (SETQ PDES (CAR ARGLIST))
      (COND (EXPERT_MODE (SETQ L (SELECTPDES PDES 1)))
            (T (SETQ L (CADDDR ARGLIST))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND L (NULL FS) (NULL CONTRADICTION_) (NULL DROPPED_EQN)))
          (RETURN NIL)))
        (PROGN
         (SETQ H (GET (CAR L) 'FAC))
         (COND
          ((OR (NULL H) (AND (FIXP H) (LESSP H 2)))
           (PROGN
            (SETQ H
                    (CDR
                     (ERR_CATCH_FAC2
                      (LIST '*SQ (CONS (CAR (GET (CAR L) 'SQVAL)) 1) T))))
            (COND
             ((AND (PAIRP H) (OR (CDR H) (GREATERP (CADDAR H) 1)))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND H (NULL DROPPED_EQN))) (RETURN NIL)))
                (PROGN
                 (SETQ G (SIMP (CADAR H)))
                 (COND
                  ((OR (ATOM (CAR G)) (ATOM (CAR (CAR G)))) (SETQ H (CDR H)))
                  (T
                   (PROGN
                    (SETQ MB (CAN_NOT_BECOME_ZEROSQ G FTEM_))
                    (COND
                     ((OR (GREATERP (CADDAR H) 1) MB)
                      (PROGN
                       (SETQ DROPPED_FACTORS T)
                       (COND
                        ((NULL NEW_SQVAL)
                         (SETQ NEW_SQVAL (GET (CAR L) 'SQVAL))))
                       (SETQ K (CADDAR H))
                       (COND ((NOT MB) (SETQ K (SUB1 K))))
                       (PROG (M)
                         (SETQ M 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE K M)) (RETURN NIL)))
                         (SETQ NEW_SQVAL (MULTSQ NEW_SQVAL (INVSQ G)))
                         (SETQ M (PLUS2 M 1))
                         (GO LAB)))))
                    (COND (MB (SETQ H (CDR H)))
                          (T
                           (PROGN
                            (SETQ G (CAR (CADR (CADAR H))))
                            (SETQ K (NO_OF_TM_SF G))
                            (SETQ PDECP PDES)
                            (PROG ()
                             WHILELABEL
                              (COND ((NOT (AND PDECP H)) (RETURN NIL)))
                              (COND
                               ((AND (EQUAL (GET (CAR PDECP) 'TERMS) K)
                                     (NEQ (CAR PDECP) (CAR L))
                                     (OR
                                      (EQUAL G (CAR (GET (CAR PDECP) 'SQVAL)))
                                      (EQUAL G
                                             (CAR
                                              (NEGSQ
                                               (GET (CAR PDECP) 'SQVAL))))))
                                (PROGN
                                 (SETQ DROPPED_EQN (CAR PDECP))
                                 (SETQ H NIL)))
                               (T (SETQ PDECP (CDR PDECP))))
                              (GO WHILELABEL))
                            (COND
                             (H
                              (PROGN
                               (SETQ FS (CONS (CONS G 1) FS))
                               (SETQ H (CDR H))))))))))))
                (GO WHILELABEL))))
            (COND
             (DROPPED_EQN
              (PROGN
               (SETQ PDES
                       (DROP_PDE (CAR L) PDES
                        (LIST 'TIMES DROPPED_EQN
                              (LIST 'QUOTIENT (PREPSQ (GET (CAR L) 'SQVAL))
                                    (PREPSQ (GET DROPPED_EQN 'SQVAL))))))
               (DROP_PDE_FROM_PROPERTIES (CAR L) PDES)))
             ((AND (NULL DROPPED_FACTORS) (OR (NULL FS) (NULL (CDR FS))))
              (PROGN (SETQ FS NIL) (PUT (CAR L) 'FAC 2)))
             ((NULL DROPPED_FACTORS) (PUT (CAR L) 'FAC FS))
             (T
              (PROGN
               (PROG (F)
                 (SETQ F ALLFLAGS_)
                LAB
                 (COND ((NULL F) (RETURN NIL)))
                 ((LAMBDA (F) (FLAG (LIST (CAR L)) F)) (CAR F))
                 (SETQ F (CDR F))
                 (GO LAB))
               (COND (RECORD_HIST (SETQ H (GET (CAR L) 'SQVAL))))
               (UPDATESQ (CAR L) NEW_SQVAL FS NIL (GET (CAR L) 'FCTS)
                (GET (CAR L) 'VARS) T (LIST 0) PDES)
               (DROP_PDE_FROM_IDTIES (CAR L) PDES
                (COND
                 (RECORD_HIST
                  (REVAL1
                   (LIST 'TIMES (GET (CAR L) 'HIST)
                         (LIST 'QUOTIENT (PREPSQ (GET (CAR L) 'SQVAL))
                               (PREPSQ H)))
                   T))
                 (T NIL)))
               (DROP_PDE_FROM_PROPERTIES (CAR L) PDES)
               (COND
                ((NULL CONTRADICTION_)
                 (SETQ PDES (EQINSERT (CAR L) (DELETE (CAR L) PDES)))))
               NIL))))))
         (COND
          ((AND PRINT_ (OR (OR FS DROPPED_EQN) CONTRADICTION_))
           (PROGN
            (PROGN
             (PRIN2 "Equation ")
             (PRIN2 (CAR L))
             (PRIN2 " factorized.")
             NIL)
            (TERPRI)
            (COND
             (CONTRADICTION_
              (PROGN (PRIN2 "This leads to a contradiction!") NIL))
             (DROPPED_EQN
              (PROGN
               (PRIN2 "It is a consequence of ")
               (PRIN2 DROPPED_EQN)
               (PRIN2 ".")
               NIL))))))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN
       (COND (CONTRADICTION_ NIL)
             ((OR DROPPED_EQN DROPPED_FACTORS) (LIST PDES (CADR ARGLIST)))
             (FS ARGLIST))))) 
(PUT 'LEADING_FACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'LEADING_FACTORS 'DEFINED-ON-LINE '6309) 
(PUT 'LEADING_FACTORS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LEADING_FACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEADING_FACTORS (U)
    (PROG (FLI V W)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (PAIRP U) (NULL (CDR U))
                (NOT (OR (ATOM (CAR U)) (ATOM (CAR (CAR U)))))))
          (RETURN NIL)))
        (PROGN
         (SETQ FLI (CONS (CAR (MKSQ (CAAAR U) (CDAAR U))) FLI))
         (SETQ U (CDAR U)))
        (GO WHILELABEL))
      (COND ((OR (ATOM U) (ATOM (CAR U))) (SETQ FLI (CONS U FLI)))
            (T
             (PROGN
              (SETQ V U)
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND (PAIRP V)
                        (NOT (OR (ATOM (CAR V)) (ATOM (CAR (CAR V)))))))
                  (RETURN NIL)))
                (SETQ V (CDAR V))
                (GO WHILELABEL))
              (COND ((EQUAL V 1) (SETQ FLI (CONS U FLI)))
                    (T
                     (PROGN
                      (SETQ W (MULTSQ (CONS U 1) (INVSQ (CONS V 1))))
                      (COND
                       ((EQUAL (CDR W) 1)
                        (SETQ FLI (CONS (CAR W) (CONS V FLI))))
                       (T (SETQ FLI (CONS U FLI))))))))))
      (RETURN FLI))) 
(PUT 'SFFAC 'NUMBER-OF-ARGS 1) 
(PUT 'SFFAC 'DEFINED-ON-LINE '6339) 
(PUT 'SFFAC 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SFFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFFAC (U)
    (PROG (U1 U2 U3 FLI V)
      (SETQ FLI (LEADING_FACTORS U))
      (SETQ U (CAR FLI))
      (SETQ FLI (CDR FLI))
      (COND
       ((NOT (OR (ATOM U) (ATOM (CAR U))))
        (PROGN
         (SETQ V (CAAAR U))
         (SETQ U1 (CDR (COMFAC U)))
         (COND
          ((AND (OR (ATOM U1) (ATOM (CAR U1))) (NEQ U 1))
           (SETQ FLI
                   (CONS (CAR (MULTSQ (CONS U 1) (INVSQ (CONS U1 1))))
                         (CONS U1 FLI))))
          (T
           (PROGN
            (SETQ U2 (SFFAC U1))
            (COND ((NULL U2) (SETQ U2 (LIST U1))))
            (PROG (U3)
              (SETQ U3 U2)
             LAB
              (COND ((NULL U3) (RETURN NIL)))
              ((LAMBDA (U3)
                 (PROGN
                  (SETQ V (MULTSQ (CONS U 1) (INVSQ (CONS U3 1))))
                  (COND
                   ((EQUAL (CDR V) 1)
                    (PROGN (SETQ FLI (CONS U3 FLI)) (SETQ U (CAR V)))))))
               (CAR U3))
              (SETQ U3 (CDR U3))
              (GO LAB))
            (SETQ FLI (CONS U FLI))))))))
      (RETURN FLI))) 
(PUT 'COUNTIDS 'NUMBER-OF-ARGS 0) 
(PUT 'COUNTIDS 'DEFINED-ON-LINE '6405) 
(PUT 'COUNTIDS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'COUNTIDS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COUNTIDS NIL (LENGTH (OBLIST))) 
(FLAG '(LOW_MEM) 'OPFN) 
(PUT 'LOW_MEM 'NUMBER-OF-ARGS 0) 
(PUT 'LOW_MEM 'DEFINED-ON-LINE '6418) 
(PUT 'LOW_MEM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LOW_MEM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LOW_MEM NIL
    (COND
     ((NOT (GETD 'OLDRECLAIM))
      (PROGN (COPYD 'OLDRECLAIM '%RECLAIM) (COPYD '%RECLAIM 'NEWRECLAIM) NIL)))) 
(FLAG '(POLYANSATZ) 'OPFN) 
(PUT 'POLYANSATZ 'NUMBER-OF-ARGS 5) 
(PUT 'POLYANSATZ 'DEFINED-ON-LINE '6425) 
(PUT 'POLYANSATZ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'POLYANSATZ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYANSATZ (EV IV FN DEGRE HOMO)
    (PROG (A FI EL1 EL2 F FL P PR)
      (SETQ A
              (REVAL1
               (LIST 'EXPT
                     (CONS 'PLUS (COND (HOMO (CDR EV)) (T (CONS 1 (CDR EV)))))
                     DEGRE)
               T))
      (SETQ A (REVERSE (CDR A)))
      (SETQ FI 0)
      (SETQ IV (CDR IV))
      (PROG (EL1 FORALL-RESULT FORALL-ENDPTR)
        (SETQ EL1 A)
        (COND ((NULL EL1) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL1)
                            (PROGN
                             (COND
                              ((OR (NOT (PAIRP EL1)) (NEQ (CAR EL1) 'TIMES))
                               (SETQ EL1 (LIST EL1)))
                              (T (SETQ EL1 (CDR EL1))))
                             (SETQ F (NEWFCT FN IV FI))
                             (SETQ FI (ADD1 FI))
                             (SETQ FL (CONS F FL))
                             (SETQ PR (LIST F))
                             (PROG (EL2)
                               (SETQ EL2 EL1)
                              LAB
                               (COND ((NULL EL2) (RETURN NIL)))
                               ((LAMBDA (EL2)
                                  (COND
                                   ((NOT (FIXP EL2)) (SETQ PR (CONS EL2 PR)))))
                                (CAR EL2))
                               (SETQ EL2 (CDR EL2))
                               (GO LAB))
                             (COND
                              ((GREATERP (LENGTH PR) 1)
                               (SETQ PR (CONS 'TIMES PR)))
                              (T (SETQ PR (CAR PR))))
                             (SETQ P (CONS PR P))))
                          (CAR EL1))
                         NIL)))
       LOOPLABEL
        (SETQ EL1 (CDR EL1))
        (COND ((NULL EL1) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (EL1)
                    (PROGN
                     (COND
                      ((OR (NOT (PAIRP EL1)) (NEQ (CAR EL1) 'TIMES))
                       (SETQ EL1 (LIST EL1)))
                      (T (SETQ EL1 (CDR EL1))))
                     (SETQ F (NEWFCT FN IV FI))
                     (SETQ FI (ADD1 FI))
                     (SETQ FL (CONS F FL))
                     (SETQ PR (LIST F))
                     (PROG (EL2)
                       (SETQ EL2 EL1)
                      LAB
                       (COND ((NULL EL2) (RETURN NIL)))
                       ((LAMBDA (EL2)
                          (COND ((NOT (FIXP EL2)) (SETQ PR (CONS EL2 PR)))))
                        (CAR EL2))
                       (SETQ EL2 (CDR EL2))
                       (GO LAB))
                     (COND
                      ((GREATERP (LENGTH PR) 1) (SETQ PR (CONS 'TIMES PR)))
                      (T (SETQ PR (CAR PR))))
                     (SETQ P (CONS PR P))))
                  (CAR EL1))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (SETQ P (REVAL1 (CONS 'PLUS P) T))
      (RETURN (LIST 'LIST P (CONS 'LIST FL))))) 
(FLAG '(POLYANS) 'OPFN) 
(PUT 'POLYANS 'NUMBER-OF-ARGS 6) 
(PUT 'POLYANS 'DEFINED-ON-LINE '6457) 
(PUT 'POLYANS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'POLYANS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE POLYANS (ORDR DGR X Y D_Y FN)
    (PROG (LL FL A I F)
      (SETQ I (SUB1 ORDR))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP I 0)) (RETURN NIL)))
        (PROGN (SETQ LL (CONS (LIST D_Y I) LL)) (SETQ I (SUB1 I)))
        (GO WHILELABEL))
      (SETQ LL (CONS Y LL))
      (SETQ LL (REVERSE (CONS X LL)))
      (SETQ FL NIL)
      (SETQ I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ I DGR)) (RETURN NIL)))
        (PROGN
         (SETQ F (NEWFCT FN LL I))
         (SETQ FL (CONS F FL))
         (SETQ A (LIST 'PLUS (LIST 'TIMES F (LIST 'EXPT (LIST D_Y ORDR) I)) A))
         (SETQ I (ADD1 I)))
        (GO WHILELABEL))
      (RETURN (LIST 'LIST (REVAL1 A T) (CONS 'LIST FL))))) 
(FLAG '(SEPANS) 'OPFN) 
(PUT 'SEPANS 'NUMBER-OF-ARGS 4) 
(PUT 'SEPANS 'DEFINED-ON-LINE '6483) 
(PUT 'SEPANS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SEPANS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SEPANS (KIND V1 V2 FN)
    (PROG (N VL1 VL2 H1 H2 H3 H4 FL)
      (COND
       ((EQUAL (CDR V1) NIL) (PROGN (SETQ VL1 (CDR V2)) (SETQ VL2 (CDR V2))))
       (T
        (PROGN
         (SETQ VL1 (CONS (CADR V1) (CDR V2)))
         (SETQ VL2 (APPEND (CDDR V1) (CDR V2))))))
      (RETURN
       (COND
        ((EQUAL KIND 0)
         (PROGN
          (SETQ VL1 (APPEND (CDR V1) (CDR V2)))
          (SETQ H1 (NEWFCT FN VL1 '_))
          (LIST 'LIST H1 (LIST 'LIST H1))))
        ((EQUAL KIND 1)
         (PROGN (SETQ H1 (NEWFCT FN VL1 1)) (LIST 'LIST H1 (LIST 'LIST H1))))
        ((EQUAL KIND 2)
         (PROGN (SETQ H1 (NEWFCT FN VL2 1)) (LIST 'LIST H1 (LIST 'LIST H1))))
        ((EQUAL KIND 3)
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (LIST 'LIST (REVAL1 (LIST 'PLUS H1 H2) T) (LIST 'LIST H1 H2))))
        ((EQUAL KIND 4)
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (LIST 'LIST (REVAL1 (LIST 'TIMES H1 H2) T) (LIST 'LIST H1 H2))))
        ((EQUAL KIND 5)
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (SETQ H3 (NEWFCT FN VL1 3))
          (LIST 'LIST (REVAL1 (LIST 'PLUS (LIST 'TIMES H1 H2) H3) T)
                (LIST 'LIST H1 H2 H3))))
        ((EQUAL KIND 6)
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (SETQ H3 (NEWFCT FN VL2 3))
          (LIST 'LIST (REVAL1 (LIST 'PLUS (LIST 'TIMES H1 H2) H3) T)
                (LIST 'LIST H1 H2 H3))))
        ((EQUAL KIND 7)
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (SETQ H3 (NEWFCT FN VL1 3))
          (SETQ H4 (NEWFCT FN VL2 4))
          (LIST 'LIST (REVAL1 (LIST 'PLUS (LIST 'TIMES H1 H2) H3 H4) T)
                (LIST 'LIST H1 H2 H3 H4))))
        ((EQUAL KIND 8)
         (PROGN
          (SETQ N 1)
          (SETQ VL1 (CDR V1))
          (SETQ VL2 (CDR V2))
          (SETQ FL NIL)
          (PROG ()
           WHILELABEL
            (COND ((NOT (NEQ VL1 NIL)) (RETURN NIL)))
            (PROGN
             (SETQ H1 (NEWFCT FN (CONS (CAR VL1) VL2) N))
             (SETQ VL1 (CDR VL1))
             (SETQ FL (CONS H1 FL))
             (SETQ N (PLUS N 1)))
            (GO WHILELABEL))
          (LIST 'LIST (CONS 'PLUS FL) (CONS 'LIST FL))))
        (T
         (PROGN
          (SETQ H1 (NEWFCT FN VL1 1))
          (SETQ H2 (NEWFCT FN VL2 2))
          (SETQ H3 (NEWFCT FN VL1 3))
          (SETQ H4 (NEWFCT FN VL2 4))
          (LIST 'LIST
                (REVAL1 (LIST 'PLUS (LIST 'TIMES H1 H2) (LIST 'TIMES H3 H4)) T)
                (LIST 'LIST H1 H2 H3 H4)))))))) 
(PUT 'CHANGE_DERIVS_ORDERING 'NUMBER-OF-ARGS 3) 
(PUT 'CHANGE_DERIVS_ORDERING 'DEFINED-ON-LINE '6569) 
(PUT 'CHANGE_DERIVS_ORDERING 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHANGE_DERIVS_ORDERING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGE_DERIVS_ORDERING (PDES FL VL)
    (PROG (P DL)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (COND
             (TR_ORDERINGS
              (PROGN
               (TERPRI)
               (PROGN (PRIN2 "Old: ") (PRIN2 (GET P 'DERIVS)) NIL)
               NIL)))
            (SETQ DL (SORT_DERIVS (GET P 'DERIVS) FL VL))
            (COND
             (TR_ORDERINGS
              (PROGN (TERPRI) (PROGN (PRIN2 "New: ") (PRIN2 DL) NIL) NIL)))
            (PUT P 'DERIVS DL)
            (PUT P 'DEC_WITH NIL)
            (PUT P 'DEC_WITH_RL NIL)
            (FLAG (LIST P) 'TO_SEPARANT)
            NIL))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN PDES))) 
(PUT 'SORT_ACCORDING_TO 'NUMBER-OF-ARGS 2) 
(PUT 'SORT_ACCORDING_TO 'DEFINED-ON-LINE '6590) 
(PUT 'SORT_ACCORDING_TO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SORT_ACCORDING_TO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SORT_ACCORDING_TO (R S)
    (PROG (SS H)
      (PROG (SS)
        (SETQ SS S)
       LAB
        (COND ((NULL SS) (RETURN NIL)))
        ((LAMBDA (SS) (COND ((MEMBER SS R) (SETQ H (CONS SS H))))) (CAR SS))
        (SETQ SS (CDR SS))
        (GO LAB))
      (RETURN (REVERSE H)))) 
(PUT 'A_BEFORE_B_ACCORDING_TO_C 'NUMBER-OF-ARGS 3) 
(PUT 'A_BEFORE_B_ACCORDING_TO_C 'DEFINED-ON-LINE '6599) 
(PUT 'A_BEFORE_B_ACCORDING_TO_C 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'A_BEFORE_B_ACCORDING_TO_C 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE A_BEFORE_B_ACCORDING_TO_C (A B S)
    (COND ((NOT (PAIRP S)) NIL) ((EQUAL B (CAR S)) NIL) ((EQUAL A (CAR S)) T)
          (T (A_BEFORE_B_ACCORDING_TO_C A B (CDR S))))) 
(PUT 'CHANGE_FCTS_ORDERING 'NUMBER-OF-ARGS 3) 
(PUT 'CHANGE_FCTS_ORDERING 'DEFINED-ON-LINE '6606) 
(PUT 'CHANGE_FCTS_ORDERING 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHANGE_FCTS_ORDERING 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGE_FCTS_ORDERING (NEWLI PDES VL)
    (PROG (S)
      (SETQ FTEM_ NEWLI)
      (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
      (PROG (S)
        (SETQ S PDES)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (PUT S 'FCTS (SORT_ACCORDING_TO (GET S 'FCTS) FTEM_))
            (PUT S 'ALLVARFCTS (SORT_ACCORDING_TO (GET S 'ALLVARFCTS) FTEM_))
            NIL))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ PDES (CHANGE_DERIVS_ORDERING PDES FTEM_ VL))
      (COND
       (TR_ORDERINGS
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "New functions list: ") (PRIN2 FTEM_) NIL)
         NIL))))) 
(PUT 'SEARCH_LI 'NUMBER-OF-ARGS 2) 
(PUT 'SEARCH_LI 'DEFINED-ON-LINE '6621) 
(PUT 'SEARCH_LI 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SEARCH_LI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEARCH_LI (L CARE)
    (COND
     ((PAIRP L)
      (COND ((EQUAL (CAR L) CARE) (LIST (CADR L)))
            (T
             (PROG (B RESUL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (PAIRP L)) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((SETQ B (SEARCH_LI (CAR L) CARE))
                    (SETQ RESUL (UNION B RESUL))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (RETURN RESUL))))))) 
(PUT 'SEARCH_LI2 'NUMBER-OF-ARGS 2) 
(PUT 'SEARCH_LI2 'DEFINED-ON-LINE '6634) 
(PUT 'SEARCH_LI2 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SEARCH_LI2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEARCH_LI2 (L CARE)
    (COND
     ((PAIRP L)
      (COND ((EQUAL (CAR L) CARE) (LIST L))
            (T
             (PROG (B RESUL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (PAIRP L)) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((SETQ B (SEARCH_LI2 (CAR L) CARE))
                    (SETQ RESUL (UNION B RESUL))))
                  (SETQ L (CDR L)))
                 (GO WHILELABEL))
               (RETURN RESUL))))))) 
(FLAG '(FILTER) 'OPFN) 
(PUT 'FILTER 'NUMBER-OF-ARGS 2) 
(PUT 'FILTER 'DEFINED-ON-LINE '6650) 
(PUT 'FILTER 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FILTER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FILTER (L CARE) (CONS 'LIST (SEARCH_LI2 L CARE))) 
(FLAG '(BACKUP_REDUCE_FLAGS) 'OPFN) 
(PUT 'BACKUP_REDUCE_FLAGS 'NUMBER-OF-ARGS 0) 
(PUT 'BACKUP_REDUCE_FLAGS 'DEFINED-ON-LINE '6654) 
(PUT 'BACKUP_REDUCE_FLAGS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'BACKUP_REDUCE_FLAGS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE BACKUP_REDUCE_FLAGS NIL
    (PROG ()
      (SETQ *DFPRINT_BAK (CONS *DFPRINT *DFPRINT_BAK))
      (SETQ *EXP_BAK (CONS *EXP *EXP_BAK))
      (SETQ *EZGCD_BAK (CONS *EZGCD *EZGCD_BAK))
      (SETQ *FULLROOTS_BAK (CONS *FULLROOTS *FULLROOTS_BAK))
      (SETQ *GCD_BAK (CONS *GCD *GCD_BAK))
      (SETQ *MCD_BAK (CONS *MCD *MCD_BAK))
      (SETQ *RATARG_BAK (CONS *RATARG *RATARG_BAK))
      (SETQ *RATIONAL_BAK (CONS *RATIONAL *RATIONAL_BAK))
      (COND ((NULL *DFPRINT) (AEVAL (ON (LIST 'DFPRINT)))))
      (COND ((NULL *EXP) (AEVAL (ON (LIST 'EXP)))))
      (COND ((NULL *EZGCD) (AEVAL (ON (LIST 'EZGCD)))))
      (COND ((NULL *FULLROOTS) (AEVAL (ON (LIST 'FULLROOTS)))))
      (COND (*GCD (AEVAL (OFF (LIST 'GCD)))))
      (COND ((NULL *MCD) (AEVAL (ON (LIST 'MCD)))))
      (COND ((NULL *RATARG) (AEVAL (ON (LIST 'RATARG)))))
      (SETQ *NOPOWERS_BAK (CONS *NOPOWERS *NOPOWERS_BAK))
      (SETQ *ALLOWDFINT_BAK (CONS *ALLOWDFINT *ALLOWDFINT_BAK))
      (COND ((NULL *NOPOWERS) (AEVAL (ON (LIST 'NOPOWERS)))))
      (COND ((NULL *ALLOWDFINT) (AEVAL (ON (LIST 'ALLOWDFINT))))))) 
(FLAG '(RECOVER_REDUCE_FLAGS) 'OPFN) 
(PUT 'RECOVER_REDUCE_FLAGS 'NUMBER-OF-ARGS 0) 
(PUT 'RECOVER_REDUCE_FLAGS 'DEFINED-ON-LINE '6685) 
(PUT 'RECOVER_REDUCE_FLAGS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'RECOVER_REDUCE_FLAGS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RECOVER_REDUCE_FLAGS NIL
    (PROG ()
      (COND
       ((NEQ *DFPRINT (CAR *DFPRINT_BAK))
        (COND (*DFPRINT (AEVAL (OFF (LIST 'DFPRINT))))
              (T (AEVAL (ON (LIST 'DFPRINT)))))))
      (SETQ *DFPRINT_BAK (CDR *DFPRINT_BAK))
      (COND
       ((NEQ *EXP (CAR *EXP_BAK))
        (COND (*EXP (AEVAL (OFF (LIST 'EXP)))) (T (AEVAL (ON (LIST 'EXP)))))))
      (SETQ *EXP_BAK (CDR *EXP_BAK))
      (COND
       ((NEQ *EZGCD (CAR *EZGCD_BAK))
        (COND (*EZGCD (AEVAL (OFF (LIST 'EZGCD))))
              (T (AEVAL (ON (LIST 'EZGCD)))))))
      (SETQ *EZGCD_BAK (CDR *EZGCD_BAK))
      (COND
       ((NEQ *FULLROOTS (CAR *FULLROOTS_BAK))
        (COND (*FULLROOTS (AEVAL (OFF (LIST 'FULLROOTS))))
              (T (AEVAL (ON (LIST 'FULLROOTS)))))))
      (SETQ *FULLROOTS_BAK (CDR *FULLROOTS_BAK))
      (COND
       ((NEQ *GCD (CAR *GCD_BAK))
        (COND (*GCD (AEVAL (OFF (LIST 'GCD)))) (T (AEVAL (ON (LIST 'GCD)))))))
      (SETQ *GCD_BAK (CDR *GCD_BAK))
      (COND
       ((NEQ *MCD (CAR *MCD_BAK))
        (COND (*MCD (AEVAL (OFF (LIST 'MCD)))) (T (AEVAL (ON (LIST 'MCD)))))))
      (SETQ *MCD_BAK (CDR *MCD_BAK))
      (COND
       ((NEQ *RATARG (CAR *RATARG_BAK))
        (COND (*RATARG (AEVAL (OFF (LIST 'RATARG))))
              (T (AEVAL (ON (LIST 'RATARG)))))))
      (SETQ *RATARG_BAK (CDR *RATARG_BAK))
      (COND
       ((NEQ *RATIONAL (CAR *RATIONAL_BAK))
        (COND (*RATIONAL (AEVAL (OFF (LIST 'RATIONAL))))
              (T (AEVAL (ON (LIST 'RATIONAL)))))))
      (SETQ *RATIONAL_BAK (CDR *RATIONAL_BAK))
      (COND
       ((NEQ *NOPOWERS (CAR *NOPOWERS_BAK))
        (COND (*NOPOWERS (AEVAL (OFF (LIST 'NOPOWERS))))
              (T (AEVAL (ON (LIST 'NOPOWERS)))))))
      (SETQ *NOPOWERS_BAK (CDR *NOPOWERS_BAK))
      (COND
       ((NEQ *ALLOWDFINT (CAR *ALLOWDFINT_BAK))
        (COND (*ALLOWDFINT (AEVAL (OFF (LIST 'ALLOWDFINT))))
              (T (AEVAL (ON (LIST 'ALLOWDFINT)))))))
      (SETQ *ALLOWDFINT_BAK (CDR *ALLOWDFINT_BAK)))) 
(PUT 'MAKLIST 'NUMBER-OF-ARGS 1) 
(FLAG '(MAKLIST) 'OPFN) 
(PUT 'MAKLIST 'DEFINED-ON-LINE '6728) 
(PUT 'MAKLIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MAKLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKLIST (EX)
    (COND ((BOOLVALUE* (REVALX (ATOM (REVALX EX)))) (AEVAL (LIST 'LIST EX)))
          ((BOOLVALUE* (REVALX (NEQ (CAR (REVALX EX)) 'LIST)))
           (SETQ EX (AEVAL (LIST 'LIST EX))))
          (T (AEVAL EX)))) 
(PUT 'ADD_TO_LAST_STEPS 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_TO_LAST_STEPS 'DEFINED-ON-LINE '6734) 
(PUT 'ADD_TO_LAST_STEPS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_TO_LAST_STEPS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_TO_LAST_STEPS (H)
    (PROG (N)
      (SETQ LAST_STEPS (CONS H LAST_STEPS))
      (COND
       ((FIXP SIZE_WATCH)
        (PROGN
         (SETQ N 0)
         (SETQ H LAST_STEPS)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (LESSP N SIZE_WATCH) (CDR H))) (RETURN NIL)))
           (PROGN (SETQ N (ADD1 N)) (SETQ H (CDR H)))
           (GO WHILELABEL))
         (COND ((CDR H) (RPLACD H NIL)))))))) 
(PUT 'SAME_STEPS 'NUMBER-OF-ARGS 2) 
(PUT 'SAME_STEPS 'DEFINED-ON-LINE '6746) 
(PUT 'SAME_STEPS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SAME_STEPS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAME_STEPS (A B)
    (COND
     ((AND (EQUAL (CAR A) (CAR B))
           (OR (EQUAL (CDDR A) (CDDR B))
               (AND (NEQ (CAR A) 'SUBST) (NEQ (CAR A) 27) (NEQ (CAR A) 30)
                    (NEQ (CAR A) 11) (NEQ (CAR A) 59) (NEQ (CAR A) 'SUB_SYS))))
      T)
     (T NIL))) 
(PUT 'IN_CYCLE 'NUMBER-OF-ARGS 1) 
(PUT 'IN_CYCLE 'DEFINED-ON-LINE '6757) 
(PUT 'IN_CYCLE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'IN_CYCLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IN_CYCLE (H)
    (PROG (CPLS1 CPLS2 N M CYCLE)
      (SETQ CPLS1 LAST_STEPS)
      (COND
       ((EQUAL (CAR H) 11)
        (PROGN
         (SETQ N 0)
         (SETQ M 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND CPLS1 (LESSP M 20))) (RETURN NIL)))
           (PROGN
            (COND ((SAME_STEPS H (CAR CPLS1)) (SETQ N (ADD1 N))))
            (SETQ M (ADD1 M))
            (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (COND ((AND (GREATERP N 1) (GREATERP (TIMES 3 N) M)) (SETQ CYCLE T))
               (T (SETQ CYCLE NIL)))))
       ((EQUAL (CAR H) 'SUBST)
        (PROGN
         (SETQ N 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT CPLS1) (RETURN NIL)))
           (PROGN
            (COND ((SAME_STEPS H (CAR CPLS1)) (SETQ N (ADD1 N))))
            (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (SETQ CYCLE
                 (COND
                  ((GREATERP N 2)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "A partial substitution has been repeated too often.")
                     NIL)
                    (TERPRI)
                    (PROGN (PRIN2 "It will now be made rigorously.") NIL)
                    (TERPRI)
                    T))
                  (T NIL)))))
       ((OR (EQUAL (CAR H) 9) (EQUAL (CAR H) 80))
        (PROGN
         (SETQ N 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (EQUAL N 1) CPLS1)) (RETURN NIL)))
           (PROGN
            (COND ((SAME_STEPS H (CAR CPLS1)) (SETQ N (ADD1 N))))
            (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (COND ((GREATERP N 1) (SETQ CYCLE T)) (T (SETQ CYCLE NIL)))))
       ((EQUAL (CAR H) 32)
        (PROGN
         (SETQ N 1)
         (SETQ M 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND CPLS1 (LESSP N 6) (LESSP M 100))) (RETURN NIL)))
           (PROGN
            (COND ((SAME_STEPS H (CAR CPLS1)) (SETQ N (ADD1 N))))
            (SETQ M (ADD1 M))
            (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (COND ((GEQ N 6) (SETQ CYCLE T)) (T (SETQ CYCLE NIL)))))
       ((AND (EQUAL (CAR H) 59) CPLS1 (SAME_STEPS H (CAR CPLS1)))
        (SETQ CYCLE T))
       (T
        (PROGN
         (SETQ N 1)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND CPLS1 (EQUAL (CAR H) (CAAR CPLS1))
                   (ZEROP (DIFFERENCE (DIFFERENCE (CADR H) N) (CADAR CPLS1)))))
             (RETURN NIL)))
           (PROGN (SETQ N (ADD1 N)) (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND CPLS1 (NOT (SAME_STEPS H (CAR CPLS1))))) (RETURN NIL)))
           (PROGN (SETQ N (ADD1 N)) (SETQ CPLS1 (CDR CPLS1)))
           (GO WHILELABEL))
         (COND
          ((OR (NULL CPLS1)
               (GREATERP (REVAL1 (LIST 'PLUS N N) T) (LENGTH LAST_STEPS)))
           (SETQ CYCLE NIL))
          (T
           (PROGN
            (SETQ CPLS1 (CDR CPLS1))
            (SETQ CPLS2 LAST_STEPS)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND (GREATERP N 0) (SAME_STEPS (CAR CPLS2) (CAR CPLS1))))
                (RETURN NIL)))
              (PROGN
               (SETQ CPLS1 (CDR CPLS1))
               (SETQ CPLS2 (CDR CPLS2))
               (SETQ N (SUB1 N)))
              (GO WHILELABEL))
            (COND
             ((AND (EQUAL N 0) PRINT_)
              (PROGN
               (PROGN
                (PRIN2
                 (COND
                  ((EQUAL (CAR H) 'SUB_SYS)
                   "A step to find overdet. sub-systems (")
                  ((EQUAL (CAR H) 9) "A derivative replacement (")
                  ((EQUAL (CAR H) 11) "An algebraic length reduction (")
                  ((EQUAL (CAR H) 27) "A length reducing simplification (")
                  ((EQUAL (CAR H) 59) "A Groebner Basis computation (")
                  (T "A step (")))
                (PRIN2 (CAR H))
                (PRIN2 ") was prevented")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "to avoid a cycle.") NIL)
               (TERPRI)
               NIL)))
            (SETQ CYCLE (COND ((GREATERP N 0) NIL) (T T))))))
         (COND ((NULL CYCLE) (ADD_TO_LAST_STEPS H)))
         NIL)))
      (RETURN CYCLE))) 
(PUT 'SWITCHP 'NUMBER-OF-ARGS 1) 
(PUT 'SWITCHP 'DEFINED-ON-LINE '6845) 
(PUT 'SWITCHP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SWITCHP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SWITCHP (X)
    (COND
     ((IDP X)
      (COND
       ((FLAGP X 'SWITCH)
        (PROGN
         (SETQ X (INTERN (BLDMSG_INTERNAL "*%w" (LIST X))))
         (COND ((BOUNDP X) (PRINT (LIST X (EVAL X))))))))))) 
(ENDMODULE) 
(MODULE (LIST 'SOLUTION_HANDLING)) 
(PUT 'SAVE_SOLUTION 'NUMBER-OF-ARGS 6) 
(PUT 'SAVE_SOLUTION 'DEFINED-ON-LINE '6861) 
(PUT 'SAVE_SOLUTION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SAVE_SOLUTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SAVE_SOLUTION (EQNS ASSIGNS FREEF INEQ INEQOR FILE_NAME)
    (PROG (S H P CONTI A SAVE OFL*BAK)
      (COND (FILE_NAME (SETQ S FILE_NAME))
            (T
             (PROGN
              (SETQ S (LEVEL_STRING SESSION_))
              (SETQ S (EXPLODE S))
              (SETQ S (COMPRESS (CONS '|"| (CONS 'S (CONS 'O (CDDDR S))))))
              NIL)))
      (SETQ SOL_LIST (UNION (LIST S) SOL_LIST))
      (SETQ A (OPEN S 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* S)
      (SETQ SAVE (WRS A))
      (PROGN (PRIN2 "off echo$ ") NIL)
      (PROGN (PRIN2 "backup_:='(") NIL)
      (TERPRI)
      (PROG (H)
        (SETQ H FREEF)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (COND ((SETQ P (ASSOC H DEPL*)) (SETQ CONTI (CONS P CONTI)))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (PROGN (PRIN2 "% A list of dependencies, like ((f x y) (g x))") NIL)
      (TERPRI)
      (PRINT CONTI)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "% A list of unsolved equations") NIL)
      (TERPRI)
      (PRINT EQNS)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "% A list of assignments") NIL)
      (TERPRI)
      (PRINT ASSIGNS)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "% A list of free or unresolved functions") NIL)
      (TERPRI)
      (PRINT FREEF)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "% A list of non-vanishing expressions.") NIL)
      (TERPRI)
      (PRINT INEQ)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "% A list of or-lists. Each or-list has elements that ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "% are factor-list, such that for each or-list at least")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "% from one factor-list all elements must be non-zero. ")
       NIL)
      (TERPRI)
      (PRINT INEQOR)
      (PROGN (PRIN2 " ") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 ")$") NIL)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (RETURN S))) 
(PUT 'PRINT_INDEXED_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_INDEXED_LIST 'DEFINED-ON-LINE '6933) 
(PUT 'PRINT_INDEXED_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_INDEXED_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_INDEXED_LIST (LI)
    (PROG (A H)
      (TERPRI)
      (SETQ H 0)
      (PROG (A)
        (SETQ A LI)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ H (ADD1 H))
            (PROGN (PRIN2 "[") (PRIN2 H) (PRIN2 "]") NIL)
            (TERPRI)
            (MATHPRINT A)))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB)))) 
(PUT 'PRINTDHMSTIME 'NUMBER-OF-ARGS 1) 
(PUT 'PRINTDHMSTIME 'DEFINED-ON-LINE '6944) 
(PUT 'PRINTDHMSTIME 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINTDHMSTIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINTDHMSTIME (A)
    (PROG (B)
      (COND
       ((GREATERP A 10000)
        (PROGN
         (PROGN (PRIN2 " = ") NIL)
         (COND
          ((GEQ A 86400000)
           (PROGN
            (SETQ B (FLOOR (QUOTIENT A 86400000)))
            (PROGN
             (PRIN2 B)
             (PRIN2 (COND ((EQUAL B 1) " day ") (T " days  ")))
             NIL)
            (SETQ A (DIFFERENCE A (TIMES B 86400000))))))
         (COND
          ((GEQ A 3600000)
           (PROGN
            (SETQ B (FLOOR (QUOTIENT A 3600000)))
            (PROGN
             (PRIN2 B)
             (PRIN2 (COND ((EQUAL B 1) " hour ") (T " hours  ")))
             NIL)
            (SETQ A (DIFFERENCE A (TIMES B 3600000))))))
         (COND
          ((GEQ A 60000)
           (PROGN
            (SETQ B (FLOOR (QUOTIENT A 60000)))
            (PROGN
             (PRIN2 B)
             (PRIN2 (COND ((EQUAL B 1) " minute ") (T " minutes  ")))
             NIL)
            (SETQ A (DIFFERENCE A (TIMES B 60000))))))
         (COND
          ((GEQ A 1000)
           (PROGN
            (SETQ B (FLOOR (QUOTIENT A 1000)))
            (PROGN
             (PRIN2 B)
             (PRIN2 (COND ((EQUAL B 1) " seccond ") (T " seconds  ")))
             NIL)
            (SETQ A (DIFFERENCE A (TIMES B 1000))))))
         (COND ((NEQ A 0) (PROGN (PRIN2 A) (PRIN2 " msec ") NIL)))))))) 
(PUT 'SUB_LIST 'NUMBER-OF-ARGS 3) 
(PUT 'SUB_LIST 'DEFINED-ON-LINE '6973) 
(PUT 'SUB_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SUB_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUB_LIST (SB AIM TR_MERGE)
    (PROG (A B)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SB AIM)) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SB))
         (SETQ SB (CDR SB))
         (COND (TR_MERGE (SETQ B AIM)))
         (SETQ AIM (ERR_CATCH_SUB (CADR A) (CADDR A) AIM))
         (COND
          ((AND TR_MERGE (NULL AIM))
           (PROGN
            (PROGN (PRIN2 "Sub: ") NIL)
            (MATHPRINT A)
            (PROGN (PRIN2 "in: ") NIL)
            (MATHPRINT B)
            (PROGN (PRIN2 "gives a singular result.") NIL)
            (TERPRI)))))
        (GO WHILELABEL))
      (COND
       ((NULL AIM)
        (PROGN
         (PROGN (PRIN2 "Substitutions give singularities.") NIL)
         (TERPRI)
         NIL)))
      (RETURN AIM))) 
(PUT 'MERGE_TWO 'NUMBER-OF-ARGS 5) 
(PUT 'MERGE_TWO 'DEFINED-ON-LINE '7006) 
(PUT 'MERGE_TWO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MERGE_TWO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MERGE_TWO (S1 SOL1 S2 SOL2 ABSORB)
    (PROG (ELI_2 SINGULAR_ELI REGULAR_ELI A B COND2 SB REMAIN_SB SINGULAR_SB
           REGULAR_SB C2 REMAIN_C2 REMAIN_NUM_C2 H HH TRY_TO_SUB TRY_TO_SUB_CP
           NUM_SB SINGULAR_EX NEW_EQN SINGULAR_EX_CP INEQ2 INE INEQNEW INEQDROP
           TR_MERGE EXTRA_PAR_IN_S1 GAUGE_OF_S2 GAUGE_OF_S2_CP DID_TRAFO N
           REMAIN_C2_CP DROPPED_ASSIGN_IN_S2 NEW_ASSIGN_IN_S2 ASS1 ASS2
           SOL1_EQN SOL2_EQN GB)
      (COND
       (TR_MERGE
        (PROGN
         (PROGN (PRIN2 "*** sol1 ***: ") (PRIN2 S1) NIL)
         (TERPRI)
         (COND
          ((CADR SOL1)
           (PROGN
            (PROGN (PRIN2 "Remaining equations:") NIL)
            (DEPRINT (CADR SOL1)))))
         (PRINT_INDEXED_LIST (CADDR SOL1))
         (PROGN (PRIN2 "*** sol2 ***: ") (PRIN2 S2) NIL)
         (TERPRI)
         (COND
          ((CADR SOL2)
           (PROGN
            (PROGN (PRIN2 "Remaining equations:") NIL)
            (DEPRINT (CADR SOL2)))))
         (PRINT_INDEXED_LIST (CADDR SOL2))
         (PROGN (PRIN2 "free param in sol1: ") (PRIN2 (CADDDR SOL1)) NIL)
         (TERPRI)
         (PROGN (PRIN2 "free param in sol2: ") (PRIN2 (CADDDR SOL2)) NIL)
         (TERPRI))))
      (SETQ ASS1 (CADDR SOL1))
      (PROG (A)
        (SETQ A (CADDDR SOL1))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (SETQ ASS1 (DELETE (LIST 'EQUAL A A) ASS1))) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ ASS2 (CADDR SOL2))
      (PROG (A)
        (SETQ A (CADDDR SOL2))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (SETQ ASS2 (DELETE (LIST 'EQUAL A A) ASS2))) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ SOL1_EQN (CONS 'LIST (CADR SOL1)))
      (SETQ SOL2_EQN (CONS 'LIST (CADR SOL2)))
      (COND
       ((CDR SOL1_EQN)
        (PROGN
         (COND
          (TR_MERGE
           (PROGN
            (PROGN (PRIN2 "Initial preparation of unsolved eqn in sol1") NIL)
            (TERPRI))))
         (COND
          ((NULL (SETQ SOL1_EQN (SUB_LIST ASS1 SOL1_EQN TR_MERGE)))
           (RETURN NIL)))
         NIL)))
      (COND
       ((CDR SOL2_EQN)
        (PROGN
         (COND
          (TR_MERGE
           (PROGN
            (PROGN (PRIN2 "Initial preparation of unsolved eqn in sol2") NIL)
            (TERPRI))))
         (COND
          ((NULL (SETQ SOL2_EQN (SUB_LIST ASS2 SOL2_EQN TR_MERGE)))
           (RETURN NIL)))
         NIL)))
      (COND
       ((CDR SOL2_EQN)
        (PROGN
         (COND
          (TR_MERGE
           (PROGN (PROGN (PRIN2 "sol1 substitutions in sol2") NIL) (TERPRI))))
         (COND
          ((NULL (SETQ SOL2_EQN (SUB_LIST ASS1 SOL2_EQN TR_MERGE)))
           (RETURN NIL)))
         NIL)))
      (COND ((AND (NULL (CDR SOL1_EQN)) (CDR SOL2_EQN)) (RETURN NIL)))
      (COND
       ((CDR SOL1_EQN)
        (PROGN
         (AEVAL (LIST 'TORDER (CONS 'LIST (CADDDR SOL1)) 'LEX))
         (SETQ GB (AEVAL (LIST 'GROEBNER SOL1_EQN)))
         (COND
          ((BOOLVALUE* TR_MERGE)
           (PROGN
            (ASSGNPRI (AEVAL "gb=") NIL 'FIRST)
            (ASSGNPRI (AEVAL GB) NIL 'LAST))))
         (WHILE
          (AND (EVALNEQ (AEVAL* SOL2_EQN) (AEVAL* (LIST 'LIST)))
               (EVALEQUAL
                (AEVAL* (LIST 'PREDUCE (LIST 'NUM (LIST 'FIRST SOL2_EQN)) GB))
                0))
          (SETQ SOL2_EQN (AEVAL* (LIST 'REST SOL2_EQN)))))))
      (SETQ SOL2_EQN (CDR SOL2_EQN))
      (COND
       (TR_MERGE
        (COND
         ((NULL SOL2_EQN)
          (PROGN
           (PROGN
            (PRIN2 "The remaining equations of solution sol2 are in the")
            NIL)
           (TERPRI)
           (PROGN
            (PRIN2 "ideal of the remaining equations of solution sol1.")
            NIL)
           (TERPRI)))
         (T
          (PROGN
           (PROGN (PRIN2 "Equation ") NIL)
           (MATHPRINT (CAR SOL2_EQN))
           (PROGN (PRIN2 "of solution sol2 is not in the ideal of") NIL)
           (TERPRI)
           (PROGN (PRIN2 "the remaining equations of solution sol1.") NIL)
           (TERPRI)
           (PROGN (PRIN2 "--> sol1 is not a special case of sol2.") NIL)
           (TERPRI)
           NIL)))))
      (COND (SOL2_EQN (RETURN NIL)))
      (SETQ ELI_2
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A ASS2)
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
      (SETQ COND2
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A ASS2)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (LIST 'PLUS (CADR A)
                                          (LIST 'MINUS (CADDR A))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (LIST 'PLUS (CADR A) (LIST 'MINUS (CADDR A))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ COND2 (CONS 'LIST COND2))
      (SETQ SB ASS1)
      (PROG ()
       WHILELABEL
        (COND ((NOT SB) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SB))
         (SETQ SB (CDR SB))
         (COND
          ((MEMBER (CADR A) ELI_2)
           (PROGN
            (SETQ ELI_2 (DELETE (CADR A) ELI_2))
            (SETQ COND2 (ERR_CATCH_SUB (CADR A) (CADDR A) COND2))))
          (T (SETQ REMAIN_SB (CONS A REMAIN_SB)))))
        (GO WHILELABEL))
      (SETQ ELI_2 (APPEND ELI_2 (CADDDR SOL2)))
      (SETQ REMAIN_C2 COND2)
      (SETQ COND2 (CDR COND2))
      (SETQ C2 NIL)
      (SETQ H 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND COND2 (OR (NULL C2) (ZEROP C2)))) (RETURN NIL)))
        (PROGN
         (SETQ C2 (CAR COND2))
         (SETQ H (ADD1 H))
         (COND
          (TR_MERGE
           (PROGN
            (PROGN (PRIN2 "[") (PRIN2 H) (PRIN2 "]") NIL)
            (TERPRI)
            (MATHPRINT C2))))
         (SETQ SB REMAIN_SB)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND SB C2 (NOT (ZEROP C2)))) (RETURN NIL)))
           (PROGN
            (SETQ A (CAR SB))
            (SETQ SB (CDR SB))
            (SETQ C2 (AEVAL* (LIST 'NUM C2)))
            (COND (TR_MERGE (SETQ B C2)))
            (SETQ C2 (ERR_CATCH_SUB (CADR A) (CADDR A) C2))
            (COND
             ((AND TR_MERGE (NEQ B C2))
              (PROGN
               (PROGN (PRIN2 "Sub: ") NIL)
               (MATHPRINT A)
               (COND
                (C2 (PROGN (PROGN (PRIN2 "new value=") NIL) (MATHPRINT C2)))
                (T (PROGN (PROGN (PRIN2 "singular result") NIL) (TERPRI)))))))
            (COND
             ((AND C2 (NOT (ZEROP C2)) GB)
              (PROGN
               (SETQ C2 (AEVAL* (LIST 'PREDUCE (LIST 'NUM C2) GB)))
               (COND
                (TR_MERGE
                 (PROGN
                  (COND
                   ((ZEROP C2)
                    (PROGN
                     (PRIN2
                      "which vanishes modulo the remaining eqn.s of sol1.")
                     NIL))
                   (T
                    (PROGN
                     (PRIN2
                      "which does not vanish modulo the remaining eqn.s of sol1.")
                     NIL)))
                  (TERPRI)
                  NIL)))))))
           (GO WHILELABEL))
         (COND
          ((NULL C2) (SETQ REMAIN_NUM_C2 (CONS (CAR COND2) REMAIN_NUM_C2))))
         (SETQ COND2 (CDR COND2)))
        (GO WHILELABEL))
      (COND ((AND C2 (NOT (ZEROP C2))) (RETURN NIL)))
      (COND
       (REMAIN_NUM_C2
        (PROGN
         (PROGN (PRIN2 "Even substitutions in the numerator is giving ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "singularities like log(0).") NIL)
         (TERPRI)
         (RETURN NIL))))
      (PROGN (PRIN2 "Substitutions in numerators give all zero") NIL)
      (TERPRI)
      (SETQ SB REMAIN_SB)
      (PROG ()
       WHILELABEL
        (COND ((NOT SB) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SB))
         (SETQ SB (CDR SB))
         (SETQ H (ERR_CATCH_SUB (CADR A) (CADDR A) REMAIN_C2))
         (COND ((NULL H) (SETQ SINGULAR_SB (CONS A SINGULAR_SB)))
               (T (SETQ REGULAR_SB (CONS A REGULAR_SB)))))
        (GO WHILELABEL))
      (COND
       (TR_MERGE
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "regular_sb: ") NIL)
         (MATHPRINT (CONS 'LIST REGULAR_SB)))))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN (PRIN2 "singular_sb: ") NIL)
         (MATHPRINT (CONS 'LIST SINGULAR_SB)))))
      (COND
       (SINGULAR_SB
        (PROGN
         (PROGN (PRIN2 "Substitutions lead to singularities.") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "Solution ")
          (PRIN2 S2)
          (PRIN2 " has to be transformed.")
          NIL)
         (TERPRI))))
      (SETQ SINGULAR_EX
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A SINGULAR_SB)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (REVAL1
                                     (LIST 'PLUS (CADR A)
                                           (LIST 'MINUS (CADDR A)))
                                     T))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (REVAL1
                             (LIST 'PLUS (CADR A) (LIST 'MINUS (CADDR A))) T))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN
          (PRIN2 "The following are expressions which vanish due to sol1 and")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "which lead to singularities when used for substitutions in sol2")
          NIL)
         (TERPRI)
         (MATHPRINT (CONS 'LIST SINGULAR_EX)))))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN
          (PRIN2
           "The following are all free parameters in sol2 for which there are")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "substitutions in sol1") NIL)
         (TERPRI)
         NIL)))
      (SETQ SINGULAR_ELI
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A SINGULAR_SB)
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
      (SETQ REGULAR_ELI
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A REGULAR_SB)
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
      (COND
       (TR_MERGE
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "singular_eli: ") NIL)
         (MATHPRINT (CONS 'LIST SINGULAR_ELI)))))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN (PRIN2 "regular_eli: ") NIL)
         (MATHPRINT (CONS 'LIST REGULAR_ELI)))))
      (SETQ EXTRA_PAR_IN_S1 (SETDIFF (CADDDR SOL1) (CADDDR SOL2)))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN
          (PRIN2 "Param in sol1 and not in sol2: ")
          (PRIN2 EXTRA_PAR_IN_S1)
          NIL)
         (TERPRI))))
      (PROG (A)
        (SETQ A EXTRA_PAR_IN_S1)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ H ASS2)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND H (NEQ (CADAR H) A))) (RETURN NIL)))
              (SETQ H (CDR H))
              (GO WHILELABEL))
            (COND
             ((NULL H)
              (PROGN
               (PRIN2 "ERROR, there must be an assignment of a in sol2!")
               NIL))
             (T
              (PROGN
               (COND
                (TR_MERGE
                 (PROGN
                  (PROGN
                   (PRIN2 "Assignment in ")
                   (PRIN2 S2)
                   (PRIN2 " of a variable that is a free parameter in ")
                   (PRIN2 S1)
                   (PRIN2 " :")
                   NIL)
                  (TERPRI)
                  (MATHPRINT (CAR H))
                  NIL)))
               (SETQ DROPPED_ASSIGN_IN_S2 (CONS (CAR H) DROPPED_ASSIGN_IN_S2))
               (SETQ GAUGE_OF_S2
                       (CONS
                        (AEVAL
                         (LIST 'NUM
                               (LIST 'PLUS (CADR (CAR H))
                                     (LIST 'MINUS (CADDR (CAR H))))))
                        GAUGE_OF_S2)))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ GAUGE_OF_S2 (CONS 'LIST GAUGE_OF_S2))
      (COND
       (TR_MERGE
        (PROGN (PROGN (PRIN2 "gauge_of_s2=") NIL) (MATHPRINT GAUGE_OF_S2))))
      (SETQ TRY_TO_SUB (APPEND SINGULAR_ELI REGULAR_ELI))
      (SETQ H (REVERSE TRY_TO_SUB))
      (PROG (A)
        (SETQ A H)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((OR (AND FLIN_ (NOT (FREEOF FLIN_ A)))
                 (AND (NOT FLIN_)
                      (PROGN
                       (SETQ COND2 REMAIN_C2)
                       (PROG ()
                        WHILELABEL
                         (COND
                          ((NOT (AND COND2 (LIN_CHECK (CAR COND2) (LIST A))))
                           (RETURN NIL)))
                         (SETQ COND2 (CDR COND2))
                         (GO WHILELABEL))
                       (NULL COND2))))
             (PROGN
              (COND
               (TR_MERGE
                (PROGN
                 (PROGN
                  (PRIN2 "Because ")
                  (PRIN2 A)
                  (PRIN2 " is either in flin_ or appears linearly in sol2,")
                  NIL)
                 (TERPRI)
                 (PROGN (PRIN2 "it gets a higher priority.") NIL)
                 (TERPRI)
                 NIL)))
              (SETQ TRY_TO_SUB (CONS A (DELETE A TRY_TO_SUB)))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ N 1)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ DID_TRAFO NIL)
         (SETQ GAUGE_OF_S2_CP (CDR GAUGE_OF_S2))
         (PROG ()
          WHILELABEL
           (COND ((NOT GAUGE_OF_S2_CP) (RETURN NIL)))
           (PROGN
            (SETQ SB (REVAL1 (CAR GAUGE_OF_S2_CP) T))
            (SETQ GAUGE_OF_S2_CP (CDR GAUGE_OF_S2_CP))
            (COND
             ((NOT (ZEROP SB))
              (PROGN
               (SETQ TRY_TO_SUB_CP TRY_TO_SUB)
               (COND
                (TR_MERGE
                 (PROGN
                  (PROGN (PRIN2 "next relation to be used: 0=") NIL)
                  (MATHPRINT SB)
                  (PROGN (PRIN2 "try_to_sub=") (PRIN2 TRY_TO_SUB) NIL)
                  (TERPRI))))
               (SETQ H (ERR_CATCH_FAC SB))
               (COND
                (H
                 (PROGN
                  (SETQ SB NIL)
                  (SETQ H (CDR H))
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT H) (RETURN NIL)))
                    (PROGN
                     (COND
                      ((PAIRP (CAR H))
                       (COND
                        ((NOT
                          (AND (EQUAL (CAAR H) 'QUOTIENT) (FIXP (CADAR H))
                               (FIXP (CADDAR H))))
                         (COND
                          ((EQUAL (CAAR H) 'LIST)
                           (COND
                            ((PAIRP (CADAR H)) (SETQ SB (CONS (CADAR H) SB)))
                            (T NIL)))
                          (T (SETQ SB (CONS (CAR H) SB))))))))
                     (SETQ H (CDR H))
                     NIL)
                    (GO WHILELABEL)))))
               (COND
                (TR_MERGE
                 (PROGN
                  (PROGN
                   (PRIN2 "After dropping single variable factors ")
                   (PRIN2 (LENGTH SB))
                   (PRIN2 " factor(s) remain")
                   NIL)
                  (TERPRI))))
               (SETQ SB (REVAL1 (CONS 'TIMES (CONS 1 SB)) T))
               (COND
                (TR_MERGE
                 (PROGN
                  (PROGN (PRIN2 "New relation used for substitution: sb=") NIL)
                  (MATHPRINT SB)
                  (TERPRI))))
               (COND
                ((AND FLIN_ (NOT (FREEOFLIST SB FLIN_)))
                 (PROGN
                  (SETQ H NIL)
                  (PROG (A)
                    (SETQ A TRY_TO_SUB_CP)
                   LAB
                    (COND ((NULL A) (RETURN NIL)))
                    ((LAMBDA (A)
                       (COND ((NOT (FREEOF FLIN_ A)) (SETQ H (CONS A H)))))
                     (CAR A))
                    (SETQ A (CDR A))
                    (GO LAB))
                  (SETQ TRY_TO_SUB_CP H))))
               (PROG ()
                WHILELABEL
                 (COND ((NOT TRY_TO_SUB_CP) (RETURN NIL)))
                 (PROGN
                  (SETQ A (CAR TRY_TO_SUB_CP))
                  (SETQ TRY_TO_SUB_CP (CDR TRY_TO_SUB_CP))
                  (COND
                   (TR_MERGE
                    (PROGN
                     (PROGN (PRIN2 "try to sub next: ") (PRIN2 A) NIL)
                     (TERPRI))))
                  (COND
                   ((AND (NOT (FREEOF SB A)) (LIN_CHECK SB (LIST A)))
                    (PROGN
                     (SETQ NUM_SB
                             (REVAL1
                              (LIST 'DIFFERENCE SB
                                    (LIST 'TIMES A (COEFFN SB A 1)))
                              T))
                     (COND
                      (TR_MERGE
                       (PROGN
                        (PROGN (PRIN2 "num_sb=") NIL)
                        (MATHPRINT NUM_SB))))
                     (COND
                      (T
                       (PROGN
                        (SETQ ELI_2 (DELETE A ELI_2))
                        (SETQ B (CADR (SOLVEEVAL (LIST SB A))))
                        (SETQ H (ERR_CATCH_SUB (CADR B) (CADDR B) REMAIN_C2))
                        (COND
                         ((AND TR_MERGE (NULL H))
                          (PROGN
                           (PROGN (PRIN2 "Trafo ") NIL)
                           (MATHPRINT B)
                           (PROGN (PRIN2 " was singular.") NIL)
                           (TERPRI))))
                        (COND
                         (H
                          (PROGN
                           (SETQ GAUGE_OF_S2
                                   (ERR_CATCH_SUB (CADR B) (CADDR B)
                                    GAUGE_OF_S2))
                           (SETQ GAUGE_OF_S2
                                   (CONS 'LIST
                                         (PROG (GAUGE_OF_S2_CP FORALL-RESULT
                                                FORALL-ENDPTR)
                                           (SETQ GAUGE_OF_S2_CP
                                                   (CDR GAUGE_OF_S2))
                                           (COND
                                            ((NULL GAUGE_OF_S2_CP)
                                             (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA
                                                                 (
                                                                  GAUGE_OF_S2_CP)
                                                               (AEVAL*
                                                                (LIST 'NUM
                                                                      GAUGE_OF_S2_CP)))
                                                             (CAR
                                                              GAUGE_OF_S2_CP))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ GAUGE_OF_S2_CP
                                                   (CDR GAUGE_OF_S2_CP))
                                           (COND
                                            ((NULL GAUGE_OF_S2_CP)
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (GAUGE_OF_S2_CP)
                                                       (AEVAL*
                                                        (LIST 'NUM
                                                              GAUGE_OF_S2_CP)))
                                                     (CAR GAUGE_OF_S2_CP))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))))
                           (SETQ GAUGE_OF_S2_CP NIL)
                           (SETQ NEW_ASSIGN_IN_S2 (CONS B NEW_ASSIGN_IN_S2))
                           (SETQ DID_TRAFO T)
                           (PROGN
                            (PRIN2
                             "In order to avoid a singularity when doing substitutions")
                            NIL)
                           (TERPRI)
                           (PROGN
                            (PRIN2
                             "the supposed to be more general solution was transformed using:")
                            NIL)
                           (TERPRI)
                           (MATHPRINT B)
                           (COND
                            (TR_MERGE
                             (PROGN
                              (PROGN (PRIN2 "The new gauge_of_s2: ") NIL)
                              (MATHPRINT GAUGE_OF_S2))))
                           (SETQ REMAIN_C2 H)
                           (SETQ H (APPEND REGULAR_SB SINGULAR_SB))
                           (PROG ()
                            WHILELABEL
                             (COND
                              ((NOT (AND H (NEQ A (CADAR H)))) (RETURN NIL)))
                             (SETQ H (CDR H))
                             (GO WHILELABEL))
                           (COND
                            (H
                             (SETQ REMAIN_C2
                                     (APPEND REMAIN_C2
                                             (LIST
                                              (LIST 'DIFFERENCE (CADDAR H)
                                                    (CADDR B)))))))
                           (COND
                            (TR_MERGE
                             (PROGN
                              (PROGN (PRIN2 "remain_c2=") NIL)
                              (PRINT_INDEXED_LIST (CDR REMAIN_C2)))))
                           (SETQ SINGULAR_EX_CP NIL)
                           (SETQ TRY_TO_SUB (DELETE A TRY_TO_SUB))
                           (SETQ TRY_TO_SUB_CP NIL)
                           (SETQ N (PLUS N 1))))))))))))
                 (GO WHILELABEL))
               NIL))))
           (GO WHILELABEL))
         NIL)
        (COND ((NOT (EQUAL DID_TRAFO NIL)) (GO REPEATLABEL))))
      (COND
       (TR_MERGE
        (PROGN
         (PROGN
          (PRIN2 "After completing the trafo the new list of parameters of")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "sol2 is: ") (PRIN2 ELI_2) NIL)
         (TERPRI)
         (PROGN (PRIN2 "sol1 has free parameters: ") (PRIN2 (CADDDR SOL1)) NIL)
         (TERPRI))))
      (COND
       ((NOT_INCLUDED (CADDDR SOL1) ELI_2)
        (RETURN
         (PROGN
          (PROGN
           (PRIN2
            "Something seems wrong in merge_sol(): after the transformation of")
           NIL)
          (TERPRI)
          (PROGN
           (PRIN2
            "sol2, all free parameters of sol1 should be free parameters of sol2.")
           NIL)
          (TERPRI)
          NIL)))
       (T
        (PROGN
         (COND
          (TR_MERGE
           (PROGN
            (PROGN
             (PRIN2 "All free parameters of sol1 are free parameters of sol2")
             NIL)
            (TERPRI)))))))
      (SETQ SB (APPEND REGULAR_SB SINGULAR_SB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SB REMAIN_C2)) (RETURN NIL)))
        (PROGN
         (SETQ A (CAR SB))
         (SETQ SB (CDR SB))
         (SETQ REMAIN_C2_CP REMAIN_C2)
         (SETQ REMAIN_C2 (ERR_CATCH_SUB (CADR A) (CADDR A) REMAIN_C2))
         (COND
          (TR_MERGE
           (COND
            ((NULL REMAIN_C2)
             (PROGN
              (PROGN (PRIN2 "The following subst. was singular: ") NIL)
              (MATHPRINT A)))
            (T
             (PROGN
              (PROGN (PRIN2 "Remaining substitution: ") NIL)
              (MATHPRINT A)
              NIL))))))
        (GO WHILELABEL))
      (COND ((NULL REMAIN_C2) (SETQ REMAIN_C2 REMAIN_C2_CP))
            (T (SETQ REMAIN_C2_CP REMAIN_C2)))
      (SETQ REMAIN_C2_CP (CDR REMAIN_C2_CP))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND REMAIN_C2_CP
                (OR (ZEROP (CAR REMAIN_C2_CP))
                    (AND GB
                         (ZEROP
                          (AEVAL*
                           (LIST 'PREDUCE (LIST 'NUM (CAR REMAIN_C2_CP))
                                 GB)))))))
          (RETURN NIL)))
        (SETQ REMAIN_C2_CP (CDR REMAIN_C2_CP))
        (GO WHILELABEL))
      (COND
       (REMAIN_C2_CP
        (PROGN
         (SETQ REMAIN_C2_CP REMAIN_C2)
         (COND
          (TR_MERGE
           (PROGN
            (PROGN (PRIN2 "remain_c2=") NIL)
            (PRINT_INDEXED_LIST (CDR REMAIN_C2_CP)))))
         (SETQ H (CADDDR SOL1))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND H
                   (PROGN
                    (COND
                     (TR_MERGE
                      (PROGN
                       (PRIN2 "Substitution of ")
                       (PRIN2 (CAR H))
                       (PRIN2 " by: ")
                       NIL)))
                    (PROG ()
                     REPEATLABEL
                      (PROGN
                       (SETQ A (PLUS 1 (RANDOM 10000)))
                       (COND (TR_MERGE (PROGN (PROGN (PRIN2 A) NIL) (TERPRI))))
                       (SETQ A (ERR_CATCH_SUB (CAR H) A REMAIN_C2_CP)))
                      (COND ((NOT A) (GO REPEATLABEL))))
                    (SETQ REMAIN_C2_CP A)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND A (OR (NOT (NUMBERP (CAR A))) (ZEROP (CAR A)))))
                        (RETURN NIL)))
                      (SETQ A (CDR A))
                      (GO WHILELABEL))
                    (NOT A))))
             (RETURN NIL)))
           (SETQ H (CDR H))
           (GO WHILELABEL))
         (COND
          (H
           (RETURN
            (PROGN
             (PROGN
              (PRIN2 "In the following S1 stands for ")
              (PRIN2 S1)
              (PRIN2 "and S2 stands for ")
              (PRIN2 S2)
              (PRIN2 " . ")
              (PRIN2
               "Solution S1 fulfills all conditions of solution S2 when conditions")
              (PRIN2
               "are made denominator free. But, after rewriting solution S2 so that")
              (PRIN2
               "all free parameters of solution S1 are also free parameters of S2")
              (PRIN2
               "then the new solution S2 now requires the vanishing of an expression")
              (PRIN2
               "in these free parameters which is not allowed by S1. Therefore S1")
              (PRIN2 "is not a special case of S2.")
              NIL)
             NIL))))
         (COND
          ((AND TR_MERGE REMAIN_C2_CP)
           (PROGN
            (PROGN (PRIN2 "remain_c2_cp after subst = ") NIL)
            (MATHPRINT (CONS 'LIST REMAIN_C2)))))
         (PROGN
          (PRIN2 "Solution ")
          (PRIN2 S1)
          (PRIN2 " is not less restrictive than solution")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 S2)
          (PRIN2 " and fulfills all conditions of solution ")
          (PRIN2 S2)
          (PRIN2 " .")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "But it was not possible for the program to re-formulate solution ")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 S2)
          (PRIN2 " to include both solutions in a single set of")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "assignments without vanishing denominators. :-( ") NIL)
         (TERPRI)
         (RETURN NIL)))
       (T
        (RETURN
         (PROGN
          (SETQ INEQ2 (CAR (CDDDDR SOL2)))
          (PROG ()
           WHILELABEL
            (COND ((NOT INEQ2) (RETURN NIL)))
            (PROGN
             (SETQ INE (CAR INEQ2))
             (PROG (A)
               (SETQ A ASS1)
              LAB
               (COND ((NULL A) (RETURN NIL)))
               ((LAMBDA (A)
                  (SETQ INE (REVAL1 (SUBST (CADDR A) (CADR A) INE) T)))
                (CAR A))
               (SETQ A (CDR A))
               (GO LAB))
             (COND
              ((NOT (ZEROP (REVAL1 INE T)))
               (SETQ INEQNEW (CONS (CAR INEQ2) INEQNEW)))
              (T (SETQ INEQDROP (CONS (CAR INEQ2) INEQDROP))))
             (SETQ INEQ2 (CDR INEQ2)))
            (GO WHILELABEL))
          (COND
           (ABSORB
            (PROGN
             (SETQ H (CONS 'LIST ASS2))
             (SETQ B (CADDDR SOL2))
             (COND
              (TR_MERGE
               (PROGN
                (PROGN (PRIN2 "h0=") NIL)
                (PRINT_INDEXED_LIST H)
                (PROGN (PRIN2 "dropped_assign_in_s2=") NIL)
                (PRINT_INDEXED_LIST DROPPED_ASSIGN_IN_S2)
                (PROGN (PRIN2 "new_assign_in_s2=") NIL)
                (PRINT_INDEXED_LIST NEW_ASSIGN_IN_S2)
                NIL)))
             (PROG (A)
               (SETQ A DROPPED_ASSIGN_IN_S2)
              LAB
               (COND ((NULL A) (RETURN NIL)))
               ((LAMBDA (A)
                  (PROGN
                   (SETQ H (DELETE A H))
                   (SETQ B (CONS (REVAL1 (CADR A) T) B))))
                (CAR A))
               (SETQ A (CDR A))
               (GO LAB))
             (COND
              (TR_MERGE
               (PROGN (PROGN (PRIN2 "h1=") NIL) (PRINT_INDEXED_LIST H))))
             (SETQ NEW_EQN (CONS 'LIST (CADR SOL2)))
             (PROG (A)
               (SETQ A (REVERSE NEW_ASSIGN_IN_S2))
              LAB
               (COND ((NULL A) (RETURN NIL)))
               ((LAMBDA (A)
                  (COND
                   (H
                    (PROGN
                     (SETQ B (DELETE (REVAL1 (CADR A) T) B))
                     (COND
                      (TR_MERGE
                       (PROGN
                        (PROGN (PRIN2 "a=") (PRIN2 A) NIL)
                        (TERPRI)
                        (PROGN (PRIN2 "h2=") NIL)
                        (PRINT_INDEXED_LIST H))))
                     (SETQ H (ERR_CATCH_SUB (CADR A) (CADDR A) H))
                     (SETQ NEW_EQN (ERR_CATCH_SUB (CADR A) (CADDR A) NEW_EQN))
                     (COND ((NULL NEW_EQN) (SETQ H NIL))
                           (T
                            (SETQ NEW_EQN
                                    (PROG (HH FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ HH (GETRLIST (AEVAL NEW_EQN)))
                                      (COND
                                       ((NULL HH) (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (HH)
                                                          (AEVAL
                                                           (LIST 'NUM HH)))
                                                        (CAR HH))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ HH (CDR HH))
                                      (COND
                                       ((NULL HH)
                                        (RETURN (CONS 'LIST FORALL-RESULT))))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (HH)
                                                  (AEVAL (LIST 'NUM HH)))
                                                (CAR HH))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))))
                     (COND (H (SETQ H (REVAL1 (APPEND H (LIST A)) T))))))))
                (CAR A))
               (SETQ A (CDR A))
               (GO LAB))
             (COND
              ((NULL H)
               (PROGN
                (PRIN2 "A seemingly successful transformation of ")
                (PRIN2 S2)
                (PRIN2 "went singular when performing the transformation ")
                (PRIN2 "finally on the whole solution.")
                NIL))
              (T
               (PROGN
                (COND
                 ((CDR NEW_EQN)
                  (PROGN
                   (COND
                    ((EVALGREATERP (AEVAL (LIST 'LENGTH NEW_EQN)) 1)
                     (PROGN
                      (AEVAL (LIST 'TORDER (CONS 'LIST B) 'LEX))
                      (SETQ GB (AEVAL (LIST 'GROEBNER NEW_EQN)))
                      (COND
                       ((BOOLVALUE* TR_MERGE)
                        (PROGN
                         (ASSGNPRI (AEVAL "gb=") NIL 'FIRST)
                         (ASSGNPRI (AEVAL GB) NIL 'LAST))))
                      (AEVAL 'NIL))))
                   (SETQ H
                           (CONS 'LIST
                                 (PROG (HH FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ HH (CDR H))
                                   (COND ((NULL HH) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (HH)
                                                       (COND
                                                        ((AND
                                                          (PAIRP (CADDR HH))
                                                          (OR
                                                           (EQUAL (CAADDR HH)
                                                                  'QUOTIENT)
                                                           (AND
                                                            (EQUAL (CAADDR HH)
                                                                   '*SQ)
                                                            (NEQ
                                                             (CDR
                                                              (CADR
                                                               (CADDR HH)))
                                                             1))))
                                                         HH)
                                                        (T
                                                         (LIST 'EQUAL (CADR HH)
                                                               (AEVAL
                                                                (LIST 'PREDUCE
                                                                      (CADDR
                                                                       HH)
                                                                      GB))))))
                                                     (CAR HH))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ HH (CDR HH))
                                   (COND ((NULL HH) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (HH)
                                               (COND
                                                ((AND (PAIRP (CADDR HH))
                                                      (OR
                                                       (EQUAL (CAADDR HH)
                                                              'QUOTIENT)
                                                       (AND
                                                        (EQUAL (CAADDR HH)
                                                               '*SQ)
                                                        (NEQ
                                                         (CDR
                                                          (CADR (CADDR HH)))
                                                         1))))
                                                 HH)
                                                (T
                                                 (LIST 'EQUAL (CADR HH)
                                                       (AEVAL
                                                        (LIST 'PREDUCE
                                                              (CADDR HH)
                                                              GB))))))
                                             (CAR HH))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))))
                   (AEVAL 'NIL))))
                (SETQ SOL_LIST (DELETE S1 SOL_LIST))
                (SAVE_SOLUTION (CDR NEW_EQN) (CDR H) B INEQNEW
                 (CADR (CDDDDR SOL2)) S2)
                NIL))))))
          (COND ((AND ABSORB (NULL H)) NIL)
                (T
                 (PROGN
                  (COND
                   ((NULL INEQDROP)
                    (PROGN
                     (PROGN
                      (PRIN2 "Strange: merging ")
                      (PRIN2 S1)
                      (PRIN2 " and ")
                      (PRIN2 S2)
                      (PRIN2 " without dropping inequalities!")
                      NIL)
                     (TERPRI)
                     (PROGN
                      (PRIN2 "Probably ")
                      (PRIN2 S2)
                      (PRIN2 " had already been merged with ")
                      (PRIN2 S1)
                      (PRIN2 " or similar before.")
                      NIL)
                     (TERPRI)))
                   (PRINT_
                    (PROGN
                     (PROGN
                      (PRIN2 "Solution ")
                      (PRIN2 S2)
                      (PRIN2 " includes ")
                      (PRIN2 S1)
                      (PRIN2 " by dropping ")
                      NIL)
                     (COND
                      ((EQUAL (LENGTH INEQDROP) 1)
                       (PROGN (PRIN2 "inequality") NIL))
                      (T (PROGN (PRIN2 "inequalities") NIL)))
                     (TERPRI)
                     (PROG (INE)
                       (SETQ INE INEQDROP)
                      LAB
                       (COND ((NULL INE) (RETURN NIL)))
                       ((LAMBDA (INE) (MATHPRINT INE)) (CAR INE))
                       (SETQ INE (CDR INE))
                       (GO LAB)))))
                  S2))))))))) 
(PUT 'PREPARE_SOL_LIST 'NUMBER-OF-ARGS 0) 
(PUT 'PREPARE_SOL_LIST 'DEFINED-ON-LINE '7666) 
(PUT 'PREPARE_SOL_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PREPARE_SOL_LIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PREPARE_SOL_LIST NIL
    (PROG (S H)
      (CHANGE_PROMPT_TO "")
      (SETQ S (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
      (COND ((NOT (FILEP S)) (LIST_SOL_ON_DISK))
            (T
             (PROGN
              (IN (LIST S))
              (COND
               ((OR (NULL SOL_LIST) (ZEROP (LENGTH SOL_LIST)))
                (LIST_SOL_ON_DISK))
               (T
                (PROGN
                 (SETQ H (LENGTH SOL_LIST))
                 (PROGN
                  (PRIN2 "Do you want to see the list of names of the ")
                  NIL)
                 (COND ((EQUAL H 1) (PROGN (PRIN2 "single solution? ") NIL))
                       (T (PROGN (PRIN2 H) (PRIN2 " solutions? (y/n) ") NIL)))
                 (SETQ H (TERMREAD))
                 (COND
                  ((EQUAL H 'Y)
                   (PROGN
                    (TERPRI)
                    (PROGN (PRIN2 SOL_LIST) NIL)
                    (TERPRI)
                    (TERPRI))))
                 (PROGN
                  (PRIN2
                   "Is this the list to work on                           (Y)  ")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2 "or shall all solution files of this session in the ")
                  NIL)
                 (TERPRI)
                 (PROGN
                  (PRIN2
                   "current directory be collected and used?              (N): ")
                  NIL)
                 (SETQ H (TERMREAD))
                 (COND
                  ((EQUAL H 'N)
                   (PROGN
                    (LIST_SOL_ON_DISK)
                    (PROGN (PRIN2 "The following list is used:") NIL)
                    (TERPRI)
                    (TERPRI)
                    (PROGN (PRIN2 SOL_LIST) NIL)
                    (TERPRI)
                    (TERPRI))))))))))
      (RESTORE_INTERACTIVE_PROMPT))) 
(FLAG '(MERGE_SOL) 'OPFN) 
(PUT 'MERGE_SOL 'NUMBER-OF-ARGS 0) 
(PUT 'MERGE_SOL 'DEFINED-ON-LINE '7698) 
(PUT 'MERGE_SOL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MERGE_SOL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MERGE_SOL NIL
    (PROG (SOL_CP SL1 SL2 S1 S2 S3 SOL1 SOL2 ECHO_BAK SEMIC_BAK)
      (COND ((NULL SESSION_) (ASK_FOR_SESSION))
            (T
             (PROGN
              (PROGN
               (PRIN2
                "Do you want to merge solutions computed in this session,")
               NIL)
              (TERPRI)
              (COND
               ((NOT (YESP "i.e. since loading CRACK the last time? "))
                (ASK_FOR_SESSION))))))
      (PREPARE_SOL_LIST)
      (SETQ SOL_CP SOL_LIST)
      (SETQ SL1 SOL_CP)
      (COND
       (SL1
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND SL1 (CDR SL1))) (RETURN NIL)))
          (PROGN
           (SETQ S1 (CAR SL1))
           (SETQ SL1 (CDR SL1))
           (SETQ ECHO_BAK *ECHO)
           (SETQ SEMIC_BAK SEMIC*)
           (SETQ SEMIC* '$)
           (IN (LIST S1))
           (SETQ *ECHO ECHO_BAK)
           (SETQ SEMIC* SEMIC_BAK)
           (SETQ SOL1 BACKUP_)
           (SETQ BACKUP_ NIL)
           (COND
            (PRINT_
             (PROGN
              (PROGN (PRIN2 "Comparing ") (PRIN2 S1) (PRIN2 " with:") NIL)
              (TERPRI))))
           (SETQ SL2 SL1)
           (PROG ()
            WHILELABEL
             (COND ((NOT SL2) (RETURN NIL)))
             (PROGN
              (SETQ S2 (CAR SL2))
              (SETQ SL2 (CDR SL2))
              (SETQ ECHO_BAK *ECHO)
              (SETQ SEMIC_BAK SEMIC*)
              (SETQ SEMIC* '$)
              (IN (LIST S2))
              (SETQ *ECHO ECHO_BAK)
              (SETQ SEMIC* SEMIC_BAK)
              (SETQ SOL2 BACKUP_)
              (SETQ BACKUP_ NIL)
              (COND
               (PRINT_ (PROGN (PROGN (PRIN2 "  ") (PRIN2 S2) NIL) (TERPRI))))
              (COND
               ((AND (NULL (CAR SOL1)) (NULL (CAR SOL2)))
                (COND
                 ((LESSP
                   (DIFFERENCE (LENGTH (CADDDR SOL1)) (LENGTH (CADR SOL1)))
                   (DIFFERENCE (LENGTH (CADDDR SOL2)) (LENGTH (CADR SOL2))))
                  (SETQ S3 (MERGE_TWO S1 SOL1 S2 SOL2 T)))
                 ((GREATERP
                   (DIFFERENCE (LENGTH (CADDDR SOL1)) (LENGTH (CADR SOL1)))
                   (DIFFERENCE (LENGTH (CADDDR SOL2)) (LENGTH (CADR SOL2))))
                  (SETQ S3 (MERGE_TWO S2 SOL2 S1 SOL1 T)))
                 (T
                  (PROGN
                   (COND
                    ((NULL (SETQ S3 (MERGE_TWO S1 SOL1 S2 SOL2 T)))
                     (SETQ S3 (MERGE_TWO S2 SOL2 S1 SOL1 T))))
                   (COND
                    (S3
                     (PROGN
                      (PROGN
                       (PRIN2 "Strange: ")
                       (PRIN2 S1)
                       (PRIN2 " is contained in ")
                       (PRIN2 S2)
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2 "but both have same number of free unknowns!")
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        "One of them has probably undergone earlier merging")
                       NIL)
                      (TERPRI)
                      NIL))))))))
              (COND ((EQUAL S3 S1) (SETQ SL1 (DELETE S2 SL1)))
                    ((EQUAL S3 S2) (SETQ SL2 NIL))))
             (GO WHILELABEL)))
          (GO WHILELABEL))))
      (SAVE_SOL_LIST))) 
(PUT 'SAVE_SOL_LIST 'NUMBER-OF-ARGS 0) 
(PUT 'SAVE_SOL_LIST 'DEFINED-ON-LINE '7768) 
(PUT 'SAVE_SOL_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SAVE_SOL_LIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SAVE_SOL_LIST NIL
    (PROG (S A OFL*BAK SAVE)
      (SETQ S (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
      (SETQ A (OPEN S 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* S)
      (SETQ SAVE (WRS A))
      (PROGN (PRIN2 "off echo$ ") NIL)
      (TERPRI)
      (COND ((NULL SOL_LIST) (PROGN (PRIN2 "sol_list:=nil") NIL))
            (T (PROGN (PROGN (PRIN2 "sol_list:='") NIL) (PRINT SOL_LIST) NIL)))
      (PROGN (PRIN2 "$") NIL)
      (TERPRI)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A))) 
(PUT 'DELETE_EMPTY_SOL_LIST_FILE 'NUMBER-OF-ARGS 0) 
(PUT 'DELETE_EMPTY_SOL_LIST_FILE 'DEFINED-ON-LINE '7794) 
(PUT 'DELETE_EMPTY_SOL_LIST_FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETE_EMPTY_SOL_LIST_FILE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DELETE_EMPTY_SOL_LIST_FILE NIL
    (COND
     ((AND (NULL SOL_LIST) (NOT (FILEP PROCESS_COUNTER)) (NULL REDUCE_CALL))
      (SYSTEM (BLDMSG_INTERNAL "rm %w%w" (LIST SESSION_ "sol_list")))))) 
(PUT 'ADD_TO_SOL_LIST 'NUMBER-OF-ARGS 0) 
(PUT 'ADD_TO_SOL_LIST 'DEFINED-ON-LINE '7800) 
(PUT 'ADD_TO_SOL_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_TO_SOL_LIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ADD_TO_SOL_LIST NIL
    (COND
     (SOL_LIST
      (PROG (FL FPID FILE PIPEIN ST CNT A SAVE OFL*BAK)
        (SETQ FILE (BLDMSG_INTERNAL "%wsol_list" (LIST SESSION_)))
        (SETQ FPID (BLDMSG_INTERNAL "%s.%w" (LIST FILE (GETPID))))
        (SETQ CNT 0)
        (PROG ()
         REPEATLABEL
          (PROGN
           (SETQ FL (RENAME-FILE FILE FPID))
           (COND
            ((NULL FL)
             (PROGN
              (SETQ PIPEIN
                      (PIPE-OPEN (BLDMSG_INTERNAL "ls %s*" (LIST FILE))
                                 'INPUT))
              (SETQ ST (CHANNELREADLINE PIPEIN))
              (CLOSE PIPEIN)
              (COND ((NEQ ST "") (SLEEP 1))
                    ((LESSP CNT 4) (PROGN (SETQ CNT (ADD1 CNT)) (SLEEP 1)))
                    (T
                     (PROGN
                      (SETQ A (OPEN FPID 'OUTPUT))
                      (SETQ OFL*BAK OFL*)
                      (SETQ OFL* FPID)
                      (SETQ SAVE (WRS A))
                      (PROGN (PRIN2 "off echo$ ") NIL)
                      (TERPRI)
                      (PROGN (PRIN2 "sol_list:='") NIL)
                      (PRINT SOL_LIST)
                      (PROGN (PRIN2 "$") NIL)
                      (TERPRI)
                      (PROGN (PRIN2 "end$") NIL)
                      (TERPRI)
                      (WRS SAVE)
                      (SETQ OFL* OFL*BAK)
                      (CLOSE A)
                      (SETQ FL T))))))))
          (COND ((NOT FL) (GO REPEATLABEL))))
        (SETQ BACKUP_ SOL_LIST)
        (IN (LIST FPID))
        (SETQ SOL_LIST (UNION SOL_LIST BACKUP_))
        (SETQ A (OPEN FPID 'OUTPUT))
        (SETQ OFL*BAK OFL*)
        (SETQ OFL* FPID)
        (SETQ SAVE (WRS A))
        (PROGN (PRIN2 "off echo$ ") NIL)
        (TERPRI)
        (PROGN (PRIN2 "sol_list:='") NIL)
        (PRINT SOL_LIST)
        (PROGN (PRIN2 "$") NIL)
        (TERPRI)
        (PROGN (PRIN2 "end$") NIL)
        (TERPRI)
        (WRS SAVE)
        (SETQ OFL* OFL*BAK)
        (CLOSE A)
        (PROG ()
         REPEATLABEL
          (PROGN
           (SETQ FL (RENAME-FILE FPID FILE))
           (COND ((NULL FL) (SLEEP 1))))
          (COND ((NOT FL) (GO REPEATLABEL)))))))) 
(PUT 'ASK_FOR_SESSION 'NUMBER-OF-ARGS 0) 
(PUT 'ASK_FOR_SESSION 'DEFINED-ON-LINE '7890) 
(PUT 'ASK_FOR_SESSION 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ASK_FOR_SESSION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ASK_FOR_SESSION NIL
    (PROGN
     (CHANGE_PROMPT_TO
      "Name of the session in double quotes (e.g. \"bu263393-\"): ")
     (TERPRI)
     (SETQ SESSION_ (TERMREAD))
     (COND
      ((NOT (STRINGP SESSION_))
       (SETQ SESSION_
               (COMPRESS (CONS '|"| (APPEND (EXPLODE2 SESSION_) '(|"|)))))))
     (RESTORE_INTERACTIVE_PROMPT))) 
(FLAG '(PRI_SOL) 'OPFN) 
(PUT 'PRI_SOL 'NUMBER-OF-ARGS 7) 
(PUT 'PRI_SOL 'DEFINED-ON-LINE '7902) 
(PUT 'PRI_SOL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRI_SOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE PRI_SOL (SIN ASSGN CROUT HTML SOLCOUNT FNAME PRIND)
    (PROG (A B C SOUT ECHO_BAK SEMIC_BAK AA SAVE OFL*BAK)
      (SETQ ECHO_BAK *ECHO)
      (SETQ SEMIC_BAK SEMIC*)
      (SETQ SEMIC* '$)
      (IN (LIST SIN))
      (SETQ *ECHO ECHO_BAK)
      (SETQ SEMIC* SEMIC_BAK)
      (COND
       (HTML
        (PROGN
         (SETQ SOUT
                 (BLDMSG_INTERNAL "%w%w%d%w"
                                  (LIST FNAME "-s" SOLCOUNT ".html")))
         (SETQ AA (OPEN SOUT 'OUTPUT))
         (SETQ OFL*BAK OFL*)
         (SETQ OFL* SOUT)
         (SETQ SAVE (WRS AA))
         (PROGN (PRIN2 "<html>") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "<head>") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "<meta http-equiv=\"Content-Type\" content=\"text/html;")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "charset=iso-8859-1\">") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "<title>Solution ")
          (PRIN2 SOLCOUNT)
          (PRIN2 " to problem ")
          (PRIN2 PRIND)
          (PRIN2 "</title>")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "</head>") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "<BODY TEXT=\"#000000\" BGCOLOR=\"#FFFFFF\">") NIL)
         (TERPRI)
         (TERPRI)
         (PROGN
          (PRIN2 "<CENTER><H2>Solution ")
          (PRIN2 SOLCOUNT)
          (PRIN2 " to problem ")
          (PRIN2 PRIND)
          (PRIN2 "</H2>")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "<HR>") NIL)
         (TERPRI)
         (COND
          ((CADR BACKUP_)
           (PROGN
            (PROGN (PRIN2 "<A HREF=\"#1\">Remaining equations</A> | ") NIL)
            (TERPRI))))
         (PROGN (PRIN2 "<A HREF=\"#2\">Expressions</A> | ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "<A HREF=\"#3\">Parameters</A> | ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "<A HREF=\"#4\">Inequalities</A> | ") NIL)
         (TERPRI)
         (PROGN (PRIN2 "<A HREF=\"#5\">Relevance</A> | ") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "<A HREF=")
          (PRIN2 PRIND)
          (PRIN2 ".html>Back to problem ")
          (PRIN2 PRIND)
          (PRIN2 "</A> ")
          NIL)
         (PROGN (PRIN2 "</CENTER>") NIL)
         (TERPRI)
         (TERPRI))))
      (PROG (A)
        (SETQ A (CAR BACKUP_))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROG (B)
             (SETQ B (CDR A))
            LAB
             (COND ((NULL B) (RETURN NIL)))
             ((LAMBDA (B) (AEVAL (DEPEND (LIST (CAR A) B)))) (CAR B))
             (SETQ B (CDR B))
             (GO LAB)))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ BACKUP_ (CDR BACKUP_))
      (TERPRI)
      (COND (HTML (PROGN (PRIN2 "<!-- ") NIL)))
      (PROGN
       (PRIN2 ">>>=======>>> SOLUTION ")
       (PRIN2 SIN)
       (PRIN2 " <<<=======<<<")
       NIL)
      (COND (HTML (PROGN (PRIN2 " --> ") NIL)))
      (TERPRI)
      (TERPRI)
      (COND
       ((OR ASSGN HTML)
        (PROGN
         (COND
          ((CAR BACKUP_)
           (PROGN
            (COND
             (HTML
              (PROGN
               (PROGN (PRIN2 "<HR><A NAME=\"1\"></A><H3>Equations</H3>") NIL)
               (TERPRI)
               (PROGN (PRIN2 "The following unsolved equations remain:") NIL)
               (TERPRI)
               (PROGN (PRIN2 "<pre>") NIL)
               NIL))
             (T (PROGN (PRIN2 "Equations:") NIL)))
            (PROG (A)
              (SETQ A (CAR BACKUP_))
             LAB
              (COND ((NULL A) (RETURN NIL)))
              ((LAMBDA (A) (MATHPRINT (LIST 'EQUAL 0 A))) (CAR A))
              (SETQ A (CDR A))
              (GO LAB))
            (COND (HTML (PROGN (PROGN (PRIN2 "</pre>") NIL) (TERPRI)))))))
         (COND
          (HTML
           (PROGN
            (PROGN (PRIN2 "<HR><A NAME=\"2\"></A><H3>Expressions</H3>") NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "The solution is given through the following expressions:")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "<pre>") NIL)
            (TERPRI)
            (PROG (A)
              (SETQ A (CADR BACKUP_))
             LAB
              (COND ((NULL A) (RETURN NIL)))
              ((LAMBDA (A) (MATHPRINT A)) (CAR A))
              (SETQ A (CDR A))
              (GO LAB))
            (PROGN (PRIN2 "</pre>") NIL)
            (TERPRI)))
          (T
           (PROGN
            (SETQ B NIL)
            (PROG (A)
              (SETQ A (CADR BACKUP_))
             LAB
              (COND ((NULL A) (RETURN NIL)))
              ((LAMBDA (A)
                 (SETQ B
                         (CONS
                          (LIST 'EQUAL (CADR A)
                                (COND
                                 ((AND (PAIRP (CADDR A))
                                       (EQUAL (CAR (CADDR A)) '*SQ))
                                  (CADR (CADDR A)))
                                 (T (SIMP (CADDR A)))))
                          B)))
               (CAR A))
              (SETQ A (CDR A))
              (GO LAB))
            (PRINT_FORG B NIL))))
         (TERPRI)
         (COND
          (HTML
           (PROGN
            (PROGN (PRIN2 "<HR><A NAME=\"3\"></A><H3>Parameters</H3>") NIL)
            (TERPRI)
            (PROGN
             (PRIN2
              "Apart from the condition that they must not vanish to give")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "a non-trivial solution and a non-singular solution with")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2
              "non-vanishing denominators, the following parameters are free:")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "<pre> ") NIL)
            (FCTPRINT (CADDR BACKUP_))
            (PROGN (PRIN2 "</pre>") NIL)
            (TERPRI)))
          (T
           (PROGN
            (PROGN
             (PRIN2 (LENGTH (CADDR BACKUP_)))
             (PRIN2 " free unknowns: ")
             NIL)
            (LISTPRINT (CADDR BACKUP_))
            (PRINT_INEQ
             (CONS
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (CADDDR BACKUP_))
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
                (GO LOOPLABEL))
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (CAR (CDDDDR BACKUP_)))
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (PROG (B FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ B A)
                                      (COND ((NULL B) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (B)
                                                          (PROG (C
                                                                 FORALL-RESULT
                                                                 FORALL-ENDPTR)
                                                            (SETQ C B)
                                                            (COND
                                                             ((NULL C)
                                                              (RETURN NIL)))
                                                            (SETQ FORALL-RESULT
                                                                    (SETQ FORALL-ENDPTR
                                                                            (CONS
                                                                             ((LAMBDA
                                                                                  (
                                                                                   C)
                                                                                (SIMP
                                                                                 C))
                                                                              (CAR
                                                                               C))
                                                                             NIL)))
                                                           LOOPLABEL
                                                            (SETQ C (CDR C))
                                                            (COND
                                                             ((NULL C)
                                                              (RETURN
                                                               FORALL-RESULT)))
                                                            (RPLACD
                                                             FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (C)
                                                                 (SIMP C))
                                                               (CAR C))
                                                              NIL))
                                                            (SETQ FORALL-ENDPTR
                                                                    (CDR
                                                                     FORALL-ENDPTR))
                                                            (GO LOOPLABEL)))
                                                        (CAR B))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ B (CDR B))
                                      (COND ((NULL B) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (B)
                                                  (PROG (C FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ C B)
                                                    (COND
                                                     ((NULL C) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (C)
                                                                        (SIMP
                                                                         C))
                                                                      (CAR C))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ C (CDR C))
                                                    (COND
                                                     ((NULL C)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (C)
                                                                (SIMP C))
                                                              (CAR C))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))
                                                (CAR B))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (PROG (B FORALL-RESULT FORALL-ENDPTR)
                              (SETQ B A)
                              (COND ((NULL B) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (B)
                                                  (PROG (C FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ C B)
                                                    (COND
                                                     ((NULL C) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (C)
                                                                        (SIMP
                                                                         C))
                                                                      (CAR C))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ C (CDR C))
                                                    (COND
                                                     ((NULL C)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (C)
                                                                (SIMP C))
                                                              (CAR C))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))
                                                (CAR B))
                                               NIL)))
                             LOOPLABEL
                              (SETQ B (CDR B))
                              (COND ((NULL B) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (B)
                                          (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ C B)
                                            (COND ((NULL C) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (C)
                                                                (SIMP C))
                                                              (CAR C))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ C (CDR C))
                                            (COND
                                             ((NULL C) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (C) (SIMP C))
                                                      (CAR C))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))
                                        (CAR B))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))
            NIL)))
         (COND
          (HTML
           (PROGN
            (PROGN (PRIN2 "<HR><A NAME=\"4\"></A><H3>Inequalities</H3>") NIL)
            (TERPRI)
            (PROGN
             (PRIN2
              "In the following not identically vanishing expressions are shown.")
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "<pre> ") NIL)
            (MATHPRINT (CONS 'LIST (CADDDR BACKUP_)))
            (PROGN (PRIN2 "</pre>") NIL)
            (TERPRI)
            (COND
             ((AND (CDDDDR BACKUP_) (CAR (CDDDDR BACKUP_)))
              (PROGN
               (PROGN
                (PRIN2
                 "Next come so-called OR-lists of FACTOR-lists in the following sense.")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "Each FACTOR-list represents the factors of an expression and at least one of")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "these expressions must not vanish in each OR-list. In other words, in each")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "OR-list at least one FACTOR-list must not vanish, i.e. none of the expressions")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "in the FACTOR-list may vanish.<BR>") NIL)
               (TERPRI)
               (PROG (A)
                 (SETQ A (CAR (CDDDDR BACKUP_)))
                LAB
                 (COND ((NULL A) (RETURN NIL)))
                 ((LAMBDA (A)
                    (PROGN
                     (PROGN (PRIN2 "OR-list:") NIL)
                     (TERPRI)
                     (PROGN (PRIN2 "<pre> ") NIL)
                     (MATHPRINT
                      (CONS 'LIST
                            (PROG (B FORALL-RESULT FORALL-ENDPTR)
                              (SETQ B A)
                              (COND ((NULL B) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (B) (CONS 'LIST B))
                                                (CAR B))
                                               NIL)))
                             LOOPLABEL
                              (SETQ B (CDR B))
                              (COND ((NULL B) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (B) (CONS 'LIST B)) (CAR B))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                     (PROGN (PRIN2 "</pre>") NIL)
                     (TERPRI)))
                  (CAR A))
                 (SETQ A (CDR A))
                 (GO LAB))))))))
         NIL)))
      (COND
       (HTML
        (PROGN
         (PROGN
          (PRIN2
           "<HR><A NAME=\"5\"></A><H3>Relevance for the application:</H3>")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "<pre>") NIL))))
      (COND
       ((OR CROUT HTML)
        (PROGN
         (AEVAL
          (LIST 'CRACK_OUT (CONS 'LIST (CAR BACKUP_))
                (CONS 'LIST (CADR BACKUP_)) (CONS 'LIST (CADDR BACKUP_))
                (CONS 'LIST (CADDDR BACKUP_)) SOLCOUNT))
         NIL)))
      (COND
       (HTML
        (PROGN
         (PROGN (PRIN2 "</pre>") NIL)
         (TERPRI)
         (PROGN (PRIN2 "<HR>") NIL)
         (TERPRI)
         (PROGN (PRIN2 "</body>") NIL)
         (TERPRI)
         (PROGN (PRIN2 "</html>") NIL)
         (TERPRI)
         (WRS SAVE)
         (SETQ OFL* OFL*BAK)
         (CLOSE AA)
         NIL)))
      (SETQ BACKUP_ NIL))) 
(FLAG '(PRINT_ALL_SOL) 'OPFN) 
(PUT 'PRINT_ALL_SOL 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_ALL_SOL 'DEFINED-ON-LINE '8048) 
(PUT 'PRINT_ALL_SOL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_ALL_SOL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_ALL_SOL NIL
    (PROG (A ASSGN CROUT NATBAK PRINT_MORE_BAK FNAME SOLCOUNT HTML PRIND
           PRINT_BAK)
      (PROGN
       (PRIN2 "This is a reminder for you to read in any file CRACK_OUT.RED")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "with a procedure CRACK_OUT() in case that is necessary to display")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "results following from solutions to be printed.") NIL)
      (TERPRI)
      (TERPRI)
      (COND ((NULL SESSION_) (ASK_FOR_SESSION))
            (T
             (PROGN
              (PROGN
               (PRIN2
                "Do you want to print solutions computed in this session,")
               NIL)
              (TERPRI)
              (COND
               ((NOT (YESP "i.e. since loading CRACK the last time? "))
                (ASK_FOR_SESSION)))
              NIL)))
      (PREPARE_SOL_LIST)
      (SETQ NATBAK *NAT)
      (SETQ PRINT_MORE_BAK PRINT_MORE)
      (SETQ PRINT_MORE T)
      (SETQ PRINT_BAK PRINT_)
      (SETQ PRINT_ 100000)
      (COND
       ((YESP "Do you want to generate an html file for each solution? ")
        (PROGN
         (SETQ HTML T)
         (TERPRI)
         (PROGN (PRIN2 "What is the file name (including the path)") NIL)
         (TERPRI)
         (PROGN (PRIN2 "that shall be used (in double quotes) ? ") NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "(A suffix '-si'  will be added for each solution 'i'.) ")
          NIL)
         (CHANGE_PROMPT_TO "")
         (SETQ FNAME (TERMREAD))
         (TERPRI)
         (PROGN (PRIN2 "What is a short name for the problem? ") NIL)
         (SETQ PRIND (TERMREAD))
         (RESTORE_INTERACTIVE_PROMPT)
         (TERPRI)
         NIL))
       (T
        (PROGN
         (COND
          ((YESP "Do you want to see the computed value of each function? ")
           (SETQ ASSGN T)))
         (COND
          ((YESP "Do you want procedure `crack_out' to be called? ")
           (PROGN
            (SETQ CROUT T)
            (COND
             ((AND FLIN_ FHOM_)
              (COND
               ((YESP "Do you want to print less (e.g. no symmetries)? ")
                (SETQ PRINT_MORE NIL)))))
            (COND
             ((NOT
               (YESP
                "Do you want natural output (no if you want to paste and copy)? "))
              (SETQ *NAT NIL)))
            NIL)))
         NIL)))
      (SETQ SOLCOUNT 0)
      (SETQ FSUB_ NIL)
      (PROG (A)
        (SETQ A SOL_LIST)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (SETQ SOLCOUNT (ADD1 SOLCOUNT))
            (PRI_SOL A ASSGN CROUT HTML SOLCOUNT FNAME PRIND)
            NIL))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ *NAT NATBAK)
      (SETQ PRINT_ PRINT_BAK)
      (SETQ PRINT_MORE PRINT_MORE_BAK))) 
(PUT 'FREQUENT_FACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'FREQUENT_FACTORS 'DEFINED-ON-LINE '8111) 
(PUT 'FREQUENT_FACTORS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FREQUENT_FACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREQUENT_FACTORS (PDES)
    (PROG (P PV F FCL FCC H NF)
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
               (SETQ NF (LENGTH PV))
               (PROG ()
                WHILELABEL
                 (COND ((NOT PV) (RETURN NIL)))
                 (PROGN
                  (SETQ F (CAR PV))
                  (SETQ PV (CDR PV))
                  (SETQ FCC FCL)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND FCC (NEQ (CADDAR FCC) F))) (RETURN NIL)))
                    (SETQ FCC (CDR FCC))
                    (GO WHILELABEL))
                  (COND
                   (FCC
                    (PROGN
                     (SETQ H
                             (LIST (ADD1 (CAAR FCC))
                                   (COND ((LESSP NF (CAADAR FCC)) (LIST NF 1))
                                         ((EQUAL NF (CAADAR FCC))
                                          (LIST NF (ADD1 (CADR (CADAR FCC)))))
                                         (T (CADAR FCC)))
                                   F))
                     (RPLACA FCC H)
                     NIL))
                   (T (SETQ FCL (CONS (LIST 1 (LIST NF 1) F) FCL)))))
                 (GO WHILELABEL))
               NIL)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (REV_IDX_SORT FCL)))) 
(PUT 'PRINT_FACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_FACTORS 'DEFINED-ON-LINE '8155) 
(PUT 'PRINT_FACTORS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_FACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_FACTORS (PDES)
    (PROG (FCL P Q)
      (SETQ FCL (REVERSE (FREQUENT_FACTORS PDES)))
      (PROGN
       (PRIN2 "Number of occurences, eqn of fewest # of factors, the factor: ")
       NIL)
      (TERPRI)
      (PROG (P)
        (SETQ P FCL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((GREATERP (SETQ Q (PDEWEIGHTSF (CAR (CADDR P)) FTEM_)) PRINT_)
             (PROGN
              (PROGN
               (PRIN2 (CAR P))
               (PRIN2 ",")
               (PRIN2 (CADR P))
               (PRIN2 " : ")
               (PRIN2 (NO_OF_TM_SF (CAR (CADDR P))))
               (PRIN2 " terms")
               NIL)
              (TERPRI)))
            (T
             (PROGN
              (PROGN
               (PRIN2 (CAR P))
               (PRIN2 ",")
               (PRIN2 (CADR P))
               (PRIN2 " : ")
               NIL)
              (SETQ P (LIST '*SQ (CADDR P) T))
              (COND
               ((EQUAL Q 1) (PROGN (PROGN (PRIN2 (REVAL1 P T)) NIL) (TERPRI)))
               (T (MATHPRINT P)))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB)))) 
(PUT 'FREQUENT_COEFFICIENTS 'NUMBER-OF-ARGS 1) 
(PUT 'FREQUENT_COEFFICIENTS 'DEFINED-ON-LINE '8170) 
(PUT 'FREQUENT_COEFFICIENTS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FREQUENT_COEFFICIENTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREQUENT_COEFFICIENTS (PDES)
    (PROG (S G CL H P Q R)
      (PROG (S)
        (SETQ S PDES)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((AND (FCTEVAL S) (SETQ G (GET S 'FCTEVAL_NLI)))
             (PROG (H)
               (SETQ H G)
              LAB
               (COND ((NULL H) (RETURN NIL)))
               ((LAMBDA (H)
                  (PROGN
                   (SETQ Q (SIMPLIFYSQ (CAR H) FTEM_ T NIL NIL))
                   (PROG (R)
                     (SETQ R Q)
                    LAB
                     (COND ((NULL R) (RETURN NIL)))
                     ((LAMBDA (R)
                        (COND
                         ((NULL (SETQ P (ASSOC R CL)))
                          (SETQ CL
                                  (CONS (CONS R (CONS (LIST S) (LIST (CDR H))))
                                        CL)))
                         (T
                          (PROGN
                           (SETQ CL (DELETE P CL))
                           (SETQ CL
                                   (CONS
                                    (CONS R
                                          (CONS (UNION (LIST S) (CADR P))
                                                (UNION (LIST (CDR H))
                                                       (CDDR P))))
                                    CL))))))
                      (CAR R))
                     (SETQ R (CDR R))
                     (GO LAB))))
                (CAR H))
               (SETQ H (CDR H))
               (GO LAB)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ CL
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H CL)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (CONS
                                     (MIN (LENGTH (CADR H)) (LENGTH (CDDR H)))
                                     (CAR H)))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (CONS (MIN (LENGTH (CADR H)) (LENGTH (CDDR H)))
                                  (CAR H)))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (REV_IDX_SORT CL)))) 
(PUT 'PRINT_COEFFICIENTS 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_COEFFICIENTS 'DEFINED-ON-LINE '8190) 
(PUT 'PRINT_COEFFICIENTS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PRINT_COEFFICIENTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_COEFFICIENTS (PDES)
    (PROG (CL P Q)
      (PROGN (PRIN2 "This can take  longer.") NIL)
      (TERPRI)
      (PROGN (PRIN2 "The shown number is the minimum of ") NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "- the number of different equations in which the coefficient occurs and")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2
        "- the number of different functions of which this is a coefficient.")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "# of subst., the coeff.: ") NIL)
      (TERPRI)
      (SETQ CL (REVERSE (FREQUENT_COEFFICIENTS PDES)))
      (PROG (P)
        (SETQ P CL)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((GREATERP (SETQ Q (PDEWEIGHTSF (CAR (CDR P)) FTEM_)) PRINT_)
             (PROGN
              (PROGN
               (PRIN2 (CAR P))
               (PRIN2 " : ")
               (PRIN2 (NO_OF_TM_SF (CAR (CDR P))))
               (PRIN2 " terms")
               NIL)
              (TERPRI)))
            (T
             (PROGN
              (PROGN (PRIN2 (CAR P)) (PRIN2 " : ") NIL)
              (SETQ P (LIST '*SQ (CDR P) T))
              (COND
               ((EQUAL Q 1) (PROGN (PROGN (PRIN2 (REVAL1 P T)) NIL) (TERPRI)))
               (T (MATHPRINT P)))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB)))) 
(PUT 'CASE_ON_MOST_FREQU_FACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'CASE_ON_MOST_FREQU_FACTORS 'DEFINED-ON-LINE '8209) 
(PUT 'CASE_ON_MOST_FREQU_FACTORS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CASE_ON_MOST_FREQU_FACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASE_ON_MOST_FREQU_FACTORS (ARGLIST)
    (PROG (H MAXF BEST H3 H4)
      (SETQ H (FREQUENT_FACTORS (CAR ARGLIST)))
      (COND ((NULL H) (RETURN NIL)))
      (SETQ MAXF (CAAR H))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND H
                (GREATERP (DIFFERENCE (TIMES (CAAR H) 10) (TIMES MAXF 2)) 0)))
          (RETURN NIL)))
        (PROGN
         (COND ((NOT (PAIRP (CADDAR H))) (SETQ H4 T))
               (T
                (PROGN
                 (SETQ H3
                         (MKEQSQ (CADDAR H) NIL NIL FTEM_ VL_ ALLFLAGS_ T
                          (LIST 0) NIL NIL))
                 (FCTEVAL H3)
                 (SETQ H4 (OR (GET H3 'FCTEVAL_LIN) (GET H3 'FCTEVAL_NCA)))
                 (DROP_PDE H3 NIL NIL)
                 NIL)))
         (COND
          ((AND H4
                (OR (NULL BEST) (LESSP (CAADAR H) (CAADR BEST))
                    (AND (EQUAL (CAADAR H) (CAADR BEST))
                         (GREATERP (CADADR (CAR H)) (CADADR BEST)))))
           (SETQ BEST (CAR H))))
         (SETQ H (CDR H)))
        (GO WHILELABEL))
      (RETURN
       (COND
        (BEST
         (SPLIT_INTO_CASES
          (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) (CADDR BEST))))
        (T NIL))))) 
(PUT 'SOL_IN_LIST 'NUMBER-OF-ARGS 3) 
(PUT 'SOL_IN_LIST 'DEFINED-ON-LINE '8251) 
(PUT 'SOL_IN_LIST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SOL_IN_LIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOL_IN_LIST (SET1 SET2 SOL_LIST2)
    (PROG (SET2CP S1 S2 FOUND SOL1 SOL2 SAME_SETS ECHO_BAK SEMIC_BAK)
      (PROG ()
       WHILELABEL
        (COND ((NOT SET1) (RETURN NIL)))
        (PROGN
         (SETQ S1 (CAR SET1))
         (SETQ SET1 (CDR SET1))
         (SETQ ECHO_BAK *ECHO)
         (SETQ SEMIC_BAK SEMIC*)
         (SETQ SEMIC* '$)
         (IN (LIST S1))
         (SETQ *ECHO ECHO_BAK)
         (SETQ SEMIC* SEMIC_BAK)
         (SETQ SOL1 BACKUP_)
         (SETQ BACKUP_ NIL)
         (SETQ SET2CP SET2)
         (SETQ FOUND NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND SET2CP (NOT FOUND))) (RETURN NIL)))
           (PROGN
            (SETQ S2 (CAR SET2CP))
            (SETQ SET2CP (CDR SET2CP))
            (SETQ ECHO_BAK *ECHO)
            (SETQ SEMIC_BAK SEMIC*)
            (SETQ SEMIC* '$)
            (IN (LIST S2))
            (SETQ *ECHO ECHO_BAK)
            (SETQ SEMIC* SEMIC_BAK)
            (SETQ SOL2 BACKUP_)
            (SETQ BACKUP_ NIL)
            (SETQ FOUND (MERGE_TWO S1 SOL1 S2 SOL2 NIL))
            NIL)
           (GO WHILELABEL))
         (COND
          ((NOT FOUND)
           (PROGN
            (SETQ SAME_SETS NIL)
            (COND
             (PRINT_
              (PROGN
               (PROGN
                (PRIN2 "Solution ")
                (PRIN2 S1)
                (PRIN2 " is not included in ")
                (PRIN2 SOL_LIST2)
                NIL)
               (TERPRI))))))))
        (GO WHILELABEL))
      (RETURN SAME_SETS))) 
(FLAG '(SAME_SOL_SETS) 'OPFN) 
(PUT 'SAME_SOL_SETS 'NUMBER-OF-ARGS 0) 
(PUT 'SAME_SOL_SETS 'DEFINED-ON-LINE '8283) 
(PUT 'SAME_SOL_SETS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SAME_SOL_SETS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SAME_SOL_SETS NIL
    (PROG (SESSION_BAK SET1 SET2 SOL_LIST1 SOL_LIST2 ECHO_BAK SEMIC_BAK)
      (SETQ SESSION_BAK SESSION_)
      (PROGN
       (PRIN2 "Two sets of solutions are compared whether they are identical.")
       NIL)
      (PROGN
       (PRIN2
        "What is the name of the session that produced the first set of solutions?")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "(CRACK will look for the file `sessionname'+`sol_list'.)")
       NIL)
      (TERPRI)
      (ASK_FOR_SESSION)
      (SETQ SOL_LIST1 (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
      (SETQ ECHO_BAK *ECHO)
      (SETQ SEMIC_BAK SEMIC*)
      (SETQ SEMIC* '$)
      (IN (LIST SOL_LIST1))
      (SETQ *ECHO ECHO_BAK)
      (SETQ SEMIC* SEMIC_BAK)
      (SETQ SET1 SOL_LIST)
      (PROGN
       (PRIN2
        "What is the name of the session that produced the second set of solutions?")
       NIL)
      (TERPRI)
      (ASK_FOR_SESSION)
      (SETQ SOL_LIST2 (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list")))
      (SETQ ECHO_BAK *ECHO)
      (SETQ SEMIC_BAK SEMIC*)
      (SETQ SEMIC* '$)
      (IN (LIST SOL_LIST2))
      (SETQ *ECHO ECHO_BAK)
      (SETQ SEMIC* SEMIC_BAK)
      (SETQ SET2 SOL_LIST)
      (SETQ SESSION_ SESSION_BAK)
      (SOL_IN_LIST SET1 SET2 SOL_LIST2)
      (SOL_IN_LIST SET2 SET1 SOL_LIST1))) 
(FLAG '(CLEAR_SESSION_FILES) 'OPFN) 
(PUT 'CLEAR_SESSION_FILES 'NUMBER-OF-ARGS 0) 
(PUT 'CLEAR_SESSION_FILES 'DEFINED-ON-LINE '8324) 
(PUT 'CLEAR_SESSION_FILES 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CLEAR_SESSION_FILES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CLEAR_SESSION_FILES NIL
    (PROG (S)
      (SETQ S (EXPLODE SESSION_))
      (SETQ S (COMPRESS (CONS '|"| (CDDDR S))))
      (SETQ S (BLDMSG_INTERNAL "%w%w%w" (LIST "rm ??" S "*")))
      (SYSTEM S))) 
(PUT 'LIST_SOL_ON_DISK 'NUMBER-OF-ARGS 0) 
(PUT 'LIST_SOL_ON_DISK 'DEFINED-ON-LINE '8332) 
(PUT 'LIST_SOL_ON_DISK 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_SOL_ON_DISK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIST_SOL_ON_DISK NIL
    (PROG (S CHN XX OLDCASE)
      (SETQ S (LEVEL_STRING SESSION_))
      (SETQ S (EXPLODE S))
      (SETQ S (COMPRESS (CONS '|"| (CONS 'S (CONS 'O (CDDDR S))))))
      (SYSTEM (BLDMSG_INTERNAL "ls %s* > %w%w" (LIST S SESSION_ "sol_list")))
      (SETQ CHN
              (OPEN (BLDMSG_INTERNAL "%w%w" (LIST SESSION_ "sol_list"))
                    'INPUT))
      (SETQ CHN (RDS CHN))
      (SETQ SOL_LIST NIL)
      (SETQ OLDCASE (INPUT-CASE NIL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (SETQ XX (READ)) (NEQ XX (INT2ID 4)))) (RETURN NIL)))
        (SETQ SOL_LIST (CONS (BLDMSG_INTERNAL "%w" (LIST XX)) SOL_LIST))
        (GO WHILELABEL))
      (CLOSE (RDS CHN))
      (SAVE_SOL_LIST)
      (INPUT-CASE OLDCASE))) 
(PUT 'FNC_OF_NEW_VAR 'NUMBER-OF-ARGS 0) 
(PUT 'FNC_OF_NEW_VAR 'DEFINED-ON-LINE '8358) 
(PUT 'FNC_OF_NEW_VAR 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FNC_OF_NEW_VAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FNC_OF_NEW_VAR NIL
    (PROG (H4 H5 H6)
      (SETQ H4
              (PROG (H5 FORALL-RESULT FORALL-ENDPTR)
                (SETQ H5 (CDR DONE_TRAFO))
               STARTOVER
                (COND ((NULL H5) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (H5)
                           (PROG (H6 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ H6 (CDR H5))
                             (COND ((NULL H6) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (H6) (CADR H6))
                                               (CAR H6))
                                              NIL)))
                            LOOPLABEL
                             (SETQ H6 (CDR H6))
                             (COND ((NULL H6) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (H6) (CADR H6)) (CAR H6))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR H5)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ H5 (CDR H5))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL H5) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (H5)
                           (PROG (H6 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ H6 (CDR H5))
                             (COND ((NULL H6) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (H6) (CADR H6))
                                               (CAR H6))
                                              NIL)))
                            LOOPLABEL
                             (SETQ H6 (CDR H6))
                             (COND ((NULL H6) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (H6) (CADR H6)) (CAR H6))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR H5)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ H5 (CDR H5))
                (GO LOOPLABEL)))
      (SETQ H5 NIL)
      (PROG (H6)
        (SETQ H6 DEPL*)
       LAB
        (COND ((NULL H6) (RETURN NIL)))
        ((LAMBDA (H6)
           (COND ((NOT (FREEOFLIST H6 H4)) (SETQ H5 (CONS (CAR H6) H5)))))
         (CAR H6))
        (SETQ H6 (CDR H6))
        (GO LAB))
      (RETURN H5))) 
(PUT 'COPY-FILE 'NUMBER-OF-ARGS 2) 
(PUT 'COPY-FILE 'DEFINED-ON-LINE '8373) 
(PUT 'COPY-FILE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'COPY-FILE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COPY-FILE (N1 N2)
    (PROG (F1 F2 C SAVERAISE)
      (SETQ SAVERAISE (CONS *RAISE *LOWER))
      (SETQ *RAISE (SETQ *LOWER NIL))
      (COND ((NULL (SETQ F1 (OPEN N1 'INPUT))) (RETURN NIL)))
      (COND
       ((NULL (SETQ F2 (OPEN N2 'OUTPUT))) (PROGN (CLOSE F1) (RETURN NIL))))
      (SETQ F1 (RDS F1))
      (SETQ F2 (WRS F2))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ (SETQ C (READCH)) '$EOF$)) (RETURN NIL)))
        (PRIN2 C)
        (GO WHILELABEL))
      (CLOSE (RDS F1))
      (CLOSE (WRS F2))
      (SETQ *RAISE (CAR SAVERAISE))
      (SETQ *LOWER (CDR SAVERAISE))
      (RETURN T))) 
(PUT 'DELETE-FILE-EXACT 'NUMBER-OF-ARGS 1) 
(PUT 'DELETE-FILE-EXACT 'DEFINED-ON-LINE '8400) 
(PUT 'DELETE-FILE-EXACT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETE-FILE-EXACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETE-FILE-EXACT (FI)
    (COND
     ((AND
       (OR (MEMQ 'LINUX-GNU LISPSYSTEM*) (MEMQ 'CYGWIN LISPSYSTEM*)
           (MEMQ 'UNIX LISPSYSTEM*))
       (NOT (MEMQ 'WIN32 LISPSYSTEM*)) (NOT (MEMQ 'WIN64 LISPSYSTEM*)))
      (SYSTEM (BLDMSG_INTERNAL "rm -f %w" (LIST FI))))
     ((FILEP FI) (SYSTEM (BLDMSG_INTERNAL "del \"%w\"" (LIST FI)))))) 
(PUT 'DELETE-FILE-MATCH 'NUMBER-OF-ARGS 1) 
(PUT 'DELETE-FILE-MATCH 'DEFINED-ON-LINE '8426) 
(PUT 'DELETE-FILE-MATCH 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETE-FILE-MATCH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETE-FILE-MATCH (FI)
    (COND
     ((AND
       (OR (MEMQ 'LINUX-GNU LISPSYSTEM*) (MEMQ 'CYGWIN LISPSYSTEM*)
           (MEMQ 'UNIX LISPSYSTEM*))
       (NOT (MEMQ 'WIN32 LISPSYSTEM*)) (NOT (MEMQ 'WIN64 LISPSYSTEM*)))
      (SYSTEM (BLDMSG_INTERNAL "rm -f %s" (LIST FI))))
     (T
      (PROG (U)
        (PROG (C)
          (SETQ C (EXPLODE FI))
         LAB
          (COND ((NULL C) (RETURN NIL)))
          ((LAMBDA (C)
             (COND ((OR (EQUAL C '?) (EQUAL C '*)) (SETQ U (CONS 'X U)))
                   (T (SETQ U (CONS C U)))))
           (CAR C))
          (SETQ C (CDR C))
          (GO LAB))
        (SETQ U (COMPRESS (REVERSE U)))
        (SETQ U (OPEN U 'OUTPUT))
        (COND (U (CLOSE U)))
        (RETURN (SYSTEM (BLDMSG_INTERNAL "del \"%s\"" (LIST FI)))))))) 
(ENDMODULE) 
(MODULE (LIST 'UNIQUIFY)) 
(PUT 'UNIQUIFYSQ 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYSQ 'DEFINED-ON-LINE '8480) 
(PUT 'UNIQUIFYSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYSQ (U) (PROG () (UNIQUIFYF (CAR U)) (UNIQUIFYF (CDR U)) (RETURN U))) 
(PUT 'UNIQUIFYF 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYF 'DEFINED-ON-LINE '8487) 
(PUT 'UNIQUIFYF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYF (U)
    (PROG ()
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN NIL)) ((ATOM (CAAAR U)) NIL)
            (T (RPLACA (CAAR U) (UNIQUIFYK (CAAAR U)))))
      (UNIQUIFYF (CDAR U))
      (UNIQUIFYF (CDR U))
      (RETURN U))) 
(PUT 'UNIQUIFYK 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYK 'DEFINED-ON-LINE '8508) 
(PUT 'UNIQUIFYK 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYK (U)
    (PROG (X)
      (COND ((SFP U) (UNIQUIFYF U)))
      (SETQ X (FKERN U))
      (COND ((SFP (CAR X)) (RETURN (CAR X))))
      (COND ((MEMQ 'USED* (CDDR X)) (RETURN (CAR X))) (T (ACONC X 'USED*)))
      (PROG (ARG)
        (SETQ ARG (CDR U))
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG) (COND ((ATOM ARG) NIL) (T (UNIQUIFYK ARG)))) (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (RETURN (CAR X)))) 
(PUT 'UNIQUIFYKORD 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYKORD 'DEFINED-ON-LINE '8521) 
(PUT 'UNIQUIFYKORD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYKORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYKORD (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (COND ((ATOM J) J) (T (UNIQUIFYK J))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J) (COND ((ATOM J) J) (T (UNIQUIFYK J)))) (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'UNIQUIFYDEPL 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYDEPL 'DEFINED-ON-LINE '8525) 
(PUT 'UNIQUIFYDEPL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYDEPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYDEPL (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND ((ATOM (CAR J)) J)
                                (T (CONS (UNIQUIFYK (CAR J)) (CDR J)))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND ((ATOM (CAR J)) J)
                        (T (CONS (UNIQUIFYK (CAR J)) (CDR J)))))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'UNIQUIFYASYMPLIS 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUIFYASYMPLIS 'DEFINED-ON-LINE '8529) 
(PUT 'UNIQUIFYASYMPLIS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYASYMPLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUIFYASYMPLIS (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND ((ATOM (CAR J)) J)
                                (T (CONS (UNIQUIFYK (CAR J)) (CDR J)))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND ((ATOM (CAR J)) J)
                        (T (CONS (UNIQUIFYK (CAR J)) (CDR J)))))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'UNIQUENESSSQ 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUENESSSQ 'DEFINED-ON-LINE '8533) 
(PUT 'UNIQUENESSSQ 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUENESSSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUENESSSQ (U) (PROGN (UNIQUENESSF (CAR U)) (UNIQUENESSF (CDR U)) NIL)) 
(PUT 'UNIQUIFYALL 'NUMBER-OF-ARGS 2) 
(PUT 'UNIQUIFYALL 'DEFINED-ON-LINE '8538) 
(PUT 'UNIQUIFYALL 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUIFYALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNIQUIFYALL (PDES FORG)
    (PROG (A B C)
      (PROG (A)
        (SETQ A PDES)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (UNIQUIFYSQ (GET A 'SQVAL))
            (COND
             ((PAIRP (GET A 'FAC))
              (PROG (B)
                (SETQ B (GET A 'FAC))
               LAB
                (COND ((NULL B) (RETURN NIL)))
                ((LAMBDA (B) (UNIQUIFYSQ B)) (CAR B))
                (SETQ B (CDR B))
                (GO LAB))))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_LIN))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_NCA))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_NLI))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_LIN))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NCA))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NLI))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NUS))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUIFYSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A FORG)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((AND (PAIRP A) (EQUAL (CAR A) 'EQUAL)) (UNIQUIFYSQ (CADDR A)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A INEQ_)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (UNIQUIFYSQ A)) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A INEQ_OR)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROG (B)
             (SETQ B A)
            LAB
             (COND ((NULL B) (RETURN NIL)))
             ((LAMBDA (B)
                (PROG (C)
                  (SETQ C B)
                 LAB
                  (COND ((NULL C) (RETURN NIL)))
                  ((LAMBDA (C) (UNIQUIFYSQ C)) (CAR C))
                  (SETQ C (CDR C))
                  (GO LAB)))
              (CAR B))
             (SETQ B (CDR B))
             (GO LAB)))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB)))) 
(PUT 'UNIQUENESSF 'NUMBER-OF-ARGS 1) 
(PUT 'UNIQUENESSF 'DEFINED-ON-LINE '8559) 
(PUT 'UNIQUENESSF 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'UNIQUENESSF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIQUENESSF (U)
    (PROG ()
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN NIL)))
      (COND
       ((AND (NULL (OR (ATOM U) (ATOM (CAR U)))) (NULL (ATOM (CAAAR U))))
        (COND
         ((NULL (ATSOC (CAAAR U) (GET (CAR (CAAAR U)) 'KLIST)))
          (PROGN
           (PRIN2 "head kernel of ")
           (PRIN2 U)
           (PRIN2 " is not unique!")
           NIL)))))
      (UNIQUENESSF (CDAR U))
      (UNIQUENESSF (CDR U)))) 
(ENDMODULE) 
(MODULE (LIST 'PARSEFORMOUTPUT)) 
(FLUID '(*MSG *INT SEMIC*)) 
(GLOBAL '(CURSYM* NXTSYM*)) 
(PUT 'FORMOUTSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'FORMOUTSTAT 'DEFINED-ON-LINE '8593) 
(PUT 'FORMOUTSTAT 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORMOUTSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FORMOUTSTAT NIL
    (PROG (X Y S *MSG)
      (NEWTOK '((+) FORMOUTPLUS))
      (NEWTOK '((-) FORMOUTMINUS))
      (FLAG '(FORMOUTPLUS) 'DELIM)
      (FLAG '(FORMOUTMINUS) 'DELIM)
      (COND ((EQ NXTSYM* '-) (SCAN)))
      (COND ((EQ CURSYM* 'FORMOUTMINUS) (SETQ S (MINUS 1))) (T (SETQ S 1)))
      (SETQ X (SETQ Y (FORMOUTTERM S (XREAD T))))
      (COND ((EQ CURSYM* '*SEMICOL*) (GO B)))
     A
      (COND ((EQ CURSYM* 'FORMOUTMINUS) (SETQ S (MINUS 1))) (T (SETQ S 1)))
      (PLANTLOWERTERM Y (FORMOUTTERM S (XREAD T)))
      (COND
       ((AND (NULL (OR (ATOM Y) (ATOM (CAR Y)))) (CDR Y)) (SETQ Y (CDR Y))))
      (COND ((NULL (EQ CURSYM* '*SEMICOL*)) (GO A)))
     B
      (REMFLAG '(FORMOUTPLUS) 'DELIM)
      (REMFLAG '(FORMOUTMINUS) 'DELIM)
      (NEWTOK '((+) PLUS))
      (NEWTOK '((-) DIFFERENCE)))) 
(PUT 'FORMOUTPUT 'STAT 'FORMOUTSTAT) 
(PUT 'FORMOUTPUTREAD 'NUMBER-OF-ARGS 1) 
(PUT 'FORMOUTPUTREAD 'DEFINED-ON-LINE '8619) 
(PUT 'FORMOUTPUTREAD 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORMOUTPUTREAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORMOUTPUTREAD (U)
    (PROG (X Y S *MSG ICHAN OLDICHAN *INT SEMIC)
      (SETQ ICHAN (OPEN (MKFIL* U) 'INPUT))
      (SETQ OLDICHAN (RDS ICHAN))
      (NEWTOK '((+) FORMOUTPLUS))
      (NEWTOK '((-) FORMOUTMINUS))
      (FLAG '(FORMOUTPLUS) 'DELIM)
      (FLAG '(FORMOUTMINUS) 'DELIM)
      (SETQ SEMIC SEMIC*)
      (SCAN)
      (COND ((EQ CURSYM* 'FORMOUTMINUS) (PROGN (SETQ S (MINUS 1)) (SCAN)))
            (T (SETQ S 1)))
      (SETQ X (SETQ Y (FORMOUTTERM S (XREAD1 T))))
      (COND ((EQ CURSYM* '*SEMICOL*) (GO B)))
     A
      (COND ((EQ CURSYM* 'FORMOUTMINUS) (SETQ S (MINUS 1))) (T (SETQ S 1)))
      (PLANTLOWERTERM Y (FORMOUTTERM S (XREAD T)))
      (COND
       ((AND (NULL (OR (ATOM Y) (ATOM (CAR Y)))) (CDR Y)) (SETQ Y (CDR Y))))
      (COND ((NULL (EQ CURSYM* '*SEMICOL*)) (GO A)))
     B
      (REMFLAG '(FORMOUTPLUS) 'DELIM)
      (REMFLAG '(FORMOUTMINUS) 'DELIM)
      (NEWTOK '((+) PLUS))
      (NEWTOK '((-) DIFFERENCE))
      (RDS OLDICHAN)
      (CLOSE ICHAN)
      (SETQ SEMIC* SEMIC)
      (RETURN
       (COND ((OR (ATOM X) (ATOM (CAR X))) X)
             (T (MK*SQ (CONS (COND (ALG_POLY X) (T (REORDER X))) 1))))))) 
(PUT 'FORMOUTTERM 'NUMBER-OF-ARGS 2) 
(PUT 'FORMOUTTERM 'DEFINED-ON-LINE '8652) 
(PUT 'FORMOUTTERM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORMOUTTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMOUTTERM (S U)
    (PROG (NUMC)
      (COND
       ((NULL (EQCAR U 'TIMES))
        (RETURN
         (COND ((NUMBERP U) (TIMES U S))
               ((ATOM U) (CONS (CONS (CONS U 1) S) NIL))
               ((EQ (CAR U) 'QUOTIENT)
                (CONS '|:RN:| (CONS (CADR U) (CADDR U))))
               ((EQ (CAR U) 'EXPT)
                (CONS
                 (CONS
                  (CONS
                   (COND ((ATOM (CADR U)) (CADR U)) (T (UNIQUIFYK (CADR U))))
                   (CADDR U))
                  S)
                 NIL))
               (T (CONS (CONS (CONS (UNIQUIFYK U) 1) S) NIL))))))
      (SETQ U (CDR U))
      (SETQ NUMC S)
      (COND
       ((NUMBERP (CAR U))
        (PROGN (SETQ NUMC (TIMES S (CAR U))) (SETQ U (CDR U)))))
      (COND
       ((EQCAR (CAR U) 'QUOTIENT)
        (PROGN
         (SETQ NUMC (CONS '|:RN:| (CONS (TIMES S (CADAR U)) (CADDAR U))))
         (SETQ U (CDR U)))))
      (RETURN (FORMOUTNESTTERM U NUMC)))) 
(PUT 'FORMOUTNESTTERM 'NUMBER-OF-ARGS 2) 
(PUT 'FORMOUTNESTTERM 'DEFINED-ON-LINE '8670) 
(PUT 'FORMOUTNESTTERM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FORMOUTNESTTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMOUTNESTTERM (U NUMC)
    (COND ((NULL U) NUMC)
          ((ATOM (CAR U))
           (CONS (CONS (CONS (CAR U) 1) (FORMOUTNESTTERM (CDR U) NUMC)) NIL))
          ((EQ (CAAR U) 'EXPT)
           (CONS
            (CONS
             (CONS
              (COND ((ATOM (CADAR U)) (CADAR U)) (T (UNIQUIFYK (CADAR U))))
              (CADDAR U))
             (FORMOUTNESTTERM (CDR U) NUMC))
            NIL))
          (T
           (CONS
            (CONS (CONS (UNIQUIFYK (CAR U)) 1) (FORMOUTNESTTERM (CDR U) NUMC))
            NIL)))) 
(PUT 'PLANTLOWERTERM 'NUMBER-OF-ARGS 2) 
(PUT 'PLANTLOWERTERM 'DEFINED-ON-LINE '8678) 
(PUT 'PLANTLOWERTERM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'PLANTLOWERTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLANTLOWERTERM (U V)
    (COND ((OR (ATOM V) (ATOM (CAR V))) (RPLACD U V))
          ((AND (EQ (CAAAR U) (CAAAR V)) (EQUAL (CDAAR U) (CDAAR V)))
           (PROG ()
            A
             (COND ((OR (ATOM V) (ATOM (CAR V))) (GO C)))
             (SETQ V (CDAR V))
             (SETQ U (CDAR U))
             (COND
              ((AND (EQ (CAAAR U) (CAAAR V)) (EQUAL (CDAAR U) (CDAAR V)))
               (GO A)))
            C
             (COND ((NULL (CDR U)) (RETURN (RPLACD U V))))
            B
             (SETQ U (CDR U))
             (GO C)))
          (T (RPLACD U V)))) 
(ENDMODULE) 
(MODULE (LIST 'WRITEFRM)) 
(PUT 'WRITESQFRM 'NUMBER-OF-ARGS 1) 
(PUT 'WRITESQFRM 'DEFINED-ON-LINE '8706) 
(PUT 'WRITESQFRM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITESQFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITESQFRM (U)
    (PROG ()
      (COND ((EQUAL (CDR U) 1) (PROGN (WRITEFRM (CAR U)) (PRIN2T ";")))
            ((NUMBERP (CDR U))
             (COND
              ((CDR (CAR U))
               (PROGN
                (PRIN2 "(")
                (WRITEFRM (CAR U))
                (PRIN2 ")/")
                (WRITEFRM (CDR U))
                (PRIN2T ";")))
              (T
               (PROGN
                (WRITEFRM (CAR U))
                (PRIN2 "/")
                (WRITEFRM (CDR U))
                (PRIN2 ";")))))
            ((OR (NUMBERP (CAR U)) (NULL (CDR (CAR U))))
             (PROGN
              (WRITEFRM (CAR U))
              (PRIN2 "/(")
              (WRITEFRM (CDR U))
              (PRIN2T ");")))
            (T
             (PROGN
              (PRIN2 "(")
              (WRITEFRM (CAR U))
              (PRIN2 ")/(")
              (WRITEFRM (CDR U))
              (PRIN2T ");")))))) 
(PUT 'WRITESFFRM 'NUMBER-OF-ARGS 1) 
(PUT 'WRITESFFRM 'DEFINED-ON-LINE '8722) 
(PUT 'WRITESFFRM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITESFFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITESFFRM (U) (PROG () (PROGN (WRITEFRM U) (PRIN2T ";")))) 
(PUT 'WRITEFRM1 'NUMBER-OF-ARGS 1) 
(PUT 'WRITEFRM1 'DEFINED-ON-LINE '8725) 
(PUT 'WRITEFRM1 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITEFRM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITEFRM1 (U)
    (PROG (Y)
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (RETURN (COND ((EQUAL U 1) (PRIN2 U)) (T (WRITEDOMAIN U))))))
      (COND ((ATOM (CAAAR U)) (PRIN2 (CAAAR U))) (T (WRITEKERN (CAAAR U))))
      (COND ((NOT (EQUAL (CDAAR U) 1)) (PROGN (PRIN2 "^") (PRIN2 (CDAAR U)))))
      (SETQ Y (CDAR U))
      (COND
       ((OR (ATOM Y) (ATOM (CAR Y)))
        (RETURN
         (COND ((EQUAL Y 1) (PRIN2 " "))
               (T (PROGN (PRIN2 " * ") (WRITEDOMAIN Y) (PRIN2 " ")))))))
      (COND ((NULL (CDR Y)) (RETURN (PROGN (PRIN2 " * ") (WRITEFRM1 Y)))))
      (PRIN2 "* (")
     A
      (WRITEFRM1 (CONS (CAR Y) NIL))
      (SETQ Y (CDR Y))
      (COND ((OR (ATOM Y) (ATOM (CAR Y))) (GO B)))
      (COND (Y (PRIN2 " + ")))
      (GO A)
     B
      (COND
       ((AND (NUMBERP Y) (MINUSP Y)) (PROGN (PRIN2 " - ") (SETQ Y (MINUS Y))))
       (Y (PRIN2 " + ")))
      (COND (Y (WRITEFRM1 Y)))
      (COND (Y (PRIN2 ") ")) (T (PRIN2 ")"))))) 
(PUT 'WRITEFRM 'NUMBER-OF-ARGS 1) 
(PUT 'WRITEFRM 'DEFINED-ON-LINE '8749) 
(PUT 'WRITEFRM 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITEFRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITEFRM (U)
    (PROG ()
     A
      (COND ((OR (ATOM U) (ATOM (CAR U))) (GO B)))
      (WRITEFRM1 (CONS (CAR U) NIL))
      (SETQ U (CDR U))
      (COND
       ((AND (NUMBERP U) (MINUSP U)) (PROGN (PRIN2 " - ") (SETQ U (MINUS U))))
       (U (PRIN2 " + ")))
      (GO A)
     B
      (COND (U (PRIN2 U))))) 
(PUT 'WRITEKERN 'NUMBER-OF-ARGS 1) 
(PUT 'WRITEKERN 'DEFINED-ON-LINE '8761) 
(PUT 'WRITEKERN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITEKERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITEKERN (U)
    (PROG ()
      (PRIN2 (CAR U))
      (PRIN2 "(")
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (GO B)))
      (COND ((OR (ATOM (CAR U)) (NUMBERP (CAR U))) (PRIN2 (CAR U)))
            (T (WRITEKERN (CAR U))))
      (COND ((CDR U) (PRIN2 ",")))
      (GO A)
     B
      (PRIN2 ")"))) 
(PUT 'WRITEDOMAIN 'NUMBER-OF-ARGS 1) 
(PUT 'WRITEDOMAIN 'DEFINED-ON-LINE '8774) 
(PUT 'WRITEDOMAIN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'WRITEDOMAIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WRITEDOMAIN (U)
    (PROG ()
      (COND
       ((AND (NUMBERP U) (MINUSP U)) (PROGN (PRIN2 "(") (PRIN2 U) (PRIN2 ")")))
       ((EQCAR U '|:RN:|)
        (PROGN
         (PRIN2 "(")
         (PRIN2 (CADR U))
         (PRIN2 "/")
         (PRIN2 (CDDR U))
         (PRIN2 ")")))
       (T (PRIN2 U))))) 
(ENDMODULE) 
(MODULE (LIST 'CONSISTENCY_CHECKS)) 
(PUT 'CHECK_HISTORY 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_HISTORY 'DEFINED-ON-LINE '8810) 
(PUT 'CHECK_HISTORY 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHECK_HISTORY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_HISTORY (PDES)
    (PROG (P Q H K)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ H (SIMP (GET P 'HISTRY_)))
            (PROG (Q)
              (SETQ Q PDES)
             LAB
              (COND ((NULL Q) (RETURN NIL)))
              ((LAMBDA (Q)
                 (SETQ H
                         (SUBSQ H
                                (LIST (CONS Q (LIST '*SQ (GET Q 'SQVAL) T))))))
               (CAR Q))
              (SETQ Q (CDR Q))
              (GO LAB))
            (COND
             ((NOT (SQZEROP (ADDSQ (GET P 'SQVAL) (NEGSQ H))))
              (PROGN
               (PROGN
                (PRIN2 "The history value of ")
                (PRIN2 P)
                (PRIN2 " is not correct!")
                NIL)
               (SETQ K T)
               (TERPRI))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       ((NULL K)
        (PROGN (PROGN (PRIN2 "History data are consistent.") NIL) (TERPRI)))))) 
(PUT 'CHECK_GLOBALS 'NUMBER-OF-ARGS 0) 
(PUT 'CHECK_GLOBALS 'DEFINED-ON-LINE '8827) 
(PUT 'CHECK_GLOBALS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CHECK_GLOBALS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CHECK_GLOBALS NIL
    (PROG (FLAG VAR)
      (PROG (VAR)
        (SETQ VAR GLOBAL_LIST_INTEGER)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (COND
            ((NOT (FIXP (EVAL VAR)))
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 VAR)
               (PRIN2 " needs to be an integer: ")
               (PRIN2 (EVAL VAR))
               (PRIN2 " is invalid")
               NIL)
              (SETQ FLAG VAR)))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (PROG (VAR)
        (SETQ VAR GLOBAL_LIST_NINTEGER)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (COND
            ((AND (NOT (FIXP (EVAL VAR))) (NEQ (EVAL VAR) NIL))
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 VAR)
               (PRIN2 " needs to be an integer or nil: ")
               (PRIN2 (EVAL VAR))
               (PRIN2 " is invalid")
               NIL)
              (SETQ FLAG VAR)))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (PROG (VAR)
        (SETQ VAR GLOBAL_LIST_FLOAT)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (COND
            ((NOT (NUMBERP (EVAL VAR)))
             (PROGN
              (TERPRI)
              (PROGN
               (PRIN2 VAR)
               (PRIN2 " needs to be a number: ")
               (PRIN2 (EVAL VAR))
               (PRIN2 " is invalid")
               NIL)
              (SETQ FLAG VAR)))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (RETURN FLAG))) 
(PUT 'INTERNTEST 'NUMBER-OF-ARGS 2) 
(PUT 'INTERNTEST 'DEFINED-ON-LINE '8861) 
(PUT 'INTERNTEST 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'INTERNTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTERNTEST (PDES FORG)
    (PROG (A B C)
      (PROG (A)
        (SETQ A PDES)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROGN
            (UNIQUENESSSQ (GET A 'SQVAL))
            (COND
             ((PAIRP (GET A 'FAC))
              (PROG (B)
                (SETQ B (GET A 'FAC))
               LAB
                (COND ((NULL B) (RETURN NIL)))
                ((LAMBDA (B) (UNIQUENESSSQ B)) (CAR B))
                (SETQ B (CDR B))
                (GO LAB))))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_LIN))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_NCA))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCTEVAL_NLI))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_LIN))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NCA))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NLI))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))
            (PROG (B)
              (SETQ B (GET A 'FCT_NLI_NUS))
             LAB
              (COND ((NULL B) (RETURN NIL)))
              ((LAMBDA (B) (UNIQUENESSSQ (CAR B))) (CAR B))
              (SETQ B (CDR B))
              (GO LAB))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A FORG)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((AND (PAIRP A) (EQUAL (CAR A) 'EQUAL)) (UNIQUENESSSQ (CADDR A)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A INEQ_)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (UNIQUENESSSQ A)) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (PROG (A)
        (SETQ A INEQ_OR)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (PROG (B)
             (SETQ B A)
            LAB
             (COND ((NULL B) (RETURN NIL)))
             ((LAMBDA (B)
                (PROG (C)
                  (SETQ C B)
                 LAB
                  (COND ((NULL C) (RETURN NIL)))
                  ((LAMBDA (C) (UNIQUENESSSQ C)) (CAR C))
                  (SETQ C (CDR C))
                  (GO LAB)))
              (CAR B))
             (SETQ B (CDR B))
             (GO LAB)))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB)))) 
(ENDMODULE) 
(MODULE (LIST 'TREEOFCASES)) 
(PUT 'LIST_CURRENT_CASE_ASSUMPTIONS 'NUMBER-OF-ARGS 0) 
(PUT 'LIST_CURRENT_CASE_ASSUMPTIONS 'DEFINED-ON-LINE '8893) 
(PUT 'LIST_CURRENT_CASE_ASSUMPTIONS 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'LIST_CURRENT_CASE_ASSUMPTIONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIST_CURRENT_CASE_ASSUMPTIONS NIL
    (COND
     ((NULL KEEP_CASE_TREE)
      (PROGN
       (PRIN2 "To list all case assumptions the ")
       (PRIN2 " computation had to be started with keep_case_tree:=t .")
       NIL))
     ((NULL SESSION_)
      (PROGN
       (PRIN2 "Either there have no case distinctions been")
       (PRIN2 " made yet or the current computation is a side computation for")
       (PRIN2 "  which case assumptions are not stored in a case tree")
       NIL))
     ((NULL LEVEL_)
      (PROGN (PRIN2 "There have no case distinctions been made yet") NIL))
     (T
      (PROG (LV CT CTF ECHO_BAK SEMIC_BAK NAT_BAK)
        (SETQ LV (REVERSE LEVEL_))
        (SETQ CTF (EXPLODE SESSION_))
        (SETQ CTF
                (BLDMSG_INTERNAL "%w"
                                 (LIST
                                  (COMPRESS
                                   (CONS (CAR CTF)
                                         (CONS 'C (CONS 'T (CDDDR CTF))))))))
        (COND
         ((NULL (FILEP CTF))
          (RETURN
           (PROGN (PROGN (PRIN2 "There is no file ") (PRIN2 CTF) NIL) NIL))))
        (SETQ ECHO_BAK *ECHO)
        (SETQ SEMIC_BAK SEMIC*)
        (SETQ SEMIC* '$)
        (IN (LIST CTF))
        (SETQ *ECHO ECHO_BAK)
        (SETQ SEMIC* SEMIC_BAK)
        (SETQ CT BACKUP_)
        (SETQ BACKUP_ NIL)
        (SETQ NAT_BAK *NAT)
        (OFF (LIST 'NAT))
        (PROG ()
         WHILELABEL
          (COND ((NOT LV) (RETURN NIL)))
          (PROGN
           (SETQ CT (CDDDR CT))
           (PROG ()
            WHILELABEL
             (COND
              ((NOT (AND (CDR CT) (NEQ (CAADR CT) (CAR LV)))) (RETURN NIL)))
             (SETQ CT (CDR CT))
             (GO WHILELABEL))
           (COND
            ((NULL (CDR CT))
             (PROGN
              (PROGN
               (PRIN2 "### ERROR in CaseTree: case not found in ct, lv=")
               (PRIN2 LV)
               NIL)
              (TERPRI)))
            (T
             (PROGN
              (TERPRI)
              (COND
               ((NULL (CADADR CT))
                (PROGN
                 (PROGN (PRIN2 "0 <> ") NIL)
                 (MATHPRINT (CAR (CDDADR CT)))))
               (T (PROGN (PROGN (PRIN2 "0 =  ") NIL) (MATHPRINT (CADADR CT)))))
              (SETQ CT (CADR CT))
              (SETQ LV (CDR LV))))))
          (GO WHILELABEL))
        (COND ((NEQ *NAT NAT_BAK) (ON (LIST 'NAT)))))))) 
(PUT 'CONSISTENTTREE 'NUMBER-OF-ARGS 2) 
(PUT 'CONSISTENTTREE 'DEFINED-ON-LINE '8960) 
(PUT 'CONSISTENTTREE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CONSISTENTTREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONSISTENTTREE (CT LV)
    (COND
     ((AND CT (CDDDDR CT))
      (COND
       ((ZEROP (CADDDR CT))
        (PROGN
         (PRIN2 "### ERROR in CaseTree: Case ")
         (PRIN2 (APPEND LV (LIST (CAR CT))))
         (PRIN2 " has not started")
         (PRIN2 " but has already sub-cases!")
         NIL))
       (T
        (PROG (CTC UN)
          (SETQ CTC (CDDDDR CT))
          (SETQ LV (APPEND LV (LIST (CAR CT))))
          (PROG ()
           WHILELABEL
            (COND ((NOT CTC) (RETURN NIL)))
            (PROGN
             (CONSISTENTTREE (CAR CTC) LV)
             (COND ((LESSP (CADDDR (CAR CTC)) 2) (SETQ UN T)))
             (SETQ CTC (CDR CTC)))
            (GO WHILELABEL))
          (COND
           ((AND (GREATERP (CADDDR CT) 1) UN)
            (PROGN
             (PRIN2 "### ERROR in CaseTree: Case ")
             (PRIN2 LV)
             (PRIN2 " is completed")
             (PRIN2 " but not all subcases are completed!")
             NIL))))))))) 
(PUT 'CASETREE 'NUMBER-OF-ARGS 1) 
(PUT 'CASETREE 'DEFINED-ON-LINE '8991) 
(PUT 'CASETREE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CASETREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASETREE (INP)
    (COND
     ((AND SESSION_ KEEP_CASE_TREE)
      (PROG (LV CT CTC CTF ECHO_BAK SEMIC_BAK FL FPID NEWSPLIT NEWFILE MAXTRIES
             A SAVE OFL*BAK)
        (COND
         ((AND (PAIRP INP) (EQUAL (CAR LEVEL_) 1))
          (PROGN (SETQ NEWSPLIT T) (SETQ LV (REVERSE (CDR LEVEL_))) NIL))
         (T (SETQ LV (REVERSE LEVEL_))))
        (SETQ CTF (EXPLODE SESSION_))
        (SETQ CTF
                (BLDMSG_INTERNAL "%w"
                                 (LIST
                                  (COMPRESS
                                   (CONS (CAR CTF)
                                         (CONS 'C (CONS 'T (CDDDR CTF))))))))
        (COND
         ((AND (NULL LV) (NULL (FILEP CTF)))
          (PROGN (SETQ NEWFILE T) (SETQ CT (LIST NIL NIL NIL 1))))
         (T
          (PROGN
           (SETQ FPID (BLDMSG_INTERNAL "%s.%w" (LIST CTF (GETPID))))
           (SETQ MAXTRIES 0)
           (PROG ()
            REPEATLABEL
             (PROGN
              (SETQ FL (RENAME-FILE CTF FPID))
              (SETQ MAXTRIES (ADD1 MAXTRIES))
              (COND ((NULL FL) (SLEEP 0.5))))
             (COND ((NOT (OR FL (EQUAL MAXTRIES 5))) (GO REPEATLABEL))))
           (COND
            ((EQUAL MAXTRIES 5)
             (RETURN
              (PROGN
               (PROGN
                (PRIN2 "### ERROR in CaseTree: file ")
                (PRIN2 CTF)
                (PRIN2 " not found.")
                NIL)
               (TERPRI)
               (PROGN (PRIN2 "--> No more tries. (keep_case_tree:=nil)") NIL)
               (TERPRI)
               NIL))))
           (SETQ ECHO_BAK *ECHO)
           (SETQ SEMIC_BAK SEMIC*)
           (SETQ SEMIC* '$)
           (IN (LIST FPID))
           (SETQ *ECHO ECHO_BAK)
           (SETQ SEMIC* SEMIC_BAK)
           (SETQ CT BACKUP_)
           (SETQ BACKUP_ NIL)
           NIL)))
        (SETQ CTC CT)
        (PROG ()
         WHILELABEL
          (COND ((NOT LV) (RETURN NIL)))
          (PROGN
           (SETQ CTC (CDDDR CTC))
           (PROG ()
            WHILELABEL
             (COND
              ((NOT (AND (CDR CTC) (NEQ (CAADR CTC) (CAR LV)))) (RETURN NIL)))
             (SETQ CTC (CDR CTC))
             (GO WHILELABEL))
           (COND
            ((NULL (CDR CTC))
             (PROGN
              (PROGN
               (PRIN2 "### ERROR in CaseTree: case not found in ct, lv=")
               (PRIN2 LV)
               NIL)
              (TERPRI)))
            (T (PROGN (SETQ CTC (CADR CTC)) (SETQ LV (CDR LV))))))
          (GO WHILELABEL))
        (SETQ CTC (CDDDR CTC))
        (COND
         ((AND (PAIRP INP) (CDR CTC))
          (PROGN
           (PROGN (PRIN2 "### ERROR in CaseTree: lv=nil, cdr ctc=") NIL)
           (EQPRINT (CDR CTC))
           (TERPRI)))
         (NEWSPLIT
          (COND
           ((EQUAL (CAAR INP) 'EQUAL)
            (COND
             ((ZEROP (CADAR INP))
              (RPLACD CTC
                      (LIST (LIST 1 (CADDAR INP) NIL 1)
                            (LIST 2 NIL (CADDAR INP) 0))))
             (T
              (RPLACD CTC
                      (LIST (LIST 1 (CADAR INP) NIL 1)
                            (LIST 2 NIL (CADAR INP) 0))))))
           ((ZEROP (CADAR INP))
            (RPLACD CTC
                    (LIST (LIST 1 NIL (CADDAR INP) 1)
                          (LIST 2 (CADDAR INP) NIL 0))))
           (T
            (RPLACD CTC
                    (LIST (LIST 1 NIL (CADAR INP) 1)
                          (LIST 2 (CADAR INP) NIL 0))))))
         ((PAIRP INP) (RPLACA CTC 1))
         (T
          (PROGN
           (COND ((NULL INP) (RPLACA CTC 2)) (T (RPLACA CTC (PLUS 2 INP))))
           NIL)))
        (CONSISTENTTREE CT NIL)
        (COND
         (NEWFILE
          (PROGN
           (SETQ A (OPEN CTF 'OUTPUT))
           (SETQ OFL*BAK OFL*)
           (SETQ OFL* CTF)
           (SETQ SAVE (WRS A))
           (PROGN (PRIN2 "off echo$ ") NIL)
           (PROGN (PRIN2 "backup_:= '") NIL)
           (PRINT CT)
           (PROGN (PRIN2 " $") NIL)
           (TERPRI)
           (PROGN (PRIN2 "end$") NIL)
           (TERPRI)
           (WRS SAVE)
           (SETQ OFL* OFL*BAK)
           (CLOSE A)
           NIL))
         (T
          (PROGN
           (SETQ A (OPEN FPID 'OUTPUT))
           (SETQ OFL*BAK OFL*)
           (SETQ OFL* FPID)
           (SETQ SAVE (WRS A))
           (PROGN (PRIN2 "off echo$ ") NIL)
           (PROGN (PRIN2 "backup_:= '") NIL)
           (PRINT CT)
           (PROGN (PRIN2 " $") NIL)
           (TERPRI)
           (PROGN (PRIN2 "end$") NIL)
           (TERPRI)
           (WRS SAVE)
           (SETQ OFL* OFL*BAK)
           (CLOSE A)
           (SETQ MAXTRIES 0)
           (PROG ()
            REPEATLABEL
             (PROGN
              (SETQ FL (RENAME-FILE FPID CTF))
              (SETQ MAXTRIES (ADD1 MAXTRIES))
              (COND ((NULL FL) (SLEEP 0.5))))
             (COND ((NOT (OR FL (EQUAL MAXTRIES 5))) (GO REPEATLABEL))))
           NIL))))))) 
(PUT 'FIND_UNSOLVED_CASE 'NUMBER-OF-ARGS 0) 
(PUT 'FIND_UNSOLVED_CASE 'DEFINED-ON-LINE '9159) 
(PUT 'FIND_UNSOLVED_CASE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'FIND_UNSOLVED_CASE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIND_UNSOLVED_CASE NIL
    (PROG (CTF FPID FL CT CTC CTCC SOLN CONDI ECHO_BAK SEMIC_BAK MAXTRIES A
           SAVE OFL*BAK)
      (SETQ CTF (EXPLODE SESSION_))
      (SETQ CTF
              (BLDMSG_INTERNAL "%w"
                               (LIST
                                (COMPRESS
                                 (CONS (CAR CTF)
                                       (CONS 'C (CONS 'T (CDDDR CTF))))))))
      (SETQ FPID (BLDMSG_INTERNAL "%s.%w" (LIST CTF (GETPID))))
      (SETQ MAXTRIES 0)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ FL (RENAME-FILE CTF FPID))
         (SETQ MAXTRIES (ADD1 MAXTRIES))
         (COND ((NULL FL) (SLEEP 1))))
        (COND ((NOT (OR FL (EQUAL MAXTRIES 5))) (GO REPEATLABEL))))
      (COND
       ((EQUAL MAXTRIES 5)
        (RETURN
         (PROGN
          (PROGN
           (PRIN2 "### ERROR in CaseTree: file ")
           (PRIN2 CTF)
           (PRIN2 " not found")
           NIL)
          NIL))))
      (SETQ ECHO_BAK *ECHO)
      (SETQ SEMIC_BAK SEMIC*)
      (SETQ SEMIC* '$)
      (IN (LIST FPID))
      (SETQ *ECHO ECHO_BAK)
      (SETQ SEMIC* SEMIC_BAK)
      (SETQ CT BACKUP_)
      (SETQ BACKUP_ NIL)
      (COND ((GREATERP (CADDDR CT) 1) (GO FINO)))
     AGAIN1
      (SETQ LEVEL_ NIL)
      (SETQ CTC CT)
     AGAIN2
      (COND
       ((CDDDDR CTC)
        (PROGN
         (SETQ CTCC (CDDDDR CTC))
         (SETQ SOLN 0)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND CTCC (GREATERP (CADDDR (CAR CTCC)) 1))) (RETURN NIL)))
           (PROGN
            (SETQ SOLN (PLUS SOLN (DIFFERENCE (CADDDR (CAR CTCC)) 2)))
            (SETQ CTCC (CDR CTCC)))
           (GO WHILELABEL))
         (COND
          ((NULL CTCC)
           (PROGN
            (SYSTEM (BLDMSG_INTERNAL "rm %w" (LIST (LEVEL_STRING SESSION_))))
            (COND ((EQUAL CTC CT) (GO FINO))
                  (T (PROGN (RPLACA (CDDDR CTC) (PLUS SOLN 2)) (GO AGAIN1))))))
          (T
           (PROGN
            (SETQ CTC (CAR CTCC))
            (SETQ LEVEL_ (CONS (CAR CTC) LEVEL_))
            (GO AGAIN2)))))))
      (SETQ CONDI
              (COND ((CADR CTC) (LIST 'EQUAL (CADR CTC) 0))
                    (T (LIST 'NEQ (CADDR CTC) 0))))
      (COND ((EQUAL (CADDDR CTC) 0) (RPLACA (CDDDR CTC) 1)))
      (CONSISTENTTREE CT NIL)
      (SETQ A (OPEN FPID 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* FPID)
      (SETQ SAVE (WRS A))
      (PROGN (PRIN2 "off echo$ ") NIL)
      (PROGN (PRIN2 "backup_:= '") NIL)
      (PRINT CT)
      (PROGN (PRIN2 " $") NIL)
      (TERPRI)
      (PROGN (PRIN2 "end$") NIL)
      (TERPRI)
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
     FINO
      (PROG ()
       REPEATLABEL
        (PROGN (SETQ FL (RENAME-FILE FPID CTF)) (COND ((NULL FL) (SLEEP 1))))
        (COND ((NOT FL) (GO REPEATLABEL))))
      (RETURN CONDI))) 
(FLAG '(CRACKPICKUP) 'OPFN) 
(PUT 'CRACKPICKUP 'NUMBER-OF-ARGS 0) 
(PUT 'CRACKPICKUP 'DEFINED-ON-LINE '9268) 
(PUT 'CRACKPICKUP 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CRACKPICKUP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CRACKPICKUP NIL
    (PROG (S LEVEL_BAK LEVSTRI)
      (TERPRI)
      (SETQ OLD_HISTORY NIL)
      (COND ((NULL SESSION_) (ASK_FOR_SESSION))
            (T
             (PROGN
              (PROGN
               (PRIN2
                "Do you want to compute remaining cases left over in this session,")
               NIL)
              (TERPRI)
              (COND
               ((NOT (YESP "i.e. since loading CRACK the last time? "))
                (ASK_FOR_SESSION)))
              NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ S (FIND_UNSOLVED_CASE))) (RETURN NIL)))
        (PROGN
         (SETQ LEVEL_BAK LEVEL_)
         (SETQ LEVEL_ (CDR LEVEL_))
         (PROGN
          (PRIN2 "Computation of the case ")
          (PRIN2 (REVERSE LEVEL_BAK))
          NIL)
         (TERPRI)
         (SETQ LEVSTRI (LEVEL_STRING SESSION_))
         (SETQ OLD_HISTORY
                 (COND
                  ((EQUAL (CAR S) 'NEQ)
                   (LIST 'RB LEVSTRI 'AS 'LEVEL_ (LIST 'QUOTE LEVEL_BAK) 'N
                         (CADR S)))
                  (T
                   (LIST 'RB LEVSTRI 'AS 'LEVEL_ (LIST 'QUOTE LEVEL_BAK) 'R 'N
                         'NEW_PDE (CADR S) 2))))
         (AEVAL* (OFF (LIST 'BATCH_MODE)))
         (AEVAL*
          (LIST 'CRACK (LIST 'LIST) (LIST 'LIST) (LIST 'LIST) (LIST 'LIST)))
         NIL)
        (GO WHILELABEL)))) 
(PUT 'DELETE_CASE_TREE 'NUMBER-OF-ARGS 0) 
(PUT 'DELETE_CASE_TREE 'DEFINED-ON-LINE '9303) 
(PUT 'DELETE_CASE_TREE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'DELETE_CASE_TREE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DELETE_CASE_TREE NIL
    (PROG (CTF)
      (SETQ CTF (EXPLODE SESSION_))
      (SETQ CTF
              (BLDMSG_INTERNAL "%w"
                               (LIST
                                (COMPRESS
                                 (CONS (CAR CTF)
                                       (CONS 'C (CONS 'T (CDDDR CTF))))))))
      (COND ((FILEP CTF) (DELETE-FILE-EXACT CTF))))) 
(ENDMODULE) 
(MODULE (LIST 'LET_RULE_HANDLING)) 
(PUT 'COPYRULE2EQN 'NUMBER-OF-ARGS 2) 
(PUT 'COPYRULE2EQN 'DEFINED-ON-LINE '9318) 
(PUT 'COPYRULE2EQN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'COPYRULE2EQN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COPYRULE2EQN (H PDES)
    (PROG (L)
      (SETQ L
              (MKEQSQ (SIMP* (LIST 'DIFFERENCE (CADR H) (CADDR H))) NIL NIL
               FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL PDES))
      (SETQ PDES (EQINSERT L PDES))
      (RETURN PDES))) 
(PUT 'MOVERULE2EQN 'NUMBER-OF-ARGS 2) 
(PUT 'MOVERULE2EQN 'DEFINED-ON-LINE '9327) 
(PUT 'MOVERULE2EQN 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'MOVERULE2EQN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOVERULE2EQN (H PDES)
    (PROGN
     (SETQ USERRULES_ (DELETE H USERRULES_))
     (SETQ PDES (COPYRULE2EQN H PDES))
     (AEVAL (CLEARRULES (LIST (LIST 'LIST H))))
     PDES)) 
(PUT 'ADD_A_RULE 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_A_RULE 'DEFINED-ON-LINE '9336) 
(PUT 'ADD_A_RULE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'ADD_A_RULE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_A_RULE (PDES FORG)
    (PROG (L S H PL DNR)
      (CHANGE_PROMPT_TO "")
      (PROGN
       (PRIN2 "In the LET-rule you are going to add you can not introduce ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "new functions to be computed. If your LET-rules involve ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "such functions then you have to add equations before which ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "involve these functions in order to introduce the functions ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "to the program. ") NIL)
      (TERPRI)
      (TERPRI)
      (PROGN (PRIN2 "You can either") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "- give the name (terminated by ;) of a rule list to be ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "  activated that has been defined before the call of CRACK, or")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "- give the name (terminated by ;) of an equation which ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "  is to be converted to a LET rule, or") NIL)
      (TERPRI)
      (PROGN (PRIN2 "- type in the new LET-rule in the form like") NIL)
      (TERPRI)
      (PROGN (PRIN2 "  sqrt(e)**(-~x*log(~y)/~z) => y**(-x/z/2);   : ") NIL)
      (TERPRI)
      (SETQ L (TERMXREAD))
      (COND
       ((ATOM L)
        (COND ((MEMBER L PDES) (PROGN (SETQ PL L) (RULE_FROM_PDE L)))
              (T (PROGN (SETQ PL NIL) (AEVAL (LET (LIST L)))))))
       (T
        (PROGN
         (SETQ USERRULES_ (CONS 'LIST (CONS L (CDR USERRULES_))))
         (PROGN
          (ASSGNPRI (AEVAL "The new list of user defined rules: ") NIL 'FIRST)
          (ASSGNPRI (AEVAL USERRULES_) NIL 'LAST))
         (TERPRI)
         NIL)))
      (PROGN
       (PRIN2 "Shall all current LET-rules be applied to all current ")
       (PRIN2 "equations NOW (y/n)? ")
       NIL)
      (SETQ L (TERMREAD))
      (COND
       ((OR (EQUAL L 'Y) (EQUAL L 'Y))
        (PROGN
         (AEVAL (LET (LIST USERRULES_)))
         (COND
          ((NULL PL)
           (PROGN
            (PROGN
             (PRIN2
              "Give an equation name to which the LET-rule should not be applied ")
             (PRIN2
              "now or press ENTER if the rule should be applied to all equations: ")
             NIL)
            (SETQ PL (TERMREAD))
            NIL)))
         (SETQ S PDES)
         (PROG (H)
           (SETQ H S)
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND
               ((AND (NEQ H PL) (NULL CONTRADICTION_))
                (PROGN
                 (SETQ L
                         (MKEQSQ (GET H 'SQVAL) NIL NIL (GET H 'FCTS)
                          (GET H 'VARS) ALLFLAGS_ T (LIST 0) NIL PDES))
                 (COND
                  ((AND L (NEQ (GET H 'SQVAL) (GET L 'SQVAL)))
                   (PROGN
                    (SETQ PDES (DROP_PDE H PDES NIL))
                    (SETQ PDES (EQINSERT L PDES)))))))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         (AEVAL (CLEARRULES (LIST USERRULES_)))
         (PROG (H)
           (SETQ H (CDR USERRULES_))
          LAB
           (COND ((NULL H) (RETURN NIL)))
           ((LAMBDA (H)
              (COND ((NULL CONTRADICTION_) (SETQ PDES (COPYRULE2EQN H PDES)))))
            (CAR H))
           (SETQ H (CDR H))
           (GO LAB))
         NIL)))
      (PROGN
       (PRIN2 "Shall all current LET-rules be applied to simplify all ")
       (PRIN2 "computed functions/constants (forg) NOW (y/n)? ")
       NIL)
      (SETQ L (TERMREAD))
      (COND
       ((OR (EQUAL L 'Y) (EQUAL L 'Y))
        (PROGN
         (AEVAL (LET (LIST USERRULES_)))
         (SETQ FORG
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H FORG)
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (H)
                                       (COND ((ATOM H) H)
                                             ((EQUAL (CAR H) 'EQUAL)
                                              (PROGN
                                               (SETQ DNR
                                                       (SIMP*
                                                        (LIST '*SQ
                                                              (CONS
                                                               (CDR (CADDR H))
                                                               1)
                                                              NIL)))
                                               (COND
                                                ((SQZEROP DNR)
                                                 (PROGN
                                                  (SETQ CONTRADICTION_ T)
                                                  (TERPRI)
                                                  (PROGN
                                                   (PRIN2
                                                    "##### ERROR: When applying LET rules in the denominator of the ")
                                                   (PRIN2 "forg entry: ")
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
                                                                       (CADDR
                                                                        H)
                                                                       NIL))))
                                                  (PUT (CADR H) 'FCTS
                                                       (SORT_ACCORDING_TO
                                                        (SMEMBERL FTEM_
                                                         (CADDR H))
                                                        FTEM_))
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
                               (COND ((ATOM H) H)
                                     ((EQUAL (CAR H) 'EQUAL)
                                      (PROGN
                                       (SETQ DNR
                                               (SIMP*
                                                (LIST '*SQ
                                                      (CONS (CDR (CADDR H)) 1)
                                                      NIL)))
                                       (COND
                                        ((SQZEROP DNR)
                                         (PROGN
                                          (SETQ CONTRADICTION_ T)
                                          (TERPRI)
                                          (PROGN
                                           (PRIN2
                                            "##### ERROR: When applying LET rules in the denominator of the ")
                                           (PRIN2 "forg entry: ")
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
                                                         (LIST '*SQ (CADDR H)
                                                               NIL))))
                                          (PUT (CADR H) 'FCTS
                                               (SORT_ACCORDING_TO
                                                (SMEMBERL FTEM_ (CADDR H))
                                                FTEM_))
                                          H)))))
                                     (T H)))
                             (CAR H))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (AEVAL (CLEARRULES (LIST USERRULES_)))
         NIL)))
      (TERPRI)
      (PROGN (PRIN2 "Warning: Changes of equations based on LET-rules") NIL)
      (TERPRI)
      (PROGN (PRIN2 "are not recorded in the history of equations.") NIL)
      (TERPRI)
      (RETURN (LIST PDES FORG)))) 
(PUT 'CLEAR_A_RULE 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAR_A_RULE 'DEFINED-ON-LINE '9426) 
(PUT 'CLEAR_A_RULE 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'CLEAR_A_RULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAR_A_RULE (PDES)
    (PROG (L S)
      (CHANGE_PROMPT_TO "")
      (PROGN (PRIN2 "These are all the user defined rules: ") NIL)
      (TERPRI)
      (ASSGNPRI (AEVAL USERRULES_) NIL 'ONLY)
      (PROGN (PRIN2 "You can either") NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "- give the number of a rule above to be dropped, or ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "- give the name of a rule list to be disabled that was ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "  activated already before the call of CRACK, or ") NIL)
      (TERPRI)
      (PROGN (PRIN2 "- enter 0 to return to menu: ") NIL)
      (SETQ L (TERMREAD))
      (COND
       ((NEQ L 0)
        (COND
         ((NOT (FIXP L))
          (PROGN
           (AEVAL (CLEARRULES (LIST L)))
           (PROGN
            (PRIN2 "Rule list ")
            (PRIN2 L)
            (PRIN2 " has been disabled.")
            NIL)
           (TERPRI)))
         ((GREATERP L (SUB1 (LENGTH USERRULES_)))
          (PROGN (PROGN (PRIN2 "This number is too big.") NIL) (TERPRI)))
         (T
          (PROGN
           (SETQ S (CDR USERRULES_))
           (PROG ()
            WHILELABEL
             (COND ((NOT (GREATERP L 1)) (RETURN NIL)))
             (PROGN (SETQ L (SUB1 L)) (SETQ S (CDR S)))
             (GO WHILELABEL))
           (PROGN
            (PRIN2
             "Apart from being copied as an equation, should it also be deleted as
  rule? (Y/N) ")
            NIL)
           (SETQ L (TERMREAD))
           (PROG ()
            REPEATLABEL
             (SETQ L (TERMREAD))
             (COND ((NOT (OR (EQUAL L 'Y) (EQUAL L 'N))) (GO REPEATLABEL))))
           (SETQ PDES
                   (COND ((EQUAL L 'Y) (MOVERULE2EQN (CAR S) PDES))
                         (T (COPYRULE2EQN (CAR S) PDES))))
           (ASSGNPRI (AEVAL USERRULES_) NIL 'ONLY)
           (TERPRI)
           NIL)))))
      (RETURN PDES))) 
(PUT 'SS_MODULO 'NUMBER-OF-ARGS 0) 
(PUT 'SS_MODULO 'DEFINED-ON-LINE '9459) 
(PUT 'SS_MODULO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'SS_MODULO 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SS_MODULO NIL
    (PROG (L)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (PROGN
         (PROGN
          (PRIN2
           "Enter a number modulo which computations shall be performed, like 65537.")
          NIL)
         (PROGN
          (PRIN2
           "If the number is not a prime number then the next prime number is taken:  ")
          NIL)
         (SETQ L (TERMREAD)))
        (COND ((NOT (AND (FIXP L) (GREATERP L 1))) (GO REPEATLABEL))))
      (SETQ MODULAR_COMP (NEXTPRIME (DIFFERENCE L 1)))
      (SETMOD MODULAR_COMP))) 
(PUT 'START_STOP_MODULO 'NUMBER-OF-ARGS 0) 
(PUT 'START_STOP_MODULO 'DEFINED-ON-LINE '9471) 
(PUT 'START_STOP_MODULO 'DEFINED-IN-FILE 'CRACK/CRUTIL.RED) 
(PUT 'START_STOP_MODULO 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE START_STOP_MODULO NIL
    (PROG (L)
      (CHANGE_PROMPT_TO "")
      (COND
       (MODULAR_COMP
        (PROGN
         (SETQ L (SETMOD 1))
         (SETMOD L)
         (COND
          ((NEQ L MODULAR_COMP)
           (PROGN
            (PROGN
             (PRIN2 "### WARNING: The setmod value ")
             (PRIN2 L)
             (PRIN2 " is not equal modular_comp=")
             (PRIN2 MODULAR_COMP)
             (PRIN2 "!")
             NIL)
            (TERPRI))))
         (PROG ()
          REPEATLABEL
           (PROGN
            (PROGN
             (PRIN2 "Currently computations are done modulo ")
             (PRIN2 L)
             NIL)
            (TERPRI)
            (PROGN (PRIN2 "To change this number        enter c ") NIL)
            (TERPRI)
            (PROGN (PRIN2 "To stop modular computations enter p ") NIL)
            (TERPRI)
            (PROGN (PRIN2 "To return to menu            enter 0 : ") NIL)
            (SETQ L (TERMREAD)))
           (COND
            ((NOT (OR (EQUAL L 'P) (EQUAL L 'C) (EQUAL L 0)))
             (GO REPEATLABEL))))
         (COND ((EQUAL L 'C) (SS_MODULO))
               ((EQUAL L 'P) (SETQ MODULAR_COMP NIL)))))
       (T
        (PROGN
         (PROG ()
          REPEATLABEL
           (PROGN
            (PROGN
             (PRIN2 "To start computation modular a number enter t ")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "To return to menu enter                     0 ")
             NIL)
            (SETQ L (TERMREAD)))
           (COND ((NOT (OR (EQUAL L 'T) (EQUAL L 0))) (GO REPEATLABEL))))
         (COND ((EQUAL L 'T) (SS_MODULO)))))))) 
(ENDMODULE) 