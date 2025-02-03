(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DECOUPLING)) 
(FLUID '(FORM_CHNPAIR)) 
(PUT 'HIGH_PRIO_DECOUPLING 'NUMBER-OF-ARGS 1) 
(PUT 'HIGH_PRIO_DECOUPLING 'DEFINED-ON-LINE '34) 
(PUT 'HIGH_PRIO_DECOUPLING 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'HIGH_PRIO_DECOUPLING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HIGH_PRIO_DECOUPLING (ARGLIST)
    (PROG (L)
      (SETQ L
              (DEC_ONE_STEP (CAR ARGLIST) (CAR ARGLIST) FTEM_ (CADDR ARGLIST) T
               0))
      (COND (L (SETQ L (LIST L (CADR ARGLIST)))))
      (RETURN L))) 
(PUT 'DECOUPLING 'NUMBER-OF-ARGS 1) 
(PUT 'DECOUPLING 'DEFINED-ON-LINE '44) 
(PUT 'DECOUPLING 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DECOUPLING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOUPLING (ARGLIST)
    (PROG (L)
      (SETQ L
              (DEC_ONE_STEP (CAR ARGLIST) (CAR ARGLIST) FTEM_ (CADDR ARGLIST)
               NIL 0))
      (COND (L (SETQ L (LIST L (CADR ARGLIST)))))
      (RETURN L))) 
(PUT 'INHOM_DECOUPLING 'NUMBER-OF-ARGS 1) 
(PUT 'INHOM_DECOUPLING 'DEFINED-ON-LINE '54) 
(PUT 'INHOM_DECOUPLING 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'INHOM_DECOUPLING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INHOM_DECOUPLING (ARGLIST)
    (COND
     (FHOM_
      (PROG (P P1)
        (PROG (P)
          (SETQ P (CAR ARGLIST))
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (COND ((FREEOFLIST (GET P 'FCTS) FHOM_) (SETQ P1 (CONS P P1)))))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (COND
         ((AND P1 (CDR P1))
          (RETURN
           (PROGN
            (SETQ P1
                    (DEC_ONE_STEP (CAR ARGLIST) (REVERSE P1) FTEM_
                     (CADDR ARGLIST) NIL 0))
            (COND (P1 (LIST P1 (CADR ARGLIST))) (T NIL)))))))))) 
(PUT 'WHICH_DERIV 'NUMBER-OF-ARGS 2) 
(PUT 'WHICH_DERIV 'DEFINED-ON-LINE '70) 
(PUT 'WHICH_DERIV 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'WHICH_DERIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WHICH_DERIV (P Q)
    (PROG (L N A)
      (PROG ()
       WHILELABEL
        (COND ((NOT Q) (RETURN NIL)))
        (COND
         ((SETQ A (MEMBER (CAR Q) P))
          (PROGN
           (SETQ Q (CDR Q))
           (COND
            ((AND Q (NUMBERP (CAR Q)))
             (PROGN (SETQ N (CAR Q)) (SETQ Q (CDR Q))))
            (T (SETQ N 1)))
           (SETQ N
                   (DIFFERENCE N
                               (COND
                                ((AND (PAIRP (CDR A)) (NUMBERP (CADR A)))
                                 (CADR A))
                                (T 1))))
           (COND
            ((GREATERP N 0)
             (PROGN
              (SETQ L (CONS (CAR A) L))
              (COND ((GREATERP N 1) (SETQ L (CONS N L)))))))))
         (T
          (PROGN
           (SETQ L (CONS (CAR Q) L))
           (SETQ Q (CDR Q))
           (COND
            ((AND Q (NUMBERP (CAR Q)))
             (PROGN (SETQ L (CONS (CAR Q) L)) (SETQ Q (CDR Q))))))))
        (GO WHILELABEL))
      (RETURN (APPEND (REVERSE L) Q)))) 
(PUT 'DEC_LD_INFO 'NUMBER-OF-ARGS 7) 
(PUT 'DEC_LD_INFO 'DEFINED-ON-LINE '98) 
(PUT 'DEC_LD_INFO 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_LD_INFO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE DEC_LD_INFO (P Q SIMPP SIMPQ F VL RL)
    (PROG (S L L1 L1D L2 L2D VL1 VL2 D1 D2 LD1 LD2 WD1 WD2 CAAR_LD FOUND)
      (COND ((AND SIMPP SIMPQ) (RETURN NIL)))
      (SETQ VL1 (INTERSECTION VL (GET P 'VARS)))
      (SETQ VL2 (INTERSECTION VL (GET Q 'VARS)))
      (PROG (A)
        (SETQ A (GET P 'DERIVS))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (COND ((EQUAL (CAAR A) F) (SETQ L1 (CONS A L1)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ L1 (SORT_DERIVS (REVERSE L1) (LIST F) VL1))
      (SETQ L NIL)
      (PROG (A)
        (SETQ A L1)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NOT (MEMBER (CDAR A) L))
             (PROGN
              (SETQ L (CONS (CDAR A) L))
              (SETQ L1D
                      (CONS (LIST (CDAR A) (ABSODEG (CDAR A)) (CDR A))
                            L1D))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ L1 (REVERSE L))
      (SETQ L1D (REVERSE L1D))
      (PROG (A)
        (SETQ A (GET Q 'DERIVS))
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (COND ((EQUAL (CAAR A) F) (SETQ L2 (CONS A L2)))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ L2 (SORT_DERIVS (REVERSE L2) (LIST F) VL2))
      (SETQ L NIL)
      (PROG (A)
        (SETQ A L2)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (COND
            ((NOT (MEMBER (CDAR A) L))
             (PROGN
              (SETQ L (CONS (CDAR A) L))
              (SETQ L2D
                      (CONS (LIST (CDAR A) (ABSODEG (CDAR A)) (CDR A))
                            L2D))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (SETQ L2 (REVERSE L))
      (SETQ L2D (REVERSE L2D))
      (COND
       ((NOT SIMPP)
        (PROGN
         (SETQ CAAR_LD (CAAR L1D))
         (SETQ D1 (CADAR L1D))
         (SETQ D2 (CADDAR L1D))
         (SETQ L L2D)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND L
                   (OR (LESSP D1 (CADAR L))
                       (AND (EQUAL D1 (CADAR L)) (LEQ D2 (CADDAR L))))))
             (RETURN NIL)))
           (PROGN
            (SETQ S (WHICH_DERIV CAAR_LD (CAAR L)))
            (COND
             ((EQUAL (PLUS (ABSODEG S) D1) (CADAR L))
              (PROGN (SETQ LD2 (CAAR L)) (SETQ FOUND T) (SETQ L NIL)))
             (T (SETQ L (CDR L)))))
           (GO WHILELABEL)))))
      (COND ((AND SIMPQ (NULL FOUND)) (RETURN NIL)))
      (COND
       ((AND (NOT LD2) (NOT SIMPQ))
        (PROGN
         (SETQ CAAR_LD (CAAR L2D))
         (SETQ D1 (CADAR L2D))
         (SETQ D2 (CADDAR L2D))
         (SETQ L L1D)
         (SETQ FOUND NIL)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND L
                   (OR (LESSP D1 (CADAR L))
                       (AND (EQUAL D1 (CADAR L)) (LEQ D2 (CADDAR L))))))
             (RETURN NIL)))
           (PROGN
            (SETQ S (WHICH_DERIV CAAR_LD (CAAR L)))
            (COND
             ((EQUAL (PLUS (ABSODEG S) D1) (CADAR L))
              (PROGN (SETQ LD1 (CAAR L)) (SETQ FOUND T) (SETQ L NIL)))
             (T (SETQ L (CDR L)))))
           (GO WHILELABEL)))))
      (COND ((AND SIMPP (NULL FOUND)) (RETURN NIL)))
      (RETURN
       (COND (LD2 (CONS (CONS S (CAAR L1D)) (CONS NIL LD2)))
             (LD1 (CONS (CONS NIL LD1) (CONS S (CAAR L2D))))
             (T
              (PROGN
               (SETQ WD1 (WHICH_DERIV (CAAR L1D) (CAAR L2D)))
               (SETQ WD2 (WHICH_DERIV (CAAR L2D) (CAAR L1D)))
               (COND
                ((OR (AND SIMPQ WD2) (AND SIMPP WD1) (AND RL WD1 WD2)) NIL)
                (T (CONS (CONS WD1 (CAAR L1D)) (CONS WD2 (CAAR L2D))))))))))) 
(PUT 'DIFFEQ 'NUMBER-OF-ARGS 3) 
(PUT 'DIFFEQ 'DEFINED-ON-LINE '259) 
(PUT 'DIFFEQ 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DIFFEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIFFEQ (F SD R)
    (PROG (RDIF RD CONTRADIC A AD B BD RESU MUST_BE_SUBST)
      (TERPRI)
      (PROGN
       (PRIN2 "How often is equation ")
       (PRIN2 R)
       (PRIN2 " to be differentiated?")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "(just `;' for no differentiation or, for example, `x,y,2;' ): ")
       NIL)
      (SETQ RDIF (TERMLISTREAD))
      (SETQ RD (GET R 'DERIVS))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND RD (NULL CONTRADIC))) (RETURN NIL)))
        (PROGN
         (SETQ A (CAAR RD))
         (SETQ RD (CDR RD))
         (COND
          ((EQUAL F (CAR A))
           (PROGN
            (SETQ AD (CDR A))
            (COND ((CDR A) (SETQ A (CONS 'DF A))) (T (SETQ A (CAR A))))
            (COND ((NULL RDIF) (SETQ B A))
                  (T (SETQ B (REVAL1 (CONS 'DF (CONS A RDIF)) T))))
            (COND ((PAIRP B) (SETQ BD (CDDR B))) (T (SETQ BD NIL)))
            (COND
             ((ZEROP B)
              (PROGN
               (PROGN
                (PRIN2 "The function ")
                (PRIN2 F)
                (PRIN2 " differentiated that way gives zero.")
                NIL)
               (SETQ CONTRADIC T)
               NIL))
             ((AND (NULL (WHICH_DERIV BD SD)) (WHICH_DERIV SD BD))
              (COND ((NULL RDIF) (SETQ MUST_BE_SUBST B))
                    (T
                     (PROGN
                      (SETQ CONTRADIC T)
                      (TERPRI)
                      (PROGN
                       (PRIN2 "This differentiation of equation ")
                       (PRIN2 R)
                       (PRIN2 " will generate a derivative ")
                       (PRIN2 B)
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        " which is a derivative of the derivative to be eliminated.")
                       NIL)
                      (TERPRI)
                      NIL))))
             ((EQUAL BD SD) (SETQ RESU (LIST R RDIF AD))))))))
        (GO WHILELABEL))
      (RETURN
       (COND ((OR CONTRADIC (NULL RESU)) NIL) (T (CONS RESU MUST_BE_SUBST)))))) 
(PUT 'READ_SUB_DIFF 'NUMBER-OF-ARGS 2) 
(PUT 'READ_SUB_DIFF 'DEFINED-ON-LINE '307) 
(PUT 'READ_SUB_DIFF 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'READ_SUB_DIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE READ_SUB_DIFF (P Q)
    (PROG (S L0 L M0 M1 F SD INFO_P INFO_Q CONTRADIC LET_CONFLICT)
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN (PRIN2 "What is the derivative to be eliminated? ") NIL)
      (PROGN (PRIN2 "(e.g.  df(f,x,y,2); or f; ) ") NIL)
      (TERPRI)
      (SETQ L0 (TERMXREAD))
      (SETQ L (REVAL1 L0 T))
      (COND ((NULL L) (RETURN NIL))
            ((NOT (PAIRP L)) (COND ((NEQ L0 L) (SETQ LET_CONFLICT T)) (T NIL)))
            ((NEQ (CAR L) 'DF)
             (COND
              ((NEQ (CAR L0) 'DF)
               (PROGN
                (PROGN (PRIN2 "Not a derivative!") NIL)
                (TERPRI)
                (RETURN NIL)))
              (T (SETQ LET_CONFLICT T))))
            ((NEQ (CADR L) (CADR L0)) (SETQ LET_CONFLICT T))
            (T
             (PROGN
              (SETQ M0 (CDDR L0))
              (SETQ M1 (CDDR L))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND M1 (NULL LET_CONFLICT))) (RETURN NIL)))
                (COND ((FIXP (CAR M1)) (SETQ M1 (CDR M1)))
                      (T
                       (PROGN
                        (COND
                         ((NEQ (NO_OF_V (CAR M1) M1) (NO_OF_V (CAR M1) M0))
                          (SETQ LET_CONFLICT T)))
                        (SETQ M1 (CDR M1)))))
                (GO WHILELABEL)))))
      (COND
       (LET_CONFLICT
        (PROGN
         (PROGN
          (PRIN2 "Due to a LET-rule in operation this elimination ")
          (PRIN2 "is not possible.")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "To delete a LET-rule use 'cr'.") NIL)
         (TERPRI)
         (RETURN NIL))))
      (COND ((PAIRP L) (PROGN (SETQ F (CADR L)) (SETQ SD (CDDR L))))
            (T (PROGN (SETQ F L) (SETQ SD NIL))))
      (SETQ INFO_P (DIFFEQ F SD P))
      (COND (INFO_P (SETQ INFO_Q (DIFFEQ F SD Q))))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN
       (COND
        ((AND INFO_P INFO_Q)
         (PROGN
          (COND ((AND (NULL (CADAR INFO_P)) (CADAR INFO_Q)) (SETQ S P))
                ((AND (NULL (CADAR INFO_Q)) (CADAR INFO_P)) (SETQ S Q))
                ((AND (CADAR INFO_P) (CADAR INFO_Q)) (SETQ S NIL))
                (T
                 (PROGN
                  (TERPRI)
                  (PROGN
                   (PRIN2
                    "Which equation is to be substituted (if that is possible)? Input ")
                   (PRIN2 P)
                   (PRIN2 " or ")
                   (PRIN2 Q)
                   (PRIN2 ": ")
                   NIL)
                  (PROG ()
                   REPEATLABEL
                    (SETQ S (REVAL1 (TERMREAD) T))
                    (COND
                     ((NOT (OR (EQUAL S P) (EQUAL S Q))) (GO REPEATLABEL)))))))
          (COND
           ((AND (EQUAL S P) (CDR INFO_Q))
            (PROGN
             (SETQ CONTRADIC T)
             (TERPRI)
             (PROGN
              (PRIN2 "The derivative ")
              (PRIN2 (CDR INFO_Q))
              (PRIN2 " would enter ")
              (PRIN2 P)
              NIL)
             (TERPRI)
             (PROGN
              (PRIN2
               " which is a derivative of the derivative to be substituted.")
              NIL)
             NIL)))
          (COND
           ((AND (EQUAL S Q) (CDR INFO_P))
            (PROGN
             (SETQ CONTRADIC T)
             (TERPRI)
             (PROGN
              (PRIN2 "The derivative ")
              (PRIN2 (CDR INFO_P))
              (PRIN2 " would enter ")
              (PRIN2 Q)
              NIL)
             (TERPRI)
             (PROGN
              (PRIN2
               " which is a derivative of the derivative to be substituted.")
              NIL)
             NIL)))
          (COND (CONTRADIC NIL)
                (T (CONS (LIST (CAR INFO_P) (CAR INFO_Q) L S NIL) 1)))))
        (T NIL))))) 
(PUT 'DEC_INFO 'NUMBER-OF-ARGS 6) 
(PUT 'DEC_INFO 'DEFINED-ON-LINE '378) 
(PUT 'DEC_INFO 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_INFO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_INFO (P Q F VL RL ORDERING)
    (PROG (A B L L1 INFO M N FP FQ FPMQ FQMP S LENP LENQ DP DQ SIMPP SIMPQ
           LET_CONFLICT)
      (COND (EXPERT_MODE (RETURN (READ_SUB_DIFF P Q))))
      (SETQ LENP (GET P 'LENGTH))
      (SETQ LENQ (GET Q 'LENGTH))
      (COND ((AND RL (GREATERP (TIMES LENP LENQ) MAX_RED_LEN)) (RETURN NIL)))
      (SETQ SIMPP
              (OR (NULL (GET P 'ALLVARFCTS)) (NEQ F (CAAAR (GET P 'DERIVS)))))
      (SETQ SIMPQ
              (OR (NULL (GET Q 'ALLVARFCTS)) (NEQ F (CAAAR (GET Q 'DERIVS)))))
      (SETQ L (DEC_LD_INFO P Q SIMPP SIMPQ F VL RL))
      (COND ((NOT L) (PROGN (ADD_BOTH_DEC_WITH ORDERING P Q RL) (RETURN NIL))))
      (SETQ A (CAAR L))
      (SETQ B (CADR L))
      (COND
       ((AND STRUC_EQN
             (OR (AND A B (NOT (FREEOF (AEVAL 'STRUC_DONE) F)))
                 (AND (GREATERP (GET P 'NO_DERIVS) 0)
                      (EQUAL (GET Q 'NO_DERIVS) 0))
                 (AND (EQUAL (GET P 'NO_DERIVS) 0)
                      (GREATERP (GET Q 'NO_DERIVS) 0))))
        (RETURN NIL)))
      (SETQ L1 (CDDR L))
      (SETQ L (CDAR L))
      (COND
       ((AND (NULL A) (NULL L))
        (COND ((NEQ F (REVAL1 F T)) (SETQ LET_CONFLICT T)) (T NIL)))
       (T
        (PROGN
         (SETQ M (REVAL1 (CONS 'DF (CONS F (APPEND L A))) T))
         (COND
          ((OR (NOT (PAIRP M)) (NEQ (CAR M) 'DF) (NEQ (CADR M) F))
           (SETQ LET_CONFLICT T))
          (T
           (PROGN
            (SETQ M (CDDR M))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND M (NULL LET_CONFLICT))) (RETURN NIL)))
              (COND ((FIXP (CAR M)) (SETQ M (CDR M)))
                    (T
                     (PROGN
                      (COND
                       ((NEQ (PLUS (NO_OF_V (CAR M) A) (NO_OF_V (CAR M) L))
                             (NO_OF_V (CAR M) M))
                        (SETQ LET_CONFLICT T)))
                      (SETQ M (CDR M)))))
              (GO WHILELABEL))))))))
      (COND
       (LET_CONFLICT
        (PROGN
         (COND
          (PRINT_
           (PROGN
            (PROGN
             (PRIN2 "Due to a let-rule in operation equations ")
             (PRIN2 P)
             (PRIN2 ",")
             (PRIN2 Q)
             (PRIN2 " will not be paired.")
             NIL)
            (TERPRI)
            NIL)))
         (ADD_BOTH_DEC_WITH ORDERING P Q RL)
         (RETURN NIL))))
      (COND ((AND A (NOT B)) (SETQ S Q)) ((AND B (NOT A)) (SETQ S P))
            ((NOT (OR A B))
             (COND
              ((AND STRUC_EQN L L1)
               (PROGN
                (SETQ M (GET P 'NO_DERIVS))
                (SETQ N (GET Q 'NO_DERIVS))
                (COND ((GREATERP M N) (SETQ S P)) ((LESSP M N) (SETQ S Q))
                      ((GREATERP (GET P 'LENGTH) (GET Q 'LENGTH)) (SETQ S P))
                      (T (SETQ S Q)))))
              (T
               (PROGN
                (SETQ DP (GET P 'DERIVS))
                (SETQ DQ (GET Q 'DERIVS))
                (PROG ()
                 REPEATLABEL
                  (PROGN
                   (SETQ S (TOTAL_LESS_DFREL (CAR DP) (CAR DQ) FTEM_ VL))
                   (SETQ DP (CDR DP))
                   (SETQ DQ (CDR DQ)))
                  (COND
                   ((NOT (OR (NEQ S 0) (NULL DP) (NULL DQ)))
                    (GO REPEATLABEL))))
                (COND ((EQUAL S T) (SETQ S Q)) (T (SETQ S P))))))))
      (SETQ FP (GET P 'ALLVARFCTS))
      (SETQ FQ (GET Q 'ALLVARFCTS))
      (SETQ FQMP (LENGTH (SETDIFF_ACCORDING_TO FQ FP FTEM_)))
      (SETQ FPMQ (LENGTH (SETDIFF_ACCORDING_TO FP FQ FTEM_)))
      (COND
       (NIL
        (COND
         (TR_DECOUPLE
          (PROGN
           (TERPRI)
           (PROGN
            (PRIN2 "p=")
            (PRIN2 P)
            (PRIN2 " q=")
            (PRIN2 Q)
            (PRIN2 " s=")
            (PRIN2 S)
            (PRIN2 " lfp=")
            (PRIN2 (LENGTH FP))
            (PRIN2 " lfq=")
            (PRIN2 (LENGTH FQ))
            (PRIN2 " lfu=")
            (PRIN2 (LENGTH (UNION FP FQ)))
            (PRIN2 " fqmp=")
            (PRIN2 FQMP)
            (PRIN2 " fpmq=")
            (PRIN2 FPMQ)
            NIL))))))
      (COND
       ((AND (NULL A) (NULL B))
        (PROGN
         (SETQ DP (GET P 'DERIVS))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND DP (NEQ (CAAAR DP) F))) (RETURN NIL)))
           (SETQ DP (CDR DP))
           (GO WHILELABEL))
         (SETQ DQ (GET Q 'DERIVS))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND DP (NEQ (CAAAR DQ) F))) (RETURN NIL)))
           (SETQ DQ (CDR DQ))
           (GO WHILELABEL))
         (COND
          (DEC_DEPTH_FIRST_SEARCH
           (PROGN
            (SETQ M (LENGTH (GET P 'ALLVARFCTS)))
            (SETQ N (LENGTH (GET Q 'ALLVARFCTS)))
            (SETQ M
                    (TIMES (COND ((GREATERP M N) (EXPT 10 M)) (T (EXPT 10 N)))
                           (COND ((GREATERP (CDAR DP) (CDAR DQ)) (CDAR DP))
                                 (T (CDAR DQ)))))))
          (T
           (PROGN
            (SETQ M (DIFFERENCE (CDAR DP) (CDAR DQ)))
            (COND ((LESSP M 0) (SETQ M (MINUS M))))
            (SETQ M (TIMES (EXPT 2 M) (PLUS LENQ LENP)))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND DP (EQUAL (CAAAR DP) F))) (RETURN NIL)))
              (PROGN (SETQ M (TIMES M 2)) (SETQ DP (CDR DP)))
              (GO WHILELABEL))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND DQ (EQUAL (CAAAR DQ) F))) (RETURN NIL)))
              (PROGN (SETQ M (TIMES M 2)) (SETQ DQ (CDR DQ)))
              (GO WHILELABEL))
            (COND
             ((GREATERP FQMP 0) (SETQ M (TIMES M (EXPT 3 (TIMES 2 FQMP))))))
            (COND
             ((GREATERP FPMQ 0) (SETQ M (TIMES M (EXPT 3 (TIMES 2 FPMQ))))))
            NIL)))))
       (T
        (SETQ M
                (TIMES
                 (PLUS (TIMES (EXPT 1.5 (ABSODEG A)) LENP)
                       (TIMES (EXPT 1.5 (ABSODEG B)) LENQ))
                 (EXPT (LENGTH (UNION FP FQ)) 20)))))
      (COND (NIL (COND (TR_DECOUPLE (PROGN (PRIN2 " m2=") (PRIN2 M) NIL)))))
      (COND
       (S
        (PROGN
         (COND
          ((AND (EQUAL S Q) (GREATERP LENP LENQ))
           (SETQ M (QUOTIENT (TIMES M LENP) LENQ)))
          ((AND (EQUAL S P) (GREATERP LENQ LENP))
           (SETQ M (QUOTIENT (TIMES M LENQ) LENP))))
         (COND
          ((AND (EQUAL S P) (GREATERP FQMP 0))
           (SETQ M (TIMES M (EXPT 10 (TIMES 2 FQMP)))))
          ((AND (EQUAL S Q) (GREATERP FPMQ 0))
           (SETQ M (TIMES M (EXPT 10 (TIMES 2 FPMQ))))))
         (COND
          (STRUC_EQN
           (COND
            ((OR (AND A (IS_ALGEBRAIC P)) (AND B (IS_ALGEBRAIC Q)))
             (SETQ M (TIMES M (EXPT 10 100))))
            ((AND (IS_ALGEBRAIC P) (IS_ALGEBRAIC Q))
             (SETQ M (QUOTIENT M (EXPT 10 5)))))))))
       (T (SETQ M (TIMES M 10))))
      (COND
       ((AND FLIN_ (FREEOF FLIN_ F) (NOT (FREEOFLIST FP FLIN_))
             (NOT (FREEOFLIST FQ FLIN_)))
        (SETQ M (TIMES M 100))))
      (COND (NIL (COND (TR_DECOUPLE (PROGN (PRIN2 " m3=") (PRIN2 M) NIL)))))
      (SETQ INFO
              (CONS
               (LIST (LIST P A L) (LIST Q B L1)
                     (COND ((AND (NULL A) (NULL L)) F)
                           (T (REVAL1 (CONS 'DF (CONS F (APPEND L A))) T)))
                     S (OR RL SIMPP SIMPQ))
               M))
      (RETURN INFO))) 
(PUT 'DEC_INFO_LEQ 'NUMBER-OF-ARGS 2) 
(PUT 'DEC_INFO_LEQ 'DEFINED-ON-LINE '627) 
(PUT 'DEC_INFO_LEQ 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_INFO_LEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEC_INFO_LEQ (IP IQ)
    (PROG (H DFP DFQ FP FQ)
      (RETURN
       (COND
        ((AND IP IQ)
         (COND
          (DEC_DEPTH_FIRST_SEARCH
           (PROGN
            (SETQ DFP (CADDAR IP))
            (SETQ FP (COND ((PAIRP DFP) (CADR DFP)) (T DFP)))
            (SETQ DFQ (CADDAR IQ))
            (SETQ FQ (COND ((PAIRP DFQ) (CADR DFQ)) (T DFQ)))
            (COND
             ((NEQ FP FQ)
              (COND ((EQUAL FP (WHICH_FIRST FP FQ FTEM_)) NIL) (T T)))
             (T
              (PROGN
               (SETQ DFP (COND ((PAIRP DFP) (CDR DFP)) (T (LIST DFP))))
               (SETQ DFQ (COND ((PAIRP DFQ) (CDR DFQ)) (T (LIST DFQ))))
               (SETQ H
                       (TOTAL_LESS_DFREL (CONS DFP 1) (CONS DFQ 1)
                        (COND ((EQUAL FP FQ) (LIST FP)) (T (LIST FP FQ))) VL_))
               (OR (EQUAL H T) (AND (EQUAL H 0) (LEQ (CDR IP) (CDR IQ)))))))))
          (T (LEQ (CDR IP) (CDR IQ)))))
        (IP IP) (T IQ))))) 
(PUT 'DEC_AND_FCT_SELECT 'NUMBER-OF-ARGS 5) 
(PUT 'DEC_AND_FCT_SELECT 'DEFINED-ON-LINE '656) 
(PUT 'DEC_AND_FCT_SELECT 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_AND_FCT_SELECT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_AND_FCT_SELECT (PDES VL RL HP ORDERING)
    (PROG (MIN F L L1 L2 DONE_PDES CAR_PDES LEN D_CAR_PDES VAL_CAR_PDES VAL_P
           D_P W1 W2 RTN F_IN_FLIN ALLVARFL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND PDES (NULL RTN))) (RETURN NIL)))
        (PROGN
         (SETQ CAR_PDES (CAR PDES))
         (SETQ ALLVARFL (GET CAR_PDES 'ALLVARFCTS))
         (COND
          ((OR EXPERT_MODE
               (AND (FLAGP CAR_PDES 'TO_DECOUP) ALLVARFL
                    (OR (NULL HP)
                        (AND (LESSP (LENGTH ALLVARFL) 4)
                             (LESSP (GET CAR_PDES 'TERMS) 10))
                        (EQUAL (LENGTH ALLVARFL) 1))))
           (PROGN
            (SETQ F (CAAAR (GET CAR_PDES 'DERIVS)))
            (COND
             ((OR (NOT FLIN_) (SETQ F_IN_FLIN (NOT (FREEOF FLIN_ F)))
                  (AND FHOM_ (ZEROP (CAR (GET CAR_PDES 'HOM_DEG))))
                  (AND (NULL FHOM_) (FREEOFLIST (GET CAR_PDES 'FCTS) FLIN_))
                  (FREEOFLIST ALLVARFL FLIN_))
              (PROGN
               (SETQ LEN (GET CAR_PDES 'PRINTLENGTH))
               (COND
                ((AND (NULL RECORD_HIST) (EQUAL LEN 1))
                 (PROGN
                  (SETQ VAL_CAR_PDES (CAR (GET CAR_PDES 'SQVAL)))
                  (COND
                   ((AND (NULL (CDR VAL_CAR_PDES)) (EQ (CDAR VAL_CAR_PDES) 1)
                         (EQ (CDAAR VAL_CAR_PDES) 1)
                         (PAIRP (CAAAR VAL_CAR_PDES))
                         (EQUAL (CAAAAR VAL_CAR_PDES) 'DF)
                         (EQUAL (CADR (CAAAR VAL_CAR_PDES)) F))
                    (SETQ D_CAR_PDES (CDDR (CAAAR VAL_CAR_PDES))))
                   ((EQUAL (CAAAR VAL_CAR_PDES) F) (SETQ D_CAR_PDES NIL))
                   (T (SETQ LEN 1000))))))
               (SETQ L (ASSOC ORDERING (GET CAR_PDES 'DEC_WITH)))
               (COND
                (RL
                 (SETQ L
                         (APPEND L
                                 (ASSOC ORDERING
                                        (GET CAR_PDES 'DEC_WITH_RL))))))
               (PROG (P)
                 (SETQ P (CDR PDES))
                LAB
                 (COND ((NULL P) (RETURN NIL)))
                 ((LAMBDA (P)
                    (COND
                     ((OR EXPERT_MODE
                          (AND (FLAGP P 'TO_DECOUP)
                               (MEMBER F (GET P 'RATIONAL))
                               (OR (NULL HP)
                                   (AND
                                    (LESSP
                                     (SETQ L1
                                             (LENGTH
                                              (UNION ALLVARFL
                                                     (GET P 'ALLVARFCTS))))
                                     4)
                                    (LESSP (GET P 'TERMS) 10))
                                   (EQUAL L1 1))
                               (OR (NOT (MEMBER P L))
                                   (AND
                                    (NOT
                                     (MEMBER CAR_PDES
                                             (ASSOC ORDERING
                                                    (GET P 'DEC_WITH))))
                                    (OR (NULL RL)
                                        (NOT
                                         (MEMBER CAR_PDES
                                                 (ASSOC ORDERING
                                                        (GET P
                                                             'DEC_WITH_RL)))))))))
                      (COND
                       ((AND (NULL RECORD_HIST) (EQUAL LEN 1)
                             (EQUAL (GET P 'PRINTLENGTH) 1))
                        (PROGN
                         (SETQ VAL_P (CAR (GET P 'SQVAL)))
                         (SETQ D_P 0)
                         (COND
                          ((AND (NULL (CDR VAL_P)) (EQ (CDAR VAL_P) 1)
                                (EQ (CDAAR VAL_P) 1) (PAIRP (CAAAR VAL_P))
                                (EQUAL (CAAAAR VAL_P) 'DF)
                                (EQUAL (CADR (CAAAR VAL_P)) F))
                           (SETQ D_P (CDDR (CAAAR VAL_P))))
                          ((EQUAL (CAAAR VAL_P) F) (SETQ D_P NIL)))
                         (COND
                          ((NEQ D_P 0)
                           (PROGN
                            (SETQ W1 (WHICH_DERIV D_P D_CAR_PDES))
                            (SETQ W2 (WHICH_DERIV D_CAR_PDES D_P))
                            (COND
                             ((AND W1 W2)
                              (ADD_BOTH_DEC_WITH ORDERING CAR_PDES P RL))
                             ((NULL W1)
                              (SETQ RTN
                                      (LIST (LIST P NIL D_P)
                                            (LIST CAR_PDES W2 D_CAR_PDES)
                                            (CAAAR VAL_P) P NIL)))
                             (T
                              (SETQ RTN
                                      (LIST (LIST P W1 D_P)
                                            (LIST CAR_PDES NIL D_CAR_PDES)
                                            (CAAAR VAL_CAR_PDES) CAR_PDES
                                            NIL)))))))))
                       (T
                        (PROGN
                         (SETQ L1 (DEC_INFO CAR_PDES P F VL RL ORDERING))
                         (COND
                          ((AND EXPERT_MODE (NULL L1))
                           (PROGN
                            (SETQ PDES (LIST NIL))
                            (SETQ DONE_PDES NIL)
                            (SETQ L2 NIL)))
                          ((OR EXPERT_MODE
                               (AND QUICK_DECOUP L1 (CADDDR (CAR L1))
                                    (OR (NULL STRUC_EQN)
                                        (AND (NULL (IS_ALGEBRAIC CAR_PDES))
                                             (NULL (IS_ALGEBRAIC P))))))
                           (SETQ RTN (CAR L1)))
                          (L1 (SETQ L2 (CONS L1 L2))))))))))
                  (CAR P))
                 (SETQ P (CDR P))
                 (GO LAB))
               (COND
                ((NULL RTN)
                 (PROG (P)
                   (SETQ P DONE_PDES)
                  LAB
                   (COND ((NULL P) (RETURN NIL)))
                   ((LAMBDA (P)
                      (COND
                       ((AND (FLAGP P 'TO_DECOUP) (MEMBER F (GET P 'RATIONAL))
                             (OR (NULL HP)
                                 (AND
                                  (LESSP
                                   (SETQ L1
                                           (LENGTH
                                            (UNION ALLVARFL
                                                   (GET P 'ALLVARFCTS))))
                                   4)
                                  (LESSP (GET P 'TERMS) 10))
                                 (EQUAL L1 1))
                             (OR (NOT (MEMBER P L))
                                 (AND
                                  (NOT
                                   (MEMBER CAR_PDES
                                           (ASSOC ORDERING (GET P 'DEC_WITH))))
                                  (OR (NULL RL)
                                      (NOT
                                       (MEMBER CAR_PDES
                                               (ASSOC ORDERING
                                                      (GET P
                                                           'DEC_WITH_RL)))))))
                             (OR (NULL (GET P 'ALLVARFCTS))
                                 (NEQ F (CAR (GET P 'ALLVARFCTS)))))
                        (PROGN
                         (SETQ L1 (DEC_INFO CAR_PDES P F VL RL ORDERING))
                         (COND
                          ((AND EXPERT_MODE (NULL L1))
                           (PROGN
                            (SETQ PDES (LIST NIL))
                            (SETQ DONE_PDES NIL)
                            (SETQ L2 NIL)))
                          ((AND QUICK_DECOUP L1 (CADDDR (CAR L1))
                                (OR (NULL STRUC_EQN)
                                    (AND (NULL (IS_ALGEBRAIC CAR_PDES))
                                         (NULL (IS_ALGEBRAIC P)))))
                           (SETQ RTN (CAR L1)))
                          (L1 (SETQ L2 (CONS L1 L2))))))))
                    (CAR P))
                   (SETQ P (CDR P))
                   (GO LAB))))))))))
         (SETQ DONE_PDES (CONS CAR_PDES DONE_PDES))
         (SETQ PDES (CDR PDES)))
        (GO WHILELABEL))
      (COND (RTN (RETURN RTN)))
      (SETQ L1 NIL)
      (PROG (L)
        (SETQ L L2)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (COND
            ((CADDDR (CAR L))
             (PROGN
              (SETQ F (CADDAR L))
              (COND ((PAIRP F) (SETQ F (CADR F))))
              (COND
               ((AND (EQUAL (CAAAR L) (CADDDR (CAR L)))
                     (GET (CAAAR L) 'ALLVARFCTS)
                     (EQUAL F (CAR (GET (CAAAR L) 'ALLVARFCTS))))
                (SETQ L1 (UNION (LIST (CAAAR L)) L1))))
              (COND
               ((AND (EQUAL (CAADAR L) (CADDDR (CAR L)))
                     (GET (CAADAR L) 'ALLVARFCTS)
                     (EQUAL F (CAR (GET (CAADAR L) 'ALLVARFCTS))))
                (SETQ L1 (UNION (LIST (CAADAR L)) L1))))
              NIL))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (PROG (L)
        (SETQ L L2)
       LAB
        (COND ((NULL L) (RETURN NIL)))
        ((LAMBDA (L)
           (COND
            ((AND
              (OR (EQUAL (CADAAR L) NIL) (EQUAL (CADR (CADAR L)) NIL)
                  (AND (FREEOF L1 (CAAAR L)) (FREEOF L1 (CAADAR L))))
              (DEC_INFO_LEQ L MIN))
             (SETQ MIN L))))
         (CAR L))
        (SETQ L (CDR L))
        (GO LAB))
      (COND
       (MIN
        (PROGN
         (SETQ L (CAR MIN))
         (COND
          ((AND (CADAR L) (CADADR L))
           (PROGN
            (SETQ F (CADDR L))
            (COND ((PAIRP F) (SETQ F (CADR F))))
            (ADD_BOTH_DEC_WITH ORDERING (CAAR L) (CAADR L) RL)
            NIL)))
         (RETURN L)))))) 
(PUT 'ERR_CATCH_ELIMIN 'NUMBER-OF-ARGS 12) 
(PUT 'ERR_CATCH_ELIMIN 'DEFINED-ON-LINE '865) 
(PUT 'ERR_CATCH_ELIMIN 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'ERR_CATCH_ELIMIN 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE ERR_CATCH_ELIMIN (DDPCP P LTP PMULT DGP DDQCP Q LTQ QMULT DGQ X ONCE)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_ELIMIN))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_ELIMIN)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'ELIMIN (MKQUOTE DDPCP) (MKQUOTE P) (MKQUOTE LTP)
                        (MKQUOTE PMULT) (MKQUOTE DGP) (MKQUOTE DDQCP)
                        (MKQUOTE Q) (MKQUOTE LTQ) (MKQUOTE QMULT) (MKQUOTE DGQ)
                        (MKQUOTE X) (MKQUOTE ONCE))
                  T T)))
       T)
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'ERR_CATCH_LOADFORM2 'NUMBER-OF-ARGS 0) 
(PUT 'ERR_CATCH_LOADFORM2 'DEFINED-ON-LINE '886) 
(PUT 'ERR_CATCH_LOADFORM2 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'ERR_CATCH_LOADFORM2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ERR_CATCH_LOADFORM2 NIL
    (PROG (H)
      (COND ((NULL CRACK_LOAD_COMMAND) (CRACK_LOAD_CMD)))
      (SETQ H
              (COMPRESS
               (REVERSE
                (CONS '|"|
                      (CDDDDR
                       (CDDDR (REVERSE (EXPLODE CRACK_LOAD_COMMAND))))))))
      (COND
       ((OR (AND (MEMBER 'CSL LISPSYSTEM*) (MEMBER 'SIXTY-FOUR LISPSYSTEM*))
            (AND (MEMBER 'PSL LISPSYSTEM*) (BETAP (ASHIFT 1 20))))
        (SETQ H (BLDMSG_INTERNAL "%w/form2_64" (LIST H))))
       (T (SETQ H (BLDMSG_INTERNAL "%w/form2_32" (LIST H)))))
      (SETQ H (ERRORSET (LIST 'LOAD H) T T))
      (COND ((ATOM H) (SETQ FORM_PIPE NIL))))) 
(PUT 'ELIMIN 'NUMBER-OF-ARGS 12) 
(PUT 'ELIMIN 'DEFINED-ON-LINE '901) 
(PUT 'ELIMIN 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'ELIMIN 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE ELIMIN (DDPCP P LTP PMULT DGP DDQCP Q LTQ QMULT DGQ X ONCE)
    (PROG (DGS S QUOTI FLG LTS XSQ LCO NATBAK FORMPUTFILE FORMSZEFILE
           FORMGETFILE SS SZE CURRENT_DIR T0 T1 T2 T3 T4 EQN_INPUT OLDINPU
           INTBAK UNCACHEDBAK FCPP FCQP FCSP FCPQ FCQQ FCSQ H KLIST SLIST FLIST
           OPRTCH CHNOUT ALL_RATIONAL_KERNELS XTRNLCALL)
      (COND ((GREATERP DGP DGQ) (PROGN (SETQ FLG T) (SETQ DGS DGQ)))
            (T (SETQ DGS DGP)))
      (SETQ FCPP (SIMP 1))
      (SETQ FCPQ (SIMP 0))
      (SETQ FCQQ (SIMP 1))
      (SETQ FCQP (SIMP 0))
      (SETQ XSQ (MKSQ X 1))
      (COND
       ((AND (NULL (GET DDPCP 'NON_RAT_KERN)) (NULL (GET DDQCP 'NON_RAT_KERN)))
        (SETQ ALL_RATIONAL_KERNELS T)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ DGS 0)) (RETURN NIL)))
        (PROGN
         (COND
          ((AND FORM_COMP ALL_RATIONAL_KERNELS)
           (PROGN
            (SETQ T0 (TIME))
            (SETQ NATBAK *NAT)
            (COND (*NAT (AEVAL* (OFF (LIST 'NAT)))))
            (SETQ SS (LEVEL_STRING SESSION_))
            (SETQ CURRENT_DIR
                    (COND ((NEQ FORM_TMP_DIR "") FORM_TMP_DIR) (T (PWD))))
            (SETQ FORM_EQN_IDX (ADD1 FORM_EQN_IDX))
            (SETQ FORMPUTFILE
                    (BLDMSG_INTERNAL "%w%w_%d%w"
                                     (LIST CURRENT_DIR SS FORM_EQN_IDX
                                           ".frm")))
            (SETQ FORMSZEFILE
                    (BLDMSG_INTERNAL "%w%w_%d%w"
                                     (LIST CURRENT_DIR SS FORM_EQN_IDX
                                           ".sze")))
            (SETQ FORMGETFILE
                    (BLDMSG_INTERNAL "%w%w_%d%w"
                                     (LIST CURRENT_DIR SS FORM_EQN_IDX
                                           ".red")))
            (COND (FORM_PIPE (ERR_CATCH_LOADFORM2)))
            (COND
             (FORM_PIPE
              (PROGN
               (SYSTEM
                (BLDMSG_INTERNAL "mkdir -p /tmp/formtempdir.%w"
                                 (LIST (GETPID))))
               (CD (BLDMSG_INTERNAL "/tmp/formtempdir.%w" (LIST (GETPID))))
               (COND
                (FORM_CHNPAIR
                 (PROGN
                  (CLOSE (CADR FORM_CHNPAIR))
                  (CLOSE (CADDR FORM_CHNPAIR)))))
               (SETQ FORM_CHNPAIR (START_FORM))
               (SETQ CHNOUT (WRS (CADDR FORM_CHNPAIR)))
               (LINELENGTH 1000000)
               NIL))
             (T (AEVAL* (OUT (LIST FORMPUTFILE)))))
            (SETQ OPRTCH (GET 'EXPT 'PRTCH))
            (PUT 'EXPT 'PRTCH '^)
            (PROGN (PRIN2 "#: CommentChar %") NIL)
            (TERPRI)
            (COND
             ((NEQ FORM_TMP_DIR "")
              (PROGN
               (PROGN
                (PRIN2
                 (BLDMSG_INTERNAL "%w%w" (LIST "#: TempDir " FORM_TMP_DIR)))
                NIL)
               (TERPRI))))
            (PROGN (PRIN2 "on highfirst;") NIL)
            (TERPRI)
            (SETQ KLIST (UNION (GET DDPCP 'KERN) (GET DDQCP 'KERN)))
            (PROG (H)
              (SETQ H KLIST)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (COND ((ATOM H) (SETQ SLIST (UNION (LIST H) SLIST)))
                       (T
                        (PROGN
                         (COND
                          ((EQUAL (CAR H) 'DF)
                           (SETQ SLIST (UNION (LIST (CADR H)) SLIST))))
                         (SETQ FLIST (UNION (LIST (CAR H)) FLIST))
                         NIL))))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (SETQ SLIST (KERNEL_SORT (UNION VL_ SLIST)))
            (SETQ FLIST (KERNEL_SORT FLIST))
            (PROGN (PRIN2 "Off statistics;") NIL)
            (TERPRI)
            (COND
             (FLIST
              (PROGN
               (PROGN (PRIN2 "CFunctions") NIL)
               (PROG (H)
                 (SETQ H FLIST)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H) (PROGN (PRIN2 " ") (PRIN2 H) NIL)) (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               (PROGN (PRIN2 ";") NIL)
               (TERPRI))))
            (PROGN (PRIN2 "Symbols") NIL)
            (PROG (H)
              (SETQ H SLIST)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H) (PROGN (PRIN2 " ") (PRIN2 H) NIL)) (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (PROGN (PRIN2 ";") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L p =") NIL)
            (WRITESQFRM P)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L mp=") NIL)
            (WRITESQFRM PMULT)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L q =") NIL)
            (WRITESQFRM Q)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L mq=") NIL)
            (WRITESQFRM QMULT)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L rez=p*mp-q*mq;") NIL)
            (TERPRI)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (TERPRI)
            (PROGN (PRIN2 "skip;") NIL)
            (TERPRI)
            (PROGN (PRIN2 "L nt=termsin_(rez);") NIL)
            (TERPRI)
            (PROGN (PRIN2 ".sort") NIL)
            (TERPRI)
            (PROGN (PRIN2 "skip;") NIL)
            (TERPRI)
            (COND
             (FORM_PIPE
              (PROGN
               (PROGN (PRIN2 "#toexternal \"%e $ \", nt ") NIL)
               (TERPRI)
               (PROGN (PRIN2 "#toexternal \"     \"     ") NIL)
               (TERPRI)
               (PROGN (PRIN2 "#toexternal \" end$\"     ") NIL)
               (TERPRI)
               (PROGN (PRIN2 "#toexternal \"     \"     ") NIL)
               (TERPRI)
               (TERPRI)
               NIL))
             (T
              (PROGN
               (PROGN
                (PRIN2 "#write <")
                (PRIN2 FORMSZEFILE)
                (PRIN2 "> \"%E $ \", nt ")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "#write <")
                (PRIN2 FORMSZEFILE)
                (PRIN2 "> \"     \"     ")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "#write <")
                (PRIN2 FORMSZEFILE)
                (PRIN2 "> \" end$\"     ")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2 "#write <")
                (PRIN2 FORMSZEFILE)
                (PRIN2 "> \"     \"     ")
                NIL)
               (TERPRI)
               (TERPRI)
               NIL)))
            (PROGN
             (PRIN2 "#write <")
             (PRIN2 FORMGETFILE)
             (PRIN2 "> \"%E $ \", rez")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "#write <")
             (PRIN2 FORMGETFILE)
             (PRIN2 "> \"     \"     ")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "#write <")
             (PRIN2 FORMGETFILE)
             (PRIN2 "> \" end$\"     ")
             NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "#write <")
             (PRIN2 FORMGETFILE)
             (PRIN2 "> \"     \"     ")
             NIL)
            (TERPRI)
            (TERPRI)
            (PROGN (PRIN2 ".end") NIL)
            (TERPRI)
            (COND
             (FORM_PIPE
              (PROGN (WRS CHNOUT) (CHANNELFLUSH (CADDR FORM_CHNPAIR))))
             (T (AEVAL* (SHUT (LIST FORMPUTFILE)))))
            (PUT 'EXPT 'PRTCH OPRTCH)
            (COND (NATBAK (AEVAL* (ON (LIST 'NAT)))))
            (SETQ T1 (TIME))
            (COND
             ((NOT FORM_PIPE)
              (PROGN
               (SETQ XTRNLCALL (GETENV "FormCall"))
               (COND
                ((NULL XTRNLCALL)
                 (PROGN
                  (COND ((NULL CRACK_LOAD_COMMAND) (CRACK_LOAD_CMD)))
                  (SETQ XTRNLCALL
                          (COMPRESS
                           (REVERSE
                            (CONS '|"|
                                  (CDDDDR
                                   (CDDDR
                                    (REVERSE
                                     (EXPLODE CRACK_LOAD_COMMAND))))))))
                  (COND
                   ((OR
                     (AND (MEMBER 'CSL LISPSYSTEM*)
                          (MEMBER 'SIXTY-FOUR LISPSYSTEM*))
                     (AND (MEMBER 'PSL LISPSYSTEM*) (BETAP (ASHIFT 1 20))))
                    (SETENV "FormCall"
                            (BLDMSG_INTERNAL "%w/form64/form"
                                             (LIST XTRNLCALL))))
                   (T
                    (SETENV "FormCall"
                            (BLDMSG_INTERNAL "%w/form32/form"
                                             (LIST XTRNLCALL)))))
                  (SETQ XTRNLCALL (GETENV "FormCall")))))
               (COND
                ((NULL XTRNLCALL)
                 (REDERR "The environment variable FormCall is not set.")))
               (COND
                ((NULL (FILEP XTRNLCALL))
                 (REDERR
                  (LIST "No file " XTRNLCALL
                        " was found. Probably form is not installed."
                        " This should have been distributed with CRACK."))))
               (SETQ SS
                       (BLDMSG_INTERNAL "%w -q %w"
                                        (LIST XTRNLCALL FORMPUTFILE)))
               (SYSTEM SS))))
            (SETQ T2 T1)
            (COND ((NOT FORM_PIPE) (PROGN NIL)))
            (SETQ UNCACHEDBAK *UNCACHED)
            (SETQ *UNCACHED T)
            (SETQ INTBAK *INT)
            (SETQ *INT NIL)
            (COND
             (FORM_PIPE
              (PROGN
               (SETQ OLDINPU (RDS (CADR FORM_CHNPAIR)))
               (SETQ SZE (XREAD T))))
             (T
              (PROGN
               (SETQ EQN_INPUT (OPEN FORMSZEFILE 'INPUT))
               (SETQ OLDINPU (RDS EQN_INPUT))
               (SETQ SZE (XREAD T))
               (CLOSE EQN_INPUT))))
            (RDS OLDINPU)
            (COND
             (PRINT_
              (PROGN
               (PROGN
                (PRIN2 "  FORM computed ")
                (PRIN2 SZE)
                (PRIN2 " term(s).")
                NIL)
               (TERPRI))))
            (COND
             ((NOT FORM_PIPE)
              (PROGN
               (SETQ SS (BLDMSG_INTERNAL "rm %w" (LIST FORMSZEFILE)))
               (SYSTEM SS))))
            (SETQ FORM_EQN_ON_DISK
                    (CONS (CONS SZE FORMGETFILE) FORM_EQN_ON_DISK))
            (SETQ T3 (TIME))
            (SETQ *INT INTBAK)
            (SETQ *UNCACHED UNCACHEDBAK)
            NIL)))
         (COND
          ((AND FORM_COMP (GREATERP SZE FORM_MAX_READ))
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (PROGN
                (PRIN2 "Because the equation with ")
                (PRIN2 SZE)
                (PRIN2 " terms computed by FORM is too big, it")
                NIL)
               (TERPRI)
               (PROGN
                (PRIN2
                 "is not read into REDUCE but also not computed in REDUCE --> different try.")
                NIL)
               (TERPRI)
               NIL)))
            NIL))
          (T
           (PROGN
            (SETQ S (ADDSQ (MULTSQ P PMULT) (NEGSQ (MULTSQ Q QMULT))))
            (COND
             ((NULL ALL_RATIONAL_KERNELS) (SETQ S (SIMP* (LIST '*SQ S NIL)))))
            (COND
             (FORM_COMP
              (PROGN
               (SETQ T4 (TIME))
               (SETQ SS (BLDMSG_INTERNAL "rm %w" (LIST FORMGETFILE)))
               (SYSTEM SS)
               (SETQ FORM_EQN_ON_DISK (CDR FORM_EQN_ON_DISK))))))))
         (COND
          (NIL
           (COND
            (FORM_COMP
             (PROGN
              (COND ((EQUAL S SS) (PROGN (PRIN2 "FORM is CORRECT!") NIL))
                    (T (PROGN (PRIN2 "FORM was WRONG!") NIL)))
              (TERPRI)
              (COND
               ((SQZEROP (ADDSQ S (NEGSQ SS)))
                (PROGN (PRIN2 "FORM is CORRECT!") NIL))
               (T (PROGN (PRIN2 "FORM was WRONG!") NIL)))
              (TERPRI)
              (COND
               ((SQZEROP (ADDSQ S (NEGSQ (RESIMP SS))))
                (PROGN (PRIN2 "FORM is CORRECT!") NIL))
               (T (PROGN (PRIN2 "FORM was WRONG!") NIL)))
              (TERPRI)
              (COND
               ((SQZEROP (RESIMP (ADDSQ S (NEGSQ SS))))
                (PROGN (PRIN2 "FORM is CORRECT!") NIL))
               (T (PROGN (PRIN2 "FORM was WRONG!") NIL)))
              (TERPRI)
              NIL)))))
         (SETQ FCSP (ADDSQ (MULTSQ FCPP PMULT) (NEGSQ (MULTSQ FCQP QMULT))))
         (SETQ FCSQ (ADDSQ (MULTSQ FCPQ PMULT) (NEGSQ (MULTSQ FCQQ QMULT))))
         (COND ((OR ONCE T) (SETQ DGS 0))
               (T
                (PROGN
                 (SETQ LCO (COEFF1 (LIST '*SQ S T) (LIST '*SQ XSQ T) NIL))
                 (SETQ DGS HIPOW*)
                 (SETQ LCO (NTH (CDR LCO) (ADD1 DGS)))
                 (SETQ LCO
                         (COND
                          ((AND (PAIRP LCO) (EQUAL (CAR LCO) '*SQ)) (CADR LCO))
                          (T (SIMP LCO))))
                 (SETQ LTS (MULTSQ LCO (MKSQ X DGS)))
                 (COND
                  ((EQUAL FLG T)
                   (PROGN
                    (SETQ P S)
                    (SETQ LTP LTS)
                    (SETQ DGP DGS)
                    (SETQ FCPP FCSP)
                    (SETQ FCPQ FCSQ)
                    (COND ((GREATERP DGQ DGP) (SETQ FLG NIL)))))
                  (T
                   (PROGN
                    (SETQ Q S)
                    (SETQ LTQ LTS)
                    (SETQ DGQ DGS)
                    (SETQ FCQP FCSP)
                    (SETQ FCQQ FCSQ)
                    (COND ((GREATERP DGP DGQ) (SETQ FLG T))))))
                 (SETQ QUOTI (MULTSQ LTP (INVSQ LTQ)))
                 (SETQ QMULT (CONS (CAR QUOTI) 1))
                 (SETQ PMULT (CONS (CDR QUOTI) 1))))))
        (GO WHILELABEL))
      (RETURN (LIST S FCSP FCSQ)))) 
(PUT 'DEC_NEW_EQUATION 'NUMBER-OF-ARGS 3) 
(PUT 'DEC_NEW_EQUATION 'DEFINED-ON-LINE '1251) 
(PUT 'DEC_NEW_EQUATION 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_NEW_EQUATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_NEW_EQUATION (L RL PDES)
    (PROG (LD F IP IQ S NVL LCO P DDP DDPCP LDP LDPSQ LTP DGP Q DDQ DDQCP LDQ
           LDQSQ LTQ DGQ H ONCE COMMFAC PMULT QMULT)
      (SETQ LD (CADDR L))
      (SETQ F (COND ((PAIRP LD) (CADR LD)) (T LD)))
      (SETQ IP (CADAR L))
      (SETQ IQ (CADADR L))
      (SETQ S (CADDDR L))
      (SETQ ONCE (CAR (CDDDDR L)))
      (SETQ DDP (CAAR L))
      (SETQ DDPCP DDP)
      (SETQ DDQ (CAADR L))
      (SETQ DDQCP DDQ)
      (SETQ P (GET DDP 'SQVAL))
      (SETQ Q (GET DDQ 'SQVAL))
      (COND
       (RECORD_HIST
        (PROGN
         (SETQ NVL (GET DDP 'NVARS))
         (COND ((LESSP (GET DDQ 'NVARS) NVL) (SETQ NVL (GET DDQ 'NVARS))))
         (COND ((EQUAL S DDP) (SETQ DDP (GET DDP 'HISTRY_)))
               ((EQUAL S DDQ) (SETQ DDQ (GET DDQ 'HISTRY_)))))))
      (SETQ DDP (SIMP DDP))
      (SETQ DDQ (SIMP DDQ))
      (COND
       ((AND PRINT_ (OR (AND (NULL RL) TR_DECOUPLE) (AND RL TR_REDLENGTH)))
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "  first pde ") (PRIN2 (CAAR L)) (PRIN2 ": ") NIL)
         (TYPEEQ (CAAR L))
         (COND (IP (PROGN (PRIN2 "is diff. wrt. ") (PRIN2 IP) (PRIN2 ",") NIL))
               (T (PROGN (PRIN2 "is not differentiated,") NIL)))
         (PROGN (PRIN2 "  second pde ") (PRIN2 (CAADR L)) (PRIN2 ": ") NIL)
         (TYPEEQ (CAADR L))
         (COND (IQ (PROGN (PRIN2 "is diff. wrt. ") (PRIN2 IQ) (PRIN2 " ") NIL))
               (T (PROGN (PRIN2 "is not differentiated, ") NIL)))
         (PROGN (PRIN2 "to eliminate ") NIL)
         (MATHPRINT LD)
         NIL)))
      (COND ((ATOM LD) (SETQ LDP LD))
            (T
             (PROGN
              (SETQ LDP (CADR LD))
              (COND
               ((CADDAR L) (SETQ LDP (CONS 'DF (CONS LDP (CADDAR L)))))))))
      (SETQ LDPSQ (MKSQ LDP 1))
      (SETQ LCO (COEFF1 (LIST '*SQ P T) (LIST '*SQ LDPSQ T) NIL))
      (SETQ DGP HIPOW*)
      (COND
       ((NULL IP)
        (PROGN
         (SETQ LCO (NTH (CDR LCO) (ADD1 DGP)))
         (SETQ LCO
                 (COND ((AND (PAIRP LCO) (EQUAL (CAR LCO) '*SQ)) (CADR LCO))
                       (T (SIMP LCO))))
         (SETQ LTP (MULTSQ LCO (MKSQ LDP DGP)))
         NIL))
       (T
        (PROGN
         (SETQ DGP 1)
         (SETQ LTP
                 (MULTSQ (DIFFSQ P (CAAAR (CAR (MKSQ LDP 1))))
                         (MULTIPLE_DIFFSQ LDPSQ IP)))
         (SETQ P (MULTIPLE_DIFFSQ P IP))
         (COND (RECORD_HIST (SETQ DDP (MULTIPLE_DIFFSQ DDP IP)))))))
      (COND ((ATOM LD) (SETQ LDQ LD))
            (T
             (PROGN
              (SETQ LDQ (CADR LD))
              (COND
               ((CADDAR (CDR L))
                (SETQ LDQ (CONS 'DF (CONS LDQ (CADDAR (CDR L))))))))))
      (SETQ LDQSQ (MKSQ LDQ 1))
      (SETQ LCO (COEFF1 (LIST '*SQ Q T) (LIST '*SQ LDQSQ T) NIL))
      (SETQ DGQ HIPOW*)
      (COND
       ((NULL IQ)
        (PROGN
         (SETQ LCO (NTH (CDR LCO) (ADD1 DGQ)))
         (SETQ LCO
                 (COND ((AND (PAIRP LCO) (EQUAL (CAR LCO) '*SQ)) (CADR LCO))
                       (T (SIMP LCO))))
         (SETQ LTQ (MULTSQ LCO (MKSQ LDQ DGQ)))
         NIL))
       (T
        (PROGN
         (SETQ DGQ 1)
         (SETQ LTQ
                 (MULTSQ (DIFFSQ Q (CAAAR (CAR (MKSQ LDQ 1))))
                         (MULTIPLE_DIFFSQ LDQSQ IQ)))
         (SETQ Q (MULTIPLE_DIFFSQ Q IQ))
         (COND (RECORD_HIST (SETQ DDQ (MULTIPLE_DIFFSQ DDQ IQ)))))))
      (SETQ COMMFAC (ERR_CATCH_GCD (LIST '*SQ LTP T) (LIST '*SQ LTQ T)))
      (SETQ COMMFAC
              (COND
               ((AND (PAIRP COMMFAC) (EQUAL (CAR COMMFAC) '*SQ))
                (CADR COMMFAC))
               (T (SIMP COMMFAC))))
      (SETQ QMULT (MULTSQ LTP (INVSQ COMMFAC)))
      (SETQ PMULT (MULTSQ LTQ (INVSQ COMMFAC)))
      (COND
       ((SETQ H (NOT_NECC_AND_SUFF_EQN L PMULT QMULT PDES LTP LTQ))
        (COND ((ATOM H) (SETQ S NIL)) (T (RETURN 0)))))
      (RETURN
       (COND ((AND (CAR (CDDDDR L)) (NULL S)) NIL)
             ((SETQ H
                      (ERR_CATCH_ELIMIN DDPCP P LTP PMULT DGP DDQCP Q LTQ QMULT
                       DGQ LD ONCE))
              (LIST H S DDPCP DDQCP DDP DDQ LD))
             (T NIL))))) 
(PUT 'DEC_REDUCTION 'NUMBER-OF-ARGS 6) 
(PUT 'DEC_REDUCTION 'DEFINED-ON-LINE '1389) 
(PUT 'DEC_REDUCTION 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_REDUCTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_REDUCTION (H PDES FTEM VL RL ORDERING)
    (PROG (S P Q DDP DDQ LD LEN A IP IQ F)
      (SETQ S (CADR H))
      (SETQ P (CADDR H))
      (SETQ Q (CADDDR H))
      (SETQ DDP (NTH H 5))
      (SETQ DDQ (NTH H 6))
      (SETQ LD (NTH H 7))
      (COND ((PAIRP LD) (SETQ F (CADR LD))) (T (SETQ F LD)))
      (SETQ H (CAR H))
      (COND
       ((OR (AND (NULL RL) TR_DECOUPLE) (AND RL TR_REDLENGTH))
        (PROGN
         (TERPRI)
         (PROGN
          (PRIN2 P)
          (PRIN2 " (resp its derivative) is multiplied with")
          NIL)
         (TERPRI)
         (SETQ A (CADR H))
         (SETQ LEN (DELENGTHSQ A))
         (COND ((AND PRINT_ (LESSP LEN PRINT_)) (MATHPRINT (LIST '*SQ A T)))
               (T
                (PROGN
                 (PROGN
                  (PRIN2 "expr. with ")
                  (PRIN2 LEN)
                  (PRIN2 " terms.")
                  NIL)
                 (TERPRI))))
         (PROGN
          (PRIN2 Q)
          (PRIN2 " (resp its derivative) is multiplied with")
          NIL)
         (TERPRI)
         (SETQ A (CADDR H))
         (SETQ LEN (DELENGTHSQ A))
         (COND ((AND PRINT_ (LESSP LEN PRINT_)) (MATHPRINT (LIST '*SQ A T)))
               (T
                (PROGN
                 (PROGN
                  (PRIN2 "expr. with ")
                  (PRIN2 LEN)
                  (PRIN2 " terms.")
                  NIL)
                 (TERPRI)))))))
      (COND
       ((AND (NULL RL) (NULL EXPERT_MODE) (CAR H) S
             (OR (NULL STRUC_EQN) (ATOM LD)))
        (PROGN
         (SETQ LEN (NO_OF_TERMS (CAR H)))
         (COND ((AND (PAIRP LD) (EQUAL (CAR LD) 'DF)) (SETQ LD (CDR LD)))
               (T (SETQ LD (LIST LD))))
         (COND
          ((OR
            (AND (EQUAL S P) (NEQ LD (CAAR (GET P 'DERIVS)))
                 (GREATERP LEN (GET P 'TERMS)))
            (AND (EQUAL S Q) (NEQ LD (CAAR (GET Q 'DERIVS)))
                 (GREATERP LEN (GET Q 'TERMS))))
           (RETURN
            (PROGN
             (COND
              (PRINT_
               (PROGN
                (PROGN
                 (PRIN2 "The tried reduction of a non-leading derivative")
                 NIL)
                (TERPRI)
                (PROGN
                 (PRIN2 "would have only increased the equation's length.")
                 NIL)
                (TERPRI))))
             (ADD_BOTH_DEC_WITH ORDERING P Q RL)
             (LIST NIL)))))
         (COND ((CDR LD) (SETQ LD (CONS 'DF LD))) (T (SETQ LD (CAR LD))))
         NIL)))
      (COND
       ((CAR H)
        (COND
         ((AND (SQZEROP (CAR H)) (NULL RL))
          (PROGN
           (COND
            (PRINT_
             (PROGN
              (TERPRI)
              (PROGN (PRIN2 " An identity 0=0 results. ") NIL))))
           (COND
            ((AND (NULL IP) (NULL IQ) (NULL S)
                  (OR *BATCH_MODE (GEQ BATCHCOUNT_ STEPCOUNTER_)))
             (PROGN
              (SETQ A PROC_LIST_)
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND A (NEQ (CAR A) 8) (NEQ (CAR A) 30))) (RETURN NIL)))
                (SETQ A (CDR A))
                (GO WHILELABEL))
              (COND
               ((AND A (EQUAL (CAR A) 8))
                (SETQ TO_DO_LIST
                        (CONS
                         (LIST 'FACTORIZE_ANY
                               (LIST (COND ((GET P 'FAC) P) (T Q))))
                         TO_DO_LIST))))
              NIL)))
           (SETQ A NIL)
           (ADD_BOTH_DEC_WITH ORDERING P Q RL)
           NIL))
         (T
          (PROGN
           (SETQ A
                   (MKEQSQ (CAR H) NIL NIL FTEM VL ALLFLAGS_ T (LIST 0)
                    (PREPSQ
                     (ADDSQ (MULTSQ (CADR H) DDP) (MULTSQ (CADDR H) DDQ)))
                    PDES))
           (COND
            ((AND PRINT_
                  (OR (AND (NULL RL) TR_DECOUPLE) (AND RL TR_REDLENGTH)))
             (PROGN
              (TERPRI)
              (MATHPRINT (REVAL1 (LIST 'EQUAL A (GET A 'HISTRY_)) T))))))))))
      (COND
       ((AND RECORD_HIST (CAR H) (SQZEROP (CAR H)))
        (NEW_IDTY (PREPSQ (ADDSQ (MULTSQ (CADR H) DDP) (MULTSQ (CADDR H) DDQ)))
         PDES T)))
      (COND
       (PRINT_
        (PROGN
         (PROGN (PRIN2 "Eliminate ") NIL)
         (MATHPRINT LD)
         (PROGN
          (PRIN2 "from ")
          (PRIN2 (COND (IP (CONS 'DF (CONS P IP))) (T P)))
          (PRIN2 " and ")
          (PRIN2 (COND (IQ (CONS 'DF (CONS Q IQ))) (T Q)))
          (PRIN2 ". ")
          NIL)
         (COND
          (A
           (PROGN
            (COND
             (S
              (PROGN
               (PROGN (PRIN2 S) (PRIN2 ": ") NIL)
               (TERPRI)
               (TYPEEQ S)
               (PROGN (PRIN2 "is replaced by ") (PRIN2 A) (PRIN2 ": ") NIL)))
             (T (PROGN (PRIN2 A) (PRIN2 " is added: ") NIL)))
            (TERPRI)
            (TYPEEQ A)))
          (S (PROGN (PROGN (PRIN2 S) (PRIN2 " is deleted.") NIL) (TERPRI))))
         NIL)))
      (COND ((NULL S) (ADD_BOTH_DEC_WITH ORDERING P Q RL))
            (T (PROGN (DROP_PDE S (COND (A (CONS A PDES)) (T PDES)) NIL))))
      (RETURN (LIST A)))) 
(PUT 'DEC_FCT_CHECK 'NUMBER-OF-ARGS 2) 
(PUT 'DEC_FCT_CHECK 'DEFINED-ON-LINE '1589) 
(PUT 'DEC_FCT_CHECK 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_FCT_CHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEC_FCT_CHECK (A L)
    (PROG (FT N)
      (SETQ FT (GET A 'FCTS))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FT L)) (RETURN NIL)))
        (PROGN
         (COND
          ((FLAGP (CAR L) 'TO_DECOUP)
           (SETQ FT (SETDIFF_ACCORDING_TO FT (GET (CAR L) 'FCTS) FTEM_))))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (SETQ N (GET A 'NVARS))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND FT (LEQ N (LENGTH (FCTARGS (CAR FT)))))) (RETURN NIL)))
        (SETQ FT (CDR FT))
        (GO WHILELABEL))
      (COND (FT (REMFLAG (LIST A) 'TO_DECOUP)))
      (RETURN FT))) 
(PUT 'NOT_NECC_AND_SUFF_EQN 'NUMBER-OF-ARGS 6) 
(PUT 'NOT_NECC_AND_SUFF_EQN 'DEFINED-ON-LINE '1604) 
(PUT 'NOT_NECC_AND_SUFF_EQN 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'NOT_NECC_AND_SUFF_EQN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NOT_NECC_AND_SUFF_EQN (L PMULT QMULT PDES LTP LTQ)
    (COND (LIN_PROBLEM NIL)
          (T
           (PROG (S H HH MULTPL SQU Q PDESCP FCTR SUBTRAHEND)
             (SETQ S (CADDDR L))
             (COND
              ((EQUAL S (CAAR L))
               (PROGN (SETQ H (CADR L)) (SETQ MULTPL PMULT)))
              (T (PROGN (SETQ H (CAR L)) (SETQ MULTPL QMULT))))
             (RETURN
              (COND
               ((OR (NULL S) (GET (CAR H) 'LINEAR_)
                    (AND (CADR H) (NULL (FLAGP (CAR H) 'TO_SEPARANT))))
                NIL)
               ((CAN_NOT_BECOME_ZEROSQ MULTPL (GET (CAR H) 'FCTS)) NIL)
               ((OR (NULL DEC_DEPTH_FIRST_SEARCH) (CAR (CDDDDR L))) 0)
               (T
                (PROGN
                 (SETQ HH (SIMPLIFYSQ MULTPL (SMEMBERL FTEM_ MULTPL) T NIL T))
                 (SETQ SQU HH)
                 (SETQ PDESCP NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (AND SQU (NULL PDESCP))) (RETURN NIL)))
                   (PROGN
                    (SETQ PDESCP PDES)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND PDESCP
                              (NEQ (GET (CAR PDESCP) 'SQVAL) (CAR SQU))))
                        (RETURN NIL)))
                      (SETQ PDESCP (CDR PDESCP))
                      (GO WHILELABEL))
                    (SETQ SQU (CDR SQU)))
                   (GO WHILELABEL))
                 (SETQ SQU (CAR HH))
                 (PROG (Q)
                   (SETQ Q (CDR HH))
                  LAB
                   (COND ((NULL Q) (RETURN NIL)))
                   ((LAMBDA (Q) (SETQ SQU (MULTSQ Q SQU))) (CAR Q))
                   (SETQ Q (CDR Q))
                   (GO LAB))
                 (COND
                  ((NULL PDESCP)
                   (PROGN
                    (SETQ PDESCP PDES)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (AND PDESCP (NEQ (GET (CAR PDESCP) 'SQVAL) SQU)))
                        (RETURN NIL)))
                      (SETQ PDESCP (CDR PDESCP))
                      (GO WHILELABEL))
                    NIL)))
                 (COND
                  (PDESCP
                   (PROGN
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN
                        (PRIN2 "When attempting to reduce equation ")
                        (PRIN2 S)
                        (PRIN2 " with ")
                        (PRIN2 (CAR H))
                        (PRIN2 " it was realized that the multiplier ")
                        NIL)
                       (MATHPRINT (LIST '*SQ MULTPL T))
                       (PROGN
                        (PRIN2 " to ")
                        (PRIN2 S)
                        (PRIN2 " simplified to ")
                        NIL)
                       (MATHPRINT (LIST '*SQ SQU T))
                       (PROGN
                        (PRIN2 "must vanish due to equation ")
                        (PRIN2 (CAR PDESCP))
                        (PRIN2 ". Therefore ")
                        (PRIN2 (CAR H))
                        (PRIN2 " is simplified next.")
                        NIL)
                       (TERPRI))))
                    (SETQ SUBTRAHEND (COND ((EQUAL S (CAAR L)) LTQ) (T LTP)))
                    (SETQ SQU (ADDSQ (GET (CAR H) 'SQVAL) (NEGSQ SUBTRAHEND)))
                    (SETQ FCTR
                            (PREPSQ
                             (MULTSQ SUBTRAHEND
                                     (INVSQ (GET (CAR PDESCP) 'SQVAL)))))
                    (COND (PRINT_ (PROGN (PROGN (PRIN2 "") NIL) NIL)))
                    (SETQ TO_DO_LIST
                            (CONS
                             (LIST 'REPLACE_EQUATION
                                   (LIST (CAR H) NIL SQU
                                         (LIST 'DIFFERENCE (CAR H)
                                               (LIST 'TIMES FCTR
                                                     (CAR PDESCP)))))
                             TO_DO_LIST))
                    (LIST 1)))
                  (T
                   (PROGN
                    (COND
                     (PRINT_
                      (PROGN
                       (PROGN (PRIN2 "To reduce the leading derivative") NIL)
                       (TERPRI)
                       (MATHPRINT (CADDR L))
                       (PROGN
                        (PRIN2 "in ")
                        (PRIN2 S)
                        (PRIN2 " using ")
                        (PRIN2 (CAR H))
                        (PRIN2 " a case distinction will be made.")
                        NIL)
                       (TERPRI))))
                    (SETQ TO_DO_LIST
                            (CONS (LIST 'SPLIT_INTO_CASES MULTPL)
                                  TO_DO_LIST))))))))))))) 
(PUT 'DEC_ONE_STEP 'NUMBER-OF-ARGS 6) 
(PUT 'DEC_ONE_STEP 'DEFINED-ON-LINE '1697) 
(PUT 'DEC_ONE_STEP 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_ONE_STEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_ONE_STEP (PDES L FTEM VL HP ORDERING)
    (PROG (L0 L1 L2 RULI CNTR MOD_SWITCHED)
      (COND ((NOT EXPERT_MODE) (SETQ L0 L))
            (T
             (PROGN
              (SETQ L0 (SELECTPDES L 2))
              (DROP_DEC_WITH (CAR L0) (CADR L0) NIL)
              (DROP_DEC_WITH (CADR L0) (CAR L0) NIL)
              NIL)))
      (COND
       ((AND MODULAR_COMP (NULL *MODULAR))
        (PROGN (ON (LIST 'MODULAR)) (SETQ MOD_SWITCHED T))))
      (SETQ RULI (START_LET_RULES))
      (SETQ CNTR 0)
     AGAIN
      (SETQ L1 (DEC_AND_FCT_SELECT L0 VL NIL HP ORDERING))
      (COND ((NULL L1) (SETQ L NIL))
            ((IN_CYCLE
              (LIST 30 STEPCOUNTER_ (GET (CAAR L1) 'PRINTLENGTH)
                    (GET (CAADR L1) 'PRINTLENGTH) (CADDR L1)
                    (GET (CADDDR L1) 'PRINTLENGTH)
                    (LENGTH (GET (CADDDR L1) 'FCTS))))
             (PROGN
              (ADD_BOTH_DEC_WITH ORDERING (CAAR L1) (CAADR L1) NIL)
              (GO AGAIN)
              NIL))
            ((NULL (SETQ L2 (DEC_NEW_EQUATION L1 NIL PDES)))
             (PROGN
              (SETQ L NIL)
              (ADD_BOTH_DEC_WITH ORDERING (CAAR L1) (CAADR L1) NIL)
              (SETQ CNTR (ADD1 CNTR))
              (COND ((LESSP CNTR 4) (GO AGAIN)))))
            ((NOT (PAIRP L2)) NIL)
            ((NULL (SETQ L2 (DEC_REDUCTION L2 PDES FTEM VL NIL ORDERING)))
             (SETQ L NIL))
            (T
             (PROGN
              (SETQ L PDES)
              (COND
               ((AND (CADDDR L1) (EQUAL (GET (CADDDR L1) 'SQVAL) NIL))
                (SETQ L (DELETE (CADDDR L1) L))))
              (PROG (A)
                (SETQ A L2)
               LAB
                (COND ((NULL A) (RETURN NIL)))
                ((LAMBDA (A) (COND (A (PROGN (SETQ L (EQINSERT A L)) NIL))))
                 (CAR A))
                (SETQ A (CDR A))
                (GO LAB)))))
      (STOP_LET_RULES RULI)
      (COND (MOD_SWITCHED (OFF (LIST 'MODULAR))))
      (RETURN L))) 
(PUT 'DEC_TRY_TO_RED_LEN 'NUMBER-OF-ARGS 4) 
(PUT 'DEC_TRY_TO_RED_LEN 'DEFINED-ON-LINE '1761) 
(PUT 'DEC_TRY_TO_RED_LEN 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_TRY_TO_RED_LEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_TRY_TO_RED_LEN (PDES PDES_TO_CHOOSE_FROM VL ORDERING)
    (PROG (L1 L2 L3 P Q S CNTR)
      (SETQ CNTR 0)
     AGAIN
      (SETQ L1 (DEC_AND_FCT_SELECT PDES_TO_CHOOSE_FROM VL T NIL ORDERING))
      (COND
       (L1
        (PROGN
         (COND
          ((IN_CYCLE
            (LIST 27 STEPCOUNTER_ (GET (CAAR L1) 'PRINTLENGTH)
                  (GET (CAADR L1) 'PRINTLENGTH) (CADDR L1)
                  (GET (CADDDR L1) 'PRINTLENGTH)
                  (LENGTH (GET (CADDDR L1) 'FCTS))))
           (PROGN
            (ADD_BOTH_DEC_WITH ORDERING (CAAR L1) (CAADR L1) T)
            (GO AGAIN)
            NIL)))
         (SETQ L2 (DEC_NEW_EQUATION L1 T PDES))
         (SETQ P (CAAR L1))
         (SETQ Q (CAADR L1))
         (SETQ S (CADDDR L1))
         (SETQ L3 (UNION (GET P 'FCTS) (GET Q 'FCTS)))
         (COND
          ((OR (NULL L2)
               (GREATERP (PDEWEIGHT (CAAR L2) L3) (GET (CADDDR L1) 'LENGTH))
               (AND (EQUAL S P) (NOT (CAN_NOT_BECOME_ZEROSQ (CADAR L2) L3)))
               (AND (EQUAL S Q) (NOT (CAN_NOT_BECOME_ZEROSQ (CADDAR L2) L3))))
           (PROGN
            (COND ((NULL L2) (SETQ CNTR (ADD1 CNTR))))
            (SETQ L2 NIL)
            (ADD_BOTH_DEC_WITH ORDERING P Q T)
            (SETQ LAST_STEPS (CDR LAST_STEPS))
            (COND ((LESSP CNTR 20000000) (GO AGAIN)))))))))
      (RETURN L2))) 
(PUT 'ERR_CATCH_RED_LEN 'NUMBER-OF-ARGS 4) 
(PUT 'ERR_CATCH_RED_LEN 'DEFINED-ON-LINE '1808) 
(PUT 'ERR_CATCH_RED_LEN 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'ERR_CATCH_RED_LEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_RED_LEN (A1 A2 A3 A4)
    (PROG (H BAK KERNLIST*BAK KORD*BAK BAKUP_BAK)
      (SETQ BAK MAX_GC_COUNTER)
      (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_RED_LEN))
      (SETQ KERNLIST*BAK KERNLIST*)
      (SETQ KORD*BAK KORD*)
      (SETQ BAKUP_BAK BACKUP_)
      (SETQ BACKUP_ 'MAX_GC_RED_LEN)
      ((LAMBDA (*PROTFG)
         (SETQ H
                 (ERRORSET
                  (LIST 'DEC_TRY_TO_RED_LEN (MKQUOTE A1) (MKQUOTE A2)
                        (MKQUOTE A3) (MKQUOTE A4))
                  NIL NIL)))
       T)
      (SETQ KERNLIST* KERNLIST*BAK)
      (SETQ KORD* KORD*BAK)
      (SETQ ERFG* NIL)
      (SETQ MAX_GC_COUNTER BAK)
      (SETQ BACKUP_ BAKUP_BAK)
      (RETURN (COND ((ERRORP H) NIL) (T (CAR H)))))) 
(PUT 'DEC_AND_RED_LEN_ONE_STEP 'NUMBER-OF-ARGS 4) 
(PUT 'DEC_AND_RED_LEN_ONE_STEP 'DEFINED-ON-LINE '1825) 
(PUT 'DEC_AND_RED_LEN_ONE_STEP 'DEFINED-IN-FILE 'CRACK/CRDEC.RED) 
(PUT 'DEC_AND_RED_LEN_ONE_STEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEC_AND_RED_LEN_ONE_STEP (PDES FTEM VL ORDERING)
    (PROG (L L1 L2 L3 RULI)
      (SETQ L PDES)
      (COND ((NOT EXPERT_MODE) (SETQ L1 L))
            (T
             (PROGN
              (SETQ L1 (SELECTPDES L 2))
              (DROP_DEC_WITH (CAR L1) (CADR L1) T)
              (DROP_DEC_WITH (CADR L1) (CAR L1) T)
              NIL)))
      (SETQ RULI (START_LET_RULES))
     AGAIN
      (SETQ L2 (ERR_CATCH_RED_LEN PDES L1 VL ORDERING))
      (COND ((NULL L2) (RETURN NIL)))
      (COND
       ((SETQ L3 (DEC_REDUCTION L2 PDES FTEM VL T ORDERING))
        (PROGN
         (SETQ L (DELETE (CADR L2) L))
         (PROG (A)
           (SETQ A L3)
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (COND (A (PROGN (SETQ L (EQINSERT A L)) NIL))))
            (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         NIL))
       (T
        (PROGN
         (SETQ LAST_STEPS (CDR LAST_STEPS))
         (COND ((NOT EXPERT_MODE) (PROGN (SETQ L1 L) (GO AGAIN)))))))
      (STOP_LET_RULES RULI)
      (RETURN L))) 
(ENDMODULE) 