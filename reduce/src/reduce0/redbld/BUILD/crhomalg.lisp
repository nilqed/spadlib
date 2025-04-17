(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HOMALGSYS)) 
(FLUID '(TR_HOM_ALG)) 
(SETQ TR_HOM_ALG T) 
(PUT 'LINEARIZE_BI_LIN 'NUMBER-OF-ARGS 1) 
(PUT 'LINEARIZE_BI_LIN 'DEFINED-ON-LINE '37) 
(PUT 'LINEARIZE_BI_LIN 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'LINEARIZE_BI_LIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINEARIZE_BI_LIN (ARGLIST)
    (COND
     ((NULL FHOM_)
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2
           "To use this module, you need functions (in fhom_) that occur homogeneously.")
          NIL)
         (TERPRI)))
       (T NIL)))
     ((OR (NULL (CAR ARGLIST)) (NULL (CDAR ARGLIST)))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2 "To use this module, you need at least 2 equations.")
          NIL)
         (TERPRI)))
       (T NIL)))
     (T
      (PROG (PDES NEWPDES1 SB SB2 SB3 H PF NEWP TM TMCP S PRINT_BAK FCP F1 F2
             FHOMTEM)
        (SETQ PRINT_BAK PRINT_)
        (SETQ PRINT_ NIL)
        (SETQ PDES (CAR ARGLIST))
        (SETQ FHOMTEM (INTERSECTION FHOM_ FTEM_))
        (SETQ FHOMTEM (KERNEL_SORT FHOMTEM))
        (PROGN (PRIN2 "Formulating a substitution list for squares") NIL)
        (TERPRI)
        (SETQ SB2
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F FHOMTEM)
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F)
                                      (PROGN
                                       (SETQ H (INTERN (GENSYM)))
                                       (SETQ FTEM_ (FCTINSERT H FTEM_))
                                       (SETQ FLIN_ (CONS H FLIN_))
                                       (SETQ FLIN_
                                               (SORT_ACCORDING_TO FLIN_ FTEM_))
                                       (SETQ NEWP
                                               (ADDSQ (MKSQ F 2)
                                                      (NEGSQ (MKSQ H 1))))
                                       (SETQ NEWPDES1
                                               (CONS
                                                (MKEQSQ NEWP NIL NIL FTEM_ VL_
                                                 ALLFLAGS_ T (LIST 0) NIL PDES)
                                                NEWPDES1))
                                       (CONS F H)))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (F)
                              (PROGN
                               (SETQ H (INTERN (GENSYM)))
                               (SETQ FTEM_ (FCTINSERT H FTEM_))
                               (SETQ FLIN_ (CONS H FLIN_))
                               (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
                               (SETQ NEWP
                                       (ADDSQ (MKSQ F 2) (NEGSQ (MKSQ H 1))))
                               (SETQ NEWPDES1
                                       (CONS
                                        (MKEQSQ NEWP NIL NIL FTEM_ VL_
                                         ALLFLAGS_ T (LIST 0) NIL PDES)
                                        NEWPDES1))
                               (CONS F H)))
                            (CAR F))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROGN (PRIN2 "Formulating a substitution list for products") NIL)
        (TERPRI)
        (SETQ FCP FHOMTEM)
        (PROG ()
         WHILELABEL
          (COND ((NOT (CDR FCP)) (RETURN NIL)))
          (PROGN
           (SETQ SB3
                   (PROG (F FORALL-RESULT FORALL-ENDPTR)
                     (SETQ F (CDR FCP))
                     (COND ((NULL F) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (F)
                                         (PROGN
                                          (SETQ H (INTERN (GENSYM)))
                                          (SETQ FTEM_ (FCTINSERT H FTEM_))
                                          (SETQ FLIN_ (CONS H FLIN_))
                                          (SETQ FLIN_
                                                  (SORT_ACCORDING_TO FLIN_
                                                   FTEM_))
                                          (SETQ NEWP
                                                  (ADDSQ
                                                   (MULTSQ (MKSQ (CAR FCP) 1)
                                                           (MKSQ F 1))
                                                   (NEGSQ (MKSQ H 1))))
                                          (SETQ NEWPDES1
                                                  (CONS
                                                   (MKEQSQ NEWP NIL NIL FTEM_
                                                    VL_ ALLFLAGS_ T (LIST 0)
                                                    NIL PDES)
                                                   NEWPDES1))
                                          (CONS F H)))
                                       (CAR F))
                                      NIL)))
                    LOOPLABEL
                     (SETQ F (CDR F))
                     (COND ((NULL F) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (F)
                                 (PROGN
                                  (SETQ H (INTERN (GENSYM)))
                                  (SETQ FTEM_ (FCTINSERT H FTEM_))
                                  (SETQ FLIN_ (CONS H FLIN_))
                                  (SETQ FLIN_ (SORT_ACCORDING_TO FLIN_ FTEM_))
                                  (SETQ NEWP
                                          (ADDSQ
                                           (MULTSQ (MKSQ (CAR FCP) 1)
                                                   (MKSQ F 1))
                                           (NEGSQ (MKSQ H 1))))
                                  (SETQ NEWPDES1
                                          (CONS
                                           (MKEQSQ NEWP NIL NIL FTEM_ VL_
                                            ALLFLAGS_ T (LIST 0) NIL PDES)
                                           NEWPDES1))
                                  (CONS F H)))
                               (CAR F))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ SB (CONS (CONS (CAR FCP) SB3) SB))
           (SETQ FCP (CDR FCP))
           NIL)
          (GO WHILELABEL))
        (PROGN (PRIN2 "Conversion of equations") NIL)
        (TERPRI)
        (PROG (P)
          (SETQ P PDES)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (COND
              ((AND (GET P 'FCT_HOM) (SETQ H (GET P 'HOM_DEG))
                    (OR (EQUAL (CAR H) 2) (EQUAL (CAR H) 0))
                    (OR (EQUAL (CADR H) 2) (EQUAL (CADR H) 0)))
               (PROGN
                (SETQ NEWP (SIMP 0))
                (SETQ PF (CAR (GET P 'SQVAL)))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT PF) (RETURN NIL)))
                  (PROGN
                   (SETQ TM (FIRST_TERM_SF PF))
                   (SETQ PF (ADDF PF (NEGF TM)))
                   (SETQ TMCP TM)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT (NOT (MEMBER (CAAAR TMCP) FHOMTEM))) (RETURN NIL)))
                     (SETQ TMCP (CDAR TMCP))
                     (GO WHILELABEL))
                   (SETQ F1 (CAAAR TMCP))
                   (COND
                    ((EQUAL (CDAAR TMCP) 2)
                     (PROGN
                      (SETQ S (ASSOC F1 SB2))
                      (SETQ NEWP
                              (ADDSQ NEWP
                                     (MULTSQ
                                      (MULTSQ (CONS TM 1) (INVSQ (MKSQ F1 2)))
                                      (MKSQ (CDR S) 1))))
                      NIL))
                    (T
                     (PROGN
                      (SETQ TMCP (CDAR TMCP))
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (NOT (MEMBER (CAAAR TMCP) FHOMTEM)))
                          (RETURN NIL)))
                        (SETQ TMCP (CDAR TMCP))
                        (GO WHILELABEL))
                      (SETQ F2 (CAAAR TMCP))
                      (SETQ S (ASSOC F1 SB))
                      (SETQ S (ASSOC F2 (CDR S)))
                      (SETQ NEWP
                              (ADDSQ NEWP
                                     (MULTSQ
                                      (MULTSQ
                                       (MULTSQ (CONS TM 1) (INVSQ (MKSQ F1 1)))
                                       (INVSQ (MKSQ F2 1)))
                                      (MKSQ (CDR S) 1))))
                      NIL))))
                  (GO WHILELABEL))
                (SETQ PDES (DROP_PDE P PDES NIL))
                (SETQ PDES
                        (EQINSERT
                         (MKEQSQ NEWP NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0)
                          NIL PDES)
                         PDES))
                NIL))))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (SETQ PDES (APPEND NEWPDES1 PDES))
        (SETQ PRINT_ PRINT_BAK)
        (RETURN (CONS PDES (CDR ARGLIST))))))) 
(PUT 'BI_LIN_EXPT 'NUMBER-OF-ARGS 1) 
(FLAG '(BI_LIN_EXPT) 'OPFN) 
(PUT 'BI_LIN_EXPT 'DEFINED-ON-LINE '134) 
(PUT 'BI_LIN_EXPT 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'BI_LIN_EXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BI_LIN_EXPT (P)
    (COND ((EVALEQUAL (AEVAL P) 1) 0)
          ((EVALLESSP (AEVAL (LIST 'ARGLENGTH P)) 2) 1)
          ((AND (EVALEQUAL (AEVAL (LIST 'ARGLENGTH P)) 2)
                (EVALEQUAL (AEVAL (LIST 'PART P 0)) (AEVAL 'EXPT)))
           (AEVAL (LIST 'PART P 2)))
          (T (ASSGNPRI (AEVAL "error!") NIL 'ONLY)))) 
(PUT 'FIND_HOM_DEG_SF 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_HOM_DEG_SF 'DEFINED-ON-LINE '140) 
(PUT 'FIND_HOM_DEG_SF 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'FIND_HOM_DEG_SF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_HOM_DEG_SF (P)
    (PROG (TM F SB L1 L2 MOD_SWITCHED)
      (SETQ TM (FIRST_TERM_SF P))
      (SETQ L1 (GENSYM))
      (SETQ L2 (GENSYM))
      (PROG (F)
        (SETQ F FLIN_)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (SETQ SB (CONS (CONS F (LIST 'TIMES F L1)) SB))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (PROG (F)
        (SETQ F (SETDIFF FHOM_ FLIN_))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (SETQ SB (CONS (CONS F (LIST 'TIMES F L2)) SB))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       ((AND MODULAR_COMP (NULL *MODULAR))
        (PROGN (ON (LIST 'MODULAR)) (SETQ MOD_SWITCHED T))))
      (SETQ TM
              (CAR
               (MULTSQ (SIMP (LIST '*SQ (SUBF TM SB) NIL))
                       (INVSQ (CONS TM 1)))))
      (COND (MOD_SWITCHED (OFF (LIST 'MODULAR))))
      (RETURN
       (COND ((OR (NOT (PAIRP TM)) (OR (ATOM TM) (ATOM (CAR TM)))) (LIST 0 0))
             ((EQUAL (CAAAR TM) L1)
              (COND
               ((OR (OR (ATOM (CDAR TM)) (ATOM (CAR (CDAR TM))))
                    (AND (PAIRP TM) (EQUAL (CAR TM) '|:GI:|)))
                (LIST (CDAAR TM) 0))
               (T (LIST (CDAAR TM) (CDAAR (CDAR TM))))))
             ((OR (OR (ATOM (CDAR TM)) (ATOM (CAR (CDAR TM))))
                  (AND (PAIRP TM) (EQUAL (CAR TM) '|:GI:|)))
              (LIST 0 (CDAAR TM)))
             (T (LIST (CDAAR (CDAR TM)) (CDAAR TM))))))) 
(PUT 'FIND_HOMO_WEIGHTS 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_HOMO_WEIGHTS 'DEFINED-ON-LINE '205) 
(PUT 'FIND_HOMO_WEIGHTS 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'FIND_HOMO_WEIGHTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_HOMO_WEIGHTS (PDES)
    (COND ((NULL ALG_POLY) NIL)
          (T
           (PROG (W P S SF TF WL ALI WTLI WT ELI)
             (PROG (F)
               (SETQ F FTEM_)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F)
                  (PROGN
                   (SETQ W (MKID 'W_ F))
                   (SETQ WL (CONS W WL))
                   (SETQ ALI (CONS (CONS F W) ALI))
                   NIL))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (PROG (P)
               (SETQ P PDES)
              LAB
               (COND ((NULL P) (RETURN NIL)))
               ((LAMBDA (P)
                  (COND
                   ((AND (GREATERP (GET P 'TERMS) 1)
                         (NULL (GET P 'NONRATIONAL)))
                    (PROGN
                     (SETQ SF (CAR (GET P 'SQVAL)))
                     (SETQ WTLI NIL)
                     (PROG ()
                      WHILELABEL
                       (COND ((NOT SF) (RETURN NIL)))
                       (PROGN
                        (SETQ TF (FIRST_TERM_SF SF))
                        (SETQ SF (ADDF SF (NEGF TF)))
                        (SETQ WT NIL)
                        (PROG ()
                         WHILELABEL
                          (COND
                           ((NOT (AND TF (NOT (OR (ATOM TF) (ATOM (CAR TF))))))
                            (RETURN NIL)))
                          (PROGN
                           (SETQ W (ASSOC (CAAAR TF) ALI))
                           (COND
                            (W
                             (PROGN
                              (SETQ WT
                                      (CONS (LIST 'TIMES (CDAAR TF) (CDR W))
                                            WT)))))
                           (SETQ TF (CDAR TF)))
                          (GO WHILELABEL))
                        (SETQ WT
                                (COND ((NULL WT) 0) ((CDR WT) (CONS 'PLUS WT))
                                      (T (CAR WT))))
                        (SETQ WTLI (CONS WT WTLI)))
                       (GO WHILELABEL))
                     (COND
                      ((AND WTLI (CDR WTLI))
                       (PROG (W)
                         (SETQ W (CDR WTLI))
                        LAB
                         (COND ((NULL W) (RETURN NIL)))
                         ((LAMBDA (W)
                            (SETQ ELI
                                    (CONS
                                     (REVAL1 (LIST 'DIFFERENCE (CAR WTLI) W) T)
                                     ELI)))
                          (CAR W))
                         (SETQ W (CDR W))
                         (GO LAB))))))))
                (CAR P))
               (SETQ P (CDR P))
               (GO LAB))
             (SETQ !ARBINT 0)
             (SETQ S (SOLVEEVAL (LIST (CONS 'LIST ELI) (CONS 'LIST WL))))
             (COND
              ((EQUAL !ARBINT 0)
               (PROGN (PRIN2 "This system is not homogeneous.") NIL))
              ((EQUAL !ARBINT 1)
               (PROGN
                (PRIN2 "This system has the following homogeneity:")
                NIL))
              (T
               (PROGN
                (PRIN2 "The following are possible homogeneities:")
                NIL)))
             (MATHPRINT S)
             (RETURN NIL))))) 
(FLAG '(MAKE_HOM_ANSATZ) 'OPFN) 
(PUT 'MAKE_HOM_ANSATZ 'NUMBER-OF-ARGS 4) 
(PUT 'MAKE_HOM_ANSATZ 'DEFINED-ON-LINE '274) 
(PUT 'MAKE_HOM_ANSATZ 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'MAKE_HOM_ANSATZ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE_HOM_ANSATZ (F_1 F_2 D1 D2)
    (PROG (ANS ANS1 ANS2 H FL RPLY)
      (COND ((NULL F_1) (SETQ ANS1 1))
            ((NULL (CDR F_1)) (SETQ ANS1 (LIST 'EXPT (CAR F_1) D1)))
            (T (SETQ ANS1 (LIST 'EXPT (CONS 'PLUS F_1) D1))))
      (COND ((NULL F_2) (SETQ ANS2 1))
            ((NULL (CDR F_2)) (SETQ ANS2 (LIST 'EXPT (CAR F_1) D2)))
            (T (SETQ ANS2 (LIST 'EXPT (CONS 'PLUS F_2) D2))))
      (SETQ ANS (REVAL1 (LIST 'TIMES ANS1 ANS2) T))
      (RETURN
       (COND
        ((OR (NOT (PAIRP ANS)) (NEQ (CAR ANS) 'PLUS))
         (PROGN
          (SETQ H (GENSYM))
          (LIST 'LIST (LIST 'TIMES H ANS) (LIST 'LIST H))))
        (T
         (PROGN
          (SETQ ANS (CDR ANS))
          (PROG (F)
            (SETQ F ANS)
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F)
               (PROGN
                (SETQ H (GENSYM))
                (SETQ FL (CONS H FL))
                (SETQ RPLY (CONS (LIST 'TIMES H F) RPLY))))
             (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (LIST 'LIST (CONS 'PLUS RPLY) (CONS 'LIST FL)))))))) 
(PUT 'BI_LIN_EQN_LIN_COMB 'NUMBER-OF-ARGS 1) 
(PUT 'BI_LIN_EQN_LIN_COMB 'DEFINED-ON-LINE '331) 
(PUT 'BI_LIN_EQN_LIN_COMB 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'BI_LIN_EQN_LIN_COMB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BI_LIN_EQN_LIN_COMB (PDES)
    (PROG (P FL RS H)
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2 "Formulating a linear combination of all equations.")
          NIL)
         (TERPRI))))
      (SETQ RS (CONS NIL 1))
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ H (GENSYM))
            (SETQ FL (CONS H FL))
            (SETQ RS (ADDSQ (MULTSQ (SIMP H) (GET P 'SQVAL)) RS))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (LIST 'LIST (LIST '*SQ RS T) (CONS 'LIST FL))))) 
(PUT 'DROP_DEP_BI_LIN 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_DEP_BI_LIN 'DEFINED-ON-LINE '351) 
(PUT 'DROP_DEP_BI_LIN 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'DROP_DEP_BI_LIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_DEP_BI_LIN (ARGLIST)
    (PROG (PDES CND FL F CNDCP C LINDE AGAIN)
      (SETQ PDES (CAR ARGLIST))
      (COND
       ((AND PDES (CDR PDES))
        (PROG ()
         REPEATLABEL
          (PROGN
           (SETQ AGAIN NIL)
           (SETQ CND (BI_LIN_EQN_LIN_COMB PDES))
           (SETQ FL (CADDR CND))
           (SETQ CND
                   (SPLIT_SIMPLIFY
                    (LIST (LIST 'LIST (CADR CND)) (LIST 'LIST) FL
                          (CONS 'LIST FTEM_) T)))
           (COND
            (PRINT_
             (PROGN
              (PROGN (PRIN2 "Now solving the linear system.") NIL)
              (TERPRI))))
           (SETQ !ARBINT 0)
           (SETQ CND (CADR (SOLVEEVAL (LIST CND FL))))
           (COND
            (CND
             (PROGN
              (SETQ CND (CAR CND))
              (PROG (F)
                (SETQ F 1)
               LAB
                (COND ((MINUSP (DIFFERENCE !ARBINT F)) (RETURN NIL)))
                (PROGN
                 (SETQ CNDCP CND)
                 (PROG (C)
                   (SETQ C 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE !ARBINT C)) (RETURN NIL)))
                   (COND
                    ((NEQ C F)
                     (SETQ CNDCP
                             (AEVAL*
                              (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX C) 0)
                                    CNDCP)))))
                   (SETQ C (PLUS2 C 1))
                   (GO LAB))
                 (SETQ CNDCP (CDR CNDCP))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT
                      (AND CNDCP
                           (OR (ZEROP (CADDAR CNDCP))
                               (NOT (FREEOF LINDE (CADAR CNDCP))))))
                     (RETURN NIL)))
                   (SETQ CNDCP (CDR CNDCP))
                   (GO WHILELABEL))
                 (COND
                  ((NULL CNDCP)
                   (PROGN
                    (PROGN
                     (PRIN2
                      "The computation to find redundant equations has to be done again.")
                     NIL)
                    (TERPRI)))
                  (T (SETQ LINDE (CONS (REVAL1 (CADAR CNDCP) T) LINDE))))
                 NIL)
                (SETQ F (PLUS2 F 1))
                (GO LAB))
              (COND ((NULL LINDE) (PROGN (PRIN2 "No equations deleted.") NIL))
                    (T
                     (PROGN
                      (PRIN2 "Deleted redundant equations: ")
                      (PRIN2 LINDE)
                      NIL)))
              (TERPRI)
              (PROG (F)
                (SETQ F LINDE)
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F) (SETQ PDES (DROP_PDE F PDES NIL))) (CAR F))
                (SETQ F (CDR F))
                (GO LAB))
              (COND
               (AGAIN
                (PROGN
                 (PROGN (PRIN2 "This computation has to be repeated") NIL)
                 (TERPRI))))))))
          (COND ((NOT (NULL AGAIN)) (GO REPEATLABEL))))))
      (RETURN (COND (LINDE (CONS PDES (CDR ARGLIST))) (T NIL))))) 
(PUT 'FIND_FACTOR_BI_LIN 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_FACTOR_BI_LIN 'DEFINED-ON-LINE '401) 
(PUT 'FIND_FACTOR_BI_LIN 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'FIND_FACTOR_BI_LIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_FACTOR_BI_LIN (ARGLIST)
    (PROG (H PDES FC RHS LHS LHSFL CND CNDCP FL FL_1 FL_2 HDG DG1 DG2 INDX1
           INDX2 AGAIN FOUNDSOME)
      (PROGN
       (PRIN2 "Before starting to determine factorizable equations with ")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "a given factor (preferably one known to be non-zero), all")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "redundant equations have to be dropped.") NIL)
      (TERPRI)
      (PROGN (PRIN2 "Has this already been done? (y/n) ") NIL)
      (CHANGE_PROMPT_TO "")
      (PROG ()
       REPEATLABEL
        (SETQ H (TERMREAD))
        (COND ((NOT (OR (EQUAL H 'Y) (EQUAL H 'N))) (GO REPEATLABEL))))
      (COND ((EQUAL H 'N) (SETQ ARGLIST (DROP_DEP_BI_LIN ARGLIST))))
      (TERPRI)
      (SETQ FL_1 FLIN_)
      (SETQ FL_2 (SETDIFF_ACCORDING_TO FTEM_ FLIN_ FTEM_))
      (SETQ PDES (CAR ARGLIST))
      (PROGN (PRIN2 "Start of determining factorizable equations.") NIL)
      (TERPRI)
      (PROG ()
       REPEATLABEL
        (PROGN
         (PROGN
          (PRIN2
           "Give a factor of the sum of equations to be found: (terminate with ; ) ")
          NIL)
         (TERPRI)
         (SETQ FC (TERMXREAD))
         (COND
          ((NULL RHS)
           (PROGN
            (SETQ RHS (BI_LIN_EQN_LIN_COMB PDES))
            (SETQ FL (CADDR RHS))
            (SETQ RHS (CADR RHS))
            NIL)))
         (SETQ HDG (FIND_HOM_DEG_SF (CAR (SIMP FC))))
         (SETQ DG1 (DIFFERENCE 1 (CAR HDG)))
         (SETQ DG2 (DIFFERENCE 1 (CADR HDG)))
         (SETQ LHS (MAKE_HOM_ANSATZ FL_1 FL_2 DG1 DG2))
         (SETQ LHSFL (CADDR LHS))
         (SETQ LHS (CADR LHS))
         (SETQ CND
                 (SPLIT_SIMPLIFY
                  (LIST
                   (LIST 'LIST
                         (AEVAL* (LIST 'DIFFERENCE (LIST 'TIMES FC LHS) RHS)))
                   (LIST 'LIST) FL (CONS 'LIST FTEM_) T)))
         (COND
          (PRINT_
           (PROGN
            (PROGN (PRIN2 "Now solving the linear system.") NIL)
            (TERPRI))))
         (SETQ !ARBINT 0)
         (SETQ CND
                 (CDR
                  (SOLVEEVAL
                   (LIST CND (CONS 'LIST (APPEND (CDR LHSFL) (CDR FL)))))))
         (COND
          (CND
           (PROGN
            (SETQ CND (CAR CND))
            (SETQ LHS (AEVAL* (LIST 'SUB CND LHS)))
            (PROG (INDX1)
              (SETQ INDX1 1)
             LAB
              (COND ((MINUSP (DIFFERENCE !ARBINT INDX1)) (RETURN NIL)))
              (PROGN
               (SETQ CNDCP LHS)
               (PROG (INDX2)
                 (SETQ INDX2 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE !ARBINT INDX2)) (RETURN NIL)))
                 (COND
                  ((NEQ INDX2 INDX1)
                   (SETQ CNDCP
                           (AEVAL*
                            (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX INDX2) 0)
                                  CNDCP))))
                  (T
                   (SETQ CNDCP
                           (AEVAL*
                            (LIST 'SUB (LIST 'EQUAL (LIST 'ARBCOMPLEX INDX2) 1)
                                  CNDCP)))))
                 (SETQ INDX2 (PLUS2 INDX2 1))
                 (GO LAB))
               (COND
                ((NOT (ZEROP CNDCP))
                 (PROGN
                  (SETQ CNDCP (LIST '*SQ (MULTSQ (SIMP FC) CNDCP) T))
                  (SETQ PDES
                          (EQINSERT
                           (SETQ H
                                   (MKEQSQ CNDCP NIL NIL FTEM_ VL_ ALLFLAGS_ T
                                    (LIST 0) NIL PDES))
                           PDES))
                  (COND
                   ((AND H (NOT (FREEOF PDES H)))
                    (PROGN
                     (SETQ FOUNDSOME T)
                     (PROGN (PRIN2 "New equation: ") (PRIN2 H) NIL)
                     (MATHPRINT (LIST '*SQ (GET H 'SQVAL) T))
                     (SETQ RHS NIL)
                     NIL)))))))
              (SETQ INDX1 (PLUS2 INDX1 1))
              (GO LAB)))))
         (PROGN
          (PRIN2 "Do you want to find further factorizable equations ")
          (PRIN2 "with other factors? (y/n) ")
          NIL)
         (PROG ()
          REPEATLABEL
           (SETQ H (TERMREAD))
           (COND ((NOT (OR (EQUAL H 'Y) (EQUAL H 'N))) (GO REPEATLABEL))))
         (COND ((EQUAL H 'Y) (SETQ AGAIN T)) (T (SETQ AGAIN NIL)))
         NIL)
        (COND ((NOT (NULL AGAIN)) (GO REPEATLABEL))))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN (COND (FOUNDSOME (CONS PDES (CDR ARGLIST))) (T NIL))))) 
(PUT 'ELIGIBLE_EQN 'NUMBER-OF-ARGS 3) 
(PUT 'ELIGIBLE_EQN 'DEFINED-ON-LINE '487) 
(PUT 'ELIGIBLE_EQN 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'ELIGIBLE_EQN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELIGIBLE_EQN (P ALLNZF FEWEST_F_SO_FAR)
    (PROG (HD MAXDEG MAXDEGDERIVS ALLD DER)
      (SETQ HD (GET P 'HOM_DEG))
      (RETURN
       (COND
        ((OR (GET P 'NONRATIONAL) (NULL HD) (NEQ (CAR (GET P 'HOM_DEG)) 0)
             (LESSP (CADR (GET P 'HOM_DEG)) 2)
             (GREATERP (LENGTH (GET P 'FCT_HOM)) 3)
             (AND (EQUAL FEWEST_F_SO_FAR 2) (NEQ (LENGTH (GET P 'FCT_HOM)) 2))
             (FREEOFLIST (GET P 'FCT_HOM) ALLNZF))
         NIL)
        ((EQUAL (LENGTH (GET P 'FCT_HOM)) 2)
         (CONS 2 (SMEMBERL ALLNZF (GET P 'FCT_HOM))))
        (T
         (PROGN
          (SETQ ALLD (GET P 'DERIVS))
          (SETQ MAXDEG 0)
          (SETQ MAXDEGDERIVS NIL)
          (PROG ()
           WHILELABEL
            (COND ((NOT ALLD) (RETURN NIL)))
            (PROGN
             (SETQ DER (CAR ALLD))
             (SETQ ALLD (CDR ALLD))
             (COND
              ((NULL (CDAR DER))
               (COND
                ((EQUAL (CDR DER) MAXDEG)
                 (SETQ MAXDEGDERIVS (CONS DER MAXDEGDERIVS)))
                ((GREATERP (CDR DER) MAXDEG)
                 (PROGN
                  (SETQ MAXDEGDERIVS (LIST DER))
                  (SETQ MAXDEG (CDR DER))))))))
            (GO WHILELABEL))
          (CONS 3 (SMEMBERL ALLNZF MAXDEGDERIVS)))))))) 
(PUT 'PARA_SOLVE_HOM_EQU 'NUMBER-OF-ARGS 1) 
(PUT 'PARA_SOLVE_HOM_EQU 'DEFINED-ON-LINE '525) 
(PUT 'PARA_SOLVE_HOM_EQU 'DEFINED-IN-FILE 'CRACK/CRHOMALG.RED) 
(PUT 'PARA_SOLVE_HOM_EQU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARA_SOLVE_HOM_EQU (ARGLIST)
    (COND
     ((NULL FHOM_)
      (COND
       (PRINT_
        (PROGN
         (PRIN2 "This function is only applicable to homogeneous problems.")
         NIL))
       (T NIL)))
     (T
      (PROG (P H F FP BESTF L1 PDES FORG NZF ALLNZF PLI ALLF NEWF NEWE VL
             FEWEST_F_SO_FAR)
        (SETQ PDES (CAR ARGLIST))
        (SETQ FORG (CADR ARGLIST))
        (COND (EXPERT_MODE (SETQ L1 (SELECTPDES PDES 1)))
              (T (SETQ L1 (CADDDR ARGLIST))))
        (PROG (P)
          (SETQ P INEQ_)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (COND
              ((NO_NUMBER_ATOM_SQ P)
               (SETQ ALLNZF (CONS (CAAAR (CAR P)) ALLNZF)))))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (SETQ FEWEST_F_SO_FAR 100)
        (PROG ()
         WHILELABEL
          (COND ((NOT L1) (RETURN NIL)))
          (PROGN
           (SETQ P (CAR L1))
           (SETQ L1 (CDR L1))
           (SETQ H (ELIGIBLE_EQN P ALLNZF FEWEST_F_SO_FAR))
           (COND
            (H
             (PROGN
              (SETQ FEWEST_F_SO_FAR (CAR H))
              (SETQ PLI (CONS P PLI))
              (PROG (F)
                (SETQ F (CDR H))
               LAB
                (COND ((NULL F) (RETURN NIL)))
                ((LAMBDA (F)
                   (PROGN
                    (SETQ FP (ASSOC F NZF))
                    (COND ((NULL FP) (SETQ NZF (CONS (CONS F 1) NZF)))
                          (T
                           (SETQ NZF
                                   (CONS (CONS F (ADD1 (CDR FP)))
                                         (DELETE FP NZF)))))))
                 (CAR F))
                (SETQ F (CDR F))
                (GO LAB))))))
          (GO WHILELABEL))
        (COND
         ((NULL NZF)
          (RETURN
           (COND
            (PRINT_
             (PROGN
              (PROGN
               (PRIN2 "Either the equations do involve more than 3 functions,")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 " or they do not involve functions that are known")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "to be globally homogeneous, or none of the homogeneous")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "functions in the equations is known to be non-zero.")
               NIL)
              (TERPRI)
              (PROGN
               (PRIN2 "In that case a case distinction 44 for one of these")
               NIL)
              (TERPRI)
              (PROGN (PRIN2 "functions would help.") NIL)
              (TERPRI)
              NIL))
            (T NIL))))
         (T
          (PROGN
           (SETQ BESTF (CAR NZF))
           (SETQ NZF (CDR NZF))
           (PROG ()
            WHILELABEL
             (COND ((NOT NZF) (RETURN NIL)))
             (PROGN
              (COND ((GREATERP (CDAR NZF) (CDR BESTF)) (SETQ BESTF (CAR NZF))))
              (SETQ NZF (CDR NZF)))
             (GO WHILELABEL)))))
        (SETQ BESTF (CAR BESTF))
        (PROG (P)
          (SETQ P PLI)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P) (SETQ ALLF (UNION (GET P 'FCT_HOM) ALLF))) (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (COND ((NULL (CDR ALLF)) (RETURN NIL)))
        (SETQ VL (FCTARGS BESTF))
        (PROG (F)
          (SETQ F ALLF)
         LAB
          (COND ((NULL F) (RETURN NIL)))
          ((LAMBDA (F)
             (COND
              ((NEQ F BESTF)
               (PROGN
                (SETQ NEWF (NEWFCT FNAME_ VL NFCT_))
                (SETQ NFCT_ (ADD1 NFCT_))
                (SETQ FTEM_ (APPEND FTEM_ (LIST NEWF)))
                (SETQ NEWE
                        (MKEQSQ
                         (SIMP (LIST 'DIFFERENCE F (LIST 'TIMES NEWF BESTF)))
                         NIL NIL (LIST F NEWF BESTF) VL ALLFLAGS_ T (LIST 0)
                         NIL PDES))
                (SETQ PDES (EQINSERT NEWE PDES))
                (SETQ TO_DO_LIST
                        (CONS (LIST 'SUBST_LEVEL_35 (LIST NEWE) F) TO_DO_LIST))
                NIL))))
           (CAR F))
          (SETQ F (CDR F))
          (GO LAB))
        (RETURN (LIST PDES FORG)))))) 
(ENDMODULE) 