(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LINALGSYS)) 
(FLUID '(COUNT_TRIES TR_SUBSYS MAX_LOSOF MATRIX_849)) 
(SETQ TR_SUBSYS NIL) 
(PUT 'TRIAN_LIN_ALG 'NUMBER-OF-ARGS 1) 
(PUT 'TRIAN_LIN_ALG 'DEFINED-ON-LINE '36) 
(PUT 'TRIAN_LIN_ALG 'DEFINED-IN-FILE 'CRACK/CRLINALG.RED) 
(PUT 'TRIAN_LIN_ALG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIAN_LIN_ALG (ARGLIST)
    (COND ((NOT LIN_PROBLEM) NIL)
          (T
           (PROG (H1 H2 H3 H4 F FL NEWFL TR_OPT REMAIN_PDES REMAIN_FL LI
                  TOTAL_TERMS)
             (SETQ TR_OPT T)
             (SETQ H2 (CAR ARGLIST))
             (PROG ()
              WHILELABEL
               (COND ((NOT H2) (RETURN NIL)))
               (PROGN
                (COND ((IS_ALGEBRAIC (CAR H2)) (SETQ H1 (CONS (CAR H2) H1))))
                (SETQ H2 (CDR H2)))
               (GO WHILELABEL))
             (SPOT_OVER_DET H1 NIL NIL NIL)
             (PROGN (PRIN2 "count_tries=") (PRIN2 COUNT_TRIES) NIL)
             (TERPRI)
             (RETURN NIL)
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ H2 (ALG_LENGTH_REDUCTION (LIST H1 NIL VL_ H1)))
                (COND (H2 (SETQ H1 (CAR H2)))))
               (COND ((NOT (OR CONTRADICTION_ (NULL H2))) (GO REPEATLABEL))))
             (SETQ REMAIN_PDES H1)
             (SETQ TOTAL_TERMS 0)
             (PROG (H2)
               (SETQ H2 REMAIN_PDES)
              LAB
               (COND ((NULL H2) (RETURN NIL)))
               ((LAMBDA (H2)
                  (SETQ TOTAL_TERMS (PLUS TOTAL_TERMS (GET H2 'TERMS))))
                (CAR H2))
               (SETQ H2 (CDR H2))
               (GO LAB))
             (PROG (H2)
               (SETQ H2 H1)
              LAB
               (COND ((NULL H2) (RETURN NIL)))
               ((LAMBDA (H2) (SETQ FL (ADD_EQU_TO_FL H2 FL))) (CAR H2))
               (SETQ H2 (CDR H2))
               (GO LAB))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND FL (NULL CONTRADICTION_))) (RETURN NIL)))
               (PROGN
                (SETQ FL (IDX_SORT FL))
                (COND
                 (TR_OPT
                  (PROGN
                   (TERPRI)
                   (PROGN (PRIN2 "fl2=") NIL)
                   (PRETTYPRINT FL))))
                (COND
                 ((EQUAL (CAAR FL) 1)
                  (PROGN
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT (LEQ (CAAR FL) 1)) (RETURN NIL)))
                     (PROGN
                      (COND
                       ((AND TR_OPT (EQUAL (CAAR FL) 1))
                        (PROGN
                         (PROGN
                          (PRIN2 "equation ")
                          (PRIN2 (CADDAR FL))
                          (PRIN2 " determines ")
                          (PRIN2 (CADAR FL))
                          NIL)
                         (TERPRI))))
                      (SETQ NEWFL (CONS (CADAR FL) NEWFL))
                      (SETQ FL
                              (COND ((EQUAL (CAAR FL) 0) (CDR FL))
                                    (T
                                     (PROGN
                                      (SETQ REMAIN_PDES
                                              (DELETE (CADDAR FL) REMAIN_PDES))
                                      (SETQ TOTAL_TERMS
                                              (DIFFERENCE TOTAL_TERMS
                                                          (GET (CADDAR FL)
                                                               'TERMS)))
                                      (SETQ FL
                                              (DEL_EQU_FROM_FL (CADDAR FL)
                                               (CDR FL))))))))
                     (GO WHILELABEL))
                   NIL))
                 (T
                  (PROGN
                   (SETQ REMAIN_FL
                           (PROG (H3 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ H3 FL)
                             (COND ((NULL H3) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (H3) (CADR H3))
                                               (CAR H3))
                                              NIL)))
                            LOOPLABEL
                             (SETQ H3 (CDR H3))
                             (COND ((NULL H3) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (H3) (CADR H3)) (CAR H3))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                   (PROG (H1)
                     (SETQ H1 REMAIN_PDES)
                    LAB
                     (COND ((NULL H1) (RETURN NIL)))
                     ((LAMBDA (H1)
                        (PROGN
                         (SETQ H2 (GET H1 'FCTEVAL_LIN))
                         (SETQ LI NIL)
                         (COND
                          ((NULL H2)
                           (PROGN
                            (PROG (F)
                              (SETQ F REMAIN_FL)
                             LAB
                              (COND ((NULL F) (RETURN NIL)))
                              ((LAMBDA (F)
                                 (COND
                                  ((NOT (FREEOF (GET H1 'RATIONAL) F))
                                   (SETQ LI
                                           (CONS
                                            (CONS
                                             (COEFFN
                                              (LIST '*SQ (GET H1 'SQVAL) T) F
                                              1)
                                             F)
                                            LI)))))
                               (CAR F))
                              (SETQ F (CDR F))
                              (GO LAB))
                            NIL))
                          (T
                           (PROGN
                            (PROG ()
                             WHILELABEL
                              (COND ((NOT H2) (RETURN NIL)))
                              (PROGN
                               (COND
                                ((NOT (FREEOF (CDAR H2) REMAIN_FL))
                                 (SETQ LI (CONS (CAR H2) LI))))
                               (SETQ H2 (CDR H2)))
                              (GO WHILELABEL)))))
                         (COND (LI (PUT H1 'FCTEVAL_LIN (REVERSE LI))))
                         NIL))
                      (CAR H1))
                     (SETQ H1 (CDR H1))
                     (GO LAB))
                   (SETQ H1
                           (MAKE_SUBST REMAIN_PDES REMAIN_FL VL_ REMAIN_PDES
                            NIL NIL NIL NIL T T T NIL T NIL))
                   (COND
                    ((AND (NULL CONTRADICTION_) H1)
                     (PROGN
                      (SETQ H2 (CADDR H1))
                      (SETQ H3 (DIFFERENCE TOTAL_TERMS (GET H2 'TERMS)))
                      (SETQ REMAIN_PDES (DELETE H2 (CAR H1)))
                      (SETQ TOTAL_TERMS 0)
                      (PROG (H4)
                        (SETQ H4 REMAIN_PDES)
                       LAB
                        (COND ((NULL H4) (RETURN NIL)))
                        ((LAMBDA (H4)
                           (SETQ TOTAL_TERMS
                                   (PLUS TOTAL_TERMS (GET H4 'TERMS))))
                         (CAR H4))
                        (SETQ H4 (CDR H4))
                        (GO LAB))
                      (COND
                       (TR_OPT
                        (PROGN
                         (PROGN
                          (PRIN2 "equation ")
                          (PRIN2 H2)
                          (PRIN2 " now disregarded")
                          NIL)
                         (TERPRI)
                         (PROGN
                          (PRIN2 "growth: ")
                          (PRIN2 (DIFFERENCE TOTAL_TERMS H3))
                          (PRIN2 " terms")
                          NIL)
                         (TERPRI)
                         (PROGN
                          (PRIN2 (LENGTH REMAIN_PDES))
                          (PRIN2 " remaining PDEs: ")
                          (PRIN2 REMAIN_PDES)
                          NIL)
                         (TERPRI)
                         NIL)))
                      (SETQ FL (DEL_EQU_FROM_FL H2 FL))
                      (SETQ H2 (CADR H1))
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT
                           (OR (NOT (PAIRP (CAR H2))) (NEQ (CAAR H2) 'EQUAL)))
                          (RETURN NIL)))
                        (SETQ H2 (CDR H2))
                        (GO WHILELABEL))
                      (SETQ F (CADAR H2))
                      (SETQ REMAIN_FL (DELETE F REMAIN_FL))
                      (COND
                       (TR_OPT
                        (PROGN
                         (PROGN
                          (PRIN2 (LENGTH REMAIN_FL))
                          (PRIN2 " remaining functions: ")
                          (PRIN2 REMAIN_FL)
                          NIL)
                         (TERPRI)
                         NIL)))
                      (COND
                       ((EQUAL (CADAR FL) F)
                        (PROGN (SETQ H4 (CDDAR FL)) (SETQ FL (CDR FL))))
                       (T
                        (PROGN
                         (SETQ H3 FL)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT (NEQ (CADADR H3) F)) (RETURN NIL)))
                           (SETQ H3 (CDR H3))
                           (GO WHILELABEL))
                         (SETQ H4 (CDDADR H3))
                         (RPLACD H3 (CDDR H3))
                         NIL)))
                      (PROG (H3)
                        (SETQ H3 H4)
                       LAB
                        (COND ((NULL H3) (RETURN NIL)))
                        ((LAMBDA (H3)
                           (PROGN
                            (SETQ FL (DEL_EQU_FROM_FL H3 FL))
                            (COND
                             ((NOT (FREEOF REMAIN_PDES H3))
                              (SETQ FL (ADD_EQU_TO_FL H3 FL))))))
                         (CAR H3))
                        (SETQ H3 (CDR H3))
                        (GO LAB))
                      (PROG ()
                       REPEATLABEL
                        (PROGN
                         (SETQ H2
                                 (ALG_LENGTH_REDUCTION
                                  (LIST REMAIN_PDES NIL VL_ REMAIN_PDES)))
                         (COND
                          (H2
                           (PROGN
                            (SETQ H3 (SETDIFF REMAIN_PDES (CAR H2)))
                            (PROG (H4)
                              (SETQ H4 H3)
                             LAB
                              (COND ((NULL H4) (RETURN NIL)))
                              ((LAMBDA (H4) (SETQ FL (DEL_EQU_FROM_FL H4 FL)))
                               (CAR H4))
                              (SETQ H4 (CDR H4))
                              (GO LAB))
                            (SETQ REMAIN_PDES (CAR H2))
                            (PROG (H3)
                              (SETQ H3 (CADDR H2))
                             LAB
                              (COND ((NULL H3) (RETURN NIL)))
                              ((LAMBDA (H3)
                                 (PROGN
                                  (SETQ FL (DEL_EQU_FROM_FL H3 FL))
                                  (COND
                                   ((NOT (FREEOF REMAIN_PDES H3))
                                    (SETQ FL (ADD_EQU_TO_FL H3 FL))))))
                               (CAR H3))
                              (SETQ H3 (CDR H3))
                              (GO LAB))))))
                        (COND
                         ((NOT (OR CONTRADICTION_ (NULL H2)))
                          (GO REPEATLABEL))))
                      NIL))
                    (T (REDERR "make_subst=nil, what now???")))
                   NIL))))
               (GO WHILELABEL))
             (COND
              ((NEQ NEWFL FTEM_)
               (CHANGE_FCTS_ORDERING NEWFL (CAR ARGLIST) VL_))))))) 
(PUT 'FLIN_NON_TRIV_COND 'NUMBER-OF-ARGS 1) 
(PUT 'FLIN_NON_TRIV_COND 'DEFINED-ON-LINE '185) 
(PUT 'FLIN_NON_TRIV_COND 'DEFINED-IN-FILE 'CRACK/CRLINALG.RED) 
(PUT 'FLIN_NON_TRIV_COND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLIN_NON_TRIV_COND (PDES)
    (COND
     ((AND ALG_POLY FLIN_ FHOM_)
      (PROG (OLDORDER P H PCF ALLCF FL U V TR_SUBSYS SYSLI SY R S NCONDI
             SOME_NEW NO_OF_PDES UMAX MINSIZE FI A SAVE OFL*BAK *NATBAT)
        (SETQ OLDORDER (SETKORDER FLIN_))
        (SETQ U 0)
        (PROG (P)
          (SETQ P PDES)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
             (COND
              ((AND (SETQ H (GET P 'HOM_DEG)) (EQUAL (CAR H) 1))
               (PROGN
                (SETQ U (ADD1 U))
                (SETQ H (REORDER (CAR (GET P 'SQVAL))))
                (SETQ FL FLIN_)
                (SETQ PCF NIL)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT FL) (RETURN NIL)))
                  (PROGN
                   (COND
                    ((OR (NULL H) (NEQ (CAAAR H) (CAR FL)))
                     (SETQ PCF (CONS 0 PCF)))
                    (T
                     (PROGN
                      (SETQ PCF (CONS (LIST '*SQ (CONS (CDAR H) 1) T) PCF))
                      (SETQ H (CDR H)))))
                   (SETQ FL (CDR FL)))
                  (GO WHILELABEL))
                (SETQ ALLCF (CONS PCF ALLCF))))))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))
        (SETKORDER OLDORDER)
        (SETQ V (LENGTH FLIN_))
        (COND
         (PRINT_
          (PROGN
           (PROGN
            (PRIN2 "There will be ")
            (PRIN2
             (AEVAL
              (LIST 'QUOTIENT
                    (LIST 'QUOTIENT (LIST 'FACTORIAL U)
                          (LIST 'FACTORIAL (LIST 'DIFFERENCE U V)))
                    (LIST 'FACTORIAL V))))
            (PRIN2 " equations")
            NIL)
           (TERPRI)
           (CHANGE_PROMPT_TO "")
           (PROGN (PRIN2 "How many shall be generated? ") NIL)
           (SETQ UMAX (TERMREAD))
           (RESTORE_INTERACTIVE_PROMPT)))
         (T (SETQ UMAX 1000000)))
        (SETQ SYSLI (OUT_OFF V U ALLCF))
        (SETQ U 0)
        (SETQ MINSIZE 10000000)
        (SETQ FI (LEVEL_STRING SESSION_))
        (SETQ FI (BLDMSG_INTERNAL "%s-" (LIST FI)))
        (SETQ FI (CDDDR (EXPLODE FI)))
        (SETQ FI (COMPRESS (CONS '|"| (CONS 'C (CONS 'D FI)))))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 10 I)) (RETURN NIL)))
          (PROGN
           (SETQ R (RANDOM 10))
           (SETQ FI (BLDMSG_INTERNAL "%w%d" (LIST FI R))))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ A (OPEN FI 'OUTPUT))
        (SETQ OFL*BAK OFL*)
        (SETQ OFL* FI)
        (SETQ SAVE (WRS A))
        (SETQ *NATBAT *NAT)
        (OFF (LIST 'NAT))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND SYSLI (LESSP U UMAX))) (RETURN NIL)))
          (PROGN
           (SETQ SY (CAR SYSLI))
           (SETQ SYSLI (CDR SYSLI))
           (SETQ U (ADD1 U))
           (MACHEMATRIX 'MATRIX_849 V V)
           (PROG (R)
             (SETQ R 1)
            LAB
             (COND ((MINUSP (DIFFERENCE V R)) (RETURN NIL)))
             (PROG (S)
               (SETQ S 1)
              LAB
               (COND ((MINUSP (DIFFERENCE V S)) (RETURN NIL)))
               (SETZEWERT 'MATRIX_849 R S (NTH (NTH SY S) R))
               (SETQ S (PLUS2 S 1))
               (GO LAB))
             (SETQ R (PLUS2 R 1))
             (GO LAB))
           (SETQ H (DETERMINANTE 'MATRIX_849))
           (SETQ P (NO_OF_TM_SF (CAR H)))
           (ASSGNPRI (AEVAL* (LIST '*SQ H 'T)) NIL 'ONLY)
           (SETK 'MATRIX_849 NIL))
          (GO WHILELABEL))
        (PROGN (PRIN2 "end$") NIL)
        (TERPRI)
        (WRS SAVE)
        (SETQ OFL* OFL*BAK)
        (CLOSE A)
        (COND ((NEQ *NAT *NATBAT) (ON (LIST 'NAT))))
        (SETQ EQUATIONS_FILE FI)
        (COND
         (TR_SUBSYS
          (PROGN (PROGN (PRIN2 "ncondi=") (PRIN2 NCONDI) NIL) (TERPRI))))
        (SETQ NO_OF_PDES (LENGTH PDES))
        (SETQ S NIL)
        (PROG (H)
          (SETQ H NCONDI)
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (PROGN
              (SETQ R
                      (MKEQSQ H NIL NIL FTEM_ VL_ ALLFLAGS_ T (LIST 0) NIL
                       PDES))
              (SETQ PDES (EQINSERT R PDES))
              (SETQ S (CONS R S))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (PROG (H)
          (SETQ H S)
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (COND ((MEMBER H PDES) (SETQ SOME_NEW (CONS H SOME_NEW)))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (COND
         ((AND PRINT_ SOME_NEW)
          (PROGN
           (PROGN
            (PRIN2 "New equations due to vanishing coeff. determinants: ")
            (PRIN2 (CAR SOME_NEW))
            NIL)
           (PROG (H)
             (SETQ H (CDR SOME_NEW))
            LAB
             (COND ((NULL H) (RETURN NIL)))
             ((LAMBDA (H) (PROGN (PRIN2 ", ") (PRIN2 H) NIL)) (CAR H))
             (SETQ H (CDR H))
             (GO LAB)))))
        (RETURN
         (COND
          ((OR SOME_NEW (NEQ NO_OF_PDES (LENGTH PDES)))
           (COND
            ((IN_CYCLE
              (PROGN
               (SETQ U 0)
               (PROG (R)
                 (SETQ R SOME_NEW)
                LAB
                 (COND ((NULL R) (RETURN NIL)))
                 ((LAMBDA (R) (SETQ U (PLUS U (GET R 'PRINTLENGTH)))) (CAR R))
                 (SETQ R (CDR R))
                 (GO LAB))
               (SETQ R (LENGTH SOME_NEW))
               (SETQ S 0)
               (SETQ H NIL)
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND (LESSP S 3) SOME_NEW)) (RETURN NIL)))
                 (PROGN
                  (SETQ S (ADD1 S))
                  (SETQ H (CONS (GET (CAR SOME_NEW) 'TERMS) H))
                  (SETQ SOME_NEW (CDR SOME_NEW)))
                 (GO WHILELABEL))
               (CONS 'SUB_SYS (CONS STEPCOUNTER_ (CONS R (CONS U H))))))
             NIL)
            (T PDES)))
          (T NIL))))))) 
(PUT 'SUBLINFCT 'NUMBER-OF-ARGS 1) 
(PUT 'SUBLINFCT 'DEFINED-ON-LINE '315) 
(PUT 'SUBLINFCT 'DEFINED-IN-FILE 'CRACK/CRLINALG.RED) 
(PUT 'SUBLINFCT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUBLINFCT (ARGLIST)
    (COND ((NULL FLIN_) NIL)
          (T
           (PROG (PDES FLCP P F H COEFGCD PDESCP CASESUB NOCASUB S FF Q)
             (SETQ PDES (CAR ARGLIST))
             (SETQ FLCP FLIN_)
             (PROG (P)
               (SETQ P PDES)
              LAB
               (COND ((NULL P) (RETURN NIL)))
               ((LAMBDA (P) (FCTEVAL P)) (CAR P))
               (SETQ P (CDR P))
               (GO LAB))
             (PROG ()
              WHILELABEL
               (COND ((NOT FLCP) (RETURN NIL)))
               (PROGN
                (SETQ F (CAR FLCP))
                (SETQ FLCP (CDR FLCP))
                (SETQ COEFGCD NIL)
                (SETQ PDESCP PDES)
                (PROG ()
                 WHILELABEL
                  (COND ((NOT (AND PDESCP (NEQ COEFGCD 1))) (RETURN NIL)))
                  (PROGN
                   (SETQ P (CAR PDESCP))
                   (SETQ PDESCP (CDR PDESCP))
                   (COND
                    ((MEMBER F (GET P 'FCTS))
                     (PROGN
                      (SETQ H (GET P 'FCTEVAL_LIN))
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT (AND H (NEQ F (CDAR H)))) (RETURN NIL)))
                        (SETQ H (CDR H))
                        (GO WHILELABEL))
                      (COND
                       ((NULL H)
                        (PROGN
                         (SETQ H (GET P 'FCTEVAL_NCA))
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT (AND H (NEQ F (CDAR H)))) (RETURN NIL)))
                           (SETQ H (CDR H))
                           (GO WHILELABEL))
                         (COND
                          ((NULL H)
                           (PROGN
                            (SETQ H (GET P 'FCTEVAL_NLI))
                            (PROG ()
                             WHILELABEL
                              (COND
                               ((NOT (AND H (NEQ F (CDAR H)))) (RETURN NIL)))
                              (SETQ H (CDR H))
                              (GO WHILELABEL))))))))
                      (COND
                       ((NULL H)
                        (PROGN (PRIN2 "### Error in sublinfct!") NIL)))
                      (TERPRI)
                      (SETQ COEFGCD
                              (COND ((NULL COEFGCD) (MK*SQ (CAAR H)))
                                    (T
                                     (ERR_CATCH_GCD (MK*SQ (CAAR H))
                                      COEFGCD))))
                      NIL))))
                  (GO WHILELABEL))
                (COND
                 ((NEQ COEFGCD 1)
                  (PROGN
                   (COND
                    ((AND (PAIRP COEFGCD) (EQUAL (CAR COEFGCD) '*SQ))
                     (SETQ H
                             (SIMPLIFYSQ (CADR COEFGCD) (GET P 'FCTS) T NIL
                              NIL)))
                    (T (SETQ H (CONS 1 1))))
                   (COND
                    ((NEQ H (LIST (CONS 1 1)))
                     (SETQ CASESUB (CONS (CONS COEFGCD F) CASESUB)))
                    (T (SETQ NOCASUB (CONS (CONS COEFGCD F) NOCASUB))))))))
               (GO WHILELABEL))
             (COND
              (PRINT_
               (PROGN
                (COND
                 (CASESUB
                  (PROGN
                   (PROGN (PRIN2 "Case generating substitutions:") NIL)
                   (TERPRI)
                   (MATHPRINT
                    (CONS 'LIST
                          (PROG (S FORALL-RESULT FORALL-ENDPTR)
                            (SETQ S CASESUB)
                            (COND ((NULL S) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (S)
                                                (LIST '*SQ
                                                      (MULTSQ (CADAR S)
                                                              (SIMP (CDR S)))
                                                      T))
                                              (CAR S))
                                             NIL)))
                           LOOPLABEL
                            (SETQ S (CDR S))
                            (COND ((NULL S) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (S)
                                        (LIST '*SQ
                                              (MULTSQ (CADAR S) (SIMP (CDR S)))
                                              T))
                                      (CAR S))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))))
                (COND
                 (NOCASUB
                  (PROGN
                   (PROGN (PRIN2 "Non-case generating substitutions:") NIL)
                   (TERPRI)
                   (MATHPRINT
                    (CONS 'LIST
                          (PROG (S FORALL-RESULT FORALL-ENDPTR)
                            (SETQ S NOCASUB)
                            (COND ((NULL S) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (S)
                                                (LIST '*SQ
                                                      (MULTSQ (CADAR S)
                                                              (SIMP (CDR S)))
                                                      T))
                                              (CAR S))
                                             NIL)))
                           LOOPLABEL
                            (SETQ S (CDR S))
                            (COND ((NULL S) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (S)
                                        (LIST '*SQ
                                              (MULTSQ (CADAR S) (SIMP (CDR S)))
                                              T))
                                      (CAR S))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))))))))
             (COND ((AND (NULL CASESUB) (NULL NOCASUB)) (RETURN NIL)))
             (COND
              (CASESUB
               (PROGN
                (COND
                 (NOCASUB
                  (PROGN
                   (SETQ S (LENGTH NOCASUB))
                   (PROGN
                    (PRIN2 "After ")
                    (PRIN2 S)
                    (PRIN2
                     (COND ((EQUAL S 1) " substitution") (T " substitutions")))
                    (PRIN2 " a case distinction for ")
                    NIL)
                   (TERPRI)
                   (MATHPRINT (LIST '*SQ (CADAAR CASESUB) T))
                   (PROGN
                    (PRIN2
                     "is made. In the case <>0 the shortening substitution")
                    NIL)
                   (TERPRI)
                   (PROGN (PRIN2 "should be performed again.") NIL)
                   (TERPRI))))
                (SETQ TO_DO_LIST
                        (UNION (LIST (LIST 'SPLIT_INTO_CASES (CADAAR CASESUB)))
                               TO_DO_LIST))
                NIL)))
             (COND
              (NOCASUB
               (PROG (S)
                 (SETQ S NOCASUB)
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PROGN
                     (SETQ FF (MKID FNAME_ NFCT_))
                     (SETQ NFCT_ (ADD1 NFCT_))
                     (SETQ H (ASSOC (CDR S) DEPL*))
                     (COND
                      ((PAIRP H) (SETQ DEPL* (CONS (CONS FF (CDR H)) DEPL*))))
                     (SETQ FTEM_ (FCTINSERT FF FTEM_))
                     (SETQ FLIN_ (SORT_ACCORDING_TO (CONS FF FLIN_) FTEM_))
                     (COND
                      ((MEMBER (CDR S) FHOM_)
                       (SETQ FHOM_ (SORT_ACCORDING_TO (CONS FF FHOM_) FTEM_))))
                     (SETQ Q
                             (MKEQSQ
                              (ADDSQ (MULTSQ (CADAR S) (SIMP (CDR S)))
                                     (NEGSQ (SIMP FF)))
                              NIL NIL FTEM_ VL_ ALLFLAGS_ NIL (LIST 0) NIL
                              PDES))
                     (PUT Q 'NOT_TO_EVAL (LIST FF))
                     (SETQ PDES (EQINSERT Q PDES))
                     (FCTEVAL Q)
                     (SETQ TO_DO_LIST
                             (CONS
                              (CONS 'SUBST_LEVEL_35 (LIST (LIST Q) (CDR S)))
                              TO_DO_LIST))
                     NIL))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))))
             (RETURN (LIST PDES (CADR ARGLIST))))))) 
(ENDMODULE) 