(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INEQUALITIES)) 
(PUT 'SIMPLIFYSQ 'NUMBER-OF-ARGS 5) 
(PUT 'SIMPLIFYSQ 'DEFINED-ON-LINE '32) 
(PUT 'SIMPLIFYSQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMPLIFYSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFYSQ (P FTEM FCTR EN SEPA)
    (PROG (ENHI SI SF S PRINT_BAK)
      (COND ((AND EN RECORD_HIST) (SETQ ENHI (GET EN 'HISTRY_))))
      (RETURN
       (COND ((OR (NOT P) (SQZEROP P)) (LIST (SIMP 0)))
             ((NOT (PAIRP P))
              (COND ((FREEOF FTEM P) (LIST (SIMP 1))) (T (LIST (SIMP P)))))
             ((NEQ (CDR P) 1)
              (PROGN
               (COND
                (ENHI (PUT EN 'HISTRY_ (LIST 'TIMES ENHI (PREPF (CDR P))))))
               (SIMPLIFYSQ (CONS (CAR P) 1) FTEM FCTR EN SEPA)))
             ((MINUSF (CAR P))
              (PROGN
               (COND (ENHI (PUT EN 'HISTRY_ (LIST 'MINUS ENHI))))
               (SIMPLIFYSQ (NEGSQ P) FTEM FCTR EN SEPA)))
             ((OR (FREEOFLIST P FTEM) (MEMBER P INEQ_)) (LIST (SIMP 1)))
             ((AND (NULL (CDR (CAR P))) (EQUAL 1 (CDAR (CAR P))))
              (COND
               ((NEQ 1 (CDAAR (CAR P)))
                (PROGN
                 (COND
                  (ENHI
                   (PUT EN 'HISTRY_
                        (LIST 'EXPT ENHI (LIST 'QUOTIENT 1 (CDAAR (CAR P)))))))
                 (SIMPLIFYSQ (MKSQ (CAAAR (CAR P)) 1) FTEM FCTR EN SEPA)))
               ((NOT (PAIRP (CAAAR (CAR P)))) (LIST P))
               ((EQUAL (CAR (CAAAR (CAR P))) 'EXPT)
                (COND
                 ((FREEOFLIST (CADDR (CAAAR (CAR P))) FTEM)
                  (PROGN
                   (COND
                    (ENHI
                     (PUT EN 'HISTRY_
                          (LIST 'EXPT ENHI
                                (LIST 'QUOTIENT 1 (CADDR (CAAAR (CAR P))))))))
                   (SIMPLIFYSQ (SIMP (CADR (CAAAR (CAR P)))) FTEM FCTR EN
                    SEPA)))
                 ((FREEOFLIST (CADR (CAAAR (CAR P))) FTEM) (LIST (SIMP 1)))
                 (T (LIST P))))
               ((OR (EQUAL (CAR (CAAAR (CAR P))) 'LOG)
                    (EQUAL (CAR (CAAAR (CAR P))) 'LN))
                (PROGN
                 (COND
                  (ENHI
                   (PUT EN 'HISTRY_
                        (REVAL1 (LIST 'PLUS (LIST 'EXPT 'E ENHI) (MINUS 1))
                                T))))
                 (SIMPLIFYSQ (SIMP (LIST 'DIFFERENCE (CADR (CAAAR (CAR P))) 1))
                  FTEM FCTR EN SEPA)))
               ((EQUAL (CAR (CAAAR (CAR P))) 'LOG10)
                (PROGN
                 (COND
                  (ENHI
                   (PUT EN 'HISTRY_
                        (REVAL1 (LIST 'PLUS (LIST 'EXPT 10 ENHI) (MINUS 1))
                                T))))
                 (SIMPLIFYSQ (SIMP (LIST 'DIFFERENCE (CADR (CAAAR (CAR P))) 1))
                  FTEM FCTR EN SEPA)))
               (T (LIST P))))
             (T
              (PROGN
               (COND ((NULL FCTR) (SETQ SF (LIST P)))
                     (T
                      (PROGN
                       (COND
                        ((MORE_THAN_X_TERMS (CAR P)
                          (COND (*COMPLEX MAX_TERM_TO_FAC_COMPLEX)
                                (T MAX_TERM_TO_FAC_REAL)))
                         (SETQ SI (SFFAC (CAR P))))
                        (T
                         (PROGN
                          (SETQ S
                                  (CDR
                                   (ERR_CATCH_FAC2
                                    (LIST '*SQ (CONS (CAR P) 1) T))))
                          (COND
                           ((OR (NULL S)
                                (AND (NULL *COMPLEX)
                                     (OR (NOT (FREEOF S 'I))
                                         (NOT (FREEOF S '|:GI:|)))))
                            (SETQ SI NIL))
                           (T
                            (PROG ()
                             WHILELABEL
                              (COND ((NOT S) (RETURN NIL)))
                              (PROGN
                               (SETQ SF (SIMP (CADAR S)))
                               (COND
                                ((AND ENHI (NEQ (CDR SF) 1))
                                 (PROGN
                                  (PROGN
                                   (PRIN2
                                    "##### Unexpectedly a denominator is not =1 . As a consequence")
                                   NIL)
                                  (TERPRI)
                                  (PROGN
                                   (PRIN2
                                    "      the histry_ value of equation ")
                                   (PRIN2 EN)
                                   (PRIN2 " will be wrong. #####")
                                   NIL)
                                  (TERPRI))))
                               (SETQ SF (CAR SF))
                               (PROG (N)
                                 (SETQ N 1)
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE (CADDAR S) N))
                                   (RETURN NIL)))
                                 (SETQ SI (CONS SF SI))
                                 (SETQ N (PLUS2 N 1))
                                 (GO LAB))
                               (SETQ S (CDR S)))
                              (GO WHILELABEL)))))))
                       (COND ((NULL SI) (SETQ SF (LIST P)))
                             (T
                              (PROGN
                               (SETQ SF NIL)
                               (PROG ()
                                WHILELABEL
                                 (COND ((NOT SI) (RETURN NIL)))
                                 (PROGN
                                  (SETQ S
                                          (SIMPLIFYSQ (CONS (CAR SI) 1) FTEM
                                           NIL NIL SEPA))
                                  (COND
                                   ((EQUAL (CAR S) (CONS 1 1))
                                    (COND
                                     (ENHI
                                      (PUT EN 'HISTRY_
                                           (LIST 'QUOTIENT (GET EN 'HISTRY_)
                                                 (PREPF (CAR SI)))))
                                     (T NIL)))
                                   ((SQZEROP (CAR S))
                                    (PROGN
                                     (SETQ SF (LIST (SIMP 0)))
                                     (SETQ SI (LIST NIL))))
                                   ((MEMBER (CAR S) SF)
                                    (COND
                                     (ENHI
                                      (PUT EN 'HISTRY_
                                           (LIST 'QUOTIENT (GET EN 'HISTRY_)
                                                 (PREPF (CAR SI)))))
                                     (T NIL)))
                                   (T
                                    (PROGN
                                     (COND
                                      ((AND ENHI
                                            (NEQ (CAR S) (CONS (CAR SI) 1)))
                                       (PUT EN 'HISTRY_
                                            (REVAL1
                                             (LIST 'TIMES
                                                   (LIST 'QUOTIENT
                                                         (GET EN 'HISTRY_)
                                                         (PREPF (CAR SI)))
                                                   (PREPF (CAAR S)))
                                             T))))
                                     (SETQ SF (CONS (CAR S) SF)))))
                                  (SETQ SI (CDR SI)))
                                 (GO WHILELABEL))
                               (COND
                                ((NULL SF) (SETQ SF (LIST (CONS 1 1)))))))))))
               (COND
                ((AND SEPA VL_ (NULL FCTR) (NEQ SF (LIST (CONS 0 1)))
                      (NEQ SF (LIST (CONS 1 1))) (CDR (CAR (CAR SF)))
                      (PROGN
                       (SETQ SI (KERNELS (CAR (CAR SF))))
                       (SETDIFF (SMEMBERL VL_ SI)
                                (ARGSET (SMEMBERL FTEM SI)))))
                 (PROGN
                  (SETQ PRINT_BAK PRINT_)
                  (SETQ PRINT_ NIL)
                  (SETQ SI
                          (SEPAR (REVAL1 (LIST '*SQ (CAR SF) NIL) T) FTEM VL_
                           NIL NIL))
                  (COND
                   ((CDR SI)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT SI) (RETURN NIL)))
                      (COND
                       ((EQUAL (LIST (CONS 1 1))
                               (SIMPLIFYSQ (SIMP (CDAR SI)) FTEM T NIL NIL))
                        (PROGN (SETQ SI NIL) (SETQ SF (LIST (CONS 1 1)))))
                       (T (SETQ SI (CDR SI))))
                      (GO WHILELABEL))))
                  (SETQ PRINT_ PRINT_BAK))))
               SF)))))) 
(PUT 'SIMPLIFYPDESQ 'NUMBER-OF-ARGS 5) 
(PUT 'SIMPLIFYPDESQ 'DEFINED-ON-LINE '178) 
(PUT 'SIMPLIFYPDESQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMPLIFYPDESQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPLIFYPDESQ (P FTEM FCTR EN SEPA)
    (PROG (L)
      (SETQ L (SIMPLIFYSQ (SUBS2 P) FTEM FCTR EN SEPA))
      (RETURN
       (COND
        ((EQUAL L (LIST (CONS 1 1)))
         (PROGN (SETQ CONTRADICTION_ T) (CONS (CAR L) 1)))
        ((NULL (CDR L)) (CONS (CAR L) 1))
        (T
         (PROGN
          (SETQ P (CAR L))
          (PROG (H)
            (SETQ H (CDR L))
           LAB
            (COND ((NULL H) (RETURN NIL)))
            ((LAMBDA (H) (SETQ P (MULTSQ P H))) (CAR H))
            (SETQ H (CDR H))
            (GO LAB))
          (CONS P L))))))) 
(PUT 'CAN_NOT_BECOME_ZEROSQ 'NUMBER-OF-ARGS 2) 
(PUT 'CAN_NOT_BECOME_ZEROSQ 'DEFINED-ON-LINE '204) 
(PUT 'CAN_NOT_BECOME_ZEROSQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'CAN_NOT_BECOME_ZEROSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CAN_NOT_BECOME_ZEROSQ (P FTEM)
    (COND ((EQUAL (SIMPLIFYSQ P FTEM T NIL T) (LIST (CONS 1 1))) T) (T NIL))) 
(PUT 'ADD_TO_INEQ_OR 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_TO_INEQ_OR 'DEFINED-ON-LINE '210) 
(PUT 'ADD_TO_INEQ_OR 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'ADD_TO_INEQ_OR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_TO_INEQ_OR (NEW_OR INEQ_OR_TMP)
    (PROG (IOCP)
      (SETQ IOCP INEQ_OR_TMP)
      (PROG ()
       WHILELABEL
        (COND ((NOT IOCP) (RETURN NIL)))
        (COND
         ((NULL (NOT_INCLUDED (CAR IOCP) NEW_OR))
          (PROGN (SETQ NEW_OR NIL) (SETQ IOCP NIL)))
         (T
          (PROGN
           (COND
            ((NULL (NOT_INCLUDED NEW_OR (CAR IOCP)))
             (DELETE (CAR IOCP) INEQ_OR_TMP)))
           (SETQ IOCP (CDR IOCP)))))
        (GO WHILELABEL))
      (COND (NEW_OR (SETQ INEQ_OR_TMP (CONS NEW_OR INEQ_OR_TMP))))
      (RETURN INEQ_OR_TMP))) 
(PUT 'SIMPSQINEQ_OR_ADHOC 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPSQINEQ_OR_ADHOC 'DEFINED-ON-LINE '229) 
(PUT 'SIMPSQINEQ_OR_ADHOC 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMPSQINEQ_OR_ADHOC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPSQINEQ_OR_ADHOC (PDES)
    (PROG (L1 L1CP L2 L2CP L3 ICP KEEP_L1 NEW_SCALAR_INEQ IS_ZERO)
      (SETQ ICP INEQ_OR)
      (SETQ INEQ_OR NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT ICP) (RETURN NIL)))
        (PROGN
         (SETQ L1 (CAR ICP))
         (SETQ ICP (CDR ICP))
         (SETQ KEEP_L1 T)
         (SETQ L1CP L1)
         (SETQ L1 NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT L1CP) (RETURN NIL)))
           (PROGN
            (SETQ L2 (CAR L1CP))
            (SETQ L1CP (CDR L1CP))
            (SETQ L2CP NIL)
            (SETQ IS_ZERO NIL)
            (PROG (L3)
              (SETQ L3 L2)
             LAB
              (COND ((NULL L3) (RETURN NIL)))
              ((LAMBDA (L3)
                 (PROGN
                  (SETQ L3 (SIMPLIFYSQ L3 FTEM_ T NIL T))
                  (COND ((EQUAL L3 (LIST (CONS NIL 1))) (SETQ IS_ZERO T)))
                  (COND
                   ((NEQ L3 (LIST (CONS 1 1))) (SETQ L2CP (UNION L3 L2CP))))))
               (CAR L3))
              (SETQ L3 (CDR L3))
              (GO LAB))
            (COND
             ((NULL IS_ZERO)
              (COND ((NULL L2CP) (PROGN (SETQ KEEP_L1 NIL) (SETQ L1CP NIL)))
                    (T (SETQ L1 (ADD_TO_INEQ_OR L2CP L1)))))))
           (GO WHILELABEL))
         (COND
          (KEEP_L1
           (COND ((NULL L1) (SETQ CONTRADICTION_ T))
                 ((NULL (CDR L1))
                  (SETQ NEW_SCALAR_INEQ (UNION (CAR L1) NEW_SCALAR_INEQ)))
                 (T (SETQ INEQ_OR (CONS L1 INEQ_OR)))))))
        (GO WHILELABEL))
      (COND
       ((NULL CONTRADICTION_)
        (PROG (L2)
          (SETQ L2 NEW_SCALAR_INEQ)
         LAB
          (COND ((NULL L2) (RETURN NIL)))
          ((LAMBDA (L2) (ADDSQINEQ PDES L2 NIL)) (CAR L2))
          (SETQ L2 (CDR L2))
          (GO LAB)))))) 
(PUT 'SIMPSQINEQ_OR 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPSQINEQ_OR 'DEFINED-ON-LINE '287) 
(PUT 'SIMPSQINEQ_OR 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMPSQINEQ_OR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPSQINEQ_OR (PDES NEWIN)
    (PROG (L1 L1CP L2 L2CP L3 ICP KEEP_L1 NEW_SCALAR_INEQ)
      (SETQ ICP INEQ_OR)
      (SETQ INEQ_OR NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT ICP) (RETURN NIL)))
        (PROGN
         (SETQ L1 (CAR ICP))
         (SETQ ICP (CDR ICP))
         (SETQ KEEP_L1 T)
         (SETQ L1CP L1)
         (PROG ()
          WHILELABEL
           (COND ((NOT L1CP) (RETURN NIL)))
           (PROGN
            (SETQ L2 (CAR L1CP))
            (SETQ L1CP (CDR L1CP))
            (SETQ L2CP L2)
            (PROG (L3)
              (SETQ L3 L2)
             LAB
              (COND ((NULL L3) (RETURN NIL)))
              ((LAMBDA (L3)
                 (COND ((EQUAL NEWIN L3) (SETQ L2CP (DELETE L3 L2CP)))))
               (CAR L3))
              (SETQ L3 (CDR L3))
              (GO LAB))
            (COND ((NULL L2CP) (PROGN (SETQ KEEP_L1 NIL) (SETQ L1CP NIL)))
                  ((NEQ L2CP L2) (SETQ L1 (ADD_TO_INEQ_OR L2CP L1)))))
           (GO WHILELABEL))
         (COND
          (KEEP_L1
           (COND
            ((NULL (CDR L1))
             (SETQ NEW_SCALAR_INEQ (UNION (CAR L1) NEW_SCALAR_INEQ)))
            (T (SETQ INEQ_OR (CONS L1 INEQ_OR)))))))
        (GO WHILELABEL))
      (PROG (L2)
        (SETQ L2 NEW_SCALAR_INEQ)
       LAB
        (COND ((NULL L2) (RETURN NIL)))
        ((LAMBDA (L2) (ADDSQINEQ PDES L2 NIL)) (CAR L2))
        (SETQ L2 (CDR L2))
        (GO LAB)))) 
(PUT 'ALL_SUB_DERIVATIVES 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_SUB_DERIVATIVES 'DEFINED-ON-LINE '341) 
(PUT 'ALL_SUB_DERIVATIVES 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'ALL_SUB_DERIVATIVES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_SUB_DERIVATIVES (A)
    (COND ((NULL (CDDR A)) (LIST (CADR A)))
          (T
           (PROG (F X N DL DLDONE ALL_DERI)
             (SETQ F (CADR A))
             (SETQ DL (CDDR A))
             (PROG ()
              WHILELABEL
               (COND ((NOT DL) (RETURN NIL)))
               (PROGN
                (SETQ X (CAR DL))
                (SETQ DL (CDR DL))
                (COND ((NULL DL) (SETQ N 1))
                      ((FIXP (CAR DL))
                       (PROGN (SETQ N (CAR DL)) (SETQ DL (CDR DL))))
                      (T (SETQ N 1)))
                (SETQ ALL_DERI
                        (UNION
                         (ALL_SUB_DERIVATIVES
                          (CONS 'DF
                                (CONS F
                                      (APPEND DLDONE
                                              (COND ((EQUAL N 1) DL)
                                                    ((EQUAL N 2) (CONS X DL))
                                                    (T
                                                     (CONS X
                                                           (CONS (SUB1 N)
                                                                 DL))))))))
                         ALL_DERI))
                (COND ((EQUAL N 1) (SETQ DLDONE (NCONC DLDONE (LIST X))))
                      (T (SETQ DLDONE (NCONC DLDONE (LIST X N))))))
               (GO WHILELABEL))
             (RETURN (CONS A ALL_DERI)))))) 
(PUT 'CHECKCASE2SEP 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKCASE2SEP 'DEFINED-ON-LINE '370) 
(PUT 'CHECKCASE2SEP 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'CHECKCASE2SEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKCASE2SEP (PDES NEWINESQ)
    (PROG (H P)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((EQUAL NEWINESQ (GET P 'CASE2SEP))
             (PROGN
              (PUT P 'CASE2SEP NIL)
              (SETQ H (STARDEP3 (GET P 'VARS) (GET P 'KERN) (GET P 'DERIVS)))
              (COND
               (H
                (PROGN
                 (PUT P 'STARDE (LIST (CONS 0 (CAR H))))
                 (FLAG (LIST P) 'TO_SEP))))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB)))) 
(PUT 'ADDSQINEQ 'NUMBER-OF-ARGS 3) 
(PUT 'ADDSQINEQ 'DEFINED-ON-LINE '385) 
(PUT 'ADDSQINEQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'ADDSQINEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDSQINEQ (PDES NEWINEQ SIMPLI)
    (PROG (H1 H2 H3 P Q)
      (COND
       ((NULL LIN_PROBLEM)
        (PROG (H2)
          (SETQ H2 PDES)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2)
             (COND
              ((NULL (GET H2 'LINEAR_))
               (PROGN
                (COND
                 ((AND (NULL (FLAGP H2 'TO_GENSEP)) (GET H2 'STARDE))
                  (FLAG (LIST H2) 'TO_GENSEP)))
                (COND ((NULL (FLAGP H2 'TO_INT)) (FLAG (LIST H2) 'TO_INT)))
                (COND
                 ((NULL (FLAGP H2 'TO_FULLINT))
                  (FLAG (LIST H2) 'TO_FULLINT)))))))
           (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB))))
      (COND (SIMPLI (SETQ H1 (SIMPLIFYSQ NEWINEQ FTEM_ T NIL T)))
            (T (SETQ H1 (LIST NEWINEQ))))
      (COND
       ((NULL (CDR H1))
        (COND
         ((SQZEROP (CAR H1))
          (RETURN
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (PROGN
                (PRIN2 "Added inequality is actually zero --> contradiction")
                NIL)
               (TERPRI))))
            (SETQ CONTRADICTION_ T)
            NIL)))
         ((OR (ATOM (CAR H1)) (ATOM (CAR (CAR H1)))) (RETURN NIL)))))
      (SETQ H3 NIL)
      (COND
       (VL_
        (PROG (H2)
          (SETQ H2 H1)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2)
             (COND
              ((AND (ONE_TERMPSF (CAR H2)) (PAIRP (CAR H2))
                    (EQUAL (CDAR (CAR H2)) 1))
               (PROGN
                (CHECKCASE2SEP PDES H2)
                (COND
                 ((AND (PAIRP (CAAAR (CAR H2)))
                       (EQUAL (CAR (CAAAR (CAR H2))) 'DF))
                  (SETQ H3
                          (UNION (CDR (ALL_SUB_DERIVATIVES (CAAAR (CAR H2))))
                                 H3))))
                NIL))))
           (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB))))
      (PROG (H2)
        (SETQ H2 H3)
       LAB
        (COND ((NULL H2) (RETURN NIL)))
        ((LAMBDA (H2)
           (PROGN
            (SETQ H2 (MKSQ H2 1))
            (CHECKCASE2SEP PDES H2)
            (SETQ H1 (CONS H2 H1))
            NIL))
         (CAR H2))
        (SETQ H2 (CDR H2))
        (GO LAB))
      (SETQ H3 NIL)
      (PROG (H2)
        (SETQ H2 H1)
       LAB
        (COND ((NULL H2) (RETURN NIL)))
        ((LAMBDA (H2)
           (COND
            ((NOT (MEMBER H2 INEQ_))
             (PROGN (SETQ H3 (CONS H2 H3)) (SETQ INEQ_ (CONS H2 INEQ_))))))
         (CAR H2))
        (SETQ H2 (CDR H2))
        (GO LAB))
      (COND
       ((AND PRINT_ H3)
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 "The list of inequalities got extended by: ") NIL)
         (PROG (H2)
           (SETQ H2 H3)
          LAB
           (COND ((NULL H2) (RETURN NIL)))
           ((LAMBDA (H2)
              (COND
               ((GREATERP (DELENGTHSQ H2) PRINT_)
                (PROGN
                 (TERPRI)
                 (PROGN
                  (PRIN2 "An expression with ")
                  (PRIN2 (NO_OF_TERMS H2))
                  (PRIN2 " terms")
                  NIL)
                 (TERPRI)))
               (T (MATHPRINT (LIST '*SQ H2 T)))))
            (CAR H2))
           (SETQ H2 (CDR H2))
           (GO LAB)))))
      (COND
       (PDES
        (PROG (H2)
          (SETQ H2 H3)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2) (UPDATESQFCTEVAL PDES H2)) (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB))))
      (COND
       ((AND PDES (NULL LIN_PROBLEM))
        (PROG (H2)
          (SETQ H2 H3)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2)
             (COND
              ((AND (NO_NUMBER_ATOM_SQ H2)
                    (MEMBER (SETQ H1 (CAAAR (CAR H2))) FTEM_))
               (PROGN
                (GIVE_LOW_PRIORITY PDES H1)
                (PROG (P)
                  (SETQ P PDES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (COND
                      ((AND (NOT (GET P 'LINEAR_))
                            (NOT (FREEOF (GET P 'FCTS) H1)))
                       (PROGN
                        (PUT P 'RL_WITH NIL)
                        (PROG (Q)
                          (SETQ Q PDES)
                         LAB
                          (COND ((NULL Q) (RETURN NIL)))
                          ((LAMBDA (Q) (COND ((NEQ Q P) (DROP_RL_WITH P Q))))
                           (CAR Q))
                          (SETQ Q (CDR Q))
                          (GO LAB))
                        (SETQ Q (GET P 'FCT_HOM))
                        (COND
                         ((AND (EQUAL (LENGTH Q) 2) (FIXP (GET P 'FAC))
                               (EQUAL (LENGTH (GET P 'FCTS)) 2)
                               (EQUAL (CAR (GET P 'HOM_DEG)) 0))
                          (COND
                           ((EQUAL H1 (CAR Q))
                            (ADDSQINEQ PDES (SIMP (CADR Q)) NIL))
                           (T (ADDSQINEQ PDES (SIMP (CAR Q)) NIL)))))))))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))))))
           (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB))))
      (COND
       ((AND H3 PDES)
        (PROG (H2)
          (SETQ H2 PDES)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2)
             (COND ((EQUAL (GET H2 'TERMS) 2) (NEW_INEQ_FROM_EQU_SQ H2 PDES))))
           (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB))))
      (COND
       (INEQ_OR
        (PROG (H2)
          (SETQ H2 H3)
         LAB
          (COND ((NULL H2) (RETURN NIL)))
          ((LAMBDA (H2) (SIMPSQINEQ_OR PDES H2)) (CAR H2))
          (SETQ H2 (CDR H2))
          (GO LAB)))))) 
(PUT 'SIMP_INEQ_WITH_PDES 'NUMBER-OF-ARGS 3) 
(PUT 'SIMP_INEQ_WITH_PDES 'DEFINED-ON-LINE '513) 
(PUT 'SIMP_INEQ_WITH_PDES 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMP_INEQ_WITH_PDES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMP_INEQ_WITH_PDES (A1 AVOID PDES)
    (COND
     (A1
      (COND ((SQZEROP (CAR A1)) (LIST (CAR A1)))
            (T
             (PROG (A A2 OCCUR P K FNS KERNELLIST AKERNELS FTEM_CP OK AK)
               (PROG (A)
                 (SETQ A A1)
                LAB
                 (COND ((NULL A) (RETURN NIL)))
                 ((LAMBDA (A)
                    (COND
                     ((OR (AND FHOM_ (NOT (FREEOFLIST A FLIN_)))
                          (MORE_THAN_X_TERMS (CAR A) MAX_TERM_TO_PRED)
                          (AND (NULL ALG_POLY)
                               (PROGN
                                (SETQ AKERNELS (KERNELS (CAR A)))
                                (SETQ OK T)
                                (PROG (AK)
                                  (SETQ AK AKERNELS)
                                 LAB
                                  (COND ((NULL AK) (RETURN NIL)))
                                  ((LAMBDA (AK)
                                     (COND
                                      (OK
                                       (PROGN
                                        (SETQ FTEM_CP FTEM_)
                                        (PROG ()
                                         WHILELABEL
                                          (COND
                                           ((NOT
                                             (AND FTEM_CP
                                                  (OR (FREEOF AK (CAR FTEM_CP))
                                                      (POLYNOP AK
                                                       (CAR FTEM_CP)))))
                                            (RETURN NIL)))
                                          (SETQ FTEM_CP (CDR FTEM_CP))
                                          (GO WHILELABEL))
                                        (COND (FTEM_CP (SETQ OK NIL)))))))
                                   (CAR AK))
                                  (SETQ AK (CDR AK))
                                  (GO LAB))
                                (COND (OK NIL) (T T)))))
                      (SETQ A2 (CONS A A2)))
                     (T
                      (PROGN
                       (SETQ OCCUR NIL)
                       (PROG (P)
                         (SETQ P PDES)
                        LAB
                         (COND ((NULL P) (RETURN NIL)))
                         ((LAMBDA (P)
                            (COND
                             ((AND
                               (OR (NULL FHOM_) (ZEROP (CAR (GET P 'HOM_DEG))))
                               (LEQ (GET P 'TERMS) MAX_TERM_TO_PRED)
                               (NULL (GET P 'STARDE))
                               (NULL (GET P 'NONRATIONAL))
                               (NOT (FREEOF A (CAAAR (GET P 'DERIVS))))
                               (OR (NULL AVOID) (FREEOF (GET P 'FCTS) AVOID))
                               (FREEOF (GET P 'KERN) 'EXPT))
                              (PROGN
                               (COND
                                ((NOT ALG_POLY)
                                 (PROGN
                                  (SETQ FNS (GET P 'FCTS))
                                  (PROG (K)
                                    (SETQ K (GET P 'KERN))
                                   LAB
                                    (COND ((NULL K) (RETURN NIL)))
                                    ((LAMBDA (K)
                                       (COND
                                        ((NOT (FREEOFLIST K FNS))
                                         (SETQ KERNELLIST
                                                 (UNION (LIST K)
                                                        KERNELLIST)))))
                                     (CAR K))
                                    (SETQ K (CDR K))
                                    (GO LAB)))))
                               (SETQ OCCUR
                                       (CONS (LIST '*SQ (GET P 'SQVAL) T)
                                             OCCUR))
                               NIL))))
                          (CAR P))
                         (SETQ P (CDR P))
                         (GO LAB))
                       (COND
                        (OCCUR
                         (PROGN
                          (COND
                           (ALG_POLY
                            (AEVAL (LIST 'TORDER (CONS 'LIST FTEM_) 'LEX)))
                           (T
                            (PROGN
                             (PROG (K)
                               (SETQ K AKERNELS)
                              LAB
                               (COND ((NULL K) (RETURN NIL)))
                               ((LAMBDA (K)
                                  (COND
                                   ((NOT (FREEOFLIST K FTEM_))
                                    (SETQ KERNELLIST
                                            (UNION (LIST K) KERNELLIST)))))
                                (CAR K))
                               (SETQ K (CDR K))
                               (GO LAB))
                             (AEVAL
                              (LIST 'TORDER (CONS 'LIST KERNELLIST) 'LEX))
                             NIL)))
                          (SETQ P
                                  (ERR_CATCH_PREDUCE (LIST '*SQ A NIL)
                                   (CONS 'LIST OCCUR)))
                          (SETQ A2
                                  (COND
                                   (P
                                    (APPEND (SIMPLIFYSQ (SIMP P) FTEM_ T NIL T)
                                            A2))
                                   (T (CONS A A2))))))
                        (T (SETQ A2 (CONS A A2))))))))
                  (CAR A))
                 (SETQ A (CDR A))
                 (GO LAB))
               (RETURN A2))))))) 
(PUT 'SIMP_INEQ_WITH_EQU_SQ 'NUMBER-OF-ARGS 3) 
(PUT 'SIMP_INEQ_WITH_EQU_SQ 'DEFINED-ON-LINE '586) 
(PUT 'SIMP_INEQ_WITH_EQU_SQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMP_INEQ_WITH_EQU_SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMP_INEQ_WITH_EQU_SQ (L EQN PDES)
    (COND
     ((AND (EQUAL (GET EQN 'TERMS) 1)
           (NO_NUMBER_ATOM_SF (CAR (GET EQN 'SQVAL))))
      (SIMP_INEQ_WITH_SUBST_SQ L 0 (CAAAR (CAR (GET EQN 'SQVAL))) PDES))
     (T
      (PROG (F A A1 A2 KEPT NEWIN)
        (SETQ F (CAAAR (GET EQN 'DERIVS)))
        (PROG ()
         WHILELABEL
          (COND ((NOT L) (RETURN NIL)))
          (PROGN
           (COND
            ((OR (FREEOF (CAR L) F)
                 (AND (ONE_TERMPSF (CAR (CAR L))) (LESSP 2 (GET EQN 'TERMS)))
                 (EQUAL (MORE_THAN_X_TERMS (CAR (CAR L)) MAX_TERM_TO_PRED)
                        NIL))
             (SETQ KEPT (CONS (CAR L) KEPT)))
            (T
             (PROGN
              (SETQ A1
                      (ERR_CATCH_PREDUCE (LIST '*SQ (CAR L) T)
                       (LIST 'LIST (LIST '*SQ (GET EQN 'SQVAL) T))))
              (SETQ A1
                      (SIMPLIFYSQ (COND (A1 (SIMP A1)) (T (CAR L))) FTEM_ T NIL
                       T))
              (COND
               ((OR (CDR A1) (NEQ (CAR A1) (CONS 1 1)))
                (COND
                 ((AND (NULL (CDR A1)) (EQUAL (CAR A1) (CAR L)))
                  (SETQ KEPT (CONS (CAR L) KEPT)))
                 (T
                  (PROGN
                   (SETQ A2 (SIMP_INEQ_WITH_PDES A1 NIL PDES))
                   (PROG (A)
                     (SETQ A A2)
                    LAB
                     (COND ((NULL A) (RETURN NIL)))
                     ((LAMBDA (A)
                        (COND
                         ((SQZEROP A)
                          (PROGN
                           (COND
                            (PRINT_
                             (PROGN
                              (TERPRI)
                              (PROGN (PRIN2 "The new equation:") NIL)
                              (EQPRINT
                               (LIST 'EQUAL 0 (LIST '*SQ (GET EQN 'SQVAL) T)))
                              (PROGN (PRIN2 "transformed one factor:") NIL)
                              (EQPRINT (LIST '*SQ (CAR L) T))
                              (PROGN
                               (PRIN2
                                "of one expression within one OR-inequality modulo")
                               NIL)
                              (TERPRI)
                              (PROGN
                               (PRIN2 "all other equations to zero.")
                               NIL)
                              (TERPRI))))
                           (SETQ L (LIST (CAR L)))
                           (SETQ KEPT NIL)
                           (SETQ NEWIN (LIST (CONS 0 1)))))
                         ((NEQ A (CONS 1 1)) (SETQ NEWIN (CONS A NEWIN)))))
                      (CAR A))
                     (SETQ A (CDR A))
                     (GO LAB)))))))
              NIL)))
           (SETQ L (CDR L)))
          (GO WHILELABEL))
        (RETURN (CONS KEPT NEWIN)))))) 
(PUT 'SIMP_ALL_INEQ_WITH_EQU_SQ 'NUMBER-OF-ARGS 2) 
(PUT 'SIMP_ALL_INEQ_WITH_EQU_SQ 'DEFINED-ON-LINE '667) 
(PUT 'SIMP_ALL_INEQ_WITH_EQU_SQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMP_ALL_INEQ_WITH_EQU_SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMP_ALL_INEQ_WITH_EQU_SQ (EQN PDES)
    (COND
     ((AND ALG_POLY (LESSP (GET EQN 'TERMS) MAX_TERM_TO_PRED)
           (FREEOF (GET EQN 'KERN) 'EXPT) (NULL (GET EQN 'STARDE))
           (GET EQN 'DERIVS))
      (PROG (L L1 L2 A NEWOR)
        (COND (ALG_POLY (AEVAL (LIST 'TORDER (CONS 'LIST FTEM_) 'LEX)))
              (T (PROGN NIL)))
        (SETQ L1 INEQ_)
        (SETQ INEQ_ NIL)
        (SETQ L (SIMP_INEQ_WITH_EQU_SQ L1 EQN PDES))
        (COND
         ((EQUAL (CDR L) (LIST (CONS 0 1)))
          (PROGN (SETQ INEQ_ L1) (SETQ CONTRADICTION_ T)))
         (T
          (PROGN
           (SETQ INEQ_ (CAR L))
           (PROG (A)
             (SETQ A (CDR L))
            LAB
             (COND ((NULL A) (RETURN NIL)))
             ((LAMBDA (A) (ADDSQINEQ PDES A NIL)) (CAR A))
             (SETQ A (CDR A))
             (GO LAB))
           (SETQ L INEQ_OR)
           (SETQ INEQ_OR NIL)
           (PROG ()
            WHILELABEL
             (COND ((NOT L) (RETURN NIL)))
             (PROGN
              (SETQ NEWOR NIL)
              (SETQ L1 (CAR L))
              (PROG ()
               WHILELABEL
                (COND ((NOT L1) (RETURN NIL)))
                (PROGN
                 (SETQ L2 (CAR L1))
                 (SETQ L1 (CDR L1))
                 (SETQ L2 (SIMP_INEQ_WITH_EQU_SQ L2 EQN PDES))
                 (COND
                  ((NEQ (CDR L2) (LIST (CONS 0 1)))
                   (COND
                    ((AND (NULL (CAR L2)) (NULL (CDR L2)))
                     (PROGN
                      (SETQ NEWOR (LIST (LIST (CONS 1 1))))
                      (SETQ L1 NIL)))
                    (T
                     (SETQ NEWOR
                             (UNION (LIST (NCONC (CAR L2) (CDR L2)))
                                    NEWOR)))))))
                (GO WHILELABEL))
              (COND
               ((NULL NEWOR)
                (PROGN
                 (COND
                  (PRINT_
                   (PROGN
                    (TERPRI)
                    (PROGN (PRIN2 "Contradiction: The equation:") NIL)
                    (EQPRINT (LIST 'EQUAL 0 (LIST '*SQ (GET EQN 'SQVAL) T)))
                    (PROGN
                     (PRIN2
                      "transformed all expressions, each given as a list of factors:")
                     NIL)
                    (MATHPRINT
                     (CONS 'LIST
                           (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L1 (CAR L))
                             (COND ((NULL L1) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L1)
                                                 (CONS 'LIST
                                                       (PROG (L2 FORALL-RESULT
                                                              FORALL-ENDPTR)
                                                         (SETQ L2 L1)
                                                         (COND
                                                          ((NULL L2)
                                                           (RETURN NIL)))
                                                         (SETQ FORALL-RESULT
                                                                 (SETQ FORALL-ENDPTR
                                                                         (CONS
                                                                          ((LAMBDA
                                                                               (
                                                                                L2)
                                                                             (LIST
                                                                              '*SQ
                                                                              L2
                                                                              T))
                                                                           (CAR
                                                                            L2))
                                                                          NIL)))
                                                        LOOPLABEL
                                                         (SETQ L2 (CDR L2))
                                                         (COND
                                                          ((NULL L2)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (RPLACD FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (L2)
                                                                     (LIST '*SQ
                                                                           L2
                                                                           T))
                                                                   (CAR L2))
                                                                  NIL))
                                                         (SETQ FORALL-ENDPTR
                                                                 (CDR
                                                                  FORALL-ENDPTR))
                                                         (GO LOOPLABEL))))
                                               (CAR L1))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L1 (CDR L1))
                             (COND ((NULL L1) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (L1)
                                         (CONS 'LIST
                                               (PROG (L2 FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ L2 L1)
                                                 (COND
                                                  ((NULL L2) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (L2)
                                                                     (LIST '*SQ
                                                                           L2
                                                                           T))
                                                                   (CAR L2))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ L2 (CDR L2))
                                                 (COND
                                                  ((NULL L2)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (L2)
                                                             (LIST '*SQ L2 T))
                                                           (CAR L2))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL))))
                                       (CAR L1))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                    (PROGN
                     (PRIN2
                      "within one OR-inequality modulo all other equations to zero.")
                     NIL)
                    (TERPRI))))
                 (SETQ CONTRADICTION_ T)
                 (SETQ L (LIST NIL))))
               ((NULL (CDR NEWOR))
                (COND
                 ((NEQ (CAR NEWOR) (LIST (CONS 1 1)))
                  (PROGN
                   (COND
                    (PRINT_
                     (PROGN
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        "Due to vanishing expressions an OR-inequality is now")
                       NIL)
                      (TERPRI)
                      (PROGN
                       (PRIN2
                        "converted to a normal inequality where each of the")
                       NIL)
                      (TERPRI)
                      (PROGN (PRIN2 "following factors must not vanish: ") NIL)
                      (MATHPRINT
                       (CONS 'LIST
                             (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                               (SETQ L2 (CAR NEWOR))
                               (COND ((NULL L2) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (L2) (LIST '*SQ L2 T))
                                                 (CAR L2))
                                                NIL)))
                              LOOPLABEL
                               (SETQ L2 (CDR L2))
                               (COND ((NULL L2) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L2) (LIST '*SQ L2 T))
                                         (CAR L2))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))
                      NIL)))
                   (PROG (L2)
                     (SETQ L2 (CAR NEWOR))
                    LAB
                     (COND ((NULL L2) (RETURN NIL)))
                     ((LAMBDA (L2) (ADDSQINEQ PDES L2 NIL)) (CAR L2))
                     (SETQ L2 (CDR L2))
                     (GO LAB))))
                 (T NIL)))
               (T (SETQ INEQ_OR (CONS NEWOR INEQ_OR))))
              (SETQ L (CDR L)))
             (GO WHILELABEL))))))))) 
(PUT 'NEW_INEQ_FROM_EQU_SQ 'NUMBER-OF-ARGS 2) 
(PUT 'NEW_INEQ_FROM_EQU_SQ 'DEFINED-ON-LINE '748) 
(PUT 'NEW_INEQ_FROM_EQU_SQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'NEW_INEQ_FROM_EQU_SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEW_INEQ_FROM_EQU_SQ (EQU PDES)
    (COND
     ((AND PDES (NULL LIN_PROBLEM) (FIXP (GET EQU 'FAC)))
      (COND
       ((EQUAL (GET EQU 'TERMS) 2)
        (PROG (SF T1 T2)
          (SETQ SF (CAR (GET EQU 'SQVAL)))
          (SETQ T1 (FIRST_TERM_SF SF))
          (SETQ T2
                  (COND ((CDR SF) (FIRST_TERM_SF (CDR SF)))
                        (T (FIRST_TERM_SF (ADDF SF (NEGF T1))))))
          (SETQ T1 (SIMPLIFYSQ (CONS T1 1) (GET EQU 'FCTS) T NIL NIL))
          (SETQ T2 (SIMPLIFYSQ (CONS T2 1) (GET EQU 'FCTS) T NIL NIL))
          (COND
           ((EQUAL T1 (LIST (CONS 1 1)))
            (COND
             ((NEQ T2 (LIST (CONS 1 1)))
              (PROG (H)
                (SETQ H T2)
               LAB
                (COND ((NULL H) (RETURN NIL)))
                ((LAMBDA (H) (ADDSQINEQ PDES H NIL)) (CAR H))
                (SETQ H (CDR H))
                (GO LAB)))
             (T NIL)))
           ((EQUAL T2 (LIST (CONS 1 1)))
            (PROG (H)
              (SETQ H T1)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H) (ADDSQINEQ PDES H NIL)) (CAR H))
              (SETQ H (CDR H))
              (GO LAB))))))
       ((AND (NULL (GET EQU 'FCT_HOM)) (EQUAL (GET EQU 'NVARS) 0))
        (PROG (SF T1 T2 H)
          (SETQ SF (CAR (GET EQU 'SQVAL)))
          (SETQ T1 (NUM_TERM_SF SF))
          (COND
           (T1
            (PROGN
             (SETQ T2 (ADDF SF (NEGF T1)))
             (SETQ T2 (SIMPLIFYSQ (CONS T2 1) (GET EQU 'FCTS) T NIL NIL))
             (COND
              ((CDR T2)
               (PROG (H)
                 (SETQ H T2)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (COND ((LESSP (NO_OF_TM_SF H) 5) (ADDSQINEQ PDES H NIL))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))))))))))))) 
(PUT 'SIMP_INEQ_WITH_SUBST_SQ 'NUMBER-OF-ARGS 4) 
(PUT 'SIMP_INEQ_WITH_SUBST_SQ 'DEFINED-ON-LINE '788) 
(PUT 'SIMP_INEQ_WITH_SUBST_SQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMP_INEQ_WITH_SUBST_SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMP_INEQ_WITH_SUBST_SQ (L NEW OLD PDES)
    (PROG (A A1 A2 KEPT NEWIN)
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (COND ((FREEOF (CAR L) OLD) (SETQ KEPT (CONS (CAR L) KEPT)))
               (T
                (PROGN
                 (SETQ A1
                         (SIMPLIFYSQ (SUBSQ (CAR L) (LIST (CONS OLD NEW)))
                          FTEM_ T NIL T))
                 (COND
                  ((OR (CDR A1) (NEQ (CAR A1) (CONS 1 1)))
                   (PROGN
                    (SETQ A2 (SIMP_INEQ_WITH_PDES A1 OLD PDES))
                    (PROG (A)
                      (SETQ A A2)
                     LAB
                      (COND ((NULL A) (RETURN NIL)))
                      ((LAMBDA (A)
                         (COND
                          ((SQZEROP A)
                           (PROGN
                            (COND
                             (PRINT_
                              (PROGN
                               (TERPRI)
                               (PROGN (PRIN2 "The substitution:") NIL)
                               (EQPRINT (LIST 'EQUAL OLD NEW))
                               (PROGN (PRIN2 "transformed one factor:") NIL)
                               (EQPRINT (LIST '*SQ (CAR L) T))
                               (PROGN
                                (PRIN2
                                 "of one expression within one OR-inequality modulo")
                                NIL)
                               (TERPRI)
                               (PROGN
                                (PRIN2 "all other equations to zero.")
                                NIL)
                               (TERPRI))))
                            (SETQ L (LIST (CAR L)))
                            (SETQ KEPT NIL)
                            (SETQ NEWIN (LIST (CONS 0 1)))))
                          ((NEQ A (CONS 1 1)) (SETQ NEWIN (CONS A NEWIN)))))
                       (CAR A))
                      (SETQ A (CDR A))
                      (GO LAB)))))
                 NIL)))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN (CONS KEPT NEWIN)))) 
(PUT 'SIMP_ALL_INEQ_WITH_SUBST_SQ 'NUMBER-OF-ARGS 3) 
(PUT 'SIMP_ALL_INEQ_WITH_SUBST_SQ 'DEFINED-ON-LINE '844) 
(PUT 'SIMP_ALL_INEQ_WITH_SUBST_SQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'SIMP_ALL_INEQ_WITH_SUBST_SQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMP_ALL_INEQ_WITH_SUBST_SQ (NEW OLD PDES)
    (PROG (L L1 L2 L3 A NEWOR)
      (COND (ALG_POLY (AEVAL (LIST 'TORDER (CONS 'LIST FTEM_) 'LEX)))
            (T (PROGN NIL)))
      (SETQ L1 INEQ_)
      (SETQ INEQ_ NIL)
      (SETQ L (SIMP_INEQ_WITH_SUBST_SQ L1 NEW OLD PDES))
      (COND
       ((EQUAL (CDR L) (LIST (CONS 0 1)))
        (PROGN (SETQ INEQ_ L1) (SETQ CONTRADICTION_ T)))
       (T
        (PROGN
         (SETQ INEQ_ (CAR L))
         (PROG (A)
           (SETQ A (CDR L))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (ADDSQINEQ PDES A NIL)) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ L INEQ_OR)
         (SETQ INEQ_OR NIL)
         (COND
          ((AND (MEMBER OLD FLIN_) (MEMBER OLD FHOM_))
           (SETQ L3 (LIST (SIMP OLD)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ L1 (CAR L))
            (COND ((AND L3 (MEMBER L3 L1)) (SETQ NEWOR (DELETE L3 L1)))
                  (T
                   (PROGN
                    (SETQ NEWOR NIL)
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT L1) (RETURN NIL)))
                      (PROGN
                       (SETQ L2 (CAR L1))
                       (SETQ L1 (CDR L1))
                       (SETQ L2 (SIMP_INEQ_WITH_SUBST_SQ L2 NEW OLD PDES))
                       (COND
                        ((NEQ (CDR L2) (LIST (CONS 0 1)))
                         (COND
                          ((AND (NULL (CAR L2)) (NULL (CDR L2)))
                           (PROGN
                            (SETQ NEWOR (LIST (LIST (CONS 1 1))))
                            (SETQ L1 NIL)))
                          (T
                           (SETQ NEWOR
                                   (ADD_TO_INEQ_OR (NCONC (CAR L2) (CDR L2))
                                    NEWOR)))))))
                      (GO WHILELABEL)))))
            (COND
             ((NULL NEWOR)
              (PROGN
               (COND
                (PRINT_
                 (PROGN
                  (TERPRI)
                  (PROGN (PRIN2 "Contradiction: The substitution:") NIL)
                  (EQPRINT (LIST 'EQUAL OLD NEW))
                  (PROGN
                   (PRIN2
                    "transformed all expressions, each given as a list of factors:")
                   NIL)
                  (MATHPRINT
                   (CONS 'LIST
                         (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L1 (CAR L))
                           (COND ((NULL L1) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (L1)
                                               (CONS 'LIST
                                                     (PROG (L2 FORALL-RESULT
                                                            FORALL-ENDPTR)
                                                       (SETQ L2 L1)
                                                       (COND
                                                        ((NULL L2)
                                                         (RETURN NIL)))
                                                       (SETQ FORALL-RESULT
                                                               (SETQ FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              L2)
                                                                           (LIST
                                                                            '*SQ
                                                                            L2
                                                                            T))
                                                                         (CAR
                                                                          L2))
                                                                        NIL)))
                                                      LOOPLABEL
                                                       (SETQ L2 (CDR L2))
                                                       (COND
                                                        ((NULL L2)
                                                         (RETURN
                                                          FORALL-RESULT)))
                                                       (RPLACD FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (L2)
                                                                   (LIST '*SQ
                                                                         L2 T))
                                                                 (CAR L2))
                                                                NIL))
                                                       (SETQ FORALL-ENDPTR
                                                               (CDR
                                                                FORALL-ENDPTR))
                                                       (GO LOOPLABEL))))
                                             (CAR L1))
                                            NIL)))
                          LOOPLABEL
                           (SETQ L1 (CDR L1))
                           (COND ((NULL L1) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (L1)
                                       (CONS 'LIST
                                             (PROG (L2 FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ L2 L1)
                                               (COND ((NULL L2) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (L2)
                                                                   (LIST '*SQ
                                                                         L2 T))
                                                                 (CAR L2))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ L2 (CDR L2))
                                               (COND
                                                ((NULL L2)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (L2)
                                                           (LIST '*SQ L2 T))
                                                         (CAR L2))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL))))
                                     (CAR L1))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                  (PROGN
                   (PRIN2
                    "within one OR-inequality modulo all other equations to zero.")
                   NIL)
                  (TERPRI))))
               (SETQ CONTRADICTION_ T)
               (SETQ L (LIST NIL))))
             ((NULL (CDR NEWOR))
              (COND
               ((NEQ (CAR NEWOR) (LIST (CONS 1 1)))
                (PROGN
                 (COND
                  (PRINT_
                   (PROGN
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "Due to vanishing expressions an OR-inequality is now")
                     NIL)
                    (TERPRI)
                    (PROGN
                     (PRIN2
                      "converted to a normal inequality where each of the")
                     NIL)
                    (TERPRI)
                    (PROGN (PRIN2 "following factors must not vanish: ") NIL)
                    (MATHPRINT
                     (CONS 'LIST
                           (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L2 (CAR NEWOR))
                             (COND ((NULL L2) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L2) (LIST '*SQ L2 T))
                                               (CAR L2))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L2 (CDR L2))
                             (COND ((NULL L2) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (L2) (LIST '*SQ L2 T)) (CAR L2))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                    NIL)))
                 (PROG (L2)
                   (SETQ L2 (CAR NEWOR))
                  LAB
                   (COND ((NULL L2) (RETURN NIL)))
                   ((LAMBDA (L2) (ADDSQINEQ PDES L2 NIL)) (CAR L2))
                   (SETQ L2 (CDR L2))
                   (GO LAB))))
               (T NIL)))
             (T (SETQ INEQ_OR (CONS NEWOR INEQ_OR))))
            (SETQ L (CDR L)))
           (GO WHILELABEL))))))) 
(PUT 'NEWINEQU 'NUMBER-OF-ARGS 1) 
(PUT 'NEWINEQU 'DEFINED-ON-LINE '922) 
(PUT 'NEWINEQU 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'NEWINEQU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEWINEQU (PDES)
    (PROG (EX)
      (CHANGE_PROMPT_TO "")
      (PROGN
       (PRIN2 "Input of a value for the new non-vanishing expression.")
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "You can use names of pds, e.g. 3*e_12 - df(e_13,x) + 8; ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "Terminate the expression with ; or $ : ") NIL)
      (TERPRI)
      (SETQ EX (SIMP (TERMXREAD)))
      (PROG (A)
        (SETQ A PDES)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A)
           (SETQ EX (SUBSQ EX (LIST (CONS A (LIST '*SQ (GET A 'SQVAL) T))))))
         (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (TERPRI)
      (RESTORE_INTERACTIVE_PROMPT)
      (ADDSQINEQ PDES EX T))) 
(PUT 'PREDUCE_LIST 'NUMBER-OF-ARGS 2) 
(PUT 'PREDUCE_LIST 'DEFINED-ON-LINE '942) 
(PUT 'PREDUCE_LIST 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'PREDUCE_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PREDUCE_LIST (L EQNS)
    (PROG (VIOL P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NULL VIOL) L)) (RETURN NIL)))
        (COND
         ((MORE_THAN_X_TERMS (CAR (CAR L)) MAX_TERM_TO_PRED) (SETQ L (CDR L)))
         (T
          (PROGN
           (SETQ P (ERR_CATCH_PREDUCE (LIST '*SQ (CAR L) T) EQNS))
           (COND ((SQZEROP P) (SETQ VIOL (CAR L))) (T (SETQ L (CDR L)))))))
        (GO WHILELABEL))
      (RETURN VIOL))) 
(PUT 'CHECK_INEQ 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_INEQ 'DEFINED-ON-LINE '963) 
(PUT 'CHECK_INEQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'CHECK_INEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_INEQ (ARGLIST)
    (COND
     (VL_
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2 "This module is only applicable to algebraic problems.")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "The global list of independent variables vl_ is not empty")
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2
           "which indicates this is a differential system, so the module")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "is not applied.") NIL)
         (TERPRI)))
       (T NIL)))
     (T
      (PROG (EQNS G H VANISHES)
        (PROG (H)
          (SETQ H (CAR ARGLIST))
         LAB
          (COND ((NULL H) (RETURN NIL)))
          ((LAMBDA (H)
             (COND
              ((LEQ (GET H 'TERMS) MAX_TERM_TO_PRED)
               (SETQ EQNS (CONS (LIST '*SQ (GET H 'SQVAL) T) EQNS)))))
           (CAR H))
          (SETQ H (CDR H))
          (GO LAB))
        (COND ((NULL EQNS) (RETURN NIL)))
        (SETQ EQNS (CONS 'LIST EQNS))
        (AEVAL (LIST 'TORDER (CONS 'LIST FTEM_) 'REVGRADLEX))
        (COND
         ((SETQ H (PREDUCE_LIST INEQ_ EQNS))
          (RETURN
           (PROGN
            (COND
             (PRINT_
              (PROGN
               (PROGN (PRIN2 "The inequality 0 <> ") NIL)
               (EQPRINT (LIST '*SQ H T))
               (PROGN (PRIN2 " is violated.") NIL)
               (TERPRI))))
            (SETQ CONTRADICTION_ T)
            NIL))))
        (SETQ H INEQ_OR)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND H (NULL CONTRADICTION_))) (RETURN NIL)))
          (PROGN
           (SETQ VANISHES T)
           (SETQ G (CAR H))
           (PROG ()
            WHILELABEL
             (COND ((NOT (AND VANISHES G)) (RETURN NIL)))
             (PROGN
              (SETQ VANISHES (PREDUCE_LIST (CAR G) EQNS))
              (SETQ G (CDR G)))
             (GO WHILELABEL))
           (COND
            (VANISHES
             (PROGN
              (SETQ CONTRADICTION_ T)
              (PROGN (PRIN2 "The OR-inequality {") NIL)
              (PROG (G)
                (SETQ G (CAR H))
               LAB
                (COND ((NULL G) (RETURN NIL)))
                ((LAMBDA (G) (EQPRINT G)) (CAR G))
                (SETQ G (CDR G))
                (GO LAB))
              (PROGN
               (PRIN2
                "} is violated as each expression vanishes modulo all equations.")
               NIL)
              (TERPRI)))
            (T (SETQ H (CDR H)))))
          (GO WHILELABEL)))))) 
(PUT 'DELETE_INEQ 'NUMBER-OF-ARGS 1) 
(PUT 'DELETE_INEQ 'DEFINED-ON-LINE '1013) 
(PUT 'DELETE_INEQ 'DEFINED-IN-FILE 'CRACK/CRINEQ.RED) 
(PUT 'DELETE_INEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETE_INEQ (PDES)
    (PROG (N P)
      (PRINT_INEQ (CONS INEQ_ NIL))
      (CHANGE_PROMPT_TO "")
      (TERPRI)
      (PROGN
       (PRIN2 "Select an inequality to be dropped through a number 1 - ")
       (PRIN2 (LENGTH INEQ_))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "based on the order in which they are printed above. ")
       NIL)
      (TERPRI)
      (PROGN (PRIN2 "To return to main menu input 0. ") NIL)
      (SETQ N (TERMREAD))
      (COND ((ZEROP N) (RETURN NIL)))
      (COND
       ((NOT (FIXP N))
        (RETURN
         (PROGN (PROGN (PRIN2 "This was not a number!") NIL) (TERPRI) NIL))))
      (SETQ INEQ_ (DELETE (NTH INEQ_ N) INEQ_))
      (COND
       ((YESP
         "Shall possible substitution be re-determined (not recommended) ? ")
        (PROG (P)
          (SETQ P PDES)
         LAB
          (COND ((NULL P) (RETURN NIL)))
          ((LAMBDA (P)
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
              NIL))
           (CAR P))
          (SETQ P (CDR P))
          (GO LAB))))
      (COND
       (INEQ_OR
        (PROGN
         (PRINT_INEQ (CONS NIL INEQ_OR))
         (TERPRI)
         (PROGN
          (PRIN2 "Select an OR-inequality to be dropped through a number 1 - ")
          (PRIN2 (LENGTH INEQ_OR))
          NIL)
         (TERPRI)
         (PROGN
          (PRIN2 "based on the order in which they are printed above. ")
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "Input a 0 if no inequality shall be deleted: ") NIL)
         (SETQ N (TERMREAD))
         (COND ((ZEROP N) (RETURN NIL)))
         (COND
          ((NOT (FIXP N))
           (RETURN
            (PROGN
             (PROGN (PRIN2 "This was not a number!") NIL)
             (TERPRI)
             NIL))))
         (SETQ INEQ_OR (DELETE (NTH INEQ_OR N) INEQ_OR))
         NIL)))
      (RESTORE_INTERACTIVE_PROMPT))) 
(ENDMODULE) 