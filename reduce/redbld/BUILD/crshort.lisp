(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SHORTENING)) 
(PUT 'ALG_LENGTH_REDUCTION 'NUMBER-OF-ARGS 1) 
(PUT 'ALG_LENGTH_REDUCTION 'DEFINED-ON-LINE '34) 
(PUT 'ALG_LENGTH_REDUCTION 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'ALG_LENGTH_REDUCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG_LENGTH_REDUCTION (ARGLIST)
    (PROG (PDES L L1 P CPU GC)
      (SETQ CPU (TIME))
      (SETQ GC (GCTIME))
      (SETQ PDES (CAR ARGLIST))
      (COND (EXPERT_MODE (PROGN (SETQ L (SELECTPDES PDES 2)) (SETQ P NIL)))
            (T
             (PROGN
              (SETQ L PDES)
              (SETQ P (CADDDR ARGLIST))
              (COND ((EQ L P) (SETQ P NIL)))
              NIL)))
      (SETQ *RATIONAL_BAK (CONS *RATIONAL *RATIONAL_BAK))
      (COND (*RATIONAL (AEVAL (OFF (LIST 'RATIONAL)))))
      (COND
       (STRUC_EQN
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (COND ((IS_ALGEBRAIC (CAR L)) (SETQ L1 (CONS (CAR L) L1))))
            (SETQ L (CDR L)))
           (GO WHILELABEL))
         (SETQ L (REVERSE L1)))))
      (COND
       ((AND L (CDR L) (SETQ L1 (ERR_CATCH_SHORT PDES L P)))
        (PROGN
         (PROG (A)
           (SETQ A (CDR L1))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (SETQ PDES (DROP_PDE A PDES NIL))) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (PROG (A)
           (SETQ A (CAR L1))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (COND (A (SETQ PDES (EQINSERT A PDES))))) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (PROG (A)
           (SETQ A (CAR L1))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (COND (A (DEC_FCT_CHECK A PDES)))) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ L NIL)
         (PROG (A)
           (SETQ A (CAR L1))
          LAB
           (COND ((NULL A) (RETURN NIL)))
           ((LAMBDA (A) (COND (A (SETQ L (CONS A L))))) (CAR A))
           (SETQ A (CDR A))
           (GO LAB))
         (SETQ L (LIST PDES (CADR ARGLIST) L))))
       (T (SETQ L NIL)))
      (COND
       ((AND PRINT_ *TIME)
        (PROGN
         (PROGN
          (PRIN2 " time : ")
          (PRIN2 (DIFFERENCE (TIME) CPU))
          (PRIN2 " ms    GC time : ")
          (PRIN2 (DIFFERENCE (GCTIME) GC))
          (PRIN2 " ms.      ")
          NIL))))
      (COND
       ((NEQ *RATIONAL (CAR *RATIONAL_BAK))
        (COND (*RATIONAL (AEVAL (OFF (LIST 'RATIONAL))))
              (T (AEVAL (ON (LIST 'RATIONAL)))))))
      (SETQ *RATIONAL_BAK (CDR *RATIONAL_BAK))
      (RETURN L))) 
(PUT 'ERR_CATCH_SHORT 'NUMBER-OF-ARGS 3) 
(PUT 'ERR_CATCH_SHORT 'DEFINED-ON-LINE '85) 
(PUT 'ERR_CATCH_SHORT 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'ERR_CATCH_SHORT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ERR_CATCH_SHORT (ALLPDES PDES P2)
    (PROG (G H BAK KERNLIST*BAK KORD*BAK MI NEWP P1 BAKUP_BAK S)
      (COND
       (T
        (PROGN
         (SETQ BAK MAX_GC_COUNTER)
         (SETQ MAX_GC_COUNTER (PLUS MY_GC_COUNTER MAX_GC_SHORT))
         (SETQ BAKUP_BAK BACKUP_)
         (SETQ BACKUP_ 'MAX_GC_SHORT)
         (SETQ KERNLIST*BAK KERNLIST*)
         (SETQ KORD*BAK KORD*)
         ((LAMBDA (*PROTFG)
            (SETQ H
                    (ERRORSET (LIST 'SHORTEN_PDES (MKQUOTE PDES) (MKQUOTE P2))
                              NIL NIL)))
          T)
         (SETQ KORD* KORD*BAK)
         (SETQ KERNLIST* KERNLIST*BAK)
         (SETQ ERFG* NIL)
         (SETQ BACKUP_ BAKUP_BAK)
         (SETQ MAX_GC_COUNTER BAK)
         (COND ((OR (ERRORP H) (EQUAL (CAR H) NIL)) (RETURN NIL)))
         (SETQ H (CAR H))))
       (T
        (PROGN
         (SETQ H (SHORTEN_PDES PDES P2))
         (COND ((NULL H) (RETURN NIL))))))
      (SETQ MI (CAR H))
      (SETQ NEWP (CDR H))
      (SETQ P1 0)
      (PROG (PC)
        (SETQ PC (CDR NEWP))
       LAB
        (COND ((NULL PC) (RETURN NIL)))
        ((LAMBDA (PC) (SETQ P1 (PLUS P1 (GET PC 'TERMS)))) (CAR PC))
        (SETQ PC (CDR PC))
        (GO LAB))
      (SETQ H
              (PROG (PC FORALL-RESULT FORALL-ENDPTR)
                (SETQ PC (CAR NEWP))
                (COND ((NULL PC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PC)
                                    (COND
                                     ((SQZEROP PC)
                                      (PROGN (SETQ NEQU_ (ADD1 NEQU_)) NIL))
                                     (T
                                      (MKEQSQ PC NIL NIL
                                       (FCTSORT
                                        (UNION (GET (CAR MI) 'FCTS)
                                               (GET (CADR MI) 'FCTS)))
                                       (UNION (GET (CAR MI) 'VARS)
                                              (GET (CADR MI) 'VARS))
                                       ALLFLAGS_ T (LIST 0) NIL ALLPDES))))
                                  (CAR PC))
                                 NIL)))
               LOOPLABEL
                (SETQ PC (CDR PC))
                (COND ((NULL PC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PC)
                            (COND
                             ((SQZEROP PC)
                              (PROGN (SETQ NEQU_ (ADD1 NEQU_)) NIL))
                             (T
                              (MKEQSQ PC NIL NIL
                               (FCTSORT
                                (UNION (GET (CAR MI) 'FCTS)
                                       (GET (CADR MI) 'FCTS)))
                               (UNION (GET (CAR MI) 'VARS)
                                      (GET (CADR MI) 'VARS))
                               ALLFLAGS_ T (LIST 0) NIL ALLPDES))))
                          (CAR PC))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (PC)
        (SETQ PC H)
       LAB
        (COND ((NULL PC) (RETURN NIL)))
        ((LAMBDA (PC) (COND (PC (SETQ P1 (DIFFERENCE P1 (GET PC 'TERMS))))))
         (CAR PC))
        (SETQ PC (CDR PC))
        (GO LAB))
      (SETQ S (CONS H (CDR NEWP)))
      (COND
       (NIL
        (COND
         (PRINT_
          (PROGN
           (COND
            (TR_SHORT
             (PROGN
              (PROG (G)
                (SETQ G (CDR NEWP))
               LAB
                (COND ((NULL G) (RETURN NIL)))
                ((LAMBDA (G)
                   (PROGN (PROGN (PRIN2 G) (PRIN2 ": ") NIL) (TYPEEQ G)))
                 (CAR G))
                (SETQ G (CDR G))
                (GO LAB))
              (PROG (G)
                (SETQ G H)
               LAB
                (COND ((NULL G) (RETURN NIL)))
                ((LAMBDA (G)
                   (COND
                    ((NULL G)
                     (PROGN
                      (PROGN (PRIN2 "This gives identity 0=0.") NIL)
                      (TERPRI)))
                    (T (PROGN (PROGN (PRIN2 G) (PRIN2 ": ") NIL) (TYPEEQ G)))))
                 (CAR G))
                (SETQ G (CDR G))
                (GO LAB))
              NIL)))
           (COND
            ((GEQ P1 0)
             (PROGN (PRIN2 "shortening by ") (PRIN2 P1) (PRIN2 " term") NIL))
            ((LESSP P1 0)
             (PROGN
              (PRIN2 "increasing by ")
              (PRIN2 (MINUS P1))
              (PRIN2 " term")
              NIL)))
           (COND ((AND (NEQ P1 1) (NEQ P1 (MINUS 1))) (PROGN (PRIN2 "s") NIL)))
           (COND
            ((AND H (CAR H) (NULL (CDR H)))
             (PROGN
              (PROGN
               (PRIN2 " to now ")
               (PRIN2 (SETQ G (GET (CAR H) 'TERMS)))
               (PRIN2 " term")
               NIL)
              (COND ((NEQ G 1) (PROGN (PRIN2 "s") NIL)))
              (PROGN (PRIN2 ".") NIL)
              (TERPRI)
              NIL))
            ((NULL H) (PROGN (PROGN (PRIN2 " to 0=0 .") NIL) (TERPRI)))))))))
      (COND
       ((GREATERP P1 0)
        (PROG (PC)
          (SETQ PC (CDR NEWP))
         LAB
          (COND ((NULL PC) (RETURN NIL)))
          ((LAMBDA (PC) (DROP_PDE PC NIL NIL)) (CAR PC))
          (SETQ PC (CDR PC))
          (GO LAB)))
       (T
        (PROG (PC)
          (SETQ PC H)
         LAB
          (COND ((NULL PC) (RETURN NIL)))
          ((LAMBDA (PC)
             (COND
              (PC
               (PROGN (ADD_RL_WITH (CAR MI) PC) (ADD_RL_WITH (CADR MI) PC)))))
           (CAR PC))
          (SETQ PC (CDR PC))
          (GO LAB))))
      (RETURN S))) 
(PUT 'ALG_LENGTH_REDUCE_1 'NUMBER-OF-ARGS 1) 
(PUT 'ALG_LENGTH_REDUCE_1 'DEFINED-ON-LINE '161) 
(PUT 'ALG_LENGTH_REDUCE_1 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'ALG_LENGTH_REDUCE_1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG_LENGTH_REDUCE_1 (ARGLIST)
    (PROG (P Q H P_TO_EVAL HCP)
      (COND (*BATCH_MODE (RETURN NIL)))
      (COND
       ((OR PRINT_ (NULL OLD_HISTORY))
        (PROGN
         (TERPRI)
         (CHANGE_PROMPT_TO "")
         (PROGN (PRIN2 "All equations:") NIL)
         (TERPRI)
         (LISTPRINT (CAR ARGLIST))
         (TERPRI)
         (TERPRI)
         (PROGN (PRIN2 "Which equation should be length reduced: ") NIL)
         NIL)))
      (SETQ P (TERMREAD))
      (SETQ P_TO_EVAL (FLAGP P 'TO_EVAL))
      (FLAG (LIST P) 'TO_EVAL)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ H
                 (ALG_LENGTH_REDUCTION
                  (LIST (CAR ARGLIST) (CADR ARGLIST) (CADDR ARGLIST) P)))
         (COND
          (H
           (PROGN
            (SETQ Q (SETDIFF (CAR H) (CONS P (CAR ARGLIST))))
            (COND (Q (PROGN (SETQ ARGLIST H) (SETQ P (CAR Q)))))
            (SETQ HCP H)
            NIL))))
        (COND ((NOT (OR (NULL H) (FREEOF (CAR H) P))) (GO REPEATLABEL))))
      (COND
       ((AND (NOT P_TO_EVAL) H (NOT (FREEOF (CAR H) P)))
        (REMFLAG (LIST P) 'TO_EVAL)))
      (RESTORE_INTERACTIVE_PROMPT)
      (RETURN HCP))) 
(PUT 'IS_ALGEBRAIC 'NUMBER-OF-ARGS 1) 
(PUT 'IS_ALGEBRAIC 'DEFINED-ON-LINE '208) 
(PUT 'IS_ALGEBRAIC 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'IS_ALGEBRAIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS_ALGEBRAIC (P) (COND ((GET P 'NO_DERIVS) T) (T NIL))) 
(PUT 'SHORTEN_PDES 'NUMBER-OF-ARGS 2) 
(PUT 'SHORTEN_PDES 'DEFINED-ON-LINE '213) 
(PUT 'SHORTEN_PDES 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'SHORTEN_PDES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SHORTEN_PDES (DES P2)
    (COND ((OR (NOT (PAIRP DES)) (NOT (PAIRP (CDR DES)))) (CONS NIL NIL))
          (T
           (PROG (P2RL P PC PCC NEWP INEQ_PRE)
             (PROG (P1)
               (SETQ P1 INEQ_)
              LAB
               (COND ((NULL P1) (RETURN NIL)))
               ((LAMBDA (P1)
                  (COND
                   ((ONE_TERMPSF (CAR P1))
                    (SETQ INEQ_PRE (CONS (PREPSQ P1) INEQ_PRE)))))
                (CAR P1))
               (SETQ P1 (CDR P1))
               (GO LAB))
             (COND
              (ALG_POLY (SETQ INEQ_PRE (SORT_ACCORDING_TO INEQ_PRE FTEM_))))
             (COND
              (P2
               (PROGN
                (COND
                 ((AND PRINT_ TR_SHORT)
                  (PROGN
                   (PROGN
                    (PRIN2 "Trying to shorten the following equation: ")
                    NIL)
                   NIL)))
                (SETQ P2RL (TIMES 2 (GET P2 'TERMS)))
                (PROG (P)
                  (SETQ P DES)
                 LAB
                  (COND ((NULL P) (RETURN NIL)))
                  ((LAMBDA (P)
                     (COND
                      ((AND (NEQ P P2) (LEQ (GET P 'TERMS) P2RL))
                       (SETQ NEWP (CONS P NEWP)))))
                   (CAR P))
                  (SETQ P (CDR P))
                  (GO LAB))
                (COND ((NULL NEWP) (RETURN NIL)))
                (SETQ DES (REVERSIP NEWP))
                (SETQ PCC (LIST P2))
                (SETQ DES (NCONC DES PCC))
                (SETQ PCC (CONS 1 PCC))
                (SETQ NEWP NIL)
                (SETQ P2RL NIL)
                NIL))
              (T
               (PROGN
                (COND
                 ((AND PRINT_ TR_SHORT)
                  (PROGN
                   (PROGN
                    (PRIN2 "Trying to shorten the following equations: ")
                    NIL)
                   NIL)))
                (SETQ PCC DES)
                (COND
                 (LARGEST_FULLY_SHORTENED
                  (PROGN
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT (AND PCC (NEQ LARGEST_FULLY_SHORTENED (CAR PCC))))
                       (RETURN NIL)))
                     (SETQ PCC (CDR PCC))
                     (GO WHILELABEL))
                   (COND ((OR (NULL PCC) (NULL (CDR PCC))) (RETURN NIL)))))))))
             (COND ((NULL (CDR PCC)) (RETURN NIL)))
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ PCC (CDR PCC))
                (SETQ P2 (CAR PCC))
                (COND
                 ((AND PRINT_ TR_SHORT)
                  (PROGN
                   (PRIN2 P2)
                   (PRIN2 "(")
                   (PRIN2 (GET P2 'TERMS))
                   (PRIN2 ") ")
                   NIL)))
                (SETQ P2RL (GET P2 'RL_WITH))
                (SETQ PC DES)
                (PROG ()
                 WHILELABEL
                  (COND
                   ((NOT (AND (NULL NEWP) (NOT (EQ PC PCC)))) (RETURN NIL)))
                  (PROGN
                   (COND
                    ((NOT (MEMBER (CAR PC) P2RL))
                     (PROGN
                      (COND
                       ((AND (EQUAL BACKUP_ 'MAX_GC_SHORT)
                             (GREATERP MY_GC_COUNTER MAX_GC_COUNTER))
                        (REDERR "Stop in shorten_pdes().")))
                      (SETQ NEWP (SHORTEN (CAR PC) P2 INEQ_PRE))
                      (COND
                       ((NULL NEWP)
                        (PROGN
                         (ADD_RL_WITH (CAR PC) P2)
                         (SETQ PC (CDR PC)))))))
                    (T (SETQ PC (CDR PC)))))
                  (GO WHILELABEL))
                (COND ((NULL NEWP) (SETQ LARGEST_FULLY_SHORTENED (CAR PCC)))))
               (COND ((NOT (OR NEWP (NULL (CDR PCC)))) (GO REPEATLABEL))))
             (RETURN (COND (NEWP (CONS (LIST (CAR PC) P2) NEWP)) (T NIL))))))) 
(PUT 'PARTITION_3 'NUMBER-OF-ARGS 3) 
(PUT 'PARTITION_3 'DEFINED-ON-LINE '284) 
(PUT 'PARTITION_3 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'PARTITION_3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARTITION_3 (DE ANB A)
    (PROG (L L1 MV)
      (SETQ L (REORDER (CAR (GET DE 'SQVAL))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND L (NOT (OR (ATOM L) (ATOM (CAR L))))
                (MEMBER (SETQ MV (CAAAR L)) ANB)))
          (RETURN NIL)))
        (PROGN
         (SETQ L1 (CONS (CONS (NO_OF_TM_SF (CDAR L)) (CONS MV (CDAR L))) L1))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND L (NOT (OR (ATOM L) (ATOM (CAR L)))) (MEMBER (CAAAR L) A)))
          (RETURN NIL)))
        (SETQ L (CDR L))
        (GO WHILELABEL))
      (RETURN (COND (L (CONS (CONS (NO_OF_TM_SF L) (CONS 1 L)) L1)) (T L1))))) 
(PUT 'SETDIFF_ACCORDING_TO 'NUMBER-OF-ARGS 3) 
(PUT 'SETDIFF_ACCORDING_TO 'DEFINED-ON-LINE '305) 
(PUT 'SETDIFF_ACCORDING_TO 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'SETDIFF_ACCORDING_TO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETDIFF_ACCORDING_TO (A B FT)
    (PROG (AMB NOFT BCP)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A B)) (RETURN NIL)))
        (COND
         ((EQ (CAR A) (CAR B))
          (PROGN (SETQ A (CDR A)) (SETQ B (CDR B)) (SETQ FT (CDR FT))))
         (T
          (PROGN
           (COND
            ((AND (NULL NOFT) (OR (PAIRP (CAR A)) (PAIRP (CAR B))))
             (SETQ NOFT T)))
           (COND
            (NOFT
             (PROGN
              (SETQ BCP B)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND BCP (NEQ (CAR A) (CAR BCP)))) (RETURN NIL)))
                (SETQ BCP (CDR BCP))
                (GO WHILELABEL))
              (COND ((NULL BCP) (SETQ AMB (CONS (CAR A) AMB))))
              (SETQ A (CDR A))))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT (AND (NEQ (CAR FT) (CAR A)) (NEQ (CAR FT) (CAR B))))
                  (RETURN NIL)))
                (SETQ FT (CDR FT))
                (GO WHILELABEL))
              (COND
               ((EQ (CAR FT) (CAR A))
                (PROGN (SETQ AMB (CONS (CAR A) AMB)) (SETQ A (CDR A))))
               (T (SETQ B (CDR B))))
              (SETQ FT (CDR FT))))))))
        (GO WHILELABEL))
      (RETURN (COND (A (NCONC (REVERSIP AMB) A)) (T (REVERSIP AMB)))))) 
(PUT 'ADD_FCT_KERN 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_FCT_KERN 'DEFINED-ON-LINE '339) 
(PUT 'ADD_FCT_KERN 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'ADD_FCT_KERN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_FCT_KERN (DE INEQ_PRE)
    (COND
     (ALG_POLY
      (COND (LIN_PROBLEM (CONS (GET DE 'FCTS) NIL))
            (FLIN_
             (PROG (L N)
               (SETQ L (GET DE 'FCT_KERN_LIN))
               (SETQ N (GET DE 'FCT_KERN_NLI))
               (COND
                ((AND (NULL L) (NULL N))
                 (PROGN
                  (SETQ N
                          (SETDIFF_ACCORDING_TO (SETQ L (GET DE 'FCTS)) FLIN_
                           FTEM_))
                  (SETQ L (SETDIFF_ACCORDING_TO L N FTEM_))
                  (SETQ N (SETDIFF_ACCORDING_TO N INEQ_PRE FTEM_))
                  (PUT DE 'FCT_KERN_LIN L)
                  (PUT DE 'FCT_KERN_NLI N))))
               (RETURN (CONS L N))))
            (T
             (PROG (N)
               (SETQ N (GET DE 'FCT_KERN_NLI))
               (COND
                ((NULL N)
                 (PROGN
                  (SETQ N (SETDIFF_ACCORDING_TO (GET DE 'FCTS) INEQ_PRE FTEM_))
                  (PUT DE 'FCT_KERN_NLI N)
                  NIL)))
               (RETURN (CONS NIL N))))))
     (LIN_PROBLEM
      (PROG (L R S)
        (SETQ L (GET DE 'FCT_KERN_LIN))
        (COND
         ((NULL L)
          (PROGN
           (SETQ R (GET DE 'FCTS))
           (PROG (S)
             (SETQ S (GET DE 'KERN))
            LAB
             (COND ((NULL S) (RETURN NIL)))
             ((LAMBDA (S) (COND ((NOT (FREEOFLIST S R)) (SETQ L (CONS S L)))))
              (CAR S))
             (SETQ S (CDR S))
             (GO LAB))
           (PUT DE 'FCT_KERN_LIN L)
           NIL)))
        (RETURN (CONS L NIL))))
     (FLIN_
      (PROG (L N R S)
        (SETQ L (GET DE 'FCT_KERN_LIN))
        (SETQ N (GET DE 'FCT_KERN_NLI))
        (COND
         ((AND (NULL L) (NULL N))
          (PROGN
           (SETQ R (GET DE 'FCTS))
           (PROG (S)
             (SETQ S (GET DE 'KERN))
            LAB
             (COND ((NULL S) (RETURN NIL)))
             ((LAMBDA (S)
                (COND
                 ((NOT (FREEOFLIST S R))
                  (COND ((NOT (FREEOFLIST S FLIN_)) (SETQ L (CONS S L)))
                        ((NOT (MEMBER S INEQ_PRE)) (SETQ N (CONS S N)))))))
              (CAR S))
             (SETQ S (CDR S))
             (GO LAB))
           (PUT DE 'FCT_KERN_LIN L)
           (PUT DE 'FCT_KERN_NLI N))))
        (RETURN (CONS L N))))
     (T
      (PROG (N R S)
        (SETQ N (GET DE 'FCT_KERN_NLI))
        (COND
         ((NULL N)
          (PROGN
           (SETQ R (GET DE 'FCTS))
           (PROG (S)
             (SETQ S (GET DE 'KERN))
            LAB
             (COND ((NULL S) (RETURN NIL)))
             ((LAMBDA (S)
                (COND
                 ((AND (NOT (FREEOFLIST S R)) (NOT (MEMBER S INEQ_PRE)))
                  (SETQ N (CONS S N)))))
              (CAR S))
             (SETQ S (CDR S))
             (GO LAB))
           (PUT DE 'FCT_KERN_NLI N))))
        (RETURN (CONS NIL N)))))) 
(PUT 'SHORTEN 'NUMBER-OF-ARGS 3) 
(PUT 'SHORTEN 'DEFINED-ON-LINE '399) 
(PUT 'SHORTEN 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'SHORTEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SHORTEN (DE1 DE2 INEQ_PRE)
    (PROG (A B R L1 L2 NL1 NL2 L1UL2 L1ML2 L2ML1 L1IL2 OLDORDER DE1P DE2P
           TERMSOF1 TERMSOF2 FLIP M1 M2 N1 N2 QL QL1 QL2 MAXCANCEL TAKE_FIRST
           TR_SHORT_LOCAL NO_FACTORS_X_1 NO_FACTORS_X_2 HOMO CHANGED_KORDER
           DONOTREPLACE1 DONOTREPLACE2 TRYBOTH REPLACE1 BESTQ)
      (COND
       (TR_SHORT_LOCAL
        (DEPRINT
         (LIST (LIST '*SQ (GET DE1 'SQVAL) T)
               (LIST '*SQ (GET DE2 'SQVAL) T)))))
      (COND
       ((AND FHOM_ (EQUAL 1 (CAR (GET DE1 'HOM_DEG)))
             (EQUAL 1 (CAR (GET DE2 'HOM_DEG))))
        (SETQ HOMO T)))
      (SETQ TERMSOF1 (GET DE1 'TERMS))
      (SETQ TERMSOF2 (GET DE2 'TERMS))
      (COND
       ((OR (NULL (FLAGP DE1 'TO_EVAL))
            (AND HOMO
                 (LESSP (CADR (GET DE1 'HOM_DEG)) (CADR (GET DE2 'HOM_DEG))))
            (LESSP (TIMES 2 TERMSOF1) TERMSOF2))
        (SETQ DONOTREPLACE1 T)))
      (COND
       ((OR (NULL (FLAGP DE2 'TO_EVAL))
            (AND HOMO
                 (LESSP (CADR (GET DE2 'HOM_DEG)) (CADR (GET DE1 'HOM_DEG))))
            (LESSP (TIMES 2 TERMSOF2) TERMSOF1))
        (SETQ DONOTREPLACE2 T)))
      (COND ((AND DONOTREPLACE1 DONOTREPLACE2) (RETURN NIL)))
      (SETQ NL1 (ADD_FCT_KERN DE1 INEQ_PRE))
      (SETQ NL2 (ADD_FCT_KERN DE2 INEQ_PRE))
      (SETQ L1 (SORT_ACCORDING_TO (CAR NL1) FTEM_))
      (SETQ NL1 (CDR NL1))
      (SETQ L2 (SORT_ACCORDING_TO (CAR NL2) FTEM_))
      (SETQ NL2 (CDR NL2))
      (COND
       ((AND L1 L2)
        (PROGN
         (SETQ L1ML2 (SETDIFF_ACCORDING_TO L1 L2 FTEM_))
         (SETQ L1IL2 (SETDIFF_ACCORDING_TO L1 L1ML2 FTEM_))
         (COND ((NULL L1IL2) (RETURN NIL)))
         (SETQ L2ML1 (SETDIFF_ACCORDING_TO L2 L1 FTEM_))
         (SETQ L1UL2 (UNION L1 L2))
         (SETQ OLDORDER (SETKORDER (APPEND L1IL2 (APPEND L1ML2 L2ML1))))
         (SETQ CHANGED_KORDER T)
         (SETQ DE1P (PARTITION_3 DE1 L1IL2 L1ML2))
         (SETQ DE2P (PARTITION_3 DE2 L1IL2 L2ML1))
         (SETKORDER OLDORDER)
         (COND ((OR (NULL DE1P) (NULL DE2P)) (RETURN NIL)))
         (COND
          ((AND (EQUAL (CADAR DE1P) 1) (NEQ (CADAR DE2P) 1))
           (SETQ DE1P (CDR DE1P))))
         (COND
          ((AND (EQUAL (CADAR DE2P) 1) (NEQ (CADAR DE1P) 1))
           (SETQ DE2P (CDR DE2P))))
         (SETQ A DE1P)
         (SETQ B DE2P)
         (SETQ N2 NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND A B)) (RETURN NIL)))
           (COND
            ((NEQ (CADAR A) (CADAR B))
             (PROGN
              (SETQ R B)
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND R (NEQ (CADAR R) (CADAR A)))) (RETURN NIL)))
                (SETQ R (CDR R))
                (GO WHILELABEL))
              (COND (R (SETQ B R))
                    (T
                     (PROGN
                      (SETQ R A)
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (AND R (NEQ (CADAR R) (CADAR B))))
                          (RETURN NIL)))
                        (SETQ R (CDR R))
                        (GO WHILELABEL))
                      (SETQ A R))))))
            (T
             (PROGN
              (SETQ N1
                      (COND ((LESSP (CAAR A) (CAAR B)) (CAAR A)) (T (CAAR B))))
              (SETQ N2 (CONS (TIMES 2 N1) N2))
              (SETQ A (CDR A))
              (SETQ B (CDR B))
              NIL)))
           (GO WHILELABEL))
         (SETQ MAXCANCEL (LIST 0))
         (SETQ N1 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT N2) (RETURN NIL)))
           (PROGN
            (SETQ N1 (PLUS N1 (CAR N2)))
            (SETQ N2 (CDR N2))
            (SETQ MAXCANCEL (CONS N1 MAXCANCEL))
            NIL)
           (GO WHILELABEL))
         (COND
          (TR_SHORT_LOCAL
           (PROGN
            (PROGN (PRIN2 "-----") NIL)
            (TERPRI)
            (PROGN (PRIN2 "flin_=") (PRIN2 FLIN_) NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "flin_\\ftem_=")
             (PRIN2 (SETDIFF_ACCORDING_TO FLIN_ FTEM_ FTEM_))
             NIL)
            (PROGN
             (PRIN2 "ftem_\\flin_=")
             (PRIN2 (SETDIFF_ACCORDING_TO FTEM_ FLIN_ FTEM_))
             NIL)
            (PROGN (PRIN2 "get(de1,'fcts)=") (PRIN2 (GET DE1 'FCTS)) NIL)
            (TERPRI)
            (PROGN (PRIN2 "get(de2,'fcts)=") (PRIN2 (GET DE2 'FCTS)) NIL)
            (TERPRI)
            (PROGN (PRIN2 "l1=") (PRIN2 L1) NIL)
            (TERPRI)
            (PROGN (PRIN2 "nl1=") (PRIN2 NL1) NIL)
            (TERPRI)
            (PROGN (PRIN2 "l2=") (PRIN2 L2) NIL)
            (TERPRI)
            (PROGN (PRIN2 "nl2=") (PRIN2 NL2) NIL)
            (TERPRI)
            (PROGN (PRIN2 "l1ml2=") (PRIN2 L1ML2) NIL)
            (TERPRI)
            (PROGN (PRIN2 "l2ml1=") (PRIN2 L2ML1) NIL)
            (TERPRI)
            (PROGN (PRIN2 "l1il2=") (PRIN2 L1IL2) NIL)
            (TERPRI)
            (PROGN (PRIN2 "length de1p=") (PRIN2 (LENGTH DE1P)) NIL)
            (TERPRI)
            (PROGN (PRIN2 "length de2p=") (PRIN2 (LENGTH DE2P)) NIL)
            (TERPRI)
            (PROGN (PRIN2 "de1p=") (PRIN2 DE1P) NIL)
            (TERPRI)
            (PROGN (PRIN2 "de2p=") (PRIN2 DE2P) NIL)
            (TERPRI)
            (PROGN (PRIN2 "---------") NIL)
            (TERPRI)
            (PROGN
             (PRIN2 "de1:")
             (PRIN2 DE1)
             (PRIN2 " with ")
             (PRIN2 TERMSOF1)
             (PRIN2 " terms")
             NIL)
            (TERPRI)
            (SETQ A DE1P)
            (PROG ()
             WHILELABEL
              (COND ((NOT A) (RETURN NIL)))
              (PROGN
               (PROGN (PRIN2 "caar  a=") (PRIN2 (CAAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cadar a=") (PRIN2 (CADAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cddar a=") (PRIN2 (CDDAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cddar a=") NIL)
               (ASSGNPRI (AEVAL* (LIST '*SQ (CONS (CDDAR A) 1) T)) NIL 'ONLY)
               (SETQ A (CDR A))
               NIL)
              (GO WHILELABEL))
            (TERPRI)
            (PROGN
             (PRIN2 "de2:")
             (PRIN2 DE2)
             (PRIN2 " with ")
             (PRIN2 TERMSOF2)
             (PRIN2 " terms")
             NIL)
            (TERPRI)
            (SETQ A DE2P)
            (PROG ()
             WHILELABEL
              (COND ((NOT A) (RETURN NIL)))
              (PROGN
               (PROGN (PRIN2 "caar  a=") (PRIN2 (CAAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cadar a=") (PRIN2 (CADAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cddar a=") (PRIN2 (CDDAR A)) NIL)
               (TERPRI)
               (PROGN (PRIN2 "cddar a=") NIL)
               (ASSGNPRI (AEVAL* (LIST '*SQ (CONS (CDDAR A) 1) T)) NIL 'ONLY)
               (SETQ A (CDR A))
               NIL)
              (GO WHILELABEL))
            (TERPRI)
            NIL)))))
       (T
        (PROGN
         (SETQ DE1P (LIST (CONS TERMSOF1 (CONS 1 (CAR (GET DE1 'SQVAL))))))
         (SETQ DE2P (LIST (CONS TERMSOF2 (CONS 1 (CAR (GET DE2 'SQVAL))))))
         (SETQ MAXCANCEL
                 (COND ((LESSP TERMSOF1 TERMSOF2) (LIST (TIMES 2 TERMSOF1) 0))
                       (T (LIST (TIMES 2 TERMSOF2) 0)))))))
      (COND
       ((LESSP (CAR MAXCANCEL) TERMSOF1)
        (COND (DONOTREPLACE1 (RETURN NIL)) (T (SETQ DONOTREPLACE2 T)))))
      (COND
       ((LESSP (CAR MAXCANCEL) TERMSOF2)
        (COND (DONOTREPLACE2 (RETURN NIL)) (T (SETQ DONOTREPLACE1 T)))))
      (COND
       ((OR DONOTREPLACE2 (LESSP TERMSOF2 TERMSOF1))
        (PROGN
         (SETQ FLIP T)
         (SETQ A DE1P)
         (SETQ DE1P DE2P)
         (SETQ DE2P A)
         (SETQ NO_FACTORS_X_2
                 (COND ((OR L1 (NULL L2)) NL2)
                       (T
                        (APPEND NL2
                                (SETDIFF_ACCORDING_TO L2 INEQ_PRE FTEM_)))))
         (SETQ NO_FACTORS_X_1
                 (COND ((OR L2 (NULL L1)) NL1)
                       (T
                        (APPEND NL1
                                (SETDIFF_ACCORDING_TO L1 INEQ_PRE FTEM_)))))
         (SETQ N1 TERMSOF2)
         (SETQ N2 TERMSOF1)))
       (T
        (PROGN
         (SETQ NO_FACTORS_X_1
                 (COND ((OR L1 (NULL L2)) NL2)
                       (T
                        (APPEND NL2
                                (SETDIFF_ACCORDING_TO L2 INEQ_PRE FTEM_)))))
         (SETQ NO_FACTORS_X_2
                 (COND ((OR L2 (NULL L1)) NL1)
                       (T
                        (APPEND NL1
                                (SETDIFF_ACCORDING_TO L1 INEQ_PRE FTEM_)))))
         (SETQ N1 TERMSOF1)
         (SETQ N2 TERMSOF2))))
      (COND ((AND (NULL DONOTREPLACE1) (NULL DONOTREPLACE2)) (SETQ TRYBOTH T)))
      (COND
       ((NEQ LENGTH_INC_ALG 1.0)
        (PROGN
         (SETQ M1
                 (REVAL1
                  (AEVAL (LIST 'CEILING (LIST 'QUOTIENT N1 LENGTH_INC_ALG)))
                  T))
         (SETQ M2
                 (REVAL1
                  (AEVAL (LIST 'CEILING (LIST 'QUOTIENT N2 LENGTH_INC_ALG)))
                  T))))
       (T (PROGN (SETQ M1 N1) (SETQ M2 N2))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ B
                 (SHORT QL1 QL2 (CDDAR DE1P) (CDDAR DE2P) M1 M2
                  (TIMES 2 (CAAR DE1P))
                  (DIFFERENCE (CAR MAXCANCEL) (CADR MAXCANCEL))
                  (CADR MAXCANCEL) TAKE_FIRST NO_FACTORS_X_1 NO_FACTORS_X_2
                  TRYBOTH))
         (COND
          (B
           (PROGN
            (SETQ QL1 (CAR B))
            (SETQ QL2 (CADR B))
            (SETQ A (CDDR B))
            (COND
             ((AND A TAKE_FIRST)
              (PROGN (SETQ DE1P (CAR A)) (SETQ DE2P (CDR A)) NIL))
             (T (PROGN (SETQ DE1P (CDR DE1P)) (SETQ DE2P (CDR DE2P)) NIL)))
            (SETQ MAXCANCEL (CDR MAXCANCEL))
            NIL))
          (T (SETQ A NIL)))
         NIL)
        (COND
         ((NOT (OR (NULL B) (AND A TAKE_FIRST) (NULL DE1P)))
          (GO REPEATLABEL))))
      (COND
       ((AND B (NULL TAKE_FIRST))
        (PROGN
         (COND
          ((AND (NULL QL2) (OR (NULL TRYBOTH) (NULL QL1)))
           (PROGN
            (PROGN (PRIN2 "##### SOMETHING IS WRONG ###") NIL)
            (TERPRI)
            (PROGN
             (PRIN2
              " short() should have recognized that there is no successful quotient.")
             NIL)
            (TERPRI))))
         (SETQ QL2 (FIND_BEST_QUOTIENT QL2))
         (SETQ QL1 (FIND_BEST_QUOTIENT QL1))
         (COND
          ((GREATERP (DIFFERENCE (CAAR QL1) N2) (DIFFERENCE (CAAR QL2) N1))
           (PROGN (SETQ REPLACE1 T) (SETQ QL QL1)))
          (T (SETQ QL QL2)))
         (SETQ BESTQ (CAR QL))
         (SETQ R (PLUS N1 (DIFFERENCE N2 (CAR BESTQ))))
         (SETQ DE1P (CADR BESTQ))
         (SETQ DE2P (CDDR BESTQ))
         (SETQ QL (CDR QL)))))
      (RETURN
       (COND ((OR (NULL B) (AND TAKE_FIRST (NULL A))) NIL)
             (T
              (PROGN
               (SETQ NL2
                       (COND (REPLACE1 (COND (FLIP DE2) (T DE1))) (FLIP DE1)
                             (T DE2)))
               (SETQ NL1 (COND ((EQUAL NL2 DE2) DE1) (T DE2)))
               (SETQ L1 (GET NL1 'SQVAL))
               (SETQ L2 (GET NL2 'SQVAL))
               (SETQ M1
                       (COND
                        ((EQUAL NL2 DE2)
                         (COND (FLIP (CONS DE1P DE2P)) (T (CONS DE2P DE1P))))
                        (FLIP (CONS DE2P DE1P)) (T (CONS DE1P DE2P))))
               (SETQ L2 (ADDSQ L2 (NEGSQ (MULTSQ M1 L1))))
               (COND
                ((AND (NULL TAKE_FIRST)
                      (NEQ R (SETQ M2 (NO_OF_TM_SF (CAR L2)))))
                 (PROGN
                  (PROGN
                   (PRIN2
                    "##############################################################################")
                   NIL)
                  (PROGN (PRIN2 "Wrong expected length value:") NIL)
                  (TERPRI)
                  (PROGN (PRIN2 "expected value: r=") (PRIN2 R) NIL)
                  (TERPRI)
                  (PROGN (PRIN2 "real no of terms: ") (PRIN2 M2) NIL)
                  (TERPRI)
                  (PROGN
                   (AEVAL (OFF (LIST 'NAT)))
                   (PROGN
                    (ASSGNPRI (AEVAL "de1:=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL (LIST '*SQ (GET DE1 'SQVAL) T)) NIL
                              'LAST))
                   (PROGN
                    (ASSGNPRI (AEVAL "de2:=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL (LIST '*SQ (GET DE2 'SQVAL) T)) NIL
                              'LAST))
                   (PROGN
                    (ASSGNPRI (AEVAL "m1 =") NIL 'FIRST)
                    (ASSGNPRI (AEVAL (LIST '*SQ M1 T)) NIL 'LAST))
                   (AEVAL (ON (LIST 'NAT))))
                  (PROGN (PRIN2 "ql=") NIL)
                  (TERPRI)
                  (PRETTYPRINT QL))))
               (COND (TAKE_FIRST (SETQ R (NO_OF_TM_SF (CAR L2))))
                     (T
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT QL) (RETURN NIL)))
                        (PROGN
                         (COND
                          ((NEQ BESTQ (CAR QL))
                           (PROGN
                            (SETQ M2
                                    (COND
                                     ((EQUAL NL2 DE2)
                                      (COND (FLIP (CONS (CADAR QL) (CDDAR QL)))
                                            (T (CONS (CDDAR QL) (CADAR QL)))))
                                     (FLIP (CONS (CDDAR QL) (CADAR QL)))
                                     (T (CONS (CADAR QL) (CDDAR QL)))))
                            (SETQ A (ADDSQ L2 (NEGSQ (MULTSQ M2 L1))))
                            (SETQ B (NO_OF_TM_SF (CAR A)))
                            (COND
                             ((LESSP B R)
                              (PROGN
                               (SETQ L2 A)
                               (SETQ R B)
                               (SETQ M1 (ADDSQ M1 M2))))))))
                         (SETQ QL (CDR QL))
                         NIL)
                        (GO WHILELABEL))))
               (COND
                ((IN_CYCLE
                  (LIST 11 STEPCOUNTER_ (GET L1 'PRINTLENGTH)
                        (LENGTH (GET L1 'FCTS)) (CAR M1) (GET L2 'PRINTLENGTH)
                        (LENGTH (GET L2 'FCTS)) (CDR M1)))
                 (PROGN
                  (COND
                   ((AND PRINT_ TR_SHORT)
                    (PROGN
                     (PROGN
                      (PRIN2
                       "To avoid a loop, a possible shortening was not performed.")
                      NIL)
                     (TERPRI))))
                  NIL))
                (T
                 (PROGN
                  (COND
                   (PRINT_
                    (PROGN
                     (SETQ A (GET NL2 'TERMS))
                     (SETQ B (DIFFERENCE A R))
                     (COND
                      ((NULL TR_SHORT)
                       (PROGN
                        (PRIN2 "Shortening by ")
                        (PRIN2 B)
                        (PRIN2 (COND ((EQUAL B 1) " term") (T " terms")))
                        (PRIN2 " to now ")
                        (PRIN2 R)
                        (PRIN2 (COND ((EQUAL R 1) " term.") (T " terms.")))
                        NIL))
                      (T
                       (PROGN
                        (TERPRI)
                        (COND
                         ((EQUAL R 0)
                          (PROGN
                           (PROGN
                            (PRIN2 "Equation ")
                            (PRIN2 NL2)
                            (PRIN2 "(")
                            (PRIN2 A)
                            (PRIN2 ") is deleted as it is a consequence of ")
                            (PRIN2 NL1)
                            (PRIN2 "(")
                            (PRIN2 (GET NL1 'TERMS))
                            (PRIN2 ") :")
                            NIL)
                           (PROGN
                            (ASSGNPRI (AEVAL " 0 = ") NIL 'FIRST)
                            (ASSGNPRI
                             (AEVAL
                              (LIST '*SQ
                                    (CONS
                                     (CAR
                                      (ADDSQ (MKSQ NL2 1)
                                             (NEGSQ (MULTSQ M1 (MKSQ NL1 1)))))
                                     1)
                                    T))
                             NIL 'LAST))))
                         (T
                          (PROGN
                           (COND
                            ((NULL (CAR RECYCLE_EQNS))
                             (SETQ M2 (MKID EQNAME_ NEQU_)))
                            (T (SETQ M2 (CAAR RECYCLE_EQNS))))
                           (PROGN
                            (PRIN2 "Equation ")
                            (PRIN2 NL2)
                            (PRIN2 "(")
                            (PRIN2 A)
                            (PRIN2 ") is shortened by ")
                            (PRIN2 B)
                            (PRIN2 (COND ((EQUAL B 1) " term") (T " terms")))
                            (PRIN2 " using ")
                            (PRIN2 NL1)
                            (PRIN2 "(")
                            (PRIN2 (GET NL1 'TERMS))
                            (PRIN2 ") ")
                            (PRIN2 "and ")
                            (PRIN2 "is ")
                            (PRIN2 "replaced by:")
                            NIL)
                           (PROGN
                            (ASSGNPRI (AEVAL M2) NIL 'FIRST)
                            (ASSGNPRI (AEVAL "(") NIL NIL)
                            (ASSGNPRI (AEVAL R) NIL NIL)
                            (ASSGNPRI (AEVAL ") = ") NIL NIL)
                            (ASSGNPRI
                             (AEVAL
                              (LIST '*SQ
                                    (CONS
                                     (CAR
                                      (ADDSQ (MKSQ NL2 1)
                                             (NEGSQ (MULTSQ M1 (MKSQ NL1 1)))))
                                     1)
                                    T))
                             NIL 'LAST)))))))))))
                  (CONS (LIST L2) (LIST NL2))))))))))) 
(PUT 'CLEAN_NUM 'NUMBER-OF-ARGS 2) 
(PUT 'CLEAN_NUM 'DEFINED-ON-LINE '790) 
(PUT 'CLEAN_NUM 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'CLEAN_NUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLEAN_NUM (QC J)
    (PROG (QC1 NALL)
      (RETURN
       (COND ((LEQ (TIMES 2 (CDAAR QC)) J) T)
             (T
              (PROGN
               (SETQ QC1 (CAR QC))
               (SETQ NALL (CDAR QC1))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (CDR QC1)) (RETURN NIL)))
                 (COND
                  ((LEQ (PLUS (CDADR QC1) NALL) J) (RPLACD QC1 (CDDR QC1)))
                  (T (SETQ QC1 (CDR QC1))))
                 (GO WHILELABEL))
               (COND ((EQUAL QC1 (CAR QC)) T) (T NIL)))))))) 
(PUT 'CLEAN_DEN 'NUMBER-OF-ARGS 2) 
(PUT 'CLEAN_DEN 'DEFINED-ON-LINE '806) 
(PUT 'CLEAN_DEN 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'CLEAN_DEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLEAN_DEN (QC J)
    (PROG (QCC)
      (SETQ QCC QC)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR QC)) (RETURN NIL)))
        (COND ((CLEAN_NUM (CDR QC) J) (RPLACD QC (CDDR QC)))
              (T (SETQ QC (CDR QC))))
        (GO WHILELABEL))
      (RETURN (NULL (CDR QCC))))) 
(PUT 'FIND_BEST_QUOTIENT 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_BEST_QUOTIENT 'DEFINED-ON-LINE '818) 
(PUT 'FIND_BEST_QUOTIENT 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'FIND_BEST_QUOTIENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_BEST_QUOTIENT (QL)
    (PROG (S CP N M QLIST BESTNU BESTQ)
      (SETQ BESTQ (CONS 0 (CONS NIL NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT QL) (RETURN NIL)))
        (PROGN
         (SETQ S (CDAR QL))
         (PROG ()
          WHILELABEL
           (COND ((NOT S) (RETURN NIL)))
           (PROGN
            (SETQ CP (CAR S))
            (SETQ S (CDR S))
            (SETQ N (CDAR CP))
            (SETQ BESTNU (CDR CP))
            (PROG ()
             WHILELABEL
              (COND ((NOT (CDR BESTNU)) (RETURN NIL)))
              (COND
               ((LEQ (CDAR BESTNU) (CDADR BESTNU)) (SETQ BESTNU (CDR BESTNU)))
               (T (RPLACD BESTNU (CDDR BESTNU))))
              (GO WHILELABEL))
            (SETQ M (PLUS N (CDAR BESTNU)))
            (SETQ QLIST
                    (CONS
                     (CONS M
                           (CONS
                            (COND ((EQUAL (CAAAR BESTNU) 1) (CAAR CP))
                                  (T
                                   ((LAMBDA (G131)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAAR CP) G131))
                                       (T (POLY-MULTF (CAAR CP) G131))))
                                    (CAAAR BESTNU))))
                            (COND ((EQUAL (CDAAR BESTNU) 1) (CAAR QL))
                                  (T
                                   ((LAMBDA (G133)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAAR QL) G133))
                                       (T (POLY-MULTF (CAAR QL) G133))))
                                    (CDAAR BESTNU))))))
                     QLIST))
            (COND ((GREATERP M (CAR BESTQ)) (SETQ BESTQ (CAR QLIST)))))
           (GO WHILELABEL))
         (SETQ QL (CDR QL)))
        (GO WHILELABEL))
      (RETURN (CONS BESTQ QLIST)))) 
(PUT 'SHORT 'NUMBER-OF-ARGS 13) 
(PUT 'SHORT 'DEFINED-ON-LINE '885) 
(PUT 'SHORT 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'SHORT 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SHORT
    (QL1 QL2 D1 D2 N1 N2 N1_NOW MAX_SAVE_NOW MAX_SAVE_LATER TAKE_FIRST
     NO_FACTORS_X_1 NO_FACTORS_X_2 TRYBOTH)
    (PROG (LASTQSUCCESS D2COP J1 J2 E1 E2 Q DCL NU LDCL LNU H REPL1 REPL2
           ALLOWEDQ MAX_COEFF_LEN_REACHED)
      (SETQ J1 (PLUS MAX_SAVE_LATER MAX_SAVE_NOW))
      (SETQ J2 (DIFFERENCE N1 J1))
      (SETQ J1 (DIFFERENCE N2 J1))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ N1_NOW (DIFFERENCE N1_NOW 2))
         (SETQ E1 (FIRST_TERM_SF D1))
         (SETQ D1 (ADDF D1 (NEGF E1)))
         (SETQ D2COP D2)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND D2COP (NOT (AND TAKE_FIRST LASTQSUCCESS))))
             (RETURN NIL)))
           (PROGN
            (SETQ E2 (FIRST_TERM_SF D2COP))
            (SETQ D2COP (ADDF D2COP (NEGF E2)))
            (SETQ Q (CANCEL (CONS E1 E2)))
            (SETQ DCL (CDR Q))
            (SETQ REPL1 TRYBOTH)
            (COND
             ((OR (ATOM DCL) (ATOM (CAR DCL)))
              (PROGN (SETQ LDCL DCL) (SETQ DCL 1)))
             (T
              (PROGN
               (COND
                ((AND REPL1 NO_FACTORS_X_1
                      (NOT (FREEOFLIST DCL NO_FACTORS_X_1)))
                 (SETQ REPL1 NIL)))
               (SETQ H DCL)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT
                    (NOT
                     ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                      (SETQ LDCL (CDAR H)))))
                   (RETURN NIL)))
                 (SETQ H LDCL)
                 (GO WHILELABEL))
               (RPLACD (CAR H) 1)
               NIL)))
            (SETQ NU (CAR Q))
            (COND
             ((OR (ATOM NU) (ATOM (CAR NU)))
              (PROGN
               (SETQ LNU NU)
               (SETQ NU 1)
               (SETQ REPL2 T)
               (SETQ ALLOWEDQ T)))
             (T
              (PROGN
               (COND
                ((AND NO_FACTORS_X_2 (NOT (FREEOFLIST NU NO_FACTORS_X_2)))
                 (PROGN (SETQ ALLOWEDQ REPL1) (SETQ REPL2 NIL)))
                (T (PROGN (SETQ REPL2 T) (SETQ ALLOWEDQ T))))
               (COND
                (ALLOWEDQ
                 (PROGN
                  (SETQ H NU)
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT
                       (NOT
                        ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
                         (SETQ LNU (CDAR H)))))
                      (RETURN NIL)))
                    (SETQ H LNU)
                    (GO WHILELABEL))
                  (COND
                   ((NEQ LNU 1) (SETQ NU (CAR (CANCEL (CONS NU LNU))))))))))))
            (COND
             ((AND ALLOWEDQ
                   (OR
                    (AND (NUMBERP LNU)
                         (OR (GREATERP LNU MAX_COEFF_LEN)
                             (LESSP LNU (MINUS MAX_COEFF_LEN))))
                    (AND (PAIRP LNU) (EQUAL (CAR LNU) '|:GI:|)
                         (OR (GREATERP (CADR LNU) MAX_COEFF_LEN)
                             (LESSP (CADR LNU) (MINUS MAX_COEFF_LEN))
                             (GREATERP (CDDR LNU) MAX_COEFF_LEN)
                             (LESSP (CDDR LNU) (MINUS MAX_COEFF_LEN))))
                    (AND (NUMBERP LDCL)
                         (OR (GREATERP LDCL MAX_COEFF_LEN)
                             (LESSP LDCL (MINUS MAX_COEFF_LEN))))
                    (AND (PAIRP LDCL) (EQUAL (CAR LDCL) '|:GI:|)
                         (OR (GREATERP (CADR LDCL) MAX_COEFF_LEN)
                             (LESSP (CADR LDCL) (MINUS MAX_COEFF_LEN))
                             (GREATERP (CDDR LDCL) MAX_COEFF_LEN)
                             (LESSP (CDDR LDCL) (MINUS MAX_COEFF_LEN))))))
              (PROGN
               (COND
                ((AND TR_SHORT (NULL MAX_COEFF_LEN_REACHED))
                 (PROGN
                  (PROGN
                   (PRIN2 "### Num. factors grew too large in shortening.")
                   NIL)
                  (TERPRI)
                  NIL)))
               (SETQ MAX_COEFF_LEN_REACHED T)))
             ((NULL ALLOWEDQ) (SETQ LASTQSUCCESS NIL))
             ((AND REPL1 (OR (NULL REPL2) (GREATERP N1 N2)))
              (PROGN
               (SETQ H (ADD_QUOTIENT QL1 NU LNU DCL LDCL J1))
               (COND ((GREATERP (CAR H) N2) (SETQ LASTQSUCCESS T))
                     (T (SETQ LASTQSUCCESS NIL)))
               (SETQ QL1 (CDR H))))
             (T
              (PROGN
               (SETQ H (ADD_QUOTIENT QL2 NU LNU DCL LDCL J2))
               (COND ((GREATERP (CAR H) N1) (SETQ LASTQSUCCESS T))
                     (T (SETQ LASTQSUCCESS NIL)))
               (SETQ QL2 (CDR H))))))
           (GO WHILELABEL))
         (COND
          ((LESSP N1_NOW MAX_SAVE_NOW)
           (PROGN
            (SETQ J1 (PLUS MAX_SAVE_LATER N1_NOW))
            (SETQ J2 (DIFFERENCE N1 J1))
            (SETQ J1 (DIFFERENCE N2 J1))
            (COND ((GREATERP J1 0) (SETQ QL1 (CLEAN_QL QL1 J1))))
            (COND ((GREATERP J2 0) (SETQ QL2 (CLEAN_QL QL2 J2))))
            NIL)))
         NIL)
        (COND
         ((NOT
           (OR (NULL D1) (AND TAKE_FIRST LASTQSUCCESS)
               (AND (OR (NULL TRYBOTH) (AND (GREATERP J1 0) (NULL QL1)))
                    (GREATERP J2 0) (NULL QL2))))
          (GO REPEATLABEL))))
      (RETURN
       (COND
        ((AND (OR (NULL TRYBOTH) (AND (GREATERP J1 0) (NULL QL1)))
              (GREATERP J2 0) (NULL QL2))
         NIL)
        ((NULL D1) (CONS QL1 (CONS QL2 NIL))) (T (CONS QL1 (CONS QL2 Q))))))) 
(PUT 'CLEAN_QL 'NUMBER-OF-ARGS 2) 
(PUT 'CLEAN_QL 'DEFINED-ON-LINE '1066) 
(PUT 'CLEAN_QL 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'CLEAN_QL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLEAN_QL (QL J)
    (PROG (QC)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND QL (CLEAN_DEN (CAR QL) J))) (RETURN NIL)))
        (SETQ QL (CDR QL))
        (GO WHILELABEL))
      (COND
       (QL
        (PROGN
         (SETQ QC QL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR QC)) (RETURN NIL)))
           (COND ((CLEAN_DEN (CADR QC) J) (RPLACD QC (CDDR QC)))
                 (T (SETQ QC (CDR QC))))
           (GO WHILELABEL)))))
      (RETURN QL))) 
(PUT 'ADD_QUOTIENT 'NUMBER-OF-ARGS 6) 
(PUT 'ADD_QUOTIENT 'DEFINED-ON-LINE '1080) 
(PUT 'ADD_QUOTIENT 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'ADD_QUOTIENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_QUOTIENT (QL NU LNU DCL LDCL J)
    (PROG (NALL M QC QQ PREQC)
      (COND
       ((NULL *COMPLEX)
        (PROGN
         (COND
          ((PAIRP LDCL)
           (PROGN
            (PROGN
             (PRIN2 "####### ldcl=")
             (PRIN2 LDCL)
             (PRIN2 " is not an integer!")
             NIL)
            (TERPRI))))
         (COND
          ((AND (PAIRP LNU) (NEQ (CAR LNU) '|:RN:|))
           (PROGN
            (PROGN
             (PRIN2 "####### lnu=")
             (PRIN2 LNU)
             (PRIN2 " is not rational!")
             NIL)
            (TERPRI))))
         (COND ((FIXP LNU) (SETQ QQ (CONS LNU LDCL)))
               ((EQUAL LDCL 1) (SETQ QQ (CDR LNU)))
               (T
                (PROGN
                 (SETQ M (CANCEL (CONS (CADR LNU) LDCL)))
                 (SETQ QQ (CONS (CAR M) (TIMES (CDDR LNU) (CDR M)))))))))
       ((FIXP LDCL) (SETQ QQ (CONS LNU LDCL)))
       (T (SETQ QQ (MULTSQ (CONS LNU 1) (INVSQ (CONS LDCL 1))))))
      (SETQ QC QL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND QC (NEQ DCL (CAAR QC)))) (RETURN NIL)))
        (SETQ QC (CDR QC))
        (GO WHILELABEL))
      (COND
       ((NULL QC)
        (COND
         ((LEQ J 0)
          (PROGN
           (SETQ NALL 2)
           (SETQ QL
                   (CONS (CONS DCL (LIST (LIST (CONS NU 1) (CONS QQ 1))))
                         QL))))
         (T (SETQ NALL 0))))
       (T
        (PROGN
         (SETQ QC (CDAR QC))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND QC (NEQ NU (CAAAR QC)))) (RETURN NIL)))
           (PROGN (SETQ PREQC QC) (SETQ QC (CDR QC)))
           (GO WHILELABEL))
         (COND
          ((NULL QC)
           (COND
            ((LEQ J 0)
             (PROGN
              (SETQ NALL 2)
              (RPLACD PREQC (LIST (LIST (CONS NU 1) (CONS QQ 1))))))
            (T (SETQ NALL 0))))
          (T
           (PROGN
            (SETQ NALL (ADD1 (CDAAR QC)))
            (RPLACD (CAAR QC) NALL)
            (SETQ QC (CDAR QC))
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND QC (NEQ QQ (CAAR QC)))) (RETURN NIL)))
              (PROGN (SETQ PREQC QC) (SETQ QC (CDR QC)))
              (GO WHILELABEL))
            (COND
             ((NULL QC)
              (PROGN
               (RPLACD PREQC (LIST (CONS QQ 1)))
               (SETQ NALL (ADD1 NALL))))
             (T
              (PROGN
               (SETQ M (ADD1 (CDAR QC)))
               (RPLACD (CAR QC) M)
               (SETQ NALL (PLUS NALL M)))))))))))
      (RETURN (CONS NALL QL)))) 
(PUT 'DROP_LIN_DEP 'NUMBER-OF-ARGS 1) 
(PUT 'DROP_LIN_DEP 'DEFINED-ON-LINE '1184) 
(PUT 'DROP_LIN_DEP 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'DROP_LIN_DEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DROP_LIN_DEP (ARGLIST)
    (PROG (PDES TR_DROP P CP INCRE NEWPDES M H S R A V VLI INDP INDLI CONLI MLI
           SUCCESS)
      (SETQ PDES (CAR ARGLIST))
      (COND
       ((AND PDES (CDR PDES))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT PDES) (RETURN NIL)))
           (PROGN
            (SETQ P (CAR PDES))
            (SETQ PDES (CDR PDES))
            (SETQ NEWPDES (CONS P NEWPDES))
            (SETQ M (GENSYM))
            (SETQ A (SORT_PARTITION P NIL (GET P 'FCTS) NIL))
            (COND
             (TR_DROP
              (PROGN
               (PROGN (PRIN2 "new eqn:") NIL)
               (PRETTYPRINT A)
               (PROGN (PRIN2 "multiplier for this equation: m=") (PRIN2 M) NIL)
               (TERPRI))))
            (SETQ INDP NIL)
            (PROG (H)
              (SETQ H A)
             LAB
              (COND ((NULL H) (RETURN NIL)))
              ((LAMBDA (H)
                 (PROGN
                  (SETQ S (CAR H))
                  (SETQ CP VLI)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (AND CP (NEQ S (CAAR CP)))) (RETURN NIL)))
                    (SETQ CP (CDR CP))
                    (GO WHILELABEL))
                  (COND
                   (TR_DROP
                    (PROGN
                     (PROGN (PRIN2 "searched for: s=") (PRIN2 S) NIL)
                     (TERPRI)
                     (COND
                      (CP
                       (PROGN (PRIN2 "found: car cp=") (PRIN2 (CAR CP)) NIL)))
                     (TERPRI)
                     NIL)))
                  (COND
                   (CP
                    (PROGN
                     (SETQ R (CADAR CP))
                     (SETQ INCRE
                             (REVAL1
                              (LIST 'QUOTIENT
                                    (LIST 'TIMES M R
                                          (COND
                                           ((GREATERP (CADR H) 1)
                                            (CONS 'PLUS (CADDR H)))
                                           (T (CAADDR H))))
                                    S)
                              T))
                     (RPLACA (CDDAR CP) (CONS INCRE (CADDAR CP)))))
                   (T
                    (PROGN
                     (SETQ R
                             (COND ((OR (PAIRP S) (NUMBERP S)) (GENSYM))
                                   (T S)))
                     (SETQ INDP S)
                     (SETQ INCRE
                             (REVAL1
                              (LIST 'QUOTIENT
                                    (LIST 'TIMES M R
                                          (COND
                                           ((GREATERP (CADR H) 1)
                                            (CONS 'PLUS (CADDR H)))
                                           (T (CAADDR H))))
                                    S)
                              T))
                     (SETQ VLI (CONS (LIST S R (LIST INCRE)) VLI)))))
                  (COND
                   (TR_DROP
                    (PROGN
                     (PROGN (PRIN2 "corresponding symbol: r=") (PRIN2 R) NIL)
                     (TERPRI)
                     (PROGN (PRIN2 "upd: incre=") (PRIN2 INCRE) NIL)
                     (TERPRI)
                     (PROGN (PRIN2 "vli=") NIL)
                     (PRETTYPRINT VLI))))
                  NIL))
               (CAR H))
              (SETQ H (CDR H))
              (GO LAB))
            (SETQ MLI (CONS M MLI))
            (SETQ INDLI (CONS INDP INDLI)))
           (GO WHILELABEL))
         (PROG ()
          WHILELABEL
           (COND ((NOT VLI) (RETURN NIL)))
           (PROGN
            (SETQ V (CADDAR VLI))
            (SETQ VLI (CDR VLI))
            (SETQ CONLI
                    (CONS (COND ((CDR V) (CONS 'PLUS V)) (T (CAR V))) CONLI)))
           (GO WHILELABEL))
         (SETQ PDES NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (CDR NEWPDES)) (RETURN NIL)))
           (PROGN
            (COND
             (TR_DROP
              (PROGN
               (TERPRI)
               (COND
                ((CAR INDLI)
                 (PROGN
                  (PRIN2 "lin. indep. without search of ")
                  (PRIN2 (CAR NEWPDES))
                  (PRIN2 " due to the occurence of ")
                  (PRIN2 (CAR INDLI))
                  NIL))
                (T
                 (PROGN
                  (PRIN2 "lin. indep. investigation for ")
                  (PRIN2 (CAR NEWPDES))
                  NIL)))
               NIL)))
            (COND ((CAR INDLI) (SETQ PDES (CONS (CAR NEWPDES) PDES)))
                  (T
                   (PROGN
                    (SETQ S
                            (CDR
                             (SOLVEEVAL
                              (LIST (CONS 'LIST (SUBST 1 (CAR MLI) CONLI))
                                    (CONS 'LIST (CDR MLI))))))
                    (COND
                     (S
                      (PROGN
                       (DROP_PDE (CAR NEWPDES) NIL NIL)
                       (SETQ SUCCESS T)
                       (COND
                        (PRINT_
                         (PROGN
                          (TERPRI)
                          (PROGN
                           (PRIN2 "Eqn. ")
                           (PRIN2 (CAR NEWPDES))
                           (PRIN2
                            " has been dropped due to linear dependence.")
                           NIL)
                          NIL)))))
                     (T
                      (PROGN
                       (SETQ PDES (CONS (CAR NEWPDES) PDES))
                       (COND
                        (TR_DROP
                         (PROGN
                          (TERPRI)
                          (PROGN
                           (PRIN2 "Eqn. ")
                           (PRIN2 (CAR NEWPDES))
                           (PRIN2 " is lin. indep.")
                           NIL)
                          NIL))))))
                    NIL)))
            (SETQ NEWPDES (CDR NEWPDES))
            (SETQ INDLI (CDR INDLI))
            (SETQ CONLI (SUBST 0 (CAR MLI) CONLI))
            (SETQ MLI (CDR MLI)))
           (GO WHILELABEL))
         (SETQ PDES (CONS (CAR NEWPDES) PDES)))))
      (RETURN (COND (SUCCESS (LIST PDES (CADR ARGLIST))) (T NIL))))) 
(PUT 'FIND_1_TERM_EQN 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_1_TERM_EQN 'DEFINED-ON-LINE '1296) 
(PUT 'FIND_1_TERM_EQN 'DEFINED-IN-FILE 'CRACK/CRSHORT.RED) 
(PUT 'FIND_1_TERM_EQN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_1_TERM_EQN (ARGLIST)
    (COND ((NOT LIN_PROBLEM) NIL)
          (T
           (PROG (PDES TR_DROP P CP INCRE M H S R A V VLI INDP INDLI CONLI MLI
                  MPLI SUCCESS SLI SLILEN MAXLEN NEWCONLI NEWPDES NEWP FL VL)
             (COND (TR_DROP (TERPRI)))
             (SETQ PDES (CAR ARGLIST))
             (SETQ NEWPDES PDES)
             (COND
              ((AND PDES (CDR PDES))
               (PROGN
                (PROG ()
                 WHILELABEL
                  (COND ((NOT PDES) (RETURN NIL)))
                  (PROGN
                   (SETQ P (CAR PDES))
                   (SETQ PDES (CDR PDES))
                   (SETQ M (GENSYM))
                   (COND
                    (TR_DROP
                     (PROGN
                      (TERPRI)
                      (PROGN (PRIN2 "multiplier m=") (PRIN2 M) NIL)
                      (TERPRI))))
                   (SETQ A (SORT_PARTITION P NIL (GET P 'FCTS) NIL))
                   (PROG (H)
                     (SETQ H A)
                    LAB
                     (COND ((NULL H) (RETURN NIL)))
                     ((LAMBDA (H)
                        (PROGN
                         (SETQ S (CAR H))
                         (SETQ CP VLI)
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT (AND CP (NEQ S (CAAR CP)))) (RETURN NIL)))
                           (SETQ CP (CDR CP))
                           (GO WHILELABEL))
                         (COND
                          (TR_DROP
                           (PROGN
                            (PROGN (PRIN2 "searched for: s=") (PRIN2 S) NIL)
                            (TERPRI)
                            (COND
                             (CP
                              (PROGN
                               (PROGN
                                (PRIN2 "found: car cp=")
                                (PRIN2 (CAR CP))
                                NIL)
                               (TERPRI)
                               NIL))))))
                         (COND
                          (CP
                           (PROGN
                            (SETQ R (CADAR CP))
                            (SETQ INCRE
                                    (REVAL1
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES M
                                                 (COND
                                                  ((GREATERP (CADR H) 1)
                                                   (CONS 'PLUS (CADDR H)))
                                                  (T (CAADDR H))))
                                           S)
                                     T))
                            (RPLACA (CDDAR CP) (CONS INCRE (CADDAR CP)))))
                          (T
                           (PROGN
                            (SETQ R
                                    (COND ((OR (PAIRP S) (NUMBERP S)) (GENSYM))
                                          (T S)))
                            (SETQ INDP S)
                            (SETQ INCRE
                                    (REVAL1
                                     (LIST 'QUOTIENT
                                           (LIST 'TIMES M
                                                 (COND
                                                  ((GREATERP (CADR H) 1)
                                                   (CONS 'PLUS (CADDR H)))
                                                  (T (CAADDR H))))
                                           S)
                                     T))
                            (SETQ VLI (CONS (LIST S R (LIST INCRE)) VLI)))))
                         (COND
                          (TR_DROP
                           (PROGN
                            (PROGN
                             (PRIN2 "corresponding symbol: r=")
                             (PRIN2 R)
                             NIL)
                            (TERPRI)
                            (PROGN (PRIN2 "upd: incre=") (PRIN2 INCRE) NIL)
                            (TERPRI)
                            (PROGN (PRIN2 "vli=") NIL)
                            (PRETTYPRINT VLI))))
                         NIL))
                      (CAR H))
                     (SETQ H (CDR H))
                     (GO LAB))
                   (SETQ MLI (CONS M MLI))
                   (SETQ MPLI (CONS (CONS M P) MPLI))
                   (SETQ INDLI (CONS INDP INDLI)))
                  (GO WHILELABEL))
                (PROG ()
                 WHILELABEL
                  (COND ((NOT VLI) (RETURN NIL)))
                  (PROGN
                   (SETQ SLI (CONS (CAAR VLI) SLI))
                   (SETQ V (CADDAR VLI))
                   (SETQ VLI (CDR VLI))
                   (SETQ CONLI
                           (CONS (COND ((CDR V) (CONS 'PLUS V)) (T (CAR V)))
                                 CONLI)))
                  (GO WHILELABEL))
                (SETQ SLILEN (LENGTH SLI))
                (SETQ MLI (CONS 'LIST MLI))
                (SETQ CONLI (CONS 'LIST CONLI))
                (COND
                 (TR_DROP
                  (PROGN
                   (PROGN (PRIN2 "sli=") (PRIN2 SLI) NIL)
                   (TERPRI)
                   (PROGN
                    (ASSGNPRI (AEVAL "mli=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL MLI) NIL 'LAST))
                   (PROGN
                    (ASSGNPRI (AEVAL "conli=") NIL 'FIRST)
                    (ASSGNPRI (AEVAL CONLI) NIL 'LAST))
                   (PROGN (PRIN2 "mpli=") (PRIN2 MPLI) NIL)
                   (TERPRI)
                   NIL)))
                (PROG (H)
                  (SETQ H 1)
                 LAB
                  (COND ((MINUSP (DIFFERENCE SLILEN H)) (RETURN NIL)))
                  (PROGN
                   (SETQ NEWP (CAR SLI))
                   (SETQ SLI (CDR SLI))
                   (SETQ PDES NEWPDES)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT
                        (AND PDES
                             (OR (GREATERP (GET (CAR PDES) 'TERMS) 1)
                                 (NOT
                                  (ZEROP
                                   (REVAL1
                                    (LIST 'DIFFERENCE (GET (CAR PDES) 'VAL)
                                          NEWP)
                                    T))))))
                       (RETURN NIL)))
                     (SETQ PDES (CDR PDES))
                     (GO WHILELABEL))
                   (COND
                    ((NULL PDES)
                     (PROGN
                      (SETQ CP CONLI)
                      (PROG (S)
                        (SETQ S 1)
                       LAB
                        (COND ((MINUSP (DIFFERENCE H S)) (RETURN NIL)))
                        (SETQ CP (CDR CP))
                        (SETQ S (PLUS2 S 1))
                        (GO LAB))
                      (RPLACA CP (REVAL1 (LIST 'PLUS 1 (CAR CP)) T))
                      (COND
                       (TR_DROP
                        (PROGN
                         (PROGN (PRIN2 "h=") (PRIN2 H) NIL)
                         (TERPRI)
                         (PROGN
                          (ASSGNPRI (AEVAL* "new conli=") NIL 'FIRST)
                          (ASSGNPRI (AEVAL* CONLI) NIL 'LAST))
                         NIL)))
                      (SETQ S (CDR (SOLVEEVAL (LIST CONLI MLI))))
                      (COND
                       ((AND (NULL S) TR_DROP)
                        (PROGN (PROGN (PRIN2 "no success") NIL) (TERPRI))))
                      (COND
                       (S
                        (PROGN
                         (COND
                          ((NULL SUCCESS)
                           (PROG (P)
                             (SETQ P NEWPDES)
                            LAB
                             (COND ((NULL P) (RETURN NIL)))
                             ((LAMBDA (P)
                                (PROGN
                                 (SETQ FL (UNION (GET P 'FCTS) FL))
                                 (SETQ VL (UNION (GET P 'VARS) VL))))
                              (CAR P))
                             (SETQ P (CDR P))
                             (GO LAB))))
                         (SETQ SUCCESS T)
                         (SETQ MAXLEN 0)
                         (SETQ S (CDAR S))
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT S) (RETURN NIL)))
                           (PROGN
                            (COND
                             ((NEQ (CADDAR S) 0)
                              (PROGN
                               (SETQ R (CADAR S))
                               (SETQ CP MPLI)
                               (PROG ()
                                WHILELABEL
                                 (COND ((NOT (NEQ (CAAR CP) R)) (RETURN NIL)))
                                 (SETQ CP (CDR CP))
                                 (GO WHILELABEL))
                               (COND
                                ((GREATERP (GET (CDAR CP) 'TERMS) MAXLEN)
                                 (PROGN
                                  (SETQ P (CDAR CP))
                                  (SETQ M R)
                                  (SETQ MAXLEN (GET P 'TERMS))
                                  NIL))))))
                            (SETQ S (CDR S)))
                           (GO WHILELABEL))
                         (SETQ R 0)
                         (SETQ NEWCONLI NIL)
                         (PROG ()
                          WHILELABEL
                           (COND ((NOT CONLI) (RETURN NIL)))
                           (PROGN
                            (SETQ V (SUBST 0 M (CAR CONLI)))
                            (SETQ CONLI (CDR CONLI))
                            (COND
                             ((EQUAL R H)
                              (PROGN
                               (SETQ V
                                       (REVAL1
                                        (LIST 'PLUS (LIST 'TIMES M NEWP) V) T))
                               NIL)))
                            (SETQ NEWCONLI (CONS V NEWCONLI))
                            (SETQ R (ADD1 R)))
                           (GO WHILELABEL))
                         (SETQ CONLI (REVERSE NEWCONLI))
                         (SETQ NEWP
                                 (MKEQSQ NIL NIL NEWP FL VL ALLFLAGS_ T
                                  (LIST 0) NIL NEWPDES))
                         (SETQ NEWPDES (CONS NEWP NEWPDES))
                         (COND
                          (PRINT_
                           (PROGN
                            (TERPRI)
                            (PROGN
                             (PRIN2 "The new equation ")
                             (PRIN2 NEWP)
                             NIL)
                            (TYPEEQ NEWP)
                            (PROGN (PRIN2 " replaces ") (PRIN2 P) NIL)
                            (TYPEEQ P)
                            NIL)))
                         (DROP_PDE P NIL NIL)
                         (SETQ NEWPDES (DELETE P NEWPDES))
                         (SETQ MPLI (SUBST NEWP P MPLI))
                         (COND
                          (TR_DROP
                           (PROGN
                            (PROGN (PRIN2 "mpli=") (PRIN2 MPLI) NIL)
                            (TERPRI)
                            NIL)))
                         NIL)))
                      (SETQ CP CONLI)
                      (PROG (S)
                        (SETQ S 1)
                       LAB
                        (COND ((MINUSP (DIFFERENCE H S)) (RETURN NIL)))
                        (SETQ CP (CDR CP))
                        (SETQ S (PLUS2 S 1))
                        (GO LAB))
                      (RPLACA CP (REVAL1 (LIST 'PLUS (MINUS 1) (CAR CP)) T))
                      NIL))))
                  (SETQ H (PLUS2 H 1))
                  (GO LAB))
                NIL)))
             (RETURN (COND (SUCCESS (LIST NEWPDES (CADR ARGLIST))) (T NIL))))))) 
(ENDMODULE) 