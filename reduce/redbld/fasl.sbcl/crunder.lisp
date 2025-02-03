(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'UNDERDETDE)) 
(PUT 'UNDETALG 'NUMBER-OF-ARGS 1) 
(PUT 'UNDETALG 'DEFINED-ON-LINE '33) 
(PUT 'UNDETALG 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UNDETALG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNDETALG (ARGLIST)
    (PROG (PDES FORG L L1 P H F1 F2 F3 F4 F5)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE (PROGN (SETQ L1 (SELECTPDES PDES 1)) (FLAG L1 'TO_UNDER)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND L1 (NULL L))) (RETURN NIL)))
        (COND
         ((AND (NULL (FLAGP (CAR L1) 'TO_UNDER))
               (EQUAL (GET (CAR L1) 'NVARS) 0))
          (SETQ L1 (CDR L1)))
         (T
          (PROGN
           (SETQ P (CAR L1))
           (SETQ L1 (CDR L1))
           (REMFLAG (LIST P) 'TO_UNDER)
           (SETQ H (GET P 'FCTS))
           (COND
            ((AND (GREATERP (GET P 'TERMS) 1) (NULL (GET P 'NONRATIONAL))
                  (CDR H) (NULL (CDDR H)))
             (PROGN
              (SETQ F1 (CAR H))
              (SETQ F2 (CADR H))
              (SETQ F3 NIL)
              (SETQ F4 NIL)
              (SETQ F5 NIL)
              (SETQ H (GET P 'DERIVS))
              (PROG ()
               WHILELABEL
                (COND ((NOT H) (RETURN NIL)))
                (PROGN
                 (COND
                  ((GREATERP (CDAR H) 1)
                   (SETQ F3 (UNION (LIST (CAAAR H)) F3))))
                 (COND
                  ((EQUAL (CDAR H) 2) (SETQ F4 (UNION (LIST (CAAAR H)) F4)))
                  ((GREATERP (CDAR H) 2)
                   (SETQ F5 (UNION (LIST (CAAAR H)) F5))))
                 (SETQ H (CDR H)))
                (GO WHILELABEL))
              (COND
               ((AND (MEMBER F1 F3) (MEMBER F2 F3)) (SETQ L (TRYALG1 P PDES))))
              (COND
               ((AND (NULL L) (NULL F5) (MEMBER F1 F4) (MEMBER F2 F4))
                (SETQ L (TRYALG2 P PDES))))))))))
        (GO WHILELABEL))
      (COND ((NULL L) (RETURN NIL)))
      (COND
       (PRINT_
        (PROGN
         (PROGN
          (PRIN2
           "Parametric solution of the 'underdetermined' algebraic equation ")
          (PRIN2 P)
          NIL)
         (TERPRI)
         (PROGN (PRIN2 "giving the new algebraic equation(s) ") NIL)
         (LISTPRINT L)
         (TERPRI))))
      (PROG (H)
        (SETQ H L)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ PDES (EQINSERT H PDES))
            (COND
             ((MEMBER H PDES)
              (SETQ TO_DO_LIST
                      (CONS (LIST 'SUBST_LEVEL_4 (LIST H)) TO_DO_LIST))))
            NIL))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (RETURN (LIST PDES FORG)))) 
(PUT 'TRYALG1 'NUMBER-OF-ARGS 2) 
(PUT 'TRYALG1 'DEFINED-ON-LINE '110) 
(PUT 'TRYALG1 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'TRYALG1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRYALG1 (P PDES)
    (PROG (F1 F2 F1D F2D D1 D2 GD PHI NEWF Q)
      (SETQ F2 (GET P 'FCTS))
      (SETQ F1 (CAR F2))
      (SETQ F1D (CAAAR (CAR (MKSQ F1 1))))
      (SETQ F2 (CADR F2))
      (SETQ F2D (CAAAR (CAR (MKSQ F2 1))))
      (SETQ D1 (DIFFSQ (GET P 'SQVAL) F1D))
      (SETQ D2 (DIFFSQ (GET P 'SQVAL) F2D))
      (SETQ GD (ERR_CATCH_GCD (LIST '*SQ D1 T) (LIST '*SQ D2 T)))
      (RETURN
       (COND ((OR (FREEOF GD F1) (FREEOF GD F2)) NIL)
             (T
              (PROGN
               (SETQ D1 (MULTSQ D1 (INVSQ (CADR GD))))
               (SETQ D2 (MULTSQ D2 (INVSQ (CADR GD))))
               (COND
                ((NOT
                  (SQZEROP (ADDSQ (DIFFSQ D1 F2D) (NEGSQ (DIFFSQ D2 F1D)))))
                 NIL)
                (T
                 (PROGN
                  (SETQ PHI (SIMP (REVAL1 (LIST 'INT (PREPSQ D1) F1) T)))
                  (SETQ PHI
                          (ADDSQ PHI
                                 (SIMP
                                  (REVAL1
                                   (LIST 'INT
                                         (PREPSQ
                                          (ADDSQ D2 (NEGSQ (DIFFSQ PHI F2D))))
                                         F2)
                                   T))))
                  (SETQ NEWF (NEWFCT FNAME_ NIL NFCT_))
                  (SETQ NFCT_ (ADD1 NFCT_))
                  (SETQ FTEM_ (FCTINSERT NEWF FTEM_))
                  (SETQ Q
                          (MKEQSQ (ADDSQ (SIMP NEWF) (NEGSQ PHI)) NIL NIL
                           (LIST NEWF F1 F2) NIL ALLFLAGS_ NIL (LIST 0) NIL
                           PDES))
                  (PUT Q 'NOT_TO_EVAL (LIST NEWF))
                  (LIST Q)))))))))) 
(PUT 'TRYALG2 'NUMBER-OF-ARGS 2) 
(PUT 'TRYALG2 'DEFINED-ON-LINE '144) 
(PUT 'TRYALG2 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'TRYALG2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRYALG2 (P PDES)
    (PROG (H F1 F2 F3 S K L D Q)
      (SETQ H (GET P 'FCTS))
      (SETQ F1 (CAR H))
      (SETQ F2 (CADR H))
      (SETQ S (GENSYM))
      (SETQ K (SETKORDER (LIST S)))
      (SETQ H
              (SIMP*
               (LIST '*SQ
                     (SUBSQ (GET P 'SQVAL)
                            (LIST (CONS F1 (LIST 'TIMES F1 S))
                                  (CONS F2 (LIST 'TIMES F2 S))))
                     NIL)))
      (SETKORDER K)
      (RETURN
       (COND ((OR (NEQ (CAAAR (CAR H)) S) (NEQ (CDAAR (CAR H)) 2)) NIL)
             (T
              (PROGN
               (SETQ H (MKSQ (CDAR (CAR H)) 1))
               (SETQ D (SOLVEEVAL (LIST (LIST '*SQ H T) F1)))
               (COND
                ((NOT (FREEOF D 'ABS))
                 (PROGN
                  (AEVAL (LET '(ABS_)))
                  (SETQ D (AEVAL D))
                  (AEVAL (CLEARRULES (LIST 'ABS_)))
                  NIL)))
               (SETQ L NIL)
               (COND
                ((AND D (EQUAL (CAR D) 'LIST))
                 (PROG (H)
                   (SETQ H (CDR D))
                  LAB
                   (COND ((NULL H) (RETURN NIL)))
                   ((LAMBDA (H)
                      (PROGN
                       (SETQ F3 (NEWFCT FNAME_ (GET P 'VARS) NFCT_))
                       (SETQ NFCT_ (ADD1 NFCT_))
                       (SETQ FTEM_ (FCTINSERT F3 FTEM_))
                       (SETQ Q
                               (MKEQSQ
                                (ADDSQ (SIMP F3)
                                       (ADDSQ (SIMP (CADR H))
                                              (NEGSQ (SIMP (CADDR H)))))
                                NIL NIL (LIST F1 F2 F3) (GET P 'VARS) ALLFLAGS_
                                T (LIST 0) NIL PDES))
                       (PUT Q 'NOT_TO_EVAL (LIST F3))
                       (SETQ L (CONS Q L))))
                    (CAR H))
                   (SETQ H (CDR H))
                   (GO LAB))))
               L)))))) 
(PUT 'UNDETLINODE 'NUMBER-OF-ARGS 1) 
(PUT 'UNDETLINODE 'DEFINED-ON-LINE '192) 
(PUT 'UNDETLINODE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UNDETLINODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNDETLINODE (ARGLIST)
    (PROG (L L1 P PDES FORG S)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE (PROGN (SETQ L1 (SELECTPDES PDES 1)) (FLAG L1 'TO_UNDER)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (PROG ()
       WHILELABEL
        (COND ((NOT L1) (RETURN NIL)))
        (COND ((NULL (SETQ P (GET_ULODE L1))) (SETQ L1 NIL))
              (T
               (PROGN
                (SETQ L (UNDERODE P PDES))
                (SETQ P (CAR P))
                (COND
                 ((NULL L)
                  (PROGN (REMFLAG (LIST P) 'TO_UNDER) (SETQ L1 (DELETE P L1))))
                 (T
                  (PROGN
                   (COND
                    (PRINT_
                     (PROGN
                      (PROGN
                       (PRIN2
                        "Parametric solution of the underdetermined ODE ")
                       (PRIN2 P)
                       NIL)
                      (TERPRI)
                      (PROGN (PRIN2 "giving the new ODEs ") NIL)
                      (SETQ S L)
                      (PROG ()
                       WHILELABEL
                        (COND ((NOT S) (RETURN NIL)))
                        (PROGN
                         (PROGN (PRIN2 (CAR S)) NIL)
                         (SETQ S (CDR S))
                         (COND (S (PROGN (PRIN2 ",") NIL))))
                        (GO WHILELABEL))
                      (TERPRI))))
                   (SETQ PDES (DROP_PDE P PDES NIL))
                   (PROG (S)
                     (SETQ S L)
                    LAB
                     (COND ((NULL S) (RETURN NIL)))
                     ((LAMBDA (S) (SETQ PDES (EQINSERT S PDES))) (CAR S))
                     (SETQ S (CDR S))
                     (GO LAB))
                   (SETQ L (LIST PDES FORG))
                   (SETQ L1 NIL)
                   NIL))))))
        (GO WHILELABEL))
      (RETURN L))) 
(PUT 'UNDETLINPDE 'NUMBER-OF-ARGS 1) 
(PUT 'UNDETLINPDE 'DEFINED-ON-LINE '227) 
(PUT 'UNDETLINPDE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UNDETLINPDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNDETLINPDE (ARGLIST)
    (PROG (L L1 P PDES FORG)
      (SETQ PDES (CAR ARGLIST))
      (SETQ FORG (CADR ARGLIST))
      (COND
       (EXPERT_MODE (PROGN (SETQ L1 (SELECTPDES PDES 1)) (FLAG L1 'TO_UNDER)))
       (T (SETQ L1 (CADDDR ARGLIST))))
      (PROG ()
       WHILELABEL
        (COND ((NOT L1) (RETURN NIL)))
        (COND ((NULL (SETQ P (GET_ULPDE L1))) (SETQ L1 NIL))
              (T
               (PROGN
                (SETQ L (UNDERPDE P PDES))
                (SETQ P (CAR P))
                (COND
                 ((NULL L)
                  (PROGN (REMFLAG (LIST P) 'TO_UNDER) (SETQ L1 (DELETE P L1))))
                 (T
                  (PROGN
                   (SETQ PDES (DROP_PDE P PDES NIL))
                   (PROG (S)
                     (SETQ S L)
                    LAB
                     (COND ((NULL S) (RETURN NIL)))
                     ((LAMBDA (S) (SETQ PDES (EQINSERT S PDES))) (CAR S))
                     (SETQ S (CDR S))
                     (GO LAB))
                   (SETQ L (LIST PDES FORG))
                   (SETQ L1 NIL)
                   NIL))))))
        (GO WHILELABEL))
      (RETURN L))) 
(PUT 'GET_ULODE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_ULODE 'DEFINED-ON-LINE '263) 
(PUT 'GET_ULODE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'GET_ULODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_ULODE (PDES)
    (PROG (H BEST_ULODE)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((FLAGP P 'TO_UNDER)
             (COND ((NULL (SETQ H (ULODEP P))) (REMFLAG (LIST P) 'TO_UNDER))
                   ((OR (NULL BEST_ULODE) (LESSP (CAR H) (CAR BEST_ULODE)))
                    (SETQ BEST_ULODE H))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (COND (BEST_ULODE (CDR BEST_ULODE)) (T NIL))))) 
(PUT 'GET_ULPDE 'NUMBER-OF-ARGS 1) 
(PUT 'GET_ULPDE 'DEFINED-ON-LINE '275) 
(PUT 'GET_ULPDE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'GET_ULPDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_ULPDE (PDES)
    (PROG (H BEST_ULPDE)
      (PROG (P)
        (SETQ P PDES)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((AND (FLAGP P 'TO_UNDER) (GREATERP (GET P 'NVARS) 1))
             (COND ((NULL (SETQ H (ULPDEP P))) (REMFLAG (LIST P) 'TO_UNDER))
                   ((OR (NULL BEST_ULPDE) (LESSP (CAR H) (CAR BEST_ULPDE)))
                    (SETQ BEST_ULPDE H))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (COND (BEST_ULPDE (CDR BEST_ULPDE)) (T NIL))))) 
(PUT 'UDALGP 'NUMBER-OF-ARGS 1) 
(PUT 'UDALGP 'DEFINED-ON-LINE '286) 
(PUT 'UDALGP 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UDALGP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UDALGP (P)
    (COND
     ((OR (LESSP (GET P 'TERMS) 2) (GET P 'NONRATIONAL)
          (NEQ (LENGTH (GET P 'FCTS)) 2))
      NIL)
     (T
      (PROG (MXDEG DFS)
        (SETQ MXDEG 0)
        (SETQ DFS (GET P 'DERIVS))
        (PROG ()
         WHILELABEL
          (COND ((NOT DFS) (RETURN NIL)))
          (PROGN
           (COND ((GREATERP (CDAR DFS) MXDEG) (SETQ MXDEG (CDAR DFS))))
           (SETQ DFS (CDR DFS)))
          (GO WHILELABEL))
        (RETURN (COND ((LESSP MXDEG 2) NIL) (T (CONS MXDEG P)))))))) 
(PUT 'ULODEP 'NUMBER-OF-ARGS 1) 
(PUT 'ULODEP 'DEFINED-ON-LINE '301) 
(PUT 'ULODEP 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'ULODEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ULODEP (P)
    (PROG (TR_ULODE DRVS ULODE ALLVARF MINORD DV F X H FOUND MINORDF
           TOTALORDER)
      (SETQ DRVS (GET P 'DERIVS))
      (SETQ ULODE T)
      (SETQ ALLVARF (GET P 'ALLVARFCTS))
      (COND
       (TR_ULODE
        (PROGN (PROGN (PRIN2 "allvarf=") (PRIN2 ALLVARF) NIL) (TERPRI) NIL)))
      (SETQ MINORD 1000)
      (COND ((NOT (AND ALLVARF (CDR ALLVARF))) (SETQ ULODE NIL))
            (T
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND ULODE DRVS)) (RETURN NIL)))
               (PROGN
                (SETQ DV (CAAR DRVS))
                (SETQ F (CAR DV))
                (COND
                 (TR_ULODE
                  (PROGN
                   (PROGN
                    (PRIN2 "car drvs=")
                    (PRIN2 (CAR DRVS))
                    (PRIN2 "  dv=")
                    (PRIN2 DV)
                    (PRIN2 "  f=")
                    (PRIN2 F)
                    (PRIN2 "  member(f,allvarf)=")
                    (PRIN2 (MEMBER F ALLVARF))
                    NIL)
                   (TERPRI)
                   NIL)))
                (COND
                 ((MEMBER F ALLVARF)
                  (COND
                   ((OR (NEQ (CDAR DRVS) 1)
                        (AND (CDR DV)
                             (OR (AND X (NEQ X (CADR DV)))
                                 (AND (CDDR DV)
                                      (OR (NOT (FIXP (CADDR DV)))
                                          (CDDDR DV))))))
                    (PROGN
                     (SETQ ULODE NIL)
                     (COND
                      (TR_ULODE
                       (PROGN
                        (PROGN (PRIN2 "new ulode=") (PRIN2 ULODE) NIL)
                        (TERPRI)
                        NIL)))
                     NIL))
                   ((NULL (CDR DV))
                    (COND ((NOT (MEMBER F FOUND)) (SETQ ULODE NIL)) (T NIL)))
                   ((NULL X)
                    (PROGN
                     (COND
                      (TR_ULODE
                       (PROGN (PROGN (PRIN2 "null x") NIL) (TERPRI) NIL)))
                     (SETQ FOUND (CONS F FOUND))
                     (SETQ X (CADR DV))
                     (SETQ MINORDF (CAR DV))
                     (COND ((NULL (CDDR DV)) (SETQ MINORD 1))
                           (T (SETQ MINORD (CADDR DV))))
                     (SETQ TOTALORDER MINORD)))
                   ((NOT (MEMBER F FOUND))
                    (PROGN
                     (SETQ FOUND (CONS F FOUND))
                     (COND ((NULL (CDDR DV)) (SETQ H 1))
                           (T (SETQ H (CADDR DV))))
                     (SETQ TOTALORDER (PLUS TOTALORDER H))
                     (COND
                      ((LESSP H MINORD)
                       (PROGN (SETQ MINORD H) (SETQ MINORDF (CAR DV)))))))
                   (T NIL)))
                 ((OR (NULL X) (MEMBER X (FCTARGS F))) (SETQ ULODE NIL)))
                (COND
                 (TR_ULODE
                  (PROGN
                   (PROGN
                    (PRIN2 "found=")
                    (PRIN2 FOUND)
                    (PRIN2 "  minord=")
                    (PRIN2 MINORD)
                    (PRIN2 "  minordf=")
                    (PRIN2 MINORDF)
                    NIL)
                   (TERPRI)
                   NIL)))
                (SETQ DRVS (CDR DRVS))
                NIL)
               (GO WHILELABEL))))
      (COND
       ((AND ULODE (NULL (GET P 'LINEAR_))
             (NULL (LIN_CHECK_SQ (GET P 'SQVAL) (GET P 'ALLVARFCTS))))
        (SETQ ULODE NIL)))
      (COND
       (TR_ULODE
        (PROGN (PROGN (PRIN2 "ulode=") (PRIN2 ULODE) NIL) (TERPRI) NIL)))
      (RETURN (COND (ULODE (LIST TOTALORDER P X MINORD MINORDF)) (T NIL))))) 
(PUT 'ULPDEP_ 'NUMBER-OF-ARGS 1) 
(PUT 'ULPDEP_ 'DEFINED-ON-LINE '391) 
(PUT 'ULPDEP_ 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'ULPDEP_ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ULPDEP_ (P)
    (PROG (TR_ULPDE DRVS DRV ULPDE ALLVARF ALLVARFCOP VLD VL V PDO FN F
           NO_OF_DRVS NO_OF_TMS ORDR MAXORDR PARTI)
      (SETQ DRVS (GET P 'DERIVS))
      (SETQ ULPDE T)
      (SETQ ALLVARF (GET P 'ALLVARFCTS))
      (COND
       (TR_ULPDE
        (PROGN (PROGN (PRIN2 "allvarf=") (PRIN2 ALLVARF) NIL) (TERPRI) NIL)))
      (COND ((NOT (AND ALLVARF (CDR ALLVARF))) (SETQ ULPDE NIL))
            (T
             (PROGN
              (SETQ ALLVARFCOP ALLVARF)
              (SETQ NO_OF_TMS 0)
              (SETQ VL (GET P 'VARS))
              (PROG ()
               WHILELABEL
                (COND ((NOT (AND ULPDE ALLVARFCOP)) (RETURN NIL)))
                (PROGN
                 (SETQ PDO (GET P 'SQVAL))
                 (SETQ FN (CAR ALLVARFCOP))
                 (SETQ ALLVARFCOP (CDR ALLVARFCOP))
                 (PROG (F)
                   (SETQ F ALLVARF)
                  LAB
                   (COND ((NULL F) (RETURN NIL)))
                   ((LAMBDA (F)
                      (COND
                       ((NEQ F FN) (SETQ PDO (SUBSQ PDO (LIST (CONS F 0)))))))
                    (CAR F))
                   (SETQ F (CDR F))
                   (GO LAB))
                 (COND
                  ((NOT (LIN_CHECK_SQ PDO (LIST FN)))
                   (PROGN
                    (COND
                     (TR_ULPDE
                      (PROGN
                       (PROGN (PRIN2 "not linear in ") (PRIN2 F) NIL)
                       (TERPRI))))
                    (SETQ ULPDE NIL)))
                  (T
                   (PROGN
                    (SETQ NO_OF_TMS (PLUS NO_OF_TMS (NO_OF_TM_SF (CAR PDO))))
                    (SETQ VLD NIL)
                    (PROG (V)
                      (SETQ V VL)
                     LAB
                      (COND ((NULL V) (RETURN NIL)))
                      ((LAMBDA (V)
                         (COND ((NOT (FREEOF PDO V)) (SETQ VLD (CONS V VLD)))))
                       (CAR V))
                      (SETQ V (CDR V))
                      (GO LAB))
                    (SETQ NO_OF_DRVS 0)
                    (PROG (DRV)
                      (SETQ DRV DRVS)
                     LAB
                      (COND ((NULL DRV) (RETURN NIL)))
                      ((LAMBDA (DRV)
                         (COND
                          ((EQUAL FN (CAAR DRV))
                           (PROGN
                            (SETQ ORDR (ABSODEG (CDAR DRV)))
                            (COND
                             ((OR (EQUAL NO_OF_DRVS 0) (GREATERP ORDR MAXORDR))
                              (SETQ MAXORDR ORDR)))
                            (SETQ NO_OF_DRVS (ADD1 NO_OF_DRVS))
                            NIL))))
                       (CAR DRV))
                      (SETQ DRV (CDR DRV))
                      (GO LAB))
                    (SETQ PARTI
                            (CONS (LIST PDO FN VLD NO_OF_DRVS MAXORDR) PARTI))
                    NIL))))
                (GO WHILELABEL))
              (COND
               ((NEQ NO_OF_TMS (GET P 'TERMS))
                (PROGN
                 (COND
                  (TR_ULPDE
                   (PROGN
                    (PROGN (PRIN2 "not a lin. homog. PDE") NIL)
                    (TERPRI))))
                 (SETQ ULPDE NIL)
                 NIL)))
              (COND
               (TR_ULPDE
                (PROGN (PROGN (PRIN2 "parti=") NIL) (PRETTYPRINT PARTI) NIL)))
              NIL)))
      (RETURN (COND ((NULL ULPDE) NIL) (T PARTI))))) 
(PUT 'ULPDEP 'NUMBER-OF-ARGS 1) 
(PUT 'ULPDEP 'DEFINED-ON-LINE '465) 
(PUT 'ULPDEP 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'ULPDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ULPDEP (P)
    (PROG (TR_ULPDE DRVS DRV ULPDE PARTI PDE DIFOP1 DIFOP2 COMMU DISJN
           TOTALCOST)
      (SETQ DRVS (GET P 'DERIVS))
      (SETQ ULPDE (ULPDEP_ P))
      (COND
       (ULPDE
        (PROGN
         (SETQ PARTI ULPDE)
         (SETQ ULPDE T)
         (SETQ PDE NIL)
         (PROG (DIFOP1)
           (SETQ DIFOP1 PARTI)
          LAB
           (COND ((NULL DIFOP1) (RETURN NIL)))
           ((LAMBDA (DIFOP1)
              (PROGN
               (SETQ COMMU T)
               (PROG (DIFOP2)
                 (SETQ DIFOP2 PARTI)
                LAB
                 (COND ((NULL DIFOP2) (RETURN NIL)))
                 ((LAMBDA (DIFOP2)
                    (COND
                     ((AND (NEQ (CADR DIFOP1) (CADR DIFOP2))
                           (NOT
                            (SQZEROP
                             (ADDSQ
                              (SUBSQ (CAR DIFOP2)
                                     (LIST
                                      (CONS (CADR DIFOP2)
                                            (LIST '*SQ (CAR DIFOP1) T))))
                              (NEGSQ
                               (SUBSQ
                                (SUBSQ (CAR DIFOP1)
                                       (LIST
                                        (CONS (CADR DIFOP1)
                                              (LIST '*SQ (CAR DIFOP2) T))))
                                (LIST (CONS (CADR DIFOP2) (CADR DIFOP1)))))))))
                      (PROGN
                       (SETQ COMMU NIL)
                       (COND
                        (TR_ULPDE
                         (PROGN
                          (PROGN (PRIN2 "no commutation of:") NIL)
                          (TERPRI)
                          (PRETTYPRINT DIFOP1)
                          (PROGN (PRIN2 "and ") NIL)
                          (TERPRI)
                          (PRETTYPRINT DIFOP2))))))))
                  (CAR DIFOP2))
                 (SETQ DIFOP2 (CDR DIFOP2))
                 (GO LAB))
               (COND
                (COMMU
                 (PROGN
                  (SETQ DISJN NIL)
                  (PROG (DIFOP2)
                    (SETQ DIFOP2 PARTI)
                   LAB
                    (COND ((NULL DIFOP2) (RETURN NIL)))
                    ((LAMBDA (DIFOP2)
                       (COND
                        ((AND (NEQ (CADR DIFOP1) (CADR DIFOP2))
                              (FREEOFLIST (CADDR DIFOP1) (CADDR DIFOP2)))
                         (SETQ DISJN T))))
                     (CAR DIFOP2))
                    (SETQ DIFOP2 (CDR DIFOP2))
                    (GO LAB))
                  (COND
                   (DISJN
                    (COND ((NULL PDE) (SETQ PDE DIFOP1))
                          ((OR (LESSP (CAR (CDDDDR DIFOP1)) (CAR (CDDDDR PDE)))
                               (AND
                                (EQUAL (CAR (CDDDDR DIFOP1))
                                       (CAR (CDDDDR PDE)))
                                (LESSP (CADDDR DIFOP1) (CADDDR PDE))))
                           (SETQ PDE DIFOP1))))))))))
            (CAR DIFOP1))
           (SETQ DIFOP1 (CDR DIFOP1))
           (GO LAB))
         (COND ((NULL PDE) (SETQ ULPDE NIL))))))
      (COND
       (TR_ULPDE
        (PROGN (PROGN (PRIN2 "ulpde=") (PRIN2 ULPDE) NIL) (TERPRI) NIL)))
      (SETQ TOTALCOST 0)
      (PROG (DRV)
        (SETQ DRV DRVS)
       LAB
        (COND ((NULL DRV) (RETURN NIL)))
        ((LAMBDA (DRV) (SETQ TOTALCOST (PLUS TOTALCOST (ABSODEG (CDAR DRV)))))
         (CAR DRV))
        (SETQ DRV (CDR DRV))
        (GO LAB))
      (RETURN (COND (ULPDE (LIST TOTALCOST P PDE PARTI)) (T NIL))))) 
(PUT 'MIN_ORD_F 'NUMBER-OF-ARGS 3) 
(PUT 'MIN_ORD_F 'DEFINED-ON-LINE '525) 
(PUT 'MIN_ORD_F 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'MIN_ORD_F 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MIN_ORD_F (ODE ALLVARF VL)
    (PROG (MINORD MINORDF NEWALLVARF F H TR_ULODE)
      (SETQ MINORD 1000)
      (SETQ MINORDF NIL)
      (SETQ NEWALLVARF NIL)
      (PROG (F)
        (SETQ F ALLVARF)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ H (LD_DERIV_SEARCH ODE F VL))
            (COND
             (TR_ULODE
              (PROGN
               (PROGN
                (PRIN2 "ld_deriv_search(")
                (PRIN2 F)
                (PRIN2 ")=")
                (PRIN2 H)
                NIL)
               (TERPRI)
               NIL)))
            (COND
             ((NOT (ZEROP (CDR H)))
              (PROGN
               (SETQ NEWALLVARF (CONS F NEWALLVARF))
               (SETQ H (CAR H))
               (SETQ H (COND ((NULL H) 0) ((NULL (CDR H)) 1) (T (CADR H))))
               (COND
                ((LESSP H MINORD)
                 (PROGN (SETQ MINORD H) (SETQ MINORDF F)))))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN (LIST MINORD MINORDF NEWALLVARF)))) 
(PUT 'UNDERODE 'NUMBER-OF-ARGS 2) 
(PUT 'UNDERODE 'DEFINED-ON-LINE '550) 
(PUT 'UNDERODE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UNDERODE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNDERODE (PCHAR PDES)
    (PROG (TR_ULODE P X ALLVARF ORGALLVARF ODE VL MINORD MINORDF ADJ F H NEWF
           SOL SUBLIST RTNLIST FREEINT_BAK FREEABS_BAK)
      (SETQ P (CAR PCHAR))
      (SETQ X (CADR PCHAR))
      (SETQ MINORD (CADDR PCHAR))
      (SETQ MINORDF (CADDDR PCHAR))
      (SETQ ALLVARF (GET P 'ALLVARFCTS))
      (SETQ ORGALLVARF ALLVARF)
      (SETQ FREEINT_BAK FREEINT_)
      (SETQ FREEINT_ NIL)
      (SETQ FREEABS_BAK FREEABS_)
      (SETQ FREEABS_ NIL)
      (CP_SQ2P_VAL P)
      (SETQ ODE (GET P 'PVAL))
      (SETQ VL (GET P 'VARS))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (GREATERP MINORD 0) (GREATERP (LENGTH ALLVARF) 1)))
          (RETURN NIL)))
        (PROGN
         (COND
          (TR_ULODE
           (PROGN
            (PROGN
             (PRIN2 "x=")
             (PRIN2 X)
             (PRIN2 "  minord=")
             (PRIN2 MINORD)
             (PRIN2 "  minordf=")
             (PRIN2 MINORDF)
             (PRIN2 "  allvarf=")
             (PRIN2 ALLVARF)
             NIL)
            (TERPRI)
            NIL)))
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ ADJ (INTPDE ODE ALLVARF VL X T))
            (COND
             (TR_ULODE
              (PROGN
               (PROGN (PRIN2 "car adj = new_function = ") NIL)
               (MATHPRINT (CAR ADJ))
               (PROGN
                (PRIN2 "cadr adj = - df(new_function,")
                (PRIN2 X)
                (PRIN2 ")=")
                NIL)
               (MATHPRINT (CADR ADJ))
               (TERPRI)
               NIL)))
            (SETQ H NIL)
            (PROG (F)
              (SETQ F ALLVARF)
             LAB
              (COND ((NULL F) (RETURN NIL)))
              ((LAMBDA (F)
                 (COND ((NOT (FREEOF (CADR ADJ) F)) (SETQ H (CONS F H)))))
               (CAR F))
              (SETQ F (CDR F))
              (GO LAB))
            (COND ((NULL H) (SETQ ODE (REVAL1 (LIST 'TIMES X ODE) T))))
            NIL)
           (COND ((NOT H) (GO REPEATLABEL))))
         (SETQ MINORDF (CADR (MIN_ORD_F ODE H VL)))
         (SETQ NEWF (NEWFCT FNAME_ VL NFCT_))
         (SETQ NFCT_ (ADD1 NFCT_))
         (COND
          (TR_ULODE
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL* "eqn=") NIL 'FIRST)
             (ASSGNPRI
              (AEVAL* (LIST 'LIST (LIST 'PLUS (LIST 'DF NEWF X) (CADR ADJ))))
              NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL* "var=") NIL 'FIRST)
             (ASSGNPRI (AEVAL* (LIST 'LIST MINORDF)) NIL 'LAST)))))
         (SETQ SOL
                 (CADR
                  (SOLVEEVAL
                   (LIST (LIST 'LIST (LIST 'PLUS (LIST 'DF NEWF X) (CADR ADJ)))
                         (LIST 'LIST MINORDF)))))
         (SETQ ALLVARF (DELETE MINORDF ALLVARF))
         (SETQ ALLVARF (CONS NEWF ALLVARF))
         (COND
          (TR_ULODE
           (PROGN
            (TERPRI)
            (PROGN (PRIN2 "sol=") (PRIN2 SOL) NIL)
            (TERPRI)
            NIL)))
         (SETQ SUBLIST (CONS SOL SUBLIST))
         (SETQ ODE
                 (REVAL1
                  (NUM
                   (REVAL1
                    (LIST 'PLUS NEWF
                          (LIST 'MINUS
                                (SUBST (CADDR SOL) (CADR SOL) (CAR ADJ))))
                    T))
                  T))
         (COND
          (TR_ULODE
           (PROGN
            (ASSGNPRI (AEVAL* "ode=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* ODE) NIL 'LAST))))
         (SETQ H (MIN_ORD_F ODE ALLVARF VL))
         (SETQ MINORD (CAR H))
         (SETQ MINORDF (CADR H))
         (SETQ ALLVARF (CADDR H))
         (COND
          ((EQUAL MINORD 0)
           (SETQ SUBLIST
                   (CONS
                    (CADR
                     (SOLVEEVAL (LIST (LIST 'LIST ODE) (LIST 'LIST MINORDF))))
                    SUBLIST))))
         (COND
          (TR_ULODE
           (PROGN
            (PROGN
             (PRIN2 "allvarf=")
             (PRIN2 ALLVARF)
             (PRIN2 "  minord=")
             (PRIN2 MINORD)
             (PRIN2 "  minordf=")
             (PRIN2 MINORDF)
             NIL)
            (TERPRI)
            NIL)))
         NIL)
        (GO WHILELABEL))
      (COND ((AND (NEQ MINORD 0) (NOT (ZEROP ODE))) (SETQ RTNLIST (LIST ODE))))
      (SETQ ODE NIL)
      (COND
       (TR_ULODE
        (PROGN (PROGN (PRIN2 "rtnlist=") (PRIN2 RTNLIST) NIL) (TERPRI))))
      (COND
       (TR_ULODE
        (PROGN
         (ASSGNPRI (AEVAL "sublist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL (CONS 'LIST SUBLIST)) NIL 'LAST))))
      (PROG ()
       WHILELABEL
        (COND ((NOT SUBLIST) (RETURN NIL)))
        (PROGN
         (COND
          ((MEMBER (CADAR SUBLIST) ORGALLVARF)
           (SETQ RTNLIST
                   (CONS
                    (REVAL1
                     (NUM
                      (REVAL1
                       (LIST 'PLUS (CADAR SUBLIST)
                             (LIST 'MINUS (CADDAR SUBLIST)))
                       T))
                     T)
                    RTNLIST))))
         (SETQ SUBLIST
                 (CDR
                  (REVAL1
                   (CONS 'LIST
                         (SUBST (CADDAR SUBLIST) (CADAR SUBLIST)
                                (CDR SUBLIST)))
                   T)))
         (COND
          (TR_ULODE
           (PROGN
            (ASSGNPRI (AEVAL* "sublist=") NIL 'FIRST)
            (ASSGNPRI (AEVAL* (CONS 'LIST SUBLIST)) NIL 'LAST)))))
        (GO WHILELABEL))
      (SETQ ALLVARF (SMEMBERL ALLVARF RTNLIST))
      (COND
       (TR_ULODE
        (PROGN (PROGN (PRIN2 "allvarf=") (PRIN2 ALLVARF) NIL) (TERPRI) NIL)))
      (PROG (H)
        (SETQ H ALLVARF)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ FTEM_ (FCTINSERT H FTEM_))
            (SETQ FLIN_ (SORT_ACCORDING_TO (CONS H FLIN_) FTEM_))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (COND
       (TR_ULODE
        (PROGN
         (ASSGNPRI (AEVAL "rtnlist=") NIL 'FIRST)
         (ASSGNPRI (AEVAL (CONS 'LIST RTNLIST)) NIL 'LAST))))
      (SETQ H
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H RTNLIST)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (MKEQSQ NIL NIL H
                                     (UNION (GET P 'FCTS) ALLVARF) VL ALLFLAGS_
                                     T (LIST 0) NIL PDES))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (MKEQSQ NIL NIL H (UNION (GET P 'FCTS) ALLVARF) VL
                             ALLFLAGS_ T (LIST 0) NIL PDES))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (PRINT_ (TERPRI)))
      (SETQ FREEINT_ FREEINT_BAK)
      (SETQ FREEABS_ FREEABS_BAK)
      (RETURN H))) 
(PUT 'UNDERPDE 'NUMBER-OF-ARGS 2) 
(PUT 'UNDERPDE 'DEFINED-ON-LINE '667) 
(PUT 'UNDERPDE 'DEFINED-IN-FILE 'CRACK/CRUNDER.RED) 
(PUT 'UNDERPDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNDERPDE (PCHAR PDES)
    (PROG (TR_ULPDE LDO PARTI FN LCOND DIFOP H FL EQLIST VL)
      (SETQ LDO (CADR PCHAR))
      (SETQ PARTI (CADDR PCHAR))
      (SETQ VL (GET (CAR PCHAR) 'VARS))
      (SETQ FN (CADR LDO))
      (SETQ LCOND (LIST FN))
      (COND
       (TR_ULPDE
        (PROGN
         (PROGN (PRIN2 "ldo=") NIL)
         (PRETTYPRINT PARTI)
         (PROGN (PRIN2 "parti=") NIL)
         (PRETTYPRINT PARTI))))
      (PROG (DIFOP)
        (SETQ DIFOP PARTI)
       LAB
        (COND ((NULL DIFOP) (RETURN NIL)))
        ((LAMBDA (DIFOP)
           (COND
            ((NEQ (CADR DIFOP) FN)
             (PROGN
              (SETQ H (NEWFCT FNAME_ VL NFCT_))
              (SETQ NFCT_ (ADD1 NFCT_))
              (COND (PRINT_ (TERPRI)))
              (SETQ FL (CONS H FL))
              (SETQ EQLIST
                      (CONS
                       (CONS (LIST (CADR DIFOP) H)
                             (REVAL1
                              (LIST 'DIFFERENCE (CADR DIFOP)
                                    (SUBST H FN (PREPSQ (CAR LDO))))
                              T))
                       EQLIST))
              (SETQ LCOND
                      (CONS (SUBST H (CADR DIFOP) (PREPSQ (CAR DIFOP)))
                            LCOND))))))
         (CAR DIFOP))
        (SETQ DIFOP (CDR DIFOP))
        (GO LAB))
      (SETQ EQLIST
              (CONS
               (CONS (APPEND (GET (CAR PCHAR) 'FCTS) FL) (CONS 'PLUS LCOND))
               EQLIST))
      (COND
       (TR_ULPDE
        (PROGN (PROGN (PRIN2 "eqlist=") NIL) (PRETTYPRINT EQLIST) NIL)))
      (PROG (H)
        (SETQ H FL)
       LAB
        (COND ((NULL H) (RETURN NIL)))
        ((LAMBDA (H)
           (PROGN
            (SETQ FTEM_ (FCTINSERT H FTEM_))
            (SETQ FLIN_ (SORT_ACCORDING_TO (CONS H FLIN_) FTEM_))))
         (CAR H))
        (SETQ H (CDR H))
        (GO LAB))
      (SETQ H
              (PROG (H FORALL-RESULT FORALL-ENDPTR)
                (SETQ H EQLIST)
                (COND ((NULL H) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (H)
                                    (MKEQSQ NIL NIL (CDR H) (CAR H)
                                     (GET (CAR PCHAR) 'VARS) ALLFLAGS_ T
                                     (LIST 0) NIL PDES))
                                  (CAR H))
                                 NIL)))
               LOOPLABEL
                (SETQ H (CDR H))
                (COND ((NULL H) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (H)
                            (MKEQSQ NIL NIL (CDR H) (CAR H)
                             (GET (CAR PCHAR) 'VARS) ALLFLAGS_ T (LIST 0) NIL
                             PDES))
                          (CAR H))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (PRINT_ (TERPRI)))
      (RETURN H))) 
(ENDMODULE) 