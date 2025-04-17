(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'WU)) 
(CREATE-PACKAGE '(WU) '(CONTRIB MISC)) 
(FLUID '(*TRWU *TRCHRSTREM WUVARLIST* KORD*)) 
(SWITCH (LIST 'TRWU 'TRCHRSTREM)) 
(PUT 'WUCONSTANTP 'NUMBER-OF-ARGS 1) 
(PUT 'WUCONSTANTP 'DEFINED-ON-LINE '93) 
(PUT 'WUCONSTANTP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUCONSTANTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WUCONSTANTP (F)
    (OR (OR (ATOM F) (ATOM (CAR F))) (NOT (MEMQ (CAAAR F) WUVARLIST*)))) 
(DE WUCLASS (F) (COND ((WUCONSTANTP F) NIL) (T (CAAAR F)))) 
(PUT 'WUCLASS 'NUMBER-OF-ARGS 1) 
(PUT 'WUCLASS 'DEFINED-ON-LINE '98) 
(PUT 'WUCLASS 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUCLASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'WUCLASS 'INLINE '(LAMBDA (F) (COND ((WUCONSTANTP F) NIL) (T (CAAAR F))))) 
(DE WUDEG (F) (COND ((WUCONSTANTP F) 0) (T (CDAAR F)))) 
(PUT 'WUDEG 'NUMBER-OF-ARGS 1) 
(PUT 'WUDEG 'DEFINED-ON-LINE '101) 
(PUT 'WUDEG 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUDEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'WUDEG 'INLINE '(LAMBDA (F) (COND ((WUCONSTANTP F) 0) (T (CDAAR F))))) 
(DE WUINITIAL (F) (COND ((WUCONSTANTP F) F) (T (CDAR F)))) 
(PUT 'WUINITIAL 'NUMBER-OF-ARGS 1) 
(PUT 'WUINITIAL 'DEFINED-ON-LINE '104) 
(PUT 'WUINITIAL 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUINITIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'WUINITIAL 'INLINE '(LAMBDA (F) (COND ((WUCONSTANTP F) F) (T (CDAR F))))) 
(PUT 'WUREDUCEDPOLYSP 'NUMBER-OF-ARGS 2) 
(PUT 'WUREDUCEDPOLYSP 'DEFINED-ON-LINE '107) 
(PUT 'WUREDUCEDPOLYSP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUREDUCEDPOLYSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WUREDUCEDPOLYSP (F POLYLIST)
    (OR (NULL POLYLIST)
        (AND (WUREDUCEDP F (CAR POLYLIST)) (WUREDUCEDPOLYSP F (CDR POLYLIST))))) 
(PUT 'WUREDUCEDP 'NUMBER-OF-ARGS 2) 
(PUT 'WUREDUCEDP 'DEFINED-ON-LINE '112) 
(PUT 'WUREDUCEDP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUREDUCEDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WUREDUCEDP (G F)
    (OR (WUCONSTANTP F) (WUCONSTANTP G)
        (LESSP (DEGINVAR G (COND ((WUCONSTANTP F) NIL) (T (CAAAR F))))
               (CDAAR F)))) 
(PUT 'DEGINVAR 'NUMBER-OF-ARGS 2) 
(PUT 'DEGINVAR 'DEFINED-ON-LINE '118) 
(PUT 'DEGINVAR 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'DEGINVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGINVAR (F X)
    (COND ((WUCONSTANTP F) 0) ((EQUAL (CAAAR F) X) (CDAAR F))
          (T
           (PROG (KORD*)
             (SETQ KORD* (LIST X))
             (SETQ F (REORDER F))
             (RETURN (COND ((EQUAL (CAAAR F) X) (CDAAR F)) (T 0))))))) 
(FLUID '(WUKORD*)) 
(PUT 'SYMBOLLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'SYMBOLLESSP 'DEFINED-ON-LINE '131) 
(PUT 'SYMBOLLESSP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'SYMBOLLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMBOLLESSP (X Y)
    (COND ((NULL Y) NIL) ((NULL X) T) (WUKORD* (WUORDERP X Y))
          (T (NOT (ORDERP X Y))))) 
(PUT 'WUORDERP 'NUMBER-OF-ARGS 2) 
(PUT 'WUORDERP 'DEFINED-ON-LINE '138) 
(PUT 'WUORDERP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUORDERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WUORDERP (X Y)
    (PROG (KORD ANSW)
      (COND ((EQ X Y) (RETURN NIL)))
      (SETQ KORD WUKORD*)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND KORD (NOT ANSW))) (RETURN NIL)))
        (COND
         ((EQ X (CAR KORD))
          (SETQ ANSW (COND ((MEMQ Y (CDR KORD)) 'YES) (T 'NO))))
         ((EQ Y (CAR KORD))
          (SETQ ANSW (COND ((MEMQ X (CDR KORD)) 'NO) (T 'YES))))
         (T (SETQ KORD (CDR KORD))))
        (GO WHILELABEL))
      (RETURN (COND (ANSW (EQ ANSW 'YES)) (T (NOT (ORDERP X Y))))))) 
(DE CLASSLESSP (C1 C2) (SYMBOLLESSP C1 C2)) 
(PUT 'CLASSLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'CLASSLESSP 'DEFINED-ON-LINE '154) 
(PUT 'CLASSLESSP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'CLASSLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'CLASSLESSP 'INLINE '(LAMBDA (C1 C2) (SYMBOLLESSP C1 C2))) 
(PUT 'WULESSP 'NUMBER-OF-ARGS 2) 
(PUT 'WULESSP 'DEFINED-ON-LINE '158) 
(PUT 'WULESSP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WULESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WULESSP (F G)
    (OR
     (SYMBOLLESSP (COND ((WUCONSTANTP F) NIL) (T (CAAAR F)))
      (COND ((WUCONSTANTP G) NIL) (T (CAAAR G))))
     (AND
      (EQUAL (COND ((WUCONSTANTP F) NIL) (T (CAAAR F)))
             (COND ((WUCONSTANTP G) NIL) (T (CAAAR G))))
      (LESSP (COND ((WUCONSTANTP F) 0) (T (CDAAR F)))
             (COND ((WUCONSTANTP G) 0) (T (CDAAR G))))))) 
(PUT 'WULESSP* 'NUMBER-OF-ARGS 2) 
(PUT 'WULESSP* 'DEFINED-ON-LINE '164) 
(PUT 'WULESSP* 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WULESSP* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WULESSP* (F G)
    (COND ((WULESSP F G) T) ((WULESSP G F) NIL) (T (TOTALLESSP F G)))) 
(DE NIL2ZERO (U) (COND ((NULL U) 0) (T U))) 
(PUT 'NIL2ZERO 'NUMBER-OF-ARGS 1) 
(PUT 'NIL2ZERO 'DEFINED-ON-LINE '170) 
(PUT 'NIL2ZERO 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'NIL2ZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'NIL2ZERO 'INLINE '(LAMBDA (U) (COND ((NULL U) 0) (T U)))) 
(PUT 'TOTALLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'TOTALLESSP 'DEFINED-ON-LINE '173) 
(PUT 'TOTALLESSP 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'TOTALLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTALLESSP (F G) (EQUAL (TOTALCOMPARE F G) 'LESS)) 
(PUT 'TOTALCOMPARE 'NUMBER-OF-ARGS 2) 
(PUT 'TOTALCOMPARE 'DEFINED-ON-LINE '177) 
(PUT 'TOTALCOMPARE 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'TOTALCOMPARE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTALCOMPARE (F G)
    (COND ((EQUAL F G) 'EQUAL) ((WULESSP F G) 'LESS) ((WULESSP G F) 'GREATER)
          ((WUCONSTANTP F) (TOTALCOMPARECONSTANTS F G))
          (T
           (PROG (ANSW)
             (SETQ ANSW (TOTALCOMPARE (CDAR F) (CDAR G)))
             (COND ((NEQ ANSW 'EQUAL) (RETURN ANSW)))
             (RETURN (TOTALCOMPARE (CDR F) (CDR G))))))) 
(PUT 'TOTALCOMPARECONSTANTS 'NUMBER-OF-ARGS 2) 
(PUT 'TOTALCOMPARECONSTANTS 'DEFINED-ON-LINE '191) 
(PUT 'TOTALCOMPARECONSTANTS 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'TOTALCOMPARECONSTANTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTALCOMPARECONSTANTS (F G)
    (COND ((EQUAL F G) 'EQUAL)
          ((OR (ATOM F) (ATOM (CAR F)))
           (COND
            ((OR (ATOM G) (ATOM (CAR G)))
             (COND
              ((LESSP (COND ((NULL F) 0) (T F)) (COND ((NULL G) 0) (T G)))
               'LESS)
              (T 'GREATER)))
            (T 'LESS)))
          ((OR (ATOM G) (ATOM (CAR G))) 'GREATER)
          (T
           (PROG (WUKORD* WUVARLIST* ANSW)
             (COND ((SYMBOLLESSP (CAAAR F) (CAAAR G)) (RETURN 'LESS))
                   ((SYMBOLLESSP (CAAAR G) (CAAAR F)) (RETURN 'GREATER))
                   (T (SETQ ANSW (TOTALCOMPARECONSTANTS (CDAR F) (CDAR G)))))
             (COND ((NEQ ANSW 'EQUAL) (RETURN ANSW)))
             (RETURN (TOTALCOMPARECONSTANTS (CDR F) (CDR G))))))) 
(PUT 'WUSORT 'NUMBER-OF-ARGS 1) 
(PUT 'WUSORT 'DEFINED-ON-LINE '207) 
(PUT 'WUSORT 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUSORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WUSORT (POLYLIST) (SORT POLYLIST 'WULESSP*)) 
(PUT 'COLLECTVARS 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECTVARS 'DEFINED-ON-LINE '211) 
(PUT 'COLLECTVARS 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'COLLECTVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECTVARS (POLYLIST)
    (PROG (VARLIST)
      (SETQ VARLIST
              (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                (SETQ POLY POLYLIST)
               STARTOVER
                (COND ((NULL POLY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (POLY) (COLLECTPOLYVARS POLY)) (CAR POLY)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ POLY (CDR POLY))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (POLY) (COLLECTPOLYVARS POLY)) (CAR POLY)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ POLY (CDR POLY))
                (GO LOOPLABEL)))
      (RETURN (SORT (UNION VARLIST NIL) 'SYMBOLLESSP)))) 
(PUT 'COLLECTPOLYVARS 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECTPOLYVARS 'DEFINED-ON-LINE '218) 
(PUT 'COLLECTPOLYVARS 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'COLLECTPOLYVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECTPOLYVARS (POLY) (COLLECTPOLYVARSAUX POLY NIL)) 
(PUT 'COLLECTPOLYVARSAUX 'NUMBER-OF-ARGS 2) 
(PUT 'COLLECTPOLYVARSAUX 'DEFINED-ON-LINE '221) 
(PUT 'COLLECTPOLYVARSAUX 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'COLLECTPOLYVARSAUX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COLLECTPOLYVARSAUX (POLY SOFAR)
    (COND ((OR (ATOM POLY) (ATOM (CAR POLY))) SOFAR)
          (T
           (UNION (UNION SOFAR (LIST (CAAAR POLY)))
                  (UNION (COLLECTPOLYVARSAUX (CDAR POLY) NIL)
                         (COLLECTPOLYVARSAUX (CDR POLY) NIL)))))) 
(PUT 'PICKBASICSET 'NUMBER-OF-ARGS 1) 
(PUT 'PICKBASICSET 'DEFINED-ON-LINE '228) 
(PUT 'PICKBASICSET 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'PICKBASICSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PICKBASICSET (POLYLIST)
    (PROG (BASICSET)
      (PROG (VAR)
        (SETQ VAR WUVARLIST*)
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROGN
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND POLYLIST (SYMBOLLESSP (CAAAR (CAR POLYLIST)) VAR)))
                (RETURN NIL)))
              (SETQ POLYLIST (CDR POLYLIST))
              (GO WHILELABEL))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND POLYLIST (EQUAL VAR (CAAAR (CAR POLYLIST)))
                      (NOT (WUREDUCEDPOLYSP (CAR POLYLIST) BASICSET))))
                (RETURN NIL)))
              (SETQ POLYLIST (CDR POLYLIST))
              (GO WHILELABEL))
            (COND
             ((AND POLYLIST (EQUAL VAR (CAAAR (CAR POLYLIST))))
              (PROGN
               (SETQ BASICSET (CONS (CAR POLYLIST) BASICSET))
               (SETQ POLYLIST (CDR POLYLIST)))))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (RETURN (REVERSIP BASICSET)))) 
(PUT 'WUPSEUDODIVIDE 'NUMBER-OF-ARGS 3) 
(PUT 'WUPSEUDODIVIDE 'DEFINED-ON-LINE '245) 
(PUT 'WUPSEUDODIVIDE 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WUPSEUDODIVIDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE WUPSEUDODIVIDE (F G X)
    (PROG (ORIGF OLDKORD LCOEFF DEGF DEGG ANSW FUDGE)
      (SETQ ORIGF F)
      (SETQ OLDKORD (SETKORDER (LIST X)))
      (SETQ F (REORDER F))
      (COND
       ((OR (WUCONSTANTP F) (NEQ (CAAAR F) X))
        (PROGN (SETKORDER OLDKORD) (RETURN (CONS NIL ORIGF)))))
      (SETQ G (REORDER G))
      (COND
       ((OR (WUCONSTANTP G) (NEQ (CAAAR G) X))
        (PROGN
         (SETQ F
                 ((LAMBDA (G125)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF F G125))
                          (T (POLY-MULTF F G125))))
                  ((LAMBDA (*EXP) (QUOTF1 G (GCDF* (CDAR F) G))) T)))
         (SETKORDER OLDKORD)
         (RETURN (CONS (REORDER F) NIL)))))
      (SETQ DEGF (CDAAR F))
      (SETQ DEGG (CDAAR G))
      (COND
       ((LESSP (PLUS (DIFFERENCE DEGF DEGG) 1) 0)
        (PROGN (SETKORDER OLDKORD) (RETURN (CONS NIL ORIGF)))))
      (SETQ LCOEFF (CDAR G))
      (SETQ LCOEFF (EXPTF LCOEFF (PLUS (DIFFERENCE DEGF DEGG) 1)))
      (SETQ ANSW
              (QREMF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF LCOEFF F))
                     (T (POLY-MULTF LCOEFF F)))
               G))
      (SETQ FUDGE (GCDF* (GCDF* LCOEFF (CDR ANSW)) (CAR ANSW)))
      (SETQ ANSW
              (CONS ((LAMBDA (*EXP) (QUOTF1 (CAR ANSW) FUDGE)) T)
                    ((LAMBDA (*EXP) (QUOTF1 (CDR ANSW) FUDGE)) T)))
      (SETKORDER OLDKORD)
      (RETURN (CONS (REORDER (CAR ANSW)) (REORDER (CDR ANSW)))))) 
(PUT 'SIMPWUPSEUDODIVIDE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPWUPSEUDODIVIDE 'DEFINED-ON-LINE '277) 
(PUT 'SIMPWUPSEUDODIVIDE 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'SIMPWUPSEUDODIVIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPWUPSEUDODIVIDE (U)
    (PROG (F G X ANSW)
      (SETQ F (*Q2F (SIMP* (CAR U))))
      (SETQ G (*Q2F (SIMP* (CADR U))))
      (SETQ X (COND ((CDDR U) (*A2K (CADDR U))) (T (CAAAR F))))
      (SETQ ANSW (WUPSEUDODIVIDE F G X))
      (RETURN
       (LIST 'LIST (MK*SQ (CONS (CAR ANSW) 1)) (MK*SQ (CONS (CDR ANSW) 1)))))) 
(PUT 'WUDIV 'PSOPFN 'SIMPWUPSEUDODIVIDE) 
(PUT 'FINDREMAINDER 'NUMBER-OF-ARGS 2) 
(PUT 'FINDREMAINDER 'DEFINED-ON-LINE '289) 
(PUT 'FINDREMAINDER 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'FINDREMAINDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDREMAINDER (F POLYLIST)
    (PROGN
     (PROG (POLY)
       (SETQ POLY POLYLIST)
      LAB
       (COND ((NULL POLY) (RETURN NIL)))
       ((LAMBDA (POLY) (SETQ F (CDR (WUPSEUDODIVIDE F POLY (CAAAR POLY)))))
        (CAR POLY))
       (SETQ POLY (CDR POLY))
       (GO LAB))
     F)) 
(PUT 'PRIN2T* 'NUMBER-OF-ARGS 1) 
(PUT 'PRIN2T* 'DEFINED-ON-LINE '296) 
(PUT 'PRIN2T* 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'PRIN2T* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIN2T* (U) (PROGN (PRIN2* U) (TERPRI* T))) 
(PUT 'CHRSTREM 'NUMBER-OF-ARGS 1) 
(PUT 'CHRSTREM 'DEFINED-ON-LINE '302) 
(PUT 'CHRSTREM 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'CHRSTREM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHRSTREM (POLYLIST)
    (PROG (REVBASICSET POLS REM REMAINDERS)
      (COND
       ((OR *TRWU *TRCHRSTREM)
        (PROGN
         (TERPRI* T)
         (PRIN2T* "--------------------------------------------------------")
         NIL)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ POLYLIST (WUSORT POLYLIST))
         (COND
          ((OR *TRWU *TRCHRSTREM)
           (PROGN
            (PRIN2T* "The new pol-set in ascending order is")
            (PROG (POLY)
              (SETQ POLY POLYLIST)
             LAB
              (COND ((NULL POLY) (RETURN NIL)))
              ((LAMBDA (POLY) (PRINTSF POLY)) (CAR POLY))
              (SETQ POLY (CDR POLY))
              (GO LAB))
            (TERPRI* T)
            NIL)))
         (COND
          ((WUCONSTANTP (CAR POLYLIST))
           (PROGN
            (COND (*TRWU (PRIN2T* "which is trivially trivial")))
            (SETQ REMAINDERS 'INCONSISTENT)
            (SETQ REVBASICSET (LIST 1))
            NIL))
          (T
           (PROGN
            (SETQ REMAINDERS NIL)
            (SETQ REVBASICSET (REVERSIP (PICKBASICSET POLYLIST)))
            NIL)))
         (COND
          ((AND *TRWU (NULL REMAINDERS))
           (PROGN
            (PRIN2T* "A basic set is")
            (PROG (POLY)
              (SETQ POLY (REVERSE REVBASICSET))
             LAB
              (COND ((NULL POLY) (RETURN NIL)))
              ((LAMBDA (POLY) (PRINTSF POLY)) (CAR POLY))
              (SETQ POLY (CDR POLY))
              (GO LAB))
            (TERPRI* T)
            NIL)))
         (SETQ POLS (SETDIFF POLYLIST REVBASICSET))
         (PROG (POLY)
           (SETQ POLY POLS)
          LAB
           (COND ((NULL POLY) (RETURN NIL)))
           ((LAMBDA (POLY)
              (COND
               ((NEQ REMAINDERS 'INCONSISTENT)
                (PROGN
                 (COND
                  (*TRWU
                   (PROGN
                    (PRIN2* "The remainder of ")
                    (PRINTSF POLY)
                    (PRIN2* "wrt the basic set is "))))
                 (SETQ REM (FINDREMAINDER POLY REVBASICSET))
                 (COND (*TRWU (PROGN (PRINTSF REM) NIL)))
                 (COND
                  (REM
                   (COND
                    ((WUCONSTANTP REM)
                     (PROGN
                      (SETQ REMAINDERS 'INCONSISTENT)
                      (COND
                       (*TRWU
                        (PROGN
                         (PRIN2T "which is a non-zero constant, and so")
                         (PRIN2T "the equations are inconsistent."))))))
                    (T
                     (SETQ REMAINDERS (UNION (LIST (ABSF REM)) REMAINDERS))))))
                 NIL))))
            (CAR POLY))
           (SETQ POLY (CDR POLY))
           (GO LAB))
         (COND
          ((AND REMAINDERS (NEQ REMAINDERS 'INCONSISTENT))
           (SETQ POLYLIST (APPEND POLYLIST REMAINDERS)))))
        (COND
         ((NOT (OR (NULL REMAINDERS) (EQUAL REMAINDERS 'INCONSISTENT)))
          (GO REPEATLABEL))))
      (COND ((EQUAL REMAINDERS 'INCONSISTENT) (SETQ REVBASICSET (LIST 1))))
      (COND
       ((OR *TRWU *TRCHRSTREM)
        (PROGN
         (TERPRI* T)
         (TERPRI* T)
         (PRIN2T* "The final characteristic set is:")
         (PROG (POLY)
           (SETQ POLY (REVERSE REVBASICSET))
          LAB
           (COND ((NULL POLY) (RETURN NIL)))
           ((LAMBDA (POLY) (PRINTSF POLY)) (CAR POLY))
           (SETQ POLY (CDR POLY))
           (GO LAB)))))
      (RETURN
       (REVERSIP
        (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
          (SETQ POLY REVBASICSET)
          (COND ((NULL POLY) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS ((LAMBDA (POLY) (ABSF POLY)) (CAR POLY)) NIL)))
         LOOPLABEL
          (SETQ POLY (CDR POLY))
          (COND ((NULL POLY) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (POLY) (ABSF POLY)) (CAR POLY)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'SIMPCHRSTREM 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPCHRSTREM 'DEFINED-ON-LINE '381) 
(PUT 'SIMPCHRSTREM 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'SIMPCHRSTREM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPCHRSTREM (U)
    (PROG (ANSW POLYLIST WUVARLIST*)
      (SETQ POLYLIST
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F U)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (*Q2F (SIMP* F))) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (*Q2F (SIMP* F))) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ WUVARLIST* (COLECTVARS POLYLIST))
      (SETQ ANSW (CHRSTREM POLYLIST))
      (RETURN
       (CONS 'LIST
             (PROG (F FORALL-RESULT FORALL-ENDPTR)
               (SETQ F ANSW)
               (COND ((NULL F) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (F) (MK*SQ (CONS F 1))) (CAR F))
                                     NIL)))
              LOOPLABEL
               (SETQ F (CDR F))
               (COND ((NULL F) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (F) (MK*SQ (CONS F 1))) (CAR F)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'CHRSTREM 'PSOPFN 'SIMPCHRSTREM) 
(PUT 'WU 'NUMBER-OF-ARGS 2) 
(PUT 'WU 'DEFINED-ON-LINE '391) 
(PUT 'WU 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'WU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WU (POLYLIST VARLIST)
    (PROG (STUFFTODO ANSW POLSET CHRSET INITIALSET INITIAL WUVARLIST*)
      (SETQ STUFFTODO
              (LIST
               (DELETE NIL
                       (UNION
                        (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                          (SETQ POLY POLYLIST)
                          (COND ((NULL POLY) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (POLY) (ABSF POLY))
                                            (CAR POLY))
                                           NIL)))
                         LOOPLABEL
                          (SETQ POLY (CDR POLY))
                          (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (POLY) (ABSF POLY)) (CAR POLY))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        NIL))))
      (COND
       ((NULL (CAR STUFFTODO))
        (PROGN
         (COND (*TRWU (PRIN2T* "trivial CHS")))
         (RETURN (LIST (CONS (LIST NIL) 1)))
         NIL)))
      (COND
       ((NULL VARLIST)
        (PROGN
         (COND (*TRWU (PRIN2T* "trivial CHS")))
         (RETURN (LIST (CONS (LIST 1) 1)))
         NIL)))
      (SETQ WUVARLIST* VARLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT STUFFTODO) (RETURN NIL)))
        (PROGN
         (SETQ POLSET (WUSORT (CAR STUFFTODO)))
         (SETQ STUFFTODO (CDR STUFFTODO))
         (SETQ CHRSET (CHRSTREM POLSET))
         (COND
          ((NEQ CHRSET '(1))
           (PROGN
            (SETQ INITIALSET
                    (PROG (POL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ POL CHRSET)
                      (COND ((NULL POL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (POL)
                                          (COND ((WUCONSTANTP POL) POL)
                                                (T (CDAR POL))))
                                        (CAR POL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ POL (CDR POL))
                      (COND ((NULL POL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (POL)
                                  (COND ((WUCONSTANTP POL) POL)
                                        (T (CDAR POL))))
                                (CAR POL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ INITIAL 1)
            (PROG (POL)
              (SETQ POL INITIALSET)
             LAB
              (COND ((NULL POL) (RETURN NIL)))
              ((LAMBDA (POL)
                 (SETQ INITIAL
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF INITIAL POL))
                               (T (POLY-MULTF INITIAL POL)))))
               (CAR POL))
              (SETQ POL (CDR POL))
              (GO LAB))
            (COND
             (*TRWU (PROGN (PRIN2* "with initial ") (PRINTSF INITIAL) NIL)))
            (COND
             ((MEMBER INITIAL CHRSET)
              (PROGN
               (COND
                (*TRWU
                 (PRIN2T*
                  "which we discard, as the initial is a member of the CHS")))
               NIL))
             (T (SETQ ANSW (UNION (LIST (CONS CHRSET INITIAL)) ANSW))))
            (PROG (INITIAL)
              (SETQ INITIAL INITIALSET)
             LAB
              (COND ((NULL INITIAL) (RETURN NIL)))
              ((LAMBDA (INITIAL)
                 (COND
                  ((NOT (WUCONSTANTP INITIAL))
                   (PROGN
                    (COND
                     ((MEMBER INITIAL POLSET)
                      (PROGN
                       (PRIN2T*
                        "*** Something awry: the initial is a member of the polset")
                       (SETQ ANSW (UNION (LIST (CONS POLSET 1)) ANSW))))
                     (T
                      (SETQ STUFFTODO
                              (UNION (LIST (WUSORT (CONS INITIAL POLSET)))
                                     STUFFTODO))))))))
               (CAR INITIAL))
              (SETQ INITIAL (CDR INITIAL))
              (GO LAB))))))
        (GO WHILELABEL))
      (COND ((NULL ANSW) (SETQ ANSW (LIST (CONS (LIST 1) 1)))))
      (COND
       (*TRWU
        (PROGN
         (TERPRI* T)
         (TERPRI* T)
         (PRIN2T* "--------------------------------------------------------")
         (PRIN2T* "Final result:")
         (PROG (ZSET)
           (SETQ ZSET ANSW)
          LAB
           (COND ((NULL ZSET) (RETURN NIL)))
           ((LAMBDA (ZSET)
              (PROGN
               (PRIN2T* "Ascending set")
               (PROG (F)
                 (SETQ F (CAR ZSET))
                LAB
                 (COND ((NULL F) (RETURN NIL)))
                 ((LAMBDA (F) (PRINTSF F)) (CAR F))
                 (SETQ F (CDR F))
                 (GO LAB))
               (PRIN2* "with initial ")
               (PRINTSF (CDR ZSET))
               (TERPRI* T)))
            (CAR ZSET))
           (SETQ ZSET (CDR ZSET))
           (GO LAB)))))
      (RETURN ANSW))) 
(PUT 'SIMPWU 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPWU 'DEFINED-ON-LINE '461) 
(PUT 'SIMPWU 'DEFINED-IN-FILE 'WU/WU.RED) 
(PUT 'SIMPWU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPWU (U)
    (PROG (POLS VARS OLDKORD ANSW NARGS)
      (SETQ NARGS (LENGTH U))
      (COND
       ((OR (EQUAL NARGS 0) (GREATERP NARGS 2))
        (REDERR "Wu called with wrong number of arguments")))
      (SETQ POLS (REVAL1 (CAR U) NIL))
      (COND ((EQUAL NARGS 2) (SETQ VARS (REVAL1 (CADR U) NIL))))
      (COND
       ((OR (AND (EQUAL NARGS 1) (NOT (EQCAR POLS 'LIST)))
            (AND (EQUAL NARGS 2) (NOT (EQCAR VARS 'LIST))))
        (REDERR "Wu: syntax wu({poly, ...}) or wu({poly, ...}, {var, ...})")))
      (SETQ OLDKORD KORD*)
      (COND
       ((EQUAL NARGS 1)
        (PROG (KORD* POLSET VARS)
          (SETQ KORD* (COND (WUKORD* (REVERSE WUKORD*)) (T OLDKORD)))
          (SETQ POLSET
                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                    (SETQ F (CDR POLS))
                    (COND ((NULL F) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (F) (REORDER (*Q2F (SIMP* F))))
                                      (CAR F))
                                     NIL)))
                   LOOPLABEL
                    (SETQ F (CDR F))
                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (F) (REORDER (*Q2F (SIMP* F)))) (CAR F))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ VARS (COLLECTVARS POLSET))
          (COND
           (*TRWU
            (PROGN
             (TERPRI* T)
             (PRIN2* "Wu variables in decreasing order: ")
             (PROG (ID)
               (SETQ ID (REVERSE VARS))
              LAB
               (COND ((NULL ID) (RETURN NIL)))
               ((LAMBDA (ID) (PROGN (PRIN2* ID) (PRIN2* " "))) (CAR ID))
               (SETQ ID (CDR ID))
               (GO LAB))
             (TERPRI* T))))
          (SETQ ANSW (WU POLSET VARS))))
       (T
        (PROG (KORD* POLSET WUKORD*)
          (SETQ KORD*
                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                    (SETQ K (CDR VARS))
                    (COND ((NULL K) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (K) (*A2K K)) (CAR K))
                                          NIL)))
                   LOOPLABEL
                    (SETQ K (CDR K))
                    (COND ((NULL K) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (K) (*A2K K)) (CAR K)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ WUKORD* (REVERSE KORD*))
          (SETQ POLSET
                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                    (SETQ F (CDR POLS))
                    (COND ((NULL F) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (F) (REORDER (*Q2F (SIMP* F))))
                                      (CAR F))
                                     NIL)))
                   LOOPLABEL
                    (SETQ F (CDR F))
                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (F) (REORDER (*Q2F (SIMP* F)))) (CAR F))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ ANSW (WU POLSET WUKORD*)))))
      (RETURN
       (CONS 'LIST
             (PROG (ZSET FORALL-RESULT FORALL-ENDPTR)
               (SETQ ZSET ANSW)
               (COND ((NULL ZSET) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ZSET)
                                   (CONS 'LIST
                                         (LIST
                                          (CONS 'LIST
                                                (PROG (F FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ F (CAR ZSET))
                                                  (COND
                                                   ((NULL F) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (F)
                                                                      (MK*SQ
                                                                       (CONS
                                                                        (ABSF
                                                                         (REORDER
                                                                          F))
                                                                        1)))
                                                                    (CAR F))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ F (CDR F))
                                                  (COND
                                                   ((NULL F)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (F)
                                                              (MK*SQ
                                                               (CONS
                                                                (ABSF
                                                                 (REORDER F))
                                                                1)))
                                                            (CAR F))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL)))
                                          (MK*SQ
                                           (CONS (ABSF (REORDER (CDR ZSET)))
                                                 1)))))
                                 (CAR ZSET))
                                NIL)))
              LOOPLABEL
               (SETQ ZSET (CDR ZSET))
               (COND ((NULL ZSET) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (ZSET)
                           (CONS 'LIST
                                 (LIST
                                  (CONS 'LIST
                                        (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ F (CAR ZSET))
                                          (COND ((NULL F) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (F)
                                                              (MK*SQ
                                                               (CONS
                                                                (ABSF
                                                                 (REORDER F))
                                                                1)))
                                                            (CAR F))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ F (CDR F))
                                          (COND
                                           ((NULL F) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (F)
                                                      (MK*SQ
                                                       (CONS (ABSF (REORDER F))
                                                             1)))
                                                    (CAR F))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                  (MK*SQ
                                   (CONS (ABSF (REORDER (CDR ZSET))) 1)))))
                         (CAR ZSET))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'WU 'PSOPFN 'SIMPWU) 
(REMPROP 'WU 'NUMBER-OF-ARGS) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(ENDMODULE) 