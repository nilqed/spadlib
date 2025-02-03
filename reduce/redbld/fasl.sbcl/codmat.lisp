(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODMAT)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL
 '(CODMAT MAXVAR ROWMIN ROWMAX ENDMAT CODHISTO HEADHISTO *VECTORC *INPUTC KNOWN
   RHSALIASES)) 
(FLUID '(PREPREFIXLIST PREFIXLIST)) 
(SWITCH (LIST 'VECTORC)) 
(SETQ *VECTORC NIL) 
(PUT 'SETROW 'NUMBER-OF-ARGS 5) 
(PUT 'SETROW 'DEFINED-ON-LINE '119) 
(PUT 'SETROW 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'SETROW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETROW (N OP FA S ZZ)
    (PROG (CODMAT1)
      (COND
       ((GREATERP (ABS N) MAXVAR)
        (PROGN
         (SETQ CODMAT1 (MKVECT (TIMES 4 MAXVAR)))
         (PROG (X)
           (SETQ X (MAX ROWMIN (MINUS MAXVAR)))
          LAB
           (COND ((MINUSP (DIFFERENCE (MIN ROWMAX MAXVAR) X)) (RETURN NIL)))
           (PUTV CODMAT1 (PLUS X (TIMES 2 MAXVAR))
                 (GETV CODMAT (PLUS MAXVAR X)))
           (SETQ X (PLUS2 X 1))
           (GO LAB))
         (SETQ CODMAT CODMAT1)
         (SETQ MAXVAR (TIMES 2 MAXVAR))
         NIL)))
      (COND ((LESSP N 0) (PUTV CODMAT (PLUS MAXVAR N) (MKVECT 4)))
            (T
             (PROGN
              (PUTV CODMAT (PLUS MAXVAR N) (MKVECT 8))
              (PUTV (GETV CODMAT (PLUS MAXVAR N)) 5 (CAR S))
              (COND ((CDR S) (PUTV (GETV CODMAT (PLUS MAXVAR N)) 6 (CADR S)))
                    (T (PUTV (GETV CODMAT (PLUS MAXVAR N)) 6 1))))))
      (PUTV (GETV CODMAT (PLUS MAXVAR N)) 0 T)
      (PUTV (GETV CODMAT (PLUS MAXVAR N)) 2 OP)
      (PUTV (GETV CODMAT (PLUS MAXVAR N)) 3 FA)
      (PUTV (GETV CODMAT (PLUS MAXVAR N)) 4 ZZ))) 
(PUT 'INSZZZ 'NUMBER-OF-ARGS 2) 
(PUT 'INSZZZ 'DEFINED-ON-LINE '168) 
(PUT 'INSZZZ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INSZZZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSZZZ (Z ZZ)
    (COND ((OR (NULL ZZ) (LESSP (CAR (CAR ZZ)) (CAR Z))) (CONS Z ZZ))
          ((EQUAL (CAR (CAR ZZ)) (CAR Z))
           (PROGN
            (RPLACA (CDR (CAR ZZ))
                    (DM-PLUS (CAR (CDR (CAR ZZ))) (CAR (CDR Z))))
            (COND ((ZEROPP (CAR (CDR (CAR ZZ)))) (CDR ZZ)) (T ZZ))))
          (T (CONS (CAR ZZ) (INSZZZ Z (CDR ZZ)))))) 
(PUT 'INSZZZN 'NUMBER-OF-ARGS 2) 
(PUT 'INSZZZN 'DEFINED-ON-LINE '186) 
(PUT 'INSZZZN 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INSZZZN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSZZZN (Z ZZ)
    (COND ((OR (NULL ZZ) (LESSP (CAR (CAR ZZ)) (CAR Z))) (CONS Z ZZ))
          ((EQUAL (CAR (CAR ZZ)) (CAR Z)) ZZ)
          (T (CONS (CAR ZZ) (INSZZZN Z (CDR ZZ)))))) 
(PUT 'INSZZZR 'NUMBER-OF-ARGS 2) 
(PUT 'INSZZZR 'DEFINED-ON-LINE '198) 
(PUT 'INSZZZR 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INSZZZR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INSZZZR (Z ZZ)
    (COND ((OR (NULL ZZ) (GREATERP (CAR (CAR ZZ)) (CAR Z))) (CONS Z ZZ))
          ((EQUAL (CAR (CAR ZZ)) (CAR Z))
           (PROGN
            (RPLACA (CDR (CAR ZZ))
                    (DM-PLUS (CAR (CDR (CAR ZZ))) (CAR (CDR Z))))
            (COND ((ZEROPP (CAR (CDR (CAR ZZ)))) (CDR ZZ)) (T ZZ))))
          (T (CONS (CAR ZZ) (INSZZZR Z (CDR ZZ)))))) 
(PUT 'PNTHXZZ 'NUMBER-OF-ARGS 2) 
(PUT 'PNTHXZZ 'DEFINED-ON-LINE '215) 
(PUT 'PNTHXZZ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'PNTHXZZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PNTHXZZ (X ZZ)
    (COND ((OR (NULL ZZ) (EQUAL (CAR (CAR ZZ)) X)) ZZ)
          (T (PNTHXZZ X (CDR ZZ))))) 
(PUT 'INSHISTO 'NUMBER-OF-ARGS 1) 
(PUT 'INSHISTO 'DEFINED-ON-LINE '224) 
(PUT 'INSHISTO 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INSHISTO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INSHISTO (X)
    (COND
     ((AND (GETV (GETV CODMAT (PLUS MAXVAR X)) 0) (GEQ X 0))
      (PROG (Y HV)
        (COND
         ((SETQ Y
                  (GETV CODHISTO
                        (SETQ HV
                                (MIN
                                 (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1))
                                 200))))
          (RPLACA (GETV (GETV CODMAT (PLUS MAXVAR Y)) 7) X))
         ((GREATERP HV HEADHISTO) (SETQ HEADHISTO HV)))
        (PUTV (GETV CODMAT (PLUS MAXVAR X)) 7 (CONS NIL Y))
        (PUTV CODHISTO HV X))))) 
(PUT 'DELHISTO 'NUMBER-OF-ARGS 1) 
(PUT 'DELHISTO 'DEFINED-ON-LINE '269) 
(PUT 'DELHISTO 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'DELHISTO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELHISTO (X)
    (COND
     ((AND (GETV (GETV CODMAT (PLUS MAXVAR X)) 0) (GEQ X 0))
      (PROG (Y Z HV)
        (SETQ Y (CAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 7)))
        (SETQ Z (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 7)))
        (SETQ HV (MIN (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 200))
        (COND (Y (RPLACD (GETV (GETV CODMAT (PLUS MAXVAR Y)) 7) Z))
              (T (PUTV CODHISTO HV Z)))
        (COND (Z (RPLACA (GETV (GETV CODMAT (PLUS MAXVAR Z)) 7) Y))))))) 
(PUT 'ROWDEL 'NUMBER-OF-ARGS 1) 
(PUT 'ROWDEL 'DEFINED-ON-LINE '284) 
(PUT 'ROWDEL 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'ROWDEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROWDEL (X)
    (PROGN
     (DELHISTO X)
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL)
     (PROG (Z)
       (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
      LAB
       (COND ((NULL Z) (RETURN NIL)))
       ((LAMBDA (Z) (DOWNWGHT (CAR Z) (CAR (CDR Z)))) (CAR Z))
       (SETQ Z (CDR Z))
       (GO LAB)))) 
(PUT 'ROWINS 'NUMBER-OF-ARGS 1) 
(PUT 'ROWINS 'DEFINED-ON-LINE '297) 
(PUT 'ROWINS 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'ROWINS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROWINS (X)
    (PROGN
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)
     (INSHISTO X)
     (PROG (Z)
       (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
      LAB
       (COND ((NULL Z) (RETURN NIL)))
       ((LAMBDA (Z) (UPWGHT (CAR Z) (CAR (CDR Z)))) (CAR Z))
       (SETQ Z (CDR Z))
       (GO LAB)))) 
(PUT 'DOWNWGHT 'NUMBER-OF-ARGS 2) 
(PUT 'DOWNWGHT 'DEFINED-ON-LINE '307) 
(PUT 'DOWNWGHT 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'DOWNWGHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOWNWGHT (X IV) (PROGN (DELHISTO X) (DOWNWGHT1 X IV) (INSHISTO X))) 
(PUT 'DOWNWGHT1 'NUMBER-OF-ARGS 2) 
(PUT 'DOWNWGHT1 'DEFINED-ON-LINE '317) 
(PUT 'DOWNWGHT1 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'DOWNWGHT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOWNWGHT1 (X IV)
    (COND
     ((NOT (|:ONEP| (DM-ABS IV)))
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1
            (CONS
             (CONS (DIFFERENCE (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)
                   (DIFFERENCE (CDAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1))
                               1))
             (DIFFERENCE (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 4))))
     (T
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1
            (CONS
             (CONS (DIFFERENCE (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)
                   (CDAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)))
             (DIFFERENCE (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)))))) 
(PUT 'UPWGHT 'NUMBER-OF-ARGS 2) 
(PUT 'UPWGHT 'DEFINED-ON-LINE '326) 
(PUT 'UPWGHT 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'UPWGHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPWGHT (X IV) (PROGN (DELHISTO X) (UPWGHT1 X IV) (INSHISTO X))) 
(PUT 'UPWGHT1 'NUMBER-OF-ARGS 2) 
(PUT 'UPWGHT1 'DEFINED-ON-LINE '336) 
(PUT 'UPWGHT1 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'UPWGHT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPWGHT1 (X IV)
    (COND
     ((NOT (|:ONEP| (DM-ABS IV)))
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1
            (CONS
             (CONS (PLUS (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)
                   (PLUS (CDAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1))
             (MIN (PLUS (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 4) 200))))
     (T
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1
            (CONS
             (CONS (PLUS (CAAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)
                   (CDAR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)))
             (MIN (PLUS (CDR (GETV (GETV CODMAT (PLUS MAXVAR X)) 1)) 1)
                  200)))))) 
(PUT 'INITWGHT 'NUMBER-OF-ARGS 1) 
(PUT 'INITWGHT 'DEFINED-ON-LINE '344) 
(PUT 'INITWGHT 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INITWGHT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INITWGHT (X)
    (PROG (AN MN)
      (SETQ AN (SETQ MN 0))
      (PROG (Z)
        (SETQ Z (GETV (GETV CODMAT (PLUS MAXVAR X)) 4))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((GETV (GETV CODMAT (PLUS MAXVAR (CAR Z))) 0)
             (PROGN
              (COND
               ((NOT (|:ONEP| (DM-ABS (CAR (CDR Z))))) (SETQ MN (PLUS MN 1))))
              (SETQ AN (PLUS AN 1))))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1
            (CONS (CONS AN MN) (PLUS AN (TIMES 3 MN)))))) 
(PUT 'REMZZZZ 'NUMBER-OF-ARGS 2) 
(PUT 'REMZZZZ 'DEFINED-ON-LINE '359) 
(PUT 'REMZZZZ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'REMZZZZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMZZZZ (ZZ1 ZZ2)
    (COND ((NULL ZZ1) ZZ2)
          ((EQUAL (CAR (CAR ZZ1)) (CAR (CAR ZZ2)))
           (REMZZZZ (CDR ZZ1) (CDR ZZ2)))
          (T (CONS (CAR ZZ2) (REMZZZZ ZZ1 (CDR ZZ2)))))) 
(PUT 'CHDEL 'NUMBER-OF-ARGS 2) 
(PUT 'CHDEL 'DEFINED-ON-LINE '371) 
(PUT 'CHDEL 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'CHDEL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHDEL (FA X)
    (PUTV (GETV CODMAT (PLUS MAXVAR FA)) 5
          (DELETE X (GETV (GETV CODMAT (PLUS MAXVAR FA)) 5)))) 
(PUT 'DELYZZ 'NUMBER-OF-ARGS 2) 
(PUT 'DELYZZ 'DEFINED-ON-LINE '378) 
(PUT 'DELYZZ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'DELYZZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DELYZZ (Y ZZ)
    (COND ((EQUAL Y (CAR (CAR ZZ))) (CDR ZZ))
          (T (CONS (CAR ZZ) (DELYZZ Y (CDR ZZ)))))) 
(PUT 'CLEARROW 'NUMBER-OF-ARGS 1) 
(PUT 'CLEARROW 'DEFINED-ON-LINE '387) 
(PUT 'CLEARROW 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'CLEARROW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEARROW (X)
    (PROGN
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 4 NIL)
     (COND
      ((GEQ X 0)
       (PROGN
        (PUTV (GETV CODMAT (PLUS MAXVAR X)) 5 NIL)
        (COND
         ((NOT (NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3)))
          (REMPROP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) 'ROWINDEX))))))
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 1 NIL)
     (PUTV (GETV CODMAT (PLUS MAXVAR X)) 3 (MINUS 1)))) 
(GLOBAL '(VARLST+ VARLST* KVARLST PREVLST CODBEXL*)) 
(FLUID '(PREPREFIXLIST PREFIXLIST)) 
(SETQ VARLST+ (SETQ VARLST* (SETQ KVARLST NIL))) 
(PUT 'FFVAR! 'NUMBER-OF-ARGS 3) 
(PUT 'FFVAR! 'DEFINED-ON-LINE '574) 
(PUT 'FFVAR! 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FFVAR! 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FFVAR! (NEX EX PREFIXLIST)
    (PROG (N NNEX ARGTYPE VAR S)
      (SETQ PREFIXLIST (CONS (CONS NEX EX) PREFIXLIST))
      (SETQ N (SETQ ROWMAX (PLUS ROWMAX 1)))
      (SETQ CODBEXL* (CONS N CODBEXL*))
      (COND ((FLAGP NEX 'NEWSYM) (PUT NEX 'ROWINDEX N)))
      (PUT NEX 'ROWOCC (LIST N))
      (FFVAR!2 N NEX (REMDIFF EX))
      (RETURN PREFIXLIST))) 
(PUT 'RESTORECSEINFO 'NUMBER-OF-ARGS 2) 
(PUT 'RESTORECSEINFO 'DEFINED-ON-LINE '594) 
(PUT 'RESTORECSEINFO 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'RESTORECSEINFO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RESTORECSEINFO (NEX EX)
    (PROG (INB NB S)
      (COND
       ((EQ NEX 'CSES)
        (COND ((ATOM EX) (FLAG (LIST EX) 'NEWSYM))
              (T
               (PROG (EL)
                 (SETQ EL (CDR EX))
                LAB
                 (COND ((NULL EL) (RETURN NIL)))
                 ((LAMBDA (EL) (FLAG (LIST EL) 'NEWSYM)) (CAR EL))
                 (SETQ EL (CDR EL))
                 (GO LAB)))))
       ((EQ (LETTERPART EX) 'G)
        (COND ((EQ (SETQ S (LETTERPART (FNEWSYM))) 'G) (INAME S))
              (T
               (PROGN
                (SETQ NB (DIGITPART EX))
                (SETQ INB (DIGITPART (FNEWSYM)))
                (PROG (J)
                  (SETQ J INB)
                 LAB
                  (COND ((MINUSP (DIFFERENCE NB J)) (RETURN NIL)))
                  (GENSYM)
                  (SETQ J (PLUS2 J 1))
                  (GO LAB))))))
       ((AND (EQ (LETTERPART EX) (LETTERPART (SETQ S (FNEWSYM))))
             (GREATERP (DIGITPART EX) (DIGITPART S)))
        (INAME EX))
       (T (INAME S))))) 
(PUT 'REMDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'REMDIFF 'DEFINED-ON-LINE '628) 
(PUT 'REMDIFF 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'REMDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMDIFF (F)
    (COND ((OR (IDP F) (CONSTP F)) F)
          (T
           (PROGN
            (COND
             ((EQ (CAR F) 'DIFFERENCE)
              (SETQ F
                      (LIST 'PLUS (REMDIFF (CADR F))
                            (LIST 'MINUS (REMDIFF (CADDR F))))))
             (T
              (CONS (CAR F)
                    (PROG (OP FORALL-RESULT FORALL-ENDPTR)
                      (SETQ OP (CDR F))
                      (COND ((NULL OP) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (OP) (REMDIFF OP)) (CAR OP))
                                       NIL)))
                     LOOPLABEL
                      (SETQ OP (CDR OP))
                      (COND ((NULL OP) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (OP) (REMDIFF OP)) (CAR OP)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))))) 
(PUT 'FFVAR!2 'NUMBER-OF-ARGS 3) 
(PUT 'FFVAR!2 'DEFINED-ON-LINE '640) 
(PUT 'FFVAR!2 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FFVAR!2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FFVAR!2 (N NEX EX)
    (COND
     ((AND (EQCAR EX 'TIMES) (NOT (FCOFTRM EX)))
      (SETROW N 'TIMES NEX (FFVAR* (CDR EX) N) NIL))
     ((AND (EQCAR EX 'EXPT) (OR (INTEGERP (CADDR EX)) (RATIONALEXPONENT EX)))
      (SETROW N 'TIMES NEX (FFVAR* (LIST EX) N) NIL))
     (T (SETROW N 'PLUS NEX (FFVAR+ (LIST EX) N) NIL)))) 
(PUT 'FCOFTRM 'NUMBER-OF-ARGS 1) 
(PUT 'FCOFTRM 'DEFINED-ON-LINE '651) 
(PUT 'FCOFTRM 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FCOFTRM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCOFTRM (F)
    (AND (AND (NULL (CDDDR F)) (CDDR F)) (CONSTP (CADR F))
         (NOT
          (AND (PAIRP (CADDR F))
               (MEMQ (CAADDR F) '(EXPT TIMES PLUS DIFFERENCE MINUS)))))) 
(PUT 'RATIONALEXPONENT 'NUMBER-OF-ARGS 1) 
(PUT 'RATIONALEXPONENT 'DEFINED-ON-LINE '661) 
(PUT 'RATIONALEXPONENT 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'RATIONALEXPONENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATIONALEXPONENT (F) (RATIONALP (CADDR F))) 
(PUT 'RATIONALP 'NUMBER-OF-ARGS 1) 
(PUT 'RATIONALP 'DEFINED-ON-LINE '670) 
(PUT 'RATIONALP 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'RATIONALP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATIONALP (F)
    (AND (EQCAR F 'QUOTIENT) (INTEGERP (CADR F)) (INTEGERP (CADDR F)))) 
(PUT 'FFVAR+ 'NUMBER-OF-ARGS 2) 
(PUT 'FFVAR+ 'DEFINED-ON-LINE '673) 
(PUT 'FFVAR+ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FFVAR+ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FFVAR+ (F RI)
    (PROG (CH N S B S1 NN)
      (PROG (TRM)
        (SETQ TRM F)
       LAB
        (COND ((NULL TRM) (RETURN NIL)))
        ((LAMBDA (TRM)
           (PROGN
            (SETQ B (SETQ S NIL))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND (PAIRP TRM) (EQ (SETQ S (CAR TRM)) 'MINUS)))
                (RETURN NIL)))
              (PROGN (SETQ TRM (CADR TRM)) (SETQ B (NOT B)))
              (GO WHILELABEL))
            (COND
             ((EQ S 'DIFFERENCE)
              (PROGN
               (SETQ TRM (LIST 'PLUS (CADR TRM) (LIST 'MINUS (CADDR TRM))))
               (SETQ S 'PLUS))))
            (COND
             ((EQ S 'PLUS)
              (PROGN
               (SETQ S1
                       (FFVAR+
                        (COND
                         (B
                          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EL (CDR TRM))
                            (COND ((NULL EL) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL) (LIST 'MINUS EL))
                                              (CAR EL))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EL (CDR EL))
                            (COND ((NULL EL) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (LIST 'MINUS EL)) (CAR EL))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                         (T (CDR TRM)))
                        RI))
               (SETQ CH (APPEND CH (CAR S1)))))
             ((EQ S 'TIMES)
              (PROGN
               (COND
                ((AND (PAIRP (CADDR TRM)) (EQ (CAADDR TRM) 'SQRT)
                      (NULL (CDDDR TRM)))
                 (SETQ TRM
                         (LIST 'TIMES (CADR TRM)
                               (LIST 'EXPT (CADR (CADDR TRM))
                                     (LIST 'QUOTIENT 1 2))))))
               (COND
                ((FCOFTRM TRM)
                 (PVARLST+ (CADDR TRM) RI
                  (COND (B (DM-MINUS (CADR TRM))) (T (CADR TRM)))))
                (T
                 (PROGN
                  (SETQ N (SETQ ROWMAX (PLUS ROWMAX 1)))
                  (SETQ S1 (FFVAR* (CDR TRM) N))
                  (COND
                   (B
                    (SETROW N 'TIMES RI (LIST (CAR S1) (DM-MINUS (CADR S1)))
                     NIL))
                   (T (SETROW N 'TIMES RI S1 NIL)))
                  (SETQ CH (CONS N CH)))))))
             (T
              (PROGN
               (COND
                ((EQ S 'SQRT)
                 (PROGN
                  (SETQ TRM
                          (CONS 'EXPT
                                (CONS (CADR TRM) (LIST (LIST 'QUOTIENT 1 2)))))
                  (SETQ S 'EXPT))))
               (COND
                ((AND (EQ S 'EXPT) (EQCAR (CADDR TRM) 'MINUS)
                      (OR (INTEGERP (CADR (CADDR TRM)))
                          (RATIONALP (CADR (CADDR TRM)))))
                 (PROGN
                  (SETQ TRM
                          (LIST 'QUOTIENT 1
                                (LIST 'EXPT (CADR TRM) (CADR (CADDR TRM)))))
                  (SETQ S 'QUOTIENT))))
               (COND
                ((AND (EQ S 'EXPT)
                      (OR (INTEGERP (CADDR TRM)) (RATIONALEXPONENT TRM)))
                 (PROGN
                  (SETQ N (SETQ ROWMAX (PLUS ROWMAX 1)))
                  (SETQ S1 (FFVAR* (LIST TRM) N))
                  (COND (B (SETROW N 'TIMES RI (LIST (CAR S1) (MINUS 1)) NIL))
                        (T (SETROW N 'TIMES RI S1 NIL)))
                  (SETQ CH (CONS N CH))))
                (T (PVARLST+ TRM RI (COND (B (MINUS 1)) (T 1))))))))
            NIL))
         (CAR TRM))
        (SETQ TRM (CDR TRM))
        (GO LAB))
      (RETURN (LIST CH)))) 
(PUT 'PVARLST+ 'NUMBER-OF-ARGS 3) 
(PUT 'PVARLST+ 'DEFINED-ON-LINE '774) 
(PUT 'PVARLST+ 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'PVARLST+ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PVARLST+ (VAR X IV)
    (PROG (L S NVAR)
      (COND
       ((CONSTP VAR) (PROGN (SETQ IV (DM-TIMES IV VAR)) (SETQ VAR '+ONE))))
      (COND ((NOT (OR (IDP VAR) (CONSTP VAR))) (SETQ VAR (FVAROP VAR X))))
      (COND
       ((NULL (SETQ S (GET VAR 'VARLST+))) (SETQ VARLST+ (CONS VAR VARLST+))))
      (PUT VAR 'VARLST+ (CONS (CONS X IV) S)))) 
(PUT 'FFVAR* 'NUMBER-OF-ARGS 2) 
(PUT 'FFVAR* 'DEFINED-ON-LINE '799) 
(PUT 'FFVAR* 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FFVAR* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FFVAR* (F RI)
    (PROG (COF CH N S B ROWNR PR NR DM)
      (SETQ COF 1)
      (PROG (FAC)
        (SETQ FAC F)
       LAB
        (COND ((NULL FAC) (RETURN NIL)))
        ((LAMBDA (FAC)
           (COND ((CONSTP FAC) (SETQ COF (DM-TIMES FAC COF)))
                 ((ATOM FAC) (PVARLST* FAC RI 1))
                 ((EQ (SETQ S (CAR FAC)) 'TIMES)
                  (PROGN
                   (SETQ S (FFVAR* (CDR FAC) RI))
                   (SETQ CH (APPEND CH (CAR S)))
                   (SETQ COF (DM-TIMES COF (CADR S)))))
                 ((MEMQ S '(PLUS DIFFERENCE MINUS))
                  (PROGN
                   (COND
                    ((AND (EQ S 'MINUS) (CONSTP (CADR FAC)) (NULL (CDDR FAC)))
                     (SETQ COF (DM-MINUS (DM-TIMES COF (CADR FAC)))))
                    (T
                     (PROGN
                      (SETQ N (SETQ ROWMAX (PLUS ROWMAX 1)))
                      (COND ((NOT B) (PROGN (SETQ B T) (SETQ ROWNR N))))
                      (SETROW N 'PLUS RI (FFVAR+ (LIST FAC) N) NIL)
                      (SETQ CH (CONS N CH)))))))
                 (T
                  (PROGN
                   (COND
                    ((EQ S 'SQRT)
                     (PROGN
                      (SETQ FAC
                              (CONS 'EXPT
                                    (CONS (CADR FAC)
                                          (LIST (LIST 'QUOTIENT 1 2)))))
                      (SETQ S 'EXPT))))
                   (COND
                    ((AND (EQ S 'EXPT) (EQCAR (CADDR FAC) 'MINUS)
                          (OR (INTEGERP (CADR (CADDR FAC)))
                              (RATIONALP (CADR (CADDR FAC)))))
                     (PROGN
                      (SETQ FAC
                              (LIST 'QUOTIENT 1
                                    (LIST 'EXPT (CADR FAC)
                                          (CADR (CADDR FAC)))))
                      (SETQ S 'QUOTIENT))))
                   (COND
                    ((AND (EQ S 'EXPT)
                          (OR (INTEGERP (CADDR FAC))
                              (SETQ NR (RATIONALEXPONENT FAC))))
                     (COND
                      ((AND (PAIRP (CADR FAC)) (EQ (CAADR FAC) 'SQRT))
                       (PROGN
                        (COND
                         (NR
                          (PROGN
                           (SETQ NR (CADR (CADDR FAC)))
                           (SETQ DM (TIMES 2 (CADDR (CADDR FAC))))))
                         (T (PROGN (SETQ NR 1) (SETQ DM 2))))
                        (PVARLST* (CADR (CADR FAC)) RI (CONS NR DM))))
                      (T
                       (PVARLST* (CADR FAC) RI
                        (COND ((INTEGERP (CADDR FAC)) (CADDR FAC))
                              (T
                               (CONS (CADR (CADDR FAC))
                                     (CADDR (CADDR FAC)))))))))
                    (T (PVARLST* FAC RI 1)))))))
         (CAR FAC))
        (SETQ FAC (CDR FAC))
        (GO LAB))
      (COND
       ((AND B (NOT (|:ONEP| (DM-ABS COF))))
        (PROGN
         (PROG (EL)
           (SETQ EL (GETV (GETV CODMAT (PLUS MAXVAR ROWNR)) 5))
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (PUTV (GETV CODMAT (PLUS MAXVAR EL)) 6
                    (DM-TIMES COF (GETV (GETV CODMAT (PLUS MAXVAR EL)) 6))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         (PROG (VAR)
           (SETQ VAR VARLST+)
          LAB
           (COND ((NULL VAR) (RETURN NIL)))
           ((LAMBDA (VAR)
              (COND
               ((SETQ PR (ASSOC ROWNR (GET VAR 'VARLST+)))
                (RPLACD PR (DM-TIMES (CDR PR) COF)))))
            (CAR VAR))
           (SETQ VAR (CDR VAR))
           (GO LAB))
         (SETQ COF 1)
         NIL)))
      (RETURN (LIST CH COF)))) 
(PUT 'PVARLST* 'NUMBER-OF-ARGS 3) 
(PUT 'PVARLST* 'DEFINED-ON-LINE '907) 
(PUT 'PVARLST* 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'PVARLST* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PVARLST* (VAR X IV)
    (PROG (L S BVAR BVAL)
      (COND
       ((CONSTP VAR)
        (PROGN
         (SETQ VAR
                 (FVAROP
                  (COND ((EQUAL IV '(1 . 2)) (LIST 'SQRT VAR))
                        (T
                         (LIST 'EXPT VAR
                               (COND
                                ((PAIRP IV) (LIST 'QUOTIENT (CAR IV) (CDR IV)))
                                (T IV)))))
                  X))
         (SETQ IV 1))))
      (COND
       ((NOT (OR (ATOM VAR) (CONSTP VAR)))
        (PROGN
         (SETQ S (GET '*BASES* 'KVARLST))
         (COND (S (SETQ BVAR (ASSOC (SETQ BVAL (REVAL1 VAR T)) S))))
         (COND (BVAR (SETQ VAR (CDR BVAR)))
               (T
                (PROGN
                 (SETQ VAR (FVAROP VAR X))
                 (PUT '*BASES* 'KVARLST (CONS (CONS BVAL VAR) S))))))))
      (COND
       ((NULL (SETQ S (GET VAR 'VARLST*))) (SETQ VARLST* (CONS VAR VARLST*))))
      (COND ((AND (PAIRP IV) (NOT (CONSTP IV))) (FLAG (LIST VAR) 'RATEXP)))
      (PUT VAR 'VARLST* (CONS (CONS X IV) S)))) 
(PUT 'FVAROP 'NUMBER-OF-ARGS 2) 
(PUT 'FVAROP 'DEFINED-ON-LINE '937) 
(PUT 'FVAROP 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FVAROP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FVAROP (F X)
    (PROG (SVP VARF VALF N FARGL S B)
      (COND
       ((AND (EQCAR F 'SQRT) (NOT (CONSTP (CADR F))))
        (SETQ F (LIST 'EXPT (CADR F) (LIST 'QUOTIENT 1 2)))))
      (SETQ B
              (OR (NOT (MEMQ (CAR F) '(PLUS MINUS TIMES EXPT)))
                  (AND (EQ (CAR F) 'EXPT)
                       (OR (NOT (OR (NUMBERP (CADDR F)) (RATIONALEXPONENT F)))
                           (EQ (CADR F) 'E) (CONSTP (CADR F))))))
      (SETQ SVP (SUBSCRIPTEDVARP (CAR F)))
      (SETQ S (GET (CAR F) 'KVARLST))
      (SETQ VARF (COND (SVP (ASSOC (IREVAL F) S)) (T (ASSOC F S))))
      (COND
       ((OR (AND VARF SVP) (AND B VARF (ALLCONST (CDR F) (CDR VARF))))
        (SETQ VARF (CDR VARF)))
       (T
        (PROGN
         (SETQ VARF (FNEWSYM))
         (PUT (CAR F) 'KVARLST
              (CONS (CONS (COND (SVP (IREVAL F)) (T F)) VARF) S))
         (COND
          ((NOT B)
           (PROGN
            (PUT VARF 'ROWINDEX (SETQ N (SETQ ROWMAX (PLUS ROWMAX 1))))
            (COND
             ((NOT
               (OR (AND (EQCAR F 'EXPT) (RATIONALEXPONENT F))
                   (FLAGP (CADR F) 'RATEXP)))
              (SETQ PREVLST (CONS (CONS X N) PREVLST))))
            (FFVAR!2 N VARF F)))
          (T
           (PROGN
            (COND
             ((NOT (AND *VECTORC SVP))
              (PROGN
               (PROG (ARG)
                 (SETQ ARG (CDR F))
                LAB
                 (COND ((NULL ARG) (RETURN NIL)))
                 ((LAMBDA (ARG)
                    (COND
                     ((NOT (OR (CONSTP ARG) (ATOM ARG)))
                      (SETQ FARGL
                              (CONS
                               (FVAROP (COND (SVP (REVAL1 ARG T)) (T ARG)) X)
                               FARGL)))
                     (T (SETQ FARGL (CONS ARG FARGL)))))
                  (CAR ARG))
                 (SETQ ARG (CDR ARG))
                 (GO LAB))
               (SETQ F (CONS (CAR F) (REVERSE FARGL)))
               NIL)))
            (SETQ KVARLST (CONS (CONS VARF F) KVARLST))))))))
      (SETQ PREVLST (CONS (CONS X VARF) PREVLST))
      (RETURN VARF))) 
(PUT 'ALLCONST 'NUMBER-OF-ARGS 2) 
(PUT 'ALLCONST 'DEFINED-ON-LINE '1003) 
(PUT 'ALLCONST 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'ALLCONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALLCONST (L F)
    (NOT
     (MEMBER NIL
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL L)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (EL) (JBCONSTP EL F)) (CAR EL))
                                     NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (EL) (JBCONSTP EL F)) (CAR EL)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))) 
(PUT 'JBCONSTP 'NUMBER-OF-ARGS 2) 
(PUT 'JBCONSTP 'DEFINED-ON-LINE '1006) 
(PUT 'JBCONSTP 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'JBCONSTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JBCONSTP (ITEM REF)
    (COND ((CONSTP ITEM) T)
          ((ATOM ITEM)
           (COND
            ((GET ITEM 'ROWOCC)
             (COND ((LESSP (CAR (GET ITEM 'ROWOCC)) (FINDVARDEF REF)) T)
                   (T NIL)))
            (T T)))
          (T
           (NOT
            (MEMBER NIL
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL (CDR ITEM))
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (JBCONSTP EL REF))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (EL) (JBCONSTP EL REF)) (CAR EL))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))) 
(PUT 'FINDVARDEF 'NUMBER-OF-ARGS 1) 
(PUT 'FINDVARDEF 'DEFINED-ON-LINE '1028) 
(PUT 'FINDVARDEF 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'FINDVARDEF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDVARDEF (V)
    (PROG (R VP VT)
      (SETQ R (GET V 'ROWOCC))
      (SETQ VT (GET V 'VARLST*))
      (SETQ VP (GET V 'VARLST+))
      (COND (R (SETQ R (CAR R)))
            (VT
             (COND
              (VP
               (COND
                ((GREATERP (SETQ VT (CAAR (REVERSE VT)))
                           (SETQ VP (CAAR (REVERSE VP))))
                 (SETQ R VT))
                (T (SETQ R VP))))
              (T (SETQ R (CAAR (REVERSE VT))))))
            (T (SETQ R (CAAR (REVERSE VP)))))
      (RETURN R))) 
(PUT 'SSETVARS 'NUMBER-OF-ARGS 1) 
(PUT 'SSETVARS 'DEFINED-ON-LINE '1048) 
(PUT 'SSETVARS 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'SSETVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SSETVARS (PREPREFIXLIST)
    (PROGN
     (SETQ PREPREFIXLIST (PREPMULTMAT PREPREFIXLIST))
     (SSETVARS1 'VARLST+ 'PLUS)
     (SSETVARS1 'VARLST* 'TIMES)
     (SETQ VARLST+ (SETQ VARLST* NIL))
     (PROG (EL)
       (SETQ EL (REVERSE PREVLST))
      LAB
       (COND ((NULL EL) (RETURN NIL)))
       ((LAMBDA (EL) (SETPREV (CAR EL) (CDR EL))) (CAR EL))
       (SETQ EL (CDR EL))
       (GO LAB))
     (PROG (EL)
       (SETQ EL KVARLST)
      LAB
       (COND ((NULL EL) (RETURN NIL)))
       ((LAMBDA (EL) (REMPROP (CADR EL) 'KVARLST)) (CAR EL))
       (SETQ EL (CDR EL))
       (GO LAB))
     (PROG (EL)
       (SETQ EL '(PLUS MINUS DIFFERENCE TIMES SQRT EXPT))
      LAB
       (COND ((NULL EL) (RETURN NIL)))
       ((LAMBDA (EL) (REMPROP EL 'KVARLST)) (CAR EL))
       (SETQ EL (CDR EL))
       (GO LAB))
     (REMPROP '*BASES* 'KVARLST)
     (SETQ ENDMAT ROWMAX)
     PREPREFIXLIST)) 
(PUT 'REVISE2 'NUMBER-OF-ARGS 2) 
(PUT 'REVISE2 'DEFINED-ON-LINE '1082) 
(PUT 'REVISE2 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'REVISE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REVISE2 (F D)
    (PROG (RES)
      (COND
       ((ATOM F)
        (COND ((CONSTP F) (RETURN F))
              ((GET F 'ALIASLIST) (RETURN (GET F 'FINALALIAS)))
              (T
               (PROGN
                (COND ((NOT (MEMBER F KNOWN)) (SETQ KNOWN (CONS F KNOWN))))
                (RETURN F)
                NIL))))
       ((NOT (CONSTP F))
        (COND
         ((SUBSCRIPTEDVARP (CAR F))
          (COND
           ((GET (CAR F) 'ALIASLIST)
            (PROGN
             (SETQ F
                     (CONS (CAR F)
                           (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                             (SETQ EL (CDR (IREVAL F)))
                             (COND ((NULL EL) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (EL) (REVISE2 EL D))
                                               (CAR EL))
                                              NIL)))
                            LOOPLABEL
                             (SETQ EL (CDR EL))
                             (COND ((NULL EL) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (EL) (REVISE2 EL D)) (CAR EL))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
             (COND
              ((SETQ RES (ASSOC F (GET (CAR F) 'FINALALIAS)))
               (RETURN (CADR RES)))
              (*VECTORC
               (PROGN
                (SETQ RHSALIASES
                        (CONS (CONS (INTRODUCE-ALIAS F) F) RHSALIASES))
                (RETURN (CAAR RHSALIASES))))
              (T (RETURN F)))))
           (*VECTORC
            (PROGN
             (SETQ RHSALIASES (CONS (CONS (INTRODUCE-ALIAS F) F) RHSALIASES))
             (RETURN (CAAR RHSALIASES))))
           (T (RETURN F))))
         ((SETQ RES (ASSOC F D)) (RETURN (CADR RES)))
         (T
          (RETURN
           (CONS (CAR F)
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL (CDR F))
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL) (REVISE2 EL D)) (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (EL) (REVISE2 EL D)) (CAR EL)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))
       (T (RETURN F))))) 
(PUT 'REVISE 'NUMBER-OF-ARGS 2) 
(PUT 'REVISE 'DEFINED-ON-LINE '1124) 
(PUT 'REVISE 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'REVISE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REVISE (F D)
    (CONS (CAR F)
          (CONS (CADR F)
                (PROG (L FORALL-RESULT FORALL-ENDPTR)
                  (SETQ L (CDDR F))
                  (COND ((NULL L) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (L) (REVISE2 L D)) (CAR L))
                                        NIL)))
                 LOOPLABEL
                  (SETQ L (CDR L))
                  (COND ((NULL L) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (L) (REVISE2 L D)) (CAR L)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))) 
(PUT 'PREREMDEP 'NUMBER-OF-ARGS 1) 
(PUT 'PREREMDEP 'DEFINED-ON-LINE '1127) 
(PUT 'PREREMDEP 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'PREREMDEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREREMDEP (FORMS)
    (PROG (DEFS VAR ALIAS RES CURRALL)
      (SETQ KNOWN NIL)
      (PROG (F)
        (SETQ F FORMS)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (COND (*INPUTC (PPRINTF (CADDR F) (CADR F))))
            (COND (*COMPLEX (SETQ F (REMCOMPLEX F))))
            (COND
             ((NOT (MEMBER (CADR F) '(CSES GSYM)))
              (COND
               ((MEMBER (CAR F) '(EQUAL SETQ))
                (PROGN
                 (SETQ F (REVISE F DEFS))
                 (COND
                  ((ATOM (SETQ VAR (CADR F)))
                   (PROGN
                    (COND
                     ((MEMBER VAR KNOWN)
                      (PROGN
                       (SETQ ALIAS (INTRODUCE-ALIAS VAR))
                       (RPLACA (CDR F) ALIAS)
                       NIL))
                     (T (SETQ KNOWN (CONS VAR KNOWN))))
                    (SETQ RES (CONS F RES))
                    NIL))
                  ((OR *VECTORC (FLAGP (CAR VAR) 'VECTORVAR))
                   (PROGN
                    (FLAG (LIST (CAR VAR)) 'SUBSCRIPTED)
                    (SETQ VAR
                            (CONS (CAR VAR)
                                  (PROG (IDX FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ IDX (CDR VAR))
                                    (COND ((NULL IDX) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (IDX)
                                                        (REMDIFF IDX))
                                                      (CAR IDX))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ IDX (CDR IDX))
                                    (COND ((NULL IDX) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (IDX) (REMDIFF IDX))
                                              (CAR IDX))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                    (SETQ ALIAS (INTRODUCE-ALIAS VAR))
                    (RPLACA (CDR F) ALIAS)
                    (SETQ RES (CONS F RES))
                    NIL))
                  (T
                   (PROGN
                    (FLAG (LIST (CAR VAR)) 'SUBSCRIPTED)
                    (SETQ VAR
                            (CONS (CAR VAR)
                                  (PROG (IE FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ IE (CDR VAR))
                                    (COND ((NULL IE) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (IE)
                                                        (COND
                                                         ((NOT (ATOM IE))
                                                          (PROGN
                                                           (COND
                                                            ((ASSOC
                                                              (SETQ IE
                                                                      (IREVAL
                                                                       IE))
                                                              DEFS)
                                                             (SETQ ALIAS
                                                                     (CADR
                                                                      (ASSOC IE
                                                                             DEFS))))
                                                            (T
                                                             (PROGN
                                                              (SETQ ALIAS
                                                                      (FNEWSYM))
                                                              (SETQ RES
                                                                      (CONS
                                                                       (LIST
                                                                        'SETQ
                                                                        ALIAS
                                                                        IE)
                                                                       RES))
                                                              (SETQ DEFS
                                                                      (CONS
                                                                       (LIST IE
                                                                             ALIAS)
                                                                       DEFS))
                                                              (SETQ CURRALL
                                                                      (CONS
                                                                       ALIAS
                                                                       CURRALL))
                                                              (FLAG
                                                               (LIST ALIAS)
                                                               'ALIASNEWSYM)
                                                              NIL)))
                                                           ALIAS))
                                                         (T IE)))
                                                      (CAR IE))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ IE (CDR IE))
                                    (COND ((NULL IE) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (IE)
                                                (COND
                                                 ((NOT (ATOM IE))
                                                  (PROGN
                                                   (COND
                                                    ((ASSOC
                                                      (SETQ IE (IREVAL IE))
                                                      DEFS)
                                                     (SETQ ALIAS
                                                             (CADR
                                                              (ASSOC IE
                                                                     DEFS))))
                                                    (T
                                                     (PROGN
                                                      (SETQ ALIAS (FNEWSYM))
                                                      (SETQ RES
                                                              (CONS
                                                               (LIST 'SETQ
                                                                     ALIAS IE)
                                                               RES))
                                                      (SETQ DEFS
                                                              (CONS
                                                               (LIST IE ALIAS)
                                                               DEFS))
                                                      (SETQ CURRALL
                                                              (CONS ALIAS
                                                                    CURRALL))
                                                      (FLAG (LIST ALIAS)
                                                            'ALIASNEWSYM)
                                                      NIL)))
                                                   ALIAS))
                                                 (T IE)))
                                              (CAR IE))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                    (SETQ ALIAS (INTRODUCE-ALIAS (IREVAL VAR)))
                    (PROG (A)
                      (SETQ A CURRALL)
                     LAB
                      (COND ((NULL A) (RETURN NIL)))
                      ((LAMBDA (A)
                         (PUT A 'INALIAS (CONS ALIAS (GET A 'INALIAS))))
                       (CAR A))
                      (SETQ A (CDR A))
                      (GO LAB))
                    (RPLACA (CDR F) ALIAS)
                    (SETQ RES (CONS F RES))
                    NIL)))))
               (T (SETQ RES (CONS F RES)))))
             (T (RESTORECSEINFO (CADR FORMS) (CADDR FORMS))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
     RESTOREALL
      (RETURN (REVERSE RES)))) 
(PUT 'INTRODUCE-ALIAS 'NUMBER-OF-ARGS 1) 
(PUT 'INTRODUCE-ALIAS 'DEFINED-ON-LINE '1315) 
(PUT 'INTRODUCE-ALIAS 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'INTRODUCE-ALIAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTRODUCE-ALIAS (VAR)
    (PROG (ALIAS V2)
      (SETQ ALIAS (FNEWSYM))
      (REMFLAG (LIST ALIAS) 'NEWSYM)
      (FLAG (LIST ALIAS) 'ALIASNEWSYM)
      (SETQ V2 (COND ((ATOM VAR) VAR) (T (CAR VAR))))
      (PUT V2 'ALIASLIST (CONS ALIAS (GET V2 'ALIASLIST)))
      (COND ((ATOM VAR) (PUT VAR 'FINALALIAS ALIAS))
            (T
             (PUT V2 'FINALALIAS
                  (CONS (LIST VAR ALIAS)
                        (DELETE (ASSOC VAR (GET V2 'FINALALIAS))
                                (GET V2 'FINALALIAS))))))
      (PUT ALIAS 'ALIAS VAR)
      (SETQ KNOWN (CONS ALIAS KNOWN))
      (RETURN ALIAS))) 
(PUT 'SSETVARS1 'NUMBER-OF-ARGS 2) 
(PUT 'SSETVARS1 'DEFINED-ON-LINE '1340) 
(PUT 'SSETVARS1 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'SSETVARS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SSETVARS1 (VARLST OPV)
    (PROG (Z ZZ ZZEL)
      (PROG (VAR)
        (SETQ VAR (EVAL VARLST))
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROGN
            (SETQ ZZ NIL)
            (SETQ ROWMIN (DIFFERENCE ROWMIN 1))
            (PROG (EL)
              (SETQ EL (GET VAR VARLST))
             LAB
              (COND ((NULL EL) (RETURN NIL)))
              ((LAMBDA (EL)
                 (PROGN
                  (SETQ Z
                          ((LAMBDA (G138)
                             (COND
                              ((OR (IDP (CDR EL)) (CONSTP (CDR EL)))
                               (CONS G138 (CONS (CDR EL) NIL)))
                              (T (CONS G138 (CDR EL)))))
                           ROWMIN))
                  (COND
                   ((OR
                     (NULL
                      (SETQ ZZEL
                              (GETV (GETV CODMAT (PLUS MAXVAR (CAR EL))) 4)))
                     (NOT (EQUAL (CAR (CAR ZZEL)) ROWMIN)))
                    (PUTV (GETV CODMAT (PLUS MAXVAR (CAR EL))) 4
                          (CONS Z ZZEL))))
                  (SETQ ZZ
                          (INSZZZ
                           (COND
                            ((OR (IDP (CDR Z)) (CONSTP (CDR Z)))
                             (CONS (CAR EL) (CONS (CDR Z) NIL)))
                            (T (CONS (CAR EL) (CDR Z))))
                           ZZ))))
               (CAR EL))
              (SETQ EL (CDR EL))
              (GO LAB))
            (PUT VAR VARLST ROWMIN)
            (SETROW ROWMIN OPV VAR NIL ZZ)))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB)))) 
(PUT 'PREPMULTMAT 'NUMBER-OF-ARGS 1) 
(PUT 'PREPMULTMAT 'DEFINED-ON-LINE '1363) 
(PUT 'PREPMULTMAT 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'PREPMULTMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREPMULTMAT (PREPREFIXLIST)
    (PROG (TLCM VAR VAREXP KVL KFOUND PVL PFOUND TEL RATVAL RATLST NEWVARLST
           HVARLST)
      (SETQ HVARLST NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL VARLST*))) (RETURN NIL)))
        (PROGN
         (SETQ VAR (CAR VARLST*))
         (SETQ VARLST* (CDR VARLST*))
         (COND
          ((FLAGP VAR 'RATEXP)
           (PROGN
            (SETQ TLCM 1)
            (REMFLAG (LIST VAR) 'RATEXP)
            (PROG (ELEM)
              (SETQ ELEM (GET VAR 'VARLST*))
             LAB
              (COND ((NULL ELEM) (RETURN NIL)))
              ((LAMBDA (ELEM)
                 (COND
                  ((PAIRP (CDR ELEM)) (SETQ TLCM (LCM2 TLCM (CDDR ELEM))))))
               (CAR ELEM))
              (SETQ ELEM (CDR ELEM))
              (GO LAB))
            (SETQ VAREXP (FNEWSYM))
            (SETQ TEL
                    (CONS VAREXP
                          (COND ((EQUAL TLCM 2) (LIST 'SQRT VAR))
                                (T
                                 (LIST 'EXPT VAR
                                       (COND
                                        ((ONEP
                                          (CDR
                                           (SETQ TEL
                                                   (SIMPQUOT (LIST 1 TLCM)))))
                                         (CAR TEL))
                                        (T
                                         (LIST 'QUOTIENT (CAR TEL)
                                               (CDR TEL)))))))))
            (COND
             ((ASSOC VAR KVARLST)
              (PROGN
               (SETQ KVL (SETQ KFOUND NIL))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND KVARLST (NOT KFOUND))) (RETURN NIL)))
                 (COND
                  ((EQ (CAAR KVARLST) VAR)
                   (PROGN
                    (SETQ KVL (CONS TEL KVL))
                    (SETQ KFOUND T)
                    (SETQ PVL (SETQ PFOUND NIL))
                    (SETQ PREVLST (REVERSE PREVLST))
                    (PROG ()
                     WHILELABEL
                      (COND ((NOT (AND PREVLST (NOT PFOUND))) (RETURN NIL)))
                      (COND
                       ((EQ (CDAR PREVLST) VAR)
                        (PROGN
                         (SETQ PVL (CONS (CONS (CAAR PREVLST) VAREXP) PVL))
                         (SETQ PFOUND T)))
                       (T
                        (PROGN
                         (SETQ PVL (CONS (CAR PREVLST) PVL))
                         (SETQ PREVLST (CDR PREVLST)))))
                      (GO WHILELABEL))
                    (COND
                     (PVL
                      (COND
                       (PREVLST (SETQ PREVLST (APPEND (REVERSE PREVLST) PVL)))
                       (T (SETQ PREVLST PVL)))))))
                  (T
                   (PROGN
                    (SETQ KVL (CONS (CAR KVARLST) KVL))
                    (SETQ KVARLST (CDR KVARLST)))))
                 (GO WHILELABEL))
               (COND
                (KVL
                 (COND (KVARLST (SETQ KVARLST (APPEND (REVERSE KVL) KVARLST)))
                       (T (SETQ KVARLST (REVERSE KVL))))))))
             (T (SETQ PREPREFIXLIST (CONS TEL PREPREFIXLIST))))
            (SETQ RATLST (SETQ NEWVARLST NIL))
            (PROG (ELEM)
              (SETQ ELEM (GET VAR 'VARLST*))
             LAB
              (COND ((NULL ELEM) (RETURN NIL)))
              ((LAMBDA (ELEM)
                 (COND
                  ((PAIRP (CDR ELEM))
                   (PROGN
                    (SETQ RATVAL
                            (DIVIDE
                             (QUOTIENT (TIMES TLCM (CADR ELEM)) (CDDR ELEM))
                             TLCM))
                    (SETQ RATLST (CONS (CONS (CAR ELEM) (CDR RATVAL)) RATLST))
                    (COND
                     ((GREATERP (CAR RATVAL) 0)
                      (SETQ NEWVARLST
                              (CONS (CONS (CAR ELEM) (CAR RATVAL))
                                    NEWVARLST))))))
                  (T (SETQ NEWVARLST (CONS ELEM NEWVARLST)))))
               (CAR ELEM))
              (SETQ ELEM (CDR ELEM))
              (GO LAB))
            (COND
             (RATLST
              (PROGN
               (PUT VAREXP 'VARLST* (REVERSE RATLST))
               (SETQ HVARLST (CONS VAREXP HVARLST)))))
            (COND
             (NEWVARLST
              (PROGN
               (PUT VAR 'VARLST* (REVERSE NEWVARLST))
               (SETQ HVARLST (CONS VAR HVARLST))))
             (T (REMPROP VAR 'VARLST*)))))
          (T (SETQ HVARLST (CONS VAR HVARLST)))))
        (GO WHILELABEL))
      (SETQ VARLST* HVARLST)
      (RETURN PREPREFIXLIST))) 
(PUT 'LCM2 'NUMBER-OF-ARGS 2) 
(PUT 'LCM2 'DEFINED-ON-LINE '1449) 
(PUT 'LCM2 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'LCM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LCM2 (A B)
    (PROG (G RES)
      (SETQ G (GCD2 A B))
      (SETQ RES (TIMES A B))
      (RETURN (QUOTIENT RES G)))) 
(PUT 'SETPREV 'NUMBER-OF-ARGS 2) 
(PUT 'SETPREV 'DEFINED-ON-LINE '1509) 
(PUT 'SETPREV 'DEFINED-IN-FILE 'SCOPE/CODMAT.RED) 
(PUT 'SETPREV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETPREV (X Y)
    (COND
     ((NUMBERP (GETV (GETV CODMAT (PLUS MAXVAR X)) 3))
      (SETPREV (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) Y))
     (T
      (PUTV (GETV CODMAT (PLUS MAXVAR X)) 8
            (CONS Y (GETV (GETV CODMAT (PLUS MAXVAR X)) 8)))))) 
(ENDMODULE) 