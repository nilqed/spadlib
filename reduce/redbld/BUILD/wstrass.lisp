(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'WSTRASS)) 
(FLUID
 '(*EXP *GCD *MCD *STRUCTURE *UNCACHED *KEEPSQRTS *TRA *TRMIN INTVAR
   LISTOFALLSQRTS LISTOFNEWSQRTS MAGICLIST PREVIOUSBASIS SQRT-INTVAR SQRTFLAG
   SQRTS-IN-INTEGRAND TAYLORASSLIST TAYLORVARIABLE THISPLACE ZLIST)) 
(GLOBAL '(COATES-FDI)) 
(EXPORTS (LIST 'SIMPWSTRASS 'WEIERSTRASS-FORM)) 
(IMPORTS
 (LIST 'GCDN 'SQRTSINPLACES 'MAKEINITIALBASIS 'MKVEC 'COMPLETEPLACES
       'INTEGRALBASIS 'NORMALBASIS 'MKSP 'MULTSQ 'XSUBSTITUTESQ 'TAYLORFORM
       'TAYLOREVALUATE 'COATESSOLVE 'CHECKPOLES 'SUBSTITUTESQ 'REMOVECMSQ
       'PRINTSQ 'INTERR 'TERPRI* 'PRINTPLACE 'FINITISE
       'FRACTIONAL-DEGREE-AT-INFINITY '*MULTSQ 'FDI-PRINT 'FDI-UPGRADE
       'FDI-REVERTSQ 'SIMP 'NEWPLACE 'XSUBSTITUTEP 'SQRTSINSQ 'REMOVEDUPLICATES
       '*EXPTF '*MULTF '*MULTSQ '*Q2F 'MAPVEC 'UPBV 'COATES-LINEQ 'ADDSQ
       '*ADDSQ)) 
(PUT 'SIMPWSTRASS 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPWSTRASS 'DEFINED-ON-LINE '64) 
(PUT 'SIMPWSTRASS 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'SIMPWSTRASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPWSTRASS (U)
    (PROG (INTVAR SQRT-INTVAR TAYLORVARIABLE TAYLORASSLIST LISTOFALLSQRTS
           LISTOFNEWSQRTS SQRTFLAG SQRTS-IN-INTEGRAND TT *KEEPSQRTS *EXP *GCD
           *MCD *STRUCTURE *UNCACHED)
      (SETQ *KEEPSQRTS T)
      (SETQ *EXP (SETQ *GCD (SETQ *MCD (SETQ *UNCACHED T))))
      (SETQ *STRUCTURE NIL)
      (SETQ TT (TIME))
      (SETQ SQRTFLAG T)
      (SETQ TAYLORVARIABLE (SETQ INTVAR (CAR U)))
      (SETQ SQRT-INTVAR (CAAAR (*Q2F (SIMPSQRTI INTVAR))))
      (SETQ U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (CDR U))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (INT-SIMP V)) (CAR V))
                                      NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (INT-SIMP V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SQRTS-IN-INTEGRAND (SQRTSINSQL U INTVAR))
      (SETQ U (ERRORSET* '(WEIERSTRASS-FORM SQRTS-IN-INTEGRAND) T))
      (COND ((ATOM U) (RETURN U)) (T (SETQ U (CAR U))))
      ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
       (LIST 'TIME 'TAKEN (DIFFERENCE (TIME) TT) 'MILLISECONDS))
      (PROGN (PRIN2 "New x value is:") (TERPRI) "New x value is:")
      (PRINTSQ (CAR U))
      (SETQ U (CDR U))
      (PROGN (PRIN2 "New y value is:") (TERPRI) "New y value is:")
      (PRINTSQ (CAR U))
      (SETQ U (CDR U))
      (PROGN
       (PRIN2 "Related by the equation")
       (TERPRI)
       "Related by the equation")
      (PRINTSQ (CAR U))
      (RETURN (CAR U)))) 
(PUT 'WSTRASS 'SIMPFN 'SIMPWSTRASS) 
(PUT 'WEIERSTRASS-FORM 'NUMBER-OF-ARGS 1) 
(PUT 'WEIERSTRASS-FORM 'DEFINED-ON-LINE '98) 
(PUT 'WEIERSTRASS-FORM 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'WEIERSTRASS-FORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WEIERSTRASS-FORM (SQRTL)
    (PROG (SQRTL2 U X2 X1 VECT A B C D LHS RHS)
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2 "Find Weierstrass form for elliptic curve defined by:")
          (TERPRI)
          "Find Weierstrass form for elliptic curve defined by:")
         (PROG (U)
           (SETQ U SQRTL)
          LAB
           (COND ((NULL U) (RETURN NIL)))
           ((LAMBDA (U) (PRINTSQ (SIMP U))) (CAR U))
           (SETQ U (CDR U))
           (GO LAB)))))
      (SETQ SQRTL2 (SQRTS-AT-INFINITY SQRTL))
      (SETQ SQRTL2
              (APPEND (CAR SQRTL2)
                      (PROG (U FORALL-RESULT FORALL-ENDPTR)
                        (SETQ U (CDR SQRTL2))
                        (COND ((NULL U) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (U) (CONS U U)) (CAR U))
                                              NIL)))
                       LOOPLABEL
                        (SETQ U (CDR U))
                        (COND ((NULL U) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CONS U U)) (CAR U)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (SETQ X2 (COATES-WSTRASS (LIST SQRTL2) (LIST (MINUS 3)) INTVAR))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Function with pole of order 3 (x2) is:")
          (TERPRI)
          "Function with pole of order 3 (x2) is:")
         (PRINTSQ X2))))
      (SETQ X1 (COATES-WSTRASS (LIST SQRTL2) (LIST (MINUS 2)) INTVAR))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "Function with pole of order 2 (x1) is:")
          (TERPRI)
          "Function with pole of order 2 (x1) is:")
         (PRINTSQ X1))))
      (SETQ VECT
              (MKVEC
               (LIST (CONS 1 1) X1 X2 (*MULTSQ X1 X1) (*MULTSQ X2 X2)
                     (*MULTSQ X1 (*MULTSQ X1 X1)) (*MULTSQ X1 X2))))
      (SETQ U (CONS (*LCM* (*EXPTF (CDR X1) 3) (*MULTF (CDR X2) (CDR X2))) 1))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
        (PUTV VECT I (*Q2F (*MULTSQ U (GETV VECT I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "List of seven functions in weierstrass!-form:")
          (TERPRI)
          "List of seven functions in weierstrass!-form:")
         (MAPVEC VECT (FUNCTION PRINTSF)))))
      (SETQ VECT (WSTRASS-LINEQ VECT))
      (COND
       ((EQ VECT 'FAILED)
        (INTERR "Linear equation solving failed in Weierstrass")))
      (SETQ X2
              (*ADDSQ (*MULTSQ (*MULTSQ (CONS 2 1) (GETV VECT 4)) X2)
                      (*ADDSQ (*MULTSQ X1 (GETV VECT 6)) (GETV VECT 2))))
      (PUTV VECT 4 (*MULTSQ (CONS (MINUS 4) 1) (GETV VECT 4)))
      (SETQ A (*MULTSQ (GETV VECT 4) (GETV VECT 5)))
      (SETQ B
              (*ADDSQ (*MULTSQ (GETV VECT 6) (GETV VECT 6))
                      (*MULTSQ (GETV VECT 3) (GETV VECT 4))))
      (SETQ C
              (*ADDSQ
               (*MULTSQ (CONS 2 1) (*MULTSQ (GETV VECT 2) (GETV VECT 6)))
               (*MULTSQ (GETV VECT 1) (GETV VECT 4))))
      (SETQ D
              (*ADDSQ (*MULTSQ (GETV VECT 2) (GETV VECT 2))
                      (*MULTSQ (GETV VECT 0) (GETV VECT 4))))
      (SETQ LHS (*MULTSQ X2 X2))
      (SETQ RHS
              (*ADDSQ D
                      (*MULTSQ X1
                               (*ADDSQ C
                                       (*MULTSQ X1
                                                (*ADDSQ B (*MULTSQ X1 A)))))))
      (COND
       ((NEQ LHS RHS)
        (PROGN
         (PRINTSQ LHS)
         (PRINTSQ RHS)
         (INTERR "Previous two unequal - consistency failure 1"))))
      (SETQ U (*LCM* (*LCM* (CDR A) (CDR B)) (*LCM* (CDR C) (CDR D))))
      (COND
       ((NEQ U 1)
        (PROGN
         (SETQ X2 (*MULTSQ X2 (CONS U 1)))
         (SETQ U (CONS (*MULTF U U) 1))
         (SETQ A (*MULTSQ A U))
         (SETQ B (*MULTSQ B U))
         (SETQ C (*MULTSQ C U))
         (SETQ D (*MULTSQ D U)))))
      (COND
       ((AND (CAR B) (NOT ((LAMBDA (*EXP) (QUOTF1 (CAR B) 3)) T)))
        (PROGN
         (SETQ X2 (MULTSQ (CONS 3 1) X2))
         (SETQ U (CONS 9 1))
         (SETQ A (MULTSQ A U))
         (SETQ B (MULTSQ B U))
         (SETQ C (MULTSQ C U))
         (SETQ D (MULTSQ D U)))))
      (SETQ X2 (*MULTSQ X2 A))
      (SETQ X1 (*MULTSQ X1 A))
      (SETQ C (*MULTSQ A C))
      (SETQ D (*MULTSQ (*MULTSQ A A) D))
      (SETQ LHS (*MULTSQ X2 X2))
      (SETQ RHS (*ADDSQ D (*MULTSQ X1 (*ADDSQ C (*MULTSQ X1 (*ADDSQ B X1))))))
      (COND
       ((NEQ LHS RHS)
        (PROGN
         (PRINTSQ LHS)
         (PRINTSQ RHS)
         (INTERR "Previous two unequal - consistency failure 2"))))
      (SETQ B (CONS ((LAMBDA (*EXP) (QUOTF1 (CAR B) 3)) T) 1))
      (SETQ X1 (*ADDSQ X1 B))
      (SETQ D
              (*ADDSQ D
                      (*ADDSQ (MULTSQ (CONS 2 1) (*MULTSQ B (*MULTSQ B B)))
                              (NEGSQ (*MULTSQ C B)))))
      (SETQ C (*ADDSQ C (MULTSQ (CONS (MINUS 3) 1) (*MULTSQ B B))))
      (COND
       (*TRA
        (PROGN
         (PRINTSQ X2)
         (PRINTSQ X1)
         (PROGN (PRIN2 "with coefficients") (TERPRI) "with coefficients")
         (PRINTSQ C)
         (PRINTSQ D)
         (SETQ RHS
                 (*ADDSQ D
                         (*ADDSQ (*MULTSQ C X1) (*MULTSQ X1 (*MULTSQ X1 X1)))))
         (SETQ LHS (*MULTSQ X2 X2))
         (COND
          ((NEQ LHS RHS)
           (PROGN
            (PRINTSQ LHS)
            (PRINTSQ RHS)
            (INTERR "Previous two unequal - consistency failure 3")))))))
      (RETURN (WEIERSTRASS-FORM1 C D X1 X2)))) 
(PUT '*LCM* 'NUMBER-OF-ARGS 2) 
(PUT '*LCM* 'DEFINED-ON-LINE '215) 
(PUT '*LCM* 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT '*LCM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *LCM* (U V) (*MULTF U (QUOTF-FAIL V (GCDF U V)))) 
(PUT 'WEIERSTRASS-FORM1 'NUMBER-OF-ARGS 4) 
(PUT 'WEIERSTRASS-FORM1 'DEFINED-ON-LINE '217) 
(PUT 'WEIERSTRASS-FORM1 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'WEIERSTRASS-FORM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE WEIERSTRASS-FORM1 (C D X1 X2)
    (PROG (B U)
      (SETQ U (GCDF (CAR C) (CAR D)))
      (COND
       ((NOT (NUMBERP U))
        (PROG (CC DD)
          (SETQ U (JSQFREE U (CAAAR U)))
          (SETQ U (CDR U))
          (COND ((NULL U) (RETURN NIL)))
          (PROG (V)
            (SETQ V U)
           LAB
            (COND ((NULL V) (RETURN NIL)))
            ((LAMBDA (V)
               (PROG (W)
                 (SETQ W V)
                LAB
                 (COND ((NULL W) (RETURN NIL)))
                 ((LAMBDA (W)
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND
                          (SETQ CC
                                  (QUOTF-FAIL (CAR C)
                                              (COND
                                               (*PHYSOP-LOADED
                                                (PHYSOP-MULTF W W))
                                               (T (POLY-MULTF W W)))))
                          (SETQ DD (QUOTF-FAIL (CAR D) (EXPTF W 3)))))
                        (RETURN NIL)))
                      (PROGN
                       (SETQ C (CONS CC 1))
                       (SETQ D (CONS DD 1))
                       (SETQ X1 (*MULTSQ X1 (CONS 1 W)))
                       (SETQ X2
                               (*MULTSQ X2
                                        (CONS 1
                                              ((LAMBDA (G584)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF W G584))
                                                  (T (POLY-MULTF W G584))))
                                               (SIMPSQRT2 W))))))
                      (GO WHILELABEL)))
                  (CAR W))
                 (SETQ W (CDR W))
                 (GO LAB)))
             (CAR V))
            (SETQ V (CDR V))
            (GO LAB))
          (SETQ U
                  (GCDN (ALGINT-NUMERIC-CONTENT (CAR C))
                        (ALGINT-NUMERIC-CONTENT (CAR D)))))))
      (SETQ B 2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (GREATERP (TIMES B B) U))) (RETURN NIL)))
        (PROG (NC ND UU)
          (SETQ NC 0)
          (PROG ()
           WHILELABEL
            (COND ((NOT (EQUAL (CDR (SETQ UU (DIVIDE U B))) 0)) (RETURN NIL)))
            (PROGN (SETQ NC (PLUS NC 1)) (SETQ U (CAR UU)))
            (GO WHILELABEL))
          (COND ((LESSP NC 2) (GO NEXT)))
          (SETQ UU (ALGINT-NUMERIC-CONTENT (CAR D)))
          (SETQ ND 0)
          (PROG ()
           WHILELABEL
            (COND ((NOT (EQUAL (CDR (SETQ UU (DIVIDE UU B))) 0)) (RETURN NIL)))
            (PROGN (SETQ ND (PLUS ND 1)) (SETQ UU (CAR UU)))
            (GO WHILELABEL))
          (COND ((LESSP ND 3) (GO NEXT)))
          (SETQ NC (MIN (QUOTIENT NC 2) (QUOTIENT ND 3)))
          (SETQ UU (EXPT B NC))
          (SETQ C (MULTSQ C (CONS 1 (EXPT UU 2))))
          (SETQ D (MULTSQ D (CONS 1 (EXPT UU 3))))
          (SETQ X1 (MULTSQ X1 (CONS 1 UU)))
          (SETQ X2 (MULTSQ X2 (CONS 1 (TIMES UU (EXPT B (QUOTIENT NC 2))))))
          (COND
           ((NOT (EVENP NC)) (SETQ X2 (*MULTSQ X2 (*INVSQ (SIMPSQRTI B))))))
         NEXT
          (SETQ B (NEXTPRIME B)))
        (GO WHILELABEL))
      (SETQ U (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) 1) 1)) 1))
      (SETQ U (*ADDSQ (*ADDSQ D (MULTSQ C U)) (EXPTSQ U 3)))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2 "Standard form is y**2 = ")
          (TERPRI)
          "Standard form is y**2 = ")
         (PRINTSQ U))))
      (RETURN (LIST X1 X2 (SIMPSQRTSQ U))))) 
(PUT 'SQRTS-AT-INFINITY 'NUMBER-OF-ARGS 1) 
(PUT 'SQRTS-AT-INFINITY 'DEFINED-ON-LINE '277) 
(PUT 'SQRTS-AT-INFINITY 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'SQRTS-AT-INFINITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQRTS-AT-INFINITY (SQRTL)
    (PROG (INF HACK SQRTL2 REPEATING)
      (SETQ HACK (LIST (LIST INTVAR 'EXPT INTVAR 2)))
      (SETQ INF (LIST (LIST INTVAR 'QUOTIENT 1 INTVAR)))
      (SETQ SQRTL2 (LIST SQRT-INTVAR))
      (PROG ()
       WHILELABEL
        (COND ((NOT (MEMBER SQRT-INTVAR SQRTL2)) (RETURN NIL)))
        (PROGN
         (COND (REPEATING (SETQ INF (APPEND INF HACK))))
         (NEWPLACE INF)
         (SETQ SQRTL2
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V SQRTL)
                  STARTOVER
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (V)
                              (SQRTSINSQ (XSUBSTITUTEP V INF) INTVAR))
                            (CAR V)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ V (CDR V))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (V)
                              (SQRTSINSQ (XSUBSTITUTEP V INF) INTVAR))
                            (CAR V)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ V (CDR V))
                   (GO LOOPLABEL)))
         (SETQ REPEATING T))
        (GO WHILELABEL))
      (SETQ SQRTL2 (REMOVEDUPLICATES SQRTL2))
      (RETURN (CONS INF SQRTL2)))) 
(PUT 'COATES-WSTRASS 'NUMBER-OF-ARGS 3) 
(PUT 'COATES-WSTRASS 'DEFINED-ON-LINE '294) 
(PUT 'COATES-WSTRASS 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'COATES-WSTRASS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COATES-WSTRASS (PLACES MULTS X)
    (PROG (THISPLACE U FINITE-HACK SAVE PLACES2 MULTS2)
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PRINC "Find function with zeros of order:")
         (PROGN (PRIN2 MULTS) (TERPRI) MULTS)
         (COND (*TRA (PRINC " at ")))
         (TERPRI* T)
         (COND (*TRA (MAPC PLACES (FUNCTION PRINTPLACE)))))))
      (SETQ FINITE-HACK (FINITISE PLACES MULTS))
      (SETQ PLACES2 (CAR FINITE-HACK))
      (SETQ MULTS2 (CADR FINITE-HACK))
      (SETQ FINITE-HACK (LIST PLACES MULTS (CADDR FINITE-HACK)))
      (SETQ COATES-FDI (FRACTIONAL-DEGREE-AT-INFINITY U))
      (COND
       ((IEQUAL COATES-FDI 1)
        (RETURN
         (*MULTSQ (WSTRASSMODULE PLACES2 MULTS2 X FINITE-HACK)
                  (CONS
                   (LIST (CONS (GETPOWER (FKERN X) (CADDR FINITE-HACK)) 1))
                   1)))))
      (COND (*TRA (FDI-PRINT)))
      (NEWPLACE
       (LIST (CONS INTVAR THISPLACE) (LIST INTVAR 'EXPT INTVAR COATES-FDI)))
      (SETQ PLACES2
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J PLACES2)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (FDI-UPGRADE J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (FDI-UPGRADE J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SAVE TAYLORASSLIST)
      (SETQ U
              (WSTRASSMODULE PLACES2
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U MULTS2)
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U) (TIMES U COATES-FDI)) (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (U) (TIMES U COATES-FDI)) (CAR U))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               X FINITE-HACK))
      (SETQ TAYLORASSLIST SAVE)
      (SETQ U (FDI-REVERTSQ U))
      (RETURN
       (*MULTSQ U
                (CONS (LIST (CONS (GETPOWER (FKERN X) (CADDR FINITE-HACK)) 1))
                      1))))) 
(PUT 'WSTRASSMODULE 'NUMBER-OF-ARGS 4) 
(PUT 'WSTRASSMODULE 'DEFINED-ON-LINE '339) 
(PUT 'WSTRASSMODULE 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'WSTRASSMODULE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE WSTRASSMODULE (PLACES MULTS X FINITE-HACK)
    (PROG (PZERO MZERO U V BASIS SQRTS MAGICLIST MPOLE PPOLE)
      (SETQ SQRTS (SQRTSINPLACES PLACES))
      (COND (*TRA (PROGN (PRINC "Sqrts on this curve:") (SUPERPRINT SQRTS))))
      (SETQ U PLACES)
      (SETQ V MULTS)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND
          ((LESSP 0 (CAR V))
           (PROGN
            (SETQ MZERO (CONS (CAR V) MZERO))
            (SETQ PZERO (CONS (CAR U) PZERO))))
          (T
           (PROGN
            (SETQ MPOLE (CONS (CAR V) MPOLE))
            (SETQ PPOLE (CONS (CAR U) PPOLE)))))
         (SETQ U (CDR U))
         (SETQ V (CDR V)))
        (GO WHILELABEL))
      (SETQ BASIS (MKVEC (MAKEINITIALBASIS PPOLE)))
      (SETQ U (COMPLETEPLACES PPOLE MPOLE))
      (SETQ BASIS (INTEGRALBASIS BASIS (CAR U) (CDR U) X))
      (SETQ BASIS (NORMALBASIS BASIS X 0))
      (SETQ U (COATESSOLVE MZERO PZERO BASIS (FORCE-POLE BASIS FINITE-HACK)))
      (SETQ PREVIOUSBASIS NIL)
      (COND ((ATOM U) (RETURN U)))
      (SETQ V (CHECKPOLES (LIST U) PLACES MULTS))
      (COND ((NULL V) (RETURN 'FAILED)))
      (COND ((NOT MAGICLIST) (RETURN U)))
      (SETQ U (REMOVECMSQ (SUBSTITUTESQ U V)))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN (PRIN2 "Function is") (TERPRI) "Function is")
         (PRINTSQ U))))
      (SETQ MAGICLIST NIL)
      (COND ((CHECKPOLES (LIST U) PLACES MULTS) (RETURN U))
            (T (INTERR "Inconsistent checkpoles"))))) 
(PUT 'FORCE-POLE 'NUMBER-OF-ARGS 2) 
(PUT 'FORCE-POLE 'DEFINED-ON-LINE '386) 
(PUT 'FORCE-POLE 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'FORCE-POLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORCE-POLE (BASIS FINITE-HACK)
    (PROG (PLACES MULTS U ANS)
      (SETQ PLACES (CAR FINITE-HACK))
      (SETQ MULTS (CADR FINITE-HACK))
      (SETQ FINITE-HACK (CADDR FINITE-HACK))
      (SETQ U (CONS (LIST (CONS (GETPOWER (FKERN INTVAR) FINITE-HACK) 1)) 1))
      (SETQ BASIS
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V BASIS)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (MULTSQ U V)) (CAR V))
                                      NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (MULTSQ U V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT PLACES) (RETURN NIL)))
        (PROGN
         (SETQ U
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V BASIS)
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V)
                                       (TAYLOREVALUATE
                                        (TAYLORFORM
                                         (XSUBSTITUTESQ V (CAR PLACES)))
                                        (CAR MULTS)))
                                     (CAR V))
                                    NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (V)
                               (TAYLOREVALUATE
                                (TAYLORFORM (XSUBSTITUTESQ V (CAR PLACES)))
                                (CAR MULTS)))
                             (CAR V))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ MULTS (CDR MULTS))
         (SETQ PLACES (CDR PLACES))
         (SETQ ANS (CONS U ANS)))
        (GO WHILELABEL))
      (RETURN ANS))) 
(PUT 'WSTRASS-LINEQ 'NUMBER-OF-ARGS 1) 
(PUT 'WSTRASS-LINEQ 'DEFINED-ON-LINE '404) 
(PUT 'WSTRASS-LINEQ 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'WSTRASS-LINEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WSTRASS-LINEQ (VECT)
    (PROG (ZLIST POWLIST M RIGHTSIDE V ZERO ONE)
      (SETQ ZERO (CONS NIL 1))
      (SETQ ONE (CONS 1 1))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
        (SETQ ZLIST (VARSINSF (GETV VECT I) ZLIST))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ ZLIST (CONS INTVAR (FINDZVARS ZLIST NIL INTVAR NIL)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
        (PUTV VECT I (F2DF (GETV VECT I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
        (PROG (U)
          (SETQ U (GETV VECT I))
         LAB
          (COND ((NULL U) (RETURN NIL)))
          ((LAMBDA (U)
             (COND
              ((NOT (MEMBER (CAR U) POWLIST))
               (SETQ POWLIST (CONS (CAR U) POWLIST)))))
           (CAR U))
          (SETQ U (CDR U))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ M
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U POWLIST)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (PROG (V)
                                      (SETQ V (MKVECT 6))
                                      (PROG (I)
                                        (SETQ I 0)
                                       LAB
                                        (COND
                                         ((MINUSP (DIFFERENCE 6 I))
                                          (RETURN NIL)))
                                        (PUTV V I
                                              ((LAMBDA (U)
                                                 (COND ((NULL U) ZERO)
                                                       (T (CDR U))))
                                               (ASSOC U (GETV VECT I))))
                                        (SETQ I (PLUS2 I 1))
                                        (GO LAB))
                                      (RETURN V)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (PROG (V)
                              (SETQ V (MKVECT 6))
                              (PROG (I)
                                (SETQ I 0)
                               LAB
                                (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
                                (PUTV V I
                                      ((LAMBDA (U)
                                         (COND ((NULL U) ZERO) (T (CDR U))))
                                       (ASSOC U (GETV VECT I))))
                                (SETQ I (PLUS2 I 1))
                                (GO LAB))
                              (RETURN V)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V (MKVECT 6))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 I)) (RETURN NIL)))
        (PUTV V I ZERO)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PUTV V 4 ONE)
      (SETQ M (MKVEC (CONS V M)))
      (SETQ V (UPBV M))
      (SETQ RIGHTSIDE (MKVECT V))
      (PUTV RIGHTSIDE 0 ONE)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE V I)) (RETURN NIL)))
        (PUTV RIGHTSIDE I ZERO)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (COATES-LINEQ M RIGHTSIDE)))) 
(PUT 'ALGINT-NUMERIC-CONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'ALGINT-NUMERIC-CONTENT 'DEFINED-ON-LINE '447) 
(PUT 'ALGINT-NUMERIC-CONTENT 'DEFINED-IN-FILE 'ALGINT/WSTRASS.RED) 
(PUT 'ALGINT-NUMERIC-CONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGINT-NUMERIC-CONTENT (FORM)
    (COND ((OR (ATOM FORM) (ATOM (CAR FORM))) (ABS FORM))
          ((NULL (CDR FORM)) (ALGINT-NUMERIC-CONTENT (CDAR FORM)))
          (T
           (PROG (G1)
             (SETQ G1 (ALGINT-NUMERIC-CONTENT (CDAR FORM)))
             (COND
              ((NOT (EQUAL G1 1))
               (SETQ G1 (GCDDD G1 (ALGINT-NUMERIC-CONTENT (CDR FORM))))))
             (RETURN G1))))) 
(ENDMODULE) 