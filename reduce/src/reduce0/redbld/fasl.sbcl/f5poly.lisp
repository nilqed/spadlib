(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'F5POLY)) 
(REVISION 'F5POLY "$Id: f5poly.red 6359 2022-07-29 06:13:09Z thomas-sturm $") 
(COPYRIGHT 'F5POLY "(c) 2022 A. Demin, T. Sturm, MPI Informatics, Germany") 
(OFF1 'ALLFAC) 
(LOAD-PACKAGE 'DIPOLY) 
(PUT 'POLY_EXTRACTTORDER 'NUMBER-OF-ARGS 0) 
(DE POLY_EXTRACTTORDER NIL
    (PROG (OLDTORDER)
      (SETQ OLDTORDER (TORDER '(LIST)))
      (TORDER (CDR OLDTORDER))
      (DIPSORTINGMODE VDPSORTMODE*)
      (RETURN OLDTORDER))) 
(PUT 'POLY_INITRING 'NUMBER-OF-ARGS 1) 
(DE POLY_INITRING (U)
    (PROG (VARS ORD OLDTORDER)
      (COND ((NULL U) (PROGN (SETQ OLDTORDER (POLY_EXTRACTTORDER))))
            ((EQUAL (LENGTH U) 1)
             (PROGN
              (SETQ VARS (PROG1 (CAR U) (SETQ U (CDR U))))
              (SETQ OLDTORDER (POLY_EXTRACTTORDER))
              (SETQ GLOBAL-DIPVARS* (CONS 'LIST VARS))))
            (T
             (PROGN
              (SETQ VARS (PROG1 (CAR U) (SETQ U (CDR U))))
              (SETQ ORD (PROG1 (CAR U) (SETQ U (CDR U))))
              (COND
               ((NULL U)
                (SETQ OLDTORDER (TORDER (LIST (CONS 'LIST VARS) ORD))))
               (T
                (SETQ OLDTORDER
                        (TORDER
                         (LIST (CONS 'LIST VARS) ORD
                               (PROG1 (CAR U) (SETQ U (CDR U)))))))))))
      (RETURN OLDTORDER))) 
(DE POLY_POLYNOMIALWITHSUGAR (TS CFS SUGAR) (LIST 'P TS CFS SUGAR)) 
(PUT 'POLY_POLYNOMIALWITHSUGAR 'NUMBER-OF-ARGS 3) 
(PUTC 'POLY_POLYNOMIALWITHSUGAR 'INLINE
      '(LAMBDA (TS CFS SUGAR) (LIST 'P TS CFS SUGAR))) 
(DE POLY_POLYNOMIAL (TS CFS) (LIST 'P TS CFS 0)) 
(PUT 'POLY_POLYNOMIAL 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_POLYNOMIAL 'INLINE '(LAMBDA (TS CFS) (LIST 'P TS CFS 0))) 
(DE POLY_GETTERMS (POLY) (CADR POLY)) 
(PUT 'POLY_GETTERMS 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_GETTERMS 'INLINE '(LAMBDA (POLY) (CADR POLY))) 
(DE POLY_GETCOEFFS (POLY) (CADDR POLY)) 
(PUT 'POLY_GETCOEFFS 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_GETCOEFFS 'INLINE '(LAMBDA (POLY) (CADDR POLY))) 
(DE POLY_GETSUGAR (POLY) (CADDDR POLY)) 
(PUT 'POLY_GETSUGAR 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_GETSUGAR 'INLINE '(LAMBDA (POLY) (CADDDR POLY))) 
(DE POLY_ZERO NIL (LIST 'P NIL NIL 0)) 
(PUT 'POLY_ZERO 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_ZERO 'INLINE '(LAMBDA () (LIST 'P NIL NIL 0))) 
(DE POLY_ONE NIL (LIST 'P (LIST (POLY_ZEROEXP)) (LIST (POLY_ONECOEFF)) 0)) 
(PUT 'POLY_ONE 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_ONE 'INLINE
      '(LAMBDA () (LIST 'P (LIST (POLY_ZEROEXP)) (LIST (POLY_ONECOEFF)) 0))) 
(DE POLY_ISZERO? (P) (NULL (CADR P))) 
(PUT 'POLY_ISZERO? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISZERO? 'INLINE '(LAMBDA (P) (NULL (CADR P)))) 
(DE POLY_SF2POLY (U) (POLY_SQ2POLY (CONS U 1))) 
(PUT 'POLY_SF2POLY 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_SF2POLY 'INLINE '(LAMBDA (U) (POLY_SQ2POLY (CONS U 1)))) 
(PUT 'POLY_SF2POLY1 'NUMBER-OF-ARGS 3) 
(DE POLY_SF2POLY1 (U EV BC)
    (COND ((NULL U) (LIST 'P NIL NIL 0))
          ((OR (ATOM U) (ATOM (CAR U)))
           (LIST 'P (LIST EV) (LIST (POLY_MULCOEFF BC (POLY_2COEFF U)))
                 (POLY_TOTALDEGEXP EV)))
          (T
           (POLY_SUMPOLY (POLY_SF2POLY2 (CAAAR U) (CDAAR U) (CDAR U) EV BC)
            (POLY_SF2POLY1 (CDR U) EV BC))))) 
(PUT 'POLY_ISPOLYVAR 'NUMBER-OF-ARGS 1) 
(DE POLY_ISPOLYVAR (VAR)
    (PROG (VARS)
      (SETQ VARS GLOBAL-DIPVARS*)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VARS (NOT (EQ (CAR VARS) VAR)))) (RETURN NIL)))
        (SETQ VARS (CDR VARS))
        (GO WHILELABEL))
      (RETURN (NOT (NULL VARS))))) 
(DE POLY_GROUNDCOEFF (VAR DEG) (CONS (CONS (CONS (CONS VAR DEG) 1) NIL) 1)) 
(PUT 'POLY_GROUNDCOEFF 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_GROUNDCOEFF 'INLINE
      '(LAMBDA (VAR DEG) (CONS (CONS (CONS (CONS VAR DEG) 1) NIL) 1))) 
(PUT 'POLY_SF2POLY2 'NUMBER-OF-ARGS 5) 
(DE POLY_SF2POLY2 (VAR DG C EV BC)
    (COND
     ((POLY_ISPOLYVAR VAR)
      (POLY_SF2POLY1 C (POLY_INSERTEXP EV VAR DG (CDR GLOBAL-DIPVARS*)) BC))
     (T
      (POLY_SF2POLY1 C EV
       (POLY_MULCOEFF BC (CONS (CONS (CONS (CONS VAR DG) 1) NIL) 1)))))) 
(DE POLY_SQ2POLY (U) (POLY_SQ2POLY1 U (POLY_ZEROEXP) (POLY_ONECOEFF))) 
(PUT 'POLY_SQ2POLY 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_SQ2POLY 'INLINE
      '(LAMBDA (U) (POLY_SQ2POLY1 U (POLY_ZEROEXP) (POLY_ONECOEFF)))) 
(PUT 'POLY_SQ2POLY1 'NUMBER-OF-ARGS 3) 
(DE POLY_SQ2POLY1 (U EV BC)
    (PROG (NUMPOLY)
      (PROG (VAR)
        (SETQ VAR (KERNELS (CDR U)))
       LAB
        (COND ((NULL VAR) (RETURN NIL)))
        ((LAMBDA (VAR)
           (PROGN
            (COND
             ((POLY_ISPOLYVAR VAR)
              (REDERR
               (LIST "Polynomial variable in denominator in input:" VAR))))))
         (CAR VAR))
        (SETQ VAR (CDR VAR))
        (GO LAB))
      (SETQ NUMPOLY (POLY_SF2POLY1 (CAR U) EV BC))
      (RETURN (POLY_MULTCOEFFS NUMPOLY (CONS 1 (CDR U)))))) 
(PUT 'POLY_LEAD2SQ 'NUMBER-OF-ARGS 2) 
(DE POLY_LEAD2SQ (CF TM)
    (PROG (VS)
      (SETQ VS GLOBAL-DIPVARS*)
      (PROG (E)
        (SETQ E (CDR TM))
       LAB
        (COND ((NULL E) (RETURN NIL)))
        ((LAMBDA (E)
           (SETQ CF
                   (MULTSQ
                    (CONS
                     (MKSP* (CAR (SIMP (PROG1 (CAR VS) (SETQ VS (CDR VS))))) E)
                     1)
                    CF)))
         (CAR E))
        (SETQ E (CDR E))
        (GO LAB))
      (RETURN CF))) 
(PUT 'POLY_POLY2SQ 'NUMBER-OF-ARGS 1) 
(DE POLY_POLY2SQ (P)
    (ADDSQ (POLY_LEAD2SQ (POLY_LEADCOEFF P) (POLY_LEADTERM P))
           (POLY_POLY2SQ (POLY_TAIL P)))) 
(PUT 'POLY_REPLUS 'NUMBER-OF-ARGS 1) 
(DE POLY_REPLUS (U)
    (COND ((ATOM U) U) ((NULL (CDR U)) (CAR U)) (T (CONS 'PLUS U)))) 
(PUT 'POLY_RETIMES 'NUMBER-OF-ARGS 1) 
(PUT 'POLY_RETIMES 'DEFINED-ON-LINE '229) 
(PUT 'POLY_RETIMES 'DEFINED-IN-FILE 'F5/F5POLY.RED) 
(PUT 'POLY_RETIMES 'PROCEDURE_TYPE '(ARROW LIST LIST)) 
(DE POLY_RETIMES (U)
    (COND ((EQUAL (CAR U) 1) (COND ((CDR U) (POLY_RETIMES (CDR U))) (T 1)))
          ((NULL (CDR U)) (CAR U)) (T (CONS 'TIMES U)))) 
(PUT 'POLY_POLY2LP1 'NUMBER-OF-ARGS 1) 
(DE POLY_POLY2LP1 (U)
    (PROG (X Y)
      (COND ((NULL (CADR U)) (RETURN NIL)))
      (SETQ X (POLY_LEADCOEFF U))
      (SETQ Y (POLY_2AEXP (POLY_LEADTERM U)))
      (COND
       ((POLY_ISNEGCOEFF? X)
        (PROGN
         (RETURN
          (CONS
           (LIST 'MINUS
                 (POLY_RETIMES (CONS (POLY_2ACOEFF (POLY_NEGCOEFF X)) Y)))
           (POLY_POLY2LP1 (POLY_TAIL U)))))))
      (RETURN
       (CONS (POLY_RETIMES (CONS (POLY_2ACOEFF X) Y))
             (POLY_POLY2LP1 (POLY_TAIL U)))))) 
(PUT 'POLY_POLY2LP 'NUMBER-OF-ARGS 1) 
(DE POLY_POLY2LP (F)
    (COND ((NULL (CADR F)) 0) (T (POLY_REPLUS (POLY_POLY2LP1 F))))) 
(DE POLY_TOTALDEGEXP (E1) (CAR E1)) 
(PUT 'POLY_TOTALDEGEXP 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_TOTALDEGEXP 'INLINE '(LAMBDA (E1) (CAR E1))) 
(DE POLY_ZEROEXP NIL
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X 1)
      (COND ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
     LOOPLABEL
      (SETQ X (PLUS2 X 1))
      (COND
       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
        (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS 0 NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'POLY_ZEROEXP 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_ZEROEXP 'INLINE
      '(LAMBDA ()
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X 1)
           (COND
            ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
           (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
          LOOPLABEL
           (SETQ X (PLUS2 X 1))
           (COND
            ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
             (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS 0 NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))) 
(PUT 'POLY_SUMEXP 'NUMBER-OF-ARGS 2) 
(DE POLY_SUMEXP (E1 E2)
    (PROGN
     (COND (NIL NIL))
     (COND ((IEQUAL (CAR E1) 0) E2) ((IEQUAL (CAR E2) 0) E1)
           (T
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X E1)
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (IPLUS2 X
                                          (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (IPLUS2 X (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT 'POLY_SUBEXP 'NUMBER-OF-ARGS 2) 
(DE POLY_SUBEXP (E1 E2)
    (PROGN
     (COND (NIL NIL))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X E1)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (IDIFFERENCE X (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                         (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (X)
                   (IDIFFERENCE X (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                 (CAR X))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'POLY_ELMAXEXP 'NUMBER-OF-ARGS 2) 
(DE POLY_ELMAXEXP (E1 E2)
    (PROG (W)
      (COND (NIL NIL))
      (SETQ E1 (CDR E1))
      (SETQ E2 (CDR E2))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X E1)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (MAX X
                                         (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (MAX X (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS
        (PROG (X FORALL-RESULT)
          (SETQ X W)
          (SETQ FORALL-RESULT 0)
         LAB1
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (SETQ FORALL-RESULT (PLUS ((LAMBDA (X) X) (CAR X)) FORALL-RESULT))
          (SETQ X (CDR X))
          (GO LAB1))
        W)))) 
(PUT 'POLY_DIVEXP? 'NUMBER-OF-ARGS 2) 
(DE POLY_DIVEXP? (E1 E2)
    (PROG (BRK)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT BRK) E1)) (RETURN NIL)))
        (SETQ BRK
                (IGREATERP (PROG1 (CAR E1) (SETQ E1 (CDR E1)))
                           (PROG1 (CAR E2) (SETQ E2 (CDR E2)))))
        (GO WHILELABEL))
      (RETURN (NOT BRK)))) 
(PUT 'POLY_DISJEXP? 'NUMBER-OF-ARGS 2) 
(DE POLY_DISJEXP? (E1 E2)
    (PROG (OK)
      (SETQ E1 (CDR E1))
      (SETQ E2 (CDR E2))
      (SETQ OK T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND OK E1)) (RETURN NIL)))
        (SETQ OK
                (OR (IEQUAL (PROG1 (CAR E1) (SETQ E1 (CDR E1))) 0)
                    (IEQUAL (PROG1 (CAR E2) (SETQ E2 (CDR E2))) 0)))
        (GO WHILELABEL))
      (RETURN OK))) 
(PUT 'POLY_INSERTEXP 'NUMBER-OF-ARGS 4) 
(DE POLY_INSERTEXP (EV V DG VARS)
    (CONS (IPLUS2 (CAR EV) DG) (POLY_INSERTEXP1 (CDR EV) V DG VARS))) 
(PUT 'POLY_INSERTEXP1 'NUMBER-OF-ARGS 4) 
(DE POLY_INSERTEXP1 (EV V DG VARS)
    (COND ((OR (NULL EV) (NULL VARS)) NIL)
          ((EQ (CAR VARS) V) (CONS DG (CDR EV)))
          (T (CONS (CAR EV) (POLY_INSERTEXP1 (CDR EV) V DG (CDR VARS)))))) 
(DE POLY_2AEXP (E) (EV_2AEXP1 (CDR E) (CDR GLOBAL-DIPVARS*))) 
(PUT 'POLY_2AEXP 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_2AEXP 'INLINE
      '(LAMBDA (E) (EV_2AEXP1 (CDR E) (CDR GLOBAL-DIPVARS*)))) 
(PUT 'EV_2AEXP1 'NUMBER-OF-ARGS 2) 
(PUT 'EV_2AEXP1 'DEFINED-ON-LINE '331) 
(PUT 'EV_2AEXP1 'DEFINED-IN-FILE 'F5/F5POLY.RED) 
(PUT 'EV_2AEXP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EV_2AEXP1 (U V)
    (COND ((NULL U) NIL) ((IEQUAL (CAR U) 0) (EV_2AEXP1 (CDR U) (CDR V)))
          ((IEQUAL (CAR U) 1) (CONS (CAR V) (EV_2AEXP1 (CDR U) (CDR V))))
          (T (CONS (LIST 'EXPT (CAR V) (CAR U)) (EV_2AEXP1 (CDR U) (CDR V)))))) 
(PUT 'POLY_CMPEXPLEX 'NUMBER-OF-ARGS 2) 
(DE POLY_CMPEXPLEX (E1 E2)
    (PROGN
     (SETQ E1 (CDR E1))
     (SETQ E2 (CDR E2))
     (PROG ()
      WHILELABEL
       (COND ((NOT (AND E1 (IEQUAL (CAR E1) (CAR E2)))) (RETURN NIL)))
       (PROGN (SETQ E1 (CDR E1)) (SETQ E2 (CDR E2)))
       (GO WHILELABEL))
     (AND E1 (ILESSP (CAR E1) (CAR E2))))) 
(DE POLY_CMPEXPGRADLEX (E1 E2)
    (OR (ILESSP (CAR E1) (CAR E2))
        (AND (IEQUAL (CAR E1) (CAR E2)) (POLY_CMPEXPLEX E1 E2)))) 
(PUT 'POLY_CMPEXPGRADLEX 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_CMPEXPGRADLEX 'INLINE
      '(LAMBDA (E1 E2)
         (OR (ILESSP (CAR E1) (CAR E2))
             (AND (IEQUAL (CAR E1) (CAR E2)) (POLY_CMPEXPLEX E1 E2))))) 
(DE POLY_CMPEXPREVGRADLEX (E1 E2)
    (OR (ILESSP (CAR E1) (CAR E2))
        (AND (IEQUAL (CAR E1) (CAR E2))
             (IEQUAL (EVINVLEXCOMP (CDR E1) (CDR E2)) (MINUS 1))))) 
(PUT 'POLY_CMPEXPREVGRADLEX 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_CMPEXPREVGRADLEX 'INLINE
      '(LAMBDA (E1 E2)
         (OR (ILESSP (CAR E1) (CAR E2))
             (AND (IEQUAL (CAR E1) (CAR E2))
                  (IEQUAL (EVINVLEXCOMP (CDR E1) (CDR E2)) (MINUS 1)))))) 
(DE POLY_CMPEXPGENERIC (E1 E2) (EQUAL (EVCOMP (CDR E1) (CDR E2)) (MINUS 1))) 
(PUT 'POLY_CMPEXPGENERIC 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_CMPEXPGENERIC 'INLINE
      '(LAMBDA (E1 E2) (EQUAL (EVCOMP (CDR E1) (CDR E2)) (MINUS 1)))) 
(PUT 'POLY_CMPEXP 'NUMBER-OF-ARGS 2) 
(DE POLY_CMPEXP (E1 E2)
    (PROGN
     (COND ((EQ VDPSORTMODE* 'LEX) (POLY_CMPEXPLEX E1 E2))
           ((EQ VDPSORTMODE* 'GRADLEX)
            (OR (ILESSP (CAR E1) (CAR E2))
                (AND (IEQUAL (CAR E1) (CAR E2)) (POLY_CMPEXPLEX E1 E2))))
           ((EQ VDPSORTMODE* 'REVGRADLEX)
            (OR (ILESSP (CAR E1) (CAR E2))
                (AND (IEQUAL (CAR E1) (CAR E2))
                     (IEQUAL (EVINVLEXCOMP (CDR E1) (CDR E2)) (MINUS 1)))))
           (T (EQUAL (EVCOMP (CDR E1) (CDR E2)) (MINUS 1)))))) 
(DE POLY_TDEGCMPEXP (E1 E2) (ILESSP (CAR E1) (CAR E2))) 
(PUT 'POLY_TDEGCMPEXP 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_TDEGCMPEXP 'INLINE '(LAMBDA (E1 E2) (ILESSP (CAR E1) (CAR E2)))) 
(DE POLY_EQEXP? (E1 E2) (EQUAL E1 E2)) 
(PUT 'POLY_EQEXP? 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_EQEXP? 'INLINE '(LAMBDA (E1 E2) (EQUAL E1 E2))) 
(DE POLY_IDENTITYTERM NIL
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X 1)
      (COND ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
     LOOPLABEL
      (SETQ X (PLUS2 X 1))
      (COND
       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
        (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS 0 NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'POLY_IDENTITYTERM 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_IDENTITYTERM 'INLINE
      '(LAMBDA ()
         (PROG (X FORALL-RESULT FORALL-ENDPTR)
           (SETQ X 1)
           (COND
            ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
           (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
          LOOPLABEL
           (SETQ X (PLUS2 X 1))
           (COND
            ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
             (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS 0 NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))) 
(DE POLY_ISIDENTITYTERM? (TM) (IEQUAL (POLY_TOTALDEGTERM TM) 0)) 
(PUT 'POLY_ISIDENTITYTERM? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISIDENTITYTERM? 'INLINE
      '(LAMBDA (TM) (IEQUAL (POLY_TOTALDEGTERM TM) 0))) 
(DE POLY_TOTALDEGTERM (A) (CAR A)) 
(PUT 'POLY_TOTALDEGTERM 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_TOTALDEGTERM 'INLINE '(LAMBDA (A) (CAR A))) 
(DE POLY_MULTERM (A B) (POLY_SUMEXP A B)) 
(PUT 'POLY_MULTERM 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_MULTERM 'INLINE '(LAMBDA (A B) (POLY_SUMEXP A B))) 
(DE POLY_DIVTERM (A B) (POLY_SUBEXP A B)) 
(PUT 'POLY_DIVTERM 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_DIVTERM 'INLINE '(LAMBDA (A B) (POLY_SUBEXP A B))) 
(DE POLY_DIVIDESTERM? (A B) (POLY_DIVEXP? A B)) 
(PUT 'POLY_DIVIDESTERM? 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_DIVIDESTERM? 'INLINE '(LAMBDA (A B) (POLY_DIVEXP? A B))) 
(DE POLY_LCMTERM (A B) (POLY_ELMAXEXP A B)) 
(PUT 'POLY_LCMTERM 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_LCMTERM 'INLINE '(LAMBDA (A B) (POLY_ELMAXEXP A B))) 
(DE POLY_CMPTERM (A B) (POLY_CMPEXP A B)) 
(PUT 'POLY_CMPTERM 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_CMPTERM 'INLINE '(LAMBDA (A B) (POLY_CMPEXP A B))) 
(DE POLY_DISJTERM? (A B) (POLY_DISJEXP? A B)) 
(PUT 'POLY_DISJTERM? 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_DISJTERM? 'INLINE '(LAMBDA (A B) (POLY_DISJEXP? A B))) 
(DE POLY_EQTERM? (A B) (EQUAL A B)) 
(PUT 'POLY_EQTERM? 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_EQTERM? 'INLINE '(LAMBDA (A B) (EQUAL A B))) 
(DE POLY_TAIL (POLY)
    (LIST 'P (POLY_TAILTERMS POLY) (POLY_TAILCOEFFS POLY) (CADDDR POLY))) 
(PUT 'POLY_TAIL 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_TAIL 'INLINE
      '(LAMBDA (POLY)
         (LIST 'P (POLY_TAILTERMS POLY) (POLY_TAILCOEFFS POLY) (CADDDR POLY)))) 
(DE POLY_LEADTERM (POLY) (CAR (CADR POLY))) 
(PUT 'POLY_LEADTERM 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_LEADTERM 'INLINE '(LAMBDA (POLY) (CAR (CADR POLY)))) 
(DE POLY_LEADCOEFF (POLY) (CAR (CADDR POLY))) 
(PUT 'POLY_LEADCOEFF 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_LEADCOEFF 'INLINE '(LAMBDA (POLY) (CAR (CADDR POLY)))) 
(DE POLY_TAILTERMS (POLY) (CDR (CADR POLY))) 
(PUT 'POLY_TAILTERMS 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_TAILTERMS 'INLINE '(LAMBDA (POLY) (CDR (CADR POLY)))) 
(DE POLY_TAILCOEFFS (POLY) (CDR (CADDR POLY))) 
(PUT 'POLY_TAILCOEFFS 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_TAILCOEFFS 'INLINE '(LAMBDA (POLY) (CDR (CADDR POLY)))) 
(DE POLY_LENGTH (POLY) (LENGTH (CADR POLY))) 
(PUT 'POLY_LENGTH 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_LENGTH 'INLINE '(LAMBDA (POLY) (LENGTH (CADR POLY)))) 
(DE POLY_ISCONST? (POLY)
    (OR (NULL (CADR POLY))
        (EQUAL (CAR (CADR POLY))
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X 1)
                 (COND
                  ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                   (RETURN NIL)))
                 (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                LOOPLABEL
                 (SETQ X (PLUS2 X 1))
                 (COND
                  ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))) 
(PUT 'POLY_ISCONST? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISCONST? 'INLINE
      '(LAMBDA (POLY)
         (OR (NULL (CADR POLY))
             (EQUAL (CAR (CADR POLY))
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X 1)
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                        (RETURN NIL)))
                      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                     LOOPLABEL
                      (SETQ X (PLUS2 X 1))
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))) 
(DE POLY_ISZEROCOEFF? (A) (NOT (CAR A))) 
(PUT 'POLY_ISZEROCOEFF? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISZEROCOEFF? 'INLINE '(LAMBDA (A) (NOT (CAR A)))) 
(DE POLY_ISONECOEFF? (A) (AND (EQN (CAR A) 1) (EQN (CDR A) 1))) 
(PUT 'POLY_ISONECOEFF? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISONECOEFF? 'INLINE
      '(LAMBDA (A) (AND (EQN (CAR A) 1) (EQN (CDR A) 1)))) 
(DE POLY_ZEROCOEFF NIL (POLY_2COEFF 0)) 
(PUT 'POLY_ZEROCOEFF 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_ZEROCOEFF 'INLINE '(LAMBDA () (POLY_2COEFF 0))) 
(DE POLY_ONECOEFF NIL (POLY_2COEFF 1)) 
(PUT 'POLY_ONECOEFF 'NUMBER-OF-ARGS 0) 
(PUTC 'POLY_ONECOEFF 'INLINE '(LAMBDA () (POLY_2COEFF 1))) 
(DE POLY_2COEFF (C) (COND ((SQP C) C) (T (CONS C 1)))) 
(PUT 'POLY_2COEFF 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_2COEFF 'INLINE '(LAMBDA (C) (COND ((SQP C) C) (T (CONS C 1))))) 
(DE POLY_2ACOEFF (C) (PREPSQ C)) 
(PUT 'POLY_2ACOEFF 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_2ACOEFF 'INLINE '(LAMBDA (C) (PREPSQ C))) 
(DE POLY_ADDCOEFF (A B) (ADDSQ A B)) 
(PUT 'POLY_ADDCOEFF 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_ADDCOEFF 'INLINE '(LAMBDA (A B) (ADDSQ A B))) 
(DE POLY_SUBCOEFF (A B) (ADDSQ A (NEGSQ B))) 
(PUT 'POLY_SUBCOEFF 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_SUBCOEFF 'INLINE '(LAMBDA (A B) (ADDSQ A (NEGSQ B)))) 
(DE POLY_MULCOEFF (A B) (MULTSQ A B)) 
(PUT 'POLY_MULCOEFF 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_MULCOEFF 'INLINE '(LAMBDA (A B) (MULTSQ A B))) 
(DE POLY_NEGCOEFF (A) (NEGSQ A)) 
(PUT 'POLY_NEGCOEFF 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_NEGCOEFF 'INLINE '(LAMBDA (A) (NEGSQ A))) 
(DE POLY_ISNEGCOEFF? (A) (MINUSF (CAR A))) 
(PUT 'POLY_ISNEGCOEFF? 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_ISNEGCOEFF? 'INLINE '(LAMBDA (A) (MINUSF (CAR A)))) 
(DE POLY_DIVCOEFF (A B) (MULTSQ A (INVSQ B))) 
(PUT 'POLY_DIVCOEFF 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_DIVCOEFF 'INLINE '(LAMBDA (A B) (MULTSQ A (INVSQ B)))) 
(DE POLY_INVCOEFF (A) (PROGN (CONS (CDR A) (CAR A)))) 
(PUT 'POLY_INVCOEFF 'NUMBER-OF-ARGS 1) 
(PUTC 'POLY_INVCOEFF 'INLINE '(LAMBDA (A) (PROGN (CONS (CDR A) (CAR A))))) 
(DE POLY_PAIRCOMBTAIL (F FMULT FCOEFF G GMULT GCOEFF)
    (POLY_PAIRCOMB (LIST 'P (POLY_TAILTERMS F) (POLY_TAILCOEFFS F) (CADDDR F))
     FMULT FCOEFF (LIST 'P (POLY_TAILTERMS G) (POLY_TAILCOEFFS G) (CADDDR G))
     GMULT GCOEFF)) 
(PUT 'POLY_PAIRCOMBTAIL 'NUMBER-OF-ARGS 6) 
(PUTC 'POLY_PAIRCOMBTAIL 'INLINE
      '(LAMBDA (F FMULT FCOEFF G GMULT GCOEFF)
         (POLY_PAIRCOMB
          (LIST 'P (POLY_TAILTERMS F) (POLY_TAILCOEFFS F) (CADDDR F)) FMULT
          FCOEFF (LIST 'P (POLY_TAILTERMS G) (POLY_TAILCOEFFS G) (CADDDR G))
          GMULT GCOEFF))) 
(PUT 'POLY_PAIRCOMB 'NUMBER-OF-ARGS 6) 
(DE POLY_PAIRCOMB (F FMULT FCOEFF G GMULT GCOEFF)
    (PROG (FTERMS FCOEFFS GTERMS GCOEFFS GMULTCOEFF ISONEFMULT STERMS SCOEFFS
           FT GT FC GC NEWC SUGAR ISONEGMULT ISONEFMULTCF ISONEGMULTCF
           FMULTCOEFF)
      (SETQ FTERMS (CADR F))
      (SETQ FCOEFFS (CADDR F))
      (SETQ GTERMS (CADR G))
      (SETQ GCOEFFS (CADDR G))
      (SETQ GMULTCOEFF FCOEFF)
      (SETQ FMULTCOEFF GCOEFF)
      (SETQ ISONEFMULT (IEQUAL (POLY_TOTALDEGTERM FMULT) 0))
      (SETQ ISONEGMULT (IEQUAL (POLY_TOTALDEGTERM GMULT) 0))
      (SETQ ISONEFMULTCF
              (AND (EQN (CAR FMULTCOEFF) 1) (EQN (CDR FMULTCOEFF) 1)))
      (SETQ ISONEGMULTCF
              (AND (EQN (CAR GMULTCOEFF) 1) (EQN (CDR GMULTCOEFF) 1)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND FTERMS GTERMS)) (RETURN NIL)))
        (PROGN
         (COND
          ((NULL FT)
           (PROGN
            (SETQ FT (CAR FTERMS))
            (COND ((NOT ISONEFMULT) (SETQ FT (POLY_SUMEXP FT FMULT)))))))
         (COND
          ((NULL GT)
           (PROGN
            (SETQ GT (CAR GTERMS))
            (COND ((NOT ISONEGMULT) (SETQ GT (POLY_SUMEXP GT GMULT)))))))
         (COND
          ((POLY_CMPEXP GT FT)
           (PROGN
            (PROGN (SETQ STERMS (CONS FT STERMS)) FT)
            (COND
             (ISONEFMULTCF
              (PROG (W1)
                (SETQ W1 (CAR FCOEFFS))
                (SETQ SCOEFFS (CONS W1 SCOEFFS))
                (RETURN W1)))
             (T
              (PROG (W1)
                (SETQ W1 (MULTSQ FMULTCOEFF (CAR FCOEFFS)))
                (SETQ SCOEFFS (CONS W1 SCOEFFS))
                (RETURN W1))))
            (PROG1 (CAR FTERMS) (SETQ FTERMS (CDR FTERMS)))
            (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))
            (SETQ FT NIL)))
          ((EQUAL GT FT)
           (PROGN
            (SETQ FC
                    (COND (ISONEFMULTCF (CAR FCOEFFS))
                          (T (MULTSQ (CAR FCOEFFS) FMULTCOEFF))))
            (SETQ GC
                    (COND (ISONEGMULTCF (CAR GCOEFFS))
                          (T (MULTSQ (CAR GCOEFFS) GMULTCOEFF))))
            (SETQ NEWC (ADDSQ FC (NEGSQ GC)))
            (COND
             ((NOT (NOT (CAR NEWC)))
              (PROGN
               (PROGN (SETQ STERMS (CONS GT STERMS)) GT)
               (PROGN (SETQ SCOEFFS (CONS NEWC SCOEFFS)) NEWC))))
            (PROG1 (CAR FTERMS) (SETQ FTERMS (CDR FTERMS)))
            (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))
            (PROG1 (CAR GTERMS) (SETQ GTERMS (CDR GTERMS)))
            (PROG1 (CAR GCOEFFS) (SETQ GCOEFFS (CDR GCOEFFS)))
            (SETQ GT NIL)
            (SETQ FT NIL)))
          (T
           (PROGN
            (PROGN (SETQ STERMS (CONS GT STERMS)) GT)
            (COND
             (ISONEGMULTCF
              (PROG (W1)
                (SETQ W1 (NEGSQ (CAR GCOEFFS)))
                (SETQ SCOEFFS (CONS W1 SCOEFFS))
                (RETURN W1)))
             (T
              (PROG (W1)
                (SETQ W1 (NEGSQ (MULTSQ (CAR GCOEFFS) GMULTCOEFF)))
                (SETQ SCOEFFS (CONS W1 SCOEFFS))
                (RETURN W1))))
            (PROG1 (CAR GTERMS) (SETQ GTERMS (CDR GTERMS)))
            (PROG1 (CAR GCOEFFS) (SETQ GCOEFFS (CDR GCOEFFS)))
            (SETQ GT NIL)))))
        (GO WHILELABEL))
      (COND
       ((AND (NULL GTERMS) (NULL FTERMS))
        (PROGN
         (SETQ SCOEFFS (REVERSIP SCOEFFS))
         (SETQ STERMS (REVERSIP STERMS)))))
      (COND
       ((NOT (NULL GTERMS))
        (PROGN
         (COND
          ((IEQUAL (POLY_TOTALDEGTERM GMULT) 0)
           (SETQ STERMS (NCONC (REVERSIP STERMS) GTERMS)))
          (T
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT GTERMS) (RETURN NIL)))
              (PROG (W1)
                (SETQ W1
                        (POLY_SUMEXP
                         (PROG1 (CAR GTERMS) (SETQ GTERMS (CDR GTERMS)))
                         GMULT))
                (SETQ STERMS (CONS W1 STERMS))
                (RETURN W1))
              (GO WHILELABEL))
            (SETQ STERMS (REVERSIP STERMS)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT GCOEFFS) (RETURN NIL)))
           (PROG (W1)
             (SETQ W1
                     (NEGSQ
                      (MULTSQ
                       (PROG1 (CAR GCOEFFS) (SETQ GCOEFFS (CDR GCOEFFS)))
                       GMULTCOEFF)))
             (SETQ SCOEFFS (CONS W1 SCOEFFS))
             (RETURN W1))
           (GO WHILELABEL))
         (SETQ SCOEFFS (REVERSIP SCOEFFS)))))
      (COND
       ((NOT (NULL FTERMS))
        (PROGN
         (COND (ISONEFMULT (SETQ STERMS (NCONC (REVERSIP STERMS) FTERMS)))
               (T
                (PROGN
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT FTERMS) (RETURN NIL)))
                   (PROG (W1)
                     (SETQ W1
                             (POLY_SUMEXP
                              (PROG1 (CAR FTERMS) (SETQ FTERMS (CDR FTERMS)))
                              FMULT))
                     (SETQ STERMS (CONS W1 STERMS))
                     (RETURN W1))
                   (GO WHILELABEL))
                 (SETQ STERMS (REVERSIP STERMS)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT FCOEFFS) (RETURN NIL)))
           (PROG (W1)
             (SETQ W1
                     (MULTSQ (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))
                             FMULTCOEFF))
             (SETQ SCOEFFS (CONS W1 SCOEFFS))
             (RETURN W1))
           (GO WHILELABEL))
         (SETQ SCOEFFS (REVERSIP SCOEFFS)))))
      (SETQ SUGAR
              (MAX (PLUS (CADDDR F) (CAR FMULT))
                   (PLUS (CADDDR G) (CAR GMULT))))
      (RETURN (LIST 'P STERMS SCOEFFS SUGAR)))) 
(PUT 'COPYLIST 'NUMBER-OF-ARGS 1) 
(PUT 'COPYLIST 'DEFINED-ON-LINE '653) 
(PUT 'COPYLIST 'DEFINED-IN-FILE 'F5/F5POLY.RED) 
(PUT 'COPYLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPYLIST (L)
    (PROG (QUEUE NEWPAIR)
      (COND ((NULL L) (RETURN NIL)))
      (SETQ QUEUE (CONS (CAR L) NIL))
      (SETQ QUEUE (CONS QUEUE QUEUE))
      (SETQ L (CDR L))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL L))) (RETURN NIL)))
        (PROGN
         (SETQ NEWPAIR (CONS (CAR L) NIL))
         (SETCDR (CAR QUEUE) NEWPAIR)
         (SETCAR QUEUE NEWPAIR)
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (RETURN (CDR QUEUE)))) 
(PUT 'POLY_MULTCOEFFS 'NUMBER-OF-ARGS 2) 
(DE POLY_MULTCOEFFS (POLY CF)
    (COND ((AND (EQN (CAR CF) 1) (EQN (CDR CF) 1)) POLY)
          (T
           (LIST 'P (CADR POLY)
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C (CADDR POLY))
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (C) (MULTSQ C CF)) (CAR C))
                                         NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (C) (MULTSQ C CF)) (CAR C)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (CADDDR POLY))))) 
(PUT 'POLY_NORMALIZE 'NUMBER-OF-ARGS 1) 
(DE POLY_NORMALIZE (POLY)
    (COND
     (*F5FRACTIONFREE
      (COND
       ((OR (NOT *F5PARAMETRIC) (AND *F5PARAMETRIC *F5PARAMETRICNORMALIZE))
        (POLY_NORMALIZEBYCONTENTPARAMETRIC POLY))
       (T (POLY_NORMALIZEBYCONTENT POLY))))
     ((OR (NOT *F5PARAMETRIC) (AND *F5PARAMETRIC *F5PARAMETRICNORMALIZE))
      (POLY_NORMALIZEBYLEADPARAMETRIC POLY))
     (T (POLY_NORMALIZEBYLEAD POLY)))) 
(PUT 'POLY_NORMALIZEBYCONTENT 'NUMBER-OF-ARGS 1) 
(DE POLY_NORMALIZEBYCONTENT (POLY)
    (PROG (NEWCOEFFS CNT)
      (SETQ CNT (POLY_CONTENT POLY))
      (COND ((MINUSF (CAR (CAR (CADDR POLY)))) (SETQ CNT (NEGSQ CNT))))
      (SETQ NEWCOEFFS
              (PROG (CF FORALL-RESULT FORALL-ENDPTR)
                (SETQ CF (CADDR POLY))
                (COND ((NULL CF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CF) (MULTSQ CF (INVSQ CNT)))
                                  (CAR CF))
                                 NIL)))
               LOOPLABEL
                (SETQ CF (CDR CF))
                (COND ((NULL CF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (CF) (MULTSQ CF (INVSQ CNT))) (CAR CF))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST 'P (CADR POLY) NEWCOEFFS (CADDDR POLY))))) 
(PUT 'POLY_NORMALIZEBYCONTENTPARAMETRIC 'NUMBER-OF-ARGS 1) 
(DE POLY_NORMALIZEBYCONTENTPARAMETRIC (POLY)
    (PROG (NEWCOEFFS CNT)
      (SETQ CNT (POLY_CONTENTPARAMETRIC POLY))
      (COND (*F5PARAMETRICNORMALIZE (PARAM_ADDASSUMPTIONNORMALIZE CNT)))
      (COND ((MINUSF (CAR (CAR (CADDR POLY)))) (SETQ CNT (NEGSQ CNT))))
      (SETQ NEWCOEFFS
              (PROG (CF FORALL-RESULT FORALL-ENDPTR)
                (SETQ CF (CADDR POLY))
                (COND ((NULL CF) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (CF) (MULTSQ CF (INVSQ CNT)))
                                  (CAR CF))
                                 NIL)))
               LOOPLABEL
                (SETQ CF (CDR CF))
                (COND ((NULL CF) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (CF) (MULTSQ CF (INVSQ CNT))) (CAR CF))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (LIST 'P (CADR POLY) NEWCOEFFS (CADDDR POLY))))) 
(PUT 'POLY_NORMALIZEBYLEAD 'NUMBER-OF-ARGS 1) 
(DE POLY_NORMALIZEBYLEAD (POLY)
    (PROG (NEWCOEFFS MULT1)
      (RETURN
       (COND ((EQUAL (SFTO_DCONTENTF (CAR (CAR (CADDR POLY)))) 1) POLY)
             (T
              (PROGN
               (SETQ MULT1 (CONS (SFTO_DCONTENTF (CAR (CAR (CADDR POLY)))) 1))
               (COND
                ((MINUSF (CAR (CAR (CADDR POLY)))) (SETQ MULT1 (NEGSQ MULT1))))
               (SETQ NEWCOEFFS
                       (PROG (CF FORALL-RESULT FORALL-ENDPTR)
                         (SETQ CF (CADDR POLY))
                         (COND ((NULL CF) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (CF)
                                             (MULTSQ CF (INVSQ MULT1)))
                                           (CAR CF))
                                          NIL)))
                        LOOPLABEL
                         (SETQ CF (CDR CF))
                         (COND ((NULL CF) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (CF) (MULTSQ CF (INVSQ MULT1)))
                                   (CAR CF))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (LIST 'P (CADR POLY) NEWCOEFFS (CADDDR POLY)))))))) 
(PUT 'POLY_NORMALIZEBYLEADPARAMETRIC 'NUMBER-OF-ARGS 1) 
(DE POLY_NORMALIZEBYLEADPARAMETRIC (POLY)
    (PROG (NEWCOEFFS MULT1)
      (RETURN
       (COND
        (((LAMBDA (A) (AND (EQN (CAR A) 1) (EQN (CDR A) 1)))
          (CAR (CADDR POLY)))
         POLY)
        (T
         (PROGN
          (COND
           (*F5PARAMETRICNORMALIZE
            (PARAM_ADDASSUMPTIONNORMALIZE (CAR (CADDR POLY)))))
          (SETQ MULT1
                  ((LAMBDA (A) (PROGN (CONS (CDR A) (CAR A))))
                   (CAR (CADDR POLY))))
          (SETQ NEWCOEFFS
                  (PROG (CF FORALL-RESULT FORALL-ENDPTR)
                    (SETQ CF (CADDR POLY))
                    (COND ((NULL CF) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (CF) (MULTSQ CF MULT1)) (CAR CF))
                                     NIL)))
                   LOOPLABEL
                    (SETQ CF (CDR CF))
                    (COND ((NULL CF) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (CF) (MULTSQ CF MULT1)) (CAR CF))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (LIST 'P (CADR POLY) NEWCOEFFS (CADDDR POLY)))))))) 
(DE POLY_SUMPOLY (F G)
    (COND ((NULL (CADR F)) G) ((NULL (CADR G)) F)
          (T
           (POLY_PAIRCOMB F
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X 1)
              (COND
               ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
              (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
             LOOPLABEL
              (SETQ X (PLUS2 X 1))
              (COND
               ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS 0 NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (NEGSQ (POLY_2COEFF 1)) G
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X 1)
              (COND
               ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X)) (RETURN NIL)))
              (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
             LOOPLABEL
              (SETQ X (PLUS2 X 1))
              (COND
               ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS 0 NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (POLY_2COEFF 1))))) 
(PUT 'POLY_SUMPOLY 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_SUMPOLY 'INLINE
      '(LAMBDA (F G)
         (COND ((NULL (CADR F)) G) ((NULL (CADR G)) F)
               (T
                (POLY_PAIRCOMB F
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X 1)
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                  LOOPLABEL
                   (SETQ X (PLUS2 X 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (NEGSQ (POLY_2COEFF 1)) G
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X 1)
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                  LOOPLABEL
                   (SETQ X (PLUS2 X 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (POLY_2COEFF 1)))))) 
(PUT 'POLY_SPOLY 'NUMBER-OF-ARGS 2) 
(DE POLY_SPOLY (F G)
    (PROG (E1 E2 ELCM MULT1 MULT2)
      (SETQ E1 (CAR (CADR F)))
      (SETQ E2 (CAR (CADR G)))
      (SETQ ELCM (POLY_ELMAXEXP E1 E2))
      (SETQ MULT1 (POLY_SUBEXP ELCM E2))
      (SETQ MULT2 (POLY_SUBEXP ELCM E1))
      (RETURN
       (POLY_PAIRCOMB
        (LIST 'P (POLY_TAILTERMS F) (POLY_TAILCOEFFS F) (CADDDR F)) MULT2
        (CAR (CADDR F))
        (LIST 'P (POLY_TAILTERMS G) (POLY_TAILCOEFFS G) (CADDDR G)) MULT1
        (CAR (CADDR G)))))) 
(PUT 'POLY_TRYTOPREDUCTIONSTEP 'NUMBER-OF-ARGS 2) 
(DE POLY_TRYTOPREDUCTIONSTEP (F G)
    (PROG (GLEAD FLEAD FMULT GMULT UPDATED)
      (SETQ GLEAD (CAR (CADR G)))
      (SETQ FLEAD (CAR (CADR F)))
      (COND
       ((POLY_DIVEXP? GLEAD FLEAD)
        (PROGN
         (COND
          (*F5PARAMETRIC (PROGN (PARAM_ADDASSUMPTIONRED (CAR (CADDR G))))))
         (SETQ FMULT
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X 1)
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                  LOOPLABEL
                   (SETQ X (PLUS2 X 1))
                   (COND
                    ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ GMULT (POLY_SUBEXP FLEAD GLEAD))
         (SETQ F
                 (POLY_PAIRCOMB
                  (LIST 'P (POLY_TAILTERMS F) (POLY_TAILCOEFFS F) (CADDDR F))
                  FMULT (CAR (CADDR F))
                  (LIST 'P (POLY_TAILTERMS G) (POLY_TAILCOEFFS G) (CADDDR G))
                  GMULT (CAR (CADDR G))))
         (SETQ UPDATED T))))
      (RETURN (CONS UPDATED F)))) 
(PUT 'POLY_TRYREDUCTIONSTEP 'NUMBER-OF-ARGS 2) 
(DE POLY_TRYREDUCTIONSTEP (F G)
    (PROG (UPDATED FTERMS FCOEFFS GLEAD FCOEF FTERM FMULT GMULT)
      (SETQ FTERMS (CADR F))
      (SETQ FCOEFFS (CADDR F))
      (SETQ GLEAD (CAR (CADR G)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT UPDATED) FTERMS)) (RETURN NIL)))
        (PROGN
         (SETQ FTERM (PROG1 (CAR FTERMS) (SETQ FTERMS (CDR FTERMS))))
         (SETQ FCOEF (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS))))
         (COND
          ((POLY_DIVEXP? GLEAD FTERM)
           (PROGN
            (COND
             (*F5PARAMETRIC (PROGN (PARAM_ADDASSUMPTIONRED (CAR (CADDR G))))))
            (SETQ FMULT
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X 1)
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                        (RETURN NIL)))
                      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                     LOOPLABEL
                      (SETQ X (PLUS2 X 1))
                      (COND
                       ((MINUSP (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) X))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ GMULT (POLY_SUBEXP FTERM GLEAD))
            (SETQ F (POLY_PAIRCOMB F FMULT FCOEF G GMULT (CAR (CADDR G))))
            (SETQ UPDATED T)))))
        (GO WHILELABEL))
      (RETURN (CONS UPDATED F)))) 
(PUT 'POLY_COMMONDENOMINATOR 'NUMBER-OF-ARGS 1) 
(DE POLY_COMMONDENOMINATOR (F)
    (PROG (FCOEFFS DEN)
      (SETQ DEN 1)
      (SETQ FCOEFFS (CADDR F))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCOEFFS) (RETURN NIL)))
        (PROGN
         (SETQ DEN
                 (LCM DEN
                      (CDR
                       (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))))))
        (GO WHILELABEL))
      (RETURN (CONS DEN 1)))) 
(PUT 'POLY_CONTENTPARAMETRIC 'NUMBER-OF-ARGS 1) 
(DE POLY_CONTENTPARAMETRIC (F)
    (PROG (FCOEFFS CNT)
      (SETQ CNT (CAR (CAR (CADDR F))))
      (SETQ FCOEFFS (CDR (CADDR F)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCOEFFS) (RETURN NIL)))
        (PROGN
         (SETQ CNT
                 (GCDF CNT
                       (CAR
                        (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))))))
        (GO WHILELABEL))
      (SETQ CNT (CONS CNT 1))
      (RETURN (COND ((MINUSF (CAR CNT)) (NEGSQ CNT)) (T CNT))))) 
(PUT 'POLY_CONTENT 'NUMBER-OF-ARGS 1) 
(DE POLY_CONTENT (F)
    (PROG (FCOEFFS CNT)
      (SETQ CNT (SFTO_DCONTENTF (CAR (CAR (CADDR F)))))
      (SETQ FCOEFFS (CDR (CADDR F)))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCOEFFS) (RETURN NIL)))
        (PROGN
         (SETQ CNT
                 (GCDF CNT
                       (SFTO_DCONTENTF
                        (CAR
                         (PROG1 (CAR FCOEFFS)
                           (SETQ FCOEFFS (CDR FCOEFFS))))))))
        (GO WHILELABEL))
      (SETQ CNT (CONS CNT 1))
      (RETURN (COND ((MINUSF (CAR CNT)) (NEGSQ CNT)) (T CNT))))) 
(PUT 'POLY_SCALEDENOMINATORS 'NUMBER-OF-ARGS 1) 
(DE POLY_SCALEDENOMINATORS (F)
    (PROG (FCOEFFS NEWCOEFFS DEN)
      (SETQ DEN (POLY_COMMONDENOMINATOR F))
      (COND
       (*F5PARAMETRIC
        (PROGN
         (COND
          ((NOT (PARAM_ISCONSTASSUMPTION DEN))
           (PROGN
            (LPRIM (LIST (PREPSQ DEN) "assumed to be non-vanishing"))
            (PARAM_ADDASSUMPTIONINPUT DEN)))))))
      (SETQ FCOEFFS (CADDR F))
      (PROG ()
       WHILELABEL
        (COND ((NOT FCOEFFS) (RETURN NIL)))
        (PROGN
         (PROG (W1)
           (SETQ W1
                   (MULTSQ DEN
                           (PROG1 (CAR FCOEFFS) (SETQ FCOEFFS (CDR FCOEFFS)))))
           (SETQ NEWCOEFFS (CONS W1 NEWCOEFFS))
           (RETURN W1)))
        (GO WHILELABEL))
      (RETURN (LIST 'P (CADR F) (REVERSIP NEWCOEFFS) (CADDDR F))))) 
(DE POLY_CMPPOLYLEAD (POLY1 POLY2)
    (POLY_CMPEXP (CAR (CADR POLY1)) (CAR (CADR POLY2)))) 
(PUT 'POLY_CMPPOLYLEAD 'NUMBER-OF-ARGS 2) 
(PUTC 'POLY_CMPPOLYLEAD 'INLINE
      '(LAMBDA (POLY1 POLY2)
         (POLY_CMPEXP (CAR (CADR POLY1)) (CAR (CADR POLY2))))) 
(PUT 'POLY_LEADTOTALDEGREECMP 'NUMBER-OF-ARGS 2) 
(DE POLY_LEADTOTALDEGREECMP (POLY1 POLY2)
    (PROG (T1 T2)
      (SETQ T1 0)
      (SETQ T2 0)
      (SETQ T1 (CAR (CAR (CADR POLY1))))
      (SETQ T2 (CAR (CAR (CADR POLY2))))
      (RETURN
       (COND
        ((IEQUAL T1 T2) (POLY_CMPEXP (CAR (CADR POLY1)) (CAR (CADR POLY2))))
        (T (ILESSP T1 T2)))))) 
(PUT 'POLY_ITHVARIABLE 'NUMBER-OF-ARGS 2) 
(DE POLY_ITHVARIABLE (IDX DEG)
    (CONS DEG
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X 1)
            (COND
             ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) 1) X))
              (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS (COND ((EQUAL X IDX) DEG) (T 0)) NIL)))
           LOOPLABEL
            (SETQ X (PLUS2 X 1))
            (COND
             ((MINUSP (DIFFERENCE (DIFFERENCE (LENGTH GLOBAL-DIPVARS*) 1) X))
              (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS (COND ((EQUAL X IDX) DEG) (T 0)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(ENDMODULE) 