(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ECONOMISE)) 
(CREATE-PACKAGE '(ECONOMISE) '(NUMERIC)) 
(FLUID '(CHEBYSHEV_POLYNOMIALS)) 
(SETQ CHEBYSHEV_POLYNOMIALS
        (LIST (CONS 0 1) (CONS 1 (CONS (CONS (GETPOWER (FKERN 'X) 1) 1) NIL)))) 
(PUT 'GET_CHEB 'NUMBER-OF-ARGS 1) 
(PUT 'GET_CHEB 'DEFINED-ON-LINE '108) 
(PUT 'GET_CHEB 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'GET_CHEB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_CHEB (N)
    (PROG (W)
      (SETQ W (ASSOC N CHEBYSHEV_POLYNOMIALS))
      (COND
       ((NULL W)
        (PROG (A B)
          (SETQ A (GET_CHEB (DIFFERENCE N 2)))
          (SETQ B (GET_CHEB (DIFFERENCE N 1)))
          (SETQ W
                  (CONS N
                        (ADDF
                         ((LAMBDA (G123)
                            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G123 B))
                                  (T (POLY-MULTF G123 B))))
                          (CONS (CONS (GETPOWER (FKERN 'X) 1) 2) NIL))
                         (NEGF A))))
          (SETQ CHEBYSHEV_POLYNOMIALS (CONS W CHEBYSHEV_POLYNOMIALS)))))
      (RETURN (CDR W)))) 
(PUT 'GET_SHIFTED_CHEB 'NUMBER-OF-ARGS 3) 
(PUT 'GET_SHIFTED_CHEB 'DEFINED-ON-LINE '125) 
(PUT 'GET_SHIFTED_CHEB 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'GET_SHIFTED_CHEB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GET_SHIFTED_CHEB (N LO HI)
    (PROG (P NEWX W)
      (SETQ P (CONS (GET_CHEB N) 1))
      (SETQ NEWX
              (MULTSQ
               (ADDSQ (CONS (CONS (CONS (GETPOWER (FKERN 'X) 1) 2) NIL) 1)
                      (NEGSQ (ADDSQ LO HI)))
               (INVSQ (ADDSQ HI (NEGSQ LO)))))
      (SETQ P (SUBSQ P (LIST (CONS 'X (PREPSQ NEWX)))))
      (SETQ W (CDR P))
      (SETQ P (CDR (COEFF1 (MK*SQ (CONS (CAR P) 1)) 'X NIL)))
      (COND
       ((NEQ W 1)
        (PROGN
         (SETQ W (PREPF W))
         (SETQ P
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C P)
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (PREPSQ (SIMP (LIST 'QUOTIENT C W))))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C) (PREPSQ (SIMP (LIST 'QUOTIENT C W))))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (RETURN P))) 
(PUT 'LOSE_TRAILING_ZEROES 'NUMBER-OF-ARGS 1) 
(PUT 'LOSE_TRAILING_ZEROES 'DEFINED-ON-LINE '143) 
(PUT 'LOSE_TRAILING_ZEROES 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'LOSE_TRAILING_ZEROES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOSE_TRAILING_ZEROES (L)
    (PROG (R)
      (SETQ R (REVERSE L))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQCAR R 0)) (RETURN NIL)))
        (SETQ R (CDR R))
        (GO WHILELABEL))
      (RETURN (REVERSE R)))) 
(PUT 'SUBTRACT_MULTIPLE 'NUMBER-OF-ARGS 3) 
(PUT 'SUBTRACT_MULTIPLE 'DEFINED-ON-LINE '151) 
(PUT 'SUBTRACT_MULTIPLE 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'SUBTRACT_MULTIPLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBTRACT_MULTIPLE (SER C TI)
    (COND ((NULL SER) NIL)
          (T
           (CONS
            (PREPSQ
             (SIMP (LIST 'DIFFERENCE (CAR SER) (LIST 'TIMES C (CAR TI)))))
            (SUBTRACT_MULTIPLE (CDR SER) C (CDR TI)))))) 
(PUT 'COLLECT_EVENS 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECT_EVENS 'DEFINED-ON-LINE '160) 
(PUT 'COLLECT_EVENS 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'COLLECT_EVENS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECT_EVENS (U)
    (COND ((NULL U) NIL) ((NULL (CDR U)) (LIST (CAR U)))
          ((NOT (ZEROP (CADR U)))
           (REDERR "series does not have alternate terms zero"))
          (T (CONS (CAR U) (COLLECT_EVENS (CDDR U)))))) 
(FLUID '(|PS:EXP-LIM|)) 
(PUT 'ECONOMISE_SERIES 'NUMBER-OF-ARGS 1) 
(PUT 'ECONOMISE_SERIES 'DEFINED-ON-LINE '174) 
(PUT 'ECONOMISE_SERIES 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'ECONOMISE_SERIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ECONOMISE_SERIES (U)
    (PROG (SER VAR LO HI EPS NTERMS STOP EVENODD W W1 DEG TI ERR OLDERR)
      (TERPRI)
      (COND
       ((LESSP (LENGTH U) 3)
        (REDERR "economise_series(series, var, precision)")))
      (SETQ SER (SIMP (CAR U)))
      (SETQ LO (CONS (MINUS 1) 1))
      (SETQ HI (CONS 1 1))
      (SETQ VAR (CADR U))
      (COND
       ((AND (EQCAR VAR 'EQUAL) (EQCAR (CADDR VAR) '*INTERVAL*))
        (PROGN
         (SETQ W (CADDR VAR))
         (SETQ VAR (CADR VAR))
         (SETQ LO (SIMP (CADR W)))
         (SETQ HI (SIMP (CADDR W))))))
      (SETQ VAR (SIMP VAR))
      (SETQ W1 *ROUNDED)
      (COND ((NOT W1) (ON (LIST 'ROUNDED))))
      (SETQ W (PRECISION1 6 NIL))
      (SETQ EPS (PREPSQ (SIMP (CADDR U))))
      (PRECISION1 W NIL)
      (COND ((NOT W1) (OFF (LIST 'ROUNDED))))
      (COND ((EQCAR EPS '|:RD:|) (SETQ EPS (CDR EPS))))
      (COND ((GREATERP EPS 1.0) (PROGN (SETQ NTERMS (FIX EPS)) (SETQ EPS NIL)))
            (T (SETQ NTERMS 1)))
      (SETQ U (CDDDR U))
      (COND
       ((OR (EQCAR U 'EVEN_TERMS) (EQCAR U 'ODD_TERMS))
        (SETQ EVENODD (CAR U))))
      (SETQ W (CAR SER))
      (COND
       ((AND (NULL (CDR W)) (ONEP (CDAAR W)) (EQCAR (CAAAR W) 'TAYLOR*))
        (PROGN
         (SETQ W1 (SIMP (TAYLORTOSTANDARD (CAAAR W))))
         (SETQ SER (MULTSQ W1 (CONS (CDAR W) (CDR SER)))))))
      (COND
       ((EQCAR W '|:PS:|)
        (PROGN (SETQ W1 (|PREP:PS| W |PS:EXP-LIM|)) (SETQ SER (SIMP W1)))))
      (SETQ W (CDR SER))
      (SETQ SER (CDR (COEFF1 (MK*SQ (CONS (CAR SER) 1)) (MK*SQ VAR) NIL)))
      (COND
       ((NEQ W 1)
        (PROGN
         (SETQ W (PREPF W))
         (SETQ SER
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C SER)
                   (COND ((NULL C) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (PREPSQ (SIMP (LIST 'QUOTIENT C W))))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C) (PREPSQ (SIMP (LIST 'QUOTIENT C W))))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (SETQ SER (LOSE_TRAILING_ZEROES SER))
      (SETQ OLDERR 0.0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (GREATERP (SETQ DEG (DIFFERENCE (LENGTH SER) 1)) NTERMS)
                (NOT STOP)))
          (RETURN NIL)))
        (PROGN
         (SETQ TI (GET_SHIFTED_CHEB DEG LO HI))
         (SETQ ERR
                 (LIST 'QUOTIENT (CAR (LASTPAIR SER))
                       (EXPT 2 (DIFFERENCE DEG 1))))
         (SETQ W1 *ROUNDED)
         (COND ((NOT W1) (ON (LIST 'ROUNDED))))
         (SETQ W (PRECISION1 6 NIL))
         (SETQ ERR (PREPSQ (SIMP ERR)))
         (PRECISION1 W NIL)
         (COND ((NOT W1) (OFF (LIST 'ROUNDED))))
         (COND ((EQCAR ERR 'MINUS) (SETQ ERR (CADR ERR))))
         (COND ((EQCAR ERR '|:RD:|) (SETQ ERR (ABS (CDR ERR)))))
         (COND
          ((OR (NOT (NUMBERP ERR)) (NOT (NUMBERP EPS)) (LESSP ERR EPS))
           (PROGN
            (SETQ OLDERR ERR)
            (SETQ W (LIST 'QUOTIENT (CAR (LASTPAIR SER)) (CAR (LASTPAIR TI))))
            (SETQ SER (SUBTRACT_MULTIPLE SER W TI))
            (SETQ SER (LOSE_TRAILING_ZEROES SER))))
          (T (SETQ STOP T))))
        (GO WHILELABEL))
      (COND
       ((AND (NOT STOP) (NUMBERP OLDERR))
        (PROGN (TERPRI) (PRIN2 "Error estimate: ") (PRINT OLDERR))))
      (COND ((EQUAL EVENODD 'EVEN_TERMS) (SETQ SER (COLLECT_EVENS SER)))
            ((EQUAL EVENODD 'ODD_TERMS)
             (PROGN
              (COND
               ((NOT (ZEROP (CAR SER)))
                (REDERR
                 "series was supposed to be odd but has a constant term")))
              (SETQ SER (COLLECT_EVENS (CDR SER))))))
      (RETURN (CONS 'LIST SER)))) 
(PUT 'ECONOMISE_SERIES 'PSOPFN 'ECONOMISE_SERIES) 
(ARRAYFN 'SYMBOLIC
         (LIST (LIST '~PADE_XX 100) (LIST '~PADE_A 100) (LIST '~PADE_P 100)
               (LIST '~PADE_Q 100))) 
(FLUID '(SAVE_PRECISION SAVE_ROUNDED)) 
(PUT 'MULTIPOINT_PADE 'NUMBER-OF-ARGS 4) 
(FLAG '(MULTIPOINT_PADE) 'OPFN) 
(PUT 'MULTIPOINT_PADE 'DEFINED-ON-LINE '337) 
(PUT 'MULTIPOINT_PADE 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'MULTIPOINT_PADE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTIPOINT_PADE (F U V ACCURACY)
    (PROG (N NLIMIT TARGETERR STOPONERR WORST ERR RES)
      (COND
       ((EVALGREATERP (AEVAL ACCURACY) 1)
        (PROGN
         (SETQ N (AEVAL (LIST 'FIX ACCURACY)))
         (COND
          ((EVALGREATERP (AEVAL N) 100)
           (AEVAL
            (REDERR
             (REVALX "multipoint_pade precision must be at most 100")))))
         (SETQ STOPONERR (AEVAL 0))))
       (T
        (PROGN
         (SETQ N (AEVAL 2))
         (SETQ TARGETERR (AEVAL ACCURACY))
         (SETQ WORST (AEVAL (LIST 'PLUS TARGETERR 1)))
         (SETQ STOPONERR (AEVAL 1)))))
      (COND
       ((EVALGREATERP (AEVAL N) 50) (RETURN (AEVAL "N too large, sorry"))))
      (SETQ SAVE_ROUNDED *ROUNDED)
      (SETQ SAVE_PRECISION (PRECISION (TIMES 2 (PRECISION 0))))
      (AEVAL (ON (LIST 'ROUNDED)))
      (WHILE
       (OR
        (AND (EVALEQUAL (AEVAL* STOPONERR) 0)
             (EVALLEQ (AEVAL* N) (AEVAL* (LIST 'FIX ACCURACY))))
        (AND (EVALEQUAL (AEVAL* STOPONERR) 1)
             (EVALGREATERP (AEVAL* WORST) (AEVAL* TARGETERR))
             (EVALLESSP (AEVAL* N) 100)))
       (PROGN
        (PROG (I)
          (SETQ I 0)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
            (RETURN NIL)))
          (SETEL (LIST '~PADE_XX I)
                 (AEVAL*
                  (LIST 'PLUS U
                        (LIST 'TIMES I
                              (LIST 'QUOTIENT (LIST 'DIFFERENCE V U)
                                    (LIST 'DIFFERENCE N 1))))))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB))
        (SETEL (LIST '~PADE_A 0) (AEVAL* (LIST F (LIST '~PADE_XX 0))))
        (SETEL (LIST '~PADE_A 1)
               (AEVAL*
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE (LIST '~PADE_XX 1) (LIST '~PADE_XX 0))
                      (LIST 'DIFFERENCE (LIST F (LIST '~PADE_XX 1))
                            (LIST '~PADE_A 0)))))
        (SETEL (LIST '~PADE_P 0) (AEVAL* (LIST '~PADE_A 0)))
        (SETEL (LIST '~PADE_Q 0) (AEVAL* 1))
        (SETEL (LIST '~PADE_P 1)
               (AEVAL*
                (LIST 'PLUS (LIST 'TIMES (LIST '~PADE_A 0) (LIST '~PADE_A 1))
                      (LIST 'DIFFERENCE 'X (LIST '~PADE_XX 0)))))
        (SETEL (LIST '~PADE_Q 1) (AEVAL* (LIST '~PADE_A 1)))
        (PROG (I)
          (SETQ I 2)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE N 1)) I))
            (RETURN NIL)))
          (PROGN
           (SETEL (LIST '~PADE_A I)
                  (AEVAL*
                   (LIST 'TIMES
                         (LIST 'DIFFERENCE (LIST '~PADE_XX I)
                               (LIST '~PADE_XX (DIFFERENCE I 1)))
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST F (LIST '~PADE_XX I))
                                           (LIST 'SUB
                                                 (LIST 'EQUAL 'X
                                                       (LIST '~PADE_XX I))
                                                 (LIST '~PADE_Q
                                                       (DIFFERENCE I 2))))
                                     (LIST 'SUB
                                           (LIST 'EQUAL 'X (LIST '~PADE_XX I))
                                           (LIST '~PADE_P (DIFFERENCE I 2))))
                               (LIST 'DIFFERENCE
                                     (LIST 'SUB
                                           (LIST 'EQUAL 'X (LIST '~PADE_XX I))
                                           (LIST '~PADE_P (DIFFERENCE I 1)))
                                     (LIST 'TIMES (LIST F (LIST '~PADE_XX I))
                                           (LIST 'SUB
                                                 (LIST 'EQUAL 'X
                                                       (LIST '~PADE_XX I))
                                                 (LIST '~PADE_Q
                                                       (DIFFERENCE I 1)))))))))
           (SETEL (LIST '~PADE_P I)
                  (AEVAL*
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST '~PADE_A I)
                               (LIST '~PADE_P (DIFFERENCE I 1)))
                         (LIST 'TIMES
                               (LIST 'DIFFERENCE 'X
                                     (LIST '~PADE_XX (DIFFERENCE I 1)))
                               (LIST '~PADE_P (DIFFERENCE I 2))))))
           (SETEL (LIST '~PADE_Q I)
                  (AEVAL*
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST '~PADE_A I)
                               (LIST '~PADE_Q (DIFFERENCE I 1)))
                         (LIST 'TIMES
                               (LIST 'DIFFERENCE 'X
                                     (LIST '~PADE_XX (DIFFERENCE I 1)))
                               (LIST '~PADE_Q (DIFFERENCE I 2))))))
           (SETQ WORST (AEVAL* 0))
           (PROG (J)
             (SETQ J 0)
            LAB
             (COND
              ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'TIMES 50 N)) J))
               (RETURN NIL)))
             (PROGN
              (SETK 'Z
                    (AEVAL*
                     (LIST 'PLUS U
                           (LIST 'TIMES J
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE V U)
                                       (LIST 'TIMES 50 N))))))
              (SETQ ERR
                      (AEVAL*
                       (LIST 'ABS
                             (LIST 'DIFFERENCE (LIST F 'Z)
                                   (LIST 'QUOTIENT
                                         (LIST 'SUB (LIST 'EQUAL 'X 'Z)
                                               (LIST '~PADE_P I))
                                         (LIST 'SUB (LIST 'EQUAL 'X 'Z)
                                               (LIST '~PADE_Q I)))))))
              (COND
               ((EVALGREATERP (AEVAL* ERR) (AEVAL* WORST))
                (SETQ WORST (AEVAL* ERR)))))
             (SETQ J
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      J))
             (GO LAB)))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB))
        (COND
         ((EVALEQUAL (AEVAL* STOPONERR) 1)
          (PROGN
           (AEVAL* (LIST 'PRECISION 6))
           (ASSGNPRI "With N=" NIL NIL)
           (ASSGNPRI N NIL NIL)
           (ASSGNPRI " error = " NIL NIL)
           (ASSGNPRI WORST NIL NIL)
           (AEVAL* (TERPRI* NIL))
           (AEVAL* (PRECISION (TIMES 2 SAVE_PRECISION))))))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
      (AEVAL (LIST 'PRECISION 6))
      (PROGN
       (ASSGNPRI (AEVAL "Order ") NIL 'FIRST)
       (ASSGNPRI (AEVAL (LIST 'DIFFERENCE N 1)) NIL NIL)
       (ASSGNPRI (AEVAL "  Worst error = ") NIL NIL)
       (ASSGNPRI (AEVAL WORST) NIL 'LAST))
      (PRECISION SAVE_PRECISION)
      (SETK 'LEAD
            (AEVAL
             (LIST 'SUB (LIST 'EQUAL 'X 0)
                   (LIST '~PADE_Q (LIST 'DIFFERENCE N 2)))))
      (SETQ RES
              (AEVAL
               (LIST 'LIST
                     (LIST 'COEFF
                           (LIST 'QUOTIENT
                                 (LIST '~PADE_P (LIST 'DIFFERENCE N 2)) 'LEAD)
                           'X)
                     (LIST 'COEFF
                           (LIST 'QUOTIENT
                                 (LIST '~PADE_Q (LIST 'DIFFERENCE N 2)) 'LEAD)
                           'X))))
      (COND ((NOT SAVE_ROUNDED) (OFF (LIST 'ROUNDED))))
      (RETURN (AEVAL RES)))) 
(PUT 'EVALUATE_PADE 'NUMBER-OF-ARGS 2) 
(FLAG '(EVALUATE_PADE) 'OPFN) 
(PUT 'EVALUATE_PADE 'DEFINED-ON-LINE '407) 
(PUT 'EVALUATE_PADE 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'EVALUATE_PADE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALUATE_PADE (R X)
    (PROG (P Q)
      (SETQ P (AEVAL 0))
      (SETQ Q (AEVAL 0))
      (PROG (V)
        (SETQ V (GETRLIST (AEVAL (LIST 'REVERSE (LIST 'FIRST R)))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ P (AEVAL (LIST 'PLUS (LIST 'TIMES P X) V))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (V)
        (SETQ V (GETRLIST (AEVAL (LIST 'REVERSE (LIST 'SECOND R)))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ Q (AEVAL (LIST 'PLUS (LIST 'TIMES Q X) V))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (AEVAL (LIST 'QUOTIENT P Q))))) 
(PUT 'NUM_PADE 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_PADE) 'OPFN) 
(PUT 'NUM_PADE 'DEFINED-ON-LINE '415) 
(PUT 'NUM_PADE 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'NUM_PADE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_PADE (R X)
    (PROG (P)
      (SETQ P (AEVAL 0))
      (PROG (V)
        (SETQ V (GETRLIST (AEVAL (LIST 'REVERSE (LIST 'FIRST R)))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ P (AEVAL (LIST 'PLUS (LIST 'TIMES P X) V))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (AEVAL P)))) 
(PUT 'DEN_PADE 'NUMBER-OF-ARGS 2) 
(FLAG '(DEN_PADE) 'OPFN) 
(PUT 'DEN_PADE 'DEFINED-ON-LINE '422) 
(PUT 'DEN_PADE 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'DEN_PADE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEN_PADE (R X)
    (PROG (Q)
      (SETQ Q (AEVAL 0))
      (PROG (V)
        (SETQ V (GETRLIST (AEVAL (LIST 'REVERSE (LIST 'SECOND R)))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ Q (AEVAL (LIST 'PLUS (LIST 'TIMES Q X) V))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (AEVAL Q)))) 
(PUT 'PRINHEXLIT 'NUMBER-OF-ARGS 1) 
(FLAG '(PRINHEXLIT) 'OPFN) 
(PUT 'PRINHEXLIT 'DEFINED-ON-LINE '454) 
(PUT 'PRINHEXLIT 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'PRINHEXLIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINHEXLIT (~PADE_XX)
    (PROG (X BX HI LO IX ERR SAVE_ROUNDED)
      (SETQ SAVE_ROUNDED *ROUNDED)
      (AEVAL (ON (LIST 'ROUNDED)))
      (COND
       ((EVALEQUAL (AEVAL ~PADE_XX) (AEVAL '(|:DN:| 0 . -1)))
        (AEVAL (PRIN2 "0.0")))
       (T
        (PROGN
         (SETQ BX (AEVAL 0))
         (SETQ HI (AEVAL (LIST 'EXPT '(|:DN:| 20 . -1) 53)))
         (SETQ LO (AEVAL (LIST 'EXPT '(|:DN:| 20 . -1) 52)))
         (SETQ X (AEVAL ~PADE_XX))
         (COND
          ((EVALLESSP (AEVAL X) 0)
           (PROGN (AEVAL (PRIN2 "-")) (SETQ X (AEVAL (LIST 'MINUS X))))))
         (WHILE (EVALLESSP (AEVAL* X) (AEVAL* LO))
                (PROGN
                 (SETQ X (AEVAL* (LIST 'PLUS X X)))
                 (SETQ BX (AEVAL* (LIST 'DIFFERENCE BX 1)))))
         (WHILE (EVALGEQ (AEVAL* X) (AEVAL* HI))
                (PROGN
                 (SETQ X (AEVAL* (LIST 'QUOTIENT X 2)))
                 (SETQ BX (AEVAL* (LIST 'PLUS BX 1)))))
         (SETQ IX (AEVAL (LIST 'FIX X)))
         (SETQ ERR (AEVAL (LIST 'DIFFERENCE X IX)))
         (COND
          ((EVALGEQ (AEVAL ERR) (AEVAL '(|:DN:| 5 . -1)))
           (SETQ IX (AEVAL (LIST 'PLUS IX 1))))
          ((EVALLESSP (AEVAL ERR) (AEVAL (LIST 'MINUS '(|:DN:| 5 . -1))))
           (SETQ IX (AEVAL (LIST 'DIFFERENCE IX 1)))))
         (AEVAL (PRIN2 "0x"))
         (AEVAL (PRIN2 (HEX IX)))
         (AEVAL (PRIN2 "p"))
         (AEVAL (PRIN2 BX))
         (COND
          ((EVALLESSP (AEVAL ~PADE_XX) 0) (SETQ IX (AEVAL (LIST 'MINUS IX)))))
         (SETQ ~PADE_XX
                 (AEVAL
                  (LIST 'DIFFERENCE ~PADE_XX
                        (LIST 'TIMES IX (LIST 'EXPT '(|:DN:| 20 . -1) BX)))))
         (COND ((NOT SAVE_ROUNDED) (OFF (LIST 'ROUNDED)))))))
      (RETURN (AEVAL ~PADE_XX)))) 
(PUT 'PRINHEXLITLIST 'NUMBER-OF-ARGS 1) 
(FLAG '(PRINHEXLITLIST) 'OPFN) 
(PUT 'PRINHEXLITLIST 'DEFINED-ON-LINE '488) 
(PUT 'PRINHEXLITLIST 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'PRINHEXLITLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINHEXLITLIST (L)
    (PROG ()
      (TERPRI)
      (PRIN2 "{")
      (WHILE (EVALNEQ (AEVAL* (LIST 'LENGTH L)) 0)
             (PROGN
              (AEVAL* (TTAB 4))
              (AEVAL* (LIST 'PRINHEXLIT (LIST 'FIRST L)))
              (COND
               ((EVALNEQ (AEVAL* (LIST 'LENGTH L)) 1) (AEVAL* (PRIN2 ","))))
              (SETQ L (AEVAL* (LIST 'REST L)))
              (AEVAL* (TERPRI))))
      (PRIN2 "}")
      (TERPRI))) 
(PUT 'PRINHEXLITLIST2 'NUMBER-OF-ARGS 1) 
(FLAG '(PRINHEXLITLIST2) 'OPFN) 
(PUT 'PRINHEXLITLIST2 'DEFINED-ON-LINE '502) 
(PUT 'PRINHEXLITLIST2 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'PRINHEXLITLIST2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINHEXLITLIST2 (L)
    (PROG (W)
      (TERPRI)
      (PRIN2 "{")
      (WHILE (EVALNEQ (AEVAL* (LIST 'LENGTH L)) 0)
             (PROGN
              (AEVAL* (TTAB 4))
              (AEVAL* (PRIN2 "{"))
              (SETQ W (AEVAL* (LIST 'PRINHEXLIT (LIST 'FIRST L))))
              (AEVAL* (PRIN2 ", "))
              (AEVAL* (LIST 'PRINHEXLIT W))
              (AEVAL* (PRIN2 "}"))
              (COND
               ((EVALNEQ (AEVAL* (LIST 'LENGTH L)) 1) (AEVAL* (PRIN2 ","))))
              (SETQ L (AEVAL* (LIST 'REST L)))
              (AEVAL* (TERPRI))))
      (PRIN2 "}")
      (TERPRI))) 
(PUT 'PRINHEXLITLIST3 'NUMBER-OF-ARGS 1) 
(FLAG '(PRINHEXLITLIST3) 'OPFN) 
(PUT 'PRINHEXLITLIST3 'DEFINED-ON-LINE '521) 
(PUT 'PRINHEXLITLIST3 'DEFINED-IN-FILE 'NUMERIC/ECONOMISE.RED) 
(PUT 'PRINHEXLITLIST3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINHEXLITLIST3 (L)
    (PROG (W)
      (TERPRI)
      (PRIN2 "{")
      (WHILE (EVALNEQ (AEVAL* (LIST 'LENGTH L)) 0)
             (PROGN
              (AEVAL* (TTAB 4))
              (AEVAL* (PRIN2 "{"))
              (SETQ W (AEVAL* (LIST 'PRINHEXLIT (LIST 'FIRST L))))
              (AEVAL* (PRIN2 ", "))
              (SETQ W (AEVAL* (LIST 'PRINHEXLIT W)))
              (AEVAL* (PRIN2 ", "))
              (AEVAL* (LIST 'PRINHEXLIT W))
              (AEVAL* (PRIN2 "}"))
              (COND
               ((EVALNEQ (AEVAL* (LIST 'LENGTH L)) 1) (AEVAL* (PRIN2 ","))))
              (SETQ L (AEVAL* (LIST 'REST L)))
              (AEVAL* (TERPRI))))
      (PRIN2 "}")
      (TERPRI))) 
(ENDMODULE) 