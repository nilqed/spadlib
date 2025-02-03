(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFPOLYS)) 
(FLUID '(POWLIS1*)) 
(FLAG
 '(EULERP BERNOULLIP HERMITEP JACOBIP CHEBYSHEVT CHEBYSHEVU GEGENBAUERP
   LEGENDREP LAGUERREP)
 'SPECFN) 
(DEFLIST
 '((EULERP 2) (BERNOULLIP 2) (HEMITEP 2) (CHEBYSHEVT 2) (CHEBYSHEVU 2)
   (GEGENBAUERP 3) (JACOBIP 4) (LEGENDREP (2 3)) (LAGUERREP (2 3)))
 'NUMBER-OF-ARGS) 
(AEVAL (OPERATOR (LIST 'BERNOULLIP))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (BERNOULLIP (~ N) 0)
      (WHEN (BERNOULLI N) (AND (FIXP N) (GEQ N 0))))
     (REPLACEBY (BERNOULLIP (~ N) (~ X))
      (WHEN
       (PROG (K FORALL-RESULT)
         (SETQ K 0)
         (SETQ FORALL-RESULT 0)
        LAB1
         (COND
          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'N) K))
           (RETURN FORALL-RESULT)))
         (SETQ FORALL-RESULT
                 (AEVAL*
                  (LIST 'PLUS
                        (AEVAL*
                         (LIST 'TIMES (LIST 'BINOMIAL 'N K) (LIST 'BERNOULLI K)
                               (LIST 'EXPT 'X (LIST 'DIFFERENCE 'N K))))
                        FORALL-RESULT)))
         (SETQ K
                 ((LAMBDA (FORALL-RESULT)
                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                  K))
         (GO LAB1))
       (AND (FIXP N) (GEQ N 0)))))))) 
(AEVAL (OPERATOR (LIST 'EULERP))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (EULERP (~ N) (QUOTIENT 1 2))
      (WHEN (QUOTIENT (EULER N) (EXPT 2 N)) (AND (FIXP N) (GEQ N 0))))
     (REPLACEBY (EULERP (~ N) (~ X))
      (WHEN
       (PROG (K FORALL-RESULT)
         (SETQ K 0)
         (SETQ FORALL-RESULT 0)
        LAB1
         (COND
          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'N) K))
           (RETURN FORALL-RESULT)))
         (SETQ FORALL-RESULT
                 (AEVAL*
                  (LIST 'PLUS
                        (AEVAL*
                         (LIST 'TIMES (LIST 'BINOMIAL 'N K)
                               (LIST 'QUOTIENT (LIST 'EULER K)
                                     (LIST 'EXPT 2 K))
                               (LIST 'EXPT
                                     (LIST 'DIFFERENCE 'X (LIST 'QUOTIENT 1 2))
                                     (LIST 'DIFFERENCE 'N K))))
                        FORALL-RESULT)))
         (SETQ K
                 ((LAMBDA (FORALL-RESULT)
                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                  K))
         (GO LAB1))
       (AND (FIXP N) (GEQ N 0)))))))) 
(PUT 'MONOMIAL_BASE 'NUMBER-OF-ARGS 2) 
(FLAG '(MONOMIAL_BASE) 'OPFN) 
(PUT 'MONOMIAL_BASE 'DEFINED-ON-LINE '79) 
(PUT 'MONOMIAL_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'MONOMIAL_BASE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MONOMIAL_BASE (X N)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 0)
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS (AEVAL* (LIST 'EXPT X I)) NIL)))
     LOOPLABEL
      (SETQ I
              ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
               I))
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
        (RETURN (CONS 'LIST FORALL-RESULT))))
      (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'EXPT X I)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'TRIGONOMETRIC_BASE 'NUMBER-OF-ARGS 2) 
(FLAG '(TRIGONOMETRIC_BASE) 'OPFN) 
(PUT 'TRIGONOMETRIC_BASE 'DEFINED-ON-LINE '82) 
(PUT 'TRIGONOMETRIC_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'TRIGONOMETRIC_BASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRIGONOMETRIC_BASE (X N)
    (LIST 'CONS 1
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I 1)
           STARTOVER
            (COND
             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
              (RETURN (MAKELIST NIL))))
            (SETQ FORALL-RESULT
                    (AEVAL*
                     (LIST 'LIST (LIST 'SIN (LIST 'TIMES I X))
                           (LIST 'COS (LIST 'TIMES I X)))))
            (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
            (SETQ I
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     I))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND
             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
              (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (GETRLIST
                     (AEVAL*
                      (LIST 'LIST (LIST 'SIN (LIST 'TIMES I X))
                            (LIST 'COS (LIST 'TIMES I X))))))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ I
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     I))
            (GO LOOPLABEL)))) 
(PUT 'BERNSTEIN_BASE 'NUMBER-OF-ARGS 2) 
(FLAG '(BERNSTEIN_BASE) 'OPFN) 
(PUT 'BERNSTEIN_BASE 'DEFINED-ON-LINE '85) 
(PUT 'BERNSTEIN_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'BERNSTEIN_BASE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BERNSTEIN_BASE (X N)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 0)
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       (AEVAL*
                        (LIST 'TIMES (LIST 'BINOMIAL N I)
                              (LIST 'EXPT (LIST 'DIFFERENCE 1 X)
                                    (LIST 'DIFFERENCE N I))
                              (LIST 'EXPT X I)))
                       NIL)))
     LOOPLABEL
      (SETQ I
              ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
               I))
      (COND
       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
        (RETURN (CONS 'LIST FORALL-RESULT))))
      (RPLACD FORALL-ENDPTR
              (CONS
               (AEVAL*
                (LIST 'TIMES (LIST 'BINOMIAL N I)
                      (LIST 'EXPT (LIST 'DIFFERENCE 1 X)
                            (LIST 'DIFFERENCE N I))
                      (LIST 'EXPT X I)))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'LEGENDRE_BASE 'NUMBER-OF-ARGS 4) 
(FLAG '(LEGENDRE_BASE) 'OPFN) 
(PUT 'LEGENDRE_BASE 'DEFINED-ON-LINE '89) 
(PUT 'LEGENDRE_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'LEGENDRE_BASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LEGENDRE_BASE (X N A B)
    (LIST 'LEGENDRE_BASE1 X N
          (LIST 'LIST
                (LIST 'PLUS
                      (LIST 'DIFFERENCE (LIST 'QUOTIENT A 2)
                            (LIST 'QUOTIENT B 2))
                      (LIST 'TIMES
                            (LIST 'PLUS 1 (LIST 'QUOTIENT A 2)
                                  (LIST 'QUOTIENT B 2))
                            X))
                1)
          1 A B)) 
(PUT 'LEGENDRE_BASE1 'NUMBER-OF-ARGS 6) 
(FLAG '(LEGENDRE_BASE1) 'OPFN) 
(PUT 'LEGENDRE_BASE1 'DEFINED-ON-LINE '92) 
(PUT 'LEGENDRE_BASE1 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'LEGENDRE_BASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LEGENDRE_BASE1 (X N BASE R A B)
    (COND ((EVALGEQ (AEVAL R) (AEVAL N)) (AEVAL (LIST 'REVERSE BASE)))
          (T
           (AEVAL
            (LIST 'LEGENDRE_BASE1 X N
                  (LIST 'CONS
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT
                                          (LIST 'PLUS
                                                (LIST 'TIMES
                                                      (LIST 'PLUS
                                                            (LIST 'TIMES 2 R) A
                                                            B 1)
                                                      (LIST 'DIFFERENCE
                                                            (LIST 'EXPT A 2)
                                                            (LIST 'EXPT B 2)))
                                                (LIST 'TIMES
                                                      (LIST 'PLUS
                                                            (LIST 'TIMES 2 R) A
                                                            B)
                                                      (LIST 'PLUS
                                                            (LIST 'TIMES 2 R) 1
                                                            A B)
                                                      (LIST 'PLUS
                                                            (LIST 'TIMES 2 R) 2
                                                            A B)
                                                      X))
                                          (LIST 'TIMES 2 (LIST 'PLUS R 1)
                                                (LIST 'PLUS R 1 A B)
                                                (LIST 'PLUS (LIST 'TIMES 2 R) A
                                                      B)))
                                    (LIST 'FIRST BASE))
                              (LIST 'TIMES 2 (LIST 'PLUS R A) (LIST 'PLUS R B)
                                    (LIST 'QUOTIENT
                                          (LIST 'PLUS (LIST 'TIMES 2 R) 2 A B)
                                          (LIST 'TIMES 2 (LIST 'PLUS R 1)
                                                (LIST 'PLUS R 1 A B)
                                                (LIST 'PLUS (LIST 'TIMES 2 R) A
                                                      B)))
                                    (LIST 'SECOND BASE)))
                        BASE)
                  (LIST 'PLUS R 1) A B))))) 
(PUT 'LAGUERRE_BASE 'NUMBER-OF-ARGS 3) 
(FLAG '(LAGUERRE_BASE) 'OPFN) 
(PUT 'LAGUERRE_BASE 'DEFINED-ON-LINE '101) 
(PUT 'LAGUERRE_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'LAGUERRE_BASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LAGUERRE_BASE (X N A)
    (LIST 'LAGUERRE_BASE1 X N
          (LIST 'LIST (LIST 'PLUS (LIST 'DIFFERENCE 1 X) A) 1) 1 A)) 
(PUT 'LAGUERRE_BASE1 'NUMBER-OF-ARGS 5) 
(FLAG '(LAGUERRE_BASE1) 'OPFN) 
(PUT 'LAGUERRE_BASE1 'DEFINED-ON-LINE '104) 
(PUT 'LAGUERRE_BASE1 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'LAGUERRE_BASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LAGUERRE_BASE1 (X N BASE R A)
    (COND ((EVALGEQ (AEVAL R) (AEVAL N)) (AEVAL (LIST 'REVERSE BASE)))
          (T
           (AEVAL
            (LIST 'LAGUERRE_BASE1 X N
                  (LIST 'CONS
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT
                                          (LIST 'PLUS 1
                                                (LIST 'DIFFERENCE
                                                      (LIST 'TIMES 2 R) X)
                                                A)
                                          (LIST 'PLUS R 1))
                                    (LIST 'FIRST BASE))
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT (LIST 'PLUS R A)
                                          (LIST 'PLUS R 1))
                                    (LIST 'SECOND BASE)))
                        BASE)
                  (LIST 'PLUS R 1) A))))) 
(PUT 'HERMITE_BASE 'NUMBER-OF-ARGS 2) 
(FLAG '(HERMITE_BASE) 'OPFN) 
(PUT 'HERMITE_BASE 'DEFINED-ON-LINE '110) 
(PUT 'HERMITE_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'HERMITE_BASE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HERMITE_BASE (X N)
    (LIST 'HERMITE_BASE1 X N (LIST 'LIST (LIST 'TIMES 2 X) 1) 1)) 
(PUT 'HERMITE_BASE1 'NUMBER-OF-ARGS 4) 
(FLAG '(HERMITE_BASE1) 'OPFN) 
(PUT 'HERMITE_BASE1 'DEFINED-ON-LINE '113) 
(PUT 'HERMITE_BASE1 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'HERMITE_BASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HERMITE_BASE1 (X N BASE R)
    (COND ((EVALGEQ (AEVAL R) (AEVAL N)) (AEVAL (LIST 'REVERSE BASE)))
          (T
           (AEVAL
            (LIST 'HERMITE_BASE1 X N
                  (LIST 'CONS
                        (LIST 'DIFFERENCE (LIST 'TIMES 2 X (LIST 'FIRST BASE))
                              (LIST 'TIMES 2 R (LIST 'SECOND BASE)))
                        BASE)
                  (LIST 'PLUS R 1)))))) 
(PUT 'CHEBYSHEV_BASE_T 'NUMBER-OF-ARGS 2) 
(FLAG '(CHEBYSHEV_BASE_T) 'OPFN) 
(PUT 'CHEBYSHEV_BASE_T 'DEFINED-ON-LINE '119) 
(PUT 'CHEBYSHEV_BASE_T 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'CHEBYSHEV_BASE_T 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHEBYSHEV_BASE_T (X N) (LIST 'CHEBYSHEV_BASE_T1 X N (LIST 'LIST X 1) 1)) 
(PUT 'CHEBYSHEV_BASE_T1 'NUMBER-OF-ARGS 4) 
(FLAG '(CHEBYSHEV_BASE_T1) 'OPFN) 
(PUT 'CHEBYSHEV_BASE_T1 'DEFINED-ON-LINE '122) 
(PUT 'CHEBYSHEV_BASE_T1 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'CHEBYSHEV_BASE_T1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHEBYSHEV_BASE_T1 (X N BASE R)
    (COND ((EVALGEQ (AEVAL R) (AEVAL N)) (AEVAL (LIST 'REVERSE BASE)))
          (T
           (AEVAL
            (LIST 'CHEBYSHEV_BASE_T1 X N
                  (LIST 'CONS
                        (LIST 'DIFFERENCE (LIST 'TIMES 2 X (LIST 'FIRST BASE))
                              (LIST 'SECOND BASE))
                        BASE)
                  (LIST 'PLUS R 1)))))) 
(PUT 'CHEBYSHEV_BASE_U 'NUMBER-OF-ARGS 2) 
(FLAG '(CHEBYSHEV_BASE_U) 'OPFN) 
(PUT 'CHEBYSHEV_BASE_U 'DEFINED-ON-LINE '128) 
(PUT 'CHEBYSHEV_BASE_U 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'CHEBYSHEV_BASE_U 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHEBYSHEV_BASE_U (X N)
    (LIST 'CHEBYSHEV_BASE_T1 X N (LIST 'LIST (LIST 'TIMES 2 X) 1) 1)) 
(PUT 'GEGENBAUER_BASE1 'NUMBER-OF-ARGS 5) 
(FLAG '(GEGENBAUER_BASE1) 'OPFN) 
(PUT 'GEGENBAUER_BASE1 'DEFINED-ON-LINE '131) 
(PUT 'GEGENBAUER_BASE1 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'GEGENBAUER_BASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEGENBAUER_BASE1 (X N BASE R A)
    (COND ((EVALGEQ (AEVAL R) (AEVAL N)) (AEVAL (LIST 'REVERSE BASE)))
          (T
           (AEVAL
            (LIST 'GEGENBAUER_BASE1 X N
                  (LIST 'CONS
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES 2
                                    (LIST 'QUOTIENT (LIST 'PLUS R A)
                                          (LIST 'PLUS R 1))
                                    X (LIST 'FIRST BASE))
                              (LIST 'TIMES
                                    (LIST 'QUOTIENT
                                          (LIST 'PLUS R
                                                (LIST 'DIFFERENCE
                                                      (LIST 'TIMES 2 A) 1))
                                          (LIST 'PLUS R 1))
                                    (LIST 'SECOND BASE)))
                        BASE)
                  (LIST 'PLUS R 1) A))))) 
(PUT 'GEGENBAUER_BASE 'NUMBER-OF-ARGS 3) 
(FLAG '(GEGENBAUER_BASE) 'OPFN) 
(PUT 'GEGENBAUER_BASE 'DEFINED-ON-LINE '137) 
(PUT 'GEGENBAUER_BASE 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'GEGENBAUER_BASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEGENBAUER_BASE (X N A)
    (LIST 'GEGENBAUER_BASE1 X N (LIST 'LIST (LIST 'TIMES 2 A X) 1) 1 A)) 
(PROG (*MSG)
  (AEVAL
   (OPERATOR
    (LIST 'HERMITEP 'JACOBIP 'LEGENDREP 'LEGENDREQ '~F 'LAGUERREP 'CHEBYSHEVT
          'CHEBYSHEVU 'GEGENBAUERP)))) 
(AEVAL
 (LET
  '((REPLACEBY (LIMIT (~ (F (~ N) (~ X))) (~ X) (~ LIM))
     (WHEN (F N LIM)
      (AND (FREEOF LIM INFINITY)
           (MEMBER F
                   (LIST LEGENDREP CHEBYSHEVT CHEBYSHEVU HERMITEP LAGUERREP
                         BERNOULLIP EULERP LAGUERREP)))))))) 
(AEVAL
 (LET
  '((REPLACEBY (LIMIT (~ (F (~ N) (~ M) (~ X))) (~ X) (~ LIM))
     (WHEN (F N M LIM)
      (AND (FREEOF LIM INFINITY)
           (MEMBER F (LIST LEGENDREP LEGENDREQ GEGENBAUERP LAGUERREP)))))))) 
(AEVAL
 (LET
  '((REPLACEBY (LIMIT (~ (F (~ N) (~ M) (~ MM) (~ X))) (~ X) (~ LIM))
     (WHEN (F N M MM LIM)
      (AND (FREEOF LIM INFINITY) (MEMBER F (LIST JACOBIP)))))))) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) 0 0)
               (LIST 'TIMES (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT (LIST 'FACTORIAL 'N)
                           (LIST 'TIMES (LIST 'EXPT 2 'N)
                                 (LIST 'EXPT
                                       (LIST 'FACTORIAL (LIST 'QUOTIENT 'N 2))
                                       2)))))
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) (LIST '~ 'M) 0)
               (LIST 'TIMES (LIST 'QUOTIENT (LIST 'EXPT 2 'M) (LIST 'SQRT 'PI))
                     (LIST 'COS
                           (LIST 'TIMES (LIST 'PLUS 'N 'M)
                                 (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT
                           (LIST 'GAMMA
                                 (LIST 'QUOTIENT (LIST 'PLUS 'N 'M 1) 2))
                           (LIST 'GAMMA
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS (LIST 'DIFFERENCE 'N 'M) 2)
                                       2)))))
         (LIST 'REPLACEBY (LIST 'LEGENDREQ (LIST '~ 'N) (LIST '~ 'M) 0)
               (LIST 'TIMES
                     (LIST 'QUOTIENT (LIST 'EXPT 2 (LIST 'DIFFERENCE 'M 1))
                           (LIST 'SQRT 'PI))
                     (LIST 'SIN
                           (LIST 'TIMES (LIST 'PLUS 'N 'M)
                                 (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT
                           (LIST 'GAMMA
                                 (LIST 'QUOTIENT (LIST 'PLUS 'N 'M 1) 2))
                           (LIST 'GAMMA
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS (LIST 'DIFFERENCE 'N 'M) 2)
                                       2)))))
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) 0)
               (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'PI))
                     (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT
                           (LIST 'GAMMA (LIST 'QUOTIENT (LIST 'PLUS 'N 1) 2))
                           (LIST 'GAMMA
                                 (LIST 'QUOTIENT (LIST 'PLUS 'N 2) 2)))))
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) 1) 1)
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) (MINUS 1))
               (LIST 'EXPT (MINUS 1) 'N))
         (LIST 'REPLACEBY (LIST 'GEGENBAUERP (LIST '~ 'N) 0 0)
               (LIST 'TIMES 2
                     (LIST 'QUOTIENT
                           (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                           'N)))
         (LIST 'REPLACEBY (LIST 'GEGENBAUERP (LIST '~ 'N) (LIST '~ 'A) 0)
               (LIST 'TIMES (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT
                           (LIST 'GAMMA (LIST 'PLUS 'A (LIST 'QUOTIENT 'N 2)))
                           (LIST 'TIMES (LIST 'GAMMA 'A)
                                 (LIST 'FACTORIAL (LIST 'QUOTIENT 'N 2))))))
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVT (LIST '~ 'N) 0)
               (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2))))
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVU (LIST '~ 'N) 0)
               (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2))))
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVT (LIST '~ 'N) 1) 1)
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVU (LIST '~ 'N) 1) (LIST 'PLUS 'N 1))
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVT (LIST '~ 'N) (MINUS 1))
               (LIST 'EXPT (MINUS 1) 'N))
         (LIST 'REPLACEBY (LIST 'CHEBYSHEVU (LIST '~ 'N) (MINUS 1))
               (LIST 'TIMES (LIST 'PLUS 'N 1) (LIST 'EXPT (MINUS 1) 'N)))
         (LIST 'REPLACEBY (LIST 'LAGUERREP (LIST '~ 'N) (LIST '~ 'A) 0)
               (LIST 'BINOMIAL (LIST 'PLUS 'N 'A) 'N))
         (LIST 'REPLACEBY (LIST 'LAGUERREP (LIST '~ 'N) 0) 1)
         (LIST 'REPLACEBY (LIST 'LAGUERREP 0 (LIST '~ 'X)) 1)
         (LIST 'REPLACEBY (LIST 'HERMITEP (LIST '~ 'N) 0)
               (LIST 'TIMES (LIST 'COS (LIST 'TIMES 'N (LIST 'QUOTIENT 'PI 2)))
                     (LIST 'QUOTIENT (LIST 'FACTORIAL 'N)
                           (LIST 'FACTORIAL (LIST 'QUOTIENT 'N 2))))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (HERMITEP (~ N) (~ X))
      (WHEN
       (PROG (B1 B2 BEX R)
         (SETQ R (AEVAL 1))
         (SETQ B1 (AEVAL (LIST 'TIMES 2 'X)))
         (SETQ B2 (AEVAL 1))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE 'N 1)) I))
             (RETURN NIL)))
           (PROGN
            (SETQ BEX
                    (AEVAL*
                     (LIST 'DIFFERENCE (LIST 'TIMES 2 'X B1)
                           (LIST 'TIMES 2 R B2))))
            (SETQ R (AEVAL* (LIST 'PLUS R 1)))
            (SETQ B2 (AEVAL* B1))
            (SETQ B1 (AEVAL* BEX))
            (AEVAL* 'NIL))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (RETURN (AEVAL B1)))
       (AND (FIXP N) (GREATERP N 0) (NUMBERP X))))
     (REPLACEBY (HERMITEP (~ N) (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (SETQ TMP (AEVAL (LIST 'EXPT (LIST 'TIMES 2 'X) 'N)))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'QUOTIENT 1 4)
                                    (LIST 'PLUS K 1))
                              (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                              (LIST 'QUOTIENT
                                    (LIST 'DIFFERENCE
                                          (LIST 'DIFFERENCE 'N
                                                (LIST 'TIMES 2 K))
                                          1)
                                    (LIST 'EXPT 'X 2))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'QUOTIENT 'N 2)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES TMP
                                 (LIST 'QUOTIENT (LIST 'QUOTIENT 1 4)
                                       (LIST 'PLUS K 1))
                                 (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                                 (LIST 'QUOTIENT
                                       (LIST 'DIFFERENCE
                                             (LIST 'DIFFERENCE 'N
                                                   (LIST 'TIMES 2 K))
                                             1)
                                       (LIST 'EXPT 'X 2))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0))))
     (REPLACEBY (HERMITEP 0 (~ X)) 1))))) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST
         (LIST 'REPLACEBY (LIST 'LEGENDREP (LIST '~ 'N) (LIST '~ 'X))
               (LIST 'WHEN
                     (LIST 'PROG
                           (LIST 'K 'TMP 'RESULT 'RATIO 'OLDSLASH 'POWLIS1*)
                           (LIST 'SETQ 'OLDSLASH
                                 (LIST 'REMPROP ''SLASH ''OPMTCH))
                           (LIST 'SETQ 'TMP
                                 (LIST 'AEVAL
                                       (LIST 'LIST ''TIMES
                                             (LIST 'LIST ''EXPT 2
                                                   (LIST 'LIST ''MINUS ''N))
                                             (LIST 'LIST ''QUOTIENT
                                                   (LIST 'LIST ''FACTORIAL
                                                         (LIST 'LIST ''TIMES 2
                                                               ''N))
                                                   (LIST 'LIST ''EXPT
                                                         (LIST 'LIST
                                                               ''FACTORIAL ''N)
                                                         2))
                                             (LIST 'LIST ''EXPT ''X ''N))))
                           (LIST 'SETQ 'RESULT (LIST 'AEVAL 'TMP))
                           (LIST 'SETQ 'RATIO
                                 (LIST 'AEVAL
                                       (LIST 'LIST ''MINUS
                                             (LIST 'LIST ''TIMES
                                                   (LIST 'LIST ''QUOTIENT
                                                         (LIST 'LIST ''QUOTIENT
                                                               1 2)
                                                         (LIST 'LIST ''EXPT ''X
                                                               2))
                                                   (LIST 'LIST ''DIFFERENCE
                                                         (LIST 'LIST
                                                               ''DIFFERENCE ''N
                                                               (LIST 'LIST
                                                                     ''TIMES 2
                                                                     'K))
                                                         1)
                                                   (LIST 'LIST ''QUOTIENT
                                                         (LIST 'LIST ''QUOTIENT
                                                               (LIST 'LIST
                                                                     ''DIFFERENCE
                                                                     ''N
                                                                     (LIST
                                                                      'LIST
                                                                      ''TIMES 2
                                                                      'K))
                                                               (LIST 'LIST
                                                                     ''PLUS 'K
                                                                     1))
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               (LIST 'LIST
                                                                     ''DIFFERENCE
                                                                     (LIST
                                                                      'LIST
                                                                      ''TIMES 2
                                                                      ''N)
                                                                     (LIST
                                                                      'LIST
                                                                      ''TIMES 2
                                                                      'K))
                                                               1))))))
                           (LIST 'PROG (LIST 'K) (LIST 'SETQ 'K 0) 'LAB
                                 (LIST 'COND
                                       (LIST
                                        (LIST '|AMINUSP:|
                                              (LIST 'LIST ''DIFFERENCE
                                                    (LIST 'AEVAL*
                                                          (LIST 'LIST
                                                                ''QUOTIENT ''N
                                                                2))
                                                    'K))
                                        (LIST 'RETURN NIL)))
                                 (LIST 'PROGN
                                       (LIST 'SETQ 'TMP
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''MINUS
                                                         (LIST 'LIST ''TIMES
                                                               (LIST 'LIST
                                                                     ''QUOTIENT
                                                                     (LIST
                                                                      'LIST
                                                                      ''QUOTIENT
                                                                      'TMP 2)
                                                                     (LIST
                                                                      'LIST
                                                                      ''EXPT
                                                                      ''X 2))
                                                               (LIST 'LIST
                                                                     ''DIFFERENCE
                                                                     (LIST
                                                                      'LIST
                                                                      ''DIFFERENCE
                                                                      ''N
                                                                      (LIST
                                                                       'LIST
                                                                       ''TIMES
                                                                       2 'K))
                                                                     1)
                                                               (LIST 'LIST
                                                                     ''QUOTIENT
                                                                     (LIST
                                                                      'LIST
                                                                      ''QUOTIENT
                                                                      (LIST
                                                                       'LIST
                                                                       ''DIFFERENCE
                                                                       ''N
                                                                       (LIST
                                                                        'LIST
                                                                        ''TIMES
                                                                        2 'K))
                                                                      (LIST
                                                                       'LIST
                                                                       ''PLUS
                                                                       'K 1))
                                                                     (LIST
                                                                      'LIST
                                                                      ''DIFFERENCE
                                                                      (LIST
                                                                       'LIST
                                                                       ''DIFFERENCE
                                                                       (LIST
                                                                        'LIST
                                                                        ''TIMES
                                                                        2 ''N)
                                                                       (LIST
                                                                        'LIST
                                                                        ''TIMES
                                                                        2 'K))
                                                                      1))))))
                                       (LIST 'SETQ 'RESULT
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS 'RESULT
                                                         'TMP)))
                                       (LIST 'AEVAL* ''NIL))
                                 (LIST 'SETQ 'K
                                       (LIST
                                        (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                              (LIST 'AEVAL*
                                                    (LIST 'LIST ''PLUS
                                                          'FORALL-RESULT 1)))
                                        'K))
                                 (LIST 'GO 'LAB))
                           (LIST 'PUT ''SLASH ''OPMTCH 'OLDSLASH)
                           (LIST 'RETURN (LIST 'AEVAL 'RESULT)))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0))))
         (LIST 'REPLACEBY
               (LIST 'LEGENDREP (LIST '~ 'N) (LIST '~ 'M) (LIST '~ 'X))
               (LIST 'WHEN
                     (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'M)
                           (LIST 'EXPT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'X 2))
                                 (LIST 'QUOTIENT 'M 2))
                           (LIST 'SUB (LIST 'EQUAL '=Z 'X)
                                 (LIST 'DF (LIST 'LEGENDREP 'N '=Z) '=Z 'M)))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                           (LIST 'FIXP 'M) (LIST 'GREATERP 'M 0))))
         (LIST 'REPLACEBY
               (LIST 'JACOBIP (LIST '~ 'N) (LIST '~ 'A) (LIST '~ 'B)
                     (LIST '~ 'X))
               (LIST 'WHEN
                     (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 2 'N))
                           (LIST 'PROG (LIST 'II 'FORALL-RESULT)
                                 (LIST 'SETQ 'II 0)
                                 (LIST 'SETQ 'FORALL-RESULT 0) 'LAB1
                                 (LIST 'COND
                                       (LIST
                                        (LIST '|AMINUSP:|
                                              (LIST 'LIST ''DIFFERENCE
                                                    (LIST 'AEVAL* ''N) 'II))
                                        (LIST 'RETURN 'FORALL-RESULT)))
                                 (LIST 'SETQ 'FORALL-RESULT
                                       (LIST 'AEVAL*
                                             (LIST 'LIST ''PLUS
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST ''TIMES
                                                               (LIST 'LIST
                                                                     ''BINOMIAL
                                                                     (LIST
                                                                      'LIST
                                                                      ''PLUS
                                                                      ''N ''A)
                                                                     'II)
                                                               (LIST 'LIST
                                                                     ''BINOMIAL
                                                                     (LIST
                                                                      'LIST
                                                                      ''PLUS
                                                                      ''N ''B)
                                                                     (LIST
                                                                      'LIST
                                                                      ''DIFFERENCE
                                                                      ''N 'II))
                                                               (LIST 'LIST
                                                                     ''EXPT
                                                                     (LIST
                                                                      'LIST
                                                                      ''DIFFERENCE
                                                                      ''X 1)
                                                                     (LIST
                                                                      'LIST
                                                                      ''DIFFERENCE
                                                                      ''N 'II))
                                                               (LIST 'LIST
                                                                     ''EXPT
                                                                     (LIST
                                                                      'LIST
                                                                      ''PLUS
                                                                      ''X 1)
                                                                     'II)))
                                                   'FORALL-RESULT)))
                                 (LIST 'SETQ 'II
                                       (LIST
                                        (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                              (LIST 'AEVAL*
                                                    (LIST 'LIST ''PLUS
                                                          'FORALL-RESULT 1)))
                                        'II))
                                 (LIST 'GO 'LAB1)))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                           (LIST 'NUMBERP 'A)
                           (LIST 'GREATERP 'A (LIST 'MINUS 1))
                           (LIST 'NUMBERP 'B)
                           (LIST 'GREATERP 'B (LIST 'MINUS 1)))))
         (LIST 'REPLACEBY
               (LIST 'JACOBIP (LIST '~ 'N) (LIST '~ 'A) (LIST '~ 'B)
                     (LIST '~ 'X))
               (LIST 'WHEN
                     (LIST 'SUB (LIST 'EQUAL '=Z 'X)
                           (LIST 'FIRST
                                 (LIST 'REVERSE
                                       (LIST 'LEGENDRE_BASE '=Z 'N 'A 'B))))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0))))
         (LIST 'REPLACEBY (LIST 'LEGENDREP 0 (LIST '~ 'X)) 1)
         (LIST 'REPLACEBY (LIST 'LEGENDREP 0 0 (LIST '~ 'X)) 1)
         (LIST 'REPLACEBY
               (LIST 'JACOBIP 0 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'X)) 1))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (LAGUERREP (~ N) (~ X))
      (WHEN (LAGUERREP (~ N) 0 (~ X)) (AND (FIXP N) (GREATERP N 0))))
     (REPLACEBY (LAGUERREP (~ N) (~ ALPHA) (~ X))
      (WHEN
       (PROG (B1 B2 BEX R)
         (SETQ R (AEVAL 1))
         (SETQ B1 (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE 1 'X) 'ALPHA)))
         (SETQ B2 (AEVAL 1))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE 'N 1)) I))
             (RETURN NIL)))
           (PROGN
            (SETQ BEX
                    (AEVAL*
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS 1
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES 2 R) 'X)
                                             'ALPHA)
                                       (LIST 'PLUS R 1))
                                 B1)
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT (LIST 'PLUS R 'ALPHA)
                                       (LIST 'PLUS R 1))
                                 B2))))
            (SETQ R (AEVAL* (LIST 'PLUS R 1)))
            (SETQ B2 (AEVAL* B1))
            (SETQ B1 (AEVAL* BEX))
            (AEVAL* 'NIL))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (RETURN (AEVAL B1)))
       (AND (FIXP N) (GREATERP N 0) (NUMBERP ALPHA) (NUMBERP X))))
     (REPLACEBY (LAGUERREP (~ N) (~ ALPHA) (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (COND ((EVALEQUAL (AEVAL 'N) 0) (RETURN 1)))
         (SETQ TMP
                 (AEVAL
                  (LIST 'QUOTIENT
                        (PROG (J FORALL-RESULT)
                          (SETQ J 1)
                          (SETQ FORALL-RESULT 1)
                         LAB1
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'N) J))
                            (RETURN FORALL-RESULT)))
                          (SETQ FORALL-RESULT
                                  (AEVAL*
                                   (LIST 'TIMES (AEVAL* (LIST 'PLUS J 'ALPHA))
                                         FORALL-RESULT)))
                          (SETQ J
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   J))
                          (GO LAB1))
                        (LIST 'FACTORIAL 'N))))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'PLUS 'ALPHA K 1))
                              (LIST 'DIFFERENCE 'N K)
                              (LIST 'QUOTIENT 'X (LIST 'PLUS K 1))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'N) K)) (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT TMP (LIST 'PLUS 'ALPHA K 1))
                                 (LIST 'DIFFERENCE 'N K)
                                 (LIST 'QUOTIENT 'X (LIST 'PLUS K 1))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0))))
     (REPLACEBY (LAGUERREP 0 (~ A) (~ X)) 1))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (CHEBYSHEVT (~ N) (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (COND ((EVALEQUAL (AEVAL 'N) 0) (RETURN 1)))
         (COND ((EVALEQUAL (AEVAL 'N) 1) (RETURN (AEVAL 'X))))
         (SETQ TMP
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT 2 (LIST 'DIFFERENCE 'N 1))
                        (LIST 'EXPT 'X 'N))))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                              (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                              (LIST 'QUOTIENT
                                    (LIST 'QUOTIENT
                                          (LIST 'QUOTIENT
                                                (LIST 'DIFFERENCE
                                                      (LIST 'DIFFERENCE 'N
                                                            (LIST 'TIMES 2 K))
                                                      1)
                                                (LIST 'EXPT 'X 2))
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N K) 1))
                                    (LIST 'PLUS K 1))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:|
              (LIST 'DIFFERENCE
                    (AEVAL* (LIST 'DIFFERENCE (LIST 'QUOTIENT 'N 2) 1)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES (LIST 'QUOTIENT TMP 4)
                                 (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                                 (LIST 'QUOTIENT
                                       (LIST 'QUOTIENT
                                             (LIST 'QUOTIENT
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE 'N
                                                               (LIST 'TIMES 2
                                                                     K))
                                                         1)
                                                   (LIST 'EXPT 'X 2))
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 'N K) 1))
                                       (LIST 'PLUS K 1))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0) (NOT (NUMBERP X)))))
     (REPLACEBY (CHEBYSHEVT (~ N) (~ X))
      (WHEN
       (PROG ()
         (COND ((EVALEQUAL (AEVAL 'N) 0) (RETURN 1))
               ((EVALEQUAL (AEVAL 'N) 1) (RETURN (AEVAL 'X)))
               ((EVALEQUAL (AEVAL (LIST 'FLOOR (LIST 'QUOTIENT 'N 2)))
                           (AEVAL (LIST 'QUOTIENT 'N 2)))
                (RETURN
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES 2
                              (LIST 'EXPT
                                    (LIST 'CHEBYSHEVT (LIST 'QUOTIENT 'N 2) 'X)
                                    2))
                        1))))
               (T
                (RETURN
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES 2
                              (LIST 'CHEBYSHEVT
                                    (LIST 'QUOTIENT (LIST 'DIFFERENCE 'N 1) 2)
                                    'X)
                              (LIST 'CHEBYSHEVT
                                    (LIST 'QUOTIENT (LIST 'PLUS 'N 1) 2) 'X))
                        'X))))))
       (AND (FIXP N) (GREATERP N 0) (NUMBERP X))))
     (REPLACEBY (CHEBYSHEVT 0 (~ X)) 1))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (CHEBYSHEVU (~ N) (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (COND ((EVALEQUAL (AEVAL 'N) 0) (RETURN 1)))
         (SETQ TMP (AEVAL (LIST 'TIMES (LIST 'EXPT 2 'N) (LIST 'EXPT 'X 'N))))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'QUOTIENT 1 4)
                                    (LIST 'DIFFERENCE 'N K))
                              (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                              (LIST 'QUOTIENT
                                    (LIST 'QUOTIENT
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N
                                                      (LIST 'TIMES 2 K))
                                                1)
                                          (LIST 'EXPT 'X 2))
                                    (LIST 'PLUS K 1))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'QUOTIENT 'N 2)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT (LIST 'QUOTIENT TMP 4)
                                       (LIST 'DIFFERENCE 'N K))
                                 (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                                 (LIST 'QUOTIENT
                                       (LIST 'QUOTIENT
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 'N
                                                         (LIST 'TIMES 2 K))
                                                   1)
                                             (LIST 'EXPT 'X 2))
                                       (LIST 'PLUS K 1))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0) (NOT (NUMBERP X)))))
     (REPLACEBY (CHEBYSHEVU (~ N) (~ X))
      (WHEN
       (PROG ()
         (COND ((EVALEQUAL (AEVAL 'N) 0) (RETURN 1))
               ((EVALEQUAL (AEVAL 'N) 1) (RETURN (AEVAL (LIST 'TIMES 2 'X))))
               ((BOOLVALUE* (REVALX (LIST 'EVENP 'N)))
                (RETURN
                 (AEVAL
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES 2
                              (LIST 'CHEBYSHEVT (LIST 'QUOTIENT 'N 2) 'X)
                              (LIST 'CHEBYSHEVU (LIST 'QUOTIENT 'N 2) 'X))
                        1))))
               (T
                (RETURN
                 (AEVAL
                  (LIST 'TIMES 2
                        (LIST 'CHEBYSHEVU
                              (LIST 'QUOTIENT (LIST 'DIFFERENCE 'N 1) 2) 'X)
                        (LIST 'CHEBYSHEVT (LIST 'QUOTIENT (LIST 'PLUS 'N 1) 2)
                              'X)))))))
       (AND (FIXP N) (GREATERP N 0) (NUMBERP X))))
     (REPLACEBY (CHEBYSHEVU 0 (~ X)) 1))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (GEGENBAUERP (~ N) (~ A) (~ X))
      (WHEN
       (PROG (B1 B2 BEX R)
         (SETQ R (AEVAL 1))
         (SETQ B1 (AEVAL (LIST 'TIMES 2 'A 'X)))
         (SETQ B2 (AEVAL 1))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE 'N 1)) I))
             (RETURN NIL)))
           (PROGN
            (SETQ BEX
                    (AEVAL*
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES 2
                                 (LIST 'QUOTIENT (LIST 'PLUS R 'A)
                                       (LIST 'PLUS R 1))
                                 'X B1)
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT
                                       (LIST 'PLUS R
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES 2 'A) 1))
                                       (LIST 'PLUS R 1))
                                 B2))))
            (SETQ R (AEVAL* (LIST 'PLUS R 1)))
            (SETQ B2 (AEVAL* B1))
            (SETQ B1 (AEVAL* BEX))
            (AEVAL* 'NIL))
           (SETQ I
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    I))
           (GO LAB))
         (RETURN (AEVAL B1)))
       (AND (FIXP N) (GREATERP N 0) (NUMBERP A) (NUMBERP X))))
     (REPLACEBY (GEGENBAUERP (~ N) (~ A) (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (SETQ TMP
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'QUOTIENT
                              (PROG (J FORALL-RESULT)
                                (SETQ J 1)
                                (SETQ FORALL-RESULT 1)
                               LAB1
                                (COND
                                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'N) J))
                                  (RETURN FORALL-RESULT)))
                                (SETQ FORALL-RESULT
                                        (AEVAL*
                                         (LIST 'TIMES
                                               (AEVAL*
                                                (LIST 'PLUS 'A
                                                      (DIFFERENCE J 1)))
                                               FORALL-RESULT)))
                                (SETQ J
                                        ((LAMBDA (FORALL-RESULT)
                                           (AEVAL*
                                            (LIST 'PLUS FORALL-RESULT 1)))
                                         J))
                                (GO LAB1))
                              (LIST 'FACTORIAL 'N))
                        (LIST 'EXPT 2 'N) (LIST 'EXPT 'X 'N))))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'QUOTIENT 1 4)
                                    (LIST 'PLUS 'A
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N K) 1)))
                              (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                              (LIST 'QUOTIENT
                                    (LIST 'QUOTIENT
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N
                                                      (LIST 'TIMES 2 K))
                                                1)
                                          (LIST 'EXPT 'X 2))
                                    (LIST 'PLUS K 1))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'QUOTIENT 'N 2)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT (LIST 'QUOTIENT TMP 4)
                                       (LIST 'PLUS 'A
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 'N K) 1)))
                                 (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                                 (LIST 'QUOTIENT
                                       (LIST 'QUOTIENT
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 'N
                                                         (LIST 'TIMES 2 K))
                                                   1)
                                             (LIST 'EXPT 'X 2))
                                       (LIST 'PLUS K 1))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0) (NOT (EQUAL A 0)))))
     (REPLACEBY (GEGENBAUERP (~ N) 0 (~ X))
      (WHEN
       (PROG (K TMP RESULT RATIO OLDSLASH POWLIS1*)
         (SETQ OLDSLASH (REMPROP 'SLASH 'OPMTCH))
         (SETQ TMP
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT 2 'N)
                        (LIST 'QUOTIENT (LIST 'EXPT 'X 'N) 'N))))
         (SETQ RESULT (AEVAL TMP))
         (SETQ RATIO
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                              (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                              (LIST 'QUOTIENT
                                    (LIST 'QUOTIENT
                                          (LIST 'QUOTIENT
                                                (LIST 'DIFFERENCE
                                                      (LIST 'DIFFERENCE 'N
                                                            (LIST 'TIMES 2 K))
                                                      1)
                                                (LIST 'EXPT 'X 2))
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N K) 1))
                                    (LIST 'PLUS K 1))))))
         (PROG (K)
           (SETQ K 0)
          LAB
           (COND
            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'QUOTIENT 'N 2)) K))
             (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (AEVAL*
                     (LIST 'MINUS
                           (LIST 'TIMES (LIST 'QUOTIENT TMP 4)
                                 (LIST 'DIFFERENCE 'N (LIST 'TIMES 2 K))
                                 (LIST 'QUOTIENT
                                       (LIST 'QUOTIENT
                                             (LIST 'QUOTIENT
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE 'N
                                                               (LIST 'TIMES 2
                                                                     K))
                                                         1)
                                                   (LIST 'EXPT 'X 2))
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 'N K) 1))
                                       (LIST 'PLUS K 1))))))
            (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TMP)))
            (AEVAL* 'NIL))
           (SETQ K
                   ((LAMBDA (FORALL-RESULT)
                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                    K))
           (GO LAB))
         (PUT 'SLASH 'OPMTCH OLDSLASH)
         (RETURN (AEVAL RESULT)))
       (AND (FIXP N) (GREATERP N 0))))
     (REPLACEBY (GEGENBAUERP 0 (~ A) (~ X)) 1))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (DF (LEGENDREP (~ A) (~ B) (~ Z)) Z)
      (TIMES (QUOTIENT 1 (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE (TIMES (PLUS A B) (LEGENDREP (DIFFERENCE A 1) B Z))
                         (TIMES A Z (LEGENDREP A B Z)))))
     (REPLACEBY (DF (LEGENDREP (~ N) (~ Z)) Z)
      (TIMES (QUOTIENT N (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE (LEGENDREP (DIFFERENCE N 1) Z)
                         (TIMES Z (LEGENDREP N Z)))))
     (REPLACEBY (DF (LEGENDREQ (~ A) (~ B) (~ Z)) Z)
      (TIMES (QUOTIENT 1 (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE (TIMES (PLUS A B) (LEGENDREQ (DIFFERENCE A 1) B Z))
                         (TIMES A Z (LEGENDREQ A B Z)))))
     (REPLACEBY (DF (JACOBIP (~ N) (~ A) (~ B) (~ Z)) Z)
      (TIMES
       (QUOTIENT 1 (TIMES (DIFFERENCE 1 (EXPT Z 2)) (PLUS (TIMES 2 N) A B)))
       (PLUS (TIMES 2 (PLUS N A) (PLUS N B) (JACOBIP (DIFFERENCE N 1) A B Z))
             (TIMES N
                    (DIFFERENCE (DIFFERENCE A B)
                                (TIMES (PLUS (TIMES 2 N) A B) Z))
                    (JACOBIP N A B Z)))))
     (REPLACEBY (DF (GEGENBAUERP (~ N) (~ A) (~ Z)) Z)
      (TIMES (QUOTIENT 1 (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE
              (TIMES (PLUS N (DIFFERENCE (TIMES 2 A) 1))
                     (GEGENBAUERP (DIFFERENCE N 1) A Z))
              (TIMES N Z (GEGENBAUERP N A Z)))))
     (REPLACEBY (DF (CHEBYSHEVT (~ N) (~ Z)) Z)
      (TIMES (QUOTIENT 1 (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE (TIMES N (CHEBYSHEVT (DIFFERENCE N 1) Z))
                         (TIMES N Z (CHEBYSHEVT N Z)))))
     (REPLACEBY (DF (CHEBYSHEVU (~ N) (~ Z)) Z)
      (TIMES (QUOTIENT 1 (DIFFERENCE 1 (EXPT Z 2)))
             (DIFFERENCE (TIMES (PLUS N 1) (CHEBYSHEVU (DIFFERENCE N 1) Z))
                         (TIMES N Z (CHEBYSHEVU N Z)))))
     (REPLACEBY (DF (LAGUERREP (~ N) (~ A) (~ Z)) Z)
      (TIMES (QUOTIENT 1 Z)
             (PLUS (MINUS (TIMES (PLUS N A) (LAGUERREP (DIFFERENCE N 1) A Z)))
                   (TIMES N (LAGUERREP N A Z)))))
     (REPLACEBY (DF (LAGUERREP (~ N) (~ Z)) Z)
      (TIMES (QUOTIENT 1 Z)
             (PLUS (MINUS (TIMES N (LAGUERREP (DIFFERENCE N 1) Z)))
                   (TIMES N (LAGUERREP N Z)))))
     (REPLACEBY (DF (HERMITEP (~ N) (~ Z)) Z)
      (TIMES 2 N (HERMITEP (DIFFERENCE N 1) Z)))
     (REPLACEBY (DF (BERNOULLIP (~ N) (~ Z)) Z)
      (TIMES N (BERNOULLIP (DIFFERENCE N 1) Z)))
     (REPLACEBY (DF (EULERP (~ N) (~ Z)) Z)
      (TIMES N (EULERP (DIFFERENCE N 1) Z))))))) 
(AEVAL 'NIL) 
(FLAG '(FIBONACCI FIBONACCIP) 'OPFN) 
(FLAG '(FIBONACCI) 'INTEGER) 
(PUT 'FIBONACCI 'NUMBER-OF-ARGS 1) 
(PUT 'FIBONACCI 'NUMBER-OF-ARGS 1) 
(PUT 'FIBONACCI 'DEFINED-ON-LINE '456) 
(PUT 'FIBONACCI 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'FIBONACCI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIBONACCI (N)
    (COND
     ((NOT (FIXP N))
      (MK*SQ
       (CONS (LIST (CONS (GETPOWER (FKERN (LIST 'FIBONACCI N)) 1) 1)) 1)))
     ((EQUAL N 0) 0) ((OR (EQUAL N 1) (EQUAL N (MINUS 1))) 1)
     (T
      (PROG (I3 SGN)
        (SETQ I3 0)
        (COND ((LESSP N 0) (PROGN (SETQ SGN T) (SETQ N (MINUS N)))))
        (SETQ I3 (|FIBONACCI:AUX1| N))
        (RETURN (COND ((AND SGN (EVENP N)) (MINUS I3)) (T I3))))))) 
(GLOBAL '(|FIBONACCI:ALIST|)) 
(SETQ |FIBONACCI:ALIST|
        '((0 . 0) (1 . 1) (2 . 1) (3 . 2) (4 . 3) (5 . 5) (6 . 8) (7 . 13)
          (8 . 21) (9 . 34))) 
(PUT '|FIBONACCI:AUX1| 'NUMBER-OF-ARGS 1) 
(PUT '|FIBONACCI:AUX1| 'DEFINED-ON-LINE '473) 
(PUT '|FIBONACCI:AUX1| 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT '|FIBONACCI:AUX1| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |FIBONACCI:AUX1| (N)
    (PROG (FI)
      (SETQ FI (ATSOC N |FIBONACCI:ALIST|))
      (COND (FI (RETURN (CDR FI))))
      (SETQ FI (|FIBONACCI:AUX2| N))
      (SETQ |FIBONACCI:ALIST| (CONS (CONS N FI) |FIBONACCI:ALIST|))
      (RETURN FI))) 
(PUT '|FIBONACCI:AUX2| 'NUMBER-OF-ARGS 1) 
(PUT '|FIBONACCI:AUX2| 'DEFINED-ON-LINE '482) 
(PUT '|FIBONACCI:AUX2| 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT '|FIBONACCI:AUX2| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |FIBONACCI:AUX2| (N)
    (COND
     ((EVENP N)
      ((LAMBDA (F)
         (TIMES F
                (PLUS F
                      (TIMES 2
                             (|FIBONACCI:AUX1|
                              (DIFFERENCE (QUOTIENT N 2) 1))))))
       (|FIBONACCI:AUX1| (QUOTIENT N 2))))
     (T
      (PLUS (EXPT (|FIBONACCI:AUX1| (QUOTIENT (PLUS N 1) 2)) 2)
            (EXPT (|FIBONACCI:AUX1| (QUOTIENT (DIFFERENCE N 1) 2)) 2))))) 
(PUT 'FIBONACCIP 'NUMBER-OF-ARGS 2) 
(PUT 'FIBONACCIP 'DEFINED-ON-LINE '487) 
(PUT 'FIBONACCIP 'DEFINED-IN-FILE 'SPECFN/SFPOLYS.RED) 
(PUT 'FIBONACCIP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIBONACCIP (N X)
    (COND
     ((OR (NOT (FIXP N)) (NOT (IDP X)))
      (MK*SQ
       (CONS (LIST (CONS (GETPOWER (FKERN (LIST 'FIBONACCIP N X)) 1) 1)) 1)))
     ((OR (EQUAL N 0) (EQUAL N 1)) N)
     (T
      (PROG (I3 I2 I1 SGN)
        (COND ((LESSP N 0) (PROGN (SETQ SGN T) (SETQ N (MINUS N)))))
        (SETQ I2 1)
        (SETQ I1 0)
        (SETQ I3 0)
        (PROG (I)
          (SETQ I 2)
         LAB
          (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
          (PROGN
           (SETQ I3 (REVAL1 (LIST 'PLUS (LIST 'TIMES X I2) I1) NIL))
           (SETQ I1 I2)
           (SETQ I2 I3))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (RETURN
         (COND ((AND SGN (EVENP N)) (REVAL1 (LIST 'MINUS I3) NIL)) (T I3))))))) 
(ENDMODULE) 