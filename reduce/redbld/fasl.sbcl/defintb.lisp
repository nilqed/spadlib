(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTB)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PROG (*COMBINELOGS *EXPANDLOGS)
  (SETK 'DEFINT_CHOOSE_DATA
        (AEVAL
         (LIST 'LIST
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'QUOTIENT 1
                                 (LIST 'EXPT (LIST '~ 'A)
                                       (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                             (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST 'WHEN
                           (LIST '|DEFINT:OPF1| 1
                                 (LIST 'TIMES 'X (LIST 'LOG 'A) 'B))
                           (LIST 'AND (LIST 'FREEOF 'A 'VAR)
                                 (LIST 'FREEOF 'B 'VAR))))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'SIN (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 2 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'MINUS (LIST 'SIN (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 25 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'COS (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 3 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES (LIST 'ACOS (LIST '~ 'X))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 7 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'ACOS (LIST 'QUOTIENT 1 (LIST '~ 'X)))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE (LIST '~ 'X) 1)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 8 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'ATAN (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 9 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'MYSINH (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 10 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'QUOTIENT
                                 (LIST 'DIFFERENCE
                                       (LIST 'EXPT 'E
                                             (LIST 'TIMES 2 (LIST '~ 'X)))
                                       1)
                                 (LIST 'TIMES 2 (LIST 'EXPT 'E (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 10 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'QUOTIENT
                                 (LIST 'DIFFERENCE (LIST 'EXPT 'E (LIST '~ 'Y))
                                       1)
                                 (LIST 'TIMES 2 (LIST 'EXPT 'E (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST 'WHEN (LIST '|DEFINT:OPF1| 10 'X)
                           (LIST 'EQUAL 'Y (LIST 'TIMES 2 'X))))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'MYCOSH (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 11 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'QUOTIENT
                                 (LIST 'PLUS
                                       (LIST 'EXPT 'E
                                             (LIST 'TIMES 2 (LIST '~ 'X)))
                                       1)
                                 (LIST 'TIMES 2 (LIST 'EXPT 'E (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 11 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'QUOTIENT
                                 (LIST 'PLUS (LIST 'EXPT 'E (LIST '~ 'Y)) 1)
                                 (LIST 'TIMES 2 (LIST 'EXPT 'E (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST 'WHEN (LIST '|DEFINT:OPF1| 11 'X)
                           (LIST 'EQUAL 'Y (LIST 'TIMES 2 'X))))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'HEAVISIDE (LIST 'DIFFERENCE 1 (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 30 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'HEAVISIDE
                                 (LIST 'QUOTIENT
                                       (LIST 'DIFFERENCE (LIST '~ 'P)
                                             (LIST '~ 'X))
                                       (LIST '~ 'P)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 30 (LIST 'QUOTIENT 'X 'P)))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'HEAVISIDE (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 31 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES (LIST 'LOG (LIST '~ 'X))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 32 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES (LIST 'LOG (LIST '~ 'X))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE (LIST '~ 'X) 1)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 33 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'LOG (LIST '~ 'X))
                                       (LIST '~ 'N))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF31| 'N 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'LOG (LIST '~ 'X))
                                       (LIST '~ 'N))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE (LIST '~ 'X) 1)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF32| 'N 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'LOG (LIST 'PLUS 1 (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 34 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'LOG
                                 (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'X) 1)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 35 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'EI (LIST 'MINUS (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 36 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'SI (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 37 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'CI (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 38 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'SHI (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 39 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'ERF (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 41 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'PLUS (LIST 'MINUS (LIST 'ERF (LIST '~ 'X)))
                                 1)
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 42 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'FRESNEL_S (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 43 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST 'FRESNEL_C (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 44 'X))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'M_GAMMA (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 45 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'BESSELJ (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 50 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'BESSELY (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 51 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'BESSELI (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 52 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'BESSELK (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 53 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 54 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'STRUVEL (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 55 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'M_LEGENDREP (LIST '~ 'N) (LIST '~ 'X))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 56 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'M_LEGENDREP (LIST '~ 'N)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X)))
                                 (LIST 'HEAVISIDE
                                       (LIST 'DIFFERENCE (LIST '~ 'X) 1)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 57 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE 1 (LIST '~ 'X))
                                       (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                                 (LIST 'M_CHEBYSHEVT (LIST '~ 'N)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 58 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST 'MINUS (LIST 'QUOTIENT 1 2)))
                                 (LIST 'M_CHEBYSHEVT (LIST '~ 'N)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 59 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE 1 (LIST '~ 'X))
                                       (LIST 'QUOTIENT 1 2))
                                 (LIST 'M_CHEBYSHEVU (LIST '~ 'N)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 60 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST 'QUOTIENT 1 2))
                                 (LIST 'M_CHEBYSHEVU (LIST '~ 'N)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 61 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'M_HERMITEP (LIST '~ 'N) (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 62 'X 'N))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'M_LAGUERREP (LIST '~ 'N) (LIST '~ 'L)
                                 (LIST '~ 'X))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 63 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST '~ 'X)))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 64 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST '~ 'X)))
                                 (LIST 'DIFFERENCE 1 (LIST '~ 'X))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 64 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST '~ 'K))
                                 (LIST 'SQRT (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 64 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST '~ 'K))
                                 (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST '~ 'X)))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 64 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'MINUS
                                 (LIST 'TIMES
                                       (LIST 'EXPT
                                             (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                             (LIST '~ 'K))
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE 1 (LIST '~ 'X)))
                                       (LIST 'M_GEGENBAUERP (LIST '~ 'N)
                                             (LIST '~ 'L) (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 64 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'SQRT (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 65 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'SQRT (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                                 (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'L)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 65 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'SQRT (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST '~ 'K))
                                 (LIST 'M_GEGENBAUERP (LIST '~ 'N) (LIST '~ 'K)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 65 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'MINUS
                                 (LIST 'TIMES
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE (LIST '~ 'X) 1))
                                       (LIST 'EXPT
                                             (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                             (LIST '~ 'K))
                                       (LIST 'M_GEGENBAUERP (LIST '~ 'N)
                                             (LIST '~ 'K)
                                             (LIST 'QUOTIENT 1 (LIST '~ 'X)))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 65 'X 'N 'L))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE 1 (LIST '~ 'X))
                                       (LIST '~ 'R))
                                 (LIST 'M_JACOBIP (LIST '~ 'N) (LIST '~ 'R)
                                       (LIST '~ 'S) (LIST '~ 'X)))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 66 'X 'N 'R 'S))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'DIFFERENCE (LIST '~ 'X) 1)
                                       (LIST '~ 'R))
                                 (LIST 'M_JACOBIP (LIST '~ 'N) (LIST '~ 'R)
                                       (LIST '~ 'S)
                                       (LIST 'QUOTIENT 1 (LIST '~ 'X))))
                           (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 67 'X 'N 'R 'S))
               (LIST 'REPLACEBY (LIST 'DEFINT_CHOOSE 0 (LIST '~ 'VAR))
                     (LIST '|DEFINT:OPF1| 0 0))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST '~ 'N) (LIST '~ 'VAR))
                     (LIST 'WHEN (LIST '|DEFINT:OPF1| 0 'N)
                           (LIST 'NUMBERP 'N)))
               (LIST 'REPLACEBY
                     (LIST 'DEFINT_CHOOSE (LIST '~ 'F) (LIST '~ 'VAR))
                     'UNKNOWN))))
  (AEVAL (LET '(DEFINT_CHOOSE_DATA)))) 
(ENDMODULE) 