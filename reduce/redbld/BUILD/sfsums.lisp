(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFSUMS)) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'K))
                           (LIST 'EXPT
                                 (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST '~ 'K))
                                       1)
                                 (LIST '~ 'N)))
                     (LIST '~ 'K) 1 'INFINITY)
               (LIST 'WHEN
                     (LIST 'TIMES (LIST 'EXPT 'PI 'N)
                           (LIST 'QUOTIENT
                                 (LIST 'ABS
                                       (LIST 'EULER (LIST 'DIFFERENCE 'N 1)))
                                 (LIST 'TIMES
                                       (LIST 'FACTORIAL
                                             (LIST 'DIFFERENCE 'N 1))
                                       (LIST 'EXPT 2 (LIST 'PLUS 'N 1)))))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                           (LIST 'NOT (LIST 'EVENP 'N)))))
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'K))
                           (LIST 'EXPT
                                 (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST '~ 'K))
                                       1)
                                 2))
                     (LIST '~ 'K) 1 'INFINITY)
               (LIST 'MINUS 'CATALAN))
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'K))
                           (LIST 'EXPT
                                 (LIST 'PLUS (LIST 'TIMES 2 (LIST '~ 'K)) 1)
                                 2))
                     (LIST '~ 'K) 0 'INFINITY)
               'CATALAN)
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT 1
                           (LIST 'EXPT
                                 (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST '~ 'K))
                                       1)
                                 (LIST '~ 'N)))
                     (LIST '~ 'K) 1 'INFINITY)
               (LIST 'WHEN
                     (LIST 'TIMES (LIST 'ZETA 'N)
                           (LIST 'DIFFERENCE 1
                                 (LIST 'EXPT 2 (LIST 'MINUS 'N))))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                           (LIST 'EVENP 'N))))
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST '~ 'K) (LIST '~ 'S)))
                     (LIST '~ 'K) 1 'INFINITY)
               (LIST 'ZETA 'S))
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'K))
                           (LIST 'EXPT (LIST '~ 'K) (LIST '~ 'N)))
                     (LIST '~ 'K) 1 'INFINITY)
               (LIST 'WHEN
                     (LIST 'TIMES (LIST 'ZETA 'N)
                           (LIST 'DIFFERENCE 1
                                 (LIST 'EXPT 2 (LIST 'DIFFERENCE 1 'N))))
                     (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                           (LIST 'EVENP 'N)))))))) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'PLUS 1
                           (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST '~ 'N) 2)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT (LIST 'QUOTIENT (LIST 'SINH 'PI) 'PI)
                           (LIST 'PROD
                                 (LIST 'PLUS 1
                                       (LIST 'QUOTIENT 1
                                             (LIST 'EXPT (LIST '~ 'N) 2)))
                                 (LIST '~ 'N) 1
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'GEQ 'R 1)
                           (LIST 'LESSP 'R 15))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'PLUS 1
                           (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST '~ 'N) 3)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'QUOTIENT
                                 (LIST 'COSH
                                       (LIST 'QUOTIENT
                                             (LIST 'TIMES (LIST 'SQRT 3) 'PI)
                                             2))
                                 'PI)
                           (LIST 'PROD
                                 (LIST 'PLUS 1
                                       (LIST 'QUOTIENT 1
                                             (LIST 'EXPT (LIST '~ 'N) 3)))
                                 (LIST '~ 'N) 1
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'GEQ 'R 1)
                           (LIST 'LESSP 'R 15))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'PLUS 1
                           (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST '~ 'N) 4)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'QUOTIENT
                                 (LIST 'DIFFERENCE
                                       (LIST 'COSH
                                             (LIST 'TIMES (LIST 'SQRT 2) 'PI))
                                       (LIST 'COS
                                             (LIST 'TIMES (LIST 'SQRT 2) 'PI)))
                                 (LIST 'TIMES 2 (LIST 'EXPT 'PI 2)))
                           (LIST 'PROD
                                 (LIST 'PLUS 1
                                       (LIST 'QUOTIENT 1
                                             (LIST 'EXPT (LIST '~ 'N) 4)))
                                 (LIST '~ 'N) 1
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'GEQ 'R 1)
                           (LIST 'LESSP 'R 15))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'PLUS 1
                           (LIST 'QUOTIENT 1 (LIST 'EXPT (LIST '~ 'N) 5)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'EXPT
                                 (LIST 'TIMES
                                       (LIST 'GAMMA
                                             (LIST 'EXP
                                                   (LIST 'QUOTIENT
                                                         (LIST 'TIMES 2 'PI 'I)
                                                         5)))
                                       (LIST 'GAMMA
                                             (LIST 'EXP
                                                   (LIST 'QUOTIENT
                                                         (LIST 'TIMES 6 'PI 'I)
                                                         5))))
                                 (MINUS 2))
                           (LIST 'PROD
                                 (LIST 'PLUS 1
                                       (LIST 'QUOTIENT 1
                                             (LIST 'EXPT (LIST '~ 'N) 5)))
                                 (LIST '~ 'N) 1
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'GEQ 'R 1)
                           (LIST 'LESSP 'R 15))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'DIFFERENCE 1
                           (LIST 'QUOTIENT 4 (LIST 'EXPT (LIST '~ 'N) 2)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT (LIST 'QUOTIENT 1 6)
                           (LIST 'PROD
                                 (LIST 'DIFFERENCE 1
                                       (LIST 'QUOTIENT 4
                                             (LIST 'EXPT (LIST '~ 'N) 2)))
                                 (LIST '~ 'N) 3
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'LESSP 'R 15)
                           (LIST 'GEQ 'R 3))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'DIFFERENCE 1
                           (LIST 'QUOTIENT 8 (LIST 'EXPT (LIST '~ 'N) 3)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'QUOTIENT
                                 (LIST 'SINH (LIST 'TIMES (LIST 'SQRT 3) 'PI))
                                 (LIST 'TIMES 42 (LIST 'SQRT 3) 'PI))
                           (LIST 'PROD
                                 (LIST 'DIFFERENCE 1
                                       (LIST 'QUOTIENT 8
                                             (LIST 'EXPT (LIST '~ 'N) 3)))
                                 (LIST '~ 'N) 3
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'LESSP 'R 15)
                           (LIST 'GEQ 'R 3))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'DIFFERENCE 1
                           (LIST 'QUOTIENT 16 (LIST 'EXPT (LIST '~ 'N) 4)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'QUOTIENT (LIST 'SINH (LIST 'TIMES 2 'PI))
                                 (LIST 'TIMES 120 'PI))
                           (LIST 'PROD
                                 (LIST 'DIFFERENCE 1
                                       (LIST 'QUOTIENT 16
                                             (LIST 'EXPT (LIST '~ 'N) 4)))
                                 (LIST '~ 'N) 3
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'LESSP 'R 15)
                           (LIST 'GEQ 'R 3))))
         (LIST 'REPLACEBY
               (LIST 'PROD
                     (LIST 'DIFFERENCE 1
                           (LIST 'QUOTIENT 32 (LIST 'EXPT (LIST '~ 'N) 5)))
                     (LIST '~ 'N) (LIST '~ 'R) 'INFINITY)
               (LIST 'WHEN
                     (LIST 'QUOTIENT
                           (LIST 'TIMES (LIST 'QUOTIENT 1 1240)
                                 (LIST 'GAMMA
                                       (LIST 'TIMES 2
                                             (LIST 'EXP
                                                   (LIST 'QUOTIENT
                                                         (LIST 'TIMES 'PI 'I)
                                                         5))))
                                 (LIST 'EXPT
                                       (LIST 'GAMMA
                                             (LIST 'TIMES 2
                                                   (LIST 'EXP
                                                         (LIST 'QUOTIENT
                                                               (LIST 'TIMES 7
                                                                     'PI 'I)
                                                               5))))
                                       (MINUS 2)))
                           (LIST 'PROD
                                 (LIST 'DIFFERENCE 1
                                       (LIST 'QUOTIENT 32
                                             (LIST 'EXPT (LIST '~ 'N) 5)))
                                 (LIST '~ 'N) 3
                                 (LIST 'DIFFERENCE (LIST '~ 'R) 1)))
                     (LIST 'AND (LIST 'FIXP 'R) (LIST 'LESSP 'R 15)
                           (LIST 'GEQ 'R 3)))))))) 
(AEVAL 'NIL) 
(ENDMODULE) 