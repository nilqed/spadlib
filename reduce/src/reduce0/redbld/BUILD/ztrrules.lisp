(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ZTRRULES)) 
(SETK 'ZTRANSRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZTRANS_AUX 1 (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'QUOTIENT 'Z (LIST 'DIFFERENCE 'Z 1)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'BINOMIAL
                               (LIST 'PLUS (LIST '~ 'N) (LIST '~ (LIST '~ 'K)))
                               (LIST '~ 'M))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'EXPT 'Z (LIST 'PLUS 'K 1))
                               (LIST 'EXPT (LIST 'DIFFERENCE 'Z 1)
                                     (LIST 'PLUS 'M 1)))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'FREEOF 'M 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST 'FACTORIAL (LIST '~ 'N))
                               (LIST 'TIMES
                                     (LIST 'FACTORIAL
                                           (LIST 'DIFFERENCE (LIST '~ 'N)
                                                 (LIST '~ 'K)))
                                     (LIST 'FACTORIAL (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'ZTRANS (LIST 'BINOMIAL 'N 'K) 'N 'Z)
                         (LIST 'FREEOF 'K 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'DIFFERENCE 'K 1))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 'Z
                                           (LIST 'LOG
                                                 (LIST 'QUOTIENT 'Z
                                                       (LIST 'DIFFERENCE 'Z
                                                             1))))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'TIMES (LIST 'PLUS 'J 1)
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0 (LIST 'DIFFERENCE 'K 2))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N) (LIST 'FIXP 'K)
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'EXPT (LIST '~ 'A)
                               (LIST 'PLUS (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'A 'K)
                               (LIST 'QUOTIENT 'Z (LIST 'DIFFERENCE 'Z 'A)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'A (LIST 'QUOTIENT 1 'A))
                               (LIST 'ZTRANS (LIST 'EXPT 'A (LIST 'PLUS 'N 'K))
                                     'N 'Z))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'EXPT 'E
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'QUOTIENT 'Z
                                     (LIST 'DIFFERENCE (LIST 'EXPT 'E 'A) 'Z)))
                         (LIST 'FREEOF 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'EXPT 'E
                               (LIST 'TIMES
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))
                                     (LIST '~ (LIST '~ 'A))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'E (LIST 'TIMES 'A 'K))
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT 'Z
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'E 'A)
                                                 'Z))))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1 (LIST 'FACTORIAL (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'EXPT 'E (LIST 'QUOTIENT 1 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST 'TIMES 2 (LIST '~ 'N))
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT 'Z
                                     (LIST 'QUOTIENT (LIST 'DIFFERENCE 'K 1)
                                           2))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'SQRT 'Z)
                                           (LIST 'SINH
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'SQRT 'Z))))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL
                                                             (LIST 'PLUS
                                                                   (LIST 'TIMES
                                                                         2 'J)
                                                                   1))
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'K 3) 2))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP
                                     (LIST 'QUOTIENT (LIST 'PLUS 'K 1) 2))
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST 'TIMES 2 (LIST '~ 'N))
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'QUOTIENT 'K 2))
                               (LIST 'DIFFERENCE
                                     (LIST 'COSH
                                           (LIST 'QUOTIENT 1 (LIST 'SQRT 'Z)))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL
                                                             (LIST 'TIMES 2
                                                                   'J))
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0
                                           (LIST 'DIFFERENCE
                                                 (LIST 'QUOTIENT 'K 2) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP (LIST 'QUOTIENT 'K 2))
                               (LIST 'GEQ 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'N))
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST 'TIMES 2 (LIST '~ 'N))
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'MINUS 'Z)
                                     (LIST 'QUOTIENT (LIST 'DIFFERENCE 'K 1)
                                           2))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'SQRT 'Z)
                                           (LIST 'SIN
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'SQRT 'Z))))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'EXPT (MINUS 1) 'J)
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL
                                                             (LIST 'PLUS
                                                                   (LIST 'TIMES
                                                                         2 'J)
                                                                   1))
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE 'K 3) 2))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP
                                     (LIST 'QUOTIENT (LIST 'PLUS 'K 1) 2))
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST 'EXPT (MINUS 1) (LIST '~ 'N))
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST 'TIMES 2 (LIST '~ 'N))
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'MINUS 'Z)
                                     (LIST 'QUOTIENT 'K 2))
                               (LIST 'DIFFERENCE
                                     (LIST 'COS
                                           (LIST 'QUOTIENT 1 (LIST 'SQRT 'Z)))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'EXPT (MINUS 1) 'J)
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL
                                                             (LIST 'TIMES 2
                                                                   'J))
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0
                                           (LIST 'DIFFERENCE
                                                 (LIST 'QUOTIENT 'K 2) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP (LIST 'QUOTIENT 'K 2))
                               (LIST 'GEQ 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'SINH
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'AL))
                                           (LIST '~ 'N))
                                     (LIST '~ (LIST '~ 'P))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'TIMES 'Z (LIST 'SINH 'P))
                                           (LIST 'SINH
                                                 (LIST 'DIFFERENCE 'AL 'P)))
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'COSH 'AL)))
                                           1)))
                         (LIST 'AND (LIST 'FREEOF 'AL 'N)
                               (LIST 'FREEOF 'P 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'COSH
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'AL))
                                           (LIST '~ 'N))
                                     (LIST '~ (LIST '~ 'P))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'Z (LIST 'COSH 'P))
                                           (LIST 'COSH
                                                 (LIST 'DIFFERENCE 'AL 'P)))
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'COSH 'AL)))
                                           1)))
                         (LIST 'AND (LIST 'FREEOF 'AL 'N)
                               (LIST 'FREEOF 'P 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'SIN
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N))
                                     (LIST '~ (LIST '~ 'P))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'TIMES 'Z (LIST 'SIN 'P))
                                           (LIST 'SIN
                                                 (LIST 'DIFFERENCE 'B 'P)))
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'COS 'B)))
                                           1)))
                         (LIST 'AND (LIST 'FREEOF 'B 'N)
                               (LIST 'FREEOF 'P 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'COS
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N))
                                     (LIST '~ (LIST '~ 'P))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'Z (LIST 'COS 'P))
                                           (LIST 'COS
                                                 (LIST 'DIFFERENCE 'B 'P)))
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'COS 'B)))
                                           1)))
                         (LIST 'AND (LIST 'FREEOF 'B 'N)
                               (LIST 'FREEOF 'P 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT 'E
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'N)))
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z (LIST 'EXPT 'E 'A)
                               (LIST 'QUOTIENT (LIST 'SIN 'B)
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'EXPT 'E 'A)
                                                       (LIST 'COS 'B)))
                                           (LIST 'EXPT 'E
                                                 (LIST 'TIMES 2 'A)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'B 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT 'E
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'N)))
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE 'Z
                                           (LIST 'TIMES (LIST 'EXPT 'E 'A)
                                                 (LIST 'COS 'B)))
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'Z 2)
                                                 (LIST 'TIMES 2 'Z
                                                       (LIST 'EXPT 'E 'A)
                                                       (LIST 'COS 'B)))
                                           (LIST 'EXPT 'E
                                                 (LIST 'TIMES 2 'A)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'B 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST 'PLUS (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'DIFFERENCE 'K 1))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 'Z
                                           (LIST 'LOG
                                                 (LIST 'QUOTIENT 'Z
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'EXPT
                                                                          'Z 2)
                                                                    (LIST
                                                                     'TIMES 2
                                                                     'Z
                                                                     (LIST 'COS
                                                                           'B)))
                                                                   1)))))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'COS
                                                       (LIST 'TIMES 'B
                                                             (LIST 'PLUS 'J
                                                                   1)))
                                                 (LIST 'TIMES (LIST 'PLUS 'J 1)
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0 (LIST 'DIFFERENCE 'K 2))))
                         (LIST 'AND (LIST 'FREEOF 'B 'N) (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP 'K) (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST 'PLUS (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'DIFFERENCE 'K 1))
                               (LIST 'DIFFERENCE
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'Z
                                                 (LIST 'ATAN
                                                       (LIST 'QUOTIENT
                                                             (LIST 'SIN 'B)
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'COS
                                                                         'B)
                                                                   'Z)))))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'SIN
                                                       (LIST 'TIMES 'B
                                                             (LIST 'PLUS 'J
                                                                   1)))
                                                 (LIST 'TIMES (LIST 'PLUS 'J 1)
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0 (LIST 'DIFFERENCE 'K 2))))
                         (LIST 'AND (LIST 'FREEOF 'B 'N) (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP 'K) (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'QUOTIENT
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST 'PLUS (LIST '~ 'N)
                                                       (LIST '~
                                                             (LIST '~ 'K)))))
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'MINUS 'Z)
                                           (LIST 'DIFFERENCE 'K 1))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'Z
                                                 (LIST 'LOG
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS
                                                                   (LIST 'EXPT
                                                                         'Z 2)
                                                                   (LIST 'TIMES
                                                                         2 'Z
                                                                         (LIST
                                                                          'COS
                                                                          'B))
                                                                   (LIST
                                                                    'QUOTIENT 1
                                                                    'Z)))))
                                           (LIST 'SUM
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT (MINUS 1)
                                                             'J)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'COS
                                                                   (LIST 'TIMES
                                                                         'B
                                                                         (LIST
                                                                          'PLUS
                                                                          'J
                                                                          1)))
                                                             (LIST 'TIMES
                                                                   (LIST 'PLUS
                                                                         'J 1)
                                                                   (LIST 'EXPT
                                                                         'Z
                                                                         'J))))
                                                 'J 0
                                                 (LIST 'DIFFERENCE 'K 2)))))
                         (LIST 'AND (LIST 'FREEOF 'B 'N) (LIST 'FREEOF 'K 'N)
                               (LIST 'FIXP 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N)))
                               (LIST 'FACTORIAL (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'COS (LIST 'QUOTIENT (LIST 'SIN 'B) 'Z))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST 'COS 'B) 'Z)))
                         (LIST 'FREEOF 'B 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z 'K)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'COS
                                                 (LIST 'QUOTIENT (LIST 'SIN 'B)
                                                       'Z))
                                           (LIST 'EXPT 'E
                                                 (LIST 'QUOTIENT (LIST 'COS 'B)
                                                       'Z)))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'COS
                                                       (LIST 'TIMES 'B 'J))
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL 'J)
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0 (LIST 'DIFFERENCE 'K 1))))
                         (LIST 'AND (LIST 'FREEOF 'B 'N) (LIST 'FIXP 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'N)))
                               (LIST 'FACTORIAL (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SIN (LIST 'QUOTIENT (LIST 'SIN 'B) 'Z))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST 'COS 'B) 'Z)))
                         (LIST 'FREEOF 'B 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z 'K)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'SIN
                                                 (LIST 'QUOTIENT (LIST 'SIN 'B)
                                                       'Z))
                                           (LIST 'EXPT 'E
                                                 (LIST 'QUOTIENT (LIST 'COS 'B)
                                                       'Z)))
                                     (LIST 'SUM
                                           (LIST 'QUOTIENT
                                                 (LIST 'SIN
                                                       (LIST 'TIMES 'B 'J))
                                                 (LIST 'TIMES
                                                       (LIST 'FACTORIAL 'J)
                                                       (LIST 'EXPT 'Z 'J)))
                                           'J 0 (LIST 'DIFFERENCE 'K 1))))
                         (LIST 'AND (LIST 'FREEOF 'B 'N) (LIST 'FIXP 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX (LIST 'MINUS (LIST '~ 'F)) (LIST '~ 'N)
                         (LIST '~ 'Z))
                   (LIST 'MINUS (LIST 'ZTRANS 'F 'N 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX (LIST '~ 'A) (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'TIMES 'A (LIST 'ZTRANS 1 'N 'Z))
                         (LIST 'FREEOF 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX (LIST 'TIMES (LIST '~ 'A) (LIST '~ 'F))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'TIMES 'A (LIST 'ZTRANS 'F 'N 'Z))
                         (LIST 'FREEOF 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'B))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'QUOTIENT (LIST 'ZTRANS 'F 'N 'Z) 'B)
                         (LIST 'FREEOF 'B 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX (LIST 'QUOTIENT (LIST '~ 'A) (LIST '~ 'G))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'A
                               (LIST 'ZTRANS (LIST 'QUOTIENT 1 'G) 'N 'Z))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'NOT (LIST 'EQUAL 'A 1)))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES (LIST '~ 'A)
                               (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'A
                               (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'G) 'N 'Z))
                         (LIST 'FREEOF 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST '~ 'F)
                               (LIST 'TIMES (LIST '~ 'B) (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'G) 'N 'Z) 'B)
                         (LIST 'FREEOF 'B 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'G))
                               (LIST '~ (LIST '~ 'H)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'PLUS (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'H) 'N 'Z)
                         (LIST 'ZTRANS (LIST 'QUOTIENT 'G 'H) 'N 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'N) (LIST '~ (LIST '~ 'P)))
                               (LIST '~ (LIST '~ 'F)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES 'Z
                                     (LIST 'DF
                                           (LIST 'ZTRANS
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT 'N
                                                             (LIST 'DIFFERENCE
                                                                   'P 1))
                                                       'F)
                                                 'N 'Z)
                                           'Z)))
                         (LIST 'AND (LIST 'FREEOF 'P 'N) (LIST 'FIXP 'P)
                               (LIST 'GREATERP 'P 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'N) (LIST '~ (LIST '~ 'P)))
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                                     (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES 'Z
                                     (LIST 'DF
                                           (LIST 'ZTRANS
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT 'N
                                                             (LIST 'DIFFERENCE
                                                                   'P 1))
                                                       (LIST 'QUOTIENT 'F 'G))
                                                 'N 'Z)
                                           'Z)))
                         (LIST 'AND (LIST 'FREEOF 'P 'N) (LIST 'FIXP 'P)
                               (LIST 'GREATERP 'P 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'F
                                     (LIST 'PLUS (LIST '~ 'N) (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z 'K)
                               (LIST 'DIFFERENCE
                                     (LIST 'ZTRANS (LIST 'F 'N) 'N 'Z)
                                     (LIST 'SUM
                                           (LIST 'TIMES (LIST 'F 'N)
                                                 (LIST 'EXPT 'Z
                                                       (LIST 'MINUS 'N)))
                                           'N 0 (LIST 'DIFFERENCE 'K 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N) (LIST 'FIXP 'K)
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT
                               (LIST '~
                                     (LIST 'F
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ 'K))))
                               (LIST '~
                                     (LIST 'G
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z 'K)
                               (LIST 'DIFFERENCE
                                     (LIST 'ZTRANS
                                           (LIST 'QUOTIENT (LIST 'F 'N)
                                                 (LIST 'G 'N))
                                           'N 'Z)
                                     (LIST 'SUM
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT (LIST 'F 'N)
                                                       (LIST 'G 'N))
                                                 (LIST 'EXPT 'Z
                                                       (LIST 'MINUS 'N)))
                                           'N 0 (LIST 'DIFFERENCE 'K 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N) (LIST 'FIXP 'K)
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT 1
                               (LIST '~
                                     (LIST 'G
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'Z 'K)
                               (LIST 'DIFFERENCE
                                     (LIST 'ZTRANS
                                           (LIST 'QUOTIENT 1 (LIST 'G 'N)) 'N
                                           'Z)
                                     (LIST 'SUM
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'G 'N))
                                                 (LIST 'EXPT 'Z
                                                       (LIST 'MINUS 'N)))
                                           'N 0 (LIST 'DIFFERENCE 'K 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'N) (LIST 'FIXP 'K)
                               (LIST 'GREATERP 'K 0))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K))))
                               (LIST '~ 'F))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'A 'K)
                               (LIST 'SUB
                                     (LIST 'EQUAL 'Z (LIST 'QUOTIENT 'Z 'A))
                                     (LIST 'ZTRANS 'F 'N 'Z)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K))))
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                                     (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'A 'K)
                               (LIST 'SUB
                                     (LIST 'EQUAL 'Z (LIST 'QUOTIENT 'Z 'A))
                                     (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'G) 'N
                                           'Z)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'DIFFERENCE (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K))))
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                                     (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT 'A 'K)
                               (LIST 'SUB
                                     (LIST 'EQUAL 'Z (LIST 'QUOTIENT 'Z 'A))
                                     (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'G) 'N
                                           'Z)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST '~ 'F))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'A 'K))
                               (LIST 'SUB (LIST 'EQUAL 'Z (LIST 'TIMES 'Z 'A))
                                     (LIST 'ZTRANS 'F 'N 'Z)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'PLUS (LIST '~ 'N)
                                                 (LIST '~ (LIST '~ 'K)))))
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                                     (LIST '~ 'G)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'A 'K))
                               (LIST 'SUB (LIST 'EQUAL 'Z (LIST 'TIMES 'Z 'A))
                                     (LIST 'ZTRANS (LIST 'QUOTIENT 'F 'G) 'N
                                           'Z)))
                         (LIST 'AND (LIST 'FREEOF 'A 'N)
                               (LIST 'FREEOF 'K 'N))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'SUM
                               (LIST 'TIMES (LIST '~ (LIST 'F (LIST '~ 'K)))
                                     (LIST '~
                                           (LIST 'G
                                                 (LIST 'DIFFERENCE (LIST '~ 'N)
                                                       (LIST '~ 'K)))))
                               (LIST '~ 'K) 0 (LIST '~ 'N))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'ZTRANS (LIST 'F 'N) 'N 'Z)
                               (LIST 'ZTRANS (LIST 'G 'N) 'N 'Z))
                         (LIST 'FREEOF 'K 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K) 0
                                     (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES 'Z
                               (LIST 'QUOTIENT
                                     (LIST 'ZTRANS
                                           (LIST 'SUB (LIST 'EQUAL 'K 'N) 'F)
                                           'N 'Z)
                                     (LIST 'DIFFERENCE 'Z 1)))
                         (LIST 'AND (LIST 'FREEOF 'K 'N)
                               (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K) 0
                                     (LIST 'PLUS (LIST '~ 'N) (LIST '~ 'W))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES 'Z
                                     (LIST 'QUOTIENT
                                           (LIST 'ZTRANS
                                                 (LIST 'SUB (LIST 'EQUAL 'K 'N)
                                                       'F)
                                                 'N 'Z)
                                           (LIST 'DIFFERENCE 'Z 1)))
                               (LIST 'SUM
                                     (LIST 'TIMES (LIST 'EXPT 'Z 'X)
                                           (LIST 'DIFFERENCE
                                                 (LIST 'ZTRANS
                                                       (LIST 'SUB
                                                             (LIST 'EQUAL 'K
                                                                   'N)
                                                             'F)
                                                       'N 'Z)
                                                 (LIST 'SUM
                                                       (LIST 'QUOTIENT
                                                             (LIST 'SUB
                                                                   (LIST 'EQUAL
                                                                         'K 'N)
                                                                   'F)
                                                             (LIST 'EXPT 'Z
                                                                   'N))
                                                       'N 0
                                                       (LIST 'DIFFERENCE 'X
                                                             1))))
                                     'X 1 'W))
                         (LIST 'AND (LIST 'FREEOF 'W 'N) (LIST 'FIXP 'W)
                               (LIST 'GREATERP 'W 0)
                               (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K)
                                     (LIST '~ 'P) (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'ZTRANS
                               (LIST 'SUM
                                     (LIST 'SUB
                                           (LIST 'EQUAL 'K (LIST 'PLUS 'K 'P))
                                           'F)
                                     'K 0 (LIST 'DIFFERENCE 'N 'P))
                               'N 'Z)
                         (LIST 'AND (LIST 'FREEOF 'P 'N) (LIST 'FIXP 'P)
                               (LIST 'GREATERP 'P 0)
                               (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K) 0
                                     (LIST '~ 'NN)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT
                                     (LIST 'ZTRANS
                                           (LIST 'SUB (LIST 'EQUAL 'K 'N) 'F)
                                           'N 'Z)
                                     (LIST 'DIFFERENCE 'Z 1))
                               (LIST 'SUM
                                     (LIST 'TIMES
                                           (LIST 'QUOTIENT 1
                                                 (LIST 'EXPT 'Z 'Y))
                                           (LIST 'ZTRANS
                                                 (LIST 'SUB (LIST 'EQUAL 'K 'N)
                                                       'F)
                                                 'N 'Z))
                                     'Y 1
                                     (LIST 'DIFFERENCE
                                           (LIST 'DIFFERENCE 'N 'NN) 1)))
                         (LIST 'AND (LIST 'FREEOF (LIST 'DIFFERENCE 'NN 'N) 'N)
                               (LIST 'FIXP (LIST 'DIFFERENCE 'NN 'N))
                               (LIST 'LESSP (LIST 'DIFFERENCE 'NN 'N) 0)
                               (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K)
                                     (LIST '~ 'P) (LIST '~ 'N)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'ZTRANS
                               (LIST 'SUM
                                     (LIST 'SUB
                                           (LIST 'EQUAL 'K (LIST 'PLUS 'K 'P))
                                           'F)
                                     'K 0 (LIST 'PLUS 'N (LIST 'MINUS 'P)))
                               'N 'Z)
                         (LIST 'AND (LIST 'FREEOF 'P 'N) (LIST 'FIXP 'P)
                               (LIST 'LESSP 'P 0) (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST '~
                               (LIST 'SUMM (LIST '~ 'F) (LIST '~ 'K)
                                     (LIST '~ 'P) (LIST '~ 'Q)))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'R)
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE ''Q ''P)))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''ZTRANS
                                                 (LIST 'LIST ''SUM
                                                       (LIST 'LIST ''SUB
                                                             (LIST 'LIST
                                                                   ''EQUAL ''K
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''K
                                                                         ''P))
                                                             ''F)
                                                       ''K 0 'R)
                                                 ''N ''Z))))
                         (LIST 'AND (LIST 'NOT (LIST 'EQUAL 'P 0))
                               (LIST 'EQUAL 'SUMM 'SUM))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                               (LIST 'PLUS (LIST '~ 'N)
                                     (LIST '~ (LIST '~ 'K))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PROG NIL
                               (LIST 'AEVAL
                                     (LIST 'LIST ''NEWREDERR
                                           (LIST 'LIST ''LIST
                                                 "ERROR: zero divisor in "
                                                 (LIST 'LIST ''SUM
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''F
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         ''K)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         ''N)))
                                                       ''N 0 ''INFINITY)))))
                         (LIST 'AND (LIST 'NUMBERP 'K) (LIST 'LESSP 'K 1))))
             (LIST 'REPLACEBY
                   (LIST 'ZTRANS_AUX
                         (LIST 'QUOTIENT (LIST '~ (LIST '~ 'F))
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS (LIST '~ 'N)
                                           (LIST '~ (LIST '~ 'K)))))
                         (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PROG NIL
                               (LIST 'AEVAL
                                     (LIST 'LIST ''NEWREDERR
                                           (LIST 'LIST ''LIST
                                                 "ERROR: zero divisor in "
                                                 (LIST 'LIST ''SUM
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''F
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''FACTORIAL
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          ''K))
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         ''N)))
                                                       ''N 0 ''INFINITY)))))
                         (LIST 'AND (LIST 'NUMBERP 'K) (LIST 'LESSP 'K 0))))))) 
(AEVAL (LET '(ZTRANSRULES))) 
(LOAD-PACKAGE 'RESIDUE) 
(SETK 'ZTRANSTRIGHYPSIMPLIFICATIONRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'ASIN (LIST 'SIN (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ACOS (LIST 'COS (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ATAN (LIST 'TAN (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ACOT (LIST 'COT (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ASINH (LIST 'SINH (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ACOSH (LIST 'COSH (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ATANH (LIST 'TANH (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY (LIST 'ACOTH (LIST 'COTH (LIST '~ 'XX))) 'XX)
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'DIFFERENCE 1
                               (LIST 'EXPT (LIST 'SIN (LIST '~ 'XX)) 2))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'COS 'XX))
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'DIFFERENCE 1
                               (LIST 'EXPT (LIST 'COS (LIST '~ 'XX)) 2))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'SIN 'XX))
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'COSH (LIST '~ 'XX)) 2) 1)
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'SINH 'XX))
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'PLUS 1
                               (LIST 'EXPT (LIST 'SINH (LIST '~ 'XX)) 2))
                         (LIST 'QUOTIENT 1 2))
                   (LIST 'COSH 'XX))
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'PLUS (LIST 'COSH (LIST '~ 'XX))
                               (LIST 'SINH (LIST '~ 'XX)))
                         (LIST '~ 'NN))
                   (LIST 'PLUS (LIST 'COSH (LIST 'TIMES 'NN 'XX))
                         (LIST 'SINH (LIST 'TIMES 'NN 'XX))))
             (LIST 'REPLACEBY
                   (LIST 'EXPT
                         (LIST 'DIFFERENCE (LIST 'COSH (LIST '~ 'XX))
                               (LIST 'SINH (LIST '~ 'XX)))
                         (LIST '~ 'NN))
                   (LIST 'DIFFERENCE (LIST 'COSH (LIST 'TIMES 'NN 'XX))
                         (LIST 'SINH (LIST 'TIMES 'NN 'XX))))))) 
(AEVAL (PUT 'INVZTRANS 'SIMPFN 'SIMPIDEN)) 
(AEVAL (PUT 'INVZTRANS_AUX 'SIMPFN 'SIMPIDEN)) 
(AEVAL (PUT 'INVZTRANS1 'SIMPFN 'SIMPIDEN)) 
(AEVAL (PUT 'INVZTRANS_END 'SIMPFN 'SIMPIDEN)) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN
       (QUOTIENT
        (PROG (I FORALL-RESULT)
          (SETQ I 0)
          (SETQ FORALL-RESULT 1)
         LAB1
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE 'K 1)) I))
            (RETURN FORALL-RESULT)))
          (SETQ FORALL-RESULT
                  (AEVAL*
                   (LIST 'TIMES (AEVAL* (LIST 'DIFFERENCE 'N I))
                         FORALL-RESULT)))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB1))
        (FACTORIAL K))
       (FIXP K))))))) 
(AEVAL 'NIL) 
(PUT 'DO_INVZTRANS 'NUMBER-OF-ARGS 3) 
(FLAG '(DO_INVZTRANS) 'OPFN) 
(PUT 'DO_INVZTRANS 'DEFINED-ON-LINE '304) 
(PUT 'DO_INVZTRANS 'DEFINED-IN-FILE 'ZTRANS/ZTRRULES.RED) 
(PUT 'DO_INVZTRANS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DO_INVZTRANS (F Z N)
    (PROG (TMP NUMTMP DENTMP)
      (SETQ TMP (AEVAL (LIST 'INVZTRANS1 F Z N)))
      (SETQ NUMTMP (AEVAL (LIST 'NUM TMP)))
      (SETQ DENTMP (AEVAL (LIST 'DEN TMP)))
      (SETQ NUMTMP
              (AEVAL
               (LIST 'WHEREEXP (LIST 'LIST 'ZTRANSTRIGHYPSIMPLIFICATIONRULES)
                     NUMTMP)))
      (SETQ DENTMP
              (AEVAL
               (LIST 'WHEREEXP (LIST 'LIST 'ZTRANSTRIGHYPSIMPLIFICATIONRULES)
                     DENTMP)))
      (SETQ TMP (AEVAL (LIST 'QUOTIENT NUMTMP DENTMP)))
      (RETURN (AEVAL TMP)))) 
(PUT 'SLASH 'SIMPFN 'SIMPIDEN) 
(SETK 'INVZTRANSRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS (LIST '~ 'P) (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN '$DO_INVZTRANS$
                         (LIST 'FREEOF
                               (LIST 'SETQ '$DO_INVZTRANS$
                                     (LIST 'DO_INVZTRANS 'P 'Z 'N))
                               (LIST 'SYMBOLIC ''FAIL))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST '~ 'P) (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'TIMES 'P (LIST 'INVZTRANS1 1 'Z 'N))
                         (LIST 'AND (LIST 'FREEOF 'P 'Z)
                               (LIST 'NOT (LIST 'EQUAL 'P 1)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST 'TIMES (LIST '~ 'P) (LIST '~ 'F))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'TIMES 'P (LIST 'INVZTRANS1 'F 'Z 'N))
                         (LIST 'FREEOF 'P 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'Q))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'QUOTIENT (LIST 'INVZTRANS1 'F 'Z 'N) 'Q)
                         (LIST 'FREEOF 'Q 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST 'QUOTIENT (LIST '~ 'P) (LIST '~ 'G))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'P
                               (LIST 'INVZTRANS1 (LIST 'QUOTIENT 1 'G) 'Z 'N))
                         (LIST 'AND (LIST 'FREEOF 'P 'Z)
                               (LIST 'NOT (LIST 'EQUAL 'P 1)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1
                         (LIST 'TIMES (LIST '~ 'P)
                               (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'P
                               (LIST 'INVZTRANS1 (LIST 'QUOTIENT 'F 'G) 'Z 'N))
                         (LIST 'FREEOF 'P 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1
                         (LIST 'QUOTIENT (LIST '~ 'F)
                               (LIST 'TIMES (LIST '~ 'Q) (LIST '~ 'G)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'INVZTRANS1 (LIST 'QUOTIENT 'F 'G) 'Z 'N)
                               'Q)
                         (LIST 'FREEOF 'Q 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST 'MINUS (LIST '~ 'F)) (LIST '~ 'Z)
                         (LIST '~ 'N))
                   (LIST 'MINUS (LIST 'INVZTRANS1 'F 'Z 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'G))
                               (LIST '~ (LIST '~ 'H)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'PLUS (LIST 'INVZTRANS1 (LIST 'QUOTIENT 'F 'H) 'Z 'N)
                         (LIST 'INVZTRANS1 (LIST 'QUOTIENT 'G 'H) 'Z 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST 'QUOTIENT (LIST '~ 'F) (LIST '~ 'G))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'INVZTRANS_AUX 'F 'G 'Z 'N)
                         (LIST 'OR
                               (LIST 'NOT
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'F 'G)
                                           'SIN))
                               (LIST 'NOT
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'F 'G)
                                           'COS))
                               (LIST 'NOT
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'F 'G)
                                           'SINH))
                               (LIST 'NOT
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'F 'G)
                                           'COSH)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS1 (LIST '~ 'F) (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'INVZTRANS_END 'F 'Z 'N))))) 
(AEVAL (LET '(INVZTRANSRULES))) 
(SETK 'INVZTRANS_AUXRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'F)
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                           (LIST 'EXPT (LIST '~ 'Z) 2)))
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                           (LIST '~ 'Z))
                                     (LIST '~ 'Y)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'MINUS
                         (LIST 'INVZTRANS_AUX 'F
                               (LIST 'PLUS
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'X (LIST 'EXPT 'Z 2))
                                           (LIST 'TIMES 'W 'Z))
                                     'Y)
                               'Z 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST 'PLUS (LIST '~ 'F) (LIST '~ 'H))
                         (LIST '~ 'G) (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'PLUS (LIST 'INVZTRANS_AUX 'F 'G 'Z 'N)
                         (LIST 'INVZTRANS_AUX 'H 'G 'Z 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'Z)
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                           (LIST 'EXPT (LIST '~ 'Z) 2))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                           (LIST '~ 'Z)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'TIMES 2 (LIST 'EXPT 'SRY 'N)
                                     (LIST 'QUOTIENT
                                           (LIST 'SIN
                                                 (LIST 'TIMES
                                                       (LIST 'ACOS
                                                             (LIST 'TIMES 'SRX
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    'W
                                                                    (LIST
                                                                     'TIMES 2
                                                                     'SRY
                                                                     'X))))
                                                       'N))
                                           (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                                 (LIST 'SQRT
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES 4 'X
                                                                   'Y)
                                                             (LIST 'EXPT 'W
                                                                   2)))))))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W 0)
                                     (LIST 'LESSP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NOT (LIST 'NUMBERP 'Y))
                                     (LIST 'GREATERP 'W 0))
                               (LIST 'AND
                                     (LIST 'NOT
                                           (LIST 'AND (LIST 'NUMBERP 'X)
                                                 (LIST 'NUMBERP 'W)
                                                 (LIST 'NUMBERP 'Y)))
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'W 'X)
                                           'COSH)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'Z)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'Z) 2))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                     (LIST '~ 'Z))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'MINUS
                                     (LIST 'TIMES 2 (LIST 'EXPT 'SRY 'N)
                                           (LIST 'EXPT (MINUS 1) 'N)
                                           (LIST 'QUOTIENT
                                                 (LIST 'SIN
                                                       (LIST 'TIMES
                                                             (LIST 'ACOS
                                                                   (LIST 'TIMES
                                                                         'SRX
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          'W
                                                                          (LIST
                                                                           'TIMES
                                                                           2
                                                                           'SRY
                                                                           'X))))
                                                             'N))
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT 'SRX 'N)
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y)
                                                                   (LIST 'EXPT
                                                                         'W
                                                                         2))))))))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W 0)
                                     (LIST 'LESSP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NOT (LIST 'NUMBERP 'Y))
                                     (LIST 'LESSP 'W 0))
                               (LIST 'AND
                                     (LIST 'NOT
                                           (LIST 'AND (LIST 'NUMBERP 'X)
                                                 (LIST 'NUMBERP 'W)
                                                 (LIST 'NUMBERP 'Y)))
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'W 'X)
                                           'COSH)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'Z)
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                           (LIST 'EXPT (LIST '~ 'Z) 2))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                           (LIST '~ 'Z)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'TIMES 2 (LIST 'EXPT 'SRY 'N)
                                     (LIST 'QUOTIENT
                                           (LIST 'SINH
                                                 (LIST 'TIMES
                                                       (LIST 'ACOSH
                                                             (LIST 'TIMES 'SRX
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    'W
                                                                    (LIST
                                                                     'TIMES 2
                                                                     'SRY
                                                                     'X))))
                                                       'N))
                                           (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                                 (LIST 'SQRT
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'W 2)
                                                             (LIST 'TIMES 4 'X
                                                                   'Y)))))))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'NUMBERP 'X)
                                           (LIST 'NUMBERP 'W)
                                           (LIST 'NUMBERP 'Y))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'Z)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'Z) 2))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                     (LIST '~ 'Z))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'MINUS
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'MINUS 'SRY) 'N)
                                           (LIST 'QUOTIENT
                                                 (LIST 'SINH
                                                       (LIST 'TIMES
                                                             (LIST 'ACOSH
                                                                   (LIST 'TIMES
                                                                         'SRX
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          'W
                                                                          (LIST
                                                                           'TIMES
                                                                           2
                                                                           'SRY
                                                                           'X))))
                                                             'N))
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT 'SRX 'N)
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'W 2)
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y))))))))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'NUMBERP 'X)
                                           (LIST 'NUMBERP 'W)
                                           (LIST 'NUMBERP 'Y))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST 'EXPT (LIST '~ 'Z) 2)
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                           (LIST 'EXPT (LIST '~ 'Z) 2))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                           (LIST '~ 'Z)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES (LIST 'EXPT 'SRY 'N)
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y)
                                                                   (LIST 'EXPT
                                                                         'W
                                                                         2)))
                                                       (LIST 'COS
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOS
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N)))
                                                 (LIST 'TIMES
                                                       (LIST 'SIN
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOS
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N))
                                                       'W)))
                                     (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 4 'X 'Y)
                                                       (LIST 'EXPT 'W 2)))
                                           'X)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W 0)
                                     (LIST 'LESSP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 2 'X 'Y)))
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'GREATERP 'W 0))
                               (LIST 'AND
                                     (LIST 'NOT
                                           (LIST 'AND (LIST 'NUMBERP 'X)
                                                 (LIST 'NUMBERP 'W)
                                                 (LIST 'NUMBERP 'Y)))
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'W 'X)
                                           'COSH)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST 'EXPT (LIST '~ 'Z) 2)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'Z) 2))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                     (LIST '~ 'Z))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES (LIST 'EXPT 'SRY 'N)
                                           (LIST 'EXPT (MINUS 1) 'N)
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y)
                                                                   (LIST 'EXPT
                                                                         'W
                                                                         2)))
                                                       (LIST 'COS
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOS
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N)))
                                                 (LIST 'TIMES
                                                       (LIST 'SIN
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOS
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N))
                                                       'W)))
                                     (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 4 'X 'Y)
                                                       (LIST 'EXPT 'W 2)))
                                           'X)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W 0)
                                     (LIST 'GREATERP (LIST 'EXPT 'W 2)
                                           (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'LESSP 'W 0))
                               (LIST 'AND
                                     (LIST 'NOT
                                           (LIST 'AND (LIST 'NUMBERP 'X)
                                                 (LIST 'NUMBERP 'W)
                                                 (LIST 'NUMBERP 'Y)))
                                     (LIST 'FREEOF (LIST 'QUOTIENT 'W 'X)
                                           'COSH)))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST 'EXPT (LIST '~ 'Z) 2)
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                           (LIST 'EXPT (LIST '~ 'Z) 2))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                           (LIST '~ 'Z)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES (LIST 'EXPT 'SRY 'N)
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'W 2)
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y)))
                                                       (LIST 'COSH
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOSH
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N)))
                                                 (LIST 'TIMES
                                                       (LIST 'SINH
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOSH
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N))
                                                       'W)))
                                     (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'EXPT 'W 2)
                                                       (LIST 'TIMES 4 'X 'Y)))
                                           'X)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'NUMBERP 'X)
                                           (LIST 'NUMBERP 'W)
                                           (LIST 'NUMBERP 'Y))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST 'EXPT (LIST '~ 'Z) 2)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'Z) 2))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'W))
                                     (LIST '~ 'Z))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'SUB (LIST 'EQUAL 'SRX (LIST 'SQRT 'X))
                               (LIST 'EQUAL 'SRW (LIST 'SQRT 'W))
                               (LIST 'EQUAL 'SRY (LIST 'SQRT 'Y))
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'MINUS 'SRY) 'N)
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'W 2)
                                                                   (LIST 'TIMES
                                                                         4 'X
                                                                         'Y)))
                                                       (LIST 'COSH
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOSH
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N)))
                                                 (LIST 'TIMES
                                                       (LIST 'SINH
                                                             (LIST 'TIMES
                                                                   (LIST 'ACOSH
                                                                         (LIST
                                                                          'TIMES
                                                                          'SRX
                                                                          (LIST
                                                                           'QUOTIENT
                                                                           'W
                                                                           (LIST
                                                                            'TIMES
                                                                            2
                                                                            'SRY
                                                                            'X))))
                                                                   'N))
                                                       'W)))
                                     (LIST 'TIMES (LIST 'EXPT 'SRX 'N)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'EXPT 'W 2)
                                                       (LIST 'TIMES 4 'X 'Y)))
                                           'X)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'W)
                                     (LIST 'NUMBERP 'Y) (LIST 'GREATERP 'Y 0)
                                     (LIST 'GREATERP 'W (LIST 'TIMES 4 'X 'Y)))
                               (LIST 'NOT
                                     (LIST 'AND (LIST 'NUMBERP 'X)
                                           (LIST 'NUMBERP 'W)
                                           (LIST 'NUMBERP 'Y))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_AUX (LIST '~ 'F) (LIST '~ 'G) (LIST '~ 'Z)
                         (LIST '~ 'N))
                   (LIST 'INVZTRANS_END (LIST 'QUOTIENT 'F 'G) 'Z 'N))))) 
(AEVAL (LET '(INVZTRANS_AUXRULES))) 
(SETK 'INVZTRANS_ENDRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'ATAN
                                     (LIST 'SLASH (LIST 'SIN (LIST '~ 'B))
                                           (LIST 'DIFFERENCE
                                                 (LIST 'COS (LIST '~ 'B))
                                                 (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES 'B (LIST 'PLUS 'N 1)))
                                     (LIST 'PLUS 'N 1)))
                         (LIST 'OR (LIST 'NUMBERP 'B)
                               (LIST 'AND (LIST 'FREEOF 'B 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'B))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'ATAN
                                     (LIST 'SLASH (LIST 'SIN (LIST '~ 'B))
                                           (LIST 'PLUS (LIST '~ 'Z)
                                                 (LIST 'COS (LIST '~ 'B))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES 'B (LIST 'PLUS 'N 1)))
                                     (LIST 'PLUS 'N 1)))
                         (LIST 'OR (LIST 'NUMBERP 'A)
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'QUOTIENT (LIST '~ 'Z)
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT
                                                                   (LIST '~ 'Z)
                                                                   2)
                                                             (LIST 'TIMES
                                                                   (LIST '~ 'A)
                                                                   (LIST '~
                                                                         'Z)))
                                                       1)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES
                                           (LIST 'ACOS (LIST 'QUOTIENT 'A 2))
                                           (LIST 'PLUS 'N 1)))
                               (LIST 'PLUS 'N 1))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'A)
                                     (LIST 'GREATERP 'A 0)
                                     (LIST 'LEQ 'A (LIST 'MINUS 2)))
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'QUOTIENT (LIST '~ 'Z)
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'EXPT (LIST '~ 'Z)
                                                             2)
                                                       (LIST 'TIMES
                                                             (LIST '~ 'A)
                                                             (LIST '~ 'Z))
                                                       1)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES
                                           (LIST 'ACOS
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT 'A 2)))
                                           (LIST 'PLUS 'N 1)))
                               (LIST 'PLUS 'N 1))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'LESSP 'A 0)
                                     (LIST 'GEQ 'A (LIST 'MINUS 2)))
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'QUOTIENT
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT
                                                                   (LIST '~ 'Z)
                                                                   2)
                                                             (LIST 'TIMES
                                                                   (LIST '~ 'A)
                                                                   (LIST '~
                                                                         'Z)))
                                                       1))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'QUOTIENT
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'ACOS
                                                       (LIST 'MINUS
                                                             (LIST 'QUOTIENT 'A
                                                                   2)))
                                                 (LIST 'PLUS 'N 1)))
                                     (LIST 'PLUS 'N 1)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'LESSP 'A 0)
                                     (LIST 'GEQ 'A (LIST 'MINUS 2)))
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'QUOTIENT
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'EXPT (LIST '~ 'Z)
                                                             2)
                                                       (LIST 'TIMES
                                                             (LIST '~ 'A)
                                                             (LIST '~ 'Z))
                                                       1))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'QUOTIENT
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'ACOS
                                                       (LIST 'QUOTIENT 'A 2))
                                                 (LIST 'PLUS 'N 1)))
                                     (LIST 'PLUS 'N 1)))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'A)
                                     (LIST 'GREATERP 'A 0)
                                     (LIST 'LEQ 'A (LIST 'MINUS 2)))
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES
                               (LIST 'COS
                                     (LIST 'QUOTIENT (LIST '~ 'A)
                                           (LIST '~ 'Z)))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT (LIST '~ 'A)
                                                             2)))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'COS (LIST 'TIMES (LIST 'ASIN 'A) 'N))
                               (LIST 'FACTORIAL 'N))
                         (LIST 'OR
                               (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'LEQ 'A 1)
                                     (LIST 'GEQ 'A (LIST 'MINUS 1)))
                               (LIST 'AND (LIST 'FREEOF 'A 'Z)
                                     (LIST 'NOT (LIST 'NUMBERP 'A))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END (LIST '~ 'F) (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'PROG
                               (LIST 'DENOMINATOR 'RESULT 'SOLUTIONSET
                                     'SOLUTION '*FULLROOTS)
                               (LIST 'AEVAL
                                     (LIST 'ON (LIST 'LIST ''FULLROOTS)))
                               (LIST 'SETQ 'DENOMINATOR
                                     (LIST 'AEVAL (LIST 'LIST ''DEN ''F)))
                               (LIST 'SETQ 'SOLUTION
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''SOLVE 'DENOMINATOR
                                                 ''Z)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX 'SOLUTION)
                                                  (LIST 'REVALX ''ROOT_OF)))
                                      (LIST 'AEVAL
                                            (LIST 'REDERR
                                                  (LIST 'REVALX
                                                        "denominator could not be factorized")))))
                               (LIST 'SETQ 'SOLUTIONSET
                                     (LIST 'PROG
                                           (LIST 'I 'FORALL-RESULT
                                                 'FORALL-ENDPTR)
                                           (LIST 'SETQ 'I 1)
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'SOLUTION))
                                                              'I))
                                                  (LIST 'RETURN
                                                        (LIST 'MAKELIST NIL))))
                                           (LIST 'SETQ 'FORALL-RESULT
                                                 (LIST 'SETQ 'FORALL-ENDPTR
                                                       (LIST 'CONS
                                                             (LIST 'AEVAL*
                                                                   (LIST 'LIST
                                                                         ''PART
                                                                         (LIST
                                                                          'LIST
                                                                          ''PART
                                                                          'SOLUTION
                                                                          'I)
                                                                         2))
                                                             NIL)))
                                           'LOOPLABEL
                                           (LIST 'SETQ 'I
                                                 (LIST
                                                  (LIST 'LAMBDA
                                                        (LIST 'FORALL-RESULT)
                                                        (LIST 'AEVAL*
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    'FORALL-RESULT
                                                                    1)))
                                                  'I))
                                           (LIST 'COND
                                                 (LIST
                                                  (LIST '|AMINUSP:|
                                                        (LIST 'LIST
                                                              ''DIFFERENCE
                                                              (LIST 'AEVAL*
                                                                    (LIST 'LIST
                                                                          ''LENGTH
                                                                          'SOLUTION))
                                                              'I))
                                                  (LIST 'RETURN
                                                        (LIST 'CONS ''LIST
                                                              'FORALL-RESULT))))
                                           (LIST 'RPLACD 'FORALL-ENDPTR
                                                 (LIST 'CONS
                                                       (LIST 'AEVAL*
                                                             (LIST 'LIST ''PART
                                                                   (LIST 'LIST
                                                                         ''PART
                                                                         'SOLUTION
                                                                         'I)
                                                                   2))
                                                       NIL))
                                           (LIST 'SETQ 'FORALL-ENDPTR
                                                 (LIST 'CDR 'FORALL-ENDPTR))
                                           (LIST 'GO 'LOOPLABEL)))
                               (LIST 'SETQ 'RESULT
                                     (LIST 'PROG (LIST 'A 'FORALL-RESULT)
                                           (LIST 'SETQ 'A
                                                 (LIST 'GETRLIST
                                                       (LIST 'AEVAL
                                                             'SOLUTIONSET)))
                                           (LIST 'SETQ 'FORALL-RESULT 0) 'LAB1
                                           (LIST 'COND
                                                 (LIST (LIST 'NULL 'A)
                                                       (LIST 'RETURN
                                                             'FORALL-RESULT)))
                                           (LIST 'SETQ 'FORALL-RESULT
                                                 (LIST 'AEVAL*
                                                       (LIST 'LIST ''PLUS
                                                             (LIST
                                                              (LIST 'LAMBDA
                                                                    (LIST 'A)
                                                                    (LIST
                                                                     'AEVAL
                                                                     (LIST
                                                                      'LIST
                                                                      ''RESIDUE
                                                                      (LIST
                                                                       'LIST
                                                                       ''TIMES
                                                                       ''F
                                                                       (LIST
                                                                        'LIST
                                                                        ''EXPT
                                                                        ''Z
                                                                        (LIST
                                                                         'LIST
                                                                         ''DIFFERENCE
                                                                         ''N
                                                                         1)))
                                                                      ''Z 'A)))
                                                              (LIST 'CAR 'A))
                                                             'FORALL-RESULT)))
                                           (LIST 'SETQ 'A (LIST 'CDR 'A))
                                           (LIST 'GO 'LAB1)))
                               (LIST 'RETURN (LIST 'AEVAL 'RESULT)))
                         (LIST 'TYPE_RATPOLY 'F 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'Z) (LIST '~ (LIST '~ 'K)))
                               (LIST 'EXPT
                                     (LIST 'PLUS 'Z (LIST '~ (LIST '~ 'A)))
                                     (LIST '~ (LIST '~ 'M))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'BINOMIAL
                                     (LIST 'PLUS 'N (LIST 'DIFFERENCE 'K 1))
                                     (LIST 'DIFFERENCE 'M 1))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'MINUS 'A)
                                           (LIST 'PLUS 'N 'K))
                                     (LIST 'EXPT (LIST 'MINUS 'A) 'M)))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z) (LIST 'FREEOF 'M 'Z)
                               (LIST 'FREEOF 'A 'Z)
                               (LIST 'OR (LIST 'NOT (LIST 'NUMBERP 'K))
                                     (LIST 'AND (LIST 'NUMBERP 'K)
                                           (LIST 'FIXP 'K)))
                               (LIST 'OR (LIST 'NOT (LIST 'NUMBERP 'M))
                                     (LIST 'AND (LIST 'NUMBERP 'M)
                                           (LIST 'FIXP 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT (LIST '~ 'K) (LIST '~ 'Z)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'EXPT 'K 'N)
                               (LIST 'FACTORIAL 'N))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST '~ 'Z)))
                               (LIST '~ 'Z))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'N 'K)
                               (LIST 'QUOTIENT (LIST 'EXPT 'K 'N)
                                     (LIST 'FACTORIAL 'N)))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'EXPT (LIST 'MINUS 'K) 'N)
                               (LIST 'FACTORIAL 'N))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT 'E
                                           (LIST 'QUOTIENT (LIST '~ 'K)
                                                 (LIST '~ 'Z)))
                                     (LIST '~ 'Z)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'QUOTIENT 'N 'K)
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT (LIST 'MINUS 'K) 'N)
                                           (LIST 'FACTORIAL 'N))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT (LIST '~ 'K)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'EXPT (LIST 'QUOTIENT 'K 'J) 'N)
                               (LIST 'FACTORIAL 'N))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST '~ 'Z))))
                               (LIST '~ 'Z))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 'N (LIST 'QUOTIENT 'K 'J))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'QUOTIENT 'K 'J) 'N)
                                     (LIST 'FACTORIAL 'N)))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'MINUS (LIST 'QUOTIENT 'K 'J))
                                     'N)
                               (LIST 'FACTORIAL 'N))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT 'E
                                           (LIST 'QUOTIENT (LIST '~ 'K)
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'J))
                                                       (LIST '~ 'Z))))
                                     (LIST '~ 'Z)))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 'N
                                     (LIST 'MINUS (LIST 'QUOTIENT 'K 'J)))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'MINUS (LIST 'QUOTIENT 'K 'J))
                                           'N)
                                     (LIST 'FACTORIAL 'N)))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES
                               (LIST 'COS
                                     (LIST 'QUOTIENT
                                           (LIST 'SIN (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ 'Z)))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT
                                           (LIST 'COS (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'COS (LIST 'TIMES 'B 'N))
                               (LIST 'FACTORIAL 'N))
                         (LIST 'FREEOF 'B 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES
                               (LIST 'SIN
                                     (LIST 'QUOTIENT
                                           (LIST 'SIN (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ 'Z)))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT
                                           (LIST 'COS (LIST '~ (LIST '~ 'B)))
                                           (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'SIN (LIST 'TIMES 'B 'N))
                               (LIST 'FACTORIAL 'N))
                         (LIST 'FREEOF 'B 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'COSH
                               (LIST 'QUOTIENT (LIST '~ 'K)
                                     (LIST 'SQRT (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'EXPT 'K (LIST 'TIMES 2 'N))
                               (LIST 'FACTORIAL (LIST 'TIMES 2 'N)))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'COS
                               (LIST 'QUOTIENT (LIST '~ 'K)
                                     (LIST 'SQRT (LIST '~ 'Z))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'MINUS (LIST 'EXPT 'K 2)) 'N)
                               (LIST 'FACTORIAL (LIST 'TIMES 2 'N)))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'COSH
                               (LIST 'QUOTIENT (LIST '~ 'K)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                     (LIST 'TIMES 2 'N))
                               (LIST 'FACTORIAL (LIST 'TIMES 2 'N)))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'COS
                               (LIST 'QUOTIENT (LIST '~ 'K)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EXPT
                                     (LIST 'MINUS
                                           (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                                 2))
                                     'N)
                               (LIST 'FACTORIAL (LIST 'TIMES 2 'N)))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT 'K (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT
                                                 (LIST 'MINUS (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I 'K (LIST 'MINUS (LIST 'EXPT 'K 2))
                               (LIST 'QUOTIENT 'N
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'MINUS (LIST 'EXPT 'K 2)) 'N)
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT (LIST 'MINUS (LIST 'EXPT 'K 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT 'K (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'MINUS (LIST 'EXPT 'K 2)) 'N)
                                     (LIST 'TIMES 'I
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                       1)))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT
                                                 (LIST 'MINUS (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K (LIST 'MINUS (LIST 'EXPT 'K 2))
                               (LIST 'QUOTIENT 'N
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'SQRT (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K (LIST 'MINUS (LIST 'EXPT 'K 2))
                               (LIST 'QUOTIENT 'N
                                     (LIST 'TIMES 'I
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                       1)))))
                         (LIST 'FREEOF 'K 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'K 'J)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                           (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'K 'J)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                           (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'MINUS
                                           (LIST 'TIMES (LIST '~ 'B)
                                                 (LIST '~ 'Z))))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES
                                                 (LIST 'SQRT (LIST '~ 'B))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT (LIST 'MINUS (LIST 'EXPT 'K 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'QUOTIENT (LIST 'EXPT 'K 2)
                                                 'B)
                                           'N)
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'SQRT
                                           (LIST 'MINUS (LIST 'EXPT 'K 2)))
                                     'J)
                               (LIST 'QUOTIENT
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT 'K 2))
                                                 'N)
                                           (LIST 'EXPT 'J (LIST 'TIMES 2 'N)))
                                     (LIST 'TIMES 'I
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                       1)))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST 'MINUS (LIST '~ 'Z)))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'K 'J)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                           (LIST 'TIMES 2 'N))
                                     (LIST 'TIMES 'I
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                       1)))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'MINUS
                                           (LIST 'TIMES (LIST '~ 'B)
                                                 (LIST '~ 'Z))))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES
                                                 (LIST 'SQRT (LIST '~ 'B))
                                                 (LIST 'SQRT (LIST '~ 'Z))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'K
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'MINUS
                                                 (LIST 'QUOTIENT
                                                       (LIST 'EXPT 'K 2) 'B))
                                           'N)
                                     (LIST 'TIMES 'I
                                           (LIST 'FACTORIAL
                                                 (LIST 'PLUS (LIST 'TIMES 2 'N)
                                                       1)))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'B 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS
                                                             (LIST '~ 'Z)))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES 'I (LIST 'QUOTIENT 'K 'J)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'QUOTIENT 'K 'J)
                                           (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'Z))
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'K)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'J))
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS
                                                             (LIST '~ 'Z)))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'SQRT
                                           (LIST 'MINUS (LIST 'EXPT 'K 2)))
                                     'J)
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT
                                           (LIST 'QUOTIENT
                                                 (LIST 'SQRT
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT 'K
                                                                   2)))
                                                 'J)
                                           (LIST 'TIMES 2 'N))
                                     (LIST 'FACTORIAL
                                           (LIST 'PLUS (LIST 'TIMES 2 'N) 1))))
                         (LIST 'AND (LIST 'FREEOF 'K 'Z)
                               (LIST 'FREEOF 'J 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'QUOTIENT (LIST '~ 'Z)
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES
                                                             (LIST '~
                                                                   (LIST '~
                                                                         'B))
                                                             (LIST '~ 'Z))
                                                       (LIST '~ 'A))))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'MINUS (LIST 'QUOTIENT 'A 'B))
                                     (LIST 'PLUS 'N 1))
                               (LIST 'PLUS 'N 1))
                         (LIST 'AND (LIST 'FREEOF 'A 'Z)
                               (LIST 'FREEOF 'B 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END
                         (LIST 'TIMES (LIST '~ 'Z)
                               (LIST 'LOG
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'B))
                                                       (LIST '~ 'Z))
                                                 (LIST '~ 'A))
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST '~ 'Z)))))
                         (LIST '~ 'Z) (LIST '~ 'N))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'INVZTRANS1
                                     (LIST 'TIMES 'Z
                                           (LIST 'LOG
                                                 (LIST 'TIMES 'B
                                                       (LIST 'QUOTIENT 'Z
                                                             (LIST 'PLUS
                                                                   (LIST 'TIMES
                                                                         'B 'Z)
                                                                   'A)))))
                                     'Z 'N))
                         (LIST 'AND (LIST 'FREEOF 'A 'Z)
                               (LIST 'FREEOF 'B 'Z))))
             (LIST 'REPLACEBY
                   (LIST 'INVZTRANS_END (LIST '~ 'F) (LIST '~ 'Z) (LIST '~ 'N))
                   'FAIL)))) 
(AEVAL (LET '(INVZTRANS_ENDRULES))) 
(AEVAL 'NIL) 
(ENDMODULE) 