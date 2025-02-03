(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTF)) 
(AEVAL
 (OPERATOR
  (LIST 'CASE20 'CASE21 'CASE22 'CASE23 'CASE24 'CASE25 'CASE26 'CASE27 'CASE28
        'CASE29 'CASE30 'CASE31 'CASE32 'CASE33 'CASE34 'CASE35))) 
(SETK 'CASE20_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE20 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'N 0) (LIST 'GREATERP 'M 0)
                               (LIST 'GREATERP 'EPSILON 0) (LIST 'LESSP 'PHI 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST12 NIL
                                           NIL NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE20_RULES))) 
(SETK 'CASE21_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE21 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'M 0) (LIST 'GREATERP 'N 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'GREATERP 'PHI 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST12 NIL NIL NIL
                                           NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE21_RULES))) 
(SETK 'CASE22_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE22 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL (LIST 'TIMES 'K 'L) 0)
                               (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST10 ''TEST12 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE22_RULES))) 
(SETK 'CASE23_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE23 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL (LIST 'TIMES 'M 'N) 0)
                               (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST10 ''TEST12 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE23_RULES))) 
(SETK 'CASE24_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE24 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'GREATERP (LIST 'PLUS 'M 'N) 'P)
                               (LIST 'EQUAL 'L 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'K 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'LESSP 'EPSILON 0)
                               (LIST 'MYLESSP
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     (LIST 'PLUS 'M (LIST 'DIFFERENCE 'N 'P)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE24_RULES))) 
(SETK 'CASE25_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE25 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'GREATERP (LIST 'PLUS 'M 'N) 'Q)
                               (LIST 'EQUAL 'K 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'L 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'LESSP 'EPSILON 0)
                               (LIST 'MYLESSP
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     (LIST 'PLUS 'M (LIST 'DIFFERENCE 'N 'Q)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE25_RULES))) 
(SETK 'CASE26_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE26 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'P (LIST 'DIFFERENCE 'Q 1))
                               (LIST 'EQUAL 'L 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'K 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     'EPSILON (LIST 'PLUS 'EPSILON 1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE26_RULES))) 
(SETK 'CASE27_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE27 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'P (LIST 'PLUS 'Q 1))
                               (LIST 'EQUAL 'K 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'L 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     'EPSILON (LIST 'PLUS 'EPSILON 1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE27_RULES))) 
(SETK 'CASE28_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE28 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'LESSP 'P (LIST 'DIFFERENCE 'Q 1))
                               (LIST 'EQUAL 'L 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'K 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     'EPSILON
                                     (LIST 'PLUS 'M (LIST 'DIFFERENCE 'N 'P)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE28_RULES))) 
(SETK 'CASE29_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE29 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'GREATERP 'P (LIST 'PLUS 'Q 1))
                               (LIST 'EQUAL 'K 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'L 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'OMEGA)
                                                 (LIST 'REPART 'OMEGA)))
                                     'EPSILON
                                     (LIST 'PLUS 'M (LIST 'DIFFERENCE 'N 'Q)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST10
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE29_RULES))) 
(SETK 'CASE30_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE30 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'N 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP (LIST 'PLUS 'K 'L) 'U)
                               (LIST 'GREATERP 'M 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'LESSP 'DELTA 0)
                               (LIST 'MYLESSP
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     (LIST 'PLUS 'K (LIST 'DIFFERENCE 'L 'U)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE30_RULES))) 
(SETK 'CASE31_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE31 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'M 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP (LIST 'PLUS 'K 'L) 'V)
                               (LIST 'GREATERP 'N 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'LESSP 'DELTA 0)
                               (LIST 'MYLESSP
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     (LIST 'PLUS 'K (LIST 'DIFFERENCE 'L 'V)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE31_RULES))) 
(SETK 'CASE32_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE32 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'N 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'EQUAL 'U (LIST 'DIFFERENCE 'V 1))
                               (LIST 'GREATERP 'M 0)
                               (LIST 'GREATERP 'EPSILON 0) (LIST 'GEQ 'DELTA 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     'DELTA (LIST 'PLUS 'DELTA 1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE32_RULES))) 
(SETK 'CASE33_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE33 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'M 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'EQUAL 'U (LIST 'PLUS 'V 1))
                               (LIST 'GREATERP 'N 0)
                               (LIST 'GREATERP 'EPSILON 0) (LIST 'GEQ 'DELTA 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     'DELTA (LIST 'PLUS 'DELTA 1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE33_RULES))) 
(SETK 'CASE34_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE34 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'N 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'LESSP 'U (LIST 'DIFFERENCE 'V 1))
                               (LIST 'GREATERP 'M 0)
                               (LIST 'GREATERP 'EPSILON 0) (LIST 'GEQ 'DELTA 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     'DELTA
                                     (LIST 'PLUS 'K (LIST 'DIFFERENCE 'L 'U)
                                           1))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL 'TEST_15 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE34_RULES))) 
(SETK 'CASE35_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE35 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'K) (LIST '~ 'L) (LIST '~ 'U)
                         (LIST '~ 'V) (LIST '~ 'DELTA) (LIST '~ 'EPSILON)
                         (LIST '~ 'SIGMA) (LIST '~ 'OMEGA) (LIST '~ 'RHO)
                         (LIST '~ 'ETA) (LIST '~ 'MU) (LIST '~ 'R1)
                         (LIST '~ 'R2) (LIST '~ 'PHI) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_1B) (LIST '~ 'TEST_2) (LIST '~ 'TEST_3)
                         (LIST '~ 'TEST_4) (LIST '~ 'TEST_5) (LIST '~ 'TEST_6)
                         (LIST '~ 'TEST_7) (LIST '~ 'TEST_8) (LIST '~ 'TEST_9)
                         (LIST '~ 'TEST_10) (LIST '~ 'TEST_11)
                         (LIST '~ 'TEST_12) (LIST '~ 'TEST_13)
                         (LIST '~ 'TEST_14) (LIST '~ 'TEST_15))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'M 0) (LIST 'EQUAL 'PHI 0)
                               (LIST 'GREATERP 'U (LIST 'PLUS 'V 1))
                               (LIST 'GREATERP 'N 0)
                               (LIST 'GREATERP 'EPSILON 0) (LIST 'GEQ 'DELTA 0)
                               (LIST 'TEST_ARG
                                     (LIST 'ABS
                                           (LIST 'ATAN2 (LIST 'IMPART 'SIGMA)
                                                 (LIST 'REPART 'SIGMA)))
                                     'DELTA
                                     (LIST 'PLUS 'K (LIST 'DIFFERENCE 'L 'V)
                                           1))
                               (LIST 'EQUAL 'TEST_1A 'T)
                               (LIST 'EQUAL 'TEST_1B 'T)
                               (LIST 'EQUAL 'TEST_3 'T)
                               (LIST 'EQUAL 'TEST_12 'T)
                               (LIST 'EQUAL 'TEST_14 'T)
                               (LIST 'EQUAL 'TEST_15 'T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST12
                                           ''TEST14 ''TEST15 NIL NIL NIL NIL)
                                     'T))))))) 
(AEVAL (LET '(CASE35_RULES))) 
(AEVAL (FLAG '(TEST_ARG) 'BOOLEAN)) 
(PUT 'TEST_ARG 'NUMBER-OF-ARGS 3) 
(FLAG '(TEST_ARG) 'OPFN) 
(PUT 'TEST_ARG 'DEFINED-ON-LINE '382) 
(PUT 'TEST_ARG 'DEFINED-IN-FILE 'DEFINT/DEFINTF.RED) 
(PUT 'TEST_ARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TEST_ARG (A B C)
    (PROG (*ROUNDED DMODE*)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (AEVAL (ON (LIST 'ROUNDED)))
         (COND
          ((AND (EVALLESSP (AEVAL (LIST 'TIMES B 'PI)) (AEVAL A))
                (EVALLESSP (AEVAL A) (AEVAL (LIST 'TIMES C 'PI))))
           (PROGN (AEVAL (OFF (LIST 'ROUNDED))) (RETURN (AEVAL 'T))))
          (T (PROGN (AEVAL (OFF (LIST 'ROUNDED))) (RETURN (AEVAL 'NIL)))))
         (AEVAL 'NIL)))
       (T (RETURN (AEVAL 'T)))))) 
(AEVAL 'NIL) 
(PUT 'TRANSFORM_TEST 'NUMBER-OF-ARGS 8) 
(PUT 'TRANSFORM_TEST 'DEFINED-ON-LINE '395) 
(PUT 'TRANSFORM_TEST 'DEFINED-IN-FILE 'DEFINT/DEFINTF.RED) 
(PUT 'TRANSFORM_TEST 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TRANSFORM_TEST (N1 N2 N3 N4 N5 N6 N7 N8)
    (PROG (LST TEMP COND_TEST)
      (COND ((NEQ TRANSFORM_TST T) (RETURN T))
            (T
             (PROGN
              (SETQ LST (LIST N1 N2 N3 N4 N5 N6 N7 N8))
              (PROG (I)
                (SETQ I LST)
               LAB
                (COND ((NULL I) (RETURN NIL)))
                ((LAMBDA (I)
                   (COND
                    (I
                     (SETQ TEMP
                             (CONS (LISPEVAL (CDR (ASSOC I TRANSFORM_LST)))
                                   TEMP)))))
                 (CAR I))
                (SETQ I (CDR I))
                (GO LAB))
              NIL
              (SETQ TEMP (CONS 'AND TEMP))
              (PROG (J)
                (SETQ J SPEC_COND)
               LAB
                (COND ((NULL J) (RETURN NIL)))
                ((LAMBDA (J) (COND ((EQUAL J TEMP) (SETQ COND_TEST T))))
                 (CAR J))
                (SETQ J (CDR J))
                (GO LAB))
              (COND ((NEQ COND_TEST T) (SETQ SPEC_COND (CONS TEMP SPEC_COND))))
              (RETURN NIL)
              NIL))))) 
(FLAG '(TRANSFORM_TEST) 'OPFN) 
(FLAG '(SIGMA_TST) 'BOOLEAN) 
(PUT 'SIGMA_TST 'NUMBER-OF-ARGS 1) 
(FLAG '(SIGMA_TST) 'OPFN) 
(PUT 'SIGMA_TST 'DEFINED-ON-LINE '416) 
(PUT 'SIGMA_TST 'DEFINED-IN-FILE 'DEFINT/DEFINTF.RED) 
(PUT 'SIGMA_TST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIGMA_TST (SIGMA)
    (PROG (TEST)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (COND ((EVALGREATERP (AEVAL SIGMA) 0) (RETURN (AEVAL 'T)))
               (T (RETURN (AEVAL 'NIL))))))
       (T
        (PROGN
         (COND
          ((EVALNEQ (AEVAL TEST) (AEVAL 'T))
           (PROGN
            (SETQ TRANSFORM_LST
                    (CONS (CONS 'SIGMA_COND '(LIST 'GREATERP 'SIGMA 0))
                          TRANSFORM_LST))
            (SETQ TEST (AEVAL 'T)))))
         (RETURN (AEVAL (LIST 'REVAL 'T)))))))) 
(FLAG '(OMEGA_TST) 'BOOLEAN) 
(PUT 'OMEGA_TST 'NUMBER-OF-ARGS 1) 
(PUT 'OMEGA_TST 'DEFINED-ON-LINE '431) 
(PUT 'OMEGA_TST 'DEFINED-IN-FILE 'DEFINT/DEFINTF.RED) 
(PUT 'OMEGA_TST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OMEGA_TST (OMEGA)
    (PROG (TEST)
      (COND
       ((NEQ TRANSFORM_TST T)
        (PROGN (COND ((GREATERP OMEGA 0) (RETURN T)) (T (RETURN NIL)))))
       (T
        (PROGN
         (COND
          ((NEQ TEST T)
           (PROGN
            (SETQ TRANSFORM_LST
                    (CONS (CONS 'OMEGA_COND '(LIST 'GREATERP 'OMEGA 0))
                          TRANSFORM_LST))
            (SETQ TEST T))))
         (RETURN (REVAL1 T T))))))) 
(ENDMODULE) 