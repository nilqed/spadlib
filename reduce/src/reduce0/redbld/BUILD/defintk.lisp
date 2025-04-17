(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTK)) 
(AEVAL
 (OPERATOR
  (LIST 'TEST_CASES2 'CASE1 'CASE2 'CASE3 'CASE4 'CASE5 'CASE6 'CASE7 'CASE8
        'CASE9 'CASE10 'CASE11 'CASE12 'CASE13 'CASE14 'CASE15 'CASE16 'CASE17
        'CASE18 'CASE19))) 
(SETK 'TEST_CASES2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TEST_CASES2 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'OR
                               (LIST 'EQUAL
                                     (LIST 'CASE1 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE2 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE3 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE4 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE5 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE6 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE7 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE8 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE9 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE10 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE11 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE12 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE13 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE14 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE15 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE16 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE17 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE18 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE19 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE20 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE21 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE22 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE23 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE24 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE25 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE26 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE27 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE28 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE29 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE30 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE31 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE32 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE33 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE34 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T)
                               (LIST 'EQUAL
                                     (LIST 'CASE35 'M 'N 'P 'Q 'K 'L 'U 'V
                                           'DELTA 'EPSILON 'SIGMA 'OMEGA 'RHO
                                           'ETA 'MU 'R1 'R2 'PHI 'TEST_1A
                                           'TEST_1B 'TEST_2 'TEST_3 'TEST_4
                                           'TEST_5 'TEST_6 'TEST_7 'TEST_8
                                           'TEST_9 'TEST_10 'TEST_11 'TEST_12
                                           'TEST_13 'TEST_14 'TEST_15)
                                     'T))))))) 
(AEVAL (LET '(TEST_CASES2_RULES))) 
(SETK 'CASE1_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE1 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'NEQ (LIST 'TIMES 'M 'N 'K 'L) 0)
                               (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST_2 ''TEST3
                                           ''TEST10 ''TEST12 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE1_RULES))) 
(SETK 'CASE2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE2 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'U 'V) (LIST 'EQUAL 'DELTA 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'SIGMA_TST 'SIGMA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'RHO) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST12 ''SIGMA_COND NIL NIL NIL
                                           NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE2_RULES))) 
(SETK 'CASE3_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE3 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'P 'Q)
                               (LIST 'EQUAL 'EPSILON 0)
                               (LIST 'GREATERP 'DELTA 0)
                               (LIST 'EQUAL (LIST 'OMEGA_TST 'OMEGA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'ETA) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST 'TEST_2 ''TEST3
                                           ''TEST10 ''OMEGA_COND NIL NIL NIL
                                           NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE3_RULES))) 
(SETK 'CASE4_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE4 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'P 'Q) (LIST 'EQUAL 'U 'V)
                               (LIST 'EQUAL 'DELTA 0) (LIST 'EQUAL 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'SIGMA_TST 'SIGMA) ''T)
                               (LIST 'EQUAL (LIST 'OMEGA_TST 'OMEGA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'ETA) 1)
                               (LIST 'LESSP (LIST 'REPART 'RHO) 1)
                               (LIST 'NEQ (LIST 'EXPT 'SIGMA 'R1)
                                     (LIST 'EXPT 'OMEGA 'R2))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST_2 ''TEST3
                                           ''SIGMA_COND ''OMEGA_COND NIL NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE4_RULES))) 
(SETK 'CASE5_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE5 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'P 'Q) (LIST 'EQUAL 'U 'V)
                               (LIST 'EQUAL 'DELTA 0) (LIST 'EQUAL 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'SIGMA_TST 'SIGMA) ''T)
                               (LIST 'EQUAL (LIST 'OMEGA_TST 'OMEGA) ''T)
                               (LIST 'LESSP
                                     (LIST 'REPART (LIST 'PLUS 'ETA 'RHO)) 1)
                               (LIST 'NEQ (LIST 'EXPT 'SIGMA 'R1)
                                     (LIST 'EXPT 'OMEGA 'R2))
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''SIGMA_COND ''OMEGA_COND NIL NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE5_RULES))) 
(SETK 'CASE6_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE6 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'GREATERP 'P 'Q)
                               (LIST 'GREATERP 'K 0) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_5 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST5
                                           ''TEST10 ''TEST13 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE6_RULES))) 
(SETK 'CASE7_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE7 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'LESSP 'P 'Q) (LIST 'GREATERP 'L 0)
                               (LIST 'GREATERP 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_4 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST4
                                           ''TEST10 ''TEST13 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE7_RULES))) 
(SETK 'CASE8_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE8 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'GREATERP 'U 'V)
                               (LIST 'GREATERP 'M 0) (LIST 'GEQ 'DELTA 0)
                               (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_7 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST3 ''TEST7
                                           ''TEST11 ''TEST12 NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE8_RULES))) 
(SETK 'CASE9_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE9 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'LESSP 'U 'V) (LIST 'GREATERP 'N 0)
                               (LIST 'GEQ 'DELTA 0) (LIST 'GREATERP 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_6 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_12 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST6 ''TEST11 ''TEST12 NIL NIL
                                           NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE9_RULES))) 
(SETK 'CASE10_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE10 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'GREATERP 'P 'Q) (LIST 'EQUAL 'U 'V)
                               (LIST 'EQUAL 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'SIGMA_TST 'SIGMA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'RHO) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_5 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST5 ''TEST13 ''SIGMA_COND NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE10_RULES))) 
(SETK 'CASE11_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE11 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'LESSP 'P 'Q) (LIST 'EQUAL 'U 'V)
                               (LIST 'EQUAL 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'SIGMA_TST 'SIGMA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'RHO) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_4 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST4 ''TEST13 ''SIGMA_COND NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE11_RULES))) 
(SETK 'CASE12_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE12 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'P 'Q) (LIST 'GREATERP 'U 'V)
                               (LIST 'GEQ 'DELTA 0) (LIST 'EQUAL 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'OMEGA_TST 'OMEGA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'ETA) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_7 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST7 ''TEST11 ''OMEGA_COND NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE12_RULES))) 
(SETK 'CASE13_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE13 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'P 'Q) (LIST 'LESSP 'U 'V)
                               (LIST 'GEQ 'DELTA 0) (LIST 'EQUAL 'EPSILON 0)
                               (LIST 'EQUAL (LIST 'OMEGA_TST 'OMEGA) ''T)
                               (LIST 'LESSP (LIST 'REPART 'ETA) 1)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_6 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST6 ''TEST11 ''OMEGA_COND NIL
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE13_RULES))) 
(SETK 'CASE14_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE14 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'LESSP 'P 'Q) (LIST 'GREATERP 'U 'V)
                               (LIST 'GEQ 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_4 ''T)
                               (LIST 'EQUAL 'TEST_7 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST4 ''TEST7 ''TEST11 ''TEST13
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE14_RULES))) 
(SETK 'CASE15_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE15 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'GREATERP 'P 'Q) (LIST 'LESSP 'U 'V)
                               (LIST 'GEQ 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_5 ''T)
                               (LIST 'EQUAL 'TEST_6 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST5 ''TEST6 ''TEST11 ''TEST13
                                           NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE15_RULES))) 
(SETK 'CASE16_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE16 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'GREATERP 'P 'Q)
                               (LIST 'GREATERP 'U 'V) (LIST 'GEQ 'DELTA 0)
                               (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_5 ''T)
                               (LIST 'EQUAL 'TEST_7 ''T)
                               (LIST 'EQUAL 'TEST_8 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST5 ''TEST7 ''TEST8 ''TEST11
                                           ''TEST13 ''TEST14)
                                     ''T))))))) 
(AEVAL (LET '(CASE16_RULES))) 
(SETK 'CASE17_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE17 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'LESSP 'P 'Q) (LIST 'LESSP 'U 'V)
                               (LIST 'GEQ 'DELTA 0) (LIST 'GEQ 'EPSILON 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_4 ''T)
                               (LIST 'EQUAL 'TEST_6 ''T)
                               (LIST 'EQUAL 'TEST_9 ''T)
                               (LIST 'EQUAL 'TEST_11 ''T)
                               (LIST 'EQUAL 'TEST_13 ''T)
                               (LIST 'EQUAL 'TEST_14 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST3
                                           ''TEST4 ''TEST6 ''TEST9 ''TEST11
                                           ''TEST13 ''TEST14)
                                     ''T))))))) 
(AEVAL (LET '(CASE17_RULES))) 
(SETK 'CASE18_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE18 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'L 0) (LIST 'GREATERP 'K 0)
                               (LIST 'GREATERP 'DELTA 0)
                               (LIST 'GREATERP 'PHI 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_2 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST2 ''TEST10 NIL
                                           NIL NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE18_RULES))) 
(SETK 'CASE19_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE19 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
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
                         (LIST 'AND (LIST 'EQUAL 'K 0) (LIST 'GREATERP 'L 0)
                               (LIST 'GREATERP 'DELTA 0) (LIST 'LESSP 'PHI 0)
                               (LIST 'EQUAL 'TEST_1A ''T)
                               (LIST 'EQUAL 'TEST_1B ''T)
                               (LIST 'EQUAL 'TEST_3 ''T)
                               (LIST 'EQUAL 'TEST_10 ''T)
                               (LIST 'EQUAL
                                     (LIST 'TRANSFORM_TEST ''TEST10 NIL NIL NIL
                                           NIL NIL NIL NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE19_RULES))) 
(AEVAL 'NIL) 
(ENDMODULE) 