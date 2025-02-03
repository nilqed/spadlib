(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTI)) 
(AEVAL
 (OPERATOR
  (LIST 'TEST_CASES 'CASE_1 'CASE_2 'CASE_3 'CASE_4 'CASE_5 'CASE_6 'CASE_7))) 
(SETK 'TEST_CASES_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TEST_CASES (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'EQUAL
                                     (LIST 'CASE_1 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_2 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_3 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_4 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_5 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_6 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T)
                               (LIST 'EQUAL
                                     (LIST 'CASE_7 'M 'N 'P 'Q 'DELTA 'XI 'ETA
                                           'TEST_1 'TEST_1A 'TEST_2)
                                     ''T))))))) 
(AEVAL (LET '(TEST_CASES_RULES))) 
(SETK 'CASE_1_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_1 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'LEQ 1 'N) (LIST 'LEQ 'N 'P)
                                     (LIST 'LESSP 'P 'Q) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'GREATERP 'DELTA 0)
                                     (LIST 'NEQ 'ETA 0)
                                     (LIST 'EQUAL
                                           (LIST 'MYLESSP
                                                 (LIST 'ABS
                                                       (LIST 'ATAN2
                                                             (LIST 'IMPART
                                                                   'ETA)
                                                             (LIST 'REPART
                                                                   'ETA)))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NOT
                                           (LIST 'AND (LIST 'EQUAL 'N 0)
                                                 (LIST 'EQUAL 'M
                                                       (LIST 'PLUS 'P 1))))
                                     (LIST 'GREATERP 'DELTA 0)
                                     (LIST 'NEQ 'ETA 0)
                                     (LIST 'EQUAL
                                           (LIST 'MYLESSP
                                                 (LIST 'ABS
                                                       (LIST 'ATAN2
                                                             (LIST 'IMPART
                                                                   'ETA)
                                                             (LIST 'REPART
                                                                   'ETA)))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 0 'M)
                                     (LIST 'LEQ 'M 'Q) (LIST 'EQUAL 'Q 'P)
                                     (LIST 'GREATERP 'DELTA 0)
                                     (LIST 'NEQ 'ETA 0)
                                     (LIST 'EQUAL
                                           (LIST 'MYLESSP
                                                 (LIST 'ABS
                                                       (LIST 'ATAN2
                                                             (LIST 'IMPART
                                                                   'ETA)
                                                             (LIST 'REPART
                                                                   'ETA)))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'NOT
                                           (LIST 'EQUAL
                                                 (LIST 'ARG_TEST1
                                                       (LIST 'ABS
                                                             (LIST 'ATAN2
                                                                   (LIST
                                                                    'IMPART
                                                                    'ETA)
                                                                   (LIST
                                                                    'REPART
                                                                    'ETA)))
                                                       'DELTA)
                                                 ''T))
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_1_RULES))) 
(SETK 'CASE_2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_2 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'AND (LIST 'EQUAL 'N 0)
                               (LIST 'LEQ 1 (LIST 'PLUS 'P 1))
                               (LIST 'LEQ (LIST 'PLUS 'P 1) 'M)
                               (LIST 'LEQ 'M 'Q) (LIST 'GREATERP 'DELTA 0)
                               (LIST 'EQUAL
                                     (LIST 'MYLESSP
                                           (LIST 'ABS
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA)))
                                           'DELTA)
                                     ''T)
                               (LIST 'EQUAL 'TEST_1 ''T)
                               (LIST 'EQUAL (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                     ''T))))))) 
(AEVAL (LET '(CASE_2_RULES))) 
(SETK 'CASE_3_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_3 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'LEQ 0 'N) (LIST 'LEQ 'N 'P)
                                     (LIST 'LESSP 'P 'Q) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'GREATERP 'DELTA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST2
                                                 (LIST 'ABS
                                                       (LIST 'ATAN2
                                                             (LIST 'IMPART
                                                                   'ETA)
                                                             (LIST 'REPART
                                                                   'ETA)))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'LEQ 0 'N) (LIST 'LEQ 'N 'P)
                                     (LIST 'LEQ 'P (LIST 'DIFFERENCE 'Q 2))
                                     (LIST 'EQUAL 'DELTA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST3A
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 0)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_3_RULES))) 
(SETK 'CASE_4_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_4 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'LEQ 0 'N) (LIST 'LEQ 'N 'P)
                                     (LIST 'LEQ 1 'M) (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 2))
                                     (LIST 'NEQ 'ETA 0) (LIST 'LEQ 'DELTA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'LEQ 0 'N) (LIST 'LEQ 'N 'P)
                                     (LIST 'LEQ 1 'M) (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 2))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'DELTA 1)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST3
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 2))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'DELTA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST3A
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_4_RULES))) 
(SETK 'CASE_5_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_5 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST4
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'XI 2)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST5
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA 'XI)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'XI 2)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST6
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA 'XI)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 1 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'XI 1)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST6A
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA 'XI)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_5_RULES))) 
(SETK 'CASE_6_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_6 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'LEQ 'XI 1)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'XI 2)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST7
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA 'XI)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'LEQ 'XI 1)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST8
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q)
                                     (LIST 'EQUAL 'Q (LIST 'PLUS 'P 1))
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'XI 2)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST8A
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA 'XI)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL 'TEST_2 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1
                                                 ''TST2)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_6_RULES))) 
(SETK 'CASE_7_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'CASE_7 (LIST '~ 'M) (LIST '~ 'N) (LIST '~ 'P)
                         (LIST '~ 'Q) (LIST '~ 'DELTA) (LIST '~ 'XI)
                         (LIST '~ 'ETA) (LIST '~ 'TEST_1) (LIST '~ 'TEST_1A)
                         (LIST '~ 'TEST_2))
                   (LIST 'WHEN 'T
                         (LIST 'OR
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q) (LIST 'EQUAL 'Q 'P)
                                     (LIST 'NEQ 'ETA 0)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST9
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'EQUAL 'TEST_1A ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T))
                               (LIST 'AND (LIST 'GEQ 'P 1) (LIST 'LEQ 0 'N)
                                     (LIST 'LEQ 'N 'P) (LIST 'LEQ 1 'M)
                                     (LIST 'LEQ 'M 'Q) (LIST 'EQUAL 'Q 'P)
                                     (LIST 'NEQ 'ETA 0) (LIST 'GEQ 'DELTA 1)
                                     (LIST 'EQUAL
                                           (LIST 'ARG_TEST9A
                                                 (LIST 'ATAN2
                                                       (LIST 'IMPART 'ETA)
                                                       (LIST 'REPART 'ETA))
                                                 'DELTA)
                                           ''T)
                                     (LIST 'NOT
                                           (LIST 'EQUAL
                                                 (LIST 'ARG_TEST1
                                                       (LIST 'ABS
                                                             (LIST 'ATAN2
                                                                   (LIST
                                                                    'IMPART
                                                                    'ETA)
                                                                   (LIST
                                                                    'REPART
                                                                    'ETA)))
                                                       'DELTA)
                                                 ''T))
                                     (LIST 'EQUAL 'TEST_1 ''T)
                                     (LIST 'EQUAL
                                           (LIST 'TRANSFORM_TEST2 ''TST1 NIL)
                                           ''T)))))))) 
(AEVAL (LET '(CASE_7_RULES))) 
(AEVAL 'NIL) 
(ENDMODULE) 