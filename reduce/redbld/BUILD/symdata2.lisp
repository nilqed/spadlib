(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SYMDATA2)) 
(SET*ELEMS*GROUP 'C6 '(ID RC6 R2C6 R3C6 R4C6 R5C6)) 
(SET*GENERATORS 'C6 '(RC6)) 
(SET*RELATIONS 'C6 '(((RC6 RC6 RC6 RC6 RC6 RC6) (ID)))) 
(SET*GROUPTABLE 'C6
                '((GROUPTABLE ID RC6 R2C6 R3C6 R4C6 R5C6)
                  (ID ID RC6 R2C6 R3C6 R4C6 R5C6)
                  (RC6 RC6 R2C6 R3C6 R4C6 R5C6 ID)
                  (R2C6 R2C6 R3C6 R4C6 R5C6 ID RC6)
                  (R3C6 R3C6 R4C6 R5C6 ID RC6 R2C6)
                  (R4C6 R4C6 R5C6 ID RC6 R2C6 R3C6)
                  (R5C6 R5C6 ID RC6 R2C6 R3C6 R4C6))) 
(SET*INVERSE 'C6 '((ID RC6 R2C6 R3C6 R4C6 R5C6) (ID R5C6 R4C6 R3C6 R2C6 RC6))) 
(SET*ELEMASGEN 'C6
               '(((RC6) (RC6)) ((R2C6) (RC6 RC6)) ((R3C6) (RC6 RC6 RC6))
                 ((R4C6) (RC6 RC6 RC6 RC6)) ((R5C6) (RC6 RC6 RC6 RC6 RC6)))) 
(SET*GROUP 'C6 '((ID) (RC6) (R2C6) (R3C6) (R4C6) (R5C6))) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1)))) (RC6 (((1 . 1)))) (R2C6 (((1 . 1))))
                      (R3C6 (((1 . 1)))) (R4C6 (((1 . 1)))) (R5C6 (((1 . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1)))) (RC6 (((-1 . 1)))) (R2C6 (((1 . 1))))
                      (R3C6 (((-1 . 1)))) (R4C6 (((1 . 1))))
                      (R5C6 (((-1 . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1))))
                      (RC6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . 1)
                          . 2))))
                      (R2C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (R3C6 (((-1 . 1))))
                      (R4C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (R5C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . 1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1))))
                      (RC6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (R2C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (R3C6 (((1 . 1))))
                      (R4C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (R5C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1))))
                      (RC6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (R2C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (R3C6 (((1 . 1))))
                      (R4C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (R5C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '((ID (((1 . 1))))
                      (RC6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . 1)
                          . 2))))
                      (R2C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (R3C6 (((-1 . 1))))
                      (R4C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (R5C6
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . 1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'C6
                    '(REALTYPE (ID (((1 . 1)))) (RC6 (((1 . 1))))
                      (R2C6 (((1 . 1)))) (R3C6 (((1 . 1)))) (R4C6 (((1 . 1))))
                      (R5C6 (((1 . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'C6
                    '(REALTYPE (ID (((1 . 1)))) (RC6 (((-1 . 1))))
                      (R2C6 (((1 . 1)))) (R3C6 (((-1 . 1)))) (R4C6 (((1 . 1))))
                      (R5C6 (((-1 . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'C6
                    '(COMPLEXTYPE
                      (ID (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (RC6
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2) (1 . 2))))
                      (R2C6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (R3C6 (((-1 . 1) (NIL . 1)) ((NIL . 1) (-1 . 1))))
                      (R4C6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (R5C6
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (1 . 2)))))
                    'REAL) 
(SET*REPRESENTATION 'C6
                    '(COMPLEXTYPE
                      (ID (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (RC6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (R2C6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (R3C6 (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (R4C6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (R5C6
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2)))))
                    'REAL) 
(SET*AVAILABLE 'C6) 
(SET*ELEMS*GROUP 'S4
                 '(ID BACD ACBD ABDC DBCA CABD BCAD DACB BDCA DBAC CBDA ADBC
                   ACDB BADC CDAB DCBA CBAD ADCB BCDA BDAC CADB DABC CDBA DCAB)) 
(SET*GENERATORS 'S4 '(BACD ACBD ABDC DBCA)) 
(SET*RELATIONS 'S4
               '(((BACD BACD) (ID)) ((ACBD ACBD) (ID)) ((ABDC ABDC) (ID))
                 ((DBCA) (BACD ACBD ABDC ACBD BACD)))) 
(SET*GROUPTABLE 'S4
                '((GROUPTABLE DCAB DCBA DBAC DBCA DABC DACB CDAB CDBA CBAD CBDA
                   CABD CADB BDAC BDCA BCAD BCDA BACD BADC ADBC ADCB ACBD ACDB
                   ID ABDC)
                  (DCAB BADC ABDC CADB ACDB CBDA BCDA BACD ID DACB ADCB DBCA
                   BDCA CABD ACBD DABC ADBC DCBA CDBA CBAD BCAD DBAC BDAC DCAB
                   CDAB)
                  (DCBA BACD ID CABD ACBD CBAD BCAD BADC ABDC DABC ADBC DBAC
                   BDAC CADB ACDB DACB ADCB DCAB CDAB CBDA BCDA DBCA BDCA DCBA
                   CDBA)
                  (DBAC BCDA ACDB CBDA ABDC CADB BADC BDCA ADCB DBCA ID DACB
                   BACD CDBA ADBC DCBA ACBD DABC CABD CDAB BDAC DCAB BCAD DBAC
                   CBAD)
                  (DBCA BCAD ACBD CBAD ID CABD BACD BDAC ADBC DBAC ABDC DABC
                   BADC CDAB ADCB DCAB ACDB DACB CADB CDBA BDCA DCBA BCDA DBCA
                   CBDA)
                  (DABC BDCA ADCB CDBA ADBC CDAB BDAC BCDA ACDB DCBA ACBD DCAB
                   BCAD CBDA ABDC DBCA ID DBAC CBAD CADB BADC DACB BACD DABC
                   CABD)
                  (DACB BDAC ADBC CDAB ADCB CDBA BDCA BCAD ACBD DCAB ACDB DCBA
                   BCDA CBAD ID DBAC ABDC DBCA CBDA CABD BACD DABC BADC DACB
                   CADB)
                  (CDAB ABDC BADC ACDB CADB BCDA CBDA ID BACD ADCB DACB BDCA
                   DBCA ACBD CABD ADBC DABC CDBA DCBA BCAD CBAD BDAC DBAC CDAB
                   DCAB)
                  (CDBA ID BACD ACBD CABD BCAD CBAD ABDC BADC ADBC DABC BDAC
                   DBAC ACDB CADB ADCB DACB CDAB DCAB BCDA CBDA BDCA DBCA CDBA
                   DCBA)
                  (CBAD ACDB BCDA ABDC CBDA BADC CADB ADCB BDCA ID DBCA BACD
                   DACB ADBC CDBA ACBD DCBA CABD DABC BDAC CDAB BCAD DCAB CBAD
                   DBAC)
                  (CBDA ACBD BCAD ID CBAD BACD CABD ADBC BDAC ABDC DBAC BADC
                   DABC ADCB CDAB ACDB DCAB CADB DACB BDCA CDBA BCDA DCBA CBDA
                   DBCA)
                  (CABD ADCB BDCA ADBC CDBA BDAC CDAB ACDB BCDA ACBD DCBA BCAD
                   DCAB ABDC CBDA ID DBCA CBAD DBAC BADC CADB BACD DACB CABD
                   DABC)
                  (CADB ADBC BDAC ADCB CDAB BDCA CDBA ACBD BCAD ACDB DCAB BCDA
                   DCBA ID CBAD ABDC DBAC CBDA DBCA BACD CABD BADC DABC CADB
                   DACB)
                  (BDAC CBDA CADB BCDA BADC ACDB ABDC DBCA DACB BDCA BACD ADCB
                   ID DCBA DABC CDBA CABD ADBC ACBD DCAB DBAC CDAB CBAD BDAC
                   BCAD)
                  (BDCA CBAD CABD BCAD BACD ACBD ID DBAC DABC BDAC BADC ADBC
                   ABDC DCAB DACB CDAB CADB ADCB ACDB DCBA DBCA CDBA CBDA BDCA
                   BCDA)
                  (BCAD CADB CBDA BADC BCDA ABDC ACDB DACB DBCA BACD BDCA ID
                   ADCB DABC DCBA CABD CDBA ACBD ADBC DBAC DCAB CBAD CDAB BCAD
                   BDAC)
                  (BCDA CABD CBAD BACD BCAD ID ACBD DABC DBAC BADC BDAC ABDC
                   ADBC DACB DCAB CADB CDAB ACDB ADCB DBCA DCBA CBDA CDBA BCDA
                   BDCA)
                  (BACD CDAB CDBA BDAC BDCA ADBC ADCB DCAB DCBA BCAD BCDA ACBD
                   ACDB DBAC DBCA CBAD CBDA ID ABDC DABC DACB CABD CADB BACD
                   BADC)
                  (BADC CDBA CDAB BDCA BDAC ADCB ADBC DCBA DCAB BCDA BCAD ACDB
                   ACBD DBCA DBAC CBDA CBAD ABDC ID DACB DABC CADB CABD BADC
                   BACD)
                  (ADBC DBCA DACB DCBA DABC DCAB DBAC CBDA CADB CDBA CABD CDAB
                   CBAD BCDA BADC BDCA BACD BDAC BCAD ACDB ABDC ADCB ID ADBC
                   ACBD)
                  (ADCB DBAC DABC DCAB DACB DCBA DBCA CBAD CABD CDAB CADB CDBA
                   CBDA BCAD BACD BDAC BADC BDCA BCDA ACBD ID ADBC ABDC ADCB
                   ACDB)
                  (ACBD DACB DBCA DABC DCBA DBAC DCAB CADB CBDA CABD CDBA CBAD
                   CDAB BADC BCDA BACD BDCA BCAD BDAC ABDC ACDB ID ADCB ACBD
                   ADBC)
                  (ACDB DABC DBAC DACB DCAB DBCA DCBA CABD CBAD CADB CDAB CBDA
                   CDBA BACD BCAD BADC BDAC BCDA BDCA ID ACBD ABDC ADBC ACDB
                   ADCB)
                  (ID DCAB DCBA DBAC DBCA DABC DACB CDAB CDBA CBAD CBDA CABD
                   CADB BDAC BDCA BCAD BCDA BACD BADC ADBC ADCB ACBD ACDB ID
                   ABDC)
                  (ABDC DCBA DCAB DBCA DBAC DACB DABC CDBA CDAB CBDA CBAD CADB
                   CABD BDCA BDAC BCDA BCAD BADC BACD ADCB ADBC ACDB ACBD ABDC
                   ID))) 
(SET*INVERSE 'S4
             '((DCAB DCBA DBAC DBCA DABC DACB CDAB CDBA CBAD CBDA CABD CADB
                BDAC BDCA BCAD BCDA BACD BADC ADBC ADCB ACBD ACDB ID ABDC)
               (CDBA DCBA CBDA DBCA BCDA BDCA CDAB DCAB CBAD DBAC BCAD BDAC
                CADB DACB CABD DABC BACD BADC ACDB ADCB ACBD ADBC ID ABDC))) 
(SET*ELEMASGEN 'S4
               '(((BACD) (BACD)) ((ACBD) (ACBD)) ((ABDC) (ABDC))
                 ((DBCA) (DBCA)) ((CABD) (BACD ACBD)) ((BCAD) (ACBD BACD))
                 ((DACB) (DBCA BACD)) ((BDCA) (BACD DBCA)) ((DBAC) (ABDC DBCA))
                 ((CBDA) (DBCA ABDC)) ((ADBC) (ACBD ABDC)) ((ACDB) (ABDC ACBD))
                 ((BADC) (BACD ABDC)) ((CDAB) (ABDC BACD ACBD DBCA))
                 ((DCBA) (ACBD DBCA)) ((CBAD) (BACD ACBD BACD))
                 ((ADCB) (DBCA BACD DBCA)) ((BCDA) (ABDC ACBD BACD))
                 ((BDAC) (ACBD BACD ABDC)) ((CADB) (ABDC BACD ACBD))
                 ((DABC) (BACD ACBD ABDC)) ((CDBA) (BACD ACBD DBCA))
                 ((DCAB) (ABDC ACBD DBCA)))) 
(SET*GROUP 'S4
           '((DCAB DABC CADB BDAC BCDA CDBA) (DCBA BADC CDAB)
             (DBAC DACB CABD ADBC ACDB BCAD BDCA CBDA)
             (DBCA ADCB ABDC ACBD BACD CBAD) (ID))) 
(SET*REPRESENTATION 'S4
                    '((ID (((1 . 1)))) (BACD (((1 . 1)))) (ACBD (((1 . 1))))
                      (ABDC (((1 . 1)))) (DBCA (((1 . 1)))) (CABD (((1 . 1))))
                      (BCAD (((1 . 1)))) (DACB (((1 . 1)))) (BDCA (((1 . 1))))
                      (DBAC (((1 . 1)))) (CBDA (((1 . 1)))) (ADBC (((1 . 1))))
                      (ACDB (((1 . 1)))) (BADC (((1 . 1)))) (CDAB (((1 . 1))))
                      (DCBA (((1 . 1)))) (CBAD (((1 . 1)))) (ADCB (((1 . 1))))
                      (BCDA (((1 . 1)))) (BDAC (((1 . 1)))) (CADB (((1 . 1))))
                      (DABC (((1 . 1)))) (CDBA (((1 . 1)))) (DCAB (((1 . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'S4
                    '((ID (((1 . 1)))) (BACD (((-1 . 1)))) (ACBD (((-1 . 1))))
                      (ABDC (((-1 . 1)))) (DBCA (((-1 . 1))))
                      (CABD (((1 . 1)))) (BCAD (((1 . 1)))) (DACB (((1 . 1))))
                      (BDCA (((1 . 1)))) (DBAC (((1 . 1)))) (CBDA (((1 . 1))))
                      (ADBC (((1 . 1)))) (ACDB (((1 . 1)))) (BADC (((1 . 1))))
                      (CDAB (((1 . 1)))) (DCBA (((1 . 1)))) (CBAD (((-1 . 1))))
                      (ADCB (((-1 . 1)))) (BCDA (((-1 . 1))))
                      (BDAC (((-1 . 1)))) (CADB (((-1 . 1))))
                      (DABC (((-1 . 1)))) (CDBA (((-1 . 1))))
                      (DCAB (((-1 . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'S4
                    '((ID (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BACD
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (ACBD
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ABDC
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DBCA
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (CABD
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (BCAD
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (DACB
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (BDCA
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DBAC
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (CBDA
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ADBC
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ACDB
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (BADC (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CDAB (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (DCBA (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CBAD (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (ADCB (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BCDA (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BDAC
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (CADB
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (DABC (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CDBA
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DCAB
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'S4
                    '((ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (BACD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (ACBD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (ABDC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (DBCA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CABD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCAD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DACB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (BDCA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (DBAC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CBDA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ADBC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACDB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BADC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CDAB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DCBA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CBAD
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (ADCB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BCDA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BDAC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CADB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DABC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CDBA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (DCAB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'S4
                    '((ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (BACD
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACBD
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (ABDC
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DBCA
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CABD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCAD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DACB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (BDCA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (DBAC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CBDA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ADBC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACDB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BADC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CDAB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DCBA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CBAD
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (ADCB
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCDA
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BDAC
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CADB
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (DABC
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (CDBA
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DCAB
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'S4
                    '(REALTYPE (ID (((1 . 1)))) (BACD (((1 . 1))))
                      (ACBD (((1 . 1)))) (ABDC (((1 . 1)))) (DBCA (((1 . 1))))
                      (CABD (((1 . 1)))) (BCAD (((1 . 1)))) (DACB (((1 . 1))))
                      (BDCA (((1 . 1)))) (DBAC (((1 . 1)))) (CBDA (((1 . 1))))
                      (ADBC (((1 . 1)))) (ACDB (((1 . 1)))) (BADC (((1 . 1))))
                      (CDAB (((1 . 1)))) (DCBA (((1 . 1)))) (CBAD (((1 . 1))))
                      (ADCB (((1 . 1)))) (BCDA (((1 . 1)))) (BDAC (((1 . 1))))
                      (CADB (((1 . 1)))) (DABC (((1 . 1)))) (CDBA (((1 . 1))))
                      (DCAB (((1 . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'S4
                    '(REALTYPE (ID (((1 . 1)))) (BACD (((-1 . 1))))
                      (ACBD (((-1 . 1)))) (ABDC (((-1 . 1))))
                      (DBCA (((-1 . 1)))) (CABD (((1 . 1)))) (BCAD (((1 . 1))))
                      (DACB (((1 . 1)))) (BDCA (((1 . 1)))) (DBAC (((1 . 1))))
                      (CBDA (((1 . 1)))) (ADBC (((1 . 1)))) (ACDB (((1 . 1))))
                      (BADC (((1 . 1)))) (CDAB (((1 . 1)))) (DCBA (((1 . 1))))
                      (CBAD (((-1 . 1)))) (ADCB (((-1 . 1))))
                      (BCDA (((-1 . 1)))) (BDAC (((-1 . 1))))
                      (CADB (((-1 . 1)))) (DABC (((-1 . 1))))
                      (CDBA (((-1 . 1)))) (DCAB (((-1 . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'S4
                    '(REALTYPE (ID (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BACD
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (ACBD
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ABDC
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DBCA
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (CABD
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (BCAD
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (DACB
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (BDCA
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DBAC
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (CBDA
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ADBC
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (ACDB
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (BADC (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CDAB (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (DCBA (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CBAD (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (ADCB (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BCDA (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (BDAC
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (CADB
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (DABC (((-1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (CDBA
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (DCAB
                       (((1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2)))))
                    'REAL) 
(SET*REPRESENTATION 'S4
                    '(REALTYPE
                      (ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (BACD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (ACBD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (ABDC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (DBCA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CABD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCAD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DACB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (BDCA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (DBAC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CBDA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ADBC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACDB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BADC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CDAB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DCBA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CBAD
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (ADCB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BCDA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BDAC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CADB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DABC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CDBA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (DCAB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'S4
                    '(REALTYPE
                      (ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (BACD
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACBD
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (ABDC
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DBCA
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CABD
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCAD
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DACB
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (BDCA
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (DBAC
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (CBDA
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ADBC
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (ACDB
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (BADC
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (CDAB
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (DCBA
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CBAD
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (ADCB
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BCDA
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (BDAC
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (CADB
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (DABC
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (CDBA
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (DCAB
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1)))))
                    'REAL) 
(SET*AVAILABLE 'S4) 
(SET*ELEMS*GROUP 'A4
                 '(ID TA4 T2A4 XA4 YA4 ZA4 TXA4 TYA4 TZA4 T2XA4 T2YA4 T2ZA4)) 
(SET*GENERATORS 'A4 '(TA4 XA4 YA4 ZA4)) 
(SET*RELATIONS 'A4
               '(((ZA4) (TA4 XA4 TA4 TA4)) ((YA4) (TA4 ZA4 TA4 TA4))
                 ((XA4) (TA4 YA4 TA4 TA4)) ((TA4 TA4 TA4) (ID))
                 ((XA4 XA4) (ID)) ((YA4 YA4) (ID)) ((ZA4 ZA4) (ID))
                 ((XA4 YA4) (ZA4)))) 
(SET*GROUPTABLE 'A4
                '((GROUPTABLE ID TA4 T2A4 XA4 YA4 ZA4 TXA4 TYA4 TZA4 T2XA4
                   T2YA4 T2ZA4)
                  (ID ID TA4 T2A4 XA4 YA4 ZA4 TXA4 TYA4 TZA4 T2XA4 T2YA4 T2ZA4)
                  (TA4 TA4 T2A4 ID TXA4 TYA4 TZA4 T2XA4 T2YA4 T2ZA4 XA4 YA4
                   ZA4)
                  (T2A4 T2A4 ID TA4 T2XA4 T2YA4 T2ZA4 XA4 YA4 ZA4 TXA4 TYA4
                   TZA4)
                  (XA4 XA4 TYA4 T2ZA4 ID ZA4 YA4 TZA4 TA4 TXA4 T2YA4 T2XA4
                   T2A4)
                  (YA4 YA4 TZA4 T2XA4 ZA4 ID XA4 TYA4 TXA4 TA4 T2A4 T2ZA4
                   T2YA4)
                  (ZA4 ZA4 TXA4 T2YA4 YA4 XA4 ID TA4 TZA4 TYA4 T2ZA4 T2A4
                   T2XA4)
                  (TXA4 TXA4 T2YA4 ZA4 TA4 TZA4 TYA4 T2ZA4 T2A4 T2XA4 YA4 XA4
                   ID)
                  (TYA4 TYA4 T2ZA4 XA4 TZA4 TA4 TXA4 T2YA4 T2XA4 T2A4 ID ZA4
                   YA4)
                  (TZA4 TZA4 T2XA4 YA4 TYA4 TXA4 TA4 T2A4 T2ZA4 T2YA4 ZA4 ID
                   XA4)
                  (T2XA4 T2XA4 YA4 TZA4 T2A4 T2ZA4 T2YA4 ZA4 ID XA4 TYA4 TXA4
                   TA4)
                  (T2YA4 T2YA4 ZA4 TXA4 T2ZA4 T2A4 T2XA4 YA4 XA4 ID TA4 TZA4
                   TYA4)
                  (T2ZA4 T2ZA4 XA4 TYA4 T2YA4 T2XA4 T2A4 ID ZA4 YA4 TZA4 TA4
                   TXA4))) 
(SET*INVERSE 'A4
             '((ID TA4 T2A4 XA4 YA4 ZA4 TXA4 TYA4 TZA4 T2XA4 T2YA4 T2ZA4)
               (ID T2A4 TA4 XA4 YA4 ZA4 T2ZA4 T2XA4 T2YA4 TYA4 TZA4 TXA4))) 
(SET*ELEMASGEN 'A4
               '(((TA4) (TA4)) ((T2A4) (TA4 TA4)) ((XA4) (XA4)) ((YA4) (YA4))
                 ((ZA4) (ZA4)) ((TXA4) (TA4 XA4)) ((TYA4) (TA4 YA4))
                 ((TZA4) (TA4 ZA4)) ((T2XA4) (TA4 TA4 XA4))
                 ((T2YA4) (TA4 TA4 YA4)) ((T2ZA4) (TA4 TA4 ZA4)))) 
(SET*GROUP 'A4
           '((ID) (TXA4 TA4 TZA4 TYA4) (T2ZA4 T2A4 T2YA4 T2XA4) (YA4 XA4 ZA4))) 
(SET*REPRESENTATION 'A4
                    '((ID (((1 . 1)))) (TA4 (((1 . 1)))) (T2A4 (((1 . 1))))
                      (XA4 (((1 . 1)))) (YA4 (((1 . 1)))) (ZA4 (((1 . 1))))
                      (TXA4 (((1 . 1)))) (TYA4 (((1 . 1)))) (TZA4 (((1 . 1))))
                      (T2XA4 (((1 . 1)))) (T2YA4 (((1 . 1))))
                      (T2ZA4 (((1 . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'A4
                    '((ID (((1 . 1))))
                      (TA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (T2A4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (XA4 (((1 . 1)))) (YA4 (((1 . 1)))) (ZA4 (((1 . 1))))
                      (TXA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (TYA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (TZA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (T2XA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (T2YA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (T2ZA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'A4
                    '((ID (((1 . 1))))
                      (TA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (T2A4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (XA4 (((1 . 1)))) (YA4 (((1 . 1)))) (ZA4 (((1 . 1))))
                      (TXA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (TYA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (TZA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . -1)) . -1)
                          . 2))))
                      (T2XA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (T2YA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2))))
                      (T2ZA4
                       (((((((EXPT 3 (QUOTIENT 1 2)) . 1) ((I . 1) . 1)) . -1)
                          . 2)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'A4
                    '((ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (TA4
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (T2A4
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (XA4
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (YA4
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (ZA4
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (TXA4
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (TYA4
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (TZA4
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (T2XA4
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (T2YA4
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (T2ZA4
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1)))))
                    'COMPLEX) 
(SET*REPRESENTATION 'A4
                    '(REALTYPE (ID (((1 . 1)))) (TA4 (((1 . 1))))
                      (T2A4 (((1 . 1)))) (XA4 (((1 . 1)))) (YA4 (((1 . 1))))
                      (ZA4 (((1 . 1)))) (TXA4 (((1 . 1)))) (TYA4 (((1 . 1))))
                      (TZA4 (((1 . 1)))) (T2XA4 (((1 . 1))))
                      (T2YA4 (((1 . 1)))) (T2ZA4 (((1 . 1)))))
                    'REAL) 
(SET*REPRESENTATION 'A4
                    '(COMPLEXTYPE
                      (ID (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (TA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (T2A4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (XA4 (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (YA4 (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (ZA4 (((1 . 1) (NIL . 1)) ((NIL . 1) (1 . 1))))
                      (TXA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (TYA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (TZA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2)
                         (-1 . 2))))
                      (T2XA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (T2YA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2))))
                      (T2ZA4
                       (((-1 . 2) (((((EXPT 3 (QUOTIENT 1 2)) . 1) . -1)) . 2))
                        ((((((EXPT 3 (QUOTIENT 1 2)) . 1) . 1)) . 2)
                         (-1 . 2)))))
                    'REAL) 
(SET*REPRESENTATION 'A4
                    '(REALTYPE
                      (ID
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (TA4
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (T2A4
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (XA4
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (YA4
                       (((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))))
                      (ZA4
                       (((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))))
                      (TXA4
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (1 . 1) (NIL . 1))))
                      (TYA4
                       (((NIL . 1) (NIL . 1) (1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (TZA4
                       (((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))
                        ((NIL . 1) (-1 . 1) (NIL . 1))))
                      (T2XA4
                       (((NIL . 1) (-1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1))))
                      (T2YA4
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (-1 . 1))
                        ((-1 . 1) (NIL . 1) (NIL . 1))))
                      (T2ZA4
                       (((NIL . 1) (1 . 1) (NIL . 1))
                        ((NIL . 1) (NIL . 1) (1 . 1))
                        ((1 . 1) (NIL . 1) (NIL . 1)))))
                    'REAL) 
(SET*AVAILABLE 'A4) 
(ENDMODULE) 