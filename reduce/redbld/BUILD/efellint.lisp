(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFELLINT)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'PI_SHIFT 'NUMBER-OF-ARGS 1) 
(FLAG '(PI_SHIFT) 'OPFN) 
(PUT 'PI_SHIFT 'DEFINED-ON-LINE '33) 
(PUT 'PI_SHIFT 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'PI_SHIFT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PI_SHIFT (X)
    (PROG (N SGN)
      (SETQ SGN (AEVAL 1))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (PROGN (SETQ X (AEVAL (LIST 'MINUS X))) (SETQ SGN (AEVAL (MINUS 1))))))
      (SETQ N (AEVAL (LIST 'FIX (LIST 'QUOTIENT (LIST 'REPART X) 'PI))))
      (SETQ X (AEVAL (LIST 'DIFFERENCE X (LIST 'TIMES N 'PI))))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'REPART X)) (AEVAL (LIST 'QUOTIENT 'PI 2)))
        (PROGN
         (SETQ N (AEVAL (LIST 'PLUS N 1)))
         (SETQ X (AEVAL (LIST 'DIFFERENCE X 'PI)))
         (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'LIST (LIST 'TIMES SGN X) (LIST 'TIMES SGN N)))))) 
(OPERATOR (LIST 'ELLIPTICF)) 
(SETK 'ELLIPTICFRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'ELLIPTICF (LIST '~ 'PHI) 0) 'PHI)
             (LIST 'REPLACEBY (LIST 'ELLIPTICF (LIST '~ 'PHI) 1)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'LOG (LIST 'PLUS 1 (LIST 'SIN 'PHI)))
                               (LIST 'LOG
                                     (LIST 'DIFFERENCE 1 (LIST 'SIN 'PHI))))
                         2))
             (LIST 'REPLACEBY (LIST 'ELLIPTICF 0 (LIST '~ 'K)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF (LIST 'MINUS (LIST '~ 'PHI)) (LIST '~ 'K))
                   (LIST 'MINUS (LIST 'ELLIPTICF 'PHI 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF (LIST '~ 'PHI) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICF 'PHI 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF (LIST 'TIMES 'I (LIST '~ 'PHI)) 0)
                   (LIST 'TIMES 'I 'PHI))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF (LIST 'TIMES 'I (LIST '~ 'PHI)) 1)
                   (LIST 'TIMES 'I (LIST 'ATAN (LIST 'SINH 'PHI))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF (LIST 'QUOTIENT 'PI 2) (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICF
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       ''PI))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''ELLIPTICF 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES 2 'SHIFT
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICF (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'U 'X)
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'TIMES (LIST 'EXPT 'K 2)
                                                 (LIST 'EXPT (LIST 'SIN 'U)
                                                       2)))))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'QUOTIENT
                                                       (LIST 'ELLIPTICE 'U 'K)
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))
                                                 (LIST 'ELLIPTICF 'U 'K))
                                           'K)
                                     (LIST 'TIMES 'K (LIST 'SIN 'U)
                                           (LIST 'QUOTIENT (LIST 'COS 'U)
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K 2))
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'EXPT
                                                                          'K 2)
                                                                         (LIST
                                                                          'EXPT
                                                                          (LIST
                                                                           'SIN
                                                                           'U)
                                                                          2)))))))))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICF (LIST '~ 'PHI) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLF 'PHI 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'PHI)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(ELLIPTICFRULES)) 
(PUT 'NUM_ELLF 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ELLF) 'OPFN) 
(PUT 'NUM_ELLF 'DEFINED-ON-LINE '92) 
(PUT 'NUM_ELLF 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ELLF (PHI K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL PHI))
          ((OR (EVALEQUAL (AEVAL PHI) (AEVAL (LIST 'QUOTIENT 'PI 2)))
               (EVALEQUAL (AEVAL PHI)
                          (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 'PI 2)))))
           (AEVAL (LIST 'TIMES (LIST 'SIGN PHI) (LIST 'NUM_ELLK K))))
          ((OR (EVALEQUAL (AEVAL K) 1)
               (AND (EVALEQUAL (AEVAL K) (MINUS 1))
                    (EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
                    (EVALLESSP (AEVAL (LIST 'ABS PHI))
                               (AEVAL (LIST 'QUOTIENT 'PI 2)))))
           (AEVAL
            (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY 'S (LIST 'SIN PHI)))
                  (LIST 'QUOTIENT
                        (LIST 'LOG
                              (LIST 'QUOTIENT (LIST 'PLUS 1 'S)
                                    (LIST 'DIFFERENCE 1 'S)))
                        2))))
          (T
           (PROG (L)
             (SETQ L (AEVAL (LIST 'PI_SHIFT PHI)))
             (COND
              ((EVALNEQ (AEVAL (LIST 'SECOND L)) 0)
               (RETURN
                (AEVAL
                 (LIST 'PLUS (LIST 'TIMES (LIST 'SECOND L) (LIST 'NUM_ELLK K))
                       (LIST 'NUM_ELLF (LIST 'FIRST L) K)))))
              (T (RETURN (AEVAL (LIST 'NUM_ELLIPTIC 'ELLIPTIC_F PHI K))))))))) 
(PUT 'ELLIPTIC_F 'NUMBER-OF-ARGS 2) 
(FLAG '(ELLIPTIC_F) 'OPFN) 
(PUT 'ELLIPTIC_F 'DEFINED-ON-LINE '105) 
(PUT 'ELLIPTIC_F 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'ELLIPTIC_F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC_F (PHI K)
    (PROG (SGN)
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART K)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART K)) 0)))
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART PHI)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART PHI)) 0)))
        (PROGN
         (SETQ SGN (AEVAL (MINUS 1)))
         (SETQ PHI (AEVAL (LIST 'MINUS PHI)))
         (AEVAL 'NIL)))
       (T (SETQ SGN (AEVAL 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SIN PHI)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_1ST 0 (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                               (LIST 'LIST 1 (MINUS 1))
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                               (LIST 'LIST 1 0))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_1ST 0
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 0 1) (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_1ST
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST (MINUS 1)
                                                 (LIST 'EXPT K 2))
                                           (LIST 'LIST 1 0))))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) (AEVAL (LIST 'QUOTIENT 'PI 2)))
        (PROGN
         (SETK 'S (AEVAL (LIST 'COSH (LIST 'IMPART PHI))))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_1ST 0 1 (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_1ST 1 (LIST 'EXPT 'S 2)
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST (MINUS 1) 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))))
                         2)))))
          ((EVALLESSP (AEVAL K) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_1ST 0 1 (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_1ST 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 0 1)
                                                 (LIST 'LIST (MINUS 1) 1)
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT K 2)))
                                                 (LIST 'LIST 1 0))))
                               (LIST 'ELLINT_1ST
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                     (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0)))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_1ST 0
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_1ST
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 1 (LIST 'LIST 0 1)
                                                 (LIST 'LIST 1 (MINUS 1))
                                                 (LIST 'LIST (MINUS 1)
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 1 0))))
                               (LIST 'ELLINT_1ST 1 (LIST 'EXPT 'S 2)
                                     (LIST 'LIST 0 1) (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0)))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SINH (LIST 'IMPART PHI))))
         (COND
          ((EVALNEQ (AEVAL (LIST 'REPART K)) 0)
           (RETURN
            (AEVAL
             (LIST 'TIMES 'I SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_1ST 0 (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                               (LIST 'LIST 1 1) (LIST 'LIST 1 (LIST 'EXPT K 2))
                               (LIST 'LIST 1 0))
                         2)))))
          (T
           (PROGN
            (SETQ K (AEVAL (LIST 'IMPART K)))
            (COND
             ((EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1)
              (RETURN
               (AEVAL
                (LIST 'TIMES 'I SGN
                      (LIST 'QUOTIENT
                            (LIST 'ELLINT_1ST 0 (LIST 'EXPT 'S 2)
                                  (LIST 'LIST 0 1) (LIST 'LIST 1 1)
                                  (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                                  (LIST 'LIST 1 0))
                            2)))))
             (T
              (RETURN
               (AEVAL
                (LIST 'TIMES SGN
                      (LIST 'QUOTIENT
                            (LIST 'PLUS
                                  (LIST 'TIMES 'I
                                        (LIST 'ELLINT_1ST 0
                                              (LIST 'QUOTIENT 1
                                                    (LIST 'EXPT K 2))
                                              (LIST 'LIST 0 1) (LIST 'LIST 1 1)
                                              (LIST 'LIST 1
                                                    (LIST 'MINUS
                                                          (LIST 'EXPT K 2)))
                                              (LIST 'LIST 1 0)))
                                  (LIST 'ELLINT_1ST
                                        (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                        (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                        (LIST 'LIST 1 1)
                                        (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                        (LIST 'LIST 1 0)))
                            2))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (SETK 'S (AEVAL (LIST 'SIN PHI)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'TIMES K 'S))) 0)
             (EVALGREATERP (AEVAL (LIST 'EXPT (LIST 'TIMES K 'S) 2)) 1))
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'ELLINT_1ST 0
                                  (LIST 'QUOTIENT 1
                                        (LIST 'EXPT (LIST 'TIMES K 'S) 2))
                                  (LIST 'LIST 0 1)
                                  (LIST 'LIST 1
                                        (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                  (LIST 'LIST 1
                                        (LIST 'MINUS
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2)))
                                  (LIST 'LIST 1 0))
                            (LIST 'TIMES 'I
                                  (LIST 'ELLINT_1ST
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        1 (LIST 'LIST 0 1)
                                        (LIST 'LIST 1
                                              (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                        (LIST 'LIST (MINUS 1)
                                              (LIST 'EXPT (LIST 'EXPT K 'S) 2))
                                        (LIST 'LIST 1 0))))
                      2)))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'ELLINT_1ST 0 1 (LIST 'LIST 0 1)
                            (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT 'S 2)))
                            (LIST 'LIST 1
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT K 2)
                                              (LIST 'EXPT 'S 2))))
                            (LIST 'LIST 1 0))
                      2)))))))) 
(OPERATOR (LIST 'NOME)) 
(SETK 'ELLIPTICKRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ELLIPTICK (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICK 0) (LIST 'QUOTIENT 'PI 2))
             (LIST 'REPLACEBY (LIST '|ELLIPTICK'| 1) (LIST 'QUOTIENT 'PI 2))
             (LIST 'REPLACEBY (LIST 'ELLIPTICK (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLK 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'M)
                               (LIST 'NEQ 'M 1)
                               (LIST 'NEQ 'M (LIST 'MINUS 1)))))
             (LIST 'REPLACEBY (LIST '|ELLIPTICK'| (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLKC 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'M)
                               (LIST 'NEQ 'M 0))))
             (LIST 'REPLACEBY (LIST 'NOME 0) 0)
             (LIST 'REPLACEBY (LIST 'NOME 1) 1)
             (LIST 'REPLACEBY (LIST 'NOME (LIST '~ 'M))
                   (LIST 'EXP
                         (LIST 'MINUS
                               (LIST 'TIMES 'PI
                                     (LIST 'QUOTIENT (LIST '|ELLIPTICK'| 'M)
                                           (LIST 'ELLIPTICK 'M))))))))) 
(LET '(ELLIPTICKRULES)) 
(PUT 'NUM_ELLK 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_ELLK) 'OPFN) 
(PUT 'NUM_ELLK 'DEFINED-ON-LINE '190) 
(PUT 'NUM_ELLK 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ELLK (K)
    (COND
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (REDERR (REVALX "Logarithmic Singularity"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'QUOTIENT 'PI 2)))
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0)
           (EVALGREATERP (AEVAL (LIST 'ABS K)) 1))
      (AEVAL
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'RF 0
                         (LIST 'DIFFERENCE 1
                               (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))
                         1)
                   (LIST 'TIMES 'I
                         (LIST 'RF 0 (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)) 1)))
             (LIST 'ABS K))))
     (T (AEVAL (LIST 'RF 0 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)) 1))))) 
(PUT 'NUM_ELLKC 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_ELLKC) 'OPFN) 
(PUT 'NUM_ELLKC 'DEFINED-ON-LINE '201) 
(PUT 'NUM_ELLKC 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLKC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ELLKC (K)
    (COND
     ((EVALEQUAL (AEVAL K) 0)
      (AEVAL (REDERR (REVALX "Logarithmic Singularity"))))
     ((OR (EVALGREATERP (AEVAL (LIST 'REPART K)) 0)
          (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
               (EVALGREATERP (AEVAL (LIST 'IMPART K)) 0)))
      (AEVAL
       (LIST 'NUM_ELLK (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))))
     (T
      (PROGN
       (SETQ K (AEVAL (LIST 'MINUS K)))
       (COND
        ((EVALGREATERP (AEVAL (LIST 'IMPART K)) 0)
         (AEVAL
          (LIST 'PLUS
                (LIST 'NUM_ELLK
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                (LIST 'TIMES 2 'I (LIST 'NUM_ELLK K)))))
        (T
         (AEVAL
          (LIST 'DIFFERENCE
                (LIST 'NUM_ELLK
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                (LIST 'TIMES 2 'I (LIST 'NUM_ELLK K)))))))))) 
(PUT 'NUM_ELLE 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ELLE) 'OPFN) 
(PUT 'NUM_ELLE 'DEFINED-ON-LINE '216) 
(PUT 'NUM_ELLE 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ELLE (PHI K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL PHI))
          ((OR (EVALEQUAL (AEVAL PHI) (AEVAL (LIST 'QUOTIENT 'PI 2)))
               (EVALEQUAL (AEVAL PHI)
                          (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 'PI 2)))))
           (AEVAL (LIST 'TIMES (LIST 'SIGN PHI) (LIST 'NUM_ELLEC K))))
          ((AND (OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
                (EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
                (EVALLESSP (AEVAL (LIST 'ABS PHI))
                           (AEVAL (LIST 'QUOTIENT 'PI 2))))
           (AEVAL (LIST 'SIN PHI)))
          (T
           (PROG (L)
             (SETQ L (AEVAL (LIST 'PI_SHIFT PHI)))
             (COND
              ((EVALNEQ (AEVAL (LIST 'SECOND L)) 0)
               (RETURN
                (AEVAL
                 (LIST 'PLUS (LIST 'TIMES (LIST 'SECOND L) (LIST 'NUM_ELLEC K))
                       (LIST 'NUM_ELLE (LIST 'FIRST L) K)))))
              (T (RETURN (AEVAL (LIST 'NUM_ELLIPTIC 'ELLIPTIC_E PHI K))))))))) 
(PUT 'ELLIPTIC_E 'NUMBER-OF-ARGS 2) 
(FLAG '(ELLIPTIC_E) 'OPFN) 
(PUT 'ELLIPTIC_E 'DEFINED-ON-LINE '228) 
(PUT 'ELLIPTIC_E 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'ELLIPTIC_E 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC_E (PHI K)
    (PROG (SGN)
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART K)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART K)) 0)))
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART PHI)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART PHI)) 0)))
        (PROGN
         (SETQ SGN (AEVAL (MINUS 1)))
         (SETQ PHI (AEVAL (LIST 'MINUS PHI)))
         (AEVAL 'NIL)))
       (T (SETQ SGN (AEVAL 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SIN PHI)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2)
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                               (LIST 'LIST 1 (MINUS 1)) (LIST 'LIST 0 1)
                               (LIST 'LIST 1 0))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'ELLINT_2ND 0
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 (MINUS 1)) (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_2ND
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'EXPT 'S 2)
                                           (LIST 'LIST (MINUS 1)
                                                 (LIST 'EXPT K 2))
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 0 1) (LIST 'LIST 1 0))))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) (AEVAL (LIST 'QUOTIENT 'PI 2)))
        (PROGN
         (SETK 'S (AEVAL (LIST 'COSH (LIST 'IMPART PHI))))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_2ND 0 1
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 (MINUS 1)) (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_2ND 1 (LIST 'EXPT 'S 2)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST (MINUS 1) 1)
                                           (LIST 'LIST 0 1) (LIST 'LIST 1 0))))
                         2)))))
          ((EVALLESSP (AEVAL K) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_2ND 0 1
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 0 1) (LIST 'LIST 1 0))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_2ND 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT K 2)))
                                                 (LIST 'LIST (MINUS 1) 1)
                                                 (LIST 'LIST 0 1)
                                                 (LIST 'LIST 1 0))))
                               (LIST 'ELLINT_2ND
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'EXPT 'S 2)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST (MINUS 1) 1) (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 0)))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'ELLINT_2ND 0
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 (MINUS 1)) (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_2ND
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           1
                                           (LIST 'LIST (MINUS 1)
                                                 (LIST 'EXPT K 2))
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 0 1) (LIST 'LIST 1 0)))
                               (LIST 'ELLINT_2ND 1 (LIST 'EXPT 'S 2)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST (MINUS 1) 1) (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 0)))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SINH (LIST 'IMPART PHI))))
         (COND
          ((EVALNEQ (AEVAL (LIST 'REPART K)) 0)
           (RETURN
            (AEVAL
             (LIST 'TIMES 'I SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2)
                               (LIST 'LIST 1 (LIST 'EXPT K 2)) (LIST 'LIST 1 1)
                               (LIST 'LIST 0 1) (LIST 'LIST 1 0))
                         2)))))
          (T
           (PROGN
            (SETQ K (AEVAL (LIST 'IMPART K)))
            (COND
             ((EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1)
              (RETURN
               (AEVAL
                (LIST 'TIMES 'I SGN
                      (LIST 'QUOTIENT
                            (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2)
                                  (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                                  (LIST 'LIST 1 1) (LIST 'LIST 0 1)
                                  (LIST 'LIST 1 0))
                            2)))))
             (T
              (RETURN
               (AEVAL
                (LIST 'TIMES SGN
                      (LIST 'QUOTIENT
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES 'I
                                        (LIST 'ELLINT_2ND 0
                                              (LIST 'QUOTIENT 1
                                                    (LIST 'EXPT K 2))
                                              (LIST 'LIST 1
                                                    (LIST 'MINUS
                                                          (LIST 'EXPT K 2)))
                                              (LIST 'LIST 1 1) (LIST 'LIST 0 1)
                                              (LIST 'LIST 1 0)))
                                  (LIST 'ELLINT_2ND
                                        (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                        (LIST 'EXPT 'S 2)
                                        (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                        (LIST 'LIST 1 1) (LIST 'LIST 0 1)
                                        (LIST 'LIST 1 0)))
                            2))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (SETK 'S (AEVAL (LIST 'SIN PHI)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'TIMES K 'S))) 0)
             (EVALGREATERP (AEVAL (LIST 'EXPT (LIST 'TIMES K 'S) 2)) 1))
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'PLUS
                            (LIST 'ELLINT_2ND 0
                                  (LIST 'QUOTIENT 1
                                        (LIST 'EXPT (LIST 'TIMES K 'S) 2))
                                  (LIST 'LIST 1
                                        (LIST 'MINUS
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2)))
                                  (LIST 'LIST 1
                                        (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                  (LIST 'LIST 0 1) (LIST 'LIST 1 0))
                            (LIST 'TIMES 'I
                                  (LIST 'ELLINT_2ND
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        1
                                        (LIST 'LIST (MINUS 1)
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        (LIST 'LIST 1
                                              (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                        (LIST 'LIST 0 1) (LIST 'LIST 1 0))))
                      2)))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'ELLINT_2ND 0 1
                            (LIST 'LIST 1
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT K 2)
                                              (LIST 'EXPT 'S 2))))
                            (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT 'S 2)))
                            (LIST 'LIST 0 1) (LIST 'LIST 1 0))
                      2)))))))) 
(PUT 'NUM_ELLEC 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_ELLEC) 'OPFN) 
(PUT 'NUM_ELLEC 'DEFINED-ON-LINE '285) 
(PUT 'NUM_ELLEC 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ELLEC (K)
    (COND ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))) 1)
          ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'QUOTIENT 'PI 2)))
          (T
           (PROG (KP2 RP IP)
             (COND
              ((AND (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0)
                    (EVALGREATERP (AEVAL (LIST 'ABS K)) 1))
               (PROGN
                (SETQ KP2
                        (AEVAL
                         (LIST 'DIFFERENCE 1
                               (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))))
                (SETQ RP
                        (AEVAL
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT (LIST 'RF 0 KP2 1)
                                     (LIST 'ABS K))
                               (LIST 'QUOTIENT (LIST 'RD 0 KP2 1)
                                     (LIST 'TIMES 3 (LIST 'ABS K))))))
                (SETQ KP2 (AEVAL (LIST 'DIFFERENCE 1 KP2)))
                (SETQ IP
                        (AEVAL
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES K
                                     (LIST 'PLUS (LIST 'RD 0 KP2 1)
                                           (LIST 'RD 0 1 KP2))
                                     (LIST 'QUOTIENT KP2 3))
                               (LIST 'QUOTIENT (LIST 'RF 0 KP2 1) K))))
                (RETURN (AEVAL (LIST 'PLUS RP (LIST 'TIMES 'I IP))))
                (AEVAL 'NIL)))
              (T
               (PROGN
                (SETQ KP2 (AEVAL (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                (RETURN
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'PLUS (LIST 'RD 0 KP2 1) (LIST 'RD 0 1 KP2))
                        (LIST 'QUOTIENT KP2 3))))
                (AEVAL 'NIL)))))))) 
(PUT 'NUM_ELLECP 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_ELLECP) 'OPFN) 
(PUT 'NUM_ELLECP 'DEFINED-ON-LINE '304) 
(PUT 'NUM_ELLECP 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLECP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ELLECP (K)
    (COND
     ((OR (EVALGREATERP (AEVAL (LIST 'REPART K)) 0)
          (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
               (EVALGREATERP (AEVAL (LIST 'IMPART K)) 0)))
      (AEVAL
       (LIST 'NUM_ELLEC (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))))
     (T
      (PROGN
       (SETQ K (AEVAL (LIST 'MINUS K)))
       (COND
        ((EVALGREATERP (AEVAL (LIST 'IMPART K)) 0)
         (AEVAL
          (LIST 'PLUS
                (LIST 'NUM_ELLEC
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                (LIST 'TIMES 2 'I
                      (LIST 'DIFFERENCE (LIST 'NUM_ELLK K)
                            (LIST 'NUM_ELLEC K))))))
        (T
         (AEVAL
          (LIST 'DIFFERENCE
                (LIST 'NUM_ELLEC
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                (LIST 'TIMES 2 'I
                      (LIST 'DIFFERENCE (LIST 'NUM_ELLK K)
                            (LIST 'NUM_ELLEC K))))))))))) 
(SETK 'ELLIPTICERULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ELLIPTICE (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICE 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICE 0) (LIST 'QUOTIENT 'PI 2))
             (LIST 'REPLACEBY (LIST 'ELLIPTICE 1) 1)
             (LIST 'REPLACEBY (LIST '|ELLIPTICE'| 0) 1)
             (LIST 'REPLACEBY (LIST '|ELLIPTICE'| 1) (LIST 'QUOTIENT 'PI 2))
             (LIST 'REPLACEBY (LIST 'ELLIPTICE 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY (LIST 'ELLIPTICE (LIST '~ 'PHI) 0) 'PHI)
             (LIST 'REPLACEBY (LIST 'ELLIPTICE (LIST '~ 'PHI) 1)
                   (LIST 'SIN 'PHI))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE (LIST 'TIMES 'I (LIST '~ 'PHI)) 0)
                   (LIST 'TIMES 'I 'PHI))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE (LIST 'TIMES 'I (LIST '~ 'PHI)) 1)
                   (LIST 'TIMES 'I (LIST 'SINH 'PHI)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE (LIST 'MINUS (LIST '~ 'PHI)) (LIST '~ 'K))
                   (LIST 'MINUS (LIST 'ELLIPTICE 'PHI 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE (LIST '~ 'PHI) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICE 'PHI 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE (LIST 'QUOTIENT 'PI 2) (LIST '~ 'K))
                   (LIST 'ELLIPTICE 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICE
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       ''PI))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''ELLIPTICE 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES 2 'SHIFT
                                                       (LIST 'LIST ''ELLIPTICE
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICE (LIST '~ 'PHI) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DF 'PHI 'X)
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'TIMES (LIST 'EXPT 'M 2)
                                                 (LIST 'EXPT (LIST 'SIN 'PHI)
                                                       2)))))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'ELLIPTICE 'PHI 'M)
                                           (LIST 'ELLIPTICF 'PHI 'M))
                                     'M))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICE (LIST '~ 'PHI) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLE 'PHI 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'PHI)
                               (LIST 'NUMBERP 'M))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICE (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_ELLEC 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'M))))
             (LIST 'REPLACEBY (LIST '|ELLIPTICE'| (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_ELLECP 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(ELLIPTICERULES)) 
(OPERATOR (LIST 'ELLIPTICD)) 
(SETK 'ELLIPTICDRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ELLIPTICD (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICD 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD 0) (LIST 'QUOTIENT 'PI 4))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD (LIST '~ 'PHI) 0)
                   (LIST 'DIFFERENCE (LIST 'QUOTIENT 'PHI 2)
                         (LIST 'QUOTIENT (LIST 'SIN (LIST 'TIMES 2 'PHI)) 4)))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD (LIST '~ 'PHI) 1)
                   (LIST 'PLUS (LIST 'MINUS (LIST 'SIN 'PHI))
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'LOG
                                           (LIST 'PLUS 1 (LIST 'SIN 'PHI)))
                                     (LIST 'LOG
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'SIN 'PHI))))
                               2)))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICD (LIST 'MINUS (LIST '~ 'PHI)) (LIST '~ 'K))
                   (LIST 'MINUS (LIST 'ELLIPTICD 'PHI 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICD (LIST '~ 'PHI) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICD 'PHI 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICD (LIST 'QUOTIENT 'PI 2) (LIST '~ 'K))
                   (LIST 'ELLIPTICD 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICD
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       ''PI))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''ELLIPTICD 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES 2 'SHIFT
                                                       (LIST 'LIST ''ELLIPTICD
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICD (LIST '~ 'K)) (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'TIMES 2 (LIST 'ELLIPTICD 'K)))
                               (LIST 'QUOTIENT (LIST 'ELLIPTICE 'K)
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
                         'K))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICD (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DF 'U 'X)
                               (LIST 'QUOTIENT (LIST 'EXPT (LIST 'SIN 'U) 2)
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'TIMES (LIST 'EXPT 'K 2)
                                                       (LIST 'EXPT
                                                             (LIST 'SIN 'U)
                                                             2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'MINUS
                                                 (LIST 'TIMES 2
                                                       (LIST 'ELLIPTICD 'U
                                                             'K)))
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'ELLIPTICE 'U 'K)
                                                       (LIST 'TIMES
                                                             (LIST 'SIN 'U)
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'COS
                                                                         'U)
                                                                   (LIST 'SQRT
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'TIMES
                                                                           (LIST
                                                                            'EXPT
                                                                            'K
                                                                            2)
                                                                           (LIST
                                                                            'EXPT
                                                                            (LIST
                                                                             'SIN
                                                                             'U)
                                                                            2)))))))
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))
                                     'K))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD (LIST '~ 'PHI) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'N_ELLD 'PHI 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'PHI)
                               (LIST 'NUMBERP 'M))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICD (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'N_ELLDC 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(ELLIPTICDRULES)) 
(PUT 'N_ELLDC 'NUMBER-OF-ARGS 1) 
(FLAG '(N_ELLDC) 'OPFN) 
(PUT 'N_ELLDC 'DEFINED-ON-LINE '412) 
(PUT 'N_ELLDC 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'N_ELLDC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE N_ELLDC (K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'QUOTIENT 'PI 4)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL
            (REDERR
             (REVALX "ellipticd(k) has a logarithmic singularity at k=1"))))
          (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM_ELLDC K))))) 
(PUT 'NUM_ELLDC 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_ELLDC) 'OPFN) 
(PUT 'NUM_ELLDC 'DEFINED-ON-LINE '418) 
(PUT 'NUM_ELLDC 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_ELLDC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ELLDC (K)
    (COND
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0)
           (EVALGREATERP (AEVAL (LIST 'ABS K)) 1))
      (AEVAL
       (LIST 'PLUS
             (LIST 'QUOTIENT
                   (LIST 'RD 0
                         (LIST 'DIFFERENCE 1
                               (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))
                         1)
                   (LIST 'TIMES 3 (LIST 'EXPT K 3)))
             (LIST 'TIMES (LIST 'QUOTIENT 'I K)
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'RF 0 (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     1))
                         (LIST 'TIMES
                               (LIST 'DIFFERENCE 1
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'RD 0
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           1)
                                     3)))))))
     (T
      (AEVAL
       (LIST 'QUOTIENT (LIST 'RD 0 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)) 1)
             3))))) 
(PUT 'N_ELLD 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ELLD) 'OPFN) 
(PUT 'N_ELLD 'DEFINED-ON-LINE '427) 
(PUT 'N_ELLD 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'N_ELLD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ELLD (PHI K)
    (COND ((EVALEQUAL (AEVAL PHI) 0) 0)
          ((AND (OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
                (EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
                (EVALLESSP (AEVAL (LIST 'ABS PHI))
                           (AEVAL (LIST 'QUOTIENT 'PI 2))))
           (AEVAL
            (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY 'S (LIST 'SIN PHI)))
                  (LIST 'TIMES (LIST 'SIGN PHI)
                        (LIST 'DIFFERENCE
                              (LIST 'QUOTIENT
                                    (LIST 'LOG
                                          (LIST 'QUOTIENT (LIST 'PLUS 1 'S)
                                                (LIST 'DIFFERENCE 1 'S)))
                                    2)
                              'S)))))
          ((OR (EVALEQUAL (AEVAL PHI) (AEVAL (LIST 'QUOTIENT 'PI 2)))
               (EVALEQUAL (AEVAL PHI)
                          (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 'PI 2)))))
           (AEVAL (LIST 'TIMES (LIST 'SIGN PHI) (LIST 'N_ELLDC K))))
          (T
           (PROG (L)
             (SETQ L (AEVAL (LIST 'PI_SHIFT PHI)))
             (COND
              ((EVALNEQ (AEVAL (LIST 'SECOND L)) 0)
               (RETURN
                (AEVAL
                 (LIST 'PLUS (LIST 'TIMES (LIST 'SECOND L) (LIST 'N_ELLDC K))
                       (LIST 'N_ELLD (LIST 'FIRST L) K)))))
              (T (RETURN (AEVAL (LIST 'NUM_ELLIPTIC 'ELLIPTIC_D PHI K))))))))) 
(PUT 'ELLIPTIC_D 'NUMBER-OF-ARGS 2) 
(FLAG '(ELLIPTIC_D) 'OPFN) 
(PUT 'ELLIPTIC_D 'DEFINED-ON-LINE '441) 
(PUT 'ELLIPTIC_D 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'ELLIPTIC_D 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC_D (PHI K)
    (PROG (SGN)
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART K)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART K)) 0)))
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART PHI)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART PHI)) 0)))
        (PROGN
         (SETQ SGN (AEVAL (MINUS 1)))
         (SETQ PHI (AEVAL (LIST 'MINUS PHI)))
         (AEVAL 'NIL)))
       (T (SETQ SGN (AEVAL 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SIN PHI)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                               (LIST 'LIST 1 (MINUS 1))
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                               (LIST 'LIST 1 0))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_2ND 0
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 0 1) (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_2ND
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST (MINUS 1)
                                                 (LIST 'EXPT K 2))
                                           (LIST 'LIST 1 0))))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) (AEVAL (LIST 'QUOTIENT 'PI 2)))
        (PROGN
         (SETK 'S (AEVAL (LIST 'COSH (LIST 'IMPART PHI))))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_2ND 0 1 (LIST 'LIST 0 1)
                                     (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 1 0))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_2ND 1 (LIST 'EXPT 'S 2)
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST (MINUS 1) 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))))
                         2)))))
          ((EVALLESSP (AEVAL K) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_2ND 0 1 (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_2ND 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 0 1)
                                                 (LIST 'LIST (MINUS 1) 1)
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT K 2)))
                                                 (LIST 'LIST 1 0))))
                               (LIST 'ELLINT_2ND
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                     (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0)))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_2ND 0
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 1 0))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_2ND
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 1 (LIST 'LIST 0 1)
                                                 (LIST 'LIST 1 (MINUS 1))
                                                 (LIST 'LIST (MINUS 1)
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 1 0))))
                               (LIST 'ELLINT_2ND 1 (LIST 'EXPT 'S 2)
                                     (LIST 'LIST 0 1) (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0)))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SINH (LIST 'IMPART PHI))))
         (COND
          ((EVALNEQ (AEVAL (LIST 'REPART K)) 0)
           (RETURN
            (AEVAL
             (LIST 'MINUS
                   (LIST 'TIMES 'I SGN
                         (LIST 'QUOTIENT
                               (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2)
                                     (LIST 'LIST 0 1) (LIST 'LIST 1 1)
                                     (LIST 'LIST 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0))
                               2))))))
          (T
           (PROGN
            (SETQ K (AEVAL (LIST 'IMPART K)))
            (COND
             ((EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1)
              (RETURN
               (AEVAL
                (LIST 'MINUS
                      (LIST 'TIMES 'I SGN
                            (LIST 'QUOTIENT
                                  (LIST 'ELLINT_2ND 0 (LIST 'EXPT 'S 2)
                                        (LIST 'LIST 0 1) (LIST 'LIST 1 1)
                                        (LIST 'LIST 1
                                              (LIST 'MINUS (LIST 'EXPT K 2)))
                                        (LIST 'LIST 1 0))
                                  2))))))
             (T
              (RETURN
               (AEVAL
                (LIST 'TIMES SGN
                      (LIST 'QUOTIENT
                            (LIST 'DIFFERENCE
                                  (LIST 'MINUS
                                        (LIST 'TIMES 'I
                                              (LIST 'ELLINT_2ND 0
                                                    (LIST 'QUOTIENT 1
                                                          (LIST 'EXPT K 2))
                                                    (LIST 'LIST 0 1)
                                                    (LIST 'LIST 1 1)
                                                    (LIST 'LIST 1
                                                          (LIST 'MINUS
                                                                (LIST 'EXPT K
                                                                      2)))
                                                    (LIST 'LIST 1 0))))
                                  (LIST 'ELLINT_2ND
                                        (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                        (LIST 'EXPT 'S 2) (LIST 'LIST 0 1)
                                        (LIST 'LIST 1 1)
                                        (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                        (LIST 'LIST 1 0)))
                            2))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (SETK 'S (AEVAL (LIST 'SIN PHI)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'TIMES K 'S))) 0)
             (EVALGREATERP (AEVAL (LIST 'EXPT (LIST 'TIMES K 'S) 2)) 1))
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN (LIST 'EXPT 'S 3)
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'ELLINT_2ND 0
                                  (LIST 'QUOTIENT 1
                                        (LIST 'EXPT (LIST 'TIMES K 'S) 2))
                                  (LIST 'LIST 0 1)
                                  (LIST 'LIST 1
                                        (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                  (LIST 'LIST 1
                                        (LIST 'MINUS
                                              (LIST 'TIMES (LIST 'EXPT K 2)
                                                    (LIST 'EXPT 'S 2))))
                                  (LIST 'LIST 1 0))
                            (LIST 'TIMES 'I
                                  (LIST 'ELLINT_2ND
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        1 (LIST 'LIST 0 1)
                                        (LIST 'LIST 1
                                              (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                        (LIST 'LIST (MINUS 1)
                                              (LIST 'TIMES (LIST 'EXPT K 2)
                                                    (LIST 'EXPT 'S 2)))
                                        (LIST 'LIST 1 0))))
                      2)))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN (LIST 'EXPT 'S 3)
                (LIST 'QUOTIENT
                      (LIST 'ELLINT_2ND 0 1 (LIST 'LIST 0 1)
                            (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT 'S 2)))
                            (LIST 'LIST 1
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT K 2)
                                              (LIST 'EXPT 'S 2))))
                            (LIST 'LIST 1 0))
                      2)))))))) 
(OPERATOR (LIST 'ELLIPTICPI)) 
(SETK 'ELLIPTICPIRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST 'MINUS (LIST '~ 'A)) (LIST '~ 'K))
                   (LIST 'ELLIPTICPI 'A 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST '~ 'A) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICPI 'A 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'K) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'ELLIPTICE 'K)
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI 0 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'A) 0)
                   (LIST 'QUOTIENT 'PI
                         (LIST 'TIMES 2 (LIST 'SQRT (LIST 'DIFFERENCE 1 'A)))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST 'MINUS (LIST 'EXPT (LIST '~ 'K) 2))
                         (LIST '~ 'K))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT 'PI (LIST 'TIMES 4 (LIST 'PLUS 1 'K)))
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK 'K) 2)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST 'QUOTIENT 'PI 2) (LIST '~ 'A)
                         (LIST '~ 'K))
                   (LIST 'ELLIPTICPI 'A 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST 'MINUS (LIST '~ 'PHI)) (LIST '~ 'A)
                         (LIST '~ 'K))
                   (LIST 'MINUS (LIST 'ELLIPTICPI 'PHI 'A 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST '~ 'PHI) (LIST 'MINUS (LIST '~ 'A))
                         (LIST '~ 'K))
                   (LIST 'ELLIPTICPI 'PHI 'A 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST '~ 'PHI) (LIST '~ 'A)
                         (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ELLIPTICPI 'PHI 'A 'K))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'PHI) 0 0) 'PHI)
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'PHI) 1 0)
                   (LIST 'TAN 'PHI))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'PHI) 0 (LIST '~ 'K))
                   (LIST 'ELLIPTICF 'PHI 'K))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST '~ 'PHI) (LIST '~ 'K) (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'ELLIPTICE 'PHI 'K)
                               (LIST 'TIMES (LIST 'EXPT 'K 2) (LIST 'SIN 'PHI)
                                     (LIST 'QUOTIENT (LIST 'COS 'PHI)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT
                                                                   (LIST 'SIN
                                                                         'PHI)
                                                                   2)))))))
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'PHI) 1 (LIST '~ 'K))
                   (LIST 'DIFFERENCE (LIST 'ELLIPTICF 'PHI 'K)
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE (LIST 'ELLIPTICE 'PHI 'K)
                                     (LIST 'QUOTIENT (LIST 'TAN 'PHI)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT
                                                                   (LIST 'SIN
                                                                         'PHI)
                                                                   2))))))
                               (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2)))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'A) (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       ''PI))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''ELLIPTICPI 'ARG
                                                       ''A ''M)
                                                 (LIST 'LIST ''TIMES 2 'SHIFT
                                                       (LIST 'LIST ''ELLIPTICPI
                                                             ''A ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICPI (LIST '~ 'A) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DF 'K 'X) 'K
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE (LIST 'ELLIPTICPI 'A 'K)
                                           (LIST 'QUOTIENT (LIST 'ELLIPTICE 'K)
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))
                                     (LIST 'DIFFERENCE (LIST 'EXPT 'A 2)
                                           (LIST 'EXPT 'K 2))))
                         (LIST 'TIMES (LIST 'DF 'A 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES
                                                       (LIST 'ELLIPTICPI 'A 'K)
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT 'A
                                                                   4)))
                                                 (LIST 'TIMES
                                                       (LIST 'ELLIPTICK 'K)
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT 'A
                                                                   2))))
                                           (LIST 'TIMES (LIST 'EXPT 'A 2)
                                                 (LIST 'ELLIPTICE 'K)))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'A 2))
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'K 2)
                                                 (LIST 'EXPT 'A 2))
                                           'A)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICPI (LIST '~ 'U) 'A (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'U 'X)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'TIMES (LIST 'EXPT 'K 2)
                                                       (LIST 'EXPT
                                                             (LIST 'SIN 'U)
                                                             2))))
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'TIMES (LIST 'EXPT 'A 2)
                                                 (LIST 'EXPT (LIST 'SIN 'U)
                                                       2)))))
                         (LIST 'TIMES (LIST 'DF 'K 'X) 'K
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'ELLIPTICPI 'U 'A 'K)
                                                 (LIST 'QUOTIENT
                                                       (LIST 'ELLIPTICE 'U 'K)
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES (LIST 'EXPT 'K 2)
                                                 (LIST 'SIN 'U)
                                                 (LIST 'QUOTIENT
                                                       (LIST 'QUOTIENT
                                                             (LIST 'COS 'U)
                                                             (LIST 'SQRT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'EXPT 'K
                                                                      2)
                                                                     (LIST
                                                                      'EXPT
                                                                      (LIST
                                                                       'SIN 'U)
                                                                      2)))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))
                                     (LIST 'DIFFERENCE (LIST 'EXPT 'A 2)
                                           (LIST 'EXPT 'K 2))))
                         (LIST 'TIMES (LIST 'DF 'A 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES
                                                             (LIST 'ELLIPTICPI
                                                                   'U 'A 'K)
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'K 2)
                                                                   (LIST 'EXPT
                                                                         'A
                                                                         4)))
                                                       (LIST 'TIMES
                                                             (LIST 'ELLIPTICF
                                                                   'U 'K)
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'K 2)
                                                                   (LIST 'EXPT
                                                                         'A
                                                                         2))))
                                                 (LIST 'TIMES (LIST 'EXPT 'A 2)
                                                       (LIST 'ELLIPTICE 'U
                                                             'K)))
                                           (LIST 'TIMES (LIST 'EXPT 'A 4)
                                                 (LIST 'SQRT
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'TIMES
                                                                   (LIST 'EXPT
                                                                         'K 2)
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'SIN
                                                                          'U)
                                                                         2))))
                                                 (LIST 'SIN 'U)
                                                 (LIST 'QUOTIENT (LIST 'COS 'U)
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'TIMES
                                                                   (LIST 'EXPT
                                                                         'A 2)
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'SIN
                                                                          'U)
                                                                         2))))))
                                     (LIST 'TIMES
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'A 2))
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'K 2)
                                                 (LIST 'EXPT 'A 2))
                                           'A)))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICPI (LIST '~ 'PHI) (LIST '~ 'A) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'N_ELLPI 'PHI 'A 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'PHI)
                               (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ELLIPTICPI (LIST '~ 'A) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'N_ELLPIC 'A 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'A)
                               (LIST 'NUMBERP 'K)
                               (LIST 'NEQ (LIST 'EXPT 'K 2) 1))))))) 
(LET '(ELLIPTICPIRULES)) 
(PUT 'N_ELLPIC 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ELLPIC) 'OPFN) 
(PUT 'N_ELLPIC 'DEFINED-ON-LINE '560) 
(PUT 'N_ELLPIC 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'N_ELLPIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ELLPIC (A K)
    (COND
     ((EVALEQUAL (AEVAL K) 0)
      (AEVAL (LIST 'RC 0 (LIST 'DIFFERENCE 1 (LIST 'EXPT A 2)))))
     ((EVALEQUAL (AEVAL A) 0) (AEVAL (LIST 'NUM_ELLK K)))
     ((OR (EVALEQUAL (AEVAL A) (AEVAL K))
          (EVALEQUAL (AEVAL A) (AEVAL (LIST 'MINUS K))))
      (AEVAL
       (LIST 'QUOTIENT (LIST 'NUM_ELLEC K)
             (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
     ((AND (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0)
           (EVALGREATERP (AEVAL (LIST 'ABS K)) 1))
      (AEVAL
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'ELLINT_3RD 0 (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                         (LIST 'LIST 1 0) (LIST 'LIST 1 (MINUS 1))
                         (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                         (LIST 'LIST 0 1)
                         (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT A 2))))
                   (LIST 'TIMES 'I
                         (LIST 'ELLINT_3RD (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                               1 (LIST 'LIST 1 0) (LIST 'LIST 1 (MINUS 1))
                               (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                               (LIST 'LIST 0 1)
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT A 2))))))
             2)))
     (T
      (AEVAL
       (LIST 'QUOTIENT
             (LIST 'ELLINT_3RD 0 1 (LIST 'LIST 1 0) (LIST 'LIST 1 (MINUS 1))
                   (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                   (LIST 'LIST 0 1)
                   (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT A 2))))
             2))))) 
(PUT 'N_ELLPI 'NUMBER-OF-ARGS 3) 
(FLAG '(N_ELLPI) 'OPFN) 
(PUT 'N_ELLPI 'DEFINED-ON-LINE '570) 
(PUT 'N_ELLPI 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'N_ELLPI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_ELLPI (PHI A K)
    (COND ((EVALEQUAL (AEVAL PHI) 0) 0)
          ((OR (EVALEQUAL (AEVAL PHI) (AEVAL (LIST 'QUOTIENT 'PI 2)))
               (EVALEQUAL (AEVAL PHI)
                          (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 'PI 2)))))
           (AEVAL (LIST 'TIMES (LIST 'SIGN PHI) (LIST 'N_ELLPIC A K))))
          ((AND (EVALEQUAL (AEVAL K) 0) (EVALEQUAL (AEVAL A) 0)) (AEVAL PHI))
          ((EVALEQUAL (AEVAL K) 0)
           (COND
            ((OR (EVALEQUAL (AEVAL A) 1) (EVALEQUAL (AEVAL A) (MINUS 1)))
             (AEVAL (LIST 'TAN PHI)))
            (T
             (AEVAL
              (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY 'S (LIST 'SIN PHI)))
                    (LIST 'TIMES 'S
                          (LIST 'RC (LIST 'DIFFERENCE 1 (LIST 'EXPT 'S 2))
                                (LIST 'DIFFERENCE 1
                                      (LIST 'TIMES (LIST 'EXPT A 2)
                                            (LIST 'EXPT 'S 2))))))))))
          ((EVALEQUAL (AEVAL (LIST 'EXPT K 2)) 1)
           (COND
            ((EVALEQUAL (AEVAL (LIST 'EXPT A 2)) 1)
             (AEVAL
              (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY 'S (LIST 'SIN PHI)))
                    (LIST 'QUOTIENT
                          (LIST 'PLUS
                                (LIST 'TIMES 'S
                                      (LIST 'RC 1
                                            (LIST 'DIFFERENCE 1
                                                  (LIST 'EXPT 'S 2))))
                                (LIST 'QUOTIENT 'S
                                      (LIST 'DIFFERENCE 1 (LIST 'EXPT 'S 2))))
                          2))))
            (T
             (AEVAL
              (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY 'S (LIST 'SIN PHI)))
                    (LIST 'TIMES
                          (LIST 'QUOTIENT 'S
                                (LIST 'DIFFERENCE 1 (LIST 'EXPT A 2)))
                          (LIST 'DIFFERENCE
                                (LIST 'RC 1
                                      (LIST 'DIFFERENCE 1 (LIST 'EXPT 'S 2)))
                                (LIST 'TIMES (LIST 'EXPT A 2)
                                      (LIST 'RC 1
                                            (LIST 'DIFFERENCE 1
                                                  (LIST 'TIMES (LIST 'EXPT A 2)
                                                        (LIST 'EXPT 'S
                                                              2))))))))))))
          ((EVALEQUAL (AEVAL (LIST 'EXPT A 2)) 1)
           (AEVAL
            (LIST 'DIFFERENCE (LIST 'NUM_ELLF PHI K)
                  (LIST 'QUOTIENT
                        (LIST 'DIFFERENCE (LIST 'NUM_ELLE PHI K)
                              (LIST 'TIMES (LIST 'TAN PHI)
                                    (LIST 'SQRT
                                          (LIST 'DIFFERENCE 1
                                                (LIST 'TIMES (LIST 'EXPT K 2)
                                                      (LIST 'EXPT
                                                            (LIST 'SIN PHI)
                                                            2))))))
                        (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))))
          ((EVALEQUAL (AEVAL A) 0) (AEVAL (LIST 'NUM_ELLF PHI K)))
          ((EVALEQUAL (AEVAL (LIST 'EXPT A 2)) (AEVAL (LIST 'EXPT K 2)))
           (AEVAL
            (LIST 'QUOTIENT
                  (LIST 'DIFFERENCE (LIST 'NUM_ELLE PHI K)
                        (LIST 'TIMES (LIST 'EXPT K 2) (LIST 'SIN PHI)
                              (LIST 'QUOTIENT (LIST 'COS PHI)
                                    (LIST 'SQRT
                                          (LIST 'DIFFERENCE 1
                                                (LIST 'TIMES (LIST 'EXPT K 2)
                                                      (LIST 'EXPT
                                                            (LIST 'SIN PHI)
                                                            2)))))))
                  (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
          (T
           (PROG (L)
             (SETQ L (AEVAL (LIST 'PI_SHIFT PHI)))
             (COND
              ((EVALNEQ (AEVAL (LIST 'SECOND L)) 0)
               (RETURN
                (AEVAL
                 (LIST 'PLUS
                       (LIST 'TIMES (LIST 'SECOND L) (LIST 'N_ELLPIC A K))
                       (LIST 'N_ELLPI (LIST 'FIRST L) A K)))))
              (T (RETURN (AEVAL (LIST 'ELLIPTIC_PI PHI A K))))))))) 
(PUT 'ELLIPTIC_PI 'NUMBER-OF-ARGS 3) 
(FLAG '(ELLIPTIC_PI) 'OPFN) 
(PUT 'ELLIPTIC_PI 'DEFINED-ON-LINE '595) 
(PUT 'ELLIPTIC_PI 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'ELLIPTIC_PI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ELLIPTIC_PI (PHI A K)
    (PROG (SGN)
      (COND ((EVALEQUAL (AEVAL A) 0) (RETURN (AEVAL (LIST 'ELLIPTIC_F PHI K))))
            ((EVALEQUAL (AEVAL (LIST 'EXPT A 2)) 1)
             (RETURN
              (AEVAL
               (LIST 'PLUS
                     (LIST 'DIFFERENCE (LIST 'ELLIPTIC_F PHI K)
                           (LIST 'QUOTIENT (LIST 'ELLIPTIC_E PHI K)
                                 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))
                     (LIST 'QUOTIENT (LIST 'TAN PHI)
                           (LIST 'TIMES (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                                 (LIST 'SQRT
                                       (LIST 'DIFFERENCE 1
                                             (LIST 'TIMES (LIST 'EXPT K 2)
                                                   (LIST 'EXPT (LIST 'SIN PHI)
                                                         2))))))))))
            ((EVALEQUAL (AEVAL (LIST 'EXPT A 2)) (AEVAL (LIST 'EXPT K 2)))
             (RETURN
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'QUOTIENT (LIST 'ELLIPTIC_E PHI K)
                           (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))
                     (LIST 'TIMES (LIST 'SIN PHI)
                           (LIST 'QUOTIENT (LIST 'COS PHI)
                                 (LIST 'TIMES
                                       (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'TIMES
                                                         (LIST 'EXPT K 2)
                                                         (LIST 'EXPT
                                                               (LIST 'SIN PHI)
                                                               2))))))))))))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART K)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART K)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART K)) 0)))
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (COND
       ((OR (EVALLESSP (AEVAL (LIST 'REPART PHI)) 0)
            (AND (EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
                 (EVALLESSP (AEVAL (LIST 'IMPART PHI)) 0)))
        (PROGN
         (SETQ SGN (AEVAL (MINUS 1)))
         (SETQ PHI (AEVAL (LIST 'MINUS PHI)))
         (AEVAL 'NIL)))
       (T (SETQ SGN (AEVAL 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'IMPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SIN PHI)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_3RD 0 (LIST 'EXPT 'S 2) (LIST 'LIST 1 0)
                               (LIST 'LIST 1 (MINUS 1))
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                               (LIST 'LIST 0 1)
                               (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT A 2))))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'ELLINT_3RD 0
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'LIST 1 0) (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 0 1)
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT A 2))))
                               (LIST 'TIMES 'I
                                     (LIST 'ELLINT_3RD
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'EXPT 'S 2) (LIST 'LIST 1 0)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST (MINUS 1)
                                                 (LIST 'EXPT K 2))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT A 2))))))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) (AEVAL (LIST 'QUOTIENT 'PI 2)))
        (PROGN
         (SETK 'S (AEVAL (LIST 'COSH (LIST 'IMPART PHI))))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'IMPART K)) 0)
               (EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1))
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'DIFFERENCE
                         (LIST 'QUOTIENT
                               (LIST 'ELLINT_3RD 0 1 (LIST 'LIST 1 0)
                                     (LIST 'LIST 1 (MINUS 1))
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT K 2)))
                                     (LIST 'LIST 0 1)
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT A 2))))
                               2)
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST 'ELLINT_3RD 1 (LIST 'EXPT 'S 2)
                                           (LIST 'LIST 1 0)
                                           (LIST 'LIST (MINUS 1) 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT A 2))))
                                     2)))))))
          ((EVALLESSP (AEVAL K) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_3RD 0 1 (LIST 'LIST 1 0)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT A 2))))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_3RD 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 1 0)
                                                 (LIST 'LIST (MINUS 1) 1)
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT K 2)))
                                                 (LIST 'LIST 0 1)
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT A
                                                                   2))))))
                               (LIST 'ELLINT_3RD
                                     (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                     (LIST 'EXPT 'S 2) (LIST 'LIST 1 0)
                                     (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 0 1)
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT A 2)))))
                         2)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES SGN
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'ELLINT_3RD 0
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                           (LIST 'LIST 1 0)
                                           (LIST 'LIST 1 (MINUS 1))
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT K 2)))
                                           (LIST 'LIST 0 1)
                                           (LIST 'LIST 1
                                                 (LIST 'MINUS
                                                       (LIST 'EXPT A 2))))
                                     (LIST 'TIMES 'I
                                           (LIST 'ELLINT_3RD
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT K 2))
                                                 1 (LIST 'LIST 1 0)
                                                 (LIST 'LIST 1 (MINUS 1))
                                                 (LIST 'LIST (MINUS 1)
                                                       (LIST 'EXPT K 2))
                                                 (LIST 'LIST 0 1)
                                                 (LIST 'LIST 1
                                                       (LIST 'MINUS
                                                             (LIST 'EXPT A
                                                                   2))))))
                               (LIST 'ELLINT_3RD 1 (LIST 'EXPT 'S 2)
                                     (LIST 'LIST 1 0) (LIST 'LIST (MINUS 1) 1)
                                     (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                     (LIST 'LIST 0 1)
                                     (LIST 'LIST 1
                                           (LIST 'MINUS (LIST 'EXPT A 2)))))
                         2))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART PHI)) 0)
        (PROGN
         (SETK 'S (AEVAL (LIST 'SINH (LIST 'IMPART PHI))))
         (COND
          ((EVALNEQ (AEVAL (LIST 'REPART K)) 0)
           (RETURN
            (AEVAL
             (LIST 'TIMES 'I SGN
                   (LIST 'QUOTIENT
                         (LIST 'ELLINT_3RD 0 (LIST 'EXPT 'S 2) (LIST 'LIST 1 0)
                               (LIST 'LIST 1 1) (LIST 'LIST 1 (LIST 'EXPT K 2))
                               (LIST 'LIST 0 1)
                               (LIST 'LIST 1 (LIST 'EXPT A 2)))
                         2)))))
          (T
           (PROGN
            (SETQ K (AEVAL (LIST 'IMPART K)))
            (COND
             ((EVALLESSP (AEVAL (LIST 'TIMES K 'S)) 1)
              (RETURN
               (AEVAL
                (LIST 'TIMES 'I SGN
                      (LIST 'QUOTIENT
                            (LIST 'ELLINT_3RD 0 (LIST 'EXPT 'S 2)
                                  (LIST 'LIST 1 0) (LIST 'LIST 1 1)
                                  (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT K 2)))
                                  (LIST 'LIST 0 1)
                                  (LIST 'LIST 1 (LIST 'EXPT A 2)))
                            2)))))
             (T
              (RETURN
               (AEVAL
                (LIST 'TIMES SGN
                      (LIST 'QUOTIENT
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES 'I
                                        (LIST 'ELLINT_3RD 0
                                              (LIST 'QUOTIENT 1
                                                    (LIST 'EXPT K 2))
                                              (LIST 'LIST 1 0) (LIST 'LIST 1 1)
                                              (LIST 'LIST 1
                                                    (LIST 'MINUS
                                                          (LIST 'EXPT K 2)))
                                              (LIST 'LIST 0 1)
                                              (LIST 'LIST 1 (LIST 'EXPT A 2))))
                                  (LIST 'ELLINT_3RD
                                        (LIST 'QUOTIENT 1 (LIST 'EXPT K 2))
                                        (LIST 'EXPT 'S 2) (LIST 'LIST 1 0)
                                        (LIST 'LIST 1 1)
                                        (LIST 'LIST (MINUS 1) (LIST 'EXPT K 2))
                                        (LIST 'LIST 0 1)
                                        (LIST 'LIST 1 (LIST 'EXPT A 2))))
                            2))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL))))
      (SETK 'S (AEVAL (LIST 'SIN PHI)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'TIMES K 'S))) 0)
             (EVALGREATERP (AEVAL (LIST 'EXPT (LIST 'TIMES K 'S) 2)) 1))
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'ELLINT_3RD 0
                                  (LIST 'QUOTIENT 1
                                        (LIST 'EXPT (LIST 'TIMES K 'S) 2))
                                  (LIST 'LIST 1 0)
                                  (LIST 'LIST 1
                                        (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                  (LIST 'LIST 1
                                        (LIST 'MINUS
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2)))
                                  (LIST 'LIST 0 1)
                                  (LIST 'LIST 1
                                        (LIST 'MINUS
                                              (LIST 'EXPT (LIST 'TIMES A 'S)
                                                    2))))
                            (LIST 'TIMES 'I
                                  (LIST 'ELLINT_3RD
                                        (LIST 'QUOTIENT 1
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        1 (LIST 'LIST 1 0)
                                        (LIST 'LIST 1
                                              (LIST 'MINUS (LIST 'EXPT 'S 2)))
                                        (LIST 'LIST (MINUS 1)
                                              (LIST 'EXPT (LIST 'TIMES K 'S)
                                                    2))
                                        (LIST 'LIST 0 1)
                                        (LIST 'LIST 1
                                              (LIST 'MINUS
                                                    (LIST 'EXPT
                                                          (LIST 'TIMES A 'S)
                                                          2))))))
                      2)))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES SGN 'S
                (LIST 'QUOTIENT
                      (LIST 'ELLINT_3RD 0 1 (LIST 'LIST 1 0)
                            (LIST 'LIST 1 (LIST 'MINUS (LIST 'EXPT 'S 2)))
                            (LIST 'LIST 1
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT K 2)
                                              (LIST 'EXPT 'S 2))))
                            (LIST 'LIST 0 1)
                            (LIST 'LIST 1
                                  (LIST 'MINUS
                                        (LIST 'TIMES (LIST 'EXPT A 2)
                                              (LIST 'EXPT 'S 2)))))
                      2)))))))) 
(PUT 'NUM_JACOBIE 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIE) 'OPFN) 
(PUT 'NUM_JACOBIE 'DEFINED-ON-LINE '680) 
(PUT 'NUM_JACOBIE 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_JACOBIE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIE (U K)
    (COND ((EVALEQUAL (AEVAL U) 0) 0) ((EVALEQUAL (AEVAL K) 0) (AEVAL U))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'TANH U)))
          (T (AEVAL (LIST 'NUM_JACOBIE1 U K))))) 
(PUT 'NUM_JACOBIE1 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIE1) 'OPFN) 
(PUT 'NUM_JACOBIE1 'DEFINED-ON-LINE '686) 
(PUT 'NUM_JACOBIE1 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_JACOBIE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIE1 (U K)
    (PROG (TAU T3 XI TMP)
      (SETQ TAU (AEVAL (LIST 'K2TAU K)))
      (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
      (SETQ XI (AEVAL (LIST 'QUOTIENT U (LIST 'EXPT T3 2))))
      (SETQ TMP
              (AEVAL
               (LIST 'QUOTIENT (LIST 'N_THETA4D XI 1 TAU)
                     (LIST 'TIMES (LIST 'NUM1_THETA4 XI TAU)
                           (LIST 'EXPT T3 2)))))
      (RETURN
       (AEVAL
        (LIST 'PLUS TMP
              (LIST 'TIMES U
                    (LIST 'QUOTIENT (LIST 'NUM_ELLEC K)
                          (LIST 'NUM_ELLK K)))))))) 
(SETK 'JACOBIERULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'JACOBIE 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY (LIST 'JACOBIE (LIST '~ 'PHI) 0) 'PHI)
             (LIST 'REPLACEBY (LIST 'JACOBIE (LIST 'TIMES 'I (LIST '~ 'PHI)) 0)
                   (LIST 'TIMES 'I 'PHI))
             (LIST 'REPLACEBY (LIST 'JACOBIE (LIST '~ 'PHI) 1)
                   (LIST 'TANH 'PHI))
             (LIST 'REPLACEBY (LIST 'JACOBIE (LIST 'TIMES 'I (LIST '~ 'PHI)) 1)
                   (LIST 'TIMES 'I (LIST 'TAN 'PHI)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE (LIST 'MINUS (LIST '~ 'PHI)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBIE 'PHI 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE (LIST '~ 'PHI) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIE 'PHI 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE (LIST 'ELLIPTICK (LIST '~ 'M)) (LIST '~ 'M))
                   (LIST 'ELLIPTICE 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST 'ELLIPTICK (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'BOOLVALUE*
                                                  (LIST 'REVALX
                                                        (LIST 'LIST ''EVENP
                                                              'SHIFT))))
                                      (LIST 'SETQ 'SHIFT
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''DIFFERENCE
                                                        'SHIFT 1)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''JACOBIE 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES 'SHIFT
                                                       (LIST 'LIST ''ELLIPTICE
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'BOOLVALUE*
                                                  (LIST 'REVALX
                                                        (LIST 'LIST ''EVENP
                                                              'SHIFT))))
                                      (LIST 'SETQ 'SHIFT
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''DIFFERENCE
                                                        'SHIFT 1)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES ''I
                                                                   'SHIFT))
                                                       (LIST 'LIST
                                                             ''|ELLIPTICK'|
                                                             ''M)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''JACOBIE 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES 'SHIFT ''I
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''|ELLIPTICK'|
                                                                   ''M)
                                                             (LIST 'LIST
                                                                   ''|ELLIPTICE'|
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 2))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIE (LIST '~ 'PHI) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DF 'PHI 'X)
                               (LIST 'EXPT (LIST 'JACOBIDN 'PHI 'M) 2))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 'M
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES
                                                             (LIST 'JACOBISN
                                                                   'PHI 'M)
                                                             (LIST 'JACOBICN
                                                                   'PHI 'M)
                                                             (LIST 'JACOBIDN
                                                                   'PHI 'M))
                                                       (LIST 'TIMES
                                                             (LIST 'JACOBIE
                                                                   'PHI 'M)
                                                             (LIST 'EXPT
                                                                   (LIST
                                                                    'JACOBICN
                                                                    'PHI 'M)
                                                                   2)))
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2))))
                                     (LIST 'TIMES 'M 'PHI
                                           (LIST 'EXPT (LIST 'JACOBISN 'PHI 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICE (LIST '~ 'M)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'ELLIPTICE 'M)
                               (LIST 'ELLIPTICK 'M))
                         'M))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ELLIPTICK (LIST '~ 'M)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT (LIST 'ELLIPTICE 'M)
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))
                               (LIST 'ELLIPTICK 'M))
                         'M))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST '|ELLIPTICE'| (LIST '~ 'M)) (LIST '~ 'M))
                   (LIST 'TIMES 'M
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE (LIST '|ELLIPTICK'| 'M)
                                     (LIST '|ELLIPTICE'| 'M))
                               (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST '|ELLIPTICK'| (LIST '~ 'M)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 'M (LIST '|ELLIPTICK'| 'M))
                               (LIST 'QUOTIENT (LIST '|ELLIPTICE'| 'M) 'M))
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2))))
             (LIST 'REPLACEBY (LIST 'JACOBIE (LIST '~ 'PHI) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIE 'PHI 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'PHI)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBIERULES)) 
(PUT 'NUM_JACOBIZETA 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIZETA) 'OPFN) 
(PUT 'NUM_JACOBIZETA 'DEFINED-ON-LINE '758) 
(PUT 'NUM_JACOBIZETA 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_JACOBIZETA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIZETA (U M)
    (PROG (PHI_LIST CLIST Z CN PHI)
      (SETQ PHI_LIST
              (AEVAL
               (LIST 'REST
                     (LIST 'PHI_FUNCTION 1
                           (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT M 2))) M
                           U))))
      (SETQ CLIST
              (AEVAL
               (LIST 'REST
                     (LIST 'REVERSE
                           (LIST 'THIRD
                                 (LIST 'AGM_FUNCTION 1
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'EXPT M 2)))
                                       M))))))
      (SETQ Z (AEVAL 0))
      (PROG (PHI)
        (SETQ PHI (GETRLIST (AEVAL PHI_LIST)))
       LAB
        (COND ((NULL PHI) (RETURN NIL)))
        ((LAMBDA (PHI)
           (PROGN
            (SETQ CN (AEVAL (LIST 'FIRST CLIST)))
            (SETQ Z (AEVAL (LIST 'PLUS (LIST 'TIMES CN (LIST 'SIN PHI)) Z)))
            (SETQ CLIST (AEVAL (LIST 'REST CLIST)))
            (AEVAL 'NIL)))
         (CAR PHI))
        (SETQ PHI (CDR PHI))
        (GO LAB))
      (RETURN (AEVAL Z)))) 
(PUT 'NUM_JACOBIZETA1 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIZETA1) 'OPFN) 
(PUT 'NUM_JACOBIZETA1 'DEFINED-ON-LINE '774) 
(PUT 'NUM_JACOBIZETA1 'DEFINED-IN-FILE 'ELLIPFN/EFELLINT.RED) 
(PUT 'NUM_JACOBIZETA1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIZETA1 (U K)
    (PROG (TAU T3 XI)
      (SETQ TAU (AEVAL (LIST 'K2TAU K)))
      (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
      (SETQ XI (AEVAL (LIST 'QUOTIENT U (LIST 'EXPT T3 2))))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT (LIST 'N_THETA4D XI 1 TAU)
              (LIST 'TIMES (LIST 'NUM1_THETA4 XI TAU) (LIST 'EXPT T3 2))))))) 
(SETK 'JACOBIZETARULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'JACOBIZETA (LIST '~ 'U) 0) 0)
             (LIST 'REPLACEBY (LIST 'JACOBIZETA (LIST '~ 'U) 1)
                   (LIST 'TANH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBIZETA 'U 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIZETA 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBIZETA 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA (LIST 'ELLIPTICK (LIST '~ 'M))
                         (LIST '~ 'M))
                   0)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST 'ELLIPTICK (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'BOOLVALUE*
                                                  (LIST 'REVALX
                                                        (LIST 'LIST ''EVENP
                                                              'SHIFT))))
                                      (LIST 'SETQ 'SHIFT
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''DIFFERENCE
                                                        'SHIFT 1)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             'SHIFT)
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''JACOBIZETA 'ARG
                                                 ''M))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'BOOLVALUE*
                                                  (LIST 'REVALX
                                                        (LIST 'LIST ''EVENP
                                                              'SHIFT))))
                                      (LIST 'SETQ 'SHIFT
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''DIFFERENCE
                                                        'SHIFT 1)))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES ''I
                                                                   'SHIFT))
                                                       (LIST 'LIST
                                                             ''|ELLIPTICK'|
                                                             ''M)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''JACOBIZETA 'ARG
                                                       ''M)
                                                 (LIST 'LIST ''TIMES ''I ''PI
                                                       (LIST 'LIST ''QUOTIENT
                                                             'SHIFT
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICK
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 2))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA
                         (LIST 'PLUS (LIST '~ 'U)
                               (LIST 'TIMES 2 (LIST 'ELLIPTICK (LIST '~ 'M))))
                         'M)
                   (LIST 'JACOBIZETA 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA
                         (LIST 'DIFFERENCE (LIST 'ELLIPTICK (LIST '~ 'M))
                               (LIST '~ 'U))
                         'M)
                   (LIST 'MINUS
                         (LIST 'JACOBIZETA (LIST 'PLUS (LIST 'ELLIPTICK 'M) 'U)
                               'M)))
             (LIST 'REPLACEBY (LIST 'JACOBIZETA (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIZETA 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIZETA (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'DF
                         (LIST 'DIFFERENCE (LIST 'JACOBIE 'U 'K)
                               (LIST 'TIMES 'U
                                     (LIST 'QUOTIENT (LIST 'ELLIPTICE 'K)
                                           (LIST 'ELLIPTICK 'K))))
                         'X))))) 
(LET '(JACOBIZETARULES)) 
(ENDMODULE) 