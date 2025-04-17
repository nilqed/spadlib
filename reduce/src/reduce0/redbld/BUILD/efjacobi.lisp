(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFJACOBI)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'JACOBIAM 'JACOBISN 'JACOBICN 'JACOBIDN)) 
(OPERATOR (LIST 'JACOBIE 'JACOBINS 'JACOBINC 'JACOBIND)) 
(OPERATOR (LIST 'JACOBIZETA 'JACOBISC 'JACOBISD 'JACOBICS)) 
(OPERATOR (LIST 'JACOBIDS 'JACOBICD 'JACOBIDC)) 
(OPERATOR (LIST 'ELLIPTICK '|ELLIPTICK'| 'ELLIPTICE '|ELLIPTICE'|)) 
(PUT 'N_JACAM 'NUMBER-OF-ARGS 2) 
(FLAG '(N_JACAM) 'OPFN) 
(PUT 'N_JACAM 'DEFINED-ON-LINE '68) 
(PUT 'N_JACAM 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'N_JACAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_JACAM (Z K)
    (PROG (N POW TERM TOTAL TOL M BOUND F KP KK Q)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ KP (AEVAL (LIST 'NUM_ELLKC K)))
      (SETQ KK (AEVAL (LIST 'NUM_ELLK K)))
      (SETQ Q
              (AEVAL
               (LIST 'EXP
                     (LIST 'MINUS (LIST 'TIMES 'PI (LIST 'QUOTIENT KP KK))))))
      (SETQ Z (AEVAL (LIST 'TIMES 'PI (LIST 'QUOTIENT Z (LIST 'TIMES 2 KK)))))
      (SETQ N (AEVAL 1))
      (SETQ TOTAL (AEVAL Z))
      (SETQ BOUND (AEVAL (LIST 'EXP (LIST 'ABS (LIST 'IMPART Z)))))
      (SETQ F (AEVAL (LIST 'EXPT BOUND 2)))
      (REPEAT
       (PROGN
        (SETQ POW (AEVAL* (LIST 'EXPT Q N)))
        (SETQ POW
                (AEVAL*
                 (LIST 'QUOTIENT POW
                       (LIST 'TIMES N (LIST 'PLUS 1 (LIST 'EXPT POW 2))))))
        (SETQ TERM
                (AEVAL* (LIST 'TIMES 2 POW (LIST 'SIN (LIST 'TIMES 2 N Z)))))
        (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))
        (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
        (SETQ M (AEVAL* (LIST 'TIMES (LIST 'ABS POW) BOUND)))
        (AEVAL* 'NIL))
       (OR
        (AND (EVALEQUAL (AEVAL* TOTAL) 0) (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
        (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
      (RETURN (AEVAL TOTAL)))) 
(PUT 'NUM_JACOBIAM 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIAM) 'OPFN) 
(PUT 'NUM_JACOBIAM 'DEFINED-ON-LINE '98) 
(PUT 'NUM_JACOBIAM 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBIAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIAM (U M)
    (LIST 'FIRST
          (LIST 'PHI_FUNCTION 1
                (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT M 2))) M U))) 
(SETK 'JACOBIAMRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'JACOBIAM (LIST '~ 'U) 0) 'U)
             (LIST 'REPLACEBY (LIST 'JACOBIAM (LIST '~ 'U) 1)
                   (LIST 'ASIN (LIST 'TANH 'U)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIAM (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBIAM 'U 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIAM (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIAM 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBIAM 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIAM (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'JACOBIDN 'U 'M) (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'PLUS
                                     (LIST 'TIMES 'U
                                           (LIST 'QUOTIENT
                                                 (LIST 'JACOBIDN 'U 'M) 'M))
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'M
                                                       (LIST 'JACOBISN 'U 'M)
                                                       (LIST 'JACOBICN 'U 'K))
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIE 'U 'M)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'JACOBIDN 'U
                                                                   'M)
                                                             'M)))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))))))
             (LIST 'REPLACEBY (LIST 'JACOBIAM (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIAM 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBIAMRULES)) 
(SETK 'JACOBISNRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBISN (LIST '~ 'U) 0) (LIST 'SIN 'U))
             (LIST 'REPLACEBY (LIST 'JACOBISN (LIST '~ 'U) 1) (LIST 'TANH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBISN 'U 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBISN 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBISN 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT
                               (LIST 'PLUS 1
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 'I (LIST 'SQRT 'K)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'SQRT (LIST 'PLUS 1 'K))
                               (LIST 'TIMES 'I
                                     (LIST 'SQRT (LIST 'DIFFERENCE 1 'K))))
                         (LIST 'SQRT (LIST 'TIMES 2 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBISN
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''JACOBICD
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBISN 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST
                                                                   ''JACOBINS
                                                                   'ARG ''M)
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'TIMES 'I
                         (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D)
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBISN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'JACOBICN 'U 'M)
                               (LIST 'JACOBIDN 'U 'M) (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'M
                                                       (LIST 'JACOBISN 'U 'M)
                                                       (LIST 'EXPT
                                                             (LIST 'JACOBICN 'U
                                                                   'M)
                                                             2))
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIE 'U 'M)
                                                       (LIST 'JACOBICN 'U 'M)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'JACOBIDN 'U
                                                                   'M)
                                                             'M)))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))
                                     (LIST 'TIMES 'U (LIST 'JACOBICN 'U 'M)
                                           (LIST 'QUOTIENT
                                                 (LIST 'JACOBIDN 'U 'M)
                                                 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBISN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'DIFFERENCE (LIST 'JACOBIDN 'U 'M)
                                     (LIST 'TIMES 'M (LIST 'JACOBICN 'U 'M))))
                         'M))
             (LIST 'REPLACEBY (LIST 'JACOBISN (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBISN 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBISNRULES)) 
(SETK 'JACOBICNRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBICN (LIST '~ 'U) 0) (LIST 'COS 'U))
             (LIST 'REPLACEBY (LIST 'JACOBICN (LIST '~ 'U) 1) (LIST 'SECH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBICN 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBICN 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBICN 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
                         (LIST 'SQRT
                               (LIST 'PLUS 1
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'SQRT (LIST 'PLUS 1 'K))
                         (LIST 'SQRT 'K)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'DIFFERENCE 1 'I)
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2))))
                               (LIST 'SQRT (LIST 'TIMES 2 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBICN
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES 'S
                                                                   (LIST 'LIST
                                                                         ''JACOBISD
                                                                         'ARG
                                                                         ''M)
                                                                   (LIST 'LIST
                                                                         ''SQRT
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'LIST
                                                                           ''EXPT
                                                                           ''M
                                                                           2))))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBICN
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES ''I
                                                                   'S
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         (LIST
                                                                          'LIST
                                                                          ''JACOBIDS
                                                                          'ARG
                                                                          ''M)
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBICN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'JACOBISN 'U 'M)
                                     (LIST 'JACOBIDN 'U 'M) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIE 'U 'M)
                                                       (LIST 'JACOBISN 'U 'M)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'JACOBIDN 'U
                                                                   'M)
                                                             'M))
                                                 (LIST 'TIMES 'M
                                                       (LIST 'EXPT
                                                             (LIST 'JACOBISN 'U
                                                                   'M)
                                                             2)
                                                       (LIST 'JACOBICN 'U 'M)))
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))
                                     (LIST 'TIMES 'U (LIST 'JACOBISN 'U 'M)
                                           (LIST 'QUOTIENT
                                                 (LIST 'JACOBIDN 'U 'M)
                                                 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBICN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'QUOTIENT
                         (LIST 'ASIN (LIST 'TIMES 'M (LIST 'JACOBISN 'U 'M)))
                         'M))
             (LIST 'REPLACEBY (LIST 'JACOBICN (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBICN 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBICNRULES)) 
(SETK 'JACOBIDNRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'JACOBIDN (LIST '~ 'U) 0) 1)
             (LIST 'REPLACEBY (LIST 'JACOBIDN (LIST '~ 'U) 1) (LIST 'SECH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIDN 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBIDN 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBIDN 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'SQRT
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'SQRT (LIST 'PLUS 1 'K)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'SQRT (LIST 'DIFFERENCE 1 'K))
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT (LIST 'PLUS 1 'K))))
                               (LIST 'DIFFERENCE 1 'I))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBIDN 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''SQRT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         1
                                                                         (LIST
                                                                          'LIST
                                                                          ''EXPT
                                                                          ''M
                                                                          2)))
                                                             (LIST 'LIST
                                                                   ''JACOBIND
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBIDN
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES ''I
                                                                   (LIST 'LIST
                                                                         ''JACOBICS
                                                                         'ARG
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIDN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISN 'U 'M)
                                     (LIST 'JACOBICN 'U 'M) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 'M
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES
                                                             (LIST 'JACOBIE 'U
                                                                   'M)
                                                             (LIST 'JACOBISN 'U
                                                                   'M)
                                                             (LIST 'JACOBICN 'U
                                                                   'M))
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT
                                                                   (LIST
                                                                    'JACOBISN
                                                                    'U 'M)
                                                                   2)
                                                             (LIST 'JACOBIDN 'U
                                                                   'M)))
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2))))
                                     (LIST 'TIMES 'M 'U (LIST 'JACOBISN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBIDN (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'ASIN (LIST 'JACOBISN 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBIDN (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIDN 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBIDNRULES)) 
(SETK 'JACOBICDRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBICD (LIST '~ 'U) 0) (LIST 'COS 'U))
             (LIST 'REPLACEBY (LIST 'JACOBICD (LIST '~ 'U) 1) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBICD 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBICD 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBICD 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT
                               (LIST 'PLUS 1
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1 (LIST 'SQRT 'K)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'SQRT (LIST 'PLUS 1 'K))
                               (LIST 'TIMES 'I
                                     (LIST 'SQRT (LIST 'DIFFERENCE 1 'K))))
                         (LIST 'SQRT (LIST 'TIMES 2 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBICD
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''JACOBISN
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBICD 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST
                                                                   ''JACOBIDC
                                                                   'ARG ''M)
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBICD (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2))
                                     (LIST 'JACOBISD 'U 'M)
                                     (LIST 'JACOBIND 'U 'M) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'M 'X) (LIST 'JACOBISN 'U 'M)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE (LIST 'JACOBIE 'U 'M)
                                           (LIST 'TIMES 'U
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2))))
                                     (LIST 'TIMES 'M
                                           (LIST 'EXPT (LIST 'JACOBIDN 'U 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBICD (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'PLUS (LIST 'JACOBIND 'U 'M)
                                     (LIST 'TIMES 'M (LIST 'JACOBISD 'U 'M))))
                         'M))
             (LIST 'REPLACEBY (LIST 'JACOBICD (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBICD 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBICDRULES)) 
(SETK 'JACOBISDRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBISD (LIST '~ 'U) 0) (LIST 'SIN 'U))
             (LIST 'REPLACEBY (LIST 'JACOBISD (LIST '~ 'U) 1) (LIST 'SINH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBISD 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBISD 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBISD 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'PLUS 1
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2)))))
                               (LIST 'SQRT
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 'I
                         (LIST 'TIMES (LIST 'SQRT 'K)
                               (LIST 'SQRT (LIST 'PLUS 1 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'PLUS 1 'I)
                         (LIST 'TIMES (LIST 'SQRT (LIST 'TIMES 2 'K))
                               (LIST 'SQRT
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBISD
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''JACOBICN
                                                                         'ARG
                                                                         ''M)
                                                                   (LIST 'LIST
                                                                         ''SQRT
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'LIST
                                                                           ''EXPT
                                                                           ''M
                                                                           2))))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBISD
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             ''I
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''JACOBINC
                                                                         'ARG
                                                                         ''M)
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'TIMES 'I
                         (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D)
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBISD (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'JACOBICD 'U 'K)
                               (LIST 'JACOBIND 'U 'K) (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'MINUS
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBICN 'U 'K)
                                                       (LIST 'JACOBIE 'U 'K)))
                                           (LIST 'TIMES (LIST 'JACOBICN 'U 'K)
                                                 'U
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2)))
                                           (LIST 'TIMES (LIST 'JACOBIDN 'U 'K)
                                                 (LIST 'JACOBISN 'U 'K)
                                                 (LIST 'EXPT 'K 2)))
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'JACOBIDN 'U 'K)
                                                 2)
                                           'K
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBISD (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'MINUS
                         (LIST 'QUOTIENT
                               (LIST 'ASIN
                                     (LIST 'TIMES 'M (LIST 'JACOBICD 'U 'M)))
                               (LIST 'TIMES 'M
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))))))
             (LIST 'REPLACEBY (LIST 'JACOBISD (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBISD 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBISDRULES)) 
(SETK 'JACOBINDRULES
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'JACOBIND (LIST '~ 'U) 0) 1)
             (LIST 'REPLACEBY (LIST 'JACOBIND (LIST '~ 'U) 1) (LIST 'COSH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIND 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBIND 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBIND 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'PLUS 1 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'DIFFERENCE 1 'I)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'SQRT (LIST 'DIFFERENCE 1 'K))
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT (LIST 'PLUS 1 'K))))
                               (LIST 'TIMES 2
                                     (LIST 'SQRT
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBIND 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST
                                                                   ''JACOBIDN
                                                                   'ARG ''M)
                                                             (LIST 'LIST ''SQRT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         1
                                                                         (LIST
                                                                          'LIST
                                                                          ''EXPT
                                                                          ''M
                                                                          2)))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBIND
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES ''I
                                                             (LIST 'LIST
                                                                   ''JACOBISC
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIND (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DF 'U 'X) (LIST 'EXPT 'M 2)
                               (LIST 'JACOBISD 'U 'M) (LIST 'JACOBICD 'U 'M))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'M
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'JACOBIE
                                                                          'U
                                                                          'M)
                                                                         (LIST
                                                                          'JACOBISN
                                                                          'U
                                                                          'M)
                                                                         (LIST
                                                                          'JACOBICN
                                                                          'U
                                                                          'M))
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'EXPT
                                                                          (LIST
                                                                           'JACOBISN
                                                                           'U
                                                                           'M)
                                                                          2)
                                                                         (LIST
                                                                          'JACOBIDN
                                                                          'U
                                                                          'M)))
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'M
                                                                         2))))
                                                 (LIST 'TIMES 'M 'U
                                                       (LIST 'JACOBISN 'U 'M)
                                                       (LIST 'JACOBICN 'U 'M)))
                                           (LIST 'EXPT (LIST 'JACOBIDN 'U 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBIND (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'QUOTIENT (LIST 'ACOS (LIST 'JACOBICD 'U 'M))
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY (LIST 'JACOBIND (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIND 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBINDRULES)) 
(SETK 'JACOBIDCRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBIDC (LIST '~ 'U) 0) (LIST 'SEC 'U))
             (LIST 'REPLACEBY (LIST 'JACOBIDC (LIST '~ 'U) 1) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIDC 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBIDC 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBIDC 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'SQRT
                         (LIST 'PLUS 1
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'SQRT 'K))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'SQRT 'K)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'SQRT (LIST 'PLUS 1 'K))
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1 'K))))
                               (LIST 'SQRT 2))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBIDC
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''JACOBINS
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBIDC 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES ''M
                                                             (LIST 'LIST
                                                                   ''JACOBICD
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBIDN (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIDC (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))
                               (LIST 'JACOBISC 'U 'K) (LIST 'JACOBINC 'U 'K)
                               (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'JACOBISN 'U 'K)
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'JACOBIE 'U 'K)
                                                       (LIST 'TIMES 'U
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K
                                                                         2))))
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT
                                                             (LIST 'JACOBICN 'U
                                                                   'K)
                                                             2)
                                                       'K)))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBIDC (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'LOG
                         (LIST 'PLUS (LIST 'JACOBINC 'U 'M)
                               (LIST 'JACOBISC 'U 'M))))
             (LIST 'REPLACEBY (LIST 'JACOBIDC (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIDC 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBIDCRULES)) 
(SETK 'JACOBINCRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBINC (LIST '~ 'U) 0) (LIST 'SEC 'U))
             (LIST 'REPLACEBY (LIST 'JACOBINC (LIST '~ 'U) 1) (LIST 'COSH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBINC 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'JACOBINC 'U 'M))
             (LIST 'REPLACEBY (LIST 'JACOBINC 0 (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT
                         (LIST 'SQRT
                               (LIST 'PLUS 1
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'SQRT 'K)
                         (LIST 'SQRT (LIST 'PLUS 1 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'SQRT (LIST 'QUOTIENT 'K 2))
                         (LIST 'QUOTIENT (LIST 'PLUS 1 'I)
                               (LIST 'SQRT
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBINC
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES 'S
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         (LIST
                                                                          'LIST
                                                                          ''JACOBIDS
                                                                          'ARG
                                                                          ''M)
                                                                         (LIST
                                                                          'LIST
                                                                          ''SQRT
                                                                          (LIST
                                                                           'LIST
                                                                           ''DIFFERENCE
                                                                           1
                                                                           (LIST
                                                                            'LIST
                                                                            ''EXPT
                                                                            ''M
                                                                            2)))))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBINC
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES ''M
                                                             ''I 'S
                                                             (LIST 'LIST
                                                                   ''JACOBISD
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'JACOBICN (LIST 'QUOTIENT 'U 'D)
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBINC (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'JACOBISC 'U 'M)
                               (LIST 'JACOBIDC 'U 'M) (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES
                                                                   (LIST
                                                                    'JACOBIE 'U
                                                                    'M)
                                                                   (LIST
                                                                    'JACOBISN
                                                                    'U 'M)
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST
                                                                     'JACOBIDN
                                                                     'U 'M)
                                                                    'M))
                                                             (LIST 'TIMES 'M
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'JACOBISN
                                                                          'U
                                                                          'M)
                                                                         2)
                                                                   (LIST
                                                                    'JACOBICN
                                                                    'U 'M)))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'M
                                                                   2)))
                                                 (LIST 'TIMES 'U
                                                       (LIST 'JACOBISN 'U 'M)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'JACOBIDN 'U
                                                                   'M)
                                                             'M)))
                                           (LIST 'EXPT (LIST 'JACOBICN 'U 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBINC (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'PLUS (LIST 'JACOBIDC 'U 'M)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2)))
                                           (LIST 'JACOBISC 'U 'M))))
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY (LIST 'JACOBINC (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBINC 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBINCRULES)) 
(SETK 'JACOBISCRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBISC (LIST '~ 'U) 0) (LIST 'TAN 'U))
             (LIST 'REPLACEBY (LIST 'JACOBISC (LIST '~ 'U) 1) (LIST 'SINH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBISC 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBISC 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBISC 0 (LIST '~ 'M)) 0)
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 1
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT 'I (LIST 'SQRT (LIST 'PLUS 1 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'PLUS 1 'I)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'SQRT (LIST 'PLUS 1 'K))
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1 'K))))
                               (LIST 'TIMES 2
                                     (LIST 'SQRT
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBISC 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''JACOBICS
                                                                         'ARG
                                                                         ''M)
                                                                   (LIST 'LIST
                                                                         ''SQRT
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'LIST
                                                                           ''EXPT
                                                                           ''M
                                                                           2))))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBISC
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             ''I
                                                             (LIST 'LIST
                                                                   ''JACOBIND
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'TIMES 'I
                         (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D)
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBISC (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'JACOBIDC 'U 'K)
                               (LIST 'JACOBINC 'U 'K) (LIST 'DF 'U 'X))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIDN 'U 'K)
                                                       'U
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIDN 'U 'K)
                                                       (LIST 'JACOBIE 'U 'K)))
                                           (LIST 'TIMES (LIST 'JACOBISN 'U 'K)
                                                 (LIST 'JACOBICN 'U 'K)
                                                 (LIST 'EXPT 'K 2)))
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'JACOBICN 'U 'K)
                                                 2)
                                           'K
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBISC (LIST '~ 'U) (LIST '~ 'M)) 'U)
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'PLUS (LIST 'JACOBIDC 'U 'M)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2)))
                                           (LIST 'JACOBINC 'U 'M))))
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'M 2)))))
             (LIST 'REPLACEBY (LIST 'JACOBISC (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBISC 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBISCRULES)) 
(SETK 'JACOBINSRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBINS (LIST '~ 'U) 0) (LIST 'CSC 'U))
             (LIST 'REPLACEBY (LIST 'JACOBINS (LIST '~ 'U) 1) (LIST 'COTH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBINS 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBINS 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBINS 0 (LIST '~ 'M))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBISN 0 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'SQRT
                         (LIST 'PLUS 1
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'MINUS (LIST 'TIMES 'I (LIST 'SQRT 'K))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'SQRT (LIST 'TIMES 2 'K))
                         (LIST 'PLUS (LIST 'SQRT (LIST 'PLUS 1 'K))
                               (LIST 'TIMES 'I
                                     (LIST 'SQRT (LIST 'DIFFERENCE 1 'K))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBINS
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''JACOBIDC
                                                                   'ARG
                                                                   ''M)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBINS 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST
                                                                   ''JACOBISN
                                                                   'ARG ''M)
                                                             ''M))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D)
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBINS (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'X))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'JACOBIDS 'U 'M)
                                     (LIST 'JACOBICS 'U 'M) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'M 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES 'M
                                                             (LIST 'JACOBISN 'U
                                                                   'M)
                                                             (LIST 'EXPT
                                                                   (LIST
                                                                    'JACOBICN
                                                                    'U 'M)
                                                                   2))
                                                       (LIST 'TIMES
                                                             (LIST 'JACOBIE 'U
                                                                   'M)
                                                             (LIST 'JACOBICN 'U
                                                                   'M)
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'JACOBIDN
                                                                    'U 'M)
                                                                   'M)))
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'M 2)))
                                           (LIST 'TIMES 'U
                                                 (LIST 'JACOBICN 'U 'M)
                                                 (LIST 'QUOTIENT
                                                       (LIST 'JACOBIDN 'U 'M)
                                                       'M)))
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBINS (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'LOG
                         (LIST 'DIFFERENCE (LIST 'JACOBIDS 'U 'M)
                               (LIST 'JACOBICS 'U 'M))))
             (LIST 'REPLACEBY (LIST 'JACOBINS (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBINS 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBINSRULES)) 
(SETK 'JACOBIDSRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBIDS (LIST '~ 'U) 0) (LIST 'CSC 'U))
             (LIST 'REPLACEBY (LIST 'JACOBIDS (LIST '~ 'U) 1) (LIST 'CSCH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBIDS 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBIDS 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBIDS 0 (LIST '~ 'M))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBISD 0 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'TIMES
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
                         (LIST 'SQRT
                               (LIST 'PLUS 1
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'SQRT 'K)
                               (LIST 'SQRT (LIST 'PLUS 1 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'DIFFERENCE 1 'I)
                         (LIST 'SQRT (LIST 'QUOTIENT 'K 2))
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
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
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBIDS
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''JACOBINC
                                                                   'ARG ''M)
                                                             (LIST 'LIST ''SQRT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         1
                                                                         (LIST
                                                                          'LIST
                                                                          ''EXPT
                                                                          ''M
                                                                          2)))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
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
                                                       (LIST 'LIST ''ELLIPTICK
                                                             ''M)))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBIDS
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES 'S
                                                                   ''I ''M
                                                                   (LIST 'LIST
                                                                         ''JACOBICN
                                                                         'ARG
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D)
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBIDS (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'JACOBICS 'U 'K)
                                     (LIST 'JACOBINS 'U 'K) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBICN 'U 'K)
                                                       (LIST 'JACOBIE 'U 'K))
                                                 (LIST 'TIMES 'U
                                                       (LIST 'JACOBICN 'U 'K)
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES (LIST 'JACOBIDN 'U 'K)
                                                 (LIST 'JACOBISN 'U 'K)
                                                 (LIST 'EXPT 'K 2)))
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'JACOBISN 'U 'K)
                                                 2)
                                           'K
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBIDS (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'LOG
                         (LIST 'DIFFERENCE (LIST 'JACOBINS 'U 'M)
                               (LIST 'JACOBICS 'U 'M))))
             (LIST 'REPLACEBY (LIST 'JACOBIDS (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBIDS 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBIDSRULES)) 
(SETK 'JACOBICSRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBICS (LIST '~ 'U) 0) (LIST 'COT 'U))
             (LIST 'REPLACEBY (LIST 'JACOBICS (LIST '~ 'U) 1) (LIST 'CSCH 'U))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'M)))
                   (LIST 'JACOBICS 'U 'M))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'MINUS (LIST 'JACOBICS 'U 'M)))
             (LIST 'REPLACEBY (LIST 'JACOBICS 0 (LIST '~ 'M))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBISC 0 'M)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K)) 2)
                         (LIST '~ 'K))
                   (LIST 'SQRT
                         (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST '|ELLIPTICK'| (LIST '~ 'K)) 2))
                         (LIST '~ 'K))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'SQRT (LIST 'PLUS 1 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'PLUS
                               (LIST 'QUOTIENT (LIST 'ELLIPTICK (LIST '~ 'K))
                                     2)
                               (LIST 'TIMES 'I
                                     (LIST 'QUOTIENT
                                           (LIST '|ELLIPTICK'| (LIST '~ 'K))
                                           2)))
                         (LIST '~ 'K))
                   (LIST 'TIMES (LIST 'DIFFERENCE 1 'I)
                         (LIST 'SQRT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))))
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE (LIST 'SQRT (LIST 'PLUS 1 'K))
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1 'K))))
                               2)))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''JACOBICS 'ARG
                                                        ''M))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''SQRT
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'LIST
                                                                           ''EXPT
                                                                           ''M
                                                                           2)))
                                                                   (LIST 'LIST
                                                                         ''JACOBISC
                                                                         'ARG
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '|ELLIPTICK'| (LIST '~ 'M))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'SHIFT 'ARG 'R 'S)
                               (LIST 'SETQ 'SHIFT
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''IMPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'SHIFT 4)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
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
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'SHIFT)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST ''JACOBICS
                                                              'ARG ''M)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES 'S
                                                                   ''I
                                                                   (LIST 'LIST
                                                                         ''JACOBIDN
                                                                         'ARG
                                                                         ''M))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'IP)
                                     (LIST 'GEQ (LIST 'ABS 'IP) 1))
                               (LIST 'REPLACEBY 'IP
                                     (LIST 'IMPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT (LIST '~ (LIST '~ 'U))
                                     (LIST '~ (LIST '~ 'D))))
                         (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D)
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'M 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'JACOBICS (LIST '~ 'U) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'JACOBINS 'U 'K)
                                     (LIST 'JACOBIDS 'U 'K) (LIST 'DF 'U 'X)))
                         (LIST 'TIMES (LIST 'DF 'K 'X)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIDN 'U 'K)
                                                       'U
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))
                                                 (LIST 'TIMES
                                                       (LIST 'JACOBIDN 'U 'K)
                                                       (LIST 'JACOBIE 'U 'K)))
                                           (LIST 'TIMES (LIST 'JACOBISN 'U 'K)
                                                 (LIST 'JACOBICN 'U 'K)
                                                 (LIST 'EXPT 'K 2)))
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'JACOBISN 'U 'K)
                                                 2)
                                           'K
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'K 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'JACOBICS (LIST '~ 'U) (LIST '~ 'M))
                         (LIST '~ 'U))
                   (LIST 'LOG
                         (LIST 'DIFFERENCE (LIST 'JACOBINS 'U 'M)
                               (LIST 'JACOBIDS 'U 'M))))
             (LIST 'REPLACEBY (LIST 'JACOBICS (LIST '~ 'U) (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_JACOBICS 'U 'M)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'M))))))) 
(LET '(JACOBICSRULES)) 
(PUT 'K2NOME 'NUMBER-OF-ARGS 1) 
(FLAG '(K2NOME) 'OPFN) 
(PUT 'K2NOME 'DEFINED-ON-LINE '942) 
(PUT 'K2NOME 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'K2NOME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE K2NOME (K)
    (COND ((EVALEQUAL (AEVAL K) 0) 0)
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))) 1)
          (T (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI (LIST 'K2TAU K))))))) 
(PUT 'K2TAU 'NUMBER-OF-ARGS 1) 
(FLAG '(K2TAU) 'OPFN) 
(PUT 'K2TAU 'DEFINED-ON-LINE '949) 
(PUT 'K2TAU 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'K2TAU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE K2TAU (K)
    (COND
     ((OR (EVALEQUAL (AEVAL K) 0) (EVALEQUAL (AEVAL K) 1)
          (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (REDERR (REVALX "tau not defined when k=0, +1 or -1"))))
     (T
      (AEVAL
       (LIST 'TIMES 'I
             (LIST 'QUOTIENT (LIST '|ELLIPTICK'| K) (LIST 'ELLIPTICK K))))))) 
(PUT 'NUM_JACOBISN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBISN) 'OPFN) 
(PUT 'NUM_JACOBISN 'DEFINED-ON-LINE '956) 
(PUT 'NUM_JACOBISN 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBISN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBISN (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'SIN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'TANH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES T3
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA1 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                                 (LIST 'NUM1_THETA4 Y TAU)))))))))) 
(PUT 'NUM_JACOBINS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBINS) 'OPFN) 
(PUT 'NUM_JACOBINS 'DEFINED-ON-LINE '967) 
(PUT 'NUM_JACOBINS 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBINS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBINS (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'CSC X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'COTH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA4 Y TAU)
                           (LIST 'TIMES T3 (LIST 'NUM1_THETA1 Y TAU)))))))))) 
(PUT 'NUM_JACOBICN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBICN) 'OPFN) 
(PUT 'NUM_JACOBICN 'DEFINED-ON-LINE '978) 
(PUT 'NUM_JACOBICN 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBICN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBICN (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'COS X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'SECH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA2 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                                 (LIST 'NUM1_THETA4 Y TAU)))))))))) 
(PUT 'NUM_JACOBINC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBINC) 'OPFN) 
(PUT 'NUM_JACOBINC 'DEFINED-ON-LINE '989) 
(PUT 'NUM_JACOBINC 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBINC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBINC (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'SEC X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'COSH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA4 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                                 (LIST 'NUM1_THETA2 Y TAU)))))))))) 
(PUT 'NUM_JACOBIDN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIDN) 'OPFN) 
(PUT 'NUM_JACOBIDN 'DEFINED-ON-LINE '1000) 
(PUT 'NUM_JACOBIDN 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBIDN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIDN (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) 1)
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'SECH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA3 Y TAU)
                           (LIST 'TIMES T3 (LIST 'NUM1_THETA4 Y TAU)))))))))) 
(PUT 'NUM_JACOBIND 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIND) 'OPFN) 
(PUT 'NUM_JACOBIND 'DEFINED-ON-LINE '1011) 
(PUT 'NUM_JACOBIND 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIND (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) 1)
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'COSH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES T3
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA4 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                                 (LIST 'NUM1_THETA3 Y TAU)))))))))) 
(PUT 'NUM_JACOBISC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBISC) 'OPFN) 
(PUT 'NUM_JACOBISC 'DEFINED-ON-LINE '1022) 
(PUT 'NUM_JACOBISC 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBISC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBISC (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'TAN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'SINH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES T3
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA1 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                                 (LIST 'NUM1_THETA2 Y TAU)))))))))) 
(PUT 'NUM_JACOBICS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBICS) 'OPFN) 
(PUT 'NUM_JACOBICS 'DEFINED-ON-LINE '1033) 
(PUT 'NUM_JACOBICS 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBICS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBICS (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'COT X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'CSCH X)))
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA2 Y TAU)
                           (LIST 'TIMES T3 (LIST 'NUM1_THETA1 Y TAU)))))))))) 
(PUT 'NUM_JACOBISD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBISD) 'OPFN) 
(PUT 'NUM_JACOBISD 'DEFINED-ON-LINE '1044) 
(PUT 'NUM_JACOBISD 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBISD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBISD (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'SIN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'SINH X)))
          ((AND (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0)
                (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
                (EVALLESSP (AEVAL (LIST 'ABS K)) 1))
           (AEVAL (LIST 'NUM_JACOBISD1 X K)))
          (T
           (PROG (TAU T32 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T32 (AEVAL (LIST 'EXPT (LIST 'NUM1_THETA3 0 TAU) 2)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X T32)))
             (RETURN
              (AEVAL
               (LIST 'TIMES T32
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA1 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU)
                                 (LIST 'NUM1_THETA2 0 TAU)
                                 (LIST 'NUM1_THETA3 Y TAU)))))))))) 
(PUT 'NUM_JACOBIDS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIDS) 'OPFN) 
(PUT 'NUM_JACOBIDS 'DEFINED-ON-LINE '1057) 
(PUT 'NUM_JACOBIDS 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBIDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIDS (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'CSC X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'CSCH X)))
          (T
           (PROG (TAU T32 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T32 (AEVAL (LIST 'EXPT (LIST 'NUM1_THETA3 0 TAU) 2)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X T32)))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA4 0 TAU) (LIST 'NUM1_THETA2 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA3 Y TAU)
                           (LIST 'TIMES T32 (LIST 'NUM1_THETA1 Y TAU)))))))))) 
(PUT 'NUM_JACOBICD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBICD) 'OPFN) 
(PUT 'NUM_JACOBICD 'DEFINED-ON-LINE '1068) 
(PUT 'NUM_JACOBICD 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBICD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBICD (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'COS X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))) 1)
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES T3
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA2 Y TAU)
                           (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                                 (LIST 'NUM1_THETA3 Y TAU)))))))))) 
(PUT 'NUM_JACOBIDC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_JACOBIDC) 'OPFN) 
(PUT 'NUM_JACOBIDC 'DEFINED-ON-LINE '1079) 
(PUT 'NUM_JACOBIDC 'DEFINED-IN-FILE 'ELLIPFN/EFJACOBI.RED) 
(PUT 'NUM_JACOBIDC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_JACOBIDC (X K)
    (COND ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'SEC X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))) 1)
          (T
           (PROG (TAU T3 Y)
             (SETQ TAU (AEVAL (LIST 'K2TAU K)))
             (SETQ T3 (AEVAL (LIST 'NUM1_THETA3 0 TAU)))
             (SETQ Y (AEVAL (LIST 'QUOTIENT X (LIST 'EXPT T3 2))))
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'NUM1_THETA2 0 TAU)
                     (LIST 'QUOTIENT (LIST 'NUM1_THETA3 Y TAU)
                           (LIST 'TIMES T3 (LIST 'NUM1_THETA2 Y TAU)))))))))) 
(SETK 'TO_SN
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBICN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'JACOBISN 'X 'K) 2)))
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBIDN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'DIFFERENCE 1
                         (LIST 'TIMES (LIST 'EXPT 'K 2)
                               (LIST 'EXPT (LIST 'JACOBISN 'X 'K) 2))))))) 
(SETK 'TO_CN
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBISN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'JACOBICN 'X 'K) 2)))
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBIDN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'PLUS (LIST 'DIFFERENCE 1 (LIST 'EXPT 'K 2))
                         (LIST 'TIMES (LIST 'EXPT 'K 2)
                               (LIST 'EXPT (LIST 'JACOBICN 'X 'K) 2))))))) 
(SETK 'TO_DN
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBISN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE 1
                               (LIST 'EXPT (LIST 'JACOBIDN 'X 'K) 2))
                         (LIST 'EXPT 'K 2)))
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'JACOBICN (LIST '~ 'X) (LIST '~ 'K)) 2)
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'EXPT 'K 2) 1)
                               (LIST 'EXPT (LIST 'JACOBIDN 'X 'K) 2))
                         (LIST 'EXPT 'K 2)))))) 
(SETK 'NO_GLAISHER
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'JACOBINS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBISN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBINC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBICN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBIND (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT 1 (LIST 'JACOBIDN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBISC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBISN 'X 'K)
                         (LIST 'JACOBICN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBISD (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBISN 'X 'K)
                         (LIST 'JACOBIDN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBICS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBICN 'X 'K)
                         (LIST 'JACOBISN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBICD (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBICN 'X 'K)
                         (LIST 'JACOBIDN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBIDS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBIDN 'X 'K)
                         (LIST 'JACOBISN 'X 'K)))
             (LIST 'REPLACEBY (LIST 'JACOBIDC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'QUOTIENT (LIST 'JACOBIDN 'X 'K)
                         (LIST 'JACOBICN 'X 'K)))))) 
(SETK 'JACOBIADDITIONRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'U 'D)
                                           'M)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M) (LIST 'JACOBICN 'U 'M)
                         (LIST 'QUOTIENT (LIST 'JACOBIDN 'U 'M)
                               (LIST 'DIFFERENCE 1
                                     (LIST 'TIMES (LIST 'EXPT 'M 2)
                                           (LIST 'EXPT (LIST 'JACOBISN 'U 'M)
                                                 4))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 4)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDN (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICN (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISN
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 4)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'EXPT
                                           (LIST 'JACOBINS
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBINS
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2))
                               (LIST 'EXPT 'M 2))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 4)))
                         (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M)
                               (LIST 'JACOBICN 'U 'M) (LIST 'JACOBIDN 'U 'M))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'EXPT
                                           (LIST 'JACOBINC
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBINC
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISC
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISC
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 4)))
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'EXPT
                                           (LIST 'JACOBIND
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBIND
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISD
                                                 (LIST 'QUOTIENT 'U 'D) 'M)
                                           2)
                                     (LIST 'EXPT
                                           (LIST 'JACOBISD
                                                 (LIST 'QUOTIENT 'V 'D) 'M)
                                           2)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 4)))
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D)
                                           'M)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M) (LIST 'JACOBICN 'U 'M)
                         (LIST 'QUOTIENT (LIST 'JACOBIDN 'U 'M)
                               (LIST 'DIFFERENCE
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                                     (LIST 'TIMES
                                           (LIST 'EXPT (LIST 'JACOBISN 'U 'M)
                                                 2)
                                           (LIST 'EXPT (LIST 'JACOBIDN 'U 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D)
                                           'M)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M) (LIST 'JACOBICN 'U 'M)
                         (LIST 'QUOTIENT (LIST 'JACOBIDN 'U 'M)
                               (LIST 'DIFFERENCE
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                                     (LIST 'TIMES (LIST 'EXPT 'M 2)
                                           (LIST 'EXPT (LIST 'JACOBISN 'U 'M)
                                                 2)
                                           (LIST 'EXPT (LIST 'JACOBICN 'U 'M)
                                                 2))))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'JACOBISN 'U 'M)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1
                                                 (LIST 'JACOBIDN 'U 'M)))
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                                 (LIST 'JACOBICN 'U 'M)))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)))
                         (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M)
                               (LIST 'JACOBICN 'U 'M) (LIST 'JACOBIDN 'U 'M))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                           (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBINS (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICS (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDS (LIST 'QUOTIENT 'U 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)))
                         (LIST 'TIMES 2 (LIST 'JACOBISN 'U 'M)
                               (LIST 'JACOBICN 'U 'M) (LIST 'JACOBIDN 'U 'M))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M)))
                               (LIST 'QUOTIENT
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'JACOBIDN 'U 'M)
                                                 (LIST 'JACOBICN 'U 'M)))
                                     (LIST 'JACOBISN 'U 'M)))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIND (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISD (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBICD (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBICN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'V 'D)
                                           'M)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBINC (LIST 'QUOTIENT 'V 'D)
                                           'M))
                               (LIST 'TIMES
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISC (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBIDC (LIST 'QUOTIENT 'V 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'EXPT (LIST 'JACOBICN 'U 'M) 2)
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                                     (LIST 'EXPT (LIST 'JACOBIDN 'U 'M) 2)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC (LIST 'QUOTIENT (LIST '~ 'U) 2)
                         (LIST '~ 'M))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBIDN 'U 'M)))
                               (LIST 'SQRT
                                     (LIST 'PLUS 1 (LIST 'JACOBICN 'U 'M))))
                         (LIST 'NEQ 'U
                               (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'PLUS (LIST 'JACOBIE (LIST 'QUOTIENT 'U 'D) 'M)
                         (LIST 'DIFFERENCE
                               (LIST 'JACOBIE (LIST 'QUOTIENT 'V 'D) 'M)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBISN
                                           (LIST 'QUOTIENT (LIST 'PLUS 'U 'V)
                                                 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIE (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'JACOBIE 'U 'M))
                         (LIST 'TIMES (LIST 'EXPT 'M 2)
                               (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                               (LIST 'JACOBISN (LIST 'TIMES 2 'U) 'M))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'U) (LIST '~ 'V))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'M))
                   (LIST 'PLUS (LIST 'JACOBIZETA (LIST 'QUOTIENT 'U 'D) 'M)
                         (LIST 'DIFFERENCE
                               (LIST 'JACOBIZETA (LIST 'QUOTIENT 'V 'D) 'M)
                               (LIST 'TIMES (LIST 'EXPT 'M 2)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'U 'D) 'M)
                                     (LIST 'JACOBISN (LIST 'QUOTIENT 'V 'D) 'M)
                                     (LIST 'JACOBISN
                                           (LIST 'QUOTIENT (LIST 'PLUS 'U 'V)
                                                 'D)
                                           'M)))))
             (LIST 'REPLACEBY
                   (LIST 'JACOBIZETA (LIST 'TIMES 2 (LIST '~ 'U)) (LIST '~ 'M))
                   (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'JACOBIZETA 'U 'M))
                         (LIST 'TIMES (LIST 'EXPT 'M 2)
                               (LIST 'EXPT (LIST 'JACOBISN 'U 'M) 2)
                               (LIST 'JACOBISN (LIST 'TIMES 2 'U) 'M))))))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'JACOBISN 'FANCY-FUNCTIONSYMBOL "\\mathrm{sn}") 
(PUT 'JACOBICN 'FANCY-FUNCTIONSYMBOL "\\mathrm{cn}") 
(PUT 'JACOBIDN 'FANCY-FUNCTIONSYMBOL "\\mathrm{dn}") 
(PUT 'JACOBINS 'FANCY-FUNCTIONSYMBOL "\\mathrm{ns}") 
(PUT 'JACOBINC 'FANCY-FUNCTIONSYMBOL "\\mathrm{nc}") 
(PUT 'JACOBIND 'FANCY-FUNCTIONSYMBOL "\\mathrm{nd}") 
(PUT 'JACOBISC 'FANCY-FUNCTIONSYMBOL "\\mathrm{sc}") 
(PUT 'JACOBISD 'FANCY-FUNCTIONSYMBOL "\\mathrm{sd}") 
(PUT 'JACOBICD 'FANCY-FUNCTIONSYMBOL "\\mathrm{cd}") 
(PUT 'JACOBICS 'FANCY-FUNCTIONSYMBOL "\\mathrm{cs}") 
(PUT 'JACOBIDS 'FANCY-FUNCTIONSYMBOL "\\mathrm{ds}") 
(PUT 'JACOBIDC 'FANCY-FUNCTIONSYMBOL "\\mathrm{dc}") 
(PUT 'JACOBIAM 'FANCY-FUNCTIONSYMBOL "\\mathrm{am}") 
(PUT 'JACOBIZETA 'FANCY-FUNCTIONSYMBOL "\\mathrm{Z}") 
(PUT 'JACOBIE 'FANCY-FUNCTIONSYMBOL "\\mathcal{E}") 
(PUT 'JACOBISN 'PLAIN-FUNCTIONSYMBOL 'SN) 
(PUT 'JACOBICN 'PLAIN-FUNCTIONSYMBOL 'CN) 
(PUT 'JACOBIDN 'PLAIN-FUNCTIONSYMBOL 'DN) 
(PUT 'JACOBINS 'PLAIN-FUNCTIONSYMBOL 'NS) 
(PUT 'JACOBINC 'PLAIN-FUNCTIONSYMBOL 'NC) 
(PUT 'JACOBIND 'PLAIN-FUNCTIONSYMBOL 'ND) 
(PUT 'JACOBISC 'PLAIN-FUNCTIONSYMBOL 'SC) 
(PUT 'JACOBISD 'PLAIN-FUNCTIONSYMBOL 'SD) 
(PUT 'JACOBICD 'PLAIN-FUNCTIONSYMBOL 'CD) 
(PUT 'JACOBICS 'PLAIN-FUNCTIONSYMBOL 'CS) 
(PUT 'JACOBIDS 'PLAIN-FUNCTIONSYMBOL 'DS) 
(PUT 'JACOBIDC 'PLAIN-FUNCTIONSYMBOL 'DC) 
(PUT 'JACOBIAM 'PLAIN-FUNCTIONSYMBOL 'AM) 
(PUT 'JACOBIZETA 'PLAIN-FUNCTIONSYMBOL '|z|) 
(PUT 'JACOBIE 'PLAIN-FUNCTIONSYMBOL "E_j") 
(PUT 'ELLIPTICE 'PLAIN-FUNCTIONSYMBOL '|e|) 
(PUT 'ELLIPTICD 'PLAIN-FUNCTIONSYMBOL '|d|) 
(PUT 'ELLIPTICK 'PLAIN-FUNCTIONSYMBOL '|k|) 
(PUT 'ELLIPTICF 'PLAIN-FUNCTIONSYMBOL '|f|) 
(PUT '|ELLIPTICK'| 'PLAIN-FUNCTIONSYMBOL '|k'|) 
(PUT '|ELLIPTICE'| 'PLAIN-FUNCTIONSYMBOL '|e'|) 
(PUT 'ELLIPTICPI 'PLAIN-FUNCTIONSYMBOL '|pI|) 
(PUT 'ELLIPTICE 'FANCY-FUNCTIONSYMBOL "\\mathrm{E}") 
(PUT 'ELLIPTICD 'FANCY-FUNCTIONSYMBOL "\\mathrm{D}") 
(PUT 'ELLIPTICK 'FANCY-FUNCTIONSYMBOL "\\mathrm{K}") 
(PUT 'ELLIPTICF 'FANCY-FUNCTIONSYMBOL "\\mathrm{F}") 
(PUT '|ELLIPTICK'| 'FANCY-FUNCTIONSYMBOL "\\mathrm{K}^\\prime") 
(PUT '|ELLIPTICE'| 'FANCY-FUNCTIONSYMBOL "\\mathrm{E}^\\prime") 
(PUT 'ELLIPTICPI 'FANCY-FUNCTIONSYMBOL "\\Pi") 
(PUT '|ELLIPTICE'| 'FANCY-SYMBOL-LENGTH 4) 
(PUT '|ELLIPTICK'| 'FANCY-SYMBOL-LENGTH 4) 
(PROG (X)
  (SETQ X
          '(JACOBISN JACOBICN JACOBIDN JACOBINS JACOBINC JACOBIND JACOBISC
            JACOBISD JACOBICD JACOBICS JACOBIDS JACOBIDC JACOBIAM))
 LAB
  (COND ((NULL X) (RETURN NIL)))
  ((LAMBDA (X)
     (PROGN (PUT X 'FANCY-SYMBOL-LENGTH 4) (PUT X 'PRIFN 'PLAIN-SYMBOL)))
   (CAR X))
  (SETQ X (CDR X))
  (GO LAB)) 
(PUT 'ELLIPTICE 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICD 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICK 'PRIFN 'PLAIN-SYMBOL) 
(PUT '|ELLIPTICK'| 'PRIFN 'PLAIN-SYMBOL) 
(PUT '|ELLIPTICE'| 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICF 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICPI 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'JACOBIZETA 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'JACOBIE 'PRIFN 'PLAIN-SYMBOL) 
(FLAG
 '(JACOBISN JACOBICN JACOBIDN JACOBINS JACOBINC JACOBIND JACOBISC JACOBISD
   JACOBICS JACOBICD JACOBIDS JACOBIDC JACOBIAM JACOBIZETA JACOBIE ELLIPTICK
   |ELLIPTICK'| ELLIPTICF ELLIPTICE |ELLIPTICE'| ELLIPTICD)
 'SPECFN) 
(DEFLIST
 '((JACOBISN 2) (JACOBICN 2) (JACOBIDN 2) (JACOBINS 2) (JACOBINC 2)
   (JACOBIND 2) (JACOBISC 2) (JACOBISD 2) (JACOBICS 2) (JACOBICD 2)
   (JACOBIDS 2) (JACOBIDC 2) (JACOBIAM 2) (ELLIPTICF 2) (JACOBIZETA 2)
   (JACOBIE 2) (ELLIPTICK 1) (|ELLIPTICK'| 1) (ELLIPTICE (1 2))
   (|ELLIPTICE'| 1) (ELLIPTICD (1 2)) (ELLIPTICPI (2 3)))
 'NUMBER-OF-ARGS) 
(ENDMODULE) 