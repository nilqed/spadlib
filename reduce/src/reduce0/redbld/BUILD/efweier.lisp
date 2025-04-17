(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFWEIER)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'WEIERSTRASS 'WEIERSTRASSZETA)) 
(OPERATOR (LIST 'WEIERSTRASS_SIGMA 'WEIERSTRASS_SIGMA1)) 
(OPERATOR (LIST 'WEIERSTRASS_SIGMA2 'WEIERSTRASS_SIGMA3)) 
(OPERATOR (LIST 'ETA1 'ETA2 'ETA3)) 
(OPERATOR (LIST 'LATTICE_E1 'LATTICE_E2 'LATTICE_E3)) 
(OPERATOR (LIST 'LATTICE_G2 'LATTICE_G3 'LATTICE_DELTA 'LATTICE_G)) 
(OPERATOR (LIST 'LATTICE_OMEGA1 'LATTICE_OMEGA3)) 
(SETK 'SIGMA_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA 0 (LIST '~ 'W1) (LIST '~ 'W3)) 0)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1 (LIST '~ 'W1) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   0)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2
                         (LIST 'PLUS (LIST '~ 'W1) (LIST '~ 'W3)) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   0)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3 (LIST '~ 'W3) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   0)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'MINUS (LIST 'WEIERSTRASS_SIGMA 'U 'W1 'W3)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WEIERSTRASS_SIGMA1 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WEIERSTRASS_SIGMA2 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WEIERSTRASS_SIGMA3 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA (LIST '~ 'U)
                         (LIST 'MINUS (LIST '~ 'W1))
                         (LIST 'MINUS (LIST '~ 'W3)))
                   (LIST 'WEIERSTRASS_SIGMA 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1 (LIST '~ 'U)
                         (LIST 'MINUS (LIST '~ 'W1))
                         (LIST 'MINUS (LIST '~ 'W3)))
                   (LIST 'WEIERSTRASS_SIGMA1 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2 (LIST '~ 'U)
                         (LIST 'MINUS (LIST '~ 'W1))
                         (LIST 'MINUS (LIST '~ 'W3)))
                   (LIST 'WEIERSTRASS_SIGMA2 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3 (LIST '~ 'U)
                         (LIST 'MINUS (LIST '~ 'W1))
                         (LIST 'MINUS (LIST '~ 'W3)))
                   (LIST 'WEIERSTRASS_SIGMA3 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1 0 (LIST '~ 'W1) (LIST '~ 'W3)) 1)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2 0 (LIST '~ 'W1) (LIST '~ 'W3)) 1)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3 0 (LIST '~ 'W1) (LIST '~ 'W3)) 1)
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN
                         (LIST 'TIMES 'M (LIST 'WEIERSTRASS_SIGMA 'U 'W1 'W3))
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN (LIST 'WEIERSTRASS_SIGMA1 'U 'W1 'W3)
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN (LIST 'WEIERSTRASS_SIGMA2 'U 'W1 'W3)
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN (LIST 'WEIERSTRASS_SIGMA3 'U 'W1 'W3)
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXPT
                                                       (LIST 'MINUS 1) 'M)
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA1
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W1))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA 'ARG
                                                       ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXPT
                                                       (LIST 'MINUS 1) 'M)
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA3
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W3))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA 'ARG
                                                       ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXPT
                                                       (LIST 'MINUS 1) 'M)
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA1
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W1))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA1
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA3
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W3))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA1
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA1
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W1))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA2
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA3
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W3))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA2
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA1
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W1))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA3
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXPT
                                                       (LIST 'MINUS 1) 'M)
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES 2 'M
                                                             (LIST 'LIST ''ETA3
                                                                   ''W1 ''W3)
                                                             (LIST 'LIST ''PLUS
                                                                   'ARG
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         'M
                                                                         ''W3))))
                                                 (LIST 'LIST
                                                       ''WEIERSTRASS_SIGMA3
                                                       'ARG ''W1 ''W3)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WEIERSTRASS_SIGMA (LIST '~ 'U) (LIST '~ 'W1)
                               (LIST '~ 'W3))
                         (LIST '~ 'U))
                   (LIST 'TIMES (LIST 'WEIERSTRASS_SIGMA 'U 'W1 'W3)
                         (LIST 'WEIERSTRASSZETA 'U 'W1 'W3)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA (LIST '~ 'U) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_SIGMA 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA1 (LIST '~ 'U) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_SIGMA1 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA2 (LIST '~ 'U) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_SIGMA2 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS_SIGMA3 (LIST '~ 'U) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_SIGMA3 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))))) 
(LET '(SIGMA_RULES)) 
(SETK 'WEIERSTRASS_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WEIERSTRASS 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST '~ 'U) (LIST 'MINUS (LIST '~ 'W3))
                         (LIST '~ 'W1))
                   (LIST 'WEIERSTRASS 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'WEIERSTRASS 'U 'W1 'W3)
                               (LIST 'EXPT 'M 2))
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST '~ 'W1) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'LATTICE_E1 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST '~ 'W3) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'LATTICE_E3 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST 'PLUS (LIST '~ 'W1) (LIST '~ 'W3))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'LATTICE_E2 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WEIERSTRASS (LIST '~ 'U) (LIST '~ 'W1)
                               (LIST '~ 'W3))
                         'U)
                   (LIST 'MINUS
                         (LIST 'SQRT
                               (LIST 'DIFFERENCE
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 4
                                                 (LIST 'EXPT
                                                       (LIST 'WEIERSTRASS 'U
                                                             'W1 'W3)
                                                       3))
                                           (LIST 'TIMES
                                                 (LIST 'LATTICE_G2 'W1 'W3)
                                                 (LIST 'WEIERSTRASS 'U 'W1
                                                       'W3)))
                                     (LIST 'LATTICE_G3 'W1 'W2)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''WEIERSTRASS 'ARG ''W1
                                                 ''W3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''WEIERSTRASS 'ARG ''W1
                                                 ''W3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS (LIST '~ 'U) (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_WEIER 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_E1 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_E1 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_E2 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_E2 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_E3 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_E3 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_G2 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_G2 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_G3 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_G3 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_DELTA (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_DELTA 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'LATTICE_G (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE '|NUM_g| 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))))) 
(LET '(WEIERSTRASS_RULES)) 
(SETK 'WEIERZETA_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'MINUS (LIST 'WEIERSTRASSZETA 'U 'W1 'W3)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W1))
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'W3)))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'WEIERSTRASSZETA 'U 'W1 'W3) 'M)
                         (LIST 'AND (LIST 'NUMBERP (LIST 'REPART 'M))
                               (LIST 'NUMBERP (LIST 'IMPART 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA (LIST '~ 'U)
                         (LIST 'MINUS (LIST '~ 'W1)) (LIST '~ 'W3))
                   (LIST 'WEIERSTRASSZETA 'U 'W3 'W1))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA (LIST '~ 'W1) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'ETA1 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA (LIST '~ 'W3) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'ETA3 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA
                         (LIST 'PLUS (LIST '~ 'W1) (LIST '~ 'W3)) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'PLUS (LIST 'ETA1 'W1 'W3) (LIST 'ETA3 'W1 'W3)))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WEIERSTRASSZETA (LIST '~ 'U) (LIST '~ 'W1)
                               (LIST '~ 'W3))
                         (LIST '~ 'U))
                   (LIST 'MINUS (LIST 'WEIERSTRASS 'U 'W1 'W3)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''WEIERSTRASSZETA
                                                       'ARG ''W1 ''W3)
                                                 (LIST 'LIST ''TIMES 2 'M
                                                       (LIST 'LIST ''ETA1 ''W1
                                                             ''W3))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''WEIERSTRASSZETA
                                                       'ARG ''W1 ''W3)
                                                 (LIST 'LIST ''TIMES 2 'M
                                                       (LIST 'LIST ''ETA3 ''W1
                                                             ''W3))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA (LIST '~ 'U) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_SIGMA 'NUM_WEIERZETA 'U 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'W1) (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'ETA1 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_ETA1 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'ETA2 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_ETA2 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))
             (LIST 'REPLACEBY (LIST 'ETA3 (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN (LIST 'N_LATTICE 'NUM_ETA3 'W1 'W3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'W1)
                               (LIST 'NUMBERP 'W3))))))) 
(LET '(WEIERZETA_RULES)) 
(OPERATOR (LIST 'WEIERSTRASS1 'WEIERSTRASSZETA1)) 
(SETK 'WEIERSTRASS1_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS1 (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'G2)
                         (LIST '~ 'G3))
                   (LIST 'WEIERSTRASS1 'U 'G2 'G3))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA1 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'MINUS (LIST 'WEIERSTRASSZETA1 'U 'G2 'G3)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS1 (LIST 'TIMES 'I (LIST '~ 'U))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'MINUS (LIST 'WEIERSTRASS1 'U 'G2 (LIST 'MINUS 'G3))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA1 (LIST 'TIMES 'I (LIST '~ 'U))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'WEIERSTRASSZETA1 'U 'G2
                                     (LIST 'MINUS 'G3)))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS1 (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'WEIERSTRASS1 'U
                                     (LIST 'TIMES 'G2 (LIST 'EXPT 'M 4))
                                     (LIST 'TIMES 'G3 (LIST 'EXPT 'M 6)))
                               (LIST 'EXPT 'M 2))
                         (LIST 'NUMBERP 'M)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA1
                         (LIST 'TIMES (LIST '~ 'M) (LIST '~ 'U)) (LIST '~ 'G2)
                         (LIST '~ 'G3))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'WEIERSTRASSZETA1 'U
                                     (LIST 'TIMES 'G2 (LIST 'EXPT 'M 4))
                                     (LIST 'TIMES 'G3 (LIST 'EXPT 'M 6)))
                               'M)
                         (LIST 'NUMBERP 'M)))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASS1 (LIST '~ 'U) (LIST '~ 'G2)
                         (LIST '~ 'G3))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_WEIER1 'U 'G2 'G3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'G2) (LIST 'NUMBERP 'G3))))
             (LIST 'REPLACEBY
                   (LIST 'WEIERSTRASSZETA1 (LIST '~ 'U) (LIST '~ 'G2)
                         (LIST '~ 'G3))
                   (LIST 'WHEN (LIST 'NUM_ELLIPTIC 'NUM_WEIERZETA1 'U 'G2 'G3)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'G2) (LIST 'NUMBERP 'G3))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WEIERSTRASS1 (LIST '~ 'U) (LIST '~ 'G2)
                               (LIST '~ 'G3))
                         'U)
                   (LIST 'MINUS
                         (LIST 'SQRT
                               (LIST 'DIFFERENCE
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 4
                                                 (LIST 'EXPT
                                                       (LIST 'WEIERSTRASS1 'U
                                                             'G2 'G3)
                                                       3))
                                           (LIST 'TIMES 'G2
                                                 (LIST 'WEIERSTRASS1 'U 'G2
                                                       'G3)))
                                     'G3))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WEIERSTRASSZETA1 (LIST '~ 'U) (LIST '~ 'G2)
                               (LIST '~ 'G3))
                         (LIST '~ 'U))
                   (LIST 'MINUS (LIST 'WEIERSTRASS1 'U 'G2 'G3)))))) 
(LET '(WEIERSTRASS1_RULES)) 
(PUT 'N_SIGMA 'NUMBER-OF-ARGS 4) 
(FLAG '(N_SIGMA) 'OPFN) 
(PUT 'N_SIGMA 'DEFINED-ON-LINE '327) 
(PUT 'N_SIGMA 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'N_SIGMA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_SIGMA (F U W1 W3)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'QUOTIENT W3 W1))) 0)
      (AEVAL
       (REDERR
        (REVALX
         "Ratio of the period parameters of Weierstrass functions must be complex"))))
     (T (AEVAL (LIST 'NUM_ELLIPTIC F U W1 W3))))) 
(PUT 'N_LATTICE 'NUMBER-OF-ARGS 3) 
(FLAG '(N_LATTICE) 'OPFN) 
(PUT 'N_LATTICE 'DEFINED-ON-LINE '332) 
(PUT 'N_LATTICE 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'N_LATTICE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_LATTICE (F W1 W3)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'QUOTIENT W3 W1))) 0)
      (AEVAL
       (REDERR
        (REVALX
         "Ratio of the period parameters of the lattice must be complex"))))
     (T (AEVAL (LIST 'NUM_ELLIPTIC F W1 W3))))) 
(PUT 'NUM_WEIER 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_WEIER) 'OPFN) 
(PUT 'NUM_WEIER 'DEFINED-ON-LINE '337) 
(PUT 'NUM_WEIER 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_WEIER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_WEIER (U W1 W3)
    (PROG (Q TAU T2 T4 E2 L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ W3 (AEVAL (LIST 'SECOND L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ L (AEVAL (LIST 'FIX_ARG U W1 TAU)))
      (SETQ U (AEVAL (LIST 'FIRST L)))
      (COND
       ((EVALEQUAL (AEVAL U) 0)
        (AEVAL (REDERR (REVALX "Weierstrass has poles at lattice points")))))
      (SETK 'N3 (AEVAL (LIST 'SECOND L)))
      (SETK 'N1 (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ T2 (AEVAL (LIST 'N_THETA2 0 Q TAU)))
      (SETQ T4 (AEVAL (LIST 'N_THETA4 0 Q)))
      (SETQ E2
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT 'PI 2)
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE (LIST 'EXPT T2 4)
                                 (LIST 'EXPT T4 4))
                           (LIST 'TIMES 12 (LIST 'EXPT W1 2))))))
      (SETQ U (AEVAL (LIST 'TIMES 'PI (LIST 'QUOTIENT U (LIST 'TIMES 2 W1)))))
      (RETURN
       (AEVAL
        (LIST 'PLUS E2
              (LIST 'EXPT
                    (LIST 'TIMES 'PI T2 T4
                          (LIST 'QUOTIENT (LIST 'N_THETA3 U Q)
                                (LIST 'TIMES 2 W1 (LIST 'N_THETA1 U Q TAU))))
                    2)))))) 
(PUT 'NUM_E1 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_E1) 'OPFN) 
(PUT 'NUM_E1 'DEFINED-ON-LINE '357) 
(PUT 'NUM_E1 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_E1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_E1 (W1 W3)
    (PROG (Q TAU L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (RETURN
       (AEVAL
        (LIST 'TIMES (LIST 'EXPT (LIST 'QUOTIENT 'PI W1) 2)
              (LIST 'QUOTIENT
                    (LIST 'PLUS (LIST 'EXPT (LIST 'N_THETA2 0 Q TAU) 4)
                          (LIST 'TIMES 2 (LIST 'EXPT (LIST 'N_THETA4 0 Q) 4)))
                    12)))))) 
(PUT 'NUM_E2 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_E2) 'OPFN) 
(PUT 'NUM_E2 'DEFINED-ON-LINE '365) 
(PUT 'NUM_E2 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_E2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_E2 (W1 W3)
    (PROG (Q TAU L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (RETURN
       (AEVAL
        (LIST 'TIMES (LIST 'EXPT (LIST 'QUOTIENT 'PI W1) 2)
              (LIST 'QUOTIENT
                    (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'N_THETA2 0 Q TAU) 4)
                          (LIST 'EXPT (LIST 'N_THETA4 0 Q) 4))
                    12)))))) 
(PUT 'NUM_E3 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_E3) 'OPFN) 
(PUT 'NUM_E3 'DEFINED-ON-LINE '373) 
(PUT 'NUM_E3 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_E3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_E3 (W1 W3)
    (PROG (Q TAU L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (RETURN
       (AEVAL
        (LIST 'MINUS
              (LIST 'TIMES (LIST 'EXPT (LIST 'QUOTIENT 'PI W1) 2)
                    (LIST 'QUOTIENT
                          (LIST 'PLUS
                                (LIST 'TIMES 2
                                      (LIST 'EXPT (LIST 'N_THETA2 0 Q TAU) 4))
                                (LIST 'EXPT (LIST 'N_THETA4 0 Q) 4))
                          12))))))) 
(PUT 'LATTICE_ROOTS 'NUMBER-OF-ARGS 2) 
(FLAG '(LATTICE_ROOTS) 'OPFN) 
(PUT 'LATTICE_ROOTS 'DEFINED-ON-LINE '381) 
(PUT 'LATTICE_ROOTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'LATTICE_ROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LATTICE_ROOTS (W1 W3)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'QUOTIENT W3 W1))) 0)
      (AEVAL
       (REDERR
        (REVALX
         "Ratio of the period parameters of the lattice must be complex"))))
     (T
      (PROG (RES OLDPREC)
        (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
        (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 3) 15)))
        (SETQ RES (AEVAL (LIST 'L_ROOTS W1 W3)))
        (AEVAL (LIST 'PRECISION OLDPREC))
        (RETURN (AEVAL RES)))))) 
(PUT 'L_ROOTS 'NUMBER-OF-ARGS 2) 
(FLAG '(L_ROOTS) 'OPFN) 
(PUT 'L_ROOTS 'DEFINED-ON-LINE '392) 
(PUT 'L_ROOTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'L_ROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE L_ROOTS (W1 W3)
    (PROG (Q TAU T2 T4 M L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ T2 (AEVAL (LIST 'EXPT (LIST 'N_THETA2 0 Q TAU) 4)))
      (SETQ T4 (AEVAL (LIST 'EXPT (LIST 'N_THETA4 0 Q) 4)))
      (SETQ M
              (AEVAL
               (LIST 'QUOTIENT (LIST 'EXPT (LIST 'QUOTIENT 'PI W1) 2) 12)))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'TIMES M (LIST 'PLUS T2 (LIST 'TIMES 2 T4)))
              (LIST 'TIMES M (LIST 'DIFFERENCE T2 T4))
              (LIST 'MINUS
                    (LIST 'TIMES M (LIST 'PLUS (LIST 'TIMES 2 T2) T4)))))))) 
(PUT 'LATTICE_INVARIANTS 'NUMBER-OF-ARGS 2) 
(FLAG '(LATTICE_INVARIANTS) 'OPFN) 
(PUT 'LATTICE_INVARIANTS 'DEFINED-ON-LINE '403) 
(PUT 'LATTICE_INVARIANTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'LATTICE_INVARIANTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LATTICE_INVARIANTS (W1 W3)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'QUOTIENT W3 W1))) 0)
      (AEVAL
       (REDERR
        (REVALX
         "Ratio of the period parameters of the lattice must be complex"))))
     (T
      (PROG (RES OLDPREC)
        (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
        (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 3) 15)))
        (SETQ RES (AEVAL (LIST 'L_INVARIANTS W1 W3)))
        (AEVAL (LIST 'PRECISION OLDPREC))
        (RETURN (AEVAL RES)))))) 
(PUT 'L_INVARIANTS 'NUMBER-OF-ARGS 2) 
(FLAG '(L_INVARIANTS) 'OPFN) 
(PUT 'L_INVARIANTS 'DEFINED-ON-LINE '414) 
(PUT 'L_INVARIANTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'L_INVARIANTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE L_INVARIANTS (W1 W3)
    (PROG (L G2 G3 E1 E2 E3)
      (SETQ L (AEVAL (LIST 'L_ROOTS W1 W3)))
      (SETQ E1 (AEVAL (LIST 'FIRST L)))
      (SETQ E2 (AEVAL (LIST 'SECOND L)))
      (SETQ E3 (AEVAL (LIST 'THIRD L)))
      (SETQ G2
              (AEVAL
               (LIST 'TIMES 2
                     (LIST 'PLUS (LIST 'EXPT E1 2) (LIST 'EXPT E2 2)
                           (LIST 'EXPT E3 2)))))
      (SETQ G3 (AEVAL (LIST 'TIMES 4 E1 E2 E3)))
      (RETURN
       (AEVAL
        (LIST 'LIST G2 G3
              (LIST 'DIFFERENCE (LIST 'EXPT G2 3)
                    (LIST 'TIMES 27 (LIST 'EXPT G3 2)))
              (COND ((EVALEQUAL (AEVAL G3) 0) (AEVAL 'INFINITY))
                    (T
                     (AEVAL
                      (LIST 'QUOTIENT (LIST 'EXPT G2 3)
                            (LIST 'TIMES 27 (LIST 'EXPT G3 2))))))))))) 
(PUT 'NUM_G2 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_G2) 'OPFN) 
(PUT 'NUM_G2 'DEFINED-ON-LINE '426) 
(PUT 'NUM_G2 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_G2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_G2 (W1 W3)
    (PROG (L E1 E2 E3)
      (SETQ L (AEVAL (LIST 'L_ROOTS W1 W3)))
      (SETQ E1 (AEVAL (LIST 'FIRST L)))
      (SETQ E2 (AEVAL (LIST 'SECOND L)))
      (SETQ E3 (AEVAL (LIST 'THIRD L)))
      (RETURN
       (AEVAL
        (LIST 'TIMES 2
              (LIST 'PLUS (LIST 'EXPT E1 2) (LIST 'EXPT E2 2)
                    (LIST 'EXPT E3 2))))))) 
(PUT 'NUM_G3 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_G3) 'OPFN) 
(PUT 'NUM_G3 'DEFINED-ON-LINE '435) 
(PUT 'NUM_G3 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_G3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_G3 (W1 W3)
    (PROG (L E1 E2 E3)
      (SETQ L (AEVAL (LIST 'L_ROOTS W1 W3)))
      (SETQ E1 (AEVAL (LIST 'FIRST L)))
      (SETQ E2 (AEVAL (LIST 'SECOND L)))
      (SETQ E3 (AEVAL (LIST 'THIRD L)))
      (SETK 'G2
            (AEVAL
             (LIST 'TIMES 2
                   (LIST 'PLUS (LIST 'EXPT E1 2) (LIST 'EXPT E2 2)
                         (LIST 'EXPT E3 2)))))
      (RETURN (AEVAL (LIST 'TIMES 4 E1 E2 E3))))) 
(PUT 'NUM_DELTA 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_DELTA) 'OPFN) 
(PUT 'NUM_DELTA 'DEFINED-ON-LINE '445) 
(PUT 'NUM_DELTA 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_DELTA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_DELTA (W1 W3)
    (PROG (L G2 G3 E1 E2 E3)
      (SETQ L (AEVAL (LIST 'L_ROOTS W1 W3)))
      (SETQ E1 (AEVAL (LIST 'FIRST L)))
      (SETQ E2 (AEVAL (LIST 'SECOND L)))
      (SETQ E3 (AEVAL (LIST 'THIRD L)))
      (SETQ G2
              (AEVAL
               (LIST 'TIMES 2
                     (LIST 'PLUS (LIST 'EXPT E1 2) (LIST 'EXPT E2 2)
                           (LIST 'EXPT E3 2)))))
      (SETQ G3 (AEVAL (LIST 'TIMES 4 E1 E2 E3)))
      (RETURN
       (AEVAL
        (LIST 'DIFFERENCE (LIST 'EXPT G2 3)
              (LIST 'TIMES 27 (LIST 'EXPT G3 2))))))) 
(PUT '|NUM_g| 'NUMBER-OF-ARGS 2) 
(FLAG '(|NUM_g|) 'OPFN) 
(PUT '|NUM_g| 'DEFINED-ON-LINE '456) 
(PUT '|NUM_g| 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT '|NUM_g| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |NUM_g| (W1 W3)
    (PROG (L G2 G3 E1 E2 E3)
      (SETQ L (AEVAL (LIST 'L_ROOTS W1 W3)))
      (SETQ E1 (AEVAL (LIST 'FIRST L)))
      (SETQ E2 (AEVAL (LIST 'SECOND L)))
      (SETQ E3 (AEVAL (LIST 'THIRD L)))
      (SETQ G2
              (AEVAL
               (LIST 'TIMES 2
                     (LIST 'PLUS (LIST 'EXPT E1 2) (LIST 'EXPT E2 2)
                           (LIST 'EXPT E3 2)))))
      (SETQ G3 (AEVAL (LIST 'TIMES 4 E1 E2 E3)))
      (RETURN
       (COND ((EVALEQUAL (AEVAL G3) 0) (AEVAL 'INFINITY))
             (T
              (AEVAL
               (LIST 'QUOTIENT (LIST 'EXPT G2 3)
                     (LIST 'TIMES 27 (LIST 'EXPT G3 2))))))))) 
(PUT 'NUM_WEIERZETA 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_WEIERZETA) 'OPFN) 
(PUT 'NUM_WEIERZETA 'DEFINED-ON-LINE '467) 
(PUT 'NUM_WEIERZETA 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_WEIERZETA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_WEIERZETA (U W1 W3)
    (PROG (Q TAU N1 N3 ET1 ET3 Z L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ W3 (AEVAL (LIST 'SECOND L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ L (AEVAL (LIST 'FIX_ARG U W1 TAU)))
      (SETQ U (AEVAL (LIST 'FIRST L)))
      (COND
       ((EVALEQUAL (AEVAL U) 0)
        (AEVAL
         (REDERR (REVALX "WeierstrassZeta has poles at lattice points")))))
      (SETQ N3 (AEVAL (LIST 'SECOND L)))
      (SETQ N1 (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ ET1
              (AEVAL
               (LIST 'MINUS
                     (LIST 'TIMES
                           (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                 (LIST 'TIMES 12 W1))
                           (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                                 (LIST 'N_THETA1D 0 1 TAU))))))
      (SETQ ET3
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES ET1 TAU)
                     (LIST 'TIMES 'PI
                           (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1))))))
      (SETQ Z (AEVAL (LIST 'TIMES 'PI (LIST 'QUOTIENT U (LIST 'TIMES 2 W1)))))
      (RETURN
       (AEVAL
        (LIST 'PLUS (LIST 'TIMES ET1 (LIST 'QUOTIENT U W1))
              (LIST 'TIMES (LIST 'QUOTIENT 'PI (LIST 'TIMES 2 W1))
                    (LIST 'QUOTIENT (LIST 'N_THETA1D Z 1 TAU)
                          (LIST 'N_THETA1 Z Q TAU)))
              (LIST 'TIMES 2 N3 ET3) (LIST 'TIMES 2 N1 ET1)))))) 
(PUT 'NUM_ETA1 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ETA1) 'OPFN) 
(PUT 'NUM_ETA1 'DEFINED-ON-LINE '486) 
(PUT 'NUM_ETA1 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_ETA1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ETA1 (W1 W3)
    (PROG (TAU TMP)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'MINUS
              (LIST 'TIMES
                    (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) (LIST 'TIMES 12 W1))
                    (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                          (LIST 'N_THETA1D 0 1 TAU)))))))) 
(PUT 'NUM_ETA2 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ETA2) 'OPFN) 
(PUT 'NUM_ETA2 'DEFINED-ON-LINE '496) 
(PUT 'NUM_ETA2 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_ETA2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ETA2 (W1 W3)
    (PROG (TAU TMP)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'PLUS
              (LIST 'TIMES
                    (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) (LIST 'TIMES 12 W1))
                    (LIST 'PLUS 1 TAU)
                    (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                          (LIST 'N_THETA1D 0 1 TAU)))
              (LIST 'TIMES 'PI (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1)))))))) 
(PUT 'NUM_ETA3 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ETA3) 'OPFN) 
(PUT 'NUM_ETA3 'DEFINED-ON-LINE '507) 
(PUT 'NUM_ETA3 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_ETA3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ETA3 (W1 W3)
    (PROG (TAU TMP)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'DIFFERENCE
              (LIST 'MINUS
                    (LIST 'TIMES
                          (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                (LIST 'TIMES 12 W1))
                          TAU
                          (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                                (LIST 'N_THETA1D 0 1 TAU))))
              (LIST 'TIMES 'PI (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1)))))))) 
(PUT 'QUASI_PERIOD_FACTORS 'NUMBER-OF-ARGS 2) 
(FLAG '(QUASI_PERIOD_FACTORS) 'OPFN) 
(PUT 'QUASI_PERIOD_FACTORS 'DEFINED-ON-LINE '518) 
(PUT 'QUASI_PERIOD_FACTORS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'QUASI_PERIOD_FACTORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUASI_PERIOD_FACTORS (W1 W3)
    (COND
     ((EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'QUOTIENT W3 W1))) 0)
      (AEVAL
       (REDERR
        (REVALX
         "Ratio of the period parameters of the lattice must be complex"))))
     (T
      (PROG (RES OLDPREC)
        (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
        (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 3) 15)))
        (SETQ RES (AEVAL (LIST 'NUM_QPF W1 W3)))
        (AEVAL (LIST 'PRECISION OLDPREC))
        (RETURN (AEVAL RES)))))) 
(PUT 'NUM_QPF 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_QPF) 'OPFN) 
(PUT 'NUM_QPF 'DEFINED-ON-LINE '529) 
(PUT 'NUM_QPF 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_QPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_QPF (W1 W3)
    (PROG (TMP TAU TMP1)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (SETQ TMP
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT 'PI 2)
                     (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                           (LIST 'TIMES 12 W1 (LIST 'N_THETA1D 0 1 TAU))))))
      (SETQ TMP1
              (AEVAL (LIST 'TIMES 'PI (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1)))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'MINUS TMP)
              (LIST 'PLUS (LIST 'TIMES (LIST 'PLUS 1 TAU) TMP) TMP1)
              (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES TAU TMP)) TMP1)))))) 
(PUT 'NUM_SIGMA 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_SIGMA) 'OPFN) 
(PUT 'NUM_SIGMA 'DEFINED-ON-LINE '541) 
(PUT 'NUM_SIGMA 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_SIGMA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_SIGMA (U W1 W3)
    (PROG (Q TAU N1 N3 ET1 ET3 TD1 M L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ W3 (AEVAL (LIST 'SECOND L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ L (AEVAL (LIST 'FIX_ARG U W1 TAU)))
      (SETQ U (AEVAL (LIST 'FIRST L)))
      (SETQ N3 (AEVAL (LIST 'SECOND L)))
      (SETQ N1 (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ TD1 (AEVAL (LIST 'N_THETA1D 0 1 TAU)))
      (SETQ ET1
              (AEVAL
               (LIST 'MINUS
                     (LIST 'TIMES
                           (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                 (LIST 'TIMES 12 W1))
                           (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU) TD1)))))
      (SETQ ET3
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES ET1 TAU)
                     (LIST 'TIMES 'PI
                           (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1))))))
      (SETQ M
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (MINUS 1)
                           (LIST 'PLUS N1 N3 (LIST 'TIMES N1 N3)))
                     (LIST 'EXP
                           (LIST 'TIMES
                                 (LIST 'PLUS (LIST 'TIMES 2 N3 ET3)
                                       (LIST 'TIMES 2 N1 ET1))
                                 (LIST 'PLUS U (LIST 'TIMES N1 W1)
                                       (LIST 'TIMES N3 W3)))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES 2 M W1
              (LIST 'EXP
                    (LIST 'TIMES ET1
                          (LIST 'QUOTIENT (LIST 'EXPT U 2)
                                (LIST 'TIMES 2 W1))))
              (LIST 'QUOTIENT
                    (LIST 'N_THETA1
                          (LIST 'TIMES 'PI
                                (LIST 'QUOTIENT U (LIST 'TIMES 2 W1)))
                          Q TAU)
                    (LIST 'TIMES TD1 'PI))))))) 
(PUT 'NUM_SIGMA1 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_SIGMA1) 'OPFN) 
(PUT 'NUM_SIGMA1 'DEFINED-ON-LINE '567) 
(PUT 'NUM_SIGMA1 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_SIGMA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_SIGMA1 (U W1 W3)
    (PROG (TAU ET1 TMP)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (SETQ ET1
              (AEVAL
               (LIST 'MINUS
                     (LIST 'TIMES
                           (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                 (LIST 'TIMES 12 W1))
                           (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                                 (LIST 'N_THETA1D 0 1 TAU))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES (LIST 'EXP (LIST 'MINUS (LIST 'TIMES ET1 U)))
              (LIST 'QUOTIENT (LIST 'NUM_SIGMA (LIST 'PLUS U W1) W1 W3)
                    (LIST 'NUM_SIGMA W1 W1 W3))))))) 
(PUT 'NUM_SIGMA2 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_SIGMA2) 'OPFN) 
(PUT 'NUM_SIGMA2 'DEFINED-ON-LINE '607) 
(PUT 'NUM_SIGMA2 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_SIGMA2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_SIGMA2 (U W1 W3)
    (PROG (Q TAU N1 N3 ET1 ET3 M L)
      (SETQ L (AEVAL (LIST 'FIX_OMEGAS W1 W3)))
      (SETQ W1 (AEVAL (LIST 'FIRST L)))
      (SETQ W3 (AEVAL (LIST 'SECOND L)))
      (SETQ TAU (AEVAL (LIST 'THIRD L)))
      (SETQ L (AEVAL (LIST 'FIX_ARG U W1 TAU)))
      (SETQ U (AEVAL (LIST 'FIRST L)))
      (SETQ N3 (AEVAL (LIST 'SECOND L)))
      (SETQ N1 (AEVAL (LIST 'THIRD L)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ ET1
              (AEVAL
               (LIST 'MINUS
                     (LIST 'TIMES
                           (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                 (LIST 'TIMES 12 W1))
                           (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                                 (LIST 'N_THETA1D 0 1 TAU))))))
      (SETQ ET3
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES ET1 TAU)
                     (LIST 'TIMES 'PI
                           (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1))))))
      (SETQ M
              (AEVAL
               (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'TIMES N1 N3))
                     (LIST 'EXP
                           (LIST 'TIMES
                                 (LIST 'PLUS (LIST 'TIMES 2 N3 ET3)
                                       (LIST 'TIMES 2 N1 ET1))
                                 (LIST 'PLUS U (LIST 'TIMES N1 W1)
                                       (LIST 'TIMES N3 W3)))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES M
              (LIST 'EXP
                    (LIST 'TIMES ET1
                          (LIST 'QUOTIENT (LIST 'EXPT U 2)
                                (LIST 'TIMES 2 W1))))
              (LIST 'QUOTIENT
                    (LIST 'N_THETA3
                          (LIST 'TIMES 'PI
                                (LIST 'QUOTIENT U (LIST 'TIMES 2 W1)))
                          Q)
                    (LIST 'N_THETA3 0 Q))))))) 
(PUT 'NUM_SIGMA3 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_SIGMA3) 'OPFN) 
(PUT 'NUM_SIGMA3 'DEFINED-ON-LINE '625) 
(PUT 'NUM_SIGMA3 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_SIGMA3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_SIGMA3 (U W1 W3)
    (PROG (TAU ET3 TMP)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (SETQ ET3
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'MINUS
                           (LIST 'TIMES
                                 (LIST 'QUOTIENT (LIST 'EXPT 'PI 2)
                                       (LIST 'TIMES 12 W1))
                                 TAU
                                 (LIST 'QUOTIENT (LIST 'N_THETA1D 0 3 TAU)
                                       (LIST 'N_THETA1D 0 1 TAU))))
                     (LIST 'TIMES 'PI
                           (LIST 'QUOTIENT 'I (LIST 'TIMES 2 W1))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES (LIST 'EXP (LIST 'MINUS (LIST 'TIMES ET3 U)))
              (LIST 'QUOTIENT (LIST 'NUM_SIGMA (LIST 'PLUS U W3) W1 W3)
                    (LIST 'NUM_SIGMA W3 W1 W3))))))) 
(PUT 'FIX_OMEGAS 'NUMBER-OF-ARGS 2) 
(FLAG '(FIX_OMEGAS) 'OPFN) 
(PUT 'FIX_OMEGAS 'DEFINED-ON-LINE '653) 
(PUT 'FIX_OMEGAS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'FIX_OMEGAS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIX_OMEGAS (W1 W3)
    (PROG (TMP TAU M)
      (SETQ TAU (AEVAL (LIST 'QUOTIENT W3 W1)))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART TAU)) 0)
        (PROGN
         (SETQ TMP (AEVAL W1))
         (SETQ W1 (AEVAL W3))
         (SETQ W3 (AEVAL TMP))
         (SETQ TAU (AEVAL (LIST 'QUOTIENT 1 TAU)))
         (AEVAL 'NIL))))
      (WHILE (EVALLESSP (AEVAL* (LIST 'IMPART TAU)) (AEVAL* '(|:DN:| 7 . -1)))
             (PROGN
              (SETQ M (AEVAL* (LIST 'ROUND (LIST 'REPART TAU))))
              (SETQ TAU (AEVAL* (LIST 'DIFFERENCE TAU M)))
              (SETQ W3 (AEVAL* (LIST 'DIFFERENCE W3 (LIST 'TIMES M W1))))
              (SETQ TAU (AEVAL* (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
              (SETQ TMP (AEVAL* (LIST 'MINUS W1)))
              (SETQ W1 (AEVAL* W3))
              (SETQ W3 (AEVAL* TMP))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL (LIST 'LIST W1 W3 TAU))))) 
(PUT 'FIX_ARG 'NUMBER-OF-ARGS 3) 
(FLAG '(FIX_ARG) 'OPFN) 
(PUT 'FIX_ARG 'DEFINED-ON-LINE '670) 
(PUT 'FIX_ARG 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'FIX_ARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIX_ARG (U W1 TAU)
    (PROG (Z P1 RP1 NUMP1 NUMP2)
      (SETQ Z (AEVAL (LIST 'QUOTIENT U (LIST 'TIMES 2 W1))))
      (SETQ RP1 (AEVAL (LIST 'IMPART TAU)))
      (SETQ NUMP1 (AEVAL (LIST 'ROUND (LIST 'QUOTIENT (LIST 'IMPART Z) RP1))))
      (SETQ Z (AEVAL (LIST 'DIFFERENCE Z (LIST 'TIMES NUMP1 TAU))))
      (SETQ NUMP2 (AEVAL (LIST 'ROUND (LIST 'REPART Z))))
      (SETQ Z (AEVAL (LIST 'DIFFERENCE Z NUMP2)))
      (RETURN (AEVAL (LIST 'LIST (LIST 'TIMES 2 Z W1) NUMP1 NUMP2))))) 
(PUT 'LATTICE_GENERATORS 'NUMBER-OF-ARGS 2) 
(FLAG '(LATTICE_GENERATORS) 'OPFN) 
(PUT 'LATTICE_GENERATORS 'DEFINED-ON-LINE '685) 
(PUT 'LATTICE_GENERATORS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'LATTICE_GENERATORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LATTICE_GENERATORS (G2 G3)
    (PROG (RES OLDPREC)
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'EXPT G2 3)
                (LIST 'TIMES 27 (LIST 'EXPT G3 2))))
         0)
        (AEVAL
         (REDERR (REVALX "The discriminant of the invariants is zero.")))))
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'MAX (LIST 'PLUS OLDPREC 3) 15)))
      (SETQ RES (AEVAL (LIST 'NUM_OMEGAS G2 G3)))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL RES)))) 
(PUT 'NUM_OMEGAS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_OMEGAS) 'OPFN) 
(PUT 'NUM_OMEGAS 'DEFINED-ON-LINE '701) 
(PUT 'NUM_OMEGAS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_OMEGAS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_OMEGAS (G2 G3)
    (PROG (L KK KK1 M)
      (COND
       ((EVALEQUAL (AEVAL G2) 0)
        (PROGN
         (SETQ M
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'EXPT (LIST 'GAMMA (LIST 'QUOTIENT 1 3)) 3)
                        (LIST 'TIMES 4 'PI
                              (LIST 'EXPT G3 (LIST 'QUOTIENT 1 6))))))
         (RETURN
          (AEVAL
           (LIST 'LIST M
                 (LIST 'TIMES M
                       (LIST 'QUOTIENT
                             (LIST 'PLUS 1 (LIST 'TIMES 'I (LIST 'SQRT 3)))
                             2)))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL G3) 0)
        (PROGN
         (SETQ M
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'EXPT (LIST 'GAMMA (LIST 'QUOTIENT 1 4)) 2)
                        (LIST 'TIMES 4 (LIST 'SQRT 'PI)
                              (LIST 'EXPT G2 (LIST 'QUOTIENT 1 4))))))
         (RETURN (AEVAL (LIST 'LIST M (LIST 'TIMES 'I M))))
         (AEVAL 'NIL))))
      (SETQ L
              (AEVAL
               (LIST 'SOLVE
                     (LIST 'EQUAL
                           (LIST 'DIFFERENCE
                                 (LIST 'DIFFERENCE
                                       (LIST 'TIMES 4 (LIST 'EXPT '*X* 3))
                                       (LIST 'TIMES G2 '*X*))
                                 G3)
                           0)
                     '*X*)))
      (SETQ L
              (PROG (ELEM FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELEM (GETRLIST (AEVAL L)))
                (COND ((NULL ELEM) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELEM) (AEVAL (LIST 'RHS ELEM)))
                                  (CAR ELEM))
                                 NIL)))
               LOOPLABEL
                (SETQ ELEM (CDR ELEM))
                (COND ((NULL ELEM) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELEM) (AEVAL (LIST 'RHS ELEM))) (CAR ELEM))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'FIRST L))) 0)
             (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'SECOND L))) 0)
             (EVALEQUAL (AEVAL (LIST 'IMPART (LIST 'THIRD L))) 0))
        (SETQ L (AEVAL (LIST 'REAL-ROOTS L))))
       (T (SETQ L (AEVAL (LIST 'COMPLEX-ROOTS L)))))
      (SETK 'K (AEVAL (LIST 'FIRST L)))
      (SETK 'K1 (AEVAL (LIST 'SECOND L)))
      (SETQ KK1 (AEVAL (LIST 'AGM_BASIC 1 (LIST 'SQRT 'K1))))
      (SETQ KK (AEVAL (LIST 'AGM_BASIC 1 (LIST 'SQRT 'K))))
      (SETQ M
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 'PI 6)
                     (LIST 'SQRT
                           (LIST 'TIMES G2 (LIST 'PLUS 2 (LIST 'TIMES 'K 'K1))
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE 'K1 'K)
                                       (LIST 'TIMES G3
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'TIMES 'K 'K1)))))))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'QUOTIENT M KK1)
              (LIST 'TIMES 'I (LIST 'QUOTIENT M KK))))))) 
(PUT 'COMPLEX-ROOTS 'NUMBER-OF-ARGS 1) 
(FLAG '(COMPLEX-ROOTS) 'OPFN) 
(PUT 'COMPLEX-ROOTS 'DEFINED-ON-LINE '731) 
(PUT 'COMPLEX-ROOTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'COMPLEX-ROOTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPLEX-ROOTS (L)
    (PROG (L1 N DET K K1 TMP)
      (SETQ L
              (AEVAL
               (LIST 'LIST (LIST 'DIFFERENCE (LIST 'SECOND L) (LIST 'FIRST L))
                     (LIST 'DIFFERENCE (LIST 'THIRD L) (LIST 'SECOND L))
                     (LIST 'DIFFERENCE (LIST 'FIRST L) (LIST 'THIRD L)))))
      (SETQ DET
              (AEVAL
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'REPART (LIST 'FIRST L))
                           (LIST 'IMPART (LIST 'SECOND L)))
                     (LIST 'TIMES (LIST 'REPART (LIST 'SECOND L))
                           (LIST 'IMPART (LIST 'FIRST L))))))
      (COND
       ((EVALLESSP (AEVAL DET) 0)
        (SETQ L
                (AEVAL
                 (LIST 'LIST (LIST 'MINUS (LIST 'FIRST L))
                       (LIST 'MINUS (LIST 'SECOND L))
                       (LIST 'MINUS (LIST 'THIRD L)))))))
      (SETQ L1
              (AEVAL
               (LIST 'LIST (LIST 'ABS (LIST 'FIRST L))
                     (LIST 'ABS (LIST 'SECOND L))
                     (LIST 'ABS (LIST 'THIRD L)))))
      (SETQ N (AEVAL 1))
      (COND
       ((EVALLESSP (AEVAL (LIST 'FIRST L1)) (AEVAL (LIST 'SECOND L1)))
        (SETQ N (AEVAL 2))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'PART L1 N)) (AEVAL (LIST 'THIRD L1)))
        (SETQ N (AEVAL 3))))
      (COND
       ((EVALEQUAL (AEVAL N) 1)
        (PROGN
         (SETQ K (AEVAL (LIST 'MINUS (LIST 'THIRD L))))
         (SETQ K1 (AEVAL (LIST 'MINUS (LIST 'SECOND L))))))
       ((EVALEQUAL (AEVAL N) 2)
        (PROGN
         (SETQ K (AEVAL (LIST 'MINUS (LIST 'FIRST L))))
         (SETQ K1 (AEVAL (LIST 'MINUS (LIST 'THIRD L))))))
       (T
        (PROGN
         (SETQ K (AEVAL (LIST 'MINUS (LIST 'SECOND L))))
         (SETQ K1 (AEVAL (LIST 'MINUS (LIST 'FIRST L)))))))
      (SETQ K (AEVAL (LIST 'QUOTIENT K (LIST 'PART L N))))
      (SETQ K1 (AEVAL (LIST 'QUOTIENT K1 (LIST 'PART L N))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART K)) 0)
        (PROGN
         (SETQ TMP (AEVAL K1))
         (SETQ K1 (AEVAL K))
         (SETQ K (AEVAL TMP)))))
      (RETURN (AEVAL (LIST 'LIST K K1))))) 
(PUT 'REAL-ROOTS 'NUMBER-OF-ARGS 1) 
(FLAG '(REAL-ROOTS) 'OPFN) 
(PUT 'REAL-ROOTS 'DEFINED-ON-LINE '753) 
(PUT 'REAL-ROOTS 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'REAL-ROOTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REAL-ROOTS (L)
    (PROG (E1 E2 E3 K K1)
      (SETQ E1 (AEVAL (LIST 'MAX L)))
      (SETQ E3 (AEVAL (LIST 'MIN L)))
      (SETQ E2 (AEVAL (LIST 'DIFFERENCE (LIST 'MINUS E1) E3)))
      (SETQ K
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE E2 E3)
                     (LIST 'DIFFERENCE E1 E3))))
      (SETQ K1
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE E1 E2)
                     (LIST 'DIFFERENCE E1 E3))))
      (RETURN (AEVAL (LIST 'LIST K K1))))) 
(PUT 'NUM_WEIER1 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_WEIER1) 'OPFN) 
(PUT 'NUM_WEIER1 'DEFINED-ON-LINE '761) 
(PUT 'NUM_WEIER1 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_WEIER1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_WEIER1 (U G2 G3)
    (PROG (L)
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'EXPT G2 3)
                (LIST 'TIMES 27 (LIST 'EXPT G3 2))))
         0)
        (AEVAL
         (REDERR
          (REVALX
           "num_weier: discriminant of the Weierstrass function is zero.")))))
      (SETQ L (AEVAL (LIST 'NUM_OMEGAS G2 G3)))
      (RETURN (AEVAL (LIST 'NUM_WEIER U (LIST 'FIRST L) (LIST 'SECOND L)))))) 
(PUT 'NUM_WEIERZETA1 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_WEIERZETA1) 'OPFN) 
(PUT 'NUM_WEIERZETA1 'DEFINED-ON-LINE '769) 
(PUT 'NUM_WEIERZETA1 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'NUM_WEIERZETA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_WEIERZETA1 (U G2 G3)
    (PROG (L)
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'EXPT G2 3)
                (LIST 'TIMES 27 (LIST 'EXPT G3 2))))
         0)
        (AEVAL
         (REDERR
          (REVALX
           "num_weier: discriminant of the Weierstrass function is zero.")))))
      (SETQ L (AEVAL (LIST 'NUM_OMEGAS G2 G3)))
      (RETURN
       (AEVAL (LIST 'NUM_WEIERZETA U (LIST 'FIRST L) (LIST 'SECOND L)))))) 
(PUT 'WEIERSTRASS1 'FANCY-FUNCTIONSYMBOL "\\wp") 
(PUT 'WEIERSTRASSZETA1 'FANCY-FUNCTIONSYMBOL "\\zeta_w") 
(PUT 'WEIERSTRASS1 'FANCY-PRIFN 'FANCY-WEIER) 
(PUT 'WEIERSTRASSZETA1 'FANCY-PRIFN 'FANCY-WEIER) 
(PUT 'WEIERSTRASSZETA1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'WEIERSTRASS1 'PLAIN-FUNCTIONSYMBOL "P_w") 
(PUT 'WEIERSTRASSZZETA1 'PLAIN-FUNCTIONSYMBOL "zeta_w") 
(PUT 'WEIERSTRASS1 'PRIFN 'PLAIN-WEIER) 
(PUT 'WEIERSTRASSZETA1 'PRIFN 'PLAIN-WEIER) 
(PUT 'WEIERSTRASS 'FANCY-FUNCTIONSYMBOL "\\wp") 
(PUT 'WEIERSTRASSZETA 'FANCY-FUNCTIONSYMBOL "\\zeta_w") 
(PUT 'WEIERSTRASS_SIGMA 'FANCY-FUNCTIONSYMBOL "\\sigma") 
(PUT 'WEIERSTRASS_SIGMA1 'FANCY-FUNCTIONSYMBOL "\\sigma_1") 
(PUT 'WEIERSTRASS_SIGMA2 'FANCY-FUNCTIONSYMBOL "\\sigma_2") 
(PUT 'WEIERSTRASS_SIGMA3 'FANCY-FUNCTIONSYMBOL "\\sigma_3") 
(PUT 'ETA1 'FANCY-FUNCTIONSYMBOL "\\eta_1") 
(PUT 'ETA2 'FANCY-FUNCTIONSYMBOL "\\eta_2") 
(PUT 'ETA3 'FANCY-FUNCTIONSYMBOL "\\eta_3") 
(PUT 'WEIERSTRASSZETA 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'WEIERSTRASS_SIGMA1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'WEIERSTRASS_SIGMA2 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'WEIERSTRASS_SIGMA3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ETA1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ETA2 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ETA3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_E1 'FANCY-FUNCTIONSYMBOL "\\mathrm{e_1}") 
(PUT 'LATTICE_E2 'FANCY-FUNCTIONSYMBOL "\\mathrm{e_2}") 
(PUT 'LATTICE_E3 'FANCY-FUNCTIONSYMBOL "\\mathrm{e_3}") 
(PUT 'LATTICE_E1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_E2 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_E3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'WEIERSTRASS 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'WEIERSTRASSZETA 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'WEIERSTRASS 'PLAIN-FUNCTIONSYMBOL "P_w") 
(PUT 'WEIERSTRASSZETA 'PLAIN-FUNCTIONSYMBOL 'ZETA_W) 
(PUT 'LATTICE_E1 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_E1 'PLAIN-FUNCTIONSYMBOL 'E1) 
(PUT 'LATTICE_E2 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_E2 'PLAIN-FUNCTIONSYMBOL 'E2) 
(PUT 'LATTICE_E3 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_E3 'PLAIN-FUNCTIONSYMBOL 'E3) 
(PUT 'LATTICE_G2 'FANCY-FUNCTIONSYMBOL "\\mathrm{g_2}") 
(PUT 'LATTICE_G3 'FANCY-FUNCTIONSYMBOL "\\mathrm{g_3}") 
(PUT 'LATTICE_DELTA 'FANCY-FUNCTIONSYMBOL "\\Delta") 
(PUT 'LATTICE_G 'FANCY-FUNCTIONSYMBOL "\\mathrm{G}") 
(PUT 'LATTICE_G2 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_G3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_OMEGA1 'FANCY-FUNCTIONSYMBOL "\\omega_1") 
(PUT 'LATTICE_OMEGA3 'FANCY-FUNCTIONSYMBOL "\\omega_3") 
(PUT 'LATTICE_OMEGA1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_OMEGA3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'LATTICE_G2 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTIC_G2 'PLAIN-FUNCTIONSYMBOL 'G2) 
(PUT 'LATTICE_G3 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_G3 'PLAIN-FUNCTIONSYMBOL 'G3) 
(PUT 'LATTICE_DELTA 'PLAIN-FUNCTIONSYMBOL '|dELTA|) 
(PUT 'LATTICE_DELTA 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_G 'PLAIN-FUNCTIONSYMBOL '|g|) 
(PUT 'LATTICE_G 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_OMEGA1 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_OMEGA1 'PLAIN-FUNCTIONSYMBOL 'W1) 
(PUT 'LATTICE_OMEGA3 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'LATTICE_OMEGA3 'PLAIN-FUNCTIONSYMBOL 'W3) 
(FLAG
 '(WEIERSTRASS_SIGMA WEIERSTRASS_SIGMA1 WEIERSTRASS_SIGMA2 WEIERSTRASS_SIGMA3
   WEIERSTRASS WEIERSTRASSZETA ETA1 ETA2 ETA3 LATTICE_E1 LATTICE_E2 LATTICE_E3
   LATTICE_G2 LATTICE_G3 LATTICE_DELTA LATTICE_G LATTICE_OMEGA1 LATTICE_OMEGA3
   WEIERSTRASS1 WEIERSTRASSZETA1)
 'SPECFN) 
(DEFLIST
 '((WEIERSTRASS_SIGMA 3) (WEIERSTRASS_SIGMA1 3) (WEIERSTRASS_SIGMA2 3)
   (WEIERSTRASS_SIGMA3 3) (WEIERSTRASS 3) (WEIERSTRASSZETA 3) (ETA1 2) (ETA2 2)
   (ETA3 2) (LATTICE_E1 2) (LATTICE_E3 2) (LATTICE_E3 2) (LATTICE_ROOTS 2)
   (LATTICE_INVARIANTS 2) (LATTICE_G2 2) (LATTICE_G3 2) (LATTICE_DELTA 2)
   (LATTICE_G 2) (WEIERSTRASS1 3) (WEIERSTRASSZETA1 3) (LATTICE_GENERATORS 2)
   (QUASI_PERIOD_FACTORS 2) (LATTICE_OMEGA1 2) (LATTICE_OMEGA3 2))
 'NUMBER-OF-ARGS) 
(FLUID '(FANCY-POS* FANCY-TEXPOS FANCY-LINE*)) 
(PUT 'FANCY-WEIER 'NUMBER-OF-ARGS 1) 
(PUT 'FANCY-WEIER 'DEFINED-ON-LINE '889) 
(PUT 'FANCY-WEIER 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'FANCY-WEIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FANCY-WEIER (U)
    (PROG (POS TPOS FL W)
      (SETQ POS FANCY-POS*)
      (SETQ TPOS FANCY-TEXPOS)
      (SETQ FL FANCY-LINE*)
      (SETQ W
              (PROG (ARGS)
                (FANCY-PREFIX-OPERATOR (CAR U))
                (SETQ ARGS (LIST (CADR U) '|\|| (CADDR U) '|,| (CADDDR U)))
                (RETURN
                 (FANCY-IN-BRACKETS
                  (LIST 'FANCY-INPRINT (MKQUOTE 'TIMES) 1 (MKQUOTE ARGS)) '|(|
                  '|)|))))
      (COND
       ((EQ W 'FAILED) (SETQ FANCY-LINE* FL) (SETQ FANCY-TEXPOS TPOS)
        (SETQ FANCY-POS* POS)))
      (RETURN W))) 
(PUT 'PLAIN-WEIER 'NUMBER-OF-ARGS 1) 
(PUT 'PLAIN-WEIER 'DEFINED-ON-LINE '899) 
(PUT 'PLAIN-WEIER 'DEFINED-IN-FILE 'ELLIPFN/EFWEIER.RED) 
(PUT 'PLAIN-WEIER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PLAIN-WEIER (U)
    (PROG ()
      (PRIN2* (GET (CAR U) 'PLAIN-FUNCTIONSYMBOL))
      (PRIN2* "(")
      (MAPRIN (CADR U))
      (PRIN2* "|")
      (MAPRIN (CADDR U))
      (PRIN2* ",")
      (MAPRIN (CADDDR U))
      (PRIN2* ")")
      (RETURN U))) 
(OPERATOR (LIST 'RW* 'RW1*)) 
(SETK 'WEIER-RECIP-RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'RW* (LIST '~ 'X) (LIST '~ 'W1) (LIST '~ 'W3))
                         'X)
                   (LIST 'SQRT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 4 (LIST 'RW* 'X 'W1 'W3))
                                     (LIST 'TIMES (LIST 'LATTICE_G2 'W1 'W3)
                                           (LIST 'EXPT (LIST 'RW* 'X 'W1 'W3)
                                                 3)))
                               (LIST 'TIMES (LIST 'LATTICE_G3 'W1 'W3)
                                     (LIST 'EXPT (LIST 'RW* 'X 'W1 'W3) 4)))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'RW1* (LIST '~ 'X) (LIST '~ 'G2) (LIST '~ 'G3))
                         'X)
                   (LIST 'SQRT
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES 4 (LIST 'RW1* 'X 'G2 'G3))
                                     (LIST 'TIMES 'G2
                                           (LIST 'EXPT (LIST 'RW1* 'X 'G2 'G3)
                                                 3)))
                               (LIST 'TIMES 'G3
                                     (LIST 'EXPT (LIST 'RW1* 'X 'G2 'G3) 4)))))
             (LIST 'REPLACEBY (LIST 'RW* 0 (LIST '~ 'W1) (LIST '~ 'W3)) 0)
             (LIST 'REPLACEBY (LIST 'RW1* 0 (LIST '~ 'G2) (LIST '~ 'G3)) 0)
             (LIST 'REPLACEBY
                   (LIST 'RW* (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'RW* 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'RW1* (LIST 'MINUS (LIST '~ 'U)) (LIST '~ 'W1)
                         (LIST '~ 'W3))
                   (LIST 'RW1* 'U 'W1 'W3))
             (LIST 'REPLACEBY
                   (LIST 'RW*
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W1)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W1))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''RW* 'ARG ''W1 ''W3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'RW*
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K) (LIST '~ 'W3)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'W1) (LIST '~ 'W3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       ''W3))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''RW* 'ARG ''W1 ''W3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'RW1*
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K)
                                           (LIST 'LATTICE_OMEGA1 (LIST '~ 'G2)
                                                 (LIST '~ 'G3))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       (LIST 'LIST
                                                             ''LATTICE_OMEGA1
                                                             ''G2 ''G3)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''RW1* 'ARG ''G2
                                                 ''G3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'RW1*
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'U))
                                     (LIST 'TIMES (LIST '~ 'K)
                                           (LIST 'LATTICE_OMEGA3 (LIST '~ 'G2)
                                                 (LIST '~ 'G3))))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'G2) (LIST '~ 'G3))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'M 'ARG)
                               (LIST 'SETQ 'M
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''U
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   'M))
                                                       (LIST 'LIST
                                                             ''LATTICE_OMEGA3
                                                             ''G2 ''G3)))))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''RW1* 'ARG ''G2
                                                 ''G3))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 2))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART
                                           (LIST 'QUOTIENT 'K 'D))))))))) 
(LET '(WEIER-RECIP-RULES)) 
(ENDMODULE) 