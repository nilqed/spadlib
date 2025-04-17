(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRIGSMP1)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(CLEARRULES (LIST 'TRIG_IMAG_RULES)) 
(SETK 'TRIG_NORMALIZE2SIN*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'COS (LIST '~ 'A)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'SIN 'A) 2)))))) 
(SETK 'TRIG_NORMALIZE2COS*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SIN (LIST '~ 'A)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'COS 'A) 2)))))) 
(SETK 'TRIG_NORMALIZE2SINH*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'COSH (LIST '~ 'A)) 2)
                   (LIST 'PLUS 1 (LIST 'EXPT (LIST 'SINH 'A) 2)))))) 
(SETK 'TRIG_NORMALIZE2COSH*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SINH (LIST '~ 'A)) 2)
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'COSH 'A) 2) 1))))) 
(SETK 'TRIG_EXPAND_ADDITION*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'SIN (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COS (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'TIMES (LIST 'COS (LIST 'QUOTIENT 'A 'M))
                               (LIST 'SIN (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'COS (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COS (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'TIMES (LIST 'SIN (LIST 'QUOTIENT 'A 'M))
                               (LIST 'SIN (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'TAN
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'TAN (LIST 'QUOTIENT 'A 'M))
                               (LIST 'TAN (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'TAN (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'TAN (LIST 'QUOTIENT 'B 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'COT
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'COT (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COT (LIST 'QUOTIENT 'B 'M)))
                               1)
                         (LIST 'PLUS (LIST 'COT (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COT (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'SEC
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT 1
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SEC (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'SEC (LIST 'QUOTIENT 'B 'M))))
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'CSC (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'CSC
                                                 (LIST 'QUOTIENT 'B 'M)))))))
             (LIST 'REPLACEBY
                   (LIST 'CSC
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT 1
                         (LIST 'PLUS
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SEC (LIST 'QUOTIENT 'B 'M))
                                           (LIST 'CSC (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SEC (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'CSC
                                                 (LIST 'QUOTIENT 'B 'M)))))))
             (LIST 'REPLACEBY
                   (LIST 'TANH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'TANH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'TANH (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'TANH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'TANH (LIST 'QUOTIENT 'B 'M))))))
             (LIST 'REPLACEBY
                   (LIST 'COTH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'COTH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COTH (LIST 'QUOTIENT 'B 'M)))
                               1)
                         (LIST 'PLUS (LIST 'COTH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COTH (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'SINH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COSH (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'TIMES (LIST 'COSH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'SINH (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'COSH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'COSH (LIST 'QUOTIENT 'B 'M)))
                         (LIST 'TIMES (LIST 'SINH (LIST 'QUOTIENT 'A 'M))
                               (LIST 'SINH (LIST 'QUOTIENT 'B 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'SECH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT 1
                         (LIST 'PLUS
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SECH (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'SECH
                                                 (LIST 'QUOTIENT 'B 'M))))
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'CSCH (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'CSCH
                                                 (LIST 'QUOTIENT 'B 'M)))))))
             (LIST 'REPLACEBY
                   (LIST 'CSCH
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST '~ 'A) (LIST '~ 'B))
                               (LIST '~ (LIST '~ 'M))))
                   (LIST 'QUOTIENT 1
                         (LIST 'PLUS
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SECH (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'CSCH
                                                 (LIST 'QUOTIENT 'B 'M))))
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES
                                           (LIST 'SECH (LIST 'QUOTIENT 'B 'M))
                                           (LIST 'CSCH
                                                 (LIST 'QUOTIENT 'A 'M)))))))))) 
(SETK 'TRIG_EXPAND_MULTIPLICATION*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'SIN (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES (LIST 'COS (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 2
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M)))
                               (LIST 'COS
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SIN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'COS (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES (LIST 'SIN (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2
                                     (LIST 'EXPT
                                           (LIST 'COS
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 'N 2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           2))
                               1)
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COS
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COS
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'SIN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'SINH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES (LIST 'COSH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'LEQ 'N 15)
                               (LIST 'GREATERP 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 2
                               (LIST 'SINH
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M)))
                               (LIST 'COSH
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SINH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COSH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COSH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'COSH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES (LIST 'SINH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2
                                     (LIST 'EXPT
                                           (LIST 'COSH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 'N 2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           2))
                               1)
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COSH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'COSH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COSH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'TIMES
                                     (LIST 'SINH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'SINH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TAN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'TAN (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'TAN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'DIFFERENCE 1
                                     (LIST 'TIMES
                                           (LIST 'TAN (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'TAN
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TAN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 2
                               (LIST 'QUOTIENT
                                     (LIST 'TAN
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'EXPT
                                                 (LIST 'TAN
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TAN
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'TAN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'TAN
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'DIFFERENCE 1
                                     (LIST 'TIMES
                                           (LIST 'TAN
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           (LIST 'TAN
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'PLUS 'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TANH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'TANH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'TANH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'PLUS 1
                                     (LIST 'TIMES
                                           (LIST 'TANH (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'TANH
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TANH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES 2
                               (LIST 'QUOTIENT
                                     (LIST 'TANH
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'PLUS 1
                                           (LIST 'EXPT
                                                 (LIST 'TANH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'TANH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'TANH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'TANH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M))))
                               (LIST 'PLUS 1
                                     (LIST 'TIMES
                                           (LIST 'TANH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           (LIST 'TANH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'PLUS 'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COT
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'COT (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'COT
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))
                                     1)
                               (LIST 'PLUS (LIST 'COT (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COT
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COT
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'EXPT
                                           (LIST 'COT
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 'N 2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           2)
                                     1)
                               (LIST 'TIMES 2
                                     (LIST 'COT
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COT
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'COT
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           (LIST 'COT
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'PLUS 'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))
                                     1)
                               (LIST 'PLUS
                                     (LIST 'COT
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COT
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COTH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'TIMES
                                           (LIST 'COTH (LIST 'QUOTIENT 'A 'M))
                                           (LIST 'COTH
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))
                                     1)
                               (LIST 'PLUS (LIST 'COTH (LIST 'QUOTIENT 'A 'M))
                                     (LIST 'COTH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COTH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'EXPT
                                           (LIST 'COTH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 'N 2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           2)
                                     1)
                               (LIST 'TIMES 2
                                     (LIST 'COTH
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'COTH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'TIMES
                                           (LIST 'COTH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A 'M)))
                                           (LIST 'COTH
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT
                                                             (LIST 'PLUS 'N 1)
                                                             2)
                                                       (LIST 'QUOTIENT 'A
                                                             'M))))
                                     1)
                               (LIST 'PLUS
                                     (LIST 'COTH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'N 1)
                                                       2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     (LIST 'COTH
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'N 1) 2)
                                                 (LIST 'QUOTIENT 'A 'M)))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SEC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SEC
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSC
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SEC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT 1
                                           (LIST 'EXPT
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'EXPT
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SEC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'DIFFERENCE
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'CSC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SEC
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSC
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'CSC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SEC
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M)))
                               (LIST 'QUOTIENT
                                     (LIST 'CSC
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     2))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0))))
             (LIST 'REPLACEBY
                   (LIST 'CSC
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'SEC
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SECH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SECH
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSCH
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SECH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'EXPT
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'EXPT
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 'N
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 2))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'SECH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'CSCH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SECH
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSCH
                                                       (LIST 'QUOTIENT 'A 'M))
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'N 1)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'LEQ 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'CSCH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'SECH
                                     (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                           (LIST 'QUOTIENT 'A 'M)))
                               (LIST 'QUOTIENT
                                     (LIST 'CSCH
                                           (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                                                 (LIST 'QUOTIENT 'A 'M)))
                                     2))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 0)
                               (LIST 'GREATERP 'N 15))))
             (LIST 'REPLACEBY
                   (LIST 'CSCH
                         (LIST 'TIMES (LIST '~ 'N)
                               (LIST 'QUOTIENT (LIST '~ 'A)
                                     (LIST '~ (LIST '~ 'M)))))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT 1
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))
                                     (LIST 'QUOTIENT 1
                                           (LIST 'TIMES
                                                 (LIST 'CSCH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))
                                                 (LIST 'SECH
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'PLUS
                                                                         'N 1)
                                                                   2)
                                                             (LIST 'QUOTIENT 'A
                                                                   'M)))))))
                         (LIST 'AND (LIST 'FIXP 'N)
                               (LIST 'EQUAL (LIST 'MOD 'N 2) 1)
                               (LIST 'GREATERP 'N 15))))))) 
(SETK 'TRIG_COMBINE*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'SIN (LIST '~ 'A))
                         (LIST 'SIN (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'DIFFERENCE (LIST 'COS (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'COS (LIST 'PLUS 'A 'B)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'COS (LIST '~ 'A))
                         (LIST 'COS (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS (LIST 'COS (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'COS (LIST 'PLUS 'A 'B)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'SIN (LIST '~ 'A))
                         (LIST 'COS (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS (LIST 'SIN (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'SIN (LIST 'PLUS 'A 'B)))))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SIN (LIST '~ 'A)) 2)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'DIFFERENCE 1 (LIST 'COS (LIST 'TIMES 2 'A)))))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'COS (LIST '~ 'A)) 2)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS 1 (LIST 'COS (LIST 'TIMES 2 'A)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'SINH (LIST '~ 'A))
                         (LIST 'SINH (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'DIFFERENCE (LIST 'COSH (LIST 'PLUS 'A 'B))
                               (LIST 'COSH (LIST 'DIFFERENCE 'A 'B)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'COSH (LIST '~ 'A))
                         (LIST 'COSH (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS (LIST 'COSH (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'COSH (LIST 'PLUS 'A 'B)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'SINH (LIST '~ 'A))
                         (LIST 'COSH (LIST '~ 'B)))
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS (LIST 'SINH (LIST 'DIFFERENCE 'A 'B))
                               (LIST 'SINH (LIST 'PLUS 'A 'B)))))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SINH (LIST '~ 'A)) 2)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'DIFFERENCE (LIST 'COSH (LIST 'TIMES 2 'A)) 1)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'COSH (LIST '~ 'A)) 2)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS 1 (LIST 'COSH (LIST 'TIMES 2 'A)))))))) 
(SETK 'TRIG_STANDARDIZE*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'TAN (LIST '~ 'A))
                   (LIST 'QUOTIENT (LIST 'SIN 'A) (LIST 'COS 'A)))
             (LIST 'REPLACEBY (LIST 'COT (LIST '~ 'A))
                   (LIST 'QUOTIENT (LIST 'COS 'A) (LIST 'SIN 'A)))
             (LIST 'REPLACEBY (LIST 'TANH (LIST '~ 'A))
                   (LIST 'QUOTIENT (LIST 'SINH 'A) (LIST 'COSH 'A)))
             (LIST 'REPLACEBY (LIST 'COTH (LIST '~ 'A))
                   (LIST 'QUOTIENT (LIST 'COSH 'A) (LIST 'SINH 'A)))
             (LIST 'REPLACEBY (LIST 'SEC (LIST '~ 'A))
                   (LIST 'QUOTIENT 1 (LIST 'COS 'A)))
             (LIST 'REPLACEBY (LIST 'CSC (LIST '~ 'A))
                   (LIST 'QUOTIENT 1 (LIST 'SIN 'A)))
             (LIST 'REPLACEBY (LIST 'SECH (LIST '~ 'A))
                   (LIST 'QUOTIENT 1 (LIST 'COSH 'A)))
             (LIST 'REPLACEBY (LIST 'CSCH (LIST '~ 'A))
                   (LIST 'QUOTIENT 1 (LIST 'SINH 'A)))))) 
(SETK 'TRIG2EXP*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'A))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'EXPT 'E (LIST 'TIMES 'I 'A))
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'I 'A))))
                         2))
             (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'A))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'EXPT 'E (LIST 'TIMES 'I 'A))
                                           (LIST 'EXPT 'E
                                                 (LIST 'MINUS
                                                       (LIST 'TIMES 'I 'A))))
                                     2))))
             (LIST 'REPLACEBY (LIST 'COSH (LIST '~ 'A))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'EXPT 'E 'A)
                               (LIST 'EXPT 'E (LIST 'MINUS 'A)))
                         2))
             (LIST 'REPLACEBY (LIST 'SINH (LIST '~ 'A))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'EXPT 'E 'A)
                               (LIST 'EXPT 'E (LIST 'MINUS 'A)))
                         2))))) 
(SETK 'POW2QUOT*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST 'QUOTIENT (LIST '~ 'A) (LIST '~ 'B))
                         (LIST '~ 'C))
                   (LIST 'QUOTIENT (LIST 'EXPT 'A 'C) (LIST 'EXPT 'B 'C)))))) 
(SETK 'EXP2TRIG1*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT 'E (LIST '~ 'X))
                   (LIST 'PLUS (LIST 'COS (LIST 'QUOTIENT 'X 'I))
                         (LIST 'TIMES 'I
                               (LIST 'SIN (LIST 'QUOTIENT 'X 'I)))))))) 
(SETK 'EXP2TRIG2*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT 'E (LIST '~ 'X))
                   (LIST 'QUOTIENT 1
                         (LIST 'DIFFERENCE (LIST 'COS (LIST 'QUOTIENT 'X 'I))
                               (LIST 'TIMES 'I
                                     (LIST 'SIN (LIST 'QUOTIENT 'X 'I))))))))) 
(SETK 'TRIG2HYP*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'A))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'SINH (LIST 'TIMES 'I 'A)))))
             (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'A))
                   (LIST 'COSH (LIST 'TIMES 'I 'A)))
             (LIST 'REPLACEBY (LIST 'TAN (LIST '~ 'A))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'TANH (LIST 'TIMES 'I 'A)))))
             (LIST 'REPLACEBY (LIST 'COT (LIST '~ 'A))
                   (LIST 'TIMES 'I (LIST 'COTH (LIST 'TIMES 'I 'A))))
             (LIST 'REPLACEBY (LIST 'SEC (LIST '~ 'A))
                   (LIST 'SECH (LIST 'TIMES 'I 'A)))
             (LIST 'REPLACEBY (LIST 'CSC (LIST '~ 'A))
                   (LIST 'TIMES 'I (LIST 'CSCH (LIST 'TIMES 'I 'A))))
             (LIST 'REPLACEBY (LIST 'ASIN (LIST '~ 'A))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'ASINH (LIST 'TIMES 'I 'A)))))
             (LIST 'REPLACEBY (LIST 'ACOS (LIST '~ 'A))
                   (LIST 'MINUS (LIST 'TIMES 'I (LIST 'ACOSH 'A))))))) 
(SETK 'HYP2TRIG*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SINH (LIST '~ 'A))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I (LIST 'SIN (LIST 'TIMES 'I 'A)))))
             (LIST 'REPLACEBY (LIST 'COSH (LIST '~ 'A))
                   (LIST 'COS (LIST 'TIMES 'I 'A)))
             (LIST 'REPLACEBY (LIST 'ASINH (LIST '~ 'A))
                   (LIST 'TIMES 'I
                         (LIST 'ASIN (LIST 'MINUS (LIST 'TIMES 'I 'A)))))
             (LIST 'REPLACEBY (LIST 'ACOSH (LIST '~ 'A))
                   (LIST 'TIMES 'I (LIST 'ACOS 'A)))))) 
(SETK 'SUBTAN*
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES (LIST 'COS 'X) (LIST 'TAN 'X))
                         (LIST 'EQUAL 'TRIG_PREFERENCE 'COS)))
             (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'QUOTIENT (LIST 'SIN 'X) (LIST 'TAN 'X))
                         (LIST 'EQUAL 'TRIG_PREFERENCE 'SIN)))
             (LIST 'REPLACEBY (LIST 'SINH (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'TIMES (LIST 'COSH 'X) (LIST 'TANH 'X))
                         (LIST 'EQUAL 'HYP_PREFERENCE 'COSH)))
             (LIST 'REPLACEBY (LIST 'COSH (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'QUOTIENT (LIST 'SINH 'X) (LIST 'TANH 'X))
                         (LIST 'EQUAL 'HYP_PREFERENCE 'SINH)))))) 
(ENDMODULE) 