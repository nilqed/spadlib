(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFJACINV)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'ARCSN)) 
(OPERATOR (LIST 'ARCCN)) 
(OPERATOR (LIST 'ARCDN)) 
(OPERATOR (LIST 'ARCNS)) 
(OPERATOR (LIST 'ARCNC)) 
(OPERATOR (LIST 'ARCND)) 
(OPERATOR (LIST 'ARCSC)) 
(OPERATOR (LIST 'ARCCS)) 
(OPERATOR (LIST 'ARCSD)) 
(OPERATOR (LIST 'ARCDS)) 
(OPERATOR (LIST 'ARCCD)) 
(OPERATOR (LIST 'ARCDC)) 
(SETK 'INVJACOBIRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'JACOBISN (LIST 'ARCSN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICN (LIST 'ARCCN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDN (LIST 'ARCDN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBINS (LIST 'ARCNS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBINC (LIST 'ARCNC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIND (LIST 'ARCND (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBISC (LIST 'ARCSC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICS (LIST 'ARCCS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBISD (LIST 'ARCSD (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDS (LIST 'ARCDS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBICD (LIST 'ARCCD (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY
                   (LIST 'JACOBIDC (LIST 'ARCDC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'K))
                   'X)
             (LIST 'REPLACEBY (LIST 'ARCSN (LIST '~ 'X) 0) (LIST 'ASIN 'X))
             (LIST 'REPLACEBY (LIST 'ARCSN (LIST '~ 'X) 1) (LIST 'ATANH 'X))
             (LIST 'REPLACEBY (LIST 'ARCSN 0 (LIST '~ 'K)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ARCSN (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'ARCSN 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCSN (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCSN 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCSN 1 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY (LIST 'ARCNS (LIST '~ 'X) 0) (LIST 'ACSC 'X))
             (LIST 'REPLACEBY (LIST 'ARCNS (LIST '~ 'X) 1) (LIST 'ACOTH 'X))
             (LIST 'REPLACEBY (LIST 'ARCNS 0 (LIST '~ 'K))
                   (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ARCNS (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'ARCNS 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCNS (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCNS 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCNS 1 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY (LIST 'ARCCN (LIST '~ 'X) 0) (LIST 'ACOS 'X))
             (LIST 'REPLACEBY (LIST 'ARCCN (LIST '~ 'X) 1) (LIST 'ASECH 'X))
             (LIST 'REPLACEBY (LIST 'ARCCN 0 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY (LIST 'ARCCN 1 (LIST '~ 'K)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ARCCN (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK 'K))
                               (LIST 'ARCCN 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCCN (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCCN 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCNC (LIST '~ 'X) 0) (LIST 'ASEC 'X))
             (LIST 'REPLACEBY (LIST 'ARCNC (LIST '~ 'X) 1) (LIST 'ACOSH 'X))
             (LIST 'REPLACEBY (LIST 'ARCNC 0 (LIST '~ 'K))
                   (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ARCNC (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK 'K))
                               (LIST 'ARCNC 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCNC (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCNC 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCDN (LIST '~ 'X) 1) (LIST 'ASECH 'X))
             (LIST 'REPLACEBY (LIST 'ARCDN 0 (LIST '~ 'K))
                   (LIST 'PLUS (LIST 'ELLIPTICK 'K)
                         (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDN (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2
                                     (LIST 'PLUS (LIST 'ELLIPTICK 'K)
                                           (LIST 'TIMES 'I
                                                 (LIST '|ELLIPTICK'| 'K))))
                               (LIST 'ARCDN 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDN
                         (LIST 'SQRT
                               (LIST 'DIFFERENCE 1
                                     (LIST 'EXPT (LIST '~ 'K) 2)))
                         (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY
                   (LIST 'ARCDN (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCDN 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCND (LIST '~ 'X) 1) (LIST 'ACOSH 'X))
             (LIST 'REPLACEBY (LIST 'ARCND 0 (LIST '~ 'K))
                   (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K)))
             (LIST 'REPLACEBY
                   (LIST 'ARCND (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2
                                     (LIST 'PLUS (LIST 'ELLIPTICK 'K)
                                           (LIST 'TIMES 'I
                                                 (LIST '|ELLIPTICK'| 'K))))
                               (LIST 'ARCND 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCND
                         (LIST 'QUOTIENT 1
                               (LIST 'SQRT
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'EXPT (LIST '~ 'K) 2))))
                         (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY
                   (LIST 'ARCND (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCDN 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCSC (LIST '~ 'X) 0) (LIST 'ATAN 'X))
             (LIST 'REPLACEBY (LIST 'ARCSC (LIST '~ 'X) 1) (LIST 'ASINH 'X))
             (LIST 'REPLACEBY (LIST 'ARCSC 0 (LIST '~ 'K)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ARCSC (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'ARCSC 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCSC (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCSC 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCCS (LIST '~ 'X) 0) (LIST 'ACOT 'X))
             (LIST 'REPLACEBY (LIST 'ARCCS (LIST '~ 'X) 1) (LIST 'ACSCH 'X))
             (LIST 'REPLACEBY (LIST 'ARCCS 0 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY
                   (LIST 'ARCCS (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK 'K))
                               (LIST 'ARCCS 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCCS (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCCS 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCSD (LIST '~ 'X) 0) (LIST 'ASIN 'X))
             (LIST 'REPLACEBY (LIST 'ARCSD (LIST '~ 'X) 1) (LIST 'ASINH 'X))
             (LIST 'REPLACEBY (LIST 'ARCSD 0 (LIST '~ 'K)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ARCSD (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'ARCSD 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCSD (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCSD 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCDS (LIST '~ 'X) 0) (LIST 'ACSC 'X))
             (LIST 'REPLACEBY (LIST 'ARCDS (LIST '~ 'X) 1) (LIST 'ACSCH 'X))
             (LIST 'REPLACEBY (LIST 'ARCDS 0 (LIST '~ 'K))
                   (LIST 'PLUS (LIST 'ELLIPTICK 'K)
                         (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDS (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'ARCDS 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDS (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCCS 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCCD (LIST '~ 'X) 0) (LIST 'ACOS 'X))
             (LIST 'REPLACEBY (LIST 'ARCCD 0 (LIST '~ 'K))
                   (LIST 'ELLIPTICK 'K))
             (LIST 'REPLACEBY
                   (LIST 'ARCCD (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK 'K))
                               (LIST 'ARCCD 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCCD (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCCD 'X 'K))
             (LIST 'REPLACEBY (LIST 'ARCDC (LIST '~ 'X) 0) (LIST 'ASEC 'X))
             (LIST 'REPLACEBY (LIST 'ARCDC 0 (LIST '~ 'K))
                   (LIST 'PLUS (LIST 'ELLIPTICK 'K)
                         (LIST 'TIMES 'I (LIST '|ELLIPTICK'| 'K))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDC (LIST 'MINUS (LIST '~ 'X)) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK 'K))
                               (LIST 'ARCDC 'X 'K))
                         (LIST 'NOT (LIST 'NUMBERP 'X))))
             (LIST 'REPLACEBY
                   (LIST 'ARCDC (LIST '~ 'X) (LIST 'MINUS (LIST '~ 'K)))
                   (LIST 'ARCDC 'X 'K))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCSN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'X 2)))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'TIMES (LIST 'EXPT 'K 2)
                                                       (LIST 'EXPT 'X 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCSN 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASIN 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K 'X
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST 'EXPT
                                                                          'X
                                                                          2))
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'EXPT 'K
                                                                      2)
                                                                     (LIST
                                                                      'EXPT 'X
                                                                      2)))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCCN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'X 2)))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K 2))
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT 'X
                                                                   2)))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCCN 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ACOS 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K 'X
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST 'EXPT
                                                                          'X
                                                                          2))
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2))
                                                                         (LIST
                                                                          'TIMES
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2)
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCDN (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'X))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'X 2)))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'X 2)
                                                             1)
                                                       (LIST 'EXPT 'K 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCDN 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASIN
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'SQRT
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)))
                                                                   'K))
                                                       'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'X
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST 'EXPT
                                                                          'X
                                                                          2))
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          1)
                                                                         (LIST
                                                                          'EXPT
                                                                          'K
                                                                          2))))
                                                       (LIST 'TIMES 'K
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K
                                                                         2))))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCCD (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'X 2)))
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT 'X
                                                                   2)))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCCD 'X 'K)
                                                 'K))
                                     (LIST 'QUOTIENT
                                           (LIST 'ELLIPTICE
                                                 (LIST 'ASIN
                                                       (LIST 'SQRT
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST 'EXPT
                                                                          'X
                                                                          2))
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    1
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'EXPT 'K
                                                                      2)
                                                                     (LIST
                                                                      'EXPT 'X
                                                                      2))))))
                                                 'K)
                                           (LIST 'TIMES 'K
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCSD (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K 2))
                                                             (LIST 'EXPT 'X
                                                                   2))))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS 1
                                                       (LIST 'TIMES
                                                             (LIST 'EXPT 'K 2)
                                                             (LIST 'EXPT 'X
                                                                   2)))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCSD 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASIN
                                                             (LIST 'QUOTIENT 'X
                                                                   (LIST 'SQRT
                                                                         (LIST
                                                                          'PLUS
                                                                          1
                                                                          (LIST
                                                                           'TIMES
                                                                           (LIST
                                                                            'EXPT
                                                                            'K
                                                                            2)
                                                                           (LIST
                                                                            'EXPT
                                                                            'X
                                                                            2))))))
                                                       'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'QUOTIENT 'X
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'PLUS
                                                                          1
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2)
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2))
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          1
                                                                          (LIST
                                                                           'TIMES
                                                                           (LIST
                                                                            'DIFFERENCE
                                                                            1
                                                                            (LIST
                                                                             'EXPT
                                                                             'K
                                                                             2))
                                                                           (LIST
                                                                            'EXPT
                                                                            'X
                                                                            2))))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCND (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT 'X 2)))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K 2))
                                                       (LIST 'EXPT 'X 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCND 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASIN
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'SQRT
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          1))
                                                                   (LIST 'TIMES
                                                                         'K
                                                                         'X)))
                                                       'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'QUOTIENT
                                                 (LIST 'SQRT
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'X 2)
                                                                   1)
                                                             (LIST 'PLUS 1
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'DIFFERENCE
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2)
                                                                          1)
                                                                         (LIST
                                                                          'EXPT
                                                                          'X
                                                                          2)))))
                                                 (LIST 'TIMES 'K
                                                       (LIST 'X
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K
                                                                         2))))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCDC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'X 2)
                                                 1))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'X 2)
                                                 (LIST 'EXPT 'K 2)))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCDC 'X 'K)
                                                 'K))
                                     (LIST 'QUOTIENT
                                           (LIST 'ELLIPTICE
                                                 (LIST 'ASIN
                                                       (LIST 'SQRT
                                                             (LIST 'QUOTIENT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'EXPT
                                                                          'X 2)
                                                                    1)
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'EXPT
                                                                          'X 2)
                                                                    (LIST 'EXPT
                                                                          'K
                                                                          2)))))
                                                 'K)
                                           (LIST 'TIMES 'K
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'K 2))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCNC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'X 2)
                                                 1))
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'EXPT 'K 2)
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K 2))
                                                       (LIST 'EXPT 'X 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCNC 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASEC 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'X 2)
                                                                   1))
                                                       (LIST 'TIMES 'X
                                                             (LIST 'SQRT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'EXPT
                                                                          'K 2)
                                                                         (LIST
                                                                          'TIMES
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           1
                                                                           (LIST
                                                                            'EXPT
                                                                            'K
                                                                            2))
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2))))
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K
                                                                         2))))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCSC (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1 (LIST 'EXPT 'X 2)))
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1
                                                 (LIST 'TIMES
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K 2))
                                                       (LIST 'EXPT 'X 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCSC 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ATAN 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'QUOTIENT 'X
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'PLUS
                                                                          1
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2))
                                                                         (LIST
                                                                          'PLUS
                                                                          1
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           (LIST
                                                                            'EXPT
                                                                            'X
                                                                            2)
                                                                           (LIST
                                                                            'TIMES
                                                                            (LIST
                                                                             'EXPT
                                                                             'K
                                                                             2)
                                                                            (LIST
                                                                             'EXPT
                                                                             'X
                                                                             2)))))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCNS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'EXPT 'X 2) 1))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'X 2)
                                                             1)
                                                       (LIST 'EXPT 'K 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCNS 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'CSC 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         'X 2)
                                                                   1))
                                                       (LIST 'TIMES 'X
                                                             (LIST 'SQRT
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'EXPT
                                                                          'X 2)
                                                                    (LIST 'EXPT
                                                                          'K
                                                                          2)))
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         'K
                                                                         2))))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCDS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'PLUS (LIST 'EXPT 'X 2)
                                                       (LIST 'EXPT 'K 2)))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'EXPT 'X 2)
                                                             1)
                                                       (LIST 'EXPT 'K 2))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCDS 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ASIN
                                                             (LIST 'QUOTIENT 1
                                                                   (LIST 'SQRT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2)))))
                                                       'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'QUOTIENT 'X
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2))
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           (LIST
                                                                            'EXPT
                                                                            'X
                                                                            2)
                                                                           1)
                                                                          (LIST
                                                                           'EXPT
                                                                           'K
                                                                           2)))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'ARCCS (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'Y))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'DF 'X 'Y)
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'PLUS 1
                                                       (LIST 'EXPT 'X 2)))
                                           (LIST 'SQRT
                                                 (LIST 'PLUS (LIST 'EXPT 'X 2)
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))
                         (LIST 'TIMES (LIST 'DF 'K 'Y)
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT (LIST 'ARCCS 'X 'K)
                                                 'K))
                                     (LIST 'DIFFERENCE
                                           (LIST 'QUOTIENT
                                                 (LIST 'ELLIPTICE
                                                       (LIST 'ACOT 'X) 'K)
                                                 (LIST 'TIMES 'K
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2))))
                                           (LIST 'TIMES 'K
                                                 (LIST 'QUOTIENT
                                                       (LIST 'QUOTIENT 'X
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          1)
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'EXPT
                                                                           'X
                                                                           2)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           1
                                                                           (LIST
                                                                            'EXPT
                                                                            'K
                                                                            2))))))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT 'K
                                                                   2)))))))))
             (LIST 'REPLACEBY (LIST 'ARCSN (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ASN 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCSC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ASC 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCSD (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ASD 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCNS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ANS 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCCS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ACS 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCDS (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ADS 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCCN (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ACN 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCNC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ANC 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCDN (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ADN 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCND (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_AND 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCCD (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ACD 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))
             (LIST 'REPLACEBY (LIST 'ARCDC (LIST '~ 'X) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'NUM_ADC 'X 'K)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'X)
                               (LIST 'NUMBERP 'K))))))) 
(LET '(INVJACOBIRULES)) 
(PUT 'N_ASN 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ASN) 'OPFN) 
(PUT 'N_ASN 'DEFINED-ON-LINE '239) 
(PUT 'N_ASN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ASN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ASN (X K)
    (LIST 'TIMES X
          (LIST 'RF 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))
                (LIST 'DIFFERENCE 1
                      (LIST 'TIMES (LIST 'EXPT K 2) (LIST 'EXPT X 2)))))) 
(PUT 'N_ANS 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ANS) 'OPFN) 
(PUT 'N_ANS 'DEFINED-ON-LINE '242) 
(PUT 'N_ANS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ANS (X K)
    (LIST 'QUOTIENT
          (LIST 'RF 1 (LIST 'DIFFERENCE 1 (LIST 'QUOTIENT 1 (LIST 'EXPT X 2)))
                (LIST 'DIFFERENCE 1
                      (LIST 'QUOTIENT (LIST 'EXPT K 2) (LIST 'EXPT X 2))))
          X)) 
(PUT 'N_ASC 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ASC) 'OPFN) 
(PUT 'N_ASC 'DEFINED-ON-LINE '245) 
(PUT 'N_ASC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ASC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ASC (X K)
    (LIST 'TIMES X
          (LIST 'RF 1 (LIST 'PLUS 1 (LIST 'EXPT X 2))
                (LIST 'PLUS 1
                      (LIST 'TIMES (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                            (LIST 'EXPT X 2)))))) 
(PUT 'N_ACS 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ACS) 'OPFN) 
(PUT 'N_ACS 'DEFINED-ON-LINE '248) 
(PUT 'N_ACS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ACS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ACS (X K)
    (LIST 'QUOTIENT
          (LIST 'RF 1 (LIST 'PLUS 1 (LIST 'QUOTIENT 1 (LIST 'EXPT X 2)))
                (LIST 'PLUS 1
                      (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                            (LIST 'EXPT X 2))))
          X)) 
(PUT 'N_ASD 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ASD) 'OPFN) 
(PUT 'N_ASD 'DEFINED-ON-LINE '251) 
(PUT 'N_ASD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ASD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ASD (X K)
    (LIST 'TIMES X
          (LIST 'RF 1
                (LIST 'PLUS 1 (LIST 'TIMES (LIST 'EXPT K 2) (LIST 'EXPT X 2)))
                (LIST 'DIFFERENCE 1
                      (LIST 'TIMES (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                            (LIST 'EXPT X 2)))))) 
(PUT 'N_ADS 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ADS) 'OPFN) 
(PUT 'N_ADS 'DEFINED-ON-LINE '254) 
(PUT 'N_ADS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ADS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ADS (X K)
    (LIST 'QUOTIENT
          (LIST 'RF 1
                (LIST 'PLUS 1
                      (LIST 'QUOTIENT (LIST 'EXPT K 2) (LIST 'EXPT X 2)))
                (LIST 'DIFFERENCE 1
                      (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                            (LIST 'EXPT X 2))))
          X)) 
(PUT 'N_ACN 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ACN) 'OPFN) 
(PUT 'N_ACN 'DEFINED-ON-LINE '257) 
(PUT 'N_ACN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ACN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ACN (X K)
    (PROG (W Y)
      (SETQ W (AEVAL (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1
                           (LIST 'DIFFERENCE 1
                                 (LIST 'TIMES (LIST 'EXPT K 2) W))))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'N_ANC 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ANC) 'OPFN) 
(PUT 'N_ANC 'DEFINED-ON-LINE '266) 
(PUT 'N_ANC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ANC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ANC (X K)
    (PROG (W Y)
      (SETQ W (AEVAL (LIST 'DIFFERENCE (LIST 'EXPT X 2) 1)))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1
                           (LIST 'PLUS 1
                                 (LIST 'TIMES
                                       (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                                       W))))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'N_ADN 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ADN) 'OPFN) 
(PUT 'N_ADN 'DEFINED-ON-LINE '275) 
(PUT 'N_ADN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ADN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ADN (X K)
    (PROG (W Y)
      (SETQ W
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))
                     (LIST 'EXPT K 2))))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1 (LIST 'DIFFERENCE 1 W)))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'TIMES 2 'I (LIST '|ELLIPTICK'| K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'N_AND 'NUMBER-OF-ARGS 2) 
(FLAG '(N_AND) 'OPFN) 
(PUT 'N_AND 'DEFINED-ON-LINE '284) 
(PUT 'N_AND 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_AND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_AND (X K)
    (PROG (W Y)
      (SETQ W
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE (LIST 'EXPT X 2) 1)
                     (LIST 'EXPT K 2))))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1
                           (LIST 'DIFFERENCE 1
                                 (LIST 'TIMES
                                       (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))
                                       W))))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL
          (LIST 'DIFFERENCE (LIST 'TIMES 2 'I (LIST '|ELLIPTICK'| K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'N_ACD 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ACD) 'OPFN) 
(PUT 'N_ACD 'DEFINED-ON-LINE '293) 
(PUT 'N_ACD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ACD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ACD (X K)
    (PROG (W Y)
      (SETQ W
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))
                     (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1
                           (LIST 'PLUS 1 (LIST 'TIMES (LIST 'EXPT K 2) W))))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'N_ADC 'NUMBER-OF-ARGS 2) 
(FLAG '(N_ADC) 'OPFN) 
(PUT 'N_ADC 'DEFINED-ON-LINE '302) 
(PUT 'N_ADC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'N_ADC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_ADC (X K)
    (PROG (W Y)
      (SETQ W
              (AEVAL
               (LIST 'QUOTIENT (LIST 'DIFFERENCE (LIST 'EXPT X 2) 1)
                     (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (SETQ Y
              (AEVAL
               (LIST 'TIMES (LIST 'SQRT W)
                     (LIST 'RF (LIST 'EXPT X 2) 1 (LIST 'PLUS 1 W)))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART X)) 0)
        (RETURN
         (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES 2 (LIST 'ELLIPTICK K)) Y))))
       (T (RETURN (AEVAL Y)))))) 
(PUT 'NUM_ASN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ASN) 'OPFN) 
(PUT 'NUM_ASN 'DEFINED-ON-LINE '316) 
(PUT 'NUM_ASN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ASN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ASN (X K)
    (COND ((EVALEQUAL (AEVAL X) 0) 0)
          ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ASIN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'ATANH X)))
          (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM2_ASN X K))))) 
(PUT 'NUM2_ASN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM2_ASN) 'OPFN) 
(PUT 'NUM2_ASN 'DEFINED-ON-LINE '322) 
(PUT 'NUM2_ASN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM2_ASN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM2_ASN (X K)
    (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASN X K) (LIST 'NUM_ELLK K)
          (LIST 'TIMES 'I
                (LIST 'NUM_ELLK
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
          'ODD)) 
(PUT 'NUM1_ASN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ASN) 'OPFN) 
(PUT 'NUM1_ASN 'DEFINED-ON-LINE '326) 
(PUT 'NUM1_ASN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ASN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ASN (X K)
    (PROG (KX S)
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART K)) 0)
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (SETQ KX (AEVAL (LIST 'TIMES K X)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
             (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL (LIST 'ABS X)) 1)
           (COND
            ((EVALLEQ (AEVAL (LIST 'ABS KX)) 1)
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'SIGN X)
                     (LIST 'DIFFERENCE
                           (LIST 'RF 0 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)) 1)
                           (LIST 'TIMES 'I
                                 (LIST 'N_AND (LIST 'ABS X)
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'EXPT K 2))))))))))
            ((EVALGREATERP (AEVAL K) 1)
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'SIGN X)
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE
                                 (LIST 'DIFFERENCE
                                       (LIST 'RF 0 1
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'QUOTIENT 1
                                                         (LIST 'EXPT K 2))))
                                       (LIST 'N_ADC (LIST 'ABS X)
                                             (LIST 'QUOTIENT 1 K)))
                                 (LIST 'TIMES 'I
                                       (LIST 'N_ADN (LIST 'QUOTIENT 1 K)
                                             (LIST 'SQRT
                                                   (LIST 'DIFFERENCE 1
                                                         (LIST 'QUOTIENT 1
                                                               (LIST 'EXPT K
                                                                     2)))))))
                           K)))))
            (T
             (RETURN
              (AEVAL
               (LIST 'TIMES (LIST 'SIGN X)
                     (LIST 'DIFFERENCE
                           (LIST 'DIFFERENCE
                                 (LIST 'RF 0
                                       (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)) 1)
                                 (LIST 'TIMES 'I
                                       (LIST 'N_AND (LIST 'QUOTIENT 1 K)
                                             (LIST 'SQRT
                                                   (LIST 'DIFFERENCE 1
                                                         (LIST 'EXPT K 2))))))
                           (LIST 'N_ADC (LIST 'ABS KX) K)))))))))
         (COND
          ((EVALGREATERP (AEVAL (LIST 'ABS KX)) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES (LIST 'SIGN X)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'RF 0
                                     (LIST 'DIFFERENCE 1
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))
                                     1)
                               (LIST 'TIMES 'I
                                     (LIST 'N_AND (LIST 'ABS KX)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'QUOTIENT 1
                                                             (LIST 'EXPT K
                                                                   2)))))))
                         K)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES X
                   (LIST 'RF 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT KX 2))))))))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART KX)) 0)
             (EVALGREATERP (AEVAL (LIST 'ABS KX)) 1))
        (PROGN
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'REPART X))))
         (COND
          ((EVALEQUAL (AEVAL S) 0)
           (SETQ S (AEVAL (LIST 'MINUS (LIST 'SIGN (LIST 'IMPART X)))))))
         (RETURN
          (AEVAL
           (LIST 'TIMES S
                 (LIST 'QUOTIENT
                       (LIST 'DIFFERENCE
                             (LIST 'RF 0
                                   (LIST 'DIFFERENCE 1
                                         (LIST 'QUOTIENT 1 (LIST 'EXPT K 2)))
                                   1)
                             (LIST 'TIMES 'I
                                   (LIST 'N_AND (LIST 'ABS KX)
                                         (LIST 'SQRT
                                               (LIST 'DIFFERENCE 1
                                                     (LIST 'QUOTIENT 1
                                                           (LIST 'EXPT K
                                                                 2)))))))
                       K))))
         (AEVAL 'NIL)))
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
             (EVALGREATERP (AEVAL (LIST 'ABS X)) 1))
        (RETURN
         (AEVAL
          (LIST 'TIMES (LIST 'SIGN X)
                (LIST 'DIFFERENCE
                      (LIST 'RF 0 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)) 1)
                      (LIST 'TIMES 'I
                            (LIST 'N_AND (LIST 'ABS X)
                                  (LIST 'SQRT
                                        (LIST 'DIFFERENCE 1
                                              (LIST 'EXPT K 2))))))))))
       (T
        (RETURN
         (AEVAL
          (LIST 'TIMES X
                (LIST 'RF 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT X 2))
                      (LIST 'DIFFERENCE 1 (LIST 'EXPT KX 2)))))))))) 
(PUT 'NUM_ANS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ANS) 'OPFN) 
(PUT 'NUM_ANS 'DEFINED-ON-LINE '369) 
(PUT 'NUM_ANS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ANS (X K)
    (COND
     ((EVALEQUAL (AEVAL X) 0)
      (AEVAL (REDERR (REVALX "arcns not defined at the origin"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ACSC X)))
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (LIST 'ACOTH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ANS X K))))) 
(PUT 'NUM1_ANS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ANS) 'OPFN) 
(PUT 'NUM1_ANS 'DEFINED-ON-LINE '377) 
(PUT 'NUM1_ANS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ANS (X K)
    (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASN (LIST 'QUOTIENT 1 X) K)
          (LIST 'NUM_ELLK K)
          (LIST 'TIMES 'I
                (LIST 'NUM_ELLK
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
          'ODD)) 
(PUT 'NUM_ASC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ASC) 'OPFN) 
(PUT 'NUM_ASC 'DEFINED-ON-LINE '381) 
(PUT 'NUM_ASC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ASC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ASC (X K)
    (COND ((EVALEQUAL (AEVAL X) 0) 0)
          ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ATAN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'ASINH X)))
          (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM2_ASC X K))))) 
(PUT 'NUM2_ASC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM2_ASC) 'OPFN) 
(PUT 'NUM2_ASC 'DEFINED-ON-LINE '387) 
(PUT 'NUM2_ASC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM2_ASC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM2_ASC (X K)
    (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASC X K)
          (LIST 'TIMES 'I
                (LIST 'NUM_ELLK
                      (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
          (LIST 'NUM_ELLK K) 'ODD)) 
(PUT 'NUM1_ASC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ASC) 'OPFN) 
(PUT 'NUM1_ASC 'DEFINED-ON-LINE '391) 
(PUT 'NUM1_ASC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ASC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ASC (X K)
    (PROG (KPX KP AX S)
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART K)) 0)
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (SETQ KP (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'REPART X)) 0)
             (EVALEQUAL (AEVAL (LIST 'IMPART KP)) 0))
        (PROGN
         (SETQ AX (AEVAL (LIST 'ABS X)))
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'IMPART X))))
         (COND
          ((EVALLEQ (AEVAL (LIST 'TIMES AX KP)) 1)
           (COND
            ((EVALGREATERP (AEVAL AX) 1)
             (RETURN
              (AEVAL
               (LIST 'TIMES S
                     (LIST 'PLUS (LIST 'TIMES 'I (LIST 'NUM_ELLK KP))
                           (LIST 'N_AND AX K))))))
            (T
             (RETURN
              (AEVAL
               (LIST 'TIMES X
                     (LIST 'RF 1 (LIST 'DIFFERENCE 1 (LIST 'EXPT AX 2))
                           (LIST 'DIFFERENCE 1
                                 (LIST 'TIMES (LIST 'EXPT KP 2)
                                       (LIST 'EXPT AX 2)))))))))))
         (SETQ KPX (AEVAL (LIST 'TIMES KP AX)))
         (COND
          ((EVALGREATERP (AEVAL AX) 1)
           (COND
            ((EVALLESSP (AEVAL KP) 1)
             (RETURN
              (AEVAL
               (LIST 'TIMES S
                     (LIST 'PLUS
                           (LIST 'TIMES 'I
                                 (LIST 'DIFFERENCE
                                       (LIST 'RF 0 1
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'EXPT KP 2)))
                                       (LIST 'N_ADC KPX KP)))
                           (LIST 'N_AND (LIST 'QUOTIENT 1 KP) K))))))
            (T
             (RETURN
              (AEVAL
               (LIST 'TIMES S
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'TIMES 'I
                                       (LIST 'DIFFERENCE
                                             (LIST 'RF 0 1
                                                   (LIST 'DIFFERENCE 1
                                                         (LIST 'QUOTIENT 1
                                                               (LIST 'EXPT KP
                                                                     2))))
                                             (LIST 'N_ADC AX
                                                   (LIST 'QUOTIENT 1 KP))))
                                 (LIST 'N_ADN (LIST 'QUOTIENT 1 KP)
                                       (LIST 'SQRT
                                             (LIST 'DIFFERENCE 1
                                                   (LIST 'QUOTIENT 1
                                                         (LIST 'EXPT KP 2))))))
                           KP)))))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES S
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 'I
                                     (LIST 'RF 0 1
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT KP 2)))))
                               (LIST 'N_AND KPX
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'QUOTIENT 1
                                                       (LIST 'EXPT KP 2))))))
                         KP))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'REPART X)) 0)
        (PROGN
         (SETQ AX (AEVAL (LIST 'ABS X)))
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'IMPART X))))
         (COND
          ((EVALGREATERP (AEVAL AX) 1)
           (RETURN
            (AEVAL
             (LIST 'TIMES S
                   (LIST 'PLUS (LIST 'TIMES 'I (LIST 'NUM_ELLK KP))
                         (LIST 'N_AND AX K)))))))
         (AEVAL 'NIL))))
      (SETQ KPX (AEVAL (LIST 'TIMES KP X)))
      (COND
       ((EVALNEQ (AEVAL (LIST 'REPART X)) 0)
        (COND
         ((AND (EVALEQUAL (AEVAL (LIST 'REPART KPX)) 0)
               (EVALGREATERP (AEVAL (LIST 'ABS KPX)) 1))
          (RETURN
           (AEVAL
            (LIST 'QUOTIENT
                  (LIST 'DIFFERENCE
                        (LIST 'TIMES 'I (LIST 'NUM_ELLK (LIST 'QUOTIENT 1 KP)))
                        (LIST 'N_AND (LIST 'IMPART KPX)
                              (LIST 'SQRT
                                    (LIST 'DIFFERENCE 1
                                          (LIST 'QUOTIENT 1
                                                (LIST 'EXPT KP 2))))))
                  KP)))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES X
              (LIST 'RF 1 (LIST 'PLUS 1 (LIST 'EXPT X 2))
                    (LIST 'PLUS 1 (LIST 'EXPT KPX 2)))))))) 
(PUT 'NUM_ACS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ACS) 'OPFN) 
(PUT 'NUM_ACS 'DEFINED-ON-LINE '439) 
(PUT 'NUM_ACS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ACS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ACS (X K)
    (COND
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (COND
       ((EVALEQUAL (AEVAL X) 0)
        (AEVAL
         (REDERR
          (REVALX
           "arccs not defined at the origin when the modulus is a unit"))))
       (T (AEVAL (LIST 'ACSCH X)))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ACOT X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ACS X K))))) 
(PUT 'NUM1_ACS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ACS) 'OPFN) 
(PUT 'NUM1_ACS 'DEFINED-ON-LINE '448) 
(PUT 'NUM1_ACS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ACS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ACS (X K)
    (PROG (QP1 QP2)
      (SETQ QP2 (AEVAL (LIST 'NUM_ELLK K)))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL QP2))))
      (SETQ QP1
              (AEVAL
               (LIST 'NUM_ELLK
                     (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))))
      (RETURN
       (AEVAL
        (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASC (LIST 'QUOTIENT 1 X) K)
              (LIST 'TIMES 'I QP1) QP2 'ARCCS))))) 
(PUT 'NUM_ASD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ASD) 'OPFN) 
(PUT 'NUM_ASD 'DEFINED-ON-LINE '456) 
(PUT 'NUM_ASD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ASD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ASD (X K)
    (COND ((EVALEQUAL (AEVAL X) 0) 0)
          ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ASIN X)))
          ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
           (AEVAL (LIST 'ASINH X)))
          (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM2_ASD X K))))) 
(PUT 'NUM2_ASD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM2_ASD) 'OPFN) 
(PUT 'NUM2_ASD 'DEFINED-ON-LINE '462) 
(PUT 'NUM2_ASD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM2_ASD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM2_ASD (X K)
    (LIST 'WHEREEXP
          (LIST 'LIST (LIST 'REPLACEBY 'QP1 (LIST 'NUM_ELLK K))
                (LIST 'REPLACEBY 'QP2
                      (LIST 'NUM_ELLK
                            (LIST 'SQRT
                                  (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2))))))
          (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASD X K) 'QP1
                (LIST 'PLUS 'QP1 (LIST 'TIMES 'I 'QP2)) 'ODD))) 
(PUT 'NUM1_ASD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ASD) 'OPFN) 
(PUT 'NUM1_ASD 'DEFINED-ON-LINE '466) 
(PUT 'NUM1_ASD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ASD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ASD (X K)
    (PROG (KP AX AK KX KPX S)
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART K)) 0)
        (SETQ K (AEVAL (LIST 'MINUS K)))))
      (SETQ KP (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART X)) 0)
             (EVALEQUAL (AEVAL (LIST 'REPART K)) 0))
        (PROGN
         (SETQ AK (AEVAL (LIST 'ABS K)))
         (SETQ AX (AEVAL (LIST 'ABS X)))
         (COND
          ((EVALLEQ (AEVAL AX) (AEVAL (LIST 'QUOTIENT 1 KP)))
           (RETURN
            (AEVAL
             (LIST 'TIMES X
                   (LIST 'RF 1
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'EXPT K 2) (LIST 'EXPT X 2)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES (LIST 'EXPT KP 2)
                                     (LIST 'EXPT X 2))))))))
          ((EVALLEQ (AEVAL AX) (AEVAL (LIST 'QUOTIENT 1 AK)))
           (RETURN
            (AEVAL
             (LIST 'TIMES (LIST 'SIGN X)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'NUM_ELLK (LIST 'QUOTIENT AK KP))
                               (LIST 'TIMES 'I
                                     (LIST 'N_AND (LIST 'TIMES KP AX)
                                           (LIST 'QUOTIENT 1 KP))))
                         KP)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES (LIST 'SIGN X)
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'NUM_ELLK (LIST 'QUOTIENT AK KP))
                               (LIST 'DIFFERENCE
                                     (LIST 'N_ADC (LIST 'TIMES AX AK)
                                           (LIST 'QUOTIENT AK KP))
                                     (LIST 'TIMES 'I
                                           (LIST 'NUM_ELLK
                                                 (LIST 'QUOTIENT 1 KP)))))
                         KP)))))))))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'REPART X)) 0)
             (EVALEQUAL (AEVAL (LIST 'IMPART K)) 0) (EVALGREATERP (AEVAL K) 1))
        (PROGN
         (SETQ AX (AEVAL (LIST 'ABS X)))
         (SETQ KP (AEVAL (LIST 'ABS KP)))
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'IMPART X))))
         (COND
          ((EVALLEQ (AEVAL AX) (AEVAL (LIST 'QUOTIENT 1 K)))
           (RETURN
            (AEVAL
             (LIST 'TIMES X
                   (LIST 'RF 1
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'EXPT K 2) (LIST 'EXPT X 2)))
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'EXPT KP 2)
                                     (LIST 'EXPT X 2))))))))
          ((EVALLEQ (AEVAL AX) (AEVAL (LIST 'QUOTIENT 1 KP)))
           (RETURN
            (AEVAL
             (LIST 'TIMES S
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES 'I
                                     (LIST 'NUM_ELLK (LIST 'QUOTIENT KP K)))
                               (LIST 'N_AND (LIST 'TIMES K AX)
                                     (LIST 'QUOTIENT 1 K)))
                         K)))))
          (T
           (RETURN
            (AEVAL
             (LIST 'TIMES S
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'TIMES 'I
                                     (LIST 'DIFFERENCE
                                           (LIST 'NUM_ELLK
                                                 (LIST 'QUOTIENT KP K))
                                           (LIST 'N_ADC (LIST 'TIMES KP AX)
                                                 (LIST 'QUOTIENT KP K))))
                               (LIST 'NUM_ELLK (LIST 'QUOTIENT 1 K)))
                         K))))))
         (AEVAL 'NIL))))
      (SETQ KX (AEVAL (LIST 'TIMES K X)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'REPART KX)) 0)
             (EVALGREATERP (AEVAL (LIST 'ABS KX)) 1))
        (PROGN
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'REPART X))))
         (COND
          ((EVALNEQ (AEVAL S) (AEVAL (LIST 'SIGN (LIST 'IMPART X))))
           (SETQ S (AEVAL (LIST 'MINUS S)))))
         (RETURN
          (AEVAL
           (LIST 'TIMES S
                 (LIST 'PLUS
                       (LIST 'TIMES 'I
                             (LIST 'QUOTIENT
                                   (LIST 'NUM_ELLK
                                         (LIST 'TIMES 'I
                                               (LIST 'QUOTIENT KP K)))
                                   K))
                       (LIST 'N_ANC (LIST 'ABS KX) K)))))
         (AEVAL 'NIL))))
      (SETQ KPX (AEVAL (LIST 'TIMES KP X)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'IMPART KPX)) 0)
             (EVALGREATERP (AEVAL (LIST 'ABS KPX)) 1))
        (PROGN
         (SETQ S (AEVAL (LIST 'SIGN (LIST 'REPART X))))
         (RETURN
          (AEVAL
           (LIST 'TIMES S
                 (LIST 'DIFFERENCE
                       (LIST 'QUOTIENT
                             (LIST 'NUM_ELLK
                                   (LIST 'TIMES 'I (LIST 'QUOTIENT K KP)))
                             KP)
                       (LIST 'TIMES 'I (LIST 'N_ANC (LIST 'ABS KPX) KP))))))
         (AEVAL 'NIL))))
      (RETURN
       (AEVAL
        (LIST 'TIMES X
              (LIST 'RF 1 (LIST 'PLUS 1 (LIST 'EXPT KX 2))
                    (LIST 'DIFFERENCE 1 (LIST 'EXPT KPX 2)))))))) 
(PUT 'NUM_ADS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ADS) 'OPFN) 
(PUT 'NUM_ADS 'DEFINED-ON-LINE '515) 
(PUT 'NUM_ADS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ADS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ADS (X K)
    (COND
     ((AND (EVALEQUAL (AEVAL X) 0)
           (OR (EVALEQUAL (AEVAL K) 0) (EVALEQUAL (AEVAL K) 1)
               (EVALEQUAL (AEVAL K) (MINUS 1))))
      (AEVAL
       (REDERR
        (REVALX
         "arcds not defined at origin when the modulus is zero or a unit"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ACSC X)))
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (LIST 'ACSCH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ADS X K))))) 
(PUT 'NUM1_ADS 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ADS) 'OPFN) 
(PUT 'NUM1_ADS 'DEFINED-ON-LINE '523) 
(PUT 'NUM1_ADS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ADS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ADS (X K)
    (PROG (QP1 QP2)
      (SETQ QP1 (AEVAL (LIST 'NUM_ELLK K)))
      (SETQ QP2
              (AEVAL
               (LIST 'PLUS QP1
                     (LIST 'TIMES 'I
                           (LIST 'NUM_ELLK
                                 (LIST 'SQRT
                                       (LIST 'DIFFERENCE 1
                                             (LIST 'EXPT K 2))))))))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL QP2)))
            (T
             (RETURN
              (AEVAL
               (LIST 'PRINCIPAL_VALUE (LIST 'NUM1_ASD (LIST 'QUOTIENT 1 X) K)
                     QP1 QP2 'ODD))))))) 
(PUT 'NUM_ACN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ACN) 'OPFN) 
(PUT 'NUM_ACN 'DEFINED-ON-LINE '531) 
(PUT 'NUM_ACN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ACN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ACN (X K)
    (COND
     ((AND (EVALEQUAL (AEVAL X) 0)
           (OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))))
      (AEVAL
       (REDERR (REVALX "arccn not defined at the origin with unit modulus"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ACOS X)))
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (LIST 'ASECH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ACN X K))))) 
(PUT 'NUM1_ACN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ACN) 'OPFN) 
(PUT 'NUM1_ACN 'DEFINED-ON-LINE '540) 
(PUT 'NUM1_ACN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ACN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ACN (X K)
    (PROG (QP1 QP2 KP)
      (SETQ QP1 (AEVAL (LIST 'NUM_ELLK K)))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL QP1))))
      (SETQ KP (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (SETQ QP2 (AEVAL (LIST 'PLUS QP1 (LIST 'TIMES 'I (LIST 'NUM_ELLK KP)))))
      (RETURN
       (AEVAL
        (LIST 'PRINCIPAL_VALUE
              (LIST 'DIFFERENCE QP1 (LIST 'NUM1_ASD (LIST 'QUOTIENT X KP) K))
              QP1 QP2 'EVEN))))) 
(PUT 'NUM_ANC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ANC) 'OPFN) 
(PUT 'NUM_ANC 'DEFINED-ON-LINE '549) 
(PUT 'NUM_ANC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ANC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ANC (X K)
    (COND
     ((AND (EVALEQUAL (AEVAL X) 0)
           (OR (EVALEQUAL (AEVAL K) 0) (EVALEQUAL (AEVAL K) 1)
               (EVALEQUAL (AEVAL K) (MINUS 1))))
      (AEVAL
       (REDERR
        (REVALX "arcnc not defined at the origin with zero or unit modulus"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ASEC X)))
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (LIST 'ACOSH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ANC X K))))) 
(PUT 'NUM1_ANC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ANC) 'OPFN) 
(PUT 'NUM1_ANC 'DEFINED-ON-LINE '557) 
(PUT 'NUM1_ANC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ANC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ANC (X K)
    (PROG (QP1 QP2 KP)
      (SETQ KP (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (SETQ QP2 (AEVAL (LIST 'TIMES 'I (LIST 'NUM_ELLK KP))))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL QP2))))
      (SETQ QP1 (AEVAL (LIST 'NUM_ELLK K)))
      (RETURN
       (AEVAL
        (LIST 'PRINCIPAL_VALUE
              (LIST 'DIFFERENCE QP1
                    (LIST 'NUM1_ASD (LIST 'QUOTIENT 1 (LIST 'TIMES X KP)) K))
              QP1 (LIST 'PLUS QP1 QP2) 'EVEN))))) 
(PUT 'NUM_ADN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ADN) 'OPFN) 
(PUT 'NUM_ADN 'DEFINED-ON-LINE '567) 
(PUT 'NUM_ADN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ADN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ADN (X K)
    (COND
     ((EVALEQUAL (AEVAL K) 0)
      (AEVAL (REDERR (REVALX "arcdn not defined for zero modulus"))))
     ((AND (EVALEQUAL (AEVAL X) 0)
           (OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))))
      (AEVAL
       (REDERR (REVALX "arcdn not defined at the origin for unit modulus"))))
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (LIST 'ASECH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ADN X K))))) 
(PUT 'NUM1_ADN 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ADN) 'OPFN) 
(PUT 'NUM1_ADN 'DEFINED-ON-LINE '576) 
(PUT 'NUM1_ADN 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ADN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ADN (X K)
    (PROG (QP1 QP2 KP)
      (SETQ KP (AEVAL (LIST 'SQRT (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
      (SETQ QP1 (AEVAL (LIST 'TIMES 'I (LIST 'NUM_ELLK KP))))
      (SETQ QP2 (AEVAL (LIST 'NUM_ELLK K)))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL (LIST 'PLUS QP1 QP2))))
            (T
             (RETURN
              (AEVAL
               (LIST 'PRINCIPAL_VALUE
                     (LIST 'PLUS QP1
                           (LIST 'DIFFERENCE QP2
                                 (LIST 'NUM1_ASC
                                       (LIST 'TIMES 'I (LIST 'QUOTIENT X KP))
                                       K)))
                     QP1 QP2 'ARCDN))))))) 
(PUT 'NUM_AND 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_AND) 'OPFN) 
(PUT 'NUM_AND 'DEFINED-ON-LINE '586) 
(PUT 'NUM_AND 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_AND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_AND (X K)
    (COND
     ((EVALEQUAL (AEVAL K) 0)
      (AEVAL (REDERR (REVALX "arcnd not defined for zero modulus"))))
     ((AND (EVALNEQ (AEVAL X) 0)
           (OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1))))
      (AEVAL (LIST 'ACOSH X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_AND X K))))) 
(PUT 'NUM1_AND 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_AND) 'OPFN) 
(PUT 'NUM1_AND 'DEFINED-ON-LINE '593) 
(PUT 'NUM1_AND 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_AND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_AND (X K)
    (PROG (KP)
      (SETQ KP
              (AEVAL
               (LIST 'TIMES 'I
                     (LIST 'NUM_ELLK
                           (LIST 'SQRT
                                 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL KP)))
            (T
             (RETURN
              (AEVAL
               (LIST 'PRINCIPAL_VALUE
                     (LIST 'PLUS KP (LIST 'NUM1_ASC (LIST 'QUOTIENT X 'I) K))
                     KP (LIST 'NUM_ELLK K) 'ARCDN))))))) 
(PUT 'NUM_ACD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ACD) 'OPFN) 
(PUT 'NUM_ACD 'DEFINED-ON-LINE '601) 
(PUT 'NUM_ACD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ACD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ACD (X K)
    (COND
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (REDERR (REVALX "arccd is not defined for unit modulus"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ACOS X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ACD X K))))) 
(PUT 'NUM1_ACD 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ACD) 'OPFN) 
(PUT 'NUM1_ACD 'DEFINED-ON-LINE '608) 
(PUT 'NUM1_ACD 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ACD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ACD (X K)
    (PROG (EK)
      (SETQ EK (AEVAL (LIST 'NUM_ELLK K)))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL EK)))
            (T
             (RETURN
              (AEVAL
               (LIST 'PRINCIPAL_VALUE
                     (LIST 'DIFFERENCE EK (LIST 'NUM1_ASN X K)) EK
                     (LIST 'TIMES 'I
                           (LIST 'NUM_ELLK
                                 (LIST 'SQRT
                                       (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))
                     'EVEN))))))) 
(PUT 'NUM_ADC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_ADC) 'OPFN) 
(PUT 'NUM_ADC 'DEFINED-ON-LINE '616) 
(PUT 'NUM_ADC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM_ADC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_ADC (X K)
    (COND
     ((OR (EVALEQUAL (AEVAL K) 1) (EVALEQUAL (AEVAL K) (MINUS 1)))
      (AEVAL (REDERR (REVALX "arcdc is not defined for unit modulus"))))
     ((AND (EVALEQUAL (AEVAL X) 0) (EVALEQUAL (AEVAL K) 0))
      (AEVAL
       (REDERR (REVALX "arcdc not defined at the origin for zero modulus"))))
     ((EVALEQUAL (AEVAL K) 0) (AEVAL (LIST 'ASEC X)))
     (T (AEVAL (LIST 'NUM_ELLIPTIC 'NUM1_ADC X K))))) 
(PUT 'NUM1_ADC 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_ADC) 'OPFN) 
(PUT 'NUM1_ADC 'DEFINED-ON-LINE '625) 
(PUT 'NUM1_ADC 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'NUM1_ADC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_ADC (X K)
    (PROG (QP1 QP2)
      (SETQ QP1 (AEVAL (LIST 'NUM_ELLK K)))
      (SETQ QP2
              (AEVAL
               (LIST 'TIMES 'I
                     (LIST 'NUM_ELLK
                           (LIST 'SQRT
                                 (LIST 'DIFFERENCE 1 (LIST 'EXPT K 2)))))))
      (COND ((EVALEQUAL (AEVAL X) 0) (RETURN (AEVAL (LIST 'PLUS QP1 QP2))))
            (T
             (RETURN
              (AEVAL
               (LIST 'PRINCIPAL_VALUE
                     (LIST 'DIFFERENCE (LIST 'NUM1_ASN (LIST 'QUOTIENT 1 X) K)
                           QP1)
                     QP1 QP2 'EVEN))))))) 
(PUT 'LATTICE_COORDS 'NUMBER-OF-ARGS 3) 
(FLAG '(LATTICE_COORDS) 'OPFN) 
(PUT 'LATTICE_COORDS 'DEFINED-ON-LINE '634) 
(PUT 'LATTICE_COORDS 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'LATTICE_COORDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LATTICE_COORDS (Z Z1 Z2)
    (LIST 'WHEREEXP
          (LIST 'LIST
                (LIST 'REPLACEBY 'D
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES (LIST 'REPART Z1) (LIST 'IMPART Z2))
                            (LIST 'TIMES (LIST 'IMPART Z1)
                                  (LIST 'REPART Z2)))))
          (LIST 'LIST
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES (LIST 'IMPART Z2) (LIST 'REPART Z))
                            (LIST 'TIMES (LIST 'REPART Z2) (LIST 'IMPART Z)))
                      'D)
                (LIST 'QUOTIENT
                      (LIST 'DIFFERENCE
                            (LIST 'TIMES (LIST 'REPART Z1) (LIST 'IMPART Z))
                            (LIST 'TIMES (LIST 'IMPART Z1) (LIST 'REPART Z)))
                      'D)))) 
(PUT 'PRINCIPAL_VALUE 'NUMBER-OF-ARGS 4) 
(FLAG '(PRINCIPAL_VALUE) 'OPFN) 
(PUT 'PRINCIPAL_VALUE 'DEFINED-ON-LINE '639) 
(PUT 'PRINCIPAL_VALUE 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'PRINCIPAL_VALUE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINCIPAL_VALUE (Z Z1 Z2 PARITY)
    (PROG (A B)
      (SETQ Z (AEVAL (LIST 'LATTICE_COORDS Z Z1 Z2)))
      (SETQ A (AEVAL (LIST 'FIRST Z)))
      (SETQ B (AEVAL (LIST 'SECOND Z)))
      (WHILE (EVALGEQ (AEVAL* A) 4) (SETQ A (AEVAL* (LIST 'DIFFERENCE A 4))))
      (WHILE (EVALLESSP (AEVAL* A) 0) (SETQ A (AEVAL* (LIST 'PLUS A 4))))
      (WHILE (EVALGREATERP (AEVAL* B) 2)
             (SETQ B (AEVAL* (LIST 'DIFFERENCE B 2))))
      (WHILE (EVALLESSP (AEVAL* B) 0) (SETQ B (AEVAL* (LIST 'PLUS B 2))))
      (COND
       ((EVALEQUAL (AEVAL PARITY) (AEVAL 'EVEN))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL A) 2)
           (PROGN
            (SETQ A (AEVAL (LIST 'DIFFERENCE 4 A)))
            (SETQ B (AEVAL (LIST 'DIFFERENCE 2 B)))
            (AEVAL 'NIL))))
         (COND
          ((EVALGREATERP (AEVAL B) 1) (SETQ B (AEVAL (LIST 'DIFFERENCE 2 B)))))
         (RETURN (AEVAL (LIST 'PLUS (LIST 'TIMES A Z1) (LIST 'TIMES B Z2))))
         (AEVAL 'NIL))))
      (COND
       ((OR (EVALEQUAL (AEVAL PARITY) (AEVAL 'ODD))
            (EVALEQUAL (AEVAL PARITY) (AEVAL 'ARCCS)))
        (PROGN
         (COND
          ((AND (EVALGREATERP (AEVAL A) 1) (EVALLESSP (AEVAL A) 3))
           (PROGN
            (SETQ A (AEVAL (LIST 'DIFFERENCE 2 A)))
            (SETQ B (AEVAL (LIST 'MINUS B)))
            (AEVAL 'NIL)))
          ((AND (EVALGREATERP (AEVAL A) 1) (EVALLESSP (AEVAL A) 4))
           (SETQ A (AEVAL (LIST 'DIFFERENCE A 4)))))
         (COND
          ((EVALGREATERP (AEVAL B) 1) (SETQ B (AEVAL (LIST 'DIFFERENCE B 2))))
          ((EVALLEQ (AEVAL B) (MINUS 1)) (SETQ B (AEVAL (LIST 'PLUS 2 B)))))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALEQUAL (AEVAL PARITY) (AEVAL 'ARCCS)) (EVALLESSP (AEVAL B) 0))
        (PROGN (SETQ B (AEVAL (LIST 'PLUS 2 B))) (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'PLUS (LIST 'TIMES A Z1) (LIST 'TIMES B Z2)))))) 
(PUT 'PRINCIPAL_VALUE 'NUMBER-OF-ARGS 4) 
(FLAG '(PRINCIPAL_VALUE) 'OPFN) 
(PUT 'PRINCIPAL_VALUE 'DEFINED-ON-LINE '696) 
(PUT 'PRINCIPAL_VALUE 'DEFINED-IN-FILE 'ELLIPFN/EFJACINV.RED) 
(PUT 'PRINCIPAL_VALUE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINCIPAL_VALUE (Z Z1 Z2 PARITY)
    (PROG (A B)
      (SETQ Z (AEVAL (LIST 'LATTICE_COORDS Z Z1 Z2)))
      (SETQ A (AEVAL (LIST 'FIRST Z)))
      (SETQ B (AEVAL (LIST 'SECOND Z)))
      (WHILE (EVALGEQ (AEVAL* A) 4) (SETQ A (AEVAL* (LIST 'DIFFERENCE A 4))))
      (WHILE (EVALLESSP (AEVAL* A) 0) (SETQ A (AEVAL* (LIST 'PLUS A 4))))
      (WHILE (EVALGREATERP (AEVAL* B) 2)
             (SETQ B (AEVAL* (LIST 'DIFFERENCE B 2))))
      (WHILE (EVALLESSP (AEVAL* B) 0) (SETQ B (AEVAL* (LIST 'PLUS B 2))))
      (COND
       ((OR (EVALEQUAL (AEVAL PARITY) (AEVAL 'EVEN))
            (EVALEQUAL (AEVAL PARITY) (AEVAL 'ARCDN)))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL A) 2)
           (PROGN
            (SETQ A (AEVAL (LIST 'DIFFERENCE 4 A)))
            (SETQ B (AEVAL (LIST 'DIFFERENCE 2 B)))
            (AEVAL 'NIL))))
         (COND
          ((AND (EVALNEQ (AEVAL PARITY) (AEVAL 'ARCDN))
                (EVALGREATERP (AEVAL B) 1))
           (SETQ B (AEVAL (LIST 'DIFFERENCE B 2)))))
         (RETURN (AEVAL (LIST 'PLUS (LIST 'TIMES A Z1) (LIST 'TIMES B Z2))))
         (AEVAL 'NIL))))
      (COND
       ((OR (EVALEQUAL (AEVAL PARITY) (AEVAL 'ODD))
            (EVALEQUAL (AEVAL PARITY) (AEVAL 'ARCCS)))
        (PROGN
         (COND
          ((AND (EVALGREATERP (AEVAL A) 1) (EVALLESSP (AEVAL A) 3))
           (PROGN
            (SETQ A (AEVAL (LIST 'DIFFERENCE 2 A)))
            (SETQ B (AEVAL (LIST 'MINUS B)))
            (AEVAL 'NIL)))
          ((AND (EVALGREATERP (AEVAL A) 1) (EVALLESSP (AEVAL A) 4))
           (SETQ A (AEVAL (LIST 'DIFFERENCE A 4)))))
         (COND
          ((EVALGREATERP (AEVAL B) 1) (SETQ B (AEVAL (LIST 'DIFFERENCE B 2))))
          ((EVALLEQ (AEVAL B) (MINUS 1)) (SETQ B (AEVAL (LIST 'PLUS 2 B)))))
         (AEVAL 'NIL))))
      (COND
       ((AND (EVALEQUAL (AEVAL PARITY) (AEVAL 'ARCCS)) (EVALLESSP (AEVAL B) 0))
        (PROGN (SETQ B (AEVAL (LIST 'PLUS 2 B))) (AEVAL 'NIL))))
      (RETURN (AEVAL (LIST 'PLUS (LIST 'TIMES A Z1) (LIST 'TIMES B Z2)))))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(PUT 'ARCSN 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcsn}") 
(PUT 'ARCCN 'FANCY-FUNCTIONSYMBOL "\\mathrm{arccn}") 
(PUT 'ARCDN 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcdn}") 
(PUT 'ARCNS 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcns}") 
(PUT 'INCNC 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcnc}") 
(PUT 'ARCND 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcnd}") 
(PUT 'ARCSC 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcsc}") 
(PUT 'ARCSD 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcsd}") 
(PUT 'ARCCD 'FANCY-FUNCTIONSYMBOL "\\mathrm{arccd}") 
(PUT 'ARCCS 'FANCY-FUNCTIONSYMBOL "\\mathrm{arccs}") 
(PUT 'ARCDS 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcds}") 
(PUT 'ARCDC 'FANCY-FUNCTIONSYMBOL "\\mathrm{arcdc}") 
(PROG (X)
  (SETQ X
          '(ARCSN ARCCN ARCDN ARCNS ARCNC ARCND ARCSC ARCSD ARCCD ARCCS ARCDS
            ARCDC))
 LAB
  (COND ((NULL X) (RETURN NIL)))
  ((LAMBDA (X) (PROGN (PUT X 'FANCY-SYMBOL-LENGTH 10))) (CAR X))
  (SETQ X (CDR X))
  (GO LAB)) 
(FLAG
 '(ARCSN ARCCN ARCDN ARCNS ARCNC ARCND ARCSC ARCSD ARCCS ARCCD ARCDS ARCDC)
 'SPECFN) 
(DEFLIST
 '((ARCSN 2) (ARCCN 2) (ARCDN 2) (ARCNS 2) (ARCNC 2) (ARCND 2) (ARCSC 2)
   (ARCSD 2) (ARCCS 2) (ARCCD 2) (ARCDS 2) (ARCDC 2))
 'NUMBER-OF-ARGS) 
(ENDMODULE) 