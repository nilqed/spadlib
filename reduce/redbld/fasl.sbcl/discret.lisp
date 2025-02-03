(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DISCRET)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(DIFMATCH (LIST 'ALL 1 0 1)) 
(DIFMATCH
 (LIST 'ALL 'U (LIST 'EQUAL 'U 'ONE) 0 (LIST 'U 'I) (LIST 'EQUAL 'U 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                   (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'DIFF 'U 'X) (LIST 'EQUAL 'U 'ONE) 2
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                   (LIST 'U (LIST 'DIFFERENCE 'I 1)))
             (LIST 'PLUS 'DIP1 'DIM1))
       (LIST 'EQUAL 'U 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                   (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
             'DI))) 
(DIFMATCH
 (LIST 'ALL (LIST 'DIFF 'U 'X 2) (LIST 'EQUAL 'U 'ONE) 0
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                               (LIST 'U 'I))
                         'DIP1)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'U 'I)
                               (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                         'DIM1))
             'DI)
       (LIST 'EQUAL 'U 'HALF) 2
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 3 2)))
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         'DIP2)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 3 2))))
                         'DIM2))
             (LIST 'PLUS 'DIP1 'DIM1)))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'U 'V) (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES (LIST 'U 'I) (LIST 'V 'I)) (LIST 'EQUAL 'U 'ONE)
       (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'V 'I))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'TIMES
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'EXPT 'U 'N) (LIST 'EQUAL 'U 'ONE) 0
       (LIST 'EXPT (LIST 'U 'I) 'N) (LIST 'EQUAL 'U 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'EXPT
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         'N)
                   (LIST 'EXPT (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         'N))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'U (LIST 'EXPT 'V 'N)) (LIST 'EQUAL 'U 'ONE)
       (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES (LIST 'U 'I) (LIST 'EXPT (LIST 'V 'I) 'N))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'EXPT
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               'N)
                         (LIST 'EXPT
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               'N))
                   2))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'EXPT (LIST 'V 'I) 'N))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'TIMES
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'EXPT
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               'N))
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'EXPT
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               'N)))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'U 'V 'W) (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE)
       (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES (LIST 'U 'I) (LIST 'V 'I) (LIST 'W 'I))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'TIMES (LIST 'U 'I) (LIST 'V 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'W (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   2))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'W 'I))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'TIMES
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'W
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
                   2))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'V 'I) (LIST 'W 'I))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'TIMES
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'W
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
                   2)
             (LIST 'V 'I))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'TIMES
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
                   2)
             (LIST 'W 'I))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'TIMES
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'W (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'V (LIST 'DIFF 'U 'X)) (LIST 'EQUAL 'U 'ONE)
       (LIST 'EQUAL 'V 'ONE) 2
       (LIST 'TIMES (LIST 'V 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 2
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES (LIST 'V 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI)))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'V 'W (LIST 'DIFF 'U 'X)) (LIST 'EQUAL 'U 'ONE)
       (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'ONE) 2
       (LIST 'TIMES (LIST 'V 'I) (LIST 'W 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'HALF) 2
       (LIST 'TIMES (LIST 'V 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'W (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'ONE) 2
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'W 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'HALF) 2
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'TIMES
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'W
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                   (LIST 'PLUS 'DIP1 'DIM1)))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES (LIST 'V 'I) (LIST 'W 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'TIMES (LIST 'V 'I)
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'W (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'W 'I)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) (LIST 'EQUAL 'W 'HALF) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'TIMES
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'W (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'W
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   2)
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   'DI)))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'X 'U) (LIST 'EQUAL 'U 'ONE) 0
       (LIST 'TIMES (LIST 'X 'I) (LIST 'U 'I)) (LIST 'EQUAL 'U 'HALF) 1
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'TIMES
                         (LIST 'X (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES (LIST 'X (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'QUOTIENT 'U (LIST 'EXPT 'X 'N)) (LIST 'EQUAL 'U 'ONE) 0
       (LIST 'QUOTIENT (LIST 'U 'I) (LIST 'EXPT (LIST 'X 'I) 'N))
       (LIST 'EQUAL 'U 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'QUOTIENT
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'EXPT
                               (LIST 'X
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               'N))
                   (LIST 'QUOTIENT
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'EXPT
                               (LIST 'X (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               'N)))
             2))) 
(DIFMATCH
 (LIST 'ALL (LIST 'TIMES 'U (LIST 'QUOTIENT 'V (LIST 'EXPT 'X 'N)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT (LIST 'V 'I) (LIST 'EXPT (LIST 'X 'I) 'N)))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'TIMES (LIST 'U 'I)
             (LIST 'QUOTIENT
                   (LIST 'QUOTIENT
                         (LIST 'PLUS
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         2)
                   (LIST 'EXPT (LIST 'X 'I) 'N)))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'TIMES
             (LIST 'QUOTIENT
                   (LIST 'PLUS
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   2)
             (LIST 'QUOTIENT (LIST 'V 'I) (LIST 'EXPT (LIST 'X 'I) 'N)))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'PLUS
                   (LIST 'TIMES
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'QUOTIENT
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'EXPT
                                     (LIST 'X
                                           (LIST 'DIFFERENCE 'I
                                                 (LIST 'QUOTIENT 1 2)))
                                     'N)))
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'QUOTIENT
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'EXPT
                                     (LIST 'X
                                           (LIST 'PLUS 'I
                                                 (LIST 'QUOTIENT 1 2)))
                                     'N))))
             2))) 
(DIFMATCH
 (LIST 'ALL
       (LIST 'QUOTIENT (LIST 'DIFF (LIST 'TIMES (LIST 'EXPT 'X 'N) 'U) 'X)
             (LIST 'EXPT 'X 'N))
       (LIST 'EQUAL 'U 'ONE) 2
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'X (LIST 'PLUS 'I 1)) 'N)
                               (LIST 'U (LIST 'PLUS 'I 1)))
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'X (LIST 'DIFFERENCE 'I 1))
                                     'N)
                               (LIST 'U (LIST 'DIFFERENCE 'I 1))))
                   (LIST 'EXPT (LIST 'X 'I) 'N))
             (LIST 'PLUS 'DIM1 'DIP1))
       (LIST 'EQUAL 'U 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'EXPT
                                     (LIST 'X
                                           (LIST 'PLUS 'I
                                                 (LIST 'QUOTIENT 1 2)))
                                     'N)
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'EXPT
                                     (LIST 'X
                                           (LIST 'DIFFERENCE 'I
                                                 (LIST 'QUOTIENT 1 2)))
                                     'N)
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   'DI)
             (LIST 'EXPT (LIST 'X 'I) 'N)))) 
(DIFMATCH
 (LIST 'ALL (LIST 'DIFF (LIST 'TIMES 'U 'V) 'X) (LIST 'EQUAL 'U 'ONE)
       (LIST 'EQUAL 'V 'ONE) 4
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I 1))
                         (LIST 'V (LIST 'PLUS 'I 1)))
                   (LIST 'TIMES (LIST 'U (LIST 'DIFFERENCE 'I 1))
                         (LIST 'V (LIST 'DIFFERENCE 'I 1))))
             (LIST 'PLUS 'DIM1 'DIP1))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 2
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'U (LIST 'PLUS 'I 1))
                                     (LIST 'U 'I))
                               2)
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'U (LIST 'DIFFERENCE 'I 1))
                                     (LIST 'U 'I))
                               2)
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))))
             'DI)
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 2
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I 1))
                                     (LIST 'V 'I))
                               2)
                         (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST 'V (LIST 'DIFFERENCE 'I 1))
                                     (LIST 'V 'I))
                               2)
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))))
             'DI)
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                   (LIST 'TIMES
                         (LIST 'U (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))
                         (LIST 'V (LIST 'DIFFERENCE 'I (LIST 'QUOTIENT 1 2)))))
             'DI))) 
(DIFMATCH
 (LIST 'ALL
       (LIST 'QUOTIENT (LIST 'DIFF (LIST 'TIMES 'U 'V) 'X) (LIST 'EXPT 'X 'N))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) 4
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'U (LIST 'PLUS 'I 1))
                               (LIST 'V (LIST 'PLUS 'I 1)))
                         (LIST 'TIMES (LIST 'U (LIST 'DIFFERENCE 'I 1))
                               (LIST 'V (LIST 'DIFFERENCE 'I 1))))
                   (LIST 'EXPT (LIST 'X 'I) 'N))
             (LIST 'PLUS 'DIM1 'DIP1))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 2
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'U (LIST 'PLUS 'I 1))
                                           (LIST 'U 'I))
                                     2)
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'U (LIST 'DIFFERENCE 'I 1))
                                           (LIST 'U 'I))
                                     2)
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   (LIST 'EXPT (LIST 'X 'I) 'N))
             'DI)
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 2
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'V (LIST 'PLUS 'I 1))
                                           (LIST 'V 'I))
                                     2)
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'V (LIST 'DIFFERENCE 'I 1))
                                           (LIST 'V 'I))
                                     2)
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   (LIST 'EXPT (LIST 'X 'I) 'N))
             'DI)
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 0
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'V (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2))))
                         (LIST 'TIMES
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'V
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))))
                   (LIST 'EXPT (LIST 'X 'I) 'N))
             'DI))) 
(DIFMATCH
 (LIST 'ALL
       (LIST 'QUOTIENT (LIST 'DIFF (LIST 'TIMES 'U (LIST 'DIFF 'V 'X)) 'X)
             (LIST 'EXPT 'X 'N))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'ONE) 0
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V (LIST 'PLUS 'I 1))
                                           (LIST 'V 'I))
                                     'DIP1))
                         (LIST 'TIMES
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE (LIST 'V 'I)
                                           (LIST 'V (LIST 'DIFFERENCE 'I 1)))
                                     'DIM1)))
                   'DI)
             (LIST 'EXPT (LIST 'X 'I) 'N))
       (LIST 'EQUAL 'U 'HALF) (LIST 'EQUAL 'V 'HALF) 2
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'U (LIST 'PLUS 'I (LIST 'QUOTIENT 1 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V
                                                 (LIST 'PLUS 'I
                                                       (LIST 'QUOTIENT 3 2)))
                                           (LIST 'V
                                                 (LIST 'DIFFERENCE 'I
                                                       (LIST 'QUOTIENT 1 2))))
                                     (LIST 'PLUS 'DI 'DIP2)))
                         (LIST 'TIMES
                               (LIST 'U
                                     (LIST 'DIFFERENCE 'I
                                           (LIST 'QUOTIENT 1 2)))
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V
                                                 (LIST 'PLUS 'I
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'V
                                                 (LIST 'DIFFERENCE 'I
                                                       (LIST 'QUOTIENT 3 2))))
                                     (LIST 'PLUS 'DI 'DIM2))))
                   'DI)
             (LIST 'EXPT (LIST 'X 'I) 'N))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'ONE) 2
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'U (LIST 'PLUS 'I 1))
                                           (LIST 'U 'I))
                                     2)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V (LIST 'PLUS 'I 1))
                                           (LIST 'V 'I))
                                     'DIP1))
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'U 'I)
                                           (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                                     2)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE (LIST 'V 'I)
                                           (LIST 'V (LIST 'DIFFERENCE 'I 1)))
                                     'DIM1)))
                   'DI)
             (LIST 'EXPT (LIST 'X 'I) 'N))
       (LIST 'EQUAL 'U 'ONE) (LIST 'EQUAL 'V 'HALF) 4
       (LIST 'QUOTIENT
             (LIST 'QUOTIENT
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'U (LIST 'PLUS 'I 1))
                                           (LIST 'U 'I))
                                     2)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V
                                                 (LIST 'PLUS 'I
                                                       (LIST 'QUOTIENT 3 2)))
                                           (LIST 'V
                                                 (LIST 'DIFFERENCE 'I
                                                       (LIST 'QUOTIENT 1 2))))
                                     (LIST 'PLUS 'DI 'DIP2)))
                         (LIST 'TIMES
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS (LIST 'U 'I)
                                           (LIST 'U (LIST 'DIFFERENCE 'I 1)))
                                     2)
                               (LIST 'QUOTIENT
                                     (LIST 'DIFFERENCE
                                           (LIST 'V
                                                 (LIST 'PLUS 'I
                                                       (LIST 'QUOTIENT 1 2)))
                                           (LIST 'V
                                                 (LIST 'DIFFERENCE 'I
                                                       (LIST 'QUOTIENT 3 2))))
                                     (LIST 'PLUS 'DI 'DIM2))))
                   'DI)
             (LIST 'EXPT (LIST 'X 'I) 'N)))) 
(ENDMODULE) 