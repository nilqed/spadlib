(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTE)) 
(SETK 'LAPLACE2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'LAPLACE2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'LAPLACE2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'F1
                               (LIST 'EXPT 'E
                                     (LIST 'MINUS (LIST 'TIMES 'S 'X))))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(LAPLACE2_RULES))) 
(SETK 'HANKELT_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST 'QUOTIENT 1 (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT (LIST 'TIMES (LIST 'SQRT 'X) 'X) 'F1
                         (LIST 'BESSELJ 'N
                               (LIST 'TIMES 2
                                     (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                           (LIST 'QUOTIENT 1 2))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'HANKELT (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'HANKELT (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'F1
                               (LIST 'BESSELJ 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(HANKELT_RULES))) 
(SETK 'Y_TRANSFORM2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'Y_TRANSFORM2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'Y_TRANSFORM2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'F1
                               (LIST 'BESSELY 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(Y_TRANSFORM2_RULES))) 
(SETK 'K_TRANSFORM2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'K_TRANSFORM2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'K_TRANSFORM2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'F1
                               (LIST 'BESSELK 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(K_TRANSFORM2_RULES))) 
(SETK 'STRUVEH2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'STRUVEH2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'STRUVEH2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'F1
                               (LIST 'STRUVEH 'N
                                     (LIST 'TIMES 2
                                           (LIST 'EXPT (LIST 'TIMES 'S 'X)
                                                 (LIST 'QUOTIENT 1 2)))))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(STRUVEH2_RULES))) 
(SETK 'FOURIER_SIN2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1 (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1 (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1 (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_SIN2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1 (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'FOURIER_SIN2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT (LIST 'TIMES 'F1 (LIST 'SIN (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(FOURIER_SIN2_RULES))) 
(SETK 'FOURIER_COS2_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2 (LIST 'QUOTIENT 1 (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'X) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'EXPT 'X 'A)) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'SQRT 'X)) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST '~ 'X)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X) 'X))
                               'F1 (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES (LIST 'SQRT 'X)
                                           (LIST 'EXPT 'X 'A)))
                               'F1 (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2 (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2 (LIST '~ 'X) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'X 'F1 (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2 (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'F1)
                         (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X)) (LIST '~ 'X))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) 'X 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2
                         (LIST 'TIMES (LIST 'SQRT (LIST '~ 'X))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'A)))
                         (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'EXPT 'X 'A) 'F1
                               (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY
                   (LIST 'FOURIER_COS2 (LIST '~ 'B) (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT
                         (LIST 'TIMES 'B 'F1 (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))
             (LIST 'REPLACEBY (LIST 'FOURIER_COS2 (LIST '~ 'F1) (LIST '~ 'X))
                   (LIST 'INT (LIST 'TIMES 'F1 (LIST 'COS (LIST 'TIMES 'S 'X)))
                         'X 0 'INFINITY))))) 
(AEVAL (LET '(FOURIER_COS2_RULES))) 
(AEVAL 'NIL) 
(ENDMODULE) 