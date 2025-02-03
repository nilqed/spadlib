(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SOLVETAB)) 
(PUT 'ASIN 'INVERSE 'SIN) 
(PUT 'ACOS 'INVERSE 'COS) 
(PUT 'ATAN 'INVERSE 'TAN) 
(PUT 'ACOT 'INVERSE 'COT) 
(PUT 'ASEC 'INVERSE 'SEC) 
(PUT 'ACSC 'INVERSE 'CSC) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'SOL)) 
(FORALL
 (LIST '(A B C D X)
       '(AND (NOT (FIXP (REVALX 'C))) (RATNUMP (REVALX 'C))
             (NOT (FIXP (REVALX 'D))) (RATNUMP (REVALX 'D)))
       '(LET00
         '((EQUAL (SOL (DIFFERENCE (EXPT A C) (EXPT B D)) X)
                  (DIFFERENCE (EXPT A (TIMES C (LCM (DEN C) (DEN D))))
                              (EXPT B (TIMES D (LCM (DEN C) (DEN D)))))))))) 
(FORALL
 (LIST '(A B C D X)
       '(AND (FREEOF (REVALX 'A) (REVALX 'X)) (FREEOF (REVALX 'C) (REVALX 'X)))
       '(LET00
         '((EQUAL (SOL (DIFFERENCE (EXPT A B) (EXPT C D)) X)
                  (EXPT E (DIFFERENCE (TIMES B (LOG A)) (TIMES D (LOG C))))))))) 
(FORALL
 (LIST '(A B C D X)
       '(AND (FREEOF (REVALX 'A) (REVALX 'X)) (FREEOF (REVALX 'C) (REVALX 'X)))
       '(LET00
         '((EQUAL (SOL (PLUS (TIMES A (LOG B)) (TIMES C (LOG D))) X)
                  (DIFFERENCE (TIMES (EXPT B A) (EXPT D C)) 1)))))) 
(FORALL
 (LIST '(A B C D F X)
       '(AND (FREEOF (REVALX 'A) (REVALX 'X)) (FREEOF (REVALX 'C) (REVALX 'X)))
       '(LET00
         '((EQUAL (SOL (PLUS (TIMES A (LOG B)) (TIMES C (LOG D)) F) X)
                  (SOL (PLUS (LOG (TIMES (EXPT B A) (EXPT D C))) F) X)))))) 
(FORALL
 (LIST '(A B D F X) '(FREEOF (REVALX 'A) (REVALX 'X))
       '(LET00
         '((EQUAL (SOL (PLUS (TIMES A (LOG B)) (LOG D) F) X)
                  (SOL (PLUS (LOG (TIMES (EXPT B A) D)) F) X))
           (EQUAL (SOL (PLUS (DIFFERENCE (TIMES A (LOG B)) (LOG D)) F) X)
                  (SOL (PLUS (LOG (QUOTIENT (EXPT B A) D)) F) X)))))) 
(FORALL
 (LIST '(A B D X) '(FREEOF (REVALX 'A) (REVALX 'X))
       '(LET00
         '((EQUAL (SOL (PLUS (TIMES A (LOG B)) (LOG D)) X)
                  (DIFFERENCE (TIMES (EXPT B A) D) 1))
           (EQUAL (SOL (DIFFERENCE (TIMES A (LOG B)) (LOG D)) X)
                  (DIFFERENCE (EXPT B A) D)))))) 
(FORALL
 (LIST '(A B C X) 'T
       '(LET00
         '((EQUAL (SOL (PLUS (LOG A) (LOG B) C) X)
                  (SOL (PLUS (LOG (TIMES A B)) C) X))
           (EQUAL (SOL (PLUS (DIFFERENCE (LOG A) (LOG B)) C) X)
                  (SOL (PLUS (LOG (QUOTIENT A B)) C) X)))))) 
(FORALL
 (LIST '(A C X) '(FREEOF (REVALX 'C) (REVALX 'X))
       '(LET00
         '((EQUAL (SOL (PLUS (LOG A) C) X)
                  (DIFFERENCE A (EXPT E (MINUS C)))))))) 
(FORALL
 (LIST '(A B X) 'T
       '(LET00
         '((EQUAL (SOL (PLUS (LOG A) (LOG B)) X) (DIFFERENCE (TIMES A B) 1))
           (EQUAL (SOL (DIFFERENCE (LOG A) (LOG B)) X) (DIFFERENCE A B))
           (EQUAL (SOL (PLUS (SIN A) (SIN B)) X)
                  (COND
                   ((BOOLVALUE* (REVALX *ALLBRANCH))
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'SIN (LIST 'QUOTIENT (LIST 'PLUS 'A 'B) 2))
                           (LIST 'COS
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE 'A 'B)
                                       2)))))
                   (T (AEVAL (LIST 'PLUS 'A 'B)))))
           (EQUAL (SOL (DIFFERENCE (SIN A) (SIN B)) X)
                  (COND
                   ((BOOLVALUE* (REVALX *ALLBRANCH))
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'SIN
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE 'A 'B) 2))
                           (LIST 'COS (LIST 'QUOTIENT (LIST 'PLUS 'A 'B) 2)))))
                   (T (AEVAL (LIST 'DIFFERENCE 'A 'B)))))
           (EQUAL (SOL (PLUS (COS A) (COS B)) X)
                  (TIMES (COS (QUOTIENT (PLUS A B) 2))
                         (COS (QUOTIENT (DIFFERENCE A B) 2))))
           (EQUAL (SOL (DIFFERENCE (COS A) (COS B)) X)
                  (COND
                   ((BOOLVALUE* (REVALX *ALLBRANCH))
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'SIN (LIST 'QUOTIENT (LIST 'PLUS 'A 'B) 2))
                           (LIST 'SIN
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE 'A 'B)
                                       2)))))
                   (T (AEVAL (LIST 'DIFFERENCE 'A 'B)))))
           (EQUAL (SOL (DIFFERENCE (ASIN A) (ASIN B)) X) (DIFFERENCE A B))
           (EQUAL (SOL (PLUS (ASIN A) (ASIN B)) X) (PLUS A B))
           (EQUAL (SOL (DIFFERENCE (ACOS A) (ACOS B)) X) (DIFFERENCE A B))
           (EQUAL (SOL (PLUS (ACOS A) (ACOS B)) X) (DIFFERENCE A B)))))) 
(LET
 '((LIST
    (REPLACEBY
     (SOL
      (DIFFERENCE (TIMES (~ (~ B)) (EXPT (~ A) (~ X)))
                  (TIMES (~ (~ D)) (EXPT (~ C) (TIMES (~ F) (~ X)))))
      (~ X))
     (WHEN (DIFFERENCE (QUOTIENT B D) (EXPT (QUOTIENT (EXPT C F) A) X))
      (AND (FREEOF A X) (FREEOF B X) (FREEOF C X) (FREEOF D X) (FREEOF F X))))
    (REPLACEBY
     (SOL
      (DIFFERENCE (TIMES (~ (~ B)) (EXPT (~ A) (~ X)))
                  (TIMES (~ (~ D)) (EXPT (~ C) (~ X))))
      (~ X))
     (WHEN (DIFFERENCE (QUOTIENT B D) (EXPT (QUOTIENT C A) X))
      (AND (FREEOF A X) (FREEOF B X) (FREEOF C X) (FREEOF D X))))
    (REPLACEBY
     (SOL
      (DIFFERENCE (TIMES (~ (~ B)) (EXPT (~ A) (~ X)))
                  (TIMES (~ (~ D)) (EXPT (~ C) (~ X)) (~ G)))
      (~ X))
     (WHEN (DIFFERENCE (TIMES (QUOTIENT B D) (EXPT (QUOTIENT A C) X)) G)
      (AND (FREEOF A X) (FREEOF B X) (FREEOF C X) (FREEOF D X))))
    (REPLACEBY
     (SOL
      (DIFFERENCE (TIMES (~ (~ B)) (EXPT (~ A) (~ X)))
                  (TIMES (EXPT (~ C) (~ X)) (~ G)))
      (~ X))
     (WHEN (DIFFERENCE (TIMES B (EXPT (QUOTIENT A C) X)) G)
      (AND (FREEOF A X) (FREEOF B X) (FREEOF C X))))))) 
(SETK 'SOLVE_TRIG_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'PLUS (LIST 'TIMES (LIST 'SIN 'X) (LIST 'COS 'Y))
                         (LIST 'TIMES (LIST 'COS 'X) (LIST 'SIN 'Y))))
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'DIFFERENCE (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'SIN 'X) (LIST 'COS 'Y))
                         (LIST 'TIMES (LIST 'COS 'X) (LIST 'SIN 'Y))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'COS 'X) (LIST 'COS 'Y))
                         (LIST 'TIMES (LIST 'SIN 'X) (LIST 'SIN 'Y))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'DIFFERENCE (LIST '~ 'X) (LIST '~ 'Y)))
                   (LIST 'PLUS (LIST 'TIMES (LIST 'COS 'X) (LIST 'COS 'Y))
                         (LIST 'TIMES (LIST 'SIN 'X) (LIST 'SIN 'Y))))))) 
(FLUID '(SOLVE_INVTRIG_SOLN*)) 
(SHARE (LIST 'SOLVE_INVTRIG_SOLN*)) 
(CLEAR (LIST 'SOLVE_INVTRIG_SOLN*)) 
(SETK 'INVTRIG_SOLVE_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ASIN (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'ASIN 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ACOS (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'ACOS 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ATAN (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''TAN
                               (LIST 'PLUS (LIST 'ATAN 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ACOS (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'ACOS 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ATAN (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'ATAN 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ASIN (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'ASIN 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST 'ATAN (LIST '~ 'X)) (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'ATAN 'X) 'Y) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ASIN (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ASIN 'X)) 'Y)
                               'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ACOS (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ACOS 'X)) 'Y)
                               'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ACOS (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ACOS 'X)) 'Y)
                               'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ATAN (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''SIN
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ATAN 'X)) 'Y)
                               'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ASIN (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ASIN 'X)) 'Y)
                               'Z)))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ 'N)
                                     (LIST 'ATAN (LIST '~ 'X)))
                               (LIST '~ 'Y))
                         (LIST '~ 'Z))
                   (LIST 'WHEN SOLVE_INVTRIG_SOLN*
                         (LIST 'CHECK_SOLVE_INV_TRIG ''COS
                               (LIST 'PLUS (LIST 'TIMES 'N (LIST 'ATAN 'X)) 'Y)
                               'Z)))))) 
(LET '(INVTRIG_SOLVE_RULES)) 
(SETK 'LAMBERT_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS (LIST '~ 'X)
                               (LIST 'LOG
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                           (LIST 'QUOTIENT (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'D))))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE 'X
                               (LIST 'LAMBERT_W (LIST 'QUOTIENT 'D 'C)))
                         (LIST 'AND (LIST 'FREEOF 'C 'X)
                               (LIST 'FREEOF 'D 'X))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N)))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 'X))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE 'X
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT 'E
                                           (LIST 'MINUS
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 1 'N)
                                                       (LIST 'LAMBERT_W
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'TIMES
                                                                         'N 'A)
                                                                   (LIST 'TIMES
                                                                         'B
                                                                         (LIST
                                                                          'EXPT
                                                                          'C
                                                                          'N)))))))
                                     'C))
                         (LIST 'AND
                               (LIST 'OR (LIST 'NEQ 'A 1) (LIST 'NEQ 'B 1))
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FIXP 'N)
                               (LIST 'GREATERP 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG (LIST '~ 'C)))
                               (LIST '~ 'W))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SOL
                               (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                     (LIST 'TIMES 'B
                                           (LIST 'LOG
                                                 (LIST 'TIMES 'C
                                                       (LIST 'EXPT 'E
                                                             (LIST 'QUOTIENT 'W
                                                                   'B))))))
                               'X)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'W 'X)
                               (LIST 'NOT (LIST 'FREEOF 'C 'X)))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST 'QUOTIENT 'X
                                                       (LIST '~
                                                             (LIST '~ 'D)))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SUB
                               (LIST 'EQUAL 'X
                                     (LIST 'TIMES 'A (LIST 'QUOTIENT 'X 'B)))
                               (LIST 'SOL
                                     (LIST 'PLUS 'X
                                           (LIST 'LOG
                                                 (LIST 'TIMES 'C 'B
                                                       (LIST 'QUOTIENT 'X
                                                             (LIST 'TIMES 'A
                                                                   'D)))))
                                     'X))
                         (LIST 'AND
                               (LIST 'OR (LIST 'NEQ 'A 1) (LIST 'NEQ 'B 1))
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG
                                           (LIST 'QUOTIENT
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES
                                                             (LIST '~
                                                                   (LIST '~
                                                                         'C))
                                                             'X)
                                                       (LIST '~ 'U))
                                                 (LIST '~ (LIST '~ 'D))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SUB
                               (LIST 'EQUAL 'X
                                     (LIST 'PLUS 'X (LIST 'QUOTIENT 'U 'C)))
                               (LIST 'SOL
                                     (LIST 'NUM
                                           (LIST 'PLUS
                                                 (LIST 'TIMES 'A
                                                       (LIST 'DIFFERENCE 'X
                                                             (LIST 'QUOTIENT 'U
                                                                   'C)))
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG
                                                             (LIST 'TIMES 'C
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    'X 'D))))))
                                     'X))
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'U 'X))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG
                                           (LIST 'QUOTIENT
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'C))
                                                       (LIST 'EXPT 'X
                                                             (LIST '~ 'N)))
                                                 (LIST '~ (LIST '~ 'D))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SOL
                               (LIST 'NUM
                                     (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                           (LIST 'TIMES 'N 'B
                                                 (LIST 'LOG
                                                       (LIST 'TIMES
                                                             (LIST
                                                              'NEWROOT_OF_UNITY
                                                              'N)
                                                             'X)))
                                           (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                                                 (LIST 'LOG
                                                       (LIST 'QUOTIENT 'C
                                                             'D)))))
                               'X)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'LOG
                                           (LIST 'QUOTIENT
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'C))
                                                       (LIST 'EXPT 'X
                                                             (LIST '~ 'N)))
                                                 (LIST '~ (LIST '~ 'D))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SOL
                               (LIST 'NUM
                                     (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                           (LIST 'TIMES 'N 'B (LIST 'LOG 'X))
                                           (LIST 'TIMES (LIST 'QUOTIENT 1 'N)
                                                 (LIST 'LOG
                                                       (LIST 'QUOTIENT 'C
                                                             'D)))))
                               'X)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'N))))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'EXPT 'E
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST 'QUOTIENT (LIST '~ 'X)
                                                       (LIST '~
                                                             (LIST '~ 'D)))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SOL
                               (LIST 'NUM
                                     (LIST 'PLUS (LIST 'LOG 'A)
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'N
                                                       (LIST 'LOG
                                                             (LIST 'TIMES
                                                                   (LIST
                                                                    'NEWROOT_OF_UNITY
                                                                    'N)
                                                                   'X)))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES
                                                                   (LIST 'LOG
                                                                         (LIST
                                                                          'MINUS
                                                                          'B))
                                                                   'D)
                                                             (LIST 'TIMES 'C
                                                                   'X))
                                                       'D))))
                               'X)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST 'EXPT (LIST '~ 'X)
                                           (LIST '~ (LIST '~ 'N))))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'EXPT 'E
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST 'QUOTIENT (LIST '~ 'X)
                                                       (LIST '~
                                                             (LIST '~ 'D)))))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SOL
                               (LIST 'NUM
                                     (LIST 'PLUS (LIST 'LOG 'A)
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'N
                                                       (LIST 'LOG 'X))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES
                                                                   (LIST 'LOG
                                                                         (LIST
                                                                          'MINUS
                                                                          'B))
                                                                   'D)
                                                             (LIST 'TIMES 'C
                                                                   'X))
                                                       'D))))
                               'X)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'SOL
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST '~ 'X))
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'EXPT 'E
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST 'QUOTIENT (LIST '~ 'X)
                                                       (LIST '~
                                                             (LIST '~ 'D))))))
                               (LIST '~ 'F))
                         'X)
                   (LIST 'WHEN
                         (LIST 'SUB
                               (LIST 'EQUAL 'X
                                     (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                           (LIST 'QUOTIENT 'F 'A)))
                               (LIST 'SOL
                                     (LIST 'NUM
                                           (LIST 'PLUS 'X
                                                 (LIST 'TIMES 'B
                                                       (LIST 'EXPT 'E
                                                             (LIST 'MINUS
                                                                   (LIST 'TIMES
                                                                         'C
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          'F
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'D)))))
                                                       (LIST 'EXPT 'E
                                                             (LIST 'TIMES 'C
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    'X
                                                                    (LIST
                                                                     'TIMES 'A
                                                                     'D)))))))
                                     'X))
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'F 'X))))))) 
(PUT 'LAMBERTP 'NUMBER-OF-ARGS 2) 
(PUT 'LAMBERTP 'DEFINED-ON-LINE '253) 
(PUT 'LAMBERTP 'DEFINED-IN-FILE 'SOLVE/SOLVETAB.RED) 
(PUT 'LAMBERTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LAMBERTP (E1 X) (PROGN X (OR (SMEMQ 'LOG E1) (SMEMQ 'EXPT E1)))) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(FLUID '(SOL-RULESETS*)) 
(SETQ SOL-RULESETS* (LIST (LIST 'LAMBERTP 'LAMBERT_RULES))) 
(PUT 'SOLVE-APPLY-RULES 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-APPLY-RULES 'DEFINED-ON-LINE '262) 
(PUT 'SOLVE-APPLY-RULES 'DEFINED-IN-FILE 'SOLVE/SOLVETAB.RED) 
(PUT 'SOLVE-APPLY-RULES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-APPLY-RULES (E1 VAR)
    (PROG (RULES U *PRECISE)
      (SETQ *PRECISE T)
      (SETQ U (LIST 'SOL (MK*SQ (CONS E1 1)) VAR))
      (PROG (R)
        (SETQ R SOL-RULESETS*)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (COND
            ((APPLY (CAR R) (LIST E1 VAR))
             (SETQ RULES (CONS (CADR R) RULES)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (COND ((NULL RULES) (RETURN (SIMP* U))))
      (LOAD-PACKAGE 'ODESOLVE)
      (RETURN (CAR (EVALLETSUB2 (LIST RULES (LIST 'SIMP* (MKQUOTE U))) NIL))))) 
(ENDMODULE) 