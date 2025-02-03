(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFINT)) 
(PUT 'EI 'PLAIN-FUNCTIONSYMBOL '|eI|) 
(PUT 'CI 'PLAIN-FUNCTIONSYMBOL '|cI|) 
(PUT 'SI 'PLAIN-FUNCTIONSYMBOL '|sI|) 
(PUT 'CHI 'PLAIN-FUNCTIONSYMBOL '|cHI|) 
(PUT 'SHI 'PLAIN-FUNCTIONSYMBOL '|sHI|) 
(PUT 'EI 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'SI 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'CI 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'SHI 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'CHI 'PRIFN 'PLAIN-SYMBOL) 
(FLAG '(EI CI SI S_I CHI SHI LI ERF ERFC FRESNEL_C FRESNEL_S) 'SPECFN) 
(DEFLIST
 '((EI 1) (SI 1) (S_I 1) (CI 1) (CHI 1) (SHI 1) (LI 1) (ERF 1) (ERFC 1)
   (FRESNEL_S 1) (FRESNEL_C 1))
 'NUMBER-OF-ARGS) 
(AEVAL (LET '((REPLACEBY (LIMIT (SI (~ TT)) (~ TT) INFINITY) (QUOTIENT PI 2))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (INT (QUOTIENT (SIN (TIMES (~ (~ A)) (~ TT))) (~ TT)) (~ TT) 0 (~ Z))
      (WHEN (SI (TIMES A Z)) (FREEOF A TT))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (INT (QUOTIENT (SIN (~ TT)) (~ TT)) (~ TT) (~ Z) INFINITY)
      (MINUS (S_I Z)))
     (REPLACEBY (LIMIT (S_I (~ TT)) X INFINITY) 0))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (INT (QUOTIENT (EXP (~ TT)) (~ TT)) (~ TT) (MINUS INFINITY) (~ Z))
      (EI Z)))))) 
(AEVAL
 (LET
  '((LIST (REPLACEBY (INT (QUOTIENT 1 (LOG (~ TT))) (~ TT) 0 (~ Z)) (LI Z)))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (INT (QUOTIENT (COS (~ TT)) (~ TT)) (~ TT) (~ Z) INFINITY)
      (MINUS (CI Z)))
     (REPLACEBY
      (INT (QUOTIENT (DIFFERENCE (COS (~ TT)) 1) (~ TT)) (~ TT) 0 (~ Z))
      (PLUS (CI Z) (DIFFERENCE (PSI 1) (LOG Z)))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (INT (QUOTIENT (SINH (~ TT)) (~ TT)) (~ TT) 0 (~ Z))
      (SHI Z)))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (INT (QUOTIENT (DIFFERENCE (COSH (~ TT)) 1) (~ TT)) (~ TT) 0 (~ Z))
      (PLUS (CHI Z) (DIFFERENCE (PSI 1) (LOG Z)))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (INT (SIN (TIMES (QUOTIENT PI 2) (EXPT (~ TT) 2))) (~ TT) 0 (~ Z))
      (FRESNEL_S Z))
     (REPLACEBY (LIMIT (FRESNEL_S (~ TT)) (~ TT) INFINITY) (QUOTIENT 1 2)))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (INT (COS (TIMES (QUOTIENT PI 2) (EXPT (~ TT) 2))) (~ TT) 0 (~ Z))
      (FRESNEL_C Z))
     (REPLACEBY (LIMIT (FRESNEL_C (~ TT)) (~ TT) INFINITY) (QUOTIENT 1 2)))))) 
(AEVAL
 (LET
  '((LIST (REPLACEBY (LIMIT (ERF (~ X)) (~ X) INFINITY) 1)
          (REPLACEBY (LIMIT (ERFC (~ X)) (~ X) INFINITY) 0)
          (REPLACEBY (INT (QUOTIENT 1 (EXPT E (EXPT (~ TT) 2))) (~ TT) 0 (~ Z))
           (WHEN (TIMES (QUOTIENT (ERF Z) 2) (SQRT PI)) (FREEOF Z INFINITY)))
          (REPLACEBY
           (INT (QUOTIENT 1 (EXPT E (EXPT (~ TT) 2))) (~ TT) (~ Z) INFINITY)
           (WHEN (TIMES (QUOTIENT (ERFC Z) 2) (SQRT PI))
            (FREEOF Z INFINITY))))))) 
(PUT '|COMPUTE:INT:FUNCTIONS| 'NUMBER-OF-ARGS 2) 
(FLAG '(|COMPUTE:INT:FUNCTIONS|) 'OPFN) 
(PUT '|COMPUTE:INT:FUNCTIONS| 'DEFINED-ON-LINE '160) 
(PUT '|COMPUTE:INT:FUNCTIONS| 'DEFINED-IN-FILE 'SPECFN/SFINT.RED) 
(PUT '|COMPUTE:INT:FUNCTIONS| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |COMPUTE:INT:FUNCTIONS| (X F)
    (PROG (PRE *UNCACHED SCALE TERM N PRECIS RESULT INTERM)
      (SETQ PRE (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION PRE))
      (SETQ PRECIS (AEVAL (LIST 'EXPT 10 (LIST 'MINUS (LIST 'TIMES 2 PRE)))))
      (SETQ *UNCACHED T)
      (COND
       ((EVALEQUAL (AEVAL F) (AEVAL 'SI))
        (COND
         ((EVALLESSP (AEVAL X) 0)
          (SETQ RESULT
                  (AEVAL
                   (LIST 'MINUS
                         (LIST '|COMPUTE:INT:FUNCTIONS| (LIST 'MINUS X) F)))))
         (T
          (PROGN
           (SETQ N (AEVAL 1))
           (SETQ TERM (AEVAL X))
           (SETQ RESULT (AEVAL X))
           (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                  (PROGN
                   (SETQ TERM
                           (AEVAL*
                            (LIST 'MINUS
                                  (LIST 'TIMES 1
                                        (LIST 'QUOTIENT (LIST 'TIMES TERM X X)
                                              (LIST 'TIMES 2 N
                                                    (LIST 'PLUS
                                                          (LIST 'TIMES 2 N)
                                                          1)))))))
                   (SETQ RESULT
                           (AEVAL*
                            (LIST 'PLUS RESULT
                                  (LIST 'QUOTIENT TERM
                                        (LIST 'PLUS (LIST 'TIMES 2 N) 1)))))
                   (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
           (AEVAL 'NIL)))))
       ((EVALEQUAL (AEVAL F) (AEVAL 'CI))
        (COND
         ((EVALEQUAL (AEVAL X) 0)
          (AEVAL (RERROR 'SPECFN 0 (REVALX "Ci(0) is undefined"))))
         ((AND (REALVALUEDP (REVALX X)) (EVALLESSP (AEVAL X) 0))
          (SETQ RESULT
                  (AEVAL
                   (LIST 'PLUS
                         (LIST '|COMPUTE:INT:FUNCTIONS| (LIST 'MINUS X) F)
                         (LIST 'TIMES 'I 'PI)))))
         (T
          (PROGN
           (SETQ N (AEVAL 1))
           (SETQ TERM (AEVAL 1))
           (SETQ RESULT (AEVAL (LIST 'PLUS 'EULER_GAMMA (LIST 'LOG X))))
           (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                  (PROGN
                   (SETQ TERM
                           (AEVAL*
                            (LIST 'MINUS
                                  (LIST 'TIMES 1
                                        (LIST 'QUOTIENT (LIST 'TIMES TERM X X)
                                              (LIST 'TIMES
                                                    (LIST 'DIFFERENCE
                                                          (LIST 'TIMES 2 N) 1)
                                                    2 N))))))
                   (SETQ RESULT
                           (AEVAL*
                            (LIST 'PLUS RESULT
                                  (LIST 'QUOTIENT TERM (LIST 'TIMES 2 N)))))
                   (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
           (AEVAL 'NIL)))))
       ((EVALEQUAL (AEVAL F) (AEVAL 'EI))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL X) 0)
           (AEVAL (RERROR 'SPECFN 0 (REVALX "Ei(0) is undefined"))))
          ((NOT (REALVALUEDP (REVALX X)))
           (AEVAL (RERROR 'SPECFN 0 "Ei undefined for complex argument"))))
         (SETQ N (AEVAL 1))
         (SETQ TERM (AEVAL 1))
         (SETQ RESULT
                 (AEVAL (LIST 'PLUS 'EULER_GAMMA (LIST 'LOG (LIST 'ABS X)))))
         (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                (PROGN
                 (SETQ TERM (AEVAL* (LIST 'QUOTIENT (LIST 'TIMES TERM X) N)))
                 (SETQ RESULT
                         (AEVAL* (LIST 'PLUS RESULT (LIST 'QUOTIENT TERM N))))
                 (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL F) (AEVAL 'SHI))
        (PROGN
         (SETQ N (AEVAL 1))
         (SETQ TERM (AEVAL X))
         (SETQ RESULT (AEVAL X))
         (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                (PROGN
                 (SETQ TERM
                         (AEVAL*
                          (LIST 'QUOTIENT (LIST 'TIMES TERM X X)
                                (LIST 'TIMES 2 N
                                      (LIST 'PLUS (LIST 'TIMES 2 N) 1)))))
                 (SETQ RESULT
                         (AEVAL*
                          (LIST 'PLUS RESULT
                                (LIST 'QUOTIENT TERM
                                      (LIST 'PLUS (LIST 'TIMES 2 N) 1)))))
                 (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL F) (AEVAL 'CHI))
        (COND
         ((EVALEQUAL (AEVAL X) 0)
          (AEVAL (RERROR 'SPECFN 0 (REVALX "Chi(0) is undefined"))))
         ((AND (REALVALUEDP (REVALX X)) (EVALLESSP (AEVAL X) 0))
          (SETQ RESULT
                  (AEVAL
                   (LIST 'PLUS
                         (LIST '|COMPUTE:INT:FUNCTIONS| (LIST 'MINUS X) F)
                         (LIST 'TIMES 'I 'PI)))))
         (T
          (PROGN
           (SETQ N (AEVAL 1))
           (SETQ TERM (AEVAL 1))
           (SETQ RESULT (AEVAL (LIST 'PLUS 'EULER_GAMMA (LIST 'LOG X))))
           (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                  (PROGN
                   (SETQ TERM
                           (AEVAL*
                            (LIST 'QUOTIENT (LIST 'TIMES TERM X X)
                                  (LIST 'TIMES
                                        (LIST 'DIFFERENCE (LIST 'TIMES 2 N) 1)
                                        2 N))))
                   (SETQ RESULT
                           (AEVAL*
                            (LIST 'PLUS RESULT
                                  (LIST 'QUOTIENT TERM (LIST 'TIMES 2 N)))))
                   (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
           (AEVAL 'NIL)))))
       ((EVALEQUAL (AEVAL F) (AEVAL 'ERF))
        (COND
         ((EVALLESSP (AEVAL X) 0)
          (SETQ RESULT
                  (AEVAL
                   (LIST 'MINUS
                         (LIST '|COMPUTE:INT:FUNCTIONS| (LIST 'MINUS X) F)))))
         (T
          (PROGN
           (SETQ N (AEVAL 1))
           (SETQ TERM (AEVAL X))
           (SETQ RESULT (AEVAL X))
           (COND
            ((EVALGREATERP (AEVAL (LIST 'FLOOR (LIST 'TIMES X 7))) (AEVAL PRE))
             (AEVAL (LIST 'PRECISION (LIST 'FLOOR (LIST 'TIMES X 7))))))
           (SETQ INTERM (AEVAL (LIST 'MINUS (LIST 'TIMES 1 X X))))
           (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                  (PROGN
                   (SETQ TERM
                           (AEVAL*
                            (LIST 'QUOTIENT (LIST 'TIMES TERM INTERM) N)))
                   (SETQ RESULT
                           (AEVAL*
                            (LIST 'PLUS RESULT
                                  (LIST 'QUOTIENT TERM
                                        (LIST 'PLUS (LIST 'TIMES 2 N) 1)))))
                   (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
           (AEVAL (LIST 'PRECISION PRE))
           (SETQ RESULT
                   (AEVAL
                    (LIST 'TIMES RESULT (LIST 'QUOTIENT 2 (LIST 'SQRT 'PI)))))
           (AEVAL 'NIL)))))
       ((EVALEQUAL (AEVAL F) (AEVAL 'FRESNEL_S))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 40 . -1)))
           (AEVAL (LIST 'PRECISION (LIST 'MAX PRE 40)))))
         (COND
          ((EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 60 . -1)))
           (AEVAL (LIST 'PRECISION (LIST 'MAX PRE 80)))))
         (SETQ N (AEVAL 1))
         (SETQ TERM
                 (AEVAL (LIST 'TIMES (LIST 'EXPT X 3) (LIST 'QUOTIENT 'PI 2))))
         (SETQ RESULT (AEVAL (LIST 'QUOTIENT TERM 3)))
         (SETQ INTERM
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT X 4)
                        (LIST 'EXPT (LIST 'QUOTIENT 'PI 2) 2))))
         (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                (PROGN
                 (SETQ TERM
                         (AEVAL*
                          (LIST 'MINUS
                                (LIST 'TIMES 1
                                      (LIST 'QUOTIENT (LIST 'TIMES TERM INTERM)
                                            (LIST 'TIMES 2 N
                                                  (LIST 'PLUS (LIST 'TIMES 2 N)
                                                        1)))))))
                 (SETQ RESULT
                         (AEVAL*
                          (LIST 'PLUS RESULT
                                (LIST 'QUOTIENT TERM
                                      (LIST 'PLUS (LIST 'TIMES 4 N) 3)))))
                 (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL F) (AEVAL 'FRESNEL_C))
        (PROGN
         (COND
          ((EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 40 . -1)))
           (AEVAL (LIST 'PRECISION (LIST 'MAX PRE 40)))))
         (COND
          ((EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 60 . -1)))
           (AEVAL (LIST 'PRECISION (LIST 'MAX PRE 80)))))
         (SETQ N (AEVAL 1))
         (SETQ TERM (AEVAL X))
         (SETQ RESULT (AEVAL X))
         (SETQ INTERM
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT X 4)
                        (LIST 'EXPT (LIST 'QUOTIENT 'PI 2) 2))))
         (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                (PROGN
                 (SETQ TERM
                         (AEVAL*
                          (LIST 'MINUS
                                (LIST 'TIMES 1
                                      (LIST 'QUOTIENT (LIST 'TIMES TERM INTERM)
                                            (LIST 'TIMES 2 N
                                                  (LIST 'DIFFERENCE
                                                        (LIST 'TIMES 2 N)
                                                        1)))))))
                 (SETQ RESULT
                         (AEVAL*
                          (LIST 'PLUS RESULT
                                (LIST 'QUOTIENT TERM
                                      (LIST 'PLUS (LIST 'TIMES 4 N) 1)))))
                 (SETQ N (AEVAL* (LIST 'PLUS N 1)))))
         (AEVAL 'NIL))))
      (AEVAL (LIST 'PRECISION PRE))
      (RETURN (AEVAL RESULT)))) 
(ENDMODULE) 