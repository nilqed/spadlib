(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUBSTEXP)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(DEPEND (LIST 'FF 'X)) 
(DEPEND (LIST 'F 'X)) 
(DEPEND (LIST 'F '~K)) 
(OPERATOR (LIST 'PSSUBST)) 
(OPERATOR (LIST 'A 'PPRODUCT)) 
(SETK 'SUBST_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'MINUS (LIST '~ 'G)) (LIST '~ 'X)
                         (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'MINUS (LIST 'PSSUBST 'G 'X 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'PLUS (LIST '~ 'G) (LIST '~ 'H))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'PLUS (LIST 'PSSUBST 'G 'X 'A 'N)
                         (LIST 'PSSUBST 'H 'X 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'TIMES (LIST '~ 'C) (LIST '~ 'G))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'TIMES 'C (LIST 'PSSUBST 'G 'X 'A 'N))
                         (LIST 'AND (LIST 'FREEOF 'C 'X)
                               (LIST 'FREEOF 'C 'G))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'DF (LIST '~ 'F) (LIST '~ 'X) (LIST '~ 'K))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'POCHHAMMER (LIST 'PLUS 'N 1) 'K)
                         (LIST 'A (LIST 'PLUS 'N 'K))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'DF (LIST '~ 'F) (LIST '~ 'X))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'PLUS 'N 1) (LIST 'A (LIST 'PLUS 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'TIMES (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                               (LIST 'DF (LIST '~ 'F) (LIST '~ 'X)))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES
                         (LIST 'POCHHAMMER
                               (LIST 'PLUS 'N (LIST 'DIFFERENCE 1 'J)) 1)
                         (LIST 'A (LIST 'PLUS 'N (LIST 'DIFFERENCE 1 'J)))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'TIMES (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                               (LIST 'DF (LIST '~ 'F) (LIST '~ 'X)
                                     (LIST '~ 'K)))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES
                         (LIST 'POCHHAMMER
                               (LIST 'PLUS 'N (LIST 'DIFFERENCE 1 'J)) 'K)
                         (LIST 'A (LIST 'PLUS 'N (LIST 'DIFFERENCE 'K 'J)))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'TIMES 'FF
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J)))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'A (LIST 'DIFFERENCE 'N 'J)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'TIMES 'FF (LIST '~ 'X)) (LIST '~ 'X)
                         (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'A (LIST 'DIFFERENCE 'N 1)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'TIMES (LIST 'DF (LIST '~ 'F) (LIST '~ 'X)) 'X)
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'POCHHAMMER 'N 1) (LIST 'A 'N)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST
                         (LIST 'TIMES
                               (LIST 'DF (LIST '~ 'F) (LIST '~ 'X)
                                     (LIST '~ 'K))
                               'X)
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'POCHHAMMER 'N 'K)
                         (LIST 'A (LIST 'PLUS 'N (LIST 'DIFFERENCE 'K 1)))))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST 'F (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'A 'N))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST 'FF (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'A 'N))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST '~ 'C) (LIST '~ 'X) (LIST '~ 'A)
                         (LIST '~ 'N))
                   (LIST 'WHEN 0 (LIST 'FREEOF 'C 'X)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'J))
                         (LIST '~ 'X) (LIST '~ 'A) (LIST '~ 'N))
                   (LIST 'WHEN 0 (LIST 'FIXP 'J)))
             (LIST 'REPLACEBY
                   (LIST 'PSSUBST (LIST '~ 'X) (LIST '~ 'X) (LIST '~ 'A)
                         (LIST '~ 'N))
                   0)))) 
(SETK 'SPEC_POCHHAMMER
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SLASH (LIST 'POCHHAMMER (LIST '~ 'A) (LIST '~ 'K))
                         (LIST 'POCHHAMMER (LIST '~ 'B) (LIST '~ 'K)))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'PLUS 'A (LIST 'DIFFERENCE 'K 1))
                               (LIST 'DIFFERENCE 'A 1))
                         (LIST 'EQUAL (LIST 'DIFFERENCE 'A 'B) 1)))
             (LIST 'REPLACEBY
                   (LIST 'SLASH (LIST 'POCHHAMMER (LIST '~ 'A) (LIST '~ 'K))
                         (LIST 'POCHHAMMER (LIST '~ 'B) (LIST '~ 'K)))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT (LIST 'DIFFERENCE 'B 1)
                               (LIST 'PLUS 'B (LIST 'DIFFERENCE 'K 1)))
                         (LIST 'EQUAL (LIST 'DIFFERENCE 'B 'A) 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'POCHHAMMER (LIST '~ 'Z) (LIST '~ 'K))
                         (LIST 'POCHHAMMER (LIST '~ 'CZ) (LIST '~ 'K)))
                   (LIST 'WHEN
                         (LIST 'PROD
                               (LIST 'PLUS
                                     (LIST 'EXPT
                                           (LIST 'PLUS (LIST 'REPART 'Z)
                                                 (LIST 'DIFFERENCE 'J 1))
                                           2)
                                     (LIST 'EXPT (LIST 'IMPART 'Z) 2))
                               'J 1 'K)
                         (LIST 'AND
                               (LIST 'NOT (LIST 'EQUAL (LIST 'IMPART 'Z) 0))
                               (LIST 'EQUAL 'Z (LIST 'CONJ 'CZ)))))
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'WHEN 1 (LIST 'EQUAL 'N 0)))
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'PPRODUCT 'K 'N) (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY (LIST 'PPRODUCT (LIST '~ 'K) (LIST '~ 'II))
                   (LIST 'WHEN 1 (LIST 'EQUAL 'II 0)))
             (LIST 'REPLACEBY (LIST 'PPRODUCT (LIST '~ 'K) (LIST '~ 'II))
                   (LIST 'TIMES (LIST 'PLUS 'K (LIST 'DIFFERENCE 'II 1))
                         (LIST 'PPRODUCT 'K (LIST 'DIFFERENCE 'II 1))))))) 
(SETK 'SPEC_FACTORIAL
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SLASH (LIST 'FACTORIAL (LIST '~ 'N))
                         (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1)))
                   (LIST 'QUOTIENT 1 (LIST 'PLUS 'N 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FACTORIAL (LIST '~ 'N))
                         (LIST 'SLASH (LIST 'FACTORIAL (LIST '~ 'N))
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))))
                   (LIST 'QUOTIENT (LIST 'FACTORIAL 'N) (LIST 'PLUS 'N 1)))
             (LIST 'REPLACEBY
                   (LIST 'SLASH (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))
                         (LIST 'FACTORIAL (LIST '~ 'N)))
                   (LIST 'PLUS 'N 1))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))
                         (LIST 'SLASH
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))
                               (LIST 'FACTORIAL (LIST '~ 'N))))
                   (LIST 'TIMES (LIST 'PLUS 'N 1)
                         (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'EXPT (LIST '~ 'OTTO) (LIST '~ 'K))
                         (LIST 'SLASH (LIST 'FACTORIAL (LIST '~ 'N))
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))))
                   (LIST 'QUOTIENT (LIST 'EXPT 'OTTO 'K) (LIST 'PLUS 'N 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'EXPT (LIST '~ 'OTTO) (LIST '~ 'K))
                         (LIST 'SLASH
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))
                               (LIST 'FACTORIAL (LIST '~ 'N))))
                   (LIST 'TIMES (LIST 'EXPT 'OTTO 'K) (LIST 'PLUS 'N 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'EXPT (LIST '~ 'OTTO) (LIST '~ 'K))
                         (LIST '~ 'HUGO)
                         (LIST 'SLASH (LIST 'FACTORIAL (LIST '~ 'N))
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))))
                   (LIST 'TIMES (LIST 'EXPT 'OTTO 'K)
                         (LIST 'QUOTIENT 'HUGO (LIST 'PLUS 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'EXPT (LIST '~ 'OTTO) (LIST '~ 'K))
                         (LIST '~ 'HUGO)
                         (LIST 'SLASH
                               (LIST 'FACTORIAL (LIST 'PLUS (LIST '~ 'N) 1))
                               (LIST 'FACTORIAL (LIST '~ 'N))))
                   (LIST 'TIMES (LIST 'EXPT 'OTTO 'K) 'HUGO
                         (LIST 'PLUS 'N 1)))))) 
(ENDMODULE) 