(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFOTHER)) 
(AEVAL (OPERATOR (LIST 'STRUVEH 'STRUVEL))) 
(SETK 'STRUVE*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z)) 'Z)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE (LIST 'QUOTIENT 2 'PI)
                               (LIST 'STRUVEH 1 'Z))
                         (LIST 'AND (LIST 'NUMBERP 'N) (LIST 'EQUAL 'N 0))))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'X)) 'X)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 'X
                                     (LIST 'STRUVEH (LIST 'PLUS (MINUS 1) 'N)
                                           'X))
                               (LIST 'TIMES 'N (LIST 'STRUVEH 'N 'X)))
                         'X))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'TIMES (LIST 'EXPT 'Z 'N)
                               (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z)))
                         'Z)
                   (LIST 'TIMES (LIST 'EXPT 'Z 'N)
                         (LIST 'STRUVEH (LIST 'DIFFERENCE 'N 1) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'MINUS 'N))
                               (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z)))
                         'Z)
                   (LIST 'DIFFERENCE
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES (LIST 'SQRT 'PI) (LIST 'EXPT 2 'N)
                                     (LIST 'GAMMA
                                           (LIST 'PLUS 'N
                                                 (LIST 'QUOTIENT 3 2)))))
                         (LIST 'TIMES (LIST 'EXPT 'Z (LIST 'MINUS 'N))
                               (LIST 'STRUVEH (LIST 'PLUS 'N 1) 'Z))))
             (LIST 'REPLACEBY (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                               (LIST 'BESSELJ (LIST 'MINUS 'N) 'Z))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'LESSP 'N 0)
                               (LIST 'EQUAL (LIST 'TIMES 'N 2)
                                     (LIST 'FLOOR (LIST 'TIMES 'N 2)))
                               (LIST 'NOT
                                     (LIST 'EVENP
                                           (LIST 'FLOOR
                                                 (LIST 'TIMES 'N 2)))))))
             (LIST 'REPLACEBY (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT
                                     (LIST 'QUOTIENT 2 (LIST 'TIMES 'PI 'Z))
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 'DIFFERENCE 1 (LIST 'COS 'Z)))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL 'N (LIST 'QUOTIENT 1 2)))))
             (LIST 'REPLACEBY (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'EXPT
                                           (LIST 'QUOTIENT 'Z
                                                 (LIST 'TIMES 'PI 2))
                                           (LIST 'QUOTIENT 1 2))
                                     (LIST 'PLUS 1
                                           (LIST 'QUOTIENT 2
                                                 (LIST 'EXPT 'Z 2))))
                               (LIST 'TIMES
                                     (LIST 'EXPT
                                           (LIST 'QUOTIENT 2
                                                 (LIST 'TIMES 'PI 'Z))
                                           (LIST 'QUOTIENT 1 2))
                                     (LIST 'PLUS (LIST 'SIN 'Z)
                                           (LIST 'QUOTIENT (LIST 'COS 'Z)
                                                 'Z))))
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL 'N (LIST 'QUOTIENT 3 2)))))
             (LIST 'REPLACEBY (LIST 'STRUVEH (LIST '~ 'N) (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'TIMES 'X '(|:DN:| 5 . -1))
                                     (LIST 'PLUS 'N 1))
                               (LIST 'STRUVE_COMPUTE_TERM 'N 'X 'H))
                         (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'N)
                               (LIST 'SYMBOLIC '*ROUNDED))))
             (LIST 'REPLACEBY (LIST 'STRUVEL (LIST '~ 'N) (LIST '~ 'X))
                   (LIST 'WHEN (LIST 'STRUVE_COMPUTE_TERM 'N 'X 'L)
                         (LIST 'AND (LIST 'NUMBERP 'X) (LIST 'NUMBERP 'N)
                               (LIST 'SYMBOLIC '*ROUNDED))))
             (LIST 'REPLACEBY (LIST 'STRUVEL (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'BESSELI (LIST 'MINUS 'N) 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'N)
                               (LIST 'EQUAL (LIST 'IMPART 'N) 0)
                               (LIST 'LESSP 'N 0)
                               (LIST 'EQUAL (LIST 'TIMES 'N 2)
                                     (LIST 'FLOOR (LIST 'TIMES 'N 2)))
                               (LIST 'NOT
                                     (LIST 'EVENP
                                           (LIST 'FLOOR
                                                 (LIST 'TIMES 'N 2)))))))
             (LIST 'REPLACEBY (LIST 'STRUVEL (LIST '~ 'N) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES 'I
                                     (LIST 'EXPT 'E
                                           (LIST 'QUOTIENT
                                                 (LIST 'MINUS
                                                       (LIST 'TIMES 'I 'N 'PI))
                                                 2))
                                     (LIST 'STRUVEH 'N (LIST 'TIMES 'I 'Z))))
                         (LIST 'SYMBOLIC '*COMPLEX)))
             (LIST 'REPLACEBY
                   (LIST 'DF (LIST 'STRUVEL (LIST '~ 'N) (LIST '~ 'X)) 'X)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 'X
                                     (LIST 'STRUVEL (LIST 'PLUS (MINUS 1) 'N)
                                           'X))
                               (LIST 'TIMES 'N (LIST 'STRUVEL 'N 'X)))
                         'X))))) 
(AEVAL (LET '(STRUVE*RULES))) 
(AEVAL (OPERATOR (LIST 'LOMMEL1 'LOMMEL2))) 
(SETK 'LOMMEL*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL1 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'EXPT 2 'A)
                                           (LIST 'BESSELJ 'A 'Z)
                                           (LIST 'GAMMA (LIST 'PLUS 'A 1))))
                               (LIST 'EXPT 'Z 'A))
                         (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'EQUAL 'A (LIST 'PLUS 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL1 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'LOMMEL1 'A (LIST 'MINUS 'B) 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'B) (LIST 'LESSP 'B 0)
                               (LIST 'NEQ 'A 'B)
                               (LIST 'NEQ 'A (LIST 'PLUS 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL1 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'TIMES (LIST 'SQRT 'PI) (LIST 'EXPT 2 'A)
                                     (LIST 'GAMMA
                                           (LIST 'QUOTIENT
                                                 (LIST 'PLUS (LIST 'TIMES 2 'A)
                                                       1)
                                                 2))
                                     (LIST 'STRUVEH 'A 'Z))
                               2)
                         (LIST 'EQUAL 'A 'B)))
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL2 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'EXPT 'Z 'B)
                         (LIST 'AND (LIST 'NUMBERP 'A) (LIST 'NUMBERP 'B)
                               (LIST 'EQUAL 'A (LIST 'PLUS 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL2 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'LOMMEL2 'A (LIST 'MINUS 'B) 'Z)
                         (LIST 'AND (LIST 'NUMBERP 'B) (LIST 'LESSP 'B 0)
                               (LIST 'NEQ 'A 'B)
                               (LIST 'NEQ 'A (LIST 'PLUS 'B 1)))))
             (LIST 'REPLACEBY
                   (LIST 'LOMMEL2 (LIST '~ 'A) (LIST '~ 'B) (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'TIMES (LIST 'SQRT 'PI) (LIST 'EXPT 2 'A)
                                     (LIST 'GAMMA
                                           (LIST 'QUOTIENT
                                                 (LIST 'PLUS (LIST 'TIMES 2 'A)
                                                       1)
                                                 2))
                                     (LIST 'PLUS
                                           (LIST 'MINUS (LIST 'BESSELY 'A 'Z))
                                           (LIST 'STRUVEH 'A 'Z)))
                               2)
                         (LIST 'EQUAL 'A 'B)))))) 
(AEVAL (LET '(LOMMEL*RULES))) 
(AEVAL (OPERATOR (LIST 'WHITTAKERM 'WHITTAKERW))) 
(SETK 'WHITTAKER*RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'WHITTAKERM (LIST '~ 'K) (LIST '~ 'M) (LIST '~ 'Z))
                   (LIST 'TIMES (LIST 'EXP (LIST 'MINUS (LIST 'QUOTIENT 'Z 2)))
                         (LIST 'EXPT 'Z (LIST 'PLUS (LIST 'QUOTIENT 1 2) 'M))
                         (LIST 'KUMMERM
                               (LIST 'PLUS (LIST 'QUOTIENT 1 2)
                                     (LIST 'DIFFERENCE 'M 'K))
                               (LIST 'PLUS 1 (LIST 'TIMES 2 'M)) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'WHITTAKERW (LIST '~ 'K) (LIST '~ 'M) (LIST '~ 'Z))
                   (LIST 'TIMES (LIST 'EXP (LIST 'MINUS (LIST 'QUOTIENT 'Z 2)))
                         (LIST 'EXPT 'Z (LIST 'PLUS (LIST 'QUOTIENT 1 2) 'M))
                         (LIST 'KUMMERU
                               (LIST 'PLUS (LIST 'QUOTIENT 1 2)
                                     (LIST 'DIFFERENCE 'M 'K))
                               (LIST 'PLUS 1 (LIST 'TIMES 2 'M)) 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WHITTAKERM (LIST '~ 'N) (LIST '~ 'M)
                               (LIST '~ 'Z))
                         'Z)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'TIMES 2 'Z))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'PLUS 1
                                           (LIST 'DIFFERENCE (LIST 'TIMES 2 'M)
                                                 (LIST 'TIMES 2 'N)))
                                     (LIST 'WHITTAKERM (LIST 'DIFFERENCE 'N 1)
                                           'M 'Z))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE (LIST 'TIMES 2 'N) 'Z)
                                     (LIST 'WHITTAKERM 'N 'M 'Z)))))
             (LIST 'REPLACEBY
                   (LIST 'DF
                         (LIST 'WHITTAKERW (LIST '~ 'N) (LIST '~ 'M)
                               (LIST '~ 'Z))
                         'Z)
                   (LIST 'TIMES (LIST 'QUOTIENT 1 (LIST 'TIMES 4 'Z))
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'PLUS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES 4
                                                             (LIST 'EXPT 'M
                                                                   2)))
                                                 (LIST 'TIMES 4 'N))
                                           (LIST 'TIMES 4 (LIST 'EXPT 'N 2)))
                                     (LIST 'WHITTAKERW (LIST 'DIFFERENCE 'N 1)
                                           'M 'Z))
                               (LIST 'TIMES
                                     (LIST 'DIFFERENCE (LIST 'TIMES 4 'N)
                                           (LIST 'TIMES 2 'Z))
                                     (LIST 'WHITTAKERW 'N 'M 'Z)))))))) 
(AEVAL (LET '(WHITTAKER*RULES))) 
(FLAG '(STRUVEH STRUVEL LOMMEL1 LOMMEL2 WHITTAKERM WHITTAKERW LAMBERT_W)
      'SPECFN) 
(DEFLIST
 '((STRUVEH 2) (STRUVEL 2) (LOMMEL1 3) (LOMMEL2 3) (LAMBERT_W 1) (WHITTAKERM 3)
   (WHITTAKERW 3))
 'NUMBER-OF-ARGS) 
(PUT 'STRUVE_COMPUTE_TERM 'NUMBER-OF-ARGS 3) 
(FLAG '(STRUVE_COMPUTE_TERM) 'OPFN) 
(PUT 'STRUVE_COMPUTE_TERM 'DEFINED-ON-LINE '153) 
(PUT 'STRUVE_COMPUTE_TERM 'DEFINED-IN-FILE 'SPECFN/SFOTHER.RED) 
(PUT 'STRUVE_COMPUTE_TERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STRUVE_COMPUTE_TERM (N X H_OR_L)
    (PROG (DMODE**)
      (SETQ DMODE** DMODE*)
      (RETURN
       (PROG (PRE TERM K PRECIS RESULT *COMPLEX *ROUNDED DMODE* EXPO *MSG)
         (SETQ DMODE* DMODE**)
         (COND
          ((EVALEQUAL (AEVAL H_OR_L) (AEVAL 'L))
           (PROGN
            (AEVAL (ON (LIST 'COMPLEX)))
            (AEVAL (OFF (LIST 'ROUNDED)))
            (SETQ EXPO
                    (AEVAL
                     (LIST 'EXPT 'E
                           (LIST 'MINUS
                                 (LIST 'TIMES 'I N (LIST 'QUOTIENT 'PI 2))))))
            (AEVAL (ON (LIST 'ROUNDED)))
            (RETURN
             (AEVAL
              (LIST 'MINUS
                    (LIST 'TIMES 'I EXPO
                          (LIST 'STRUVEH N (LIST 'TIMES 'I X))))))))
          (T
           (PROGN
            (SETQ PRE (AEVAL (LIST 'PRECISION 0)))
            (SETQ PRECIS
                    (AEVAL
                     (LIST 'EXPT '(|:DN:| 100 . -1)
                           (LIST 'DIFFERENCE (LIST 'MINUS PRE) 2))))
            (SETQ RESULT (AEVAL 0))
            (PROGN
             (COND
              ((EVALGREATERP (AEVAL N) (MINUS 2))
               (PROGN
                (SETQ K (AEVAL 1))
                (SETQ TERM
                        (AEVAL
                         (LIST 'QUOTIENT (LIST 'EXPT 2 (LIST 'PLUS N 2))
                               (LIST 'TIMES 'PI
                                     (PROG (I FORALL-RESULT)
                                       (SETQ I 1)
                                       (SETQ FORALL-RESULT 1)
                                      LAB1
                                       (COND
                                        ((|AMINUSP:|
                                          (LIST 'DIFFERENCE
                                                (AEVAL* (LIST 'PLUS N 1)) I))
                                         (RETURN FORALL-RESULT)))
                                       (SETQ FORALL-RESULT
                                               (TIMES
                                                (DIFFERENCE (TIMES 2 I) 1)
                                                FORALL-RESULT))
                                       (SETQ I
                                               ((LAMBDA (FORALL-RESULT)
                                                  (AEVAL*
                                                   (LIST 'PLUS FORALL-RESULT
                                                         1)))
                                                I))
                                       (GO LAB1))))))
                (SETQ RESULT (AEVAL TERM))))
              (T
               (PROG (KK)
                 (SETQ KK 0)
                LAB
                 (COND
                  ((|AMINUSP:|
                    (LIST 'DIFFERENCE (AEVAL* (LIST 'MINUS (LIST 'PLUS N 2)))
                          KK))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ K (AEVAL* (PLUS KK 1)))
                  (SETQ TERM
                          (AEVAL*
                           (LIST 'TIMES (LIST 'EXPT (MINUS 1) KK)
                                 (LIST 'QUOTIENT
                                       (LIST 'EXPT
                                             (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                                   X)
                                             (TIMES 2 KK))
                                       (LIST 'TIMES
                                             (LIST 'GAMMA
                                                   (LIST 'PLUS KK
                                                         (LIST 'QUOTIENT 3 2)))
                                             (LIST 'GAMMA
                                                   (LIST 'PLUS KK N
                                                         (LIST 'QUOTIENT 3
                                                               2))))))))
                  (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TERM))))
                 (SETQ KK
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          KK))
                 (GO LAB))))
             (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* PRECIS))
                    (PROGN
                     (SETQ TERM
                             (AEVAL*
                              (LIST 'TIMES TERM (LIST 'MINUS '(|:DN:| 25 . -2))
                                    (LIST 'QUOTIENT (LIST 'EXPT X 2)
                                          (LIST 'TIMES
                                                (LIST 'PLUS K '(|:DN:| 5 . -1))
                                                (LIST 'PLUS K N
                                                      '(|:DN:| 5 . -1)))))))
                     (SETQ RESULT (AEVAL* (LIST 'PLUS RESULT TERM)))
                     (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
             (AEVAL 'NIL))
            (AEVAL 'NIL))))
         (RETURN (AEVAL RESULT)))))) 
(FLAG '(STRUVE_COMPUTE_TERM) 'OPFN) 
(AEVAL (NULL (REMPROP 'LAMBERT_W 'SIMPFN))) 
(AEVAL (NULL (REMFLAG '(LAMBERT_W) 'FULL))) 
(AEVAL (OPERATOR (LIST 'LAMBERT_W))) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST (LIST 'REPLACEBY (LIST 'LAMBERT_W 0) 0)
         (LIST 'REPLACEBY (LIST 'LAMBERT_W (LIST 'MINUS (LIST 'QUOTIENT 1 'E)))
               (MINUS 1))
         (LIST 'REPLACEBY
               (LIST 'SUM
                     (LIST 'TIMES
                           (LIST 'QUOTIENT
                                 (LIST 'EXPT (LIST 'MINUS (LIST '~ 'N))
                                       (LIST 'DIFFERENCE 'N 1))
                                 (LIST 'FACTORIAL 'N))
                           (LIST 'EXPT (LIST '~ 'Z) 'N))
                     'N 1 'INFINITY)
               (LIST 'LAMBERT_W 'Z))
         (LIST 'REPLACEBY (LIST 'DF (LIST 'LAMBERT_W (LIST '~ 'Z)) 'Z)
               (LIST 'QUOTIENT 1
                     (LIST 'TIMES (LIST 'PLUS 1 (LIST 'LAMBERT_W 'Z))
                           (LIST 'EXPT 'E (LIST 'LAMBERT_W 'Z)))))
         (LIST 'REPLACEBY (LIST 'LOG (LIST 'LAMBERT_W (LIST '~ 'Z)))
               (LIST 'DIFFERENCE (LIST 'LOG 'Z) (LIST 'LAMBERT_W 'Z)))
         (LIST 'REPLACEBY (LIST 'LAMBERT_W (LIST '~ 'Z))
               (LIST 'WHEN (LIST 'NUM_LAMBERT_W 'Z)
                     (LIST 'AND (LIST 'NUMBERP 'Z)
                           (LIST 'SYMBOLIC '*ROUNDED)))))))) 
(SETK 'LAMBERT_EXP_RULE
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT 'E (LIST 'LAMBERT_W (LIST '~ 'Z)))
                   (LIST 'QUOTIENT (LIST '~ 'Z) (LIST 'LAMBERT_W 'Z)))))) 
(PUT 'NUM_LAMBERT_W 'NUMBER-OF-ARGS 1) 
(FLAG '(NUM_LAMBERT_W) 'OPFN) 
(PUT 'NUM_LAMBERT_W 'DEFINED-ON-LINE '222) 
(PUT 'NUM_LAMBERT_W 'DEFINED-IN-FILE 'SPECFN/SFOTHER.RED) 
(PUT 'NUM_LAMBERT_W 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_LAMBERT_W (Z)
    (COND ((EVALEQUAL (AEVAL Z) 0) 0)
          (T
           (PROG (WJNEW WJ ACCU EXPWJ OLDPREC *COMPLEX OLDDMODE*)
             (SETQ OLDDMODE* DMODE*)
             (AEVAL (ON (LIST 'COMPLEX)))
             (SETQ OLDPREC (AEVAL (LIST 'PRECISION 5)))
             (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
             (COND
              ((EVALLEQ (AEVAL (LIST 'ABS Z)) 1)
               (COND
                ((EVALGEQ (AEVAL Z)
                          (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 'E))))
                 (SETQ WJ (AEVAL 0)))
                (T (SETQ WJ (AEVAL (LIST 'LOG Z))))))
              (T
               (SETQ WJ
                       (AEVAL
                        (LIST 'DIFFERENCE (LIST 'LOG Z)
                              (LIST 'LOG (LIST 'LOG Z)))))))
             (SETQ WJNEW (AEVAL 100))
             (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS WJNEW)) (AEVAL* ACCU))
                    (PROGN
                     (SETQ EXPWJ (AEVAL* (LIST 'EXP WJ)))
                     (SETQ WJNEW
                             (AEVAL*
                              (LIST 'MINUS
                                    (LIST 'QUOTIENT
                                          (LIST 'DIFFERENCE
                                                (LIST 'TIMES WJ EXPWJ) Z)
                                          (LIST 'DIFFERENCE
                                                (LIST 'TIMES EXPWJ
                                                      (LIST 'PLUS WJ 1))
                                                (LIST 'QUOTIENT
                                                      (LIST 'TIMES
                                                            (LIST 'QUOTIENT 1
                                                                  2)
                                                            (LIST 'PLUS WJ 2)
                                                            (LIST 'DIFFERENCE
                                                                  (LIST 'TIMES
                                                                        WJ
                                                                        EXPWJ)
                                                                  Z))
                                                      (LIST 'PLUS WJ 1)))))))
                     (SETQ WJ (AEVAL* (LIST 'PLUS WJ WJNEW)))))
             (AEVAL (LIST 'PRECISION OLDPREC))
             (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
             (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS WJNEW)) (AEVAL* ACCU))
                    (PROGN
                     (SETQ EXPWJ (AEVAL* (LIST 'EXP WJ)))
                     (SETQ WJNEW
                             (AEVAL*
                              (LIST 'MINUS
                                    (LIST 'QUOTIENT
                                          (LIST 'DIFFERENCE
                                                (LIST 'TIMES WJ EXPWJ) Z)
                                          (LIST 'DIFFERENCE
                                                (LIST 'TIMES EXPWJ
                                                      (LIST 'PLUS WJ 1))
                                                (LIST 'QUOTIENT
                                                      (LIST 'TIMES
                                                            (LIST 'QUOTIENT 1
                                                                  2)
                                                            (LIST 'PLUS WJ 2)
                                                            (LIST 'DIFFERENCE
                                                                  (LIST 'TIMES
                                                                        WJ
                                                                        EXPWJ)
                                                                  Z))
                                                      (LIST 'PLUS WJ 1)))))))
                     (SETQ WJ (AEVAL* (LIST 'PLUS WJ WJNEW)))))
             (SETQ DMODE* OLDDMODE*)
             (RETURN (AEVAL WJ)))))) 
(AEVAL 'NIL) 
(ENDMODULE) 