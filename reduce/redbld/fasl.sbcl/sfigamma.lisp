(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'IGAMMA)) 
(FLUID '(|:SFITERATIONS|)) 
(GLOBAL '(|IBETA:MAX:ITER|)) 
(SETK '|IBETA:MAX:ITER| (AEVAL 200)) 
(PUT '|IGAMMA:ITER:SERIES| 'NUMBER-OF-ARGS 5) 
(FLAG '(|IGAMMA:ITER:SERIES|) 'OPFN) 
(PUT '|IGAMMA:ITER:SERIES| 'DEFINED-ON-LINE '94) 
(PUT '|IGAMMA:ITER:SERIES| 'DEFINED-IN-FILE 'SPECFN/SFIGAMMA.RED) 
(PUT '|IGAMMA:ITER:SERIES| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |IGAMMA:ITER:SERIES| (A X ITER SUM LAST_TERM)
    (PROG (VALUE THIS_TERM)
      (COND
       ((EVALLESSP (AEVAL LAST_TERM)
                   (AEVAL
                    (LIST 'EXPT 10
                          (LIST 'MINUS (LIST 'PLUS (LIST 'PRECISION 0) 3)))))
        (SETQ VALUE (AEVAL SUM)))
       (T
        (PROGN
         (SETQ THIS_TERM
                 (AEVAL
                  (LIST 'TIMES LAST_TERM
                        (LIST 'QUOTIENT X (LIST 'PLUS A ITER)))))
         (SETQ VALUE
                 (AEVAL
                  (LIST '|IGAMMA:ITER:SERIES| A X (LIST 'PLUS ITER 1)
                        (LIST 'PLUS SUM THIS_TERM) THIS_TERM))))))
      (RETURN (AEVAL VALUE)))) 
(PUT '|IGAMMA:CONT:FRAC| 'NUMBER-OF-ARGS 4) 
(FLAG '(|IGAMMA:CONT:FRAC|) 'OPFN) 
(PUT '|IGAMMA:CONT:FRAC| 'DEFINED-ON-LINE '117) 
(PUT '|IGAMMA:CONT:FRAC| 'DEFINED-IN-FILE 'SPECFN/SFIGAMMA.RED) 
(PUT '|IGAMMA:CONT:FRAC| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |IGAMMA:CONT:FRAC| (A X ITER ITER_MAX)
    (PROG (VALUE)
      (COND
       ((EVALGREATERP (AEVAL ITER) (AEVAL ITER_MAX)) (SETQ VALUE (AEVAL 0)))
       (T
        (SETQ VALUE
                (AEVAL
                 (LIST 'QUOTIENT (LIST 'DIFFERENCE ITER A)
                       (LIST 'PLUS 1
                             (LIST 'QUOTIENT ITER
                                   (LIST 'PLUS X
                                         (LIST '|IGAMMA:CONT:FRAC| A X
                                               (LIST 'PLUS ITER 1)
                                               ITER_MAX)))))))))
      (RETURN (AEVAL VALUE)))) 
(PUT '|IGAMMA:EVAL| 'NUMBER-OF-ARGS 2) 
(FLAG '(|IGAMMA:EVAL|) 'OPFN) 
(PUT '|IGAMMA:EVAL| 'DEFINED-ON-LINE '135) 
(PUT '|IGAMMA:EVAL| 'DEFINED-IN-FILE 'SPECFN/SFIGAMMA.RED) 
(PUT '|IGAMMA:EVAL| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |IGAMMA:EVAL| (A X)
    (PROG (ARG FRAC |LAST:FRAC| ACC VALUE)
      (COND
       ((OR (EVALLEQ (AEVAL X) 1)
            (EVALLESSP (AEVAL X) (AEVAL (LIST 'PLUS A 2))))
        (PROGN
         (SETQ VALUE
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'TIMES (LIST 'EXP (LIST 'MINUS X))
                              (LIST 'EXPT X A))
                        (LIST 'QUOTIENT
                              (LIST 'PLUS 1
                                    (LIST '|IGAMMA:ITER:SERIES| A X 1 0 1))
                              (LIST 'GAMMA (LIST 'PLUS A 1))))))))
       (T
        (PROGN
         (SETQ ACC
                 (AEVAL
                  (LIST 'EXPT 10
                        (LIST 'MINUS (LIST 'PLUS (LIST 'PRECISION 0) 3)))))
         (SETQ FRAC (AEVAL (LIST '|IGAMMA:CONT:FRAC| A X 1 1)))
         (SETK '|:SFITERATIONS| (AEVAL 1))
         (REPEAT
          (PROGN
           (SETK '|:SFITERATIONS| (AEVAL* (LIST 'PLUS '|:SFITERATIONS| 1)))
           (SETQ |LAST:FRAC| (AEVAL* FRAC))
           (SETQ FRAC
                   (AEVAL* (LIST '|IGAMMA:CONT:FRAC| A X 1 '|:SFITERATIONS|))))
          (EVALLESSP (AEVAL* (LIST 'DIFFERENCE |LAST:FRAC| FRAC))
                     (AEVAL* ACC)))
         (SETQ ARG
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXP (LIST 'MINUS X))
                        (LIST 'QUOTIENT (LIST 'EXPT X A) (LIST 'GAMMA A)))))
         (SETQ VALUE
                 (AEVAL
                  (LIST 'DIFFERENCE 1
                        (LIST 'QUOTIENT ARG (LIST 'PLUS X FRAC))))))))
      (RETURN (AEVAL VALUE)))) 
(PUT '|IBETA:CONT:FRAC| 'NUMBER-OF-ARGS 5) 
(FLAG '(|IBETA:CONT:FRAC|) 'OPFN) 
(PUT '|IBETA:CONT:FRAC| 'DEFINED-ON-LINE '207) 
(PUT '|IBETA:CONT:FRAC| 'DEFINED-IN-FILE 'SPECFN/SFIGAMMA.RED) 
(PUT '|IBETA:CONT:FRAC| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |IBETA:CONT:FRAC| (ITER_FIRST ITER_MAX A B X)
    (PROG (ITER VALUE C_ODD C_EVEN)
      (COND
       ((NOT
         (AND (FIXP (REVALX ITER_FIRST)) (FIXP (REVALX ITER_MAX))
              (EVALNUMBERP (AEVAL X))))
        (AEVAL (REDERR (REVALX "ibeta!:cont!:frac called illegally")))))
      (SETQ VALUE (AEVAL 0))
      (SETQ ITER (AEVAL ITER_MAX))
      (WHILE (EVALGEQ (AEVAL* ITER) (AEVAL* ITER_FIRST))
             (PROGN
              (SETQ C_EVEN
                      (AEVAL*
                       (LIST 'MINUS
                             (LIST 'TIMES
                                   (LIST 'PLUS A (LIST 'DIFFERENCE ITER 1))
                                   (LIST 'DIFFERENCE B ITER)
                                   (LIST 'QUOTIENT X
                                         (LIST 'TIMES
                                               (LIST 'PLUS A
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 ITER)
                                                           2))
                                               (LIST 'PLUS A
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 ITER)
                                                           1))
                                               (LIST 'DIFFERENCE 1 X)))))))
              (SETQ C_ODD
                      (AEVAL*
                       (LIST 'TIMES ITER
                             (LIST 'PLUS A B (LIST 'DIFFERENCE ITER 1))
                             (LIST 'QUOTIENT X
                                   (LIST 'TIMES
                                         (LIST 'PLUS A
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2 ITER) 1))
                                         (LIST 'PLUS A (LIST 'TIMES 2 ITER))
                                         (LIST 'DIFFERENCE 1 X))))))
              (SETQ VALUE
                      (AEVAL*
                       (LIST 'QUOTIENT C_EVEN
                             (LIST 'PLUS 1
                                   (LIST 'QUOTIENT C_ODD
                                         (LIST 'PLUS 1 VALUE))))))
              (SETQ ITER (AEVAL* (LIST 'DIFFERENCE ITER 1)))))
      (RETURN (AEVAL VALUE)))) 
(PUT '|IBETA:EVAL| 'NUMBER-OF-ARGS 3) 
(FLAG '(|IBETA:EVAL|) 'OPFN) 
(PUT '|IBETA:EVAL| 'DEFINED-ON-LINE '231) 
(PUT '|IBETA:EVAL| 'DEFINED-IN-FILE 'SPECFN/SFIGAMMA.RED) 
(PUT '|IBETA:EVAL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |IBETA:EVAL| (A B X)
    (PROG (LAST_VALUE VALUE ARG |:SFITERATIONS|)
      (COND
       ((OR (EVALEQUAL (AEVAL X) 0) (EVALEQUAL (AEVAL X) 1))
        (SETQ VALUE (AEVAL X)))
       (T
        (PROGN
         (COND
          ((EVALGREATERP
            (AEVAL
             (LIST 'TIMES (LIST 'DIFFERENCE (LIST 'REPART (LIST 'PLUS A B)) 2)
                   X))
            (AEVAL (LIST 'DIFFERENCE (LIST 'REPART A) 1)))
           (SETQ VALUE
                   (AEVAL
                    (LIST 'DIFFERENCE 1
                          (LIST 'IBETA B A (LIST 'DIFFERENCE 1 X))))))
          (T
           (PROGN
            (SETQ ARG
                    (AEVAL
                     (LIST 'TIMES (LIST 'GAMMA (LIST 'PLUS A B))
                           (LIST 'EXPT X A)
                           (LIST 'QUOTIENT
                                 (LIST 'EXPT (LIST 'DIFFERENCE 1 X)
                                       (LIST 'DIFFERENCE B 1))
                                 (LIST 'TIMES A (LIST 'GAMMA A)
                                       (LIST 'GAMMA B))))))
            (SETQ |:SFITERATIONS| (AEVAL 30))
            (SETQ VALUE (AEVAL (MINUS 1)))
            (REPEAT
             (PROGN
              (SETQ LAST_VALUE (AEVAL* VALUE))
              (SETQ VALUE
                      (AEVAL*
                       (LIST 'TIMES ARG
                             (LIST 'QUOTIENT 1
                                   (LIST 'PLUS 1
                                         (LIST '|IBETA:CONT:FRAC| 1
                                               |:SFITERATIONS| A B X))))))
              (SETQ |:SFITERATIONS| (AEVAL* (LIST 'PLUS |:SFITERATIONS| 10))))
             (OR
              (EVALLESSP
               (AEVAL* (LIST 'ABS (LIST 'DIFFERENCE VALUE LAST_VALUE)))
               (AEVAL*
                (LIST 'EXPT 10
                      (LIST 'MINUS (LIST 'PLUS (LIST 'PRECISION 0) 3)))))
              (EVALGREATERP (AEVAL* |:SFITERATIONS|)
                            (AEVAL* '|IBETA:MAX:ITER|))))
            (AEVAL 'NIL)))))))
      (COND
       ((EVALGREATERP (AEVAL |:SFITERATIONS|) (AEVAL '|IBETA:MAX:ITER|))
        (ASSGNPRI
         (AEVAL
          "*** Warning: max iteration limit exceeded; result may not be accurate")
         NIL 'ONLY)))
      (RETURN (AEVAL VALUE)))) 
(AEVAL 'NIL) 
(ENDMODULE) 