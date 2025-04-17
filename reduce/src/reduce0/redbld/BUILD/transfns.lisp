(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TRANSFNS)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'TRIGEXPAND 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIGEXPAND) 'OPFN) 
(PUT 'TRIGEXPAND 'DEFINED-ON-LINE '30) 
(PUT 'TRIGEXPAND 'DEFINED-IN-FILE 'ASSIST/TRANSFNS.RED) 
(PUT 'TRIGEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIGEXPAND (WWS)
    (LIST 'WHEREEXP
          (LIST 'LIST
                (LIST 'LIST
                      (LIST 'REPLACEBY
                            (LIST 'SIN (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                            (LIST 'PLUS
                                  (LIST 'TIMES (LIST 'SIN 'X) (LIST 'COS 'Y))
                                  (LIST 'TIMES (LIST 'COS 'X) (LIST 'SIN 'Y))))
                      (LIST 'REPLACEBY
                            (LIST 'COS (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES (LIST 'COS 'X) (LIST 'COS 'Y))
                                  (LIST 'TIMES (LIST 'SIN 'X) (LIST 'SIN 'Y))))
                      (LIST 'REPLACEBY
                            (LIST 'SIN (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'X)))
                            (LIST 'WHEN
                                  (LIST 'PLUS
                                        (LIST 'TIMES (LIST 'SIN 'X)
                                              (LIST 'COS
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X)))
                                        (LIST 'TIMES (LIST 'COS 'X)
                                              (LIST 'SIN
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X))))
                                  (LIST 'AND (LIST 'FIXP 'N)
                                        (LIST 'GREATERP 'N 1))))
                      (LIST 'REPLACEBY
                            (LIST 'COS (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'X)))
                            (LIST 'WHEN
                                  (LIST 'DIFFERENCE
                                        (LIST 'TIMES (LIST 'COS 'X)
                                              (LIST 'COS
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X)))
                                        (LIST 'TIMES (LIST 'SIN 'X)
                                              (LIST 'SIN
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X))))
                                  (LIST 'AND (LIST 'FIXP 'N)
                                        (LIST 'GREATERP 'N 1))))))
          WWS)) 
(PUT 'HYPEXPAND 'NUMBER-OF-ARGS 1) 
(FLAG '(HYPEXPAND) 'OPFN) 
(PUT 'HYPEXPAND 'DEFINED-ON-LINE '38) 
(PUT 'HYPEXPAND 'DEFINED-IN-FILE 'ASSIST/TRANSFNS.RED) 
(PUT 'HYPEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPEXPAND (WWS)
    (LIST 'WHEREEXP
          (LIST 'LIST
                (LIST 'LIST
                      (LIST 'REPLACEBY
                            (LIST 'SINH (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                            (LIST 'PLUS
                                  (LIST 'TIMES (LIST 'SINH 'X) (LIST 'COSH 'Y))
                                  (LIST 'TIMES (LIST 'COSH 'X)
                                        (LIST 'SINH 'Y))))
                      (LIST 'REPLACEBY
                            (LIST 'COSH (LIST 'PLUS (LIST '~ 'X) (LIST '~ 'Y)))
                            (LIST 'PLUS
                                  (LIST 'TIMES (LIST 'COSH 'X) (LIST 'COSH 'Y))
                                  (LIST 'TIMES (LIST 'SINH 'X)
                                        (LIST 'SINH 'Y))))
                      (LIST 'REPLACEBY
                            (LIST 'SINH
                                  (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'X)))
                            (LIST 'WHEN
                                  (LIST 'PLUS
                                        (LIST 'TIMES (LIST 'SINH 'X)
                                              (LIST 'COSH
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X)))
                                        (LIST 'TIMES (LIST 'COSH 'X)
                                              (LIST 'SINH
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X))))
                                  (LIST 'AND (LIST 'FIXP 'N)
                                        (LIST 'GREATERP 'N 1))))
                      (LIST 'REPLACEBY
                            (LIST 'COSH
                                  (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'X)))
                            (LIST 'WHEN
                                  (LIST 'PLUS
                                        (LIST 'TIMES (LIST 'COSH 'X)
                                              (LIST 'COSH
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X)))
                                        (LIST 'TIMES (LIST 'SINH 'X)
                                              (LIST 'SINH
                                                    (LIST 'TIMES
                                                          (LIST 'DIFFERENCE 'N
                                                                1)
                                                          'X))))
                                  (LIST 'AND (LIST 'FIXP 'N)
                                        (LIST 'GREATERP 'N 1))))))
          WWS)) 
(OPERATOR (LIST '|#EI&|)) 
(SETK (LIST '|#EI&| 0) (AEVAL 1)) 
(SETK '|TRIG#EI&|
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT (LIST '|#EI&| (LIST '~ 'X)) (LIST '~ 'N))
                   (LIST '|#EI&| (LIST 'TIMES 'N 'X)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST '|#EI&| (LIST '~ 'X))
                         (LIST '|#EI&| (LIST '~ 'Y)))
                   (LIST '|#EI&| (LIST 'PLUS 'X 'Y)))))) 
(LET '(|TRIG#EI&|)) 
(PUT 'TRIGREDUCE 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIGREDUCE) 'OPFN) 
(PUT 'TRIGREDUCE 'DEFINED-ON-LINE '53) 
(PUT 'TRIGREDUCE 'DEFINED-IN-FILE 'ASSIST/TRANSFNS.RED) 
(PUT 'TRIGREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIGREDUCE (WWS)
    (PROGN
     (SETQ WWS
             (AEVAL
              (LIST 'WHEREEXP
                    (LIST 'LIST
                          (LIST 'LIST
                                (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'X))
                                      (LIST 'QUOTIENT
                                            (LIST 'PLUS (LIST '|#EI&| 'X)
                                                  (LIST '|#EI&|
                                                        (LIST 'MINUS 'X)))
                                            2))
                                (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'X))
                                      (LIST 'MINUS
                                            (LIST 'TIMES 'I
                                                  (LIST 'QUOTIENT
                                                        (LIST 'DIFFERENCE
                                                              (LIST '|#EI&| 'X)
                                                              (LIST '|#EI&|
                                                                    (LIST
                                                                     'MINUS
                                                                     'X)))
                                                        2))))))
                    WWS)))
     (SETQ WWS
             (AEVAL
              (LIST 'WHEREEXP
                    (LIST 'LIST
                          (LIST 'LIST
                                (LIST 'REPLACEBY (LIST '|#EI&| (LIST '~ 'X))
                                      (LIST 'PLUS (LIST 'COS 'X)
                                            (LIST 'TIMES 'I (LIST 'SIN 'X))))))
                    WWS))))) 
(PUT 'HYPREDUCE 'NUMBER-OF-ARGS 1) 
(FLAG '(HYPREDUCE) 'OPFN) 
(PUT 'HYPREDUCE 'DEFINED-ON-LINE '58) 
(PUT 'HYPREDUCE 'DEFINED-IN-FILE 'ASSIST/TRANSFNS.RED) 
(PUT 'HYPREDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPREDUCE (WWS)
    (PROGN
     (SETQ WWS
             (AEVAL
              (LIST 'WHEREEXP
                    (LIST 'LIST
                          (LIST 'LIST
                                (LIST 'REPLACEBY (LIST 'COSH (LIST '~ 'X))
                                      (LIST 'QUOTIENT
                                            (LIST 'PLUS (LIST '|#EI&| 'X)
                                                  (LIST '|#EI&|
                                                        (LIST 'MINUS 'X)))
                                            2))
                                (LIST 'REPLACEBY (LIST 'SINH (LIST '~ 'X))
                                      (LIST 'QUOTIENT
                                            (LIST 'DIFFERENCE (LIST '|#EI&| 'X)
                                                  (LIST '|#EI&|
                                                        (LIST 'MINUS 'X)))
                                            2))))
                    WWS)))
     (SETQ WWS
             (AEVAL
              (LIST 'WHEREEXP
                    (LIST 'LIST
                          (LIST 'LIST
                                (LIST 'REPLACEBY (LIST '|#EI&| (LIST '~ 'X))
                                      (LIST 'PLUS (LIST 'COSH 'X)
                                            (LIST 'SINH 'X)))))
                    WWS))))) 
(ENDMODULE) 