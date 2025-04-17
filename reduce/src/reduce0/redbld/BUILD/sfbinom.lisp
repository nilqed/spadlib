(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFBINOM)) 
(FLAG '(MOTZIN STIRLING1 STIRLING2 BINOMIAL) 'SPECFN) 
(DEFLIST '((MOTZIN 1) (STIRLING1 2) (STIRLING2 2) (BINOMIAL 2)) 'NUMBER-OF-ARGS) 
(DEFLIST '((BINOMIAL SIMPIDEN)) 'SIMPFN) 
(PUT '|:COMPUTE-BINOMIAL| 'NUMBER-OF-ARGS 2) 
(PUT '|:COMPUTE-BINOMIAL| 'DEFINED-ON-LINE '42) 
(PUT '|:COMPUTE-BINOMIAL| 'DEFINED-IN-FILE 'SPECFN/SFBINOM.RED) 
(PUT '|:COMPUTE-BINOMIAL| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:COMPUTE-BINOMIAL| (N K)
    (PROG (NN DD R)
      (SETQ NN
              (PROG (L FORALL-RESULT)
                (SETQ L 0)
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE K 1) L))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT (TIMES (DIFFERENCE N L) FORALL-RESULT))
                (SETQ L (PLUS2 L 1))
                (GO LAB1)))
      (SETQ DD (RNFACTORIAL* (*I2RN K)))
      (SETQ R (QUOTDD NN DD))
      (COND
       ((NULL R)
        (ERRACH
         (LIST "Exact division failed in binomial computation:" NN DD))))
      (RETURN R))) 
(FLAG '(|:COMPUTE-BINOMIAL|) 'OPFN) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN (|:COMPUTE-BINOMIAL| N K) (AND (FIXP N) (FIXP K) (GEQ K 0))))
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN 1 (AND (FIXP N) (FIXP K) (GEQ N K) (EQUAL K 0))))
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN 0 (AND (FIXP N) (FIXP K) (LESSP N K) (GEQ N 0))))
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN 0 (AND (FIXP N) (FIXP K) (LESSP K 0))))
     (REPLACEBY (BINOMIAL (~ N) (~ K))
      (WHEN
       (QUOTIENT (QUOTIENT (GAMMA (PLUS N 1)) (GAMMA (PLUS K 1)))
                 (GAMMA (PLUS (DIFFERENCE N K) 1)))
       (AND (NUMBERP N) (NUMBERP K)
            (NOT (AND (FIXP (DIFFERENCE N K)) (LESSP (DIFFERENCE N K) 0))))))
     (REPLACEBY (DF (BINOMIAL (~ C) (~ K)) C)
      (TIMES (BINOMIAL C K)
             (DIFFERENCE (PSI (PLUS 1 C)) (PSI (PLUS 1 (DIFFERENCE C K)))))))))) 
(AEVAL (OPERATOR (LIST 'STIRLING1 'STIRLING2))) 
(AEVAL
 (LET
  '((LIST (REPLACEBY (STIRLING1 (~ N) (~ N)) 1)
          (REPLACEBY (STIRLING1 (~ N) 0) (WHEN 0 (NOT (EQUAL N 0))))
          (REPLACEBY (STIRLING1 (~ N) (DIFFERENCE (~ N) 1))
           (MINUS (BINOMIAL N 2)))
          (REPLACEBY (STIRLING1 (~ N) (~ M))
           (WHEN 0 (AND (FIXP N) (FIXP M) (LESSP N M))))
          (REPLACEBY (STIRLING1 (~ N) (~ M))
           (WHEN
            (PROG (K FORALL-RESULT)
              (SETQ K 0)
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND
               ((|AMINUSP:|
                 (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE 'N 'M)) K))
                (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             (AEVAL*
                              (LIST 'TIMES (LIST 'EXPT (MINUS 1) K)
                                    (LIST 'BINOMIAL
                                          (LIST 'PLUS (LIST 'DIFFERENCE 'N 1)
                                                K)
                                          (LIST 'PLUS (LIST 'DIFFERENCE 'N 'M)
                                                K))
                                    (LIST 'BINOMIAL
                                          (LIST 'DIFFERENCE (LIST 'TIMES 2 'N)
                                                'M)
                                          (LIST 'DIFFERENCE
                                                (LIST 'DIFFERENCE 'N 'M) K))
                                    (LIST 'STIRLING2
                                          (LIST 'PLUS (LIST 'DIFFERENCE 'N 'M)
                                                K)
                                          K)))
                             FORALL-RESULT)))
              (SETQ K
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       K))
              (GO LAB1))
            (AND (FIXP N) (FIXP M) (GREATERP N M))))
          (REPLACEBY (STIRLING2 (~ N) (~ N)) 1)
          (REPLACEBY (STIRLING2 (~ N) 0) (WHEN 0 (NOT (EQUAL N 0))))
          (REPLACEBY (STIRLING2 (~ N) (DIFFERENCE (~ N) 1)) (BINOMIAL N 2))
          (REPLACEBY (STIRLING2 (~ N) (~ M))
           (WHEN 0 (AND (FIXP N) (FIXP M) (LESSP N M))))
          (REPLACEBY (STIRLING2 (~ N) (~ M))
           (WHEN (|CALC:STIRLING2| N M)
            (AND (FIXP N) (FIXP M) (GREATERP N M)))))))) 
(PUT '|CALC:STIRLING2| 'NUMBER-OF-ARGS 2) 
(FLAG '(|CALC:STIRLING2|) 'OPFN) 
(PUT '|CALC:STIRLING2| 'DEFINED-ON-LINE '90) 
(PUT '|CALC:STIRLING2| 'DEFINED-IN-FILE 'SPECFN/SFBINOM.RED) 
(PUT '|CALC:STIRLING2| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |CALC:STIRLING2| (N M)
    (PROG (BIN_ROW)
      (SETQ BIN_ROW (AEVAL (LIST 'BINOMIAL_ROW M)))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT
              (PROG (K FORALL-RESULT)
                (SETQ K 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* M) K))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'TIMES
                                      (LIST 'EXPT (MINUS 1)
                                            (LIST 'DIFFERENCE M K))
                                      (LIST 'PART BIN_ROW (PLUS K 1))
                                      (LIST 'EXPT K N)))
                               FORALL-RESULT)))
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (GO LAB1))
              (LIST 'FACTORIAL M)))))) 
(PUT 'STIRLING1 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'STIRLING2 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'STIRLING1 'PLAIN-FUNCTIONSYMBOL 'S) 
(PUT 'STIRLING2 'PLAIN-FUNCTIONSYMBOL '|s|) 
(PUT 'BINOMIAL_ROW 'NUMBER-OF-ARGS 1) 
(PUT 'BINOMIAL_ROW 'DEFINED-ON-LINE '107) 
(PUT 'BINOMIAL_ROW 'DEFINED-IN-FILE 'SPECFN/SFBINOM.RED) 
(PUT 'BINOMIAL_ROW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BINOMIAL_ROW (N)
    (PROG (LIST_OF_BINCOEFF NEWLIST OLD CURR)
      (COND ((OR (NOT (FIXP N)) (LESSP N 0)) (RETURN NIL)))
      (SETQ LIST_OF_BINCOEFF (LIST 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N 0)) (RETURN NIL)))
        (PROGN
         (SETQ OLD 0)
         (SETQ NEWLIST (LIST))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (EQUAL LIST_OF_BINCOEFF (LIST)))) (RETURN NIL)))
           (PROGN
            (SETQ CURR (CAR LIST_OF_BINCOEFF))
            (SETQ NEWLIST (CONS (PLUS OLD CURR) NEWLIST))
            (SETQ OLD CURR)
            (SETQ LIST_OF_BINCOEFF (REST LIST_OF_BINCOEFF))
            NIL)
           (GO WHILELABEL))
         (SETQ LIST_OF_BINCOEFF (CONS 1 NEWLIST))
         (SETQ N (DIFFERENCE N 1)))
        (GO WHILELABEL))
      (RETURN (CONS 'LIST LIST_OF_BINCOEFF)))) 
(FLAG '(BINOMIAL_ROW) 'OPFN) 
(PUT 'MOTZKIN 'NUMBER-OF-ARGS 1) 
(PUT 'MOTZKIN 'DEFINED-ON-LINE '135) 
(PUT 'MOTZKIN 'DEFINED-IN-FILE 'SPECFN/SFBINOM.RED) 
(PUT 'MOTZKIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOTZKIN (N)
    (COND ((EQUAL (SETQ N (REVAL1 N T)) 0) 1) ((EQUAL N 1) 1)
          ((OR (NOT (FIXP N)) (LESSP N 0))
           (MK*SQ
            (CONS (LIST (CONS (GETPOWER (FKERN (LIST 'MOTZKIN N)) 1) 1)) 1)))
          (T
           (PROG (VSOP OLDV NEWV)
             (SETQ NEWV (SETQ OLDV 1))
             (PROG (I)
               (SETQ I 2)
              LAB
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (PROGN
                (SETQ VSOP OLDV)
                (SETQ OLDV NEWV)
                (SETQ NEWV
                        (QUOTIENT
                         (PLUS (TIMES (DIFFERENCE (TIMES 3 I) 3) VSOP)
                               (TIMES (PLUS (TIMES 2 I) 1) OLDV))
                         (PLUS I 2)))
                NIL)
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN NEWV))))) 
(FLAG '(MOTZKIN) 'OPFN) 
(ENDMODULE) 