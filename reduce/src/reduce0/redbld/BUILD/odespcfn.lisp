(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODESPCFN)) 
(AEVAL (OPERATOR (LIST 'ODESOLVE-SPECFN*))) 
(PUT 'ODESOLVE-SPECFN 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SPECFN) 'OPFN) 
(PUT 'ODESOLVE-SPECFN 'DEFINED-ON-LINE '42) 
(PUT 'ODESOLVE-SPECFN 'DEFINED-IN-FILE 'ODESOLVE/ODESPCFN.RED) 
(PUT 'ODESOLVE-SPECFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SPECFN (ODECOEFFS1 DRIVER1 X)
    (PROG (ODE RULES SOLN)
      (AEVAL (TRACEODE1 (LIST "Looking for special-function solutions ...")))
      (SETQ ODE
              (AEVAL
               (LIST 'ODESOLVE-SPECFN* (LIST 'FIRST ODECOEFFS1)
                     (LIST 'SECOND ODECOEFFS1))))
      (SETQ RULES
              (AEVAL
               (LIST 'LIST
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN* (LIST 'MINUS X) 0)
                           (LIST 'ODESOLVE-SOLNS (LIST 'AIRY_AI X)
                                 (LIST 'AIRY_BI X)))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'MINUS (LIST 'TIMES (LIST '~ 'A3) X)) 0)
                           (LIST 'ODESOLVE-SOLNS (LIST 'AIRY_AI X)
                                 (LIST 'AIRY_BI X)
                                 (LIST 'EQUAL X
                                       (LIST 'TIMES
                                             (LIST 'EXPT 'A3
                                                   (LIST 'QUOTIENT 1 3))
                                             X))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'MINUS
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST '~ 'A3) X)
                                             (LIST '~ 'A2B)))
                                 0)
                           (LIST 'ODESOLVE-SOLNS (LIST 'AIRY_AI X)
                                 (LIST 'AIRY_BI X)
                                 (LIST 'EQUAL X
                                       (LIST 'PLUS
                                             (LIST 'TIMES
                                                   (LIST 'EXPT 'A3
                                                         (LIST 'QUOTIENT 1 3))
                                                   X)
                                             (LIST 'QUOTIENT 'A2B
                                                   (LIST 'EXPT 'A3
                                                         (LIST 'QUOTIENT 2
                                                               3)))))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'MINUS
                                       (LIST 'PLUS 1
                                             (LIST 'QUOTIENT (LIST '~ 'N2)
                                                   (LIST 'EXPT X 2))))
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS (LIST 'BESSELI 'N X)
                                 (LIST 'BESSELK 'N X)
                                 (LIST 'EQUAL 'N (LIST 'SQRT 'N2))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'DIFFERENCE 1
                                       (LIST 'QUOTIENT (LIST '~ 'N2)
                                             (LIST 'EXPT X 2)))
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS (LIST 'BESSELJ 'N X)
                                 (LIST 'BESSELY 'N X)
                                 (LIST 'EQUAL 'N (LIST 'SQRT 'N2))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'MINUS
                                       (LIST 'PLUS (LIST '~ 'A2)
                                             (LIST 'QUOTIENT (LIST '~ 'N2)
                                                   (LIST 'EXPT X 2))))
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS
                                 (LIST 'BESSELI 'N (LIST 'TIMES 'A X))
                                 (LIST 'BESSELK 'N (LIST 'TIMES 'A X))
                                 (LIST 'EQUAL 'N (LIST 'SQRT 'N2))
                                 (LIST 'EQUAL 'A (LIST 'SQRT 'A2))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN*
                                 (LIST 'DIFFERENCE (LIST '~ 'A2)
                                       (LIST 'QUOTIENT (LIST '~ 'N2)
                                             (LIST 'EXPT X 2)))
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS
                                 (LIST 'BESSELJ 'N (LIST 'TIMES 'A X))
                                 (LIST 'BESSELY 'N (LIST 'TIMES 'A X))
                                 (LIST 'EQUAL 'N (LIST 'SQRT 'N2))
                                 (LIST 'EQUAL 'A (LIST 'SQRT 'A2))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN* (LIST 'MINUS (LIST '~ 'A2))
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS
                                 (LIST 'BESSELI 0 (LIST 'TIMES 'A X))
                                 (LIST 'BESSELK 0 (LIST 'TIMES 'A X))
                                 (LIST 'EQUAL 'A (LIST 'SQRT 'A2))))
                     (LIST 'REPLACEBY
                           (LIST 'ODESOLVE-SPECFN* (LIST '~ 'A2)
                                 (LIST 'QUOTIENT 1 X))
                           (LIST 'ODESOLVE-SOLNS
                                 (LIST 'BESSELJ 0 (LIST 'TIMES 'A X))
                                 (LIST 'BESSELY 0 (LIST 'TIMES 'A X))
                                 (LIST 'EQUAL 'A (LIST 'SQRT 'A2)))))))
      (SETQ SOLN (AEVAL (LIST 'WHEREEXP (LIST 'LIST RULES) ODE)))
      (COND
       ((EVALNEQ (AEVAL SOLN) (AEVAL ODE))
        (PROGN
         (AEVAL
          (TRACEODE
           (LIST
            "The reduced ODE can be solved in terms of special functions.")))
         (SETQ SOLN (AEVAL (LIST 'PART SOLN 1)))
         (RETURN
          (COND
           ((BOOLVALUE* DRIVER1)
            (AEVAL (LIST 'LIST SOLN (LIST 'ODESOLVE-PI SOLN DRIVER1 X))))
           (T (AEVAL (LIST 'LIST SOLN)))))))))) 
(AEVAL (OPERATOR (LIST 'ODESOLVE-SOLNS*))) 
(FLAG (LIST 'ODESOLVE-SOLNS*) 'LISTARGP) 
(PUT 'ODESOLVE-SOLNS 'PSOPFN 'ODESOLVE-SOLNS) 
(PUT 'ODESOLVE-SOLNS 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-SOLNS 'DEFINED-ON-LINE '117) 
(PUT 'ODESOLVE-SOLNS 'DEFINED-IN-FILE 'ODESOLVE/ODESPCFN.RED) 
(PUT 'ODESOLVE-SOLNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-SOLNS (U)
    (PROG (SOLNS)
      (SETQ SOLNS (LIST 'LIST (CAR U) (CADR U)))
      (COND
       ((SETQ U (CDDR U))
        (PROGN
         (SETQ U (COND ((CDR U) (CONS 'LIST U)) (T (CAR U))))
         (SETQ SOLNS (AEVAL (LIST 'SUB U SOLNS))))))
      (RETURN (LIST 'ODESOLVE-SOLNS* SOLNS)))) 
(ENDMODULE) 