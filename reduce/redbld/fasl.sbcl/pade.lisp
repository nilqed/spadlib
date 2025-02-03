(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PADE)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(LOAD_PACKAGE '(TAYLOR)) 
(LOAD_PACKAGE '(SOLVE)) 
(PUT 'TAYLORP 'NUMBER-OF-ARGS 1) 
(FLAG '(TAYLORP) 'OPFN) 
(PUT 'TAYLORP 'DEFINED-ON-LINE '42) 
(PUT 'TAYLORP 'DEFINED-IN-FILE 'RATAPRX/PADE.RED) 
(PUT 'TAYLORP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLORP (X) (EQCAR X 'TAYLOR)) 
(PUT 'PADE 'NUMBER-OF-ARGS 5) 
(FLAG '(PADE) 'OPFN) 
(PUT 'PADE 'DEFINED-ON-LINE '49) 
(PUT 'PADE 'DEFINED-IN-FILE 'RATAPRX/PADE.RED) 
(PUT 'PADE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PADE (F X H N D)
    (PROG (Y G A B NUMER DENOM VARIABLE_LIST COEFF_LIST TAY_EXPSN SOLNS COUNT
           ANSWER)
      (COND
       ((AND (BOOLVALUE* (REVALX (LIST 'TYPE_RATPOLY F X)))
             (EVALLEQ (AEVAL (LIST 'DEG (LIST 'NUM F) X)) (AEVAL N))
             (EVALLEQ (AEVAL (LIST 'DEG (LIST 'DEN F) X)) (AEVAL D)))
        (RETURN (AEVAL F)))
       (T
        (PROGN
         (SETQ Y (AEVAL (GENSYM)))
         (SETQ A (GENSYM))
         (SETQ B (GENSYM))
         (AEVAL (EVAL (LIST 'OPERATOR (MKQUOTE (LIST A)))))
         (AEVAL (EVAL (LIST 'OPERATOR (MKQUOTE (LIST B)))))
         (SETQ NUMER
                 (PROG (K FORALL-RESULT)
                   (SETQ K 0)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                     (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'TIMES (LIST A K) (LIST 'EXPT Y K)))
                                  FORALL-RESULT)))
                   (SETQ K
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            K))
                   (GO LAB1)))
         (SETQ DENOM
                 (PROG (J FORALL-RESULT)
                   (SETQ J 0)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* D) J))
                     (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'TIMES (LIST B J) (LIST 'EXPT Y J)))
                                  FORALL-RESULT)))
                   (SETQ J
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            J))
                   (GO LAB1)))
         (SETQ VARIABLE_LIST
                 (AEVAL
                  (LIST 'APPEND
                        (PROG (K FORALL-RESULT FORALL-ENDPTR)
                          (SETQ K 0)
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                            (RETURN (MAKELIST NIL))))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS (AEVAL* (LIST A K)) NIL)))
                         LOOPLABEL
                          (SETQ K
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   K))
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K))
                            (RETURN (CONS 'LIST FORALL-RESULT))))
                          (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST A K)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J 0)
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* D) J))
                            (RETURN (MAKELIST NIL))))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS (AEVAL* (LIST B J)) NIL)))
                         LOOPLABEL
                          (SETQ J
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   J))
                          (COND
                           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* D) J))
                            (RETURN (CONS 'LIST FORALL-RESULT))))
                          (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST B J)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))))
         (SETQ G (AEVAL (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS Y H)) F)))
         (SETQ TAY_EXPSN
                 (AEVAL
                  (LIST 'TAYLORTOSTANDARD
                        (LIST 'TAYLOR G Y 0 (LIST 'PLUS N D)))))
         (COND
          ((NOT (FREEOF (REVALX TAY_EXPSN) (REVALX 'DF)))
           (AEVAL (REDERR (REVALX "not yet implemented"))))
          (T
           (PROGN
            (SETQ COEFF_LIST
                    (AEVAL
                     (LIST 'COEFF
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES DENOM (LIST 'NUM TAY_EXPSN))
                                 (LIST 'TIMES NUMER (LIST 'DEN TAY_EXPSN)))
                           Y)))
            (COND
             ((EVALLEQ (AEVAL (LIST 'PLUS N D 1))
                       (AEVAL (LIST 'LENGTH COEFF_LIST)))
              (SETQ COEFF_LIST
                      (PROG (K FORALL-RESULT FORALL-ENDPTR)
                        (SETQ K 1)
                        (COND
                         ((|AMINUSP:|
                           (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS N D 1)) K))
                          (RETURN (MAKELIST NIL))))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         (AEVAL* (LIST 'PART COEFF_LIST K))
                                         NIL)))
                       LOOPLABEL
                        (SETQ K
                                ((LAMBDA (FORALL-RESULT)
                                   (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                 K))
                        (COND
                         ((|AMINUSP:|
                           (LIST 'DIFFERENCE (AEVAL* (LIST 'PLUS N D 1)) K))
                          (RETURN (CONS 'LIST FORALL-RESULT))))
                        (RPLACD FORALL-ENDPTR
                                (CONS (AEVAL* (LIST 'PART COEFF_LIST K)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
            (SETQ SOLNS (AEVAL (LIST 'SOLVE COEFF_LIST VARIABLE_LIST)))
            (SETQ COUNT (AEVAL 0))
            (PROG (R)
              (SETQ R
                      (GETRLIST
                       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Q
                                 (GETRLIST
                                  (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ P (AEVAL* (LIST 'PLUS N 2)))
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE
                                             (AEVAL* (LIST 'PLUS N D 2)) P))
                                      (RETURN (MAKELIST NIL))))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     (AEVAL*
                                                      (LIST 'PART
                                                            (LIST 'FIRST SOLNS)
                                                            P))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ P
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             P))
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE
                                             (AEVAL* (LIST 'PLUS N D 2)) P))
                                      (RETURN (CONS 'LIST FORALL-RESULT))))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             (AEVAL*
                                              (LIST 'PART (LIST 'FIRST SOLNS)
                                                    P))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                         (COND ((NULL Q) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Q)
                                             (AEVAL (LIST 'PART Q 2)))
                                           (CAR Q))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Q (CDR Q))
                         (COND ((NULL Q) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (Q) (AEVAL (LIST 'PART Q 2)))
                                   (CAR Q))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
             LAB
              (COND ((NULL R) (RETURN NIL)))
              ((LAMBDA (R)
                 (COND
                  ((EVALEQUAL (AEVAL R) 0)
                   (SETQ COUNT (AEVAL (LIST 'PLUS COUNT 1))))))
               (CAR R))
              (SETQ R (CDR R))
              (GO LAB))
            (COND
             ((EVALEQUAL (AEVAL COUNT) (AEVAL (LIST 'PLUS D 1)))
              (AEVAL
               (REDERR
                (REVALX "Pade Approximation of this order does not exist"))))
             (T
              (PROGN
               (SETQ ANSWER
                       (AEVAL (LIST 'SUB SOLNS (LIST 'QUOTIENT NUMER DENOM))))
               (COND
                ((BOOLVALUE* (REVALX (LIST 'TAYLORP ANSWER)))
                 (AEVAL (REDERR (REVALX "no Pade Approximation exists"))))
                (T (RETURN (AEVAL (LIST 'SUB (LIST 'EQUAL Y X) ANSWER))))))))
            (AEVAL 'NIL))))
         (AEVAL 'NIL)))))) 
(ENDMODULE) 