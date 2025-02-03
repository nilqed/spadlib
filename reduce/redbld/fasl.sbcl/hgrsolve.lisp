(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HYPERGEOMRSOLVE)) 
(FLUID '(*TRACEFPS)) 
(PUT 'HYPERGEOMRSOLVE 'NUMBER-OF-ARGS 3) 
(FLAG '(HYPERGEOMRSOLVE) 'OPFN) 
(PUT 'HYPERGEOMRSOLVE 'DEFINED-ON-LINE '30) 
(PUT 'HYPERGEOMRSOLVE 'DEFINED-IN-FILE 'SPECFN/HGRSOLVE.RED) 
(PUT 'HYPERGEOMRSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HYPERGEOMRSOLVE (R K A0)
    (PROG (RE NNN DDD C P Q AK SOLS II)
      (SETQ P (AEVAL (LIST 'LIST)))
      (SETQ Q (AEVAL (LIST 'LIST)))
      (SETQ C (AEVAL 1))
      (SETQ RE (AEVAL (LIST 'TIMES R (LIST 'PLUS K 1))))
      (SETQ NNN (AEVAL (LIST 'OLD_FACTORIZE (LIST 'NUM RE))))
      (SETQ DDD (AEVAL (LIST 'OLD_FACTORIZE (LIST 'DEN RE))))
      (PROG (NN)
        (SETQ NN (GETRLIST (AEVAL NNN)))
       LAB
        (COND ((NULL NN) (RETURN NIL)))
        ((LAMBDA (NN)
           (COND
            ((FREEOF (REVALX NN) (REVALX K))
             (SETQ C (AEVAL (LIST 'TIMES C NN))))
            ((EVALEQUAL (AEVAL (LIST 'DEG NN K)) 1)
             (PROGN
              (SETQ C (AEVAL (LIST 'TIMES C (LIST 'COEFFN NN K 1))))
              (SETQ P
                      (AEVAL
                       (LIST 'APPEND P
                             (LIST 'LIST
                                   (LIST 'QUOTIENT (LIST 'COEFFN NN K 0)
                                         (LIST 'COEFFN NN K 1))))))))
            ((EVALEQUAL (AEVAL (LIST 'DEG NN K)) 2)
             (PROGN
              (SETQ C (AEVAL (LIST 'TIMES C (LIST 'LCOF NN K))))
              (SETQ SOLS (AEVAL (LIST 'SOLVE NN K)))
              (PROG (S)
                (SETQ S (GETRLIST (AEVAL SOLS)))
               LAB
                (COND ((NULL S) (RETURN NIL)))
                ((LAMBDA (S)
                   (PROGN
                    (PROG (I)
                      (SETQ I 1)
                     LAB
                      (COND
                       ((|AMINUSP:|
                         (LIST 'DIFFERENCE
                               (AEVAL* (LIST 'FIRST MULTIPLICITIES*)) I))
                        (RETURN NIL)))
                      (SETQ P
                              (AEVAL*
                               (LIST 'CONS (LIST 'MINUS (LIST 'RHS S)) P)))
                      (SETQ I
                              ((LAMBDA (FORALL-RESULT)
                                 (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                               I))
                      (GO LAB))
                    (SETQ MULTIPLICITIES*
                            (PROGN
                             (SETQ ALGLIST* (CONS NIL NIL))
                             (AEVAL (LIST 'REST MULTIPLICITIES*))))
                    (AEVAL 'NIL)))
                 (CAR S))
                (SETQ S (CDR S))
                (GO LAB))))
            (T (AEVAL (REDERR (REVALX " hypergeomRsolve failed"))))))
         (CAR NN))
        (SETQ NN (CDR NN))
        (GO LAB))
      (PROG (DD)
        (SETQ DD (GETRLIST (AEVAL DDD)))
       LAB
        (COND ((NULL DD) (RETURN NIL)))
        ((LAMBDA (DD)
           (COND
            ((FREEOF (REVALX DD) (REVALX K))
             (SETQ C (AEVAL (LIST 'QUOTIENT C DD))))
            ((EVALEQUAL (AEVAL (LIST 'DEG DD K)) 1)
             (PROGN
              (SETQ C (AEVAL (LIST 'QUOTIENT C (LIST 'COEFFN DD K 1))))
              (SETQ Q
                      (AEVAL
                       (LIST 'APPEND Q
                             (LIST 'LIST
                                   (LIST 'QUOTIENT (LIST 'COEFFN DD K 0)
                                         (LIST 'COEFFN DD K 1))))))))
            ((EVALEQUAL (AEVAL (LIST 'DEG DD K)) 2)
             (PROGN
              (SETQ C (AEVAL (LIST 'QUOTIENT C (LIST 'LCOF DD K))))
              (SETQ SOLS (AEVAL (LIST 'SOLVE DD K)))
              (PROG (S)
                (SETQ S (GETRLIST (AEVAL SOLS)))
               LAB
                (COND ((NULL S) (RETURN NIL)))
                ((LAMBDA (S)
                   (PROGN
                    (PROG (I)
                      (SETQ I 1)
                     LAB
                      (COND
                       ((|AMINUSP:|
                         (LIST 'DIFFERENCE
                               (AEVAL* (LIST 'FIRST MULTIPLICITIES*)) I))
                        (RETURN NIL)))
                      (SETQ Q
                              (AEVAL*
                               (LIST 'CONS (LIST 'MINUS (LIST 'RHS S)) Q)))
                      (SETQ I
                              ((LAMBDA (FORALL-RESULT)
                                 (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                               I))
                      (GO LAB))
                    (SETQ MULTIPLICITIES*
                            (PROGN
                             (SETQ ALGLIST* (CONS NIL NIL))
                             (AEVAL (LIST 'REST MULTIPLICITIES*))))
                    (AEVAL 'NIL)))
                 (CAR S))
                (SETQ S (CDR S))
                (GO LAB))
              (AEVAL 'NIL)))
            (T (AEVAL (REDERR (REVALX " hypergeomRsolve failed"))))))
         (CAR DD))
        (SETQ DD (CDR DD))
        (GO LAB))
      (SETK 'RSOLVE (AEVAL 'INFINITE))
      (PROG (S)
        (SETQ S (GETRLIST (AEVAL P)))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND
            ((AND (FIXP (REVALX S)) (EVALLESSP (AEVAL S) 0))
             (SETK 'RSOLVE (AEVAL 'FINITE)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (COND
       ((BOOLVALUE* (REVALX *TRACEFPS))
        (PROGN
         (ASSGNPRI (AEVAL "RSOLVE  = ") NIL 'FIRST)
         (ASSGNPRI (AEVAL 'RSOLVE) NIL 'LAST))))
      (SETQ P
              (PROG (S FORALL-RESULT)
                (SETQ S (GETRLIST (AEVAL P)))
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'TIMES
                               ((LAMBDA (S) (AEVAL (LIST 'POCHHAMMER S K)))
                                (CAR S))
                               FORALL-RESULT)))
                (SETQ S (CDR S))
                (GO LAB1)))
      (SETQ Q
              (PROG (S FORALL-RESULT)
                (SETQ S (GETRLIST (AEVAL Q)))
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'TIMES
                               ((LAMBDA (S) (AEVAL (LIST 'POCHHAMMER S K)))
                                (CAR S))
                               FORALL-RESULT)))
                (SETQ S (CDR S))
                (GO LAB1)))
      (SETQ AK
              (AEVAL
               (LIST 'TIMES A0 (LIST 'EXPT C K)
                     (LIST 'QUOTIENT P (LIST 'TIMES Q (LIST 'FACTORIAL K))))))
      (RETURN (AEVAL AK)))) 
(SETK 'HGSPEC_POCHHAMMER
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'KK) (LIST '~ 'NN))
                   (LIST 'WHEN 1 (LIST 'EQUAL 'NN 0)))
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'KK) 'NN)
                   (LIST 'WHEN 0 (LIST 'EQUAL 'KK 0)))
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'KK) 'NN)
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'NN)
                               (LIST 'QUOTIENT
                                     (LIST 'FACTORIAL (LIST 'MINUS 'KK))
                                     (LIST 'FACTORIAL
                                           (LIST 'DIFFERENCE (LIST 'MINUS 'KK)
                                                 'NN))))
                         (LIST 'AND (LIST 'FIXP 'KK) (LIST 'LEQ 'KK 0))))
             (LIST 'REPLACEBY (LIST 'POCHHAMMER (LIST '~ 'KK) 'NN)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS 'KK (LIST 'DIFFERENCE 'NN 1)))
                               (LIST 'FACTORIAL (LIST 'DIFFERENCE 'KK 1)))
                         (LIST 'FIXP 'KK)))
             (LIST 'REPLACEBY
                   (LIST 'POCHHAMMER (LIST '~ 'KK)
                         (LIST 'TIMES (LIST '~ 'W) 'NN))
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'FACTORIAL
                                     (LIST 'PLUS 'KK
                                           (LIST 'DIFFERENCE
                                                 (LIST 'TIMES 'W 'NN) 1)))
                               (LIST 'FACTORIAL (LIST 'DIFFERENCE 'KK 1)))
                         (LIST 'FIXP 'KK)))))) 
(ENDMODULE) 