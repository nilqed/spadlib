(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTD)) 
(FLUID '(MELLINCOEF MELLIN-COEFFICIENTS* MELLIN-TRANSFORMS*)) 
(LOAD_PACKAGE (LIST 'SPECFN 'SFGAMMA)) 
(PUT 'SIMPINTGGGG 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINTGGGG 'DEFINED-ON-LINE '34) 
(PUT 'SIMPINTGGGG 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'SIMPINTGGGG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINTGGGG (U)
    (PROG (FF1 FF2 ALPHA VAR CHOSEN_NUM COEF TEMP CONST RESULT)
      (SETQ U (DEFINT_REFORM U))
      (SETQ CONST (CAR U))
      (COND ((EQUAL CONST 0) (SETQ RESULT (CONS NIL 1)))
            (T
             (PROGN
              (SETQ U (CDR U))
              (COND ((NEQ (LENGTH U) 4) (REDERR "Integration failed")))
              (COND ((EQUAL (CAR U) 0) (SETQ FF1 '(0 0 X)))
                    (T (SETQ FF1 (PREPSQ (SIMP (CAR U))))))
              (COND ((EQUAL (CADR U) 0) (SETQ FF2 '(0 0 X)))
                    (T (SETQ FF2 (PREPSQ (SIMP (CADR U))))))
              (COND ((EQUAL FF1 'UNKNOWN) (RETURN (SIMP 'UNKNOWN))))
              (COND ((EQUAL FF2 'UNKNOWN) (RETURN (SIMP 'UNKNOWN))))
              (SETQ ALPHA (CADDR U))
              (SETQ VAR (CADDDR U))
              (COND
               ((OR (EQUAL (CAR FF1) '|DEFINT:OPF31|)
                    (EQUAL (CAR FF1) '|DEFINT:OPF32|))
                (PROGN
                 (PUT '|DEFINT:OPF1| 'G (SPEC_LOG FF1))
                 (SETQ MELLINCOEF (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 1))))
               (T
                (PROGN
                 (SETQ CHOSEN_NUM (CADR FF1))
                 (PUT '|DEFINT:OPF1| 'G (GETV MELLIN-TRANSFORMS* CHOSEN_NUM))
                 (SETQ COEF (GETV MELLIN-COEFFICIENTS* CHOSEN_NUM))
                 (COND
                  (COEF
                   (SETQ MELLINCOEF
                           (PROGN (SETQ ALGLIST* (CONS NIL NIL)) COEF)))
                  (T
                   (SETQ MELLINCOEF
                           (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 1)))))))
              (COND
               ((OR (EQUAL (CAR FF2) '|DEFINT:OPF31|)
                    (EQUAL (CAR FF2) '|DEFINT:OPF32|))
                (PUT '|DEFINT:OPF2| 'G (SPEC_LOG FF2)))
               (T
                (PROGN
                 (SETQ CHOSEN_NUM (CADR FF2))
                 (PUT '|DEFINT:OPF2| 'G (GETV MELLIN-TRANSFORMS* CHOSEN_NUM))
                 (SETQ COEF (GETV MELLIN-COEFFICIENTS* CHOSEN_NUM))
                 (COND
                  (COEF
                   (SETQ MELLINCOEF
                           (PROGN
                            (SETQ ALGLIST* (CONS NIL NIL))
                            (TIMES COEF MELLINCOEF))))))))
              (SETQ TEMP
                      (SIMP
                       (LIST 'INTGG (CONS '|DEFINT:OPF1| (CDDR FF1))
                             (CONS '|DEFINT:OPF2| (CDDR FF2)) ALPHA VAR)))
              (SETQ TEMP (PREPSQ TEMP))
              (COND
               ((NEQ TEMP 'UNKNOWN)
                (SETQ RESULT (REVAL1 (AEVAL (LIST 'TIMES CONST TEMP)) T)))
               (T (SETQ RESULT TEMP)))
              (SETQ RESULT (SIMP* RESULT))
              NIL)))
      (RETURN RESULT))) 
(PUT 'SPEC_LOG 'NUMBER-OF-ARGS 1) 
(PUT 'SPEC_LOG 'DEFINED-ON-LINE '83) 
(PUT 'SPEC_LOG 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'SPEC_LOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPEC_LOG (LS)
    (PROG (N NUM DENOM MELLIN)
      (SETQ N (CADR LS))
      (SETQ NUM
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 1 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS 1 NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DENOM
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EQUAL (CAR LS) '|DEFINT:OPF31|)
        (SETQ MELLIN
                (LIST (LIST) (LIST (PLUS N 1) 0 (PLUS N 1) (PLUS N 1)) NUM
                      DENOM (TIMES (EXPT (MINUS 1) N) (FACTORIAL N)) 'X)))
       (T
        (SETQ MELLIN
                (LIST (LIST) (LIST 0 (PLUS N 1) (PLUS N 1) (PLUS N 1)) NUM
                      DENOM (FACTORIAL N) 'X))))
      (RETURN MELLIN))) 
(AEVAL
 (OPERATOR
  (LIST 'LAPLACE2 'Y_TRANSFORM2 'K_TRANSFORM2 'STRUVEH_TRANSFORM2 'HANKELT
        'FOURIER_SIN2 'FOURIER_COS2))) 
(SETK 'GAMMA_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'GAMMA
                         (LIST 'PLUS (LIST 'QUOTIENT (LIST '~ 'N) 2)
                               (LIST 'QUOTIENT 1 2)))
                   (LIST 'TIMES
                         (LIST 'QUOTIENT (LIST 'GAMMA 'N)
                               (LIST 'TIMES
                                     (LIST 'EXPT 2 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'GAMMA (LIST 'QUOTIENT 'N 2))))
                         (LIST 'GAMMA (LIST 'QUOTIENT 1 2))))
             (LIST 'REPLACEBY
                   (LIST 'GAMMA (LIST 'PLUS (LIST 'QUOTIENT (LIST '~ 'N) 2) 1))
                   (LIST 'TIMES (LIST 'QUOTIENT 'N 2)
                         (LIST 'GAMMA (LIST 'TIMES (LIST 'QUOTIENT 1 2) 'N))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'GAMMA (LIST 'QUOTIENT 3 4))
                         (LIST 'GAMMA (LIST 'QUOTIENT 1 4)))
                   (LIST 'TIMES 'PI (LIST 'SQRT 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'GAMMA (LIST '~ 'N))
                         (LIST 'QUOTIENT (LIST '~ 'N)
                               (LIST 'GAMMA (LIST 'PLUS (LIST '~ 'N) 1))))
                   1)))) 
(AEVAL (LET '(GAMMA_RULES))) 
(SETK 'FACTORIAL_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'FACTORIAL (LIST '~ 'A))
                   (LIST 'GAMMA (LIST 'PLUS 'A 1)))))) 
(AEVAL (LET '(FACTORIAL_RULES))) 
(AEVAL 'NIL) 
(PUT 'LAPLACE_TRANSFORM 'PSOPFN 'NEW_LAPLACE) 
(PUT 'NEW_LAPLACE 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_LAPLACE 'DEFINED-ON-LINE '127) 
(PUT 'NEW_LAPLACE 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_LAPLACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_LAPLACE (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'LAPLACE2 LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'HANKEL_TRANSFORM 'PSOPFN 'NEW_HANKEL) 
(PUT 'NEW_HANKEL 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_HANKEL 'DEFINED-ON-LINE '140) 
(PUT 'NEW_HANKEL 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_HANKEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_HANKEL (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'HANKELT LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'Y_TRANSFORM 'PSOPFN 'NEW_Y_TRANSFORM) 
(PUT 'NEW_Y_TRANSFORM 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_Y_TRANSFORM 'DEFINED-ON-LINE '153) 
(PUT 'NEW_Y_TRANSFORM 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_Y_TRANSFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_Y_TRANSFORM (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'Y_TRANSFORM2 LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'K_TRANSFORM 'PSOPFN 'NEW_K_TRANSFORM) 
(PUT 'NEW_K_TRANSFORM 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_K_TRANSFORM 'DEFINED-ON-LINE '166) 
(PUT 'NEW_K_TRANSFORM 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_K_TRANSFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_K_TRANSFORM (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'K_TRANSFORM2 LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'STRUVEH_TRANSFORM 'PSOPFN 'NEW_STRUVEH) 
(PUT 'NEW_STRUVEH 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_STRUVEH 'DEFINED-ON-LINE '180) 
(PUT 'NEW_STRUVEH 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_STRUVEH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_STRUVEH (LST)
    (PROG (NEW_LST TEMP)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'STRUVEH2 LST))
      (SETQ TEMP (DEFINT_TRANS NEW_LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'FOURIER_SIN 'PSOPFN 'NEW_FOURIER_SIN) 
(PUT 'NEW_FOURIER_SIN 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_FOURIER_SIN 'DEFINED-ON-LINE '194) 
(PUT 'NEW_FOURIER_SIN 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_FOURIER_SIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_FOURIER_SIN (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'FOURIER_SIN2 LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'FOURIER_COS 'PSOPFN 'NEW_FOURIER_COS) 
(PUT 'NEW_FOURIER_COS 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_FOURIER_COS 'DEFINED-ON-LINE '207) 
(PUT 'NEW_FOURIER_COS 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'NEW_FOURIER_COS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_FOURIER_COS (LST)
    (PROG (NEW_LST)
      (SETQ LST (PRODUCT_TEST LST))
      (SETQ NEW_LST (LIST 'FOURIER_COS2 LST))
      (RETURN (DEFINT_TRANS NEW_LST)))) 
(PUT 'PRODUCT_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'PRODUCT_TEST 'DEFINED-ON-LINE '220) 
(PUT 'PRODUCT_TEST 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'PRODUCT_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRODUCT_TEST (LST)
    (PROG (TEMP)
      (SETQ PRODUCT_TST NIL)
      (COND
       ((LISTP (CAR LST))
        (PROGN
         (SETQ TEMP (CAAR LST))
         (COND
          ((AND (EQUAL TEMP 'TIMES) (LEQ (LENGTH (CDAR LST)) 3))
           (PROGN
            (SETQ LST (APPEND (CDAR LST) (CDR LST)))
            (SETQ PRODUCT_TST T))))
         NIL)))
      (RETURN LST))) 
(PUT 'DEFINT_TRANS 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_TRANS 'DEFINED-ON-LINE '234) 
(PUT 'DEFINT_TRANS 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'DEFINT_TRANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINT_TRANS (LST)
    (PROG (TYPE TEMP_LST NEW_LST VAR N1 N2 RESULT)
      (SETK 'TRANSFORM_TST (AEVAL 'T))
      (SETQ SPEC_COND 'NIL)
      (SETQ TYPE (CAR LST))
      (SETQ TEMP_LST (CADR LST))
      (SETQ VAR (NTH TEMP_LST (LENGTH TEMP_LST)))
      (SETQ NEW_LST (HYPERBOLIC_TEST TEMP_LST))
      (COND
       ((EQUAL (LENGTH TEMP_LST) 3)
        (PROGN
         (SETQ N1 (CAR NEW_LST))
         (SETQ N2 (CADR NEW_LST))
         (SETQ RESULT (REVAL1 (LIST TYPE N1 N2 VAR) T))))
       ((EQUAL (LENGTH TEMP_LST) 2)
        (PROGN
         (SETQ N1 (CAR NEW_LST))
         (SETQ RESULT (REVAL1 (LIST TYPE N1 VAR) T))))
       ((AND (EQUAL (LENGTH TEMP_LST) 4) (EQUAL PRODUCT_TST 'T))
        (PROGN
         (SETQ N1 (LIST 'TIMES (CAR NEW_LST) (CADR NEW_LST)))
         (SETQ N2 (CADDR NEW_LST))
         (SETQ RESULT (REVAL1 (LIST TYPE N1 N2 VAR) T))))
       (T
        (PROGN
         (SETK 'TRANSFORM_TST (AEVAL 'NIL))
         (REDERR "Wrong number of arguments"))))
      (RETURN RESULT))) 
(PUT 'HYPERBOLIC_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'HYPERBOLIC_TEST 'DEFINED-ON-LINE '288) 
(PUT 'HYPERBOLIC_TEST 'DEFINED-IN-FILE 'DEFINT/DEFINTD.RED) 
(PUT 'HYPERBOLIC_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HYPERBOLIC_TEST (LST)
    (PROG (U FORALL-RESULT FORALL-ENDPTR)
      (SETQ U LST)
      (COND ((NULL U) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (U)
                          (COND ((ATOM U) U)
                                ((MEMQ (CAR U) '(DIFFERENCE PLUS))
                                 (HYPERBOLIC_TEST U))
                                ((EQ (CAR U) 'SINH) (CONS 'MYSINH (CDR U)))
                                ((EQ (CAR U) 'COSH) (CONS 'MYCOSH (CDR U)))
                                (T U)))
                        (CAR U))
                       NIL)))
     LOOPLABEL
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (U)
                  (COND ((ATOM U) U)
                        ((MEMQ (CAR U) '(DIFFERENCE PLUS)) (HYPERBOLIC_TEST U))
                        ((EQ (CAR U) 'SINH) (CONS 'MYSINH (CDR U)))
                        ((EQ (CAR U) 'COSH) (CONS 'MYCOSH (CDR U))) (T U)))
                (CAR U))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(ENDMODULE) 