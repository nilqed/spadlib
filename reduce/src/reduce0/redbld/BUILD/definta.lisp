(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTA)) 
(SETQ TRANSFORM_LST 'NIL) 
(AEVAL (OPERATOR (LIST '|DEFINT:OPF1|))) 
(AEVAL (OPERATOR (LIST '|DEFINT:OPF2|))) 
(FLUID '(MELLINCOEF)) 
(DE DEFINT_GW (U) (CAAR U)) 
(PUT 'DEFINT_GW 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GW 'DEFINED-ON-LINE '45) 
(PUT 'DEFINT_GW 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GW 'INLINE '(LAMBDA (U) (CAAR U))) 
(DE DEFINT_GL (U) (CAADAR U)) 
(PUT 'DEFINT_GL 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GL 'DEFINED-ON-LINE '49) 
(PUT 'DEFINT_GL 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GL 'INLINE '(LAMBDA (U) (CAADAR U))) 
(DE DEFINT_GK (U) (CDADAR U)) 
(PUT 'DEFINT_GK 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GK 'DEFINED-ON-LINE '53) 
(PUT 'DEFINT_GK 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GK 'INLINE '(LAMBDA (U) (CDADAR U))) 
(DE DEFINT_GR (U) (CADAR U)) 
(PUT 'DEFINT_GR 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GR 'DEFINED-ON-LINE '57) 
(PUT 'DEFINT_GR 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GR 'INLINE '(LAMBDA (U) (CADAR U))) 
(DE DEFINT_GM (U) (CAADR U)) 
(PUT 'DEFINT_GM 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GM 'DEFINED-ON-LINE '61) 
(PUT 'DEFINT_GM 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GM 'INLINE '(LAMBDA (U) (CAADR U))) 
(DE DEFINT_GN (U) (CADADR U)) 
(PUT 'DEFINT_GN 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GN 'DEFINED-ON-LINE '65) 
(PUT 'DEFINT_GN 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GN 'INLINE '(LAMBDA (U) (CADADR U))) 
(DE DEFINT_GP (U) (CADDR (CADR U))) 
(PUT 'DEFINT_GP 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GP 'DEFINED-ON-LINE '69) 
(PUT 'DEFINT_GP 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GP 'INLINE '(LAMBDA (U) (CADDR (CADR U)))) 
(DE DEFINT_GQ (U) (CADDDR (CADR U))) 
(PUT 'DEFINT_GQ 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GQ 'DEFINED-ON-LINE '73) 
(PUT 'DEFINT_GQ 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GQ 'INLINE '(LAMBDA (U) (CADDDR (CADR U)))) 
(DE DEFINT_GA (U) (CADDR U)) 
(PUT 'DEFINT_GA 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GA 'DEFINED-ON-LINE '77) 
(PUT 'DEFINT_GA 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GA 'INLINE '(LAMBDA (U) (CADDR U))) 
(DE DEFINT_GB (U) (CADDDR U)) 
(PUT 'DEFINT_GB 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_GB 'DEFINED-ON-LINE '81) 
(PUT 'DEFINT_GB 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_GB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DEFINT_GB 'INLINE '(LAMBDA (U) (CADDDR U))) 
(PUT 'INTGG 'SIMPFN 'SIMPINTGG) 
(PUT 'SIMPINTGG 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINTGG 'DEFINED-ON-LINE '135) 
(PUT 'SIMPINTGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SIMPINTGG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINTGG (U)
    (PROGN
     (SETQ U (INTGGG (CAR U) (CADR U) (CADDR U) (CADDDR U)))
     (SIMP (PREPSQ U)))) 
(PUT 'INTGGG 'NUMBER-OF-ARGS 4) 
(PUT 'INTGGG 'DEFINED-ON-LINE '139) 
(PUT 'INTGGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'INTGGG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTGGG (U1 U2 U3 U4)
    (PROG (V V1 V2 S1 S2 S3 COEF UU1 UU2 TEST_1 TEST_1A TEST_2 M N P Q DELTA XI
           ETA TEST TEMP TEMP1 TEMP2 VAR VAR1 VAR2 *ALLFAC)
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Entering main procedure intggg with parameters")
         (MATHPRINT (LIST 'SETQ 'U1 U1))
         (MATHPRINT (LIST 'SETQ 'U2 U2))
         (MATHPRINT (LIST 'SETQ 'U3 U3))
         (MATHPRINT (LIST 'SETQ 'U4 U4))
         NIL)))
      (SETQ UU1 (CADR U1))
      (SETQ UU1 (PREPSQ (CADR (AEVAL UU1))))
      (SETQ UU2 (CADR U2))
      (SETQ UU2 (PREPSQ (CADR (AEVAL UU2))))
      (SETQ U1
              (COND ((NULL (CDDR U1)) (LIST '|DEFINT:OPF1| UU1))
                    (T (CONS '|DEFINT:OPF1| (CONS UU1 (CDDR U1))))))
      (SETQ U2
              (COND ((NULL (CDDR U2)) (LIST '|DEFINT:OPF2| UU2))
                    (T (CONS '|DEFINT:OPF2| (CONS UU2 (CDDR U2))))))
      (COND
       ((AND (EQUAL (GET '|DEFINT:OPF1| 'G) '(1 . 1))
             (EQUAL (GET '|DEFINT:OPF2| 'G) '(1 . 1)))
        (RETURN (SIMP 'UNKNOWN)))
       ((EQUAL (GET '|DEFINT:OPF1| 'G) '(1 . 1))
        (PROGN
         (SETQ S1 (BASTAB (CAR U2) (CDDR U2)))
         (SETQ V (TRPAR (CAR (CDDDDR S1)) (CADR U2) U4))
         (COND ((EQUAL V 'FAIL) (RETURN (SIMP 'FAIL))))
         (SETQ TEMP (CAR (CDDDDR S1)))
         (SETQ VAR (CADR U2))
         (SETQ TEMP (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X VAR) TEMP)) T))
         (SETQ S1 (LIST (CAR S1) (CADR S1) (CADDR S1) (CADDDR S1) TEMP))
         (SETQ S1 (SIMP_EXPT U3 S1 U4))
         (SETQ U3 (CAR S1))
         (SETQ S1 (CDR S1))
         (SETQ TEST_1 (TEST_1 NIL U3 S1))
         (SETQ TEST_1A (TEST_1 'A U3 S1))
         (SETQ TEST_2 (TEST2 U3 (CADR S1) (CADDR S1)))
         (SETQ M (CAAR S1))
         (SETQ N (CADAR S1))
         (SETQ P (CADDAR S1))
         (SETQ Q (CAR (CDDDAR S1)))
         (SETQ DELTA
                 (REVAL1
                  (AEVAL
                   (LIST 'PLUS M
                         (LIST 'DIFFERENCE N
                               (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                     (LIST 'PLUS P Q)))))
                  T))
         (SETQ XI (REVAL1 (AEVAL (LIST 'PLUS M (LIST 'DIFFERENCE N P))) T))
         (SETQ ETA (CAR (CDDDDR S1)))
         (SETQ ETA (REVAL1 (AEVAL (LIST 'QUOTIENT ETA U4)) T))
         (SETQ TEST
                 (LIST 'TEST_CASES M N P Q DELTA XI ETA TEST_1 TEST_1A TEST_2))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Checking test cases:") (MATHPRINT TEST) NIL)))
         (SETQ TEST (REVAL1 TEST T))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Result returned is:") (MATHPRINT TEST) NIL)))
         (COND ((EQUAL TRANSFORM_TST 'T) (SETQ TEST 'T)))
         (COND ((NEQ TEST 'T) (RETURN (SIMP 'UNKNOWN))))
         (SETQ COEF (SIMP* (CADDDR S1)))
         (SETQ S1
                 (LIST V (CAR S1)
                       (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                         (SETQ UU (CADR S1))
                         (COND ((NULL UU) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                          NIL)))
                        LOOPLABEL
                         (SETQ UU (CDR UU))
                         (COND ((NULL UU) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                         (SETQ UU (CADDR S1))
                         (COND ((NULL UU) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                          NIL)))
                        LOOPLABEL
                         (SETQ UU (CDR UU))
                         (COND ((NULL UU) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (SIMP* (SUBPREF (CADR U2) 1 U4))))
         (SETQ S3 (ADDSQ (SIMP* U3) '(1 . 1)))
         (RETURN (INTG S1 S3 COEF))))
       ((EQUAL (GET '|DEFINT:OPF2| 'G) '(1 . 1))
        (PROGN
         (SETQ S1 (BASTAB (CAR U1) (CDDR U1)))
         (SETQ V (TRPAR (CAR (CDDDDR S1)) (CADR U1) U4))
         (COND ((EQUAL V 'FAIL) (RETURN (SIMP 'FAIL))))
         (SETQ TEMP (CAR (CDDDDR S1)))
         (SETQ VAR (CADR U1))
         (SETQ TEMP (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X VAR) TEMP)) T))
         (SETQ S1 (LIST (CAR S1) (CADR S1) (CADDR S1) (CADDDR S1) TEMP))
         (SETQ S1 (SIMP_EXPT U3 S1 U4))
         (SETQ U3 (CAR S1))
         (SETQ S1 (CDR S1))
         (SETQ TEST_1 (TEST_1 NIL U3 S1))
         (SETQ TEST_1A (TEST_1 'A U3 S1))
         (SETQ TEST_2 (TEST2 U3 (CADR S1) (CADDR S1)))
         (SETQ M (CAAR S1))
         (SETQ N (CADAR S1))
         (SETQ P (CADDAR S1))
         (SETQ Q (CAR (CDDDAR S1)))
         (SETQ DELTA
                 (REVAL1
                  (AEVAL
                   (LIST 'PLUS M
                         (LIST 'DIFFERENCE N
                               (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                     (LIST 'PLUS P Q)))))
                  T))
         (SETQ XI (REVAL1 (AEVAL (LIST 'PLUS M (LIST 'DIFFERENCE N P))) T))
         (SETQ ETA (CAR (CDDDDR S1)))
         (SETQ ETA (REVAL1 (AEVAL (LIST 'QUOTIENT ETA U4)) T))
         (SETQ TEST
                 (LIST 'TEST_CASES M N P Q DELTA XI ETA TEST_1 TEST_1A TEST_2))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Checking test cases:") (MATHPRINT TEST) NIL)))
         (SETQ TEST (REVAL1 TEST T))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Result returned is:") (MATHPRINT TEST) NIL)))
         (COND ((EQUAL TRANSFORM_TST 'T) (SETQ TEST 'T)))
         (COND ((NEQ TEST 'T) (RETURN (SIMP 'UNKNOWN))))
         (SETQ COEF (SIMP* (CADDDR S1)))
         (SETQ S1
                 (LIST V (CAR S1)
                       (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                         (SETQ UU (CADR S1))
                         (COND ((NULL UU) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                          NIL)))
                        LOOPLABEL
                         (SETQ UU (CDR UU))
                         (COND ((NULL UU) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                         (SETQ UU (CADDR S1))
                         (COND ((NULL UU) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                          NIL)))
                        LOOPLABEL
                         (SETQ UU (CDR UU))
                         (COND ((NULL UU) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       (SIMP* (SUBPREF (CADR U1) 1 U4))))
         (SETQ S3 (ADDSQ (SIMP* U3) '(1 . 1)))
         (RETURN (INTG S1 S3 COEF)))))
      (SETQ S1 (BASTAB (CAR U1) (CDDR U1)))
      (SETQ S2 (BASTAB (CAR U2) (CDDR U2)))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "MeijerG representations of the first factor is:")
         (PRIN2* "MeijerG<")
         (PRIN2* (CAR (CAR S1)))
         (PRIN2* ",")
         (PRIN2* (CADR (CAR S1)))
         (PRIN2* ",")
         (PRIN2* (CADDR (CAR S1)))
         (PRIN2* ",")
         (PRIN2* (CADDDR (CAR S1)))
         (PRIN2* ">")
         (MAPRIN (CONS 'LIST (CADR S1)))
         (PRIN2* "|")
         (MAPRIN (CONS 'LIST (CADDR S1)))
         (PRIN2* "|")
         (MAPRIN (CAR (CDDDDR S1)))
         (TERPRI* T)
         (COND
          ((NEQ (CADDDR S1) 1)
           (PROGN (PRIN2* "times a factor of") (MATHPRINT (CADDDR S1)))))
         (PRIN2T "MeijerG representations of the second factor is:")
         (PRIN2* "MeijerG<")
         (PRIN2* (CAR (CAR S2)))
         (PRIN2* ",")
         (PRIN2* (CADR (CAR S2)))
         (PRIN2* ",")
         (PRIN2* (CADDR (CAR S2)))
         (PRIN2* ",")
         (PRIN2* (CADDDR (CAR S2)))
         (PRIN2* ">")
         (MAPRIN (CONS 'LIST (CADR S2)))
         (PRIN2* "|")
         (MAPRIN (CONS 'LIST (CADDR S2)))
         (PRIN2* "|")
         (MAPRIN (CAR (CDDDDR S2)))
         (TERPRI* T)
         (COND
          ((NEQ (CADDDR S2) 1)
           (PROGN (PRIN2* "times a coefficient of") (MATHPRINT (CADDDR S2)))))
         NIL)))
      (SETQ COEF (MULTSQ (SIMP* (CADDDR S1)) (SIMP* (CADDDR S2))))
      (COND
       (*TRDEFINT
        (PROGN (PRIN2T "Product of coefficients is") (PRINTSQ COEF) NIL)))
      (SETQ V1 (TRPAR (CAR (CDDDDR S1)) (CADR U1) U4))
      (COND ((EQUAL V1 'FAIL) (RETURN (SIMP 'FAIL))))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Argument of first MeijerG function is")
         (MATHPRINT
          (LIST 'TIMES (PREPSQ (CAR V1)) (LIST 'EXPT U4 (PREPSQ (CADR V1)))))
         NIL)))
      (SETQ V2 (TRPAR (CAR (CDDDDR S2)) (CADR U2) U4))
      (COND ((EQUAL V2 'FAIL) (RETURN (SIMP 'FAIL))))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Argument of second MeijerG function is")
         (MATHPRINT
          (LIST 'TIMES (PREPSQ (CAR V2)) (LIST 'EXPT U4 (PREPSQ (CADR V2)))))
         NIL)))
      (SETQ *ALLFAC T)
      (SETQ TEMP1 (CAR (CDDDDR S1)))
      (SETQ VAR1 (CADR U1))
      (SETQ TEMP1 (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X VAR1) TEMP1)) T))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "After substituting the current expression the argument to the first MeijerG function is")
         (MATHPRINT TEMP1)
         NIL)))
      (SETQ S1 (LIST (CAR S1) (CADR S1) (CADDR S1) (CADDDR S1) TEMP1))
      (SETQ TEMP2 (CAR (CDDDDR S2)))
      (SETQ VAR2 (CADR U2))
      (SETQ TEMP2 (REVAL1 (AEVAL (LIST 'SUB (LIST 'EQUAL 'X VAR2) TEMP2)) T))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "After substituting the current expression the argument to the second MeijerG function is")
         (MATHPRINT TEMP2)
         NIL)))
      (SETQ S2 (LIST (CAR S2) (CADR S2) (CADDR S2) (CADDDR S2) TEMP2))
      (SETQ S1
              (LIST V1 (CAR S1)
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADR S1))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADDR S1))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    (SIMP* (SUBPREF (CADR U1) 1 U4))))
      (SETQ S2
              (LIST V2 (CAR S2)
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADR S2))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADDR S2))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (SIMP* UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))
                    (SIMP* (SUBPREF (CADR U2) 1 U4))))
      (SETQ S3 (ADDSQ (SIMP* U3) '(1 . 1)))
      (COND
       ((OR (NOT (NUMBERP (CAADAR S1))) (NOT (NUMBERP (CAADAR S2))))
        (RETURN (SIMP 'FAIL)))
       ((LESSP (CAADAR S1) 0) (SETQ S1 (CONG S1)))
       ((LESSP (CAADAR S2) 0) (SETQ S2 (CONG S2)))
       ((EQUAL (CAADAR S1) (CDADAR S1)) (GO A))
       ((EQUAL (CAADAR S2) (CDADAR S2))
        (PROGN
         (SETQ V S1)
         (SETQ S1 S2)
         (SETQ S2 V)
         (COND (*TRDEFINT (PRIN2T "Exchanging the two MeijerG functions")))
         (GO A))))
      (SETQ COEF (MULTSQ COEF (INVSQ (CADAR S1))))
      (SETQ V (MODINTGG S3 S1 S2))
      (SETQ S3 (CAR V))
      (SETQ S1 (CADR V))
      (SETQ S2 (CADDR V))
     A
      (SETQ TEST (VALIDITY_CHECK S1 S2 U3))
      (COND ((NEQ TEST 'T) (RETURN (SIMP 'UNKNOWN))))
      (SETQ COEF
              (MULTSQ
               (COND ((NUMBERP MELLINCOEF) (SIMP MELLINCOEF))
                     (T (CADR MELLINCOEF)))
               (MULTSQ COEF (COEFINTG S1 S2 S3))))
      (COND
       (*TRDEFINT
        (PROGN (PRIN2T "Overall coefficient is") (PRINTSQ COEF) NIL)))
      (SETQ V (DELTAGG S1 S2 S3))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Parameter list of resulting single MeijerG function is")
         (MATHPRINT
          (CONS 'LIST
                (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                  (SETQ SQ (CAR V))
                  (COND ((NULL SQ) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                        NIL)))
                 LOOPLABEL
                  (SETQ SQ (CDR SQ))
                  (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
         (MATHPRINT
          (CONS 'LIST
                (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                  (SETQ SQ (CADR V))
                  (COND ((NULL SQ) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                        NIL)))
                 LOOPLABEL
                  (SETQ SQ (CDR SQ))
                  (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
         NIL)))
      (SETQ V (REDPARGF (LIST (ARGGF S1 S2) (INDGF S1 S2) (CAR V) (CADR V))))
      (SETQ V (CONS 'MEIJERG (MGRETRO (CADR V) (CADDR V) (CAR V))))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T "Resulting single MeijerG function is")
         (MATHPRINT V)
         NIL)))
      (SETQ V (REVAL1 V NIL))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "Simplification of the resulting single MeijerG function yields")
         (MATHPRINT V)
         NIL)))
      (COND ((EQCAR V '*SQ) (SETQ V (CADR V))) ((FIXP V) (SETQ V (SIMP V))))
      (COND ((EQUAL V 'FAIL) (RETURN (SIMP 'FAIL)))
            (T (RETURN (MULTSQ COEF V)))))) 
(PUT 'MGRETRO 'NUMBER-OF-ARGS 3) 
(PUT 'MGRETRO 'DEFINED-ON-LINE '471) 
(PUT 'MGRETRO 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'MGRETRO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MGRETRO (U V W)
    (PROG (CARU CARV CDRU CDRV)
      (SETQ CARU (CAR U))
      (SETQ CDRU (CDR U))
      (SETQ CARV (CAR V))
      (SETQ CDRV (CDR V))
      (RETURN
       (LIST
        (CONS 'LIST
              (CONS
               (CONS 'LIST
                     (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                       (SETQ AA CARU)
                       (COND ((NULL AA) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (AA) (PREPSQ AA)) (CAR AA))
                                        NIL)))
                      LOOPLABEL
                       (SETQ AA (CDR AA))
                       (COND ((NULL AA) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                 (SETQ AA CDRU)
                 (COND ((NULL AA) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA))
                                       NIL)))
                LOOPLABEL
                 (SETQ AA (CDR AA))
                 (COND ((NULL AA) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        (CONS 'LIST
              (CONS
               (CONS 'LIST
                     (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                       (SETQ AA CARV)
                       (COND ((NULL AA) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (AA) (PREPSQ AA)) (CAR AA))
                                        NIL)))
                      LOOPLABEL
                       (SETQ AA (CDR AA))
                       (COND ((NULL AA) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                 (SETQ AA CDRV)
                 (COND ((NULL AA) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA))
                                       NIL)))
                LOOPLABEL
                 (SETQ AA (CDR AA))
                 (COND ((NULL AA) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (AA) (PREPSQ AA)) (CAR AA)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        (PREPSQ W))))) 
(PUT 'INTG 'NUMBER-OF-ARGS 3) 
(PUT 'INTG 'DEFINED-ON-LINE '485) 
(PUT 'INTG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'INTG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTG (U1 U2 U3)
    (PROG (V)
      (COND
       ((AND (NUMBERP (CAADAR U1)) (LESSP (CAADAR U1) 0)) (SETQ U1 (CONG U1))))
      (SETQ V (MODINTG U2 U1))
      (SETQ U1 (CADR V))
      (SETQ V
              ((LAMBDA (P)
                 (PROGN
                  (PROG (PP)
                    (SETQ PP
                            (LIST U3 (EXPDEG (CAAR U1) (NEGSQ U2))
                                  (MULTSQ
                                   ((LAMBDA (P)
                                      (PROGN
                                       (PROG (PP)
                                         (SETQ PP
                                                 (APPEND
                                                  (SPECFN-LISTPLUS
                                                   (CAR
                                                    (REDPAR1 (CADDDR U1)
                                                             (CAADR U1)))
                                                   U2)
                                                  (SPECFN-LISTPLUS
                                                   (PROG (UU FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ UU
                                                             (CAR
                                                              (REDPAR1
                                                               (CADDR U1)
                                                               (CADADR U1))))
                                                     (COND
                                                      ((NULL UU) (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (UU)
                                                                         (NEGSQ
                                                                          UU))
                                                                       (CAR
                                                                        UU))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ UU (CDR UU))
                                                     (COND
                                                      ((NULL UU)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (UU)
                                                                 (NEGSQ UU))
                                                               (CAR UU))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL))
                                                   (ADDSQ '(1 . 1)
                                                          (NEGSQ U2)))))
                                        LAB
                                         (COND ((NULL PP) (RETURN NIL)))
                                         ((LAMBDA (PP)
                                            (PROGN
                                             (SETQ P
                                                     (MULTSQ
                                                      (CONS
                                                       (LIST
                                                        (CONS
                                                         (CONS
                                                          (CAR
                                                           (FKERN
                                                            (LIST 'GAMMA
                                                                  (PREPSQ
                                                                   PP))))
                                                          1)
                                                         1))
                                                       1)
                                                      P))))
                                          (CAR PP))
                                         (SETQ PP (CDR PP))
                                         (GO LAB))
                                       P))
                                    '(1 . 1))
                                   (INVSQ
                                    ((LAMBDA (P)
                                       (PROGN
                                        (PROG (PP)
                                          (SETQ PP
                                                  (APPEND
                                                   (SPECFN-LISTPLUS
                                                    (CDR
                                                     (REDPAR1 (CADDR U1)
                                                              (CADADR U1)))
                                                    U2)
                                                   (SPECFN-LISTPLUS
                                                    (PROG (UU FORALL-RESULT
                                                           FORALL-ENDPTR)
                                                      (SETQ UU
                                                              (CDR
                                                               (REDPAR1
                                                                (CADDDR U1)
                                                                (CAADR U1))))
                                                      (COND
                                                       ((NULL UU)
                                                        (RETURN NIL)))
                                                      (SETQ FORALL-RESULT
                                                              (SETQ FORALL-ENDPTR
                                                                      (CONS
                                                                       ((LAMBDA
                                                                            (
                                                                             UU)
                                                                          (NEGSQ
                                                                           UU))
                                                                        (CAR
                                                                         UU))
                                                                       NIL)))
                                                     LOOPLABEL
                                                      (SETQ UU (CDR UU))
                                                      (COND
                                                       ((NULL UU)
                                                        (RETURN
                                                         FORALL-RESULT)))
                                                      (RPLACD FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (UU)
                                                                  (NEGSQ UU))
                                                                (CAR UU))
                                                               NIL))
                                                      (SETQ FORALL-ENDPTR
                                                              (CDR
                                                               FORALL-ENDPTR))
                                                      (GO LOOPLABEL))
                                                    (ADDSQ '(1 . 1)
                                                           (NEGSQ U2)))))
                                         LAB
                                          (COND ((NULL PP) (RETURN NIL)))
                                          ((LAMBDA (PP)
                                             (PROGN
                                              (SETQ P
                                                      (MULTSQ
                                                       (CONS
                                                        (LIST
                                                         (CONS
                                                          (CONS
                                                           (CAR
                                                            (FKERN
                                                             (LIST 'GAMMA
                                                                   (PREPSQ
                                                                    PP))))
                                                           1)
                                                          1))
                                                        1)
                                                       P))))
                                           (CAR PP))
                                          (SETQ PP (CDR PP))
                                          (GO LAB))
                                        P))
                                     '(1 . 1))))))
                   LAB
                    (COND ((NULL PP) (RETURN NIL)))
                    ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
                    (SETQ PP (CDR PP))
                    (GO LAB))
                  P))
               '(1 . 1)))
      (RETURN
       (MULTSQ
        (COND ((NUMBERP MELLINCOEF) (SIMP MELLINCOEF)) (T (CADR MELLINCOEF)))
        V)))) 
(SHARE (LIST 'INTGGG-RULES1)) 
(SETQ INTGGG-RULES1
        (PROGN
         (SETQ ALGLIST* (CONS NIL NIL))
         (AEVAL
          (LIST 'LIST
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                  (LIST 'EXPT '*INTVAR* (LIST '~ 'A))))
                      (LIST 'WHEN (LIST 'LIST 'A (LIST 'TIMES 'C '*INTVAR*))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT
                                  (LIST 'TIMES (LIST '~ 'C)
                                        (LIST 'EXPT '*INTVAR* (LIST '~ 'A)))
                                  (LIST '~ 'D)))
                      (LIST 'WHEN
                            (LIST 'LIST 'A
                                  (LIST 'TIMES (LIST 'QUOTIENT 'C 'D)
                                        '*INTVAR*))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'D '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT (LIST '~ 'C)
                                  (LIST 'EXPT '*INTVAR* (LIST '~ 'A))))
                      (LIST 'WHEN (LIST 'LIST 'A (LIST 'QUOTIENT 'C '*INTVAR*))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT (LIST 'EXPT '*INTVAR* (LIST '~ 'A))
                                  (LIST '~ 'C)))
                      (LIST 'WHEN (LIST 'LIST 'A (LIST 'QUOTIENT '*INTVAR* 'C))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT 1
                                  (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                        (LIST 'EXPT '*INTVAR* (LIST '~ 'A)))))
                      (LIST 'WHEN
                            (LIST 'LIST 'A
                                  (LIST 'QUOTIENT 1
                                        (LIST 'TIMES 'C '*INTVAR*)))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                  (LIST 'EXPT '*INTVAR* (LIST '~ 'A))
                                  (LIST 'SQRT '*INTVAR*)))
                      (LIST 'WHEN
                            (LIST 'LIST (LIST 'PLUS 'A (LIST 'QUOTIENT 1 2))
                                  (LIST 'TIMES 'C '*INTVAR*))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                  (LIST 'QUOTIENT (LIST 'SQRT '*INTVAR*)
                                        (LIST 'EXPT '*INTVAR* (LIST '~ 'A)))))
                      (LIST 'WHEN
                            (LIST 'LIST
                                  (LIST 'DIFFERENCE 'A (LIST 'QUOTIENT 1 2))
                                  (LIST 'QUOTIENT 'C '*INTVAR*))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT
                                  (LIST 'TIMES
                                        (LIST 'EXPT '*INTVAR* (LIST '~ 'A))
                                        (LIST 'SQRT '*INTVAR*))
                                  (LIST '~ 'C)))
                      (LIST 'WHEN
                            (LIST 'LIST (LIST 'PLUS 'A (LIST 'QUOTIENT 1 2))
                                  (LIST 'QUOTIENT '*INTVAR* 'C))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*))))
                (LIST 'REPLACEBY
                      (LIST 'LIST
                            (LIST 'QUOTIENT (LIST 'SQRT '*INTVAR*)
                                  (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                        (LIST 'EXPT '*INTVAR* (LIST '~ 'A)))))
                      (LIST 'WHEN
                            (LIST 'LIST
                                  (LIST 'DIFFERENCE 'A (LIST 'QUOTIENT 1 2))
                                  (LIST 'QUOTIENT 1
                                        (LIST 'TIMES 'C '*INTVAR*)))
                            (LIST 'AND (LIST 'FREEOF 'C '*INTVAR*)
                                  (LIST 'FREEOF 'A '*INTVAR*)))))))) 
(AEVAL 'NIL) 
(PUT 'SIMP_EXPT 'NUMBER-OF-ARGS 3) 
(PUT 'SIMP_EXPT 'DEFINED-ON-LINE '543) 
(PUT 'SIMP_EXPT 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SIMP_EXPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMP_EXPT (U V INTVAR)
    (PROG (VAR M N COEF ALPHA BETA ALPHA1 ALPHA2 EXPT_FLAG K TEMP1 TEMP2
           VARCOEF VARNEW)
      (SETQ VAR (CAR (CDDDDR V)))
      (SETQ BETA 1)
      (COND ((EQUAL (LENGTH VAR) 0) (RETURN (CONS U V)))
            (T
             (PROGN
              (SETQ K U)
              (SETQ COEF (CADDDR V))
              (SETQ TEMP1 (SUBST INTVAR '*INTVAR* INTGGG-RULES1))
              (SETQ TEMP2
                      (PREPSQ
                       (EVALLETSUB
                        (LIST (LIST TEMP1)
                              (LIST 'SIMP* (MKQUOTE (LIST 'LIST VAR))))
                        NIL)))
              (COND
               ((GREATERP (LENGTH TEMP2) 2)
                (PROGN
                 (SETQ ALPHA (CADR TEMP2))
                 (SETQ VAR (CADDR TEMP2))
                 (SETQ EXPT_FLAG T))))
              (COND ((EQUAL EXPT_FLAG NIL) (RETURN (CONS U V)))
                    (T
                     (PROGN
                      (SETQ K
                              (REVAL1
                               (AEVAL
                                (LIST 'DIFFERENCE
                                      (LIST 'QUOTIENT (LIST 'PLUS K 1) ALPHA)
                                      1))
                               T))
                      (SETQ COEF
                              (REVAL1 (AEVAL (LIST 'QUOTIENT COEF ALPHA)) T))
                      (RETURN (LIST K (CAR V) (CADR V) (CADDR V) COEF VAR)))))
              NIL))))) 
(PUT 'TEST_1 'NUMBER-OF-ARGS 3) 
(PUT 'TEST_1 'DEFINED-ON-LINE '670) 
(PUT 'TEST_1 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TEST_1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TEST_1 (AA U V)
    (PROG (S M N A B AI BJ A_MAX B_MIN TEMP TEMP1 RND DMODE* RESULT)
      (SETQ RND *ROUNDED)
      (COND (RND (OFF (LIST 'ROUNDED))))
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (SETQ S (AEVAL (LIST 'REPART (LIST 'PLUS 1 U))))
         (SETQ S (SIMP* S))
         (SETQ M (CAAR V))
         (SETQ N (CADAR V))
         (SETQ A (CADR V))
         (SETQ B (CADDR V))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking test_1: -min Re{bj} < Re{s} < 1 - max Re{ai}    i=1..n, j=1..m")
            (MATHPRINT (LIST 'EQUAL 'S (PREPSQ S)))
            (PRIN2* (LENGTH A))
            (PRIN2* " upper parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'A
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ A)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (PRIN2* (LENGTH B))
            (PRIN2* " lower parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'B
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ B)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS ((LAMBDA (SQ) SQ) (CAR SQ))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) SQ) (CAR SQ)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            NIL)))
         (COND
          ((EQUAL AA NIL)
           (PROGN
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
              (PROGN
               (COND ((EQUAL (CAR A) 'NIL) (SETCAR A 0)))
               (SETQ AI (APPEND AI (LIST (CAR A))))
               (SETQ A (CDR A)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND
             ((NEQ AI 'NIL)
              (PROGN
               (SETQ A_MAX (SIMPMAX (LIST (CONS 'LIST AI))))
               (SETQ A_MAX (SIMPREPART (LIST (MK*SQ A_MAX)))))))
            NIL))
          ((EQUAL AA 'A)
           (PROGN
            (COND
             ((NEQ A 'NIL)
              (PROGN
               (SETQ A_MAX (SIMPMAX (LIST (CONS 'LIST A))))
               (SETQ A_MAX (SIMPREPART (LIST (MK*SQ A_MAX)))))))
            NIL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
           (PROGN
            (COND ((EQUAL (CAR B) 'NIL) (SETCAR B 0)))
            (SETQ BJ (APPEND BJ (LIST (CAR B))))
            (SETQ B (CDR B)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND
          ((NEQ BJ 'NIL)
           (PROGN
            (SETQ B_MIN (SIMPMIN (LIST (CONS 'LIST BJ))))
            (SETQ B_MIN (SIMPREPART (LIST (MK*SQ (NEGSQ B_MIN))))))))
         (COND
          ((AND (NEQ A_MAX NIL) (NEQ B_MIN NIL))
           (PROGN
            (SETQ TEMP (ADDSQ S (NEGSQ (DIFFSQ A_MAX 1))))
            (SETQ TEMP1 (ADDSQ B_MIN (NEGSQ S)))
            (COND
             ((AND (EQUAL (SIGN-OF (MK*SQ TEMP)) (MINUS 1))
                   (EQUAL (SIGN-OF (MK*SQ TEMP1)) (MINUS 1)))
              (PROGN
               (COND (RND (ON (LIST 'ROUNDED))))
               (SETQ RESULT (TEST2 (PREPSQ S) (CADR V) (CADDR V)))))
             (T
              (PROGN (COND (RND (ON (LIST 'ROUNDED)))) (SETQ RESULT 'FAIL))))))
          ((NULL A_MAX)
           (PROGN
            (SETQ TEMP (ADDSQ B_MIN (NEGSQ S)))
            (COND
             ((EQUAL (SIGN-OF (MK*SQ TEMP)) (MINUS 1))
              (PROGN (COND (RND (ON (LIST 'ROUNDED)))) (SETQ RESULT 'T)))
             (T
              (PROGN (COND (RND (ON (LIST 'ROUNDED)))) (SETQ RESULT 'FAIL))))))
          ((NULL B_MIN)
           (PROGN
            (SETQ TEMP (ADDSQ S (NEGSQ (DIFFSQ A_MAX 1))))
            (COND
             ((EQUAL (SIGN-OF (MK*SQ TEMP)) (MINUS 1))
              (PROGN (COND (RND (ON (LIST 'ROUNDED)))) (SETQ RESULT 'T)))
             (T (PROGN (COND (RND (ON (LIST 'ROUNDED)))) (SETQ RESULT 'FAIL))))
            NIL)))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Result of test1 is ") (PRIN2* RESULT) (TERPRI* T))))
         (RETURN RESULT)
         NIL))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TST1
                        '(LIST 'LESSP
                               (LIST 'LESSP
                                     (LIST 'MINUS
                                           (LIST 'MIN (LIST 'REPART 'BJ)))
                                     (LIST 'REPART 'S))
                               (LIST 'DIFFERENCE 1
                                     (LIST 'MAX (LIST 'REPART 'AI)))))
                  TRANSFORM_LST))
         (COND (RND (OFF (LIST 'ROUNDED))))
         (RETURN 'T)))))) 
(PUT 'TEST2 'NUMBER-OF-ARGS 3) 
(PUT 'TEST2 'DEFINED-ON-LINE '794) 
(PUT 'TEST2 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TEST2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TEST2 (S A B)
    (PROG (P Q SUM_A SUM_B DIFF_SUM TEMP1 TEMP2 TEMP DIFF RESULT)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (SETQ S (AEVAL (LIST 'REPART (LIST 'PLUS 1 S))))
         (SETQ P (LENGTH A))
         (SETQ Q (LENGTH B))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking test_2: Re{Sum(ai) - Sum(bj)} + 1/2 * (q + 1 - p) > (q - p) * Re{s}, i=1..p, j=1..q")
            (MATHPRINT (LIST 'EQUAL 'S S))
            (PRIN2* P)
            (PRIN2* " upper parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'A
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ A)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (PRIN2* Q)
            (PRIN2* " lower parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'B
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ B)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS ((LAMBDA (SQ) SQ) (CAR SQ))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) SQ) (CAR SQ)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            NIL)))
         (PROG (I)
           (SETQ I A)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN (SETQ SUM_A (REVAL1 (AEVAL (LIST 'PLUS SUM_A I)) T))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (PROG (J)
           (SETQ J B)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (PROGN (SETQ SUM_B (REVAL1 (AEVAL (LIST 'PLUS SUM_B J)) T))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (SETQ DIFF_SUM
                 (REVAL1 (AEVAL (LIST 'REPART (LIST 'DIFFERENCE SUM_A SUM_B)))
                         T))
         (SETQ TEMP
                 (REVAL1
                  (AEVAL
                   (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                         (LIST 'PLUS Q (LIST 'DIFFERENCE 1 P))))
                  T))
         (SETQ TEMP1 (REVAL1 (AEVAL (LIST 'PLUS DIFF_SUM TEMP)) T))
         (SETQ TEMP2 (REVAL1 (AEVAL (LIST 'TIMES (LIST 'DIFFERENCE Q P) S)) T))
         (SETQ DIFF (SIMP* (REVAL1 (AEVAL (LIST 'DIFFERENCE TEMP1 TEMP2)) T)))
         (SETQ RESULT (COND ((EQUAL (SIGN-OF (MK*SQ DIFF)) 1) T) (T 'FAIL)))
         (COND
          (*TRDEFINT
           (PROGN (PRIN2T "Result of test2 is ") (PRIN2* RESULT) (TERPRI* T))))
         (RETURN RESULT)
         NIL))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TST2
                        '(LIST 'GREATERP
                               (LIST 'PLUS
                                     (LIST 'REPART
                                           (LIST 'DIFFERENCE (LIST 'SUM 'AI)
                                                 (LIST 'SUM 'BJ)))
                                     (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                           (LIST 'PLUS 'Q
                                                 (LIST 'DIFFERENCE 1 'P))))
                               (LIST 'TIMES (LIST 'DIFFERENCE 'Q 'P)
                                     (LIST 'REPART 'S))))
                  TRANSFORM_LST))
         (RETURN 'T)
         NIL))))) 
(PUT 'VALIDITY_CHECK 'NUMBER-OF-ARGS 3) 
(PUT 'VALIDITY_CHECK 'DEFINED-ON-LINE '858) 
(PUT 'VALIDITY_CHECK 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'VALIDITY_CHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VALIDITY_CHECK (S1 S2 U3)
    (PROG (ALPHA M N P Q K L U V DELTA EPSILON SIGMA OMEGA R A B C D B_SUM
           A_SUM D_SUM C_SUM MU RHO PHI ETA R1 R2 TEST_1A TEST_1B TEST_2 TEST_3
           TEST_4 TEST_5 TEST_6 TEST_7 TEST_8 TEST_9 TEST_10 TEST_11 TEST_12
           TEST_13 TEST_14 TEST_15 TEST)
      (SETQ TRANSFORM_LST 'NIL)
      (SETQ ALPHA (REVAL1 (AEVAL (LIST 'PLUS 1 U3)) T))
      (SETQ M (CAADR S1))
      (SETQ N (CADADR S1))
      (SETQ P (CAR (CDDADR S1)))
      (SETQ Q (CADR (CDDADR S1)))
      (SETQ EPSILON
              (REVAL1
               (AEVAL
                (LIST 'PLUS M
                      (LIST 'DIFFERENCE N
                            (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                  (LIST 'PLUS P Q)))))
               T))
      (SETQ K (CAADR S2))
      (SETQ L (CADADR S2))
      (SETQ U (CAR (CDDADR S2)))
      (SETQ V (CADR (CDDADR S2)))
      (SETQ DELTA
              (REVAL1
               (AEVAL
                (LIST 'PLUS K
                      (LIST 'DIFFERENCE L
                            (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                                  (LIST 'PLUS U V)))))
               T))
      (SETQ SIGMA (PREPSQ (CAAR S1)))
      (SETQ OMEGA (PREPSQ (CAAR S2)))
      (SETQ R (PREPSQ (CADAR S2)))
      (SETQ A (CADDR S1))
      (SETQ B (CADDDR S1))
      (SETQ C (CADDR S2))
      (SETQ D (CADDDR S2))
      (PROG (I)
        (SETQ I B)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ I (PREPSQ I))
            (SETQ B_SUM (REVAL1 (AEVAL (LIST 'PLUS B_SUM I)) T))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (PROG (J)
        (SETQ J A)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ J (PREPSQ J))
            (SETQ A_SUM (REVAL1 (AEVAL (LIST 'PLUS A_SUM J)) T))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (PROG (I)
        (SETQ I D)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ I (PREPSQ I))
            (SETQ D_SUM (REVAL1 (AEVAL (LIST 'PLUS D_SUM I)) T))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (PROG (J)
        (SETQ J C)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ J (PREPSQ J))
            (SETQ C_SUM (REVAL1 (AEVAL (LIST 'PLUS C_SUM J)) T))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ MU
              (REVAL1
               (AEVAL
                (LIST 'PLUS (LIST 'DIFFERENCE B_SUM A_SUM)
                      (LIST 'TIMES (LIST 'QUOTIENT 1 2) (LIST 'DIFFERENCE P Q))
                      1))
               T))
      (SETQ RHO
              (REVAL1
               (AEVAL
                (LIST 'PLUS (LIST 'DIFFERENCE D_SUM C_SUM)
                      (LIST 'TIMES (LIST 'QUOTIENT 1 2) (LIST 'DIFFERENCE U V))
                      1))
               T))
      (SETQ PHI
              (REVAL1
               (AEVAL
                (LIST 'DIFFERENCE (LIST 'DIFFERENCE Q P)
                      (LIST 'TIMES R (LIST 'DIFFERENCE V U))))
               T))
      (SETQ ETA
              (REVAL1
               (AEVAL
                (LIST 'DIFFERENCE
                      (LIST 'DIFFERENCE
                            (LIST 'DIFFERENCE 1
                                  (LIST 'TIMES ALPHA (LIST 'DIFFERENCE V U)))
                            MU)
                      RHO))
               T))
      (COND ((LISTP R) (PROGN (SETQ R1 (CADR R)) (SETQ R2 (CADDR R))))
            (T (PROGN (SETQ R1 R) (SETQ R2 1))))
      (COND (*TRDEFINT (PRIN2T "Checking test cases:")))
      (SETQ TEST_1A (TST1A M N A B))
      (SETQ TEST_1B (TST1B K L C D))
      (SETQ TEST_2 (TST2 M K B D ALPHA R))
      (SETQ TEST_3 (TST3 N L A C ALPHA R))
      (SETQ TEST_4 (TST4 L P Q C ALPHA R MU))
      (SETQ TEST_5 (TST5 K P Q D ALPHA R MU))
      (SETQ TEST_6 (TST6 N U V A ALPHA R RHO))
      (SETQ TEST_7 (TST7 M U V B ALPHA R RHO))
      (SETQ TEST_8 (TST8 P Q U V ALPHA R MU RHO PHI))
      (SETQ TEST_9 (TST9 P Q U V ALPHA R MU RHO PHI))
      (SETQ TEST_10 (TST10 SIGMA DELTA))
      (SETQ TEST_11 (TST11 SIGMA DELTA))
      (SETQ TEST_12 (TST12 OMEGA EPSILON))
      (SETQ TEST_13 (TST13 OMEGA EPSILON))
      (SETQ TEST_14
              (TST14 U V ALPHA MU RHO DELTA EPSILON SIGMA OMEGA R PHI R1 R2))
      (COND
       ((EQUAL TEST_14 'FAIL)
        (SETQ TEST_14
                (TST14 P Q ALPHA MU RHO EPSILON DELTA OMEGA SIGMA R PHI R2
                 R1))))
      (COND ((OR (EQUAL P Q) (EQUAL U V)) (SETQ TEST_15 'FAIL))
            (T (SETQ TEST_15 (TST15 M N P Q K L U V SIGMA OMEGA ETA))))
      (SETQ TEST
              (LIST 'TEST_CASES2 M N P Q K L U V DELTA EPSILON SIGMA OMEGA RHO
                    ETA MU R1 R2 PHI TEST_1A TEST_1B TEST_2 TEST_3 TEST_4
                    TEST_5 TEST_6 TEST_7 TEST_8 TEST_9 TEST_10 TEST_11 TEST_12
                    TEST_13 TEST_14 TEST_15))
      (COND
       (*TRDEFINT
        (PROGN
         (TERPRI* T)
         (PRIN2T "Checking test cases:")
         (MATHPRINT TEST)
         NIL)))
      (SETQ TEST (REVAL1 TEST T))
      (COND
       (*TRDEFINT (PROGN (PRIN2T "Result returned is:") (MATHPRINT TEST) NIL)))
      (COND ((AND (EQUAL TRANSFORM_TST T) (NEQ SPEC_COND NIL)) (SETQ TEST T)))
      (RETURN TEST))) 
(PUT 'TST1A 'NUMBER-OF-ARGS 4) 
(PUT 'TST1A 'DEFINED-ON-LINE '967) 
(PUT 'TST1A 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST1A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TST1A (M N A B)
    (PROG (A_NEW B_NEW TEMP FAIL_TEST)
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "Checking tst1a: any difference between an upper and a lower parameter must not be a positive integer")
         (PRIN2* N)
         (PRIN2* " upper parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'A
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ A)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         (PRIN2* M)
         (PRIN2* " lower parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'B
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ B)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         NIL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN (SETQ A_NEW (APPEND A_NEW (LIST (CAR A)))) (SETQ A (CDR A)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
        (PROGN (SETQ B_NEW (APPEND B_NEW (LIST (CAR B)))) (SETQ B (CDR B)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (I)
        (SETQ I A_NEW)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (PROG (J)
              (SETQ J B_NEW)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (SETQ TEMP (ADDSQ I (NEGSQ J)))
                  (COND
                   ((AND (NEQ (CAR TEMP) 'NIL) (GREATERP (CAR TEMP) 0)
                         (EQUAL (CDR TEMP) 1))
                    (SETQ FAIL_TEST T)))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            NIL))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2* "Result of tst1a is ")
         (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
         (TERPRI* T))))
      (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T))))) 
(PUT 'TST1B 'NUMBER-OF-ARGS 4) 
(PUT 'TST1B 'DEFINED-ON-LINE '1005) 
(PUT 'TST1B 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST1B 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TST1B (K L C D)
    (PROG (C_NEW D_NEW TEMP FAIL_TEST)
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "Checking tst1b: any difference between an upper and a lower parameter must not be a positive integer")
         (PRIN2* L)
         (PRIN2* " upper parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'C
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ C)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         (PRIN2* K)
         (PRIN2* " lower parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'D
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ D)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         NIL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L I)) (RETURN NIL)))
        (PROGN (SETQ C_NEW (APPEND C_NEW (LIST (CAR C)))) (SETQ C (CDR C)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE K J)) (RETURN NIL)))
        (PROGN (SETQ D_NEW (APPEND D_NEW (LIST (CAR D)))) (SETQ D (CDR D)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (I)
        (SETQ I C_NEW)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (PROG (J)
              (SETQ J D_NEW)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (SETQ TEMP (ADDSQ I (NEGSQ J)))
                  (COND
                   ((AND (NEQ (CAR TEMP) 'NIL) (GREATERP (CAR TEMP) 0)
                         (EQUAL (CDR TEMP) 1))
                    (SETQ FAIL_TEST T)))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            NIL))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2* "Result of tst1b is ")
         (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
         (TERPRI* T))))
      (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T))))) 
(PUT 'TST2 'NUMBER-OF-ARGS 6) 
(PUT 'TST2 'DEFINED-ON-LINE '1042) 
(PUT 'TST2 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TST2 (M K B D ALPHA R)
    (PROG (B_NEW D_NEW TEMP TEMP1 TEMP2 FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST T)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR B)))
            (SETQ B_NEW (APPEND B_NEW (LIST TEMP1)))
            (SETQ B (CDR B)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE K J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP2 (PREPSQ (CAR D)))
            (SETQ D_NEW (APPEND D_NEW (LIST TEMP2)))
            (SETQ D (CDR D)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (K)
           (SETQ K B_NEW)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K)
              (PROGN
               (PROG (H)
                 (SETQ H D_NEW)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (SETQ TEMP
                             (SIMP*
                              (REVAL1
                               (AEVAL
                                (LIST 'REPART
                                      (LIST 'PLUS ALPHA (LIST 'TIMES R K) H)))
                               T)))
                     (COND
                      ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
                       (SETQ FAIL_TEST 'T)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               NIL))
            (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst2 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST2
                        '(LIST 'GREATERP
                               (LIST 'REPART
                                     (LIST 'PLUS 'ALPHA (LIST 'TIMES 'R 'BI)
                                           'DJ))
                               0))
                  TRANSFORM_LST))
         (RETURN T)))))) 
(PUT 'TST3 'NUMBER-OF-ARGS 6) 
(PUT 'TST3 'DEFINED-ON-LINE '1086) 
(PUT 'TST3 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TST3 (N L A C ALPHA R)
    (PROG (A_NEW C_NEW TEMP TEMP1 TEMP2 FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       (*TRDEFINT
        (PROGN
         (PRIN2T
          "Checking tst3: Re{alpha + r*ai + cj} < r + 1,          i=1..n, j=1..l")
         (PRIN2* "n=")
         (PRIN2* N)
         (PRIN2* " first upper parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'A
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ A)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         (PRIN2* "l=")
         (PRIN2* L)
         (PRIN2* " second upper parameters: ")
         (MATHPRINT
          (LIST 'EQUAL 'C
                (CONS 'LIST
                      (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                        (SETQ SQ C)
                        (COND ((NULL SQ) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL)))
                       LOOPLABEL
                        (SETQ SQ (CDR SQ))
                        (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
         (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
         (MATHPRINT (LIST 'EQUAL 'R R))
         NIL)))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR A)))
            (SETQ A_NEW (APPEND A_NEW (LIST TEMP1)))
            (SETQ A (CDR A)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE L J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP2 (PREPSQ (CAR C)))
            (SETQ C_NEW (APPEND C_NEW (LIST TEMP2)))
            (SETQ C (CDR C)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (K)
           (SETQ K A_NEW)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K)
              (PROGN
               (PROG (H)
                 (SETQ H C_NEW)
                LAB
                 (COND ((NULL H) (RETURN NIL)))
                 ((LAMBDA (H)
                    (PROGN
                     (SETQ TEMP
                             (SIMP*
                              (REVAL1
                               (AEVAL
                                (LIST 'DIFFERENCE
                                      (LIST 'DIFFERENCE
                                            (LIST 'REPART
                                                  (LIST 'PLUS ALPHA
                                                        (LIST 'TIMES R K) H))
                                            R)
                                      1))
                               T)))
                     (COND
                      ((OR (EQUAL (CAR TEMP) 'NIL) (GREATERP (CAR TEMP) 0))
                       (SETQ FAIL_TEST 'T)))))
                  (CAR H))
                 (SETQ H (CDR H))
                 (GO LAB))
               NIL))
            (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst3 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST 'T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST3
                        '(LIST 'LESSP
                               (LIST 'REPART
                                     (LIST 'PLUS 'ALPHA (LIST 'TIMES 'R 'AI)
                                           'CJ))
                               (LIST 'PLUS 'R 1)))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST4 'NUMBER-OF-ARGS 7) 
(PUT 'TST4 'DEFINED-ON-LINE '1140) 
(PUT 'TST4 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST4 (L P Q C ALPHA R MU)
    (PROG (C_NEW TEMP1 TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst4: (p - q)*Re{alpha + cj - 1} - r*Re{mu} > -3*r/2,   j=1..l")
            (TERPRI* T)
            (PRIN2* "p=")
            (PRIN2* P)
            (PRIN2* ",q=")
            (PRIN2* Q)
            (TERPRI* T)
            (PRIN2* L)
            (PRIN2* " second upper parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'C
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ C)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'MU MU))
            NIL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE L J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR C)))
            (SETQ C_NEW (APPEND C_NEW (LIST TEMP1)))
            (SETQ C (CDR C)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (I)
           (SETQ I C_NEW)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ TEMP
                       (SIMP*
                        (REVAL1
                         (AEVAL
                          (LIST 'PLUS
                                (LIST 'DIFFERENCE
                                      (LIST 'TIMES (LIST 'DIFFERENCE P Q)
                                            (LIST 'REPART
                                                  (LIST 'PLUS ALPHA
                                                        (LIST 'DIFFERENCE I
                                                              1))))
                                      (LIST 'TIMES R (LIST 'REPART MU)))
                                (LIST 'TIMES (LIST 'QUOTIENT 3 2) R)))
                         T)))
               (COND
                ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
                 (SETQ FAIL_TEST T)))
               NIL))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst4 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST4
                        '(LIST 'GREATERP
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'DIFFERENCE 'P 'Q)
                                           (LIST 'REPART
                                                 (LIST 'PLUS 'ALPHA
                                                       (LIST 'DIFFERENCE 'CJ
                                                             1))))
                                     (LIST 'TIMES 'R
                                           (LIST 'REPART
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'SUM 'BJ)
                                                             (LIST 'SUM 'AI))
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 1
                                                                   2)
                                                             (LIST 'DIFFERENCE
                                                                   'P 'Q))
                                                       1))))
                               (LIST 'MINUS
                                     (LIST 'TIMES 3 (LIST 'QUOTIENT 'R 2)))))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST5 'NUMBER-OF-ARGS 7) 
(PUT 'TST5 'DEFINED-ON-LINE '1191) 
(PUT 'TST5 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST5 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST5 (K P Q D ALPHA R MU)
    (PROG (D_NEW TEMP1 TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst5: (p - q)*Re{alpha + dj} - r*Re{mu} > -3*r/2,  j=1..k")
            (TERPRI* T)
            (PRIN2* "p=")
            (PRIN2* P)
            (PRIN2* ",q=")
            (PRIN2* Q)
            (TERPRI* T)
            (PRIN2* K)
            (PRIN2* " second lower parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'D
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ D)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'MU MU))
            NIL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE K J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR D)))
            (SETQ D_NEW (APPEND D_NEW (LIST TEMP1)))
            (SETQ D (CDR D)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (I)
           (SETQ I D_NEW)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ TEMP
                       (SIMP*
                        (REVAL1
                         (AEVAL
                          (LIST 'PLUS
                                (LIST 'DIFFERENCE
                                      (LIST 'TIMES (LIST 'DIFFERENCE P Q)
                                            (LIST 'REPART
                                                  (LIST 'PLUS ALPHA I)))
                                      (LIST 'TIMES R (LIST 'REPART MU)))
                                (LIST 'TIMES (LIST 'QUOTIENT 3 2) R)))
                         T)))
               (COND
                ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
                 (SETQ FAIL_TEST 'T)))
               NIL))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst5 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST5
                        '(LIST 'GREATERP
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'DIFFERENCE 'P 'Q)
                                           (LIST 'REPART
                                                 (LIST 'PLUS 'ALPHA 'DJ)))
                                     (LIST 'TIMES 'R
                                           (LIST 'REPART
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'SUM 'BJ)
                                                             (LIST 'SUM 'AI))
                                                       (LIST 'QUOTIENT
                                                             (LIST 'DIFFERENCE
                                                                   'P 'Q)
                                                             2)
                                                       1))))
                               (LIST 'MINUS
                                     (LIST 'TIMES 3 (LIST 'QUOTIENT 'R 2)))))
                  TRANSFORM_LST))
         (RETURN T)))))) 
(PUT 'TST6 'NUMBER-OF-ARGS 7) 
(PUT 'TST6 'DEFINED-ON-LINE '1245) 
(PUT 'TST6 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST6 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST6 (N U V A ALPHA R RHO)
    (PROG (A_NEW TEMP1 TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst6: (u - v)*Re{alpha + r*ai - r} - Re{rho} > -3/2 ,  i=1..n")
            (TERPRI* T)
            (PRIN2* "u=")
            (PRIN2* U)
            (PRIN2* ",v=")
            (PRIN2* V)
            (TERPRI* T)
            (PRIN2* N)
            (PRIN2* " first upper parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'A
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ A)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'RHO RHO))
            NIL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR A)))
            (SETQ A_NEW (APPEND A_NEW (LIST TEMP1)))
            (SETQ A (CDR A)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (I)
           (SETQ I A_NEW)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ TEMP
                       (SIMP*
                        (REVAL1
                         (AEVAL
                          (LIST 'PLUS
                                (LIST 'DIFFERENCE
                                      (LIST 'TIMES (LIST 'DIFFERENCE U V)
                                            (LIST 'REPART
                                                  (LIST 'PLUS ALPHA
                                                        (LIST 'DIFFERENCE
                                                              (LIST 'TIMES R I)
                                                              R))))
                                      (LIST 'REPART RHO))
                                (LIST 'QUOTIENT 3 2)))
                         T)))
               (COND
                ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
                 (SETQ FAIL_TEST 'T)))
               NIL))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst6 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST 'T) (RETURN 'FAIL)) (T (RETURN 'T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST6
                        '(LIST 'GREATERP
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'DIFFERENCE 'U 'V)
                                           (LIST 'REPART
                                                 (LIST 'PLUS 'ALPHA
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'TIMES 'R
                                                                   'AI)
                                                             'R))))
                                     (LIST 'REPART
                                           (LIST 'PLUS
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'SUM 'DJ)
                                                       (LIST 'SUM 'CI))
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 1 2)
                                                       (LIST 'DIFFERENCE 'U
                                                             'V))
                                                 1)))
                               (LIST 'MINUS (LIST 'QUOTIENT 3 2))))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST7 'NUMBER-OF-ARGS 7) 
(PUT 'TST7 'DEFINED-ON-LINE '1298) 
(PUT 'TST7 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST7 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST7 (M U V B ALPHA R RHO)
    (PROG (B_NEW TEMP1 TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst7: (u - v)*Re{alpha + r*bi} - Re{rho} > -3/2,     i=1..m")
            (TERPRI* T)
            (PRIN2* "u=")
            (PRIN2* U)
            (PRIN2* ",v=")
            (PRIN2* V)
            (TERPRI* T)
            (PRIN2* M)
            (PRIN2* " first lower parameters: ")
            (MATHPRINT
             (LIST 'EQUAL 'B
                   (CONS 'LIST
                         (PROG (SQ FORALL-RESULT FORALL-ENDPTR)
                           (SETQ SQ B)
                           (COND ((NULL SQ) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (SQ) (PREPSQ SQ))
                                             (CAR SQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ SQ (CDR SQ))
                           (COND ((NULL SQ) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (SQ) (PREPSQ SQ)) (CAR SQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'R RHO))
            NIL)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
           (PROGN
            (SETQ TEMP1 (PREPSQ (CAR B)))
            (SETQ B_NEW (APPEND B_NEW (LIST TEMP1)))
            (SETQ B (CDR B)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (I)
           (SETQ I B_NEW)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ TEMP
                       (SIMP*
                        (REVAL1
                         (AEVAL
                          (LIST 'PLUS
                                (LIST 'DIFFERENCE
                                      (LIST 'TIMES (LIST 'DIFFERENCE U V)
                                            (LIST 'REPART
                                                  (LIST 'PLUS ALPHA
                                                        (LIST 'TIMES R I))))
                                      (LIST 'REPART RHO))
                                (LIST 'QUOTIENT 3 2)))
                         T)))
               (COND
                ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
                 (SETQ FAIL_TEST 'T)))
               NIL))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst7 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST7
                        '(LIST 'GREATERP
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'DIFFERENCE 'U 'V)
                                           (LIST 'REPART
                                                 (LIST 'PLUS 'ALPHA
                                                       (LIST 'TIMES 'R 'BI))))
                                     (LIST 'REPART
                                           (LIST 'PLUS
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'SUM 'DJ)
                                                       (LIST 'SUM 'CI))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'DIFFERENCE 'U 'V)
                                                       2)
                                                 1)))
                               (LIST 'MINUS (LIST 'QUOTIENT 3 2))))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST8 'NUMBER-OF-ARGS 9) 
(PUT 'TST8 'DEFINED-ON-LINE '1352) 
(PUT 'TST8 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST8 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE TST8 (P Q U V ALPHA R MU RHO PHI)
    (PROG (SUM TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst8: abs(phi) + 2*Re{(q - p)*(v - u)*alpha + r*(v - u)*(mu - 1) + (q - p)*(rho - 1)} > 0")
            (TERPRI* T)
            (PRIN2* "p=")
            (PRIN2* P)
            (PRIN2* ",q=")
            (PRIN2* Q)
            (PRIN2* ",u=")
            (PRIN2* U)
            (PRIN2* ",v=")
            (PRIN2* V)
            (TERPRI* T)
            (MATHPRINT (LIST 'EQUAL 'PHI PHI))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'MU MU))
            (MATHPRINT (LIST 'EQUAL 'RHO RHO))
            NIL)))
         (SETQ SUM
                 (REVAL1
                  (AEVAL
                   (LIST 'TIMES 2
                         (LIST 'REPART
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                                           (LIST 'DIFFERENCE V U) ALPHA)
                                     (LIST 'TIMES R (LIST 'DIFFERENCE V U)
                                           (LIST 'DIFFERENCE MU 1))
                                     (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                                           (LIST 'DIFFERENCE RHO 1))))))
                  T))
         (SETQ TEMP
                 (SIMP* (REVAL1 (AEVAL (LIST 'PLUS (LIST 'ABS PHI) SUM)) T)))
         (COND
          ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
           (SETQ FAIL_TEST 'T)))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst8 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST8
                        '(LIST 'GREATERP
                               (LIST 'PLUS
                                     (LIST 'ABS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE 'Q 'P)
                                                 (LIST 'TIMES 'R
                                                       (LIST 'DIFFERENCE 'V
                                                             'U))))
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'Q 'P)
                                                             (LIST 'DIFFERENCE
                                                                   'V 'U)
                                                             'ALPHA)
                                                       (LIST 'TIMES 'R
                                                             (LIST 'DIFFERENCE
                                                                   'V 'U)
                                                             (LIST 'PLUS
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'SUM
                                                                          'BJ)
                                                                    (LIST 'SUM
                                                                          'AI))
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST
                                                                     'DIFFERENCE
                                                                     'P 'Q)
                                                                    2)))
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'Q 'P)
                                                             (LIST 'PLUS
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'SUM
                                                                          'DJ)
                                                                    (LIST 'SUM
                                                                          'CI))
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST
                                                                     'DIFFERENCE
                                                                     'U 'V)
                                                                    2)))))))
                               0))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST9 'NUMBER-OF-ARGS 9) 
(PUT 'TST9 'DEFINED-ON-LINE '1408) 
(PUT 'TST9 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST9 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE TST9 (P Q U V ALPHA R MU RHO PHI)
    (PROG (SUM TEMP FAIL_TEST)
      (SETQ TRANSFORM_TST (REVAL1 (AEVAL 'TRANSFORM_TST) T))
      (COND
       ((NEQ TRANSFORM_TST 'T)
        (PROGN
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T
             "Checking tst9: abs(phi) - 2*Re{(q - p)*(v - u)*alpha + r*(v - u)*(mu - 1) + (q - p)*(rho - 1)} > 0")
            (TERPRI* T)
            (PRIN2* "p=")
            (PRIN2* P)
            (PRIN2* ",q=")
            (PRIN2* Q)
            (PRIN2* ",u=")
            (PRIN2* U)
            (PRIN2* ",v=")
            (PRIN2* V)
            (TERPRI* T)
            (MATHPRINT (LIST 'EQUAL 'PHI PHI))
            (MATHPRINT (LIST 'EQUAL 'ALPHA ALPHA))
            (MATHPRINT (LIST 'EQUAL 'R R))
            (MATHPRINT (LIST 'EQUAL 'MU MU))
            (MATHPRINT (LIST 'EQUAL 'RHO RHO))
            NIL)))
         (SETQ SUM
                 (REVAL1
                  (AEVAL
                   (LIST 'TIMES 2
                         (LIST 'REPART
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                                           (LIST 'DIFFERENCE V U) ALPHA)
                                     (LIST 'TIMES R (LIST 'DIFFERENCE V U)
                                           (LIST 'DIFFERENCE MU 1))
                                     (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                                           (LIST 'DIFFERENCE RHO 1))))))
                  T))
         (SETQ TEMP
                 (SIMP*
                  (REVAL1 (AEVAL (LIST 'DIFFERENCE (LIST 'ABS PHI) SUM)) T)))
         (COND
          ((OR (EQUAL (CAR TEMP) 'NIL) (LESSP (CAR TEMP) 0))
           (SETQ FAIL_TEST 'T)))
         (COND
          (*TRDEFINT
           (PROGN
            (PRIN2T "Result of tst9 is ")
            (PRIN2* (COND (FAIL_TEST 'FAIL) (T T)))
            (TERPRI* T))))
         (COND ((EQUAL FAIL_TEST T) (RETURN 'FAIL)) (T (RETURN T)))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST9
                        '(LIST 'GREATERP
                               (LIST 'DIFFERENCE
                                     (LIST 'ABS
                                           (LIST 'DIFFERENCE
                                                 (LIST 'DIFFERENCE 'Q 'P)
                                                 (LIST 'TIMES 'R
                                                       (LIST 'DIFFERENCE 'V
                                                             'U))))
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'Q 'P)
                                                             (LIST 'DIFFERENCE
                                                                   'V 'U)
                                                             'ALPHA)
                                                       (LIST 'TIMES 'R
                                                             (LIST 'DIFFERENCE
                                                                   'V 'U)
                                                             (LIST 'PLUS
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'SUM
                                                                          'BJ)
                                                                    (LIST 'SUM
                                                                          'AI))
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST
                                                                     'DIFFERENCE
                                                                     'P 'Q)
                                                                    2)))
                                                       (LIST 'TIMES
                                                             (LIST 'DIFFERENCE
                                                                   'Q 'P)
                                                             (LIST 'PLUS
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    (LIST 'SUM
                                                                          'DJ)
                                                                    (LIST 'SUM
                                                                          'CI))
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST
                                                                     'DIFFERENCE
                                                                     'U 'V)
                                                                    2)))))))
                               0))
                  TRANSFORM_LST))
         (RETURN 'T)))))) 
(PUT 'TST10 'NUMBER-OF-ARGS 2) 
(FLAG '(TST10) 'OPFN) 
(PUT 'TST10 'DEFINED-ON-LINE '1464) 
(PUT 'TST10 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST10 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TST10 (SIGMA DELTA)
    (PROG (ARG_SIGMA PRO TEMP FAIL_TEST *ROUNDED DMODE*)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI (AEVAL "Checking tst10: abs(arg sigma) < delta*pi") NIL
                      'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "sigma=") NIL 'FIRST)
             (ASSGNPRI (AEVAL SIGMA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "delta=") NIL 'FIRST)
             (ASSGNPRI (AEVAL DELTA) NIL 'LAST))
            (AEVAL 'NIL))))
         (AEVAL (ON (LIST 'ROUNDED)))
         (SETQ ARG_SIGMA
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'ATAN2 (LIST 'IMPART SIGMA)
                              (LIST 'REPART SIGMA)))))
         (SETQ PRO (AEVAL (LIST 'TIMES DELTA 'PI)))
         (SETQ TEMP (AEVAL (LIST 'DIFFERENCE PRO ARG_SIGMA)))
         (COND
          ((AND (EVALNUMBERP (AEVAL TEMP)) (EVALLEQ (AEVAL TEMP) 0))
           (SETQ FAIL_TEST (AEVAL 'T))))
         (AEVAL (OFF (LIST 'ROUNDED)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Result of tst10 is ") NIL 'FIRST)
             (ASSGNPRI
              (COND ((BOOLVALUE* (REVALX FAIL_TEST)) (AEVAL 'FAIL))
                    (T (AEVAL 'T)))
              NIL 'LAST))
            (AEVAL 'NIL))))
         (COND
          ((EVALEQUAL (AEVAL FAIL_TEST) (AEVAL 'T)) (RETURN (REVAL1 'FAIL T)))
          (T (RETURN (AEVAL (LIST 'REVAL 'T)))))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST10
                        '(LIST 'LESSP (LIST 'ABS (LIST 'ARG 'SIGMA))
                               (LIST 'TIMES
                                     (LIST 'PLUS 'K
                                           (LIST 'DIFFERENCE 'L
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'U 'V) 2)))
                                     'PI)))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'TST11 'NUMBER-OF-ARGS 2) 
(FLAG '(TST11) 'OPFN) 
(PUT 'TST11 'DEFINED-ON-LINE '1516) 
(PUT 'TST11 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST11 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TST11 (SIGMA DELTA)
    (PROG (ARG_SIGMA PRO FAIL_TEST)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI (AEVAL "Checking tst11: abs(arg sigma) = delta*pi") NIL
                      'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "sigma=") NIL 'FIRST)
             (ASSGNPRI (AEVAL SIGMA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "delta=") NIL 'FIRST)
             (ASSGNPRI (AEVAL DELTA) NIL 'LAST))
            (AEVAL 'NIL))))
         (SETQ ARG_SIGMA
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'ATAN2 (LIST 'IMPART SIGMA)
                              (LIST 'REPART SIGMA)))))
         (SETQ PRO (AEVAL (LIST 'TIMES DELTA 'PI)))
         (COND ((EVALNEQ (AEVAL ARG_SIGMA) (AEVAL PRO)) (SETQ FAIL_TEST 'T)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Result of tst11 is ") NIL 'FIRST)
             (ASSGNPRI
              (COND ((BOOLVALUE* (REVALX FAIL_TEST)) (AEVAL 'FAIL))
                    (T (AEVAL 'T)))
              NIL 'LAST))
            (AEVAL 'NIL))))
         (COND
          ((EVALEQUAL (AEVAL FAIL_TEST) (AEVAL 'T)) (RETURN (REVAL1 'FAIL T)))
          (T (RETURN (REVAL1 'T T))))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST11
                        '(LIST 'EQUAL (LIST 'ABS (LIST 'ARG 'SIGMA))
                               (LIST 'TIMES
                                     (LIST 'PLUS 'K
                                           (LIST 'DIFFERENCE 'L
                                                 (LIST 'QUOTIENT
                                                       (LIST 'PLUS 'U 'V) 2)))
                                     'PI)))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'TST12 'NUMBER-OF-ARGS 2) 
(FLAG '(TST12) 'OPFN) 
(PUT 'TST12 'DEFINED-ON-LINE '1553) 
(PUT 'TST12 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST12 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TST12 (OMEGA EPSILON)
    (PROG (ARG_OMEGA PRO TEMP FAIL_TEST *ROUNDED DMODE*)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (AEVAL (ON (LIST 'ROUNDED)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI (AEVAL "Checking tst12: abs(arg omega) < epsilon*pi") NIL
                      'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "omega=") NIL 'FIRST)
             (ASSGNPRI (AEVAL OMEGA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "epsilon=") NIL 'FIRST)
             (ASSGNPRI (AEVAL EPSILON) NIL 'LAST))
            (AEVAL 'NIL))))
         (SETQ ARG_OMEGA
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'ATAN2 (LIST 'IMPART OMEGA)
                              (LIST 'REPART OMEGA)))))
         (SETQ PRO (AEVAL (LIST 'TIMES EPSILON 'PI)))
         (SETQ TEMP (AEVAL (LIST 'DIFFERENCE PRO ARG_OMEGA)))
         (COND
          ((AND (EVALNUMBERP (AEVAL TEMP)) (EVALLEQ (AEVAL TEMP) 0))
           (SETQ FAIL_TEST 'T)))
         (AEVAL (OFF (LIST 'ROUNDED)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Result of tst12 is ") NIL 'FIRST)
             (ASSGNPRI
              (COND ((BOOLVALUE* (REVALX FAIL_TEST)) (AEVAL 'FAIL))
                    (T (AEVAL 'T)))
              NIL 'LAST))
            (AEVAL 'NIL))))
         (COND
          ((EVALEQUAL (AEVAL FAIL_TEST) (AEVAL 'T)) (RETURN (REVAL1 'FAIL T)))
          (T (RETURN (REVAL1 'T T))))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST12
                        '(LIST 'LESSP (LIST 'ABS (LIST 'ARG 'OMEGA))
                               (LIST 'TIMES
                                     (LIST 'PLUS 'M
                                           (LIST 'DIFFERENCE 'N
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 1 2)
                                                       (LIST 'PLUS 'P 'Q))))
                                     'PI)))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'TST13 'NUMBER-OF-ARGS 2) 
(FLAG '(TST13) 'OPFN) 
(PUT 'TST13 'DEFINED-ON-LINE '1598) 
(PUT 'TST13 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST13 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TST13 (OMEGA EPSILON)
    (PROG (ARG_OMEGA PRO FAIL_TEST)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI (AEVAL "Checking tst13: abs(arg omega) = epsilon*pi") NIL
                      'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "omega=") NIL 'FIRST)
             (ASSGNPRI (AEVAL OMEGA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "epsilon=") NIL 'FIRST)
             (ASSGNPRI (AEVAL EPSILON) NIL 'LAST))
            (AEVAL 'NIL))))
         (SETQ ARG_OMEGA
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'ATAN2 (LIST 'IMPART OMEGA)
                              (LIST 'REPART OMEGA)))))
         (SETQ PRO (AEVAL (LIST 'TIMES EPSILON 'PI)))
         (COND ((EVALNEQ (AEVAL ARG_OMEGA) (AEVAL PRO)) (SETQ FAIL_TEST 'T)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Result of tst13 is ") NIL 'FIRST)
             (ASSGNPRI
              (COND ((BOOLVALUE* (REVALX FAIL_TEST)) (AEVAL 'FAIL))
                    (T (AEVAL 'T)))
              NIL 'LAST))
            (AEVAL 'NIL))))
         (COND
          ((EVALEQUAL (AEVAL FAIL_TEST) (AEVAL 'T)) (RETURN (REVAL1 'FAIL T)))
          (T (RETURN (REVAL1 'T T))))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST13
                        '(LIST 'EQUAL (LIST 'ABS (LIST 'ARG 'OMEGA))
                               (LIST 'TIMES
                                     (LIST 'PLUS 'M
                                           (LIST 'DIFFERENCE 'N
                                                 (LIST 'TIMES
                                                       (LIST 'QUOTIENT 1 2)
                                                       (LIST 'PLUS 'P 'Q))))
                                     'PI)))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'TST14 'NUMBER-OF-ARGS 13) 
(FLAG '(TST14) 'OPFN) 
(PUT 'TST14 'DEFINED-ON-LINE '1635) 
(PUT 'TST14 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST14 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST14 (U V ALPHA MU RHO DELTA EPSILON SIGMA OMEGA R PHI R1 R2)
    (PROG (TEMP Z ARG ARG_TEST *ROUNDED DMODE*)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (AEVAL (ON (LIST 'ROUNDED)))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI
             (AEVAL
              "Checking tst14: Compute z = r^[r1*(v - u)]*exp[-(r1*delta + r2*epsilon)*pi*i]")
             NIL 'ONLY)
            (ASSGNPRI
             (AEVAL
              "                  abs(arg(1 - z*sigma^(-r1)*omega^r2)) < pi when phi = 0 and epsilon + r*(delta - 1) <= 0")
             NIL 'ONLY)
            (ASSGNPRI (AEVAL "                 or") NIL 'ONLY)
            (ASSGNPRI
             (AEVAL
              "                   z = sigma^r1*omega^(-r2) when Re{mu + rho + alpha*(v - u)}<1")
             NIL 'ONLY)
            (PROGN
             (ASSGNPRI (AEVAL "u=") NIL 'FIRST)
             (ASSGNPRI (AEVAL U) NIL NIL)
             (ASSGNPRI (AEVAL ",v=") NIL NIL)
             (ASSGNPRI (AEVAL V) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "r=r1/r2=") NIL 'FIRST)
             (ASSGNPRI (AEVAL R) NIL NIL)
             (ASSGNPRI (AEVAL "; r1=") NIL NIL)
             (ASSGNPRI (AEVAL R1) NIL NIL)
             (ASSGNPRI (AEVAL ",r2=") NIL NIL)
             (ASSGNPRI (AEVAL R2) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "alpha=") NIL 'FIRST)
             (ASSGNPRI (AEVAL ALPHA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "phi=") NIL 'FIRST)
             (ASSGNPRI (AEVAL PHI) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "mu=") NIL 'FIRST)
             (ASSGNPRI (AEVAL MU) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "rho=") NIL 'FIRST)
             (ASSGNPRI (AEVAL RHO) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "sigma=") NIL 'FIRST)
             (ASSGNPRI (AEVAL SIGMA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "delta=") NIL 'FIRST)
             (ASSGNPRI (AEVAL DELTA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "omega=") NIL 'FIRST)
             (ASSGNPRI (AEVAL OMEGA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "epsilon=") NIL 'FIRST)
             (ASSGNPRI (AEVAL EPSILON) NIL 'LAST))
            (AEVAL 'NIL))))
         (SETQ TEMP
                 (AEVAL
                  (LIST 'PLUS EPSILON
                        (LIST 'TIMES R (LIST 'DIFFERENCE DELTA 1)))))
         (COND
          ((AND (EVALEQUAL (AEVAL PHI) 0) (EVALLEQ (AEVAL TEMP) 0))
           (PROGN
            (SETQ Z
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'EXPT R
                                 (LIST 'TIMES R2 (LIST 'DIFFERENCE V U)))
                           (LIST 'EXPT 'E
                                 (LIST 'MINUS
                                       (LIST 'TIMES
                                             (LIST 'PLUS (LIST 'TIMES R2 DELTA)
                                                   (LIST 'TIMES R1 EPSILON))
                                             'PI 'I))))))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (PROGN
               (ASSGNPRI (AEVAL "Case 1: z=") NIL 'FIRST)
               (ASSGNPRI (AEVAL Z) NIL 'LAST))))
            (AEVAL 'NIL)))
          ((AND
            (EVALNUMBERP
             (AEVAL
              (LIST 'PLUS MU RHO (LIST 'TIMES ALPHA (LIST 'DIFFERENCE V U)))))
            (EVALLESSP
             (AEVAL
              (LIST 'REPART
                    (LIST 'PLUS MU RHO
                          (LIST 'TIMES ALPHA (LIST 'DIFFERENCE V U)))))
             1))
           (PROGN
            (SETQ Z
                    (AEVAL
                     (LIST 'TIMES (LIST 'EXPT SIGMA R2)
                           (LIST 'EXPT OMEGA (LIST 'MINUS R1)))))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (PROGN
               (ASSGNPRI (AEVAL "Case 2: z=") NIL 'FIRST)
               (ASSGNPRI (AEVAL Z) NIL 'LAST))))))
          (T
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst14 is fail") NIL 'ONLY)))
            (RETURN (REVAL1 'FAIL T)))))
         (SETQ ARG
                 (AEVAL
                  (LIST 'DIFFERENCE 1
                        (LIST 'TIMES Z (LIST 'EXPT SIGMA (LIST 'MINUS R2))
                              (LIST 'EXPT OMEGA R1)))))
         (COND ((EVALEQUAL (AEVAL ARG) 0) (SETQ ARG_TEST (AEVAL 0)))
               (T
                (SETQ ARG_TEST
                        (AEVAL
                         (LIST 'ABS
                               (LIST 'ATAN2 (LIST 'IMPART ARG)
                                     (LIST 'REPART ARG)))))))
         (COND
          ((AND (EVALNUMBERP (AEVAL ARG_TEST))
                (EVALLESSP (AEVAL ARG_TEST) (AEVAL 'PI)))
           (PROGN
            (AEVAL (OFF (LIST 'ROUNDED)))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst14 is t") NIL 'ONLY)))
            (RETURN (REVAL1 'T T))))
          (T
           (PROGN
            (AEVAL (OFF (LIST 'ROUNDED)))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst14 is fail") NIL 'ONLY)))
            (RETURN (REVAL1 'FAIL T)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST14
                        '(LIST 'OR
                               (LIST 'AND
                                     (LIST 'ABS
                                           (LIST 'ARG
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'TIMES
                                                             (LIST 'TIMES
                                                                   (LIST 'EXPT
                                                                         'R
                                                                         (LIST
                                                                          'TIMES
                                                                          'R1
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'V
                                                                           'U)))
                                                                   (LIST 'EXP
                                                                         (LIST
                                                                          'MINUS
                                                                          (LIST
                                                                           'TIMES
                                                                           (LIST
                                                                            'PLUS
                                                                            (LIST
                                                                             'TIMES
                                                                             'R1
                                                                             (LIST
                                                                              'PLUS
                                                                              'K
                                                                              (LIST
                                                                               'DIFFERENCE
                                                                               'L
                                                                               (LIST
                                                                                'TIMES
                                                                                (LIST
                                                                                 'QUOTIENT
                                                                                 1
                                                                                 2)
                                                                                (LIST
                                                                                 'PLUS
                                                                                 'U
                                                                                 'V)))))
                                                                            (LIST
                                                                             'TIMES
                                                                             'R2
                                                                             (LIST
                                                                              'PLUS
                                                                              'M
                                                                              (LIST
                                                                               'DIFFERENCE
                                                                               'N
                                                                               (LIST
                                                                                'TIMES
                                                                                (LIST
                                                                                 'QUOTIENT
                                                                                 1
                                                                                 2)
                                                                                (LIST
                                                                                 'DIFFERENCE
                                                                                 'P
                                                                                 'Q))))))
                                                                           'PI
                                                                           'I))))
                                                             (LIST 'EXPT 'SIGMA
                                                                   (LIST 'MINUS
                                                                         'R1))
                                                             (LIST 'EXPT 'OMEGA
                                                                   'R2)))))
                                     (LIST 'EQUAL 'PHI 0)
                                     (LIST 'LEQ
                                           (LIST 'PLUS 'K
                                                 (LIST 'DIFFERENCE 'L
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 1
                                                                   2)
                                                             (LIST 'PLUS 'U
                                                                   'V)))
                                                 (LIST 'TIMES 'R
                                                       (LIST 'PLUS 'M
                                                             (LIST 'DIFFERENCE
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'QUOTIENT
                                                                      1 2)
                                                                     (LIST
                                                                      'PLUS 'P
                                                                      'Q)))
                                                                   1))))
                                           0))
                               (LIST 'AND
                                     (LIST 'LESSP
                                           (LIST 'REPART
                                                 (LIST 'PLUS
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'SUM 'BJ)
                                                             (LIST 'SUM 'AI))
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 1
                                                                   2)
                                                             (LIST 'DIFFERENCE
                                                                   'P 'Q))
                                                       1
                                                       (LIST 'DIFFERENCE
                                                             (LIST 'SUM 'DJ)
                                                             (LIST 'SUM 'CI))
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 1
                                                                   2)
                                                             (LIST 'DIFFERENCE
                                                                   'U 'V))
                                                       1
                                                       (LIST 'TIMES 'ALPHA
                                                             (LIST 'DIFFERENCE
                                                                   'V 'U))))
                                           0)
                                     (LIST 'EQUAL 'PHI 0)
                                     (LIST 'LEQ
                                           (LIST 'PLUS 'K
                                                 (LIST 'DIFFERENCE 'L
                                                       (LIST 'TIMES
                                                             (LIST 'QUOTIENT 1
                                                                   2)
                                                             (LIST 'PLUS 'U
                                                                   'V)))
                                                 (LIST 'TIMES 'R
                                                       (LIST 'PLUS 'M
                                                             (LIST 'DIFFERENCE
                                                                   (LIST
                                                                    'DIFFERENCE
                                                                    'N
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'QUOTIENT
                                                                      1 2)
                                                                     (LIST
                                                                      'PLUS 'P
                                                                      'Q)))
                                                                   1))))
                                           0))))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'TST15 'NUMBER-OF-ARGS 11) 
(FLAG '(TST15) 'OPFN) 
(PUT 'TST15 'DEFINED-ON-LINE '1730) 
(PUT 'TST15 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TST15 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TST15 (M N P Q K L U V SIGMA OMEGA ETA)
    (PROG (LC LS TEMP_LS PSI THETA ARG_OMEGA ARG_SIGMA *ROUNDED DMODE*)
      (COND
       ((EVALNEQ (AEVAL 'TRANSFORM_TST) (AEVAL 'T))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (ASSGNPRI (AEVAL "Checking tst15:") NIL 'ONLY)
            (PROGN
             (PRIN2T "Compute ")
             (MATHPRINT
              (LIST 'SETQ 'LAMBDA_C
                    (LIST 'PLUS
                          (LIST 'TIMES '(DIFFERENCE Q P)
                                '(EXPT (ABS OMEGA)
                                       (QUOTIENT 1 (DIFFERENCE Q P)))
                                '(COS PSI))
                          (LIST 'TIMES '(DIFFERENCE V U)
                                '(EXPT (ABS SIGMA)
                                       (QUOTIENT 1 (DIFFERENCE V U)))
                                '(COS THETA)))))
             (PRIN2T " and ")
             (MATHPRINT
              (LIST 'SETQ 'LAMBDA_S
                    (LIST 'PLUS
                          (LIST 'TIMES '(DIFFERENCE Q P)
                                '(EXPT (ABS OMEGA)
                                       (QUOTIENT 1 (DIFFERENCE Q P)))
                                '(SIGN (ARG OMEGA)) '(SIN PSI))
                          (LIST 'TIMES '(DIFFERENCE V U)
                                '(EXPT (ABS SIGMA)
                                       (QUOTIENT 1 (DIFFERENCE V U)))
                                '(SIGN (ARG SIGMA)) '(SIN THETA)))))
             (PRIN2T " where ")
             (MATHPRINT
              (LIST 'SETQ 'PSI
                    (LIST 'QUOTIENT
                          (LIST 'PLUS '(TIMES PI (PLUS Q (MINUS M) (MINUS N)))
                                '(ABS (ARG OMEGA)))
                          '(DIFFERENCE Q P))))
             (PRIN2T " and ")
             (MATHPRINT
              (LIST 'SETQ 'THETA
                    (LIST 'QUOTIENT
                          (LIST 'PLUS '(TIMES PI (PLUS V (MINUS K) (MINUS L)))
                                '(ABS (ARG SIGMA)))
                          '(DIFFERENCE V U))))
             NIL)
            (PROGN
             (ASSGNPRI (AEVAL "m=") NIL 'FIRST)
             (ASSGNPRI (AEVAL M) NIL NIL)
             (ASSGNPRI (AEVAL ",n=") NIL NIL)
             (ASSGNPRI (AEVAL N) NIL NIL)
             (ASSGNPRI (AEVAL ",p=") NIL NIL)
             (ASSGNPRI (AEVAL P) NIL NIL)
             (ASSGNPRI (AEVAL ",q=") NIL NIL)
             (ASSGNPRI (AEVAL Q) NIL NIL)
             (ASSGNPRI (AEVAL ",k=") NIL NIL)
             (ASSGNPRI (AEVAL K) NIL NIL)
             (ASSGNPRI (AEVAL ",l=") NIL NIL)
             (ASSGNPRI (AEVAL L) NIL NIL)
             (ASSGNPRI (AEVAL ",u=") NIL NIL)
             (ASSGNPRI (AEVAL U) NIL NIL)
             (ASSGNPRI (AEVAL ",v=") NIL NIL)
             (ASSGNPRI (AEVAL V) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "sigma=") NIL 'FIRST)
             (ASSGNPRI (AEVAL SIGMA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "omega=") NIL 'FIRST)
             (ASSGNPRI (AEVAL OMEGA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "eta=") NIL 'FIRST)
             (ASSGNPRI (AEVAL ETA) NIL 'LAST))
            (AEVAL (PRIN2T "The test fails immediately if"))
            (AEVAL
             (MATHPRINT '(OR (EQUAL (ARG SIGMA) 0) (EQUAL (ARG OMEGA) 0))))
            (AEVAL (PRIN2T "The test succeeds when"))
            (AEVAL
             (MATHPRINT
              (LIST 'OR '(GREATERP LAMBDA_C 0)
                    '(AND (EQUAL LAMBDA_C 0) (NOT (EQUAL LAMBDA_S 0))
                          (GREATERP (REPART ETA) -1))
                    '(AND (EQUAL LAMBDA_C 0) (EQUAL LAMBDA_S 0)
                          (GREATERP (REPART ETA) 0)))))
            (AEVAL 'NIL))))
         (SETQ ARG_OMEGA
                 (AEVAL
                  (LIST 'ATAN2 (LIST 'IMPART OMEGA) (LIST 'REPART OMEGA))))
         (SETQ ARG_SIGMA
                 (AEVAL
                  (LIST 'ATAN2 (LIST 'IMPART SIGMA) (LIST 'REPART SIGMA))))
         (SETQ PSI
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS (LIST 'ABS ARG_OMEGA)
                              (LIST 'TIMES
                                    (LIST 'DIFFERENCE (LIST 'DIFFERENCE Q M) N)
                                    'PI))
                        (LIST 'DIFFERENCE Q P))))
         (SETQ THETA
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS (LIST 'ABS ARG_SIGMA)
                              (LIST 'TIMES
                                    (LIST 'DIFFERENCE (LIST 'DIFFERENCE V K) L)
                                    'PI))
                        (LIST 'DIFFERENCE V U))))
         (SETQ LC
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                              (LIST 'EXPT (LIST 'ABS OMEGA)
                                    (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE Q P)))
                              (LIST 'COS PSI))
                        (LIST 'TIMES (LIST 'DIFFERENCE V U)
                              (LIST 'EXPT (LIST 'ABS SIGMA)
                                    (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE V U)))
                              (LIST 'COS THETA)))))
         (SETQ LC (AEVAL LC))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "arg(omega)=") NIL 'FIRST)
             (ASSGNPRI (AEVAL ARG_OMEGA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "arg(sigma)=") NIL 'FIRST)
             (ASSGNPRI (AEVAL ARG_SIGMA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "psi=") NIL 'FIRST)
             (ASSGNPRI (AEVAL PSI) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "theta=") NIL 'FIRST)
             (ASSGNPRI (AEVAL THETA) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "lambda_c=") NIL 'FIRST)
             (ASSGNPRI (AEVAL LC) NIL 'LAST))
            (AEVAL 'NIL))))
         (SETQ TEMP_LS
                 (AEVAL
                  (LIST 'PLUS
                        (LIST 'TIMES (LIST 'DIFFERENCE Q P)
                              (LIST 'EXPT (LIST 'ABS OMEGA)
                                    (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE Q P)))
                              (LIST 'SIGN ARG_OMEGA) (LIST 'SIN PSI))
                        (LIST 'TIMES (LIST 'DIFFERENCE V U)
                              (LIST 'EXPT (LIST 'ABS SIGMA)
                                    (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE V U)))
                              (LIST 'SIGN ARG_SIGMA) (LIST 'SIN THETA)))))
         (COND
          ((EVALNEQ (AEVAL (LIST 'TIMES ARG_SIGMA ARG_OMEGA)) 0)
           (SETQ LS (AEVAL TEMP_LS)))
          (T
           (PROGN
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst15 is fail") NIL 'ONLY)))
            (RETURN (REVAL1 'FAIL T)))))
         (COND
          ((BOOLVALUE* (REVALX *TRDEFINT))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "lambda_s=") NIL 'FIRST)
             (ASSGNPRI (AEVAL LS) NIL 'LAST))
            (AEVAL 'NIL))))
         (AEVAL (ON (LIST 'ROUNDED)))
         (COND
          ((OR (AND (EVALNUMBERP (AEVAL LC)) (EVALGREATERP (AEVAL LC) 0))
               (AND (EVALEQUAL (AEVAL LC) 0) (EVALEQUAL (AEVAL LS) 0)
                    (EVALGREATERP (AEVAL (LIST 'REPART ETA)) (MINUS 1)))
               (AND (EVALEQUAL (AEVAL LC) 0) (EVALEQUAL (AEVAL LS) 0)
                    (EVALGREATERP (AEVAL (LIST 'REPART ETA)) 0)))
           (PROGN
            (AEVAL (OFF (LIST 'ROUNDED)))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst14 is t") NIL 'ONLY)))
            (RETURN (REVAL1 'T T))))
          (T
           (PROGN
            (AEVAL (OFF (LIST 'ROUNDED)))
            (COND
             ((BOOLVALUE* (REVALX *TRDEFINT))
              (ASSGNPRI (AEVAL "Result of tst15 is fail") NIL 'ONLY)))
            (RETURN (REVAL1 'FAIL T)))))))
       (T
        (PROGN
         (SETQ TRANSFORM_LST
                 (CONS
                  (CONS 'TEST15
                        '(LIST 'OR (LIST 'GREATERP 'LAMBDA_C 0)
                               (LIST 'AND (LIST 'EQUAL 'LAMBDA_C 0)
                                     (LIST 'NEQ 'LAMBDA_S 0)
                                     (LIST 'GREATERP (LIST 'REPART 'ETA)
                                           (LIST 'MINUS 1)))
                               (LIST 'AND (LIST 'EQUAL 'LAMBDA_C 0)
                                     (LIST 'EQUAL 'LAMBDA_S 0)
                                     (LIST 'GREATERP (LIST 'REPART 'ETA) 0))))
                  TRANSFORM_LST))
         (RETURN (REVAL1 'T T))))))) 
(PUT 'BASTAB 'NUMBER-OF-ARGS 2) 
(PUT 'BASTAB 'DEFINED-ON-LINE '1854) 
(PUT 'BASTAB 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'BASTAB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BASTAB (U V)
    (COND ((EQ U '|DEFINT:OPF1|) (SUBPAR (GET '|DEFINT:OPF1| 'G) V))
          ((EQ U '|DEFINT:OPF2|) (SUBPAR (GET '|DEFINT:OPF2| 'G) V)))) 
(PUT 'SUBPAR 'NUMBER-OF-ARGS 2) 
(PUT 'SUBPAR 'DEFINED-ON-LINE '1858) 
(PUT 'SUBPAR 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SUBPAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBPAR (U V)
    (COND
     ((NULL V)
      (LIST (CADR U) (CADDR U) (CADDDR U) (CAR (CDDDDR U)) (CADR (CDDDDR U))))
     (T
      (LIST (CADR U) (SUBLIST1 (CADDR U) V (CAR U))
            (SUBLIST1 (CADDDR U) V (CAR U))
            (SUBPREF1 (CAR (CDDDDR U)) V (CAR U)) (CADR (CDDDDR U)))))) 
(PUT 'SUBLIST1 'NUMBER-OF-ARGS 3) 
(PUT 'SUBLIST1 'DEFINED-ON-LINE '1865) 
(PUT 'SUBLIST1 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SUBLIST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBLIST1 (U V Z)
    (COND
     ((OR (NULL (CDR V)) (NULL (CDR Z))) (DEFINT_SUBLIST U (CAR V) (CAR Z)))
     (T (SUBLIST1 (DEFINT_SUBLIST U (CAR V) (CAR Z)) (CDR V) (CDR Z))))) 
(PUT 'SUBPREF1 'NUMBER-OF-ARGS 3) 
(PUT 'SUBPREF1 'DEFINED-ON-LINE '1873) 
(PUT 'SUBPREF1 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SUBPREF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBPREF1 (U V Z)
    (COND ((OR (NULL (CDR V)) (NULL (CDR Z))) (SUBPREF U (CAR V) (CAR Z)))
          (T (SUBPREF (SUBPREF1 U (CDR V) (CDR Z)) (CAR V) (CAR Z))))) 
(PUT 'SUBPREF 'NUMBER-OF-ARGS 3) 
(PUT 'SUBPREF 'DEFINED-ON-LINE '1879) 
(PUT 'SUBPREF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SUBPREF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBPREF (U V Z) (PREPSQ (SUBSQNEW (SIMP* U) (SIMP* V) Z))) 
(PUT 'DEFINT_SUBLIST 'NUMBER-OF-ARGS 3) 
(PUT 'DEFINT_SUBLIST 'DEFINED-ON-LINE '1883) 
(PUT 'DEFINT_SUBLIST 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_SUBLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT_SUBLIST (U V Z)
    (COND ((NULL U) NIL)
          (T (CONS (SUBPREF (CAR U) V Z) (DEFINT_SUBLIST (CDR U) V Z))))) 
(PUT 'TRPAR 'NUMBER-OF-ARGS 3) 
(PUT 'TRPAR 'DEFINED-ON-LINE '1889) 
(PUT 'TRPAR 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'TRPAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRPAR (U1 U2 U3)
    (COND ((EQCAR U2 'PLUS) 'FAIL)
          (T
           (PROG (A3 L1 V1 V2 V3 V4)
             (COND
              ((OR (EQUAL (SETQ V1 (DUBDEG (CAR (SIMP U1)) 'X)) 'FAIL)
                   (EQUAL (SETQ V2 (DUBDEG (CDR (SIMP U1)) 'X)) 'FAIL)
                   (EQUAL (SETQ V3 (DUBDEG (CAR (SIMP U2)) U3)) 'FAIL)
                   (EQUAL (SETQ V4 (DUBDEG (CDR (SIMP U2)) U3)) 'FAIL))
               (RETURN 'FAIL)))
             (SETQ A3 (MULTSQ (ADDSQ V1 (NEGSQ V2)) (ADDSQ V3 (NEGSQ V4))))
             (SETQ L1 (SUBPREF U1 U2 'X))
             (SETQ L1 (SUBPREF L1 1 U3))
             (RETURN (LIST (SIMP* L1) A3)))))) 
(PUT 'MODINTGG 'NUMBER-OF-ARGS 3) 
(PUT 'MODINTGG 'DEFINED-ON-LINE '1909) 
(PUT 'MODINTGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'MODINTGG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODINTGG (U1 U2 U3)
    (LIST (MULTSQ U1 (INVSQ (CADAR U2)))
          (DEFINT_CHANGE U2 (LIST (CONS (CAAR U2) (LIST '(1 . 1)))) '(1))
          (DEFINT_CHANGE U3
           (LIST
            (CONS (CAAR U3) (LIST (MULTSQ (CADAR U3) (INVSQ (CADAR U2))))))
           '(1)))) 
(PUT 'DEFINT_CHANGE 'NUMBER-OF-ARGS 3) 
(PUT 'DEFINT_CHANGE 'DEFINED-ON-LINE '1915) 
(PUT 'DEFINT_CHANGE 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_CHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DEFINT_CHANGE (U1 U2 U3)
    (PROG (V K)
      (SETQ K 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT U1) (RETURN NIL)))
        (PROG ()
          (COND
           ((AND U3 (EQUAL (CAR U3) (SETQ K (PLUS K 1))))
            (PROGN
             (SETQ V (APPEND V (LIST (CAR U2))))
             (COND (U2 (SETQ U2 (CDR U2))))
             (COND (U3 (SETQ U3 (CDR U3))))))
           (T (SETQ V (APPEND V (LIST (CAR U1))))))
          (SETQ U1 (CDR U1))
          (COND ((NULL U3) (PROGN (SETQ V (APPEND V U1)) (SETQ U1 NIL)))))
        (GO WHILELABEL))
      (RETURN V))) 
(PUT 'CONG 'NUMBER-OF-ARGS 1) 
(PUT 'CONG 'DEFINED-ON-LINE '1931) 
(PUT 'CONG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'CONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONG (U)
    (LIST (LIST (INVSQ (CAAR U)) (NEGSQ (CADAR U)))
          (LIST (CADADR U) (CAADR U) (CADDDR (CADR U)) (CADDR (CADR U)))
          (PROG (UU FORALL-RESULT FORALL-ENDPTR)
            (SETQ UU
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADDDR U))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND ((NULL UU) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1))))
                              (CAR UU))
                             NIL)))
           LOOPLABEL
            (SETQ UU (CDR UU))
            (COND ((NULL UU) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1)))) (CAR UU))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (PROG (UU FORALL-RESULT FORALL-ENDPTR)
            (SETQ UU
                    (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                      (SETQ UU (CADDR U))
                      (COND ((NULL UU) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU))
                                            NIL)))
                     LOOPLABEL
                      (SETQ UU (CDR UU))
                      (COND ((NULL UU) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND ((NULL UU) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1))))
                              (CAR UU))
                             NIL)))
           LOOPLABEL
            (SETQ UU (CDR UU))
            (COND ((NULL UU) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (UU) (ADDSQ UU (NEGSQ '(-1 . 1)))) (CAR UU))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'MODINTG 'NUMBER-OF-ARGS 2) 
(PUT 'MODINTG 'DEFINED-ON-LINE '1938) 
(PUT 'MODINTG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'MODINTG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODINTG (U1 U2)
    (LIST (MULTSQ U1 (INVSQ (CADAR U2)))
          (DEFINT_CHANGE U2 (LIST (CONS (CAAR U2) (LIST '(1 . 1)))) '(1)))) 
(PUT 'CCGF 'NUMBER-OF-ARGS 1) 
(PUT 'CCGF 'DEFINED-ON-LINE '1945) 
(PUT 'CCGF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'CCGF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CCGF (U)
    (MULTSQ
     (SIMP
      (PLUS (TIMES 2 (CAADR U))
            (DIFFERENCE (DIFFERENCE (TIMES 2 (CADADR U)) (CADDR (CADR U)))
                        (CADDDR (CADR U)))))
     (INVSQ '(2 . 1)))) 
(PUT 'VGG 'NUMBER-OF-ARGS 2) 
(PUT 'VGG 'DEFINED-ON-LINE '1950) 
(PUT 'VGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'VGG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VGG (U1 U2)
    (ADDSQ (SIMP (DIFFERENCE (CADDDR (CADR U2)) (CADDR (CADR U2))))
           (NEGSQ
            (MULTSQ (CADAR U2)
                    (SIMP (DIFFERENCE (CADDDR (CADR U1)) (CADDR (CADR U1)))))))) 
(PUT 'NUGG 'NUMBER-OF-ARGS 3) 
(PUT 'NUGG 'DEFINED-ON-LINE '1955) 
(PUT 'NUGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'NUGG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUGG (U1 U2 U3)
    (ADDSQ
     (ADDSQ '(1 . 1)
            (NEGSQ
             (MULTSQ U3
                     (SIMP
                      (DIFFERENCE (CADDDR (CADR U1)) (CADDR (CADR U1)))))))
     (NEGSQ (ADDSQ (MUGF U2) (MUGF U1))))) 
(DE SUMLISTSQ (U)
    ((LAMBDA (P)
       (PROGN
        (PROG (PP)
          (SETQ PP U)
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP) (PROGN (SETQ P (ADDSQ PP P)))) (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))
        P))
     '(NIL . 1))) 
(PUT 'SUMLISTSQ 'NUMBER-OF-ARGS 1) 
(PUT 'SUMLISTSQ 'DEFINED-ON-LINE '1959) 
(PUT 'SUMLISTSQ 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'SUMLISTSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SUMLISTSQ 'INLINE
      '(LAMBDA (U)
         ((LAMBDA (P)
            (PROGN
             (PROG (PP)
               (SETQ PP U)
              LAB
               (COND ((NULL PP) (RETURN NIL)))
               ((LAMBDA (PP) (PROGN (SETQ P (ADDSQ PP P)))) (CAR PP))
               (SETQ PP (CDR PP))
               (GO LAB))
             P))
          '(NIL . 1)))) 
(PUT 'MUGF 'NUMBER-OF-ARGS 1) 
(PUT 'MUGF 'DEFINED-ON-LINE '1962) 
(PUT 'MUGF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'MUGF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MUGF (U)
    (ADDSQ
     (MULTSQ (SIMP (PLUS 2 (DIFFERENCE (CADDR (CADR U)) (CADDDR (CADR U)))))
             (INVSQ '(2 . 1)))
     (ADDSQ
      ((LAMBDA (P)
         (PROGN
          (PROG (PP)
            (SETQ PP (CADDDR U))
           LAB
            (COND ((NULL PP) (RETURN NIL)))
            ((LAMBDA (PP) (PROGN (SETQ P (ADDSQ PP P)))) (CAR PP))
            (SETQ PP (CDR PP))
            (GO LAB))
          P))
       '(NIL . 1))
      (NEGSQ
       ((LAMBDA (P)
          (PROGN
           (PROG (PP)
             (SETQ PP (CADDR U))
            LAB
             (COND ((NULL PP) (RETURN NIL)))
             ((LAMBDA (PP) (PROGN (SETQ P (ADDSQ PP P)))) (CAR PP))
             (SETQ PP (CDR PP))
             (GO LAB))
           P))
        '(NIL . 1)))))) 
(PUT 'COEFINTG 'NUMBER-OF-ARGS 3) 
(PUT 'COEFINTG 'DEFINED-ON-LINE '1967) 
(PUT 'COEFINTG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'COEFINTG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFINTG (U1 U2 U3)
    ((LAMBDA (P)
       (PROGN
        (PROG (PP)
          (SETQ PP
                  (LIST (EXPDEG (CONS (CDADAR U2) 1) (MUGF U2))
                        (EXPDEG (CONS (CAADAR U2) 1)
                                (ADDSQ (MUGF U1)
                                       (ADDSQ
                                        (MULTSQ U3
                                                (CONS
                                                 (DIFFERENCE (CADDDR (CADR U1))
                                                             (CADDR (CADR U1)))
                                                 1))
                                        (NEGSQ '(1 . 1)))))
                        (EXPDEG (CAAR U1) (NEGSQ U3))
                        (EXPDEG (SIMP '(TIMES 2 PI))
                                (ADDSQ
                                 (MULTSQ (CCGF U1)
                                         (CONS (DIFFERENCE 1 (CAADAR U2)) 1))
                                 (MULTSQ (CCGF U2)
                                         (CONS (DIFFERENCE 1 (CDADAR U2))
                                               1))))))
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))
        P))
     '(1 . 1))) 
(PUT 'DELTAGG 'NUMBER-OF-ARGS 3) 
(PUT 'DELTAGG 'DEFINED-ON-LINE '1984) 
(PUT 'DELTAGG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DELTAGG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DELTAGG (U1 U2 U3)
    (LIST
     (APPEND (DEFINT_DELTA (CAR (REDPAR1 (CADDR U2) (CADADR U2))) (CDADAR U2))
             (APPEND
              (DEFINT_DELTA
               ((LAMBDA (G130)
                  (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                    (SETQ UU
                            (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                              (SETQ UU (CADDDR U1))
                              (COND ((NULL UU) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (UU) (NEGSQ UU))
                                                (CAR UU))
                                               NIL)))
                             LOOPLABEL
                              (SETQ UU (CDR UU))
                              (COND ((NULL UU) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    (COND ((NULL UU) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (UU) (ADDSQ UU (NEGSQ G130)))
                                      (CAR UU))
                                     NIL)))
                   LOOPLABEL
                    (SETQ UU (CDR UU))
                    (COND ((NULL UU) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (UU) (ADDSQ UU (NEGSQ G130))) (CAR UU))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (ADDSQ U3 '(-1 . 1)))
               (CAADAR U2))
              (DEFINT_DELTA (CDR (REDPAR1 (CADDR U2) (CADADR U2)))
               (CDADAR U2))))
     (APPEND (DEFINT_DELTA (CAR (REDPAR1 (CADDDR U2) (CAADR U2))) (CDADAR U2))
             (APPEND
              (DEFINT_DELTA
               ((LAMBDA (G132)
                  (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                    (SETQ UU
                            (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                              (SETQ UU (CADDR U1))
                              (COND ((NULL UU) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (UU) (NEGSQ UU))
                                                (CAR UU))
                                               NIL)))
                             LOOPLABEL
                              (SETQ UU (CDR UU))
                              (COND ((NULL UU) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (UU) (NEGSQ UU)) (CAR UU))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    (COND ((NULL UU) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (UU) (ADDSQ UU (NEGSQ G132)))
                                      (CAR UU))
                                     NIL)))
                   LOOPLABEL
                    (SETQ UU (CDR UU))
                    (COND ((NULL UU) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (UU) (ADDSQ UU (NEGSQ G132))) (CAR UU))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (ADDSQ U3 '(-1 . 1)))
               (CAADAR U2))
              (DEFINT_DELTA (CDR (REDPAR1 (CADDDR U2) (CAADR U2)))
               (CDADAR U2)))))) 
(PUT 'REDPARGF 'NUMBER-OF-ARGS 1) 
(PUT 'REDPARGF 'DEFINED-ON-LINE '1993) 
(PUT 'REDPARGF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'REDPARGF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDPARGF (U)
    (PROG (V1 V2)
      (SETQ V1
              (REDPAR (CAR (REDPAR1 (CADDDR U) (CAADR U)))
                      (CDR (REDPAR1 (CADDR U) (CADADR U)))))
      (SETQ V2
              (REDPAR (CDR (REDPAR1 (CADDDR U) (CAADR U)))
                      (CAR (REDPAR1 (CADDR U) (CADADR U)))))
      (RETURN
       (LIST (CAR U) (CONS (CADR V2) (CADR V1)) (CONS (CAR V1) (CAR V2)))))) 
(PUT 'ARGGF 'NUMBER-OF-ARGS 2) 
(PUT 'ARGGF 'DEFINED-ON-LINE '2003) 
(PUT 'ARGGF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'ARGGF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ARGGF (U1 U2)
    ((LAMBDA (P)
       (PROGN
        (PROG (PP)
          (SETQ PP
                  (LIST (EXPDEG (CAAR U2) (CONS (CDADAR U2) 1))
                        (EXPDEG (CONS (CDADAR U2) 1)
                                (CONS
                                 (DIFFERENCE
                                  (TIMES (CDADAR U2) (CADDR (CADR U2)))
                                  (TIMES (CDADAR U2) (CADDDR (CADR U2))))
                                 1))
                        (INVSQ (EXPDEG (CAAR U1) (CONS (CAADAR U2) 1)))
                        (EXPDEG (CONS (CAADAR U2) 1)
                                (CONS
                                 (DIFFERENCE
                                  (TIMES (CAADAR U2) (CADDDR (CADR U1)))
                                  (TIMES (CAADAR U2) (CADDR (CADR U1))))
                                 1))))
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP) (PROGN (SETQ P (MULTSQ PP P)))) (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))
        P))
     '(1 . 1))) 
(PUT 'INDGF 'NUMBER-OF-ARGS 2) 
(PUT 'INDGF 'DEFINED-ON-LINE '2014) 
(PUT 'INDGF 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'INDGF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDGF (U1 U2)
    (LIST (PLUS (TIMES (CDADAR U2) (CAADR U2)) (TIMES (CAADAR U2) (CADADR U1)))
          (PLUS (TIMES (CDADAR U2) (CADADR U2)) (TIMES (CAADAR U2) (CAADR U1)))
          (PLUS (TIMES (CDADAR U2) (CADDR (CADR U2)))
                (TIMES (CAADAR U2) (CADDDR (CADR U1))))
          (PLUS (TIMES (CDADAR U2) (CADDDR (CADR U2)))
                (TIMES (CAADAR U2) (CADDR (CADR U1)))))) 
(PUT 'DUBDEG 'NUMBER-OF-ARGS 2) 
(PUT 'DUBDEG 'DEFINED-ON-LINE '2023) 
(PUT 'DUBDEG 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DUBDEG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DUBDEG (X Y)
    (PROG (C B A1 A3)
      (COND ((OR (NUMBERP X) (NULL X)) (RETURN '(NIL . 1))))
      (COND ((NOT (NULL (CDR X))) (RETURN 'FAIL)))
     LB1
      (SETQ A1 (CAAR X))
      (SETQ A3 (CAR A1))
      (COND ((AND (ATOM A3) (EQUAL A3 Y)) (SETQ B (CONS (CDR A1) 1))))
      (COND
       ((NOT (ATOM A3))
        (COND
         ((EQUAL (CADR A3) Y)
          (COND ((NULL (CDDR A3)) (RETURN 'FAIL))
                ((NOT (NUMP (SIMP (CADDR A3)))) (RETURN (SIMP (CADDR A3))))
                (T
                 (SETQ C
                         (CONS (TIMES (CDR A1) (CADR (CADDR A3)))
                               (CADDR (CADDR A3))))))))))
      (COND
       ((ATOM (CDAR X))
        (COND ((NULL B) (COND ((NULL C) (RETURN '(NIL . 1))) (T (RETURN C))))
              ((NULL C) (RETURN B))
              (T
               (RETURN
                (CONS (PLUS (TIMES (CAR B) (CDR C)) (CAR C)) (CDR C)))))))
      (SETQ X (CDAR X))
      (GO LB1))) 
(PUT 'DEFINT_DELTA 'NUMBER-OF-ARGS 2) 
(PUT 'DEFINT_DELTA 'DEFINED-ON-LINE '2047) 
(PUT 'DEFINT_DELTA 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DEFINT_DELTA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEFINT_DELTA (U N)
    (COND ((NULL U) NIL)
          (T
           (APPEND
            (COND ((EQUAL N 1) (LIST (CAR U)))
                  (T (DELTA0 (MULTSQ (CAR U) (INVSQ (SIMP* N))) N N)))
            (DEFINT_DELTA (CDR U) N))))) 
(PUT 'DELTA0 'NUMBER-OF-ARGS 3) 
(PUT 'DELTA0 'DEFINED-ON-LINE '2057) 
(PUT 'DELTA0 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'DELTA0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DELTA0 (U N K)
    (COND ((EQUAL K 0) NIL)
          (T (CONS U (DELTA0 (ADDSQ U (INVSQ (SIMP* N))) N (DIFFERENCE K 1)))))) 
(PUT 'NUMP 'NUMBER-OF-ARGS 1) 
(PUT 'NUMP 'DEFINED-ON-LINE '2064) 
(PUT 'NUMP 'DEFINED-IN-FILE 'DEFINT/DEFINTA.RED) 
(PUT 'NUMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMP (X) (OR (NULL (CAR X)) (AND (NUMBERP (CAR X)) (NUMBERP (CDR X))))) 
(ENDMODULE) 