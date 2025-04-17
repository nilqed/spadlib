(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINTG)) 
(FLUID '(*PRECISE)) 
(PUT 'PRINT_CONDITIONS 'NUMBER-OF-ARGS 0) 
(PUT 'PRINT_CONDITIONS 'DEFINED-ON-LINE '30) 
(PUT 'PRINT_CONDITIONS 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'PRINT_CONDITIONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINT_CONDITIONS NIL
    (PROGN
     (COND ((NEQ SPEC_COND NIL) (MATHPRINT (CONS 'OR SPEC_COND)))
           (T (REDERR "Conditions not valid")))
     (SETQ SPEC_COND NIL)
     NIL)) 
(FLAG '(PRINT_CONDITIONS) 'OPFN) 
(PUT 'DEFINT_REFORM 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT_REFORM 'DEFINED-ON-LINE '39) 
(PUT 'DEFINT_REFORM 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'DEFINT_REFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINT_REFORM (N)
    (PROG (VAR VBLE CONST RESULT REFORM_TEST TEMP_RESULT REFORM_LST LST NEW_LST
           RES COEF NEW_COEF)
      (ON (LIST 'EXP))
      (SETQ COEF 1)
      (SETQ VAR (CADDAR N))
      (SETQ CONST (CADDR N))
      (SETQ VBLE (CADDDR N))
      (PROG (I)
        (SETQ I N)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (COND
             ((EQCAR I 'DEFINT_CHOOSE)
              (PROGN
               (COND
                ((AND (NEQ I '(DEFINT_CHOOSE E X)) (NUMBERP (CADR I))
                      (NEQ (CADR I) 0))
                 (PROGN
                  (SETQ NEW_COEF (CADR I))
                  (SETQ COEF (REVAL1 (AEVAL (LIST 'TIMES COEF NEW_COEF)) T))
                  (SETQ N (CONST_CASE N))))
                ((EQUAL I '(DEFINT_CHOOSE 0 X)) (SETQ COEF 0))
                ((EQUAL I '(DEFINT_CHOOSE E X))
                 (SETQ COEF (REVAL1 (AEVAL (LIST 'TIMES 'E COEF)) T)))
                ((EQCAR (CADR I) 'EXPT)
                 (PROGN
                  (SETQ REFORM_TEST 'T)
                  (SETQ REFORM_LST (APPEND REFORM_LST (LIST I)))))
                ((AND (EQCAR (CADR I) 'QUOTIENT)
                      (OR
                       (AND (LISTP (CADADR I))
                            (NEQ (CAR (CADADR I)) 'M_CHEBYSHEVT))
                       (NOT (LISTP (CADADR I)))))
                 (PROGN
                  (SETQ REFORM_TEST 'T)
                  (SETQ REFORM_LST (APPEND REFORM_LST (LIST I)))))
                ((EQCAR (CADR I) 'TIMES)
                 (PROGN
                  (COND
                   ((AND (LISTP (CAR (CDDADR I)))
                         (OR (EQUAL (CAAR (CDDADR I)) 'M_CHEBYSHEVU)
                             (EQUAL (CAAR (CDDADR I)) 'M_JACOBIP)
                             (EQUAL (CAR (CADADR I)) 'HEAVISIDE)))
                    (SETQ LST (APPEND LST (LIST I))))
                   ((AND (LISTP (CDR (CDDADR I))) (NEQ (CDR (CDDADR I)) 'NIL)
                         (LISTP (CADR (CDDADR I)))
                         (EQUAL (CAADR (CDDADR I)) 'M_GEGENBAUERP))
                    (SETQ LST (APPEND LST (LIST I))))
                   (T
                    (PROGN
                     (SETQ REFORM_TEST 'T)
                     (SETQ REFORM_LST (APPEND REFORM_LST (LIST I)))
                     NIL)))))
                (T (SETQ LST (APPEND LST (LIST I)))))
               NIL)))
            NIL))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (COND ((EQUAL REFORM_TEST NIL) (PROGN (SETQ N (CONS COEF N)) (RETURN N)))
            (T
             (PROGN
              (PROG (I)
                (SETQ I REFORM_LST)
               LAB
                (COND ((NULL I) (RETURN NIL)))
                ((LAMBDA (I)
                   (PROGN
                    (SETQ NEW_LST (CADR I))
                    (COND
                     ((AND (EQUAL (CAR NEW_LST) 'EXPT)
                           (EQUAL (CADR NEW_LST) 'E))
                      (SETQ RES (REFORM_EXPT NEW_LST VAR)))
                     ((EQUAL (CAR NEW_LST) 'TIMES)
                      (SETQ RES (REFORM_CONST NEW_LST VAR)))
                     ((AND (EQUAL (CAR NEW_LST) 'QUOTIENT)
                           (EQUAL (CADR NEW_LST) 1))
                      (SETQ RES (REFORM_DENOM NEW_LST VAR)))
                     ((EQUAL (CAR NEW_LST) 'QUOTIENT)
                      (SETQ RES (REFORM_QUOT NEW_LST VAR))))
                    (SETQ NEW_COEF (CAR RES))
                    (SETQ COEF (REVAL1 (AEVAL (LIST 'TIMES COEF NEW_COEF)) T))
                    (SETQ RES (CDR RES))
                    (SETQ TEMP_RESULT (APPEND TEMP_RESULT RES))
                    NIL))
                 (CAR I))
                (SETQ I (CDR I))
                (GO LAB))
              (SETQ TEMP_RESULT (CONS COEF TEMP_RESULT))
              (SETQ RESULT (APPEND TEMP_RESULT LST))
              (COND
               ((AND (EQUAL LST NIL) (EQUAL (LENGTH RESULT) 2))
                (SETQ RESULT (APPEND RESULT (LIST 0)))))
              (SETQ RESULT (APPEND RESULT (LIST CONST)))
              (SETQ RESULT (APPEND RESULT (LIST VBLE)))
              (RETURN RESULT)
              NIL))))) 
(PUT 'REFORM_EXPT 'NUMBER-OF-ARGS 2) 
(PUT 'REFORM_EXPT 'DEFINED-ON-LINE '165) 
(PUT 'REFORM_EXPT 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'REFORM_EXPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REFORM_EXPT (N VAR)
    (PROG (TEMP COEF LST)
      (COND
       ((NOT (LISTP N))
        (PROGN
         (SETQ LST (LIST (LIST 'DEFINT_CHOOSE N VAR)))
         (SETQ LST (CONS 1 LST))))
       ((NEQ (LISTP (CADDR N)) T)
        (PROGN
         (COND ((NUMBERP (CADDR N)) (SETQ COEF (CONS N COEF)))
               (T (SETQ LST (LIST (LIST 'DEFINT_CHOOSE N VAR)))))
         NIL))
       ((AND (EQ (CAR (CADDR N)) 'QUOTIENT) (EQCAR (CADR (CADDR N)) 'PLUS)
             (NOT (DEPENDS (CADDR (CADDR N)) VAR)))
        (SETQ N
                (CONS 'PLUS
                      (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                        (SETQ TERM (CDR (CADR (CADDR N))))
                        (COND ((NULL TERM) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (TERM)
                                            (LIST 'QUOTIENT TERM
                                                  (CADDR (CADDR N))))
                                          (CAR TERM))
                                         NIL)))
                       LOOPLABEL
                        (SETQ TERM (CDR TERM))
                        (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TERM)
                                    (LIST 'QUOTIENT TERM (CADDR (CADDR N))))
                                  (CAR TERM))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))))
       ((NOT (EQ (CAADDR N) 'PLUS))
        (SETQ LST (LIST (LIST 'DEFINT_CHOOSE N VAR))))
       (T
        (PROGN
         (SETQ TEMP (CDADDR N))
         (PROG (I)
           (SETQ I TEMP)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (COND
                ((NOT (DEPENDS I VAR))
                 (SETQ COEF (CONS (LIST 'EXPT 'E I) COEF)))
                (T
                 (SETQ LST
                         (CONS (LIST 'DEFINT_CHOOSE (LIST 'EXPT 'E I) VAR)
                               LST))))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         NIL)))
      (COND ((NULL COEF) (SETQ COEF 1))
            ((NULL (CDR COEF)) (SETQ COEF (CAR COEF)))
            (T (SETQ COEF (CONS 'TIMES (REVERSE COEF)))))
      (SETQ LST (CONS COEF LST))
      (RETURN LST))) 
(PUT 'REFORM_CONST 'NUMBER-OF-ARGS 2) 
(PUT 'REFORM_CONST 'DEFINED-ON-LINE '205) 
(PUT 'REFORM_CONST 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'REFORM_CONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REFORM_CONST (N VAR)
    (PROG (TEMP COEF LST TEMP1)
      (SETQ TEMP N)
      (SETQ COEF (CADDR TEMP))
      (SETQ TEMP (CADR TEMP))
      (COND
       ((AND (NEQ TEMP NIL) (EQUAL (CAR TEMP) 'EXPT)
             (OR (ATOM (CADDR TEMP)) (NEQ (CAADDR TEMP) 'PLUS)))
        (PROGN
         (SETQ LST
                 (LIST
                  (LIST 'DEFINT_CHOOSE (LIST 'EXPT 'E (CADDR TEMP)) VAR)))))
       (T
        (PROGN
         (SETQ TEMP1 (CDADDR TEMP))
         (PROG (I)
           (SETQ I TEMP1)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ LST
                       (CONS
                        (LIST 'DEFINT_CHOOSE (LIST 'EXPT 'E (CAR TEMP1)) VAR)
                        LST))
               (SETQ TEMP1 (CDR TEMP1))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         NIL)))
      (COND ((NEQ COEF NIL) (SETQ LST (CONS COEF LST)))
            (T (SETQ LST (CONS 1 LST))))
      (RETURN LST))) 
(PUT 'REFORM_DENOM 'NUMBER-OF-ARGS 2) 
(PUT 'REFORM_DENOM 'DEFINED-ON-LINE '230) 
(PUT 'REFORM_DENOM 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'REFORM_DENOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REFORM_DENOM (N VAR)
    (PROG (TEMP COEF LST TEMP1)
      (SETQ TEMP (CADDR N))
      (COND ((NOT (DEPENDS TEMP VAR)) (RETURN (CONS N NIL))))
      (COND
       ((NOT (OR (EQCAR TEMP 'EXPT) (EQCAR TEMP 'TIMES)))
        (RETURN (LIST 1 (LIST 'DEFINT_CHOOSE N VAR)))))
      (COND
       ((EQCAR TEMP 'TIMES)
        (PROGN
         (PROG (FCTR)
           (SETQ FCTR (CDR TEMP))
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR)
              (COND ((DEPENDS FCTR VAR) (SETQ TEMP1 (CONS FCTR TEMP1)))
                    (T (SETQ COEF (CONS FCTR COEF)))))
            (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         (SETQ TEMP
                 (COND ((CDR TEMP1) (CONS 'TIMES (REVERSE TEMP1)))
                       (T (CAR TEMP1)))))))
      (COND
       ((AND (EQCAR TEMP 'EXPT) (NOT (EQ (CADR TEMP) 'E)))
        (COND
         ((DEPENDS (CADR TEMP) VAR)
          (SETQ LST (LIST (LIST 'DEFINT_CHOOSE (LIST 'QUOTIENT 1 TEMP)) VAR)))
         (T
          (SETQ TEMP
                  (LIST 'EXPT 'E
                        (LIST 'TIMES (LIST 'LOG (CADR TEMP))
                              (CADDR TEMP))))))))
      (COND
       ((AND TEMP (PAIRP TEMP) (PAIRP (CDR TEMP)) (PAIRP (CDDR TEMP))
             (EQCAR (CADDR TEMP) 'QUOTIENT))
        (COND
         ((AND (EQCAR (CADR (CADDR TEMP)) 'PLUS)
               (NOT (DEPENDS (CADDR (CADDR TEMP)) VAR)))
          (SETQ TEMP
                  (LIST 'EXPT (CADR TEMP)
                        (CONS 'PLUS
                              (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TERM (CDR (CADR (CADDR TEMP))))
                                (COND ((NULL TERM) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (TERM)
                                                    (LIST 'QUOTIENT TERM
                                                          (CADDR
                                                           (CADDR TEMP))))
                                                  (CAR TERM))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ TERM (CDR TERM))
                                (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (TERM)
                                            (LIST 'QUOTIENT TERM
                                                  (CADDR (CADDR TEMP))))
                                          (CAR TERM))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))))
         ((AND (LISTP (CAR (CDADDR TEMP))) (LISTP (CADR (CDADDR TEMP))))
          (PROGN
           (OFF (LIST 'MCD))
           (SETQ TEMP (LIST 'EXPT 'E (QUOTIENT_CASE (REVAL1 TEMP T))))
           (ON (LIST 'MCD)))))))
      (COND
       ((AND (EQCAR TEMP 'EXPT) (NOT (EQCAR (CADDR TEMP) 'PLUS)))
        (PROGN
         (SETQ LST
                 (LIST
                  (LIST 'DEFINT_CHOOSE
                        (LIST 'QUOTIENT 1 (LIST 'EXPT 'E (CADDR TEMP)))
                        VAR)))))
       ((AND (EQCAR TEMP 'EXPT) (EQCAR (CADDR TEMP) 'PLUS))
        (PROGN
         (SETQ TEMP1 (CDADDR TEMP))
         (PROG (I)
           (SETQ I TEMP1)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (COND
               ((NOT (DEPENDS I VAR))
                (SETQ COEF (CONS (LIST 'EXPT 'E I) COEF)))
               (T
                (SETQ LST
                        (CONS
                         (LIST 'DEFINT_CHOOSE
                               (LIST 'QUOTIENT 1 (LIST 'EXPT 'E I)) VAR)
                         LST)))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB)))))
      (COND
       (COEF
        (COND
         ((NOT (NULL (CDR COEF))) (SETQ COEF (CONS 'TIMES (REVERSE COEF))))
         (T (SETQ COEF (CAR COEF))))))
     A
      (RETURN
       (COND (COEF (SETQ LST (CONS (LIST 'QUOTIENT 1 COEF) LST)))
             (T (SETQ LST (CONS 1 LST))))))) 
(PUT 'REFORM_QUOT 'NUMBER-OF-ARGS 2) 
(PUT 'REFORM_QUOT 'DEFINED-ON-LINE '279) 
(PUT 'REFORM_QUOT 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'REFORM_QUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REFORM_QUOT (N VAR)
    (PROG (NUM DENOM NUM_COEF DENOM_COEF LST NUM1 DENOM1)
      (SETQ NUM (CADR N))
      (SETQ DENOM (CADDR N))
      (COND
       ((NOT (DEPENDS NUM VAR)) (PROGN (SETQ NUM_COEF NUM) (SETQ NUM NIL)))
       ((AND (PAIRP NUM) (EQUAL (CAR NUM) 'TIMES))
        (PROGN
         (PROG (FCTR)
           (SETQ FCTR (CDR NUM))
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR)
              (COND ((DEPENDS FCTR VAR) (SETQ NUM1 (CONS FCTR NUM1)))
                    (T (SETQ NUM_COEF (CONS FCTR NUM_COEF)))))
            (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         (COND ((NULL NUM_COEF) (SETQ NUM_COEF 1))
               ((CDR NUM_COEF)
                (SETQ NUM_COEF (CONS 'TIMES (REVERSE NUM_COEF))))
               (T (SETQ NUM_COEF (CAR NUM_COEF))))
         (COND
          (NUM
           (SETQ NUM
                   (COND ((CDR NUM) (CONS 'TIMES (REVERSE NUM)))
                         (T (CAR NUM))))))))
       (T (SETQ NUM_COEF 1)))
      (COND
       ((NOT (DEPENDS DENOM VAR))
        (PROGN (SETQ DENOM_COEF DENOM) (SETQ DENOM NIL)))
       ((EQCAR DENOM 'TIMES)
        (PROGN
         (PROG (FCTR)
           (SETQ FCTR (CDR DENOM))
          LAB
           (COND ((NULL FCTR) (RETURN NIL)))
           ((LAMBDA (FCTR)
              (COND ((DEPENDS FCTR VAR) (SETQ DENOM1 (CONS FCTR DENOM1)))
                    (T (SETQ DENOM_COEF (CONS FCTR DENOM_COEF)))))
            (CAR FCTR))
           (SETQ FCTR (CDR FCTR))
           (GO LAB))
         (COND
          (DENOM_COEF
           (SETQ DENOM_COEF
                   (COND ((CDR DENOM_COEF) (CONS 'TIMES (REVERSE DENOM_COEF)))
                         (T (CAR DENOM_COEF))))))
         (COND
          (DENOM
           (SETQ DENOM
                   (COND ((CDR DENOM1) (CONS 'TIMES (REVERSE DENOM1)))
                         (T (CAR DENOM1)))))))))
      (COND
       ((AND (EQCAR DENOM 'EXPT)
             (OR (ATOM (CADDR DENOM)) (NEQ (CAADDR DENOM) 'PLUS)))
        (SETQ LST
                (LIST
                 (LIST 'DEFINT_CHOOSE
                       (LIST 'QUOTIENT 1 (LIST 'EXPT 'E (CADDR DENOM))) VAR))))
       (DENOM
        (PROGN
         (SETQ DENOM1 (CDADDR DENOM))
         (PROG (I)
           (SETQ I DENOM1)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (SETQ LST
                      (CONS
                       (LIST 'DEFINT_CHOOSE
                             (LIST 'QUOTIENT 1 (LIST 'EXPT 'E I)) VAR)
                       LST)))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB)))))
      (COND
       ((AND (EQCAR NUM 'EXPT) (NOT (EQCAR (CADDR NUM) 'PLUS)))
        (SETQ LST
                (CONS (LIST 'DEFINT_CHOOSE (LIST 'EXPT 'E (CADDR NUM)) VAR)
                      LST)))
       ((AND (EQCAR NUM 'EXPT) (EQCAR (CADDR NUM) 'PLUS))
        (PROGN
         (SETQ NUM1 (CDADDR NUM))
         (PROG (I)
           (SETQ I NUM1)
          LAB
           (COND ((NULL I) (RETURN NIL)))
           ((LAMBDA (I)
              (PROGN
               (SETQ LST
                       (CONS
                        (LIST 'DEFINT_CHOOSE (LIST 'EXPT 'E (CAR NUM1)) VAR)
                        LST))
               (SETQ NUM1 (CDR NUM1))))
            (CAR I))
           (SETQ I (CDR I))
           (GO LAB))
         NIL))
       (T (SETQ LST (CONS (LIST 'DEFINT_CHOOSE NUM VAR) LST))))
      (COND ((NULL DENOM_COEF) (SETQ LST (CONS NUM_COEF LST)))
            (T (SETQ LST (CONS (LIST 'QUOTIENT NUM_COEF DENOM_COEF) LST))))
      (RETURN LST))) 
(PUT 'CONST_CASE 'NUMBER-OF-ARGS 1) 
(PUT 'CONST_CASE 'DEFINED-ON-LINE '353) 
(PUT 'CONST_CASE 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'CONST_CASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONST_CASE (N)
    (PROG (NEW_N)
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH N) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (NOT (LISTP (CAR N)))
               (AND (LISTP (CAR N)) (NOT (NUMBERP (CADAR N)))))
           (SETQ NEW_N (APPEND NEW_N (LIST (CAR N))))))
         (SETQ N (CDR N)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ NEW_N (APPEND NEW_N (LIST 0)))
      (SETQ NEW_N (APPEND NEW_N N))
      (RETURN NEW_N))) 
(PUT 'QUOTIENT_CASE 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTIENT_CASE 'DEFINED-ON-LINE '365) 
(PUT 'QUOTIENT_CASE 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'QUOTIENT_CASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTIENT_CASE (N)
    (PROG (LST NEW_LST)
      (SETQ LST (CDADDR N))
      (SETQ NEW_LST (LIST (CAADDR N)))
      (PROG (I)
        (SETQ I LST)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (COND
             ((AND (PAIRP I) (PAIRP (CDR I)) (PAIRP (CDDR I))
                   (NUMBERP (CADDR I)) (LESSP (CADDR I) 0))
              (PROGN
               (SETCAR (CDDR I) (MINUS (CADDR I)))
               (SETQ I (LIST (CAR I) (CADR I) (LIST 'MINUS (CADDR I)))))))
            (SETQ NEW_LST (APPEND NEW_LST (LIST I)))
            NIL))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN NEW_LST))) 
(PUT 'TRANSF 'SIMPFN 'SIMPINTEG) 
(PUT 'NEW_INDEFINT 'NUMBER-OF-ARGS 1) 
(PUT 'NEW_INDEFINT 'DEFINED-ON-LINE '384) 
(PUT 'NEW_INDEFINT 'DEFINED-IN-FILE 'DEFINT/DEFINTG.RED) 
(PUT 'NEW_INDEFINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEW_INDEFINT (LST)
    (PROG (VAR Y N1 N2 RESULT *PRECISE)
      (COND
       ((EQCAR (CAR LST) 'TIMES)
        (RETURN (NEW_INDEFINT (APPEND (CDAR LST) (CDR LST))))))
      (SETQ RESULT 'UNKNOWN)
      (SETQ VAR (NTH LST (DIFFERENCE (LENGTH LST) 1)))
      (SETQ Y (NTH LST (LENGTH LST)))
      (SETQ LST (HYPERBOLIC_TEST LST))
      (COND
       ((EQUAL (LENGTH LST) 4)
        (PROGN
         (SETQ N1 (CAR LST))
         (SETQ N2 (CADR LST))
         (SETQ RESULT (REVAL1 (AEVAL (LIST 'INDEFINT2 N1 N2 VAR Y)) T))))
       ((EQUAL (LENGTH LST) 3)
        (PROGN
         (SETQ N1 (CAR LST))
         (SETQ RESULT (REVAL1 (AEVAL (LIST 'INDEFINT2 N1 VAR Y)) T)))))
      (RETURN RESULT))) 
(ENDMODULE) 