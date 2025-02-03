(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'JORDAN)) 
(LOAD-PACKAGE 'MATRIX) 
(LOAD-PACKAGE 'ARNUM) 
(PUT 'JORDAN 'NUMBER-OF-ARGS 1) 
(PUT 'JORDAN 'DEFINED-ON-LINE '56) 
(PUT 'JORDAN 'DEFINED-IN-FILE 'NORMFORM/JORDAN.RED) 
(PUT 'JORDAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE JORDAN (A)
    (PROG (AA L TMP P PINV ANS ANS_MAT FULL_COEFF_LIST RULE_LIST INPUT_MODE)
      (MATRIX_INPUT_TEST A)
      (COND
       ((NEQ (CAR (SIZE_OF_MATRIX A)) (CADR (SIZE_OF_MATRIX A)))
        (REDERR "ERROR: expecting a square matrix. ")))
      (SETQ INPUT_MODE (GET DMODE* 'DNAME))
      (COND
       ((EQUAL INPUT_MODE 'MODULAR)
        (REDERR
         "ERROR: jordan does not work with modular on. Try jordansymbolic. ")))
      (COND
       ((AND (NEQ INPUT_MODE 'ARNUM) (NEQ INPUT_MODE 'RATIONAL))
        (ON (LIST 'RATIONAL))))
      (ON (LIST 'COMBINEEXPT))
      (SETQ TMP (NEST_INPUT A))
      (SETQ AA (CAR TMP))
      (SETQ FULL_COEFF_LIST (CADR TMP))
      (SETQ L (JORDANSYMBOLICFORM AA FULL_COEFF_LIST))
      (SETQ TMP (JORDANFORM L FULL_COEFF_LIST))
      (SETQ ANS (CAR TMP))
      (SETQ P (CADR TMP))
      (SETQ PINV (CADDR TMP))
      (SETQ RULE_LIST
              (LIST 'REPLACEBY (LIST 'CO 2 (LIST '~ 'INT))
                    (LIST 'WHEN 'INT (LIST 'NUMBERP 'INT))))
      (PROG (ELT)
        (SETQ ELT FULL_COEFF_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ TMP (LIST 'REPLACEBY (LIST 'CO 2 (LIST '~ ELT)) ELT))
            (SETQ RULE_LIST (APPEND TMP RULE_LIST))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (LET (LIST RULE_LIST))
      (SETQ ANS_MAT (DE_NEST_MAT ANS))
      (SETQ P (DE_NEST_MAT P))
      (SETQ PINV (DE_NEST_MAT PINV))
      (CLEARRULES (LIST RULE_LIST))
      (COND
       ((AND (NEQ INPUT_MODE 'ARNUM) (NEQ INPUT_MODE 'RATIONAL))
        (PROGN
         (COND ((EQUAL INPUT_MODE 'NIL) (OFF (LIST 'RATIONAL)))
               (T (ONOFF INPUT_MODE T)))
         NIL)))
      (OFF (LIST 'COMBINEEXPT))
      (RETURN (LIST 'LIST ANS_MAT P PINV)))) 
(FLAG '(JORDAN) 'OPFN) 
(PUT 'JORDANFORM 'NUMBER-OF-ARGS 2) 
(PUT 'JORDANFORM 'DEFINED-ON-LINE '121) 
(PUT 'JORDANFORM 'DEFINED-IN-FILE 'NORMFORM/JORDAN.RED) 
(PUT 'JORDANFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JORDANFORM (L FULL_COEFF_LIST)
    (PROG (JJ Z ZEROES P PINV X TMP TMP1 DE_NEST N D)
      (SETQ N 0)
      (SETQ D 0)
      (SETQ P (NTH L 3))
      (SETQ PINV (NTH L 4))
      (SETQ N (LENGTH (NTH (NTH L 2) 1)))
      (SETQ X (NTH (NTH L 2) 2))
      (SETQ JJ (NTH L 1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ D (DEG (NTH (NTH (NTH L 2) 1) I) X))
         (COND
          ((GREATERP D 1)
           (PROGN
            (SETQ Z (NTH (NTH (NTH L 2) 1) I))
            (SETQ ZEROES (LIST))
            (SETQ DE_NEST (DE_NEST_LIST Z FULL_COEFF_LIST))
            (SETQ TMP (AEVAL* (LIST 'SOLVE DE_NEST X)))
            (SETQ TMP (CDR TMP))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (LENGTH TMP) J)) (RETURN NIL)))
              (PROGN
               (COND
                ((TEST_FOR_ROOT_OF (NTH TMP J))
                 (PROGN
                  (COND
                   ((EQUAL (LENGTH (GET_COEFFS DE_NEST)) 1)
                    (PROGN
                     (ON (LIST 'COMPLEX))
                     (SETQ TMP1 (AEVAL* (LIST 'ROOTS Z)))
                     (OFF (LIST 'COMPLEX))
                     NIL))
                   (T
                    (PROGN
                     (ON (LIST 'FULLROOTS))
                     (PRIN2T "***** WARNING: fullroots turned on.")
                     (PRIN2T "               May take a while.")
                     (SETQ TMP1 (AEVAL* (LIST 'SOLVE DE_NEST X)))
                     (OFF (LIST 'FULLROOTS))
                     (SETQ TMP1 (RE_NEST_LIST TMP1 FULL_COEFF_LIST))
                     NIL)))
                  (SETQ ZEROES (APPEND ZEROES TMP1))
                  (SETQ ZEROES (CDR ZEROES))
                  NIL))
                (T
                 (PROGN
                  (SETQ TMP1 (AEVAL* (LIST 'SOLVE Z X)))
                  (SETQ TMP1 (CDR TMP1))
                  (SETQ ZEROES (APPEND ZEROES (LIST (NTH TMP1 J))))
                  NIL)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (LENGTH ZEROES) J)) (RETURN NIL)))
              (PROGN
               (SETQ TMP (NTH ZEROES J))
               (SETQ TMP (CADDR TMP))
               (SETQ JJ
                       (AEVAL*
                        (LIST 'SUB
                              (LIST 'EQUAL
                                    (LIST 'MKID X
                                          (LIST 'OFF_MOD_REVAL
                                                (LIST 'PLUS (LIST 'TIMES 10 I)
                                                      J)))
                                    TMP)
                              JJ)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (LENGTH ZEROES) J)) (RETURN NIL)))
              (PROGN
               (SETQ TMP (NTH ZEROES J))
               (SETQ TMP (CADDR TMP))
               (SETQ P
                       (AEVAL*
                        (LIST 'SUB
                              (LIST 'EQUAL
                                    (LIST 'MKID X
                                          (LIST 'OFF_MOD_REVAL
                                                (LIST 'PLUS (LIST 'TIMES 10 I)
                                                      J)))
                                    TMP)
                              P)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (LENGTH ZEROES) J)) (RETURN NIL)))
              (PROGN
               (SETQ TMP (NTH ZEROES J))
               (SETQ TMP (CADDR TMP))
               (SETQ PINV
                       (AEVAL*
                        (LIST 'SUB
                              (LIST 'EQUAL
                                    (LIST 'MKID X
                                          (LIST 'OFF_MOD_REVAL
                                                (LIST 'PLUS (LIST 'TIMES 10 I)
                                                      J)))
                                    TMP)
                              PINV)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST JJ P PINV)))) 
(PUT 'TEST_FOR_ROOT_OF 'NUMBER-OF-ARGS 1) 
(PUT 'TEST_FOR_ROOT_OF 'DEFINED-ON-LINE '213) 
(PUT 'TEST_FOR_ROOT_OF 'DEFINED-IN-FILE 'NORMFORM/JORDAN.RED) 
(PUT 'TEST_FOR_ROOT_OF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST_FOR_ROOT_OF (INPUT)
    (PROG (TMP COPY_INPUT BOOLEAN TMP1)
      (SETQ BOOLEAN NIL)
      (SETQ COPY_INPUT INPUT)
      (COND ((ATOM COPY_INPUT) (PROGN NIL))
            ((EQUAL (CAR COPY_INPUT) 'ROOT_OF) (SETQ BOOLEAN T))
            (T
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND COPY_INPUT (EQUAL BOOLEAN NIL))) (RETURN NIL)))
               (PROGN
                (SETQ TMP COPY_INPUT)
                (SETQ TMP (CAR COPY_INPUT))
                (COND ((EQUAL TMP 'ROOT_OF) (SETQ BOOLEAN T))
                      (T
                       (PROG ()
                        WHILELABEL
                         (COND
                          ((NOT (AND (PAIRP TMP) (EQUAL BOOLEAN NIL)))
                           (RETURN NIL)))
                         (PROGN
                          (SETQ TMP1 TMP)
                          (COND ((EQUAL (CAR TMP1) 'ROOT_OF) (SETQ BOOLEAN T))
                                ((ATOM TMP1) (PROGN NIL))
                                (T
                                 (PROG ()
                                  WHILELABEL
                                   (COND
                                    ((NOT
                                      (AND (PAIRP TMP1) (EQUAL BOOLEAN NIL)))
                                     (RETURN NIL)))
                                   (PROGN
                                    (COND
                                     ((EQUAL (CAR TMP1) 'ROOT_OF)
                                      (SETQ BOOLEAN T))
                                     (T (SETQ TMP1 (CAR TMP1))))
                                    NIL)
                                   (GO WHILELABEL))))
                          (SETQ TMP (CDR TMP))
                          NIL)
                         (GO WHILELABEL))))
                (SETQ COPY_INPUT (CDR COPY_INPUT))
                NIL)
               (GO WHILELABEL))))
      (RETURN BOOLEAN))) 
(FLAG '(TEST_FOR_ROOT_OF) 'BOOLEAN) 
(ENDMODULE) 