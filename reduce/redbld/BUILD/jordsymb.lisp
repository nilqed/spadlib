(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'JORDSYMB)) 
(SWITCH (LIST 'LOOKING_GOOD)) 
(PUT 'JORDANSYMBOLIC 'NUMBER-OF-ARGS 1) 
(PUT 'JORDANSYMBOLIC 'DEFINED-ON-LINE '121) 
(PUT 'JORDANSYMBOLIC 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'JORDANSYMBOLIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE JORDANSYMBOLIC (A)
    (PROG (AA P PINV TMP ANS_MAT ANS_LIST FULL_COEFF_LIST RULE_LIST OUTPUT
           INPUT_MODE BALANCED_MOD COMBINEEXPT)
      (SETQ BALANCED_MOD *BALANCED_MOD)
      (SETQ COMBINEEXPT *COMBINEEXPT)
      (MATRIX_INPUT_TEST A)
      (COND
       ((NEQ (CAR (SIZE_OF_MATRIX A)) (CADR (SIZE_OF_MATRIX A)))
        (REDERR "ERROR: expecting a square matrix. ")))
      (COND (*BALANCED_MOD (OFF1 'BALANCED_MOD)))
      (SETQ INPUT_MODE (AND DMODE* (GET DMODE* 'DNAME)))
      (COND
       ((NOT (MEMQ INPUT_MODE '(MODULAR ARNUM RATIONAL))) (ON1 'RATIONAL)))
      (ON1 'COMBINEEXPT)
      (SETQ TMP (NEST_INPUT A))
      (SETQ AA (CAR TMP))
      (SETQ FULL_COEFF_LIST (CADR TMP))
      (SETQ TMP (JORDANSYMBOLICFORM AA FULL_COEFF_LIST))
      (SETQ ANS_MAT (CAR TMP))
      (SETQ ANS_LIST (CADR TMP))
      (SETQ P (CADDR TMP))
      (SETQ PINV (CADDR (REST TMP)))
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
      (SETQ ANS_MAT (DE_NEST_MAT ANS_MAT))
      (SETCAR ANS_LIST (APPEND (LIST 'LIST) (CAR ANS_LIST)))
      (SETQ ANS_LIST (APPEND (LIST 'LIST) ANS_LIST))
      (SETCAR (CDR ANS_LIST)
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT (CADR ANS_LIST))
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT))
                                      NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ P (DE_NEST_MAT P))
      (SETQ PINV (DE_NEST_MAT PINV))
      (CLEARRULES (LIST RULE_LIST))
      (COND ((NULL INPUT_MODE) (OFF1 'RATIONAL))
            ((NOT (MEMQ INPUT_MODE '(MODULAR ARNUM RATIONAL)))
             (ON1 INPUT_MODE)))
      (COND (BALANCED_MOD (ON1 'BALANCED_MOD)))
      (COND ((NULL COMBINEEXPT) (OFF1 'COMBINEEXPT)))
      (SETQ OUTPUT (LIST 'LIST ANS_MAT ANS_LIST P PINV))
      (COND (*LOOKING_GOOD (SETQ OUTPUT (LOOKING_GOOD OUTPUT))))
      (RETURN OUTPUT))) 
(FLAG '(JORDANSYMBOLIC) 'OPFN) 
(PUT 'JORDANSYMBOLICFORM 'NUMBER-OF-ARGS 2) 
(PUT 'JORDANSYMBOLICFORM 'DEFINED-ON-LINE '190) 
(PUT 'JORDANSYMBOLICFORM 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'JORDANSYMBOLICFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JORDANSYMBOLICFORM (A FULL_COEFF_LIST)
    (PROG (L R TT TINV S SINV TMP P PINV INVARIANT)
      (SETQ TMP (RATJORDANFORM A FULL_COEFF_LIST))
      (SETQ R (CAR TMP))
      (SETQ TT (CADR TMP))
      (SETQ TINV (CADDR TMP))
      (SETQ TMP (RATJORDAN_TO_JORDAN R))
      (SETQ L (CAR TMP))
      (SETQ S (CADR TMP))
      (SETQ SINV (CADDR TMP))
      (SETQ P (OFF_MOD_REVAL (LIST 'TIMES TT S)))
      (SETQ PINV (OFF_MOD_REVAL (LIST 'TIMES SINV TINV)))
      (SETQ INVARIANT (INVARIANT_TO_JORDAN (NTH L 1)))
      (RETURN (LIST INVARIANT (NTH L 2) P PINV)))) 
(PUT 'FIND_COMPANION 'NUMBER-OF-ARGS 3) 
(PUT 'FIND_COMPANION 'DEFINED-ON-LINE '215) 
(PUT 'FIND_COMPANION 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'FIND_COMPANION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND_COMPANION (R RR X)
    (PROG (P ROW_DIM K)
      (SETQ ROW_DIM 0)
      (SETQ K 0)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX R)))
      (SETQ K (PLUS RR 1))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LEQ K ROW_DIM) (EQUAL (GETMAT R K (DIFFERENCE K 1)) 1)))
          (RETURN NIL)))
        (SETQ K (PLUS K 1))
        (GO WHILELABEL))
      (SETQ P 0)
      (PROG (J)
        (SETQ J RR)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE K 1) J)) (RETURN NIL)))
        (PROGN
         (SETQ P
                 (LIST 'PLUS P
                       (LIST 'TIMES (LIST 'MINUS (GETMAT R J (DIFFERENCE K 1)))
                             (LIST 'EXPT X (DIFFERENCE J RR)))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ P (LIST 'PLUS P (LIST 'EXPT X (DIFFERENCE K RR))))
      (RETURN P))) 
(PUT 'FIND_RATJBLOCK 'NUMBER-OF-ARGS 3) 
(PUT 'FIND_RATJBLOCK 'DEFINED-ON-LINE '238) 
(PUT 'FIND_RATJBLOCK 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'FIND_RATJBLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND_RATJBLOCK (R RR X)
    (PROG (P CONTINUE K E ROW_DIM)
      (SETQ K 0)
      (SETQ E 0)
      (SETQ ROW_DIM 0)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX R)))
      (SETQ P (REVAL1 (FIND_COMPANION R RR X) T))
      (SETQ E 1)
      (SETQ K (OFF_MOD_REVAL (LIST 'PLUS RR (DEG P X))))
      (SETQ CONTINUE T)
      (PROG ()
       WHILELABEL
        (COND ((NOT CONTINUE) (RETURN NIL)))
        (PROGN
         (COND ((GREATERP K ROW_DIM) (SETQ CONTINUE NIL)))
         (COND
          ((IDENTITYMATRIX R (DIFFERENCE K (DEG P X)) K (DEG P X))
           (PROGN (SETQ E (PLUS E 1)) (SETQ K (PLUS K (DEG P X))) NIL))
          (T (PROGN (SETQ CONTINUE NIL) NIL)))
         NIL)
        (GO WHILELABEL))
      (RETURN (LIST P E)))) 
(PUT 'IDENTITYMATRIX 'NUMBER-OF-ARGS 4) 
(PUT 'IDENTITYMATRIX 'DEFINED-ON-LINE '269) 
(PUT 'IDENTITYMATRIX 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'IDENTITYMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE IDENTITYMATRIX (A I J M)
    (PROG (ROW_DIM)
      (SETQ ROW_DIM 0)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX A)))
      (COND
       ((OR (GREATERP (PLUS I (DIFFERENCE M 1)) ROW_DIM)
            (GREATERP (PLUS J (DIFFERENCE M 1)) ROW_DIM))
        (RETURN NIL))
       (T
        (PROGN
         (COND
          ((EQUAL
            (SUBMATRIX A (LIST I (PLUS I (DIFFERENCE M 1)))
                       (LIST J (PLUS J (DIFFERENCE M 1))))
            (MAKE_IDENTITY M M))
           (RETURN T))
          (T (RETURN NIL)))
         NIL))))) 
(FLAG '(IDENTITYMATRIX) 'BOOLEAN) 
(PUT 'INVARIANT_TO_JORDAN 'NUMBER-OF-ARGS 1) 
(PUT 'INVARIANT_TO_JORDAN 'DEFINED-ON-LINE '291) 
(PUT 'INVARIANT_TO_JORDAN 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'INVARIANT_TO_JORDAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVARIANT_TO_JORDAN (INVARIANT)
    (PROG (BLOCK_LIST N M)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ N (LENGTH INVARIANT))
      (SETQ BLOCK_LIST (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ M (LENGTH (NTH (NTH INVARIANT I) 2)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
           (PROGN
            (SETQ BLOCK_LIST
                    (APPEND BLOCK_LIST
                            (LIST
                             (JORDANBLOCK (NTH (NTH INVARIANT I) 1)
                                          (NTH (NTH (NTH INVARIANT I) 2) J)))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (REVAL1 (LIST 'DIAGI BLOCK_LIST) T)))) 
(PUT 'JORDANBLOCK 'NUMBER-OF-ARGS 2) 
(PUT 'JORDANBLOCK 'DEFINED-ON-LINE '315) 
(PUT 'JORDANBLOCK 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'JORDANBLOCK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JORDANBLOCK (CONST MAT_DIM)
    (PROG (JB)
      (SETQ JB (MKMATRIX MAT_DIM MAT_DIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE MAT_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE MAT_DIM J)) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL I J)
              (PROGN
               (SETMAT JB I J CONST)
               (COND ((LESSP I MAT_DIM) (SETMAT JB I (PLUS J 1) 1)))
               NIL)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN JB))) 
(FLAG '(JORDANBLOCK) 'OPFN) 
(SWITCH (LIST 'MOD_WAS_ON)) 
(PUT 'RATJORDAN_TO_JORDAN 'NUMBER-OF-ARGS 1) 
(PUT 'RATJORDAN_TO_JORDAN 'DEFINED-ON-LINE '347) 
(PUT 'RATJORDAN_TO_JORDAN 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'RATJORDAN_TO_JORDAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATJORDAN_TO_JORDAN (R)
    (PROG (PRIM_INV TT TINV TINVLIST TLIST EXP_LIST INVARIANT P PARTT PARTTINV
           S1 T1 V W SUM1 TMP S SINV X NN N D)
      (SETQ NN 0)
      (SETQ N 0)
      (SETQ D 0)
      (COND (*LOOKING_GOOD (SETQ X 'XI)) (T (SETQ X 'LAMBDA)))
      (SETQ PRIM_INV (RATJORDAN_TO_PRIMINV R X))
      (SETQ INVARIANT (LIST))
      (SETQ TLIST (LIST))
      (SETQ TINVLIST (LIST))
      (SETQ NN (LENGTH PRIM_INV))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN I)) (RETURN NIL)))
        (PROGN
         (SETQ P (NTH (NTH PRIM_INV I) 1))
         (SETQ EXP_LIST (NTH (NTH PRIM_INV I) 2))
         (SETQ D (OFF_MOD_REVAL (DEG P X)))
         (COND
          ((EQUAL D 1)
           (PROGN
            (SETQ INVARIANT
                    (APPEND INVARIANT
                            (LIST
                             (LIST (REVAL1 (LIST 'MINUS (COEFFN P X 0)) T)
                                   EXP_LIST))))
            NIL))
          (T
           (PROGN
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
              (PROGN
               (SETQ INVARIANT
                       (APPEND INVARIANT
                               (LIST
                                (LIST
                                 (MKID X
                                       (OFF_MOD_REVAL
                                        (LIST 'PLUS (LIST 'TIMES 10 I) J)))
                                 EXP_LIST))))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL)))
         (SETQ V (MKVECT D))
         (PUTV V D 1)
         (PROG (J)
           (SETQ J (DIFFERENCE D 1))
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
           (PROGN
            (SETQ TMP 0)
            (SETQ SUM1 0)
            (PROG (K)
              (SETQ K J)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE D 1) K)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (REVAL1
                        (LIST 'TIMES (COEFFN P X K)
                              (LIST 'EXPT X (DIFFERENCE K J)))
                        T))
               (SETQ SUM1 (REVAL1 (LIST 'PLUS SUM1 TMP) T))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PUTV V J
                  (REVAL1 (LIST 'PLUS SUM1 (LIST 'EXPT X (DIFFERENCE D J))) T))
            NIL)
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         (SETQ SUM1 0)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (LENGTH EXP_LIST) J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (REVAL1 (NTH EXP_LIST J) T))
            (SETQ SUM1 (REVAL1 (LIST 'PLUS SUM1 TMP) T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ N SUM1)
         (SETQ PARTT (MKMATRIX (TIMES N D) N))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
              (PROGN
               (SETMAT PARTT (PLUS (TIMES (DIFFERENCE J 1) D) K) J (GETV V K))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ TT (MKMATRIX (TIMES N D) (TIMES N D)))
         (COND
          ((EQUAL D 1)
           (PROGN
            (COND
             (*MODULAR
              (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
            (SETQ TT
                    (COPYINTO
                     (AEVAL*
                      (LIST 'SUB
                            (LIST 'LIST
                                  (LIST 'EQUAL X
                                        (LIST 'MINUS (LIST 'COEFFN P X 0))))
                            PARTT))
                     TT 1 1))
            (COND
             (*MOD_WAS_ON
              (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
            NIL))
          (T
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
             (PROGN
              (COND
               (*MODULAR
                (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
              (SETQ TT
                      (COPYINTO
                       (AEVAL*
                        (LIST 'SUB
                              (LIST 'EQUAL X
                                    (LIST 'MKID X
                                          (LIST 'OFF_MOD_REVAL
                                                (LIST 'PLUS (LIST 'TIMES 10 I)
                                                      J))))
                              PARTT))
                       TT 1 (PLUS (TIMES (DIFFERENCE J 1) N) 1)))
              (COND
               (*MOD_WAS_ON
                (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
              NIL)
             (SETQ J (PLUS2 J 1))
             (GO LAB))))
         (SETQ TLIST (APPEND TLIST (LIST TT)))
         (SETQ TMP (AEVAL* (LIST 'DF P X)))
         (SETQ TMP (CALC_EXGCD P TMP X))
         (SETQ S1 (CADR TMP))
         (SETQ T1 (CADDR TMP))
         (SETQ W (MKVECT D))
         (PUTV W 1 T1)
         (PROG (J)
           (SETQ J 2)
          LAB
           (COND ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
           (PROGN
            (PUTV W J
                  (GET_REM (REVAL1 (LIST 'TIMES X (GETV W (DIFFERENCE J 1))) T)
                   P))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ PARTTINV (MKMATRIX N (TIMES N D)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
              (PROGN
               (SETMAT PARTTINV J (PLUS (TIMES (DIFFERENCE J 1) D) K)
                (GETV W K))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ TINV (MKMATRIX (TIMES N D) (TIMES N D)))
         (COND
          ((EQUAL D 1)
           (PROGN
            (COND
             (*MODULAR
              (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
            (SETQ TINV
                    (REVAL1
                     (COPYINTO
                      (AEVAL*
                       (LIST 'SUB
                             (LIST 'EQUAL X (LIST 'MINUS (LIST 'COEFFN P X 0)))
                             PARTTINV))
                      TINV 1 1)
                     T))
            (COND
             (*MOD_WAS_ON
              (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
            NIL))
          (T
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
             (PROGN
              (COND
               (*MODULAR
                (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
              (SETQ TINV
                      (REVAL1
                       (COPYINTO
                        (AEVAL*
                         (LIST 'SUB
                               (LIST 'EQUAL X
                                     (LIST 'MKID X
                                           (LIST 'OFF_MOD_REVAL
                                                 (LIST 'PLUS (LIST 'TIMES 10 I)
                                                       J))))
                               PARTTINV))
                        TINV (PLUS (TIMES (DIFFERENCE J 1) N) 1) 1)
                       T))
              (COND
               (*MOD_WAS_ON
                (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
              NIL)
             (SETQ J (PLUS2 J 1))
             (GO LAB))))
         (SETQ TINVLIST (APPEND TINVLIST (LIST TINV)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ S (REVAL1 (LIST 'DIAGI TLIST) T))
      (SETQ SINV (REVAL1 (LIST 'DIAGI TINVLIST) T))
      (SETQ TMP
              (LIST
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I 1)
                 (COND ((MINUSP (DIFFERENCE NN I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS (NTH (NTH PRIM_INV I) 1) NIL)))
                LOOPLABEL
                 (SETQ I (PLUS2 I 1))
                 (COND ((MINUSP (DIFFERENCE NN I)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (NTH (NTH PRIM_INV I) 1) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ TMP (APPEND TMP (LIST X)))
      (SETQ TMP (APPEND (LIST INVARIANT) (LIST TMP)))
      (RETURN (LIST TMP S SINV)))) 
(PUT 'RATJORDAN_TO_PRIMINV 'NUMBER-OF-ARGS 2) 
(PUT 'RATJORDAN_TO_PRIMINV 'DEFINED-ON-LINE '505) 
(PUT 'RATJORDAN_TO_PRIMINV 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'RATJORDAN_TO_PRIMINV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATJORDAN_TO_PRIMINV (R X)
    (PROG (P PLIST EXP_LIST L PRIM_INV N RR II NN)
      (SETQ N 0)
      (SETQ RR 0)
      (SETQ II 0)
      (SETQ NN 0)
      (SETQ N (CAR (SIZE_OF_MATRIX R)))
      (SETQ RR 1)
      (SETQ PLIST (LIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ RR N)) (RETURN NIL)))
        (PROGN
         (SETQ L (FIND_RATJBLOCK R RR X))
         (SETQ PLIST (APPEND PLIST (LIST L)))
         (SETQ RR
                 (OFF_MOD_REVAL
                  (LIST 'PLUS RR (LIST 'TIMES (NTH L 2) (DEG (NTH L 1) X)))))
         NIL)
        (GO WHILELABEL))
      (SETQ P (REVAL1 (NTH (NTH PLIST 1) 1) T))
      (SETQ EXP_LIST (LIST (NTH (NTH PLIST 1) 2)))
      (SETQ PRIM_INV (LIST))
      (SETQ NN (LENGTH PLIST))
      (SETQ II 2)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ II NN)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (REVAL1 (NTH (NTH PLIST II) 1) T) P)
           (PROGN
            (SETQ EXP_LIST (APPEND EXP_LIST (LIST (NTH (NTH PLIST II) 2))))))
          (T
           (PROGN
            (SETQ PRIM_INV (APPEND PRIM_INV (LIST (LIST P EXP_LIST))))
            (SETQ P (REVAL1 (NTH (NTH PLIST II) 1) T))
            (SETQ EXP_LIST (LIST (NTH (NTH PLIST II) 2)))
            NIL)))
         (SETQ II (PLUS II 1))
         NIL)
        (GO WHILELABEL))
      (SETQ PRIM_INV (APPEND PRIM_INV (LIST (LIST P EXP_LIST))))
      (RETURN PRIM_INV))) 
(PUT 'SUBMATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'SUBMATRIX 'DEFINED-ON-LINE '552) 
(PUT 'SUBMATRIX 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'SUBMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBMATRIX (A ROW_LIST COL_LIST)
    (PROG (AA ROW_DIM COL_DIM CAR_ROW LAST_ROW CAR_COL LAST_COL A_ROW_DIM
           A_COL_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COL_DIM 0)
      (SETQ CAR_ROW 0)
      (SETQ LAST_ROW 0)
      (SETQ CAR_COL 0)
      (SETQ LAST_COL 0)
      (SETQ A_ROW_DIM 0)
      (SETQ A_COL_DIM 0)
      (MATRIX_INPUT_TEST A)
      (COND ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST))))
      (COND ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST))))
      (SETQ CAR_ROW (CAR ROW_LIST))
      (SETQ LAST_ROW (CADR ROW_LIST))
      (SETQ ROW_DIM (PLUS (DIFFERENCE LAST_ROW CAR_ROW) 1))
      (SETQ CAR_COL (CAR COL_LIST))
      (SETQ LAST_COL (CADR COL_LIST))
      (SETQ COL_DIM (PLUS (DIFFERENCE LAST_COL CAR_COL) 1))
      (SETQ A_ROW_DIM (CAR (SIZE_OF_MATRIX A)))
      (SETQ A_COL_DIM (CADR (SIZE_OF_MATRIX A)))
      (COND
       ((OR (EQUAL CAR_ROW 0) (EQUAL LAST_ROW 0))
        (REDERR
         (LIST "0 is out of range for " A ". The car row is labelled 1."))))
      (COND
       ((OR (EQUAL CAR_COL 0) (EQUAL LAST_COL 0))
        (REDERR
         (LIST "0 is out of range for" A ". The car column is labelled 1."))))
      (COND
       ((GREATERP CAR_ROW A_ROW_DIM)
        (REDERR (LIST A "doesn't have" CAR_ROW "rows."))))
      (COND
       ((GREATERP LAST_ROW A_ROW_DIM)
        (REDERR (LIST A "doesn't have" LAST_ROW "rows."))))
      (COND
       ((GREATERP CAR_COL A_COL_DIM)
        (REDERR (LIST A "doesn't have" CAR_COL "columns."))))
      (COND
       ((GREATERP LAST_COL A_COL_DIM)
        (REDERR (LIST A "doesn't have" LAST_COL "columns."))))
      (SETQ AA (MKMATRIX ROW_DIM COL_DIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COL_DIM J)) (RETURN NIL)))
           (PROGN
            (SETMAT AA I J
             (GETMAT A (PLUS I (DIFFERENCE CAR_ROW 1))
              (PLUS J (DIFFERENCE CAR_COL 1))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN AA))) 
(FLAG '(SUBMATRIX) 'OPFN) 
(PUT 'LOOKING_GOOD 'NUMBER-OF-ARGS 1) 
(PUT 'LOOKING_GOOD 'DEFINED-ON-LINE '609) 
(PUT 'LOOKING_GOOD 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'LOOKING_GOOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOOKING_GOOD (OUTPUT)
    (PROG (RULE_LIST)
      (AEVAL (OPERATOR (LIST 'XI)))
      (AEVAL (PRINT_INDEXED (LIST 'XI)))
      (SETQ RULE_LIST (MAKE_RULE))
      (LET (LIST RULE_LIST))
      (SETQ OUTPUT (REVAL1 OUTPUT T))
      (CLEARRULES (LIST RULE_LIST))
      (RETURN OUTPUT))) 
(PUT 'MAKE_RULE 'NUMBER-OF-ARGS 0) 
(PUT 'MAKE_RULE 'DEFINED-ON-LINE '643) 
(PUT 'MAKE_RULE 'DEFINED-IN-FILE 'NORMFORM/JORDSYMB.RED) 
(PUT 'MAKE_RULE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MAKE_RULE NIL
    (PROG (RULE_LIST TMP TMP1)
      (SETQ RULE_LIST (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 9 I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE 9 J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP1 (REVAL1 (MKID 'XI I) T))
            (SETQ TMP1 (REVAL1 (MKID TMP1 J) T))
            (SETQ TMP (LIST 'REPLACEBY TMP1 (LIST 'XI I J)))
            (SETQ RULE_LIST (APPEND RULE_LIST (LIST TMP)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ RULE_LIST (APPEND (LIST 'LIST) RULE_LIST))
      (RETURN RULE_LIST))) 
(ENDMODULE) 