(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SMITHEX)) 
(PUT 'SMITHEX 'NUMBER-OF-ARGS 2) 
(PUT 'SMITHEX 'DEFINED-ON-LINE '36) 
(PUT 'SMITHEX 'DEFINED-IN-FILE 'NORMFORM/SMITHEX.RED) 
(PUT 'SMITHEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SMITHEX (MAT1 X)
    (PROG (A LEFT RIGHT TMP ISCLEAR G L R1 POLY1 POLY2 QUO1 QUO2 R LC TQUO Q
           FULL_COEFF_LIST RULE_LIST INPUT_MODE I J N M)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ N 0)
      (SETQ M 0)
      (MATRIX_INPUT_TEST MAT1)
      (SETQ INPUT_MODE (GET DMODE* 'DNAME))
      (COND
       ((EQUAL INPUT_MODE 'MODULAR)
        (REDERR "ERROR: smithex does not work with modular on.")))
      (ALL_INTEGER_ENTRIES_TEST MAT1)
      (COND
       ((AND (NEQ INPUT_MODE 'ARNUM) (NEQ INPUT_MODE 'RATIONAL))
        (ON (LIST 'RATIONAL))))
      (ON (LIST 'COMBINEEXPT))
      (SETQ TMP (NEST_INPUT_SMITH MAT1 X))
      (SETQ A (CAR TMP))
      (SETQ FULL_COEFF_LIST (CADR TMP))
      (SETQ N (CAR (SIZE_OF_MATRIX A)))
      (SETQ M (CADR (SIZE_OF_MATRIX A)))
      (SETQ LEFT (MAKE_IDENTITY N N))
      (SETQ RIGHT (MAKE_IDENTITY M M))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (MIN N M) K)) (RETURN NIL)))
        (PROGN
         (SETQ I K)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (LEQ I N) (EQUAL (GETMAT A I K) 0))) (RETURN NIL)))
           (SETQ I (PLUS I 1))
           (GO WHILELABEL))
         (SETQ J K)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (LEQ J M) (EQUAL (GETMAT A K J) 0))) (RETURN NIL)))
           (SETQ J (PLUS J 1))
           (GO WHILELABEL))
         (COND ((AND (GREATERP I N) (GREATERP J M)) (PROGN NIL))
               (T
                (PROGN
                 (PROG (L)
                   (SETQ L (PLUS I 1))
                  LAB
                   (COND ((MINUSP (DIFFERENCE N L)) (RETURN NIL)))
                   (PROGN
                    (COND ((EQUAL (GETMAT A L K) 0) (SETQ L (PLUS L 1)))
                          ((LESSP (DEG (GETMAT A L K) X)
                                  (DEG (GETMAT A I K) X))
                           (SETQ I L)))
                    NIL)
                   (SETQ L (PLUS2 L 1))
                   (GO LAB))
                 (PROG (L)
                   (SETQ L (PLUS J 1))
                  LAB
                   (COND ((MINUSP (DIFFERENCE M L)) (RETURN NIL)))
                   (PROGN
                    (COND ((EQUAL (GETMAT A K L) 0) (SETQ L (PLUS L 1)))
                          ((LESSP (DEG (GETMAT A K L) X)
                                  (DEG (GETMAT A K J) X))
                           (SETQ J L)))
                    NIL)
                   (SETQ L (PLUS2 L 1))
                   (GO LAB))
                 (COND
                  ((AND (LEQ I N)
                        (OR (GREATERP J M)
                            (LESSP (DEG (GETMAT A I K) X)
                                   (DEG (GETMAT A K J) X))))
                   (PROGN
                    (COND
                     ((NEQ I K)
                      (PROGN
                       (PROG (L)
                         (SETQ L K)
                        LAB
                         (COND ((MINUSP (DIFFERENCE M L)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP (GETMAT A I L))
                          (SETMAT A I L (GETMAT A K L))
                          (SETMAT A K L TMP)
                          NIL)
                         (SETQ L (PLUS2 L 1))
                         (GO LAB))
                       (PROG (L)
                         (SETQ L 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE N L)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP (GETMAT LEFT L I))
                          (SETMAT LEFT L I (GETMAT LEFT L K))
                          (SETMAT LEFT L K TMP)
                          NIL)
                         (SETQ L (PLUS2 L 1))
                         (GO LAB))
                       NIL)))))
                  (T
                   (PROGN
                    (COND
                     ((NEQ J K)
                      (PROGN
                       (PROG (L)
                         (SETQ L K)
                        LAB
                         (COND ((MINUSP (DIFFERENCE N L)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP (GETMAT A L J))
                          (SETMAT A L J (GETMAT A L K))
                          (SETMAT A L K TMP)
                          NIL)
                         (SETQ L (PLUS2 L 1))
                         (GO LAB))
                       (PROG (L)
                         (SETQ L 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE M L)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP (GETMAT RIGHT J L))
                          (SETMAT RIGHT J L (GETMAT RIGHT K L))
                          (SETMAT RIGHT K L TMP)
                          NIL)
                         (SETQ L (PLUS2 L 1))
                         (GO LAB))
                       NIL)))
                    NIL)))
                 (SETQ ISCLEAR NIL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT (NOT ISCLEAR)) (RETURN NIL)))
                   (PROGN
                    (PROG (I)
                      (SETQ I (PLUS K 1))
                     LAB
                      (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                      (PROGN
                       (COND ((EQUAL (GETMAT A I K) 0) (PROGN NIL))
                             (T
                              (PROGN
                               (SETQ POLY1 (GETMAT A K K))
                               (SETQ POLY2 (GETMAT A I K))
                               (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
                               (SETQ G (CAR TMP))
                               (SETQ L (CADR TMP))
                               (SETQ R1 (CADDR TMP))
                               (SETQ QUO1 (GET_QUO POLY1 G))
                               (SETQ QUO2 (GET_QUO POLY2 G))
                               (PROG (J)
                                 (SETQ J (PLUS K 1))
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (LIST 'PLUS
                                                (LIST 'TIMES L (GETMAT A K J))
                                                (LIST 'TIMES R1
                                                      (GETMAT A I J))))
                                  (SETMAT A I J
                                   (LIST 'PLUS
                                         (LIST 'TIMES QUO1 (GETMAT A I J))
                                         (LIST 'TIMES (LIST 'MINUS QUO2)
                                               (GETMAT A K J))))
                                  (SETMAT A K J TMP)
                                  NIL)
                                 (SETQ J (PLUS2 J 1))
                                 (GO LAB))
                               (PROG (J)
                                 (SETQ J 1)
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (LIST 'PLUS
                                                (LIST 'TIMES QUO1
                                                      (GETMAT LEFT J K))
                                                (LIST 'TIMES QUO2
                                                      (GETMAT LEFT J I))))
                                  (SETMAT LEFT J I
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'MINUS R1)
                                               (GETMAT LEFT J K))
                                         (LIST 'TIMES L (GETMAT LEFT J I))))
                                  (SETMAT LEFT J K TMP)
                                  NIL)
                                 (SETQ J (PLUS2 J 1))
                                 (GO LAB))
                               (SETMAT A K K G)
                               (SETMAT A I K 0)
                               NIL)))
                       NIL)
                      (SETQ I (PLUS2 I 1))
                      (GO LAB))
                    (SETQ ISCLEAR T)
                    (PROG (I)
                      (SETQ I (PLUS K 1))
                     LAB
                      (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
                      (PROGN
                       (SETQ Q (GET_QUO (GETMAT A K I) (GETMAT A K K)))
                       (SETMAT A K I (GET_REM (GETMAT A K I) (GETMAT A K K)))
                       (PROG (J)
                         (SETQ J 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                         (PROGN
                          (SETMAT RIGHT K J
                           (LIST 'PLUS (GETMAT RIGHT K J)
                                 (LIST 'TIMES Q (GETMAT RIGHT I J))))
                          NIL)
                         (SETQ J (PLUS2 J 1))
                         (GO LAB))
                       NIL)
                      (SETQ I (PLUS2 I 1))
                      (GO LAB))
                    (PROG (I)
                      (SETQ I (PLUS K 1))
                     LAB
                      (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
                      (PROGN
                       (COND ((EQUAL (GETMAT A K I) 0) (PROGN NIL))
                             (T
                              (PROGN
                               (SETQ POLY1 (GETMAT A K K))
                               (SETQ POLY2 (GETMAT A K I))
                               (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
                               (SETQ G (CAR TMP))
                               (SETQ L (CADR TMP))
                               (SETQ R1 (CADDR TMP))
                               (SETQ QUO1 (GET_QUO POLY1 G))
                               (SETQ QUO2 (GET_QUO POLY2 G))
                               (PROG (J)
                                 (SETQ J (PLUS K 1))
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (LIST 'PLUS
                                                (LIST 'TIMES L (GETMAT A J K))
                                                (LIST 'TIMES R1
                                                      (GETMAT A J I))))
                                  (SETMAT A J I
                                   (LIST 'PLUS
                                         (LIST 'TIMES QUO1 (GETMAT A J I))
                                         (LIST 'TIMES (LIST 'MINUS QUO2)
                                               (GETMAT A J K))))
                                  (SETMAT A J K TMP)
                                  NIL)
                                 (SETQ J (PLUS2 J 1))
                                 (GO LAB))
                               (PROG (J)
                                 (SETQ J 1)
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (LIST 'PLUS
                                                (LIST 'TIMES QUO1
                                                      (GETMAT RIGHT K J))
                                                (LIST 'TIMES QUO2
                                                      (GETMAT RIGHT I J))))
                                  (SETMAT RIGHT I J
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'MINUS R1)
                                               (GETMAT RIGHT K J))
                                         (LIST 'TIMES L (GETMAT RIGHT I J))))
                                  (SETMAT RIGHT K J TMP)
                                  NIL)
                                 (SETQ J (PLUS2 J 1))
                                 (GO LAB))
                               (SETMAT A K K G)
                               (SETMAT A K I 0)
                               (SETQ ISCLEAR NIL)
                               NIL)))
                       NIL)
                      (SETQ I (PLUS2 I 1))
                      (GO LAB))
                    NIL)
                   (GO WHILELABEL))
                 NIL)))
         NIL)
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ R 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (MIN N M) I)) (RETURN NIL)))
        (PROGN
         (COND
          ((NEQ (GETMAT A I I) 0)
           (PROGN
            (SETQ R (PLUS R 1))
            (COND ((EQUAL (LCOF (GETMAT A I I) X) 0) (SETQ LC (GETMAT A I I)))
                  (T (SETQ LC (LCOF (GETMAT A I I) X))))
            (SETMAT A R R (LIST 'QUOTIENT (GETMAT A I I) LC))
            (COND
             ((EQUAL I R)
              (PROGN
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                 (PROGN
                  (SETMAT RIGHT I J (LIST 'TIMES (GETMAT RIGHT I J) LC))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL))
             (T
              (PROGN
               (SETMAT A I I 0)
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP (GETMAT LEFT J R))
                  (SETMAT LEFT J R (GETMAT LEFT J I))
                  (SETMAT LEFT J I TMP)
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP (LIST 'TIMES (GETMAT RIGHT I J) LC))
                  (SETMAT RIGHT I J (LIST 'QUOTIENT (GETMAT RIGHT R J) LC))
                  (SETMAT RIGHT R J TMP)
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE R 1) I)) (RETURN NIL)))
        (PROGN
         (SETQ J (PLUS I 1))
         (PROGN
          (PROG ()
           WHILELABEL
            (COND ((NOT (AND (NEQ (GETMAT A I I) 1) (LEQ J R))) (RETURN NIL)))
            (PROGN
             (SETQ POLY1 (GETMAT A I I))
             (SETQ POLY2 (GETMAT A J J))
             (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
             (SETQ G (CAR TMP))
             (SETQ L (CADR TMP))
             (SETQ R1 (CADDR TMP))
             (SETQ QUO1 (GET_QUO POLY1 G))
             (SETQ QUO2 (GET_QUO POLY2 G))
             (SETMAT A I I G)
             (SETMAT A J J (LIST 'TIMES QUO1 (GETMAT A J J)))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
               (PROGN
                (SETQ TMP
                        (LIST 'PLUS (LIST 'TIMES QUO1 (GETMAT LEFT K I))
                              (LIST 'TIMES QUO2 (GETMAT LEFT K J))))
                (SETMAT LEFT K J
                 (LIST 'PLUS (LIST 'TIMES (LIST 'MINUS R1) (GETMAT LEFT K I))
                       (LIST 'TIMES L (GETMAT LEFT K J))))
                (SETMAT LEFT K I TMP)
                NIL)
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
               (PROGN
                (SETQ TQUO (LIST 'TIMES R1 QUO2))
                (SETQ TMP
                        (LIST 'PLUS
                              (LIST 'TIMES (LIST 'PLUS 1 (LIST 'MINUS TQUO))
                                    (GETMAT RIGHT I K))
                              (LIST 'TIMES TQUO (GETMAT RIGHT J K))))
                (SETMAT RIGHT J K
                 (LIST 'PLUS (LIST 'MINUS (GETMAT RIGHT I K))
                       (GETMAT RIGHT J K)))
                (SETMAT RIGHT I K TMP)
                NIL)
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (SETQ J (PLUS J 1))
             NIL)
            (GO WHILELABEL))
          NIL)
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
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
      (SETQ A (DE_NEST_MAT A))
      (SETQ LEFT (DE_NEST_MAT LEFT))
      (SETQ RIGHT (DE_NEST_MAT RIGHT))
      (CLEARRULES (LIST RULE_LIST))
      (COND
       ((AND (NEQ INPUT_MODE 'RATIONAL) (NEQ INPUT_MODE 'ARNUM))
        (PROGN
         (COND ((EQUAL INPUT_MODE 'NIL) (OFF (LIST 'RATIONAL)))
               (T (ONOFF INPUT_MODE T)))
         NIL)))
      (OFF (LIST 'COMBINEEXPT))
      (RETURN (LIST 'LIST A LEFT RIGHT)))) 
(FLAG '(SMITHEX) 'OPFN) 
(PUT 'GET_COEFFS_SMITH 'NUMBER-OF-ARGS 2) 
(PUT 'GET_COEFFS_SMITH 'DEFINED-ON-LINE '365) 
(PUT 'GET_COEFFS_SMITH 'DEFINED-IN-FILE 'NORMFORM/SMITHEX.RED) 
(PUT 'GET_COEFFS_SMITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_COEFFS_SMITH (POLY X)
    (PROG (KER_LIST_NUM KER_LIST_DEN NEW_LIST)
      (SETQ KER_LIST_NUM (KERNELS (*Q2F (SIMP (REVAL1 (NUM POLY) T)))))
      (SETQ KER_LIST_DEN (KERNELS (*Q2F (SIMP (REVAL1 (DEN POLY) T)))))
      (SETQ KER_LIST_NUM (UNION KER_LIST_NUM KER_LIST_DEN))
      (COND ((EQUAL KER_LIST_NUM NIL) (SETQ NEW_LIST NIL))
            (T
             (PROGN
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH KER_LIST_NUM) I)) (RETURN NIL)))
                (PROGN
                 (COND ((EQUAL (CAR KER_LIST_NUM) X) (SETQ NEW_LIST NEW_LIST))
                       (T (SETQ NEW_LIST (CONS (CAR KER_LIST_NUM) NEW_LIST))))
                 (SETQ KER_LIST_NUM (CDR KER_LIST_NUM))
                 NIL)
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              NIL)))
      (RETURN NEW_LIST))) 
(PUT 'NEST_INPUT_SMITH 'NUMBER-OF-ARGS 2) 
(PUT 'NEST_INPUT_SMITH 'DEFINED-ON-LINE '400) 
(PUT 'NEST_INPUT_SMITH 'DEFINED-IN-FILE 'NORMFORM/SMITHEX.RED) 
(PUT 'NEST_INPUT_SMITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEST_INPUT_SMITH (A X)
    (PROG (TMP COEFF_LIST FULL_COEFF_LIST AA ROW_DIM COL_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COL_DIM 0)
      (SETQ FULL_COEFF_LIST NIL)
      (SETQ COEFF_LIST NIL)
      (SETQ AA (COPY_MAT A))
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX AA)))
      (SETQ COL_DIM (CADR (SIZE_OF_MATRIX AA)))
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
            (SETQ COEFF_LIST (GET_COEFFS_SMITH (GETMAT AA I J) X))
            (COND ((EQUAL COEFF_LIST NIL) (PROGN NIL))
                  (T
                   (SETQ FULL_COEFF_LIST (UNION COEFF_LIST FULL_COEFF_LIST))))
            (PROG (ELT)
              (SETQ ELT COEFF_LIST)
             LAB
              (COND ((NULL ELT) (RETURN NIL)))
              ((LAMBDA (ELT)
                 (PROGN
                  (SETQ TMP (LIST 'CO 2 ELT))
                  (SETMAT AA I J
                   (AEVAL*
                    (LIST 'SUB (LIST 'EQUAL ELT TMP) (LIST 'GETMAT AA I J))))
                  NIL))
               (CAR ELT))
              (SETQ ELT (CDR ELT))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST AA FULL_COEFF_LIST)))) 
(SWITCH (LIST 'INT_TEST)) 
(PUT 'ALL_INTEGER_ENTRIES_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_INTEGER_ENTRIES_TEST 'DEFINED-ON-LINE '445) 
(PUT 'ALL_INTEGER_ENTRIES_TEST 'DEFINED-IN-FILE 'NORMFORM/SMITHEX.RED) 
(PUT 'ALL_INTEGER_ENTRIES_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_INTEGER_ENTRIES_TEST (MAT1)
    (PROG ()
      (ON (LIST 'INT_TEST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((MINUSP (DIFFERENCE (CAR (SIZE_OF_MATRIX MAT1)) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((MINUSP (DIFFERENCE (CADR (SIZE_OF_MATRIX MAT1)) J))
             (RETURN NIL)))
           (PROGN
            (COND
             ((AND (NOT (NUMBERP (GETMAT MAT1 I J))) *INT_TEST)
              (OFF (LIST 'INT_TEST))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       (*INT_TEST
        (PRIN2T "*** WARNING: all matrix entries are integers.
    If calculations in Z(the integers) are required, use smithex_int."))))) 
(ENDMODULE) 