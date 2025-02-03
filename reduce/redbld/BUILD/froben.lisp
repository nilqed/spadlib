(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FROBEN)) 
(PUT 'FROBENIUS 'NUMBER-OF-ARGS 1) 
(PUT 'FROBENIUS 'DEFINED-ON-LINE '84) 
(PUT 'FROBENIUS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'FROBENIUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FROBENIUS (A)
    (PROG (AA P PINV ANS TMP FULL_COEFF_LIST RULE_LIST INPUT_MODE)
      (MATRIX_INPUT_TEST A)
      (COND
       ((NEQ (CAR (SIZE_OF_MATRIX A)) (CADR (SIZE_OF_MATRIX A)))
        (REDERR "ERROR: expecting a square matrix. ")))
      (SETQ INPUT_MODE (GET DMODE* 'DNAME))
      (COND
       ((AND (NEQ INPUT_MODE 'MODULAR) (NEQ INPUT_MODE 'ARNUM)
             (NEQ INPUT_MODE 'RATIONAL))
        (ON (LIST 'RATIONAL))))
      (ON (LIST 'COMBINEEXPT))
      (SETQ TMP (NEST_INPUT A))
      (SETQ AA (CAR TMP))
      (SETQ FULL_COEFF_LIST (CADR TMP))
      (SETQ TMP (FROBENIUSFORM AA))
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
      (SETQ ANS (DE_NEST_MAT ANS))
      (SETQ P (DE_NEST_MAT P))
      (SETQ PINV (DE_NEST_MAT PINV))
      (CLEARRULES (LIST RULE_LIST))
      (COND
       ((AND (NEQ INPUT_MODE 'MODULAR) (NEQ INPUT_MODE 'ARNUM)
             (NEQ INPUT_MODE 'RATIONAL))
        (PROGN
         (COND ((EQUAL INPUT_MODE 'NIL) (OFF (LIST 'RATIONAL)))
               (T (ONOFF INPUT_MODE T)))
         NIL)))
      (OFF (LIST 'COMBINEEXPT))
      (RETURN (LIST 'LIST ANS P PINV)))) 
(FLAG '(FROBENIUS) 'OPFN) 
(PUT 'FROBENIUSFORM 'NUMBER-OF-ARGS 1) 
(PUT 'FROBENIUSFORM 'DEFINED-ON-LINE '145) 
(PUT 'FROBENIUSFORM 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'FROBENIUSFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FROBENIUSFORM (A)
    (PROG (ANS PLIST TMP P PINV INV_FACT T1 TINV V VINV X)
      (SETQ X (MKID 'X 0))
      (SETQ TMP (CYCLIC_VECTORS A X))
      (SETQ PLIST (CAR TMP))
      (SETQ V (CADR TMP))
      (SETQ VINV (CADDR TMP))
      (SETQ TMP (CYCLIC_TO_FROBENIUS PLIST X))
      (SETQ INV_FACT (CAR TMP))
      (SETQ T1 (CADR TMP))
      (SETQ TINV (CADDR TMP))
      (SETQ P (REVAL1 (LIST 'TIMES V T1) T))
      (SETQ PINV (REVAL1 (LIST 'TIMES TINV VINV) T))
      (SETQ ANS (INVFACT_TO_FROBENIUS INV_FACT X))
      (RETURN (LIST ANS P PINV)))) 
(PUT 'BASIS 'NUMBER-OF-ARGS 2) 
(PUT 'BASIS 'DEFINED-ON-LINE '172) 
(PUT 'BASIS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'BASIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BASIS (N I)
    (PROG (VV) (SETQ VV (MKMATRIX 1 N)) (SETMAT VV 1 I 1) (RETURN VV))) 
(PUT 'CALC_EXGCD 'NUMBER-OF-ARGS 3) 
(PUT 'CALC_EXGCD 'DEFINED-ON-LINE '186) 
(PUT 'CALC_EXGCD 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'CALC_EXGCD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALC_EXGCD (POLY1 POLY2 X)
    (PROG (GCD C C1 C2 D D1 D2 Q R R1 R2 S1 T1)
      (COND ((AND (EQUAL POLY1 0) (EQUAL POLY2 0)) (RETURN (LIST 0 0 0)))
            (T
             (PROGN
              (SETQ POLY1 (REVAL1 POLY1 T))
              (SETQ POLY2 (REVAL1 POLY2 T))
              (SETQ C (REVAL1 (NORM POLY1 X) T))
              (SETQ D (REVAL1 (NORM POLY2 X) T))
              (SETQ C1 1)
              (SETQ D1 0)
              (SETQ C2 0)
              (SETQ D2 1)
              (PROG ()
               WHILELABEL
                (COND ((NOT (NEQ (REVAL1 D T) 0)) (RETURN NIL)))
                (PROGN
                 (SETQ Q (REVAL1 (GET_QUO C D) T))
                 (SETQ R
                         (REVAL1 (LIST 'PLUS C (LIST 'MINUS (LIST 'TIMES Q D)))
                                 T))
                 (SETQ R1
                         (REVAL1
                          (LIST 'PLUS C1 (LIST 'MINUS (LIST 'TIMES Q D1))) T))
                 (SETQ R2
                         (REVAL1
                          (LIST 'PLUS C2 (LIST 'MINUS (LIST 'TIMES Q D2))) T))
                 (SETQ C (REVAL1 D T))
                 (SETQ C1 (REVAL1 D1 T))
                 (SETQ C2 (REVAL1 D2 T))
                 (SETQ D (REVAL1 R T))
                 (SETQ D1 (REVAL1 R1 T))
                 (SETQ D2 (REVAL1 R2 T))
                 NIL)
                (GO WHILELABEL))
              (SETQ GCD (REVAL1 (NORM C X) T))
              (SETQ S1
                      (REVAL1
                       (LIST 'QUOTIENT C1
                             (LIST 'TIMES (NORMFORM_UNIT POLY1 X)
                                   (NORMFORM_UNIT C X)))
                       T))
              (SETQ T1
                      (REVAL1
                       (LIST 'QUOTIENT C2
                             (LIST 'TIMES (NORMFORM_UNIT POLY2 X)
                                   (NORMFORM_UNIT C X)))
                       T))
              (RETURN (LIST GCD S1 T1))
              NIL))))) 
(PUT 'NORM 'NUMBER-OF-ARGS 2) 
(PUT 'NORM 'DEFINED-ON-LINE '234) 
(PUT 'NORM 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'NORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NORM (POLY X)
    (PROG (NORMAL)
      (COND ((EQUAL POLY 0) (SETQ NORMAL 0))
            ((EQUAL (LCOF POLY X) 0) (SETQ NORMAL 1))
            (T (SETQ NORMAL (REVAL1 (LIST 'QUOTIENT POLY (LCOF POLY X)) T))))
      (RETURN NORMAL))) 
(PUT 'NORMFORM_UNIT 'NUMBER-OF-ARGS 2) 
(PUT 'NORMFORM_UNIT 'DEFINED-ON-LINE '245) 
(PUT 'NORMFORM_UNIT 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'NORMFORM_UNIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NORMFORM_UNIT (POLY X)
    (PROG (UNIT1)
      (COND ((EQUAL POLY 0) (SETQ UNIT1 1))
            ((EQUAL (LCOF POLY X) 0) (SETQ UNIT1 POLY))
            (T (SETQ UNIT1 (REVAL1 (LCOF POLY X) T))))
      (RETURN UNIT1))) 
(PUT 'COMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'COMPANION 'DEFINED-ON-LINE '257) 
(PUT 'COMPANION 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'COMPANION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPANION (POLY X)
    (PROG (MAT1 N)
      (SETQ N 0)
      (SETQ N (DEG POLY X))
      (COND
       ((NEQ (DE_NEST (REVAL1 (COEFFN POLY X N) T)) 1)
        (REDERR (LIST "ERROR: polynomial" POLY " is not monic."))))
      (SETQ MAT1 (MKMATRIX N N))
      (SETMAT MAT1 1 N (LIST 'MINUS (COEFFN POLY X 0)))
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN (SETMAT MAT1 I (DIFFERENCE I 1) 1) NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETMAT MAT1 J N (LIST 'MINUS (COEFFN POLY X (DIFFERENCE J 1))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN MAT1))) 
(FLAG '(COMPANION) 'OPFN) 
(PUT 'COMPUTE_G 'NUMBER-OF-ARGS 4) 
(PUT 'COMPUTE_G 'DEFINED-ON-LINE '297) 
(PUT 'COMPUTE_G 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'COMPUTE_G 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPUTE_G (R DD PLIST X)
    (PROG (G TMP NEW_ELT)
      (SETQ G (MKMATRIX R R))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE J 1) I)) (RETURN NIL)))
           (PROGN
            (SETQ NEW_ELT 0)
            (PROG (K)
              (SETQ K (GETMAT DD 1 I))
             LAB
              (COND
               ((MINUSP (DIFFERENCE (DIFFERENCE (GETMAT DD 1 (PLUS I 1)) 1) K))
                (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (LIST 'TIMES (COEFFN (NTH PLIST J) X K)
                             (LIST 'EXPT X
                                   (LIST 'PLUS K
                                         (LIST 'MINUS (GETMAT DD 1 I))))))
               (SETQ NEW_ELT (LIST 'PLUS NEW_ELT TMP))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETMAT G I J NEW_ELT)
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ NEW_ELT 0)
         (PROG (K)
           (SETQ K (GETMAT DD 1 J))
          LAB
           (COND
            ((MINUSP (DIFFERENCE (GETMAT DD 1 (PLUS J 1)) K)) (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (LIST 'TIMES (COEFFN (NTH PLIST J) X K)
                          (LIST 'EXPT X
                                (LIST 'PLUS K (LIST 'MINUS (GETMAT DD 1 J))))))
            (SETQ NEW_ELT (LIST 'PLUS NEW_ELT TMP))
            NIL)
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (SETMAT G J J NEW_ELT)
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN G))) 
(PUT 'COPY_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'COPY_MAT 'DEFINED-ON-LINE '335) 
(PUT 'COPY_MAT 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'COPY_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPY_MAT (A)
    (PROG (C ROW_DIM COL_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COL_DIM 0)
      (MATRIX_INPUT_TEST A)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX A)))
      (SETQ COL_DIM (CADR (SIZE_OF_MATRIX A)))
      (SETQ C (MKMATRIX ROW_DIM COL_DIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COL_DIM J)) (RETURN NIL)))
           (PROGN (SETMAT C I J (GETMAT A I J)) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN C))) 
(PUT 'CYCLIC_TO_FROBENIUS 'NUMBER-OF-ARGS 2) 
(PUT 'CYCLIC_TO_FROBENIUS 'DEFINED-ON-LINE '364) 
(PUT 'CYCLIC_TO_FROBENIUS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'CYCLIC_TO_FROBENIUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CYCLIC_TO_FROBENIUS (PLIST X)
    (PROG (DD D US S G C T1 TINV INV_FACT L LINV COLUMNT ROWT RR Q COLUMNTINV
           ROWTINV TMP TMP1 R N)
      (SETQ R 0)
      (SETQ N 0)
      (SETQ R (LENGTH PLIST))
      (SETQ DD (MKMATRIX 1 (PLUS R 1)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
        (PROGN (SETMAT DD 1 (PLUS J 1) (DEG (NTH PLIST J) X)) NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ N (GETMAT DD 1 (PLUS R 1)))
      (SETQ G (COMPUTE_G R DD PLIST X))
      (SETQ TMP (UPPERSMITH G X))
      (SETQ US (CAR TMP))
      (SETQ L (CADR TMP))
      (SETQ LINV (CADDR TMP))
      (SETQ TMP (MYSMITH US L LINV X))
      (SETQ S (CAR TMP))
      (SETQ L (CADR TMP))
      (SETQ LINV (CADDR TMP))
      (SETQ D (MKMATRIX 1 R))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN (SETMAT D 1 I (DEG (GETMAT S I I) X)) NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ C (MKMATRIX 1 R))
      (SETQ T1 (MKMATRIX N N))
      (SETQ COLUMNT 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R K)) (RETURN NIL)))
           (PROGN (SETMAT C 1 K (GETMAT L K I)) NIL)
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (GETMAT D 1 I) J)) (RETURN NIL)))
           (PROGN
            (SETQ COLUMNT (PLUS COLUMNT 1))
            (PROG (II)
              (SETQ II R)
             LAB
              (COND
               ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 II))) (RETURN NIL)))
              (PROGN
               (SETQ Q (GET_QUO (GETMAT C 1 II) (GETMAT G II II)))
               (SETQ RR (GET_REM (GETMAT C 1 II) (GETMAT G II II)))
               (SETMAT C 1 II RR)
               (PROG (JJ)
                 (SETQ JJ 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE II 1) JJ)) (RETURN NIL)))
                 (PROGN
                  (SETMAT C 1 JJ
                   (REVAL1
                    (LIST 'PLUS (REVAL1 (GETMAT C 1 JJ) T)
                          (LIST 'TIMES (LIST 'MINUS Q)
                                (REVAL1 (GETMAT G JJ II) T)))
                    T))
                  NIL)
                 (SETQ JJ (PLUS2 JJ 1))
                 (GO LAB))
               NIL)
              (SETQ II (PLUS2 II (MINUS 1)))
              (GO LAB))
            (SETQ ROWT 0)
            (PROG (II)
              (SETQ II 1)
             LAB
              (COND ((MINUSP (DIFFERENCE R II)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (REVAL1
                        (LIST 'PLUS (GETMAT DD 1 (PLUS II 1))
                              (LIST 'MINUS (GETMAT DD 1 II)))
                        T))
               (PROG (JJ)
                 (SETQ JJ 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE TMP JJ)) (RETURN NIL)))
                 (PROGN
                  (SETQ ROWT (PLUS ROWT 1))
                  (SETQ TMP1 (COEFFN (GETMAT C 1 II) X (DIFFERENCE JJ 1)))
                  (SETMAT T1 ROWT COLUMNT TMP1)
                  NIL)
                 (SETQ JJ (PLUS2 JJ 1))
                 (GO LAB))
               NIL)
              (SETQ II (PLUS2 II 1))
              (GO LAB))
            (PROG (II)
              (SETQ II 1)
             LAB
              (COND ((MINUSP (DIFFERENCE R II)) (RETURN NIL)))
              (SETMAT C 1 II (LIST 'TIMES (GETMAT C 1 II) X))
              (SETQ II (PLUS2 II 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROGN
       (SETQ TINV (MKMATRIX N N))
       (SETQ COLUMNTINV 0)
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
         (PROGN
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((MINUSP (DIFFERENCE R K)) (RETURN NIL)))
            (SETMAT C 1 K (GETMAT LINV K I))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND
             ((MINUSP
               (DIFFERENCE
                (REVAL1
                 (LIST 'PLUS (GETMAT DD 1 (PLUS I 1))
                       (LIST 'MINUS (GETMAT DD 1 I)))
                 T)
                J))
              (RETURN NIL)))
            (PROGN
             (SETQ COLUMNTINV (PLUS COLUMNTINV 1))
             (SETQ ROWTINV 0)
             (PROG (II)
               (SETQ II 1)
              LAB
               (COND ((MINUSP (DIFFERENCE R II)) (RETURN NIL)))
               (PROGN
                (SETMAT C 1 II (GET_REM (GETMAT C 1 II) (GETMAT S II II)))
                (PROG (JJ)
                  (SETQ JJ 1)
                 LAB
                  (COND
                   ((MINUSP (DIFFERENCE (REVAL1 (GETMAT D 1 II) T) JJ))
                    (RETURN NIL)))
                  (PROGN
                   (SETQ ROWTINV (PLUS ROWTINV 1))
                   (SETMAT TINV ROWTINV COLUMNTINV
                    (REVAL1 (COEFFN (GETMAT C 1 II) X (DIFFERENCE JJ 1)) T))
                   NIL)
                  (SETQ JJ (PLUS2 JJ 1))
                  (GO LAB))
                NIL)
               (SETQ II (PLUS2 II 1))
               (GO LAB))
             (PROG (II)
               (SETQ II 1)
              LAB
               (COND ((MINUSP (DIFFERENCE R II)) (RETURN NIL)))
               (SETMAT C 1 II (LIST 'TIMES (GETMAT C 1 II) X))
               (SETQ II (PLUS2 II 1))
               (GO LAB))
             NIL)
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          NIL)
         (SETQ I (PLUS2 I 1))
         (GO LAB))
       NIL)
      (SETQ INV_FACT (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (COND
          ((GREATERP (GETMAT D 1 I) 0)
           (PROGN
            (SETQ INV_FACT (APPEND INV_FACT (LIST (GETMAT S I I))))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST INV_FACT T1 TINV)))) 
(PUT 'CYCLIC_VECTORS 'NUMBER-OF-ARGS 2) 
(PUT 'CYCLIC_VECTORS 'DEFINED-ON-LINE '512) 
(PUT 'CYCLIC_VECTORS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'CYCLIC_VECTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CYCLIC_VECTORS (A X)
    (PROG (V VINV PLIST U UINV S CARRIER LINCOMB VV UU SS L CAR C TMP ANS Q
           BREAK N R)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ N (CAR (SIZE_OF_MATRIX A)))
      (SETQ U (MKMATRIX N N))
      (SETQ S (MKMATRIX N N))
      (SETQ PLIST (LIST))
      (SETQ V (MKMATRIX N N))
      (SETQ VINV (MKMATRIX N N))
      (SETQ CARRIER (MKVECT N))
      (SETQ LINCOMB (MKVECT N))
      (SETQ R 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP R N)) (RETURN NIL)))
        (PROGN
         (SETQ Q 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ (GETV CARRIER Q) NIL)) (RETURN NIL)))
           (SETQ Q (PLUS Q 1))
           (GO WHILELABEL))
         (SETQ VV (BASIS N Q))
         (SETQ BREAK NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT BREAK)) (RETURN NIL)))
           (PROGN
            (SETQ UU (COPY_MAT VV))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
              (PUTV LINCOMB I 0)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
              (PROGN
               (SETQ CAR (GETV CARRIER I))
               (COND
                ((AND (NEQ CAR NIL) (NEQ (GETMAT UU 1 I) 0))
                 (PROGN
                  (SETQ C (LIST 'QUOTIENT (GETMAT UU 1 I) (GETMAT U I CAR)))
                  (SETMAT UU 1 I 0)
                  (PROG (J)
                    (SETQ J (PLUS I 1))
                   LAB
                    (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                    (PROGN
                     (SETQ TMP (LIST 'TIMES C (GETMAT U J CAR)))
                     (SETMAT UU 1 J
                      (REVAL1
                       (LIST 'PLUS (GETMAT UU 1 J)
                             (LIST 'MINUS (LIST 'TIMES C (GETMAT U J CAR))))
                       T))
                     NIL)
                    (SETQ J (PLUS2 J 1))
                    (GO LAB))
                  (PUTV LINCOMB CAR C)
                  NIL)))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ Q 1)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND (LEQ Q N) (EQUAL (REVAL1 (GETMAT UU 1 Q) T) 0)))
                (RETURN NIL)))
              (PROGN (SETQ Q (PLUS Q 1)) NIL)
              (GO WHILELABEL))
            (COND
             ((LEQ Q N)
              (PROGN
               (SETQ R (PLUS R 1))
               (PUTV CARRIER Q R)
               (PROG (J)
                 (SETQ J Q)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (SETMAT U J R (GETMAT UU 1 J))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (SETMAT V J R (GETMAT VV 1 J))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE (DIFFERENCE R 1) J)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP (GETV LINCOMB J))
                  (PROG (L)
                    (SETQ L (PLUS J 1))
                   LAB
                    (COND
                     ((MINUSP (DIFFERENCE (DIFFERENCE R 1) L)) (RETURN NIL)))
                    (SETQ TMP
                            (LIST 'PLUS TMP
                                  (LIST 'TIMES (GETMAT S J L)
                                        (GETV LINCOMB L))))
                    (SETQ L (PLUS2 L 1))
                    (GO LAB))
                  (SETMAT S J R (LIST 'MINUS TMP))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (SETMAT S R R 1)
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP 0)
                  (PROG (J)
                    (SETQ J 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                    (PROGN
                     (SETQ TMP
                             (REVAL1
                              (LIST 'PLUS TMP
                                    (REVAL1
                                     (LIST 'TIMES (REVAL1 (GETMAT A I J) T)
                                           (REVAL1 (GETMAT VV 1 J) T))
                                     T))
                              T))
                     NIL)
                    (SETQ J (PLUS2 J 1))
                    (GO LAB))
                  (SETMAT UU 1 I TMP)
                  NIL)
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (CADR (SIZE_OF_MATRIX UU)) I))
                   (RETURN NIL)))
                 (PROGN (SETMAT VV 1 I (GETMAT UU 1 I)) NIL)
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               NIL))
             (T (PROGN (SETQ BREAK T) NIL)))
            NIL)
           (GO WHILELABEL))
         (SETQ SS (MKMATRIX 1 R))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (REVAL1 (GETV LINCOMB J) T))
            (PROG (L)
              (SETQ L (PLUS J 1))
             LAB
              (COND ((MINUSP (DIFFERENCE R L)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (REVAL1
                        (LIST 'PLUS TMP
                              (LIST 'TIMES (REVAL1 (GETMAT S J L) T)
                                    (REVAL1 (GETV LINCOMB L) T)))
                        T))
               NIL)
              (SETQ L (PLUS2 L 1))
              (GO LAB))
            (SETMAT SS 1 J TMP)
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ ANS NIL)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP
                    (LIST 'TIMES (GETMAT SS 1 (PLUS R (DIFFERENCE 1 J)))
                          (LIST 'EXPT X (DIFFERENCE R J))))
            (SETQ ANS (REVAL1 (LIST 'PLUS ANS TMP) T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ TMP (REVAL1 (LIST 'PLUS (LIST 'EXPT X R) (LIST 'MINUS ANS)) T))
         (SETQ PLIST (APPEND PLIST (LIST TMP)))
         NIL)
        (GO WHILELABEL))
      (SETQ UINV (INV U CARRIER))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP 0)
            (PROG (L)
              (SETQ L I)
             LAB
              (COND ((MINUSP (DIFFERENCE N L)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (REVAL1
                        (LIST 'PLUS TMP
                              (LIST 'TIMES (REVAL1 (GETMAT S I L) T)
                                    (REVAL1 (GETMAT UINV L J) T)))
                        T))
               NIL)
              (SETQ L (PLUS2 L 1))
              (GO LAB))
            (SETMAT VINV I J TMP)
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST PLIST V VINV)))) 
(PUT 'DE_NEST 'NUMBER-OF-ARGS 1) 
(PUT 'DE_NEST 'DEFINED-ON-LINE '683) 
(PUT 'DE_NEST 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'DE_NEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DE_NEST (INPUT)
    (PROG (OUTPUT)
      (COND ((ATOM INPUT) (SETQ OUTPUT INPUT))
            ((NEQ (CAR INPUT) 'CO) (SETQ OUTPUT INPUT))
            (T (SETQ OUTPUT (CADDR INPUT))))
      (RETURN OUTPUT))) 
(PUT 'DE_NEST_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'DE_NEST_MAT 'DEFINED-ON-LINE '698) 
(PUT 'DE_NEST_MAT 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'DE_NEST_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DE_NEST_MAT (MAT1)
    (PROG (ROW_DIM COL_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COL_DIM 0)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX MAT1)))
      (SETQ COL_DIM (CADR (SIZE_OF_MATRIX MAT1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COL_DIM J)) (RETURN NIL)))
           (PROGN (SETMAT MAT1 I J (GETMAT MAT1 I J)) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN MAT1))) 
(PUT 'DIAGI 'PSOPFN 'DIAG) 
(PUT 'DIAG 'NUMBER-OF-ARGS 1) 
(PUT 'DIAG 'DEFINED-ON-LINE '725) 
(PUT 'DIAG 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'DIAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIAG (UU)
    (PROG (BIGA ARG INPUT U NARGS N AIDX STP BIGSIZE SMALLSIZE)
      (SETQ NARGS 0)
      (SETQ N 0)
      (SETQ AIDX 0)
      (SETQ STP 0)
      (SETQ BIGSIZE 0)
      (SETQ SMALLSIZE 0)
      (SETQ U (CAR UU))
      (SETQ INPUT U)
      (SETQ BIGSIZE 0)
      (SETQ NARGS (LENGTH INPUT))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NARGS I)) (RETURN NIL)))
        (PROGN
         (SETQ ARG (CAR INPUT))
         (COND
          ((EQUAL (AEVAL* (LIST 'LENGTH ARG)) 1)
           (SETQ BIGSIZE (PLUS BIGSIZE 1)))
          (T
           (PROGN
            (SETQ BIGSIZE (PLUS BIGSIZE (CAR (SIZE_OF_MATRIX ARG))))
            NIL)))
         (SETQ INPUT (CDR INPUT))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ BIGA (MKMATRIX BIGSIZE BIGSIZE))
      (SETQ AIDX 1)
      (SETQ INPUT U)
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NARGS K)) (RETURN NIL)))
        (PROGN
         (SETQ ARG (CAR INPUT))
         (COND
          ((EQUAL (AEVAL* (LIST 'LENGTH ARG)) 1)
           (PROGN
            (SETMAT BIGA AIDX AIDX ARG)
            (SETQ AIDX (PLUS AIDX 1))
            (SETQ INPUT (CDR INPUT))
            NIL))
          (T
           (PROGN
            (SETQ SMALLSIZE (CAR (SIZE_OF_MATRIX ARG)))
            (SETQ STP (PLUS SMALLSIZE (DIFFERENCE AIDX 1)))
            (PROG (I)
              (SETQ I AIDX)
             LAB
              (COND ((MINUSP (DIFFERENCE STP I)) (RETURN NIL)))
              (PROGN
               (PROG (J)
                 (SETQ J AIDX)
                LAB
                 (COND ((MINUSP (DIFFERENCE STP J)) (RETURN NIL)))
                 (PROGN
                  (SETQ ARG (CAR INPUT))
                  (SETQ ARG (CDR ARG))
                  (PROGN
                   (SETQ N 1)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT (LESSP N (PLUS (DIFFERENCE I AIDX) 1)))
                       (RETURN NIL)))
                     (PROGN (SETQ ARG (CDR ARG)) (SETQ N (PLUS N 1)) NIL)
                     (GO WHILELABEL))
                   NIL)
                  (SETQ ARG (CAR ARG))
                  (PROGN
                   (SETQ N 1)
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT (LESSP N (PLUS (DIFFERENCE J AIDX) 1)))
                       (RETURN NIL)))
                     (PROGN (SETQ ARG (CDR ARG)) (SETQ N (PLUS N 1)) NIL)
                     (GO WHILELABEL))
                   NIL)
                  (SETQ ARG (CAR ARG))
                  (SETMAT BIGA I J ARG)
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ AIDX (PLUS AIDX SMALLSIZE))
            (SETQ INPUT (CDR INPUT))
            NIL)))
         NIL)
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (RETURN BIGA))) 
(PUT 'GET_COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'GET_COEFFS 'DEFINED-ON-LINE '812) 
(PUT 'GET_COEFFS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'GET_COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_COEFFS (POLY)
    (PROG (KER_LIST_NUM KER_LIST_DEN)
      (SETQ KER_LIST_NUM (KERNELS (*Q2F (SIMP (REVAL1 (NUM POLY) T)))))
      (SETQ KER_LIST_DEN (KERNELS (*Q2F (SIMP (REVAL1 (DEN POLY) T)))))
      (SETQ KER_LIST_NUM (UNION KER_LIST_NUM KER_LIST_DEN))
      (RETURN KER_LIST_NUM))) 
(PUT 'GET_QUO 'NUMBER-OF-ARGS 2) 
(PUT 'GET_QUO 'DEFINED-ON-LINE '830) 
(PUT 'GET_QUO 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'GET_QUO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_QUO (POLY1 POLY2)
    (PROG (QUO1 INPUT1 INPUT2)
      (COND ((AND (EQUAL INPUT1 0) (EQUAL INPUT2 0)) (RETURN 0))
            (T
             (PROGN
              (SETQ INPUT1 (REVAL1 POLY1 T))
              (SETQ INPUT2 (REVAL1 POLY2 T))
              (SETQ QUO1
                      (AEVAL
                       (LIST 'QUOTIENT
                             (LIST 'DIFFERENCE INPUT1
                                   (LIST 'REMAINDER INPUT1 INPUT2))
                             INPUT2)))
              (SETQ QUO1 (REVAL1 QUO1 T))
              (RETURN QUO1)
              NIL))))) 
(PUT 'GET_REM 'NUMBER-OF-ARGS 2) 
(PUT 'GET_REM 'DEFINED-ON-LINE '851) 
(PUT 'GET_REM 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'GET_REM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_REM (POLY1 POLY2)
    (PROG (REM1 INPUT1 INPUT2)
      (SETQ INPUT1 (REVAL1 POLY1 T))
      (SETQ INPUT2 (REVAL1 POLY2 T))
      (SETQ REM1 (AEVAL (LIST 'REMAINDER INPUT1 INPUT2)))
      (SETQ REM1 (REVAL1 REM1 T))
      (RETURN REM1))) 
(PUT 'INV 'NUMBER-OF-ARGS 2) 
(PUT 'INV 'DEFINED-ON-LINE '870) 
(PUT 'INV 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'INV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INV (U CARRIER)
    (PROG (UINV TMP N)
      (SETQ N 0)
      (SETQ N (CAR (SIZE_OF_MATRIX U)))
      (SETQ UINV (MKMATRIX N N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP 0)
            (PROG (K)
              (SETQ K J)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE I 1) K)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (LIST 'PLUS TMP
                             (LIST 'TIMES (GETMAT U I (GETV CARRIER K))
                                   (GETMAT UINV (GETV CARRIER K) J))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETMAT UINV (GETV CARRIER I) J
             (LIST 'QUOTIENT (LIST 'MINUS TMP) (GETMAT U I (GETV CARRIER I))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETMAT UINV (GETV CARRIER I) I
          (LIST 'QUOTIENT 1 (GETMAT U I (GETV CARRIER I))))
         (PROG (J)
           (SETQ J (PLUS I 1))
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (SETMAT UINV (GETV CARRIER I) J 0)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN UINV))) 
(PUT 'INVFACT_TO_FROBENIUS 'NUMBER-OF-ARGS 2) 
(PUT 'INVFACT_TO_FROBENIUS 'DEFINED-ON-LINE '910) 
(PUT 'INVFACT_TO_FROBENIUS 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'INVFACT_TO_FROBENIUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVFACT_TO_FROBENIUS (INV_FACT X)
    (PROG (DIAG_MAT TMP NUM)
      (SETQ NUM 0)
      (SETQ NUM (LENGTH INV_FACT))
      (SETQ TMP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NUM I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (COMPANION (NTH INV_FACT I) X) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE NUM I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (COMPANION (NTH INV_FACT I) X) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIAG_MAT (REVAL1 (LIST 'DIAGI TMP) T))
      (RETURN DIAG_MAT))) 
(PUT 'MAKE_IDENTITY 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_IDENTITY 'DEFINED-ON-LINE '928) 
(PUT 'MAKE_IDENTITY 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'MAKE_IDENTITY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_IDENTITY (ROW_DIM COL_DIM)
    (PROG (A)
      (SETQ A (MKMATRIX ROW_DIM COL_DIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COL_DIM J)) (RETURN NIL)))
           (PROGN (COND ((EQUAL I J) (SETMAT A I I 1))) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN A))) 
(PUT 'MATRIX_INPUT_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIX_INPUT_TEST 'DEFINED-ON-LINE '949) 
(PUT 'MATRIX_INPUT_TEST 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'MATRIX_INPUT_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIX_INPUT_TEST (A)
    (PROG ()
      (COND
       ((NOT (EQCAR A 'MAT))
        (REDERR (LIST "ERROR: `" A "' is non matrix input.")))
       (T (RETURN A))))) 
(PUT 'MYSMITH 'NUMBER-OF-ARGS 4) 
(PUT 'MYSMITH 'DEFINED-ON-LINE '959) 
(PUT 'MYSMITH 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'MYSMITH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MYSMITH (US L LINV X)
    (PROG (S A B G JJ S1 T1 TMP ISCLEAR Q LC POLY1 POLY2 INPUT1 INPUT2 N R)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ N (CAR (SIZE_OF_MATRIX US)))
      (SETQ S (COPY_MAT US))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
        (PROGN
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
               (COND ((EQUAL (GETMAT S I K) 0) (PROGN NIL))
                     (T
                      (PROGN
                       (SETQ POLY1 (GETMAT S K K))
                       (SETQ POLY2 (GETMAT S I K))
                       (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
                       (SETQ G (CAR TMP))
                       (SETQ S1 (CADR TMP))
                       (SETQ T1 (CADDR TMP))
                       (SETQ A (GET_QUO POLY1 G))
                       (SETQ B (GET_QUO POLY2 G))
                       (PROG (J)
                         (SETQ J (PLUS K 1))
                        LAB
                         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                         (PROGN
                          (SETQ INPUT1 (GETMAT S K J))
                          (SETQ INPUT2 (GETMAT S I J))
                          (SETQ TMP
                                  (LIST 'PLUS (LIST 'TIMES S1 INPUT1)
                                        (LIST 'TIMES T1 INPUT2)))
                          (SETMAT S I J
                           (LIST 'PLUS (LIST 'TIMES A INPUT2)
                                 (LIST 'MINUS (LIST 'TIMES B INPUT1))))
                          (SETMAT S K J TMP)
                          NIL)
                         (SETQ J (PLUS2 J 1))
                         (GO LAB))
                       (PROG (J)
                         (SETQ J 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP
                                  (REVAL1
                                   (LIST 'PLUS (LIST 'TIMES A (GETMAT L J K))
                                         (LIST 'TIMES B (GETMAT L J I)))
                                   T))
                          (SETMAT L J I
                           (REVAL1
                            (LIST 'PLUS
                                  (LIST 'TIMES (LIST 'MINUS T1) (GETMAT L J K))
                                  (LIST 'TIMES S1 (GETMAT L J I)))
                            T))
                          (SETMAT L J K TMP)
                          NIL)
                         (SETQ J (PLUS2 J 1))
                         (GO LAB))
                       (PROG (J)
                         (SETQ J 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                         (PROGN
                          (SETQ TMP
                                  (REVAL1
                                   (LIST 'PLUS
                                         (LIST 'TIMES S1 (GETMAT LINV K J))
                                         (LIST 'TIMES T1 (GETMAT LINV I J)))
                                   T))
                          (SETMAT LINV I J
                           (REVAL1
                            (LIST 'PLUS (LIST 'TIMES A (GETMAT LINV I J))
                                  (LIST 'TIMES (LIST 'MINUS B)
                                        (GETMAT LINV K J)))
                            T))
                          (SETMAT LINV K J TMP)
                          NIL)
                         (SETQ J (PLUS2 J 1))
                         (GO LAB))
                       (SETMAT S K K G)
                       (SETMAT S I K 0)
                       NIL)))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ ISCLEAR T)
            (PROG (I)
              (SETQ I (PLUS K 1))
             LAB
              (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
              (PROGN
               (SETQ POLY1 (GETMAT S K I))
               (SETQ POLY2 (GETMAT S K K))
               (SETMAT S K I (GET_REM POLY1 POLY2))
               (SETQ Q (GET_QUO POLY1 POLY2))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (PROG (I)
              (SETQ I (PLUS K 1))
             LAB
              (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
              (PROGN
               (COND ((EQUAL (GETMAT S K I) 0) (PROGN NIL))
                     (T
                      (PROGN
                       (SETQ POLY1 (GETMAT S K K))
                       (SETQ POLY2 (GETMAT S K I))
                       (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
                       (SETQ G (CAR TMP))
                       (SETQ S1 (CADR TMP))
                       (SETQ T1 (CADDR TMP))
                       (SETQ A (GET_QUO POLY1 G))
                       (SETQ B (GET_QUO POLY2 G))
                       (PROG (J)
                         (SETQ J (PLUS K 1))
                        LAB
                         (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                         (PROGN
                          (SETQ INPUT1 (GETMAT S J K))
                          (SETQ INPUT2 (GETMAT S J I))
                          (SETQ TMP
                                  (LIST 'PLUS (LIST 'TIMES S1 INPUT1)
                                        (LIST 'TIMES T1 INPUT2)))
                          (SETMAT S J I
                           (LIST 'PLUS (LIST 'TIMES A INPUT2)
                                 (LIST 'MINUS (LIST 'TIMES B INPUT1))))
                          (SETMAT S J K TMP)
                          NIL)
                         (SETQ J (PLUS2 J 1))
                         (GO LAB))
                       (SETMAT S K K G)
                       (SETMAT S K I 0)
                       (SETQ ISCLEAR NIL)
                       NIL)))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL)
           (GO WHILELABEL))
         NIL)
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ R 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (COND
          ((NEQ (GETMAT S I I) 0)
           (PROGN
            (SETQ R (PLUS R 1))
            (COND
             ((EQUAL (OFF_MOD_LCOF (GETMAT S I I) X) 0)
              (SETQ LC (GETMAT S I I)))
             (T (SETQ LC (OFF_MOD_LCOF (GETMAT S I I) X))))
            (SETMAT S R R (LIST 'QUOTIENT (GETMAT S I I) LC))
            (COND
             ((NEQ I R)
              (PROGN
               (SETMAT S I I 0)
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP (REVAL1 (GETMAT L J R) T))
                  (SETMAT L J R (REVAL1 (GETMAT L I J) T))
                  (SETMAT L J I TMP)
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (PROGN
                  (SETQ TMP (REVAL1 (GETMAT LINV R J) T))
                  (SETMAT LINV R J (REVAL1 (GETMAT LINV I J) T))
                  (SETMAT LINV I J TMP)
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
         (SETQ JJ (PLUS I 1))
         (PROGN
          (PROG ()
           WHILELABEL
            (COND
             ((NOT (AND (NEQ (REVAL1 (GETMAT S I I) T) 1) (LEQ JJ R)))
              (RETURN NIL)))
            (PROGN
             (SETQ POLY1 (REVAL1 (GETMAT S I I) T))
             (SETQ POLY2 (REVAL1 (GETMAT S JJ JJ) T))
             (SETQ TMP (CALC_EXGCD POLY1 POLY2 X))
             (SETQ G (CAR TMP))
             (SETQ S1 (CADR TMP))
             (SETQ T1 (CADDR TMP))
             (SETQ A (GET_QUO POLY1 G))
             (SETQ B (GET_QUO POLY2 G))
             (SETMAT S I I G)
             (SETMAT S JJ JJ (LIST 'TIMES A POLY2))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
               (PROGN
                (SETQ TMP
                        (REVAL1
                         (LIST 'PLUS (LIST 'TIMES A (GETMAT L K I))
                               (LIST 'TIMES B (GETMAT L K JJ)))
                         T))
                (SETMAT L K JJ
                 (REVAL1
                  (LIST 'PLUS (LIST 'TIMES (LIST 'MINUS T1) (GETMAT L K I))
                        (LIST 'TIMES S1 (GETMAT L K JJ)))
                  T))
                (SETMAT L K I TMP)
                NIL)
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
               (PROGN
                (SETQ TMP
                        (REVAL1
                         (LIST 'PLUS (LIST 'TIMES S1 (GETMAT LINV I K))
                               (LIST 'TIMES T1 (GETMAT LINV JJ K)))
                         T))
                (SETMAT LINV JJ K
                 (REVAL1
                  (LIST 'PLUS (LIST 'TIMES A (GETMAT LINV JJ K))
                        (LIST 'TIMES (LIST 'MINUS B) (GETMAT LINV I K)))
                  T))
                (SETMAT LINV I K TMP)
                NIL)
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (SETQ JJ (PLUS JJ 1))
             NIL)
            (GO WHILELABEL))
          NIL)
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST S L LINV)))) 
(PUT 'NEST_INPUT 'NUMBER-OF-ARGS 1) 
(PUT 'NEST_INPUT 'DEFINED-ON-LINE '1154) 
(PUT 'NEST_INPUT 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'NEST_INPUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEST_INPUT (A)
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
            (SETQ COEFF_LIST (GET_COEFFS (GETMAT AA I J)))
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
(PUT 'OFF_MOD_LCOF 'NUMBER-OF-ARGS 2) 
(PUT 'OFF_MOD_LCOF 'DEFINED-ON-LINE '1194) 
(PUT 'OFF_MOD_LCOF 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'OFF_MOD_LCOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFF_MOD_LCOF (INPUT X)
    (PROG ()
      (COND
       (*MODULAR
        (PROGN
         (OFF (LIST 'MODULAR))
         (SETQ INPUT (LCOF INPUT X))
         (ON (LIST 'MODULAR))
         NIL))
       (T (SETQ INPUT (LCOF INPUT X))))
      (RETURN INPUT))) 
(PUT 'OFF_MOD_REVAL 'NUMBER-OF-ARGS 1) 
(PUT 'OFF_MOD_REVAL 'DEFINED-ON-LINE '1209) 
(PUT 'OFF_MOD_REVAL 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'OFF_MOD_REVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFF_MOD_REVAL (INPUT)
    (PROG ()
      (COND
       (*MODULAR
        (PROGN
         (OFF (LIST 'MODULAR))
         (SETQ INPUT (REVAL1 INPUT T))
         (ON (LIST 'MODULAR))
         NIL))
       (T (SETQ INPUT (REVAL1 INPUT T))))
      (RETURN INPUT))) 
(FLAG '(OFF_MOD_REVAL) 'OPFN) 
(PUT 'PLIST_TO_POLYCOMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'PLIST_TO_POLYCOMPANION 'DEFINED-ON-LINE '1230) 
(PUT 'PLIST_TO_POLYCOMPANION 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'PLIST_TO_POLYCOMPANION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLIST_TO_POLYCOMPANION (PLIST X)
    (PROG (D A R N)
      (SETQ R 0)
      (SETQ N 0)
      (SETQ R (LENGTH PLIST))
      (SETQ D (MKVECT R))
      (PUTV D 0 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PUTV D I (DEG (NTH PLIST I) X))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ N (GETV D R))
      (SETQ A (MKMATRIX N N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J (PLUS (GETV D (DIFFERENCE I 1)) 2))
          LAB
           (COND ((MINUSP (DIFFERENCE (GETV D I) J)) (RETURN NIL)))
           (SETMAT A J (DIFFERENCE J 1) 1)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (J)
           (SETQ J I)
          LAB
           (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
           (PROGN
            (PROG (K)
              (SETQ K (PLUS (GETV D (DIFFERENCE I 1)) 1))
             LAB
              (COND ((MINUSP (DIFFERENCE (GETV D I) K)) (RETURN NIL)))
              (PROGN
               (SETMAT A K (GETV D J)
                (LIST 'MINUS (COEFFN (NTH PLIST J) X (DIFFERENCE K 1))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN A))) 
(PUT 'SIZE_OF_MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'SIZE_OF_MATRIX 'DEFINED-ON-LINE '1280) 
(PUT 'SIZE_OF_MATRIX 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'SIZE_OF_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIZE_OF_MATRIX (A)
    (PROG (ROW_DIM COL_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COL_DIM 0)
      (MATRIX_INPUT_TEST A)
      (SETQ ROW_DIM (PLUS (MINUS 1) (LENGTH A)))
      (SETQ COL_DIM (LENGTH (CADR A)))
      (RETURN (LIST ROW_DIM COL_DIM)))) 
(PUT 'UPPERSMITH 'NUMBER-OF-ARGS 2) 
(PUT 'UPPERSMITH 'DEFINED-ON-LINE '1295) 
(PUT 'UPPERSMITH 'DEFINED-IN-FILE 'NORMFORM/FROBEN.RED) 
(PUT 'UPPERSMITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UPPERSMITH (G X)
    (PROG (US L LINV S1 T1 Q R TMP N)
      (SETQ N 0)
      (SETQ N (CAR (SIZE_OF_MATRIX G)))
      (SETQ US (COPY_MAT G))
      (SETQ L (MAKE_IDENTITY N N))
      (SETQ LINV (MAKE_IDENTITY N N))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE J 1) I)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (CALC_EXGCD (GETMAT US I I) (GETMAT US J J) X))
            (SETQ G (CAR TMP))
            (SETQ S1 (CADR TMP))
            (SETQ T1 (CADDR TMP))
            (SETQ Q (GET_QUO (GETMAT US I J) G))
            (SETQ R (GET_REM (GETMAT US I J) G))
            (SETMAT US I J R)
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE I 1) K)) (RETURN NIL)))
              (PROGN
               (SETQ TMP (GETMAT US K I))
               (SETMAT US K J
                (LIST 'PLUS (GETMAT US K J)
                      (LIST 'TIMES (LIST 'MINUS Q) S1 (GETMAT US K I))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K (PLUS J 1))
             LAB
              (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
              (PROGN
               (SETMAT US I K
                (LIST 'PLUS (GETMAT US I K)
                      (LIST 'TIMES (LIST 'MINUS Q) T1 (GETMAT US J K))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE I K)) (RETURN NIL)))
              (PROGN
               (SETMAT L K J
                (LIST 'PLUS (GETMAT L K J) (LIST 'TIMES Q T1 (GETMAT L K I))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETMAT LINV I J (LIST 'TIMES (LIST 'MINUS Q) T1))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN (LIST US L LINV)))) 
(ENDMODULE) 