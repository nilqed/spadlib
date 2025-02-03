(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RATJORD)) 
(NULL (LOAD-PACKAGE 'SPECFN)) 
(PUT 'RATJORDAN 'NUMBER-OF-ARGS 1) 
(PUT 'RATJORDAN 'DEFINED-ON-LINE '49) 
(PUT 'RATJORDAN 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'RATJORDAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATJORDAN (A)
    (PROG (AA TMP ANS P PINV FULL_COEFF_LIST RULE_LIST INPUT_MODE)
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
      (SETQ TMP (RATJORDANFORM AA FULL_COEFF_LIST))
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
(FLAG '(RATJORDAN) 'OPFN) 
(PUT 'RATJORDANFORM 'NUMBER-OF-ARGS 2) 
(PUT 'RATJORDANFORM 'DEFINED-ON-LINE '111) 
(PUT 'RATJORDANFORM 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'RATJORDANFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATJORDANFORM (A FULL_COEFF_LIST)
    (PROG (TMP F TT TINV PRIM_INV S SINV P PINV X)
      (SETQ X (MKID 'X 0))
      (SETQ TMP (FROBENIUSFORM A))
      (SETQ F (CAR TMP))
      (SETQ TT (CADR TMP))
      (SETQ TINV (CADDR TMP))
      (SETQ TMP (FROBENIUS_TO_RATJORDAN F FULL_COEFF_LIST X))
      (SETQ PRIM_INV (CAR TMP))
      (SETQ S (CADR TMP))
      (SETQ SINV (CADDR TMP))
      (SETQ P (REVAL1 (LIST 'TIMES TT S) T))
      (SETQ PINV (REVAL1 (LIST 'TIMES SINV TINV) T))
      (SETQ PRIM_INV (PRIMINV_TO_RATJORDAN PRIM_INV X))
      (RETURN (LIST PRIM_INV P PINV)))) 
(PUT 'COMPANION_TO_RATJORDAN 'NUMBER-OF-ARGS 3) 
(PUT 'COMPANION_TO_RATJORDAN 'DEFINED-ON-LINE '233) 
(PUT 'COMPANION_TO_RATJORDAN 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'COMPANION_TO_RATJORDAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPANION_TO_RATJORDAN (FACT_LIST F X)
    (PROG (G_LIST U_LIST BBASIS Q1 E QPOWER DIFFQ PART_BASIS RATJ_BASIS S TT G
           ROWQINV POL_LINCOMB QQ RR LINCOMB INDEX1 V U A TMP QINV Q SUM1 R N
           D)
      (SETQ R 0)
      (SETQ N 0)
      (SETQ D 0)
      (SETQ R (LENGTH FACT_LIST))
      (SETQ N (DEG F X))
      (SETQ G_LIST
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1
                                  (LIST 'EXPT (NTH (NTH FACT_LIST I) 1)
                                        (NTH (NTH FACT_LIST I) 2))
                                  T)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE R I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (REVAL1
                          (LIST 'EXPT (NTH (NTH FACT_LIST I) 1)
                                (NTH (NTH FACT_LIST I) 2))
                          T)
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U_LIST (MKVECT R))
      (COND ((EQUAL R 1) (PUTV U_LIST 1 1))
            (T
             (PROGN
              (SETQ TMP (CALC_EXGCD (NTH G_LIST 1) (NTH G_LIST 2) X))
              (SETQ S (CADR TMP))
              (SETQ TT (CADDR TMP))
              (PUTV U_LIST 1 (LIST 'TIMES TT (NTH G_LIST 2)))
              (PUTV U_LIST 2 (LIST 'TIMES S (NTH G_LIST 1)))
              (SETQ G (LIST 'TIMES (NTH G_LIST 1) (NTH G_LIST 2)))
              (PROG (I)
                (SETQ I 3)
               LAB
                (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
                (PROGN
                 (SETQ TMP (CALC_EXGCD G (NTH G_LIST I) X))
                 (SETQ S (CADR TMP))
                 (SETQ TT (CADDR TMP))
                 (PROG (J)
                   (SETQ J 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J)) (RETURN NIL)))
                   (PROGN
                    (PUTV U_LIST J
                          (GET_REM
                           (LIST 'TIMES (GETV U_LIST J) TT (NTH G_LIST I)) F))
                    NIL)
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 (PUTV U_LIST I (LIST 'TIMES S G))
                 (SETQ G (LIST 'TIMES G (NTH G_LIST I)))
                 NIL)
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              NIL)))
      (SETQ BBASIS (LIST))
      (SETQ ROWQINV 0)
      (SETQ Q (MKMATRIX N N))
      (SETQ QINV (MKMATRIX N N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ Q1 (NTH (NTH FACT_LIST I) 1))
         (SETQ E (REVAL1 (NTH (NTH FACT_LIST I) 2) T))
         (SETQ D (DEG Q1 X))
         (SETQ QPOWER (MKVECT (PLUS E 1)))
         (PUTV QPOWER 1 1)
         (PROG (J)
           (SETQ J 2)
          LAB
           (COND ((MINUSP (DIFFERENCE (PLUS E 1) J)) (RETURN NIL)))
           (PROGN
            (PUTV QPOWER J (LIST 'TIMES Q1 (GETV QPOWER (DIFFERENCE J 1))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND
          ((GREATERP E 1)
           (PROGN
            (SETQ DIFFQ (MKVECT (DIFFERENCE E 1)))
            (PUTV DIFFQ 1 (REVAL1 (AEVAL* (LIST 'DF Q1 X)) T))
            (PROG (J)
              (SETQ J 2)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE E 1) J)) (RETURN NIL)))
              (PROGN
               (SETQ TMP (REVAL1 (GETV DIFFQ (DIFFERENCE J 1)) T))
               (PUTV DIFFQ J (REVAL1 (AEVAL* (LIST 'DF TMP X)) T))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL)))
         (SETQ PART_BASIS (MKVECT E))
         (PUTV PART_BASIS 1 (REVAL1 (LIST 'EXPT Q1 (DIFFERENCE E 1)) T))
         (PROG (J)
           (SETQ J 2)
          LAB
           (COND ((MINUSP (DIFFERENCE E J)) (RETURN NIL)))
           (PROGN
            (SETQ SUM1 0)
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE J 1) K)) (RETURN NIL)))
              (PROGN
               (SETQ TMP
                       (REVAL1
                        (LIST 'TIMES
                              (REVAL1
                               (LIST 'QUOTIENT
                                     (REVAL1
                                      (LIST 'EXPT (MINUS 1) (DIFFERENCE K 1))
                                      T)
                                     (REVAL1 (LIST 'FACTORIAL K) T))
                               T)
                              (REVAL1 (GETV DIFFQ K) T)
                              (REVAL1 (GETV PART_BASIS (DIFFERENCE J K)) T))
                        T))
               (SETQ SUM1 (REVAL1 (LIST 'PLUS SUM1 TMP) T))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (PUTV PART_BASIS J (REVAL1 (LIST 'QUOTIENT SUM1 Q1) T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ RATJ_BASIS (MKVECT (TIMES E D)))
         (PUTV RATJ_BASIS 1 (GETV PART_BASIS 1))
         (PROG (K)
           (SETQ K 2)
          LAB
           (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
           (PROGN
            (PUTV RATJ_BASIS K
                  (LIST 'TIMES X (GETV RATJ_BASIS (DIFFERENCE K 1))))
            NIL)
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (PROG (J)
           (SETQ J 2)
          LAB
           (COND ((MINUSP (DIFFERENCE E J)) (RETURN NIL)))
           (PROGN
            (PUTV RATJ_BASIS (PLUS (TIMES (DIFFERENCE J 1) D) 1)
                  (GETV PART_BASIS J))
            (PROG (K)
              (SETQ K 2)
             LAB
              (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
              (PROGN
               (PUTV RATJ_BASIS (PLUS (TIMES (DIFFERENCE J 1) D) K)
                     (LIST 'PLUS
                           (LIST 'TIMES X
                                 (GETV RATJ_BASIS
                                       (PLUS (TIMES (DIFFERENCE J 1) D)
                                             (DIFFERENCE K 1))))
                           (LIST 'MINUS
                                 (GETV RATJ_BASIS
                                       (PLUS (TIMES (DIFFERENCE J 2) D)
                                             (DIFFERENCE K 1))))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (TIMES E D) K)) (RETURN NIL)))
           (PROGN
            (SETQ TT
                    (GET_REM (LIST 'TIMES (GETV U_LIST I) (GETV RATJ_BASIS K))
                     F))
            (SETQ BBASIS (APPEND BBASIS (LIST TT)))
            NIL)
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (SETQ POL_LINCOMB (MKVECT E))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE E J)) (RETURN NIL)))
           (PUTV POL_LINCOMB J 0)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ TMP (CALC_EXGCD (GETV PART_BASIS E) (GETV QPOWER (PLUS E 1)) X))
         (SETQ S (CADR TMP))
         (SETQ TT (CADDR TMP))
         (PUTV POL_LINCOMB E S)
         (PROG (J)
           (SETQ J E)
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
           (PROGN
            (SETQ QQ (GET_QUO (GETV POL_LINCOMB J) Q1))
            (SETQ RR (GET_REM (GETV POL_LINCOMB J) Q1))
            (PUTV POL_LINCOMB J RR)
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE J 1) K)) (RETURN NIL)))
              (PROGN
               (PUTV POL_LINCOMB (DIFFERENCE J K)
                     (GET_REM
                      (LIST 'PLUS (GETV POL_LINCOMB (DIFFERENCE J K))
                            (LIST 'TIMES QQ (GETV DIFFQ K)
                                  (LIST 'EXPT (MINUS 1)
                                        (LIST 'QUOTIENT K
                                              (LIST 'FACTORIAL K)))))
                      (GETV QPOWER (PLUS J 1))))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         (SETQ LINCOMB (MKVECT (TIMES E D)))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE E J)) (RETURN NIL)))
           (PROGN
            (PROG (K)
              (SETQ K 1)
             LAB
              (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
              (PROGN
               (SETQ INDEX1 (PLUS (TIMES (DIFFERENCE J 1) D) K))
               (PUTV LINCOMB INDEX1
                     (COEFFN (GETV POL_LINCOMB J) X (DIFFERENCE K 1)))
               (PROG (V)
                 (SETQ V 1)
                LAB
                 (COND
                  ((MINUSP
                    (DIFFERENCE (MIN (DIFFERENCE J 1) (DIFFERENCE K 1)) V))
                   (RETURN NIL)))
                 (PROGN
                  (PUTV LINCOMB (DIFFERENCE (DIFFERENCE INDEX1 (TIMES V D)) V)
                        (REVAL1
                         (LIST 'PLUS
                               (GETV LINCOMB
                                     (DIFFERENCE
                                      (DIFFERENCE INDEX1 (TIMES V D)) V))
                               (LIST 'TIMES
                                     (COEFFN (GETV POL_LINCOMB J) X
                                             (DIFFERENCE K 1))
                                     (BINOMIAL (DIFFERENCE K 1) V)))
                         T))
                  NIL)
                 (SETQ V (PLUS2 V 1))
                 (GO LAB))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PROG (U)
           (SETQ U 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (TIMES E D) U)) (RETURN NIL)))
           (PROGN
            (SETQ ROWQINV (PLUS ROWQINV 1))
            (SETMAT QINV ROWQINV 1 (GETV LINCOMB U))
            NIL)
           (SETQ U (PLUS2 U 1))
           (GO LAB))
         (PROG (V)
           (SETQ V 2)
          LAB
           (COND ((MINUSP (DIFFERENCE N V)) (RETURN NIL)))
           (PROGN
            (SETQ A (MKVECT (UPBV LINCOMB)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (UPBV LINCOMB) I)) (RETURN NIL)))
              (PROGN (PUTV A I (GETV LINCOMB I)) NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ INDEX1 0)
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE E 1) J)) (RETURN NIL)))
              (PROGN
               (SETQ INDEX1 (PLUS INDEX1 1))
               (PUTV LINCOMB INDEX1
                     (REVAL1
                      (LIST 'PLUS
                            (LIST 'TIMES (LIST 'MINUS (COEFFN Q1 X 0))
                                  (GETV A (TIMES J D)))
                            (GETV A (PLUS (TIMES J D) 1)))
                      T))
               (PROG (K)
                 (SETQ K 2)
                LAB
                 (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
                 (PROGN
                  (SETQ INDEX1 (PLUS INDEX1 1))
                  (PUTV LINCOMB INDEX1
                        (REVAL1
                         (LIST 'PLUS
                               (LIST 'PLUS
                                     (GETV A
                                           (PLUS (TIMES (DIFFERENCE J 1) D)
                                                 (DIFFERENCE K 1)))
                                     (LIST 'TIMES
                                           (LIST 'MINUS
                                                 (COEFFN Q1 X
                                                         (DIFFERENCE K 1)))
                                           (GETV A (TIMES J D)))
                                     (GETV A (PLUS (TIMES J D) K))))
                         T))
                  NIL)
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (SETQ INDEX1 (PLUS INDEX1 1))
            (PUTV LINCOMB INDEX1
                  (REVAL1
                   (LIST 'TIMES (LIST 'MINUS (COEFFN Q1 X 0))
                         (REVAL1 (GETV A (TIMES E D)) T))
                   T))
            (PROG (K)
              (SETQ K 2)
             LAB
              (COND ((MINUSP (DIFFERENCE D K)) (RETURN NIL)))
              (PROGN
               (SETQ INDEX1 (PLUS INDEX1 1))
               (PUTV LINCOMB INDEX1
                     (REVAL1
                      (LIST 'PLUS
                            (GETV A
                                  (PLUS (TIMES (DIFFERENCE E 1) D)
                                        (DIFFERENCE K 1)))
                            (LIST 'TIMES
                                  (LIST 'MINUS (COEFFN Q1 X (DIFFERENCE K 1)))
                                  (GETV A (TIMES E D))))
                      T))
               NIL)
              (SETQ K (PLUS2 K 1))
              (GO LAB))
            (SETQ ROWQINV (DIFFERENCE ROWQINV (TIMES E D)))
            (PROG (U)
              (SETQ U 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (TIMES E D) U)) (RETURN NIL)))
              (PROGN
               (SETQ ROWQINV (PLUS ROWQINV 1))
               (SETMAT QINV ROWQINV V (GETV LINCOMB U))
               NIL)
              (SETQ U (PLUS2 U 1))
              (GO LAB))
            NIL)
           (SETQ V (PLUS2 V 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
           (PROGN
            (SETMAT Q K J (COEFFN (NTH BBASIS J) X (DIFFERENCE K 1)))
            NIL)
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN (LIST Q QINV)))) 
(PUT 'CONVERT_TO_MULT 'NUMBER-OF-ARGS 2) 
(PUT 'CONVERT_TO_MULT 'DEFINED-ON-LINE '487) 
(PUT 'CONVERT_TO_MULT 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'CONVERT_TO_MULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONVERT_TO_MULT (FACLIST X)
    (PROG (MULTLIST Z MULT1)
      (SETQ MULT1 0)
      (SETQ FACLIST (CDR FACLIST))
      (COND ((NUMBERP (CAR FACLIST)) (SETQ FACLIST (CDR FACLIST))))
      (SETQ MULTLIST (LIST))
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE (PLUS (LENGTH FACLIST) 1) I)) (RETURN NIL)))
        (PROGN
         (SETQ MULT1 1)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (LEQ I (LENGTH FACLIST))
                   (NUMBERP
                    (SETQ Z
                            (REVAL1
                             (LIST 'QUOTIENT (NTH FACLIST (DIFFERENCE I 1))
                                   (NTH FACLIST I))
                             T)))
                   (EQUAL (ABS Z) 1)))
             (RETURN NIL)))
           (PROGN (SETQ MULT1 (PLUS MULT1 1)) (SETQ I (PLUS I 1)) NIL)
           (GO WHILELABEL))
         (COND
          ((AND (NUMBERP (OFF_MOD_LCOF (NTH FACLIST (DIFFERENCE I 1)) X))
                (NEQ (OFF_MOD_LCOF (NTH FACLIST (DIFFERENCE I 1)) X) 0))
           (PROGN
            (SETQ MULTLIST
                    (APPEND MULTLIST
                            (LIST
                             (LIST
                              (REVAL1
                               (LIST 'QUOTIENT (NTH FACLIST (DIFFERENCE I 1))
                                     (OFF_MOD_LCOF
                                      (NTH FACLIST (DIFFERENCE I 1)) X))
                               T)
                              MULT1))))
            NIL))
          ((EQUAL (CAR (NTH FACLIST (DIFFERENCE I 1))) 'MINUS)
           (PROGN
            (SETQ MULTLIST
                    (APPEND MULTLIST
                            (LIST
                             (LIST (CADR (NTH FACLIST (DIFFERENCE I 1)))
                                   MULT1))))
            NIL))
          (T
           (SETQ MULTLIST
                   (APPEND MULTLIST
                           (LIST
                            (LIST (NTH FACLIST (DIFFERENCE I 1)) MULT1))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN MULTLIST))) 
(PUT 'COPYINTO 'NUMBER-OF-ARGS 4) 
(PUT 'COPYINTO 'DEFINED-ON-LINE '552) 
(PUT 'COPYINTO 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'COPYINTO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COPYINTO (BB AA P Q)
    (PROG (A B M N R C)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ C 0)
      (MATRIX_INPUT_TEST AA)
      (MATRIX_INPUT_TEST BB)
      (COND
       ((OR (EQUAL P 0) (EQUAL Q 0))
        (REDERR "     0 is out of bounds for matrices.
     The top left element is labelled (1,1) and not (0,0).")))
      (SETQ M (CAR (SIZE_OF_MATRIX AA)))
      (SETQ N (CADR (SIZE_OF_MATRIX AA)))
      (SETQ R (CAR (SIZE_OF_MATRIX BB)))
      (SETQ C (CADR (SIZE_OF_MATRIX BB)))
      (COND
       ((OR (GREATERP (PLUS R (DIFFERENCE P 1)) M)
            (GREATERP (PLUS C (DIFFERENCE Q 1)) N))
        (REDERR
         (LIST "The matrix" BB "does not fit into" AA "at position" P Q "."))))
      (SETQ A (MKMATRIX M N))
      (SETQ B (MKMATRIX R C))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN (SETMAT A I J (GETMAT AA I J)) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE C J)) (RETURN NIL)))
           (PROGN (SETMAT B I J (GETMAT BB I J)) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE C J)) (RETURN NIL)))
           (PROGN
            (SETMAT A (PLUS P (DIFFERENCE I 1)) (PLUS Q (DIFFERENCE J 1))
             (GETMAT B I J))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN A))) 
(FLAG '(COPYINTO) 'OPFN) 
(PUT 'DE_NEST_LIST 'NUMBER-OF-ARGS 2) 
(PUT 'DE_NEST_LIST 'DEFINED-ON-LINE '613) 
(PUT 'DE_NEST_LIST 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'DE_NEST_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DE_NEST_LIST (INPUT FULL_COEFF_LIST)
    (PROG (TMP COPY RULE_LIST)
      (COND ((EQUAL FULL_COEFF_LIST NIL) (SETQ COPY INPUT))
            (T
             (PROGN
              (SETQ COPY INPUT)
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
              (COND ((ATOM COPY) (SETQ COPY (REVAL1 COPY T)))
                    (T
                     (SETQ COPY
                             (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                               (SETQ ELT COPY)
                               (COND ((NULL ELT) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (ELT) (REVAL1 ELT T))
                                                 (CAR ELT))
                                                NIL)))
                              LOOPLABEL
                               (SETQ ELT (CDR ELT))
                               (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ELT) (REVAL1 ELT T))
                                         (CAR ELT))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
              (CLEARRULES (LIST RULE_LIST))
              NIL)))
      (RETURN COPY))) 
(PUT 'DEG_SORT 'NUMBER-OF-ARGS 2) 
(PUT 'DEG_SORT 'DEFINED-ON-LINE '648) 
(PUT 'DEG_SORT 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'DEG_SORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEG_SORT (L X)
    (PROG (LL ALG N)
      (SETQ N 0)
      (COND ((EQUAL (CAR L) 'LIST) (PROGN (SETQ LL (CDR L)) (SETQ ALG T) NIL))
            (T (SETQ LL L)))
      (SETQ N (LENGTH LL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J (PLUS I 1))
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (COND
             ((LESSP (DEG (NTH LL J) X) (DEG (NTH LL I) X))
              (PROGN
               (SETQ LL
                       (APPEND
                        (APPEND
                         (APPEND
                          (PROG (K FORALL-RESULT FORALL-ENDPTR)
                            (SETQ K 1)
                            (COND
                             ((MINUSP (DIFFERENCE (DIFFERENCE I 1) K))
                              (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR (CONS (NTH LL K) NIL)))
                           LOOPLABEL
                            (SETQ K (PLUS2 K 1))
                            (COND
                             ((MINUSP (DIFFERENCE (DIFFERENCE I 1) K))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR (CONS (NTH LL K) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          (LIST (NTH LL J)))
                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                           (SETQ K I)
                           (COND
                            ((MINUSP (DIFFERENCE (DIFFERENCE J 1) K))
                             (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS (NTH LL K) NIL)))
                          LOOPLABEL
                           (SETQ K (PLUS2 K 1))
                           (COND
                            ((MINUSP (DIFFERENCE (DIFFERENCE J 1) K))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS (NTH LL K) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                        (PROG (K FORALL-RESULT FORALL-ENDPTR)
                          (SETQ K (PLUS J 1))
                          (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR (CONS (NTH LL K) NIL)))
                         LOOPLABEL
                          (SETQ K (PLUS2 K 1))
                          (COND
                           ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR (CONS (NTH LL K) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
               NIL)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND (ALG (SETQ LL (APPEND (LIST 'LIST) LL))))
      (RETURN LL))) 
(FLAG '(DEG_SORT) 'OPFN) 
(PUT 'FROBENIUS_TO_INVFACT 'NUMBER-OF-ARGS 2) 
(PUT 'FROBENIUS_TO_INVFACT 'DEFINED-ON-LINE '697) 
(PUT 'FROBENIUS_TO_INVFACT 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'FROBENIUS_TO_INVFACT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FROBENIUS_TO_INVFACT (F X)
    (PROG (P INV_FACT ROW_DIM M K)
      (SETQ ROW_DIM 0)
      (SETQ M 0)
      (SETQ K 0)
      (SETQ ROW_DIM (CAR (SIZE_OF_MATRIX F)))
      (SETQ INV_FACT (LIST))
      (SETQ K 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ K ROW_DIM)) (RETURN NIL)))
        (PROGN
         (SETQ P 0)
         (SETQ M (PLUS K 1))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (LEQ M ROW_DIM) (EQUAL (GETMAT F M (DIFFERENCE M 1)) 1)))
             (RETURN NIL)))
           (SETQ M (PLUS M 1))
           (GO WHILELABEL))
         (PROG (J)
           (SETQ J K)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE M 1) J)) (RETURN NIL)))
           (PROGN
            (SETQ P
                    (REVAL1
                     (LIST 'PLUS P
                           (LIST 'TIMES
                                 (LIST 'MINUS (GETMAT F J (DIFFERENCE M 1)))
                                 (LIST 'EXPT X (DIFFERENCE J K))))
                     T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ P (REVAL1 (LIST 'PLUS P (LIST 'EXPT X (DIFFERENCE M K))) T))
         (SETQ INV_FACT (APPEND INV_FACT (LIST P)))
         (SETQ K M)
         NIL)
        (GO WHILELABEL))
      (RETURN INV_FACT))) 
(PUT 'FROBENIUS_TO_RATJORDAN 'NUMBER-OF-ARGS 3) 
(PUT 'FROBENIUS_TO_RATJORDAN 'DEFINED-ON-LINE '732) 
(PUT 'FROBENIUS_TO_RATJORDAN 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'FROBENIUS_TO_RATJORDAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FROBENIUS_TO_RATJORDAN (F FULL_COEFF_LIST X)
    (PROG (INV_FACT GG L M H P FACT_MAT G II PP FF J T_LIST TINV_LIST FACTS TMP
           Q QINV DEGP D TT S COLS COUNT TINV SINV EXP_LIST PRIM_INV NN PROD R
           N)
      (SETQ R 0)
      (SETQ N 0)
      (SETQ INV_FACT (FROBENIUS_TO_INVFACT F X))
      (SETQ R (LENGTH INV_FACT))
      (SETQ GG
              (APPEND (LIST (NTH INV_FACT 1))
                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                        (SETQ I 2)
                        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         (GET_QUO (NTH INV_FACT I)
                                          (NTH INV_FACT (DIFFERENCE I 1)))
                                         NIL)))
                       LOOPLABEL
                        (SETQ I (PLUS2 I 1))
                        (COND
                         ((MINUSP (DIFFERENCE R I)) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 (GET_QUO (NTH INV_FACT I)
                                  (NTH INV_FACT (DIFFERENCE I 1)))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (SETQ L (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ PROD 1)
         (PROG (J)
           (SETQ J 0)
          LAB
           (COND ((MINUSP (DIFFERENCE (DEG (NTH GG I) X) J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP (DE_NEST (REVAL1 (COEFFN (NTH GG I) X J) T)))
            (COND
             ((EVALNUMBERP TMP)
              (PROGN
               (COND ((QUO_TEST TMP) (SETQ TMP (GET_DEN TMP)))
                     (T (SETQ TMP 1)))
               NIL))
             (T (PROGN (SETQ TMP (DEN TMP)) NIL)))
            (SETQ PROD (REVAL1 (LIST 'TIMES TMP PROD) T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ M PROD)
         (COND ((AND (EQUAL M 1) (EQUAL (NTH GG I) 1)) (SETQ H (LIST)))
               ((EQUAL M 1)
                (PROGN
                 (SETQ TMP (DE_NEST_LIST (NTH GG I) FULL_COEFF_LIST))
                 (SETQ TMP (OLD_FACTORIZE TMP))
                 (SETQ TMP (RE_NEST_LIST TMP FULL_COEFF_LIST))
                 (SETQ H (CONVERT_TO_MULT TMP X))
                 NIL))
               (T
                (PROGN
                 (SETQ TMP (REVAL1 (LIST 'TIMES (LIST 'CO 2 M) (NTH GG I)) T))
                 (SETQ TMP (DE_NEST_LIST TMP FULL_COEFF_LIST))
                 (SETQ TMP (OLD_FACTORIZE TMP))
                 (SETQ TMP (RE_NEST_LIST TMP FULL_COEFF_LIST))
                 (SETQ H (CONVERT_TO_MULT TMP X))
                 NIL)))
         (SETQ L
                 (APPEND L
                         (PROG (J FORALL-RESULT FORALL-ENDPTR)
                           (SETQ J 1)
                           (COND
                            ((MINUSP (DIFFERENCE (LENGTH H) J)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (LIST I
                                                  (LIST 'QUOTIENT
                                                        (NTH (NTH H J) 1)
                                                        (OFF_MOD_LCOF
                                                         (NTH (NTH H J) 1) X))
                                                  (NTH (NTH H J) 2))
                                            NIL)))
                          LOOPLABEL
                           (SETQ J (PLUS2 J 1))
                           (COND
                            ((MINUSP (DIFFERENCE (LENGTH H) J))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (LIST I
                                          (LIST 'QUOTIENT (NTH (NTH H J) 1)
                                                (OFF_MOD_LCOF (NTH (NTH H J) 1)
                                                 X))
                                          (NTH (NTH H J) 2))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ P
              (DEG_SORT
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I 1)
                 (COND ((MINUSP (DIFFERENCE (LENGTH L) I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (NTH (NTH L I) 2) NIL)))
                LOOPLABEL
                 (SETQ I (PLUS2 I 1))
                 (COND
                  ((MINUSP (DIFFERENCE (LENGTH L) I)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (NTH (NTH L I) 2) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               X))
      (SETQ N (LENGTH P))
      (SETQ G (MKMATRIX R N))
      (SETQ FACT_MAT (MKMATRIX R N))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH L) K)) (RETURN NIL)))
        (PROGN
         (SETQ II (NTH (NTH L K) 1))
         (SETQ PP (NTH (NTH L K) 2))
         (SETQ FF (NTH (NTH L K) 3))
         (SETQ J 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND (NEQ PP (NTH P J)) (LEQ J N))) (RETURN NIL)))
           (SETQ J (PLUS J 1))
           (GO WHILELABEL))
         (SETMAT G II J FF)
         NIL)
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (SETMAT FACT_MAT 1 J (GETMAT G 1 J))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (SETMAT FACT_MAT I J
             (LIST 'PLUS (GETMAT FACT_MAT (DIFFERENCE I 1) J) (GETMAT G I J)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ T_LIST (LIST))
      (SETQ TINV_LIST (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ FACTS (LIST))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (COND
             ((NEQ (GETMAT FACT_MAT I J) 0)
              (PROGN
               (SETQ FACTS
                       (APPEND
                        (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                          (SETQ ELT FACTS)
                          (COND ((NULL ELT) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS ((LAMBDA (ELT) ELT) (CAR ELT))
                                                NIL)))
                         LOOPLABEL
                          (SETQ ELT (CDR ELT))
                          (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (ELT) ELT) (CAR ELT)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        (LIST (LIST (NTH P J) (GETMAT FACT_MAT I J)))))
               NIL)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ TMP (COMPANION_TO_RATJORDAN FACTS (NTH INV_FACT I) X))
         (SETQ Q (CAR TMP))
         (SETQ QINV (CADR TMP))
         (SETQ TINV_LIST (APPEND TINV_LIST (LIST QINV)))
         (SETQ T_LIST (APPEND T_LIST (LIST Q)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ D (MKMATRIX R N))
      (SETQ DEGP (MKVECT R))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN
            (SETMAT D I J
             (LIST 'TIMES (DEG (NTH P J) X) (GETMAT FACT_MAT I J)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PUTV DEGP I
               (PROG (J FORALL-RESULT)
                 (SETQ J 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (PLUS (OFF_MOD_REVAL (GETMAT D I J)) FORALL-RESULT))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB1)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ COLS (LIST))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
           (PROGN
            (SETQ COUNT
                    (REVAL1
                     (LIST 'PLUS
                           (PROG (K FORALL-RESULT)
                             (SETQ K 1)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((MINUSP (DIFFERENCE (DIFFERENCE I 1) K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS (OFF_MOD_REVAL (GETV DEGP K))
                                           FORALL-RESULT))
                             (SETQ K (PLUS2 K 1))
                             (GO LAB1))
                           (PROG (K FORALL-RESULT)
                             (SETQ K 1)
                             (SETQ FORALL-RESULT 0)
                            LAB1
                             (COND
                              ((MINUSP (DIFFERENCE (DIFFERENCE J 1) K))
                               (RETURN FORALL-RESULT)))
                             (SETQ FORALL-RESULT
                                     (PLUS (REVAL1 (GETMAT D I K) T)
                                           FORALL-RESULT))
                             (SETQ K (PLUS2 K 1))
                             (GO LAB1)))
                     T))
            (PROG (H)
              (SETQ H 1)
             LAB
              (COND
               ((MINUSP (DIFFERENCE (OFF_MOD_REVAL (GETMAT D I J)) H))
                (RETURN NIL)))
              (PROGN
               (SETQ COLS (APPEND COLS (LIST (REVAL1 (LIST 'PLUS COUNT H) T))))
               NIL)
              (SETQ H (PLUS2 H 1))
              (GO LAB))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ TT (REVAL1 (LIST 'DIAGI T_LIST) T))
      (SETQ NN (CAR (SIZE_OF_MATRIX TT)))
      (SETQ S (MKMATRIX NN NN))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NN J)) (RETURN NIL)))
           (PROGN (SETMAT S I J (GETMAT TT I (NTH COLS J))) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ TINV (REVAL1 (LIST 'DIAGI TINV_LIST) T))
      (SETQ SINV (MKMATRIX NN NN))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NN I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE NN J)) (RETURN NIL)))
           (PROGN (SETMAT SINV I J (GETMAT TINV (NTH COLS I) J)) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ PRIM_INV (LIST))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ EXP_LIST (LIST))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
           (PROGN
            (COND
             ((NEQ (GETMAT FACT_MAT I J) 0)
              (SETQ EXP_LIST (APPEND EXP_LIST (LIST (GETMAT FACT_MAT I J))))))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ PRIM_INV (APPEND PRIM_INV (LIST (LIST (NTH P J) EXP_LIST))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN (LIST PRIM_INV S SINV)))) 
(PUT 'GET_DEN 'NUMBER-OF-ARGS 1) 
(PUT 'GET_DEN 'DEFINED-ON-LINE '946) 
(PUT 'GET_DEN 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'GET_DEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_DEN (INPUT)
    (PROG (DENOM COPY)
      (SETQ COPY INPUT)
      (COND ((EQUAL (CAR COPY) 'MINUS) (SETQ COPY (CADR COPY))))
      (SETQ DENOM (CADDR COPY))
      (RETURN DENOM))) 
(PUT 'MAKE_RATJ_BLOCK 'NUMBER-OF-ARGS 3) 
(PUT 'MAKE_RATJ_BLOCK 'DEFINED-ON-LINE '958) 
(PUT 'MAKE_RATJ_BLOCK 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'MAKE_RATJ_BLOCK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE_RATJ_BLOCK (P E X)
    (PROG (C J_BLOCK D N)
      (SETQ D 0)
      (SETQ N 0)
      (SETQ C (COMPANION P X))
      (SETQ D (DEG P X))
      (SETQ E (OFF_MOD_REVAL E))
      (SETQ N (TIMES D E))
      (SETQ J_BLOCK (MKMATRIX N N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE E I)) (RETURN NIL)))
        (PROGN
         (SETQ J_BLOCK
                 (COPYINTO C J_BLOCK (PLUS (TIMES (DIFFERENCE I 1) D) 1)
                  (PLUS (TIMES (DIFFERENCE I 1) D) 1)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N D) I)) (RETURN NIL)))
        (PROGN (SETMAT J_BLOCK I (PLUS I D) 1) NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN J_BLOCK))) 
(PUT 'PRIMINV_TO_RATJORDAN 'NUMBER-OF-ARGS 2) 
(PUT 'PRIMINV_TO_RATJORDAN 'DEFINED-ON-LINE '990) 
(PUT 'PRIMINV_TO_RATJORDAN 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'PRIMINV_TO_RATJORDAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIMINV_TO_RATJORDAN (PRIM_INV X)
    (PROG (P EXP_LIST BLOCK_LIST R)
      (SETQ R 0)
      (SETQ R (LENGTH PRIM_INV))
      (SETQ BLOCK_LIST (LIST))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROGN
         (SETQ P (NTH (NTH PRIM_INV I) 1))
         (SETQ EXP_LIST (NTH (NTH PRIM_INV I) 2))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (LENGTH EXP_LIST) J)) (RETURN NIL)))
           (PROGN
            (SETQ BLOCK_LIST
                    (APPEND BLOCK_LIST
                            (LIST (MAKE_RATJ_BLOCK P (NTH EXP_LIST J) X))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (REVAL1 (LIST 'DIAGI BLOCK_LIST) T)))) 
(PUT 'QUO_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'QUO_TEST 'DEFINED-ON-LINE '1020) 
(PUT 'QUO_TEST 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'QUO_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUO_TEST (INPUT)
    (PROG (BOOLEAN COPY)
      (SETQ COPY INPUT)
      (COND ((ATOM COPY) (PROGN NIL))
            (T
             (PROGN
              (COND ((EQUAL (CAR COPY) 'MINUS) (SETQ COPY (CADR COPY))))
              (COND ((SETCAR COPY 'QUOTIENT) (SETQ BOOLEAN T))
                    (T (SETQ BOOLEAN NIL)))
              NIL)))
      (RETURN BOOLEAN))) 
(PUT 'RE_NEST_LIST 'NUMBER-OF-ARGS 2) 
(PUT 'RE_NEST_LIST 'DEFINED-ON-LINE '1038) 
(PUT 'RE_NEST_LIST 'DEFINED-IN-FILE 'NORMFORM/RATJORD.RED) 
(PUT 'RE_NEST_LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RE_NEST_LIST (INPUT FULL_COEFF_LIST)
    (PROG (TMP COPY)
      (SETQ COPY INPUT)
      (PROG (ELT)
        (SETQ ELT FULL_COEFF_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ TMP (LIST 'CO 2 ELT))
            (SETQ COPY (AEVAL (LIST 'SUB (LIST 'EQUAL ELT TMP) COPY)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN COPY))) 
(ENDMODULE) 