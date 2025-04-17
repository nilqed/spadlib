(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SMITHEX1)) 
(PUT 'SMITHEX_INT 'NUMBER-OF-ARGS 1) 
(PUT 'SMITHEX_INT 'DEFINED-ON-LINE '41) 
(PUT 'SMITHEX_INT 'DEFINED-IN-FILE 'NORMFORM/SMITHEX1.RED) 
(PUT 'SMITHEX_INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SMITHEX_INT (B)
    (PROG (LEFT RIGHT ISCLEAR A N M I J K L TMP G LL RR INT1 INT2 QUO1 QUO2 R
           SGN RRQUO Q INPUT_MODE)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ K 0)
      (SETQ L 0)
      (SETQ TMP 0)
      (SETQ G 0)
      (SETQ LL 0)
      (SETQ RR 0)
      (SETQ INT1 0)
      (SETQ INT2 0)
      (SETQ QUO1 0)
      (SETQ QUO2 0)
      (SETQ R 0)
      (SETQ SGN 0)
      (SETQ RRQUO 0)
      (SETQ Q 0)
      (SETQ INPUT_MODE 0)
      (MATRIX_INPUT_TEST B)
      (SETQ INPUT_MODE (GET DMODE* 'DNAME))
      (COND
       ((EQUAL INPUT_MODE 'MODULAR)
        (REDERR "ERROR: smithex_int does not work with modular on.")))
      (INTEGER_ENTRIES_TEST B)
      (SETQ A (COPY_MAT B))
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
                          ((LESSP (ABS (GETMAT A L K)) (ABS (GETMAT A I K)))
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
                          ((LESSP (ABS (GETMAT A K L)) (ABS (GETMAT A K J)))
                           (SETQ J L)))
                    NIL)
                   (SETQ L (PLUS2 L 1))
                   (GO LAB))
                 (COND
                  ((AND (LEQ I N)
                        (OR (GREATERP J M)
                            (LESSP (ABS (GETMAT A I K)) (ABS (GETMAT A K J)))))
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
                               (SETQ INT1 (GETMAT A K K))
                               (SETQ INT2 (GETMAT A I K))
                               (SETQ TMP (CALC_EXGCD_INT INT1 INT2))
                               (SETQ G (CAR TMP))
                               (SETQ LL (CADR TMP))
                               (SETQ RR (CADDR TMP))
                               (SETQ QUO1 (GET_QUO_INT (GETMAT A K K) G))
                               (SETQ QUO2 (GET_QUO_INT (GETMAT A I K) G))
                               (PROG (J)
                                 (SETQ J (PLUS K 1))
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (PLUS (TIMES LL (GETMAT A K J))
                                                (TIMES RR (GETMAT A I J))))
                                  (SETMAT A I J
                                   (DIFFERENCE (TIMES QUO1 (GETMAT A I J))
                                               (TIMES QUO2 (GETMAT A K J))))
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
                                          (PLUS (TIMES QUO1 (GETMAT LEFT J K))
                                                (TIMES QUO2
                                                       (GETMAT LEFT J I))))
                                  (SETMAT LEFT J I
                                   (PLUS (MINUS (TIMES RR (GETMAT LEFT J K)))
                                         (TIMES LL (GETMAT LEFT J I))))
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
                       (SETQ Q (GET_QUO_INT (GETMAT A K I) (GETMAT A K K)))
                       (SETMAT A K I
                        (GET_REM_INT (GETMAT A K I) (GETMAT A K K)))
                       (PROG (J)
                         (SETQ J 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                         (PROGN
                          (SETMAT RIGHT K J
                           (PLUS (GETMAT RIGHT K J)
                                 (TIMES Q (GETMAT RIGHT I J))))
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
                               (SETQ TMP
                                       (CALC_EXGCD_INT (GETMAT A K K)
                                        (GETMAT A K I)))
                               (SETQ G (CAR TMP))
                               (SETQ LL (CADR TMP))
                               (SETQ RR (CADDR TMP))
                               (SETQ QUO1 (GET_QUO_INT (GETMAT A K K) G))
                               (SETQ QUO2 (GET_QUO_INT (GETMAT A K I) G))
                               (PROG (J)
                                 (SETQ J (PLUS K 1))
                                LAB
                                 (COND
                                  ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                                 (PROGN
                                  (SETQ TMP
                                          (PLUS (TIMES LL (GETMAT A J K))
                                                (TIMES RR (GETMAT A J I))))
                                  (SETMAT A J I
                                   (DIFFERENCE (TIMES QUO1 (GETMAT A J I))
                                               (TIMES QUO2 (GETMAT A J K))))
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
                                          (PLUS (TIMES QUO1 (GETMAT RIGHT K J))
                                                (TIMES QUO2
                                                       (GETMAT RIGHT I J))))
                                  (SETMAT RIGHT I J
                                   (PLUS (MINUS (TIMES RR (GETMAT RIGHT K J)))
                                         (TIMES LL (GETMAT RIGHT I J))))
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
            (SETQ SGN (AEVAL* (LIST 'SIGN (LIST 'GETMAT A I I))))
            (SETMAT A R R (TIMES SGN (GETMAT A I I)))
            (COND
             ((EQUAL I R)
              (PROGN
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                 (PROGN (SETMAT RIGHT I J (TIMES (GETMAT RIGHT I J) SGN)) NIL)
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
                  (SETQ TMP (TIMES (GETMAT RIGHT I J) SGN))
                  (SETMAT RIGHT I J (TIMES (GETMAT RIGHT R J) SGN))
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
             (SETQ INT1 (GETMAT A I I))
             (SETQ INT2 (GETMAT A J J))
             (SETQ G (CAR (CALC_EXGCD_INT INT1 INT2)))
             (SETQ LL (CADR (CALC_EXGCD_INT INT1 INT2)))
             (SETQ RR (CADDR (CALC_EXGCD_INT INT1 INT2)))
             (SETQ QUO1 (GET_QUO_INT (GETMAT A I I) G))
             (SETQ QUO2 (GET_QUO_INT (GETMAT A J J) G))
             (SETMAT A I I G)
             (SETMAT A J J (TIMES QUO1 (GETMAT A J J)))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
               (PROGN
                (SETQ TMP
                        (PLUS (TIMES QUO1 (GETMAT LEFT K I))
                              (TIMES QUO2 (GETMAT LEFT K J))))
                (SETMAT LEFT K J
                 (PLUS (MINUS (TIMES RR (GETMAT LEFT K I)))
                       (TIMES LL (GETMAT LEFT K J))))
                (SETMAT LEFT K I TMP)
                NIL)
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
               (PROGN
                (SETQ RRQUO (TIMES RR QUO2))
                (SETQ TMP
                        (PLUS (TIMES (DIFFERENCE 1 RRQUO) (GETMAT RIGHT I K))
                              (TIMES RRQUO (GETMAT RIGHT J K))))
                (SETMAT RIGHT J K
                 (PLUS (MINUS (GETMAT RIGHT I K)) (GETMAT RIGHT J K)))
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
      (RETURN (LIST 'LIST A LEFT RIGHT)))) 
(FLAG '(SMITHEX_INT) 'OPFN) 
(PUT 'CALC_EXGCD_INT 'NUMBER-OF-ARGS 2) 
(PUT 'CALC_EXGCD_INT 'DEFINED-ON-LINE '334) 
(PUT 'CALC_EXGCD_INT 'DEFINED-IN-FILE 'NORMFORM/SMITHEX1.RED) 
(PUT 'CALC_EXGCD_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CALC_EXGCD_INT (INT1 INT2)
    (PROG (GCD C C1 C2 D D1 D2 Q R R1 R2 S1 T1)
      (SETQ GCD 0)
      (SETQ C 0)
      (SETQ C1 0)
      (SETQ C2 0)
      (SETQ D 0)
      (SETQ D1 0)
      (SETQ D2 0)
      (SETQ Q 0)
      (SETQ R 0)
      (SETQ R1 0)
      (SETQ R2 0)
      (SETQ S1 0)
      (SETQ T1 0)
      (COND ((AND (EQUAL INT1 0) (EQUAL INT2 0)) (RETURN (LIST 0 0 0)))
            (T
             (PROGN
              (SETQ C (REVAL1 INT1 T))
              (SETQ D (REVAL1 INT2 T))
              (SETQ C1 1)
              (SETQ D1 0)
              (SETQ C2 0)
              (SETQ D2 1)
              (PROG ()
               WHILELABEL
                (COND ((NOT (NEQ D 0)) (RETURN NIL)))
                (PROGN
                 (SETQ Q (GET_QUO_INT C D))
                 (SETQ R (DIFFERENCE C (TIMES Q D)))
                 (SETQ R1 (DIFFERENCE C1 (TIMES Q D1)))
                 (SETQ R2 (DIFFERENCE C2 (TIMES Q D2)))
                 (SETQ C D)
                 (SETQ C1 D1)
                 (SETQ C2 D2)
                 (SETQ D R)
                 (SETQ D1 R1)
                 (SETQ D2 R2)
                 NIL)
                (GO WHILELABEL))
              (SETQ GCD (ABS C))
              (SETQ S1 C1)
              (SETQ T1 C2)
              (COND
               ((LESSP C 0)
                (PROGN (SETQ S1 (MINUS S1)) (SETQ T1 (MINUS T1)) NIL)))
              (RETURN (LIST GCD S1 T1))
              NIL))))) 
(PUT 'GET_QUO_INT 'NUMBER-OF-ARGS 2) 
(PUT 'GET_QUO_INT 'DEFINED-ON-LINE '379) 
(PUT 'GET_QUO_INT 'DEFINED-IN-FILE 'NORMFORM/SMITHEX1.RED) 
(PUT 'GET_QUO_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_QUO_INT (INT1 INT2)
    (PROG (QUO1 INPUT1 INPUT2)
      (SETQ QUO1 0)
      (SETQ INPUT1 0)
      (SETQ INPUT2 0)
      (SETQ INPUT1 (REVAL1 INT1 T))
      (SETQ INPUT2 (REVAL1 INT2 T))
      (COND ((AND (EQUAL INPUT1 0) (EQUAL INPUT2 0)) (RETURN 0))
            (T
             (PROGN
              (COND
               ((AND (LESSP INPUT1 0) (LESSP INPUT2 0))
                (PROGN
                 (SETQ INPUT1 (ABS INPUT1))
                 (SETQ INPUT2 (ABS INPUT2))
                 NIL)))
              (COND
               ((LESSP (QUOTIENT INPUT1 INPUT2) 0)
                (PROGN (SETQ QUO1 (CEILING (QUOTIENT INPUT1 INPUT2))) NIL))
               (T (PROGN (SETQ QUO1 (FLOOR (QUOTIENT INPUT1 INPUT2))) NIL)))
              (RETURN QUO1)
              NIL))))) 
(PUT 'GET_REM_INT 'NUMBER-OF-ARGS 2) 
(PUT 'GET_REM_INT 'DEFINED-ON-LINE '409) 
(PUT 'GET_REM_INT 'DEFINED-IN-FILE 'NORMFORM/SMITHEX1.RED) 
(PUT 'GET_REM_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_REM_INT (INT1 INT2)
    (PROG (REM1 INPUT1 INPUT2)
      (SETQ REM1 0)
      (SETQ INPUT1 0)
      (SETQ INPUT2 0)
      (SETQ INPUT1 (REVAL1 INT1 T))
      (SETQ INPUT2 (REVAL1 INT2 T))
      (SETQ REM1
              (DIFFERENCE INPUT1 (TIMES (GET_QUO_INT INPUT1 INPUT2) INPUT2)))
      (RETURN REM1))) 
(PUT 'INTEGER_ENTRIES_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'INTEGER_ENTRIES_TEST 'DEFINED-ON-LINE '419) 
(PUT 'INTEGER_ENTRIES_TEST 'DEFINED-IN-FILE 'NORMFORM/SMITHEX1.RED) 
(PUT 'INTEGER_ENTRIES_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTEGER_ENTRIES_TEST (B)
    (PROG ()
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CAR (SIZE_OF_MATRIX B)) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND
            ((MINUSP (DIFFERENCE (CADR (SIZE_OF_MATRIX B)) J)) (RETURN NIL)))
           (PROGN
            (COND
             ((NOT (NUMBERP (GETMAT B I J)))
              (REDERR
               "ERROR: matrix contains non_integer entries. Try smithex. "))))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB)))) 
(ENDMODULE) 