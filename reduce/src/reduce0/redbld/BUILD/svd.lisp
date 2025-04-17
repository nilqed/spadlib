(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SVD)) 
(DE MY_MINUS (U)
    (COND ((ATOM U) (LIST 'MINUS U)) ((EQUAL (CAR U) 'MINUS) (CADR U))
          (T (LIST 'MINUS U)))) 
(PUT 'MY_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'MY_MINUS 'DEFINED-ON-LINE '43) 
(PUT 'MY_MINUS 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'MY_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MY_MINUS 'INLINE
      '(LAMBDA (U)
         (COND ((ATOM U) (LIST 'MINUS U)) ((EQUAL (CAR U) 'MINUS) (CADR U))
               (T (LIST 'MINUS U))))) 
(PUT 'SVD 'NUMBER-OF-ARGS 1) 
(PUT 'SVD 'DEFINED-ON-LINE '53) 
(PUT 'SVD 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'SVD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SVD (A)
    (PROG (EE U V G X EPS TOLERANCE Q S F H Y TEST_F_SPLITTING CANCELLATION
           TEST_F_CONVERGENCE CONVERGENCE C Z DENOM Q_MAT I_ROUNDED_TURNED_ON
           TRANS_DONE I J K L L1 M N NO_ITERS)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ K 0)
      (SETQ L 0)
      (SETQ L1 0)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ NO_ITERS 0)
      (SETQ TRANS_DONE (SETQ I_ROUNDED_TURNED_ON NIL))
      (COND
       ((NOT *ROUNDED)
        ((LAMBDA (*MSG)
           (PROGN (ON1 'ROUNDED) (SETQ I_ROUNDED_TURNED_ON T) NIL))
         NIL)))
      (COND ((NOT (MATRIXP A)) (REDERR "Error in svd: non matrix input.")))
      (SETQ EPS
              (GET_NUM_PART
               ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                (LIST 'TIMES 1.5 (LIST 'EXPT 10 (MINUS 8))))))
      (SETQ TOLERANCE
              (GET_NUM_PART
               ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                (LIST 'EXPT 10 (MINUS 31)))))
      (COND
       ((LESSP (ROW_DIM A) (COLUMN_DIM A))
        (PROGN (SETQ A (AEVAL (LIST 'TP A))) (SETQ TRANS_DONE T) NIL)))
      (SETQ M (ROW_DIM A))
      (SETQ N (COLUMN_DIM A))
      (SETQ U (ERRORSET2 (LIST 'RD_COPY_MAT (MKQUOTE A))))
      (COND
       ((ERRORP U)
        (PROGN
         (COND (I_ROUNDED_TURNED_ON ((LAMBDA (*MSG) (OFF1 'ROUNDED)) NIL)))
         (COND (ERRMSG* (REDERR ERRMSG*)) (T (REDERR "Error in svd operator")))
         NIL))
       (T (SETQ U (CAR U))))
      (SETQ V (MKMATRIX N N))
      (SETQ EE (MKVECT N))
      (SETQ Q (MKVECT N))
      (SETQ G (SETQ X 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (PUTV EE I G)
         (SETQ S 0)
         (SETQ L (PLUS I 1))
         (PROG (J)
           (SETQ J I)
          LAB
           (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
           (SETQ S (|SPECRD:PLUS| S (|SPECRD:EXPT| (GETMAT U J I) 2)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND ((LESSP (GET_NUM_PART S) TOLERANCE) (SETQ G 0))
               (T
                (PROGN
                 (SETQ F (GETMAT U I I))
                 (COND ((LESSP (GET_NUM_PART F) 0) (SETQ G (|SPECRD:SQRT| S)))
                       (T
                        (SETQ G
                                ((LAMBDA (U)
                                   (COND ((ATOM U) (LIST 'MINUS U))
                                         ((EQUAL (CAR U) 'MINUS) (CADR U))
                                         (T (LIST 'MINUS U))))
                                 (|SPECRD:SQRT| S)))))
                 (SETQ H
                         (|SPECRD:PLUS| (|SPECRD:TIMES| F G)
                          (COND ((ATOM S) (LIST 'MINUS S))
                                ((EQUAL (CAR S) 'MINUS) (CADR S))
                                (T (LIST 'MINUS S)))))
                 (SETMAT U I I
                  (|SPECRD:PLUS| F
                   (COND ((ATOM G) (LIST 'MINUS G))
                         ((EQUAL (CAR G) 'MINUS) (CADR G))
                         (T (LIST 'MINUS G)))))
                 (PROG (J)
                   (SETQ J L)
                  LAB
                   (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                   (PROGN
                    (SETQ S 0)
                    (PROG (K)
                      (SETQ K I)
                     LAB
                      (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
                      (SETQ S
                              (|SPECRD:PLUS| S
                               (|SPECRD:TIMES| (GETMAT U K I) (GETMAT U K J))))
                      (SETQ K (PLUS2 K 1))
                      (GO LAB))
                    (SETQ F (|SPECRD:QUOTIENT| S H))
                    (PROG (K)
                      (SETQ K I)
                     LAB
                      (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
                      (SETMAT U K J
                       (|SPECRD:PLUS| (GETMAT U K J)
                        (|SPECRD:TIMES| F (GETMAT U K I))))
                      (SETQ K (PLUS2 K 1))
                      (GO LAB))
                    NIL)
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 NIL)))
         (PUTV Q I G)
         (SETQ S 0)
         (PROG (J)
           (SETQ J L)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (SETQ S (|SPECRD:PLUS| S (|SPECRD:EXPT| (GETMAT U I J) 2)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND ((LESSP (GET_NUM_PART S) TOLERANCE) (SETQ G 0))
               (T
                (PROGN
                 (SETQ F (GETMAT U I (PLUS I 1)))
                 (COND ((LESSP (GET_NUM_PART F) 0) (SETQ G (|SPECRD:SQRT| S)))
                       (T
                        (SETQ G
                                ((LAMBDA (U)
                                   (COND ((ATOM U) (LIST 'MINUS U))
                                         ((EQUAL (CAR U) 'MINUS) (CADR U))
                                         (T (LIST 'MINUS U))))
                                 (|SPECRD:SQRT| S)))))
                 (SETQ H
                         (|SPECRD:PLUS| (|SPECRD:TIMES| F G)
                          (COND ((ATOM S) (LIST 'MINUS S))
                                ((EQUAL (CAR S) 'MINUS) (CADR S))
                                (T (LIST 'MINUS S)))))
                 (SETMAT U I (PLUS I 1)
                  (|SPECRD:PLUS| F
                   (COND ((ATOM G) (LIST 'MINUS G))
                         ((EQUAL (CAR G) 'MINUS) (CADR G))
                         (T (LIST 'MINUS G)))))
                 (PROG (J)
                   (SETQ J L)
                  LAB
                   (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                   (PUTV EE J (|SPECRD:QUOTIENT| (GETMAT U I J) H))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 (PROG (J)
                   (SETQ J L)
                  LAB
                   (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                   (PROGN
                    (SETQ S 0)
                    (PROG (K)
                      (SETQ K L)
                     LAB
                      (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                      (SETQ S
                              (|SPECRD:PLUS| S
                               (|SPECRD:TIMES| (GETMAT U J K) (GETMAT U I K))))
                      (SETQ K (PLUS2 K 1))
                      (GO LAB))
                    (PROG (K)
                      (SETQ K L)
                     LAB
                      (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                      (SETMAT U J K
                       (|SPECRD:PLUS| (GETMAT U J K)
                        (|SPECRD:TIMES| S (GETV EE K))))
                      (SETQ K (PLUS2 K 1))
                      (GO LAB))
                    NIL)
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 NIL)))
         (SETQ Y
                 (|SPECRD:PLUS| (ABS (GET_NUM_PART (GETV Q I)))
                  (ABS (GET_NUM_PART (GETV EE I)))))
         (COND ((GREATERP (GET_NUM_PART Y) (GET_NUM_PART X)) (SETQ X Y)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I N)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (COND
          ((NEQ (GET_NUM_PART G) 0)
           (PROGN
            (SETQ H (|SPECRD:TIMES| (GETMAT U I (PLUS I 1)) G))
            (PROG (J)
              (SETQ J L)
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (SETMAT V J I (|SPECRD:QUOTIENT| (GETMAT U I J) H))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J L)
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (PROGN
               (SETQ S 0)
               (PROG (K)
                 (SETQ K L)
                LAB
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                 (SETQ S
                         (|SPECRD:PLUS| S
                          (|SPECRD:TIMES| (GETMAT U I K) (GETMAT V K J))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               (PROG (K)
                 (SETQ K L)
                LAB
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                 (SETMAT V K J
                  (|SPECRD:PLUS| (GETMAT V K J)
                   (|SPECRD:TIMES| S (GETMAT V K I))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL)))
         (PROG (J)
           (SETQ J L)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (PROGN (SETMAT V I J 0) (SETMAT V J I 0) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETMAT V I I 1)
         (SETQ G (GETV EE I))
         (SETQ L I)
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (PROG (I)
        (SETQ I N)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (SETQ L (PLUS I 1))
         (SETQ G (GETV Q I))
         (PROG (J)
           (SETQ J L)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (SETMAT U I J 0)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND
          ((NEQ (GET_NUM_PART G) 0)
           (PROGN
            (SETQ H (|SPECRD:TIMES| (GETMAT U I I) G))
            (PROG (J)
              (SETQ J L)
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (PROGN
               (SETQ S 0)
               (PROG (K)
                 (SETQ K L)
                LAB
                 (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
                 (SETQ S
                         (|SPECRD:PLUS| S
                          (|SPECRD:TIMES| (GETMAT U K I) (GETMAT U K J))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               (SETQ F (|SPECRD:QUOTIENT| S H))
               (PROG (K)
                 (SETQ K I)
                LAB
                 (COND ((MINUSP (DIFFERENCE M K)) (RETURN NIL)))
                 (SETMAT U K J
                  (|SPECRD:PLUS| (GETMAT U K J)
                   (|SPECRD:TIMES| F (GETMAT U K I))))
                 (SETQ K (PLUS2 K 1))
                 (GO LAB))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J I)
             LAB
              (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
              (SETMAT U J I (|SPECRD:QUOTIENT| (GETMAT U J I) G))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL))
          (T
           (PROG (J)
             (SETQ J I)
            LAB
             (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
             (SETMAT U J I 0)
             (SETQ J (PLUS2 J 1))
             (GO LAB))))
         (SETMAT U I I (|SPECRD:PLUS| (GETMAT U I I) 1))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SETQ EPS (GET_NUM_PART (|SPECRD:TIMES| EPS X)))
      (SETQ TEST_F_SPLITTING T)
      (SETQ K N)
      (SETQ NO_ITERS 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ K 1)) (RETURN NIL)))
        (PROGN
         (SETQ NO_ITERS (PLUS NO_ITERS 1))
         (SETQ CONVERGENCE NIL)
         (COND
          (TEST_F_SPLITTING
           (PROGN
            (SETQ L K)
            (SETQ TEST_F_CONVERGENCE (SETQ CANCELLATION NIL))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (GEQ L 1) (NOT (OR TEST_F_CONVERGENCE CANCELLATION))))
                (RETURN NIL)))
              (PROGN
               (COND
                ((LEQ (ABS (GET_NUM_PART (GETV EE L))) EPS)
                 (SETQ TEST_F_CONVERGENCE T))
                ((LEQ (ABS (GET_NUM_PART (GETV Q (DIFFERENCE L 1)))) EPS)
                 (SETQ CANCELLATION T))
                (T (SETQ L (DIFFERENCE L 1))))
               NIL)
              (GO WHILELABEL))
            NIL)))
         (COND
          ((NOT TEST_F_CONVERGENCE)
           (PROGN
            (SETQ C 0)
            (SETQ S 1)
            (SETQ L1 (DIFFERENCE L 1))
            (SETQ I L)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND (LEQ I K) (NOT TEST_F_CONVERGENCE))) (RETURN NIL)))
              (PROGN
               (SETQ F (|SPECRD:TIMES| S (GETV EE I)))
               (PUTV EE I (|SPECRD:TIMES| C (GETV EE I)))
               (COND
                ((LEQ (ABS (GET_NUM_PART F)) EPS) (SETQ TEST_F_CONVERGENCE T))
                (T
                 (PROGN
                  (SETQ G (GETV Q I))
                  (SETQ H
                          (|SPECRD:SQRT|
                           (|SPECRD:PLUS| (|SPECRD:TIMES| F F)
                            (|SPECRD:TIMES| G G))))
                  (PUTV Q I H)
                  (SETQ C (|SPECRD:QUOTIENT| G H))
                  (SETQ S
                          (|SPECRD:QUOTIENT|
                           (COND ((ATOM F) (LIST 'MINUS F))
                                 ((EQUAL (CAR F) 'MINUS) (CADR F))
                                 (T (LIST 'MINUS F)))
                           H))
                  (PROG (J)
                    (SETQ J 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                    (PROGN
                     (SETQ Y (GETMAT U J L1))
                     (SETQ Z (GETMAT U J I))
                     (SETMAT U J L1
                      (|SPECRD:PLUS| (|SPECRD:TIMES| Y C)
                       (|SPECRD:TIMES| Z S)))
                     (SETMAT U J I
                      (|SPECRD:DIFFERENCE| (|SPECRD:TIMES| Z C)
                       (|SPECRD:TIMES| Y S)))
                     NIL)
                    (SETQ J (PLUS2 J 1))
                    (GO LAB))
                  (SETQ I (PLUS I 1))
                  NIL)))
               NIL)
              (GO WHILELABEL))
            NIL)))
         (COND
          ((GEQ NO_ITERS 30)
           (REDERR
            "svd: Emergency stop, maximum number of iterations reached without convergence")))
         (SETQ Z (GETV Q K))
         (COND ((EQUAL L K) (SETQ CONVERGENCE T)))
         (COND
          ((NOT CONVERGENCE)
           (PROGN
            (SETQ X (GETV Q L))
            (SETQ Y (GETV Q (DIFFERENCE K 1)))
            (SETQ G (GETV EE (DIFFERENCE K 1)))
            (SETQ H (GETV EE K))
            (SETQ F
                    (|SPECRD:QUOTIENT|
                     (|SPECRD:PLUS|
                      (|SPECRD:TIMES|
                       (|SPECRD:PLUS| Y
                        (COND ((ATOM Z) (LIST 'MINUS Z))
                              ((EQUAL (CAR Z) 'MINUS) (CADR Z))
                              (T (LIST 'MINUS Z))))
                       (|SPECRD:PLUS| Y Z))
                      (|SPECRD:TIMES|
                       (|SPECRD:PLUS| G
                        (COND ((ATOM H) (LIST 'MINUS H))
                              ((EQUAL (CAR H) 'MINUS) (CADR H))
                              (T (LIST 'MINUS H))))
                       (|SPECRD:PLUS| G H)))
                     (|SPECRD:TIMES| (|SPECRD:TIMES| 2 H) Y)))
            (SETQ G (|SPECRD:SQRT| (|SPECRD:PLUS| (|SPECRD:TIMES| F F) 1)))
            (COND
             ((LEQ (GET_NUM_PART F) 0)
              (SETQ DENOM
                      (|SPECRD:PLUS| F
                       (COND ((ATOM G) (LIST 'MINUS G))
                             ((EQUAL (CAR G) 'MINUS) (CADR G))
                             (T (LIST 'MINUS G))))))
             (T (SETQ DENOM (|SPECRD:PLUS| F G))))
            (SETQ F
                    (|SPECRD:QUOTIENT|
                     (|SPECRD:PLUS|
                      (|SPECRD:TIMES|
                       (|SPECRD:PLUS| X
                        (COND ((ATOM Z) (LIST 'MINUS Z))
                              ((EQUAL (CAR Z) 'MINUS) (CADR Z))
                              (T (LIST 'MINUS Z))))
                       (|SPECRD:PLUS| X Z))
                      (|SPECRD:TIMES| H
                       (|SPECRD:PLUS| (|SPECRD:QUOTIENT| Y DENOM)
                        (COND ((ATOM H) (LIST 'MINUS H))
                              ((EQUAL (CAR H) 'MINUS) (CADR H))
                              (T (LIST 'MINUS H))))))
                     X))
            (SETQ C (SETQ S 1))
            (PROG (I)
              (SETQ I (PLUS L 1))
             LAB
              (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
              (PROGN
               (SETQ G (GETV EE I))
               (SETQ Y (GETV Q I))
               (SETQ H (|SPECRD:TIMES| S G))
               (SETQ G (|SPECRD:TIMES| C G))
               (SETQ Z
                       (|SPECRD:SQRT|
                        (|SPECRD:PLUS| (|SPECRD:TIMES| F F)
                         (|SPECRD:TIMES| H H))))
               (PUTV EE (DIFFERENCE I 1) Z)
               (SETQ C (|SPECRD:QUOTIENT| F Z))
               (SETQ S (|SPECRD:QUOTIENT| H Z))
               (SETQ F
                       (|SPECRD:PLUS| (|SPECRD:TIMES| X C)
                        (|SPECRD:TIMES| G S)))
               (SETQ G
                       (|SPECRD:PLUS|
                        (|SPECRD:TIMES|
                         (COND ((ATOM X) (LIST 'MINUS X))
                               ((EQUAL (CAR X) 'MINUS) (CADR X))
                               (T (LIST 'MINUS X)))
                         S)
                        (|SPECRD:TIMES| G C)))
               (SETQ H (|SPECRD:TIMES| Y S))
               (SETQ Y (|SPECRD:TIMES| Y C))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (PROGN
                  (SETQ X (GETMAT V J (DIFFERENCE I 1)))
                  (SETQ Z (GETMAT V J I))
                  (SETMAT V J (DIFFERENCE I 1)
                   (|SPECRD:PLUS| (|SPECRD:TIMES| X C) (|SPECRD:TIMES| Z S)))
                  (SETMAT V J I
                   (|SPECRD:DIFFERENCE| (|SPECRD:TIMES| Z C)
                    (|SPECRD:TIMES| X S)))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (SETQ Z
                       (|SPECRD:SQRT|
                        (|SPECRD:PLUS| (|SPECRD:TIMES| F F)
                         (|SPECRD:TIMES| H H))))
               (PUTV Q (DIFFERENCE I 1) Z)
               (SETQ C (|SPECRD:QUOTIENT| F Z))
               (SETQ S (|SPECRD:QUOTIENT| H Z))
               (SETQ F
                       (|SPECRD:PLUS| (|SPECRD:TIMES| C G)
                        (|SPECRD:TIMES| S Y)))
               (SETQ X
                       (|SPECRD:PLUS|
                        (|SPECRD:TIMES|
                         (COND ((ATOM S) (LIST 'MINUS S))
                               ((EQUAL (CAR S) 'MINUS) (CADR S))
                               (T (LIST 'MINUS S)))
                         G)
                        (|SPECRD:TIMES| C Y)))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                 (PROGN
                  (SETQ Y (GETMAT U J (DIFFERENCE I 1)))
                  (SETQ Z (GETMAT U J I))
                  (SETMAT U J (DIFFERENCE I 1)
                   (|SPECRD:PLUS| (|SPECRD:TIMES| Y C) (|SPECRD:TIMES| Z S)))
                  (SETMAT U J I
                   (|SPECRD:DIFFERENCE| (|SPECRD:TIMES| Z C)
                    (|SPECRD:TIMES| Y S)))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (PUTV EE L 0)
            (PUTV EE K F)
            (PUTV Q K X)
            NIL))
          (T
           (PROGN
            (COND
             ((LESSP (GET_NUM_PART Z) 0)
              (PROGN
               (PUTV Q K
                     (COND ((ATOM Z) (LIST 'MINUS Z))
                           ((EQUAL (CAR Z) 'MINUS) (CADR Z))
                           (T (LIST 'MINUS Z))))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                 (SETMAT V J K
                  ((LAMBDA (U)
                     (COND ((ATOM U) (LIST 'MINUS U))
                           ((EQUAL (CAR U) 'MINUS) (CADR U))
                           (T (LIST 'MINUS U))))
                   (GETMAT V J K)))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)))
            (SETQ K (DIFFERENCE K 1))
            (SETQ NO_ITERS 0)
            NIL)))
         NIL)
        (GO WHILELABEL))
      (SETQ Q_MAT (Q_TO_DIAG_MATRIX Q))
      (COND (I_ROUNDED_TURNED_ON ((LAMBDA (*MSG) (OFF1 'ROUNDED)) NIL)))
      (COND (TRANS_DONE (RETURN (LIST 'LIST (AEVAL V) Q_MAT (AEVAL U))))
            (T (RETURN (LIST 'LIST (AEVAL U) Q_MAT (AEVAL V))))))) 
(FLAG '(SVD) 'OPFN) 
(PUT 'Q_TO_DIAG_MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'Q_TO_DIAG_MATRIX 'DEFINED-ON-LINE '365) 
(PUT 'Q_TO_DIAG_MATRIX 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'Q_TO_DIAG_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE Q_TO_DIAG_MATRIX (Q)
    (PROG (Q_MAT I SQ_DIM_Q)
      (SETQ I 0)
      (SETQ SQ_DIM_Q 0)
      (SETQ SQ_DIM_Q (UPBV Q))
      (SETQ Q_MAT (MKMATRIX SQ_DIM_Q SQ_DIM_Q))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_DIM_Q I)) (RETURN NIL)))
        (SETMAT Q_MAT I I (GETV Q I))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN Q_MAT))) 
(PUT 'PSEUDO_INVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'PSEUDO_INVERSE 'DEFINED-ON-LINE '380) 
(PUT 'PSEUDO_INVERSE 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'PSEUDO_INVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSEUDO_INVERSE (IN_MAT)
    (PROG (PSU_INV SVD_LIST SIGMA1)
      (SETQ SVD_LIST (SVD IN_MAT))
      (PROGN
       (SETQ SIGMA1 (AEVAL (LIST 'TP (LIST 'SECOND SVD_LIST))))
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND
          ((|AMINUSP:|
            (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH SIGMA1))) I))
           (RETURN NIL)))
         (COND
          ((EVALNEQ (AEVAL* (LIST SIGMA1 I I)) 0)
           (SETK (LIST SIGMA1 I I)
                 (AEVAL* (LIST 'QUOTIENT 1 (LIST SIGMA1 I I))))))
         (SETQ I
                 ((LAMBDA (FORALL-RESULT)
                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                  I))
         (GO LAB)))
      (SETQ PSU_INV
              (AEVAL
               (LIST 'TIMES (LIST 'THIRD SVD_LIST) SIGMA1
                     (LIST 'TP (LIST 'FIRST SVD_LIST)))))
      (RETURN PSU_INV))) 
(FLAG '(PSEUDO_INVERSE) 'OPFN) 
(RTYPECAR (LIST 'PSEUDO_INVERSE)) 
(PUT 'RD_COPY_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'RD_COPY_MAT 'DEFINED-ON-LINE '413) 
(PUT 'RD_COPY_MAT 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'RD_COPY_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RD_COPY_MAT (A)
    (PROG (C ROW_DIM COLUMN_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COLUMN_DIM 0)
      (SETQ ROW_DIM (FIRST (SIZE_OF_MATRIX A)))
      (SETQ COLUMN_DIM (SECOND (SIZE_OF_MATRIX A)))
      (SETQ C (MKMATRIX ROW_DIM COLUMN_DIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLUMN_DIM J)) (RETURN NIL)))
           ((LAMBDA (VAL)
              (PROGN
               (COND
                ((AND (NOT (FIXP VAL)) (NOT (EQCAR VAL '|:RD:|)))
                 (RERROR 'LINALG 2
                         (LIST "svd: Non-numeric matrix element at position ("
                               I "," J ")"))))
               (SETMAT C I J VAL)))
            ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T)))) (GETMAT A I J)))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN C))) 
(PUT '|SPECRD:TIMES| 'NUMBER-OF-ARGS 2) 
(PUT '|SPECRD:TIMES| 'DEFINED-ON-LINE '444) 
(PUT '|SPECRD:TIMES| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:TIMES| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPECRD:TIMES| (U V)
    (PROG (NEGSIGN)
      (SETQ U (ADD_MINUS U))
      (SETQ V (ADD_MINUS V))
      (COND ((EQCAR U 'MINUS) (PROGN (SETQ U (CADR U)) (SETQ NEGSIGN T))))
      (COND
       ((EQCAR V 'MINUS)
        (PROGN (SETQ V (CADR V)) (SETQ NEGSIGN (NOT NEGSIGN)))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (COND ((ATOM V) (SETQ V (MKROUND (FLOAT V)))))
      (RETURN
       (COND (NEGSIGN (LIST 'MINUS (|RD:TIMES| U V))) (T (|RD:TIMES| U V)))))) 
(PUT '|SPECRD:QUOTIENT| 'NUMBER-OF-ARGS 2) 
(PUT '|SPECRD:QUOTIENT| 'DEFINED-ON-LINE '459) 
(PUT '|SPECRD:QUOTIENT| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:QUOTIENT| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPECRD:QUOTIENT| (U V)
    (PROG (NEGSIGN)
      (SETQ U (ADD_MINUS U))
      (SETQ V (ADD_MINUS V))
      (COND ((EQCAR U 'MINUS) (PROGN (SETQ U (CADR U)) (SETQ NEGSIGN T))))
      (COND
       ((EQCAR V 'MINUS)
        (PROGN (SETQ V (CADR V)) (SETQ NEGSIGN (NOT NEGSIGN)))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (COND ((ATOM V) (SETQ V (MKROUND (FLOAT V)))))
      (RETURN
       (COND (NEGSIGN (LIST 'MINUS (|RD:QUOTIENT| U V)))
             (T (|RD:QUOTIENT| U V)))))) 
(PUT '|SPECRD:EXPT| 'NUMBER-OF-ARGS 2) 
(PUT '|SPECRD:EXPT| 'DEFINED-ON-LINE '474) 
(PUT '|SPECRD:EXPT| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:EXPT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPECRD:EXPT| (U V)
    (PROG ()
      (COND
       ((OR (EQUAL U '(|:RD:| . 0.0)) (EQUAL U 0)) (RETURN '(|:RD:| . 0.0))))
      (COND ((EQCAR U 'MINUS) (SETQ U (CONS '|:RD:| (MINUS (CDADR U))))))
      (COND ((EQCAR V 'MINUS) (SETQ V (CONS '|:RD:| (MINUS (CDADR V))))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (COND ((ATOM V) (SETQ V (MKROUND (FLOAT V)))))
      (RETURN (RDEXPT* U V)))) 
(PUT '|SPECRD:SQRT| 'NUMBER-OF-ARGS 1) 
(PUT '|SPECRD:SQRT| 'DEFINED-ON-LINE '484) 
(PUT '|SPECRD:SQRT| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:SQRT| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |SPECRD:SQRT| (U)
    (PROG ()
      (COND
       ((OR (EQUAL U '(|:RD:| . 0.0)) (EQUAL U 0)) (RETURN '(|:RD:| . 0.0))))
      (COND ((EQCAR U 'MINUS) (SETQ U (CONS '|:RD:| (MINUS (CDADR U))))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (RETURN (RDSQRT* U)))) 
(PUT '|SPECRD:PLUS| 'NUMBER-OF-ARGS 2) 
(PUT '|SPECRD:PLUS| 'DEFINED-ON-LINE '492) 
(PUT '|SPECRD:PLUS| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:PLUS| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPECRD:PLUS| (U V)
    (PROG (NEGSIGN)
      (SETQ NEGSIGN 0)
      (SETQ U (ADD_MINUS U))
      (SETQ V (ADD_MINUS V))
      (COND ((EQCAR U 'MINUS) (PROGN (SETQ U (CADR U)) (SETQ NEGSIGN 1))))
      (COND
       ((EQCAR V 'MINUS)
        (PROGN (SETQ V (CADR V)) (SETQ NEGSIGN (PLUS NEGSIGN 2)))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (COND ((ATOM V) (SETQ V (MKROUND (FLOAT V)))))
      (RETURN
       (COND ((EQUAL NEGSIGN 0) (|RD:PLUS| U V))
             ((EQUAL NEGSIGN 3) (LIST 'MINUS (|RD:PLUS| U V)))
             ((EQUAL NEGSIGN 2) (|RD:DIFFERENCE| U V))
             (T (|RD:DIFFERENCE| V U)))))) 
(PUT '|SPECRD:DIFFERENCE| 'NUMBER-OF-ARGS 2) 
(PUT '|SPECRD:DIFFERENCE| 'DEFINED-ON-LINE '509) 
(PUT '|SPECRD:DIFFERENCE| 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT '|SPECRD:DIFFERENCE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPECRD:DIFFERENCE| (U V)
    (PROG (NEGSIGN)
      (SETQ NEGSIGN 0)
      (SETQ U (ADD_MINUS U))
      (SETQ V (ADD_MINUS V))
      (COND ((EQCAR U 'MINUS) (PROGN (SETQ U (CADR U)) (SETQ NEGSIGN 1))))
      (COND
       ((EQCAR V 'MINUS)
        (PROGN (SETQ V (CADR V)) (SETQ NEGSIGN (PLUS NEGSIGN 2)))))
      (COND ((ATOM U) (SETQ U (MKROUND (FLOAT U)))))
      (COND ((ATOM V) (SETQ V (MKROUND (FLOAT V)))))
      (RETURN
       (COND ((EQUAL NEGSIGN 0) (|RD:DIFFERENCE| U V))
             ((EQUAL NEGSIGN 3) (LIST 'MINUS (|RD:DIFFERENCE| U V)))
             ((EQUAL NEGSIGN 2) (|RD:PLUS| U V))
             (T (LIST 'MINUS (|RD:PLUS| V U))))))) 
(PUT 'ADD_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'ADD_MINUS 'DEFINED-ON-LINE '527) 
(PUT 'ADD_MINUS 'DEFINED-IN-FILE 'LINALG/SVD.RED) 
(PUT 'ADD_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD_MINUS (U)
    (PROG ()
      (COND ((ATOM U) (RETURN U))
            ((AND (EQUAL (CAR U) '|:RD:|) (GEQ (CDR U) 0)) (RETURN U))
            ((AND (EQUAL (CAR U) '|:RD:|) (LESSP (CDR U) 0))
             (RETURN (LIST 'MINUS (CONS '|:RD:| (ABS (CDR U))))))
            ((AND (EQUAL (CAR U) 'MINUS) (NUMBERP (CADR U))) (RETURN U))
            ((AND (EQUAL (CAR U) 'MINUS) (LESSP (CDADR U) 0))
             (RETURN (CONS '|:RD:| (ABS (CDADR U)))))
            ((EQUAL (CAR U) 'MINUS) (RETURN U))
            ((LESSP (CDR U) 0)
             (RETURN (LIST 'MINUS (CONS '|:RD:| (ABS (CDR U))))))
            (T (RETURN U))))) 
(ENDMODULE) 