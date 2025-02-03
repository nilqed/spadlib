(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATSM)) 
(PUT 'MATSM* 'NUMBER-OF-ARGS 2) 
(PUT 'MATSM* 'DEFINED-ON-LINE '35) 
(PUT 'MATSM* 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MATSM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATSM* (U V) (MATSM*1 (MATSM U))) 
(PUT 'MATSM*1 'NUMBER-OF-ARGS 1) 
(PUT 'MATSM*1 'DEFINED-ON-LINE '49) 
(PUT 'MATSM*1 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MATSM*1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATSM*1 (U)
    (PROG ()
      (SETQ U
              (CONS 'MAT
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J U)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ K J)
                                            (COND ((NULL K) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (K)
                                                                (*Q2A1
                                                                 (SUBS2* K)
                                                                 *NOSQ))
                                                              (CAR K))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ K (CDR K))
                                            (COND
                                             ((NULL K) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (K)
                                                        (*Q2A1 (SUBS2* K)
                                                               *NOSQ))
                                                      (CAR K))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ K J)
                                    (COND ((NULL K) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (K)
                                                        (*Q2A1 (SUBS2* K)
                                                               *NOSQ))
                                                      (CAR K))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ K (CDR K))
                                    (COND ((NULL K) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (K)
                                                (*Q2A1 (SUBS2* K) *NOSQ))
                                              (CAR K))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ *SUB2 NIL)
      (RETURN U))) 
(PUT 'MATSM 'NUMBER-OF-ARGS 1) 
(PUT 'MATSM 'DEFINED-ON-LINE '58) 
(PUT 'MATSM 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MATSM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATSM (U)
    (PROG (X Y)
      (PROG (J)
        (SETQ J (NSSIMP U 'MATRIX))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ Y (MULTSM (CAR J) (MATSM1 (CDR J))))
            (SETQ X (COND ((NULL X) Y) (T (ADDM X Y))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN X))) 
(PUT 'MATSM1 'NUMBER-OF-ARGS 1) 
(PUT 'MATSM1 'DEFINED-ON-LINE '66) 
(PUT 'MATSM1 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MATSM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATSM1 (U)
    (PROG (X Y Z N)
      (SETQ N 0)
     A
      (COND ((NULL U) (RETURN Z)) ((EQCAR (CAR U) '*DIV) (GO D))
            ((ATOM (CAR U)) (GO ER)) ((EQ (CAAR U) 'MAT) (GO C1))
            ((AND (FLAGP (CAAR U) 'MATMAPFN) (CDAR U)
                  (EQ (GETRTYPE (CADAR U)) 'MATRIX))
             (SETQ X (MATSM (MATRIXMAP (CAR U) NIL))))
            ((SETQ X (GET (CAAR U) 'PSOPFN))
             (PROGN
              (SETQ X (LISPAPPLY X (LIST (CDAR U))))
              (COND ((EQCAR X 'MAT) (SETQ X (MATSM X))))))
            (T
             (PROGN
              (SETQ X (LISPAPPLY (CAAR U) (CDAR U)))
              (COND ((EQCAR X 'MAT) (SETQ X (MATSM X)))))))
     B
      (SETQ Z
              (COND ((NULL Z) X)
                    ((AND (NULL (CDR Z)) (NULL (CDAR Z))) (MULTSM (CAAR Z) X))
                    (T (MULTM X Z))))
     C
      (SETQ U (CDR U))
      (GO A)
     C1
      (COND ((NOT (LCHK (CDAR U))) (RERROR 'MATRIX 3 "Matrix mismatch")))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CDAR U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ K J)
                                      (COND ((NULL K) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K) (XSIMP K))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K) (XSIMP K)) (CAR K))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROG (K FORALL-RESULT FORALL-ENDPTR)
                              (SETQ K J)
                              (COND ((NULL K) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K) (XSIMP K)) (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (K) (XSIMP K)) (CAR K))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GO B)
     D
      (SETQ Y (MATSM (CADAR U)))
      (COND
       ((NEQ (SETQ N (LENGTH (CAR Y))) (LENGTH Y))
        (RERROR 'MATRIX 4 "Non square matrix"))
       ((AND Z (NEQ N (LENGTH Z))) (RERROR 'MATRIX 5 "Matrix mismatch"))
       ((CDDAR U) (GO H)) ((AND (NULL (CDR Y)) (NULL (CDAR Y))) (GO E)))
      (SETQ X SUBFG*)
      (SETQ SUBFG* NIL)
      (COND ((NULL Z) (SETQ Z (APPLY1 (GET 'MAT 'INVERSEFN) Y)))
            ((NULL (SETQ X (GET 'MAT 'LNRSOLVEFN)))
             (SETQ Z (MULTM (APPLY1 (GET 'MAT 'INVERSEFN) Y) Z)))
            (T (SETQ Z (APPLY2 (GET 'MAT 'LNRSOLVEFN) Y Z))))
      (SETQ SUBFG* X)
      (SETQ Z
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J Z)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ K J)
                                      (COND ((NULL K) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (K)
                                                          (PROGN
                                                           (SETQ *SUB2 T)
                                                           (SUBS2 K)))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (PROGN
                                                   (SETQ *SUB2 T)
                                                   (SUBS2 K)))
                                                (CAR K))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROG (K FORALL-RESULT FORALL-ENDPTR)
                              (SETQ K J)
                              (COND ((NULL K) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (PROGN
                                                   (SETQ *SUB2 T)
                                                   (SUBS2 K)))
                                                (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K)
                                          (PROGN (SETQ *SUB2 T) (SUBS2 K)))
                                        (CAR K))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GO C)
     E
      (COND ((NULL (CAAAR Y)) (RERROR 'MATRIX 6 "Zero divisor")))
      (SETQ Y (CONS (CDR (CAAR Y)) (CAR (CAAR Y))))
      (SETQ Z (COND ((NULL Z) (LIST (LIST Y))) (T (MULTSM Y Z))))
      (GO C)
     H
      (COND ((NULL Z) (SETQ Z (GENERATEIDENT N))))
      (GO C)
     ER
      (RERROR 'MATRIX 7 (LIST "Matrix" (CAR U) "not set")))) 
(PUT 'LCHK 'NUMBER-OF-ARGS 1) 
(PUT 'LCHK 'DEFINED-ON-LINE '117) 
(PUT 'LCHK 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'LCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LCHK (U)
    (PROG (N)
      (SETQ N 0)
      (COND ((OR (NULL U) (ATOM (CAR U))) (RETURN NIL)))
      (SETQ N (LENGTH (CAR U)))
      (PROG ()
       REPEATLABEL
        (SETQ U (CDR U))
        (COND
         ((NOT (OR (NULL U) (ATOM (CAR U)) (NEQ (LENGTH (CAR U)) N)))
          (GO REPEATLABEL))))
      (RETURN (NULL U)))) 
(PUT 'ADDM 'NUMBER-OF-ARGS 2) 
(PUT 'ADDM 'DEFINED-ON-LINE '126) 
(PUT 'ADDM 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'ADDM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDM (U V)
    (COND ((EQUAL V '(((NIL . 1)))) U) ((EQUAL U '(((NIL . 1)))) V)
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J (ADDM1 U V (FUNCTION CONS)))
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (J)
                                 (ADDM1 (CAR J) (CDR J) (FUNCTION ADDSQ)))
                               (CAR J))
                              NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (J) (ADDM1 (CAR J) (CDR J) (FUNCTION ADDSQ)))
                       (CAR J))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'ADDM1 'NUMBER-OF-ARGS 3) 
(PUT 'ADDM1 'DEFINED-ON-LINE '135) 
(PUT 'ADDM1 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'ADDM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDM1 (U V W)
    (COND ((AND (NULL U) (NULL V)) NIL)
          ((OR (NULL U) (NULL V)) (RERROR 'MATRIX 8 "Matrix mismatch"))
          (T (CONS (APPLY2 W (CAR U) (CAR V)) (ADDM1 (CDR U) (CDR V) W))))) 
(PUT 'TP 'NUMBER-OF-ARGS 1) 
(PUT 'TP 'DEFINED-ON-LINE '140) 
(PUT 'TP 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'TP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TP (U) (TP1 (MATSM U))) 
(PUT 'TP 'RTYPEFN 'GETRTYPECAR) 
(PUT 'TP1 'NUMBER-OF-ARGS 1) 
(PUT 'TP1 'DEFINED-ON-LINE '144) 
(PUT 'TP1 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'TP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TP1 (U)
    (PROG (V W X Y Z)
      (SETQ V (SETQ W (LIST NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR U)) (RETURN NIL)))
        (PROGN
         (SETQ X U)
         (SETQ Y (SETQ Z (LIST NIL)))
         (PROG ()
          WHILELABEL
           (COND ((NOT X) (RETURN NIL)))
           (PROGN
            (SETQ Z (CDR (RPLACD Z (LIST (CAAR X)))))
            (SETQ X (CDR (RPLACA X (CDAR X)))))
           (GO WHILELABEL))
         (SETQ W (CDR (RPLACD W (LIST (CDR Y))))))
        (GO WHILELABEL))
      (RETURN (CDR V)))) 
(PUT 'SCALPROD 'NUMBER-OF-ARGS 2) 
(PUT 'SCALPROD 'DEFINED-ON-LINE '159) 
(PUT 'SCALPROD 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'SCALPROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SCALPROD (U V)
    (COND ((AND (NULL U) (NULL V)) (CONS NIL 1))
          ((OR (NULL U) (NULL V)) (RERROR 'MATRIX 9 "Matrix mismatch"))
          (T (ADDSQ (MULTSQ (CAR U) (CAR V)) (SCALPROD (CDR U) (CDR V)))))) 
(PUT 'MULTM 'NUMBER-OF-ARGS 2) 
(PUT 'MULTM 'DEFINED-ON-LINE '165) 
(PUT 'MULTM 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MULTM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTM (U V)
    (COND
     ((AND (EQUAL (LENGTH U) 1) (EQUAL (LENGTH (CAR U)) 1))
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J V)
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROG (K FORALL-RESULT FORALL-ENDPTR)
                              (SETQ K J)
                              (COND ((NULL K) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (MULTSQ (CAAR U) K))
                                                (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K) (MULTSQ (CAAR U) K))
                                        (CAR K))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL)))
       LOOPLABEL
        (SETQ J (CDR J))
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (J)
                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                      (SETQ K J)
                      (COND ((NULL K) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K) (MULTSQ (CAAR U) K))
                                        (CAR K))
                                       NIL)))
                     LOOPLABEL
                      (SETQ K (CDR K))
                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (K) (MULTSQ (CAAR U) K)) (CAR K))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR J))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     ((AND (EQUAL (LENGTH V) 1) (EQUAL (LENGTH (CAR V)) 1))
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J U)
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROG (K FORALL-RESULT FORALL-ENDPTR)
                              (SETQ K J)
                              (COND ((NULL K) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K)
                                                  (MULTSQ K (CAAR V)))
                                                (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K) (MULTSQ K (CAAR V)))
                                        (CAR K))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL)))
       LOOPLABEL
        (SETQ J (CDR J))
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (J)
                    (PROG (K FORALL-RESULT FORALL-ENDPTR)
                      (SETQ K J)
                      (COND ((NULL K) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (K) (MULTSQ K (CAAR V)))
                                        (CAR K))
                                       NIL)))
                     LOOPLABEL
                      (SETQ K (CDR K))
                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (K) (MULTSQ K (CAAR V))) (CAR K))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR J))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      ((LAMBDA (X)
         (PROG (Y FORALL-RESULT FORALL-ENDPTR)
           (SETQ Y U)
           (COND ((NULL Y) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (Y)
                               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ K X)
                                 (COND ((NULL K) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (K)
                                                     (SUBS2 (SCALPROD Y K)))
                                                   (CAR K))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ K (CDR K))
                                 (COND ((NULL K) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K) (SUBS2 (SCALPROD Y K)))
                                           (CAR K))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR Y))
                            NIL)))
          LOOPLABEL
           (SETQ Y (CDR Y))
           (COND ((NULL Y) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (Y)
                       (PROG (K FORALL-RESULT FORALL-ENDPTR)
                         (SETQ K X)
                         (COND ((NULL K) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (K) (SUBS2 (SCALPROD Y K)))
                                           (CAR K))
                                          NIL)))
                        LOOPLABEL
                         (SETQ K (CDR K))
                         (COND ((NULL K) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K) (SUBS2 (SCALPROD Y K))) (CAR K))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
                     (CAR Y))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
       (TP1 V))))) 
(PUT 'MULTSM 'NUMBER-OF-ARGS 2) 
(PUT 'MULTSM 'DEFINED-ON-LINE '176) 
(PUT 'MULTSM 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MULTSM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTSM (U V)
    (COND ((EQUAL U (CONS 1 1)) V)
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J V)
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (J)
                                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ K J)
                                   (COND ((NULL K) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (K) (MULTSQ K U))
                                                     (CAR K))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ K (CDR K))
                                   (COND ((NULL K) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (K) (MULTSQ K U)) (CAR K))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR J))
                              NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (J)
                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                           (SETQ K J)
                           (COND ((NULL K) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (K) (MULTSQ K U)) (CAR K))
                                            NIL)))
                          LOOPLABEL
                           (SETQ K (CDR K))
                           (COND ((NULL K) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (K) (MULTSQ K U)) (CAR K))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR J))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'MATSUB 'NUMBER-OF-ARGS 2) 
(PUT 'MATSUB 'DEFINED-ON-LINE '185) 
(PUT 'MATSUB 'DEFINED-IN-FILE 'MATRIX/MATSM.RED) 
(PUT 'MATSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATSUB (U V)
    (CONS 'MAT
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (CDR V))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ Y X)
                                  (COND ((NULL Y) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (Y) (SUBEVAL1 U Y))
                                                    (CAR Y))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ Y (CDR Y))
                                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (Y) (SUBEVAL1 U Y))
                                            (CAR Y))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X)
                        (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                          (SETQ Y X)
                          (COND ((NULL Y) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (Y) (SUBEVAL1 U Y))
                                            (CAR Y))
                                           NIL)))
                         LOOPLABEL
                          (SETQ Y (CDR Y))
                          (COND ((NULL Y) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (Y) (SUBEVAL1 U Y)) (CAR Y))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'MATRIX 'SUBFN 'MATSUB) 
(ENDMODULE) 