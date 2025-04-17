(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPSVD)) 
(PUT 'SPSVD 'NUMBER-OF-ARGS 1) 
(PUT 'SPSVD 'DEFINED-ON-LINE '42) 
(PUT 'SPSVD 'DEFINED-IN-FILE 'SPARSE/SPSVD.RED) 
(PUT 'SPSVD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPSVD (A)
    (PROG (EE U V G X EPS TOLERANCE Q S F H Y TEST_F_SPLITTING CANCELLATION
           TEST_F_CONVERGENCE CONVERGENCE C Z DENOM Q_MAT I_ROUNDED_TURNED_ON
           TRANS_DONE VAL VAL2 COLS COLS2 COLS3 TMPU TMPV I J K L L1 M N
           NO_ITERS)
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
      (COND ((NOT (MATRIXP A)) (REDERR "Error in spsvd: non matrix input."))
            ((NOT (EQCAR A 'SPARSEMAT))
             (REDERR (LIST "spsvd: not a sparse matrix" A)))
            ((NOT (SP-NOTEMPTY-P (CADR A) (SPROW_DIM A)))
             (REDERR "spsvd: empty matrix")))
      (SETQ EPS
              (GET_NUM_PART
               ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                (LIST 'TIMES 1.5 (LIST 'EXPT 10 (MINUS 8))))))
      (SETQ TOLERANCE
              (GET_NUM_PART
               ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                (LIST 'EXPT 10 (MINUS 31)))))
      (COND
       ((LESSP (SPROW_DIM A) (SPCOL_DIM A))
        (PROGN (SETQ A (AEVAL (LIST 'TP A))) (SETQ TRANS_DONE T) NIL)))
      (SETQ M (SPROW_DIM A))
      (SETQ N (SPCOL_DIM A))
      (SETQ U (ERRORSET2 (LIST 'SPRD_COPY_MAT (MKQUOTE A))))
      (COND
       ((ERRORP U)
        (PROGN
         (COND (I_ROUNDED_TURNED_ON ((LAMBDA (*MSG) (OFF1 'ROUNDED)) NIL)))
         (COND (ERRMSG* (REDERR ERRMSG*))
               (T (REDERR "Error in spsvd operator")))
         NIL))
       (T (SETQ U (CAR U))))
      (SETQ V (MKEMPSPMAT N (LIST 'SPM N N)))
      (SETQ EE (MKVECT N))
      (SETQ Q (MKVECT N))
      (SETQ G (SETQ X 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
         (PUTV EE I G)
         (SETQ S 0)
         (SETQ L (PLUS I 1))
         (SETQ COLS (FINDROW TMPU I))
         (PROG (XX)
           (SETQ XX (AND COLS (CDR COLS)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ J (CAR XX))
               (SETQ VAL (CDR XX))
               (COND
                ((AND (GEQ J I) (LEQ J M))
                 (SETQ S (|SPECRD:PLUS| S (|SPECRD:EXPT| VAL 2)))))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (COND ((LESSP (GET_NUM_PART S) TOLERANCE) (SETQ G 0))
               (T
                (PROGN
                 (SETQ F (FINDELEM2 U I I))
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
                 (LETMTR3 (LIST U I I)
                  (|SPECRD:PLUS| F
                                 (COND ((ATOM G) (LIST 'MINUS G))
                                       ((EQUAL (CAR G) 'MINUS) (CADR G))
                                       (T (LIST 'MINUS G))))
                  U NIL)
                 (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
                 (SETQ COLS (FINDROW TMPU I))
                 (PROG (J)
                   (SETQ J L)
                  LAB
                   (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
                   (PROGN
                    (SETQ COLS2 (FINDROW (SP-COPY-VECT (SMTP U NIL) NIL) J))
                    (SETQ S 0)
                    (PROG (XX)
                      (SETQ XX (AND COLS (CDR COLS)))
                     LAB
                      (COND ((NULL XX) (RETURN NIL)))
                      ((LAMBDA (XX)
                         (PROGN
                          (SETQ VAL (CDR XX))
                          (SETQ K (CAR XX))
                          (COND
                           ((AND (GEQ K I) (LEQ K M))
                            (PROGN
                             (SETQ VAL2 (ATSOC K COLS2))
                             (COND
                              (VAL2
                               (SETQ S
                                       (|SPECRD:PLUS| S
                                                      (|SPECRD:TIMES| VAL
                                                                      (CDR
                                                                       VAL2))))))
                             NIL)))
                          NIL))
                       (CAR XX))
                      (SETQ XX (CDR XX))
                      (GO LAB))
                    (SETQ F (|SPECRD:QUOTIENT| S H))
                    (PROG (XX)
                      (SETQ XX (AND COLS (CDR COLS)))
                     LAB
                      (COND ((NULL XX) (RETURN NIL)))
                      ((LAMBDA (XX)
                         (PROGN
                          (SETQ VAL (CDR XX))
                          (SETQ K (CAR XX))
                          (COND
                           ((AND (GEQ K I) (LEQ K M))
                            (PROGN
                             (SETQ VAL2 (ATSOC K COLS2))
                             (COND ((NULL VAL2) (SETQ VAL2 0))
                                   (T (SETQ VAL2 (CDR VAL2))))
                             (COND
                              ((NOT (EQUAL F '(|:RD:| . 0.0)))
                               (LETMTR3 (LIST U K J)
                                (|SPECRD:PLUS| VAL2 (|SPECRD:TIMES| F VAL)) U
                                NIL)))
                             NIL)))
                          NIL))
                       (CAR XX))
                      (SETQ XX (CDR XX))
                      (GO LAB))
                    NIL)
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 NIL)))
         (PUTV Q I G)
         (SETQ S 0)
         (SETQ COLS (FINDROW U I))
         (PROG (XX)
           (SETQ XX (AND COLS (CDR COLS)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ J (CAR XX))
               (SETQ VAL (CDR XX))
               (COND
                ((AND (GEQ J L) (LEQ J N))
                 (SETQ S (|SPECRD:PLUS| S (|SPECRD:EXPT| VAL 2)))))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (COND ((LESSP (GET_NUM_PART S) TOLERANCE) (SETQ G 0))
               (T
                (PROGN
                 (SETQ F (FINDELEM2 U I (PLUS I 1)))
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
                 (LETMTR3 (LIST U I (PLUS I 1))
                  (|SPECRD:PLUS| F
                                 (COND ((ATOM G) (LIST 'MINUS G))
                                       ((EQUAL (CAR G) 'MINUS) (CADR G))
                                       (T (LIST 'MINUS G))))
                  U NIL)
                 (SETQ COLS (FINDROW U I))
                 (PROG (XX)
                   (SETQ XX (AND COLS (CDR COLS)))
                  LAB
                   (COND ((NULL XX) (RETURN NIL)))
                   ((LAMBDA (XX)
                      (PROGN
                       (SETQ J (CAR XX))
                       (SETQ VAL (CDR XX))
                       (COND
                        ((AND (GEQ J L) (LEQ J N))
                         (PUTV EE J (|SPECRD:QUOTIENT| VAL H))))
                       NIL))
                    (CAR XX))
                   (SETQ XX (CDR XX))
                   (GO LAB))
                 (PROG (J)
                   (SETQ J L)
                  LAB
                   (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                   (PROGN
                    (SETQ COLS2 (FINDROW U J))
                    (SETQ S 0)
                    (PROG (XX)
                      (SETQ XX (AND COLS (CDR COLS)))
                     LAB
                      (COND ((NULL XX) (RETURN NIL)))
                      ((LAMBDA (XX)
                         (PROGN
                          (SETQ VAL (CDR XX))
                          (SETQ K (CAR XX))
                          (COND
                           ((AND (GEQ K L) (LEQ K N))
                            (PROGN
                             (SETQ VAL2 (ATSOC K COLS2))
                             (COND
                              (VAL2
                               (SETQ S
                                       (|SPECRD:PLUS| S
                                                      (|SPECRD:TIMES| VAL
                                                                      (CDR
                                                                       VAL2))))))
                             NIL)))
                          NIL))
                       (CAR XX))
                      (SETQ XX (CDR XX))
                      (GO LAB))
                    (PROG (K)
                      (SETQ K L)
                     LAB
                      (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                      (PROGN
                       (SETQ VAL2 (ATSOC K COLS2))
                       (SETQ VAL2 (COND ((NULL VAL2) 0) (T (CDR VAL2))))
                       (SETQ VAL (GETV EE K))
                       (COND ((EQUAL VAL NIL) (SETQ VAL 0)))
                       (LETMTR3 (LIST U J K)
                        (|SPECRD:PLUS| VAL2 (|SPECRD:TIMES| S VAL)) U NIL)
                       NIL)
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
         (SETQ COLS (FINDROW U I))
         (COND
          ((NEQ (GET_NUM_PART G) 0)
           (PROGN
            (SETQ VAL (FINDELEM2 U I (PLUS I 1)))
            (SETQ H (|SPECRD:TIMES| VAL G))
            (PROG (XX)
              (SETQ XX (CDR COLS))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ J (CAR XX))
                  (SETQ VAL (CDR XX))
                  (COND
                   ((AND (GEQ J L) (LEQ J N))
                    (LETMTR3 (LIST V J I) (|SPECRD:QUOTIENT| VAL H) V NIL)))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            (SETQ COLS (FINDROW U I))
            (SETQ TMPV (SP-COPY-VECT (SMTP V NIL) NIL))
            (PROG (J)
              (SETQ J L)
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (PROGN
               (SETQ COLS2 (FINDROW TMPV J))
               (SETQ S 0)
               (PROG (XX)
                 (SETQ XX (CDR COLS))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ K (CAR XX))
                     (SETQ VAL (CDR XX))
                     (COND
                      ((AND (GEQ K L) (LEQ K N))
                       (PROGN
                        (SETQ VAL2 (ATSOC K COLS2))
                        (COND
                         (VAL2
                          (SETQ S
                                  (|SPECRD:PLUS| S
                                                 (|SPECRD:TIMES| VAL
                                                                 (CDR
                                                                  VAL2))))))
                        NIL)))
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               (SETQ COLS3 (FINDROW TMPV I))
               (PROG (K)
                 (SETQ K L)
                LAB
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                 (PROGN
                  (SETQ VAL (ATSOC K COLS3))
                  (SETQ VAL (COND ((NULL VAL) 0) (T (CDR VAL))))
                  (SETQ VAL2 (ATSOC K COLS2))
                  (SETQ VAL2 (COND ((NULL VAL2) 0) (T (CDR VAL2))))
                  (LETMTR3 (LIST V K J)
                   (|SPECRD:PLUS| VAL2 (|SPECRD:TIMES| S VAL)) V NIL)
                  NIL)
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
           (PROGN
            (LETMTR3 (LIST V I J) 0 V NIL)
            (LETMTR3 (LIST V J I) 0 V NIL)
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (LETMTR3 (LIST V I I) 1 V NIL)
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
         (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
         (SETQ TMPV (SP-COPY-VECT (SMTP V NIL) NIL))
         (SETQ L (PLUS I 1))
         (SETQ G (GETV Q I))
         (SETQ COLS (FINDROW U I))
         (PROG (XX)
           (SETQ XX (AND COLS (CDR COLS)))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ J (CAR XX))
               (COND
                ((AND (GEQ J L) (LEQ J N)) (LETMTR3 (LIST U I J) 0 U NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (COND
          ((NEQ (GET_NUM_PART G) 0)
           (PROGN
            (SETQ H (|SPECRD:TIMES| (FINDELEM2 U I I) G))
            (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
            (SETQ COLS (FINDROW TMPU I))
            (PROG (J)
              (SETQ J L)
             LAB
              (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
              (PROGN
               (SETQ COLS2 (FINDROW TMPU J))
               (SETQ S 0)
               (PROG (XX)
                 (SETQ XX (AND COLS (CDR COLS)))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ VAL (CDR XX))
                     (SETQ K (CAR XX))
                     (COND
                      ((AND (GEQ K L) (LEQ K M))
                       (PROGN
                        (SETQ VAL2 (ATSOC K COLS2))
                        (COND
                         (VAL2
                          (SETQ S
                                  (|SPECRD:PLUS| S
                                                 (|SPECRD:TIMES| VAL
                                                                 (CDR
                                                                  VAL2))))))
                        NIL)))
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               (SETQ F (|SPECRD:QUOTIENT| S H))
               (PROG (XX)
                 (SETQ XX (AND COLS (CDR COLS)))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ VAL (CDR XX))
                     (SETQ K (CAR XX))
                     (COND
                      ((AND (GEQ K I) (LEQ K M))
                       (PROGN
                        (SETQ VAL2 (ATSOC K COLS2))
                        (COND ((EQUAL VAL2 NIL) (SETQ VAL2 (CONS 0 0))))
                        (COND
                         ((NOT (EQUAL F '(MINUS (|:RD:| . 0.0))))
                          (LETMTR3 (LIST U K J)
                           (|SPECRD:PLUS| (CDR VAL2) (|SPECRD:TIMES| F VAL)) U
                           NIL)))
                        NIL)))
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
            (SETQ COLS (FINDROW TMPU I))
            (PROG (XX)
              (SETQ XX (AND COLS (CDR COLS)))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ J (CAR XX))
                  (SETQ VAL (CDR XX))
                  (COND
                   ((AND (GEQ J I) (LEQ J M))
                    (LETMTR3 (LIST U J I) (|SPECRD:QUOTIENT| VAL G) U NIL)))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL))
          (T
           (PROG (XX)
             (SETQ XX (AND COLS (CDR COLS)))
            LAB
             (COND ((NULL XX) (RETURN NIL)))
             ((LAMBDA (XX)
                (PROGN
                 (SETQ J (CAR XX))
                 (COND
                  ((AND (GEQ J I) (LEQ J M)) (LETMTR3 (LIST U J I) 0 U NIL)))
                 NIL))
              (CAR XX))
             (SETQ XX (CDR XX))
             (GO LAB))))
         (LETMTR3 (LIST U I I) (|SPECRD:PLUS| (FINDELEM2 U I I) 1) U NIL)
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
         (SETQ TMPU (SP-COPY-VECT (SMTP U NIL) NIL))
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
               (SETQ COLS (FINDROW TMPU I))
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
                  (PROG (XX)
                    (SETQ XX (CDR COLS))
                   LAB
                    (COND ((NULL XX) (RETURN NIL)))
                    ((LAMBDA (XX)
                       (PROGN
                        (SETQ J (CAR XX))
                        (SETQ VAL (CDR XX))
                        (COND
                         ((LEQ J M)
                          (PROGN
                           (SETQ Y (FINDELEM2 U J L1))
                           (LETMTR3 (LIST U J L1)
                            (|SPECRD:PLUS| (|SPECRD:TIMES| Y C)
                                           (|SPECRD:TIMES| VAL S))
                            U NIL)
                           NIL)))
                        NIL))
                     (CAR XX))
                    (SETQ XX (CDR XX))
                    (GO LAB))
                  (SETQ I (PLUS I 1))
                  NIL)))
               NIL)
              (GO WHILELABEL))
            NIL)))
         (COND
          ((GEQ NO_ITERS 30)
           (REDERR
            "Error in spsvd: Emergency stop, maximum number of iterations reached without convergence")))
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
                                      (|SPECRD:PLUS|
                                       (|SPECRD:QUOTIENT| Y DENOM)
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
                  (SETQ Z (FINDELEM2 V J I))
                  (SETQ X (FINDELEM2 V J (DIFFERENCE I 1)))
                  (LETMTR3 (LIST V J (DIFFERENCE I 1))
                   (|SPECRD:PLUS| (|SPECRD:TIMES| X C) (|SPECRD:TIMES| Z S)) V
                   NIL)
                  (LETMTR3 (LIST V J I)
                   (|SPECRD:DIFFERENCE| (|SPECRD:TIMES| Z C)
                                        (|SPECRD:TIMES| X S))
                   V NIL)
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
                  (SETQ Y (FINDELEM2 U J (DIFFERENCE I 1)))
                  (SETQ Z (FINDELEM2 U J I))
                  (LETMTR3 (LIST U J (DIFFERENCE I 1))
                   (|SPECRD:PLUS| (|SPECRD:TIMES| Y C) (|SPECRD:TIMES| Z S)) U
                   NIL)
                  (LETMTR3 (LIST U J I)
                   (|SPECRD:DIFFERENCE| (|SPECRD:TIMES| Z C)
                                        (|SPECRD:TIMES| Y S))
                   U NIL)
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
            (SETQ TMPV (SP-COPY-VECT (SMTP V NIL) NIL))
            (COND
             ((LESSP (GET_NUM_PART Z) 0)
              (PROGN
               (PUTV Q K
                     (COND ((ATOM Z) (LIST 'MINUS Z))
                           ((EQUAL (CAR Z) 'MINUS) (CADR Z))
                           (T (LIST 'MINUS Z))))
               (SETQ COLS (FINDROW TMPV K))
               (PROG (XX)
                 (SETQ XX (CDR COLS))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ J (CAR XX))
                     (SETQ VAL (CDR XX))
                     (COND
                      ((LEQ J N)
                       (LETMTR3 (LIST V J K)
                        (COND ((ATOM VAL) (LIST 'MINUS VAL))
                              ((EQUAL (CAR VAL) 'MINUS) (CADR VAL))
                              (T (LIST 'MINUS VAL)))
                        V NIL)))
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               NIL)))
            (SETQ K (DIFFERENCE K 1))
            (SETQ NO_ITERS 0)
            NIL)))
         NIL)
        (GO WHILELABEL))
      (SETQ Q_MAT (SPQ_TO_DIAG_MATRIX Q))
      (COND (I_ROUNDED_TURNED_ON ((LAMBDA (*MSG) (OFF1 'ROUNDED)) NIL)))
      (SETQ V (SPDEN_TO_SP V))
      (SETQ U (SPDEN_TO_SP U))
      (COND (TRANS_DONE (RETURN (LIST 'LIST V Q_MAT U)))
            (T (RETURN (LIST 'LIST U Q_MAT V)))))) 
(FLAG '(SPSVD) 'OPFN) 
(PUT 'SPQ_TO_DIAG_MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'SPQ_TO_DIAG_MATRIX 'DEFINED-ON-LINE '455) 
(PUT 'SPQ_TO_DIAG_MATRIX 'DEFINED-IN-FILE 'SPARSE/SPSVD.RED) 
(PUT 'SPQ_TO_DIAG_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPQ_TO_DIAG_MATRIX (Q)
    (PROG (Q_MAT I SQ_DIM_Q VAL)
      (SETQ I 0)
      (SETQ SQ_DIM_Q 0)
      (SETQ VAL 0)
      (SETQ SQ_DIM_Q (UPBV Q))
      (SETQ Q_MAT (MKEMPSPMAT SQ_DIM_Q (LIST 'SPM SQ_DIM_Q SQ_DIM_Q)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_DIM_Q I)) (RETURN NIL)))
        (PROGN
         (SETQ VAL (GETV Q I))
         (COND ((EQUAL VAL '(|:RD:| . 0.0)) NIL)
               (T (LETMTR3 (LIST Q_MAT I I) VAL Q_MAT NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN Q_MAT))) 
(PUT 'SPDEN_TO_SP 'NUMBER-OF-ARGS 1) 
(PUT 'SPDEN_TO_SP 'DEFINED-ON-LINE '476) 
(PUT 'SPDEN_TO_SP 'DEFINED-IN-FILE 'SPARSE/SPSVD.RED) 
(PUT 'SPDEN_TO_SP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPDEN_TO_SP (LIST)
    (PROG (TL NMAT VAL COLS J)
      (SETQ TL (CADDR LIST))
      (SETQ NMAT (MKEMPSPMAT (CADR TL) TL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CADR TL) I)) (RETURN NIL)))
        (PROGN
         (SETQ COLS (FINDROW LIST I))
         (PROG (XX)
           (SETQ XX (CDR COLS))
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ J (CAR XX))
               (SETQ VAL (REVAL1 (CDR XX) T))
               (COND ((EQUAL VAL '(|:RD:| . 0.0)) NIL)
                     (T (LETMTR3 (LIST NMAT I J) VAL NMAT NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NMAT))) 
(PUT 'SPRD_COPY_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'SPRD_COPY_MAT 'DEFINED-ON-LINE '493) 
(PUT 'SPRD_COPY_MAT 'DEFINED-IN-FILE 'SPARSE/SPSVD.RED) 
(PUT 'SPRD_COPY_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPRD_COPY_MAT (A)
    (PROG (C R ROW_DIM COLUMN_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COLUMN_DIM 0)
      (SETQ C (SP-COPY-VECT A NIL))
      (SETQ ROW_DIM (SPROW_DIM A))
      (SETQ COLUMN_DIM (SPCOL_DIM A))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW_DIM I)) (RETURN NIL)))
        (PROGN
         (SETQ R (GETV (CADR C) I))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLUMN_DIM J)) (RETURN NIL)))
           ((LAMBDA (VAL XX)
              (PROGN
               (COND
                ((NOT (NULL VAL))
                 (PROGN
                  (SETQ XX
                          (COND ((FIXP (CDR VAL)) (CDR VAL))
                                (T (REVAL1 (CDR VAL) T))))
                  (COND
                   ((AND (NOT (FIXP XX)) (NOT (EQCAR XX '|:RD:|)))
                    (RERROR 'SPARSE 2
                            (LIST
                             "spsvd: Non-numeric matrix element at position ("
                             I "," J ")")))
                   (T (RPLACD VAL XX)))
                  NIL)))))
            (ATSOC J R) NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN C))) 
(PUT 'SPPSEUDO_INVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'SPPSEUDO_INVERSE 'DEFINED-ON-LINE '523) 
(PUT 'SPPSEUDO_INVERSE 'DEFINED-IN-FILE 'SPARSE/SPSVD.RED) 
(PUT 'SPPSEUDO_INVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPPSEUDO_INVERSE (IN_MAT)
    (PROG (PSU_INV SVD_LIST A B C)
      (SETQ SVD_LIST (CDR (SPSVD IN_MAT)))
      (SETQ A (CAR SVD_LIST))
      (SETQ C (CADDR SVD_LIST))
      (SETQ B (CADR SVD_LIST))
      (SETQ A (AEVAL (LIST 'TP A)))
      (SETQ B (AEVAL (LIST 'QUOTIENT 1 B)))
      (SETQ PSU_INV (AEVAL (LIST 'TIMES C B A)))
      (RETURN PSU_INV))) 
(FLAG '(SPPSEUDO_INVERSE) 'OPFN) 
(ENDMODULE) 