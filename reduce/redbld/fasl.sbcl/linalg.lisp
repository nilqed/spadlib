(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LINALG)) 
(LOAD_PACKAGE (LIST 'MATRIX)) 
(CREATE-PACKAGE
 '(LINALG LAMATRIX GRAMSCHM LUDECOM CHOLESKY SVD SIMPLEX FSIMPLEX TADJOINT)
 '(CONTRIB LINALG)) 
(SWITCH (LIST 'FAST_LA)) 
(DE MY_REVAL (N) (COND ((FIXP N) N) (T (REVAL1 N T)))) 
(PUT 'MY_REVAL 'NUMBER-OF-ARGS 1) 
(PUT 'MY_REVAL 'DEFINED-ON-LINE '107) 
(PUT 'MY_REVAL 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MY_REVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MY_REVAL 'INLINE '(LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))) 
(PUT 'SWAP_ELT 'NUMBER-OF-ARGS 3) 
(PUT 'SWAP_ELT 'DEFINED-ON-LINE '115) 
(PUT 'SWAP_ELT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SWAP_ELT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAP_ELT (IN_LIST ELT1 ELT2)
    (PROG (BUCKET)
      (SETQ BUCKET (NTH IN_LIST ELT1))
      (SETCAR (PNTH IN_LIST ELT1) (NTH IN_LIST ELT2))
      (SETCAR (PNTH IN_LIST ELT2) BUCKET))) 
(PUT 'ROW_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'ROW_DIM 'DEFINED-ON-LINE '130) 
(PUT 'ROW_DIM 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ROW_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROW_DIM (IN_MAT)
    (PROG ()
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in row_dim: input should be a matrix.")))
      (RETURN (FIRST (SIZE_OF_MATRIX IN_MAT))))) 
(PUT 'COLUMN_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'COLUMN_DIM 'DEFINED-ON-LINE '142) 
(PUT 'COLUMN_DIM 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'COLUMN_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLUMN_DIM (IN_MAT)
    (PROG ()
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in column_dim: input should be a matrix.")))
      (RETURN (SECOND (SIZE_OF_MATRIX IN_MAT))))) 
(FLAG '(ROW_DIM |,| COLUMN_DIM) 'OPFN) 
(PUT 'MATRIXP 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIXP 'DEFINED-ON-LINE '156) 
(PUT 'MATRIXP 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MATRIXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIXP (A) (COND ((NOT (EQCAR A 'MAT)) NIL) (T T))) 
(FLAG '(MATRIXP) 'BOOLEAN) 
(FLAG '(MATRIXP) 'OPFN) 
(PUT 'SIZE_OF_MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'SIZE_OF_MATRIX 'DEFINED-ON-LINE '167) 
(PUT 'SIZE_OF_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SIZE_OF_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIZE_OF_MATRIX (A)
    (PROG (ROW_DIM COLUMN_DIM)
      (SETQ ROW_DIM 0)
      (SETQ COLUMN_DIM 0)
      (SETQ ROW_DIM (PLUS (MINUS 1) (LENGTH A)))
      (SETQ COLUMN_DIM (LENGTH (CADR A)))
      (RETURN (LIST ROW_DIM COLUMN_DIM)))) 
(PUT 'COMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'COMPANION 'DEFINED-ON-LINE '180) 
(PUT 'COMPANION 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'COMPANION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPANION (POLY X)
    (PROG (MAT1 N)
      (SETQ N 0)
      (SETQ N (DEG POLY X))
      (COND
       ((NEQ
         ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T)))) (COEFFN POLY X N))
         1)
        (MSGPRI "Error in companion(first argument): Polynomial" POLY
                "is not monic." NIL T)))
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
(PUT 'FIND_COMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'FIND_COMPANION 'DEFINED-ON-LINE '212) 
(PUT 'FIND_COMPANION 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'FIND_COMPANION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIND_COMPANION (R X)
    (PROG (P ROWDIM K)
      (SETQ ROWDIM 0)
      (SETQ K 0)
      (COND
       ((NOT (MATRIXP R))
        (REDERR
         (LIST
          "Error in find_companion(first argument): should be a matrix."))))
      (SETQ ROWDIM (ROW_DIM R))
      (SETQ K 2)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LEQ K ROWDIM) (EQUAL (GETMAT R K (DIFFERENCE K 1)) 1)))
          (RETURN NIL)))
        (SETQ K (PLUS K 1))
        (GO WHILELABEL))
      (SETQ P 0)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE K 1) J)) (RETURN NIL)))
        (PROGN
         (SETQ P
                 (LIST 'PLUS P
                       (LIST 'TIMES (LIST 'MINUS (GETMAT R J (DIFFERENCE K 1)))
                             (LIST 'EXPT X (DIFFERENCE J 1)))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ P (LIST 'PLUS P (LIST 'EXPT X (DIFFERENCE K 1))))
      (RETURN P))) 
(FLAG '(COMPANION |,| FIND_COMPANION) 'OPFN) 
(PUT 'COMPANION 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'JORDAN_BLOCK 'NUMBER-OF-ARGS 2) 
(PUT 'JORDAN_BLOCK 'DEFINED-ON-LINE '239) 
(PUT 'JORDAN_BLOCK 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'JORDAN_BLOCK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JORDAN_BLOCK (CONST MAT_DIM)
    (PROG (JB)
      (COND
       ((NOT (FIXP MAT_DIM))
        (REDERR
         "Error in jordan_block(second argument): should be an integer.")))
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
(FLAG '(JORDAN_BLOCK) 'OPFN) 
(PUT 'JORDAN_BLOCK 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'SUB_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'SUB_MATRIX 'DEFINED-ON-LINE '268) 
(PUT 'SUB_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SUB_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUB_MATRIX (A ROW_LIST COL_LIST)
    (PROG (NEW_MAT)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP A)))
        (REDERR "Error in sub_matrix(first argument): should be a matrix.")))
      (SETQ NEW_MAT (STACK_ROWS A ROW_LIST))
      (SETQ NEW_MAT (AUGMENT_COLUMNS NEW_MAT COL_LIST))
      (RETURN NEW_MAT))) 
(RTYPECAR (LIST 'SUB_MATRIX)) 
(PUT 'COPY_INTO 'NUMBER-OF-ARGS 4) 
(PUT 'COPY_INTO 'DEFINED-ON-LINE '288) 
(PUT 'COPY_INTO 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'COPY_INTO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COPY_INTO (BB AA P Q)
    (PROG (A B M N R C)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ C 0)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP BB))
           (REDERR "Error in copy_into(first argument): should be a matrix.")))
         (COND
          ((NOT (MATRIXP AA))
           (REDERR
            "Error in copy_into(second argument): should be a matrix.")))
         (COND
          ((NOT (FIXP P))
           (REDERR
            "Error in copy_into(third argument): should be an integer.")))
         (COND
          ((NOT (FIXP Q))
           (REDERR
            "Error in copy_into(fourth argument): should be an integer.")))
         (COND
          ((OR (EQUAL P 0) (EQUAL Q 0))
           (PROGN
            (PRIN2T
             "***** Error in copy_into: 0 is out of bounds for matrices.")
            (PRIN2T
             "      The top left element is labelled (1,1) and not (0,0).")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ M (ROW_DIM AA))
      (SETQ N (COLUMN_DIM AA))
      (SETQ R (ROW_DIM BB))
      (SETQ C (COLUMN_DIM BB))
      (COND
       ((AND (NOT *FAST_LA)
             (OR (GREATERP (PLUS R (DIFFERENCE P 1)) M)
                 (GREATERP (PLUS C (DIFFERENCE Q 1)) N)))
        (PROGN
         (COND
          ((AND (LESSP (TIMES M N) 26) (LESSP (TIMES R C) 26))
           (PROGN
            (PRIN2T "***** Error in copy_into: the matrix")
            (MATPRI BB)
            (PRIN2T "      does not fit into")
            (MATPRI AA)
            (PRIN2 "      at position ")
            (PRIN2 P)
            (PRIN2 ",")
            (PRIN2 Q)
            (PRIN2T ".")
            (RETURN NIL)
            NIL))
          (T
           (PROGN
            (PRIN2 "***** Error in copy_into: first matrix does not fit ")
            (PRIN2 "      into second matrix at defined position.")
            (RETURN NIL)
            NIL)))
         NIL)))
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
(FLAG '(COPY_INTO) 'OPFN) 
(RTYPECAR (LIST 'COPY_INTO)) 
(PUT 'COPY_MAT 'NUMBER-OF-ARGS 1) 
(PUT 'COPY_MAT 'DEFINED-ON-LINE '372) 
(PUT 'COPY_MAT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'COPY_MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPY_MAT (U)
    (COND ((PAIRP U) (CONS (COPY_MAT (CAR U)) (COPY_MAT (CDR U)))) (T U))) 
(PUT 'DIAGONAL1 'NUMBER-OF-ARGS 1) 
(PUT 'DIAGONAL1 'DEFINED-ON-LINE '377) 
(PUT 'DIAGONAL1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'DIAGONAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIAGONAL1 (MAT_LIST)
    (PROG (DIAG_MAT)
      (COND
       ((AND (PAIRP MAT_LIST) (PAIRP (CAR MAT_LIST))
             (EQUAL (CAAR MAT_LIST) 'LIST))
        (SETQ MAT_LIST (CDAR MAT_LIST))))
      (SETQ MAT_LIST
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT MAT_LIST)
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
      (PROG (ELT)
        (SETQ ELT MAT_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((AND (MATRIXP ELT) (NOT (SQUAREP ELT)))
              (PROGN
               (COND
                ((OR (LESSP (ROW_DIM ELT) 5) (GREATERP (COLUMN_DIM ELT) 5))
                 (PROGN
                  (PRIN2T "***** Error in diagonal: ")
                  (MATPRI ELT)
                  (PRIN2T "      is not a square matrix.")
                  (REDERR "")
                  NIL))
                (T
                 (REDERR
                  "Error in diagonal: input contains non square matrix.")))
               NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ DIAG_MAT (DIAG (LIST MAT_LIST)))
      (RETURN DIAG_MAT))) 
(PUT 'DIAGONAL 'PSOPFN 'DIAGONAL1) 
(PUT 'DIAGONAL 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'DIAG 'NUMBER-OF-ARGS 1) 
(PUT 'DIAG 'DEFINED-ON-LINE '414) 
(PUT 'DIAG 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
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
          ((OR (EQUAL (AEVAL* (LIST 'LENGTH ARG)) 1) (EQCAR ARG 'QUOTIENT))
           (SETQ BIGSIZE (PLUS BIGSIZE 1)))
          (T (PROGN (SETQ BIGSIZE (PLUS BIGSIZE (ROW_DIM ARG))) NIL)))
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
          ((OR (EQUAL (AEVAL* (LIST 'LENGTH ARG)) 1) (EQCAR ARG 'QUOTIENT))
           (PROGN
            (SETMAT BIGA AIDX AIDX ARG)
            (SETQ AIDX (PLUS AIDX 1))
            (SETQ INPUT (CDR INPUT))
            NIL))
          (T
           (PROGN
            (SETQ SMALLSIZE (ROW_DIM ARG))
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
(PUT 'BAND_MATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'BAND_MATRIX 'DEFINED-ON-LINE '502) 
(PUT 'BAND_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'BAND_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BAND_MATRIX (ELT_LIST SQ_SIZE)
    (PROG (BAND_MATRIX I J NO_ELTS MIDDLE_POS)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ NO_ELTS 0)
      (SETQ MIDDLE_POS 0)
      (COND
       ((NOT (FIXP SQ_SIZE))
        (REDERR
         "Error in band_matrix(second argument): should be an integer.")))
      (COND ((ATOM ELT_LIST) (SETQ ELT_LIST (LIST ELT_LIST)))
            ((EQUAL (CAR ELT_LIST) 'LIST) (SETQ ELT_LIST (CDR ELT_LIST)))
            (T
             (REDERR
              "Error in band_matrix(first argument): should be single value or list.")))
      (SETQ NO_ELTS (LENGTH ELT_LIST))
      (COND
       ((EVENP NO_ELTS)
        (REDERR
         "Error in band matrix(first argument): number of elements must be odd.")))
      (SETQ MIDDLE_POS (REVAL1 (LIST 'QUOTIENT (PLUS NO_ELTS 1) 2) T))
      (COND
       ((GREATERP
         (COND ((FIXP MIDDLE_POS) MIDDLE_POS) (T (REVAL1 MIDDLE_POS T)))
         SQ_SIZE)
        (REDERR
         "Error in band_matrix: too many elements. Band matrix is overflowing."))
       (T (SETQ BAND_MATRIX (MKMATRIX SQ_SIZE SQ_SIZE))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE SQ_SIZE J)) (RETURN NIL)))
           (PROGN
            (COND
             ((AND (GREATERP (PLUS (DIFFERENCE MIDDLE_POS I) J) 0)
                   (LEQ (PLUS (DIFFERENCE MIDDLE_POS I) J) NO_ELTS))
              (SETMAT BAND_MATRIX I J
               (NTH ELT_LIST (PLUS (DIFFERENCE MIDDLE_POS I) J)))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN BAND_MATRIX))) 
(FLAG '(BAND_MATRIX) 'OPFN) 
(PUT 'BAND_MATRIX 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'MAKE_IDENTITY 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_IDENTITY 'DEFINED-ON-LINE '541) 
(PUT 'MAKE_IDENTITY 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MAKE_IDENTITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_IDENTITY (SQ_SIZE)
    (COND
     ((AND (NOT *FAST_LA) (NOT (FIXP SQ_SIZE)))
      (REDERR "Error in make_identity: non integer input."))
     (T
      (CONS 'MAT
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 1)
              (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J 1)
                                 (COND
                                  ((MINUSP (DIFFERENCE SQ_SIZE J))
                                   (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (COND ((EQUAL I J) 1) (T 0))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (PLUS2 J 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE SQ_SIZE J))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS (COND ((EQUAL I J) 1) (T 0))
                                               NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))
                               NIL)))
             LOOPLABEL
              (SETQ I (PLUS2 I 1))
              (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J 1)
                         (COND ((MINUSP (DIFFERENCE SQ_SIZE J)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS (COND ((EQUAL I J) 1) (T 0))
                                               NIL)))
                        LOOPLABEL
                         (SETQ J (PLUS2 J 1))
                         (COND
                          ((MINUSP (DIFFERENCE SQ_SIZE J))
                           (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS (COND ((EQUAL I J) 1) (T 0)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(FLAG '(MAKE_IDENTITY) 'OPFN) 
(PUT 'MAKE_IDENTITY 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'SQUAREP 'NUMBER-OF-ARGS 1) 
(PUT 'SQUAREP 'DEFINED-ON-LINE '555) 
(PUT 'SQUAREP 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SQUAREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQUAREP (IN_MAT)
    (PROG (TMP)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in squarep: non matrix input")))
      (SETQ TMP (SIZE_OF_MATRIX IN_MAT))
      (COND ((NEQ (FIRST TMP) (SECOND TMP)) (RETURN NIL)) (T (RETURN T))))) 
(FLAG '(SQUAREP) 'BOOLEAN) 
(FLAG '(SQUAREP) 'OPFN) 
(PUT 'SWAP_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'SWAP_ROWS 'DEFINED-ON-LINE '574) 
(PUT 'SWAP_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SWAP_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAP_ROWS (IN_MAT ROW1 ROW2)
    (PROG (NEW_MAT ROWDIM)
      (SETQ ROWDIM 0)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR "Error in swap_rows(first argument): should be a matrix.")))
         (SETQ ROWDIM (ROW_DIM IN_MAT))
         (COND
          ((NOT (FIXP ROW1))
           (REDERR
            "Error in swap_rows(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP ROW2))
           (REDERR
            "Error in swap_rows(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP ROW1 ROWDIM) (EQUAL ROW1 0))
           (REDERR
            "Error in swap_rows(second argument): out of range for input matrix.")))
         (COND
          ((OR (GREATERP ROW2 ROWDIM) (EQUAL ROW2 0))
           (REDERR
            "Error in swap_rows(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (SWAP_ELT (CDR NEW_MAT) ROW1 ROW2)
      (RETURN NEW_MAT))) 
(PUT 'SWAP_COLUMNS 'NUMBER-OF-ARGS 3) 
(PUT 'SWAP_COLUMNS 'DEFINED-ON-LINE '602) 
(PUT 'SWAP_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SWAP_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAP_COLUMNS (IN_MAT COL1 COL2)
    (PROG (NEW_MAT COLDIM)
      (SETQ COLDIM 0)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in swap_columns(first argument): should be a matrix.")))
         (SETQ COLDIM (COLUMN_DIM IN_MAT))
         (COND
          ((NOT (FIXP COL1))
           (REDERR
            "Error in swap_columns(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP COL2))
           (REDERR
            "Error in swap_columns(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP COL1 COLDIM) (EQUAL COL1 0))
           (REDERR
            "Error in swap_columns(second argument): out of range for matrix.")))
         (COND
          ((OR (GREATERP COL2 COLDIM) (EQUAL COL2 0))
           (REDERR
            "Error in swap_columns(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (PROG (ROW)
        (SETQ ROW (CDR NEW_MAT))
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW) (SWAP_ELT ROW COL1 COL2)) (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'SWAP_ENTRIES 'NUMBER-OF-ARGS 3) 
(PUT 'SWAP_ENTRIES 'DEFINED-ON-LINE '630) 
(PUT 'SWAP_ENTRIES 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'SWAP_ENTRIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAP_ENTRIES (IN_MAT ENTRY1 ENTRY2)
    (PROG (NEW_MAT ROWDIM COLDIM)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in swap_entries(first argument): should be a matrix.")))
      (COND
       ((OR (ATOM ENTRY1) (NEQ (CAR ENTRY1) 'LIST)
            (NEQ (LENGTH (CDR ENTRY1)) 2))
        (REDERR
         "Error in swap_entries(second argument): should be list of 2 elements."))
       (T (SETQ ENTRY1 (CDR ENTRY1))))
      (COND
       ((OR (ATOM ENTRY2) (NEQ (CAR ENTRY2) 'LIST)
            (NEQ (LENGTH (CDR ENTRY2)) 2))
        (REDERR
         "Error in swap_entries(third argument): should be a list of 2 elements."))
       (T (SETQ ENTRY2 (CDR ENTRY2))))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (SETQ ROWDIM (ROW_DIM IN_MAT))
         (SETQ COLDIM (COLUMN_DIM IN_MAT))
         (COND
          ((NOT (FIXP (CAR ENTRY1)))
           (PROGN
            (PRIN2 "***** Error in swap_entries(second argument): ")
            (PRIN2T "      first element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CADR ENTRY1)))
           (PROGN
            (PRIN2 "***** Error in swap_entries(second argument): ")
            (PRIN2T "      second element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CAR ENTRY1) ROWDIM) (EQUAL (CAR ENTRY1) 0))
           (PROGN
            (PRIN2 "***** Error in swap_entries(second argument): ")
            (PRIN2T "      first element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CADR ENTRY1) COLDIM) (EQUAL (CADR ENTRY1) 0))
           (PROGN
            (PRIN2 "***** Error in swap_entries(second argument): ")
            (PRIN2T "      second element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CAR ENTRY2)))
           (PROGN
            (PRIN2 "***** Error in swap_entries(third argument): ")
            (PRIN2T "      first element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CADR ENTRY2)))
           (PROGN
            (PRIN2 "***** Error in swap_entries(third argument): ")
            (PRIN2T "      second element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CAR ENTRY2) ROWDIM) (EQUAL (CAR ENTRY2) 0))
           (PROGN
            (PRIN2 "***** Error in swap_entries(third argument): ")
            (PRIN2T "      first element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((GREATERP (CADR ENTRY2) COLDIM)
           (PROGN
            (PRIN2 "***** Error in swap_entries(third argument): ")
            (PRIN2T "      second element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (SETMAT NEW_MAT (CAR ENTRY1) (CADR ENTRY1)
       (GETMAT IN_MAT (CAR ENTRY2) (CADR ENTRY2)))
      (SETMAT NEW_MAT (CAR ENTRY2) (CADR ENTRY2)
       (GETMAT IN_MAT (CAR ENTRY1) (CADR ENTRY1)))
      (RETURN NEW_MAT))) 
(RTYPECAR (LIST 'SWAP_ROWS 'SWAP_COLUMNS 'SWAP_ENTRIES)) 
(PUT 'GET_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'GET_ROWS 'DEFINED-ON-LINE '716) 
(PUT 'GET_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_ROWS (IN_MAT ROW_LIST)
    (PROG (ROWDIM COLDIM ANS TMP)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in get_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in get_rows(second argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (PROG (ELT)
        (SETQ ELT ROW_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in get_rows(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT ROWDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in get_rows(second argument): ")
               (REDERR
                "contains row number which is out of range for input matrix.")
               NIL)))
            (SETQ TMP (CONS 'MAT (LIST (NTH (CDR IN_MAT) ELT))))
            (SETQ ANS (APPEND ANS (LIST TMP)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (CONS 'LIST ANS)))) 
(PUT 'GET_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'GET_COLUMNS 'DEFINED-ON-LINE '757) 
(PUT 'GET_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_COLUMNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_COLUMNS (IN_MAT COL_LIST)
    (PROG (ROWDIM COLDIM ANS TMP)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in get_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in get_columns(second argument): ")
              (PRIN2T
               "     should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (PROG (ELT)
        (SETQ ELT COL_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in get_columns(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT COLDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in get_columns(second argument): ")
               (REDERR
                "contains column number which is out of range for input matrix.")
               NIL)))
            (SETQ TMP
                    (CONS 'MAT
                          (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                            (SETQ ROW (CDR IN_MAT))
                            (COND ((NULL ROW) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (ROW)
                                                (LIST (NTH ROW ELT)))
                                              (CAR ROW))
                                             NIL)))
                           LOOPLABEL
                            (SETQ ROW (CDR ROW))
                            (COND ((NULL ROW) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (ROW) (LIST (NTH ROW ELT)))
                                      (CAR ROW))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
            (SETQ ANS (APPEND ANS (LIST TMP)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (CONS 'LIST ANS)))) 
(FLAG '(GET_ROWS |,| GET_COLUMNS) 'OPFN) 
(PUT 'STACK_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'STACK_ROWS 'DEFINED-ON-LINE '801) 
(PUT 'STACK_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'STACK_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STACK_ROWS (IN_MAT ROW_LIST)
    (PROG ()
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in stack_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST))))
      (RETURN
       (CONS 'MAT
             (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
               (SETQ ELT ROW_LIST)
               (COND ((NULL ELT) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ELT) (NTH (CDR IN_MAT) ELT))
                                 (CAR ELT))
                                NIL)))
              LOOPLABEL
               (SETQ ELT (CDR ELT))
               (COND ((NULL ELT) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (ELT) (NTH (CDR IN_MAT) ELT)) (CAR ELT))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'AUGMENT_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'AUGMENT_COLUMNS 'DEFINED-ON-LINE '817) 
(PUT 'AUGMENT_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'AUGMENT_COLUMNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AUGMENT_COLUMNS (IN_MAT COL_LIST)
    (PROG ()
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR
         "Error in augment_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST))))
      (RETURN
       (CONS 'MAT
             (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
               (SETQ ROW (CDR IN_MAT))
               (COND ((NULL ROW) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ROW)
                                   (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ ELT COL_LIST)
                                     (COND ((NULL ELT) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (ELT)
                                                         (NTH ROW ELT))
                                                       (CAR ELT))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ ELT (CDR ELT))
                                     (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (ELT) (NTH ROW ELT))
                                               (CAR ELT))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR ROW))
                                NIL)))
              LOOPLABEL
               (SETQ ROW (CDR ROW))
               (COND ((NULL ROW) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (ROW)
                           (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                             (SETQ ELT COL_LIST)
                             (COND ((NULL ELT) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (ELT) (NTH ROW ELT))
                                               (CAR ELT))
                                              NIL)))
                            LOOPLABEL
                             (SETQ ELT (CDR ELT))
                             (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (ELT) (NTH ROW ELT)) (CAR ELT))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR ROW))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(RTYPECAR (LIST 'STACK_ROWS 'AUGMENT_COLUMNS)) 
(PUT 'ADD_ROWS 'NUMBER-OF-ARGS 4) 
(PUT 'ADD_ROWS 'DEFINED-ON-LINE '837) 
(PUT 'ADD_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ADD_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_ROWS (IN_MAT R1 R2 MULT1)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR "Error in add_rows(first argument): should be a matrix.")))
         (SETQ ROWDIM (ROW_DIM IN_MAT))
         (COND
          ((NOT (FIXP R1))
           (REDERR
            "Error in add_rows(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP R2))
           (REDERR
            "Error in add_rows(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP R1 ROWDIM) (EQUAL R1 0))
           (REDERR
            "Error in add_rows(second argument): out of range for input matrix.")))
         (COND
          ((OR (GREATERP R2 ROWDIM) (EQUAL R2 0))
           (REDERR
            "Error in add_rows(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (COND
       ((EQUAL (COND ((FIXP MULT1) MULT1) (T (REVAL1 MULT1 T))) 0)
        (RETURN NEW_MAT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE COLDIM I)) (RETURN NIL)))
        (SETMAT NEW_MAT R2 I
         (REVAL1
          (LIST 'PLUS (LIST 'TIMES MULT1 (GETMAT NEW_MAT R1 I))
                (GETMAT IN_MAT R2 I))
          T))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'ADD_COLUMNS 'NUMBER-OF-ARGS 4) 
(PUT 'ADD_COLUMNS 'DEFINED-ON-LINE '870) 
(PUT 'ADD_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ADD_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_COLUMNS (IN_MAT C1 C2 MULT1)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in add_columns(first argument): should be a matrix.")))
         (SETQ COLDIM (COLUMN_DIM IN_MAT))
         (COND
          ((NOT (FIXP C1))
           (REDERR
            "Error in add_columns(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP C2))
           (REDERR
            "Error in add_columns(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP C1 COLDIM) (EQUAL C1 0))
           (REDERR
            "Error in add_columns(second argument): out of range for input matrix.")))
         (COND
          ((OR (GREATERP C2 ROWDIM) (EQUAL C2 0))
           (REDERR
            "Error in add_columns(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (COND
       ((EQUAL (COND ((FIXP MULT1) MULT1) (T (REVAL1 MULT1 T))) 0)
        (RETURN NEW_MAT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (SETMAT NEW_MAT I C2
         (LIST 'PLUS (LIST 'TIMES MULT1 (GETMAT NEW_MAT I C1))
               (GETMAT IN_MAT I C2)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(RTYPECAR (LIST 'ADD_ROWS 'ADD_COLUMNS)) 
(PUT 'ADD_TO_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_TO_ROWS 'DEFINED-ON-LINE '906) 
(PUT 'ADD_TO_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ADD_TO_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_TO_ROWS (IN_MAT ROW_LIST VALUE)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in add_to_row(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in add_to_rows(second argument): ")
              (PRIN2T "      should be either integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((NOT (FIXP ROW))
              (REDERR
               "Error in add_to_row(second argument): should be an integer.")))
            (COND
             ((OR (GREATERP ROW ROWDIM) (EQUAL ROW 0))
              (PROGN
               (PRIN2 "***** Error in add_to_rows(second argument): ")
               (REDERR "contains row which is out of range for input matrix.")
               NIL)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE COLDIM I)) (RETURN NIL)))
              (SETMAT NEW_MAT ROW I (LIST 'PLUS (GETMAT NEW_MAT ROW I) VALUE))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'ADD_TO_COLUMNS 'NUMBER-OF-ARGS 3) 
(PUT 'ADD_TO_COLUMNS 'DEFINED-ON-LINE '945) 
(PUT 'ADD_TO_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ADD_TO_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD_TO_COLUMNS (IN_MAT COL_LIST VALUE)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR
         "Error in add_to_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in add_to_columns(second argument): ")
              (PRIN2T "      should be either integer or list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (PROG (COL)
        (SETQ COL COL_LIST)
       LAB
        (COND ((NULL COL) (RETURN NIL)))
        ((LAMBDA (COL)
           (PROGN
            (COND
             ((NOT (FIXP COL))
              (REDERR
               "Error in add_to_columns(second argument): should be an integer.")))
            (COND
             ((OR (GREATERP COL COLDIM) (EQUAL COL 0))
              (PROGN
               (PRIN2 "***** Error in add_to_columns(second argument): ")
               (REDERR
                "contains column which is out of range for input matrix.")
               NIL)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
              (SETMAT NEW_MAT I COL (LIST 'PLUS (GETMAT NEW_MAT I COL) VALUE))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL))
         (CAR COL))
        (SETQ COL (CDR COL))
        (GO LAB))
      (RETURN NEW_MAT))) 
(RTYPECAR (LIST 'ADD_TO_ROWS 'ADD_TO_COLUMNS)) 
(PUT 'MULT_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'MULT_ROWS 'DEFINED-ON-LINE '988) 
(PUT 'MULT_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MULT_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULT_ROWS (IN_MAT ROW_LIST MULT1)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in mult_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST))))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((AND (NOT *FAST_LA) (NOT (FIXP ROW)))
              (REDERR
               "Error in mult_rows(second argument): contains non integer.")))
            (COND
             ((AND (NOT *FAST_LA) (OR (GREATERP ROW ROWDIM) (EQUAL ROW 0)))
              (PROGN
               (PRIN2 "***** Error in mult_rows(second argument): ")
               (REDERR "contains row that is out of range for input matrix.")
               NIL)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE COLDIM I)) (RETURN NIL)))
              (PROGN
               (SETMAT NEW_MAT ROW I
                (REVAL1 (LIST 'TIMES MULT1 (GETMAT IN_MAT ROW I)) T))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'MULT_COLUMNS 'NUMBER-OF-ARGS 3) 
(PUT 'MULT_COLUMNS 'DEFINED-ON-LINE '1020) 
(PUT 'MULT_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MULT_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULT_COLUMNS (IN_MAT COLUMN_LIST MULT1)
    (PROG (NEW_MAT I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in mult_columns(first argument): should be a matrix.")))
      (COND ((ATOM COLUMN_LIST) (SETQ COLUMN_LIST (LIST COLUMN_LIST)))
            ((EQUAL (CAR COLUMN_LIST) 'LIST)
             (SETQ COLUMN_LIST (CDR COLUMN_LIST))))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (SETQ NEW_MAT (COPY_MAT IN_MAT))
      (PROG (COLUMN)
        (SETQ COLUMN COLUMN_LIST)
       LAB
        (COND ((NULL COLUMN) (RETURN NIL)))
        ((LAMBDA (COLUMN)
           (PROGN
            (COND
             ((AND (NOT *FAST_LA) (NOT (FIXP COLUMN)))
              (REDERR
               "Error in mult_columns(second argument): contains non integer.")))
            (COND
             ((AND (NOT *FAST_LA)
                   (OR (GREATERP COLUMN COLDIM) (EQUAL COLUMN 0)))
              (PROGN
               (PRIN2 "***** Error in mult_columns(second argument): ")
               (REDERR
                "contains column that is out of range for input matrix.")
               NIL)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
              (PROGN
               (SETMAT NEW_MAT I COLUMN
                (REVAL1 (LIST 'TIMES MULT1 (GETMAT IN_MAT I COLUMN)) T))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL))
         (CAR COLUMN))
        (SETQ COLUMN (CDR COLUMN))
        (GO LAB))
      (RETURN NEW_MAT))) 
(RTYPECAR (LIST 'MULT_ROWS 'MULT_COLUMNS)) 
(PUT 'MATRIX_AUGMENT1 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIX_AUGMENT1 'DEFINED-ON-LINE '1059) 
(PUT 'MATRIX_AUGMENT1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MATRIX_AUGMENT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIX_AUGMENT1 (MATRICES)
    (PROG (MAT_LIST NEW_LIST NEW_ROW)
      (COND
       ((AND (PAIRP MATRICES) (PAIRP (CAR MATRICES))
             (EQUAL (CAAR MATRICES) 'LIST))
        (SETQ MATRICES (CDAR MATRICES))))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (SETQ MAT_LIST
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT MATRICES)
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (ELT)
           (SETQ ELT MAT_LIST)
          LAB
           (COND ((NULL ELT) (RETURN NIL)))
           ((LAMBDA (ELT)
              (COND
               ((NOT (MATRIXP ELT))
                (REDERR "Error in matrix_augment: non matrix in input."))))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         NIL)))
      (CONST_ROWS_TEST MAT_LIST)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((MINUSP (DIFFERENCE (ROW_DIM (FIRST MAT_LIST)) I)) (RETURN NIL)))
        (PROGN
         (SETQ NEW_ROW (LIST))
         (PROG (MAT1)
           (SETQ MAT1 MAT_LIST)
          LAB
           (COND ((NULL MAT1) (RETURN NIL)))
           ((LAMBDA (MAT1) (SETQ NEW_ROW (APPEND NEW_ROW (NTH (CDR MAT1) I))))
            (CAR MAT1))
           (SETQ MAT1 (CDR MAT1))
           (GO LAB))
         (SETQ NEW_LIST (APPEND NEW_LIST (LIST NEW_ROW)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS 'MAT NEW_LIST)))) 
(RTYPECAR (LIST 'MATRIX_AUGMENT)) 
(PUT 'MATRIX_AUGMENT 'PSOPFN 'MATRIX_AUGMENT1) 
(PUT 'MATRIX_STACK1 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIX_STACK1 'DEFINED-ON-LINE '1093) 
(PUT 'MATRIX_STACK1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MATRIX_STACK1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIX_STACK1 (MATRICES)
    (PROG (MAT_LIST NEW_LIST)
      (COND
       ((AND (PAIRP MATRICES) (PAIRP (CAR MATRICES))
             (EQUAL (CAAR MATRICES) 'LIST))
        (SETQ MATRICES (CDAR MATRICES))))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (SETQ MAT_LIST
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT MATRICES)
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (ELT) (REVAL1 ELT T)) (CAR ELT))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG (ELT)
           (SETQ ELT MAT_LIST)
          LAB
           (COND ((NULL ELT) (RETURN NIL)))
           ((LAMBDA (ELT)
              (COND
               ((NOT (MATRIXP ELT))
                (REDERR "Error in matrix_stack: non matrix in input."))))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         NIL)))
      (CONST_COLUMNS_TEST MAT_LIST)
      (PROG (MAT1)
        (SETQ MAT1 MAT_LIST)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1) (SETQ NEW_LIST (APPEND NEW_LIST (CDR MAT1))))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (RETURN (CONS 'MAT NEW_LIST)))) 
(RTYPECAR (LIST 'MATRIX_STACK)) 
(PUT 'MATRIX_STACK 'PSOPFN 'MATRIX_STACK1) 
(PUT 'NO_ROWS 'NUMBER-OF-ARGS 1) 
(PUT 'NO_ROWS 'DEFINED-ON-LINE '1121) 
(PUT 'NO_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'NO_ROWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_ROWS (MAT_LIST)
    (PROG (MAT1 FORALL-RESULT)
      (SETQ MAT1 MAT_LIST)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL MAT1) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (MAT1) (ROW_DIM MAT1)) (CAR MAT1)) FORALL-RESULT))
      (SETQ MAT1 (CDR MAT1))
      (GO LAB1))) 
(PUT 'NO_COLS 'NUMBER-OF-ARGS 1) 
(PUT 'NO_COLS 'DEFINED-ON-LINE '1130) 
(PUT 'NO_COLS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'NO_COLS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_COLS (MAT_LIST)
    (PROG (MAT1 FORALL-RESULT)
      (SETQ MAT1 MAT_LIST)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL MAT1) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (MAT1) (COLUMN_DIM MAT1)) (CAR MAT1))
                    FORALL-RESULT))
      (SETQ MAT1 (CDR MAT1))
      (GO LAB1))) 
(PUT 'CONST_ROWS_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'CONST_ROWS_TEST 'DEFINED-ON-LINE '1138) 
(PUT 'CONST_ROWS_TEST 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CONST_ROWS_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONST_ROWS_TEST (MAT_LIST)
    (PROG (I LISTLEN ROWDIM)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ ROWDIM 0)
      (SETQ LISTLEN (LENGTH MAT_LIST))
      (SETQ ROWDIM (ROW_DIM (CAR MAT_LIST)))
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (LESSP I LISTLEN)
                (EQUAL (ROW_DIM (CAR MAT_LIST)) (ROW_DIM (CADR MAT_LIST)))))
          (RETURN NIL)))
        (PROGN (SETQ I (PLUS I 1)) (SETQ MAT_LIST (CDR MAT_LIST)) NIL)
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN ROWDIM))
            (T
             (PROGN
              (PRIN2 "***** Error in matrix_augment: ")
              (REDERR "all input matrices must have the same row dimension.")
              NIL))))) 
(PUT 'CONST_COLUMNS_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'CONST_COLUMNS_TEST 'DEFINED-ON-LINE '1160) 
(PUT 'CONST_COLUMNS_TEST 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CONST_COLUMNS_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONST_COLUMNS_TEST (MAT_LIST)
    (PROG (I LISTLEN COLDIM)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ COLDIM 0)
      (SETQ LISTLEN (LENGTH MAT_LIST))
      (SETQ COLDIM (COLUMN_DIM (CAR MAT_LIST)))
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (LESSP I LISTLEN)
                (EQUAL (COLUMN_DIM (CAR MAT_LIST))
                       (COLUMN_DIM (CADR MAT_LIST)))))
          (RETURN NIL)))
        (PROGN (SETQ I (PLUS I 1)) (SETQ MAT_LIST (CDR MAT_LIST)) NIL)
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN COLDIM))
            (T
             (PROGN
              (PRIN2 "***** Error in matrix_stack: ")
              (REDERR
               "all input matrices must have the same column dimension.")
              (RETURN NIL)
              NIL))))) 
(PUT 'BLOCK_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'BLOCK_MATRIX 'DEFINED-ON-LINE '1189) 
(PUT 'BLOCK_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'BLOCK_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BLOCK_MATRIX (ROWS COLS MAT_LIST)
    (PROG (BLOCK_MAT ROW_LIST ROWDIM COLDIM START_ROW START_COL I J)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ START_ROW 0)
      (SETQ START_COL 0)
      (SETQ I 0)
      (SETQ J 0)
      (COND
       ((NOT (FIXP ROWS))
        (REDERR
         "Error in block_matrix(first argument): should be an integer.")))
      (COND
       ((EQUAL ROWS 0)
        (PROGN
         (PRIN2 "***** Error in block_matrix(first argument): ")
         (PRIN2T "      should be an integer greater than 0.")
         (RETURN NIL)
         NIL)))
      (COND
       ((NOT (FIXP COLS))
        (REDERR
         "Error in block_matrix(second argument): should be an integer.")))
      (COND
       ((EQUAL COLS 0)
        (PROGN
         (PRIN2 "***** Error in block_matrix(second argument): ")
         (PRIN2T "      should be an integer greater than 0.")
         (RETURN NIL)
         NIL)))
      (COND ((MATRIXP MAT_LIST) (SETQ MAT_LIST (LIST MAT_LIST)))
            ((AND (PAIRP MAT_LIST) (EQUAL (CAR MAT_LIST) 'LIST))
             (SETQ MAT_LIST (CDR MAT_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in block_matrix(third argument): ")
              (PRIN2T
               "      should be either a single matrix or a list of matrices.")
              (RETURN NIL)
              NIL)))
      (COND
       ((NEQ (TIMES ROWS COLS) (LENGTH MAT_LIST))
        (REDERR
         "Error in block_matrix(third argument): Incorrect number of matrices.")))
      (SETQ ROW_LIST (CREATE_ROW_LIST ROWS COLS MAT_LIST))
      (SETQ ROWDIM (CHECK_ROWS ROW_LIST))
      (SETQ COLDIM (CHECK_COLS ROW_LIST))
      (SETQ BLOCK_MAT (MKMATRIX ROWDIM COLDIM))
      (SETQ START_ROW 1)
      (SETQ START_COL 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH ROW_LIST) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLS J)) (RETURN NIL)))
           (PROGN
            (SETQ BLOCK_MAT
                    (COPY_INTO (NTH (NTH ROW_LIST I) J) BLOCK_MAT START_ROW
                     START_COL))
            (SETQ START_COL
                    (PLUS START_COL (COLUMN_DIM (NTH (NTH ROW_LIST I) J))))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ START_COL 1)
         (SETQ START_ROW (PLUS START_ROW (ROW_DIM (NTH (NTH ROW_LIST I) 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN BLOCK_MAT))) 
(FLAG '(BLOCK_MATRIX) 'OPFN) 
(PUT 'BLOCK_MATRIX 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'CREATE_ROW_LIST 'NUMBER-OF-ARGS 3) 
(PUT 'CREATE_ROW_LIST 'DEFINED-ON-LINE '1249) 
(PUT 'CREATE_ROW_LIST 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CREATE_ROW_LIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CREATE_ROW_LIST (ROWS COLS MAT_LIST)
    (PROG (ROW_LIST TMP_LIST I J INCREMENT)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ INCREMENT 0)
      (SETQ INCREMENT 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
        (PROGN
         (SETQ TMP_LIST (LIST))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLS J)) (RETURN NIL)))
           (PROGN
            (SETQ TMP_LIST (APPEND TMP_LIST (LIST (NTH MAT_LIST INCREMENT))))
            (SETQ INCREMENT (PLUS INCREMENT 1))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ ROW_LIST (APPEND ROW_LIST (LIST TMP_LIST)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ROW_LIST))) 
(PUT 'CHECK_COLS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_COLS 'DEFINED-ON-LINE '1275) 
(PUT 'CHECK_COLS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CHECK_COLS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_COLS (ROW_LIST)
    (PROG (I LISTLEN)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ I 1)
      (SETQ LISTLEN (LENGTH ROW_LIST))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (LESSP I LISTLEN)
                (EQUAL (NO_COLS (NTH ROW_LIST I))
                       (NO_COLS (NTH ROW_LIST (PLUS I 1))))))
          (RETURN NIL)))
        (SETQ I (PLUS I 1))
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN (NO_COLS (NTH ROW_LIST I))))
            (T
             (PROGN
              (PRIN2
               "***** Error in block_matrix: column dimensions of matrices ")
              (PRIN2T "      into block_matrix are not compatible")
              (RETURN NIL)
              NIL))))) 
(PUT 'CHECK_ROWS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_ROWS 'DEFINED-ON-LINE '1299) 
(PUT 'CHECK_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CHECK_ROWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_ROWS (ROW_LIST)
    (PROG (I LISTLEN ROWDIM ELTLEN J)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ ROWDIM 0)
      (SETQ ELTLEN 0)
      (SETQ J 0)
      (SETQ I 1)
      (SETQ LISTLEN (LENGTH ROW_LIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ I LISTLEN)) (RETURN NIL)))
        (PROGN
         (SETQ ELTLEN (LENGTH (NTH ROW_LIST I)))
         (SETQ J 1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (LESSP J ELTLEN)) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL (ROW_DIM (NTH (NTH ROW_LIST I) J))
                     (ROW_DIM (NTH (NTH ROW_LIST I) (PLUS J 1))))
              (SETQ J (PLUS J 1)))
             (T
              (PROGN
               (PRIN2 "***** Error in block_matrix: row dimensions of ")
               (REDERR "matrices into block_matrix are not compatible")
               NIL)))
            NIL)
           (GO WHILELABEL))
         (SETQ ROWDIM (PLUS ROWDIM (ROW_DIM (NTH (NTH ROW_LIST I) J))))
         (SETQ I (PLUS I 1))
         NIL)
        (GO WHILELABEL))
      (RETURN ROWDIM))) 
(PUT 'VANDERMONDE1 'NUMBER-OF-ARGS 1) 
(PUT 'VANDERMONDE1 'DEFINED-ON-LINE '1333) 
(PUT 'VANDERMONDE1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'VANDERMONDE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VANDERMONDE1 (VARIABLES)
    (PROG (VAND IN_LIST I J SQ_SIZE)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ SQ_SIZE 0)
      (COND
       ((AND (PAIRP VARIABLES) (PAIRP (CAR VARIABLES))
             (EQUAL (CAAR VARIABLES) 'LIST))
        (SETQ VARIABLES (CDAR VARIABLES))))
      (SETQ IN_LIST
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT VARIABLES)
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (COND ((FIXP ELT) ELT) (T (REVAL1 ELT T))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (COND ((FIXP ELT) ELT) (T (REVAL1 ELT T))))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SQ_SIZE (LENGTH IN_LIST))
      (SETQ VAND (MKMATRIX SQ_SIZE SQ_SIZE))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
        (PROGN
         (SETMAT VAND I 1 1)
         (PROG (J)
           (SETQ J 2)
          LAB
           (COND ((MINUSP (DIFFERENCE SQ_SIZE J)) (RETURN NIL)))
           (PROGN
            (SETMAT VAND I J
             (REVAL1
              (LIST 'EXPT (NTH IN_LIST I) (LIST 'PLUS J (LIST 'MINUS 1))) T))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN VAND))) 
(PUT 'VANDERMONDE 'PSOPFN 'VANDERMONDE1) 
(PUT 'VANDERMONDE 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'TOEPLITZ1 'NUMBER-OF-ARGS 1) 
(PUT 'TOEPLITZ1 'DEFINED-ON-LINE '1366) 
(PUT 'TOEPLITZ1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'TOEPLITZ1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOEPLITZ1 (VARIABLES)
    (PROG (TOEP IN_LIST I J SQ_SIZE)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ SQ_SIZE 0)
      (COND
       ((AND (PAIRP VARIABLES) (PAIRP (CAR VARIABLES))
             (EQUAL (CAAR VARIABLES) 'LIST))
        (SETQ VARIABLES (CDAR VARIABLES))))
      (SETQ IN_LIST
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT VARIABLES)
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT)
                                    (COND ((FIXP ELT) ELT) (T (REVAL1 ELT T))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (COND ((FIXP ELT) ELT) (T (REVAL1 ELT T))))
                          (CAR ELT))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SQ_SIZE (LENGTH IN_LIST))
      (SETQ TOEP (MKMATRIX SQ_SIZE SQ_SIZE))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 0)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE I 1) J)) (RETURN NIL)))
           (PROGN
            (SETMAT TOEP I (DIFFERENCE I J) (NTH IN_LIST (PLUS J 1)))
            (SETMAT TOEP (DIFFERENCE I J) I (NTH IN_LIST (PLUS J 1)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN TOEP))) 
(PUT 'TOEPLITZ 'PSOPFN 'TOEPLITZ1) 
(PUT 'TOEPLITZ 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'KRONECKER_PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'KRONECKER_PRODUCT 'DEFINED-ON-LINE '1401) 
(PUT 'KRONECKER_PRODUCT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'KRONECKER_PRODUCT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KRONECKER_PRODUCT (AA BB)
    (PROG (A B M N R C)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ C 0)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP AA))
           (REDERR
            "Error in kronecker_product (first argument): should be a matrix.")))
         (COND
          ((NOT (MATRIXP BB))
           (REDERR
            "Error in kronecker_product (second argument): should be a matrix.")))
         NIL)))
      (SETQ M (ROW_DIM AA))
      (SETQ N (COLUMN_DIM AA))
      (SETQ R (ROW_DIM BB))
      (SETQ C (COLUMN_DIM BB))
      (SETQ A (MKMATRIX (TIMES M R) (TIMES N C)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
          (PROGN
           (SETQ B (GETMAT AA I J))
           (PROG (II)
             (SETQ II 1)
            LAB
             (COND ((MINUSP (DIFFERENCE C II)) (RETURN NIL)))
             (PROG (JJ)
               (SETQ JJ 1)
              LAB
               (COND ((MINUSP (DIFFERENCE R JJ)) (RETURN NIL)))
               (SETMAT A (PLUS (TIMES (DIFFERENCE I 1) R) JJ)
                (PLUS (TIMES (DIFFERENCE J 1) C) II)
                (REVAL1 (LIST 'TIMES B (GETMAT BB JJ II)) T))
               (SETQ JJ (PLUS2 JJ 1))
               (GO LAB))
             (SETQ II (PLUS2 II 1))
             (GO LAB))
           NIL)
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN A))) 
(RTYPECAR (LIST 'KRONECKER_PRODUCT)) 
(PUT 'MINOR 'NUMBER-OF-ARGS 3) 
(PUT 'MINOR 'DEFINED-ON-LINE '1437) 
(PUT 'MINOR 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MINOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MINOR (IN_MAT ROW COL)
    (PROG (MIN)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR "Error in minor(first argument): should be a matrix.")))
         (COND
          ((NOT (FIXP ROW))
           (REDERR "Error in minor(second argument): should be an integer.")))
         (COND
          ((OR (GREATERP ROW (ROW_DIM IN_MAT)) (EQUAL ROW 0))
           (REDERR
            "Error in minor(second argument): out of range for input matrix.")))
         (COND
          ((NOT (FIXP COL))
           (REDERR "Error in minor(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP COL (COLUMN_DIM IN_MAT)) (EQUAL COL 0))
           (REDERR
            "Error in minor(second argument): out of range for input matrix.")))
         NIL)))
      (SETQ MIN (REMOVE_ROWS IN_MAT ROW))
      (SETQ MIN (REMOVE_COLUMNS MIN COL))
      (RETURN MIN))) 
(PUT 'REMOVE_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'REMOVE_ROWS 'DEFINED-ON-LINE '1463) 
(PUT 'REMOVE_ROWS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'REMOVE_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_ROWS (IN_MAT ROW_LIST)
    (PROG (UNIQUE_ROW_LIST NEW_LIST ROWDIM ROW)
      (SETQ ROWDIM 0)
      (SETQ ROW 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in remove_rows(first argument): non matrix input.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in remove_rows(second argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ UNIQUE_ROW_LIST (LIST))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((NOT (INTERSECTION (LIST ROW) UNIQUE_ROW_LIST))
              (SETQ UNIQUE_ROW_LIST (APPEND UNIQUE_ROW_LIST (LIST ROW)))))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (PROG (ROW)
           (SETQ ROW UNIQUE_ROW_LIST)
          LAB
           (COND ((NULL ROW) (RETURN NIL)))
           ((LAMBDA (ROW)
              (COND
               ((NOT (FIXP ROW))
                (REDERR
                 "Error in remove_rows(second argument): contains a non integer."))))
            (CAR ROW))
           (SETQ ROW (CDR ROW))
           (GO LAB))
         (PROG (ROW)
           (SETQ ROW UNIQUE_ROW_LIST)
          LAB
           (COND ((NULL ROW) (RETURN NIL)))
           ((LAMBDA (ROW)
              (COND
               ((OR (GREATERP ROW ROWDIM) (EQUAL ROW 0))
                (REDERR
                 "Error in remove_rows(second argument): out of range for input matrix."))))
            (CAR ROW))
           (SETQ ROW (CDR ROW))
           (GO LAB))
         (COND
          ((EQUAL (LENGTH UNIQUE_ROW_LIST) ROWDIM)
           (PROGN
            (PRIN2 "***** Warning in remove_rows:")
            (PRIN2T "      all the rows have been removed. Returning nil.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM ROW)) (RETURN NIL)))
        (COND
         ((NOT (INTERSECTION (LIST ROW) UNIQUE_ROW_LIST))
          (SETQ NEW_LIST (APPEND NEW_LIST (LIST (NTH (CDR IN_MAT) ROW))))))
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN (CONS 'MAT NEW_LIST)))) 
(PUT 'REMOVE_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'REMOVE_COLUMNS 'DEFINED-ON-LINE '1517) 
(PUT 'REMOVE_COLUMNS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'REMOVE_COLUMNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMOVE_COLUMNS (IN_MAT COL_LIST)
    (PROG (UNIQUE_COL_LIST NEW_LIST ROW_LIST COLDIM ROW COL)
      (SETQ COLDIM 0)
      (SETQ ROW 0)
      (SETQ COL 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in remove_columns(first argument): non matrix input.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in remove_columns(second argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ UNIQUE_COL_LIST (LIST))
      (PROG (COL)
        (SETQ COL COL_LIST)
       LAB
        (COND ((NULL COL) (RETURN NIL)))
        ((LAMBDA (COL)
           (PROGN
            (COND
             ((NOT (INTERSECTION (LIST COL) UNIQUE_COL_LIST))
              (SETQ UNIQUE_COL_LIST (APPEND UNIQUE_COL_LIST (LIST COL)))))
            NIL))
         (CAR COL))
        (SETQ COL (CDR COL))
        (GO LAB))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (PROG (COL)
           (SETQ COL UNIQUE_COL_LIST)
          LAB
           (COND ((NULL COL) (RETURN NIL)))
           ((LAMBDA (COL)
              (COND
               ((NOT (FIXP COL))
                (REDERR
                 "Error in remove_columns(second argument): contains a non integer."))))
            (CAR COL))
           (SETQ COL (CDR COL))
           (GO LAB))
         (PROG (COL)
           (SETQ COL UNIQUE_COL_LIST)
          LAB
           (COND ((NULL COL) (RETURN NIL)))
           ((LAMBDA (COL)
              (COND
               ((OR (GREATERP COL COLDIM) (EQUAL COL 0))
                (REDERR
                 "Error in remove_columns(second argument): out of range for matrix."))))
            (CAR COL))
           (SETQ COL (CDR COL))
           (GO LAB))
         (COND
          ((EQUAL (LENGTH UNIQUE_COL_LIST) COLDIM)
           (PROGN
            (PRIN2 "***** Warning in remove_columns: ")
            (PRIN2T "      all the columns have been removed. Returning nil.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (PROG (ROW)
        (SETQ ROW (CDR IN_MAT))
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (SETQ ROW_LIST (LIST))
            (PROG (COL)
              (SETQ COL 1)
             LAB
              (COND ((MINUSP (DIFFERENCE COLDIM COL)) (RETURN NIL)))
              (PROGN
               (COND
                ((NOT (INTERSECTION (LIST COL) UNIQUE_COL_LIST))
                 (SETQ ROW_LIST (APPEND ROW_LIST (LIST (NTH ROW COL))))))
               NIL)
              (SETQ COL (PLUS2 COL 1))
              (GO LAB))
            (SETQ NEW_LIST (APPEND NEW_LIST (LIST ROW_LIST)))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN (CONS 'MAT NEW_LIST)))) 
(FLAG '(MINOR |,| REMOVE_ROWS |,| REMOVE_COLUMNS) 'OPFN) 
(RTYPECAR (LIST 'MINOR 'REMOVE_COLUMNS 'REMOVE_ROWS)) 
(SWITCH (LIST 'IMAGINARY)) 
(SWITCH (LIST 'NOT_NEGATIVE)) 
(SWITCH (LIST 'ONLY_INTEGER)) 
(SWITCH (LIST 'SYMMETRIC)) 
(SWITCH (LIST 'UPPER_MATRIX)) 
(SWITCH (LIST 'LOWER_MATRIX)) 
(PUT 'RANDOM_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'RANDOM_MINUS 'DEFINED-ON-LINE '1612) 
(PUT 'RANDOM_MINUS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'RANDOM_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANDOM_MINUS (LIMIT)
    (PROG (R)
      (SETQ R (RANDOM LIMIT))
      (COND ((EVENP (RANDOM 1000)) (SETQ R (LIST 'MINUS R))))
      (RETURN R))) 
(PUT 'RANDOM_MAKE_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'RANDOM_MAKE_MINUS 'DEFINED-ON-LINE '1625) 
(PUT 'RANDOM_MAKE_MINUS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'RANDOM_MAKE_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANDOM_MAKE_MINUS (U) (COND ((EVENP (RANDOM 1000)) (LIST 'MINUS U)) (T U))) 
(PUT 'RANDOM_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'RANDOM_MATRIX 'DEFINED-ON-LINE '1633) 
(PUT 'RANDOM_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'RANDOM_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RANDOM_MATRIX (ROWDIM COLDIM LIMIT)
    (PROG (RANDMAT RANDOM_DECIMAL I J START CURRENT_PRECISION)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ START 0)
      (SETQ CURRENT_PRECISION 0)
      (COND
       ((AND *LOWER_MATRIX *UPPER_MATRIX)
        (PROGN
         (PRIN2 "***** Error in random matrix: ")
         (PRIN2T "      both upper_matrix and lower_matrix switches are on.")
         (RETURN NIL)
         NIL)))
      (COND
       ((AND *UPPER_MATRIX *SYMMETRIC)
        (PROGN
         (PRIN2 "***** Error in random_matriix: ")
         (PRIN2T "      both upper_matrix and symmetric switches are on.")
         (RETURN NIL)
         NIL)))
      (COND
       ((AND *LOWER_MATRIX *SYMMETRIC)
        (PROGN
         (PRIN2 "***** Error in random_matrix: ")
         (PRIN2T "      both lower_matrix and symmetric switches are on.")
         (RETURN NIL)
         NIL)))
      (COND
       ((NOT (FIXP LIMIT))
        (SETQ LIMIT (AEVAL (LIST 'FLOOR (LIST 'ABS LIMIT))))))
      (COND
       ((NOT (FIXP ROWDIM))
        (REDERR
         "Error in random_matrix(first argument): should be an integer.")))
      (COND
       ((EQUAL ROWDIM 0)
        (REDERR
         "Error in random_matrix(first argument): should be integer > than 0.")))
      (COND
       ((NOT (FIXP COLDIM))
        (REDERR
         "Error in random_matrix(second argument): should be an integer.")))
      (COND
       ((EQUAL COLDIM 0)
        (PROGN
         (PRIN2 "***** Error in random_matrix(second argument): ")
         (PRIN2T "      should be an integer greater than 0.")
         (RETURN NIL)
         NIL)))
      (SETQ CURRENT_PRECISION (PRECISION 0))
      (COND (*IMAGINARY (SETQ RANDMAT (IM_RANDOM_MATRIX ROWDIM COLDIM LIMIT)))
            (T
             (PROGN
              (SETQ START 1)
              (SETQ RANDMAT (MKMATRIX ROWDIM COLDIM))
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
                (PROGN
                 (COND ((OR *SYMMETRIC *LOWER_MATRIX) (SETQ COLDIM I))
                       (*UPPER_MATRIX (SETQ START I)))
                 (PROG (J)
                   (SETQ J START)
                  LAB
                   (COND ((MINUSP (DIFFERENCE COLDIM J)) (RETURN NIL)))
                   (PROG (R1 R2)
                     (SETQ R1 (RANDOM LIMIT))
                     (SETQ R2 (RANDOM (EXPT 10 CURRENT_PRECISION)))
                     (SETQ RANDOM_DECIMAL
                             (LIST 'PLUS R1
                                   (LIST 'QUOTIENT R2
                                         (EXPT 10 CURRENT_PRECISION))))
                     (COND
                      ((AND *ONLY_INTEGER *NOT_NEGATIVE)
                       (SETMAT RANDMAT I J (RANDOM LIMIT)))
                      (*ONLY_INTEGER (SETMAT RANDMAT I J (RANDOM_MINUS LIMIT)))
                      (*NOT_NEGATIVE (SETMAT RANDMAT I J RANDOM_DECIMAL))
                      (T
                       (SETMAT RANDMAT I J
                        (RANDOM_MAKE_MINUS RANDOM_DECIMAL))))
                     (COND
                      (*SYMMETRIC (SETMAT RANDMAT J I (GETMAT RANDMAT I J)))))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 NIL)
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              NIL)))
      (RETURN RANDMAT))) 
(FLAG '(RANDOM_MATRIX) 'OPFN) 
(PUT 'RANDOM_MATRIX 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'IM_RANDOM_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'IM_RANDOM_MATRIX 'DEFINED-ON-LINE '1709) 
(PUT 'IM_RANDOM_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'IM_RANDOM_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE IM_RANDOM_MATRIX (ROWDIM COLDIM LIMIT)
    (PROG (RANDMAT RANDOM_DECIMAL IM_RANDOM_DECIMAL I J START
           CURRENT_PRECISION)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ START 0)
      (SETQ CURRENT_PRECISION 0)
      (SETQ START 1)
      (SETQ CURRENT_PRECISION (PRECISION 0))
      (SETQ RANDMAT (MKMATRIX ROWDIM COLDIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (COND ((OR *SYMMETRIC *LOWER_MATRIX) (SETQ COLDIM I))
               (*UPPER_MATRIX (SETQ START I)))
         (PROG (J)
           (SETQ J START)
          LAB
           (COND ((MINUSP (DIFFERENCE COLDIM J)) (RETURN NIL)))
           (PROG (R1 R2)
             (SETQ R1 (RANDOM LIMIT))
             (SETQ R2 (RANDOM (EXPT 10 CURRENT_PRECISION)))
             (SETQ RANDOM_DECIMAL
                     (LIST 'PLUS 1
                           (LIST 'QUOTIENT R2 (EXPT 10 CURRENT_PRECISION))))
             (SETQ R1 (RANDOM LIMIT))
             (SETQ R2 (RANDOM (EXPT 10 CURRENT_PRECISION)))
             (SETQ IM_RANDOM_DECIMAL
                     (LIST 'PLUS R1
                           (LIST 'QUOTIENT R2 (EXPT 10 CURRENT_PRECISION))))
             (COND
              ((AND *ONLY_INTEGER *NOT_NEGATIVE)
               (PROGN
                (SETQ R1 (RANDOM LIMIT))
                (SETQ R2 (RANDOM LIMIT))
                (SETMAT RANDMAT I J (LIST 'PLUS R1 (LIST 'TIMES 'I R2)))))
              (*ONLY_INTEGER
               (PROGN
                (SETQ R1 (RANDOM_MINUS LIMIT))
                (SETQ R2 (RANDOM_MINUS LIMIT))
                (SETMAT RANDMAT I J (LIST 'PLUS R1 (LIST 'TIMES 'I R2)))))
              (*NOT_NEGATIVE
               (SETMAT RANDMAT I J
                (LIST 'PLUS RANDOM_DECIMAL
                      (LIST 'TIMES 'I IM_RANDOM_DECIMAL))))
              (T
               (PROGN
                (SETQ R1 (RANDOM_MAKE_MINUS RANDOM_DECIMAL))
                (SETQ R2 (RANDOM_MAKE_MINUS IM_RANDOM_DECIMAL))
                (SETMAT RANDMAT I J (LIST 'PLUS R1 (LIST 'TIMES 'I R2))))))
             (COND (*SYMMETRIC (SETMAT RANDMAT J I (GETMAT RANDMAT I J)))))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN RANDMAT))) 
(RTYPECAR (LIST 'IM_RANDOM_MATRIX)) 
(PUT 'EXTEND 'NUMBER-OF-ARGS 4) 
(PUT 'EXTEND 'DEFINED-ON-LINE '1765) 
(PUT 'EXTEND 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'EXTEND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTEND (IN_MAT ROWS COLS ENTRY)
    (PROG (EX_MAT ROWDIM COLDIM I J)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ I 0)
      (SETQ J 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in extend(first argument): should be a matrix.")))
      (COND
       ((NOT (FIXP ROWS))
        (REDERR "Error in extend(second argument): should be an integer.")))
      (COND
       ((NOT (FIXP COLS))
        (REDERR "Error in extend(third argument): should be an integer.")))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (SETQ EX_MAT (MKMATRIX (PLUS ROWDIM ROWS) (PLUS COLDIM COLS)))
      (SETQ EX_MAT (COPY_INTO IN_MAT EX_MAT 1 1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (PLUS ROWDIM ROWS) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (PLUS COLDIM COLS) J)) (RETURN NIL)))
           (PROGN
            (COND ((AND (LEQ I ROWDIM) (LEQ J COLDIM)) (PROGN NIL))
                  (T (SETMAT EX_MAT I J ENTRY)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN EX_MAT))) 
(FLAG '(EXTEND) 'OPFN) 
(RTYPECAR (LIST 'EXTEND)) 
(PUT 'CHAR_MATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'CHAR_MATRIX 'DEFINED-ON-LINE '1801) 
(PUT 'CHAR_MATRIX 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CHAR_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHAR_MATRIX (IN_MAT LMBDA)
    (PROG (CARMAT ROWDIM)
      (SETQ ROWDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in char_matrix(first argument): should be a matrix.")))
      (COND
       ((NOT (SQUAREP IN_MAT))
        (REDERR
         "Error in char_matrix(first argument): must be a square matrix.")))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ CARMAT
              (LIST 'PLUS (LIST 'TIMES LMBDA (MAKE_IDENTITY ROWDIM))
                    (LIST 'MINUS IN_MAT)))
      (RETURN CARMAT))) 
(PUT 'CHAR_POLY 'NUMBER-OF-ARGS 2) 
(PUT 'CHAR_POLY 'DEFINED-ON-LINE '1821) 
(PUT 'CHAR_POLY 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CHAR_POLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHAR_POLY (IN_MAT LMBDA)
    (PROG (CHPOLY CARMAT)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in char_poly(first argument): should be a matrix.")))
      (SETQ CARMAT (CHAR_MATRIX IN_MAT LMBDA))
      (SETQ CHPOLY (AEVAL (LIST 'DET CARMAT)))
      (RETURN CHPOLY))) 
(FLAG '(CHAR_MATRIX CHAR_POLY) 'OPFN) 
(RTYPECAR (LIST 'CHAR_MATRIX)) 
(PUT 'PIVOT 'NUMBER-OF-ARGS 3) 
(PUT 'PIVOT 'DEFINED-ON-LINE '1846) 
(PUT 'PIVOT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'PIVOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PIVOT (IN_MAT PIVOT_ROW PIVOT_COL)
    (PROG (PIV_MAT RATIO I J ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in pivot(first argument): should be a matrix.")))
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (FIXP PIVOT_ROW))
           (REDERR "Error in pivot(second argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_ROW ROWDIM) (EQUAL PIVOT_ROW 0))
           (REDERR
            "Error in pivot(second argument): out of range for input matrix.")))
         (COND
          ((NOT (FIXP PIVOT_COL))
           (REDERR "Error in pivot(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_COL COLDIM) (EQUAL PIVOT_COL 0))
           (REDERR
            "Error in pivot(third argument): out of range for input matrix.")))
         (COND
          ((EQUAL (GETMAT IN_MAT PIVOT_ROW PIVOT_COL) 0)
           (REDERR "Error in pivot: cannot pivot on a zero entry.")))
         NIL)))
      (SETQ PIV_MAT (COPY_MAT IN_MAT))
      (SETQ PIV_MAT (COPY_MAT IN_MAT))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLDIM J)) (RETURN NIL)))
           (PROGN
            (COND ((EQUAL I PIVOT_ROW) (PROGN NIL))
                  (T
                   (PROGN
                    (SETQ RATIO
                            (LIST 'QUOTIENT (GETMAT IN_MAT I PIVOT_COL)
                                  (GETMAT IN_MAT PIVOT_ROW PIVOT_COL)))
                    (SETMAT PIV_MAT I J
                     (LIST 'PLUS (GETMAT IN_MAT I J)
                           (LIST 'MINUS
                                 (LIST 'TIMES RATIO
                                       (GETMAT IN_MAT PIVOT_ROW J)))))
                    NIL)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN PIV_MAT))) 
(PUT 'ROWS_PIVOT 'NUMBER-OF-ARGS 4) 
(PUT 'ROWS_PIVOT 'DEFINED-ON-LINE '1892) 
(PUT 'ROWS_PIVOT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'ROWS_PIVOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ROWS_PIVOT (IN_MAT PIVOT_ROW PIVOT_COL ROW_LIST)
    (PROG (PIV_MAT RATIO J ROWDIM COLDIM)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ ROWDIM (ROW_DIM IN_MAT))
      (SETQ COLDIM (COLUMN_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in rows_pivot(first argument): should be a matrix.")))
         (SETQ ROWDIM (ROW_DIM IN_MAT))
         (SETQ COLDIM (COLUMN_DIM IN_MAT))
         (COND
          ((NOT (FIXP PIVOT_ROW))
           (REDERR "Error in pivot(second argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_ROW ROWDIM) (EQUAL PIVOT_ROW 0))
           (REDERR
            "Error in rows_pivot(second argument): out of range for input matrix.")))
         (COND
          ((NOT (FIXP PIVOT_COL))
           (REDERR "Error in pivot(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_COL COLDIM) (EQUAL PIVOT_COL 0))
           (REDERR
            "Error in rows_pivot(third argument): out of range for input matrix.")))
         NIL)))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((AND (PAIRP ROW_LIST) (EQUAL (CAR ROW_LIST) 'LIST))
             (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in rows_pivot(fourth argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (COND
       ((EQUAL (GETMAT IN_MAT PIVOT_ROW PIVOT_COL) 0)
        (REDERR "Error in rows_pivot: cannot pivot on a zero entry.")))
      (SETQ PIV_MAT (COPY_MAT IN_MAT))
      (PROG (ELT)
        (SETQ ELT ROW_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT *FAST_LA)
              (PROGN
               (COND
                ((NOT (FIXP ELT))
                 (REDERR
                  "Error in rows_pivot: fourth argument contains a non integer.")))
               (COND
                ((OR (GREATERP ELT ROWDIM) (EQUAL ELT 0))
                 (PROGN
                  (PRIN2 "***** Error in rows_pivot(fourth argument): ")
                  (REDERR
                   "contains row which is out of range for input matrix.")
                  NIL)))
               NIL)))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE COLDIM J)) (RETURN NIL)))
              (PROGN
               (COND ((EQUAL ELT PIVOT_ROW) (PROGN NIL))
                     (T
                      (PROGN
                       (SETQ RATIO
                               (LIST 'QUOTIENT (GETMAT IN_MAT ELT PIVOT_COL)
                                     (GETMAT IN_MAT PIVOT_ROW PIVOT_COL)))
                       (SETMAT PIV_MAT ELT J
                        (LIST 'PLUS (GETMAT IN_MAT ELT J)
                              (LIST 'MINUS
                                    (LIST 'TIMES RATIO
                                          (GETMAT IN_MAT PIVOT_ROW J)))))
                       NIL)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN PIV_MAT))) 
(FLAG '(PIVOT |,| ROWS_PIVOT) 'OPFN) 
(RTYPECAR (LIST 'PIVOT 'ROWS_PIVOT)) 
(PUT 'MAT_JACOBIAN 'NUMBER-OF-ARGS 2) 
(PUT 'MAT_JACOBIAN 'DEFINED-ON-LINE '1968) 
(PUT 'MAT_JACOBIAN 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MAT_JACOBIAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAT_JACOBIAN (EXP_LIST VAR_LIST)
    (PROG (JAC EXP1 VAR1 I J ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND ((ATOM EXP_LIST) (SETQ EXP_LIST (LIST EXP_LIST)))
            ((NEQ (CAR EXP_LIST) 'LIST)
             (REDERR
              "Error in mat_jacobian(first argument): expressions must be in a list."))
            (T (SETQ EXP_LIST (CDR EXP_LIST))))
      (COND ((ATOM VAR_LIST) (SETQ VAR_LIST (LIST VAR_LIST)))
            ((NEQ (CAR VAR_LIST) 'LIST)
             (REDERR
              "Error in mat_jacobian(second argument): variables must be in a list."))
            (T (SETQ VAR_LIST (CDR VAR_LIST))))
      (SETQ ROWDIM (LENGTH EXP_LIST))
      (SETQ COLDIM (LENGTH VAR_LIST))
      (SETQ JAC (MKMATRIX ROWDIM COLDIM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE COLDIM J)) (RETURN NIL)))
           (PROGN
            (SETQ EXP1 (NTH EXP_LIST I))
            (SETQ VAR1 (NTH VAR_LIST J))
            (SETMAT JAC I J (AEVAL* (LIST 'DF EXP1 VAR1)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN JAC))) 
(FLAG '(MAT_JACOBIAN) 'OPFN) 
(PUT 'MAT_JACOBIAN 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'HESSIAN 'NUMBER-OF-ARGS 2) 
(PUT 'HESSIAN 'DEFINED-ON-LINE '2004) 
(PUT 'HESSIAN 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'HESSIAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HESSIAN (POLY VARIABLES)
    (PROG (HESS_MAT PART1 PART2 ELT ROW COL SQ_SIZE)
      (SETQ ROW 0)
      (SETQ COL 0)
      (SETQ SQ_SIZE 0)
      (COND ((ATOM VARIABLES) (SETQ VARIABLES (LIST VARIABLES)))
            ((EQUAL (CAR VARIABLES) 'LIST) (SETQ VARIABLES (CDR VARIABLES)))
            (T
             (PROGN
              (PRIN2 "***** Error in hessian(second argument): ")
              (PRIN2T
               "      should be either a single variable or a list of variables.")
              (RETURN NIL)
              NIL)))
      (SETQ SQ_SIZE (LENGTH VARIABLES))
      (SETQ HESS_MAT (MKMATRIX SQ_SIZE SQ_SIZE))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE ROW)) (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE SQ_SIZE COL)) (RETURN NIL)))
           (PROGN
            (SETQ PART1 (NTH VARIABLES ROW))
            (SETQ PART2 (NTH VARIABLES COL))
            (SETQ ELT (AEVAL* (LIST 'DF (LIST 'DF POLY PART1) PART2)))
            (SETMAT HESS_MAT ROW COL ELT)
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN HESS_MAT))) 
(FLAG '(HESSIAN) 'OPFN) 
(PUT 'HESSIAN 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'HERMITIAN_TP 'NUMBER-OF-ARGS 1) 
(PUT 'HERMITIAN_TP 'DEFINED-ON-LINE '2045) 
(PUT 'HERMITIAN_TP 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'HERMITIAN_TP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HERMITIAN_TP (IN_MAT)
    (PROG (H_TP ELEMENT ROW COL)
      (SETQ ROW 0)
      (SETQ COL 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in hermitian_tp: non matrix input.")))
      (SETQ H_TP (AEVAL (LIST 'TP IN_MAT)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (ROW_DIM H_TP) ROW)) (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (COLUMN_DIM H_TP) COL)) (RETURN NIL)))
           (PROGN
            (SETQ ELEMENT (GETMAT H_TP ROW COL))
            (SETMAT H_TP ROW COL (MK*SQ (SIMPCONJ (LIST ELEMENT))))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN H_TP))) 
(FLAG '(HERMITIAN_TP) 'OPFN) 
(RTYPECAR (LIST 'HERMITIAN_TP)) 
(PUT 'HILBERT 'NUMBER-OF-ARGS 2) 
(PUT 'HILBERT 'DEFINED-ON-LINE '2074) 
(PUT 'HILBERT 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'HILBERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HILBERT (SQ_SIZE VALUE)
    (PROG (HIL_MAT DENOM ROW COL)
      (SETQ ROW 0)
      (SETQ COL 0)
      (COND
       ((OR (NOT (FIXP SQ_SIZE)) (LESSP SQ_SIZE 1))
        (REDERR
         "Error in hilbert(first argument): must be a positive integer.")))
      (SETQ HIL_MAT (MKMATRIX SQ_SIZE SQ_SIZE))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE ROW)) (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE SQ_SIZE COL)) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL
               (SETQ DENOM (REVAL1 (LIST 'PLUS ROW COL (LIST 'MINUS VALUE)) T))
               0)
              (REDERR "Error in hilbert: division by zero."))
             (T (SETMAT HIL_MAT ROW COL (LIST 'QUOTIENT 1 DENOM))))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN HIL_MAT))) 
(FLAG '(HILBERT) 'OPFN) 
(PUT 'HILBERT 'RTYPEFN 'QUOTEMATRIX) 
(PUT 'COEFF_MATRIX 'PSOPFN 'COEFF_MATRIX1) 
(PUT 'COEFF_MATRIX1 'NUMBER-OF-ARGS 1) 
(PUT 'COEFF_MATRIX1 'DEFINED-ON-LINE '2105) 
(PUT 'COEFF_MATRIX1 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'COEFF_MATRIX1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COEFF_MATRIX1 (EQUATION_LIST)
    (PROG (VARIABLE_LIST A X B)
      (COND
       ((AND (PAIRP (CAR EQUATION_LIST)) (EQUAL (CAAR EQUATION_LIST) 'LIST))
        (SETQ EQUATION_LIST (CDAR EQUATION_LIST))))
      (SETQ EQUATION_LIST (REMOVE_EQUALS EQUATION_LIST))
      (SETQ VARIABLE_LIST (GET_VARIABLE_LIST EQUATION_LIST))
      (COND
       ((EQUAL VARIABLE_LIST NIL)
        (REDERR "Error in coeff_matrix: no variables in input.")))
      (CHECK_LINEARITY EQUATION_LIST VARIABLE_LIST)
      (SETQ A (GET_A EQUATION_LIST VARIABLE_LIST))
      (SETQ X (GET_X VARIABLE_LIST))
      (SETQ B (GET_B EQUATION_LIST VARIABLE_LIST))
      (RETURN (LIST 'LIST A X B)))) 
(PUT 'REMOVE_EQUALS 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE_EQUALS 'DEFINED-ON-LINE '2130) 
(PUT 'REMOVE_EQUALS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'REMOVE_EQUALS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMOVE_EQUALS (EQUATION_LIST)
    (PROG ()
      (SETQ EQUATION_LIST
              (PROG (EQUATION FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQUATION EQUATION_LIST)
                (COND ((NULL EQUATION) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQUATION)
                                    (COND
                                     ((AND (PAIRP EQUATION)
                                           (EQUAL (CAR EQUATION) 'EQUAL))
                                      (REVAL1
                                       (LIST 'PLUS (CADR EQUATION)
                                             (LIST 'MINUS (CADDR EQUATION)))
                                       T))
                                     (T EQUATION)))
                                  (CAR EQUATION))
                                 NIL)))
               LOOPLABEL
                (SETQ EQUATION (CDR EQUATION))
                (COND ((NULL EQUATION) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQUATION)
                            (COND
                             ((AND (PAIRP EQUATION)
                                   (EQUAL (CAR EQUATION) 'EQUAL))
                              (REVAL1
                               (LIST 'PLUS (CADR EQUATION)
                                     (LIST 'MINUS (CADDR EQUATION)))
                               T))
                             (T EQUATION)))
                          (CAR EQUATION))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN EQUATION_LIST))) 
(PUT 'GET_VARIABLE_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'GET_VARIABLE_LIST 'DEFINED-ON-LINE '2145) 
(PUT 'GET_VARIABLE_LIST 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_VARIABLE_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_VARIABLE_LIST (EQUATION_LIST)
    (PROG (VARIABLE_LIST)
      (PROG (EQUATION)
        (SETQ EQUATION EQUATION_LIST)
       LAB
        (COND ((NULL EQUATION) (RETURN NIL)))
        ((LAMBDA (EQUATION)
           (SETQ VARIABLE_LIST (UNION (GET_COEFFS EQUATION) VARIABLE_LIST)))
         (CAR EQUATION))
        (SETQ EQUATION (CDR EQUATION))
        (GO LAB))
      (RETURN (REVERSE VARIABLE_LIST)))) 
(PUT 'CHECK_LINEARITY 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_LINEARITY 'DEFINED-ON-LINE '2158) 
(PUT 'CHECK_LINEARITY 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'CHECK_LINEARITY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_LINEARITY (EQUATION_LIST VARIABLE_LIST)
    (PROG (EQUATION)
      (SETQ EQUATION EQUATION_LIST)
     LAB
      (COND ((NULL EQUATION) (RETURN NIL)))
      ((LAMBDA (EQUATION)
         (PROGN
          (PROG (VARIABLE)
            (SETQ VARIABLE VARIABLE_LIST)
           LAB
            (COND ((NULL VARIABLE) (RETURN NIL)))
            ((LAMBDA (VARIABLE)
               (PROGN
                (COND
                 ((GREATERP (DEG EQUATION VARIABLE) 1)
                  (REDERR
                   "Error in coeff_matrix: the equations are not linear.")))
                NIL))
             (CAR VARIABLE))
            (SETQ VARIABLE (CDR VARIABLE))
            (GO LAB))
          NIL))
       (CAR EQUATION))
      (SETQ EQUATION (CDR EQUATION))
      (GO LAB))) 
(PUT 'GET_A 'NUMBER-OF-ARGS 2) 
(PUT 'GET_A 'DEFINED-ON-LINE '2173) 
(PUT 'GET_A 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_A (EQUATION_LIST VARIABLE_LIST)
    (PROG (A ELEMENT VAR_ELT ROW COL LENGTH_EQUATION_LIST LENGTH_VARIABLE_LIST)
      (SETQ ROW 0)
      (SETQ COL 0)
      (SETQ LENGTH_EQUATION_LIST 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ LENGTH_EQUATION_LIST (LENGTH EQUATION_LIST))
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ A (MKMATRIX (LENGTH EQUATION_LIST) (LENGTH VARIABLE_LIST)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LENGTH_EQUATION_LIST ROW)) (RETURN NIL)))
        (PROGN
         (PROG (COL)
           (SETQ COL 1)
          LAB
           (COND ((MINUSP (DIFFERENCE LENGTH_VARIABLE_LIST COL)) (RETURN NIL)))
           (PROGN
            (SETQ ELEMENT (NTH EQUATION_LIST ROW))
            (SETQ VAR_ELT (NTH VARIABLE_LIST COL))
            (SETMAT A ROW COL (AEVAL* (LIST 'COEFFN ELEMENT VAR_ELT 1)))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN A))) 
(PUT 'GET_B 'NUMBER-OF-ARGS 2) 
(PUT 'GET_B 'DEFINED-ON-LINE '2194) 
(PUT 'GET_B 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_B (EQUATION_LIST VARIABLE_LIST)
    (PROG (SUBSTITUTION_LIST INTEGER_LIST B LENGTH_INTEGER_LIST ROW)
      (SETQ LENGTH_INTEGER_LIST 0)
      (SETQ ROW 0)
      (SETQ SUBSTITUTION_LIST
              (CONS 'LIST
                    (PROG (VARIABLE FORALL-RESULT FORALL-ENDPTR)
                      (SETQ VARIABLE VARIABLE_LIST)
                      (COND ((NULL VARIABLE) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (VARIABLE)
                                          (LIST 'EQUAL VARIABLE 0))
                                        (CAR VARIABLE))
                                       NIL)))
                     LOOPLABEL
                      (SETQ VARIABLE (CDR VARIABLE))
                      (COND ((NULL VARIABLE) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (VARIABLE) (LIST 'EQUAL VARIABLE 0))
                                (CAR VARIABLE))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ INTEGER_LIST
              (PROG (EQUATION FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQUATION EQUATION_LIST)
                (COND ((NULL EQUATION) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQUATION)
                                    (AEVAL
                                     (LIST 'SUB SUBSTITUTION_LIST EQUATION)))
                                  (CAR EQUATION))
                                 NIL)))
               LOOPLABEL
                (SETQ EQUATION (CDR EQUATION))
                (COND ((NULL EQUATION) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQUATION)
                            (AEVAL (LIST 'SUB SUBSTITUTION_LIST EQUATION)))
                          (CAR EQUATION))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LENGTH_INTEGER_LIST (LENGTH INTEGER_LIST))
      (SETQ B (MKMATRIX LENGTH_INTEGER_LIST 1))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LENGTH_INTEGER_LIST ROW)) (RETURN NIL)))
        (SETMAT B ROW 1 (MINUS (NTH INTEGER_LIST ROW)))
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN B))) 
(PUT 'GET_X 'NUMBER-OF-ARGS 1) 
(PUT 'GET_X 'DEFINED-ON-LINE '2215) 
(PUT 'GET_X 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_X 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_X (VARIABLE_LIST)
    (PROG (X ROW LENGTH_VARIABLE_LIST)
      (SETQ ROW 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ X (MKMATRIX LENGTH_VARIABLE_LIST 1))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VARIABLE_LIST) ROW)) (RETURN NIL)))
        (SETMAT X ROW 1 (NTH VARIABLE_LIST ROW))
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN X))) 
(PUT 'GET_COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'GET_COEFFS 'DEFINED-ON-LINE '2228) 
(PUT 'GET_COEFFS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'GET_COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_COEFFS (POLY)
    (PROG (KER_LIST_NUM KER_LIST_DEN)
      (SETQ KER_LIST_NUM (KERNELS (*Q2F (SIMP (REVAL1 (NUM POLY) T)))))
      (SETQ KER_LIST_DEN (KERNELS (*Q2F (SIMP (REVAL1 (DEN POLY) T)))))
      (SETQ KER_LIST_NUM (UNION KER_LIST_NUM KER_LIST_DEN))
      (RETURN KER_LIST_NUM))) 
(DE MY_REVLIS (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T)))) (CAR J))
                    NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MY_REVLIS 'NUMBER-OF-ARGS 1) 
(PUT 'MY_REVLIS 'DEFINED-ON-LINE '2244) 
(PUT 'MY_REVLIS 'DEFINED-IN-FILE 'LINALG/LINALG.RED) 
(PUT 'MY_REVLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MY_REVLIS 'INLINE
      '(LAMBDA (U)
         (PROG (J FORALL-RESULT FORALL-ENDPTR)
           (SETQ J U)
           (COND ((NULL J) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T))))
                             (CAR J))
                            NIL)))
          LOOPLABEL
           (SETQ J (CDR J))
           (COND ((NULL J) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (J) (COND ((FIXP J) J) (T (REVAL1 J T)))) (CAR J))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))) 
(ENDMODULE) 