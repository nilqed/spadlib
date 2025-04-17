(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPLINALG)) 
(SWITCH (LIST 'FAST_LA)) 
(PUT 'SPMAKE_IDENTITY 'NUMBER-OF-ARGS 1) 
(PUT 'SPMAKE_IDENTITY 'DEFINED-ON-LINE '59) 
(PUT 'SPMAKE_IDENTITY 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMAKE_IDENTITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMAKE_IDENTITY (SIZE)
    (PROG (TM)
      (COND
       ((AND (NOT *FAST_LA) (NOT (FIXP SIZE)))
        (REDERR "Error in spmake_identity: non integer input.")))
      (SETQ TM (LIST 'SPARSEMAT (MKVECT SIZE) (LIST 'SPM SIZE SIZE)))
      (PROG (I)
        (SETQ I SIZE)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN (LETMTR3 (LIST TM I I) 1 TM NIL))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN TM))) 
(FLAG '(SPMAKE_IDENTITY) 'OPFN) 
(PUT 'SPROW_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'SPROW_DIM 'DEFINED-ON-LINE '74) 
(PUT 'SPROW_DIM 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPROW_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPROW_DIM (U)
    (PROG (RES)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP U)))
        (REDERR "Error in sprow_dim: input should be a matrix.")))
      (SETQ RES (CADR (SPMATLENGTH U)))
      (RETURN RES))) 
(PUT 'SPCOL_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'SPCOL_DIM 'DEFINED-ON-LINE '84) 
(PUT 'SPCOL_DIM 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCOL_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCOL_DIM (U)
    (PROG (RES)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP U)))
        (REDERR "Error in spcol_dim: input should be a matrix.")))
      (SETQ RES (CADDR (SPMATLENGTH U)))
      (RETURN RES))) 
(FLAG '(SPROW_DIM |,| SPCOL_DIM) 'OPFN) 
(PUT 'MATRIXP 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIXP 'DEFINED-ON-LINE '97) 
(PUT 'MATRIXP 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'MATRIXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIXP (U)
    (PROG ()
      (COND ((NOT (PAIRP U)) (SETQ U (REVAL1 U T))))
      (COND
       ((AND (NOT (EQCAR U 'MAT)) (NOT (EQCAR U 'SPARSEMAT))) (RETURN NIL))
       (T (RETURN T))))) 
(FLAG '(MATRIXP) 'BOOLEAN) 
(FLAG '(MATRIXP) 'OPFN) 
(PUT 'SPARSEMATP 'NUMBER-OF-ARGS 1) 
(PUT 'SPARSEMATP 'DEFINED-ON-LINE '109) 
(PUT 'SPARSEMATP 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPARSEMATP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPARSEMATP (U) (COND ((NOT (EQCAR U 'SPARSEMAT)) NIL) (T T))) 
(FLAG '(SPARSEMATP) 'BOOLEAN) 
(FLAG '(SPARSEMATP) 'OPFN) 
(PUT 'SQUAREP 'NUMBER-OF-ARGS 1) 
(PUT 'SQUAREP 'DEFINED-ON-LINE '118) 
(PUT 'SQUAREP 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SQUAREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQUAREP (U)
    (PROG (TMP)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP U)))
        (REDERR "Error in squarep: non matrix input")))
      (COND ((SPARSEMATP U) (SETQ TMP (CDR (SPMATLENGTH U))))
            (T (SETQ TMP (SIZE_OF_MATRIX U))))
      (COND ((NEQ (CAR TMP) (CADR TMP)) (RETURN NIL)) (T (RETURN T))))) 
(FLAG '(SQUAREP) 'BOOLEAN) 
(FLAG '(SQUAREP) 'OPFN) 
(PUT 'SYMMETRICP 'NUMBER-OF-ARGS 1) 
(PUT 'SYMMETRICP 'DEFINED-ON-LINE '135) 
(PUT 'SYMMETRICP 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SYMMETRICP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYMMETRICP (U)
    (COND ((EQCAR U 'SPARSEMAT) (COND ((NEQ (SMTP U NIL) U) NIL) (T T)))
          ((NEQ (AEVAL (LIST 'TP U)) U) NIL) (T T))) 
(FLAG '(SYMMETRICP) 'BOOLEAN) 
(FLAG '(SYMMETRICP) 'OPFN) 
(PUT 'SPJORDAN_BLOCK 'NUMBER-OF-ARGS 2) 
(PUT 'SPJORDAN_BLOCK 'DEFINED-ON-LINE '147) 
(PUT 'SPJORDAN_BLOCK 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPJORDAN_BLOCK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPJORDAN_BLOCK (CONST MAT_DIM)
    (PROG (TM)
      (SETQ TM (MKEMPSPMAT MAT_DIM (LIST 'SPM MAT_DIM MAT_DIM)))
      (COND
       ((NOT (FIXP MAT_DIM))
        (REDERR
         "Error in spjordan_block(second argument): should be an integer.")))
      (PROG (I)
        (SETQ I MAT_DIM)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (LETMTR3 (LIST TM I I) CONST TM NIL)
         (COND ((LESSP I MAT_DIM) (LETMTR3 (LIST TM I (PLUS I 1)) 1 TM NIL)))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN TM))) 
(FLAG '(SPJORDAN_BLOCK) 'OPFN) 
(PUT 'SPMINOR 'NUMBER-OF-ARGS 3) 
(PUT 'SPMINOR 'DEFINED-ON-LINE '163) 
(PUT 'SPMINOR 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMINOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPMINOR (LIST ROW COL)
    (PROG (LEN LENA LENB RLIST)
      (SETQ LEN (CADDR LIST))
      (SETQ RLIST (SP-COPY-VECT LIST NIL))
      (SETQ LENA (CADR LEN))
      (SETQ LENB (CADDR LEN))
      (COND
       ((NOT (MATRIXP LIST))
        (REDERR "Error in spminor(first argument): should be a matrix.")))
      (COND
       ((NOT (FIXP ROW))
        (REDERR "Error in spminor(second argument): should be an integer.")))
      (COND
       ((NOT (FIXP COL))
        (REDERR "Error in spminor(third argument): should be an integer.")))
      (COND
       ((NOT (AND (GREATERP ROW 0) (LESSP ROW (PLUS LENA 1))))
        (RERROR 'MATRIX 20 "Row number out of range")))
      (COND
       ((NOT (AND (GREATERP COL 0) (LESSP COL (PLUS LENB 1))))
        (RERROR 'MATRIX 21 "Column number out of range")))
      (SPREMROW ROW RLIST)
      (SPREMCOL COL RLIST)
      (SETQ RLIST (REWRITE RLIST (DIFFERENCE LENA 1) ROW COL))
      (RETURN RLIST))) 
(FLAG '(SPMINOR) 'OPFN) 
(PUT 'SPBAND_MATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'SPBAND_MATRIX 'DEFINED-ON-LINE '193) 
(PUT 'SPBAND_MATRIX 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPBAND_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPBAND_MATRIX (ELT_LIST SQ_SIZE)
    (PROG (TM I J IT NO_ELTS MIDDLE_POS)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ IT 0)
      (SETQ NO_ELTS 0)
      (SETQ MIDDLE_POS 0)
      (SETQ TM (MKEMPSPMAT SQ_SIZE (LIST 'SPM SQ_SIZE SQ_SIZE)))
      (COND
       ((NOT (FIXP SQ_SIZE))
        (REDERR
         "Error in spband_matrix(second argument): should be an integer.")))
      (COND ((ATOM ELT_LIST) (SETQ ELT_LIST (LIST ELT_LIST)))
            ((EQUAL (CAR ELT_LIST) 'LIST) (SETQ ELT_LIST (CDR ELT_LIST)))
            (T
             (REDERR
              "Error in spband_matrix(first argument): should be single value or list.")))
      (SETQ NO_ELTS (LENGTH ELT_LIST))
      (COND
       ((EVENP NO_ELTS)
        (REDERR
         "Error in spband matrix(first argument): number of elements must be odd.")))
      (SETQ MIDDLE_POS (REVAL1 (LIST 'QUOTIENT (PLUS NO_ELTS 1) 2) T))
      (COND
       ((GREATERP
         (COND ((FIXP MIDDLE_POS) MIDDLE_POS) (T (REVAL1 MIDDLE_POS T)))
         SQ_SIZE)
        (REDERR
         "Error in spband_matrix: too many elements. Band matrix is overflowing.")))
      (SETQ IT 2)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SQ_SIZE I)) (RETURN NIL)))
        (PROGN
         (COND ((LEQ I MIDDLE_POS) (SETQ J 1)) (T (SETQ J IT)))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (GREATERP (PLUS (DIFFERENCE MIDDLE_POS I) J) 0)
                   (LEQ J SQ_SIZE)
                   (LEQ (PLUS (DIFFERENCE MIDDLE_POS I) J) NO_ELTS)))
             (RETURN NIL)))
           (PROGN
            (LETMTR3 (LIST TM I J)
             (NTH ELT_LIST (PLUS (DIFFERENCE MIDDLE_POS I) J)) TM NIL)
            (SETQ J (PLUS J 1))
            NIL)
           (GO WHILELABEL))
         (COND ((GREATERP I MIDDLE_POS) (SETQ IT (PLUS IT 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN TM))) 
(FLAG '(SPBAND_MATRIX) 'OPFN) 
(PUT 'SPSTACK_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'SPSTACK_ROWS 'DEFINED-ON-LINE '230) 
(PUT 'SPSTACK_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPSTACK_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPSTACK_ROWS (IN_MAT ROW_LIST)
    (PROG (TL RLIST RES LIST ROWDIM CNT COLDIM)
      (SETQ ROWDIM 0)
      (SETQ CNT 0)
      (SETQ COLDIM 0)
      (SETQ LIST IN_MAT)
      (SETQ CNT 1)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in spstack_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spstack_rows(second argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ TL (LIST 'SMP (LENGTH ROW_LIST) COLDIM))
      (SETQ RES (MKEMPSPMAT (LENGTH ROW_LIST) TL))
      (PROG (ELT)
        (SETQ ELT ROW_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in spstack_rows(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT ROWDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in spstack_rows(second argument): ")
               (REDERR
                "contains row number which is out of range for input matrix.")
               NIL)))
            (SETQ RLIST (FINDROW LIST ELT))
            (COND ((EQUAL RLIST NIL) (SETQ CNT (PLUS CNT 1)))
                  (T
                   (PROGN
                    (LETMTR3 (LIST RES CNT) RLIST RES NIL)
                    (SETQ CNT (PLUS CNT 1)))))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN RES))) 
(PUT 'SPAUGMENT_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'SPAUGMENT_COLUMNS 'DEFINED-ON-LINE '272) 
(PUT 'SPAUGMENT_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPAUGMENT_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPAUGMENT_COLUMNS (IN_MAT COL_LIST)
    (PROG (CNT COLDIM RCNT ROWDIM TL RLIST RES LIST RRLIST COLIST VAL RES1)
      (SETQ CNT 0)
      (SETQ COLDIM 0)
      (SETQ RCNT 0)
      (SETQ ROWDIM 0)
      (SETQ LIST IN_MAT)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR
         "Error in spaugment_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spaugment_columns(second argument): ")
              (PRIN2T
               "     should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ CNT 1)
      (SETQ RCNT 1)
      (SETQ TL (LIST 'SPM ROWDIM (LENGTH COL_LIST)))
      (SETQ RES (MKEMPSPMAT ROWDIM TL))
      (PROG (ELT)
        (SETQ ELT COL_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in spaugment_columns(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT COLDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in spaugment_columns(second argument): ")
               (REDERR
                "contains column number which is out of range for input matrix.")
               NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (SETQ RRLIST (FINDROW LIST I))
         (COND
          (RRLIST
           (PROGN
            (PROG (ELT)
              (SETQ ELT COL_LIST)
             LAB
              (COND ((NULL ELT) (RETURN NIL)))
              ((LAMBDA (ELT)
                 (PROGN
                  (SETQ COLIST (ATSOC ELT RRLIST))
                  (COND ((EQUAL COLIST NIL) (SETQ CNT (PLUS CNT 1)))
                        (T
                         (PROGN
                          (SETQ VAL (CDR COLIST))
                          (SETQ RES1 (CONS CNT VAL))
                          (SETQ RLIST (CONS RES1 RLIST))
                          (SETQ CNT (PLUS CNT 1))
                          NIL)))
                  NIL))
               (CAR ELT))
              (SETQ ELT (CDR ELT))
              (GO LAB))
            (COND
             (RLIST
              (LETMTR3 (LIST RES I) (CONS (LIST NIL) (REVERSE RLIST)) RES
               NIL)))
            (SETQ RLIST NIL)
            (SETQ CNT 1)
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN RES))) 
(FLAG '(SPSTACK_ROWS |,| SPAUGMENT_COLUMNS) 'OPFN) 
(PUT 'SPGET_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'SPGET_ROWS 'DEFINED-ON-LINE '333) 
(PUT 'SPGET_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPGET_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPGET_ROWS (IN_MAT ROW_LIST)
    (PROG (TL HE RLIST RES LIST RLIST1 ROWDIM CNT COLDIM)
      (SETQ ROWDIM 0)
      (SETQ CNT 0)
      (SETQ COLDIM 0)
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ TL (LIST 'SPM (LENGTH ROW_LIST) COLDIM))
      (SETQ LIST IN_MAT)
      (SETQ CNT 1)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in spget_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spget_rows(second argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (PROG (ELT)
        (SETQ ELT ROW_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in spget_rows(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT ROWDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in spget_rows(second argument): ")
               (REDERR
                "contains row number which is out of range for input matrix.")
               NIL)))
            (SETQ RLIST (FINDROW LIST ELT))
            (COND ((EQUAL RLIST NIL) NIL)
                  (T
                   (PROGN
                    (SETQ RLIST1 (MKEMPSPMAT 1 (LIST 'SPM 1 COLDIM)))
                    (LETMTR3 (LIST RLIST1 1) RLIST RLIST1 NIL)
                    (SETQ RES (APPEND RES (LIST RLIST1)))
                    NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (CONS 'LIST RES)))) 
(PUT 'SPGET_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'SPGET_COLUMNS 'DEFINED-ON-LINE '379) 
(PUT 'SPGET_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPGET_COLUMNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPGET_COLUMNS (IN_MAT COL_LIST)
    (PROG (COLDIM RCNT ROWDIM TL RLIST RES LIST NLIST RRLIST COLIST VAL RES1)
      (SETQ COLDIM 0)
      (SETQ RCNT 0)
      (SETQ ROWDIM 0)
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ TL (LIST 'SPM ROWDIM (LENGTH COL_LIST)))
      (SETQ LIST IN_MAT)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR
         "Error in spget_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spget_columns(second argument): ")
              (PRIN2T
               "     should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ RCNT 1)
      (PROG (ELT)
        (SETQ ELT COL_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (COND
             ((NOT (FIXP ELT))
              (REDERR
               "Error in spget_columns(second argument): contains non integer.")))
            (COND
             ((OR (GREATERP ELT COLDIM) (EQUAL ELT 0))
              (PROGN
               (PRIN2 "***** Error in get_columns(second argument): ")
               (REDERR
                "contains column number which is out of range for input matrix.")
               NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (PROG (ELT)
        (SETQ ELT COL_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SETQ RLIST (MKEMPSPMAT COLDIM (LIST 'SPM COLDIM 1)))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
              (PROGN
               (SETQ RRLIST (FINDROW LIST I))
               (COND
                (RRLIST
                 (PROGN
                  (SETQ COLIST (ATSOC ELT RRLIST))
                  (COND ((EQUAL COLIST NIL) (SETQ RCNT (PLUS RCNT 1)))
                        (T
                         (PROGN
                          (SETQ VAL (CDR COLIST))
                          (SETQ RES1 (LIST (CONS 1 VAL)))
                          (LETMTR3 (LIST RLIST RCNT) (CONS (LIST NIL) RES1)
                           RLIST NIL)
                          (SETQ RCNT (PLUS RCNT 1))
                          NIL)))
                  NIL)))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ RES (APPEND RES (LIST RLIST)))
            (SETQ RLIST NIL)
            (SETQ RCNT 1)
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN (CONS 'LIST RES)))) 
(FLAG '(SPGET_ROWS |,| SPGET_COLUMNS) 'OPFN) 
(PUT 'SPREMOVE_ROWS 'NUMBER-OF-ARGS 2) 
(PUT 'SPREMOVE_ROWS 'DEFINED-ON-LINE '436) 
(PUT 'SPREMOVE_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPREMOVE_ROWS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPREMOVE_ROWS (IN_MAT ROW_LIST)
    (PROG (UNIQUE_ROW_LIST LIST TL ROWDIM ROW CNT)
      (SETQ ROWDIM 0)
      (SETQ ROW 0)
      (SETQ CNT 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in spremove_rows(first argument): non matrix input.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spremove_rows(second argument): ")
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
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
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
                 "Error in spremove_rows(second argument): contains a non integer."))))
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
                 "Error in spremove_rows(second argument): out of range for input matrix."))))
            (CAR ROW))
           (SETQ ROW (CDR ROW))
           (GO LAB))
         (COND
          ((EQUAL (LENGTH UNIQUE_ROW_LIST) ROWDIM)
           (PROGN
            (PRIN2 "***** Warning in spremove_rows:")
            (PRIN2T "      all the rows have been removed. Returning nil.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ CNT 0)
      (SETQ TL
              (LIST 'SPM (DIFFERENCE ROWDIM (LENGTH UNIQUE_ROW_LIST))
                    (SPCOL_DIM IN_MAT)))
      (SETQ LIST (SP-COPY-VECT IN_MAT TL))
      (PROG (ELT)
        (SETQ ELT UNIQUE_ROW_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SPREMROW (DIFFERENCE ELT CNT) LIST)
            (SETQ LIST (REWRITE LIST ROWDIM (DIFFERENCE ELT CNT) 0))
            (SETQ CNT (PLUS CNT 1))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN LIST))) 
(PUT 'SPREMOVE_COLUMNS 'NUMBER-OF-ARGS 2) 
(PUT 'SPREMOVE_COLUMNS 'DEFINED-ON-LINE '490) 
(PUT 'SPREMOVE_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPREMOVE_COLUMNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPREMOVE_COLUMNS (IN_MAT COL_LIST)
    (PROG (UNIQUE_COL_LIST TL LIST COLDIM COL CNT)
      (SETQ COLDIM 0)
      (SETQ COL 0)
      (SETQ CNT 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR
         "Error in spremove_columns(first argument): non matrix input.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spremove_columns(second argument): ")
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
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
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
                 "Error in spremove_columns(second argument): contains a non integer."))))
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
                 "Error in spremove_columns(second argument): out of range for matrix."))))
            (CAR COL))
           (SETQ COL (CDR COL))
           (GO LAB))
         (COND
          ((EQUAL (LENGTH UNIQUE_COL_LIST) COLDIM)
           (PROGN
            (PRIN2 "***** Warning in spremove_columns: ")
            (PRIN2T "      all the columns have been removed. Returning nil.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ CNT 0)
      (SETQ TL
              (LIST 'SPM (SPROW_DIM IN_MAT)
                    (DIFFERENCE COLDIM (LENGTH UNIQUE_COL_LIST))))
      (SETQ LIST (SP-COPY-VECT IN_MAT TL))
      (PROG (ELT)
        (SETQ ELT UNIQUE_COL_LIST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (PROGN
            (SPREMCOL (DIFFERENCE ELT CNT) LIST)
            (SETQ LIST (REWRITE LIST COLDIM 0 (DIFFERENCE ELT CNT)))
            (SETQ CNT (PLUS CNT 1))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN LIST))) 
(FLAG '(SPREMOVE_ROWS |,| SPREMOVE_COLUMNS) 'OPFN) 
(PUT 'SPBLOCK_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'SPBLOCK_MATRIX 'DEFINED-ON-LINE '548) 
(PUT 'SPBLOCK_MATRIX 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPBLOCK_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPBLOCK_MATRIX (ROWS COLS MAT_LIST)
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
         (PRIN2 "***** Error in spblock_matrix(first argument): ")
         (PRIN2T "      should be an integer greater than 0.")
         (RETURN NIL)
         NIL)))
      (COND
       ((NOT (FIXP COLS))
        (REDERR
         "Error in spblock_matrix(second argument): should be an integer.")))
      (COND
       ((EQUAL COLS 0)
        (PROGN
         (PRIN2 "***** Error in spblock_matrix(second argument): ")
         (PRIN2T "      should be an integer greater than 0.")
         (RETURN NIL)
         NIL)))
      (COND ((MATRIXP MAT_LIST) (SETQ MAT_LIST (LIST MAT_LIST)))
            ((AND (PAIRP MAT_LIST) (EQUAL (CAR MAT_LIST) 'LIST))
             (SETQ MAT_LIST (CDR MAT_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spblock_matrix(third argument): ")
              (PRIN2T
               "      should be either a single matrix or a list of matrices.")
              (RETURN NIL)
              NIL)))
      (COND
       ((NEQ (TIMES ROWS COLS) (LENGTH MAT_LIST))
        (REDERR
         "Error in spblock_matrix(third argument): Incorrect number of matrices.")))
      (SETQ ROW_LIST (SPCREATE_ROW_LIST ROWS COLS MAT_LIST))
      (SETQ ROWDIM (SPCHECK_ROWS ROW_LIST))
      (SETQ COLDIM (SPCHECK_COLS ROW_LIST))
      (SETQ BLOCK_MAT (MKEMPSPMAT ROWDIM (LIST 'SPM ROWDIM COLDIM)))
      (SETQ START_ROW 1)
      (SETQ START_COL 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH ROW_LIST) I)) (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J COLS)
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
           (PROGN
            (SETQ BLOCK_MAT
                    (SPCOPY_INTO (NTH (NTH ROW_LIST I) J) BLOCK_MAT START_ROW
                     START_COL))
            (SETQ START_COL
                    (PLUS START_COL (SPCOL_DIM (NTH (NTH ROW_LIST I) J))))
            NIL)
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         (SETQ START_COL 1)
         (SETQ START_ROW (PLUS START_ROW (SPROW_DIM (NTH (NTH ROW_LIST I) 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN BLOCK_MAT))) 
(FLAG '(SPBLOCK_MATRIX) 'OPFN) 
(PUT 'SPCREATE_ROW_LIST 'NUMBER-OF-ARGS 3) 
(PUT 'SPCREATE_ROW_LIST 'DEFINED-ON-LINE '601) 
(PUT 'SPCREATE_ROW_LIST 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCREATE_ROW_LIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPCREATE_ROW_LIST (ROWS COLS MAT_LIST)
    (PROG (ROW_LIST TMP_LIST LIST I J INCREMENT)
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
            (SETQ LIST (NTH MAT_LIST INCREMENT))
            (COND ((NOT (SPARSEMATP LIST)) (SETQ LIST (SPTRANSMAT LIST))))
            (SETQ TMP_LIST (APPEND TMP_LIST (LIST LIST)))
            (SETQ INCREMENT (PLUS INCREMENT 1))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ ROW_LIST (APPEND ROW_LIST (LIST TMP_LIST)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ROW_LIST))) 
(PUT 'SPCHECK_ROWS 'NUMBER-OF-ARGS 1) 
(PUT 'SPCHECK_ROWS 'DEFINED-ON-LINE '627) 
(PUT 'SPCHECK_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCHECK_ROWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCHECK_ROWS (ROW_LIST)
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
             ((EQUAL (SPROW_DIM (NTH (NTH ROW_LIST I) J))
                     (SPROW_DIM (NTH (NTH ROW_LIST I) (PLUS J 1))))
              (SETQ J (PLUS J 1)))
             (T
              (PROGN
               (PRIN2 "***** Error in spblock_matrix: row dimensions of ")
               (REDERR "matrices into spblock_matrix are not compatible")
               NIL)))
            NIL)
           (GO WHILELABEL))
         (SETQ ROWDIM (PLUS ROWDIM (SPROW_DIM (NTH (NTH ROW_LIST I) J))))
         (SETQ I (PLUS I 1))
         NIL)
        (GO WHILELABEL))
      (RETURN ROWDIM))) 
(PUT 'SPCHECK_COLS 'NUMBER-OF-ARGS 1) 
(PUT 'SPCHECK_COLS 'DEFINED-ON-LINE '658) 
(PUT 'SPCHECK_COLS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCHECK_COLS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCHECK_COLS (ROW_LIST)
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
                (EQUAL (SPNO_COLS (NTH ROW_LIST I))
                       (SPNO_COLS (NTH ROW_LIST (PLUS I 1))))))
          (RETURN NIL)))
        (SETQ I (PLUS I 1))
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN (SPNO_COLS (NTH ROW_LIST I))))
            (T
             (PROGN
              (PRIN2
               "***** Error in spblock_matrix: column dimensions of matrices ")
              (PRIN2T "      into spblock_matrix are not compatible")
              (RETURN NIL)
              NIL))))) 
(PUT 'SPNO_ROWS 'NUMBER-OF-ARGS 1) 
(PUT 'SPNO_ROWS 'DEFINED-ON-LINE '680) 
(PUT 'SPNO_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPNO_ROWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPNO_ROWS (MAT_LIST)
    (PROG (MAT1 FORALL-RESULT)
      (SETQ MAT1 MAT_LIST)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL MAT1) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (MAT1) (SPROW_DIM MAT1)) (CAR MAT1))
                    FORALL-RESULT))
      (SETQ MAT1 (CDR MAT1))
      (GO LAB1))) 
(PUT 'SPNO_COLS 'NUMBER-OF-ARGS 1) 
(PUT 'SPNO_COLS 'DEFINED-ON-LINE '689) 
(PUT 'SPNO_COLS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPNO_COLS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPNO_COLS (MAT_LIST)
    (PROG (MAT1 FORALL-RESULT)
      (SETQ MAT1 MAT_LIST)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL MAT1) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (MAT1) (SPCOL_DIM MAT1)) (CAR MAT1))
                    FORALL-RESULT))
      (SETQ MAT1 (CDR MAT1))
      (GO LAB1))) 
(PUT 'SPCOPY_INTO 'NUMBER-OF-ARGS 4) 
(PUT 'SPCOPY_INTO 'DEFINED-ON-LINE '697) 
(PUT 'SPCOPY_INTO 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCOPY_INTO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPCOPY_INTO (BB AA P Q)
    (PROG (A B M N R C VAL J COL)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ C 0)
      (SETQ VAL 0)
      (SETQ J 0)
      (SETQ COL 0)
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP BB))
           (REDERR
            "Error in spcopy_into(first argument): should be a matrix.")))
         (COND
          ((NOT (MATRIXP AA))
           (REDERR
            "Error in spcopy_into(second argument): should be a matrix.")))
         (COND
          ((NOT (FIXP P))
           (REDERR
            "Error in spcopy_into(third argument): should be an integer.")))
         (COND
          ((NOT (FIXP Q))
           (REDERR
            "Error in spcopy_into(fourth argument): should be an integer.")))
         (COND
          ((OR (EQUAL P 0) (EQUAL Q 0))
           (PROGN
            (PRIN2T
             "***** Error in spcopy_into: 0 is out of bounds for matrices.")
            (PRIN2T
             "      The top left element is labelled (1,1) and not (0,0).")
            (RETURN NIL)
            NIL)))
         NIL)))
      (COND ((NOT (SPARSEMATP BB)) (SETQ BB (SPTRANSMAT BB))))
      (SETQ M (SPROW_DIM AA))
      (SETQ N (SPCOL_DIM AA))
      (SETQ R (SPROW_DIM BB))
      (SETQ C (SPCOL_DIM BB))
      (COND
       ((AND (NOT *FAST_LA)
             (OR (GREATERP (PLUS R (DIFFERENCE P 1)) M)
                 (GREATERP (PLUS C (DIFFERENCE Q 1)) N)))
        (PROGN
         (COND
          ((AND (LESSP (TIMES M N) 26) (LESSP (TIMES R C) 26))
           (PROGN
            (PRIN2T "***** Error in spcopy_into: the matrix")
            (MYSPMATPRI2 BB)
            (PRIN2T "      does not fit into")
            (MYSPMATPRI2 AA)
            (PRIN2 "      at position ")
            (PRIN2 P)
            (PRIN2 ",")
            (PRIN2 Q)
            (PRIN2T ".")
            (RETURN NIL)
            NIL))
          (T
           (PROGN
            (PRIN2 "***** Error in spcopy_into: first matrix does not fit ")
            (PRIN2 "      into second matrix at defined position.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ A (SP-COPY-VECT AA (LIST 'SPM M N)))
      (PROG (I)
        (SETQ I R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW BB I))
         (COND
          (COL
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR COL))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ VAL (CDR XX))
                  (SETQ J (CAR XX))
                  (LETMTR3
                   (LIST A (PLUS P (DIFFERENCE I 1)) (PLUS Q (DIFFERENCE J 1)))
                   VAL A NIL)
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN A))) 
(FLAG '(SPCOPY_INTO) 'OPFN) 
(PUT 'SWAPROW 'NUMBER-OF-ARGS 4) 
(PUT 'SWAPROW 'DEFINED-ON-LINE '765) 
(PUT 'SWAPROW 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SWAPROW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAPROW (ILIST ROW1 ROW2 LEN)
    (PROG (R1 R2 RLIST NLIST ALIST A B CNT AA BB LIST)
      (SETQ LIST ILIST)
      (SETQ R1 (ASSOC ROW1 LIST))
      (SETQ R2 (ASSOC ROW2 LIST))
      (COND
       ((AND (EQUAL R1 NIL) (NOT (EQUAL R2 NIL)))
        (PROGN (SETQ A ROW1) (SETQ AA (CONS ROW1 (CDR R2))) (SETQ B (CAR R2))))
       ((AND (EQUAL R2 NIL) (NOT (EQUAL R1 NIL)))
        (PROGN (SETQ B ROW2) (SETQ BB (CONS ROW2 (CDR R1))) (SETQ A (CAR R1))))
       ((NOT (AND (EQUAL R1 NIL) (EQUAL R2 NIL)))
        (PROGN (SETQ B (CAR R2)) (SETQ A (CAR R1))))
       (T (SETQ CNT (PLUS LEN 1))))
      (SETQ CNT 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL CNT (PLUS LEN 1)))) (RETURN NIL)))
        (PROGN
         (COND ((EQUAL LIST NIL) (SETQ ALIST (LIST LIST)))
               (T (SETQ ALIST (CAR LIST))))
         (COND
          ((EQUAL (CAR ALIST) A)
           (PROGN
            (COND
             ((EQUAL R2 NIL)
              (PROGN
               (COND
                ((EQUAL CNT B)
                 (PROGN (SETQ NLIST (CONS AA NLIST)) (SETQ LIST (CDR LIST))))
                ((EQUAL CNT A)
                 (PROGN (SETQ NLIST NLIST) (SETQ LIST (CDR LIST)))))
               NIL))
             (T
              (PROGN
               (SETQ RLIST (CONS A (CDR R2)))
               (SETQ NLIST (CONS RLIST NLIST))
               (SETQ LIST (CDR LIST)))))
            NIL))
          ((EQUAL (CAR ALIST) B)
           (PROGN
            (COND
             ((EQUAL R1 NIL)
              (PROGN
               (COND
                ((EQUAL CNT A)
                 (PROGN (SETQ NLIST (CONS AA NLIST)) (SETQ LIST (CDR LIST))))
                ((EQUAL CNT B)
                 (PROGN (SETQ NLIST NLIST) (SETQ LIST (CDR LIST)))))
               NIL))
             (T
              (PROGN
               (SETQ RLIST (CONS B (CDR R1)))
               (SETQ NLIST (CONS RLIST NLIST))
               (SETQ LIST (CDR LIST)))))
            NIL))
          ((AND (EQUAL R1 NIL) (NOT (EQUAL CNT (PLUS LEN 1))) (EQUAL CNT A))
           (SETQ NLIST (CONS AA NLIST)))
          ((AND (EQUAL R2 NIL) (NOT (EQUAL CNT (PLUS LEN 1))) (EQUAL CNT B))
           (SETQ NLIST (CONS BB NLIST)))
          (T
           (PROGN
            (COND ((EQUAL ALIST '(NIL)) NIL)
                  (T
                   (PROGN
                    (SETQ NLIST (CONS ALIST NLIST))
                    (SETQ LIST (CDR LIST))))))))
         (SETQ CNT (PLUS CNT 1))
         NIL)
        (GO WHILELABEL))
      (COND ((EQUAL NLIST NIL) (RETURN ILIST)) (T (RETURN (REVERSE NLIST)))))) 
(PUT 'SPSWAP_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'SPSWAP_ROWS 'DEFINED-ON-LINE '822) 
(PUT 'SPSWAP_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPSWAP_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPSWAP_ROWS (IN_MAT ROW1 ROW2)
    (PROG (NEW_MAT LIST PP R1 R2 ROWDIM)
      (SETQ ROWDIM 0)
      (SETQ LIST (SP-COPY-VECT IN_MAT NIL))
      (PROGN
       (COND
        ((NOT (MATRIXP IN_MAT))
         (REDERR "Error in spswap_rows(first argument): should be a matrix.")))
       (SETQ ROWDIM (SPROW_DIM IN_MAT))
       (COND
        ((NOT (FIXP ROW1))
         (REDERR
          "Error in spswap_rows(second argument): should be an integer.")))
       (COND
        ((NOT (FIXP ROW2))
         (REDERR
          "Error in spswap_rows(third argument): should be an integer.")))
       (COND
        ((OR (GREATERP ROW1 ROWDIM) (EQUAL ROW1 0))
         (REDERR
          "Error in spswap_rows(second argument): out of range for input matrix.")))
       (COND
        ((OR (GREATERP ROW2 ROWDIM) (EQUAL ROW2 0))
         (REDERR
          "Error in spswap_rows(third argument): out of range for input matrix.")))
       NIL)
      (COND ((LESSP ROW1 ROW2) NIL)
            (T (PROGN (SETQ PP ROW1) (SETQ ROW1 ROW2) (SETQ ROW2 PP) NIL)))
      (SETQ R1 (FINDROW LIST ROW1))
      (SETQ R2 (FINDROW LIST ROW2))
      (LETMTR3 (LIST LIST ROW1) R2 LIST NIL)
      (LETMTR3 (LIST LIST ROW2) R1 LIST NIL)
      (RETURN LIST))) 
(PUT 'SWAPCOL 'NUMBER-OF-ARGS 4) 
(PUT 'SWAPCOL 'DEFINED-ON-LINE '851) 
(PUT 'SWAPCOL 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SWAPCOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAPCOL (ILIST COL1 COL2 LEN)
    (PROG (C1 C2 RLIST NLIST ALIST A B AA BB CNT ROWN LIST ROW)
      (SETQ CNT 1)
      (PROG (I)
        (SETQ I LEN)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (SETQ ROW (FINDROW ILIST I))
         (COND
          ((NOT (EQUAL ROW NIL))
           (PROGN
            (SETQ C1 (ATSOC COL1 ROW))
            (SETQ C2 (ATSOC COL2 ROW))
            (COND
             ((AND (EQUAL C1 NIL) (NOT (EQUAL C2 NIL)))
              (PROGN
               (SETQ A COL1)
               (SETQ AA (CONS COL1 (CDR C2)))
               (SETQ B (CAR C2))))
             ((AND (EQUAL C2 NIL) (NOT (EQUAL C1 NIL)))
              (PROGN
               (SETQ B COL2)
               (SETQ BB (CONS COL2 (CDR C1)))
               (SETQ A (CAR C1))))
             ((NOT (AND (EQUAL C1 NIL) (EQUAL C2 NIL)))
              (PROGN (SETQ B (CAR C2)) (SETQ A (CAR C1))))
             (T (SETQ CNT (PLUS LEN 1))))
            (SETQ ROWN I)
            (SETQ LIST (CDR ROW))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (EQUAL CNT (PLUS LEN 1)))) (RETURN NIL)))
              (PROGN
               (COND ((EQUAL LIST NIL) (SETQ ALIST (LIST LIST)))
                     (T (SETQ ALIST (CAR LIST))))
               (COND
                ((EQUAL (CAR ALIST) A)
                 (PROGN
                  (COND
                   ((EQUAL C2 NIL)
                    (PROGN
                     (COND
                      ((EQUAL CNT B)
                       (PROGN
                        (SETQ NLIST (CONS BB NLIST))
                        (SETQ LIST (CDR LIST))))
                      ((EQUAL CNT A)
                       (PROGN (SETQ NLIST NLIST) (SETQ LIST (CDR LIST)))))
                     NIL))
                   (T
                    (PROGN
                     (SETQ RLIST (CONS A (CDR C2)))
                     (SETQ NLIST (CONS RLIST NLIST))
                     (SETQ LIST (CDR LIST)))))))
                ((EQUAL (CAR ALIST) B)
                 (PROGN
                  (COND
                   ((EQUAL C1 NIL)
                    (PROGN
                     (COND
                      ((EQUAL CNT A)
                       (PROGN
                        (SETQ NLIST (CONS AA NLIST))
                        (SETQ LIST (CDR LIST))))
                      ((EQUAL CNT B)
                       (PROGN (SETQ NLIST NLIST) (SETQ LIST (CDR LIST)))))
                     NIL))
                   (T
                    (PROGN
                     (SETQ RLIST (CONS B (CDR C1)))
                     (SETQ NLIST (CONS RLIST NLIST))
                     (SETQ LIST (CDR LIST)))))))
                ((AND (EQUAL C1 NIL) (NOT (EQUAL CNT (PLUS LEN 1)))
                      (EQUAL CNT A))
                 (SETQ NLIST (CONS AA NLIST)))
                ((AND (EQUAL C2 NIL) (NOT (EQUAL CNT (PLUS LEN 1)))
                      (EQUAL CNT B))
                 (SETQ NLIST (CONS BB NLIST)))
                (T
                 (PROGN
                  (COND ((EQUAL ALIST '(NIL)) NIL)
                        (T
                         (PROGN
                          (SETQ NLIST (CONS ALIST NLIST))
                          (SETQ LIST (CDR LIST)))))
                  NIL)))
               (SETQ CNT (PLUS CNT 1))
               NIL)
              (GO WHILELABEL))
            (COND
             ((EQUAL NLIST NIL)
              (LETMTR3 (LIST ILIST ROWN) (CONS (LIST NIL) LIST) ILIST NIL))
             (T
              (LETMTR3 (LIST ILIST ROWN) (CONS (LIST NIL) (REVERSE NLIST))
               ILIST NIL)))
            (SETQ NLIST NIL)
            (SETQ CNT 1)
            NIL)))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN ILIST))) 
(PUT 'SPSWAP_COLS 'NUMBER-OF-ARGS 3) 
(PUT 'SPSWAP_COLS 'DEFINED-ON-LINE '919) 
(PUT 'SPSWAP_COLS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPSWAP_COLS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPSWAP_COLS (IN_MAT COL1 COL2)
    (PROG (NEW_MAT LIST PP COLDIM)
      (SETQ COLDIM 0)
      (SETQ LIST (SP-COPY-VECT IN_MAT NIL))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in spswap_columns(first argument): should be a matrix.")))
         (SETQ COLDIM (SPCOL_DIM IN_MAT))
         (COND
          ((NOT (FIXP COL1))
           (REDERR
            "Error in spswap_columns(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP COL2))
           (REDERR
            "Error in spswap_columns(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP COL1 COLDIM) (EQUAL COL1 0))
           (REDERR
            "Error in spswap_columns(second argument): out of range for matrix.")))
         (COND
          ((OR (GREATERP COL2 COLDIM) (EQUAL COL2 0))
           (REDERR
            "Error in spswap_columns(third argument): out of range for input matrix.")))
         NIL)))
      (COND ((LESSP COL1 COL2) NIL)
            (T (PROGN (SETQ PP COL1) (SETQ COL1 COL2) (SETQ COL2 PP) NIL)))
      (SETQ NEW_MAT (SWAPCOL LIST COL1 COL2 (CADDR (CADDR IN_MAT))))
      (RETURN NEW_MAT))) 
(PUT 'SPSWAP_ENTRIES 'NUMBER-OF-ARGS 3) 
(PUT 'SPSWAP_ENTRIES 'DEFINED-ON-LINE '948) 
(PUT 'SPSWAP_ENTRIES 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPSWAP_ENTRIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPSWAP_ENTRIES (IN_MAT ENTRY1 ENTRY2)
    (PROG (NEW_MAT ROWDIM COLDIM VAL1 VAL2)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ VAL1 0)
      (SETQ VAL2 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR
         "Error in spswap_entries(first argument): should be a matrix.")))
      (COND
       ((OR (ATOM ENTRY1) (NEQ (CAR ENTRY1) 'LIST)
            (NEQ (LENGTH (CDR ENTRY1)) 2))
        (REDERR
         "Error in spswap_entries(second argument): should be list of 2 elements."))
       (T (SETQ ENTRY1 (CDR ENTRY1))))
      (COND
       ((OR (ATOM ENTRY2) (NEQ (CAR ENTRY2) 'LIST)
            (NEQ (LENGTH (CDR ENTRY2)) 2))
        (REDERR
         "Error in spswap_entries(third argument): should be a list of 2 elements."))
       (T (SETQ ENTRY2 (CDR ENTRY2))))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (SETQ ROWDIM (SPROW_DIM IN_MAT))
         (SETQ COLDIM (SPCOL_DIM IN_MAT))
         (COND
          ((NOT (FIXP (CAR ENTRY1)))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(second argument): ")
            (PRIN2T "      first element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CADR ENTRY1)))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(second argument): ")
            (PRIN2T "      second element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CAR ENTRY1) ROWDIM) (EQUAL (CAR ENTRY1) 0))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(second argument): ")
            (PRIN2T "      first element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CADR ENTRY1) COLDIM) (EQUAL (CADR ENTRY1) 0))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(second argument): ")
            (PRIN2T "      second element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CAR ENTRY2)))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(third argument): ")
            (PRIN2T "      first element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((NOT (FIXP (CADR ENTRY2)))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(third argument): ")
            (PRIN2T "      second element in list must be an integer.")
            (RETURN NIL)
            NIL)))
         (COND
          ((OR (GREATERP (CAR ENTRY2) ROWDIM) (EQUAL (CAR ENTRY2) 0))
           (PROGN
            (PRIN2 "***** Error in spswap_entries(third argument): ")
            (PRIN2T "      first element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         (COND
          ((GREATERP (CADR ENTRY2) COLDIM)
           (PROGN
            (PRIN2 "***** Error in spswap_entries(third argument): ")
            (PRIN2T "      second element is out of range for input matrix.")
            (RETURN NIL)
            NIL)))
         NIL)))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (SETQ VAL1 (FINDELEM2 NEW_MAT (CAR ENTRY1) (CADR ENTRY1)))
      (SETQ VAL2 (FINDELEM2 NEW_MAT (CAR ENTRY2) (CADR ENTRY2)))
      (LETMTR3 (LIST NEW_MAT (CAR ENTRY1) (CADR ENTRY1)) VAL2 NEW_MAT NIL)
      (LETMTR3 (LIST NEW_MAT (CAR ENTRY2) (CADR ENTRY2)) VAL1 NEW_MAT NIL)
      (RETURN NEW_MAT))) 
(FLAG '(SPSWAP_ROWS |,| SPSWAP_COLS |,| SPSWAP_ENTRIES) 'OPFN) 
(PUT 'REWRITE2 'NUMBER-OF-ARGS 2) 
(PUT 'REWRITE2 'DEFINED-ON-LINE '1037) 
(PUT 'REWRITE2 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'REWRITE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REWRITE2 (LIST NUM)
    (PROG (VAL OLDCOL NEWCOL NLIST)
      (PROG (COL)
        (SETQ COL LIST)
       LAB
        (COND ((NULL COL) (RETURN NIL)))
        ((LAMBDA (COL)
           (PROGN
            (SETQ VAL (CDR COL))
            (SETQ OLDCOL (CAR COL))
            (SETQ OLDCOL (PLUS OLDCOL NUM))
            (SETQ NEWCOL (CONS OLDCOL VAL))
            (SETQ NLIST (CONS NEWCOL NLIST))
            NIL))
         (CAR COL))
        (SETQ COL (CDR COL))
        (GO LAB))
      (RETURN (REVERSE NLIST)))) 
(PUT 'EXPAN2 'NUMBER-OF-ARGS 3) 
(PUT 'EXPAN2 'DEFINED-ON-LINE '1049) 
(PUT 'EXPAN2 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'EXPAN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPAN2 (MLIST ROW LIST)
    (PROG (ROWS COLS ROWN NEWCOLS NEWROWS RLIST CNT SIZE)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROW I)) (RETURN NIL)))
        (PROGN
         (SETQ CNT 0)
         (PROG (MAT1)
           (SETQ MAT1 MLIST)
          LAB
           (COND ((NULL MAT1) (RETURN NIL)))
           ((LAMBDA (MAT1)
              (PROGN
               (SETQ SIZE (SPCOL_DIM MAT1))
               (SETQ ROWS (FINDROW MAT1 I))
               (COND ((EQUAL ROWS NIL) NIL)
                     (T
                      (PROGN
                       (SETQ COLS (CDR ROWS))
                       (SETQ ROWN I)
                       (COND
                        ((EQUAL CNT 0)
                         (PROGN
                          (SETQ NEWCOLS (APPEND COLS NEWCOLS))
                          (SETQ CNT (PLUS CNT SIZE))))
                        (T
                         (PROGN
                          (SETQ COLS (REWRITE2 COLS CNT))
                          (SETQ NEWCOLS (APPEND NEWCOLS COLS))
                          (SETQ CNT (PLUS CNT SIZE))
                          NIL)))
                       NIL)))
               NIL))
            (CAR MAT1))
           (SETQ MAT1 (CDR MAT1))
           (GO LAB))
         (COND
          ((NOT (EQUAL NEWCOLS NIL))
           (PROGN
            (LETMTR3 (LIST LIST I) (CONS (LIST NIL) NEWCOLS) LIST NIL)
            NIL)))
         (SETQ NEWCOLS NIL)
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN LIST))) 
(PUT 'SPMATRIX_AUGMENT 'PSOPFN 'SPMATRIX_AUGMENT1) 
(PUT 'SPMATRIX_AUGMENT1 'NUMBER-OF-ARGS 1) 
(PUT 'SPMATRIX_AUGMENT1 'DEFINED-ON-LINE '1079) 
(PUT 'SPMATRIX_AUGMENT1 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMATRIX_AUGMENT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMATRIX_AUGMENT1 (MATRICES)
    (PROG (MAT_LIST MAT1 NEW_LIST HE TL NUM ROW COL LIST CNT)
      (SETQ CNT 0)
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
                                    ((LAMBDA (ELT)
                                       (PROGN
                                        (COND
                                         ((NOT
                                           (SPARSEMATP
                                            (SETQ LIST (REVAL1 ELT T))))
                                          (SPTRANSMAT LIST))
                                         (T LIST))))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT)
                               (PROGN
                                (COND
                                 ((NOT (SPARSEMATP (SETQ LIST (REVAL1 ELT T))))
                                  (SPTRANSMAT LIST))
                                 (T LIST))))
                             (CAR ELT))
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
                (REDERR "Error in spmatrix_augment: non matrix in input."))))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         NIL)))
      (SPCONST_ROWS_TEST MAT_LIST)
      (SETQ MAT1 (CAR MAT_LIST))
      (SETQ ROW (SPROW_DIM MAT1))
      (PROG (MAT1)
        (SETQ MAT1 MAT_LIST)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1)
           (PROGN (SETQ COL (SPCOL_DIM MAT1)) (SETQ CNT (PLUS CNT COL)) NIL))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (SETQ COL CNT)
      (SETQ LIST (MKEMPSPMAT ROW (LIST 'SPM ROW COL)))
      (SETQ NEW_LIST (EXPAN2 MAT_LIST ROW LIST))
      (RETURN NEW_LIST))) 
(PUT 'SPMATRIX_STACK 'PSOPFN 'SPMATRIX_STACK1) 
(PUT 'SPMATRIX_STACK1 'NUMBER-OF-ARGS 1) 
(PUT 'SPMATRIX_STACK1 'DEFINED-ON-LINE '1114) 
(PUT 'SPMATRIX_STACK1 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMATRIX_STACK1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMATRIX_STACK1 (MATRICES)
    (PROG (MAT_LIST NEW_LIST HE TL NAM ROW COL LIST CNT)
      (SETQ CNT 0)
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
                                    ((LAMBDA (ELT)
                                       (PROGN
                                        (COND
                                         ((NOT
                                           (SPARSEMATP
                                            (SETQ LIST (REVAL1 ELT T))))
                                          (SPTRANSMAT LIST))
                                         (T LIST))))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT)
                               (PROGN
                                (COND
                                 ((NOT (SPARSEMATP (SETQ LIST (REVAL1 ELT T))))
                                  (SPTRANSMAT LIST))
                                 (T LIST))))
                             (CAR ELT))
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
                (REDERR "Error in spmatrix_stack: non matrix in input."))))
            (CAR ELT))
           (SETQ ELT (CDR ELT))
           (GO LAB))
         NIL)))
      (SPCONST_COLUMNS_TEST MAT_LIST)
      (SETQ COL (SPCOL_DIM (CAR MAT_LIST)))
      (PROG (MAT1)
        (SETQ MAT1 MAT_LIST)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1)
           (PROGN (SETQ ROW (SPROW_DIM MAT1)) (SETQ CNT (PLUS CNT ROW)) NIL))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (SETQ ROW CNT)
      (SETQ NEW_LIST (MKEMPSPMAT ROW (LIST 'SPM ROW COL)))
      (SETQ CNT 1)
      (PROG (MAT1)
        (SETQ MAT1 MAT_LIST)
       LAB
        (COND ((NULL MAT1) (RETURN NIL)))
        ((LAMBDA (MAT1)
           (PROGN
            (SETQ ROW (SPROW_DIM MAT1))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE ROW I)) (RETURN NIL)))
              (PROGN
               (SETQ HE (FINDROW MAT1 I))
               (COND (HE (LETMTR3 (LIST NEW_LIST CNT) HE NEW_LIST NIL)))
               (SETQ CNT (PLUS CNT 1))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            NIL))
         (CAR MAT1))
        (SETQ MAT1 (CDR MAT1))
        (GO LAB))
      (RETURN NEW_LIST))) 
(PUT 'SPCONST_ROWS_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'SPCONST_ROWS_TEST 'DEFINED-ON-LINE '1150) 
(PUT 'SPCONST_ROWS_TEST 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCONST_ROWS_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCONST_ROWS_TEST (MAT_LIST)
    (PROG (I LISTLEN ROWDIM)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ ROWDIM 0)
      (SETQ LISTLEN (LENGTH MAT_LIST))
      (SETQ ROWDIM (SPROW_DIM (CAR MAT_LIST)))
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (LESSP I LISTLEN)
                (EQUAL (SPROW_DIM (CAR MAT_LIST))
                       (SPROW_DIM (CADR MAT_LIST)))))
          (RETURN NIL)))
        (PROGN (SETQ I (PLUS I 1)) (SETQ MAT_LIST (CDR MAT_LIST)) NIL)
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN ROWDIM))
            (T
             (PROGN
              (PRIN2 "***** Error in spmatrix_augment: ")
              (REDERR "all input matrices must have the same row dimension.")
              NIL))))) 
(PUT 'SPCONST_COLUMNS_TEST 'NUMBER-OF-ARGS 1) 
(PUT 'SPCONST_COLUMNS_TEST 'DEFINED-ON-LINE '1172) 
(PUT 'SPCONST_COLUMNS_TEST 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCONST_COLUMNS_TEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCONST_COLUMNS_TEST (MAT_LIST)
    (PROG (I LISTLEN COLDIM)
      (SETQ I 0)
      (SETQ LISTLEN 0)
      (SETQ COLDIM 0)
      (SETQ LISTLEN (LENGTH MAT_LIST))
      (SETQ COLDIM (SPCOL_DIM (CAR MAT_LIST)))
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (LESSP I LISTLEN)
                (EQUAL (SPCOL_DIM (CAR MAT_LIST))
                       (SPCOL_DIM (CADR MAT_LIST)))))
          (RETURN NIL)))
        (PROGN (SETQ I (PLUS I 1)) (SETQ MAT_LIST (CDR MAT_LIST)) NIL)
        (GO WHILELABEL))
      (COND ((EQUAL I LISTLEN) (RETURN COLDIM))
            (T
             (PROGN
              (PRIN2 "***** Error in spmatrix_stack: ")
              (REDERR
               "all input matrices must have the same column dimension.")
              (RETURN NIL)
              NIL))))) 
(PUT 'SPEXTEND 'NUMBER-OF-ARGS 4) 
(PUT 'SPEXTEND 'DEFINED-ON-LINE '1198) 
(PUT 'SPEXTEND 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPEXTEND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPEXTEND (IN_MAT ROWS COLS ENTRY)
    (PROG (EX_MAT ROWDIM COLDIM I J)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ I 0)
      (SETQ J 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in spextend(first argument): should be a matrix.")))
      (COND
       ((NOT (FIXP ROWS))
        (REDERR "Error in spextend(second argument): should be an integer.")))
      (COND
       ((NOT (FIXP COLS))
        (REDERR "Error in spextend(third argument): should be an integer.")))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ EX_MAT
              (MKEMPSPMAT (PLUS ROWDIM ROWS)
               (LIST 'SMP (PLUS ROWDIM ROWS) (PLUS COLDIM COLS))))
      (SETQ EX_MAT (SPCOPY_INTO IN_MAT EX_MAT 1 1))
      (PROG (I)
        (SETQ I (PLUS ROWDIM ROWS))
       LAB
        (COND
         ((MINUSP (TIMES (MINUS 1) (DIFFERENCE (PLUS ROWDIM 1) I)))
          (RETURN NIL)))
        (PROGN
         (PROG (J)
           (SETQ J (PLUS COLDIM COLS))
          LAB
           (COND
            ((MINUSP (TIMES (MINUS 1) (DIFFERENCE (PLUS COLDIM 1) J)))
             (RETURN NIL)))
           (PROGN (LETMTR3 (LIST EX_MAT I J) ENTRY EX_MAT NIL) NIL)
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN EX_MAT))) 
(FLAG '(SPEXTEND) 'OPFN) 
(PUT 'SPDIAGONAL 'PSOPFN 'SPDIAGONAL1) 
(PUT 'SPDIAGONAL1 'NUMBER-OF-ARGS 1) 
(PUT 'SPDIAGONAL1 'DEFINED-ON-LINE '1231) 
(PUT 'SPDIAGONAL1 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPDIAGONAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPDIAGONAL1 (MAT_LIST)
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
                                (CONS
                                 ((LAMBDA (ELT)
                                    (PROGN
                                     (COND
                                      ((AND (NOT (SPARSEMATP (REVAL1 ELT NIL)))
                                            (NOT (NUMBERP ELT)))
                                       (SPTRANSMAT ELT))
                                      (T (REVAL1 ELT T)))))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ELT)
                            (PROGN
                             (COND
                              ((AND (NOT (SPARSEMATP (REVAL1 ELT NIL)))
                                    (NOT (NUMBERP ELT)))
                               (SPTRANSMAT ELT))
                              (T (REVAL1 ELT T)))))
                          (CAR ELT))
                         NIL))
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
                ((OR (LESSP (SPROW_DIM ELT) 5) (GREATERP (SPCOL_DIM ELT) 5))
                 (PROGN
                  (PRIN2T "***** Error in spdiagonal: ")
                  (MYSPMATPRI2 ELT)
                  (PRIN2T "      is not a square matrix.")
                  (REDERR "")
                  NIL))
                (T
                 (REDERR
                  "Error in spdiagonal: input contains non square matrix.")))
               NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (SETQ DIAG_MAT (SPDIAG (LIST MAT_LIST)))
      (RETURN DIAG_MAT))) 
(PUT 'SPDIAG 'NUMBER-OF-ARGS 1) 
(PUT 'SPDIAG 'DEFINED-ON-LINE '1261) 
(PUT 'SPDIAG 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPDIAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPDIAG (UU)
    (PROG (BIGA ARG INPUT U VAL A B COL J NARGS AIDX STP BIGSIZE SMALLSIZE)
      (SETQ NARGS 0)
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
          (T (PROGN (SETQ BIGSIZE (PLUS BIGSIZE (SPROW_DIM ARG))) NIL)))
         (SETQ INPUT (CDR INPUT))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ BIGA (MKEMPSPMAT BIGSIZE (LIST 'SPM BIGSIZE BIGSIZE)))
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
            (LETMTR3 (LIST BIGA AIDX AIDX) ARG BIGA NIL)
            (SETQ AIDX (PLUS AIDX 1))
            (SETQ INPUT (CDR INPUT))
            NIL))
          (T
           (PROGN
            (SETQ SMALLSIZE (SPROW_DIM ARG))
            (SETQ STP (PLUS SMALLSIZE (DIFFERENCE AIDX 1)))
            (SETQ A 1)
            (PROG (I)
              (SETQ I AIDX)
             LAB
              (COND ((MINUSP (DIFFERENCE STP I)) (RETURN NIL)))
              (PROGN
               (SETQ COL (FINDROW ARG A))
               (COND
                (COL
                 (PROGN
                  (PROG (XX)
                    (SETQ XX (CDR COL))
                   LAB
                    (COND ((NULL XX) (RETURN NIL)))
                    ((LAMBDA (XX)
                       (PROGN
                        (SETQ VAL (CDR XX))
                        (SETQ J (PLUS (DIFFERENCE AIDX 1) (CAR XX)))
                        (LETMTR3 (LIST BIGA I J) VAL BIGA NIL)
                        NIL))
                     (CAR XX))
                    (SETQ XX (CDR XX))
                    (GO LAB))
                  NIL)))
               (SETQ A (PLUS A 1))
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
(PUT 'SPADD_ROWS 'NUMBER-OF-ARGS 4) 
(PUT 'SPADD_ROWS 'DEFINED-ON-LINE '1326) 
(PUT 'SPADD_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPADD_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPADD_ROWS (IN_MAT R1 R2 MULT1)
    (PROG (NEW_MAT VAL VAL1 VAL2 ROW1 ROW2 I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in spadd_rows(first argument): should be a matrix.")))
         (SETQ ROWDIM (SPROW_DIM IN_MAT))
         (COND
          ((NOT (FIXP R1))
           (REDERR
            "Error in spadd_rows(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP R2))
           (REDERR
            "Error in spadd_rows(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP R1 ROWDIM) (EQUAL R1 0))
           (REDERR
            "Error in spadd_rows(second argument): out of range for input matrix.")))
         (COND
          ((OR (GREATERP R2 ROWDIM) (EQUAL R2 0))
           (REDERR
            "Error in spadd_rows(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (COND
       ((EQUAL (COND ((FIXP MULT1) MULT1) (T (REVAL1 MULT1 T))) 0)
        (RETURN NEW_MAT)))
      (SETQ ROW1 (FINDROW IN_MAT R1))
      (SETQ ROW2 (FINDROW IN_MAT R2))
      (PROG (XX)
        (SETQ XX (CDR ROW1))
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (PROGN
            (SETQ I (CAR XX))
            (SETQ VAL1 (CDR XX))
            (SETQ VAL2 (ATSOC I ROW2))
            (SETQ VAL (REVAL1 (LIST 'TIMES MULT1 VAL1) T))
            (COND
             (VAL2
              (PROGN
               (SETQ VAL (REVAL1 (LIST 'PLUS VAL (CDR VAL2)) T))
               (COND
                ((NOT (EQUAL VAL 0))
                 (LETMTR3 (LIST NEW_MAT R2 I) VAL NEW_MAT NIL)))
               NIL))
             (T (LETMTR3 (LIST NEW_MAT R2 I) VAL NEW_MAT NIL)))
            NIL))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'SPADD_COLUMNS 'NUMBER-OF-ARGS 4) 
(PUT 'SPADD_COLUMNS 'DEFINED-ON-LINE '1366) 
(PUT 'SPADD_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPADD_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPADD_COLUMNS (IN_MAT C1 C2 MULT1)
    (PROG (NEW_MAT VAL I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in spadd_columns(first argument): should be a matrix.")))
         (SETQ COLDIM (SPCOL_DIM IN_MAT))
         (COND
          ((NOT (FIXP C1))
           (REDERR
            "Error in spadd_columns(second argument): should be an integer.")))
         (COND
          ((NOT (FIXP C2))
           (REDERR
            "Error in spadd_columns(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP C1 COLDIM) (EQUAL C1 0))
           (REDERR
            "Error in spadd_columns(second argument): out of range for input matrix.")))
         (COND
          ((OR (GREATERP C2 ROWDIM) (EQUAL C2 0))
           (REDERR
            "Error in spadd_columns(third argument): out of range for input matrix.")))
         NIL)))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (COND
       ((EQUAL (COND ((FIXP MULT1) MULT1) (T (REVAL1 MULT1 T))) 0)
        (RETURN NEW_MAT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (SETQ VAL
                 (REVAL1
                  (LIST 'PLUS (LIST 'TIMES MULT1 (FINDELEM2 NEW_MAT I C1))
                        (FINDELEM2 IN_MAT I C2))
                  T))
         (COND
          ((NOT (EQUAL VAL 0)) (LETMTR3 (LIST NEW_MAT I C2) VAL NEW_MAT NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(FLAG '(SPADD_ROWS |,| SPADD_COLUMNS) 'OPFN) 
(PUT 'SPADD_TO_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'SPADD_TO_ROWS 'DEFINED-ON-LINE '1402) 
(PUT 'SPADD_TO_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPADD_TO_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPADD_TO_ROWS (IN_MAT ROW_LIST VALUE)
    (PROG (NEW_MAT COL VAL I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in spadd_to_row(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spadd_to_rows(second argument): ")
              (PRIN2T "      should be either integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((NOT (FIXP ROW))
              (REDERR
               "Error in spadd_to_row(second argument): should be an integer.")))
            (COND
             ((OR (GREATERP ROW ROWDIM) (EQUAL ROW 0))
              (PROGN
               (PRIN2 "***** Error in spadd_to_rows(second argument): ")
               (REDERR "contains row which is out of range for input matrix.")
               NIL)))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (PROG (I)
              (SETQ I COLDIM)
             LAB
              (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
              (LETMTR3 (LIST NEW_MAT ROW I) VALUE NEW_MAT NIL)
              (SETQ I (PLUS2 I (MINUS 1)))
              (GO LAB))
            (SETQ COL (FINDROW IN_MAT ROW))
            (COND
             (COL
              (PROGN
               (PROG (XX)
                 (SETQ XX (CDR COL))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ I (CAR XX))
                     (SETQ VAL (CDR XX))
                     (LETMTR3 (LIST NEW_MAT ROW I)
                      (REVAL1 (LIST 'PLUS VAL VALUE) T) NEW_MAT NIL)
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               NIL)))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'SPADD_TO_COLUMNS 'NUMBER-OF-ARGS 3) 
(PUT 'SPADD_TO_COLUMNS 'DEFINED-ON-LINE '1444) 
(PUT 'SPADD_TO_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPADD_TO_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPADD_TO_COLUMNS (IN_MAT COL_LIST VALUE)
    (PROG (NEW_MAT COL VAL I ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR
         "Error in spadd_to_columns(first argument): should be a matrix.")))
      (COND ((ATOM COL_LIST) (SETQ COL_LIST (LIST COL_LIST)))
            ((EQUAL (CAR COL_LIST) 'LIST) (SETQ COL_LIST (CDR COL_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in spadd_to_columns(second argument): ")
              (PRIN2T "      should be either integer or list of integers.")
              (RETURN NIL)
              NIL)))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (PROG (COL)
        (SETQ COL COL_LIST)
       LAB
        (COND ((NULL COL) (RETURN NIL)))
        ((LAMBDA (COL)
           (PROGN
            (COND
             ((NOT (FIXP COL))
              (REDERR
               "Error in spadd_to_columns(second argument): should be an integer.")))
            (COND
             ((OR (GREATERP COL COLDIM) (EQUAL COL 0))
              (PROGN
               (PRIN2 "***** Error in spadd_to_columns(second argument): ")
               (REDERR
                "contains column which is out of range for input matrix.")
               NIL)))
            NIL))
         (CAR COL))
        (SETQ COL (CDR COL))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW IN_MAT I))
         (PROG (XX)
           (SETQ XX COL_LIST)
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ VAL (ATSOC XX COL))
               (COND
                (VAL
                 (PROGN
                  (LETMTR3 (LIST NEW_MAT I XX)
                   (REVAL1 (LIST 'PLUS (CDR VAL) VALUE) T) NEW_MAT NIL)
                  NIL))
                (T (LETMTR3 (LIST NEW_MAT I XX) VALUE NEW_MAT NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(FLAG '(SPADD_TO_ROWS |,| SPADD_TO_COLUMNS) 'OPFN) 
(PUT 'SPMULT_ROWS 'NUMBER-OF-ARGS 3) 
(PUT 'SPMULT_ROWS 'DEFINED-ON-LINE '1495) 
(PUT 'SPMULT_ROWS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMULT_ROWS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPMULT_ROWS (IN_MAT ROW_LIST MULT1)
    (PROG (NEW_MAT COL I ROWDIM COLDIM VAL)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ VAL 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in spmult_rows(first argument): should be a matrix.")))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((EQUAL (CAR ROW_LIST) 'LIST) (SETQ ROW_LIST (CDR ROW_LIST))))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (PROG (ROW)
        (SETQ ROW ROW_LIST)
       LAB
        (COND ((NULL ROW) (RETURN NIL)))
        ((LAMBDA (ROW)
           (PROGN
            (COND
             ((AND (NOT *FAST_LA) (NOT (FIXP ROW)))
              (REDERR
               "Error in spmult_rows(second argument): contains non integer.")))
            (COND
             ((AND (NOT *FAST_LA) (OR (GREATERP ROW ROWDIM) (EQUAL ROW 0)))
              (PROGN
               (PRIN2 "***** Error in spmult_rows(second argument): ")
               (REDERR "contains row that is out of range for input matrix.")
               NIL)))
            (SETQ COL (FINDROW IN_MAT ROW))
            (COND
             (COL
              (PROGN
               (PROG (XX)
                 (SETQ XX (CDR COL))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ I (CAR XX))
                     (SETQ VAL (CDR XX))
                     (LETMTR3 (LIST NEW_MAT ROW I)
                      (REVAL1 (LIST 'TIMES MULT1 VAL) T) NEW_MAT NIL)
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               NIL)))
            NIL))
         (CAR ROW))
        (SETQ ROW (CDR ROW))
        (GO LAB))
      (RETURN NEW_MAT))) 
(PUT 'SPMULT_COLUMNS 'NUMBER-OF-ARGS 3) 
(PUT 'SPMULT_COLUMNS 'DEFINED-ON-LINE '1530) 
(PUT 'SPMULT_COLUMNS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPMULT_COLUMNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPMULT_COLUMNS (IN_MAT COLUMN_LIST MULT1)
    (PROG (NEW_MAT COL I ROWDIM COLDIM VAL)
      (SETQ I 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ VAL 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR
         "Error in spmult_columns(first argument): should be a matrix.")))
      (COND ((ATOM COLUMN_LIST) (SETQ COLUMN_LIST (LIST COLUMN_LIST)))
            ((EQUAL (CAR COLUMN_LIST) 'LIST)
             (SETQ COLUMN_LIST (CDR COLUMN_LIST))))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (SETQ NEW_MAT (SP-COPY-VECT IN_MAT NIL))
      (PROG (COLUMN)
        (SETQ COLUMN COLUMN_LIST)
       LAB
        (COND ((NULL COLUMN) (RETURN NIL)))
        ((LAMBDA (COLUMN)
           (PROGN
            (COND
             ((AND (NOT *FAST_LA) (NOT (FIXP COLUMN)))
              (REDERR
               "Error in spmult_columns(second argument): contains non integer.")))
            (COND
             ((AND (NOT *FAST_LA)
                   (OR (GREATERP COLUMN COLDIM) (EQUAL COLUMN 0)))
              (PROGN
               (PRIN2 "***** Error in spmult_columns(second argument): ")
               (REDERR
                "contains column that is out of range for input matrix.")
               NIL)))
            NIL))
         (CAR COLUMN))
        (SETQ COLUMN (CDR COLUMN))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW IN_MAT I))
         (COND
          (COL
           (PROGN
            (PROG (XX)
              (SETQ XX COLUMN_LIST)
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ VAL (ATSOC XX COL))
                  (COND
                   (VAL
                    (LETMTR3 (LIST NEW_MAT I XX)
                     (REVAL1 (LIST 'TIMES MULT1 (CDR VAL)) T) NEW_MAT NIL)))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW_MAT))) 
(FLAG '(SPMULT_ROWS |,| SPMULT_COLUMNS) 'OPFN) 
(PUT 'SPCHAR_MATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'SPCHAR_MATRIX 'DEFINED-ON-LINE '1570) 
(PUT 'SPCHAR_MATRIX 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCHAR_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPCHAR_MATRIX (IN_MAT LMBDA)
    (PROG (CHARMAT ROWDIM)
      (SETQ ROWDIM 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR
         "Error in spchar_matrix(first argument): should be a matrix.")))
      (COND
       ((NOT (SQUAREP IN_MAT))
        (REDERR
         "Error in spchar_matrix(first argument): must be a square matrix.")))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ CHARMAT
              (LIST 'PLUS (LIST 'TIMES LMBDA (SPMAKE_IDENTITY ROWDIM))
                    (LIST 'MINUS IN_MAT)))
      (RETURN CHARMAT))) 
(PUT 'SPCHAR_POLY 'NUMBER-OF-ARGS 2) 
(PUT 'SPCHAR_POLY 'DEFINED-ON-LINE '1587) 
(PUT 'SPCHAR_POLY 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCHAR_POLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPCHAR_POLY (IN_MAT LMBDA)
    (PROG (CHPOLY CARMAT)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in spchar_poly(first argument): should be a matrix.")))
      (SETQ CARMAT (SPCHAR_MATRIX IN_MAT LMBDA))
      (SETQ CHPOLY (AEVAL (LIST 'DET CARMAT)))
      (RETURN CHPOLY))) 
(FLAG '(SPCHAR_MATRIX |,| SPCHAR_POLY) 'OPFN) 
(PUT 'SPHERMITIAN_TP 'NUMBER-OF-ARGS 1) 
(PUT 'SPHERMITIAN_TP 'DEFINED-ON-LINE '1604) 
(PUT 'SPHERMITIAN_TP 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPHERMITIAN_TP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPHERMITIAN_TP (IN_MAT)
    (PROG (H_TP ELEMENT II ROW COL)
      (SETQ II 0)
      (SETQ ROW 0)
      (SETQ COL 0)
      (COND
       ((NOT (MATRIXP IN_MAT))
        (REDERR "Error in sphermitian_tp: non matrix input.")))
      (SETQ H_TP (AEVAL (LIST 'TP IN_MAT)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (SPROW_DIM H_TP) ROW)) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW H_TP ROW))
         (COND
          (COL
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR COL))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ II (CAR XX))
                  (SETQ ELEMENT (CDR XX))
                  (LETMTR3 (LIST H_TP ROW II)
                   (AEVAL*
                    (LIST 'DIFFERENCE (LIST 'REPART ELEMENT)
                          (LIST 'TIMES 'I (LIST 'IMPART ELEMENT))))
                   H_TP NIL)
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN H_TP))) 
(FLAG '(SPHERMITIAN_TP) 'OPFN) 
(PUT 'SPSUB_MATRIX 'NUMBER-OF-ARGS 3) 
(PUT 'SPSUB_MATRIX 'DEFINED-ON-LINE '1631) 
(PUT 'SPSUB_MATRIX 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPSUB_MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPSUB_MATRIX (A ROW_LIST COL_LIST)
    (PROG (NEW_MAT)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP A)))
        (REDERR "Error in spsub_matrix(first argument): should be a matrix.")))
      (SETQ NEW_MAT (SPSTACK_ROWS A ROW_LIST))
      (SETQ NEW_MAT (SPAUGMENT_COLUMNS NEW_MAT COL_LIST))
      (RETURN NEW_MAT))) 
(FLAG '(SPSUB_MATRIX) 'OPFN) 
(PUT 'SPPIVOT 'NUMBER-OF-ARGS 3) 
(PUT 'SPPIVOT 'DEFINED-ON-LINE '1646) 
(PUT 'SPPIVOT 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPPIVOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPPIVOT (IN_MAT PIVOT_ROW PIVOT_COL)
    (PROG (PIV_MAT RATIO VAL COL VAL1 VAL2 I J ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND
       ((AND (NOT *FAST_LA) (NOT (MATRIXP IN_MAT)))
        (REDERR "Error in sppivot(first argument): should be a matrix.")))
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (FIXP PIVOT_ROW))
           (REDERR
            "Error in sppivot(second argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_ROW ROWDIM) (EQUAL PIVOT_ROW 0))
           (REDERR
            "Error in sppivot(second argument): out of range for input matrix.")))
         (COND
          ((NOT (FIXP PIVOT_COL))
           (REDERR "Error in sppivot(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_COL COLDIM) (EQUAL PIVOT_COL 0))
           (REDERR
            "Error in sppivot(third argument): out of range for input matrix.")))
         (COND
          ((EQUAL (FINDELEM2 IN_MAT PIVOT_ROW PIVOT_COL) 0)
           (REDERR "Error in sppivot: cannot pivot on a zero entry.")))
         NIL)))
      (SETQ PIV_MAT (SP-COPY-VECT IN_MAT NIL))
      (SETQ VAL2 (FINDELEM2 IN_MAT PIVOT_ROW PIVOT_COL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWDIM I)) (RETURN NIL)))
        (PROGN
         (SETQ COL (FINDROW IN_MAT I))
         (SETQ VAL1 (ATSOC PIVOT_COL COL))
         (COND (VAL1 (SETQ VAL1 (CDR VAL1))))
         (SETQ RATIO (REVAL1 (LIST 'QUOTIENT VAL1 VAL2) T))
         (COND
          (COL
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR COL))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ J (CAR XX))
                  (SETQ VAL1 (CDR XX))
                  (COND ((EQUAL I PIVOT_ROW) (PROGN NIL))
                        (T
                         (PROGN
                          (SETQ VAL
                                  (REVAL1
                                   (LIST 'PLUS VAL1
                                         (LIST 'MINUS
                                               (LIST 'TIMES RATIO
                                                     (FINDELEM2 IN_MAT
                                                      PIVOT_ROW J))))
                                   T))
                          (LETMTR3 (LIST PIV_MAT I J) VAL PIV_MAT NIL)
                          NIL)))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN PIV_MAT))) 
(PUT 'SPROWS_PIVOT 'NUMBER-OF-ARGS 4) 
(PUT 'SPROWS_PIVOT 'DEFINED-ON-LINE '1696) 
(PUT 'SPROWS_PIVOT 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPROWS_PIVOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPROWS_PIVOT (IN_MAT PIVOT_ROW PIVOT_COL ROW_LIST)
    (PROG (PIV_MAT RATIO VAL COL VAL1 VAL2 J ROWDIM COLDIM)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (SETQ ROWDIM (SPROW_DIM IN_MAT))
      (SETQ COLDIM (SPCOL_DIM IN_MAT))
      (COND
       ((NOT *FAST_LA)
        (PROGN
         (COND
          ((NOT (MATRIXP IN_MAT))
           (REDERR
            "Error in sprows_pivot(first argument): should be a matrix.")))
         (COND
          ((NOT (FIXP PIVOT_ROW))
           (REDERR
            "Error in sprows_pivot(second argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_ROW ROWDIM) (EQUAL PIVOT_ROW 0))
           (REDERR
            "Error in sprows_pivot(second argument): out of range for input matrix.")))
         (COND
          ((NOT (FIXP PIVOT_COL))
           (REDERR
            "Error in sprows_pivot(third argument): should be an integer.")))
         (COND
          ((OR (GREATERP PIVOT_COL COLDIM) (EQUAL PIVOT_COL 0))
           (REDERR
            "Error in sprows_pivot(third argument): out of range for input matrix.")))
         NIL)))
      (COND ((ATOM ROW_LIST) (SETQ ROW_LIST (LIST ROW_LIST)))
            ((AND (PAIRP ROW_LIST) (EQUAL (CAR ROW_LIST) 'LIST))
             (SETQ ROW_LIST (CDR ROW_LIST)))
            (T
             (PROGN
              (PRIN2 "***** Error in sprows_pivot(fourth argument): ")
              (PRIN2T
               "      should be either an integer or a list of integers.")
              (RETURN NIL)
              NIL)))
      (COND
       ((EQUAL (FINDELEM2 IN_MAT PIVOT_ROW PIVOT_COL) 0)
        (REDERR "Error in sprows_pivot: cannot pivot on a zero entry.")))
      (SETQ PIV_MAT (SP-COPY-VECT IN_MAT NIL))
      (SETQ VAL2 (FINDELEM2 IN_MAT PIVOT_ROW PIVOT_COL))
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
                  "Error in sprows_pivot: fourth argument contains a non integer.")))
               (COND
                ((OR (GREATERP ELT ROWDIM) (EQUAL ELT 0))
                 (PROGN
                  (PRIN2 "***** Error in sprows_pivot(fourth argument): ")
                  (REDERR
                   "contains row which is out of range for input matrix.")
                  NIL)))
               NIL)))
            (COND ((EQUAL ELT PIVOT_ROW) NIL)
                  (T
                   (SETQ RATIO
                           (REVAL1
                            (LIST 'QUOTIENT (FINDELEM2 IN_MAT ELT PIVOT_COL)
                                  VAL2)
                            T))))
            (SETQ COL (FINDROW IN_MAT ELT))
            (COND
             (COL
              (PROGN
               (PROG (XX)
                 (SETQ XX (CDR COL))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ J (CAR XX))
                     (SETQ VAL1 (CDR XX))
                     (SETQ VAL
                             (REVAL1
                              (LIST 'PLUS VAL1
                                    (LIST 'MINUS
                                          (LIST 'TIMES RATIO
                                                (FINDELEM2 IN_MAT PIVOT_ROW
                                                 J))))
                              T))
                     (LETMTR3 (LIST PIV_MAT ELT J) VAL PIV_MAT NIL)
                     NIL))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               NIL)))
            NIL))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (RETURN PIV_MAT))) 
(FLAG '(SPPIVOT |,| SPROWS_PIVOT) 'OPFN) 
(PUT 'SPJACOBIAN 'NUMBER-OF-ARGS 2) 
(PUT 'SPJACOBIAN 'DEFINED-ON-LINE '1762) 
(PUT 'SPJACOBIAN 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPJACOBIAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPJACOBIAN (EXP_LIST VAR_LIST)
    (PROG (JAC EXP1 VAR1 VAL I J ROWDIM COLDIM)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ ROWDIM 0)
      (SETQ COLDIM 0)
      (COND ((ATOM EXP_LIST) (SETQ EXP_LIST (LIST EXP_LIST)))
            ((NEQ (CAR EXP_LIST) 'LIST)
             (REDERR
              "Error in spjacobian(first argument): expressions must be in a list."))
            (T (SETQ EXP_LIST (CDR EXP_LIST))))
      (COND ((ATOM VAR_LIST) (SETQ VAR_LIST (LIST VAR_LIST)))
            ((NEQ (CAR VAR_LIST) 'LIST)
             (REDERR
              "Error in jacobian(second argument): variables must be in a list."))
            (T (SETQ VAR_LIST (CDR VAR_LIST))))
      (SETQ ROWDIM (LENGTH EXP_LIST))
      (SETQ COLDIM (LENGTH VAR_LIST))
      (SETQ JAC (MKEMPSPMAT ROWDIM (LIST 'SPM ROWDIM COLDIM)))
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
            (SETQ VAL (AEVAL* (LIST 'DF EXP1 VAR1)))
            (COND ((EQUAL VAL 0) NIL) (T (LETMTR3 (LIST JAC I J) VAL JAC NIL)))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN JAC))) 
(FLAG '(SPJACOBIAN) 'OPFN) 
(PUT 'SPHESSIAN 'NUMBER-OF-ARGS 2) 
(PUT 'SPHESSIAN 'DEFINED-ON-LINE '1800) 
(PUT 'SPHESSIAN 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPHESSIAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPHESSIAN (POLY VARIABLES)
    (PROG (HESS_MAT PART1 PART2 ELT ROW COL SQ_SIZE)
      (SETQ ROW 0)
      (SETQ COL 0)
      (SETQ SQ_SIZE 0)
      (COND ((ATOM VARIABLES) (SETQ VARIABLES (LIST VARIABLES)))
            ((EQUAL (CAR VARIABLES) 'LIST) (SETQ VARIABLES (CDR VARIABLES)))
            (T
             (PROGN
              (PRIN2 "***** Error in sphessian(second argument): ")
              (PRIN2T
               "      should be either a single variable or a list of variables.")
              (RETURN NIL)
              NIL)))
      (SETQ SQ_SIZE (LENGTH VARIABLES))
      (SETQ HESS_MAT (MKEMPSPMAT SQ_SIZE (LIST 'SPM SQ_SIZE SQ_SIZE)))
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
            (COND ((EQUAL ELT 0) NIL)
                  (T (LETMTR3 (LIST HESS_MAT ROW COL) ELT HESS_MAT NIL)))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN HESS_MAT))) 
(FLAG '(SPHESSIAN) 'OPFN) 
(PUT 'SPCOEFF_MATRIX 'PSOPFN 'SPCOEFF_MATRIX1) 
(PUT 'SPCOEFF_MATRIX1 'NUMBER-OF-ARGS 1) 
(PUT 'SPCOEFF_MATRIX1 'DEFINED-ON-LINE '1840) 
(PUT 'SPCOEFF_MATRIX1 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCOEFF_MATRIX1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCOEFF_MATRIX1 (EQUATION_LIST)
    (PROG (VARIABLE_LIST A X B)
      (COND
       ((AND (PAIRP (CAR EQUATION_LIST)) (EQUAL (CAAR EQUATION_LIST) 'LIST))
        (SETQ EQUATION_LIST (CDAR EQUATION_LIST))))
      (SETQ EQUATION_LIST (REMOVE_EQUALS EQUATION_LIST))
      (SETQ VARIABLE_LIST (GET_VARIABLE_LIST EQUATION_LIST))
      (COND
       ((EQUAL VARIABLE_LIST NIL)
        (REDERR "Error in spcoeff_matrix: no variables in input.")))
      (CHECK_LINEARITY EQUATION_LIST VARIABLE_LIST)
      (SETQ A (SPGET_A EQUATION_LIST VARIABLE_LIST))
      (SETQ X (SPGET_X VARIABLE_LIST))
      (SETQ B (SPGET_B EQUATION_LIST VARIABLE_LIST))
      (RETURN (LIST 'LIST A X B)))) 
(PUT 'REMOVE_EQUALS 'NUMBER-OF-ARGS 1) 
(PUT 'REMOVE_EQUALS 'DEFINED-ON-LINE '1856) 
(PUT 'REMOVE_EQUALS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
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
(PUT 'GET_VARIABLE_LIST 'DEFINED-ON-LINE '1869) 
(PUT 'GET_VARIABLE_LIST 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
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
(PUT 'CHECK_LINEARITY 'DEFINED-ON-LINE '1882) 
(PUT 'CHECK_LINEARITY 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
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
                   "Error in spcoeff_matrix: the equations are not linear.")))
                NIL))
             (CAR VARIABLE))
            (SETQ VARIABLE (CDR VARIABLE))
            (GO LAB))
          NIL))
       (CAR EQUATION))
      (SETQ EQUATION (CDR EQUATION))
      (GO LAB))) 
(PUT 'SPGET_A 'NUMBER-OF-ARGS 2) 
(PUT 'SPGET_A 'DEFINED-ON-LINE '1897) 
(PUT 'SPGET_A 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPGET_A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPGET_A (EQUATION_LIST VARIABLE_LIST)
    (PROG (A ELEMENT VAR_ELT VAL ROW COL LENGTH_EQUATION_LIST
           LENGTH_VARIABLE_LIST)
      (SETQ ROW 0)
      (SETQ COL 0)
      (SETQ LENGTH_EQUATION_LIST 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ LENGTH_EQUATION_LIST (LENGTH EQUATION_LIST))
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ A
              (MKEMPSPMAT (LENGTH EQUATION_LIST)
               (LIST 'SPM (LENGTH EQUATION_LIST) (LENGTH VARIABLE_LIST))))
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
            (SETQ VAL (AEVAL* (LIST 'COEFFN ELEMENT VAR_ELT 1)))
            (COND ((EQUAL VAL 0) NIL) (T (LETMTR3 (LIST A ROW COL) VAL A NIL)))
            NIL)
           (SETQ COL (PLUS2 COL 1))
           (GO LAB))
         NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN A))) 
(PUT 'SPGET_B 'NUMBER-OF-ARGS 2) 
(PUT 'SPGET_B 'DEFINED-ON-LINE '1919) 
(PUT 'SPGET_B 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPGET_B 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPGET_B (EQUATION_LIST VARIABLE_LIST)
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
      (SETQ B
              (MKEMPSPMAT LENGTH_INTEGER_LIST
               (LIST 'SPM LENGTH_INTEGER_LIST 1)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LENGTH_INTEGER_LIST ROW)) (RETURN NIL)))
        (LETMTR3 (LIST B ROW 1) (MINUS (NTH INTEGER_LIST ROW)) B NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN B))) 
(PUT 'SPGET_X 'NUMBER-OF-ARGS 1) 
(PUT 'SPGET_X 'DEFINED-ON-LINE '1940) 
(PUT 'SPGET_X 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPGET_X 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPGET_X (VARIABLE_LIST)
    (PROG (X ROW LENGTH_VARIABLE_LIST)
      (SETQ ROW 0)
      (SETQ LENGTH_VARIABLE_LIST 0)
      (SETQ LENGTH_VARIABLE_LIST (LENGTH VARIABLE_LIST))
      (SETQ X
              (MKEMPSPMAT LENGTH_VARIABLE_LIST
               (LIST 'SPM LENGTH_VARIABLE_LIST 1)))
      (PROG (ROW)
        (SETQ ROW 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VARIABLE_LIST) ROW)) (RETURN NIL)))
        (LETMTR3 (LIST X ROW 1) (NTH VARIABLE_LIST ROW) X NIL)
        (SETQ ROW (PLUS2 ROW 1))
        (GO LAB))
      (RETURN X))) 
(PUT 'GET_COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'GET_COEFFS 'DEFINED-ON-LINE '1953) 
(PUT 'GET_COEFFS 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'GET_COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_COEFFS (POLY)
    (PROG (KER_LIST_NUM KER_LIST_DEN)
      (SETQ KER_LIST_NUM (KERNELS (*Q2F (SIMP (REVAL1 (NUM POLY) T)))))
      (SETQ KER_LIST_DEN (KERNELS (*Q2F (SIMP (REVAL1 (DEN POLY) T)))))
      (SETQ KER_LIST_NUM (UNION KER_LIST_NUM KER_LIST_DEN))
      (RETURN KER_LIST_NUM))) 
(PUT 'SPCOMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'SPCOMPANION 'DEFINED-ON-LINE '1972) 
(PUT 'SPCOMPANION 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPCOMPANION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPCOMPANION (POLY X)
    (PROG (MAT1 N VAL)
      (SETQ N 0)
      (SETQ VAL 0)
      (SETQ N (DEG POLY X))
      (COND
       ((NEQ
         ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T)))) (COEFFN POLY X N))
         1)
        (MSGPRI "Error in spcompanion(first argument): Polynomial" POLY
                "is not monic." NIL T)))
      (SETQ MAT1 (MKEMPSPMAT N (LIST 'SMP N N)))
      (SETQ VAL (COEFFN POLY X 0))
      (COND ((EQUAL VAL 0) NIL)
            (T (LETMTR3 (LIST MAT1 1 N) (LIST 'MINUS VAL) MAT1 NIL)))
      (PROG (I)
        (SETQ I 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN (LETMTR3 (LIST MAT1 I (DIFFERENCE I 1)) 1 MAT1 NIL) NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ VAL (COEFFN POLY X (DIFFERENCE J 1)))
         (COND ((EQUAL VAL 0) NIL)
               (T (LETMTR3 (LIST MAT1 J N) (LIST 'MINUS VAL) MAT1 NIL)))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN MAT1))) 
(PUT 'SPFIND_COMPANION 'NUMBER-OF-ARGS 2) 
(PUT 'SPFIND_COMPANION 'DEFINED-ON-LINE '2000) 
(PUT 'SPFIND_COMPANION 'DEFINED-IN-FILE 'SPARSE/SPLINALG.RED) 
(PUT 'SPFIND_COMPANION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPFIND_COMPANION (R X)
    (PROG (P ROWDIM K)
      (SETQ ROWDIM 0)
      (SETQ K 0)
      (COND
       ((NOT (MATRIXP R))
        (REDERR
         (LIST
          "Error in spfind_companion(first argument): should be a matrix."))))
      (SETQ ROWDIM (SPROW_DIM R))
      (SETQ K 2)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LEQ K ROWDIM) (EQUAL (FINDELEM2 R K (DIFFERENCE K 1)) 1)))
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
                       (LIST 'TIMES
                             (LIST 'MINUS (FINDELEM2 R J (DIFFERENCE K 1)))
                             (LIST 'EXPT X (DIFFERENCE J 1)))))
         NIL)
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ P (LIST 'PLUS P (LIST 'EXPT X (DIFFERENCE K 1))))
      (RETURN P))) 
(FLAG '(SPCOMPANION |,| SPFIND_COMPANION) 'OPFN) 
(ENDMODULE) 