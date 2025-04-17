(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPARSMAT)) 
(PUT 'MATRIX 'EVFN 'SPMATSM*) 
(PUT 'SPARSE 'NUMBER-OF-ARGS 1) 
(PUT 'SPARSE 'DEFINED-ON-LINE '48) 
(PUT 'SPARSE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPARSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPARSE (U)
    (PROG (V W X)
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((ATOM J)
             (COND ((NULL (SETQ X (GETTYPE J))) (PUT J 'RTYPE 'SPARSE))
                   ((EQ X 'SPARSE)
                    (PROGN
                     (LPRIM (LIST X J "redefined"))
                     (PUT J 'RTYPE 'SPARSE)))
                   (T (TYPERR (LIST X J) "sparse"))))
            ((OR (NOT (IDP (CAR J))) (NEQ (LENGTH (SETQ V (REVLIS (CDR J)))) 2)
                 (NOT (NATNUMLIS V)))
             (ERRPRI2 J 'HOLD))
            ((OR (NOT (SETQ X (GETTYPE (CAR J)))) (EQ X 'SPARSE))
             (PROGN
              (SETQ W NIL)
              (PUT (CAR J) 'RTYPE 'SPARSE)
              (PUT (CAR J) 'AVALUE
                   (LIST 'SPARSE
                         (LIST 'SPARSEMAT (MKVECT (CADR J))
                               (CONS 'SPM (CDR J)))))
              NIL))
            (T (TYPERR (LIST X (CAR J)) "sparse"))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB)))) 
(PUT 'NATNUMLIS 'NUMBER-OF-ARGS 1) 
(PUT 'NATNUMLIS 'DEFINED-ON-LINE '72) 
(PUT 'NATNUMLIS 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'NATNUMLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NATNUMLIS (U)
    (OR (NULL U) (AND (FIXP (CAR U)) (GREATERP (CAR U) 0) (NATNUMLIS (CDR U))))) 
(RLISTAT '(SPARSE)) 
(PUT 'SPARSEMAT 'RTYPEFN 'SPQUOTEMATRIX) 
(PUT 'SPQUOTEMATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'SPQUOTEMATRIX 'DEFINED-ON-LINE '92) 
(PUT 'SPQUOTEMATRIX 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPQUOTEMATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPQUOTEMATRIX (U) 'SPARSE) 
(FLAG '(SPARSEMAT TP) 'SPMATFLG) 
(FLAG '(SPARSEMAT) 'NONCOMMUTING) 
(PUT 'SPARSEMAT 'PRIFN 'MYSPMATPRI2) 
(FLAG '(SPARSEMAT) 'STRUCT) 
(PUT 'SPARSE 'FN 'SPMATFLG) 
(PUT 'SPARSE 'EVFN 'SPMATSM*) 
(FLAG '(SPARSE) 'PRIFN) 
(FLAG '(SPARSE) 'SPRIFN) 
(PUT 'SPARSE 'TAG 'SPARSEMAT) 
(PUT 'SPARSE 'LENGTHFN 'SPMATLENGTH) 
(PUT 'SPARSE 'GETELEMFN 'GETSPMATELEM2) 
(PUT 'SPARSE 'SETELEMFN 'SETSPMATELEM2) 
(PUT 'SPMATSM* 'NUMBER-OF-ARGS 2) 
(PUT 'SPMATSM* 'DEFINED-ON-LINE '120) 
(PUT 'SPMATSM* 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMATSM* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPMATSM* (U V)
    (PROG (X)
      (PROGN
       (SETQ X (SPMATSM U))
       (COND ((EQCAR X 'SPARSEMAT) (RETURN X)) (T (RETURN (MATSM*1 X))))
       NIL))) 
(PUT 'SPMKSCALMAT 'NUMBER-OF-ARGS 1) 
(PUT 'SPMKSCALMAT 'DEFINED-ON-LINE '132) 
(PUT 'SPMKSCALMAT 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMKSCALMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMKSCALMAT (U) (LIST 'SPARSEMAT (LIST 'SPM 1 1))) 
(PUT 'SORTROWELEM 'NUMBER-OF-ARGS 5) 
(PUT 'SORTROWELEM 'DEFINED-ON-LINE '138) 
(PUT 'SORTROWELEM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SORTROWELEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SORTROWELEM (ROW U VAL Y LEN)
    (PROG (X V ELEM LIS)
      (SETQ V U)
      (SETQ X U)
      (SETQ LIS U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL ELEM NIL)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL V NIL)
           (PROGN (RPLACD X (LIST (CONS ROW (LIST VAL)))) (SETQ ELEM T)))
          ((NOT (EQUAL (CAR V) NIL))
           (PROGN
            (COND
             ((LESSP ROW (CAAR V))
              (PROGN
               (COND
                ((EQUAL (CAR V) (CAR LIS))
                 (RPLACD Y
                         (APPEND (LIST (CONS (CONS ROW (LIST VAL)) V))
                                 (LIST LEN))))
                (T (RPLACD X (RPLACD (LIST (CONS ROW (LIST VAL))) V))))
               (SETQ ELEM T)))
             ((GREATERP ROW (CAAR V))
              (PROGN
               (SETQ ELEM NIL)
               (SETQ X U)
               (SETQ U (CDR U))
               (SETQ V U))))))
          (T
           (PROGN
            (RPLACD Y (LIST (LIST (CONS ROW (LIST VAL))) LEN))
            (SETQ ELEM T))))
         NIL)
        (GO WHILELABEL)))) 
(PUT 'SORTCOLELEM 'NUMBER-OF-ARGS 3) 
(PUT 'SORTCOLELEM 'DEFINED-ON-LINE '160) 
(PUT 'SORTCOLELEM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SORTCOLELEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SORTCOLELEM (COL U VAL)
    (PROG (V ELEM)
      (SETQ V (CDR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL ELEM NIL)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL V NIL)
           (PROGN (RPLACD U (LIST (CONS COL VAL))) (SETQ ELEM T)))
          ((LESSP COL (CAAR V))
           (PROGN (RPLACD U (RPLACD (LIST (CONS COL VAL)) V)) (SETQ ELEM T)))
          ((GREATERP COL (CAAR V))
           (PROGN (SETQ ELEM NIL) (SETQ U (CDR U)) (SETQ V (CDR U)))))
         NIL)
        (GO WHILELABEL)))) 
(PUT 'LENGTHREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LENGTHREVAL 'DEFINED-ON-LINE '175) 
(PUT 'LENGTHREVAL 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'LENGTHREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LENGTHREVAL (U)
    (PROG (V W X)
      (COND
       ((NEQ (LENGTH U) 1)
        (RERROR 'ALG 11 "LENGTH called with wrong number of arguments")))
      (SETQ U (CAR U))
      (COND
       ((AND (IDP U) (ARRAYP U)) (RETURN (CONS 'LIST (GET U 'DIMENSION)))))
      (SETQ V (REVAL1 U NIL))
      (COND
       ((AND (SETQ W (GETRTYPE V)) (SETQ X (GET W 'LENGTHFN)))
        (COND ((EQUAL W 'SPARSE) (RETURN (APPLY1 X U)))
              (T (RETURN (APPLY1 X V)))))
       ((ATOM V) (RETURN 1))
       ((OR (NOT (IDP (CAR V))) (NOT (SETQ X (GET (CAR V) 'LENGTHFN))))
        (COND (W (LPRIE (LIST "LENGTH not defined for argument of type" W)))
              (T (TYPERR U "LENGTH argument"))))
       (T (RETURN (APPLY1 X (CDR V))))))) 
(PUT 'SPMATLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'SPMATLENGTH 'DEFINED-ON-LINE '194) 
(PUT 'SPMATLENGTH 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMATLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMATLENGTH (U)
    (PROG (Y X)
      (COND ((PAIRP U) (SETQ X U)) (T (SETQ X (CADR (GET U 'AVALUE)))))
      (SETQ Y (CDR (CADDR X)))
      (COND
       ((NOT (EQCAR X 'SPARSEMAT))
        (RERROR 'MATRIX 2 (LIST "Matrix" U "not set")))
       (T (RETURN (LIST 'LIST (CAR Y) (CADR Y))))))) 
(PUT 'GETSPMATELEM2 'NUMBER-OF-ARGS 1) 
(PUT 'GETSPMATELEM2 'DEFINED-ON-LINE '205) 
(PUT 'GETSPMATELEM2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'GETSPMATELEM2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETSPMATELEM2 (U)
    (PROG (X Y P)
      (SETQ X (GET (CAR U) 'AVALUE))
      (SETQ Y (CDR (CADDR (CADR X))))
      (COND
       ((OR (NULL X) (NOT (EQ (CAR X) 'SPARSE))) (TYPERR (CAR U) "sparse"))
       ((NOT (EQCAR (SETQ X (CADR X)) 'SPARSEMAT))
        (COND ((IDP X) (RETURN (GETMATELEM2 (CONS X (CDR U)))))
              (T (RERROR 'SPARSE 1 (LIST "Matrix" (CAR U) "not set")))))
       ((OR (NEQ (LENGTH (SETQ P (REVLIS (CDR U)))) 2)
            (NOT (AND (FIXP (CAR P)) (GREATERP (CAR P) 0)))
            (NOT (AND (FIXP (CADR P)) (GREATERP (CADR P) 0))))
        (MSGPRI "Invalid sparse matrix index:" (CONS (CAR U) P) NIL NIL T))
       ((OR (GREATERP (CAR P) (CAR Y)) (GREATERP (CADR P) (CADR Y)))
        (TYPERR (CAR P) "The dimensions are wrong - matrix unaligned"))
       (T (RETURN (FINDELEM2 X (CAR P) (CADR P))))))) 
(PUT 'FINDELEM2 'NUMBER-OF-ARGS 3) 
(PUT 'FINDELEM2 'DEFINED-ON-LINE '226) 
(PUT 'FINDELEM2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'FINDELEM2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FINDELEM2 (X ROW COL)
    (PROG (LIST RLIST COLIST RES)
      (COND ((AND (PAIRP X) (EQ (CAR X) 'SPARSEMAT)) (SETQ LIST (CADR X)))
            (T (SETQ LIST X)))
      (SETQ RLIST (GETV LIST ROW))
      (SETQ COLIST (ATSOC COL RLIST))
      (COND ((EQUAL COLIST NIL) (SETQ RES 0)) (T (SETQ RES (CDR COLIST))))
      (RETURN RES))) 
(PUT 'FINDROW 'NUMBER-OF-ARGS 2) 
(PUT 'FINDROW 'DEFINED-ON-LINE '237) 
(PUT 'FINDROW 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'FINDROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDROW (X ROW)
    (PROG (LIST RLIST)
      (COND ((AND (PAIRP X) (EQ (CAR X) 'SPARSEMAT)) (SETQ LIST (CADR X)))
            (T (SETQ LIST X)))
      (SETQ RLIST (GETV LIST ROW))
      (RETURN RLIST))) 
(PUT 'MKEMPSPMAT 'NUMBER-OF-ARGS 2) 
(PUT 'MKEMPSPMAT 'DEFINED-ON-LINE '245) 
(PUT 'MKEMPSPMAT 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MKEMPSPMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKEMPSPMAT (ROW LEN)
    (PROG (RES) (SETQ RES (LIST 'SPARSEMAT (MKVECT ROW) LEN)) (RETURN RES))) 
(PUT 'SP-COPY-VECT 'NUMBER-OF-ARGS 2) 
(PUT 'SP-COPY-VECT 'DEFINED-ON-LINE '251) 
(PUT 'SP-COPY-VECT 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SP-COPY-VECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SP-COPY-VECT (LIST LEN)
    (PROG (OLDVEC NEWVEC)
      (SETQ OLDVEC (CADR LIST))
      (SETQ NEWVEC (FULLCOPY OLDVEC))
      (COND ((NOT LEN) (SETQ LEN (CADDR LIST))))
      (RETURN (LIST 'SPARSEMAT NEWVEC LEN)))) 
(PUT 'FULLCOPY 'NUMBER-OF-ARGS 1) 
(PUT 'FULLCOPY 'DEFINED-ON-LINE '263) 
(PUT 'FULLCOPY 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'FULLCOPY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FULLCOPY (S)
    (COND ((PAIRP S) (CONS (FULLCOPY (CAR S)) (FULLCOPY (CDR S))))
          ((VECTORP S)
           (PROG (COP SI)
             (SETQ SI 0)
             (SETQ SI (UPBV S))
             (SETQ COP (MKVECT SI))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE SI I)) (RETURN NIL)))
               (PUTV COP I (FULLCOPY (GETV S I)))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN COP)))
          (T S))) 
(PUT 'LETMTR3 'NUMBER-OF-ARGS 4) 
(PUT 'LETMTR3 'DEFINED-ON-LINE '280) 
(PUT 'LETMTR3 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'LETMTR3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LETMTR3 (U V Y TYP)
    (PROG (Z ROWELEM COLELEM LEN LIST)
      (SETQ LEN (CADDR Y))
      (COND
       ((EQUAL (CDDR U) NIL)
        (PROGN
         (COND
          ((NOT (EQCAR Y 'SPARSEMAT))
           (RERROR 'MATRIX 10 (LIST "Matrix" (CAR U) "not set")))
          ((OR (NOT (NUMLIS (SETQ Z (REVLIS (CDR U))))) (NEQ (LENGTH Z) 1))
           (RETURN (ERRPRI2 U 'HOLD))))
         (PUTV (CADR Y) (CADR U) V)
         NIL))
       (T
        (PROGN
         (COND
          ((NOT (EQCAR Y 'SPARSEMAT))
           (RERROR 'MATRIX 10 (LIST "Matrix" (CAR U) "not set")))
          ((OR (NOT (NUMLIS (SETQ Z (REVLIS (CDR U))))) (NEQ (LENGTH Z) 2))
           (RETURN (ERRPRI2 U 'HOLD))))
         (SETQ ROWELEM (GETV (CADR Y) (CAR Z)))
         (COND
          ((EQUAL ROWELEM NIL)
           (PROGN
            (COND ((AND (EQUAL V 0) (NOT (EQUAL TYP 'CX))) NIL)
                  (T
                   (PUTV (CADR Y) (CAR Z)
                         (LIST (LIST NIL) (CONS (CADR Z) V)))))
            NIL))
          (T
           (PROGN
            (SETQ COLELEM (ATSOC (CADR Z) ROWELEM))
            (COND
             ((EQUAL COLELEM NIL)
              (PROGN
               (COND ((AND (EQUAL V 0) (NOT (EQUAL TYP 'CX))) NIL)
                     (T (SORTCOLELEM (CADR Z) ROWELEM V)))
               NIL))
             (T
              (PROGN
               (COND
                ((AND (EQUAL V 0) (NOT (EQUAL TYP 'CX)))
                 (PROGN
                  (COND
                   ((EQUAL (CDDR ROWELEM) NIL)
                    (PROGN (PUTV (CADR Y) (CAR Z) NIL) NIL))
                   (T (RPLACD ROWELEM (CDR (DELETE COLELEM ROWELEM)))))
                  NIL))
                (T (RPLACD COLELEM V)))
               NIL)))
            NIL)))
         NIL))))) 
(PUT 'SETSPMATELEM2 'NUMBER-OF-ARGS 2) 
(PUT 'SETSPMATELEM2 'DEFINED-ON-LINE '322) 
(PUT 'SETSPMATELEM2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SETSPMATELEM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETSPMATELEM2 (U V)
    (PROG (X Y P)
      (SETQ X (GET (CAR U) 'AVALUE))
      (COND
       ((OR (NULL X) (NOT (EQ (CAR X) 'SPARSE)))
        (TYPERR (CAR U) "sparse matrix"))
       ((NOT (EQCAR (SETQ X (CADR X)) 'SPARSEMAT))
        (RERROR 'SPARSE 10 (LIST "Sparse matrix" (CAR U) "not set"))))
      (SETQ Y (CDR (CADDR X)))
      (SETQ P (REVLIS (CDR U)))
      (COND
       ((OR (NEQ (LENGTH P) 2) (NOT (AND (FIXP (CAR P)) (GREATERP (CAR P) 0)))
            (NOT (AND (FIXP (CADR P)) (GREATERP (CADR P) 0))))
        (MSGPRI "Invalid sparse matrix index:" (CONS (CAR U) P) NIL NIL T))
       ((OR (GREATERP (CAR P) (CAR Y)) (GREATERP (CADR P) (CADR Y)))
        (TYPERR (CAR U) "The dimensions are wrong - matrix unaligned"))
       (T (RETURN (LETMTR3 U V X NIL)))))) 
(PUT 'SP-NOTEMPTY-P 'NUMBER-OF-ARGS 2) 
(PUT 'SP-NOTEMPTY-P 'DEFINED-ON-LINE '341) 
(PUT 'SP-NOTEMPTY-P 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SP-NOTEMPTY-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SP-NOTEMPTY-P (VEC VAL)
    (PROG (RES I)
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NOT RES) (LEQ I VAL))) (RETURN NIL)))
        (COND ((NOT (NULL (GETV VEC I))) (PROGN (SETQ RES T) (GO DONE) NIL))
              (T (SETQ I (PLUS I 1))))
        (GO WHILELABEL))
     DONE
      (RETURN RES))) 
(PUT 'SPARPRI 'NUMBER-OF-ARGS 3) 
(PUT 'SPARPRI 'DEFINED-ON-LINE '355) 
(PUT 'SPARPRI 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPARPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPARPRI (U I NAM)
    (PROG (VAL ROW)
      (SETQ VAL U)
      (SETQ ROW I)
      (PROG (X)
        (SETQ X VAL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND (*FORT (ASSGNPRI (CDR X) (LIST (LIST NAM ROW (CAR X))) 'ONLY))
                 (T
                  (PROGN
                   (WRITEPRI
                    (LIST 'QUOTE (LIST 'SETQ (LIST NAM ROW (CAR X)) (CDR X)))
                    'FIRST)
                   (WRITEPRI ''$ 'LAST)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'MYSPMATPRI2 'NUMBER-OF-ARGS 1) 
(PUT 'MYSPMATPRI2 'DEFINED-ON-LINE '368) 
(PUT 'MYSPMATPRI2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MYSPMATPRI2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYSPMATPRI2 (U) (MYSPMATPRI1 U NIL)) 
(PUT 'MYSPMATPRI1 'NUMBER-OF-ARGS 2) 
(PUT 'MYSPMATPRI1 'DEFINED-ON-LINE '370) 
(PUT 'MYSPMATPRI1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MYSPMATPRI1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MYSPMATPRI1 (U NAM)
    (PROG (MATR LIST FL)
      (PROGN
       (SETQ MATR (CADR U))
       (COND ((NULL NAM) (SETQ NAM 'SPM)))
       (SETQ FL (SP-NOTEMPTY-P MATR (CADR (CADDR U))))
       (COND
        (FL
         (PROGN
          (PROG (I)
            (SETQ I 1)
           LAB
            (COND ((MINUSP (DIFFERENCE (CADR (CADDR U)) I)) (RETURN NIL)))
            (PROGN
             (SETQ LIST (GETV MATR I))
             (COND ((NOT (EQUAL LIST NIL)) (SPARPRI (CDR LIST) I NAM))))
            (SETQ I (PLUS2 I 1))
            (GO LAB))
          NIL))
        (T (PRINT "Empty Matrix")))
       NIL))) 
(PUT 'SPSETMATPRI 'NUMBER-OF-ARGS 2) 
(PUT 'SPSETMATPRI 'DEFINED-ON-LINE '388) 
(PUT 'SPSETMATPRI 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPSETMATPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPSETMATPRI (U V) (MYSPMATPRI1 V U)) 
(PUT 'SPARSEMAT 'SETPRIFN 'SPSETMATPRI) 
(PUT 'SMTP 'NUMBER-OF-ARGS 2) 
(PUT 'SMTP 'DEFINED-ON-LINE '398) 
(PUT 'SMTP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SMTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SMTP (U TYP)
    (PROG (X TM ROW NEWCOL NEWROW VAL LEN COL ROWS)
      (COND
       ((ATOM U) (PROGN (SETQ X (CADR (GET U 'AVALUE))) (SETQ LEN (CADDR X))))
       ((EQCAR U 'SPARSEMAT) (PROGN (SETQ X U) (SETQ LEN (CADDR X))))
       (T (PROGN (SETQ X (SMTP (CADR U) TYP)) (SETQ LEN (CADDR X)))))
      (SETQ ROW (CADR LEN))
      (SETQ COL (CADDR LEN))
      (SETQ TM (MKEMPSPMAT COL (LIST 'SPM COL ROW)))
      (COND
       ((NOT (EQCAR X 'SPARSEMAT))
        (RERROR 'MATRIX 2 (LIST "Matrix" U "not set")))
       (T
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE ROW I)) (RETURN NIL)))
          (PROGN
           (SETQ ROWS (FINDROW X I))
           (COND
            ((NOT (EQUAL ROWS NIL))
             (PROGN
              (SETQ NEWCOL I)
              (PROG (COLS)
                (SETQ COLS (CDR ROWS))
               LAB
                (COND ((NULL COLS) (RETURN NIL)))
                ((LAMBDA (COLS)
                   (PROGN
                    (SETQ NEWROW (CAR COLS))
                    (SETQ VAL (CDR COLS))
                    (LETMTR3 (LIST TM NEWROW NEWCOL) VAL TM TYP)))
                 (CAR COLS))
                (SETQ COLS (CDR COLS))
                (GO LAB))
              NIL)))
           NIL)
          (SETQ I (PLUS2 I 1))
          (GO LAB))))
      (RETURN TM))) 
(PUT 'TP 'NUMBER-OF-ARGS 1) 
(PUT 'TP 'DEFINED-ON-LINE '421) 
(PUT 'TP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'TP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TP (U)
    (COND ((EQUAL (CHECKSP U) 'SPARSE) (SMTP (SPMATSM U) NIL))
          (T (TP1 (SPMATSM U))))) 
(PUT 'TRANSMAT1 'NUMBER-OF-ARGS 1) 
(PUT 'TRANSMAT1 'DEFINED-ON-LINE '435) 
(PUT 'TRANSMAT1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'TRANSMAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRANSMAT1 (U)
    (PROG (VEC V X RCNT CCNT ELEM ROW RLIST)
      (SETQ X (CDR (REVAL1 (CAR U) NIL)))
      (SETQ RCNT 0)
      (SETQ CCNT 0)
      (SETQ V (CDR (MATLENGTH (REVAL1 (CAR U) NIL))))
      (SETQ VEC (MKEMPSPMAT (CAR V) (CONS 'SPM V)))
      (SETQ RLIST (LIST (LIST NIL)))
      (PROG (ROWS)
        (SETQ ROWS X)
       LAB
        (COND ((NULL ROWS) (RETURN NIL)))
        ((LAMBDA (ROWS)
           (PROGN
            (SETQ ROW ROWS)
            (SETQ RCNT (PLUS RCNT 1))
            (PROG (COLS)
              (SETQ COLS ROW)
             LAB
              (COND ((NULL COLS) (RETURN NIL)))
              ((LAMBDA (COLS)
                 (PROGN
                  (SETQ ELEM COLS)
                  (SETQ CCNT (PLUS CCNT 1))
                  (COND ((EQUAL ELEM 0) NIL)
                        (T (SETQ RLIST (CONS (CONS CCNT ELEM) RLIST))))))
               (CAR COLS))
              (SETQ COLS (CDR COLS))
              (GO LAB))
            (SETQ RLIST (REVERSE RLIST))
            (COND
             ((NOT (EQUAL RLIST (LIST (LIST NIL))))
              (LETMTR3 (LIST VEC RCNT) RLIST VEC NIL)))
            (SETQ CCNT 0)
            (SETQ RLIST (LIST (LIST NIL)))
            NIL))
         (CAR ROWS))
        (SETQ ROWS (CDR ROWS))
        (GO LAB))
      (PUT (CAR U) 'AVALUE (LIST 'SPARSE VEC))
      (PUT (CAR U) 'RTYPE 'SPARSE))) 
(PUT 'TRANSMAT 'PSOPFN 'TRANSMAT1) 
(PUT 'SPTRANSMAT 'NUMBER-OF-ARGS 1) 
(PUT 'SPTRANSMAT 'DEFINED-ON-LINE '470) 
(PUT 'SPTRANSMAT 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPTRANSMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPTRANSMAT (U)
    (PROG (V X RCNT CCNT ELEM ROW RLIST VEC)
      (COND ((PAIRP U) (PROGN (SETQ X U) (SETQ V (CDR (MATLENGTH U)))))
            (T
             (PROGN
              (SETQ X (REVAL1 U NIL))
              (SETQ V (CDR (MATLENGTH (REVAL1 U NIL)))))))
      (SETQ RCNT 0)
      (SETQ CCNT 0)
      (SETQ VEC (MKEMPSPMAT (CAR V) (CONS 'SPM V)))
      (SETQ RLIST (LIST (LIST NIL)))
      (PROG (ROWS)
        (SETQ ROWS (CDR X))
       LAB
        (COND ((NULL ROWS) (RETURN NIL)))
        ((LAMBDA (ROWS)
           (PROGN
            (SETQ ROW ROWS)
            (SETQ RCNT (PLUS RCNT 1))
            (PROG (COLS)
              (SETQ COLS ROW)
             LAB
              (COND ((NULL COLS) (RETURN NIL)))
              ((LAMBDA (COLS)
                 (PROGN
                  (SETQ ELEM COLS)
                  (SETQ CCNT (PLUS CCNT 1))
                  (COND ((EQUAL ELEM 0) NIL)
                        (T (SETQ RLIST (CONS (CONS CCNT ELEM) RLIST))))))
               (CAR COLS))
              (SETQ COLS (CDR COLS))
              (GO LAB))
            (SETQ RLIST (REVERSE RLIST))
            (COND
             ((NOT (EQUAL RLIST (LIST (LIST NIL))))
              (LETMTR3 (LIST VEC RCNT) RLIST VEC NIL)))
            (SETQ CCNT 0)
            (SETQ RLIST (LIST (LIST NIL)))
            NIL))
         (CAR ROWS))
        (SETQ ROWS (CDR ROWS))
        (GO LAB))
      (RETURN VEC))) 
(PUT 'TRANS 'NUMBER-OF-ARGS 1) 
(PUT 'TRANS 'DEFINED-ON-LINE '496) 
(PUT 'TRANS 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'TRANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRANS (U)
    (PROG (X RES)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ X (CHECKSP (CAR U)))
         (COND
          ((OR (EQUAL X NIL) (EQUAL X 'SPARSE))
           (PROGN (SETQ RES (CONS (CAR U) RES)) (SETQ U (CDR U))))
          ((EQUAL X 'MATRIX)
           (PROGN
            (COND
             ((PAIRP (CAR U))
              (PROGN
               (COND
                ((EQUAL (CAAR U) 'MAT)
                 (SETQ RES (CONS (SPTRANSMAT (CAR U)) RES)))
                (T (SETQ RES (CONS (TRANS (CAR U)) RES))))
               NIL))
             (T (SETQ RES (CONS (SPTRANSMAT (CAR U)) RES))))
            (SETQ U (CDR U))
            NIL))
          (T (PROGN (SETQ RES (CONS (TRANS (CAR U)) RES)) (SETQ U (CDR U)))))
         NIL)
        (GO WHILELABEL))
      (RETURN (REVERSE RES)))) 
(PUT 'SPMATSM 'NUMBER-OF-ARGS 1) 
(PUT 'SPMATSM 'DEFINED-ON-LINE '520) 
(PUT 'SPMATSM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMATSM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMATSM (U)
    (PROG (X Y R)
      (COND
       ((EQ (SETQ R (GETRTYPE U)) 'YETUNKNOWNTYPE)
        (SETQ U (EVAL-YETUNKNOWNTYPEEXPR U NIL))))
      (COND
       ((PAIRP U)
        (PROGN
         (COND ((EQCAR U 'SPARSEMAT) (SETQ R 'SPARSE))
               ((EQUAL (CHECKSP U) 'SPARSE) (SETQ R 'SPARSE))
               ((EQUAL (CHECKSP U) 'MATRIX) (SETQ R 'MATRIX))
               (T (PROGN (SETQ U (TRANS U)) (SETQ R 'SPARSE))))
         NIL))
       ((OR (EQ (SETQ R (CHECKSP U)) 'SPARSE)
            (AND (NULL R) (EQ (GETRTYPE U) 'SPARSE)))
        (SETQ R 'SPARSE))
       (T (SETQ R 'MATRIX)))
      (PROG (J)
        (SETQ J (NSSIMP U R))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ Y (MULTSM (CAR J) (MATSM1 (CDR J))))
            (SETQ X (COND ((NULL X) Y) (T (ADDM X Y))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND ((EQUAL (LENGTH X) 1) (RETURN (CAR X))) (T (RETURN X))))) 
(DE MATSM1-XSIMP (U)
    (COND ((NOT (LCHK (CDR U))) (RERROR 'MATRIX 3 "Matrix mismatch"))
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J (CDR U))
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
                                   (CONS ((LAMBDA (K) (XSIMP K)) (CAR K)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR J))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'MATSM1-XSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'MATSM1-XSIMP 'DEFINED-ON-LINE '555) 
(PUT 'MATSM1-XSIMP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MATSM1-XSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MATSM1-XSIMP 'INLINE
      '(LAMBDA (U)
         (COND ((NOT (LCHK (CDR U))) (RERROR 'MATRIX 3 "Matrix mismatch"))
               (T
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J (CDR U))
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
                                                            (XSIMP K))
                                                          (CAR K))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ K (CDR K))
                                        (COND
                                         ((NULL K) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (K) (XSIMP K))
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
                                                 ((LAMBDA (K) (XSIMP K))
                                                  (CAR K))
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
                  (GO LOOPLABEL)))))) 
(DE SPARSEMAT-XSIMP (U)
    (PROG (ROWS NEW OLDROW)
      (SETQ ROWS 0)
      (SETQ ROWS (SPROW_DIM U))
      (SETQ NEW (MKEMPSPMAT ROWS (CADDR U)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
        (PROGN
         (SETQ OLDROW (GETV (CADR U) I))
         (COND
          ((NOT (NULL OLDROW))
           (PUTV (CADR NEW) I
                 (CONS (LIST NIL)
                       (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                         (SETQ EL (CDR OLDROW))
                         (COND ((NULL EL) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (EL)
                                             (CONS (CAR EL) (EXPCHK (CDR EL))))
                                           (CAR EL))
                                          NIL)))
                        LOOPLABEL
                         (SETQ EL (CDR EL))
                         (COND ((NULL EL) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL)
                                     (CONS (CAR EL) (EXPCHK (CDR EL))))
                                   (CAR EL))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW))) 
(PUT 'SPARSEMAT-XSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'SPARSEMAT-XSIMP 'DEFINED-ON-LINE '560) 
(PUT 'SPARSEMAT-XSIMP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPARSEMAT-XSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SPARSEMAT-XSIMP 'INLINE
      '(LAMBDA (U)
         (PROG (ROWS NEW OLDROW)
           (SETQ ROWS 0)
           (SETQ ROWS (SPROW_DIM U))
           (SETQ NEW (MKEMPSPMAT ROWS (CADDR U)))
           (PROG (I)
             (SETQ I 1)
            LAB
             (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
             (PROGN
              (SETQ OLDROW (GETV (CADR U) I))
              (COND
               ((NOT (NULL OLDROW))
                (PUTV (CADR NEW) I
                      (CONS (LIST NIL)
                            (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                              (SETQ EL (CDR OLDROW))
                              (COND ((NULL EL) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (EL)
                                                  (CONS (CAR EL)
                                                        (EXPCHK (CDR EL))))
                                                (CAR EL))
                                               NIL)))
                             LOOPLABEL
                              (SETQ EL (CDR EL))
                              (COND ((NULL EL) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL)
                                          (CONS (CAR EL) (EXPCHK (CDR EL))))
                                        (CAR EL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))))
              NIL)
             (SETQ I (PLUS2 I 1))
             (GO LAB))
           (RETURN NEW)))) 
(DE SPARSEMAT-SUBS2 (U)
    (PROG (ROWS NEW OLDROW)
      (SETQ ROWS 0)
      (SETQ ROWS (SPROW_DIM U))
      (SETQ NEW (MKEMPSPMAT ROWS (CADDR U)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
        (PROGN
         (SETQ OLDROW (GETV (CADR U) I))
         (COND
          ((NOT (NULL OLDROW))
           (PUTV (CADR NEW) I
                 (CONS (LIST NIL)
                       (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                         (SETQ EL (CDR OLDROW))
                         (COND ((NULL EL) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (EL)
                                             (CONS (CAR EL)
                                                   (PROGN
                                                    (SETQ *SUB2 T)
                                                    (MK*SQ
                                                     (SUBS2
                                                      (SIMP (CDR EL)))))))
                                           (CAR EL))
                                          NIL)))
                        LOOPLABEL
                         (SETQ EL (CDR EL))
                         (COND ((NULL EL) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL)
                                     (CONS (CAR EL)
                                           (PROGN
                                            (SETQ *SUB2 T)
                                            (MK*SQ (SUBS2 (SIMP (CDR EL)))))))
                                   (CAR EL))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEW))) 
(PUT 'SPARSEMAT-SUBS2 'NUMBER-OF-ARGS 1) 
(PUT 'SPARSEMAT-SUBS2 'DEFINED-ON-LINE '574) 
(PUT 'SPARSEMAT-SUBS2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPARSEMAT-SUBS2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SPARSEMAT-SUBS2 'INLINE
      '(LAMBDA (U)
         (PROG (ROWS NEW OLDROW)
           (SETQ ROWS 0)
           (SETQ ROWS (SPROW_DIM U))
           (SETQ NEW (MKEMPSPMAT ROWS (CADDR U)))
           (PROG (I)
             (SETQ I 1)
            LAB
             (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
             (PROGN
              (SETQ OLDROW (GETV (CADR U) I))
              (COND
               ((NOT (NULL OLDROW))
                (PUTV (CADR NEW) I
                      (CONS (LIST NIL)
                            (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                              (SETQ EL (CDR OLDROW))
                              (COND ((NULL EL) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (EL)
                                                  (CONS (CAR EL)
                                                        (PROGN
                                                         (SETQ *SUB2 T)
                                                         (MK*SQ
                                                          (SUBS2
                                                           (SIMP (CDR EL)))))))
                                                (CAR EL))
                                               NIL)))
                             LOOPLABEL
                              (SETQ EL (CDR EL))
                              (COND ((NULL EL) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL)
                                          (CONS (CAR EL)
                                                (PROGN
                                                 (SETQ *SUB2 T)
                                                 (MK*SQ
                                                  (SUBS2 (SIMP (CDR EL)))))))
                                        (CAR EL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))))
              NIL)
             (SETQ I (PLUS2 I 1))
             (GO LAB))
           (RETURN NEW)))) 
(PUT 'MATSM1 'NUMBER-OF-ARGS 1) 
(PUT 'MATSM1 'DEFINED-ON-LINE '589) 
(PUT 'MATSM1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MATSM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATSM1 (U)
    (PROG (X Y Z LEN N)
      (SETQ N 0)
     A
      (COND ((NULL U) (RETURN Z)) ((EQCAR (CAR U) '*DIV) (GO D))
            ((ATOM (CAR U)) (GO ER))
            ((EQ (CAAR U) 'MAT)
             (SETQ X
                     (COND
                      ((NOT (LCHK (CDR (CAR U))))
                       (RERROR 'MATRIX 3 "Matrix mismatch"))
                      (T
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J (CDR (CAR U)))
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J)
                                             (PROG (K FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ K J)
                                               (COND ((NULL K) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (K)
                                                                   (XSIMP K))
                                                                 (CAR K))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ K (CDR K))
                                               (COND
                                                ((NULL K)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (K) (XSIMP K))
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
                                                        ((LAMBDA (K) (XSIMP K))
                                                         (CAR K))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ K (CDR K))
                                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (K) (XSIMP K))
                                                 (CAR K))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))
                                   (CAR J))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
            ((EQ (CAAR U) 'SPARSEMAT)
             (SETQ X
                     (PROG (ROWS NEW OLDROW)
                       (SETQ ROWS 0)
                       (SETQ ROWS (SPROW_DIM (CAR U)))
                       (SETQ NEW (MKEMPSPMAT ROWS (CADDR (CAR U))))
                       (PROG (I)
                         (SETQ I 1)
                        LAB
                         (COND ((MINUSP (DIFFERENCE ROWS I)) (RETURN NIL)))
                         (PROGN
                          (SETQ OLDROW (GETV (CADR (CAR U)) I))
                          (COND
                           ((NOT (NULL OLDROW))
                            (PUTV (CADR NEW) I
                                  (CONS (LIST NIL)
                                        (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ EL (CDR OLDROW))
                                          (COND ((NULL EL) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (EL)
                                                              (CONS (CAR EL)
                                                                    (EXPCHK
                                                                     (CDR
                                                                      EL))))
                                                            (CAR EL))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ EL (CDR EL))
                                          (COND
                                           ((NULL EL) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (EL)
                                                      (CONS (CAR EL)
                                                            (EXPCHK (CDR EL))))
                                                    (CAR EL))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))))
                          NIL)
                         (SETQ I (PLUS2 I 1))
                         (GO LAB))
                       (RETURN NEW))))
            ((AND (FLAGP (CAAR U) 'MATMAPFN) (CDAR U)
                  (EQ (GETRTYPE (CADAR U)) 'MATRIX))
             (SETQ X (MATSM (MATRIXMAP (CAR U) NIL))))
            ((SETQ X (GET (CAAR U) 'PSOPFN))
             (PROGN
              (SETQ X (LISPAPPLY X (LIST (CDAR U))))
              (COND ((EQCAR X 'MAT) (SETQ X (MATSM X))))))
            ((AND (EQ (CAAR U) 'SPARSEMAT) (EQUAL (LENGTH U) 1))
             (PROGN (SETQ Z U) (GO C)))
            (T
             (PROGN
              (SETQ X (LISPAPPLY (CAAR U) (CDAR U)))
              (COND ((EQCAR X 'MAT) (SETQ X (MATSM X)))
                    ((EQCAR X 'SPARSEMAT) (SETQ X (SPMATSM X)))))))
     B
      (SETQ Z
              (COND ((NULL Z) X)
                    ((AND (NULL (CDR Z)) (NULL (CDAR Z))) (MULTSM (CAAR Z) X))
                    ((EQ (CAR X) 'SPARSEMAT) (SPMULTM Z (LIST X)))
                    (T (MULTM X Z))))
     C
      (SETQ U (CDR U))
      (GO A)
     D
      (COND
       ((EQUAL (CHECKSP (CADAR U)) 'SPARSE)
        (PROGN
         (SETQ Y (SPMATSM (CADAR U)))
         (SETQ LEN (CDAR (REVERSE Y)))
         (COND
          ((NOT (EQUAL (CAR LEN) (CADR LEN)))
           (RERROR 'MATRIX 4 "Non square sparse matrix")))))
       (T
        (PROGN
         (SETQ Y (MATSM (CADAR U)))
         (COND
          ((NEQ (SETQ N (LENGTH (CAR Y))) (LENGTH Y))
           (RERROR 'MATRIX 4 "Non square matrix"))
          ((AND Z (NEQ N (LENGTH Z))) (RERROR 'MATRIX 5 "Matrix mismatch"))
          ((CDDAR U) (GO H)) ((AND (NULL (CDR Y)) (NULL (CDAR Y))) (GO E))))))
      (SETQ X SUBFG*)
      (SETQ SUBFG* NIL)
      (COND ((NULL Z) (SETQ Z (APPLY1 (GET 'MAT 'INVERSEFN) Y)))
            ((EQUAL (CAAR Z) 'SPARSEMAT)
             (PROGN
              (SETQ Z (SPMULTM (CAR (APPLY1 (GET 'MAT 'INVERSEFN) Y)) Z))
              (SETQ U (CDR U))
              NIL))
            ((NULL (SETQ X (GET 'MAT 'LNRSOLVEFN)))
             (SETQ Z (MULTM (APPLY1 (GET 'MAT 'INVERSEFN) Y) Z)))
            (T (SETQ Z (APPLY2 (GET 'MAT 'LNRSOLVEFN) Y Z))))
      (SETQ SUBFG* X)
      (COND
       ((EQUAL (CAAR Z) 'SPARSEMAT)
        (PROGN
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (UPBV (CADR (CAR Z))) I)) (RETURN NIL)))
           ((LAMBDA (ROW)
              (PROGN
               (COND
                ((NOT (NULL ROW))
                 (PROGN
                  (PUTV (CADR (CAR Z)) I
                        (CONS (CAR ROW)
                              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL (CDR ROW))
                                (COND ((NULL EL) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL)
                                                    (PROGN
                                                     (SETQ *SUB2 T)
                                                     (CONS (CAR EL)
                                                           (MK*SQ
                                                            (SUBS2
                                                             (SIMP
                                                              (CDR EL)))))))
                                                  (CAR EL))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ EL (CDR EL))
                                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EL)
                                            (PROGN
                                             (SETQ *SUB2 T)
                                             (CONS (CAR EL)
                                                   (MK*SQ
                                                    (SUBS2 (SIMP (CDR EL)))))))
                                          (CAR EL))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))))))
            (GETV (CADR (CAR Z)) I))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         NIL))
       (T
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
                                        (COND
                                         ((NULL K) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (K)
                                                    (PROGN
                                                     (SETQ *SUB2 T)
                                                     (SUBS2 K)))
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
                  (GO LOOPLABEL)))))
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
(PUT 'MULTSM 'NUMBER-OF-ARGS 2) 
(PUT 'MULTSM 'DEFINED-ON-LINE '670) 
(PUT 'MULTSM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MULTSM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTSM (U V)
    (PROG ()
      (COND
       ((AND (NOT (EQUAL (LENGTH V) 1)) (EQUAL (CAR V) 'SPARSEMAT))
        (SETQ V (LIST V))))
      (COND ((EQUAL U (CONS 1 1)) (RETURN V))
            ((EQUAL (CAAR V) 'SPARSEMAT) (RETURN (SPMULTSM U (CAR V))))
            (T
             (RETURN
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
                                                          (MULTSQ U K))
                                                        (CAR K))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ K (CDR K))
                                      (COND ((NULL K) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (K) (MULTSQ U K))
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
                                               ((LAMBDA (K) (MULTSQ U K))
                                                (CAR K))
                                               NIL)))
                             LOOPLABEL
                              (SETQ K (CDR K))
                              (COND ((NULL K) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (K) (MULTSQ U K)) (CAR K))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'SPMULTSM 'NUMBER-OF-ARGS 2) 
(PUT 'SPMULTSM 'DEFINED-ON-LINE '682) 
(PUT 'SPMULTSM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMULTSM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPMULTSM (U V)
    (PROG (LEN TM ROW COL NEWVAL VAL ROWS)
      (SETQ LEN (CADDR V))
      (SETQ TM (MKEMPSPMAT (CADR LEN) LEN))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CADR LEN) I)) (RETURN NIL)))
        (PROGN
         (SETQ ROWS (FINDROW V I))
         (SETQ ROW I)
         (COND
          ((NOT (EQUAL ROWS NIL))
           (PROGN
            (PROG (COLS)
              (SETQ COLS (CDR ROWS))
             LAB
              (COND ((NULL COLS) (RETURN NIL)))
              ((LAMBDA (COLS)
                 (PROGN
                  (SETQ COL (CAR COLS))
                  (SETQ VAL (SIMP (CDR COLS)))
                  (SETQ NEWVAL (MULTSQ U VAL))
                  (SETQ NEWVAL (MK*SQ NEWVAL))
                  (COND
                   ((NOT (EQUAL NEWVAL 0))
                    (LETMTR3 (LIST TM ROW COL) NEWVAL TM NIL)))
                  NIL))
               (CAR COLS))
              (SETQ COLS (CDR COLS))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (LIST TM)))) 
(PUT 'ADDM 'NUMBER-OF-ARGS 2) 
(PUT 'ADDM 'DEFINED-ON-LINE '706) 
(PUT 'ADDM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'ADDM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDM (U V)
    (PROG (RES)
      (COND
       ((AND (NOT (EQUAL (LENGTH U) 1)) (EQUAL (CAR U) 'SPARSEMAT))
        (SETQ U (LIST U))))
      (COND
       ((AND (NOT (EQUAL (LENGTH V) 1)) (EQUAL (CAR V) 'SPARSEMAT))
        (SETQ V (LIST V))))
      (COND
       ((AND (EQUAL (CAAR U) 'SPARSEMAT) (EQUAL (CAAR V) 'SPARSEMAT))
        (SETQ RES (SMADDM (CAR U) (CAR V))))
       ((EQUAL V '(((NIL . 1)))) U)
       (T
        (SETQ RES
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
                           ((LAMBDA (J)
                              (ADDM1 (CAR J) (CDR J) (FUNCTION ADDSQ)))
                            (CAR J))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN RES))) 
(PUT 'ADDM1 'NUMBER-OF-ARGS 3) 
(PUT 'ADDM1 'DEFINED-ON-LINE '724) 
(PUT 'ADDM1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'ADDM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDM1 (U V W)
    (COND ((AND (NULL U) (NULL V)) NIL)
          ((OR (NULL U) (NULL V)) (RERROR 'MATRIX 8 "Matrix mismatch"))
          (T (CONS (APPLY2 W (CAR U) (CAR V)) (ADDM1 (CDR U) (CDR V) W))))) 
(PUT 'SMADDM 'NUMBER-OF-ARGS 2) 
(PUT 'SMADDM 'DEFINED-ON-LINE '731) 
(PUT 'SMADDM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SMADDM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SMADDM (U V)
    (PROG (LENA LENB LEN)
      (SETQ LEN (CADDR V))
      (SETQ LENA (CDR (CADDR U)))
      (SETQ LENB (CDR (CADDR V)))
      (COND ((NOT (EQUAL LENA LENB)) (RERROR 'MATRIX 8 "Matrix mismatch"))
            (T (RETURN (SMADDM2 U V LEN)))))) 
(PUT 'SMADDM2 'NUMBER-OF-ARGS 3) 
(PUT 'SMADDM2 'DEFINED-ON-LINE '743) 
(PUT 'SMADDM2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SMADDM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SMADDM2 (U V LENA)
    (PROG (TM ROWAS ROWBS ROWA ROWB ROWNA ROWNB VAL1 VAL2 J NEWVAL)
      (SETQ ROWAS U)
      (SETQ ROWBS V)
      (SETQ TM (SP-COPY-VECT ROWBS NIL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CADR LENA) I)) (RETURN NIL)))
        (PROGN
         (SETQ ROWA (FINDROW ROWAS I))
         (SETQ ROWNA I)
         (SETQ ROWB (FINDROW ROWBS I))
         (SETQ ROWNB I)
         (COND
          ((NOT (EQUAL ROWA NIL))
           (PROGN
            (PROG (XX)
              (SETQ XX (CDR ROWA))
             LAB
              (COND ((NULL XX) (RETURN NIL)))
              ((LAMBDA (XX)
                 (PROGN
                  (SETQ J (CAR XX))
                  (SETQ VAL1 (CDR XX))
                  (SETQ VAL2 (ATSOC J ROWB))
                  (COND
                   ((EQUAL VAL2 NIL)
                    (PROGN (LETMTR3 (LIST TM I J) VAL1 TM NIL)))
                   (T
                    (PROGN
                     (SETQ VAL2 (CDR VAL2))
                     (SETQ NEWVAL (ADDSQ (SIMP VAL1) (SIMP VAL2)))
                     (SETQ NEWVAL (MK*SQ NEWVAL))
                     (LETMTR3 (LIST TM I J) NEWVAL TM NIL)
                     NIL)))
                  NIL))
               (CAR XX))
              (SETQ XX (CDR XX))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN TM))) 
(PUT 'SMADDM1 'NUMBER-OF-ARGS 3) 
(PUT 'SMADDM1 'DEFINED-ON-LINE '774) 
(PUT 'SMADDM1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SMADDM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SMADDM1 (U V LENA)
    (PROG (TM ROWAS ROWBS ROWA ROWB COLA COLB COLAS COLBS COLS COL NEWVAL VALA
           VALB VAL COLNA COLNB ROWNA ROWNB)
      (SETQ TM (MKEMPSPMAT (CADR LENA) LENA))
      (SETQ ROWAS (CADR U))
      (SETQ ROWBS (CADR V))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CADR LENA) I)) (RETURN NIL)))
        (PROGN
         (SETQ ROWA (FINDROW ROWAS I))
         (SETQ ROWNA I)
         (SETQ ROWB (FINDROW ROWBS I))
         (SETQ ROWNB I)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (NOT (OR (EQUAL ROWA NIL) (EQUAL ROWB NIL)))) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL ROWNA ROWNB)
              (PROGN
               (SETQ COLAS (CDR ROWA))
               (SETQ COLBS (CDR ROWB))
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (NOT (OR (EQUAL COLAS NIL) (EQUAL COLBS NIL))))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ COLA (CAR COLAS))
                  (SETQ COLB (CAR COLBS))
                  (SETQ COLNA (CAR COLA))
                  (SETQ COLNB (CAR COLB))
                  (COND
                   ((EQUAL COLNA COLNB)
                    (PROGN
                     (SETQ VALA (SIMP (CDR COLA)))
                     (SETQ VALB (SIMP (CDR COLB)))
                     (SETQ NEWVAL (ADDSQ VALA VALB))
                     (SETQ NEWVAL (MK*SQ NEWVAL))
                     (COND
                      ((NOT (EQUAL NEWVAL 0))
                       (LETMTR3 (LIST TM ROWNA COLNA) NEWVAL TM NIL)))
                     (SETQ COLBS (CDR COLBS))
                     (SETQ COLAS (CDR COLAS))))
                   ((GREATERP COLNA COLNB)
                    (PROGN
                     (SETQ VALB (CDR COLB))
                     (COND
                      ((NOT (EQUAL VALB 0))
                       (LETMTR3 (LIST TM ROWNB COLNB) VALB TM NIL)))
                     (SETQ COLBS (CDR COLBS))))
                   (T
                    (PROGN
                     (SETQ VALA (CDR COLA))
                     (COND
                      ((NOT (EQUAL VALA 0))
                       (LETMTR3 (LIST TM ROWNA COLNA) VALA TM NIL)))
                     (SETQ COLAS (CDR COLAS)))))
                  NIL)
                 (GO WHILELABEL))
               (COND
                ((NOT (EQUAL COLAS NIL))
                 (PROGN
                  (PROG (X)
                    (SETQ X COLAS)
                   LAB
                    (COND ((NULL X) (RETURN NIL)))
                    ((LAMBDA (X)
                       (PROGN
                        (LETMTR3 (LIST TM ROWNA (CAR X)) (CDR X) TM NIL)))
                     (CAR X))
                    (SETQ X (CDR X))
                    (GO LAB))
                  NIL))
                ((NOT (EQUAL COLBS NIL))
                 (PROGN
                  (PROG (X)
                    (SETQ X COLBS)
                   LAB
                    (COND ((NULL X) (RETURN NIL)))
                    ((LAMBDA (X)
                       (PROGN
                        (LETMTR3 (LIST TM ROWNA (CAR X)) (CDR X) TM NIL)))
                     (CAR X))
                    (SETQ X (CDR X))
                    (GO LAB))
                  NIL)))
               (SETQ ROWA NIL)
               (SETQ ROWB NIL)
               NIL))
             ((GREATERP ROWNA ROWNB)
              (PROGN
               (PROG (COLS)
                 (SETQ COLS (CDR ROWB))
                LAB
                 (COND ((NULL COLS) (RETURN NIL)))
                 ((LAMBDA (COLS)
                    (PROGN
                     (SETQ COL (CAR COLS))
                     (SETQ VAL (CDR COLS))
                     (COND
                      ((NOT (EQUAL VAL 0))
                       (LETMTR3 (LIST TM ROWNB COL) VAL TM NIL)))))
                  (CAR COLS))
                 (SETQ COLS (CDR COLS))
                 (GO LAB))
               (SETQ ROWB NIL)
               NIL))
             (T
              (PROGN
               (PROG (COLS)
                 (SETQ COLS (CDR ROWA))
                LAB
                 (COND ((NULL COLS) (RETURN NIL)))
                 ((LAMBDA (COLS)
                    (PROGN
                     (SETQ COL (CAR COLS))
                     (SETQ VAL (CDR COLS))
                     (COND
                      ((NOT (EQUAL VAL 0))
                       (LETMTR3 (LIST TM ROWNA COL) VAL TM NIL)))))
                  (CAR COLS))
                 (SETQ COLS (CDR COLS))
                 (GO LAB))
               (SETQ ROWA NIL)
               NIL)))
            NIL)
           (GO WHILELABEL))
         (COND
          ((NOT (EQUAL ROWA NIL))
           (PROGN
            (PROG (COLS)
              (SETQ COLS (CDR ROWA))
             LAB
              (COND ((NULL COLS) (RETURN NIL)))
              ((LAMBDA (COLS)
                 (PROGN
                  (SETQ COL (CAR COLS))
                  (SETQ VAL (CDR COLS))
                  (COND
                   ((NOT (EQUAL VAL 0))
                    (LETMTR3 (LIST TM ROWNA COL) VAL TM NIL)))))
               (CAR COLS))
              (SETQ COLS (CDR COLS))
              (GO LAB))
            NIL))
          ((NOT (EQUAL ROWB NIL))
           (PROGN
            (PROG (COLS)
              (SETQ COLS (CDR ROWB))
             LAB
              (COND ((NULL COLS) (RETURN NIL)))
              ((LAMBDA (COLS)
                 (PROGN
                  (SETQ COL (CAR COLS))
                  (SETQ VAL (CDR COLS))
                  (COND
                   ((NOT (EQUAL VAL 0))
                    (LETMTR3 (LIST TM ROWNB COL) VAL TM NIL)))))
               (CAR COLS))
              (SETQ COLS (CDR COLS))
              (GO LAB))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN TM))) 
(PUT 'SPMULTM 'NUMBER-OF-ARGS 2) 
(PUT 'SPMULTM 'DEFINED-ON-LINE '873) 
(PUT 'SPMULTM 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMULTM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPMULTM (U V)
    (PROG (LENA LENB NLEN)
      (COND
       ((NOT (EQUAL (CDR V) NIL))
        (PROGN
         (SETQ V (LIST (SPMULTM (CAR V) (CDR V))))
         (RETURN (SPMULTM U V))))
       (T
        (PROGN
         (SETQ LENA (CADDR (CAR V)))
         (SETQ LENB (CADDR U))
         (SETQ NLEN (LIST 'SPM (CADR LENA) (CADDR LENB)))
         (COND
          ((NOT (EQUAL (CADDR LENA) (CADR LENB)))
           (RERROR 'MATRIX 8 "Matrix mismatch"))
          (T (RETURN (SPMULTM2 (CAR V) (SMTP U NIL) NLEN))))
         NIL))))) 
(PUT 'SPMULTM2 'NUMBER-OF-ARGS 3) 
(PUT 'SPMULTM2 'DEFINED-ON-LINE '889) 
(PUT 'SPMULTM2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMULTM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPMULTM2 (U V LEN)
    (PROG (TM ROWAS ROWBS ROWA ROWS VAL1 VAL2 NEWVAL SMNEWVAL JJ)
      (SETQ TM (MKEMPSPMAT (CADR LEN) LEN))
      (COND
       ((OR (NOT (SP-NOTEMPTY-P (CADR U) (CADR (CADDR U))))
            (NOT (SP-NOTEMPTY-P (CADR V) (CADR (CADDR V)))))
        (RETURN TM))
       (T
        (PROGN
         (SETQ ROWAS (CADR U))
         (SETQ ROWBS (CADR V))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (CADR (CADDR U)) I)) (RETURN NIL)))
           (PROGN
            (SETQ ROWA (FINDROW ROWAS I))
            (COND
             (ROWA
              (PROGN
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE (CADR (CADDR V)) J)) (RETURN NIL)))
                 (PROGN
                  (SETQ ROWS (FINDROW ROWBS J))
                  (COND
                   (ROWS
                    (PROGN
                     (SETQ SMNEWVAL (SIMP 0))
                     (PROG (XX)
                       (SETQ XX (CDR ROWA))
                      LAB
                       (COND ((NULL XX) (RETURN NIL)))
                       ((LAMBDA (XX)
                          (PROGN
                           (SETQ JJ (CAR XX))
                           (SETQ VAL1 (CDR XX))
                           (SETQ VAL2 (ATSOC JJ ROWS))
                           (COND
                            (VAL2
                             (PROGN
                              (SETQ VAL2 (CDR VAL2))
                              (SETQ NEWVAL (MULTSQ (SIMP VAL1) (SIMP VAL2)))
                              (SETQ SMNEWVAL (ADDSQ SMNEWVAL NEWVAL))
                              NIL))
                            (T (PROGN (SETQ SMNEWVAL SMNEWVAL))))
                           NIL))
                        (CAR XX))
                       (SETQ XX (CDR XX))
                       (GO LAB))
                     (SETQ NEWVAL (MK*SQ SMNEWVAL))
                     (COND
                      ((NOT (EQUAL NEWVAL 0))
                       (LETMTR3 (LIST TM I J) NEWVAL TM NIL)))
                     NIL)))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (RETURN TM)
         NIL))))) 
(PUT 'SPMULTM1 'NUMBER-OF-ARGS 3) 
(PUT 'SPMULTM1 'DEFINED-ON-LINE '927) 
(PUT 'SPMULTM1 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMULTM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPMULTM1 (U V LEN)
    (PROG (TM ROWAS ROWBS ROWA ROWNA ROWNB COLAS COLBS COLA COLB VALA VALB
           NEWVAL SMNEWVAL COLNA COLNB ROWS)
      (SETQ TM (MKEMPSPMAT (CADR LEN) LEN))
      (COND
       ((OR (NOT (SP-NOTEMPTY-P (CADR U) (CADR (CADDR U))))
            (NOT (SP-NOTEMPTY-P (CADR V) (CADR (CADDR V)))))
        (RETURN TM))
       (T
        (PROGN
         (SETQ ROWAS (CADR U))
         (SETQ ROWBS (CADR V))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (CADR (CADDR U)) I)) (RETURN NIL)))
           (PROGN
            (SETQ ROWA (FINDROW ROWAS I))
            (PROG ()
             WHILELABEL
              (COND ((NOT ROWA) (RETURN NIL)))
              (PROGN
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE (CADR (CADDR V)) J)) (RETURN NIL)))
                 (PROGN
                  (SETQ ROWS (FINDROW ROWBS J))
                  (COND
                   (ROWS
                    (PROGN
                     (SETQ ROWNA I)
                     (SETQ COLAS (CDR ROWA))
                     (SETQ ROWNB J)
                     (SETQ COLBS (CDR ROWS))
                     (SETQ SMNEWVAL (SIMP 0))
                     (PROG ()
                      WHILELABEL
                       (COND
                        ((NOT (NOT (OR (EQUAL COLAS NIL) (EQUAL COLBS NIL))))
                         (RETURN NIL)))
                       (PROGN
                        (SETQ COLA (CAR COLAS))
                        (SETQ COLB (CAR COLBS))
                        (SETQ COLNA (CAR COLA))
                        (SETQ COLNB (CAR COLB))
                        (COND
                         ((EQUAL COLNA COLNB)
                          (PROGN
                           (SETQ VALA (SIMP (CDR COLA)))
                           (SETQ VALB (SIMP (CDR COLB)))
                           (SETQ NEWVAL (MULTSQ VALA VALB))
                           (SETQ SMNEWVAL (ADDSQ SMNEWVAL NEWVAL))
                           (SETQ COLBS (CDR COLBS))
                           (SETQ COLAS (CDR COLAS))))
                         ((GREATERP COLNA COLNB)
                          (PROGN (SETQ COLBS (CDR COLBS))))
                         (T (PROGN (SETQ COLAS (CDR COLAS)))))
                        NIL)
                       (GO WHILELABEL))
                     (SETQ NEWVAL (MK*SQ SMNEWVAL))
                     (COND
                      ((NOT (EQUAL NEWVAL 0))
                       (LETMTR3 (LIST TM ROWNA ROWNB) NEWVAL TM NIL)))
                     NIL)))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               (SETQ ROWA NIL)
               NIL)
              (GO WHILELABEL))
            NIL)
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (RETURN TM)))))) 
(PUT 'CHECKSP 'NUMBER-OF-ARGS 1) 
(PUT 'CHECKSP 'DEFINED-ON-LINE '983) 
(PUT 'CHECKSP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'CHECKSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECKSP (U)
    (PROG (X SP M)
      (COND
       ((AND (ATOM U) (NOT (NUMBERP U)))
        (PROGN
         (SETQ X (GET U 'AVALUE))
         (COND ((NOT (EQUAL X NIL)) (SETQ X (CAR X))))))
       ((PAIRP U)
        (PROGN
         (COND ((EQUAL (CAR U) 'SPARSEMAT) (SETQ SP 'SPARSE))
               ((EQUAL (CAR U) 'MAT) (SETQ M 'MATRIX))
               (T
                (PROGN
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT U) (RETURN NIL)))
                   (PROGN
                    (COND
                     ((PAIRP (CAR U))
                      (PROGN
                       (COND ((EQUAL (CAR U) 'SPARSEMAT) (SETQ SP 'SPARSE))
                             ((EQUAL (CAR U) 'MAT) (SETQ M 'MATRIX))
                             (T
                              (PROGN
                               (COND ((NOT (PAIRP U)) (SETQ X NIL))
                                     (T (SETQ X (LIST (CHECKSP (CAR U))))))
                               (COND ((NOT (EQUAL X NIL)) (SETQ X (CAR X))))
                               (COND ((EQUAL X 'SPARSE) (SETQ SP 'SPARSE))
                                     ((EQUAL X 'MATRIX) (SETQ M 'MATRIX)))
                               NIL)))
                       (SETQ U (CDR U))
                       (COND ((NOT (PAIRP U)) (SETQ U NIL)))
                       NIL))
                     (T
                      (PROGN
                       (SETQ X (GET (CAR U) 'AVALUE))
                       (COND ((NOT (EQUAL X NIL)) (SETQ X (CAR X))))
                       (COND
                        ((EQUAL X 'SPARSE)
                         (PROGN (SETQ SP 'SPARSE) (SETQ U (CDR U))))
                        ((EQUAL X 'MATRIX)
                         (PROGN (SETQ M 'MATRIX) (SETQ U (CDR U))))
                        (T (SETQ U (CDR U))))
                       (COND ((NOT (PAIRP U)) (SETQ U NIL)))
                       NIL)))
                    NIL)
                   (GO WHILELABEL))
                 NIL)))
         (COND ((AND SP (NOT M)) (SETQ X SP)) ((AND M (NOT SP)) (SETQ X M))
               (T (SETQ X (CONS SP M))))
         NIL))
       (T (SETQ X NIL)))
      (RETURN X))) 
(PUT 'SPRMCOL 'NUMBER-OF-ARGS 2) 
(PUT 'SPRMCOL 'DEFINED-ON-LINE '1028) 
(PUT 'SPRMCOL 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPRMCOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPRMCOL (NUM LIST)
    (PROG (ROW ROE RLIST NEWLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT LIST) (RETURN NIL)))
        (PROGN
         (SETQ ROW (CAR LIST))
         (SETQ ROE (CDR ROW))
         (SETQ RLIST (CONS (CAR ROW) RLIST))
         (PROG ()
          WHILELABEL
           (COND ((NOT ROE) (RETURN NIL)))
           (PROGN
            (COND ((EQUAL NUM (CAAR ROE)) (SETQ ROE (CDR ROE)))
                  (T
                   (PROGN
                    (SETQ RLIST (CONS (CAR ROE) RLIST))
                    (SETQ ROE (CDR ROE)))))
            NIL)
           (GO WHILELABEL))
         (SETQ LIST (CDR LIST))
         (SETQ NEWLIST (CONS (REVERSE RLIST) NEWLIST))
         (SETQ RLIST NIL)
         NIL)
        (GO WHILELABEL))
      (RETURN (REVERSE NEWLIST)))) 
(PUT 'SIMPDET 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDET 'DEFINED-ON-LINE '1052) 
(PUT 'SIMPDET 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SIMPDET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDET (U)
    (PROG (X Y)
      (SETQ Y (SPMATSM (CARX U 'DET)))
      (RETURN
       (COND ((EQUAL (CHECKSP Y) 'SPARSE) (SPDET Y))
             ((OR *CRAMER *ROUNDED
                  ((LAMBDA (*PROTFG)
                     (ERRORP
                      (SETQ X
                              (ERRORSET (LIST 'BAREISS-DET (MKQUOTE U)) NIL
                                        NIL))))
                   T))
              (DETQ Y))
             (T (CAR X)))))) 
(PUT 'SPDET 'NUMBER-OF-ARGS 1) 
(PUT 'SPDET 'DEFINED-ON-LINE '1065) 
(PUT 'SPDET 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPDET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPDET (U)
    (PROG (LEN LENA LENB LLIST ANS)
      (SETQ LEN (CDR (CADDR U)))
      (SETQ LENA (CAR LEN))
      (SETQ LENB (CADR LEN))
      (SETQ LLIST (CADR U))
      (COND ((NOT (EQUAL LENA LENB)) (REDERR "Non square sparse matrix"))
            ((EQUAL LENA 2) (RETURN (TWODET LLIST)))
            ((EQUAL LENA 1) (RETURN (FINDELEM2 LLIST 1 1)))
            (T
             (GREATERP
              (PROGN
               (MATRIX_CLRHASH)
               (SETQ ANS (NSIMPDET LLIST LENA LENA 0 1))
               (MATRIX_CLRHASH))
              (RETURN ANS)))))) 
(PUT 'NSIMPDET 'NUMBER-OF-ARGS 5) 
(PUT 'NSIMPDET 'DEFINED-ON-LINE '1081) 
(PUT 'NSIMPDET 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'NSIMPDET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NSIMPDET (U SIZE LEN IGNNUM FIRSTROW)
    (PROG (N2 I ROW SIGN Z X)
      (SETQ N2 0)
      (SETQ I 0)
      (SETQ ROW (GETV U FIRSTROW))
      (COND ((NOT (NULL ROW)) (SETQ ROW (CDR ROW))))
      (SETQ N2 1)
      (COND
       ((EQUAL LEN 1)
        (RETURN
         (PROGN
          (SETQ I 1)
          (PROG ()
           WHILELABEL
            (COND ((NOT (TWOMEM N2 IGNNUM)) (RETURN NIL)))
            (PROGN (SETQ N2 (TIMES 2 N2)) (SETQ I (PLUS I 1)))
            (GO WHILELABEL))
          (COND ((NULL (SETQ X (ATSOC I ROW))) (CONS NIL 1))
                (T (SIMP (CDR X))))))))
      (COND ((SETQ Z (MATRIX_GETHASH IGNNUM)) (RETURN (CDR Z))))
      (SETQ LEN (DIFFERENCE LEN 1))
      (SETQ Z (CONS NIL 1))
      (PROG (COLINDEX)
        (SETQ COLINDEX 1)
       LAB
        (COND ((MINUSP (DIFFERENCE SIZE COLINDEX)) (RETURN NIL)))
        (PROGN
         (SETQ X (ATSOC COLINDEX ROW))
         (COND
          ((NOT (TWOMEM N2 IGNNUM))
           (PROGN
            (COND
             ((AND X (CAR (SETQ X (SIMP (CDR X)))))
              (PROGN
               (COND (SIGN (SETQ X (NEGSQ X))))
               (SETQ Z
                       (ADDSQ
                        (MULTSQ X
                                (NSIMPDET U SIZE LEN (PLUS N2 IGNNUM)
                                 (PLUS FIRSTROW 1)))
                        Z)))))
            (SETQ SIGN (NOT SIGN)))))
         (SETQ N2 (TIMES 2 N2)))
        (SETQ COLINDEX (PLUS2 COLINDEX 1))
        (GO LAB))
      (MATRIX_PUTHASH IGNNUM Z)
      (RETURN Z))) 
(PUT 'TWODET 'NUMBER-OF-ARGS 1) 
(PUT 'TWODET 'DEFINED-ON-LINE '1119) 
(PUT 'TWODET 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'TWODET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TWODET (LIST)
    (PROG (VAL1 VAL2 RES)
      (SETQ VAL1
              (MULTSQ (SIMP (FINDELEM2 LIST 1 1)) (SIMP (FINDELEM2 LIST 2 2))))
      (SETQ VAL2
              (MULTSQ (SIMP (FINDELEM2 LIST 2 1)) (SIMP (FINDELEM2 LIST 1 2))))
      (SETQ RES (ADDSQ VAL1 (NEGSQ VAL2)))
      (RETURN RES))) 
(PUT 'SPAUGMENT 'NUMBER-OF-ARGS 2) 
(PUT 'SPAUGMENT 'DEFINED-ON-LINE '1132) 
(PUT 'SPAUGMENT 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPAUGMENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPAUGMENT (LIST LEN)
    (PROG ()
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
        (PROGN (LETMTR3 (LIST LIST I (PLUS I LEN)) 1 LIST NIL))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN LIST))) 
(PUT 'SWAP 'NUMBER-OF-ARGS 3) 
(PUT 'SWAP 'DEFINED-ON-LINE '1148) 
(PUT 'SWAP 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SWAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SWAP (ROW REST I)
    (PROG (ROWB LEN)
      (SETQ LEN (CADR (CADDR REST)))
      (COND ((EQUAL I LEN) (RERROR 'MATRIX 13 "Singular Matrix")))
      (SETQ ROWB (FINDROW REST (PLUS I 1)))
      (COND
       ((EQUAL I (CAAR (CDR ROWB)))
        (PROGN
         (LETMTR3 (LIST REST I) ROWB REST NIL)
         (LETMTR3 (LIST REST (PLUS I 1)) ROW REST NIL)
         NIL))
       (T (PROGN (SWAP ROWB REST (PLUS I 1)))))
      (RETURN REST))) 
(PUT 'SPGAUSS 'NUMBER-OF-ARGS 2) 
(PUT 'SPGAUSS 'DEFINED-ON-LINE '1161) 
(PUT 'SPGAUSS 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPGAUSS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPGAUSS (LIST LEN)
    (PROG (ROWS NROW FROW ROW COLS PIV PLIST NDROW DROW MVAL CLIST ROWN RCNT
           CCNT ROWLIST)
      (SETQ ROWS (SPAUGMENT LIST LEN))
      (SETQ RCNT 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (CADR (CADDR LIST)) I)) (RETURN NIL)))
        (PROGN
         (SETQ FROW (FINDROW ROWS I))
         (COND
          ((NOT (EQUAL FROW NIL))
           (PROGN
            (SETQ ROW I)
            (SETQ PIV 0)
            (COND
             ((NOT (EQUAL ROW (PLUS RCNT 1)))
              (RERROR 'MATRIX 13 "Singular Matrix")))
            (PROG ()
             WHILELABEL
              (COND ((NOT (EQUAL PIV 0)) (RETURN NIL)))
              (PROGN
               (SETQ COLS (CDR FROW))
               (COND
                ((EQUAL (CAAR COLS) ROW)
                 (PROGN
                  (SETQ PIV (SIMP (CDAR COLS)))
                  (SETQ PIV (CONS (CDR PIV) (CAR PIV)))))
                (T
                 (PROGN
                  (SETQ ROWLIST (SWAP FROW ROWS I))
                  (SETQ FROW (FINDROW ROWLIST I))
                  NIL)))
               NIL)
              (GO WHILELABEL))
            (COND
             ((NOT (EQUAL PIV (SIMP 1)))
              (PROGN
               (SETQ FROW
                       (CONS (LIST NIL)
                             (PROG (XX FORALL-RESULT FORALL-ENDPTR)
                               (SETQ XX (CDR FROW))
                               (COND ((NULL XX) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (XX)
                                                   (CONS (CAR XX)
                                                         (MK*SQ
                                                          (MULTSQ PIV
                                                                  (SIMP
                                                                   (CDR
                                                                    XX))))))
                                                 (CAR XX))
                                                NIL)))
                              LOOPLABEL
                               (SETQ XX (CDR XX))
                               (COND ((NULL XX) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (XX)
                                           (CONS (CAR XX)
                                                 (MK*SQ
                                                  (MULTSQ PIV
                                                          (SIMP (CDR XX))))))
                                         (CAR XX))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))
               NIL)))
            (LETMTR3 (LIST ROWS I) FROW ROWS NIL)
            (PROG (J)
              (SETQ J (PLUS I 1))
             LAB
              (COND ((MINUSP (DIFFERENCE (CADR (CADDR LIST)) J)) (RETURN NIL)))
              (PROGN
               (SETQ DROW (FINDROW ROWS J))
               (COND
                (DROW
                 (PROGN
                  (SETQ ROWN J)
                  (SETQ CCNT (CAAR (CDR DROW)))
                  (COND
                   ((EQUAL CCNT ROW)
                    (PROGN
                     (SETQ MVAL (SIMP (CDADR DROW)))
                     (COND ((EQUAL MVAL (CONS NIL 1)) (SETQ MVAL MVAL))
                           (T
                            (SETQ MVAL (CONS (MINUS (CAR MVAL)) (CDR MVAL)))))
                     NIL))
                   (T (SETQ MVAL (SIMP 0))))
                  (SETQ CLIST (MKEMPSPMAT 1 (LIST 'SPM 1 1)))
                  (SETQ PLIST (MKEMPSPMAT 1 (LIST 'SPM 1 1)))
                  (LETMTR3 (LIST CLIST 1) DROW CLIST NIL)
                  (COND ((EQUAL MVAL (SIMP 0)) (SETQ NDROW CLIST))
                        (T
                         (PROGN
                          (SETQ NROW
                                  (CONS (LIST NIL)
                                        (PROG (XX FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ XX (CDR FROW))
                                          (COND ((NULL XX) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (XX)
                                                              (CONS (CAR XX)
                                                                    (MK*SQ
                                                                     (MULTSQ
                                                                      MVAL
                                                                      (SIMP
                                                                       (CDR
                                                                        XX))))))
                                                            (CAR XX))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ XX (CDR XX))
                                          (COND
                                           ((NULL XX) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (XX)
                                                      (CONS (CAR XX)
                                                            (MK*SQ
                                                             (MULTSQ MVAL
                                                                     (SIMP
                                                                      (CDR
                                                                       XX))))))
                                                    (CAR XX))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))
                          (LETMTR3 (LIST PLIST 1) NROW PLIST NIL)
                          (SETQ NDROW
                                  (SMADDM2 CLIST PLIST
                                   (LIST 'SPM 1 (CADDR (CADDR LIST)))))
                          (SETQ NDROW (FINDROW (CADR NDROW) 1))
                          (COND
                           ((EQUAL NDROW NIL)
                            (RERROR 'MATRIX 13 "Singular Matrix")))
                          (LETMTR3 (LIST ROWS J) NDROW ROWS NIL)
                          NIL)))
                  NIL)))
               NIL)
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (SETQ RCNT (PLUS RCNT 1))
            NIL))
          (T (PROGN (SETQ RCNT (PLUS RCNT 1)))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SPBACK_SUB ROWS LEN)
      (RETURN ROWS))) 
(PUT 'SUMSOL 'NUMBER-OF-ARGS 2) 
(PUT 'SUMSOL 'DEFINED-ON-LINE '1232) 
(PUT 'SUMSOL 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SUMSOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUMSOL (LIST LEN)
    (PROG (CLIST ROW)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
        (PROGN
         (SETQ ROW (FINDROW LIST I))
         (COND
          ((NOT (EQUAL ROW NIL))
           (PROGN
            (SETQ CLIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X ROW)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (CONS (DIFFERENCE (CAR X) LEN)
                                                (CDR X)))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS (DIFFERENCE (CAR X) LEN) (CDR X)))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (LETMTR3 (LIST LIST I) (CONS (LIST NIL) CLIST) LIST NIL)
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB)))) 
(PUT 'SUMSOL2 'NUMBER-OF-ARGS 4) 
(PUT 'SUMSOL2 'DEFINED-ON-LINE '1246) 
(PUT 'SUMSOL2 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SUMSOL2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUMSOL2 (ROWS ROW LISTB LEN)
    (PROG (RCNT COL VAL SUM RLIST LENA ELIST LLIST LIST MVAL)
      (SETQ RCNT ROW)
      (SETQ LISTB (CDR LISTB))
      (SETQ ELIST (CDR LISTB))
      (SETQ SUM 0)
      (SETQ LENA (PLUS LEN 1))
      (COND ((EQUAL ROW LEN) (RETURN ELIST)))
      (PROG (I)
        (SETQ I LENA)
       LAB
        (COND ((MINUSP (DIFFERENCE (PLUS (TIMES 2 LEN) 1) I)) (RETURN NIL)))
        (PROGN
         (SETQ SUM (SIMP 0))
         (PROG (XX)
           (SETQ XX ELIST)
          LAB
           (COND ((NULL XX) (RETURN NIL)))
           ((LAMBDA (XX)
              (PROGN
               (SETQ VAL (SIMP (CDR XX)))
               (SETQ COL (CAR XX))
               (COND
                ((LESSP COL (PLUS LEN 1))
                 (PROGN
                  (SETQ MVAL (FINDELEM2 ROWS COL LENA))
                  (COND ((EQUAL MVAL 0) (SETQ SUM SUM))
                        (T (SETQ SUM (ADDSQ SUM (MULTSQ VAL (SIMP MVAL))))))
                  NIL)))
               NIL))
            (CAR XX))
           (SETQ XX (CDR XX))
           (GO LAB))
         (SETQ LIST (ATSOC LENA ELIST))
         (SETQ LLIST (SOL LIST SUM LENA))
         (COND ((NOT (EQUAL LLIST NIL)) (SETQ RLIST (CONS LLIST RLIST)))
               (T (SETQ RLIST RLIST)))
         (SETQ RCNT ROW)
         (SETQ LENA (PLUS LENA 1))
         (SETQ ELIST (CDR LISTB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (REVERSE RLIST)))) 
(PUT 'SOL 'NUMBER-OF-ARGS 3) 
(PUT 'SOL 'DEFINED-ON-LINE '1279) 
(PUT 'SOL 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOL (LIST SUM CCNT)
    (PROG (COL VAL NVAL NLIST)
      (COND ((EQUAL LIST NIL) (PROGN (SETQ COL CCNT) (SETQ VAL (SIMP 0))))
            (T (PROGN (SETQ COL (CAR LIST)) (SETQ VAL (SIMP (CDR LIST))))))
      (COND ((EQUAL CCNT COL) (SETQ VAL VAL)) (T (SETQ VAL (SIMP 0))))
      (COND ((EQUAL SUM (SIMP 0)) (SETQ NVAL (MK*SQ VAL)))
            (T (SETQ NVAL (MK*SQ (ADDSQ VAL (NEGSQ SUM))))))
      (COND ((NOT (EQUAL NVAL 0)) (SETQ NLIST (CONS CCNT NVAL))))
      (RETURN NLIST))) 
(PUT 'SPBACK_SUB 'NUMBER-OF-ARGS 2) 
(PUT 'SPBACK_SUB 'DEFINED-ON-LINE '1293) 
(PUT 'SPBACK_SUB 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPBACK_SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPBACK_SUB (LIST LEN)
    (PROG (ILIST LROW RCNT)
      (SETQ RCNT 0)
      (PROG (I)
        (SETQ I LEN)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 I))) (RETURN NIL)))
        (PROGN
         (SETQ LROW (FINDROW (CADR LIST) I))
         (COND
          ((NOT (EQUAL LROW NIL))
           (PROGN
            (SETQ ILIST (SUMSOL2 LIST I LROW LEN))
            (LETMTR3 (LIST LIST I) ILIST LIST NIL)
            NIL)))
         NIL)
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (SUMSOL LIST LEN)
      (RETURN LIST))) 
(PUT 'SPMATINVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'SPMATINVERSE 'DEFINED-ON-LINE '1309) 
(PUT 'SPMATINVERSE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPMATINVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPMATINVERSE (LIST)
    (PROG (ROWS LEN)
      (SETQ LEN (CADDR LIST))
      (SETQ ROWS (MKEMPSPMAT (CADR LEN) LEN))
      (SETQ ROWS (SP-COPY-VECT LIST NIL))
      (SETQ ROWS (SPGAUSS ROWS (CADR LEN)))
      (RETURN (LIST ROWS)))) 
(PUT 'MATINVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'MATINVERSE 'DEFINED-ON-LINE '1321) 
(PUT 'MATINVERSE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'MATINVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATINVERSE (U)
    (COND ((EQUAL (CAR U) 'SPARSEMAT) (SPMATINVERSE U))
          (T (LNRSOLVE U (GENERATEIDENT (LENGTH U)))))) 
(PUT 'SIMPTRACE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPTRACE 'DEFINED-ON-LINE '1330) 
(PUT 'SIMPTRACE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SIMPTRACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPTRACE (U)
    (PROG (N Z)
      (SETQ N 0)
      (COND ((EQUAL (CHECKSP U) 'SPARSE) (SETQ Z (SPTRACE (SPMATSM (CAR U)))))
            (T
             (PROGN
              (SETQ U (SPMATSM (CARX U 'TRACE)))
              (COND
               ((NEQ (LENGTH U) (LENGTH (CAR U)))
                (REDERR "Non square matrix")))
              (SETQ N 1)
              (SETQ Z (CONS NIL 1))
              (PROG (X)
                (SETQ X U)
               LAB
                (COND ((NULL X) (RETURN NIL)))
                ((LAMBDA (X)
                   (PROGN (SETQ Z (ADDSQ (NTH X N) Z)) (SETQ N (PLUS N 1))))
                 (CAR X))
                (SETQ X (CDR X))
                (GO LAB))
              NIL)))
      (RETURN Z))) 
(PUT 'SPTRACE 'NUMBER-OF-ARGS 1) 
(PUT 'SPTRACE 'DEFINED-ON-LINE '1345) 
(PUT 'SPTRACE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPTRACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPTRACE (LIST)
    (PROG (VAL SUM RLIST LEN)
      (SETQ LEN (CADAR (REVERSE LIST)))
      (SETQ RLIST (CADR LIST))
      (SETQ SUM (SIMP 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
        (PROGN
         (SETQ VAL (SIMP (FINDELEM2 RLIST I I)))
         (SETQ SUM (ADDSQ SUM VAL))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN SUM))) 
(PUT 'SIMPCOFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPCOFACTOR 'DEFINED-ON-LINE '1365) 
(PUT 'SIMPCOFACTOR 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SIMPCOFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPCOFACTOR (U)
    (COND
     ((EQUAL (CHECKSP (CAR U)) 'SPARSE)
      (SPCOFACTOR (SPMATSM (CAR U)) (IEVAL (CADR U))
       (IEVAL (CARX (CDDR U) 'COFACTOR))))
     (T
      (COFACTORQ (SPMATSM (CAR U)) (IEVAL (CADR U))
                 (IEVAL (CARX (CDDR U) 'COFACTOR)))))) 
(PUT 'SPREMCOL 'NUMBER-OF-ARGS 2) 
(PUT 'SPREMCOL 'DEFINED-ON-LINE '1373) 
(PUT 'SPREMCOL 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPREMCOL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPREMCOL (NUM LIST)
    (PROG (ROW COL LEN)
      (SETQ LEN (CADR (CADDR LIST)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN I)) (RETURN NIL)))
        (PROGN
         (SETQ ROW (FINDROW LIST I))
         (COND
          ((NOT (EQUAL ROW NIL))
           (PROGN
            (SETQ COL (ATSOC NUM ROW))
            (COND
             (COL
              (PROGN
               (SETQ ROW (DELETE COL ROW))
               (LETMTR3 (LIST LIST I) ROW LIST NIL))))
            NIL)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB)))) 
(PUT 'SPREMROW 'NUMBER-OF-ARGS 2) 
(PUT 'SPREMROW 'DEFINED-ON-LINE '1389) 
(PUT 'SPREMROW 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPREMROW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPREMROW (NUM LIST) (PROG () (LETMTR3 (LIST LIST NUM) NIL LIST NIL))) 
(PUT 'SPCOFACTOR 'NUMBER-OF-ARGS 3) 
(PUT 'SPCOFACTOR 'DEFINED-ON-LINE '1396) 
(PUT 'SPCOFACTOR 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'SPCOFACTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPCOFACTOR (LIST ROW COL)
    (PROG (LEN LENA LENB RLIST RES)
      (SETQ LEN (CADDR LIST))
      (SETQ RLIST (SP-COPY-VECT LIST LEN))
      (SETQ LENA (CADR LEN))
      (SETQ LENB (CADDR LEN))
      (COND
       ((NOT (AND (GREATERP ROW 0) (LESSP ROW (PLUS LENA 1))))
        (RERROR 'MATRIX 20 "Row number out of range")))
      (COND
       ((NOT (AND (GREATERP COL 0) (LESSP COL (PLUS LENA 1))))
        (RERROR 'MATRIX 21 "Column number out of range")))
      (COND ((NOT (EQUAL LENA LENB)) (RERROR 'MATRIX 22 "non-square matrix")))
      (SPREMROW ROW RLIST)
      (SPREMCOL COL RLIST)
      (COND ((EQUAL RLIST NIL) (SETQ RES (SIMP NIL)))
            (T
             (PROGN
              (REWRITE RLIST (DIFFERENCE LENA 1) ROW COL)
              (MATRIX_CLRHASH)
              (SETQ RES
                      (NSIMPDET RLIST (DIFFERENCE LENA 1) (DIFFERENCE LENA 1) 0
                       1))
              (MATRIX_CLRHASH)
              (COND
               ((EQUAL (REMAINDER (PLUS ROW COL) 2) 1) (SETQ RES (NEGSQ RES))))
              NIL)))
      (RETURN RES))) 
(PUT 'REWRITE 'NUMBER-OF-ARGS 4) 
(PUT 'REWRITE 'DEFINED-ON-LINE '1427) 
(PUT 'REWRITE 'DEFINED-IN-FILE 'SPARSE/SPARSMAT.RED) 
(PUT 'REWRITE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REWRITE (LIST LEN ROW COL)
    (PROG (RCNT CCNT ROWS COLS COLA COLN RLIST CNT VAL ROWN RRCNT LENG UNT)
      (SETQ RCNT 1)
      (SETQ RRCNT 1)
      (SETQ LENG (CADDR LIST))
      (COND ((EQUAL (CADR LENG) (CADDR LENG)) (SETQ UNT (PLUS LEN 1)))
            (T (SETQ UNT LEN)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE UNT I)) (RETURN NIL)))
        (PROGN
         (SETQ ROWS (FINDROW LIST I))
         (COND
          ((NOT (EQUAL ROWS NIL))
           (PROGN
            (SETQ COLS (CDR ROWS))
            (SETQ ROWN I)
            (COND ((EQUAL RCNT ROW) (SETQ RCNT (PLUS RCNT 1))))
            (COND
             ((EQUAL ROWN RCNT)
              (PROGN
               (SETQ CNT 1)
               (SETQ CCNT 1)
               (SETQ RLIST NIL)
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND COLS (NOT (EQUAL CNT (PLUS LEN 1)))))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ COLA (CAR COLS))
                  (SETQ COLN (CAR COLA))
                  (SETQ VAL (CDR COLA))
                  (COND ((EQUAL CNT COL) (SETQ CCNT (PLUS CCNT 1))))
                  (COND
                   ((EQUAL COLN CCNT)
                    (PROGN
                     (SETQ RLIST (CONS (CONS CNT VAL) RLIST))
                     (SETQ CNT (PLUS CNT 1))
                     (SETQ COLS (CDR COLS))
                     (SETQ CCNT (PLUS CCNT 1))))
                   (T
                    (PROGN (SETQ CCNT (PLUS CCNT 1)) (SETQ CNT (PLUS CNT 1)))))
                  NIL)
                 (GO WHILELABEL))
               (LETMTR3 (LIST LIST RRCNT) (CONS (LIST NIL) (REVERSE RLIST))
                LIST NIL)
               (SETQ RRCNT (PLUS RRCNT 1))
               (SETQ RCNT (PLUS RCNT 1))
               NIL))
             (T (PROGN (SETQ RCNT (PLUS RCNT 1)) (SETQ RRCNT (PLUS RRCNT 1)))))
            NIL))
          (T (SETQ RCNT (PLUS RCNT 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((EQUAL (PLUS LEN 1) (CADR (CADDR LIST)))
        (LETMTR3 (LIST LIST (PLUS LEN 1)) NIL LIST NIL)))
      (RETURN LIST))) 
(ENDMODULE) 