(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATREXT)) 
(PUT 'NATNUMLIS 'NUMBER-OF-ARGS 1) 
(PUT 'NATNUMLIS 'DEFINED-ON-LINE '31) 
(PUT 'NATNUMLIS 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'NATNUMLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NATNUMLIS (U)
    (OR (NULL U)
        (AND (NUMBERP (CAR U)) (FIXP (CAR U)) (GREATERP (CAR U) 0)
             (NATNUMLIS (CDR U))))) 
(PUT 'MKIDM 'NUMBER-OF-ARGS 2) 
(PUT 'MKIDM 'DEFINED-ON-LINE '37) 
(PUT 'MKIDM 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'MKIDM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKIDM (U J) (MKID U J)) 
(FLAG '(MKIDM) 'OPFN) 
(FLAG '(MKIDM) 'NOVAL) 
(PUT 'BAGLMAT 'NUMBER-OF-ARGS 2) 
(PUT 'BAGLMAT 'DEFINED-ON-LINE '46) 
(PUT 'BAGLMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'BAGLMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BAGLMAT (U OP)
    (COND ((GETRTYPE OP) (REDERR (LIST OP "should be an identifier")))
          (T
           (PROG (X Y)
             (COND
              ((ATOM OP)
               (COND ((NOT (SETQ Y (GETTYPE OP))) (PUT OP 'RTYPE 'MATRIX))
                     (T (TYPERR (LIST Y OP) "matrix")))))
             (COND
              ((NEQ (RDEPTH (LIST U)) 2)
               (REDERR "depth of list or bag must be 2")))
             (SETQ X (CDR U))
             (SETQ X
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J X)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J)
                                           (PROG (K FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ K (CDR J))
                                             (COND ((NULL K) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (K) K)
                                                               (CAR K))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ K (CDR K))
                                             (COND
                                              ((NULL K)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) K) (CAR K))
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
                                     (SETQ K (CDR J))
                                     (COND ((NULL K) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) K) (CAR K))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ K (CDR K))
                                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS ((LAMBDA (K) K) (CAR K))
                                                   NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR J))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (PUT OP 'AVALUE (LIST 'MATRIX (CONS 'MAT X)))
             (RETURN T))))) 
(FLAG '(BAGLMAT) 'OPFN) 
(PUT 'RCOERCEMAT 'NUMBER-OF-ARGS 1) 
(PUT 'RCOERCEMAT 'DEFINED-ON-LINE '63) 
(PUT 'RCOERCEMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'RCOERCEMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RCOERCEMAT (U)
    (PROG (X PRF)
      (SETQ X (REVAL1 (CAR U) T))
      (COND
       ((NEQ (GETRTYPE X) 'MATRIX) (REDERR (LIST X "should be a matrix"))))
      (SETQ PRF (CADR U))
      (COND ((NEQ (CAR X) 'MAT) (TYPERR X "matrix"))
            ((NEQ PRF 'LIST)
             (PROGN (SETQ PRF (REVAL1 PRF T)) (SIMPBAGPROP (LIST PRF T)))))
      (SETQ X (CDR X))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CONS PRF J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CONS PRF J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS PRF X)))) 
(PUT 'COERCEMAT 'PSOPFN 'RCOERCEMAT) 
(PUT 'RCOERCEMAT 'NUMBER_OF_ARGS 2) 
(PUT 'N-1ZERO 'NUMBER-OF-ARGS 2) 
(PUT 'N-1ZERO 'DEFINED-ON-LINE '82) 
(PUT 'N-1ZERO 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'N-1ZERO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N-1ZERO (N K)
    (COND ((EQUAL N 0) NIL) ((EQUAL K 1) (CONS 1 (NZERO (DIFFERENCE N 1))))
          ((EQUAL K N) (APPEND (NZERO (DIFFERENCE N 1)) (CONS 1 NIL)))
          (T
           (APPEND (NZERO (DIFFERENCE K 1))
                   (CONS 1 (NZERO (DIFFERENCE N K))))))) 
(PUT 'UNITMAT 'NUMBER-OF-ARGS 1) 
(PUT 'UNITMAT 'DEFINED-ON-LINE '88) 
(PUT 'UNITMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'UNITMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNITMAT (U)
    (PROG (L SY X AA)
      (PROG (S)
        (SETQ S U)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (COND
             ((OR (ATOM S) (NEQ (LENGTH (SETQ L (REVLIS (CDR S)))) 1)
                  (NOT (NATNUMLIS L)))
              (ERRPRI2 S 'HOLD))
             (T
              (PROGN
               (SETQ AA NIL)
               (SETQ SY (CAR S))
               (SETQ X (GETTYPE SY))
               (COND
                ((NOT (NULL X))
                 (COND ((EQ X 'MATRIX) (LPRIM (LIST X SY "redefined")))
                       (T (TYPERR (LIST X SY) "matrix")))))
               (SETQ L (CAR L))
               (PROG (N)
                 (SETQ N 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE L N)) (RETURN NIL)))
                 (SETQ AA (CONS (N-1ZERO L (PLUS (DIFFERENCE L N) 1)) AA))
                 (SETQ N (PLUS2 N 1))
                 (GO LAB))
               (PUT SY 'RTYPE 'MATRIX)
               (PUT SY 'AVALUE (LIST 'MATRIX (CONS 'MAT AA))))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB)))) 
(PUT 'UNITMAT 'STAT 'RLIS) 
(PUT 'SUBMAT 'NUMBER-OF-ARGS 3) 
(PUT 'SUBMAT 'DEFINED-ON-LINE '104) 
(PUT 'SUBMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'SUBMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBMAT (U NL NC)
    (COND ((NEQ (GETRTYPE U) 'MATRIX) (REDERR (LIST U "should be a matrix")))
          (T
           (PROG (X)
             (SETQ X (MATSM U))
             (COND ((AND (EQUAL NL 0) (EQUAL NC 0)) (RETURN X))
                   ((NEQ NL 0) (SETQ X (REMOVE X NL))))
             (COND
              ((NEQ NC 0)
               (SETQ X
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J X)
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (REMOVE J NC)) (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (REMOVE J NC)) (CAR J))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
             (RETURN X))))) 
(PUT 'SUBMAT 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(SUBMAT) 'MATFLG) 
(PUT 'MATSUBR 'NUMBER-OF-ARGS 3) 
(PUT 'MATSUBR 'DEFINED-ON-LINE '122) 
(PUT 'MATSUBR 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'MATSUBR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MATSUBR (M BGL NR)
    (COND ((NEQ (GETRTYPE M) 'MATRIX) (REDERR (LIST M "should be a matrix")))
          (T
           (PROG (X Y RES XL)
             (SETQ XL 0)
             (SETQ Y (REVAL1 BGL T))
             (COND ((NOT (BAGLISTP Y)) (TYPERR Y "bag or list"))
                   ((LEQ NR 0) (REDERR " THIRD ARG. MUST BE POSITIVE"))
                   (T (SETQ X (MATSM M))))
             (SETQ XL (LENGTH X))
             (COND
              ((NEQ (LENGTH (SETQ Y (CDR Y))) XL) (REDERR " MATRIX MISMATCH")))
             (SETQ Y
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J Y)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (SIMP J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (COND
              ((GREATERP (DIFFERENCE NR XL) 0)
               (REDERR " row number is out of range")))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (GREATERP (SETQ NR (DIFFERENCE NR 1)) 0)) (RETURN NIL)))
               (PROGN (SETQ RES (CONS (CAR X) RES)) (SETQ X (CDR X)))
               (GO WHILELABEL))
             (RPLACA X Y)
             (SETQ RES (APPEND (REVERSE RES) X))
             (RETURN RES))))) 
(PUT 'MATSUBR 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(MATSUBR) 'MATFLG) 
(PUT 'MATSUBC 'NUMBER-OF-ARGS 3) 
(PUT 'MATSUBC 'DEFINED-ON-LINE '146) 
(PUT 'MATSUBC 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'MATSUBC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MATSUBC (M BGL NC)
    (COND ((NEQ (GETRTYPE M) 'MATRIX) (REDERR (LIST M "should be a matrix")))
          (T
           (PROG (X Y RES XL)
             (SETQ XL 0)
             (SETQ Y (REVAL1 BGL T))
             (COND ((NOT (BAGLISTP Y)) (TYPERR Y "bag or list"))
                   ((LEQ NC 0) (REDERR " THIRD ARG. MUST BE POSITIVE"))
                   (T (SETQ X (TP1 (MATSM M)))))
             (SETQ XL (LENGTH X))
             (COND
              ((NEQ (LENGTH (SETQ Y (CDR Y))) XL) (REDERR " MATRIX MISMATCH")))
             (SETQ Y
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J Y)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (SIMP J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (COND
              ((GREATERP (DIFFERENCE NC XL) 0)
               (REDERR " column  number is out of range")))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (GREATERP (SETQ NC (DIFFERENCE NC 1)) 0)) (RETURN NIL)))
               (PROGN (SETQ RES (CONS (CAR X) RES)) (SETQ X (CDR X)))
               (GO WHILELABEL))
             (RPLACA X Y)
             (SETQ RES (TP1 (APPEND (REVERSE RES) X)))
             (RETURN RES))))) 
(PUT 'MATSUBC 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(MATSUBC) 'MATFLG) 
(PUT 'RMATEXTR 'NUMBER-OF-ARGS 1) 
(PUT 'RMATEXTR 'DEFINED-ON-LINE '169) 
(PUT 'RMATEXTR 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'RMATEXTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMATEXTR (U)
    (PROG (X Y N NL)
      (SETQ N 0)
      (SETQ NL 0)
      (SETQ X (MATSM (CAR U)))
      (SETQ Y (REVAL1 (CADR U) T))
      (SETQ N (REVAL1 (CADDR U) T))
      (COND
       ((NOT (FIXP N))
        (REDERR "Arguments are: matrix, vector name, line number"))
       ((NOT (BAGLISTP (LIST Y))) (SIMPBAGPROP (LIST Y T))))
      (SETQ NL (LENGTH X))
      (COND ((OR (LEQ N 0) (GREATERP N NL)) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N 1)) (RETURN NIL)))
        (PROGN (SETQ X (CDR X)) (SETQ N (DIFFERENCE N 1)))
        (GO WHILELABEL))
      (COND ((NULL X) (RETURN NIL)))
      (RETURN
       (SETQ X
               (CONS Y
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CAR X))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))) 
(PUT 'RMATEXTC 'NUMBER-OF-ARGS 1) 
(PUT 'RMATEXTC 'DEFINED-ON-LINE '183) 
(PUT 'RMATEXTC 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'RMATEXTC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMATEXTC (U)
    (PROG (X Y N NC)
      (SETQ N 0)
      (SETQ NC 0)
      (SETQ X (TP1 (MATSM (CAR U))))
      (SETQ Y (REVAL1 (CADR U) T))
      (SETQ N (REVAL1 (CADDR U) T))
      (COND
       ((NOT (FIXP N))
        (REDERR "Arguments are: matrix, vector name, line number"))
       ((NOT (BAGLISTP (LIST Y))) (SIMPBAGPROP (LIST Y T))))
      (SETQ NC (LENGTH X))
      (COND ((OR (LEQ N 0) (GREATERP N NC)) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N 1)) (RETURN NIL)))
        (PROGN (SETQ X (CDR X)) (SETQ N (DIFFERENCE N 1)))
        (GO WHILELABEL))
      (COND ((NULL X) (RETURN NIL)))
      (RETURN
       (SETQ X
               (CONS Y
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CAR X))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (PREPSQ J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))) 
(PUT 'MATEXTR 'PSOPFN 'RMATEXTR) 
(PUT 'MATEXTC 'PSOPFN 'RMATEXTC) 
(PUT 'HCONCMAT 'NUMBER-OF-ARGS 2) 
(PUT 'HCONCMAT 'DEFINED-ON-LINE '201) 
(PUT 'HCONCMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'HCONCMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HCONCMAT (U V) (|HCONCMAT:| (MATSM U) (MATSM V))) 
(PUT '|HCONCMAT:| 'NUMBER-OF-ARGS 2) 
(PUT '|HCONCMAT:| 'DEFINED-ON-LINE '205) 
(PUT '|HCONCMAT:| 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT '|HCONCMAT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |HCONCMAT:| (U V)
    (COND ((NULL U) V) ((NULL V) U)
          (T (CONS (APPEND (CAR U) (CAR V)) (|HCONCMAT:| (CDR U) (CDR V)))))) 
(PUT 'HCONCMAT 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(HCONCMAT) 'MATFLG) 
(PUT 'VCONCMAT 'NUMBER-OF-ARGS 2) 
(PUT 'VCONCMAT 'DEFINED-ON-LINE '212) 
(PUT 'VCONCMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'VCONCMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VCONCMAT (U V) (APPEND (MATSM U) (MATSM V))) 
(PUT 'VCONCMAT 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(VCONCMAT) 'MATFLG) 
(PUT 'TPRODL 'NUMBER-OF-ARGS 2) 
(PUT 'TPRODL 'DEFINED-ON-LINE '220) 
(PUT 'TPRODL 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'TPRODL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TPRODL (U V)
    (PROG (AA UL)
     L1
      (COND ((NULL U) (RETURN AA)))
      (SETQ UL (CAR U))
      (SETQ UL (MULTSM UL V))
      (SETQ AA (|HCONCMAT:| AA UL))
      (SETQ U (CDR U))
      (GO L1))) 
(PUT 'TPMAT 'NUMBER-OF-ARGS 2) 
(PUT 'TPMAT 'DEFINED-ON-LINE '230) 
(PUT 'TPMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'TPMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TPMAT (U V)
    (COND ((NULL (GETTYPE U)) (MULTSM (SIMP U) (MATSM V)))
          ((NULL (GETTYPE V)) (MULTSM (SIMP V) (MATSM U)))
          (T
           (PROG (AA UU VV)
             (SETQ UU (MATSM U))
             (SETQ VV (MATSM V))
             (PROG (X)
               (SETQ X UU)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X) (SETQ AA (APPEND AA (TPRODL X VV)))) (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN AA))))) 
(INFIX (LIST 'TPMAT)) 
(PUT 'TPMAT 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(TPMAT) 'MATFLG) 
(PUT 'HERMAT 'NUMBER-OF-ARGS 2) 
(FLAG '(HERMAT) 'OPFN) 
(PUT 'HERMAT 'DEFINED-ON-LINE '245) 
(PUT 'HERMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'HERMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HERMAT (M HM)
    (PROG (ML LL)
      (SETQ M (AEVAL (LIST 'TP M)))
      (SETQ ML (AEVAL (LIST 'COERCEMAT M 'LIST)))
      (SETQ LL
              (AEVAL
               (LIST 'LIST (LIST 'LENGTH (LIST 'FIRST ML)) (LIST 'LENGTH ML))))
      (SETQ ML
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST LL)) J))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ K 1)
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE
                                            (AEVAL* (LIST 'SECOND LL)) K))
                                     (RETURN (MAKELIST NIL))))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (AEVAL*
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL 'I
                                                                 (LIST 'MINUS
                                                                       'I))
                                                           (LIST 'CONS
                                                                 (LIST 'CONS ML
                                                                       J)
                                                                 K)))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ K
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            K))
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE
                                            (AEVAL* (LIST 'SECOND LL)) K))
                                     (RETURN (CONS 'LIST FORALL-RESULT))))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL 'I
                                                         (LIST 'MINUS 'I))
                                                   (LIST 'CONS
                                                         (LIST 'CONS ML J) K)))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ J
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         J))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST LL)) J))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                           (SETQ K 1)
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'SECOND LL)) K))
                             (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (AEVAL*
                                             (LIST 'SUB
                                                   (LIST 'EQUAL 'I
                                                         (LIST 'MINUS 'I))
                                                   (LIST 'CONS
                                                         (LIST 'CONS ML J) K)))
                                            NIL)))
                          LOOPLABEL
                           (SETQ K
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    K))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'SECOND LL)) K))
                             (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (AEVAL*
                                     (LIST 'SUB
                                           (LIST 'EQUAL 'I (LIST 'MINUS 'I))
                                           (LIST 'CONS (LIST 'CONS ML J) K)))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL (LIST 'BAGLMAT ML HM))
      (RETURN (AEVAL HM)))) 
(PUT 'SETELTMAT 'NUMBER-OF-ARGS 4) 
(PUT 'SETELTMAT 'DEFINED-ON-LINE '257) 
(PUT 'SETELTMAT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'SETELTMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETELTMAT (M ELT I J)
    (PROG (RES)
      (SETQ RES (MATSM M))
      (RPLACA (PNTH (NTH RES I) J) (SIMP ELT))
      (RETURN RES))) 
(PUT 'SETELTMAT 'RTYPEFN 'GETRTYPECAR) 
(FLAG '(SETELTMAT) 'MATFLG) 
(PUT 'SIMPGETELT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGETELT 'DEFINED-ON-LINE '266) 
(PUT 'SIMPGETELT 'DEFINED-IN-FILE 'ASSIST/MATREXT.RED) 
(PUT 'SIMPGETELT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGETELT (U)
    (PROG (MM)
      (SETQ MM (MATSM (CAR U)))
      (RETURN (NTH (NTH MM (CADR U)) (CADDR U))))) 
(PUT 'GETELTMAT 'SIMPFN 'SIMPGETELT) 
(ENDMODULE) 