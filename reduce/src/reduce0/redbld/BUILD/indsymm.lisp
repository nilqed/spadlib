(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INDSYMM)) 
(FLUID '(INDL)) 
(PUT 'INDEX-SYMMETRIESTAT 'NUMBER-OF-ARGS 0) 
(PUT 'INDEX-SYMMETRIESTAT 'DEFINED-ON-LINE '36) 
(PUT 'INDEX-SYMMETRIESTAT 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDEX-SYMMETRIESTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INDEX-SYMMETRIESTAT NIL
    (PROG (RES X Y)
      (SCAN)
     A
      (SETQ RES
              (CONS
               (PROG (INDEXEDVARS SYMS ASYMS)
                D
                 (SETQ INDEXEDVARS (CONS (XREAD1 'FOR) INDEXEDVARS))
                 (COND ((NULL (EQ CURSYM* '*COLON*)) (PROGN (SCAN) (GO D))))
                 (SETQ X (SCAN))
                 (COND ((EQ X 'SYMMETRIC) (GO SYM))
                       ((EQ X 'ANTISYMMETRIC) (GO ASYM))
                       (T (SYMERR 'INDEX-SYMMETRIES T)))
                SYM
                 (COND
                  ((EQ (SCAN) 'IN)
                   (PROG ()
                     (SCAN)
                     (FLAG '(ANTISYMMETRIC) 'DELIM)
                    B
                     (SETQ Y (CDR (XREAD1 'FOR)))
                     (COND
                      ((EQCAR (CAR Y) 'LIST)
                       (SETQ Y
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J Y)
                                 (COND ((NULL J) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (COND
                                                   ((AND (EQCAR (CAR J) 'LIST)
                                                         (OR (NULL (CDR J))
                                                             (EQUAL
                                                              (LENGTH (CAR J))
                                                              (LENGTH
                                                               (CADR J)))))
                                                    (CDAR J))
                                                   (T
                                                    (SYMERR 'INDEX-SYMMETRIES
                                                            T)))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (CDR J))
                                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (COND
                                           ((AND (EQCAR (CAR J) 'LIST)
                                                 (OR (NULL (CDR J))
                                                     (EQUAL (LENGTH (CAR J))
                                                            (LENGTH
                                                             (CADR J)))))
                                            (CDAR J))
                                           (T (SYMERR 'INDEX-SYMMETRIES T)))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))))
                     (SETQ SYMS (CONS Y SYMS))
                     (COND
                      ((AND (NULL (EQ (SETQ X CURSYM*) 'ANTISYMMETRIC))
                            (NULL (EQ X '*SEMICOL*)) (EQ (SCAN) '*LCBKT*))
                       (GO B)))
                     (REMFLAG '(ANTISYMMETRIC) 'DELIM)))
                  (T
                   (PROGN
                    (SETQ SYMS 'SYMMETRIC)
                    (SETQ X CURSYM*)
                    (COND ((EQ X '*COMMA*) (SCAN))))))
                 (COND ((EQ X 'ANTISYMMETRIC) (GO ASYM))
                       (T (RETURN (LIST INDEXEDVARS SYMS ASYMS))))
                ASYM
                 (COND
                  ((EQ (SCAN) 'IN)
                   (PROG ()
                     (SCAN)
                     (FLAG '(SYMMETRIC) 'DELIM)
                    C
                     (SETQ Y (CDR (XREAD1 'FOR)))
                     (COND
                      ((EQCAR (CAR Y) 'LIST)
                       (SETQ Y
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J Y)
                                 (COND ((NULL J) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (COND
                                                   ((AND (EQCAR (CAR J) 'LIST)
                                                         (OR (NULL (CDR J))
                                                             (EQUAL
                                                              (LENGTH (CAR J))
                                                              (LENGTH
                                                               (CADR J)))))
                                                    (CDAR J))
                                                   (T
                                                    (SYMERR 'INDEX-SYMMETRIES
                                                            T)))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (CDR J))
                                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (COND
                                           ((AND (EQCAR (CAR J) 'LIST)
                                                 (OR (NULL (CDR J))
                                                     (EQUAL (LENGTH (CAR J))
                                                            (LENGTH
                                                             (CADR J)))))
                                            (CDAR J))
                                           (T (SYMERR 'INDEX-SYMMETRIES T)))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))))
                     (SETQ ASYMS (CONS Y ASYMS))
                     (COND
                      ((AND (NULL (EQ (SETQ X CURSYM*) 'SYMMETRIC))
                            (NULL (EQ X '*SEMICOL*)) (EQ (SCAN) '*LCBKT*))
                       (GO C)))
                     (REMFLAG '(SYMMETRIC) 'DELIM)))
                  (T
                   (PROGN
                    (SETQ ASYMS 'ANTISYMMETRIC)
                    (SETQ X CURSYM*)
                    (COND ((EQ X '*COMMA*) (SCAN))))))
                 (COND ((EQ X 'SYMMETRIC) (GO SYM))
                       (T (RETURN (LIST INDEXEDVARS SYMS ASYMS)))))
               RES))
      (COND ((NULL (EQ X '*SEMICOL*)) (GO A)))
      (RETURN (LIST 'INDEXSYMMETRIES (MKQUOTE RES))))) 
(PUT 'INDEX_SYMMETRIES 'STAT 'INDEX-SYMMETRIESTAT) 
(PUT 'INDEXSYMMETRIES 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXSYMMETRIES 'DEFINED-ON-LINE '98) 
(PUT 'INDEXSYMMETRIES 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDEXSYMMETRIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXSYMMETRIES (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J)
         (PROG (V X Y Z N)
           (SETQ N 0)
           (SETQ V (CDR J))
           (PROG (M)
             (SETQ M (CAR J))
            LAB
             (COND ((NULL M) (RETURN NIL)))
             ((LAMBDA (M)
                (PROGN
                 (SETQ X V)
                 (COND
                  ((EQ (CAR V) 'SYMMETRIC)
                   (SETQ X (CONS (LIST (CDR M)) (CDR V))))
                  ((EQ (CADR V) 'ANTISYMMETRIC)
                   (SETQ X (LIST (CAR V) (LIST (CDR M))))))
                 (SETQ N 0)
                 (SETQ Z X)
                 (PROG (K)
                   (SETQ K (CDR M))
                  LAB
                   (COND ((NULL K) (RETURN NIL)))
                   ((LAMBDA (K)
                      (PROGN
                       (SETQ X
                               (SUBST (LIST 'NTH 'INDL (SETQ N (PLUS N 1))) K
                                      X))
                       (SETQ Z (SUBST N K Z))))
                    (CAR K))
                   (SETQ K (CDR K))
                   (GO LAB))
                 (SETQ Y
                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L (CAR X))
                           (COND ((NULL L) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (L)
                                               (LIST 'LAMBDA '(INDL)
                                                     (LIST 'TOT-SYM-INDP
                                                           (LIST 'EVLIS
                                                                 (COND
                                                                  ((ATOM
                                                                    (CAAR L))
                                                                   (MKQUOTE L))
                                                                  (T
                                                                   (MKQUOTE
                                                                    (PROG (R
                                                                           FORALL-RESULT
                                                                           FORALL-ENDPTR)
                                                                      (SETQ R
                                                                              L)
                                                                      (COND
                                                                       ((NULL
                                                                         R)
                                                                        (RETURN
                                                                         NIL)))
                                                                      (SETQ FORALL-RESULT
                                                                              (SETQ FORALL-ENDPTR
                                                                                      (CONS
                                                                                       ((LAMBDA
                                                                                            (
                                                                                             R)
                                                                                          (LIST
                                                                                           'EVLIS
                                                                                           (MKQUOTE
                                                                                            R)))
                                                                                        (CAR
                                                                                         R))
                                                                                       NIL)))
                                                                     LOOPLABEL
                                                                      (SETQ R
                                                                              (CDR
                                                                               R))
                                                                      (COND
                                                                       ((NULL
                                                                         R)
                                                                        (RETURN
                                                                         FORALL-RESULT)))
                                                                      (RPLACD
                                                                       FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              R)
                                                                           (LIST
                                                                            'EVLIS
                                                                            (MKQUOTE
                                                                             R)))
                                                                         (CAR
                                                                          R))
                                                                        NIL))
                                                                      (SETQ FORALL-ENDPTR
                                                                              (CDR
                                                                               FORALL-ENDPTR))
                                                                      (GO
                                                                       LOOPLABEL)))))))))
                                             (CAR L))
                                            NIL)))
                          LOOPLABEL
                           (SETQ L (CDR L))
                           (COND ((NULL L) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (L)
                                       (LIST 'LAMBDA '(INDL)
                                             (LIST 'TOT-SYM-INDP
                                                   (LIST 'EVLIS
                                                         (COND
                                                          ((ATOM (CAAR L))
                                                           (MKQUOTE L))
                                                          (T
                                                           (MKQUOTE
                                                            (PROG (R
                                                                   FORALL-RESULT
                                                                   FORALL-ENDPTR)
                                                              (SETQ R L)
                                                              (COND
                                                               ((NULL R)
                                                                (RETURN NIL)))
                                                              (SETQ FORALL-RESULT
                                                                      (SETQ FORALL-ENDPTR
                                                                              (CONS
                                                                               ((LAMBDA
                                                                                    (
                                                                                     R)
                                                                                  (LIST
                                                                                   'EVLIS
                                                                                   (MKQUOTE
                                                                                    R)))
                                                                                (CAR
                                                                                 R))
                                                                               NIL)))
                                                             LOOPLABEL
                                                              (SETQ R (CDR R))
                                                              (COND
                                                               ((NULL R)
                                                                (RETURN
                                                                 FORALL-RESULT)))
                                                              (RPLACD
                                                               FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (R)
                                                                   (LIST 'EVLIS
                                                                         (MKQUOTE
                                                                          R)))
                                                                 (CAR R))
                                                                NIL))
                                                              (SETQ FORALL-ENDPTR
                                                                      (CDR
                                                                       FORALL-ENDPTR))
                                                              (GO
                                                               LOOPLABEL)))))))))
                                     (CAR L))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (PROG (L)
                   (SETQ L (CADR X))
                  LAB
                   (COND ((NULL L) (RETURN NIL)))
                   ((LAMBDA (L)
                      (SETQ Y
                              (CONS
                               (LIST 'LAMBDA '(INDL)
                                     (LIST 'TOT-ASYM-INDP
                                           (LIST 'EVLIS
                                                 (COND
                                                  ((ATOM (CAAR L)) (MKQUOTE L))
                                                  (T
                                                   (MKQUOTE
                                                    (PROG (R FORALL-RESULT
                                                           FORALL-ENDPTR)
                                                      (SETQ R L)
                                                      (COND
                                                       ((NULL R) (RETURN NIL)))
                                                      (SETQ FORALL-RESULT
                                                              (SETQ FORALL-ENDPTR
                                                                      (CONS
                                                                       ((LAMBDA
                                                                            (R)
                                                                          (LIST
                                                                           'EVLIS
                                                                           (MKQUOTE
                                                                            R)))
                                                                        (CAR
                                                                         R))
                                                                       NIL)))
                                                     LOOPLABEL
                                                      (SETQ R (CDR R))
                                                      (COND
                                                       ((NULL R)
                                                        (RETURN
                                                         FORALL-RESULT)))
                                                      (RPLACD FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (R)
                                                                  (LIST 'EVLIS
                                                                        (MKQUOTE
                                                                         R)))
                                                                (CAR R))
                                                               NIL))
                                                      (SETQ FORALL-ENDPTR
                                                              (CDR
                                                               FORALL-ENDPTR))
                                                      (GO LOOPLABEL))))))))
                               Y)))
                    (CAR L))
                   (SETQ L (CDR L))
                   (GO LAB))
                 (PUT (CAR M) 'INDXSYMMETRIES Y)
                 (SETQ Y
                         (PROG (L FORALL-RESULT FORALL-ENDPTR)
                           (SETQ L (CAR Z))
                           (COND ((NULL L) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (L)
                                               (LIST 'LAMBDA '(INDL)
                                                     (LIST 'SYMMETRIZE-INDS
                                                           (MKQUOTE L) 'INDL)))
                                             (CAR L))
                                            NIL)))
                          LOOPLABEL
                           (SETQ L (CDR L))
                           (COND ((NULL L) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (L)
                                       (LIST 'LAMBDA '(INDL)
                                             (LIST 'SYMMETRIZE-INDS (MKQUOTE L)
                                                   'INDL)))
                                     (CAR L))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (PROG (L)
                   (SETQ L (CADR Z))
                  LAB
                   (COND ((NULL L) (RETURN NIL)))
                   ((LAMBDA (L)
                      (SETQ Y
                              (CONS
                               (LIST 'LAMBDA '(INDL)
                                     (LIST 'ASYMMETRIZE-INDS (MKQUOTE L)
                                           'INDL))
                               Y)))
                    (CAR L))
                   (SETQ L (CDR L))
                   (GO LAB))
                 (PUT (CAR M) 'INDXSYMMETRIZE Y)))
              (CAR M))
             (SETQ M (CDR M))
             (GO LAB))))
       (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'INDXSYMP 'NUMBER-OF-ARGS 2) 
(PUT 'INDXSYMP 'DEFINED-ON-LINE '140) 
(PUT 'INDXSYMP 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDXSYMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDXSYMP (U BOOL)
    (OR (NULL BOOL) (AND (APPLY1 (CAR BOOL) U) (INDXSYMP U (CDR BOOL))))) 
(PUT 'TOT-SYM-INDP 'NUMBER-OF-ARGS 1) 
(PUT 'TOT-SYM-INDP 'DEFINED-ON-LINE '143) 
(PUT 'TOT-SYM-INDP 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'TOT-SYM-INDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOT-SYM-INDP (U)
    (OR (NULL U) (NULL (CDR U)) (EQUAL (CAR U) (CADR U))
        (AND
         (COND ((ATOM (CAR U)) (INDORDP (CAR U) (CADR U)))
               (T
                (OR (INDXCHK (CAR U)) (INDXCHK (CADR U))
                    (INDORDLP (CAR U) (CADR U)))))
         (TOT-SYM-INDP (CDR U))))) 
(PUT 'TOT-ASYM-INDP 'NUMBER-OF-ARGS 1) 
(PUT 'TOT-ASYM-INDP 'DEFINED-ON-LINE '150) 
(PUT 'TOT-ASYM-INDP 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'TOT-ASYM-INDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOT-ASYM-INDP (U)
    (OR (NULL U) (NULL (CDR U))
        (AND
         (AND (NULL (EQUAL (CAR U) (CADR U)))
              (COND ((ATOM (CAR U)) (INDORDP (CAR U) (CADR U)))
                    (T
                     (OR (INDXCHK (CAR U)) (INDXCHK (CADR U))
                         (INDORDLP (CAR U) (CADR U))))))
         (TOT-ASYM-INDP (CDR U))))) 
(PUT 'INDEXSYMMETRIZE 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXSYMMETRIZE 'DEFINED-ON-LINE '157) 
(PUT 'INDEXSYMMETRIZE 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDEXSYMMETRIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXSYMMETRIZE (U)
    (PROG (X Y SGN)
      (SETQ SGN 0)
      (SETQ X (GET (CAR U) 'INDXSYMMETRIZE))
      (SETQ SGN 1)
      (SETQ Y (CONS 1 (CDR U)))
     A
      (COND ((NULL X) (RETURN (CONS SGN (CONS (CAR U) (CDR Y))))))
      (SETQ Y (APPLY1 (CAR X) (CDR Y)))
      (COND ((NULL Y) (RETURN NIL)))
      (SETQ SGN (TIMES (CAR Y) SGN))
      (SETQ X (CDR X))
      (GO A))) 
(PUT 'SYMMETRIZE-INDS 'NUMBER-OF-ARGS 2) 
(PUT 'SYMMETRIZE-INDS 'DEFINED-ON-LINE '170) 
(PUT 'SYMMETRIZE-INDS 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'SYMMETRIZE-INDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMMETRIZE-INDS (U V)
    (PROG (X Y Z N)
      (SETQ N 0)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((ATOM J) (NTH V J))
                                          (T
                                           (PROG (K FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ K J)
                                             (COND ((NULL K) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (K)
                                                                 (NTH V K))
                                                               (CAR K))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ K (CDR K))
                                             (COND
                                              ((NULL K)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) (NTH V K))
                                                       (CAR K))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((ATOM J) (NTH V J))
                                  (T
                                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ K J)
                                     (COND ((NULL K) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) (NTH V K))
                                                       (CAR K))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ K (CDR K))
                                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (K) (NTH V K)) (CAR K))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Z (COND ((ATOM (CAR X)) (INDORDN X)) (T (FLATINDL (INDORDLN X)))))
      (COND ((NULL (ATOM (CAR U))) (SETQ U (FLATINDL U))))
      (SETQ X (PAIR U Z))
      (RETURN
       (CONS 1
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J V)
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (COND
                                    ((AND X
                                          (EQUAL (CAAR X) (SETQ N (PLUS N 1))))
                                     (PROGN
                                      (SETQ Y (CDAR X))
                                      (SETQ X (CDR X))
                                      Y))
                                    (T J)))
                                 (CAR J))
                                NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (J)
                           (COND
                            ((AND X (EQUAL (CAAR X) (SETQ N (PLUS N 1))))
                             (PROGN (SETQ Y (CDAR X)) (SETQ X (CDR X)) Y))
                            (T J)))
                         (CAR J))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'ASYMMETRIZE-INDS 'NUMBER-OF-ARGS 2) 
(PUT 'ASYMMETRIZE-INDS 'DEFINED-ON-LINE '188) 
(PUT 'ASYMMETRIZE-INDS 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'ASYMMETRIZE-INDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASYMMETRIZE-INDS (U V)
    (PROG (X Y Z N SGN)
      (SETQ N 0)
      (SETQ SGN 0)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((ATOM J) (NTH V J))
                                          (T
                                           (PROG (K FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ K J)
                                             (COND ((NULL K) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (K)
                                                                 (NTH V K))
                                                               (CAR K))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ K (CDR K))
                                             (COND
                                              ((NULL K)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) (NTH V K))
                                                       (CAR K))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((ATOM J) (NTH V J))
                                  (T
                                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ K J)
                                     (COND ((NULL K) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (K) (NTH V K))
                                                       (CAR K))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ K (CDR K))
                                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (K) (NTH V K)) (CAR K))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((REPEATS X) (RETURN NIL)))
      (SETQ SGN
              (COND
               ((PERMP
                 (SETQ Z (COND ((ATOM (CAR X)) (INDORDN X)) (T (INDORDLN X))))
                 X)
                1)
               (T (MINUS 1))))
      (COND
       ((NULL (ATOM (CAR U)))
        (PROGN (SETQ U (FLATINDL U)) (SETQ Z (FLATINDL Z)))))
      (SETQ Z (PAIR U Z))
      (RETURN
       (CONS SGN
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J V)
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (COND
                                    ((AND Z
                                          (EQUAL (CAAR Z) (SETQ N (PLUS N 1))))
                                     (PROGN
                                      (SETQ Y (CDAR Z))
                                      (SETQ Z (CDR Z))
                                      Y))
                                    (T J)))
                                 (CAR J))
                                NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (J)
                           (COND
                            ((AND Z (EQUAL (CAAR Z) (SETQ N (PLUS N 1))))
                             (PROGN (SETQ Y (CDAR Z)) (SETQ Z (CDR Z)) Y))
                            (T J)))
                         (CAR J))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'INDORDLN 'NUMBER-OF-ARGS 1) 
(PUT 'INDORDLN 'DEFINED-ON-LINE '209) 
(PUT 'INDORDLN 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDORDLN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDORDLN (U)
    (COND ((NULL U) NIL) ((NULL (CDR U)) U)
          ((NULL (CDDR U)) (INDORDL2 (CAR U) (CADR U)))
          (T (INDORDLAD (CAR U) (INDORDLN (CDR U)))))) 
(PUT 'INDORDL2 'NUMBER-OF-ARGS 2) 
(PUT 'INDORDL2 'DEFINED-ON-LINE '215) 
(PUT 'INDORDL2 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDORDL2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDORDL2 (U V) (COND ((INDORDLP U V) (LIST U V)) (T (LIST V U)))) 
(PUT 'INDORDLAD 'NUMBER-OF-ARGS 2) 
(PUT 'INDORDLAD 'DEFINED-ON-LINE '218) 
(PUT 'INDORDLAD 'DEFINED-IN-FILE 'EXCALC/INDSYMM.RED) 
(PUT 'INDORDLAD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDORDLAD (A U)
    (COND ((NULL U) (LIST A)) ((INDORDLP A (CAR U)) (CONS A U))
          (T (CONS (CAR U) (INDORDLAD A (CDR U)))))) 
(ENDMODULE) 