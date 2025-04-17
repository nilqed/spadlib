(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MOID)) 
(PUT 'MOID_FROM_BAS 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_FROM_BAS 'DEFINED-ON-LINE '71) 
(PUT 'MOID_FROM_BAS 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_FROM_BAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_FROM_BAS (BAS)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (BAS_ZERODELETE BAS))
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (DP_LMON (BAS_DPOLY X))) (CAR X))
                            NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (X) (DP_LMON (BAS_DPOLY X))) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MOID_FROM_DPMAT 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_FROM_DPMAT 'DEFINED-ON-LINE '76) 
(PUT 'MOID_FROM_DPMAT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_FROM_DPMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_FROM_DPMAT (M)
    ((LAMBDA (U)
       (COND ((EQUAL (DPMAT_COLS M) 0) (LIST (CONS 0 U)))
             (T
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE (DPMAT_COLS M) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (CONS I
                                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ X U)
                                        STARTOVER
                                         (COND ((NULL X) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 ((LAMBDA (X)
                                                    (COND
                                                     ((EQUAL
                                                       (COND ((NULL (CAR X)) 0)
                                                             (T (CAAR X)))
                                                       I)
                                                      (LIST
                                                       (MO_DELETECOMP X)))))
                                                  (CAR X)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-RESULT))
                                         (SETQ X (CDR X))
                                         (COND
                                          ((ATOM FORALL-ENDPTR)
                                           (GO STARTOVER)))
                                        LOOPLABEL
                                         (COND
                                          ((NULL X) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 ((LAMBDA (X)
                                                    (COND
                                                     ((EQUAL
                                                       (COND ((NULL (CAR X)) 0)
                                                             (T (CAAR X)))
                                                       I)
                                                      (LIST
                                                       (MO_DELETECOMP X)))))
                                                  (CAR X)))
                                         (SETQ FORALL-ENDPTR
                                                 (LASTPAIR FORALL-ENDPTR))
                                         (SETQ X (CDR X))
                                         (GO LOOPLABEL)))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (DPMAT_COLS M) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (CONS I
                               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ X U)
                                STARTOVER
                                 (COND ((NULL X) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         ((LAMBDA (X)
                                            (COND
                                             ((EQUAL
                                               (COND ((NULL (CAR X)) 0)
                                                     (T (CAAR X)))
                                               I)
                                              (LIST (MO_DELETECOMP X)))))
                                          (CAR X)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                 (SETQ X (CDR X))
                                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                LOOPLABEL
                                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         ((LAMBDA (X)
                                            (COND
                                             ((EQUAL
                                               (COND ((NULL (CAR X)) 0)
                                                     (T (CAAR X)))
                                               I)
                                              (LIST (MO_DELETECOMP X)))))
                                          (CAR X)))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                 (SETQ X (CDR X))
                                 (GO LOOPLABEL)))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))))
     (MOID_FROM_BAS (DPMAT_LIST M)))) 
(PUT 'MOID_2A 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_2A 'DEFINED-ON-LINE '83) 
(PUT 'MOID_2A 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_2A (M)
    (CONS 'LIST
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X M)
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (DP_2A (LIST (DP_TERM (CALI_BC_FI 1) X))))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X) (DP_2A (LIST (DP_TERM (CALI_BC_FI 1) X))))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'MOID_FROM_A 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_FROM_A 'DEFINED-ON-LINE '87) 
(PUT 'MOID_FROM_A 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_FROM_A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_FROM_A (M)
    (COND ((NOT (EQCAR M 'LIST)) (TYPERR M "moideal"))
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X (CDR M))
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (DP_LMON (DP_FROM_A X))) (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (X) (DP_LMON (DP_FROM_A X))) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'MOID_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_PRINT 'DEFINED-ON-LINE '92) 
(PUT 'MOID_PRINT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_PRINT (M) (MATHPRINT (MOID_2A M))) 
(PUT 'MOID_SUM 'NUMBER-OF-ARGS 2) 
(PUT 'MOID_SUM 'DEFINED-ON-LINE '96) 
(PUT 'MOID_SUM 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_SUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID_SUM (A B) (MOID_RED (APPEND A B))) 
(PUT 'MOID_INTERSECT 'NUMBER-OF-ARGS 2) 
(PUT 'MOID_INTERSECT 'DEFINED-ON-LINE '100) 
(PUT 'MOID_INTERSECT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_INTERSECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID_INTERSECT (A B)
    (PROG (C)
      (PROG ()
       WHILELABEL
        (COND ((NOT B) (RETURN NIL)))
        (PROGN
         (SETQ C
                 (NCONC
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X A)
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X) (MO_LCM X (CAR B))) (CAR X))
                                     NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (MO_LCM X (CAR B))) (CAR X))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  C))
         (SETQ B (CDR B)))
        (GO WHILELABEL))
      (RETURN (MOID_RED C)))) 
(PUT 'MOID_SORT 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_SORT 'DEFINED-ON-LINE '110) 
(PUT 'MOID_SORT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_SORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_SORT (M) (SORT M (FUNCTION MO_DLEXCOMP))) 
(PUT 'MOID_RED 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_RED 'DEFINED-ON-LINE '114) 
(PUT 'MOID_RED 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_RED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_RED (M) (MOID=RED (MOID_SORT M))) 
(PUT 'MOID=RED 'NUMBER-OF-ARGS 1) 
(PUT 'MOID=RED 'DEFINED-ON-LINE '118) 
(PUT 'MOID=RED 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID=RED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID=RED (M)
    (PROG (V)
      (PROG ()
       WHILELABEL
        (COND ((NOT M) (RETURN NIL)))
        (PROGN
         (COND ((NOT (MOID_MEMBER (CAR M) (CDR M))) (SETQ V (CONS (CAR M) V))))
         (SETQ M (CDR M))
         NIL)
        (GO WHILELABEL))
      (RETURN (REVERSIP V)))) 
(PUT 'MOID_MEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'MOID_MEMBER 'DEFINED-ON-LINE '127) 
(PUT 'MOID_MEMBER 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_MEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID_MEMBER (MO M)
    (COND ((NULL M) NIL)
          (T (OR (MO_VDIVIDES? (CAR M) MO) (MOID_MEMBER MO (CDR M)))))) 
(PUT 'MOID_RADICAL 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_RADICAL 'DEFINED-ON-LINE '132) 
(PUT 'MOID_RADICAL 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_RADICAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_RADICAL (U)
    (MOID_RED
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (MO_RADICAL X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (MO_RADICAL X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'MOID_QUOT 'NUMBER-OF-ARGS 2) 
(PUT 'MOID_QUOT 'DEFINED-ON-LINE '136) 
(PUT 'MOID_QUOT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_QUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID_QUOT (M X)
    (MOID_RED
     (PROG (U FORALL-RESULT FORALL-ENDPTR)
       (SETQ U M)
       (COND ((NULL U) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (U) (MO_DIFF U (MO_GCD U X))) (CAR U))
                             NIL)))
      LOOPLABEL
       (SETQ U (CDR U))
       (COND ((NULL U) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (U) (MO_DIFF U (MO_GCD U X))) (CAR U)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'MOID_PRIMES 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_PRIMES 'DEFINED-ON-LINE '144) 
(PUT 'MOID_PRIMES 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_PRIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_PRIMES (M)
    (PROG (C M1 M2)
      (SETQ M
              (LISTMINIMIZE
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X M)
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (MO_SUPPORT X)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (MO_SUPPORT X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (FUNCTION SUBSETP)))
      (PROG (X)
        (SETQ X M)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((EQUAL (LENGTH X) 1) (SETQ M1 (CONS (CAR X) M1)))
                 (T (SETQ M2 (CONS X M2)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (MOID=PRIMES M2 (RING_NAMES CALI=BASERING)))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (APPEND M1 X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (APPEND M1 X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MOID=PRIMES 'NUMBER-OF-ARGS 2) 
(PUT 'MOID=PRIMES 'DEFINED-ON-LINE '155) 
(PUT 'MOID=PRIMES 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID=PRIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID=PRIMES (M VARS)
    (COND ((NULL M) (LIST NIL))
          (T
           (PROG (B)
             (SETQ B T)
             (PROG (X)
               (SETQ X M)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X) (SETQ B (AND B (INTERSECTION X VARS)))) (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (COND ((NOT B) (RETURN NIL)))
             (RETURN
              (LISTMINIMIZE
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (INTERSECTION (CAR M) VARS))
                STARTOVER
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (X)
                            (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Y
                                      (MOID=PRIMES (MOID=SPS X (CDR M))
                                       (SETQ VARS (DELETE X VARS))))
                              (COND ((NULL Y) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y) (CONS X Y))
                                                (CAR Y))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Y (CDR Y))
                              (COND ((NULL Y) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (Y) (CONS X Y)) (CAR Y))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ X (CDR X))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (X)
                            (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Y
                                      (MOID=PRIMES (MOID=SPS X (CDR M))
                                       (SETQ VARS (DELETE X VARS))))
                              (COND ((NULL Y) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y) (CONS X Y))
                                                (CAR Y))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Y (CDR Y))
                              (COND ((NULL Y) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (Y) (CONS X Y)) (CAR Y))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ X (CDR X))
                 (GO LOOPLABEL))
               (FUNCTION SUBSETP))))))) 
(PUT 'MOID=SPS 'NUMBER-OF-ARGS 2) 
(PUT 'MOID=SPS 'DEFINED-ON-LINE '167) 
(PUT 'MOID=SPS 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID=SPS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID=SPS (X M)
    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
      (SETQ Y M)
     STARTOVER
      (COND ((NULL Y) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (Y) (COND ((NOT (MEMQ X Y)) (LIST Y)))) (CAR Y)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ Y (CDR Y))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL Y) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (Y) (COND ((NOT (MEMQ X Y)) (LIST Y)))) (CAR Y)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ Y (CDR Y))
      (GO LOOPLABEL))) 
(PUT 'MOID_MAX 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_MAX 'DEFINED-ON-LINE '173) 
(PUT 'MOID_MAX 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_MAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_MAX (L)
    (COND ((NULL L) NIL)
          (T
           (CAR
            (SORT L (FUNCTION (LAMBDA (X Y) (GEQ (LENGTH X) (LENGTH Y))))))))) 
(PUT 'INDEPVARSETS* 'NUMBER-OF-ARGS 1) 
(PUT 'INDEPVARSETS* 'DEFINED-ON-LINE '178) 
(PUT 'INDEPVARSETS* 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'INDEPVARSETS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEPVARSETS* (M)
    (PROG (U N)
      (SETQ U
              (LISTMINIMIZE
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (MOID_FROM_DPMAT M))
                STARTOVER
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (X) (MOID_PRIMES (CDR X))) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ X (CDR X))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (X) (MOID_PRIMES (CDR X))) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ X (CDR X))
                 (GO LOOPLABEL))
               (FUNCTION SUBSETP)))
      (SETQ N (RING_NAMES CALI=BASERING))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X U)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (SETDIFF N X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (SETDIFF N X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MOID_GOODINDEPVARSET 'NUMBER-OF-ARGS 1) 
(PUT 'MOID_GOODINDEPVARSET 'DEFINED-ON-LINE '191) 
(PUT 'MOID_GOODINDEPVARSET 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID_GOODINDEPVARSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOID_GOODINDEPVARSET (M)
    (PROG (L N L1)
      (SETQ L
              (SORT (INDEPVARSETS* M)
                    (FUNCTION (LAMBDA (X Y) (GEQ (LENGTH X) (LENGTH Y))))))
      (COND ((NULL L) (RETURN NIL)))
      (SETQ N (LENGTH (FIRST L)))
      (SETQ L
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X L)
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (COND ((EQUAL (LENGTH X) N) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (COND ((EQUAL (LENGTH X) N) (LIST X))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (PROG (X)
        (SETQ X (REVERSE (RING_NAMES CALI=BASERING)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((GREATERP (LENGTH L) 1)
             (PROGN
              (SETQ L1
                      (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                        (SETQ Y L)
                       STARTOVER
                        (COND ((NULL Y) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (Y) (COND ((MEMBER X Y) (LIST Y))))
                                 (CAR Y)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ Y (CDR Y))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL Y) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (Y) (COND ((MEMBER X Y) (LIST Y))))
                                 (CAR Y)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ Y (CDR Y))
                        (GO LOOPLABEL)))
              (COND (L1 (SETQ L L1)))
              NIL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (FIRST L)))) 
(PUT 'DIM* 'NUMBER-OF-ARGS 1) 
(PUT 'DIM* 'DEFINED-ON-LINE '208) 
(PUT 'DIM* 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'DIM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIM* (M)
    (COND ((NOT (EQCAR M 'DPMAT)) (TYPERR M "DPMAT"))
          (T (LENGTH (MOID_MAX (INDEPVARSETS* M)))))) 
(PUT 'CODIM* 'NUMBER-OF-ARGS 1) 
(PUT 'CODIM* 'DEFINED-ON-LINE '214) 
(PUT 'CODIM* 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'CODIM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CODIM* (M) (DIFFERENCE (LENGTH (RING_NAMES CALI=BASERING)) (DIM* M))) 
(FLAG '(EASYINDEPSET) 'OPFN) 
(PUT 'EASYINDEPSET 'NUMBER-OF-ARGS 1) 
(PUT 'EASYINDEPSET 'DEFINED-ON-LINE '220) 
(PUT 'EASYINDEPSET 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'EASYINDEPSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYINDEPSET (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST (EASYINDEPSET* (DPMAT_FROM_A (REVAL1 M T)))))
     (T (EASYINDEPSET* M)))) 
(PUT 'EASYINDEPSET* 'NUMBER-OF-ARGS 1) 
(PUT 'EASYINDEPSET* 'DEFINED-ON-LINE '225) 
(PUT 'EASYINDEPSET* 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'EASYINDEPSET* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYINDEPSET* (M)
    (PROG (B C D)
      (SETQ M
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X M)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (MO_SUPPORT X)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (MO_SUPPORT X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ B (SETQ C (RING_NAMES CALI=BASERING)))
      (PROG (X)
        (SETQ X B)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((MOID=EPT (SETQ D (DELETE X C)) M) (SETQ C D))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (SETDIFF (RING_NAMES CALI=BASERING) C)))) 
(PUT 'MOID=EPT 'NUMBER-OF-ARGS 2) 
(PUT 'MOID=EPT 'DEFINED-ON-LINE '235) 
(PUT 'MOID=EPT 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'MOID=EPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOID=EPT (L M)
    (COND ((NULL M) T) (T (AND (INTERSECTION L (CAR M)) (MOID=EPT L (CDR M)))))) 
(FLAG '(EASYDIM) 'OPFN) 
(PUT 'EASYDIM 'NUMBER-OF-ARGS 1) 
(PUT 'EASYDIM 'DEFINED-ON-LINE '240) 
(PUT 'EASYDIM 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'EASYDIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYDIM (M)
    (COND ((EQUAL *MODE 'ALGEBRAIC) (EASYDIM* (DPMAT_FROM_A (REVAL1 M T))))
          (T (EASYDIM* M)))) 
(PUT 'EASYDIM* 'NUMBER-OF-ARGS 1) 
(PUT 'EASYDIM* 'DEFINED-ON-LINE '244) 
(PUT 'EASYDIM* 'DEFINED-IN-FILE 'CALI/MOID.RED) 
(PUT 'EASYDIM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYDIM* (M)
    (COND ((NOT (EQCAR M 'DPMAT)) (TYPERR M "DPMAT"))
          (T
           (LISTEXPAND (FUNCTION MAX2)
                       (PROG (X FORALL-RESULT FORALL-ENDPTR)
                         (SETQ X (MOID_FROM_DPMAT M))
                         (COND ((NULL X) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (X)
                                             (LENGTH (EASYINDEPSET* (CDR X))))
                                           (CAR X))
                                          NIL)))
                        LOOPLABEL
                         (SETQ X (CDR X))
                         (COND ((NULL X) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (X)
                                     (LENGTH (EASYINDEPSET* (CDR X))))
                                   (CAR X))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))) 
(ENDMODULE) 