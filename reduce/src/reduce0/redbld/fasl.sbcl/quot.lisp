(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'QUOT)) 
(FLAG '(MATQUOT) 'OPFN) 
(PUT 'MATQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'MATQUOT 'DEFINED-ON-LINE '46) 
(PUT 'MATQUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATQUOT (M F)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (COND
       ((OR (EQCAR F 'LIST) (EQCAR F 'MAT))
        (REDERR "Syntax : matquot(dpmat,dpoly)"))
       (T
        (DPMAT_2A
         (MATQUOT* (DPMAT_FROM_A (REVAL1 M T)) (DP_FROM_A (REVAL1 F T)))))))
     (T (MATQUOT* M F)))) 
(PUT 'MATQUOT* 'NUMBER-OF-ARGS 2) 
(PUT 'MATQUOT* 'DEFINED-ON-LINE '53) 
(PUT 'MATQUOT* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATQUOT* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATQUOT* (M F)
    (COND ((DP_UNIT? F) M)
          ((EQUAL (DPMAT_COLS M) 0) (MAT2LIST* (QUOT=QUOT (IDEAL2MAT* M) F)))
          (T (QUOT=QUOT M F)))) 
(PUT 'QUOT=QUOT 'NUMBER-OF-ARGS 2) 
(PUT 'QUOT=QUOT 'DEFINED-ON-LINE '58) 
(PUT 'QUOT=QUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'QUOT=QUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOT=QUOT (M F)
    (PROG (A B)
      (SETQ A
              (MATINTERSECT*
               (LIST M
                     (DPMAT_TIMES_DPOLY F
                      (DPMAT_UNIT (DPMAT_COLS M) (DPMAT_COLDEGS M))))))
      (SETQ B
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (DPMAT_LIST A))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (BAS_MAKE (BAS_NR X)
                                     (CAR (DP_PSEUDODIVMOD (BAS_DPOLY X) F))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (BAS_MAKE (BAS_NR X)
                             (CAR (DP_PSEUDODIVMOD (BAS_DPOLY X) F))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (DPMAT_MAKE (DPMAT_ROWS A) (DPMAT_COLS A) B (DPMAT_COLDEGS M)
        (DPMAT_GBTAG A))))) 
(FLAG '(IDEALQUOTIENT) 'OPFN) 
(PUT 'IDEALQUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'IDEALQUOTIENT 'DEFINED-ON-LINE '73) 
(PUT 'IDEALQUOTIENT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'IDEALQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDEALQUOTIENT (M N)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A
       (IDEALQUOTIENT2* (DPMAT_FROM_A (REVAL1 M T))
        (DPMAT_FROM_A (REVAL1 N T)))))
     (T (IDEALQUOTIENT2* M N)))) 
(FLAG '(MODULEQUOTIENT) 'OPFN) 
(PUT 'MODULEQUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'MODULEQUOTIENT 'DEFINED-ON-LINE '84) 
(PUT 'MODULEQUOTIENT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MODULEQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODULEQUOTIENT (M N)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A
       (MODULEQUOTIENT2* (DPMAT_FROM_A (REVAL1 M T))
        (DPMAT_FROM_A (REVAL1 N T)))))
     (T (MODULEQUOTIENT2* M N)))) 
(FLAG '(ANNIHILATOR) 'OPFN) 
(PUT 'ANNIHILATOR 'NUMBER-OF-ARGS 1) 
(PUT 'ANNIHILATOR 'DEFINED-ON-LINE '93) 
(PUT 'ANNIHILATOR 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'ANNIHILATOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANNIHILATOR (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (ANNIHILATOR2* (DPMAT_FROM_A (REVAL1 M T)))))
     (T (ANNIHILATOR2* M)))) 
(PUT 'IDEALQUOTIENT2* 'NUMBER-OF-ARGS 2) 
(PUT 'IDEALQUOTIENT2* 'DEFINED-ON-LINE '100) 
(PUT 'IDEALQUOTIENT2* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'IDEALQUOTIENT2* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDEALQUOTIENT2* (M N)
    (COND
     ((GREATERP (DPMAT_COLS N) 0)
      (REDERR "Syntax : idealquotient(dpmat,ideal)"))
     ((EQUAL (DPMAT_COLS M) 0) (MODULEQUOTIENT2* M N))
     ((EQUAL (DPMAT_COLS M) 1)
      (IDEAL2MAT* (MODULEQUOTIENT2* M (IDEAL2MAT* N))))
     (T
      (MATINTERSECT*
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (DPMAT_LIST N))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (X) (QUOT=QUOT M (BAS_DPOLY X))) (CAR X))
                          NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (QUOT=QUOT M (BAS_DPOLY X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'MODULEQUOTIENT2* 'NUMBER-OF-ARGS 2) 
(PUT 'MODULEQUOTIENT2* 'DEFINED-ON-LINE '108) 
(PUT 'MODULEQUOTIENT2* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MODULEQUOTIENT2* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODULEQUOTIENT2* (M N)
    ((LAMBDA (CALI=DEGREES)
       (PROG (C)
         (COND
          ((NOT (EQUAL (SETQ C (DPMAT_COLS M)) (DPMAT_COLS N)))
           (REDERR
            "MODULEQUOTIENT only for submodules of a common free module")))
         (COND
          ((NOT (EQUAL (DPMAT_COLDEGS M) (DPMAT_COLDEGS N)))
           (REDERR "matrices don't match for MODULEQUOTIENT")))
         (COND
          ((EQUAL C 0)
           (PROGN (SETQ M (IDEAL2MAT* M)) (SETQ N (IDEAL2MAT* N)))))
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (SETQ N
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (DPMAT_LIST N))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ N
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X N)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (RETURN
          (COND ((NULL N) (DPMAT_FROM_DPOLY (DP_FI 1)))
                (T
                 (MATINTERSECT*
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X N)
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X) (QUOT=MQUOT M X)) (CAR X))
                                     NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (QUOT=MQUOT M X)) (CAR X)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))))
     CALI=DEGREES)) 
(PUT 'QUOT=MQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'QUOT=MQUOT 'DEFINED-ON-LINE '122) 
(PUT 'QUOT=MQUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'QUOT=MQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOT=MQUOT (M F)
    (PROG (A B)
      (SETQ A
              (MATINTERSECT*
               (LIST M
                     (DPMAT_MAKE 1 (DPMAT_COLS M) (LIST (BAS_MAKE 1 F))
                      (DPMAT_COLDEGS M) T))))
      (SETQ B
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (DPMAT_LIST A))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (BAS_MAKE (BAS_NR X)
                                     (CAR (DP_PSEUDODIVMOD (BAS_DPOLY X) F))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (BAS_MAKE (BAS_NR X)
                             (CAR (DP_PSEUDODIVMOD (BAS_DPOLY X) F))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (DPMAT_MAKE (DPMAT_ROWS A) 0 B NIL NIL)))) 
(PUT 'ANNIHILATOR2* 'NUMBER-OF-ARGS 1) 
(PUT 'ANNIHILATOR2* 'DEFINED-ON-LINE '131) 
(PUT 'ANNIHILATOR2* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'ANNIHILATOR2* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANNIHILATOR2* (M)
    (COND ((EQUAL (DPMAT_COLS M) 0) M) ((EQUAL (DPMAT_COLS M) 1) (MAT2LIST* M))
          (T
           (MODULEQUOTIENT2* M (DPMAT_UNIT (DPMAT_COLS M) (DPMAT_COLDEGS M)))))) 
(PUT 'IDEALQUOTIENT1* 'NUMBER-OF-ARGS 2) 
(PUT 'IDEALQUOTIENT1* 'DEFINED-ON-LINE '138) 
(PUT 'IDEALQUOTIENT1* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'IDEALQUOTIENT1* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDEALQUOTIENT1* (M N)
    (COND
     ((GREATERP (DPMAT_COLS N) 0) (REDERR "second parameter must be an ideal"))
     ((EQUAL (DPMAT_COLS M) 0) (MODULEQUOTIENT1* M N))
     ((EQUAL (DPMAT_COLS M) 1)
      (IDEAL2MAT* (MODULEQUOTIENT1* M (IDEAL2MAT* N))))
     (T
      ((LAMBDA (CALI=DEGREES CALI=BASERING)
         (PROG (U1 U2 F V R M1)
           (SETQ V (LIST (MAKE_CALI_VARNAME)))
           (SETQ R CALI=BASERING)
           (SETRING*
            (RING_SUM R (RING_DEFINE V (DEGREEORDER* V) 'REVLEX '(1))))
           (SETQ CALI=DEGREES (MO_DEGNEWORDER (DPMAT_COLDEGS M)))
           (SETQ N
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (DPMAT_LIST N))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X) (DP_NEWORDER X)) (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (X) (DP_NEWORDER X)) (CAR X)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ U1 (SETQ U2 (DP_FROM_A (CAR V))))
           (SETQ F (CAR N))
           (PROG (X)
             (SETQ X N)
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X)
                (PROGN
                 (SETQ F (DP_SUM F (DP_PROD U1 X)))
                 (SETQ U1 (DP_PROD U1 U2))))
              (CAR X))
             (SETQ X (CDR X))
             (GO LAB))
           (SETQ M1
                   (DPMAT_SIEVE (GBASIS* (QUOT=QUOT (DPMAT_NEWORDER M NIL) F))
                    V T))
           (SETRING* R)
           (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
           (RETURN (DPMAT_NEWORDER M1 T))))
       CALI=DEGREES CALI=BASERING)))) 
(PUT 'MODULEQUOTIENT1* 'NUMBER-OF-ARGS 2) 
(PUT 'MODULEQUOTIENT1* 'DEFINED-ON-LINE '158) 
(PUT 'MODULEQUOTIENT1* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MODULEQUOTIENT1* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODULEQUOTIENT1* (M N)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (C U1 U2 F V R M1)
         (COND
          ((NOT (EQUAL (SETQ C (DPMAT_COLS M)) (DPMAT_COLS N)))
           (REDERR
            "MODULEQUOTIENT only for submodules of a common free module")))
         (COND
          ((NOT (EQUAL (DPMAT_COLDEGS M) (DPMAT_COLDEGS N)))
           (REDERR "matrices don't match for MODULEQUOTIENT")))
         (COND
          ((EQUAL C 0)
           (PROGN (SETQ M (IDEAL2MAT* M)) (SETQ N (IDEAL2MAT* N)))))
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (SETQ N
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (DPMAT_LIST N))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X) (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ N
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X N)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))
         (COND ((NULL N) (RETURN (DPMAT_FROM_DPOLY (DP_FI 1)))))
         (SETQ V (LIST (MAKE_CALI_VARNAME)))
         (SETQ R CALI=BASERING)
         (SETRING* (RING_SUM R (RING_DEFINE V (DEGREEORDER* V) 'REVLEX '(1))))
         (SETQ CALI=DEGREES (MO_DEGNEWORDER CALI=DEGREES))
         (SETQ U1 (SETQ U2 (DP_FROM_A (CAR V))))
         (SETQ F (DP_NEWORDER (CAR N)))
         (PROG (X)
           (SETQ X N)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ F (DP_SUM F (DP_PROD U1 (DP_NEWORDER X))))
               (SETQ U1 (DP_PROD U1 U2))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (SETQ M1
                 (DPMAT_SIEVE (GBASIS* (QUOT=MQUOT (DPMAT_NEWORDER M NIL) F)) V
                  T))
         (SETRING* R)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (RETURN (DPMAT_NEWORDER M1 T))))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'ANNIHILATOR1* 'NUMBER-OF-ARGS 1) 
(PUT 'ANNIHILATOR1* 'DEFINED-ON-LINE '184) 
(PUT 'ANNIHILATOR1* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'ANNIHILATOR1* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANNIHILATOR1* (M)
    (COND ((EQUAL (DPMAT_COLS M) 0) M) ((EQUAL (DPMAT_COLS M) 1) M)
          (T
           (MODULEQUOTIENT1* M (DPMAT_UNIT (DPMAT_COLS M) (DPMAT_COLDEGS M)))))) 
(FLAG '(MATQQUOT) 'OPFN) 
(PUT 'MATQQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'MATQQUOT 'DEFINED-ON-LINE '192) 
(PUT 'MATQQUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATQQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATQQUOT (M F)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (COND
       ((OR (EQCAR F 'LIST) (EQCAR F 'MAT))
        (REDERR "Syntax : matquot(dpmat,dpoly)"))
       (T
        (DPMAT_2A
         (MATQQUOT* (DPMAT_FROM_A (REVAL1 M T)) (DP_FROM_A (REVAL1 F T)))))))
     (T (MATQQUOT* M F)))) 
(PUT 'MATQQUOT* 'NUMBER-OF-ARGS 2) 
(PUT 'MATQQUOT* 'DEFINED-ON-LINE '201) 
(PUT 'MATQQUOT* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATQQUOT* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATQQUOT* (M F)
    (COND ((DP_UNIT? F) M)
          ((EQUAL (DPMAT_COLS M) 0)
           (MAT2LIST* (QUOT=STABQUOT (IDEAL2MAT* M) (LIST F))))
          (T (QUOT=STABQUOT M (LIST F))))) 
(FLAG '(MATSTABQUOT) 'OPFN) 
(PUT 'MATSTABQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'MATSTABQUOT 'DEFINED-ON-LINE '208) 
(PUT 'MATSTABQUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATSTABQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATSTABQUOT (M F)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A
       (MATSTABQUOT* (DPMAT_FROM_A (REVAL1 M T)) (DPMAT_FROM_A (REVAL1 F T)))))
     (T (MATSTABQUOT* M F)))) 
(PUT 'MATSTABQUOT* 'NUMBER-OF-ARGS 2) 
(PUT 'MATSTABQUOT* 'DEFINED-ON-LINE '214) 
(PUT 'MATSTABQUOT* 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'MATSTABQUOT* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATSTABQUOT* (M F)
    (COND
     ((GREATERP (DPMAT_COLS F) 0) (REDERR "stable quotient only by ideals"))
     (T
      (PROG (C)
        (COND
         ((EQUAL (SETQ C (DPMAT_COLS M)) 0)
          (PROGN
           (SETQ F
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (DPMAT_LIST F))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (MATOP_PSEUDOMOD (BAS_DPOLY X) M))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ F
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X F)
                    STARTOVER
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ X (CDR X))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (X) (COND (X (LIST X)))) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ X (CDR X))
                     (GO LOOPLABEL)))))
         (T
          (SETQ F
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X (DPMAT_LIST F))
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (X) (BAS_DPOLY X)) (CAR X))
                                          NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (BAS_DPOLY X)) (CAR X)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
        (COND
         ((NULL F)
          (RETURN
           (COND ((EQUAL C 0) (DPMAT_FROM_DPOLY (DP_FI 1)))
                 (T (DPMAT_UNIT C (DPMAT_COLDEGS M)))))))
        (COND ((DP_UNIT? (CAR F)) (RETURN M)))
        (COND
         ((EQUAL C 0) (RETURN (MAT2LIST* (QUOT=STABQUOT (IDEAL2MAT* M) F))))
         (T (RETURN (QUOT=STABQUOT M F)))))))) 
(PUT 'QUOT=STABQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'QUOT=STABQUOT 'DEFINED-ON-LINE '231) 
(PUT 'QUOT=STABQUOT 'DEFINED-IN-FILE 'CALI/QUOT.RED) 
(PUT 'QUOT=STABQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOT=STABQUOT (M F)
    (COND ((EQUAL (DPMAT_COLS M) 0) (REDERR "quot_stabquot only for cols>0"))
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (M1 P P1 P2 V V1 V2 C)
                (SETQ V1 (MAKE_CALI_VARNAME))
                (SETQ V2 (MAKE_CALI_VARNAME))
                (SETQ V (LIST V1 V2))
                (SETRING*
                 (RING_SUM (SETQ C CALI=BASERING)
                  (RING_DEFINE V (DEGREEORDER* V) 'LEX '(1 1))))
                (SETQ CALI=DEGREES (MO_DEGNEWORDER (DPMAT_COLDEGS M)))
                (SETQ P1 (SETQ P2 (DP_FROM_A V1)))
                (SETQ F
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X F)
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X) (DP_NEWORDER X))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (DP_NEWORDER X)) (CAR X))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ P (CAR F))
                (PROG (X)
                  (SETQ X (CDR F))
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (PROGN
                      (SETQ P (DP_SUM (DP_PROD P1 X) P))
                      (SETQ P1 (DP_PROD P1 P2))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (SETQ P (DP_DIFF (DP_FI 1) (DP_PROD (DP_FROM_A V2) P)))
                (SETQ M1
                        (MATSUM*
                         (LIST (DPMAT_NEWORDER M NIL)
                               (DPMAT_TIMES_DPOLY P
                                (DPMAT_UNIT (DPMAT_COLS M) CALI=DEGREES)))))
                (SETQ M1 (DPMAT_SIEVE (GBASIS* M1) V T))
                (SETRING* C)
                (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
                (RETURN (DPMAT_NEWORDER M1 T))))
            CALI=DEGREES CALI=BASERING)))) 
(ENDMODULE) 