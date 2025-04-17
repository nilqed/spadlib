(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RING)) 
(PUT 'RING_DEFINE 'NUMBER-OF-ARGS 4) 
(PUT 'RING_DEFINE 'DEFINED-ON-LINE '59) 
(PUT 'RING_DEFINE 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_DEFINE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RING_DEFINE (N TO1 TYPE ECART)
    (LIST 'RING (CONS 'CALI=MK N) TO1 TYPE ECART)) 
(PUT 'SETRING* 'NUMBER-OF-ARGS 1) 
(PUT 'SETRING* 'DEFINED-ON-LINE '62) 
(PUT 'SETRING* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'SETRING* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETRING* (C)
    (PROG ()
      (COND
       ((AND *NOETHERIAN (NOT (RING_ISNOETHERIAN C)))
        (REDERR "term order is not noetherian")))
      (SETQ CALI=BASERING C)
      (SETKORDER (RING_ALL_NAMES C))
      (RETURN C))) 
(PUT 'SETECART* 'NUMBER-OF-ARGS 1) 
(PUT 'SETECART* 'DEFINED-ON-LINE '71) 
(PUT 'SETECART* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'SETECART* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETECART* (E)
    (PROG (R)
      (SETQ R CALI=BASERING)
      (COND
       ((NOT (RING_CHECKECART E (RING_NAMES R))) (TYPERR E "ecart vector"))
       (T
        (SETQ CALI=BASERING
                (RING_DEFINE (RING_NAMES R) (RING_DEGREES R) (RING_TAG R)
                 E)))))) 
(PUT 'RING_2A 'NUMBER-OF-ARGS 1) 
(PUT 'RING_2A 'DEFINED-ON-LINE '79) 
(PUT 'RING_2A 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_2A (C)
    (CONS 'LIST
          (LIST (CONS 'LIST (RING_NAMES C))
                (CONS 'LIST
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X (RING_DEGREES C))
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                      NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                (RING_TAG C) (CONS 'LIST (RING_ECART C))))) 
(PUT 'RING_FROM_A 'NUMBER-OF-ARGS 1) 
(PUT 'RING_FROM_A 'DEFINED-ON-LINE '84) 
(PUT 'RING_FROM_A 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_FROM_A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_FROM_A (U)
    (PROG (VARS TORD C R TAG ECART)
      (COND ((NOT (EQCAR U 'LIST)) (TYPERR U "ring")) (T (SETQ U (CDR U))))
      (SETQ VARS (REVAL1 (CAR U) T))
      (SETQ TORD (REVAL1 (CADR U) T))
      (SETQ TAG (REVAL1 (CADDR U) T))
      (COND ((EQUAL (LENGTH U) 4) (SETQ ECART (REVAL1 (CADDDR U) T))))
      (COND ((NOT (MEMQ TAG '(LEX REVLEX))) (TYPERR TAG "term order tag")))
      (COND ((NOT (EQCAR VARS 'LIST)) (TYPERR VARS "variable list"))
            (T (SETQ VARS (CDR VARS))))
      (COND ((EQUAL TORD (LIST 'LIST)) (SETQ C NIL))
            ((NOT (SETQ C (RING=TESTTORD VARS TORD)))
             (TYPERR TORD "term order degrees")))
      (COND
       ((NULL ECART)
        (COND
         ((OR (NULL TORD) (NOT (RING_CHECKECART (CAR TORD) VARS)))
          (SETQ ECART
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X VARS)
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
         (T (SETQ ECART (CAR TORD)))))
       ((NOT (RING_CHECKECART (CDR ECART) VARS)) (TYPERR ECART "ecart list"))
       (T (SETQ ECART (CDR ECART))))
      (SETQ R (RING_DEFINE VARS C TAG ECART))
      (COND
       ((AND *NOETHERIAN (NOT (RING_ISNOETHERIAN R)))
        (REDERR "Term order is non noetherian")))
      (RETURN R))) 
(PUT 'RING=TESTTORD 'NUMBER-OF-ARGS 2) 
(PUT 'RING=TESTTORD 'DEFINED-ON-LINE '108) 
(PUT 'RING=TESTTORD 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TESTTORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING=TESTTORD (VARS U)
    (COND
     ((AND (RING=LENGTHTEST (CDR U) (PLUS (LENGTH VARS) 1))
           (RING=CONTENTTEST (CDR U)))
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (CDR U))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T NIL))) 
(PUT 'RING=LENGTHTEST 'NUMBER-OF-ARGS 2) 
(PUT 'RING=LENGTHTEST 'DEFINED-ON-LINE '116) 
(PUT 'RING=LENGTHTEST 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=LENGTHTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING=LENGTHTEST (M V)
    (COND ((NULL M) T)
          (T
           (AND (EQCAR (CAR M) 'LIST) (EQUAL (LENGTH (CAR M)) V)
                (RING=LENGTHTEST (CDR M) V))))) 
(PUT 'RING=CONTENTTEST 'NUMBER-OF-ARGS 1) 
(PUT 'RING=CONTENTTEST 'DEFINED-ON-LINE '123) 
(PUT 'RING=CONTENTTEST 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=CONTENTTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=CONTENTTEST (M)
    (COND ((NULL M) T)
          (T (AND (NUMBERLISTP (CDAR M)) (RING=CONTENTTEST (CDR M)))))) 
(PUT 'RING_NAMES 'NUMBER-OF-ARGS 1) 
(PUT 'RING_NAMES 'DEFINED-ON-LINE '128) 
(PUT 'RING_NAMES 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_NAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_NAMES (R) (CDADR R)) 
(PUT 'RING_ALL_NAMES 'NUMBER-OF-ARGS 1) 
(PUT 'RING_ALL_NAMES 'DEFINED-ON-LINE '131) 
(PUT 'RING_ALL_NAMES 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_ALL_NAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_ALL_NAMES (R) (CADR R)) 
(PUT 'RING_DEGREES 'NUMBER-OF-ARGS 1) 
(PUT 'RING_DEGREES 'DEFINED-ON-LINE '133) 
(PUT 'RING_DEGREES 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_DEGREES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_DEGREES (R) (CADDR R)) 
(PUT 'RING_TAG 'NUMBER-OF-ARGS 1) 
(PUT 'RING_TAG 'DEFINED-ON-LINE '135) 
(PUT 'RING_TAG 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_TAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_TAG (R) (CADDDR R)) 
(PUT 'RING_ECART 'NUMBER-OF-ARGS 1) 
(PUT 'RING_ECART 'DEFINED-ON-LINE '137) 
(PUT 'RING_ECART 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_ECART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_ECART (R) (NTH R 5)) 
(PUT 'RING=TRANS 'NUMBER-OF-ARGS 1) 
(PUT 'RING=TRANS 'DEFINED-ON-LINE '141) 
(PUT 'RING=TRANS 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TRANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=TRANS (D)
    (COND ((OR (NULL D) (NULL (CAR D))) NIL)
          (T
           (CONS
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X D)
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (RING=TRANS
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X D)
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
              LOOPLABEL
               (SETQ X (CDR X))
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))) 
(PUT 'RING=TESTLEX 'NUMBER-OF-ARGS 1) 
(PUT 'RING=TESTLEX 'DEFINED-ON-LINE '147) 
(PUT 'RING=TESTLEX 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TESTLEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=TESTLEX (D)
    (COND ((NULL D) T)
          (T (AND (RING=TESTLEX1 (CAR D)) (RING=TESTLEX (CDR D)))))) 
(PUT 'RING=TESTLEX1 'NUMBER-OF-ARGS 1) 
(PUT 'RING=TESTLEX1 'DEFINED-ON-LINE '151) 
(PUT 'RING=TESTLEX1 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TESTLEX1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=TESTLEX1 (D)
    (COND ((NULL D) T) ((EQUAL (CAR D) 0) (RING=TESTLEX1 (CDR D)))
          (T (GREATERP (CAR D) 0)))) 
(PUT 'RING=TESTREVLEX 'NUMBER-OF-ARGS 1) 
(PUT 'RING=TESTREVLEX 'DEFINED-ON-LINE '156) 
(PUT 'RING=TESTREVLEX 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TESTREVLEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=TESTREVLEX (D)
    (COND ((NULL D) T)
          (T (AND (RING=TESTREVLEX1 (CAR D)) (RING=TESTREVLEX (CDR D)))))) 
(PUT 'RING=TESTREVLEX1 'NUMBER-OF-ARGS 1) 
(PUT 'RING=TESTREVLEX1 'DEFINED-ON-LINE '160) 
(PUT 'RING=TESTREVLEX1 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TESTREVLEX1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=TESTREVLEX1 (D)
    (COND ((NULL D) NIL) ((EQUAL (CAR D) 0) (RING=TESTREVLEX1 (CDR D)))
          (T (GREATERP (CAR D) 0)))) 
(PUT 'RING_ISNOETHERIAN 'NUMBER-OF-ARGS 1) 
(PUT 'RING_ISNOETHERIAN 'DEFINED-ON-LINE '165) 
(PUT 'RING_ISNOETHERIAN 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_ISNOETHERIAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING_ISNOETHERIAN (R)
    (COND
     ((EQUAL (RING_TAG R) 'REVLEX)
      (RING=TESTREVLEX (RING=TRANS (RING_DEGREES R))))
     (T (RING=TESTLEX (RING=TRANS (RING_DEGREES R)))))) 
(PUT 'RING=DEGPOS 'NUMBER-OF-ARGS 1) 
(PUT 'RING=DEGPOS 'DEFINED-ON-LINE '172) 
(PUT 'RING=DEGPOS 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=DEGPOS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RING=DEGPOS (D)
    (COND ((NULL D) T) (T (AND (GREATERP (CAR D) 0) (RING=DEGPOS (CDR D)))))) 
(PUT 'RING_CHECKECART 'NUMBER-OF-ARGS 2) 
(PUT 'RING_CHECKECART 'DEFINED-ON-LINE '176) 
(PUT 'RING_CHECKECART 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_CHECKECART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING_CHECKECART (E VARS)
    (AND (EQUAL (LENGTH E) (LENGTH VARS)) (RING=DEGPOS E))) 
(PUT 'NOETHERIAN 'SIMPFG '((T (RING=TEST)))) 
(PUT 'RING=TEST 'NUMBER-OF-ARGS 0) 
(PUT 'RING=TEST 'DEFINED-ON-LINE '183) 
(PUT 'RING=TEST 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING=TEST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RING=TEST NIL
    (COND
     ((NOT (RING_ISNOETHERIAN CALI=BASERING))
      (PROGN
       (SETQ *NOETHERIAN NIL)
       (REDERR "Current term order is not noetherian"))))) 
(FLAG '(ELIMINATIONORDER) 'OPFN) 
(PUT 'ELIMINATIONORDER 'NUMBER-OF-ARGS 2) 
(PUT 'ELIMINATIONORDER 'DEFINED-ON-LINE '192) 
(PUT 'ELIMINATIONORDER 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'ELIMINATIONORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIMINATIONORDER (V1 V2)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X
                      (ELIMINATIONORDER* (CDR (REVAL1 V1 T))
                       (CDR (REVAL1 V2 T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (ELIMINATIONORDER* V1 V2)))) 
(FLAG '(DEGREEORDER) 'OPFN) 
(PUT 'DEGREEORDER 'NUMBER-OF-ARGS 1) 
(PUT 'DEGREEORDER 'DEFINED-ON-LINE '201) 
(PUT 'DEGREEORDER 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'DEGREEORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEGREEORDER (VARS)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (DEGREEORDER* (CDR (REVAL1 VARS T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (DEGREEORDER* VARS)))) 
(FLAG '(LOCALORDER) 'OPFN) 
(PUT 'LOCALORDER 'NUMBER-OF-ARGS 1) 
(PUT 'LOCALORDER 'DEFINED-ON-LINE '208) 
(PUT 'LOCALORDER 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'LOCALORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOCALORDER (VARS)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (LOCALORDER* (CDR (REVAL1 VARS T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (LOCALORDER* VARS)))) 
(FLAG '(BLOCKORDER) 'OPFN) 
(PUT 'BLOCKORDER 'NUMBER-OF-ARGS 2) 
(PUT 'BLOCKORDER 'DEFINED-ON-LINE '215) 
(PUT 'BLOCKORDER 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'BLOCKORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BLOCKORDER (V1 V2)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (BLOCKORDER* (CDR (REVAL1 V1 T)) (CDR (REVAL1 V2 T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (CONS 'LIST X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (BLOCKORDER* V1 V2)))) 
(PUT 'BLOCKORDER* 'NUMBER-OF-ARGS 2) 
(PUT 'BLOCKORDER* 'DEFINED-ON-LINE '222) 
(PUT 'BLOCKORDER* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'BLOCKORDER* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BLOCKORDER* (VARS L)
    (COND
     ((NEQ
       (PROG (X FORALL-RESULT)
         (SETQ X L)
         (SETQ FORALL-RESULT 0)
        LAB1
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (SETQ FORALL-RESULT (PLUS ((LAMBDA (X) X) (CAR X)) FORALL-RESULT))
         (SETQ X (CDR X))
         (GO LAB1))
       (LENGTH VARS))
      (REDERR "block lengths sum doesn't match variable number"))
     (T
      (PROG (U PRE POST)
        (SETQ PRE 0)
        (SETQ POST 0)
        (SETQ PRE 0)
        (SETQ POST (LENGTH VARS))
        (PROG (X)
          (SETQ X L)
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (PROGN
              (SETQ U
                      (CONS
                       (APPEND
                        (APPEND
                         (PROG (I FORALL-RESULT FORALL-ENDPTR)
                           (SETQ I 1)
                           (COND ((MINUSP (DIFFERENCE PRE I)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                          LOOPLABEL
                           (SETQ I (PLUS2 I 1))
                           (COND
                            ((MINUSP (DIFFERENCE PRE I))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         (PROG (I FORALL-RESULT FORALL-ENDPTR)
                           (SETQ I 1)
                           (COND ((MINUSP (DIFFERENCE X I)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS 1 NIL)))
                          LOOPLABEL
                           (SETQ I (PLUS2 I 1))
                           (COND
                            ((MINUSP (DIFFERENCE X I)) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS 1 NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                        (PROG (I FORALL-RESULT FORALL-ENDPTR)
                          (SETQ I 1)
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE POST X) I))
                            (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                         LOOPLABEL
                          (SETQ I (PLUS2 I 1))
                          (COND
                           ((MINUSP (DIFFERENCE (DIFFERENCE POST X) I))
                            (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                       U))
              (SETQ PRE (PLUS PRE X))
              (SETQ POST (DIFFERENCE POST X))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (RETURN (REVERSIP U)))))) 
(PUT 'ELIMINATIONORDER* 'NUMBER-OF-ARGS 2) 
(PUT 'ELIMINATIONORDER* 'DEFINED-ON-LINE '237) 
(PUT 'ELIMINATIONORDER* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'ELIMINATIONORDER* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ELIMINATIONORDER* (V1 V2)
    (LIST
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X V1)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X) (COND ((MEMBER X V2) 1) (T 0))) (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (COND ((MEMBER X V2) 1) (T 0))) (CAR X))
                     NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X V1)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X) (COND ((MEMBER X V2) 0) (T 1))) (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (COND ((MEMBER X V2) 0) (T 1))) (CAR X))
                     NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'DEGREEORDER* 'NUMBER-OF-ARGS 1) 
(PUT 'DEGREEORDER* 'DEFINED-ON-LINE '245) 
(PUT 'DEGREEORDER* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'DEGREEORDER* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEGREEORDER* (VARS)
    (LIST
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X VARS)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LOCALORDER* 'NUMBER-OF-ARGS 1) 
(PUT 'LOCALORDER* 'DEFINED-ON-LINE '248) 
(PUT 'LOCALORDER* 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'LOCALORDER* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOCALORDER* (VARS)
    (LIST
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X VARS)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (MINUS 1)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (MINUS 1)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'RING_RLP 'NUMBER-OF-ARGS 2) 
(PUT 'RING_RLP 'DEFINED-ON-LINE '253) 
(PUT 'RING_RLP 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_RLP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING_RLP (R U)
    (RING_DEFINE (RING_NAMES R)
     (CONS
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (RING_NAMES R))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (COND ((MEMBER X U) 1) (T 0))) (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (COND ((MEMBER X U) 1) (T 0))) (CAR X))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (APPEND
       (REVERSE
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X U)
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                (SETQ Y (RING_NAMES R))
                                (COND ((NULL Y) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Y)
                                                    (COND
                                                     ((EQUAL X Y) (MINUS 1))
                                                     (T 0)))
                                                  (CAR Y))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ Y (CDR Y))
                                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Y)
                                            (COND ((EQUAL X Y) (MINUS 1))
                                                  (T 0)))
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
                        (SETQ Y (RING_NAMES R))
                        (COND ((NULL Y) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Y)
                                            (COND ((EQUAL X Y) (MINUS 1))
                                                  (T 0)))
                                          (CAR Y))
                                         NIL)))
                       LOOPLABEL
                        (SETQ Y (CDR Y))
                        (COND ((NULL Y) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y)
                                    (COND ((EQUAL X Y) (MINUS 1)) (T 0)))
                                  (CAR Y))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                    (CAR X))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))
       (RING_DEGREES R)))
     (RING_TAG R) (RING_ECART R))) 
(PUT 'RING_LP 'NUMBER-OF-ARGS 2) 
(PUT 'RING_LP 'DEFINED-ON-LINE '262) 
(PUT 'RING_LP 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_LP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING_LP (R U)
    (RING_DEFINE (RING_NAMES R)
     (APPEND
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X U)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                              (SETQ Y (RING_NAMES R))
                              (COND ((NULL Y) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y)
                                                  (COND ((EQUAL X Y) 1) (T 0)))
                                                (CAR Y))
                                               NIL)))
                             LOOPLABEL
                              (SETQ Y (CDR Y))
                              (COND ((NULL Y) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (COND ((EQUAL X Y) 1) (T 0)))
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
                      (SETQ Y (RING_NAMES R))
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (COND ((EQUAL X Y) 1) (T 0)))
                                        (CAR Y))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Y) (COND ((EQUAL X Y) 1) (T 0)))
                                (CAR Y))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR X))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (RING_DEGREES R))
     (RING_TAG R) (RING_ECART R))) 
(PUT 'RING_SUM 'NUMBER-OF-ARGS 2) 
(PUT 'RING_SUM 'DEFINED-ON-LINE '270) 
(PUT 'RING_SUM 'DEFINED-IN-FILE 'CALI/RING.RED) 
(PUT 'RING_SUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RING_SUM (A B)
    (PROG (VARS ZEROA ZEROB DEGS ECART)
      (COND
       ((NOT (DISJOINT (RING_NAMES A) (RING_NAMES B)))
        (REDERR "RINGSUM only for disjoint variable sets")))
      (SETQ VARS (APPEND (RING_NAMES A) (RING_NAMES B)))
      (SETQ ECART (APPEND (RING_ECART A) (RING_ECART B)))
      (SETQ ZEROA
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (RING_NAMES A))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) 0) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 0) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ZEROB
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (RING_NAMES B))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) 0) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 0) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DEGS
              (APPEND
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (RING_DEGREES B))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (APPEND ZEROA X)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (APPEND ZEROA X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (RING_DEGREES A))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (APPEND X ZEROB)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (APPEND X ZEROB)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN (RING_DEFINE VARS DEGS (RING_TAG A) ECART)))) 
(SETRING* (RING_DEFINE '(T X Y Z) '((1 1 1 1)) 'REVLEX '(1 1 1 1))) 
(SETQ *NOETHERIAN T) 
(ENDMODULE) 