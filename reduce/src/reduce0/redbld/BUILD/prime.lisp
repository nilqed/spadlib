(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PRIME)) 
(SWITCH (LIST 'FACTORPRIMES)) 
(SETQ *FACTORPRIMES T) 
(PUT 'PRIME=MKSQRFREE 'NUMBER-OF-ARGS 2) 
(PUT 'PRIME=MKSQRFREE 'DEFINED-ON-LINE '96) 
(PUT 'PRIME=MKSQRFREE 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=MKSQRFREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIME=MKSQRFREE (POL X)
    (PROG (P)
      (SETQ P (CAR (SIMP (DP_2A POL))))
      (RETURN (DP_FROM_A (PREPF (CAR (QREMF P (GCDF* P (CAR (DIFFF P X)))))))))) 
(PUT 'ZERORADICAL 'PSOPFN 'PRIME=EVZERO) 
(PUT 'PRIME=EVZERO 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=EVZERO 'DEFINED-ON-LINE '104) 
(PUT 'PRIME=EVZERO 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=EVZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=EVZERO (M)
    (PROG (C)
      (INTF_TEST M)
      (INTF_GET (SETQ M (CAR M)))
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (RETURN (DPMAT_2A (ZERORADICAL* C))))) 
(PUT 'ZERORADICAL* 'NUMBER-OF-ARGS 1) 
(PUT 'ZERORADICAL* 'DEFINED-ON-LINE '112) 
(PUT 'ZERORADICAL* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ZERORADICAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZERORADICAL* (M)
    (COND
     ((OR (GREATERP (DPMAT_COLS M) 0) (NOT (DIMZEROP* M)))
      (REDERR "ZERORADICAL only for zerodimensional ideals"))
     ((DPMAT_UNITIDEAL? M) M)
     (T
      (PROG (U)
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (RING_NAMES CALI=BASERING))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (BAS_MAKE 0
                                       (PRIME=MKSQRFREE (ODIM_UP X M) X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (BAS_MAKE 0 (PRIME=MKSQRFREE (ODIM_UP X M) X)))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ U (DPMAT_MAKE (LENGTH U) 0 (BAS_RENUMBER U) NIL NIL))
        (RETURN (GBASIS* (MATSUM* (LIST M U)))))))) 
(PUT 'ISZERORADICAL 'PSOPFN 'PRIME=EVISZERO) 
(PUT 'PRIME=EVISZERO 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=EVISZERO 'DEFINED-ON-LINE '125) 
(PUT 'PRIME=EVISZERO 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=EVISZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=EVISZERO (M)
    (PROG (C)
      (INTF_TEST M)
      (INTF_GET (SETQ M (CAR M)))
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (RETURN (COND ((ISZERORADICAL* C) 'YES) (T 'NO))))) 
(PUT 'ISZERORADICAL* 'NUMBER-OF-ARGS 1) 
(PUT 'ISZERORADICAL* 'DEFINED-ON-LINE '133) 
(PUT 'ISZERORADICAL* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ISZERORADICAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISZERORADICAL* (M)
    (COND
     ((OR (GREATERP (DPMAT_COLS M) 0) (NOT (DIMZEROP* M)))
      (REDERR "ISZERORADICAL only for zerodimensional ideals"))
     ((DPMAT_UNITIDEAL? M) T)
     (T
      (PROG (ISRADICAL)
        (SETQ ISRADICAL T)
        (PROG (X)
          (SETQ X (RING_NAMES CALI=BASERING))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X)
             (SETQ ISRADICAL
                     (AND ISRADICAL
                          (NULL
                           (MATOP_PSEUDOMOD (PRIME=MKSQRFREE (ODIM_UP X M) X)
                            M)))))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))
        (RETURN ISRADICAL))))) 
(FLAG '(ZEROPRIMES) 'OPFN) 
(PUT 'ZEROPRIMES 'NUMBER-OF-ARGS 1) 
(PUT 'ZEROPRIMES 'DEFINED-ON-LINE '149) 
(PUT 'ZEROPRIMES 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ZEROPRIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZEROPRIMES (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (ZEROPRIMES* (DPMAT_FROM_A (REVAL1 M T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (ZEROPRIMES* M)))) 
(PUT 'ZEROPRIMES* 'NUMBER-OF-ARGS 1) 
(PUT 'ZEROPRIMES* 'DEFINED-ON-LINE '155) 
(PUT 'ZEROPRIMES* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ZEROPRIMES* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZEROPRIMES* (M)
    (LISTMINIMIZE
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X
               (COND (*FACTORPRIMES (GROEBF_ZEROPRIMES1 (ANNIHILATOR2* M) NIL))
                     (T (PRIME_ZEROPRIMES1 (GBASIS* (ANNIHILATOR2* M))))))
      STARTOVER
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ X (CDR X))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ X (CDR X))
       (GO LOOPLABEL))
     (FUNCTION SUBMODULEP*))) 
(PUT 'PRIME_ISZEROPRIME 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME_ISZEROPRIME 'DEFINED-ON-LINE '163) 
(PUT 'PRIME_ISZEROPRIME 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME_ISZEROPRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME_ISZEROPRIME (M)
    (COND
     ((OR (GREATERP (DPMAT_COLS M) 0) (NOT (DIMZEROP* M)))
      (REDERR "iszeroprime only for zerodimensional ideals"))
     ((DPMAT_UNITIDEAL? M) (REDERR "the ideal is the unit ideal"))
     (T (AND (PRIME=ISZEROPRIME1 M) (PRIME=ISZEROPRIME2 M))))) 
(PUT 'PRIME_ZEROPRIMES1 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME_ZEROPRIMES1 'DEFINED-ON-LINE '170) 
(PUT 'PRIME_ZEROPRIMES1 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME_ZEROPRIMES1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME_ZEROPRIMES1 (M)
    (COND ((GREATERP (DPMAT_COLS M) 0) (REDERR "ZEROPRIMES only for ideals"))
          ((DPMAT_UNITIDEAL? M) NIL)
          ((NOT (DIMZEROP* M))
           (REDERR "ZEROPRIMES only for zerodimensional ideals"))
          (T
           (PROG (L)
             (SETQ L (LIST M))
             (PROG (X)
               (SETQ X (RING_NAMES CALI=BASERING))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (SETQ L
                          (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                            (SETQ Y L)
                           STARTOVER
                            (COND ((NULL Y) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (Y)
                                       (COND
                                        ((NOT
                                          (MEMBER X
                                                  (PROG (V FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ V (DPMAT_LIST Y))
                                                   STARTOVER
                                                    (COND
                                                     ((NULL V) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            ((LAMBDA (V)
                                                               (LIST
                                                                (MO_LINEAR
                                                                 (DP_LMON
                                                                  (BAS_DPOLY
                                                                   V)))))
                                                             (CAR V)))
                                                    (SETQ FORALL-ENDPTR
                                                            (LASTPAIR
                                                             FORALL-RESULT))
                                                    (SETQ V (CDR V))
                                                    (COND
                                                     ((ATOM FORALL-ENDPTR)
                                                      (GO STARTOVER)))
                                                   LOOPLABEL
                                                    (COND
                                                     ((NULL V)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            ((LAMBDA (V)
                                                               (LIST
                                                                (MO_LINEAR
                                                                 (DP_LMON
                                                                  (BAS_DPOLY
                                                                   V)))))
                                                             (CAR V)))
                                                    (SETQ FORALL-ENDPTR
                                                            (LASTPAIR
                                                             FORALL-ENDPTR))
                                                    (SETQ V (CDR V))
                                                    (GO LOOPLABEL))))
                                         (PROG (U P)
                                           (SETQ U
                                                   (DP_FACTOR
                                                    (SETQ P (ODIM_UP X Y))))
                                           (COND
                                            ((AND (EQUAL (LENGTH U) 1)
                                                  (EQUAL (CAR U) P))
                                             (RETURN (LIST Y)))
                                            (T
                                             (RETURN
                                              (PROG (Z FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ Z U)
                                               STARTOVER
                                                (COND ((NULL Z) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        ((LAMBDA (Z)
                                                           (COND
                                                            ((NOT
                                                              (DPMAT_UNITIDEAL?
                                                               (SETQ P
                                                                       (GBASIS*
                                                                        (MATSUM*
                                                                         (LIST
                                                                          Y
                                                                          (DPMAT_FROM_DPOLY
                                                                           Z)))))))
                                                             (LIST P))))
                                                         (CAR Z)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-RESULT))
                                                (SETQ Z (CDR Z))
                                                (COND
                                                 ((ATOM FORALL-ENDPTR)
                                                  (GO STARTOVER)))
                                               LOOPLABEL
                                                (COND
                                                 ((NULL Z)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        ((LAMBDA (Z)
                                                           (COND
                                                            ((NOT
                                                              (DPMAT_UNITIDEAL?
                                                               (SETQ P
                                                                       (GBASIS*
                                                                        (MATSUM*
                                                                         (LIST
                                                                          Y
                                                                          (DPMAT_FROM_DPOLY
                                                                           Z)))))))
                                                             (LIST P))))
                                                         (CAR Z)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-ENDPTR))
                                                (SETQ Z (CDR Z))
                                                (GO LOOPLABEL)))))))
                                        (T (LIST Y))))
                                     (CAR Y)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ Y (CDR Y))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL Y) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (Y)
                                       (COND
                                        ((NOT
                                          (MEMBER X
                                                  (PROG (V FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ V (DPMAT_LIST Y))
                                                   STARTOVER
                                                    (COND
                                                     ((NULL V) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            ((LAMBDA (V)
                                                               (LIST
                                                                (MO_LINEAR
                                                                 (DP_LMON
                                                                  (BAS_DPOLY
                                                                   V)))))
                                                             (CAR V)))
                                                    (SETQ FORALL-ENDPTR
                                                            (LASTPAIR
                                                             FORALL-RESULT))
                                                    (SETQ V (CDR V))
                                                    (COND
                                                     ((ATOM FORALL-ENDPTR)
                                                      (GO STARTOVER)))
                                                   LOOPLABEL
                                                    (COND
                                                     ((NULL V)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            ((LAMBDA (V)
                                                               (LIST
                                                                (MO_LINEAR
                                                                 (DP_LMON
                                                                  (BAS_DPOLY
                                                                   V)))))
                                                             (CAR V)))
                                                    (SETQ FORALL-ENDPTR
                                                            (LASTPAIR
                                                             FORALL-ENDPTR))
                                                    (SETQ V (CDR V))
                                                    (GO LOOPLABEL))))
                                         (PROG (U P)
                                           (SETQ U
                                                   (DP_FACTOR
                                                    (SETQ P (ODIM_UP X Y))))
                                           (COND
                                            ((AND (EQUAL (LENGTH U) 1)
                                                  (EQUAL (CAR U) P))
                                             (RETURN (LIST Y)))
                                            (T
                                             (RETURN
                                              (PROG (Z FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ Z U)
                                               STARTOVER
                                                (COND ((NULL Z) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        ((LAMBDA (Z)
                                                           (COND
                                                            ((NOT
                                                              (DPMAT_UNITIDEAL?
                                                               (SETQ P
                                                                       (GBASIS*
                                                                        (MATSUM*
                                                                         (LIST
                                                                          Y
                                                                          (DPMAT_FROM_DPOLY
                                                                           Z)))))))
                                                             (LIST P))))
                                                         (CAR Z)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-RESULT))
                                                (SETQ Z (CDR Z))
                                                (COND
                                                 ((ATOM FORALL-ENDPTR)
                                                  (GO STARTOVER)))
                                               LOOPLABEL
                                                (COND
                                                 ((NULL Z)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        ((LAMBDA (Z)
                                                           (COND
                                                            ((NOT
                                                              (DPMAT_UNITIDEAL?
                                                               (SETQ P
                                                                       (GBASIS*
                                                                        (MATSUM*
                                                                         (LIST
                                                                          Y
                                                                          (DPMAT_FROM_DPOLY
                                                                           Z)))))))
                                                             (LIST P))))
                                                         (CAR Z)))
                                                (SETQ FORALL-ENDPTR
                                                        (LASTPAIR
                                                         FORALL-ENDPTR))
                                                (SETQ Z (CDR Z))
                                                (GO LOOPLABEL)))))))
                                        (T (LIST Y))))
                                     (CAR Y)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ Y (CDR Y))
                            (GO LOOPLABEL))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN L))))) 
(PUT 'PRIME=ISZEROPRIME1 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=ISZEROPRIME1 'DEFINED-ON-LINE '195) 
(PUT 'PRIME=ISZEROPRIME1 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ISZEROPRIME1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=ISZEROPRIME1 (M)
    (COND ((GREATERP (DPMAT_COLS M) 0) (REDERR "ISZEROPRIME only for ideals"))
          ((DPMAT_UNITIDEAL? M) NIL)
          ((NOT (DIMZEROP* M))
           (REDERR "ISZEROPRIME only for zerodimensional ideals"))
          (T
           (PROG (B)
             (SETQ B T)
             (PROG (X)
               (SETQ X (RING_NAMES CALI=BASERING))
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (SETQ B
                          (AND B
                               (PROG (U P)
                                 (SETQ U (DP_FACTOR (SETQ P (ODIM_UP X M))))
                                 (COND
                                  ((AND (EQUAL (LENGTH U) 1) (EQUAL (CAR U) P))
                                   (RETURN T))
                                  (T (RETURN NIL)))))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN B))))) 
(PUT 'PRIME_GPCHANGE 'NUMBER-OF-ARGS 3) 
(PUT 'PRIME_GPCHANGE 'DEFINED-ON-LINE '212) 
(PUT 'PRIME_GPCHANGE 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME_GPCHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRIME_GPCHANGE (VARS V M)
    (COND ((OR (NULL VARS) (DPMAT_UNITIDEAL? M)) M)
          (T
           (PROG (S X A)
             (SETQ S 0)
             (SETQ X (MO_FROM_A (CAR VARS)))
             (SETQ A
                     (LIST
                      (CONS V
                            (PREPF
                             (ADDF (LIST (CONS (CONS V 1) 1))
                                   (LIST (CONS (CONS (CAR VARS) 1) 1)))))))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (NOT (MEMBER X (MOID_FROM_BAS (DPMAT_LIST M))))
                       (LESSP (SETQ S (PLUS S 1)) 10)))
                 (RETURN NIL)))
               (SETQ M (GBASIS* (DPMAT_SUB A M)))
               (GO WHILELABEL))
             (COND
              ((EQUAL S 10) (REDERR " change to general position failed")))
             (RETURN (PRIME_GPCHANGE (CDR VARS) V M)))))) 
(PUT 'PRIME=ZEROPRIMES2 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=ZEROPRIMES2 'DEFINED-ON-LINE '229) 
(PUT 'PRIME=ZEROPRIMES2 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ZEROPRIMES2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=ZEROPRIMES2 (M)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (C V VARS U D R)
         (SETQ C CALI=BASERING)
         (SETQ VARS (RING_NAMES C))
         (SETQ V (MAKE_CALI_VARNAME))
         (SETQ U
                 (SETDIFF VARS
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X (MOID_FROM_BAS (DPMAT_LIST M)))
                           STARTOVER
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (X) (LIST (MO_LINEAR X)))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ X (CDR X))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (X) (LIST (MO_LINEAR X)))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ X (CDR X))
                            (GO LOOPLABEL))))
         (COND ((EQUAL (LENGTH U) 1) (RETURN (PRIME=ZEROPRIMES3 M (FIRST U)))))
         (COND
          ((EQUAL (RING_TAG C) 'REVLEX)
           (SETQ R (RING_DEFINE VARS (RING_DEGREES C) 'LEX (RING_ECART C))))
          (T (SETQ R C)))
         (SETRING* (RING_SUM R (RING_DEFINE (LIST V) NIL 'LEX '(1))))
         (SETQ CALI=DEGREES NIL)
         (SETQ M
                 (GBASIS*
                  (MATSUM*
                   (LIST (DPMAT_NEWORDER M NIL)
                         (DPMAT_FROM_DPOLY (DP_FROM_A V))))))
         (SETQ U
                 (SETDIFF (CONS V VARS)
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X (MOID_FROM_BAS (DPMAT_LIST M)))
                           STARTOVER
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (X) (LIST (MO_LINEAR X)))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ X (CDR X))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (X) (LIST (MO_LINEAR X)))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ X (CDR X))
                            (GO LOOPLABEL))))
         (COND
          ((NOT (DPMAT_UNITIDEAL? M))
           (PROGN
            (SETQ M (PRIME_GPCHANGE U V M))
            (SETQ U
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (PRIME=ZEROPRIMES3 M V))
                     STARTOVER
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (X)
                                 (COND
                                  ((AND (NOT (DPMAT_UNITIDEAL? X))
                                        (NOT
                                         (DPMAT_UNITIDEAL?
                                          (SETQ D (ELIMINATE* X (LIST V))))))
                                   (LIST D))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ X (CDR X))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (X)
                                 (COND
                                  ((AND (NOT (DPMAT_UNITIDEAL? X))
                                        (NOT
                                         (DPMAT_UNITIDEAL?
                                          (SETQ D (ELIMINATE* X (LIST V))))))
                                   (LIST D))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ X (CDR X))
                      (GO LOOPLABEL)))))
          (T (SETQ U NIL)))
         (SETRING* C)
         (RETURN
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X U)
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X) (GBASIS* (DPMAT_NEWORDER X NIL)))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X) (GBASIS* (DPMAT_NEWORDER X NIL))) (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'PRIME=ZEROPRIMES3 'NUMBER-OF-ARGS 2) 
(PUT 'PRIME=ZEROPRIMES3 'DEFINED-ON-LINE '260) 
(PUT 'PRIME=ZEROPRIMES3 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ZEROPRIMES3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIME=ZEROPRIMES3 (M V)
    (PROG (U P)
      (SETQ U (DPMAT_LIST M))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND U
                (NOT
                 (EQUAL (MO_SUPPORT (DP_LMON (SETQ P (BAS_DPOLY (CAR U)))))
                        (LIST V)))))
          (RETURN NIL)))
        (SETQ U (CDR U))
        (GO WHILELABEL))
      (COND ((NULL U) (REDERR "univariate polynomial not found")))
      (SETQ P
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X
                        (CDR
                         ((LAMBDA (*FACTOR) (FCTRF (CAR (SIMP (DP_2A P)))))
                          T)))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (DPMAT_FROM_DPOLY
                                     (DP_FROM_A (PREPF (CAR X)))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (DPMAT_FROM_DPOLY (DP_FROM_A (PREPF (CAR X)))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X P)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (MATSUM* (LIST M X))) (CAR X))
                               NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (MATSUM* (LIST M X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'PRIME=ISZEROPRIME2 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=ISZEROPRIME2 'DEFINED-ON-LINE '272) 
(PUT 'PRIME=ISZEROPRIME2 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ISZEROPRIME2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=ISZEROPRIME2 (M)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (C V VARS U R)
         (SETQ C CALI=BASERING)
         (SETQ VARS (RING_NAMES C))
         (SETQ V (MAKE_CALI_VARNAME))
         (COND
          ((EQUAL (RING_TAG C) 'REVLEX)
           (SETQ R (RING_DEFINE VARS (RING_DEGREES C) 'LEX (RING_ECART C))))
          (T (SETQ R C)))
         (SETRING* (RING_SUM R (RING_DEFINE (LIST V) NIL 'LEX '(1))))
         (SETQ CALI=DEGREES NIL)
         (SETQ M
                 (MATSUM*
                  (LIST (DPMAT_NEWORDER M NIL)
                        (DPMAT_FROM_DPOLY (DP_FROM_A V)))))
         (SETQ M (PRIME_GPCHANGE VARS V (GBASIS* M)))
         (SETQ U (PRIME=ISZEROPRIME3 M V))
         (SETRING* C)
         (RETURN U)))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'PRIME=ISZEROPRIME3 'NUMBER-OF-ARGS 2) 
(PUT 'PRIME=ISZEROPRIME3 'DEFINED-ON-LINE '290) 
(PUT 'PRIME=ISZEROPRIME3 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ISZEROPRIME3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIME=ISZEROPRIME3 (M V)
    (PROG (U P)
      (SETQ U (DPMAT_LIST M))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND U
                (NOT
                 (EQUAL (MO_SUPPORT (DP_LMON (SETQ P (BAS_DPOLY (CAR U)))))
                        (LIST V)))))
          (RETURN NIL)))
        (SETQ U (CDR U))
        (GO WHILELABEL))
      (COND ((NULL U) (REDERR "univariate polynomial not found")))
      (COND
       ((OR
         (GREATERP
          (LENGTH
           (SETQ U
                   (CDR
                    ((LAMBDA (*FACTOR) (FCTRF (CAR (SIMP (DP_2A P))))) T))))
          1)
         (GREATERP (CDAR U) 1))
        (RETURN NIL))
       (T (RETURN T))))) 
(PUT 'ISPRIME 'PSOPFN 'PRIME=ISPRIME) 
(PUT 'PRIME=ISPRIME 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=ISPRIME 'DEFINED-ON-LINE '304) 
(PUT 'PRIME=ISPRIME 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ISPRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=ISPRIME (M)
    (PROG (C)
      (INTF_TEST M)
      (INTF_GET (SETQ M (CAR M)))
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (RETURN (COND ((ISPRIME* C) 'YES) (T 'NO))))) 
(PUT 'ISPRIME* 'NUMBER-OF-ARGS 1) 
(PUT 'ISPRIME* 'DEFINED-ON-LINE '312) 
(PUT 'ISPRIME* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ISPRIME* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISPRIME* (M)
    (COND ((GREATERP (DPMAT_COLS M) 0) (REDERR "prime test only for ideals"))
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (VARS U V C1 C2 M1 M2 LC)
                (SETQ V (MOID_GOODINDEPVARSET M))
                (SETQ CALI=DEGREES NIL)
                (COND ((NULL V) (RETURN (PRIME_ISZEROPRIME M))))
                (SETQ VARS (RING_NAMES (SETQ C1 CALI=BASERING)))
                (SETQ U (SETDIFF (RING_NAMES C1) V))
                (SETRING* (RING_RLP C1 U))
                (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
                (SETRING*
                 (SETQ C2
                         (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X U)
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS ((LAMBDA (X) 1) (CAR X))
                                                  NIL)))
                           LOOPLABEL
                            (SETQ X (CDR X))
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
                (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1)))
                (COND
                 ((DPMAT_UNITIDEAL? M1)
                  (PROGN (SETRING* C1) (REDERR "Input must be a gbasis"))))
                (SETQ LC (CALI_BC_2A (PRIME=QUOT M1)))
                (SETRING* C1)
                (SETQ M2 (GBASIS* (MATQQUOT* M (DP_FROM_A LC))))
                (COND ((NOT (SUBMODULEP* M2 M)) (RETURN NIL)))
                (SETRING* C2)
                (SETQ U (PRIME_ISZEROPRIME M1))
                (SETRING* C1)
                (RETURN U)))
            CALI=DEGREES CALI=BASERING)))) 
(FLAG '(ISOLATEDPRIMES) 'OPFN) 
(PUT 'ISOLATEDPRIMES 'NUMBER-OF-ARGS 1) 
(PUT 'ISOLATEDPRIMES 'DEFINED-ON-LINE '340) 
(PUT 'ISOLATEDPRIMES 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ISOLATEDPRIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISOLATEDPRIMES (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (ISOLATEDPRIMES* (DPMAT_FROM_A (REVAL1 M T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (DPMAT_2A X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (ISOLATEDPRIMES* M)))) 
(PUT 'ISOLATEDPRIMES* 'NUMBER-OF-ARGS 1) 
(PUT 'ISOLATEDPRIMES* 'DEFINED-ON-LINE '346) 
(PUT 'ISOLATEDPRIMES* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ISOLATEDPRIMES* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISOLATEDPRIMES* (M)
    (COND
     (*FACTORPRIMES
      (LISTMINIMIZE
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (GROEBFACTOR* (ANNIHILATOR2* M) NIL))
        STARTOVER
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (X) (PRIME=FACTORISOPRIMES (CAR X))) (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ X (CDR X))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (X) (PRIME=FACTORISOPRIMES (CAR X))) (CAR X)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ X (CDR X))
         (GO LOOPLABEL))
       (FUNCTION SUBMODULEP*)))
     (T (PRIME=ISOPRIMES (GBASIS* (ANNIHILATOR2* M)))))) 
(PUT 'PRIME=ISOPRIMES 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=ISOPRIMES 'DEFINED-ON-LINE '355) 
(PUT 'PRIME=ISOPRIMES 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=ISOPRIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=ISOPRIMES (M)
    (COND ((DPMAT_ZERO? M) NIL)
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (U C V VARS M1 M2 L P)
                (COND
                 ((NULL (SETQ V (ODIM_PARAMETER M)))
                  (RETURN
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (PRIME_ZEROPRIMES1 M))
                    STARTOVER
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ X (CDR X))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ X (CDR X))
                     (GO LOOPLABEL)))))
                (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
                (SETQ CALI=DEGREES NIL)
                (SETQ U (DELETE V VARS))
                (SETRING* (RING_RLP C U))
                (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
                (SETRING*
                 (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X U)
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
                (SETQ P
                        (CALI_BC_2A
                         (PRIME=QUOT
                          (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
                (SETQ L
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X (PRIME=ISOPRIMES M1))
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X)
                                              (CONS (DPMAT_2A X)
                                                    (CALI_BC_2A
                                                     (PRIME=QUOT X))))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (CONS (DPMAT_2A X)
                                            (CALI_BC_2A (PRIME=QUOT X))))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETRING* C)
                (SETQ L
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X L)
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X)
                                              (GBASIS*
                                               (MATQQUOT*
                                                (DPMAT_FROM_A (CAR X))
                                                (DP_FROM_A (CDR X)))))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (GBASIS*
                                       (MATQQUOT* (DPMAT_FROM_A (CAR X))
                                        (DP_FROM_A (CDR X)))))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (COND
                 ((OR (DP_UNIT? (SETQ P (DP_FROM_A P)))
                      (SUBMODULEP* (MATQQUOT* M P) M)
                      (DPMAT_UNITIDEAL?
                       (SETQ M2
                               (GBASIS*
                                (MATSUM* (LIST M (DPMAT_FROM_DPOLY P)))))))
                  (RETURN L))
                 (T
                  (RETURN
                   (LISTMINIMIZE (APPEND L (PRIME=ISOPRIMES M2))
                                 (FUNCTION SUBMODULEP*)))))))
            CALI=DEGREES CALI=BASERING)))) 
(PUT 'PRIME=FACTORISOPRIMES 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=FACTORISOPRIMES 'DEFINED-ON-LINE '384) 
(PUT 'PRIME=FACTORISOPRIMES 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=FACTORISOPRIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=FACTORISOPRIMES (M)
    (COND ((DPMAT_ZERO? M) NIL)
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (U C V VARS M1 M2 L P)
                (COND
                 ((NULL (SETQ V (ODIM_PARAMETER M)))
                  (RETURN
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (GROEBF_ZEROPRIMES1 M NIL))
                    STARTOVER
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ X (CDR X))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (X) (PRIME=ZEROPRIMES2 X)) (CAR X)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ X (CDR X))
                     (GO LOOPLABEL)))))
                (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
                (SETQ CALI=DEGREES NIL)
                (SETQ U (DELETE V VARS))
                (SETRING* (RING_RLP C U))
                (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
                (SETRING*
                 (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X U)
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
                (SETQ P
                        (CALI_BC_2A
                         (PRIME=QUOT
                          (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
                (SETQ L
                        (PROG (X FORALL-RESULT FORALL-ENDPTR)
                          (SETQ X (PRIME=FACTORISOPRIMES M1))
                          (COND ((NULL X) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (X)
                                              (CONS (DPMAT_2A X)
                                                    (CALI_BC_2A
                                                     (PRIME=QUOT X))))
                                            (CAR X))
                                           NIL)))
                         LOOPLABEL
                          (SETQ X (CDR X))
                          (COND ((NULL X) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (CONS (DPMAT_2A X)
                                            (CALI_BC_2A (PRIME=QUOT X))))
                                    (CAR X))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETRING* C)
                (SETQ L
                        (LISTGROEBFACTOR*
                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                           (SETQ X L)
                           (COND ((NULL X) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (X)
                                               (MATQQUOT*
                                                (DPMAT_FROM_A (CAR X))
                                                (DP_FROM_A (CDR X))))
                                             (CAR X))
                                            NIL)))
                          LOOPLABEL
                           (SETQ X (CDR X))
                           (COND ((NULL X) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (MATQQUOT* (DPMAT_FROM_A (CAR X))
                                        (DP_FROM_A (CDR X))))
                                     (CAR X))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (COND
                 ((OR (DP_UNIT? (SETQ P (DP_FROM_A P)))
                      (SUBMODULEP* (MATQQUOT* M P) M)
                      (NULL
                       (SETQ M2
                               (GROEBFACTOR*
                                (MATSUM* (LIST M (DPMAT_FROM_DPOLY P))) NIL))))
                  (RETURN L))
                 (T
                  (RETURN
                   (LISTMINIMIZE
                    (APPEND L
                            (PROG (X FORALL-RESULT FORALL-ENDPTR)
                              (SETQ X M2)
                             STARTOVER
                              (COND ((NULL X) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (X)
                                         (PRIME=FACTORISOPRIMES (CAR X)))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ X (CDR X))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL X) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (X)
                                         (PRIME=FACTORISOPRIMES (CAR X)))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ X (CDR X))
                              (GO LOOPLABEL)))
                    (FUNCTION SUBMODULEP*)))))))
            CALI=DEGREES CALI=BASERING)))) 
(PUT 'PRIME=QUOT 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=QUOT 'DEFINED-ON-LINE '414) 
(PUT 'PRIME=QUOT 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=QUOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=QUOT (M)
    (PROG (P U)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (DPMAT_LIST M))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (DP_LC (BAS_DPOLY X))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (DP_LC (BAS_DPOLY X))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL U) (RETURN (CALI_BC_FI 1))))
      (SETQ P (CAR U))
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ P (CALI_BC_LCM P X))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN P))) 
(FLAG '(RADICAL) 'OPFN) 
(PUT 'RADICAL 'NUMBER-OF-ARGS 1) 
(PUT 'RADICAL 'DEFINED-ON-LINE '426) 
(PUT 'RADICAL 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'RADICAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RADICAL (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (RADICAL* (GBASIS* (DPMAT_FROM_A (REVAL1 M T))))))
     (T (RADICAL* M)))) 
(PUT 'RADICAL* 'NUMBER-OF-ARGS 1) 
(PUT 'RADICAL* 'DEFINED-ON-LINE '432) 
(PUT 'RADICAL* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'RADICAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RADICAL* (M)
    (COND ((GREATERP (DPMAT_COLS M) 0) (REDERR "RADICAL only for ideals"))
          (T
           ((LAMBDA (CALI=DEGREES CALI=BASERING)
              (PROG (U C V VARS M1 L P P1)
                (COND
                 ((NULL (SETQ V (ODIM_PARAMETER M)))
                  (RETURN (ZERORADICAL* M))))
                (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
                (SETQ CALI=DEGREES NIL)
                (SETQ U (DELETE V VARS))
                (SETRING* (RING_RLP C U))
                (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
                (SETRING*
                 (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X U)
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
                (SETQ P
                        (CALI_BC_2A
                         (PRIME=QUOT
                          (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
                (SETQ L (RADICAL* M1))
                (SETQ P1 (CALI_BC_2A (PRIME=QUOT L)))
                (SETQ L (DPMAT_2A L))
                (SETRING* C)
                (SETQ L (GBASIS* (MATQQUOT* (DPMAT_FROM_A L) (DP_FROM_A P1))))
                (COND
                 ((OR (DP_UNIT? (SETQ P (DP_FROM_A P)))
                      (SUBMODULEP* (MATQQUOT* M P) M))
                  (RETURN L))
                 (T
                  (PROGN
                   (SETQ M1
                           (RADICAL*
                            (GBASIS* (MATSUM* (LIST M (DPMAT_FROM_DPOLY P))))))
                   (COND ((SUBMODULEP* M1 L) (SETQ L M1))
                         ((NOT (SUBMODULEP* L M1))
                          (SETQ L (MATINTERSECT* (LIST L M1)))))
                   NIL)))
                (RETURN L)))
            CALI=DEGREES CALI=BASERING)))) 
(FLAG '(UNMIXEDRADICAL) 'OPFN) 
(PUT 'UNMIXEDRADICAL 'NUMBER-OF-ARGS 1) 
(PUT 'UNMIXEDRADICAL 'DEFINED-ON-LINE '462) 
(PUT 'UNMIXEDRADICAL 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'UNMIXEDRADICAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNMIXEDRADICAL (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (UNMIXEDRADICAL* (GBASIS* (DPMAT_FROM_A (REVAL1 M T))))))
     (T (UNMIXEDRADICAL* M)))) 
(PUT 'UNMIXEDRADICAL* 'NUMBER-OF-ARGS 1) 
(PUT 'UNMIXEDRADICAL* 'DEFINED-ON-LINE '468) 
(PUT 'UNMIXEDRADICAL* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'UNMIXEDRADICAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNMIXEDRADICAL* (M)
    (COND
     ((GREATERP (DPMAT_COLS M) 0) (REDERR "UNMIXEDRADICAL only for ideals"))
     (T
      ((LAMBDA (CALI=DEGREES CALI=BASERING)
         (PROG (U C D V VARS M1 L P P1)
           (COND
            ((NULL (SETQ V (MOID_GOODINDEPVARSET M)))
             (RETURN (ZERORADICAL* M))))
           (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
           (SETQ D (LENGTH V))
           (SETQ U (SETDIFF VARS V))
           (SETRING* (RING_RLP C U))
           (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
           (SETRING*
            (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X U)
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
           (SETQ P
                   (CALI_BC_2A
                    (PRIME=QUOT (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
           (SETQ L (ZERORADICAL* M1))
           (SETQ P1 (CALI_BC_2A (PRIME=QUOT L)))
           (SETQ L (DPMAT_2A L))
           (SETRING* C)
           (SETQ L (MATQQUOT* (DPMAT_FROM_A L) (DP_FROM_A P1)))
           (COND ((DP_UNIT? (SETQ P (DP_FROM_A P))) (RETURN L))
                 (T
                  (PROGN
                   (SETQ M1 (GBASIS* (MATSUM* (LIST M (DPMAT_FROM_DPOLY P)))))
                   (COND
                    ((EQUAL (DIM* M1) D)
                     (SETQ L (MATINTERSECT* (LIST L (UNMIXEDRADICAL* M1))))))
                   NIL)))
           (RETURN L)))
       CALI=DEGREES CALI=BASERING)))) 
(FLAG '(EQHULL) 'OPFN) 
(PUT 'EQHULL 'NUMBER-OF-ARGS 1) 
(PUT 'EQHULL 'DEFINED-ON-LINE '496) 
(PUT 'EQHULL 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'EQHULL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQHULL (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (DPMAT_2A (EQHULL* (GBASIS* (DPMAT_FROM_A (REVAL1 M T))))))
     (T (EQHULL* M)))) 
(PUT 'EQHULL* 'NUMBER-OF-ARGS 1) 
(PUT 'EQHULL* 'DEFINED-ON-LINE '502) 
(PUT 'EQHULL* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'EQHULL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQHULL* (M)
    (PROG (D)
      (COND ((EQUAL (SETQ D (DIM* M)) 0) (RETURN M))
            (T (RETURN (PRIME=EQHULL M D)))))) 
(PUT 'PRIME=EQHULL 'NUMBER-OF-ARGS 2) 
(PUT 'PRIME=EQHULL 'DEFINED-ON-LINE '509) 
(PUT 'PRIME=EQHULL 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=EQHULL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIME=EQHULL (M D)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (U C V VARS M1 L P)
         (SETQ V (MOID_GOODINDEPVARSET M))
         (COND
          ((NEQ (LENGTH V) D)
           (REDERR "EQHULL found a component of wrong dimension")))
         (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
         (SETQ CALI=DEGREES NIL)
         (SETQ U (SETDIFF (RING_NAMES C) V))
         (SETRING* (RING_RLP C U))
         (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
         (SETRING*
          (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X U)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))
         (SETQ P
                 (CALI_BC_2A
                  (PRIME=QUOT (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
         (SETRING* C)
         (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
         (COND
          ((SUBMODULEP* (SETQ L (MATQQUOT* M (DP_FROM_A P))) M) (RETURN M)))
         (SETQ M1 (GBASIS* (MATSTABQUOT* M (ANNIHILATOR2* L))))
         (COND
          ((EQUAL (DIM* M1) D)
           (RETURN (MATINTERSECT* (LIST L (PRIME=EQHULL M1 D)))))
          (T (RETURN L)))))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'PRIME_SEPARATE 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME_SEPARATE 'DEFINED-ON-LINE '555) 
(PUT 'PRIME_SEPARATE 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME_SEPARATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME_SEPARATE (L)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (CONS X (PRIME=POLYNOMIAL X (DELETE X L))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X) (CONS X (PRIME=POLYNOMIAL X (DELETE X L))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'PRIME=POLYNOMIAL 'NUMBER-OF-ARGS 2) 
(PUT 'PRIME=POLYNOMIAL 'DEFINED-ON-LINE '561) 
(PUT 'PRIME=POLYNOMIAL 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=POLYNOMIAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRIME=POLYNOMIAL (X L)
    (COND ((NULL L) (DP_FI 1))
          (T
           (PROG (U P Q)
             (SETQ P (PRIME=POLYNOMIAL X (CDR L)))
             (COND ((NULL (MATOP_PSEUDOMOD P (CAR L))) (RETURN P)))
             (SETQ U (DPMAT_LIST (CAR L)))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND U
                       (NULL
                        (MATOP_PSEUDOMOD (SETQ Q (BAS_DPOLY (CAR U))) X))))
                 (RETURN NIL)))
               (SETQ U (CDR U))
               (GO WHILELABEL))
             (COND ((NULL U) (REDERR "prime ideal separation failed"))
                   (T (RETURN (DP_PROD P Q)))))))) 
(FLAG '(ZEROPRIMARYDECOMPOSITION) 'OPFN) 
(PUT 'ZEROPRIMARYDECOMPOSITION 'NUMBER-OF-ARGS 1) 
(PUT 'ZEROPRIMARYDECOMPOSITION 'DEFINED-ON-LINE '575) 
(PUT 'ZEROPRIMARYDECOMPOSITION 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ZEROPRIMARYDECOMPOSITION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZEROPRIMARYDECOMPOSITION (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (ZEROPRIMARYDECOMPOSITION* (DPMAT_FROM_A (REVAL1 M T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS 'LIST
                                        (LIST (DPMAT_2A (FIRST X))
                                              (DPMAT_2A (SECOND X)))))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS 'LIST
                                (LIST (DPMAT_2A (FIRST X))
                                      (DPMAT_2A (SECOND X)))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (ZEROPRIMARYDECOMPOSITION* M)))) 
(PUT 'ZEROPRIMARYDECOMPOSITION* 'NUMBER-OF-ARGS 1) 
(PUT 'ZEROPRIMARYDECOMPOSITION* 'DEFINED-ON-LINE '584) 
(PUT 'ZEROPRIMARYDECOMPOSITION* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'ZEROPRIMARYDECOMPOSITION* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZEROPRIMARYDECOMPOSITION* (M)
    (COND
     ((NOT (DIMZEROP* M))
      (REDERR
       "zeroprimarydecomposition only for zerodimensional ideals or modules"))
     (T
      (PROG (F FORALL-RESULT FORALL-ENDPTR)
        (SETQ F
                (PRIME_SEPARATE
                 (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Y (ZEROPRIMES* M))
                   (COND ((NULL Y) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (Y) (GBASIS* Y)) (CAR Y))
                                         NIL)))
                  LOOPLABEL
                   (SETQ Y (CDR Y))
                   (COND ((NULL Y) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (Y) (GBASIS* Y)) (CAR Y)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (COND ((NULL F) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F) (LIST (MATQQUOT* M (CDR F)) (CAR F)))
                          (CAR F))
                         NIL)))
       LOOPLABEL
        (SETQ F (CDR F))
        (COND ((NULL F) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (F) (LIST (MATQQUOT* M (CDR F)) (CAR F))) (CAR F))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(FLAG '(EASYPRIMARYDECOMPOSITION) 'OPFN) 
(PUT 'EASYPRIMARYDECOMPOSITION 'NUMBER-OF-ARGS 1) 
(PUT 'EASYPRIMARYDECOMPOSITION 'DEFINED-ON-LINE '596) 
(PUT 'EASYPRIMARYDECOMPOSITION 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'EASYPRIMARYDECOMPOSITION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYPRIMARYDECOMPOSITION (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (EASYPRIMARYDECOMPOSITION* (DPMAT_FROM_A (REVAL1 M T))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS 'LIST
                                        (LIST (DPMAT_2A (FIRST X))
                                              (DPMAT_2A (SECOND X)))))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS 'LIST
                                (LIST (DPMAT_2A (FIRST X))
                                      (DPMAT_2A (SECOND X)))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (EASYPRIMARYDECOMPOSITION* M)))) 
(PUT 'EASYPRIMARYDECOMPOSITION* 'NUMBER-OF-ARGS 1) 
(PUT 'EASYPRIMARYDECOMPOSITION* 'DEFINED-ON-LINE '603) 
(PUT 'EASYPRIMARYDECOMPOSITION* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'EASYPRIMARYDECOMPOSITION* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EASYPRIMARYDECOMPOSITION* (M)
    (PROG (U)
      (SETQ U (ISOLATEDPRIMES* M))
      (RETURN
       (COND ((NULL U) NIL) ((EQUAL (LENGTH U) 1) (LIST (LIST M (CAR U))))
             (T
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F
                        (PRIME_SEPARATE
                         (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                           (SETQ Y U)
                           (COND ((NULL Y) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Y) (GBASIS* Y)) (CAR Y))
                                            NIL)))
                          LOOPLABEL
                           (SETQ Y (CDR Y))
                           (COND ((NULL Y) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (Y) (GBASIS* Y)) (CAR Y))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (LIST (MATQQUOT* M (CDR F)) (CAR F)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F) (LIST (MATQQUOT* M (CDR F)) (CAR F)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(FLAG '(PRIMARYDECOMPOSITION) 'OPFN) 
(PUT 'PRIMARYDECOMPOSITION 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMARYDECOMPOSITION 'DEFINED-ON-LINE '616) 
(PUT 'PRIMARYDECOMPOSITION 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIMARYDECOMPOSITION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMARYDECOMPOSITION (M)
    (COND
     ((EQUAL *MODE 'ALGEBRAIC)
      (CONS 'LIST
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X
                      (PRIMARYDECOMPOSITION*
                       (GBASIS* (DPMAT_FROM_A (REVAL1 M T)))))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (CONS 'LIST
                                        (LIST (DPMAT_2A (FIRST X))
                                              (DPMAT_2A (SECOND X)))))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS 'LIST
                                (LIST (DPMAT_2A (FIRST X))
                                      (DPMAT_2A (SECOND X)))))
                        (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (PRIMARYDECOMPOSITION* M)))) 
(PUT 'PRIMARYDECOMPOSITION* 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMARYDECOMPOSITION* 'DEFINED-ON-LINE '623) 
(PUT 'PRIMARYDECOMPOSITION* 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIMARYDECOMPOSITION* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMARYDECOMPOSITION* (M)
    (COND
     ((EQUAL (DPMAT_COLS M) 0)
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X (PRIME=DECOMPOSE1 (IDEAL2MAT* M)))
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (LIST (MAT2LIST* (FIRST X)) (SECOND X)))
                          (CAR X))
                         NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (X) (LIST (MAT2LIST* (FIRST X)) (SECOND X))) (CAR X))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T (PRIME=DECOMPOSE1 M)))) 
(PUT 'PRIME=DECOMPOSE1 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=DECOMPOSE1 'DEFINED-ON-LINE '632) 
(PUT 'PRIME=DECOMPOSE1 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=DECOMPOSE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=DECOMPOSE1 (M)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (U C V VARS M1 L L1 P Q)
         (COND
          ((NULL (SETQ V (ODIM_PARAMETER M)))
           (RETURN (ZEROPRIMARYDECOMPOSITION* M))))
         (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
         (SETQ CALI=DEGREES NIL)
         (SETQ U (DELETE V VARS))
         (SETRING* (RING_RLP C U))
         (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
         (SETRING*
          (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X U)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))
         (SETQ P
                 (CALI_BC_2A
                  (PRIME=QUOT (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
         (SETQ L
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (PRIME=DECOMPOSE1 M1))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST
                                        (CONS (DPMAT_2A (FIRST X))
                                              (CALI_BC_2A
                                               (PRIME=QUOT (FIRST X))))
                                        (CONS (DPMAT_2A (SECOND X))
                                              (CALI_BC_2A
                                               (PRIME=QUOT (SECOND X))))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (LIST
                                (CONS (DPMAT_2A (FIRST X))
                                      (CALI_BC_2A (PRIME=QUOT (FIRST X))))
                                (CONS (DPMAT_2A (SECOND X))
                                      (CALI_BC_2A (PRIME=QUOT (SECOND X))))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETRING* C)
         (SETQ L
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X L)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (PROGN
                                        (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
                                        (LIST
                                         (GBASIS*
                                          (MATQQUOT*
                                           (DPMAT_FROM_A (CAR (FIRST X)))
                                           (DP_FROM_A (CDR (FIRST X)))))
                                         (GBASIS*
                                          (MATQQUOT*
                                           (DPMAT_FROM_A (CAR (SECOND X)))
                                           (DP_FROM_A (CDR (SECOND X))))))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (PROGN
                                (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
                                (LIST
                                 (GBASIS*
                                  (MATQQUOT* (DPMAT_FROM_A (CAR (FIRST X)))
                                   (DP_FROM_A (CDR (FIRST X)))))
                                 (GBASIS*
                                  (MATQQUOT* (DPMAT_FROM_A (CAR (SECOND X)))
                                   (DP_FROM_A (CDR (SECOND X))))))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((OR (DP_UNIT? (SETQ P (DP_FROM_A P)))
               (SUBMODULEP* (SETQ M1 (MATQQUOT* M P)) M))
           (RETURN L))
          (T
           (PROGN
            (SETQ Q P)
            (SETQ V 1)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (NOT (SUBMODULEP* (SETQ M1 (DPMAT_TIMES_DPOLY P M1)) M))
                      (LESSP V 15)))
                (RETURN NIL)))
              (PROGN (SETQ Q (DP_PROD P Q)) (SETQ V (PLUS V 1)))
              (GO WHILELABEL))
            (COND
             ((EQUAL V 15)
              (REDERR "Power detection in prime!=decompose1 failed")))
            (SETQ L1
                    (PRIME=DECOMPOSE1
                     (GBASIS*
                      (MATSUM*
                       (LIST M
                             (DPMAT_TIMES_DPOLY Q
                              (DPMAT_UNIT (DPMAT_COLS M)
                               (DPMAT_COLDEGS M))))))))
            (SETQ P
                    (APPEND
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X L)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (SECOND X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X L1)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (SECOND X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (SETQ L
                    (APPEND L
                            (PROG (X FORALL-RESULT FORALL-ENDPTR)
                              (SETQ X L1)
                             STARTOVER
                              (COND ((NULL X) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (X)
                                         (COND
                                          ((PRIME=NECESSARY (SECOND X) M P)
                                           (LIST X))))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ X (CDR X))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL X) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (X)
                                         (COND
                                          ((PRIME=NECESSARY (SECOND X) M P)
                                           (LIST X))))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ X (CDR X))
                              (GO LOOPLABEL))))
            NIL)))
         (RETURN L)))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'PRIME=DECOMPOSE2 'NUMBER-OF-ARGS 1) 
(PUT 'PRIME=DECOMPOSE2 'DEFINED-ON-LINE '691) 
(PUT 'PRIME=DECOMPOSE2 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=DECOMPOSE2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIME=DECOMPOSE2 (M)
    ((LAMBDA (CALI=DEGREES CALI=BASERING)
       (PROG (U C V VARS M1 L L1 P Q)
         (SETQ V (MOID_GOODINDEPVARSET M))
         (COND ((NULL V) (RETURN (ZEROPRIMARYDECOMPOSITION* M))))
         (SETQ VARS (RING_NAMES (SETQ C CALI=BASERING)))
         (SETQ CALI=DEGREES NIL)
         (SETQ U (SETDIFF VARS V))
         (SETRING* (RING_RLP C U))
         (SETQ M1 (DPMAT_2A (GBASIS* (DPMAT_NEWORDER M NIL))))
         (SETRING*
          (RING_DEFINE U (DEGREEORDER* U) 'REVLEX
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X U)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) 1) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))
         (SETQ P
                 (CALI_BC_2A
                  (PRIME=QUOT (SETQ M1 (GROEB_MINGB (DPMAT_FROM_A M1))))))
         (SETQ L
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (ZEROPRIMARYDECOMPOSITION* M1))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (LIST
                                        (CONS (DPMAT_2A (FIRST X))
                                              (CALI_BC_2A
                                               (PRIME=QUOT (FIRST X))))
                                        (CONS (DPMAT_2A (SECOND X))
                                              (CALI_BC_2A
                                               (PRIME=QUOT (SECOND X))))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (LIST
                                (CONS (DPMAT_2A (FIRST X))
                                      (CALI_BC_2A (PRIME=QUOT (FIRST X))))
                                (CONS (DPMAT_2A (SECOND X))
                                      (CALI_BC_2A (PRIME=QUOT (SECOND X))))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETRING* C)
         (SETQ L
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X L)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (PROGN
                                        (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
                                        (LIST
                                         (GBASIS*
                                          (MATQQUOT*
                                           (DPMAT_FROM_A (CAR (FIRST X)))
                                           (DP_FROM_A (CDR (FIRST X)))))
                                         (GBASIS*
                                          (MATQQUOT*
                                           (DPMAT_FROM_A (CAR (SECOND X)))
                                           (DP_FROM_A (CDR (SECOND X))))))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (PROGN
                                (SETQ CALI=DEGREES (DPMAT_COLDEGS M))
                                (LIST
                                 (GBASIS*
                                  (MATQQUOT* (DPMAT_FROM_A (CAR (FIRST X)))
                                   (DP_FROM_A (CDR (FIRST X)))))
                                 (GBASIS*
                                  (MATQQUOT* (DPMAT_FROM_A (CAR (SECOND X)))
                                   (DP_FROM_A (CDR (SECOND X))))))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((OR (DP_UNIT? (SETQ P (DP_FROM_A P)))
               (SUBMODULEP* (SETQ M1 (MATQQUOT* M P)) M))
           (RETURN L))
          (T
           (PROGN
            (SETQ Q P)
            (SETQ V 1)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (NOT (SUBMODULEP* (SETQ M1 (DPMAT_TIMES_DPOLY P M1)) M))
                      (LESSP V 15)))
                (RETURN NIL)))
              (PROGN (SETQ Q (DP_PROD P Q)) (SETQ V (PLUS V 1)))
              (GO WHILELABEL))
            (COND
             ((EQUAL V 15)
              (REDERR "Power detection in prime!=decompose2 failed")))
            (SETQ L1
                    (PRIME=DECOMPOSE2
                     (GBASIS*
                      (MATSUM*
                       (LIST M
                             (DPMAT_TIMES_DPOLY Q
                              (DPMAT_UNIT (DPMAT_COLS M)
                               (DPMAT_COLDEGS M))))))))
            (SETQ P
                    (APPEND
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X L)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (SECOND X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X L1)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (SECOND X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
            (SETQ L
                    (APPEND L
                            (PROG (X FORALL-RESULT FORALL-ENDPTR)
                              (SETQ X L1)
                             STARTOVER
                              (COND ((NULL X) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      ((LAMBDA (X)
                                         (COND
                                          ((PRIME=NECESSARY (SECOND X) M P)
                                           (LIST X))))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                              (SETQ X (CDR X))
                              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                             LOOPLABEL
                              (COND ((NULL X) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      ((LAMBDA (X)
                                         (COND
                                          ((PRIME=NECESSARY (SECOND X) M P)
                                           (LIST X))))
                                       (CAR X)))
                              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                              (SETQ X (CDR X))
                              (GO LOOPLABEL))))
            NIL)))
         (RETURN L)))
     CALI=DEGREES CALI=BASERING)) 
(PUT 'PRIME=NECESSARY 'NUMBER-OF-ARGS 3) 
(PUT 'PRIME=NECESSARY 'DEFINED-ON-LINE '736) 
(PUT 'PRIME=NECESSARY 'DEFINED-IN-FILE 'CALI/PRIME.RED) 
(PUT 'PRIME=NECESSARY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRIME=NECESSARY (P M L)
    (PROG (L1 UNIT)
      (SETQ L1
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U L)
               STARTOVER
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (U)
                           (COND
                            ((OR (EQUAL U P) (SUBMODULEP* U P)) (LIST T))))
                         (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ U (CDR U))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (U)
                           (COND
                            ((OR (EQUAL U P) (SUBMODULEP* U P)) (LIST T))))
                         (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ U (CDR U))
                (GO LOOPLABEL)))
      (COND
       ((NULL L1)
        (REDERR "prime!=necessary: supplied prime's list incorrect")))
      (COND ((EQUAL (LENGTH L1) 1) (RETURN T)))
      (SETQ UNIT (DPMAT_UNIT (DPMAT_COLS M) CALI=DEGREES))
      (SETQ L1
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U L)
               STARTOVER
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (U) (COND ((NOT (SUBMODULEP* U P)) (LIST U))))
                         (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ U (CDR U))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (U) (COND ((NOT (SUBMODULEP* U P)) (LIST U))))
                         (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ U (CDR U))
                (GO LOOPLABEL)))
      (SETQ L (DELETE P (SETDIFF L L1)))
      (SETQ M (MATQQUOT* M (PRIME=POLYNOMIAL P L1)))
      (RETURN (NOT (SUBMODULEP* (MATSTABQUOT* M P) M))))) 
(ENDMODULE) 