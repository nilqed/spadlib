(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GLMAT)) 
(FLUID '(*CRAMER *FACTOR *GCD *SQFREE *SUB2 KORD*)) 
(GLOBAL '(!ARBINT)) 
(COND ((NULL !ARBINT) (SETQ !ARBINT 0))) 
(SWITCH (LIST 'CRAMER)) 
(PUT 'CRAMER 'SIMPFG
     '((T (PUT 'MAT 'LNRSOLVEFN 'CLNRSOLVE) (PUT 'MAT 'INVERSEFN 'MATINV))
       (NIL (PUT 'MAT 'LNRSOLVEFN 'LNRSOLVE)
        (PUT 'MAT 'INVERSEFN 'MATINVERSE)))) 
(DEFLIST '((ARBCOMPLEX SIMPIDEN)) 'SIMPFN) 
(PUT 'CLNRSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'CLNRSOLVE 'DEFINED-ON-LINE '52) 
(PUT 'CLNRSOLVE 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'CLNRSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CLNRSOLVE (U V) (MULTM (MATINV U) V)) 
(PUT 'MINV 'NUMBER-OF-ARGS 1) 
(PUT 'MINV 'DEFINED-ON-LINE '56) 
(PUT 'MINV 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MINV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINV (U) (MATINV (MATSM U))) 
(PUT 'MINV 'RTYPEFN 'QUOTEMATRIX) 
(REMPROP 'MATEIGEN 'RTYPEFN) 
(PUT 'MATINV 'NUMBER-OF-ARGS 1) 
(PUT 'MATINV 'DEFINED-ON-LINE '65) 
(PUT 'MATINV 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MATINV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATINV (U)
    (PROG (SGN X Y Z *EXP L M LM)
      (SETQ L 0)
      (SETQ M 0)
      (SETQ LM 0)
      (SETQ *EXP T)
      (SETQ Z 1)
      (SETQ LM (LENGTH (CAR U)))
      (PROG (V)
        (SETQ V U)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ Y 1)
            (PROG (W)
              (SETQ W V)
             LAB
              (COND ((NULL W) (RETURN NIL)))
              ((LAMBDA (W) (SETQ Y (LCM Y (CDR W)))) (CAR W))
              (SETQ W (CDR W))
              (GO LAB))
            (SETQ M LM)
            (SETQ X
                    (CONS (CONS (LIST (CONS NIL (SETQ L (PLUS L 1)))) (NEGF Y))
                          NIL))
            (PROG (J)
              (SETQ J (REVERSE V))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (COND
                   ((CAR J)
                    (SETQ X
                            (CONS
                             (CONS (LIST M)
                                   ((LAMBDA (G552)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAR J) G552))
                                       (T (POLY-MULTF (CAR J) G552))))
                                    ((LAMBDA (*EXP) (QUOTF1 Y (CDR J))) T)))
                             X))))
                  (SETQ M (DIFFERENCE M 1))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (SETQ Z (|C:EXTMULT| X Z))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND ((SINGULARCHK (CAAR Z)) (RERROR 'MATRIX 13 "Singular matrix")))
      (SETQ SGN (EVENP (LENGTH (CAAR Z))))
      (RETURN
       (PROG (K FORALL-RESULT FORALL-ENDPTR)
         (SETQ K (CAAR Z))
         (COND ((NULL K) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (K)
                             (PROGN
                              (SETQ SGN (NOT SGN))
                              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                (SETQ J (CAAR Z))
                                (COND ((NULL J) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (J)
                                                    (MKGLIMAT K Z SGN J))
                                                  (CAR J))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ J (CDR J))
                                (COND ((NULL J) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (J) (MKGLIMAT K Z SGN J))
                                          (CAR J))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))
                           (CAR K))
                          NIL)))
        LOOPLABEL
         (SETQ K (CDR K))
         (COND ((NULL K) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (K)
                     (PROGN
                      (SETQ SGN (NOT SGN))
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J (CAAR Z))
                        (COND ((NULL J) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (J) (MKGLIMAT K Z SGN J))
                                          (CAR J))
                                         NIL)))
                       LOOPLABEL
                        (SETQ J (CDR J))
                        (COND ((NULL J) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (MKGLIMAT K Z SGN J)) (CAR J))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
                   (CAR K))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'SINGULARCHK 'NUMBER-OF-ARGS 1) 
(PUT 'SINGULARCHK 'DEFINED-ON-LINE '89) 
(PUT 'SINGULARCHK 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'SINGULARCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SINGULARCHK (U) (PAIRP (CAR (LASTPAIR U)))) 
(FLAG '(MATEIGEN) 'OPFN) 
(FLAG '(MATEIGEN) 'NOVAL) 
(PUT 'MATEIGEN 'NUMBER-OF-ARGS 2) 
(PUT 'MATEIGEN 'DEFINED-ON-LINE '95) 
(PUT 'MATEIGEN 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MATEIGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATEIGEN (U EIVAL)
    (PROG (ARBVARS EXU SGN Q R S X Y Z EIVEC *FACTOR *SQFREE *EXP *ROUNDED L)
      (SETQ L 0)
      (SETQ *EXP T)
      (COND ((NOT (EQ (GETRTYPE U) 'MATRIX)) (TYPERR U "matrix")))
      (SETQ EIVAL (*A2K EIVAL))
      (SETQ KORD* (CONS EIVAL KORD*))
      (SETQ EXU (MATEIGEN1 (MATSM U) EIVAL))
      (SETQ Q (CAR EXU))
      (SETQ Y (CADR EXU))
      (SETQ Z (CADDR EXU))
      (SETQ EXU (CDDDR EXU))
      (SETQ *SQFREE T)
      (PROG (J)
        (SETQ J (CDR (FCTRF (CAR (SUBS2 (CONS (CDAR Z) 1))))))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((AND (NULL (OR (ATOM (CAR J)) (ATOM (CAR (CAR J)))))
                  (EQ (CAAAR (CAR J)) EIVAL))
             (SETQ S
                     (CONS
                      (COND
                       ((NULL (CDR (CAR J)))
                        (CONS (LIST (CONS (CONS (CAAAR (CAR J)) 1) 1))
                              (TIMES (CDAAR (CAR J)) (CDR J))))
                       (T J))
                      S)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (PROG (J)
        (SETQ J Q)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           ((LAMBDA (Y)
              ((LAMBDA (X)
                 (COND (X (RPLACD X (PLUS (CDR X) (CDR J))))
                       (T (SETQ S (CONS (CONS Y (CDR J)) S)))))
               (ASSOC Y S)))
            (ABSF (REORDER (CAR J)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ L (LENGTH S))
      (SETQ R
              (CONS 'LIST
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J S)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (PROGN
                                           (COND
                                            ((NULL
                                              (AND (EQUAL (CDR J) 1)
                                                   (EQUAL L 1)))
                                             (PROGN
                                              (SETQ Y 1)
                                              (PROG (K)
                                                (SETQ K EXU)
                                               LAB
                                                (COND ((NULL K) (RETURN NIL)))
                                                ((LAMBDA (K)
                                                   (COND
                                                    ((SETQ X
                                                             (REDUCE-MOD-EIG
                                                              (CAR J)
                                                              (|C:EXTMULT| K
                                                               Y)))
                                                     (SETQ Y X))))
                                                 (CAR K))
                                                (SETQ K (CDR K))
                                                (GO LAB)))))
                                           (SETQ ARBVARS NIL)
                                           (PROG (K)
                                             (SETQ K (CAAR Z))
                                            LAB
                                             (COND ((NULL K) (RETURN NIL)))
                                             ((LAMBDA (K)
                                                (COND
                                                 ((OR (EQUAL Y 1)
                                                      (NULL
                                                       (MEMBER K (CAAR Y))))
                                                  (SETQ ARBVARS
                                                          (CONS
                                                           (CONS K
                                                                 (MAKEARBCOMPLEX))
                                                           ARBVARS)))))
                                              (CAR K))
                                             (SETQ K (CDR K))
                                             (GO LAB))
                                           (SETQ SGN
                                                   (OR (EQUAL Y 1)
                                                       (EVENP
                                                        (LENGTH (CAAR Y)))))
                                           (SETQ EIVEC
                                                   (CONS 'MAT
                                                         (PROG (K FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ K (CAAR Z))
                                                           (COND
                                                            ((NULL K)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  K)
                                                                               (LIST
                                                                                (COND
                                                                                 ((SETQ X
                                                                                          (ASSOC
                                                                                           K
                                                                                           ARBVARS))
                                                                                  (CAAAR
                                                                                   (CDR
                                                                                    X)))
                                                                                 (T
                                                                                  (PREPSQ*
                                                                                   (MKGLEIG
                                                                                    K
                                                                                    Y
                                                                                    (SETQ SGN
                                                                                            (NOT
                                                                                             SGN))
                                                                                    ARBVARS))))))
                                                                             (CAR
                                                                              K))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ K (CDR K))
                                                           (COND
                                                            ((NULL K)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (K)
                                                                (LIST
                                                                 (COND
                                                                  ((SETQ X
                                                                           (ASSOC
                                                                            K
                                                                            ARBVARS))
                                                                   (CAAAR
                                                                    (CDR X)))
                                                                  (T
                                                                   (PREPSQ*
                                                                    (MKGLEIG K
                                                                     Y
                                                                     (SETQ SGN
                                                                             (NOT
                                                                              SGN))
                                                                     ARBVARS))))))
                                                              (CAR K))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL))))
                                           (LIST 'LIST
                                                 (PREPSQ* (CONS (CAR J) 1))
                                                 (CDR J) EIVEC)))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (PROGN
                                   (COND
                                    ((NULL (AND (EQUAL (CDR J) 1) (EQUAL L 1)))
                                     (PROGN
                                      (SETQ Y 1)
                                      (PROG (K)
                                        (SETQ K EXU)
                                       LAB
                                        (COND ((NULL K) (RETURN NIL)))
                                        ((LAMBDA (K)
                                           (COND
                                            ((SETQ X
                                                     (REDUCE-MOD-EIG (CAR J)
                                                      (|C:EXTMULT| K Y)))
                                             (SETQ Y X))))
                                         (CAR K))
                                        (SETQ K (CDR K))
                                        (GO LAB)))))
                                   (SETQ ARBVARS NIL)
                                   (PROG (K)
                                     (SETQ K (CAAR Z))
                                    LAB
                                     (COND ((NULL K) (RETURN NIL)))
                                     ((LAMBDA (K)
                                        (COND
                                         ((OR (EQUAL Y 1)
                                              (NULL (MEMBER K (CAAR Y))))
                                          (SETQ ARBVARS
                                                  (CONS
                                                   (CONS K (MAKEARBCOMPLEX))
                                                   ARBVARS)))))
                                      (CAR K))
                                     (SETQ K (CDR K))
                                     (GO LAB))
                                   (SETQ SGN
                                           (OR (EQUAL Y 1)
                                               (EVENP (LENGTH (CAAR Y)))))
                                   (SETQ EIVEC
                                           (CONS 'MAT
                                                 (PROG (K FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ K (CAAR Z))
                                                   (COND
                                                    ((NULL K) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (K)
                                                                       (LIST
                                                                        (COND
                                                                         ((SETQ X
                                                                                  (ASSOC
                                                                                   K
                                                                                   ARBVARS))
                                                                          (CAAAR
                                                                           (CDR
                                                                            X)))
                                                                         (T
                                                                          (PREPSQ*
                                                                           (MKGLEIG
                                                                            K Y
                                                                            (SETQ SGN
                                                                                    (NOT
                                                                                     SGN))
                                                                            ARBVARS))))))
                                                                     (CAR K))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ K (CDR K))
                                                   (COND
                                                    ((NULL K)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (K)
                                                               (LIST
                                                                (COND
                                                                 ((SETQ X
                                                                          (ASSOC
                                                                           K
                                                                           ARBVARS))
                                                                  (CAAAR
                                                                   (CDR X)))
                                                                 (T
                                                                  (PREPSQ*
                                                                   (MKGLEIG K Y
                                                                    (SETQ SGN
                                                                            (NOT
                                                                             SGN))
                                                                    ARBVARS))))))
                                                             (CAR K))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))))
                                   (LIST 'LIST (PREPSQ* (CONS (CAR J) 1))
                                         (CDR J) EIVEC)))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ KORD* (CDR KORD*))
      (RETURN R))) 
(PUT 'MATEIGEN1 'NUMBER-OF-ARGS 2) 
(PUT 'MATEIGEN1 'DEFINED-ON-LINE '151) 
(PUT 'MATEIGEN1 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MATEIGEN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MATEIGEN1 (U EIVAL)
    (PROG (Q X Y Z L LM M)
      (SETQ L 0)
      (SETQ LM 0)
      (SETQ M 0)
      (SETQ LM (LENGTH (CAR U)))
      (SETQ Z 1)
      (SETQ U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V U)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (PROGN
                                     (SETQ Y 1)
                                     (PROG (W)
                                       (SETQ W V)
                                      LAB
                                       (COND ((NULL W) (RETURN NIL)))
                                       ((LAMBDA (W) (SETQ Y (LCM Y (CDR W))))
                                        (CAR W))
                                       (SETQ W (CDR W))
                                       (GO LAB))
                                     (SETQ M LM)
                                     (SETQ L (PLUS L 1))
                                     (SETQ X NIL)
                                     (PROG (J)
                                       (SETQ J (REVERSE V))
                                      LAB
                                       (COND ((NULL J) (RETURN NIL)))
                                       ((LAMBDA (J)
                                          (PROGN
                                           (COND
                                            ((OR (CAR J) (EQUAL L M))
                                             (SETQ X
                                                     (CONS
                                                      (CONS (LIST M)
                                                            ((LAMBDA
                                                                 (G555 G556)
                                                               (COND
                                                                (*PHYSOP-LOADED
                                                                 (PHYSOP-MULTF
                                                                  G555 G556))
                                                                (T
                                                                 (POLY-MULTF
                                                                  G555 G556))))
                                                             (COND
                                                              ((EQUAL L M)
                                                               (ADDF (CAR J)
                                                                     (NEGF
                                                                      ((LAMBDA
                                                                           (
                                                                            G553)
                                                                         (COND
                                                                          (*PHYSOP-LOADED
                                                                           (PHYSOP-MULTF
                                                                            G553
                                                                            (CDR
                                                                             J)))
                                                                          (T
                                                                           (POLY-MULTF
                                                                            G553
                                                                            (CDR
                                                                             J)))))
                                                                       (LIST
                                                                        (CONS
                                                                         (CONS
                                                                          EIVAL
                                                                          1)
                                                                         1))))))
                                                              (T (CAR J)))
                                                             ((LAMBDA (*EXP)
                                                                (QUOTF1 Y
                                                                        (CDR
                                                                         J)))
                                                              T)))
                                                      X))))
                                           (SETQ M (DIFFERENCE M 1))))
                                        (CAR J))
                                       (SETQ J (CDR J))
                                       (GO LAB))
                                     (SETQ Y Z)
                                     (SETQ Z
                                             (|C:EXTMULT|
                                              (COND
                                               ((NULL (CDR X))
                                                (PROGN
                                                 ((LAMBDA (P)
                                                    (SETQ Q
                                                            (COND
                                                             (P
                                                              (CONS
                                                               (CONS (CAR P)
                                                                     (PLUS
                                                                      (CDR P)
                                                                      1))
                                                               (DELETE P Q)))
                                                             (T
                                                              (CONS
                                                               (CONS (CDAR X)
                                                                     1)
                                                               Q)))))
                                                  (ASSOC (CDAR X) Q))
                                                 (LIST (CONS (CAAR X) 1))))
                                               (T X))
                                              Z))
                                     X))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ Y 1)
                             (PROG (W)
                               (SETQ W V)
                              LAB
                               (COND ((NULL W) (RETURN NIL)))
                               ((LAMBDA (W) (SETQ Y (LCM Y (CDR W)))) (CAR W))
                               (SETQ W (CDR W))
                               (GO LAB))
                             (SETQ M LM)
                             (SETQ L (PLUS L 1))
                             (SETQ X NIL)
                             (PROG (J)
                               (SETQ J (REVERSE V))
                              LAB
                               (COND ((NULL J) (RETURN NIL)))
                               ((LAMBDA (J)
                                  (PROGN
                                   (COND
                                    ((OR (CAR J) (EQUAL L M))
                                     (SETQ X
                                             (CONS
                                              (CONS (LIST M)
                                                    ((LAMBDA (G555 G556)
                                                       (COND
                                                        (*PHYSOP-LOADED
                                                         (PHYSOP-MULTF G555
                                                          G556))
                                                        (T
                                                         (POLY-MULTF G555
                                                                     G556))))
                                                     (COND
                                                      ((EQUAL L M)
                                                       (ADDF (CAR J)
                                                             (NEGF
                                                              ((LAMBDA (G553)
                                                                 (COND
                                                                  (*PHYSOP-LOADED
                                                                   (PHYSOP-MULTF
                                                                    G553
                                                                    (CDR J)))
                                                                  (T
                                                                   (POLY-MULTF
                                                                    G553
                                                                    (CDR J)))))
                                                               (LIST
                                                                (CONS
                                                                 (CONS EIVAL 1)
                                                                 1))))))
                                                      (T (CAR J)))
                                                     ((LAMBDA (*EXP)
                                                        (QUOTF1 Y (CDR J)))
                                                      T)))
                                              X))))
                                   (SETQ M (DIFFERENCE M 1))))
                                (CAR J))
                               (SETQ J (CDR J))
                               (GO LAB))
                             (SETQ Y Z)
                             (SETQ Z
                                     (|C:EXTMULT|
                                      (COND
                                       ((NULL (CDR X))
                                        (PROGN
                                         ((LAMBDA (P)
                                            (SETQ Q
                                                    (COND
                                                     (P
                                                      (CONS
                                                       (CONS (CAR P)
                                                             (PLUS (CDR P) 1))
                                                       (DELETE P Q)))
                                                     (T
                                                      (CONS (CONS (CDAR X) 1)
                                                            Q)))))
                                          (ASSOC (CDAR X) Q))
                                         (LIST (CONS (CAAR X) 1))))
                                       (T X))
                                      Z))
                             X))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS Q (CONS Y (CONS Z U)))))) 
(PUT 'REDUCE-MOD-EIG 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCE-MOD-EIG 'DEFINED-ON-LINE '178) 
(PUT 'REDUCE-MOD-EIG 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'REDUCE-MOD-EIG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCE-MOD-EIG (U V)
    (PROG (X Y)
      (PROG (J)
        (SETQ J V)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         ((CAR (SETQ Y (REDUCE-MOD-EIGF U (CDAR J))))
          (SETQ X (CONS (CONS (CAAR J) Y) X))))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ Y 1)
      (PROG (J)
        (SETQ J X)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (SETQ Y (LCM Y (CDR (CDAR J))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN
       (PROG (J FORALL-RESULT FORALL-ENDPTR)
         (SETQ J (REVERSE X))
         (COND ((NULL J) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (CONS (CAAR J)
                                ((LAMBDA (G557 G558)
                                   (COND
                                    (*PHYSOP-LOADED (PHYSOP-MULTF G557 G558))
                                    (T (POLY-MULTF G557 G558))))
                                 (CAR (CDAR J))
                                 ((LAMBDA (*EXP) (QUOTF1 Y (CDR (CDAR J))))
                                  T)))
                          NIL)))
        LOOPLABEL
         (SETQ J (CDR J))
         (COND ((NULL J) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  (CONS (CAAR J)
                        ((LAMBDA (G557 G558)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G557 G558))
                                 (T (POLY-MULTF G557 G558))))
                         (CAR (CDAR J))
                         ((LAMBDA (*EXP) (QUOTF1 Y (CDR (CDAR J)))) T)))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'REDUCE-MOD-EIGF 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCE-MOD-EIGF 'DEFINED-ON-LINE '190) 
(PUT 'REDUCE-MOD-EIGF 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'REDUCE-MOD-EIGF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCE-MOD-EIGF (U V)
    ((LAMBDA (*SUB2)
       (SUBS2
        (REDUCE-EIVAL-POWERS
         (CONS (CAAR U) (NEGSQ (CANCEL (CONS (CDR U) (CDAR U))))) V)))
     *SUB2)) 
(PUT 'REDUCE-EIVAL-POWERS 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCE-EIVAL-POWERS 'DEFINED-ON-LINE '194) 
(PUT 'REDUCE-EIVAL-POWERS 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'REDUCE-EIVAL-POWERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCE-EIVAL-POWERS (V U)
    (COND
     ((OR (OR (ATOM U) (ATOM (CAR U))) (NULL (EQ (CAAAR U) (CAAR V))))
      (CONS U 1))
     (T (REDUCE-EIVAL-POWERS1 V (CONS U 1))))) 
(PUT 'REDUCE-EIVAL-POWERS1 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCE-EIVAL-POWERS1 'DEFINED-ON-LINE '198) 
(PUT 'REDUCE-EIVAL-POWERS1 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'REDUCE-EIVAL-POWERS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCE-EIVAL-POWERS1 (V U)
    (COND
     ((OR (OR (ATOM (CAR U)) (ATOM (CAR (CAR U))))
          (LESSP (CDAAR (CAR U)) (CDR (CAR V))))
      U)
     ((EQUAL (CDAAR (CAR U)) (CDR (CAR V)))
      (ADDSQ (MULTSQ (CDR V) (CONS (CDAR (CAR U)) (CDR U)))
             (CONS (CDR (CAR U)) (CDR U))))
     (T
      (REDUCE-EIVAL-POWERS1 V
       (ADDSQ
        (MULTSQ
         (CONS
          ((LAMBDA (G560)
             ((LAMBDA (G544)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G560))
                      (T (POLY-MULTF G544 G560))))
              (LIST
               (CONS
                (CONS (CAAAR (CAR U))
                      (DIFFERENCE (CDAAR (CAR U)) (CDR (CAR V))))
                1))))
           (CDAR (CAR U)))
          (CDR U))
         (CDR V))
        (CONS (CDR (CAR U)) (CDR U))))))) 
(PUT 'DETEX 'NUMBER-OF-ARGS 1) 
(PUT 'DETEX 'DEFINED-ON-LINE '211) 
(PUT 'DETEX 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'DETEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DETEX (U)
    (PROG (F X Y Z M LM)
      (SETQ M 0)
      (SETQ LM 0)
      (SETQ Z 1)
      (SETQ U (MATSM (CAR U)))
      (SETQ LM (LENGTH (CAR U)))
      (SETQ F 1)
      (PROG (V)
        (SETQ V U)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ Y 1)
            (PROG (W)
              (SETQ W V)
             LAB
              (COND ((NULL W) (RETURN NIL)))
              ((LAMBDA (W) (SETQ Y (LCM Y (CDR W)))) (CAR W))
              (SETQ W (CDR W))
              (GO LAB))
            (SETQ F
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF Y F))
                          (T (POLY-MULTF Y F))))
            (SETQ M LM)
            (SETQ X NIL)
            (PROG (J)
              (SETQ J V)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (COND
                   ((CAR J)
                    (SETQ X
                            (CONS
                             (CONS (LIST M)
                                   ((LAMBDA (G562)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAR J) G562))
                                       (T (POLY-MULTF (CAR J) G562))))
                                    ((LAMBDA (*EXP) (QUOTF1 Y (CDR J))) T)))
                             X))))
                  (SETQ M (DIFFERENCE M 1))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (SETQ Z (|C:EXTMULT| X Z))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CANCEL (CONS (CDAR Z) F))))) 
(PUT 'MKGLIMAT 'NUMBER-OF-ARGS 4) 
(PUT 'MKGLIMAT 'DEFINED-ON-LINE '238) 
(PUT 'MKGLIMAT 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MKGLIMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKGLIMAT (U V SGN K)
    (PROG (S X Y)
      (SETQ X (CONS NIL 1))
      (SETQ Y (CAAR V))
      (PROG (J)
        (SETQ J (CDR V))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         ((SETQ S (GLMATTERM U Y J K))
          (SETQ X (ADDSQ (CANCEL (CONS S (CDAR V))) X))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (COND (SGN (NEGSQ X)) (T X))))) 
(PUT 'GLMATTERM 'NUMBER-OF-ARGS 4) 
(PUT 'GLMATTERM 'DEFINED-ON-LINE '248) 
(PUT 'GLMATTERM 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'GLMATTERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLMATTERM (U V W K)
    (PROG (X Y SGN)
      (SETQ X (CAAR W))
     A
      (COND
       ((NULL X)
        (RETURN
         (COND ((AND (PAIRP (CAR Y)) (EQUAL (CDAR Y) K)) (CDAR W)) (T NIL)))))
      (COND ((EQUAL (CAR X) U) (RETURN NIL))
            ((MEMBER (CAR X) V)
             (PROGN (SETQ X (CDR X)) (COND (Y (SETQ SGN (NOT SGN))))))
            (Y (RETURN NIL))
            (T (PROGN (SETQ Y (LIST (CAR X))) (SETQ X (CDR X)))))
      (GO A))) 
(PUT 'MKGLEIG 'NUMBER-OF-ARGS 4) 
(PUT 'MKGLEIG 'DEFINED-ON-LINE '261) 
(PUT 'MKGLEIG 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'MKGLEIG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKGLEIG (U V SGN ARBVARS)
    (PROG (S X Y *GCD)
      (SETQ X (CONS NIL 1))
      (SETQ Y (CAAR V))
      (SETQ *GCD T)
      (PROG (J)
        (SETQ J (CDR V))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        (COND
         ((SETQ S (GLSOLEIG U Y J ARBVARS))
          (SETQ X (ADDSQ (CANCEL (CONS S (CDAR V))) X))))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (COND (SGN (NEGSQ X)) (T X))))) 
(PUT 'GLSOLEIG 'NUMBER-OF-ARGS 4) 
(PUT 'GLSOLEIG 'DEFINED-ON-LINE '272) 
(PUT 'GLSOLEIG 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT 'GLSOLEIG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLSOLEIG (U V W ARBVARS)
    (PROG (X Y SGN)
      (SETQ X (CAAR W))
     A
      (COND
       ((NULL X)
        (RETURN
         (COND ((NULL (CAR Y)) (CDAR W))
               (T
                ((LAMBDA (G563 G564)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G563 G564))
                         (T (POLY-MULTF G563 G564))))
                 (CDR (ASSOC (CAR Y) ARBVARS))
                 (COND (SGN (NEGF (CDAR W))) (T (CDAR W)))))))))
      (COND ((EQUAL (CAR X) U) (RETURN NIL))
            ((MEMBER (CAR X) V)
             (PROGN (SETQ X (CDR X)) (COND (Y (SETQ SGN (NOT SGN))))))
            (Y (RETURN NIL))
            (T (PROGN (SETQ Y (LIST (CAR X))) (SETQ X (CDR X)))))
      (GO A))) 
(PUT '|C:EXTMULT| 'NUMBER-OF-ARGS 2) 
(PUT '|C:EXTMULT| 'DEFINED-ON-LINE '341) 
(PUT '|C:EXTMULT| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:EXTMULT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:EXTMULT| (U V)
    (PROG (STACK W X R R1)
      (SETQ STACK NIL)
     TOP
      (COND ((OR (NULL U) (NULL V)) (PROGN (SETQ R NIL) (GO DONE)))
            ((EQUAL V 1) (PROGN (SETQ R U) (GO DONE))))
      (SETQ X (|C:ORDEXN| (CAR (CAAR U)) (CAAR V)))
      (COND
       (X
        (PROGN
         (SETQ STACK (CONS (CONS 1 (CONS X (CONS U V))) STACK))
         (SETQ U (CDR U))
         (GO TOP)))
       (T
        (PROGN
         (SETQ STACK (CONS (CONS 2 (CONS U V)) STACK))
         (SETQ U (CDR U))
         (GO TOP))))
     DONE
      (COND ((NULL STACK) (RETURN R)))
      (SETQ W (CAR STACK))
      (SETQ STACK (CDR STACK))
      (COND
       ((EQUAL (CAR W) 1)
        (PROGN
         (SETQ W (CDR W))
         (SETQ X (CAR W))
         (SETQ W (CDR W))
         (SETQ U (CAR W))
         (SETQ V (CDR W))
         (SETQ STACK (CONS (CONS 3 (CONS R (CONS X (CONS U V)))) STACK))
         (SETQ U (LIST (CAR U)))
         (SETQ V (CDR V))
         (GO TOP)))
       ((EQUAL (CAR W) 2)
        (PROGN
         (SETQ W (CDR W))
         (SETQ U (CAR W))
         (SETQ V (CDR W))
         (SETQ STACK (CONS (CONS 4 R) STACK))
         (SETQ U (LIST (CAR U)))
         (SETQ V (CDR V))
         (GO TOP)))
       ((EQUAL (CAR W) 3)
        (PROGN
         (SETQ W (CDR W))
         (SETQ R1 (CAR W))
         (SETQ W (CDR W))
         (SETQ X (CAR W))
         (SETQ W (CDR W))
         (SETQ U (CAR W))
         (SETQ V (CDR W))
         (SETQ W (|C:SUBS2MULTF| (CDAR U) (CDAR V)))
         (COND ((CAR X) (SETQ W (NEGF W))))
         (SETQ R (|C:EXTADD| (CONS (CONS (CDR X) W) NIL) (|C:EXTADD| R R1)))
         (GO DONE)))
       ((EQUAL (CAR W) 4) (PROGN (SETQ R (|C:EXTADD| R (CDR W))) (GO DONE))))
      (REDERR "should never get here"))) 
(PUT '|C:SUBS2MULTF| 'NUMBER-OF-ARGS 2) 
(PUT '|C:SUBS2MULTF| 'DEFINED-ON-LINE '398) 
(PUT '|C:SUBS2MULTF| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:SUBS2MULTF| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:SUBS2MULTF| (U V)
    ((LAMBDA (*SUB2)
       ((LAMBDA (X)
          (COND ((NEQ (CDR X) 1) (RERROR 'MATRIX 14 "Sub error in glnrsolve"))
                (T (CAR X))))
        (SUBS2
         (CONS (COND (*PHYSOP-LOADED (PHYSOP-MULTF U V)) (T (POLY-MULTF U V)))
               1))))
     *SUB2)) 
(PUT '|C:EXTADD| 'NUMBER-OF-ARGS 2) 
(PUT '|C:EXTADD| 'DEFINED-ON-LINE '415) 
(PUT '|C:EXTADD| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:EXTADD| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:EXTADD| (U V)
    (PROG (X R W)
     TOP
      (COND ((NULL U) (PROGN (SETQ R V) (GO DONE)))
            ((NULL V) (PROGN (SETQ R U) (GO DONE)))
            ((EQUAL (CAAR U) (CAAR V))
             (PROGN
              (SETQ X (ADDF (CDAR U) (CDAR V)))
              (COND
               ((NULL X) (PROGN (SETQ U (CDR U)) (SETQ V (CDR V)) (GO TOP)))
               (T
                (PROGN
                 (SETQ W (CONS (CONS (CAAR U) X) W))
                 (SETQ U (CDR U))
                 (SETQ V (CDR V))
                 (GO TOP))))))
            ((|C:ORDEXP| (CAAR U) (CAAR V))
             (PROGN (SETQ W (CONS (CAR U) W)) (SETQ U (CDR U)) (GO TOP)))
            (T (PROGN (SETQ W (CONS (CAR V) W)) (SETQ V (CDR V)) (GO TOP))))
     DONE
      (PROG ()
       WHILELABEL
        (COND ((NOT W) (RETURN NIL)))
        (PROGN (SETQ R (CONS (CAR W) R)) (SETQ W (CDR W)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT '|C:ORDEXP| 'NUMBER-OF-ARGS 2) 
(PUT '|C:ORDEXP| 'DEFINED-ON-LINE '447) 
(PUT '|C:ORDEXP| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:ORDEXP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:ORDEXP| (U V)
    (COND ((NULL U) T) ((EQUAL (CAR U) (CAR V)) (|C:ORDEXP| (CDR U) (CDR V)))
          (T (|C:ORDXP| (CAR U) (CAR V))))) 
(PUT '|C:ORDEXN| 'NUMBER-OF-ARGS 2) 
(PUT '|C:ORDEXN| 'DEFINED-ON-LINE '452) 
(PUT '|C:ORDEXN| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:ORDEXN| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:ORDEXN| (U V)
    (PROG (S X)
     A
      (COND ((NULL V) (RETURN (CONS S (REVERSE (CONS U X)))))
            ((OR (EQUAL U (CAR V)) (AND (PAIRP U) (PAIRP (CAR V))))
             (RETURN NIL))
            ((|C:ORDXP| U (CAR V))
             (RETURN (CONS S (APPEND (REVERSE (CONS U X)) V))))
            (T
             (PROGN
              (SETQ X (CONS (CAR V) X))
              (SETQ V (CDR V))
              (SETQ S (NOT S)))))
      (GO A))) 
(PUT '|C:ORDXP| 'NUMBER-OF-ARGS 2) 
(PUT '|C:ORDXP| 'DEFINED-ON-LINE '468) 
(PUT '|C:ORDXP| 'DEFINED-IN-FILE 'MATRIX/GLMAT.RED) 
(PUT '|C:ORDXP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |C:ORDXP| (U V)
    (COND ((PAIRP U) (COND ((PAIRP V) (LESSP (CDR U) (CDR V))) (T NIL)))
          ((PAIRP V) T) (T (LESSP U V)))) 
(ENDMODULE) 