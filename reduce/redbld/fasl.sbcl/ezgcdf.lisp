(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EZGCDF)) 
(FLUID
 '(*EXP *GCD *HEUGCD *OVERVIEW *TRFAC ALPHALIST BAD-CASE BEST-KNOWN-FACTORS
   CURRENT-MODULUS DMODE* FACTOR-LEVEL FACTOR-TRACE-LIST FULL-GCD
   HENSEL-GROWTH-SIZE IMAGE-FACTORS IMAGE-SET IRREDUCIBLE KORD*
   M-IMAGE-VARIABLE MULTIVARIATE-FACTORS MULTIVARIATE-INPUT-POLY NON-MONIC
   NO-OF-PRIMES-TO-TRY NUMBER-OF-FACTORS PRIME-BASE RECONSTRUCTING-GCD
   REDUCED-DEGREE-LCLST REDUCTION-COUNT TARGET-FACTOR-COUNT
   TRUE-LEADING-COEFFTS UNLUCKY-CASE)) 
(GLOBAL '(ERFG*)) 
(PUT 'EZGCDF 'NUMBER-OF-ARGS 2) 
(PUT 'EZGCDF 'DEFINED-ON-LINE '64) 
(PUT 'EZGCDF 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'EZGCDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EZGCDF (U V)
    (PROG (ERFGX KORDX X)
      (SETQ ERFGX ERFG*)
      (SETQ KORDX KORD*)
      (SETQ X (ERRORSET2 (LIST 'EZGCDF1 (MKQUOTE U) (MKQUOTE V))))
      (COND ((ERRORP X) (PROGN (SETQ ERFG* ERFGX) (SETKORDER KORDX))))
      (COND ((NULL (ERRORP X)) (RETURN (FIRST X))) (T (RETURN (GCDF1 U V)))))) 
(PUT 'EZGCDF1 'NUMBER-OF-ARGS 2) 
(PUT 'EZGCDF1 'DEFINED-ON-LINE '81) 
(PUT 'EZGCDF1 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'EZGCDF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EZGCDF1 (U V) ((LAMBDA (FACTOR-LEVEL) (POLY-ABS (GCDLIST (LIST U V)))) 0)) 
(PUT 'SIMPNPRIMITIVE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPNPRIMITIVE 'DEFINED-ON-LINE '100) 
(PUT 'SIMPNPRIMITIVE 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'SIMPNPRIMITIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPNPRIMITIVE (P)
    (PROG (NP DP)
      (COND
       ((OR (ATOM P) (NOT (ATOM (CDR P))))
        (RERROR 'EZGCD 2 "nprimitive requires just one argument")))
      (SETQ P (SIMP* (CAR P)))
      (COND ((NULL (CAR P)) (RETURN (CONS NIL 1))))
      (SETQ NP (QUOTFAIL (CAR P) (NUMERIC-CONTENT (CAR P))))
      (SETQ DP (QUOTFAIL (CDR P) (NUMERIC-CONTENT (CDR P))))
      (RETURN (CONS NP DP)))) 
(PUT 'NPRIMITIVE 'SIMPFN 'SIMPNPRIMITIVE) 
(PUT 'POLY-GCD 'NUMBER-OF-ARGS 2) 
(PUT 'POLY-GCD 'DEFINED-ON-LINE '116) 
(PUT 'POLY-GCD 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'POLY-GCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLY-GCD (U V)
    (PROG (*EXP Z)
      (COND ((NULL U) (RETURN (POLY-ABS V))) ((NULL V) (RETURN (POLY-ABS U)))
            ((OR (EQUAL U 1) (EQUAL V 1)) (RETURN 1)))
      (SETQ *EXP T)
      (COND ((QUOTF1 U V) (SETQ Z V)) ((QUOTF1 V U) (SETQ Z U))
            (*GCD (SETQ Z (GCDLIST (LIST U V)))) (T (SETQ Z 1)))
      (RETURN (POLY-ABS Z)))) 
(PUT 'EZGCD-COMFAC 'NUMBER-OF-ARGS 1) 
(PUT 'EZGCD-COMFAC 'DEFINED-ON-LINE '136) 
(PUT 'EZGCD-COMFAC 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'EZGCD-COMFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EZGCD-COMFAC (P)
    (COND ((OR (ATOM P) (ATOM (CAR P))) (CONS NIL (POLY-ABS P)))
          ((NULL (CDR P)) (CONS (CAAR P) (POLY-ABS (CDAR P))))
          (T
           (PROG (POWER COEFLIST VAR)
             (SETQ VAR (CAAAR P))
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (AND (EQUAL (CAAAR P) VAR)
                       (NOT (OR (ATOM (CDR P)) (ATOM (CAR (CDR P)))))))
                 (RETURN NIL)))
               (PROGN
                (SETQ COEFLIST (CONS (CDAR P) COEFLIST))
                (SETQ P (CDR P)))
               (GO WHILELABEL))
             (COND
              ((EQUAL (CAAAR P) VAR)
               (PROGN
                (SETQ COEFLIST (CONS (CDAR P) COEFLIST))
                (COND ((NULL (CDR P)) (SETQ POWER (CAAR P)))
                      (T (SETQ COEFLIST (CONS (CDR P) COEFLIST))))))
              (T (SETQ COEFLIST (CONS P COEFLIST))))
             (RETURN (CONS POWER (GCDLIST COEFLIST))))))) 
(PUT 'GCD-WITH-NUMBER 'NUMBER-OF-ARGS 2) 
(PUT 'GCD-WITH-NUMBER 'DEFINED-ON-LINE '161) 
(PUT 'GCD-WITH-NUMBER 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCD-WITH-NUMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCD-WITH-NUMBER (N A)
    (COND ((OR (EQUAL N 1) (NOT (ATOM N)) (FLAGP DMODE* 'FIELD)) 1)
          ((OR (ATOM A) (ATOM (CAR A)))
           (COND ((EQUAL A NIL) (ABS N)) ((NOT (ATOM A)) 1) (T (GCDDD N A))))
          (T (GCD-WITH-NUMBER (GCD-WITH-NUMBER N (CDAR A)) (CDR A))))) 
(PUT 'CONTENTS-WITH-RESPECT-TO 'NUMBER-OF-ARGS 2) 
(PUT 'CONTENTS-WITH-RESPECT-TO 'DEFINED-ON-LINE '174) 
(PUT 'CONTENTS-WITH-RESPECT-TO 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'CONTENTS-WITH-RESPECT-TO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONTENTS-WITH-RESPECT-TO (P V)
    (COND ((OR (ATOM P) (ATOM (CAR P))) (CONS NIL (POLY-ABS P)))
          ((EQUAL (CAAAR P) V) (EZGCD-COMFAC P))
          (T
           (PROG (W Y)
             (SETQ Y (UPDKORDER V))
             (SETQ W (EZGCD-COMFAC (REORDER P)))
             (SETKORDER Y)
             (RETURN W))))) 
(PUT 'NUMERIC-CONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'NUMERIC-CONTENT 'DEFINED-ON-LINE '185) 
(PUT 'NUMERIC-CONTENT 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'NUMERIC-CONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMERIC-CONTENT (FORM)
    (COND ((OR (ATOM FORM) (ATOM (CAR FORM))) (ABSF FORM))
          ((NULL (CDR FORM)) (NUMERIC-CONTENT (CDAR FORM)))
          (T
           (PROG (G1)
             (SETQ G1 (NUMERIC-CONTENT (CDAR FORM)))
             (COND
              ((NOT (EQUAL G1 1))
               (SETQ G1 (GCDDD G1 (NUMERIC-CONTENT (CDR FORM))))))
             (RETURN G1))))) 
(PUT 'GCDLIST 'NUMBER-OF-ARGS 1) 
(PUT 'GCDLIST 'DEFINED-ON-LINE '196) 
(PUT 'GCDLIST 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GCDLIST (L)
    (COND ((NULL L) NIL) ((NULL (CDR L)) (POLY-ABS (CAR L)))
          ((OR (ATOM (CAR L)) (ATOM (CAR (CAR L)))) (GCDLD (CDR L) (CAR L)))
          (T
           (PROG (L1 GCONT X)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (NULL L))) (RETURN NIL)))
               (PROGN
                (COND ((NULL (CAR L)) (SETQ L (CDR L)))
                      ((OR (ATOM (CAR L)) (ATOM (CAR (CAR L))))
                       (PROGN
                        (SETQ L1
                                (LIST
                                 (LIST
                                  (GCDLD (CDR L)
                                   (GCDLD (MAPCARCAR L1) (CAR L))))))
                        (SETQ L NIL)))
                      (T
                       (PROGN
                        (SETQ L1 (CONS (CONS (CAR L) (POWERS1 (CAR L))) L1))
                        (SETQ L (CDR L))))))
               (GO WHILELABEL))
             (COND ((NULL L1) (RETURN NIL))
                   ((NULL (CDR L1)) (RETURN (POLY-ABS (CAAR L1)))))
             (SETQ GCONT NIL)
             (SETQ X NIL)
             (SETQ L
                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                       (SETQ P L1)
                       (COND ((NULL P) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (P)
                                           (PROG (GCONT1 GCONT2 W)
                                             (SETQ W
                                                     (PROG (Y FORALL-RESULT
                                                            FORALL-ENDPTR)
                                                       (SETQ Y (CDR P))
                                                       (COND
                                                        ((NULL Y)
                                                         (RETURN NIL)))
                                                       (SETQ FORALL-RESULT
                                                               (SETQ FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              Y)
                                                                           (PROGN
                                                                            (SETQ GCONT1
                                                                                    (CONS
                                                                                     (CONS
                                                                                      (CAR
                                                                                       Y)
                                                                                      (CDDR
                                                                                       Y))
                                                                                     GCONT1))
                                                                            (CONS
                                                                             (CAR
                                                                              Y)
                                                                             (DIFFERENCE
                                                                              (CADR
                                                                               Y)
                                                                              (CDDR
                                                                               Y)))))
                                                                         (CAR
                                                                          Y))
                                                                        NIL)))
                                                      LOOPLABEL
                                                       (SETQ Y (CDR Y))
                                                       (COND
                                                        ((NULL Y)
                                                         (RETURN
                                                          FORALL-RESULT)))
                                                       (RPLACD FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (Y)
                                                                   (PROGN
                                                                    (SETQ GCONT1
                                                                            (CONS
                                                                             (CONS
                                                                              (CAR
                                                                               Y)
                                                                              (CDDR
                                                                               Y))
                                                                             GCONT1))
                                                                    (CONS
                                                                     (CAR Y)
                                                                     (DIFFERENCE
                                                                      (CADR Y)
                                                                      (CDDR
                                                                       Y)))))
                                                                 (CAR Y))
                                                                NIL))
                                                       (SETQ FORALL-ENDPTR
                                                               (CDR
                                                                FORALL-ENDPTR))
                                                       (GO LOOPLABEL)))
                                             (SETQ GCONT2
                                                     (NUMERIC-CONTENT (CAR P)))
                                             (COND
                                              ((NULL X)
                                               (PROGN
                                                (SETQ GCONT GCONT1)
                                                (SETQ X GCONT2)))
                                              (T
                                               (PROGN
                                                (SETQ GCONT
                                                        (VINTERSECTION GCONT
                                                         GCONT1))
                                                (SETQ X (GCDDD X GCONT2)))))
                                             (PROG (Q)
                                               (SETQ Q GCONT1)
                                              LAB
                                               (COND ((NULL Q) (RETURN NIL)))
                                               ((LAMBDA (Q)
                                                  (COND
                                                   ((NOT (EQUAL (CDR Q) 0))
                                                    (SETQ GCONT2
                                                            ((LAMBDA (G544)
                                                               (COND
                                                                (*PHYSOP-LOADED
                                                                 (PHYSOP-MULTF
                                                                  GCONT2 G544))
                                                                (T
                                                                 (POLY-MULTF
                                                                  GCONT2
                                                                  G544))))
                                                             (LIST
                                                              (CONS
                                                               (GETPOWER
                                                                (FKERN (CAR Q))
                                                                (CDR Q))
                                                               1)))))))
                                                (CAR Q))
                                               (SETQ Q (CDR Q))
                                               (GO LAB))
                                             (RETURN
                                              (CONS
                                               (QUOTFAIL1 (CAR P) GCONT2
                                                "Term content division failed")
                                               W))))
                                         (CAR P))
                                        NIL)))
                      LOOPLABEL
                       (SETQ P (CDR P))
                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P)
                                   (PROG (GCONT1 GCONT2 W)
                                     (SETQ W
                                             (PROG (Y FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ Y (CDR P))
                                               (COND ((NULL Y) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (Y)
                                                                   (PROGN
                                                                    (SETQ GCONT1
                                                                            (CONS
                                                                             (CONS
                                                                              (CAR
                                                                               Y)
                                                                              (CDDR
                                                                               Y))
                                                                             GCONT1))
                                                                    (CONS
                                                                     (CAR Y)
                                                                     (DIFFERENCE
                                                                      (CADR Y)
                                                                      (CDDR
                                                                       Y)))))
                                                                 (CAR Y))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ Y (CDR Y))
                                               (COND
                                                ((NULL Y)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (Y)
                                                           (PROGN
                                                            (SETQ GCONT1
                                                                    (CONS
                                                                     (CONS
                                                                      (CAR Y)
                                                                      (CDDR Y))
                                                                     GCONT1))
                                                            (CONS (CAR Y)
                                                                  (DIFFERENCE
                                                                   (CADR Y)
                                                                   (CDDR Y)))))
                                                         (CAR Y))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                     (SETQ GCONT2 (NUMERIC-CONTENT (CAR P)))
                                     (COND
                                      ((NULL X)
                                       (PROGN
                                        (SETQ GCONT GCONT1)
                                        (SETQ X GCONT2)))
                                      (T
                                       (PROGN
                                        (SETQ GCONT
                                                (VINTERSECTION GCONT GCONT1))
                                        (SETQ X (GCDDD X GCONT2)))))
                                     (PROG (Q)
                                       (SETQ Q GCONT1)
                                      LAB
                                       (COND ((NULL Q) (RETURN NIL)))
                                       ((LAMBDA (Q)
                                          (COND
                                           ((NOT (EQUAL (CDR Q) 0))
                                            (SETQ GCONT2
                                                    ((LAMBDA (G544)
                                                       (COND
                                                        (*PHYSOP-LOADED
                                                         (PHYSOP-MULTF GCONT2
                                                          G544))
                                                        (T
                                                         (POLY-MULTF GCONT2
                                                                     G544))))
                                                     (LIST
                                                      (CONS
                                                       (GETPOWER
                                                        (FKERN (CAR Q))
                                                        (CDR Q))
                                                       1)))))))
                                        (CAR Q))
                                       (SETQ Q (CDR Q))
                                       (GO LAB))
                                     (RETURN
                                      (CONS
                                       (QUOTFAIL1 (CAR P) GCONT2
                                        "Term content division failed")
                                       W))))
                                 (CAR P))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (PROG (Q)
               (SETQ Q GCONT)
              LAB
               (COND ((NULL Q) (RETURN NIL)))
               ((LAMBDA (Q)
                  (SETQ X
                          ((LAMBDA (G546)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF X G546))
                                   (T (POLY-MULTF X G546))))
                           (LIST
                            (CONS (GETPOWER (FKERN (CAR Q)) (CDR Q)) 1)))))
                (CAR Q))
               (SETQ Q (CDR Q))
               (GO LAB))
             (RETURN
              (POLY-ABS
               ((LAMBDA (G548)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF X G548))
                        (T (POLY-MULTF X G548))))
                (GCDLIST1 L)))))))) 
(PUT 'GCDLIST1 'NUMBER-OF-ARGS 1) 
(PUT 'GCDLIST1 'DEFINED-ON-LINE '258) 
(PUT 'GCDLIST1 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GCDLIST1 (L)
    (PROG (UNIONV INTERSECTIONV VORD X L1 REDUCTION-COUNT)
      (SETQ UNIONV (SETQ INTERSECTIONV (CDAR L)))
      (PROG (P)
        (SETQ P (CDR L))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ UNIONV (VUNION UNIONV (CDR P)))
            (SETQ INTERSECTIONV (VINTERSECTION INTERSECTIONV (CDR P)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND ((NULL INTERSECTIONV) (RETURN 1)))
      (PROG (V)
        (SETQ V INTERSECTIONV)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ UNIONV (VDELETE V UNIONV))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ INTERSECTIONV (SORT INTERSECTIONV (FUNCTION LESSPCDR)))
      (COND
       ((EQUAL (CDAR INTERSECTIONV) 1)
        (PROGN
         (SETQ VORD (MAPCARCAR (APPEND INTERSECTIONV UNIONV)))
         (SETQ L1 (SETKORDER VORD))
         (SETQ X
                 (GCDLIST3
                  (PROG (P FORALL-RESULT FORALL-ENDPTR)
                    (SETQ P L)
                    (COND ((NULL P) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (P) (REORDER (CAR P))) (CAR P))
                                     NIL)))
                   LOOPLABEL
                    (SETQ P (CDR P))
                    (COND ((NULL P) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (P) (REORDER (CAR P))) (CAR P))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  NIL VORD))
         (SETKORDER L1)
         (RETURN (REORDER X))))
       ((NULL UNIONV) (RETURN (GCDLIST2 L INTERSECTIONV))))
      (SETQ VORD (SETKORDER (MAPCARCAR (APPEND UNIONV INTERSECTIONV))))
      (SETQ L1 NIL)
      (PROG (P)
        (SETQ P L)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ L1
                   (SPLIT-WRT-VARIABLES (REORDER (CAR P)) (MAPCARCAR UNIONV)
                    L1)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETKORDER VORD)
      (RETURN
       (GCDLIST1
        (PROG (P FORALL-RESULT FORALL-ENDPTR)
          (SETQ P L1)
          (COND ((NULL P) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P)
                              (CONS (REORDER P)
                                    (TOTAL-DEGREE-IN-POWERS P NIL)))
                            (CAR P))
                           NIL)))
         LOOPLABEL
          (SETQ P (CDR P))
          (COND ((NULL P) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (P)
                      (CONS (REORDER P) (TOTAL-DEGREE-IN-POWERS P NIL)))
                    (CAR P))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'GCDLIST2 'NUMBER-OF-ARGS 2) 
(PUT 'GCDLIST2 'DEFINED-ON-LINE '297) 
(PUT 'GCDLIST2 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCDLIST2 (L VARS)
    (PROG (X X1 GG LMODP ONESTEP VORD OLDMOD IMAGE-SET GCDPOW UNLUCKY-CASE)
      (COND
       ((NULL (CDR VARS))
        (RETURN
         (COND ((AND *HEUGCD (SETQ X (HEU-GCD-LIST (MAPCARCAR L)))) X)
               (T (GCDLIST3 (MAPCARCAR L) NIL (LIST (CAAR VARS))))))))
      (SETQ OLDMOD (SET-MODULUS NIL))
      (SETQ VARS (MAPCARCAR (SORT VARS (FUNCTION GREATERPCDR))))
      (SETQ L
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P L)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS (CAR P)
                                          (SORT (CDR P) (FUNCTION LESSPCDR))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (CAR P) (SORT (CDR P) (FUNCTION LESSPCDR))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ L (SORT L (FUNCTION LESSPCDADR)))
      (SETQ X (INTERSECTION (DEG2VARS (CDAR L)) (DEG2VARS (CDADR L))))
      (COND ((NOT (NULL X)) (PROGN (GO X-TO-TOP))))
     TRY-AGAIN
      (SET-MODULUS (RANDOM-PRIME))
      (SETQ UNLUCKY-CASE NIL)
      (SETQ IMAGE-SET
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V VARS)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (CONS V
                                          (MODULAR-NUMBER
                                           (NEXT-RANDOM-NUMBER))))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V)
                            (CONS V (MODULAR-NUMBER (NEXT-RANDOM-NUMBER))))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X1 VARS)
     TRY-VARS
      (COND ((NULL X1) (GO IMAGES-TRIED)))
      (SETQ LMODP
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P L)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (MAKE-IMAGE-MOD-P (CAR P) (CAR X1)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P) (MAKE-IMAGE-MOD-P (CAR P) (CAR X1)))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (UNLUCKY-CASE (GO TRY-AGAIN)))
      (SETQ LMODP (SORT LMODP (FUNCTION LESSPDEG)))
      (SETQ GG (GCDLIST-MOD-P (CAR LMODP) (CDR LMODP)))
      (COND
       ((OR (OR (ATOM GG) (ATOM (CAR GG)))
            (AND (LESSP REDUCTION-COUNT 2) (SETQ ONESTEP T)))
        (PROGN (SETQ X (LIST (CAR X1))) (GO X-TO-TOP))))
      (SETQ GCDPOW (CONS (CONS (CAR X1) (CDAAR GG)) GCDPOW))
      (SETQ X1 (CDR X1))
      (GO TRY-VARS)
     IMAGES-TRIED
      (SETQ VORD (MAPCARCAR (SORT GCDPOW (FUNCTION GREATERPCDR))))
      (GO ORDER-CHOSEN)
     X-TO-TOP
      (PROG (V)
        (SETQ V X)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ VARS (DELETE V VARS))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (SETQ VORD (APPEND X VARS))
     ORDER-CHOSEN
      (SET-MODULUS OLDMOD)
      (SETQ VARS (SETKORDER VORD))
      (SETQ X
              (GCDLIST3
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P L)
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (P) (REORDER (CAR P))) (CAR P))
                                       NIL)))
                LOOPLABEL
                 (SETQ P (CDR P))
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (REORDER (CAR P))) (CAR P)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               ONESTEP VORD))
      (SETKORDER VARS)
      (RETURN (REORDER X)))) 
(PUT 'GCDLIST-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'GCDLIST-MOD-P 'DEFINED-ON-LINE '378) 
(PUT 'GCDLIST-MOD-P 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST-MOD-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCDLIST-MOD-P (GG L)
    (COND ((NULL L) GG) ((EQUAL GG 1) 1)
          (T (GCDLIST-MOD-P (GCD-MOD-P GG (CAR L)) (CDR L))))) 
(PUT 'DEG2VARS 'NUMBER-OF-ARGS 1) 
(PUT 'DEG2VARS 'DEFINED-ON-LINE '383) 
(PUT 'DEG2VARS 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'DEG2VARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEG2VARS (L)
    (COND ((NULL L) NIL) ((GREATERP (CDAR L) 2) NIL)
          (T (CONS (CAAR L) (DEG2VARS (CDR L)))))) 
(PUT 'VDELETE 'NUMBER-OF-ARGS 2) 
(PUT 'VDELETE 'DEFINED-ON-LINE '388) 
(PUT 'VDELETE 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'VDELETE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VDELETE (A B)
    (COND ((NULL B) NIL) ((EQUAL (CAR A) (CAAR B)) (CDR B))
          (T (CONS (CAR B) (VDELETE A (CDR B)))))) 
(PUT 'VINTERSECTION 'NUMBER-OF-ARGS 2) 
(PUT 'VINTERSECTION 'DEFINED-ON-LINE '393) 
(PUT 'VINTERSECTION 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'VINTERSECTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VINTERSECTION (A B)
    (PROG (C)
      (RETURN
       (COND ((NULL A) NIL)
             ((NULL (SETQ C (ASSOC (CAAR A) B))) (VINTERSECTION (CDR A) B))
             ((GREATERP (CDAR A) (CDR C))
              (COND ((EQUAL (CDR C) 0) (VINTERSECTION (CDR A) B))
                    (T (CONS C (VINTERSECTION (CDR A) B)))))
             ((EQUAL (CDAR A) 0) (VINTERSECTION (CDR A) B))
             (T (CONS (CAR A) (VINTERSECTION (CDR A) B))))))) 
(PUT 'VUNION 'NUMBER-OF-ARGS 2) 
(PUT 'VUNION 'DEFINED-ON-LINE '406) 
(PUT 'VUNION 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'VUNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VUNION (A B)
    (PROG (C)
      (RETURN
       (COND ((NULL A) B)
             ((NULL (SETQ C (ASSOC (CAAR A) B)))
              (CONS (CAR A) (VUNION (CDR A) B)))
             ((GREATERP (CDAR A) (CDR C))
              (CONS (CAR A) (VUNION (CDR A) (DELETE C B))))
             (T (CONS C (VUNION (CDR A) (DELETE C B)))))))) 
(PUT 'MAPCARCAR 'NUMBER-OF-ARGS 1) 
(PUT 'MAPCARCAR 'DEFINED-ON-LINE '416) 
(PUT 'MAPCARCAR 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAPCARCAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAPCARCAR (L)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X L)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'GCDLD 'NUMBER-OF-ARGS 2) 
(PUT 'GCDLD 'DEFINED-ON-LINE '420) 
(PUT 'GCDLD 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GCDLD (L N)
    (COND ((OR (EQUAL N 1) (EQUAL N (MINUS 1))) 1) ((EQUAL L NIL) (ABS N))
          ((EQUAL (CAR L) NIL) (GCDLD (CDR L) N))
          (T (GCDLD (CDR L) (GCD-WITH-NUMBER N (CAR L)))))) 
(PUT 'SPLIT-WRT-VARIABLES 'NUMBER-OF-ARGS 3) 
(PUT 'SPLIT-WRT-VARIABLES 'DEFINED-ON-LINE '427) 
(PUT 'SPLIT-WRT-VARIABLES 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'SPLIT-WRT-VARIABLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPLIT-WRT-VARIABLES (P VL L)
    (COND ((EQUAL P NIL) L) ((AND (NOT (NULL L)) (EQUAL (CAR L) 1)) L)
          ((OR (ATOM P) (ATOM (CAR P))) (CONS (ABS P) L))
          ((MEMBER (CAAAR P) VL)
           (SPLIT-WRT-VARIABLES (CDR P) VL
            (SPLIT-WRT-VARIABLES (CDAR P) VL L)))
          (T (CONS P L)))) 
(PUT 'GCDLIST3 'NUMBER-OF-ARGS 3) 
(PUT 'GCDLIST3 'DEFINED-ON-LINE '437) 
(PUT 'GCDLIST3 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GCDLIST3 (L ONESTEP VLIST)
    (PROG (UNLUCKY-CASE IMAGE-SET GG GCONT L1 W W1 W2 REDUCED-DEGREE-LCLST P1
           P2)
      (SETQ L1
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P L)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (CONS P (EZGCD-COMFAC P)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CONS P (EZGCD-COMFAC P))) (CAR P))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ L
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C L1)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C)
                                    (QUOTFAIL1 (CAR C) (COMFAC-TO-POLY (CDR C))
                                     "Content divison in gcdlist3 failed"))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (C)
                            (QUOTFAIL1 (CAR C) (COMFAC-TO-POLY (CDR C))
                             "Content divison in gcdlist3 failed"))
                          (CAR C))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ GCONT
              (GCDLIST
               (PROG (C FORALL-RESULT FORALL-ENDPTR)
                 (SETQ C L1)
                 (COND ((NULL C) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (C) (CDDR C)) (CAR C)) NIL)))
                LOOPLABEL
                 (SETQ C (CDR C))
                 (COND ((NULL C) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (C) (CDDR C)) (CAR C)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       ((OR (ATOM GCONT) (ATOM (CAR GCONT)))
        (COND ((NOT (EQUAL GCONT 1)) (ERRORF "gcont has numeric part")))))
      (SETQ L
              (SORT
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P L)
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (P) (POLY-ABS P)) (CAR P))
                                       NIL)))
                LOOPLABEL
                 (SETQ P (CDR P))
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (POLY-ABS P)) (CAR P)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (FUNCTION ORDP)))
      (SETQ W NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT L) (RETURN NIL)))
        (PROGN
         (SETQ W (CONS (CAR L) W))
         (PROG ()
          REPEATLABEL
           (SETQ L (CDR L))
           (COND
            ((NOT (OR (NULL L) (NOT (EQUAL (CAR W) (CAR L)))))
             (GO REPEATLABEL)))))
        (GO WHILELABEL))
      (SETQ L (REVERSIP W))
      (SETQ W NIL)
      (COND
       ((NULL (CDR L))
        (RETURN
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF GCONT (CAR L)))
               (T (POLY-MULTF GCONT (CAR L)))))))
      (COND
       (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
         (SETQ GG (CAR (SETQ L (SORT L (FUNCTION DEGREE-ORDER))))))
        (RETURN GCONT)))
      (COND
       ((EQUAL (CDAAR GG) 1)
        (PROGN
         (COND
          ((DIVISION-TEST GG L)
           (RETURN
            ((LAMBDA (G549)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF G549 GCONT))
                     (T (POLY-MULTF G549 GCONT))))
             (POLY-ABS GG))))
          (T (RETURN GCONT))))))
      (COND
       (ONESTEP
        (PROGN
         (SETQ P1 (POLY-ABS (CAR L)))
         (SETQ P2 (POLY-ABS (CADR L)))
         (COND
          ((EQUAL P1 P2)
           (PROGN
            (COND
             ((DIVISION-TEST P1 (CDDR L))
              (RETURN
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF P1 GCONT))
                     (T (POLY-MULTF P1 GCONT))))))))
          (T
           (PROGN
            (SETQ GG (POLY-GCD (CDAR P1) (CDAR P2)))
            (SETQ W1
                    ((LAMBDA (G552)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR P1) G552))
                             (T (POLY-MULTF (CDR P1) G552))))
                     (QUOTFAIL1 (CDAR P2) GG
                      "Division failure when just one pseudoremainder step needed")))
            (SETQ W2
                    ((LAMBDA (G554)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR P2) G554))
                             (T (POLY-MULTF (CDR P2) G554))))
                     (NEGF
                      (QUOTFAIL1 (CDAR P1) GG
                       "Division failure when just one pseudoremainder step needed"))))
            (SETQ W (DIFFERENCE (CDAAR P1) (CDAAR P2)))
            (COND
             ((GREATERP W 0)
              (SETQ W2
                      ((LAMBDA (G556)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF W2 G556))
                               (T (POLY-MULTF W2 G556))))
                       (CONS (CONS (GETPOWER (FKERN (CAAAR P2)) W) 1) NIL))))
             ((LESSP W 0)
              (SETQ W1
                      ((LAMBDA (G558)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF W1 G558))
                               (T (POLY-MULTF W1 G558))))
                       (CONS (CONS (GETPOWER (FKERN (CAAAR P1)) (MINUS W)) 1)
                             NIL)))))
            (SETQ GG (EZGCD-PP (ADDF W1 W2)))
            (COND
             ((DIVISION-TEST GG L)
              (RETURN
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF GG GCONT))
                     (T (POLY-MULTF GG GCONT))))))))))))
      (RETURN (GCDLIST31 L VLIST GCONT GG L1)))) 
(PUT 'GCDLIST31 'NUMBER-OF-ARGS 5) 
(PUT 'GCDLIST31 'DEFINED-ON-LINE '512) 
(PUT 'GCDLIST31 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'GCDLIST31 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GCDLIST31 (L VLIST GCONT GG L1)
    (PROG (COFACTOR LCG OLD-MODULUS PRIME W W1 ZEROS-LIST)
      (SETQ OLD-MODULUS (SET-MODULUS NIL))
      (SETQ LCG
              (PROG (POLY FORALL-RESULT FORALL-ENDPTR)
                (SETQ POLY L)
                (COND ((NULL POLY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (POLY) (CDAR POLY)) (CAR POLY))
                                      NIL)))
               LOOPLABEL
                (SETQ POLY (CDR POLY))
                (COND ((NULL POLY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (POLY) (CDAR POLY)) (CAR POLY)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LCG (GCDLIST LCG))
     TRY-AGAIN
      (SETQ UNLUCKY-CASE NIL)
      (SETQ IMAGE-SET NIL)
      (SET-MODULUS (SETQ PRIME (RANDOM-PRIME)))
      (SETQ W L)
      (COND
       ((NOT ZEROS-LIST)
        (PROGN
         (SETQ IMAGE-SET
                 (SETQ ZEROS-LIST (TRY-MAX-ZEROS-FOR-IMAGE-SET W VLIST)))
         NIL)))
      (SETQ GG (MAKE-IMAGE-MOD-P (CAR W) (CAR VLIST)))
      (COND (UNLUCKY-CASE (PROGN (GO TRY-AGAIN))))
      (SETQ L1 (LIST (CONS (CAR W) GG)))
     MAKE-IMAGES
      (COND ((NULL (SETQ W (CDR W))) (GO IMAGES-CREATED-SUCCESSFULLY)))
      (SETQ L1 (CONS (CONS (CAR W) (MAKE-IMAGE-MOD-P (CAR W) (CAR VLIST))) L1))
      (COND (UNLUCKY-CASE (PROGN (GO TRY-AGAIN))))
      (SETQ GG (GCD-MOD-P GG (CDAR L1)))
      (COND
       ((OR (ATOM GG) (ATOM (CAR GG)))
        (PROGN (SET-MODULUS OLD-MODULUS) (RETURN GCONT))))
      (GO MAKE-IMAGES)
     IMAGES-CREATED-SUCCESSFULLY
      (SETQ L1 (REVERSIP L1))
      (COND
       ((EQUAL (CDAAR GG) (CDAAR (CAR L)))
        (PROGN (SETQ GG (POLY-ABS (CAR L))) (GO RESULT)))
       ((AND (EQUAL (CDAAR (CAR L)) (ADD1 (CDAAR GG)))
             (EQUAL (CDAAR (CAR L)) (CDAAR (CADR L))))
        (PROGN
         (SETQ GG (POLY-GCD (CDAR (CAR L)) (CDAR (CADR L))))
         (SETQ GG
                 (EZGCD-PP
                  (ADDF
                   ((LAMBDA (G559 G560)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G559 G560))
                            (T (POLY-MULTF G559 G560))))
                    (CDR (CAR L))
                    (QUOTFAIL1 (CDAR (CADR L)) GG
                     "Division failure when just one pseudoremainder step needed"))
                   ((LAMBDA (G561 G562)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G561 G562))
                            (T (POLY-MULTF G561 G562))))
                    (CDR (CADR L))
                    (NEGF
                     (QUOTFAIL1 (CDAR (CAR L)) GG
                      "Divison failure when just one pseudoremainder step needed"))))))
         (GO RESULT))))
      (SETQ W L1)
     FIND-GOOD-COFACTOR
      (COND ((NULL W) (GO SPECIAL-CASE)))
      (COND
       (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
         (GCD-MOD-P GG (SETQ COFACTOR (QUOTIENT-MOD-P (CDAR W) GG))))
        (GO GOOD-COFACTOR-FOUND)))
      (SETQ W (CDR W))
      (GO FIND-GOOD-COFACTOR)
     GOOD-COFACTOR-FOUND
      (SETQ COFACTOR (MONIC-MOD-P COFACTOR))
      (SETQ W (CAAR W))
      (SETQ IMAGE-SET (SORT IMAGE-SET (FUNCTION ORDOPCAR)))
      (SETQ GG (RECONSTRUCT-GCD W GG COFACTOR PRIME IMAGE-SET LCG))
      (COND ((EQUAL GG 'NOGOOD) (GO TRY-AGAIN)))
      (GO RESULT)
     SPECIAL-CASE
      (SETQ REDUCED-DEGREE-LCLST NIL)
     TRY-REDUCED-DEGREE-AGAIN
      (SETQ W1 (REDUCED-DEGREE (CAADR L1) (CAAR L1)))
      (SETQ W (CAR W1))
      (SETQ W1 (CDR W1))
      (COND
       ((AND (NOT (OR (ATOM W) (ATOM (CAR W))))
             (OR (OR (ATOM W1) (ATOM (CAR W1))) (NEQ (CDAAR W) (CDAAR W1))))
        (GO TRY-AGAIN)))
      (COND
       ((AND (OR (ATOM W) (ATOM (CAR W))) (NOT (NULL W)))
        (PROGN (SET-MODULUS OLD-MODULUS) (RETURN GCONT))))
      (COND
       ((AND W (EQUAL (CDAAR W) (CDAAR GG))) (PROGN (SETQ GG W) (GO RESULT))))
      (COND
       ((NULL W)
        (PROGN
         (SETQ L1 (CONS (CAR L1) (CDDR L1)))
         (COND
          ((NULL (CDR L1)) (PROGN (SETQ GG (POLY-ABS (CAAR L1))) (GO RESULT))))
         (GO TRY-REDUCED-DEGREE-AGAIN))))
      (COND
       ((LEQ (CDAAR W) (CDAAR GG)) (PROGN (SETQ GG (POLY-ABS W)) (GO RESULT)))
       (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U))))
         (GCD-MOD-P GG (SETQ COFACTOR (QUOTIENT-MOD-P W1 GG))))
        (PROGN (SETQ W (LIST (LIST W))) (GO GOOD-COFACTOR-FOUND))))
      (SETQ L1
              (COND
               ((LEQ (CDAAR W) (CDAAR (CAAR L1)))
                (CONS (CONS W W1) (CONS (CAR L1) (CDDR L1))))
               (T (CONS (CAR L1) (CONS (CONS W W1) (CDDR L1))))))
      (GO TRY-REDUCED-DEGREE-AGAIN)
     RESULT
      (COND
       ((DIVISION-TEST GG L)
        (PROGN
         (SET-MODULUS OLD-MODULUS)
         (RETURN
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF GG GCONT))
                (T (POLY-MULTF GG GCONT)))))))
      (GO TRY-AGAIN))) 
(PUT 'MAKE-A-LIST-OF-VARIABLES 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-A-LIST-OF-VARIABLES 'DEFINED-ON-LINE '662) 
(PUT 'MAKE-A-LIST-OF-VARIABLES 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAKE-A-LIST-OF-VARIABLES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-A-LIST-OF-VARIABLES (L)
    (PROG (VLIST)
      (PROG (LL)
        (SETQ LL L)
       LAB
        (COND ((NULL LL) (RETURN NIL)))
        ((LAMBDA (LL) (SETQ VLIST (VARIABLES.IN.FORM LL VLIST))) (CAR LL))
        (SETQ LL (CDR LL))
        (GO LAB))
      (RETURN (MAKE-ORDER-CONSISTENT VLIST KORD*)))) 
(PUT 'MAKE-ORDER-CONSISTENT 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-ORDER-CONSISTENT 'DEFINED-ON-LINE '668) 
(PUT 'MAKE-ORDER-CONSISTENT 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAKE-ORDER-CONSISTENT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-ORDER-CONSISTENT (L M)
    (COND ((NULL L) NIL) ((NULL M) (ERRORF "Variable missing from kord*"))
          ((MEMBER (CAR M) L)
           (CONS (CAR M) (MAKE-ORDER-CONSISTENT (DELETE (CAR M) L) (CDR M))))
          (T (MAKE-ORDER-CONSISTENT L (CDR M))))) 
(PUT 'TRY-MAX-ZEROS-FOR-IMAGE-SET 'NUMBER-OF-ARGS 2) 
(PUT 'TRY-MAX-ZEROS-FOR-IMAGE-SET 'DEFINED-ON-LINE '677) 
(PUT 'TRY-MAX-ZEROS-FOR-IMAGE-SET 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'TRY-MAX-ZEROS-FOR-IMAGE-SET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRY-MAX-ZEROS-FOR-IMAGE-SET (L VLIST)
    (COND ((NULL VLIST) (ERROR 50 "vlist not set in try-max-zeros-..."))
          (T
           (PROG (Z)
             (SETQ Z
                     (PROG (V FORALL-RESULT FORALL-ENDPTR)
                       (SETQ V (CDR VLIST))
                       (COND ((NULL V) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (V)
                                           (COND
                                            ((OR
                                              ((LAMBDA (U)
                                                 (OR (ATOM U) (ATOM (CAR U))))
                                               (CDAR (CAR L)))
                                              (NULL
                                               ((LAMBDA (*EXP)
                                                  (QUOTF1 (CDAR (CAR L))
                                                          (LIST
                                                           (CONS (CONS V 1)
                                                                 1))))
                                                T)))
                                             (CONS V 0))
                                            (T
                                             (CONS V
                                                   (MODULAR-NUMBER
                                                    (NEXT-RANDOM-NUMBER))))))
                                         (CAR V))
                                        NIL)))
                      LOOPLABEL
                       (SETQ V (CDR V))
                       (COND ((NULL V) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (V)
                                   (COND
                                    ((OR
                                      ((LAMBDA (U)
                                         (OR (ATOM U) (ATOM (CAR U))))
                                       (CDAR (CAR L)))
                                      (NULL
                                       ((LAMBDA (*EXP)
                                          (QUOTF1 (CDAR (CAR L))
                                                  (LIST (CONS (CONS V 1) 1))))
                                        T)))
                                     (CONS V 0))
                                    (T
                                     (CONS V
                                           (MODULAR-NUMBER
                                            (NEXT-RANDOM-NUMBER))))))
                                 (CAR V))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (PROG (FF)
               (SETQ FF (CDR L))
              LAB
               (COND ((NULL FF) (RETURN NIL)))
               ((LAMBDA (FF)
                  (SETQ Z
                          (PROG (W FORALL-RESULT FORALL-ENDPTR)
                            (SETQ W Z)
                            (COND ((NULL W) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (W)
                                                (COND
                                                 ((ZEROP (CDR W))
                                                  (COND
                                                   ((OR
                                                     (OR (ATOM (CDAR FF))
                                                         (ATOM
                                                          (CAR (CDAR FF))))
                                                     (NULL
                                                      ((LAMBDA (*EXP)
                                                         (QUOTF1 (CDAR FF)
                                                                 (LIST
                                                                  (CONS
                                                                   (CONS
                                                                    (CAR W) 1)
                                                                   1))))
                                                       T)))
                                                    W)
                                                   (T
                                                    (CONS (CAR W)
                                                          (MODULAR-NUMBER
                                                           (NEXT-RANDOM-NUMBER))))))
                                                 (T W)))
                                              (CAR W))
                                             NIL)))
                           LOOPLABEL
                            (SETQ W (CDR W))
                            (COND ((NULL W) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (W)
                                        (COND
                                         ((ZEROP (CDR W))
                                          (COND
                                           ((OR
                                             (OR (ATOM (CDAR FF))
                                                 (ATOM (CAR (CDAR FF))))
                                             (NULL
                                              ((LAMBDA (*EXP)
                                                 (QUOTF1 (CDAR FF)
                                                         (LIST
                                                          (CONS
                                                           (CONS (CAR W) 1)
                                                           1))))
                                               T)))
                                            W)
                                           (T
                                            (CONS (CAR W)
                                                  (MODULAR-NUMBER
                                                   (NEXT-RANDOM-NUMBER))))))
                                         (T W)))
                                      (CAR W))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
                (CAR FF))
               (SETQ FF (CDR FF))
               (GO LAB))
             (RETURN Z))))) 
(PUT 'RECONSTRUCT-GCD 'NUMBER-OF-ARGS 6) 
(PUT 'RECONSTRUCT-GCD 'DEFINED-ON-LINE '693) 
(PUT 'RECONSTRUCT-GCD 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'RECONSTRUCT-GCD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RECONSTRUCT-GCD (FULL-POLY GG COFACTOR P IMSET LCG)
    (COND
     ((NULL
       (ADDF FULL-POLY
             (NEGF
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF GG COFACTOR))
                    (T (POLY-MULTF GG COFACTOR))))))
      GG)
     (T
      ((LAMBDA (FACTOR-LEVEL)
         (PROG (NUMBER-OF-FACTORS IMAGE-FACTORS TRUE-LEADING-COEFFTS
                MULTIVARIATE-INPUT-POLY NO-OF-PRIMES-TO-TRY IRREDUCIBLE
                NON-MONIC BAD-CASE TARGET-FACTOR-COUNT MULTIVARIATE-FACTORS
                HENSEL-GROWTH-SIZE ALPHALIST BEST-KNOWN-FACTORS PRIME-BASE
                M-IMAGE-VARIABLE RECONSTRUCTING-GCD FULL-GCD)
           (COND
            ((NOT (EQUAL CURRENT-MODULUS P))
             (ERRORF "gcdlist has not restored the modulus")))
           (COND ((POLY-MINUSP LCG) (ERROR 50 (LIST "Negative GCD: " LCG))))
           (SETQ FULL-POLY (POLY-ABS FULL-POLY))
           (INITIALISE-HENSEL-FLUIDS FULL-POLY GG COFACTOR P LCG)
           (COND ((EQUAL (DETERMINE-MORE-COEFFTS) 'DONE) (RETURN FULL-GCD)))
           (COND
            ((NULL ALPHALIST)
             (SETQ ALPHALIST
                     (ALPHAS 2
                      (LIST (GETV IMAGE-FACTORS 1) (GETV IMAGE-FACTORS 2))
                      1))))
           (COND
            ((EQUAL ALPHALIST '|FACTORS NOT COPRIME|)
             (ERRORF (LIST "image factors not coprime?" IMAGE-FACTORS))))
           (COND
            ((NOT *OVERVIEW)
             (PROG (STREAM)
               (COND
                ((OR *TRALLFAC (AND *TRFAC (EQUAL FACTOR-LEVEL 1)))
                 (SETQ STREAM (CONS NIL NIL)))
                (T (SETQ STREAM (ASSOC FACTOR-LEVEL FACTOR-TRACE-LIST))))
               (COND
                (STREAM
                 (PROGN
                  (SETQ STREAM (WRS (CDR STREAM)))
                  (PROGN
                   (PROGN
                    (PRIN2*
                     "The following modular polynomials are chosen such that:")
                    (TERPRI* NIL))
                   (TERPRI)
                   (PRIN2* "   a(2)*f(1) + a(1)*f(2) = 1 mod ")
                   (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL))
                   (TERPRI)
                   (PROGN
                    (PRIN2* "  where degree of a(1) < degree of f(1),")
                    (TERPRI* NIL))
                   (PROGN
                    (PRIN2* "    and degree of a(2) < degree of f(2),")
                    (TERPRI* NIL))
                   (PROGN (PRIN2* "    and") (TERPRI* NIL))
                   (PROG (I)
                     (SETQ I 1)
                    LAB
                     (COND ((MINUSP (DIFFERENCE 2 I)) (RETURN NIL)))
                     (PROGN
                      (PRIN2* "    a(")
                      (PRIN2* I)
                      (PRIN2* ")=")
                      (PRINTSF (CDR (GET-ALPHA (GETV IMAGE-FACTORS I))))
                      (PRIN2* "and f(")
                      (PRIN2* I)
                      (PRIN2* ")=")
                      (PRINTSF (GETV IMAGE-FACTORS I))
                      (TERPRI* T))
                     (SETQ I (PLUS2 I 1))
                     (GO LAB)))
                  (WRS STREAM)))))))
           (RECONSTRUCT-MULTIVARIATE-FACTORS
            (PROG (V FORALL-RESULT FORALL-ENDPTR)
              (SETQ V IMSET)
              (COND ((NULL V) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (V)
                                  (CONS (CAR V) (MODULAR-NUMBER (CDR V))))
                                (CAR V))
                               NIL)))
             LOOPLABEL
              (SETQ V (CDR V))
              (COND ((NULL V) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (V) (CONS (CAR V) (MODULAR-NUMBER (CDR V))))
                        (CAR V))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           (COND ((OR IRREDUCIBLE BAD-CASE) (RETURN 'NOGOOD))
                 (T (RETURN FULL-GCD)))))
       (PLUS FACTOR-LEVEL 1))))) 
(PUT 'INITIALISE-HENSEL-FLUIDS 'NUMBER-OF-ARGS 5) 
(PUT 'INITIALISE-HENSEL-FLUIDS 'DEFINED-ON-LINE '744) 
(PUT 'INITIALISE-HENSEL-FLUIDS 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'INITIALISE-HENSEL-FLUIDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INITIALISE-HENSEL-FLUIDS (FPOLY FAC1 FAC2 P LCF1)
    (PROG (LC1-IMAGE LC2-IMAGE)
      (SETQ RECONSTRUCTING-GCD T)
      (SETQ MULTIVARIATE-INPUT-POLY
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF FPOLY LCF1))
                    (T (POLY-MULTF FPOLY LCF1))))
      (SETQ NO-OF-PRIMES-TO-TRY 5)
      (SETQ PRIME-BASE (SETQ HENSEL-GROWTH-SIZE P))
      (SETQ NUMBER-OF-FACTORS 2)
      (SETQ LC1-IMAGE (MAKE-NUMERIC-IMAGE-MOD-P LCF1))
      (SETQ LC2-IMAGE (MAKE-NUMERIC-IMAGE-MOD-P (CDAR FPOLY)))
      (SETQ FAC1 (TIMES-MOD-P LC1-IMAGE FAC1))
      (SETQ FAC2 (TIMES-MOD-P LC2-IMAGE FAC2))
      (SETQ IMAGE-FACTORS (MKVECT 2))
      (SETQ TRUE-LEADING-COEFFTS (MKVECT 2))
      (PUTV IMAGE-FACTORS 1 FAC1)
      (PUTV IMAGE-FACTORS 2 FAC2)
      (PUTV TRUE-LEADING-COEFFTS 1 LCF1)
      (PUTV TRUE-LEADING-COEFFTS 2 (CDAR FPOLY))
      (SETQ NON-MONIC (NOT (EQUAL LCF1 1)))
      (SETQ M-IMAGE-VARIABLE (CAAAR FPOLY)))) 
(PUT 'DIVISION-TEST 'NUMBER-OF-ARGS 2) 
(PUT 'DIVISION-TEST 'DEFINED-ON-LINE '769) 
(PUT 'DIVISION-TEST 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'DIVISION-TEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIVISION-TEST (GG L)
    (COND ((NULL L) T) ((NULL ((LAMBDA (*EXP) (QUOTF1 (CAR L) GG)) T)) NIL)
          (T (DIVISION-TEST GG (CDR L))))) 
(PUT 'DEGREE-ORDER 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREE-ORDER 'DEFINED-ON-LINE '777) 
(PUT 'DEGREE-ORDER 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'DEGREE-ORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEGREE-ORDER (A B)
    (COND ((OR (ATOM A) (ATOM (CAR A))) T) ((OR (ATOM B) (ATOM (CAR B))) NIL)
          (T (LESSP (CDAAR A) (CDAAR B))))) 
(PUT 'MAKE-IMAGE-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-IMAGE-MOD-P 'DEFINED-ON-LINE '783) 
(PUT 'MAKE-IMAGE-MOD-P 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAKE-IMAGE-MOD-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-IMAGE-MOD-P (P V)
    (PROG (LP)
      (SETQ LP (DEGREE-IN-VARIABLE P V))
      (SETQ P (MAKE-UNIVARIATE-IMAGE-MOD-P P V))
      (COND ((NOT (EQUAL (DEGREE-IN-VARIABLE P V) LP)) (SETQ UNLUCKY-CASE T)))
      (RETURN P))) 
(PUT 'MAKE-UNIVARIATE-IMAGE-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-UNIVARIATE-IMAGE-MOD-P 'DEFINED-ON-LINE '813) 
(PUT 'MAKE-UNIVARIATE-IMAGE-MOD-P 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAKE-UNIVARIATE-IMAGE-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-UNIVARIATE-IMAGE-MOD-P (P V)
    (PROG (W R)
      (COND
       ((OR (ATOM P) (ATOM (CAR P)))
        (PROGN
         (COND ((EQUAL P NIL) (RETURN NIL))
               (T
                (RETURN
                 ((LAMBDA (U) (COND ((ZEROP U) NIL) (T U)))
                  (MODULAR-NUMBER P))))))))
      (COND
       ((EQUAL (CAAAR P) V)
        (PROGN
         (SETQ R NIL)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND (NOT (OR (ATOM P) (ATOM (CAR P)))) (EQUAL (CAAAR P) V)))
             (RETURN NIL)))
           (PROGN
            (SETQ W (MAKE-UNIVARIATE-IMAGE-MOD-P (CDAR P) V))
            (COND (W (SETQ R (CONS (CONS (CAAR P) W) R))))
            (SETQ P (CDR P)))
           (GO WHILELABEL))
         (SETQ P (MAKE-UNIVARIATE-IMAGE-MOD-P P V))
         NIL
         (PROG ()
          WHILELABEL
           (COND ((NOT R) (RETURN NIL)))
           (PROGN (SETQ W (CDR R)) (RPLACD R P) (SETQ P R) (SETQ R W))
           (GO WHILELABEL))
         (RETURN P)))
       (T
        (PROGN
         (SETQ R NIL)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (NOT (OR (ATOM P) (ATOM (CAR P))))
                   (NOT (EQUAL (CAAAR P) V))))
             (RETURN NIL)))
           (PROGN
            (SETQ R
                    (PLUS-MOD-P R
                     (TIMES-MOD-P (IMAGE-OF-POWER (CAAAR P) (CDAAR P))
                      (MAKE-UNIVARIATE-IMAGE-MOD-P (CDAR P) V))))
            (SETQ P (CDR P)))
           (GO WHILELABEL))
         (SETQ P (MAKE-UNIVARIATE-IMAGE-MOD-P P V))
         (RETURN (PLUS-MOD-P R P))))))) 
(PUT 'IMAGE-OF-POWER 'NUMBER-OF-ARGS 2) 
(PUT 'IMAGE-OF-POWER 'DEFINED-ON-LINE '844) 
(PUT 'IMAGE-OF-POWER 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'IMAGE-OF-POWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IMAGE-OF-POWER (V N)
    (PROG (W)
      (SETQ W (ASSOC V IMAGE-SET))
      (COND
       ((NULL W)
        (PROGN
         (SETQ W (MODULAR-NUMBER (NEXT-RANDOM-NUMBER)))
         (SETQ IMAGE-SET (CONS (CONS V W) IMAGE-SET))))
       (T (SETQ W (CDR W))))
      (RETURN (MODULAR-EXPT W N)))) 
(PUT 'MAKE-NUMERIC-IMAGE-MOD-P 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-NUMERIC-IMAGE-MOD-P 'DEFINED-ON-LINE '855) 
(PUT 'MAKE-NUMERIC-IMAGE-MOD-P 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'MAKE-NUMERIC-IMAGE-MOD-P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-NUMERIC-IMAGE-MOD-P (P)
    (COND
     ((OR (ATOM P) (ATOM (CAR P)))
      (COND ((EQUAL P NIL) 0) (T (MODULAR-NUMBER P))))
     (T
      (PROG (RESULT)
        (SETQ RESULT
                (IPLUS2
                 (REMAINDER
                  (TIMES (IMAGE-OF-POWER (CAAAR P) (CDAAR P))
                         (MAKE-NUMERIC-IMAGE-MOD-P (CDAR P)))
                  CURRENT-MODULUS)
                 (MAKE-NUMERIC-IMAGE-MOD-P (CDR P))))
        (COND
         ((NOT (ILESSP RESULT CURRENT-MODULUS))
          (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
        (RETURN RESULT))))) 
(PUT 'TOTAL-DEGREE-IN-POWERS 'NUMBER-OF-ARGS 2) 
(PUT 'TOTAL-DEGREE-IN-POWERS 'DEFINED-ON-LINE '866) 
(PUT 'TOTAL-DEGREE-IN-POWERS 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'TOTAL-DEGREE-IN-POWERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOTAL-DEGREE-IN-POWERS (FORM POWLST)
    (COND ((OR (NULL FORM) (OR (ATOM FORM) (ATOM (CAR FORM)))) POWLST)
          (T
           (PROG (X)
             (COND
              ((SETQ X (ATSOC (CAAAR FORM) POWLST))
               (AND (GREATERP (CDAAR FORM) (CDR X)) (RPLACD X (CDAAR FORM))))
              (T (SETQ POWLST (CONS (CONS (CAAAR FORM) (CDAAR FORM)) POWLST))))
             (RETURN
              (TOTAL-DEGREE-IN-POWERS (CDR FORM)
               (TOTAL-DEGREE-IN-POWERS (CDAR FORM) POWLST))))))) 
(PUT 'POWERS1 'NUMBER-OF-ARGS 1) 
(PUT 'POWERS1 'DEFINED-ON-LINE '880) 
(PUT 'POWERS1 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'POWERS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POWERS1 (FORM) (POWERS2 FORM (POWERS3 FORM NIL) NIL)) 
(PUT 'POWERS3 'NUMBER-OF-ARGS 2) 
(PUT 'POWERS3 'DEFINED-ON-LINE '885) 
(PUT 'POWERS3 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'POWERS3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POWERS3 (FORM L)
    (COND ((OR (ATOM FORM) (ATOM (CAR FORM))) L)
          (T
           (POWERS3 (CDAR FORM)
            (CONS (CONS (CAAAR FORM) (CONS (CDAAR FORM) (CDAAR FORM))) L))))) 
(PUT 'POWERS2 'NUMBER-OF-ARGS 3) 
(PUT 'POWERS2 'DEFINED-ON-LINE '891) 
(PUT 'POWERS2 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'POWERS2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE POWERS2 (FORM POWLST THISMONOMIAL)
    (COND
     ((OR (ATOM FORM) (ATOM (CAR FORM)))
      (COND ((NULL FORM) POWLST) (T (POWERS4 THISMONOMIAL POWLST))))
     (T
      (POWERS2 (CDAR FORM) (POWERS2 (CDR FORM) POWLST THISMONOMIAL)
       (CONS (CAAR FORM) THISMONOMIAL))))) 
(PUT 'POWERS4 'NUMBER-OF-ARGS 2) 
(PUT 'POWERS4 'DEFINED-ON-LINE '898) 
(PUT 'POWERS4 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'POWERS4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POWERS4 (NEW OLD)
    (COND
     ((NULL NEW)
      (PROG (V FORALL-RESULT FORALL-ENDPTR)
        (SETQ V OLD)
        (COND ((NULL V) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (CONS (CAR V) (CONS (CADR V) 0)))
                          (CAR V))
                         NIL)))
       LOOPLABEL
        (SETQ V (CDR V))
        (COND ((NULL V) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (V) (CONS (CAR V) (CONS (CADR V) 0))) (CAR V))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     ((NULL OLD)
      (PROG (V FORALL-RESULT FORALL-ENDPTR)
        (SETQ V NEW)
        (COND ((NULL V) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V) (CONS (CAR V) (CONS (CDR V) 0))) (CAR V))
                         NIL)))
       LOOPLABEL
        (SETQ V (CDR V))
        (COND ((NULL V) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (V) (CONS (CAR V) (CONS (CDR V) 0))) (CAR V))
                      NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     ((EQUAL (CAAR NEW) (CAAR OLD))
      (PROGN
       (COND
        ((GREATERP (CDAR NEW) (CADAR OLD)) (RPLACA (CDAR OLD) (CDAR NEW))))
       (COND ((LESSP (CDAR NEW) (CDDAR OLD)) (RPLACD (CDAR OLD) (CDAR NEW))))
       (RPLACD OLD (POWERS4 (CDR NEW) (CDR OLD)))))
     ((ORDOP (CAAR NEW) (CAAR OLD))
      (PROGN (RPLACD (CDAR OLD) 0) (RPLACD OLD (POWERS4 NEW (CDR OLD)))))
     (T (CONS (CONS (CAAR NEW) (CONS (CDAR NEW) 0)) (POWERS4 (CDR NEW) OLD))))) 
(PUT 'EZGCD-PP 'NUMBER-OF-ARGS 1) 
(PUT 'EZGCD-PP 'DEFINED-ON-LINE '914) 
(PUT 'EZGCD-PP 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'EZGCD-PP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EZGCD-PP (U) (QUOTF1 U (COMFAC-TO-POLY (EZGCD-COMFAC U)))) 
(PUT 'EZGCD-SQFRF 'NUMBER-OF-ARGS 1) 
(PUT 'EZGCD-SQFRF 'DEFINED-ON-LINE '918) 
(PUT 'EZGCD-SQFRF 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'EZGCD-SQFRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EZGCD-SQFRF (P)
    (PROG (PDASH P1 D V)
      (SETQ PDASH (DIFF P (SETQ V (CAAAR P))))
      (SETQ D (POLY-GCD P PDASH))
      (COND ((OR (ATOM D) (ATOM (CAR D))) (RETURN (LIST P))))
      (SETQ P (QUOTFAIL1 P D "GCD division in factor-sqfrf failed"))
      (SETQ P1
              (POLY-GCD P
               (ADDF (QUOTFAIL1 PDASH D "GCD division in factor-sqfrf failed")
                     (NEGF (DIFF P V)))))
      (RETURN (CONS P1 (EZGCD-SQFRF D))))) 
(PUT 'REDUCED-DEGREE 'NUMBER-OF-ARGS 2) 
(PUT 'REDUCED-DEGREE 'DEFINED-ON-LINE '933) 
(PUT 'REDUCED-DEGREE 'DEFINED-IN-FILE 'FACTOR/EZGCDF.RED) 
(PUT 'REDUCED-DEGREE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDUCED-DEGREE (U V)
    (PROG (VAR W X)
      (COND ((OR (EQUAL U V) (QUOTF1 U V)) (RETURN (CONS NIL NIL)))
            ((EQUAL (CDAAR V) 1) (RETURN (CONS 1 1))))
      (SETQ VAR (CAAAR U))
      (COND ((EQUAL (CDAAR U) (CDAAR V)) (SETQ X (NEGF (CDAR U))))
            (T
             (SETQ X
                     (CONS
                      (CONS
                       (GETPOWER (FKERN VAR) (DIFFERENCE (CDAAR U) (CDAAR V)))
                       (NEGF (CDAR U)))
                      NIL))))
      (SETQ W
              (ADDF
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR V) U))
                     (T (POLY-MULTF (CDAR V) U)))
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF X V))
                     (T (POLY-MULTF X V)))))
      (COND ((EQUAL (DEGR W VAR) 0) (RETURN (CONS 1 1))))
      (SETQ REDUCED-DEGREE-LCLST (ADDLC V REDUCED-DEGREE-LCLST))
      (COND ((SETQ X (QUOTF1 W (CDAR W))) (SETQ W X))
            (T
             (PROG (Y)
               (SETQ Y REDUCED-DEGREE-LCLST)
              LAB
               (COND ((NULL Y) (RETURN NIL)))
               ((LAMBDA (Y)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (SETQ X (QUOTF1 W Y))) (RETURN NIL)))
                    (SETQ W X)
                    (GO WHILELABEL)))
                (CAR Y))
               (SETQ Y (CDR Y))
               (GO LAB))))
      (SETQ U V)
      (SETQ V (EZGCD-PP W))
      (COND ((EQUAL (DEGR V VAR) 0) (RETURN (CONS 1 1)))
            (T (RETURN (CONS V (MAKE-UNIVARIATE-IMAGE-MOD-P V VAR))))))) 
(ENDMODULE) 