(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'IMAGESET)) 
(FLUID
 '(*FORCE-PRIME *FORCE-ZERO-SET *TRFAC BAD-CASE CHOSEN-PRIME CURRENT-MODULUS
   F-NUMVEC FACTOR-LEVEL FACTOR-TRACE-LIST FACTOR-X FACTORED-LC
   FORBIDDEN-PRIMES SMALLEST-PRIME FORBIDDEN-SETS IMAGE-CONTENT IMAGE-LC
   IMAGE-MOD-P IMAGE-POLY IMAGE-SET IMAGE-SET-MODULUS KORD* M-IMAGE-VARIABLE
   MODULUS/2 MULTIVARIATE-INPUT-POLY NO-OF-PRIMES-TO-TRY OTHERVARS POLYZERO
   SAVE-ZSET USABLE-SET-FOUND VARS-TO-KILL ZERO-SET-TRIED ZEROVARSET ZSET)) 
(PUT 'GENERATE-AN-IMAGE-SET-WITH-PRIME 'NUMBER-OF-ARGS 1) 
(PUT 'GENERATE-AN-IMAGE-SET-WITH-PRIME 'DEFINED-ON-LINE '82) 
(PUT 'GENERATE-AN-IMAGE-SET-WITH-PRIME 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'GENERATE-AN-IMAGE-SET-WITH-PRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERATE-AN-IMAGE-SET-WITH-PRIME (GOOD-SET-NEEDED)
    (PROG (CURRENTLY-FORBIDDEN-SETS U)
      (SETQ U MULTIVARIATE-INPUT-POLY)
      (SETQ IMAGE-SET NIL)
      (SETQ CURRENTLY-FORBIDDEN-SETS FORBIDDEN-SETS)
     TRYANOTHERSET
      (COND
       (IMAGE-SET
        (SETQ CURRENTLY-FORBIDDEN-SETS
                (CONS IMAGE-SET CURRENTLY-FORBIDDEN-SETS))))
      (SETQ IMAGE-SET (GET-NEW-SET CURRENTLY-FORBIDDEN-SETS))
      (SETQ IMAGE-LC (MAKE-IMAGE-LC-LIST (CDAR U) IMAGE-SET))
      (COND ((EQUAL (CAAR IMAGE-LC) 0) (GO TRYANOTHERSET)))
      (SETQ IMAGE-POLY (MAKE-IMAGE U IMAGE-SET))
      (SETQ IMAGE-CONTENT (GET.CONTENT IMAGE-POLY))
      (SETQ IMAGE-POLY (QUOTFAIL IMAGE-POLY IMAGE-CONTENT))
      (COND
       ((NOT
         (NULL
          ((LAMBDA (*EXP)
             (QUOTF1 IMAGE-POLY (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1))))
           T)))
        (GO TRYANOTHERSET)))
      (SETQ IMAGE-MOD-P
              (FIND-A-VALID-PRIME IMAGE-LC IMAGE-POLY
               (NOT (NUMBERP IMAGE-CONTENT))))
      (COND ((EQUAL IMAGE-MOD-P 'NOT-SQUARE-FREE) (GO TRYANOTHERSET)))
      (COND
       (FACTORED-LC
        (COND
         ((SETQ F-NUMVEC (UNIQUE-F-NOS FACTORED-LC IMAGE-CONTENT IMAGE-SET))
          (SETQ USABLE-SET-FOUND T))
         (T
          (PROGN
           (COND
            ((AND (NOT USABLE-SET-FOUND) GOOD-SET-NEEDED)
             (GO TRYANOTHERSET)))))))))) 
(PUT 'GET-NEW-SET 'NUMBER-OF-ARGS 1) 
(PUT 'GET-NEW-SET 'DEFINED-ON-LINE '154) 
(PUT 'GET-NEW-SET 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'GET-NEW-SET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-NEW-SET (FORBIDDEN-S)
    (PROG (OLD.M ALIST N NEXTZSET W)
      (COND
       (ZERO-SET-TRIED
        (PROGN
         (COND
          (*FORCE-ZERO-SET
           (ERRORF "Zero set tried - possibly it was invalid")))
         (SETQ IMAGE-SET-MODULUS (IADD1 IMAGE-SET-MODULUS))
         (SETQ OLD.M (SET-MODULUS IMAGE-SET-MODULUS))
         (SETQ ALIST
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V VARS-TO-KILL)
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V)
                                       (PROGN
                                        (SETQ N
                                                (MODULAR-NUMBER
                                                 (NEXT-RANDOM-NUMBER)))
                                        (COND
                                         ((GREATERP N MODULUS/2)
                                          (SETQ N
                                                  (DIFFERENCE N
                                                              CURRENT-MODULUS))))
                                        (COND
                                         ((CDR V)
                                          (PROGN
                                           (PROG ()
                                            WHILELABEL
                                             (COND
                                              ((NOT
                                                (OR (EQUAL N 0) (EQUAL N 1)
                                                    (EQUAL N
                                                           (ISUB1
                                                            CURRENT-MODULUS))))
                                               (RETURN NIL)))
                                             (SETQ N
                                                     (MODULAR-NUMBER
                                                      (NEXT-RANDOM-NUMBER)))
                                             (GO WHILELABEL))
                                           (COND
                                            ((GREATERP N MODULUS/2)
                                             (SETQ N
                                                     (DIFFERENCE N
                                                                 CURRENT-MODULUS)))))))
                                        (CONS (CAR V) N)))
                                     (CAR V))
                                    NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (V)
                               (PROGN
                                (SETQ N (MODULAR-NUMBER (NEXT-RANDOM-NUMBER)))
                                (COND
                                 ((GREATERP N MODULUS/2)
                                  (SETQ N (DIFFERENCE N CURRENT-MODULUS))))
                                (COND
                                 ((CDR V)
                                  (PROGN
                                   (PROG ()
                                    WHILELABEL
                                     (COND
                                      ((NOT
                                        (OR (EQUAL N 0) (EQUAL N 1)
                                            (EQUAL N (ISUB1 CURRENT-MODULUS))))
                                       (RETURN NIL)))
                                     (SETQ N
                                             (MODULAR-NUMBER
                                              (NEXT-RANDOM-NUMBER)))
                                     (GO WHILELABEL))
                                   (COND
                                    ((GREATERP N MODULUS/2)
                                     (SETQ N
                                             (DIFFERENCE N
                                                         CURRENT-MODULUS)))))))
                                (CONS (CAR V) N)))
                             (CAR V))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T
        (PROGN
         (SETQ OLD.M (SET-MODULUS IMAGE-SET-MODULUS))
         (SETQ NEXTZSET (CAR ZSET))
         (SETQ ALIST
                 (PROG (ZV FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ZV ZEROVARSET)
                   (COND ((NULL ZV) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ZV)
                                       (PROGN
                                        (SETQ W (CONS ZV (CAR NEXTZSET)))
                                        (SETQ NEXTZSET (CDR NEXTZSET))
                                        W))
                                     (CAR ZV))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ZV (CDR ZV))
                   (COND ((NULL ZV) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ZV)
                               (PROGN
                                (SETQ W (CONS ZV (CAR NEXTZSET)))
                                (SETQ NEXTZSET (CDR NEXTZSET))
                                W))
                             (CAR ZV))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          (OTHERVARS
           (SETQ ALIST
                   (APPEND ALIST
                           (PROG (V FORALL-RESULT FORALL-ENDPTR)
                             (SETQ V OTHERVARS)
                             (COND ((NULL V) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (V)
                                                 (PROGN
                                                  (SETQ N
                                                          (MODULAR-NUMBER
                                                           (NEXT-RANDOM-NUMBER)))
                                                  (PROG ()
                                                   WHILELABEL
                                                    (COND
                                                     ((NOT
                                                       (OR (EQUAL N 0)
                                                           (EQUAL N 1)
                                                           (EQUAL N
                                                                  (ISUB1
                                                                   CURRENT-MODULUS))))
                                                      (RETURN NIL)))
                                                    (SETQ N
                                                            (MODULAR-NUMBER
                                                             (NEXT-RANDOM-NUMBER)))
                                                    (GO WHILELABEL))
                                                  (COND
                                                   ((GREATERP N MODULUS/2)
                                                    (SETQ N
                                                            (DIFFERENCE N
                                                                        CURRENT-MODULUS))))
                                                  (CONS V N)))
                                               (CAR V))
                                              NIL)))
                            LOOPLABEL
                             (SETQ V (CDR V))
                             (COND ((NULL V) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (V)
                                         (PROGN
                                          (SETQ N
                                                  (MODULAR-NUMBER
                                                   (NEXT-RANDOM-NUMBER)))
                                          (PROG ()
                                           WHILELABEL
                                            (COND
                                             ((NOT
                                               (OR (EQUAL N 0) (EQUAL N 1)
                                                   (EQUAL N
                                                          (ISUB1
                                                           CURRENT-MODULUS))))
                                              (RETURN NIL)))
                                            (SETQ N
                                                    (MODULAR-NUMBER
                                                     (NEXT-RANDOM-NUMBER)))
                                            (GO WHILELABEL))
                                          (COND
                                           ((GREATERP N MODULUS/2)
                                            (SETQ N
                                                    (DIFFERENCE N
                                                                CURRENT-MODULUS))))
                                          (CONS V N)))
                                       (CAR V))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))))
         (COND
          ((NULL (SETQ ZSET (CDR ZSET)))
           (COND ((NULL SAVE-ZSET) (SETQ ZERO-SET-TRIED T))
                 (T (SETQ ZSET (MAKE-NEXT-ZSET SAVE-ZSET))))))
         (SETQ ALIST
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (CDR KORD*))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (ATSOC V ALIST)) (CAR V))
                                         NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (ATSOC V ALIST)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         NIL)))
      (SET-MODULUS OLD.M)
      (RETURN
       (COND ((MEMBER ALIST FORBIDDEN-S) (GET-NEW-SET FORBIDDEN-S))
             (T ALIST))))) 
(PUT 'FIND-A-VALID-PRIME 'NUMBER-OF-ARGS 3) 
(PUT 'FIND-A-VALID-PRIME 'DEFINED-ON-LINE '209) 
(PUT 'FIND-A-VALID-PRIME 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'FIND-A-VALID-PRIME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FIND-A-VALID-PRIME (LC-U U FACTOR-X)
    (PROG (CURRENTLY-FORBIDDEN-PRIMES RES PRIME-COUNT V W)
      (COND
       (FACTOR-X
        (SETQ U
                ((LAMBDA (G613)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF U G613))
                         (T (POLY-MULTF U G613))))
                 (SETQ V (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1)))))))
      (SETQ CHOSEN-PRIME NIL)
      (SETQ CURRENTLY-FORBIDDEN-PRIMES FORBIDDEN-PRIMES)
      (SETQ PRIME-COUNT 1)
     TRYANOTHERPRIME
      (COND
       (CHOSEN-PRIME
        (SETQ CURRENTLY-FORBIDDEN-PRIMES
                (CONS CHOSEN-PRIME CURRENTLY-FORBIDDEN-PRIMES))))
      (SETQ CHOSEN-PRIME (GET-NEW-PRIME CURRENTLY-FORBIDDEN-PRIMES))
      (SET-MODULUS CHOSEN-PRIME)
      (COND
       ((NOT (ATOM LC-U))
        (PROGN
         (SETQ W LC-U)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND W
                   (OR
                    (AND (OR (ATOM (CAAR W)) (ATOM (CAR (CAAR W))))
                         (NOT (EQUAL (MODULAR-NUMBER (CAAR W)) 0)))
                    (NOT
                     (OR (OR (ATOM (CAAR W)) (ATOM (CAR (CAAR W))))
                         (EQUAL (MODULAR-NUMBER (LNC (CAAR W))) 0))))))
             (RETURN NIL)))
           (SETQ W (CDR W))
           (GO WHILELABEL))
         (COND (W (GO TRYANOTHERPRIME)))))
       ((EQUAL (MODULAR-NUMBER LC-U) 0) (GO TRYANOTHERPRIME)))
      (SETQ RES (MONIC-MOD-P (REDUCE-MOD-P U)))
      (COND
       ((NOT (SQUARE-FREE-MOD-P RES))
        (COND
         ((AND MULTIVARIATE-INPUT-POLY
               (GREATERP (SETQ PRIME-COUNT (PLUS PRIME-COUNT 1))
                         NO-OF-PRIMES-TO-TRY))
          (PROGN
           (SETQ NO-OF-PRIMES-TO-TRY (PLUS NO-OF-PRIMES-TO-TRY 1))
           (SETQ RES 'NOT-SQUARE-FREE)))
         (T (GO TRYANOTHERPRIME)))))
      (COND
       ((AND FACTOR-X (NOT (EQUAL RES 'NOT-SQUARE-FREE)))
        (SETQ RES (QUOTFAIL-MOD-P RES V))))
      (RETURN RES))) 
(PUT 'GET-NEW-PRIME 'NUMBER-OF-ARGS 1) 
(PUT 'GET-NEW-PRIME 'DEFINED-ON-LINE '246) 
(PUT 'GET-NEW-PRIME 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'GET-NEW-PRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-NEW-PRIME (FORBIDDEN-P)
    (COND (*FORCE-PRIME *FORCE-PRIME)
          (T
           (PROG (P PRIMES-DONE)
             (PROG (PP)
               (SETQ PP FORBIDDEN-P)
              LAB
               (COND ((NULL PP) (RETURN NIL)))
               ((LAMBDA (PP)
                  (COND
                   ((LESSP PP 32) (SETQ PRIMES-DONE (CONS PP PRIMES-DONE)))))
                (CAR PP))
               (SETQ PP (CDR PP))
               (GO LAB))
            TRYAGAIN
             (COND
              ((NULL (SETQ P (RANDOM-TEENY-PRIME PRIMES-DONE)))
               (PROGN (SETQ P (RANDOM-SMALL-PRIME)) (SETQ PRIMES-DONE 'ALL)))
              (T (SETQ PRIMES-DONE (CONS P PRIMES-DONE))))
             (COND
              ((OR (MEMBER P FORBIDDEN-P) (LEQ P SMALLEST-PRIME))
               (GO TRYAGAIN)))
             (RETURN P))))) 
(PUT 'UNIQUE-F-NOS 'NUMBER-OF-ARGS 3) 
(PUT 'UNIQUE-F-NOS 'DEFINED-ON-LINE '269) 
(PUT 'UNIQUE-F-NOS 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'UNIQUE-F-NOS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE UNIQUE-F-NOS (V CONT.U0 IM.SET)
    (PROG (D K Q R LC.IMAGE.VEC)
      (SETQ K (LENGTH (CDR V)))
      (COND ((NOT (NUMBERP CONT.U0)) (SETQ CONT.U0 (CDAR CONT.U0))))
      (PUTV (SETQ D (MKVECT K)) 0 (ABS (TIMES CONT.U0 (CAR V))))
      (PUTV (SETQ LC.IMAGE.VEC (MKVECT K)) 0 (ABS (TIMES CONT.U0 (CAR V))))
      (SETQ V (CDR V))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
        (PROGN
         (SETQ Q (ABS (MAKE-IMAGE (CAAR V) IM.SET)))
         (PUTV LC.IMAGE.VEC I Q)
         (SETQ V (CDR V))
         (PROG (J)
           (SETQ J (ISUB1 I))
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 J))) (RETURN NIL)))
           (PROGN
            (SETQ R (GETV D J))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (ONEP R))) (RETURN NIL)))
              (PROGN (SETQ R (GCDN R Q)) (SETQ Q (QUOTIENT Q R)))
              (GO WHILELABEL))
            (COND
             ((ONEP Q) (PROGN (SETQ LC.IMAGE.VEC NIL) (SETQ J (MINUS 1))))))
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         (COND ((NULL LC.IMAGE.VEC) (SETQ I (PLUS K 1))) (T (PUTV D I Q)))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN LC.IMAGE.VEC))) 
(PUT 'GET.CONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'GET.CONTENT 'DEFINED-ON-LINE '307) 
(PUT 'GET.CONTENT 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'GET.CONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET.CONTENT (U)
    (PROG (C)
      (SETQ C
              (COND ((POLY-MINUSP U) (MINUS (NUMERIC-CONTENT U)))
                    (T (NUMERIC-CONTENT U))))
      (COND
       ((NOT
         (NULL
          ((LAMBDA (*EXP) (QUOTF1 U (LIST (CONS (CONS M-IMAGE-VARIABLE 1) 1))))
           T)))
        (SETQ C
                ((LAMBDA (G616)
                   (COND ((NULL C) G616)
                         (T
                          (CONS (CONS (GETPOWER (FKERN M-IMAGE-VARIABLE) 1) C)
                                G616))))
                 POLYZERO))))
      (RETURN C))) 
(PUT 'DISTRIBUTE.LC 'NUMBER-OF-ARGS 4) 
(PUT 'DISTRIBUTE.LC 'DEFINED-ON-LINE '330) 
(PUT 'DISTRIBUTE.LC 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'DISTRIBUTE.LC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DISTRIBUTE.LC (R IM.FACTORS S V)
    ((LAMBDA (FACTOR-LEVEL)
       (PROG (K DELTA DIV.COUNT Q UF I D MAX.MULT F NUMVEC DVEC WVEC DTWID W)
         (SETQ DELTA (CADDR (CDDR S)))
         (DIST.LC.MSG1 DELTA IM.FACTORS R S V)
         (SETQ V (CDR V))
         (SETQ K (LENGTH V))
         (SETQ NUMVEC (CADR (CDDR (CDDDR S))))
         (SETQ DVEC (MKVECT R))
         (SETQ WVEC (MKVECT R))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
           (PROGN
            (PUTV DVEC J 1)
            (PUTV WVEC J (TIMES DELTA (CDAR (GETV IM.FACTORS J)))))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (SETQ V (REVERSE V))
         (PROG (J)
           (SETQ J K)
          LAB
           (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 J))) (RETURN NIL)))
           (PROGN
            (SETQ F (CAAR V))
            (SETQ MAX.MULT (CDAR V))
            (SETQ V (CDR V))
            (SETQ D (GETV NUMVEC J))
            (SETQ I 1)
            (SETQ DIV.COUNT 0)
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
                  (PRIN2* "f(")
                  (PRIN2* J)
                  (PRIN2* ")= ")
                  (PRINTSF F)
                  (PRIN2* "There are ")
                  (PRIN2* MAX.MULT)
                  (PROGN
                   (PRIN2* " of these in the leading coefficient.")
                   (TERPRI* NIL))
                  (PRIN2* "The absolute value of the image of f(")
                  (PRIN2* J)
                  (PRIN2* ")= ")
                  (PROGN (PRIN2* D) (TERPRI* NIL)))
                 (WRS STREAM)))))
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (AND (ILESSP DIV.COUNT MAX.MULT) (NOT (IGREATERP I R))))
                (RETURN NIL)))
              (PROGN
               (SETQ Q (DIVIDE (GETV WVEC I) D))
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
                     (PRIN2* "  Trial divide into ")
                     (PRIN2* (GETV WVEC I))
                     (PROGN (PRIN2* " :") (TERPRI* NIL)))
                    (WRS STREAM)))))
               (PROG ()
                WHILELABEL
                 (COND
                  ((NOT (AND (ZEROP (CDR Q)) (ILESSP DIV.COUNT MAX.MULT)))
                   (RETURN NIL)))
                 (PROGN
                  (PUTV DVEC I
                        ((LAMBDA (G617)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G617 F))
                                 (T (POLY-MULTF G617 F))))
                         (GETV DVEC I)))
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
                        (PRIN2* "    It goes so an f(")
                        (PRIN2* J)
                        (PRIN2* ") belongs in ")
                        (PRINTSF (GETV IM.FACTORS I))
                        (PROGN (PRIN2* "  Try again...") (TERPRI* NIL)))
                       (WRS STREAM)))))
                  (SETQ DIV.COUNT (IADD1 DIV.COUNT))
                  (PUTV WVEC I (CAR Q))
                  (SETQ Q (DIVIDE (CAR Q) D))
                  NIL)
                 (GO WHILELABEL))
               (SETQ I (IADD1 I))
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
                      (PRIN2* "    no good so try another factor ...")
                      (TERPRI* NIL)))
                    (WRS STREAM))))))
              (GO WHILELABEL))
            (COND
             ((ILESSP DIV.COUNT MAX.MULT)
              (PROGN (SETQ BAD-CASE T) (SETQ DIV.COUNT MAX.MULT)))))
           (SETQ J (PLUS2 J (MINUS 1)))
           (GO LAB))
         (COND (BAD-CASE (RETURN NIL)))
         (DIST.LC.MSG2 DVEC IM.FACTORS R)
         (COND
          ((ONEP DELTA)
           (PROGN
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
              (PROGN
               (SETQ W
                       (QUOTIENT (CDAR (GETV IM.FACTORS J))
                                 (EVALUATE-IN-ORDER (GETV DVEC J) (CAR S))))
               (COND
                ((LESSP W 0)
                 (PROG (OLDPOLY)
                   (SETQ DELTA (MINUS DELTA))
                   (SETQ OLDPOLY (GETV IM.FACTORS J))
                   (PUTV IM.FACTORS J (NEGF OLDPOLY))
                   (MULTIPLY-ALPHAS (MINUS 1) OLDPOLY (GETV IM.FACTORS J)))))
               (PUTV DVEC J
                     ((LAMBDA (G619 G620)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G619 G620))
                              (T (POLY-MULTF G619 G620))))
                      (ABS W) (GETV DVEC J))))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (DIST.LC.MSG3 DVEC IM.FACTORS R)
            (RETURN (CONS DELTA DVEC)))))
         (DIST.LC.MSG4 DELTA)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
           (PROGN
            (SETQ DTWID (EVALUATE-IN-ORDER (GETV DVEC J) (CAR S)))
            (SETQ UF (GETV IM.FACTORS J))
            (SETQ D (GCDDD (CDAR UF) DTWID))
            (PUTV DVEC J
                  ((LAMBDA (G621 G622)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G621 G622))
                           (T (POLY-MULTF G621 G622))))
                   (QUOTIENT (CDAR UF) D) (GETV DVEC J)))
            (PUTV IM.FACTORS J
                  ((LAMBDA (G623)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G623 UF))
                           (T (POLY-MULTF G623 UF))))
                   (QUOTIENT DTWID D)))
            (MULTIPLY-ALPHAS-RECIP (QUOTIENT DTWID D) UF (GETV IM.FACTORS J))
            (SETQ DELTA (QUOTIENT DELTA (QUOTIENT DTWID D))))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (COND
          ((LEQ DELTA 0)
           (PROGN
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
                  (PRIN2* "final delta is -ve in distribute!.lc")
                  (PROGN (PRIN2* DELTA) (TERPRI* NIL)))
                 (WRS STREAM)))))
            (SETQ DELTA 1))))
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
               (PROGN (PRIN2* "     Finally we have:") (TERPRI* NIL))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
                 (PROGN
                  (PRINSF (GETV IM.FACTORS J))
                  (PRIN2* " with l.c. ")
                  (PRINTSF (GETV DVEC J)))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB)))
              (WRS STREAM)))))
         (RETURN (CONS DELTA DVEC))))
     (TIMES FACTOR-LEVEL 10))) 
(PUT 'DIST.LC.MSG1 'NUMBER-OF-ARGS 5) 
(PUT 'DIST.LC.MSG1 'DEFINED-ON-LINE '462) 
(PUT 'DIST.LC.MSG1 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'DIST.LC.MSG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIST.LC.MSG1 (DELTA IM.FACTORS R S V)
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
          (TERPRI)
          (TERPRI)
          (PROGN
           (PRIN2* "We have a polynomial whose image factors (call")
           (TERPRI* NIL))
          (PROGN (PRIN2* "them the IM-factors) are:") (TERPRI* NIL))
          (PRIN2* DELTA)
          (PROGN (PRIN2* " (= numeric content, delta)") (TERPRI* NIL))
          (EZGCD_PRINTVEC " f(" R ")= " IM.FACTORS)
          (PRIN2* "  wrt the image set: ")
          (PROG (X)
            (SETQ X (CAR S))
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (PROGN
                (PRIN2* (CAR X))
                (PRIN2* "=")
                (PRIN2* (CDR X))
                (PRIN2* ";")))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))
          (TERPRI* NIL)
          (PROGN
           (PRIN2* "We also have its true multivariate leading")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "coefficient whose factors (call these the")
           (TERPRI* NIL))
          (PROGN (PRIN2* "LC-factors) are:") (TERPRI* NIL))
          (FAC-PRINTFACTORS V)
          (PROGN
           (PRIN2* "We want to determine how these LC-factors are")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "distributed over the leading coefficients of each")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "IM-factor.  This enables us to feed the resulting")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "image factors into a multivariate Hensel")
           (TERPRI* NIL))
          (PROGN (PRIN2* "construction.") (TERPRI* NIL))
          (PROGN
           (PRIN2* "We distribute each LC-factor in turn by dividing")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "its image into delta times the leading coefficient")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "of each IM-factor until it finds one that it")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "divides exactly. The image set is chosen such that")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "this will only happen for the IM-factors to which")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "this LC-factor belongs - (there may be more than")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "one if the LC-factor occurs several times in the")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "leading coefficient of the original polynomial).")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "This choice also requires that we distribute the")
           (TERPRI* NIL))
          (PROGN (PRIN2* "LC-factors in a specific order:") (TERPRI* NIL)))
         (WRS STREAM)))))) 
(PUT 'DIST.LC.MSG2 'NUMBER-OF-ARGS 3) 
(PUT 'DIST.LC.MSG2 'DEFINED-ON-LINE '494) 
(PUT 'DIST.LC.MSG2 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'DIST.LC.MSG2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIST.LC.MSG2 (DVEC IM.FACTORS R)
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
           (PRIN2* "The leading coefficients are now correct to within an")
           (TERPRI* NIL))
          (PROGN (PRIN2* "integer factor and are as follows:") (TERPRI* NIL))
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
            (PROGN
             (PRINSF (GETV IM.FACTORS J))
             (PRIN2* " with l.c. ")
             (PRINTSF (GETV DVEC J)))
            (SETQ J (PLUS2 J 1))
            (GO LAB)))
         (WRS STREAM)))))) 
(PUT 'DIST.LC.MSG3 'NUMBER-OF-ARGS 3) 
(PUT 'DIST.LC.MSG3 'DEFINED-ON-LINE '503) 
(PUT 'DIST.LC.MSG3 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'DIST.LC.MSG3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DIST.LC.MSG3 (DVEC IM.FACTORS R)
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
           (PRIN2* "Since delta=1, we have no non-trivial content of the")
           (TERPRI* NIL))
          (PROGN
           (PRIN2*
            "image to deal with so we know the true leading coefficients")
           (TERPRI* NIL))
          (PROGN
           (PRIN2*
            "exactly.  We fix the signs of the IM-factors to match those")
           (TERPRI* NIL))
          (PROGN (PRIN2* "of their true leading coefficients:") (TERPRI* NIL))
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
            (PROGN
             (PRINSF (GETV IM.FACTORS J))
             (PRIN2* " with l.c. ")
             (PRINTSF (GETV DVEC J)))
            (SETQ J (PLUS2 J 1))
            (GO LAB)))
         (WRS STREAM)))))) 
(PUT 'DIST.LC.MSG4 'NUMBER-OF-ARGS 1) 
(PUT 'DIST.LC.MSG4 'DEFINED-ON-LINE '516) 
(PUT 'DIST.LC.MSG4 'DEFINED-IN-FILE 'FACTOR/IMAGESET.RED) 
(PUT 'DIST.LC.MSG4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIST.LC.MSG4 (DELTA)
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
          (PRIN2* " Here delta is not 1 meaning that we have a content, ")
          (PROGN (PRIN2* DELTA) (TERPRI* NIL))
          (PROGN
           (PRIN2* "of the image to distribute among the factors somehow.")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "For each IM-factor we can divide its leading")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "coefficient by the image of its determined leading")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "coefficient and see if there is a non-trivial result.")
           (TERPRI* NIL))
          (PROGN
           (PRIN2* "This will indicate a factor of delta belonging to this")
           (TERPRI* NIL))
          (PROGN (PRIN2* "IM-factor's leading coefficient.") (TERPRI* NIL)))
         (WRS STREAM)))))) 
(ENDMODULE) 