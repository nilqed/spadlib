(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FACMISC)) 
(FLUID
 '(CURRENT-MODULUS IMAGE-SET-MODULUS MODULUS/2 OTHERVARS POLYZERO SAVE-ZSET
   ZEROVARSET)) 
(GLOBAL '(LARGEST-SMALL-MODULUS PSEUDO-PRIMES TEENY-PRIMES)) 
(PUT 'MULTIVARIATEP 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIVARIATEP 'DEFINED-ON-LINE '44) 
(PUT 'MULTIVARIATEP 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MULTIVARIATEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIVARIATEP (A V)
    (COND ((OR (ATOM A) (ATOM (CAR A))) NIL) ((NOT (EQ (CAAAR A) V)) T)
          ((MULTIVARIATEP (CDAR A) V) T) (T (MULTIVARIATEP (CDR A) V)))) 
(PUT 'VARIABLES-IN-FORM 'NUMBER-OF-ARGS 1) 
(PUT 'VARIABLES-IN-FORM 'DEFINED-ON-LINE '50) 
(PUT 'VARIABLES-IN-FORM 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'VARIABLES-IN-FORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARIABLES-IN-FORM (A) (VARIABLES.IN.FORM A NIL)) 
(PUT 'GET.COEFFT.BOUND 'NUMBER-OF-ARGS 2) 
(PUT 'GET.COEFFT.BOUND 'DEFINED-ON-LINE '54) 
(PUT 'GET.COEFFT.BOUND 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'GET.COEFFT.BOUND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET.COEFFT.BOUND (POLY DEGBD)
    (MAX (TIMES (GET-HEIGHT POLY) (FIXEXPFLOAT (SUMOF DEGBD))) 110)) 
(PUT 'SUMOF 'NUMBER-OF-ARGS 1) 
(PUT 'SUMOF 'DEFINED-ON-LINE '62) 
(PUT 'SUMOF 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'SUMOF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SUMOF (DEGBD)
    (COND ((NULL DEGBD) 0) (T (PLUS (CDAR DEGBD) (SUMOF (CDR DEGBD)))))) 
(PUT 'FIXEXPFLOAT 'NUMBER-OF-ARGS 1) 
(PUT 'FIXEXPFLOAT 'DEFINED-ON-LINE '65) 
(PUT 'FIXEXPFLOAT 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'FIXEXPFLOAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXEXPFLOAT (N)
    (COND ((GREATERP N 10) (TIMES 22027 (FIXEXPFLOAT (DIFFERENCE N 10))))
          (T (CEILING (EXP (FLOAT N)))))) 
(PUT 'QUOTFAIL 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTFAIL 'DEFINED-ON-LINE '78) 
(PUT 'QUOTFAIL 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'QUOTFAIL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTFAIL (A B)
    (COND ((NULL A) POLYZERO)
          (T
           (PROG (W)
             (SETQ W ((LAMBDA (*EXP) (QUOTF1 A B)) T))
             (COND ((NULL W) (ERRORF (LIST "Unexpected division failure" A B)))
                   (T (RETURN W))))))) 
(PUT 'QUOTFAIL1 'NUMBER-OF-ARGS 3) 
(PUT 'QUOTFAIL1 'DEFINED-ON-LINE '87) 
(PUT 'QUOTFAIL1 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'QUOTFAIL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUOTFAIL1 (A B MSG)
    (COND ((NULL A) POLYZERO)
          (T
           (PROG (W)
             (SETQ W ((LAMBDA (*EXP) (QUOTF1 A B)) T))
             (COND ((NULL W) (ERRORF MSG)) (T (RETURN W))))))) 
(PUT 'SET-TEENY-PRIMES 'NUMBER-OF-ARGS 0) 
(PUT 'SET-TEENY-PRIMES 'DEFINED-ON-LINE '100) 
(PUT 'SET-TEENY-PRIMES 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'SET-TEENY-PRIMES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SET-TEENY-PRIMES NIL
    (PROG (I)
      (SETQ I (MINUS 1))
      (SETQ TEENY-PRIMES (MKVECT 9))
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 3)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 5)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 7)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 11)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 13)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 17)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 19)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 23)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 29)
      (PUTV TEENY-PRIMES (SETQ I (IADD1 I)) 31))) 
(SET-TEENY-PRIMES) 
(PUT 'RANDOM-SMALL-PRIME 'NUMBER-OF-ARGS 0) 
(PUT 'RANDOM-SMALL-PRIME 'DEFINED-ON-LINE '118) 
(PUT 'RANDOM-SMALL-PRIME 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'RANDOM-SMALL-PRIME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RANDOM-SMALL-PRIME NIL
    (PROG (P)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ P (SMALL-RANDOM-NUMBER))
         (COND ((EVENP P) (SETQ P (IADD1 P)))))
        (COND ((NOT (PRIMEP P)) (GO REPEATLABEL))))
      (RETURN P))) 
(PUT 'SMALL-RANDOM-NUMBER 'NUMBER-OF-ARGS 0) 
(PUT 'SMALL-RANDOM-NUMBER 'DEFINED-ON-LINE '126) 
(PUT 'SMALL-RANDOM-NUMBER 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'SMALL-RANDOM-NUMBER 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SMALL-RANDOM-NUMBER NIL
    (PROG (W)
      (SETQ W (REMAINDER (NEXT-RANDOM-NUMBER) 1000))
      (SETQ W (PLUS (REMAINDER (NEXT-RANDOM-NUMBER) 1000) (TIMES 1000 W)))
      (COND ((LESSP W 0) (SETQ W (PLUS W 1000000))))
      (SETQ W (PLUS 1.0 (TIMES 1.5 (QUOTIENT (FLOAT W) 1000000.0))))
      (SETQ W (TIMES W W))
      (RETURN (FIX (EXP W))))) 
(PUT 'RANDOM-TEENY-PRIME 'NUMBER-OF-ARGS 1) 
(PUT 'RANDOM-TEENY-PRIME 'DEFINED-ON-LINE '150) 
(PUT 'RANDOM-TEENY-PRIME 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'RANDOM-TEENY-PRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANDOM-TEENY-PRIME (L)
    (COND ((OR (EQUAL L 'ALL) (EQUAL (LENGTH L) 10)) NIL)
          (T
           (PROG (P)
             (PROG ()
              REPEATLABEL
               (SETQ P (GETV TEENY-PRIMES (REMAINDER (NEXT-RANDOM-NUMBER) 10)))
               (COND ((NOT (NOT (MEMBER P L))) (GO REPEATLABEL))))
             (RETURN P))))) 
(PUT 'MILLER-RABIN 'NUMBER-OF-ARGS 2) 
(PUT 'MILLER-RABIN 'DEFINED-ON-LINE '185) 
(PUT 'MILLER-RABIN 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MILLER-RABIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MILLER-RABIN (A N)
    (PROG (D S X)
      (COND ((EQUAL N 1) (RETURN NIL))
            ((EQUAL (REMAINDER N 2) 0) (RETURN (EQUAL N 2)))
            ((LEQ N 7) (RETURN T)))
      (SETQ D (DIFFERENCE N 1))
      (SETQ S 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (REMAINDER D 2) 0)) (RETURN NIL)))
        (PROGN (SETQ D (QUOTIENT D 2)) (SETQ S (PLUS S 1)))
        (GO WHILELABEL))
      (SET-MODULUS N)
      (SETQ A (REMAINDER A N))
      (SETQ X (MODULAR-EXPT A D))
      (COND ((OR (EQUAL X 1) (EQUAL X (DIFFERENCE N 1))) (RETURN T)))
     LOOP
      (COND ((EQUAL S 1) (RETURN NIL)))
      (SETQ X (REMAINDER (TIMES X X) CURRENT-MODULUS))
      (COND ((EQUAL X 1) (RETURN NIL)) ((EQUAL X (DIFFERENCE N 1)) (RETURN T)))
      (SETQ S (DIFFERENCE S 1))
      (GO LOOP))) 
(PUT 'PRIMEP27 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMEP27 'DEFINED-ON-LINE '218) 
(PUT 'PRIMEP27 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'PRIMEP27 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMEP27 (N)
    (PROG (OLDMOD R)
      (COND ((EQUAL N 1493) (RETURN T)))
      (SETQ OLDMOD (SET-MODULUS NIL))
      (SETQ R
              (AND (MILLER-RABIN 8958 N) (MILLER-RABIN 4704 N)
                   (NOT (OR (EQUAL N 9131401) (EQUAL N 6089071)))))
      (SET-MODULUS OLDMOD)
      (RETURN R))) 
(PUT 'RANDOM-PRIME 'NUMBER-OF-ARGS 0) 
(PUT 'RANDOM-PRIME 'DEFINED-ON-LINE '243) 
(PUT 'RANDOM-PRIME 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'RANDOM-PRIME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RANDOM-PRIME NIL
    (PROG (N LSM LSM2)
      (SETQ LSM LARGEST-SMALL-MODULUS)
      (COND ((GREATERP LSM (EXPT 2 27)) (SETQ LSM (EXPT 2 27))))
      (SETQ LSM2 (QUOTIENT LSM 2))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ N (REMAINDER (NEXT-RANDOM-NUMBER) LSM))
         (COND ((LESSP N LSM2) (SETQ N (PLUS N LSM2))))
         (COND ((EVENP N) (SETQ N (PLUS N 1)))))
        (COND ((NOT (PRIMEP27 N)) (GO REPEATLABEL))))
      (RETURN N))) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-P 'NUMBER-OF-ARGS 3) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-P 'DEFINED-ON-LINE '267) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-P 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORM-SUM-AND-PRODUCT-MOD-P (AVEC FVEC R)
    (PROG (S)
      (SETQ S POLYZERO)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (SETQ S (PLUS-MOD-P (TIMES-MOD-P (GETV AVEC I) (GETV FVEC I)) S))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN S))) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-M 'NUMBER-OF-ARGS 3) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-M 'DEFINED-ON-LINE '277) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-M 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'FORM-SUM-AND-PRODUCT-MOD-M 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORM-SUM-AND-PRODUCT-MOD-M (AVEC FVEC R)
    (PROG (S)
      (SETQ S POLYZERO)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (SETQ S (PLUS-MOD-P (TIMES-MOD-P (GETV AVEC I) (GETV FVEC I)) S))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN S))) 
(PUT 'REDUCE-VEC-BY-ONE-VAR-MOD-P 'NUMBER-OF-ARGS 3) 
(PUT 'REDUCE-VEC-BY-ONE-VAR-MOD-P 'DEFINED-ON-LINE '288) 
(PUT 'REDUCE-VEC-BY-ONE-VAR-MOD-P 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'REDUCE-VEC-BY-ONE-VAR-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REDUCE-VEC-BY-ONE-VAR-MOD-P (V PT N)
    (PROG (NEWV)
      (SETQ NEWV (MKVECT N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV NEWV I (EVALUATE-MOD-P (GETV V I) (CAR PT) (CDR PT)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEWV))) 
(PUT 'MAKE-BIVARIATE-VEC-MOD-P 'NUMBER-OF-ARGS 4) 
(PUT 'MAKE-BIVARIATE-VEC-MOD-P 'DEFINED-ON-LINE '298) 
(PUT 'MAKE-BIVARIATE-VEC-MOD-P 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MAKE-BIVARIATE-VEC-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-BIVARIATE-VEC-MOD-P (V IMSET VAR N)
    (PROG (NEWV)
      (SETQ NEWV (MKVECT N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PUTV NEWV I (MAKE-BIVARIATE-MOD-P (GETV V I) IMSET VAR))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN NEWV))) 
(PUT 'TIMES-VECTOR-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'TIMES-VECTOR-MOD-P 'DEFINED-ON-LINE '306) 
(PUT 'TIMES-VECTOR-MOD-P 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'TIMES-VECTOR-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TIMES-VECTOR-MOD-P (V N)
    (PROG (W)
      (SETQ W 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ W (TIMES-MOD-P (GETV V I) W))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN W))) 
(PUT 'MAKE-VEC-MODULAR-SYMMETRIC 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-VEC-MODULAR-SYMMETRIC 'DEFINED-ON-LINE '314) 
(PUT 'MAKE-VEC-MODULAR-SYMMETRIC 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MAKE-VEC-MODULAR-SYMMETRIC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-VEC-MODULAR-SYMMETRIC (V N)
    (PROG (I)
      (SETQ I 1)
     LAB
      (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
      (PUTV V I (MAKE-MODULAR-SYMMETRIC (GETV V I)))
      (SETQ I (PLUS2 I 1))
      (GO LAB))) 
(PUT 'MAKE-ZEROVARSET 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-ZEROVARSET 'DEFINED-ON-LINE '322) 
(PUT 'MAKE-ZEROVARSET 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MAKE-ZEROVARSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-ZEROVARSET (VLIST)
    (PROG (W)
      (SETQ W VLIST)
     LAB
      (COND ((NULL W) (RETURN NIL)))
      ((LAMBDA (W)
         (COND ((CDR W) (SETQ OTHERVARS (CONS (CAR W) OTHERVARS)))
               (T (SETQ ZEROVARSET (CONS (CAR W) ZEROVARSET)))))
       (CAR W))
      (SETQ W (CDR W))
      (GO LAB))) 
(PUT 'MAKE-ZEROSET-LIST 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-ZEROSET-LIST 'DEFINED-ON-LINE '331) 
(PUT 'MAKE-ZEROSET-LIST 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MAKE-ZEROSET-LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-ZEROSET-LIST (N)
    (PROG (W)
      (PROG (K)
        (SETQ K 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
        (SETQ W (APPEND W (KCOMBNS K N)))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (RETURN W))) 
(PUT 'KCOMBNS 'NUMBER-OF-ARGS 2) 
(PUT 'KCOMBNS 'DEFINED-ON-LINE '339) 
(PUT 'KCOMBNS 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'KCOMBNS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KCOMBNS (K M)
    (COND
     ((OR (EQUAL K 0) (EQUAL K M))
      (PROG (W)
        (COND ((EQUAL K M) (SETQ K 1)))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE M I)) (RETURN NIL)))
          (SETQ W (CONS K W))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (RETURN (LIST W))))
     ((OR (EQUAL K 1) (EQUAL K (ISUB1 M)))
      (PROGN
       (COND ((EQUAL K (ISUB1 M)) (SETQ K 0)))
       (LIST-WITH-ONE-A K (IDIFFERENCE 1 K) M)))
     (T
      (APPEND
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (KCOMBNS (ISUB1 K) (ISUB1 M)))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CONS 1 X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CONS 1 X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (KCOMBNS K (ISUB1 M)))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CONS 0 X)) (CAR X)) NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CONS 0 X)) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'LIST-WITH-ONE-A 'NUMBER-OF-ARGS 3) 
(PUT 'LIST-WITH-ONE-A 'DEFINED-ON-LINE '354) 
(PUT 'LIST-WITH-ONE-A 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'LIST-WITH-ONE-A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIST-WITH-ONE-A (A B M)
    (PROG (W X R)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (ISUB1 M) I)) (RETURN NIL)))
        (SETQ W (CONS B W))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ R (LIST (CONS A W)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (ISUB1 M) I)) (RETURN NIL)))
        (PROGN
         (SETQ X (CONS (CAR W) X))
         (SETQ W (CDR W))
         (SETQ R (CONS (APPEND X (CONS A W)) R)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN R))) 
(PUT 'MAKE-NEXT-ZSET 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-NEXT-ZSET 'DEFINED-ON-LINE '365) 
(PUT 'MAKE-NEXT-ZSET 'DEFINED-IN-FILE 'FACTOR/FACMISC.RED) 
(PUT 'MAKE-NEXT-ZSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-NEXT-ZSET (L)
    (PROG (K W)
      (SETQ IMAGE-SET-MODULUS (IADD1 IMAGE-SET-MODULUS))
      (SET-MODULUS IMAGE-SET-MODULUS)
      (SETQ W
              (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                (SETQ LL (CDR L))
                (COND ((NULL LL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (LL)
                                    (PROG (N FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ N LL)
                                      (COND ((NULL N) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (N)
                                                          (COND ((EQUAL N 0) N)
                                                                (T
                                                                 (PROGN
                                                                  (SETQ K
                                                                          (MODULAR-NUMBER
                                                                           (NEXT-RANDOM-NUMBER)))
                                                                  (PROG ()
                                                                   WHILELABEL
                                                                    (COND
                                                                     ((NOT
                                                                       (OR
                                                                        (ZEROP
                                                                         K)
                                                                        (ONEP
                                                                         K)))
                                                                      (RETURN
                                                                       NIL)))
                                                                    (SETQ K
                                                                            (MODULAR-NUMBER
                                                                             (NEXT-RANDOM-NUMBER)))
                                                                    (GO
                                                                     WHILELABEL))
                                                                  (COND
                                                                   ((GREATERP K
                                                                              MODULUS/2)
                                                                    (SETQ K
                                                                            (DIFFERENCE
                                                                             K
                                                                             CURRENT-MODULUS))))
                                                                  K))))
                                                        (CAR N))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ N (CDR N))
                                      (COND ((NULL N) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (N)
                                                  (COND ((EQUAL N 0) N)
                                                        (T
                                                         (PROGN
                                                          (SETQ K
                                                                  (MODULAR-NUMBER
                                                                   (NEXT-RANDOM-NUMBER)))
                                                          (PROG ()
                                                           WHILELABEL
                                                            (COND
                                                             ((NOT
                                                               (OR (ZEROP K)
                                                                   (ONEP K)))
                                                              (RETURN NIL)))
                                                            (SETQ K
                                                                    (MODULAR-NUMBER
                                                                     (NEXT-RANDOM-NUMBER)))
                                                            (GO WHILELABEL))
                                                          (COND
                                                           ((GREATERP K
                                                                      MODULUS/2)
                                                            (SETQ K
                                                                    (DIFFERENCE
                                                                     K
                                                                     CURRENT-MODULUS))))
                                                          K))))
                                                (CAR N))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR LL))
                                 NIL)))
               LOOPLABEL
                (SETQ LL (CDR LL))
                (COND ((NULL LL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (LL)
                            (PROG (N FORALL-RESULT FORALL-ENDPTR)
                              (SETQ N LL)
                              (COND ((NULL N) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (N)
                                                  (COND ((EQUAL N 0) N)
                                                        (T
                                                         (PROGN
                                                          (SETQ K
                                                                  (MODULAR-NUMBER
                                                                   (NEXT-RANDOM-NUMBER)))
                                                          (PROG ()
                                                           WHILELABEL
                                                            (COND
                                                             ((NOT
                                                               (OR (ZEROP K)
                                                                   (ONEP K)))
                                                              (RETURN NIL)))
                                                            (SETQ K
                                                                    (MODULAR-NUMBER
                                                                     (NEXT-RANDOM-NUMBER)))
                                                            (GO WHILELABEL))
                                                          (COND
                                                           ((GREATERP K
                                                                      MODULUS/2)
                                                            (SETQ K
                                                                    (DIFFERENCE
                                                                     K
                                                                     CURRENT-MODULUS))))
                                                          K))))
                                                (CAR N))
                                               NIL)))
                             LOOPLABEL
                              (SETQ N (CDR N))
                              (COND ((NULL N) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (N)
                                          (COND ((EQUAL N 0) N)
                                                (T
                                                 (PROGN
                                                  (SETQ K
                                                          (MODULAR-NUMBER
                                                           (NEXT-RANDOM-NUMBER)))
                                                  (PROG ()
                                                   WHILELABEL
                                                    (COND
                                                     ((NOT
                                                       (OR (ZEROP K) (ONEP K)))
                                                      (RETURN NIL)))
                                                    (SETQ K
                                                            (MODULAR-NUMBER
                                                             (NEXT-RANDOM-NUMBER)))
                                                    (GO WHILELABEL))
                                                  (COND
                                                   ((GREATERP K MODULUS/2)
                                                    (SETQ K
                                                            (DIFFERENCE K
                                                                        CURRENT-MODULUS))))
                                                  K))))
                                        (CAR N))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR LL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SAVE-ZSET NIL)
      (RETURN W))) 
(ENDMODULE) 