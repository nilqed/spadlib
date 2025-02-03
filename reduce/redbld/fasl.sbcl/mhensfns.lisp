(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MHENSFNS)) 
(FLUID
 '(*TRFAC ALPHALIST CURRENT-MODULUS DEGREE-BOUNDS DELFVEC FACTOR-LEVEL
   FACTOR-TRACE-LIST FORBIDDEN-PRIMES SMALLEST-PRIME HENSEL-GROWTH-SIZE
   IMAGE-FACTORS MAX-UNKNOWNS MULTIVARIATE-INPUT-POLY NON-MONIC
   NUMBER-OF-FACTORS NUMBER-OF-UNKNOWNS POLYZERO PRIME-BASE)) 
(PUT 'SET-DEGREE-BOUNDS 'NUMBER-OF-ARGS 3) 
(PUT 'SET-DEGREE-BOUNDS 'DEFINED-ON-LINE '58) 
(PUT 'SET-DEGREE-BOUNDS 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'SET-DEGREE-BOUNDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SET-DEGREE-BOUNDS (V P1 P2)
    (SETQ DEGREE-BOUNDS
            (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
              (SETQ VAR V)
              (COND ((NULL VAR) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (VAR)
                                  (CONS (CAR VAR)
                                        (MAX (DEGREE-IN-VARIABLE P1 (CAR VAR))
                                             (DEGREE-IN-VARIABLE P2
                                              (CAR VAR)))))
                                (CAR VAR))
                               NIL)))
             LOOPLABEL
              (SETQ VAR (CDR VAR))
              (COND ((NULL VAR) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (VAR)
                          (CONS (CAR VAR)
                                (MAX (DEGREE-IN-VARIABLE P1 (CAR VAR))
                                     (DEGREE-IN-VARIABLE P2 (CAR VAR)))))
                        (CAR VAR))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))) 
(PUT 'GET-DEGREE-BOUND 'NUMBER-OF-ARGS 1) 
(PUT 'GET-DEGREE-BOUND 'DEFINED-ON-LINE '63) 
(PUT 'GET-DEGREE-BOUND 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'GET-DEGREE-BOUND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-DEGREE-BOUND (V)
    (PROG (W)
      (SETQ W (ATSOC V DEGREE-BOUNDS))
      (COND
       ((NULL W)
        (ERRORF (LIST "Degree bound not found for " V " in " DEGREE-BOUNDS))))
      (RETURN (CDR W)))) 
(PUT 'CHOOSE-LARGER-PRIME 'NUMBER-OF-ARGS 1) 
(PUT 'CHOOSE-LARGER-PRIME 'DEFINED-ON-LINE '71) 
(PUT 'CHOOSE-LARGER-PRIME 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'CHOOSE-LARGER-PRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOOSE-LARGER-PRIME (N)
    (COND
     ((GREATERP N (DIFFERENCE (EXPT 2 24) 1))
      (ERRORF (LIST "Cannot choose prime > given number:" N)))
     (T
      (PROG (P FLIST-MOD-P K FVEC-MOD-P FORBIDDEN-PRIMES SMALLEST-PRIME)
        (SETQ SMALLEST-PRIME N)
       TRYNEWPRIME
        (COND (P (SETQ FORBIDDEN-PRIMES (CONS P FORBIDDEN-PRIMES))))
        (SETQ P (RANDOM-PRIME))
        (SET-MODULUS P)
        (COND
         ((OR (NOT (GREATERP P N)) (MEMBER P FORBIDDEN-PRIMES)
              (NULL (REDUCE-MOD-P (CDAR MULTIVARIATE-INPUT-POLY))))
          (GO TRYNEWPRIME)))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
          (SETQ FLIST-MOD-P
                  (CONS (REDUCE-MOD-P (GETV IMAGE-FACTORS I)) FLIST-MOD-P))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ ALPHALIST (ALPHAS NUMBER-OF-FACTORS FLIST-MOD-P 1))
        (COND ((EQUAL ALPHALIST '|FACTORS NOT COPRIME|) (GO TRYNEWPRIME)))
        (SETQ HENSEL-GROWTH-SIZE P)
        (SETQ PRIME-BASE P)
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
              (PRIN2* "New prime chosen: ")
              (PROGN (PRIN2* HENSEL-GROWTH-SIZE) (TERPRI* NIL)))
             (WRS STREAM)))))
        (SETQ K NUMBER-OF-FACTORS)
        (SETQ FVEC-MOD-P (MKVECT K))
        (PROG (W)
          (SETQ W FLIST-MOD-P)
         LAB
          (COND ((NULL W) (RETURN NIL)))
          ((LAMBDA (W) (PROGN (PUTV FVEC-MOD-P K W) (SETQ K (ISUB1 K))))
           (CAR W))
          (SETQ W (CDR W))
          (GO LAB))
        (RETURN FVEC-MOD-P))))) 
(PUT 'BINOMIAL-COEFFT-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'BINOMIAL-COEFFT-MOD-P 'DEFINED-ON-LINE '107) 
(PUT 'BINOMIAL-COEFFT-MOD-P 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'BINOMIAL-COEFFT-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BINOMIAL-COEFFT-MOD-P (N R)
    (COND ((LESSP N R) NIL) ((EQUAL N R) 1)
          ((EQUAL R 1)
           ((LAMBDA (U) (COND ((ZEROP U) NIL) (T U))) (MODULAR-NUMBER N)))
          (T
           (PROG (N-C-R B J)
             (SETQ N-C-R 1)
             (SETQ B (MIN R (DIFFERENCE N R)))
             (SETQ N (MODULAR-NUMBER N))
             (SETQ R (MODULAR-NUMBER R))
             (PROG (I)
               (SETQ I 1)
              LAB
               (COND ((MINUSP (DIFFERENCE B I)) (RETURN NIL)))
               (PROGN
                (SETQ J (MODULAR-NUMBER I))
                (SETQ N-C-R
                        (REMAINDER
                         (TIMES
                          (REMAINDER
                           (TIMES N-C-R
                                  (PROG (RESULT)
                                    (SETQ RESULT
                                            (IDIFFERENCE N
                                                         (PROG (RESULT)
                                                           (SETQ RESULT
                                                                   (IDIFFERENCE
                                                                    J 1))
                                                           (COND
                                                            ((IMINUSP RESULT)
                                                             (SETQ RESULT
                                                                     (IPLUS2
                                                                      RESULT
                                                                      CURRENT-MODULUS))))
                                                           (RETURN RESULT))))
                                    (COND
                                     ((IMINUSP RESULT)
                                      (SETQ RESULT
                                              (IPLUS2 RESULT
                                                      CURRENT-MODULUS))))
                                    (RETURN RESULT)))
                           CURRENT-MODULUS)
                          (MODULAR-RECIPROCAL J))
                         CURRENT-MODULUS)))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN (COND ((ZEROP N-C-R) NIL) (T N-C-R))))))) 
(PUT 'MAKE-MULTIVARIATE-HATVEC-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-MULTIVARIATE-HATVEC-MOD-P 'DEFINED-ON-LINE '125) 
(PUT 'MAKE-MULTIVARIATE-HATVEC-MOD-P 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'MAKE-MULTIVARIATE-HATVEC-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-MULTIVARIATE-HATVEC-MOD-P (BVEC N)
    (PROG (BHATVEC R)
      (SETQ BHATVEC (MKVECT N))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ R 1)
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
           (COND ((NOT (EQUAL J I)) (SETQ R (TIMES-MOD-P R (GETV BVEC J)))))
           (SETQ J (PLUS2 J 1))
           (GO LAB))
         (PUTV BHATVEC I R))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN BHATVEC))) 
(PUT 'MAX-DEGREE-IN-VAR 'NUMBER-OF-ARGS 2) 
(PUT 'MAX-DEGREE-IN-VAR 'DEFINED-ON-LINE '139) 
(PUT 'MAX-DEGREE-IN-VAR 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'MAX-DEGREE-IN-VAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAX-DEGREE-IN-VAR (FVEC V)
    (PROG (R D)
      (SETQ R 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (COND
         ((LESSP R (SETQ D (DEGREE-IN-VARIABLE (GETV FVEC I) V))) (SETQ R D)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN R))) 
(PUT 'MAKE-GROWTH-FACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-GROWTH-FACTOR 'DEFINED-ON-LINE '147) 
(PUT 'MAKE-GROWTH-FACTOR 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'MAKE-GROWTH-FACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-GROWTH-FACTOR (PT)
    (COND ((EQUAL (CDR PT) 0) (LIST (CONS (CONS (CAR PT) 1) 1)))
          (T
           (PLUS-MOD-P (LIST (CONS (CONS (CAR PT) 1) 1))
            (COND ((EQUAL (CDR PT) 0) (CDR PT))
                  (T (IDIFFERENCE CURRENT-MODULUS (CDR PT)))))))) 
(PUT 'TERMS-DONE-MOD-P 'NUMBER-OF-ARGS 3) 
(PUT 'TERMS-DONE-MOD-P 'DEFINED-ON-LINE '152) 
(PUT 'TERMS-DONE-MOD-P 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'TERMS-DONE-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMS-DONE-MOD-P (FVEC DELFVEC DELFACTOR)
    (PROG (FLIST DELFLIST)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ FLIST (CONS (GETV FVEC I) FLIST))
         (SETQ DELFLIST (CONS (GETV DELFVEC I) DELFLIST)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN
       (TERMS-DONE1-MOD-P NUMBER-OF-FACTORS FLIST DELFLIST NUMBER-OF-FACTORS
        DELFACTOR)))) 
(PUT 'TERMS-DONE1-MOD-P 'NUMBER-OF-ARGS 5) 
(PUT 'TERMS-DONE1-MOD-P 'DEFINED-ON-LINE '162) 
(PUT 'TERMS-DONE1-MOD-P 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'TERMS-DONE1-MOD-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TERMS-DONE1-MOD-P (N FLIST DELFLIST R M)
    (COND ((EQUAL N 1) (CONS (CAR FLIST) (CAR DELFLIST)))
          (T
           (PROG (K I F1 F2 DELF1 DELF2)
             (SETQ K (QUOTIENT N 2))
             (SETQ I 1)
             (PROG (F)
               (SETQ F FLIST)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ F2 (CONS F F2)))
                         (T (SETQ F1 (CONS F F1))))
                   (SETQ I (PLUS I 1))))
                (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ I 1)
             (PROG (DELF)
               (SETQ DELF DELFLIST)
              LAB
               (COND ((NULL DELF) (RETURN NIL)))
               ((LAMBDA (DELF)
                  (PROGN
                   (COND ((GREATERP I K) (SETQ DELF2 (CONS DELF DELF2)))
                         (T (SETQ DELF1 (CONS DELF DELF1))))
                   (SETQ I (PLUS I 1))))
                (CAR DELF))
               (SETQ DELF (CDR DELF))
               (GO LAB))
             (SETQ F1 (TERMS-DONE1-MOD-P K F1 DELF1 R M))
             (SETQ DELF1 (CDR F1))
             (SETQ F1 (CAR F1))
             (SETQ F2 (TERMS-DONE1-MOD-P (DIFFERENCE N K) F2 DELF2 R M))
             (SETQ DELF2 (CDR F2))
             (SETQ F2 (CAR F2))
             (SETQ DELF1
                     (PLUS-MOD-P
                      (PLUS-MOD-P (TIMES-MOD-P F1 DELF2)
                       (TIMES-MOD-P F2 DELF1))
                      (TIMES-MOD-P (TIMES-MOD-P DELF1 M) DELF2)))
             (COND ((EQUAL N R) (RETURN DELF1)))
             (RETURN (CONS (TIMES-MOD-P F1 F2) DELF1)))))) 
(PUT 'PRIMITIVE.PARTS 'NUMBER-OF-ARGS 3) 
(PUT 'PRIMITIVE.PARTS 'DEFINED-ON-LINE '188) 
(PUT 'PRIMITIVE.PARTS 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'PRIMITIVE.PARTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRIMITIVE.PARTS (FLIST VAR UNIVARIATE-INPUTS)
    (PROG (C PRIMF)
      (COND
       ((NULL VAR)
        (ERRORF "Must take primitive parts wrt some non-null variable")))
      (COND
       (NON-MONIC
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
               (PRIN2* "Because we multiplied the original primitive ")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "polynomial by a multiple of its leading coefficient ")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "(see (a) above), the factors we have now are not ")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "necessarily primitive. However the required factors ")
               (TERPRI* NIL))
              (PROGN
               (PRIN2* "are merely their primitive parts.")
               (TERPRI* NIL)))
             (WRS STREAM)))))))
      (RETURN
       (PROG (FW FORALL-RESULT FORALL-ENDPTR)
         (SETQ FW FLIST)
         (COND ((NULL FW) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (FW)
                             (PROGN
                              (COND
                               ((NOT
                                 ((LAMBDA (|##Z|)
                                    (AND
                                     (NOT (OR (ATOM |##Z|) (ATOM (CAR |##Z|))))
                                     (EQUAL (CAAAR |##Z|) VAR)))
                                  FW))
                                (ERRORF (LIST "wrong variable" VAR FW))))
                              (SETQ C (COMFAC FW))
                              (COND
                               ((CAR C)
                                (ERRORF
                                 (LIST "factor divisible by main variable:" FW
                                       (CAR C)))))
                              (SETQ PRIMF (QUOTFAIL FW (CDR C)))
                              (COND
                               ((AND (NOT (EQUAL (CDR C) 1)) UNIVARIATE-INPUTS)
                                (MULTIPLY-ALPHAS (CDR C) FW PRIMF)))
                              PRIMF))
                           (CAR FW))
                          NIL)))
        LOOPLABEL
         (SETQ FW (CDR FW))
         (COND ((NULL FW) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (FW)
                     (PROGN
                      (COND
                       ((NOT
                         ((LAMBDA (|##Z|)
                            (AND (NOT (OR (ATOM |##Z|) (ATOM (CAR |##Z|))))
                                 (EQUAL (CAAAR |##Z|) VAR)))
                          FW))
                        (ERRORF (LIST "wrong variable" VAR FW))))
                      (SETQ C (COMFAC FW))
                      (COND
                       ((CAR C)
                        (ERRORF
                         (LIST "factor divisible by main variable:" FW
                               (CAR C)))))
                      (SETQ PRIMF (QUOTFAIL FW (CDR C)))
                      (COND
                       ((AND (NOT (EQUAL (CDR C) 1)) UNIVARIATE-INPUTS)
                        (MULTIPLY-ALPHAS (CDR C) FW PRIMF)))
                      PRIMF))
                   (CAR FW))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MAKE-PREDICTED-FORMS 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-PREDICTED-FORMS 'DEFINED-ON-LINE '216) 
(PUT 'MAKE-PREDICTED-FORMS 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'MAKE-PREDICTED-FORMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-PREDICTED-FORMS (PFS V)
    (PROG (L N PVEC J W)
      (SETQ MAX-UNKNOWNS 0)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV PFS I))
         (SETQ L (SORT (SPREADVAR W V NIL) (FUNCTION LESSP)))
         (SETQ N (IADD1 (LENGTH L)))
         (SETQ NUMBER-OF-UNKNOWNS (CONS (CONS N I) NUMBER-OF-UNKNOWNS))
         (COND ((LESSP MAX-UNKNOWNS N) (SETQ MAX-UNKNOWNS N)))
         (SETQ PVEC (MKVECT (ISUB1 N)))
         (SETQ J 0)
         (PUTV PVEC J (ISUB1 N))
         (PROG (M)
           (SETQ M L)
          LAB
           (COND ((NULL M) (RETURN NIL)))
           ((LAMBDA (M) (PUTV PVEC (SETQ J (IADD1 J)) M)) (CAR M))
           (SETQ M (CDR M))
           (GO LAB))
         (PUTV PFS I PVEC)
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ NUMBER-OF-UNKNOWNS (SORT NUMBER-OF-UNKNOWNS (FUNCTION LESSPCAR)))
      (RETURN MAX-UNKNOWNS))) 
(PUT 'MAKE-CORRECTION-VECTORS 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE-CORRECTION-VECTORS 'DEFINED-ON-LINE '250) 
(PUT 'MAKE-CORRECTION-VECTORS 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'MAKE-CORRECTION-VECTORS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE-CORRECTION-VECTORS (BFS N)
    (PROG (CVS CV)
      (SETQ CVS (MKVECT NUMBER-OF-FACTORS))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN (SETQ CV (MKVECT N)) (PUTV CV 1 (GETV BFS I)) (PUTV CVS I CV))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN CVS))) 
(PUT 'CONSTRUCT-SOLN-MATRICES 'NUMBER-OF-ARGS 2) 
(PUT 'CONSTRUCT-SOLN-MATRICES 'DEFINED-ON-LINE '268) 
(PUT 'CONSTRUCT-SOLN-MATRICES 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'CONSTRUCT-SOLN-MATRICES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONSTRUCT-SOLN-MATRICES (PFS VAL)
    (PROG (SOLN-MATRIX RESVEC N PV)
      (SETQ RESVEC (MKVECT NUMBER-OF-FACTORS))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
        (PROGN
         (SETQ PV (GETV PFS I))
         (SETQ SOLN-MATRIX (MKVECT (SETQ N (IADD1 (GETV PV 0)))))
         (CONSTRUCT-ITH-MATRIX SOLN-MATRIX PV N VAL)
         (PUTV RESVEC I SOLN-MATRIX))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN RESVEC))) 
(PUT 'CONSTRUCT-ITH-MATRIX 'NUMBER-OF-ARGS 4) 
(PUT 'CONSTRUCT-ITH-MATRIX 'DEFINED-ON-LINE '284) 
(PUT 'CONSTRUCT-ITH-MATRIX 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'CONSTRUCT-ITH-MATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONSTRUCT-ITH-MATRIX (SM PV N VAL)
    (PROG (MV)
      (SETQ MV (MKVECT N))
      (PUTV MV 1 1)
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PUTV MV J (MODULAR-EXPT VAL (GETV PV (ISUB1 J))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PUTV SM 1 MV)
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ MV (MKVECT N))
         (PUTV MV 1 0)
         (CONSTRUCT-MATRIX-ROW MV (ISUB1 J) PV N VAL)
         (PUTV SM J MV))
        (SETQ J (PLUS2 J 1))
        (GO LAB)))) 
(PUT 'CONSTRUCT-MATRIX-ROW 'NUMBER-OF-ARGS 5) 
(PUT 'CONSTRUCT-MATRIX-ROW 'DEFINED-ON-LINE '299) 
(PUT 'CONSTRUCT-MATRIX-ROW 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'CONSTRUCT-MATRIX-ROW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONSTRUCT-MATRIX-ROW (MROW J PV N VAL)
    (PROG (D)
      (PROG (K)
        (SETQ K 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
        (PROGN
         (SETQ D (GETV PV (ISUB1 K)))
         (COND ((LESSP D J) (PUTV MROW K 0))
               (T
                (PROGN
                 (SETQ D
                         (REMAINDER
                          (TIMES (*D2N (BINOMIAL-COEFFT-MOD-P D J))
                                 (MODULAR-EXPT VAL (IDIFFERENCE D J)))
                          CURRENT-MODULUS))
                 (PUTV MROW K D)))))
        (SETQ K (PLUS2 K 1))
        (GO LAB)))) 
(PUT 'PRINT-LINEAR-SYSTEMS 'NUMBER-OF-ARGS 4) 
(PUT 'PRINT-LINEAR-SYSTEMS 'DEFINED-ON-LINE '311) 
(PUT 'PRINT-LINEAR-SYSTEMS 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'PRINT-LINEAR-SYSTEMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINT-LINEAR-SYSTEMS (SOLN-M CORRECTION-V PREDICTED-F V)
    (PROGN
     (PROG (I)
       (SETQ I 1)
      LAB
       (COND ((MINUSP (DIFFERENCE NUMBER-OF-FACTORS I)) (RETURN NIL)))
       (PRINT-LINEAR-SYSTEM I SOLN-M CORRECTION-V PREDICTED-F V)
       (SETQ I (PLUS2 I 1))
       (GO LAB))
     (TERPRI* NIL))) 
(PUT 'PRINT-LINEAR-SYSTEM 'NUMBER-OF-ARGS 5) 
(PUT 'PRINT-LINEAR-SYSTEM 'DEFINED-ON-LINE '318) 
(PUT 'PRINT-LINEAR-SYSTEM 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'PRINT-LINEAR-SYSTEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINT-LINEAR-SYSTEM (I SOLN-M CORRECTION-V PREDICTED-F V)
    (PROG (PV SM CV MR N TT)
      (TERPRI* T)
      (PRIN2* " i = ")
      (PROGN (PRIN2* I) (TERPRI* NIL))
      (TERPRI* NIL)
      (SETQ SM (GETV SOLN-M I))
      (SETQ CV (GETV CORRECTION-V I))
      (SETQ PV (GETV PREDICTED-F I))
      (SETQ N (IADD1 (GETV PV 0)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (PRIN2* "(  ")
         (SETQ TT 2)
         (SETQ MR (GETV SM J))
         (PROG (K)
           (SETQ K 1)
          LAB
           (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
           (PROGN (PRIN2* (GETV MR K)) (TTAB* (SETQ TT (PLUS TT 10))))
           (SETQ K (PLUS2 K 1))
           (GO LAB))
         (PRIN2* ")  ( [")
         (COND ((EQUAL J 1) (PRIN2* 1))
               (T
                (PRINSF
                 ((LAMBDA (G592)
                    (COND ((NULL 1) G592)
                          (T
                           (CONS
                            (CONS (GETPOWER (FKERN V) (GETV PV (ISUB1 J))) 1)
                            G592))))
                  POLYZERO))))
         (PRIN2* "]")
         (TTAB* (SETQ TT (PLUS TT 10)))
         (PRIN2* " )")
         (COND ((EQUAL J (QUOTIENT N 2)) (PRIN2* "  =  (  "))
               (T (PRIN2* "     (  ")))
         (PRINSF (GETV CV J))
         (TTAB* (SETQ TT (PLUS TT 30)))
         (PROGN (PRIN2* ")") (TERPRI* NIL))
         (COND
          ((NOT (EQUAL J N))
           (PROGN
            (SETQ TT 2)
            (PRIN2* "(")
            (TTAB* (SETQ TT (PLUS TT (TIMES N 10))))
            (PRIN2* ")  (")
            (TTAB* (SETQ TT (PLUS TT 10)))
            (PRIN2* " )     (")
            (TTAB* (SETQ TT (PLUS TT 30)))
            (PROGN (PRIN2* ")") (TERPRI* NIL))))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (TERPRI* T))) 
(PUT 'TRY-PREDICTION 'NUMBER-OF-ARGS 9) 
(PUT 'TRY-PREDICTION 'DEFINED-ON-LINE '356) 
(PUT 'TRY-PREDICTION 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'TRY-PREDICTION 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE TRY-PREDICTION (SM CV PV N I POLY V FF FFHAT)
    (PROG (W FFI FHATI)
      (SETQ SM (GETV SM I))
      (SETQ CV (GETV CV I))
      (SETQ PV (GETV PV I))
      (COND
       ((NOT (EQUAL N (IADD1 (GETV PV 0))))
        (ERRORF
         (LIST "Predicted unknowns gone wrong? " N (IADD1 (GETV PV 0))))))
      (COND
       ((NULL (GETV (GETV SM 1) 0))
        (PROGN
         (SETQ W (LU-FACTORIZE-MOD-P SM N))
         (COND
          ((EQUAL W 'SINGULAR)
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
                  (PRIN2* "Prediction for ")
                  (PRIN2* (COND ((NULL FF) 'F) (T 'A)))
                  (PRIN2* "(")
                  (PRIN2* I)
                  (PROGN
                   (PRIN2* ") failed due to singular matrix.")
                   (TERPRI* NIL)))
                 (WRS STREAM)))))
            (RETURN (CONS W I))))))))
      (BACK-SUBSTITUTE SM CV N)
      (SETQ W
              (COND ((NULL FF) (TRY-FACTOR POLY CV PV N V))
                    (T
                     (PROGN
                      (SETQ FFI (GETV FF I))
                      (SETQ FHATI (GETV FFHAT I))
                      (TRY-ALPHA POLY CV PV N V FFI FHATI)))))
      (COND
       ((EQUAL W 'BAD-PREDICTION)
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
               (PRIN2* "Prediction for ")
               (PRIN2* (COND ((NULL FF) 'F) (T 'A)))
               (PRIN2* "(")
               (PRIN2* I)
               (PROGN (PRIN2* ") was an inadequate guess.") (TERPRI* NIL)))
              (WRS STREAM)))))
         (RETURN (CONS W I)))))
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
            (PRIN2* "Prediction for ")
            (PRIN2* (COND ((NULL FF) 'F) (T 'A)))
            (PRIN2* "(")
            (PRIN2* I)
            (PRIN2* ") worked: ")
            (PRINTSF (CAR W)))
           (WRS STREAM)))))
      (RETURN (CONS I W)))) 
(PUT 'TRY-FACTOR 'NUMBER-OF-ARGS 5) 
(PUT 'TRY-FACTOR 'DEFINED-ON-LINE '397) 
(PUT 'TRY-FACTOR 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'TRY-FACTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRY-FACTOR (POLY TESTV PREDICTEDF N V)
    (PROG (R W)
      (SETQ R (GETV TESTV 1))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ W
                 ((LAMBDA (G595)
                    (COND ((NULL 1) G595)
                          (T
                           (CONS
                            (CONS
                             (GETPOWER (FKERN V) (GETV PREDICTEDF (ISUB1 J)))
                             1)
                            G595))))
                  POLYZERO))
         (SETQ R (PLUS-MOD-P R (TIMES-MOD-P W (GETV TESTV J)))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (SETQ W (QUOTIENT-MOD-P POLY R))
      (COND
       ((OR (NULL W) (NOT (NULL (DIFFERENCE-MOD-P POLY (TIMES-MOD-P W R)))))
        (RETURN 'BAD-PREDICTION))
       (T (RETURN (LIST R W)))))) 
(PUT 'TRY-ALPHA 'NUMBER-OF-ARGS 7) 
(PUT 'TRY-ALPHA 'DEFINED-ON-LINE '411) 
(PUT 'TRY-ALPHA 'DEFINED-IN-FILE 'FACTOR/MHENSFNS.RED) 
(PUT 'TRY-ALPHA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE TRY-ALPHA (POLY TESTV PREDICTEDF N V FI FHATI)
    (PROG (R W WR)
      (SETQ R (GETV TESTV 1))
      (PROG (J)
        (SETQ J 2)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROGN
         (SETQ W
                 ((LAMBDA (G598)
                    (COND ((NULL 1) G598)
                          (T
                           (CONS
                            (CONS
                             (GETPOWER (FKERN V) (GETV PREDICTEDF (ISUB1 J)))
                             1)
                            G598))))
                  POLYZERO))
         (SETQ R (PLUS-MOD-P R (TIMES-MOD-P W (GETV TESTV J)))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (COND
       ((NULL (SETQ WR (DIFFERENCE-MOD-P POLY (TIMES-MOD-P R FHATI))))
        (RETURN (LIST R WR))))
      (SETQ W (QUOTIENT-MOD-P WR FI))
      (COND
       ((OR (NULL W) (NOT (NULL (DIFFERENCE-MOD-P WR (TIMES-MOD-P W FI)))))
        (RETURN 'BAD-PREDICTION))
       (T (RETURN (LIST R WR)))))) 
(ENDMODULE) 