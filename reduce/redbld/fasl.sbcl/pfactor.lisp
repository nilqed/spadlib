(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PFACTOR)) 
(FLUID
 '(*BALANCED_MOD *GCD CURRENT-MODULUS M-IMAGE-VARIABLE MODULAR-INFO MODULUS/2
   USER-PRIME)) 
(GLOBAL '(LARGEST-SMALL-MODULUS)) 
(PUT 'PFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'PFACTOR 'DEFINED-ON-LINE '40) 
(PUT 'PFACTOR 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'PFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PFACTOR (Q P)
    (PROG (USER-PRIME CURRENT-MODULUS MODULUS/2 R X)
      (COND ((NOT (NUMBERP P)) (TYPERR P "number"))
            ((NOT (PRIMEP P)) (TYPERR P "prime"))
            ((AND (GREATERP P LARGEST-SMALL-MODULUS)
                  (NOT (MEMQ 'CSL LISPSYSTEM*)))
             (REDERR (LIST P "too large a modulus for factorization"))))
      (SETQ USER-PRIME P)
      (SET-MODULUS P)
      (COND
       ((OR (OR (ATOM Q) (ATOM (CAR Q))) (NULL (REDUCE-MOD-P (CDAR Q))))
        (PRIN2T "*** Degenerate case in modular factorization")))
      (COND
       ((NOT (EQUAL (LENGTH (VARIABLES-IN-FORM Q)) 1))
        (RETURN (FCTRFKRONM Q))))
      (SETQ R (REDUCE-MOD-P Q))
      (SETQ X (LNC R))
      (SETQ R (MONIC-MOD-P R))
      (SETQ R (ERRORSET* (LIST 'FACTOR-FORM-MOD-P (MKQUOTE R)) T))
      (COND
       ((NOT (ERRORP R))
        (RETURN
         (CONS X
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CAR R))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (CONS (MOD-ADJUST (CAR J)) (CDR J)))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J) (CONS (MOD-ADJUST (CAR J)) (CDR J)))
                           (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (PRIN2T "****** Factorization failed ******")
      (RETURN (LIST 1 (PREPF Q))))) 
(PUT 'MOD-ADJUST 'NUMBER-OF-ARGS 1) 
(PUT 'MOD-ADJUST 'DEFINED-ON-LINE '70) 
(PUT 'MOD-ADJUST 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'MOD-ADJUST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOD-ADJUST (U) (COND ((NULL *BALANCED_MOD) U) (T (MOD-ADJUST1 U)))) 
(PUT 'MOD-ADJUST1 'NUMBER-OF-ARGS 1) 
(PUT 'MOD-ADJUST1 'DEFINED-ON-LINE '74) 
(PUT 'MOD-ADJUST1 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'MOD-ADJUST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOD-ADJUST1 (U)
    (COND
     ((OR (ATOM U) (ATOM (CAR U)))
      (COND ((FIXP U) (*MODULAR2F U)) ((EQCAR U '|:MOD:|) (*MODULAR2F (CDR U)))
            (T (TYPERR U "modular number"))))
     (T (CONS (CONS (CAAR U) (MOD-ADJUST1 (CDAR U))) (MOD-ADJUST1 (CDR U)))))) 
(PUT 'FACTOR-FORM-MOD-P 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR-FORM-MOD-P 'DEFINED-ON-LINE '82) 
(PUT 'FACTOR-FORM-MOD-P 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'FACTOR-FORM-MOD-P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR-FORM-MOD-P (P) (SORT-FACTORS (FACTORIZE-BY-SQUARE-FREE-MOD-P P))) 
(PUT 'FACTORIZE-BY-SQUARE-FREE-MOD-P 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE-BY-SQUARE-FREE-MOD-P 'DEFINED-ON-LINE '92) 
(PUT 'FACTORIZE-BY-SQUARE-FREE-MOD-P 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'FACTORIZE-BY-SQUARE-FREE-MOD-P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE-BY-SQUARE-FREE-MOD-P (P)
    (COND ((EQUAL P 1) NIL)
          ((OR (ATOM P) (ATOM (CAR P))) (CONS (CONS P 1) NIL))
          (T
           (PROG (DP V)
             (SETQ V (CONS (CONS (GETPOWER (FKERN (CAAAR P)) 1) 1) NIL))
             (SETQ DP 0)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (EQUAL (EVALUATE-MOD-P P (CAAAR V) 0) 0)) (RETURN NIL)))
               (PROGN (SETQ P (QUOTFAIL-MOD-P P V)) (SETQ DP (PLUS DP 1)))
               (GO WHILELABEL))
             (COND
              ((GREATERP DP 0)
               (RETURN (CONS (CONS V DP) (FACTORIZE-BY-SQUARE-FREE-MOD-P P)))))
             (SETQ DP (DERIVATIVE-MOD-P P))
             (COND
              ((EQUAL DP NIL)
               (PROGN
                (SETQ P (DIVIDE-EXPONENTS-BY-P P CURRENT-MODULUS))
                (SETQ P (FACTORIZE-BY-SQUARE-FREE-MOD-P P))
                (RETURN (MULTIPLY-MULTIPLICITIES P CURRENT-MODULUS)))))
             (SETQ DP (GCD-MOD-P P DP))
             (COND ((EQUAL DP 1) (RETURN (FACTORIZE-PP-MOD-P P))))
             (SETQ P (QUOTFAIL-MOD-P P DP))
             (SETQ P (FACTORIZE-PP-MOD-P P))
             (SETQ DP (FACTORIZE-BY-SQUARE-FREE-MOD-P DP))
             (RETURN (MERGEFACTORS P DP)))))) 
(PUT 'DIVIDE-EXPONENTS-BY-P 'NUMBER-OF-ARGS 2) 
(PUT 'DIVIDE-EXPONENTS-BY-P 'DEFINED-ON-LINE '134) 
(PUT 'DIVIDE-EXPONENTS-BY-P 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'DIVIDE-EXPONENTS-BY-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIVIDE-EXPONENTS-BY-P (P N)
    (COND ((OR (ATOM P) (ATOM (CAR P))) P)
          (T
           (CONS
            (CONS (GETPOWER (FKERN (CAAAR P)) (EXACTQUOTIENT (CDAAR P) N))
                  (CDAR P))
            (DIVIDE-EXPONENTS-BY-P (CDR P) N))))) 
(PUT 'EXACTQUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'EXACTQUOTIENT 'DEFINED-ON-LINE '139) 
(PUT 'EXACTQUOTIENT 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'EXACTQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXACTQUOTIENT (A B)
    (PROG (W)
      (SETQ W (DIVIDE A B))
      (COND ((EQUAL (CDR W) 0) (RETURN (CAR W))))
      (ERROR 50 (LIST "Inexact division" (LIST A B W))))) 
(PUT 'MULTIPLY-MULTIPLICITIES 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIPLY-MULTIPLICITIES 'DEFINED-ON-LINE '148) 
(PUT 'MULTIPLY-MULTIPLICITIES 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'MULTIPLY-MULTIPLICITIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIPLY-MULTIPLICITIES (L N)
    (COND ((NULL L) NIL)
          (T
           (CONS (CONS (CAAR L) (TIMES N (CDAR L)))
                 (MULTIPLY-MULTIPLICITIES (CDR L) N))))) 
(PUT 'MERGEFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'MERGEFACTORS 'DEFINED-ON-LINE '154) 
(PUT 'MERGEFACTORS 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'MERGEFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MERGEFACTORS (A B)
    (COND ((NULL A) B) (T (MERGEFACTORS (CDR A) (ADDFACTOR (CAR A) B))))) 
(PUT 'ADDFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'ADDFACTOR 'DEFINED-ON-LINE '161) 
(PUT 'ADDFACTOR 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'ADDFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDFACTOR (A B)
    (COND ((NULL B) (LIST A))
          ((EQUAL (CAR A) (CAAR B))
           (CONS (CONS (CAR A) (PLUS (CDR A) (CDAR B))) (CDR B)))
          (T (CONS (CAR B) (ADDFACTOR A (CDR B)))))) 
(PUT 'FACTORIZE-PP-MOD-P 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE-PP-MOD-P 'DEFINED-ON-LINE '168) 
(PUT 'FACTORIZE-PP-MOD-P 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'FACTORIZE-PP-MOD-P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE-PP-MOD-P (P)
    (PROG (VARS)
      (COND ((EQUAL P 1) (RETURN NIL))
            ((OR (ATOM P) (ATOM (CAR P))) (RETURN (CONS (CONS P 1) NIL))))
      (SETQ VARS (VARIABLES-IN-FORM P))
      (COND ((EQUAL (LENGTH VARS) 1) (RETURN (UNIFAC-MOD-P P))))
      (ERRORF "shambled in pfactor - multivariate case resurfaced"))) 
(PUT 'UNIFAC-MOD-P 'NUMBER-OF-ARGS 1) 
(PUT 'UNIFAC-MOD-P 'DEFINED-ON-LINE '182) 
(PUT 'UNIFAC-MOD-P 'DEFINED-IN-FILE 'FACTOR/PFACTOR.RED) 
(PUT 'UNIFAC-MOD-P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIFAC-MOD-P (P)
    (PROG (MODULAR-INFO M-IMAGE-VARIABLE)
      (COND ((OR (ATOM P) (ATOM (CAR P))) (RETURN NIL))
            ((EQUAL (CDAAR P) 1) (RETURN (CONS (CONS P 1) NIL))))
      (SETQ MODULAR-INFO (MKVECT 1))
      (SETQ M-IMAGE-VARIABLE (CAAAR P))
      (GET-FACTOR-COUNT-MOD-P 1 P USER-PRIME NIL)
      (GET-FACTORS-MOD-P 1 USER-PRIME)
      (RETURN
       (PROG (Z FORALL-RESULT FORALL-ENDPTR)
         (SETQ Z (GETV MODULAR-INFO 1))
         (COND ((NULL Z) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (Z) (CONS Z 1)) (CAR Z)) NIL)))
        LOOPLABEL
         (SETQ Z (CDR Z))
         (COND ((NULL Z) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (Z) (CONS Z 1)) (CAR Z)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(ENDMODULE) 