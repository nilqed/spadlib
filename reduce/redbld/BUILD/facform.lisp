(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FACFORM)) 
(FLUID
 '(*EXP *EZGCD *FACTOR *FORCE-PRIME *GCD *IFACTOR *NOPOWERS *KERNREVERSE
   *LIMITEDFACTORS *SQFREE *TRFAC CURRENT-MODULUS DMODE* M-IMAGE-VARIABLE
   NCMP*)) 
(SWITCH (LIST 'LIMITEDFACTORS 'NOPOWERS)) 
(PUT 'SQFREE 'SIMPFG
     '((T (RMSUBS) (SETQ *EXP NIL)) (NIL (RMSUBS) (SETQ *EXP T)))) 
(PUT 'FAC-MERGE 'NUMBER-OF-ARGS 2) 
(PUT 'FAC-MERGE 'DEFINED-ON-LINE '60) 
(PUT 'FAC-MERGE 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FAC-MERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FAC-MERGE (U V)
    (CONS
     (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) (CAR V)))
           (T (POLY-MULTF (CAR U) (CAR V))))
     (APPEND (CDR U) (CDR V)))) 
(PUT 'FACTORIZE 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE 'DEFINED-ON-LINE '64) 
(PUT 'FACTORIZE 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTORIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE (U)
    ((LAMBDA (*IFACTOR)
       (PROG (X Y)
         (SETQ X (SIMP* U))
         (SETQ Y (CDR X))
         (COND ((NOT (OR (ATOM Y) (ATOM (CAR Y)))) (TYPERR U "polynomial")))
         (SETQ U (CAR X))
         (COND
          ((EQUAL U 1)
           (RETURN (LIST 'LIST (COND (*NOPOWERS 1) (T (LIST 'LIST 1 1))))))
          ((FIXP U) (SETQ *IFACTOR T)))
         (COND
          ((AND *FORCE-PRIME (NOT (PRIMEP *FORCE-PRIME)))
           (TYPERR *FORCE-PRIME "prime")))
         (SETQ U
                 (COND
                  ((AND DMODE* (NOT (MEMQ DMODE* '(|:RD:| |:CR:|))))
                   (COND
                    ((GET DMODE* 'FACTORFN)
                     (PROG (*FACTOR) (SETQ *FACTOR T) (RETURN (FCTRF U))))
                    (T
                     (RERROR 'POLY 14
                             (LIST "Factorization not supported over domain"
                                   (GET DMODE* 'DNAME))))))
                  (T (FCTRF U))))
         (RETURN (FACFORM2LIST U Y))))
     *IFACTOR)) 
(PUT 'FACFORM2LIST 'NUMBER-OF-ARGS 2) 
(PUT 'FACFORM2LIST 'DEFINED-ON-LINE '88) 
(PUT 'FACFORM2LIST 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACFORM2LIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FACFORM2LIST (X Y)
    (PROG (FACTOR-COUNT Z)
      (COND ((AND (NULL (CAR X)) (NULL (CDR X))) (RETURN (LIST 'LIST)))
            ((NULL *NOPOWERS) (SETQ Z (FACFORM2LIST2 X)))
            (T
             (PROGN
              (SETQ Z (CONS (CONS 0 (CAR X)) NIL))
              (SETQ X (REVERSIP* (CDR X)))
              (SETQ FACTOR-COUNT 0)
              (PROG (FFF)
                (SETQ FFF X)
               LAB
                (COND ((NULL FFF) (RETURN NIL)))
                ((LAMBDA (FFF)
                   (PROG (I)
                     (SETQ I 1)
                    LAB
                     (COND ((MINUSP (DIFFERENCE (CDR FFF) I)) (RETURN NIL)))
                     (SETQ Z
                             (CONS
                              (CONS (SETQ FACTOR-COUNT (PLUS FACTOR-COUNT 1))
                                    (MK*SQ (CONS (CAR FFF) 1)))
                              Z))
                     (SETQ I (PLUS2 I 1))
                     (GO LAB)))
                 (CAR FFF))
                (SETQ FFF (CDR FFF))
                (GO LAB))
              (SETQ Z (MULTIPLE-RESULT Z NIL))
              (COND ((ATOM Z) (TYPERR Z "factor form"))
                    ((AND (NUMBERP (CADR Z)) (LESSP (CADR Z) 0) (CDDR Z))
                     (SETQ Z
                             (CONS (CAR Z)
                                   (CONS (MINUS (CADR Z))
                                         (CONS (MK*SQ (NEGSQ (SIMP (CADDR Z))))
                                               (CDDDR Z)))))))
              (SETQ Z (CDR Z))
              (COND ((EQUAL (CAR Z) 1) (SETQ Z (CDR Z)))
                    ((NOT (FIXP (CAR Z)))
                     (SETQ Z (CONS (PREPD (CAR Z)) (CDR Z))))
                    (*IFACTOR
                     (SETQ Z
                             (APPEND
                              (PAIRLIST2LIST (REVERSIP (ZFACTOR (CAR Z))))
                              (CDR Z))))))))
      (COND ((NEQ Y 1) (SETQ Z (CONS (LIST 'RECIP (PREPD Y)) Z))))
      (RETURN (CONS 'LIST Z)))) 
(PUT 'FACFORM2LIST2 'NUMBER-OF-ARGS 1) 
(PUT 'FACFORM2LIST2 'DEFINED-ON-LINE '120) 
(PUT 'FACFORM2LIST2 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACFORM2LIST2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACFORM2LIST2 (U)
    (PROG (BOOL X)
      (COND
       ((|:MINUSP| (SETQ X (CAR U)))
        (PROGN (SETQ BOOL T) (SETQ X (|:MINUS| X)))))
      (SETQ U (CDR U))
      (COND
       ((NEQ X 1)
        (COND
         ((AND *IFACTOR (FIXP X)) (SETQ U (APPEND (REVERSIP (ZFACTOR X)) U)))
         (T (SETQ U (CONS (CONS X 1) U))))))
      (SETQ X NIL)
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((AND BOOL (NOT (EVENP (CDR J))))
             (PROGN
              (SETQ BOOL NIL)
              (SETQ X (CONS (CONS (NEGF (CAR J)) (CDR J)) X))))
            (T (SETQ X (CONS J X)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ U NIL)
      (PROG (J)
        (SETQ J X)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((FIXP (CAR J)) (SETQ U (CONS (LIST 'LIST (CAR J) (CDR J)) U)))
            (T
             (SETQ U (CONS (LIST 'LIST (MK*SQ (CONS (CAR J) 1)) (CDR J)) U)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (COND (BOOL (CONS '(LIST -1 1) U)) (T U))))) 
(PUT 'OLD_FACTORIZE 'NUMBER-OF-ARGS 1) 
(PUT 'OLD_FACTORIZE 'DEFINED-ON-LINE '142) 
(PUT 'OLD_FACTORIZE 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'OLD_FACTORIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OLD_FACTORIZE (U) ((LAMBDA (*NOPOWERS) (FACTORIZE U)) T)) 
(PUT 'FACTORIZE-MOD-P 'NUMBER-OF-ARGS 2) 
(PUT 'FACTORIZE-MOD-P 'DEFINED-ON-LINE '144) 
(PUT 'FACTORIZE-MOD-P 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTORIZE-MOD-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FACTORIZE-MOD-P (U P)
    (PROG (R S1 S2)
      (SETQ S1 (SETMOD P))
      (SETQ S2 *MODULAR)
      (COND ((NOT S2) (SETDMODE 'MODULAR T)))
      (SETQ R (FACTORIZE U))
      (COND ((NOT S2) (SETDMODE 'MODULAR NIL)))
      (SETMOD S1)
      (RETURN R))) 
(FLAG '(FACTORIZE OLD_FACTORIZE FACTORIZE-MOD-P) 'OPFN) 
(PUT 'PAIRLIST2LIST 'NUMBER-OF-ARGS 1) 
(PUT 'PAIRLIST2LIST 'DEFINED-ON-LINE '158) 
(PUT 'PAIRLIST2LIST 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'PAIRLIST2LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PAIRLIST2LIST (U)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X U)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT ((LAMBDA (X) (NLIST (CAR X) (CDR X))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR ((LAMBDA (X) (NLIST (CAR X) (CDR X))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(DE FACTORF (U) (FCTRF U)) 
(PUT 'FACTORF 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORF 'DEFINED-ON-LINE '168) 
(PUT 'FACTORF 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTORF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FACTORF 'INLINE '(LAMBDA (U) (FCTRF U))) 
(PUT 'FCTRF 'NUMBER-OF-ARGS 1) 
(PUT 'FCTRF 'DEFINED-ON-LINE '171) 
(PUT 'FCTRF 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FCTRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTRF (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (LIST U))
          ((AND (NULL (CDR U)) (OR (ATOM (CDAR U)) (ATOM (CAR (CDAR U)))))
           (LIST (CDAR U)
                 (CONS (CONS (CONS (CONS (CAAAR U) 1) 1) NIL) (CDAAR U))))
          ((AND (EQUAL (CDAAR U) 1) (OR (ATOM (CDAR U)) (ATOM (CAR (CDAR U))))
                (OR (ATOM (CDR U)) (ATOM (CAR (CDR U)))))
           (PROG (G)
             (SETQ G (GCDF (CDAR U) (CDR U)))
             (COND ((MINUSF (CDAR U)) (SETQ G (NEGF G))))
             (RETURN
              (LIST G
                    (CONS
                     (CONS
                      (CONS (CAAR U) ((LAMBDA (*EXP) (QUOTF1 (CDAR U) G)) T))
                      ((LAMBDA (*EXP) (QUOTF1 (CDR U) G)) T))
                     1)))))
          (T
           ((LAMBDA (*EXP NCMP*)
              (PROG (*EZGCD *GCD DENOM X Y)
                (COND
                 ((AND NCMP* (NOT (AND *NCMP (NONCOMFP1 U))))
                  (SETQ NCMP* NIL)))
                (SETQ *GCD T)
                (COND
                 ((AND (NULL *LIMITEDFACTORS) (NULL DMODE*)) (SETQ *EZGCD T)))
                (COND
                 ((NULL *MCD)
                  (RERROR 'POLY 15 "Factorization invalid with MCD off"))
                 ((NULL *EXP)
                  (PROGN (SETQ *EXP T) (SETQ U (*Q2F (RESIMP (CONS U 1)))))))
                (COND
                 ((EQ DMODE* '|:RN:|)
                  (PROGN
                   (SETQ DMODE* NIL)
                   (SETQ ALGLIST* (CONS NIL NIL))
                   (SETQ X (SIMP (PREPF U)))
                   (COND
                    ((ATOM (CDR X))
                     (PROGN (SETQ DENOM (CDR X)) (SETQ U (CAR X))))
                    (T (SETQ DENOM 1))))))
                (COND
                 ((NULL NCMP*)
                  (PROGN
                   (SETQ X (SF2SS U))
                   (COND
                    ((HOMOGP X)
                     (PROGN
                      (COND
                       (*TRFAC
                        (PRIN2T
                         "This polynomial is homogeneous - variables scaled")))
                      (SETQ Y (CONS (CAAAR X) (LISTSUM (CAAADR X))))
                      (SETQ X
                              (FCTRF1
                               (SS2SF
                                (CONS (CAR X)
                                      (CONS (REVERSE (SUBS0 (CADR X))) 1)))))
                      (SETQ X (RCONST Y X))
                      (RETURN (CONS (CAR X) (SORT-FACTORS (CDR X))))))))))
                (SETQ U (FCTRF1 U))
                (COND
                 (DENOM
                  (PROGN
                   (SETQ ALGLIST* (CONS NIL NIL))
                   (SETQ DMODE* '|:RN:|)
                   (SETCAR U (QUOTF* (CAR U) DENOM)))))
                (RETURN (CONS (CAR U) (SORT-FACTORS (CDR U))))))
            *EXP NCMP*)))) 
(PUT 'FCTRF1 'NUMBER-OF-ARGS 1) 
(PUT 'FCTRF1 'DEFINED-ON-LINE '221) 
(PUT 'FCTRF1 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FCTRF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FCTRF1 (U)
    (PROG (X Y Z)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (LIST U))))
      (COND
       ((AND (FLAGP DMODE* 'FIELD) (NEQ (SETQ Z (LNC U)) 1))
        (SETQ U (MULTD (|:RECIP| Z) U)))
       ((AND DMODE* (SETQ Y (GET DMODE* 'UNITSFN)))
        (PROGN
         (SETQ X (APPLY2 Y (CONS 1 U) (LNC U)))
         (SETQ U (CDR X))
         (SETQ Z (|:RECIP| (CAR X))))))
      (SETQ X (COMFAC U))
      (SETQ U ((LAMBDA (*EXP) (QUOTF1 U (COMFAC-TO-POLY X))) T))
      (SETQ Y (FCTRF1 (CDR X)))
      (COND
       ((CAR X)
        (SETQ Y
                (CONS (CAR Y)
                      (CONS (CONS (LIST (CONS (CONS (CAAR X) 1) 1)) (CDAR X))
                            (CDR Y))))))
      (COND ((AND Z (NEQ Z 1)) (SETQ Y (CONS (MULTD Z (CAR Y)) (CDR Y)))))
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (RETURN
         (CONS
          (COND (*PHYSOP-LOADED (PHYSOP-MULTF U (CAR Y)))
                (T (POLY-MULTF U (CAR Y))))
          (CDR Y))))
       ((MINUSF U)
        (PROGN (SETQ U (NEGF U)) (SETQ Y (CONS (NEGF (CAR Y)) (CDR Y))))))
      (RETURN (FAC-MERGE (FACTOR-PRIM-F U) Y)))) 
(PUT 'FACTORIZE-FORM-RECURSION 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIZE-FORM-RECURSION 'DEFINED-ON-LINE '241) 
(PUT 'FACTORIZE-FORM-RECURSION 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTORIZE-FORM-RECURSION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIZE-FORM-RECURSION (U) (FCTRF1 U)) 
(PUT 'FACTOR-PRIM-F 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR-PRIM-F 'DEFINED-ON-LINE '244) 
(PUT 'FACTOR-PRIM-F 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTOR-PRIM-F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR-PRIM-F (U)
    (PROG (V W X Y)
      (COND (NCMP* (RETURN (LIST 1 (CONS U 1)))))
      (COND
       ((AND DMODE* (SETQ X (GET DMODE* 'SQFRFACTORFN)))
        (COND (*FACTOR (SETQ V (APPLY1 X U)))
              (T (SETQ V (LIST 1 (CONS U 1))))))
       ((AND (FLAGP DMODE* 'FIELD) (NEQ (SETQ W (LNC U)) 1))
        (SETQ V (CONS W (SQFRF (MULTD (|:RECIP| W) U)))))
       ((AND (SETQ W (GET DMODE* 'UNITS)) (SETQ W (ASSOC (SETQ Y (LNC U)) W)))
        (SETQ V (CONS Y (SQFRF (MULTD (CDR W) U)))))
       (T (SETQ V (CONS 1 (SQFRF U)))))
      (COND ((AND X (EQ X (GET DMODE* 'FACTORFN))) (RETURN V)))
      (SETQ W (LIST (CAR V)))
      (PROG (X)
        (SETQ X (CDR V))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ W (FAC-MERGE (FACTOR-PRIM-SQFREE-F X) W))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN W))) 
(PUT 'FACTOR-PRIM-SQFREE-F 'NUMBER-OF-ARGS 1) 
(PUT 'FACTOR-PRIM-SQFREE-F 'DEFINED-ON-LINE '265) 
(PUT 'FACTOR-PRIM-SQFREE-F 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTOR-PRIM-SQFREE-F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTOR-PRIM-SQFREE-F (U)
    (PROG (X Y *MSG R)
      (SETQ R *ROUNDED)
      (COND
       ((AND R (UNIVARIATEP (CAR U)) (EQUAL (CDAR (CAR U)) 1)
             (EQUAL (CDR U) 1))
        (RETURN (UNIFACTOR U)))
       ((OR R *COMPLEX *RATIONAL)
        (PROGN
         (COND (R (ON (LIST 'RATIONAL))))
         (SETQ U (CONS (CAR (RESIMP (CONS (CAR U) 1))) (CDR U))))))
      (COND
       ((NULL *LIMITEDFACTORS)
        (PROGN
         (COND ((NULL DMODE*) (SETQ Y 'INTERNAL-FACTORF))
               (T
                (PROGN
                 (SETQ X (GET DMODE* 'SQFRFACTORFN))
                 (SETQ Y (GET DMODE* 'FACTORFN))
                 (COND ((AND X (NOT (EQ X Y))) (SETQ Y 'INTERNAL-FACTORF))
                       ((EQUAL Y 'FACTORF) (SETQ Y 'INTERNAL-FACTORF))))))
         (COND
          (Y
           (PROGN
            (SETQ Y (APPLY1 Y (CAR U)))
            (SETQ U
                    (CONS (EXPTF (CAR Y) (CDR U))
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CDR Y))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J)
                                                (CONS (CAR J) (CDR U)))
                                              (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (J) (CONS (CAR J) (CDR U)))
                                      (CAR J))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
            (GO RET)))))))
      (SETQ U (FACTOR-PRIM-SQFREE-F-1 (CAR U) (CDR U)))
     RET
      (COND
       (R
        (PROGN
         (ON (LIST 'ROUNDED))
         (SETQ U
                 (CONS (CAR U)
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J (CDR U))
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J)
                                             (CONS
                                              (CAR (RESIMP (CONS (CAR J) 1)))
                                              (CDR J)))
                                           (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (CONS (CAR (RESIMP (CONS (CAR J) 1)))
                                           (CDR J)))
                                   (CAR J))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (RETURN U))) 
(PUT 'UNIFACTOR 'NUMBER-OF-ARGS 1) 
(PUT 'UNIFACTOR 'DEFINED-ON-LINE '300) 
(PUT 'UNIFACTOR 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'UNIFACTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIFACTOR (U)
    (COND
     ((NOT (EQCAR (SETQ U (ROOT_VAL (LIST (MK*SQ U)))) 'LIST))
      (ERRACH (LIST "unifactor1" U)))
     (T
      (CONS 1
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDR U))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (COND
                                   ((NOT (EQCAR J 'EQUAL))
                                    (ERRACH (LIST "unifactor2" U)))
                                   (T
                                    (ADDSQ
                                     (CONS (LIST (CONS (CONS (CADR J) 1) 1)) 1)
                                     (NEGSQ (SIMP (CADDR J)))))))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND
                           ((NOT (EQCAR J 'EQUAL))
                            (ERRACH (LIST "unifactor2" U)))
                           (T
                            (ADDSQ (CONS (LIST (CONS (CONS (CADR J) 1) 1)) 1)
                                   (NEGSQ (SIMP (CADDR J)))))))
                        (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))))) 
(PUT 'DISTRIBUTE.MULTIPLICITY 'NUMBER-OF-ARGS 2) 
(PUT 'DISTRIBUTE.MULTIPLICITY 'DEFINED-ON-LINE '307) 
(PUT 'DISTRIBUTE.MULTIPLICITY 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'DISTRIBUTE.MULTIPLICITY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DISTRIBUTE.MULTIPLICITY (FACTORLIST N)
    (PROG (W FORALL-RESULT FORALL-ENDPTR)
      (SETQ W FACTORLIST)
      (COND ((NULL W) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (W) (CONS W N)) (CAR W)) NIL)))
     LOOPLABEL
      (SETQ W (CDR W))
      (COND ((NULL W) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (W) (CONS W N)) (CAR W)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'INTERNAL-FACTORF 'NUMBER-OF-ARGS 1) 
(PUT 'INTERNAL-FACTORF 'DEFINED-ON-LINE '314) 
(PUT 'INTERNAL-FACTORF 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'INTERNAL-FACTORF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTERNAL-FACTORF (U)
    ((LAMBDA (CURRENT-MODULUS)
       (PROG (M-IMAGE-VARIABLE NEW-KORDER OLD-KORDER SIGN V W D)
         (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (LIST U))))
         (SETQ NEW-KORDER (KERNORD U NIL))
         (COND (*KERNREVERSE (SETQ NEW-KORDER (REVERSE NEW-KORDER))))
         (SETQ OLD-KORDER (SETKORDER NEW-KORDER))
         (SETQ U (REORDER U))
         (COND ((MINUSF U) (PROGN (SETQ SIGN (NOT SIGN)) (SETQ U (NEGF U)))))
         (SETQ V (COMFAC U))
         (SETQ U (QUOTF1 U (COMFAC-TO-POLY V)))
         (COND ((OR (ATOM U) (ATOM (CAR U))) (PROGN (SETQ D U) (SETQ U NIL)))
               (T
                (PROGN
                 (SETQ D 1)
                 (SETQ M-IMAGE-VARIABLE (CAAAR U))
                 (SETQ U
                         (DISTRIBUTE.MULTIPLICITY
                          (FACTORIZE-PRIMITIVE-POLYNOMIAL U) 1)))))
         (SETQ W (CAR V))
         (SETQ V (FCTRF1 (CDR V)))
         (COND
          (W
           (SETQ V
                   (CONS (CAR V)
                         (CONS (CONS (LIST (CONS (CONS (CAR W) 1) 1)) (CDR W))
                               (CDR V))))))
         (COND (SIGN (SETQ D (NEGF D))))
         (SETQ U (FAC-MERGE V (CONS D U)))
         (SETKORDER OLD-KORDER)
         (SETQ V
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (CDR U))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P)
                                       (CONS
                                        (COND
                                         ((MINUSF (SETQ D (REORDER (CAR P))))
                                          (PROGN
                                           (SETQ SIGN (NOT SIGN))
                                           (NEGF D)))
                                         (T D))
                                        (CDR P)))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P)
                               (CONS
                                (COND
                                 ((MINUSF (SETQ D (REORDER (CAR P))))
                                  (PROGN (SETQ SIGN (NOT SIGN)) (NEGF D)))
                                 (T D))
                                (CDR P)))
                             (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN
          (CONS (COND ((NEQ SIGN (MINUSF (CAR U))) (NEGF (CAR U))) (T (CAR U)))
                V))))
     CURRENT-MODULUS)) 
(PUT 'FACTOR-PRIM-SQFREE-F-1 'NUMBER-OF-ARGS 2) 
(PUT 'FACTOR-PRIM-SQFREE-F-1 'DEFINED-ON-LINE '380) 
(PUT 'FACTOR-PRIM-SQFREE-F-1 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'FACTOR-PRIM-SQFREE-F-1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FACTOR-PRIM-SQFREE-F-1 (U N)
    ((LAMBDA (X)
       (CONS (EXPTF (CAR X) N)
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J (CDR X))
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (CONS J N)) (CAR J)) NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (CONS J N)) (CAR J)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
     (PRSQFRFACF U))) 
(PUT 'SQFRF 'NUMBER-OF-ARGS 1) 
(PUT 'SQFRF 'DEFINED-ON-LINE '384) 
(PUT 'SQFRF 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'SQFRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQFRF (U)
    (PROG (N *GCD UNITS V W X Y Z *MSG R)
      (SETQ N 0)
      (SETQ *GCD T)
      (COND
       ((SETQ R *ROUNDED)
        (PROGN (ON (LIST 'RATIONAL)) (SETQ U (CAR (RESIMP (CONS U 1)))))))
      (SETQ N 1)
      (SETQ X (CAAAR U))
      ((LAMBDA (*EZGCD) (SETQ V (GCDF U (DIFF U X)))) T)
      (SETQ U (QUOTF-FAIL U V))
      (COND
       ((AND (FLAGP DMODE* 'FIELD) (NEQ (SETQ Y (LNC U)) 1))
        (PROGN (SETQ U (MULTD (|:RECIP| Y) U)) (SETQ V (MULTD Y V)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (DEGR V X) 0)) (RETURN NIL)))
        (PROGN
         (SETQ W (GCDF V U))
         (COND
          ((NEQ U W)
           (SETQ Z (CONS (CONS ((LAMBDA (*EXP) (QUOTF1 U W)) T) N) Z))))
         (SETQ V ((LAMBDA (*EXP) (QUOTF1 V W)) T))
         (SETQ U W)
         (SETQ N (PLUS N 1)))
        (GO WHILELABEL))
      (COND
       (R
        (PROGN
         (ON (LIST 'ROUNDED))
         (SETQ U (CAR (RESIMP (CONS U 1))))
         (SETQ Z
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J Z)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (CONS (CAR (RESIMP (CONS (CAR J) 1)))
                                             (CDR J)))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J)
                               (CONS (CAR (RESIMP (CONS (CAR J) 1))) (CDR J)))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (COND ((AND (NEQ V 1) (ASSOC V UNITS)) (SETQ V 1)))
      (COND
       ((NEQ V 1)
        (COND
         ((EQUAL N 1)
          (SETQ U
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF V U))
                        (T (POLY-MULTF V U)))))
         ((SETQ W (RASSOC 1 Z))
          (RPLACA W
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF V (CAR W)))
                        (T (POLY-MULTF V (CAR W))))))
         ((AND (NULL Z) (NEQ (SETQ W (ROOTXF V N)) 'FAILED))
          (SETQ U
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF W U))
                        (T (POLY-MULTF W U)))))
         ((NOT (OR (ATOM V) (ATOM (CAR V)))) (SETQ Z (ACONC Z (CONS V 1))))
         (T (ERRACH (LIST "sqfrf failure" U N Z))))))
      (RETURN (CONS (CONS U N) Z)))) 
(PUT 'SQUARE_FREE 'NUMBER-OF-ARGS 1) 
(PUT 'SQUARE_FREE 'DEFINED-ON-LINE '431) 
(PUT 'SQUARE_FREE 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'SQUARE_FREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQUARE_FREE (U)
    (CONS 'LIST
          (PROG (V FORALL-RESULT FORALL-ENDPTR)
            (SETQ V (SQFRF (*Q2F (SIMP* U))))
            (COND ((NULL V) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (V)
                                (LIST 'LIST (MK*SQ (CONS (CAR V) 1)) (CDR V)))
                              (CAR V))
                             NIL)))
           LOOPLABEL
            (SETQ V (CDR V))
            (COND ((NULL V) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (V)
                        (LIST 'LIST (MK*SQ (CONS (CAR V) 1)) (CDR V)))
                      (CAR V))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(FLAG '(SQUARE_FREE) 'OPFN) 
(PUT 'DIFF 'NUMBER-OF-ARGS 2) 
(PUT 'DIFF 'DEFINED-ON-LINE '437) 
(PUT 'DIFF 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'DIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFF (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (ADDF
            (ADDF
             ((LAMBDA (G612)
                ((LAMBDA (G544)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G612))
                         (T (POLY-MULTF G544 G612))))
                 (LIST (CONS (CAAR U) 1))))
              (DIFF (CDAR U) V))
             ((LAMBDA (G614)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) G614))
                      (T (POLY-MULTF (CDAR U) G614))))
              (DIFFP1 (CAAR U) V)))
            (DIFF (CDR U) V))))) 
(PUT 'DIFFP1 'NUMBER-OF-ARGS 2) 
(PUT 'DIFFP1 'DEFINED-ON-LINE '445) 
(PUT 'DIFFP1 'DEFINED-IN-FILE 'POLY/FACFORM.RED) 
(PUT 'DIFFP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFFP1 (U V)
    (COND ((NOT (EQ (CAR U) V)) NIL) ((EQUAL (CDR U) 1) 1)
          (T
           (MULTD (CDR U)
                  (LIST (CONS (CONS (CAR U) (DIFFERENCE (CDR U) 1)) 1)))))) 
(ENDMODULE) 