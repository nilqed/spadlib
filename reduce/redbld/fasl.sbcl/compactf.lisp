(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'COMPACTF)) 
(FLUID '(FRLIS* MV-VARS*)) 
(GLOBAL '(*TRCOMPACT)) 
(SWITCH (LIST 'TRCOMPACT)) 
(PUT 'COMPACT 'SIMPFN 'SIMPCOMPACT) 
(PUT 'SIMPCOMPACT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPCOMPACT 'DEFINED-ON-LINE '42) 
(PUT 'SIMPCOMPACT 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'SIMPCOMPACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPCOMPACT (U)
    (PROG (BOOL)
      (COND
       ((OR (NULL U) (NULL (CDR U)))
        (RERROR 'COMPACT 1 (LIST "Wrong number of arguments to compact"))))
      (COND ((NULL *EXP) (PROGN (RMSUBS) (SETQ BOOL (SETQ *EXP T)))))
      (SETQ U (ERRORSET* (LIST 'SIMPCOMPACT1 (MKQUOTE U)) NIL))
      (COND (BOOL (SETQ *EXP NIL)))
      (COND ((ERRORP U) (RERROR 'COMPACT 2 "Compact error")))
      (RETURN (CAR U)))) 
(PUT 'SIMPCOMPACT1 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPCOMPACT1 'DEFINED-ON-LINE '54) 
(PUT 'SIMPCOMPACT1 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'SIMPCOMPACT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPCOMPACT1 (U)
    (PROG (V X Y W)
      (SETQ V (SIMP* (CAR U)))
      (SETQ U (CADR U))
      (COND
       ((IDP U)
        (COND ((EQCAR (SETQ X (GET U 'AVALUE)) 'LIST) (SETQ U (CADR X)))
              (T (TYPERR U "list"))))
       ((EQ (GETRTYPE U) 'LIST) (SETQ U (CDR U))) (T (TYPERR U "list")))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ W T)
                                     (COND
                                      ((OR (EQCAR J 'EQUAL)
                                           (EQCAR J 'REPLACEBY))
                                       (PROGN
                                        (COND
                                         ((EQCAR (SETQ Y (CADDR J)) 'WHEN)
                                          (PROGN
                                           (SETQ W
                                                   (COMPACTBOOL
                                                    (FORMBOOL (CADDR Y) NIL
                                                              'ALGEBRAIC)))
                                           (SETQ Y (CADR Y)))))
                                        (SETQ J
                                                (LIST 'DIFFERENCE (CADR J)
                                                      Y)))))
                                     (COND
                                      ((SETQ Y (COMPACTFMATCH2 J))
                                       (PROGN
                                        (SETQ J
                                                (SUBLIS
                                                 (PROG (X FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ X Y)
                                                   (COND
                                                    ((NULL X) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (X)
                                                                       (CONS X
                                                                             (CADR
                                                                              X)))
                                                                     (CAR X))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ X (CDR X))
                                                   (COND
                                                    ((NULL X)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X)
                                                               (CONS X
                                                                     (CADR X)))
                                                             (CAR X))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))
                                                 J))
                                        (SETQ J
                                                (SUBLIS
                                                 (PROG (X FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ X Y)
                                                   (COND
                                                    ((NULL X) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (X)
                                                                       (CONS
                                                                        (CADR
                                                                         X)
                                                                        X))
                                                                     (CAR X))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ X (CDR X))
                                                   (COND
                                                    ((NULL X)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X)
                                                               (CONS (CADR X)
                                                                     X))
                                                             (CAR X))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL))
                                                 J)))))
                                     (CONS J W)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ W T)
                             (COND
                              ((OR (EQCAR J 'EQUAL) (EQCAR J 'REPLACEBY))
                               (PROGN
                                (COND
                                 ((EQCAR (SETQ Y (CADDR J)) 'WHEN)
                                  (PROGN
                                   (SETQ W
                                           (COMPACTBOOL
                                            (FORMBOOL (CADDR Y) NIL
                                                      'ALGEBRAIC)))
                                   (SETQ Y (CADR Y)))))
                                (SETQ J (LIST 'DIFFERENCE (CADR J) Y)))))
                             (COND
                              ((SETQ Y (COMPACTFMATCH2 J))
                               (PROGN
                                (SETQ J
                                        (SUBLIS
                                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ X Y)
                                           (COND ((NULL X) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X)
                                                               (CONS X
                                                                     (CADR X)))
                                                             (CAR X))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ X (CDR X))
                                           (COND
                                            ((NULL X) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (X)
                                                       (CONS X (CADR X)))
                                                     (CAR X))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))
                                         J))
                                (SETQ J
                                        (SUBLIS
                                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ X Y)
                                           (COND ((NULL X) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X)
                                                               (CONS (CADR X)
                                                                     X))
                                                             (CAR X))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ X (CDR X))
                                           (COND
                                            ((NULL X) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (X)
                                                       (CONS (CADR X) X))
                                                     (CAR X))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))
                                         J)))))
                             (CONS J W)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (SETQ V (COMPACTSQ V (SIMP* (CAR J)) (CDR J)))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN V))) 
(PUT 'COMPACTBOOL 'NUMBER-OF-ARGS 1) 
(PUT 'COMPACTBOOL 'DEFINED-ON-LINE '80) 
(PUT 'COMPACTBOOL 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTBOOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPACTBOOL (W)
    (COND ((ATOM W) W)
          ((AND (EQCAR W 'LIST) (CDR W) (EQUAL (CADR W) ''~))
           (LIST 'QUOTE (LIST '~ (CADR (CADDR W)))))
          (T (CONS (COMPACTBOOL (CAR W)) (COMPACTBOOL (CDR W)))))) 
(PUT 'COMPACTSQ 'NUMBER-OF-ARGS 3) 
(PUT 'COMPACTSQ 'DEFINED-ON-LINE '91) 
(PUT 'COMPACTSQ 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTSQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTSQ (U V C)
    (PROG ()
      (COND
       ((NEQ (CDR V) 1)
        (MSGPRI "Relation denominator" (PREPF (CDR V)) "discarded" NIL NIL)))
      (SETQ V (CAR V))
      (RETURN
       (MULTSQ (CONS (COMPACTF (CAR U) V C) 1)
               (CONS 1 (COMPACTF (CDR U) V C)))))) 
(PUT 'COMPACTF 'NUMBER-OF-ARGS 3) 
(PUT 'COMPACTF 'DEFINED-ON-LINE '103) 
(PUT 'COMPACTF 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF (U V C)
    (PROG (X N)
      (SETQ N 0)
      (COND
       (*TRCOMPACT
        (PROGN
         (PRIN2T "*** Arguments on entering compactf:")
         (MATHPRINT (MK*SQ (CONS U 1)))
         (MATHPRINT (MK*SQ (CONS V 1))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ X U)) (RETURN NIL)))
        (PROGN (SETQ X U) (SETQ U (COMPACTF0 U V C)) (SETQ N (PLUS N 1)))
        (GO WHILELABEL))
      (COND
       ((AND *TRCOMPACT (GREATERP N 2))
        (PROGN (PRIN2 " *** Compactf looped ") (PRIN2 N) (PRIN2T " times"))))
      (RETURN U))) 
(PUT 'COMPACTF0 'NUMBER-OF-ARGS 3) 
(PUT 'COMPACTF0 'DEFINED-ON-LINE '118) 
(PUT 'COMPACTF0 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF0 (U V C)
    (PROG (X Y W)
      (SETQ X (KERNELS U))
      (SETQ Y (KERNELS V))
      (COND ((NOT (SMEMQ '~ V)) (RETURN (COMPACTF1 U V X Y))))
      (PROG (P)
        (SETQ P (COMPACTFMATCH X Y))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((AND P (NOT (SMEMQ '~ (SETQ W (SUBLIS P C)))) (EVAL W)
                  (NOT (SMEMQ '~ (SETQ W (CAR (SUBF V P))))))
             (SETQ U (COMPACTF1 U W X (KERNELS W))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN U))) 
(PUT 'COMPACTFMATCH 'NUMBER-OF-ARGS 2) 
(PUT 'COMPACTFMATCH 'DEFINED-ON-LINE '130) 
(PUT 'COMPACTFMATCH 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTFMATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPACTFMATCH (X Y)
    (COND ((OR (NULL X) (NULL Y)) '(NIL))
          (T
           (PROG (Y1 Z R)
             (SETQ Z (COMPACTFMATCH X (CDR Y)))
             (COND ((NOT (SMEMQ '~ (CAR Y))) (RETURN Z)))
             (SETQ Y1 (CAR Y))
             (SETQ Y (CDR Y))
             (SETQ R
                     (PROG (X1 FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X1 X)
                      STARTOVER
                       (COND ((NULL X1) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (X1)
                                  (PROG (W FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ W (COMPACTFMATCH1 X1 Y1))
                                   STARTOVER
                                    (COND ((NULL W) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (W)
                                               (PROG (Q FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ Q
                                                         (COMPACTFMATCH
                                                          (DELETE X1 X)
                                                          (SUBLIS W Y)))
                                                 (COND ((NULL Q) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (Q)
                                                                     (UNION W
                                                                            Q))
                                                                   (CAR Q))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ Q (CDR Q))
                                                 (COND
                                                  ((NULL Q)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (Q)
                                                             (UNION W Q))
                                                           (CAR Q))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                             (CAR W)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ W (CDR W))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL W) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            ((LAMBDA (W)
                                               (PROG (Q FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ Q
                                                         (COMPACTFMATCH
                                                          (DELETE X1 X)
                                                          (SUBLIS W Y)))
                                                 (COND ((NULL Q) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (Q)
                                                                     (UNION W
                                                                            Q))
                                                                   (CAR Q))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ Q (CDR Q))
                                                 (COND
                                                  ((NULL Q)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (Q)
                                                             (UNION W Q))
                                                           (CAR Q))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                             (CAR W)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ W (CDR W))
                                    (GO LOOPLABEL)))
                                (CAR X1)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ X1 (CDR X1))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL X1) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (X1)
                                  (PROG (W FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ W (COMPACTFMATCH1 X1 Y1))
                                   STARTOVER
                                    (COND ((NULL W) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (W)
                                               (PROG (Q FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ Q
                                                         (COMPACTFMATCH
                                                          (DELETE X1 X)
                                                          (SUBLIS W Y)))
                                                 (COND ((NULL Q) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (Q)
                                                                     (UNION W
                                                                            Q))
                                                                   (CAR Q))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ Q (CDR Q))
                                                 (COND
                                                  ((NULL Q)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (Q)
                                                             (UNION W Q))
                                                           (CAR Q))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                             (CAR W)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ W (CDR W))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL W) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            ((LAMBDA (W)
                                               (PROG (Q FORALL-RESULT
                                                      FORALL-ENDPTR)
                                                 (SETQ Q
                                                         (COMPACTFMATCH
                                                          (DELETE X1 X)
                                                          (SUBLIS W Y)))
                                                 (COND ((NULL Q) (RETURN NIL)))
                                                 (SETQ FORALL-RESULT
                                                         (SETQ FORALL-ENDPTR
                                                                 (CONS
                                                                  ((LAMBDA (Q)
                                                                     (UNION W
                                                                            Q))
                                                                   (CAR Q))
                                                                  NIL)))
                                                LOOPLABEL
                                                 (SETQ Q (CDR Q))
                                                 (COND
                                                  ((NULL Q)
                                                   (RETURN FORALL-RESULT)))
                                                 (RPLACD FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (Q)
                                                             (UNION W Q))
                                                           (CAR Q))
                                                          NIL))
                                                 (SETQ FORALL-ENDPTR
                                                         (CDR FORALL-ENDPTR))
                                                 (GO LOOPLABEL)))
                                             (CAR W)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ W (CDR W))
                                    (GO LOOPLABEL)))
                                (CAR X1)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ X1 (CDR X1))
                       (GO LOOPLABEL)))
             (RETURN (UNION R Z)))))) 
(PUT 'COMPACTFMATCH1 'NUMBER-OF-ARGS 2) 
(PUT 'COMPACTFMATCH1 'DEFINED-ON-LINE '146) 
(PUT 'COMPACTFMATCH1 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTFMATCH1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPACTFMATCH1 (X Y)
    (COND ((EQUAL (CAR Y) '~) (LIST (LIST (CONS Y X))))
          ((AND (PAIRP X) (EQUAL (CAR X) (CAR Y)))
           ((LAMBDA (FRLIS*) (MCHARG (CDR X) (CDR Y) (CAR Y)))
            (NCONC (COMPACTFMATCH2 Y) FRLIS*))))) 
(PUT 'COMPACTFMATCH2 'NUMBER-OF-ARGS 1) 
(PUT 'COMPACTFMATCH2 'DEFINED-ON-LINE '152) 
(PUT 'COMPACTFMATCH2 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTFMATCH2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPACTFMATCH2 (Y)
    (COND ((ATOM Y) NIL) ((EQUAL (CAR Y) '~) (LIST Y))
          (T (APPEND (COMPACTFMATCH2 (CAR Y)) (COMPACTFMATCH2 (CDR Y)))))) 
(PUT 'COMPACTF1 'NUMBER-OF-ARGS 4) 
(PUT 'COMPACTF1 'DEFINED-ON-LINE '157) 
(PUT 'COMPACTF1 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF1 (U V X Y)
    (PROG (Z)
      (SETQ Z (INTERSECTION X Y))
      (COND ((NULL Z) (RETURN U)))
      (RETURN (COMPACTF11 U V X Y Z)))) 
(PUT 'COMPACTF11 'NUMBER-OF-ARGS 5) 
(PUT 'COMPACTF11 'DEFINED-ON-LINE '173) 
(PUT 'COMPACTF11 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF11 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF11 (U V X Y Z)
    (PROG (W)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN U)))
      (SETQ Y (APPEND Z (SETDIFF Y Z)))
      (SETQ X (APPEND (SETDIFF X Z) Y))
      (SETQ X (SETKORDER X))
      (SETQ U (REORDER U))
      (SETQ V (REORDER V))
      (SETQ Z (COMFAC-TO-POLY (COMFAC U)))
      (SETQ U ((LAMBDA (*EXP) (QUOTF1 U Z)) T))
      (SETQ U (REMCHKF U V Y))
      (SETQ W (COMPACTF2 U (MV-REDUCED-COEFFS (SF2MV V Y)) Y))
      (COND ((LESSP (TERMSF W) (TERMSF U)) (SETQ U W)))
      (COND
       ((NOT (KERNLP Z))
        (PROGN
         (SETQ Z (REMCHKF Z V Y))
         (SETQ W (COMPACTF2 Z (MV-REDUCED-COEFFS (SF2MV V Y)) Y))
         (COND ((LESSP (TERMSF W) (TERMSF Z)) (SETQ Z W))))))
      (SETQ U (COND (*PHYSOP-LOADED (PHYSOP-MULTF Z U)) (T (POLY-MULTF Z U))))
      (SETKORDER X)
      (SETQ U (REORDER U))
      (COND
       (*TRCOMPACT
        (PROGN
         (PRIN2T "*** Value on leaving compactf11:")
         (MATHPRINT (MK*SQ (CONS U 1))))))
      (RETURN U))) 
(PUT 'REMCHKF 'NUMBER-OF-ARGS 3) 
(PUT 'REMCHKF 'DEFINED-ON-LINE '200) 
(PUT 'REMCHKF 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'REMCHKF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REMCHKF (U V VARS)
    ((LAMBDA (X)
       (COND
        ((OR (OR (ATOM X) (ATOM (CAR X)))
             (NULL (INTERSECTION (KERNELS U) VARS)))
         X)
        (T (CONS (CAR X) (REMCHKF (CDR X) V VARS)))))
     (REMCHKF1 U V))) 
(PUT 'REMCHKF1 'NUMBER-OF-ARGS 2) 
(PUT 'REMCHKF1 'DEFINED-ON-LINE '209) 
(PUT 'REMCHKF1 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'REMCHKF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REMCHKF1 (U V)
    (PROG (N)
      (SETQ N 0)
      (SETQ N (TERMSF U))
      (SETQ V (XREMF U V N))
      (COND ((OR (NULL V) (GEQ (TERMSF (SETQ V (CAR V))) N)) (RETURN U))
            (*TRCOMPACT (PRIN2T "*** Remainder smaller")))
      (RETURN V))) 
(PUT 'XREMF 'NUMBER-OF-ARGS 3) 
(PUT 'XREMF 'DEFINED-ON-LINE '218) 
(PUT 'XREMF 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'XREMF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XREMF (U V M)
    (PROG (M1 M2 N X Y Z)
      (SETQ M1 0)
      (SETQ M2 0)
      (SETQ N 0)
      (COND ((OR (ATOM V) (ATOM (CAR V))) (RETURN (LIST (CDR (QREMD U V))))))
      (SETQ M2 (TERMSF U))
     A
      (COND ((LEQ M 0) (RETURN NIL))
            ((OR (ATOM U) (ATOM (CAR U))) (RETURN (LIST (ADDF Z U))))
            ((EQ (CAAAR U) (CAAAR V))
             (COND
              ((LESSP (SETQ N (DIFFERENCE (CDAAR U) (CDAAR V))) 0)
               (RETURN (LIST (ADDF Z U))))
              (T
               (PROGN
                (SETQ X (QREMF (CDAR U) (CDAR V)))
                (SETQ Y
                        ((LAMBDA (G544)
                           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 (CDR X)))
                                 (T (POLY-MULTF G544 (CDR X)))))
                         (LIST (CONS (CAAR U) 1))))
                (SETQ M (PLUS M M1))
                (SETQ Z (ADDF Z Y))
                (SETQ M1 (TERMSF Z))
                (SETQ M (PLUS (DIFFERENCE M M1) M2))
                (SETQ U
                        (COND ((NULL (CAR X)) (CDR U))
                              (T
                               (ADDF
                                (ADDF U
                                      ((LAMBDA (G543 G544)
                                         (COND
                                          (*PHYSOP-LOADED
                                           (PHYSOP-MULTF G543 G544))
                                          (T (POLY-MULTF G543 G544))))
                                       (COND ((EQUAL N 0) V)
                                             (T
                                              ((LAMBDA (G544)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF G544 V))
                                                  (T (POLY-MULTF G544 V))))
                                               (LIST
                                                (CONS (CONS (CAAAR U) N) 1)))))
                                       (NEGF (CAR X))))
                                (NEGF Y)))))
                (SETQ M2 (TERMSF U))
                (SETQ M (DIFFERENCE M M2))
                (GO A)))))
            ((NOT (ORDOP (CAAAR U) (CAAAR V))) (RETURN (LIST (ADDF Z U)))))
      (SETQ M (PLUS M M1))
      (SETQ X (XREMF (CDAR U) V M))
      (COND ((NULL X) (RETURN NIL)))
      (SETQ Z
              (ADDF Z
                    ((LAMBDA (G544)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 (CAR X)))
                             (T (POLY-MULTF G544 (CAR X)))))
                     (LIST (CONS (CAAR U) 1)))))
      (SETQ M1 (TERMSF Z))
      (SETQ M (DIFFERENCE M M1))
      (SETQ U (CDR U))
      (GO A))) 
(PUT 'COMPACTF2 'NUMBER-OF-ARGS 3) 
(PUT 'COMPACTF2 'DEFINED-ON-LINE '255) 
(PUT 'COMPACTF2 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF2 (U V VARS)
    (COND ((OR (ATOM U) (ATOM (CAR U))) U)
          ((MEMQ (CAAAR U) VARS) (COMPACTF3 U V VARS))
          (T
           (CONS (CONS (CAAR U) (COMPACTF2 (CDAR U) V VARS))
                 (COMPACTF2 (CDR U) V VARS))))) 
(PUT 'COMPACTF3 'NUMBER-OF-ARGS 3) 
(PUT 'COMPACTF3 'DEFINED-ON-LINE '262) 
(PUT 'COMPACTF3 'DEFINED-IN-FILE 'MISC/COMPACTF.RED) 
(PUT 'COMPACTF3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPACTF3 (U V VARS)
    (PROG (MV-VARS*)
      (SETQ MV-VARS* VARS)
      (RETURN (MV2SF (MV-COMPACT (SF2MV U VARS) V NIL) VARS)))) 
(ENDMODULE) 