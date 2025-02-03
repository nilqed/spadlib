(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RANDPOLY)) 
(CREATE-PACKAGE '(RANDPOLY) '(CONTRIB MISC)) 
(DE RAND_APPLY_C (C) (SIMP* (APPLY C NIL))) 
(PUT 'RAND_APPLY_C 'NUMBER-OF-ARGS 1) 
(PUT 'RAND_APPLY_C 'DEFINED-ON-LINE '35) 
(PUT 'RAND_APPLY_C 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND_APPLY_C 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'RAND_APPLY_C 'INLINE '(LAMBDA (C) (SIMP* (APPLY C NIL)))) 
(PUT 'APPLY_E 'NUMBER-OF-ARGS 1) 
(PUT 'APPLY_E 'DEFINED-ON-LINE '40) 
(PUT 'APPLY_E 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'APPLY_E 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE APPLY_E (E)
    (COND ((FIXP (SETQ E (APPLY E NIL))) E)
          (T (REDERR "randpoly expons function must return an integer")))) 
(PUT 'RANDPOLY 'SIMPFN 'RANDPOLY) 
(FLAG '(RANDPOLY) 'LISTARGP) 
(PUT 'RANDPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'RANDPOLY 'DEFINED-ON-LINE '50) 
(PUT 'RANDPOLY 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RANDPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANDPOLY (U)
    (PROG (V UNIVAR C E TRMS D O S P SUBLIST)
      (SETQ C (FUNCTION (LAMBDA () (DIFFERENCE (RANDOM 199) 99))))
      (SETQ D 5)
      (SETQ O 0)
      (SETQ TRMS 6)
      (SETQ S 'SPARSE)
      (PROG (WTL*)
        (SETQ V (CAR (SETQ U (REVLIS U))))
        (SETQ V
                (COND ((EQCAR V 'LIST) (CDR V))
                      (T (PROGN (SETQ UNIVAR T) (CONS V NIL)))))
        (SETQ V
                (PROG (VV FORALL-RESULT FORALL-ENDPTR)
                  (SETQ VV V)
                  (COND ((NULL VV) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (VV)
                                      (PROG (TMPVAR)
                                        (COND
                                         ((EQEXPR VV)
                                          (PROGN
                                           (SETQ VV (*EQN2A VV))
                                           (SETQ O 1)))
                                         ((KERNP (SIMP* VV)) (RETURN VV)))
                                        (SETQ TMPVAR (GENSYM))
                                        (SETQ SUBLIST
                                                (CONS (LIST 'EQUAL TMPVAR VV)
                                                      SUBLIST))
                                        (RETURN TMPVAR)))
                                    (CAR VV))
                                   NIL)))
                 LOOPLABEL
                  (SETQ VV (CDR VV))
                  (COND ((NULL VV) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (VV)
                              (PROG (TMPVAR)
                                (COND
                                 ((EQEXPR VV)
                                  (PROGN (SETQ VV (*EQN2A VV)) (SETQ O 1)))
                                 ((KERNP (SIMP* VV)) (RETURN VV)))
                                (SETQ TMPVAR (GENSYM))
                                (SETQ SUBLIST
                                        (CONS (LIST 'EQUAL TMPVAR VV) SUBLIST))
                                (RETURN TMPVAR)))
                            (CAR VV))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (COND (UNIVAR (SETQ V (CAR V)))))
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((OR (EQ X 'DENSE) (EQ X 'SPARSE)) (SETQ S X))
                 ((NOT
                   (AND (EQEXPR X)
                        (COND
                         ((AND (EQ (CADR X) 'COEFFS)
                               (RAND_FUNCTIONP (CADDR X)))
                          (SETQ C (CADDR X)))
                         ((AND (EQ (CADR X) 'EXPONS)
                               (RAND_FUNCTIONP (CADDR X)))
                          (SETQ E (CADDR X)))
                         ((AND (MEMQ (CADR X) '(DEGREE DEG MAXDEG))
                               (NATNUMP (CADDR X)))
                          (SETQ D (CADDR X)))
                         ((AND (MEMQ (CADR X) '(ORD MINDEG))
                               (NATNUMP (CADDR X)))
                          (SETQ O (CADDR X)))
                         ((AND (EQ (CADR X) 'TERMS) (NATNUMP (CADDR X)))
                          (SETQ TRMS (CADDR X))))))
                  (TYPERR X "optional randpoly argument"))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ P (CONS NIL 1))
      (COND
       ((LEQ O D)
        (COND
         ((EQ S 'SPARSE)
          (COND
           ((NULL E)
            (PROG (X)
              (SETQ X (RAND-MONS-SPARSE V TRMS D O UNIVAR))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (SETQ P (ADDSQ P (MULTSQ (SIMP* (APPLY C NIL)) (CONS X 1)))))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB)))
           (UNIVAR
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE TRMS I)) (RETURN NIL)))
              (SETQ P
                      (ADDSQ P
                             (MULTSQ (SIMP* (APPLY C NIL))
                                     (*KP2Q V (APPLY_E E)))))
              (SETQ I (PLUS2 I 1))
              (GO LAB)))
           (T
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE TRMS I)) (RETURN NIL)))
              ((LAMBDA (CC)
                 (COND
                  ((CAR CC)
                   (SETQ P
                           (ADDSQ P
                                  (PROGN
                                   (PROG (VV)
                                     (SETQ VV V)
                                    LAB
                                     (COND ((NULL VV) (RETURN NIL)))
                                     ((LAMBDA (VV)
                                        (SETQ CC
                                                (MULTSQ CC
                                                        (*KP2Q VV
                                                         (APPLY_E E)))))
                                      (CAR VV))
                                     (SETQ VV (CDR VV))
                                     (GO LAB))
                                   CC))))))
               (SIMP* (APPLY C NIL)))
              (SETQ I (PLUS2 I 1))
              (GO LAB)))))
         (UNIVAR
          (PROGN
           (SETQ P (SIMP* (APPLY C NIL)))
           (COND ((GREATERP O 0) (SETQ P (MULTSQ P (MKSQ V O)))))
           (PROG (I)
             (SETQ I (PLUS O 1))
            LAB
             (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
             (SETQ P (ADDSQ P (MULTSQ (SIMP* (APPLY C NIL)) (MKSQ V I))))
             (SETQ I (PLUS2 I 1))
             (GO LAB))))
         (T
          (PROG (X)
            (SETQ X (RAND-MONS-DENSE V D O))
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X)
               (SETQ P (ADDSQ P (MULTSQ (SIMP* (APPLY C NIL)) (CONS X 1)))))
             (CAR X))
            (SETQ X (CDR X))
            (GO LAB))))))
      (RETURN
       (COND (SUBLIST (SIMP* (SUBEVAL (APPEND SUBLIST (LIST (MK*SQ P))))))
             (T P))))) 
(PUT 'RAND_FUNCTIONP 'NUMBER-OF-ARGS 1) 
(PUT 'RAND_FUNCTIONP 'DEFINED-ON-LINE '142) 
(PUT 'RAND_FUNCTIONP 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND_FUNCTIONP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAND_FUNCTIONP (F) (OR (GETD F) (EQCAR F 'LAMBDA))) 
(PUT 'NATNUMP 'NUMBER-OF-ARGS 1) 
(PUT 'NATNUMP 'DEFINED-ON-LINE '146) 
(PUT 'NATNUMP 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'NATNUMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NATNUMP (N) (AND (FIXP N) (GEQ N 0))) 
(DE KP2F (K P) (CAR (MKSQ K P))) 
(PUT 'KP2F 'NUMBER-OF-ARGS 2) 
(PUT 'KP2F 'DEFINED-ON-LINE '150) 
(PUT 'KP2F 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'KP2F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'KP2F 'INLINE '(LAMBDA (K P) (CAR (MKSQ K P)))) 
(PUT '*KP2F 'NUMBER-OF-ARGS 2) 
(PUT '*KP2F 'DEFINED-ON-LINE '156) 
(PUT '*KP2F 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT '*KP2F 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *KP2F (K P) (COND ((GREATERP P 0) (CAR (MKSQ K P))) (T 1))) 
(PUT '*KP2Q 'NUMBER-OF-ARGS 2) 
(PUT '*KP2Q 'DEFINED-ON-LINE '162) 
(PUT '*KP2Q 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT '*KP2Q 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *KP2Q (K P)
    (COND ((GREATERP P 0) (MKSQ K P)) ((ZEROP P) (CONS 1 1))
          ((NULL (CAR (SETQ K (MKSQ K (MINUS P))))) (REDERR "Zero divisor"))
          (T (CONS (CDR K) (CAR K))))) 
(PUT 'RAND-MONS-DENSE 'NUMBER-OF-ARGS 3) 
(PUT 'RAND-MONS-DENSE 'DEFINED-ON-LINE '175) 
(PUT 'RAND-MONS-DENSE 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND-MONS-DENSE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RAND-MONS-DENSE (V D O)
    (PROG (V_1)
      (SETQ V_1 (CAR V))
      (SETQ V (CDR V))
      (RETURN
       (COND
        ((NULL V)
         (CONS (COND ((GREATERP O 0) (CAR (MKSQ V_1 O))) (T 1))
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I (PLUS O 1))
                 (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (CAR (MKSQ V_1 I)) NIL)))
                LOOPLABEL
                 (SETQ I (PLUS2 I 1))
                 (COND ((MINUSP (DIFFERENCE D I)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (CAR (MKSQ V_1 I)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        (T
         (APPEND (RAND-MONS-DENSE V D O)
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                  STARTOVER
                   (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (V_1^I)
                              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                (SETQ X
                                        (RAND-MONS-DENSE V (DIFFERENCE D I)
                                         (MAX 0 (DIFFERENCE O I))))
                                (COND ((NULL X) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (X)
                                                    (COND
                                                     (*PHYSOP-LOADED
                                                      (PHYSOP-MULTF V_1^I X))
                                                     (T (POLY-MULTF V_1^I X))))
                                                  (CAR X))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ X (CDR X))
                                (COND ((NULL X) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (COND
                                             (*PHYSOP-LOADED
                                              (PHYSOP-MULTF V_1^I X))
                                             (T (POLY-MULTF V_1^I X))))
                                          (CAR X))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR (MKSQ V_1 I))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ I (PLUS2 I 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((MINUSP (DIFFERENCE D I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (V_1^I)
                              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                (SETQ X
                                        (RAND-MONS-DENSE V (DIFFERENCE D I)
                                         (MAX 0 (DIFFERENCE O I))))
                                (COND ((NULL X) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (X)
                                                    (COND
                                                     (*PHYSOP-LOADED
                                                      (PHYSOP-MULTF V_1^I X))
                                                     (T (POLY-MULTF V_1^I X))))
                                                  (CAR X))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ X (CDR X))
                                (COND ((NULL X) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (COND
                                             (*PHYSOP-LOADED
                                              (PHYSOP-MULTF V_1^I X))
                                             (T (POLY-MULTF V_1^I X))))
                                          (CAR X))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR (MKSQ V_1 I))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ I (PLUS2 I 1))
                   (GO LOOPLABEL)))))))) 
(PUT 'RAND-MONS-SPARSE 'NUMBER-OF-ARGS 5) 
(PUT 'RAND-MONS-SPARSE 'DEFINED-ON-LINE '193) 
(PUT 'RAND-MONS-SPARSE 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND-MONS-SPARSE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RAND-MONS-SPARSE (V TRMS D O UNIVAR)
    (PROG (N V_1 MAXTRMS OTRMS S)
      (COND (UNIVAR (SETQ MAXTRMS (PLUS D (DIFFERENCE 1 O))))
            (T
             (PROGN
              (SETQ N (LENGTH V))
              (SETQ V_1 (CAR V))
              (SETQ OTRMS
                      (COND ((ZEROP O) 0)
                            (T (BINOMIAL (PLUS N (DIFFERENCE O 1)) N))))
              (SETQ MAXTRMS (DIFFERENCE (BINOMIAL (PLUS N D) N) OTRMS)))))
      (SETQ S (RAND-COMB MAXTRMS (MIN MAXTRMS TRMS)))
      (RETURN
       (COND
        (UNIVAR
         (PROG (SS FORALL-RESULT FORALL-ENDPTR)
           (SETQ SS S)
           (COND ((NULL SS) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (SS) (*KP2F V (PLUS SS O))) (CAR SS))
                                 NIL)))
          LOOPLABEL
           (SETQ SS (CDR SS))
           (COND ((NULL SS) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (SS) (*KP2F V (PLUS SS O))) (CAR SS)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
        (T
         (PROG (SS FORALL-RESULT FORALL-ENDPTR)
           (SETQ SS S)
           (COND ((NULL SS) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SS)
                               (PROG (P)
                                 (SETQ P 1)
                                 (SETQ SS
                                         (CONS NIL
                                               (INTTOVEC (PLUS SS OTRMS) N)))
                                 (PROG (VV)
                                   (SETQ VV V)
                                  LAB
                                   (COND ((NULL VV) (RETURN NIL)))
                                   ((LAMBDA (VV)
                                      (SETQ P
                                              ((LAMBDA (G122)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF G122 P))
                                                  (T (POLY-MULTF G122 P))))
                                               (*KP2F VV
                                                (CAR (SETQ SS (CDR SS)))))))
                                    (CAR VV))
                                   (SETQ VV (CDR VV))
                                   (GO LAB))
                                 (RETURN P)))
                             (CAR SS))
                            NIL)))
          LOOPLABEL
           (SETQ SS (CDR SS))
           (COND ((NULL SS) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (SS)
                       (PROG (P)
                         (SETQ P 1)
                         (SETQ SS (CONS NIL (INTTOVEC (PLUS SS OTRMS) N)))
                         (PROG (VV)
                           (SETQ VV V)
                          LAB
                           (COND ((NULL VV) (RETURN NIL)))
                           ((LAMBDA (VV)
                              (SETQ P
                                      ((LAMBDA (G122)
                                         (COND
                                          (*PHYSOP-LOADED
                                           (PHYSOP-MULTF G122 P))
                                          (T (POLY-MULTF G122 P))))
                                       (*KP2F VV (CAR (SETQ SS (CDR SS)))))))
                            (CAR VV))
                           (SETQ VV (CDR VV))
                           (GO LAB))
                         (RETURN P)))
                     (CAR SS))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(GLOBAL '(_BINOMIALK _BINOMIALB _BINOMIALN)) 
(PUT 'BINOMIAL 'NUMBER-OF-ARGS 2) 
(PUT 'BINOMIAL 'DEFINED-ON-LINE '233) 
(PUT 'BINOMIAL 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'BINOMIAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BINOMIAL (N K)
    (PROG (N1 B)
      (COND ((EQUAL K 0) (RETURN 1)))
      (COND ((LESSP N (TIMES 2 K)) (RETURN (BINOMIAL N (DIFFERENCE N K)))))
      (SETQ N1 (PLUS N 1))
      (COND
       ((EQUAL _BINOMIALN N)
        (PROGN
         (SETQ B _BINOMIALB)
         (COND
          ((LEQ _BINOMIALK K)
           (PROG (I)
             (SETQ I (PLUS _BINOMIALK 1))
            LAB
             (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
             (SETQ B (QUOTIENT (TIMES (DIFFERENCE N1 I) B) I))
             (SETQ I (PLUS2 I 1))
             (GO LAB)))
          (T
           (PROG (I)
             (SETQ I _BINOMIALK)
            LAB
             (COND
              ((MINUSP (TIMES (MINUS 1) (DIFFERENCE (PLUS K 1) I)))
               (RETURN NIL)))
             (SETQ B (QUOTIENT (TIMES I B) (DIFFERENCE N1 I)))
             (SETQ I (PLUS2 I (MINUS 1)))
             (GO LAB))))))
       (T
        (PROGN
         (SETQ B 1)
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
           (SETQ B (QUOTIENT (TIMES (DIFFERENCE N1 I) B) I))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ _BINOMIALN N))))
      (SETQ _BINOMIALK K)
      (RETURN (SETQ _BINOMIALB B)))) 
(PUT 'RAND-COMB 'NUMBER-OF-ARGS 2) 
(PUT 'RAND-COMB 'DEFINED-ON-LINE '258) 
(PUT 'RAND-COMB 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND-COMB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RAND-COMB (N M)
    (COND
     ((EQUAL M N)
      (PROG (I FORALL-RESULT FORALL-ENDPTR)
        (SETQ I 0)
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE M 1) I)) (RETURN NIL)))
        (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
       LOOPLABEL
        (SETQ I (PLUS2 I 1))
        (COND
         ((MINUSP (DIFFERENCE (DIFFERENCE M 1) I)) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS I NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (PROG (S)
        (COND
         ((LESSP (DIFFERENCE N M) M)
          (PROG (R)
            (SETQ R (RAND-COMB N (DIFFERENCE N M)))
            (PROG (RR)
              (SETQ RR 0)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) RR)) (RETURN NIL)))
              (COND ((NOT (MEMBER RR R)) (SETQ S (CONS RR S))))
              (SETQ RR (PLUS2 RR 1))
              (GO LAB))))
         (T
          (PROG (I)
            (SETQ I 0)
           LAB
            (COND ((MINUSP (DIFFERENCE (DIFFERENCE M 1) I)) (RETURN NIL)))
            (PROG (RR)
              (PROG ()
               WHILELABEL
                (COND ((NOT (MEMBER (SETQ RR (RANDOM N)) S)) (RETURN NIL)))
               NIL
                (GO WHILELABEL))
              (SETQ S (CONS RR S)))
            (SETQ I (PLUS2 I 1))
            (GO LAB))))
        (RETURN S))))) 
(PUT 'INTTOVEC 'NUMBER-OF-ARGS 2) 
(PUT 'INTTOVEC 'DEFINED-ON-LINE '281) 
(PUT 'INTTOVEC 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'INTTOVEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTTOVEC (M N) (INTTOVEC1 N (INTTOVEC-SOLVE N M))) 
(PUT 'INTTOVEC1 'NUMBER-OF-ARGS 2) 
(PUT 'INTTOVEC1 'DEFINED-ON-LINE '286) 
(PUT 'INTTOVEC1 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'INTTOVEC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTTOVEC1 (N DM)
    (COND ((EQUAL N 1) (CONS (CAR DM) NIL))
          (T
           ((LAMBDA (DM1)
              (CONS (DIFFERENCE (CAR DM) (CAR DM1))
                    (INTTOVEC1 (DIFFERENCE N 1) DM1)))
            (INTTOVEC-SOLVE (DIFFERENCE N 1) (CDR DM)))))) 
(PUT 'INTTOVEC-SOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'INTTOVEC-SOLVE 'DEFINED-ON-LINE '295) 
(PUT 'INTTOVEC-SOLVE 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'INTTOVEC-SOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTTOVEC-SOLVE (N M)
    (COND ((OR (EQUAL M 0) (EQUAL N 1)) (CONS M 0))
          (T
           (PROG (D C CC)
             (SETQ D 0)
             (SETQ CC 1)
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ C CC)
                (SETQ D (PLUS D 1))
                (SETQ CC (QUOTIENT (TIMES (PLUS N D) C) D))
                NIL)
               (COND ((NOT (GREATERP CC M)) (GO REPEATLABEL))))
             (RETURN (CONS D (DIFFERENCE M C))))))) 
(NEWTOK '((|.| |.|) *INTERVAL*)) 
(PRECEDENCE (LIST '*INTERVAL* 'OR)) 
(AEVAL (OPERATOR (LIST '*INTERVAL*))) 
(PUT '*INTERVAL* 'PRTCH '| .. |) 
(PUT 'RAND 'PSOPFN 'RAND) 
(PUT 'RAND 'NUMBER-OF-ARGS 1) 
(PUT 'RAND 'DEFINED-ON-LINE '335) 
(PUT 'RAND 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'RAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAND (U)
    (COND
     ((OR (NULL U) (AND (CDR U) (CDDR U)))
      (REDERR "rand takes 1 or 2 arguments"))
     (T
      (PROG (FNAME FN)
        (COND
         ((AND (CDR U) (NOT (IDP (SETQ FNAME (REVAL1 (CADR U) T)))))
          (TYPERR FNAME "procedure name")))
        (SETQ FN
                (COND
                 ((AND (FIXP (SETQ U (REVAL1 (CAR U) T))) (GREATERP U 0))
                  (LIST 'RANDOM U))
                 ((EQCAR U '*INTERVAL*)
                  (PROG (A B)
                    (COND
                     ((NOT
                       (AND (FIXP (SETQ A (CADR U))) (FIXP (SETQ B (CADDR U)))
                            (LESSP A B)))
                      (REDERR
                       "rand range argument a .. b must have integer a,b with a < b")))
                    (RETURN
                     (COND ((ZEROP A) (LIST 'RANDOM (PLUS B 1)))
                           (T
                            (LIST 'PLUS2 A
                                  (LIST 'RANDOM
                                        (PLUS (DIFFERENCE B A) 1))))))))
                 (T (TYPERR U "integer or integer range"))))
        (SETQ FN (LIST 'LAMBDA NIL FN))
        (RETURN
         (COND (FNAME (PROGN (FLAG (LIST FNAME) 'OPFN) (PUTD FNAME 'EXPR FN)))
               (*COMP (PUTD (GENSYM) 'EXPR FN)) (T FN))))))) 
(REMFLAG '(RANDOM) 'OPFN) 
(PUT 'RANDOM 'PSOPFN 'EVALRANDOM) 
(PUT 'EVALRANDOM 'NUMBER-OF-ARGS 1) 
(PUT 'EVALRANDOM 'DEFINED-ON-LINE '366) 
(PUT 'EVALRANDOM 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'EVALRANDOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALRANDOM (U)
    (COND ((OR (NULL U) (CDR U)) (REDERR "random takes a single argument"))
          ((EQCAR (SETQ U (REVAL1 (CAR U) T)) '*INTERVAL*)
           (PROG (A B)
             (COND
              ((NOT
                (AND (FIXP (SETQ A (CADR U))) (FIXP (SETQ B (CADDR U)))
                     (LESSP A B)))
               (REDERR
                "random range argument a .. b must have integer a,b with a < b")))
             (RETURN
              (COND ((ZEROP A) (RANDOM (PLUS B 1)))
                    (T (PLUS A (RANDOM (PLUS (DIFFERENCE B A) 1))))))))
          ((AND (FIXP U) (GREATERP U 0)) (RANDOM U))
          (T (TYPERR U "integer or integer range")))) 
(PUT 'PROC 'PSOPFN 'PROC) 
(PUT 'PROC 'NUMBER-OF-ARGS 1) 
(PUT 'PROC 'DEFINED-ON-LINE '388) 
(PUT 'PROC 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'PROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROC (U)
    (COND ((NULL U) (REDERR "proc requires at least a body argument"))
          (T
           (PROGN
            (SETQ U (LIST 'LAMBDA NIL (LIST 'AEVAL* (MKQUOTE (CAR U)))))
            (COND (*COMP (PUTD (GENSYM) 'EXPR U)) (T U)))))) 
(PUT 'EVALPROC 'PSOPFN 'EVALPROC) 
(PUT 'EVALPROC 'NUMBER-OF-ARGS 1) 
(PUT 'EVALPROC 'DEFINED-ON-LINE '406) 
(PUT 'EVALPROC 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'EVALPROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALPROC (R) (APPLY (GETPROC (CAR R)) (REVLIS (CDR R)))) 
(PUT 'SHOWPROC 'PSOPFN 'SHOWPROC) 
(PUT 'SHOWPROC 'NUMBER-OF-ARGS 1) 
(PUT 'SHOWPROC 'DEFINED-ON-LINE '413) 
(PUT 'SHOWPROC 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'SHOWPROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHOWPROC (R)
    ((LAMBDA (RR)
       (COND
        ((CODEP RR) (REDERR "Argument is a compiled proc -- cannot display"))
        (T (PROGN (TERPRI) (RPRINT (SUBST 'PLUS 'PLUS2 RR)) NIL))))
     (GETPROC (CAR R)))) 
(PUT 'GETPROC 'NUMBER-OF-ARGS 1) 
(PUT 'GETPROC 'DEFINED-ON-LINE '420) 
(PUT 'GETPROC 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'GETPROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETPROC (R)
    (OR
     (COND
      ((IDP R)
       (OR (GETFNBODY R)
           (AND (SETQ R (GET R 'AVALUE)) (EQ (CAR R) 'SCALAR)
                (EQCAR (SETQ R (CADR R)) 'LAMBDA) R)
           (GETFNBODY R)))
      ((PAIRP R)
       (COND ((EQ (CAR R) 'LAMBDA) R)
             ((EQCAR
               (SETQ R
                       ((LAMBDA (X) (COND (X (APPLY X (LIST (CDR R))))))
                        (GET (CAR R) 'PSOPFN)))
               'LAMBDA)
              R))))
     (REDERR "Argument is not a proc"))) 
(PUT 'GETFNBODY 'NUMBER-OF-ARGS 1) 
(PUT 'GETFNBODY 'DEFINED-ON-LINE '433) 
(PUT 'GETFNBODY 'DEFINED-IN-FILE 'MISC/RANDPOLY.RED) 
(PUT 'GETFNBODY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETFNBODY (R) (AND (SETQ R (GETD R)) (CDR R))) 
(ENDMODULE) 