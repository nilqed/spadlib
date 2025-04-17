(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REACTEQN)) 
(CREATE-PACKAGE '(REACTEQN) '(CONTRIB MISC)) 
(AEVAL (OPERATOR (LIST 'RIGHTARROW))) 
(NEWTOK '((- >) RIGHTARROW)) 
(INFIX (LIST 'RIGHTARROW)) 
(PRECEDENCE (LIST 'RIGHTARROW 'EQUAL)) 
(AEVAL (OPERATOR (LIST 'DOUBLEARROW))) 
(NEWTOK '((< >) DOUBLEARROW)) 
(INFIX (LIST 'DOUBLEARROW)) 
(PRECEDENCE (LIST 'DOUBLEARROW 'EQUAL)) 
(AEVAL (OPERATOR (LIST 'RATE))) 
(GLOBAL '(SPECIES)) 
(SHARE (LIST 'SPECIES)) 
(GLOBAL '(RATES)) 
(SHARE (LIST 'RATES)) 
(PUT 'REAC2ODE 'PSOPFN 'R2OEVAL) 
(PUT 'R2OEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'R2OEVAL 'DEFINED-ON-LINE '54) 
(PUT 'R2OEVAL 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2OEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2OEVAL (U)
    (PROG (R K X RHS LHS RATEL ODEL OLDORDER LHSL RHSL RC)
      (SETQ RC 0)
      (COND
       ((EQCAR SPECIES 'LIST)
        (SETQ ODEL
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (CDR SPECIES))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (CONS (REVAL1 X T) 0)) (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (CONS (REVAL1 X T) 0)) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ U (REVAL1 (CAR U) T))
      (COND ((NOT (EQCAR U 'LIST)) (TYPERR U "list of reactions")))
      (SETQ U (CDR U))
     LOOP
      (COND ((NULL U) (GO FINIS)))
      (SETQ R (REVAL1 (CAR U) T))
      (SETQ U (CDR U))
      (COND
       ((OR (NOT (PAIRP R)) (NOT (MEMQ (CAR R) '(RIGHTARROW DOUBLEARROW))))
        (GO SYNERROR)))
      (SETQ LHS (R2SPECLIST (CADR R)))
      (SETQ RHS (R2SPECLIST (CADDR R)))
      (PROG (X)
        (SETQ X (APPEND LHS RHS))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ ODEL (R2OADDSPECIES (CDR X) ODEL))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ K
              (COND
               ((AND U (SETQ X (REVAL1 (CAR U) T))
                     (NOT
                      (AND (PAIRP X)
                           (MEMQ (CAR X) '(RIGHTARROW DOUBLEARROW)))))
                (PROGN (SETQ U (CDR U)) X))
               (T (LIST 'RATE (SETQ RC (PLUS RC 1))))))
      (SETQ RATEL (CONS K RATEL))
      (R2OREACTION LHS RHS K ODEL)
      (COND
       ((EQUAL (CAR R) 'DOUBLEARROW)
        (PROGN
         (SETQ K
                 (COND
                  ((AND U (SETQ X (REVAL1 (CAR U) T))
                        (NOT
                         (AND (PAIRP X)
                              (MEMQ (CAR X) '(RIGHTARROW DOUBLEARROW)))))
                   (PROGN (SETQ U (CDR U)) X))
                  (T (LIST 'RATE (SETQ RC (PLUS RC 1))))))
         (SETQ RATEL (CONS K RATEL))
         (R2OREACTION RHS LHS K ODEL)
         NIL)))
      (SETQ LHSL (CONS LHS LHSL))
      (SETQ RHSL (CONS RHS RHSL))
      (GO LOOP)
     FINIS
      (SETQ RATEL (REVERSIP RATEL))
      (SETQ RATES (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST RATEL)))
      (PROG (X)
        (SETQ X RATEL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((OR (NUMBERP X) (AND (PAIRP X) (GET (CAR X) 'DNAME)))
             (SETQ RATEL (DELETE X RATEL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ SPECIES
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X ODEL)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (CAR X)) (CAR X))
                                             NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (R2OMAT (CDR SPECIES) (REVERSIP LHSL) (REVERSIP RHSL))
      (PROG (R)
        (SETQ R RATEL)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R) (COND ((NOT (IDP R)) (SETQ RATEL (DELETE R RATEL)))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (COND (RATEL (EVAL (LIST 'ORDER (MKQUOTE RATEL)))))
      (SETQ OLDORDER (SETKORDER (APPEND RATEL (CDR SPECIES))))
      (SETQ ODEL
              (CONS 'LIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X ODEL)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (LIST 'EQUAL (LIST 'DF (CAR X) 'T)
                                                (REVAL1 (CDR X) T)))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (LIST 'EQUAL (LIST 'DF (CAR X) 'T)
                                        (REVAL1 (CDR X) T)))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETKORDER OLDORDER)
      (RETURN ODEL)
     SYNERROR
      (TYPERR R "reaction"))) 
(PUT 'R2OMAT 'NUMBER-OF-ARGS 3) 
(PUT 'R2OMAT 'DEFINED-ON-LINE '109) 
(PUT 'R2OMAT 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2OMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE R2OMAT (SP LHSL RHSL)
    (PROG (M NREAC NSPEC J)
      (SETQ NREAC 0)
      (SETQ NSPEC 0)
      (SETQ J 0)
      (SETQ NSPEC (LENGTH SP))
      (SETQ NREAC (LENGTH LHSL))
      (APPLY 'MATRIX (LIST (LIST (LIST 'INPUTMAT NREAC NSPEC))))
      (APPLY 'MATRIX (LIST (LIST (LIST 'OUTPUTMAT NREAC NSPEC))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE NREAC I)) (RETURN NIL)))
        (PROGN
         (PROG (X)
           (SETQ X (NTH LHSL I))
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ J (R2FINDINDEX (CDR X) SP))
               (SETMATELEM (LIST 'INPUTMAT I J) (CAR X))
               NIL))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (PROG (X)
           (SETQ X (NTH RHSL I))
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ J (R2FINDINDEX (CDR X) SP))
               (SETMATELEM (LIST 'OUTPUTMAT I J) (CAR X))
               NIL))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB)))) 
(PUT 'R2FINDINDEX 'NUMBER-OF-ARGS 2) 
(PUT 'R2FINDINDEX 'DEFINED-ON-LINE '127) 
(PUT 'R2FINDINDEX 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2FINDINDEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE R2FINDINDEX (A L) (R2FINDINDEX1 A L 1)) 
(PUT 'R2FINDINDEX1 'NUMBER-OF-ARGS 3) 
(PUT 'R2FINDINDEX1 'DEFINED-ON-LINE '129) 
(PUT 'R2FINDINDEX1 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2FINDINDEX1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE R2FINDINDEX1 (A L N)
    (COND ((NULL L) (REDERR "index not found")) ((EQUAL A (CAR L)) N)
          (T (R2FINDINDEX1 A (CDR L) (PLUS N 1))))) 
(PUT 'R2SPECLIST 'NUMBER-OF-ARGS 1) 
(PUT 'R2SPECLIST 'DEFINED-ON-LINE '134) 
(PUT 'R2SPECLIST 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2SPECLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2SPECLIST (U)
    (PROGN
     (SETQ U (COND ((EQCAR U 'PLUS) (CDR U)) (T (LIST U))))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (R2SPECLIST1 X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (R2SPECLIST1 X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'R2SPECLIST1 'NUMBER-OF-ARGS 1) 
(PUT 'R2SPECLIST1 'DEFINED-ON-LINE '139) 
(PUT 'R2SPECLIST1 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2SPECLIST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2SPECLIST1 (X)
    (COND ((EQCAR X 'TIMES) (R2SPECLIST2 (CADR X) (CADDR X) (CDDDR X)))
          (T (CONS 1 X)))) 
(PUT 'R2SPECLIST2 'NUMBER-OF-ARGS 3) 
(PUT 'R2SPECLIST2 'DEFINED-ON-LINE '143) 
(PUT 'R2SPECLIST2 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2SPECLIST2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE R2SPECLIST2 (X1 X2 RST)
    (COND
     ((OR (NOT (NULL RST)) (AND (NOT (FIXP X1)) (NOT (FIXP X2))))
      (TYPERR (APPEND (LIST 'TIMES X1 X2) RST) "species"))
     ((FIXP X1) (CONS X1 X2)) (T (CONS X2 X1)))) 
(PUT 'R2OADDSPECIES 'NUMBER-OF-ARGS 2) 
(PUT 'R2OADDSPECIES 'DEFINED-ON-LINE '148) 
(PUT 'R2OADDSPECIES 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2OADDSPECIES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE R2OADDSPECIES (S ODEL)
    (COND ((ASSOC S ODEL) ODEL)
          (T
           (PROGN
            (PRIN2 "new species: ")
            (PRIN2T S)
            (APPEND ODEL (LIST (CONS S 0))))))) 
(PUT 'R2OREACTION 'NUMBER-OF-ARGS 4) 
(PUT 'R2OREACTION 'DEFINED-ON-LINE '154) 
(PUT 'R2OREACTION 'DEFINED-IN-FILE 'MISC/REACTEQN.RED) 
(PUT 'R2OREACTION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE R2OREACTION (LHS RHS K ODEL)
    (PROG (COEFF E)
      (SETQ COEFF K)
      (PROG (X)
        (SETQ X LHS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ COEFF
                   (REVAL1 (LIST 'TIMES COEFF (LIST 'EXPT (CDR X) (CAR X)))
                           NIL)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X LHS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ E (ASSOC (CDR X) ODEL))
            (RPLACD E
                    (REVAL1
                     (LIST 'DIFFERENCE (CDR E) (LIST 'TIMES COEFF (CAR X)))
                     T))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X RHS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ E (ASSOC (CDR X) ODEL))
            (RPLACD E
                    (REVAL1 (LIST 'PLUS (CDR E) (LIST 'TIMES COEFF (CAR X)))
                            T))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN ODEL))) 
(ENDMODULE) 