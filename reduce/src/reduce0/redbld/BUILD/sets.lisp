(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SETS)) 
(DEFLIST '((LOCAL SCALAR)) 'NEWNAM) 
(NEWTOK '((|\\|) SETDIFF)) 
(PUT 'SETDIFF 'FANCY-INFIX-SYMBOL "\\backslash") 
(CREATE-PACKAGE '(SETS) '(CONTRIB MISC)) 
(DE SORT-SET (L) (SORT L (FUNCTION SET-ORDP))) 
(PUT 'SORT-SET 'NUMBER-OF-ARGS 1) 
(PUT 'SORT-SET 'DEFINED-ON-LINE '77) 
(PUT 'SORT-SET 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SORT-SET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SORT-SET 'INLINE '(LAMBDA (L) (SORT L (FUNCTION SET-ORDP)))) 
(PUT 'SET-ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'SET-ORDP 'DEFINED-ON-LINE '80) 
(PUT 'SET-ORDP 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SET-ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SET-ORDP (U V)
    (COND ((AND (NUMBERP U) (NUMBERP V)) (LESSP U V)) (T (ORDP U V)))) 
(INFIX (LIST 'UNION 'INTERSECTION 'SETDIFF)) 
(PUT 'INTERSECT 'NEWNAM 'INTERSECTION) 
(PUT 'MINUS 'NEWNAM 'SETDIFF) 
(PRECEDENCE (LIST 'SETDIFF 'DIFFERENCE)) 
(PRECEDENCE (LIST 'UNION 'SETDIFF)) 
(PRECEDENCE (LIST 'INTERSECTION 'UNION)) 
(PUT 'UNION 'SIMPFN 'SIMPUNION) 
(PUT 'INTERSECTION 'SIMPFN 'SIMPINTERSECTION) 
(PUT 'SETDIFF 'SIMPFN 'SIMPSETDIFF) 
(FLAG '(UNION INTERSECTION) 'NARY) 
(PUT 'UNION 'UNARY 'UNION) 
(PUT 'INTERSECTION 'UNARY 'INTERSECTION) 
(FLAG (LIST 'UNION 'INTERSECTION) 'LISTARGP) 
(FLAG '(UNION INTERSECTION) 'SYMMETRIC) 
(GLOBAL '(EMPTY_SET)) 
(SETQ EMPTY_SET '(LIST)) 
(PUT 'SIMPUNION 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPUNION 'DEFINED-ON-LINE '116) 
(PUT 'SIMPUNION 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SIMPUNION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPUNION (ARGS)
    ((LAMBDA (R)
       (CONS
        (LIST
         (CONS
          (GETPOWER
           (FKERN
            (COND
             ((EQ (CAR R) 'UNION)
              (COND
               ((CDR (SETQ R (DELETE EMPTY_SET (CDR R))))
                (CONS 'UNION (ORDN R)))
               (T (CAR R))))
             (T R)))
           1)
          1))
        1))
     (APPLYSETOP 'UNION ARGS))) 
(PUT 'SIMPINTERSECTION 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINTERSECTION 'DEFINED-ON-LINE '124) 
(PUT 'SIMPINTERSECTION 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SIMPINTERSECTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINTERSECTION (ARGS)
    ((LAMBDA (R)
       (CONS
        (LIST
         (CONS
          (GETPOWER
           (FKERN
            (COND
             ((EQ (CAR R) 'INTERSECTION)
              (COND ((MEMBER EMPTY_SET (SETQ R (CDR R))) EMPTY_SET)
                    ((CDR R) (CONS 'INTERSECTION (ORDN R))) (T (CAR R))))
             (T R)))
           1)
          1))
        1))
     (APPLYSETOP 'INTERSECTION ARGS))) 
(PUT 'SIMPSETDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPSETDIFF 'DEFINED-ON-LINE '132) 
(PUT 'SIMPSETDIFF 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SIMPSETDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPSETDIFF (ARGS)
    ((LAMBDA (R)
       (CONS
        (LIST
         (CONS
          (GETPOWER
           (FKERN
            (COND
             ((EQ (CAR R) 'SETDIFF)
              (COND
               ((OR (EQUAL (CADR R) (CADDR R)) (EQUAL (CADR R) EMPTY_SET))
                EMPTY_SET)
               ((EQUAL (CADDR R) EMPTY_SET) (CADR R)) (T R)))
             (T R)))
           1)
          1))
        1))
     (APPLYSETOP 'SETDIFF ARGS))) 
(FLAG '(UNION |,| INTERSECTION |,| SETDIFF) 'SETVALUED) 
(PUT 'APPLYSETOP 'NUMBER-OF-ARGS 2) 
(PUT 'APPLYSETOP 'DEFINED-ON-LINE '146) 
(PUT 'APPLYSETOP 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'APPLYSETOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APPLYSETOP (SETOP ARGS)
    (PROG (SET_ARG SYM_ARGS SETDIFF_ARGS)
      (SETQ SET_ARG 0)
      (SETQ SETDIFF_ARGS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U ARGS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (COND
                                     ((EQCAR (SETQ U (REVAL1 U T)) 'LIST)
                                      (PROGN
                                       (SETQ U (DELETE-DUPS (CDR U)))
                                       (SETQ SET_ARG
                                               (COND ((EQUAL SET_ARG 0) U)
                                                     (T
                                                      (APPLY2 SETOP SET_ARG
                                                              U))))
                                       (MAKE-SET U)))
                                     ((OR (IDP U)
                                          (AND (PAIRP U)
                                               (FLAGP (CAR U) 'SETVALUED)))
                                      (PROGN
                                       (COND
                                        ((NOT (MEMBER U SYM_ARGS))
                                         (SETQ SYM_ARGS (CONS U SYM_ARGS))))
                                       U))
                                     (T (TYPERR U "set"))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (COND
                             ((EQCAR (SETQ U (REVAL1 U T)) 'LIST)
                              (PROGN
                               (SETQ U (DELETE-DUPS (CDR U)))
                               (SETQ SET_ARG
                                       (COND ((EQUAL SET_ARG 0) U)
                                             (T (APPLY2 SETOP SET_ARG U))))
                               (MAKE-SET U)))
                             ((OR (IDP U)
                                  (AND (PAIRP U) (FLAGP (CAR U) 'SETVALUED)))
                              (PROGN
                               (COND
                                ((NOT (MEMBER U SYM_ARGS))
                                 (SETQ SYM_ARGS (CONS U SYM_ARGS))))
                               U))
                             (T (TYPERR U "set"))))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (COND
        (SYM_ARGS
         (CONS SETOP
               (COND ((EQ SETOP 'SETDIFF) SETDIFF_ARGS)
                     ((EQUAL SET_ARG 0) SYM_ARGS)
                     (T (CONS (MAKE-SET SET_ARG) SYM_ARGS)))))
        (T (REVAL1 (MAKE-SET SET_ARG) NIL)))))) 
(FLAG '(MKSET) 'OPFN) 
(PUT 'MKSET 'NUMBER-OF-ARGS 1) 
(PUT 'MKSET 'DEFINED-ON-LINE '174) 
(PUT 'MKSET 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'MKSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKSET (RLIST) (MAKE-SET (DELETE-DUPS (GETRLIST RLIST)))) 
(PUT 'MAKE-SET 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-SET 'DEFINED-ON-LINE '182) 
(PUT 'MAKE-SET 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'MAKE-SET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-SET (L) (CONS 'LIST (SORT L (FUNCTION SET-ORDP)))) 
(PUT 'DELETE-DUPS 'NUMBER-OF-ARGS 1) 
(PUT 'DELETE-DUPS 'DEFINED-ON-LINE '185) 
(PUT 'DELETE-DUPS 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'DELETE-DUPS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETE-DUPS (L)
    (COND
     (L
      (COND ((MEMBER (CAR L) (CDR L)) (DELETE-DUPS (CDR L)))
            (T (CONS (CAR L) (DELETE-DUPS (CDR L)))))))) 
(INFIX (LIST 'SUBSET_EQ 'SUBSET 'SET_EQ)) 
(PRECEDENCE (LIST 'SUBSET_EQ 'LESSP)) 
(PRECEDENCE (LIST 'SUBSET 'SUBSET_EQ)) 
(PRECEDENCE (LIST 'SET_EQ 'EQUAL)) 
(PUT 'MEMBER 'BOOLFN 'EVALMEMBER) 
(PUT 'SUBSET_EQ 'BOOLFN 'EVALSUBSET_EQ) 
(PUT 'SUBSET 'BOOLFN 'EVALSUBSET) 
(PUT 'SET_EQ 'BOOLFN 'EVALSET_EQ) 
(PUT 'EVALMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'EVALMEMBER 'DEFINED-ON-LINE '205) 
(PUT 'EVALMEMBER 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALMEMBER (EL RLIST) (MEMBER EL (GETRLIST RLIST))) 
(PUT 'EVALSUBSET_EQ 'NUMBER-OF-ARGS 2) 
(PUT 'EVALSUBSET_EQ 'DEFINED-ON-LINE '209) 
(PUT 'EVALSUBSET_EQ 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALSUBSET_EQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALSUBSET_EQ (U V)
    ((LAMBDA (R)
       (COND ((ATOM R) R)
             (T (OR (APPLY (FUNCTION EQUAL) R) (EVALSYMSUBSET R)))))
     (EVALSETBOOL 'SUBSET_EQ U V))) 
(PUT 'SUBSET_EQ 'SETBOOLFN (FUNCTION SUBSETP)) 
(PUT 'EVALSUBSET 'NUMBER-OF-ARGS 2) 
(PUT 'EVALSUBSET 'DEFINED-ON-LINE '215) 
(PUT 'EVALSUBSET 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALSUBSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALSUBSET (U V)
    ((LAMBDA (R) (COND ((ATOM R) R) (T (EVALSYMSUBSET R))))
     (EVALSETBOOL 'SUBSET U V))) 
(PUT 'SUBSETNEQP 'NUMBER-OF-ARGS 2) 
(PUT 'SUBSETNEQP 'DEFINED-ON-LINE '219) 
(PUT 'SUBSETNEQP 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SUBSETNEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBSETNEQP (U V) (AND (SUBSETP U V) (NOT (SUBSETP V U)))) 
(PUT 'SUBSET 'SETBOOLFN (FUNCTION SUBSETNEQP)) 
(PUT 'EVALSYMSUBSET 'NUMBER-OF-ARGS 1) 
(PUT 'EVALSYMSUBSET 'DEFINED-ON-LINE '224) 
(PUT 'EVALSYMSUBSET 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALSYMSUBSET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALSYMSUBSET (ARGS)
    ((LAMBDA (U V)
       (OR (AND (EQCAR V 'UNION) (MEMBER U (CDR V)))
           (AND (EQCAR U 'INTERSECTION) (MEMBER V (CDR U)))
           (AND (EQCAR U 'SETDIFF)
                (OR (EQUAL (CADR U) V)
                    (AND (EQCAR V 'UNION) (MEMBER (CADR U) (CDR V)))))))
     (CAR ARGS) (CADR ARGS))) 
(PUT 'EVALSET_EQ 'NUMBER-OF-ARGS 2) 
(PUT 'EVALSET_EQ 'DEFINED-ON-LINE '239) 
(PUT 'EVALSET_EQ 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALSET_EQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALSET_EQ (U V)
    ((LAMBDA (R) (COND ((ATOM R) R) (T (APPLY (FUNCTION EQUAL) R))))
     (EVALSETBOOL 'SET_EQ U V))) 
(PUT 'SETEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'SETEQUAL 'DEFINED-ON-LINE '243) 
(PUT 'SETEQUAL 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'SETEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETEQUAL (U V) (AND (SUBSETP U V) (SUBSETP V U))) 
(PUT 'SET_EQ 'SETBOOLFN (FUNCTION SETEQUAL)) 
(PUT 'EVALSETBOOL 'NUMBER-OF-ARGS 3) 
(PUT 'EVALSETBOOL 'DEFINED-ON-LINE '248) 
(PUT 'EVALSETBOOL 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALSETBOOL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EVALSETBOOL (SETBOOL U V)
    (PROG (R SET_ARGS SYM_ARGS)
      (SETQ R
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL (LIST U V))
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (COND
                                     ((EQCAR EL 'LIST)
                                      (PROGN (SETQ SET_ARGS T) (CDR EL)))
                                     ((OR (IDP (SETQ EL (REVAL1 EL T)))
                                          (AND (PAIRP EL)
                                               (FLAGP (CAR EL) 'SETVALUED)))
                                      (PROGN (SETQ SYM_ARGS T) EL))
                                     (T (TYPERR EL "set"))))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (COND
                             ((EQCAR EL 'LIST)
                              (PROGN (SETQ SET_ARGS T) (CDR EL)))
                             ((OR (IDP (SETQ EL (REVAL1 EL T)))
                                  (AND (PAIRP EL) (FLAGP (CAR EL) 'SETVALUED)))
                              (PROGN (SETQ SYM_ARGS T) EL))
                             (T (TYPERR EL "set"))))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (COND
        (SET_ARGS
         (COND
          (SYM_ARGS
           (MSGPRI "Cannot evaluate" (LIST SETBOOL (REVAL1 U T) (REVAL1 V T))
                   "as Boolean-valued set expression" NIL T))
          (T (APPLY (GET SETBOOL 'SETBOOLFN) R))))
        (T R))))) 
(FLAG '(EVALB) 'OPFN) 
(PUT 'EVALB 'NUMBER-OF-ARGS 1) 
(PUT 'EVALB 'DEFINED-ON-LINE '272) 
(PUT 'EVALB 'DEFINED-IN-FILE 'MISC/SETS.RED) 
(PUT 'EVALB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALB (CONDITION)
    (COND ((EVAL (FORMBOOL CONDITION NIL 'ALGEBRAIC)) 'TRUE) (T 'FALSE))) 
(FLAG '(EVALB) 'NOVAL) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SETK 'SET_DISTRIBUTION_RULE
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'INTERSECTION (LIST '~ 'X)
                         (LIST 'UNION (LIST '~ 'Y) (LIST '~ 'Z)))
                   (LIST 'UNION (LIST 'INTERSECTION 'X 'Y)
                         (LIST 'INTERSECTION 'X 'Z)))))) 
(ENDMODULE) 