(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PMPATCH)) 
(GLOBAL '(SIMPCOUNT* SIMPLIMIT*)) 
(PUT 'LISTEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'LISTEVAL 'DEFINED-ON-LINE '42) 
(PUT 'LISTEVAL 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'LISTEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTEVAL (U V)
    (PROGN
     (COND
      ((GREATERP (SETQ SIMPCOUNT* (PLUS SIMPCOUNT* 1)) SIMPLIMIT*)
       (PROGN
        (SETQ SIMPCOUNT* 0)
        (RERROR 'RLISP 18 "Simplification recursion too deep"))))
     (SETQ U
             (COND
              ((ATOM U)
               ((LAMBDA (X)
                  (LISTEVAL
                   (COND ((FLAGP U 'SHARE) (EVAL U)) (X (CADR X))
                         (T (TYPERR U 'LIST)))
                   V))
                (GET U 'AVALUE)))
              ((EQUAL (CAR U) 'LIST)
               (CONS 'LIST
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X (CDR U))
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
              (T
               ((LAMBDA (X)
                  (COND (X (APPLY2 X (CDR U) V))
                        (T (RERROR 'RLISP 19 "Illegal operation on lists"))))
                (GET (CAR U) 'LISTFN)))))
     (SETQ SIMPCOUNT* (DIFFERENCE SIMPCOUNT* 1))
     U)) 
(FLUID '(SUBSTITUTION)) 
(PUT 'EQUALREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EQUALREVAL 'DEFINED-ON-LINE '67) 
(PUT 'EQUALREVAL 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'EQUALREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQUALREVAL (U)
    (COND
     ((NULL SUBSTITUTION)
      (CONS 'EQUAL (CONS (CAR U) (LIST (REVAL1 (CADR U) T)))))
     ((EVALEQUAL (CAR U) (CADR U)) T) (T 0))) 
(PUT 'NOSIMP 'NUMBER-OF-ARGS 2) 
(PUT 'NOSIMP 'DEFINED-ON-LINE '81) 
(PUT 'NOSIMP 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'NOSIMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NOSIMP (FN LIST) (PROGN (PUT FN 'NOSIMP LIST) NIL)) 
(FLAG '(NOSIMP) 'OPFN) 
(FLAG '(NOSIMP) 'NOVAL) 
(PUT 'FNREVAL 'NUMBER-OF-ARGS 3) 
(PUT 'FNREVAL 'DEFINED-ON-LINE '88) 
(PUT 'FNREVAL 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'FNREVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FNREVAL (U V MODE)
    (COND ((NULL U) NIL) ((EQ V T) U)
          ((NULL V)
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J U)
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (J) (REVAL1 J MODE)) (CAR J))
                                   NIL)))
            LOOPLABEL
             (SETQ J (CDR J))
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (J) (REVAL1 J MODE)) (CAR J)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          (T
           (CONS (COND ((CAR V) (CAR U)) (T (REVAL1 (CAR U) MODE)))
                 (FNREVAL (CDR U) (CDR V) MODE))))) 
(PUT 'OPFNEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'OPFNEVAL 'DEFINED-ON-LINE '99) 
(PUT 'OPFNEVAL 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'OPFNEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPFNEVAL (U)
    (COND
     ((FLAGP (CAR U) 'REMEMBER)
      (PROG (INTERM RESUL X)
        (SETQ INTERM
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J
                          (COND ((FLAGP (CAR U) 'NOVAL) (CDR U))
                                (T (FNREVAL (CDR U) (GET (CAR U) 'NOSIMP) T))))
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (J)
                                      (COND ((FIXP J) J) (T (MKQUOTE J))))
                                    (CAR J))
                                   NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (J) (COND ((FIXP J) J) (T (MKQUOTE J))))
                            (CAR J))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (COND
         ((SETQ X (ASSOC (CONS (CAR U) INTERM) (GET (CAR U) 'KVALUE)))
          (RETURN (CADR X))))
        (SETQ RESUL (OPFNEVAL1 (CAR U) INTERM))
        (PUT-KVALUE (CAR U) (GET (CAR U) 'KVALUE) (CONS (CAR U) INTERM) RESUL)
        (RETURN RESUL)))
     (T
      (OPFNEVAL1 (CAR U)
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J
                           (COND ((FLAGP (CAR U) 'NOVAL) (CDR U))
                                 (T
                                  (FNREVAL (CDR U) (GET (CAR U) 'NOSIMP) T))))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (MKQUOTE J)) (CAR J))
                                         NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (MKQUOTE J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(FLUID '(NCMP* SUBFG*)) 
(PUT 'SIMPIDEN 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPIDEN 'DEFINED-ON-LINE '118) 
(PUT 'SIMPIDEN 'DEFINED-IN-FILE 'PM/PMPATCH.RED) 
(PUT 'SIMPIDEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPIDEN (U)
    (PROG (BOOL FN X Y Z)
      (SETQ FN (CAR U))
      (SETQ U (CDR U))
      (COND
       ((AND (OR (GET FN '|:RN:|) (GET FN '|:RD:|)) (SETQ X (VALUECHK FN U)))
        (RETURN X)))
      (COND ((FLAGP FN 'SPECFN) (CHECK-ARGNUM FN U)))
      (COND
       ((AND U (EQCAR (CAR U) 'LIST) (NULL (CDR U)))
        (RETURN (MKSQ (LIST FN (REVAL1 (CAR U) NIL)) 1))))
      (SETQ X (FNREVAL U (GET FN 'NOSIMP) NIL))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQCAR J '*SQ) (PREPSQXX (CADR J)))
                                          ((NUMBERP J) J)
                                          (T (PROGN (SETQ BOOL T) J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((EQCAR J '*SQ) (PREPSQXX (CADR J)))
                                  ((NUMBERP J) J) (T (PROGN (SETQ BOOL T) J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND U (EQUAL (CAR U) 0) (FLAGP FN 'ODD) (NOT (FLAGP FN 'NONZERO)))
        (RETURN (CONS NIL 1))))
      (SETQ U (CONS FN U))
      (COND ((FLAGP FN 'NONCOM) (SETQ NCMP* T)))
      (COND ((NULL SUBFG*) (GO C))
            ((AND (FLAGP FN 'LINEAR) (NEQ (SETQ Z (FORMLNR U)) U))
             (RETURN (SIMP Z)))
            ((SETQ Z (OPMTCH U)) (RETURN (SIMP Z))))
     C
      (COND ((FLAGP FN 'SYMMETRIC) (SETQ U (CONS FN (ORDN (CDR U)))))
            ((FLAGP FN 'ANTISYMMETRIC)
             (PROGN
              (COND ((REPEATS (CDR U)) (RETURN (CONS NIL 1)))
                    ((NOT (PERMP (SETQ Z (ORDN (CDR U))) (CDR U))) (SETQ Y T)))
              (SETQ FN (CONS (CAR U) Z))
              (COND
               ((AND (NEQ Z (CDR U)) (SETQ Z (OPMTCH FN)))
                (RETURN (COND (Y (NEGSQ (SIMP Z))) (T (SIMP Z))))))
              (SETQ U FN))))
      (COND
       ((AND (OR (FLAGP FN 'EVEN) (FLAGP FN 'ODD)) X
             (MINUSF (CAR (SETQ X (SIMP (CAR X))))))
        (PROGN
         (COND ((NOT (FLAGP FN 'EVEN)) (SETQ Y (NOT Y))))
         (SETQ U (CONS FN (CONS (PREPSQXX (NEGSQ X)) (CDDR U))))
         (COND
          ((SETQ Z (OPMTCH U))
           (RETURN (COND (Y (NEGSQ (SIMP Z))) (T (SIMP Z)))))))))
      (SETQ U (MKSQ U 1))
      (RETURN (COND (Y (NEGSQ U)) (T U))))) 
(ENDMODULE) 