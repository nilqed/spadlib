(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INVBINT)) 
(PUT 'INVTORDER 'NUMBER-OF-ARGS 1) 
(PUT 'INVTORDER 'DEFINED-ON-LINE '28) 
(PUT 'INVTORDER 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'INVTORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVTORDER (U)
    (PROG (W O)
      (SETQ W (REVAL1 (CAR U) T))
      (SETQ O (ASSOC W '((GRADLEX . GLEX) (REVGRADLEX . GREV) (LEX . LEX))))
      (COND ((NULL O) (TYPERR W "involutive term ordering")))
      (SETQ ORDERING (CDR O))
      (SETQ INVSYSVARS*
              (COND
               ((CDR U)
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y (CDR (LISTEVAL (CADR U) NIL)))
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Y) (REVAL1 Y T)) (CAR Y))
                                        NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Y) (REVAL1 Y T)) (CAR Y)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T NIL))))) 
(PUT 'INVTORDER 'STAT 'RLIS) 
(PUT 'INVBASE 'NUMBER-OF-ARGS 1) 
(PUT 'INVBASE 'DEFINED-ON-LINE '42) 
(PUT 'INVBASE 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'INVBASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVBASE (U)
    (PROG (SYS VARS R)
      (SETQ U (REVAL1 (CAR U) T))
      (COND ((NOT (EQCAR U 'LIST)) (REDERR "Argument to invbase not a list")))
      (SETQ SYS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CDR U))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ P (REVAL1 P T))
                                     (COND
                                      ((EQCAR P 'EQUAL)
                                       (SETQ P
                                               (REVAL1
                                                (LIST 'DIFFERENCE (CADR P)
                                                      (CADDR P))
                                                T))))
                                     P))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ P (REVAL1 P T))
                             (COND
                              ((EQCAR P 'EQUAL)
                               (SETQ P
                                       (REVAL1
                                        (LIST 'DIFFERENCE (CADR P) (CADDR P))
                                        T))))
                             P))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (OR INVSYSVARS* (GVARLIS SYS)))
      (READSYS (CONS 'LIST SYS) (CONS 'LIST VARS))
      (INVBASE*)
      (SETQ R
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P GG*)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS 'PLUS
                                          (PROG (M FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ M (GETV GV* (CAR P)))
                                            (COND ((NULL M) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (M)
                                                                (PREPSQ
                                                                 (*DI2Q
                                                                  (LIST M)
                                                                  VARS)))
                                                              (CAR M))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ M (CDR M))
                                            (COND
                                             ((NULL M) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (M)
                                                        (PREPSQ
                                                         (*DI2Q (LIST M)
                                                          VARS)))
                                                      (CAR M))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS 'PLUS
                                  (PROG (M FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ M (GETV GV* (CAR P)))
                                    (COND ((NULL M) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (M)
                                                        (PREPSQ
                                                         (*DI2Q (LIST M)
                                                          VARS)))
                                                      (CAR M))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ M (CDR M))
                                    (COND ((NULL M) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (M)
                                                (PREPSQ (*DI2Q (LIST M) VARS)))
                                              (CAR M))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST R)))) 
(PUT 'INVBASE 'PSOPFN 'INVBASE) 
(PUT 'INVLEX 'NUMBER-OF-ARGS 1) 
(PUT 'INVLEX 'DEFINED-ON-LINE '62) 
(PUT 'INVLEX 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'INVLEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVLEX (U)
    (PROG (SYS VARS R)
      (SETQ U (REVAL1 (CAR U) T))
      (COND ((NOT (EQCAR U 'LIST)) (REDERR "Argument to invlex not a list")))
      (SETQ SYS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CDR U))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ P (REVAL1 P T))
                                     (COND
                                      ((EQCAR P 'EQUAL)
                                       (SETQ P
                                               (REVAL1
                                                (LIST 'DIFFERENCE (CADR P)
                                                      (CADDR P))
                                                T))))
                                     P))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ P (REVAL1 P T))
                             (COND
                              ((EQCAR P 'EQUAL)
                               (SETQ P
                                       (REVAL1
                                        (LIST 'DIFFERENCE (CADR P) (CADDR P))
                                        T))))
                             P))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (OR INVSYSVARS* (GVARLIS SYS)))
      (READSYS (CONS 'LIST SYS) (CONS 'LIST VARS))
      (INVLEX*)
      ((LAMBDA (ORDERING)
         (SETQ R
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P GG*)
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P)
                                       (CONS 'PLUS
                                             (PROG (M FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ M (GETV GV* (CAR P)))
                                               (COND ((NULL M) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (M)
                                                                   (PREPSQ
                                                                    (*DI2Q
                                                                     (LIST M)
                                                                     VARS)))
                                                                 (CAR M))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ M (CDR M))
                                               (COND
                                                ((NULL M)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (M)
                                                           (PREPSQ
                                                            (*DI2Q (LIST M)
                                                             VARS)))
                                                         (CAR M))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL))))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P)
                               (CONS 'PLUS
                                     (PROG (M FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ M (GETV GV* (CAR P)))
                                       (COND ((NULL M) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (M)
                                                           (PREPSQ
                                                            (*DI2Q (LIST M)
                                                             VARS)))
                                                         (CAR M))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ M (CDR M))
                                       (COND ((NULL M) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (M)
                                                   (PREPSQ
                                                    (*DI2Q (LIST M) VARS)))
                                                 (CAR M))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
       'LEX)
      (RETURN (CONS 'LIST R)))) 
(PUT 'INVLEX 'PSOPFN 'INVLEX) 
(PUT 'INVTEST 'NUMBER-OF-ARGS 1) 
(PUT 'INVTEST 'DEFINED-ON-LINE '82) 
(PUT 'INVTEST 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'INVTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVTEST (U)
    (PROG (SYS VARS R)
      (SETQ U (REVAL1 (CAR U) T))
      (COND ((NOT (EQCAR U 'LIST)) (REDERR "Argument to invtest not a list")))
      (SETQ SYS
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CDR U))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ P (REVAL1 P T))
                                     (COND
                                      ((EQCAR P 'EQUAL)
                                       (SETQ P
                                               (REVAL1
                                                (LIST 'DIFFERENCE (CADR P)
                                                      (CADDR P))
                                                T))))
                                     P))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ P (REVAL1 P T))
                             (COND
                              ((EQCAR P 'EQUAL)
                               (SETQ P
                                       (REVAL1
                                        (LIST 'DIFFERENCE (CADR P) (CADDR P))
                                        T))))
                             P))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (OR INVSYSVARS* (GVARLIS SYS)))
      (READSYS (CONS 'LIST SYS) (CONS 'LIST VARS))
      (RETURN (INVTEST*)))) 
(PUT 'INVTEST 'PSOPFN 'INVTEST) 
(PUT 'GVARLIS 'NUMBER-OF-ARGS 1) 
(PUT 'GVARLIS 'DEFINED-ON-LINE '100) 
(PUT 'GVARLIS 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'GVARLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GVARLIS (U) (SORT (GVARLIS1 U NIL) (FUNCTION ORDOP))) 
(PUT 'GVARLIS1 'NUMBER-OF-ARGS 2) 
(PUT 'GVARLIS1 'DEFINED-ON-LINE '104) 
(PUT 'GVARLIS1 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'GVARLIS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GVARLIS1 (U V)
    (COND ((NULL U) V) (T (UNION (GVAR1 (CAR U) V) (GVARLIS1 (CDR U) V))))) 
(PUT 'GVAR1 'NUMBER-OF-ARGS 2) 
(PUT 'GVAR1 'DEFINED-ON-LINE '108) 
(PUT 'GVAR1 'DEFINED-IN-FILE 'INVBASE/INVBINT.RED) 
(PUT 'GVAR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GVAR1 (U V)
    (COND ((OR (NULL U) (NUMBERP U) (AND (EQ U 'I) *COMPLEX)) V)
          ((ATOM U) (COND ((MEMBER U V) V) (T (CONS U V))))
          ((GET (CAR U) 'DNAME) V)
          ((MEMQ (CAR U) '(PLUS TIMES EXPT DIFFERENCE MINUS))
           (GVARLIS1 (CDR U) V))
          ((EQ (CAR U) 'QUOTIENT) (GVAR1 (CADR U) V)) ((MEMBER U V) V)
          (T (CONS U V)))) 
(ENDMODULE) 