(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'POLINEQ)) 
(GLOBAL '(!ARBINT)) 
(COND ((NOT (GET 'ARBREAL 'SIMPFN)) (MKOP 'ARBREAL))) 
(PUT 'POLINEQEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'POLINEQEVAL 'DEFINED-ON-LINE '39) 
(PUT 'POLINEQEVAL 'DEFINED-IN-FILE 'SOLVE/POLINEQ.RED) 
(PUT 'POLINEQEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLINEQEVAL (U)
    (PROG (W X)
      (SETQ W (REVAL1 (CAR U) T))
      (COND
       ((EQCAR W 'LIST)
        (SETQ W
                (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Q (CDR W))
                  (COND ((NULL Q) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Q) (REVAL1 Q T)) (CAR Q))
                                        NIL)))
                 LOOPLABEL
                  (SETQ Q (CDR Q))
                  (COND ((NULL Q) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Q) (REVAL1 Q T)) (CAR Q)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (T (SETQ W (LIST W))))
      (COND ((CDR U) (SETQ X (REVAL1 (CADR U) T))))
      (COND
       ((EQCAR X 'LIST)
        (COND ((CDDR X) (TYPERR X "variable")) (T (SETQ X (CADR X))))))
      (RETURN (POLINEQ0 W X)))) 
(PUT 'POLINEQ0 'NUMBER-OF-ARGS 2) 
(PUT 'POLINEQ0 'DEFINED-ON-LINE '50) 
(PUT 'POLINEQ0 'DEFINED-IN-FILE 'SOLVE/POLINEQ.RED) 
(PUT 'POLINEQ0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLINEQ0 (UL X)
    (PROG (B N D L W WL OP U R S Y Z)
     LOOP
      (SETQ U (CAR UL))
      (SETQ UL (CDR UL))
      (COND
       ((OR (NOT (PAIRP U))
            (NOT (MEMQ (SETQ OP (CAR U)) '(GEQ GREATERP LEQ LESSP))))
        (GO TYPERR)))
      (SETQ S (OR S (EQUAL OP 'GREATERP) (EQUAL OP 'LESSP)))
      (SETQ W
              (SIMP
               (COND
                ((OR (EQUAL OP 'GREATERP) (EQUAL OP 'GEQ))
                 (LIST 'DIFFERENCE (CADR U) (CADDR U)))
                (T (LIST 'DIFFERENCE (CADDR U) (CADR U))))))
      (SETQ WL (CONS W WL))
      (SETQ Y
              (OR
               (AND (NOT (OR (ATOM (CAR W)) (ATOM (CAR (CAR W)))))
                    (CAAAR (CAR W)))
               (AND (NOT (OR (ATOM (CDR W)) (ATOM (CAR (CDR W)))))
                    (CAAAR (CDR W)))))
      (COND
       ((OR (NULL Y) (AND X (NEQ X Y))
            (AND (PAIRP Y) (OR (GET (CAR Y) '|:RD:|) (GET (CAR Y) 'OPMTCH))))
        (GO TYPERR)))
      (SETQ X Y)
      (SETQ N (APPEND N (POLINEQ-REALROOTS (CAR W) X)))
      (SETQ D (APPEND D (POLINEQ-REALROOTS (CDR W) X)))
      (COND (UL (GO LOOP)))
      (PROG (Y)
        (SETQ Y (APPEND N D))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (COND ((NOT (MEMBER Y B)) (SETQ B (CONS Y B))))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (COND
       ((NULL B)
        (RETURN
         (COND
          ((POLINEQCHECK WL (LIST (CONS X 0)))
           (LIST 'LIST
                 (LIST 'EQUAL X
                       (LIST 'ARBREAL (SETQ !ARBINT (PLUS !ARBINT 1))))))
          (T '(LIST))))))
      (SETQ B (SORT B 'EVALLESSP))
      (PROG ()
       WHILELABEL
        (COND ((NOT B) (RETURN NIL)))
        (PROGN
         (COND
          ((NULL L)
           (SETQ L (LIST (LIST (LIST 'DIFFERENCE (CAR B) 1) NIL (CAR B))))))
         (SETQ Z
                 (COND
                  ((CDR B) (LIST 'QUOTIENT (LIST 'PLUS (CAR B) (CADR B)) 2))
                  (T (LIST 'PLUS (CAR B) 1))))
         (SETQ L (CONS (LIST Z (CAR B) (COND ((CDR B) (CADR B)))) L))
         (SETQ B (CDR B))
         NIL)
        (GO WHILELABEL))
      (PROG (V)
        (SETQ V L)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (COND
             ((POLINEQCHECK WL (LIST (CONS X (CAR V))))
              (SETQ R
                      (CONS
                       (COND
                        ((NULL (CADR V))
                         (LIST (COND (S 'LESSP) (T 'LEQ)) X (CADDR V)))
                        ((NULL (CADDR V))
                         (LIST (COND (S 'GREATERP) (T 'GEQ)) X (CADR V)))
                        (T (LIST 'EQUAL X (CONS '*INTERVAL* (CDR V)))))
                       R))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (CONS 'LIST R))
     TYPERR
      (REDERR "wrong arguments for polynomial inequality solver"))) 
(PUT 'POLINEQCHECK 'NUMBER-OF-ARGS 2) 
(PUT 'POLINEQCHECK 'DEFINED-ON-LINE '97) 
(PUT 'POLINEQCHECK 'DEFINED-IN-FILE 'SOLVE/POLINEQ.RED) 
(PUT 'POLINEQCHECK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLINEQCHECK (WL P)
    (OR (NULL WL)
        (AND (NOT (MINUSF (CAR (SUBSQ (CAR WL) P))))
             (POLINEQCHECK (CDR WL) P)))) 
(PUT 'POLINEQ-REALROOTS 'NUMBER-OF-ARGS 2) 
(PUT 'POLINEQ-REALROOTS 'DEFINED-ON-LINE '101) 
(PUT 'POLINEQ-REALROOTS 'DEFINED-IN-FILE 'SOLVE/POLINEQ.RED) 
(PUT 'POLINEQ-REALROOTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLINEQ-REALROOTS (U X)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (PROG (F FORALL-RESULT FORALL-ENDPTR)
             (SETQ F (CDR (FCTRF U)))
            STARTOVER
             (COND ((NULL F) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (F)
                        (PROGN
                         (SETQ F (CAR F))
                         (COND
                          ((NEQ (CAAAR F) X) (REDERR "too many variables")))
                         (COND
                          ((EQUAL (CDAAR F) 1)
                           (LIST
                            (REVAL1
                             (LIST 'QUOTIENT (PREPF (NEGF (CDR F)))
                                   (PREPF (CDAR F)))
                             T)))
                          (T
                           (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Y (CDR (REALROOTS (LIST (PREPF F)))))
                             (COND ((NULL Y) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y) (CADDR Y)) (CAR Y))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Y (CDR Y))
                             (COND ((NULL Y) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (Y) (CADDR Y)) (CAR Y))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))))
                      (CAR F)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ F (CDR F))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL F) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (F)
                        (PROGN
                         (SETQ F (CAR F))
                         (COND
                          ((NEQ (CAAAR F) X) (REDERR "too many variables")))
                         (COND
                          ((EQUAL (CDAAR F) 1)
                           (LIST
                            (REVAL1
                             (LIST 'QUOTIENT (PREPF (NEGF (CDR F)))
                                   (PREPF (CDAR F)))
                             T)))
                          (T
                           (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Y (CDR (REALROOTS (LIST (PREPF F)))))
                             (COND ((NULL Y) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y) (CADDR Y)) (CAR Y))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Y (CDR Y))
                             (COND ((NULL Y) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (Y) (CADDR Y)) (CAR Y))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))))
                      (CAR F)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ F (CDR F))
             (GO LOOPLABEL))))) 
(ENDMODULE) 