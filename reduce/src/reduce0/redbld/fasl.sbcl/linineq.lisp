(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LININEQ)) 
(PUT 'LININEQ 'PSOPFN
     (FUNCTION (LAMBDA (U) (REDERR "USE simplex (package linalg) instead")))) 
(GLOBAL '(*TRLININEQ *TRLININEQINT *PRLININEQ)) 
(SWITCH (LIST 'TRLININEQ 'PRLININEQ 'TRLININEQINT)) 
(FLUID '(LININEQINTERVAL* LININEQRECORD*)) 
(FLUID '(*INEQERR)) 
(PUT 'LININEQEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LININEQEVAL 'DEFINED-ON-LINE '66) 
(PUT 'LININEQEVAL 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LININEQEVAL (U)
    (PROG (PROB EQUA REQUA VARS OLDORDER RES U1 X Y P E MSG DIRECTION REC
           LININEQRECORD* R INTVARS W1 W2 OP)
      (SETQ MSG (OR *PRLININEQ *TRLININEQ))
      (SETQ *INEQERR NIL)
      (SETQ U1 (REVAL1 (CAR U) T))
      (SETQ U1 (COND ((EQUAL (CAR U1) 'LIST) (GETRLIST U1)) (T (LIST U1))))
      (SETQ U (CDR U))
      (COND
       (U
        (PROGN
         (SETQ X (REVAL1 (CAR U) T))
         (SETQ VARS (COND ((EQCAR X 'LIST) (GETRLIST X)) (T (LIST X))))
         (SETQ U (CDR U)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ X (REVAL1 (CAR U) T))
         (SETQ U (CDR U))
         (COND
          ((AND (EQCAR X 'EQUAL)
                (OR (AND (EQUAL (CADR X) 'RECORD) (SETQ REC T))
                    (AND (EQUAL (CADR X) 'INT)
                         (SETQ INTVARS (GETRLIST (CADDR X))))))
           T)
          (T (PROGN (SETQ *INEQERR 2) (TYPERR X "illegal parameter")))))
        (GO WHILELABEL))
      (SETQ X NIL)
      (PROG (U)
        (SETQ U VARS)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (SETQ U (REVAL1 U T))
            (COND
             ((EQCAR U 'EQUAL)
              (COND
               ((MEMBER (CADDR U) '(MIN MAX))
                (PROGN
                 (SETQ DIRECTION (CONS (CONS (CADR U) (CADDR U)) DIRECTION))
                 (SETQ U (CADR U))))
               (T
                (PROGN
                 (SETQ *INEQERR 2)
                 (REDERR "illegal form in 2nd parameter"))))))
            (COND
             ((AND (SMEMBER U U1) (NOT (MEMBER U X))) (SETQ X (CONS U X))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (SETQ X (SETQ VARS (REVERSIP X)))
      (PROG ()
       WHILELABEL
        (COND ((NOT U1) (RETURN NIL)))
        (PROGN
         (SETQ U (REVAL1 (CAR U1) T))
         (SETQ U1 (CDR U1))
         (COND
          ((OR (NOT (PAIRP U)) (NOT (MEMQ (CAR U) '(GEQ LEQ EQUAL))))
           (PROGN (SETQ *INEQERR 2) (TYPERR U "inequality"))))
         (SETQ OP (CAR U))
         (SETQ W1 (REVAL1 (CADR U) T))
         (SETQ W2 (REVAL1 (CADDR U) T))
         (COND
          ((EQUAL OP 'GEQ)
           (COND ((SMEMQ 'INFINITY W2) NIL)
                 ((EQCAR W2 'MAX)
                  (PROG (Q)
                    (SETQ Q (CDR W2))
                   LAB
                    (COND ((NULL Q) (RETURN NIL)))
                    ((LAMBDA (Q) (SETQ U1 (APPEND U1 (LIST (LIST 'GEQ W1 Q)))))
                     (CAR Q))
                    (SETQ Q (CDR Q))
                    (GO LAB)))
                 (T (SETQ PROB (CONS (CONS (SIMP W1) (SIMP W2)) PROB)))))
          ((EQUAL OP 'LEQ)
           (COND ((SMEMQ 'INFINITY W2) NIL)
                 ((EQCAR W2 'MIN)
                  (PROG (Q)
                    (SETQ Q (CDR W2))
                   LAB
                    (COND ((NULL Q) (RETURN NIL)))
                    ((LAMBDA (Q) (SETQ U1 (APPEND U1 (LIST (LIST 'LEQ W1 Q)))))
                     (CAR Q))
                    (SETQ Q (CDR Q))
                    (GO LAB)))
                 (T (SETQ PROB (CONS (CONS (SIMP W2) (SIMP W1)) PROB)))))
          ((EQUAL OP 'EQUAL)
           (COND
            ((EQCAR W2 '*INTERVAL*)
             (SETQ U1
                     (APPEND U1
                             (LIST (LIST 'GEQ W1 (CADR W2))
                                   (LIST 'LEQ W1 (CADDR W2))))))
            (T (SETQ EQUA (CONS (CONS (SIMP W1) (SIMP W2)) EQUA)))))
          (T (PROGN (SETQ *INEQERR 1) (TYPERR U "inequality")))))
        (GO WHILELABEL))
      (PROG (P)
        (SETQ P (APPEND EQUA PROB))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (COND
             ((OR
               (NOT ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CDR (CAR P))))
               (NOT ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CDR (CDR P)))))
              (PROGN
               (SETQ *INEQERR 1)
               (REDERR "unable to process nonlinear system"))))
            (SETQ VARS
                    (LININEQEVALTEST (CAR (CAR P))
                     (LININEQEVALTEST (CAR (CDR P)) VARS)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (MSG (PROGN (PRIN2 "variables:") (PRIN2T VARS))))
      (SETQ OLDORDER (SETKORDER VARS))
      (SETQ PROB
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P PROB)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS
                                     (CONS (REORDER (CAR (CAR P)))
                                           (CDR (CAR P)))
                                     (CONS (REORDER (CAR (CDR P)))
                                           (CDR (CDR P)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (CONS (REORDER (CAR (CAR P))) (CDR (CAR P)))
                                  (CONS (REORDER (CAR (CDR P)))
                                        (CDR (CDR P)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EQUA
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P EQUA)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (CONS
                                     (CONS (REORDER (CAR (CAR P)))
                                           (CDR (CAR P)))
                                     (CONS (REORDER (CAR (CDR P)))
                                           (CDR (CDR P)))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (CONS (CONS (REORDER (CAR (CAR P))) (CDR (CAR P)))
                                  (CONS (REORDER (CAR (CDR P)))
                                        (CDR (CDR P)))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT EQUA) (RETURN NIL)))
        (PROGN
         (SETQ E (CAR EQUA))
         (SETQ EQUA (CDR EQUA))
         (SETQ E (ADDSQ (CAR E) (NEGSQ (CDR E))))
         (COND
          ((OR (ATOM (CAR E)) (ATOM (CAR (CAR E))))
           (PROGN
            (COND
             ((CAR E)
              (PROGN
               (SETQ *INEQERR 0)
               (REDERR "equation part inconsistent"))))))
          (T
           (PROGN
            (SETQ U
                    (LIST
                     (CONS (SETQ X (CAAAR (CAR E)))
                           (PREPSQ
                            (SETQ Y
                                    (MULTSQ (CONS (NEGF (CDR (CAR E))) 1)
                                            (INVSQ
                                             (CONS (CDAR (CAR E)) 1))))))))
            (COND
             ((MEMBER X INTVARS)
              (PROGN
               (SETQ X (SIMP X))
               (SETQ PROB (APPEND (LIST (CONS X Y) (CONS Y X)) PROB))))
             (T
              (PROGN
               (SETQ PROB
                       (PROG (P FORALL-RESULT FORALL-ENDPTR)
                         (SETQ P PROB)
                         (COND ((NULL P) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (P)
                                             (CONS (SUBSQ (CAR P) U)
                                                   (SUBSQ (CDR P) U)))
                                           (CAR P))
                                          NIL)))
                        LOOPLABEL
                         (SETQ P (CDR P))
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (P)
                                     (CONS (SUBSQ (CAR P) U)
                                           (SUBSQ (CDR P) U)))
                                   (CAR P))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ EQUA
                       (PROG (P FORALL-RESULT FORALL-ENDPTR)
                         (SETQ P EQUA)
                         (COND ((NULL P) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (P)
                                             (CONS (SUBSQ (CAR P) U)
                                                   (SUBSQ (CDR P) U)))
                                           (CAR P))
                                          NIL)))
                        LOOPLABEL
                         (SETQ P (CDR P))
                         (COND ((NULL P) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (P)
                                     (CONS (SUBSQ (CAR P) U)
                                           (SUBSQ (CDR P) U)))
                                   (CAR P))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (SETQ REQUA (APPEND U REQUA))
               (COND
                (MSG
                 (PROGN
                  (PRIN2 "         ")
                  (PRIN2 X)
                  (PRIN2 " eliminated by equation")
                  (TERPRI))))
               (SETQ VARS (DELETE X VARS))
               NIL)))))))
        (GO WHILELABEL))
      (SETQ RES
              (COND (INTVARS (LININEQINT PROB VARS MSG DIRECTION REC INTVARS))
                    (T (LININEQ1 PROB VARS MSG DIRECTION REC))))
      (COND ((NULL RES) (RETURN '(LIST))) ((EQUAL RES T) (SETQ RES NIL)))
      (PROG (E)
        (SETQ E REQUA)
       LAB
        (COND ((NULL E) (RETURN NIL)))
        ((LAMBDA (E)
           (PROGN
            (SETQ X (PREPSQ (SUBSQ (SETQ Y (SIMP (CDR E))) RES)))
            (SETQ RES (CONS (CONS (CAR E) X) RES))
            (COND
             (REC
              (PROGN
               (SETQ X (PREPSQ Y))
               (SETQ LININEQRECORD* (CONS (LIST X X) LININEQRECORD*)))))))
         (CAR E))
        (SETQ E (CDR E))
        (GO LAB))
      (SETKORDER OLDORDER)
      (SETQ R
              (COND
               (REC
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P (LIQSIMP-MAXMIN (PAIR RES LININEQRECORD*)))
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P)
                                      (LIST 'LIST
                                            (LIST 'EQUAL (CAAR P) (CDAR P))
                                            (CADR P) (CADDR P)))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P)
                              (LIST 'LIST (LIST 'EQUAL (CAAR P) (CDAR P))
                                    (CADR P) (CADDR P)))
                            (CAR P))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P RES)
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P))) (CAR P))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN (CONS 'LIST R)))) 
(PUT 'LININEQSEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LININEQSEVAL 'DEFINED-ON-LINE '186) 
(PUT 'LININEQSEVAL 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQSEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LININEQSEVAL (U)
    (CONS 'LIST
          (REVERSIP
           (PROG (Q FORALL-RESULT FORALL-ENDPTR)
             (SETQ Q (CDR (LININEQEVAL (APPEND U '((EQUAL RECORD T))))))
             (COND ((NULL Q) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (Q)
                                 (LIST 'EQUAL (CADR (CADR Q))
                                       (COND
                                        ((EQUAL (CADDR Q) (CADDDR Q))
                                         (CADDR Q))
                                        (T (CONS '*INTERVAL* (CDDR Q))))))
                               (CAR Q))
                              NIL)))
            LOOPLABEL
             (SETQ Q (CDR Q))
             (COND ((NULL Q) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (Q)
                         (LIST 'EQUAL (CADR (CADR Q))
                               (COND ((EQUAL (CADDR Q) (CADDDR Q)) (CADDR Q))
                                     (T (CONS '*INTERVAL* (CDDR Q))))))
                       (CAR Q))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'LININEQEVALTEST 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQEVALTEST 'DEFINED-ON-LINE '193) 
(PUT 'LININEQEVALTEST 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVALTEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQEVALTEST (F V)
    (COND ((OR (ATOM F) (ATOM (CAR F))) V)
          ((NOT (EQUAL (CDAAR F) 1))
           (PROGN
            (SETQ *INEQERR 1)
            (REDERR "unable to process nonlinear system")))
          ((MEMBER (CAAAR F) V) (LININEQEVALTEST (CDR F) V))
          (T (LININEQEVALTEST (CDR F) (CONS (CAAAR F) V))))) 
(PUT 'LININEQ0 'NUMBER-OF-ARGS 4) 
(PUT 'LININEQ0 'DEFINED-ON-LINE '203) 
(PUT 'LININEQ0 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQ0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LININEQ0 (PROB VARS DIR REC)
    (PROG (OLDORDER RES)
      (SETQ LININEQRECORD* NIL)
      (SETQ OLDORDER (SETKORDER VARS))
      (SETQ PROB
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U PROB)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (CONS (SIMP U) (CONS NIL 1)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U) (CONS (SIMP U) (CONS NIL 1))) (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RES (LININEQ1 PROB VARS NIL DIR REC))
      (SETKORDER OLDORDER)
      (RETURN RES))) 
(PUT 'LININEQINT 'NUMBER-OF-ARGS 6) 
(PUT 'LININEQINT 'DEFINED-ON-LINE '218) 
(PUT 'LININEQINT 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LININEQINT (PROB VARS MSG DIR REC INTVARS)
    (PROG (X X0 Y Y0 Y1 Z W PROBLEMS BEST Z0 ZBEST ZF BESTR)
      (PROG (X)
        (SETQ X VARS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((MEMBER X INTVARS)
             (PROGN (SETQ W (CONS X W)) (SETQ INTVARS (DELETE X INTVARS))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (INTVARS
        (PROGN
         (SETQ *INEQERR 2)
         (TYPERR (CONS 'LIST INTVARS) "int variables"))))
      (SETQ INTVARS (REVERSIP W))
      (COND
       (DIR
        (PROGN
         (SETQ Z (CAAR DIR))
         (SETQ ZF (COND ((EQUAL (CDAR DIR) 'MAX) 1) (T (MINUS 1)))))))
      (SETQ PROBLEMS (LIST (CONS NIL PROB)))
      (PROG ()
       WHILELABEL
        (COND ((NOT PROBLEMS) (RETURN NIL)))
        (PROGN
         (SETQ Z0 (CAAR PROBLEMS))
         (SETQ PROB (CDAR PROBLEMS))
         (SETQ PROBLEMS (CDR PROBLEMS))
         (COND
          ((OR MSG *TRLININEQINT)
           (LININEQPRINT2 "=== next integer subproblem" PROB)))
         (SETQ W
                 (COND
                  ((AND BEST
                        (NOT
                         (EVALGREATERP (LIST 'TIMES ZF Z0)
                                       (LIST 'TIMES ZF ZBEST))))
                   NIL)
                  (T (LININEQ1 PROB VARS MSG DIR REC))))
         (COND (*TRLININEQINT (LININEQPRINT3 "=== subresult" W)))
         (COND
          ((AND W DIR)
           (PROGN
            (SETQ Z0 (CDR (ASSOC Z W)))
            (COND
             ((AND BEST
                   (EVALGREATERP (LIST 'TIMES ZF ZBEST) (LIST 'TIMES ZF Z0)))
              (SETQ W NIL))))))
         (COND
          (W
           (PROGN
            (SETQ Y (LIST PROB))
            (PROG (X)
              (SETQ X INTVARS)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (PROGN
                  (SETQ X0 (CDR (ASSOC X W)))
                  (COND
                   ((NOT (FIXP X0))
                    (PROGN
                     (SETQ X (SIMP X))
                     (SETQ Y0 (SIMP (LIST 'CEILING X0)))
                     (SETQ Y1 (SIMP (LIST 'FLOOR X0)))
                     (SETQ Y
                             (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                               (SETQ Q Y)
                              STARTOVER
                               (COND ((NULL Q) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (Q)
                                          (LIST (CONS (CONS X Y0) Q)
                                                (CONS (CONS Y1 X) Q)))
                                        (CAR Q)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ Q (CDR Q))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL Q) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (Q)
                                          (LIST (CONS (CONS X Y0) Q)
                                                (CONS (CONS Y1 X) Q)))
                                        (CAR Q)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ Q (CDR Q))
                               (GO LOOPLABEL)))
                     (COND
                      ((OR MSG *TRLININEQINT)
                       (PROGN
                        (WRITEPRI "branch and bound with" 'FIRST)
                        (WRITEPRI
                         (MKQUOTE
                          (LIST 'LIST
                                (LIST 'GEQ (SETQ X (PREPSQ X)) (PREPSQ Y0))
                                (LIST 'LEQ X (PREPSQ Y1))))
                         'LAST)))))))))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (COND
             ((CDR Y)
              (SETQ PROBLEMS
                      (APPEND PROBLEMS
                              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                (SETQ Q Y)
                                (COND ((NULL Q) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q) (CONS Z0 Q))
                                                  (CAR Q))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ Q (CDR Q))
                                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Q) (CONS Z0 Q)) (CAR Q))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))))
             (T
              (PROGN
               (SETQ ZBEST Z0)
               (SETQ BEST W)
               (SETQ BESTR LININEQRECORD*)
               (COND (*TRLININEQINT (PRIN2T "===>  is feasable")))))))))
         (COND ((AND BEST (NULL DIR)) (SETQ PROBLEMS NIL))))
        (GO WHILELABEL))
      (SETQ LININEQRECORD* BESTR)
      (RETURN BEST))) 
(PUT 'LININEQ1 'NUMBER-OF-ARGS 5) 
(PUT 'LININEQ1 'DEFINED-ON-LINE '267) 
(PUT 'LININEQ1 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQ1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LININEQ1 (PROB VARS MSG DIR REC)
    (PROG (V VQ LH RH X Y Z PROB1 PROB2 PROB3 PROB4 NPROB SW SOL)
      (COND ((NULL VARS) (RETURN (LININEQ2 PROB MSG))))
      (SETQ V (CAR VARS))
      (SETQ VARS (CDR VARS))
      (SETQ VQ (MKSQ V 1))
      (COND
       (*TRLININEQ
        (LININEQPRINT2 (LIST "next variable:" V "; initial system:") PROB)))
      (SETQ PROB (LININEQNORMALIZE PROB))
      (PROG (P)
        (SETQ P PROB)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ LH (CAR P))
            (SETQ RH (CDR P))
            (COND
             ((AND (NOT (OR (ATOM (CAR LH)) (ATOM (CAR (CAR LH)))))
                   (EQUAL (CAAAR (CAR LH)) V))
              (PROGN
               (SETQ X (INVSQ (CONS (CDAR (CAR LH)) 1)))
               (SETQ SW (LESSP (CAR X) 0))
               (SETQ LH (MULTSQ LH X))
               (SETQ RH (MULTSQ RH X))
               (SETQ RH (ADDSQ RH (CONS (NEGF (CDR (CAR LH))) (CDR LH))))
               (COND ((NOT SW) (SETQ PROB1 (CONS (CONS VQ RH) PROB1)))
                     (T (SETQ PROB2 (CONS (CONS RH VQ) PROB2))))
               NIL))
             ((AND (OR (ATOM (CAR RH)) (ATOM (CAR (CAR RH))))
                   (OR (ATOM (CAR LH)) (ATOM (CAR (CAR LH)))))
              (SETQ PROB4 (CONS (CONS LH RH) PROB4)))
             (T (SETQ PROB3 (CONS (CONS LH RH) PROB3))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       ((AND (NULL PROB1) (NULL PROB2) VARS)
        (PROGN
         (SETQ SOL (LININEQ1 PROB VARS MSG DIR REC))
         (COND
          (REC
           (SETQ LININEQRECORD*
                   (APPEND LININEQRECORD* '(((MINUS INFINITY) INFINITY))))))
         (RETURN (COND (SOL (CONS (CONS V 0) SOL)) (T NIL))))))
      (COND
       (*TRLININEQ
        (PROGN
         (LININEQPRINT2 "class 1:" PROB1)
         (LININEQPRINT2 "class 2:" PROB2)
         (LININEQPRINT2 "class 3:" PROB3)
         (LININEQPRINT2 "class 4:" PROB4))))
      (COND
       (REC
        (PROGN
         (SETQ X
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U PROB1)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (PREPSQ (CDR U))) (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (U) (PREPSQ (CDR U))) (CAR U)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ Y
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U PROB2)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (PREPSQ (CAR U))) (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (U) (PREPSQ (CAR U))) (CAR U)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ X
                 (COND ((NULL X) '(MINUS INFINITY)) ((NULL (CDR X)) (CAR X))
                       (T (CONS 'MAX X))))
         (SETQ Y
                 (COND ((NULL Y) 'INFINITY) ((NULL (CDR Y)) (CAR Y))
                       (T (CONS 'MIN Y))))
         (SETQ LININEQRECORD* (APPEND LININEQRECORD* (LIST (LIST X Y)))))))
      (COND ((NOT (LININEQ2 PROB4 MSG)) (RETURN NIL)))
      (SETQ NPROB
              (APPEND PROB3
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X PROB1)
                       STARTOVER
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (X)
                                   (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ Y PROB2)
                                     (COND ((NULL Y) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (Y)
                                                         (CONS (CAR Y)
                                                               (CDR X)))
                                                       (CAR Y))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ Y (CDR Y))
                                     (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (CONS (CAR Y) (CDR X)))
                                               (CAR Y))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR X)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ X (CDR X))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (X)
                                   (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ Y PROB2)
                                     (COND ((NULL Y) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (Y)
                                                         (CONS (CAR Y)
                                                               (CDR X)))
                                                       (CAR Y))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ Y (CDR Y))
                                     (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Y)
                                                 (CONS (CAR Y) (CDR X)))
                                               (CAR Y))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR X)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ X (CDR X))
                        (GO LOOPLABEL))))
      (COND
       (VARS
        (PROGN
         (COND
          ((NULL (SETQ SOL (LININEQ1 NPROB VARS MSG DIR REC))) (RETURN NIL)))))
       ((NOT (LININEQ2 NPROB MSG)) (RETURN NIL)))
      (SETQ X
              (COND ((NULL PROB1) NIL)
                    (T
                     (LININEQEVALMAX
                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                        (SETQ P PROB1)
                        (COND ((NULL P) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (P) (SUBSQ (CDR P) SOL))
                                          (CAR P))
                                         NIL)))
                       LOOPLABEL
                        (SETQ P (CDR P))
                        (COND ((NULL P) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (SUBSQ (CDR P) SOL)) (CAR P))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))
      (SETQ Y
              (COND ((NULL PROB2) NIL)
                    (T
                     (LININEQEVALMIN
                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                        (SETQ P PROB2)
                        (COND ((NULL P) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (P) (SUBSQ (CAR P) SOL))
                                          (CAR P))
                                         NIL)))
                       LOOPLABEL
                        (SETQ P (CDR P))
                        (COND ((NULL P) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (SUBSQ (CAR P) SOL)) (CAR P))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))
      (COND ((SETQ Z (ASSOC V DIR)) (SETQ Z (CDR Z))))
      (COND
       (MSG
        (PROGN
         (WRITEPRI "         " 'FIRST)
         (WRITEPRI (MKQUOTE (COND (X (PREPSQ X)) (T '(MINUS INFINITY)))) NIL)
         (WRITEPRI " <= " NIL)
         (WRITEPRI (MKQUOTE V) NIL)
         (WRITEPRI " <= " NIL)
         (WRITEPRI (MKQUOTE (COND (Y (PREPSQ Y)) (T 'INFINITY))) NIL)
         (WRITEPRI ";   " NIL))))
      (SETQ LININEQINTERVAL* (CONS X Y))
      (COND
       ((OR (AND (EQUAL Z 'MIN) (NULL X)) (AND (EQUAL Z 'MAX) (NULL Y)))
        (PROGN
         (COND (MSG (WRITEPRI " max/min cannot be resolved" 'LAST)))
         (RETURN NIL))))
      (COND
       ((NOT (EQUAL X Y))
        (COND ((EQUAL Z 'MIN) (SETQ Y NIL)) ((EQUAL Z 'MAX) (SETQ X NIL)))))
      (COND
       (MSG
        (PROGN
         (WRITEPRI
          (COND ((AND (NULL X) (NULL Y)) " completely free: ")
                ((NULL Y) " minimum: ") ((NULL X) " maximum: ")
                ((EQUAL X Y) " zero length interval: ") (T " middle: "))
          NIL))))
      (COND ((AND (NULL X) (NULL Y)) (SETQ X 0)) ((NULL X) (SETQ X (PREPSQ Y)))
            ((NULL Y) (SETQ X (PREPSQ X)))
            ((SQLESSP Y X)
             (PROGN
              (PRIN2 "system inconsistent:")
              (PRIN2 (PREPSQ X))
              (PRIN2 " not <= ")
              (PRIN2T (PREPSQ Y))
              (RETURN NIL)))
            (T (SETQ X (LIST 'QUOTIENT (LIST 'PLUS (PREPSQ X) (PREPSQ Y)) 2))))
      (SETQ X (REVAL1 X NIL))
      (COND (MSG (WRITEPRI (MKQUOTE (LIST 'EQUAL V X)) 'LAST)))
      (RETURN (CONS (CONS V X) SOL)))) 
(PUT 'LININEQ2 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQ2 'DEFINED-ON-LINE '370) 
(PUT 'LININEQ2 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQ2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQ2 (PROB MSG)
    (PROG (RH LH)
     LOOP
      (COND ((NULL PROB) (RETURN T)))
      (SETQ LH (CAAR PROB))
      (SETQ RH (CDAR PROB))
      (COND
       ((OR (NOT (OR (ATOM (CAR RH)) (ATOM (CAR (CAR RH)))))
            (NOT (OR (ATOM (CAR LH)) (ATOM (CAR (CAR LH))))))
        (PROGN (SETQ *INEQERR 1) (REDERR (LIST " non numeric:" RH LH)))))
      (COND
       ((SQLESSP LH RH)
        (PROGN
         (COND
          (MSG
           (PROGN
            (WRITEPRI "system inconsistent: " 'FIRST)
            (WRITEPRI (MKQUOTE (PREPSQ LH)) NIL)
            (WRITEPRI " not >= " NIL)
            (WRITEPRI (MKQUOTE (PREPSQ RH)) 'LAST))))
         (RETURN NIL))))
      (SETQ PROB (CDR PROB))
      (GO LOOP))) 
(PUT 'LININEQNORMALIZE 'NUMBER-OF-ARGS 1) 
(PUT 'LININEQNORMALIZE 'DEFINED-ON-LINE '388) 
(PUT 'LININEQNORMALIZE 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQNORMALIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LININEQNORMALIZE (PROB)
    (PROG (R LH RH D AB X)
      (PROG (P)
        (SETQ P PROB)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ LH (CAR P))
            (SETQ RH (CDR P))
            (SETQ LH (ADDSQ LH (NEGSQ RH)))
            (SETQ D (CDR LH))
            (SETQ LH (CAR LH))
            (SETQ AB LH)
            (SETQ X (COND ((OR (ATOM LH) (ATOM (CAR LH))) 1) (T (CDAR AB))))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NOT (OR (ATOM AB) (ATOM (CAR AB))))) (RETURN NIL)))
              (PROGN (SETQ X (GCDF X (CDAR AB))) (SETQ AB (CDR AB)))
              (GO WHILELABEL))
            (SETQ AB (NEGF AB))
            (SETQ LH (MULTSQ (CONS (ADDF LH AB) 1) (CONS 1 X)))
            (SETQ RH (MULTSQ (CONS AB 1) (CONS 1 X)))
            (SETQ X (ASSOC LH R))
            (COND ((NULL X) (SETQ R (CONS (CONS LH RH) R)))
                  ((SQLESSP (CDR X) RH) (RPLACD X RH)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (*TRLININEQ (LININEQPRINT2 "normalized and reduced:" R)))
      (RETURN R))) 
(PUT 'LININEQEVALMIN 'NUMBER-OF-ARGS 1) 
(PUT 'LININEQEVALMIN 'DEFINED-ON-LINE '415) 
(PUT 'LININEQEVALMIN 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVALMIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LININEQEVALMIN (U) (LININEQEVALMIN1 (CAR U) (CDR U))) 
(PUT 'LININEQEVALMIN1 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQEVALMIN1 'DEFINED-ON-LINE '419) 
(PUT 'LININEQEVALMIN1 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVALMIN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQEVALMIN1 (Q U)
    (COND ((NULL U) Q)
          (T
           ((LAMBDA (X)
              (LININEQEVALMIN1 (COND ((AND X (|:MINUSP| X)) Q) (T (CAR U)))
               (CDR U)))
            (CAR (ADDSQ Q (NEGSQ (CAR U)))))))) 
(PUT 'LININEQEVALMAX 'NUMBER-OF-ARGS 1) 
(PUT 'LININEQEVALMAX 'DEFINED-ON-LINE '424) 
(PUT 'LININEQEVALMAX 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVALMAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LININEQEVALMAX (U) (LININEQEVALMAX1 (CAR U) (CDR U))) 
(PUT 'LININEQEVALMAX1 'NUMBER-OF-ARGS 2) 
(PUT 'LININEQEVALMAX1 'DEFINED-ON-LINE '428) 
(PUT 'LININEQEVALMAX1 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LININEQEVALMAX1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LININEQEVALMAX1 (Q U)
    (COND ((NULL U) Q)
          (T
           ((LAMBDA (X)
              (LININEQEVALMAX1 (COND ((AND X (|:MINUSP| X)) (CAR U)) (T Q))
               (CDR U)))
            (CAR (ADDSQ Q (NEGSQ (CAR U)))))))) 
(PUT 'SQLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'SQLESSP 'DEFINED-ON-LINE '434) 
(PUT 'SQLESSP 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'SQLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SQLESSP (Q1 Q2)
    ((LAMBDA (X) (AND X (|:MINUSP| X))) (CAR (ADDSQ Q1 (NEGSQ Q2))))) 
(PUT 'LIQSIMP-MAXMIN 'NUMBER-OF-ARGS 1) 
(PUT 'LIQSIMP-MAXMIN 'DEFINED-ON-LINE '437) 
(PUT 'LIQSIMP-MAXMIN 'DEFINED-IN-FILE 'SOLVE/LININEQ.RED) 
(PUT 'LIQSIMP-MAXMIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIQSIMP-MAXMIN (W) (LIQSIMP2-MAXMIN (LIQSIMP1-MAXMIN W))) 
(ENDMODULE) 