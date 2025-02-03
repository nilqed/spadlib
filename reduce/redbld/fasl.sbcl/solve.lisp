(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SOLVE)) 
(CREATE-PACKAGE
 '(SOLVE SOLVE1 PPSOLN SOLVELNR GLSOLVE SOLVEALG SOLVETAB QUARTIC) NIL) 
(LOAD-PACKAGE 'MATRIX) 
(FLUID '(*EXP *EZGCD *MULTIPLICITIES !GCD DMODE* VARS*)) 
(FLUID '(INSIDE-SOLVEEVAL SOLVE-GENSYMCOUNTER SOLVE-GENSYMPREFIX)) 
(SETQ SOLVE-GENSYMPREFIX (EXPLODE '!SOLVEVAR)) 
(GLOBAL '(MULTIPLICITIES* ASSUMPTIONS REQUIREMENTS)) 
(FLAG '(MULTIPLICITIES* ASSUMPTIONS REQUIREMENTS) 'SHARE) 
(AEVAL (OPERATOR (LIST 'ONE_OF))) 
(PUT 'ARBINT 'SIMPFN 'SIMPIDEN) 
(PUT 'ARBINT 'CMPXSPLITFN 'SIMP) 
(FLAG '(EXPAND_CASES) 'OPFN) 
(PUT 'SIMP-ARBCOMPLEX 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-ARBCOMPLEX 'DEFINED-ON-LINE '68) 
(PUT 'SIMP-ARBCOMPLEX 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'SIMP-ARBCOMPLEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-ARBCOMPLEX (U) ((LAMBDA (DMODE*) (SIMPIDEN (CONS 'ARBCOMPLEX U))) NIL)) 
(DEFLIST '((ARBCOMPLEX SIMP-ARBCOMPLEX)) 'SIMPFN) 
(PUT 'FREEOFL 'NUMBER-OF-ARGS 2) 
(PUT 'FREEOFL 'DEFINED-ON-LINE '76) 
(PUT 'FREEOFL 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'FREEOFL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FREEOFL (U V) (OR (NULL V) (AND (FREEOF U (CAR V)) (FREEOFL U (CDR V))))) 
(PUT 'ALLKERN 'NUMBER-OF-ARGS 1) 
(PUT 'ALLKERN 'DEFINED-ON-LINE '79) 
(PUT 'ALLKERN 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'ALLKERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLKERN (ELST)
    (COND ((NULL ELST) NIL)
          (T (UNION (KERNELS (CAR (CAR ELST))) (ALLKERN (CDR ELST)))))) 
(PUT 'TOPKERN 'NUMBER-OF-ARGS 2) 
(PUT 'TOPKERN 'DEFINED-ON-LINE '85) 
(PUT 'TOPKERN 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'TOPKERN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOPKERN (U X)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J (KERNELS U))
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J) (COND ((NOT (FREEOF J X)) (LIST J)) (T NIL)))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J) (COND ((NOT (FREEOF J X)) (LIST J)) (T NIL)))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'COEFLIS 'NUMBER-OF-ARGS 1) 
(PUT 'COEFLIS 'DEFINED-ON-LINE '90) 
(PUT 'COEFLIS 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'COEFLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COEFLIS (EX)
    (PROG (ANS VAR)
      (COND ((OR (ATOM EX) (ATOM (CAR EX))) (RETURN (CONS 0 EX))))
      (SETQ VAR (CAAAR EX))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (ATOM EX) (ATOM (CAR EX)))) (EQUAL (CAAAR EX) VAR)))
          (RETURN NIL)))
        (PROGN
         (SETQ ANS (CONS (CONS (CDAAR EX) (REORDER (CDAR EX))) ANS))
         (SETQ EX (CDR EX)))
        (GO WHILELABEL))
      (COND (EX (SETQ ANS (CONS (CONS 0 (REORDER EX)) ANS))))
      (RETURN ANS))) 
(FLUID '(SOLVEMETHODS*)) 
(PUT 'SOLVE 'PSOPFN 'SOLVEEVAL) 
(PUT 'SOLVEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEEVAL 'DEFINED-ON-LINE '134) 
(PUT 'SOLVEEVAL 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'SOLVEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEEVAL (U)
    (PROG (W R M)
      (SETQ W
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q U)
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
                (GO LOOPLABEL)))
      (SETQ M SOLVEMETHODS*)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NULL R) M)) (RETURN NIL)))
        (PROGN (SETQ R (APPLY1 (CAR M) W)) (SETQ M (CDR M)))
        (GO WHILELABEL))
      (RETURN
       (COND ((NULL R) (SOLVEEVAL1 W)) ((EQCAR R 'FAILED) (CONS 'SOLVE U))
             (T R))))) 
(PUT 'ODESOLVE* 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE* 'DEFINED-ON-LINE '150) 
(PUT 'ODESOLVE* 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'ODESOLVE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE* (U)
    (AND (NEQ (LENGTH U) 2) (SMEMQ 'DF U)
         (PROGN
          (LOAD-PACKAGE 'ODESOLVE)
          (COND ((AND (FLAGP 'ODESOLVE 'OPFN) (NEQ (LENGTH U) 3)) '(FAILED))
                (T (REVAL1 (CONS 'ODESOLVE U) NIL)))))) 
(SETQ SOLVEMETHODS* (UNION '(ODESOLVE*) SOLVEMETHODS*)) 
(PUT 'SOLVEEVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEEVAL1 'DEFINED-ON-LINE '161) 
(PUT 'SOLVEEVAL1 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'SOLVEEVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEEVAL1 (U)
    (PROG (*EZGCD !GCD VARS* NARGS)
      (SETQ NARGS 0)
      (COND ((ATOM U) (RERROR 'SOLVE 1 "SOLVE called with no equations"))
            ((NULL DMODE*) (SETQ *EZGCD T)))
      (SETQ NARGS (LENGTH U))
      (COND
       ((NOT INSIDE-SOLVEEVAL)
        (PROGN
         (SETQ SOLVE-GENSYMCOUNTER 1)
         (SETQ ASSUMPTIONS
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (SETQ REQUIREMENTS
                          (PROGN
                           (SETQ ALGLIST* (CONS NIL NIL))
                           (LIST 'LIST))))))))
      ((LAMBDA (INSIDE-SOLVEEVAL *RESIMP)
         (SETQ U
                 (COND ((EQUAL NARGS 1) (SOLVE0 (CAR U) NIL))
                       ((EQUAL NARGS 2) (SOLVE0 (CAR U) (CADR U)))
                       (T
                        (PROGN
                         (LPRIM "Please put SOLVE unknowns in a list")
                         (SOLVE0 (CAR U) (CONS 'LIST (CDR U))))))))
       T (NOT *EXP))
      (COND
       ((NOT INSIDE-SOLVEEVAL)
        (PROGN
         (SETQ ASSUMPTIONS
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (SOLVE-CLEAN-INFO ASSUMPTIONS T)))
         (SETQ REQUIREMENTS
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (SOLVE-CLEAN-INFO REQUIREMENTS NIL))))))
      (RETURN (*SOLVELIST2SOLVEEQLIST U)))) 
(PUT '*SOLVELIST2SOLVEEQLIST 'NUMBER-OF-ARGS 1) 
(PUT '*SOLVELIST2SOLVEEQLIST 'DEFINED-ON-LINE '180) 
(PUT '*SOLVELIST2SOLVEEQLIST 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT '*SOLVELIST2SOLVEEQLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *SOLVELIST2SOLVEEQLIST (U)
    (PROG (X Y Z)
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SOLVEORDER J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SOLVEORDER J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (J)
        (SETQ J U)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (COND ((EQUAL (CADDR J) 0) (RERROR 'SOLVE 2 "zero multiplicity"))
                  ((NULL (CADR J))
                   (SETQ X
                           (PROG (K FORALL-RESULT FORALL-ENDPTR)
                             (SETQ K (CAR J))
                             (COND ((NULL K) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (K)
                                                 (LIST 'EQUAL (*Q2A1 K *NOSQ)
                                                       0))
                                               (CAR K))
                                              NIL)))
                            LOOPLABEL
                             (SETQ K (CDR K))
                             (COND ((NULL K) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (K)
                                         (LIST 'EQUAL (*Q2A1 K *NOSQ) 0))
                                       (CAR K))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                  (T
                   (SETQ X
                           (PROG (K FORALL-RESULT FORALL-ENDPTR)
                             (SETQ K (PAIR (CADR J) (CAR J)))
                             (COND ((NULL K) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (K)
                                                 (LIST 'EQUAL (CAR K)
                                                       (*Q2A1 (CDR K) *NOSQ)))
                                               (CAR K))
                                              NIL)))
                            LOOPLABEL
                             (SETQ K (CDR K))
                             (COND ((NULL K) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (K)
                                         (LIST 'EQUAL (CAR K)
                                               (*Q2A1 (CDR K) *NOSQ)))
                                       (CAR K))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))))
            (COND ((GREATERP (LENGTH VARS*) 1) (SETQ X (CONS 'LIST X)))
                  (T (SETQ X (CAR X))))
            (SETQ Z (CONS (CONS (CADDR J) X) Z))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ Z (SORT Z (FUNCTION ORDP)))
      (SETQ X NIL)
      (COND
       (*MULTIPLICITIES
        (PROGN
         (PROG (K)
           (SETQ K Z)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K)
              (PROG (I)
                (SETQ I 1)
               LAB
                (COND ((MINUSP (DIFFERENCE (CAR K) I)) (RETURN NIL)))
                (SETQ X (CONS (CDR K) X))
                (SETQ I (PLUS2 I 1))
                (GO LAB)))
            (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (SETQ MULTIPLICITIES* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) NIL))))
       (T
        (PROGN
         (PROG (K)
           (SETQ K Z)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K)
              (PROGN (SETQ X (CONS (CDR K) X)) (SETQ Y (CONS (CAR K) Y))))
            (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (SETQ MULTIPLICITIES*
                 (PROGN
                  (SETQ ALGLIST* (CONS NIL NIL))
                  (CONS 'LIST (REVERSIP Y)))))))
      (RETURN (CONS 'LIST (REVERSIP X))))) 
(PUT 'SOLVEORDER 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEORDER 'DEFINED-ON-LINE '204) 
(PUT 'SOLVEORDER 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'SOLVEORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEORDER (U)
    (PROG (V W X Y Z)
      (SETQ V VARS*)
      (SETQ X (CADR U))
      (COND ((LESSP (LENGTH X) (LENGTH V)) (SETQ V (SETDIFF V (SETDIFF V X)))))
      (COND ((OR (NULL X) (EQUAL X V)) (RETURN U)))
      (SETQ Y (CAR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT X) (RETURN NIL)))
        (PROGN
         (SETQ Z (CONS (CONS (CAR X) (CAR Y)) Z))
         (SETQ X (CDR X))
         (SETQ Y (CDR Y)))
        (GO WHILELABEL))
      (SETQ W V)
     A
      (COND ((NULL W) (RETURN (CONS (REVERSIP X) (CONS V (CDDR U)))))
            ((NULL (SETQ Y (DEPASSOC (CAR W) Z))) (RETURN U))
            (T (SETQ X (CONS (CDR Y) X))))
      (SETQ W (CDR W))
      (GO A))) 
(PUT 'DEPASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'DEPASSOC 'DEFINED-ON-LINE '222) 
(PUT 'DEPASSOC 'DEFINED-IN-FILE 'SOLVE/SOLVE.RED) 
(PUT 'DEPASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEPASSOC (U V)
    (COND ((NULL V) NIL) ((EQUAL U (CAAR V)) (CAR V))
          ((DEPENDS (CAAR V) U) NIL) (T (DEPASSOC U (CDR V))))) 
(ENDMODULE) 