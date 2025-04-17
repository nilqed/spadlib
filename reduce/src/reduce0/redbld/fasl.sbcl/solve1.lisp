(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SOLVE1)) 
(FLUID
 '(*ALLBRANCH *ARBVARS *EXP *EZGCD *FULLROOTS *LIMITEDFACTORS *MULTIPLICITIES
   *NOTSEPARATE *NUMVAL *NUMVAL* *PRECISE *ROUNDED *SOLVEALGP *SOLVESINGULAR
   *VAROPT !GCD |:PREC:| ASYMPLIS* ALGLIST* DMODE* KORD* VARS*
   **NOROOTVARRENAMEP**)) 
(GLOBAL '(!ARBINT MULTIPLICITIES* ASSUMPTIONS REQUIREMENTS)) 
(SWITCH
 (LIST (LIST 'EQUAL 'ALLBRANCH 'ON) (LIST 'EQUAL 'ARBVARS 'ON) 'FULLROOTS
       'MULTIPLICITIES (LIST 'EQUAL 'SOLVESINGULAR 'ON))) 
(PUT 'FULLROOTS 'SIMPFG '((T (RMSUBS)))) 
(FLAG '(*ALLBRANCH MULTIPLICITIES* ASSUMPTIONS REQUIREMENTS) 'SHARE) 
(PUT 'SOLVE0 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE0 'DEFINED-ON-LINE '67) 
(PUT 'SOLVE0 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE0 (ELST XLST)
    (PROG (*EXP *NOTSEPARATE W NEQN)
      (SETQ NEQN 0)
      (SETQ *EXP T)
      (SETQ ELST
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (SOLVEARGCHK ELST))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* (*EQN2A J))) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* (*EQN2A J))) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ NEQN (LENGTH ELST))
      (COND
       ((NULL XLST)
        (PROGN
         (SETQ VARS* (SOLVEVARS ELST))
         (TERPRI)
         (COND ((NULL VARS*) NIL)
               ((CDR VARS*)
                (PROGN (PRIN2* "Unknowns: ") (MAPRIN (CONS 'LIST VARS*))))
               (T (PROGN (PRIN2* "Unknown: ") (MAPRIN (CAR VARS*)))))
         (TERPRI* NIL)))
       (T
        (PROGN
         (SETQ XLST (SOLVEARGCHK XLST))
         (SETQ VARS*
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J XLST)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (COND
       ((EQUAL (LENGTH VARS*) 0)
        (RERROR 'SOLVE 3 "SOLVE called with no variables")))
      (COND
       ((AND (EQUAL NEQN 1) (EQUAL (LENGTH VARS*) 1))
        (COND
         ((NULL (CAR (CAR ELST)))
          (RETURN
           (COND
            (*SOLVESINGULAR
             (LIST (LIST (LIST (CONS (MAKEARBCOMPLEX) 1)) VARS* 1)))
            (T NIL))))
         ((OR (SOLUTIONP (SETQ W (SOLVESQ (CAR ELST) (CAR VARS*) 1)))
              (NULL *SOLVEALGP) (UNIVARIATEP (CAR (CAR ELST))))
          (RETURN W)))))
      (SETQ ELST
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J ELST)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (SOLVESYS ELST VARS*))
      (COND ((NULL W) (RETURN NIL)))
      (COND ((MEMQ (CAR W) (LIST 'T 'INCONSISTENT 'SINGULAR)) (RETURN (CDR W)))
            ((OR (EQ (CAR W) 'FAILED) (NULL (CAR W)))
             (RETURN
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J ELST)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (LIST (LIST (CONS J 1)) NIL 1))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (LIST (LIST (CONS J 1)) NIL 1)) (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))
            (T (ERRACH (LIST "Improper solve solution tag" (CAR W))))))) 
(PUT 'BASIC-KERN 'NUMBER-OF-ARGS 1) 
(PUT 'BASIC-KERN 'DEFINED-ON-LINE '113) 
(PUT 'BASIC-KERN 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'BASIC-KERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BASIC-KERN (U)
    ((LAMBDA (W)
       (PROGN
        (PROG (K)
          (SETQ K U)
         LAB
          (COND ((NULL K) (RETURN NIL)))
          ((LAMBDA (K) (SETQ W (UNION (BASIC-KERN1 K) W))) (CAR K))
          (SETQ K (CDR K))
          (GO LAB))
        W))
     NIL)) 
(PUT 'BASIC-KERN1 'NUMBER-OF-ARGS 1) 
(PUT 'BASIC-KERN1 'DEFINED-ON-LINE '116) 
(PUT 'BASIC-KERN1 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'BASIC-KERN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BASIC-KERN1 (U)
    (PROG (W)
      (COND ((ATOM U) (RETURN (LIST U)))
            ((AND (ALGEBRAIC-FUNCTION (CAR U))
                  (SETQ W
                          (ALLBKERN
                           (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                             (SETQ Q (CDR U))
                             (COND ((NULL Q) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (Q) (SIMP Q)) (CAR Q))
                                              NIL)))
                            LOOPLABEL
                             (SETQ Q (CDR Q))
                             (COND ((NULL Q) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (Q) (SIMP Q)) (CAR Q))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))))
             (RETURN W))
            (T (RETURN (LIST U)))))) 
(PUT 'ALGEBRAIC-FUNCTION 'NUMBER-OF-ARGS 1) 
(PUT 'ALGEBRAIC-FUNCTION 'DEFINED-ON-LINE '126) 
(PUT 'ALGEBRAIC-FUNCTION 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'ALGEBRAIC-FUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGEBRAIC-FUNCTION (Q)
    (OR (FLAGP Q 'REALVALUED) (FLAGP Q 'ALWAYSREALVALUED) (GET Q '|:RD:|)
        (GET Q '|:CR:|) (GET Q 'OPMTCH))) 
(PUT 'ALLBKERN 'NUMBER-OF-ARGS 1) 
(PUT 'ALLBKERN 'DEFINED-ON-LINE '135) 
(PUT 'ALLBKERN 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'ALLBKERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLBKERN (ELST)
    (COND ((NULL ELST) NIL)
          (T
           (UNION (BASIC-KERN (KERNELS (CAR (CAR ELST))))
                  (ALLBKERN (CDR ELST)))))) 
(PUT 'SOLVEVARS 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEVARS 'DEFINED-ON-LINE '140) 
(PUT 'SOLVEVARS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEVARS (ELST)
    ((LAMBDA (S)
       (PROGN
        (PROG (J)
          (SETQ J (ALLBKERN ELST))
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J) (COND ((NOT (CONSTANT_EXPRP J)) (SETQ S (ORDAD J S)))))
           (CAR J))
          (SETQ J (CDR J))
          (GO LAB))
        S))
     NIL)) 
(PUT 'SOLUTIONP 'NUMBER-OF-ARGS 1) 
(PUT 'SOLUTIONP 'DEFINED-ON-LINE '145) 
(PUT 'SOLUTIONP 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLUTIONP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLUTIONP (U)
    (OR (NULL U)
        (AND (CADAR U) (NOT (ROOT_OF_SOLN_P (CAAR U))) (SOLUTIONP (CDR U))))) 
(PUT 'ROOT_OF_SOLN_P 'NUMBER-OF-ARGS 1) 
(PUT 'ROOT_OF_SOLN_P 'DEFINED-ON-LINE '148) 
(PUT 'ROOT_OF_SOLN_P 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'ROOT_OF_SOLN_P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOT_OF_SOLN_P (U)
    (AND (NULL (CDR U)) (KERNP (SETQ U (CAR U)))
         (EQCAR (CAAAR (CAR U)) 'ROOT_OF))) 
(PUT 'SOLVEARGCHK 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEARGCHK 'DEFINED-ON-LINE '151) 
(PUT 'SOLVEARGCHK 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEARGCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEARGCHK (U)
    (COND ((EQ (GETRTYPE (SETQ U (REVAL1 U T))) 'LIST) (CDR (REVAL1 U T)))
          ((OR (ATOM U) (NOT (EQ (CAR U) 'LST))) (LIST U)) (T (CDR U)))) 
(PUT 'SOLVE-CLEAN-INFO 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-CLEAN-INFO 'DEFINED-ON-LINE '159) 
(PUT 'SOLVE-CLEAN-INFO 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE-CLEAN-INFO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-CLEAN-INFO (FL FLG)
    (PROG (R W P)
      (PROG (FORM)
        (SETQ FORM (CDR FL))
       LAB
        (COND ((NULL FORM) (RETURN NIL)))
        ((LAMBDA (FORM)
           (COND
            ((NOT P)
             (COND
              ((CONSTANT_EXPRP (SETQ FORM (REVAL1 FORM T)))
               (COND ((NOT FLG) (SETQ P (SETQ R (LIST 1))))))
              (FLG
               (PROG (W)
                 (SETQ W (CDR (FCTRF (CAR (SIMP FORM)))))
                LAB
                 (COND ((NULL W) (RETURN NIL)))
                 ((LAMBDA (W)
                    (PROGN
                     (SETQ W (ABSF (CAR W)))
                     (COND ((NOT (MEMBER W R)) (SETQ R (CONS W R))))))
                  (CAR W))
                 (SETQ W (CDR W))
                 (GO LAB)))
              (T
               (PROGN
                (SETQ W (ABSF (CAR (SIMP (LIST 'NPRIMITIVE FORM)))))
                (COND
                 ((NOT (OR (ATOM W) (ATOM (CAR W))))
                  (SETQ W (CAAR (SQFRF W)))))
                (PROG (Z)
                  (SETQ Z R)
                 LAB
                  (COND ((NULL Z) (RETURN NIL)))
                  ((LAMBDA (Z)
                     (COND
                      (W
                       (COND ((NULL (CDR (QREMF Z W))) (SETQ R (DELETE Z R)))
                             ((NULL (CDR (QREMF W Z))) (SETQ W NIL))))))
                   (CAR Z))
                  (SETQ Z (CDR Z))
                  (GO LAB))
                (COND (W (SETQ R (CONS W R))))))))))
         (CAR FORM))
        (SETQ FORM (CDR FORM))
        (GO LAB))
      (RETURN
       (CONS 'LIST
             (PROG (Q FORALL-RESULT FORALL-ENDPTR)
               (SETQ Q R)
               (COND ((NULL Q) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (Q) (PREPF Q)) (CAR Q)) NIL)))
              LOOPLABEL
               (SETQ Q (CDR Q))
               (COND ((NULL Q) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (Q) (PREPF Q)) (CAR Q)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'SOLVESQ 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVESQ 'DEFINED-ON-LINE '186) 
(PUT 'SOLVESQ 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVESQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVESQ (EX VAR MUL)
    (PROG (R X)
      (SETQ R
              (PROG (W FORALL-RESULT FORALL-ENDPTR)
                (SETQ W (SOLVESQ1 EX VAR MUL))
               STARTOVER
                (COND ((NULL W) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (W)
                           (COND
                            ((OR (NULL (CADR W))
                                 (EQCAR (SETQ X (PREPSQ (CAAR W))) 'ROOT_OF)
                                 (CAR
                                  (SUBFX (CDR EX) (LIST (CONS (CAADR W) X)))))
                             (LIST W))))
                         (CAR W)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ W (CDR W))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL W) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (W)
                           (COND
                            ((OR (NULL (CADR W))
                                 (EQCAR (SETQ X (PREPSQ (CAAR W))) 'ROOT_OF)
                                 (CAR
                                  (SUBFX (CDR EX) (LIST (CONS (CAADR W) X)))))
                             (LIST W))))
                         (CAR W)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ W (CDR W))
                (GO LOOPLABEL)))
      (COND
       ((AND R (NOT (OR (ATOM (CDR EX)) (ATOM (CAR (CDR EX))))))
        (SETQ ASSUMPTIONS
                (PROGN
                 (SETQ ALGLIST* (CONS NIL NIL))
                 (APPEND ASSUMPTIONS (LIST (PREPF (CDR EX))))))))
      (RETURN R))) 
(PUT 'SUBFX 'NUMBER-OF-ARGS 2) 
(PUT 'SUBFX 'DEFINED-ON-LINE '200) 
(PUT 'SUBFX 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SUBFX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBFX (U V)
    ((LAMBDA (X) (COND ((ERRORP X) (CONS NIL 1)) (T (CAR X))))
     (ERRORSET2 (LIST 'SUBF (MKQUOTE U) (MKQUOTE V))))) 
(FLUID
 '(SOLVE-APPLY-RULES-RECURSION-LEVEL* SOLVE-APPLY-RULES-MAX-RECURSION-DEPTH*)) 
(SETQ SOLVE-APPLY-RULES-MAX-RECURSION-DEPTH* 20) 
(SETQ SOLVE-APPLY-RULES-RECURSION-LEVEL* 0) 
(PUT 'SOLVESQ1 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVESQ1 'DEFINED-ON-LINE '214) 
(PUT 'SOLVESQ1 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVESQ1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVESQ1 (EX VAR MUL)
    (PROG (E1 OLDKORDER X1 Y Z MU)
      (SETQ MU 0)
      (SETQ EX (CAR EX))
      (COND ((NULL (SETQ X1 (TOPKERN EX VAR))) (RETURN NIL)))
      (SETQ OLDKORDER (SETKORDER (LIST VAR)))
      (SETQ E1 (REORDER EX))
      (SETKORDER OLDKORDER)
      (COND
       (*MODULAR
        (PROGN
         (LOAD_PACKAGE (LIST 'MODSR))
         (RETURN (MSOLVESYS (LIST E1) X1 NIL)))))
      (COND
       ((AND (EQUAL (CAAAR E1) VAR) (NULL (CDR X1)) (EQUAL (CDAAR E1) 1))
        (RETURN
         (LIST
          (LIST
           (LIST
            (MULTSQ (CONS (NEGF (REORDER (CDR E1))) 1)
                    (INVSQ (CONS (REORDER (CDAR E1)) 1))))
           (LIST VAR) MUL)))))
      (SETQ EX (COND (*ROUNDED (LIST 1 (CONS EX 1))) (T (FCTRF EX))))
      (COND ((OR (ATOM (CAR EX)) (ATOM (CAR (CAR EX)))) (SETQ EX (CDR EX)))
            (T (SETQ EX (CONS (CONS (CAR EX) 1) (CDR EX)))))
      (PROG (J)
        (SETQ J EX)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ E1 (CAR J))
            (SETQ X1 (TOPKERN E1 VAR))
            (SETQ MU (TIMES MUL (CDR J)))
            (COND
             ((AND (NULL *ROUNDED) (EQUAL (LENGTH X1) 1)
                   (LESSP (LENGTH (KERNELS E1)) 5)
                   (GREATERP (LENGTH (SETQ Y (DECOMPOSEF1 E1 NIL))) 1)
                   (SETQ Y (SOLVEDECOMP (REVERSE Y) (CAR X1) MU)))
              (SETQ Z (SOLNSMERGE Y Z)))
             ((AND
               (EQUAL
                ((LAMBDA (KORD*) (DEGR (SETQ Y (REORDER E1)) VAR)) (LIST VAR))
                1)
               (NOT (SMEMBER VAR (DELETE VAR X1))))
              (PROGN
               (SETQ Y
                       (LIST
                        (LIST
                         (MULTSQ (CONS (NEGF (REORDER (CDR Y))) 1)
                                 (INVSQ (CONS (REORDER (CDAR Y)) 1))))
                        (LIST VAR) MU))
               (SETQ Z (CONS Y Z))))
             (X1
              (SETQ Z
                      (SOLNSMERGE
                       (COND ((NULL (CDR X1)) (SOLVE1 E1 (CAR X1) VAR MU))
                             ((NEQ
                               (SETQ Y (PRINCIPLE-OF-POWERS-SOLN E1 X1 VAR MU))
                               'UNSOLVED)
                              Y)
                             ((NOT
                               (SMEMQ 'SOL
                                      (SETQ X1 (SOLVE-APPLY-RULES E1 VAR))))
                              (PROGN
                               (COND
                                ((GEQ SOLVE-APPLY-RULES-RECURSION-LEVEL*
                                      SOLVE-APPLY-RULES-MAX-RECURSION-DEPTH*)
                                 (RERROR 'SOLVE 4
                                         "SOLVE: table recursion too deep"))
                                (T
                                 ((LAMBDA (SOLVE-APPLY-RULES-RECURSION-LEVEL*)
                                    (SOLVESQ X1 VAR MU))
                                  (PLUS SOLVE-APPLY-RULES-RECURSION-LEVEL*
                                        1))))))
                             (T (MKROOTSOF (CONS E1 1) VAR MU)))
                       Z))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN Z))) 
(PUT 'SOLVEDECOMP 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVEDECOMP 'DEFINED-ON-LINE '272) 
(PUT 'SOLVEDECOMP 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEDECOMP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEDECOMP (U VAR MU)
    (PROG (FAILED X Y)
      (COND
       ((EQUAL (LENGTH (SETQ X (SOLVE0 (CAR U) (CADADR U)))) 1) (RETURN NIL)))
      (SETQ U (CDR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ Y X)
         (SETQ X NIL)
         (PROG (J)
           (SETQ J Y)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (COND
               ((OR (NEQ (CADDR J) 1) (NULL (CADR J)))
                (PROGN
                 (LPRIM (LIST "Tell Hearn solvedecomp" Y U))
                 (SETQ FAILED T)
                 NIL))
               (T
                (SETQ X
                        (SOLNSMERGE
                         (SOLVE0
                          (LIST 'DIFFERENCE (PREPSQ (CAAR J)) (CADDAR U))
                          (COND ((CDR U) (CADADR U)) (T VAR)))
                         X)))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (COND (FAILED (SETQ U NIL)) (T (SETQ U (CDR U)))))
        (GO WHILELABEL))
      (RETURN (COND (FAILED NIL) (T (ADJUSTMUL X MU)))))) 
(PUT 'ADJUSTMUL 'NUMBER-OF-ARGS 2) 
(PUT 'ADJUSTMUL 'DEFINED-ON-LINE '293) 
(PUT 'ADJUSTMUL 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'ADJUSTMUL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADJUSTMUL (U N)
    (COND ((EQUAL N 1) U)
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X U)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (LIST (CAR X) (CADR X) (TIMES N (CADDR X))))
                               (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (X) (LIST (CAR X) (CADR X) (TIMES N (CADDR X))))
                       (CAR X))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'SOLVE1 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVE1 'DEFINED-ON-LINE '298) 
(PUT 'SOLVE1 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE1 (E1 X1 VAR MU)
    (PROG (*NUMVAL*) (SETQ *NUMVAL* *NUMVAL) (RETURN (SOLVE11 E1 X1 VAR MU)))) 
(PUT 'SOLVE11 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVE11 'DEFINED-ON-LINE '309) 
(PUT 'SOLVE11 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE11 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE11 (E1 X1 VAR MU)
    (PROG (*NUMVAL B COEFS HIPOW W N)
      (SETQ N 0)
      (COND
       ((AND (NULL *FULLROOTS) (NULL *ROUNDED) (GREATERP (NUMRDEG E1 VAR) 2))
        (RETURN (MKROOTSOF (CONS E1 1) VAR MU))))
      (SETQ *NUMVAL T)
      (SETQ COEFS (ERRORSET* (LIST 'SOLVECOEFF (MKQUOTE E1) (MKQUOTE X1)) NIL))
      (COND
       ((OR (ATOM COEFS) (AND (ATOM X1) (NEQ X1 VAR)))
        (RETURN (MKROOTSOF (CONS E1 1) VAR MU))))
      (SETQ COEFS (CAR COEFS))
      (SETQ N !GCD)
      (SETQ HIPOW (CAR (SETQ W (CAR (REVERSE COEFS)))))
      (COND
       ((NOT ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CAR (CDR W))))
        (SETQ ASSUMPTIONS
                (PROGN
                 (SETQ ALGLIST* (CONS NIL NIL))
                 (APPEND ASSUMPTIONS (LIST (PREPF (CAR (CDR W)))))))))
      (COND
       ((NOT ((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (CDR (CDR W))))
        (SETQ ASSUMPTIONS
                (PROGN
                 (SETQ ALGLIST* (CONS NIL NIL))
                 (APPEND ASSUMPTIONS (LIST (PREPF (CDR (CDR W)))))))))
      (COND
       ((EQUAL HIPOW 1)
        (RETURN
         (PROG (LINCOEFF Y Z)
           (COND ((NULL (CDR COEFS)) (SETQ B 0))
                 (T
                  (SETQ B
                          (PREPSQ
                           (MULTSQ (NEGSQ (CDAR COEFS))
                                   (INVSQ (CDADR COEFS)))))))
           (COND ((NEQ N 1) (SETQ B (LIST 'EXPT B (LIST 'QUOTIENT 1 N)))))
           (PROG (K)
             (SETQ K 0)
            LAB
             (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K)) (RETURN NIL)))
             (PROGN
              (SETQ LINCOEFF
                      (LIST 'TIMES B
                            (MKEXP (LIST 'QUOTIENT (LIST 'TIMES K 2 'PI) N))))
              ((LAMBDA (ALGLIST*) (SETQ LINCOEFF (SIMP* LINCOEFF)))
               (CONS NIL NIL))
              (COND
               ((EQUAL X1 VAR)
                (SETQ Y (SOLNMERGE (LIST LINCOEFF) (LIST VAR) MU Y)))
               ((NOT (IDP (SETQ Z (CAR X1)))) (TYPERR Z "solve operator"))
               ((SETQ Z (GET Z 'SOLVEFN))
                (SETQ Y
                        (SOLNSMERGE (APPLY1 Z (LIST (CDR X1) VAR MU LINCOEFF))
                         Y)))
               ((SETQ Z (GET (CAR X1) 'INVERSE))
                (SETQ Y
                        (SOLNSMERGE
                         (SOLVESQ
                          (ADDSQ (SIMP* (CADR X1))
                                 (NEGSQ (SIMP* (LIST Z (MK*SQ LINCOEFF)))))
                          VAR MU)
                         Y)))
               (T
                (SETQ Y
                        (SOLNSMERGE
                         (MKROOTSOF (ADDSQ (SIMP* X1) (NEGSQ LINCOEFF)) VAR MU)
                         Y)))))
             (SETQ K (PLUS2 K 1))
             (GO LAB))
           (RETURN Y))))
       ((EQUAL HIPOW 2)
        (RETURN
         (PROGN
          (SETQ X1 (EXPTSQ (SIMP* X1) N))
          (SETQ W NIL)
          (PROG (J)
            (SETQ J
                    (SOLVEQUADRATIC (GETCOEFF COEFS 2) (GETCOEFF COEFS 1)
                     (GETCOEFF COEFS 0)))
           LAB
            (COND ((NULL J) (RETURN NIL)))
            ((LAMBDA (J)
               (SETQ W (SOLNSMERGE (SOLVESQ (ADDSQ X1 (NEGSQ J)) VAR MU) W)))
             (CAR J))
            (SETQ J (CDR J))
            (GO LAB))
          W)))
       (T (RETURN (SOLVEHIPOW E1 X1 VAR MU COEFS HIPOW)))))) 
(PUT 'SOLNSMERGE 'NUMBER-OF-ARGS 2) 
(PUT 'SOLNSMERGE 'DEFINED-ON-LINE '368) 
(PUT 'SOLNSMERGE 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLNSMERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLNSMERGE (U V)
    (COND ((NULL U) V)
          (T (SOLNSMERGE (CDR U) (SOLNMERGE (CAAR U) (CADAR U) (CADDAR U) V))))) 
(PUT 'GETCOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'GETCOEFF 'DEFINED-ON-LINE '372) 
(PUT 'GETCOEFF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'GETCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETCOEFF (U N)
    (COND ((NULL U) (CONS NIL 1)) ((EQUAL N (CAAR U)) (CDAR U))
          ((LESSP N (CAAR U)) (CONS NIL 1)) (T (GETCOEFF (CDR U) N)))) 
(PUT 'PUTCOEFF 'NUMBER-OF-ARGS 3) 
(PUT 'PUTCOEFF 'DEFINED-ON-LINE '379) 
(PUT 'PUTCOEFF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'PUTCOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PUTCOEFF (U N V)
    (COND ((NULL U) (LIST (CONS N V)))
          ((EQUAL N (CAAR U)) (CONS (CONS N V) (CDR U)))
          ((LESSP N (CAAR U)) (CONS (CONS N V) U))
          (T (CONS (CAR U) (PUTCOEFF (CDR U) N V))))) 
(PUT 'SOLVEHIPOW 'NUMBER-OF-ARGS 6) 
(PUT 'SOLVEHIPOW 'DEFINED-ON-LINE '386) 
(PUT 'SOLVEHIPOW 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEHIPOW 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEHIPOW (E1 X1 VAR MU COEFS HIPOW)
    (PROG (B C D F RCOEFFS Z)
      (SETQ F (QUOTIENT (PLUS HIPOW 1) 2))
      (SETQ D (EXPTSQ (SIMP* X1) !GCD))
      (SETQ RCOEFFS (REVERSE COEFS))
      (RETURN
       (COND
        ((SOLVE1TEST1 COEFS RCOEFFS F)
         (COND
          ((EQUAL (PLUS F F) (PLUS HIPOW 1))
           (PROGN
            (SETQ C (ADDSQ D (CONS 1 1)))
            (SOLNSMERGE (SOLVESQ C VAR MU)
             (SOLVESQ (MULTSQ (CONS E1 1) (INVSQ C)) VAR MU))))
          (T
           (PROGN
            (SETQ COEFS (PUTCOEFF COEFS 0 (CONS 2 1)))
            (SETQ COEFS (PUTCOEFF COEFS 1 (SIMP* '!X)))
            (SETQ C
                    (ADDSQ
                     (MULTSQ (GETCOEFF COEFS (PLUS F 1)) (GETCOEFF COEFS 1))
                     (GETCOEFF COEFS F)))
            (PROG (J)
              (SETQ J 2)
             LAB
              (COND ((MINUSP (DIFFERENCE F J)) (RETURN NIL)))
              (PROGN
               (SETQ COEFS
                       (PUTCOEFF COEFS J
                        (ADDSQ
                         (MULTSQ (GETCOEFF COEFS 1)
                                 (GETCOEFF COEFS (DIFFERENCE J 1)))
                         (NEGSQ (GETCOEFF COEFS (DIFFERENCE J 2))))))
               (SETQ C
                       (ADDSQ C
                              (MULTSQ (GETCOEFF COEFS J)
                                      (GETCOEFF COEFS (PLUS F J))))))
              (SETQ J (PLUS2 J 1))
              (GO LAB))
            (PROG (J)
              (SETQ J (SOLVESQ C '!X MU))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (SETQ Z
                         (SOLNSMERGE
                          (SOLVESQ
                           (ADDSQ (CONS 1 1)
                                  (MULTSQ D (ADDSQ D (NEGSQ (CAAR J)))))
                           VAR (CADDR J))
                          Z)))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            Z))))
        ((SOLVE1TEST2 COEFS RCOEFFS F)
         (PROGN
          (SETQ C (ADDSQ D (CONS (MINUS 1) 1)))
          (SETQ B (SOLVESQ C VAR MU))
          (SETQ E1 (MULTSQ (CONS E1 1) (INVSQ C)))
          (COND
           ((EQUAL (PLUS F F) HIPOW)
            (PROGN
             (SETQ C (ADDSQ D (CONS 1 1)))
             (SETQ B (SOLNSMERGE (SOLVESQ C VAR MU) B))
             (SETQ E1 (MULTSQ E1 (INVSQ C))))))
          (SOLNSMERGE (SOLVESQ E1 VAR MU) B)))
        ((AND *ROUNDED (UNIVARIATEP E1)) (REVERSIP (SOLVEROOTS E1 VAR MU)))
        ((NULL *FULLROOTS) (MKROOTSOF (CONS E1 1) VAR MU))
        ((EQUAL HIPOW 3)
         (PROGN
          (PROG (J)
            (SETQ J
                    (SOLVECUBIC (GETCOEFF COEFS 3) (GETCOEFF COEFS 2)
                     (GETCOEFF COEFS 1) (GETCOEFF COEFS 0)))
           LAB
            (COND ((NULL J) (RETURN NIL)))
            ((LAMBDA (J)
               (SETQ Z (SOLNSMERGE (SOLVESQ (ADDSQ D (NEGSQ J)) VAR MU) Z)))
             (CAR J))
            (SETQ J (CDR J))
            (GO LAB))
          Z))
        ((EQUAL HIPOW 4)
         (PROGN
          (PROG (J)
            (SETQ J
                    (SOLVEQUARTIC (GETCOEFF COEFS 4) (GETCOEFF COEFS 3)
                     (GETCOEFF COEFS 2) (GETCOEFF COEFS 1) (GETCOEFF COEFS 0)))
           LAB
            (COND ((NULL J) (RETURN NIL)))
            ((LAMBDA (J)
               (SETQ Z (SOLNSMERGE (SOLVESQ (ADDSQ D (NEGSQ J)) VAR MU) Z)))
             (CAR J))
            (SETQ J (CDR J))
            (GO LAB))
          Z))
        (T (MKROOTSOF (CONS E1 1) VAR MU)))))) 
(PUT 'SOLNMERGE 'NUMBER-OF-ARGS 4) 
(PUT 'SOLNMERGE 'DEFINED-ON-LINE '454) 
(PUT 'SOLNMERGE 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLNMERGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLNMERGE (U VARLIST MU Y)
    (COND ((NULL Y) (LIST (LIST U VARLIST MU)))
          ((AND (EQUAL U (CAAR Y)) (EQUAL VARLIST (CADAR Y)))
           (CONS (LIST (CAAR Y) (CADAR Y) (PLUS MU (CADDAR Y))) (CDR Y)))
          (T (CONS (CAR Y) (SOLNMERGE U VARLIST MU (CDR Y)))))) 
(PUT 'NILCHK 'NUMBER-OF-ARGS 1) 
(PUT 'NILCHK 'DEFINED-ON-LINE '462) 
(PUT 'NILCHK 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'NILCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NILCHK (U) (COND ((NULL U) (CONS U 1)) (T U))) 
(PUT 'SOLVE1TEST1 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVE1TEST1 'DEFINED-ON-LINE '464) 
(PUT 'SOLVE1TEST1 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE1TEST1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE1TEST1 (COEFS RCOEFFS F)
    (PROG (J P)
      (SETQ J 0)
      (SETQ P 0)
      (COND
       ((OR (NULL COEFS) (NEQ (CAAR COEFS) 0) (NULL *FULLROOTS)) (RETURN NIL)))
      (SETQ P (PLUS (CAAR COEFS) (CAAR RCOEFFS)))
     A
      (COND ((GREATERP J F) (RETURN T))
            ((OR (NEQ (PLUS (CAAR COEFS) (CAAR RCOEFFS)) P)
                 (NEQ (CDAR COEFS) (CDAR RCOEFFS)))
             (RETURN NIL)))
      (SETQ COEFS (CDR COEFS))
      (SETQ RCOEFFS (CDR RCOEFFS))
      (SETQ J (PLUS J 1))
      (GO A))) 
(PUT 'SOLVE1TEST2 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVE1TEST2 'DEFINED-ON-LINE '479) 
(PUT 'SOLVE1TEST2 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVE1TEST2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE1TEST2 (COEFS RCOEFFS F)
    (PROG (J P)
      (SETQ J 0)
      (SETQ P 0)
      (COND
       ((OR (NULL COEFS) (NEQ (CAAR COEFS) 0) (NULL *FULLROOTS)) (RETURN NIL)))
      (SETQ P (PLUS (CAAR COEFS) (CAAR RCOEFFS)))
     A
      (COND ((GREATERP J F) (RETURN T))
            ((OR (NEQ (PLUS (CAAR COEFS) (CAAR RCOEFFS)) P)
                 (CAR (ADDSQ (CDAR COEFS) (CDAR RCOEFFS))))
             (RETURN NIL)))
      (SETQ COEFS (CDR COEFS))
      (SETQ RCOEFFS (CDR RCOEFFS))
      (SETQ J (PLUS J 1))
      (GO A))) 
(PUT 'SOLVEABS 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEABS 'DEFINED-ON-LINE '495) 
(PUT 'SOLVEABS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEABS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEABS (U)
    (PROG (MU VAR LINCOEFF S)
      (SETQ VAR (CADR U))
      (SETQ MU (CADDR U))
      (SETQ LINCOEFF (CADDDR U))
      (SETQ S (SIGN-OF (PREPSQ LINCOEFF)))
      (COND ((EQUAL S (MINUS 1)) (RETURN NIL)))
      (SETQ U (SIMP* (CAAR U)))
      (COND ((EQUAL S 0) (RETURN (SOLVESQ U VAR MU)))
            (T
             (RETURN
              (SOLNSMERGE (SOLVESQ (ADDSQ U LINCOEFF) VAR MU)
               (SOLVESQ (ADDSQ U (NEGSQ LINCOEFF)) VAR MU))))))) 
(PUT 'ABS 'SOLVEFN 'SOLVEABS) 
(PUT 'SOLVEEXPT 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEEXPT 'DEFINED-ON-LINE '510) 
(PUT 'SOLVEEXPT 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEEXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEEXPT (U)
    (PROG (C MU VAR LINCOEFF)
      (SETQ VAR (CADR U))
      (SETQ MU (CADDR U))
      (SETQ LINCOEFF (CADDDR U))
      (SETQ U (CAR U))
      (RETURN
       (COND
        ((FREEOF (CAR U) VAR)
         (COND ((NULL (CAR LINCOEFF)) NIL)
               (T
                (PROGN
                 (COND
                  (*ALLBRANCH
                   (PROGN
                    (SETQ !ARBINT (PLUS !ARBINT 1))
                    (SETQ C (LIST 'TIMES 2 'I 'PI (LIST 'ARBINT !ARBINT)))))
                  (T (SETQ C 0)))
                 (SOLVESQ
                  (ADDSQ (SIMP* (CADR U))
                         (NEGSQ
                          (MULTSQ
                           (ADDSQ (SOLVEEXPT-LOGTERM LINCOEFF) (SIMP* C))
                           (INVSQ (SIMP* (LIST 'LOG (CAR U)))))))
                  VAR MU)))))
        ((AND (FREEOF (CADR U) VAR) (NULL (CAR LINCOEFF)))
         (COND
          ((CHECK-CONDITION (LIST 'EQUAL (LIST 'SIGN (CADR U)) 1))
           (SOLVESQ (SIMP* (CAR U)) VAR MU))
          (T (SOLVEEXPT-ROOTSOF U LINCOEFF VAR MU))))
        ((FREEOF (CADR U) VAR)
         (COND ((RATNUMP (CADR U)) (SOLVE-FRACTIONAL-POWER U LINCOEFF VAR MU))
               (T
                (PROGN
                 (COND
                  (*ALLBRANCH
                   (PROGN
                    (SETQ !ARBINT (PLUS !ARBINT 1))
                    (SETQ C
                            (MKEXP
                             (LIST 'QUOTIENT
                                   (LIST 'TIMES 2 'PI (LIST 'ARBINT !ARBINT))
                                   (CADR U))))))
                  (T (SETQ C 1)))
                 (SOLVESQ
                  (ADDSQ (SIMP* (CAR U))
                         (NEGSQ
                          (MULTSQ
                           (SIMP*
                            (LIST 'EXPT (MK*SQ LINCOEFF)
                                  (MK*SQ (INVSQ (SIMP* (CADR U))))))
                           (SIMP* C))))
                  VAR MU)))))
        (T (SOLVEEXPT-ROOTSOF U LINCOEFF VAR MU)))))) 
(PUT 'SOLVEEXPT-ROOTSOF 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVEEXPT-ROOTSOF 'DEFINED-ON-LINE '557) 
(PUT 'SOLVEEXPT-ROOTSOF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEEXPT-ROOTSOF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEEXPT-ROOTSOF (U LINCOEFF VAR MU)
    (MKROOTSOF (ADDSQ (SIMP* (CONS 'EXPT U)) (NEGSQ LINCOEFF)) VAR MU)) 
(PUT 'EXPT 'SOLVEFN 'SOLVEEXPT) 
(PUT 'SOLVEEXPT-LOGTERM 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEEXPT-LOGTERM 'DEFINED-ON-LINE '562) 
(PUT 'SOLVEEXPT-LOGTERM 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEEXPT-LOGTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEEXPT-LOGTERM (LINCOEFF) (SIMP* (LIST 'LOG (MK*SQ LINCOEFF)))) 
(PUT 'SOLVELOG 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVELOG 'DEFINED-ON-LINE '572) 
(PUT 'SOLVELOG 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVELOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVELOG (U)
    (SOLVESQ
     (ADDSQ (SIMP* (CAAR U))
            (NEGSQ (SIMP* (LIST 'EXPT 'E (MK*SQ (CADDDR U))))))
     (CADR U) (CADDR U))) 
(PUT 'LOG 'SOLVEFN 'SOLVELOG) 
(PUT 'SOLVEINVPAT 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEINVPAT 'DEFINED-ON-LINE '578) 
(PUT 'SOLVEINVPAT 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEINVPAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEINVPAT (U OP)
    (PROG (C F)
      (SETQ F (GET OP 'SOLVEINVPAT))
      (COND
       ((SMEMQ 'ARBINT F)
        (SETQ F
                (SUBST
                 (COND
                  (*ALLBRANCH (LIST 'ARBINT (SETQ !ARBINT (PLUS !ARBINT 1))))
                  (T 0))
                 'ARBINT F))))
      (COND ((NOT *ALLBRANCH) (SETQ F (LIST (CAR F)))))
      (RETURN
       (PROG (C FORALL-RESULT FORALL-ENDPTR)
         (SETQ C (REVERSE F))
        STARTOVER
         (COND ((NULL C) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (C)
                    (SOLVESQ
                     (SIMP*
                      (SUBST (CAAR U) '(~ V)
                             (SUBST (MK*SQ (CADDDR U)) '(~ R) C)))
                     (CADR U) (CADDR U)))
                  (CAR C)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ C (CDR C))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL C) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (C)
                    (SOLVESQ
                     (SIMP*
                      (SUBST (CAAR U) '(~ V)
                             (SUBST (MK*SQ (CADDDR U)) '(~ R) C)))
                     (CADR U) (CADDR U)))
                  (CAR C)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ C (CDR C))
         (GO LOOPLABEL))))) 
(PUT 'COS 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACOS (~ R)) (TIMES 2 ARBINT PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ACOS (~ R))) (TIMES 2 ARBINT PI)))) 
(PUT 'COS 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'COS))) 
(PUT 'SIN 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ASIN (~ R)) (TIMES 2 ARBINT PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ASIN (~ R))) (TIMES 2 ARBINT PI)
                  PI))) 
(PUT 'SIN 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'SIN))) 
(PUT 'SEC 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ASEC (~ R)) (TIMES 2 ARBINT PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ASEC (~ R))) (TIMES 2 ARBINT PI)))) 
(PUT 'SEC 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'SEC))) 
(PUT 'CSC 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACSC (~ R)) (TIMES 2 ARBINT PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ACSC (~ R))) (TIMES 2 ARBINT PI)
                  PI))) 
(PUT 'CSC 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'CSC))) 
(PUT 'TAN 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ATAN (~ R)) (TIMES ARBINT PI)))) 
(PUT 'TAN 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'TAN))) 
(PUT 'COT 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACOT (~ R)) (TIMES ARBINT PI)))) 
(PUT 'COT 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'COT))) 
(PUT 'COSH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACOSH (~ R)) (TIMES 2 ARBINT I PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ACOSH (~ R)))
                  (TIMES 2 ARBINT I PI)))) 
(PUT 'COSH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'COSH))) 
(PUT 'SINH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ASINH (~ R)) (TIMES 2 ARBINT I PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ASINH (~ R)))
                  (TIMES 2 ARBINT I PI) (TIMES I PI)))) 
(PUT 'SINH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'SINH))) 
(PUT 'SECH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ASECH (~ R)) (TIMES 2 ARBINT I PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ASECH (~ R)))
                  (TIMES 2 ARBINT I PI)))) 
(PUT 'SECH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'SECH))) 
(PUT 'CSCH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACSCH (~ R)) (TIMES 2 ARBINT I PI))
           '(PLUS (DIFFERENCE (MINUS (~ V)) (ACSCH (~ R)))
                  (TIMES 2 ARBINT I PI) (TIMES I PI)))) 
(PUT 'CSCH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'CSCH))) 
(PUT 'TANH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ATANH (~ R)) (TIMES ARBINT I PI)))) 
(PUT 'TANH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'TANH))) 
(PUT 'COTH 'SOLVEINVPAT
     (LIST '(PLUS (MINUS (~ V)) (ACOTH (~ R)) (TIMES ARBINT I PI)))) 
(PUT 'COTH 'SOLVEFN '(LAMBDA (U) (SOLVEINVPAT U 'COTH))) 
(PUT 'MKEXP 'NUMBER-OF-ARGS 1) 
(PUT 'MKEXP 'DEFINED-ON-LINE '657) 
(PUT 'MKEXP 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'MKEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKEXP (U)
    (REVAL1
     ((LAMBDA (*ROUNDED DMODE*)
        (AEVAL*
         ((LAMBDA (X)
            (LIST 'PLUS (LIST 'COS X) (LIST 'TIMES 'I (LIST 'SIN X))))
          (REVAL1 U T))))
      NIL NIL)
     T)) 
(PUT 'SOLVECOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVECOEFF 'DEFINED-ON-LINE '662) 
(PUT 'SOLVECOEFF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVECOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVECOEFF (EX VAR)
    (PROG (CLIST OLDKORD)
      (SETQ OLDKORD (UPDKORDER VAR))
      (SETQ CLIST (REORDER EX))
      (SETKORDER OLDKORD)
      (SETQ CLIST (COEFLIS CLIST))
      (SETQ !GCD (CAAR CLIST))
      (PROG (X)
        (SETQ X (CDR CLIST))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ !GCD (GCDN (CAR X) !GCD))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X CLIST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (RPLACA X (QUOTIENT (CAR X) !GCD))
            (RPLACD X (CONS (CDR X) 1))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN CLIST))) 
(PUT 'SOLVEROOTS 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVEROOTS 'DEFINED-ON-LINE '679) 
(PUT 'SOLVEROOTS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEROOTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEROOTS (EX VAR MU)
    (PROG (X Y)
      (SETQ X (REVAL1 (LIST 'ROOT_VAL (MK*SQ (CONS EX 1))) T))
      (COND
       ((NOT (EQ (CAR X) 'LIST)) (ERRACH (LIST "incorrect root format" EX))))
      (PROG (Z)
        (SETQ Z (CDR X))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (SETQ Y
                   (SOLNSMERGE
                    (SOLVESQ
                     (COND
                      ((NOT (EQ (CAR Z) 'EQUAL))
                       (ERRACH (LIST "incorrect root format" EX)))
                      (T (SIMPPLUS (LIST (CADR Z) (LIST 'MINUS (CADDR Z))))))
                     VAR MU)
                    Y)))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN Y))) 
(PUT 'SOLVESYS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVESYS 'DEFINED-ON-LINE '714) 
(PUT 'SOLVESYS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVESYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVESYS (EXLIS VARLIS)
    (PROG (OLDKORD OLDVARS)
      (COND
       (*MODULAR
        (PROGN (LOAD-PACKAGE 'MODSR) (RETURN (MSOLVESYS EXLIS VARLIS T)))))
      (SETQ OLDVARS VARS*)
      (SETQ OLDKORD (SETKORDER VARLIS))
      (SETQ EXLIS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J EXLIS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (REORDER J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (REORDER J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EXLIS
              (ERRORSET* (LIST 'SOLVEMIXEDSYS (MKQUOTE EXLIS) (MKQUOTE VARLIS))
                         T))
      (SETKORDER OLDKORD)
      (SETQ VARS* OLDVARS)
      (COND ((ERRORP EXLIS) (ERROR1)))
      (RETURN (CAR EXLIS)))) 
(PUT 'SOLVEMIXEDSYS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEMIXEDSYS 'DEFINED-ON-LINE '737) 
(PUT 'SOLVEMIXEDSYS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SOLVEMIXEDSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEMIXEDSYS (EXLIS VARLIS)
    (COND
     ((NULL (CADR (SETQ EXLIS (SIFTNONLNR EXLIS VARLIS))))
      (SOLVELNRSYS (CAR EXLIS) VARLIS))
     ((NULL (CAR EXLIS)) (SOLVENONLNRSYS (CADR EXLIS) VARLIS))
     (T
      (PROG (X Y Z)
        ((LAMBDA (*ARBVARS) (SETQ X (SOLVELNRSYS (CAR EXLIS) VARLIS))) NIL)
        (COND ((EQUAL (CAR X) 'INCONSISTENT) (RETURN X)) (T (SETQ X (CADR X))))
        (SETQ Z
                (PAIR (CADR X)
                      (PROG (EX FORALL-RESULT FORALL-ENDPTR)
                        (SETQ EX (CAR X))
                        (COND ((NULL EX) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (EX) (MK*SQ EX)) (CAR EX))
                                         NIL)))
                       LOOPLABEL
                        (SETQ EX (CDR EX))
                        (COND ((NULL EX) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (EX) (MK*SQ EX)) (CAR EX)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (SETQ EXLIS
                (PROG (EX FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EX (CADR EXLIS))
                 STARTOVER
                  (COND ((NULL EX) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (EX)
                             (COND
                              ((SETQ EX (CAR (SUBS2 (RESIMP (SUBF EX Z)))))
                               (LIST EX))))
                           (CAR EX)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ EX (CDR EX))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL EX) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (EX)
                             (COND
                              ((SETQ EX (CAR (SUBS2 (RESIMP (SUBF EX Z)))))
                               (LIST EX))))
                           (CAR EX)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ EX (CDR EX))
                  (GO LOOPLABEL)))
        (SETQ VARLIS (SETDIFF VARLIS (CADR X)))
        (SETQ Y (SOLVEMIXEDSYS EXLIS VARLIS))
        (COND
         ((MEMQ (CAR Y) (LIST 'INCONSISTENT 'SINGULAR 'FAILED NIL)) (RETURN Y))
         (T
          (RETURN
           (CONS T
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S (CDR Y))
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (S)
                                       (PROGN
                                        (SETQ Z
                                                (PROG (PR FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ PR
                                                          (PAIR (CADR S)
                                                                (CAR S)))
                                                 STARTOVER
                                                  (COND
                                                   ((NULL PR) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          ((LAMBDA (PR)
                                                             (COND
                                                              ((NOT
                                                                (SMEMQ 'ROOT_OF
                                                                       (CDR
                                                                        PR)))
                                                               (LIST
                                                                (CONS (CAR PR)
                                                                      (MK*SQ
                                                                       (CDR
                                                                        PR)))))))
                                                           (CAR PR)))
                                                  (SETQ FORALL-ENDPTR
                                                          (LASTPAIR
                                                           FORALL-RESULT))
                                                  (SETQ PR (CDR PR))
                                                  (COND
                                                   ((ATOM FORALL-ENDPTR)
                                                    (GO STARTOVER)))
                                                 LOOPLABEL
                                                  (COND
                                                   ((NULL PR)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          ((LAMBDA (PR)
                                                             (COND
                                                              ((NOT
                                                                (SMEMQ 'ROOT_OF
                                                                       (CDR
                                                                        PR)))
                                                               (LIST
                                                                (CONS (CAR PR)
                                                                      (MK*SQ
                                                                       (CDR
                                                                        PR)))))))
                                                           (CAR PR)))
                                                  (SETQ FORALL-ENDPTR
                                                          (LASTPAIR
                                                           FORALL-ENDPTR))
                                                  (SETQ PR (CDR PR))
                                                  (GO LOOPLABEL)))
                                        (LIST
                                         (APPEND (CAR S)
                                                 (PROG (EX FORALL-RESULT
                                                        FORALL-ENDPTR)
                                                   (SETQ EX (CAR X))
                                                   (COND
                                                    ((NULL EX) (RETURN NIL)))
                                                   (SETQ FORALL-RESULT
                                                           (SETQ FORALL-ENDPTR
                                                                   (CONS
                                                                    ((LAMBDA
                                                                         (EX)
                                                                       (SUBSQ
                                                                        EX Z))
                                                                     (CAR EX))
                                                                    NIL)))
                                                  LOOPLABEL
                                                   (SETQ EX (CDR EX))
                                                   (COND
                                                    ((NULL EX)
                                                     (RETURN FORALL-RESULT)))
                                                   (RPLACD FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EX)
                                                               (SUBSQ EX Z))
                                                             (CAR EX))
                                                            NIL))
                                                   (SETQ FORALL-ENDPTR
                                                           (CDR FORALL-ENDPTR))
                                                   (GO LOOPLABEL)))
                                         (APPEND (CADR S) (CADR X))
                                         (CADDR S))))
                                     (CAR S))
                                    NIL)))
                  LOOPLABEL
                   (SETQ S (CDR S))
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (S)
                               (PROGN
                                (SETQ Z
                                        (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ PR (PAIR (CADR S) (CAR S)))
                                         STARTOVER
                                          (COND ((NULL PR) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  ((LAMBDA (PR)
                                                     (COND
                                                      ((NOT
                                                        (SMEMQ 'ROOT_OF
                                                               (CDR PR)))
                                                       (LIST
                                                        (CONS (CAR PR)
                                                              (MK*SQ
                                                               (CDR PR)))))))
                                                   (CAR PR)))
                                          (SETQ FORALL-ENDPTR
                                                  (LASTPAIR FORALL-RESULT))
                                          (SETQ PR (CDR PR))
                                          (COND
                                           ((ATOM FORALL-ENDPTR)
                                            (GO STARTOVER)))
                                         LOOPLABEL
                                          (COND
                                           ((NULL PR) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  ((LAMBDA (PR)
                                                     (COND
                                                      ((NOT
                                                        (SMEMQ 'ROOT_OF
                                                               (CDR PR)))
                                                       (LIST
                                                        (CONS (CAR PR)
                                                              (MK*SQ
                                                               (CDR PR)))))))
                                                   (CAR PR)))
                                          (SETQ FORALL-ENDPTR
                                                  (LASTPAIR FORALL-ENDPTR))
                                          (SETQ PR (CDR PR))
                                          (GO LOOPLABEL)))
                                (LIST
                                 (APPEND (CAR S)
                                         (PROG (EX FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ EX (CAR X))
                                           (COND ((NULL EX) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EX)
                                                               (SUBSQ EX Z))
                                                             (CAR EX))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ EX (CDR EX))
                                           (COND
                                            ((NULL EX) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (EX) (SUBSQ EX Z))
                                                     (CAR EX))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (APPEND (CADR S) (CADR X)) (CADDR S))))
                             (CAR S))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))))) 
(PUT 'SIFTNONLNR 'NUMBER-OF-ARGS 2) 
(PUT 'SIFTNONLNR 'DEFINED-ON-LINE '769) 
(PUT 'SIFTNONLNR 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SIFTNONLNR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIFTNONLNR (EXLIS VARLIS)
    (PROG (LIN NONLIN)
      (PROG (EX)
        (SETQ EX EXLIS)
       LAB
        (COND ((NULL EX) (RETURN NIL)))
        ((LAMBDA (EX)
           (COND
            (EX
             (COND ((NONLNR EX VARLIS) (SETQ NONLIN (CONS EX NONLIN)))
                   (T (SETQ LIN (CONS EX LIN)))))))
         (CAR EX))
        (SETQ EX (CDR EX))
        (GO LAB))
      (RETURN (LIST (REVERSIP LIN) (REVERSIP NONLIN))))) 
(PUT 'NONLNRSYS 'NUMBER-OF-ARGS 2) 
(PUT 'NONLNRSYS 'DEFINED-ON-LINE '781) 
(PUT 'NONLNRSYS 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'NONLNRSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONLNRSYS (EXLIS VARLIS)
    (COND ((NULL EXLIS) NIL)
          (T (OR (NONLNR (CAR EXLIS) VARLIS) (NONLNRSYS (CDR EXLIS) VARLIS))))) 
(PUT 'NONLNR 'NUMBER-OF-ARGS 2) 
(PUT 'NONLNR 'DEFINED-ON-LINE '788) 
(PUT 'NONLNR 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'NONLNR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONLNR (EX VARLIS)
    (COND ((OR (ATOM EX) (ATOM (CAR EX))) NIL)
          ((MEMBER (CAAAR EX) VARLIS)
           (OR (GREATERP (CDAAR EX) 1) (NOT (FREEOFL (CDAR EX) VARLIS))
               (NONLNR (CDR EX) VARLIS)))
          (T
           (OR (NOT (FREEOFL (CAAAR EX) VARLIS)) (NONLNR (CDAR EX) VARLIS)
               (NONLNR (CDR EX) VARLIS))))) 
(PUT 'MKROOTSOFTAG 'NUMBER-OF-ARGS 0) 
(PUT 'MKROOTSOFTAG 'DEFINED-ON-LINE '801) 
(PUT 'MKROOTSOFTAG 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'MKROOTSOFTAG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKROOTSOFTAG NIL
    (PROG (NAME N)
      (SETQ N 0)
     LOOP
      (SETQ N (IPLUS2 N 1))
      (SETQ NAME (INTERN (COMPRESS (APPEND '(T A G _) (EXPLODE N)))))
      (COND ((FLAGP NAME 'USED*) (GO LOOP)))
      (RETURN (REVAL1 NAME T)))) 
(PUT 'MKROOTSOF 'NUMBER-OF-ARGS 3) 
(PUT 'MKROOTSOF 'DEFINED-ON-LINE '809) 
(PUT 'MKROOTSOF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'MKROOTSOF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKROOTSOF (E1 VAR MU)
    (PROG (X NAME)
      (SETQ X (COND ((IDP VAR) VAR) (T 'Q_)))
      (SETQ NAME (OR **NOROOTVARRENAMEP** (MKROOTSOFTAG)))
      (COND
       ((NOT **NOROOTVARRENAMEP**)
        (PROG ()
         WHILELABEL
          (COND ((NOT (SMEMBER X E1)) (RETURN NIL)))
          (SETQ X (INTERN (COMPRESS (APPEND (EXPLODE X) (EXPLODE '_)))))
          (GO WHILELABEL))))
      (SETQ E1 (PREPSQ* E1))
      (COND ((NEQ X VAR) (SETQ E1 (SUBST X VAR E1))))
      (RETURN
       (LIST
        (LIST
         (LIST
          (CONS (LIST (CONS (GETPOWER (FKERN (LIST 'ROOT_OF E1 X NAME)) 1) 1))
                1))
         (LIST VAR) MU))))) 
(PUT 'ROOT_OF 'PSOPFN 'ROOT_OF_EVAL) 
(PUT 'ROOT_OF_EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ROOT_OF_EVAL 'DEFINED-ON-LINE '823) 
(PUT 'ROOT_OF_EVAL 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'ROOT_OF_EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOT_OF_EVAL (U)
    (PROG (**NOROOTVARRENAMEP** X N V)
      (COND ((NULL (CDR U)) (REDERR "Too few arguments to root_eval")))
      (SETQ N (COND ((CDDR U) (CADDR U)) (T (MKROOTSOFTAG))))
      (SETQ **NOROOTVARRENAMEP** N)
      (SETQ V
              (COND ((NOT (NULL (CADR U))) (CADR U))
                    (T (CAR (SOLVEVARS (LIST (SIMP* (*EQN2A (CAR U)))))))))
      (SETQ X (SOLVEEVAL1 (LIST (CAR U) V)))
      (COND ((EQCAR X 'LIST) (SETQ X (CDR X))) (T (TYPERR X "list")))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQCAR J 'EQUAL) (CADDR J))
                                          (T (TYPERR J "equation"))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((EQCAR J 'EQUAL) (CADDR J))
                                  (T (TYPERR J "equation"))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL X) (REDERR "solve confusion in root_of_eval"))
            ((NULL (CDR X)) (RETURN (CAR X)))
            (T (RETURN (LIST 'ONE_OF (CONS 'LIST X) N)))))) 
(PUT 'ROOT_OF 'SUBFUNC 'SUBROOTOF) 
(PUT 'SUBROOTOF 'NUMBER-OF-ARGS 2) 
(PUT 'SUBROOTOF 'DEFINED-ON-LINE '841) 
(PUT 'SUBROOTOF 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'SUBROOTOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBROOTOF (L EXPN)
    (PROG (X Y)
      (PROG (J)
        (SETQ J (CDDR EXPN))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND
            ((SETQ X (ASSOC J L))
             (PROGN (SETQ Y (CONS X Y)) (SETQ L (DELETE X L))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ EXPN
              (CONS (SUBLIS L (CAR EXPN))
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDR EXPN))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (SUBSUBLIS L J)) (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (SUBSUBLIS L J)) (CAR J))
                                    NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (COND ((NULL Y) (RETURN EXPN)))
      (SETQ EXPN
              (ACONC*
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (REVERSIP* Y))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (LIST 'EQUAL (CAR J)
                                           (REVAL1 (CDR J) NIL)))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J)
                             (LIST 'EQUAL (CAR J) (REVAL1 (CDR J) NIL)))
                           (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               EXPN))
      (RETURN
       (COND (L (SUBEVAL EXPN))
             (T
              (MK*SQ
               (CONS (LIST (CONS (GETPOWER (FKERN (CONS 'SUB EXPN)) 1) 1))
                     1))))))) 
(PUT 'POLYPEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'POLYPEVAL 'DEFINED-ON-LINE '856) 
(PUT 'POLYPEVAL 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'POLYPEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POLYPEVAL (U)
    (PROG (BOOL V)
      (SETQ V (CADR U))
      (SETQ U (SIMP (CAR U)))
      (COND ((NEQ (CDR U) 1) (RETURN NIL)) (T (SETQ U (KERNELS (CAR U)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (NULL BOOL))) (RETURN NIL)))
        (PROGN
         (COND ((AND (NEQ V (CAR U)) (SMEMBER V (CAR U))) (SETQ BOOL T)))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN (NULL BOOL)))) 
(PUT 'POLYP 'PSOPFN 'POLYPEVAL) 
((LAMBDA (DMODE* *MODULAR *ROUNDED *COMPLEX *MCD)
   (PROGN
    (AEVAL (DEPEND (LIST '~P '~X)))
    (AEVAL
     (LET
      '((REPLACEBY (EXPT (ROOT_OF (~ P) (~ X) (~ TG)) (~ N))
         (WHEN
          (SUB (EQUAL X (ROOT_OF P X TG))
           (MINUS
            (TIMES (QUOTIENT (REDUCT P X) (COEFFN P X (DEG P X)))
                   (EXPT X (DIFFERENCE N (DEG P X))))))
          (AND (POLYP P X) (FIXP N) (GEQ (DEG P X) 1) (GEQ N (DEG P X))))))))
    (AEVAL (NODEPEND (LIST '~P '~X)))
    (AEVAL 'NIL)))
 NIL NIL NIL NIL T) 
(PUT 'EXPAND_CASES 'NUMBER-OF-ARGS 1) 
(PUT 'EXPAND_CASES 'DEFINED-ON-LINE '884) 
(PUT 'EXPAND_CASES 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'EXPAND_CASES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPAND_CASES (U)
    (PROG (BOOL SL TAGS)
      (SETQ SL (LIST NIL))
      (SETQ TAGS (LIST NIL))
      (SETQ U (REVAL1 U T))
      (COND ((NOT (EQCAR U 'LIST)) (TYPERR U "equation list"))
            (T (SETQ U (CDR U))))
      (COND
       ((EQCAR (CAR U) 'LIST)
        (PROGN
         (SETQ U
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J U)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (COND ((EQCAR J 'LIST) (CDR J))
                                             (T (TYPERR J "equation list"))))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J)
                               (COND ((EQCAR J 'LIST) (CDR J))
                                     (T (TYPERR J "equation list"))))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ BOOL T)))
       (T
        (SETQ U
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J U)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (LIST J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (LIST J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
               STARTOVER
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (J) (EXPAND_CASE1 J SL TAGS)) (CAR J)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ J (CDR J))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (J) (EXPAND_CASE1 J SL TAGS)) (CAR J)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ J (CDR J))
                (GO LOOPLABEL)))
      (RETURN
       (CONS 'LIST
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J U)
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (COND ((NULL BOOL) (CAR J))
                                         (T (CONS 'LIST J))))
                                 (CAR J))
                                NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (J)
                           (COND ((NULL BOOL) (CAR J)) (T (CONS 'LIST J))))
                         (CAR J))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'EXPAND_CASE1 'NUMBER-OF-ARGS 3) 
(PUT 'EXPAND_CASE1 'DEFINED-ON-LINE '901) 
(PUT 'EXPAND_CASE1 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'EXPAND_CASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPAND_CASE1 (U SL TAGS)
    (COND ((NULL U) NIL)
          (T
           (EXPAND_MERGE (EXPAND_CASE2 (CAR U) SL TAGS)
            (EXPAND_CASE1 (CDR U) SL TAGS))))) 
(PUT 'EXPAND_MERGE 'NUMBER-OF-ARGS 2) 
(PUT 'EXPAND_MERGE 'DEFINED-ON-LINE '906) 
(PUT 'EXPAND_MERGE 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'EXPAND_MERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPAND_MERGE (U V)
    (COND
     ((NULL V)
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J U)
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (LIST J)) (CAR J)) NIL)))
       LOOPLABEL
        (SETQ J (CDR J))
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (LIST J)) (CAR J)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (PROG (J FORALL-RESULT FORALL-ENDPTR)
        (SETQ J U)
       STARTOVER
        (COND ((NULL J) (RETURN NIL)))
        (SETQ FORALL-RESULT
                ((LAMBDA (J)
                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                     (SETQ K V)
                     (COND ((NULL K) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (K) (CONS J K)) (CAR K))
                                           NIL)))
                    LOOPLABEL
                     (SETQ K (CDR K))
                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (K) (CONS J K)) (CAR K)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                 (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ J (CDR J))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL J) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                ((LAMBDA (J)
                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                     (SETQ K V)
                     (COND ((NULL K) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (K) (CONS J K)) (CAR K))
                                           NIL)))
                    LOOPLABEL
                     (SETQ K (CDR K))
                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (K) (CONS J K)) (CAR K)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
                 (CAR J)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ J (CDR J))
        (GO LOOPLABEL))))) 
(PUT 'EXPAND_CASE2 'NUMBER-OF-ARGS 3) 
(PUT 'EXPAND_CASE2 'DEFINED-ON-LINE '910) 
(PUT 'EXPAND_CASE2 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'EXPAND_CASE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPAND_CASE2 (U SL TAGS)
    (PROG (TAG V VAR)
      (SETQ VAR (CADR U))
      (SETQ V (CADDR U))
      (COND
       ((EQCAR V 'ONE_OF)
        (PROGN
         (SETQ TAG (CADDR V))
         (COND ((MEMBER TAG TAGS) (TYPERR TAG "unique choice tag"))
               ((NULL (ASSOC TAG SL))
                (SETCDR SL (CONS (CONS TAG (CDADR V)) (CDR SL)))))
         (RETURN
          (COND
           ((EQCAR (CADR V) 'LIST)
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J (CDADR V))
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (LIST 'EQUAL VAR J)) (CAR J))
                                    NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (J) (LIST 'EQUAL VAR J)) (CAR J)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           (T (TYPERR (CADR V) "list"))))))
       ((EQCAR V 'ROOT_OF)
        (PROGN
         (SETQ TAG (CADDDR V))
         (SETCDR TAGS (CONS TAG (CDR TAGS)))
         (COND ((ASSOC TAG SL) (TYPERR TAG "unique choice tag"))))))
      (RETURN (LIST U)))) 
(FLUID '(SOLVE_INVTRIG_SOLN*)) 
(SHARE (LIST 'SOLVE_INVTRIG_SOLN*)) 
(PUT 'CHECK_SOLVE_INV_TRIG 'NUMBER-OF-ARGS 3) 
(PUT 'CHECK_SOLVE_INV_TRIG 'DEFINED-ON-LINE '936) 
(PUT 'CHECK_SOLVE_INV_TRIG 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'CHECK_SOLVE_INV_TRIG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHECK_SOLVE_INV_TRIG (FN EQU VAR)
    (PROG (X S)
      (SETQ X
              (EVALLETSUB2
               (LIST '(SOLVE_TRIG_RULES) (LIST 'SIMP* (MKQUOTE (LIST FN EQU))))
               NIL))
      (COND
       ((OR (ERRORP X) (NOT (FREEOF (SETQ X (CAR X)) '(ASIN ACOS ATAN))))
        (RETURN NIL)))
      (PROG (SOL)
        (SETQ SOL
                (CDR
                 (SOLVEEVAL1
                  (LIST (MK*SQ (ADDSQ X (NEGSQ (SIMP* (LIST FN 0))))) VAR))))
       LAB
        (COND ((NULL SOL) (RETURN NIL)))
        ((LAMBDA (SOL)
           (COND ((IS_SOLUTION SOL EQU) (SETQ S (CONS (CADDR SOL) S)))))
         (CAR SOL))
        (SETQ SOL (CDR SOL))
        (GO LAB))
      (COND
       ((NULL S)
        (PROGN
         (SETQ SOLVE_INVTRIG_SOLN* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 1))
         (RETURN T)))
       ((NULL (CDR S)) (SETQ S (CAR S))) (T (SETQ S (CONS 'ONE_OF S))))
      (SETQ SOLVE_INVTRIG_SOLN*
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (LIST 'DIFFERENCE VAR S)))
      (RETURN T))) 
(FLAG '(CHECK_SOLVE_INV_TRIG) 'BOOLEAN) 
(PUT 'IS_SOLUTION 'NUMBER-OF-ARGS 2) 
(PUT 'IS_SOLUTION 'DEFINED-ON-LINE '955) 
(PUT 'IS_SOLUTION 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'IS_SOLUTION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IS_SOLUTION (SOL EQU)
    (PROG (VAR S RHS RESULT)
      (SETQ VAR (CADR SOL))
      (SETQ RHS (CADDR SOL))
      (SETQ EQU (CAR (SIMP* EQU)))
      (COND
       ((EQCAR RHS 'ONE_OF)
        (SETQ RESULT
                (CHECK-SOLNS
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S (CDR RHS))
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (S)
                                       (LIST (LIST (SIMP* S)) (LIST VAR) 1))
                                     (CAR S))
                                    NIL)))
                  LOOPLABEL
                   (SETQ S (CDR S))
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (S) (LIST (LIST (SIMP* S)) (LIST VAR) 1))
                             (CAR S))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 EQU VAR)))
       ((EQCAR RHS 'ROOT_OF) (SETQ RESULT T))
       (T
        (SETQ RESULT
                (CHECK-SOLNS (LIST (LIST (LIST (SIMP* RHS)) (LIST VAR) 1)) EQU
                 VAR))))
      (RETURN (COND ((NOT (EQ RESULT 'UNSOLVED)) RESULT) (T NIL))))) 
(PUT 'CHECK-CONDITION 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-CONDITION 'DEFINED-ON-LINE '969) 
(PUT 'CHECK-CONDITION 'DEFINED-IN-FILE 'SOLVE/SOLVE1.RED) 
(PUT 'CHECK-CONDITION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-CONDITION (U)
    (OR (NULL *PRECISE) (EVAL (FORMBOOL (REVAL1 U T) NIL 'ALGEBRAIC)))) 
(ENDMODULE) 