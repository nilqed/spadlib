(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SOLVEALG)) 
(FLUID '(*EXPANDEXPT)) 
(FLUID
 '(SYSTEM* OSYSTEM* UV* IV* FV* KL* SUB* INV* DEPL* *SOLVEALGP SOLVEALGDB*
   LAST-VARS* CONST-VARS* ROOT-VARS* *EXPLI GROEBROOTS* *TEST_SOLVEALG *ARBVARS
   *VAROPT SOLVE-GENSYMCOUNTER SOLVE-GENSYMPREFIX)) 
(FLUID '(*TRNONLNR)) 
(GLOBAL '(LOADED-PACKAGES* !ARBINT)) 
(SWITCH (LIST 'TRNONLNR)) 
(SETQ *SOLVEALGP T) 
(SETK 'SOLVEALG-RULES1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'PLUS (LIST '~ 'ALPHA) (LIST '~ 'BETA)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'SIN 'ALPHA) (LIST 'COS 'BETA))
                         (LIST 'TIMES (LIST 'COS 'ALPHA) (LIST 'SIN 'BETA))))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'PLUS (LIST '~ 'ALPHA) (LIST '~ 'BETA)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'COS 'ALPHA) (LIST 'COS 'BETA))
                         (LIST 'TIMES (LIST 'SIN 'ALPHA) (LIST 'SIN 'BETA))))
             (LIST 'REPLACEBY
                   (LIST 'SIN (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'ALPHA)))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'SIN 'ALPHA)
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA)))
                               (LIST 'TIMES (LIST 'COS 'ALPHA)
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA))))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'COS (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'ALPHA)))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'COS 'ALPHA)
                                     (LIST 'COS
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA)))
                               (LIST 'TIMES (LIST 'SIN 'ALPHA)
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA))))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SIN (LIST '~ 'ALPHA)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'COS 'ALPHA) 2)))
             (LIST 'REPLACEBY
                   (LIST 'SINH (LIST 'PLUS (LIST '~ 'ALPHA) (LIST '~ 'BETA)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'SINH 'ALPHA) (LIST 'COSH 'BETA))
                         (LIST 'TIMES (LIST 'COSH 'ALPHA) (LIST 'SINH 'BETA))))
             (LIST 'REPLACEBY
                   (LIST 'COSH (LIST 'PLUS (LIST '~ 'ALPHA) (LIST '~ 'BETA)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'COSH 'ALPHA) (LIST 'COSH 'BETA))
                         (LIST 'TIMES (LIST 'SINH 'ALPHA) (LIST 'SINH 'BETA))))
             (LIST 'REPLACEBY
                   (LIST 'SINH (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'ALPHA)))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'SINH 'ALPHA)
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA)))
                               (LIST 'TIMES (LIST 'COSH 'ALPHA)
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA))))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'COSH (LIST 'TIMES (LIST '~ 'N) (LIST '~ 'ALPHA)))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'COSH 'ALPHA)
                                     (LIST 'COSH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA)))
                               (LIST 'TIMES (LIST 'SINH 'ALPHA)
                                     (LIST 'SINH
                                           (LIST 'TIMES (LIST 'DIFFERENCE 'N 1)
                                                 'ALPHA))))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SINH (LIST '~ 'ALPHA)) 2)
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'COSH 'ALPHA) 2) 1))))) 
(SETK 'SOLVEALG-RULES2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'TAN (LIST '~ 'ALPHA))
                   (LIST 'QUOTIENT (LIST 'SIN 'ALPHA) (LIST 'COS 'ALPHA)))
             (LIST 'REPLACEBY (LIST 'COT (LIST '~ 'ALPHA))
                   (LIST 'QUOTIENT (LIST 'COS 'ALPHA) (LIST 'SIN 'ALPHA)))
             (LIST 'REPLACEBY (LIST 'TANH (LIST '~ 'ALPHA))
                   (LIST 'QUOTIENT (LIST 'SINH 'ALPHA) (LIST 'COSH 'ALPHA)))
             (LIST 'REPLACEBY (LIST 'COTH (LIST '~ 'ALPHA))
                   (LIST 'QUOTIENT (LIST 'COSH 'ALPHA) (LIST 'SINH 'ALPHA)))))) 
(SETK 'SOLVEALG-RULES3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SIN (LIST '~ 'ALPHA)) 2)
                   (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'COS 'ALPHA) 2)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SINH (LIST '~ 'ALPHA)) 2)
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'COSH 'ALPHA) 2) 1))))) 
(AEVAL (OPERATOR (LIST 'MY-EXPT))) 
(SETK 'SOLVEALG-RULES4
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'MY-EXPT (LIST '~ 'A) (LIST '~ 'B))
                         (LIST 'MY-EXPT 'A (LIST '~ 'C)))
                   (LIST 'MY-EXPT 'A (LIST 'PLUS 'B 'C)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'MY-EXPT (LIST '~ 'A) (LIST '~ 'B)) 'A)
                   (LIST 'MY-EXPT 'A (LIST 'PLUS 'B 1)))))) 
(AEVAL 'NIL) 
(PUT 'SOLVENONLNRSYS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRSYS 'DEFINED-ON-LINE '158) 
(PUT 'SOLVENONLNRSYS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYS (SYS UV) (SOLVENONLNRSYS0 SYS UV NIL)) 
(PUT 'SOLVENONLNRSYS0 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVENONLNRSYS0 'DEFINED-ON-LINE '166) 
(PUT 'SOLVENONLNRSYS0 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYS0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYS0 (SYS UV LVARS)
    (PROG (Q R S TAG *EXPANDEXPT)
      (SETQ S SYS)
      (SETQ SYS NIL)
      (PROG (X)
        (SETQ X S)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ SYS (UNION SYS (LIST X)))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ S '(NIL))
      (COND ((SOLVE-PSYSP SYS UV) (SETQ S (LIST SYS)))
            (T
             (PROG (P)
               (SETQ P SYS)
              LAB
               (COND ((NULL P) (RETURN NIL)))
               ((LAMBDA (P)
                  (PROGN
                   (SETQ R NIL)
                   (PROG (Q)
                     (SETQ Q (CDR (FCTRF P)))
                    LAB
                     (COND ((NULL Q) (RETURN NIL)))
                     ((LAMBDA (Q)
                        (COND
                         ((TOPKERNLIS (CAR Q) UV)
                          (PROG (U)
                            (SETQ U S)
                           LAB
                            (COND ((NULL U) (RETURN NIL)))
                            ((LAMBDA (U) (SETQ R (CONS (CONS (CAR Q) U) R)))
                             (CAR U))
                            (SETQ U (CDR U))
                            (GO LAB)))))
                      (CAR Q))
                     (SETQ Q (CDR Q))
                     (GO LAB))
                   (SETQ S R)))
                (CAR P))
               (SETQ P (CDR P))
               (GO LAB))))
      (SETQ TAG 'FAILED)
      (SETQ R NIL)
      (PROG (U)
        (SETQ U S)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (PROGN
            (SETQ U (SOLVENONLNRCOLLECTEXPT U))
            (SETQ Q (SOLVENONLNRSYS1 U UV))
            (COND ((EQCAR Q 'FAILED) (SETQ Q (SOLVENONLNRSYSSEP U UV))))
            (COND
             ((EQCAR Q 'FAILED) (SETQ Q (SOLVENONLNRSYSLIN U UV NIL LVARS))))
            (COND ((EQCAR Q 'NOT) (SETQ Q (SOLVENONLNRSYSLIN U UV T LVARS))))
            (COND ((EQCAR Q 'NOT) (SETQ Q '(FAILED))))
            (COND ((AND (CAR Q) (NEQ (CAR Q) 'FAILED)) (SETQ TAG (CAR Q))))
            (SETQ Q
                    (COND ((NEQ (CAR Q) 'FAILED) (CDR Q))
                          (T
                           (PROG (J FORALL-RESULT FORALL-ENDPTR)
                             (SETQ J U)
                             (COND ((NULL J) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (J)
                                                 (LIST (LIST (CONS J 1)) NIL
                                                       1))
                                               (CAR J))
                                              NIL)))
                            LOOPLABEL
                             (SETQ J (CDR J))
                             (COND ((NULL J) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (J)
                                         (LIST (LIST (CONS J 1)) NIL 1))
                                       (CAR J))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))))
            (SETQ R (UNION Q R))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (RETURN
       (COND ((OR (EQ TAG 'INCONSISTENT) (EQ TAG 'FAILED)) (LIST TAG))
             (T (CONS TAG R)))))) 
(PUT 'TOPKERNLIS 'NUMBER-OF-ARGS 2) 
(PUT 'TOPKERNLIS 'DEFINED-ON-LINE '194) 
(PUT 'TOPKERNLIS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'TOPKERNLIS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TOPKERNLIS (U V) (AND V (OR (TOPKERN U (CAR V)) (TOPKERNLIS U (CDR V))))) 
(PUT 'SOLVENONLNRCOLLECTEXPT 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVENONLNRCOLLECTEXPT 'DEFINED-ON-LINE '197) 
(PUT 'SOLVENONLNRCOLLECTEXPT 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRCOLLECTEXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVENONLNRCOLLECTEXPT (U)
    (COND ((NOT (SMEMQ 'EXPT U)) U)
          (T
           (PROGN
            (EVAL '(LET0 '(SOLVEALG-RULES4)))
            (SETQ U
                    (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Q U)
                      (COND ((NULL Q) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Q)
                                          (CAR
                                           (SIMP
                                            (SUBST 'EXPT 'MY-EXPT
                                                   (REVAL1
                                                    (PREPF
                                                     (SUBST 'MY-EXPT 'EXPT Q))
                                                    T)))))
                                        (CAR Q))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Q (CDR Q))
                      (COND ((NULL Q) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Q)
                                  (CAR
                                   (SIMP
                                    (SUBST 'EXPT 'MY-EXPT
                                           (REVAL1
                                            (PREPF (SUBST 'MY-EXPT 'EXPT Q))
                                            T)))))
                                (CAR Q))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (EVAL '(CLEARRULES '(SOLVEALG-RULES4)))
            U)))) 
(PUT 'SOLVENONLNRSYSLIN 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVENONLNRSYSLIN 'DEFINED-ON-LINE '208) 
(PUT 'SOLVENONLNRSYSLIN 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYSLIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYSLIN (EQS VARS MODE LVARS)
    (PROG (D E E1 LX N S Q X V W W1 NEQS NVARS)
      (SETQ V VARS)
     VAR_LOOP
      (COND ((NULL V) (RETURN '(NOT))))
      (SETQ X (CAR V))
      (SETQ V (CDR V))
      (SETQ W EQS)
      (COND ((MEMBER X LVARS) (GO VAR_LOOP)))
      (SETQ LVARS (CONS X LVARS))
      (SETQ LX (LIST X))
     EQN_LOOP
      (COND ((NULL W) (GO VAR_LOOP)))
      (SETQ E (CAR W))
      (SETQ W (CDR W))
      (COND ((NULL E) (GO EQN_LOOP)))
      (COND ((OR (ATOM E) (ATOM (CAR E))) (RETURN '(INCONSISTENT))))
      ((LAMBDA (KORD*) (SETQ E1 (REORDER E))) LX)
      (COND
       ((OR (NOT (EQUAL (CAAAR E1) X)) (GREATERP (CDAAR E1) 1)
            (SMEMQ X (SETQ D (CDAR E1))) (SMEMQ X (SETQ N (CDR E1))))
        (GO EQN_LOOP)))
      (COND
       ((NOT MODE)
        (PROGN
         (SETQ W NIL)
         (PROG (Y)
           (SETQ Y VARS)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y) (SETQ W (OR W (SMEMQ Y D)))) (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (COND (W (RETURN '(NOT)))))))
      (SETQ N (REORDER N))
      (SETQ D (REORDER D))
      (SETQ S (MULTSQ (CONS (NEGF N) 1) (INVSQ (CONS D 1))))
      (SETQ NEQS
              (PROG (EQN FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQN (DELETE E EQS))
               STARTOVER
                (COND ((NULL EQN) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (EQN)
                           (PROGN
                            (SETQ Q
                                    (CAR
                                     (SUBF EQN (LIST (CONS X (PREPSQ S))))))
                            (COND (Q (LIST Q)))))
                         (CAR EQN)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EQN (CDR EQN))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EQN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (EQN)
                           (PROGN
                            (SETQ Q
                                    (CAR
                                     (SUBF EQN (LIST (CONS X (PREPSQ S))))))
                            (COND (Q (LIST Q)))))
                         (CAR EQN)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EQN (CDR EQN))
                (GO LOOPLABEL)))
      (SETQ NVARS
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y (DELETE X VARS))
               STARTOVER
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (Y) (COND ((SMEMQ Y NEQS) (LIST Y))))
                         (CAR Y)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ Y (CDR Y))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (Y) (COND ((SMEMQ Y NEQS) (LIST Y))))
                         (CAR Y)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ Y (CDR Y))
                (GO LOOPLABEL)))
      (SETQ W
              (COND ((NULL NEQS) '(T (NIL NIL 1)))
                    ((NULL NVARS) '(INCONSISTENT))
                    ((CDR NEQS) (SOLVENONLNRSYS0 NEQS NVARS LVARS))
                    (T (SOLVENONLNRSYSONE (CAR NEQS) (CAR NVARS)))))
      (COND ((EQ (CAR W) 'FAILED) (RETURN W)))
      (SETQ W (ADD-VARIABLE-TO-TAGGED-SOLUTIONS X S W))
      (COND ((OR (OR (ATOM D) (ATOM (CAR D))) (NOT MODE)) (RETURN W)))
      (SETQ W1 (SOLVENONLNRSYS0 (CONS N (CONS D EQS)) VARS LVARS))
      (RETURN (MERGE-TWO-TAGGED-SOLUTIONS W W1)))) 
(PUT 'SOLVENONLNRSYSONE 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRSYSONE 'DEFINED-ON-LINE '255) 
(PUT 'SOLVENONLNRSYSONE 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYSONE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYSONE (F X)
    (PROG (W)
      (SETQ W (SOLVESQ (CONS F 1) X 1))
      (COND ((NULL W) (RETURN '(INCONSISTENT)))
            ((NULL (CADR (CAR W))) (RETURN '(FAILED))))
      (RETURN (CONS T W)))) 
(PUT 'ADD-VARIABLE-TO-TAGGED-SOLUTIONS 'NUMBER-OF-ARGS 3) 
(PUT 'ADD-VARIABLE-TO-TAGGED-SOLUTIONS 'DEFINED-ON-LINE '273) 
(PUT 'ADD-VARIABLE-TO-TAGGED-SOLUTIONS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'ADD-VARIABLE-TO-TAGGED-SOLUTIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADD-VARIABLE-TO-TAGGED-SOLUTIONS (X S Y)
    (COND ((EQCAR Y 'INCONSISTENT) Y)
          ((OR (NULL Y) (NULL (CDR Y))) (LIST T (LIST (LIST S) (LIST X) 1)))
          (T
           (CONS (CAR Y)
                 (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Q (CDR Y))
                   (COND ((NULL Q) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Q)
                                       (LIST (APPEND (CAR Q) (LIST S))
                                             (APPEND (CADR Q) (LIST X))
                                             (CADDR Q)))
                                     (CAR Q))
                                    NIL)))
                  LOOPLABEL
                   (SETQ Q (CDR Q))
                   (COND ((NULL Q) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (Q)
                               (LIST (APPEND (CAR Q) (LIST S))
                                     (APPEND (CADR Q) (LIST X)) (CADDR Q)))
                             (CAR Q))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'MERGE-TWO-TAGGED-SOLUTIONS 'NUMBER-OF-ARGS 2) 
(PUT 'MERGE-TWO-TAGGED-SOLUTIONS 'DEFINED-ON-LINE '281) 
(PUT 'MERGE-TWO-TAGGED-SOLUTIONS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'MERGE-TWO-TAGGED-SOLUTIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MERGE-TWO-TAGGED-SOLUTIONS (W1 W2)
    (COND ((OR (EQUAL (CAR W1) 'FAILED) (EQUAL (CAR W2) 'FAILED)) '(FAILED))
          ((EQUAL (CAR W1) 'INCONSISTENT) W2)
          ((EQUAL (CAR W2) 'INCONSISTENT) W1)
          (T (CONS (CAR W1) (APPEND (CDR W1) (CDR W2)))))) 
(PUT 'SOLVENONLNRSYSSEP 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRSYSSEP 'DEFINED-ON-LINE '289) 
(PUT 'SOLVENONLNRSYSSEP 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYSSEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYSSEP (EQS VARS)
    (PROG (Y R S R0 U W TAG)
      (COND ((NULL VARS) (RETURN '(FAILED)))
            ((NULL (CDR EQS))
             (PROGN
              (COND
               ((NOT (SMEMBER (CAR VARS) (CAR EQS)))
                (RETURN (SOLVENONLNRSYSSEP EQS (CDR VARS)))))
              (SETQ R (SOLVESQ (CONS (CAR EQS) 1) (CAR VARS) 1))
              (RETURN
               (COND ((AND R (CADR (CAR R))) (CONS 'T R)) (T '(FAILED)))))))
      (PROG (X)
        (SETQ X VARS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((NULL Y)
             (PROGN
              (SETQ R NIL)
              (PROG (U)
                (SETQ U EQS)
               LAB
                (COND ((NULL U) (RETURN NIL)))
                ((LAMBDA (U) (COND ((SMEMBER X U) (SETQ R (CONS U R)))))
                 (CAR U))
                (SETQ U (CDR U))
                (GO LAB))
              (COND ((AND R (NULL (CDR R))) (SETQ Y X)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND ((NULL Y) (RETURN '(FAILED))))
      (SETQ R (CAR R))
      (SETQ S (SOLVENONLNRSYS (DELETE R EQS) (DELETE Y VARS)))
      (COND ((EQUAL (CAR S) 'FAILED) (RETURN S)) (T (SETQ S (CDR S))))
      (SETQ TAG T)
      (SETQ U
              (PROG (S0 FORALL-RESULT FORALL-ENDPTR)
                (SETQ S0 S)
               STARTOVER
                (COND ((NULL S0) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S0)
                           (PROGN
                            (SETQ W
                                    (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ Q (PAIR (CADR S0) (CAR S0)))
                                     STARTOVER
                                      (COND ((NULL Q) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              ((LAMBDA (Q)
                                                 (COND
                                                  ((NOT
                                                    (SMEMQ 'ROOT_OF (CDR Q)))
                                                   (LIST
                                                    (CONS (CAR Q)
                                                          (PREPSQ (CDR Q)))))))
                                               (CAR Q)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-RESULT))
                                      (SETQ Q (CDR Q))
                                      (COND
                                       ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                     LOOPLABEL
                                      (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              ((LAMBDA (Q)
                                                 (COND
                                                  ((NOT
                                                    (SMEMQ 'ROOT_OF (CDR Q)))
                                                   (LIST
                                                    (CONS (CAR Q)
                                                          (PREPSQ (CDR Q)))))))
                                               (CAR Q)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-ENDPTR))
                                      (SETQ Q (CDR Q))
                                      (GO LOOPLABEL)))
                            (SETQ R0 (SUBF R W))
                            (SETQ R0 (SOLVESQ R0 Y (CADDR S0)))
                            (COND
                             ((OR (NULL R0) (NULL (CADR (CAR R0))))
                              (SETQ TAG 'FAILED)))
                            (PROG (R1 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ R1 R0)
                              (COND ((NULL R1) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (R1)
                                                  (LIST
                                                   (CONS (CAAR R1) (CAR S0))
                                                   (CONS Y (CADR S0))
                                                   (CADDR R1)))
                                                (CAR R1))
                                               NIL)))
                             LOOPLABEL
                              (SETQ R1 (CDR R1))
                              (COND ((NULL R1) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (R1)
                                          (LIST (CONS (CAAR R1) (CAR S0))
                                                (CONS Y (CADR S0)) (CADDR R1)))
                                        (CAR R1))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR S0)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S0 (CDR S0))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S0) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S0)
                           (PROGN
                            (SETQ W
                                    (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ Q (PAIR (CADR S0) (CAR S0)))
                                     STARTOVER
                                      (COND ((NULL Q) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              ((LAMBDA (Q)
                                                 (COND
                                                  ((NOT
                                                    (SMEMQ 'ROOT_OF (CDR Q)))
                                                   (LIST
                                                    (CONS (CAR Q)
                                                          (PREPSQ (CDR Q)))))))
                                               (CAR Q)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-RESULT))
                                      (SETQ Q (CDR Q))
                                      (COND
                                       ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                     LOOPLABEL
                                      (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              ((LAMBDA (Q)
                                                 (COND
                                                  ((NOT
                                                    (SMEMQ 'ROOT_OF (CDR Q)))
                                                   (LIST
                                                    (CONS (CAR Q)
                                                          (PREPSQ (CDR Q)))))))
                                               (CAR Q)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-ENDPTR))
                                      (SETQ Q (CDR Q))
                                      (GO LOOPLABEL)))
                            (SETQ R0 (SUBF R W))
                            (SETQ R0 (SOLVESQ R0 Y (CADDR S0)))
                            (COND
                             ((OR (NULL R0) (NULL (CADR (CAR R0))))
                              (SETQ TAG 'FAILED)))
                            (PROG (R1 FORALL-RESULT FORALL-ENDPTR)
                              (SETQ R1 R0)
                              (COND ((NULL R1) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (R1)
                                                  (LIST
                                                   (CONS (CAAR R1) (CAR S0))
                                                   (CONS Y (CADR S0))
                                                   (CADDR R1)))
                                                (CAR R1))
                                               NIL)))
                             LOOPLABEL
                              (SETQ R1 (CDR R1))
                              (COND ((NULL R1) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (R1)
                                          (LIST (CONS (CAAR R1) (CAR S0))
                                                (CONS Y (CADR S0)) (CADDR R1)))
                                        (CAR R1))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR S0)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S0 (CDR S0))
                (GO LOOPLABEL)))
      (RETURN (CONS TAG U)))) 
(PUT 'SOLVE-PSYSP 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-PSYSP 'DEFINED-ON-LINE '319) 
(PUT 'SOLVE-PSYSP 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVE-PSYSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-PSYSP (S UV)
    (OR (NULL S) (AND (SOLVE-PSYSP1 (CAR S) UV) (SOLVE-PSYSP (CDR S) UV)))) 
(PUT 'SOLVE-PSYSP1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-PSYSP1 'DEFINED-ON-LINE '323) 
(PUT 'SOLVE-PSYSP1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVE-PSYSP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-PSYSP1 (F UV)
    (OR (OR (ATOM F) (ATOM (CAR F)))
        (AND (OR (MEMBER (CAAAR F) UV) (SOLVE-PSYSP2 (CAAAR F) UV))
             (SOLVE-PSYSP1 (CDAR F) UV) (SOLVE-PSYSP1 (CDR F) UV)))) 
(PUT 'SOLVE-PSYSP2 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVE-PSYSP2 'DEFINED-ON-LINE '328) 
(PUT 'SOLVE-PSYSP2 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVE-PSYSP2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVE-PSYSP2 (V UV)
    (OR (NULL UV) (AND (NOT (SMEMBER (CAR UV) V)) (SOLVE-PSYSP2 V (CDR UV))))) 
(PUT 'SOLVENONLNRSYS1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRSYS1 'DEFINED-ON-LINE '332) 
(PUT 'SOLVENONLNRSYS1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRSYS1 (SYSTEM* UV*)
    (PROG (R RULES)
      (SETQ OSYSTEM* SYSTEM*)
      (COND ((SOLVEALGTRIG0 SYSTEM*) (SETQ RULES '(SOLVEALG-RULES1))))
      (COND
       ((OR (SMEMQ 'TAN SYSTEM*) (SMEMQ 'COT SYSTEM*) (SMEMQ 'TANH SYSTEM*)
            (SMEMQ 'COTH SYSTEM*))
        (SETQ RULES (CONS 'SOLVEALG-RULES2 RULES))))
      (SETQ R (EVALLETSUB2 (LIST RULES '(SOLVENONLNRSYSPRE)) NIL))
      (COND ((ERRORP R) (RETURN '(FAILED))) (T (SETQ SYSTEM* (CAR R))))
      (SETQ R (SOLVENONLNRSYS2))
      (RETURN R))) 
(PUT 'SOLVENONLNRSYSPRE 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVENONLNRSYSPRE 'DEFINED-ON-LINE '345) 
(PUT 'SOLVENONLNRSYSPRE 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYSPRE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVENONLNRSYSPRE NIL
    ((LAMBDA (DMODE*)
       (PROG (P FORALL-RESULT FORALL-ENDPTR)
         (SETQ P SYSTEM*)
         (COND ((NULL P) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (CAR (SIMP (PREPF P)))) (CAR P))
                               NIL)))
        LOOPLABEL
         (SETQ P (CDR P))
         (COND ((NULL P) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (P) (CAR (SIMP (PREPF P)))) (CAR P)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
     NIL)) 
(PUT 'SOLVENLNRSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVENLNRSIMP 'DEFINED-ON-LINE '349) 
(PUT 'SOLVENLNRSIMP 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENLNRSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVENLNRSIMP (U) (SIMP* U)) 
(PUT 'SOLVENONLNRSYS2 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVENONLNRSYS2 'DEFINED-ON-LINE '359) 
(PUT 'SOLVENONLNRSYS2 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRSYS2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVENONLNRSYS2 NIL
    (COND ((NULL *SOLVEALGP) (SETQ SYSTEM* '(FAILED)))
          (T
           ((LAMBDA (DEPL*)
              (PROG (IV* KL* INV* FV* R W *SOLVEALGP SOLVEALGDB* SUB*
                     LAST-VARS* GROEBROOTS* CONST-VARS* ROOT-VARS*)
                (COND
                 ((NOT *VAROPT)
                  (SETQ DEPL*
                          (APPEND (PAIR UV* (APPEND (CDR UV*) (LIST (GENSYM))))
                                  DEPL*))))
                (PROG (F)
                  (SETQ F SYSTEM*)
                 LAB
                  (COND ((NULL F) (RETURN NIL)))
                  ((LAMBDA (F)
                     (SOLVEALGK0
                      (COND (DMODE* ((LAMBDA (DMODE*) (CAR (SUBF F NIL))) NIL))
                            (T F))))
                   (CAR F))
                  (SETQ F (CDR F))
                  (GO LAB))
                (COND (*TRNONLNR (PRINT (LIST "original kernels:" KL*))))
                (COND
                 ((AND (NULL (CDR SYSTEM*)) (NULL (CDR UV*)))
                  (COND
                   ((AND (OR (SMEMQ 'SIN SYSTEM*) (SMEMQ 'COS SYSTEM*))
                         (SETQ R
                                 (SOLVENONLNRTANSUB
                                  (PREPF (SETQ W (CAR SYSTEM*))) (CAR UV*)))
                         (CAR R))
                    (RETURN (SOLVENONLNRTANSOLVE R (CAR UV*) W)))
                   ((AND (OR (SMEMQ 'SINH SYSTEM*) (SMEMQ 'COSH SYSTEM*))
                         (SETQ R
                                 (SOLVENONLNRTANHSUB
                                  (PREPF (SETQ W (CAR SYSTEM*))) (CAR UV*)))
                         (CAR R))
                    (RETURN (SOLVENONLNRTANHSOLVE R (CAR UV*) W))))))
                (COND
                 (((LAMBDA (DMODE*)
                     (ERRORP (ERRORSET '(SOLVEALGK1) *TRNONLNR NIL)))
                   NIL)
                  (RETURN (SETQ SYSTEM* '(FAILED)))))
                (SETQ SYSTEM*
                        (CONS 'LIST
                              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                (SETQ P SYSTEM*)
                                (COND ((NULL P) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (P) (PREPF P))
                                                  (CAR P))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ P (CDR P))
                                (COND ((NULL P) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS ((LAMBDA (P) (PREPF P)) (CAR P))
                                              NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))
                (COND
                 ((NOT (MEMQ 'GROEBNER LOADED-PACKAGES*))
                  (LOAD-PACKAGE 'GROEBNER)))
                (PROG (X)
                  (SETQ X IV*)
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X)
                     (COND
                      ((NOT (MEMBER X LAST-VARS*))
                       (PROG (Y)
                         (SETQ Y LAST-VARS*)
                        LAB
                         (COND ((NULL Y) (RETURN NIL)))
                         ((LAMBDA (Y) (DEPEND1 X Y T)) (CAR Y))
                         (SETQ Y (CDR Y))
                         (GO LAB)))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (SETQ IV* (SORT IV* (FUNCTION DEPENDS)))
                (COND
                 (*TRNONLNR
                  (PROGN
                   (PRIN2T "Entering Groebner for system")
                   (WRITEPRI (MKQUOTE SYSTEM*) 'ONLY)
                   (WRITEPRI (MKQUOTE (CONS 'LIST IV*)) 'ONLY))))
                (SETQ R (LIST SYSTEM* (CONS 'LIST IV*)))
                (SETQ R (GROESOLVEEVAL R))
                (COND
                 (*TRNONLNR
                  (PROGN
                   (PRIN2T "leaving Groebner with intermediate result")
                   (WRITEPRI (MKQUOTE R) 'ONLY)
                   (TERPRI)
                   (TERPRI))))
                (COND ((MEMQ 'SIN SOLVEALGDB*) (SETQ R (SOLVEALGTRIG2 R))))
                (COND ((MEMQ 'SINH SOLVEALGDB*) (SETQ R (SOLVEALGHYP2 R))))
                (SETQ R
                        (COND ((EQUAL R '(LIST)) '(INCONSISTENT))
                              (T (SOLVEALGINV R))))
                (SETQ SYSTEM* R)
                (RETURN R)))
            DEPL*)))) 
(PUT 'SOLVEALGK0 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGK0 'DEFINED-ON-LINE '405) 
(PUT 'SOLVEALGK0 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGK0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGK0 (P)
    (COND ((OR (ATOM P) (ATOM (CAR P))) NIL)
          (T
           (PROGN
            (COND
             ((AND (NOT (MEMBER (CAAAR P) KL*)) (NOT (MEMBER (CAAAR P) IV*)))
              (SETQ KL* (CONS (CAAAR P) KL*))))
            (SOLVEALGK0 (CDAR P))
            (SOLVEALGK0 (CDR P)))))) 
(PUT 'SOLVEALGK1 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVEALGK1 'DEFINED-ON-LINE '412) 
(PUT 'SOLVEALGK1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGK1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVEALGK1 NIL
    (PROG (K KL0 KL1)
      (SETQ K (CAR KL*))
      (PROG ()
       WHILELABEL
        (COND ((NOT K) (RETURN NIL)))
        (PROGN
         (SETQ KL0 (CONS K KL0))
         (SOLVEALGK2 K)
         (SETQ KL1 KL*)
         (SETQ K NIL)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND KL1 (NULL K))) (RETURN NIL)))
           (COND ((NOT (MEMBER (CAR KL1) KL0)) (SETQ K (CAR KL1)))
                 (T (SETQ KL1 (CDR KL1))))
           (GO WHILELABEL)))
        (GO WHILELABEL)))) 
(PUT 'SOLVEALGK2 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGK2 'DEFINED-ON-LINE '425) 
(PUT 'SOLVEALGK2 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGK2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGK2 (K)
    ((LAMBDA (X)
       (COND ((MEMBER K UV*) (AND (SOLVEALGVB0 K) (SETQ IV* (CONS K IV*))))
             ((ATOM K) T) ((EQ (CAR K) 'EXPT) (SOLVEALGEXPT K X))
             ((MEMQ (CAR K) '(SIN COS TAN COT)) (SOLVEALGTRIG K X))
             ((MEMQ (CAR K) '(SINH COSH TANH COTH)) (SOLVEALGHYP K X))
             ((NULL X) T) (T (SOLVEALGGEN K X))))
     (SOLVEALGTEST K))) 
(PUT 'SOLVEALGTEST 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGTEST 'DEFINED-ON-LINE '434) 
(PUT 'SOLVEALGTEST 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGTEST (K) (COND ((ATOM K) NIL) (T (SOLVEALGTEST0 K)))) 
(PUT 'SOLVEALGTEST0 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGTEST0 'DEFINED-ON-LINE '439) 
(PUT 'SOLVEALGTEST0 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTEST0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGTEST0 (K) (OR (SOLVEALGTEST1 K IV*) (SOLVEALGTEST1 K UV*))) 
(PUT 'SOLVEALGTEST1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGTEST1 'DEFINED-ON-LINE '443) 
(PUT 'SOLVEALGTEST1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTEST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGTEST1 (K KL)
    (COND ((NULL KL) NIL) ((MEMBER K KL) (LIST K)) ((ATOM K) NIL)
          (T
           (UNION (COND ((SMEMBER (CAR KL) (CDR K)) (LIST (CAR KL))) (T NIL))
                  (SOLVEALGTEST1 K (CDR KL)))))) 
(PUT 'SOLVEALGVB 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGVB 'DEFINED-ON-LINE '452) 
(PUT 'SOLVEALGVB 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGVB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGVB (K) (SETQ FV* (APPEND (SOLVEALGVB0 K) FV*))) 
(PUT 'SOLVEALGVB0 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGVB0 'DEFINED-ON-LINE '458) 
(PUT 'SOLVEALGVB0 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGVB0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGVB0 (K)
    (PROG (AK)
      (SETQ AK (ALLKERNELS K NIL))
      (COND
       ((OR (INTERSECTION AK IV*) (INTERSECTION AK FV*))
        (ERROR 99 (LIST "transcendental variable dependency from" K))))
      (RETURN AK))) 
(PUT 'ALLKERNELS 'NUMBER-OF-ARGS 2) 
(PUT 'ALLKERNELS 'DEFINED-ON-LINE '466) 
(PUT 'ALLKERNELS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'ALLKERNELS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALLKERNELS (A KL)
    (COND ((NUMBERP A) KL)
          ((OR (ATOM A) (MEMBER A UV*))
           (COND ((NOT (MEMBER A KL)) (CONS A KL)) (T KL)))
          (T
           (PROGN
            (PROG (X)
              (SETQ X (CDR A))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 ((LAMBDA (S)
                    (SETQ KL (ALLKERNELS1 (CAR S) (ALLKERNELS1 (CDR S) KL))))
                  (SIMP X)))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            KL)))) 
(PUT 'ALLKERNELS1 'NUMBER-OF-ARGS 2) 
(PUT 'ALLKERNELS1 'DEFINED-ON-LINE '476) 
(PUT 'ALLKERNELS1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'ALLKERNELS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALLKERNELS1 (F KL)
    (COND ((OR (ATOM F) (ATOM (CAR F))) KL)
          (T
           (PROGN
            (COND
             ((NOT (MEMBER (CAAAR F) KL))
              (SETQ KL (ALLKERNELS (CAAAR F) (CONS (CAAAR F) KL)))))
            (ALLKERNELS1 (CDAR F) (ALLKERNELS1 (CDR F) KL)))))) 
(PUT 'SOLVEALGEXPT 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGEXPT 'DEFINED-ON-LINE '482) 
(PUT 'SOLVEALGEXPT 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGEXPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGEXPT (K X)
    ((LAMBDA (M)
       (COND
        ((AND (EQCAR M 'QUOTIENT) (FIXP (CADDR M)))
         (COND ((EQUAL (CADR M) 1) (SOLVEALGRAD (CADR K) (CADDR M) X))
               (T (SOLVEALGRADX (CADR K) (CADR M) (CADDR M) X))))
        ((NULL X) (SOLVEALGID K))
        (((LAMBDA (W)
            (AND (NULL (INTERSECTION W UV*)) (NULL (INTERSECTION W IV*))
                 (NULL (INTERSECTION W FV*))))
          (ALLKERNELS M NIL))
         (SOLVEALGGEN K X))
        (T (SOLVEALGEXPTGEN K X))))
     (CADDR K))) 
(PUT 'SOLVEALGEXPTGEN 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGEXPTGEN 'DEFINED-ON-LINE '496) 
(PUT 'SOLVEALGEXPTGEN 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGEXPTGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGEXPTGEN (K X)
    (PROG (BAS XP NV)
      (SETQ BAS (CADR K))
      (SETQ XP (CADDR K))
      (COND ((SOLVEALGTEST1 XP UV*) (RETURN (SOLVEALGEXPTGEN1 K X)))
            ((SOLVEALGTEST1 BAS UV*) (RETURN (SOLVEALGGEN K X))))
      (SETQ NV
              '(((EXPT &ALPHA N)) (&BETA) (((EXPT &ALPHA N) . &BETA))
                ((&BETA (EXPT &ALPHA N) &)) NIL))
      (SETQ NV (SUBST BAS '&ALPHA NV))
      (SETQ NV (SUBST (SOLVE-GENSYM) '&BETA NV))
      (SETQ NV (SUBST XP 'N NV))
      (RETURN (SOLVEALGUPD NV NIL)))) 
(PUT 'SOLVE-GENSYM 'NUMBER-OF-ARGS 0) 
(PUT 'SOLVE-GENSYM 'DEFINED-ON-LINE '521) 
(PUT 'SOLVE-GENSYM 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVE-GENSYM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SOLVE-GENSYM NIL
    (PROG (W)
      (SETQ W (EXPLODE SOLVE-GENSYMCOUNTER))
      (SETQ SOLVE-GENSYMCOUNTER (PLUS SOLVE-GENSYMCOUNTER 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP (LENGTH W) 4)) (RETURN NIL)))
        (SETQ W (CONS '|0| W))
        (GO WHILELABEL))
      (RETURN (COMPRESS (APPEND SOLVE-GENSYMPREFIX W))))) 
(PUT 'SOLVEALGEXPTGEN1 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGEXPTGEN1 'DEFINED-ON-LINE '530) 
(PUT 'SOLVEALGEXPTGEN1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGEXPTGEN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGEXPTGEN1 (K X)
    (PROG (BAS XP XPL Q R NK SUB)
      (SETQ BAS (CADR K))
      (SETQ XP (CADDR K))
      (SETQ XPL (LIST (CONS (CONS 1 1) XP)))
      (PROG (K)
        (SETQ K KL*)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (COND
            ((AND (EQCAR K 'EXPT) (EQUAL (CADR K) BAS)
                  (PROGN
                   (SETQ Q (SIMP (LIST 'QUOTIENT (SETQ R (CADDR K)) XP)))
                   (AND (FIXP (CAR Q)) (FIXP (CDR Q)))))
             (PROGN
              (SETQ KL* (DELETE K KL*))
              (SETQ XPL (CONS (CONS Q R) XPL))))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (SETQ Q 1)
      (PROG (E)
        (SETQ E XPL)
       LAB
        (COND ((NULL E) (RETURN NIL)))
        ((LAMBDA (E) (SETQ Q (LCM Q (CDR (CAR E))))) (CAR E))
        (SETQ E (CDR E))
        (GO LAB))
      (SETQ NK (REVAL1 (LIST 'EXPT BAS (LIST 'QUOTIENT XP Q)) T))
      (SETQ SUB
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E XPL)
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (E)
                                    (CONS (LIST 'EXPT BAS (CDR E))
                                          (LIST 'EXPT NK
                                                (TIMES (CAR (CAR E))
                                                       (QUOTIENT Q
                                                                 (CDR
                                                                  (CAR E)))))))
                                  (CAR E))
                                 NIL)))
               LOOPLABEL
                (SETQ E (CDR E))
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (E)
                            (CONS (LIST 'EXPT BAS (CDR E))
                                  (LIST 'EXPT NK
                                        (TIMES (CAR (CAR E))
                                               (QUOTIENT Q (CDR (CAR E)))))))
                          (CAR E))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SYSTEM* (SUBLIS SUB SYSTEM*))
      (RETURN (SOLVEALGGEN NK X)))) 
(PUT 'SOLVEALGRADX 'NUMBER-OF-ARGS 4) 
(PUT 'SOLVEALGRADX 'DEFINED-ON-LINE '555) 
(PUT 'SOLVEALGRADX 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGRADX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGRADX (X M N Y)
    (SOLVEALGEXPTGEN1 (LIST 'EXPT X (LIST 'QUOTIENT M N)) Y)) 
(PUT 'SOLVEALGRAD 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVEALGRAD 'DEFINED-ON-LINE '559) 
(PUT 'SOLVEALGRAD 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGRAD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGRAD (X N Y)
    (PROG (NV M &BETA)
      (SETQ &BETA (SOLVE-GENSYM))
      (SETQ NV
              '(((EXPT &ALPHA (QUOTIENT 1 &N))) (&BETA)
                (((EXPT &ALPHA (QUOTIENT 1 &N)) . &BETA)) NIL
                ((DIFFERENCE (EXPT &BETA &N) &ALPHA))))
      (SETQ M (LIST (CONS '&ALPHA X) (CONS '&BETA &BETA) (CONS '&N N)))
      (SETQ NV (SUBLA M NV))
      (SETQ ROOT-VARS* (CONS &BETA ROOT-VARS*))
      (COND
       ((OR (NULL Y) (EQUAL Y (LIST X)))
        (SETQ GROEBROOTS*
                (CONS
                 (CONS
                  (LIST 'PLUS (LIST 'EXPT &BETA N) (REVAL1 (LIST 'MINUS X) T))
                  (LIST
                   (LIST
                    (LIST 'EQUAL &BETA (LIST 'EXPT X (LIST 'QUOTIENT 1 N))))))
                 GROEBROOTS*))))
      (COND ((NULL Y) (SETQ LAST-VARS* (CONS &BETA LAST-VARS*))))
      (RETURN (SOLVEALGUPD NV Y)))) 
(PUT 'SOLVEALGTRIG0 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGTRIG0 'DEFINED-ON-LINE '586) 
(PUT 'SOLVEALGTRIG0 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTRIG0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGTRIG0 (F)
    (PROG (ARGS R C)
      (SETQ ARGS
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A (SOLVEALGTRIG01 F NIL))
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    ((LAMBDA (Q)
                                       (UNION (KERNELS (CAR Q))
                                              (KERNELS (CDR Q))))
                                     (SIMP A)))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            ((LAMBDA (Q)
                               (UNION (KERNELS (CAR Q)) (KERNELS (CDR Q))))
                             (SIMP A)))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT ARGS) (RETURN NIL)))
        (PROGN
         (SETQ C (CAR ARGS))
         (SETQ ARGS (CDR ARGS))
         (PROG (Q)
           (SETQ Q ARGS)
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q) (SETQ R (OR R (INTERSECTION C Q)))) (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'SOLVEALGTRIG01 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGTRIG01 'DEFINED-ON-LINE '596) 
(PUT 'SOLVEALGTRIG01 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTRIG01 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGTRIG01 (F ARGS)
    (COND ((ATOM F) ARGS)
          ((MEMQ (CAR F) '(SIN COS TAN COT SINH COSH TANH COTH))
           (COND ((CONSTANT_EXPRP (CADR F)) ARGS)
                 (T (UNION (LIST (CADR F)) ARGS))))
          (T (SOLVEALGTRIG01 (CDR F) (SOLVEALGTRIG01 (CAR F) ARGS))))) 
(AEVAL (OPERATOR (LIST 'P_SIGN 'THE_1))) 
(AEVAL
 (LET
  '((REPLACEBY (P_SIGN (~ X))
     (COND ((EVALEQUAL (AEVAL (LIST 'SIGN 'X)) 0) 1)
           (T (AEVAL (LIST 'SIGN 'X)))))))) 
(AEVAL (LET '((REPLACEBY (THE_1 (~ X)) 1)))) 
(PUT 'SOLVEALGTRIG 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGTRIG 'DEFINED-ON-LINE '607) 
(PUT 'SOLVEALGTRIG 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTRIG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGTRIG (K X)
    (PROG (NV M S &ALPHA &BETA)
      (SETQ SOLVEALGDB* (UNION '(SIN) SOLVEALGDB*))
      (COND
       (X
        (COND ((CDR X) (ERROR 99 "too many variables in trig. function"))
              (T (SETQ X (CAR X))))))
      (SOLVEALGVB K)
      (SETQ NV
              '(((SIN &ALPHA) (COS &ALPHA) (TAN &ALPHA) (COT &ALPHA))
                ((SIN &BETA) (COS &BETA))
                (((SIN &ALPHA) SIN &BETA) ((COS &ALPHA) COS &BETA))
                (((SIN &BETA)
                  (COND ((AND *EXPLI (TEST_TRIG)) '(&LOC (P_SIGN (&& &))))
                        (T '(&X (&& (ROOT_OF (EQUAL (SIN &ALPHA) &) &X))))))
                 ((COS &BETA)
                  (COND
                   ((AND *EXPLI (TEST_TRIG))
                    '(&X (PLUS (&& (TIMES &LOC (ACOS &))) (TIMES 2 PI &ARB))))
                   (T '(&X (&& (ROOT_OF (EQUAL (COS &ALPHA) &) &X)))))))
                ((PLUS (EXPT (SIN &BETA) 2) (EXPT (COS &BETA) 2) -1))))
      (SETQ S (COND (X (SOLVEALGINNER (CADR K) X)) (T 'THE_1)))
      (SETQ &BETA (SOLVE-GENSYM))
      (SETQ M
              (LIST (CONS '&ALPHA (SETQ &ALPHA (CADR K))) (CONS '&BETA &BETA)
                    (CONS '&LOC (SOLVE-GENSYM))
                    (CONS '&ARB (LIST 'ARBINT (SETQ !ARBINT (PLUS !ARBINT 1))))
                    (CONS '&X X) (CONS '&& S)))
      (SETQ NV (SUBLIS-PAT M NV))
      (COND
       (X
        (SETQ LAST-VARS*
                (APPEND LAST-VARS*
                        (LIST (LIST 'SIN &BETA) (LIST 'COS &BETA)))))
       (T
        (SETQ CONST-VARS*
                (APPEND CONST-VARS*
                        (LIST (CONS (LIST 'SIN &BETA) (LIST 'SIN &ALPHA))
                              (CONS (LIST 'COS &BETA) (LIST 'COS &ALPHA)))))))
      (RETURN (SOLVEALGUPD NV NIL)))) 
(PUT 'SOLVEALGHYP 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGHYP 'DEFINED-ON-LINE '661) 
(PUT 'SOLVEALGHYP 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGHYP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGHYP (K X)
    (PROG (NV M S &ALPHA &BETA)
      (SETQ SOLVEALGDB* (UNION '(SINH) SOLVEALGDB*))
      (COND
       (X
        (COND ((CDR X) (ERROR 99 "too many variables in hyp. function"))
              (T (SETQ X (CAR X))))))
      (SOLVEALGVB K)
      (SETQ NV
              '(((SINH &ALPHA) (COSH &ALPHA) (TANH &ALPHA) (COTH &ALPHA))
                ((SINH &BETA) (COSH &BETA))
                (((SINH &ALPHA) SINH &BETA) ((COSH &ALPHA) COSH &BETA))
                (((SINH &BETA)
                  (COND ((AND *EXPLI (TEST_HYP)) '(&LOC (P_SIGN (&& &))))
                        (T '(&X (&& (ROOT_OF (EQUAL (SINH &ALPHA) &) &X))))))
                 ((COSH &BETA)
                  (COND
                   ((AND *EXPLI (TEST_HYP))
                    '(&X
                      (PLUS (&& (TIMES &LOC (ACOSH &))) (TIMES 2 PI I &ARB))))
                   (T '(&X (&& (ROOT_OF (EQUAL (COSH &ALPHA) &) &X)))))))
                ((PLUS (MINUS (EXPT (SINH &BETA) 2)) (EXPT (COSH &BETA) 2)
                       -1))))
      (SETQ S (COND (X (SOLVEALGINNER (CADR K) X)) (T 'THE_1)))
      (SETQ &BETA (SOLVE-GENSYM))
      (SETQ M
              (LIST (CONS '&ALPHA (SETQ &ALPHA (CADR K))) (CONS '&BETA &BETA)
                    (CONS '&LOC (SOLVE-GENSYM))
                    (CONS '&ARB (LIST 'ARBINT (SETQ !ARBINT (PLUS !ARBINT 1))))
                    (CONS '&X X) (CONS '&& S)))
      (SETQ NV (SUBLIS-PAT M NV))
      (COND
       (X
        (SETQ LAST-VARS*
                (APPEND LAST-VARS*
                        (LIST (LIST 'SINH &BETA) (LIST 'COSH &BETA)))))
       (T
        (SETQ CONST-VARS*
                (APPEND CONST-VARS*
                        (LIST (CONS (LIST 'SINH &BETA) (LIST 'SINH &ALPHA))
                              (CONS (LIST 'COSH &BETA)
                                    (LIST 'COSH &ALPHA)))))))
      (RETURN (SOLVEALGUPD NV NIL)))) 
(PUT 'SOLVEALGTRIG2 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGTRIG2 'DEFINED-ON-LINE '713) 
(PUT 'SOLVEALGTRIG2 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGTRIG2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGTRIG2 (U)
    (PROG (R W OP V RH)
      (PROG (S)
        (SETQ S (CDR U))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ W NIL)
            (PROG (E)
              (SETQ E S)
             LAB
              (COND ((NULL E) (RETURN NIL)))
              ((LAMBDA (E)
                 (COND
                  ((AND (EQCAR E 'EQUAL)
                        (OR (EQCAR (CADR E) 'SIN) (EQCAR (CADR E) 'COS))
                        (SETQ OP (CAADR E)) (SETQ V (CADR (CADR E)))
                        (MEMBER
                         (COND ((EQCAR (SETQ RH (CADDR E)) '*SQ*) (CADR RH))
                               (T RH))
                         (SUBST (LIST (COND ((EQUAL OP 'SIN) 'COS) (T 'SIN)) V)
                                '-FORM-
                                '((MINUS
                                   (SQRT (PLUS (MINUS (EXPT -FORM- 2)) 1)))
                                  (SQRT (PLUS (MINUS (EXPT -FORM- 2)) 1))))))
                   NIL)
                  (T (SETQ W (CONS E W)))))
               (CAR E))
              (SETQ E (CDR E))
              (GO LAB))
            (SETQ W (REVERSE W))
            (COND ((NOT (MEMBER W R)) (SETQ R (CONS W R))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RETURN (CONS 'LIST (REVERSE R))))) 
(PUT 'SOLVEALGHYP2 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGHYP2 'DEFINED-ON-LINE '734) 
(PUT 'SOLVEALGHYP2 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGHYP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGHYP2 (U)
    (PROG (R W OP V RH)
      (PROG (S)
        (SETQ S (CDR U))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ W NIL)
            (PROG (E)
              (SETQ E S)
             LAB
              (COND ((NULL E) (RETURN NIL)))
              ((LAMBDA (E)
                 (COND
                  ((AND (EQCAR E 'EQUAL)
                        (OR (EQCAR (CADR E) 'SINH) (EQCAR (CADR E) 'COSH))
                        (SETQ OP (CAADR E)) (SETQ V (CADR (CADR E)))
                        (MEMBER
                         (COND ((EQCAR (SETQ RH (CADDR E)) '*SQ*) (CADR RH))
                               (T RH))
                         (COND
                          ((EQUAL OP 'SINH)
                           (SUBST (LIST 'COSH V) '-FORM-
                                  '((MINUS (SQRT (PLUS (EXPT -FORM- 2) 1)))
                                    (SQRT (PLUS (EXPT -FORM- 2) 1)))))
                          (T
                           (SUBST (LIST 'SINH V) '-FORM-
                                  '((MINUS
                                     (SQRT (PLUS (EXPT -FORM- 2) (MINUS 1))))
                                    (SQRT
                                     (PLUS (EXPT -FORM- 2) (MINUS 1)))))))))
                   NIL)
                  (T (SETQ W (CONS E W)))))
               (CAR E))
              (SETQ E (CDR E))
              (GO LAB))
            (SETQ W (REVERSE W))
            (COND ((NOT (MEMBER W R)) (SETQ R (CONS W R))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RETURN (CONS 'LIST (REVERSE R))))) 
(PUT 'SOLVEALGGEN 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGGEN 'DEFINED-ON-LINE '760) 
(PUT 'SOLVEALGGEN 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGGEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGGEN (K X)
    (PROG (NV M S)
      (COND ((CDR X) (ERROR 99 "too many variables in function expression")))
      (SETQ X (CAR X))
      (SOLVEALGVB K)
      (SETQ NV
              '((&ALPHA) (&BETA) ((&ALPHA . &BETA)) ((&BETA '(&X (&& &))))
                NIL))
      (SETQ S (SOLVEALGINNER K X))
      (SETQ M
              (LIST (CONS '&ALPHA K) (CONS '&BETA (SOLVE-GENSYM)) (CONS '&X X)
                    (CONS '&& S)))
      (SETQ NV (SUBLIS-PAT M NV))
      (RETURN (SOLVEALGUPD NV NIL)))) 
(PUT 'SOLVEALGID 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGID 'DEFINED-ON-LINE '788) 
(PUT 'SOLVEALGID 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGID (K)
    (PROG (NV M)
      (SETQ NV '((&ALPHA) NIL ((&ALPHA . &BETA)) ((&BETA NIL . &ALPHA)) NIL))
      (SETQ M (LIST (CONS '&ALPHA K) (CONS '&BETA (SOLVE-GENSYM))))
      (SETQ NV (SUBLIS M NV))
      (RETURN (SOLVEALGUPD NV NIL)))) 
(PUT 'SOLVEALGINNER 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGINNER 'DEFINED-ON-LINE '808) 
(PUT 'SOLVEALGINNER 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGINNER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGINNER (S X)
    (PROGN
     (SETQ S (SOLVEEVAL1 (LIST (LIST 'EQUAL S '|#|) (LIST 'LIST X))))
     (SETQ S (REVAL1 (CADR S) T))
     (COND
      ((OR (NOT (EQCAR S 'EQUAL)) (NOT (EQUAL (CADR S) X)))
       (ERROR 99 "inner expression cannot be inverted")))
     (LIST 'LAMBDA '(|#|) (CADDR S)))) 
(PUT 'SOLVEALGUPD 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGUPD 'DEFINED-ON-LINE '815) 
(PUT 'SOLVEALGUPD 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGUPD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGUPD (U INNERVARS)
    (PROG (OV NV SUB INV NEQS)
      (SETQ OV (CAR U))
      (SETQ U (CDR U))
      (SETQ NV (CAR U))
      (SETQ U (CDR U))
      (SETQ SUB (CAR U))
      (SETQ U (CDR U))
      (SETQ INV (CAR U))
      (SETQ U (CDR U))
      (SETQ NEQS (CAR U))
      (SETQ U (CDR U))
      (PROG (X)
        (SETQ X OV)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ KL* (DELETE X KL*))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (X)
        (SETQ X INNERVARS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (Y)
             (SETQ Y NV)
            LAB
             (COND ((NULL Y) (RETURN NIL)))
             ((LAMBDA (Y) (DEPEND1 Y X T)) (CAR Y))
             (SETQ Y (CDR Y))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ SUB* (APPEND SUB SUB*))
      (SETQ IV* (APPEND NV IV*))
      (SETQ INV* (APPEND INV INV*))
      (SETQ SYSTEM*
              (APPEND
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U NEQS)
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U)
                                     (PROGN
                                      (SETQ U (CAR (SIMP U)))
                                      (SOLVEALGK0 U)
                                      U))
                                   (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (U)
                             (PROGN (SETQ U (CAR (SIMP U))) (SOLVEALGK0 U) U))
                           (CAR U))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U SYSTEM*)
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U) (CAR (SUBF U SUB))) (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (U) (CAR (SUBF U SUB))) (CAR U)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN T))) 
(PUT 'SOLVEALGINV 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALGINV 'DEFINED-ON-LINE '832) 
(PUT 'SOLVEALGINV 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGINV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALGINV (U)
    (PROG (V R S M LH RH Y Z TAG SUB0 SUB *EXPLI NOARB ARBS ABORT N)
      (SETQ N 0)
      (SETQ SUB0
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P SUB*)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (CONS (CDR P) (CAR P))) (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CONS (CDR P) (CAR P))) (CAR P))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TAG T)
      (SETQ R
              (PROG (SOL FORALL-RESULT FORALL-ENDPTR)
                (SETQ SOL (CDR U))
               STARTOVER
                (COND ((NULL SOL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (SOL)
                           (PROGN
                            (SETQ SUB SUB0)
                            (SETQ ABORT
                                    (SETQ V
                                            (SETQ R
                                                    (SETQ S
                                                            (SETQ NOARB
                                                                    (SETQ ARBS
                                                                            NIL))))))
                            (COND
                             (*TEST_SOLVEALG
                              (PROGN
                               (PRIN2T "================================")
                               (PRIN2T CONST-VARS*)
                               (PRIN2T " next basis:")
                               (WRITEPRI (MKQUOTE SOL) 'ONLY))))
                            (PROG (EQN)
                              (SETQ EQN (REVERSE (CDR SOL)))
                             LAB
                              (COND ((NULL EQN) (RETURN NIL)))
                              ((LAMBDA (EQN)
                                 (PROGN
                                  (SETQ LH (CADR EQN))
                                  (SETQ RH (SUBSQ (SIMP* (CADDR EQN)) S))
                                  (COND
                                   (*TEST_SOLVEALG
                                    (WRITEPRI
                                     (MKQUOTE (LIST 'EQUAL LH (PREPSQ RH)))
                                     'ONLY)))
                                  (SETQ *EXPLI (MEMBER LH IV*))
                                  (COND
                                   ((AND (SETQ Y (ASSOC LH CONST-VARS*))
                                         (CONSTANT_EXPRP (PREPSQ RH))
                                         (CAR
                                          (ADDSQ RH (NEGSQ (SIMP (CDR Y))))))
                                    (SETQ ABORT T)))
                                  (COND
                                   ((AND (MEMQ LH ROOT-VARS*)
                                         (NUMBERP
                                          (SETQ Y
                                                  (REVAL1
                                                   (LIST 'SIGN (PREPSQ RH))
                                                   T)))
                                         (LESSP Y 0))
                                    (SETQ ABORT T)))
                                  (COND ((NOT *EXPLI) (SETQ NOARB T)))
                                  (COND
                                   ((AND *EXPLI (NOT NOARB))
                                    (PROGN
                                     (PROG (X)
                                       (SETQ X UV*)
                                      LAB
                                       (COND ((NULL X) (RETURN NIL)))
                                       ((LAMBDA (X)
                                          (COND
                                           ((AND *ARBVARS
                                                 (SOLVEALGDEPENDS RH X)
                                                 (NOT (MEMBER X FV*))
                                                 (NOT (MEMBER X ARBS)))
                                            (PROGN
                                             (SETQ Z (CAAAR (MAKEARBCOMPLEX)))
                                             (SETQ Y Z)
                                             (SETQ V (CONS X V))
                                             (SETQ R (CONS (SIMP Y) R))
                                             (SETQ ARBS (CONS X ARBS))))))
                                        (CAR X))
                                       (SETQ X (CDR X))
                                       (GO LAB))
                                     (COND
                                      ((NOT (SMEMQ 'ROOT_OF RH))
                                       (SETQ S (CONS (CONS LH (PREPSQ RH)) S)))
                                      (T (SETQ FV* (CONS LH FV*)))))))
                                  (COND
                                   ((SETQ M (ASSOC LH INV*))
                                    (PROGN
                                     (SETQ M (CDR M))
                                     (SETQ LH (CAR M))
                                     (SETQ KL* EQN)
                                     (COND
                                      ((OR (EQCAR LH 'COND) (EQCAR LH 'QUOTE))
                                       (SETQ LH (CAR (SETQ M (LISPEVAL LH))))))
                                     (SETQ RH (SUBST (PREPSQ RH) '& (CADR M)))
                                     ((LAMBDA (*PROTFG)
                                        (SETQ RH
                                                (ERRORSET
                                                 (LIST 'SOLVENLNRSIMP
                                                       (MKQUOTE RH))
                                                 *TRNONLNR NIL)))
                                      T)
                                     (COND ((ERRORP RH) (PROGN (SETQ ABORT T)))
                                           (T (SETQ RH (CAR RH)))))))
                                  (COND
                                   ((NOT ABORT)
                                    (PROGN
                                     (COND
                                      ((AND (NOT (MEMBER LH UV*)) *EXPLI)
                                       (PROGN
                                        (SETQ SUB
                                                (APPEND SUB
                                                        (LIST
                                                         (CONS LH
                                                               (SETQ Z
                                                                       (PREPSQ
                                                                        (SUBSQ
                                                                         RH
                                                                         SUB)))))))
                                        (COND
                                         ((SMEMBER LH R)
                                          (SETQ R (SUBST Z LH R)))))))
                                     (COND
                                      ((AND (OR (MEMBER LH UV*) (NOT *EXPLI))
                                            (NOT
                                             (PROGN
                                              (SETQ Z (SUBSQ RH SUB))
                                              (SETQ N (LENGTH (MEMBER Z R)))
                                              (AND (GREATERP N 0)
                                                   (EQUAL LH
                                                          (NTH V
                                                               (PLUS (LENGTH V)
                                                                     (DIFFERENCE
                                                                      1
                                                                      N))))))))
                                       (PROGN
                                        (SETQ R (CONS Z R))
                                        (SETQ V (CONS LH V))
                                        NIL))))))))
                               (CAR EQN))
                              (SETQ EQN (CDR EQN))
                              (GO LAB))
                            (PROG (X)
                              (SETQ X UV*)
                             LAB
                              (COND ((NULL X) (RETURN NIL)))
                              ((LAMBDA (X)
                                 (COND
                                  ((NOT (MEMBER X V))
                                   (COND
                                    ((NOT (SMEMBER X R))
                                     (PROGN
                                      (SETQ Z (CAAAR (MAKEARBCOMPLEX)))
                                      (SETQ Y Z)
                                      (SETQ V (CONS X V))
                                      (SETQ R (CONS (SIMP Y) R))
                                      (SETQ ARBS (CONS X ARBS))))))))
                               (CAR X))
                              (SETQ X (CDR X))
                              (GO LAB))
                            (COND
                             (*TEST_SOLVEALG
                              (COND (ABORT (YESP "aborted"))
                                    (T
                                     (PROGN
                                      (PRIN2T " --------> ")
                                      (WRITEPRI
                                       (MKQUOTE
                                        (CONS 'LIST
                                              (PROG (U FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ U (PAIR V R))
                                                (COND ((NULL U) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (U)
                                                                    (LIST
                                                                     'EQUAL
                                                                     (CAR U)
                                                                     (PREPSQ
                                                                      (CDR
                                                                       U))))
                                                                  (CAR U))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ U (CDR U))
                                                (COND
                                                 ((NULL U)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (U)
                                                            (LIST 'EQUAL
                                                                  (CAR U)
                                                                  (PREPSQ
                                                                   (CDR U))))
                                                          (CAR U))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                       'ONLY)
                                      (PRIN2T
                                       "================================")
                                      (YESP "continue?"))))))
                            (COND
                             ((NOT ABORT)
                              (LIST (CONS (REVERSE R) (REVERSE V)))))))
                         (CAR SOL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ SOL (CDR SOL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL SOL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (SOL)
                           (PROGN
                            (SETQ SUB SUB0)
                            (SETQ ABORT
                                    (SETQ V
                                            (SETQ R
                                                    (SETQ S
                                                            (SETQ NOARB
                                                                    (SETQ ARBS
                                                                            NIL))))))
                            (COND
                             (*TEST_SOLVEALG
                              (PROGN
                               (PRIN2T "================================")
                               (PRIN2T CONST-VARS*)
                               (PRIN2T " next basis:")
                               (WRITEPRI (MKQUOTE SOL) 'ONLY))))
                            (PROG (EQN)
                              (SETQ EQN (REVERSE (CDR SOL)))
                             LAB
                              (COND ((NULL EQN) (RETURN NIL)))
                              ((LAMBDA (EQN)
                                 (PROGN
                                  (SETQ LH (CADR EQN))
                                  (SETQ RH (SUBSQ (SIMP* (CADDR EQN)) S))
                                  (COND
                                   (*TEST_SOLVEALG
                                    (WRITEPRI
                                     (MKQUOTE (LIST 'EQUAL LH (PREPSQ RH)))
                                     'ONLY)))
                                  (SETQ *EXPLI (MEMBER LH IV*))
                                  (COND
                                   ((AND (SETQ Y (ASSOC LH CONST-VARS*))
                                         (CONSTANT_EXPRP (PREPSQ RH))
                                         (CAR
                                          (ADDSQ RH (NEGSQ (SIMP (CDR Y))))))
                                    (SETQ ABORT T)))
                                  (COND
                                   ((AND (MEMQ LH ROOT-VARS*)
                                         (NUMBERP
                                          (SETQ Y
                                                  (REVAL1
                                                   (LIST 'SIGN (PREPSQ RH))
                                                   T)))
                                         (LESSP Y 0))
                                    (SETQ ABORT T)))
                                  (COND ((NOT *EXPLI) (SETQ NOARB T)))
                                  (COND
                                   ((AND *EXPLI (NOT NOARB))
                                    (PROGN
                                     (PROG (X)
                                       (SETQ X UV*)
                                      LAB
                                       (COND ((NULL X) (RETURN NIL)))
                                       ((LAMBDA (X)
                                          (COND
                                           ((AND *ARBVARS
                                                 (SOLVEALGDEPENDS RH X)
                                                 (NOT (MEMBER X FV*))
                                                 (NOT (MEMBER X ARBS)))
                                            (PROGN
                                             (SETQ Z (CAAAR (MAKEARBCOMPLEX)))
                                             (SETQ Y Z)
                                             (SETQ V (CONS X V))
                                             (SETQ R (CONS (SIMP Y) R))
                                             (SETQ ARBS (CONS X ARBS))))))
                                        (CAR X))
                                       (SETQ X (CDR X))
                                       (GO LAB))
                                     (COND
                                      ((NOT (SMEMQ 'ROOT_OF RH))
                                       (SETQ S (CONS (CONS LH (PREPSQ RH)) S)))
                                      (T (SETQ FV* (CONS LH FV*)))))))
                                  (COND
                                   ((SETQ M (ASSOC LH INV*))
                                    (PROGN
                                     (SETQ M (CDR M))
                                     (SETQ LH (CAR M))
                                     (SETQ KL* EQN)
                                     (COND
                                      ((OR (EQCAR LH 'COND) (EQCAR LH 'QUOTE))
                                       (SETQ LH (CAR (SETQ M (LISPEVAL LH))))))
                                     (SETQ RH (SUBST (PREPSQ RH) '& (CADR M)))
                                     ((LAMBDA (*PROTFG)
                                        (SETQ RH
                                                (ERRORSET
                                                 (LIST 'SOLVENLNRSIMP
                                                       (MKQUOTE RH))
                                                 *TRNONLNR NIL)))
                                      T)
                                     (COND ((ERRORP RH) (PROGN (SETQ ABORT T)))
                                           (T (SETQ RH (CAR RH)))))))
                                  (COND
                                   ((NOT ABORT)
                                    (PROGN
                                     (COND
                                      ((AND (NOT (MEMBER LH UV*)) *EXPLI)
                                       (PROGN
                                        (SETQ SUB
                                                (APPEND SUB
                                                        (LIST
                                                         (CONS LH
                                                               (SETQ Z
                                                                       (PREPSQ
                                                                        (SUBSQ
                                                                         RH
                                                                         SUB)))))))
                                        (COND
                                         ((SMEMBER LH R)
                                          (SETQ R (SUBST Z LH R)))))))
                                     (COND
                                      ((AND (OR (MEMBER LH UV*) (NOT *EXPLI))
                                            (NOT
                                             (PROGN
                                              (SETQ Z (SUBSQ RH SUB))
                                              (SETQ N (LENGTH (MEMBER Z R)))
                                              (AND (GREATERP N 0)
                                                   (EQUAL LH
                                                          (NTH V
                                                               (PLUS (LENGTH V)
                                                                     (DIFFERENCE
                                                                      1
                                                                      N))))))))
                                       (PROGN
                                        (SETQ R (CONS Z R))
                                        (SETQ V (CONS LH V))
                                        NIL))))))))
                               (CAR EQN))
                              (SETQ EQN (CDR EQN))
                              (GO LAB))
                            (PROG (X)
                              (SETQ X UV*)
                             LAB
                              (COND ((NULL X) (RETURN NIL)))
                              ((LAMBDA (X)
                                 (COND
                                  ((NOT (MEMBER X V))
                                   (COND
                                    ((NOT (SMEMBER X R))
                                     (PROGN
                                      (SETQ Z (CAAAR (MAKEARBCOMPLEX)))
                                      (SETQ Y Z)
                                      (SETQ V (CONS X V))
                                      (SETQ R (CONS (SIMP Y) R))
                                      (SETQ ARBS (CONS X ARBS))))))))
                               (CAR X))
                              (SETQ X (CDR X))
                              (GO LAB))
                            (COND
                             (*TEST_SOLVEALG
                              (COND (ABORT (YESP "aborted"))
                                    (T
                                     (PROGN
                                      (PRIN2T " --------> ")
                                      (WRITEPRI
                                       (MKQUOTE
                                        (CONS 'LIST
                                              (PROG (U FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ U (PAIR V R))
                                                (COND ((NULL U) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (U)
                                                                    (LIST
                                                                     'EQUAL
                                                                     (CAR U)
                                                                     (PREPSQ
                                                                      (CDR
                                                                       U))))
                                                                  (CAR U))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ U (CDR U))
                                                (COND
                                                 ((NULL U)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (U)
                                                            (LIST 'EQUAL
                                                                  (CAR U)
                                                                  (PREPSQ
                                                                   (CDR U))))
                                                          (CAR U))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))))
                                       'ONLY)
                                      (PRIN2T
                                       "================================")
                                      (YESP "continue?"))))))
                            (COND
                             ((NOT ABORT)
                              (LIST (CONS (REVERSE R) (REVERSE V)))))))
                         (CAR SOL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ SOL (CDR SOL))
                (GO LOOPLABEL)))
      (RETURN (SOLVEALG-VERIFY TAG R)))) 
(PUT 'SOLVEALGDEPENDS 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALGDEPENDS 'DEFINED-ON-LINE '907) 
(PUT 'SOLVEALGDEPENDS 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALGDEPENDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALGDEPENDS (U X)
    (COND ((EQUAL U X) T) ((ATOM U) NIL)
          ((EQCAR U 'ROOT_OF)
           (COND ((EQUAL X (CADDR U)) NIL) (T (SOLVEALGDEPENDS (CADR U) X))))
          (T (OR (SOLVEALGDEPENDS (CAR U) X) (SOLVEALGDEPENDS (CDR U) X))))) 
(PUT 'TEST_TRIG 'NUMBER-OF-ARGS 0) 
(PUT 'TEST_TRIG 'DEFINED-ON-LINE '915) 
(PUT 'TEST_TRIG 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'TEST_TRIG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TEST_TRIG NIL
    (PROG (LH RH R)
      (SETQ LH (CADR KL*))
      (SETQ RH (CADDR KL*))
      (COND ((MEMBER (CONS LH NIL) SOLVEALGDB*) (RETURN NIL)))
      (SETQ R
              (AND (NOT *COMPLEX) (NOT (SMEMQ 'I KL*))
                   (NOT (SMEMQ '|:GI:| KL*)) (NOT (SMEMQ '|:CR:| KL*))
                   (NOT (SMEMQ 'ROOT_OF KL*))))
      (COND
       ((NOT R)
        (SETQ SOLVEALGDB*
                (APPEND SOLVEALGDB*
                        (LIST (CONS (CONS 'SIN (CDR LH)) NIL)
                              (CONS (CONS 'COS (CDR LH)) NIL))))))
      (RETURN R))) 
(PUT 'TEST_HYP 'NUMBER-OF-ARGS 0) 
(PUT 'TEST_HYP 'DEFINED-ON-LINE '926) 
(PUT 'TEST_HYP 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'TEST_HYP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TEST_HYP NIL
    (PROG (LH RH R)
      (SETQ LH (CADR KL*))
      (SETQ RH (CADDR KL*))
      (COND ((MEMBER (CONS LH NIL) SOLVEALGDB*) (RETURN NIL)))
      (SETQ R
              (AND (NOT *COMPLEX) (NOT (SMEMQ 'I KL*))
                   (NOT (SMEMQ '|:GI:| KL*)) (NOT (SMEMQ '|:CR:| KL*))
                   (NOT (SMEMQ 'ROOT_OF KL*))))
      (COND
       ((NOT R)
        (SETQ SOLVEALGDB*
                (APPEND SOLVEALGDB*
                        (LIST (CONS (CONS 'SINH (CDR LH)) NIL)
                              (CONS (CONS 'COSH (CDR LH)) NIL))))))
      (RETURN R))) 
(FLUID '(*SOLVEALG_VERIFY)) 
(PUT 'SOLVEALG-VERIFY 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVEALG-VERIFY 'DEFINED-ON-LINE '945) 
(PUT 'SOLVEALG-VERIFY 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALG-VERIFY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVEALG-VERIFY (TAG R)
    (PROGN
     (COND
      ((AND *ROUNDED *SOLVEALG_VERIFY)
       (PROG (MIN S CMPL *MSG)
         (SETQ CMPL *COMPLEX)
         (COND ((NOT CMPL) (SETDMODE 'COMPLEX (SETQ *COMPLEX T))))
         (SETQ S
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U R)
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (U) (CONS (SOLVEALG-VERIFY1 U) U))
                                     (CAR U))
                                    NIL)))
                  LOOPLABEL
                   (SETQ U (CDR U))
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (U) (CONS (SOLVEALG-VERIFY1 U) U))
                             (CAR U))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ MIN (SIMP '(QUOTIENT 1 100)))
         (SETQ R
                 (PROG (U FORALL-RESULT FORALL-ENDPTR)
                   (SETQ U S)
                  STARTOVER
                   (COND ((NULL U) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (U)
                              (COND
                               ((OR (NULL (CAR U))
                                    (MINUSF (CAR (ADDSQ (CAR U) (NEGSQ MIN)))))
                                (LIST (CDR U)))))
                            (CAR U)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ U (CDR U))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL U) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (U)
                              (COND
                               ((OR (NULL (CAR U))
                                    (MINUSF (CAR (ADDSQ (CAR U) (NEGSQ MIN)))))
                                (LIST (CDR U)))))
                            (CAR U)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ U (CDR U))
                   (GO LOOPLABEL)))
         (COND
          ((NOT CMPL) (PROGN (SETDMODE 'COMPLEX NIL) (SETQ *COMPLEX NIL)))))))
     (CONS TAG
           (PROG (Q FORALL-RESULT FORALL-ENDPTR)
             (SETQ Q R)
             (COND ((NULL Q) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (Q)
                                 (CONS (CAR Q) (CONS (CDR Q) (LIST 1))))
                               (CAR Q))
                              NIL)))
            LOOPLABEL
             (SETQ Q (CDR Q))
             (COND ((NULL Q) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (Q) (CONS (CAR Q) (CONS (CDR Q) (LIST 1))))
                       (CAR Q))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'SOLVEALG-VERIFY1 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVEALG-VERIFY1 'DEFINED-ON-LINE '960) 
(PUT 'SOLVEALG-VERIFY1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVEALG-VERIFY1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVEALG-VERIFY1 (S)
    (PROG (SUB NEXPLI X Y SUM FAIL)
      (SETQ SUB
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U (PAIR (CDR S) (CAR S)))
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (COND
                                     ((NOT NEXPLI)
                                      (PROGN
                                       (SETQ Y (PREPSQ (CDR U)))
                                       (COND
                                        ((NOT
                                          (OR (OR (ATOM Y) (ATOM (CAR Y)))
                                              (CONSTANT_EXPRP Y)))
                                         (SETQ NEXPLI T)))
                                       (CONS (CAR U) Y)))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (COND
                             ((NOT NEXPLI)
                              (PROGN
                               (SETQ Y (PREPSQ (CDR U)))
                               (COND
                                ((NOT
                                  (OR (OR (ATOM Y) (ATOM (CAR Y)))
                                      (CONSTANT_EXPRP Y)))
                                 (SETQ NEXPLI T)))
                               (CONS (CAR U) Y)))))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (NEXPLI (RETURN NIL)))
      (SETQ SUM (CONS NIL 1))
      (PROG (U)
        (SETQ U OSYSTEM*)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U)
           (COND
            ((NOT FAIL)
             (PROGN
              (SETQ X (SUBF U SUB))
              (COND
               ((OR (ATOM (CAR X)) (ATOM (CAR (CAR X))))
                (SETQ SUM (ADDSQ SUM (CONS (ABSF (CAR X)) (CDR X)))))
               (T (SETQ FAIL T)))))))
         (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (RETURN (COND (FAIL NIL) (T SUM))))) 
(PUT 'SUBLIS-PAT 'NUMBER-OF-ARGS 2) 
(PUT 'SUBLIS-PAT 'DEFINED-ON-LINE '977) 
(PUT 'SUBLIS-PAT 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SUBLIS-PAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBLIS-PAT (A U)
    (PROG (V)
      (COND
       ((ATOM U)
        (RETURN
         (PROGN
          (SETQ V (ASSOC U A))
          (COND (V (SUBLIS-PAT A (CDR V))) (T U))))))
      (SETQ V (ASSOC (CAR U) A))
      (COND
       ((AND V (SETQ V (CDR V)) (EQCAR V 'LAMBDA))
        (RETURN (SUBLIS-PAT (CONS (CONS (CAADR V) (CADR U)) A) (CADDR V)))))
      (RETURN (SUBLIS-PAT1 A U)))) 
(PUT 'SUBLIS-PAT1 'NUMBER-OF-ARGS 2) 
(PUT 'SUBLIS-PAT1 'DEFINED-ON-LINE '988) 
(PUT 'SUBLIS-PAT1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SUBLIS-PAT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBLIS-PAT1 (A L)
    (COND ((NULL L) NIL) ((ATOM L) (SUBLIS-PAT A L))
          (T (CONS (SUBLIS-PAT A (CAR L)) (SUBLIS-PAT1 A (CDR L)))))) 
(PUT 'SOLVENONLNRTANSUB 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRTANSUB 'DEFINED-ON-LINE '997) 
(PUT 'SOLVENONLNRTANSUB 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANSUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRTANSUB (P X)
    (COND
     ((AND (NOT (SMEMQ 'SIN P)) (NOT (SMEMQ 'COS P)))
      (COND ((SMEMQ X P) NIL) (T (CONS NIL P))))
     ((EQUAL (CAR P) 'COS)
      (COND
       ((SMEMQ X (CDR P))
        (CONS (CDR P)
              '(QUOTIENT (DIFFERENCE 1 (EXPT TG- 2)) (PLUS 1 (EXPT TG- 2)))))
       (T (CONS NIL P))))
     ((EQUAL (CAR P) 'SIN)
      (COND
       ((SMEMQ X (CDR P))
        (CONS (CDR P) '(QUOTIENT (TIMES 2 TG-) (PLUS 1 (EXPT TG- 2)))))
       (T (CONS NIL P))))
     (T
      ((LAMBDA (CA CD)
         (COND
          ((AND CA CD
                (OR (EQUAL (CAR CA) (CAR CD)) (NULL (CAR CA)) (NULL (CAR CD))))
           (CONS (OR (CAR CA) (CAR CD)) (CONS (CDR CA) (CDR CD))))))
       (SOLVENONLNRTANSUB (CAR P) X) (SOLVENONLNRTANSUB (CDR P) X))))) 
(PUT 'SOLVENONLNRTANSOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVENONLNRTANSOLVE 'DEFINED-ON-LINE '1016) 
(PUT 'SOLVENONLNRTANSOLVE 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRTANSOLVE (U X W)
    (PROG (V S Z R Y AR)
      (SETQ AR 0)
      (SETQ AR !ARBINT)
      (SETQ V (CAAR U))
      (SETQ U (PREPF (CAR (SIMP (CDR U)))))
      (SETQ S (SOLVEEVAL (LIST U 'TG-)))
      (SETQ !ARBINT AR)
      (PROG (Q)
        (SETQ Q (CDR S))
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (SETQ Z (REVAL1 (CADDR Q) T))
            (SETQ Z (REVAL1 (SUBLIS (SOLVENONLNRTANSOLVE1 Z) Z) T))
            (SETQ !ARBINT AR)
            (SETQ Y
                    (SOLVE0 (LIST 'EQUAL (LIST 'TAN (LIST 'QUOTIENT V 2)) Z)
                            X))
            (SETQ R (UNION Y R))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (SETQ Y
              (ERRORSET2
               (LIST 'SUBF (MKQUOTE W) (MKQUOTE (LIST (CONS X 'PI))))))
      (COND
       ((AND (NULL (ERRORP Y)) (NULL (CAR (CAR Y))))
        (PROGN
         (SETQ !ARBINT AR)
         (SETQ R (UNION (SOLVE0 (LIST 'EQUAL (LIST 'COS X) (MINUS 1)) X) R)))))
      (RETURN (CONS T R)))) 
(PUT 'SOLVENONLNRTANSOLVE1 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVENONLNRTANSOLVE1 'DEFINED-ON-LINE '1037) 
(PUT 'SOLVENONLNRTANSOLVE1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANSOLVE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVENONLNRTANSOLVE1 (U)
    (COND ((ATOM U) NIL)
          ((AND (EQUAL (CAR U) 'EXPT) (EQCAR (CADR U) 'COS)
                (EQUAL (CADDR U) 2))
           (LIST
            (CONS U
                  (LIST 'DIFFERENCE 1
                        (LIST 'EXPT (LIST 'SIN (CADR (CADR U))) 2)))))
          (T
           (UNION (SOLVENONLNRTANSOLVE1 (CAR U))
                  (SOLVENONLNRTANSOLVE1 (CDR U)))))) 
(PUT 'SOLVENONLNRTANHSUB 'NUMBER-OF-ARGS 2) 
(PUT 'SOLVENONLNRTANHSUB 'DEFINED-ON-LINE '1048) 
(PUT 'SOLVENONLNRTANHSUB 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANHSUB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRTANHSUB (P X)
    (COND
     ((AND (NOT (SMEMQ 'SINH P)) (NOT (SMEMQ 'COSH P)))
      (COND ((SMEMQ X P) NIL) (T (CONS NIL P))))
     ((EQUAL (CAR P) 'COSH)
      (COND
       ((SMEMQ X (CDR P))
        (CONS (CDR P)
              '(QUOTIENT (PLUS 1 (EXPT TGH- 2)) (DIFFERENCE 1 (EXPT TGH- 2)))))
       (T (CONS NIL P))))
     ((EQUAL (CAR P) 'SINH)
      (COND
       ((SMEMQ X (CDR P))
        (CONS (CDR P) '(QUOTIENT (TIMES 2 TGH-) (DIFFERENCE 1 (EXPT TGH- 2)))))
       (T (CONS NIL P))))
     (T
      ((LAMBDA (CA CD)
         (COND
          ((AND CA CD
                (OR (EQUAL (CAR CA) (CAR CD)) (NULL (CAR CA)) (NULL (CAR CD))))
           (CONS (OR (CAR CA) (CAR CD)) (CONS (CDR CA) (CDR CD))))))
       (SOLVENONLNRTANHSUB (CAR P) X) (SOLVENONLNRTANHSUB (CDR P) X))))) 
(PUT 'SOLVENONLNRTANHSOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVENONLNRTANHSOLVE 'DEFINED-ON-LINE '1067) 
(PUT 'SOLVENONLNRTANHSOLVE 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANHSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVENONLNRTANHSOLVE (U X W)
    (PROG (V S Z R Y AR)
      (SETQ AR !ARBINT)
      (SETQ V (CAAR U))
      (SETQ U (PREPF (CAR (SIMP (CDR U)))))
      (SETQ S (SOLVEEVAL (LIST U 'TGH-)))
      (SETQ AR !ARBINT)
      (PROG (Q)
        (SETQ Q (CDR S))
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (SETQ Z (REVAL1 (CADDR Q) T))
            (SETQ Z (REVAL1 (SUBLIS (SOLVENONLNRTANHSOLVE1 Z) Z) T))
            (SETQ !ARBINT AR)
            (SETQ Y
                    (SOLVE0 (LIST 'EQUAL (LIST 'TANH (LIST 'QUOTIENT V 2)) Z)
                            X))
            (SETQ R (UNION Y R))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (COND
       ((AND *COMPLEX (NULL (CAR (SUBF W (LIST (CONS X '(TIMES PI I)))))))
        (PROGN
         (SETQ !ARBINT AR)
         (SETQ R
                 (UNION (SOLVE0 (LIST 'EQUAL (LIST 'COSH X) (MINUS 1)) X)
                        R)))))
      (RETURN (CONS T R)))) 
(PUT 'SOLVENONLNRTANHSOLVE1 'NUMBER-OF-ARGS 1) 
(PUT 'SOLVENONLNRTANHSOLVE1 'DEFINED-ON-LINE '1083) 
(PUT 'SOLVENONLNRTANHSOLVE1 'DEFINED-IN-FILE 'SOLVE/SOLVEALG.RED) 
(PUT 'SOLVENONLNRTANHSOLVE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SOLVENONLNRTANHSOLVE1 (U)
    (COND ((ATOM U) NIL)
          ((AND (EQUAL (CAR U) 'EXPT) (EQCAR (CADR U) 'COSH)
                (EQUAL (CADDR U) 2))
           (LIST
            (CONS U
                  (LIST 'PLUS 1 (LIST 'EXPT (LIST 'SINH (CADR (CADR U))) 2)))))
          (T
           (UNION (SOLVENONLNRTANHSOLVE1 (CAR U))
                  (SOLVENONLNRTANHSOLVE1 (CDR U)))))) 
(ENDMODULE) 