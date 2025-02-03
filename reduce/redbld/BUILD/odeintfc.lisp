(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODEINTFC)) 
(PUT 'ODESOLVE 'PSOPFN 'ODESOLVE-EVAL) 
(PUT 'DSOLVE 'PSOPFN 'ODESOLVE-EVAL) 
(FLAG (LIST 'ODESOLVE 'DSOLVE) 'LISTARGP) 
(PUT 'ODESOLVE-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-EVAL 'DEFINED-ON-LINE '107) 
(PUT 'ODESOLVE-EVAL 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-EVAL (ARGS)
    ((LAMBDA (RESULT)
       (COND
        ((OR *DIV *INTSTR *FACTOR (NOT *EXP) (NOT *MCD))
         (NOINT2INT (REVAL1 RESULT T)))
        (T (NOINT2INT RESULT))))
     (PROG (*EVALLHSEQP *MULTIPLICITIES *DIV *INTSTR *EXP *MCD *FACTOR *IFACTOR
            *PRECISE *NOPOWERS *ALGINT *ECHO *DF_PARTIAL)
       (SETQ *DF_PARTIAL (SETQ *EVALLHSEQP (SETQ *EXP (SETQ *MCD T))))
       (RETURN (ODESOLVE-EVAL1 ARGS))))) 
(PUT 'ODESOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE 'DEFINED-ON-LINE '121) 
(PUT 'ODESOLVE 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE (ODE Y X) (ODESOLVE-EVAL (LIST ODE Y X))) 
(GLOBAL '(ODESOLVE-TRACING-SYNONYMS)) 
(SETQ ODESOLVE-TRACING-SYNONYMS '(TRODE TRACE TRACING)) 
(PUT 'ODESOLVE-EVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-EVAL1 'DEFINED-ON-LINE '129) 
(PUT 'ODESOLVE-EVAL1 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-EVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-EVAL1 (ARGS)
    ((LAMBDA
         (*ODESOLVE_IMPLICIT *ODESOLVE_EXPLICIT *ODESOLVE_EXPAND *TRODE
          *ODESOLVE_NOINT *ODESOLVE_VERBOSE *ODESOLVE_BASIS *ODESOLVE_NOSWAP
          *ODESOLVE_NORECURSE *ODESOLVE_FAST *ODESOLVE_CHECK)
       (PROG (ODE SYSTEM Y X YCONDS XCONDS CONDS SOLN)
         (COND
          ((NULL ARGS)
           (REDERR "ODESolve requires at least one argument -- the ODE")))
         (PROG (DF_SIMPFN *UNCACHED)
           (SETQ *UNCACHED T)
           (SETQ DF_SIMPFN (GET 'DF 'SIMPFN))
           (PUT 'DF 'SIMPFN 'SIMPIDEN)
           (SETQ ARGS (ERRORSET* (LIST 'REVLIS (MKQUOTE ARGS)) T))
           (PUT 'DF 'SIMPFN DF_SIMPFN)
           (COND ((ERRORP ARGS) (ERROR1)))
           (SETQ ARGS (CAR ARGS)))
         (SETQ ODE (CAR ARGS))
         (SETQ ARGS (CDR ARGS))
         (SETQ SYSTEM (EQCAR ODE 'LIST))
         (COND
          (ARGS
           (PROGN
            (SETQ Y (CAR ARGS))
            (COND
             ((EQCAR Y 'LIST)
              (COND ((NULL (CDR Y)) (SETQ Y 'EMPTY))
                    ((OR (EQCAR (CADR Y) 'LIST) (EQCAR (CADR Y) 'EQUAL))
                     (SETQ Y NIL))
                    (SYSTEM
                     (SETQ Y
                             (CONS 'LIST
                                   (PROG (YY FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ YY (CDR Y))
                                     (COND ((NULL YY) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (YY) (*A2K YY))
                                                       (CAR YY))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ YY (CDR YY))
                                     (COND ((NULL YY) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (YY) (*A2K YY))
                                               (CAR YY))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))))
                    (T
                     (MSGPRI "ODESolve: invalid second argument" Y NIL NIL
                             T))))
             (SYSTEM (TYPERR Y "dependent var list"))
             ((EQCAR Y 'EQUAL)
              (COND
               ((MEMQ (CADR Y) (CONS 'OUTPUT ODESOLVE-TRACING-SYNONYMS))
                (SETQ Y NIL))
               (T (PROGN (SETQ YCONDS (CADDR Y)) (SETQ Y (*A2K (CADR Y)))))))
             ((NOT (SMEMBER (SETQ Y (*A2K Y)) ODE)) (SETQ Y NIL))))))
         (COND (Y (SETQ ARGS (CDR ARGS))))
         (COND
          ((AND ARGS Y)
           (PROGN
            (SETQ X (CAR ARGS))
            (COND
             ((EQCAR X 'LIST)
              (COND ((NULL (CDR X)) (SETQ X 'EMPTY)) (T (SETQ X NIL))))
             ((EQCAR X 'EQUAL)
              (COND
               ((MEMQ (CADR X) (CONS 'OUTPUT ODESOLVE-TRACING-SYNONYMS))
                (SETQ X NIL))
               (T (PROGN (SETQ XCONDS (CADDR X)) (SETQ X (*A2K (CADR X)))))))
             ((NOT (SMEMBER (SETQ X (*A2K X)) ODE)) (SETQ X NIL))))))
         (COND (X (SETQ ARGS (CDR ARGS))))
         (COND ((EQ Y 'EMPTY) (SETQ Y NIL)))
         (COND ((EQ X 'EMPTY) (SETQ X NIL)))
         (COND
          ((AND (NULL X) Y)
           (COND
            ((EQCAR Y 'LIST)
             (PROG (YY)
               (SETQ YY (CDR Y))
               (PROG ()
                WHILELABEL
                 (COND ((NOT (AND YY (ATOM (CAR YY)))) (RETURN NIL)))
                 (SETQ YY (CDR YY))
                 (GO WHILELABEL))
               (COND ((AND YY (CDAR YY)) (SETQ X (CADAR YY))))
               (COND ((NOT (IDP X)) (SETQ X NIL)))))
            ((AND (PAIRP Y) (CDR Y)) (SETQ X (CADR Y))))))
         (COND
          ((OR (NULL Y) (NULL X))
           (PROG (DF_SIMPFN K_LIST DF_LIST)
             (SETQ DF_SIMPFN (GET 'DF 'SIMPFN))
             (PUT 'DF 'SIMPFN 'SIMPIDEN)
             (SETQ K_LIST (ERRORSET* (LIST 'GET_K_LIST (MKQUOTE ODE)) T))
             (PUT 'DF 'SIMPFN DF_SIMPFN)
             (COND ((ERRORP K_LIST) (ERROR1)) (T (SETQ K_LIST (CAR K_LIST))))
             (SETQ DF_LIST (GET_OP_KNL 'DF (CAR K_LIST)))
             (PROG (KNL)
               (SETQ KNL (CDR K_LIST))
              LAB
               (COND ((NULL KNL) (RETURN NIL)))
               ((LAMBDA (KNL)
                  (SETQ DF_LIST (UNION DF_LIST (GET_OP_KNL 'DF KNL))))
                (CAR KNL))
               (SETQ KNL (CDR KNL))
               (GO LAB))
             (COND
              ((NULL DF_LIST)
               (REDERR "No derivatives found -- use solve instead.")))
             (COND
              ((NULL Y)
               (PROGN
                (SETQ Y (CONS (CADAR DF_LIST) NIL))
                (PROG (EL)
                  (SETQ EL (CDR DF_LIST))
                 LAB
                  (COND ((NULL EL) (RETURN NIL)))
                  ((LAMBDA (EL)
                     (COND
                      ((NOT (MEMBER (CADR EL) Y))
                       (SETQ Y (CONS (CADR EL) Y)))))
                   (CAR EL))
                  (SETQ EL (CDR EL))
                  (GO LAB))
                (COND
                 (SYSTEM
                  (COND
                   ((LESSP (LENGTH ODE) (LENGTH Y))
                    (REDERR "ODESolve: under-determined system of ODEs."))
                   (T (SETQ Y (CONS 'LIST Y)))))
                 ((CDR Y)
                  (MSGPRI "ODESolve -- too many dependent variables:"
                          (CONS 'LIST Y) NIL NIL T))
                 (T (SETQ Y (CAR Y))))
                (MSGPRI "Dependent var(s) assumed to be" Y NIL NIL NIL))))
             (COND
              ((NULL X)
               (PROGN
                (SETQ X (CADDAR DF_LIST))
                (MSGPRI "Independent var assumed to be" X NIL NIL NIL)))))))
         (ENSUREDEPENDENCY Y X)
         (SETQ ODE (REVAL1 ODE NIL))
         (COND
          (SYSTEM
           (COND
            ((GREATERP (LENGTH ODE) 2)
             (RETURN
              (ODESOLVE-DEPEND
               (CONS 'LIST
                     (PROG (O FORALL-RESULT FORALL-ENDPTR)
                       (SETQ O (CDR ODE))
                       (COND ((NULL O) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (O) (*EQN2A O)) (CAR O))
                                             NIL)))
                      LOOPLABEL
                       (SETQ O (CDR O))
                       (COND ((NULL O) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (O) (*EQN2A O)) (CAR O)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               Y X NIL)))
            (T (PROGN (SETQ ODE (*EQN2A (CADR ODE))) (SETQ Y (CADR Y))))))
          (T (SETQ ODE (*EQN2A ODE))))
         (COND
          (ARGS
           (COND
            ((EQCAR (SETQ CONDS (REVAL1 (CAR ARGS) NIL)) 'LIST)
             (PROGN
              (SETQ ARGS (CDR ARGS))
              (SETQ CONDS
                      (COND ((NOT (EQCAR (CADR CONDS) 'LIST)) (CONS CONDS NIL))
                            (T (CDR CONDS))))))
            (T (SETQ CONDS NIL)))))
         (COND
          (YCONDS
           (SETQ YCONDS
                   (COND ((EQCAR YCONDS 'LIST) (CDR YCONDS))
                         (T (CONS YCONDS NIL))))))
         (COND
          (XCONDS
           (SETQ XCONDS
                   (COND ((EQCAR XCONDS 'LIST) (CDR XCONDS))
                         (T (CONS XCONDS NIL))))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND YCONDS XCONDS)) (RETURN NIL)))
           (PROGN
            (SETQ CONDS
                    (CONS
                     (LIST 'LIST (LIST 'EQUAL X (CAR XCONDS))
                           (LIST 'EQUAL Y (CAR YCONDS)))
                     CONDS))
            (SETQ YCONDS (CDR YCONDS))
            (SETQ XCONDS (CDR XCONDS)))
           (GO WHILELABEL))
         (COND
          ((OR YCONDS XCONDS) (REDERR "Different condition list lengths")))
         (COND
          (CONDS (SETQ CONDS (CONS 'LIST (ODESOLVE-SORT-CONDS CONDS Y X)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT ARGS) (RETURN NIL)))
           (PROG (ARG)
             (SETQ ARG (CAR ARGS))
             (SETQ ARGS (CDR ARGS))
             (COND
              ((EQCAR ARG 'EQUAL)
               (COND
                ((EQ (CADR ARG) 'OUTPUT) (SETQ ARGS (CONS (CADDR ARG) ARGS)))
                ((MEMQ (CADR ARG) ODESOLVE-TRACING-SYNONYMS)
                 (SETQ *TRODE
                         (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CADDR ARG))))
                (T (MSGPRI "Invalid ODESolve option" ARG "ignored." NIL NIL))))
              ((MEMQ ARG
                     '(IMPLICIT EXPLICIT EXPAND NOINT VERBOSE BASIS NOSWAP
                       NORECURSE FAST CHECK))
               (SET (MKID '*ODESOLVE_ ARG) T))
              ((EQ ARG 'ALGINT) (ON1 'ALGINT))
              ((OR (EQ ARG 'FULL) *ODESOLVE_FULL)
               (SETQ *ODESOLVE_EXPAND (SETQ *ODESOLVE_EXPLICIT T)))
              ((MEMQ ARG ODESOLVE-TRACING-SYNONYMS)
               (SETQ *TRODE (PROGN (SETQ ALGLIST* (CONS NIL NIL)) T)))
              ((MEMQ ARG '(LAPLACE NUMERIC SERIES))
               (REDERR (LIST "ODESolve option" ARG "not yet implemented.")))
              (T (REDERR (LIST "Invalid ODESolve option" ARG)))))
           (GO WHILELABEL))
         (COND
          (*ODESOLVE_VERBOSE
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "ODE: ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (LIST 'EQUAL (LIST 'NUM ODE) 0)) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "Dependent variable: ") NIL 'FIRST)
             (ASSGNPRI (AEVAL Y) NIL NIL)
             (ASSGNPRI (AEVAL ";  independent variable: ") NIL NIL)
             (ASSGNPRI (AEVAL X) NIL 'LAST))
            (PROGN
             (ASSGNPRI (AEVAL "Conditions: ") NIL 'FIRST)
             (ASSGNPRI (AEVAL (OR CONDS "none")) NIL 'LAST))
            (AEVAL 'NIL))))
         (COND (CONDS (SETQ *ODESOLVE_BASIS NIL)))
         (COND
          ((NOT (GETD 'ODESOLVE*0)) (RETURN (LIST 'ODESOLVE ODE Y X CONDS))))
         (COND
          ((NULL (SETQ SOLN (ODESOLVE-DEPEND ODE Y X CONDS)))
           (RETURN (AEVAL (LIST 'LIST (LIST 'EQUAL (LIST 'NUM ODE) 0))))))
         (COND
          ((AND *ODESOLVE_EXPLICIT
                (NOT
                 (AND (EQCAR (CADR SOLN) 'LIST)
                      (NOT (EQCAR (CADADR SOLN) 'EQUAL)))))
           (SETQ SOLN (ODESOLVE-MAKE-EXPLICIT SOLN Y CONDS))))
         (COND (*ODESOLVE_EXPAND (SETQ SOLN (EXPAND_ROOTS_OF_UNITY SOLN))))
         (COND
          (*ODESOLVE_CHECK
           ((LAMBDA (*NOINT)
              (ODE-SOLN-CHECK
               (COND (*ODESOLVE_NOINT (NOINT2INT SOLN)) (T SOLN)) ODE Y X
               CONDS))
            T)))
         (RETURN SOLN)))
     *ODESOLVE_IMPLICIT *ODESOLVE_EXPLICIT *ODESOLVE_EXPAND *TRODE
     *ODESOLVE_NOINT *ODESOLVE_VERBOSE *ODESOLVE_BASIS *ODESOLVE_NOSWAP
     *ODESOLVE_NORECURSE *ODESOLVE_FAST *ODESOLVE_CHECK)) 
(PUT 'ODESOLVE-MAKE-EXPLICIT 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-MAKE-EXPLICIT 'DEFINED-ON-LINE '346) 
(PUT 'ODESOLVE-MAKE-EXPLICIT 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-MAKE-EXPLICIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-MAKE-EXPLICIT (SOLNS Y CONDS)
    (PROGN
     (SETQ SOLNS
             (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
               (SETQ SOLN (CDR SOLNS))
              STARTOVER
               (COND ((NULL SOLN) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (SOLN)
                          (COND ((EQ (CADR SOLN) Y) (LIST SOLN))
                                (T
                                 (PROGN
                                  (TRACEODE
                                   (LIST
                                    "Solution before trying to solve for dependent variable is "
                                    SOLN))
                                  (CDR
                                   (REVAL1 (REVAL1 (LIST 'SOLVE SOLN Y) NIL)
                                           T))))))
                        (CAR SOLN)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ SOLN (CDR SOLN))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (SOLN)
                          (COND ((EQ (CADR SOLN) Y) (LIST SOLN))
                                (T
                                 (PROGN
                                  (TRACEODE
                                   (LIST
                                    "Solution before trying to solve for dependent variable is "
                                    SOLN))
                                  (CDR
                                   (REVAL1 (REVAL1 (LIST 'SOLVE SOLN Y) NIL)
                                           T))))))
                        (CAR SOLN)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ SOLN (CDR SOLN))
               (GO LOOPLABEL)))
     (COND
      (CONDS
       (PROG (COND)
         (SETQ COND (CDR CONDS))
        LAB
         (COND ((NULL COND) (RETURN NIL)))
         ((LAMBDA (COND)
            (PROG (XCOND)
              (SETQ XCOND (CADR COND))
              (SETQ COND
                      (CONS 'LIST
                            (PROG (C FORALL-RESULT FORALL-ENDPTR)
                              (SETQ C (CDDR COND))
                              (COND ((NULL C) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (C) (*EQN2A C))
                                                (CAR C))
                                               NIL)))
                             LOOPLABEL
                              (SETQ C (CDR C))
                              (COND ((NULL C) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (C) (*EQN2A C)) (CAR C))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
              (SETQ SOLNS
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S SOLNS)
                       STARTOVER
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (S)
                                   (COND
                                    ((OR (EQCAR (CADDR S) 'ROOT_OF)
                                         (EQUAL
                                          (UNION
                                           (CDR
                                            (SUBEVAL
                                             (LIST XCOND
                                                   (SUBEVAL (LIST S COND)))))
                                           NIL)
                                          (LIST 0)))
                                     (LIST S))
                                    (T
                                     (AEVAL
                                      (TRACEODE
                                       (LIST "Solution " S
                                             " discarded -- does not satisfy conditions"))))))
                                 (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ S (CDR S))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (S)
                                   (COND
                                    ((OR (EQCAR (CADDR S) 'ROOT_OF)
                                         (EQUAL
                                          (UNION
                                           (CDR
                                            (SUBEVAL
                                             (LIST XCOND
                                                   (SUBEVAL (LIST S COND)))))
                                           NIL)
                                          (LIST 0)))
                                     (LIST S))
                                    (T
                                     (AEVAL
                                      (TRACEODE
                                       (LIST "Solution " S
                                             " discarded -- does not satisfy conditions"))))))
                                 (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ S (CDR S))
                        (GO LOOPLABEL)))))
          (CAR COND))
         (SETQ COND (CDR COND))
         (GO LAB))))
     (CONS 'LIST SOLNS))) 
(PUT 'TRIG_SIMPLIFY 'NUMBER-OF-ARGS 1) 
(FLAG '(TRIG_SIMPLIFY) 'OPFN) 
(PUT 'TRIG_SIMPLIFY 'DEFINED-ON-LINE '390) 
(PUT 'TRIG_SIMPLIFY 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'TRIG_SIMPLIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIG_SIMPLIFY (U) (LIST 'WHEREEXP (LIST 'LIST 'TAN_HALF_ANGLE_RULES) U)) 
(SETK 'TAN_HALF_ANGLE_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'SIN (LIST '~ 'U))
                   (LIST 'TIMES 2
                         (LIST 'QUOTIENT (LIST 'TAN (LIST 'QUOTIENT 'U 2))
                               (LIST 'PLUS 1
                                     (LIST 'EXPT
                                           (LIST 'TAN (LIST 'QUOTIENT 'U 2))
                                           2)))))
             (LIST 'REPLACEBY (LIST 'COS (LIST '~ 'U))
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE 1
                               (LIST 'EXPT (LIST 'TAN (LIST 'QUOTIENT 'U 2))
                                     2))
                         (LIST 'PLUS 1
                               (LIST 'EXPT (LIST 'TAN (LIST 'QUOTIENT 'U 2))
                                     2))))))) 
(PUT 'GET_K_LIST 'NUMBER-OF-ARGS 1) 
(PUT 'GET_K_LIST 'DEFINED-ON-LINE '399) 
(PUT 'GET_K_LIST 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'GET_K_LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_K_LIST (ODE)
    (PROG (K_LIST *UNCACHED)
      (SETQ *UNCACHED T)
      (COND ((EQCAR ODE 'SETK) (SETQ ODE (CADDR ODE))))
      (COND
       ((EQCAR (SETQ ODE (REVAL1 ODE T)) 'LIST)
        (PROGN
         (SETQ K_LIST (GET_K_LIST1 (CADR ODE)))
         (PROG (EL)
           (SETQ EL (CDDR ODE))
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL) (SETQ K_LIST (UNION K_LIST (GET_K_LIST1 EL))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))))
       (T (SETQ K_LIST (GET_K_LIST1 ODE))))
      (RETURN K_LIST))) 
(PUT 'GET_K_LIST1 'NUMBER-OF-ARGS 1) 
(PUT 'GET_K_LIST1 'DEFINED-ON-LINE '415) 
(PUT 'GET_K_LIST1 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'GET_K_LIST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET_K_LIST1 (ODE)
    ((LAMBDA (O) (UNION (KERNELS (CAR O)) (KERNELS (CDR O))))
     (SIMP (*EQN2A ODE)))) 
(PUT 'GET_OP_KNL 'NUMBER-OF-ARGS 2) 
(PUT 'GET_OP_KNL 'DEFINED-ON-LINE '419) 
(PUT 'GET_OP_KNL 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'GET_OP_KNL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_OP_KNL (OP KNL)
    (COND
     ((PAIRP KNL)
      (COND ((EQ (CAR KNL) OP) (CONS KNL NIL))
            (T
             ((LAMBDA (OP_IN_CAR OP_IN_CDR)
                (COND (OP_IN_CAR (UNION OP_IN_CAR OP_IN_CDR)) (T OP_IN_CDR)))
              (GET_OP_KNL OP (CAR KNL)) (GET_OP_KNL OP (CDR KNL)))))))) 
(PUT 'ENSUREDEPENDENCY 'NUMBER-OF-ARGS 2) 
(PUT 'ENSUREDEPENDENCY 'DEFINED-ON-LINE '428) 
(PUT 'ENSUREDEPENDENCY 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ENSUREDEPENDENCY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ENSUREDEPENDENCY (Y X)
    (PROG (YY)
      (SETQ YY (COND ((EQCAR Y 'LIST) (CDR Y)) (T (CONS Y NIL))))
     LAB
      (COND ((NULL YY) (RETURN NIL)))
      ((LAMBDA (YY)
         (COND
          ((NOT (DEPENDS YY X))
           (PROGN (MSGPRI "depend" YY "," X NIL) (DEPEND1 YY X T)))))
       (CAR YY))
      (SETQ YY (CDR YY))
      (GO LAB))) 
(PUT 'ODESOLVE-SORT-CONDS 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-SORT-CONDS 'DEFINED-ON-LINE '436) 
(PUT 'ODESOLVE-SORT-CONDS 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-SORT-CONDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SORT-CONDS (CONDS Y X)
    (PROG (COND_ALIST)
      (PROG (COND)
        (SETQ COND CONDS)
       LAB
        (COND ((NULL COND) (RETURN NIL)))
        ((LAMBDA (COND)
           (PROG (X_COND Y_CONDS X_ALIST)
             (COND ((NOT (EQCAR COND 'LIST)) (TYPERR COND "ode condition")))
             (SETQ Y_CONDS
                     (PROG (C FORALL-RESULT FORALL-ENDPTR)
                       (SETQ C (CDR COND))
                      STARTOVER
                       (COND ((NULL C) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (C)
                                  (COND
                                   ((NOT (CONDEQ C Y X))
                                    (TYPERR C "ode condition equation"))
                                   ((EQ (CADR C) X)
                                    (PROGN (SETQ X_COND C) NIL))
                                   (T (CONS C NIL))))
                                (CAR C)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ C (CDR C))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL C) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (C)
                                  (COND
                                   ((NOT (CONDEQ C Y X))
                                    (TYPERR C "ode condition equation"))
                                   ((EQ (CADR C) X)
                                    (PROGN (SETQ X_COND C) NIL))
                                   (T (CONS C NIL))))
                                (CAR C)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ C (CDR C))
                       (GO LOOPLABEL)))
             (COND
              ((NULL X_COND)
               (MSGPRI NIL X "omitted from ode condition" COND T)))
             (COND
              ((NULL Y_CONDS)
               (MSGPRI NIL Y "omitted from ode condition" COND T)))
             (COND
              ((SETQ X_ALIST (ASSOC X_COND COND_ALIST))
               (NCONC X_ALIST Y_CONDS))
              (T (SETQ COND_ALIST (CONS (CONS X_COND Y_CONDS) COND_ALIST))))))
         (CAR COND))
        (SETQ COND (CDR COND))
        (GO LAB))
      (RETURN
       (PROG (COND FORALL-RESULT FORALL-ENDPTR)
         (SETQ COND COND_ALIST)
         (COND ((NULL COND) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (COND)
                             (CONS 'LIST
                                   (COND ((NULL (CDDR COND)) COND)
                                         (T
                                          (CONS (CAR COND)
                                                (PROG (SORTED NEXT_SORTED THIS
                                                       NEXT RESULT)
                                                  (SETQ SORTED
                                                          (SORT (CDR COND)
                                                                'LESSP-DERIV-ORD))
                                                  (PROG ()
                                                   WHILELABEL
                                                    (COND
                                                     ((NOT
                                                       (AND SORTED
                                                            (SETQ NEXT_SORTED
                                                                    (CDR
                                                                     SORTED))))
                                                      (RETURN NIL)))
                                                    (PROGN
                                                     (COND
                                                      ((EQ
                                                        (CADR
                                                         (SETQ THIS
                                                                 (CAR SORTED)))
                                                        (CADR
                                                         (SETQ NEXT
                                                                 (CAR
                                                                  NEXT_SORTED))))
                                                       (COND
                                                        ((NEQ (CADDR THIS)
                                                              (CADDR NEXT))
                                                         (MSGPRI
                                                          "Inconsistent conditions:"
                                                          (LIST 'LIST THIS
                                                                NEXT)
                                                          "at" (CAR COND) T))))
                                                      (T
                                                       (SETQ RESULT
                                                               (CONS THIS
                                                                     RESULT))))
                                                     (SETQ SORTED NEXT_SORTED))
                                                    (GO WHILELABEL))
                                                  (RETURN
                                                   (REVERSIP
                                                    (CONS NEXT RESULT)))))))))
                           (CAR COND))
                          NIL)))
        LOOPLABEL
         (SETQ COND (CDR COND))
         (COND ((NULL COND) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (COND)
                     (CONS 'LIST
                           (COND ((NULL (CDDR COND)) COND)
                                 (T
                                  (CONS (CAR COND)
                                        (PROG (SORTED NEXT_SORTED THIS NEXT
                                               RESULT)
                                          (SETQ SORTED
                                                  (SORT (CDR COND)
                                                        'LESSP-DERIV-ORD))
                                          (PROG ()
                                           WHILELABEL
                                            (COND
                                             ((NOT
                                               (AND SORTED
                                                    (SETQ NEXT_SORTED
                                                            (CDR SORTED))))
                                              (RETURN NIL)))
                                            (PROGN
                                             (COND
                                              ((EQ
                                                (CADR (SETQ THIS (CAR SORTED)))
                                                (CADR
                                                 (SETQ NEXT
                                                         (CAR NEXT_SORTED))))
                                               (COND
                                                ((NEQ (CADDR THIS)
                                                      (CADDR NEXT))
                                                 (MSGPRI
                                                  "Inconsistent conditions:"
                                                  (LIST 'LIST THIS NEXT) "at"
                                                  (CAR COND) T))))
                                              (T
                                               (SETQ RESULT
                                                       (CONS THIS RESULT))))
                                             (SETQ SORTED NEXT_SORTED))
                                            (GO WHILELABEL))
                                          (RETURN
                                           (REVERSIP (CONS NEXT RESULT)))))))))
                   (CAR COND))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'CONDEQ 'NUMBER-OF-ARGS 3) 
(PUT 'CONDEQ 'DEFINED-ON-LINE '486) 
(PUT 'CONDEQ 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'CONDEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CONDEQ (C Y X)
    (AND (EQEXPR C)
         (OR (EQ (SETQ C (CADR C)) X) (EQ C Y)
             (AND (EQCAR C 'DF) (EQ (CADR C) Y) (EQ (CADDR C) X)
                  (OR (NULL (CDDDR C)) (FIXP (CADDDR C))))))) 
(PUT 'LESSP-DERIV-ORD 'NUMBER-OF-ARGS 2) 
(PUT 'LESSP-DERIV-ORD 'DEFINED-ON-LINE '494) 
(PUT 'LESSP-DERIV-ORD 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'LESSP-DERIV-ORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LESSP-DERIV-ORD (A B)
    (COND ((ATOM (SETQ A (CADR A))) (NOT (ATOM (CADR B))))
          ((ATOM (SETQ B (CADR B))) (NOT (ATOM (CADR A))))
          ((NOT (EQ (CAR A) 'DF)) (EQ (CAR B) 'DF))
          (T
           (AND (EQ (CAR B) 'DF)
                (COND ((NULL (SETQ A (CDDDR A))) (CDDDR B))
                      (T (AND (SETQ B (CDDDR B)) (LESSP (CAR A) (CAR B))))))))) 
(PUT 'ODESOLVE-DEPEND 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-DEPEND 'DEFINED-ON-LINE '515) 
(PUT 'ODESOLVE-DEPEND 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-DEPEND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-DEPEND (ODE Y X CONDS)
    ((LAMBDA (DEPL*)
       (PROG (XEQT YLIST SUBLIST)
         (SETQ Y (COND ((EQCAR ODE 'LIST) (CDR Y)) (T (CONS Y NIL))))
         (COND
          ((EQ X T)
           (PROGN
            (SETQ XEQT T)
            (SETQ X (GENSYM))
            (PROG (YY)
              (SETQ YY Y)
             LAB
              (COND ((NULL YY) (RETURN NIL)))
              ((LAMBDA (YY) (COND ((IDP YY) (DEPEND1 YY X T)))) (CAR YY))
              (SETQ YY (CDR YY))
              (GO LAB))
            (SETQ ODE (SUBST X T (REVAL1 ODE T)))
            (COND (CONDS (SETQ CONDS (SUBST X T (REVAL1 CONDS T)))))
            (SETQ SUBLIST (CONS (CONS T X) SUBLIST)))))
         (PROG (YY)
           (SETQ YY Y)
          LAB
           (COND ((NULL YY) (RETURN NIL)))
           ((LAMBDA (YY)
              (COND
               ((AND (IDP YY) (NOT (EQ YY T)))
                (PROGN
                 (SETQ YLIST (CONS YY YLIST))
                 ((LAMBDA (*MSG) (DEPEND1 X YY NIL)) NIL)
                 NIL))
               (T
                (PROG (YYY)
                  (SETQ YYY (GENSYM))
                  (DEPEND1 YYY X T)
                  (SETQ YLIST (CONS YYY YLIST))
                  (PUT YYY 'ODESOLVE-DEPVAR YY)
                  (SETQ SUBLIST (CONS (CONS YY YYY) SUBLIST))
                  (COND (XEQT (SETQ YY (SUBEVAL (LIST (LIST 'EQUAL T X) YY)))))
                  (ODESOLVE-DELAY-CHECK ODE YY)
                  (SETQ ODE (SUBEVAL (LIST (LIST 'EQUAL YY YYY) ODE)))
                  (COND
                   (CONDS
                    (SETQ CONDS
                            (SUBEVAL (LIST (LIST 'EQUAL YY YYY) CONDS)))))))))
            (CAR YY))
           (SETQ YY (CDR YY))
           (GO LAB))
         (SETQ YLIST (REVERSE YLIST))
         (SETQ ODE
                 (COND ((EQCAR ODE 'LIST) (ODESOLVE-SYSTEM (CDR ODE) YLIST X))
                       (CONDS (ODESOLVE-WITH-CONDS ODE (CAR YLIST) X CONDS))
                       (T (ODESOLVE*0 ODE (CAR YLIST) X))))
         (COND ((NULL ODE) (RETURN NIL)))
         (COND
          (SUBLIST
           (PROG (*NOINT)
             (SETQ ODE (REVAL1 ODE T))
             (PROG (S)
               (SETQ S SUBLIST)
              LAB
               (COND ((NULL S) (RETURN NIL)))
               ((LAMBDA (S) (SETQ ODE (SUBST (CAR S) (CDR S) ODE))) (CAR S))
               (SETQ S (CDR S))
               (GO LAB)))))
         (RETURN ODE)))
     DEPL*)) 
(PUT 'ODESOLVE-SYSTEM 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-SYSTEM 'DEFINED-ON-LINE '570) 
(PUT 'ODESOLVE-SYSTEM 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-SYSTEM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SYSTEM (ODE Y X)
    (LIST 'ODESOLVE-SYSTEM (CONS 'LIST ODE) (CONS 'LIST Y) X)) 
(AEVAL (OPERATOR (LIST 'ODESOLVE-SYSTEM))) 
(PUT 'ODESOLVE-DELAY-CHECK 'NUMBER-OF-ARGS 2) 
(PUT 'ODESOLVE-DELAY-CHECK 'DEFINED-ON-LINE '575) 
(PUT 'ODESOLVE-DELAY-CHECK 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-DELAY-CHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-DELAY-CHECK (ODE Y)
    (PROG (ODELIST)
      (SETQ ODELIST (COND ((EQCAR ODE 'LIST) (CDR ODE)) (T (CONS ODE NIL))))
      (PROG (ODE)
        (SETQ ODE ODELIST)
       LAB
        (COND ((NULL ODE) (RETURN NIL)))
        ((LAMBDA (ODE)
           ((LAMBDA (Y_OP)
              (PROG (KNL)
                (SETQ KNL (KERNELS (CAR (SIMP ODE))))
               LAB
                (COND ((NULL KNL) (RETURN NIL)))
                ((LAMBDA (KNL)
                   (PROG (YY)
                     (SETQ YY (GET_OP_KNL Y_OP KNL))
                    LAB
                     (COND ((NULL YY) (RETURN NIL)))
                     ((LAMBDA (YY)
                        (COND
                         ((NOT (EQ YY Y))
                          (MSGPRI "Arguments of" Y_OP "differ --"
                                  "solving delay equations is not implemented."
                                  T))))
                      (CAR YY))
                     (SETQ YY (CDR YY))
                     (GO LAB)))
                 (CAR KNL))
                (SETQ KNL (CDR KNL))
                (GO LAB)))
            (CAR Y)))
         (CAR ODE))
        (SETQ ODE (CDR ODE))
        (GO LAB)))) 
(PUT 'ODESOLVE-WITH-CONDS 'NUMBER-OF-ARGS 4) 
(FLAG '(ODESOLVE-WITH-CONDS) 'OPFN) 
(PUT 'ODESOLVE-WITH-CONDS 'DEFINED-ON-LINE '607) 
(PUT 'ODESOLVE-WITH-CONDS 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-WITH-CONDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-WITH-CONDS (ODE Y X CONDS)
    (PROG (FIRST!ARBCONST ARBCONSTS)
      (SETQ FIRST!ARBCONST (AEVAL (LIST 'PLUS '!ARBCONST 1)))
      (SETK 'ODE_SOLUTION (AEVAL (ODESOLVE*0 ODE Y X)))
      (COND ((NOT (BOOLVALUE* (REVALX 'ODE_SOLUTION))) (RETURN (AEVAL 'NIL))))
      (AEVAL (TRACEODE (LIST "General solution is " 'ODE_SOLUTION)))
      (AEVAL (TRACEODE (LIST "Applying conditions " CONDS)))
      (SETQ ARBCONSTS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I (AEVAL* FIRST!ARBCONST))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (LIST 'ARBCONST I)) NIL)))
               LOOPLABEL
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) I))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST 'ARBCONST I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
         (SETQ SOLN (GETRLIST (AEVAL 'ODE_SOLUTION)))
        STARTOVER
         (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 ((LAMBDA (SOLN)
                    (AEVAL
                     (LIST 'ODESOLVE-WITH-CONDS1 SOLN Y X CONDS ARBCONSTS)))
                  (CAR SOLN)))
         (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
         (SETQ SOLN (CDR SOLN))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (GETRLIST
                  ((LAMBDA (SOLN)
                     (AEVAL
                      (LIST 'ODESOLVE-WITH-CONDS1 SOLN Y X CONDS ARBCONSTS)))
                   (CAR SOLN))))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ SOLN (CDR SOLN))
         (GO LOOPLABEL))))) 
(PUT 'ODESOLVE-WITH-CONDS1 'NUMBER-OF-ARGS 5) 
(FLAG '(ODESOLVE-WITH-CONDS1) 'OPFN) 
(PUT 'ODESOLVE-WITH-CONDS1 'DEFINED-ON-LINE '633) 
(PUT 'ODESOLVE-WITH-CONDS1 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-WITH-CONDS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-WITH-CONDS1 (SOLN Y X CONDS ARBCONSTS)
    (PROG (ARBCONSTEQNS)
      (SETQ ARBCONSTEQNS
              (PROG (COND FORALL-RESULT FORALL-ENDPTR)
                (SETQ COND (GETRLIST (AEVAL CONDS)))
               STARTOVER
                (COND ((NULL COND) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        ((LAMBDA (COND)
                           (PROG (XCOND YCOND DFCONDS ARBCONSTEQNS)
                             (SETQ XCOND (AEVAL (LIST 'FIRST COND)))
                             (SETQ COND (AEVAL (LIST 'REST COND)))
                             (SETQ YCOND (AEVAL (LIST 'FIRST COND)))
                             (COND
                              ((EVALEQUAL (AEVAL (LIST 'LHS YCOND)) (AEVAL Y))
                               (SETQ COND (AEVAL (LIST 'REST COND))))
                              (T (SETQ YCOND (AEVAL 0))))
                             (SETQ ARBCONSTEQNS
                                     (COND
                                      ((BOOLVALUE* YCOND)
                                       (AEVAL
                                        (LIST 'LIST
                                              (LIST 'SUB
                                                    (SETQ XCOND
                                                            (AEVAL
                                                             (LIST 'LIST XCOND
                                                                   YCOND)))
                                                    SOLN))))
                                      (T (AEVAL (LIST 'LIST)))))
                             (SETQ DFCONDS (AEVAL (LIST 'LIST)))
                             (WHILE
                              (EVALNEQ (AEVAL* COND) (AEVAL* (LIST 'LIST)))
                              (PROG (DFCOND RESULT)
                                (SETQ DFCOND (AEVAL* (LIST 'FIRST COND)))
                                (SETQ COND (AEVAL* (LIST 'REST COND)))
                                (SETQ DFCONDS
                                        (AEVAL* (LIST 'CONS DFCOND DFCONDS)))
                                (SETQ RESULT
                                        (AEVAL*
                                         (LIST 'SUB DFCONDS
                                               (LIST 'MAP
                                                     (LIST 'REPLACEBY Y
                                                           (LIST 'LHS DFCOND))
                                                     SOLN))))
                                (COND
                                 ((NOT (FREEOF (REVALX RESULT) (REVALX 'DF)))
                                  (AEVAL*
                                   (REDERR
                                    (REVALX "Cannot apply conditions")))))
                                (SETQ ARBCONSTEQNS
                                        (AEVAL*
                                         (LIST 'CONS (LIST 'SUB XCOND RESULT)
                                               ARBCONSTEQNS)))))
                             (RETURN (AEVAL ARBCONSTEQNS))))
                         (CAR COND)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ COND (CDR COND))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL COND) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         ((LAMBDA (COND)
                            (PROG (XCOND YCOND DFCONDS ARBCONSTEQNS)
                              (SETQ XCOND (AEVAL (LIST 'FIRST COND)))
                              (SETQ COND (AEVAL (LIST 'REST COND)))
                              (SETQ YCOND (AEVAL (LIST 'FIRST COND)))
                              (COND
                               ((EVALEQUAL (AEVAL (LIST 'LHS YCOND)) (AEVAL Y))
                                (SETQ COND (AEVAL (LIST 'REST COND))))
                               (T (SETQ YCOND (AEVAL 0))))
                              (SETQ ARBCONSTEQNS
                                      (COND
                                       ((BOOLVALUE* YCOND)
                                        (AEVAL
                                         (LIST 'LIST
                                               (LIST 'SUB
                                                     (SETQ XCOND
                                                             (AEVAL
                                                              (LIST 'LIST XCOND
                                                                    YCOND)))
                                                     SOLN))))
                                       (T (AEVAL (LIST 'LIST)))))
                              (SETQ DFCONDS (AEVAL (LIST 'LIST)))
                              (WHILE
                               (EVALNEQ (AEVAL* COND) (AEVAL* (LIST 'LIST)))
                               (PROG (DFCOND RESULT)
                                 (SETQ DFCOND (AEVAL* (LIST 'FIRST COND)))
                                 (SETQ COND (AEVAL* (LIST 'REST COND)))
                                 (SETQ DFCONDS
                                         (AEVAL* (LIST 'CONS DFCOND DFCONDS)))
                                 (SETQ RESULT
                                         (AEVAL*
                                          (LIST 'SUB DFCONDS
                                                (LIST 'MAP
                                                      (LIST 'REPLACEBY Y
                                                            (LIST 'LHS DFCOND))
                                                      SOLN))))
                                 (COND
                                  ((NOT (FREEOF (REVALX RESULT) (REVALX 'DF)))
                                   (AEVAL*
                                    (REDERR
                                     (REVALX "Cannot apply conditions")))))
                                 (SETQ ARBCONSTEQNS
                                         (AEVAL*
                                          (LIST 'CONS (LIST 'SUB XCOND RESULT)
                                                ARBCONSTEQNS)))))
                              (RETURN (AEVAL ARBCONSTEQNS))))
                          (CAR COND))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ COND (CDR COND))
                (GO LOOPLABEL)))
      (SETQ ARBCONSTS (AEVAL (LIST 'SOLVE ARBCONSTEQNS ARBCONSTS)))
      (RETURN
       (PROG (COND FORALL-RESULT FORALL-ENDPTR)
         (SETQ COND (GETRLIST (AEVAL ARBCONSTS)))
         (COND ((NULL COND) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (COND)
                             (COND
                              ((EVALEQUAL (AEVAL (LIST 'RHS SOLN)) 0)
                               (AEVAL
                                (LIST 'EQUAL
                                      (LIST 'NUM
                                            (LIST 'SUB COND (LIST 'LHS SOLN)))
                                      0)))
                              (T (AEVAL (LIST 'SUB COND SOLN)))))
                           (CAR COND))
                          NIL)))
        LOOPLABEL
         (SETQ COND (CDR COND))
         (COND ((NULL COND) (RETURN (CONS 'LIST FORALL-RESULT))))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (COND)
                     (COND
                      ((EVALEQUAL (AEVAL (LIST 'RHS SOLN)) 0)
                       (AEVAL
                        (LIST 'EQUAL
                              (LIST 'NUM (LIST 'SUB COND (LIST 'LHS SOLN)))
                              0)))
                      (T (AEVAL (LIST 'SUB COND SOLN)))))
                   (CAR COND))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'ODE-SOLN-CHECK 'NUMBER-OF-ARGS 5) 
(FLAG '(ODE-SOLN-CHECK) 'OPFN) 
(PUT 'ODE-SOLN-CHECK 'DEFINED-ON-LINE '701) 
(PUT 'ODE-SOLN-CHECK 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODE-SOLN-CHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODE-SOLN-CHECK (SOLN ODE Y X CONDS)
    (PROG (N *ALLOWDFINT *EXPANDDF)
      (SETQ *ALLOWDFINT (SETQ *EXPANDDF T))
      (SETQ ODE (AEVAL (LIST 'NUM (LIST '*EQN2A ODE))))
      (SETQ N (AEVAL (LIST 'ODE-ORDER ODE Y)))
      (COND
       ((BOOLVALUE*
         (REVALX
          (AND (EQCAR (CADR SOLN) 'LIST) (NOT (EQCAR (CADADR SOLN) 'EQUAL)))))
        (PROGN
         (COND
          ((AND (EVALEQUAL (AEVAL (LIST 'ARGLENGTH SOLN)) 2)
                (BOOLVALUE* (REVALX (LIST 'SECOND SOLN))))
           (SETQ ODE
                   (AEVAL
                    (LIST 'NUM
                          (LIST 'SUB
                                (LIST 'EQUAL Y
                                      (LIST 'PLUS Y (LIST 'SECOND SOLN)))
                                ODE))))))
         (COND
          ((EVALNEQ
            (AEVAL (LIST 'LENGTH (SETQ SOLN (AEVAL (LIST 'FIRST SOLN)))))
            (AEVAL N))
           (PROGN
            (ASSGNPRI (AEVAL "ODESolve warning - ") NIL 'FIRST)
            (ASSGNPRI (AEVAL "wrong number of functions in basis!") NIL
                      'LAST))))
         (PROG (S)
           (SETQ S (GETRLIST (AEVAL SOLN)))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((AND
                 (BOOLVALUE*
                  (SETQ S (REVALX (LIST 'SUB (LIST 'EQUAL Y S) ODE))))
                 (BOOLVALUE* (REVALX (LIST 'TRIGSIMP S))))
                (PROGN
                 (ASSGNPRI (AEVAL "ODESolve warning - ") NIL 'FIRST)
                 (ASSGNPRI (AEVAL "basis function may not satisfy ODE: ") NIL
                           NIL)
                 (ASSGNPRI (AEVAL S) NIL 'LAST)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))))
       (T
        (PROGN
         (COND
          ((AND (NOT (BOOLVALUE* CONDS))
                (EVALLESSP (AEVAL (LIST 'ODESOLVE-ARBCONSTS SOLN)) (AEVAL N)))
           (PROGN
            (ASSGNPRI (AEVAL "ODESolve warning - ") NIL 'FIRST)
            (ASSGNPRI
             (AEVAL "too few arbitrary constants in general solution!") NIL
             'LAST))))
         (PROG (S)
           (SETQ S (GETRLIST (AEVAL SOLN)))
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (COND
               ((BOOLVALUE* (REVALX (LIST 'ODE-COMP-SOLN-FAILS S ODE Y X N)))
                (PROGN
                 (ASSGNPRI (AEVAL "ODESolve warning - ") NIL 'FIRST)
                 (ASSGNPRI (AEVAL "component solution may not satisfy ODE: ")
                           NIL NIL)
                 (ASSGNPRI (AEVAL S) NIL 'LAST)))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (AEVAL 'NIL)))))) 
(PUT 'ODE-COMP-SOLN-FAILS 'NUMBER-OF-ARGS 5) 
(FLAG '(ODE-COMP-SOLN-FAILS) 'OPFN) 
(PUT 'ODE-COMP-SOLN-FAILS 'DEFINED-ON-LINE '743) 
(PUT 'ODE-COMP-SOLN-FAILS 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODE-COMP-SOLN-FAILS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODE-COMP-SOLN-FAILS (SOLN ODE Y X N)
    (COND
     ((BOOLVALUE* (REVALX (EQCAR SOLN 'EQUAL)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL Y))
             (FREEOF (REVALX (LIST 'RHS SOLN)) (REVALX Y)))
        (COND
         ((BOOLVALUE* (SETQ ODE (REVALX (LIST 'SUB SOLN ODE))))
          (AEVAL (LIST 'TRIGSIMP ODE)))))
       ((AND (EVALEQUAL (AEVAL (LIST 'RHS SOLN)) 0)
             (EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL ODE)))
        1)
       (T
        (PROG (DERIVS DERIV)
          (SETQ DERIVS
                  (AEVAL
                   (LIST 'LIST
                         (SETQ SOLN (AEVAL (LIST 'NUM (LIST '*EQN2A SOLN)))))))
          (PROG (I)
            (SETQ I 1)
           LAB
            (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
            (SETQ DERIVS
                    (AEVAL*
                     (LIST 'CONS
                           (SETQ SOLN (AEVAL* (LIST 'NUM (LIST 'DF SOLN X))))
                           DERIVS)))
            (SETQ I
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     I))
            (GO LAB))
          (WHILE
           (AND (EVALGREATERP (AEVAL* N) 0)
                (BOOLVALUE*
                 (PROGN
                  (SETQ DERIV
                          (REVALX
                           (LIST 'SOLVE (LIST 'FIRST DERIVS)
                                 (LIST 'DF Y X N))))
                  (COND ((EVALEQUAL (REVALX DERIV) (REVALX (LIST 'LIST))) 0)
                        (T
                         (SETQ ODE
                                 (REVALX
                                  (LIST 'NUM
                                        (LIST 'SUB (LIST 'FIRST DERIV)
                                              ODE)))))))))
           (PROGN
            (SETQ N (AEVAL* (LIST 'DIFFERENCE N 1)))
            (SETQ DERIVS (AEVAL* (LIST 'REST DERIVS)))))
          (COND
           ((EVALEQUAL (AEVAL DERIV) (AEVAL (LIST 'LIST)))
            (PROGN
             (PROGN
              (ASSGNPRI (AEVAL "ODESolve warning - cannot compute ") NIL
                        'FIRST)
              (ASSGNPRI (AEVAL (LIST 'DF Y X N)) NIL 'LAST))
             (RETURN 1))))
          (SETQ DERIVS (AEVAL (LIST 'FIRST DERIVS)))
          (SETQ ODE
                  (AEVAL
                   (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY DERIVS 0))
                         ODE)))
          (RETURN (AEVAL ODE))))))
     ((BOOLVALUE* (REVALX (AND (EQCAR SOLN 'LIST) (EQCAR (CADR SOLN) 'EQUAL))))
      (PROG (XX YY P DP/DX DERIV DERIVS)
        (SETQ YY (AEVAL (LIST 'RHS (LIST 'FIRST SOLN))))
        (SETQ XX (AEVAL (LIST 'RHS (LIST 'SECOND SOLN))))
        (SETQ P (AEVAL (LIST 'THIRD SOLN)))
        (SETQ DP/DX (AEVAL (LIST 'QUOTIENT 1 (LIST 'DF XX P))))
        (SETQ DERIVS (AEVAL (LIST 'LIST (SETQ DERIV (AEVAL YY)))))
        (PROG (I)
          (SETQ I 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
          (SETQ DERIVS
                  (AEVAL*
                   (LIST 'CONS
                         (SETQ DERIV
                                 (AEVAL*
                                  (LIST 'TIMES DP/DX (LIST 'DF DERIV P))))
                         DERIVS)))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB))
        (WHILE
         (AND (EVALGREATERP (AEVAL* N) 0)
              (BOOLVALUE*
               (SETQ ODE
                       (REVALX
                        (LIST 'NUM
                              (LIST 'SUB
                                    (LIST 'EQUAL (LIST 'DF Y X N)
                                          (LIST 'FIRST DERIVS))
                                    ODE))))))
         (PROGN
          (SETQ N (AEVAL* (LIST 'DIFFERENCE N 1)))
          (SETQ DERIVS (AEVAL* (LIST 'REST DERIVS)))))
        (RETURN
         (AEVAL (LIST 'SUB (LIST 'EQUAL Y YY) (LIST 'EQUAL X XX) ODE)))))
     (T
      (PROGN
       (ASSGNPRI (AEVAL "ODESolve warning - invalid solution type: ") NIL
                 'FIRST)
       (ASSGNPRI (AEVAL SOLN) NIL 'LAST))))) 
(FLUID '(ODESOLVE-ARBCONST-ARGS)) 
(FLAG '(ODESOLVE-ARBCONSTS) 'OPFN) 
(PUT 'ODESOLVE-ARBCONSTS 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-ARBCONSTS 'DEFINED-ON-LINE '809) 
(PUT 'ODESOLVE-ARBCONSTS 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-ARBCONSTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-ARBCONSTS (U)
    (PROG (ODESOLVE-ARBCONST-ARGS)
      (ODESOLVE-ARBCONSTS1 U)
      (RETURN (LENGTH ODESOLVE-ARBCONST-ARGS)))) 
(PUT 'ODESOLVE-ARBCONSTS1 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-ARBCONSTS1 'DEFINED-ON-LINE '816) 
(PUT 'ODESOLVE-ARBCONSTS1 'DEFINED-IN-FILE 'ODESOLVE/ODEINTFC.RED) 
(PUT 'ODESOLVE-ARBCONSTS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-ARBCONSTS1 (U)
    (COND
     ((NOT (ATOM U))
      (COND
       ((EQ (CAR U) 'ARBCONST)
        (COND
         ((NOT (MEMBER (CADR U) ODESOLVE-ARBCONST-ARGS))
          (SETQ ODESOLVE-ARBCONST-ARGS
                  (CONS (CADR U) ODESOLVE-ARBCONST-ARGS)))))
       (T
        (PROGN (ODESOLVE-ARBCONSTS1 (CAR U)) (ODESOLVE-ARBCONSTS1 (CDR U)))))))) 
(ENDMODULE) 