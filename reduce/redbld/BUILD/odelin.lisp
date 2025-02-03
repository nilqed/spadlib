(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODELIN)) 
(GLOBAL '(ODESOLVE_BEFORE_LIN_HOOK ODESOLVE_AFTER_LIN_HOOK)) 
(PUT 'ODESOLVE-LINEAR 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-LINEAR) 'OPFN) 
(PUT 'ODESOLVE-LINEAR 'DEFINED-ON-LINE '83) 
(PUT 'ODESOLVE-LINEAR 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINEAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINEAR (ODE Y X)
    (PROG (REDUCED_ODE AUXVAR AUXEQN ODECOEFFS FIRST_ARB SOLUTION DRIVER)
      (SETQ DRIVER (AEVAL (LIST 'MINUS (LIST 'SUB (LIST 'EQUAL Y 0) ODE))))
      (SETQ REDUCED_ODE (AEVAL (LIST 'PLUS ODE DRIVER)))
      (SETQ AUXVAR (AEVAL (GENSYM)))
      (SETQ AUXEQN
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'SUB
                           (LIST 'EQUAL Y
                                 (LIST 'EXPT 'E (LIST 'TIMES AUXVAR X)))
                           REDUCED_ODE)
                     (LIST 'EXPT 'E (LIST 'TIMES AUXVAR X)))))
      (SETQ ODECOEFFS (AEVAL (LIST 'COEFF AUXEQN AUXVAR)))
      (AEVAL (TRACEODE (LIST "This is a linear ODE of order " HIPOW* ".")))
      (SETQ FIRST_ARB (AEVAL (LIST 'PLUS '!ARBCONST 1)))
      (COND
       ((NOT
         (SETQ SOLUTION
                 (OR
                  (ODESOLVE-LINEAR-BASIS ODECOEFFS DRIVER Y X HIPOW* LOWPOW*)
                  (AND (NOT *ODESOLVE_FAST)
                       (PROGN
                        (TRACEODE
                         (LIST
                          "But ODESolve cannot solve it using linear techniques, so ..."))
                        ((LAMBDA (*ODESOLVE_BASIS)
                           (ODESOLVE-INTERCHANGE ODE Y X))
                         NIL))))))
        (RETURN NIL)))
      (RETURN
       (COND
        ((BOOLVALUE* (REVALX *ODESOLVE_BASIS))
         (COND
          ((EVALEQUAL (AEVAL (LIST 'PART SOLUTION 1 0)) (AEVAL 'EQUAL))
           (COND
            ((AND
              (EVALEQUAL (AEVAL (LIST 'LHS (LIST 'FIRST SOLUTION))) (AEVAL Y))
              (BOOLVALUE*
               (SETQ AUXEQN
                       (REVALX
                        (LIST 'ODESOLVE-LINCOMB2BASIS
                              (LIST 'RHS (LIST 'FIRST SOLUTION)) FIRST_ARB
                              '!ARBCONST)))))
             (AEVAL AUXEQN))
            (T
             (PROGN
              (ASSGNPRI
               (AEVAL
                "***** Cannot convert nonlinear combination solution to basis!")
               NIL 'ONLY)
              (AEVAL SOLUTION)))))
          (T (AEVAL SOLUTION))))
        ((EVALEQUAL (AEVAL (LIST 'PART SOLUTION 1 0)) (AEVAL 'LIST))
         (AEVAL
          (LIST 'LIST
                (LIST 'EQUAL Y (LIST 'ODESOLVE-BASIS2LINCOMB SOLUTION)))))
        (T (AEVAL SOLUTION)))))) 
(PUT 'ODESOLVE-LINEAR-BASIS 'NUMBER-OF-ARGS 6) 
(FLAG '(ODESOLVE-LINEAR-BASIS) 'OPFN) 
(PUT 'ODESOLVE-LINEAR-BASIS 'DEFINED-ON-LINE '130) 
(PUT 'ODESOLVE-LINEAR-BASIS 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINEAR-BASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINEAR-BASIS (ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER)
    (COND ((EQUAL ODE_ORDER 1) (ODESOLVE-LINEAR1 ODECOEFFS DRIVER X))
          (T
           (OR
            (ODESOLVE-RUN-HOOK 'ODESOLVE_BEFORE_LIN_HOOK
             (LIST ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER))
            (ODESOLVE-LINEARN ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER NIL)
            (ODESOLVE-RUN-HOOK 'ODESOLVE_AFTER_LIN_HOOK
             (LIST ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER)))))) 
(PUT 'ODESOLVE-LINEAR-BASIS-RECURSIVE 'NUMBER-OF-ARGS 6) 
(FLAG '(ODESOLVE-LINEAR-BASIS-RECURSIVE) 'OPFN) 
(PUT 'ODESOLVE-LINEAR-BASIS-RECURSIVE 'DEFINED-ON-LINE '146) 
(PUT 'ODESOLVE-LINEAR-BASIS-RECURSIVE 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINEAR-BASIS-RECURSIVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINEAR-BASIS-RECURSIVE (ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER)
    (COND ((EQUAL ODE_ORDER 1) (ODESOLVE-LINEAR1 ODECOEFFS DRIVER X))
          (T (ODESOLVE-LINEARN ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER T)))) 
(PUT 'ODESOLVE-LINEAR1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-LINEAR1) 'OPFN) 
(PUT 'ODESOLVE-LINEAR1 'DEFINED-ON-LINE '161) 
(PUT 'ODESOLVE-LINEAR1 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINEAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINEAR1 (ODECOEFFS DRIVER X)
    (PROG (A P Q)
      (SETQ A (AEVAL (LIST 'SECOND ODECOEFFS)))
      (SETQ P (AEVAL (LIST 'QUOTIENT (LIST 'FIRST ODECOEFFS) A)))
      (SETQ Q (AEVAL (LIST 'QUOTIENT DRIVER A)))
      (RETURN
       (COND
        ((BOOLVALUE* P)
         (PROG (INTFACTOR *COMBINELOGS)
           (AEVAL
            (TRACEODE (LIST "It is solved by the integrating factor method.")))
           (SETQ *COMBINELOGS T)
           (SETQ P
                   (AEVAL
                    (LIST 'WHEREEXP
                          (LIST 'LIST
                                (LIST 'REPLACEBY (LIST 'TAN (LIST '~ X))
                                      (LIST 'QUOTIENT (LIST 'SIN X)
                                            (LIST 'COS X))))
                          P)))
           (SETQ INTFACTOR (AEVAL (LIST 'EXP (LIST 'INT P X))))
           (RETURN
            (COND
             ((BOOLVALUE* Q)
              (AEVAL
               (LIST 'LIST (LIST 'LIST (LIST 'QUOTIENT 1 INTFACTOR))
                     (LIST 'QUOTIENT
                           (LIST 'ODESOLVE-INT (LIST 'TIMES INTFACTOR Q) X)
                           INTFACTOR))))
             (T
              (AEVAL
               (LIST 'LIST (LIST 'LIST (LIST 'QUOTIENT 1 INTFACTOR)))))))))
        (T
         (PROGN
          (AEVAL (TRACEODE (LIST "It is solved by quadrature.")))
          (COND
           ((BOOLVALUE* Q)
            (AEVAL (LIST 'LIST (LIST 'LIST 1) (LIST 'ODESOLVE-INT Q X))))
           (T (AEVAL (LIST 'LIST (LIST 'LIST 1))))))))))) 
(PUT 'ODESOLVE-LINEARN 'NUMBER-OF-ARGS 7) 
(FLAG '(ODESOLVE-LINEARN) 'OPFN) 
(PUT 'ODESOLVE-LINEARN 'DEFINED-ON-LINE '203) 
(PUT 'ODESOLVE-LINEARN 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINEARN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE ODESOLVE-LINEARN (ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER RECURSIVE)
    (PROG (LCOEFF ODECOEFFS1 DRIVER1 SOLUTION)
      (COND
       ((EVALEQUAL
         (SETQ LCOEFF (AEVAL (LIST 'PART ODECOEFFS (LIST 'PLUS ODE_ORDER 1))))
         1)
        (PROGN
         (SETQ ODECOEFFS1 (AEVAL ODECOEFFS))
         (SETQ DRIVER1 (AEVAL DRIVER))))
       (T
        (PROGN
         (SETQ ODECOEFFS1
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C (GETRLIST (AEVAL ODECOEFFS)))
                   (COND ((NULL C) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (C)
                                       (AEVAL (LIST 'QUOTIENT C LCOEFF)))
                                     (CAR C))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (CDR C))
                   (COND ((NULL C) (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (C) (AEVAL (LIST 'QUOTIENT C LCOEFF)))
                             (CAR C))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ DRIVER1 (AEVAL (LIST 'QUOTIENT DRIVER LCOEFF))))))
      (COND ((BOOLVALUE* RECURSIVE) (GO A)))
      (COND
       ((FREEOF (REVALX ODECOEFFS1) (REVALX X))
        (RETURN (AEVAL (LIST 'ODESOLVE-LCC ODECOEFFS1 DRIVER1 X ODE_ORDER)))))
      (AEVAL (TRACEODE (LIST "It has non-constant coefficients.")))
      (COND
       ((BOOLVALUE*
         (SETQ SOLUTION
                 (REVALX
                  (LIST 'ODESOLVE-EULER ODECOEFFS1 DRIVER1 X ODE_ORDER))))
        (RETURN (AEVAL SOLUTION))))
      (COND
       ((AND (EVALNEQ (AEVAL MIN_ORDER) 0)
             (EVALNEQ (AEVAL ODE_ORDER) (AEVAL MIN_ORDER))
             (BOOLVALUE*
              (SETQ SOLUTION
                      (REVALX
                       (LIST 'ODELIN-REDUCE-ORDER ODECOEFFS DRIVER Y X
                             ODE_ORDER MIN_ORDER)))))
        (RETURN (AEVAL SOLUTION))))
     A
      (COND
       ((BOOLVALUE*
         (SETQ SOLUTION
                 (REVALX
                  (LIST 'ODELIN-EXACT ODECOEFFS1 DRIVER1 Y X ODE_ORDER))))
        (RETURN (AEVAL SOLUTION))))
      (COND
       ((AND (EVALNEQ (AEVAL LCOEFF) 1)
             (BOOLVALUE*
              (SETQ SOLUTION
                      (REVALX
                       (LIST 'ODELIN-EXACT ODECOEFFS DRIVER Y X ODE_ORDER)))))
        (RETURN (AEVAL SOLUTION))))
      (COND
       ((AND (EVALEQUAL (AEVAL ODE_ORDER) 2)
             (BOOLVALUE*
              (SETQ SOLUTION
                      (REVALX (LIST 'ODESOLVE-SPECFN ODECOEFFS1 DRIVER1 X)))))
        (RETURN (AEVAL SOLUTION)))))) 
(PUT 'ODESOLVE-BASIS2LINCOMB 'NUMBER-OF-ARGS 1) 
(FLAG '(ODESOLVE-BASIS2LINCOMB) 'OPFN) 
(PUT 'ODESOLVE-BASIS2LINCOMB 'DEFINED-ON-LINE '275) 
(PUT 'ODESOLVE-BASIS2LINCOMB 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-BASIS2LINCOMB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-BASIS2LINCOMB (SOLUTION)
    (PROG (LINCOMB)
      (SETQ LINCOMB
              (PROG (B FORALL-RESULT)
                (SETQ B (GETRLIST (AEVAL (LIST 'FIRST SOLUTION))))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL B) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               ((LAMBDA (B)
                                  (AEVAL
                                   (LIST 'TIMES
                                         (PROGN (AEVAL (LIST 'NEWARBCONST)))
                                         B)))
                                (CAR B))
                               FORALL-RESULT)))
                (SETQ B (CDR B))
                (GO LAB1)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'LENGTH SOLUTION)) 1)
        (SETQ LINCOMB (AEVAL (LIST 'PLUS LINCOMB (LIST 'SECOND SOLUTION))))))
      (RETURN (AEVAL LINCOMB)))) 
(PUT 'ODESOLVE-LINCOMB2BASIS 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-LINCOMB2BASIS) 'OPFN) 
(PUT 'ODESOLVE-LINCOMB2BASIS 'DEFINED-ON-LINE '286) 
(PUT 'ODESOLVE-LINCOMB2BASIS 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINCOMB2BASIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINCOMB2BASIS (LINCOMB FIRST_ARB LAST_ARB)
    (LIST 'ODESOLVE-LINCOMB2BASIS1 (LIST 'LIST) LINCOMB FIRST_ARB LAST_ARB)) 
(PUT 'ODESOLVE-LINCOMB2BASIS1 'NUMBER-OF-ARGS 4) 
(FLAG '(ODESOLVE-LINCOMB2BASIS1) 'OPFN) 
(PUT 'ODESOLVE-LINCOMB2BASIS1 'DEFINED-ON-LINE '291) 
(PUT 'ODESOLVE-LINCOMB2BASIS1 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LINCOMB2BASIS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LINCOMB2BASIS1 (BASIS LINCOMB FIRST_ARB LAST_ARB)
    (PROG (COEFFS C)
      (SETQ C (AEVAL (LIST 'ARBCONST LAST_ARB)))
      (SETQ COEFFS (AEVAL (LIST 'COEFF LINCOMB C)))
      (COND
       ((OR (EVALGREATERP (AEVAL HIPOW*) 1)
            (BOOLVALUE* (REVALX (LIST 'SMEMBER C COEFFS))))
        (RETURN (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL HIPOW*) 1)
        (PROGN
         (SETQ BASIS (AEVAL (LIST 'CONS (LIST 'SECOND COEFFS) BASIS)))
         (SETQ LINCOMB (AEVAL (LIST 'FIRST COEFFS))))))
      (RETURN
       (COND
        ((EVALGEQ (AEVAL FIRST_ARB) (AEVAL LAST_ARB))
         (AEVAL (LIST 'LIST BASIS LINCOMB)))
        (T
         (AEVAL
          (LIST 'ODESOLVE-LINCOMB2BASIS1 BASIS LINCOMB FIRST_ARB
                (LIST 'DIFFERENCE LAST_ARB 1)))))))) 
(PUT 'ODESOLVE-LCC 'NUMBER-OF-ARGS 4) 
(FLAG '(ODESOLVE-LCC) 'OPFN) 
(PUT 'ODESOLVE-LCC 'DEFINED-ON-LINE '343) 
(PUT 'ODESOLVE-LCC 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LCC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LCC (ODECOEFFS DRIVER X ODE_ORDER)
    (PROG (AUXVAR AUXEQN I AUXROOTS SOLUTIONS PI_)
      (AEVAL (TRACEODE (LIST "It has constant coefficients.")))
      (SETQ AUXVAR (AEVAL (GENSYM)))
      (SETQ I (AEVAL (MINUS 1)))
      (SETQ AUXEQN
              (PROG (C FORALL-RESULT)
                (SETQ C (GETRLIST (AEVAL ODECOEFFS)))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               ((LAMBDA (C)
                                  (AEVAL
                                   (LIST 'TIMES C
                                         (LIST 'EXPT AUXVAR
                                               (SETQ I
                                                       (AEVAL
                                                        (LIST 'PLUS I 1)))))))
                                (CAR C))
                               FORALL-RESULT)))
                (SETQ C (CDR C))
                (GO LAB1)))
      (SETQ AUXROOTS (AEVAL (LIST 'SOLVE AUXEQN AUXVAR)))
      (COND
       ((EVALNEQ (AEVAL ODE_ORDER)
                 (PROG (MULTI FORALL-RESULT)
                   (SETQ MULTI (GETRLIST (AEVAL MULTIPLICITIES*)))
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((NULL MULTI) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  ((LAMBDA (MULTI) (AEVAL MULTI)) (CAR MULTI))
                                  FORALL-RESULT)))
                   (SETQ MULTI (CDR MULTI))
                   (GO LAB1)))
        (RETURN
         (AEVAL
          (TRACEODE (LIST "But insufficient roots of auxiliary equation!"))))))
      (SETQ SOLUTIONS (AEVAL AUXROOTS))
     A
      (COND
       ((EVALNEQ (AEVAL (LIST 'LHS (LIST 'FIRST SOLUTIONS))) (AEVAL AUXVAR))
        (RETURN
         (AEVAL
          (TRACEODE (LIST "But auxiliary equation could not be solved!"))))))
      (COND
       ((EVALNEQ (SETQ SOLUTIONS (AEVAL (LIST 'REST SOLUTIONS)))
                 (AEVAL (LIST 'LIST)))
        (GO A)))
      (SETQ SOLUTIONS (AEVAL (LIST 'ODESOLVE-LCC-COMPSOLN AUXROOTS X)))
      (COND
       ((EVALEQUAL (AEVAL DRIVER) 0) (RETURN (AEVAL (LIST 'LIST SOLUTIONS)))))
      (COND
       ((NOT
         (BOOLVALUE*
          (SETQ PI_ (REVALX (LIST 'ODESOLVE-LCC-PI AUXROOTS DRIVER X)))))
        (SETQ PI_ (AEVAL (LIST 'ODESOLVE-PI SOLUTIONS DRIVER X)))))
      (RETURN (AEVAL (LIST 'LIST SOLUTIONS PI_))))) 
(PUT 'ODESOLVE-LCC-COMPSOLN 'NUMBER-OF-ARGS 2) 
(FLAG '(ODESOLVE-LCC-COMPSOLN) 'OPFN) 
(PUT 'ODESOLVE-LCC-COMPSOLN 'DEFINED-ON-LINE '373) 
(PUT 'ODESOLVE-LCC-COMPSOLN 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LCC-COMPSOLN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LCC-COMPSOLN (AUXROOTS X)
    (PROG (MULTILIST CROOTLIST ANS MULTI IMROOT EXPPART)
      (SETQ MULTILIST (AEVAL MULTIPLICITIES*))
      (SETQ CROOTLIST (AEVAL (LIST 'LIST)))
      (SETQ ANS (AEVAL (LIST 'LIST)))
      (PROG (ROOT)
        (SETQ ROOT (GETRLIST (AEVAL AUXROOTS)))
       LAB
        (COND ((NULL ROOT) (RETURN NIL)))
        ((LAMBDA (ROOT)
           (PROGN
            (SETQ ROOT (AEVAL (LIST 'RHS ROOT)))
            (SETQ MULTI (AEVAL (LIST 'FIRST MULTILIST)))
            (SETQ MULTILIST (AEVAL (LIST 'REST MULTILIST)))
            (SETQ IMROOT (AEVAL (LIST 'IMPART* ROOT)))
            (COND
             ((EVALEQUAL (AEVAL IMROOT) 0)
              (PROGN
               (SETQ EXPPART (AEVAL (LIST 'EXP (LIST 'TIMES ROOT X))))
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND
                  ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MULTI) J))
                   (RETURN NIL)))
                 (SETQ ANS
                         (AEVAL*
                          (LIST 'CONS
                                (LIST 'TIMES (LIST 'EXPT X (DIFFERENCE J 1))
                                      EXPPART)
                                ANS)))
                 (SETQ J
                         ((LAMBDA (FORALL-RESULT)
                            (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                          J))
                 (GO LAB))))
             (T
              (PROG (CONJROOT CONJMULTI)
                (SETQ CONJROOT (AEVAL (LIST 'CONJ* ROOT)))
                (SETQ CONJMULTI (AEVAL 0))
                (SETQ CROOTLIST
                        (PROG (ROOT FORALL-RESULT FORALL-ENDPTR)
                          (SETQ ROOT (GETRLIST (AEVAL CROOTLIST)))
                         STARTOVER
                          (COND ((NULL ROOT) (RETURN (MAKELIST NIL))))
                          (SETQ FORALL-RESULT
                                  ((LAMBDA (ROOT)
                                     (COND
                                      ((EVALEQUAL (AEVAL (LIST 'FIRST ROOT))
                                                  (AEVAL CONJROOT))
                                       (PROGN
                                        (SETQ CONJMULTI
                                                (AEVAL (LIST 'SECOND ROOT)))
                                        (AEVAL (LIST 'LIST))))
                                      (T (AEVAL (LIST 'LIST ROOT)))))
                                   (CAR ROOT)))
                          (SETQ FORALL-ENDPTR
                                  (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                          (SETQ ROOT (CDR ROOT))
                          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                         LOOPLABEL
                          (COND ((NULL ROOT) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (GETRLIST
                                   ((LAMBDA (ROOT)
                                      (COND
                                       ((EVALEQUAL (AEVAL (LIST 'FIRST ROOT))
                                                   (AEVAL CONJROOT))
                                        (PROGN
                                         (SETQ CONJMULTI
                                                 (AEVAL (LIST 'SECOND ROOT)))
                                         (AEVAL (LIST 'LIST))))
                                       (T (AEVAL (LIST 'LIST ROOT)))))
                                    (CAR ROOT))))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                          (SETQ ROOT (CDR ROOT))
                          (GO LOOPLABEL)))
                (COND
                 ((BOOLVALUE* CONJMULTI)
                  (PROG (MINMULTI)
                    (SETQ EXPPART
                            (AEVAL
                             (LIST 'EXP (LIST 'TIMES (LIST 'REPART* ROOT) X))))
                    (SETQ MINMULTI (AEVAL (LIST 'MIN MULTI CONJMULTI)))
                    (SETQ IMROOT (AEVAL (LIST 'ABS IMROOT)))
                    (SETQ IMROOT
                            (AEVAL
                             (LIST 'WHEREEXP
                                   (LIST 'LIST
                                         (LIST 'REPLACEBY
                                               (LIST 'ABS (LIST '~ X)) X))
                                   IMROOT)))
                    (PROG (J)
                      (SETQ J 1)
                     LAB
                      (COND
                       ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MINMULTI) J))
                        (RETURN NIL)))
                      (SETQ ANS
                              (AEVAL*
                               (LIST 'CONS
                                     (LIST 'TIMES
                                           (LIST 'EXPT X (DIFFERENCE J 1))
                                           (LIST 'COS (LIST 'TIMES IMROOT X))
                                           EXPPART)
                                     (LIST 'CONS
                                           (LIST 'TIMES
                                                 (LIST 'EXPT X
                                                       (DIFFERENCE J 1))
                                                 (LIST 'SIN
                                                       (LIST 'TIMES IMROOT X))
                                                 EXPPART)
                                           ANS))))
                      (SETQ J
                              ((LAMBDA (FORALL-RESULT)
                                 (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                               J))
                      (GO LAB))
                    (COND
                     ((EVALNEQ (AEVAL MULTI) (AEVAL CONJMULTI))
                      (PROGN
                       (SETQ MINMULTI (AEVAL (LIST 'PLUS MINMULTI 1)))
                       (SETQ EXPPART (AEVAL (LIST 'EXP (LIST 'TIMES ROOT X))))
                       (PROG (J)
                         (SETQ J (AEVAL* MINMULTI))
                        LAB
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MULTI) J))
                           (RETURN NIL)))
                         (SETQ ANS
                                 (AEVAL*
                                  (LIST 'CONS
                                        (LIST 'TIMES
                                              (LIST 'EXPT X
                                                    (LIST 'DIFFERENCE J 1))
                                              EXPPART)
                                        ANS)))
                         (SETQ J
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  J))
                         (GO LAB))
                       (SETQ EXPPART
                               (AEVAL (LIST 'EXP (LIST 'TIMES CONJROOT X))))
                       (PROG (J)
                         (SETQ J (AEVAL* MINMULTI))
                        LAB
                         (COND
                          ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* CONJMULTI) J))
                           (RETURN NIL)))
                         (SETQ ANS
                                 (AEVAL*
                                  (LIST 'CONS
                                        (LIST 'TIMES
                                              (LIST 'EXPT X
                                                    (LIST 'DIFFERENCE J 1))
                                              EXPPART)
                                        ANS)))
                         (SETQ J
                                 ((LAMBDA (FORALL-RESULT)
                                    (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                  J))
                         (GO LAB)))))))
                 (T
                  (SETQ CROOTLIST
                          (AEVAL
                           (LIST 'CONS (LIST 'LIST ROOT MULTI)
                                 CROOTLIST))))))))))
         (CAR ROOT))
        (SETQ ROOT (CDR ROOT))
        (GO LAB))
      (PROG (ROOT)
        (SETQ ROOT (GETRLIST (AEVAL CROOTLIST)))
       LAB
        (COND ((NULL ROOT) (RETURN NIL)))
        ((LAMBDA (ROOT)
           (PROGN
            (SETQ EXPPART
                    (AEVAL (LIST 'EXP (LIST 'TIMES (LIST 'FIRST ROOT) X))))
            (SETQ MULTI (AEVAL (LIST 'SECOND ROOT)))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MULTI) J)) (RETURN NIL)))
              (SETQ ANS
                      (AEVAL*
                       (LIST 'CONS
                             (LIST 'TIMES (LIST 'EXPT X (DIFFERENCE J 1))
                                   EXPPART)
                             ANS)))
              (SETQ J
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       J))
              (GO LAB))))
         (CAR ROOT))
        (SETQ ROOT (CDR ROOT))
        (GO LAB))
      (RETURN (AEVAL ANS)))) 
(SETK 'VARS-ARE-REAL
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'REPART (LIST '~ 'X)) 'X)
             (LIST 'REPLACEBY (LIST 'IMPART (LIST '~ 'X)) 0)))) 
(PUT 'REPART* 'NUMBER-OF-ARGS 1) 
(FLAG '(REPART*) 'OPFN) 
(PUT 'REPART* 'DEFINED-ON-LINE '446) 
(PUT 'REPART* 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'REPART* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPART* (U)
    (PROGN
     (SETQ U (AEVAL (LIST 'REPART U)))
     (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'VARS-ARE-REAL) U)))) 
(PUT 'IMPART* 'NUMBER-OF-ARGS 1) 
(FLAG '(IMPART*) 'OPFN) 
(PUT 'IMPART* 'DEFINED-ON-LINE '449) 
(PUT 'IMPART* 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'IMPART* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IMPART* (U)
    (PROGN
     (SETQ U (AEVAL (LIST 'IMPART U)))
     (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'VARS-ARE-REAL) U)))) 
(PUT 'CONJ* 'NUMBER-OF-ARGS 1) 
(FLAG '(CONJ*) 'OPFN) 
(PUT 'CONJ* 'DEFINED-ON-LINE '452) 
(PUT 'CONJ* 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'CONJ* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONJ* (U)
    (PROGN
     (SETQ U (AEVAL (LIST 'CONJ U)))
     (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'VARS-ARE-REAL) U)))) 
(PUT 'ODESOLVE-LCC-PI 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-LCC-PI) 'OPFN) 
(PUT 'ODESOLVE-LCC-PI 'DEFINED-ON-LINE '455) 
(PUT 'ODESOLVE-LCC-PI 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-LCC-PI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-LCC-PI (AUXROOTS DRIVER X)
    (PROG (EXP_MX MULTIPLICITIES MULTI)
      (AEVAL
       (TRACEODE
        (LIST "Constructing particular integral using `D-operator method'.")))
      (SETQ MULTIPLICITIES (AEVAL MULTIPLICITIES*))
      (WHILE
       (AND (BOOLVALUE* DRIVER)
            (EVALNEQ (AEVAL* AUXROOTS) (AEVAL* (LIST 'LIST))))
       (PROGN
        (SETQ EXP_MX
                (AEVAL*
                 (LIST 'EXP
                       (LIST 'TIMES (LIST 'RHS (LIST 'FIRST AUXROOTS)) X))))
        (SETQ DRIVER (AEVAL* (LIST 'QUOTIENT DRIVER EXP_MX)))
        (SETQ MULTI (AEVAL* (LIST 'FIRST MULTIPLICITIES)))
        (WHILE (AND (BOOLVALUE* DRIVER) (EVALGEQ (AEVAL* MULTI) 1))
               (PROGN
                (SETQ DRIVER (AEVAL* (LIST 'INT DRIVER X)))
                (COND
                 ((FREEOF (REVALX DRIVER) (REVALX 'INT))
                  (SETQ MULTI (AEVAL* (LIST 'DIFFERENCE MULTI 1))))
                 (T (SETQ DRIVER (AEVAL* 0))))))
        (SETQ DRIVER (AEVAL* (LIST 'TIMES EXP_MX DRIVER)))
        (SETQ AUXROOTS (AEVAL* (LIST 'REST AUXROOTS)))
        (SETQ MULTIPLICITIES (AEVAL* (LIST 'REST MULTIPLICITIES)))))
      (COND
       ((EVALEQUAL (AEVAL DRIVER) 0)
        (AEVAL (TRACEODE (LIST "But cannot evaluate the integrals, so ..."))))
       (T (RETURN (AEVAL DRIVER)))))) 
(PUT 'ODESOLVE-PI 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-PI) 'OPFN) 
(PUT 'ODESOLVE-PI 'DEFINED-ON-LINE '484) 
(PUT 'ODESOLVE-PI 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-PI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-PI (SOLUTIONS R X)
    (PROG (N)
      (AEVAL
       (TRACEODE
        (LIST
         "Constructing particular integral using `variation of parameters'.")))
      (RETURN
       (COND
        ((EVALEQUAL (SETQ N (AEVAL (LIST 'LENGTH SOLUTIONS))) 2)
         (PROG (Y1 Y2 W)
           (SETQ Y1 (AEVAL (LIST 'FIRST SOLUTIONS)))
           (SETQ Y2 (AEVAL (LIST 'SECOND SOLUTIONS)))
           (SETQ W
                   (AEVAL
                    (LIST 'TRIGSIMP
                          (LIST 'DIFFERENCE (LIST 'TIMES Y1 (LIST 'DF Y2 X))
                                (LIST 'TIMES Y2 (LIST 'DF Y1 X))))))
           (AEVAL (TRACEODE (LIST "The Wronskian is " W)))
           (SETQ R (AEVAL (LIST 'QUOTIENT R W)))
           (RETURN
            (AEVAL
             (LIST 'PLUS
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'ODE-INT (LIST 'TIMES Y2 R) X) Y1))
                   (LIST 'TIMES (LIST 'ODE-INT (LIST 'TIMES Y1 R) X) Y2))))))
        (T
         (PROG (WMAT YS W I)
           (SETQ WMAT (AEVAL (LIST 'LIST (SETQ YS (AEVAL SOLUTIONS)))))
           (PROG (I)
             (SETQ I 2)
            LAB
             (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I)) (RETURN NIL)))
             (SETQ WMAT
                     (AEVAL*
                      (LIST 'CONS
                            (SETQ YS
                                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ Y (GETRLIST (AEVAL* YS)))
                                      (COND ((NULL Y) (RETURN (MAKELIST NIL))))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (Y)
                                                          (AEVAL*
                                                           (LIST 'DF Y X)))
                                                        (CAR Y))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ Y (CDR Y))
                                      (COND
                                       ((NULL Y)
                                        (RETURN (CONS 'LIST FORALL-RESULT))))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (Y)
                                                  (AEVAL* (LIST 'DF Y X)))
                                                (CAR Y))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            WMAT)))
             (SETQ I
                     ((LAMBDA (FORALL-RESULT)
                        (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                      I))
             (GO LAB))
           (AEVAL (LOAD_PACKAGE (LIST 'MATRIX)))
           (SETQ WMAT (AEVAL (LIST 'LIST2MAT (LIST 'REVERSE WMAT))))
           (SETQ W (AEVAL (LIST 'TRIGSIMP (LIST 'DET WMAT))))
           (AEVAL (TRACEODE (LIST "The Wronskian is " W)))
           (SETQ R (AEVAL (LIST 'QUOTIENT R W)))
           (SETQ I (AEVAL 0))
           (RETURN
            (PROG (Y FORALL-RESULT)
              (SETQ Y (GETRLIST (AEVAL SOLUTIONS)))
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND ((NULL Y) (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             ((LAMBDA (Y)
                                (AEVAL
                                 (LIST 'TIMES
                                       (LIST 'ODE-INT
                                             (LIST 'TIMES
                                                   (LIST 'COFACTOR WMAT N
                                                         (SETQ I
                                                                 (AEVAL
                                                                  (LIST 'PLUS I
                                                                        1))))
                                                   R)
                                             X)
                                       Y)))
                              (CAR Y))
                             FORALL-RESULT)))
              (SETQ Y (CDR Y))
              (GO LAB1))))))))) 
(FLAG '(LIST2MAT) 'OPFN) 
(PUT 'LIST2MAT 'NUMBER-OF-ARGS 1) 
(PUT 'LIST2MAT 'DEFINED-ON-LINE '526) 
(PUT 'LIST2MAT 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'LIST2MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIST2MAT (M)
    (CONS 'MAT
          (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
            (SETQ ROW (CDR M))
            (COND ((NULL ROW) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (ROW) (CDR ROW)) (CAR ROW)) NIL)))
           LOOPLABEL
            (SETQ ROW (CDR ROW))
            (COND ((NULL ROW) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (ROW) (CDR ROW)) (CAR ROW)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'ODESOLVE-EULER 'NUMBER-OF-ARGS 4) 
(FLAG '(ODESOLVE-EULER) 'OPFN) 
(PUT 'ODESOLVE-EULER 'DEFINED-ON-LINE '536) 
(PUT 'ODESOLVE-EULER 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODESOLVE-EULER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EULER (ODECOEFFS DRIVER X ODE_ORDER)
    (PROG (TMP SHIFT I C SOLUTION)
      (SETQ ODECOEFFS (AEVAL (LIST 'REVERSE ODECOEFFS)))
      (SETQ TMP (AEVAL (LIST 'REST ODECOEFFS)))
      (SETQ I (AEVAL 1))
      (WHILE (EVALEQUAL (AEVAL* (LIST 'FIRST TMP)) 0)
             (PROGN
              (SETQ TMP (AEVAL* (LIST 'REST TMP)))
              (SETQ I (AEVAL* (LIST 'PLUS I 1)))))
      (SETQ TMP (AEVAL (LIST 'FIRST TMP)))
      (SETQ TMP (AEVAL (LIST 'DEN TMP)))
      (SETQ TMP (AEVAL (LIST 'REVERSE (LIST 'COEFF TMP X))))
      (COND ((EVALNEQ (AEVAL HIPOW*) (AEVAL I)) (RETURN (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'SECOND TMP)))
        (PROGN
         (SETQ SHIFT
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'SECOND TMP)
                        (LIST 'TIMES I (LIST 'FIRST TMP)))))
         (SETQ DRIVER
                 (AEVAL
                  (LIST 'SUB (LIST 'EQUAL X (LIST 'DIFFERENCE X SHIFT))
                        DRIVER))))))
      (SETQ TMP (AEVAL (LIST 'LIST (LIST 'FIRST ODECOEFFS))))
      (SETQ I (AEVAL 0))
      (SETQ ODECOEFFS (AEVAL (LIST 'REST ODECOEFFS)))
     A
      (COND
       ((EVALNEQ (AEVAL ODECOEFFS) (AEVAL (LIST 'LIST)))
        (PROGN
         (SETQ C
                 (AEVAL
                  (LIST 'TIMES (LIST 'FIRST ODECOEFFS)
                        (LIST 'EXPT (LIST 'PLUS X SHIFT)
                              (SETQ I (AEVAL (LIST 'PLUS I 1)))))))
         (COND ((NOT (FREEOF (REVALX C) (REVALX X))) (RETURN (AEVAL 'NIL))))
         (SETQ TMP (AEVAL (LIST 'CONS C TMP)))
         (SETQ ODECOEFFS (AEVAL (LIST 'REST ODECOEFFS)))
         (GO A))))
      (SETQ ODECOEFFS (AEVAL TMP))
      (AEVAL
       (TRACEODE
        (LIST "It is of the homogeneous (Euler) type "
              (COND ((BOOLVALUE* SHIFT) (AEVAL "(with shifted coefficients) "))
                    (T (AEVAL "")))
              "and is reducible to a simpler ODE ...")))
      (SETQ I (AEVAL (MINUS 2)))
      (SETQ TMP
              (PROG (C FORALL-RESULT)
                (SETQ C (GETRLIST (AEVAL ODECOEFFS)))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               ((LAMBDA (C)
                                  (PROGN
                                   (SETQ I (AEVAL (LIST 'PLUS I 1)))
                                   (AEVAL
                                    (LIST 'TIMES C
                                          (PROG (J FORALL-RESULT)
                                            (SETQ J 0)
                                            (SETQ FORALL-RESULT 1)
                                           LAB1
                                            (COND
                                             ((|AMINUSP:|
                                               (LIST 'DIFFERENCE (AEVAL* I) J))
                                              (RETURN FORALL-RESULT)))
                                            (SETQ FORALL-RESULT
                                                    (AEVAL*
                                                     (LIST 'TIMES
                                                           (AEVAL*
                                                            (LIST 'DIFFERENCE X
                                                                  J))
                                                           FORALL-RESULT)))
                                            (SETQ J
                                                    ((LAMBDA (FORALL-RESULT)
                                                       (AEVAL*
                                                        (LIST 'PLUS
                                                              FORALL-RESULT
                                                              1)))
                                                     J))
                                            (GO LAB1))))))
                                (CAR C))
                               FORALL-RESULT)))
                (SETQ C (CDR C))
                (GO LAB1)))
      (SETQ ODECOEFFS (AEVAL (LIST 'COEFF TMP X)))
      (SETQ DRIVER
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL X (LIST 'EXPT 'E X))
                     (LIST 'TIMES DRIVER (LIST 'EXPT X ODE_ORDER)))))
      (SETQ SOLUTION (AEVAL (LIST 'ODESOLVE-LCC ODECOEFFS DRIVER X ODE_ORDER)))
      (SETQ SOLUTION
              (AEVAL (LIST 'SUB (LIST 'EQUAL X (LIST 'LOG X)) SOLUTION)))
      (COND
       ((BOOLVALUE* SHIFT)
        (SETQ SOLUTION
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS X SHIFT)) SOLUTION)))))
      (RETURN (AEVAL SOLUTION)))) 
(PUT 'ODELIN-EXACT 'NUMBER-OF-ARGS 5) 
(FLAG '(ODELIN-EXACT) 'OPFN) 
(PUT 'ODELIN-EXACT 'DEFINED-ON-LINE '584) 
(PUT 'ODELIN-EXACT 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODELIN-EXACT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODELIN-EXACT (P_LIST DRIVER Y X N)
    (PROG (P_0 C Q_LIST Q CONST SOLN PI_)
      (SETQ P_0 (AEVAL (LIST 'FIRST P_LIST)))
      (SETQ P_LIST (AEVAL (LIST 'REVERSE (LIST 'REST P_LIST))))
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL P_LIST)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ C (AEVAL (LIST 'DIFFERENCE P (LIST 'DF C X)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ C (AEVAL (LIST 'DIFFERENCE P_0 (LIST 'DF C X))))
      (COND ((BOOLVALUE* C) (RETURN (AEVAL 'NIL))))
      (SETQ Q_LIST (AEVAL (LIST 'LIST)))
      (PROG (P)
        (SETQ P (GETRLIST (AEVAL P_LIST)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ Q_LIST
                   (AEVAL
                    (LIST 'CONS
                          (SETQ Q (AEVAL (LIST 'DIFFERENCE P (LIST 'DF Q X))))
                          Q_LIST))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ DRIVER
              (AEVAL
               (LIST 'PLUS (LIST 'INT DRIVER X)
                     (SETQ CONST (AEVAL (GENSYM))))))
      (AEVAL
       (TRACEODE
        (LIST "It is exact, and the following linear ODE of order "
              (LIST 'DIFFERENCE N 1) " is a first integral:")))
      (COND
       ((BOOLVALUE* (REVALX *TRODE))
        (PROGN
         (SETQ C (AEVAL Y))
         (SETQ SOLN
                 (AEVAL
                  (LIST 'PLUS (LIST 'TIMES (LIST 'FIRST Q_LIST) Y)
                        (PROG (Q FORALL-RESULT)
                          (SETQ Q (GETRLIST (AEVAL (LIST 'REST Q_LIST))))
                          (SETQ FORALL-RESULT 0)
                         LAB1
                          (COND ((NULL Q) (RETURN FORALL-RESULT)))
                          (SETQ FORALL-RESULT
                                  (AEVAL*
                                   (LIST 'PLUS
                                         ((LAMBDA (Q)
                                            (AEVAL
                                             (LIST 'TIMES Q
                                                   (SETQ C
                                                           (AEVAL
                                                            (LIST 'DF C X))))))
                                          (CAR Q))
                                         FORALL-RESULT)))
                          (SETQ Q (CDR Q))
                          (GO LAB1)))))
         (ASSGNPRI (AEVAL (LIST 'EQUAL SOLN DRIVER)) NIL 'ONLY))))
      (SETQ C (AEVAL Q_LIST))
      (SETQ SOLN
              (AEVAL
               (LIST 'ODESOLVE-LINEAR-BASIS-RECURSIVE Q_LIST DRIVER Y X
                     (LIST 'DIFFERENCE N 1) 0)))
      (SETQ PI_ (AEVAL (LIST 'SECOND SOLN)))
      (SETQ PI_ (AEVAL (LIST 'COEFF PI_ CONST)))
      (RETURN
       (COND
        ((EVALEQUAL (AEVAL HIPOW*) 1)
         (COND
          ((BOOLVALUE* (REVALX (LIST 'FIRST PI_)))
           (AEVAL
            (LIST 'LIST (LIST 'CONS (LIST 'SECOND PI_) (LIST 'FIRST SOLN))
                  (LIST 'FIRST PI_))))
          (T
           (AEVAL
            (LIST 'LIST (LIST 'CONS (LIST 'SECOND PI_) (LIST 'FIRST SOLN)))))))
        (T
         (PROGN
          (PROGN
           (ASSGNPRI (AEVAL "*** Internal error in ODELin!-Exact:") NIL 'FIRST)
           (ASSGNPRI (AEVAL " cannot separate basis functions! ") NIL 'LAST))
          (ASSGNPRI (AEVAL "(Probably caused by `noint' option.)") NIL 'ONLY)
          (AEVAL SOLN))))))) 
(PUT 'ODELIN-REDUCE-ORDER 'NUMBER-OF-ARGS 6) 
(FLAG '(ODELIN-REDUCE-ORDER) 'OPFN) 
(PUT 'ODELIN-REDUCE-ORDER 'DEFINED-ON-LINE '630) 
(PUT 'ODELIN-REDUCE-ORDER 'DEFINED-IN-FILE 'ODESOLVE/ODELIN.RED) 
(PUT 'ODELIN-REDUCE-ORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODELIN-REDUCE-ORDER (ODECOEFFS DRIVER Y X ODE_ORDER MIN_ORDER)
    (PROG (SOLUTION PI_)
      (SETQ ODE_ORDER (AEVAL (LIST 'DIFFERENCE ODE_ORDER MIN_ORDER)))
      (PROG (ORD)
        (SETQ ORD 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MIN_ORDER) ORD)) (RETURN NIL)))
        (SETQ ODECOEFFS (AEVAL* (LIST 'REST ODECOEFFS)))
        (SETQ ORD
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 ORD))
        (GO LAB))
      (AEVAL
       (TRACEODE
        (LIST "Performing trivial order reduction to give the order " ODE_ORDER
              " linear ODE with coefficients (low -- high): " ODECOEFFS)))
      (SETQ SOLUTION
              (AEVAL
               (LIST 'ODESOLVE-LINEAR-BASIS-RECURSIVE ODECOEFFS DRIVER Y X
                     ODE_ORDER 0)))
      (COND
       ((NOT (BOOLVALUE* SOLUTION))
        (PROGN
         (AEVAL
          (TRACEODE (LIST "But ODESolve cannot solve the reduced ODE! ")))
         (RETURN (AEVAL 'NIL)))))
      (AEVAL (TRACEODE (LIST "Solution of order-reduced ODE is " SOLUTION)))
      (AEVAL
       (TRACEODE
        (LIST "Restoring order, " (LIST 'REPLACEBY Y (LIST 'DF Y X MIN_ORDER))
              ", to give: " (LIST 'EQUAL (LIST 'DF Y X MIN_ORDER) SOLUTION)
              " and re-solving ...")))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'LENGTH SOLUTION)) 1)
        (SETQ PI_ (AEVAL (LIST 'SECOND SOLUTION)))))
      (SETQ SOLUTION
              (AEVAL
               (LIST 'APPEND
                     (PROG (C FORALL-RESULT FORALL-ENDPTR)
                       (SETQ C (GETRLIST (AEVAL (LIST 'FIRST SOLUTION))))
                       (COND ((NULL C) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (C)
                                           (AEVAL
                                            (LIST 'ODESOLVE-MULTI-INT C X
                                                  MIN_ORDER)))
                                         (CAR C))
                                        NIL)))
                      LOOPLABEL
                       (SETQ C (CDR C))
                       (COND ((NULL C) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (C)
                                   (AEVAL
                                    (LIST 'ODESOLVE-MULTI-INT C X MIN_ORDER)))
                                 (CAR C))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                       (SETQ I (AEVAL* (LIST 'DIFFERENCE MIN_ORDER 1)))
                       (COND
                        ((|AMINUSP:|
                          (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 0 I)))
                         (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS (AEVAL* (LIST 'EXPT X I)) NIL)))
                      LOOPLABEL
                       (SETQ I
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL*
                                   (LIST 'PLUS FORALL-RESULT (MINUS 1))))
                                I))
                       (COND
                        ((|AMINUSP:|
                          (LIST 'TIMES (MINUS 1) (LIST 'DIFFERENCE 0 I)))
                         (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS (AEVAL* (LIST 'EXPT X I)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (RETURN
       (COND
        ((BOOLVALUE* PI_)
         (AEVAL
          (LIST 'LIST SOLUTION (LIST 'ODESOLVE-MULTI-INT PI_ X MIN_ORDER))))
        (T (AEVAL (LIST 'LIST SOLUTION))))))) 
(ENDMODULE) 