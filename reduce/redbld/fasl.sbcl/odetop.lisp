(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODETOP)) 
(PUT 'ODESOLVE-RUN-HOOK 'NUMBER-OF-ARGS 2) 
(PUT 'ODESOLVE-RUN-HOOK 'DEFINED-ON-LINE '44) 
(PUT 'ODESOLVE-RUN-HOOK 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-RUN-HOOK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-RUN-HOOK (HOOK ARGS)
    (COND ((GETD HOOK) (APPLY HOOK ARGS))
          ((BOUNDP HOOK)
           (PROGN
            (SETQ HOOK (EVAL HOOK))
            (COND ((ATOM HOOK) (AND (GETD HOOK) (APPLY HOOK ARGS)))
                  (T
                   (PROG (RESULT)
                     (PROG ()
                      WHILELABEL
                       (COND
                        ((NOT
                          (AND HOOK
                               (NULL
                                (AND (GETD (CAR HOOK))
                                     (SETQ RESULT (APPLY (CAR HOOK) ARGS))))))
                         (RETURN NIL)))
                       (SETQ HOOK (CDR HOOK))
                       (GO WHILELABEL))
                     (COND (HOOK (RETURN RESULT)))))))))) 
(FLUID '(ODESOLVE-INTERCHANGE-LIST* *ODESOLVE-NORECURSE)) 
(GLOBAL '(ODESOLVE-STANDARD-X ODESOLVE-STANDARD-Y)) 
(SETQ ODESOLVE-STANDARD-X (GENSYM)) 
(SETQ ODESOLVE-STANDARD-Y (GENSYM)) 
(PUT 'ODESOLVE-STANDARDIZE 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-STANDARDIZE 'DEFINED-ON-LINE '77) 
(PUT 'ODESOLVE-STANDARDIZE 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-STANDARDIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-STANDARDIZE (ODE Y X)
    (SUBST ODESOLVE-STANDARD-Y Y
           (SUBST ODESOLVE-STANDARD-X X (PREPF (CAR (SIMP* ODE)))))) 
(PUT 'ODESIMP-INTERRUPT 'NUMBER-OF-ARGS 3) 
(PUT 'ODESIMP-INTERRUPT 'DEFINED-ON-LINE '83) 
(PUT 'ODESIMP-INTERRUPT 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESIMP-INTERRUPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESIMP-INTERRUPT (ODE Y X)
    (PROG (STD_ODE)
      (SETQ ODE (NUM (*EQN2A ODE)))
      (COND
       ((MEMBER (SETQ STD_ODE (ODESOLVE-STANDARDIZE ODE Y X))
                ODESOLVE-INTERCHANGE-LIST*)
        (PROGN
         (TRACEODE (LIST "ODE simplifier loop interrupted! "))
         (RETURN T))))
      (SETQ ODESOLVE-INTERCHANGE-LIST*
              (CONS STD_ODE ODESOLVE-INTERCHANGE-LIST*)))) 
(GLOBAL '(ODESOLVE_BEFORE_HOOK ODESOLVE_AFTER_HOOK)) 
(GLOBAL '(ODESOLVE_BEFORE_NON_HOOK ODESOLVE_AFTER_NON_HOOK)) 
(PUT 'ODESOLVE*0 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE*0) 'OPFN) 
(PUT 'ODESOLVE*0 'DEFINED-ON-LINE '104) 
(PUT 'ODESOLVE*0 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE*0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE*0 (ODE Y X)
    (PROG (*PRECISE SOLUTION *ODESOLVE-NORECURSE ODESOLVE-INTERCHANGE-LIST*
           *ODESOLVE-SOLVABLE-XY)
      (SETQ ODE (NUM (*EQN2A ODE)))
      (COND
       ((SETQ SOLUTION
                (OR (ODESOLVE-RUN-HOOK 'ODESOLVE_BEFORE_HOOK (LIST ODE Y X))
                    (ODESOLVE*1 ODE Y X)
                    (AND (NOT *ODESOLVE_FAST) (ODESOLVE-DIFF ODE Y X))
                    (ODESOLVE-RUN-HOOK 'ODESOLVE_AFTER_HOOK (LIST ODE Y X))))
        (RETURN SOLUTION)))
      (TRACEODE (LIST "ODESolve cannot solve this ODE!")))) 
(PUT 'ODESOLVE*1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE*1) 'OPFN) 
(PUT 'ODESOLVE*1 'DEFINED-ON-LINE '123) 
(PUT 'ODESOLVE*1 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE*1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE*1 (ODE Y X)
    (COND
     (*ODESOLVE-NORECURSE
      (TRACEODE (LIST "ODESolve terminated: no recursion mode!")))
     ((ODESIMP-INTERRUPT ODE Y X) NIL)
     (T
      (PROGN
       (SETQ *ODESOLVE-NORECURSE *ODESOLVE_NORECURSE)
       (TRACEODE1 (LIST "Entering top-level general recursive solver ..."))
       (COND ((ODE-LINEARP ODE Y) (ODESOLVE-LINEAR ODE Y X))
             (T
              (PROG (*ODESOLVE_BASIS ODE_FACTORS SOLNS)
                (AEVAL
                 (TRACEODE1
                  (LIST
                   "Trying to factorize nonlinear ODE algebraically ...")))
                (SETQ ODE_FACTORS (AEVAL (LIST 'FACTORIZE ODE)))
                (COND
                 ((AND (EVALEQUAL (AEVAL (LIST 'LENGTH ODE_FACTORS)) 1)
                       (EVALEQUAL
                        (AEVAL (LIST 'SECOND (LIST 'FIRST ODE_FACTORS))) 1))
                  (RETURN (AEVAL (LIST 'ODESOLVE-NONLINEAR ODE Y X)))))
                (AEVAL
                 (TRACEODE
                  (LIST
                   "This is a nonlinear ODE that factorizes algebraically "
                   "and each distinct factor ODE will be solved separately ...")))
                (SETQ SOLNS (AEVAL (LIST 'LIST)))
                (WHILE (EVALNEQ (AEVAL* ODE_FACTORS) (AEVAL* (LIST 'LIST)))
                       (PROG (FAC)
                         (COND
                          ((BOOLVALUE*
                            (REVALX
                             (LIST 'SMEMBER Y
                                   (SETQ FAC
                                           (REVALX
                                            (LIST 'FIRST
                                                  (LIST 'FIRST
                                                        ODE_FACTORS)))))))
                           (COND
                            ((BOOLVALUE*
                              (SETQ FAC (REVALX (LIST 'ODESOLVE*2* FAC Y X))))
                             (PROGN
                              (SETQ SOLNS (AEVAL* (LIST 'APPEND SOLNS FAC)))
                              (SETQ ODE_FACTORS
                                      (AEVAL* (LIST 'REST ODE_FACTORS)))))
                            (T
                             (SETQ SOLNS
                                     (SETQ ODE_FACTORS
                                             (AEVAL* (LIST 'LIST)))))))
                          (T
                           (PROGN
                            (COND
                             ((OR (BOOLVALUE* (REVALX (LIST 'DEPENDS FAC X)))
                                  (BOOLVALUE* (REVALX (LIST 'DEPENDS FAC Y))))
                              (AEVAL*
                               (MSGPRI "ODE factor" FAC "ignored" NIL NIL))))
                            (SETQ ODE_FACTORS
                                    (AEVAL* (LIST 'REST ODE_FACTORS))))))))
                (RETURN
                 (COND
                  ((EVALEQUAL (AEVAL SOLNS) (AEVAL (LIST 'LIST)))
                   (AEVAL (LIST 'ODESOLVE-EXACT* ODE Y X)))
                  (T (AEVAL SOLNS))))))))))) 
(PUT 'ODESOLVE-FIRSTORDER 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-FIRSTORDER) 'OPFN) 
(PUT 'ODESOLVE-FIRSTORDER 'DEFINED-ON-LINE '173) 
(PUT 'ODESOLVE-FIRSTORDER 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-FIRSTORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-FIRSTORDER (ODE Y X)
    (PROGN
     (SETQ ODE (NUM (*EQN2A ODE)))
     (TRACEODE (LIST (LIST 'EQUAL ODE 0)))
     (ODESOLVE*1 ODE Y X))) 
(PUT 'ODESOLVE*2* 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE*2*) 'OPFN) 
(PUT 'ODESOLVE*2* 'DEFINED-ON-LINE '188) 
(PUT 'ODESOLVE*2* 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE*2* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE*2* (ODE Y X)
    (COND
     ((BOOLVALUE* (REVALX (LIST 'SMEMBER 'DF ODE)))
      (AEVAL (LIST 'ODESOLVE*2 ODE Y X)))
     ((EVALEQUAL (AEVAL ODE) (AEVAL Y)) (AEVAL (LIST 'LIST (LIST 'EQUAL Y 0))))
     (T (AEVAL (LIST 'SOLVE ODE Y))))) 
(PUT 'ODESOLVE*2 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE*2) 'OPFN) 
(PUT 'ODESOLVE*2 'DEFINED-ON-LINE '196) 
(PUT 'ODESOLVE*2 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE*2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE*2 (ODE Y X)
    (PROGN
     (TRACEODE1
      (LIST "Entering top-level recursive solver "
            "without algebraic factorization ..."))
     (TRACEODE (LIST (LIST 'EQUAL ODE 0)))
     (COND ((ODE-LINEARP ODE Y) (ODESOLVE-LINEAR ODE Y X))
           (T (ODESOLVE-NONLINEAR ODE Y X))))) 
(PUT 'ODESOLVE-NONLINEAR 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-NONLINEAR) 'OPFN) 
(PUT 'ODESOLVE-NONLINEAR 'DEFINED-ON-LINE '215) 
(PUT 'ODESOLVE-NONLINEAR 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-NONLINEAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-NONLINEAR (ODE Y X)
    (PROG (ODE_ORDER)
      (SETQ ODE_ORDER (ODE-ORDER ODE Y))
      (TRACEODE (LIST "This is a nonlinear ODE of order " ODE_ORDER "."))
      (RETURN
       (OR
        (ODESOLVE-RUN-HOOK 'ODESOLVE_BEFORE_NON_HOOK (LIST ODE Y X ODE_ORDER))
        (COND ((EQUAL ODE_ORDER 1) (ODESOLVE-NONLINEAR1 ODE Y X))
              (T (ODESOLVE-NONLINEARN ODE Y X)))
        (ODESOLVE-EXACT ODE Y X ODE_ORDER)
        (AND (NOT *ODESOLVE_FAST) (ODESOLVE-ALG-SOLVE ODE Y X))
        (AND (NOT *ODESOLVE_FAST) (ODESOLVE-INTERCHANGE ODE Y X))
        (ODESOLVE-RUN-HOOK 'ODESOLVE_AFTER_NON_HOOK
         (LIST ODE Y X ODE_ORDER)))))) 
(PUT 'ODESOLVE-INTERCHANGE 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-INTERCHANGE 'DEFINED-ON-LINE '236) 
(PUT 'ODESOLVE-INTERCHANGE 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-INTERCHANGE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-INTERCHANGE (ODE Y X)
    (COND
     (*ODESOLVE_NOSWAP
      (TRACEODE (LIST "ODESolve terminated: no variable swap mode!")))
     (T
      ((LAMBDA (DEPL*)
         (PROG (*PRECISE)
           (TRACEODE
            (LIST "Interchanging dependent and independent variables ..."))
           (DEPEND1 X Y T)
           (PROG (RULES)
             (SETQ RULES
                     (AEVAL
                      (LIST 'LIST
                            (LIST 'REPLACEBY
                                  (LIST 'ODESOLVE-DF Y X (LIST '~ 'N))
                                  (LIST 'WHEN
                                        (LIST 'TIMES
                                              (LIST 'QUOTIENT 1
                                                    (LIST 'ODESOLVE-DF X Y))
                                              (LIST 'ODESOLVE-DF
                                                    (LIST 'ODESOLVE-DF Y X
                                                          (LIST 'DIFFERENCE 'N
                                                                1))
                                                    Y))
                                        (LIST 'GREATERP 'N 1)))
                            (LIST 'REPLACEBY (LIST 'ODESOLVE-DF Y X)
                                  (LIST 'QUOTIENT 1 (LIST 'ODESOLVE-DF X Y)))
                            (LIST 'REPLACEBY (LIST 'ODESOLVE-DF Y X 1)
                                  (LIST 'QUOTIENT 1
                                        (LIST 'ODESOLVE-DF X Y))))))
             (SETQ ODE (AEVAL (LIST 'SUB (LIST 'EQUAL 'DF 'ODESOLVE-DF) ODE)))
             (SETQ ODE (AEVAL (LIST 'WHEREEXP (LIST 'LIST RULES) ODE)))
             (SETQ ODE
                     (AEVAL
                      (LIST 'NUM
                            (LIST 'SUB (LIST 'EQUAL 'ODESOLVE-DF 'DF) ODE)))))
           (DEPEND1 Y X NIL)
           (TRACEODE (LIST ODE))
           (SETQ ODE (ODESOLVE*1 ODE X Y))
           (COND
            (ODE
             (RETURN
              (CONS 'LIST
                    (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
                      (SETQ SOLN (CDR ODE))
                     STARTOVER
                      (COND ((NULL SOLN) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (SOLN)
                                 (COND ((SMEMBER Y SOLN) (LIST SOLN))
                                       (T (LIST))))
                               (CAR SOLN)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ SOLN (CDR SOLN))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (SOLN)
                                 (COND ((SMEMBER Y SOLN) (LIST SOLN))
                                       (T (LIST))))
                               (CAR SOLN)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ SOLN (CDR SOLN))
                      (GO LOOPLABEL))))))))
       DEPL*)))) 
(PUT 'ODESOLVE-EXACT* 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-EXACT*) 'OPFN) 
(PUT 'ODESOLVE-EXACT* 'DEFINED-ON-LINE '291) 
(PUT 'ODESOLVE-EXACT* 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-EXACT* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EXACT* (ODE Y X)
    (LIST 'ODESOLVE-EXACT ODE Y X (LIST 'ODE-ORDER ODE Y))) 
(PUT 'ODESOLVE-EXACT 'NUMBER-OF-ARGS 4) 
(FLAG '(ODESOLVE-EXACT) 'OPFN) 
(PUT 'ODESOLVE-EXACT 'DEFINED-ON-LINE '296) 
(PUT 'ODESOLVE-EXACT 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-EXACT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EXACT (ODE Y X ODE_ORDER)
    (PROG (C DEN_ODE RESULT)
      (AEVAL (TRACEODE1 (LIST "Checking for an exact ode ...")))
      (SETQ C (AEVAL (LIST 'COEFF (LIST 'NUM ODE) (LIST 'DF Y X ODE_ORDER))))
      (SETQ DEN_ODE (AEVAL (LIST 'DEN ODE)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'DEPENDS DEN_ODE X))))
        (SETQ DEN_ODE (AEVAL 0))))
      (COND
       ((OR (EVALNEQ (AEVAL (LIST 'LENGTH C)) 2)
            (BOOLVALUE* (REVALX (LIST 'SMEMBER (LIST 'DF Y X ODE_ORDER) C))))
        (RETURN (AEVAL 'NIL))))
      (RETURN
       (COND
        ((EVALEQUAL (AEVAL ODE_ORDER) 1)
         (AEVAL (ODESOLVE-EXACT-1 C DEN_ODE Y X)))
        ((EVALEQUAL (AEVAL ODE_ORDER) 2)
         (AEVAL (ODESOLVE-EXACT-2 C DEN_ODE Y X))))))) 
(PUT 'ODESOLVE-EXACT-1 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-EXACT-1 'DEFINED-ON-LINE '315) 
(PUT 'ODESOLVE-EXACT-1 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-EXACT-1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EXACT-1 (C DEN_ODE Y X)
    ((LAMBDA (DEPL*)
       (PROG (M N)
         (SETQ M (AEVAL (LIST 'FIRST C)))
         (SETQ N (AEVAL (LIST 'SECOND C)))
         (DEPEND1 Y X NIL)
         (COND
          ((AND
            (BOOLVALUE*
             (REVALX (LIST 'DIFFERENCE (LIST 'DF M Y) (LIST 'DF N X))))
            (OR (NOT (BOOLVALUE* DEN_ODE))
                (BOOLVALUE*
                 (REVALX
                  (LIST 'DIFFERENCE
                        (LIST 'DF (SETQ M (REVALX (LIST 'QUOTIENT M DEN_ODE)))
                              Y)
                        (LIST 'DF (SETQ N (REVALX (LIST 'QUOTIENT N DEN_ODE)))
                              X))))))
           (RETURN (AEVAL 'NIL))))
         (AEVAL (TRACEODE (LIST "It is exact and is solved by quadrature.")))
         (RETURN
          (AEVAL (LIST 'LIST (LIST 'EQUAL (LIST 'EXACT1_PDE M N Y X) 0))))))
     DEPL*)) 
(PUT 'EXACT1_PDE 'NUMBER-OF-ARGS 4) 
(FLAG '(EXACT1_PDE) 'OPFN) 
(PUT 'EXACT1_PDE 'DEFINED-ON-LINE '329) 
(PUT 'EXACT1_PDE 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'EXACT1_PDE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXACT1_PDE (M N Y X)
    (PROG (INT_M)
      (SETQ INT_M (AEVAL (LIST 'INT M X)))
      (RETURN
       (AEVAL
        (LIST 'NUM
              (LIST 'PLUS INT_M
                    (LIST 'INT (LIST 'DIFFERENCE N (LIST 'DF INT_M Y)) Y)
                    (LIST 'NEWARBCONST))))))) 
(PUT 'ODESOLVE-EXACT-2 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-EXACT-2 'DEFINED-ON-LINE '339) 
(PUT 'ODESOLVE-EXACT-2 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-EXACT-2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EXACT-2 (C DEN_ODE Y X)
    ((LAMBDA (DEPL*)
       (PROG (P F G H FIRST_INT H_X H_Y)
         (SETQ P (AEVAL (LIST 'GENSYM)))
         (SETQ F
                 (AEVAL
                  (LIST 'SUB (LIST 'EQUAL (LIST 'DF Y X) P) (LIST 'SECOND C))))
         (SETQ G
                 (AEVAL
                  (LIST 'SUB (LIST 'EQUAL (LIST 'DF Y X) P) (LIST 'FIRST C))))
         (DEPEND1 Y X NIL)
         (COND
          ((AND (BOOLVALUE* (REVALX (LIST 'ODESOLVE-EXACT-2-TEST F G P Y X)))
                (OR (NOT (BOOLVALUE* DEN_ODE))
                    (BOOLVALUE*
                     (REVALX
                      (LIST 'ODESOLVE-EXACT-2-TEST
                            (SETQ F (REVALX (LIST 'QUOTIENT F DEN_ODE)))
                            (SETQ G (REVALX (LIST 'QUOTIENT G DEN_ODE))) P Y
                            X)))))
           (RETURN (AEVAL 'NIL))))
         (AEVAL
          (TRACEODE
           (LIST "It is exact and a first integral can be constructed ...")))
         (SETQ H (AEVAL (LIST 'GENSYM)))
         (DEPEND1 H X T)
         (DEPEND1 H Y T)
         (SETQ FIRST_INT (AEVAL (LIST 'PLUS (LIST 'INT F P) H)))
         (SETQ C
                 (AEVAL
                  (LIST 'PLUS (LIST 'DF FIRST_INT X)
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES (LIST 'DF FIRST_INT Y) P) G))))
         (SETQ C (AEVAL (LIST 'COEFF (LIST 'NUM C) P)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'LENGTH C)) 2)
               (BOOLVALUE* (REVALX (LIST 'DEPENDS C P))))
           (RETURN
            (AEVAL
             (TRACEODE
              (LIST
               "but ODESolve cannot determine the arbitrary function!"))))))
         (SETQ H_X (AEVAL (LIST 'COEFF (LIST 'FIRST C) (LIST 'DF H X))))
         (SETQ H_X
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'FIRST H_X)
                              (LIST 'SECOND H_X)))))
         (SETQ H_Y (AEVAL (LIST 'COEFF (LIST 'SECOND C) (LIST 'DF H Y))))
         (SETQ H_Y
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'FIRST H_Y)
                              (LIST 'SECOND H_Y)))))
         (SETQ H_X (AEVAL (LIST 'EXACT1_PDE H_X H_Y Y X)))
         (DEPEND1 Y X T)
         (SETQ FIRST_INT
                 (AEVAL
                  (LIST 'SUB (LIST 'EQUAL H H_X) (LIST 'EQUAL P (LIST 'DF Y X))
                        FIRST_INT)))
         (SETQ FIRST_INT (AEVAL (LIST 'ODESOLVE-FIRSTORDER FIRST_INT Y X)))
         (RETURN
          (COND
           ((BOOLVALUE* FIRST_INT)
            (AEVAL (LIST 'ODESOLVE-SIMP-ARBCONSTS FIRST_INT Y X)))
           (T (AEVAL (TRACEODE (LIST "But ODESolve cannot solve it!"))))))))
     DEPL*)) 
(PUT 'ODESOLVE-EXACT-2-TEST 'NUMBER-OF-ARGS 5) 
(FLAG '(ODESOLVE-EXACT-2-TEST) 'OPFN) 
(PUT 'ODESOLVE-EXACT-2-TEST 'DEFINED-ON-LINE '377) 
(PUT 'ODESOLVE-EXACT-2-TEST 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-EXACT-2-TEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EXACT-2-TEST (F G P Y X)
    (COND
     ((OR
       (BOOLVALUE*
        (REVALX
         (LIST 'DIFFERENCE
               (LIST 'PLUS (LIST 'DF F X 2) (LIST 'TIMES 2 P (LIST 'DF F X Y))
                     (LIST 'TIMES (LIST 'EXPT P 2) (LIST 'DF F Y 2)))
               (LIST 'PLUS (LIST 'DF G X P)
                     (LIST 'DIFFERENCE (LIST 'TIMES P (LIST 'DF G Y P))
                           (LIST 'DF G Y))))))
       (BOOLVALUE*
        (REVALX
         (LIST 'DIFFERENCE
               (LIST 'PLUS (LIST 'DF F X P) (LIST 'TIMES P (LIST 'DF F Y P))
                     (LIST 'TIMES 2 (LIST 'DF F Y)))
               (LIST 'DF G P 2)))))
      1))) 
(SWITCH (LIST (LIST 'EQUAL 'ODESOLVE_DIFF 'ON))) 
(FLUID '(*ARBVARS)) 
(PUT 'ODESOLVE-DIFF 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-DIFF) 'OPFN) 
(PUT 'ODESOLVE-DIFF 'DEFINED-ON-LINE '390) 
(PUT 'ODESOLVE-DIFF 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-DIFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-DIFF (ODE Y X)
    (COND
     ((BOOLVALUE* (REVALX *ODESOLVE_DIFF))
      (PROG (ODE_FACTORS SOLNS)
        (AEVAL (LOAD_PACKAGE (LIST 'SOLVE)))
        (AEVAL
         (TRACEODE1
          (LIST "Trying to factorize derivative of ODE algebraically ...")))
        (SETQ ODE_FACTORS
                (AEVAL (LIST 'FACTORIZE (LIST 'NUM (LIST 'DF ODE X)))))
        (COND
         ((AND (EVALEQUAL (AEVAL (LIST 'LENGTH ODE_FACTORS)) 1)
               (EVALEQUAL (AEVAL (LIST 'SECOND (LIST 'FIRST ODE_FACTORS))) 1))
          (RETURN (AEVAL 'NIL))))
        (AEVAL
         (TRACEODE
          (LIST "The derivative of the ODE factorizes algebraically "
                "and each distinct factor ODE will be solved separately ...")))
        (SETQ SOLNS (AEVAL (LIST 'LIST)))
        (WHILE (EVALNEQ (AEVAL* ODE_FACTORS) (AEVAL* (LIST 'LIST)))
               (PROG (FAC DERIV_ORDERS FIRST!ARBCONST ARBCONSTS *ARBVARS)
                 (SETQ FAC (AEVAL* (LIST 'FIRST (LIST 'FIRST ODE_FACTORS))))
                 (SETQ ODE_FACTORS (AEVAL* (LIST 'REST ODE_FACTORS)))
                 (SETQ DERIV_ORDERS (AEVAL* (LIST 'GET_DERIV_ORDERS FAC Y)))
                 (COND
                  ((EVALEQUAL (AEVAL* DERIV_ORDERS) (AEVAL* (LIST 'LIST)))
                   (RETURN (AEVAL* 'NIL))))
                 (COND
                  ((EVALEQUAL (AEVAL* DERIV_ORDERS) (AEVAL* (LIST 'LIST 0)))
                   (RETURN
                    (PROG (S)
                      (SETQ S (GETRLIST (AEVAL* (LIST 'SOLVE FAC Y))))
                     LAB
                      (COND ((NULL S) (RETURN NIL)))
                      ((LAMBDA (S)
                         (COND
                          ((EVALEQUAL (AEVAL* (LIST 'SUB S ODE)) 0)
                           (SETQ SOLNS
                                   (AEVAL*
                                    (LIST 'CONS (LIST 'EQUAL S 0) SOLNS))))))
                       (CAR S))
                      (SETQ S (CDR S))
                      (GO LAB)))))
                 (SETQ FIRST!ARBCONST (AEVAL* (LIST 'PLUS '!ARBCONST 1)))
                 (SETQ FAC (AEVAL* (LIST 'ODESOLVE*2 FAC Y X)))
                 (COND
                  ((NOT (BOOLVALUE* FAC))
                   (RETURN
                    (SETQ SOLNS (SETQ ODE_FACTORS (AEVAL* (LIST 'LIST)))))))
                 (SETQ ARBCONSTS
                         (PROG (I FORALL-RESULT FORALL-ENDPTR)
                           (SETQ I (AEVAL* FIRST!ARBCONST))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) I))
                             (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS (AEVAL* (LIST 'ARBCONST I))
                                                 NIL)))
                          LOOPLABEL
                           (SETQ I
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    I))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* '!ARBCONST) I))
                             (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS (AEVAL* (LIST 'ARBCONST I)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (PROG (SOLN)
                   (SETQ SOLN (GETRLIST (AEVAL* FAC)))
                  LAB
                   (COND ((NULL SOLN) (RETURN NIL)))
                   ((LAMBDA (SOLN)
                      (PROG (S)
                        (SETQ S
                                (GETRLIST
                                 (AEVAL*
                                  (LIST 'SOLVE (LIST 'SUB SOLN ODE)
                                        ARBCONSTS))))
                       LAB
                        (COND ((NULL S) (RETURN NIL)))
                        ((LAMBDA (S)
                           (SETQ SOLNS
                                   (AEVAL*
                                    (LIST 'CONS (LIST 'SUB S SOLN) SOLNS))))
                         (CAR S))
                        (SETQ S (CDR S))
                        (GO LAB)))
                    (CAR SOLN))
                   (SETQ SOLN (CDR SOLN))
                   (GO LAB))))
        (COND
         ((EVALNEQ (AEVAL SOLNS) (AEVAL (LIST 'LIST))) (RETURN (AEVAL SOLNS))))
        (AEVAL (TRACEODE (LIST "... but cannot solve all factor ODEs."))))))) 
(PUT 'ODESOLVE-ALG-SOLVE 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-ALG-SOLVE) 'OPFN) 
(PUT 'ODESOLVE-ALG-SOLVE 'DEFINED-ON-LINE '429) 
(PUT 'ODESOLVE-ALG-SOLVE 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODESOLVE-ALG-SOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-ALG-SOLVE (ODE Y X)
    (PROG (DERIV L R D ROOT_ODES SOLNS *FULLROOTS *TRIGFORM *PRECISE)
      (AEVAL
       (TRACEODE1
        (LIST "Trying to solve algebraically for a single derivative ...")))
      (SETQ DERIV (AEVAL (LIST 'DELETE 0 (LIST 'GET_DERIV_ORDERS ODE Y))))
      (COND ((EVALNEQ (AEVAL (LIST 'LENGTH DERIV)) 1) (RETURN (AEVAL 'NIL))))
      (SETQ DERIV (AEVAL (LIST 'DF Y X (LIST 'FIRST DERIV))))
      (COND
       ((NOT
         (OR
          (BOOLVALUE*
           (REVALX
            (LIST 'SMEMBER DERIV (SETQ L (REVALX (LIST 'LCOF ODE DERIV))))))
          (BOOLVALUE*
           (REVALX
            (LIST 'SMEMBER DERIV
                  (SETQ R (REVALX (LIST 'REDUCT ODE DERIV))))))))
        (COND
         ((EVALEQUAL (SETQ D (AEVAL (LIST 'DEG ODE DERIV))) 1)
          (RETURN (AEVAL 'NIL)))
         (T
          (SETQ ROOT_ODES
                  (AEVAL
                   (LIST 'LIST
                         (LIST 'NUM
                               (LIST 'DIFFERENCE DERIV
                                     (LIST 'TIMES
                                           (LIST 'EXPT
                                                 (LIST 'MINUS
                                                       (LIST 'QUOTIENT R L))
                                                 (LIST 'QUOTIENT 1 D))
                                           (LIST 'NEWROOT_OF_UNITY D))))))))))
       (T
        (PROGN
         (SETQ ROOT_ODES (AEVAL (LIST 'SOLVE ODE DERIV)))
         (COND
          ((NOT
            (OR (EVALGREATERP (AEVAL (LIST 'LENGTH ROOT_ODES)) 1)
                (EVALGREATERP (AEVAL (LIST 'FIRST MULTIPLICITIES*)) 1)))
           (RETURN (AEVAL 'NIL))))
         (SETQ ROOT_ODES
                 (PROG (ODE FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ODE (GETRLIST (AEVAL ROOT_ODES)))
                   (COND ((NULL ODE) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ODE)
                                       (AEVAL
                                        (LIST 'NUM
                                              (COND
                                               ((BOOLVALUE*
                                                 (REVALX
                                                  (EQCAR (CADDR ODE)
                                                         'ROOT_OF)))
                                                (AEVAL
                                                 (LIST 'SUB
                                                       (LIST 'EQUAL
                                                             (LIST 'PART
                                                                   (LIST 'RHS
                                                                         ODE)
                                                                   2)
                                                             DERIV)
                                                       (LIST 'PART
                                                             (LIST 'RHS ODE)
                                                             1))))
                                               (T
                                                (AEVAL
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'LHS ODE)
                                                       (LIST 'RHS ODE))))))))
                                     (CAR ODE))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ODE (CDR ODE))
                   (COND ((NULL ODE) (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ODE)
                               (AEVAL
                                (LIST 'NUM
                                      (COND
                                       ((BOOLVALUE*
                                         (REVALX (EQCAR (CADDR ODE) 'ROOT_OF)))
                                        (AEVAL
                                         (LIST 'SUB
                                               (LIST 'EQUAL
                                                     (LIST 'PART
                                                           (LIST 'RHS ODE) 2)
                                                     DERIV)
                                               (LIST 'PART (LIST 'RHS ODE)
                                                     1))))
                                       (T
                                        (AEVAL
                                         (LIST 'DIFFERENCE (LIST 'LHS ODE)
                                               (LIST 'RHS ODE))))))))
                             (CAR ODE))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (AEVAL
       (TRACEODE
        (LIST "It can be (partially) solved algebraically "
              "for the single-order derivative "
              "and each `root ODE' will be solved separately ...")))
      (SETQ SOLNS (AEVAL (LIST 'LIST)))
      (WHILE (EVALNEQ (AEVAL* ROOT_ODES) (AEVAL* (LIST 'LIST)))
             (PROG (SOLN)
               (COND
                ((BOOLVALUE*
                  (SETQ SOLN
                          (REVALX
                           (LIST 'ODESOLVE*2 (LIST 'FIRST ROOT_ODES) Y X))))
                 (PROGN
                  (SETQ SOLNS (AEVAL* (LIST 'APPEND SOLNS SOLN)))
                  (SETQ ROOT_ODES (AEVAL* (LIST 'REST ROOT_ODES)))))
                (T (SETQ SOLNS (SETQ ROOT_ODES (AEVAL* (LIST 'LIST))))))))
      (COND
       ((EVALNEQ (AEVAL SOLNS) (AEVAL (LIST 'LIST))) (RETURN (AEVAL SOLNS)))))) 
(FLAG '(ODE-LINEARP) 'OPFN) 
(PUT 'ODE-LINEARP 'NUMBER-OF-ARGS 2) 
(PUT 'ODE-LINEARP 'DEFINED-ON-LINE '487) 
(PUT 'ODE-LINEARP 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODE-LINEARP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODE-LINEARP (ODE Y) (ODE-LIN-FORM-P (CAR (SIMP* ODE)) (*A2K Y))) 
(PUT 'ODE-LIN-FORM-P 'NUMBER-OF-ARGS 2) 
(PUT 'ODE-LIN-FORM-P 'DEFINED-ON-LINE '493) 
(PUT 'ODE-LIN-FORM-P 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODE-LIN-FORM-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODE-LIN-FORM-P (SF Y)
    (OR (OR (ATOM SF) (ATOM (CAR SF)))
        (AND (ODE-LIN-TERM-P (CAR SF) Y) (ODE-LIN-FORM-P (CDR SF) Y)))) 
(PUT 'ODE-LIN-TERM-P 'NUMBER-OF-ARGS 2) 
(PUT 'ODE-LIN-TERM-P 'DEFINED-ON-LINE '499) 
(PUT 'ODE-LIN-TERM-P 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODE-LIN-TERM-P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODE-LIN-TERM-P (ST Y)
    (PROG (KNL)
      (SETQ KNL (CAAR ST))
      (RETURN
       (COND
        ((OR (EQ KNL Y) (AND (EQCAR KNL 'DF) (EQ (CADR KNL) Y)))
         (AND (EQ (CDAR ST) 1) (NOT (DEPENDS (CDR ST) Y))))
        ((NOT (DEPENDS KNL Y)) (ODE-LIN-FORM-P (CDR ST) Y)))))) 
(FLAG '(ODE-ORDER) 'OPFN) 
(PUT 'ODE-ORDER 'NUMBER-OF-ARGS 2) 
(PUT 'ODE-ORDER 'DEFINED-ON-LINE '513) 
(PUT 'ODE-ORDER 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'ODE-ORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODE-ORDER (U Y)
    (COND ((ATOM U) 0)
          ((AND (EQ (CAR U) 'DF) (EQ (CADR U) Y))
           (COND ((CDDDR U) (CADDDR U)) (T 1)))
          (T (MAX (ODE-ORDER (CAR U) Y) (ODE-ORDER (CDR U) Y))))) 
(FLAG '(GET_DERIV_ORDERS) 'OPFN) 
(PUT 'GET_DERIV_ORDERS 'NUMBER-OF-ARGS 2) 
(PUT 'GET_DERIV_ORDERS 'DEFINED-ON-LINE '525) 
(PUT 'GET_DERIV_ORDERS 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'GET_DERIV_ORDERS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_DERIV_ORDERS (ODE Y)
    (PROG (RESULT)
      (SETQ ODE (KERNELS (CAR (SIMP* ODE))))
      (COND ((NULL ODE) (RETURN (CONS 'LIST NIL))))
      (SETQ RESULT (GET_DERIV_ORDS_KNL (CAR ODE) Y))
      (PROG (KNL)
        (SETQ KNL (CDR ODE))
       LAB
        (COND ((NULL KNL) (RETURN NIL)))
        ((LAMBDA (KNL) (SETQ RESULT (UNION (GET_DERIV_ORDS_KNL KNL Y) RESULT)))
         (CAR KNL))
        (SETQ KNL (CDR KNL))
        (GO LAB))
      (RETURN (CONS 'LIST RESULT)))) 
(PUT 'GET_DERIV_ORDS_KNL 'NUMBER-OF-ARGS 2) 
(PUT 'GET_DERIV_ORDS_KNL 'DEFINED-ON-LINE '543) 
(PUT 'GET_DERIV_ORDS_KNL 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'GET_DERIV_ORDS_KNL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET_DERIV_ORDS_KNL (KNL Y)
    (COND ((ATOM KNL) (COND ((EQ KNL Y) (CONS 0 NIL))))
          ((EQ (CAR KNL) 'DF)
           (COND
            ((EQ (CADR KNL) Y)
             (CONS (COND ((CDDDR KNL) (CADDDR KNL)) (T 1)) NIL))))
          (T
           ((LAMBDA (IN_CAR IN_CDR)
              (COND (IN_CAR (UNION IN_CAR IN_CDR)) (T IN_CDR)))
            (GET_DERIV_ORDS_KNL (CAR KNL) Y)
            (GET_DERIV_ORDS_KNL (CDR KNL) Y))))) 
(AEVAL (OPERATOR (LIST 'ROOT_OF_UNITY 'PLUS_OR_MINUS))) 
(FLUID '(*INTFLAG*)) 
(AEVAL
 (LET
  '((REPLACEBY (EXPT (PLUS_OR_MINUS (~ TAG)) 2)
     (WHEN 1 (SYMBOLIC (NOT *INTFLAG*))))
    (REPLACEBY (EXPT (ROOT_OF_UNITY (~ N) (~ TAG)) N)
     (WHEN 1 (SYMBOLIC (NOT *INTFLAG*))))))) 
(PUT 'NEWROOT_OF_UNITY 'NUMBER-OF-ARGS 1) 
(FLAG '(NEWROOT_OF_UNITY) 'OPFN) 
(PUT 'NEWROOT_OF_UNITY 'DEFINED-ON-LINE '572) 
(PUT 'NEWROOT_OF_UNITY 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'NEWROOT_OF_UNITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEWROOT_OF_UNITY (N)
    (COND
     ((EVALEQUAL (AEVAL N) 0)
      (AEVAL (REDERR (REVALX "zeroth roots of unity undefined"))))
     ((AND (EVALNUMBERP (AEVAL N))
           (EVALEQUAL (SETQ N (AEVAL (LIST 'ABS (LIST 'NUM N)))) 1))
      1)
     ((EVALEQUAL (AEVAL N) 2)
      (AEVAL (LIST 'PLUS_OR_MINUS (LIST 'NEWROOT_OF_UNITY_TAG))))
     (T (AEVAL (LIST 'ROOT_OF_UNITY N (LIST 'NEWROOT_OF_UNITY_TAG)))))) 
(PUT 'NEWPLUS_OR_MINUS 'NUMBER-OF-ARGS 0) 
(FLAG '(NEWPLUS_OR_MINUS) 'OPFN) 
(PUT 'NEWPLUS_OR_MINUS 'DEFINED-ON-LINE '580) 
(PUT 'NEWPLUS_OR_MINUS 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'NEWPLUS_OR_MINUS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEWPLUS_OR_MINUS NIL (LIST 'PLUS_OR_MINUS (NEWROOT_OF_UNITY_TAG))) 
(PUT 'NEWROOT_OF_UNITY_TAG 'NUMBER-OF-ARGS 0) 
(FLAG '(NEWROOT_OF_UNITY_TAG) 'OPFN) 
(PUT 'NEWROOT_OF_UNITY_TAG 'DEFINED-ON-LINE '586) 
(PUT 'NEWROOT_OF_UNITY_TAG 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'NEWROOT_OF_UNITY_TAG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEWROOT_OF_UNITY_TAG NIL (MKROOTSOFTAG)) 
(DEFINE (LIST (LIST 'EQUAL 'EXPAND_PLUS_OR_MINUS 'EXPAND_ROOTS_OF_UNITY))) 
(DEFINE (LIST (LIST 'EQUAL 'EXPAND_ROOT_OF_UNITY 'EXPAND_ROOTS_OF_UNITY))) 
(FLAG '(EXPAND_ROOTS_OF_UNITY) 'OPFN) 
(FLAG '(EXPAND_ROOTS_OF_UNITY) 'NOVAL) 
(PUT 'EXPAND_ROOTS_OF_UNITY 'NUMBER-OF-ARGS 1) 
(PUT 'EXPAND_ROOTS_OF_UNITY 'DEFINED-ON-LINE '596) 
(PUT 'EXPAND_ROOTS_OF_UNITY 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'EXPAND_ROOTS_OF_UNITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPAND_ROOTS_OF_UNITY (U)
    (PROG (*NOINT)
      (SETQ *NOINT T)
      (SETQ U (REVAL1 U NIL))
      (RETURN
       (CONS 'LIST
             (UNION
              (PROG (UU FORALL-RESULT FORALL-ENDPTR)
                (SETQ UU (COND ((EQCAR U 'LIST) (CDR U)) (T (LIST U))))
               STARTOVER
                (COND ((NULL UU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (UU)
                           (CDR
                            (EXPAND_ROOTS_OF_UNITY1 (CONS 'LIST (LIST UU)))))
                         (CAR UU)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ UU (CDR UU))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL UU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (UU)
                           (CDR
                            (EXPAND_ROOTS_OF_UNITY1 (CONS 'LIST (LIST UU)))))
                         (CAR UU)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ UU (CDR UU))
                (GO LOOPLABEL))
              NIL))))) 
(PUT 'EXPAND_ROOTS_OF_UNITY1 'NUMBER-OF-ARGS 1) 
(PUT 'EXPAND_ROOTS_OF_UNITY1 'DEFINED-ON-LINE '603) 
(PUT 'EXPAND_ROOTS_OF_UNITY1 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'EXPAND_ROOTS_OF_UNITY1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPAND_ROOTS_OF_UNITY1 (U)
    ((LAMBDA (R)
       (COND
        (R
         (EXPAND_ROOTS_OF_UNITY1
          (CONS 'LIST
                (APPEND
                 (COND
                  ((EQ (CAR R) 'PLUS_OR_MINUS)
                   (CDR (SUBEVAL (LIST (LIST 'EQUAL R (MINUS 1)) U))))
                  (T
                   (PROG (N N-1)
                     (COND
                      ((NOT (FIXP (SETQ N (CAR (SIMP* (CADR R))))))
                       (TYPERR N "root of unity")))
                     (SETQ N-1 (SUB1 N))
                     (RETURN
                      (PROG (M FORALL-RESULT FORALL-ENDPTR)
                        (SETQ M 1)
                       STARTOVER
                        (COND ((MINUSP (DIFFERENCE N-1 M)) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (CDR
                                 (AEVAL*
                                  (LIST 'SUB
                                        (LIST 'EQUAL R
                                              (LIST 'EXP
                                                    (LIST 'TIMES 'I 2 'PI
                                                          (LIST 'QUOTIENT M
                                                                N))))
                                        U))))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ M (PLUS2 M 1))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND
                         ((MINUSP (DIFFERENCE N-1 M)) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CDR
                                 (AEVAL*
                                  (LIST 'SUB
                                        (LIST 'EQUAL R
                                              (LIST 'EXP
                                                    (LIST 'TIMES 'I 2 'PI
                                                          (LIST 'QUOTIENT M
                                                                N))))
                                        U))))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ M (PLUS2 M 1))
                        (GO LOOPLABEL))))))
                 (CDR (SUBEVAL (LIST (LIST 'EQUAL R 1) U)))))))
        (T U)))
     (FIND_ROOT_OF_UNITY (CDR U)))) 
(PUT 'FIND_ROOT_OF_UNITY 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_ROOT_OF_UNITY 'DEFINED-ON-LINE '617) 
(PUT 'FIND_ROOT_OF_UNITY 'DEFINED-IN-FILE 'ODESOLVE/ODETOP.RED) 
(PUT 'FIND_ROOT_OF_UNITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_ROOT_OF_UNITY (U)
    (COND ((ATOM U) NIL) ((EQ (CAR U) 'PLUS_OR_MINUS) U)
          ((AND (EQ (CAR U) 'ROOT_OF_UNITY) (EVALNUMBERP (CADR U))) U)
          (T (OR (FIND_ROOT_OF_UNITY (CAR U)) (FIND_ROOT_OF_UNITY (CDR U)))))) 
(ENDMODULE) 