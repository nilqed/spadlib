(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODENON1)) 
(GLOBAL '(ODESOLVE_BEFORE_NON1GRAD_HOOK ODESOLVE_AFTER_NON1GRAD_HOOK)) 
(PUT 'ODESOLVE-NONLINEAR1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-NONLINEAR1) 'OPFN) 
(PUT 'ODESOLVE-NONLINEAR1 'DEFINED-ON-LINE '66) 
(PUT 'ODESOLVE-NONLINEAR1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-NONLINEAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-NONLINEAR1 (ODE Y X)
    (PROG (ODECOEFFS GRADIENT SOLUTION P ODE_P)
      (AEVAL
       (TRACEODE1
        (LIST "Entering top-level non-linear first-order solver ...")))
      (SETQ P (AEVAL (LIST 'GENSYM)))
      (SETQ ODE_P
              (AEVAL
               (LIST 'NUM (LIST 'SUB (LIST 'EQUAL (LIST 'DF Y X) P) ODE))))
      (SETQ ODECOEFFS (AEVAL (LIST 'COEFF ODE_P P)))
      (COND
       ((AND (EVALEQUAL (AEVAL (LIST 'LENGTH ODECOEFFS)) 2)
             (NOT (BOOLVALUE* (REVALX (LIST 'SMEMBER P ODECOEFFS)))))
        (PROGN
         (SETQ GRADIENT
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'FIRST ODECOEFFS)
                              (LIST 'SECOND ODECOEFFS)))))
         (COND
          ((SETQ SOLUTION
                   (OR
                    (ODESOLVE-RUN-HOOK 'ODESOLVE_BEFORE_NON1GRAD_HOOK
                     (LIST GRADIENT Y X))
                    (ODESOLVE-SEPARABLE GRADIENT Y X)
                    (ODESOLVE-QUASISEPARABLE GRADIENT Y X)
                    (ODESOLVE-HOMOGENEOUS GRADIENT Y X)
                    (ODESOLVE-QUASIHOMOG GRADIENT Y X)
                    (ODESOLVE-BERNOULLI GRADIENT Y X)
                    (ODESOLVE-RICCATI GRADIENT Y X)
                    (ODESOLVE-RUN-HOOK 'ODESOLVE_AFTER_NON1GRAD_HOOK
                     (LIST GRADIENT Y X))))
           (RETURN SOLUTION))))))
      (COND
       ((BOOLVALUE*
         (SETQ SOLUTION (REVALX (LIST 'ODESOLVE-CLAIRAUT ODE ODE_P P Y X))))
        (RETURN (AEVAL SOLUTION))))
      (COND (*ODESOLVE-SOLVABLE-XY (RETURN NIL)))
      (SETQ *ODESOLVE-SOLVABLE-XY T)
      (RETURN
       (OR (ODESOLVE-SOLVABLE-Y ODE_P P Y X)
           (ODESOLVE-SOLVABLE-X ODE_P P Y X))))) 
(PUT 'ODENON-LINEAR1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODENON-LINEAR1) 'OPFN) 
(PUT 'ODENON-LINEAR1 'DEFINED-ON-LINE '116) 
(PUT 'ODENON-LINEAR1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODENON-LINEAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODENON-LINEAR1 (ODE Y X)
    (PROG (GRADIENT)
      (SETQ GRADIENT (AEVAL (LIST 'COEFF (LIST 'NUM ODE) (LIST 'DF Y X))))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'MINUS
                     (LIST 'QUOTIENT (LIST 'FIRST GRADIENT)
                           (LIST 'SECOND GRADIENT)))))
      (AEVAL (TRACEODE* (LIST "This is a first-order linear ODE solved by ")))
      (RETURN
       (COND
        ((BOOLVALUE* (REVALX (LIST 'SMEMBER Y GRADIENT)))
         (PROG (P Q)
           (AEVAL (TRACEODE (LIST "the integrating factor method.")))
           (SETQ P
                   (AEVAL
                    (LIST 'QUOTIENT (LIST 'LCOF (LIST 'NUM GRADIENT) Y)
                          (LIST 'DEN GRADIENT))))
           (SETQ Q (AEVAL (LIST 'DIFFERENCE GRADIENT (LIST 'TIMES P Y))))
           (RETURN
            (AEVAL
             (LIST 'LIST (LIST 'EQUAL Y (LIST 'ODENON-LINEAR1PQ P Q X)))))))
        (T
         (PROGN
          (AEVAL (TRACEODE (LIST "quadrature.")))
          (AEVAL
           (LIST 'LIST
                 (LIST 'EQUAL Y
                       (LIST 'PLUS (LIST 'ODESOLVE-INT GRADIENT X)
                             (LIST 'NEWARBCONST))))))))))) 
(PUT 'ODENON-LINEAR1PQ 'NUMBER-OF-ARGS 3) 
(FLAG '(ODENON-LINEAR1PQ) 'OPFN) 
(PUT 'ODENON-LINEAR1PQ 'DEFINED-ON-LINE '137) 
(PUT 'ODENON-LINEAR1PQ 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODENON-LINEAR1PQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODENON-LINEAR1PQ (P Q X)
    (PROG (INTFACTOR *COMBINELOGS)
      (SETQ *COMBINELOGS T)
      (SETQ INTFACTOR (AEVAL (LIST 'EXP (LIST 'INT (LIST 'MINUS P) X))))
      (RETURN
       (AEVAL
        (LIST 'QUOTIENT
              (LIST 'PLUS (LIST 'NEWARBCONST)
                    (LIST 'ODESOLVE-INT (LIST 'TIMES INTFACTOR Q) X))
              INTFACTOR))))) 
(PUT 'ODESOLVE-SEPARABLE 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SEPARABLE) 'OPFN) 
(PUT 'ODESOLVE-SEPARABLE 'DEFINED-ON-LINE '159) 
(PUT 'ODESOLVE-SEPARABLE 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SEPARABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SEPARABLE (GRADIENT Y X)
    (PROG (F G *REDEFMSG)
      (AEVAL (TRACEODE1 (LIST "Testing for a separable ODE ...")))
      ((LAMBDA (DEPL*)
         (PROGN
          (DEPEND1 Y X NIL)
          (COPYD 'ODESOLVE-OLD-SUBSUBLIS 'SUBSUBLIS)
          (COPYD 'SUBSUBLIS 'ODESOLVE-SUBSUBLIS)
          (SETQ G
                  (ERRORSET*
                   (LIST 'ODESOLVE-SEPARABLE1 (MKQUOTE GRADIENT) (MKQUOTE X))
                   NIL))
          (COPYD 'SUBSUBLIS 'ODESOLVE-OLD-SUBSUBLIS)
          (COND ((ERRORP G) (REDERR (LIST "(in ODESolve!-Separable1)" EMSG*))))
          (SETQ G (CAR G))
          (COND ((DEPENDS G X) (SETQ G NIL)))
          NIL))
       DEPL*)
      (COND ((NOT (BOOLVALUE* G)) (RETURN (AEVAL 'NIL))))
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'DEPENDS (SETQ F (REVALX (LIST 'QUOTIENT GRADIENT G))) Y)))
        (RETURN (AEVAL 'NIL))))
      (AEVAL (TRACEODE (LIST "It is separable.")))
      (SETQ GRADIENT (AEVAL (LIST 'INT (LIST 'QUOTIENT 1 G) Y)))
      (SETQ GRADIENT
              (COND
               ((EVALEQUAL (AEVAL (LIST 'PART GRADIENT 0)) (AEVAL 'LOG))
                (AEVAL
                 (LIST 'DIFFERENCE (LIST 'PART GRADIENT 1)
                       (LIST 'TIMES (LIST 'NEWARBCONST)
                             (LIST 'EXP (LIST 'INT F X))))))
               (T
                (AEVAL
                 (LIST 'PLUS (LIST 'DIFFERENCE GRADIENT (LIST 'INT F X))
                       (LIST 'NEWARBCONST))))))
      (RETURN (AEVAL (LIST 'LIST (LIST 'EQUAL (LIST 'NUM GRADIENT) 0)))))) 
(PUT 'ODESOLVE-SEPARABLE1 'NUMBER-OF-ARGS 2) 
(FLAG '(ODESOLVE-SEPARABLE1) 'OPFN) 
(PUT 'ODESOLVE-SEPARABLE1 'DEFINED-ON-LINE '195) 
(PUT 'ODESOLVE-SEPARABLE1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SEPARABLE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SEPARABLE1 (GRADIENT X)
    (PROG (NUMER DENOM ALPHA D N)
      (SETQ NUMER (AEVAL (LIST 'NUM GRADIENT)))
      (SETQ DENOM (AEVAL (LIST 'DEN GRADIENT)))
      (SETQ ALPHA (AEVAL 0))
      (WHILE
       (OR
        (EVALEQUAL (SETQ D (AEVAL* (LIST 'SUB (LIST 'EQUAL X ALPHA) DENOM))) 0)
        (EVALEQUAL (SETQ N (AEVAL* (LIST 'SUB (LIST 'EQUAL X ALPHA) NUMER)))
                   0))
       (SETQ ALPHA (AEVAL* (LIST 'PLUS ALPHA 1))))
      (RETURN (AEVAL (LIST 'QUOTIENT N D))))) 
(PUT 'ODESOLVE-SUBSUBLIS 'NUMBER-OF-ARGS 2) 
(PUT 'ODESOLVE-SUBSUBLIS 'DEFINED-ON-LINE '208) 
(PUT 'ODESOLVE-SUBSUBLIS 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SUBSUBLIS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SUBSUBLIS (U V)
    (PROG (X)
      (RETURN
       (COND ((SETQ X (ASSOC V U)) (CDR X))
             ((AND (EQCAR V 'SQRT)
                   (SETQ X (ASSOC (LIST 'EXPT (CADR V) '(QUOTIENT 1 2)) U)))
              (CDR X))
             ((ATOM V)
              (COND
               ((AND (SETQ X (ASSOC V DEPL*))
                     (PROGN
                      (PROG ()
                       WHILELABEL
                        (COND
                         ((NOT (AND (SETQ X (CDR X)) (NOT (ASSOC (CAR X) U))))
                          (RETURN NIL)))
                       NIL
                        (GO WHILELABEL))
                      X))
                (MKID V '!))
               (T V)))
             ((NOT (IDP (CAR V)))
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J V)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (ODESOLVE-SUBSUBLIS U J))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (ODESOLVE-SUBSUBLIS U J)) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
             ((SETQ X (GET (CAR V) 'SUBFUNC)) (APPLY2 X U V))
             ((GET (CAR V) 'DNAME) V)
             ((EQ (CAR V) '*SQ) (ODESOLVE-SUBSUBLIS U (PREPSQ (CADR V))))
             (T
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J V)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (ODESOLVE-SUBSUBLIS U J))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (ODESOLVE-SUBSUBLIS U J)) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'ODESOLVE-QUASISEPARABLE 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-QUASISEPARABLE) 'OPFN) 
(PUT 'ODESOLVE-QUASISEPARABLE 'DEFINED-ON-LINE '239) 
(PUT 'ODESOLVE-QUASISEPARABLE 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-QUASISEPARABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-QUASISEPARABLE (GRADIENT Y X)
    (PROG (K)
      (AEVAL (TRACEODE1 (LIST "Testing for a quasi-separable ODE ...")))
      (SETQ K
              (AEVAL
               (LIST 'WHEREEXP (LIST 'LIST (LIST 'REPLACEBY (LIST 'DF Y X) 0))
                     (LIST 'QUOTIENT (LIST 'DF GRADIENT X)
                           (LIST 'DF GRADIENT Y)))))
      (COND ((BOOLVALUE* (REVALX (LIST 'DEPENDS K X))) (RETURN (AEVAL 'NIL))))
      (AEVAL
       (TRACEODE
        (LIST "It is separable after letting "
              (LIST 'REPLACEBY (LIST 'PLUS Y (LIST 'TIMES K X)) Y))))
      (SETQ GRADIENT
              (AEVAL (LIST 'PLUS (LIST 'SUB (LIST 'EQUAL X 0) GRADIENT) K)))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'PLUS
                     (LIST 'DIFFERENCE
                           (LIST 'SUB
                                 (LIST 'EQUAL Y
                                       (LIST 'PLUS Y (LIST 'TIMES K X)))
                                 (LIST 'INT (LIST 'QUOTIENT 1 GRADIENT) Y))
                           X)
                     (LIST 'NEWARBCONST))))
      (RETURN (AEVAL (LIST 'LIST (LIST 'EQUAL (LIST 'NUM GRADIENT) 0)))))) 
(PUT 'ODESOLVE-HOMOGENEOUS 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-HOMOGENEOUS) 'OPFN) 
(PUT 'ODESOLVE-HOMOGENEOUS 'DEFINED-ON-LINE '261) 
(PUT 'ODESOLVE-HOMOGENEOUS 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-HOMOGENEOUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-HOMOGENEOUS (GRADIENT Y X)
    (PROG (V)
      (SETQ V (AEVAL (LIST 'GENSYM)))
      (AEVAL (TRACEODE1 (LIST "Testing for a homogeneous ODE ...")))
      (SETQ GRADIENT
              (AEVAL (LIST 'SUB (LIST 'EQUAL Y (LIST 'TIMES V X)) GRADIENT)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'DEPENDS GRADIENT X)))
        (RETURN (AEVAL 'NIL))))
      (AEVAL
       (TRACEODE
        (LIST "It is of algebraically homogeneous type "
              "solved by a change of variables of the form `y = vx'.")))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'EXP
                     (LIST 'INT
                           (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE GRADIENT V))
                           V))))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY
                                 (LIST 'TIMES (LIST 'SQRT (LIST '~ 'A))
                                       (LIST 'SQRT (LIST '~ 'B)))
                                 (LIST 'SQRT (LIST 'TIMES 'A 'B))))
                     GRADIENT)))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'PLUS
                     (LIST 'SUB (LIST 'EQUAL V (LIST 'QUOTIENT Y X))
                           (LIST 'QUOTIENT X GRADIENT))
                     (LIST 'NEWARBCONST))))
      (RETURN (AEVAL (LIST 'LIST (LIST 'EQUAL (LIST 'NUM GRADIENT) 0)))))) 
(PUT 'ODESOLVE-QUASIHOMOG 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-QUASIHOMOG) 'OPFN) 
(PUT 'ODESOLVE-QUASIHOMOG 'DEFINED-ON-LINE '294) 
(PUT 'ODESOLVE-QUASIHOMOG 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-QUASIHOMOG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-QUASIHOMOG (GRADIENT Y X)
    (PROG (TMP N D SOLN)
      (AEVAL (TRACEODE1 (LIST "Testing for a quasi-homogeneous ODE ...")))
      (COND
       ((NOT
         (BOOLVALUE*
          (SETQ TMP (REVALX (ODESOLVE-QUASIHOMOG1 (REVAL1 GRADIENT T) X)))))
        (RETURN (AEVAL 'NIL))))
      (SETQ N (AEVAL (LIST 'NUM TMP)))
      (SETQ D (AEVAL (LIST 'DEN TMP)))
      (COND
       ((EVALNEQ (SETQ TMP (AEVAL (LIST 'DEG N X))) (AEVAL (LIST 'DEG D X)))
        (RETURN (AEVAL 'NIL))))
      (COND ((EVALEQUAL (AEVAL TMP) 1) (AEVAL 'NIL))
            (T
             (PROGN
              (SETQ N (AEVAL (LIST 'QUOTIENT N (LIST 'GCD N (LIST 'DF N X)))))
              (SETQ D (AEVAL (LIST 'QUOTIENT D (LIST 'GCD D (LIST 'DF D X)))))
              (AEVAL 'NIL))))
      (COND
       ((OR
         (EVALNEQ (AEVAL (LIST 'LENGTH (SETQ TMP (AEVAL (LIST 'COEFF N Y)))))
                  2)
         (BOOLVALUE* (REVALX (LIST 'DEPENDS TMP Y))))
        (RETURN (AEVAL 'NIL))))
      (COND
       ((OR (BOOLVALUE* (REVALX (LIST 'DEPENDS (LIST 'SECOND TMP) X)))
            (EVALNEQ
             (AEVAL
              (LIST 'LENGTH
                    (SETQ TMP (AEVAL (LIST 'COEFF (LIST 'FIRST TMP) X)))))
             2)
            (BOOLVALUE* (REVALX (LIST 'DEPENDS TMP X))))
        (RETURN (AEVAL 'NIL))))
      (COND
       ((OR
         (EVALNEQ (AEVAL (LIST 'LENGTH (SETQ TMP (AEVAL (LIST 'COEFF D Y)))))
                  2)
         (BOOLVALUE* (REVALX (LIST 'DEPENDS TMP Y))))
        (RETURN (AEVAL 'NIL))))
      (COND
       ((OR (BOOLVALUE* (REVALX (LIST 'DEPENDS (LIST 'SECOND TMP) X)))
            (EVALNEQ
             (AEVAL
              (LIST 'LENGTH
                    (SETQ TMP (AEVAL (LIST 'COEFF (LIST 'FIRST TMP) X)))))
             2)
            (BOOLVALUE* (REVALX (LIST 'DEPENDS TMP X))))
        (RETURN (AEVAL 'NIL))))
      (AEVAL
       (TRACEODE
        (LIST "It is quasi-homogeneous if "
              "the result of shifting the origin is homogeneous ...")))
      (SETQ SOLN
              (AEVAL
               (LIST 'FIRST (LIST 'SOLVE (LIST 'LIST N D) (LIST 'LIST X Y)))))
      (SETQ N (AEVAL (LIST 'RHS (LIST 'FIRST SOLN))))
      (SETQ D (AEVAL (LIST 'RHS (LIST 'SECOND SOLN))))
      (SETQ GRADIENT
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS X N))
                     (LIST 'EQUAL Y (LIST 'PLUS Y D)) GRADIENT)))
      (COND
       ((BOOLVALUE*
         (SETQ SOLN (REVALX (LIST 'ODESOLVE-HOMOGENEOUS GRADIENT Y X))))
        (RETURN
         (AEVAL
          (LIST 'SUB (LIST 'EQUAL X (LIST 'DIFFERENCE X N))
                (LIST 'EQUAL Y (LIST 'DIFFERENCE Y D)) SOLN)))))
      (AEVAL (TRACEODE (LIST "... which it is not!"))))) 
(PUT 'ODESOLVE-QUASIHOMOG1 'NUMBER-OF-ARGS 2) 
(PUT 'ODESOLVE-QUASIHOMOG1 'DEFINED-ON-LINE '349) 
(PUT 'ODESOLVE-QUASIHOMOG1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-QUASIHOMOG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-QUASIHOMOG1 (U X)
    (COND ((ATOM U) NIL)
          ((AND (EQ (CAR U) 'QUOTIENT) (DEPENDS (CADR U) X)
                (DEPENDS (CADDR U) X))
           (COND
            ((EQCAR (CADR U) 'EXPT)
             (COND
              ((AND (EQCAR (CADDR U) 'EXPT)
                    (EQ (CADDR (CADR U)) (CADDR (CADDR U))))
               (LIST 'QUOTIENT (CADR (CADR U)) (CADR (CADDR U))))))
            (T
             (COND ((AND (EQCAR (CADR U) 'PLUS) (EQCAR (CADDR U) 'PLUS)) U)))))
          (T
           (PROG ()
            A
             (COND
              ((SETQ U (CDR U))
               (COND
                ((DEPENDS (CAR U) X) (RETURN (ODESOLVE-QUASIHOMOG1 (CAR U) X)))
                (T (GO A))))))))) 
(FLAG '(ODESOLVE-BERNOULLI) 'OPFN) 
(PUT 'ODESOLVE-BERNOULLI 'NUMBER-OF-ARGS 3) 
(PUT 'ODESOLVE-BERNOULLI 'DEFINED-ON-LINE '377) 
(PUT 'ODESOLVE-BERNOULLI 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-BERNOULLI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-BERNOULLI (RHS Y X)
    ((LAMBDA (DEPL*)
       (PROG (NUM_RHS DEN_RHS C1 C2 D D1 D2 D3 P Q N)
         (TRACEODE1 (LIST "Testing for a Bernoulli ODE ..."))
         ((LAMBDA (*MSG) (DEPEND1 X Y NIL)) NIL)
         ((LAMBDA (KORD*) (PROGN (UPDKORDER Y) (SETQ RHS (SIMP RHS)))) KORD*)
         (SETQ NUM_RHS (CAR RHS))
         (SETQ DEN_RHS (CDR RHS))
         (COND
          ((OR (OR (ATOM NUM_RHS) (ATOM (CAR NUM_RHS)))
               (NOT (EQ (CAAAR NUM_RHS) Y)))
           (RETURN NIL)))
         (COND
          ((NULL (CDR NUM_RHS))
           (PROGN
            (SETQ D (CDAAR NUM_RHS))
            (SETQ NUM_RHS (CDAR NUM_RHS))
            (COND ((OR (ATOM NUM_RHS) (ATOM (CAR NUM_RHS))) (RETURN NIL))))))
         (COND
          ((SETQ C1 (GET!Y^N*C NUM_RHS Y))
           (PROGN (SETQ D1 (CAR C1)) (SETQ C1 (CDR C1))))
          (T (RETURN NIL)))
         (SETQ NUM_RHS (CDR NUM_RHS))
         (COND
          ((NOT (SMEMBER Y NUM_RHS)) (PROGN (SETQ D2 0) (SETQ C2 NUM_RHS)))
          ((CDR NUM_RHS) (RETURN NIL))
          ((SETQ C2 (GET!Y^N*C NUM_RHS Y))
           (PROGN (SETQ D2 (CAR C2)) (SETQ C2 (CDR C2))))
          (T (RETURN NIL)))
         (COND
          ((SMEMBER Y DEN_RHS)
           (COND
            ((AND (NULL (CDR DEN_RHS)) (SETQ DEN_RHS (GET!Y^N*C DEN_RHS Y)))
             (PROGN
              (SETQ D3 (CAR DEN_RHS))
              (SETQ DEN_RHS (CDR DEN_RHS))
              (SETQ D1 (LIST 'DIFFERENCE D1 D3))
              (SETQ D2 (LIST 'DIFFERENCE D2 D3))))
            (T (RETURN NIL)))))
         (COND
          (D (PROGN (SETQ D1 (LIST 'PLUS D1 D)) (SETQ D2 (LIST 'PLUS D2 D)))))
         (SETQ D1 (REVAL1 D1 NIL))
         (SETQ D2 (REVAL1 D2 NIL))
         (COND ((EQUAL D1 1) (PROGN (SETQ P C1) (SETQ Q C2) (SETQ N D2)))
               ((EQUAL D2 1) (PROGN (SETQ P C2) (SETQ Q C1) (SETQ N D1)))
               (T (RETURN NIL)))
         (COND
          ((OR (BERNOULLI-DEPEND-CHECK P Y) (BERNOULLI-DEPEND-CHECK Q Y)
               (BERNOULLI-DEPEND-CHECK DEN_RHS Y) (BERNOULLI-DEPEND-CHECK N X))
           (RETURN NIL)))
         (SETQ P (MK*SQ (CONS P DEN_RHS)))
         (SETQ Q (MK*SQ (CONS Q DEN_RHS)))
         (RETURN (ODESOLVE-BERNOULLI1 P Q Y X N))))
     DEPL*)) 
(PUT 'BERNOULLI-DEPEND-CHECK 'NUMBER-OF-ARGS 2) 
(PUT 'BERNOULLI-DEPEND-CHECK 'DEFINED-ON-LINE '438) 
(PUT 'BERNOULLI-DEPEND-CHECK 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'BERNOULLI-DEPEND-CHECK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BERNOULLI-DEPEND-CHECK (F XY)
    (COND
     ((CAR (DIFFF F XY))
      (PROGN
       (TRACEODE (LIST "It is not of Bernoulli type because ..."))
       (MSGPRI NIL (PREPF F) "depends (possibly implicitly) on"
               (OR (GET XY 'ODESOLVE-DEPVAR) XY) NIL)
       T)))) 
(PUT 'GET!Y^N*C 'NUMBER-OF-ARGS 2) 
(PUT 'GET!Y^N*C 'DEFINED-ON-LINE '448) 
(PUT 'GET!Y^N*C 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'GET!Y^N*C 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET!Y^N*C (U Y)
    (PROG (N C)
      (COND
       ((EQ (CAAAR U) Y)
        (PROGN
         (SETQ N (CDAAR U))
         (SETQ C (CDAR U))
         (RETURN
          (COND
           ((AND (NOT (OR (ATOM C) (ATOM (CAR C)))) (SMEMBER Y (CAAAR C)))
            (COND
             ((SETQ C (GET!Y^N*C1 C Y))
              (CONS (LIST 'PLUS N (CAR C)) (CDR C)))))
           (T (CONS N C))))))
       (T (RETURN (GET!Y^N*C1 U Y)))))) 
(PUT 'GET!Y^N*C1 'NUMBER-OF-ARGS 2) 
(PUT 'GET!Y^N*C1 'DEFINED-ON-LINE '463) 
(PUT 'GET!Y^N*C1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'GET!Y^N*C1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET!Y^N*C1 (U Y)
    (PROG (N C)
      (SETQ N (CAAAR U))
      (COND ((NOT (AND (EQCAR N 'EXPT) (EQ (CADR N) Y))) (RETURN NIL)))
      (SETQ N (LIST 'TIMES (CADDR N) (CDAAR U)))
      (SETQ C (CDAR U))
      (RETURN (CONS N C)))) 
(PUT 'ODESOLVE-BERNOULLI1 'NUMBER-OF-ARGS 5) 
(FLAG '(ODESOLVE-BERNOULLI1) 'OPFN) 
(PUT 'ODESOLVE-BERNOULLI1 'DEFINED-ON-LINE '473) 
(PUT 'ODESOLVE-BERNOULLI1 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-BERNOULLI1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-BERNOULLI1 (P Q Y X N)
    (PROG (*ODESOLVE_NOINT)
      (AEVAL (TRACEODE (LIST "It is of Bernoulli type.")))
      (SETQ N (AEVAL (LIST 'DIFFERENCE 1 N)))
      (RETURN
       (COND
        ((BOOLVALUE* (REVALX *ODESOLVE_EXPLICIT))
         (AEVAL
          (LIST 'LIST
                (LIST 'EQUAL Y
                      (LIST 'TIMES
                            (LIST 'EXPT
                                  (LIST 'ODENON-LINEAR1PQ (LIST 'TIMES N P)
                                        (LIST 'TIMES N Q) X)
                                  (LIST 'QUOTIENT 1 N))
                            (LIST 'NEWROOT_OF_UNITY N))))))
        (T
         (AEVAL
          (LIST 'LIST
                (LIST 'EQUAL (LIST 'EXPT Y N)
                      (LIST 'ODENON-LINEAR1PQ (LIST 'TIMES N P)
                            (LIST 'TIMES N Q) X))))))))) 
(PUT 'ODESOLVE-RICCATI 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-RICCATI) 'OPFN) 
(PUT 'ODESOLVE-RICCATI 'DEFINED-ON-LINE '487) 
(PUT 'ODESOLVE-RICCATI 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-RICCATI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-RICCATI (RHS Y X)
    (COND
     ((NOT *ODESOLVE_FAST)
      (PROG (A B C SOLN *RATARG)
        (AEVAL (TRACEODE1 (LIST "Testing for a Riccati ODE ...")))
        (SETQ *RATARG T)
        (SETQ C (AEVAL (LIST 'COEFF RHS Y)))
        (COND
         ((OR (EVALNEQ (AEVAL (LIST 'LENGTH C)) 3)
              (BOOLVALUE* (REVALX (LIST 'DEPENDS C Y))))
          (RETURN (AEVAL 'NIL))))
        (SETQ A (AEVAL (LIST 'THIRD C)))
        (SETQ B (AEVAL (LIST 'SECOND C)))
        (SETQ C (AEVAL (LIST 'FIRST C)))
        (SETQ C (AEVAL (LIST 'TIMES A C)))
        (SETQ B
                (AEVAL
                 (LIST 'MINUS
                       (LIST 'PLUS (LIST 'QUOTIENT (LIST 'DF A X) A) B))))
        (AEVAL
         (TRACEODE
          (LIST "It is of Riccati type "
                "and transforms into the linear second-order ODE: "
                (LIST 'EQUAL
                      (LIST 'NUM
                            (LIST 'PLUS (LIST 'DF Y X 2)
                                  (LIST 'TIMES B (LIST 'DF Y X))
                                  (LIST 'TIMES C Y)))
                      0))))
        (SETQ SOLN (AEVAL (LIST 'LIST C B 1)))
        (SETQ SOLN
                (AEVAL
                 (LIST 'ODESOLVE-LINEAR-BASIS SOLN 0 Y X 2
                       (COND ((BOOLVALUE* C) 0) ((BOOLVALUE* B) 1) (T 2)))))
        (COND
         ((NOT (BOOLVALUE* SOLN))
          (PROGN
           (AEVAL (TRACEODE (LIST "But ODESolve cannot solve it!")))
           (RETURN (AEVAL 'NIL)))))
        (SETQ SOLN (AEVAL (LIST 'FIRST SOLN)))
        (SETQ SOLN
                (AEVAL
                 (LIST 'PLUS
                       (LIST 'TIMES (LIST 'NEWARBCONST) (LIST 'FIRST SOLN))
                       (LIST 'SECOND SOLN))))
        (RETURN
         (AEVAL
          (LIST 'LIST
                (LIST 'EQUAL Y
                      (LIST 'SUB (LIST 'EQUAL Y SOLN)
                            (LIST 'MINUS
                                  (LIST 'QUOTIENT (LIST 'DF Y X)
                                        (LIST 'TIMES A Y)))))))))))) 
(PUT 'ODESOLVE-CLAIRAUT 'NUMBER-OF-ARGS 5) 
(FLAG '(ODESOLVE-CLAIRAUT) 'OPFN) 
(PUT 'ODESOLVE-CLAIRAUT 'DEFINED-ON-LINE '525) 
(PUT 'ODESOLVE-CLAIRAUT 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-CLAIRAUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-CLAIRAUT (ODE ODE_P P Y X)
    (PROG (SING_SOLN)
      (AEVAL (TRACEODE1 (LIST "Testing for a Clairaut ODE ...")))
      (SETQ SING_SOLN
              (AEVAL (LIST 'SUB (LIST 'EQUAL Y (LIST 'TIMES X P)) ODE_P)))
      (COND
       ((OR (BOOLVALUE* (REVALX (LIST 'DEPENDS SING_SOLN X)))
            (BOOLVALUE* (REVALX (LIST 'DEPENDS SING_SOLN Y))))
        (RETURN (AEVAL 'NIL))))
      (AEVAL (TRACEODE (LIST "It is of Clairaut type.")))
      (SETQ SING_SOLN (AEVAL (LIST 'SOLVE (LIST 'DF ODE X) (LIST 'DF Y X))))
      (SETQ SING_SOLN
              (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
                (SETQ SOLN (GETRLIST (AEVAL SING_SOLN)))
               STARTOVER
                (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        ((LAMBDA (SOLN)
                           (COND
                            ((BOOLVALUE*
                              (REVALX (EQCAR (CADDR SOLN) 'ROOT_OF)))
                             (AEVAL (LIST 'LIST)))
                            (T
                             (AEVAL
                              (LIST 'LIST
                                    (LIST 'EQUAL
                                          (LIST 'NUM (LIST 'SUB SOLN ODE))
                                          0))))))
                         (CAR SOLN)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ SOLN (CDR SOLN))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         ((LAMBDA (SOLN)
                            (COND
                             ((BOOLVALUE*
                               (REVALX (EQCAR (CADDR SOLN) 'ROOT_OF)))
                              (AEVAL (LIST 'LIST)))
                             (T
                              (AEVAL
                               (LIST 'LIST
                                     (LIST 'EQUAL
                                           (LIST 'NUM (LIST 'SUB SOLN ODE))
                                           0))))))
                          (CAR SOLN))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ SOLN (CDR SOLN))
                (GO LOOPLABEL)))
      (RETURN
       (AEVAL
        (LIST 'CONS
              (LIST 'EQUAL
                    (LIST 'SUB (LIST 'EQUAL P (LIST 'NEWARBCONST)) ODE_P) 0)
              SING_SOLN))))) 
(FLAG '(ODESOLVE-SOLVABLE-Y) 'OPFN) 
(PUT 'ODESOLVE-SOLVABLE-Y 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-SOLVABLE-Y 'DEFINED-ON-LINE '546) 
(PUT 'ODESOLVE-SOLVABLE-Y 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SOLVABLE-Y 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SOLVABLE-Y (ODE_P P Y X)
    ((LAMBDA (DEPL*)
       (PROG (C LAGRANGE ODE_Y ODE_X)
         (AEVAL
          (TRACEODE1
           (LIST "Testing for a \"solvable for y\" or Lagrange ODE ...")))
         (SETQ C (AEVAL (LIST 'COEFF ODE_P Y)))
         (COND
          ((OR (EVALNEQ (AEVAL (LIST 'LENGTH C)) 2)
               (BOOLVALUE* (REVALX (LIST 'DEPENDS C Y))))
           (RETURN (AEVAL 'NIL))))
         (SETQ ODE_Y
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'FIRST C) (LIST 'SECOND C)))))
         (COND
          ((AND (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDS (LIST 'DEN ODE_Y) X))))
                (EVALEQUAL
                 (AEVAL
                  (LIST 'LENGTH
                        (SETQ C (AEVAL (LIST 'COEFF (LIST 'NUM ODE_Y) X)))))
                 2)
                (NOT (BOOLVALUE* (REVALX (LIST 'DEPENDS C X)))))
           (SETQ LAGRANGE (AEVAL 1))))
         (DEPEND1 P X T)
         (SETQ ODE_X
                 (AEVAL (LIST 'NUM (LIST 'DIFFERENCE P (LIST 'DF ODE_Y X)))))
         (COND
          ((BOOLVALUE* LAGRANGE)
           (PROGN
            (AEVAL (DEPEND1 X P T))
            (SETQ ODE_X
                    (AEVAL
                     (LIST 'NUM
                           (LIST 'WHEREEXP
                                 (LIST 'LIST
                                       (LIST 'REPLACEBY (LIST 'DF P X)
                                             (LIST 'QUOTIENT 1
                                                   (LIST 'DF X P))))
                                 ODE_X))))
            (AEVAL (DEPEND1 P X NIL))
            (AEVAL
             (TRACEODE
              (LIST "It is of Lagrange type and reduces to this "
                    "subsidiary ODE for x(y'): " (LIST 'EQUAL ODE_X 0))))
            (SETQ ODE_X (AEVAL (LIST 'ODENON-LINEAR1 ODE_X X P)))))
          ((BOOLVALUE* (REVALX *ODESOLVE_FAST))
           (RETURN
            (AEVAL
             (TRACEODE
              (LIST "Sub-solver terminated: fast mode, no heuristics!")))))
          (T
           (PROGN
            (AEVAL
             (TRACEODE
              (LIST "It is \"solvable for y\" and reduces "
                    "to this subsidiary ODE for y'(x):")))
            (SETQ ODE_X (AEVAL (LIST 'ODESOLVE-FIRSTORDER ODE_X P X)))
            (COND
             ((NOT (BOOLVALUE* ODE_X))
              (PROGN
               (AEVAL (TRACEODE (LIST "But ODESolve cannot solve it!")))
               (RETURN (AEVAL 'NIL))))))))
         (AEVAL
          (TRACEODE
           (LIST "The subsidiary solution is " ODE_X
                 " and the main ODE can be solved parametrically "
                 "in terms of the derivative.")))
         (RETURN
          (COND
           ((BOOLVALUE* (REVALX (OR *ODESOLVE_IMPLICIT *ODESOLVE_EXPLICIT)))
            (AEVAL (LIST 'ODESOLVE-ELIM-PARAM ODE_Y Y ODE_X P Y)))
           ((BOOLVALUE* LAGRANGE)
            (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
              (SETQ SOLN (GETRLIST (AEVAL ODE_X)))
              (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (SOLN)
                                  (AEVAL
                                   (LIST 'ODESOLVE-SIMP-ARBPARAM
                                         (LIST 'SUB
                                               (LIST 'EQUAL P
                                                     (LIST 'NEWARBPARAM))
                                               (LIST 'LIST
                                                     (LIST 'EQUAL Y
                                                           (LIST 'SUB SOLN
                                                                 ODE_Y))
                                                     SOLN P)))))
                                (CAR SOLN))
                               NIL)))
             LOOPLABEL
              (SETQ SOLN (CDR SOLN))
              (COND ((NULL SOLN) (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (SOLN)
                          (AEVAL
                           (LIST 'ODESOLVE-SIMP-ARBPARAM
                                 (LIST 'SUB (LIST 'EQUAL P (LIST 'NEWARBPARAM))
                                       (LIST 'LIST
                                             (LIST 'EQUAL Y
                                                   (LIST 'SUB SOLN ODE_Y))
                                             SOLN P)))))
                        (CAR SOLN))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           (T
            (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
              (SETQ SOLN (GETRLIST (AEVAL ODE_X)))
             STARTOVER
              (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      ((LAMBDA (SOLN)
                         (PROGN
                          (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN X)))
                          (PROG (S FORALL-RESULT FORALL-ENDPTR)
                            (SETQ S (GETRLIST (AEVAL SOLN)))
                            (COND ((NULL S) (RETURN (MAKELIST NIL))))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (S)
                                                (AEVAL
                                                 (LIST 'ODESOLVE-SIMP-ARBPARAM
                                                       (LIST 'SUB
                                                             (LIST 'EQUAL P
                                                                   (LIST
                                                                    'NEWARBPARAM))
                                                             (COND
                                                              ((BOOLVALUE*
                                                                (REVALX
                                                                 (EQCAR
                                                                  (CADDR S)
                                                                  'ROOT_OF)))
                                                               (AEVAL
                                                                (LIST 'LIST
                                                                      (LIST
                                                                       'EQUAL Y
                                                                       ODE_Y)
                                                                      (LIST
                                                                       'SUB
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'PART
                                                                         (LIST
                                                                          'RHS
                                                                          S)
                                                                         2)
                                                                        X)
                                                                       (LIST
                                                                        'PART
                                                                        (LIST
                                                                         'RHS
                                                                         S)
                                                                        1))
                                                                      P)))
                                                              (T
                                                               (AEVAL
                                                                (LIST 'LIST
                                                                      (LIST
                                                                       'EQUAL Y
                                                                       (LIST
                                                                        'SUB S
                                                                        ODE_Y))
                                                                      S
                                                                      P))))))))
                                              (CAR S))
                                             NIL)))
                           LOOPLABEL
                            (SETQ S (CDR S))
                            (COND
                             ((NULL S) (RETURN (CONS 'LIST FORALL-RESULT))))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (S)
                                        (AEVAL
                                         (LIST 'ODESOLVE-SIMP-ARBPARAM
                                               (LIST 'SUB
                                                     (LIST 'EQUAL P
                                                           (LIST 'NEWARBPARAM))
                                                     (COND
                                                      ((BOOLVALUE*
                                                        (REVALX
                                                         (EQCAR (CADDR S)
                                                                'ROOT_OF)))
                                                       (AEVAL
                                                        (LIST 'LIST
                                                              (LIST 'EQUAL Y
                                                                    ODE_Y)
                                                              (LIST 'SUB
                                                                    (LIST
                                                                     'EQUAL
                                                                     (LIST
                                                                      'PART
                                                                      (LIST
                                                                       'RHS S)
                                                                      2)
                                                                     X)
                                                                    (LIST 'PART
                                                                          (LIST
                                                                           'RHS
                                                                           S)
                                                                          1))
                                                              P)))
                                                      (T
                                                       (AEVAL
                                                        (LIST 'LIST
                                                              (LIST 'EQUAL Y
                                                                    (LIST 'SUB
                                                                          S
                                                                          ODE_Y))
                                                              S P))))))))
                                      (CAR S))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
                       (CAR SOLN)))
              (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
              (SETQ SOLN (CDR SOLN))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (GETRLIST
                       ((LAMBDA (SOLN)
                          (PROGN
                           (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN X)))
                           (PROG (S FORALL-RESULT FORALL-ENDPTR)
                             (SETQ S (GETRLIST (AEVAL SOLN)))
                             (COND ((NULL S) (RETURN (MAKELIST NIL))))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (S)
                                                 (AEVAL
                                                  (LIST 'ODESOLVE-SIMP-ARBPARAM
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL P
                                                                    (LIST
                                                                     'NEWARBPARAM))
                                                              (COND
                                                               ((BOOLVALUE*
                                                                 (REVALX
                                                                  (EQCAR
                                                                   (CADDR S)
                                                                   'ROOT_OF)))
                                                                (AEVAL
                                                                 (LIST 'LIST
                                                                       (LIST
                                                                        'EQUAL
                                                                        Y
                                                                        ODE_Y)
                                                                       (LIST
                                                                        'SUB
                                                                        (LIST
                                                                         'EQUAL
                                                                         (LIST
                                                                          'PART
                                                                          (LIST
                                                                           'RHS
                                                                           S)
                                                                          2)
                                                                         X)
                                                                        (LIST
                                                                         'PART
                                                                         (LIST
                                                                          'RHS
                                                                          S)
                                                                         1))
                                                                       P)))
                                                               (T
                                                                (AEVAL
                                                                 (LIST 'LIST
                                                                       (LIST
                                                                        'EQUAL
                                                                        Y
                                                                        (LIST
                                                                         'SUB S
                                                                         ODE_Y))
                                                                       S
                                                                       P))))))))
                                               (CAR S))
                                              NIL)))
                            LOOPLABEL
                             (SETQ S (CDR S))
                             (COND
                              ((NULL S) (RETURN (CONS 'LIST FORALL-RESULT))))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (S)
                                         (AEVAL
                                          (LIST 'ODESOLVE-SIMP-ARBPARAM
                                                (LIST 'SUB
                                                      (LIST 'EQUAL P
                                                            (LIST
                                                             'NEWARBPARAM))
                                                      (COND
                                                       ((BOOLVALUE*
                                                         (REVALX
                                                          (EQCAR (CADDR S)
                                                                 'ROOT_OF)))
                                                        (AEVAL
                                                         (LIST 'LIST
                                                               (LIST 'EQUAL Y
                                                                     ODE_Y)
                                                               (LIST 'SUB
                                                                     (LIST
                                                                      'EQUAL
                                                                      (LIST
                                                                       'PART
                                                                       (LIST
                                                                        'RHS S)
                                                                       2)
                                                                      X)
                                                                     (LIST
                                                                      'PART
                                                                      (LIST
                                                                       'RHS S)
                                                                      1))
                                                               P)))
                                                       (T
                                                        (AEVAL
                                                         (LIST 'LIST
                                                               (LIST 'EQUAL Y
                                                                     (LIST 'SUB
                                                                           S
                                                                           ODE_Y))
                                                               S P))))))))
                                       (CAR S))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))))
                        (CAR SOLN))))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ SOLN (CDR SOLN))
              (GO LOOPLABEL)))))))
     DEPL*)) 
(FLAG '(ODESOLVE-SOLVABLE-X) 'OPFN) 
(PUT 'ODESOLVE-SOLVABLE-X 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-SOLVABLE-X 'DEFINED-ON-LINE '612) 
(PUT 'ODESOLVE-SOLVABLE-X 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SOLVABLE-X 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SOLVABLE-X (ODE_P P Y X)
    ((LAMBDA (DEPL*)
       (AND (NOT *ODESOLVE_FAST)
            (PROG (C ODE_X ODE_Y)
              (AEVAL
               (TRACEODE1 (LIST "Testing for a \"solvable for x\" ODE ...")))
              (SETQ C (AEVAL (LIST 'COEFF ODE_P X)))
              (COND
               ((OR (EVALNEQ (AEVAL (LIST 'LENGTH C)) 2)
                    (BOOLVALUE* (REVALX (LIST 'DEPENDS C X))))
                (RETURN (AEVAL 'NIL))))
              (SETQ ODE_X
                      (AEVAL
                       (LIST 'MINUS
                             (LIST 'QUOTIENT (LIST 'FIRST C)
                                   (LIST 'SECOND C)))))
              (DEPEND1 P Y T)
              (SETQ ODE_Y
                      (AEVAL
                       (LIST 'NUM
                             (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 P)
                                   (LIST 'DF ODE_X Y)))))
              (AEVAL
               (TRACEODE
                (LIST "It is \"solvable for x\" and reduces "
                      "to this subsidiary ODE for y'(y):")))
              (SETQ ODE_Y (AEVAL (LIST 'ODESOLVE-FIRSTORDER ODE_Y P Y)))
              (COND
               ((NOT (BOOLVALUE* ODE_Y))
                (PROGN
                 (AEVAL (TRACEODE (LIST "But ODESolve cannot solve it! ")))
                 (RETURN (AEVAL 'NIL)))))
              (AEVAL
               (TRACEODE
                (LIST "The subsidiary solution is " ODE_Y
                      " and the main ODE can be solved parametrically "
                      "in terms of the derivative.")))
              (RETURN
               (COND
                ((BOOLVALUE*
                  (REVALX (OR *ODESOLVE_IMPLICIT *ODESOLVE_EXPLICIT)))
                 (AEVAL (LIST 'ODESOLVE-ELIM-PARAM ODE_X X ODE_Y P Y)))
                (T
                 (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SOLN (GETRLIST (AEVAL ODE_Y)))
                  STARTOVER
                   (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (SOLN)
                              (PROGN
                               (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN Y)))
                               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ S (GETRLIST (AEVAL SOLN)))
                                 (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (S)
                                                     (AEVAL
                                                      (LIST
                                                       'ODESOLVE-SIMP-ARBPARAM
                                                       (LIST 'SUB
                                                             (LIST 'EQUAL P
                                                                   (LIST
                                                                    'NEWARBPARAM))
                                                             (COND
                                                              ((BOOLVALUE*
                                                                (REVALX
                                                                 (EQCAR
                                                                  (CADDR S)
                                                                  'ROOT_OF)))
                                                               (AEVAL
                                                                (LIST 'LIST
                                                                      (LIST
                                                                       'SUB
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'PART
                                                                         (LIST
                                                                          'RHS
                                                                          S)
                                                                         2)
                                                                        Y)
                                                                       (LIST
                                                                        'PART
                                                                        (LIST
                                                                         'RHS
                                                                         S)
                                                                        1))
                                                                      (LIST
                                                                       'EQUAL X
                                                                       ODE_X)
                                                                      P)))
                                                              (T
                                                               (AEVAL
                                                                (LIST 'LIST S
                                                                      (LIST
                                                                       'EQUAL X
                                                                       (LIST
                                                                        'SUB S
                                                                        ODE_X))
                                                                      P))))))))
                                                   (CAR S))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ S (CDR S))
                                 (COND
                                  ((NULL S)
                                   (RETURN (CONS 'LIST FORALL-RESULT))))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (S)
                                             (AEVAL
                                              (LIST 'ODESOLVE-SIMP-ARBPARAM
                                                    (LIST 'SUB
                                                          (LIST 'EQUAL P
                                                                (LIST
                                                                 'NEWARBPARAM))
                                                          (COND
                                                           ((BOOLVALUE*
                                                             (REVALX
                                                              (EQCAR (CADDR S)
                                                                     'ROOT_OF)))
                                                            (AEVAL
                                                             (LIST 'LIST
                                                                   (LIST 'SUB
                                                                         (LIST
                                                                          'EQUAL
                                                                          (LIST
                                                                           'PART
                                                                           (LIST
                                                                            'RHS
                                                                            S)
                                                                           2)
                                                                          Y)
                                                                         (LIST
                                                                          'PART
                                                                          (LIST
                                                                           'RHS
                                                                           S)
                                                                          1))
                                                                   (LIST 'EQUAL
                                                                         X
                                                                         ODE_X)
                                                                   P)))
                                                           (T
                                                            (AEVAL
                                                             (LIST 'LIST S
                                                                   (LIST 'EQUAL
                                                                         X
                                                                         (LIST
                                                                          'SUB
                                                                          S
                                                                          ODE_X))
                                                                   P))))))))
                                           (CAR S))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
                            (CAR SOLN)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ SOLN (CDR SOLN))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (SOLN)
                               (PROGN
                                (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN Y)))
                                (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ S (GETRLIST (AEVAL SOLN)))
                                  (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (S)
                                                      (AEVAL
                                                       (LIST
                                                        'ODESOLVE-SIMP-ARBPARAM
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL P
                                                                    (LIST
                                                                     'NEWARBPARAM))
                                                              (COND
                                                               ((BOOLVALUE*
                                                                 (REVALX
                                                                  (EQCAR
                                                                   (CADDR S)
                                                                   'ROOT_OF)))
                                                                (AEVAL
                                                                 (LIST 'LIST
                                                                       (LIST
                                                                        'SUB
                                                                        (LIST
                                                                         'EQUAL
                                                                         (LIST
                                                                          'PART
                                                                          (LIST
                                                                           'RHS
                                                                           S)
                                                                          2)
                                                                         Y)
                                                                        (LIST
                                                                         'PART
                                                                         (LIST
                                                                          'RHS
                                                                          S)
                                                                         1))
                                                                       (LIST
                                                                        'EQUAL
                                                                        X
                                                                        ODE_X)
                                                                       P)))
                                                               (T
                                                                (AEVAL
                                                                 (LIST 'LIST S
                                                                       (LIST
                                                                        'EQUAL
                                                                        X
                                                                        (LIST
                                                                         'SUB S
                                                                         ODE_X))
                                                                       P))))))))
                                                    (CAR S))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ S (CDR S))
                                  (COND
                                   ((NULL S)
                                    (RETURN (CONS 'LIST FORALL-RESULT))))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S)
                                              (AEVAL
                                               (LIST 'ODESOLVE-SIMP-ARBPARAM
                                                     (LIST 'SUB
                                                           (LIST 'EQUAL P
                                                                 (LIST
                                                                  'NEWARBPARAM))
                                                           (COND
                                                            ((BOOLVALUE*
                                                              (REVALX
                                                               (EQCAR (CADDR S)
                                                                      'ROOT_OF)))
                                                             (AEVAL
                                                              (LIST 'LIST
                                                                    (LIST 'SUB
                                                                          (LIST
                                                                           'EQUAL
                                                                           (LIST
                                                                            'PART
                                                                            (LIST
                                                                             'RHS
                                                                             S)
                                                                            2)
                                                                           Y)
                                                                          (LIST
                                                                           'PART
                                                                           (LIST
                                                                            'RHS
                                                                            S)
                                                                           1))
                                                                    (LIST
                                                                     'EQUAL X
                                                                     ODE_X)
                                                                    P)))
                                                            (T
                                                             (AEVAL
                                                              (LIST 'LIST S
                                                                    (LIST
                                                                     'EQUAL X
                                                                     (LIST 'SUB
                                                                           S
                                                                           ODE_X))
                                                                    P))))))))
                                            (CAR S))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))
                             (CAR SOLN))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ SOLN (CDR SOLN))
                   (GO LOOPLABEL))))))))
     DEPL*)) 
(AEVAL (OPERATOR (LIST 'ARBPARAM))) 
(SETK '!ARBPARAM (AEVAL 0)) 
(PUT 'NEWARBPARAM 'NUMBER-OF-ARGS 0) 
(FLAG '(NEWARBPARAM) 'OPFN) 
(PUT 'NEWARBPARAM 'DEFINED-ON-LINE '660) 
(PUT 'NEWARBPARAM 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'NEWARBPARAM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEWARBPARAM NIL
    (LIST 'ARBPARAM (SETK '!ARBPARAM (AEVAL (LIST 'PLUS '!ARBPARAM 1))))) 
(PUT 'ODESOLVE-ELIM-PARAM 'NUMBER-OF-ARGS 5) 
(FLAG '(ODESOLVE-ELIM-PARAM) 'OPFN) 
(PUT 'ODESOLVE-ELIM-PARAM 'DEFINED-ON-LINE '663) 
(PUT 'ODESOLVE-ELIM-PARAM 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-ELIM-PARAM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-ELIM-PARAM (ODE_Y Y ODE_X P DEPVAR)
    (PROG (SOLN RESULT)
      (COND
       ((BOOLVALUE*
         (REVALX
          (LIST 'POLYNOMIALP
                (SETQ ODE_Y (REVALX (LIST 'NUM (LIST 'DIFFERENCE Y ODE_Y))))
                P)))
        (PROGN
         (SETQ RESULT (AEVAL (LIST 'LIST)))
         (WHILE
          (AND (EVALNEQ (AEVAL* ODE_X) (AEVAL* (LIST 'LIST)))
               (BOOLVALUE*
                (REVALX
                 (LIST 'POLYNOMIALP
                       (SETQ SOLN
                               (REVALX
                                (LIST 'NUM
                                      (LIST '*EQN2A (LIST 'FIRST ODE_X)))))
                       P))))
          (PROGN
           (SETQ RESULT
                   (AEVAL* (LIST 'CONS (LIST 'RESULTANT ODE_Y SOLN P) RESULT)))
           (SETQ ODE_X (AEVAL* (LIST 'REST ODE_X)))))
         (COND
          ((EVALEQUAL (AEVAL ODE_X) (AEVAL (LIST 'LIST)))
           (RETURN (AEVAL (LIST 'ODESOLVE-TIDY-IMPLICIT RESULT Y))))))))
      (SETQ ODE_Y (AEVAL (LIST 'SOLVE ODE_Y P)))
      (SETQ ODE_Y (AEVAL (LIST 'EXPAND_CASES ODE_Y)))
      (COND
       ((NOT (BOOLVALUE* (REVALX (LIST 'SMEMBER 'ROOT_OF ODE_Y))))
        (PROGN
         (SETQ RESULT
                 (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
                   (SETQ SOLN (GETRLIST (AEVAL ODE_Y)))
                  STARTOVER
                   (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (SOLN)
                              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                (SETQ S (GETRLIST (AEVAL ODE_X)))
                               STARTOVER
                                (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (S)
                                           (COND
                                            ((EVALEQUAL (AEVAL (LIST 'RHS S))
                                                        0)
                                             (COND
                                              ((EVALNEQ
                                                (SETQ S
                                                        (AEVAL
                                                         (LIST 'SUB SOLN
                                                               (LIST 'NUM
                                                                     (LIST 'LHS
                                                                           S)))))
                                                0)
                                               (AEVAL
                                                (LIST 'LIST (LIST 'NUM S))))
                                              (T (AEVAL (LIST 'LIST)))))
                                            (T
                                             (AEVAL
                                              (LIST 'LIST
                                                    (LIST 'NUM
                                                          (LIST 'DIFFERENCE
                                                                (LIST 'SUB SOLN
                                                                      (LIST
                                                                       'RHS S))
                                                                'X)))))))
                                         (CAR S)))
                                (SETQ FORALL-ENDPTR
                                        (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                                (SETQ S (CDR S))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL S) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (GETRLIST
                                         ((LAMBDA (S)
                                            (COND
                                             ((EVALEQUAL (AEVAL (LIST 'RHS S))
                                                         0)
                                              (COND
                                               ((EVALNEQ
                                                 (SETQ S
                                                         (AEVAL
                                                          (LIST 'SUB SOLN
                                                                (LIST 'NUM
                                                                      (LIST
                                                                       'LHS
                                                                       S)))))
                                                 0)
                                                (AEVAL
                                                 (LIST 'LIST (LIST 'NUM S))))
                                               (T (AEVAL (LIST 'LIST)))))
                                             (T
                                              (AEVAL
                                               (LIST 'LIST
                                                     (LIST 'NUM
                                                           (LIST 'DIFFERENCE
                                                                 (LIST 'SUB
                                                                       SOLN
                                                                       (LIST
                                                                        'RHS
                                                                        S))
                                                                 'X)))))))
                                          (CAR S))))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ S (CDR S))
                                (GO LOOPLABEL)))
                            (CAR SOLN)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ SOLN (CDR SOLN))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (SOLN)
                               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ S (GETRLIST (AEVAL ODE_X)))
                                STARTOVER
                                 (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         ((LAMBDA (S)
                                            (COND
                                             ((EVALEQUAL (AEVAL (LIST 'RHS S))
                                                         0)
                                              (COND
                                               ((EVALNEQ
                                                 (SETQ S
                                                         (AEVAL
                                                          (LIST 'SUB SOLN
                                                                (LIST 'NUM
                                                                      (LIST
                                                                       'LHS
                                                                       S)))))
                                                 0)
                                                (AEVAL
                                                 (LIST 'LIST (LIST 'NUM S))))
                                               (T (AEVAL (LIST 'LIST)))))
                                             (T
                                              (AEVAL
                                               (LIST 'LIST
                                                     (LIST 'NUM
                                                           (LIST 'DIFFERENCE
                                                                 (LIST 'SUB
                                                                       SOLN
                                                                       (LIST
                                                                        'RHS
                                                                        S))
                                                                 'X)))))))
                                          (CAR S)))
                                 (SETQ FORALL-ENDPTR
                                         (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                                 (SETQ S (CDR S))
                                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                LOOPLABEL
                                 (COND ((NULL S) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (GETRLIST
                                          ((LAMBDA (S)
                                             (COND
                                              ((EVALEQUAL (AEVAL (LIST 'RHS S))
                                                          0)
                                               (COND
                                                ((EVALNEQ
                                                  (SETQ S
                                                          (AEVAL
                                                           (LIST 'SUB SOLN
                                                                 (LIST 'NUM
                                                                       (LIST
                                                                        'LHS
                                                                        S)))))
                                                  0)
                                                 (AEVAL
                                                  (LIST 'LIST (LIST 'NUM S))))
                                                (T (AEVAL (LIST 'LIST)))))
                                              (T
                                               (AEVAL
                                                (LIST 'LIST
                                                      (LIST 'NUM
                                                            (LIST 'DIFFERENCE
                                                                  (LIST 'SUB
                                                                        SOLN
                                                                        (LIST
                                                                         'RHS
                                                                         S))
                                                                  'X)))))))
                                           (CAR S))))
                                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                 (SETQ S (CDR S))
                                 (GO LOOPLABEL)))
                             (CAR SOLN))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ SOLN (CDR SOLN))
                   (GO LOOPLABEL)))
         (RETURN (AEVAL (LIST 'ODESOLVE-TIDY-IMPLICIT RESULT DEPVAR))))))
      (AEVAL
       (TRACEODE
        (LIST "But cannot eliminate parameter "
              "to make solution explicit."))))) 
(PUT 'ODESOLVE-TIDY-IMPLICIT 'NUMBER-OF-ARGS 2) 
(FLAG '(ODESOLVE-TIDY-IMPLICIT) 'OPFN) 
(PUT 'ODESOLVE-TIDY-IMPLICIT 'DEFINED-ON-LINE '699) 
(PUT 'ODESOLVE-TIDY-IMPLICIT 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-TIDY-IMPLICIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-TIDY-IMPLICIT (SOLNS DEPVAR)
    (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
      (SETQ SOLN (GETRLIST (AEVAL SOLNS)))
     STARTOVER
      (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              ((LAMBDA (SOLN)
                 (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FAC (GETRLIST (AEVAL (LIST 'FACTORIZE SOLN))))
                  STARTOVER
                   (COND ((NULL FAC) (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (FAC)
                              (COND
                               ((BOOLVALUE*
                                 (REVALX
                                  (LIST 'SMEMBER DEPVAR
                                        (SETQ FAC
                                                (REVALX (LIST 'FIRST FAC))))))
                                (AEVAL (LIST 'LIST (LIST 'EQUAL FAC 0))))
                               (T (AEVAL (LIST 'LIST)))))
                            (CAR FAC)))
                   (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                   (SETQ FAC (CDR FAC))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (GETRLIST
                            ((LAMBDA (FAC)
                               (COND
                                ((BOOLVALUE*
                                  (REVALX
                                   (LIST 'SMEMBER DEPVAR
                                         (SETQ FAC
                                                 (REVALX (LIST 'FIRST FAC))))))
                                 (AEVAL (LIST 'LIST (LIST 'EQUAL FAC 0))))
                                (T (AEVAL (LIST 'LIST)))))
                             (CAR FAC))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ FAC (CDR FAC))
                   (GO LOOPLABEL)))
               (CAR SOLN)))
      (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
      (SETQ SOLN (CDR SOLN))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL SOLN) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (GETRLIST
               ((LAMBDA (SOLN)
                  (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                    (SETQ FAC (GETRLIST (AEVAL (LIST 'FACTORIZE SOLN))))
                   STARTOVER
                    (COND ((NULL FAC) (RETURN (MAKELIST NIL))))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (FAC)
                               (COND
                                ((BOOLVALUE*
                                  (REVALX
                                   (LIST 'SMEMBER DEPVAR
                                         (SETQ FAC
                                                 (REVALX (LIST 'FIRST FAC))))))
                                 (AEVAL (LIST 'LIST (LIST 'EQUAL FAC 0))))
                                (T (AEVAL (LIST 'LIST)))))
                             (CAR FAC)))
                    (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                    (SETQ FAC (CDR FAC))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (GETRLIST
                             ((LAMBDA (FAC)
                                (COND
                                 ((BOOLVALUE*
                                   (REVALX
                                    (LIST 'SMEMBER DEPVAR
                                          (SETQ FAC
                                                  (REVALX
                                                   (LIST 'FIRST FAC))))))
                                  (AEVAL (LIST 'LIST (LIST 'EQUAL FAC 0))))
                                 (T (AEVAL (LIST 'LIST)))))
                              (CAR FAC))))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ FAC (CDR FAC))
                    (GO LOOPLABEL)))
                (CAR SOLN))))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ SOLN (CDR SOLN))
      (GO LOOPLABEL))) 
(SWITCH (LIST 'ODESOLVE_SIMP_ARBPARAM)) 
(FLAG '(ODESOLVE-SIMP-ARBPARAM) 'OPFN) 
(PUT 'ODESOLVE-SIMP-ARBPARAM 'NUMBER-OF-ARGS 1) 
(PUT 'ODESOLVE-SIMP-ARBPARAM 'DEFINED-ON-LINE '708) 
(PUT 'ODESOLVE-SIMP-ARBPARAM 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'ODESOLVE-SIMP-ARBPARAM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODESOLVE-SIMP-ARBPARAM (U)
    (PROG (*PRECISE X Y SS_X SS_Y ARBEXPRNS_X ARBEXPRNS_Y ARBEXPRNS PARAM)
      (COND
       ((NOT (AND (EQCAR U 'LIST) (EQUAL (LENGTH U) 4)))
        (TYPERR U "parametric ODE solution")))
      (COND ((NOT *ODESOLVE_SIMP_ARBPARAM) (RETURN U)))
      (SETQ X (LHS (CADR U)))
      (SETQ Y (LHS (CADDR U)))
      (COND
       ((NOT (SETQ SS_X (ODESOLVE-STRUCTR (CADDR (CADR U)) X Y 'ARBPARAM)))
        (RETURN U)))
      (COND
       ((NOT (SETQ SS_Y (ODESOLVE-STRUCTR (CADDR (CADDR U)) X Y 'ARBPARAM)))
        (RETURN U)))
      (SETQ SS_X (CDR SS_X))
      (SETQ SS_Y (CDR SS_Y))
      (SETQ ARBEXPRNS_X
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (CDR SS_X))
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (S) (CADDR S)) (CAR S)) NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (S) (CADDR S)) (CAR S)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ARBEXPRNS_Y
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (CDR SS_Y))
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (S) (CADDR S)) (CAR S)) NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (S) (CADDR S)) (CAR S)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((NULL (SETQ ARBEXPRNS (INTERSECTION ARBEXPRNS_X ARBEXPRNS_Y)))
        (RETURN U)))
      (SETQ ARBEXPRNS_X (CDR SS_X))
      (SETQ SS_X (CAR SS_X))
      (SETQ ARBEXPRNS_Y (CDR SS_Y))
      (SETQ SS_Y (CAR SS_Y))
      (SETQ ARBEXPRNS_X
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S ARBEXPRNS_X)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (COND ((MEMBER (CADDR S) ARBEXPRNS) (LIST S))
                                 (T
                                  (PROGN
                                   (SETQ SS_X (SUBEVAL (LIST S SS_X)))
                                   NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (COND ((MEMBER (CADDR S) ARBEXPRNS) (LIST S))
                                 (T
                                  (PROGN
                                   (SETQ SS_X (SUBEVAL (LIST S SS_X)))
                                   NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETQ ARBEXPRNS_Y
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S ARBEXPRNS_Y)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (COND ((MEMBER (CADDR S) ARBEXPRNS) (LIST S))
                                 (T
                                  (PROGN
                                   (SETQ SS_Y (SUBEVAL (LIST S SS_Y)))
                                   NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (COND ((MEMBER (CADDR S) ARBEXPRNS) (LIST S))
                                 (T
                                  (PROGN
                                   (SETQ SS_Y (SUBEVAL (LIST S SS_Y)))
                                   NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (TRACEODE
       (LIST "Simplifying the arbparam expressions in " U
             " by the rewrites ..."))
      (SETQ PARAM (CADDDR U))
      (PROG (S)
        (SETQ S ARBEXPRNS_X)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (TRACEODE (LIST (LIST 'REPLACEBY (LIST 'RHS S) PARAM)))
            (SETQ SS_X (AEVAL (LIST 'SUB (LIST 'SOLVE S PARAM) SS_X)))
            (SETQ SS_X (SUBEVAL (LIST (LIST 'EQUAL (CADR S) PARAM) SS_X)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (PROG (S)
        (SETQ S ARBEXPRNS_Y)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ SS_Y (AEVAL (LIST 'SUB (LIST 'SOLVE S PARAM) SS_Y)))
            (SETQ SS_Y (SUBEVAL (LIST (LIST 'EQUAL (CADR S) PARAM) SS_Y)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (COND
       ((AND (SMEMBER PARAM (DEN SS_X)) (SMEMBER PARAM (DEN SS_Y)))
        (PROG (SS_X1 SS_Y1)
          (SETQ SS_X1
                  (AEVAL
                   (LIST 'SUB (LIST 'EQUAL PARAM (LIST 'QUOTIENT 1 PARAM))
                         SS_X)))
          (COND ((SMEMBER PARAM (DEN SS_X1)) (RETURN NIL)))
          (SETQ SS_Y1
                  (AEVAL
                   (LIST 'SUB (LIST 'EQUAL PARAM (LIST 'QUOTIENT 1 PARAM))
                         SS_Y)))
          (COND ((SMEMBER PARAM (DEN SS_Y1)) (RETURN NIL)))
          (TRACEODE
           (LIST "Simplifying further by the rewrite "
                 (LIST 'REPLACEBY PARAM (LIST 'QUOTIENT 1 PARAM))))
          (SETQ SS_X SS_X1)
          (SETQ SS_Y SS_Y1))))
      (RETURN
       (CONS 'LIST (LIST (LIST 'EQUAL X SS_X) (LIST 'EQUAL Y SS_Y) PARAM))))) 
(FLAG '(POLYNOMIALP) 'OPFN) 
(PUT 'POLYNOMIALP 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMIALP 'DEFINED-ON-LINE '778) 
(PUT 'POLYNOMIALP 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'POLYNOMIALP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMIALP (POL VAR) (POLYNOMIAL-FORM-P (CAR (SIMP* POL)) (*A2K VAR))) 
(PUT 'POLYNOMIAL-FORM-P 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMIAL-FORM-P 'DEFINED-ON-LINE '783) 
(PUT 'POLYNOMIAL-FORM-P 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'POLYNOMIAL-FORM-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMIAL-FORM-P (SF Y)
    (OR (OR (ATOM SF) (ATOM (CAR SF)))
        (AND (POLYNOMIAL-TERM-P (CAR SF) Y) (POLYNOMIAL-FORM-P (CDR SF) Y)))) 
(PUT 'POLYNOMIAL-TERM-P 'NUMBER-OF-ARGS 2) 
(PUT 'POLYNOMIAL-TERM-P 'DEFINED-ON-LINE '789) 
(PUT 'POLYNOMIAL-TERM-P 'DEFINED-IN-FILE 'ODESOLVE/ODENON1.RED) 
(PUT 'POLYNOMIAL-TERM-P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE POLYNOMIAL-TERM-P (ST Y)
    (COND ((EQ (CAAR ST) Y) (NOT (SMEMBER Y (CDR ST))))
          ((NOT (SMEMBER Y (CAAR ST))) (POLYNOMIAL-FORM-P (CDR ST) Y)))) 
(ENDMODULE) 