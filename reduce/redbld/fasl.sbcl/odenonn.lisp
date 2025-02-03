(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODENONN)) 
(PUT 'ODESOLVE-NONLINEARN 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-NONLINEARN) 'OPFN) 
(PUT 'ODESOLVE-NONLINEARN 'DEFINED-ON-LINE '39) 
(PUT 'ODESOLVE-NONLINEARN 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-NONLINEARN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-NONLINEARN (ODE Y X) (ODENON-REDUCE-ORDER ODE Y X)) 
(DEFINE (LIST (LIST 'EQUAL 'ODENON-REDUCE-ORDER-NEXT 'ODESOLVE-SHIFT))) 
(DEFINE (LIST (LIST 'EQUAL 'ODESOLVE-SHIFT-NEXT 'ODESOLVE-NONLINEARN*1))) 
(SWITCH (LIST (LIST 'EQUAL 'ODESOLVE_EQUIDIM_Y 'ON))) 
(PUT 'ODESOLVE-NONLINEARN*1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-NONLINEARN*1) 'OPFN) 
(PUT 'ODESOLVE-NONLINEARN*1 'DEFINED-ON-LINE '55) 
(PUT 'ODESOLVE-NONLINEARN*1 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-NONLINEARN*1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-NONLINEARN*1 (ODE Y X)
    (OR (ODESOLVE-AUTONOMOUS ODE Y X) (ODESOLVE-SCALEINV ODE Y X)
        (AND *ODESOLVE_EQUIDIM_Y (ODESOLVE-EQUIDIM-Y ODE Y X)))) 
(PUT 'ODENON-REDUCE-ORDER 'NUMBER-OF-ARGS 3) 
(FLAG '(ODENON-REDUCE-ORDER) 'OPFN) 
(PUT 'ODENON-REDUCE-ORDER 'DEFINED-ON-LINE '63) 
(PUT 'ODENON-REDUCE-ORDER 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODENON-REDUCE-ORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODENON-REDUCE-ORDER (ODE Y X)
    (PROG (DERIV_ORDERS MIN_ORDER MAX_ORDER)
      (AEVAL (TRACEODE1 (LIST "Trying trivial order reduction ...")))
      (SETQ DERIV_ORDERS (AEVAL (LIST 'GET_DERIV_ORDERS ODE Y)))
      (COND
       ((OR (EVALEQUAL (AEVAL DERIV_ORDERS) (AEVAL (LIST 'LIST)))
            (EVALEQUAL (AEVAL DERIV_ORDERS) (AEVAL (LIST 'LIST 0))))
        (RETURN (AEVAL (LIST 'LIST (LIST 'EQUAL ODE 0))))))
      (COND
       ((OR (EVALEQUAL (SETQ MIN_ORDER (AEVAL (LIST 'MIN DERIV_ORDERS))) 0)
            (EVALEQUAL (AEVAL (LIST 'LENGTH DERIV_ORDERS)) 1))
        (RETURN (AEVAL (LIST 'ODESOLVE-SHIFT ODE Y X)))))
      (SETQ MAX_ORDER (AEVAL (LIST 'MAX DERIV_ORDERS)))
      (SETQ ODE (AEVAL (LIST 'SUB (LIST 'EQUAL 'DF 'ODESOLVE-DF) ODE)))
      (PROG (ORD)
        (SETQ ORD (AEVAL* MIN_ORDER))
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* MAX_ORDER) ORD)) (RETURN NIL)))
        (SETQ ODE
                (COND
                 ((EVALEQUAL (AEVAL* ORD) 1)
                  (AEVAL*
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'ODESOLVE-DF Y X) Y))
                         ODE)))
                 (T
                  (AEVAL*
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY (LIST 'ODESOLVE-DF Y X ORD)
                                     (LIST 'ODESOLVE-DF Y X
                                           (LIST 'DIFFERENCE ORD MIN_ORDER))))
                         ODE)))))
        (SETQ ORD
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 ORD))
        (GO LAB))
      (SETQ ODE (AEVAL (LIST 'SUB (LIST 'EQUAL 'ODESOLVE-DF 'DF) ODE)))
      (AEVAL
       (TRACEODE
        (LIST "Performing trivial order reduction to give " "the order "
              (LIST 'DIFFERENCE MAX_ORDER MIN_ORDER) " nonlinear ODE: "
              (LIST 'EQUAL ODE 0))))
      (SETQ ODE
              (AEVAL
               ((LAMBDA (*ODESOLVE_EXPLICIT)
                  (COND
                   ((EQUAL (DIFFERENCE MAX_ORDER MIN_ORDER) 1)
                    (ODESOLVE-NONLINEAR1 ODE Y X))
                   (T (ODESOLVE-SHIFT ODE Y X))))
                T)))
      (COND
       ((NOT (BOOLVALUE* ODE))
        (PROGN
         (AEVAL (TRACEODE (LIST "Cannot solve order-reduced ODE!")))
         (RETURN (AEVAL 'NIL)))))
      (AEVAL (TRACEODE (LIST "Solution of order-reduced ODE is " ODE)))
      (AEVAL
       (TRACEODE
        (LIST "Restoring order, " (LIST 'REPLACEBY Y (LIST 'DF Y X MIN_ORDER))
              ", to give: "
              (LIST 'SUB (LIST 'EQUAL Y (LIST 'DF Y X MIN_ORDER)) ODE)
              " and re-solving ...")))
      (SETQ ODE
              (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
                (SETQ SOLN (GETRLIST (AEVAL ODE)))
               STARTOVER
                (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        ((LAMBDA (SOLN)
                           (COND
                            ((EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL Y))
                             (AEVAL
                              (LIST 'LIST
                                    (LIST 'EQUAL Y
                                          (LIST 'ODESOLVE-MULTI-INT*
                                                (LIST 'RHS SOLN) X
                                                MIN_ORDER)))))
                            (T
                             (PROGN
                              (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN Y)))
                              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                (SETQ S (GETRLIST (AEVAL SOLN)))
                                (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (S)
                                                    (COND
                                                     ((EVALEQUAL
                                                       (AEVAL (LIST 'LHS S))
                                                       (AEVAL Y))
                                                      (AEVAL
                                                       (LIST 'EQUAL Y
                                                             (LIST
                                                              'ODESOLVE-MULTI-INT*
                                                              (LIST 'RHS S) X
                                                              MIN_ORDER))))
                                                     (T
                                                      (AEVAL
                                                       (LIST 'SUB
                                                             (LIST 'EQUAL Y
                                                                   (LIST 'DF Y
                                                                         X
                                                                         MIN_ORDER))
                                                             S)))))
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
                                            (COND
                                             ((EVALEQUAL (AEVAL (LIST 'LHS S))
                                                         (AEVAL Y))
                                              (AEVAL
                                               (LIST 'EQUAL Y
                                                     (LIST 'ODESOLVE-MULTI-INT*
                                                           (LIST 'RHS S) X
                                                           MIN_ORDER))))
                                             (T
                                              (AEVAL
                                               (LIST 'SUB
                                                     (LIST 'EQUAL Y
                                                           (LIST 'DF Y X
                                                                 MIN_ORDER))
                                                     S)))))
                                          (CAR S))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))))
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
                             ((EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL Y))
                              (AEVAL
                               (LIST 'LIST
                                     (LIST 'EQUAL Y
                                           (LIST 'ODESOLVE-MULTI-INT*
                                                 (LIST 'RHS SOLN) X
                                                 MIN_ORDER)))))
                             (T
                              (PROGN
                               (SETQ SOLN (AEVAL (LIST 'SOLVE SOLN Y)))
                               (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ S (GETRLIST (AEVAL SOLN)))
                                 (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (S)
                                                     (COND
                                                      ((EVALEQUAL
                                                        (AEVAL (LIST 'LHS S))
                                                        (AEVAL Y))
                                                       (AEVAL
                                                        (LIST 'EQUAL Y
                                                              (LIST
                                                               'ODESOLVE-MULTI-INT*
                                                               (LIST 'RHS S) X
                                                               MIN_ORDER))))
                                                      (T
                                                       (AEVAL
                                                        (LIST 'SUB
                                                              (LIST 'EQUAL Y
                                                                    (LIST 'DF Y
                                                                          X
                                                                          MIN_ORDER))
                                                              S)))))
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
                                             (COND
                                              ((EVALEQUAL (AEVAL (LIST 'LHS S))
                                                          (AEVAL Y))
                                               (AEVAL
                                                (LIST 'EQUAL Y
                                                      (LIST
                                                       'ODESOLVE-MULTI-INT*
                                                       (LIST 'RHS S) X
                                                       MIN_ORDER))))
                                              (T
                                               (AEVAL
                                                (LIST 'SUB
                                                      (LIST 'EQUAL Y
                                                            (LIST 'DF Y X
                                                                  MIN_ORDER))
                                                      S)))))
                                           (CAR S))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))))
                          (CAR SOLN))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ SOLN (CDR SOLN))
                (GO LOOPLABEL)))
      (RETURN (AEVAL (LIST 'ODESOLVE-SIMP-ARBCONSTS ODE Y X))))) 
(PUT 'ODESOLVE-MULTI-INT* 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-MULTI-INT*) 'OPFN) 
(PUT 'ODESOLVE-MULTI-INT* 'DEFINED-ON-LINE '121) 
(PUT 'ODESOLVE-MULTI-INT* 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-MULTI-INT* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-MULTI-INT* (Y X M)
    (LIST 'PLUS (LIST 'ODESOLVE-MULTI-INT Y X M)
          (PROG (I FORALL-RESULT)
            (SETQ I 0)
            (SETQ FORALL-RESULT 0)
           LAB1
            (COND
             ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'DIFFERENCE M 1)) I))
              (RETURN FORALL-RESULT)))
            (SETQ FORALL-RESULT
                    (AEVAL*
                     (LIST 'PLUS
                           (AEVAL*
                            (LIST 'TIMES (PROGN (AEVAL* (LIST 'NEWARBCONST)))
                                  (LIST 'EXPT X I)))
                           FORALL-RESULT)))
            (SETQ I
                    ((LAMBDA (FORALL-RESULT)
                       (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                     I))
            (GO LAB1)))) 
(AEVAL (OPERATOR (LIST 'ODESOLVE-SUB*))) 
(PUT 'ODESOLVE-SHIFT 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SHIFT) 'OPFN) 
(PUT 'ODESOLVE-SHIFT 'DEFINED-ON-LINE '131) 
(PUT 'ODESOLVE-SHIFT 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-SHIFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SHIFT (ODE Y X)
    (COND
     ((NOT *ODESOLVE_FAST)
      (PROG (DERIV_ORDERS A C D)
        (AEVAL
         (TRACEODE1 (LIST "Looking for an independent variable shift ...")))
        (SETQ DERIV_ORDERS (AEVAL (LIST 'GET_DERIV_ORDERS ODE Y)))
        (SETQ DERIV_ORDERS (AEVAL (LIST 'SORT DERIV_ORDERS 'GREATERP)))
        (WHILE
         (AND (EVALNEQ (AEVAL* DERIV_ORDERS) (AEVAL* (LIST 'LIST)))
              (FREEOF
               (SETQ C
                       (REVALX
                        (LIST 'LCOF ODE
                              (LIST 'DF Y X (LIST 'FIRST DERIV_ORDERS)))))
               (REVALX X)))
         (SETQ DERIV_ORDERS (AEVAL* (LIST 'REST DERIV_ORDERS))))
        (COND
         ((EVALEQUAL (AEVAL DERIV_ORDERS) (AEVAL (LIST 'LIST)))
          (RETURN (AEVAL (LIST 'ODESOLVE-NONLINEARN*1 ODE Y X)))))
        (COND
         ((EVALNEQ (SETQ D (AEVAL (LIST 'DEG C X))) 1)
          (PROGN
           (SETQ C (AEVAL (LIST 'DECOMPOSE C)))
           (WHILE
            (AND
             (EVALNEQ (SETQ C (AEVAL* (LIST 'REST C))) (AEVAL* (LIST 'LIST)))
             (EVALNEQ (AEVAL* (LIST 'DEG (LIST 'RHS (LIST 'FIRST C)) X)) 1))
            (AEVAL* 'NIL))
           (COND
            ((EVALNEQ (AEVAL C) (AEVAL (LIST 'LIST)))
             (SETQ C (AEVAL (LIST 'RHS (LIST 'FIRST C))))))
           (COND
            ((EVALNEQ (AEVAL (LIST 'DEG C X)) 1)
             (RETURN (AEVAL (LIST 'ODESOLVE-NONLINEARN*1 ODE Y X))))))))
        (COND
         ((OR (NOT (FREEOF (REVALX C) (REVALX Y)))
              (NOT (FREEOF (SETQ C (REVALX (LIST 'COEFF C X))) (REVALX X)))
              (EVALEQUAL (AEVAL (LIST 'FIRST C)) 0))
          (RETURN (AEVAL (LIST 'ODESOLVE-NONLINEARN*1 ODE Y X)))))
        (SETQ C
                (AEVAL
                 (LIST 'QUOTIENT (LIST 'FIRST C)
                       (SETQ A (AEVAL (LIST 'SECOND C))))))
        (SETQ ODE
                (AEVAL
                 (LIST 'QUOTIENT
                       (LIST 'SUB (LIST 'EQUAL X (LIST 'DIFFERENCE X C)) ODE)
                       (LIST 'EXPT A D))))
        (SETQ ODE
                (AEVAL
                 (LIST 'NUM
                       (LIST 'SUB (LIST 'EQUAL 'SUB 'ODESOLVE-SUB*) ODE))))
        (SETQ ODE
                (AEVAL
                 (LIST 'WHEREEXP
                       (LIST 'LIST
                             (LIST 'REPLACEBY
                                   (LIST 'ODESOLVE-SUB* (LIST '~ '|A |)
                                         (LIST '~ '|B |))
                                   '|B |))
                       ODE)))
        (AEVAL
         (TRACEODE
          (LIST "This ODE can be simplified by the "
                "independent variable shift "
                (LIST 'REPLACEBY X (LIST 'DIFFERENCE X C)) " to give: "
                (LIST 'EQUAL ODE 0))))
        (SETQ ODE (AEVAL (LIST 'ODESOLVE-NONLINEARN*1 ODE Y X)))
        (COND
         ((BOOLVALUE* ODE)
          (RETURN
           (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
             (SETQ SOLN (GETRLIST (AEVAL ODE)))
             (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (SOLN)
                                 (COND
                                  ((BOOLVALUE* (REVALX (EQCAR SOLN 'LIST)))
                                   (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ S (GETRLIST (AEVAL SOLN)))
                                     (COND ((NULL S) (RETURN (MAKELIST NIL))))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (S)
                                                         (COND
                                                          ((AND
                                                            (BOOLVALUE*
                                                             (REVALX
                                                              (EQCAR S
                                                                     'EQUAL)))
                                                            (EVALEQUAL
                                                             (AEVAL
                                                              (LIST 'LHS S))
                                                             (AEVAL X)))
                                                           (AEVAL
                                                            (LIST 'EQUAL X
                                                                  (LIST
                                                                   'DIFFERENCE
                                                                   (LIST 'RHS
                                                                         S)
                                                                   C))))
                                                          (T (AEVAL S))))
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
                                                 (COND
                                                  ((AND
                                                    (BOOLVALUE*
                                                     (REVALX (EQCAR S 'EQUAL)))
                                                    (EVALEQUAL
                                                     (AEVAL (LIST 'LHS S))
                                                     (AEVAL X)))
                                                   (AEVAL
                                                    (LIST 'EQUAL X
                                                          (LIST 'DIFFERENCE
                                                                (LIST 'RHS S)
                                                                C))))
                                                  (T (AEVAL S))))
                                               (CAR S))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                  (T
                                   (AEVAL
                                    (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS X C))
                                          SOLN)))))
                               (CAR SOLN))
                              NIL)))
            LOOPLABEL
             (SETQ SOLN (CDR SOLN))
             (COND ((NULL SOLN) (RETURN (CONS 'LIST FORALL-RESULT))))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (SOLN)
                         (COND
                          ((BOOLVALUE* (REVALX (EQCAR SOLN 'LIST)))
                           (PROG (S FORALL-RESULT FORALL-ENDPTR)
                             (SETQ S (GETRLIST (AEVAL SOLN)))
                             (COND ((NULL S) (RETURN (MAKELIST NIL))))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (S)
                                                 (COND
                                                  ((AND
                                                    (BOOLVALUE*
                                                     (REVALX (EQCAR S 'EQUAL)))
                                                    (EVALEQUAL
                                                     (AEVAL (LIST 'LHS S))
                                                     (AEVAL X)))
                                                   (AEVAL
                                                    (LIST 'EQUAL X
                                                          (LIST 'DIFFERENCE
                                                                (LIST 'RHS S)
                                                                C))))
                                                  (T (AEVAL S))))
                                               (CAR S))
                                              NIL)))
                            LOOPLABEL
                             (SETQ S (CDR S))
                             (COND
                              ((NULL S) (RETURN (CONS 'LIST FORALL-RESULT))))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (S)
                                         (COND
                                          ((AND
                                            (BOOLVALUE*
                                             (REVALX (EQCAR S 'EQUAL)))
                                            (EVALEQUAL (AEVAL (LIST 'LHS S))
                                                       (AEVAL X)))
                                           (AEVAL
                                            (LIST 'EQUAL X
                                                  (LIST 'DIFFERENCE
                                                        (LIST 'RHS S) C))))
                                          (T (AEVAL S))))
                                       (CAR S))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                          (T
                           (AEVAL
                            (LIST 'SUB (LIST 'EQUAL X (LIST 'PLUS X C))
                                  SOLN)))))
                       (CAR SOLN))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))))))) 
(PUT 'ODESOLVE-AUTONOMOUS 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-AUTONOMOUS) 'OPFN) 
(PUT 'ODESOLVE-AUTONOMOUS 'DEFINED-ON-LINE '184) 
(PUT 'ODESOLVE-AUTONOMOUS 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-AUTONOMOUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-AUTONOMOUS (ODE Y X)
    (PROG (ODE1 U SOLN)
      (AEVAL (TRACEODE1 (LIST "Testing whether ODE is autonomous ...")))
      (SETQ ODE1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST (LIST 'REPLACEBY (LIST 'DF Y X) 1)
                           (LIST 'REPLACEBY (LIST 'DF Y X (LIST '~ 'N)) 1))
                     ODE)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'SMEMBER X ODE1))) (RETURN (AEVAL 'NIL))))
      (SETQ U (AEVAL (LIST 'GENSYM)))
      (DEPEND1 U X T)
      (DEPEND1 U Y T)
      (SETQ ODE
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST (LIST 'REPLACEBY (LIST 'DF Y X) U)
                           (LIST 'REPLACEBY (LIST 'DF Y X (LIST '~ 'N))
                                 (LIST 'DF U X (LIST 'DIFFERENCE 'N 1))))
                     ODE)))
      (SETQ ODE
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'DF U X (LIST '~ 'N))
                                 (LIST 'WHEN
                                       (LIST 'TIMES U
                                             (LIST 'DF
                                                   (LIST 'DF U X
                                                         (LIST 'DIFFERENCE 'N
                                                               1))
                                                   Y))
                                       (LIST 'GREATERP 'N 1)))
                           (LIST 'REPLACEBY (LIST 'DF U X)
                                 (LIST 'TIMES U (LIST 'DF U Y))))
                     ODE)))
      (DEPEND1 U X NIL)
      (AEVAL
       (TRACEODE
        (LIST "This ODE is autonomous -- transforming dependent variable "
              "to derivative to give this ODE of order 1 lower: "
              (LIST 'EQUAL ODE 0))))
      (SETQ ODE (AEVAL ((LAMBDA (*ODESOLVE_EXPLICIT) (ODESOLVE*1 ODE U Y)) T)))
      (COND
       ((NOT (BOOLVALUE* ODE))
        (PROGN
         (AEVAL (DEPEND1 U Y NIL))
         (AEVAL (TRACEODE (LIST "Cannot solve transformed autonomous ODE!")))
         (RETURN (AEVAL 'NIL)))))
      (SETQ ODE (AEVAL (LIST 'SUB (LIST 'EQUAL U (LIST 'DF Y X)) ODE)))
      (DEPEND1 U Y NIL)
      (AEVAL
       (TRACEODE (LIST "Restoring order to give these first-order ODEs ...")))
      (SETQ SOLN (AEVAL (LIST 'LIST)))
     A
      (COND
       ((EVALNEQ (AEVAL ODE) (AEVAL (LIST 'LIST)))
        (COND
         ((BOOLVALUE*
           (SETQ U (REVALX (LIST 'ODESOLVE-FIRSTORDER (LIST 'FIRST ODE) Y X))))
          (PROGN
           (SETQ SOLN (AEVAL (LIST 'APPEND SOLN U)))
           (SETQ ODE (AEVAL (LIST 'REST ODE)))
           (GO A)))
         (T
          (PROGN
           (AEVAL
            (TRACEODE
             (LIST "Cannot solve one of the first-order ODEs "
                   "arising from solution of transformed autonomous ODE!")))
           (RETURN (AEVAL 'NIL)))))))
      (RETURN (AEVAL (LIST 'ODESOLVE-SIMP-ARBCONSTS SOLN Y X))))) 
(PUT 'ODESOLVE-SCALEINV 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SCALEINV) 'OPFN) 
(PUT 'ODESOLVE-SCALEINV 'DEFINED-ON-LINE '228) 
(PUT 'ODESOLVE-SCALEINV 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-SCALEINV 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SCALEINV (ODE Y X)
    (PROG (U P ODE1 POW *ALLFAC)
      (AEVAL
       (TRACEODE1
        (LIST "Testing whether ODE is scale invariant or "
              "equidimensional in the independent variable " X " ...")))
      (SETQ U (AEVAL (LIST 'GENSYM)))
      (SETQ P (AEVAL (LIST 'GENSYM)))
      (SETQ ODE1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'DF Y X (LIST '~ 'N))
                                 (LIST 'TIMES (LIST 'MKID U 'N)
                                       (LIST 'EXPT X (LIST 'DIFFERENCE P 'N))))
                           (LIST 'REPLACEBY (LIST 'DF Y X)
                                 (LIST 'TIMES (LIST 'MKID U 1)
                                       (LIST 'EXPT X (LIST 'DIFFERENCE P 1)))))
                     ODE)))
      (SETQ ODE1
              (AEVAL
               (LIST 'NUM
                     (LIST 'SUB
                           (LIST 'EQUAL Y (LIST 'TIMES U (LIST 'EXPT X P)))
                           ODE1))))
      (PROG (PART1 N_PARTS)
        (SETQ PART1 (AEVAL (LIST 'PART ODE1 1)))
        (SETQ N_PARTS (AEVAL (LIST 'ARGLENGTH ODE1)))
        (PROG (I)
          (SETQ I 2)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N_PARTS) I)) (RETURN NIL)))
          (PROGN
           (SETK 'PARTI (AEVAL* (LIST 'QUOTIENT (LIST 'PART ODE1 I) PART1)))
           (SETQ POW
                   (AEVAL*
                    (LIST 'TIMES (LIST 'DF 'PARTI X)
                          (LIST 'QUOTIENT X 'PARTI))))
           (COND
            ((BOOLVALUE* POW)
             (PROGN
              (SETQ POW (AEVAL* (LIST 'SOLVE POW P)))
              (SETQ N_PARTS (AEVAL* 0))))))
          (SETQ I
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   I))
          (GO LAB))
        (COND
         ((BOOLVALUE* N_PARTS)
          (RETURN (SETQ POW (AEVAL (LIST 'LIST (LIST 'EQUAL P 0)))))))
        (SETQ ODE1
                (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE ODE1 PART1) PART1)))
        (WHILE
         (AND (EVALNEQ (AEVAL* POW) (AEVAL* (LIST 'LIST)))
              (OR (BOOLVALUE* (REVALX (EQCAR (CADDR (CADR POW)) 'ROOT_OF)))
                  (NOT
                   (FREEOF (REVALX (LIST 'SUB (LIST 'FIRST POW) ODE1))
                           (REVALX X)))))
         (SETQ POW (AEVAL* (LIST 'REST POW)))))
      (COND
       ((EVALEQUAL (AEVAL POW) (AEVAL (LIST 'LIST))) (RETURN (AEVAL 'NIL))))
      (COND
       ((NOT (BOOLVALUE* (SETQ P (REVALX (LIST 'RHS (LIST 'FIRST POW))))))
        (RETURN (AEVAL (LIST 'ODESOLVE-SCALEINV-EQUIDIM-X ODE Y X)))))
      (DEPEND1 U X T)
      (SETQ ODE
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL Y (LIST 'TIMES (LIST 'EXPT X P) U))
                     ODE)))
      (AEVAL
       (TRACEODE
        (LIST "This ODE is scale invariant -- applying "
              (LIST 'REPLACEBY Y (LIST 'TIMES (LIST 'EXPT X P) U))
              " to transform to the simpler ODE: " (LIST 'EQUAL ODE 0))))
      (SETQ ODE (AEVAL (LIST 'ODESOLVE-SCALEINV-EQUIDIM-X ODE U X)))
      (DEPEND1 U X NIL)
      (COND
       ((BOOLVALUE* ODE)
        (RETURN
         (AEVAL
          (LIST 'SUB (LIST 'EQUAL U (LIST 'QUOTIENT Y (LIST 'EXPT X P)))
                ODE)))))
      (AEVAL
       (TRACEODE (LIST "Cannot solve transformed scale invariant ODE!"))))) 
(PUT 'ODESOLVE-SCALEINV-EQUIDIM-X 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SCALEINV-EQUIDIM-X) 'OPFN) 
(PUT 'ODESOLVE-SCALEINV-EQUIDIM-X 'DEFINED-ON-LINE '279) 
(PUT 'ODESOLVE-SCALEINV-EQUIDIM-X 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-SCALEINV-EQUIDIM-X 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SCALEINV-EQUIDIM-X (ODE Y X)
    (PROG (TT EXP_TT)
      (SETQ TT (AEVAL (LIST 'GENSYM)))
      (SETQ EXP_TT (AEVAL (LIST 'EXP TT)))
      (DEPEND1 Y TT T)
      (SETQ ODE
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'DF Y X)
                                 (LIST 'QUOTIENT (LIST 'DF Y TT) EXP_TT))
                           (LIST 'REPLACEBY (LIST 'DF Y X (LIST '~ 'N))
                                 (LIST 'WHEN
                                       (LIST 'QUOTIENT
                                             (LIST 'DF
                                                   (LIST 'DF Y X
                                                         (LIST 'DIFFERENCE 'N
                                                               1))
                                                   TT)
                                             EXP_TT)
                                       (LIST 'AND (LIST 'NUMBERP 'N)
                                             (LIST 'GREATERP 'N 0)))))
                     ODE)))
      (SETQ ODE (AEVAL (LIST 'NUM (LIST 'SUB (LIST 'EQUAL X EXP_TT) ODE))))
      (AEVAL
       (TRACEODE
        (LIST "This ODE is equidimensional in the independent variable " X
              " -- applying " (LIST 'REPLACEBY X EXP_TT)
              " to transform to the simpler ODE: " (LIST 'EQUAL ODE 0))))
      (DEPEND1 Y X NIL)
      (SETQ ODE (AEVAL (ODESOLVE-AUTONOMOUS ODE Y TT)))
      (DEPEND1 Y X T)
      (DEPEND1 Y TT NIL)
      (COND
       ((BOOLVALUE* ODE)
        (RETURN (AEVAL (LIST 'SUB (LIST 'EQUAL TT (LIST 'LOG X)) ODE)))))
      (AEVAL
       (TRACEODE (LIST "Cannot solve transformed equidimensional ODE!"))))) 
(PUT 'ODESOLVE-EQUIDIM-Y 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-EQUIDIM-Y) 'OPFN) 
(PUT 'ODESOLVE-EQUIDIM-Y 'DEFINED-ON-LINE '311) 
(PUT 'ODESOLVE-EQUIDIM-Y 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-EQUIDIM-Y 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-EQUIDIM-Y (ODE Y X)
    (PROG (ODE1 U EXP_U)
      (AEVAL
       (TRACEODE1
        (LIST "Testing whether ODE is equidimensional in "
              "the dependent variable " Y " ...")))
      (SETQ U (AEVAL (LIST 'GENSYM)))
      (SETQ ODE1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'REPLACEBY (LIST 'DF Y X (LIST '~ 'N))
                                 (LIST 'TIMES Y (LIST 'MKID U 'N)))
                           (LIST 'REPLACEBY (LIST 'DF Y X) (LIST 'TIMES Y U)))
                     ODE)))
      (COND
       ((OR (BOOLVALUE* (REVALX (LIST 'REDUCT ODE1 Y)))
            (BOOLVALUE* (REVALX (LIST 'DEPENDS (LIST 'LCOF ODE1 Y) Y))))
        (RETURN (AEVAL 'NIL))))
      (SETQ EXP_U (AEVAL (LIST 'EXP U)))
      (DEPEND1 U X T)
      (SETQ ODE
              (AEVAL
               (LIST 'LCOF (LIST 'NUM (LIST 'SUB (LIST 'EQUAL Y EXP_U) ODE))
                     EXP_U)))
      (AEVAL
       (TRACEODE
        (LIST "This ODE is equidimensional in the dependent variable " Y
              " -- applying " (LIST 'REPLACEBY Y EXP_U)
              " to transform to the simpler ODE: " (LIST 'EQUAL ODE 0))))
      (SETQ ODE (AEVAL (LIST 'ODESOLVE*1 ODE U X)))
      (DEPEND1 U X NIL)
      (COND
       ((NOT (BOOLVALUE* ODE))
        (PROGN
         (AEVAL
          (TRACEODE (LIST "Cannot solve transformed equidimensional ODE!")))
         (RETURN (AEVAL 'NIL)))))
      (RETURN
       (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
         (SETQ SOLN (GETRLIST (AEVAL ODE)))
         (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (SOLN)
                             (COND
                              ((EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL U))
                               (AEVAL
                                (LIST 'EQUAL Y (LIST 'EXP (LIST 'RHS SOLN)))))
                              (T
                               (AEVAL
                                (LIST 'SUB (LIST 'EQUAL U (LIST 'LOG Y))
                                      ODE)))))
                           (CAR SOLN))
                          NIL)))
        LOOPLABEL
         (SETQ SOLN (CDR SOLN))
         (COND ((NULL SOLN) (RETURN (CONS 'LIST FORALL-RESULT))))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (SOLN)
                     (COND
                      ((EVALEQUAL (AEVAL (LIST 'LHS SOLN)) (AEVAL U))
                       (AEVAL (LIST 'EQUAL Y (LIST 'EXP (LIST 'RHS SOLN)))))
                      (T
                       (AEVAL (LIST 'SUB (LIST 'EQUAL U (LIST 'LOG Y)) ODE)))))
                   (CAR SOLN))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SIMP-ARBCONSTS) 'OPFN) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS 'DEFINED-ON-LINE '357) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SIMP-ARBCONSTS (SOLNS Y X)
    (PROG (SOLN FORALL-RESULT FORALL-ENDPTR)
      (SETQ SOLN (GETRLIST (AEVAL SOLNS)))
      (COND ((NULL SOLN) (RETURN (MAKELIST NIL))))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (SOLN)
                          (COND
                           ((BOOLVALUE* (REVALX (EQCAR SOLN 'LIST)))
                            (AEVAL SOLN))
                           (T
                            (AEVAL
                             (LIST 'EQUAL
                                   (LIST 'ODESOLVE-SIMP-ARBCONSTS1
                                         (LIST 'LHS SOLN) Y X)
                                   (LIST 'ODESOLVE-SIMP-ARBCONSTS1
                                         (LIST 'RHS SOLN) Y X))))))
                        (CAR SOLN))
                       NIL)))
     LOOPLABEL
      (SETQ SOLN (CDR SOLN))
      (COND ((NULL SOLN) (RETURN (CONS 'LIST FORALL-RESULT))))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (SOLN)
                  (COND ((BOOLVALUE* (REVALX (EQCAR SOLN 'LIST))) (AEVAL SOLN))
                        (T
                         (AEVAL
                          (LIST 'EQUAL
                                (LIST 'ODESOLVE-SIMP-ARBCONSTS1
                                      (LIST 'LHS SOLN) Y X)
                                (LIST 'ODESOLVE-SIMP-ARBCONSTS1
                                      (LIST 'RHS SOLN) Y X))))))
                (CAR SOLN))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS1 'NUMBER-OF-ARGS 3) 
(FLAG '(ODESOLVE-SIMP-ARBCONSTS1) 'OPFN) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS1 'DEFINED-ON-LINE '365) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS1 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-SIMP-ARBCONSTS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-SIMP-ARBCONSTS1 (SOLN Y X)
    (PROG (*PRECISE SS ACEXPRNS)
      (COND
       ((NOT
         (BOOLVALUE*
          (SETQ SS (REVALX (LIST 'ODESOLVE-STRUCTR SOLN X Y 'ARBCONST)))))
        (RETURN (AEVAL SOLN))))
      (SETQ ACEXPRNS (AEVAL (LIST 'REST SS)))
      (SETQ SS (AEVAL (LIST 'FIRST SS)))
      (AEVAL
       (TRACEODE
        (LIST "Simplifying the arbconst expressions in " SOLN
              " by the rewrites ...")))
      (PROG (S)
        (SETQ S (GETRLIST (AEVAL ACEXPRNS)))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (AEVAL
             (LIST 'WHEREEXP
                   (LIST 'LIST
                         (LIST 'REPLACEBY (LIST 'ARBCONST (LIST '~ 'N))
                               (LIST 'SETK ''N* (LIST 'AEVAL ''N))))
                   (LIST 'RHS S)))
            (AEVAL (TRACEODE (LIST (LIST 'RHS S) " => " (LIST 'ARBCONST 'N*))))
            (SETQ SS
                    (AEVAL
                     (LIST 'SUB (LIST 'SOLVE S (LIST 'ARBCONST 'N*)) SS)))
            (SETQ SS
                    (AEVAL
                     (LIST 'SUB
                           (LIST 'EQUAL (LIST 'LHS S) (LIST 'ARBCONST 'N*))
                           SS)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RETURN (AEVAL SS)))) 
(FLAG '(ODESOLVE-STRUCTR) 'OPFN) 
(PUT 'ODESOLVE-STRUCTR 'NUMBER-OF-ARGS 4) 
(PUT 'ODESOLVE-STRUCTR 'DEFINED-ON-LINE '428) 
(PUT 'ODESOLVE-STRUCTR 'DEFINED-IN-FILE 'ODESOLVE/ODENONN.RED) 
(PUT 'ODESOLVE-STRUCTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODESOLVE-STRUCTR (U X Y ARBOP)
    (PROG (*SAVESTRUCTR *PRECISE SS ARBEXPRNS)
      (SETQ *SAVESTRUCTR T)
      (SETQ SS (CDR (STRUCTR (LIST U))))
      (COND ((NULL (CDR SS)) (RETURN NIL)))
      (SETQ SS
              (CONS (CAR SS)
                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                      (SETQ S (CDR SS))
                     STARTOVER
                      (COND ((NULL S) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (S)
                                 (COND
                                  ((EQCAR (CADDR (SETQ S (REVAL1 S T))) ARBOP)
                                   (PROGN
                                    (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                    NIL))
                                  (T (LIST S))))
                               (CAR S)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ S (CDR S))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (S)
                                 (COND
                                  ((EQCAR (CADDR (SETQ S (REVAL1 S T))) ARBOP)
                                   (PROGN
                                    (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                    NIL))
                                  (T (LIST S))))
                               (CAR S)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ S (CDR S))
                      (GO LOOPLABEL))))
      (COND ((NULL (CDR SS)) (RETURN NIL)))
      (COND
       (ARBEXPRNS
        (SETQ SS (CDR (SUBEVAL (NCONC ARBEXPRNS (LIST (CONS 'LIST SS))))))))
      (SETQ ARBEXPRNS NIL)
      (SETQ SS
              (CONS (CAR SS)
                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                      (SETQ S (CDR SS))
                     STARTOVER
                      (COND ((NULL S) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (S)
                                 (COND
                                  ((NOT (SMEMBER ARBOP S))
                                   (PROGN
                                    (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                    NIL))
                                  (T (LIST S))))
                               (CAR S)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ S (CDR S))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (S)
                                 (COND
                                  ((NOT (SMEMBER ARBOP S))
                                   (PROGN
                                    (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                    NIL))
                                  (T (LIST S))))
                               (CAR S)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ S (CDR S))
                      (GO LOOPLABEL))))
      (COND ((NULL (CDR SS)) (RETURN NIL)))
      (COND
       (ARBEXPRNS
        (SETQ SS (CDR (SUBEVAL (NCONC ARBEXPRNS (LIST (CONS 'LIST SS))))))))
      (SETQ ARBEXPRNS T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ARBEXPRNS (CDR SS))) (RETURN NIL)))
        (PROGN
         (SETQ ARBEXPRNS NIL)
         (SETQ SS
                 (CONS (CAR SS)
                       (PROG (S FORALL-RESULT FORALL-ENDPTR)
                         (SETQ S (CDR SS))
                        STARTOVER
                         (COND ((NULL S) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 ((LAMBDA (S)
                                    (COND
                                     ((OR (DEPENDS S X) (DEPENDS S Y))
                                      (PROGN
                                       (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                       NIL))
                                     (T (LIST S))))
                                  (CAR S)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                         (SETQ S (CDR S))
                         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                        LOOPLABEL
                         (COND ((NULL S) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 ((LAMBDA (S)
                                    (COND
                                     ((OR (DEPENDS S X) (DEPENDS S Y))
                                      (PROGN
                                       (SETQ ARBEXPRNS (CONS S ARBEXPRNS))
                                       NIL))
                                     (T (LIST S))))
                                  (CAR S)))
                         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                         (SETQ S (CDR S))
                         (GO LOOPLABEL))))
         (COND
          (ARBEXPRNS
           (SETQ SS
                   (CDR (SUBEVAL (NCONC ARBEXPRNS (LIST (CONS 'LIST SS)))))))))
        (GO WHILELABEL))
      (COND ((NULL (CDR SS)) (RETURN NIL)))
      (RETURN (CONS 'LIST SS)))) 
(ENDMODULE) 