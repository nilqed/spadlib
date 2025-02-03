(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RUNGEKU)) 
(FLUID '(*NOEQUIV ACCURACY*)) 
(GLOBAL '(ITERATIONS* *TRNUMERIC)) 
(PUT 'NUM_ODESOLVE 'PSOPFN 'RUNGEKUTTAEVAL) 
(PUT 'RUNGEKUTTAEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RUNGEKUTTAEVAL 'DEFINED-ON-LINE '38) 
(PUT 'RUNGEKUTTAEVAL 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUNGEKUTTAEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RUNGEKUTTAEVAL (U)
    (PROG (E F X Y SX SY EN D V W Q Z)
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (ACCURACYCONTROL U 20 6))
      (SETQ E (CAR U))
      (SETQ U (CDR U))
      (SETQ E (COND ((EQCAR E 'LIST) (CDR E)) (T (LIST E))))
      (SETQ Q (CAR U))
      (SETQ U (CDR U))
      (SETQ Q (COND ((EQCAR Q 'LIST) (CDR Q)) (T (LIST Q))))
      (PROG (YY)
        (SETQ YY Q)
       LAB
        (COND ((NULL YY) (RETURN NIL)))
        ((LAMBDA (YY)
           (PROGN
            (COND
             ((OR (NOT (EQCAR YY 'EQUAL)) (NOT (IDP (CADR YY))))
              (TYPERR YY "expression `dep. variable=starting value'")))
            (SETQ SY (CONS (CADDR YY) SY))
            (SETQ Y (CONS (CADR YY) Y))
            NIL))
         (CAR YY))
        (SETQ YY (CDR YY))
        (GO LAB))
      (SETQ SY (REVERSIP SY))
      (SETQ Y (REVERSIP Y))
      (COND
       ((NEQ (LENGTH SY) (LENGTH E))
        (REDERR "number of equations and variables don't correspond")))
      (SETQ X (CAR U))
      (SETQ U (CDR U))
      (COND
       ((OR (NOT (EQCAR X 'EQUAL)) (NOT (IDP (CADR X))))
        (TYPERR X "expression `indep. variable=interval'")))
      (COND ((NOT *ROUNDED) (SETDMODE 'ROUNDED (SETQ Z (SETQ *ROUNDED T)))))
      (SETQ W (REVALNUMINTERVAL (CADDR X) T))
      (COND (Z (SETDMODE 'ROUNDED (SETQ Z (SETQ *ROUNDED NIL)))))
      (COND ((NULL W) (TYPERR X "expression `indep. variable=interval'")))
      (SETQ SX (CAR W))
      (SETQ EN (CADR W))
      (SETQ X (CADR X))
      (SETQ Q
              (PROG (YY FORALL-RESULT FORALL-ENDPTR)
                (SETQ YY Y)
                (COND ((NULL YY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (YY) (LIST 'DF YY X)) (CAR YY))
                                      NIL)))
               LOOPLABEL
                (SETQ YY (CDR YY))
                (COND ((NULL YY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (YY) (LIST 'DF YY X)) (CAR YY)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V (CDR (SOLVEEVAL (LIST (CONS 'LIST E) (CONS 'LIST Q)))))
      (COND ((CDR V) (RUKSYSERR NIL "cannot convert to explicit ODE system")))
      (SETQ F
              (PROG (D FORALL-RESULT FORALL-ENDPTR)
                (SETQ D Q)
                (COND ((NULL D) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (D) (RUKUFINDRHS D V)) (CAR D))
                                      NIL)))
               LOOPLABEL
                (SETQ D (CDR D))
                (COND ((NULL D) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (D) (RUKUFINDRHS D V)) (CAR D)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (RUNGEKUTTA1 F X Y SX EN SY)))) 
(PUT 'RUKUFINDRHS 'NUMBER-OF-ARGS 2) 
(PUT 'RUKUFINDRHS 'DEFINED-ON-LINE '78) 
(PUT 'RUKUFINDRHS 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUKUFINDRHS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RUKUFINDRHS (D E)
    (COND ((ATOM E) (RUKSYSERR D " cannot be moved to lhs of system"))
          ((AND (EQCAR (CAR E) 'EQUAL) (EQUAL (CADR (CAR E)) D))
           (REVAL1 (CADDR (CAR E)) T))
          ((AND (PAIRP E) (EQCAR (CAR E) 'LIST)) (RUKUFINDRHS D (CDAR E)))
          (T (RUKUFINDRHS D (CDR E))))) 
(PUT 'RUKSYSERR 'NUMBER-OF-ARGS 2) 
(PUT 'RUKSYSERR 'DEFINED-ON-LINE '87) 
(PUT 'RUKSYSERR 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUKSYSERR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RUKSYSERR (U M)
    (PROGN
     (COND (U (WRITEPRI (MKQUOTE U) 'FIRST)))
     (WRITEPRI (MKQUOTE M) 'LAST)
     (REDERR "Runge-Kutta failed")
     NIL)) 
(PUT 'RUNGEKUTTA1 'NUMBER-OF-ARGS 6) 
(PUT 'RUNGEKUTTA1 'DEFINED-ON-LINE '93) 
(PUT 'RUNGEKUTTA1 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUNGEKUTTA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RUNGEKUTTA1 (F X Y SX EX SY)
    ((LAMBDA (*ROUNDBF)
       (PROG (ACC R OLDMODE *NOEQUIV PREC)
         (SETQ PREC 0)
         (COND
          ((NOT (MEMQ DMODE* '(|:RD:| |:CR|)))
           (PROGN (SETQ OLDMODE T) (SETDMODE 'ROUNDED T))))
         (SETQ *NOEQUIV T)
         (SETQ PREC (PRECISION 0))
         (SETQ SX (FORCE-TO-DM (CAR (SIMP SX))))
         (SETQ EX (FORCE-TO-DM (CAR (SIMP EX))))
         (SETQ SY
                 (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Z SY)
                   (COND ((NULL Z) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Z) (FORCE-TO-DM (CAR (SIMP Z))))
                                     (CAR Z))
                                    NIL)))
                  LOOPLABEL
                   (SETQ Z (CDR Z))
                   (COND ((NULL Z) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (Z) (FORCE-TO-DM (CAR (SIMP Z)))) (CAR Z))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ ACC (|::QUOTIENT| 1 (EXPT 10 ACCURACY*)))
         (COND (*TRNUMERIC (PRIN2T "starting rungekutta iteration")))
         (SETQ R (RUNGEKUTTA2 F X Y SX EX SY ACC))
         (COND (OLDMODE (SETDMODE 'ROUNDED NIL)))
         (PRECISION PREC)
         (COND ((NULL R) (REDERR "no solution found")))
         (RETURN (CONS 'LIST R))))
     *ROUNDBF)) 
(PUT 'RUNGEKUTTA2 'NUMBER-OF-ARGS 7) 
(PUT 'RUNGEKUTTA2 'DEFINED-ON-LINE '112) 
(PUT 'RUNGEKUTTA2 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUNGEKUTTA2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE RUNGEKUTTA2 (F XX YY XS XE YS ACC)
    (PROG (H H2 H4 X Y R W1 W2 VARS DIR COUNT ST)
      (SETQ COUNT 0)
      (SETQ ST 0)
      (SETQ VARS (CONS XX YY))
      (SETQ X XS)
      (SETQ Y YS)
      (SETQ H (|::QUOTIENT| (|:DIFFERENCE| XE XS) ITERATIONS*))
      (SETQ DIR ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) H 0))
      (SETQ ST ITERATIONS*)
     ADAPT
      (SETQ H2 (|::QUOTIENT| H 2))
      (SETQ H4 (|::QUOTIENT| H 4))
      (SETQ W1 (RUNGEKUTTASTEP F VARS X Y H H2))
      (SETQ W2 (RUNGEKUTTASTEP F VARS X Y H2 H4))
      (SETQ W2 (RUNGEKUTTASTEP F VARS (|:PLUSN| X H2) W2 H2 H4))
      (COND
       (((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A)))
         (NORMLIST (LIST-LIST W2 W1)) ACC)
        (PROGN (SETQ H H2) (SETQ ST (|:PLUSN| ST ST)) (GO ADAPT))))
      (COND
       ((AND *TRNUMERIC (NOT (EQUAL ST ITERATIONS*)))
        (PROGN
         (PRIN2 "*** RungeKutta increased number of steps to ")
         (PRIN2T ST))))
     LOOP
      (COND
       ((OR (AND DIR ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| B A))) X XE))
            (AND (NOT DIR)
                 ((LAMBDA (A B) (|:MINUSP| (|:DIFFERENCE| A B))) X XE)))
        (GO FINIS)))
      (SETQ R (CONS (CONS 'LIST (CONS X Y)) R))
      (SETQ COUNT (ADD1 COUNT))
      (SETQ Y (RUNGEKUTTASTEP F VARS X Y H H2))
      (SETQ X (|:PLUSN| X H))
      (GO LOOP)
     FINIS
      (SETQ R
              (CONS (CONS 'LIST (CONS XX YY))
                    (CONS (CONS 'LIST (CONS XS YS))
                          (RUNGEKUTTARES (REVERSIP R) ST))))
      (RETURN R))) 
(PUT 'RUNGEKUTTARES 'NUMBER-OF-ARGS 2) 
(PUT 'RUNGEKUTTARES 'DEFINED-ON-LINE '156) 
(PUT 'RUNGEKUTTARES 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUNGEKUTTARES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RUNGEKUTTARES (L ST)
    (COND ((EQUAL ST ITERATIONS*) (CDR L))
          (T
           ((LAMBDA (M)
              (PROGN
               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                 (SETQ I 1)
                 (COND ((MINUSP (DIFFERENCE ITERATIONS* I)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  (PROGN
                                   (PROG (J)
                                     (SETQ J 1)
                                    LAB
                                     (COND
                                      ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                                     (SETQ L (CDR L))
                                     (SETQ J (PLUS2 J 1))
                                     (GO LAB))
                                   (CAR L))
                                  NIL)))
                LOOPLABEL
                 (SETQ I (PLUS2 I 1))
                 (COND
                  ((MINUSP (DIFFERENCE ITERATIONS* I)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          (PROGN
                           (PROG (J)
                             (SETQ J 1)
                            LAB
                             (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                             (SETQ L (CDR L))
                             (SETQ J (PLUS2 J 1))
                             (GO LAB))
                           (CAR L))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
            (QUOTIENT ST ITERATIONS*))))) 
(PUT 'RUNGEKUTTASTEP 'NUMBER-OF-ARGS 6) 
(PUT 'RUNGEKUTTASTEP 'DEFINED-ON-LINE '165) 
(PUT 'RUNGEKUTTASTEP 'DEFINED-IN-FILE 'NUMERIC/RUNGEKU.RED) 
(PUT 'RUNGEKUTTASTEP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RUNGEKUTTASTEP (F VARS X Y H H2)
    (PROG (D1 D2 D3 D4)
      (SETQ D1 (LIST-EVALUATE F VARS (CONS X Y)))
      (SETQ D2
              (LIST-EVALUATE F VARS
               (CONS (|:PLUSN| X H2) (LIST+LIST Y (SCAL*LIST H2 D1)))))
      (SETQ D3
              (LIST-EVALUATE F VARS
               (CONS (|:PLUSN| X H2) (LIST+LIST Y (SCAL*LIST H2 D2)))))
      (SETQ D4
              (LIST-EVALUATE F VARS
               (CONS (|:PLUSN| X H) (LIST+LIST Y (SCAL*LIST H D3)))))
      (SETQ Y
              (LIST+LIST Y
               (SCAL*LIST (|::QUOTIENT| H 6)
                (LIST+LIST D1
                 (LIST+LIST (SCAL*LIST 2 D2)
                  (LIST+LIST (SCAL*LIST 2 D3) D4))))))
      (RETURN Y))) 
(ENDMODULE) 