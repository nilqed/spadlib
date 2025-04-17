(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GENUS)) 
(FLUID
 '(*GALOIS *TRA *TRMIN *INSIDE-INT* GAUSSIANI INTVAR LISTOFALLSQRTS
   LISTOFNEWSQRTS NESTEDSQRTS PREVIOUSBASIS SQRT-INTVAR SQRT-PLACES-ALIST
   SQRTFLAG SQRTS-IN-INTEGRAND TAYLORASSLIST TAYLORVARIABLE)) 
(PUT 'SIMPGENUS 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGENUS 'DEFINED-ON-LINE '47) 
(PUT 'SIMPGENUS 'DEFINED-IN-FILE 'ALGINT/GENUS.RED) 
(PUT 'SIMPGENUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGENUS (U)
    (PROG (INTVAR SQRT-INTVAR TAYLORVARIABLE TAYLORASSLIST LISTOFNEWSQRTS
           LISTOFALLSQRTS SQRT-PLACES-ALIST SQRTFLAG SQRTS-IN-INTEGRAND TT
           *INSIDE-INT*)
      (SETQ TT (TIME))
      (SETQ SQRTFLAG T)
      (SETQ TAYLORVARIABLE (SETQ INTVAR (CAR U)))
      (SETQ *INSIDE-INT* T)
      (SETQ SQRT-INTVAR (CAAAR (*Q2F (SIMPSQRTI INTVAR))))
      (SETQ LISTOFNEWSQRTS (LIST (CAAAR GAUSSIANI)))
      (SETQ LISTOFALLSQRTS (LIST (CONS (CADR (CAAAR GAUSSIANI)) GAUSSIANI)))
      (SETQ U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V (CDR U))
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (SIMP* V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (SIMP* V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SQRTS-IN-INTEGRAND (SQRTSINSQL U INTVAR))
      (SETQ U (*N2SQ (LENGTH (DIFFERENTIALS-1 SQRTS-IN-INTEGRAND))))
      ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
       (LIST 'TIME 'TAKEN (DIFFERENCE (TIME) TT) 'MILLISECONDS))
      (RETURN U))) 
(PUT 'GENUS 'SIMPFN 'SIMPGENUS) 
(PUT '*N2SQ 'NUMBER-OF-ARGS 1) 
(PUT '*N2SQ 'DEFINED-ON-LINE '69) 
(PUT '*N2SQ 'DEFINED-IN-FILE 'ALGINT/GENUS.RED) 
(PUT '*N2SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *N2SQ (U1) (COND ((EQUAL U1 0) (CONS NIL 1)) (T (CONS U1 1)))) 
(PUT 'DIFFERENTIALS-1 'NUMBER-OF-ARGS 1) 
(PUT 'DIFFERENTIALS-1 'DEFINED-ON-LINE '72) 
(PUT 'DIFFERENTIALS-1 'DEFINED-IN-FILE 'ALGINT/GENUS.RED) 
(PUT 'DIFFERENTIALS-1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFFERENTIALS-1 (SQRTL)
    (PROG (ASQRTL FACLIST PLACES V NESTEDSQRTS BASIS U N HARD-ONES
           SQRTS-IN-PROBLEM)
      (SETQ ASQRTL
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U SQRTL)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (*Q2F (SIMP (CADR U)))) (CAR U))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN
          (PRIN2
           "Find the differentials of the first kind on curve defined by:")
          (TERPRI)
          "Find the differentials of the first kind on curve defined by:")
         (MAPC ASQRTL (FUNCTION PRINTSF)))))
      (PROG (S)
        (SETQ S ASQRTL)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ FACLIST
                    (PROG (U FORALL-RESULT FORALL-ENDPTR)
                      (SETQ U (JFACTOR S INTVAR))
                      (COND ((NULL U) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (U) (CAR U)) (CAR U))
                                            NIL)))
                     LOOPLABEL
                      (SETQ U (CDR U))
                      (COND ((NULL U) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (U) (CAR U)) (CAR U)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             (*TRA
              (PROGN
               (PRINC INTVAR)
               (PROGN
                (PRIN2 " is not a local variable at the roots of:")
                (TERPRI)
                " is not a local variable at the roots of:")
               (MAPC FACLIST (FUNCTION PRINTSF)))))
            (PROG (UU)
              (SETQ UU FACLIST)
             LAB
              (COND ((NULL UU) (RETURN NIL)))
              ((LAMBDA (UU)
                 (PROGN
                  (SETQ V (STT UU INTVAR))
                  (COND ((NEQ 1 (CAR V)) (SETQ HARD-ONES (CONS UU HARD-ONES)))
                        (T
                         (PROGN
                          (SETQ U
                                  (CONS
                                   (ADDF UU
                                         (CONS
                                          (CONS (GETPOWER (FKERN INTVAR) 1)
                                                (NEGF (CDR V)))
                                          NIL))
                                   (CDR V)))
                          (SETQ U
                                  (LIST
                                   (LIST INTVAR 'DIFFERENCE INTVAR (PREPSQ U))
                                   (LIST INTVAR 'EXPT INTVAR 2)))
                          (PROG (W)
                            (SETQ W
                                    (SQRTSIGN
                                     (PROG (W FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ W
                                               (UNION (DELETE S ASQRTL)
                                                      (DELETE UU FACLIST)))
                                      STARTOVER
                                       (COND ((NULL W) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               ((LAMBDA (W)
                                                  (SQRTSINSQ
                                                   (SIMPSQRTSQ
                                                    (MULTSQ
                                                     (SUBSTITUTESQ (CONS W 1)
                                                      U)
                                                     (CONS 1
                                                           (LIST
                                                            (CONS
                                                             (GETPOWER
                                                              (FKERN INTVAR) 2)
                                                             1)))))
                                                   INTVAR))
                                                (CAR W)))
                                       (SETQ FORALL-ENDPTR
                                               (LASTPAIR FORALL-RESULT))
                                       (SETQ W (CDR W))
                                       (COND
                                        ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                      LOOPLABEL
                                       (COND ((NULL W) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               ((LAMBDA (W)
                                                  (SQRTSINSQ
                                                   (SIMPSQRTSQ
                                                    (MULTSQ
                                                     (SUBSTITUTESQ (CONS W 1)
                                                      U)
                                                     (CONS 1
                                                           (LIST
                                                            (CONS
                                                             (GETPOWER
                                                              (FKERN INTVAR) 2)
                                                             1)))))
                                                   INTVAR))
                                                (CAR W)))
                                       (SETQ FORALL-ENDPTR
                                               (LASTPAIR FORALL-ENDPTR))
                                       (SETQ W (CDR W))
                                       (GO LOOPLABEL))
                                     INTVAR))
                           LAB
                            (COND ((NULL W) (RETURN NIL)))
                            ((LAMBDA (W)
                               (SETQ PLACES (CONS (APPEND U W) PLACES)))
                             (CAR W))
                            (SETQ W (CDR W))
                            (GO LAB)))))))
               (CAR UU))
              (SETQ UU (CDR UU))
              (GO LAB))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ SQRTS-IN-PROBLEM
              (NCONC
               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                 (SETQ U HARD-ONES)
                 (COND ((NULL U) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (U)
                                     (LIST (CONS INTVAR INTVAR)
                                           ((LAMBDA (U) (CONS U U))
                                            (LIST 'SQRT (PREPF U)))))
                                   (CAR U))
                                  NIL)))
                LOOPLABEL
                 (SETQ U (CDR U))
                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (U)
                             (LIST (CONS INTVAR INTVAR)
                                   ((LAMBDA (U) (CONS U U))
                                    (LIST 'SQRT (PREPF U)))))
                           (CAR U))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               PLACES))
      (SETQ BASIS (MAKEINITIALBASIS SQRTS-IN-PROBLEM))
      (SETQ BASIS
              (INTEGRALBASIS (MKVEC BASIS) PLACES (MKILIST PLACES (MINUS 1))
               INTVAR))
      (COND
       ((NOT *GALOIS)
        (SETQ BASIS
                (COMBINE-SQRTS BASIS (GETSQRTSFROMPLACES SQRTS-IN-PROBLEM)))))
      (COND
       (HARD-ONES
        (PROGN
         (SETQ V (UPBV BASIS))
         (SETQ U 1)
         (PROG (V)
           (SETQ V HARD-ONES)
          LAB
           (COND ((NULL V) (RETURN NIL)))
           ((LAMBDA (V)
              (SETQ U
                      ((LAMBDA (G551)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF U G551))
                               (T (POLY-MULTF U G551))))
                       (LIST
                        (CONS (GETPOWER (FKERN (LIST 'SQRT (PREPF V))) 1)
                              1)))))
            (CAR V))
           (SETQ V (CDR V))
           (GO LAB))
         (SETQ HARD-ONES (CONS 1 U))
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((MINUSP (DIFFERENCE V I)) (RETURN NIL)))
           (PUTV BASIS I (MULTSQ (GETV BASIS I) HARD-ONES))
           (SETQ I (PLUS2 I 1))
           (GO LAB)))))
      (COND ((NOT *GALOIS) (SETQ BASIS (MODIFY-SQRTS BASIS SQRTL))))
      (SETQ V (FRACTIONAL-DEGREE-AT-INFINITY SQRTL))
      (COND ((IEQUAL V 1) (SETQ N 2)) (T (SETQ N (DIFFERENCE (TIMES 2 V) 1))))
      (SETQ BASIS (NORMALBASIS BASIS INTVAR N))
      (SETQ PREVIOUSBASIS NIL)
      (COND
       ((OR *TRA *TRMIN)
        (PROGN
         (PROGN (PRIN2 "Differentials are:") (TERPRI) "Differentials are:")
         (MAPC BASIS (FUNCTION PRINTSQ)))))
      (RETURN BASIS))) 
(ENDMODULE) 