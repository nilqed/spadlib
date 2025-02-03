(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PF)) 
(FLUID '(*EXP *LIMITEDFACTORS *GCD KORD*)) 
(FLAG '(PF) 'OPFN) 
(FLAG '(PF) 'NOVAL) 
(PUT 'PF 'NUMBER-OF-ARGS 2) 
(PUT 'PF 'DEFINED-ON-LINE '43) 
(PUT 'PF 'DEFINED-IN-FILE 'MISC/PF.RED) 
(PUT 'PF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PF (U VAR)
    (PROG (*EXP *GCD KORD* *LIMITEDFACTORS POLYPART RFACTOR U1 U2 U3 U4 X XX Y)
      (SETQ *EXP (SETQ *GCD T))
      (SETQ VAR (*A2KWOWEIGHT VAR))
      (SETQ XX (UPDKORDER VAR))
      (SETQ X (SUBS2 (RESIMP (SIMP* U))))
      (SETQ U1 (CDR X))
      (COND
       ((EQUAL (DEGR U1 VAR) 0)
        (PROGN (SETKORDER XX) (RETURN (LIST 'LIST U)))))
      (SETQ U2 (QREMSQ (CONS (CAR X) 1) (CONS U1 1) VAR))
      (COND ((CAAR U2) (SETQ POLYPART (CAR U2))))
      (SETQ RFACTOR (CONS 1 1))
      (SETQ U2 (CDR U2))
      (SETQ U3 (FCTRF U1))
      (SETQ X (CDR U3))
      (SETQ U3 (CAR U3))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM U3) (ATOM (CAR U3))))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQ (CAAAR U3) VAR)
           (SETQ X
                   (CONS (CONS (LIST (CONS (CONS (CAAAR U3) 1) 1)) (CDAAR U3))
                         X)))
          (T
           (PROGN
            (SETQ U4 (LIST (CONS (CAAR U3) 1)))
            (SETQ RFACTOR
                    (CONS (CAR RFACTOR)
                          (COND
                           (*PHYSOP-LOADED (PHYSOP-MULTF U4 (CDR RFACTOR)))
                           (T (POLY-MULTF U4 (CDR RFACTOR))))))
            (SETQ U1 ((LAMBDA (*EXP) (QUOTF1 U1 U4)) T)))))
         (SETQ U3 (CDAR U3)))
        (GO WHILELABEL))
      (COND
       ((NEQ U3 1)
        (PROGN
         (SETQ RFACTOR
                 (CONS (CAR RFACTOR)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF U3 (CDR RFACTOR)))
                             (T (POLY-MULTF U3 (CDR RFACTOR))))))
         (SETQ U1 ((LAMBDA (*EXP) (QUOTF1 U1 U3)) T)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (LENGTH X) 1)) (RETURN NIL)))
        (PROGN
         (SETQ U3 (EXPTF (CAAR X) (CDAR X)))
         (SETQ U1 ((LAMBDA (*EXP) (QUOTF1 U1 U3)) T))
         (COND
          ((EQUAL (DEGR U3 VAR) 0)
           (SETQ RFACTOR
                   (CONS (CAR RFACTOR)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF U3 (CDR RFACTOR)))
                               (T (POLY-MULTF U3 (CDR RFACTOR)))))))
          (T
           (PROGN
            (SETQ U4 (XEUCL U1 U3 VAR))
            (SETQ Y
                    (CONS
                     (CONS
                      (MULTSQ (REMSQ (MULTSQ (CAR U4) U2) (CONS U3 1) VAR)
                              RFACTOR)
                      (CAR X))
                     Y))
            (SETQ U2 (MULTSQ (CDR U4) U2)))))
         (SETQ X (CDR X)))
        (GO WHILELABEL))
      (SETQ U3 (EXPTF (CAAR X) (CDAR X)))
      (COND ((EQUAL U2 (CONS NIL 1)) NIL)
            ((EQUAL (DEGR U3 VAR) 0)
             (SETQ RFACTOR
                     (CONS (CAR RFACTOR)
                           (COND
                            (*PHYSOP-LOADED (PHYSOP-MULTF U3 (CDR RFACTOR)))
                            (T (POLY-MULTF U3 (CDR RFACTOR)))))))
            (T
             (SETQ Y
                     (CONS
                      (CONS (MULTSQ RFACTOR (REMSQ U2 (CONS U3 1) VAR))
                            (CAR X))
                      Y))))
      (SETQ X NIL)
      (PROG (J)
        (SETQ J Y)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND ((EQUAL (CDDR J) 1) (SETQ X (CONS J X)))
                 (T
                  (SETQ X
                          (APPEND (PFPOWER (CAR J) (CADR J) (CDDR J) VAR)
                                  X)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (LIST 'QUOTIENT (PREPSQ* (CAR J))
                                          (COND
                                           ((EQUAL (CDDR J) 1)
                                            (PREPF (CADR J)))
                                           (T
                                            (LIST 'EXPT (PREPF (CADR J))
                                                  (CDDR J))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (LIST 'QUOTIENT (PREPSQ* (CAR J))
                                  (COND ((EQUAL (CDDR J) 1) (PREPF (CADR J)))
                                        (T
                                         (LIST 'EXPT (PREPF (CADR J))
                                               (CDDR J))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (POLYPART (SETQ X (CONS (PREPSQ* POLYPART) X))))
      (SETKORDER XX)
      (RETURN (CONS 'LIST X)))) 
(PUT 'XEUCL 'NUMBER-OF-ARGS 3) 
(PUT 'XEUCL 'DEFINED-ON-LINE '105) 
(PUT 'XEUCL 'DEFINED-IN-FILE 'MISC/PF.RED) 
(PUT 'XEUCL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE XEUCL (U V VAR)
    (PROG (Q R S W)
      (SETQ Q (LIST (CONS 1 1) (CONS NIL 1)))
      (SETQ R (LIST (CONS NIL 1) (CONS 1 1)))
      (COND
       ((LESSP (DEGR U VAR) (DEGR V VAR))
        (PROGN
         (SETQ S U)
         (SETQ U V)
         (SETQ V S)
         (SETQ S Q)
         (SETQ Q R)
         (SETQ R S))))
      (SETQ U (CONS U 1))
      (SETQ V (CONS V 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CAR V)) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (DEGR (CAR V) VAR) 0)
           (SETQ W (CONS (MULTSQ U (INVSQ V)) (CONS NIL 1))))
          (T (SETQ W (QREMSQ U V VAR))))
         (SETQ S
                 (LIST (ADDSQ (CAR Q) (NEGSQ (MULTSQ (CAR W) (CAR R))))
                       (ADDSQ (CADR Q) (NEGSQ (MULTSQ (CAR W) (CADR R))))))
         (SETQ U V)
         (SETQ V (CDR W))
         (SETQ Q R)
         (SETQ R S))
        (GO WHILELABEL))
      (SETQ V (CONS (LNC (CAR U)) (CDR U)))
      (SETQ R (MULTSQ V (INVSQ U)))
      (RETURN
       (CONS (MULTSQ R (MULTSQ (CAR Q) (INVSQ V)))
             (MULTSQ R (MULTSQ (CADR Q) (INVSQ V))))))) 
(PUT 'QREMSQ 'NUMBER-OF-ARGS 3) 
(PUT 'QREMSQ 'DEFINED-ON-LINE '132) 
(PUT 'QREMSQ 'DEFINED-IN-FILE 'MISC/PF.RED) 
(PUT 'QREMSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QREMSQ (U V VAR)
    ((LAMBDA (X) (CONS (MULTSQ (ADDSQ U (NEGSQ X)) (INVSQ V)) X))
     (REMSQ U V VAR))) 
(PUT 'REMSQ 'NUMBER-OF-ARGS 3) 
(PUT 'REMSQ 'DEFINED-ON-LINE '138) 
(PUT 'REMSQ 'DEFINED-IN-FILE 'MISC/PF.RED) 
(PUT 'REMSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REMSQ (U V VAR)
    (PROG (M N X)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ N (DEGR (CAR V) VAR))
      (COND ((EQUAL N 0) (REDERR (LIST "Remsq given zero degree polynomial"))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GEQ (SETQ M (DEGR (CAR U) VAR)) N)) (RETURN NIL)))
        (PROGN
         (COND ((EQUAL M N) (SETQ X V))
               (T
                (SETQ X
                        (MULTSQ
                         (CONS (LIST (CONS (CONS VAR (DIFFERENCE M N)) 1)) 1)
                         V))))
         (SETQ U
                 (ADDSQ U
                        (NEGSQ
                         (MULTSQ
                          (CONS
                           ((LAMBDA (G123)
                              (COND
                               (*PHYSOP-LOADED (PHYSOP-MULTF G123 (CDR V)))
                               (T (POLY-MULTF G123 (CDR V)))))
                            (CDAR (CAR U)))
                           ((LAMBDA (G125)
                              (COND
                               (*PHYSOP-LOADED (PHYSOP-MULTF G125 (CDR U)))
                               (T (POLY-MULTF G125 (CDR U)))))
                            (CDAR (CAR V))))
                          X)))))
        (GO WHILELABEL))
      (RETURN U))) 
(PUT 'PFPOWER 'NUMBER-OF-ARGS 4) 
(PUT 'PFPOWER 'DEFINED-ON-LINE '154) 
(PUT 'PFPOWER 'DEFINED-IN-FILE 'MISC/PF.RED) 
(PUT 'PFPOWER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PFPOWER (U V N VAR)
    (PROG (X Z)
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (DEGR (CAR U) VAR) 0)) (RETURN NIL)))
        (PROGN
         (SETQ X (QREMSQ U (CONS V 1) VAR))
         (SETQ Z (CONS (CONS (CDR X) (CONS V N)) Z))
         (SETQ N (DIFFERENCE N 1))
         (SETQ U (CAR X)))
        (GO WHILELABEL))
      (COND ((CAR U) (SETQ Z (CONS (CONS U (CONS V N)) Z))))
      (RETURN Z))) 
(ENDMODULE) 