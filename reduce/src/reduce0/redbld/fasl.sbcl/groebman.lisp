(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBMAN)) 
(FLAG '(GROEBRESTRICTION GROEBRESMAX GVARSLAST GROEBPROTFILE GLTB) 'SHARE) 
(PUT 'GSORTEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GSORTEVAL 'DEFINED-ON-LINE '33) 
(PUT 'GSORTEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBMAN.RED) 
(PUT 'GSORTEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GSORTEVAL (PARS)
    (PROG (VARS U V W OLDORDER NOLIST *FACTOR *EXP *GSUGAR N PCOUNT*)
      (SETQ N 0)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (SETQ N (LENGTH PARS))
      (SETQ U (REVAL1 (CAR PARS) T))
      (SETQ V (COND ((GREATERP N 1) (REVAL1 (CADR PARS) T)) (T NIL)))
      (COND
       ((NOT (EQCAR U 'LIST)) (PROGN (SETQ NOLIST T) (SETQ U (LIST 'LIST U)))))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'GSORT)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ *VDPINTEGER NIL)
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (A2VDP J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (A2VDP J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (VDPLSORT W))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X W)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (DIP2A (CADR (CDDR X)))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (DIP2A (CADR (CDDR X)))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (MEMBER 0 W)) (RETURN NIL)))
        (SETQ W (DELETE 0 W))
        (GO WHILELABEL))
      (SETKORDER OLDORDER)
      (RETURN (COND ((AND NOLIST W) (CAR W)) (T (CONS 'LIST W)))))) 
(PUT 'GSORT 'PSOPFN 'GSORTEVAL) 
(PUT 'GSPLITEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GSPLITEVAL 'DEFINED-ON-LINE '58) 
(PUT 'GSPLITEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBMAN.RED) 
(PUT 'GSPLITEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GSPLITEVAL (PARS)
    (PROG (VARS X U V W OLDORDER *FACTOR *EXP *GSUGAR N PCOUNT*)
      (SETQ N 0)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (SETQ N (LENGTH PARS))
      (SETQ U (REVAL1 (CAR PARS) T))
      (SETQ V (COND ((GREATERP N 1) (REVAL1 (CADR PARS) T)) (T NIL)))
      (SETQ U (LIST 'LIST U))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'GSPLIT)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ *VDPINTEGER NIL)
      (SETQ W (A2VDP (CAR W)))
      (COND ((OR (NULL W) (NULL (CADR (CDDR W)))) (SETQ X W))
            (T
             (PROGN
              (SETQ X (VDPFMON (CADDR W) (CADR W)))
              (SETQ W (VDPRED W)))))
      (SETQ W (LIST 'LIST (DIP2A (CADR (CDDR X))) (DIP2A (CADR (CDDR W)))))
      (SETKORDER OLDORDER)
      (RETURN W))) 
(PUT 'GSPLIT 'PSOPFN 'GSPLITEVAL) 
(PUT 'GSPOLYEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GSPOLYEVAL 'DEFINED-ON-LINE '80) 
(PUT 'GSPOLYEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBMAN.RED) 
(PUT 'GSPOLYEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GSPOLYEVAL (PARS)
    (PROG (VARS U U1 U2 V W OLDORDER *FACTOR *EXP *GSUGAR N PCOUNT*)
      (SETQ N 0)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (SETQ N (LENGTH PARS))
      (COND
       ((OR (LESSP N 2) (IGREATERP N 3))
        (RERROR 'GROEBNR2 1 "gspoly, illegal number or parameters")))
      (SETQ U1 (CAR PARS))
      (SETQ U2 (CADR PARS))
      (SETQ U (LIST 'LIST U1 U2))
      (SETQ V (COND ((GREATERP N 2) (GROEREVLIST (CADDR PARS))) (T NIL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'GSPOLY)))
      (GROEDOMAINMODE)
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (F2VDP (CAR (SIMP J)))) (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (F2VDP (CAR (SIMP J)))) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (DIP2A (CADR (CDDR (GROEBSPOLYNOM3 (CAR W) (CADR W))))))
      (SETKORDER OLDORDER)
      (RETURN W))) 
(PUT 'GSPOLY 'PSOPFN 'GSPOLYEVAL) 
(PUT 'GVARSEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GVARSEVAL 'DEFINED-ON-LINE '102) 
(PUT 'GVARSEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBMAN.RED) 
(PUT 'GVARSEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GVARSEVAL (U)
    (PROG (N V *FACTOR *EXP *GSUGAR)
      (SETQ N 0)
      (SETQ *EXP T)
      (SETQ N (LENGTH U))
      (SETQ V
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST (REVAL1 (CAR U) T)))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ V (GROEBNERVARS V NIL))
      (SETQ V
              (COND
               ((EQUAL N 2) (INTERSECTION V (GROEREVLIST (REVAL1 (CADR U) T))))
               (T V)))
      (RETURN (CONS 'LIST V)))) 
(PUT 'GVARS 'PSOPFN 'GVARSEVAL) 
(PUT 'GREDUCEEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GREDUCEEVAL 'DEFINED-ON-LINE '115) 
(PUT 'GREDUCEEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBMAN.RED) 
(PUT 'GREDUCEEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GREDUCEEVAL (PARS)
    (PROG (VARS X U V W NP OLDORDER *FACTOR *GROEBFAC *EXP *GSUGAR N PCOUNT*)
      (SETQ N 0)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (COND
       (*GROEBPROT
        (SETQ GROEBPROTFILE
                (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (LIST 'LIST)))))
      (SETQ N (LENGTH PARS))
      (SETQ X (REVAL1 (CAR PARS) T))
      (SETQ U (REVAL1 (CADR PARS) T))
      (SETQ V (COND ((GREATERP N 2) (REVAL1 (CADDR PARS) T)) (T NIL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL W) (RERROR 'GROEBNR2 2 "Empty list in greduce")))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'GREDUCE)))
      (SETQ OLDORDER (VDPINIT VARS))
      (GROEDOMAINMODE)
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (REORDER (CAR (SIMP J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (REORDER (CAR (SIMP J)))) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*GROEBOPT
        (PROGN
         (SETQ W (VDPVORDOPT W VARS))
         (SETQ VARS (CDR W))
         (SETQ W (CAR W))
         (VDPINIT VARS))))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (F2VDP J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (F2VDP J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*GROEBPROT
        (SETQ W
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J W)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (VDPENUMERATE J)) (CAR J))
                                        NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (VDPENUMERATE J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       ((NOT *VDPINTEGER)
        (PROGN
         (SETQ NP T)
         (PROG (P)
           (SETQ P W)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (SETQ NP
                      (COND (NP (DIPCOEFFCIENTSFROMDOMAIN? (CADR (CDDR P))))
                            (T NIL))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (COND
          ((NOT NP) (PROGN (SETQ *VDPMODULAR NIL) (SETQ *VDPINTEGER T)))))))
      (SETQ W (GROEBNER2 W NIL))
      (SETQ X (A2VDP X))
      (COND
       (*GROEBPROT
        (PROGN
         (SETQ W
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J W)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J) (VDPENUMERATE J)) (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (J) (VDPENUMERATE J)) (CAR J)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (GROEBPROTSETQ 'CANDIDATE (DIP2A (CADR (CDDR X))))
         (PROG (J)
           (SETQ J W)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (GROEBPROTSETQ (MKID 'POLY (VDPGETPROP J 'NUMBER))
                             (DIP2A (CADR (CDDR J)))))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB)))))
      (SETQ W (CAR W))
      (SETQ *VDPINTEGER NIL)
      (SETQ W (GROEBNORMALFORM X W 'SORT))
      (SETQ W (DIP2A (CADR (CDDR W))))
      (SETKORDER OLDORDER)
      (SETQ GVARSLAST (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST VARS)))
      (RETURN (COND (W W) (T 0))))) 
(PUT 'GREDUCE 'PSOPFN 'GREDUCEEVAL) 
(PUT 'PREDUCE 'PSOPFN 'PREDUCEEVAL) 
(ENDMODULE) 