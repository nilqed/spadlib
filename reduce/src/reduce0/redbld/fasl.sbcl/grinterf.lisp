(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GRINTERF)) 
(FLAG '(GROEBRESTRICTION GVARSLAST GROEBPROTFILE GLTB GLTERMS GMODULE) 'SHARE) 
(SWITCH (LIST 'GROEBOPT 'TRGROEB 'GLTBASIS 'GSUGAR)) 
(SETQ VDPSORTMODE* 'LEX) 
(SETQ GLTB (PROGN (SETQ ALGLIST* (CONS NIL NIL)) '(LIST))) 
(PUT 'GROEBNEREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNEREVAL 'DEFINED-ON-LINE '41) 
(PUT 'GROEBNEREVAL 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBNEREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNEREVAL (U)
    (PROG (N *GROEBFAC *GROEBRM *FACTOR *EXP)
      (SETQ *EXP T)
      (SETQ N (LENGTH U))
      (COND
       ((EQUAL N 1) (RETURN (CADR (GROEBNER1 (REVAL1 (CAR U) T) NIL NIL))))
       ((NEQ N 2)
        (RERROR 'GROEBNER 1 "groebner called with wrong number of arguments")))
      (SETQ U (GROEBNER1 (REVAL1 (CAR U) T) (REVAL1 (CADR U) T) NIL))
      (COND
       (*GLTBASIS
        (SETQ GLTB (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CADR GLTB)))))
      (RETURN (CADR U)))) 
(PUT 'GROEBNER 'PSOPFN 'GROEBNEREVAL) 
(PUT 'GROEBNERFEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNERFEVAL 'DEFINED-ON-LINE '54) 
(PUT 'GROEBNERFEVAL 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBNERFEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNERFEVAL (U)
    (PROG (N *GROEBFAC *GROEBRM *FACTOR *EXP *EZGCD S R Q)
      (SETQ *EXP T)
      (SETQ *GROEBRM (SETQ *GROEBFAC T))
      (SETQ GROEBRESTRICTION* (REVAL1 GROEBRESTRICTION T))
      (COND ((NULL DMODE*) (SETQ *EZGCD T)))
      (SETQ N (LENGTH U))
      (SETQ R
              (COND ((EQUAL N 1) (GROEBNER1 (REVAL1 (CAR U) T) NIL NIL))
                    ((EQUAL N 2)
                     (GROEBNER1 (REVAL1 (CAR U) T) (REVAL1 (CADR U) T) NIL))
                    ((NEQ N 3)
                     (RERROR 'GROEBNER 2
                             "groebner called with wrong number of arguments"))
                    (T
                     (GROEBNER1 (REVAL1 (CAR U) T) (REVAL1 (CADR U) T)
                      (REVAL1 (CADDR U) T)))))
      (SETQ Q R)
      (PROG ()
       WHILELABEL
        (COND ((NOT Q) (RETURN NIL)))
        (PROGN
         (SETQ S (CAR Q))
         (SETQ Q (CDR Q))
         (COND ((MEMBER S Q) (SETQ R (DELETE S R)))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'GROEBNERF 'PSOPFN 'GROEBNERFEVAL) 
(PUT 'IDQUOTIENTEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'IDQUOTIENTEVAL 'DEFINED-ON-LINE '75) 
(PUT 'IDQUOTIENTEVAL 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'IDQUOTIENTEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDQUOTIENTEVAL (U)
    (PROG (N *FACTOR *EXP)
      (SETQ *EXP T)
      (SETQ N (LENGTH U))
      (COND
       ((EQUAL N 2)
        (RETURN (GROEBIDQ (REVAL1 (CAR U) T) (REVAL1 (CADR U) T) NIL)))
       ((NEQ N 3)
        (RERROR 'GROEBNER 3
                "idquotient called with wrong number of arguments"))
       (T
        (RETURN
         (GROEBIDQ (REVAL1 (CAR U) T) (REVAL1 (CADR U) T)
          (REVAL1 (CADDR U) T))))))) 
(PUT 'IDEALQUOTIENT 'PSOPFN 'IDQUOTIENTEVAL) 
(PUT 'SATURATIONEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SATURATIONEVAL 'DEFINED-ON-LINE '85) 
(PUT 'SATURATIONEVAL 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'SATURATIONEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SATURATIONEVAL (U)
    (PROG (A B C *FACTOR *EXP)
      (SETQ *EXP T)
      (COND ((EQUAL (LENGTH U) 2) (GO AA)))
      (RERROR 'GROEBNER 19 "saturation called with wrong number of arguments")
     AA
      (SETQ A (REVAL1 (CAR U) T))
      (COND ((EQUAL (CAR A) 'LIST) (GO BB)))
      (RERROR 'GROEBNER 20 "saturation, first parameter must be a list")
     BB
      (SETQ A
              (CONS 'LIST
                    (PROG (AA FORALL-RESULT FORALL-ENDPTR)
                      (SETQ AA (CDR A))
                      (COND ((NULL AA) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (AA)
                                          (COND
                                           ((EQEXPR AA) (REVAL1 (*EQN2A AA) T))
                                           (T AA)))
                                        (CAR AA))
                                       NIL)))
                     LOOPLABEL
                      (SETQ AA (CDR AA))
                      (COND ((NULL AA) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (AA)
                                  (COND ((EQEXPR AA) (REVAL1 (*EQN2A AA) T))
                                        (T AA)))
                                (CAR AA))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ C (REVAL1 (CADR U) T))
      (COND
       ((EQUAL (CAR C) 'LIST)
        (RERROR 'GROEBNER 25
                "saturation, second parameter must not be a list")))
      (COND ((EQEXPR C) (SETQ C (REVAL1 (*EQN2A C) T))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL B A))) (RETURN NIL)))
        (PROGN (COND (B (SETQ A B))) (SETQ B (GROEBIDQ A C NIL)))
        (GO WHILELABEL))
      (RETURN B))) 
(PUT 'SATURATION 'PSOPFN 'SATURATIONEVAL) 
(PUT 'GROEBNER1 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBNER1 'DEFINED-ON-LINE '102) 
(PUT 'GROEBNER1 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBNER1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBNER1 (U V R)
    (PROG (VARS W NP OLDORDER *GRMOD* PCOUNT*)
      (SETQ PCOUNT* 0)
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
      (COND ((NULL W) (RERROR 'GROEBNER 4 "empty list in groebner")))
      (SETQ VARS (GROEBNERVARS W V))
      (COND (R (SETQ R (GROEREVLIST R))))
      (GROEDOMAINMODE)
      (COND (VARS (GO NOTEMPTY)))
      (SETQ U 0)
      (PROG (P)
        (SETQ P W)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (COND ((NEQ P 0) (SETQ U 1)))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (LIST 'LIST (LIST 'LIST U)))
     NOTEMPTY
      (COND
       ((AND (EQ DMODE* '|:MOD:|) (NULL (SETDIFF (GVARLIS W) VARS))
             (LESSP CURRENT-MODULUS LARGEST-SMALL-MODULUS))
        (SETQ *GRMOD* T)))
      (SETQ OLDORDER (VDPINIT VARS))
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
       ((AND *GROEBOPT (MEMQ VDPSORTMODE* '(LEX GRADLEX REVGRADLEX)))
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
      (COND
       (*GROEBPROT
        (SETQ GROEBPROTFILE
                (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (LIST 'LIST)))))
      (COND
       (R
        (SETQ R
                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                  (SETQ P R)
                  (COND ((NULL P) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (P)
                                      (VDPSIMPCONT (F2VDP (CAR (SIMP P)))))
                                    (CAR P))
                                   NIL)))
                 LOOPLABEL
                  (SETQ P (CDR P))
                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (P) (VDPSIMPCONT (F2VDP (CAR (SIMP P)))))
                            (CAR P))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ W (GROEBNER2 W R))
      (COND
       ((CDR W)
        (PROG (*GSUGAR)
          (PROG (B)
            (SETQ B W)
           LAB
            (COND ((NULL B) (RETURN NIL)))
            ((LAMBDA (B)
               (PROG (C)
                 (SETQ C W)
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C)
                    (COND
                     ((AND B (NEQ B C))
                      (PROGN
                       (SETQ V T)
                       (PROG (P)
                         (SETQ P C)
                        LAB
                         (COND ((NULL P) (RETURN NIL)))
                         ((LAMBDA (P)
                            (SETQ V
                                    (AND V
                                         ((LAMBDA (U)
                                            (OR (NULL U)
                                                (NULL (CADR (CDDR U)))))
                                          (GROEBNORMALFORM P B 'LIST)))))
                          (CAR P))
                         (SETQ P (CDR P))
                         (GO LAB))
                       (COND
                        (V (PROGN (SETQ W (DELETE B W)) (SETQ B NIL))))))))
                  (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB)))
             (CAR B))
            (SETQ B (CDR B))
            (GO LAB)))))
      (COND
       (*GLTBASIS
        (SETQ GLTB
                (PROGN
                 (SETQ ALGLIST* (CONS NIL NIL))
                 (CONS 'LIST
                       (PROG (BASE FORALL-RESULT FORALL-ENDPTR)
                         (SETQ BASE W)
                         (COND ((NULL BASE) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (BASE)
                                             (CONS 'LIST
                                                   (PROG (J FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ J BASE)
                                                     (COND
                                                      ((NULL J) (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (J)
                                                                         (DIP2A
                                                                          (CADR
                                                                           (CDDR
                                                                            (VDPFMON
                                                                             (A2BC
                                                                              1)
                                                                             (CADR
                                                                              J))))))
                                                                       (CAR J))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ J (CDR J))
                                                     (COND
                                                      ((NULL J)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (J)
                                                                 (DIP2A
                                                                  (CADR
                                                                   (CDDR
                                                                    (VDPFMON
                                                                     (A2BC 1)
                                                                     (CADR
                                                                      J))))))
                                                               (CAR J))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL))))
                                           (CAR BASE))
                                          NIL)))
                        LOOPLABEL
                         (SETQ BASE (CDR BASE))
                         (COND ((NULL BASE) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (BASE)
                                     (CONS 'LIST
                                           (PROG (J FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ J BASE)
                                             (COND ((NULL J) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (J)
                                                                 (DIP2A
                                                                  (CADR
                                                                   (CDDR
                                                                    (VDPFMON
                                                                     (A2BC 1)
                                                                     (CADR
                                                                      J))))))
                                                               (CAR J))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ J (CDR J))
                                             (COND
                                              ((NULL J)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (J)
                                                         (DIP2A
                                                          (CADR
                                                           (CDDR
                                                            (VDPFMON (A2BC 1)
                                                                     (CADR
                                                                      J))))))
                                                       (CAR J))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL))))
                                   (CAR BASE))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (SETQ W
              (CONS 'LIST
                    (PROG (BASE FORALL-RESULT FORALL-ENDPTR)
                      (SETQ BASE W)
                      (COND ((NULL BASE) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (BASE)
                                          (CONS 'LIST
                                                (PROG (J FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ J BASE)
                                                  (COND
                                                   ((NULL J) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA (J)
                                                                      (DIP2A
                                                                       (CADR
                                                                        (CDDR
                                                                         J))))
                                                                    (CAR J))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ J (CDR J))
                                                  (COND
                                                   ((NULL J)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (J)
                                                              (DIP2A
                                                               (CADR
                                                                (CDDR J))))
                                                            (CAR J))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL))))
                                        (CAR BASE))
                                       NIL)))
                     LOOPLABEL
                      (SETQ BASE (CDR BASE))
                      (COND ((NULL BASE) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (BASE)
                                  (CONS 'LIST
                                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ J BASE)
                                          (COND ((NULL J) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (J)
                                                              (DIP2A
                                                               (CADR
                                                                (CDDR J))))
                                                            (CAR J))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ J (CDR J))
                                          (COND
                                           ((NULL J) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J)
                                                      (DIP2A (CADR (CDDR J))))
                                                    (CAR J))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))
                                (CAR BASE))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ DIPEVLIST* (LIST NIL))
      (SETQ GVARSLAST (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST VARS)))
      (RETURN W))) 
(PUT 'GROEBNERVARS 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBNERVARS 'DEFINED-ON-LINE '146) 
(PUT 'GROEBNERVARS 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBNERVARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBNERVARS (W V)
    (PROG (Z DV GDV VARS)
      (COND ((EQUAL V '(LIST)) (SETQ V NIL)))
      (SETQ V (OR V (AND (SETQ GDV (CDR GLOBAL-DIPVARS*)) GLOBAL-DIPVARS*)))
      (SETQ VARS
              (COND
               ((NULL V)
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J (GVARLIS W))
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T
                (PROGN
                 (SETQ Z (GVARLIS W))
                 (GROEBNERZEROBC (SETDIFF Z (SETQ V (GROEREVLIST V))))
                 (PROG (J)
                   (SETQ J V)
                  LAB
                   (COND ((NULL J) (RETURN NIL)))
                   ((LAMBDA (J)
                      (COND ((MEMBER J Z) (SETQ DV (CONS (*A2K J) DV)))))
                    (CAR J))
                   (SETQ J (CDR J))
                   (GO LAB))
                 (SETQ DV (REVERSIP DV))
                 (COND
                  ((AND (NOT (EQUAL (LENGTH V) (LENGTH DV))) *TRGROEB)
                   (PROGN
                    (PRIN2 " Groebner: ")
                    (PRIN2 (DIFFERENCE (LENGTH V) (LENGTH DV)))
                    (PRIN2T " of the variables not used")
                    (TERPRI))))
                 DV))))
      (RETURN (OR GDV VARS)))) 
(PUT 'GROEBNERZEROBC 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNERZEROBC 'DEFINED-ON-LINE '164) 
(PUT 'GROEBNERZEROBC 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBNERZEROBC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNERZEROBC (U)
    (COND
     (U
      (PROG (W M P)
        (SETQ BCZERODIVL* NIL)
        (SETQ M *MATCH)
        (SETQ *MATCH NIL)
        (SETQ P POWLIS*)
        (SETQ POWLIS* NIL)
        (PROG (R)
          (SETQ R M)
         LAB
          (COND ((NULL R) (RETURN NIL)))
          ((LAMBDA (R)
             (COND
              ((EQUAL (CADR R) '(NIL . T))
               (PROGN
                (SETQ W
                        (CAR
                         (SIMP
                          (LIST 'DIFFERENCE
                                (CONS 'TIMES
                                      (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ Q (CAR R))
                                        (COND ((NULL Q) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (Q)
                                                            (LIST 'EXPT (CAR Q)
                                                                  (CDR Q)))
                                                          (CAR Q))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ Q (CDR Q))
                                        (COND
                                         ((NULL Q) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q)
                                                    (LIST 'EXPT (CAR Q)
                                                          (CDR Q)))
                                                  (CAR Q))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))
                                (CADDR R)))))
                (PROG (X)
                  (SETQ X (KERNELS W))
                 LAB
                  (COND ((NULL X) (RETURN NIL)))
                  ((LAMBDA (X) (COND ((NOT (MEMBER X U)) (SETQ W NIL))))
                   (CAR X))
                  (SETQ X (CDR X))
                  (GO LAB))
                (COND (W (SETQ BCZERODIVL* (CONS W BCZERODIVL*))))))))
           (CAR R))
          (SETQ R (CDR R))
          (GO LAB))
        (PROG (R)
          (SETQ R P)
         LAB
          (COND ((NULL R) (RETURN NIL)))
          ((LAMBDA (R)
             (COND
              ((AND (MEMBER (CAR R) U) (EQUAL (CADDR R) '(NIL . T)))
               (PROGN
                (SETQ W
                        (CAR
                         (SIMP
                          (LIST 'DIFFERENCE (LIST 'EXPT (CAR R) (CADR R))
                                (CADDDR R)))))
                (SETQ BCZERODIVL* (CONS W BCZERODIVL*))))))
           (CAR R))
          (SETQ R (CDR R))
          (GO LAB))
        (PROG (R)
          (SETQ R ASYMPLIS*)
         LAB
          (COND ((NULL R) (RETURN NIL)))
          ((LAMBDA (R)
             (COND
              ((MEMBER (CAR R) U)
               (SETQ BCZERODIVL* (CONS (CONS (CONS R 1) NIL) BCZERODIVL*)))))
           (CAR R))
          (SETQ R (CDR R))
          (GO LAB))
        (SETQ *MATCH M)
        (SETQ POWLIS* P))))) 
(PUT 'GVARLIS 'NUMBER-OF-ARGS 1) 
(PUT 'GVARLIS 'DEFINED-ON-LINE '185) 
(PUT 'GVARLIS 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GVARLIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GVARLIS (U) (SORT (GVARLIS1 U NIL) (FUNCTION ORDOP))) 
(PUT 'GVARLIS1 'NUMBER-OF-ARGS 2) 
(PUT 'GVARLIS1 'DEFINED-ON-LINE '189) 
(PUT 'GVARLIS1 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GVARLIS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GVARLIS1 (U V)
    (COND ((NULL U) V) (T (UNION (GVAR1 (CAR U) V) (GVARLIS1 (CDR U) V))))) 
(PUT 'GVAR1 'NUMBER-OF-ARGS 2) 
(PUT 'GVAR1 'DEFINED-ON-LINE '192) 
(PUT 'GVAR1 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GVAR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GVAR1 (U V)
    (COND ((OR (NULL U) (NUMBERP U) (AND (EQ U 'I) *COMPLEX)) V)
          ((ATOM U) (COND ((MEMBER U V) V) (T (CONS U V))))
          ((GET (CAR U) 'DNAME) V)
          ((MEMQ (CAR U) '(PLUS TIMES EXPT DIFFERENCE MINUS))
           (GVARLIS1 (CDR U) V))
          ((EQ (CAR U) 'QUOTIENT) (GVAR1 (CADR U) V)) ((MEMBER U V) V)
          (T (CONS U V)))) 
(PUT 'GROEBIDQ 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBIDQ 'DEFINED-ON-LINE '201) 
(PUT 'GROEBIDQ 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEBIDQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBIDQ (U F V)
    (PROG (VARS W NP OLDORDER *FACTOR *EXP PCOUNT*)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
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
      (COND ((NULL W) (RERROR 'GROEBNER 5 "empty list in idealquotient")))
      (COND ((EQEXPR F) (SETQ F (*EQN2A F))))
      (SETQ VARS (GROEBNERVARS W V))
      (GROEDOMAINMODE)
      (COND ((NULL VARS) (VDPERR 'IDEALQUOTIENT)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CAR (SIMP J))) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CAR (SIMP J))) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ F (CAR (SIMP F)))
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
      (SETQ F (F2VDP F))
      (COND
       ((NOT *VDPINTEGER)
        (PROGN
         (SETQ NP T)
         (PROG (P)
           (SETQ P (CONS F W))
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
      (SETQ W (GROEBIDQ2 W F))
      (SETQ W
              (CONS 'LIST
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J W)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (DIP2A (CADR (CDDR J))))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (DIP2A (CADR (CDDR J)))) (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETKORDER OLDORDER)
      (RETURN W))) 
(FLUID '(*BACKTRACE)) 
(PUT 'VDPERR 'NUMBER-OF-ARGS 1) 
(PUT 'VDPERR 'DEFINED-ON-LINE '231) 
(PUT 'VDPERR 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'VDPERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPERR (NAME)
    (PROGN
     (PRIN2 "**** Groebner illegal parmeter in ")
     (PRIN2 NAME)
     (COND (*BACKTRACE (BACKTRACE)))
     (RERROR 'GROEBNER 6 ",e.g. no relevant variables found"))) 
(PUT 'GROEPARAMS 'NUMBER-OF-ARGS 3) 
(PUT 'GROEPARAMS 'DEFINED-ON-LINE '237) 
(PUT 'GROEPARAMS 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEPARAMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEPARAMS (U NMIN NMAX)
    (PROG (N W)
      (SETQ N (LENGTH U))
      (COND
       ((OR (LESSP N NMIN) (GREATERP N NMAX))
        (RERROR 'GROEBNER 7
                "illegal number of parameters in call to groebner package")))
      (SETQ U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V U)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (V)
                                    (PROGN
                                     (SETQ W (REVAL1 V T))
                                     (COND
                                      ((EQCAR W 'LIST)
                                       (CONS 'LIST (GROEREVLIST W)))
                                      (T W))))
                                  (CAR V))
                                 NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (V)
                            (PROGN
                             (SETQ W (REVAL1 V T))
                             (COND
                              ((EQCAR W 'LIST) (CONS 'LIST (GROEREVLIST W)))
                              (T W))))
                          (CAR V))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP (LENGTH U) NMAX)) (RETURN NIL)))
        (SETQ U (APPEND U '(NIL)))
        (GO WHILELABEL))
      (RETURN U))) 
(PUT 'VDPINIT 'NUMBER-OF-ARGS 1) 
(PUT 'VDPINIT 'DEFINED-ON-LINE '254) 
(PUT 'VDPINIT 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'VDPINIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VDPINIT (VARS)
    (PROG (R GM)
      (COND
       ((AND (EQCAR GMODULE 'LIST) (CDR GMODULE))
        (SETQ GM
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y (CDR GMODULE))
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (Y)
                                      (PROGN
                                       (SETQ Y (REVAL1 Y T))
                                       (COND
                                        ((NOT (MEMBER Y VARS))
                                         (SETQ VARS (APPEND VARS (LIST Y)))))
                                       Y))
                                    (CAR Y))
                                   NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (Y)
                              (PROGN
                               (SETQ Y (REVAL1 Y T))
                               (COND
                                ((NOT (MEMBER Y VARS))
                                 (SETQ VARS (APPEND VARS (LIST Y)))))
                               Y))
                            (CAR Y))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ R (VDPINIT2 VARS))
      (SETQ GMODULE* (COND (GM (CADR (A2VDP (CONS 'TIMES GM))))))
      (RETURN R))) 
(PUT 'GROEDOMAINMODE 'NUMBER-OF-ARGS 0) 
(PUT 'GROEDOMAINMODE 'DEFINED-ON-LINE '264) 
(PUT 'GROEDOMAINMODE 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEDOMAINMODE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GROEDOMAINMODE NIL
    (PROGN
     (SETQ *VDPINTEGER (SETQ *VDPMODULAR NIL))
     (COND ((NOT (FLAGP DMODE* 'FIELD)) (SETQ *VDPINTEGER T))
           (*MODULAR (SETQ *VDPMODULAR T))))) 
(PUT 'GROEDELETIP 'NUMBER-OF-ARGS 2) 
(PUT 'GROEDELETIP 'DEFINED-ON-LINE '274) 
(PUT 'GROEDELETIP 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEDELETIP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEDELETIP (A B)
    (PROG (Q)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND B (EQUAL A (CAR B)))) (RETURN NIL)))
        (SETQ B (CDR B))
        (GO WHILELABEL))
      (COND ((NULL B) (RETURN NIL)))
      (SETQ Q B)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR B)) (RETURN NIL)))
        (COND ((EQUAL A (CADR B)) (SETCDR B (CDDR B))) (T (SETQ B (CDR B))))
        (GO WHILELABEL))
      (RETURN Q))) 
(PUT 'GROEREVLIST 'NUMBER-OF-ARGS 1) 
(PUT 'GROEREVLIST 'DEFINED-ON-LINE '281) 
(PUT 'GROEREVLIST 'DEFINED-IN-FILE 'GROEBNER/GRINTERF.RED) 
(PUT 'GROEREVLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEREVLIST (U)
    (PROGN
     (COND ((IDP U) (SETQ U (REVAL1 U T))))
     (PROG (P FORALL-RESULT FORALL-ENDPTR)
       (SETQ P (GETRLIST U))
       (COND ((NULL P) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (P) (REVAL1 P T)) (CAR P)) NIL)))
      LOOPLABEL
       (SETQ P (CDR P))
       (COND ((NULL P) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) (REVAL1 P T)) (CAR P)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(ENDMODULE) 