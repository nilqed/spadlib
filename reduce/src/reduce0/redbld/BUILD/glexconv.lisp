(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GLEXCONV)) 
(FLAG '(GVARSLAST) 'SHARE) 
(SWITCH (LIST 'GROEBFAC 'TRGROEB)) 
(FLUID '(PCOUNT*)) 
(FLUID '(GLEXMAT*)) 
(PUT 'GLEXCONVERTEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GLEXCONVERTEVAL 'DEFINED-ON-LINE '45) 
(PUT 'GLEXCONVERTEVAL 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXCONVERTEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GLEXCONVERTEVAL (U)
    (PROG (*GROEBFAC *GROEBRM *FACTOR *GSUGAR V BAS VARS MAXDEG NEWVARS *EXP)
      (SETQ *EXP T)
      (SETQ U
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P U)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (REVAL1 P T)) (CAR P))
                                      NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (REVAL1 P T)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ BAS (CAR U))
      (SETQ U (CDR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR U))
         (SETQ U (CDR U))
         (COND ((AND (EQCAR V 'LIST) (NULL VARS)) (SETQ VARS V))
               ((EQCAR V 'EQUAL)
                (COND
                 ((AND (SETQ V (CDR V)) (EQCAR V 'MAXDEG))
                  (SETQ MAXDEG (CADR V)))
                 ((EQCAR V 'NEWVARS) (SETQ NEWVARS (CADR V)))
                 (T
                  (PROGN
                   (PRIN2 (CAR V))
                   (RERROR 'GROEBNR2 4 "glexconvert, keyword unknown")))))
               (T
                (RERROR 'GROEBNR2 5
                        "Glexconvert, too many positional parameters"))))
        (GO WHILELABEL))
      (RETURN (GLEXBASE1 BAS VARS MAXDEG NEWVARS)))) 
(PUT 'GLEXCONVERT 'PSOPFN 'GLEXCONVERTEVAL) 
(PUT 'GLEXBASE1 'NUMBER-OF-ARGS 4) 
(PUT 'GLEXBASE1 'DEFINED-ON-LINE '64) 
(PUT 'GLEXBASE1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXBASE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXBASE1 (U V MAXDEG NV)
    (PROG (VARS W ND OLDORDER *GCD *EZGCD *GSUGAR PCOUNT*)
      (SETQ PCOUNT* 0)
      (SETQ *GCD T)
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
      (COND ((NULL W) (RERROR 'GROEBNR2 6 "Empty list in Groebner")))
      (SETQ VARS (GROEBNERVARS W V))
      (SETQ *VDPINTEGER (SETQ *VDPMODULAR NIL))
      (COND ((NOT (FLAGP DMODE* 'FIELD)) (SETQ *VDPINTEGER T))
            (*MODULAR (SETQ *VDPMODULAR T)))
      (COND ((NULL VARS) (VDPERR 'GROEBNER)))
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
      (PROG (P)
        (SETQ P W)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ ND (OR ND (NOT (DIPCOEFFCIENTSFROMDOMAIN? (CADR (CDDR P)))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       (ND
        (PROGN
         (SETQ *VDPMODULAR NIL)
         (SETQ *VDPINTEGER T)
         (SETQ GLEXDOMAIN* 2)))
       (T (SETQ GLEXDOMAIN* 1)))
      (COND ((AND (EQUAL GLEXDOMAIN* 1) (NOT *VDPMODULAR)) (SETQ *EZGCD T)))
      (COND ((NULL MAXDEG) (SETQ MAXDEG 200)))
      (COND (NV (SETQ NV (GROEREVLIST NV))))
      (COND ((NULL NV) (SETQ NV VARS))
            (T
             (PROG (X)
               (SETQ X NV)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (COND
                   ((NOT (MEMBER X VARS))
                    (PROGN
                     (RERROR 'GROEBNR2 7
                             (LIST "new variable " X
                                   " is not a basis variable"))))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))))
      (SETQ U
              (PROG (V FORALL-RESULT FORALL-ENDPTR)
                (SETQ V NV)
                (COND ((NULL V) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (V) (A2VDP V)) (CAR V)) NIL)))
               LOOPLABEL
                (SETQ V (CDR V))
                (COND ((NULL V) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (V) (A2VDP V)) (CAR V)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GBTEST W)
      (SETQ W (GLEXBASE2 W U MAXDEG))
      (SETQ W
              (CONS 'LIST
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J W)
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (J) (PREPF J)) (CAR J))
                                            NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (PREPF J)) (CAR J)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETKORDER OLDORDER)
      (SETQ GVARSLAST (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST VARS)))
      (RETURN W))) 
(FLUID '(GLEXEQSYS* GLEXVARS* GLEXCOUNT* GLEXSUB*)) 
(PUT 'GLEXBASE2 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXBASE2 'DEFINED-ON-LINE '100) 
(PUT 'GLEXBASE2 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXBASE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXBASE2 (OLDBASE VARS MAXDEG)
    (PROG (LEXBASE STAIRCASE MONBASE MONOM LISTOFNEXTS VECT Q GLEXEQSYS*
           GLEXVARS* GLEXSUB* N)
      (SETQ N 0)
      (COND
       ((NOT (GROEZERODIM? OLDBASE (LENGTH VARS)))
        (PRIN2T "####### warning: ideal is not zerodimensional ######")))
      (SETQ GLEXMAT*
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U VARS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (CADR U)) (CAR U)) NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (U) (CADR U)) (CAR U)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ MONBASE (SETQ STAIRCASE (SETQ LEXBASE NIL)))
      (SETQ MONOM (A2VDP 1))
      (SETQ LISTOFNEXTS NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL MONOM NIL))) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (GLEXMULTIPLETEST MONOM STAIRCASE))
           (PROGN
            (SETQ VECT (GLEXNORMALFORM MONOM OLDBASE))
            (SETQ Q (GLEXLINREL MONOM VECT MONBASE))
            (COND
             (Q
              (PROGN
               (SETQ LEXBASE (CONS Q LEXBASE))
               (SETQ MAXDEG NIL)
               (SETQ STAIRCASE (CONS MONOM STAIRCASE))))
             (T
              (PROGN
               (SETQ MONBASE (GLEXADDTOMONBASE MONOM VECT MONBASE))
               (SETQ N (IPLUS2 N 1))
               (COND
                ((AND MAXDEG (IGREATERP N MAXDEG))
                 (RERROR 'GROEBNR2 8
                         "No univar. polynomial within degree bound")))
               (SETQ LISTOFNEXTS (GLEXINSERNEXTS MONOM LISTOFNEXTS VARS))))))))
         (COND ((NULL LISTOFNEXTS) (SETQ MONOM NIL))
               (T
                (PROGN
                 (SETQ MONOM (CAR LISTOFNEXTS))
                 (SETQ LISTOFNEXTS (CDR LISTOFNEXTS))))))
        (GO WHILELABEL))
      (RETURN LEXBASE))) 
(PUT 'GLEXINSERNEXTS 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXINSERNEXTS 'DEFINED-ON-LINE '131) 
(PUT 'GLEXINSERNEXTS 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXINSERNEXTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXINSERNEXTS (MONOM L VARS)
    (PROG (X)
      (PROG (V)
        (SETQ V VARS)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ X (VDPPROD MONOM V))
            (COND
             ((NOT (VDPMEMBER X L))
              (PROGN
               (VDPPUTPROP X 'FACTOR MONOM)
               (VDPPUTPROP X 'MONFAC V)
               (SETQ L (GLEXINSERNEXTS1 X L)))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN L))) 
(PUT 'GLEXMULTIPLETEST 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXMULTIPLETEST 'DEFINED-ON-LINE '140) 
(PUT 'GLEXMULTIPLETEST 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXMULTIPLETEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXMULTIPLETEST (MONOM STAIRCASE)
    (COND ((NULL STAIRCASE) NIL)
          ((VEVMTEST? (CADR MONOM) (CADR (CAR STAIRCASE))) T)
          (T (GLEXMULTIPLETEST MONOM (CDR STAIRCASE))))) 
(PUT 'GLEXINSERNEXTS1 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXINSERNEXTS1 'DEFINED-ON-LINE '146) 
(PUT 'GLEXINSERNEXTS1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXINSERNEXTS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXINSERNEXTS1 (M L)
    (COND ((NULL L) (LIST M)) ((GLEXCOMP (CADR M) (CADR (CAR L))) (CONS M L))
          (T (CONS (CAR L) (GLEXINSERNEXTS1 M (CDR L)))))) 
(PUT 'GLEXCOMP 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXCOMP 'DEFINED-ON-LINE '151) 
(PUT 'GLEXCOMP 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXCOMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXCOMP (EV1 EV2)
    (GLEXCOMP0 (GLEXCOMPMAP EV1 GLEXMAT*) (GLEXCOMPMAP EV2 GLEXMAT*))) 
(PUT 'GLEXCOMP0 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXCOMP0 'DEFINED-ON-LINE '156) 
(PUT 'GLEXCOMP0 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXCOMP0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXCOMP0 (EV1 EV2)
    (COND ((NULL EV1) NIL) ((NULL EV2) (GLEXCOMP0 EV1 '(0)))
          ((EQUAL (IDIFFERENCE (CAR EV1) (CAR EV2)) 0)
           (GLEXCOMP0 (CDR EV1) (CDR EV2)))
          ((ILESSP (CAR EV1) (CAR EV2)) T) (T NIL))) 
(PUT 'GLEXCOMPMAP 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXCOMPMAP 'DEFINED-ON-LINE '163) 
(PUT 'GLEXCOMPMAP 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXCOMPMAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXCOMPMAP (EV MA)
    (COND ((NULL MA) NIL)
          (T (CONS (GLEXCOMPMAP1 EV (CAR MA)) (GLEXCOMPMAP EV (CDR MA)))))) 
(PUT 'GLEXCOMPMAP1 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXCOMPMAP1 'DEFINED-ON-LINE '167) 
(PUT 'GLEXCOMPMAP1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXCOMPMAP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXCOMPMAP1 (EV1 EV2)
    (COND ((OR (NULL EV1) (NULL EV2)) 0)
          (T
           (IPLUS2 (ITIMES2 (CAR EV1) (CAR EV2))
                   (GLEXCOMPMAP1 (CDR EV1) (CDR EV2)))))) 
(PUT 'GLEXADDTOMONBASE 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXADDTOMONBASE 'DEFINED-ON-LINE '172) 
(PUT 'GLEXADDTOMONBASE 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXADDTOMONBASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXADDTOMONBASE (MONOM VECT MONBASE)
    (PROG (X)
      (COND
       ((NULL GLEXEQSYS*)
        (PROGN (SETQ GLEXEQSYS* (A2VDP 0)) (SETQ GLEXCOUNT* (MINUS 1)))))
      (SETQ X (MKID 'GUNIVAR (SETQ GLEXCOUNT* (PLUS GLEXCOUNT* 1))))
      (SETQ GLEXEQSYS* (VDPSUM GLEXEQSYS* (VDPPROD (A2VDP X) (CDR VECT))))
      (SETQ GLEXSUB* (CONS (CONS X (CONS MONOM VECT)) GLEXSUB*))
      (SETQ GLEXVARS* (CONS X GLEXVARS*))
      (RETURN (CONS (CONS MONOM VECT) MONBASE)))) 
(PUT 'GLEXLINRELOLD 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXLINRELOLD 'DEFINED-ON-LINE '184) 
(PUT 'GLEXLINRELOLD 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXLINRELOLD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXLINRELOLD (MONOM VECT MONBASE)
    (COND
     (MONBASE
      (PROG (SYS SUB AUXVARS R V X N)
        (SETQ N 0)
        (SETQ V (CDR VECT))
        (PROG (B)
          (SETQ B (REVERSE MONBASE))
         LAB
          (COND ((NULL B) (RETURN NIL)))
          ((LAMBDA (B)
             (PROGN
              (SETQ X (MKID 'GUNIVAR N))
              (SETQ N (PLUS N 1))
              (SETQ V (VDPSUM V (VDPPROD (A2VDP X) (CDDR B))))
              (SETQ SUB (CONS (CONS X B) SUB))
              (SETQ AUXVARS (CONS X AUXVARS))))
           (CAR B))
          (SETQ B (CDR B))
          (GO LAB))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (NOT (OR (NULL V) (NULL (CADR (CDDR V)))))) (RETURN NIL)))
          (PROGN
           (SETQ SYS (CONS (DIP2F (CADR (CDDR (VDPFMON (CADDR V) NIL)))) SYS))
           (SETQ V (VDPRED V)))
          (GO WHILELABEL))
        (SETQ X SYS)
        (SETQ SYS (GROELINSOLVE SYS AUXVARS))
        (COND ((NULL SYS) (RETURN NIL)))
        (COND (*TRGROEB (PRIN2T "======= constructing new basis polynomial")))
        (SETQ R (CONS (DIP2F (CADR (CDDR (VDPPROD MONOM (CAR VECT))))) 1))
        (PROG (S)
          (SETQ S SUB)
         LAB
          (COND ((NULL S) (RETURN NIL)))
          ((LAMBDA (S)
             (SETQ R
                     (ADDSQ R
                            (MULTSQ
                             (CONS
                              (DIP2F
                               (CADR (CDDR (VDPPROD (CADR S) (CADDR S)))))
                              1)
                             (CDR (ASSOC (CAR S) SYS))))))
           (CAR S))
          (SETQ S (CDR S))
          (GO LAB))
        (SETQ R (DIP2F (CADR (CDDR (VDPSIMPCONT (F2VDP (CAR R)))))))
        (RETURN R))))) 
(PUT 'GLEXLINREL 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXLINREL 'DEFINED-ON-LINE '206) 
(PUT 'GLEXLINREL 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXLINREL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXLINREL (MONOM VECT MONBASE)
    (COND
     (MONBASE
      (PROG (SYS R V X)
        (SETQ V (VDPSUM (CDR VECT) GLEXEQSYS*))
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (NOT (OR (NULL V) (NULL (CADR (CDDR V)))))) (RETURN NIL)))
          (PROGN
           (SETQ SYS (CONS (DIP2F (CADR (CDDR (VDPFMON (CADDR V) NIL)))) SYS))
           (SETQ V (VDPRED V)))
          (GO WHILELABEL))
        (SETQ X SYS)
        (SETQ SYS (GROELINSOLVE SYS GLEXVARS*))
        (COND ((NULL SYS) (RETURN NIL)))
        (SETQ R (CONS (DIP2F (CADR (CDDR (VDPPROD MONOM (CAR VECT))))) 1))
        (PROG (S)
          (SETQ S GLEXSUB*)
         LAB
          (COND ((NULL S) (RETURN NIL)))
          ((LAMBDA (S)
             (SETQ R
                     (ADDSQ R
                            (MULTSQ
                             (CONS
                              (DIP2F
                               (CADR (CDDR (VDPPROD (CADR S) (CADDR S)))))
                              1)
                             (CDR (ASSOC (CAR S) SYS))))))
           (CAR S))
          (SETQ S (CDR S))
          (GO LAB))
        (SETQ R (DIP2F (CADR (CDDR (VDPSIMPCONT (F2VDP (CAR R)))))))
        (RETURN R))))) 
(PUT 'GLEXNORMALFORM 'NUMBER-OF-ARGS 2) 
(PUT 'GLEXNORMALFORM 'DEFINED-ON-LINE '221) 
(PUT 'GLEXNORMALFORM 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXNORMALFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GLEXNORMALFORM (M G)
    (PROG (COF VECT R F FAC1)
      (COND (*TRGROEB (PRIN2T "======= reducing ")))
      (SETQ FAC1 (VDPGETPROP M 'FACTOR))
      (COND (FAC1 (SETQ VECT (VDPGETPROP FAC1 'VECTOR))))
      (COND
       (VECT
        (PROGN
         (SETQ F (VDPPROD (CDR VECT) (VDPGETPROP M 'MONFAC)))
         (SETQ COF (CAR VECT))))
       (T (PROGN (SETQ F M) (SETQ COF (A2VDP 1)))))
      (SETQ R (GLEXNORMALFORM1 F G COF))
      (VDPPUTPROP M 'VECTOR R)
      (COND
       (*TRGROEB
        (PROGN
         (VDPPRINT (VDPPROD (CAR R) M))
         (PRIN2T "=====> ")
         (VDPPRINT (CDR R)))))
      (RETURN R))) 
(PUT 'GLEXNORMALFORM1 'NUMBER-OF-ARGS 3) 
(PUT 'GLEXNORMALFORM1 'DEFINED-ON-LINE '238) 
(PUT 'GLEXNORMALFORM1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GLEXNORMALFORM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GLEXNORMALFORM1 (F G COF)
    (PROG (F1 C VEV DIVISOR DONE FOLD A B)
      (SETQ FOLD F)
      (SETQ F1 (A2VDP 0))
      (SETQ A (A2VDP 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))) (RETURN NIL)))
        (PROG ()
          (SETQ VEV (CADR F))
          (SETQ C (CADDR F))
          (SETQ DIVISOR (GROEBSEARCHINLIST VEV G))
          (COND (DIVISOR (SETQ DONE T)))
          (COND
           (DIVISOR
            (COND
             (*VDPINTEGER
              (PROGN
               (SETQ F (GROEBREDUCEONESTEPINT F A C VEV DIVISOR))
               (SETQ B SECONDVALUE*)
               (SETQ COF (VDPPROD B COF))
               (COND
                ((NOT (OR (NULL F1) (NULL (CADR (CDDR F1)))))
                 (SETQ F1 (VDPPROD B F1))))))
             (T (SETQ F (GROEBREDUCEONESTEPRAT F NIL C VEV DIVISOR)))))
           (T
            (PROGN
             (SETQ F1 (VDPAPPENDMON F1 (CADDR F) (CADR F)))
             (SETQ F (VDPRED F))))))
        (GO WHILELABEL))
      (COND ((NOT DONE) (RETURN (CONS COF FOLD))))
      (SETQ F (GROEBSIMPCONT2 F1 COF))
      (SETQ COF SECONDVALUE*)
      (RETURN (CONS COF F)))) 
(PUT 'GROELINSOLVE 'NUMBER-OF-ARGS 2) 
(PUT 'GROELINSOLVE 'DEFINED-ON-LINE '255) 
(PUT 'GROELINSOLVE 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GROELINSOLVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROELINSOLVE (EQUATIONS XVARS)
    ((LAMBDA (*EZGCD)
       (PROG (R Q TEST OLDMOD OLDMODULUS)
         (COND (*TRGROEB (PRIN2T "======= testing linear dependency ")))
         (SETQ R T)
         (COND
          ((AND (NOT *MODULAR) (EQUAL GLEXDOMAIN* 1))
           ((LAMBDA (*EZGCD)
              (PROGN
               (SETQ OLDMOD DMODE*)
               (COND (OLDMOD (SETDMODE (GET OLDMOD 'DNAME) NIL)))
               (SETQ OLDMODULUS CURRENT-MODULUS)
               (SETMOD (LIST 16381))
               (SETDMODE 'MODULAR T)
               (SETQ R
                       (GROELINSOLVE1
                        (PROG (U FORALL-RESULT FORALL-ENDPTR)
                          (SETQ U EQUATIONS)
                          (COND ((NULL U) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (U) (CAR (SIMP (PREPF U))))
                                            (CAR U))
                                           NIL)))
                         LOOPLABEL
                          (SETQ U (CDR U))
                          (COND ((NULL U) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (U) (CAR (SIMP (PREPF U))))
                                    (CAR U))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        XVARS))
               (SETDMODE 'MODULAR NIL)
               (SETMOD (LIST OLDMODULUS))
               (COND (OLDMOD (SETDMODE (GET OLDMOD 'DNAME) T)))
               NIL))
            NIL)))
         (COND ((NULL R) (RETURN NIL)))
         (SETQ R (GROELINSOLVE1 EQUATIONS XVARS))
         (COND ((NULL R) (RETURN NIL)))
         (PROG (S)
           (SETQ S R)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S) (COND ((NOT (EQUAL (CDR (CDR S)) 1)) (SETQ TEST T))))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         (COND (TEST (RETURN R)))
         (SETQ Q (CAR (CDR (CAR R))))
         (RETURN R)))
     *EZGCD)) 
(PUT 'GROELINSOLVE1 'NUMBER-OF-ARGS 2) 
(PUT 'GROELINSOLVE1 'DEFINED-ON-LINE '285) 
(PUT 'GROELINSOLVE1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GROELINSOLVE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROELINSOLVE1 (EQUATIONS XVARS)
    (PROG (OLDORDER X P SOLUTIONS VAL LATER BREAK GC FIELD)
      (SETQ OLDORDER (SETKORDER XVARS))
      (SETQ FIELD (AND DMODE* (FLAGP DMODE* 'FIELD)))
      (SETQ EQUATIONS
              (PROG (EQA FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQA EQUATIONS)
                (COND ((NULL EQA) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EQA) (REORDER EQA)) (CAR EQA))
                                      NIL)))
               LOOPLABEL
                (SETQ EQA (CDR EQA))
                (COND ((NULL EQA) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EQA) (REORDER EQA)) (CAR EQA)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (EQA)
        (SETQ EQA EQUATIONS)
       LAB
        (COND ((NULL EQA) (RETURN NIL)))
        ((LAMBDA (EQA)
           (COND ((AND EQA (OR (ATOM EQA) (ATOM (CAR EQA)))) (SETQ BREAK T))))
         (CAR EQA))
        (SETQ EQA (CDR EQA))
        (GO LAB))
      (COND (BREAK (GO EMPTY)))
      (SETQ EQUATIONS (SORT EQUATIONS (FUNCTION GRLOELINORD)))
     AGAIN
      (SETQ BREAK NIL)
      (PROG (EQA)
        (SETQ EQA EQUATIONS)
       LAB
        (COND ((NULL EQA) (RETURN NIL)))
        ((LAMBDA (EQA)
           (COND
            ((NOT BREAK)
             (PROGN
              (COND ((NULL EQA) (SETQ EQUATIONS (DELETE EQA EQUATIONS)))
                    ((OR (ATOM EQA) (ATOM (CAR EQA))) (SETQ BREAK T))
                    ((NOT (MEMBER (CAAAR EQA) XVARS)) (SETQ BREAK T))
                    ((OR (OR (ATOM (CDR EQA)) (ATOM (CAR (CDR EQA))))
                         (NOT (MEMBER (CAAAR (CDR EQA)) XVARS)))
                     (PROGN
                      (SETQ EQUATIONS (DELETE EQA EQUATIONS))
                      (SETQ X (CAAAR EQA))
                      (SETQ VAL
                              (COND
                               ((EQUAL (CDAR EQA) 1) (CONS (NEGF (CDR EQA)) 1))
                               (T
                                (MULTSQ (CONS (NEGF (CDR EQA)) 1)
                                        (CONS 1 (CDAR EQA))))))
                      (SETQ SOLUTIONS (CONS (CONS X VAL) SOLUTIONS))
                      (SETQ EQUATIONS
                              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                (SETQ Q EQUATIONS)
                                (COND ((NULL Q) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q)
                                                    (GROELINSUB Q
                                                     (LIST (CONS X VAL))))
                                                  (CAR Q))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ Q (CDR Q))
                                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Q)
                                            (GROELINSUB Q (LIST (CONS X VAL))))
                                          (CAR Q))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ LATER
                              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                                (SETQ Q LATER)
                                (COND ((NULL Q) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (Q)
                                                    (GROELINSUB Q
                                                     (LIST (CONS X VAL))))
                                                  (CAR Q))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ Q (CDR Q))
                                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Q)
                                            (GROELINSUB Q (LIST (CONS X VAL))))
                                          (CAR Q))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                      (SETQ BREAK 0))))))))
         (CAR EQA))
        (SETQ EQA (CDR EQA))
        (GO LAB))
      (COND ((EQUAL BREAK 0) (GO AGAIN)) (BREAK (GO EMPTY)))
      (COND ((NULL EQUATIONS) (GO READY)))
      (SETQ EQUATIONS (SORT EQUATIONS (FUNCTION GRLOELINORD)))
      (SETQ P (CAR EQUATIONS))
      (SETQ X (CAAAR P))
      (SETQ EQUATIONS
              (PROG (EQA FORALL-RESULT FORALL-ENDPTR)
                (SETQ EQA (CDR EQUATIONS))
                (COND ((NULL EQA) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EQA)
                                    (COND
                                     ((EQUAL (CAAAR EQA) X)
                                      (PROGN
                                       (COND
                                        (FIELD
                                         (SETQ EQA
                                                 (ADDF EQA
                                                       (NEGF
                                                        ((LAMBDA (G125)
                                                           (COND
                                                            (*PHYSOP-LOADED
                                                             (PHYSOP-MULTF G125
                                                              P))
                                                            (T
                                                             (POLY-MULTF G125
                                                                         P))))
                                                         ((LAMBDA (*EXP)
                                                            (QUOTF1 (CDAR EQA)
                                                                    (CDAR P)))
                                                          T))))))
                                        (T
                                         (PROGN
                                          (SETQ GC (GCDF (CDAR P) (CDAR EQA)))
                                          (SETQ EQA
                                                  (ADDF
                                                   ((LAMBDA (G127)
                                                      (COND
                                                       (*PHYSOP-LOADED
                                                        (PHYSOP-MULTF G127
                                                         EQA))
                                                       (T
                                                        (POLY-MULTF G127
                                                                    EQA))))
                                                    (QUOTF-FAIL (CDAR P) GC))
                                                   (NEGF
                                                    ((LAMBDA (G129)
                                                       (COND
                                                        (*PHYSOP-LOADED
                                                         (PHYSOP-MULTF G129 P))
                                                        (T
                                                         (POLY-MULTF G129 P))))
                                                     (QUOTF-FAIL (CDAR EQA)
                                                                 GC))))))))
                                       (COND
                                        ((NOT (OR (ATOM EQA) (ATOM (CAR EQA))))
                                         (SETQ EQA
                                                 (CAR
                                                  (MULTSQ (CONS EQA 1)
                                                          (CONS 1
                                                                (CDAR
                                                                 EQA)))))))
                                       EQA))
                                     (T EQA)))
                                  (CAR EQA))
                                 NIL)))
               LOOPLABEL
                (SETQ EQA (CDR EQA))
                (COND ((NULL EQA) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EQA)
                            (COND
                             ((EQUAL (CAAAR EQA) X)
                              (PROGN
                               (COND
                                (FIELD
                                 (SETQ EQA
                                         (ADDF EQA
                                               (NEGF
                                                ((LAMBDA (G125)
                                                   (COND
                                                    (*PHYSOP-LOADED
                                                     (PHYSOP-MULTF G125 P))
                                                    (T (POLY-MULTF G125 P))))
                                                 ((LAMBDA (*EXP)
                                                    (QUOTF1 (CDAR EQA)
                                                            (CDAR P)))
                                                  T))))))
                                (T
                                 (PROGN
                                  (SETQ GC (GCDF (CDAR P) (CDAR EQA)))
                                  (SETQ EQA
                                          (ADDF
                                           ((LAMBDA (G127)
                                              (COND
                                               (*PHYSOP-LOADED
                                                (PHYSOP-MULTF G127 EQA))
                                               (T (POLY-MULTF G127 EQA))))
                                            (QUOTF-FAIL (CDAR P) GC))
                                           (NEGF
                                            ((LAMBDA (G129)
                                               (COND
                                                (*PHYSOP-LOADED
                                                 (PHYSOP-MULTF G129 P))
                                                (T (POLY-MULTF G129 P))))
                                             (QUOTF-FAIL (CDAR EQA) GC))))))))
                               (COND
                                ((NOT (OR (ATOM EQA) (ATOM (CAR EQA))))
                                 (SETQ EQA
                                         (CAR
                                          (MULTSQ (CONS EQA 1)
                                                  (CONS 1 (CDAR EQA)))))))
                               EQA))
                             (T EQA)))
                          (CAR EQA))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LATER (CONS P LATER))
      (GO AGAIN)
     READY
      (PROG ()
       WHILELABEL
        (COND ((NOT LATER) (RETURN NIL)))
        (PROGN
         (SETQ P (CAR LATER))
         (SETQ LATER (CDR LATER))
         (SETQ P (GROELINSUB P SOLUTIONS))
         (COND
          ((OR (OR (ATOM P) (ATOM (CAR P))) (NOT (MEMBER (CAAAR P) XVARS))
               (AND (NOT (OR (ATOM (CDR P)) (ATOM (CAR (CDR P)))))
                    (MEMBER (CAAAR (CDR P)) XVARS)))
           (PROGN (SETQ BREAK T) (SETQ LATER NIL))))
         (SETQ X (CAAAR P))
         (SETQ VAL
                 (COND ((EQUAL (CDAR P) 1) (CONS (NEGF (CDR P)) 1))
                       (T
                        (MULTSQ (CONS (NEGF (CDR P)) 1)
                                (INVSQ (CONS (CDAR P) 1))))))
         (SETQ SOLUTIONS (CONS (CONS X VAL) SOLUTIONS)))
        (GO WHILELABEL))
      (COND (BREAK (GO EMPTY)) (T (GO FINIS)))
     EMPTY
      (SETQ SOLUTIONS NIL)
     FINIS
      (SETKORDER OLDORDER)
      (SETQ SOLUTIONS
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S SOLUTIONS)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS (CAR S)
                                          (CONS (REORDER (CAR (CDR S)))
                                                (REORDER (CDR (CDR S))))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (CONS (CAR S)
                                  (CONS (REORDER (CAR (CDR S)))
                                        (REORDER (CDR (CDR S))))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN SOLUTIONS))) 
(PUT 'GRLOELINORD 'NUMBER-OF-ARGS 2) 
(PUT 'GRLOELINORD 'DEFINED-ON-LINE '354) 
(PUT 'GRLOELINORD 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GRLOELINORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GRLOELINORD (U V) (ORDOP (CAAAR U) (CAAAR V))) 
(PUT 'GROELINSUB 'NUMBER-OF-ARGS 2) 
(PUT 'GROELINSUB 'DEFINED-ON-LINE '373) 
(PUT 'GROELINSUB 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GROELINSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROELINSUB (S A) (CAR (GROELINSUB1 S A))) 
(PUT 'GROELINSUB1 'NUMBER-OF-ARGS 2) 
(PUT 'GROELINSUB1 'DEFINED-ON-LINE '380) 
(PUT 'GROELINSUB1 'DEFINED-IN-FILE 'GROEBNER/GLEXCONV.RED) 
(PUT 'GROELINSUB1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROELINSUB1 (S A)
    (COND ((OR (ATOM S) (ATOM (CAR S))) (CONS S 1))
          (T
           ((LAMBDA (X Y)
              (COND (X (ADDSQ (MULTSQ (CDR X) (CONS (CDAR S) 1)) Y))
                    (T (ADDSQ (CONS (CONS (CAR S) NIL) 1) Y))))
            (ASSOC (CAAAR S) A) (GROELINSUB1 (CDR S) A))))) 
(ENDMODULE) 