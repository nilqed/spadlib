(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBTRA)) 
(SWITCH
 (LIST 'GROEBOPT 'GROEBFAC 'TRGROEB 'TRGROEBS 'TRGROEB1 'TRGROEBR 'GROEBSTAT
       'GROEBPROT)) 
(PUT 'GROEBNERTRAEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNERTRAEVAL 'DEFINED-ON-LINE '40) 
(PUT 'GROEBNERTRAEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBNERTRAEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNERTRAEVAL (U)
    (PROG (N *GROEBFAC *GROEBRM *GROEBPROT *GSUGAR)
      (SETQ N 0)
      (SETQ N (LENGTH U))
      (COND ((EQUAL N 1) (RETURN (GROEBNERTRA1 (REVAL1 (CAR U) T) NIL NIL)))
            ((NEQ N 2)
             (RERROR 'GROEBNR2 10
                     "groebnert called with wrong number of arguments"))
            (T
             (RETURN
              (GROEBNERTRA1 (REVAL1 (CAR U) T) (REVAL1 (CADR U) T) NIL)))))) 
(PUT 'GROEBNERT 'PSOPFN 'GROEBNERTRAEVAL) 
(PUT 'GROEBNERTRA1 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBNERTRA1 'DEFINED-ON-LINE '50) 
(PUT 'GROEBNERTRA1 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBNERTRA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBNERTRA1 (U V MOD1)
    (PROG (VARS W Y Z X NP OLDORDER GROETAGS* TAGVARS PCOUNT* NMOD)
      (SETQ PCOUNT* 0)
      (SETQ NMOD 0)
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GETRLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (COND
                                      ((OR (NOT (EQCAR J 'EQUAL))
                                           (NOT
                                            (OR (IDP (SETQ W (CADR J)))
                                                (AND (PAIRP W)
                                                     (EQUAL W (REVAL1 W T))
                                                     (EQUAL
                                                      (GET (CAR W) 'SIMPFN)
                                                      'SIMPIDEN)))))
                                       (RERROR 'GROEBNR2 11
                                               "groebnert parameter not {...,name=polynomial,...}"))
                                      (T (CONS (CADR J) (CADDR J))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (COND
                              ((OR (NOT (EQCAR J 'EQUAL))
                                   (NOT
                                    (OR (IDP (SETQ W (CADR J)))
                                        (AND (PAIRP W) (EQUAL W (REVAL1 W T))
                                             (EQUAL (GET (CAR W) 'SIMPFN)
                                                    'SIMPIDEN)))))
                               (RERROR 'GROEBNR2 11
                                       "groebnert parameter not {...,name=polynomial,...}"))
                              (T (CONS (CADR J) (CADDR J))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL U) (RERROR 'GROEBNR2 12 "empty list in groebner"))
            ((NULL (CDR U))
             (RETURN (CONS 'LIST (LIST 'EQUAL (CDAR U) (CAAR U))))))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (MOD1
        (PROGN
         (SETQ Z NIL)
         (PROG (VECT)
           (SETQ VECT W)
          LAB
           (COND ((NULL VECT) (RETURN NIL)))
           ((LAMBDA (VECT)
              (PROGN
               (COND
                ((NOT (EQCAR VECT 'LIST))
                 (RERROR 'GROEBNR2 13 "non list given to groebner")))
               (COND ((EQUAL NMOD 0) (SETQ NMOD (LENGTH (CDR VECT))))
                     ((LESSP NMOD (SETQ X (LENGTH (CDR VECT))))
                      (EQUAL NMOD X)))
               (SETQ Z (APPEND (CDR VECT) Z))))
            (CAR VECT))
           (SETQ VECT (CDR VECT))
           (GO LAB))
         (COND
          ((NOT (EQCAR MOD1 'LIST))
           (RERROR 'GROEBNR2 14 "illegal column weights specified")))
         (SETQ VARS (GROEBNERVARS Z V))
         (SETQ TAGVARS
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                   (COND ((MINUSP (DIFFERENCE NMOD I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR (CONS (MKID '| COL| I) NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND ((MINUSP (DIFFERENCE NMOD I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS (MKID '| COL| I) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ W
                 (PROG (VECT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ VECT W)
                   (COND ((NULL VECT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (VECT)
                                       (PROGN
                                        (SETQ Z TAGVARS)
                                        (SETQ Y (CDR MOD1))
                                        (CONS 'PLUS
                                              (PROG (P FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ P (CDR VECT))
                                                (COND ((NULL P) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (P)
                                                                    (CONS
                                                                     'TIMES
                                                                     (CONS
                                                                      (LIST
                                                                       'EXPT
                                                                       (CAR Z)
                                                                       (CAR Y))
                                                                      (PROGN
                                                                       (SETQ Z
                                                                               (CDR
                                                                                Z))
                                                                       (SETQ Y
                                                                               (CDR
                                                                                Y))
                                                                       (COND
                                                                        ((NULL
                                                                          Y)
                                                                         (SETQ Y
                                                                                 '(1))))
                                                                       (LIST
                                                                        P)))))
                                                                  (CAR P))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ P (CDR P))
                                                (COND
                                                 ((NULL P)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (P)
                                                            (CONS 'TIMES
                                                                  (CONS
                                                                   (LIST 'EXPT
                                                                         (CAR
                                                                          Z)
                                                                         (CAR
                                                                          Y))
                                                                   (PROGN
                                                                    (SETQ Z
                                                                            (CDR
                                                                             Z))
                                                                    (SETQ Y
                                                                            (CDR
                                                                             Y))
                                                                    (COND
                                                                     ((NULL Y)
                                                                      (SETQ Y
                                                                              '(1))))
                                                                    (LIST
                                                                     P)))))
                                                          (CAR P))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL)))))
                                     (CAR VECT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ VECT (CDR VECT))
                   (COND ((NULL VECT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (VECT)
                               (PROGN
                                (SETQ Z TAGVARS)
                                (SETQ Y (CDR MOD1))
                                (CONS 'PLUS
                                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ P (CDR VECT))
                                        (COND ((NULL P) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (P)
                                                            (CONS 'TIMES
                                                                  (CONS
                                                                   (LIST 'EXPT
                                                                         (CAR
                                                                          Z)
                                                                         (CAR
                                                                          Y))
                                                                   (PROGN
                                                                    (SETQ Z
                                                                            (CDR
                                                                             Z))
                                                                    (SETQ Y
                                                                            (CDR
                                                                             Y))
                                                                    (COND
                                                                     ((NULL Y)
                                                                      (SETQ Y
                                                                              '(1))))
                                                                    (LIST
                                                                     P)))))
                                                          (CAR P))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ P (CDR P))
                                        (COND
                                         ((NULL P) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (P)
                                                    (CONS 'TIMES
                                                          (CONS
                                                           (LIST 'EXPT (CAR Z)
                                                                 (CAR Y))
                                                           (PROGN
                                                            (SETQ Z (CDR Z))
                                                            (SETQ Y (CDR Y))
                                                            (COND
                                                             ((NULL Y)
                                                              (SETQ Y '(1))))
                                                            (LIST P)))))
                                                  (CAR P))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL)))))
                             (CAR VECT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ GROETAGS* (PLUS (LENGTH VARS) 1))
         (SETQ VARS (APPEND VARS TAGVARS))))
       (T (SETQ VARS (GROEBNERVARS W V))))
      (GROEDOMAINMODE)
      (COND ((NULL VARS) (VDPERR 'GROEBNER)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ U
              (PAIR
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X U)
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               W))
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (PROGN
                                     (SETQ Z (SIMP (CDR X)))
                                     (CONS
                                      (MULTSQ (SIMP (CAR X)) (CONS (CDR Z) 1))
                                      (REORDER (CAR Z)))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROGN
                             (SETQ Z (SIMP (CDR X)))
                             (CONS (MULTSQ (SIMP (CAR X)) (CONS (CDR Z) 1))
                                   (REORDER (CAR Z)))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
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
              (PAIR
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X U)
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CAR X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               W))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ Z (F2VDP (CDR J)))
                                     (VDPPUTPROP Z 'COFACT (CAR J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ Z (F2VDP (CDR J)))
                             (VDPPUTPROP Z 'COFACT (CAR J))))
                          (CAR J))
                         NIL))
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
      (SETQ W (GROEBTRA2 W))
      (SETQ W
              (COND (MOD1 (GROEBNERMODRES W NMOD TAGVARS))
                    (T (GROEBNERTRARES W))))
      (SETKORDER OLDORDER)
      (SETQ GVARSLAST (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST VARS)))
      (RETURN W))) 
(PUT 'GROEBNERTRARES 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNERTRARES 'DEFINED-ON-LINE '113) 
(PUT 'GROEBNERTRARES 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBNERTRARES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNERTRARES (W)
    (PROG (C U)
      (RETURN
       (CONS 'LIST
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J W)
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (PROGN
                                    (SETQ C (PREPSQ (VDPGETPROP J 'COFACT)))
                                    (SETQ U (DIP2A (CADR (CDDR J))))
                                    (COND ((EQUAL C 0) U)
                                          (T (LIST 'EQUAL U C)))))
                                 (CAR J))
                                NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (J)
                           (PROGN
                            (SETQ C (PREPSQ (VDPGETPROP J 'COFACT)))
                            (SETQ U (DIP2A (CADR (CDDR J))))
                            (COND ((EQUAL C 0) U) (T (LIST 'EQUAL U C)))))
                         (CAR J))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'GROEBNERMODRES 'NUMBER-OF-ARGS 3) 
(PUT 'GROEBNERMODRES 'DEFINED-ON-LINE '119) 
(PUT 'GROEBNERMODRES 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBNERMODRES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBNERMODRES (W N TAGVARS)
    (PROG (X C OLDORDER)
      (SETQ C
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U W)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (PREPSQ (VDPGETPROP U 'COFACT)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U) (PREPSQ (VDPGETPROP U 'COFACT))) (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ OLDORDER (SETKORDER TAGVARS))
      (SETQ W
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U W)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (CONS 'LIST
                                          (PROGN
                                           (SETQ U
                                                   (CAR
                                                    (SIMP
                                                     (DIP2A (CADR (CDDR U))))))
                                           (PROG (I FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ I 1)
                                             (COND
                                              ((MINUSP (DIFFERENCE N I))
                                               (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              (PREPF
                                                               (COND
                                                                ((AND
                                                                  (NOT
                                                                   (OR (ATOM U)
                                                                       (ATOM
                                                                        (CAR
                                                                         U))))
                                                                  (EQUAL
                                                                   (CAAAR U)
                                                                   (NTH TAGVARS
                                                                        I)))
                                                                 (PROGN
                                                                  (SETQ X
                                                                          (CDAR
                                                                           U))
                                                                  (SETQ U
                                                                          (CDR
                                                                           U))
                                                                  X))
                                                                (T NIL)))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ I (PLUS2 I 1))
                                             (COND
                                              ((MINUSP (DIFFERENCE N I))
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      (PREPF
                                                       (COND
                                                        ((AND
                                                          (NOT
                                                           (OR (ATOM U)
                                                               (ATOM (CAR U))))
                                                          (EQUAL (CAAAR U)
                                                                 (NTH TAGVARS
                                                                      I)))
                                                         (PROGN
                                                          (SETQ X (CDAR U))
                                                          (SETQ U (CDR U))
                                                          X))
                                                        (T NIL)))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (CONS 'LIST
                                  (PROGN
                                   (SETQ U
                                           (CAR
                                            (SIMP (DIP2A (CADR (CDDR U))))))
                                   (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ I 1)
                                     (COND
                                      ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      (PREPF
                                                       (COND
                                                        ((AND
                                                          (NOT
                                                           (OR (ATOM U)
                                                               (ATOM (CAR U))))
                                                          (EQUAL (CAAAR U)
                                                                 (NTH TAGVARS
                                                                      I)))
                                                         (PROGN
                                                          (SETQ X (CDAR U))
                                                          (SETQ U (CDR U))
                                                          X))
                                                        (T NIL)))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ I (PLUS2 I 1))
                                     (COND
                                      ((MINUSP (DIFFERENCE N I))
                                       (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              (PREPF
                                               (COND
                                                ((AND
                                                  (NOT
                                                   (OR (ATOM U)
                                                       (ATOM (CAR U))))
                                                  (EQUAL (CAAAR U)
                                                         (NTH TAGVARS I)))
                                                 (PROGN
                                                  (SETQ X (CDAR U))
                                                  (SETQ U (CDR U))
                                                  X))
                                                (T NIL)))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETKORDER OLDORDER)
      (SETQ W
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U W)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U) (DIP2A (CADR (CDDR (A2VDP U)))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U) (DIP2A (CADR (CDDR (A2VDP U))))) (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (PAIR W C))
      (RETURN
       (CONS 'LIST
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P W)
               (COND ((NULL P) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P)
                                   (COND ((EQUAL (CDR P) 0) (CAR P))
                                         (T (LIST 'EQUAL (CAR P) (CDR P)))))
                                 (CAR P))
                                NIL)))
              LOOPLABEL
               (SETQ P (CDR P))
               (COND ((NULL P) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P)
                           (COND ((EQUAL (CDR P) 0) (CAR P))
                                 (T (LIST 'EQUAL (CAR P) (CDR P)))))
                         (CAR P))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'PREDUCETEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'PREDUCETEVAL 'DEFINED-ON-LINE '136) 
(PUT 'PREDUCETEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'PREDUCETEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PREDUCETEVAL (PARS)
    (PROG (VARS X Y U V W Z OLDORDER *FACTOR *EXP *GSUGAR PCOUNT*)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (SETQ PARS (GROEPARAMS PARS 2 3))
      (SETQ Y (CAR PARS))
      (SETQ U (CADR PARS))
      (SETQ V (CADDR PARS))
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GETRLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (COND ((NOT (EQCAR J 'EQUAL)) (CONS J J))
                                           (T (CONS (CADR J) (CADDR J))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (COND ((NOT (EQCAR J 'EQUAL)) (CONS J J))
                                   (T (CONS (CADR J) (CADDR J))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL U) (RERROR 'GROEBNR2 15 "empty list in preducet")))
      (SETQ W
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P U)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (GROEDOMAINMODE)
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
               (T (GETRLIST V))))
      (COND ((NOT VARS) (VDPERR 'PREDUCET)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (PROGN
                                     (SETQ Z (SIMP (CDR X)))
                                     (CONS
                                      (MULTSQ (SIMP (CAR X)) (CONS (CDR Z) 1))
                                      (REORDER (CAR Z)))))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X)
                            (PROGN
                             (SETQ Z (SIMP (CDR X)))
                             (CONS (MULTSQ (SIMP (CAR X)) (CONS (CDR Z) 1))
                                   (REORDER (CAR Z)))))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ Z (F2VDP (CDR J)))
                                     (VDPPUTPROP Z 'COFACT (CAR J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ Z (F2VDP (CDR J)))
                             (VDPPUTPROP Z 'COFACT (CAR J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NOT (EQCAR Y 'EQUAL)) (SETQ Y (LIST 'EQUAL Y Y))))
      (SETQ X (A2VDP (CADDR Y)))
      (VDPPUTPROP X 'COFACT (SIMP (CADR Y)))
      (SETQ W (TRANORMALFORM X W 'SORT 'F))
      (SETQ U
              (LIST 'EQUAL (DIP2A (CADR (CDDR W)))
                    (PREPSQ (VDPGETPROP W 'COFACT))))
      (SETKORDER OLDORDER)
      (RETURN U))) 
(PUT 'PREDUCET 'PSOPFN 'PREDUCETEVAL) 
(PUT 'GROEBNERMODEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBNERMODEVAL 'DEFINED-ON-LINE '172) 
(PUT 'GROEBNERMODEVAL 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBNERMODEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBNERMODEVAL (U)
    ((LAMBDA (N)
       (COND
        ((OR (EQUAL N 0) (GREATERP N 3))
         (RERROR 'GROEBNR2 16
                 "groebnerm called with wrong number of arguments"))
        (T
         (GROEBNERTRA1 (REVAL1 (CAR U) T)
          (COND ((GEQ N 2) (REVAL1 (CADR U) T)) (T NIL))
          (COND ((GEQ N 3) (REVAL1 (CADDR U) T)) (T '(LIST 1)))))))
     (LENGTH U))) 
(PUT 'GROEBNERM 'PSOPFN 'GROEBNERMODEVAL) 
(PUT 'GROEBTRA2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBTRA2 'DEFINED-ON-LINE '184) 
(PUT 'GROEBTRA2 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBTRA2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBTRA2 (P)
    (PROG (GROETIME* TIM1 SPAC SPAC1 P1 PAIRSDONE* FACTORLVEVEL* FACTORTIME*)
      (SETQ FACTORTIME* 0)
      (SETQ GROETIME* (TIME))
      (VDPONEPOL)
      (SETQ HCOUNT*
              (SETQ PCOUNT*
                      (SETQ MCOUNT*
                              (SETQ FCOUNT*
                                      (SETQ BCOUNT*
                                              (SETQ B4COUNT*
                                                      (SETQ HZEROCOUNT*
                                                              (SETQ BASECOUNT*
                                                                      0))))))))
      (COND
       (*TRGROEB
        (PROGN
         (PRIN2 "Groebner calculation with backtracking starts ")
         (TERPRIT 2))))
      (SETQ SPAC (GCTIME))
      (SETQ P1 (GROEBTRA3 P))
      (COND
       ((OR *TRGROEB *TRGROEBR *GROEBSTAT)
        (PROGN
         (SETQ SPAC1 (DIFFERENCE (GCTIME) SPAC))
         (TERPRI)
         (PRIN2T "statistics for Groebner calculation")
         (PRIN2T "===================================")
         (PRIN2 " total computing time(including gc): ")
         (PRIN2 (DIFFERENCE (SETQ TIM1 (TIME)) GROETIME*))
         (PRIN2T "          milliseconds  ")
         (PRIN2 "(time spent for garbage collection:  ")
         (PRIN2 SPAC1)
         (PRIN2T "          milliseconds)")
         (TERPRIT 1)
         (PRIN2 "H-polynomials total: ")
         (PRIN2T HCOUNT*)
         (PRIN2 "H-polynomials zero : ")
         (PRIN2T HZEROCOUNT*)
         (PRIN2 "Crit M hits: ")
         (PRIN2T MCOUNT*)
         (PRIN2 "Crit F hits: ")
         (PRIN2T FCOUNT*)
         (PRIN2 "Crit B hits: ")
         (PRIN2T BCOUNT*)
         (PRIN2 "Crit B4 hits: ")
         (PRIN2T B4COUNT*))))
      (RETURN P1))) 
(PUT 'GROEBTRA3 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBTRA3 'DEFINED-ON-LINE '214) 
(PUT 'GROEBTRA3 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBTRA3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBTRA3 (G0)
    (PROG (X G D D1 D2 P P1 S H G99 ONE)
      (SETQ X
              (PROG (FJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FJ G0)
                (COND ((NULL FJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FJ) (VDPENUMERATE (TRASIMPCONT FJ)))
                                  (CAR FJ))
                                 NIL)))
               LOOPLABEL
                (SETQ FJ (CDR FJ))
                (COND ((NULL FJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FJ) (VDPENUMERATE (TRASIMPCONT FJ)))
                          (CAR FJ))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (FJ)
        (SETQ FJ X)
       LAB
        (COND ((NULL FJ) (RETURN NIL)))
        ((LAMBDA (FJ) (SETQ G (VDPLSORTIN FJ G0))) (CAR FJ))
        (SETQ FJ (CDR FJ))
        (GO LAB))
      (SETQ G0 G)
      (SETQ G NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (OR D G0) (NOT ONE))) (RETURN NIL)))
        (PROG ()
          (COND
           (G0
            (PROGN
             (SETQ H (CAR G0))
             (SETQ G0 (CDR G0))
             (SETQ P (LIST NIL H H))))
           (T
            (PROGN
             (SETQ P (CAR D))
             (SETQ D (CDR D))
             (SETQ S (TRASPOLYNOM (CADR P) (CADDR P)))
             (TRAMESS3 P S)
             (SETQ H (GROEBNORMALFORM S G99 'TREE))
             (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (GROEBMESS4 P D))
                   (T
                    (SETQ H (TRASIMPCONT (TRANORMALFORM S G99 'TREE 'H))))))))
          (COND ((OR (NULL H) (NULL (CADR (CDDR H)))) (GO BOTT)))
          (COND
           ((OR (NULL (CADR H))
                (AND (EQUAL (CAR (CADR H)) 0) (VEVZERO?1 (CDR (CADR H)))))
            (PROGN
             (TRAMESS5 P H)
             (SETQ G0 (SETQ D NIL))
             (SETQ G (LIST H))
             (GO BOTT))))
          (SETQ S NIL)
          (SETQ H (VDPENUMERATE H))
          (TRAMESS5 P H)
          (SETQ D1 NIL)
          (PROG (F)
            (SETQ F G)
           LAB
            (COND ((NULL F) (RETURN NIL)))
            ((LAMBDA (F)
               (COND
                ((GROEBMODUCRIT F H)
                 (PROGN
                  (SETQ D1
                          (GROEBCPLISTSORTIN
                           (LIST (VEVLCM (CADR F) (CADR H)) F H) D1))
                  (COND
                   ((EQUAL (VEVLCM (CADR F) (CADR H)) (CADR F))
                    (PROGN (SETQ G (DELETE F G)) (GROEBMESS2 F))))))))
             (CAR F))
            (SETQ F (CDR F))
            (GO LAB))
          (GROEBMESS51 D1)
          (SETQ D2 NIL)
          (PROG ()
           WHILELABEL
            (COND ((NOT D1) (RETURN NIL)))
            (PROGN
             (SETQ D1 (GROEBINVOKECRITF D1))
             (SETQ P1 (CAR D1))
             (SETQ D1 (CDR D1))
             (COND
              ((GROEBBUCHCRIT4 (CADR P1) (CADDR P1) (CAR P1))
               (SETQ D2 (APPEND D2 (LIST P1)))))
             (SETQ D1 (GROEBINVOKECRITM P1 D1)))
            (GO WHILELABEL))
          (SETQ D (GROEBINVOKECRITB H D))
          (SETQ D (GROEBCPLISTMERGE D D2))
          (SETQ G (CONS H G))
          (SETQ G99 (GROEBSTREEADD H G99))
          (GROEBMESS8 G D)
         BOTT)
        (GO WHILELABEL))
      (RETURN (GROEBTRA3POST G)))) 
(PUT 'GROEBTRA3POST 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBTRA3POST 'DEFINED-ON-LINE '263) 
(PUT 'GROEBTRA3POST 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBTRA3POST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBTRA3POST (G)
    (PROG (R P)
      (SETQ G (VDPLSORT G))
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ P (TRANORMALFORM (CAR G) (CDR G) 'SORT 'F))
         (COND
          ((NOT (OR (NULL P) (NULL (CADR (CDDR P)))))
           (SETQ R (CONS (TRASIMPCONT P) R))))
         (SETQ G (CDR G)))
        (GO WHILELABEL))
      (RETURN (REVERSIP R)))) 
(PUT 'TRANORMALFORM 'NUMBER-OF-ARGS 4) 
(PUT 'TRANORMALFORM 'DEFINED-ON-LINE '277) 
(PUT 'TRANORMALFORM 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRANORMALFORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANORMALFORM (F G TYPE MODE)
    (PROG (C VEV DIVISOR BREAK)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (NOT (OR (NULL F) (NULL (CADR (CDDR F))))) (NOT BREAK)))
          (RETURN NIL)))
        (PROG ()
          (SETQ VEV (CADR F))
          (SETQ C (CADDR F))
          (SETQ DIVISOR (GROEBSEARCHINLIST VEV G))
          (COND
           ((AND DIVISOR *TRGROEBS)
            (PROGN (PRIN2 "//-") (PRIN2 (VDPGETPROP DIVISOR 'NUMBER)))))
          (COND
           (DIVISOR
            (COND
             (*VDPINTEGER (SETQ F (TRAREDUCEONESTEPINT F NIL C VEV DIVISOR)))
             (T (SETQ F (TRAREDUCEONESTEPRAT F NIL C VEV DIVISOR)))))
           (T (SETQ BREAK T))))
        (GO WHILELABEL))
      (COND ((EQUAL MODE 'F) (SETQ F (TRANORMALFORM1 F G TYPE MODE))))
      (RETURN F))) 
(PUT 'TRANORMALFORM1 'NUMBER-OF-ARGS 4) 
(PUT 'TRANORMALFORM1 'DEFINED-ON-LINE '301) 
(PUT 'TRANORMALFORM1 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRANORMALFORM1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRANORMALFORM1 (F G TYPE MODE)
    (PROG (C VEV DIVISOR BREAK F1)
      (SETQ MODE NIL)
      (SETQ F1 F)
      (SETQ TYPE NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (OR (NULL F) (NULL (CADR (CDDR F)))))
                (NOT (OR (NULL F1) (NULL (CADR (CDDR F1)))))))
          (RETURN NIL)))
        (PROGN
         (SETQ F1 F)
         (SETQ BREAK NIL)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND (NOT (OR (NULL F1) (NULL (CADR (CDDR F1))))) (NOT BREAK)))
             (RETURN NIL)))
           (PROGN
            (SETQ VEV (CADR F1))
            (SETQ C (CADDR F1))
            (SETQ F1 (VDPRED F1))
            (SETQ DIVISOR (GROEBSEARCHINLIST VEV G))
            (COND
             ((AND DIVISOR *TRGROEBS)
              (PROGN (PRIN2 "//-") (PRIN2 (VDPGETPROP DIVISOR 'NUMBER)))))
            (COND
             (DIVISOR
              (PROGN
               (COND
                (*VDPINTEGER
                 (SETQ F (TRAREDUCEONESTEPINT F NIL C VEV DIVISOR)))
                (T (SETQ F (TRAREDUCEONESTEPRAT F NIL C VEV DIVISOR))))
               (SETQ BREAK T)))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN F))) 
(PUT 'TRAREDUCEONESTEPINT 'NUMBER-OF-ARGS 5) 
(PUT 'TRAREDUCEONESTEPINT 'DEFINED-ON-LINE '324) 
(PUT 'TRAREDUCEONESTEPINT 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAREDUCEONESTEPINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRAREDUCEONESTEPINT (F DUMMY C VEV G1)
    (PROG (VEVLCM A B CG X FCOFA GCOFA)
      (SETQ DUMMY NIL)
      (SETQ FCOFA (VDPGETPROP F 'COFACT))
      (COND ((NULL FCOFA) (SETQ FCOFA (CONS NIL 1))))
      (SETQ GCOFA (VDPGETPROP G1 'COFACT))
      (COND ((NULL GCOFA) (SETQ GCOFA (CONS NIL 1))))
      (SETQ VEVLCM (VEVDIF VEV (CADR G1)))
      (SETQ CG (CADDR G1))
      (SETQ X (VBCGCD C CG))
      (SETQ A (BCQUOT CG X))
      (SETQ B (BCQUOT C X))
      (SETQ F
              (VDPILCOMB1 F A (VEVMAPTOZERO1 VDPVARS* NIL) G1 (BCNEG B)
                          VEVLCM))
      (SETQ X
              (VDPILCOMB1TRA FCOFA A (VEVMAPTOZERO1 VDPVARS* NIL) GCOFA
               (BCNEG B) VEVLCM))
      (VDPPUTPROP F 'COFACT X)
      (RETURN F))) 
(PUT 'TRAREDUCEONESTEPRAT 'NUMBER-OF-ARGS 5) 
(PUT 'TRAREDUCEONESTEPRAT 'DEFINED-ON-LINE '343) 
(PUT 'TRAREDUCEONESTEPRAT 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAREDUCEONESTEPRAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRAREDUCEONESTEPRAT (F DUMMY C VEV G1)
    (PROG (X FCOFA GCOFA)
      (SETQ DUMMY NIL)
      (SETQ FCOFA (VDPGETPROP F 'COFACT))
      (SETQ GCOFA (VDPGETPROP G1 'COFACT))
      (SETQ VEV (VEVDIF VEV (CADR G1)))
      (SETQ X (BCNEG (BCQUOT C (CADDR G1))))
      (SETQ F (VDPILCOMB1 F (A2BC 1) (VEVMAPTOZERO1 VDPVARS* NIL) G1 X VEV))
      (SETQ X
              (VDPILCOMB1TRA FCOFA (A2BC 1) (VEVMAPTOZERO1 VDPVARS* NIL) GCOFA
               X VEV))
      (VDPPUTPROP F 'COFACT X)
      (RETURN F))) 
(PUT 'TRASPOLYNOM 'NUMBER-OF-ARGS 2) 
(PUT 'TRASPOLYNOM 'DEFINED-ON-LINE '361) 
(PUT 'TRASPOLYNOM 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRASPOLYNOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRASPOLYNOM (P1 P2)
    (PROG (S EP1 EP2 EP RP1 RP2 DB1 DB2 X COFAC1 COFAC2)
      (COND ((OR (NULL P1) (NULL (CADR (CDDR P1)))) (RETURN P1)))
      (COND ((OR (NULL P1) (NULL (CADR (CDDR P1)))) (RETURN P2)))
      (SETQ COFAC1 (VDPGETPROP P1 'COFACT))
      (SETQ COFAC2 (VDPGETPROP P2 'COFACT))
      (SETQ EP1 (CADR P1))
      (SETQ EP2 (CADR P2))
      (SETQ EP (VEVLCM EP1 EP2))
      (SETQ RP1 (VDPRED P1))
      (SETQ RP2 (VDPRED P2))
      (SETQ DB1 (CADDR P1))
      (SETQ DB2 (CADDR P2))
      (COND
       (*VDPINTEGER
        (PROGN
         (SETQ X (VBCGCD DB1 DB2))
         (SETQ DB1 (BCQUOT DB1 X))
         (SETQ DB2 (BCQUOT DB2 X)))))
      (SETQ EP1 (VEVDIF EP EP1))
      (SETQ EP2 (VEVDIF EP EP2))
      (SETQ DB2 (BCNEG DB2))
      (SETQ S (VDPILCOMB1 RP2 DB1 EP2 RP1 DB2 EP1))
      (SETQ X (VDPILCOMB1TRA COFAC2 DB1 EP2 COFAC1 DB2 EP1))
      (VDPPUTPROP S 'COFACT X)
      (RETURN S))) 
(PUT 'TRASIMPCONT 'NUMBER-OF-ARGS 1) 
(PUT 'TRASIMPCONT 'DEFINED-ON-LINE '383) 
(PUT 'TRASIMPCONT 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRASIMPCONT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRASIMPCONT (P) (COND (*VDPINTEGER (TRASIMPCONTI P)) (T (TRASIMPCONTR P)))) 
(PUT 'TRASIMPCONTI 'NUMBER-OF-ARGS 1) 
(PUT 'TRASIMPCONTI 'DEFINED-ON-LINE '389) 
(PUT 'TRASIMPCONTI 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRASIMPCONTI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRASIMPCONTI (P)
    (PROG (RES NUM COFAC)
      (COND ((OR (NULL P) (NULL (CADR (CDDR P)))) (RETURN P)))
      (SETQ COFAC (VDPGETPROP P 'COFACT))
      (SETQ NUM (CAR (VDPCONTENTI P)))
      (COND ((NOT (VBCPLUS? NUM)) (SETQ NUM (BCNEG NUM))))
      (COND ((NOT (VBCPLUS? (CADDR P))) (SETQ NUM (BCNEG NUM))))
      (COND ((BCONE? NUM) (RETURN P)))
      (SETQ RES (VDPDIVMON P NUM NIL))
      (SETQ COFAC (VDPREDUCECONTITRA COFAC NUM NIL))
      (SETQ RES (VDPPUTPROP RES 'COFACT COFAC))
      (RETURN RES))) 
(PUT 'TRASIMPCONTR 'NUMBER-OF-ARGS 1) 
(PUT 'TRASIMPCONTR 'DEFINED-ON-LINE '406) 
(PUT 'TRASIMPCONTR 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRASIMPCONTR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRASIMPCONTR (P)
    (PROG (RES COFAC)
      (SETQ COFAC (VDPGETPROP P 'COFACT))
      (COND ((OR (OR (NULL P) (NULL (CADR (CDDR P)))) (CADDR P)) (RETURN P)))
      (SETQ RES (VDPDIVMON P (CADDR P) NIL))
      (SETQ COFAC (VDPREDUCECONTITRA COFAC (CADDR P) NIL))
      (SETQ RES (VDPPUTPROP RES 'COFACT COFAC))
      (RETURN RES))) 
(PUT 'VDPILCOMB1TRA 'NUMBER-OF-ARGS 6) 
(PUT 'VDPILCOMB1TRA 'DEFINED-ON-LINE '415) 
(PUT 'VDPILCOMB1TRA 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'VDPILCOMB1TRA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE VDPILCOMB1TRA (COFAC1 DB1 EP1 COFAC2 DB2 EP2)
    (ADDSQ (MULTSQ COFAC1 (CONS (DIP2F (CADR (CDDR (VDPFMON DB1 EP1)))) 1))
           (MULTSQ COFAC2 (CONS (DIP2F (CADR (CDDR (VDPFMON DB2 EP2)))) 1)))) 
(PUT 'VDPREDUCECONTITRA 'NUMBER-OF-ARGS 3) 
(PUT 'VDPREDUCECONTITRA 'DEFINED-ON-LINE '420) 
(PUT 'VDPREDUCECONTITRA 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'VDPREDUCECONTITRA 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VDPREDUCECONTITRA (COFAC NUM DUMMY)
    (PROGN (SETQ DUMMY NIL) (MULTSQ COFAC (INVSQ (SIMP (BC2A NUM)))))) 
(PUT 'GROEBMODUCRIT 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBMODUCRIT 'DEFINED-ON-LINE '429) 
(PUT 'GROEBMODUCRIT 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'GROEBMODUCRIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBMODUCRIT (P1 P2)
    (OR (NULL GROETAGS*)
        (EQUAL (PNTH (CADR P1) GROETAGS*) (PNTH (CADR P2) GROETAGS*)))) 
(PUT 'TRAMESS0 'NUMBER-OF-ARGS 1) 
(PUT 'TRAMESS0 'DEFINED-ON-LINE '438) 
(PUT 'TRAMESS0 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAMESS0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRAMESS0 (X)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2T "adding member to intermediate quotient base:")
       (VDPPRINT X)
       (TERPRI))))) 
(PUT 'TRAMESS1 'NUMBER-OF-ARGS 3) 
(PUT 'TRAMESS1 'DEFINED-ON-LINE '442) 
(PUT 'TRAMESS1 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAMESS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRAMESS1 (X P1 P2)
    (COND
     (*TRGROEB
      (PROGN
       (PRIN2 "pair(")
       (PRIN2 (VDPGETPROP P1 'NUMBER))
       (PRIN2 ",")
       (PRIN2 (VDPGETPROP P2 'NUMBER))
       (PRIN2T ") adding member to intermediate quotient base:")
       (VDPPRINT X)
       (TERPRI))))) 
(PUT 'TRAMESS5 'NUMBER-OF-ARGS 2) 
(PUT 'TRAMESS5 'DEFINED-ON-LINE '448) 
(PUT 'TRAMESS5 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAMESS5 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRAMESS5 (P PP)
    (COND
     ((CAR P)
      (PROGN
       (SETQ HCOUNT* (PLUS HCOUNT* 1))
       (COND
        (*TRGROEB
         (PROGN
          (TERPRI)
          (PRIN2 "H-polynomial ")
          (PRIN2 PCOUNT*)
          (GROEBMESSFF " from pair(" (CADR P) NIL)
          (GROEBMESSFF "," (CADDR P) ")")
          (VDPPRINT PP)
          (PRIN2T "with cofactor")
          (WRITEPRI (MKQUOTE (PREPSQ (VDPGETPROP PP 'COFACT))) 'ONLY)
          (GROETIMEPRINT))))))
     (*TRGROEB
      (PROGN (PRIN2T "candidate from input:") (VDPPRINT PP) (GROETIMEPRINT))))) 
(PUT 'TRAMESS3 'NUMBER-OF-ARGS 2) 
(PUT 'TRAMESS3 'DEFINED-ON-LINE '460) 
(PUT 'TRAMESS3 'DEFINED-IN-FILE 'GROEBNER/GROEBTRA.RED) 
(PUT 'TRAMESS3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRAMESS3 (P S)
    (COND
     (*TRGROEBS
      (PROGN
       (PRIN2 "S-polynomial from ")
       (GROEBPAIRPRINT P)
       (VDPPRINT S)
       (PRIN2T "with cofactor")
       (WRITEPRI (MKQUOTE (PREPSQ (VDPGETPROP S 'COFACT))) 'ONLY)
       (GROETIMEPRINT)
       (TERPRIT 3))))) 
(ENDMODULE) 