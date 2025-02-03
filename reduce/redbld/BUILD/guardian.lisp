(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID '(GD_RCSID* GD_COPYRIGHT*)) 
(SETQ GD_RCSID* "$Id: guardian.red 5872 2021-07-29 14:53:49Z arthurcnorman $") 
(SETQ GD_COPYRIGHT* "(c) 1999 A. Dolzmann, 1999, 2009 T. Sturm") 
(MODULE (LIST 'GUARDIAN)) 
(CREATE-PACKAGE '(GUARDIAN GUARDIANSCHEMES GUARDIANPRINT) NIL) 
(LOAD-PACKAGE 'MATRIX) 
(LOAD-PACKAGE 'REDLOG) 
(RL_SET '(OFSF)) 
(SWITCH (LIST 'GUARDIAN 'GDQE 'GDSMART)) 
(ON1 'GUARDIAN) 
(ON1 'GDQE) 
(ON1 'GDSMART) 
(PUT 'CQUOTEGEX 'NUMBER-OF-ARGS 1) 
(PUT 'CQUOTEGEX 'DEFINED-ON-LINE '50) 
(PUT 'CQUOTEGEX 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'CQUOTEGEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CQUOTEGEX (X) (COND (*GUARDIAN 'GEX))) 
(PUT 'GEX 'EVFN 'GD_REVAL) 
(PUT 'GEX 'LENGTHFN 'LENGTHCDR) 
(AEVAL (OPERATOR (LIST 'GE 'GEC 'GEG 'GER))) 
(PUT 'GE 'RTYPEFN 'CQUOTEGEX) 
(FLAG '(MKGE) 'OPFN) 
(PUT 'MKGE 'NUMBER-OF-ARGS 1) 
(PUT 'MKGE 'DEFINED-ON-LINE '63) 
(PUT 'MKGE 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'MKGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKGE (X) (GD_REVAL X NIL)) 
(PUT 'GD_REVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GD_REVAL 'DEFINED-ON-LINE '66) 
(PUT 'GD_REVAL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_REVAL (U V)
    (PROG (W)
      (SETQ W (GD_REVALEVAL U V))
      (SETQ W (GD_ETA W))
      (SETQ W (GD_REVALSIMPL W))
      (COND (*GDSMART (SETQ W (GD_REVALSM W))))
      (RETURN W))) 
(PUT 'GD_REVALEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GD_REVALEVAL 'DEFINED-ON-LINE '77) 
(PUT 'GD_REVALEVAL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_REVALEVAL (U V)
    (COND ((ATOM U) (GD_REVALATOM U V))
          ((EQ (CAR U) 'GE)
           (GD_GESIMPL
            (GD_FLATTEN
             (CONS 'GE
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X (CDR U))
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (LIST (CAR X) (CADR X)
                                               (GD_REVALEVAL (CADDR X) V)))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (LIST (CAR X) (CADR X)
                                       (GD_REVALEVAL (CADDR X) V)))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))))
          (T
           (GD_GESIMPL
            (GD_FLATTEN
             (GD_REVALEVALOP (CAR U)
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR U))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (GD_REVALEVAL X V)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (GD_REVALEVAL X V)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))))))) 
(PUT 'GD_REVALATOM 'NUMBER-OF-ARGS 2) 
(PUT 'GD_REVALATOM 'DEFINED-ON-LINE '88) 
(PUT 'GD_REVALATOM 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALATOM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_REVALATOM (U V)
    (PROG (W)
      (COND ((NULL U) (TYPERR "nil" "gex")))
      (COND ((STRINGP U) (TYPERR (LIST "string" U) "gex")))
      (COND
       ((SETQ W (RL_GETTYPE U))
        (PROGN
         (COND
          ((EQ W 'SCALAR)
           (RETURN
            ((LAMBDA (*GUARDIAN)
               (GD_REVALEVAL (REVAL1 (CADR (GET U 'AVALUE)) T) V))
             NIL))))
         (COND ((EQ W 'GEX) (RETURN (GD_REVALEVAL (CADR (GET U 'AVALUE)) V))))
         (TYPERR (LIST W U) "gex"))))
      (RETURN (LIST 'GE (LIST 'GEG 'TRUE U))))) 
(PUT 'GD_FLATTEN 'NUMBER-OF-ARGS 1) 
(PUT 'GD_FLATTEN 'DEFINED-ON-LINE '105) 
(PUT 'GD_FLATTEN 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_FLATTEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_FLATTEN (NGE)
    (PROG (W)
      (RETURN
       (CONS 'GE
             (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
               (SETQ CASE (CDR NGE))
              STARTOVER
               (COND ((NULL CASE) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (CASE)
                          (PROG (SUBCASE FORALL-RESULT FORALL-ENDPTR)
                            (SETQ SUBCASE (CDR (CADDR CASE)))
                           STARTOVER
                            (COND ((NULL SUBCASE) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (SUBCASE)
                                       (PROGN
                                        (SETQ W
                                                (GD_NEWTYPE (CAR CASE)
                                                 (CAR SUBCASE)))
                                        (COND
                                         (W
                                          (LIST
                                           (LIST W
                                                 (LIST 'AND (CADR CASE)
                                                       (CADR SUBCASE))
                                                 (CADDR SUBCASE)))))))
                                     (CAR SUBCASE)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ SUBCASE (CDR SUBCASE))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL SUBCASE) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (SUBCASE)
                                       (PROGN
                                        (SETQ W
                                                (GD_NEWTYPE (CAR CASE)
                                                 (CAR SUBCASE)))
                                        (COND
                                         (W
                                          (LIST
                                           (LIST W
                                                 (LIST 'AND (CADR CASE)
                                                       (CADR SUBCASE))
                                                 (CADDR SUBCASE)))))))
                                     (CAR SUBCASE)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ SUBCASE (CDR SUBCASE))
                            (GO LOOPLABEL)))
                        (CAR CASE)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ CASE (CDR CASE))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL CASE) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (CASE)
                          (PROG (SUBCASE FORALL-RESULT FORALL-ENDPTR)
                            (SETQ SUBCASE (CDR (CADDR CASE)))
                           STARTOVER
                            (COND ((NULL SUBCASE) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (SUBCASE)
                                       (PROGN
                                        (SETQ W
                                                (GD_NEWTYPE (CAR CASE)
                                                 (CAR SUBCASE)))
                                        (COND
                                         (W
                                          (LIST
                                           (LIST W
                                                 (LIST 'AND (CADR CASE)
                                                       (CADR SUBCASE))
                                                 (CADDR SUBCASE)))))))
                                     (CAR SUBCASE)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ SUBCASE (CDR SUBCASE))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL SUBCASE) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (SUBCASE)
                                       (PROGN
                                        (SETQ W
                                                (GD_NEWTYPE (CAR CASE)
                                                 (CAR SUBCASE)))
                                        (COND
                                         (W
                                          (LIST
                                           (LIST W
                                                 (LIST 'AND (CADR CASE)
                                                       (CADR SUBCASE))
                                                 (CADDR SUBCASE)))))))
                                     (CAR SUBCASE)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ SUBCASE (CDR SUBCASE))
                            (GO LOOPLABEL)))
                        (CAR CASE)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ CASE (CDR CASE))
               (GO LOOPLABEL)))))) 
(PUT 'GD_NEWTYPE 'NUMBER-OF-ARGS 2) 
(PUT 'GD_NEWTYPE 'DEFINED-ON-LINE '116) 
(PUT 'GD_NEWTYPE 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_NEWTYPE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_NEWTYPE (T1 T2)
    (COND ((EQ T1 T2) T1) ((EQ T1 'GEC) (COND ((EQ T2 'GEG) 'GEC) (T NIL)))
          ((EQ T1 'GEG) T2) ((EQ T2 'GEC) NIL) (T 'GER))) 
(PUT 'GD_REVALEVALOP 'NUMBER-OF-ARGS 2) 
(PUT 'GD_REVALEVALOP 'DEFINED-ON-LINE '134) 
(PUT 'GD_REVALEVALOP 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALEVALOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_REVALEVALOP (OP GEL)
    (PROG (GTAG RGEL GCGAMMAL GCTL)
      (SETQ GTAG 'GEG)
      (PROG (GE)
        (SETQ GE GEL)
       LAB
        (COND ((NULL GE) (RETURN NIL)))
        ((LAMBDA (GE)
           (PROGN
            (COND
             ((EQ (CAR (CAR (CDR GE))) 'GER)
              (PROGN
               (SETQ GTAG 'GER)
               (SETQ RGEL (CONS (CONS 'GE (CDR (CDR GE))) RGEL))))
             (T (SETQ RGEL (CONS GE RGEL))))
            (SETQ GCGAMMAL (CONS (CADR (CAR (CDR GE))) GCGAMMAL))
            (SETQ GCTL (CONS (CADDR (CAR (CDR GE))) GCTL))))
         (CAR GE))
        (SETQ GE (CDR GE))
        (GO LAB))
      (SETQ GCGAMMAL (REVERSIP GCGAMMAL))
      (SETQ GCTL (REVERSIP GCTL))
      (SETQ RGEL (REVERSIP RGEL))
      (RETURN (GD_REVALEVALOP1 OP GTAG (LIST GCGAMMAL GCTL) RGEL)))) 
(PUT 'GD_REVALEVALOP1 'NUMBER-OF-ARGS 4) 
(PUT 'GD_REVALEVALOP1 'DEFINED-ON-LINE '152) 
(PUT 'GD_REVALEVALOP1 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALEVALOP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GD_REVALEVALOP1 (OP GTAG GCASE GEL)
    (PROG (W G)
      (SETQ G
              (LIST GTAG (CONS 'AND (CAR GCASE))
                    (GD_APPLYSCHEME OP (CADR GCASE))))
      (SETQ W (GD_CARTPROD GEL))
      (COND ((EQ GTAG 'GEG) (SETQ W (CDR W))))
      (RETURN
       (CONS 'GE
             (CONS G
                   (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                     (SETQ CASE W)
                     (COND ((NULL CASE) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (CASE)
                                         (LIST 'GEC (CONS 'AND (CAR CASE))
                                               (GD_APPLYSCHEME OP
                                                (CADR CASE))))
                                       (CAR CASE))
                                      NIL)))
                    LOOPLABEL
                     (SETQ CASE (CDR CASE))
                     (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (CASE)
                                 (LIST 'GEC (CONS 'AND (CAR CASE))
                                       (GD_APPLYSCHEME OP (CADR CASE))))
                               (CAR CASE))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))))) 
(PUT 'GD_APPLYSCHEME 'NUMBER-OF-ARGS 2) 
(PUT 'GD_APPLYSCHEME 'DEFINED-ON-LINE '165) 
(PUT 'GD_APPLYSCHEME 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_APPLYSCHEME 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_APPLYSCHEME (OP TL)
    (PROG (AL N)
      (SETQ N 0)
      (PROG (X)
        (SETQ X TL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ N (PLUS N 1))
            (SETQ AL (CONS (CONS (MKID 'A N) (CAR TL)) AL))
            (SETQ TL (CDR TL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (SUBLIS AL (GD_GETSCHEME OP N))))) 
(PUT 'GD_CARTPROD 'NUMBER-OF-ARGS 1) 
(PUT 'GD_CARTPROD 'DEFINED-ON-LINE '176) 
(PUT 'GD_CARTPROD 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_CARTPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_CARTPROD (GEL)
    (PROG (W)
      (COND
       ((NULL (CDR GEL))
        (RETURN
         (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
           (SETQ CASE (CDR (CAR GEL)))
           (COND ((NULL CASE) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (CASE)
                               (LIST (LIST (CADR CASE)) (LIST (CADDR CASE))))
                             (CAR CASE))
                            NIL)))
          LOOPLABEL
           (SETQ CASE (CDR CASE))
           (COND ((NULL CASE) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (CASE)
                       (LIST (LIST (CADR CASE)) (LIST (CADDR CASE))))
                     (CAR CASE))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (SETQ W (GD_CARTPROD (CDR GEL)))
      (RETURN
       (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
         (SETQ CASE (CDR (CAR GEL)))
        STARTOVER
         (COND ((NULL CASE) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (CASE)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X W)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (LIST (CONS (CADR CASE) (CAR X))
                                                (CONS (CADDR CASE) (CADR X))))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (LIST (CONS (CADR CASE) (CAR X))
                                        (CONS (CADDR CASE) (CADR X))))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR CASE)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ CASE (CDR CASE))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL CASE) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (CASE)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X W)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (LIST (CONS (CADR CASE) (CAR X))
                                                (CONS (CADDR CASE) (CADR X))))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (LIST (CONS (CADR CASE) (CAR X))
                                        (CONS (CADDR CASE) (CADR X))))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR CASE)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ CASE (CDR CASE))
         (GO LOOPLABEL))))) 
(PUT 'GD_GESIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GESIMPL 'DEFINED-ON-LINE '189) 
(PUT 'GD_GESIMPL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_GESIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GESIMPL (GE)
    (CONS 'GE
          (CONS (GD_GCASESIMPL (CADR GE))
                (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                  (SETQ CASE (CDDR GE))
                 STARTOVER
                  (COND ((NULL CASE) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (CASE) (GD_CASESIMPLL CASE)) (CAR CASE)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ CASE (CDR CASE))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (CASE) (GD_CASESIMPLL CASE)) (CAR CASE)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ CASE (CDR CASE))
                  (GO LOOPLABEL))))) 
(PUT 'GD_GCASESIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GCASESIMPL 'DEFINED-ON-LINE '193) 
(PUT 'GD_GCASESIMPL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_GCASESIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GCASESIMPL (GCASE)
    (LIST (CAR GCASE) (GD_SIMPL (CADR GCASE)) (CADDR GCASE))) 
(PUT 'GD_CASESIMPLL 'NUMBER-OF-ARGS 1) 
(PUT 'GD_CASESIMPLL 'DEFINED-ON-LINE '196) 
(PUT 'GD_CASESIMPLL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_CASESIMPLL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_CASESIMPLL (CASE)
    ((LAMBDA (W)
       (COND ((NEQ W 'FALSE) (LIST (LIST (CAR CASE) W (CADDR CASE))))))
     (GD_SIMPL (CADR CASE)))) 
(PUT 'GD_SIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'GD_SIMPL 'DEFINED-ON-LINE '199) 
(PUT 'GD_SIMPL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_SIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_SIMPL (F)
    ((LAMBDA (*GUARDIAN *RLNZDEN *RLADDCOND)
       (RL_PREPFOF (RL_SIMPL (RL_SIMP F) NIL (MINUS 1))))
     NIL T NIL)) 
(PUT 'GD_ETA 'NUMBER-OF-ARGS 1) 
(PUT 'GD_ETA 'DEFINED-ON-LINE '203) 
(PUT 'GD_ETA 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_ETA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_ETA (GE)
    ((LAMBDA (*GUARDIAN)
       (CONS 'GE
             (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
               (SETQ CASE (CDR GE))
               (COND ((NULL CASE) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (CASE)
                                   (LIST (CAR CASE) (CADR CASE)
                                         (REVAL1 (CADDR CASE) T)))
                                 (CAR CASE))
                                NIL)))
              LOOPLABEL
               (SETQ CASE (CDR CASE))
               (COND ((NULL CASE) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (CASE)
                           (LIST (CAR CASE) (CADR CASE)
                                 (REVAL1 (CADDR CASE) T)))
                         (CAR CASE))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
     NIL)) 
(PUT 'GD_REVALSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'GD_REVALSIMPL 'DEFINED-ON-LINE '208) 
(PUT 'GD_REVALSIMPL 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_REVALSIMPL (GE)
    (GD_REVALSIMPLRMF (GD_REVALSIMPLRECT (GD_REVALSIMPLCC GE)))) 
(PUT 'GD_REVALSIMPLCC 'NUMBER-OF-ARGS 1) 
(PUT 'GD_REVALSIMPLCC 'DEFINED-ON-LINE '212) 
(PUT 'GD_REVALSIMPLCC 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALSIMPLCC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_REVALSIMPLCC (GE)
    (PROG (NW SC C)
      (PROG (CASE)
        (SETQ CASE (CDR (CDR GE)))
       LAB
        (COND ((NULL CASE) (RETURN NIL)))
        ((LAMBDA (CASE)
           (PROGN
            (SETQ SC NW)
            (SETQ C T)
            (PROG ()
             WHILELABEL
              (COND ((NOT (AND SC C)) (RETURN NIL)))
              (PROGN
               (COND
                ((EQUAL (CADDR (CAR SC)) (CADDR CASE))
                 (PROGN
                  (SETCAR (CDR (CAR SC))
                          (GD_SIMPL (LIST 'OR (CADR (CAR SC)) (CADR CASE))))
                  (SETQ C NIL))))
               (SETQ SC (CDR SC)))
              (GO WHILELABEL))
            (COND (C (SETQ NW (CONS CASE NW))))
            NIL))
         (CAR CASE))
        (SETQ CASE (CDR CASE))
        (GO LAB))
      (RETURN (CONS 'GE (CONS (CAR (CDR GE)) (REVERSIP NW)))))) 
(PUT 'GD_REVALSIMPLRMF 'NUMBER-OF-ARGS 1) 
(PUT 'GD_REVALSIMPLRMF 'DEFINED-ON-LINE '230) 
(PUT 'GD_REVALSIMPLRMF 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALSIMPLRMF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_REVALSIMPLRMF (GE)
    (PROG (NGCASE)
      (COND ((NULL *GDQE) (RETURN GE)))
      (SETQ NGCASE
              (COND
               ((GD_FALSEP (CADR (CAR (CDR GE))))
                (LIST (CAR (CAR (CDR GE))) 'FALSE (CADDR (CAR (CDR GE)))))
               (T (CAR (CDR GE)))))
      (RETURN
       (CONS 'GE
             (CONS NGCASE
                   (PROG (CASE FORALL-RESULT FORALL-ENDPTR)
                     (SETQ CASE (CDR (CDR GE)))
                    STARTOVER
                     (COND ((NULL CASE) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (CASE)
                                (COND
                                 ((NOT (GD_FALSEP (CADR CASE))) (LIST CASE))))
                              (CAR CASE)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ CASE (CDR CASE))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL CASE) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (CASE)
                                (COND
                                 ((NOT (GD_FALSEP (CADR CASE))) (LIST CASE))))
                              (CAR CASE)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ CASE (CDR CASE))
                     (GO LOOPLABEL))))))) 
(PUT 'GD_FALSEP 'NUMBER-OF-ARGS 1) 
(PUT 'GD_FALSEP 'DEFINED-ON-LINE '242) 
(PUT 'GD_FALSEP 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_FALSEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_FALSEP (F)
    (PROG (*GUARDIAN *RLVERBOSE)
      (COND ((GD_CKERNP F) (RETURN NIL)))
      (RETURN (EQ (RL_PREPFOF (RL_QE (RL_EX (RL_SIMP F) NIL) NIL)) 'FALSE)))) 
(PUT 'GD_REVALSIMPLRECT 'NUMBER-OF-ARGS 1) 
(PUT 'GD_REVALSIMPLRECT 'DEFINED-ON-LINE '250) 
(PUT 'GD_REVALSIMPLRECT 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALSIMPLRECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_REVALSIMPLRECT (GE)
    (PROG (SC)
      (COND
       ((GD_TRUEP (CADR (CAR (CDR GE))))
        (SETQ GE
                (CONS 'GE
                      (CONS
                       (LIST (CAR (CAR (CDR GE))) 'TRUE (CADDR (CAR (CDR GE))))
                       (CDDR GE))))))
      (SETQ SC (CDDR GE))
      (PROG ()
       WHILELABEL
        (COND ((NOT SC) (RETURN NIL)))
        (PROGN
         (COND
          ((GD_TRUEP (CADR (CAR SC)))
           (PROGN
            (SETQ GE
                    (CONS 'GE
                          (CONS (CADR GE)
                                (LIST
                                 (LIST (CAR (CAR SC)) 'TRUE
                                       (CADDR (CAR SC)))))))
            (SETQ SC NIL)))
          (T (SETQ SC (CDR SC)))))
        (GO WHILELABEL))
      (RETURN GE))) 
(PUT 'GD_TRUEP 'NUMBER-OF-ARGS 1) 
(PUT 'GD_TRUEP 'DEFINED-ON-LINE '266) 
(PUT 'GD_TRUEP 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_TRUEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_TRUEP (F)
    (PROG (*GUARDIAN *RLVERBOSE)
      (COND ((EQ F 'TRUE) (RETURN T)))
      (COND ((OR (NULL *GDQE) (GD_CKERNP F)) (RETURN NIL)))
      (RETURN (EQ (RL_PREPFOF (RL_QE (RL_ALL (RL_SIMP F) NIL) NIL)) 'TRUE)))) 
(PUT 'GD_CKERNP 'NUMBER-OF-ARGS 1) 
(PUT 'GD_CKERNP 'DEFINED-ON-LINE '276) 
(PUT 'GD_CKERNP 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_CKERNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_CKERNP (F)
    (PROG (VL CKERN)
      (SETQ VL (RL_FVARL (RL_SIMP F)))
      (PROG ()
       WHILELABEL
        (COND ((NOT VL) (RETURN NIL)))
        (COND ((PAIRP (CAR VL)) (PROGN (SETQ VL NIL) (SETQ CKERN T)))
              (T (SETQ VL (CDR VL))))
        (GO WHILELABEL))
      (RETURN CKERN))) 
(PUT 'GD_REVALSM 'NUMBER-OF-ARGS 1) 
(PUT 'GD_REVALSM 'DEFINED-ON-LINE '290) 
(PUT 'GD_REVALSM 'DEFINED-IN-FILE 'GUARDIAN/GUARDIAN.RED) 
(PUT 'GD_REVALSM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_REVALSM (GE)
    (PROG (GCOND SCGE THISCASE NEWGCASE)
      (SETQ GCOND (CADR (CADR GE)))
      (SETQ SCGE (CDDR GE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCGE (NOT NEWGCASE))) (RETURN NIL)))
        (PROGN
         (SETQ THISCASE (CAR SCGE))
         (SETQ SCGE (CDR SCGE))
         (COND
          ((OR (EQ (CADR THISCASE) 'TRUE) (EQUAL (CADR THISCASE) GCOND))
           (SETQ NEWGCASE (CONS 'GEG (CDR THISCASE))))))
        (GO WHILELABEL))
      (COND (NEWGCASE (RETURN (LIST 'GE NEWGCASE))))
      (COND ((NOT *GDQE) (RETURN GE)))
      (SETQ SCGE (CDDR GE))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SCGE (NOT NEWGCASE))) (RETURN NIL)))
        (PROGN
         (SETQ THISCASE (CAR SCGE))
         (SETQ SCGE (CDR SCGE))
         (COND
          ((GD_TRUEP (LIST 'IMPL GCOND (CADR THISCASE)))
           (SETQ NEWGCASE (CONS 'GEG (CDR THISCASE))))))
        (GO WHILELABEL))
      (COND (NEWGCASE (RETURN (LIST 'GE NEWGCASE))))
      (RETURN GE))) 
(ENDMODULE) 