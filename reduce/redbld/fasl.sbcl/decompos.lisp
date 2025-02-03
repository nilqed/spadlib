(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DECOMPOS)) 
(GLOBAL '(DECOMPOSEGENSYM*)) 
(PUT 'DECOMPOSE 'PSOPFN 'DECOMPOSESF) 
(PUT 'DECOMPOSESF 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSESF 'DEFINED-ON-LINE '51) 
(PUT 'DECOMPOSESF 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSESF (F)
    ((LAMBDA (*FACTOR *EXP)
       (CONS 'LIST (REVERSE (DECOMPOSEF2 (SIMP (REVAL1 (CAR F) T)) T))))
     NIL T)) 
(PUT 'DECOMPOSEF1 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSEF1 'DEFINED-ON-LINE '55) 
(PUT 'DECOMPOSEF1 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEF1 (F MSG) (DECOMPOSEF2 (CONS F 1) MSG)) 
(PUT 'DECOMPOSEF2 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSEF2 'DEFINED-ON-LINE '58) 
(PUT 'DECOMPOSEF2 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEF2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEF2 (F MSG)
    (PROG (HVARS R RR X Y U VARS NEWVARS D)
      (SETQ DECOMPOSEGENSYM* 1000)
      (SETQ VARS (DECOMPOSESFVARS (CAR F) NIL))
      (SETQ NEWVARS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X VARS)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (DECOMPOSEGENSYM)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (DECOMPOSEGENSYM)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ D (CDR F))
      (COND
       ((NOT (OR (ATOM D) (ATOM (CAR D))))
        (RERROR 'POLY 18 (TYPERR (PREPSQ F) "polynomial"))))
      (SETQ F (CAR (SUBF (CAR F) (PAIR VARS NEWVARS))))
      (COND ((EQUAL (LENGTH VARS) 1) (SETQ R (DECOMPOSESFUNI0 F)))
            (T (SETQ R (DECOMPOSESFMULTI F NEWVARS))))
      (SETQ HVARS '(U V W A B C D E))
      (PROG (X)
        (SETQ X VARS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ HVARS (DELETE X HVARS))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT R) (RETURN NIL)))
        (PROGN
         (COND
          ((CDR R)
           (PROGN
            (SETQ Y X)
            (SETQ X NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT (NULL X)) (RETURN NIL)))
              (COND
               (HVARS
                (PROGN
                 (SETQ X (CAR HVARS))
                 (SETQ HVARS (CDR HVARS))
                 (COND ((NOT (EQUAL X (REVAL1 X T))) (SETQ X NIL)))))
               (T (SETQ X (DECOMPOSEGENSYM))))
              (GO WHILELABEL))
            (SETQ U
                    (PREPSQ
                     (SUBSQ (CAR R) (LIST (CONS (CAAAR (CAR (CAR R))) X)))))
            (COND
             ((NEQ D 1)
              (PROGN (SETQ U (LIST 'QUOTIENT U (PREPF D))) (SETQ D 1))))
            (SETQ RR (CONS (COND (Y (LIST 'EQUAL Y U)) (T U)) RR))))
          (T
           (PROGN
            (SETQ U (PREPSQ (CAR R)))
            (SETQ Y X)
            (SETQ RR (CONS (COND (Y (LIST 'EQUAL Y U)) (T U)) RR)))))
         (SETQ R (CDR R)))
        (GO WHILELABEL))
      (SETQ RR (CONS (SUBLA (PAIR NEWVARS VARS) (CAR RR)) (CDR RR)))
      (RETURN RR))) 
(PUT 'DECOMPOSESFVARS 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSESFVARS 'DEFINED-ON-LINE '89) 
(PUT 'DECOMPOSESFVARS 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFVARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSESFVARS (F V)
    (COND ((OR (ATOM F) (ATOM (CAR F))) V)
          (T
           (DECOMPOSESFVARS (CDR F)
                            (DECOMPOSESFVARS (CDAR F)
                                             (COND
                                              ((NOT (MEMBER (CAAAR F) V))
                                               (APPEND V (LIST (CAAAR F))))
                                              (T V))))))) 
(PUT 'DECOMPOSESFUNI0 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSESFUNI0 'DEFINED-ON-LINE '97) 
(PUT 'DECOMPOSESFUNI0 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFUNI0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSESFUNI0 (F)
    (PROG (P FORALL-RESULT FORALL-ENDPTR)
      (SETQ P (DECOMPOSESFUNI F))
      (COND ((NULL P) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (P) (CONS P 1)) (CAR P)) NIL)))
     LOOPLABEL
      (SETQ P (CDR P))
      (COND ((NULL P) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (P) (CONS P 1)) (CAR P)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'DECOMPOSESFUNI 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSESFUNI 'DEFINED-ON-LINE '100) 
(PUT 'DECOMPOSESFUNI 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFUNI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSESFUNI (F)
    (PROG (X Y RES DDFL H TESTF N)
      (SETQ N 0)
      (SETQ N (CDAAR F))
      (COND ((PRIMEP N) (RETURN (LIST F))))
      (SETQ X (CAAAR F))
      (SETQ Y (DECOMPOSEGENSYM))
      (SETQ DDFL (DECOMPOSEFCTRF (DECOMPOSEDF F X)))
      (COND
       ((GREATERP (LENGTH DDFL) 1)
        (PROG (D)
          (SETQ D DDFL)
         LAB
          (COND ((NULL D) (RETURN NIL)))
          ((LAMBDA (D)
             (COND
              ((AND (NULL RES) (EQUAL 0 (REMAINDER N (PLUS (CDAAR D) 1))))
               (PROGN
                (SETQ H (CAR (DECOMPOSEINT D X)))
                (COND
                 ((NULL TESTF)
                  (SETQ TESTF
                          (ADDF F (NEGF (CAR (SUBF F (LIST (CONS X Y)))))))))
                (COND
                 (((LAMBDA (*EXP)
                     (QUOTF1 TESTF
                             (ADDF H (NEGF (CAR (SUBF H (LIST (CONS X Y))))))))
                   T)
                  (SETQ RES (LIST (DECOMPOSEBACKSUBSTUNI F H X) H))))
                (COND ((AND RES (LESSP (CDAAR (CAR RES)) 2)) (SETQ RES NIL)))
                NIL))))
           (CAR D))
          (SETQ D (CDR D))
          (GO LAB))))
      (COND ((NULL RES) (RETURN (LIST F)))
            (T
             (RETURN
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U RES)
               STARTOVER
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (U) (DECOMPOSESFUNI U)) (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ U (CDR U))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (U) (DECOMPOSESFUNI U)) (CAR U)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ U (CDR U))
                (GO LOOPLABEL))))))) 
(PUT 'DECOMPOSEFCTRF 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSEFCTRF 'DEFINED-ON-LINE '123) 
(PUT 'DECOMPOSEFCTRF 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEFCTRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSEFCTRF (F)
    (PROG (U W Q)
      (SETQ Q (FCTRF F))
      (SETQ U (CDR Q))
      (COND ((AND (EQUAL (LENGTH U) 1) (EQUAL (CDAR U) 1)) (RETURN (LIST F))))
      (SETQ W
              (DELETE ((LAMBDA (*EXP) (QUOTF1 F (CAR Q))) T)
                      (DECOMPOSEFCTRF1 U)))
      (SETQ W (DELETE 1 W))
      (RETURN W))) 
(PUT 'DECOMPOSEFCTRF1 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSEFCTRF1 'DEFINED-ON-LINE '134) 
(PUT 'DECOMPOSEFCTRF1 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEFCTRF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSEFCTRF1 (V)
    (COND ((NULL V) '(1))
          (T
           (PROG (R C Q)
             (SETQ C (CAR V))
             (SETQ R (DECOMPOSEFCTRF1 (CDR V)))
             (SETQ Q
                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                       (SETQ I 1)
                       (COND ((MINUSP (DIFFERENCE (CDR C) I)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS (EXPTF (CAR C) I) NIL)))
                      LOOPLABEL
                       (SETQ I (PLUS2 I 1))
                       (COND
                        ((MINUSP (DIFFERENCE (CDR C) I))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS (EXPTF (CAR C) I) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN
              (APPEND R
                      (PROG (U FORALL-RESULT FORALL-ENDPTR)
                        (SETQ U Q)
                       STARTOVER
                        (COND ((NULL U) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (U)
                                   (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ P R)
                                     (COND ((NULL P) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (P)
                                                         (COND
                                                          (*PHYSOP-LOADED
                                                           (PHYSOP-MULTF U P))
                                                          (T
                                                           (POLY-MULTF U P))))
                                                       (CAR P))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ P (CDR P))
                                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF U P))
                                                  (T (POLY-MULTF U P))))
                                               (CAR P))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR U)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ U (CDR U))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL U) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (U)
                                   (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ P R)
                                     (COND ((NULL P) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (P)
                                                         (COND
                                                          (*PHYSOP-LOADED
                                                           (PHYSOP-MULTF U P))
                                                          (T
                                                           (POLY-MULTF U P))))
                                                       (CAR P))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ P (CDR P))
                                     (COND ((NULL P) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P)
                                                 (COND
                                                  (*PHYSOP-LOADED
                                                   (PHYSOP-MULTF U P))
                                                  (T (POLY-MULTF U P))))
                                               (CAR P))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR U)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ U (CDR U))
                        (GO LOOPLABEL)))))))) 
(PUT 'DECOMPOSEBACKSUBSTUNI 'NUMBER-OF-ARGS 3) 
(PUT 'DECOMPOSEBACKSUBSTUNI 'DEFINED-ON-LINE '148) 
(PUT 'DECOMPOSEBACKSUBSTUNI 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEBACKSUBSTUNI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEBACKSUBSTUNI (F H X)
    (PROG (C G N P PARS ANSATZ EQS)
      (SETQ P 1)
      (SETQ N (QUOTIENT (CDAAR F) (CDAAR H)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (PROGN
         (SETQ C (MKID 'COEFF I))
         (SETQ PARS (CONS C PARS))
         (SETQ ANSATZ
                 (ADDF
                  ((LAMBDA (G685)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF G685 P))
                           (T (POLY-MULTF G685 P))))
                   (CAR (SIMP C)))
                  ANSATZ))
         (SETQ P
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF P H))
                       (T (POLY-MULTF P H))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ PARS (REVERSE PARS))
      (SETQ ANSATZ (ADDF F (NEGF ANSATZ)))
      (SETQ EQS (DECOMPOSECOEFF ANSATZ (LIST X)))
      (SETQ EQS
              (SOLVEEVAL
               (LIST
                (CONS 'LIST
                      (PROG (U FORALL-RESULT FORALL-ENDPTR)
                        (SETQ U EQS)
                        (COND ((NULL U) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (U) (PREPF U)) (CAR U))
                                              NIL)))
                       LOOPLABEL
                        (SETQ U (CDR U))
                        (COND ((NULL U) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (U) (PREPF U)) (CAR U)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
                (CONS 'LIST PARS))))
      (SETQ EQS (CDR (CADR EQS)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ G
                (ADDF G
                      (CAR
                       (SIMP
                        (LIST 'TIMES (LIST 'EXPT X I)
                              (CADDR (NTH EQS (PLUS I 1))))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN G))) 
(PUT 'DECOMPOSEDF 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSEDF 'DEFINED-ON-LINE '169) 
(PUT 'DECOMPOSEDF 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEDF (F X)
    (COND ((OR (OR (ATOM F) (ATOM (CAR F))) (NOT (EQUAL (CAAAR F) X))) NIL)
          ((EQUAL (CDAAR F) 1) (CDAR F))
          (T
           (CONS
            (CONS (CONS (CAAAR F) (DIFFERENCE (CDAAR F) 1))
                  ((LAMBDA (G688)
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR F) G688))
                           (T (POLY-MULTF (CDAR F) G688))))
                   (CDAAR F)))
            (DECOMPOSEDF (CDR F) X))))) 
(PUT 'DECOMPOSEINT 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSEINT 'DEFINED-ON-LINE '177) 
(PUT 'DECOMPOSEINT 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEINT (F X)
    (COND ((NULL F) (CONS NIL 1))
          ((OR (ATOM F) (ATOM (CAR F)))
           (CONS (CONS (CONS (CONS X 1) F) NIL) 1))
          (T
           (ADDSQ
            (MULTSQ (CONS (CONS (CONS (CONS X (PLUS (CDAAR F) 1)) 1) NIL) 1)
                    (MULTSQ (CONS (CDAR F) 1) (CONS 1 (PLUS (CDAAR F) 1))))
            (DECOMPOSEINT (CDR F) X))))) 
(PUT 'DECOMPOSECOEFF 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSECOEFF 'DEFINED-ON-LINE '186) 
(PUT 'DECOMPOSECOEFF 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSECOEFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSECOEFF (F VARS)
    (PROG (O)
      (SETQ O (SETKORDER VARS))
      (SETQ F (REORDER F))
      (SETKORDER O)
      (RETURN (DECOMPOSECOEFF1 F VARS)))) 
(PUT 'DECOMPOSECOEFF1 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSECOEFF1 'DEFINED-ON-LINE '195) 
(PUT 'DECOMPOSECOEFF1 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSECOEFF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSECOEFF1 (F VARS)
    (COND ((OR (ATOM F) (ATOM (CAR F))) NIL)
          ((NOT (MEMBER (CAAAR F) VARS)) (LIST F))
          (T
           (NCONC (DECOMPOSECOEFF1 (CDAR F) VARS)
                  (DECOMPOSECOEFF1 (CDR F) VARS))))) 
(PUT 'DECOMPOSETDG 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSETDG 'DEFINED-ON-LINE '200) 
(PUT 'DECOMPOSETDG 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSETDG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSETDG (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) 0)
          (T
           (MAX (PLUS (CDAAR F) (DECOMPOSETDG (CDAR F)))
                (DECOMPOSETDG (CDR F)))))) 
(PUT 'DECOMPOSEDEGR 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSEDEGR 'DEFINED-ON-LINE '205) 
(PUT 'DECOMPOSEDEGR 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEDEGR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEDEGR (F VL)
    (COND ((OR (ATOM F) (ATOM (CAR F))) VL)
          (T
           ((LAMBDA (V)
              (PROGN
               (COND ((GREATERP (CDAAR F) (CDR V)) (SETCDR V (CDAAR F))))
               (DECOMPOSEDEGR (CDAR F) VL)
               (DECOMPOSEDEGR (CDR F) VL)
               VL))
            (ASSOC (CAAAR F) VL))))) 
(PUT 'COMPOSE 'NUMBER-OF-ARGS 2) 
(PUT 'COMPOSE 'DEFINED-ON-LINE '212) 
(PUT 'COMPOSE 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'COMPOSE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMPOSE (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) U)
          (T (CAR (SUBF U (LIST (CONS (CAAAR U) (PREPF V)))))))) 
(PUT 'DECOMPOSESFMULTI 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSESFMULTI 'DEFINED-ON-LINE '228) 
(PUT 'DECOMPOSESFMULTI 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFMULTI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSESFMULTI (F VARS)
    (PROG (DM FT R RR A Q C P1 P2)
      (COND
       ((OR (NULL DMODE*) (NOT (FLAGP DMODE* 'FIELD)))
        (PROGN
         ((LAMBDA (*MSG) (SETDMODE 'RATIONAL T)) NIL)
         (SETQ DM T)
         (SETQ FT (*Q2F (RESIMP (CONS F 1))))))
       (T (SETQ FT F)))
      (SETQ R (DECOMPOSESFMULTI1 FT VARS))
      (COND (DM ((LAMBDA (*MSG) (SETDMODE 'RATIONAL NIL)) NIL)))
      (COND ((NULL (CDR R)) (RETURN (LIST (CONS F 1)))))
      (SETQ RR
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (REVERSE R))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (SIMP (PREPF P))) (CAR P))
                                      NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (SIMP (PREPF P))) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND RR (CDR RR))) (RETURN NIL)))
        (PROGN
         (SETQ P1 (CAR RR))
         (SETQ P2 (CADR RR))
         (SETQ Q (CDR P1))
         (SETQ A (CAR P1))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (OR (ATOM A) (ATOM (CAR A))))) (RETURN NIL)))
           (SETQ A (CDR A))
           (GO WHILELABEL))
         (SETQ P1 (ADDF (CAR P1) (NEGF A)))
         (SETQ C (DECOMPOSENORMFAC P1))
         (SETQ P1 (MULTSQ (CONS P1 1) (CONS 1 C)))
         (SETQ P2
                 (SUBSQ P2
                        (LIST
                         (CONS (CAAAR (CAR P2))
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'TIMES (DECOMPOSEGENSYM)
                                                 (PREPF C))
                                           (PREPF A))
                                     (PREPF Q))))))
         (SETQ R (CONS P1 R))
         (SETQ RR (CONS P2 (CDDR RR))))
        (GO WHILELABEL))
      (RETURN (CONS (CAR RR) R)))) 
(PUT 'DECOMPOSESFMULTI1 'NUMBER-OF-ARGS 2) 
(PUT 'DECOMPOSESFMULTI1 'DEFINED-ON-LINE '259) 
(PUT 'DECOMPOSESFMULTI1 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFMULTI1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSESFMULTI1 (F VARS)
    (PROG (LVARS FT RT X1 A0 KORD U SIGMA N M)
      (SETQ N 0)
      (SETQ M 0)
      (SETQ U
              (DECOMPOSEDEGR F
                             (PROG (X FORALL-RESULT FORALL-ENDPTR)
                               (SETQ X VARS)
                               (COND ((NULL X) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (X) (CONS X 0))
                                                 (CAR X))
                                                NIL)))
                              LOOPLABEL
                               (SETQ X (CDR X))
                               (COND ((NULL X) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS ((LAMBDA (X) (CONS X 0)) (CAR X))
                                             NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))
      (SETQ N (MINUS 1))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((LESSP N (CDR X)) (PROGN (SETQ N (CDR X)) (SETQ X1 (CAR X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND ((LESSP N 2) (RETURN (LIST F))))
      (SETQ VARS (CONS X1 (DELETE X1 VARS)))
      (SETQ KORD (SETKORDER VARS))
      (SETQ F (REORDER F))
      (SETQ N (DECOMPOSETDG F))
      (SETQ X1 (CAR VARS))
      (SETQ LVARS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR VARS))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (CONS X (DECOMPOSEGENSYM)))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CONS X (DECOMPOSEGENSYM))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
     AGAIN
      (COND ((GREATERP M 10) (PROGN (SETQ RT (LIST F)) (GO RET))))
      (SETQ SIGMA
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X LVARS)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (CONS X (RANDOM 1000))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CONS X (RANDOM 1000))) (CAR X))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FT
              (CAR
               (SUBF F
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X SIGMA)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X)
                                           (CONS (CAAR X)
                                                 (LIST 'PLUS (CDAR X)
                                                       (LIST 'TIMES X1
                                                             (CDR X)))))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X)
                                   (CONS (CAAR X)
                                         (LIST 'PLUS (CDAR X)
                                               (LIST 'TIMES X1 (CDR X)))))
                                 (CAR X))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (COND
       ((NOT (OR (ATOM (CDAR FT)) (ATOM (CAR (CDAR FT)))))
        (PROGN (SETQ M (PLUS M 1)) (GO AGAIN))))
      (SETQ A0 (CDAR FT))
      (SETQ FT ((LAMBDA (*EXP) (QUOTF1 FT A0)) T))
      (SETQ RT (DECOMPOSESFMNORM FT N (SUBLIS LVARS VARS)))
      (COND
       ((CDR RT)
        (PROGN
         (SETQ RT (REVERSE RT))
         (SETQ RT
                 (CONS
                  (CAR
                   (SUBF (CAR RT)
                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                           (SETQ X SIGMA)
                           (COND ((NULL X) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (X)
                                               (CONS (CDAR X)
                                                     (LIST 'DIFFERENCE (CAAR X)
                                                           (LIST 'TIMES (CDR X)
                                                                 X1))))
                                             (CAR X))
                                            NIL)))
                          LOOPLABEL
                           (SETQ X (CDR X))
                           (COND ((NULL X) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (CONS (CDAR X)
                                             (LIST 'DIFFERENCE (CAAR X)
                                                   (LIST 'TIMES (CDR X) X1))))
                                     (CAR X))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                  (CONS
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF A0 (CADR RT)))
                         (T (POLY-MULTF A0 (CADR RT))))
                   (CDDR RT))))
         NIL))
       (T (SETQ RT (LIST F))))
     RET
      (SETKORDER KORD)
      (SETQ RT
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P RT)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (REORDER P)) (CAR P)) NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (P) (REORDER P)) (CAR P)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (COND
        ((AND (CDR RT) (GREATERP (DECOMPOSETDG (CAR RT)) 1))
         (APPEND (REVERSE (CDR RT)) (DECOMPOSESFMULTI1 (CAR RT) VARS)))
        (T (REVERSE RT)))))) 
(PUT 'DECOMPOSELMON 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSELMON 'DEFINED-ON-LINE '301) 
(PUT 'DECOMPOSELMON 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSELMON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSELMON (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) NIL)
          (T (CONS (CAAAR F) (DECOMPOSELMON (CDAR F)))))) 
(PUT 'DECOMPOSENORMFAC 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSENORMFAC 'DEFINED-ON-LINE '306) 
(PUT 'DECOMPOSENORMFAC 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSENORMFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSENORMFAC (P1)
    (COND
     ((OR (NULL DMODE*) (NOT (FLAGP DMODE* 'FIELD)))
      ((LAMBDA (G689 G690)
         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G689 G690))
               (T (POLY-MULTF G689 G690))))
       (CAR (MKABSFD (DECOMPOSECONT P1))) (DECOMPOSESIGN P1)))
     (T
      (PROGN
       (PROG ()
        WHILELABEL
         (COND ((NOT (NOT (OR (ATOM P1) (ATOM (CAR P1))))) (RETURN NIL)))
         (SETQ P1 (CDAR P1))
         (GO WHILELABEL))
       P1)))) 
(PUT 'DECOMPOSECONT 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSECONT 'DEFINED-ON-LINE '311) 
(PUT 'DECOMPOSECONT 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSECONT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSECONT (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) F)
          (T (GCDF (DECOMPOSECONT (CDAR F)) (DECOMPOSECONT (CDR F)))))) 
(PUT 'DECOMPOSESIGN 'NUMBER-OF-ARGS 1) 
(PUT 'DECOMPOSESIGN 'DEFINED-ON-LINE '316) 
(PUT 'DECOMPOSESIGN 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECOMPOSESIGN (F)
    (COND
     ((OR (ATOM F) (ATOM (CAR F)))
      (CAR (MULTSQ (CONS F 1) (INVSQ (MKABSFD F)))))
     (T (DECOMPOSESIGN (CDAR F))))) 
(PUT 'DECOMPOSESFMNORM 'NUMBER-OF-ARGS 3) 
(PUT 'DECOMPOSESFMNORM 'DEFINED-ON-LINE '322) 
(PUT 'DECOMPOSESFMNORM 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSESFMNORM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSESFMNORM (F N VARS)
    (PROG (X X1 F0 G U ABORT H K TT Q V R S)
      (SETQ R 0)
      (SETQ S 0)
      (SETQ X1 (CAR VARS))
      (SETQ F0
              (CAR
               (SUBF F
                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                       (SETQ Y (CDR VARS))
                       (COND ((NULL Y) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (Y) (CONS Y 0)) (CAR Y))
                                             NIL)))
                      LOOPLABEL
                       (SETQ Y (CDR Y))
                       (COND ((NULL Y) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (Y) (CONS Y 0)) (CAR Y)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ U (DECOMPOSESFUNI F0))
      (COND
       ((NULL (CDR U))
        (PROGN (SETQ U (APPEND U (LIST (LIST (CONS (CONS X1 1) 1))))))))
      (SETQ X (DECOMPOSEGENSYM))
      (SETQ G (CAR (SUBF (CAR U) (LIST (CONS X1 X)))))
      (SETQ R (CDAAR G))
      (SETQ H (CADR U))
      (SETQ U (CDDR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ V (CAR U))
         (SETQ U (CDR U))
         (SETQ H (CAR (SUBF H (LIST (CONS X1 X)))))
         (SETQ H (COMPOSE H V))
         NIL)
        (GO WHILELABEL))
      (SETQ S (DIVIDE N R))
      (COND ((NOT (EQUAL (CDR S) 0)) (GO FAIL)) (T (SETQ S (CAR S))))
      (SETQ K H)
      (SETQ TT (COMPOSE (DECOMPOSEDF G X) H))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE S I)) (RETURN NIL)))
        (COND
         ((NOT ABORT)
          (PROGN
           (SETQ U (DECOMPOSEHOMOG (ADDF F (NEGF (COMPOSE G K))) X1 I))
           (SETQ Q ((LAMBDA (*EXP) (QUOTF1 U TT)) T))
           (COND ((AND U (NULL Q)) (SETQ ABORT T))
                 (T (PROGN (SETQ H Q) (SETQ K (ADDF K H))))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND (ABORT (GO FAIL)))
      (SETQ H K)
      (COND ((EQUAL F (COMPOSE G H)) (RETURN (LIST G H))))
     FAIL
      (RETURN (LIST F)))) 
(PUT 'DECOMPOSEHOMOG 'NUMBER-OF-ARGS 3) 
(PUT 'DECOMPOSEHOMOG 'DEFINED-ON-LINE '362) 
(PUT 'DECOMPOSEHOMOG 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEHOMOG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEHOMOG (F X D)
    (PROG (U V)
      (SETQ U (DECOMPOSEHOMOG1 F X D))
      (PROG (M)
        (SETQ M U)
       LAB
        (COND ((NULL M) (RETURN NIL)))
        ((LAMBDA (M) (SETQ V (ADDF V M))) (CAR M))
        (SETQ M (CDR M))
        (GO LAB))
      (RETURN V))) 
(PUT 'DECOMPOSEHOMOG1 'NUMBER-OF-ARGS 3) 
(PUT 'DECOMPOSEHOMOG1 'DEFINED-ON-LINE '373) 
(PUT 'DECOMPOSEHOMOG1 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEHOMOG1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DECOMPOSEHOMOG1 (F X D)
    (COND ((OR (LESSP D 0) (NULL F)) NIL)
          ((OR (ATOM F) (ATOM (CAR F))) (COND ((EQUAL D 0) (LIST F)) (T NIL)))
          (T
           (PROG (U1 U2)
             (SETQ U1
                     (DECOMPOSEHOMOG1 (CDAR F) X
                                      (COND ((EQUAL (CAAAR F) X) D)
                                            (T (DIFFERENCE D (CDAAR F))))))
             (SETQ U2 (DECOMPOSEHOMOG1 (CDR F) X D))
             (RETURN
              (NCONC
               (PROG (V FORALL-RESULT FORALL-ENDPTR)
                 (SETQ V U1)
                 (COND ((NULL V) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (V)
                                     ((LAMBDA (G691)
                                        (COND
                                         (*PHYSOP-LOADED (PHYSOP-MULTF G691 V))
                                         (T (POLY-MULTF G691 V))))
                                      (CONS (CONS (CONS (CAAAR F) (CDAAR F)) 1)
                                            NIL)))
                                   (CAR V))
                                  NIL)))
                LOOPLABEL
                 (SETQ V (CDR V))
                 (COND ((NULL V) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (V)
                             ((LAMBDA (G691)
                                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G691 V))
                                      (T (POLY-MULTF G691 V))))
                              (CONS (CONS (CONS (CAAAR F) (CDAAR F)) 1) NIL)))
                           (CAR V))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               U2)))))) 
(PUT 'DECOMPOSEGENSYM 'NUMBER-OF-ARGS 0) 
(PUT 'DECOMPOSEGENSYM 'DEFINED-ON-LINE '388) 
(PUT 'DECOMPOSEGENSYM 'DEFINED-IN-FILE 'POLY/DECOMPOS.RED) 
(PUT 'DECOMPOSEGENSYM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DECOMPOSEGENSYM NIL
    (COMPRESS
     (APPEND '(! |d| ! C ! |.|)
             (EXPLODE2 (SETQ DECOMPOSEGENSYM* (PLUS DECOMPOSEGENSYM* 1)))))) 
(ENDMODULE) 