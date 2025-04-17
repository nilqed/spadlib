(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CTINTRO)) 
(FLUID '(DUMMY_ID* G_DVNAMES)) 
(LOAD_PACKAGE (LIST 'DUMMY)) 
(PUT 'REMSYM 'NUMBER-OF-ARGS 1) 
(PUT 'REMSYM 'DEFINED-ON-LINE '43) 
(PUT 'REMSYM 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'REMSYM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMSYM (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J)
         (COND ((FLAGP J 'SYMMETRIC) (REMFLAG (LIST J) 'SYMMETRIC))
               ((FLAGP J 'ANTISYMMETRIC) (REMFLAG (LIST J) 'ANTISYMMETRIC))
               (T (REMPROP J 'SYMTREE))))
       (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'SYM_SIGN 'NUMBER-OF-ARGS 1) 
(PUT 'SYM_SIGN 'DEFINED-ON-LINE '54) 
(PUT 'SYM_SIGN 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'SYM_SIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM_SIGN (U)
    ((LAMBDA (Y) (COND ((PERMP (ORDN Y) Y) U) (T (NEGF U))))
     (CAR (SELECT_VARS (CAAAR U))))) 
(PUT 'SIMPSUMSYM 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPSUMSYM 'DEFINED-ON-LINE '60) 
(PUT 'SIMPSUMSYM 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'SIMPSUMSYM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPSUMSYM (U)
    (COND
     ((GEQ (LENGTH U) 5)
      (REDERR "less than 5 arguments required for symmetrize"))
     (T
      (PROG (UT UU X RES OPER FN SYM BOOL BOOLFN N THESIGN)
        (SETQ N 0)
        (SETQ THESIGN 0)
        (SETQ THESIGN 1)
        (SETQ FN (CADDR U))
        (SETQ OPER (CADR U))
        (COND ((NOT (IDP OPER)) (TYPERR OPER "operator"))
              ((NULL (FLAGP OPER 'OPFN))
               (COND
                ((NULL (GET OPER 'SIMPFN)) (PUT OPER 'SIMPFN 'SIMPIDEN)))))
        (FLAG (LIST OPER) 'LISTARGP)
        (SETQ SYM (COND ((CDDDR U) (COND ((EQ (CADDDR U) 'PERM_SIGN) T)))))
        (COND
         ((AND SYM (NULL (PERMP (CDAR U) (ORDN (CDAR U)))))
          (SETQ THESIGN (MINUS THESIGN))))
        (COND
         ((NOT (MEMQ (GETTYPE FN) '(PROCEDURE ALGEBRAIC_PROCEDURE)))
          (TYPERR FN "procedure")))
        (SETQ UT (SELECT_VARS (CAR U)))
        ((LAMBDA (X)
           (SETQ UU
                   (COND
                    ((FLAGP FN 'OPFN) (PROGN (SETQ BOOLFN T) (REVAL1 X T)))
                    ((EQ (CAR (REVAL1 X T)) 'MINUS) (CDADR (REVAL1 X T)))
                    (T (CDR (REVAL1 X T))))))
         (CONS OPER (CAR UT)))
        (SETQ N (LENGTH UU))
        (SETQ X
                (COND
                 ((AND (LISTP (CAR UU)) (NULL (FLAGP OPER 'TENSOR))
                       (NOT BOOLFN))
                  (PROGN (SETQ BOOL T) (APPLY1 FN (CDAR UU))))
                 ((AND BOOLFN (LISTP (CADR UU)) (NULL (FLAGP OPER 'TENSOR)))
                  (PROGN (SETQ BOOL T) (APPLY1 FN (CADR UU))))
                 (T (APPLY1 FN UU))))
        (COND ((FLAGP FN 'OPFN) (SETQ X (ALG_TO_SYMB X))))
        (SETQ N (DIFFERENCE (LENGTH X) 1))
        (COND
         ((NOT BOOL)
          (PROGN
           (SETQ RES
                   (COND
                    (SYM
                     (SYM_SIGN
                      (LIST
                       (CONS
                        (GETPOWER
                         (FKERN
                          (COND
                           ((CADR UT) (CONS OPER (CONS (CADR UT) (CAR X))))
                           (T (CONS OPER (CAR X)))))
                         1)
                        1))))
                    (T
                     (LIST
                      (CONS
                       (GETPOWER
                        (FKERN
                         (COND ((CADR UT) (CONS OPER (CONS (CADR UT) (CAR X))))
                               (T (CONS OPER (CAR X)))))
                        1)
                       1)))))
           (PROG (I)
             (SETQ I 1)
            LAB
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
             (PROGN
              (SETQ UU (CADR X))
              (ACONC RES
                     (COND
                      (SYM
                       (CAR
                        (SYM_SIGN
                         (LIST
                          (CONS
                           (GETPOWER
                            (FKERN
                             (COND ((CADR UT) (CONS OPER (CONS (CADR UT) UU)))
                                   (T (CONS OPER UU))))
                            1)
                           1)))))
                      (T
                       (CONS
                        (GETPOWER
                         (FKERN
                          (COND ((CADR UT) (CONS OPER (CONS (CADR UT) UU)))
                                (T (CONS OPER UU))))
                         1)
                        1))))
              (DELQIP UU X)
              NIL)
             (SETQ I (PLUS2 I 1))
             (GO LAB))
           NIL))
         (T
          (PROGN
           (SETQ RES
                   (COND
                    (SYM
                     (SYM_SIGN
                      (LIST
                       (CONS
                        (GETPOWER
                         (FKERN
                          (CONS OPER
                                (LIST
                                 (CONS 'LIST
                                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ I (CAR X))
                                         (COND ((NULL I) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (I)
                                                             (MK*SQ (SIMP* I)))
                                                           (CAR I))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ I (CDR I))
                                         (COND
                                          ((NULL I) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (I)
                                                     (MK*SQ (SIMP* I)))
                                                   (CAR I))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL))))))
                         1)
                        1))))
                    (T
                     (LIST
                      (CONS
                       (GETPOWER
                        (FKERN
                         (CONS OPER
                               (LIST
                                (CONS 'LIST
                                      (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                        (SETQ I (CAR X))
                                        (COND ((NULL I) (RETURN NIL)))
                                        (SETQ FORALL-RESULT
                                                (SETQ FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (I)
                                                            (MK*SQ (SIMP* I)))
                                                          (CAR I))
                                                         NIL)))
                                       LOOPLABEL
                                        (SETQ I (CDR I))
                                        (COND
                                         ((NULL I) (RETURN FORALL-RESULT)))
                                        (RPLACD FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (I)
                                                    (MK*SQ (SIMP* I)))
                                                  (CAR I))
                                                 NIL))
                                        (SETQ FORALL-ENDPTR
                                                (CDR FORALL-ENDPTR))
                                        (GO LOOPLABEL))))))
                        1)
                       1)))))
           (PROG (I)
             (SETQ I 1)
            LAB
             (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
             (PROGN
              (SETQ UU (CADR X))
              (ACONC RES
                     (COND
                      (SYM
                       (CAR
                        (SYM_SIGN
                         (LIST
                          (CONS
                           (GETPOWER
                            (FKERN
                             (CONS OPER
                                   (LIST
                                    (CONS 'LIST
                                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ J UU)
                                            (COND ((NULL J) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (J)
                                                                (SIMP* J))
                                                              (CAR J))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ J (CDR J))
                                            (COND
                                             ((NULL J) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (J) (SIMP* J))
                                                      (CAR J))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))))
                            1)
                           1)))))
                      (T
                       (CONS
                        (GETPOWER
                         (FKERN
                          (CONS OPER
                                (LIST
                                 (CONS 'LIST
                                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                         (SETQ I UU)
                                         (COND ((NULL I) (RETURN NIL)))
                                         (SETQ FORALL-RESULT
                                                 (SETQ FORALL-ENDPTR
                                                         (CONS
                                                          ((LAMBDA (I)
                                                             (MK*SQ (SIMP* I)))
                                                           (CAR I))
                                                          NIL)))
                                        LOOPLABEL
                                         (SETQ I (CDR I))
                                         (COND
                                          ((NULL I) (RETURN FORALL-RESULT)))
                                         (RPLACD FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (I)
                                                     (MK*SQ (SIMP* I)))
                                                   (CAR I))
                                                  NIL))
                                         (SETQ FORALL-ENDPTR
                                                 (CDR FORALL-ENDPTR))
                                         (GO LOOPLABEL))))))
                         1)
                        1))))
              (DELQIP UU X)
              NIL)
             (SETQ I (PLUS2 I 1))
             (GO LAB))
           NIL)))
        (RETURN
         (COND
          ((EQ (GET OPER 'TAG) 'LIST)
           (SIMP*
            (CONS 'LIST
                  (PROG (W FORALL-RESULT FORALL-ENDPTR)
                    (SETQ W RES)
                    (COND ((NULL W) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (W) (CAAR W)) (CAR W))
                                          NIL)))
                   LOOPLABEL
                    (SETQ W (CDR W))
                    (COND ((NULL W) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (W) (CAAR W)) (CAR W)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
          (T
           (RESIMP
            (CONS
             ((LAMBDA (G540)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF G540 RES))
                      (T (POLY-MULTF G540 RES))))
              (COND ((ZEROP THESIGN) NIL) (T THESIGN)))
             1))))))))) 
(PUT 'DUMMY_NAM 'NUMBER-OF-ARGS 1) 
(PUT 'DUMMY_NAM 'DEFINED-ON-LINE '135) 
(PUT 'DUMMY_NAM 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'DUMMY_NAM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUMMY_NAM (U) (PROGN (SETQ G_DVNAMES (LIST2VECT* (ORDN U) 'SYMBOLIC)) T)) 
(PUT 'DV_SKELSPLIT 'NUMBER-OF-ARGS 1) 
(PUT 'DV_SKELSPLIT 'DEFINED-ON-LINE '147) 
(PUT 'DV_SKELSPLIT 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'DV_SKELSPLIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_SKELSPLIT (CAMB)
    (PROG (VAR_CAMB SKEL STREE SUBSKELS COUNT IND MAXIND THESIGN)
      (SETQ COUNT 0)
      (SETQ IND 0)
      (SETQ MAXIND 0)
      (SETQ THESIGN 0)
      (SETQ THESIGN 1)
      (SETQ VAR_CAMB
              (COND
               ((LISTP CAMB)
                (COND
                 ((AND (LISTP (CADR CAMB)) (EQUAL (CAADR CAMB) 'LIST))
                  (CADR CAMB))))))
      (COND
       ((SETQ IND (DUMMYP CAMB))
        (RETURN (LIST 1 IND (CONS '~DV (LIST '* IND)))))
       ((OR (NOT (LISTP CAMB)) (AND VAR_CAMB (NULL (CDDR CAMB))))
        (RETURN (LIST 1 0 (CONS CAMB NIL)))))
      (SETQ STREE (GET (CAR CAMB) 'SYMTREE))
      (COND
       ((NOT STREE)
        (PROGN
         (SETQ STREE
                 (PROG (COUNT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ COUNT 1)
                   (COND
                    ((MINUSP
                      (DIFFERENCE
                       (LENGTH (COND (VAR_CAMB (CDDR CAMB)) (T (CDR CAMB))))
                       COUNT))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS COUNT NIL)))
                  LOOPLABEL
                   (SETQ COUNT (PLUS2 COUNT 1))
                   (COND
                    ((MINUSP
                      (DIFFERENCE
                       (LENGTH (COND (VAR_CAMB (CDDR CAMB)) (T (CDR CAMB))))
                       COUNT))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS COUNT NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND ((FLAGP (CAR CAMB) 'SYMMETRIC) (SETQ STREE (CONS '+ STREE)))
               ((FLAGP (CAR CAMB) 'ANTISYMMETRIC) (SETQ STREE (CONS '- STREE)))
               (T (SETQ STREE (CONS '* STREE)))))))
      (SETQ SUBSKELS
              (MKVECT
               (DIFFERENCE
                (LENGTH (COND (VAR_CAMB (CDDR CAMB)) (T (CDR CAMB)))) 1)))
      (SETQ COUNT 0)
      (PROG (ARG)
        (SETQ ARG (COND (VAR_CAMB (CDDR CAMB)) (T (CDR CAMB))))
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (PROGN
            (SETQ COUNT (PLUS COUNT 1))
            (COND
             ((SETQ IND (DUMMYP ARG))
              (PROGN
               (SETQ MAXIND (MAX MAXIND IND))
               (COND
                ((IDP ARG)
                 (PUTV SUBSKELS (DIFFERENCE COUNT 1)
                       (CONS '~DV (LIST '* IND))))
                (T
                 (PUTV SUBSKELS (DIFFERENCE COUNT 1)
                       (CONS '~DVA (LIST '* IND)))))))
             (T (PUTV SUBSKELS (DIFFERENCE COUNT 1) (CONS ARG NIL))))
            NIL))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (SETQ STREE (ST_SORTTREE STREE SUBSKELS (FUNCTION IDCONS_ORDP)))
      (COND ((AND STREE (EQUAL (CAR STREE) 0)) (RETURN NIL)))
      (SETQ THESIGN (CAR STREE))
      (SETQ SKEL (DV_SKELSPLIT1 (CDR STREE) SUBSKELS))
      (SETQ STREE (ST_CONSOLIDATE (CDR SKEL)))
      (SETQ SKEL
              (COND (VAR_CAMB (CONS (CAR CAMB) (CONS VAR_CAMB (CAR SKEL))))
                    (T (CONS (CAR CAMB) (CAR SKEL)))))
      (RETURN (LIST THESIGN MAXIND (CONS SKEL STREE))))) 
(PUT 'DUMMYP 'NUMBER-OF-ARGS 1) 
(PUT 'DUMMYP 'DEFINED-ON-LINE '195) 
(PUT 'DUMMYP 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'DUMMYP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DUMMYP (VAR)
    (PROG (VARSPLIT COUNT RES)
      (SETQ COUNT 0)
      (SETQ RES 0)
      (COND
       ((LISTP VAR)
        (COND ((EQCAR VAR 'MINUS) (SETQ VAR (CADR VAR))) (T (RETURN NIL)))))
      (COND ((OR (NUMBERP VAR) (*ID2NUM VAR)) (RETURN NIL)))
      (SETQ COUNT 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ COUNT (UPBVE G_DVNAMES))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL VAR (GETV G_DVNAMES (DIFFERENCE COUNT 1)))
           (PROGN (SETQ RES COUNT) (SETQ COUNT (PLUS (UPBVE G_DVNAMES) 1))))
          (T (SETQ COUNT (PLUS COUNT 1))))
         NIL)
        (GO WHILELABEL))
      (COND
       ((EQUAL RES 0)
        (PROGN
         (SETQ VARSPLIT (AD_SPLITNAME VAR))
         (COND ((EQ (CAR VARSPLIT) G_DVBASE) (RETURN (CDR VARSPLIT))))))
       (T (RETURN RES))))) 
(PUT 'DV_SKEL2FACTOR1 'NUMBER-OF-ARGS 2) 
(PUT 'DV_SKEL2FACTOR1 'DEFINED-ON-LINE '227) 
(PUT 'DV_SKEL2FACTOR1 'DEFINED-IN-FILE 'ASSIST/CTINTRO.RED) 
(PUT 'DV_SKEL2FACTOR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DV_SKEL2FACTOR1 (SKEL_KERN DVARS)
    (PROG (DVAR SCR)
      (COND ((NULL SKEL_KERN) (RETURN NIL)))
      (RETURN
       (COND
        ((LISTP SKEL_KERN)
         (PROGN
          (SETQ SCR (DV_SKEL2FACTOR1 (CAR SKEL_KERN) DVARS))
          (SETQ SCR (CONS SCR (DV_SKEL2FACTOR1 (CDR SKEL_KERN) DVARS)))))
        ((EQ SKEL_KERN '~DV)
         (PROGN
          (SETQ DVAR (CAR DVARS))
          (COND
           ((CDR DVARS)
            (PROGN
             (RPLACA DVARS (CADR DVARS))
             (RPLACD DVARS (CDDR DVARS))
             NIL)))
          DVAR))
        ((EQ SKEL_KERN '~DVA)
         (PROGN
          (SETQ DVAR (CAR DVARS))
          (COND
           ((CDR DVARS)
            (PROGN
             (RPLACA DVARS (CADR DVARS))
             (RPLACD DVARS (CDDR DVARS))
             NIL)))
          (CONS 'MINUS (CONS DVAR NIL))))
        (T SKEL_KERN))))) 
(ENDMODULE) 