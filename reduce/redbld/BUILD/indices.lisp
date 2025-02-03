(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INDICES)) 
(FLUID
 '(*EXP *MSG *NAT *SUB2 ALGLIST* FANCY-POS* FANCY-LINE* FRASC* SUBFG* METRICD*
   METRICU*)) 
(GLOBAL '(MCOND*)) 
(GLOBAL
 '(BASISFORML* BASISVECTORL* KEEPL* NATURALFRAME2COFRAME DBASEFORM2BASE2FORM
   DIMEX* INDXL* NATURALVECTOR2FRAMEVECTOR COORD* CURSYM* DETM* *NOSUM NOSUML*
   COMMUTATOR-OF-FRAMEVECTORS)) 
(PUT 'INDEXEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'INDEXEVAL 'DEFINED-ON-LINE '40) 
(PUT 'INDEXEVAL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDEXEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDEXEVAL (U V)
    (PROG (X ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ V (SIMP* U))
      (SETQ X SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ V
              (MULTSQ (XPNDIND (PARTITSQ (CONS (CAR V) 1) 'INDVARPF))
                      (INVSQ (XPNDIND (PARTITSQ (CONS (CDR V) 1) 'INDVARPF)))))
      (SETQ SUBFG* X)
      (COND
       ((AND (CAR V) (NULL (INDVARPF (LIST (CAR (CAR V))))))
        (SETQ V (EXC-MK*SQ2 (RESIMP V))))
       (T (SETQ V (PREPSQXX V))))
      (RETURN V))) 
(PUT 'EXC-MK*SQ2 'NUMBER-OF-ARGS 1) 
(PUT 'EXC-MK*SQ2 'DEFINED-ON-LINE '61) 
(PUT 'EXC-MK*SQ2 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'EXC-MK*SQ2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXC-MK*SQ2 (U)
    (PROG (X)
      (SETQ X *SUB2)
      (SETQ U (SUBS2 U))
      (SETQ *SUB2 X)
      (RETURN (MK*SQ U)))) 
(PUT 'XPNDIND 'NUMBER-OF-ARGS 1) 
(PUT 'XPNDIND 'DEFINED-ON-LINE '69) 
(PUT 'XPNDIND 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'XPNDIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XPNDIND (U)
    (PROG (X Y)
      (SETQ Y (CONS NIL 1))
     A
      (COND ((NULL U) (RETURN Y)))
      (COND
       ((NULL (SETQ X (CONTIND (CAAR U))))
        (SETQ Y (ADDSQ (MULTSQ (CONS (CAAR U) 1) (CDAR U)) Y)))
       (T
        (PROG (K)
          (SETQ K (MKAINDXC X NIL))
         LAB
          (COND ((NULL K) (RETURN NIL)))
          ((LAMBDA (K)
             (SETQ Y
                     (ADDSQ (MULTSQ (SUBCINDICES (CAAR U) (PAIR X K)) (CDAR U))
                            Y)))
           (CAR K))
          (SETQ K (CDR K))
          (GO LAB))))
      (SETQ U (CDR U))
      (GO A))) 
(PUT 'SUBCINDICES 'NUMBER-OF-ARGS 2) 
(PUT 'SUBCINDICES 'DEFINED-ON-LINE '82) 
(PUT 'SUBCINDICES 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SUBCINDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBCINDICES (U L)
    (PROG (ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (RETURN
       (COND ((OR (ATOM U) (ATOM (CAR U))) (CONS U 1))
             (T
              (ADDSQ
               (MULTSQ
                (EXPTSQ
                 (COND
                  ((FLAGP (CAR (CAAAR U)) 'INDEXVAR)
                   (SIMPINDEXVAR (SUBLA L (CAAAR U))))
                  (T (SIMP (SUBINDK L (CAAAR U)))))
                 (CDAAR U))
                (SUBCINDICES (CDAR U) L))
               (SUBCINDICES (CDR U) L))))))) 
(PUT 'SUBINDK 'NUMBER-OF-ARGS 2) 
(PUT 'SUBINDK 'DEFINED-ON-LINE '95) 
(PUT 'SUBINDK 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SUBINDK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBINDK (L U)
    (CONS (CAR U)
          (PROG (J FORALL-RESULT FORALL-ENDPTR)
            (SETQ J (CDR U))
            (COND ((NULL J) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (J)
                                (COND ((ATOM J) J)
                                      ((AND (IDP (CAR J)) (GET (CAR J) 'DNAME))
                                       J)
                                      ((FLAGP (CAR J) 'INDEXVAR)
                                       (CONS (CAR J) (SUBLA L (CDR J))))
                                      (T (SUBINDK L J))))
                              (CAR J))
                             NIL)))
           LOOPLABEL
            (SETQ J (CDR J))
            (COND ((NULL J) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (J)
                        (COND ((ATOM J) J)
                              ((AND (IDP (CAR J)) (GET (CAR J) 'DNAME)) J)
                              ((FLAGP (CAR J) 'INDEXVAR)
                               (CONS (CAR J) (SUBLA L (CDR J))))
                              (T (SUBINDK L J))))
                      (CAR J))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'FORM-WITH-FREE-INDICES 'EVFN 'INDEXEVAL) 
(PUT 'INDEXED-FORM 'RTYPEFN 'FREEINDEXCHK) 
(PUT 'FORM-WITH-FREE-INDICES 'SETPRIFN 'INDXPRI) 
(PUT 'FREEINDEXCHK 'NUMBER-OF-ARGS 1) 
(PUT 'FREEINDEXCHK 'DEFINED-ON-LINE '111) 
(PUT 'FREEINDEXCHK 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'FREEINDEXCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREEINDEXCHK (U)
    (COND ((AND U INDXL* (INDXCHK U)) 'FORM-WITH-FREE-INDICES) (T NIL))) 
(PUT 'INDVARP 'NUMBER-OF-ARGS 1) 
(PUT 'INDVARP 'DEFINED-ON-LINE '115) 
(PUT 'INDVARP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDVARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDVARP (U)
    (AND (NULL *NOSUM) INDXL*
         (COND
          ((EQCAR U '*SQ)
           (OR (INDVARPF (CAR (CADR U))) (INDVARPF (CDR (CADR U)))))
          (T (FREEINDP U))))) 
(PUT 'INDVARPF 'NUMBER-OF-ARGS 1) 
(PUT 'INDVARPF 'DEFINED-ON-LINE '122) 
(PUT 'INDVARPF 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDVARPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDVARPF (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) NIL)
          (T
           (OR
            (COND ((SFP (CAAAR U)) (INDVARPF (CAAAR U)))
                  (T (FREEINDP (CAAAR U))))
            (INDVARPF (CDAR U)) (INDVARPF (CDR U)))))) 
(PUT 'FREEINDP 'NUMBER-OF-ARGS 1) 
(PUT 'FREEINDP 'DEFINED-ON-LINE '129) 
(PUT 'FREEINDP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'FREEINDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREEINDP (U)
    (PROG (X)
      (RETURN
       (COND ((OR (NULL U) (NUMBERP U)) NIL) ((ATOM U) NIL)
             ((EQ (CAR U) '*SQ) (FREEINDP (PREPSQ (CADR U))))
             ((AND (IDP (CAR U)) (GET (CAR U) 'DNAME)) NIL)
             ((FLAGP (CAR U) 'INDEXVAR) (INDXCHK (CDR U)))
             ((SETQ X (GET (CAR U) 'INDEXFUN)) (FREEINDP (APPLY1 X (CDR U))))
             ((EQ (CAR U) 'PARTDF)
              (COND ((NULL (CDDR U)) (FREEINDP (CADR U)))
                    (T (OR (FREEINDP (CADR U)) (FREEINDP (CADDR U))))))
             (T (OR (LFREEINDP (CDR U)) (FREEINDP (CAR U)))))))) 
(PUT 'LFREEINDP 'NUMBER-OF-ARGS 1) 
(PUT 'LFREEINDP 'DEFINED-ON-LINE '144) 
(PUT 'LFREEINDP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'LFREEINDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LFREEINDP (U) (AND U (OR (FREEINDP (CAR U)) (LFREEINDP (CDR U))))) 
(PUT 'INDXCHK 'NUMBER-OF-ARGS 1) 
(PUT 'INDXCHK 'DEFINED-ON-LINE '147) 
(PUT 'INDXCHK 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDXCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDXCHK (U)
    (PROG (X Y)
      (SETQ X U)
      (SETQ Y (UNION INDXL* NOSUML*))
     A
      (COND ((NULL X) (RETURN NIL)))
      (COND
       ((NULL
         (MEMQ
          (COND
           ((ATOM (CAR X))
            (COND
             ((NUMBERP (CAR X))
              ((LAMBDA (U)
                 (COND ((AND (LEQ U 15) (GEQ U 0)) (GETV INTS-AS-SYMBOLS* U))
                       (T (INTERN (LIST2STRING (EXPLODE U))))))
               (ABS (CAR X))))
             (T (CAR X))))
           ((NUMBERP (CADAR X))
            ((LAMBDA (U)
               (COND ((AND (LEQ U 15) (GEQ U 0)) (GETV INTS-AS-SYMBOLS* U))
                     (T (INTERN (LIST2STRING (EXPLODE U))))))
             (CADAR X)))
           (T (CADAR X)))
          Y))
        (RETURN T)))
      (SETQ X (CDR X))
      (GO A))) 
(PUT 'INDEXRANGE 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXRANGE 'DEFINED-ON-LINE '163) 
(PUT 'INDEXRANGE 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDEXRANGE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXRANGE (U)
    (PROG ()
      (COND ((NULL (EQCAR (CAR U) 'EQUAL)) (SETQ INDXL* (MKINDXL U)))
            (T
             (PROG (J)
               (SETQ J U)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J)
                  (PROG (NAMES RANGE)
                    (SETQ NAMES (CADR J))
                    (SETQ RANGE (CADDR J))
                    (COND ((ATOM NAMES) (SETQ NAMES (LIST NAMES)))
                          ((NULL (EQ (CAR NAMES) 'LIST))
                           (RERROR 'EXCALC 11 "badly formed indexrangelist"))
                          (T (SETQ NAMES (CDR NAMES))))
                    (COND ((ATOM RANGE) (SETQ RANGE (LIST RANGE)))
                          ((NULL (EQ (CAR RANGE) 'LIST))
                           (RERROR 'EXCALC 11 "badly formed indexrangelist"))
                          (T (SETQ RANGE (CDR RANGE))))
                    (SETQ RANGE (MKINDXL RANGE))
                    (SETQ INDXL* (REVERSE (UNION RANGE (REVERSE INDXL*))))
                    (PROG (K)
                      (SETQ K NAMES)
                     LAB
                      (COND ((NULL K) (RETURN NIL)))
                      ((LAMBDA (K) (PUT K 'INDEXRANGE RANGE)) (CAR K))
                      (SETQ K (CDR K))
                      (GO LAB))))
                (CAR J))
               (SETQ J (CDR J))
               (GO LAB)))))) 
(PUT 'NOSUM 'NUMBER-OF-ARGS 1) 
(PUT 'NOSUM 'DEFINED-ON-LINE '188) 
(PUT 'NOSUM 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'NOSUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOSUM (U) (PROGN (SETQ NOSUML* (UNION (MKINDXL U) NOSUML*)) NIL)) 
(PUT 'RENOSUM 'NUMBER-OF-ARGS 1) 
(PUT 'RENOSUM 'DEFINED-ON-LINE '191) 
(PUT 'RENOSUM 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'RENOSUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RENOSUM (U) (PROGN (SETQ NOSUML* (SETDIFF (MKINDXL U) NOSUML*)) NIL)) 
(PUT 'MKINDXL 'NUMBER-OF-ARGS 1) 
(PUT 'MKINDXL 'DEFINED-ON-LINE '194) 
(PUT 'MKINDXL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'MKINDXL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKINDXL (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND
                           ((NUMBERP J)
                            (COND
                             ((AND (LEQ J 15) (GEQ J 0))
                              (GETV INTS-AS-SYMBOLS* J))
                             (T (INTERN (LIST2STRING (EXPLODE J))))))
                           (T J)))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND
                   ((NUMBERP J)
                    (COND
                     ((AND (LEQ J 15) (GEQ J 0)) (GETV INTS-AS-SYMBOLS* J))
                     (T (INTERN (LIST2STRING (EXPLODE J))))))
                   (T J)))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(RLISTAT '(INDEXRANGE NOSUM RENOSUM)) 
(DE UPINDP (U) (ATOM (REVALIND U))) 
(PUT 'UPINDP 'NUMBER-OF-ARGS 1) 
(PUT 'UPINDP 'DEFINED-ON-LINE '200) 
(PUT 'UPINDP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'UPINDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'UPINDP 'INLINE '(LAMBDA (U) (ATOM (REVALIND U)))) 
(PUT 'ALLIND 'NUMBER-OF-ARGS 1) 
(PUT 'ALLIND 'DEFINED-ON-LINE '204) 
(PUT 'ALLIND 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'ALLIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLIND (U) (ALLIND1 U NIL)) 
(PUT 'ALLIND1 'NUMBER-OF-ARGS 2) 
(PUT 'ALLIND1 'DEFINED-ON-LINE '208) 
(PUT 'ALLIND1 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'ALLIND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALLIND1 (U V)
    (COND ((OR (ATOM U) (ATOM (CAR U))) V)
          (T
           (ALLIND1 (CDR U)
            (ALLIND1 (CDAR U) (APPEND V (ALLINDK (CAAAR U)))))))) 
(PUT 'ALLINDK 'NUMBER-OF-ARGS 1) 
(PUT 'ALLINDK 'DEFINED-ON-LINE '212) 
(PUT 'ALLINDK 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'ALLINDK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLINDK (U)
    (PROG (X)
      (RETURN
       (COND ((ATOM U) NIL)
             ((FLAGP (CAR U) 'INDEXVAR)
              (PROGN
               (PROG (J)
                 (SETQ J (CDR U))
                LAB
                 (COND ((NULL J) (RETURN NIL)))
                 ((LAMBDA (J)
                    (COND
                     ((ATOM (SETQ J (REVALIND J)))
                      (COND ((NULL (MEMQ J INDXL*)) (SETQ X (CONS J X)))
                            (T NIL)))
                     ((NULL (MEMQ (CADR J) INDXL*)) (SETQ X (CONS J X)))))
                  (CAR J))
                 (SETQ J (CDR J))
                 (GO LAB))
               (REVERSE X)))
             ((SETQ X (GET (CAR U) 'INDEXFUN)) (ALLINDK (APPLY1 X (CDR U))))
             ((EQ (CAR U) 'PARTDF)
              (COND
               ((NULL (CDDR U))
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J (ALLINDK (CDR U)))
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J))
                                        NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T
                (APPEND (ALLINDK (CADR U))
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J (ALLINDK (CDDR U)))
                          (COND ((NULL J) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (J) (LIST 'MINUS J))
                                            (CAR J))
                                           NIL)))
                         LOOPLABEL
                          (SETQ J (CDR J))
                          (COND ((NULL J) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))))
             (T (APPEND (ALLINDK (CAR U)) (ALLINDK (CDR U)))))))) 
(PUT 'CONTIND 'NUMBER-OF-ARGS 1) 
(PUT 'CONTIND 'DEFINED-ON-LINE '235) 
(PUT 'CONTIND 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'CONTIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONTIND (U)
    (PROG (DNLIST UPLIST)
      (PROG (J)
        (SETQ J (ALLIND U))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND ((ATOM (REVALIND J)) (SETQ UPLIST (CONS J UPLIST)))
                 (T (SETQ DNLIST (CONS (CADR J) DNLIST)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (SETDIFF (INTERSECTION UPLIST DNLIST) NOSUML*)))) 
(PUT 'MKAINDXC 'NUMBER-OF-ARGS 2) 
(PUT 'MKAINDXC 'DEFINED-ON-LINE '244) 
(PUT 'MKAINDXC 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'MKAINDXC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKAINDXC (U BOOL)
    (PROG (R X)
      (SETQ R (LIST U))
      (PROG (K)
        (SETQ K U)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (COND ((SETQ X (GETINDEXR K)) (SETQ R (MAPPL X K R BOOL)))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (RETURN R))) 
(PUT 'MAPPL 'NUMBER-OF-ARGS 4) 
(PUT 'MAPPL 'DEFINED-ON-LINE '255) 
(PUT 'MAPPL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'MAPPL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAPPL (U V W BOOL)
    ((LAMBDA (X)
       (COND ((NULL (CDR U)) X) (X (APPEND X (MAPPL (CDR U) V W BOOL)))
             (T (MAPPL (CDR U) V W BOOL))))
     (CHKSYMMETRIES&SUBST (CAR U) V W BOOL))) 
(PUT 'CHKSYMMETRIES&SUBST 'NUMBER-OF-ARGS 4) 
(PUT 'CHKSYMMETRIES&SUBST 'DEFINED-ON-LINE '272) 
(PUT 'CHKSYMMETRIES&SUBST 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'CHKSYMMETRIES&SUBST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHKSYMMETRIES&SUBST (U V W BOOL)
    (PROG (R X)
      (PROG (Y)
        (SETQ Y W)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (SETQ X (CHKSYMMETRIES&SUB1 U V Y BOOL))
            (COND (X (SETQ R (CONS X R))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (REVERSIP R)))) 
(PUT 'CHKSYMMETRIES&SUB1 'NUMBER-OF-ARGS 4) 
(PUT 'CHKSYMMETRIES&SUB1 'DEFINED-ON-LINE '281) 
(PUT 'CHKSYMMETRIES&SUB1 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'CHKSYMMETRIES&SUB1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHKSYMMETRIES&SUB1 (U V W BOOL)
    ((LAMBDA (X) (COND ((OR (NULL BOOL) (INDXSYMP X BOOL)) X) (T NIL)))
     (SUBST U V W))) 
(PUT 'GETINDEXR 'NUMBER-OF-ARGS 1) 
(PUT 'GETINDEXR 'DEFINED-ON-LINE '286) 
(PUT 'GETINDEXR 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'GETINDEXR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETINDEXR (U)
    (COND ((MEMQ U INDXL*) NIL)
          (T ((LAMBDA (X) (COND (X X) (T INDXL*))) (GET U 'INDEXRANGE))))) 
(PUT 'FLATINDXL 'NUMBER-OF-ARGS 1) 
(PUT 'FLATINDXL 'DEFINED-ON-LINE '291) 
(PUT 'FLATINDXL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'FLATINDXL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLATINDXL (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J) (COND ((ATOM J) J) (T (CADR J)))) (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (J) (COND ((ATOM J) J) (T (CADR J)))) (CAR J))
                    NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'INDEXLET 'NUMBER-OF-ARGS 5) 
(PUT 'INDEXLET 'DEFINED-ON-LINE '294) 
(PUT 'INDEXLET 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDEXLET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INDEXLET (U V LTYPE B RTYPE)
    (COND
     ((FLAGP (CAR U) 'INDEXVAR)
      (COND (B (SETINDEXVAR U V))
            (T
             (PROG (X Y Z MSG)
               (SETQ MSG *MSG)
               (SETQ *MSG NIL)
               (SETQ U (CAAAR (CAR (SIMP0 U))))
               (SETQ Z (FLATINDXL (ALLIND (LIST (CONS (CONS U 1) 1)))))
               (PROG (J)
                 (SETQ J (MKAINDXC Z (GET (CAR U) 'INDXSYMMETRIES)))
                LAB
                 (COND ((NULL J) (RETURN NIL)))
                 ((LAMBDA (J)
                    (PROGN
                     (LET2 (SETQ X (CAAAR (CAR (SIMP0 (SUBLA (PAIR Z J) U)))))
                           NIL NIL NIL)
                     (COND
                      ((SETQ Y (ASSOC X KEEPL*))
                       (SETQ KEEPL* (DELETE Y KEEPL*))))))
                  (CAR J))
                 (SETQ J (CDR J))
                 (GO LAB))
               (SETQ *MSG MSG)
               (COND
                ((AND BASISFORML* (EQ (CAR U) (CAAR BASISFORML*))
                      (NULL (CDDR U)))
                 (PROGN
                  (SETQ NATURALFRAME2COFRAME NIL)
                  (SETQ DBASEFORM2BASE2FORM NIL)
                  (SETQ BASISFORML* NIL))))
               (COND
                ((AND BASISVECTORL* (EQ (CAR U) (CAAR BASISVECTORL*))
                      (NULL (CDDR U)))
                 (PROGN
                  (SETQ NATURALVECTOR2FRAMEVECTOR NIL)
                  (SETQ COMMUTATOR-OF-FRAMEVECTORS NIL)
                  (SETQ BASISVECTORL* NIL))))
               (SETQ Y (GET (CAR U) 'IFDEGREE))
               (SETQ Z (ASSOC (LENGTH (CDR U)) Y))
               (SETQ Y (DELETE Z Y))
               (REMPROP (CAR U) 'IFDEGREE)
               (COND (Y (PUT (CAR U) 'IFDEGREE Y))
                     (T
                      (PROGN
                       (REMPROP (CAR U) 'RTYPE)
                       (REMPROP (CAR U) 'PARTITFN)
                       (REMPROP (CAR U) 'INDXSYMMETRIES)
                       (REMPROP (CAR U) 'INDXSYMMETRIZE)
                       (COND
                        ((FLAGP (CAR U) 'COVARIANT)
                         (REMFLAG (LIST (CAR U)) 'COVARIANT)))
                       (REMFLAG (LIST (CAR U)) 'INDEXVAR))))))))
     ((NEQ (SUBLA FRASC* U) U)
      (PUT (CAR (SETQ U (SUBLA FRASC* U))) 'OPMTCH
           (XADD*
            (CONS
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J (CDR U))
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (REVALIND J)) (CAR J)) NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (REVALIND J)) (CAR J)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))
             (LIST (CONS NIL (COND (MCOND* MCOND*) (T T))) V NIL))
            (GET (CAR U) 'OPMTCH) B)))
     (T (SETINDEXVAR U V)))) 
(PUT 'FORM-WITH-FREE-INDICES 'TYPELETFN 'INDEXLET) 
(PUT 'SETINDEXVAR 'NUMBER-OF-ARGS 2) 
(PUT 'SETINDEXVAR 'DEFINED-ON-LINE '340) 
(PUT 'SETINDEXVAR 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SETINDEXVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SETINDEXVAR (U V)
    (PROG (R S W X Y Z Z1 ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ X (CONS METRICU* (FLAGP (CAR U) 'COVARIANT)))
      (SETQ METRICU* NIL)
      (COND ((CDR X) (REMFLAG (LIST (CAR U)) 'COVARIANT)))
      (SETQ U (SIMP0 U))
      (COND
       ((OR (CDR (CAR U)) (NEQ (CDR U) 1))
        (RERROR 'EXCALC 6 "Illegal assignment")))
      (SETQ U (CAR U))
      (SETQ R (CANCEL (CONS 1 (CDAR U))))
      (SETQ U (CAAAR U))
      (SETQ METRICU* (CAR X))
      (COND ((CDR X) (FLAG (LIST (CAR U)) 'COVARIANT)))
      (SETQ Z1 (ALLIND (LIST (CONS (CONS U 1) 1))))
      (SETQ Z (FLATINDXL Z1))
      (COND
       ((AND INDXL* METRICU*)
        (PROGN
         (SETQ Z1
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J Z1)
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (COND
                                        ((FLAGP (CAR U) 'COVARIANT)
                                         (COND
                                          ((ATOM (REVALIND J))
                                           (PROGN
                                            (SETQ U
                                                    (CONS (CAR U)
                                                          (SUBST
                                                           (LIST 'MINUS J) J
                                                           (CDR U))))
                                            (CONS 'LOWER J)))
                                          (T (CADR J))))
                                        ((ATOM (REVALIND J)) J)
                                        (T
                                         (PROGN
                                          (SETQ U
                                                  (CONS (CAR U)
                                                        (SUBST J (CADR J)
                                                               (CDR U))))
                                          (CONS 'RAISE (CADR J))))))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J)
                               (COND
                                ((FLAGP (CAR U) 'COVARIANT)
                                 (COND
                                  ((ATOM (REVALIND J))
                                   (PROGN
                                    (SETQ U
                                            (CONS (CAR U)
                                                  (SUBST (LIST 'MINUS J) J
                                                         (CDR U))))
                                    (CONS 'LOWER J)))
                                  (T (CADR J))))
                                ((ATOM (REVALIND J)) J)
                                (T
                                 (PROGN
                                  (SETQ U
                                          (CONS (CAR U)
                                                (SUBST J (CADR J) (CDR U))))
                                  (CONS 'RAISE (CADR J))))))
                             (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ U
                 (CONS (CAR U)
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J (CDR U))
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (REVALIND J)) (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (REVALIND J)) (CAR J))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))))
       (T (SETQ Z1 Z)))
      (SETQ R (MULTSQ (SIMP* V) R))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (MKAINDXC Z (GET (CAR U) 'INDXSYMMETRIES)))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (PROGN
                                     (SETQ X (MKLETINDXC (PAIR Z1 J)))
                                     (SETQ S (CONS NIL 1))
                                     (SETQ Y SUBFG*)
                                     (SETQ SUBFG* NIL)
                                     (PROG (K)
                                       (SETQ K X)
                                      LAB
                                       (COND ((NULL K) (RETURN NIL)))
                                       ((LAMBDA (K)
                                          (SETQ S
                                                  (ADDSQ
                                                   (MULTSQ (CAR K)
                                                           (SUBFINDICES (CAR R)
                                                            (CDR K)))
                                                   S)))
                                        (CAR K))
                                       (SETQ K (CDR K))
                                       (GO LAB))
                                     (SETQ SUBFG* Y)
                                     (SETQ Y
                                             (*Q2F
                                              (SIMP0 (SUBLA (PAIR Z J) U))))
                                     (CONS (CAAAR Y)
                                           (EXC-MK*SQ2
                                            (MULTSQ
                                             (SUBF
                                              (COND ((MINUSF Y) (NEGF (CAR S)))
                                                    (T (CAR S)))
                                              NIL)
                                             (INVSQ
                                              (SUBF
                                               (COND
                                                (*PHYSOP-LOADED
                                                 (PHYSOP-MULTF (CDR R)
                                                  (CDR S)))
                                                (T
                                                 (POLY-MULTF (CDR R) (CDR S))))
                                               NIL)))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (PROGN
                             (SETQ X (MKLETINDXC (PAIR Z1 J)))
                             (SETQ S (CONS NIL 1))
                             (SETQ Y SUBFG*)
                             (SETQ SUBFG* NIL)
                             (PROG (K)
                               (SETQ K X)
                              LAB
                               (COND ((NULL K) (RETURN NIL)))
                               ((LAMBDA (K)
                                  (SETQ S
                                          (ADDSQ
                                           (MULTSQ (CAR K)
                                                   (SUBFINDICES (CAR R)
                                                    (CDR K)))
                                           S)))
                                (CAR K))
                               (SETQ K (CDR K))
                               (GO LAB))
                             (SETQ SUBFG* Y)
                             (SETQ Y (*Q2F (SIMP0 (SUBLA (PAIR Z J) U))))
                             (CONS (CAAAR Y)
                                   (EXC-MK*SQ2
                                    (MULTSQ
                                     (SUBF
                                      (COND ((MINUSF Y) (NEGF (CAR S)))
                                            (T (CAR S)))
                                      NIL)
                                     (INVSQ
                                      (SUBF
                                       (COND
                                        (*PHYSOP-LOADED
                                         (PHYSOP-MULTF (CDR R) (CDR S)))
                                        (T (POLY-MULTF (CDR R) (CDR S))))
                                       NIL)))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (J)
        (SETQ J W)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (LET2 (CAR J) (CDR J) NIL T)) (CAR J))
        (SETQ J (CDR J))
        (GO LAB)))) 
(PUT 'MKLETINDXC 'NUMBER-OF-ARGS 1) 
(PUT 'MKLETINDXC 'DEFINED-ON-LINE '384) 
(PUT 'MKLETINDXC 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'MKLETINDXC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKLETINDXC (U)
    (PROG (R N)
      (SETQ N 0)
      (SETQ R
              (LIST
               (CONS (CONS 1 1)
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J U)
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J)
                                           (COND ((ATOM (CAR J)) (CAR J))
                                                 (T (CDAR J))))
                                         (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (J)
                                   (COND ((ATOM (CAR J)) (CAR J))
                                         (T (CDAR J))))
                                 (CAR J))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (PROG (K)
        (SETQ K U)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (SETQ N (PLUS N 1))
            (COND
             ((ATOM (CAR K))
              (SETQ R
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J R)
                        (COND ((NULL J) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (J)
                                            (CONS (CAR J)
                                                  (SUBINDEXN K N (CDR J))))
                                          (CAR J))
                                         NIL)))
                       LOOPLABEL
                        (SETQ J (CDR J))
                        (COND ((NULL J) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS (CAR J) (SUBINDEXN K N (CDR J))))
                                  (CAR J))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
             (T
              (SETQ R
                      (MAPLETIND
                       (COND ((EQ (CAAR K) 'RAISE) (GETUPPER (CDR K)))
                             (T (GETLOWER (CDR K))))
                       (CDAR K) R N))))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (RETURN R))) 
(PUT 'SUBINDEXN 'NUMBER-OF-ARGS 3) 
(PUT 'SUBINDEXN 'DEFINED-ON-LINE '400) 
(PUT 'SUBINDEXN 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SUBINDEXN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBINDEXN (U N V)
    (COND ((EQUAL N 1) (CONS U (CDR V)))
          (T (CONS (CAR V) (SUBINDEXN U (DIFFERENCE N 1) (CDR V)))))) 
(PUT 'MAPLETIND 'NUMBER-OF-ARGS 4) 
(PUT 'MAPLETIND 'DEFINED-ON-LINE '404) 
(PUT 'MAPLETIND 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'MAPLETIND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAPLETIND (U V W N)
    (COND ((NULL U) NIL)
          (T
           (APPEND
            (PROG (J FORALL-RESULT FORALL-ENDPTR)
              (SETQ J W)
              (COND ((NULL J) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J)
                                  (CONS (MULTSQ (SIMP* (CDAR U)) (CAR J))
                                        (SUBINDEXN (CONS V (CAAR U)) N
                                         (CDR J))))
                                (CAR J))
                               NIL)))
             LOOPLABEL
              (SETQ J (CDR J))
              (COND ((NULL J) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (CONS (MULTSQ (SIMP* (CDAR U)) (CAR J))
                                (SUBINDEXN (CONS V (CAAR U)) N (CDR J))))
                        (CAR J))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))
            (MAPLETIND (CDR U) V W N))))) 
(PUT 'FORM-WITH-FREE-INDICES 'SETELEMFN 'SETINDEXVAR) 
(PUT 'CLEARFDEGREE 'NUMBER-OF-ARGS 1) 
(PUT 'CLEARFDEGREE 'DEFINED-ON-LINE '413) 
(PUT 'CLEARFDEGREE 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'CLEARFDEGREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEARFDEGREE (X)
    (PROGN
     (AND (ATOM X)
          (PROGN
           (REMPROP X 'FDEGREE)
           (REMPROP X 'CLEARFN)
           (REMPROP X 'RTYPE)
           (COND ((MEMQ X COORD*) (SETQ COORD* (DELETE X COORD*))))))
     (LET2 X X NIL NIL)
     (LET2 X X T NIL))) 
(PUT 'SUBFINDICES 'NUMBER-OF-ARGS 2) 
(PUT 'SUBFINDICES 'DEFINED-ON-LINE '419) 
(PUT 'SUBFINDICES 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SUBFINDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBFINDICES (U L)
    (PROG (ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (RETURN
       (COND ((OR (ATOM U) (ATOM (CAR U))) (CONS U 1))
             (T
              (ADDSQ
               (MULTSQ
                (COND ((ATOM (CAAAR U)) (CONS (LIST (CONS (CAAR U) 1)) 1))
                      ((SFP (CAAAR U))
                       (EXPTSQ (SUBFINDICES (CAAAR U) L) (CDAAR U)))
                      ((FLAGP (CAR (CAAAR U)) 'INDEXVAR)
                       (EXPTSQ
                        (SIMPINDEXVAR
                         (CONS (CAR (CAAAR U)) (SUBLA L (CDR (CAAAR U)))))
                        (CDAAR U)))
                      ((MEMQ (CAR (CAAAR U))
                             '(WEDGE D PARTDF INNERPROD LIEDF HODGE VARDF))
                       (EXPTSQ (SIMP (SUBINDK L (CAAAR U))) (CDAAR U)))
                      (T (CONS (LIST (CONS (CAAR U) 1)) 1)))
                (SUBFINDICES (CDAR U) L))
               (SUBFINDICES (CDR U) L))))))) 
(PUT 'INDXPRI1 'NUMBER-OF-ARGS 1) 
(PUT 'INDXPRI1 'DEFINED-ON-LINE '440) 
(PUT 'INDXPRI1 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDXPRI1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDXPRI1 (U)
    (PROG (METRICU IL DNLIST UPLIST R X Y Z B)
      (SETQ METRICU METRICU*)
      (SETQ METRICU* NIL)
      (SETQ IL (ALLIND (LIST (CAR (CAR (SIMP0 U))))))
      (PROG (J)
        (SETQ J IL)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (COND ((ATOM (REVALIND J)) (SETQ UPLIST (CONS J UPLIST)))
                 (T (SETQ DNLIST (CONS (CADR J) DNLIST)))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (PROG (J)
        (SETQ J (INTERSECTION UPLIST DNLIST))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (SETQ IL (DELETE J (DELETE (REVALIND (LIST 'MINUS J)) IL))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ METRICU* METRICU)
      (SETQ Y (FLATINDXL IL))
      (SETQ R (SIMP* U))
      (PROG (J)
        (SETQ J (MKAINDXC Y NIL))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ X (PAIR Y J))
            (SETQ Z
                    (EXC-MK*SQ2
                     (MULTSQ (SUBFINDICES (CAR R) X) (CONS 1 (CDR R)))))
            (COND
             ((NULL (AND *NERO (EQUAL Z 0)))
              (PROGN
               (MAPRIN (LIST 'SETQ (SUBLA X (CONS 'NS IL)) Z))
               (SETQ B T)
               (COND ((NOT *NAT) (PRIN2* "$")))
               (TERPRI* T))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND ((AND *NERO (NULL B)) (MAPRIN " "))))) 
(PUT 'INDXPRI 'NUMBER-OF-ARGS 2) 
(PUT 'INDXPRI 'DEFINED-ON-LINE '466) 
(PUT 'INDXPRI 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'INDXPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDXPRI (V U)
    (PROG (X Y Z B)
      (SETQ Y (FLATINDXL (ALLINDK V)))
      (PROG (J)
        (SETQ J
                (MKAINDXC Y
                 (COND ((COPOSP (CDR V)) (GET (CAR V) 'INDXSYMMETRIES))
                       (T NIL))))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ X (PAIR Y J))
            (SETQ Z (REVAL1 (SUBLA X V) NIL))
            (COND
             ((NULL (AND *NERO (EQUAL Z 0)))
              (PROGN
               (MAPRIN (LIST 'SETQ (SUBLA X V) Z))
               (SETQ B T)
               (COND ((NOT *NAT) (PRIN2* "$")))
               (TERPRI* T))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND ((AND *NERO (NULL B)) (MAPRIN " "))))) 
(PUT 'COPOSP 'NUMBER-OF-ARGS 1) 
(PUT 'COPOSP 'DEFINED-ON-LINE '482) 
(PUT 'COPOSP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'COPOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPOSP (U)
    (OR (NULL (CDR U))
        (COND ((ATOM (CAR U)) (CONTPOSP (CDR U))) (T (COVPOSP (CDR U)))))) 
(PUT 'CONTPOSP 'NUMBER-OF-ARGS 1) 
(PUT 'CONTPOSP 'DEFINED-ON-LINE '488) 
(PUT 'CONTPOSP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'CONTPOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONTPOSP (U) (OR (NULL U) (AND (ATOM (CAR U)) (CONTPOSP (CDR U))))) 
(PUT 'COVPOSP 'NUMBER-OF-ARGS 1) 
(PUT 'COVPOSP 'DEFINED-ON-LINE '492) 
(PUT 'COVPOSP 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'COVPOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COVPOSP (U) (OR (NULL U) (AND (NULL (ATOM (CAR U))) (COVPOSP (CDR U))))) 
(PUT 'NS 'PRIFN 'INDVARPRT) 
(PUT 'NS 'FANCY-PPRIFN 'XINDVARPRT) 
(FLAG '(FORM-WITH-FREE-INDICES) 'SPECASSGN) 
(PUT 'NS 'FANCY-PPRIFN 'XINDVARPRT) 
(PUT 'SIMPINDEXVAR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINDEXVAR 'DEFINED-ON-LINE '502) 
(PUT 'SIMPINDEXVAR 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'SIMPINDEXVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINDEXVAR (U) (*PF2SQ (PARTITINDEXVAR U))) 
(PUT 'PARTITINDEXVAR 'NUMBER-OF-ARGS 1) 
(PUT 'PARTITINDEXVAR 'DEFINED-ON-LINE '506) 
(PUT 'PARTITINDEXVAR 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'PARTITINDEXVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PARTITINDEXVAR (U)
    (PROG (FREEL X Y Z V SGN W)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CDR U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    ((LAMBDA (K)
                                       (COND
                                        ((ATOM K)
                                         (COND
                                          ((NUMBERP K)
                                           (COND
                                            ((MINUSP K)
                                             (LIST 'MINUS
                                                   ((LAMBDA (U)
                                                      (COND
                                                       ((AND (LEQ U 15)
                                                             (GEQ U 0))
                                                        (GETV INTS-AS-SYMBOLS*
                                                              U))
                                                       (T
                                                        (INTERN
                                                         (LIST2STRING
                                                          (EXPLODE U))))))
                                                    (ABS K))))
                                            (T
                                             (COND
                                              ((AND (LEQ K 15) (GEQ K 0))
                                               (GETV INTS-AS-SYMBOLS* K))
                                              (T
                                               (INTERN
                                                (LIST2STRING (EXPLODE K))))))))
                                          (T K)))
                                        ((NUMBERP (CADR K))
                                         (LIST 'MINUS
                                               (COND
                                                ((AND (LEQ (CADR K) 15)
                                                      (GEQ (CADR K) 0))
                                                 (GETV INTS-AS-SYMBOLS*
                                                       (CADR K)))
                                                (T
                                                 (INTERN
                                                  (LIST2STRING
                                                   (EXPLODE (CADR K))))))))
                                        (T K)))
                                     (REVALIND J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            ((LAMBDA (K)
                               (COND
                                ((ATOM K)
                                 (COND
                                  ((NUMBERP K)
                                   (COND
                                    ((MINUSP K)
                                     (LIST 'MINUS
                                           ((LAMBDA (U)
                                              (COND
                                               ((AND (LEQ U 15) (GEQ U 0))
                                                (GETV INTS-AS-SYMBOLS* U))
                                               (T
                                                (INTERN
                                                 (LIST2STRING (EXPLODE U))))))
                                            (ABS K))))
                                    (T
                                     (COND
                                      ((AND (LEQ K 15) (GEQ K 0))
                                       (GETV INTS-AS-SYMBOLS* K))
                                      (T
                                       (INTERN (LIST2STRING (EXPLODE K))))))))
                                  (T K)))
                                ((NUMBERP (CADR K))
                                 (LIST 'MINUS
                                       (COND
                                        ((AND (LEQ (CADR K) 15)
                                              (GEQ (CADR K) 0))
                                         (GETV INTS-AS-SYMBOLS* (CADR K)))
                                        (T
                                         (INTERN
                                          (LIST2STRING (EXPLODE (CADR K))))))))
                                (T K)))
                             (REVALIND J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (DEG*FORM U))
      (COND ((NULL METRICU*) (GO A)))
      (SETQ Z X)
      (COND
       ((NULL (FLAGP (CAR U) 'COVARIANT))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND Z (OR (ATOM (CAR Z)) (NULL (ATSOC (CADAR Z) METRICU*)))))
             (RETURN NIL)))
           (PROGN
            (SETQ Y (CONS (CAR Z) Y))
            (COND ((NULL (ATOM (CAR Z))) (SETQ FREEL (CONS (CADAR Z) FREEL))))
            (SETQ Z (CDR Z)))
           (GO WHILELABEL))
         (COND
          (Z
           (PROGN
            (SETQ V NIL)
            (SETQ Y (REVERSE Y))
            (PROG (J)
              (SETQ J (GETLOWER (CADAR Z)))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (SETQ V
                         (ADDPF
                          (MULTPFSQ
                           (PARTITINDEXVAR
                            (CONS (CAR U) (APPEND Y (CONS (CAR J) (CDR Z)))))
                           (SIMP (CDR J)))
                          V)))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (RETURN V))))))
       (T
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND Z
                   (OR (NULL (ATOM (CAR Z))) (NULL (ATSOC (CAR Z) METRICU*)))))
             (RETURN NIL)))
           (PROGN
            (SETQ Y (CONS (CAR Z) Y))
            (COND ((ATOM (CAR Z)) (SETQ FREEL (CONS (CAR Z) FREEL))))
            (SETQ Z (CDR Z)))
           (GO WHILELABEL))
         (COND
          (Z
           (PROGN
            (SETQ V NIL)
            (SETQ Y (REVERSE Y))
            (PROG (J)
              (SETQ J (GETUPPER (CAR Z)))
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (SETQ V
                         (ADDPF
                          (MULTPFSQ
                           (PARTITINDEXVAR
                            (CONS (CAR U)
                                  (APPEND Y
                                          (CONS (LIST 'MINUS (CAR J))
                                                (CDR Z)))))
                           (SIMP (CDR J)))
                          V)))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (RETURN V)))))))
     A
      (COND
       ((OR (NULL (COPOSP X)) (NULL (GET (CAR U) 'INDXSYMMETRIES)))
        (RETURN
         (COND (W (MKUPF (CONS (CAR U) X)))
               (T (CONS (CONS 1 (MKSQ (CONS (CAR U) X) 1)) NIL))))))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (COND ((ATOM J) J) (T (CADR J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((ATOM J) J) (T (CADR J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X (INDEXSYMMETRIZE (CONS (CAR U) X)))
      (COND ((NULL X) (RETURN NIL)))
      (COND ((EQUAL (CAR X) (MINUS 1)) (SETQ SGN T)))
      (SETQ X (CDDR X))
      (COND
       ((FLAGP (CAR U) 'COVARIANT)
        (SETQ X
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J X)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (J)
                                      (COND ((MEMQ J FREEL) J)
                                            (T (LIST 'MINUS J))))
                                    (CAR J))
                                   NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (J)
                              (COND ((MEMQ J FREEL) J) (T (LIST 'MINUS J))))
                            (CAR J))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       ((AND (NULL METRICU*) (NULL (ATOM (CADR U))))
        (SETQ X
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J X)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J))
                                        NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (T
        (SETQ X
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J X)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (J)
                                      (COND ((MEMQ J FREEL) (LIST 'MINUS J))
                                            (T J)))
                                    (CAR J))
                                   NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (J)
                              (COND ((MEMQ J FREEL) (LIST 'MINUS J)) (T J)))
                            (CAR J))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (RETURN
       (COND
        (W
         (COND (SGN (MULTPFSQ (MKUPF (CONS (CAR U) X)) (CONS (MINUS 1) 1)))
               (T (MKUPF (CONS (CAR U) X)))))
        (SGN (CONS (CONS 1 (NEGSQ (MKSQ (CONS (CAR U) X) 1))) NIL))
        (T (CONS (CONS 1 (MKSQ (CONS (CAR U) X) 1)) NIL)))))) 
(PUT 'FLATINDL 'NUMBER-OF-ARGS 1) 
(PUT 'FLATINDL 'DEFINED-ON-LINE '568) 
(PUT 'FLATINDL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'FLATINDL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLATINDL (U) (COND ((NULL U) NIL) (T (APPEND (CAR U) (FLATINDL (CDR U)))))) 
(PUT 'REVALIND 'NUMBER-OF-ARGS 1) 
(PUT 'REVALIND 'DEFINED-ON-LINE '572) 
(PUT 'REVALIND 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'REVALIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALIND (U)
    (PROG (X Y ALGLIST* DMODE*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ X SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ U (SUBST '|0| 0 U))
      (SETQ Y (PREPSQ (SIMP U)))
      (SETQ SUBFG* X)
      (RETURN Y))) 
(PUT 'REVALINDL 'NUMBER-OF-ARGS 1) 
(PUT 'REVALINDL 'DEFINED-ON-LINE '583) 
(PUT 'REVALINDL 'DEFINED-IN-FILE 'EXCALC/INDICES.RED) 
(PUT 'REVALINDL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALINDL (U)
    (PROG (IND FORALL-RESULT FORALL-ENDPTR)
      (SETQ IND U)
      (COND ((NULL IND) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (IND) (REVALIND IND)) (CAR IND)) NIL)))
     LOOPLABEL
      (SETQ IND (CDR IND))
      (COND ((NULL IND) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (IND) (REVALIND IND)) (CAR IND)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(ENDMODULE) 