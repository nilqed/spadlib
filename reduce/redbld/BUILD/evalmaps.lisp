(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EVALMAPS)) 
(EXPORTS (LIST 'STRAND-ALG-TOP)) 
(IMPORTS (LIST 'COLOR-STRAND 'CONTRACT-STRAND)) 
(PUT 'PERMPL 'NUMBER-OF-ARGS 2) 
(PUT 'PERMPL 'DEFINED-ON-LINE '33) 
(PUT 'PERMPL 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PERMPL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PERMPL (U V)
    (COND ((NULL U) T) ((EQUAL (CAR U) (CAR V)) (PERMPL (CDR U) (CDR V)))
          (T (NOT (PERMPL (CDR U) (L-SUBST1 (CAR V) (CAR U) (CDR V))))))) 
(PUT 'REPEATSP 'NUMBER-OF-ARGS 1) 
(PUT 'REPEATSP 'DEFINED-ON-LINE '38) 
(PUT 'REPEATSP 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'REPEATSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPEATSP (U)
    (COND ((NULL U) NIL) (T (OR (MEMBER (CAR U) (CDR U)) (REPEATSP (CDR U)))))) 
(PUT 'L-SUBST1 'NUMBER-OF-ARGS 3) 
(PUT 'L-SUBST1 'DEFINED-ON-LINE '42) 
(PUT 'L-SUBST1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'L-SUBST1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE L-SUBST1 (NEW OLD L)
    (COND ((NULL L) NIL) ((EQUAL OLD (CAR L)) (CONS NEW (CDR L)))
          (T (CONS (CAR L) (L-SUBST1 NEW OLD (CDR L)))))) 
(PUT 'PROPAGATOR 'NUMBER-OF-ARGS 2) 
(PUT 'PROPAGATOR 'DEFINED-ON-LINE '49) 
(PUT 'PROPAGATOR 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PROPAGATOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROPAGATOR (U V)
    (COND ((NULL U) 1) ((OR (REPEATSP U) (REPEATSP V)) 0)
          (T (CONS 'PLUS (PROPAG U (PERMUTATIONS V) V))))) 
(PUT 'PROPAG 'NUMBER-OF-ARGS 3) 
(PUT 'PROPAG 'DEFINED-ON-LINE '54) 
(PUT 'PROPAG 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PROPAG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROPAG (U L V)
    (COND ((NULL L) NIL)
          (T
           (CONS
            (COND ((PERMPL V (CAR L)) (CONS 'TIMES (PRPG U (CAR L))))
                  (T (LIST 'MINUS (CONS 'TIMES (PRPG U (CAR L))))))
            (PROPAG U (CDR L) V))))) 
(PUT 'PRPG 'NUMBER-OF-ARGS 2) 
(PUT 'PRPG 'DEFINED-ON-LINE '59) 
(PUT 'PRPG 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PRPG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRPG (U V)
    (COND ((NULL U) NIL)
          (T (CONS (LIST 'CONS (CAR U) (CAR V)) (PRPG (CDR U) (CDR V)))))) 
(PUT 'LINE 'NUMBER-OF-ARGS 2) 
(PUT 'LINE 'DEFINED-ON-LINE '63) 
(PUT 'LINE 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'LINE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINE (X Y) (PROPAGATOR (CDR X) (CDR Y))) 
(PUT 'STRAND-ALG-TOP 'NUMBER-OF-ARGS 3) 
(PUT 'STRAND-ALG-TOP 'DEFINED-ON-LINE '68) 
(PUT 'STRAND-ALG-TOP 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'STRAND-ALG-TOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STRAND-ALG-TOP (STRAND MAP_ EDLST)
    (PROG (RLST)
      (SETQ STRAND (DELETEZ1 STRAND EDLST))
      (SETQ RLST (COLOR-STRAND EDLST MAP_ 1))
      (SETQ STRAND (CONTRACT-STRAND STRAND RLST))
      (RETURN (DSTR-TO-ALG STRAND RLST NIL)))) 
(PUT 'MKTAILS 'NUMBER-OF-ARGS 3) 
(PUT 'MKTAILS 'DEFINED-ON-LINE '82) 
(PUT 'MKTAILS 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MKTAILS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKTAILS (SIDE RLST DUMP)
    (PROG (PNTR NEWDUMP W Z)
      (COND ((NULL SIDE) (RETURN (CONS NIL DUMP))))
      (SETQ PNTR SIDE)
      (SETQ NEWDUMP DUMP)
      (PROG ()
       WHILELABEL
        (COND ((NOT PNTR) (RETURN NIL)))
        (PROGN
         (SETQ W (MKTAILS1 (CAR PNTR) RLST NEWDUMP))
         (SETQ NEWDUMP (CDR W))
         (SETQ Z (APPEND (CAR W) Z))
         (SETQ PNTR (CDR PNTR)))
        (GO WHILELABEL))
      (RETURN (CONS Z NEWDUMP)))) 
(PUT 'MKTAILS1 'NUMBER-OF-ARGS 3) 
(PUT 'MKTAILS1 'DEFINED-ON-LINE '95) 
(PUT 'MKTAILS1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MKTAILS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKTAILS1 (RNAME RLST DUMP)
    (PROG (COLOR PRENAME Z)
      (SETQ COLOR (GETROAD RNAME RLST))
      (COND ((EQUAL 0 COLOR) (RETURN (CONS NIL DUMP))))
      (COND
       ((EQUAL 0 (CDR RNAME))
        (RETURN
         (CONS (LIST (OR (GET (CAR RNAME) 'REPLACE_BY_VECTOR) (CAR RNAME)))
               DUMP))))
      (SETQ Z (ASSOC RNAME DUMP))
      (COND
       (Z
        (RETURN
         (COND ((NULL (CDDR Z)) (CONS (CDR Z) DUMP))
               (T (CONS (REVERSE (CDR Z)) DUMP))))))
      (SETQ PRENAME RNAME)
      (SETQ Z (MKINDS PRENAME COLOR))
      (RETURN (CONS Z (CONS (CONS RNAME Z) DUMP))))) 
(PUT 'MKINDS 'NUMBER-OF-ARGS 2) 
(PUT 'MKINDS 'DEFINED-ON-LINE '113) 
(PUT 'MKINDS 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MKINDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKINDS (PRENAME COLOR)
    (COND ((EQUAL COLOR 0) NIL)
          (T
           (PROG (INDX)
             (SETQ INDX (CONS PRENAME COLOR))
             (RETURN (CONS INDX (MKINDS PRENAME (SUB1 COLOR)))))))) 
(PUT 'GETROAD 'NUMBER-OF-ARGS 2) 
(PUT 'GETROAD 'DEFINED-ON-LINE '123) 
(PUT 'GETROAD 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'GETROAD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETROAD (RNAME RLST)
    (COND ((NULL RLST) 1)
          ((EQUAL (CDR RNAME) (CDAR RLST))
           (CDR (QASSOC (CAR RNAME) (CAAR RLST))))
          (T (GETROAD RNAME (CDR RLST))))) 
(PUT 'QASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'QASSOC 'DEFINED-ON-LINE '130) 
(PUT 'QASSOC 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'QASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QASSOC (ATM ALST)
    (COND ((NULL ALST) NIL) ((EQ ATM (CAAR ALST)) (CAR ALST))
          (T (QASSOC ATM (CDR ALST))))) 
(PUT 'FROM-RODIONOV 'NUMBER-OF-ARGS 1) 
(PUT 'FROM-RODIONOV 'DEFINED-ON-LINE '137) 
(PUT 'FROM-RODIONOV 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'FROM-RODIONOV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FROM-RODIONOV (X)
    (PROG (STRAND EDGES EDGELSTS MAP_ W)
      (SETQ EDGES (CAR X))
      (SETQ MAP_ (CADR X))
      (SETQ EDGELSTS (CDDR X))
      (SETQ STRAND (MAP_-TO-STRAND EDGES MAP_))
      (SETQ W
              (PROG (EDLST FORALL-RESULT FORALL-ENDPTR)
                (SETQ EDLST EDGELSTS)
                (COND ((NULL EDLST) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EDLST)
                                    (STRAND-ALG-TOP STRAND MAP_ EDLST))
                                  (CAR EDLST))
                                 NIL)))
               LOOPLABEL
                (SETQ EDLST (CDR EDLST))
                (COND ((NULL EDLST) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EDLST) (STRAND-ALG-TOP STRAND MAP_ EDLST))
                          (CAR EDLST))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (REVAL1 (CONS 'PLUS W) T)))) 
(PUT 'TOP1 'NUMBER-OF-ARGS 1) 
(PUT 'TOP1 'DEFINED-ON-LINE '148) 
(PUT 'TOP1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'TOP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOP1 (X) (MATHPRINT (FROM-RODIONOV (TO_TARANOV X)))) 
(PUT 'F^ 'NUMBER-OF-ARGS 2) 
(PUT 'F^ 'DEFINED-ON-LINE '153) 
(PUT 'F^ 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'F^ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE F^ (N M)
    (COND ((LESSP N M) (CVITERR "Incorrect args of f!^")) ((EQUAL N M) 1)
          (T (TIMES N (F^ (SUB1 N) M))))) 
(PUT 'MK-COEFF1 'NUMBER-OF-ARGS 2) 
(PUT 'MK-COEFF1 'DEFINED-ON-LINE '162) 
(PUT 'MK-COEFF1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MK-COEFF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK-COEFF1 (ALIST RLST)
    (COND ((NULL ALIST) 1)
          (T
           (EVAL
            (CONS 'TIMES
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X ALIST)
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (X)
                                        (FACTORIAL (GETROAD (CAR X) RLST)))
                                      (CAR X))
                                     NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X) (FACTORIAL (GETROAD (CAR X) RLST)))
                              (CAR X))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))))) 
(PUT 'PROP-SIMP 'NUMBER-OF-ARGS 2) 
(PUT 'PROP-SIMP 'DEFINED-ON-LINE '170) 
(PUT 'PROP-SIMP 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PROP-SIMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROP-SIMP (L1 L2) (PROP-SIMP1 L1 L2 NIL 0 1)) 
(PUT 'PROP-SIMP1 'NUMBER-OF-ARGS 5) 
(PUT 'PROP-SIMP1 'DEFINED-ON-LINE '173) 
(PUT 'PROP-SIMP1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PROP-SIMP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROP-SIMP1 (L1 L2 S LNGTH SGN)
    (COND ((NULL L2) (CONS (LIST LNGTH SGN) (CONS L1 (REVERSE S))))
          (T
           ((LAMBDA (Z)
              (COND
               ((NULL Z) (PROP-SIMP1 L1 (CDR L2) (CONS (CAR L2) S) LNGTH SGN))
               (T
                (PROP-SIMP1 (CDR Z) (CDR L2) S (ADD1 LNGTH)
                 (TIMES (CAR Z) SGN (EXPT (MINUS 1) (LENGTH S)))))))
            (PROP-SIMP2 L1 (CAR L2)))))) 
(PUT 'PROP-SIMP2 'NUMBER-OF-ARGS 2) 
(PUT 'PROP-SIMP2 'DEFINED-ON-LINE '182) 
(PUT 'PROP-SIMP2 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'PROP-SIMP2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROP-SIMP2 (L IND)
    (PROG (SIGN)
      (COND
       ((SETQ SIGN (COND ((ATOM IND) NIL) (T (MEMBER IND L))))
        (RETURN
         (CONS (EXPT (MINUS 1) (DIFFERENCE (LENGTH L) (LENGTH SIGN)))
               (DELETE IND L))))
       (T (RETURN NIL))))) 
(PUT 'MK-CONTRACT-COEFF 'NUMBER-OF-ARGS 1) 
(PUT 'MK-CONTRACT-COEFF 'DEFINED-ON-LINE '190) 
(PUT 'MK-CONTRACT-COEFF 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MK-CONTRACT-COEFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK-CONTRACT-COEFF (U)
    (COND ((EQUAL (CAAR U) 0) 1)
          (T
           (PROG (NUMR DENR PK K)
             (SETQ PK (CAAR U))
             (SETQ K (LENGTH (CADR U)))
             (SETQ NUMR
                     (CONSTIMES
                      (CONS (CADAR U) (MK-NUMR NDIM* K (PLUS K PK)))))
             (RETURN NUMR))))) 
(PUT 'MK-NUMR 'NUMBER-OF-ARGS 3) 
(PUT 'MK-NUMR 'DEFINED-ON-LINE '202) 
(PUT 'MK-NUMR 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MK-NUMR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK-NUMR (N K P)
    (COND ((EQUAL K P) NIL)
          (T
           (CONS (COND ((EQUAL K 0) N) (T (LIST 'DIFFERENCE N K)))
                 (MK-NUMR N (ADD1 K) P))))) 
(PUT 'MOD-INDEX 'NUMBER-OF-ARGS 2) 
(PUT 'MOD-INDEX 'DEFINED-ON-LINE '206) 
(PUT 'MOD-INDEX 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MOD-INDEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD-INDEX (TERM DUMP)
    (PROG (COEFF SIGN)
      (SETQ COEFF (LIST 1))
      (SETQ TERM
              (COND ((SETQ SIGN (EQ (CAR TERM) 'MINUS)) (CDADR TERM))
                    (T (CDR TERM))))
      (PROG ()
       WHILELABEL
        (COND ((NOT TERM) (RETURN NIL)))
        (PROGN
         (COND ((FREE (CAR TERM)) (SETQ COEFF (CONS (CAR TERM) COEFF)))
               (T (SETQ DUMP (MOD-DUMP (CDAR TERM) DUMP))))
         (SETQ TERM (CDR TERM)))
        (GO WHILELABEL))
      (RETURN
       (CONS
        (COND
         (SIGN
          (COND ((NULL (CDR COEFF)) (MINUS 1))
                (T (CONS 'MINUS (LIST (CONSTIMES COEFF))))))
         ((NULL (CDR COEFF)) 1) (T (CONSTIMES COEFF)))
        DUMP)))) 
(PUT 'DPROPAGATOR 'NUMBER-OF-ARGS 3) 
(PUT 'DPROPAGATOR 'DEFINED-ON-LINE '229) 
(PUT 'DPROPAGATOR 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'DPROPAGATOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DPROPAGATOR (L1 L2 DUMP)
    ((LAMBDA (Z)
       (COND ((EQUAL Z 0) Z) ((EQUAL Z 1) (CONS NIL DUMP))
             (T
              (PROG (TRM FORALL-RESULT FORALL-ENDPTR)
                (SETQ TRM (CDR Z))
                (COND ((NULL TRM) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TRM) (MOD-INDEX TRM DUMP))
                                  (CAR TRM))
                                 NIL)))
               LOOPLABEL
                (SETQ TRM (CDR TRM))
                (COND ((NULL TRM) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (TRM) (MOD-INDEX TRM DUMP)) (CAR TRM))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))))
     (PROPAGATOR L1 L2))) 
(PUT 'DVERTEX-TO-PROJECTOR 'NUMBER-OF-ARGS 3) 
(PUT 'DVERTEX-TO-PROJECTOR 'DEFINED-ON-LINE '237) 
(PUT 'DVERTEX-TO-PROJECTOR 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'DVERTEX-TO-PROJECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DVERTEX-TO-PROJECTOR (SVERT RLST DUMP)
    (PROG (L1 L2 COEFF W)
      (SETQ L1 (MKTAILS (CADR SVERT) RLST DUMP))
      (COND ((REPEATSP (CAR L1)) (RETURN 0)))
      (SETQ L2 (MKTAILS (CADDR SVERT) RLST (CDR L1)))
      (COND ((REPEATSP (CAR L2)) (RETURN 0)))
      (SETQ DUMP (CDR L2))
      (SETQ W (PROP-SIMP (CAR L1) (REVERSE (CAR L2))))
      (SETQ COEFF (MK-CONTRACT-COEFF W))
      (RETURN (CONS COEFF (DPROPAGATOR (CADR W) (CDDR W) DUMP))))) 
(PUT 'DSTR-TO-ALG 'NUMBER-OF-ARGS 3) 
(PUT 'DSTR-TO-ALG 'DEFINED-ON-LINE '250) 
(PUT 'DSTR-TO-ALG 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'DSTR-TO-ALG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DSTR-TO-ALG (STRAND RLST DUMP)
    (COND ((NULL STRAND) (CONSRECIP (LIST (MK-COEFF1 DUMP RLST))))
          (T
           (PROG (VRTX)
             (SETQ VRTX (DVERTEX-TO-PROJECTOR (CAR STRAND) RLST DUMP))
             (COND ((EQUAL 0 VRTX) (RETURN 0)))
             (COND
              ((NULL (CADR VRTX))
               (RETURN
                (COND
                 ((EQUAL 1 (CAR VRTX))
                  (DSTR-TO-ALG (CDR STRAND) RLST (CDDR VRTX)))
                 (T
                  (CVITIMES2 (CAR VRTX)
                   (DSTR-TO-ALG (CDR STRAND) RLST (CDDR VRTX))))))))
             (RETURN
              (CVITIMES2 (CAR VRTX)
               (CONSPLUS
                (PROG (TRM FORALL-RESULT FORALL-ENDPTR)
                  (SETQ TRM (CDR VRTX))
                  (COND ((NULL TRM) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (TRM)
                                      (CVITIMES2 (CAR TRM)
                                       (DSTR-TO-ALG (CDR STRAND) RLST
                                        (CDR TRM))))
                                    (CAR TRM))
                                   NIL)))
                 LOOPLABEL
                  (SETQ TRM (CDR TRM))
                  (COND ((NULL TRM) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (TRM)
                              (CVITIMES2 (CAR TRM)
                               (DSTR-TO-ALG (CDR STRAND) RLST (CDR TRM))))
                            (CAR TRM))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))))))) 
(PUT 'CVITIMES2 'NUMBER-OF-ARGS 2) 
(PUT 'CVITIMES2 'DEFINED-ON-LINE '273) 
(PUT 'CVITIMES2 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'CVITIMES2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CVITIMES2 (X Y)
    (COND ((OR (EQUAL X 0) (EQUAL Y 0)) 0) ((EQUAL X 1) Y) ((EQUAL Y 1) X)
          (T (LIST 'TIMES X Y)))) 
(PUT 'FREE 'NUMBER-OF-ARGS 1) 
(PUT 'FREE 'DEFINED-ON-LINE '280) 
(PUT 'FREE 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'FREE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREE (DLT) (AND (FREEIND (CADR DLT)) (FREEIND (CADDR DLT)))) 
(PUT 'FREEIND 'NUMBER-OF-ARGS 1) 
(PUT 'FREEIND 'DEFINED-ON-LINE '283) 
(PUT 'FREEIND 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'FREEIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FREEIND (IND) (ATOM IND)) 
(PUT 'MOD-DUMP 'NUMBER-OF-ARGS 2) 
(PUT 'MOD-DUMP 'DEFINED-ON-LINE '288) 
(PUT 'MOD-DUMP 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MOD-DUMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD-DUMP (L DUMP)
    (COND ((NOT (FREEIND (CAR L))) (MOD-DUMP1 (CADR L) (CAR L) DUMP))
          (T (MOD-DUMP1 (CAR L) (CADR L) DUMP)))) 
(PUT 'MOD-DUMP1 'NUMBER-OF-ARGS 3) 
(PUT 'MOD-DUMP1 'DEFINED-ON-LINE '292) 
(PUT 'MOD-DUMP1 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'MOD-DUMP1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MOD-DUMP1 (NEW OLD DUMP)
    (COND ((NULL DUMP) NIL)
          (T
           (CONS (CONS (CAAR DUMP) (L-SUBST NEW OLD (CDAR DUMP)))
                 (MOD-DUMP1 NEW OLD (CDR DUMP)))))) 
(PUT 'L-SUBST 'NUMBER-OF-ARGS 3) 
(PUT 'L-SUBST 'DEFINED-ON-LINE '297) 
(PUT 'L-SUBST 'DEFINED-IN-FILE 'HEPHYS/EVALMAPS.RED) 
(PUT 'L-SUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE L-SUBST (NEW OLD L)
    (COND ((NULL L) NIL)
          ((EQUAL OLD (CAR L)) (CONS NEW (L-SUBST NEW OLD (CDR L))))
          (T (CONS (CAR L) (L-SUBST NEW OLD (CDR L)))))) 
(ENDMODULE) 