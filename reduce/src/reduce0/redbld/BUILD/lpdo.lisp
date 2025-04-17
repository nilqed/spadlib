(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LPDO)) 
(REVISION 'LPDO "$Id: lpdo.red 3816 2016-10-18 01:09:24Z schoepf $") 
(COPYRIGHT 'LPDO "(c) 2008-2016 Thomas Sturm") 
(FLUID '(*UTF8)) 
(COND (*UTF8 (ON1 'UTF8EXP))) 
(LOAD-PACKAGE 'REDLOG) 
(LOAD-PACKAGE 'OFSF) 
(RL_SET '(REALS)) 
(SWITCH (LIST 'LPDODF 'LPDOCOEFFNORM)) 
(ON1 'LPDODF) 
(ON1 'LPDOCOEFFNORM) 
(INFIX (LIST 'LPDOTIMES)) 
(FLAG '(LPDOTIMES) 'SPACED) 
(FLAG '(LPDOTIMES) 'NARY) 
(PRECEDENCE (LIST 'LPDOTIMES 'TIMES)) 
((LAMBDA (*MSG) (NEWTOK '((* * *) LPDOTIMES))) NIL) 
(PUT '*LPDO 'RTYPEFN 'QUOTELPDO) 
(PUT '*LPDO 'PRIFN 'LPDO_PRI*LPDO) 
(PUT '*LPDO 'FANCY-PRIFN 'LPDO_PRI*LPDO) 
(PUT '*LPDO 'FANCY-SETPRIFN 'LPDO_SETPRI*LPDO) 
(PUT '*LPDO 'LPDO_SIMPFN 'LPDO_SIMP*LPDO) 
(PUT 'PLUS 'RTYPEFN 'GETRTYPEOR) 
(PUT 'PLUS 'LPDO_SIMPFN 'LPDO_SIMPPLUS) 
(PUT 'MINUS 'RTYPEFN 'GETRTYPEOR) 
(PUT 'MINUS 'LPDO_SIMPFN 'LPDO_SIMPMINUS) 
(PUT 'DIFFERENCE 'RTYPEFN 'GETRTYPEOR) 
(PUT 'DIFFERENCE 'LPDO_SIMPFN 'LPDO_SIMPDIFFERENCE) 
(PUT 'EXPT 'RTYPEFN 'GETRTYPEOR) 
(PUT 'EXPT 'LPDO_SIMPFN 'LPDO_SIMPEXPT) 
(PUT 'LPDOTIMES 'RTYPEFN 'QUOTELPDO) 
(PUT 'LPDOTIMES 'LPDO_SIMPFN 'LPDO_SIMPLPDOTIMES) 
(PUT 'TIMES 'RTYPEFN 'GETRTYPEOR) 
(PUT 'TIMES 'LPDO_SIMPFN 'LPDO_SIMPLPDOTIMES) 
(PUT 'ABS 'LPDO_SIMPFN 'LPDO_SIMPABS) 
(PUT 'QUOTIENT 'LPDO_SIMPFN 'LPDO_SIMPQUOTIENT) 
(PUT 'PARTIAL 'RTYPEFN 'QUOTELPDO) 
(COND
 ((RL_TEXMACSP)
  (PROGN
   (PUT 'PARTIAL 'FANCY-FUNCTIONSYMBOL "\\partial ")
   (PUT 'PARTIAL 'FANCY-PRIFN 'LPDO_FANCY-PRIPARTIAL)))) 
(PUT 'PARTIAL 'LPDO_SIMPFN 'LPDO_SIMPPARTIAL) 
(PUT 'DIFF 'SIMPFN 'LPDO_SIMPDIFF1) 
(PUT 'DIFF 'LPDO_SIMPFN 'LPDO_SIMPDIFF) 
(PUT 'LPDO 'EVFN 'LPDO_REVAL) 
(PUT 'LPDO 'SUBFN 'LPDO_SUBST0) 
(PUT 'LPDO 'TAG '*LPDO) 
(FLUID '(LPDOPOL* *RLVERBOSE SHAPESYM*)) 
(SETQ SHAPESYM* '|#|) 
(COND ((NULL LPDOPOL*) (SETQ LPDOPOL* (CONS (MINUS 1) NIL)))) 
(PUT 'LPDOSET 'PSOPFN 'LPDO_SET) 
(PUT 'LPDOWEYL 'PSOPFN 'LPDO_WEYL) 
(PUT 'LPDORAT 'PSOPFN 'LPDO_RAT) 
(PUT 'LPDO_SET 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SET 'DEFINED-ON-LINE '109) 
(PUT 'LPDO_SET 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SET (L)
    (PROG (W)
      (COND ((NULL L) (RETURN (CONS 'LIST LPDOPOL*))))
      (RMSUBS)
      (SETQ W LPDOPOL*)
      (COND ((AND (NULL (CDR L)) (EQCAR (CAR L) 'LIST)) (SETQ L (CDAR L)))
            ((EQCAR L (MINUS 1)) (SETQ L (CONS (MINUS 1) NIL)))
            (T (SETQ L (CDR (REVAL1 (CAR L) T)))))
      (SETQ LPDOPOL* (CONS (CAR L) (SORT (CDR L) 'ORDP)))
      (RETURN (CONS 'LIST W)))) 
(PUT 'LPDO_WEYL 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_WEYL 'DEFINED-ON-LINE '125) 
(PUT 'LPDO_WEYL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_WEYL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_WEYL (L)
    (CONS 'LIST
          (CONS (LPDO_POLXPND1 SHAPESYM* (CDR L) (CAR L) 'TIMES 'REVAL)
                (CDR L)))) 
(PUT 'LPDO_RAT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_RAT 'DEFINED-ON-LINE '128) 
(PUT 'LPDO_RAT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_RAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_RAT (L)
    (PROG (NSYM DSYM NUM DEN)
      (SETQ NSYM (INTERN (COMPRESS (NCONC (EXPLODE SHAPESYM*) (LIST '_ 'N)))))
      (SETQ DSYM (INTERN (COMPRESS (NCONC (EXPLODE SHAPESYM*) (LIST '_ 'D)))))
      (SETQ NUM (LPDO_POLXPND1 NSYM (CDDR L) (CAR L) 'TIMES 'REVAL))
      (SETQ DEN (LPDO_POLXPND1 DSYM (CDDR L) (CADR L) 'TIMES 'REVAL))
      (RETURN (CONS 'LIST (CONS (LIST 'QUOTIENT NUM DEN) (CDDR L)))))) 
(PUT 'LPDO_TEMPLP 'NUMBER-OF-ARGS 0) 
(PUT 'LPDO_TEMPLP 'DEFINED-ON-LINE '138) 
(PUT 'LPDO_TEMPLP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_TEMPLP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LPDO_TEMPLP NIL
    (AND (NEQ (CAR LPDOPOL*) (MINUS 1)) (NOT (NULL (CDR LPDOPOL*))))) 
(PUT 'QUOTELPDO 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTELPDO 'DEFINED-ON-LINE '141) 
(PUT 'QUOTELPDO 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'QUOTELPDO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTELPDO (X) 'LPDO) 
(PUT 'LPDO_PRI*LPDO 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PRI*LPDO 'DEFINED-ON-LINE '144) 
(PUT 'LPDO_PRI*LPDO 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PRI*LPDO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PRI*LPDO (U) (MAPRIN (LPDO_PREP (CADR U)))) 
(PUT 'LPDO_SETPRI*LPDO 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SETPRI*LPDO 'DEFINED-ON-LINE '147) 
(PUT 'LPDO_SETPRI*LPDO 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SETPRI*LPDO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SETPRI*LPDO (X U)
    (PROGN (FANCY-MAPRINT X 0) (FANCY-PRIN2* ":=" 4) (LPDO_PRI*LPDO U))) 
(PUT 'LPDO_FANCY-PRIPARTIAL 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_FANCY-PRIPARTIAL 'DEFINED-ON-LINE '154) 
(PUT 'LPDO_FANCY-PRIPARTIAL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FANCY-PRIPARTIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_FANCY-PRIPARTIAL (U)
    (COND ((NULL *NAT) 'FAILED)
          (T
           (PROGN
            (FANCY-PREFIX-OPERATOR (CAR U))
            (FANCY-MAPRINT-ATOM (CADR U) 0))))) 
(PUT 'LPDO_REVAL 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_REVAL 'DEFINED-ON-LINE '162) 
(PUT 'LPDO_REVAL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_REVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_REVAL (U V)
    (COND (V (LPDO_PREP (LPDO_SIMP U))) (T (LPDO_MK*LPDO (LPDO_SIMP U))))) 
(PUT 'LPDO_SIMP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMP 'DEFINED-ON-LINE '165) 
(PUT 'LPDO_SIMP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMP (U)
    (PROG (W)
      (COND ((ATOM U) (RETURN (LPDO_SIMPATOM U))))
      (ARGNOCHK U)
      (COND
       ((FLAGP (CAR U) 'OPFN)
        (RETURN
         (LPDO_SIMP
          (APPLY (CAR U)
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR U))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                         NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))
      (COND
       ((SETQ W (GET (CAR U) 'LPDO_SIMPFN))
        (RETURN
         (COND ((FLAGP W 'FULL) (APPLY W (LIST U)))
               (T (APPLY W (LIST (CDR U))))))))
      (COND
       ((SETQ W (GET (CAR U) 'PSOPFN))
        (RETURN (LPDO_SIMP (APPLY1 W (CDR U))))))
      (COND
       ((SETQ W (GET (CAR U) 'PREPFN2))
        (RETURN (LPDO_SIMP (APPLY W (LIST U))))))
      (COND ((CDR U) (TYPERR U "lpdo (no arguments permitted)")))
      (COND
       ((NEQ (GET U 'SIMPFN) 'LPDO_XPND)
        (PROGN
         (PUT (CAR U) 'SIMPFN 'LPDO_XPND)
         (FLAG (LIST (CAR U)) 'FULL)
         (RMSUBS))))
      (RETURN (LIST (CONS (LPDO_XPND U) NIL))))) 
(PUT 'LPDO_XPND 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_XPND 'DEFINED-ON-LINE '187) 
(PUT 'LPDO_XPND 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_XPND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_XPND (A)
    (PROG (VL)
      (COND ((NOT (LPDO_TEMPLP)) (RETURN (MKSQ A 1))))
      (SETQ VL (CDR LPDOPOL*))
      (COND ((NULL VL) (RETURN (MKSQ A 1))))
      (SETQ A (CAR A))
      (RETURN (LPDO_TXPND A)))) 
(PUT 'LPDO_TXPND 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_TXPND 'DEFINED-ON-LINE '196) 
(PUT 'LPDO_TXPND 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_TXPND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_TXPND (A) (SIMP (LPDO_TXPND1 A (CAR LPDOPOL*)))) 
(PUT 'LPDO_TXPND1 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_TXPND1 'DEFINED-ON-LINE '199) 
(PUT 'LPDO_TXPND1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_TXPND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_TXPND1 (A TEMPL)
    (PROG (W)
      (COND
       ((AND (IDP TEMPL) (EQCAR (SETQ W (EXPLODE2 TEMPL)) SHAPESYM*))
        (RETURN (INTERN (COMPRESS (NCONC (EXPLODE2 A) (CDR W)))))))
      (COND ((ATOM TEMPL) (RETURN TEMPL)))
      (RETURN
       (CONS (CAR TEMPL)
             (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
               (SETQ ARG (CDR TEMPL))
               (COND ((NULL ARG) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ARG) (LPDO_TXPND1 A ARG)) (CAR ARG))
                                NIL)))
              LOOPLABEL
               (SETQ ARG (CDR ARG))
               (COND ((NULL ARG) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (ARG) (LPDO_TXPND1 A ARG)) (CAR ARG))
                             NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'LPDO_POLXPND1 'NUMBER-OF-ARGS 5) 
(PUT 'LPDO_POLXPND1 'DEFINED-ON-LINE '208) 
(PUT 'LPDO_POLXPND1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_POLXPND1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_POLXPND1 (A VL1 DEG TM SM)
    (PROG (W V TUPL ATUP VL)
      (SETQ TUPL (LPDO_TUPL (LENGTH VL1) DEG))
      (SETQ W
              (PROG (TUP FORALL-RESULT FORALL-ENDPTR)
                (SETQ TUP TUPL)
                (COND ((NULL TUP) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TUP)
                                    (PROGN
                                     (SETQ VL VL1)
                                     (SETQ ATUP
                                             (COND
                                              ((IDP A)
                                               (MKID
                                                (LPDO_MKID A (CONS '_ TUP))
                                                '_))
                                              (T
                                               (LIST
                                                (LPDO_MKID (CAR A)
                                                 (CONS '_ TUP))))))
                                     (LPDO_SMKN TM
                                      (CONS ATUP
                                            (PROG (D FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ D TUP)
                                             STARTOVER
                                              (COND ((NULL D) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      ((LAMBDA (D)
                                                         (PROGN
                                                          (SETQ V (CAR VL))
                                                          (SETQ VL (CDR VL))
                                                          (COND
                                                           ((EQN D 1) (LIST V))
                                                           ((GREATERP D 1)
                                                            (LIST
                                                             (LIST 'EXPT V
                                                                   D))))))
                                                       (CAR D)))
                                              (SETQ FORALL-ENDPTR
                                                      (LASTPAIR FORALL-RESULT))
                                              (SETQ D (CDR D))
                                              (COND
                                               ((ATOM FORALL-ENDPTR)
                                                (GO STARTOVER)))
                                             LOOPLABEL
                                              (COND
                                               ((NULL D)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      ((LAMBDA (D)
                                                         (PROGN
                                                          (SETQ V (CAR VL))
                                                          (SETQ VL (CDR VL))
                                                          (COND
                                                           ((EQN D 1) (LIST V))
                                                           ((GREATERP D 1)
                                                            (LIST
                                                             (LIST 'EXPT V
                                                                   D))))))
                                                       (CAR D)))
                                              (SETQ FORALL-ENDPTR
                                                      (LASTPAIR FORALL-ENDPTR))
                                              (SETQ D (CDR D))
                                              (GO LOOPLABEL))))))
                                  (CAR TUP))
                                 NIL)))
               LOOPLABEL
                (SETQ TUP (CDR TUP))
                (COND ((NULL TUP) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (TUP)
                            (PROGN
                             (SETQ VL VL1)
                             (SETQ ATUP
                                     (COND
                                      ((IDP A)
                                       (MKID (LPDO_MKID A (CONS '_ TUP)) '_))
                                      (T
                                       (LIST
                                        (LPDO_MKID (CAR A) (CONS '_ TUP))))))
                             (LPDO_SMKN TM
                              (CONS ATUP
                                    (PROG (D FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ D TUP)
                                     STARTOVER
                                      (COND ((NULL D) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              ((LAMBDA (D)
                                                 (PROGN
                                                  (SETQ V (CAR VL))
                                                  (SETQ VL (CDR VL))
                                                  (COND ((EQN D 1) (LIST V))
                                                        ((GREATERP D 1)
                                                         (LIST
                                                          (LIST 'EXPT V D))))))
                                               (CAR D)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-RESULT))
                                      (SETQ D (CDR D))
                                      (COND
                                       ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                     LOOPLABEL
                                      (COND ((NULL D) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              ((LAMBDA (D)
                                                 (PROGN
                                                  (SETQ V (CAR VL))
                                                  (SETQ VL (CDR VL))
                                                  (COND ((EQN D 1) (LIST V))
                                                        ((GREATERP D 1)
                                                         (LIST
                                                          (LIST 'EXPT V D))))))
                                               (CAR D)))
                                      (SETQ FORALL-ENDPTR
                                              (LASTPAIR FORALL-ENDPTR))
                                      (SETQ D (CDR D))
                                      (GO LOOPLABEL))))))
                          (CAR TUP))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (APPLY SM (LIST (LPDO_SMKN 'PLUS W)))))) 
(PUT 'LPDO_TUPL 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_TUPL 'DEFINED-ON-LINE '226) 
(PUT 'LPDO_TUPL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_TUPL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_TUPL (K N)
    (COND
     ((EQN K 1)
      (PROG (I FORALL-RESULT FORALL-ENDPTR)
        (SETQ I 0)
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (LIST I) NIL)))
       LOOPLABEL
        (SETQ I (PLUS2 I 1))
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR (CONS (LIST I) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL)))
     (T
      (PROG (I FORALL-RESULT FORALL-ENDPTR)
        (SETQ I 0)
       STARTOVER
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (PROG (R FORALL-RESULT FORALL-ENDPTR)
                  (SETQ R (LPDO_TUPL (DIFFERENCE K 1) (DIFFERENCE N I)))
                  (COND ((NULL R) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (R) (CONS I R)) (CAR R))
                                        NIL)))
                 LOOPLABEL
                  (SETQ R (CDR R))
                  (COND ((NULL R) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (R) (CONS I R)) (CAR R)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ I (PLUS2 I 1))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (PROG (R FORALL-RESULT FORALL-ENDPTR)
                  (SETQ R (LPDO_TUPL (DIFFERENCE K 1) (DIFFERENCE N I)))
                  (COND ((NULL R) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (R) (CONS I R)) (CAR R))
                                        NIL)))
                 LOOPLABEL
                  (SETQ R (CDR R))
                  (COND ((NULL R) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (R) (CONS I R)) (CAR R)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ I (PLUS2 I 1))
        (GO LOOPLABEL))))) 
(PUT 'LPDO_MKID 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_MKID 'DEFINED-ON-LINE '233) 
(PUT 'LPDO_MKID 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MKID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_MKID (ID L)
    (COND ((NULL L) ID) (T (LPDO_MKID (MKID ID (CAR L)) (CDR L))))) 
(PUT 'LPDO_SIMPATOM 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPATOM 'DEFINED-ON-LINE '236) 
(PUT 'LPDO_SIMPATOM 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPATOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPATOM (U)
    (PROG (W)
      (COND ((NULL U) (TYPERR "nil" "lpdo")))
      (COND ((STRINGP U) (TYPERR (LIST "string" U) "lpdo")))
      (COND
       ((SETQ W (LPDO_GETTYPE U))
        (PROGN
         (COND
          ((OR (EQ W 'LPDO) (EQ W 'SCALAR))
           (RETURN (LPDO_SIMP (CADR (GET U 'AVALUE))))))
         (TYPERR (LIST W U) "lpdo"))))
      (COND ((OR (NUMBERP U) (IDP U)) (RETURN (LIST (CONS (SIMP U) NIL)))))
      (TYPERR U "lpdo"))) 
(PUT 'LPDO_GETTYPE 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_GETTYPE 'DEFINED-ON-LINE '249) 
(PUT 'LPDO_GETTYPE 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_GETTYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_GETTYPE (V)
    ((LAMBDA (W) (COND (W (CAR W)) (T (GET V 'RTYPE)))) (GET V 'AVALUE))) 
(PUT 'LPDO_RESIMP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_RESIMP 'DEFINED-ON-LINE '254) 
(PUT 'LPDO_RESIMP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_RESIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_RESIMP (DP) (LPDO_SIMP (LPDO_PREP DP))) 
(PUT 'LPDO_PREP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PREP 'DEFINED-ON-LINE '257) 
(PUT 'LPDO_PREP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PREP (DP)
    (COND ((NULL DP) 0) ((NULL (CDR DP)) (LPDO_PREPLPDOTIMES (CAR DP)))
          (T
           (CONS 'PLUS
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X DP)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (LPDO_PREPLPDOTIMES X))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (LPDO_PREPLPDOTIMES X)) (CAR X))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'LPDO_PREPLPDOTIMES 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PREPLPDOTIMES 'DEFINED-ON-LINE '265) 
(PUT 'LPDO_PREPLPDOTIMES 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PREPLPDOTIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PREPLPDOTIMES (X)
    (PROG (W NEG)
      (COND ((NULL (CDR X)) (RETURN (PREPSQ (CAR X)))))
      (SETQ W (PREPSQ (CAR X)))
      (COND ((EQCAR W 'MINUS) (PROGN (SETQ NEG T) (SETQ W (CADR W)))))
      (SETQ W
              (COND ((EQN W 1) (LPDO_SMKN 'TIMES (CDR X)))
                    (T (CONS 'TIMES (CONS W (CDR X))))))
      (COND (NEG (SETQ W (LIST 'MINUS W))))
      (RETURN W))) 
(PUT 'LPDO_MK*LPDO 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_MK*LPDO 'DEFINED-ON-LINE '285) 
(PUT 'LPDO_MK*LPDO 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MK*LPDO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_MK*LPDO (DP)
    (CONS '*LPDO (CONS DP (COND (*RESUBS *SQVAR*) (T (LIST NIL)))))) 
(PUT 'LPDO_SIMP*LPDO 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMP*LPDO 'DEFINED-ON-LINE '288) 
(PUT 'LPDO_SIMP*LPDO 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMP*LPDO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMP*LPDO (U) (COND ((CADR U) (CAR U)) (T (LPDO_RESIMP (CAR U))))) 
(PUT 'LPDO_SIMPPARTIAL 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPPARTIAL 'DEFINED-ON-LINE '294) 
(PUT 'LPDO_SIMPPARTIAL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPPARTIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPPARTIAL (X)
    (PROGN
     (LPDO_PARTIALCHK X)
     (LIST (CONS (CONS 1 1) (LIST (CONS 'PARTIAL X)))))) 
(PUT 'LPDO_PARTIALCHK 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PARTIALCHK 'DEFINED-ON-LINE '300) 
(PUT 'LPDO_PARTIALCHK 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PARTIALCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PARTIALCHK (X)
    (COND ((NOT (IDP (CAR X))) (TYPERR (CAR X) "variable")))) 
(PUT 'LPDO_SIMPLPDOTIMES 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPLPDOTIMES 'DEFINED-ON-LINE '303) 
(PUT 'LPDO_SIMPLPDOTIMES 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPLPDOTIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPLPDOTIMES (U)
    (LPDO_MULTN
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LPDO_SIMP X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (LPDO_SIMP X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_MULTN 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_MULTN 'DEFINED-ON-LINE '306) 
(PUT 'LPDO_MULTN 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MULTN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_MULTN (L)
    (LPDO_COMPACT
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X (LPDO_CARTPROD L))
      STARTOVER
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT ((LAMBDA (X) (LPDO_COMMUTE X)) (CAR X)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ X (CDR X))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR ((LAMBDA (X) (LPDO_COMMUTE X)) (CAR X)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ X (CDR X))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_MULT 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_MULT 'DEFINED-ON-LINE '309) 
(PUT 'LPDO_MULT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_MULT (DP1 DP2) (LPDO_MULTN (LIST DP1 DP2))) 
(PUT 'LPDO_CARTPROD 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_CARTPROD 'DEFINED-ON-LINE '312) 
(PUT 'LPDO_CARTPROD 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_CARTPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_CARTPROD (SUML)
    (PROG (A W)
      (COND ((NULL SUML) (RETURN NIL)))
      (COND
       ((NULL (CDR SUML))
        (RETURN
         (PROG (M FORALL-RESULT FORALL-ENDPTR)
           (SETQ M (CAR SUML))
           (COND ((NULL M) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (M) (LIST M)) (CAR M)) NIL)))
          LOOPLABEL
           (SETQ M (CDR M))
           (COND ((NULL M) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (M) (LIST M)) (CAR M)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (SETQ A (CAR SUML))
      (SETQ W (LPDO_CARTPROD (CDR SUML)))
      (RETURN
       (PROG (M FORALL-RESULT FORALL-ENDPTR)
         (SETQ M A)
        STARTOVER
         (COND ((NULL M) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (M)
                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Y W)
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (Y) (CONS M Y)) (CAR Y))
                                            NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (Y) (CONS M Y)) (CAR Y)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR M)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ M (CDR M))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL M) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (M)
                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Y W)
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (Y) (CONS M Y)) (CAR Y))
                                            NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (Y) (CONS M Y)) (CAR Y)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR M)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ M (CDR M))
         (GO LOOPLABEL))))) 
(PUT 'LPDO_COMMUTE 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_COMMUTE 'DEFINED-ON-LINE '325) 
(PUT 'LPDO_COMMUTE 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COMMUTE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_COMMUTE (U) (COND (U (LPDO_COMMUTE2 (CAR U) (LPDO_COMMUTE (CDR U)))))) 
(PUT 'LPDO_COMMUTE2 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_COMMUTE2 'DEFINED-ON-LINE '330) 
(PUT 'LPDO_COMMUTE2 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COMMUTE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_COMMUTE2 (A D)
    (COND ((NULL D) (LIST A))
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X D)
            STARTOVER
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT ((LAMBDA (X) (LPDO_COMMUTE21 A X)) (CAR X)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ X (CDR X))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR ((LAMBDA (X) (LPDO_COMMUTE21 A X)) (CAR X)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ X (CDR X))
             (GO LOOPLABEL))))) 
(PUT 'LPDO_COMMUTE21 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_COMMUTE21 'DEFINED-ON-LINE '335) 
(PUT 'LPDO_COMMUTE21 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COMMUTE21 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_COMMUTE21 (A D)
    (PROG (P1 D1 P2 D2 V W PT P2DIFF AK N)
      (SETQ N 0)
      (SETQ P1 (CAR A))
      (SETQ D1 (CDR A))
      (SETQ P2 (CAR D))
      (SETQ D2 (CDR D))
      (COND
       ((OR
         (AND (OR (ATOM (CAR P2)) (ATOM (CAR (CAR P2))))
              (OR (ATOM (CDR P2)) (ATOM (CAR (CDR P2)))))
         (NULL D1))
        (RETURN (LIST (CONS (MULTSQ P1 P2) (APPEND D1 D2))))))
      (SETQ PT (CAR D1))
      (SETQ D1 (CDR D1))
      (SETQ V (LPDO_PARTIALV PT))
      (SETQ N (LPDO_PARTIALDEG PT))
      (SETQ W
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K N)
               STARTOVER
                (COND
                 ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 K))) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROGN
                         (SETQ P2DIFF
                                 (MULTSQ (SIMP (CHOOSE K N))
                                         (LPDO_DF (PREPSQ P2) V
                                          (DIFFERENCE N K))))
                         (SETQ AK (LPDO_AK PT K))
                         (LPDO_COMMUTE21 (CONS P1 D1)
                          (CONS P2DIFF (COND (AK (CONS AK D2)) (T D2))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ K (PLUS2 K (MINUS 1)))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 K)))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROGN
                         (SETQ P2DIFF
                                 (MULTSQ (SIMP (CHOOSE K N))
                                         (LPDO_DF (PREPSQ P2) V
                                          (DIFFERENCE N K))))
                         (SETQ AK (LPDO_AK PT K))
                         (LPDO_COMMUTE21 (CONS P1 D1)
                          (CONS P2DIFF (COND (AK (CONS AK D2)) (T D2))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ K (PLUS2 K (MINUS 1)))
                (GO LOOPLABEL)))
      (RETURN W))) 
(PUT 'LPDO_AK 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_AK 'DEFINED-ON-LINE '357) 
(PUT 'LPDO_AK 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_AK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_AK (A K)
    (COND ((EQN K 0) NIL) ((EQN K 1) (LPDO_PARTIALBAS A))
          (T (LIST 'EXPT (LPDO_PARTIALBAS A) K)))) 
(PUT 'LPDO_COMPACT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_COMPACT 'DEFINED-ON-LINE '365) 
(PUT 'LPDO_COMPACT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COMPACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_COMPACT (DP)
    (PROG (W AL)
      (PROG (M)
        (SETQ M DP)
       LAB
        (COND ((NULL M) (RETURN NIL)))
        ((LAMBDA (M)
           (SETQ AL (LPDO_ALINSERT AL (LPDO_COMPACTPT (CDR M)) (CAR M))))
         (CAR M))
        (SETQ M (CDR M))
        (GO LAB))
      (SETQ AL (SORT AL (FUNCTION (LAMBDA (X1 X2) (ORDP (CAR X1) (CAR X2))))))
      (SETQ W
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR AL)
               STARTOVER
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PR)
                           (COND
                            ((NOT (NULL (CAR (CDR PR))))
                             (LIST (CONS (CDR PR) (CAR PR))))))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PR (CDR PR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PR)
                           (COND
                            ((NOT (NULL (CAR (CDR PR))))
                             (LIST (CONS (CDR PR) (CAR PR))))))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PR (CDR PR))
                (GO LOOPLABEL)))
      (RETURN W))) 
(PUT 'LPDO_ALINSERT 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_ALINSERT 'DEFINED-ON-LINE '375) 
(PUT 'LPDO_ALINSERT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ALINSERT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ALINSERT (AL KEY P)
    (PROG (W SM)
      (SETQ W (ASSOC KEY AL))
      (COND ((NULL W) (RETURN (CONS (CONS KEY P) AL))))
      (SETQ SM (ADDSQ (CDR W) P))
      (COND ((NULL (CAR SM)) (RETURN (DELETE W AL))))
      (SETCDR W SM)
      (RETURN AL))) 
(PUT 'LPDO_COMPACTPT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_COMPACTPT 'DEFINED-ON-LINE '387) 
(PUT 'LPDO_COMPACTPT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COMPACTPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_COMPACTPT (L)
    (PROG (AL V N)
      (SETQ N 0)
      (PROG (PT)
        (SETQ PT L)
       LAB
        (COND ((NULL PT) (RETURN NIL)))
        ((LAMBDA (PT)
           (PROGN
            (SETQ V (LPDO_PARTIALV PT))
            (SETQ N (LPDO_PARTIALDEG PT))
            (SETQ AL (LPDO_ALINSERTPT AL V N))))
         (CAR PT))
        (SETQ PT (CDR PT))
        (GO LAB))
      (SETQ AL (SORT AL (FUNCTION (LAMBDA (X1 X2) (ORDP (CAR X1) (CAR X2))))))
      (RETURN
       (PROG (PR FORALL-RESULT FORALL-ENDPTR)
         (SETQ PR AL)
         (COND ((NULL PR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (PR)
                             (LPDO_EXPTPT (LIST 'PARTIAL (CAR PR)) (CDR PR)))
                           (CAR PR))
                          NIL)))
        LOOPLABEL
         (SETQ PR (CDR PR))
         (COND ((NULL PR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (PR)
                     (LPDO_EXPTPT (LIST 'PARTIAL (CAR PR)) (CDR PR)))
                   (CAR PR))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'LPDO_ALINSERTPT 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_ALINSERTPT 'DEFINED-ON-LINE '399) 
(PUT 'LPDO_ALINSERTPT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ALINSERTPT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ALINSERTPT (AL V N)
    (PROG (W)
      (SETQ W (ASSOC V AL))
      (COND ((NULL W) (RETURN (CONS (CONS V N) AL))))
      (SETCDR W (PLUS (CDR W) N))
      (RETURN AL))) 
(PUT 'LPDO_SIMPDIFFERENCE 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPDIFFERENCE 'DEFINED-ON-LINE '408) 
(PUT 'LPDO_SIMPDIFFERENCE 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPDIFFERENCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPDIFFERENCE (U)
    (LPDO_SIMP (LIST 'PLUS (CAR U) (LIST 'MINUS (CADR U))))) 
(PUT 'LPDO_SIMPPLUS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPPLUS 'DEFINED-ON-LINE '411) 
(PUT 'LPDO_SIMPPLUS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPPLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPPLUS (U)
    (LPDO_ADDN
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X U)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LPDO_SIMP X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (LPDO_SIMP X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_ADDN 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_ADDN 'DEFINED-ON-LINE '414) 
(PUT 'LPDO_ADDN 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ADDN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_ADDN (L)
    (LPDO_COMPACT
     (PROG (DP FORALL-RESULT FORALL-ENDPTR)
       (SETQ DP L)
      STARTOVER
       (COND ((NULL DP) (RETURN NIL)))
       (SETQ FORALL-RESULT ((LAMBDA (DP) (COPY DP)) (CAR DP)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ DP (CDR DP))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL DP) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR ((LAMBDA (DP) (COPY DP)) (CAR DP)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ DP (CDR DP))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_ADD 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_ADD 'DEFINED-ON-LINE '417) 
(PUT 'LPDO_ADD 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ADD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_ADD (DP1 DP2) (LPDO_ADDN (LIST DP1 DP2))) 
(PUT 'LPDO_SIMPMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPMINUS 'DEFINED-ON-LINE '420) 
(PUT 'LPDO_SIMPMINUS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPMINUS (U) (LPDO_MINUS (LPDO_SIMP (CAR U)))) 
(PUT 'LPDO_MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_MINUS 'DEFINED-ON-LINE '423) 
(PUT 'LPDO_MINUS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_MINUS (DP)
    (PROG (MON FORALL-RESULT FORALL-ENDPTR)
      (SETQ MON DP)
      (COND ((NULL MON) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (MON) (CONS (NEGSQ (CAR MON)) (CDR MON)))
                        (CAR MON))
                       NIL)))
     LOOPLABEL
      (SETQ MON (CDR MON))
      (COND ((NULL MON) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (MON) (CONS (NEGSQ (CAR MON)) (CDR MON))) (CAR MON))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'LPDO_SIMPQUOTIENT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPQUOTIENT 'DEFINED-ON-LINE '426) 
(PUT 'LPDO_SIMPQUOTIENT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPQUOTIENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPQUOTIENT (U) (LPDO_QUOT (LPDO_SIMP (CAR U)) (LPDO_SIMP (CADR U)))) 
(PUT 'LPDO_QUOT 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_QUOT 'DEFINED-ON-LINE '429) 
(PUT 'LPDO_QUOT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_QUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_QUOT (N D)
    (PROGN
     (COND
      ((OR (CDR D) (CDAR D))
       (TYPERR (CONS 'QUOTIENT (LIST N D)) "lpdo (partial in denominator)")))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X N)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (X)
                           (CONS (MULTSQ (CAR X) (INVSQ (CAAR D))) (CDR X)))
                         (CAR X))
                        NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (X) (CONS (MULTSQ (CAR X) (INVSQ (CAAR D))) (CDR X)))
                 (CAR X))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_SIMPTIMES 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPTIMES 'DEFINED-ON-LINE '437) 
(PUT 'LPDO_SIMPTIMES 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPTIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPTIMES (U)
    (PROG (W) (SETQ W (SIMP (CONS 'TIMES U))) (RETURN (LIST (CONS W NIL))))) 
(PUT 'LPDO_SIMPABS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPABS 'DEFINED-ON-LINE '443) 
(PUT 'LPDO_SIMPABS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPABS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPABS (U)
    (PROG (W) (SETQ W (SIMP (CONS 'ABS U))) (RETURN (LIST (CONS W NIL))))) 
(PUT 'LPDO_SIMPEXPT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPEXPT 'DEFINED-ON-LINE '449) 
(PUT 'LPDO_SIMPEXPT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPEXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPEXPT (U)
    (PROG (D)
      (SETQ D 0)
      (SETQ D (REVAL1 (CADR U) T))
      (COND ((NOT (NUMBERP D)) (TYPERR U "lpdo exponent")))
      (COND ((EQN D 0) (RETURN (LPDO_SIMP 1))))
      (COND
       ((EQCAR (CAR U) 'PARTIAL)
        (PROGN
         (LPDO_PARTIALCHK (CAR U))
         (RETURN (LIST (CONS (CONS 1 1) (LIST (LIST 'EXPT (CAR U) D))))))))
      (RETURN
       (LPDO_SIMP
        (CONS 'LPDOTIMES
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (CAR U) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE D I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (CAR U) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'LPDO_SIMPDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPDIFF 'DEFINED-ON-LINE '463) 
(PUT 'LPDO_SIMPDIFF 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPDIFF (L) (LIST (CONS (LPDO_SIMPDIFF1 L) NIL))) 
(PUT 'LPDO_SIMPDIFF1 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SIMPDIFF1 'DEFINED-ON-LINE '466) 
(PUT 'LPDO_SIMPDIFF1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SIMPDIFF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SIMPDIFF1 (L)
    (PROG (W C)
      (SETQ W (SIMP (CAR L)))
      (COND
       ((AND *LPDODF (LPDO_IDLP (KERNELS (CAR W)))
             (LPDO_IDLP (KERNELS (CDR W))))
        (RETURN (SIMP (CONS 'DF L)))))
      (COND ((AND (CDDR L) (EQN (CADDR L) 0)) (RETURN W)))
      (SETQ C (SFTO_DCONTENTF (CAR W)))
      (COND
       ((NEQ C 1)
        (RETURN
         (MULTSQ (CONS C 1)
                 (MKSQ
                  (CONS 'DIFF
                        (CONS (PREPSQ (MULTSQ W (INVSQ (CONS C 1)))) (CDR L)))
                  1)))))
      (SETQ C (SFTO_DCONTENTF (CDR W)))
      (COND
       ((NEQ C 1)
        (RETURN
         (MULTSQ
          (MKSQ (CONS 'DIFF (CONS (PREPSQ (MULTSQ W (CONS C 1))) (CDR L))) 1)
          (INVSQ (CONS C 1))))))
      (RETURN (MKSQ (CONS 'DIFF (REVLIS L)) 1)))) 
(PUT 'LPDO_IDLP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_IDLP 'DEFINED-ON-LINE '485) 
(PUT 'LPDO_IDLP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_IDLP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_IDLP (L) (OR (NULL L) (AND (IDP (CAR L)) (LPDO_IDLP (CDR L))))) 
(PUT 'LPDO_SMKN 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SMKN 'DEFINED-ON-LINE '488) 
(PUT 'LPDO_SMKN 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SMKN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SMKN (OP ARGL)
    (COND ((NULL ARGL) (COND ((EQ OP 'PLUS) 0) (T 1)))
          ((NULL (CDR ARGL)) (CAR ARGL)) (T (CONS OP ARGL)))) 
(PUT 'LPDO_PARTIALV 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PARTIALV 'DEFINED-ON-LINE '495) 
(PUT 'LPDO_PARTIALV 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PARTIALV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PARTIALV (U)
    (COND ((EQCAR U 'PARTIAL) (CADR U))
          ((OR (EQCAR U 'MINUS) (EQCAR U 'EXPT)) (LPDO_PARTIALV (CADR U))))) 
(PUT 'LPDO_PARTIALDEG 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PARTIALDEG 'DEFINED-ON-LINE '501) 
(PUT 'LPDO_PARTIALDEG 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PARTIALDEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PARTIALDEG (U)
    (COND ((EQCAR U 'PARTIAL) 1) ((EQCAR U 'MINUS) (LPDO_PARTIALDEG (CADR U)))
          ((AND (EQCAR U 'EXPT) (EQCAR (CADR U) 'PARTIAL)) (CADDR U)) (T 0))) 
(PUT 'LPDO_PARTIALBAS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PARTIALBAS 'DEFINED-ON-LINE '511) 
(PUT 'LPDO_PARTIALBAS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PARTIALBAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PARTIALBAS (U)
    (COND ((EQCAR U 'PARTIAL) U)
          ((OR (EQCAR U 'MINUS) (EQCAR U 'EXPT)) (LPDO_PARTIALBAS (CADR U)))
          (T (REDERR (LIST "lpdo_partialbas: unexpected term" U))))) 
(PUT 'LPDO_DF 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_DF 'DEFINED-ON-LINE '519) 
(PUT 'LPDO_DF 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_DF (U X N)
    (COND ((AND *LPDODF (LPDO_TEMPLP)) (SIMP (LIST 'DF U X N)))
          (T (LPDO_SIMPDIFF1 (LIST U X N))))) 
(PUT 'LPDO_D2C 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_D2C 'DEFINED-ON-LINE '525) 
(PUT 'LPDO_D2C 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_D2C 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_D2C (U) (LPDO_SUB U 'LPDOTIMES 'TIMES)) 
(PUT 'LPDO_C2D 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_C2D 'DEFINED-ON-LINE '528) 
(PUT 'LPDO_C2D 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_C2D 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_C2D (U) (LPDO_SUB U 'TIMES 'LPDOTIMES)) 
(PUT 'LPDO_SUB 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_SUB 'DEFINED-ON-LINE '531) 
(PUT 'LPDO_SUB 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_SUB (U TIM1 TIM2)
    (COND ((ATOM U) U)
          (T
           (CONS (COND ((EQ (CAR U) TIM1) TIM2) (T (CAR U)))
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR U))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (LPDO_SUB X TIM1 TIM2))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (LPDO_SUB X TIM1 TIM2)) (CAR X))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'LPDO_EXPTPT 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_EXPTPT 'DEFINED-ON-LINE '538) 
(PUT 'LPDO_EXPTPT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_EXPTPT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_EXPTPT (PT N)
    (PROG (D B)
      (SETQ B (LPDO_PARTIALBAS PT))
      (SETQ D (TIMES (LPDO_PARTIALDEG PT) N))
      (COND ((EQN D 0) (REDERR (LIST "lpdo_exptpt: degree zero"))))
      (COND ((EQN D 1) (RETURN B)))
      (RETURN (LIST 'EXPT B D)))) 
(PUT 'LPDOORD 'PSOPFN 'LPDO_ORD$) 
(PUT 'LPDO_ORD$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_ORD$ 'DEFINED-ON-LINE '549) 
(PUT 'LPDO_ORD$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ORD$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_ORD$ (L)
    (PROGN (LPDO_ARGNOCHK 'LPDOSYM 1 1 L) (LPDO_ORD (LPDO_SIMP (CAR L))))) 
(PUT 'LPDO_ORD 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_ORD 'DEFINED-ON-LINE '555) 
(PUT 'LPDO_ORD 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_ORD (DP)
    (PROG (ORD MONORD)
      (SETQ ORD 0)
      (SETQ MONORD 0)
      (PROG (MON)
        (SETQ MON DP)
       LAB
        (COND ((NULL MON) (RETURN NIL)))
        ((LAMBDA (MON)
           (PROGN
            (SETQ MONORD (LPDO_MONORD MON))
            (COND ((GREATERP MONORD ORD) (SETQ ORD MONORD)))))
         (CAR MON))
        (SETQ MON (CDR MON))
        (GO LAB))
      (RETURN ORD))) 
(PUT 'LPDO_MONORD 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_MONORD 'DEFINED-ON-LINE '564) 
(PUT 'LPDO_MONORD 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MONORD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_MONORD (MON)
    (PROG (PT FORALL-RESULT)
      (SETQ PT (CDR MON))
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL PT) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (PT) (LPDO_PARTIALDEG PT)) (CAR PT))
                    FORALL-RESULT))
      (SETQ PT (CDR PT))
      (GO LAB1))) 
(PUT 'LPDOSYM 'PSOPFN 'LPDO_SYM$) 
(PUT 'LPDO_SYM$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SYM$ 'DEFINED-ON-LINE '569) 
(PUT 'LPDO_SYM$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SYM$ (L)
    (PROG (W)
      (LPDO_ARGNOCHK 'LPDOSYM 1 3 L)
      (SETQ W (LPDO_SIMP (CAR L)))
      (RETURN
       (MK*SQ
        (LPDO_SYM W (COND ((CDR L) (REVAL1 (CADR L) T)) (T (LPDO_ORD W)))
         (COND ((AND (CDR L) (CDDR L)) (CADDR L)) (T 'Y))))))) 
(SWITCH (LIST 'LPDOTRSYM)) 
(PUT 'LPDO_SYM 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_SYM 'DEFINED-ON-LINE '579) 
(PUT 'LPDO_SYM 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_SYM (D M Y)
    (PROG (RES)
      (SETQ RES (CONS NIL 1))
      (COND ((LESSP M 0) (RETURN RES)))
      (PROG (MON)
        (SETQ MON D)
       LAB
        (COND ((NULL MON) (RETURN NIL)))
        ((LAMBDA (MON)
           (COND
            ((EQN (LPDO_MONORD MON) M)
             (SETQ RES
                     (ADDSQ RES
                            (MULTSQ (CAR MON) (LPDO_PTL2SYM (CDR MON) Y)))))))
         (CAR MON))
        (SETQ MON (CDR MON))
        (GO LAB))
      (COND
       (*LPDOTRSYM
        (PROGN
         (MATHPRINT (LPDO_PREP D))
         (IOTO_PRIN2T (LIST "sym_" M "="))
         (MAPRIN (PREPSQ RES))
         (IOTO_FLUSH))))
      (RETURN RES))) 
(PUT 'LPDO_PTL2SYM 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_PTL2SYM 'DEFINED-ON-LINE '596) 
(PUT 'LPDO_PTL2SYM 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PTL2SYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_PTL2SYM (PTL Y)
    (PROG (V D W)
      (SETQ W 1)
      (PROG (PT)
        (SETQ PT PTL)
       LAB
        (COND ((NULL PT) (RETURN NIL)))
        ((LAMBDA (PT)
           (PROGN
            (SETQ V (LPDO_PARTIALV PT))
            (SETQ D (LPDO_PARTIALDEG PT))
            (SETQ W
                    ((LAMBDA (G125)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G125))
                             (T (POLY-MULTF W G125))))
                     (EXPTF
                      (LIST (CONS (CONS (LPDO_MKID Y (LIST '_ V '_)) 1) 1))
                      D)))))
         (CAR PT))
        (SETQ PT (CDR PT))
        (GO LAB))
      (RETURN (CONS W 1)))) 
(PUT 'LPDO_SUBST0 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SUBST0 'DEFINED-ON-LINE '607) 
(PUT 'LPDO_SUBST0 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SUBST0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SUBST0 (AL D) (LPDO_MK*LPDO (LPDO_SUBST AL (LPDO_SIMP D)))) 
(PUT 'LPDO_SUBST 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SUBST 'DEFINED-ON-LINE '610) 
(PUT 'LPDO_SUBST 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SUBST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SUBST (AL D)
    (PROG (W)
      (RETURN
       (PROG (DMON FORALL-RESULT FORALL-ENDPTR)
         (SETQ DMON D)
        STARTOVER
         (COND ((NULL DMON) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (DMON)
                    (PROGN
                     (SETQ W (SUBSQ (CAR DMON) AL))
                     (COND ((CAR W) (LIST (CONS W (CDR DMON)))))))
                  (CAR DMON)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ DMON (CDR DMON))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL DMON) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (DMON)
                    (PROGN
                     (SETQ W (SUBSQ (CAR DMON) AL))
                     (COND ((CAR W) (LIST (CONS W (CDR DMON)))))))
                  (CAR DMON)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ DMON (CDR DMON))
         (GO LOOPLABEL))))) 
(PUT 'LPDOS 'PSOPFN 'LPDO_S$) 
(PUT 'LPDO_S$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_S$ 'DEFINED-ON-LINE '620) 
(PUT 'LPDO_S$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_S$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_S$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDO_S 3 4 L)
     (MK*SQ
      (LPDO_S (LPDO_SIMP (CAR L)) (LPDO_SIMP (CADR L)) (REVAL1 (CADDR L) T)
       (COND ((CDDDR L) (CADDDR L)) (T 'Y)))))) 
(PUT 'LPDO_S 'NUMBER-OF-ARGS 4) 
(PUT 'LPDO_S 'DEFINED-ON-LINE '627) 
(PUT 'LPDO_S 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_S 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_S (P Q M Y)
    (PROG (RES)
      (SETQ RES (CONS NIL 1))
      (PROG (MON)
        (SETQ MON Q)
       LAB
        (COND ((NULL MON) (RETURN NIL)))
        ((LAMBDA (MON)
           (COND
            ((EQN (LPDO_MONORD MON) M)
             (SETQ RES
                     (ADDSQ RES
                            (MULTSQ (LPDO_S1 P (CAR MON))
                                    (LPDO_PTL2SYM (CDR MON) Y)))))))
         (CAR MON))
        (SETQ MON (CDR MON))
        (GO LAB))
      (RETURN RES))) 
(PUT 'LPDO_S1 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_S1 'DEFINED-ON-LINE '637) 
(PUT 'LPDO_S1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_S1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_S1 (P QJ)
    (PROG (RES V D W)
      (SETQ RES (CONS NIL 1))
      (PROG (MON)
        (SETQ MON P)
       LAB
        (COND ((NULL MON) (RETURN NIL)))
        ((LAMBDA (MON)
           (PROGN
            (COND
             ((CDR MON)
              (PROGN
               (SETQ V (LPDO_PARTIALV (CAR (CDR MON))))
               (SETQ D (LPDO_PARTIALDEG (CAR (CDR MON))))
               (SETQ W (MULTSQ (CAR MON) (LPDO_DF (PREPSQ QJ) V D)))))
             (T (SETQ W (MULTSQ (CAR MON) QJ))))
            (SETQ RES (ADDSQ RES W))))
         (CAR MON))
        (SETQ MON (CDR MON))
        (GO LAB))
      (RETURN RES))) 
(PUT 'LPDOFAC 'PSOPFN 'LPDO_FAC$) 
(PUT 'LPDO_FAC$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_FAC$ 'DEFINED-ON-LINE '654) 
(PUT 'LPDO_FAC$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FAC$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_FAC$ (L)
    (PROG (W D P Q Y PRULEL QRULEL)
      (LPDO_ARGNOCHK 'LPDOFAC 1 4 L)
      (SETQ D (LPDO_SIMP (CAR L)))
      (SETQ L (CDR L))
      (COND (L (PROGN (SETQ P (LPDO_SIMP (CAR L))) (SETQ L (CDR L))))
            (T (SETQ P '(P))))
      (COND (L (PROGN (SETQ Q (LPDO_SIMP (CAR L))) (SETQ L (CDR L))))
            (T (SETQ Q '(Q))))
      (COND (L (PROGN (SETQ Y (CAR L)) (SETQ L (CDR L)))) (T (SETQ Y 'Y)))
      (SETQ W (LPDO_FAC D P Q Y))
      (RETURN (RL_MK*FOF W)))) 
(PUT 'LPDO_FAC 'NUMBER-OF-ARGS 4) 
(PUT 'LPDO_FAC 'DEFINED-ON-LINE '667) 
(PUT 'LPDO_FAC 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FAC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FAC (D P Q Y)
    (PROG (W)
      (COND
       ((NOT (LPDO_TEMPLP))
        (REDERR "lpdo_fac: use lpdoset to fix delta ring")))
      (COND ((LPDO_GENFUNP P) (SETQ P (LPDO_P2PP P D))))
      (COND ((LPDO_GENFUNP Q) (SETQ Q (LPDO_Q2QQ Q D))))
      (SETQ W
              (PROG (LHS FORALL-RESULT FORALL-ENDPTR)
                (SETQ LHS (LPDO_FACLHSL D P Q Y))
               STARTOVER
                (COND ((NULL LHS) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (LHS)
                           (COND
                            ((OR (ATOM (CDR LHS)) (ATOM (CAR (CDR LHS))))
                             (LIST (LIST 'EQUAL (CAR LHS) NIL)))
                            (T
                             (LIST (LIST 'EQUAL (CAR LHS) NIL)
                                   (LIST 'NEQ (CDR LHS) NIL)))))
                         (CAR LHS)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ LHS (CDR LHS))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL LHS) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (LHS)
                           (COND
                            ((OR (ATOM (CDR LHS)) (ATOM (CAR (CDR LHS))))
                             (LIST (LIST 'EQUAL (CAR LHS) NIL)))
                            (T
                             (LIST (LIST 'EQUAL (CAR LHS) NIL)
                                   (LIST 'NEQ (CDR LHS) NIL)))))
                         (CAR LHS)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ LHS (CDR LHS))
                (GO LOOPLABEL)))
      (RETURN
       (LPDO_FACQUANTIFY
        (COND ((AND W (CDR W)) (CONS 'AND W))
              ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE))) (T (CAR W)))
        D P Q Y)))) 
(PUT 'LPDO_P2PP 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_P2PP 'DEFINED-ON-LINE '693) 
(PUT 'LPDO_P2PP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_P2PP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_P2PP (P D) (LPDO_POLXPND1 P (LPDO_PTL D) 1 'LPDOTIMES 'LPDO_SIMP)) 
(PUT 'LPDO_Q2QQ 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_Q2QQ 'DEFINED-ON-LINE '696) 
(PUT 'LPDO_Q2QQ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_Q2QQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_Q2QQ (Q D)
    (LPDO_POLXPND1 Q (LPDO_PTL D) (DIFFERENCE (LPDO_ORD D) 1) 'LPDOTIMES
     'LPDO_SIMP)) 
(PUT 'LPDO_FACLHSL 'NUMBER-OF-ARGS 4) 
(PUT 'LPDO_FACLHSL 'DEFINED-ON-LINE '699) 
(PUT 'LPDO_FACLHSL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACLHSL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FACLHSL (D PP QQ Y)
    (PROG (LHS RHS)
      (RETURN
       (PROG (I FORALL-RESULT FORALL-ENDPTR)
         (SETQ I (LPDO_ORD D))
         (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (PROGN
                           (SETQ LHS
                                   (ADDSQ (LPDO_SYM D I Y)
                                          (NEGSQ (LPDO_S PP QQ I Y))))
                           (SETQ RHS
                                   (MULTSQ (LPDO_SYM PP 1 Y)
                                           (LPDO_SYM QQ (DIFFERENCE I 1) Y)))
                           (SETQ LHS (ADDSQ LHS (NEGSQ RHS)))
                           LHS)
                          NIL)))
        LOOPLABEL
         (SETQ I (PLUS2 I (MINUS 1)))
         (COND
          ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  (PROGN
                   (SETQ LHS
                           (ADDSQ (LPDO_SYM D I Y) (NEGSQ (LPDO_S PP QQ I Y))))
                   (SETQ RHS
                           (MULTSQ (LPDO_SYM PP 1 Y)
                                   (LPDO_SYM QQ (DIFFERENCE I 1) Y)))
                   (SETQ LHS (ADDSQ LHS (NEGSQ RHS)))
                   LHS)
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'LPDO_GENFUNP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_GENFUNP 'DEFINED-ON-LINE '709) 
(PUT 'LPDO_GENFUNP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_GENFUNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_GENFUNP (P) (AND (PAIRP P) (NULL (CDR P)) (ATOM (CAR P)))) 
(PUT 'LPDO_FACQUANTIFY 'NUMBER-OF-ARGS 5) 
(PUT 'LPDO_FACQUANTIFY 'DEFINED-ON-LINE '712) 
(PUT 'LPDO_FACQUANTIFY 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACQUANTIFY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FACQUANTIFY (PHI D PP QQ Y)
    (PROG (DCL PCL QCL PL QL XL YL W)
      (SETQ DCL (LPDO_COEFFS D))
      (SETQ PCL (LPDO_COEFFS PP))
      (SETQ QCL (LPDO_COEFFS QQ))
      (PROG (V)
        (SETQ V (RL_FVARL PHI))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (COND ((MEMQ V (CDR LPDOPOL*)) (SETQ XL (CONS V XL)))
                 ((AND (SETQ W (LPDO_MYKERNP V)) (EQCAR W Y))
                  (SETQ YL (CONS V YL)))
                 ((AND (MEMQ V PCL) (NOT (MEMQ V DCL))) (SETQ PL (CONS V PL)))
                 ((AND (MEMQ V QCL) (NOT (MEMQ V DCL)))
                  (SETQ QL (CONS V QL)))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (V)
        (SETQ V (REVERSIP (NCONC (SORT XL 'ORDP) (SORT YL 'ORDP))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ PHI (LIST 'ALL V PHI))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (PROG (V)
        (SETQ V (REVERSIP (NCONC (SORT PL 'ORDP) (SORT QL 'ORDP))))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (SETQ PHI (LIST 'EX V PHI))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN PHI))) 
(PUT 'LPDO_COEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_COEFFS 'DEFINED-ON-LINE '733) 
(PUT 'LPDO_COEFFS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_COEFFS (DP)
    (PROG (KL)
      (PROG (DMON)
        (SETQ DMON DP)
       LAB
        (COND ((NULL DMON) (RETURN NIL)))
        ((LAMBDA (DMON)
           (PROG (K)
             (SETQ K
                     (UNION (KERNELS (CAR (CAR DMON)))
                            (KERNELS (CDR (CAR DMON)))))
            LAB
             (COND ((NULL K) (RETURN NIL)))
             ((LAMBDA (K)
                (COND ((NOT (EQCAR K 'PARTIAL)) (SETQ KL (LTO_INSERTQ K KL)))))
              (CAR K))
             (SETQ K (CDR K))
             (GO LAB)))
         (CAR DMON))
        (SETQ DMON (CDR DMON))
        (GO LAB))
      (RETURN KL))) 
(PUT 'LPDO_MYKERNP 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_MYKERNP 'DEFINED-ON-LINE '742) 
(PUT 'LPDO_MYKERNP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MYKERNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_MYKERNP (ID)
    (PROG (V EXPL)
      (COND ((NOT (IDP ID)) (RETURN NIL)))
      (SETQ EXPL (REVERSIP (EXPLODE ID)))
      (COND ((NEQ (CAR EXPL) '_) (RETURN NIL)))
      (SETQ EXPL (CDR EXPL))
      (COND ((EQ (CAR EXPL) '!) (SETQ EXPL (CDR EXPL))))
      (SETQ EXPL (REVERSIP EXPL))
      (SETQ V (CAR EXPL))
      (SETQ EXPL (CDR EXPL))
      (COND
       ((AND (EQ (CAR EXPL) '!) (EQCAR (CDR EXPL) '_)) (SETQ EXPL (CDR EXPL))))
      (COND ((NEQ (CAR EXPL) '_) (RETURN NIL)))
      (SETQ EXPL (CDR EXPL))
      (RETURN (CONS (INTERN V) (INTERN (COMPRESS (CONS '! EXPL))))))) 
(PUT 'LPDOFACX 'PSOPFN 'LPDO_FACX$) 
(PUT 'LPDO_FACX$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_FACX$ 'DEFINED-ON-LINE '766) 
(PUT 'LPDO_FACX$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACX$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_FACX$ (L)
    (PROG (W D PSI P Q Y EPS)
      (LPDO_ARGNOCHK 'LPDOFACX 1 6 L)
      (SETQ D (LPDO_SIMP (PROG1 (CAR L) (SETQ L (CDR L)))))
      (SETQ PSI
              (RL_SIMP (COND (L (PROG1 (CAR L) (SETQ L (CDR L)))) (T 'TRUE))))
      (SETQ P (COND (L (LPDO_SIMP (PROG1 (CAR L) (SETQ L (CDR L))))) (T '(P))))
      (SETQ Q (COND (L (LPDO_SIMP (PROG1 (CAR L) (SETQ L (CDR L))))) (T '(Q))))
      (SETQ EPS (COND (L (PROG1 (CAR L) (SETQ L (CDR L)))) (T 'EPSILON)))
      (SETQ Y (COND (L (PROG1 (CAR L) (SETQ L (CDR L)))) (T 'Y)))
      (SETQ W (LPDO_FACX D PSI P Q EPS Y))
      (RETURN (RL_MK*FOF W)))) 
(PUT 'LPDO_FACX 'NUMBER-OF-ARGS 6) 
(PUT 'LPDO_FACX 'DEFINED-ON-LINE '779) 
(PUT 'LPDO_FACX 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FACX (D PSI P Q EPS Y)
    (PROG (W)
      (COND
       ((NOT (LPDO_TEMPLP))
        (REDERR "lpdo_fac: use lpdoset to fix delta ring")))
      (COND ((LPDO_GENFUNP P) (SETQ P (LPDO_P2PP P D))))
      (COND ((LPDO_GENFUNP Q) (SETQ Q (LPDO_P2PP Q D))))
      (SETQ W
              (COND
               (*LPDOCOEFFNORM
                (PROG (LHS FORALL-RESULT FORALL-ENDPTR)
                  (SETQ LHS (LPDO_FACLHSL D P Q Y))
                  (COND ((NULL LHS) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (LHS)
                                      (PROGN
                                       (COND
                                        ((NOT
                                          (OR (ATOM (CDR LHS))
                                              (ATOM (CAR (CDR LHS)))))
                                         (LPRIM
                                          (LIST
                                           "dropping denominator in equation:"
                                           (PREPF (CDR LHS))))))
                                       (LPDO_ABSLEQ (CAR LHS) EPS Y)))
                                    (CAR LHS))
                                   NIL)))
                 LOOPLABEL
                  (SETQ LHS (CDR LHS))
                  (COND ((NULL LHS) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (LHS)
                              (PROGN
                               (COND
                                ((NOT
                                  (OR (ATOM (CDR LHS)) (ATOM (CAR (CDR LHS)))))
                                 (LPRIM
                                  (LIST "dropping denominator in equation:"
                                        (PREPF (CDR LHS))))))
                               (LPDO_ABSLEQ (CAR LHS) EPS Y)))
                            (CAR LHS))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T
                (PROG (LHS FORALL-RESULT FORALL-ENDPTR)
                  (SETQ LHS (LPDO_FACLHSL D P Q Y))
                  (COND ((NULL LHS) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (LHS)
                                      (PROGN
                                       (COND
                                        ((NOT
                                          (OR (ATOM (CDR LHS))
                                              (ATOM (CAR (CDR LHS)))))
                                         (LPRIM
                                          (LIST
                                           "dropping denominator in equation:"
                                           (PREPF (CDR LHS))))))
                                       (LPDO_ABSLEQ_LASARUK (CAR LHS) EPS Y)))
                                    (CAR LHS))
                                   NIL)))
                 LOOPLABEL
                  (SETQ LHS (CDR LHS))
                  (COND ((NULL LHS) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (LHS)
                              (PROGN
                               (COND
                                ((NOT
                                  (OR (ATOM (CDR LHS)) (ATOM (CAR (CDR LHS)))))
                                 (LPRIM
                                  (LIST "dropping denominator in equation:"
                                        (PREPF (CDR LHS))))))
                               (LPDO_ABSLEQ_LASARUK (CAR LHS) EPS Y)))
                            (CAR LHS))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ W
              (LIST 'IMPL PSI
                    (COND ((AND W (CDR W)) (CONS 'AND W))
                          ((NULL W) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                          (T (CAR W)))))
      (RETURN (LPDO_FACQUANTIFY W D P Q Y)))) 
(PUT 'LPDO_ABSLEQ 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_ABSLEQ 'DEFINED-ON-LINE '804) 
(PUT 'LPDO_ABSLEQ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ABSLEQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ABSLEQ (LHS EPS Y)
    ((LAMBDA (G127)
       (COND ((AND G127 (CDR G127)) (CONS 'AND G127))
             ((NULL G127) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G127))))
     (PROG (F FORALL-RESULT FORALL-ENDPTR)
       (SETQ F (LPDO_ALLCOEFFS LHS (LPDO_YLIST LHS Y)))
       (COND ((NULL F) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (F)
                           (CONS 'AND
                                 (LIST
                                  (LIST 'LEQ
                                        (ADDF (NEGF F) (NEGF (CAR (SIMP EPS))))
                                        NIL)
                                  (LIST 'LEQ (ADDF F (NEGF (CAR (SIMP EPS))))
                                        NIL))))
                         (CAR F))
                        NIL)))
      LOOPLABEL
       (SETQ F (CDR F))
       (COND ((NULL F) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (F)
                   (CONS 'AND
                         (LIST
                          (LIST 'LEQ (ADDF (NEGF F) (NEGF (CAR (SIMP EPS))))
                                NIL)
                          (LIST 'LEQ (ADDF F (NEGF (CAR (SIMP EPS)))) NIL))))
                 (CAR F))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LPDO_ABSLEQ_LASARUK 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_ABSLEQ_LASARUK 'DEFINED-ON-LINE '810) 
(PUT 'LPDO_ABSLEQ_LASARUK 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ABSLEQ_LASARUK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ABSLEQ_LASARUK (LHS EPS Y)
    (PROG (S)
      (PROG (F)
        (SETQ F (LPDO_ALLCOEFFS LHS (LPDO_YLIST LHS Y)))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (SETQ S (ADDF S F))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN
       (CONS 'AND
             (LIST (LIST 'LEQ (ADDF (NEGF S) (NEGF (CAR (SIMP EPS)))) NIL)
                   (LIST 'LEQ (ADDF S (NEGF (CAR (SIMP EPS)))) NIL)))))) 
(PUT 'LPDO_ALLCOEFFS 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_ALLCOEFFS 'DEFINED-ON-LINE '819) 
(PUT 'LPDO_ALLCOEFFS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ALLCOEFFS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_ALLCOEFFS (F VL) (LPDO_ALLCOEFFS1 (LIST F) VL)) 
(PUT 'LPDO_ALLCOEFFS1 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_ALLCOEFFS1 'DEFINED-ON-LINE '822) 
(PUT 'LPDO_ALLCOEFFS1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ALLCOEFFS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_ALLCOEFFS1 (L VL)
    (COND ((NULL VL) L)
          (T
           (LPDO_ALLCOEFFS1
            (PROG (F FORALL-RESULT FORALL-ENDPTR)
              (SETQ F L)
             STARTOVER
              (COND ((NULL F) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      ((LAMBDA (F)
                         (LPDO_COEFS (SFTO_REORDER F (CAR VL)) (CAR VL)))
                       (CAR F)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
              (SETQ F (CDR F))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND ((NULL F) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      ((LAMBDA (F)
                         (LPDO_COEFS (SFTO_REORDER F (CAR VL)) (CAR VL)))
                       (CAR F)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ F (CDR F))
              (GO LOOPLABEL))
            (CDR VL))))) 
(PUT 'LPDO_YLIST 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_YLIST 'DEFINED-ON-LINE '829) 
(PUT 'LPDO_YLIST 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_YLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_YLIST (LHS Y)
    (PROG (W)
      (RETURN
       (PROG (V FORALL-RESULT FORALL-ENDPTR)
         (SETQ V (KERNELS LHS))
        STARTOVER
         (COND ((NULL V) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (V)
                    (PROGN
                     (SETQ W (LPDO_MYKERNP V))
                     (COND ((AND W (EQCAR W Y)) (LIST V)))))
                  (CAR V)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ V (CDR V))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL V) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (V)
                    (PROGN
                     (SETQ W (LPDO_MYKERNP V))
                     (COND ((AND W (EQCAR W Y)) (LIST V)))))
                  (CAR V)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ V (CDR V))
         (GO LOOPLABEL))))) 
(PUT 'LPDO_COEFS 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_COEFS 'DEFINED-ON-LINE '837) 
(PUT 'LPDO_COEFS 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_COEFS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_COEFS (F V)
    (COND
     ((AND (NOT (OR (ATOM F) (ATOM (CAR F)))) (EQ (CAAAR F) V)) (COEFFS F))
     (T (LIST F)))) 
(PUT 'LPDO_ABSLEQ_OLD 'NUMBER-OF-ARGS 3) 
(PUT 'LPDO_ABSLEQ_OLD 'DEFINED-ON-LINE '840) 
(PUT 'LPDO_ABSLEQ_OLD 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ABSLEQ_OLD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ABSLEQ_OLD (LHS EPS Y)
    (CONS 'AND
          (LIST (LIST 'LEQ (ADDF (NEGF LHS) (NEGF (CAR (SIMP EPS)))) NIL)
                (LIST 'LEQ (ADDF LHS (NEGF (CAR (SIMP EPS)))) NIL)))) 
(PUT 'LPDOPTL 'PSOPFN 'LPDO_PTL$) 
(PUT 'LPDO_PTL$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PTL$ 'DEFINED-ON-LINE '847) 
(PUT 'LPDO_PTL$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PTL$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PTL$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDOPTL 1 1 L)
     (CONS 'LIST (LPDO_PTL (LPDO_SIMP (CAR L)))))) 
(PUT 'LPDO_PTL 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_PTL 'DEFINED-ON-LINE '854) 
(PUT 'LPDO_PTL 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_PTL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_PTL (D)
    (PROG (RES)
      (PROG (MON)
        (SETQ MON D)
       LAB
        (COND ((NULL MON) (RETURN NIL)))
        ((LAMBDA (MON)
           (PROG (PT)
             (SETQ PT (CDR MON))
            LAB
             (COND ((NULL PT) (RETURN NIL)))
             ((LAMBDA (PT) (SETQ RES (LTO_INSERT (LPDO_PARTIALBAS PT) RES)))
              (CAR PT))
             (SETQ PT (CDR PT))
             (GO LAB)))
         (CAR MON))
        (SETQ MON (CDR MON))
        (GO LAB))
      (RETURN (SORT RES 'ORDP)))) 
(PUT 'LPDOGP 'PSOPFN 'LPDO_GP$) 
(PUT 'LPDO_GP$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_GP$ 'DEFINED-ON-LINE '866) 
(PUT 'LPDO_GP$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_GP$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_GP$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDOGP 3 3 L)
     (MK*SQ
      (LPDO_POLXPND1 (CAR L) (CDR (REVAL1 (CADR L) T)) (REVAL1 (CADDR L) T)
       'TIMES 'SIMP)))) 
(PUT 'LPDOGDP 'PSOPFN 'LPDO_GDP$) 
(PUT 'LPDO_GDP$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_GDP$ 'DEFINED-ON-LINE '875) 
(PUT 'LPDO_GDP$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_GDP$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_GDP$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDOGDP 3 3 L)
     (LPDO_MK*LPDO
      (LPDO_POLXPND1 (CAR L)
       (PROG (V FORALL-RESULT FORALL-ENDPTR)
         (SETQ V (CDR (REVAL1 (CADR L) T)))
         (COND ((NULL V) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (V)
                             (COND ((IDP V) (LIST 'PARTIAL V)) (T V)))
                           (CAR V))
                          NIL)))
        LOOPLABEL
         (SETQ V (CDR V))
         (COND ((NULL V) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (V) (COND ((IDP V) (LIST 'PARTIAL V)) (T V)))
                   (CAR V))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))
       (REVAL1 (CADDR L) T) 'LPDOTIMES 'LPDO_SIMP)))) 
(PUT 'LPDOSYM2DP 'PSOPFN 'LPDO_SYM2DP$) 
(PUT 'LPDO_SYM2DP$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_SYM2DP$ 'DEFINED-ON-LINE '887) 
(PUT 'LPDO_SYM2DP$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM2DP$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_SYM2DP$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDOSYM2DP 1 2 L)
     (LPDO_MK*LPDO
      (LPDO_SYM2DP (SIMP (CAR L)) (COND ((CDR L) (CADR L)) (T 'Y)))))) 
(PUT 'LPDO_SYM2DP 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SYM2DP 'DEFINED-ON-LINE '893) 
(PUT 'LPDO_SYM2DP 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM2DP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SYM2DP (Q V)
    (LPDO_QUOT (LPDO_SYM2DP1 (CAR Q) V) (LPDO_SIMP (PREPF (CDR Q))))) 
(PUT 'LPDO_SYM2DP1 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SYM2DP1 'DEFINED-ON-LINE '896) 
(PUT 'LPDO_SYM2DP1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM2DP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SYM2DP1 (F V)
    (COND ((OR (ATOM F) (ATOM (CAR F))) (LPDO_SIMP (PREPF F)))
          (T
           (LPDO_ADD
            (LPDO_MULT1 (LPDO_SYM2DP1 (CDAR F) V)
             (LPDO_SIMP (LIST 'EXPT (LPDO_SYM2DPV (CAAAR F) V) (CDAAR F))))
            (LPDO_SYM2DP1 (CDR F) V))))) 
(PUT 'LPDO_SYM2DPV 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_SYM2DPV 'DEFINED-ON-LINE '904) 
(PUT 'LPDO_SYM2DPV 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_SYM2DPV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_SYM2DPV (KN V)
    (PROG (EKN)
      (SETQ EKN (EXPLODE KN))
      (COND ((NOT (EQCAR EKN V)) (RETURN KN)))
      (SETQ EKN (CDR EKN))
      (COND ((NOT (EQCAR EKN '_)) (RETURN KN)))
      (SETQ EKN (REVERSIP (CDR EKN)))
      (COND ((NOT (EQCAR EKN '_)) (RETURN KN)))
      (SETQ EKN (REVERSIP (CDR EKN)))
      (RETURN (LIST 'PARTIAL (INTERN (COMPRESS EKN)))))) 
(PUT 'LPDO_MULT1 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_MULT1 'DEFINED-ON-LINE '916) 
(PUT 'LPDO_MULT1 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_MULT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_MULT1 (P Q)
    (LPDO_COMPACT
     (PROG (PMON FORALL-RESULT FORALL-ENDPTR)
       (SETQ PMON P)
      STARTOVER
       (COND ((NULL PMON) (RETURN NIL)))
       (SETQ FORALL-RESULT
               ((LAMBDA (PMON)
                  (PROG (QMON FORALL-RESULT FORALL-ENDPTR)
                    (SETQ QMON Q)
                    (COND ((NULL QMON) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (QMON)
                                        (CONS (MULTSQ (CAR PMON) (CAR QMON))
                                              (APPEND (CDR PMON) (CDR QMON))))
                                      (CAR QMON))
                                     NIL)))
                   LOOPLABEL
                    (SETQ QMON (CDR QMON))
                    (COND ((NULL QMON) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (QMON)
                                (CONS (MULTSQ (CAR PMON) (CAR QMON))
                                      (APPEND (CDR PMON) (CDR QMON))))
                              (CAR QMON))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (CAR PMON)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ PMON (CDR PMON))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL PMON) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               ((LAMBDA (PMON)
                  (PROG (QMON FORALL-RESULT FORALL-ENDPTR)
                    (SETQ QMON Q)
                    (COND ((NULL QMON) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (QMON)
                                        (CONS (MULTSQ (CAR PMON) (CAR QMON))
                                              (APPEND (CDR PMON) (CDR QMON))))
                                      (CAR QMON))
                                     NIL)))
                   LOOPLABEL
                    (SETQ QMON (CDR QMON))
                    (COND ((NULL QMON) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (QMON)
                                (CONS (MULTSQ (CAR PMON) (CAR QMON))
                                      (APPEND (CDR PMON) (CDR QMON))))
                              (CAR QMON))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (CAR PMON)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ PMON (CDR PMON))
       (GO LOOPLABEL)))) 
(PUT 'LPDOHRECT 'PSOPFN 'LPDO_HRECT$) 
(PUT 'LPDO_HRECT$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_HRECT$ 'DEFINED-ON-LINE '926) 
(PUT 'LPDO_HRECT$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_HRECT$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_HRECT$ (L)
    (PROGN (LPDO_ARGNOCHK 'LPDOHRECT 1 1 L) (RL_MK*FOF (LPDO_HRECT (CAR L))))) 
(PUT 'LPDO_HRECT 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_HRECT 'DEFINED-ON-LINE '932) 
(PUT 'LPDO_HRECT 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_HRECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_HRECT (M)
    (PROG (MV)
      (COND
       ((NOT (LPDO_TEMPLP))
        (REDERR "lpdo_hrect: use lpdoset to fix delta ring")))
      (RETURN
       (CONS 'AND
             (PROG (V FORALL-RESULT FORALL-ENDPTR)
               (SETQ V (CDR LPDOPOL*))
              STARTOVER
               (COND ((NULL V) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (V)
                          (PROGN
                           (SETQ MV
                                   (COND ((NUMBERP M) M)
                                         (T
                                          (LIST
                                           (CONS
                                            (CONS (LPDO_MKID M (LIST '_ V '_))
                                                  1)
                                            1)))))
                           (LIST
                            (LIST 'LEQ
                                  (ADDF (NEGF MV)
                                        (NEGF (LIST (CONS (CONS V 1) 1))))
                                  NIL)
                            (LIST 'LEQ
                                  (ADDF (LIST (CONS (CONS V 1) 1)) (NEGF MV))
                                  NIL))))
                        (CAR V)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ V (CDR V))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL V) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (V)
                          (PROGN
                           (SETQ MV
                                   (COND ((NUMBERP M) M)
                                         (T
                                          (LIST
                                           (CONS
                                            (CONS (LPDO_MKID M (LIST '_ V '_))
                                                  1)
                                            1)))))
                           (LIST
                            (LIST 'LEQ
                                  (ADDF (NEGF MV)
                                        (NEGF (LIST (CONS (CONS V 1) 1))))
                                  NIL)
                            (LIST 'LEQ
                                  (ADDF (LIST (CONS (CONS V 1) 1)) (NEGF MV))
                                  NIL))))
                        (CAR V)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ V (CDR V))
               (GO LOOPLABEL)))))) 
(PUT 'LPDOHCIRC 'PSOPFN 'LPDO_HCIRC$) 
(PUT 'LPDO_HCIRC$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_HCIRC$ 'DEFINED-ON-LINE '945) 
(PUT 'LPDO_HCIRC$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_HCIRC$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_HCIRC$ (L)
    (PROGN (LPDO_ARGNOCHK 'LPDOCIRC 1 1 L) (RL_MK*FOF (LPDO_HCIRC (CAR L))))) 
(PUT 'LPDO_HCIRC 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_HCIRC 'DEFINED-ON-LINE '951) 
(PUT 'LPDO_HCIRC 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_HCIRC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_HCIRC (R)
    (PROG (LHS)
      (COND
       ((NOT (LPDO_TEMPLP))
        (REDERR "lpdo_hcirc: use lpdoset to fix delta ring")))
      (SETQ R
              (COND ((NUMBERP R) (EXPT R 2))
                    (T (EXPTF (LIST (CONS (CONS R 1) 1)) 2))))
      (PROG (V)
        (SETQ V (CDR LPDOPOL*))
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (SETQ LHS (ADDF LHS (EXPTF (LIST (CONS (CONS V 1) 1)) 2))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN (LIST 'LEQ (ADDF LHS (NEGF R)) NIL)))) 
(PUT 'LPDOFACTORIZE 'PSOPFN 'LPDO_FACTORIZE$) 
(PUT 'LPDO_FACTORIZE$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_FACTORIZE$ 'DEFINED-ON-LINE '963) 
(PUT 'LPDO_FACTORIZE$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACTORIZE$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_FACTORIZE$ (L)
    (PROGN
     (LPDO_ARGNOCHK 'LPDOFACTORIZE 1 3 L)
     (REVAL1
      (LIST 'LPDO_FACTORIZE (CAR L)
            (COND ((CDR L) (CADR L)) (T (LIST 'LPDOGLFAC (CAR L))))
            (COND ((AND (CDR L) (CDDR L)) (CADDR L))
                  (T (LIST 'LPDOGOFAC (CAR L)))))
      T))) 
(PUT 'LPDO_FACTORIZE 'NUMBER-OF-ARGS 3) 
(FLAG '(LPDO_FACTORIZE) 'OPFN) 
(PUT 'LPDO_FACTORIZE 'DEFINED-ON-LINE '979) 
(PUT 'LPDO_FACTORIZE 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACTORIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FACTORIZE (F P Q)
    (PROG (FF SO P0 Q0 GAMMA FAILEDP W)
      (AEVAL (ON (LIST 'RLQEAPRECISE)))
      (SETQ FF (AEVAL (LIST 'LPDOFAC F P Q)))
      (SETQ SO (AEVAL (LIST 'RLQEA FF)))
      (SETQ SO
              (PROG (BRA FORALL-RESULT FORALL-ENDPTR)
                (SETQ BRA (GETRLIST (AEVAL SO)))
               STARTOVER
                (COND ((NULL BRA) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        ((LAMBDA (BRA)
                           (PROGN
                            (SETQ GAMMA
                                    (AEVAL (LIST 'RLSIMPL (LIST 'FIRST BRA))))
                            (COND
                             ((EVALEQUAL (AEVAL GAMMA) (AEVAL 'FALSE))
                              (AEVAL (LIST 'LIST)))
                             ((EVALEQUAL (AEVAL GAMMA) (AEVAL 'TRUE))
                              (PROGN
                               (SETQ P0
                                       (AEVAL
                                        (LIST 'SUB (LIST 'SECOND BRA) P)))
                               (SETQ Q0
                                       (AEVAL
                                        (LIST 'SUB (LIST 'SECOND BRA) Q)))
                               (AEVAL
                                (LIST 'LIST (LIST 'LPDO_FIXSIGN P0 Q0)))))
                             (T
                              (PROGN
                               (SETQ FAILEDP (AEVAL 'T))
                               (AEVAL (LIST 'LIST)))))))
                         (CAR BRA)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ BRA (CDR BRA))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL BRA) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         ((LAMBDA (BRA)
                            (PROGN
                             (SETQ GAMMA
                                     (AEVAL (LIST 'RLSIMPL (LIST 'FIRST BRA))))
                             (COND
                              ((EVALEQUAL (AEVAL GAMMA) (AEVAL 'FALSE))
                               (AEVAL (LIST 'LIST)))
                              ((EVALEQUAL (AEVAL GAMMA) (AEVAL 'TRUE))
                               (PROGN
                                (SETQ P0
                                        (AEVAL
                                         (LIST 'SUB (LIST 'SECOND BRA) P)))
                                (SETQ Q0
                                        (AEVAL
                                         (LIST 'SUB (LIST 'SECOND BRA) Q)))
                                (AEVAL
                                 (LIST 'LIST (LIST 'LPDO_FIXSIGN P0 Q0)))))
                              (T
                               (PROGN
                                (SETQ FAILEDP (AEVAL 'T))
                                (AEVAL (LIST 'LIST)))))))
                          (CAR BRA))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ BRA (CDR BRA))
                (GO LOOPLABEL)))
      (COND
       ((AND (EVALEQUAL (AEVAL SO) (AEVAL (LIST 'LIST))) (BOOLVALUE* FAILEDP))
        (RETURN (AEVAL 'FAILED))))
      (RETURN (AEVAL SO)))) 
(FLAG '(LPDO_FIXSIGN) 'OPFN) 
(PUT 'LPDO_FIXSIGN 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_FIXSIGN 'DEFINED-ON-LINE '1010) 
(PUT 'LPDO_FIXSIGN 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FIXSIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_FIXSIGN (P0 Q0)
    (PROGN
     (SETQ P0 (LPDO_SIMP P0))
     (SETQ Q0 (LPDO_SIMP Q0))
     (PROG (G128)
       (SETQ G128 (LPDO_FIXSIGN0 P0 Q0))
       (SETQ P0 (CAR G128))
       (SETQ Q0 (CDR G128))
       (RETURN G128))
     (LIST 'LIST (LPDO_MK*LPDO P0) (LPDO_MK*LPDO Q0)))) 
(PUT 'LPDO_FIXSIGN0 'NUMBER-OF-ARGS 2) 
(PUT 'LPDO_FIXSIGN0 'DEFINED-ON-LINE '1018) 
(PUT 'LPDO_FIXSIGN0 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FIXSIGN0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPDO_FIXSIGN0 (P0 Q0)
    (COND
     ((AND P0 (MINUSF (CAR (CAAR P0)))) (CONS (LPDO_MINUS P0) (LPDO_MINUS Q0)))
     (T (CONS P0 Q0)))) 
(PUT 'LPDOGLFAC 'NUMBER-OF-ARGS 1) 
(FLAG '(LPDOGLFAC) 'OPFN) 
(PUT 'LPDOGLFAC 'DEFINED-ON-LINE '1024) 
(PUT 'LPDOGLFAC 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDOGLFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDOGLFAC (F) (LIST 'LPDOGDP (LIST 'P) (LIST 'LPDOPTL F) 1)) 
(PUT 'LPDOGOFAC 'NUMBER-OF-ARGS 1) 
(FLAG '(LPDOGOFAC) 'OPFN) 
(PUT 'LPDOGOFAC 'DEFINED-ON-LINE '1027) 
(PUT 'LPDOGOFAC 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDOGOFAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDOGOFAC (F)
    (LIST 'LPDOGDP (LIST 'Q) (LIST 'LPDOPTL F)
          (LIST 'DIFFERENCE (LIST 'LPDOORD F) 1))) 
(PUT 'LPDOFACTORIZEX 'PSOPFN 'LPDO_FACTORIZEX$) 
(PUT 'LPDO_FACTORIZEX$ 'NUMBER-OF-ARGS 1) 
(PUT 'LPDO_FACTORIZEX$ 'DEFINED-ON-LINE '1032) 
(PUT 'LPDO_FACTORIZEX$ 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACTORIZEX$ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPDO_FACTORIZEX$ (L)
    (PROG (AF F PSI P Q EPS)
      (LPDO_ARGNOCHK 'LPDOFACTORIZEX 1 5 L)
      (SETQ AF (PROG1 (CAR L) (SETQ L (CDR L))))
      (SETQ F (LPDO_SIMP AF))
      (SETQ PSI
              (COND (L (RL_SIMP (PROG1 (CAR L) (SETQ L (CDR L))))) (T 'TRUE)))
      (SETQ P (COND (L (LPDO_SIMP (PROG1 (CAR L) (SETQ L (CDR L))))) (T '(P))))
      (SETQ Q (COND (L (LPDO_SIMP (PROG1 (CAR L) (SETQ L (CDR L))))) (T '(Q))))
      (SETQ EPS (COND (L (PROG1 (CAR L) (SETQ L (CDR L)))) (T 'EPSILON)))
      (RETURN
       (CONS 'LIST
             (PROG (BRA FORALL-RESULT FORALL-ENDPTR)
               (SETQ BRA (LPDO_FACTORIZEX F PSI P Q EPS))
               (COND ((NULL BRA) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (BRA)
                                   (LIST 'LIST (RL_PREPFOF (CAR BRA))
                                         (LIST 'LIST (LPDO_PREP (CAADR BRA))
                                               (LPDO_PREP (CADADR BRA)))))
                                 (CAR BRA))
                                NIL)))
              LOOPLABEL
               (SETQ BRA (CDR BRA))
               (COND ((NULL BRA) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (BRA)
                           (LIST 'LIST (RL_PREPFOF (CAR BRA))
                                 (LIST 'LIST (LPDO_PREP (CAADR BRA))
                                       (LPDO_PREP (CADADR BRA)))))
                         (CAR BRA))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'LPDO_FACTORIZEX 'NUMBER-OF-ARGS 5) 
(PUT 'LPDO_FACTORIZEX 'DEFINED-ON-LINE '1047) 
(PUT 'LPDO_FACTORIZEX 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_FACTORIZEX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_FACTORIZEX (F PSI P Q EPS)
    (PROG (FF SO P0 Q0 RES W GAMMA WW)
      (ON1 'RLQEAPRECISE)
      (COND
       ((NOT (LPDO_TEMPLP))
        (REDERR "lpdo_fac: use lpdoset to fix delta ring")))
      (COND ((LPDO_GENFUNP P) (SETQ P (LPDO_P2PP P F))))
      (COND ((LPDO_GENFUNP Q) (SETQ Q (LPDO_P2PP Q F))))
      (SETQ FF (LPDO_FACX F PSI P Q EPS 'Y))
      (SETQ SO (RL_QEA FF NIL))
      (PROG (BRA)
        (SETQ BRA SO)
       LAB
        (COND ((NULL BRA) (RETURN NIL)))
        ((LAMBDA (BRA)
           (PROGN
            (SETQ GAMMA (CAR BRA))
            (COND
             ((NEQ GAMMA 'FALSE)
              (PROGN
               (SETQ P0 (LPDO_SUBST (CDR BRA) P))
               (SETQ Q0 (LPDO_SUBST (CDR BRA) Q))
               (SETQ WW (LPDO_FIXSIGN0 P0 Q0))
               (SETQ W (LIST GAMMA (LIST (CAR WW) (CDR WW))))
               (SETQ RES (LTO_INSERT W RES)))))))
         (CAR BRA))
        (SETQ BRA (CDR BRA))
        (GO LAB))
      (RETURN (REVERSIP RES)))) 
(PUT 'LPDO_ARGNOCHK 'NUMBER-OF-ARGS 4) 
(PUT 'LPDO_ARGNOCHK 'DEFINED-ON-LINE '1071) 
(PUT 'LPDO_ARGNOCHK 'DEFINED-IN-FILE 'LPDO/LPDO.RED) 
(PUT 'LPDO_ARGNOCHK 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LPDO_ARGNOCHK (NAME MI MA ARGL)
    (PROG (LEN)
      (SETQ LEN 0)
      (SETQ LEN (LENGTH ARGL))
      (COND
       ((OR (LESSP LEN MI) (GREATERP LEN MA))
        (REDERR
         (LIST NAME "called with" LEN "aruments instead of" MI "-" MA)))))) 
(ENDMODULE) 