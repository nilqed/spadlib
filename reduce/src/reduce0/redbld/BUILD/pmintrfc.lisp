(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PMINTRFC)) 
(FLUID '(*TRPM RPT SUBFG* SUBSTITUTION VARSTACK*)) 
(SWITCH (LIST 'TRPM)) 
(PUT 'M 'PSOPFN 'MX) 
(PUT 'MX 'NUMBER-OF-ARGS 1) 
(PUT 'MX 'DEFINED-ON-LINE '82) 
(PUT 'MX 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'MX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MX (U) (PM_M1 (REVAL1 (CAR U) T) (REVAL1 (CADR U) T))) 
(PUT 'PM_M1 'NUMBER-OF-ARGS 2) 
(PUT 'PM_M1 'DEFINED-ON-LINE '84) 
(PUT 'PM_M1 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'PM_M1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PM_M1 (EXP TEMP)
    (PROG (SUBSTITUTION MMATCH COUNT FREEVARS)
      (SETQ COUNT 0)
      (SETQ FREEVARS (IDSORT (UNION (FINDNEWVARS TEMP) NIL)))
      (SETQ SUBSTITUTION (COND (FREEVARS FREEVARS) (T T)))
      (PROG (J)
        (SETQ J FREEVARS)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (PUT J 'BINDING (CONS 'UNBOUND (GET J 'BINDING))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (SETQ MMATCH (AMATCH TEMP EXP T NIL))
      (PROG (J)
        (SETQ J FREEVARS)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (PUT J 'BINDING (CDR (GET J 'BINDING)))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND
       (MMATCH
        (RETURN
         (COND
          (FREEVARS
           (CONS 'LIST
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (PAIR FREEVARS MMATCH))
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J) (LIST 'REP (CAR J) (CDR J)))
                                     (CAR J))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (CDR J))
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (J) (LIST 'REP (CAR J) (CDR J))) (CAR J))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T T))))))) 
(PUT 'FIXREPLIST 'NUMBER-OF-ARGS 1) 
(PUT 'FIXREPLIST 'DEFINED-ON-LINE '98) 
(PUT 'FIXREPLIST 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'FIXREPLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXREPLIST (REPSET)
    (PROG (REPLIST)
      (COND ((MEMQ (CAR REPSET) '(REP REPD)) (SETQ REPLIST (LIST REPSET)))
            (T (SETQ REPLIST (CDR REPSET))))
      (SETQ REPLIST
              (PROG (REP FORALL-RESULT FORALL-ENDPTR)
                (SETQ REP REPLIST)
                (COND ((NULL REP) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (REP) (FIXREP REP)) (CAR REP))
                                      NIL)))
               LOOPLABEL
                (SETQ REP (CDR REP))
                (COND ((NULL REP) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (REP) (FIXREP REP)) (CAR REP)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN REPLIST))) 
(FLUID '(|PM:GENSYM-COUNT*|)) 
(SETQ |PM:GENSYM-COUNT*| 0) 
(PUT '|PM:GENSYM| 'NUMBER-OF-ARGS 0) 
(PUT '|PM:GENSYM| 'DEFINED-ON-LINE '118) 
(PUT '|PM:GENSYM| 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT '|PM:GENSYM| 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE |PM:GENSYM| NIL
    (COMPRESS
     (CONS '?
           (CONS '_
                 (EXPLODE
                  (SETQ |PM:GENSYM-COUNT*| (PLUS |PM:GENSYM-COUNT*| 1))))))) 
(FLUID '(FREEVARLIST*)) 
(PUT 'MAKE-UNIQUE-FREEVARS 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE-UNIQUE-FREEVARS 'DEFINED-ON-LINE '124) 
(PUT 'MAKE-UNIQUE-FREEVARS 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'MAKE-UNIQUE-FREEVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE-UNIQUE-FREEVARS (FORM)
    (COND
     ((ATOM FORM)
      (COND
       ((GET FORM 'GEN)
        (PROG (X)
          (SETQ X (ATSOC FORM FREEVARLIST*))
          (COND
           ((NULL X)
            (PROGN
             (SETQ X (CONS FORM (|PM:GENSYM|)))
             (PUT (CDR X) 'GEN T)
             (SETQ FREEVARLIST* (CONS X FREEVARLIST*)))))
          (RETURN (CDR X))))
       (T FORM)))
     (T
      (PROG (X FORALL-RESULT FORALL-ENDPTR)
        (SETQ X FORM)
        (COND ((NULL X) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (MAKE-UNIQUE-FREEVARS X)) (CAR X))
                              NIL)))
       LOOPLABEL
        (SETQ X (CDR X))
        (COND ((NULL X) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS ((LAMBDA (X) (MAKE-UNIQUE-FREEVARS X)) (CAR X)) NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))))) 
(PUT 'FIXREP 'NUMBER-OF-ARGS 1) 
(PUT 'FIXREP 'DEFINED-ON-LINE '136) 
(PUT 'FIXREP 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'FIXREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXREP (REPL)
    (PROGN
     ((LAMBDA (FREEVARLIST*) (SETQ REPL (MAKE-UNIQUE-FREEVARS REPL))) NIL)
     (COND
      ((FLAGP (CAADR REPL) 'ASSOC)
       (COND
        ((FLAGP (CAADR REPL) 'SYMMETRIC)
         (LIST (CAR REPL) (APPEND (CADR REPL) (LIST '|??;|))
               (LIST (CAADR REPL) (CADDR REPL) '|??;|)))
        (T
         (LIST (CAR REPL)
               (CONS (CAADR REPL)
                     (CONS '??^ (APPEND (CDADR REPL) (LIST '|??;|))))
               (LIST (CAADR REPL) '??^ (CADDR REPL) '|??;|)))))
      (T REPL)))) 
(PUT 'S 'PSOPFN 'SX) 
(PUT 'SX 'NUMBER-OF-ARGS 1) 
(PUT 'SX 'DEFINED-ON-LINE '152) 
(PUT 'SX 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SX (ARG)
    (REVAL1
     (S0 (REVAL1 (CAR ARG) T) (REVAL1 (CADR ARG) T)
      (COND ((CDDR ARG) (REVAL1 (CADDR ARG) T)) (T 1))
      (COND ((AND (CDDR ARG) (CDDDR ARG)) (REVAL1 (CAR (CDDDR ARG)) T))
            (T 'INF)))
     T)) 
(PUT 'SI 'PSOPFN 'SI-X) 
(PUT 'SI-X 'NUMBER-OF-ARGS 1) 
(PUT 'SI-X 'DEFINED-ON-LINE '162) 
(PUT 'SI-X 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SI-X 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SI-X (ARG)
    (REVAL1
     (S0 (REVAL1 (CAR ARG) T) (REVAL1 (CADR ARG) T) 'INF
      (COND ((CDDR ARG) (REVAL1 (CADDR ARG) T)) (T 'INF)))
     T)) 
(PUT 'S0 'NUMBER-OF-ARGS 4) 
(PUT 'S0 'DEFINED-ON-LINE '168) 
(PUT 'S0 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'S0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE S0 (EXP REPSET RPT DEPTH)
    (COND
     ((OR (LEQ (LENGTH REPSET) 1) (NOT (MEMQ (CAR REPSET) '(REP REPD LIST))))
      EXP)
     ((OR (AND (NEQ DEPTH 'INF) (LESSP DEPTH 0))
          (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
      EXP)
     (T (SBREADTH EXP (FIXREPLIST REPSET) DEPTH)))) 
(PUT 'SBREADTH 'NUMBER-OF-ARGS 3) 
(PUT 'SBREADTH 'DEFINED-ON-LINE '176) 
(PUT 'SBREADTH 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SBREADTH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SBREADTH (EXP REPLIST DEPTH)
    (PROGN
     (SETQ EXP (SROOT EXP REPLIST))
     (COND
      ((OR (AND (NEQ DEPTH 'INF) (LEQ DEPTH 0))
           (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
       EXP)
      (T
       (SSBREADTH EXP REPLIST
        (COND ((NEQ DEPTH 'INF) (DIFFERENCE DEPTH 1)) (T DEPTH))))))) 
(PUT 'SSBREADTH 'NUMBER-OF-ARGS 3) 
(PUT 'SSBREADTH 'DEFINED-ON-LINE '185) 
(PUT 'SSBREADTH 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SSBREADTH 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SSBREADTH (EXP REPLIST DEPTH)
    (PROG (NEWEXP NEW REPS)
      (COND
       ((OR (AND (NEQ DEPTH 'INF) (LESSP DEPTH 0))
            (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
        (RETURN EXP)))
      (PROG ()
       REPEATLABEL
        (PROG ()
          (SETQ NEW NIL)
          (SETQ REPS REPLIST)
         A
          (SETQ EXP
                  (REVAL1
                   (PROG (SUBEXP FORALL-RESULT FORALL-ENDPTR)
                     (SETQ SUBEXP EXP)
                     (COND ((NULL SUBEXP) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (SUBEXP)
                                         (PROGN
                                          (SETQ NEWEXP
                                                  (SROOT1 SUBEXP (CAR REPS)))
                                          (SETQ NEW
                                                  (OR NEW (NEQ SUBEXP NEWEXP)))
                                          NEWEXP))
                                       (CAR SUBEXP))
                                      NIL)))
                    LOOPLABEL
                     (SETQ SUBEXP (CDR SUBEXP))
                     (COND ((NULL SUBEXP) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (SUBEXP)
                                 (PROGN
                                  (SETQ NEWEXP (SROOT1 SUBEXP (CAR REPS)))
                                  (SETQ NEW (OR NEW (NEQ SUBEXP NEWEXP)))
                                  NEWEXP))
                               (CAR SUBEXP))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))
                   T))
          (COND ((NOT (OR NEW (NULL (SETQ REPS (CDR REPS))))) (GO A))))
        (COND ((NOT (OR (ATOM EXP) (NOT NEW))) (GO REPEATLABEL))))
      (RETURN
       (COND
        ((OR (AND (NEQ DEPTH 'INF) (LEQ DEPTH 0))
             (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
         EXP)
        (T
         (PROG (SUBEXP FORALL-RESULT FORALL-ENDPTR)
           (SETQ SUBEXP EXP)
           (COND ((NULL SUBEXP) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (SUBEXP)
                               (SSBREADTH SUBEXP REPLIST
                                (COND ((NEQ DEPTH 'INF) (DIFFERENCE DEPTH 1))
                                      (T DEPTH))))
                             (CAR SUBEXP))
                            NIL)))
          LOOPLABEL
           (SETQ SUBEXP (CDR SUBEXP))
           (COND ((NULL SUBEXP) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (SUBEXP)
                       (SSBREADTH SUBEXP REPLIST
                        (COND ((NEQ DEPTH 'INF) (DIFFERENCE DEPTH 1))
                              (T DEPTH))))
                     (CAR SUBEXP))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(PUT 'SD 'PSOPFN 'SDX) 
(PUT 'SDX 'NUMBER-OF-ARGS 1) 
(PUT 'SDX 'DEFINED-ON-LINE '212) 
(PUT 'SDX 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SDX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SDX (ARG)
    (REVAL1
     (SD0 (REVAL1 (CAR ARG) T) (REVAL1 (CADR ARG) T)
      (COND ((CDDR ARG) (REVAL1 (CADDR ARG) T)) (T 1))
      (COND ((AND (CDDR ARG) (CDDDR ARG)) (REVAL1 (CAR (CDDDR ARG)) T))
            (T 'INF)))
     T)) 
(PUT 'SDI 'PSOPFN 'SDI) 
(PUT 'SDI 'NUMBER-OF-ARGS 1) 
(PUT 'SDI 'DEFINED-ON-LINE '222) 
(PUT 'SDI 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SDI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SDI (ARG)
    (REVAL1
     (SD0 (REVAL1 (CAR ARG) T) (REVAL1 (CADR ARG) T) 'INF
      (COND ((CDDR ARG) (REVAL1 (CADDR ARG) T)) (T 'INF)))
     T)) 
(PUT 'SD0 'NUMBER-OF-ARGS 4) 
(PUT 'SD0 'DEFINED-ON-LINE '228) 
(PUT 'SD0 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SD0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SD0 (EXP REPSET RPT DEPTH)
    (COND
     ((OR (LEQ (LENGTH REPSET) 1) (NOT (MEMQ (CAR REPSET) '(REP REPD LIST))))
      EXP)
     ((OR (AND (NEQ DEPTH 'INF) (LESSP DEPTH 0))
          (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
      EXP)
     (T (SDEPTH EXP (FIXREPLIST REPSET) DEPTH)))) 
(PUT 'SDEPTH 'NUMBER-OF-ARGS 3) 
(PUT 'SDEPTH 'DEFINED-ON-LINE '236) 
(PUT 'SDEPTH 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SDEPTH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SDEPTH (EXP REPLIST DEPTH)
    (PROGN
     (SETQ EXP (SROOT EXP REPLIST))
     (COND
      ((OR (AND (NEQ DEPTH 'INF) (LEQ DEPTH 0))
           (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP))
       EXP)
      (T
       (CONS (CAR EXP)
             (PROG (SUBTERM FORALL-RESULT FORALL-ENDPTR)
               (SETQ SUBTERM (CDR EXP))
               (COND ((NULL SUBTERM) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (SUBTERM)
                                   (SDEPTH SUBTERM REPLIST
                                    (COND
                                     ((NEQ DEPTH 'INF) (DIFFERENCE DEPTH 1))
                                     (T DEPTH))))
                                 (CAR SUBTERM))
                                NIL)))
              LOOPLABEL
               (SETQ SUBTERM (CDR SUBTERM))
               (COND ((NULL SUBTERM) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (SUBTERM)
                           (SDEPTH SUBTERM REPLIST
                            (COND ((NEQ DEPTH 'INF) (DIFFERENCE DEPTH 1))
                                  (T DEPTH))))
                         (CAR SUBTERM))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))) 
(PUT 'SROOT 'NUMBER-OF-ARGS 2) 
(PUT 'SROOT 'DEFINED-ON-LINE '244) 
(PUT 'SROOT 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SROOT (EXP REPLIST)
    (PROG (OLDEXP REPS)
      (COND ((OR (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP)) (RETURN EXP)))
      (PROG ()
       REPEATLABEL
        (PROG ()
          (SETQ OLDEXP EXP)
          (SETQ REPS REPLIST)
         A
          (SETQ EXP (SROOT1 EXP (CAR REPS)))
          (COND
           ((NOT (OR (NEQ EXP OLDEXP) (NULL (SETQ REPS (CDR REPS))))) (GO A)))
          (COND ((NEQ EXP OLDEXP) (SETQ EXP (REVAL1 EXP T)))))
        (COND ((NOT (OR (ATOM EXP) (EQ EXP OLDEXP))) (GO REPEATLABEL))))
      (RETURN EXP))) 
(PUT 'SROOT1 'NUMBER-OF-ARGS 2) 
(PUT 'SROOT1 'DEFINED-ON-LINE '263) 
(PUT 'SROOT1 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SROOT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SROOT1 (EXP REP)
    (PROG (FREEVARS SUBSTITUTION MMATCH)
      (COND
       ((OR (AND (NEQ RPT 'INF) (LEQ RPT 0)) (ATOM EXP)
            (NEQ (CAR EXP) (CAADR REP)))
        (RETURN EXP)))
      (SETQ FREEVARS (UNION (FINDNEWVARS (CADR REP)) NIL))
      (SETQ SUBSTITUTION (CADDR REP))
      (PROG (J)
        (SETQ J FREEVARS)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (PUT J 'BINDING (CONS 'UNBOUND (GET J 'BINDING))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (COND
       (*TRPM
        (PROGN
         (PROGN (PRIN2 "Trying rule  ") NIL)
         (RPRINT REP)
         (PROGN (PRIN2 "against      ") NIL)
         (RPRINT EXP))))
      (SETQ MMATCH (AMATCH (CADR REP) EXP T NIL))
      (COND
       (*TRPM
        (PROGN
         (COND
          (MMATCH
           (PROGN
            (PROGN (PRIN2 "producing    ") NIL)
            (RPRINT (SETQ MMATCH (EMBED-NULL-FN MMATCH)))))
          (T (PROGN (PROGN (PRIN2 "failed") NIL) (TERPRI))))
         (TERPRI))))
      (PROG (J)
        (SETQ J FREEVARS)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (PUT J 'BINDING (CDR (GET J 'BINDING)))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN
       (COND
        (MMATCH
         (PROGN
          (COND ((NEQ RPT 'INF) (SETQ RPT (DIFFERENCE RPT 1))))
          (EMBED-NULL-FN MMATCH)))
        (T EXP))))) 
(PUT 'EMBED-NULL-FN 'NUMBER-OF-ARGS 1) 
(PUT 'EMBED-NULL-FN 'DEFINED-ON-LINE '287) 
(PUT 'EMBED-NULL-FN 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'EMBED-NULL-FN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EMBED-NULL-FN (U)
    (COND ((ATOM U) U)
          (T
           (PROG (J FORALL-RESULT FORALL-ENDPTR)
             (SETQ J U)
            STARTOVER
             (COND ((NULL J) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (J)
                        (COND ((ATOM J) (LIST J))
                              ((EQ (CAR J) 'NULL-FN) (EMBED-NULL-FN (CDR J)))
                              (T (LIST (EMBED-NULL-FN J)))))
                      (CAR J)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ J (CDR J))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL J) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (J)
                        (COND ((ATOM J) (LIST J))
                              ((EQ (CAR J) 'NULL-FN) (EMBED-NULL-FN (CDR J)))
                              (T (LIST (EMBED-NULL-FN J)))))
                      (CAR J)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ J (CDR J))
             (GO LOOPLABEL))))) 
(AEVAL (OPERATOR (LIST 'NULL-FN))) 
(PUT 'NULL-FN 'PRIFN 'NULL-FN-PRI) 
(FLUID '(ORIG* POSN*)) 
(PUT 'NULL-FN-PRI 'NUMBER-OF-ARGS 1) 
(PUT 'NULL-FN-PRI 'DEFINED-ON-LINE '302) 
(PUT 'NULL-FN-PRI 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'NULL-FN-PRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULL-FN-PRI (L)
    ((LAMBDA (ORIG*)
       (PROG (SPLIT U)
         (SETQ U L)
         (SETQ L (CDR L))
         (PRIN2* "[")
         (SETQ ORIG* (COND ((LESSP POSN* 18) POSN*) (T (PLUS ORIG* 3))))
         (COND ((NULL L) (GO B)))
         (SETQ SPLIT (ZEROP (TREESIZEP1 L 40)))
        A
         (MAPRINT (NEGNUMBERCHK (CAR L)) 0)
         (SETQ L (CDR L))
         (COND ((NULL L) (GO B)))
         (OPRIN '*COMMA*)
         (COND (SPLIT (TERPRI* T)))
         (GO A)
        B
         (PRIN2* "]")
         (RETURN U)))
     ORIG*)) 
(FLAG '(RSET) 'OPFN) 
(PUT 'RSET 'NUMBER-OF-ARGS 2) 
(PUT 'RSET 'DEFINED-ON-LINE '328) 
(PUT 'RSET 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'RSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RSET (TEMP EXP)
    (COND ((ATOM TEMP) (SETK TEMP EXP))
          (T
           (PROG (OLDSUBFG* VARSTACK*)
             (SETQ OLDSUBFG* SUBFG*)
             (SETQ SUBFG* NIL)
             (SETQ TEMP (REVAL1 TEMP T))
             (PUT (CAR TEMP) 'OPMTCH
                  (RINSERT (FIXREP (CONS 'RSET (LIST TEMP EXP)))
                   (GET (CAR TEMP) 'OPMTCH)))
             (SETQ SUBFG* OLDSUBFG*)
             (RETURN EXP))))) 
(FLAG '(RSETD) 'OPFN) 
(PUT 'RSETD 'NUMBER-OF-ARGS 2) 
(PUT 'RSETD 'DEFINED-ON-LINE '345) 
(PUT 'RSETD 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'RSETD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RSETD (TEMP EXP)
    (COND ((ATOM TEMP) (CONS 'HOLD (SETK TEMP EXP)))
          (T
           (CONS 'HOLD
                 (LIST
                  (PROG (OLDSUBFG* VARSTACK*)
                    (SETQ OLDSUBFG* SUBFG*)
                    (SETQ SUBFG* NIL)
                    (SETQ TEMP (REVAL1 TEMP T))
                    (PUT (CAR TEMP) 'OPMTCH
                         (RINSERT (FIXREP (CONS 'RSETD (LIST TEMP EXP)))
                          (GET (CAR TEMP) 'OPMTCH)))
                    (SETQ SUBFG* OLDSUBFG*)
                    (RETURN EXP))))))) 
(PUT 'RINSERT 'NUMBER-OF-ARGS 2) 
(PUT 'RINSERT 'DEFINED-ON-LINE '360) 
(PUT 'RINSERT 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'RINSERT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RINSERT (RULE RULELIST)
    (COND
     ((OR (NULL RULELIST) (NOT (ATOM (CAAR RULELIST)))) (CONS RULE RULELIST))
     (T
      ((LAMBDA (SS)
         (COND
          ((EQ SS 'EQUAL)
           (COND ((CADR RULE) (CONS RULE (CDR RULELIST))) (T (CDR RULELIST))))
          ((EQ SS 'T) (CONS RULE RULELIST))
          (T (CONS (CAR RULELIST) (RINSERT RULE (CDR RULELIST))))))
       (SUPERSET (CADAR RULELIST) (CADR RULE)))))) 
(PUT 'SUPERSET 'NUMBER-OF-ARGS 2) 
(PUT 'SUPERSET 'DEFINED-ON-LINE '376) 
(PUT 'SUPERSET 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'SUPERSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUPERSET (TEMP1 TEMP2)
    (PROG (MMATCH)
      (SETQ MMATCH (PM_M1 TEMP2 TEMP1))
      (RETURN
       (COND ((NULL MMATCH) NIL) ((EQ MMATCH 'T) 'EQUAL)
             ((NOT (BOUND2GEN (CDR MMATCH))) T)
             ((NULL (SETQ MMATCH (PM_M1 TEMP1 TEMP1))) T) (T 'EQUAL))))) 
(PUT 'BOUND2GEN 'NUMBER-OF-ARGS 1) 
(PUT 'BOUND2GEN 'DEFINED-ON-LINE '387) 
(PUT 'BOUND2GEN 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'BOUND2GEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOUND2GEN (REPLIST)
    (OR (NULL REPLIST)
        (AND
         ((LAMBDA (U) (AND (ATOM U) (OR (GET U 'GEN) (MGENP U))))
          (CADDAR REPLIST))
         (BOUND2GEN (CDR REPLIST))))) 
(FLAG '(AREP) 'OPFN) 
(PUT 'AREP 'NUMBER-OF-ARGS 1) 
(PUT 'AREP 'DEFINED-ON-LINE '393) 
(PUT 'AREP 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'AREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AREP (REPLIST)
    (COND ((ATOM REPLIST) REPLIST)
          ((EQ (CAR REPLIST) 'REP) (LIST 'RSET (CADR REPLIST) (CADDR REPLIST)))
          ((EQ (CAR REPLIST) 'REPD)
           (LIST 'RSETD (CADR REPLIST) (CADDR REPLIST)))
          ((EQ (CAR REPLIST) 'LIST)
           (CONS 'LIST
                 (PROG (REP FORALL-RESULT FORALL-ENDPTR)
                   (SETQ REP (CDR REPLIST))
                   (COND ((NULL REP) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (REP) (AREP REP)) (CAR REP))
                                         NIL)))
                  LOOPLABEL
                   (SETQ REP (CDR REP))
                   (COND ((NULL REP) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (REP) (AREP REP)) (CAR REP)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T NIL))) 
(FLAG '(DREP) 'OPFN) 
(PUT 'DREP 'NUMBER-OF-ARGS 1) 
(PUT 'DREP 'DEFINED-ON-LINE '408) 
(PUT 'DREP 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'DREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DREP (REPLIST)
    (COND ((ATOM REPLIST) REPLIST)
          ((EQ (CAR REPLIST) 'REP) (LIST 'RSET (CADR REPLIST) NIL))
          ((EQ (CAR REPLIST) 'REPD) (LIST 'RSETD (CADR REPLIST) NIL))
          ((EQ (CAR REPLIST) 'LIST)
           (CONS 'LIST
                 (PROG (REP FORALL-RESULT FORALL-ENDPTR)
                   (SETQ REP (CDR REPLIST))
                   (COND ((NULL REP) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (REP) (DREP REP)) (CAR REP))
                                         NIL)))
                  LOOPLABEL
                   (SETQ REP (CDR REP))
                   (COND ((NULL REP) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (REP) (DREP REP)) (CAR REP)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T NIL))) 
(PUT 'OPMTCH 'NUMBER-OF-ARGS 1) 
(PUT 'OPMTCH 'DEFINED-ON-LINE '419) 
(PUT 'OPMTCH 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'OPMTCH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPMTCH (EXP)
    (PROG (OLDEXP REPLIST RPT)
      (SETQ RPT 'INF)
      (SETQ REPLIST (GET (CAR EXP) 'OPMTCH))
      (COND ((OR (NULL REPLIST) (NULL SUBFG*)) (RETURN NIL)))
      (SETQ OLDEXP EXP)
      (PROG ()
       REPEATLABEL
        (SETQ EXP
                (COND ((ATOM (CAAR REPLIST)) (SROOT1 EXP (CAR REPLIST)))
                      (T (OLDMTCH EXP (CAR REPLIST)))))
        (COND
         ((NOT (OR (NEQ EXP OLDEXP) (NULL (SETQ REPLIST (CDR REPLIST)))))
          (GO REPEATLABEL))))
      (RETURN (COND ((EQ EXP OLDEXP) NIL) (T EXP))))) 
(PUT 'OLDMTCH 'NUMBER-OF-ARGS 2) 
(PUT 'OLDMTCH 'DEFINED-ON-LINE '432) 
(PUT 'OLDMTCH 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'OLDMTCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OLDMTCH (EXP RULE)
    (PROG (X Y)
      (SETQ Y (MCHARG (CDR EXP) (CAR RULE) (CAR EXP)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND Y (NULL X))) (RETURN NIL)))
        (PROGN
         (SETQ X
                 (COND
                  ((EVAL (SUBLA (CAR Y) (CDADR RULE)))
                   (SUBLA (CAR Y) (CADDR RULE)))))
         (SETQ Y (CDR Y)))
        (GO WHILELABEL))
      (RETURN (COND (X X) (T EXP))))) 
(PUT '? 'GEN T) 
(PUT '|??;| 'MGEN T) 
(PUT '??$ 'MGEN T) 
(PUT '??^ 'MGEN T) 
(FLAG '(PROP-ALG) 'OPFN) 
(NEWTOK '((_) PROP-ALG)) 
(PUT 'PROP-ALG 'NUMBER-OF-ARGS 1) 
(PUT 'PROP-ALG 'DEFINED-ON-LINE '454) 
(PUT 'PROP-ALG 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'PROP-ALG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROP-ALG (F)
    (PROG (X)
      (SETQ X (PROP F))
      (PROG ()
       WHILELABEL
        (COND ((NOT X) (RETURN NIL)))
        (PROGN
         (PRIN2 (CAR X))
         (PRIN2 "  ")
         (PRINT (CADR X))
         (PRINT " ")
         (SETQ X (CDDR X)))
        (GO WHILELABEL)))) 
(FLAG '(PRECEQ) 'OPFN) 
(PUT 'PRECEQ 'NUMBER-OF-ARGS 2) 
(PUT 'PRECEQ 'DEFINED-ON-LINE '463) 
(PUT 'PRECEQ 'DEFINED-IN-FILE 'PM/PMINTRFC.RED) 
(PUT 'PRECEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRECEQ (U V)
    (PROGN (PUT U 'OP (GET V 'OP)) (PUT U 'INFIX (GET V 'INFIX)) NIL)) 
(NEWTOK '((|:| -) RSET)) 
(NEWTOK '((|:| |:| -) RSETD)) 
(NEWTOK '((- >) REP)) 
(NEWTOK '((- - >) REPD)) 
(NEWTOK '((_ =) SUCH-THAT)) 
(FLAG '(SUCH-THAT) 'SPACED) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SETQ *MSG NIL) 
(MKOP 'RSET) 
(MKOP 'RSETD) 
(INFIX (LIST 'RSET 'RSETD)) 
(SETQ *MSG T) 
(AEVAL (LIST 'NOSIMP 'RSET '(T NIL))) 
(AEVAL (LIST 'NOSIMP 'RSETD '(T T))) 
(PRECEDENCE (LIST 'RSETD 'RSET)) 
(MKOP 'REP) 
(INFIX (LIST 'REP)) 
(PRECEDENCE (LIST 'REP 'RSETD)) 
(MKOP 'REPD) 
(INFIX (LIST 'REPD)) 
(AEVAL (LIST 'NOSIMP 'REPD '(NIL T))) 
(PRECEDENCE (LIST 'REPD 'REP)) 
(MKOP 'SUCH-THAT) 
(INFIX (LIST 'SUCH-THAT)) 
(AEVAL (LIST 'NOSIMP 'SUCH-THAT '(NIL T))) 
(PRECEDENCE (LIST 'SUCH-THAT 'REPD)) 
(OPERATOR (LIST 'HOLD)) 
(AEVAL (LIST 'NOSIMP 'HOLD 'T)) 
(FLAG '(RSET RSETD REP REPD SUCH-THAT) 'RIGHT) 
(AEVAL (LIST 'PRECEQ 'RSETD 'RSET)) 
(AEVAL (LIST 'PRECEQ 'REPD 'REP)) 
(FLAG '(PLUS TIMES EXPT) 'ASSOC) 
(ENDMODULE) 