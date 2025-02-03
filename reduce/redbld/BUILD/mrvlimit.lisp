(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MRVLIMIT)) 
(CREATE-PACKAGE '(MRVLIMIT) NIL) 
(FLUID '(*TRLIMIT)) 
(FLUID '(*MRV-RECURSION-LEVEL*)) 
(GLOBAL '(ERFG*)) 
(SHARE (LIST '*MRV-RECURSION-LEVEL*)) 
(SETQ *MRV-RECURSION-LEVEL* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 0)) 
(SWITCH (LIST (LIST 'EQUAL 'TRLIMIT 'OFF))) 
(LOAD_PACKAGE (LIST 'LIMITS)) 
(DE MRV_CONSTANTP (F X) (OR (CONSTANT_EXPRP F) (FREEOF F X))) 
(PUT 'MRV_CONSTANTP 'NUMBER-OF-ARGS 2) 
(PUT 'MRV_CONSTANTP 'DEFINED-ON-LINE '58) 
(PUT 'MRV_CONSTANTP 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_CONSTANTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'MRV_CONSTANTP 'INLINE
      '(LAMBDA (F X) (OR (CONSTANT_EXPRP F) (FREEOF F X)))) 
(PUT 'MRV_MAXI 'NUMBER-OF-ARGS 3) 
(PUT 'MRV_MAXI 'DEFINED-ON-LINE '63) 
(PUT 'MRV_MAXI 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_MAXI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRV_MAXI (F G VAR)
    (COND ((OR (CONSTANT_EXPRP F) (FREEOF F VAR)) G)
          ((OR (CONSTANT_EXPRP G) (FREEOF G VAR)) F) ((EQUAL F G) F)
          ((NOT (NULL (INTERSECTION F G))) (UNION F G))
          (T (MRV_COMPARE F G VAR)))) 
(PUT 'MRV_MAXI1 'NUMBER-OF-ARGS 2) 
(FLAG '(MRV_MAXI1) 'OPFN) 
(PUT 'MRV_MAXI1 'DEFINED-ON-LINE '78) 
(PUT 'MRV_MAXI1 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_MAXI1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV_MAXI1 (F G) (CONS 'LIST (MRV_MAXI (CDR F) (CDR G) 'X))) 
(PUT 'MRV-COMPUTE-LOGLIM 'NUMBER-OF-ARGS 3) 
(PUT 'MRV-COMPUTE-LOGLIM 'DEFINED-ON-LINE '80) 
(PUT 'MRV-COMPUTE-LOGLIM 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-COMPUTE-LOGLIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRV-COMPUTE-LOGLIM (F G VAR)
    (PROG (*EXPANDLOGS)
      (SETQ *EXPANDLOGS T)
      (COND ((AND (EQCAR F 'EXPT) (EQUAL (CADR F) 'E)) (SETQ F (CADDR F)))
            (T (SETQ F (LIST 'LOG F))))
      (COND ((AND (EQCAR G 'EXPT) (EQUAL (CADR G) 'E)) (SETQ G (CADDR G)))
            (T (SETQ G (LIST 'LOG G))))
      (RETURN (MRV-LIMIT1 (REVAL1 (LIST 'QUOTIENT F G) T) VAR)))) 
(PUT 'MRV_COMPARE 'NUMBER-OF-ARGS 3) 
(PUT 'MRV_COMPARE 'DEFINED-ON-LINE '89) 
(PUT 'MRV_COMPARE 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_COMPARE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRV_COMPARE (F G VAR)
    ((LAMBDA (S)
       (COND ((EQUAL S 0) G) ((MEMBER S '(INFINITY (MINUS INFINITY))) F)
             (T (UNION F G))))
     (MRV-COMPUTE-LOGLIM (CAR F) (CAR G) VAR))) 
(FLAG '(MRV_COMPARE) 'OPFN) 
(PUT 'MRV-LOGSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'MRV-LOGSIMP 'DEFINED-ON-LINE '97) 
(PUT 'MRV-LOGSIMP 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-LOGSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRV-LOGSIMP (EXPTTERM)
    (COND ((EQ (CADR EXPTTERM) 'E) (CADDR EXPTTERM))
          (T (LIST 'TIMES (CADDR EXPTTERM) (LIST 'LOG (CADR EXPTTERM)))))) 
(PUT 'MRV-SET 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-SET 'DEFINED-ON-LINE '103) 
(PUT 'MRV-SET 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-SET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-SET (U VAR)
    (PROG (LI2 OP ARGL)
      (COND ((OR (CONSTANT_EXPRP U) (FREEOF U VAR)) (RETURN NIL))
            ((EQUAL U '(LIST)) (RETURN NIL)) ((EQUAL U VAR) (RETURN (LIST U))))
      (COND ((EQCAR U '*SQ) (SETQ U (PREPSQ (CADR U)))))
      (COND ((PAIRP U) (PROGN (SETQ OP (CAR U)) (SETQ ARGL (CDR U)))))
      (COND
       ((MEMQ OP '(MINUS LOG SQRT))
        (RETURN
         (COND ((EQUAL (FIRST ARGL) VAR) (LIST VAR))
               ((NOT
                 ((LAMBDA (G125) (OR (CONSTANT_EXPRP G125) (FREEOF G125 VAR)))
                  (FIRST ARGL)))
                (MRV (FIRST ARGL) VAR))))))
      (COND
       ((MEMQ OP '(PLUS TIMES))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND ARGL
                   ((LAMBDA (G127)
                      (OR (CONSTANT_EXPRP G127) (FREEOF G127 VAR)))
                    (FIRST ARGL))))
             (RETURN NIL)))
           (SETQ ARGL (REST ARGL))
           (GO WHILELABEL))
         (COND ((NULL ARGL) (RETURN NIL)))
         (SETQ LI2 (MRV (FIRST ARGL) VAR))
         (SETQ ARGL (REST ARGL))
         (PROG ()
          WHILELABEL
           (COND ((NOT ARGL) (RETURN NIL)))
           (PROGN
            (COND
             ((NOT
               ((LAMBDA (G129) (OR (CONSTANT_EXPRP G129) (FREEOF G129 VAR)))
                (FIRST ARGL)))
              (SETQ LI2 (MRV_MAXI (MRV (FIRST ARGL) VAR) LI2 VAR))))
            (SETQ ARGL (REST ARGL)))
           (GO WHILELABEL))
         (RETURN LI2)
         NIL))
       ((EQ OP 'EXPT)
        (RETURN
         (COND
          (((LAMBDA (G131) (OR (CONSTANT_EXPRP G131) (FREEOF G131 VAR)))
            (SECOND ARGL))
           (MRV (FIRST ARGL) VAR))
          ((MEMBER (MRV-LIMIT1 (MRV-LOGSIMP U) VAR)
                   '(INFINITY (MINUS INFINITY)))
           (MRV_MAXI (LIST U) (MRV (SECOND ARGL) VAR) VAR))
          (T (MRV_MAXI (MRV (FIRST ARGL) VAR) (MRV (SECOND ARGL) VAR) VAR)))))
       ((NULL (REST ARGL)) (RETURN (MRV (FIRST ARGL) VAR)))
       ((NULL (CDDR ARGL))
        (RETURN (MRV_MAXI (MRV (FIRST ARGL) VAR) (MRV (SECOND ARGL) VAR) VAR)))
       (T (RERROR 'MRVLIMIT 1 "mrv not implemented"))))) 
(PUT 'MRV 'NUMBER-OF-ARGS 2) 
(PUT 'MRV 'DEFINED-ON-LINE '147) 
(PUT 'MRV 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV (U VAR)
    (PROG (V)
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Entering mrv(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): ")
         (MAPRIN U)
         (TERPRI* T)
         NIL)))
      (SETQ V (MRV-SET U VAR))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Mrv set of ")
         (MAPRIN U)
         (TERPRI* T)
         (PRIN2* " is ")
         (MAPRIN (CONS 'LIST V))
         (TERPRI* T)
         NIL)))
      (RETURN V))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'MRV1 'NUMBER-OF-ARGS 2) 
(PUT 'MRV1 'DEFINED-ON-LINE '165) 
(PUT 'MRV1 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV1 (LI VAR) (CONS 'LIST (MRV LI VAR))) 
(FLAG '(MRV1) 'OPFN) 
(FLUID '(MRV-CURVAR*)) 
(PUT 'MRV-ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-ORDP 'DEFINED-ON-LINE '170) 
(PUT 'MRV-ORDP 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-ORDP (A B) (GREATERP (LENGTH A) (LENGTH B))) 
(DE MRV-SMALLEST (U)
    (COND ((ATOM U) U)
          (T
           (PROG (V LNG1 LNG2)
             (SETQ V (CAR U))
             (SETQ LNG1 (MRV-LENGTH V))
             (PROG (EL)
               (SETQ EL (CDR U))
              LAB
               (COND ((NULL EL) (RETURN NIL)))
               ((LAMBDA (EL)
                  (COND
                   ((GREATERP LNG1 (SETQ LNG2 (MRV-LENGTH EL)))
                    (PROGN (SETQ V EL) (SETQ LNG1 LNG2)))))
                (CAR EL))
               (SETQ EL (CDR EL))
               (GO LAB))
             (RETURN V))))) 
(PUT 'MRV-SMALLEST 'NUMBER-OF-ARGS 1) 
(PUT 'MRV-SMALLEST 'DEFINED-ON-LINE '174) 
(PUT 'MRV-SMALLEST 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-SMALLEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MRV-SMALLEST 'INLINE
      '(LAMBDA (U)
         (COND ((ATOM U) U)
               (T
                (PROG (V LNG1 LNG2)
                  (SETQ V (CAR U))
                  (SETQ LNG1 (MRV-LENGTH V))
                  (PROG (EL)
                    (SETQ EL (CDR U))
                   LAB
                    (COND ((NULL EL) (RETURN NIL)))
                    ((LAMBDA (EL)
                       (COND
                        ((GREATERP LNG1 (SETQ LNG2 (MRV-LENGTH EL)))
                         (PROGN (SETQ V EL) (SETQ LNG1 LNG2)))))
                     (CAR EL))
                    (SETQ EL (CDR EL))
                    (GO LAB))
                  (RETURN V)))))) 
(PUT 'MRV-REWRITE 'NUMBER-OF-ARGS 4) 
(PUT 'MRV-REWRITE 'DEFINED-ON-LINE '183) 
(PUT 'MRV-REWRITE 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-REWRITE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRV-REWRITE (U VAR OMEGA W)
    (PROG (MRV-CURVAR* RLIST G LOGG LOGW S)
      (SETQ MRV-CURVAR* VAR)
      (SETQ OMEGA (SORT OMEGA (FUNCTION MRV-ORDP)))
      (SETQ G
              (COND ((ATOM OMEGA) OMEGA)
                    (T
                     (PROG (V LNG1 LNG2)
                       (SETQ V (CAR OMEGA))
                       (SETQ LNG1 (MRV-LENGTH V))
                       (PROG (EL)
                         (SETQ EL (CDR OMEGA))
                        LAB
                         (COND ((NULL EL) (RETURN NIL)))
                         ((LAMBDA (EL)
                            (COND
                             ((GREATERP LNG1 (SETQ LNG2 (MRV-LENGTH EL)))
                              (PROGN (SETQ V EL) (SETQ LNG1 LNG2)))))
                          (CAR EL))
                         (SETQ EL (CDR EL))
                         (GO LAB))
                       (RETURN V)))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-rewrite(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): Rewriting ")
         (MAPRIN U)
         (TERPRI* T)
         (PRIN2* " omega is ")
         (MAPRIN (CONS 'LIST OMEGA))
         (TERPRI* T)
         (PRIN2* "Smallest subexpression is ")
         (MAPRIN G)
         (TERPRI* T)
         NIL)))
      (SETQ LOGW (SETQ LOGG (MRV-LOGSIMP G)))
      (SETQ S (MRV-SIGN LOGG VAR))
      (COND
       ((EQUAL S 1)
        (PROGN
         (SETQ W (LIST 'EXPT W (MINUS 1)))
         (SETQ LOGW (LIST 'MINUS LOGW)))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-rewrite(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): Sign of ")
         (MAPRIN G)
         (PRIN2* " is ")
         (PRIN2* S)
         (TERPRI* T)
         NIL)))
      (SETQ RLIST
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F OMEGA)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (PROG (LOGF C WEXPT EXPT2 REPL)
                             (SETQ LOGF (MRV-LOGSIMP F))
                             (SETQ C
                                     (MRV-LEADING-TERM
                                      (REVAL1 (LIST 'QUOTIENT LOGF LOGG) T) VAR
                                      NIL))
                             (COND
                              ((NEQ (CADR C) 0)
                               (RERROR 'MRVLIMIT 1
                                       (LIST
                                        "mrv-rewrite: elements must be in the same compatibility class"
                                        LOGF LOGG))))
                             (SETQ C (CAR C))
                             (SETQ WEXPT
                                     (COND ((EQUAL C 1) W)
                                           (T (LIST 'EXPT W C))))
                             (SETQ EXPT2
                                     (REVAL1
                                      (LIST 'DIFFERENCE LOGF
                                            (LIST 'TIMES C LOGG))
                                      T))
                             (SETQ REPL
                                     (CONS F
                                           (COND ((EQUAL EXPT2 0) WEXPT)
                                                 (T
                                                  (LIST 'TIMES WEXPT
                                                        (LIST 'EXPT 'E
                                                              EXPT2))))))
                             (SETQ WEXPT
                                     (COND ((EQUAL C (MINUS 1)) W)
                                           (T
                                            (REVAL1
                                             (LIST 'EXPT W (LIST 'MINUS C))
                                             T))))
                             (SETQ EXPT2
                                     (REVAL1
                                      (LIST 'DIFFERENCE (LIST 'TIMES C LOGG)
                                            LOGF)
                                      T))
                             (RETURN
                              (LIST REPL
                                    (CONS (REVAL1 (LIST 'EXPT F (MINUS 1)) T)
                                          (COND ((EQUAL EXPT2 0) WEXPT)
                                                (T
                                                 (LIST 'TIMES WEXPT
                                                       (LIST 'EXPT 'E
                                                             EXPT2)))))))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (PROG (LOGF C WEXPT EXPT2 REPL)
                             (SETQ LOGF (MRV-LOGSIMP F))
                             (SETQ C
                                     (MRV-LEADING-TERM
                                      (REVAL1 (LIST 'QUOTIENT LOGF LOGG) T) VAR
                                      NIL))
                             (COND
                              ((NEQ (CADR C) 0)
                               (RERROR 'MRVLIMIT 1
                                       (LIST
                                        "mrv-rewrite: elements must be in the same compatibility class"
                                        LOGF LOGG))))
                             (SETQ C (CAR C))
                             (SETQ WEXPT
                                     (COND ((EQUAL C 1) W)
                                           (T (LIST 'EXPT W C))))
                             (SETQ EXPT2
                                     (REVAL1
                                      (LIST 'DIFFERENCE LOGF
                                            (LIST 'TIMES C LOGG))
                                      T))
                             (SETQ REPL
                                     (CONS F
                                           (COND ((EQUAL EXPT2 0) WEXPT)
                                                 (T
                                                  (LIST 'TIMES WEXPT
                                                        (LIST 'EXPT 'E
                                                              EXPT2))))))
                             (SETQ WEXPT
                                     (COND ((EQUAL C (MINUS 1)) W)
                                           (T
                                            (REVAL1
                                             (LIST 'EXPT W (LIST 'MINUS C))
                                             T))))
                             (SETQ EXPT2
                                     (REVAL1
                                      (LIST 'DIFFERENCE (LIST 'TIMES C LOGG)
                                            LOGF)
                                      T))
                             (RETURN
                              (LIST REPL
                                    (CONS (REVAL1 (LIST 'EXPT F (MINUS 1)) T)
                                          (COND ((EQUAL EXPT2 0) WEXPT)
                                                (T
                                                 (LIST 'TIMES WEXPT
                                                       (LIST 'EXPT 'E
                                                             EXPT2)))))))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-rewrite(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): rewriting: ")
         (MAPRIN U)
         (TERPRI* T)
         (PRIN2* "Replacement list is")
         (TERPRI* T)
         (PROG (PP)
           (SETQ PP RLIST)
          LAB
           (COND ((NULL PP) (RETURN NIL)))
           ((LAMBDA (PP)
              (PROGN (MAPRIN (LIST 'REPLACEBY (CAR PP) (CDR PP))) (TERPRI* T)))
            (CAR PP))
           (SETQ PP (CDR PP))
           (GO LAB))
         NIL)))
      (PROG (PP)
        (SETQ PP RLIST)
       LAB
        (COND ((NULL PP) (RETURN NIL)))
        ((LAMBDA (PP) (SETQ U (SUBST (CDR PP) (CAR PP) U))) (CAR PP))
        (SETQ PP (CDR PP))
        (GO LAB))
      (COND
       (*TRLIMIT (PROGN (PRIN2* "After rewriting: ") (MAPRIN U) (TERPRI* T))))
      (SETQ U (SUBST LOGW (LIST 'LOG W) U))
      (RETURN (CONS U LOGW)))) 
(PUT 'MRV-EXPT-SMEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-EXPT-SMEMBER 'DEFINED-ON-LINE '245) 
(PUT 'MRV-EXPT-SMEMBER 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-EXPT-SMEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-EXPT-SMEMBER (U V)
    (OR (SMEMBER U V)
        (AND (EQCAR U 'EXPT)
             ((LAMBDA (U1) (SMEMBER U1 V))
              (LIST 'EXPT (CADR U)
                    (COND ((EQCAR (CADDR U) 'MINUS) (CADR (CADDR U)))
                          (T (LIST 'MINUS (CADDR U))))))))) 
(PUT 'MRV-EXPAND-SERIES 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-EXPAND-SERIES 'DEFINED-ON-LINE '252) 
(PUT 'MRV-EXPAND-SERIES 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-EXPAND-SERIES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-EXPAND-SERIES (F W)
    (PROG (S COEFFS)
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-leading!-term(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): series expansion of ")
         (MAPRIN F)
         (TERPRI* T)
         NIL)))
      (SETQ S
              (ERRORSET* (LIST 'SIMPTAYLOR (MKQUOTE (LIST F W 0 2)))
                         *BACKTRACE))
      (COND
       ((ATOM S)
        (PROGN
         (COND
          (*TRLIMIT
           (PROGN
            (TERPRI* T)
            (PRIN2* "Error ")
            (PRIN2* S)
            (PRIN2* " during Taylor expansion of")
            (MATHPRINT F)
            NIL)))
         (RERROR 'MRVLIMIT 3 (LIST "Error in Taylor expansion"))
         (RETURN LIST)))
       ((NOT (KERNP (SETQ S (CAR S))))
        (PROGN
         (COND
          (*TRLIMIT
           (PROGN
            (TERPRI* T)
            (PRIN2* "Taylor expansion of")
            (MATHPRINT F)
            (PRIN2* "yields unexpected result")
            (PRINTSQ S)
            (TERPRI* T))))
         (RERROR 'MRVLIMIT 3 (LIST "mrv_limit: Error in Taylor expansion"))))
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-leading!-term(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): series expansion is ")
         (PRINTSQ S)
         (TERPRI* T)
         NIL)))
      (SETQ S (CAAAR (CAR S)))
      (SETQ S (CADR S))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND S (NULL (CAR (CDR (CAR S)))))) (RETURN NIL)))
        (SETQ S (CDR S))
        (GO WHILELABEL))
      (COND ((NULL S) (RETURN (LIST 0 0)))
            (T (RETURN (LIST (PREPSQ (CDAR S)) (CAAAAR S)))))
      (SETQ S (REVAL1 (LIST 'TAYLOR F W 0 4) NIL))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "In mrv!-leading!-term(")
         (PRIN2* *MRV-RECURSION-LEVEL*)
         (PRIN2* "): series expansion is ")
         (MAPRIN S)
         (TERPRI* T)
         NIL)))
      (SETQ S (TAYLORTOSTANDARD S))
      (SETQ COEFFS (COEFFEVAL (LIST S W)))
      (RETURN (LIST (NTH (CDR COEFFS) (PLUS LOWPOW* 1)) LOWPOW*)))) 
(PUT 'MRV-LEADING-TERM 'NUMBER-OF-ARGS 3) 
(PUT 'MRV-LEADING-TERM 'DEFINED-ON-LINE '307) 
(PUT 'MRV-LEADING-TERM 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-LEADING-TERM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MRV-LEADING-TERM (U VAR OMEGA)
    ((LAMBDA (RESULT *MRV-RECURSION-LEVEL*)
       (PROGN
        (COND
         (*TRLIMIT
          (PROGN
           (TERPRI* T)
           (PRIN2* "Entering mrv!-leading!-term(")
           (PRIN2* *MRV-RECURSION-LEVEL*)
           (PRIN2* ") for ")
           (MAPRIN U)
           (TERPRI* T)
           (PRIN2* "w.r.t. var ")
           (MAPRIN VAR)
           (TERPRI* T)
           (PRIN2* "omega = ")
           (MAPRIN (CONS 'LIST OMEGA))
           (TERPRI* T)
           NIL)))
        (SETQ RESULT
                (COND ((OR (CONSTANT_EXPRP U) (FREEOF U VAR)) (LIST U 0))
                      (T
                       (PROG (E0 S W F LOGW)
                         (SETQ E0 U)
                         (SETQ OMEGA
                                 (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ TERM OMEGA)
                                  STARTOVER
                                   (COND ((NULL TERM) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           ((LAMBDA (TERM)
                                              (COND
                                               ((MRV-EXPT-SMEMBER TERM E0)
                                                (LIST TERM))))
                                            (CAR TERM)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-RESULT))
                                   (SETQ TERM (CDR TERM))
                                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                  LOOPLABEL
                                   (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           ((LAMBDA (TERM)
                                              (COND
                                               ((MRV-EXPT-SMEMBER TERM E0)
                                                (LIST TERM))))
                                            (CAR TERM)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-ENDPTR))
                                   (SETQ TERM (CDR TERM))
                                   (GO LOOPLABEL)))
                         (COND ((NULL OMEGA) (SETQ OMEGA (MRV E0 VAR))))
                         (COND
                          (*TRLIMIT
                           (PROGN
                            (PRIN2* "Omega set of ")
                            (MAPRIN E0)
                            (PRIN2* " is ")
                            (MAPRIN (CONS 'LIST OMEGA))
                            (TERPRI* T)
                            NIL)))
                         (COND
                          ((MEMBER VAR OMEGA)
                           (RETURN
                            (MRV-MOVEDOWN
                             (MRV-LEADING-TERM (MRV-MOVEUP E0 VAR) VAR
                              (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TERM OMEGA)
                                (COND ((NULL TERM) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (TERM)
                                                    (MRV-MOVEUP TERM VAR))
                                                  (CAR TERM))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ TERM (CDR TERM))
                                (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (TERM) (MRV-MOVEUP TERM VAR))
                                          (CAR TERM))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                             VAR))))
                         (SETQ W (GENSYM))
                         (PROG (G134)
                           (SETQ G134 (MRV-REWRITE E0 VAR OMEGA W))
                           (SETQ F (CAR G134))
                           (SETQ LOGW (CDR G134))
                           (RETURN G134))
                         (RETURN
                          (EVALLETSUB
                           (LIST (LIST (LIST 'REPLACEBY (LIST 'LOG W) LOGW))
                                 (LIST 'MRV-EXPAND-SERIES (MKQUOTE F)
                                       (MKQUOTE W)))
                           NIL))))))
        (COND
         (*TRLIMIT
          (PROGN
           (PRIN2* "Exiting mrv!-leading!-term(")
           (PRIN2* *MRV-RECURSION-LEVEL*)
           (PRIN2* ") with {coeff,exponent}=")
           (MAPRIN (CONS 'LIST RESULT))
           (TERPRI* T)
           NIL)))
        RESULT))
     0 (PLUS *MRV-RECURSION-LEVEL* 1))) 
(PUT 'MRV-SIMP-LOGEXP 'NUMBER-OF-ARGS 1) 
(PUT 'MRV-SIMP-LOGEXP 'DEFINED-ON-LINE '353) 
(PUT 'MRV-SIMP-LOGEXP 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-SIMP-LOGEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRV-SIMP-LOGEXP (U)
    (COND ((ATOM U) U)
          ((AND (EQ (CAR U) 'LOG) (EQCAR (CADR U) 'EXPT) (EQ (CADADR U) 'E))
           (CADDR (CADR U)))
          (T
           (CONS (CAR U)
                 (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ARG (CDR U))
                   (COND ((NULL ARG) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ARG) (MRV-SIMP-LOGEXP ARG))
                                     (CAR ARG))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ARG (CDR ARG))
                   (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ARG) (MRV-SIMP-LOGEXP ARG)) (CAR ARG))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'MRV-MOVEUP 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-MOVEUP 'DEFINED-ON-LINE '360) 
(PUT 'MRV-MOVEUP 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-MOVEUP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-MOVEUP (U VAR) (MRV-SIMP-LOGEXP (SUBST (LIST 'EXPT 'E VAR) VAR U))) 
(PUT 'MRV-MOVEDOWN 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-MOVEDOWN 'DEFINED-ON-LINE '364) 
(PUT 'MRV-MOVEDOWN 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-MOVEDOWN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-MOVEDOWN (U VAR)
    (PROG (V FORALL-RESULT FORALL-ENDPTR)
      (SETQ V U)
      (COND ((NULL V) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (V)
                          (SQCHK
                           (REVAL1
                            (LIST 'SUB (LIST 'EQUAL VAR (LIST 'LOG VAR)) V)
                            T)))
                        (CAR V))
                       NIL)))
     LOOPLABEL
      (SETQ V (CDR V))
      (COND ((NULL V) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (V)
                  (SQCHK
                   (REVAL1 (LIST 'SUB (LIST 'EQUAL VAR (LIST 'LOG VAR)) V) T)))
                (CAR V))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MRV-SIGN 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-SIGN 'DEFINED-ON-LINE '368) 
(PUT 'MRV-SIGN 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-SIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-SIGN (U VAR)
    (COND ((OR (CONSTANT_EXPRP U) (FREEOF U VAR)) (OR (SIGN-OF U) 0))
          ((EQUAL U VAR) 1) ((EQCAR U 'MINUS) (MINUS (MRV-SIGN (CADR U) VAR)))
          ((EQCAR U 'TIMES)
           (PROG (P)
             (SETQ P 0)
             (SETQ P 1)
             (PROG (FCT)
               (SETQ FCT (CDR U))
              LAB
               (COND ((NULL FCT) (RETURN NIL)))
               ((LAMBDA (FCT) (SETQ P (TIMES P (MRV-SIGN FCT VAR)))) (CAR FCT))
               (SETQ FCT (CDR FCT))
               (GO LAB))
             (RETURN P)))
          ((EQCAR U 'EXPT)
           (COND
            ((OR (EQUAL (CADR U) 'E) (EQUAL (CADR U) VAR)
                 (EQUAL (SIGN-OF (CADR U)) 1))
             1)
            (T (CAR (MRV-LEADING-TERM U VAR NIL)))))
          ((EQCAR U 'LOG)
           (COND
            ((EQUAL (SIGN-OF (CADR U)) (MINUS 1))
             (RERROR 'MRVLIMIT 2 "Complex limit"))
            (T (MRV-SIGN (LIST 'PLUS (MINUS 1) (CADR U)) VAR))))
          ((EQCAR U 'QUOTIENT)
           ((LAMBDA (DENOM-SIGN)
              (COND
               ((EQUAL DENOM-SIGN 0)
                (RERROR 'MRVLIMIT 2 (LIST "Cannot compute the sign of" U)))
               (T (TIMES (MRV-SIGN (CADR U) VAR) DENOM-SIGN))))
            (MRV-SIGN (CADDR U) VAR)))
          ((OR (EQCAR U 'PLUS) (EQCAR U 'DIFFERENCE))
           (MRV-SIGN (CAR (MRV-LEADING-TERM U VAR NIL)) VAR))
          (T (RERROR 'MRVLIMIT 2 (LIST "Cannot compute the sign of" U))))) 
(PUT 'MRV-LENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'MRV-LENGTH 'DEFINED-ON-LINE '392) 
(PUT 'MRV-LENGTH 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRV-LENGTH (U)
    (COND ((EQUAL U 'LIST) NIL) ((ATOM U) 1)
          ((ATOM (CAR U)) (PLUS 1 (MRV-LENGTH (CDR U))))
          (T (PLUS (MRV-LENGTH (CAR U)) (MRV-LENGTH (CDR U)))))) 
(PUT 'MRV_LENGTH 'NUMBER-OF-ARGS 1) 
(FLAG '(MRV_LENGTH) 'OPFN) 
(PUT 'MRV_LENGTH 'DEFINED-ON-LINE '398) 
(PUT 'MRV_LENGTH 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_LENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRV_LENGTH (U) (MRV-LENGTH U)) 
(PUT 'MRV-LIMIT1A 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-LIMIT1A 'DEFINED-ON-LINE '405) 
(PUT 'MRV-LIMIT1A 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-LIMIT1A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-LIMIT1A (U VAR)
    (COND ((OR (CONSTANT_EXPRP U) (FREEOF U VAR)) U)
          (T
           (PROG (C0 E0 S S1)
             (PROG (G135 G136)
               (SETQ G135 (MRV-LEADING-TERM U VAR NIL))
               (SETQ G136 G135)
               (SETQ C0 (CAR G135))
               (SETQ G135 (CDR G135))
               (SETQ E0 (CAR G135))
               (SETQ G135 (CDR G135))
               (RETURN G136))
             (SETQ S (MRV-SIGN E0 VAR))
             (COND ((EQUAL S 0) (RETURN (MRV-LIMIT1 C0 VAR)))
                   ((EQUAL S 1) (RETURN 0))
                   ((EQUAL S (MINUS 1))
                    (PROGN
                     (SETQ S1 (MRV-SIGN C0 VAR))
                     (COND ((EQUAL S1 1) (RETURN 'INFINITY))
                           ((EQUAL S1 (MINUS 1)) (RETURN '(MINUS INFINITY)))
                           (T
                            (PROGN
                             (PRIN2 "mrv!_limit1a: sign is ")
                             (PRIN2 S1)
                             NIL)))
                     NIL))))))) 
(PUT 'MRV-LIMIT1 'NUMBER-OF-ARGS 2) 
(PUT 'MRV-LIMIT1 'DEFINED-ON-LINE '421) 
(PUT 'MRV-LIMIT1 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV-LIMIT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MRV-LIMIT1 (U VAR)
    ((LAMBDA (R)
       (PROGN
        (COND
         (*TRLIMIT
          (PROGN
           (PRIN2* "Result of mrv!-limit1 of ")
           (MAPRIN U)
           (PRIN2* " w.r.t. ")
           (MAPRIN VAR)
           (PRIN2* " is ")
           (TERPRI* T)
           (COND ((NULL R) (PRIN2* "nil")) (T (MAPRIN R)))
           (TERPRI* T))))
        R))
     (MRV-LIMIT1A U VAR))) 
(PUT 'MRV_LIMIT 'NUMBER-OF-ARGS 1) 
(PUT 'MRV_LIMIT 'DEFINED-ON-LINE '432) 
(PUT 'MRV_LIMIT 'DEFINED-IN-FILE 'LIMIT/MRVLIMIT.RED) 
(PUT 'MRV_LIMIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MRV_LIMIT (ARGL)
    (COND
     ((NEQ (LENGTH ARGL) 3)
      (REDERR (LIST "mrv_limit called with wrong number of arguments" ARGL)))
     (T
      (PROG (U X X0 R NEWVAR *PROTFG *EXPANDLOGS *EXPANDEXPT *COMBINEEXPT
             *PRECISE)
        (SETQ NEWVAR (GENSYM))
        (SETQ *PROTFG (NOT *BACKTRACE))
        (SETQ *EXPANDLOGS T)
        (SETQ *COMBINEEXPT T)
        (SETQ U (REVAL1 (CAR ARGL) T))
        (SETQ X (CADR ARGL))
        (SETQ X0 (CADDR ARGL))
        (COND ((EQUAL X0 'INFINITY) (SETQ NEWVAR X))
              ((EQUAL X0 '(MINUS INFINITY))
               (SETQ U
                       (SUBEVAL
                        (LIST (LIST 'EQUAL X (LIST 'MINUS NEWVAR)) U))))
              (T (REDERR (LIST "mrv_limit: Invalid 3rd argument" X0))))
        (SETQ R
                (ERRORSET* (LIST 'MRV-LIMIT1 (MKQUOTE U) (MKQUOTE NEWVAR))
                           *BACKTRACE))
        (RETURN
         (COND ((ERRORP R) (PROGN (SETQ ERFG* NIL) (LIST 'MRV_LIMIT U X X0)))
               (T (CAR R)))))))) 
(PUT 'MRV_LIMIT 'PSOPFN 'MRV_LIMIT) 
(REMFLAG '(MRV_LIMIT) 'OPFN) 
(ENDMODULE) 