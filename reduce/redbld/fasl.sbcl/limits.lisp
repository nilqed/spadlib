(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LIMITS)) 
(LOAD-PACKAGE 'TPS) 
(SETQ |PS:ORDER-LIMIT| 100) 
(FLUID '(*USETAYLOR *TRLIMIT TRLIMITLEVEL*)) 
(SWITCH (LIST (LIST 'EQUAL 'USETAYLOR 'OFF))) 
(SWITCH (LIST 'TRLIMIT)) 
(SETQ TRLIMITLEVEL* 0) 
(FLUID
 '(*PRECISE |LHOP#| |LPLUS#| *PROTFG *MSG *ROUNDED *COMPLEX *FACTOR |#NNN|
   |LIM00#| *CRLIMTEST *LIM00REC)) 
(SETQ *LIM00REC T) 
(GLOBAL '(ERFG* |EXPTCONV#|)) 
(GLOBAL '(|ABSLIMS#|)) 
(SETQ |ABSLIMS#| (LIST 0 1 (MINUS 1) 'INFINITY '(MINUS INFINITY))) 
(FLUID '(LSIMPDPTH)) 
(GLOBAL '(|LD0#|)) 
(SETQ |LD0#| 3) 
(FLAG '(LIMIT LIMIT+ LIMIT-) 'FULL) 
(PUT 'LIMIT 'SIMPFN 'SIMPLIMIT) 
(PUT 'LIMIT+ 'SIMPFN 'SIMPLIMIT) 
(PUT 'LIMIT- 'SIMPFN 'SIMPLIMIT) 
(PUT 'LIMIT2 'NUMBER-OF-ARGS 4) 
(PUT 'LIMIT2 'DEFINED-ON-LINE '96) 
(PUT 'LIMIT2 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT2 (TOP BOT XXX A) ((LAMBDA (|LHOP#|) (LHOPITAL TOP BOT XXX A)) 0)) 
(PUT 'LIMIT+ 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT+ 'DEFINED-ON-LINE '99) 
(PUT 'LIMIT+ 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT+ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT+ (EX X A)
    (PROGN
     (SETQ EX (SIMP* (LIMLOGSORT EX)))
     (COND ((EQUAL A 'INFINITY) (REDERR "Cannot approach infinity from above"))
           ((EQUAL A '(MINUS INFINITY))
            (LIMIT
             (PREPSQ
              (SUBSQ EX
                     (LIST
                      (CONS X
                            (LIST 'QUOTIENT (MINUS 1)
                                  (LIST 'EXPT '*EPS* 2))))))
             '*EPS* 0))
           (T
            (LIMIT
             (PREPSQ
              (SUBSQ EX (LIST (CONS X (LIST 'PLUS A (LIST 'EXPT '*EPS* 2))))))
             '*EPS* 0))))) 
(PUT 'LIMIT- 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT- 'DEFINED-ON-LINE '108) 
(PUT 'LIMIT- 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT- 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT- (EX X A)
    (PROGN
     (SETQ EX (SIMP* (LIMLOGSORT EX)))
     (COND
      ((EQUAL A 'INFINITY)
       (LIMIT
        (PREPSQ
         (SUBSQ EX (LIST (CONS X (LIST 'QUOTIENT 1 (LIST 'EXPT '*EPS* 2))))))
        '*EPS* 0))
      ((EQUAL A '(MINUS INFINITY))
       (REDERR "Cannot approach -infinity from below"))
      (T
       (LIMIT
        (PREPSQ
         (SUBSQ EX (LIST (CONS X (LIST 'DIFFERENCE A (LIST 'EXPT '*EPS* 2))))))
        '*EPS* 0))))) 
(PUT 'LIMIT 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT 'DEFINED-ON-LINE '118) 
(PUT 'LIMIT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT (EX XXX A)
    ((LAMBDA (*COMBINELOGS |LHOP#| |LPLUS#| |LIM00#| LSIMPDPTH)
       (LIMIT0 (LIMLOGSORT EX) XXX A))
     NIL 0 0 NIL 0)) 
(PUT 'LIMLOGSORT 'NUMBER-OF-ARGS 1) 
(PUT 'LIMLOGSORT 'DEFINED-ON-LINE '121) 
(PUT 'LIMLOGSORT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMLOGSORT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMLOGSORT (X)
    (PROG (*PRECISE)
      (SETQ X (PREPSQ (SIMP* X)))
      (RETURN (COND ((GREATERP (COUNTOF 'LOG X) 1) (LOGSORT X)) (T X))))) 
(PUT 'COUNTOF 'NUMBER-OF-ARGS 2) 
(PUT 'COUNTOF 'DEFINED-ON-LINE '127) 
(PUT 'COUNTOF 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'COUNTOF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COUNTOF (U V)
    (COND ((EQUAL U V) 1) ((ATOM V) 0)
          (T (PLUS (COUNTOF U (CAR V)) (COUNTOF U (CDR V)))))) 
(PUT 'SIMPLIMIT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLIMIT 'DEFINED-ON-LINE '131) 
(PUT 'SIMPLIMIT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'SIMPLIMIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIMIT (U)
    (PROG (FN EXPRN VAR VAL V RESULT *PRECISE)
      (COND
       ((NEQ (LENGTH U) 4)
        (RERROR 'LIMIT 1
                (LIST "Improper number of arguments to" (CAR U) "operator"))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2 "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): Entering simplimit:")
         (TERPRI* T)
         (MATHPRINT U)
         NIL)))
      (SETQ FN (CAR U))
      (SETQ EXPRN (CADR U))
      (SETQ VAR (*A2K (CADDR U)))
      (SETQ VAL (CADDDR U))
      ((LAMBDA (*PROTFG)
         (SETQ V
                 (ERRORSET*
                  (LIST 'APPLY (MKQUOTE FN) (MKQUOTE (LIST EXPRN VAR VAL)))
                  NIL)))
       T)
      (SETQ RESULT
              (COND
               ((OR (ERRORP V) (EQUAL (SETQ V (CAR V)) (REVAL1 'FAILED NIL)))
                (MKSQ (LIST FN (REVAL1 EXPRN NIL) VAR VAL) 1))
               (T (SIMP* V))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2 "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): simplimit result:")
         (TERPRI* T)
         (PRINTSQ RESULT)
         NIL)))
      (RETURN RESULT))) 
(SWITCH (LIST 'LIMIT_NOHOPITAL)) 
(PUT 'LIMIT0 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT0 'DEFINED-ON-LINE '157) 
(PUT 'LIMIT0 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT0 (EXP X A)
    (PROG (EXP1 RESULT)
      (SETQ EXP1 (SIMP* EXP))
      (COND
       ((EQUAL A 'INFINITY)
        (PROGN
         (SETQ RESULT
                 (LIMIT00
                  (SUBSQ EXP1
                         (LIST (CONS X (LIST 'QUOTIENT 1 (LIST 'EXPT X 2)))))
                  X))
         (RETURN RESULT)
         (RETURN
          (COND ((NOT (EQUAL RESULT (REVAL1 'FAILED NIL))) RESULT)
                (T (LIMIT2 (PREPF (CAR EXP1)) (PREPF (CDR EXP1)) X A))))
         NIL)))
      (COND
       ((EQUAL A '(MINUS INFINITY))
        (PROGN
         (SETQ RESULT
                 (LIMIT00
                  (SUBSQ EXP1
                         (LIST
                          (CONS X
                                (LIST 'QUOTIENT (MINUS 1) (LIST 'EXPT X 2)))))
                  X))
         (RETURN RESULT)
         (RETURN
          (COND ((NOT (EQUAL RESULT (REVAL1 'FAILED NIL))) RESULT)
                (T (LIMIT2 (PREPF (CAR EXP1)) (PREPF (CDR EXP1)) X A))))
         NIL)))
      (RETURN
       ((LAMBDA (Y)
          (PROGN
           ((LAMBDA (*EXPANDLOGS *PROTFG)
              (SETQ Y
                      (ERRORSET*
                       (LIST 'SUBSQ (MKQUOTE (SETQ EXP (SIMP* EXP)))
                             (MKQUOTE (LIST (CONS X A))))
                       NIL)))
            T T)
           (COND
            ((AND (NOT (ERRORP Y))
                  (NOT (EQUAL (SETQ Y (CAR Y)) (REVAL1 'FAILED NIL))))
             (MK*SQ Y))
            ((NEQ A 0)
             (LIMIT00 (SUBSQ EXP1 (LIST (CONS X (LIST 'PLUS A X)))) X))
            (T (LIMIT00 EXP1 X)))))
        NIL)))) 
(PUT 'LIMIT-EXPTCOMBINE 'NUMBER-OF-ARGS 1) 
(PUT 'LIMIT-EXPTCOMBINE 'DEFINED-ON-LINE '189) 
(PUT 'LIMIT-EXPTCOMBINE 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT-EXPTCOMBINE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMIT-EXPTCOMBINE (P)
    (COND
     ((AND (EQCAR P 'QUOTIENT) (EQCAR (CADR P) 'EXPT) (EQCAR (CADDR P) 'EXPT)
           (EQUAL (CADDR (CADR P)) (CADDR (CADDR P))) (NUMBERP (CADR (CADR P)))
           (NUMBERP (CADR (CADDR P))))
      (SETQ P
              (LIST 'EXPT (LIST 'QUOTIENT (CADR (CADR P)) (CADR (CADDR P)))
                    (CADDR (CADR P)))))
     (T P))) 
(DE LIMIT-FACTRPREP (P) ((LAMBDA (*FACTOR) (PREPSQ (SIMP* P))) T)) 
(PUT 'LIMIT-FACTRPREP 'NUMBER-OF-ARGS 1) 
(PUT 'LIMIT-FACTRPREP 'DEFINED-ON-LINE '196) 
(PUT 'LIMIT-FACTRPREP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT-FACTRPREP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LIMIT-FACTRPREP 'INLINE
      '(LAMBDA (P) ((LAMBDA (*FACTOR) (PREPSQ (SIMP* P))) T))) 
(PUT 'LIMIT00 'NUMBER-OF-ARGS 2) 
(PUT 'LIMIT00 'DEFINED-ON-LINE '199) 
(PUT 'LIMIT00 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT00 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMIT00 (EX X)
    ((LAMBDA (TRLIMITLEVEL*)
       (PROG (P P1 Z XPWRLCM LIM LS RESULT)
         (COND
          (*TRLIMIT
           (PROGN
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): Entering limit00:")
            (TERPRI* T)
            (PRINTSQ EX)
            (PRIN2* "w.r.t.")
            (MATHPRINT X)
            NIL)))
         (SETQ P (LIMIT-EXPTCOMBINE (PREPSQ EX)))
         (COND ((SETQ LIM (CRLIMITSET P X)) (GO RET)))
         (COND
          ((NOT |LIM00#|)
           (PROGN
            (SETQ |LIM00#| (NOT *LIM00REC))
            (SETQ P1
                    (LIMIT-EXPTCOMBINE
                     ((LAMBDA (*FACTOR) (PREPSQ (SIMP* P))) T)))
            (COND
             ((NEQ (SETQ XPWRLCM (XPWRLCMP P1 X)) 1)
              (PROGN
               (SETQ EX (SUBSQ EX (LIST (CONS X (LIST 'EXPT X XPWRLCM)))))
               (SETQ P1
                       (LIMIT-EXPTCOMBINE
                        ((LAMBDA (*FACTOR) (PREPSQ (SIMP* (PREPSQ EX))))
                         T))))))
            (COND
             ((NEQ (SETQ Z (PWRDENP P1 X)) 1)
              (SETQ EX (SIMP* (LIST 'EXPT P1 Z)))))
            (COND
             ((SETQ LIM
                      (CRLIMITSET (SETQ P (LIMIT-EXPTCOMBINE (PREPSQ EX))) X))
              (GO RET))))))
         (COND ((GREATERP LSIMPDPTH |LD0#|) (SETQ LIM (REVAL1 'FAILED NIL)))
               (T
                (PROGN
                 (SETQ LSIMPDPTH (PLUS LSIMPDPTH 1))
                 (SETQ LS T)
                 (SETQ LIM (LIMSIMP P X))
                 (COND
                  ((AND (EQUAL (PREPSQ (SIMP* LIM)) 'FAILED)
                        (EQUAL LSIMPDPTH 1))
                   (PROGN
                    (SETQ |EXPTCONV#| NIL)
                    (SETQ P (EXPT2EXP P X))
                    (COND (|EXPTCONV#| (SETQ LIM (LIMSIMP P X))))))))))
        RET
         (SETQ RESULT
                 (PROGN
                  (COND (LS (SETQ LSIMPDPTH (DIFFERENCE LSIMPDPTH 1))))
                  (COND ((OR (NOT Z) (EQUAL Z 1) (EQUAL LIM 0)) LIM)
                        ((EQUAL (SETQ LS (PREPSQ (SIMP* LIM)))
                                '(MINUS INFINITY))
                         (COND
                          ((EQUAL (EXPT (MINUS 1) Z) 1) (REVAL1 'INFINITY NIL))
                          (T LIM)))
                        ((MEMBER LS '(INFINITY FAILED)) LIM)
                        (T
                         (MK*SQ
                          (SIMP*
                           (LIST 'EXPT (PREPSQ (SIMP* LIM))
                                 (LIST 'QUOTIENT 1 Z))))))))
         (COND
          (*TRLIMIT
           (PROGN
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): limit00 returns")
            (MATHPRINT RESULT)
            NIL)))
         (RETURN RESULT)))
     (PLUS TRLIMITLEVEL* 1))) 
(PUT 'EXPT2EXP 'NUMBER-OF-ARGS 2) 
(PUT 'EXPT2EXP 'DEFINED-ON-LINE '244) 
(PUT 'EXPT2EXP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'EXPT2EXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPT2EXP (P X)
    (COND ((ATOM P) P)
          ((AND (EQCAR P 'EXPT) (NOT (FREEOF (CADR P) X))
                (NOT (FREEOF (CADDR P) X)))
           (PROGN
            (SETQ |EXPTCONV#| T)
            (LIST 'EXPT 'E (LIST 'TIMES (LIST 'LOG (CADR P)) (CADDR P)))))
          (T (CONS (EXPT2EXP (CAR P) X) (EXPT2EXP (CDR P) X))))) 
(PUT 'XPWRLCMP 'NUMBER-OF-ARGS 2) 
(PUT 'XPWRLCMP 'DEFINED-ON-LINE '251) 
(PUT 'XPWRLCMP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'XPWRLCMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XPWRLCMP (P X)
    (COND ((ATOM P) 1)
          ((AND (EQCAR P 'EXPT) (EQUAL (CADR P) X)) (GETDENOM (CADDR P)))
          ((EQCAR P 'SQRT) (GETDENOMX (CADR P) X))
          (T (LCM (XPWRLCMP (CAR P) X) (XPWRLCMP (CDR P) X))))) 
(PUT 'GETDENOMX 'NUMBER-OF-ARGS 2) 
(PUT 'GETDENOMX 'DEFINED-ON-LINE '257) 
(PUT 'GETDENOMX 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'GETDENOMX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETDENOMX (P X)
    (COND ((FREEOF P X) 1) ((EQCAR P 'MINUS) (GETDENOMX (CADR P) X))
          ((OR (EQUAL P X) (AND (EQCAR P 'TIMES) (MEMBER X (CDR P)))) 2)
          (T (XPWRLCMP P X)))) 
(PUT 'GETDENOM 'NUMBER-OF-ARGS 1) 
(PUT 'GETDENOM 'DEFINED-ON-LINE '263) 
(PUT 'GETDENOM 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'GETDENOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETDENOM (P)
    (COND ((EQCAR P 'MINUS) (GETDENOM (CADR P)))
          ((AND (EQCAR P 'QUOTIENT) (NUMBERP (CADDR P))) (CADDR P)) (T 1))) 
(PUT 'PWRDENP 'NUMBER-OF-ARGS 2) 
(PUT 'PWRDENP 'DEFINED-ON-LINE '268) 
(PUT 'PWRDENP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'PWRDENP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PWRDENP (P X)
    (COND ((ATOM P) 1)
          ((AND (EQCAR P 'EXPT) (NOT (FREEOF (CADR P) X)))
           (GETDENOM (CADDR P)))
          ((AND (EQCAR P 'SQRT) (NOT (FREEOF (CADR P) X))) 2)
          ((EQCAR P 'MINUS) (PWRDENP (CADR P) X))
          ((MEMBER (CAR P) '(TIMES QUOTIENT))
           ((LAMBDA (M)
              (PROGN
               (PROG (C)
                 (SETQ C (CDR P))
                LAB
                 (COND ((NULL C) (RETURN NIL)))
                 ((LAMBDA (C) (SETQ M (LCM M (PWRDENP C X)))) (CAR C))
                 (SETQ C (CDR C))
                 (GO LAB))
               M))
            1))
          ((ATOM (CAR P)) 1) (T (LCM (PWRDENP (CAR P) X) (PWRDENP (CDR P) X))))) 
(PUT 'LIMITSET 'NUMBER-OF-ARGS 3) 
(PUT 'LIMITSET 'DEFINED-ON-LINE '280) 
(PUT 'LIMITSET 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMITSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMITSET (EX X A)
    (PROG (RESULT EX_IN)
      (COND
       (*TRLIMIT
        (PROGN
         (SETQ EX_IN EX)
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): Trying power series expansion using ")
         (PRIN2* (COND (*USETAYLOR "Taylor w.r.t.") (T "TPS w.r.t.")))
         (MATHPRINT X)
         (PRIN2* "around")
         (MATHPRINT A)
         (PRIN2* "of expression")
         (MATHPRINT EX)
         NIL)))
      (COND
       (*USETAYLOR
        (PROGN
         ((LAMBDA (*PROTFG)
            (SETQ EX
                    (ERRORSET*
                     (LIST 'LIMIT1T (MKQUOTE EX) (MKQUOTE X) (MKQUOTE A))
                     NIL)))
          T)
         (SETQ RESULT (COND ((ERRORP EX) NIL) (T (CAR EX))))))
       (T
        (PROG (OLDPSLIM)
          (SETQ OLDPSLIM (SIMPPSEXPLIM '(1)))
          ((LAMBDA (*PROTFG)
             (SETQ EX
                     (ERRORSET*
                      (LIST 'LIMIT1P (MKQUOTE EX) (MKQUOTE X) (MKQUOTE A))
                      NIL)))
           T)
          (SIMPPSEXPLIM (LIST (CAR OLDPSLIM)))
          (SETQ RESULT (COND ((ERRORP EX) NIL) (T (CAR EX)))))))
      (COND
       (*TRLIMIT
        (PROGN
         (COND
          ((NULL RESULT)
           (PROGN
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): Expansion failed")
            (TERPRI* T)
            NIL))
          (T
           (PROGN
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): Power series expansion gives")
            (TERPRI* T)
            (MATHPRINT (LIST 'REPLACEBY EX_IN RESULT))
            NIL)))
         NIL)))
      (RETURN RESULT))) 
(PUT 'LIMIT1T 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT1T 'DEFINED-ON-LINE '313) 
(PUT 'LIMIT1T 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT1T 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT1T (EX X A)
    (PROG (NNN VVV OLDKLIST)
      (SETQ OLDKLIST (GET 'TAYLOR* 'KLIST))
      (SETQ EX (LIST EX X A 0))
      (SETQ VVV (ERRORSET* (LIST 'SIMPTAYLOR (MKQUOTE EX)) *BACKTRACE))
      (RESETKLIST 'TAYLOR* OLDKLIST)
      (COND ((ERRORP VVV) (PROGN (COND (*BACKTRACE (BREAK))) (RETURN NIL)))
            (T (SETQ EX (CAR VVV))))
      (COND ((KERNP EX) (SETQ EX (CAAAR (CAR EX)))) (T (RETURN NIL)))
      (COND ((NOT (EQCAR EX 'TAYLOR*)) (RETURN NIL)) (T (SETQ EX (CADR EX))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND EX (NULL (CAR (CDR (CAR EX)))))) (RETURN NIL)))
        (SETQ EX (CDR EX))
        (GO WHILELABEL))
      (COND ((NULL EX) (RETURN (SETQ |#NNN| 0)))
            (T (SETQ |#NNN| (SETQ NNN (CAAAAR EX)))))
      (SETQ VVV (CDAR EX))
      (RETURN
       (COND ((TAYEXP-GREATERP NNN 0) 0) ((EQUAL NNN 0) (MK*SQ VVV))
             (*COMPLEX 'INFINITY)
             (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ NNN (CAR VVV)))
              (COND ((|:MINUSP| NNN) '(MINUS INFINITY)) (T 'INFINITY)))
             ((NOT (REALVALUEDP (PREPSQ VVV))) 'INFINITY)
             (T
              (PROGN
               (SETQ VVV (SIMP-SIGN (LIST (MK*SQ VVV))))
               (COND
                ((AND (ATOM (CDR VVV)) (FIXP (CAR VVV)))
                 (PROGN
                  (COND ((EQUAL (CAR VVV) 0) 0) ((EQUAL (CAR VVV) 1) 'INFINITY)
                        (T '(MINUS INFINITY)))))
                (T (REVAL1 (LIST 'TIMES (MK*SQ VVV) 'INFINITY) NIL))))))))) 
(PUT 'LIMIT1P 'NUMBER-OF-ARGS 3) 
(PUT 'LIMIT1P 'DEFINED-ON-LINE '346) 
(PUT 'LIMIT1P 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMIT1P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMIT1P (EX X A)
    (PROG (AAA NNN VVV)
      (SETQ AAA (MK*SQ (SIMPPS1 EX X A)))
      (SETQ |#NNN| (SETQ NNN (MK*SQ (SIMPPSORDER (LIST AAA)))))
      (SETQ VVV (SIMPPSTERM1 AAA (MIN NNN 0)))
      (RETURN
       (COND ((GREATERP NNN 0) 0) ((EQUAL NNN 0) (MK*SQ VVV))
             (*COMPLEX 'INFINITY)
             (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ NNN (CAR VVV)))
              (COND ((|:MINUSP| NNN) '(MINUS INFINITY)) (T 'INFINITY)))
             ((NOT (REALVALUEDP (PREPSQ VVV))) 'INFINITY)
             (T
              (PROGN
               (SETQ VVV (SIMP-SIGN (LIST (MK*SQ VVV))))
               (COND
                ((AND (ATOM (CDR VVV)) (FIXP (CAR VVV)))
                 (PROGN
                  (COND ((EQUAL (CAR VVV) 0) 0) ((EQUAL (CAR VVV) 1) 'INFINITY)
                        (T '(MINUS INFINITY)))))
                (T (REVAL1 (LIST 'TIMES (MK*SQ VVV) 'INFINITY) NIL))))))))) 
(PUT 'CRLIMITSET 'NUMBER-OF-ARGS 2) 
(PUT 'CRLIMITSET 'DEFINED-ON-LINE '366) 
(PUT 'CRLIMITSET 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'CRLIMITSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CRLIMITSET (EX X)
    ((LAMBDA (R C *MSG)
       (PROG (LIM1 LIM2 N1 FG LIMCR |#NNN|)
         (SETQ LIM1 (LIMITSET EX X 0))
         (COND ((NULL LIM1) (COND ((AND R C) (RETURN NIL)) (T (GO A)))))
         (COND
          ((OR (LESSP (SETQ N1 |#NNN|) 0) (MEMBER LIM1 |ABSLIMS#|) (AND R C))
           (RETURN LIM1)))
        A
         (COND ((NOT *CRLIMTEST) (RETURN LIM1)))
         (COND ((NOT R) (ON (LIST 'ROUNDED))))
         (COND ((NOT C) (ON (LIST 'COMPLEX))))
         (COND
          ((OR (NOT (SETQ LIM2 (LIMITSET EX X 0))) (GREATERP |#NNN| N1))
           (PROGN (SETQ FG T) (GO RET))))
         (COND ((OR (LESSP |#NNN| N1) (MEMBER LIM2 |ABSLIMS#|)) (GO RET)))
         (COND
          ((AND (SETQ LIMCR (TOPEVALSETSQ LIM1))
                (EVALEQUAL (PREPSQ (SIMP* LIM2)) (PREPSQ LIMCR)))
           (SETQ FG T)))
        RET
         (COND ((NOT R) (OFF (LIST 'ROUNDED))))
         (COND ((NOT C) (OFF (LIST 'COMPLEX))))
         (RETURN (COND (FG LIM1) (T LIM2)))))
     *ROUNDED *COMPLEX NIL)) 
(PUT 'TOPEVALSETSQ 'NUMBER-OF-ARGS 1) 
(PUT 'TOPEVALSETSQ 'DEFINED-ON-LINE '386) 
(PUT 'TOPEVALSETSQ 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'TOPEVALSETSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOPEVALSETSQ (U)
    ((LAMBDA (R C *MSG)
       (PROGN
        (COND ((NOT R) (ON (LIST 'ROUNDED))))
        (COND ((NOT C) (ON (LIST 'COMPLEX))))
        ((LAMBDA (*PROTFG)
           (SETQ U
                   (ERRORSET*
                    (LIST 'SIMP*
                          (LIST 'AEVAL
                                (LIST 'PREPSQ (LIST 'SIMP* (MKQUOTE U)))))
                    NIL)))
         T)
        (COND ((NOT R) (OFF (LIST 'ROUNDED))))
        (COND ((NOT C) (OFF (LIST 'COMPLEX))))
        (COND ((ERRORP U) NIL) (T (CAR U)))))
     *ROUNDED *COMPLEX NIL)) 
(PUT 'TIMES 'LIMSFN 'LTIMESFN) 
(PUT 'QUOTIENT 'LIMSFN 'LQUOTFN) 
(PUT 'PLUS 'LIMSFN 'LPLUSFN) 
(PUT 'EXPT 'LIMSFN 'LEXPTFN) 
(PUT 'LIMSIMP 'NUMBER-OF-ARGS 2) 
(PUT 'LIMSIMP 'DEFINED-ON-LINE '399) 
(PUT 'LIMSIMP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMSIMP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMSIMP (EX X)
    (PROG (Y C Z M EX0)
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): Entering limsimp:")
         (TERPRI* T)
         (MATHPRINT EX)
         (PRIN2* "w.r.t.")
         (MATHPRINT X)
         NIL)))
      (COND ((EQCAR EX 'MINUS) (PROGN (SETQ M T) (SETQ EX (CADR EX)))))
      (SETQ EX0 EX)
      (COND
       ((NOT (ATOM EX))
        (PROGN
         (COND
          ((SETQ Z (GET (SETQ Y (CAR EX)) 'LIMSFN))
           (SETQ EX (APPLY Z (LIST EX X)))))))
       (T (PROGN (COND ((EQ EX X) (SETQ EX 0))) (GO RET))))
      (COND ((EQ Y 'PLUS) (GO RET)))
      (COND ((EQ Y 'EXPT) (COND (EX (GO RET)) (T (SETQ EX (CONS EX0 1))))))
      (COND (Z (PROGN (SETQ Z (CAR EX)) (SETQ C (CDR EX))))
            (T
             (PROGN
              (SETQ EX (SIMP* EX))
              (SETQ Z (PREPF (CAR EX)))
              (SETQ C (PREPF (CDR EX))))))
      (SETQ EX (LHOPITAL Z C X 0))
     RET
      (COND
       ((AND M (NEQ (PREPSQ (SIMP* EX)) 'FAILED))
        (SETQ EX (REVAL1 (LMINUS2 EX) NIL))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): limsimp returns")
         (MATHPRINT EX)
         NIL)))
      (RETURN EX))) 
(PUT 'LMINUS2 'NUMBER-OF-ARGS 1) 
(PUT 'LMINUS2 'DEFINED-ON-LINE '431) 
(PUT 'LMINUS2 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LMINUS2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LMINUS2 (EX)
    (COND ((NUMBERP EX) (MINUS EX)) ((EQCAR EX 'MINUS) (CADR EX))
          (T (LIST 'MINUS EX)))) 
(PUT 'LTIMESFN 'NUMBER-OF-ARGS 2) 
(PUT 'LTIMESFN 'DEFINED-ON-LINE '436) 
(PUT 'LTIMESFN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LTIMESFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LTIMESFN (EX X) (SPECCHK EX 1 X)) 
(PUT 'LQUOTFN 'NUMBER-OF-ARGS 2) 
(PUT 'LQUOTFN 'DEFINED-ON-LINE '438) 
(PUT 'LQUOTFN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LQUOTFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LQUOTFN (EX X) (SPECCHK (CADR EX) (CADDR EX) X)) 
(PUT 'LEXPTFN 'NUMBER-OF-ARGS 2) 
(PUT 'LEXPTFN 'DEFINED-ON-LINE '442) 
(PUT 'LEXPTFN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LEXPTFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LEXPTFN (EX X)
    (COND
     ((AND (NOT (EVALEQUAL (CADR EX) 0)) (FREEOF (CADR EX) X)
           (EQUAL (LIMIT00 (SIMP* (CADDR EX)) X) 0))
      1))) 
(PUT 'SPECCHK 'NUMBER-OF-ARGS 3) 
(PUT 'SPECCHK 'DEFINED-ON-LINE '447) 
(PUT 'SPECCHK 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'SPECCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPECCHK (TOP BOT X)
    (PROG (TLIST BLIST TINFS BINFS TLOGS BLOGS TZROS BZROS TNRMS BNRMS M)
      (COND ((EQCAR TOP 'MINUS) (PROGN (SETQ M T) (SETQ TOP (CADR TOP)))))
      (COND
       ((EQCAR BOT 'MINUS) (PROGN (SETQ M (NOT M)) (SETQ BOT (CADR BOT)))))
      (SETQ TLIST (LIMSORT (TIMSIFT TOP X) X))
      (SETQ BLIST (LIMSORT (TIMSIFT BOT X) X))
      (SETQ TINFS (CDR (SETQ TLOGS (LOGCOMB (CADR TLIST) X))))
      (SETQ TLOGS (CAR TLOGS))
      (SETQ BINFS (CDR (SETQ BLOGS (LOGCOMB (CADR BLIST) X))))
      (SETQ BLOGS (CAR BLOGS))
      (SETQ TZROS (CAR TLIST))
      (SETQ TNRMS (CADDR TLIST))
      (SETQ BZROS (CAR BLIST))
      (SETQ BNRMS (CADDR BLIST))
      (COND
       ((AND TLOGS (NOT BLOGS))
        (PROGN
         (SETQ TOP (TRIML (APPEND TLOGS TNRMS)))
         (SETQ BOT
                 (TRIML
                  (APPEND BZROS
                          (APPEND BINFS
                                  (APPEND BNRMS
                                          (TRIMQ (APPEND TINFS TZROS)))))))))
       ((AND BLOGS (NOT TLOGS))
        (PROGN
         (SETQ BOT (TRIML (APPEND BLOGS BNRMS)))
         (SETQ TOP
                 (TRIML
                  (APPEND TZROS
                          (APPEND TINFS
                                  (APPEND TNRMS
                                          (TRIMQ (APPEND BINFS BZROS)))))))))
       (T
        (PROGN
         (COND
          ((AND (NULL TNRMS) (NOT (NULL TZROS)) (NULL BZROS) (NULL BINFS)
                (NULL TINFS))
           (PROGN
            (SETQ TNRMS
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X TZROS)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (X) X) (CAR X)) NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) X) (CAR X)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ TZROS NIL))))
         (SETQ TOP (TRIML (APPEND (CADR TLIST) (APPEND TNRMS (TRIMQ BZROS)))))
         (SETQ BOT
                 (TRIML (APPEND (CADR BLIST) (APPEND BNRMS (TRIMQ TZROS))))))))
      (COND (M (SETQ TOP (LIST 'MINUS TOP))))
      (RETURN (CONS TOP BOT)))) 
(PUT 'TRIMQ 'NUMBER-OF-ARGS 1) 
(PUT 'TRIMQ 'DEFINED-ON-LINE '475) 
(PUT 'TRIMQ 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'TRIMQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIMQ (L)
    (COND
     (L
      (LIST
       (LIST 'QUOTIENT 1
             (COND ((GREATERP (LENGTH L) 1) (CONS 'TIMES L)) (T (CAR L)))))))) 
(PUT 'TRIML 'NUMBER-OF-ARGS 1) 
(PUT 'TRIML 'DEFINED-ON-LINE '479) 
(PUT 'TRIML 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'TRIML 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIML (L)
    (COND ((NULL L) 1) ((GREATERP (LENGTH L) 1) (CONS 'TIMES L)) (T (CAR L)))) 
(PUT 'LIMSORT 'NUMBER-OF-ARGS 2) 
(PUT 'LIMSORT 'DEFINED-ON-LINE '482) 
(PUT 'LIMSORT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMSORT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMSORT (EX X)
    (PROG (ZROS INFS NRMS Q S)
      (PROG (C)
        (SETQ C EX)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ Q (CAR (SETQ S (SIMP* (LIMIT00 (SIMP* C) X)))))
            (COND
             ((OR (ATOM Q) (ATOM (CAR Q)))
              (PROGN
               (COND ((NOT (|:ZEROP| Q)) (SETQ NRMS (CONS (PREPD Q) NRMS)))
                     (T (SETQ ZROS (CONS C ZROS))))))
             ((MEMQ (CAAAR Q) '(FAILED INFINITY)) (SETQ INFS (CONS C INFS)))
             (T (SETQ NRMS (CONS (PREPSQ S) NRMS))))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (RETURN (LIST ZROS INFS NRMS)))) 
(PUT 'LOGCOMB 'NUMBER-OF-ARGS 2) 
(PUT 'LOGCOMB 'DEFINED-ON-LINE '492) 
(PUT 'LOGCOMB 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LOGCOMB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOGCOMB (TINF X)
    (PROG (TLOG C Z)
      (PROG ()
       WHILELABEL
        (COND ((NOT TINF) (RETURN NIL)))
        (PROGN
         (SETQ C (CAR TINF))
         (SETQ TINF (CDR TINF))
         (COND
          ((OR (EQCAR C 'LOG) (AND (EQCAR C 'EXPT) (EQCAR (CADR C) 'LOG))
               (AND (EQCAR C 'PLUS)
                    (OR (EQCAR (CADR (SETQ C (LOGJOIN C X))) 'LOG)
                        (AND (EQCAR (CADR C) 'MINUS) (EQCAR (CADADR C) 'LOG)))
                    (FREEOF (CDDR C) X)))
           (SETQ TLOG (CONS C TLOG)))
          (T (SETQ Z (CONS C Z)))))
        (GO WHILELABEL))
      (RETURN (CONS TLOG (REVERSIP Z))))) 
(PUT 'LOGJOIN 'NUMBER-OF-ARGS 2) 
(PUT 'LOGJOIN 'DEFINED-ON-LINE '506) 
(PUT 'LOGJOIN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LOGJOIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOGJOIN (P X)
    (PROG (LL Z)
      (PROG (C)
        (SETQ C (CDR P))
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (COND ((FREEOF C X) (SETQ Z (CONS C Z)))
                 ((EQCAR C 'LOG) (SETQ LL (CONS (CADR C) LL)))
                 ((AND (EQCAR C 'MINUS) (EQCAR (CADR C) 'LOG))
                  (SETQ LL (CONS (LIST 'QUOTIENT 1 (CADADR C)) LL)))
                 (T (SETQ Z (CONS C Z)))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (COND (LL (SETQ LL (LIST (LIST 'LOG (CONS 'TIMES LL))))))
      (RETURN (CONS (CAR P) (APPEND LL (REVERSIP Z)))))) 
(PUT 'TIMSIFT 'NUMBER-OF-ARGS 2) 
(PUT 'TIMSIFT 'DEFINED-ON-LINE '518) 
(PUT 'TIMSIFT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'TIMSIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TIMSIFT (EX X)
    (COND ((EQCAR EX 'TIMES) (CDR EX)) ((EQCAR EX 'PLUS) (LIST (LOGJOIN EX X)))
          (T (LIST EX)))) 
(PUT 'LPLUSFN 'NUMBER-OF-ARGS 2) 
(PUT 'LPLUSFN 'DEFINED-ON-LINE '525) 
(PUT 'LPLUSFN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LPLUSFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPLUSFN (EX X)
    (PROG (Z INFS NRMS VALS VP VM CZ VNIX)
      (SETQ |LPLUS#| (PLUS |LPLUS#| 1))
      (COND ((GREATERP |LPLUS#| 4) (RETURN (REVAL1 'FAILED NIL))))
      (SETQ Z (LIMSORT (CDR EX) X))
      (SETQ NRMS (CADDR Z))
      (SETQ INFS (CADR Z))
      (COND
       ((GREATERP (LENGTH INFS) 1)
        (PROGN
         (SETQ INFS (LOGJOIN (CONS 'PLUS INFS) X))
         (SETQ INFS (COND ((EQCAR INFS 'PLUS) (CDR INFS)) (T (LIST INFS)))))))
      (SETQ VALS
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C INFS)
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (C)
                                    (MINFIX
                                     (PREPSQ (SIMP* (LIMIT00 (SIMP* C) X)))))
                                  (CAR C))
                                 NIL)))
               LOOPLABEL
                (SETQ C (CDR C))
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (C)
                            (MINFIX (PREPSQ (SIMP* (LIMIT00 (SIMP* C) X)))))
                          (CAR C))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Z INFS)
      (PROG (C)
        (SETQ C VALS)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ CZ (CAR Z))
            (SETQ Z (CDR Z))
            (COND ((EQ C 'INFINITY) (SETQ VP (CONS CZ VP)))
                  ((EQUAL C '(MINUS INFINITY)) (SETQ VM (CONS CZ VM)))
                  ((EQ C 'FAILED) (SETQ VNIX (CONS CZ VNIX)))
                  (T (SETQ NRMS (CONS CZ NRMS))))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (COND
       ((OR (AND VM (NOT VP)) (AND VP (NOT VM)) (EQUAL (LENGTH VNIX) 1)
            (GREATERP (LENGTH VM) 1) (GREATERP (LENGTH VP) 1))
        (RETURN (REVAL1 'FAILED NIL))))
      (COND (VM (SETQ VM (QFORM (CAR VP) VM))))
      (COND (VNIX (SETQ VNIX (QFORM (CAR VNIX) (CDR VNIX)))))
      (SETQ VM (APPEND NRMS (APPEND VM VNIX)))
      (RETURN
       (COND ((NULL VM) 0)
             (T
              (LIMIT00
               (SIMP*
                (COND ((GREATERP (LENGTH VM) 1) (CONS 'PLUS VM)) (T (CAR VM))))
               X)))))) 
(PUT 'MINFIX 'NUMBER-OF-ARGS 1) 
(PUT 'MINFIX 'DEFINED-ON-LINE '556) 
(PUT 'MINFIX 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'MINFIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MINFIX (V)
    (COND ((AND (EQCAR V 'MINUS) (NUMBERP (CADR V))) (MINUS (CADR V))) (T V))) 
(PUT 'QFORM 'NUMBER-OF-ARGS 2) 
(PUT 'QFORM 'DEFINED-ON-LINE '559) 
(PUT 'QFORM 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'QFORM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QFORM (A B)
    (LIST
     (LIST 'QUOTIENT
           (LIST 'PLUS 1
                 (LIST 'QUOTIENT
                       (COND ((EQUAL (LENGTH B) 1) (CAR B)) (T (CONS 'PLUS B)))
                       A))
           (LIST 'QUOTIENT 1 A)))) 
(PUT 'LHOPITAL 'NUMBER-OF-ARGS 4) 
(PUT 'LHOPITAL 'DEFINED-ON-LINE '564) 
(PUT 'LHOPITAL 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LHOPITAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LHOPITAL (TOP BOT XXX A)
    (PROG (LIMT LIMB NVT NVB RESULT)
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): Trying l'Hopitals rule with numerator")
         (TERPRI* T)
         (MATHPRINT TOP)
         (PRIN2* "and denominator")
         (TERPRI* T)
         (MATHPRINT BOT)
         NIL)))
      (SETQ NVT (NOTVAL (SETQ LIMT (LIMFIX TOP XXX A))))
      (SETQ NVB (NOTVAL (SETQ LIMB (LIMFIX BOT XXX A))))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): limit of numerator is")
         (TERPRI* T)
         (MATHPRINT LIMT)
         (PRIN2* "limit of denominator is")
         (MATHPRINT LIMB)
         NIL)))
      (COND ((OR (AND (EQUAL LIMT 0) (EQUAL LIMB 0)) (AND NVT NVB)) (GO LHOP)))
      (COND ((OR (SPECVAL LIMT) (SPECVAL LIMB)) (RETURN (SPECCOMB LIMT LIMB))))
      (COND ((EQUAL LIMB 0) (RETURN (REVAL1 'INFINITY NIL))))
      (RETURN (REVAL1 (LIST 'QUOTIENT LIMT LIMB) NIL))
     LHOP
      (SETQ |LHOP#| (PLUS |LHOP#| 1))
      (COND ((GREATERP |LHOP#| 6) (RETURN (REVAL1 'FAILED NIL))))
      (SETQ RESULT
              (LIMIT0
               (PREPSQ
                (MULTSQ (DIFFSQ (SIMP* TOP) XXX)
                        (INVSQ (DIFFSQ (SIMP* BOT) XXX))))
               XXX A))
      (COND
       (*TRLIMIT
        (PROGN
         (PRIN2* "Limit (")
         (PRIN2* TRLIMITLEVEL*)
         (PRIN2* "): Application of l'Hopitals rule gives")
         (TERPRI* T)
         (MATHPRINT RESULT)
         NIL)))
      (RETURN RESULT))) 
(PUT 'NOTVAL 'NUMBER-OF-ARGS 1) 
(PUT 'NOTVAL 'DEFINED-ON-LINE '598) 
(PUT 'NOTVAL 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'NOTVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NOTVAL (LIM) (OR (NOT LIM) (INFINP (PREPSQ (SIMP* LIM))))) 
(PUT 'INFINP 'NUMBER-OF-ARGS 1) 
(PUT 'INFINP 'DEFINED-ON-LINE '601) 
(PUT 'INFINP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'INFINP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INFINP (X) (MEMBER X '(INFINITY (MINUS INFINITY)))) 
(PUT 'SPECVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SPECVAL 'DEFINED-ON-LINE '603) 
(PUT 'SPECVAL 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'SPECVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPECVAL (LIM) (OR (NOTVAL LIM) (EQ LIM 'BOUNDED))) 
(PUT 'SPECCOMB 'NUMBER-OF-ARGS 2) 
(PUT 'SPECCOMB 'DEFINED-ON-LINE '607) 
(PUT 'SPECCOMB 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'SPECCOMB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SPECCOMB (A B)
    (REVAL1
     (COND ((OR (NOT A) (NOT B) (EQ B 'BOUNDED)) 'FAILED) ((NOTVAL B) 0)
           ((NOTVAL A)
            (COND
             ((NUMBERP B)
              (COND ((GEQ B 0) A) ((EQ A 'INFINITY) '(MINUS INFINITY))
                    (T 'INFINITY)))
             (T
              ((LAMBDA (C CC)
                 (COND
                  (C
                   (PROGN
                    (SETQ C (SIMP-SIGN (LIST (MK*SQ C))))
                    (COND
                     ((AND (ATOM (CDR C)) (FIXP (CAR C)) (NEQ (CAR C) 0))
                      (SETQ CC (CAR C))))
                    (COND
                     (CC (SETQ C (COND ((EQ A 'INFINITY) 1) (T (MINUS 1))))))
                    (COND
                     (CC
                      (COND ((EQUAL (TIMES C CC) 1) 'INFINITY)
                            (T '(MINUS INFINITY))))
                     (T (LIST 'TIMES (LIST 'SIGN B) A)))))
                  (T (LIST 'QUOTIENT A B))))
               (TOPEVALSETSQ (PREPSQ (SIMP* B))) NIL))))
           (T 'FAILED))
     NIL)) 
(PUT 'LIMFIX 'NUMBER-OF-ARGS 3) 
(PUT 'LIMFIX 'DEFINED-ON-LINE '625) 
(PUT 'LIMFIX 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMFIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMFIX (EX X A)
    ((LAMBDA (VAL) (COND (VAL VAL) (T (LIMITEST EX X A)))) (LIMITSET EX X A))) 
(PUT 'LIMITEST 'NUMBER-OF-ARGS 3) 
(PUT 'LIMITEST 'DEFINED-ON-LINE '630) 
(PUT 'LIMITEST 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMITEST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMITEST (EX X A)
    ((LAMBDA (TRLIMITLEVEL*)
       (PROG (RESULT EX_IN)
         (COND
          (*TRLIMIT
           (PROGN
            (SETQ EX_IN EX)
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): Entering limitest for")
            (MATHPRINT EX)
            (PRIN2* "w.r.t. variable")
            (MATHPRINT X)
            (PRIN2* "towards")
            (MATHPRINT A)
            NIL)))
         (COND
          (EX
           (COND ((ATOM EX) (COND ((EQ EX X) A) (T EX)))
                 (T
                  (SETQ RESULT
                          (PROG (Y ARG VAL)
                            (COND
                             ((EQCAR EX 'EXPT)
                              (COND
                               ((EQ (CADR EX) 'E)
                                (SETQ EX (LIST 'EXP (CADDR EX))))
                               (T
                                (RETURN (EXPTEST (CADR EX) (CADDR EX) X A))))))
                            (COND
                             ((SETQ Y (GET (CAR EX) 'FIXFN))
                              (RETURN (APPLY1 Y (LIMFIX (CADR EX) X A))))
                             ((SETQ Y (GET (CAR EX) 'LIMCOMB))
                              (RETURN (APPLY3 Y (CDR EX) X A))))))))))
        RET
         (COND
          (*TRLIMIT
           (PROGN
            (PRIN2* "Limit (")
            (PRIN2* TRLIMITLEVEL*)
            (PRIN2* "): limitest returns")
            (TERPRI* T)
            (MATHPRINT (LIST 'REPLACEBY EX_IN RESULT))
            NIL)))
         (RETURN RESULT)))
     (PLUS TRLIMITLEVEL* 1))) 
(PUT 'EXPTEST 'NUMBER-OF-ARGS 4) 
(PUT 'EXPTEST 'DEFINED-ON-LINE '657) 
(PUT 'EXPTEST 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'EXPTEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPTEST (B N X A)
    (COND
     ((NUMBERP N)
      (COND ((LESSP N 0) (LIMQUOT1 1 (EXPTEST B (MINUS N) X A)))
            ((EQUAL N 0) 1)
            (T
             ((LAMBDA (Y)
                (COND ((EQUAL (TIMES 2 Y) N) (LIMLABS (LIMITEST B X A)))
                      (T (LIMITEST B X A))))
              (QUOTIENT N 2)))))
     ((EQUAL (CAR (SIMP-SIGN (LIST (LIST 'DIFFERENCE B 1)))) 1)
      (LIMITEST (LIST 'EXP (LIST 'TIMES N (LIST 'LOG B))) X A))
     ((EQUAL (CAR (SIMP-SIGN (LIST B))) 1)
      (LIMITEST
       (LIST 'QUOTIENT 1
             (LIST 'EXP (LIST 'TIMES N (LIST 'MINUS (LIST 'LOG B)))))
       X A)))) 
(PUT 'LIMLABS 'NUMBER-OF-ARGS 1) 
(PUT 'LIMLABS 'DEFINED-ON-LINE '668) 
(PUT 'LIMLABS 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMLABS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMLABS (A)
    (COND ((NULL A) NIL) ((INFINP A) 'INFINITY) ((EQ A 'BOUNDED) 'BOUNDED)
          (T
           (PROG (N D)
             (SETQ D (CDR (SETQ N (SIMP* A))))
             (SETQ N (CAR N))
             (RETURN
              (COND ((NULL N) A) ((NOT (NUMBERP N)) NIL)
                    (T (CONS (MK*SQ (ABS A)) D)))))))) 
(PUT 'LIMPLUS 'NUMBER-OF-ARGS 3) 
(PUT 'LIMPLUS 'DEFINED-ON-LINE '676) 
(PUT 'LIMPLUS 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMPLUS (EXL X A)
    (COND ((NULL EXL) 0)
          (T
           (LIMPLUS1 (MKALG (LIMFIX (CAR EXL) X A)) (LIMPLUS (CDR EXL) X A))))) 
(PUT 'LIMPLUS1 'NUMBER-OF-ARGS 2) 
(PUT 'LIMPLUS1 'DEFINED-ON-LINE '680) 
(PUT 'LIMPLUS1 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMPLUS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMPLUS1 (A B)
    (COND ((OR (NULL A) (NULL B)) NIL)
          ((INFINP A) (COND ((INFINP B) (COND ((EQ A B) A) (T NIL))) (T A)))
          ((INFINP B) B) ((OR (EQ A 'BOUNDED) (EQ B 'BOUNDED)) 'BOUNDED)
          (T (MK*SQ (ADDSQ (SIMP* A) (SIMP* B)))))) 
(PUT 'LIMTIMES 'NUMBER-OF-ARGS 3) 
(PUT 'LIMTIMES 'DEFINED-ON-LINE '689) 
(PUT 'LIMTIMES 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMTIMES (EXL X A)
    (COND ((NULL EXL) 1)
          (T
           (LTIMES1 (MKALG (LIMFIX (CAR EXL) X A)) (LIMTIMES (CDR EXL) X A))))) 
(PUT 'MKALG 'NUMBER-OF-ARGS 1) 
(PUT 'MKALG 'DEFINED-ON-LINE '693) 
(PUT 'MKALG 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'MKALG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKALG (X) (MINFIX (COND ((EQCAR X '*SQ) (PREPSQ (SIMP* X))) (T X)))) 
(PUT 'LTIMES1 'NUMBER-OF-ARGS 2) 
(PUT 'LTIMES1 'DEFINED-ON-LINE '696) 
(PUT 'LTIMES1 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LTIMES1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LTIMES1 (A B)
    (PROG (C)
      (RETURN
       (COND ((OR (NULL A) (NULL B)) NIL)
             ((INFINP A)
              (COND
               ((INFINP B)
                (COND ((EQUAL A B) 'INFINITY) (T '(MINUS INFINITY))))
               ((OR (EQ B 'BOUNDED) (EQUAL B 0)) NIL)
               ((EQ (SETQ C (LIMPOSP B)) 'FAILED) NIL) (C A) (T (LMINUS1 A))))
             ((INFINP B)
              (COND ((OR (EQ A 'BOUNDED) (EQUAL A 0)) NIL)
                    ((EQ (SETQ C (LIMPOSP A)) 'FAILED) NIL) (C B)
                    (T (LMINUS1 B))))
             ((OR (EQ A 'BOUNDED) (EQ B 'BOUNDED)) 'BOUNDED)
             (T (MK*SQ (MULTSQ (SIMP* A) (SIMP* B)))))))) 
(PUT 'LIMPOSP 'NUMBER-OF-ARGS 1) 
(PUT 'LIMPOSP 'DEFINED-ON-LINE '712) 
(PUT 'LIMPOSP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMPOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMPOSP (A)
    ((LAMBDA (S)
       (COND ((NOT (ATOM (CDR S))) 'FAILED) ((EQUAL (CAR S) 1) T)
             ((EQUAL (CAR S) (MINUS 1)) NIL) (T 'FAILED)))
     (SIMP-SIGN (LIST A)))) 
(PUT 'LMINUS 'NUMBER-OF-ARGS 3) 
(PUT 'LMINUS 'DEFINED-ON-LINE '719) 
(PUT 'LMINUS 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LMINUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LMINUS (EXL X A) (LMINUS1 (MKALG (LIMFIX (CAR EXL) X A)))) 
(PUT 'LMINUS1 'NUMBER-OF-ARGS 1) 
(PUT 'LMINUS1 'DEFINED-ON-LINE '722) 
(PUT 'LMINUS1 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LMINUS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LMINUS1 (A)
    (COND
     (A
      (COND ((EQ A 'INFINITY) '(MINUS INFINITY))
            ((EQUAL A '(MINUS INFINITY)) 'INFINITY) ((EQ A 'BOUNDED) A)
            (T (MK*SQ (NEGSQ (SIMP* A)))))))) 
(PUT 'LIMQUOT 'NUMBER-OF-ARGS 3) 
(PUT 'LIMQUOT 'DEFINED-ON-LINE '728) 
(PUT 'LIMQUOT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LIMQUOT (EXL X A)
    (LIMQUOT1 (MKALG (LIMFIX (CAR EXL) X A)) (MKALG (LIMFIX (CADR EXL) X A)))) 
(PUT 'LIMQUOT1 'NUMBER-OF-ARGS 2) 
(PUT 'LIMQUOT1 'DEFINED-ON-LINE '731) 
(PUT 'LIMQUOT1 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'LIMQUOT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMQUOT1 (A B)
    (PROG (C)
      (RETURN
       (COND ((OR (NULL A) (NULL B)) NIL)
             ((INFINP A)
              (COND ((INFINP B) NIL) ((EQ B 'BOUNDED) NIL) ((EQUAL B 0) A)
                    ((EQ (SETQ C (LIMPOSP B)) 'FAILED) NIL) (C A)
                    (T (LMINUS1 A))))
             ((INFINP B) 0)
             ((EQ A 'BOUNDED) (COND ((EQUAL B 0) NIL) (T 'BOUNDED)))
             ((OR (EQUAL B 0) (EQ B 'BOUNDED)) NIL)
             (T (MK*SQ (MULTSQ (SIMP* A) (INVSQ (SIMP* B))))))))) 
(PUT 'LOG 'FIXFN 'FIXLOG) 
(PUT 'SIN 'FIXFN 'FIXSIN) 
(PUT 'COS 'FIXFN 'FIXSIN) 
(PUT 'SQRT 'FIXFN 'FIXSQRT) 
(PUT 'COSH 'FIXFN 'FIXCOSH) 
(PUT 'SINH 'FIXFN 'FIXSINH) 
(PUT 'EXP 'FIXFN 'FIXEXP) 
(PUT 'PLUS 'LIMCOMB 'LIMPLUS) 
(PUT 'MINUS 'LIMCOMB 'LMINUS) 
(PUT 'TIMES 'LIMCOMB 'LIMTIMES) 
(PUT 'QUOTIENT 'LIMCOMB 'LIMQUOT) 
(PUT 'FIXLOG 'NUMBER-OF-ARGS 1) 
(PUT 'FIXLOG 'DEFINED-ON-LINE '757) 
(PUT 'FIXLOG 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXLOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXLOG (X) (COND ((ZEROP X) '(MINUS INFINITY)) ((INFINP X) 'INFINITY))) 
(PUT 'FIXSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'FIXSQRT 'DEFINED-ON-LINE '760) 
(PUT 'FIXSQRT 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXSQRT (X) (COND ((ZEROP X) 0) ((INFINP X) 'INFINITY))) 
(PUT 'FIXSIN 'NUMBER-OF-ARGS 1) 
(PUT 'FIXSIN 'DEFINED-ON-LINE '763) 
(PUT 'FIXSIN 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXSIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXSIN (X) (COND ((INFINP X) 'BOUNDED))) 
(PUT 'FIXCOSH 'NUMBER-OF-ARGS 1) 
(PUT 'FIXCOSH 'DEFINED-ON-LINE '766) 
(PUT 'FIXCOSH 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXCOSH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXCOSH (X) (COND ((INFINP X) 'INFINITY))) 
(PUT 'FIXSINH 'NUMBER-OF-ARGS 1) 
(PUT 'FIXSINH 'DEFINED-ON-LINE '769) 
(PUT 'FIXSINH 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXSINH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXSINH (X) (COND ((INFINP X) X))) 
(PUT 'FIXEXP 'NUMBER-OF-ARGS 1) 
(PUT 'FIXEXP 'DEFINED-ON-LINE '772) 
(PUT 'FIXEXP 'DEFINED-IN-FILE 'LIMIT/LIMITS.RED) 
(PUT 'FIXEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIXEXP (X)
    (COND ((ZEROP X) 1) ((EQ X 'INFINITY) X) ((EQUAL X '(MINUS INFINITY)) 0))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (LIMIT (EXPT (PLUS (~ A) (EXPT (~ B) (~ X))) (QUOTIENT (~ C) X)) X
       INFINITY)
      (WHEN (EXPT B C) (AND (FREEOF B X) (FREEOF A X) (FREEOF C X)))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (LIMIT
       (EXPT (PLUS (~ A) (EXPT (~ B) (EXPT (~ X) (~ N))))
             (QUOTIENT (~ C) (EXPT X N)))
       X INFINITY)
      (WHEN (EXPT B C)
       (AND (FREEOF B X) (FREEOF A X) (FIXP N) (GREATERP N 1)
            (FREEOF C X)))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (LIMIT
       (QUOTIENT 1 (EXPT (PLUS (~ A) (EXPT (~ B) (~ X))) (QUOTIENT (~ C) X))) X
       INFINITY)
      (WHEN (EXPT B (MINUS C))
       (AND (FREEOF B X) (FREEOF A X) (FREEOF C X)))))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY
      (LIMIT
       (QUOTIENT 1
                 (EXPT (PLUS (~ A) (EXPT (~ B) (EXPT (~ X) (~ N))))
                       (QUOTIENT (~ C) (EXPT X N))))
       X INFINITY)
      (WHEN (EXPT B (MINUS C))
       (AND (FREEOF B X) (FREEOF A X) (FIXP N) (GREATERP N 1)
            (FREEOF C X)))))))) 
(ENDMODULE) 