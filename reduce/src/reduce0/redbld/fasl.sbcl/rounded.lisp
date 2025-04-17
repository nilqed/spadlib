(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ROUNDED)) 
(EXPORTS
 (LIST 'CHKINT* 'CHKRN* 'CONVPREC 'CONVPREC* 'DEG2RAD* 'I2RD* 'LOGFP 'MKROUND
       '|RD:DIFFERENCE| '|RD:MINUS| '|RD:MINUSP| '|RD:ONEP| '|RD:PLUS|
       '|RD:PREP| '|RD:PRIN| '|RD:QUOTIENT| '|RD:SIMP| '|RD:TIMES| '|RD:ZEROP|
       'RDPREP1 'RDQOTERR 'RDZCHK 'RNDBFON 'ROUND* 'ROUNDBFOFF 'ROUNDBFON
       'ROUNDCONSTANTS 'SAFE-FP-PLUS 'SAFE-FP-TIMES 'SAFE-FP-QUOT)) 
(IMPORTS
 (LIST '*D2Q '|:DIFFERENCE| '|:MINUS| '|:MINUSP| '|:ZEROP| '|ABS:| 'AEVAL
       'APPLY1 'BF2FLR 'BFDIFFER 'BFEXPLODE0 'BFINVERSE 'BFLESSP 'BFLOAT
       'BFMINUS 'BFMINUSP '|BFPRIN:| '|BFTRIM:| '|BFZEROP:| 'BFZP 'CEILING
       'COPYD 'DEG2RAD* 'DIFBF 'DIVBF 'DMODERR '|EP:| 'EQCAR '|EQUAL:| 'ERRORP
       'ERRORSET* 'FL2INT 'FL2RD 'FLOAT-BFP 'FLOOR 'FT2RN1 'GEQ '|GREATERP:|
       'GRPBF '|I2BF:| 'INITDMODE 'INVBF 'LEQ '|LESSP:| 'LOG 'LPRIM 'LSHIFT
       '|MAKE:IBF| '|MAKE:RD| '|MINUS:| '|MINUSP:| 'MKQUOTE 'MSGPRI '|MT:| 'NEQ
       'NORMBF 'OFF1 'ON1 'OVER 'PLUBF '|PRECI:| 'R2BF 'RD2FL '|RD:FORCEBF|
       'REALRAT 'RERROR 'RETAG 'RMSUBS '|ROUND:MT| 'SETK 'SQRT 'TIMBF '|TIMES:|
       'UNION)) 
(FLUID '(|:PREC:| |:BPREC:| |:PRINT-PREC:| |MINPREC#| |ROOTACC##|)) 
(FLUID '(DMODE* *BFSPACE *NUMVAL *ROUNDBF **ROUNDBF *NORNDBF)) 
(FLUID '(*NOCONVERT)) 
(GLOBAL '(BFONE* EPSQRT* !LOG2OF10 !LOG2)) 
(GLOBAL '(DOMAINLIST* !NFPD !NBFPD !FLPREC !RDPREC MXFLBF! MNFLBF!)) 
(GLOBAL
 '(!PLUMAX !PLUMIN !TIMMAX !TIMMIN !MAXFLBF !MINFLBF !FLEPS1 !FLEPS2 !FLINT
   !MAXBFLEXP !MAXARG)) 
(GLOBAL '(RD-TOLERANCE* CR-TOLERANCE* YY! BFZ* !SMLSIN)) 
(SWITCH (LIST 'ROUNDED)) 
(SETQ !FLPREC (DIFFERENCE !NFPD 3)) 
(SETQ !SMLSIN (EXPT 10.0 (MINUS (PLUS 2 !FLPREC)))) 
(PUT 'LOGFP 'NUMBER-OF-ARGS 1) 
(PUT 'LOGFP 'DEFINED-ON-LINE '83) 
(PUT 'LOGFP 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'LOGFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOGFP (X)
    ((LAMBDA (M)
       ((LAMBDA (P)
          (PLUS (LOG (QUOTIENT M (FLOAT (ASHIFT 1 P))))
                (TIMES (PLUS P (CDDR X)) !LOG2)))
        (DIFFERENCE (|MSD:| (ABS (CADR X))) 1)))
     (CADR X))) 
(PUT 'ROUNDCONSTANTS 'NUMBER-OF-ARGS 0) 
(PUT 'ROUNDCONSTANTS 'DEFINED-ON-LINE '88) 
(PUT 'ROUNDCONSTANTS 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'ROUNDCONSTANTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ROUNDCONSTANTS NIL
    (PROGN
     (SETQ !PLUMAX (EXPT 2.0 (DIFFERENCE !MAXBFLEXP 1)))
     (SETQ !MINFLBF (INVBF (SETQ !MAXFLBF (CONS '|:RD:| (CONS 1 !MAXBFLEXP)))))
     (SETQ !PLUMIN (QUOTIENT (EXPT 10.0 !FLPREC) !PLUMAX))
     (SETQ !TIMMIN (QUOTIENT 1 (SETQ !TIMMAX (SQRT !PLUMAX))))
     (SETQ !MAXARG (LOGFP !MAXFLBF)))) 
(SWITCH (LIST 'BFSPACE 'NUMVAL 'ROUNDBF)) 
(SETQ *BFSPACE NIL) 
(SETQ *NUMVAL T) 
(PUT 'ROUNDBF 'SIMPFG '((T (ROUNDBFON)) (NIL (ROUNDBFOFF)))) 
(PUT 'ROUNDBFON 'NUMBER-OF-ARGS 0) 
(PUT 'ROUNDBFON 'DEFINED-ON-LINE '103) 
(PUT 'ROUNDBFON 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'ROUNDBFON 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ROUNDBFON NIL (SETQ **ROUNDBF T)) 
(PUT 'ROUNDBFOFF 'NUMBER-OF-ARGS 0) 
(PUT 'ROUNDBFOFF 'DEFINED-ON-LINE '105) 
(PUT 'ROUNDBFOFF 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'ROUNDBFOFF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ROUNDBFOFF NIL (SETQ **ROUNDBF (GREATERP !RDPREC !FLPREC))) 
(SETQ DOMAINLIST* (UNION '(|:RD:|) DOMAINLIST*)) 
(PUT 'ROUNDED 'TAG '|:RD:|) 
(PUT '|:RD:| 'DNAME 'ROUNDED) 
(FLAG '(|:RD:|) 'FIELD) 
(PUT '|:RD:| 'I2D 'I2RD*) 
(PUT '|:RD:| 'MINUSP '|RD:MINUSP|) 
(PUT '|:RD:| 'PLUS '|RD:PLUS|) 
(PUT '|:RD:| 'TIMES '|RD:TIMES|) 
(PUT '|:RD:| 'DIFFERENCE '|RD:DIFFERENCE|) 
(PUT '|:RD:| 'QUOTIENT '|RD:QUOTIENT|) 
(PUT '|:RD:| 'ZEROP '|RD:ZEROP|) 
(PUT '|:RD:| 'ONEP '|RD:ONEP|) 
(PUT '|:RD:| 'PREPFN '|RD:PREP|) 
(PUT '|:RD:| 'PRIFN '|RD:PRIN|) 
(PUT '|:RD:| 'MINUS '|RD:MINUS|) 
(PUT '|:RD:| 'ROOTFN '|RD:ROOT|) 
(PUT '|:RD:| '|:RN:| '*RD2RN) 
(PUT '|:RN:| '|:RD:| '*RN2RD) 
(PUT 'ROUND* 'NUMBER-OF-ARGS 1) 
(PUT 'ROUND* 'DEFINED-ON-LINE '129) 
(PUT 'ROUND* 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'ROUND* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROUND* (X) (COND ((ATOM (CDR X)) (CDR X)) (T X))) 
(PUT 'MKROUND 'NUMBER-OF-ARGS 1) 
(PUT 'MKROUND 'DEFINED-ON-LINE '134) 
(PUT 'MKROUND 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'MKROUND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKROUND (U) (COND ((ATOM U) (CONS '|:RD:| U)) (T U))) 
(PUT 'PRINT-PRECISION 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT-PRECISION 'DEFINED-ON-LINE '140) 
(PUT 'PRINT-PRECISION 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRINT-PRECISION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT-PRECISION (N)
    (PROG (OLDPREC)
      (COND ((EQUAL N 0) (RETURN |:PRINT-PREC:|)))
      (COND
       ((LESSP N 0)
        (PROGN
         (SETQ OLDPREC |:PRINT-PREC:|)
         (SETQ |:PRINT-PREC:| NIL)
         (RETURN OLDPREC))))
      (COND
       ((GREATERP N |:PREC:|)
        (PROGN
         (MSGPRI NIL "attempt to set print!-precision greater than"
                 "precision ignored" NIL NIL)
         (RETURN NIL))))
      (SETQ OLDPREC |:PRINT-PREC:|)
      (SETQ |:PRINT-PREC:| N)
      (RETURN OLDPREC))) 
(PUT 'PRINT_PRECISION 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_PRECISION 'DEFINED-ON-LINE '158) 
(PUT 'PRINT_PRECISION 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRINT_PRECISION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_PRECISION (N) (PRINT-PRECISION N)) 
(PUT 'PRECISION0 'NUMBER-OF-ARGS 1) 
(PUT 'PRECISION0 'DEFINED-ON-LINE '162) 
(PUT 'PRECISION0 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRECISION0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRECISION0 (N)
    (COND
     ((MEMBER N '((NIL) NIL (RESET)))
      (PROGN (SETQ |ROOTACC##| NIL) (PRECISION !FLPREC)))
     ((OR (CDR N)
          (NOT
           (NUMBERP
            (SETQ N
                    (PREPSQ
                     (SIMP*
                      (REVAL1 (LIST 'FIX (PREPSQ (SIMP* (CAR N)))) NIL))))))
          (LESSP N 0))
      (RERROR 'ARITH 5 "positive numeric value or `RESET' required"))
     (T
      (PROGN
       (COND ((GREATERP N 0) (SETQ |ROOTACC##| (MAX N 6))))
       (PRECISION N))))) 
(PUT 'PRECISION 'PSOPFN 'PRECISION0) 
(PUT 'PRECISION 'NUMBER-OF-ARGS 1) 
(PUT 'PRECISION 'DEFINED-ON-LINE '174) 
(PUT 'PRECISION 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRECISION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRECISION (N)
    (PROGN
     (COND
      ((OR (NOT (NUMBERP N)) (LESSP N 0))
       (RERROR 'ARITH 6 "positive number required")))
     (PRECISION1 N T))) 
(SETQ !LOG2OF10 (QUOTIENT (LOG 10) (LOG 2))) 
(PUT 'DECPREC2INTERNAL 'NUMBER-OF-ARGS 1) 
(PUT 'DECPREC2INTERNAL 'DEFINED-ON-LINE '183) 
(PUT 'DECPREC2INTERNAL 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'DECPREC2INTERNAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DECPREC2INTERNAL (P) (PLUS (CEILING (TIMES P !LOG2OF10)) 3)) 
(PUT 'PRECISION1 'NUMBER-OF-ARGS 2) 
(PUT 'PRECISION1 'DEFINED-ON-LINE '189) 
(PUT 'PRECISION1 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRECISION1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRECISION1 (N BOOL)
    (PROG (OLDPREC)
      (COND ((EQUAL N 0) (RETURN !RDPREC)))
      (COND (BOOL (RMSUBS)))
      (SETQ OLDPREC !RDPREC)
      (SETQ |:PREC:|
              (PLUS (SETQ !RDPREC (COND (*ROUNDBF N) (T (MAX N |MINPREC#|))))
                    2))
      (COND
       ((AND |:PRINT-PREC:| (LESSP N (PLUS |:PRINT-PREC:| 2)))
        (SETQ |:PRINT-PREC:| NIL)))
      (SETQ |:BPREC:| (DECPREC2INTERNAL |:PREC:|))
      (SETQ EPSQRT* (CONS '|:RD:| (CONS 1 (MINUS (QUOTIENT |:BPREC:| 2)))))
      (SETQ RD-TOLERANCE* (CONS '|:RD:| (CONS 1 (DIFFERENCE 6 |:BPREC:|))))
      (SETQ CR-TOLERANCE*
              (CONS '|:RD:| (CONS 1 (TIMES 2 (DIFFERENCE 6 |:BPREC:|)))))
      (SETQ **ROUNDBF (OR (GREATERP !RDPREC !FLPREC) *ROUNDBF))
      (RETURN OLDPREC))) 
(FLAG '(PRINT-PRECISION) 'OPFN) 
(FLAG '(PRINT_PRECISION) 'OPFN) 
(PUT '*RD2RN 'NUMBER-OF-ARGS 1) 
(PUT '*RD2RN 'DEFINED-ON-LINE '211) 
(PUT '*RD2RN 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '*RD2RN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *RD2RN (X)
    (PROG (P R R1 R2 D1 D2 OV)
      (COND ((|RD:ZEROP| X) (RETURN (CONS '|:RN:| (CONS 0 1)))))
      (SETQ P (PRECISION 0))
      (SETQ R (RD2RN1 X))
      (SETQ R1 (CONS '|:RN:| R))
      (COND
       ((OR (LESSP (ABS (CAR R)) 10) (LESSP (CDR R) 10)
            (LESSP
             (TIMES 2
                    (MAX (LENGTH (EXPLODE (CDR R)))
                         (LENGTH (EXPLODE (ABS (CAR R))))))
             (PLUS P 1)))
        (GO RET)))
      (SETQ R2
              (CONS '|:RN:|
                    (REALRAT
                     (|BFTRIM:|
                      (COND ((ATOM (CDR X)) (FL2BF (CDR X))) (T X))))))
      (PRECISION (PLUS 2 P))
      (SETQ D1 (|:DIFFERENCE| X R1))
      (COND ((|:MINUSP| D1) (SETQ D1 (|:MINUS| D1))))
      (SETQ D2 (|:DIFFERENCE| X R2))
      (COND ((|:MINUSP| D2) (SETQ D2 (|:MINUS| D2))))
      (COND ((OR (|:ZEROP| D2) (|:MINUSP| (|:DIFFERENCE| D2 D1))) (SETQ OV T)))
      (PRECISION P)
     RET
      (RETURN (COND (OV R2) (T R1))))) 
(PUT 'RD2RN1 'NUMBER-OF-ARGS 1) 
(PUT 'RD2RN1 'DEFINED-ON-LINE '231) 
(PUT 'RD2RN1 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'RD2RN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RD2RN1 (N) (COND ((ATOM (CDR N)) (FT2RN1 (CDR N))) (T (BF2RN1 N)))) 
(PUT 'BF2RN1 'NUMBER-OF-ARGS 1) 
(PUT 'BF2RN1 'DEFINED-ON-LINE '234) 
(PUT 'BF2RN1 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'BF2RN1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BF2RN1 (N)
    (PROG (NEGP A P0 P1 Q0 Q1 W FLAGG NN R0 R1)
      (COND ((LESSP (CADR N) 0) (PROGN (SETQ NEGP T) (SETQ N (|MINUS:| N)))))
      (SETQ NN N)
     TOP
      (SETQ A
              ((LAMBDA (M D) (COND ((EQUAL D 0) M) (T (ASHIFT M D)))) (CADR N)
               (CDDR N)))
      (SETQ N (DIFBF N (NORMBF (CONS '|:RD:| (CONS A 0)))))
      (COND
       ((NOT FLAGG)
        (PROGN (SETQ FLAGG T) (SETQ P0 1) (SETQ P1 A) (SETQ Q0 0) (SETQ Q1 1)))
       (T
        (PROGN
         (SETQ W (PLUS P0 (TIMES A P1)))
         (SETQ P0 P1)
         (SETQ P1 W)
         (SETQ R0 R1)
         (SETQ W (PLUS Q0 (TIMES A Q1)))
         (SETQ Q0 Q1)
         (SETQ Q1 W))))
      (SETQ R1
              (|ABS:|
               (DIFBF NN
                      (NORMBF
                       (|DIVIDE:| (CONS '|:RD:| (CONS P1 0))
                                  (CONS '|:RD:| (CONS Q1 0)) |:BPREC:|)))))
      (COND
       ((OR (|BFZEROP:| N) (|BFZEROP:| R1))
        (RETURN (COND (NEGP (CONS (MINUS P1) Q1)) (T (CONS P1 Q1)))))
       ((AND R0 (NOT (|GREATERP:| R0 R1)))
        (RETURN (COND (NEGP (CONS (MINUS P0) Q0)) (T (CONS P0 Q0))))))
      (SETQ N (INVBF N))
      (GO TOP))) 
(PUT '*RN2RD 'NUMBER-OF-ARGS 1) 
(PUT '*RN2RD 'DEFINED-ON-LINE '257) 
(PUT '*RN2RD 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '*RN2RD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *RN2RD (U) (MKROUND (CHKRN* (R2BF (CDR U))))) 
(SETQ |MINPREC#| (MIN 6 (DIFFERENCE !FLPREC 2))) 
(PRECISION1 !FLPREC NIL) 
(SETQ !FLEPS1 (EXPT 2.0 (DIFFERENCE 6 |:BPREC:|))) 
(SETQ !FLEPS2 (EXPT !FLEPS1 2)) 
(PUT 'PRECMSG 'NUMBER-OF-ARGS 1) 
(PUT 'PRECMSG 'DEFINED-ON-LINE '273) 
(PUT 'PRECMSG 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'PRECMSG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRECMSG (PR)
    (COND
     ((GREATERP PR !RDPREC)
      (PROGN
       (MSGPRI NIL "precision increased to" PR NIL NIL)
       (PRECISION1 PR T))))) 
(PUT '|RD:SIMP| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:SIMP| 'DEFINED-ON-LINE '278) 
(PUT '|RD:SIMP| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:SIMP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:SIMP| (U)
    (COND ((AND (NULL (ATOM U)) (EQUAL (CAR U) 0)) (CONS NIL 1))
          ((OR (NULL DMODE*) (EQ DMODE* '|:GI:|))
           ((LAMBDA (X) (COND ((EQCAR X '|:RN:|) (CDR X)) (T (CONS X 1))))
            (*RD2RN (CONS '|:RD:| U))))
          ((MEMQ DMODE* '(|:RD:| |:CR:|)) (CONS (MKROUND (CONVPREC* U)) 1))
          (T
           ((LAMBDA (Y)
              (COND (Y (*D2Q (APPLY1 Y (CONS '|:RD:| U))))
                    (T (DMODERR '|:RD:| DMODE*))))
            (GET '|:RD:| DMODE*))))) 
(PUT '|:RD:| 'SIMPFN '|RD:SIMP|) 
(PUT 'RNDBFON 'NUMBER-OF-ARGS 0) 
(PUT 'RNDBFON 'DEFINED-ON-LINE '292) 
(PUT 'RNDBFON 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'RNDBFON 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RNDBFON NIL
    (COND
     ((NOT *NORNDBF)
      (PROGN
       (SETQ **ROUNDBF T)
       (COND
        ((LESSP |:PREC:| (PLUS !FLPREC 3))
         (PROGN
          (SETQ *ROUNDBF T)
          (LPRIM "ROUNDBF turned on to increase accuracy")))))))) 
(PUT 'I2RD* 'NUMBER-OF-ARGS 1) 
(PUT 'I2RD* 'DEFINED-ON-LINE '298) 
(PUT 'I2RD* 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'I2RD* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE I2RD* (U) (MKROUND (CHKINT* U))) 
(PUT 'CHKINT* 'NUMBER-OF-ARGS 1) 
(PUT 'CHKINT* 'DEFINED-ON-LINE '302) 
(PUT 'CHKINT* 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CHKINT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHKINT* (U)
    (COND
     (**ROUNDBF
      (COND ((FLOATP U) (FL2BF U))
            (T
             (NORMBF
              (COND ((NOT (ATOM U)) U) ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                    (T (|READ:NUM| U)))))))
     (T
      ((LAMBDA (X)
         (COND ((FLOATP U) U) ((LEQ (|MSD:| X) !MAXBFLEXP) (FLOAT U))
               (T
                (PROGN
                 (RNDBFON)
                 (COND ((FLOATP U) (FL2BF U))
                       (T
                        (NORMBF
                         (COND ((NOT (ATOM U)) U)
                               ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                               (T (|READ:NUM| U))))))))))
       (ABS U))))) 
(SETQ MNFLBF! (INVBF (SETQ MXFLBF! (CONS '|:RD:| (CONS 1 800))))) 
(PUT 'CHKRN* 'NUMBER-OF-ARGS 1) 
(PUT 'CHKRN* 'DEFINED-ON-LINE '310) 
(PUT 'CHKRN* 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CHKRN* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHKRN* (U) (COND (**ROUNDBF U) (T (BF2FLCK U)))) 
(PUT 'BF2FLCK 'NUMBER-OF-ARGS 1) 
(PUT 'BF2FLCK 'DEFINED-ON-LINE '313) 
(PUT 'BF2FLCK 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'BF2FLCK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BF2FLCK (U)
    (COND (**ROUNDBF U) ((EQUAL (CADR U) 0) 0.0)
          (T
           ((LAMBDA (R)
              (COND
               ((AND (NOT (GRPBF !MINFLBF R)) (NOT (GRPBF R !MAXFLBF)))
                (BF2FLR U))
               (T (PROGN (RNDBFON) U))))
            (|ABS:| U))))) 
(PUT 'CONVCHK 'NUMBER-OF-ARGS 1) 
(PUT 'CONVCHK 'DEFINED-ON-LINE '320) 
(PUT 'CONVCHK 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CONVCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVCHK (X)
    (COND
     (**ROUNDBF
      (COND
       ((ATOM X)
        (COND ((FLOATP X) (FL2BF X))
              (T
               (NORMBF
                (COND ((NOT (ATOM X)) X) ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                      (T (|READ:NUM| X)))))))
       (T X)))
     ((ATOM X) X) (T (BF2FLCK X)))) 
(PUT 'CONVPREC* 'NUMBER-OF-ARGS 1) 
(PUT 'CONVPREC* 'DEFINED-ON-LINE '324) 
(PUT 'CONVPREC* 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CONVPREC* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVPREC* (U) (CONVCHK (COND ((ATOM U) U) (T (CONS '|:RD:| U))))) 
(PUT 'CONVPREC 'NUMBER-OF-ARGS 1) 
(PUT 'CONVPREC 'DEFINED-ON-LINE '327) 
(PUT 'CONVPREC 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CONVPREC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVPREC (U) (CONVCHK (ROUND* U))) 
(PUT '|RD:MINUSP| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:MINUSP| 'DEFINED-ON-LINE '329) 
(PUT '|RD:MINUSP| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:MINUSP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:MINUSP| (U) (COND ((ATOM (CDR U)) (MINUSP (CDR U))) (T (|MINUSP:| U)))) 
(PUT 'CONVPRC2 'NUMBER-OF-ARGS 2) 
(PUT 'CONVPRC2 'DEFINED-ON-LINE '333) 
(PUT 'CONVPRC2 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'CONVPRC2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONVPRC2 (U V)
    (PROGN
     (SETQ U (CONVPREC U))
     (SETQ YY! (CONVPREC V))
     (COND
      (*ROUNDBF
       (PROGN
        (SETQ YY!
                ((LAMBDA (X)
                   (COND ((FLOATP X) (FL2BF X))
                         (T
                          (NORMBF
                           (COND ((NOT (ATOM X)) X)
                                 ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                 (T (|READ:NUM| X)))))))
                 YY!))
        (COND ((FLOATP U) (FL2BF U))
              (T
               (NORMBF
                (COND ((NOT (ATOM U)) U) ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                      (T (|READ:NUM| U))))))))
      (T U)))) 
(PUT 'RDZCHK 'NUMBER-OF-ARGS 3) 
(PUT 'RDZCHK 'DEFINED-ON-LINE '337) 
(PUT 'RDZCHK 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'RDZCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RDZCHK (U X Y)
    (COND
     ((ATOM U)
      (COND
       ((OR (EQUAL U 0.0) (AND (GREATERP X 0.0) (GREATERP Y 0.0))
            (AND (LESSP X 0.0) (LESSP Y 0.0)))
        U)
       ((LESSP (ABS U) (TIMES (ABS X) !FLEPS1)) 0.0) (T U)))
     ((OR (EQUAL (CADR U) 0) (AND (GREATERP (CADR X) 0) (GREATERP (CADR Y) 0))
          (AND (LESSP (CADR X) 0) (LESSP (CADR Y) 0)))
      U)
     ((|LESSP:| (|ABS:| U) (|TIMES:| (|ABS:| X) RD-TOLERANCE*)) BFZ*) (T U))) 
(PUT '|RD:PLUS| 'NUMBER-OF-ARGS 2) 
(PUT '|RD:PLUS| 'DEFINED-ON-LINE '346) 
(PUT '|RD:PLUS| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:PLUS| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RD:PLUS| (U V)
    ((LAMBDA (Z)
       (COND
        ((AND (NOT **ROUNDBF) (ATOM (CDR U)) (ATOM (CDR V))
              (SETQ Z (SAFE-FP-PLUS (CDR U) (CDR V))))
         (CONS '|:RD:| Z))
        (T
         (PROG (X Y)
           (SETQ X (CONVPRC2 U V))
           (SETQ Y YY!)
           (SETQ U
                   (COND ((NOT (ATOM X)) (PLUBF X Y))
                         (T
                          (PROGN
                           (SETQ Z
                                   (ERRORSET*
                                    (LIST 'PLUS2 (MKQUOTE X) (MKQUOTE Y)) NIL))
                           (COND
                            ((ERRORP Z)
                             (PROGN
                              (RNDBFON)
                              (PLUBF
                               (SETQ X
                                       (COND ((FLOATP X) (FL2BF X))
                                             (T
                                              (NORMBF
                                               (COND ((NOT (ATOM X)) X)
                                                     ((FIXP X)
                                                      (CONS '|:RD:|
                                                            (CONS X 0)))
                                                     (T (|READ:NUM| X)))))))
                               (SETQ Y
                                       (COND ((FLOATP Y) (FL2BF Y))
                                             (T
                                              (NORMBF
                                               (COND ((NOT (ATOM Y)) Y)
                                                     ((FIXP Y)
                                                      (CONS '|:RD:|
                                                            (CONS Y 0)))
                                                     (T (|READ:NUM| Y))))))))))
                            (T (CAR Z)))))))
           (RETURN (MKROUND (RDZCHK U X Y)))))))
     NIL)) 
(PUT '|RD:DIFFERENCE| 'NUMBER-OF-ARGS 2) 
(PUT '|RD:DIFFERENCE| 'DEFINED-ON-LINE '375) 
(PUT '|RD:DIFFERENCE| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:DIFFERENCE| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RD:DIFFERENCE| (U V)
    ((LAMBDA (Z)
       (COND
        ((AND (NOT **ROUNDBF) (ATOM (CDR U)) (ATOM (CDR V))
              (SETQ Z (SAFE-FP-PLUS (CDR U) (SAFE-FP-MINUS (CDR V)))))
         (CONS '|:RD:| Z))
        (T
         (PROG (X Y)
           (SETQ X (CONVPRC2 U V))
           (SETQ Y YY!)
           (SETQ U
                   (COND ((NOT (ATOM X)) (DIFBF X Y))
                         (T
                          (PROGN
                           (SETQ Z
                                   (ERRORSET*
                                    (LIST 'DIFFERENCE (MKQUOTE X) (MKQUOTE Y))
                                    NIL))
                           (COND
                            ((ERRORP Z)
                             (PROGN
                              (RNDBFON)
                              (DIFBF
                               (SETQ X
                                       (COND ((FLOATP X) (FL2BF X))
                                             (T
                                              (NORMBF
                                               (COND ((NOT (ATOM X)) X)
                                                     ((FIXP X)
                                                      (CONS '|:RD:|
                                                            (CONS X 0)))
                                                     (T (|READ:NUM| X)))))))
                               (SETQ Y
                                       (COND ((FLOATP Y) (FL2BF Y))
                                             (T
                                              (NORMBF
                                               (COND ((NOT (ATOM Y)) Y)
                                                     ((FIXP Y)
                                                      (CONS '|:RD:|
                                                            (CONS Y 0)))
                                                     (T (|READ:NUM| Y))))))))))
                            (T (CAR Z)))))))
           (RETURN
            (MKROUND
             (RDZCHK U X (COND ((ATOM Y) (MINUS Y)) (T (|MINUS:| Y))))))))))
     NIL)) 
(PUT '|RD:TIMES| 'NUMBER-OF-ARGS 2) 
(PUT '|RD:TIMES| 'DEFINED-ON-LINE '396) 
(PUT '|RD:TIMES| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:TIMES| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RD:TIMES| (U V)
    ((LAMBDA (Z)
       (COND
        ((AND (NOT **ROUNDBF) (ATOM (CDR U)) (ATOM (CDR V))
              (SETQ Z (SAFE-FP-TIMES (CDR U) (CDR V))))
         (CONS '|:RD:| Z))
        (T
         (PROG (X Y)
           (SETQ X (CONVPRC2 U V))
           (SETQ Y YY!)
           (RETURN
            (MKROUND
             (COND
              ((NOT (ATOM X)) (NORMBF (|ROUND:MT| (|TIMES:| X Y) |:BPREC:|)))
              (T
               (PROGN
                (SETQ Z (ERRORSET* (LIST 'TIMES2 (MKQUOTE X) (MKQUOTE Y)) NIL))
                (COND
                 ((ERRORP Z)
                  (PROGN
                   (RNDBFON)
                   (NORMBF
                    (|ROUND:MT|
                     (|TIMES:|
                      (COND ((FLOATP X) (FL2BF X))
                            (T
                             (NORMBF
                              (COND ((NOT (ATOM X)) X)
                                    ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                    (T (|READ:NUM| X))))))
                      (COND ((FLOATP Y) (FL2BF Y))
                            (T
                             (NORMBF
                              (COND ((NOT (ATOM Y)) Y)
                                    ((FIXP Y) (CONS '|:RD:| (CONS Y 0)))
                                    (T (|READ:NUM| Y)))))))
                     |:BPREC:|))))
                 (T (CAR Z))))))))))))
     NIL)) 
(PUT '|RD:QUOTIENT| 'NUMBER-OF-ARGS 2) 
(PUT '|RD:QUOTIENT| 'DEFINED-ON-LINE '412) 
(PUT '|RD:QUOTIENT| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:QUOTIENT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RD:QUOTIENT| (U V)
    (COND ((|:ZEROP| V) (RERROR 'ARITH 7 "division by zero"))
          (T
           ((LAMBDA (Z)
              (COND
               ((AND (NOT **ROUNDBF) (ATOM (CDR U)) (ATOM (CDR V))
                     (SETQ Z (SAFE-FP-QUOT (CDR U) (CDR V))))
                (CONS '|:RD:| Z))
               (T
                (PROG (X Y)
                  (SETQ X (CONVPRC2 U V))
                  (SETQ Y YY!)
                  (COND ((AND (ATOM X) (ZEROP Y)) (RDQOTERR)))
                  (RETURN
                   (MKROUND
                    (COND
                     ((NOT (ATOM X))
                      (COND ((EQUAL (CADR Y) 0) (RDQOTERR))
                            (T (NORMBF (|DIVIDE:| X Y |:BPREC:|)))))
                     (T
                      (PROGN
                       (SETQ Z
                               (ERRORSET*
                                (LIST 'QUOTIENT (MKQUOTE X) (MKQUOTE Y)) NIL))
                       (COND
                        ((ERRORP Z)
                         (PROGN
                          (RNDBFON)
                          (NORMBF
                           (|DIVIDE:|
                            (COND ((FLOATP X) (FL2BF X))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM X)) X)
                                          ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                          (T (|READ:NUM| X))))))
                            (COND ((FLOATP Y) (FL2BF Y))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM Y)) Y)
                                          ((FIXP Y) (CONS '|:RD:| (CONS Y 0)))
                                          (T (|READ:NUM| Y))))))
                            |:BPREC:|))))
                        (T (CAR Z))))))))))))
            NIL)))) 
(PUT 'RDQOTERR 'NUMBER-OF-ARGS 0) 
(PUT 'RDQOTERR 'DEFINED-ON-LINE '432) 
(PUT 'RDQOTERR 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'RDQOTERR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RDQOTERR NIL (ERROR 0 "zero divisor in quotient")) 
(GLOBAL '(*NONEGZEROMINUS *NONEGZEROTIMES)) 
(SETQ *NONEGZEROMINUS NIL) 
(SETQ *NONEGZEROTIMES T) 
(PUT 'SAFE-FP-MINUS 'NUMBER-OF-ARGS 1) 
(PUT 'SAFE-FP-MINUS 'DEFINED-ON-LINE '474) 
(PUT 'SAFE-FP-MINUS 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'SAFE-FP-MINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SAFE-FP-MINUS (U)
    (COND ((AND *NONEGZEROMINUS (EQUAL U 0.0)) 0.0) (T (MINUS U)))) 
(GLOBAL '(!TWO460 !TWO512 !TWO564 !TWO1023 !MINNORM !MINNEGNORM)) 
(REMPROP '!TWO460 'CONSTANT?) 
(REMPROP '!TWO512 'CONSTANT?) 
(REMPROP '!TWO564 'CONSTANT?) 
(REMPROP '!TWO1023 'CONSTANT?) 
(SETQ !TWO460 (EXPT 2.0 460)) 
(SETQ !TWO512 (EXPT 2.0 512)) 
(SETQ !TWO564 (EXPT 2.0 564)) 
(SETQ !TWO1023 (EXPT 2.0 1023)) 
(SETQ !MINNORM (EXPT 2.0 (MINUS 1022))) 
(SETQ !MINNEGNORM (MINUS !MINNORM)) 
(PUT '!TWO460 'CONSTANT? !TWO460) 
(PUT '!TWO512 'CONSTANT? !TWO512) 
(PUT '!TWO564 'CONSTANT? !TWO564) 
(PUT '!TWO1023 'CONSTANT? !TWO1023) 
(PUT '!MINNORM 'CONSTANT? !MINNORM) 
(PUT '!MINNEGNORM 'CONSTANT? !MINNEGNORM) 
(PUT 'SAFE-FP-PLUS 'NUMBER-OF-ARGS 2) 
(PUT 'SAFE-FP-PLUS 'DEFINED-ON-LINE '577) 
(PUT 'SAFE-FP-PLUS 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'SAFE-FP-PLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAFE-FP-PLUS (U V)
    (COND
     ((LESSP U 0.0)
      (COND
       ((LESSP V 0.0)
        (PROGN
         (COND ((LEQ (PLUS (TIMES 0.5 U) (TIMES 0.5 V)) (MINUS !TWO1023)) NIL)
               (T (PLUS U V)))))
       (T
        (PROG (R)
          (SETQ R (PLUS U V))
          (COND
           ((EQUAL R 0.0)
            (PROGN (COND (*NONEGZEROTIMES (RETURN 0.0)) (T (RETURN R))))))
          (COND
           ((AND (LESSP R !MINNORM) (GREATERP R !MINNEGNORM)) (RETURN NIL))
           ((EQUAL (DIFFERENCE U (TIMES R 0.001953125)) U) (RETURN 0.0))
           (T (RETURN R)))))))
     ((LESSP V 0.0)
      (PROG (R)
        (SETQ R (PLUS U V))
        (COND
         ((EQUAL R 0.0)
          (PROGN (COND (*NONEGZEROTIMES (RETURN 0.0)) (T (RETURN R))))))
        (COND ((AND (LESSP R !MINNORM) (GREATERP R !MINNEGNORM)) (RETURN NIL))
              ((EQUAL (DIFFERENCE U (TIMES R 0.001953125)) U) (RETURN 0.0))
              (T (RETURN R)))))
     (T
      (PROGN
       (COND ((GEQ (PLUS (TIMES 0.5 U) (TIMES 0.5 V)) !TWO1023) NIL)
             (T (PLUS U V))))))) 
(PUT 'SAFE-FP-TIMES 'NUMBER-OF-ARGS 2) 
(PUT 'SAFE-FP-TIMES 'DEFINED-ON-LINE '626) 
(PUT 'SAFE-FP-TIMES 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'SAFE-FP-TIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAFE-FP-TIMES (U V)
    (PROG (U1 V1)
      (COND
       ((OR (EQUAL U 0.0) (EQUAL V 0.0))
        (PROGN
         (COND (*NONEGZEROTIMES (RETURN 0.0)) (T (RETURN (TIMES U V)))))))
      (COND ((LESSP U 0.0) (SETQ U1 (MINUS U))) (T (SETQ U1 U)))
      (COND ((LESSP V 0.0) (SETQ V1 (MINUS V))) (T (SETQ V1 V)))
      (COND
       ((LESSP U1 !TWO512)
        (COND
         ((LESSP V1 !TWO512)
          (PROGN (COND ((LESSP (TIMES U1 V1) !MINNORM) (RETURN NIL)))))
         (T
          (PROGN
           (COND
            ((GEQ (TIMES U1 (QUOTIENT V1 !TWO512)) !TWO512) (RETURN NIL)))))))
       ((LESSP V1 !TWO512)
        (PROGN
         (COND ((GEQ (TIMES (QUOTIENT U1 !TWO512) V1) !TWO512) (RETURN NIL)))))
       (T (RETURN NIL)))
      (RETURN (TIMES U V)))) 
(PUT 'SAFE-FP-QUOT 'NUMBER-OF-ARGS 2) 
(PUT 'SAFE-FP-QUOT 'DEFINED-ON-LINE '661) 
(PUT 'SAFE-FP-QUOT 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'SAFE-FP-QUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAFE-FP-QUOT (U V)
    (PROG (U1 V1)
      (COND ((EQUAL V 0.0) (RETURN NIL)))
      (COND
       ((EQUAL U 0.0)
        (PROGN
         (COND (*NONEGZEROTIMES (RETURN 0.0)) (T (RETURN (QUOTIENT U V)))))))
      (COND ((LESSP U 0.0) (SETQ U1 (MINUS U))) (T (SETQ U1 U)))
      (COND ((LESSP V 0.0) (SETQ V1 (MINUS V))) (T (SETQ V1 V)))
      (COND
       ((LESSP U1 !TWO512)
        (COND
         ((GREATERP V1 (QUOTIENT 1.0 !TWO512))
          (PROGN (COND ((LESSP (QUOTIENT U1 V1) !MINNORM) (RETURN NIL)))))
         (T
          (PROGN
           (COND
            ((GEQ (QUOTIENT U1 (TIMES V1 !TWO564)) !TWO460) (RETURN NIL)))))))
       ((GREATERP V1 (QUOTIENT 1.0 !TWO512))
        (PROGN
         (COND
          ((GEQ (QUOTIENT (QUOTIENT U1 !TWO512) V1) !TWO512) (RETURN NIL)))))
       (T
        (PROGN
         (COND
          ((GEQ (QUOTIENT (QUOTIENT U1 !TWO512) (TIMES V1 !TWO564))
                (EXPT 2.0 (MINUS 52)))
           (RETURN NIL))))))
      (RETURN (QUOTIENT U V)))) 
(PUT '|RD:ZEROP| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:ZEROP| 'DEFINED-ON-LINE '695) 
(PUT '|RD:ZEROP| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:ZEROP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:ZEROP| (U)
    (COND ((ATOM (CDR U)) (ZEROP (CDR U))) (T (EQUAL (CADR U) 0)))) 
(PUT '|RD:MINUS| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:MINUS| 'DEFINED-ON-LINE '699) 
(PUT '|RD:MINUS| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:MINUS| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:MINUS| (U)
    (COND ((ATOM (CDR U)) (CONS '|:RD:| (SAFE-FP-MINUS (CDR U))))
          (T (|MINUS:| U)))) 
(PUT '|RD:ONEP| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:ONEP| 'DEFINED-ON-LINE '703) 
(PUT '|RD:ONEP| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:ONEP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:ONEP| (U)
    (COND ((ATOM (CDR U)) (LESSP (ABS (DIFFERENCE 1.0 (CDR U))) !FLEPS1))
          (T (|EQUAL:| BFONE* (|BFTRIM:| U))))) 
(PUT '|RD:ROOT| 'NUMBER-OF-ARGS 2) 
(PUT '|RD:ROOT| 'DEFINED-ON-LINE '709) 
(PUT '|RD:ROOT| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:ROOT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |RD:ROOT| (U N)
    (COND ((ATOM (CDR U)) (CONS '|:RD:| (EXPT (CDR U) (RECIP (FLOAT N)))))
          (T (|TEXPT:ANY| U (|QUOTIENT:| BFONE* (CONS '|:RD:| (CONS N 0))))))) 
(PUT '|RD:PREP| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:PREP| 'DEFINED-ON-LINE '719) 
(PUT '|RD:PREP| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:PREP| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:PREP| (U)
    (COND (*NOCONVERT (RDPREP1 U)) ((|RD:ONEP| U) 1)
          ((|RD:ONEP| (|RD:MINUS| U)) (MINUS 1)) (T (RDPREP1 U)))) 
(PUT 'RDPREP1 'NUMBER-OF-ARGS 1) 
(PUT 'RDPREP1 'DEFINED-ON-LINE '732) 
(PUT 'RDPREP1 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'RDPREP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RDPREP1 (U) (COND ((ATOM (CDR U)) U) (T (|ROUND:MT| U |:BPREC:|)))) 
(PUT '|RD:PRIN| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:PRIN| 'DEFINED-ON-LINE '736) 
(PUT '|RD:PRIN| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:PRIN| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:PRIN| (U)
    (COND
     ((AND (ATOM (CDR U)) (NOT (EQN (DIFFERENCE (CDR U) (CDR U)) 0.0)))
      (PRIN2* (CDR U)))
     (T (|BFPRIN:| (|BFTRIM:| (COND ((ATOM (CDR U)) (FL2BF (CDR U))) (T U))))))) 
(PUT '|RD:EXPLODE| 'NUMBER-OF-ARGS 1) 
(PUT '|RD:EXPLODE| 'DEFINED-ON-LINE '742) 
(PUT '|RD:EXPLODE| 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT '|RD:EXPLODE| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RD:EXPLODE| (U)
    (BFEXPLODE0 (|BFTRIM:| (COND ((ATOM (CDR U)) (FL2BF (CDR U))) (T U))))) 
(INITDMODE 'ROUNDED) 
(PUT 'EVALF 'PSOPFN 'EVALF0) 
(PUT 'EVALF0 'NUMBER-OF-ARGS 1) 
(PUT 'EVALF0 'DEFINED-ON-LINE '749) 
(PUT 'EVALF0 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'EVALF0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALF0 (U)
    (PROG (SP W)
      (COND ((CDR U) (SETQ SP (PRECISION0 (CDR U)))))
      (COND (*ROUNDED (SETQ W (REVAL1 (CAR U) NIL)))
            (T
             (PROGN
              (ON1 'ROUNDED)
              (SETQ W (REVAL1 (CAR U) NIL))
              (OFF1 'ROUNDED)
              NIL)))
      (COND
       ((CDR U)
        (PROGN
         (COND
          ((GREATERP (CADR U) SP)
           (PROGN
            (PRIN2 "*** required accuracy exceeds current precision (")
            (PRIN2 SP)
            (PRIN2T ")")
            (PRIN2T "*** printing with required accuracy ...")
            (MATHPRINT W)
            (PRIN2T "*** finished printing"))))
         (PRECISION0 (LIST SP)))))
      (RETURN W))) 
(PUT 'EVALNUM 'PSOPFN 'EVALNUM0) 
(PUT 'EVALNUM0 'NUMBER-OF-ARGS 1) 
(PUT 'EVALNUM0 'DEFINED-ON-LINE '779) 
(PUT 'EVALNUM0 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'EVALNUM0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALNUM0 (U)
    (PROG (SP W)
      (COND ((CDR U) (SETQ SP (PRECISION0 (CDR U)))))
      (COND (*ROUNDED (SETQ W (REVAL1 (CAR U) NIL)))
            (T
             (PROGN
              (ON1 'ROUNDED)
              (SETQ W (REVAL1 (CAR U) NIL))
              (OFF1 'ROUNDED)
              (SETQ W (REVAL1 W NIL)))))
      (COND ((CDR U) (PRECISION0 (LIST SP))))
      (RETURN W))) 
(PUT 'HEXFLOAT1 'NUMBER-OF-ARGS 1) 
(PUT 'HEXFLOAT1 'DEFINED-ON-LINE '800) 
(PUT 'HEXFLOAT1 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'HEXFLOAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HEXFLOAT1 (W1)
    (COND
     ((FLOATP W1)
      (PROG (W S X M1 M2 M3 M4 N)
        (COND ((NOT (NUMBERP W1)) (RETURN W1)) ((EQUAL W1 0.0) (RETURN "0.0")))
        (COND ((NOT (EQN W1 W1)) (RETURN "NaN"))
              ((EQUAL W1 (TIMES 0.5 W1))
               (PROGN
                (COND ((GREATERP W1 0.0) (RETURN "inf"))
                      ((LESSP W1 0.0) (RETURN "-inf")) (T (RETURN "NaN"))))))
        (COND ((LESSP W1 0.0) (PROGN (SETQ S T) (SETQ W1 (MINUS W1)))))
        (SETQ X 0)
        (SETQ N 0)
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (LESSP W1 0.5) (LESSP N 5000))) (RETURN NIL)))
          (PROGN
           (SETQ W1 (TIMES 2.0 W1))
           (SETQ X (DIFFERENCE X 1))
           (SETQ N (PLUS N 1)))
          (GO WHILELABEL))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND (GEQ W1 1.0) (LESSP N 5000))) (RETURN NIL)))
          (PROGN
           (SETQ W1 (QUOTIENT W1 2.0))
           (SETQ X (PLUS X 1))
           (SETQ N (PLUS N 1)))
          (GO WHILELABEL))
        (COND ((GEQ N 5000) (RETURN "hexfloat failed")))
        (SETQ W1 (TIMES W1 32.0))
        (SETQ M1 (FIX W1))
        (SETQ W1 (DIFFERENCE W1 (FLOAT M1)))
        (SETQ W1 (TIMES W1 65536.0))
        (SETQ M2 (FIX W1))
        (SETQ W1 (DIFFERENCE W1 (FLOAT M2)))
        (SETQ W1 (TIMES W1 65536.0))
        (SETQ M3 (FIX W1))
        (SETQ W1 (DIFFERENCE W1 (FLOAT M3)))
        (SETQ W1 (TIMES W1 65536.0))
        (SETQ M4 (FIX W1))
        (SETQ W1 (DIFFERENCE W1 (FLOAT M4)))
        (COND ((NOT (ZEROP W1)) (ERROR 1 "Floating point oddity in hexfloat")))
        (SETQ M1 (EXPLODEHEX M1))
        (SETQ M2 (CDR (EXPLODEHEX (PLUS M2 65536))))
        (SETQ M3 (CDR (EXPLODEHEX (PLUS M3 65536))))
        (SETQ M4 (CDR (EXPLODEHEX (PLUS M4 65536))))
        (SETQ W (CONS '|b| (EXPLODE X)))
        (SETQ W (APPEND M4 (CONS '_ W)))
        (SETQ W (APPEND M3 (CONS '_ W)))
        (SETQ W (APPEND M2 (CONS '_ W)))
        (SETQ W (APPEND M1 (CONS '_ W)))
        (COND (S (SETQ W (CONS '- W))))
        (RETURN (LIST2STRING W))))
     ((ATOM W1) W1) (T (CONS (HEXFLOAT1 (CAR W1)) (HEXFLOAT1 (CDR W1)))))) 
(PUT 'HEXFLOAT 'NUMBER-OF-ARGS 1) 
(PUT 'HEXFLOAT 'DEFINED-ON-LINE '849) 
(PUT 'HEXFLOAT 'DEFINED-IN-FILE 'ARITH/ROUNDED.RED) 
(PUT 'HEXFLOAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HEXFLOAT (U)
    (PROG (W W1)
      (COND ((NUMBERP U) (RETURN (HEXFLOAT1 U))) ((ATOM U) (RETURN U))
            (T
             (PROGN
              (SETQ W (REVAL1 (CAR U) NIL))
              (SETQ W1 (PREPSQ (SIMP W))))))
      (COND
       ((AND (EQCAR W1 '|:RD:|) (FLOATP (CDR W1)))
        (RETURN (HEXFLOAT1 (CDR W1))))
       (T (RETURN W))))) 
(PUT 'HEXFLOAT 'PSOPFN 'HEXFLOAT) 
(ENDMODULE) 