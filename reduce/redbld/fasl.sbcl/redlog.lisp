(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REDLOG)) 
(REVISION 'REDLOG "$Id: redlog.red 6174 2021-11-15 15:53:14Z thomas-sturm $") 
(COPYRIGHT 'REDLOG "(c) 1995-2021 A. Dolzmann, T. Sturm") 
(CREATE-PACKAGE '(REDLOG RLPRINT RLAMI RLTYPES RLSERVICES RLBLACKBOXES RLCONT)
                NIL) 
(EXPORTS
 (LIST 'QUOTELOG 'RL_MKBB 'RL_MKSERV 'RL_OP 'RL_ARG1 'RL_ARG2L 'RL_ARG2R
       'RL_ARGN 'RL_VAR 'RL_MAT 'RL_MK1 'RL_MK2 'RL_MKN 'RL_SMKN 'RL_MKQ
       'RL_QUAP 'RL_JUNCTP 'RL_BASBP 'RL_EXTBP 'RL_BOOLP 'RL_TVALP 'RL_CXP
       'RL_MK*FOF 'RL_REVAL 'RL_PREPFOF 'RL_CLEANUP 'RL_SIMP 'RL_SIMPBOP
       'RL_SIMPQ 'RL_SIMP*FOF 'RL_LENGTHLOGICAL 'RL_SUB*FOF 'RL_PRINT*FOF
       'RL_PRIQ 'RL_PPRIOP 'RL_FANCY-PPRIOP 'RL_FANCY-PRIQ 'RL_INTERF1
       'RL_A2S-DECDEG1 'RL_A2S-VARL 'RL_A2S-NUMBER 'RL_A2S-ID 'RL_A2S-ATL
       'RL_A2S-POSF 'RL_S2A-SIMPL 'RL_S2A-GQE 'RL_S2A-GQEA 'RL_S2A-QEA
       'RL_S2A-OPT 'RL_S2A-ATL 'RL_S2A-ML 'RL_S2A-TERM 'RL_S2A-DECDEG1
       'RL_A2S-TARGFN 'RL_A2S-TERML 'RL_S2A-TERML 'RL_A2S-TERM 'RL_S2A-VARL
       'RL_S2A-FBVARL 'RL_S2A-STRUCT 'RLMKOR 'RLMKAND 'RL_SET$ 'RL_SET 'RL_EXIT
       'RL_ENTER 'RL_ONP 'RL_VONOFF 'RL_UPDCACHE 'RL_SERVIADD 'RL_BBIADD)) 
(PUT 'REDLOG 'KNOWN-PACKAGES
     '(ACFSF CL DCFSF DVFSF IBALP MRI OFSF PASF QQE QQE_OFSF REDLOG RLSUPPORT
       RLTOOLS SMT TALP TPLP)) 
(FLAG '(FORMULA) 'ASSERT_DYNTYPE) 
(PUT 'FORMULAL 'ASSERT_DYNTYPECHK 'LISTP) 
(FLAG '(FORMULAL) 'ASSERT_DYNTYPE) 
(FLAG '(VOID) 'ASSERT_DYNTYPE) 
(SETQ RL_BUILTINS* (CONS '? (DELETE '? RL_BUILTINS*))) 
(PUT '? 'RL_SUPPORT 'RL_BUILTIN) 
(PUT '? 'RL_BUILTIN
     '((DOC
        (SYNOPSIS (1 . "?") (1 . "?builtins") (1 . "?services") (1 . "?types")
         (1 . "?switches") (1 . "?X"))
        (DESCRIPTION . "help") (RETURNS . "Void")
        (ARGUMENTS (1 "X" . "a specific service, type, or switch")) (SEEALSO)))) 
(FLUID '(RL_CID*)) 
(FLUID '(RL_SERVL*)) 
(FLUID '(RL_BBL*)) 
(FLUID '(RL_DEFLANG*)) 
(FLUID '(RL_ARGL*)) 
(FLUID '(RL_USEDCNAME*)) 
(FLUID '(RL_OCSWITCHES*)) 
(FLUID '(*UTF8)) 
(FLUID '(FANCY-LINE* FANCY-POS*)) 
(FLUID '(*STRICT_ARGCOUNT)) 
(FLUID '(XTIME*)) 
(COND ((NULL XTIME*) (SETQ XTIME* 0))) 
(SWITCH (LIST 'XTIME)) 
(ON1 'XTIME) 
(PUT 'RL_REGISTEREXTERNALTIME 'NUMBER-OF-ARGS 1) 
(DE RL_REGISTEREXTERNALTIME (MS)
    (PROGN
     (SETQ XTIME* (PLUS XTIME* MS))
     (COND
      (*XTIME
       (PROGN
        (SETQ OTIME* (DIFFERENCE OTIME* MS))
        (SETQ OTIME1* (DIFFERENCE OTIME1* MS))
        (SETQ OTIME2* (DIFFERENCE OTIME2* MS))
        (SETQ OTIME3* (DIFFERENCE OTIME3* MS)))))
     NIL)) 
(SWITCH (LIST 'RLABOUT)) 
(SWITCH (LIST 'RLBROP 'RLSIMPL 'RLREALTIME)) 
(ON1 'RLBROP) 
(OFF1 'RLSIMPL) 
(OFF1 'RLREALTIME) 
(SWITCH (LIST 'RLADDCOND 'RLNZDEN 'RLPOSDEN)) 
(OFF1 'RLADDCOND) 
(OFF1 'RLNZDEN) 
(OFF1 'RLPOSDEN) 
(SWITCH (LIST 'RLVERBOSE)) 
(OFF1 'RLVERBOSE) 
(SWITCH (LIST 'RLBRKCXK)) 
(PUT 'RLBRKCXK 'SIMPFG '((T (RMSUBS)) (NIL (RMSUBS)))) 
(OFF1 'RLBRKCXK) 
(SWITCH (LIST 'RLSISO 'RLSIPW 'RLSIPO 'RLSIFACO 'RLSIPLUGTHEO 'RLSID)) 
(ON1 'RLSID) 
(ON1 'RLSIPO) 
(ON1 'RLSIPLUGTHEO) 
(OFF1 'RLSIPW) 
(ON1 'RLSISO) 
(SWITCH (LIST 'RLSUSI 'RLSUSIADD 'RLSUSIGS 'RLSUSIMULT)) 
(OFF1 'RLSUSI) 
(OFF1 'RLSUSIMULT) 
(OFF1 'RLSUSIGS) 
(ON1 'RLSUSIADD) 
(SWITCH
 (LIST 'RLGSBNF 'RLGSERF 'RLGSPROD 'RLGSRAD 'RLGSRED 'RLGSSUB 'RLGSUTORD
       'RLGSVB)) 
(ON1 'RLGSSUB) 
(ON1 'RLGSRAD) 
(ON1 'RLGSRED) 
(ON1 'RLGSERF) 
(OFF1 'RLGSPROD) 
(ON1 'RLGSVB) 
(ON1 'RLGSBNF) 
(OFF1 'RLGSUTORD) 
(SWITCH
 (LIST 'RLANUEXGCDNORMALIZE 'RLANUEXSGNOPT 'RLANUEXVERBOSE 'RLCADDECDEG
       'RLCADDNFFORMULA 'RLCADEXTONLY 'RLCADFASTEVAL 'RLCADFULLDIMONLY
       'RLCADPBFVS 'RLCADPREPONLY 'RLCADPROJONLY 'RLCADRAWFORMULA 'RLCADTE
       'RLCADTRIMTREE 'RLCADVERBOSE)) 
(ON1 'RLCADDNFFORMULA) 
(OFF1 'RLCADPREPONLY) 
(OFF1 'RLCADPROJONLY) 
(OFF1 'RLCADEXTONLY) 
(OFF1 'RLCADVERBOSE) 
(ON1 'RLCADFASTEVAL) 
(OFF1 'RLCADFULLDIMONLY) 
(ON1 'RLCADTRIMTREE) 
(OFF1 'RLCADRAWFORMULA) 
(OFF1 'RLANUEXVERBOSE) 
(ON1 'RLANUEXGCDNORMALIZE) 
(OFF1 'RLANUEXSGNOPT) 
(OFF1 'RLCADDECDEG) 
(ON1 'RLCADTE) 
(ON1 'RLCADPBFVS) 
(SWITCH
 (LIST 'RLHQECONNECT 'RLHQEDIM0 'RLHQEGBDIMMIN 'RLHQEGBRED 'RLHQESTRCONST
       'RLHQETFCFAST 'RLHQETFCFULLSPLIT 'RLHQETFCSPLIT 'RLHQETHEORY
       'RLHQEVARSEL 'RLHQEVARSELX 'RLHQEVB)) 
(ON1 'RLHQETFCSPLIT) 
(OFF1 'RLHQETFCFAST) 
(OFF1 'RLHQETFCFULLSPLIT) 
(OFF1 'RLHQEVB) 
(ON1 'RLHQEVARSEL) 
(ON1 'RLHQEVARSELX) 
(ON1 'RLHQETHEORY) 
(OFF1 'RLHQEDIM0) 
(OFF1 'RLHQEGBRED) 
(OFF1 'RLHQECONNECT) 
(ON1 'RLHQESTRCONST) 
(ON1 'RLHQEGBDIMMIN) 
(SWITCH
 (LIST 'RLXOPT 'RLXOPTPL 'RLXOPTRI 'RLXOPTRIC 'RLXOPTRIR 'RLXOPTSB 'RLXOPTSES)) 
(ON1 'RLXOPT) 
(ON1 'RLXOPTSB) 
(ON1 'RLXOPTPL) 
(ON1 'RLXOPTRI) 
(OFF1 'RLXOPTRIC) 
(OFF1 'RLXOPTRIR) 
(ON1 'RLXOPTSES) 
(SWITCH (LIST 'RLOPT1S 'RLPARALLEL)) 
(OFF1 'RLOPT1S) 
(OFF1 'RLPARALLEL) 
(SWITCH (LIST 'RLVSLLEARN 'RLVSLLOG)) 
(ON1 'RLVSLLEARN) 
(OFF1 'RLVSLLOG) 
(SWITCH (LIST 'RLBNFSAC 'RLBNFSM)) 
(ON1 'RLBNFSAC) 
(OFF1 'RLBNFSM) 
(SWITCH (LIST 'RLRESI)) 
(ON1 'RLRESI) 
(SWITCH (LIST 'RLDAVGCD)) 
(ON1 'RLDAVGCD) 
(SWITCH (LIST 'RLOURDET 'RLVMATVB)) 
(OFF1 'RLOURDET) 
(OFF1 'RLVMATVB) 
(SWITCH (LIST 'RLENFFAC 'RLENFFACNE 'RLPLSIMPL)) 
(OFF1 'RLENFFAC) 
(ON1 'RLENFFACNE) 
(ON1 'RLPLSIMPL) 
(SWITCH (LIST 'RLTNFT)) 
(ON1 'RLTNFT) 
(PUT 'QUOTELOG 'NUMBER-OF-ARGS 1) 
(PUT 'QUOTELOG 'DEFINED-ON-LINE '273) 
(PUT 'QUOTELOG 'DEFINED-IN-FILE 'REDLOG/RL/REDLOG.RED) 
(PUT 'QUOTELOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUOTELOG (X) 'LOGICAL) 
(PUT 'LOGICAL 'TAG '*FOF) 
(PUT 'LOGICAL 'EVFN 'RL_REVAL) 
(PUT 'LOGICAL 'SUBFN 'RL_SUB*FOF) 
(PUT 'LOGICAL 'LENGTHFN 'RL_LENGTHLOGICAL) 
(SETQ RL_BUILTINS* (CONS 'TRUE (DELETE 'TRUE RL_BUILTINS*))) 
(PUT 'TRUE 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'TRUE 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "true")) (DESCRIPTION . "constant formula true")
        (RETURNS . "Formula") (ARGUMENTS) (SEEALSO FALSE)))) 
(PUT 'TRUE 'RTYPE 'LOGICAL) 
(SETQ RL_BUILTINS* (CONS 'FALSE (DELETE 'FALSE RL_BUILTINS*))) 
(PUT 'FALSE 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'FALSE 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "false")) (DESCRIPTION . "constant formula false")
        (RETURNS . "Formula") (ARGUMENTS) (SEEALSO TRUE)))) 
(PUT 'FALSE 'RTYPE 'LOGICAL) 
(PUT '*FOF 'RTYPEFN 'QUOTELOG) 
(PUT '*FOF 'RL_SIMPFN 'RL_SIMP*FOF) 
(SETQ RL_BUILTINS* (CONS 'AND (DELETE 'AND RL_BUILTINS*))) 
(PUT 'AND 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'AND 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "f_1:Formula and ... and f_n:Formula"))
        (DESCRIPTION . "Boolean conjunction") (RETURNS . "Formula")
        (ARGUMENTS (1 "f_1, ..., f_n" . "argument formulas"))
        (SEEALSO MKAND OR)))) 
(PUT 'AND 'RTYPEFN 'QUOTELOG) 
(PUT 'AND 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'AND 'RL_PREPFN 'RL_PREPBOP) 
(SETQ RL_BUILTINS* (CONS 'OR (DELETE 'OR RL_BUILTINS*))) 
(PUT 'OR 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'OR 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "f_1:Formula or ... or f_n:Formula"))
        (DESCRIPTION . "Boolean disjunction") (RETURNS . "Formula")
        (ARGUMENTS (1 "f_1, ..., f_n" . "argument formulas"))
        (SEEALSO AND IMPL MKOR)))) 
(PUT 'OR 'RTYPEFN 'QUOTELOG) 
(PUT 'OR 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'OR 'RL_PREPFN 'RL_PREPBOP) 
(SETQ RL_BUILTINS* (CONS 'NOT (DELETE 'NOT RL_BUILTINS*))) 
(PUT 'NOT 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'NOT 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "not f:Formula")) (DESCRIPTION . "Boolean negation")
        (RETURNS . "Formula") (ARGUMENTS (1 "f" . "argument formula"))
        (SEEALSO IMPL)))) 
(PUT 'NOT 'RTYPEFN 'QUOTELOG) 
(PUT 'NOT 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'NOT 'RL_PREPFN 'RL_PREPBOP) 
(SETQ RL_BUILTINS* (CONS 'IMPL (DELETE 'IMPL RL_BUILTINS*))) 
(PUT 'IMPL 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'IMPL 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "f_1:Formula impl f_2:Formula"))
        (DESCRIPTION . "Boolean implication") (RETURNS . "Formula")
        (ARGUMENTS (1 "f_1" . "argument formula")
         (2 "f_2" . "argument formula"))
        (SEEALSO EQUIV NOT OR REPL)))) 
(MKOP 'IMPL) 
(INFIX (LIST 'IMPL)) 
(PUT 'IMPL 'RTYPEFN 'QUOTELOG) 
(PUT 'IMPL 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'IMPL 'RL_PREPFN 'RL_PREPBOP) 
(PUT 'IMPL 'NUMBER-OF-ARGS 2) 
(SETQ RL_BUILTINS* (CONS 'REPL (DELETE 'REPL RL_BUILTINS*))) 
(PUT 'REPL 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'REPL 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "f_1:Formula repl f_2:Formula"))
        (DESCRIPTION . "Boolean reverse implication (aka replication)")
        (RETURNS . "Formula")
        (ARGUMENTS (1 "f_1" . "argument formula")
         (2 "f_2" . "argument formula"))
        (SEEALSO EQUIV IMPL)))) 
(MKOP 'REPL) 
(INFIX (LIST 'REPL)) 
(PUT 'REPL 'RTYPEFN 'QUOTELOG) 
(PUT 'REPL 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'REPL 'RL_PREPFN 'RL_PREPBOP) 
(PUT 'REPL 'NUMBER-OF-ARGS 2) 
(SETQ RL_BUILTINS* (CONS 'EQUIV (DELETE 'EQUIV RL_BUILTINS*))) 
(PUT 'EQUIV 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'EQUIV 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "f_1:Formula equiv f_2:Formula"))
        (DESCRIPTION . "Boolean biimplication (aka equivalence)")
        (RETURNS . "Formula")
        (ARGUMENTS (1 "f_1" . "argument formula")
         (2 "f_2" . "argument formula"))
        (SEEALSO IMPL REPL)))) 
(MKOP 'EQUIV) 
(INFIX (LIST 'EQUIV)) 
(PUT 'EQUIV 'RTYPEFN 'QUOTELOG) 
(PUT 'EQUIV 'RL_SIMPFN 'RL_SIMPBOP) 
(PUT 'EQUIV 'RL_PREPFN 'RL_PREPBOP) 
(PUT 'EQUIV 'NUMBER-OF-ARGS 2) 
(FLAG '(IMPL REPL EQUIV AND OR) 'SPACED) 
(PRECEDENCE (LIST 'EQUIV 'WHEN)) 
(PRECEDENCE (LIST 'REPL 'EQUIV)) 
(PRECEDENCE (LIST 'IMPL 'REPL)) 
(FLAG '(TRUE FALSE) 'RESERVED) 
(SETQ RL_BUILTINS* (CONS 'EX (DELETE 'EX RL_BUILTINS*))) 
(PUT 'EX 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'EX 'RL_BUILTIN
     '((DOC
        (SYNOPSIS (1 . "ex(x: Variable, formula: Formula)")
         (2 . "ex(X: List(Variable), formula: Formula)"))
        (DESCRIPTION . "existential quantifier") (RETURNS . "Formula")
        (ARGUMENTS (1 "x" . "variable to be existentially quantified")
         (2 "X" . "list of variables to be existentially quantified")
         (2 "formula"
          . "formula to be prefixed with the existential quantifier(s)"))
        (SEEALSO ALL RLEX)))) 
(PUT 'EX 'RTYPEFN 'QUOTELOG) 
(PUT 'EX 'RL_SIMPFN 'RL_SIMPQ) 
(PUT 'EX 'NUMBER-OF-ARGS 2) 
(PUT 'EX 'RL_PREPFN 'RL_PREPQ) 
(SETQ RL_BUILTINS* (CONS 'ALL (DELETE 'ALL RL_BUILTINS*))) 
(PUT 'ALL 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'ALL 'RL_BUILTIN
     '((DOC
        (SYNOPSIS (1 . "all(x: Variable, formula: Formula)")
         (2 . "all(X: List(Variable), formula: Formula)"))
        (DESCRIPTION . "universal quantifier") (RETURNS . "Formula")
        (ARGUMENTS (1 "x" . "variable to be universally quantified")
         (2 "X" . "list of variables to be universally quantified")
         (2 "formula"
          . "formula to be prefixed with the universal quantifier(s)"))
        (SEEALSO EX RLALL)))) 
(PUT 'ALL 'RTYPEFN 'QUOTELOG) 
(PUT 'ALL 'RL_SIMPFN 'RL_SIMPQ) 
(PUT 'ALL 'NUMBER-OF-ARGS 2) 
(PUT 'ALL 'RL_PREPFN 'RL_PREPQ) 
(SETQ RL_BUILTINS* (CONS 'BEX (DELETE 'BEX RL_BUILTINS*))) 
(PUT 'BEX 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'BEX 'RL_BUILTIN
     '((DOC
        (SYNOPSIS (1 . "bex(x: Variable, bound: Formula, formula: Formula)"))
        (DESCRIPTION . "bounded existential quantifier (integer domain only)")
        (RETURNS . "Formula")
        (ARGUMENTS (1 "x" . "variable to be existentially quantified")
         (2 "bound"
          . "quantifier-free formula that constrains x to a finite set")
         (2 "formula"
          . "formula to be prefixed with the bounded existential quantifier"))
        (SEEALSO BALL)))) 
(PUT 'BEX 'RTYPEFN 'QUOTELOG) 
(PUT 'BEX 'RL_SIMPFN 'RL_SIMPBQ) 
(PUT 'BEX 'NUMBER-OF-ARGS 3) 
(PUT 'BEX 'RL_PREPFN 'RL_PREPBQ) 
(SETQ RL_BUILTINS* (CONS 'BALL (DELETE 'BALL RL_BUILTINS*))) 
(PUT 'BALL 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'BALL 'RL_BUILTIN
     '((DOC
        (SYNOPSIS (1 . "ball(x: Variable, bound: Formula, formula: Formula)"))
        (DESCRIPTION . "bounded universal quantifier (integer domain only)")
        (RETURNS . "Formula")
        (ARGUMENTS (1 "x" . "variable to be universally quantified")
         (2 "bound"
          . "quantifier-free formula that constrains x to a finite set")
         (2 "formula"
          . "formula to be prefixed with the bounded universal quantifier"))
        (SEEALSO BEX)))) 
(PUT 'BALL 'RTYPEFN 'QUOTELOG) 
(PUT 'BALL 'RL_SIMPFN 'RL_SIMPBQ) 
(PUT 'BALL 'NUMBER-OF-ARGS 3) 
(PUT 'BALL 'RL_PREPFN 'RL_PREPBQ) 
(FLAG '(RL_SIMPBOP RL_SIMPQ RL_SIMPBQ RL_PREPBOP RL_PREPQ RL_PREPBQ) 'FULL) 
(SETQ RL_BUILTINS* (CONS 'RLABOUT (DELETE 'RLABOUT RL_BUILTINS*))) 
(PUT 'RLABOUT 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'RLABOUT 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "rlabout()"))
        (DESCRIPTION . "print revision information for REDLOG")
        (RETURNS . "Void") (ARGUMENTS) (SEEALSO)))) 
(PUT 'RLABOUT 'NUMBER-OF-ARGS 0) 
(FLAG '(RLABOUT) 'OPFN) 
(PUT 'RLABOUT 'DEFINED-ON-LINE '508) 
(PUT 'RLABOUT 'DEFINED-IN-FILE 'REDLOG/RL/REDLOG.RED) 
(PUT 'RLABOUT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RLABOUT NIL (RL_ABOUT)) 
(PUT 'RL_ABOUT 'NUMBER-OF-ARGS 0) 
(DE RL_ABOUT NIL
    (PROG (REV DATE TIME YEAR)
      (PROG (G133 G134)
        (SETQ G133 (RL_GETVERSION))
        (SETQ G134 G133)
        (SETQ REV (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ DATE (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ TIME (CAR G133))
        (SETQ G133 (CDR G133))
        (RETURN G134))
      (IOTO_TPRIN2T (LIST "Redlog Revision " REV " of " DATE ", " TIME))
      (SETQ YEAR (CAR (LTO_STRINGSPLIT DATE '(-))))
      (IOTO_TPRIN2 (LIST "(c) 1992-" YEAR " T. Sturm and A. Dolzmann"))
      (IOTO_PRIN2T " (www.redlog.eu)")
      (IOTO_TPRIN2T "type ?; for help"))) 
(PUT 'RL_GETVERSION 'NUMBER-OF-ARGS 0) 
(DE RL_GETVERSION NIL
    (PROG (W RES REV CUR)
      (SETQ REV 0)
      (SETQ CUR 0)
      (PROG (PACK)
        (SETQ PACK (GET 'REDLOG 'KNOWN-PACKAGES))
       LAB
        (COND ((NULL PACK) (RETURN NIL)))
        ((LAMBDA (PACK)
           (PROG (MOD)
             (SETQ MOD (GET PACK 'PACKAGE))
            LAB
             (COND ((NULL MOD) (RETURN NIL)))
             ((LAMBDA (MOD)
                (PROGN
                 (SETQ W
                         (LTO_STRINGSPLIT (GET MOD 'REVISION)
                                          (LIST '| | $EOL$ CR* FF* TAB*)))
                 (COND
                  ((EQUAL (LENGTH W) 7)
                   (PROGN
                    (SETQ REV (LTO_ID2INT (LTO_STRING2ID (NTH W 3))))
                    (COND
                     ((GREATERP REV CUR)
                      (PROGN
                       (SETQ CUR REV)
                       (SETQ RES (LIST (NTH W 3) (NTH W 4) (NTH W 5))))))))
                  (T
                   (LPRIM
                    (LTO_SCONCAT
                     (LIST "revision missing on " (LTO_AT2STR PACK) "/"
                           (LTO_AT2STR MOD))))))))
              (CAR MOD))
             (SETQ MOD (CDR MOD))
             (GO LAB)))
         (CAR PACK))
        (SETQ PACK (CDR PACK))
        (GO LAB))
      (RETURN RES))) 
(DE RL_OP (F) (COND ((ATOM F) F) (T (CAR F)))) 
(PUT 'RL_OP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_OP 'INLINE '(LAMBDA (F) (COND ((ATOM F) F) (T (CAR F))))) 
(DE RL_ARG1 (F) (CADR F)) 
(PUT 'RL_ARG1 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_ARG1 'INLINE '(LAMBDA (F) (CADR F))) 
(DE RL_ARG2L (F) (CADR F)) 
(PUT 'RL_ARG2L 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_ARG2L 'INLINE '(LAMBDA (F) (CADR F))) 
(DE RL_ARG2R (F) (CADDR F)) 
(PUT 'RL_ARG2R 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_ARG2R 'INLINE '(LAMBDA (F) (CADDR F))) 
(DE RL_ARGN (F) (CDR F)) 
(PUT 'RL_ARGN 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_ARGN 'INLINE '(LAMBDA (F) (CDR F))) 
(DE RL_VAR (F) (CADR F)) 
(PUT 'RL_VAR 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_VAR 'INLINE '(LAMBDA (F) (CADR F))) 
(DE RL_MAT (F) (CADDR F)) 
(PUT 'RL_MAT 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_MAT 'INLINE '(LAMBDA (F) (CADDR F))) 
(DE RL_B (F) (CADDDR F)) 
(PUT 'RL_B 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_B 'INLINE '(LAMBDA (F) (CADDDR F))) 
(DE RL_MK1 (UOP ARG) (LIST UOP ARG)) 
(PUT 'RL_MK1 'NUMBER-OF-ARGS 2) 
(PUTC 'RL_MK1 'INLINE '(LAMBDA (UOP ARG) (LIST UOP ARG))) 
(DE RL_MK2 (BOP LARG RARG) (LIST BOP LARG RARG)) 
(PUT 'RL_MK2 'NUMBER-OF-ARGS 3) 
(PUTC 'RL_MK2 'INLINE '(LAMBDA (BOP LARG RARG) (LIST BOP LARG RARG))) 
(DE RL_MKN (NOP ARGL) (CONS NOP ARGL)) 
(PUT 'RL_MKN 'NUMBER-OF-ARGS 2) 
(PUTC 'RL_MKN 'INLINE '(LAMBDA (NOP ARGL) (CONS NOP ARGL))) 
(DE RL_SMKN (NOP ARGL)
    (COND ((AND ARGL (CDR ARGL)) (CONS NOP ARGL))
          ((NULL ARGL) (COND ((EQ NOP 'AND) 'TRUE) (T 'FALSE))) (T (CAR ARGL)))) 
(PUT 'RL_SMKN 'NUMBER-OF-ARGS 2) 
(PUTC 'RL_SMKN 'INLINE
      '(LAMBDA (NOP ARGL)
         (COND ((AND ARGL (CDR ARGL)) (CONS NOP ARGL))
               ((NULL ARGL) (COND ((EQ NOP 'AND) 'TRUE) (T 'FALSE)))
               (T (CAR ARGL))))) 
(DE RL_MKQ (Q V M) (LIST Q V M)) 
(PUT 'RL_MKQ 'NUMBER-OF-ARGS 3) 
(PUTC 'RL_MKQ 'INLINE '(LAMBDA (Q V M) (LIST Q V M))) 
(DE RL_MKBQ (Q V B M) (LIST Q V M B)) 
(PUT 'RL_MKBQ 'NUMBER-OF-ARGS 4) 
(PUTC 'RL_MKBQ 'INLINE '(LAMBDA (Q V B M) (LIST Q V M B))) 
(DE RL_QUAP (X) (OR (EQ X 'EX) (EQ X 'ALL))) 
(PUT 'RL_QUAP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_QUAP 'INLINE '(LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))) 
(DE RL_BQUAP (X) (OR (EQ X 'BEX) (EQ X 'BALL))) 
(PUT 'RL_BQUAP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_BQUAP 'INLINE '(LAMBDA (X) (OR (EQ X 'BEX) (EQ X 'BALL)))) 
(DE RL_JUNCTP (X) (OR (EQ X 'OR) (EQ X 'AND))) 
(PUT 'RL_JUNCTP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_JUNCTP 'INLINE '(LAMBDA (X) (OR (EQ X 'OR) (EQ X 'AND)))) 
(DE RL_BASBP (X) (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))) 
(PUT 'RL_BASBP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_BASBP 'INLINE
      '(LAMBDA (X) (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT)))) 
(DE RL_EXTBP (X) (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV))) 
(PUT 'RL_EXTBP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_EXTBP 'INLINE
      '(LAMBDA (X) (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV)))) 
(DE RL_BOOLP (X)
    (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
        (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV)))) 
(PUT 'RL_BOOLP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_BOOLP 'INLINE
      '(LAMBDA (X)
         (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
             (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV))))) 
(DE RL_TVALP (X) (OR (EQ X 'TRUE) (EQ X 'FALSE))) 
(PUT 'RL_TVALP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_TVALP 'INLINE '(LAMBDA (X) (OR (EQ X 'TRUE) (EQ X 'FALSE)))) 
(DE RL_CXP (X)
    (OR (OR (EQ X 'TRUE) (EQ X 'FALSE))
        (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
            (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV)))
        (OR (EQ X 'EX) (EQ X 'ALL)) (OR (EQ X 'BEX) (EQ X 'BALL)))) 
(PUT 'RL_CXP 'NUMBER-OF-ARGS 1) 
(PUTC 'RL_CXP 'INLINE
      '(LAMBDA (X)
         (OR (OR (EQ X 'TRUE) (EQ X 'FALSE))
             (OR (OR (OR (EQ X 'OR) (EQ X 'AND)) (EQ X 'NOT))
                 (OR (EQ X 'IMPL) (EQ X 'REPL) (EQ X 'EQUIV)))
             (OR (EQ X 'EX) (EQ X 'ALL)) (OR (EQ X 'BEX) (EQ X 'BALL))))) 
(ENDMODULE) 