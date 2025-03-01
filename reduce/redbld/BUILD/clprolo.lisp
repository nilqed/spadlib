(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID '(*SAVEDEF *GC-HOOK* *NOINLINES)) 
(GLOBAL '(*PSL *CSL)) 
(SETQ *PSL T) 
(PUT 'CREATE-PACKAGE 'NUMBER-OF-ARGS 2) 
(PUT 'CREATE-PACKAGE 'DEFINED-ON-LINE '41) 
(PUT 'CREATE-PACKAGE 'DEFINED-IN-FILE 'CLPROLO.RED) 
(PUT 'CREATE-PACKAGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CREATE-PACKAGE (U V)
    (COND ((NULL (IDP (CAR U))) (TYPERR (CAR U) "package name"))
          (T (PROGN (PUT (CAR U) 'PACKAGE U) (CAR U))))) 
(PUT 'EVLOAD 'NUMBER-OF-ARGS 1) 
(PUT 'EVLOAD 'DEFINED-ON-LINE '52) 
(PUT 'EVLOAD 'DEFINED-IN-FILE 'CLPROLO.RED) 
(PUT 'EVLOAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVLOAD (L)
    (PROG ()
     WHILELABEL
      (COND ((NOT L) (RETURN NIL)))
      (PROGN (APPLY (FUNCTION LOAD) (LIST (CAR L))) (SETQ L (CDR L)))
      (GO WHILELABEL))) 
(FLAG '(CONCAT) 'VARIADIC) 
(FLAG '(EQCAR) 'LOSE) 
(FLAG '(PRINC) 'LOSE) 
(FLAG
 '(FIRST SECOND THIRD REST LASTPAIR LASTCAR NTH PNTH REVERSIP EVENP ODDP
         SYMBOL-NAME)
 'LOSE) 
(FLAG
 '(LIST2WIDESTRING WIDESTRING2LIST STRING-STORE1 STRING-STORE2 STRING-STORE3
                   STRING-STORE4 MOAN-IF-NOT-FOLLOWER MOAN-IF-TRUNCATED)
 'LOSE) 
(FLAG
 '(IPLUS2 ITIMES2 ISUB1 IADD1 IMINUS IDIFFERENCE IQUOTIENT IREMAINDER IGREATERP
          ILESSP IMINUSP IEQUAL)
 'LOSE) 
(FLAG '(GCDN LCMN) 'LOSE) 
(FLAG '(GEQ LEQ) 'LOSE) 
(FLAG '(YESP1) 'LOSE) 
(FLAG '(RED-CHAR-DOWNCASE) 'LOSE) 
(FLAG '(ORDERP) 'LOSE) 