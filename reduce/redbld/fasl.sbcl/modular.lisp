(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MODULAR)) 
(GLOBAL '(DOMAINLIST*)) 
(FLUID '(*BALANCED_MOD *MODULAR *PRECISE CURRENT-MODULUS ALGLIST* DMODE*)) 
(SWITCH (LIST 'MODULAR 'BALANCED_MOD)) 
(SETQ DOMAINLIST* (UNION '(|:MOD:|) DOMAINLIST*)) 
(PUT 'MODULAR 'TAG '|:MOD:|) 
(PUT '|:MOD:| 'DNAME 'MODULAR) 
(FLAG '(|:MOD:|) 'FIELD) 
(FLAG '(|:MOD:|) 'CONVERT) 
(PUT '|:MOD:| 'I2D '*I2MOD) 
(PUT '|:MOD:| '|:FT:| 'MODCNV) 
(PUT '|:MOD:| '|:RN:| 'MODCNV) 
(PUT '|:MOD:| 'MINUSP '|MODMINUSP:|) 
(PUT '|:MOD:| 'PLUS '|MODPLUS:|) 
(PUT '|:MOD:| 'TIMES '|MODTIMES:|) 
(PUT '|:MOD:| 'DIFFERENCE '|MODDIFFERENCE:|) 
(PUT '|:MOD:| 'QUOTIENT '|MODQUOTIENT:|) 
(PUT '|:MOD:| 'DIVIDE '|MODDIVIDE:|) 
(PUT '|:MOD:| 'GCD '|MODGCD:|) 
(PUT '|:MOD:| 'ZEROP '|MODZEROP:|) 
(PUT '|:MOD:| 'ONEP '|MODONEP:|) 
(PUT '|:MOD:| 'FACTORFN '|FACTORMOD:|) 
(PUT '|:MOD:| 'SQFRFACTORFN '|FACTORMOD:|) 
(PUT '|:MOD:| 'EXPT '|EXPTMOD:|) 
(PUT '|:MOD:| 'PREPFN '|MODPREP:|) 
(PUT '|:MOD:| 'PRIFN '(LAMBDA (X) (PRIN2* (PREPF X)))) 
(PUT '|:MOD:| 'UNITSFN '|:MOD:UNITCONV|) 
(PUT '*MODULAR2F 'NUMBER-OF-ARGS 1) 
(PUT '*MODULAR2F 'DEFINED-ON-LINE '65) 
(PUT '*MODULAR2F 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '*MODULAR2F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *MODULAR2F (U)
    (COND ((EQUAL U 0) NIL)
          (*BALANCED_MOD
           (COND
            ((GREATERP (PLUS U U) CURRENT-MODULUS)
             (CONS '|:MOD:| (DIFFERENCE U CURRENT-MODULUS)))
            ((LEQ (PLUS U U) (MINUS CURRENT-MODULUS))
             (*MODULAR2F (PLUS U CURRENT-MODULUS)))
            (T (CONS '|:MOD:| U))))
          (T (CONS '|:MOD:| U)))) 
(PUT '|EXPTMOD:| 'NUMBER-OF-ARGS 2) 
(PUT '|EXPTMOD:| 'DEFINED-ON-LINE '78) 
(PUT '|EXPTMOD:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|EXPTMOD:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |EXPTMOD:| (U N) (*MODULAR2F (GENERAL-MODULAR-EXPT (CDR U) N))) 
(PUT '|:MOD:UNITCONV| 'NUMBER-OF-ARGS 2) 
(PUT '|:MOD:UNITCONV| 'DEFINED-ON-LINE '83) 
(PUT '|:MOD:UNITCONV| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|:MOD:UNITCONV| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:MOD:UNITCONV| (U V)
    (COND ((EQUAL V 1) U)
          (T
           ((LAMBDA (Y)
              ((LAMBDA (X)
                 (COND (X (CONS (MULTD X (CAR U)) (MULTD X (CDR U))))
                       (T (MOD-ERROR (LIST 'QUOTIENT 1 (CDR V))))))
               (*MODULAR2F (|:MOD:UNITS| CURRENT-MODULUS Y 0 1))))
            (COND ((OR (GREATERP (CDR V) 0) (NULL *BALANCED_MOD)) (CDR V))
                  (T (PLUS CURRENT-MODULUS (CDR V)))))))) 
(PUT '|:MOD:UNITS| 'NUMBER-OF-ARGS 4) 
(PUT '|:MOD:UNITS| 'DEFINED-ON-LINE '91) 
(PUT '|:MOD:UNITS| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|:MOD:UNITS| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE |:MOD:UNITS| (A B X Y)
    (COND ((EQUAL B 0) 0)
          ((EQUAL B 1) (COND ((LESSP Y 0) (PLUS Y CURRENT-MODULUS)) (T Y)))
          (T
           (PROG (W)
             (SETQ W (QUOTIENT A B))
             (RETURN
              (|:MOD:UNITS| B (DIFFERENCE A (TIMES B W)) Y
                            (DIFFERENCE X (TIMES Y W)))))))) 
(PUT '*I2MOD 'NUMBER-OF-ARGS 1) 
(PUT '*I2MOD 'DEFINED-ON-LINE '101) 
(PUT '*I2MOD 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '*I2MOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *I2MOD (U) (*MODULAR2F (GENERAL-MODULAR-NUMBER U))) 
(PUT 'MODCNV 'NUMBER-OF-ARGS 1) 
(PUT 'MODCNV 'DEFINED-ON-LINE '106) 
(PUT 'MODCNV 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'MODCNV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MODCNV (U)
    (RERROR 'POLY 13
            (LIST "Conversion between modular integers and"
                  (GET (CAR U) 'DNAME) "not defined"))) 
(PUT '|MODMINUSP:| 'NUMBER-OF-ARGS 1) 
(PUT '|MODMINUSP:| 'DEFINED-ON-LINE '110) 
(PUT '|MODMINUSP:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODMINUSP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |MODMINUSP:| (U)
    (COND (*BALANCED_MOD (GREATERP (TIMES 2 (CDR U)) CURRENT-MODULUS)) (T NIL))) 
(PUT '|MODPLUS:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODPLUS:| 'DEFINED-ON-LINE '113) 
(PUT '|MODPLUS:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODPLUS:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODPLUS:| (U V) (*MODULAR2F (GENERAL-MODULAR-PLUS (CDR U) (CDR V)))) 
(PUT '|MODTIMES:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODTIMES:| 'DEFINED-ON-LINE '116) 
(PUT '|MODTIMES:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODTIMES:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODTIMES:| (U V) (*MODULAR2F (GENERAL-MODULAR-TIMES (CDR U) (CDR V)))) 
(PUT '|MODDIFFERENCE:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODDIFFERENCE:| 'DEFINED-ON-LINE '119) 
(PUT '|MODDIFFERENCE:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODDIFFERENCE:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODDIFFERENCE:| (U V)
    (*MODULAR2F (GENERAL-MODULAR-DIFFERENCE (CDR U) (CDR V)))) 
(PUT '|MODDIVIDE:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODDIVIDE:| 'DEFINED-ON-LINE '122) 
(PUT '|MODDIVIDE:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODDIVIDE:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODDIVIDE:| (U V) (CONS (*I2MOD 0) U)) 
(PUT '|MODGCD:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODGCD:| 'DEFINED-ON-LINE '124) 
(PUT '|MODGCD:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODGCD:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODGCD:| (U V) (*I2MOD 1)) 
(PUT '|MODQUOTIENT:| 'NUMBER-OF-ARGS 2) 
(PUT '|MODQUOTIENT:| 'DEFINED-ON-LINE '126) 
(PUT '|MODQUOTIENT:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODQUOTIENT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |MODQUOTIENT:| (U V)
    (*MODULAR2F
     (GENERAL-MODULAR-TIMES (CDR U) (GENERAL-MODULAR-RECIPROCAL (CDR V))))) 
(PUT '|MODZEROP:| 'NUMBER-OF-ARGS 1) 
(PUT '|MODZEROP:| 'DEFINED-ON-LINE '130) 
(PUT '|MODZEROP:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODZEROP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |MODZEROP:| (U) (EQUAL (CDR U) 0)) 
(PUT '|MODONEP:| 'NUMBER-OF-ARGS 1) 
(PUT '|MODONEP:| 'DEFINED-ON-LINE '132) 
(PUT '|MODONEP:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODONEP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |MODONEP:| (U) (EQUAL (CDR U) 1)) 
(PUT '|FACTORMOD:| 'NUMBER-OF-ARGS 1) 
(PUT '|FACTORMOD:| 'DEFINED-ON-LINE '134) 
(PUT '|FACTORMOD:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|FACTORMOD:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |FACTORMOD:| (U)
    (PROG (ALGLIST* DMODE*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (RETURN (PFACTOR (*Q2F (RESIMP (CONS U 1))) CURRENT-MODULUS)))) 
(PUT '|MODPREP:| 'NUMBER-OF-ARGS 1) 
(PUT '|MODPREP:| 'DEFINED-ON-LINE '140) 
(PUT '|MODPREP:| 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT '|MODPREP:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |MODPREP:| (U) (CDR U)) 
(INITDMODE 'MODULAR) 
(PUT 'SETMOD 'NUMBER-OF-ARGS 1) 
(PUT 'SETMOD 'DEFINED-ON-LINE '149) 
(PUT 'SETMOD 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'SETMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SETMOD (U)
    (PROG (DMODE*)
      (COND ((NOT (ATOM U)) (SETQ U (CAR U))))
      (SETQ U (REVAL1 U T))
      (COND
       ((AND (FIXP U) (GREATERP U 0))
        (PROGN
         (COND ((PRIMEP U) (FLAG '(|:MOD:|) 'FIELD))
               (T (REMFLAG '(|:MOD:|) 'FIELD)))
         (RETURN (SET-GENERAL-MODULUS U))))
       ((OR (EQUAL U 0) (NULL U)) (RETURN CURRENT-MODULUS))
       (T (TYPERR U "modulus"))))) 
(PUT 'SETMOD 'PSOPFN 'SETMOD) 
(PUT 'EVALMOD 'NUMBER-OF-ARGS 1) 
(PUT 'EVALMOD 'DEFINED-ON-LINE '192) 
(PUT 'EVALMOD 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'EVALMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVALMOD (U)
    (PROG (DM CP M MM W *ROUNDED *MODULAR)
      (COND
       (*COMPLEX
        ((LAMBDA (*MSG)
           (PROGN (SETQ CP T) (SETDMODE 'COMPLEX NIL) (SETQ *COMPLEX NIL)))
         NIL)))
      (COND ((SETQ DM (GET DMODE* 'DNAME)) (SETDMODE DM NIL)))
      (SETQ W (AEVAL* (CAR U)))
      (SETQ M (IEVAL (CADR U)))
      (SETDMODE 'MODULAR T)
      (SETQ *MODULAR T)
      (SETQ MM (APPLY1 'SETMOD (LIST M)))
      (SETQ W (AEVAL* W))
      (APPLY1 'SETMOD (LIST MM))
      (COND
       ((NEQ DM 'MODULAR)
        (PROGN (SETDMODE 'MODULAR NIL) (COND (DM (SETDMODE DM T))))))
      (COND
       (CP
        ((LAMBDA (*MSG) (PROGN (SETDMODE 'COMPLEX T) (SETQ *COMPLEX T))) NIL)))
      (RETURN W))) 
(PUT '|:MOD:| 'DOMAINVALCHK 'MOD-DOMAINVALCHK) 
(PUT 'MOD-DOMAINVALCHK 'NUMBER-OF-ARGS 2) 
(PUT 'MOD-DOMAINVALCHK 'DEFINED-ON-LINE '216) 
(PUT 'MOD-DOMAINVALCHK 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'MOD-DOMAINVALCHK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD-DOMAINVALCHK (FN U)
    (PROG (W)
      (SETQ W
              (COND ((EQUAL FN 'EXPT) (MOD-EXPT-FRACT (CAR U) (CADR U)))
                    (T NIL)))
      (RETURN (COND ((EQUAL W 'FAILED) NIL) (T (CONS W 1)))))) 
(PUT 'MOD-EXPT-FRACT 'NUMBER-OF-ARGS 2) 
(PUT 'MOD-EXPT-FRACT 'DEFINED-ON-LINE '223) 
(PUT 'MOD-EXPT-FRACT 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'MOD-EXPT-FRACT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOD-EXPT-FRACT (U X)
    (PROG (N M W)
      (COND ((EQUAL (CDR U) 1) (SETQ U (CAR U))) (T (GO DONE)))
      (COND ((EQCAR U '|:MOD:|) T) ((FIXP U) (SETQ U (CONS '|:MOD:| U)))
            (T (GO DONE)))
      (COND ((EQUAL U '(|:MOD:| . 1)) (RETURN 1)))
      (SETQ N (CAR X))
      (SETQ M (CDR X))
      (COND ((OR (NOT (FIXP N)) (NOT (FIXP M))) (GO DONE)))
      (COND ((EQUAL M 1) (RETURN (|EXPTMOD:| U N))))
      (LOAD-PACKAGE 'MODSR)
      (SETQ W
              (MSOLVE
               (LIST (LIST 'EQUAL (LIST 'EXPT 'X M) (LIST 'EXPT (CDR U) N)))))
      (COND ((EQ W 'FAILED) (RETURN W)) (T (SETQ W (CDR W))))
      (COND ((NULL W) (MOD-ERROR (LIST 'EXPT U (LIST 'QUOTIENT N M)))))
      (COND
       ((OR (NULL (CDR W)) (NULL *PRECISE)) (RETURN (CADDR (CADR (CAR W))))))
      (RETURN
       (LIST
        (CONS (CONS (CAR (FKERN (LIST 'EXPT (CDR U) (LIST 'QUOTIENT N M)))) 1)
              1)))
     DONE
      (RETURN (COND ((OR (NULL W) (CDR W)) 'FAILED) (T (CADDR (CAR W))))))) 
(PUT 'MOD-ERROR 'NUMBER-OF-ARGS 1) 
(PUT 'MOD-ERROR 'DEFINED-ON-LINE '248) 
(PUT 'MOD-ERROR 'DEFINED-IN-FILE 'POLY/MODULAR.RED) 
(PUT 'MOD-ERROR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MOD-ERROR (U) (TYPERR U (LIST "expression mod" CURRENT-MODULUS))) 
(ENDMODULE) 