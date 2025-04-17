(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DRIVER)) 
(FLUID
 '(*ALGINT *BACKTRACE *COMBINEEXPT *COMBINELOGS *EXP *EXPANDLOGS *GCD *INTFLAG*
   *KEEPSQRTS *LIMITEDFACTORS *MCD *NOINTSUBST *NONCOMP *NOLNR *PARTIALINTDF
   *PRECISE *PRECISE_COMPLEX *PURERISCH *RATIONALIZE *STRUCTURE *TRDINT *TRINT
   *TRINTSUBST *UNCACHED *INSIDE-INT* *HOLD-INT* BASIC-LISTOFNEWSQRTS
   BASIC-LISTOFALLSQRTS GAUSSIANI INTVAR KORD* LISTOFNEWSQRTS LISTOFALLSQRTS
   LOGLIST POWLIS* SQRT-INTVAR SQRT-PLACES-ALIST SUBFG* VARLIST VARSTACK* XLOGS
   ZLIST)) 
(GLOBAL '(ERFG*)) 
(EXPORTS (LIST 'INTEGRATESQ 'SIMPINT 'SIMPINT1)) 
(IMPORTS
 (LIST 'ALGEBRAICCASE 'ALGFNPL 'FINDZVARS 'GETVARIABLES 'INTERR 'PRINTSQ
       'TRANSCENDENTALCASE 'VARSINLIST 'KERNP 'SIMPCAR 'PREPSQ 'MKSQ 'SIMP
       'OPMTCH 'FORMLNR 'MATHPRINT)) 
(SWITCH (LIST 'ALGINT 'NOINTSUBST 'NOLNR 'TRDINT 'TRINT 'TRINTSUBST)) 
(SWITCH (LIST 'HYPERBOLIC)) 
(SETQ *INSIDE-INT* (SETQ *HOLD-INT* NIL)) 
(PUT 'SIMPINT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINT 'DEFINED-ON-LINE '95) 
(PUT 'SIMPINT 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'SIMPINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINT (U)
    (COND (*HOLD-INT* (SIMPIDEN (CONS 'INT U)))
          ((OR (ATOM U) (NULL (CDR U))
               (AND (CDDR U) (OR (NULL (CDDDR U)) (CDDDDR U))))
           (RERROR 'INT 1 "Improper number of arguments to INT"))
          ((CDDR U) (SIMPDINT U))
          (T
           ((LAMBDA (*INSIDE-INT*)
              (PROGN
               (PROG (ANS DMOD EXPRESSION VARIABLE LOGLIST OLDVARSTACK
                      *INTFLAG* *PURERISCH CFLAG INTVAR LISTOFNEWSQRTS
                      LISTOFALLSQRTS SQRTFN SQRT-INTVAR SQRT-PLACES-ALIST
                      BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS COEFFT
                      VARCHANGE W *PRECISE *PRECISE_COMPLEX *COMBINEEXPT)
                 (SETQ *INTFLAG* T)
                 (SETQ VARIABLE (*A2K (CADR U)))
                 (COND
                  ((NOT
                    (OR (IDP VARIABLE)
                        (AND (PAIRP VARIABLE) (NUMLISTP (CDR VARIABLE)))))
                   (PROGN
                    (SETQ VARCHANGE (CONS VARIABLE (INTERN (GENSYM))))
                    (COND
                     (*TRINT
                      ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
                       (LIST "Integration kernel" VARIABLE
                             "replaced by simple variable" (CDR VARCHANGE)))))
                    (SETQ VARIABLE (CDR VARCHANGE)))))
                 (SETQ INTVAR VARIABLE)
                 (SETQ W (CDDR U))
                 (COND (W (RERROR 'INT 3 "Too many arguments to INT")))
                 (SETQ LISTOFNEWSQRTS (LIST (CAAAR GAUSSIANI)))
                 (SETQ LISTOFALLSQRTS
                         (LIST (CONS (CADR (CAAAR GAUSSIANI)) GAUSSIANI)))
                 (SETQ SQRTFN *INSIDE-INT*)
                 (SETQ *INSIDE-INT* T)
                 (COND
                  (DMODE*
                   ((LAMBDA (*MSG)
                      (PROGN
                       (COND
                        ((SETQ CFLAG (GET DMODE* 'CMPXFN))
                         (ONOFF 'COMPLEX NIL)))
                       (COND
                        ((SETQ DMOD (GET DMODE* 'DNAME)) (ONOFF DMOD NIL)))))
                    NIL)))
                 (PROG (DMODE* *EXP *GCD *KEEPSQRTS *LIMITEDFACTORS *MCD
                        *RATIONALIZE *STRUCTURE *UNCACHED KORD* ANS1 BADBIT
                        DENEXP ERFG NEXP ONETERM)
                   (SETQ *KEEPSQRTS (SETQ *LIMITEDFACTORS T))
                   (SETQ *EXP
                           (SETQ *GCD
                                   (SETQ *MCD
                                           (SETQ *STRUCTURE
                                                   (SETQ *UNCACHED T)))))
                   (SETQ DMODE* NIL)
                   (COND
                    (*ALGINT
                     (PROGN
                      (SETQ SQRT-INTVAR (*Q2F (SIMPSQRTI VARIABLE)))
                      (COND
                       ((OR (CDR SQRT-INTVAR) (NEQ (CDAR SQRT-INTVAR) 1)
                            (NEQ (CDAAR SQRT-INTVAR) 1))
                        (INTERR "Sqrt(x) not properly formed"))
                       (T (SETQ SQRT-INTVAR (CAAAR SQRT-INTVAR))))
                      (SETQ BASIC-LISTOFALLSQRTS LISTOFALLSQRTS)
                      (SETQ BASIC-LISTOFNEWSQRTS LISTOFNEWSQRTS)
                      (SQRTSAVE BASIC-LISTOFALLSQRTS BASIC-LISTOFNEWSQRTS
                       (LIST (CONS VARIABLE VARIABLE))))))
                   (SETQ COEFFT (CONS 1 1))
                   (SETQ EXPRESSION (INT-SIMP (CAR U)))
                   (COND
                    (VARCHANGE
                     (PROGN
                      (DEPEND1 (CAR VARCHANGE) (CDR VARCHANGE) T)
                      (SETQ EXPRESSION
                              (INT-SUBSQ EXPRESSION (LIST VARCHANGE))))))
                   (SETQ DENEXP (CONS 1 (CDR EXPRESSION)))
                   (SETQ NEXP (CAR EXPRESSION))
                   (PROG ()
                    WHILELABEL
                     (COND
                      ((NOT
                        (AND (NOT (ATOM NEXP)) (NULL (CDR NEXP))
                             (NOT (DEPENDS (CAAAR NEXP) VARIABLE))))
                       (RETURN NIL)))
                     (PROGN
                      (SETQ COEFFT
                              (MULTSQ COEFFT
                                      (CONS (CONS (CONS (CAAR NEXP) 1) NIL)
                                            1)))
                      (SETQ NEXP (CDAR NEXP)))
                     (GO WHILELABEL))
                   (SETQ ANS1 NIL)
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT NEXP) (RETURN NIL)))
                     (PROG (X ZV TMP)
                       (COND
                        ((ATOM NEXP)
                         (PROGN (SETQ X (CONS NEXP 1)) (SETQ NEXP NIL)))
                        (T
                         (PROGN
                          (SETQ X (CONS (LIST (CAR NEXP)) 1))
                          (SETQ NEXP (CDR NEXP)))))
                       (SETQ X (MULTSQ X DENEXP))
                       (SETQ ZV (ZVARS (GETVARIABLES X) ZV VARIABLE T))
                       (SETQ TMP ANS1)
                       (PROG ()
                        WHILELABEL
                         (COND ((NOT TMP) (RETURN NIL)))
                         (PROGN
                          (COND
                           ((EQUAL ZV (CAAR TMP))
                            (PROGN
                             (RPLACD (CAR TMP) (ADDSQ (CDAR TMP) X))
                             (SETQ TMP NIL)
                             (SETQ ZV NIL)))
                           (T (SETQ TMP (CDR TMP)))))
                         (GO WHILELABEL))
                       (COND (ZV (SETQ ANS1 (CONS (CONS ZV X) ANS1)))))
                     (GO WHILELABEL))
                   (COND ((EQUAL (LENGTH ANS1) 1) (SETQ ONETERM T)))
                   (SETQ NEXP ANS1)
                   (SETQ ANS (CONS NIL 1))
                   (SETQ BADBIT (CONS NIL 1))
                   (PROG ()
                    WHILELABEL
                     (COND ((NOT NEXP) (RETURN NIL)))
                     (PROGN
                      (SETQ U (CDAR NEXP))
                      (COND
                       (*TRDINT
                        (PROGN
                         (PRINC "Integrate")
                         (PRINTSQ U)
                         (PRINC "with Zvars ")
                         (PRINT (CAAR NEXP)))))
                      (SETQ ERFG ERFG*)
                      (SETQ ANS1
                              (ERRORSET*
                               (LIST 'INTEGRATESQ (MKQUOTE U)
                                     (MKQUOTE VARIABLE) (MKQUOTE LOGLIST)
                                     (MKQUOTE (CAAR NEXP)))
                               *BACKTRACE))
                      (SETQ ERFG* ERFG)
                      (SETQ NEXP (CDR NEXP))
                      (COND ((ERRORP ANS1) (SETQ BADBIT (ADDSQ BADBIT U)))
                            (T
                             (PROGN
                              (SETQ ANS (ADDSQ (CAAR ANS1) ANS))
                              (SETQ BADBIT (ADDSQ (CDAR ANS1) BADBIT))))))
                     (GO WHILELABEL))
                   (COND
                    (*TRDINT
                     (PROGN
                      (PRIN2 "Partial answer=")
                      (PRINTSQ ANS)
                      (PRIN2 "To do=")
                      (PRINTSQ BADBIT))))
                   (COND
                    ((NEQ BADBIT '(NIL . 1))
                     (PROGN
                      (SETKORDER NIL)
                      (SETQ BADBIT (REORDSQ BADBIT))
                      (SETQ ANS (REORDSQ ANS))
                      (SETQ COEFFT (REORDSQ COEFFT))
                      (COND
                       (*TRDINT
                        (PROGN (PRINC "Retrying...") (PRINTSQ BADBIT))))
                      (COND
                       ((AND ONETERM (EQUAL ANS '(NIL . 1))) (SETQ ANS1 NIL))
                       (T
                        (SETQ ANS1
                                (ERRORSET*
                                 (LIST 'INTEGRATESQ (MKQUOTE BADBIT)
                                       (MKQUOTE VARIABLE) (MKQUOTE LOGLIST)
                                       NIL)
                                 *BACKTRACE))))
                      (COND
                       ((OR (NULL ANS1) (ERRORP ANS1))
                        (SETQ ANS
                                (ADDSQ ANS
                                       (SIMPINT1
                                        (CONS BADBIT (CONS VARIABLE W))))))
                       (T
                        (PROGN
                         (SETQ ANS (ADDSQ ANS (CAAR ANS1)))
                         (COND
                          ((NOT (SMEMQ VARIABLE ANS)) (SETQ ANS (CONS NIL 1))))
                         (COND
                          ((NEQ (CDAR ANS1) '(NIL . 1))
                           (SETQ ANS
                                   (ADDSQ ANS
                                          (SIMPINT1
                                           (CONS (CDAR ANS1)
                                                 (CONS VARIABLE W))))))))))))))
                 (SETQ ANS (MULTSQ COEFFT ANS))
                 (COND
                  (*TRDINT
                   (PROGN
                    (PROGN
                     (PRIN2 "Resimp and all that")
                     (TERPRI)
                     "Resimp and all that")
                    (PRINTSQ ANS))))
                 (SETQ *INSIDE-INT* SQRTFN)
                 ((LAMBDA (*HOLD-INT*)
                    (PROGN
                     ((LAMBDA (*MSG)
                        (PROGN
                         (COND (DMOD (ONOFF DMOD T)))
                         (COND (CFLAG (ONOFF 'COMPLEX T)))))
                      NIL)
                     (SETQ OLDVARSTACK VARSTACK*)
                     (SETQ VARSTACK* NIL)
                     (SETQ ANS
                             (ERRORSET*
                              (LIST 'INT-RESUB (MKQUOTE ANS)
                                    (MKQUOTE VARCHANGE))
                              T))))
                  T)
                 (SETQ VARSTACK* OLDVARSTACK)
                 (RETURN (COND ((ERRORP ANS) (ERROR1)) (T (CAR ANS)))))))
            *INSIDE-INT*)))) 
(PUT 'INT-RESUB 'NUMBER-OF-ARGS 2) 
(PUT 'INT-RESUB 'DEFINED-ON-LINE '262) 
(PUT 'INT-RESUB 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'INT-RESUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INT-RESUB (X V)
    (COND
     (V
      (PROGN
       (SETQ X (INT-SUBSQ X (LIST (CONS (CDR V) (CAR V)))))
       (DEPEND1 (CAR V) (CDR V) NIL)
       (RESIMP X)))
     (T (RESIMP X)))) 
(PUT 'INT-SUBSQ 'NUMBER-OF-ARGS 2) 
(PUT 'INT-SUBSQ 'DEFINED-ON-LINE '269) 
(PUT 'INT-SUBSQ 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'INT-SUBSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INT-SUBSQ (X V)
    (PROG (SUBFUNCS SUBFG*)
      (SETQ SUBFUNCS (LIST (REMPROP 'DF 'SUBFUNC) (REMPROP 'INT 'SUBFUNC)))
      (SETQ X (SUBSQ X V))
      (PUT 'DF 'SUBFUNC (CAR SUBFUNCS))
      (PUT 'INT 'SUBFUNC (CADR SUBFUNCS))
      (RETURN X))) 
(PUT 'NUMLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'NUMLISTP 'DEFINED-ON-LINE '281) 
(PUT 'NUMLISTP 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'NUMLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUMLISTP (U) (OR (NULL U) (AND (NUMBERP (CAR U)) (NUMLISTP (CDR U))))) 
(PUT 'INT-SIMP 'NUMBER-OF-ARGS 1) 
(PUT 'INT-SIMP 'DEFINED-ON-LINE '298) 
(PUT 'INT-SIMP 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'INT-SIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT-SIMP (U) (SUBS2 (RESIMP (SIMP* U)))) 
(PUT 'INT 'SIMPFN 'SIMPINT) 
(PUT 'INTEGRATESQ 'NUMBER-OF-ARGS 4) 
(PUT 'INTEGRATESQ 'DEFINED-ON-LINE '305) 
(PUT 'INTEGRATESQ 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'INTEGRATESQ 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTEGRATESQ (INTEGRAND VAR XLOGS ZV)
    (PROG (VARLIST X ZLIST *NONCOMP INTVAR)
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Start of Integration; integrand is ")
          (TERPRI)
          "Start of Integration; integrand is ")
         (PRINTSQ INTEGRAND))))
      (SETQ INTVAR VAR)
      (SETQ *NONCOMP
              (OR (AND *NCMP (NONCOMFP1 (CAR INTEGRAND)))
                  (AND *NCMP (NONCOMFP1 (CDR INTEGRAND)))))
      (SETQ VARLIST (GETVARIABLES INTEGRAND))
      (SETQ VARLIST (VARSINLIST XLOGS VARLIST))
      (COND (ZV (SETQ ZLIST ZV))
            (T (SETQ ZLIST (ZVARS VARLIST ZLIST VAR NIL))))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Determination of the differential field descriptor")
          (TERPRI)
          "Determination of the differential field descriptor")
         (PROGN (PRIN2 "gives the functions:") (TERPRI) "gives the functions:")
         (PRINT ZLIST))))
      (SETQ X (LOOK_FOR_SUBSTITUTE INTEGRAND VAR ZLIST))
      (COND (X (RETURN X)))
      (COND
       ((AND *PURERISCH (NOT (ALLOWEDFNS ZLIST)))
        (RETURN (CONS (CONS NIL 1) INTEGRAND))))
      (SETQ VARLIST (SETDIFF VARLIST ZLIST))
      (COND
       ((AND *ALGINT (CDR ZLIST) (ALGFNPL ZLIST VAR))
        (RETURN (ALGEBRAICCASE INTEGRAND ZLIST VARLIST)))
       (T (RETURN (TRANSCENDENTALCASE INTEGRAND VAR XLOGS ZLIST VARLIST)))))) 
(PUT 'ZVARS 'NUMBER-OF-ARGS 4) 
(PUT 'ZVARS 'DEFINED-ON-LINE '338) 
(PUT 'ZVARS 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'ZVARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ZVARS (X ZV VARIABLE BOOL)
    (PROG (OLDZLIST N)
      (SETQ N 0)
      (SETQ ZV (FINDZVARS X (LIST VARIABLE) VARIABLE NIL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (NEQ OLDZLIST ZV) (LESSP N 5))) (RETURN NIL)))
        (PROGN
         (SETQ OLDZLIST ZV)
         (PROG (ZZ)
           (SETQ ZZ OLDZLIST)
          LAB
           (COND ((NULL ZZ) (RETURN NIL)))
           ((LAMBDA (ZZ)
              (SETQ ZV (FINDZVARS (PSEUDODIFF ZZ VARIABLE) ZV VARIABLE T)))
            (CAR ZZ))
           (SETQ ZZ (CDR ZZ))
           (GO LAB))
         (SETQ N (PLUS N 1)))
        (GO WHILELABEL))
      (COND (BOOL (SETQ ZV (SORT ZV (FUNCTION ORDP)))))
      (RETURN ZV))) 
(PUT 'PSEUDODIFF 'NUMBER-OF-ARGS 2) 
(PUT 'PSEUDODIFF 'DEFINED-ON-LINE '375) 
(PUT 'PSEUDODIFF 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'PSEUDODIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PSEUDODIFF (A VAR)
    (COND
     ((ATOM A)
      (COND ((DEPENDS A VAR) (LIST (PREPSQ (SIMPDF (LIST A VAR))))) (T NIL)))
     ((MEMQ (CAR A) '(ATAN EQUAL LOG PLUS QUOTIENT SQRT TIMES MINUS))
      (PROG (AA BB)
        (PROG (ZZ)
          (SETQ ZZ (CDR A))
         LAB
          (COND ((NULL ZZ) (RETURN NIL)))
          ((LAMBDA (ZZ)
             (PROGN (SETQ BB (PSEUDODIFF ZZ VAR)) (SETQ AA (UNION BB AA))))
           (CAR ZZ))
          (SETQ ZZ (CDR ZZ))
          (GO LAB))
        (RETURN AA)))
     ((EQ (CAR A) 'EXPT)
      (COND
       ((DEPENDS (CADR A) VAR)
        (COND
         ((DEPENDS (CADDR A) VAR)
          (CONS (PREPSQ (SIMP (LIST 'LOG (CADR A))))
                (CONS (CADR A)
                      (CONS (CADDR A)
                            (UNION (PSEUDODIFF (CADR A) VAR)
                                   (PSEUDODIFF (CADDR A) VAR))))))
         (T (CONS (CADR A) (PSEUDODIFF (CADR A) VAR)))))
       (T (CONS (CADDR A) (PSEUDODIFF (CADDR A) VAR)))))
     (T (LIST (PREPSQ (SIMPDF (LIST A VAR))))))) 
(PUT 'LOOK_FOR_SUBSTITUTE 'NUMBER-OF-ARGS 3) 
(PUT 'LOOK_FOR_SUBSTITUTE 'DEFINED-ON-LINE '397) 
(PUT 'LOOK_FOR_SUBSTITUTE 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_SUBSTITUTE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_SUBSTITUTE (INTEGRAND VAR ZLIST)
    (COND (*NOINTSUBST NIL)
          (T
           (PROG (RES ZZ)
             (COND ((ATOM ZLIST) (RETURN NIL)))
             (SETQ ZZ (CAR ZLIST))
             (COND
              ((PAIRP ZZ)
               (PROGN
                (COND
                 ((SETQ RES (LOOK_FOR_EXPONENTIAL INTEGRAND VAR ZLIST))
                  (RETURN RES))
                 ((SETQ RES (LOOK_FOR_RATIONAL INTEGRAND VAR ZZ)) (RETURN RES))
                 ((SETQ RES (LOOK_FOR_QUAD INTEGRAND VAR ZZ)) (RETURN RES))))))
             (RETURN (LOOK_FOR_SUBSTITUTE INTEGRAND VAR (CDR ZLIST))))))) 
(PUT 'LOOK_FOR_EXPONENTIAL 'NUMBER-OF-ARGS 3) 
(PUT 'LOOK_FOR_EXPONENTIAL 'DEFINED-ON-LINE '411) 
(PUT 'LOOK_FOR_EXPONENTIAL 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_EXPONENTIAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_EXPONENTIAL (INTEGRAND VAR ZZ)
    (COND ((GET VAR 'LOOK_FOR_RATIONAL) NIL)
          ((OR (NOT (EQCAR (SETQ ZZ (CAR ZZ)) 'EXPT)) (DEPENDS (CADR ZZ) VAR))
           NIL)
          (T
           (PROG (B C KLIS NEWVAR FLG RES N)
             (SETQ N 0)
             (SETQ ZZ (CADDR ZZ))
             (COND
              ((AND (EQCAR ZZ 'EXPT) (EQUAL (CADR ZZ) VAR) (FIXP (CADDR ZZ)))
               (PROGN (SETQ FLG T) (SETQ B 1) (SETQ N (CADDR ZZ))))
              ((AND (EQCAR ZZ 'QUOTIENT) (NOT (DEPENDS (CADR ZZ) VAR)))
               (PROGN
                (SETQ B (CADR ZZ))
                (SETQ ZZ (CADDR ZZ))
                (COND ((EQUAL ZZ VAR) (SETQ N (MINUS 1)))
                      ((AND (EQCAR ZZ 'EXPT) (EQUAL (CADR ZZ) VAR)
                            (FIXP (CADDR ZZ)))
                       (SETQ N (MINUS (CADDR ZZ))))
                      ((NOT (EQCAR ZZ 'TIMES)) (RETURN NIL))
                      (T
                       (PROGN
                        (PROG (FCTR)
                          (SETQ FCTR (CDR ZZ))
                         LAB
                          (COND ((NULL FCTR) (RETURN NIL)))
                          ((LAMBDA (FCTR)
                             (COND
                              ((NOT (DEPENDS FCTR VAR)) (SETQ C (CONS FCTR C)))
                              (T (SETQ KLIS (CONS FCTR KLIS)))))
                           (CAR FCTR))
                          (SETQ FCTR (CDR FCTR))
                          (GO LAB))
                        (COND ((CDR KLIS) (RETURN NIL)))
                        (SETQ ZZ (CAR KLIS))
                        (SETQ B (LIST 'QUOTIENT B (RETIMES C)))
                        (COND ((EQUAL ZZ VAR) (SETQ N (MINUS 1)))
                              ((AND (EQCAR ZZ 'EXPT) (EQUAL (CADR ZZ) VAR)
                                    (FIXP (CADDR ZZ)))
                               (SETQ N (MINUS (CADDR ZZ))))
                              (T (RETURN NIL))))))))
              (T (RETURN NIL)))
             (SETQ NEWVAR (INT-GENSYM1 'INTVAR))
             (PUT NEWVAR 'LOOK_FOR_EXPONENTIAL N)
             ((LAMBDA (S)
                (SETQ RES
                        (SUBST-AND-INT INTEGRAND VAR NEWVAR S
                         (RETIMES (LIST B (LIST 'EXPT VAR N)))
                         (SIMP (LIST 'QUOTIENT S (LIST 'TIMES N NEWVAR)))
                         (LIST 'NOBAD))))
              (LIST 'EXPT
                    (COND ((EQUAL B 1) NEWVAR) (T (LIST 'QUOTIENT NEWVAR B)))
                    (COND ((EQUAL N (MINUS 1)) (MINUS 1))
                          (T (LIST 'QUOTIENT 1 N)))))
             (REMPROP NEWVAR 'LOOK_FOR_EXPONENTIAL)
             (RETURN RES))))) 
(PUT 'LOOK_FOR_RATIONAL 'NUMBER-OF-ARGS 3) 
(PUT 'LOOK_FOR_RATIONAL 'DEFINED-ON-LINE '463) 
(PUT 'LOOK_FOR_RATIONAL 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_RATIONAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_RATIONAL (INTEGRAND VAR ZZ)
    (COND ((GET VAR 'LOOK_FOR_EXPONENTIAL) NIL)
          ((AND (EQUAL (CAR ZZ) 'SQRT) (EQUAL (CADR ZZ) VAR))
           (LOOK_FOR_RATIONAL1 INTEGRAND VAR 2))
          ((AND (EQUAL (CAR ZZ) 'EXPT) (EQUAL (CADR ZZ) VAR) (LISTP (CADDR ZZ))
                (EQUAL (CAADDR ZZ) 'QUOTIENT) (NUMBERP (CADR (CADDR ZZ)))
                (NUMBERP (CADDR (CADDR ZZ))))
           (LOOK_FOR_RATIONAL1 INTEGRAND VAR (CADDR (CADDR ZZ))))
          (T NIL))) 
(PUT 'LOOK_FOR_RATIONAL1 'NUMBER-OF-ARGS 3) 
(PUT 'LOOK_FOR_RATIONAL1 'DEFINED-ON-LINE '478) 
(PUT 'LOOK_FOR_RATIONAL1 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_RATIONAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_RATIONAL1 (INTEGRAND VAR M)
    (PROG (NEWVAR RES MN2M-1 SBST BCKSBST)
      (SETQ NEWVAR (INT-GENSYM1 'INTVAR))
      (SETQ MN2M-1 (CONS (CONS (CONS (CONS NEWVAR (DIFFERENCE M 1)) M) NIL) 1))
      (SETQ SBST (LIST 'EXPT NEWVAR M))
      (SETQ BCKSBST (LIST 'EXPT VAR (LIST 'QUOTIENT 1 M)))
      (PUT NEWVAR 'LOOK_FOR_RATIONAL M)
      (SETQ RES (SUBST-AND-INT INTEGRAND VAR NEWVAR SBST BCKSBST MN2M-1 NIL))
      (REMPROP NEWVAR 'LOOK_FOR_RATIONAL)
      (RETURN RES))) 
(PUT 'LOOK_FOR_QUAD 'NUMBER-OF-ARGS 3) 
(PUT 'LOOK_FOR_QUAD 'DEFINED-ON-LINE '528) 
(PUT 'LOOK_FOR_QUAD 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_QUAD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_QUAD (INTEGRAND VAR ZZ)
    (PROG ()
      (COND
       ((OR
         (AND (EQUAL (CAR ZZ) 'SQRT) (LISTP (CADR ZZ))
              (EQUAL (CAADR ZZ) 'PLUS))
         (AND (EQUAL (CAR ZZ) 'EXPT) (LISTP (CADR ZZ)) (EQUAL (CAADR ZZ) 'PLUS)
              (LISTP (CADDR ZZ)) (EQUAL (CAR (CADDR ZZ)) 'QUOTIENT)
              (FIXP (CADDR (CADDR ZZ)))))
        (PROGN
         (SETQ ZZ (SIMP (CADR ZZ)))
         (COND
          ((EQUAL (CDR ZZ) 1)
           (PROGN
            (SETQ ZZ (CDR (COEFF1 (PREPSQ ZZ) VAR NIL)))
            (COND
             ((EQUAL (LENGTH ZZ) 2)
              (RETURN
               (PROG (A B NVAR SBST BCKSBST FCTR)
                 (SETQ A (CAR ZZ))
                 (SETQ B (CADR ZZ))
                 (COND ((OR (DEPENDS A VAR) (DEPENDS B VAR)) (RETURN NIL)))
                 (SETQ NVAR (INT-GENSYM1 'INTVAR))
                 (COND
                  ((OR *TRINT *TRINTSUBST)
                   (PROGN
                    (PRIN2 "Linear shift suggested ")
                    (PRIN2 A)
                    (PRIN2 " ")
                    (PRIN2 B)
                    (TERPRI)
                    NIL)))
                 (SETQ SBST
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE (LIST 'EXPT NVAR 2) A) B))
                 (SETQ BCKSBST (LIST 'SQRT (LIST 'PLUS (LIST 'TIMES VAR B) A)))
                 (SETQ FCTR (SIMP (LIST 'QUOTIENT (LIST 'TIMES NVAR 2) B)))
                 (RETURN
                  (SUBST-AND-INT INTEGRAND VAR NVAR SBST BCKSBST FCTR
                   (LIST 'NOBAD))))))
             ((EQUAL (LENGTH ZZ) 3)
              (RETURN
               (PROG (A B C)
                 (SETQ A (CAR ZZ))
                 (SETQ B (CADR ZZ))
                 (SETQ C (CADDR ZZ))
                 (COND
                  ((OR (DEPENDS A VAR) (DEPENDS B VAR) (DEPENDS C VAR))
                   (RETURN NIL)))
                 (SETQ A
                         (SIMP*
                          (LIST 'DIFFERENCE A
                                (LIST 'TIMES B B
                                      (LIST 'QUOTIENT 1 (LIST 'TIMES 4 C))))))
                 (COND ((NULL (CAR A)) (RETURN NIL)))
                 (SETQ B (SIMP (LIST 'QUOTIENT B (LIST 'TIMES 2 C))))
                 (SETQ C (SIMP C))
                 (RETURN
                  (COND
                   ((MINUSF (CAR C))
                    (PROGN
                     (COND
                      ((MINUSF (CAR A))
                       (PROG (*HYPERBOLIC *PRECISE)
                         (SETQ *HYPERBOLIC T)
                         (SETQ *PRECISE T)
                         (RETURN (LOOK_FOR_INVHYP INTEGRAND NIL VAR A B C))))
                      (T (LOOK_FOR_ASIN INTEGRAND VAR A B C)))))
                   (T
                    (PROGN
                     (COND
                      ((MINUSF (CAR A))
                       (LOOK_FOR_INVHYP INTEGRAND T VAR A B C))
                      (T (LOOK_FOR_INVHYP INTEGRAND NIL VAR A B C))))))))))
             ((EQUAL (LENGTH ZZ) 5)
              (RETURN
               (PROG (A B C D E NN DD MM)
                 (SETQ A (CAR ZZ))
                 (SETQ B (CADR ZZ))
                 (SETQ C (CADDR ZZ))
                 (SETQ D (CADDDR ZZ))
                 (SETQ E (CAR (CDDDDR ZZ)))
                 (COND ((OR (NOT (EQUAL B 0)) (NOT (EQUAL D 0))) (RETURN NIL)))
                 (COND
                  ((OR (OR (DEPENDS A VAR) (DEPENDS C VAR)) (DEPENDS E VAR))
                   (RETURN NIL)))
                 (SETQ NN (CAR INTEGRAND))
                 (SETQ DD (CDR INTEGRAND))
                 (COND
                  ((AND
                    (EQUAL
                     (CDR
                      (SETQ MM
                              (MULTSQ (CONS NN 1)
                                      (INVSQ
                                       (CONS
                                        (LIST
                                         (CONS (GETPOWER (FKERN VAR) 1) 1))
                                        1)))))
                     1)
                    (EVEN_POWER (CAR MM) VAR) (EVEN_POWER DD VAR))
                   (PROGN (RETURN (SQRT_SUBSTITUTE (CAR MM) DD VAR)) NIL)))
                 (COND
                  ((AND
                    (EQUAL
                     (CDR
                      (SETQ MM
                              (MULTSQ (CONS DD 1)
                                      (INVSQ
                                       (CONS
                                        (LIST
                                         (CONS (GETPOWER (FKERN VAR) 1) 1))
                                        1)))))
                     1)
                    (EVEN_POWER NN VAR) (EVEN_POWER (CAR MM) VAR))
                   (PROGN
                    (RETURN
                     (SQRT_SUBSTITUTE NN
                      ((LAMBDA (G552)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF DD G552))
                               (T (POLY-MULTF DD G552))))
                       (LIST (CONS (GETPOWER (FKERN VAR) 1) 1)))
                      VAR))
                    NIL)))
                 (RETURN NIL)))))
            NIL))))))
      (RETURN NIL))) 
(PUT 'LOOK_FOR_ASIN 'NUMBER-OF-ARGS 5) 
(PUT 'LOOK_FOR_ASIN 'DEFINED-ON-LINE '649) 
(PUT 'LOOK_FOR_ASIN 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_ASIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_ASIN (INTEGRAND VAR A B C)
    (PROG (NEWVAR RES SS SQMN ONEMTH FCTR SBST BCKSHFT M N X)
      (SETQ M (PREPSQ A))
      (SETQ N (PREPSQ C))
      (SETQ B (PREPSQ B))
      (SETQ NEWVAR (INT-GENSYM1 'INTVAR))
      (SETQ SQMN
              (PREPSQ
               (APPLY1 (GET 'SQRT 'SIMPFN)
                       (LIST (LIST 'QUOTIENT (LIST 'MINUS N) M)))))
      (SETQ ONEMTH (LIST 'COS NEWVAR))
      (SETQ SS (LIST 'SIN NEWVAR))
      (SETQ POWLIS*
              (CONS
               (LIST SS 2 '(NIL . T) (LIST 'DIFFERENCE 1 (LIST 'EXPT ONEMTH 2))
                     NIL)
               POWLIS*))
      (SETQ SBST (LIST 'DIFFERENCE (LIST 'QUOTIENT SS SQMN) B))
      (SETQ FCTR (MULTSQ (SIMP ONEMTH) (INVSQ (SIMP SQMN))))
      (SETQ INTEGRAND
              (SUBS2Q (MULTSQ (SUBSQ INTEGRAND (LIST (CONS VAR SBST))) FCTR)))
      (SETQ BCKSHFT (LIST 'TIMES (LIST 'PLUS VAR B) SQMN))
      (SETQ SS
              (LIST (CONS (REVAL1 (LIST 'SIN NEWVAR) T) BCKSHFT)
                    (CONS NEWVAR (LIST 'ASIN BCKSHFT))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PRIN2 "Integrand is transformed by substitution to ")
         (TERPRI* T)
         (PRINTSQ INTEGRAND)
         (PRIN2 "using substitution ")
         (TERPRI* T)
         (MATHPRINT (LIST 'REPLACEBY VAR SBST))
         NIL)))
      (SETQ RES (INTEGRATESQ-SUBSTITUTED INTEGRAND NEWVAR))
      (SETQ POWLIS* (CDR POWLIS*))
      (COND
       ((NULL RES)
        (PROGN
         (COND
          ((OR *TRINT *TRINTSUBST)
           (PROGN
            (PRIN2 "Substituted integral FAILED")
            (TERPRI)
            "Substituted integral FAILED")))
         (RETURN NIL))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN
          (PRIN2 "Result of substituted integral is:")
          (TERPRI)
          "Result of substituted integral is:")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES)))))
         NIL)))
      (COND
       ((NOT (NULL (CAR (CDR RES))))
        (PROGN
         ((LAMBDA (*NOLNR) (SETQ X (SIMPINT1 (LIST (CDR RES) NEWVAR)))) T)
         (COND
          ((FREEOF X 'INT) (SETQ RES (CONS (ADDSQ (CAR RES) X) (CONS NIL 1))))
          ((NULL (CAR (CAR RES)))
           (PROGN
            (COND
             ((OR *TRINT *TRINTSUBST)
              (PROGN
               (PRINC "Returning because still a bad part of ")
               (PRINTSQ (CDR RES))
               NIL)))
            (RETURN NIL)
            NIL))))))
      (SETQ RES
              (CONS (SUBSQ (CAR RES) (CDR SS))
                    (SUBSQ (MULTSQ (CDR RES) (INVSQ FCTR)) SS)))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN (PRIN2 "Transforming back:") (TERPRI) "Transforming back:")
         (MATHPRINT (LIST 'REPLACEBY NEWVAR (CDR (CAR SS))))
         (PRIN2 "giving ...")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES))))))))
      (COND ((NULL (CAR (CAR RES))) (RETURN NIL)))
      (RETURN RES))) 
(PUT 'LOOK_FOR_INVHYP 'NUMBER-OF-ARGS 6) 
(PUT 'LOOK_FOR_INVHYP 'DEFINED-ON-LINE '724) 
(PUT 'LOOK_FOR_INVHYP 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_INVHYP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_INVHYP (INTEGRAND DO_ACOSH VAR A B C)
    (PROG (NEWVAR RES SS SQMN ONEMTH FCTR SBST BCKSHFT M N REALDOM)
      (SETQ M (PREPSQ A))
      (SETQ N (PREPSQ C))
      (SETQ B (PREPSQ B))
      (SETQ NEWVAR (INT-GENSYM1 'INTVAR))
      (COND
       (DO_ACOSH
        (PROGN
         (SETQ SQMN
                 (PREPSQ
                  (APPLY1 (GET 'SQRT 'SIMPFN)
                          (LIST (LIST 'QUOTIENT N (LIST 'MINUS M))))))
         (SETQ ONEMTH (LIST 'SINH NEWVAR))
         (SETQ SS (LIST 'COSH NEWVAR))))
       (T
        (PROGN
         (SETQ SQMN
                 (PREPSQ
                  (APPLY1 (GET 'SQRT 'SIMPFN) (LIST (LIST 'QUOTIENT N M)))))
         (SETQ ONEMTH (LIST 'COSH NEWVAR))
         (SETQ SS (LIST 'SINH NEWVAR)))))
      (SETQ SBST (LIST 'DIFFERENCE (LIST 'QUOTIENT SS SQMN) B))
      (SETQ FCTR (MULTSQ (SIMP ONEMTH) (INVSQ (SIMP SQMN))))
      (SETQ POWLIS*
              (CONS
               (LIST SS 2 '(NIL . T)
                     (LIST (COND (DO_ACOSH 'PLUS) (T 'DIFFERENCE))
                           (LIST 'EXPT ONEMTH 2) 1)
                     NIL)
               POWLIS*))
      (SETQ INTEGRAND
              (SUBS2Q (MULTSQ (SUBSQ INTEGRAND (LIST (CONS VAR SBST))) FCTR)))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PRIN2 "Integrand is transformed by substitution to ")
         (TERPRI* T)
         (PRINTSQ INTEGRAND)
         (PRIN2 "using substitution ")
         (TERPRI* T)
         (MATHPRINT (LIST 'REPLACEBY VAR SBST))
         NIL)))
      (SETQ REALDOM (NOT (SMEMBER '(SQRT -1) INTEGRAND)))
      (SETQ RES (INTEGRATESQ-SUBSTITUTED INTEGRAND NEWVAR))
      (SETQ POWLIS* (CDR POWLIS*))
      (COND
       ((NULL RES)
        (PROGN
         (COND
          ((OR *TRINT *TRINTSUBST)
           (PROGN
            (PRIN2 "Substituted integral FAILED")
            (TERPRI)
            "Substituted integral FAILED")))
         (RETURN NIL))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN
          (PRIN2 "Result of substituted integral is:")
          (TERPRI)
          "Result of substituted integral is:")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES)))))
         NIL)))
      (SETQ BCKSHFT (LIST 'TIMES (LIST 'PLUS VAR B) SQMN))
      (SETQ SS
              (CONS
               (REVAL1
                (COND (DO_ACOSH (LIST 'COSH NEWVAR)) (T (LIST 'SINH NEWVAR)))
                T)
               BCKSHFT))
      (COND
       (*HYPERBOLIC
        (PROGN
         (SETQ SS
                 (LIST SS
                       (CONS NEWVAR
                             (LIST (COND (DO_ACOSH 'ACOSH) (T 'ASINH))
                                   BCKSHFT))))
         NIL))
       (T
        (PROGN
         (SETQ SS
                 (LIST SS
                       (CONS NEWVAR
                             (REVAL1
                              (COND
                               (DO_ACOSH
                                (SUBST BCKSHFT 'SS
                                       '(LOG
                                         (PLUS SS
                                               (SQRT
                                                (DIFFERENCE (TIMES SS SS)
                                                            1))))))
                               (T
                                (SUBST BCKSHFT 'SS
                                       '(LOG
                                         (PLUS SS
                                               (SQRT
                                                (PLUS (TIMES SS SS) 1)))))))
                              T)))))))
      (SETQ RES
              (CONS (SQRT2TOP (SUBSQ (CAR RES) (CDR SS)))
                    (SQRT2TOP (SUBSQ (MULTSQ (CDR RES) (INVSQ FCTR)) SS))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN (PRIN2 "Transforming back:") (TERPRI) "Transforming back:")
         (MATHPRINT (LIST 'REPLACEBY NEWVAR (CDR (CADR SS))))
         (PRIN2 "giving ...")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES))))))))
      (COND
       ((OR (NULL (CAR (CAR RES))) (NOT (NULL (CAR (CDR RES))))) (RETURN NIL)))
      (COND
       ((AND REALDOM (SMEMBER '(SQRT -1) RES))
        (PROGN
         (COND ((OR *TRINT *TRINTSUBST) (PRINT "Wrong sheet")))
         (RETURN NIL)
         NIL)))
      (RETURN RES))) 
(PUT 'INTEGRATESQ-SUBSTITUTED 'NUMBER-OF-ARGS 2) 
(PUT 'INTEGRATESQ-SUBSTITUTED 'DEFINED-ON-LINE '813) 
(PUT 'INTEGRATESQ-SUBSTITUTED 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'INTEGRATESQ-SUBSTITUTED 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTEGRATESQ-SUBSTITUTED (INTEGRAND NEWVAR)
    (PROG (COEFFT NEXP ERFG RES)
      (SETQ COEFFT (CONS 1 1))
      (SETQ NEXP (CAR INTEGRAND))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT (ATOM NEXP)) (NULL (CDR NEXP))
                (NOT (DEPENDS (CAAAR NEXP) NEWVAR))))
          (RETURN NIL)))
        (PROGN
         (SETQ COEFFT (MULTSQ COEFFT (CONS (LIST (CONS (CAAR NEXP) 1)) 1)))
         (SETQ NEXP (CDAR NEXP)))
        (GO WHILELABEL))
      (SETQ ERFG ERFG*)
      ((LAMBDA (INTVAR)
         (SETQ RES
                 (ERRORSET*
                  (LIST 'INTEGRATESQ (MKQUOTE INTEGRAND) (MKQUOTE NEWVAR) NIL
                        NIL)
                  *BACKTRACE)))
       NEWVAR)
      (SETQ ERFG* ERFG)
      (COND ((OR (NULL RES) (ERRORP RES)) (RETURN NIL))
            (T (SETQ RES (CAR RES))))
      (RETURN RES))) 
(PUT 'SUBST-AND-INT 'NUMBER-OF-ARGS 7) 
(PUT 'SUBST-AND-INT 'DEFINED-ON-LINE '836) 
(PUT 'SUBST-AND-INT 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'SUBST-AND-INT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(DE SUBST-AND-INT (INTEGRAND VAR NVAR SBST BCKSBST FCT FLAGS)
    (PROG (RES X REALDOM)
      (SETQ INTEGRAND (SUBSQ INTEGRAND (LIST (CONS VAR SBST))))
      (SETQ INTEGRAND (MULTSQ INTEGRAND FCT))
      (COND ((MEMQ 'DOSUBS2Q FLAGS) (SETQ INTEGRAND (SUBS2Q INTEGRAND))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PRIN2 "Integrand is transformed by substitution to ")
         (TERPRI* T)
         (PRINTSQ INTEGRAND)
         (PRIN2 "using substitution ")
         (TERPRI* T)
         (MATHPRINT (LIST 'REPLACEBY VAR SBST))
         NIL)))
      (SETQ REALDOM
              (AND (MEMQ 'REALDOM FLAGS) (NOT (SMEMBER '(SQRT -1) INTEGRAND))))
      (SETQ RES (INTEGRATESQ-SUBSTITUTED INTEGRAND NVAR))
      (COND
       ((NULL RES)
        (PROGN
         (COND
          ((OR *TRINT *TRINTSUBST)
           (PROGN
            (PRIN2 "Substituted integral FAILED")
            (TERPRI)
            "Substituted integral FAILED")))
         (RETURN NIL))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN
          (PRIN2 "Result of substituted integral is:")
          (TERPRI)
          "Result of substituted integral is:")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES)))))
         NIL)))
      (COND
       ((NOT (NULL (CAR (CDR RES))))
        (PROGN
         ((LAMBDA (*NOLNR) (SETQ X (SIMPINT1 (LIST (CDR RES) NVAR)))) T)
         (COND
          ((FREEOF X 'INT) (SETQ RES (CONS (ADDSQ (CAR RES) X) (CONS NIL 1))))
          ((OR (MEMQ 'NOBAD FLAGS) (NULL (CAR (CAR RES))))
           (PROGN
            (COND
             ((OR *TRINT *TRINTSUBST)
              (PROGN
               (PRINC "Returning because still a bad part of ")
               (PRINTSQ (CDR RES))
               NIL)))
            (RETURN NIL)
            NIL))))))
      (SETQ BCKSBST (LIST (CONS NVAR BCKSBST)))
      (SETQ RES
              (CONS (SUBSQ (CAR RES) BCKSBST)
                    (SUBSQ (MULTSQ (CDR RES) (INVSQ FCT)) BCKSBST)))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN (PRIN2 "Transforming back...") (TERPRI) "Transforming back...")
         (MATHPRINT (LIST 'REPLACEBY NVAR (CDR (CAR BCKSBST))))
         (PRIN2 "giving ...")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES))))))))
      (COND
       ((AND REALDOM (SMEMBER '(SQRT -1) RES))
        (PROGN
         (COND ((OR *TRINT *TRINTSUBST) (PRINT "Wrong sheet")))
         (RETURN NIL)
         NIL)))
      (RETURN RES))) 
(PUT 'SIMPINT1 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINT1 'DEFINED-ON-LINE '905) 
(PUT 'SIMPINT1 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'SIMPINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINT1 (U)
    (PROG (*KEEPSQRTS V VARSTACK*)
      (SETQ U (CONS 'INT (CONS (PREPSQ (CAR U)) (CDR U))))
      (COND
       ((NEQ (SETQ V (FORMLNR U)) U)
        (COND
         (*NOLNR
          (PROGN
           (SETQ V (SIMP (SUBST 'INT* 'INT V)))
           (RETURN (CONS (REMAKESF (CAR V)) (REMAKESF (CDR V))))))
         (T
          (PROGN
           (SETQ *NOLNR (CONS NIL *NOLNR))
           (SETQ V (ERRORSET* (LIST 'SIMP (MKQUOTE V)) *BACKTRACE))
           (COND ((PAIRP V) (SETQ V (CAR V))) (T (SETQ V (SIMP U))))
           (SETQ *NOLNR (CDR *NOLNR))
           (RETURN V))))))
      (RETURN (COND ((SETQ V (OPMTCH U)) (SIMP V)) (T (SYMINT U)))))) 
(MKOP 'INT*) 
(PUT 'INT* 'SIMPFN 'SIMPINT*) 
(PUT 'SIMPINT* 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPINT* 'DEFINED-ON-LINE '927) 
(PUT 'SIMPINT* 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'SIMPINT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPINT* (U)
    (PROG (X)
      (RETURN
       (COND ((SETQ X (OPMTCH (CONS 'INT U))) (SIMP X))
             (T (SIMPIDEN (CONS 'INT* U))))))) 
(PUT 'REMAKESF 'NUMBER-OF-ARGS 1) 
(PUT 'REMAKESF 'DEFINED-ON-LINE '933) 
(PUT 'REMAKESF 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'REMAKESF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMAKESF (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) U)
          (T
           (ADDF
            ((LAMBDA (G554)
               ((LAMBDA (G544)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 G554))
                        (T (POLY-MULTF G544 G554))))
                (LIST
                 (CONS
                  (COND
                   ((EQCAR (CAAAR U) 'INT*)
                    (GETPOWER (FKERN (CONS 'INT (CDR (CAAAR U)))) (CDAAR U)))
                   (T (CAAR U)))
                  1))))
             (REMAKESF (CDAR U)))
            (REMAKESF (CDR U)))))) 
(PUT 'ALLOWEDFNS 'NUMBER-OF-ARGS 1) 
(PUT 'ALLOWEDFNS 'DEFINED-ON-LINE '941) 
(PUT 'ALLOWEDFNS 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'ALLOWEDFNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLOWEDFNS (U)
    (COND ((NULL U) T)
          ((ATOM (CAR U))
           (OR (EQUAL (CAR U) INTVAR) (NOT (DEPENDS (CAR U) INTVAR))))
          ((AND (EQUAL (CAAR U) 'EXPT) (NOT (EQUAL (CADAR U) 'E))
                (NOT (DEPENDS (CADAR U) INTVAR)) (DEPENDS (CADDAR U) INTVAR))
           NIL)
          ((FLAGP (CAAR U) 'TRANSCENDENTAL) (ALLOWEDFNS (CDR U))) (T NIL))) 
(PUT 'LOOK_FOR_POWER 'NUMBER-OF-ARGS 2) 
(PUT 'LOOK_FOR_POWER 'DEFINED-ON-LINE '950) 
(PUT 'LOOK_FOR_POWER 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'LOOK_FOR_POWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOOK_FOR_POWER (INTEGRAND VAR)
    (PROG (NN DD MM)
      (SETQ NN (CAR INTEGRAND))
      (SETQ DD (CDR INTEGRAND))
      (COND
       ((AND
         (EQUAL
          (CDR
           (SETQ MM
                   (MULTSQ (CONS NN 1)
                           (INVSQ
                            (CONS (LIST (CONS (GETPOWER (FKERN VAR) 1) 1))
                                  1)))))
          1)
         (EVEN_POWER (CAR MM) VAR) (EVEN_POWER DD VAR))
        (PROGN (RETURN (SQRT_SUBSTITUTE (CAR MM) DD VAR)) NIL)))
      (COND
       ((AND
         (EQUAL
          (CDR
           (SETQ MM
                   (MULTSQ (CONS DD 1)
                           (INVSQ
                            (CONS (LIST (CONS (GETPOWER (FKERN VAR) 1) 1))
                                  1)))))
          1)
         (EVEN_POWER NN VAR) (EVEN_POWER (CAR MM) VAR))
        (PROGN (RETURN (SQRT_SUBSTITUTE NN (CAR MM) VAR)) NIL)))
      (RETURN NIL))) 
(PUT 'EVEN_POWER 'NUMBER-OF-ARGS 2) 
(PUT 'EVEN_POWER 'DEFINED-ON-LINE '967) 
(PUT 'EVEN_POWER 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'EVEN_POWER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVEN_POWER (XPR VAR)
    (COND ((ATOM XPR) T)
          ((EQUAL (CAAAR XPR) VAR)
           (PROGN
            (COND
             ((EVENP (CDR (CAAR XPR)))
              (AND (EVEN_POWER (CDAR XPR) VAR) (EVEN_POWER (CDR XPR) VAR)))
             (T NIL))))
          ((AND (EQCAR (CAAAR XPR) 'EXPT) (EQUAL (CADR (CAAAR XPR)) VAR)
                (EVENP (CADDR (CAAAR XPR))))
           T)
          ((ATOM (CAAAR XPR))
           (AND (EVEN_POWER (CDAR XPR) VAR) (EVEN_POWER (CDR XPR) VAR)))
          ((AND (EVEN_POWER (CDR XPR) VAR) (EVEN_POWER (CDAR XPR) VAR))
           (EVEN_PREP (CAAAR XPR) VAR)))) 
(PUT 'EVEN_PREP 'NUMBER-OF-ARGS 2) 
(PUT 'EVEN_PREP 'DEFINED-ON-LINE '981) 
(PUT 'EVEN_PREP 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'EVEN_PREP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVEN_PREP (XPR VAR)
    (COND ((EQUAL XPR VAR) NIL) ((ATOM XPR) T)
          ((AND (EQCAR XPR 'EXPT) (EQUAL (CADR XPR) VAR) (EVENP (CADDR XPR)))
           T)
          ((EVEN_PREP (CAR XPR) VAR) (EVEN_PREP (CDR XPR) VAR)))) 
(PUT 'SQRT_SUBSTITUTE 'NUMBER-OF-ARGS 3) 
(PUT 'SQRT_SUBSTITUTE 'DEFINED-ON-LINE '988) 
(PUT 'SQRT_SUBSTITUTE 'DEFINED-IN-FILE 'INT/DRIVER.RED) 
(PUT 'SQRT_SUBSTITUTE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SQRT_SUBSTITUTE (NN DD VAR)
    (PROG (NEWVAR INTEGRAND RES SS *KEEPSQRTS)
      (SETQ NEWVAR (INT-GENSYM1 'INTVAR))
      (SETQ INTEGRAND
              (SUBST (LIST 'SQRT NEWVAR) VAR
                     (LIST 'QUOTIENT (PREPSQ (CONS NN DD)) 2)))
      (SETQ INTEGRAND (PREPSQ (SIMP INTEGRAND)))
      (SETQ INTEGRAND (SIMP INTEGRAND))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PRIN2 "Integrand is transformed by substitution to ")
         (TERPRI* T)
         (PRINTSQ INTEGRAND)
         (PRIN2 "using substitution ")
         (TERPRI* T)
         (MATHPRINT (LIST 'REPLACEBY VAR (LIST 'SQRT NEWVAR)))
         NIL)))
      (SETQ RES (INTEGRATESQ-SUBSTITUTED INTEGRAND NEWVAR))
      (COND
       ((NULL RES)
        (PROGN
         (COND
          ((OR *TRINT *TRINTSUBST)
           (PROGN
            (PRIN2 "Substituted integral FAILED")
            (TERPRI)
            "Substituted integral FAILED")))
         (RETURN NIL))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN
          (PRIN2 "Result of substituted integral is:")
          (TERPRI)
          "Result of substituted integral is:")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES)))))
         NIL)))
      (SETQ SS (LIST (CONS NEWVAR (LIST 'EXPT VAR 2))))
      (SETQ RES
              (CONS (SUBSQ (CAR RES) SS)
                    (MULTSQ (CONS (CONS (CONS (CONS VAR 1) 2) NIL) 1)
                            (SUBSQ (CDR RES) SS))))
      (COND
       ((OR *TRINT *TRINTSUBST)
        (PROGN
         (PROGN (PRIN2 "Transforming back...") (TERPRI) "Transforming back...")
         (MATHPRINT (LIST 'REPLACEBY NEWVAR (CDR (CAR SS))))
         (PRIN2 "giving ...")
         (PRINTSQ (CAR RES))
         (COND
          ((NOT (NULL (CAR (CDR RES))))
           (PROGN (PRIN2 " plus a bad part of ") (PRINTSQ (CDR RES))))))))
      (RETURN RES))) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(SETK 'INTRULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'EXPT 'E
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ACOSH (LIST '~ 'X))))
                   (LIST 'WHEN
                         (LIST 'EXPT
                               (LIST 'PLUS
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE (LIST 'EXPT 'X 2)
                                                 1))
                                     'X)
                               'N)
                         (LIST 'NUMBERP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'EXPT 'E
                         (LIST 'TIMES (LIST '~ 'N) (LIST 'ASINH (LIST '~ 'X))))
                   (LIST 'WHEN
                         (LIST 'EXPT
                               (LIST 'PLUS
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'EXPT 'X 2) 1))
                                     'X)
                               'N)
                         (LIST 'NUMBERP 'N)))
             (LIST 'REPLACEBY (LIST 'EXPT 'E (LIST 'ACOSH (LIST '~ 'X)))
                   (LIST 'PLUS
                         (LIST 'SQRT (LIST 'DIFFERENCE (LIST 'EXPT 'X 2) 1))
                         'X))
             (LIST 'REPLACEBY (LIST 'EXPT 'E (LIST 'ASINH (LIST '~ 'X)))
                   (LIST 'PLUS (LIST 'SQRT (LIST 'PLUS (LIST 'EXPT 'X 2) 1))
                         'X))
             (LIST 'REPLACEBY (LIST 'COSH (LIST 'LOG (LIST '~ 'X)))
                   (LIST 'QUOTIENT (LIST 'PLUS (LIST 'EXPT 'X 2) 1)
                         (LIST 'TIMES 2 'X)))
             (LIST 'REPLACEBY (LIST 'SINH (LIST 'LOG (LIST '~ 'X)))
                   (LIST 'QUOTIENT (LIST 'DIFFERENCE (LIST 'EXPT 'X 2) 1)
                         (LIST 'TIMES 2 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'X))
                               (LIST 'DIFFERENCE (LIST '~ 'B) 'X))
                         'X)
                   (LIST 'WHEN (LIST 'DILOG (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'X))
                               (LIST 'DIFFERENCE (LIST 'TIMES (LIST '~ 'B) 'X)
                                     (LIST 'EXPT 'X 2)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'QUOTIENT
                                     (LIST 'DILOG (LIST 'QUOTIENT 'X 'B)) 'B)
                               (LIST 'QUOTIENT (LIST 'EXPT (LIST 'LOG 'X) 2)
                                     (LIST 'TIMES 2 'B)))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'EXPT (LIST '~ 'F)
                               (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                     (LIST 'EXPT (LIST '~ 'X) 2)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'TIMES 'I
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG 'F)))
                                           'X))
                               (LIST 'QUOTIENT (LIST 'SQRT 'PI)
                                     (LIST 'TIMES 2 'I
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG 'F))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT (LIST 'EXPT (LIST '~ 'X) 2)
                                     (LIST '~ 'B)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'TIMES 'I
                                           (LIST 'QUOTIENT 'X
                                                 (LIST 'SQRT 'B))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT (LIST 'SQRT 'B)
                                     (LIST 'TIMES 2 'I)))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'F)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'EXPT (LIST '~ 'X) 2))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'TIMES
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG 'F)))
                                           'X))
                               (LIST 'QUOTIENT (LIST 'SQRT 'PI)
                                     (LIST 'TIMES 2
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG 'F))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'EXPT (LIST '~ 'F)
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT (LIST '~ 'X) 2)
                                           (LIST '~ 'B))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'QUOTIENT 'X
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'LOG 'F)))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT
                                     (LIST 'SQRT
                                           (LIST 'TIMES 'B (LIST 'LOG 'F)))
                                     2))
                         (LIST 'AND (LIST 'FREEOF 'F 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'EXPT (LIST '~ 'F)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST 'EXPT (LIST '~ 'X) 2))
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST '~ 'X)))
                                     (LIST '~ (LIST '~ 'D))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'I
                                                 (LIST 'SQRT
                                                       (LIST 'TIMES 'B
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'LOG
                                                                         'F)
                                                                   'D)))
                                                 'X)
                                           (LIST 'TIMES 'C
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 2 'I 'D
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         'B
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          (LIST
                                                                           'LOG
                                                                           'F)
                                                                          'D))))))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT
                                     (LIST 'EXP
                                           (LIST 'MINUS
                                                 (LIST 'TIMES (LIST 'EXPT 'C 2)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             (LIST 'TIMES 4 'B
                                                                   'D)))))
                                     (LIST 'TIMES 2 'I
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             'D))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'EXPT (LIST '~ 'F)
                               (LIST 'QUOTIENT
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT
                                                 (LIST 'EXPT (LIST '~ 'X) 2)
                                                 (LIST '~ 'B))
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                                 (LIST '~ 'X)))
                                     (LIST '~ (LIST '~ 'D))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'ERF
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES 'I
                                                 (LIST 'SQRT
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             (LIST 'TIMES 'B
                                                                   'D)))
                                                 'X)
                                           (LIST 'TIMES 'C
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 2 'I 'D
                                                             (LIST 'SQRT
                                                                   (LIST
                                                                    'QUOTIENT
                                                                    (LIST 'LOG
                                                                          'F)
                                                                    (LIST
                                                                     'TIMES 'B
                                                                     'D))))))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT
                                     (LIST 'EXP
                                           (LIST 'MINUS
                                                 (LIST 'TIMES 'B
                                                       (LIST 'EXPT 'C 2)
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             (LIST 'TIMES 4
                                                                   'D)))))
                                     (LIST 'TIMES 2 'I
                                           (LIST 'SQRT
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 'B 'D))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'F)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'C))
                                           (LIST '~ 'X)))
                               (LIST 'EXPT (LIST '~ 'F)
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'B))
                                                       (LIST 'EXPT (LIST '~ 'X)
                                                             2))
                                                 (LIST '~ (LIST '~ 'D)))
                                           (LIST '~ (LIST '~ 'A)))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EXPT 'F
                                     (LIST 'MINUS (LIST 'QUOTIENT 'D 'A)))
                               (LIST 'ERF
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES
                                                 (LIST 'SQRT
                                                       (LIST 'TIMES 'B
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'LOG
                                                                         'F)
                                                                   'A)))
                                                 'X)
                                           (LIST 'TIMES 'C
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 2
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         'B
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          (LIST
                                                                           'LOG
                                                                           'F)
                                                                          'A))))))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT
                                     (LIST 'EXP
                                           (LIST 'TIMES 'A (LIST 'EXPT 'C 2)
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 4 'B))))
                                     (LIST 'TIMES 2
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             'A))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'A 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST '~ 'G)
                               (LIST 'EXPT (LIST '~ 'F)
                                     (LIST 'QUOTIENT
                                           (LIST 'PLUS
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'B))
                                                       (LIST 'EXPT (LIST '~ 'X)
                                                             2))
                                                 (LIST 'TIMES
                                                       (LIST '~ (LIST '~ 'C))
                                                       (LIST '~ 'X))
                                                 (LIST '~ (LIST '~ 'D)))
                                           (LIST '~ (LIST '~ 'A)))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES 'G
                               (LIST 'EXPT 'F
                                     (LIST 'MINUS (LIST 'QUOTIENT 'D 'A)))
                               (LIST 'ERF
                                     (LIST 'PLUS
                                           (LIST 'TIMES
                                                 (LIST 'SQRT
                                                       (LIST 'TIMES 'B
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'LOG
                                                                         'F)
                                                                   'A)))
                                                 'X)
                                           (LIST 'TIMES 'C
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 2 'A
                                                             (LIST 'SQRT
                                                                   (LIST 'TIMES
                                                                         'B
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          (LIST
                                                                           'LOG
                                                                           'F)
                                                                          'A))))))))
                               (LIST 'SQRT 'PI)
                               (LIST 'QUOTIENT
                                     (LIST 'EXP
                                           (LIST 'TIMES (LIST 'EXPT 'C 2)
                                                 (LIST 'QUOTIENT (LIST 'LOG 'F)
                                                       (LIST 'TIMES 4 'A 'B))))
                                     (LIST 'TIMES 2
                                           (LIST 'SQRT
                                                 (LIST 'TIMES 'B
                                                       (LIST 'QUOTIENT
                                                             (LIST 'LOG 'F)
                                                             'A))))))
                         (LIST 'AND (LIST 'FREEOF 'F 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'C 'X) (LIST 'FREEOF 'D 'X)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'G 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 'E
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'EI (LIST 'TIMES 'B 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'EI (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXP
                                           (LIST 'TIMES (LIST '~ 'X)
                                                 (LIST '~ (LIST '~ 'B))))
                                     'X))
                         'X)
                   (LIST 'WHEN (LIST 'EI (LIST 'MINUS (LIST 'TIMES 'X 'B)))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXP
                                           (LIST 'QUOTIENT (LIST '~ 'X)
                                                 (LIST '~ 'B)))
                                     'X))
                         'X)
                   (LIST 'WHEN (LIST 'EI (LIST 'MINUS (LIST 'QUOTIENT 'X 'B)))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'EI (LIST 'TIMES 'X 'B (LIST 'LOG 'A)))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN
                         (LIST 'EI
                               (LIST 'TIMES (LIST 'QUOTIENT 'X 'B)
                                     (LIST 'LOG 'A)))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST '~ 'X)))
                                     'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'EI
                               (LIST 'MINUS
                                     (LIST 'TIMES 'X 'B (LIST 'LOG 'A))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'QUOTIENT (LIST '~ 'X)
                                                 (LIST '~ 'B)))
                                     'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'EI
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'QUOTIENT 'X 'B)
                                           (LIST 'LOG 'A))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST 'EXPT (LIST '~ 'X)
                                                 (LIST '~ 'N))))
                               (LIST '~ 'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI
                                     (LIST 'TIMES (LIST 'EXPT 'X 'N) 'B
                                           (LIST 'LOG 'A)))
                               'N)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST '~ 'A)
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT (LIST '~ 'X)
                                                 (LIST '~ 'N))
                                           (LIST '~ 'B)))
                               (LIST '~ 'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI
                                     (LIST 'TIMES
                                           (LIST 'QUOTIENT (LIST 'EXPT 'X 'N)
                                                 'B)
                                           (LIST 'LOG 'A)))
                               'N)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST 'EXPT (LIST '~ 'X)
                                                       (LIST '~ 'N))))
                                     'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI
                                     (LIST 'MINUS
                                           (LIST 'TIMES (LIST 'EXPT 'X 'N) 'B
                                                 (LIST 'LOG 'A))))
                               'N)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST '~ 'A)
                                           (LIST 'QUOTIENT
                                                 (LIST 'EXPT (LIST '~ 'X)
                                                       (LIST '~ 'N))
                                                 (LIST '~ 'B)))
                                     'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'QUOTIENT
                                                       (LIST 'EXPT 'X 'N) 'B)
                                                 (LIST 'LOG 'A))))
                               'N)
                         (LIST 'AND (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X)
                               (LIST 'FREEOF 'N 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'SIN
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'SI (LIST 'TIMES 'B 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'SIN
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'SI (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'X)))
                               (LIST 'QUOTIENT
                                     (LIST 'SIN
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                                 (LIST '~ 'X)))
                                     'X))
                         'X)
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                               (LIST 'SI
                                     (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                           (LIST 'TIMES 'B 'X))))
                         (LIST 'TIMES (LIST 'QUOTIENT 1 2)
                               (LIST 'SI
                                     (LIST 'DIFFERENCE (LIST 'TIMES 'A 'X)
                                           (LIST 'TIMES 'B 'X))))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST 'SIN (LIST '~ 'X))
                               (LIST 'EXPT 'X 2))
                         'X)
                   (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT (LIST 'SIN 'X) 'X))
                         (LIST 'CI 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'SIN (LIST '~ 'X)) 2) 'X)
                         'X)
                   (LIST 'QUOTIENT
                         (LIST 'DIFFERENCE (LIST 'LOG 'X)
                               (LIST 'CI (LIST 'TIMES 2 'X)))
                         2))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'CI (LIST 'TIMES 'B 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'COS
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'CI (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST 'COS (LIST '~ 'X))
                               (LIST 'EXPT 'X 2))
                         'X)
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'QUOTIENT (LIST 'COS 'X) 'X))
                         (LIST 'SI 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'EXPT (LIST 'COS (LIST '~ 'X)) 2) 'X)
                         'X)
                   (LIST 'PLUS (LIST 'LOG 'X)
                         (LIST 'QUOTIENT (LIST 'CI (LIST 'TIMES 2 'X)) 2)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'SINH
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'SHI (LIST 'TIMES 'B 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'SINH
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'SHI (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'COSH
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'CHI (LIST 'TIMES 'B 'X))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'COSH
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B)))
                               'X)
                         'X)
                   (LIST 'WHEN (LIST 'CHI (LIST 'QUOTIENT 'X 'B))
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'SIN
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST 'EXPT 'X 2)))
                         'X)
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'QUOTIENT (LIST 'SQRT 'PI) (LIST 'SQRT 2))
                               (LIST 'SQRT 'A))
                         (LIST 'FRESNEL_S
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'TIMES 2
                                                 (LIST 'QUOTIENT 'A 'PI)))
                                     'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'COS
                               (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                     (LIST 'EXPT 'X 2)))
                         'X)
                   (LIST 'TIMES
                         (LIST 'QUOTIENT
                               (LIST 'QUOTIENT (LIST 'SQRT 'PI) (LIST 'SQRT 2))
                               (LIST 'SQRT 'A))
                         (LIST 'FRESNEL_C
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'TIMES 2
                                                 (LIST 'QUOTIENT 'A 'PI)))
                                     'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'LOG
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI (LIST 'LOG (LIST 'TIMES 'B 'X))) 'B)
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'LOG
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'B))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EI (LIST 'LOG (LIST 'QUOTIENT 'X 'B)))
                               'B)
                         (LIST 'FREEOF 'B 'X)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'LOG
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ 'B))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'QUOTIENT
                               (LIST 'EI
                                     (LIST 'LOG
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               'A)
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'LOG
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT (LIST '~ 'X)
                                                 (LIST '~ 'A))
                                           (LIST '~ 'B))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'TIMES
                               (LIST 'EI
                                     (LIST 'LOG
                                           (LIST 'PLUS (LIST 'QUOTIENT 'X 'A)
                                                 'B)))
                               'A)
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST '~ 'X) (LIST 'LOG (LIST '~ 'X)))
                         'X)
                   (LIST 'EI (LIST 'TIMES 2 (LIST 'LOG 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N))
                               (LIST 'LOG 'X))
                         'X)
                   (LIST 'WHEN
                         (LIST 'EI
                               (LIST 'TIMES (LIST 'PLUS 'N 1) (LIST 'LOG 'X)))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT 1
                               (LIST 'TIMES
                                     (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N))
                                     (LIST 'LOG 'X)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'EI
                               (LIST 'TIMES (LIST 'PLUS (LIST 'MINUS 'N) 1)
                                     (LIST 'LOG 'X)))
                         (LIST 'FIXP 'N)))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'EI
                               (LIST 'LOG
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 'X)
                                           (LIST '~ (LIST '~ 'B)))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'EI
                                           (LIST 'LOG
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES 'A 'X)
                                                       'B)))
                                     (LIST 'PLUS 'X (LIST 'QUOTIENT 'B 'A)))
                               (LIST 'QUOTIENT
                                     (LIST 'EI
                                           (LIST 'TIMES 2
                                                 (LIST 'LOG
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'A
                                                                   'X)
                                                             'B))))
                                     'A))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'ASIN
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'B))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'A)
                               (LIST 'PLUS
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X) 'B)
                                           (LIST 'ASIN
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES 'A 'X)
                                                       'B)))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'A
                                                                   'X)
                                                             'B)
                                                       2)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'N)))
                               (LIST 'ASIN
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B)))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X (LIST 'PLUS 'N 1))
                                           (LIST 'PLUS 'N 1))
                                     (LIST 'ASIN
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A (LIST 'PLUS 'N 1))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'PLUS 'N 1))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'ASIN
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B))))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X
                                                 (LIST 'DIFFERENCE 1 'N))
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'ASIN
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'DIFFERENCE 1 'N))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'ACOS
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'B))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'A)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X) 'B)
                                           (LIST 'ACOS
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES 'A 'X)
                                                       'B)))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE 1
                                                 (LIST 'EXPT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'A
                                                                   'X)
                                                             'B)
                                                       2)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'N)))
                               (LIST 'ACOS
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B)))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X (LIST 'PLUS 'N 1))
                                           (LIST 'PLUS 'N 1))
                                     (LIST 'ACOS
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A (LIST 'PLUS 'N 1))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'PLUS 'N 1))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'ACOS
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B))))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X
                                                 (LIST 'DIFFERENCE 1 'N))
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'ACOS
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'DIFFERENCE 1 'N))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'ASINH
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'B))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'A)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X) 'B)
                                           (LIST 'ASINH
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES 'A 'X)
                                                       'B)))
                                     (LIST 'SQRT
                                           (LIST 'PLUS 1
                                                 (LIST 'EXPT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'A
                                                                   'X)
                                                             'B)
                                                       2)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'N)))
                               (LIST 'ASINH
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B)))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X (LIST 'PLUS 'N 1))
                                           (LIST 'PLUS 'N 1))
                                     (LIST 'ASINH
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A (LIST 'PLUS 'N 1))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'PLUS 'N 1))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS 1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'PLUS 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'ASINH
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B))))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X
                                                 (LIST 'DIFFERENCE 1 'N))
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'ASINH
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'DIFFERENCE 1 'N))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'PLUS 1
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)))
                                                       (LIST 'PLUS 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'ACOSH
                               (LIST 'PLUS
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                           (LIST '~ 'X))
                                     (LIST '~ (LIST '~ 'B))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 'A)
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X) 'B)
                                           (LIST 'ACOSH
                                                 (LIST 'PLUS
                                                       (LIST 'TIMES 'A 'X)
                                                       'B)))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'EXPT
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES 'A
                                                                   'X)
                                                             'B)
                                                       2)
                                                 1))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ (LIST '~ 'N)))
                               (LIST 'ACOSH
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B)))))
                         (LIST '~ 'X))
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X (LIST 'PLUS 'N 1))
                                           (LIST 'PLUS 'N 1))
                                     (LIST 'ACOSH
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A (LIST 'PLUS 'N 1))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'PLUS 'N 1))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)
                                                                   1))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 0)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'QUOTIENT
                               (LIST 'ACOSH
                                     (LIST 'PLUS
                                           (LIST 'TIMES (LIST '~ (LIST '~ 'A))
                                                 (LIST '~ 'X))
                                           (LIST '~ (LIST '~ 'B))))
                               (LIST 'EXPT (LIST '~ 'X) (LIST '~ 'N)))
                         'X)
                   (LIST 'WHEN
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT 'X
                                                 (LIST 'DIFFERENCE 1 'N))
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'ACOSH
                                           (LIST 'PLUS (LIST 'TIMES 'A 'X)
                                                 'B)))
                               (LIST 'TIMES
                                     (LIST 'QUOTIENT 'A
                                           (LIST 'DIFFERENCE 1 'N))
                                     (LIST 'INT
                                           (LIST 'TIMES
                                                 (LIST 'EXPT 'X
                                                       (LIST 'DIFFERENCE 1 'N))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'SQRT
                                                             (LIST 'DIFFERENCE
                                                                   (LIST 'EXPT
                                                                         (LIST
                                                                          'PLUS
                                                                          (LIST
                                                                           'TIMES
                                                                           'A
                                                                           'X)
                                                                          'B)
                                                                         2)
                                                                   1))
                                                       (LIST 'DIFFERENCE 1
                                                             (LIST 'EXPT
                                                                   (LIST 'PLUS
                                                                         (LIST
                                                                          'TIMES
                                                                          'A
                                                                          'X)
                                                                         'B)
                                                                   2))))
                                           'X)))
                         (LIST 'AND (LIST 'FIXP 'N) (LIST 'GREATERP 'N 1)
                               (LIST 'FREEOF 'A 'X) (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY
                   (LIST 'INT
                         (LIST 'ERF
                               (LIST 'PLUS (LIST '~ (LIST '~ 'A))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'B))
                                           (LIST '~ 'X))))
                         'X)
                   (LIST 'WHEN
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'PLUS 'A (LIST 'TIMES 'B 'X))
                                     (LIST 'QUOTIENT
                                           (LIST 'ERF
                                                 (LIST 'PLUS 'A
                                                       (LIST 'TIMES 'B 'X)))
                                           'B))
                               (LIST 'QUOTIENT 1
                                     (LIST 'TIMES 'B (LIST 'SQRT 'PI)
                                           (LIST 'EXPT 'E
                                                 (LIST 'EXPT
                                                       (LIST 'PLUS 'A
                                                             (LIST 'TIMES 'B
                                                                   'X))
                                                       2)))))
                         (LIST 'AND (LIST 'FREEOF 'A 'X)
                               (LIST 'FREEOF 'B 'X))))
             (LIST 'REPLACEBY (LIST 'INT (LIST 'PSI (LIST '~ 'Z)) 'Z)
                   (LIST 'LOG (LIST 'GAMMA 'Z)))
             (LIST 'REPLACEBY
                   (LIST 'INT (LIST 'POLYGAMMA (LIST '~ 'N) (LIST '~ 'X))
                         (LIST '~ 'X))
                   (LIST 'POLYGAMMA (LIST 'DIFFERENCE 'N 1) 'X))))) 
(PROG (OLDMODE)
  (COND (DMODE* (SETQ OLDMODE (SETDMODE DMODE* NIL))))
  (AEVAL (LET '(INTRULES)))
  (COND (OLDMODE (SETDMODE OLDMODE T)))) 
(ENDMODULE) 