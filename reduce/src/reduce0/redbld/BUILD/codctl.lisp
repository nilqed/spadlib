(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CODCTL)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL
 '(CODMAT ENDMAT *ACINFO PREVLST *SIDREL MAXVAR MALST ROWMAX ROWMIN *PRIALL
   *PRIMAT CODBEXL* *PREFIX *AGAIN OPS KVARLST CNAME* CINDEX* OPTLANG*
   GENTRANLANG* VARLST* VARLST+ *OUTSTK* *OPTDECS *INPUTC *VECTORC *INTERN
   MIN-EXPR-LENGTH*)) 
(FLUID '(*GENTRANOPT *DOUBLE *PERIOD *NOEQUIV)) 
(SWITCH
 (LIST 'ACINFO 'SIDREL 'PRIALL 'PRIMAT 'PREFIX 'OPTDECS 'AGAIN 'INPUTC 'VECTORC
       'INTERN)) 
(SETQ CODMAT
        (SETQ *PRIALL
                (SETQ *PRIMAT
                        (SETQ *SIDREL (SETQ *OPTDECS (SETQ OPTLANG* NIL)))))) 
(SETQ *AGAIN
        (SETQ *PREFIX
                (SETQ *ACINFO
                        (SETQ *INPUTC (SETQ *INTERN (SETQ *VECTORC NIL)))))) 
(SETQ MIN-EXPR-LENGTH* NIL) 
(SETQ ROWMIN 0) 
(SETQ ROWMAX (MINUS 1)) 
(LOAD-PACKAGE 'GENTRAN) 
(COND ((NOT *ROUNDED) (PROGN (ON (LIST 'ROUNDED)) (OFF (LIST 'ROUNDED))))) 
(PUT 'OPTIMIZE 'STAT 'OPTIMIZESTAT) 
(GLOBAL '(OPTLANG* AVARLST KNOWN RHSALIASES)) 
(FLUID '(*FORT PREPREFIXLIST PREFIXLIST)) 
(PUT 'OPTIMIZESTAT 'NUMBER-OF-ARGS 0) 
(PUT 'OPTIMIZESTAT 'DEFINED-ON-LINE '208) 
(PUT 'OPTIMIZESTAT 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'OPTIMIZESTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OPTIMIZESTAT NIL
    (PROG (FORMS VNAME INFILES OUTFILE X DECS KWDS DELIMS)
      (SYMTABREM '*MAIN* '*DECS*)
      (SETQ KWDS '(INAME IN OUT DECLARE))
      (SETQ DELIMS (APPEND KWDS '(*SEMICOL* *RSQB* END)))
      (FLAG KWDS 'DELIM)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (MEMQ CURSYM* DELIMS))) (RETURN NIL)))
        (COND ((SETQ X (XREADFORMS)) (SETQ FORMS (APPEND FORMS X))))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (MEMQ CURSYM* KWDS)) (RETURN NIL)))
        (COND ((EQ CURSYM* 'INAME) (SETQ VNAME (XREAD T)))
              ((EQ CURSYM* 'IN)
               (COND ((ATOM (SETQ X (XREAD NIL))) (SETQ INFILES (LIST X)))
                     ((EQCAR X '*COMMA*) (SETQ INFILES (CDR X)))
                     (T (SETQ INFILES X))))
              ((EQ CURSYM* 'OUT) (SETQ OUTFILE (XREAD T)))
              ((EQ CURSYM* 'DECLARE)
               (SETQ DECS (APPEND DECS (CDR (DECLARESTAT))))))
        (GO WHILELABEL))
      (REMFLAG KWDS 'DELIM)
      (RETURN
       (LIST 'SYMOPTIMIZE (MKQUOTE FORMS) (MKQUOTE INFILES) (MKQUOTE OUTFILE)
             (MKQUOTE VNAME) (MKQUOTE DECS))))) 
(PUT 'ALGOPTEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ALGOPTEVAL 'DEFINED-ON-LINE '246) 
(PUT 'ALGOPTEVAL 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'ALGOPTEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALGOPTEVAL (U)
    (PROG (SU RES INTERN* NARGS)
      (SETQ NARGS 0)
      (SETQ INTERN* *INTERN)
      (SETQ *INTERN 'T)
      (SETQ NARGS (LENGTH U))
      (SETQ U
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL U)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (COND
                                     ((AND (LISTP EL) (EQCAR EL 'LIST)
                                           (ALLSTRING (CADR EL)))
                                      (CDR EL))
                                     (T EL)))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (COND
                             ((AND (LISTP EL) (EQCAR EL 'LIST)
                                   (ALLSTRING (CADR EL)))
                              (CDR EL))
                             (T EL)))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND (LISTP (CAR U)) (NOT (ALLSTRING (CAR U)))
             (NOT (EQCAR (CAR U) 'LIST)))
        (SETQ U (CONS (LIST 'LIST (CAR U)) (CDR U)))))
      (SETQ RES
              (COND
               ((EQUAL NARGS 1)
                (COND
                 ((SETQ SU (ALLSTRING (CAR U)))
                  (SYMOPTIMIZE NIL SU NIL NIL NIL))
                 (T (SYMOPTIMIZE (CAR U) NIL NIL NIL NIL))))
               ((EQUAL NARGS 2)
                (COND
                 ((SETQ SU (ALLSTRING (CADR U)))
                  (SYMOPTIMIZE (CAR U) SU NIL NIL NIL))
                 ((AND (SETQ SU (ALLSTRING (CAR U))) (ATOM (CADR U)))
                  (SYMOPTIMIZE NIL SU NIL (CADR U) NIL))
                 ((ATOM (CADR U)) (SYMOPTIMIZE (CAR U) NIL NIL (CADR U) NIL))
                 (T '**ERROR**)))
               ((AND (EQUAL NARGS 3) (SETQ SU (ALLSTRING (CADR U))))
                (SYMOPTIMIZE (CAR U) SU NIL (CADDR U) NIL))
               (T '**ERROR**)))
      (SETQ *INTERN INTERN*)
      (COND ((EQ RES '**ERROR**) (REDERR "SYNTAX ERROR IN ARGUMENTS ALGOPT"))
            (T
             (RETURN
              (ALGRESULTS1
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL RES)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL)))
                                   (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (EL) (CONS (CADR EL) (CADDR EL))) (CAR EL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'ALGOPT 'PSOPFN 'ALGOPTEVAL) 
(PUT 'ALLSTRING 'NUMBER-OF-ARGS 1) 
(PUT 'ALLSTRING 'DEFINED-ON-LINE '291) 
(PUT 'ALLSTRING 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'ALLSTRING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLSTRING (S)
    (COND ((ATOM S) (COND ((STRINGP S) (LIST S)) (T NIL)))
          ((NOT
            (MEMBER NIL
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL S)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (STRINGP EL)) (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (EL) (STRINGP EL)) (CAR EL)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
           S)
          (T NIL))) 
(GLOBAL '(*ALGPRI *OPTDECS)) 
(SWITCH (LIST 'ALGPRI 'OPTDECS)) 
(SETQ *OPTDECS NIL) 
(PUT 'SYMOPTIMIZE 'NUMBER-OF-ARGS 5) 
(PUT 'SYMOPTIMIZE 'DEFINED-ON-LINE '310) 
(PUT 'SYMOPTIMIZE 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'SYMOPTIMIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SYMOPTIMIZE (FORMS INFILES OUTFILE VNAME DECS)
    (PROG (ALGPRI ECHO FN FORMS1 OPTDECS COMDECS)
      (SETQ ECHO *ECHO)
      (EVAL (LIST 'OFF (MKQUOTE (LIST 'ECHO))))
      (COND (INFILES (SETQ FORMS (APPEND FORMS (FILES2FORMS INFILES)))))
      (SETQ ALGPRI *ALGPRI)
      (SETQ *ECHO ECHO)
      (COND
       (DECS
        (PROGN
         (SETQ OPTDECS *OPTDECS)
         (SETQ *OPTDECS T)
         (COND
          ((OR (SETQ COMDECS (ASSOC 'COMPLEX DECS))
               (SETQ COMDECS (ASSOC 'COMPLEX*16 DECS)))
           (PROGN
            (COND
             ((NOT (FREEOF COMDECS 'I))
              (SETQ FORMS (CONS '(SETQ I (SQRT (MINUS 1))) FORMS))))
            NIL))))))
      (EVAL (LIST 'OFF (MKQUOTE (LIST 'ALGPRI))))
      (COND (VNAME (INAME VNAME)))
      (SETQ FORMS (ANALYSE_FORMS FORMS))
      (SETQ *ALGPRI ALGPRI)
      (PREPROC1 (CONS 'DECLARE DECS))
      (SETQ PREFIXLIST
              (EVAL
               (FORMOPTIMIZE (LIST 'OPTIMIZEFORMS FORMS OUTFILE VNAME) *VARS*
                *MODE)))
      (COND (DECS (SETQ *OPTDECS OPTDECS)))
      (COND
       (*INTERN
        (RETURN
         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
           (SETQ EL PREFIXLIST)
           (COND ((NULL EL) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL) (LIST 'SETQ (CAR EL) (CDR EL)))
                             (CAR EL))
                            NIL)))
          LOOPLABEL
           (SETQ EL (CDR EL))
           (COND ((NULL EL) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (EL) (LIST 'SETQ (CAR EL) (CDR EL))) (CAR EL))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(PUT 'SYMOPTIMIZE 'NUMBER-OF-ARGS 5) 
(PUT 'SYMOPTIMIZE 'DEFINED-ON-LINE '349) 
(PUT 'SYMOPTIMIZE 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'SYMOPTIMIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SYMOPTIMIZE (FORMS INFILES OUTFILE VNAME DECS)
    (PROG (ALGPRI ECHO FN FORMS1 OPTDECS COMDECS)
      (SETQ ECHO *ECHO)
      (EVAL (LIST 'OFF (MKQUOTE (LIST 'ECHO))))
      (COND (INFILES (SETQ FORMS (APPEND FORMS (FILES2FORMS INFILES)))))
      (SETQ ALGPRI *ALGPRI)
      (SETQ *ECHO ECHO)
      (COND (DECS (PROGN (SETQ OPTDECS *OPTDECS) (SETQ *OPTDECS T) NIL)))
      (EVAL (LIST 'OFF (MKQUOTE (LIST 'ALGPRI))))
      (COND (VNAME (INAME VNAME)))
      (SETQ FORMS (ANALYSE_FORMS FORMS))
      (SETQ *ALGPRI ALGPRI)
      (PREPROC1 (CONS 'DECLARE DECS))
      (SETQ PREFIXLIST
              (EVAL
               (FORMOPTIMIZE (LIST 'OPTIMIZEFORMS FORMS OUTFILE VNAME) *VARS*
                *MODE)))
      (COND (DECS (SETQ *OPTDECS OPTDECS)))
      (COND
       (*INTERN
        (RETURN
         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
           (SETQ EL PREFIXLIST)
           (COND ((NULL EL) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL) (LIST 'SETQ (CAR EL) (CDR EL)))
                             (CAR EL))
                            NIL)))
          LOOPLABEL
           (SETQ EL (CDR EL))
           (COND ((NULL EL) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (EL) (LIST 'SETQ (CAR EL) (CDR EL))) (CAR EL))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(PUT 'ANALYSE_FORMS 'NUMBER-OF-ARGS 1) 
(PUT 'ANALYSE_FORMS 'DEFINED-ON-LINE '381) 
(PUT 'ANALYSE_FORMS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'ANALYSE_FORMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANALYSE_FORMS (FORMS)
    (PROG (FN RES FORMS1)
      (COND ((ATOM FORMS) (SETQ FORMS (LIST FORMS)))
            ((AND (LISTP FORMS) (GET (CAR FORMS) 'AVALUE)
                  (MEMBER (CAR (GET (CAR FORMS) 'AVALUE)) '(ARRAY MATRIX)))
             (SETQ FORMS (LIST FORMS)))
            ((AND (LISTP FORMS) (EQCAR FORMS 'LIST)) (SETQ FORMS (CDR FORMS))))
      (SETQ RES
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FORMS)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND
                            ((AND (ATOM F) (EQUAL (CAR (GET F 'AVALUE)) 'LIST))
                             (CDR (REVAL1 F T)))
                            ((AND (LISTP F) (GET (CAR F) 'AVALUE)
                                  (MEMBER (CAR (GET (CAR F) 'AVALUE))
                                          '(ARRAY MATRIX)))
                             (CDR (REVAL1 F T)))
                            ((AND (LISTP F) (EQCAR F 'LIST)) (LIST F))
                            ((AND (LISTP F) (EQCAR F 'EQUAL)
                                  (EQCAR (CADDR F) '*SQ))
                             (LIST (LIST 'EQUAL (CADR F) (SQ2PRE (CADDR F)))))
                            ((AND (LISTP F)
                                  (NOT
                                   (MEMBER (CAR F)
                                           '(EQUAL LSETQ LRSETQ RSETQ SETQ))))
                             (PROGN
                              (SETQ FORMS1
                                      (APPLY
                                       (COND
                                        ((SETQ FN (GET (CAR F) 'PSOPFN)) FN)
                                        (T (CAR F)))
                                       (COND
                                        ((GET (CAR F) 'PSOPFN)
                                         (LIST
                                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ X (CDR F))
                                            (COND ((NULL X) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (X) X)
                                                              (CAR X))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ X (CDR X))
                                            (COND
                                             ((NULL X) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X) X) (CAR X))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                        (T
                                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ X (CDR F))
                                           (COND ((NULL X) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X) X)
                                                             (CAR X))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ X (CDR X))
                                           (COND
                                            ((NULL X) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (X) X) (CAR X))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))))))
                              (COND
                               ((AND (PAIRP FORMS1) (EQCAR FORMS1 'LIST))
                                (CDR FORMS1))
                               (T FORMS1))))
                            (T (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND
                            ((AND (ATOM F) (EQUAL (CAR (GET F 'AVALUE)) 'LIST))
                             (CDR (REVAL1 F T)))
                            ((AND (LISTP F) (GET (CAR F) 'AVALUE)
                                  (MEMBER (CAR (GET (CAR F) 'AVALUE))
                                          '(ARRAY MATRIX)))
                             (CDR (REVAL1 F T)))
                            ((AND (LISTP F) (EQCAR F 'LIST)) (LIST F))
                            ((AND (LISTP F) (EQCAR F 'EQUAL)
                                  (EQCAR (CADDR F) '*SQ))
                             (LIST (LIST 'EQUAL (CADR F) (SQ2PRE (CADDR F)))))
                            ((AND (LISTP F)
                                  (NOT
                                   (MEMBER (CAR F)
                                           '(EQUAL LSETQ LRSETQ RSETQ SETQ))))
                             (PROGN
                              (SETQ FORMS1
                                      (APPLY
                                       (COND
                                        ((SETQ FN (GET (CAR F) 'PSOPFN)) FN)
                                        (T (CAR F)))
                                       (COND
                                        ((GET (CAR F) 'PSOPFN)
                                         (LIST
                                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ X (CDR F))
                                            (COND ((NULL X) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (X) X)
                                                              (CAR X))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ X (CDR X))
                                            (COND
                                             ((NULL X) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X) X) (CAR X))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                        (T
                                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ X (CDR F))
                                           (COND ((NULL X) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (X) X)
                                                             (CAR X))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ X (CDR X))
                                           (COND
                                            ((NULL X) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (X) X) (CAR X))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))))))
                              (COND
                               ((AND (PAIRP FORMS1) (EQCAR FORMS1 'LIST))
                                (CDR FORMS1))
                               (T FORMS1))))
                            (T (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (RETURN
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F RES)
        STARTOVER
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (F)
                    (COND
                     ((AND (LISTP F) (EQCAR F 'LIST)) (ANALYSE_FORMS (CDR F)))
                     (T (LIST F))))
                  (CAR F)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ F (CDR F))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (F)
                    (COND
                     ((AND (LISTP F) (EQCAR F 'LIST)) (ANALYSE_FORMS (CDR F)))
                     (T (LIST F))))
                  (CAR F)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ F (CDR F))
         (GO LOOPLABEL))))) 
(PUT 'XREADFORMS 'NUMBER-OF-ARGS 0) 
(PUT 'XREADFORMS 'DEFINED-ON-LINE '426) 
(PUT 'XREADFORMS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'XREADFORMS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE XREADFORMS NIL
    (PROG (X)
      (SETQ X (XREAD T))
      (COND ((AND (LISTP X) (EQCAR X 'LIST)) (RETURN (FLATTENLIST X)))
            (X (RETURN (LIST X))) (T (RETURN X))))) 
(PUT 'FLATTENLIST 'NUMBER-OF-ARGS 1) 
(PUT 'FLATTENLIST 'DEFINED-ON-LINE '437) 
(PUT 'FLATTENLIST 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'FLATTENLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLATTENLIST (X)
    (COND ((OR (ATOM X) (CONSTP X)) X)
          (T
           (PROGN
            (COND
             ((EQCAR X 'LIST)
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y (CDR X))
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (FLATTENLIST Y)) (CAR Y))
                                      NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (FLATTENLIST Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
             (T X)))))) 
(PUT 'FILES2FORMS 'NUMBER-OF-ARGS 1) 
(PUT 'FILES2FORMS 'DEFINED-ON-LINE '447) 
(PUT 'FILES2FORMS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'FILES2FORMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FILES2FORMS (FLIST)
    (PROG (CH HOLDCH X FORMS)
      (SETQ HOLDCH (RDS NIL))
      (SETQ FORMS NIL)
      (PROG (F)
        (SETQ F FLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ CH (OPEN (MKFIL F) 'INPUT))
            (RDS CH)
            (PROG ()
             WHILELABEL
              (COND ((NOT (SETQ X (XREADFORMS))) (RETURN NIL)))
              (SETQ FORMS (APPEND FORMS X))
              (GO WHILELABEL))
            (RDS HOLDCH)
            (CLOSE CH)))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN FORMS))) 
(PUT 'FORMOPTIMIZE 'NUMBER-OF-ARGS 3) 
(PUT 'FORMOPTIMIZE 'DEFINED-ON-LINE '464) 
(PUT 'FORMOPTIMIZE 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'FORMOPTIMIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMOPTIMIZE (U VARS MODE)
    (CONS (CAR U)
          (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
            (SETQ ARG (CDR U))
            (COND ((NULL ARG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ARG) (FORMOPTIMIZE1 ARG VARS MODE))
                              (CAR ARG))
                             NIL)))
           LOOPLABEL
            (SETQ ARG (CDR ARG))
            (COND ((NULL ARG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (ARG) (FORMOPTIMIZE1 ARG VARS MODE)) (CAR ARG))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'CHOPCHOP 'NUMBER-OF-ARGS 1) 
(PUT 'CHOPCHOP 'DEFINED-ON-LINE '468) 
(PUT 'CHOPCHOP 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'CHOPCHOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHOPCHOP (REP)
    (COND
     ((GREATERP (LENGTH (EXPLODE (ABS (CAR REP)))) !RDPREC)
      (PROG (SGN RESTLIST LASTCHOP EXPPART)
        (SETQ RESTLIST (REVERSE (EXPLODE (ABS (CAR REP)))))
        (SETQ SGN (LESSP (CAR REP) 0))
        (SETQ EXPPART (CDR REP))
        (PROG ()
         WHILELABEL
          (COND ((NOT (GREATERP (LENGTH RESTLIST) !RDPREC)) (RETURN NIL)))
          (PROGN
           (SETQ LASTCHOP (CAR RESTLIST))
           (SETQ RESTLIST (CDR RESTLIST))
           (SETQ EXPPART (PLUS EXPPART 1)))
          (GO WHILELABEL))
        (SETQ RESTLIST (COMPRESS (REVERSE RESTLIST)))
        (COND
         ((GEQ (COMPRESS (LIST LASTCHOP)) 5)
          (SETQ RESTLIST (PLUS RESTLIST 1))))
        (RETURN
         (CONS (COND (SGN (MINUS (TIMES 1 RESTLIST))) (T RESTLIST)) EXPPART))))
     (T REP))) 
(PUT 'FORMOPTIMIZE1 'NUMBER-OF-ARGS 3) 
(PUT 'FORMOPTIMIZE1 'DEFINED-ON-LINE '489) 
(PUT 'FORMOPTIMIZE1 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'FORMOPTIMIZE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMOPTIMIZE1 (U VARS MODE)
    (COND ((CONSTP U) (MKQUOTE U)) ((ATOM U) (MKQUOTE U))
          ((MEMBER (CAR U) '(|:RD:| |:CR:| |:CRN:| |:DN:|))
           (MKQUOTE
            (PROGN
             (SETQ U (CHOPCHOP (CDR U)))
             (DECIMAL2INTERNAL (CAR U) (CDR U)))))
          ((EQ (CAR U) '|:INT:|) (MKQUOTE (CADR U)))
          ((EQCAR U 'EVAL)
           (LIST 'SQ2PRE (LIST 'AEVAL (FORM1 (CADR U) VARS MODE))))
          ((MEMQ (CAR U) '(LSETQ RSETQ LRSETQ))
           (PROG (OP LHS RHS)
             (SETQ OP (CAR U))
             (SETQ LHS (CADR U))
             (SETQ RHS (CADDR U))
             (COND
              ((AND (MEMQ OP '(LSETQ LRSETQ)) (LISTP LHS))
               (SETQ LHS
                       (CONS (CAR LHS)
                             (PROG (S FORALL-RESULT FORALL-ENDPTR)
                               (SETQ S (CDR LHS))
                               (COND ((NULL S) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (S) (LIST 'EVAL S))
                                                 (CAR S))
                                                NIL)))
                              LOOPLABEL
                               (SETQ S (CDR S))
                               (COND ((NULL S) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (S) (LIST 'EVAL S)) (CAR S))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))))
             (COND ((MEMQ OP '(RSETQ LRSETQ)) (SETQ RHS (LIST 'EVAL RHS))))
             (RETURN (FORMOPTIMIZE1 (LIST 'SETQ LHS RHS) VARS MODE))))
          (T
           (CONS 'LIST
                 (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELT U)
                   (COND ((NULL ELT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELT)
                                       (FORMOPTIMIZE1 ELT VARS MODE))
                                     (CAR ELT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELT (CDR ELT))
                   (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (ELT) (FORMOPTIMIZE1 ELT VARS MODE))
                             (CAR ELT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'SQ2PRE 'NUMBER-OF-ARGS 1) 
(PUT 'SQ2PRE 'DEFINED-ON-LINE '522) 
(PUT 'SQ2PRE 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'SQ2PRE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQ2PRE (F)
    (COND ((ATOM F) F) ((AND (LISTP F) (EQCAR F '*SQ)) (PREPSQ (CADR F)))
          (T (PREPSQ F)))) 
(PUT 'OPTIMIZEFORMS 'NUMBER-OF-ARGS 3) 
(PUT 'OPTIMIZEFORMS 'DEFINED-ON-LINE '534) 
(PUT 'OPTIMIZEFORMS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'OPTIMIZEFORMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE OPTIMIZEFORMS (FORMS OUTFILE VNAME)
    (PROG (NOEQUIV DOUBLE PERIOD CH FORT HOLDCH OPTLANG PRIMAT ACINFO INPUTC)
      (SETQ PERIOD *PERIOD)
      (SETQ *PERIOD NIL)
      (SETQ NOEQUIV *NOEQUIV)
      (SETQ *NOEQUIV T)
      (SETQ DOUBLE *DOUBLE)
      (PUT '|:RD:| 'ZEROP '|RD:ZEROP:|)
      (COND ((AND VNAME (NOT (GETD 'NEWSYM))) (INAME VNAME)))
      (COND
       (*FORT
        (PROGN
         (SETQ FORT T)
         (SETQ *FORT NIL)
         (SETQ OPTLANG OPTLANG*)
         (SETQ OPTLANG* 'FORTRAN))))
      (COND
       (OUTFILE
        (PROGN
         (COND
          ((NOT OPTLANG*)
           (PROGN
            (SETQ HOLDCH (WRS NIL))
            (COND
             ((SETQ CH (ASSOC (INTERN OUTFILE) *OUTSTK*)) (SETQ CH (CDR CH)))
             (T (SETQ CH (OPEN (MKFIL OUTFILE) 'OUTPUT))))
            (WRS CH)))
          (T (EVAL (LIST 'GENTRANOUTPUSH (LIST 'QUOTE (LIST OUTFILE)))))))))
      (COND
       (*PRIALL
        (PROGN
         (SETQ PRIMAT *PRIMAT)
         (SETQ ACINFO *ACINFO)
         (SETQ INPUTC *INPUTC)
         (SETQ *PRIMAT (SETQ *ACINFO (SETQ *INPUTC T))))))
      (SETQ PREFIXLIST (CALC FORMS))
      (COND
       (*PRIALL
        (PROGN
         (SETQ *PRIMAT PRIMAT)
         (SETQ *ACINFO ACINFO)
         (SETQ *INPUTC INPUTC))))
      (COND
       (OUTFILE
        (PROGN
         (COND
          ((NOT OPTLANG*)
           (PROGN
            (COND ((OR (NOT *NAT) *AGAIN) (PROGN (PRIN2 ";end;") NIL)))
            (COND
             ((ASSOC (INTERN OUTFILE) *OUTSTK*) (PROGN (TERPRI) (WRS HOLDCH)))
             (T (PROGN (WRS HOLDCH) (CLOSE CH))))))
          (T (EVAL '(GENTRANPOP '(NIL))))))))
      (COND (FORT (PROGN (SETQ *FORT T) (SETQ OPTLANG* OPTLANG))))
      (PUT '|:RD:| 'ZEROP '|RD:ZEROP|)
      (SETQ *DOUBLE DOUBLE)
      (SETQ *NOEQUIV NOEQUIV)
      (SETQ *PERIOD PERIOD)
      (RETURN PREFIXLIST))) 
(PUT 'OPT 'NUMBER-OF-ARGS 1) 
(PUT 'OPT 'DEFINED-ON-LINE '580) 
(PUT 'OPT 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'OPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OPT (FORMS)
    (PROG (SEQ RES FORT OPTLANG)
      (SETQ FORT *FORT)
      (SETQ *FORT NIL)
      (SETQ OPTLANG OPTLANG*)
      (SETQ OPTLANG* GENTRANLANG*)
      (COND ((ATOM FORMS) (SETQ RES FORMS))
            ((EQCAR FORMS 'SETQ)
             (PROGN
              (SETQ RES
                      (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                        (SETQ PR (OPTIMIZEFORMS (LIST FORMS) NIL NIL))
                        (COND ((NULL PR) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (PR)
                                            (LIST 'SETQ (CAR PR) (CDR PR)))
                                          (CAR PR))
                                         NIL)))
                       LOOPLABEL
                        (SETQ PR (CDR PR))
                        (COND ((NULL PR) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR) (LIST 'SETQ (CAR PR) (CDR PR)))
                                  (CAR PR))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (COND ((ONEP (LENGTH RES)) (SETQ RES (CAR RES)))
                    (T (SETQ RES (MKSTMTGP 0 RES))))))
            ((ATOM (CAR FORMS))
             (SETQ RES (CONS (CAR FORMS) (OPT (CDR FORMS)))))
            (T
             (PROGN
              (SETQ SEQ NIL)
              (PROG ()
               WHILELABEL
                (COND
                 ((NOT
                   (AND FORMS (LISTP (CAR FORMS)) (EQCAR (CAR FORMS) 'SETQ)))
                  (RETURN NIL)))
                (PROGN
                 (SETQ SEQ (CONS (CAR FORMS) SEQ))
                 (SETQ FORMS (CDR FORMS)))
                (GO WHILELABEL))
              (COND
               (SEQ
                (PROGN
                 (SETQ SEQ
                         (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                           (SETQ PR (OPTIMIZEFORMS (REVERSE SEQ) NIL NIL))
                           (COND ((NULL PR) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (PR)
                                               (LIST 'SETQ (CAR PR) (CDR PR)))
                                             (CAR PR))
                                            NIL)))
                          LOOPLABEL
                           (SETQ PR (CDR PR))
                           (COND ((NULL PR) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (PR)
                                       (LIST 'SETQ (CAR PR) (CDR PR)))
                                     (CAR PR))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                 (COND
                  ((GREATERP (LENGTH SEQ) 1)
                   (SETQ SEQ (LIST (MKSTMTGP 0 SEQ)))))
                 (SETQ RES (APPEND SEQ (OPT FORMS)))))
               (T (SETQ RES (CONS (OPT (CAR FORMS)) (OPT (CDR FORMS))))))
              NIL)))
      (SETQ OPTLANG* OPTLANG)
      (SETQ *FORT FORT)
      (RETURN RES))) 
(PUT 'INIT 'NUMBER-OF-ARGS 1) 
(PUT 'INIT 'DEFINED-ON-LINE '627) 
(PUT 'INIT 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'INIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INIT (N)
    (PROG (VAR)
      (PROG (Y)
        (SETQ Y ROWMIN)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX Y)) (RETURN NIL)))
        (COND
         ((AND (GETV CODMAT (PLUS MAXVAR Y))
               (NOT
                (NUMBERP (SETQ VAR (GETV (GETV CODMAT (PLUS MAXVAR Y)) 3)))))
          (PROGN
           (REMPROP VAR 'NPCDVAR)
           (REMPROP VAR 'NVARLST)
           (REMPROP VAR 'VARLST+)
           (REMPROP VAR 'VARLST*)
           (REMPROP VAR 'ROWINDEX)
           (REMPROP VAR 'NEX)
           (REMPROP VAR 'INLHS)
           (REMPROP VAR 'ROWOCC)
           (REMPROP VAR 'KVARLST)
           (REMPROP VAR 'ALIAS)
           (REMPROP VAR 'FINALALIAS)
           (REMPROP VAR 'ALIASLIST)
           (REMPROP VAR 'INALIAS)
           NIL)))
        (SETQ Y (PLUS2 Y 1))
        (GO LAB))
      (COND
       ((EQUAL MAXVAR N)
        (PROG (X)
          (SETQ X 0)
         LAB
          (COND ((MINUSP (DIFFERENCE (TIMES 2 N) X)) (RETURN NIL)))
          (PUTV CODMAT X NIL)
          (SETQ X (PLUS2 X 1))
          (GO LAB)))
       (T (SETQ CODMAT (MKVECT (TIMES 2 N)))))
      (COND
       (KVARLST
        (PROG (ITEM)
          (SETQ ITEM KVARLST)
         LAB
          (COND ((NULL ITEM) (RETURN NIL)))
          ((LAMBDA (ITEM)
             (PROGN (REMPROP (CADR ITEM) 'KVARLST) (REMPROP (CADR ITEM) 'NEX)))
           (CAR ITEM))
          (SETQ ITEM (CDR ITEM))
          (GO LAB))))
      (PROG (ITEM)
        (SETQ ITEM '(PLUS MINUS DIFFERENCE TIMES EXPT SQRT))
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM) (REMPROP ITEM 'KVARLST)) (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (SETQ VARLST*
              (SETQ VARLST+
                      (SETQ PREVLST
                              (SETQ KVARLST
                                      (SETQ CODBEXL* (SETQ AVARLST NIL))))))
      (SETQ MALST (SETQ PREPREFIXLIST NIL))
      (SETQ PREFIXLIST NIL)
      (SETQ ROWMAX (MINUS 1))
      (SETQ MAXVAR N)
      (SETQ ROWMIN 0)
      (SETQ OPS (LIST 0 0 0 0)))) 
(PUT 'CALC 'NUMBER-OF-ARGS 1) 
(PUT 'CALC 'DEFINED-ON-LINE '670) 
(PUT 'CALC 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'CALC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CALC (FORMS)
    (PROG (FIL)
      (INIT 200)
      (SETQ PREFIXLIST (SETQ RHSALIASES NIL))
      (SETQ FORMS (PREREMDEP FORMS))
      (PROG (ITEM)
        (SETQ ITEM FORMS)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (SETQ PREFIXLIST (FFVAR! (CADR ITEM) (CADDR ITEM) PREFIXLIST)))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (SETQ PREPREFIXLIST (SSETVARS PREPREFIXLIST))
      (SETQ FIL (WRS NIL))
      (COND (*PRIMAT (PRIMAT)))
      (COND (*ACINFO (COUNTNOP (REVERSE PREFIXLIST) 'INPUT)))
      (OPTIMIZELOOP)
      (TERPRI)
      (WRS FIL)
      (SETQ PREFIXLIST (MAKEPREFIXL PREPREFIXLIST NIL))
      (COND (*GENTRANOPT (TYPEALL PREFIXLIST))
            ((NOT *INTERN) (PRIRESULT PREFIXLIST)))
      (SETQ FIL (WRS NIL))
      (COND ((GETD 'NEWSYM) (REMD 'NEWSYM)))
      (COND (*ACINFO (PROGN (COUNTNOP (REVERSE PREFIXLIST) 'OUTPUT) (TERPRI))))
      (COND
       (*PRIMAT
        (PROGN
         (PROG (X)
           (SETQ X ROWMIN)
          LAB
           (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
           (COND
            ((OR (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 1))
                 (EQUAL (GETV (GETV CODMAT (PLUS MAXVAR X)) 3) (MINUS 2)))
             (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 NIL))
            (T (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)))
           (SETQ X (PLUS2 X 1))
           (GO LAB))
         (PRIMAT)
         NIL)))
      (WRS FIL)
      (RETURN PREFIXLIST))) 
(PUT 'PPRINTF 'NUMBER-OF-ARGS 2) 
(PUT 'PPRINTF 'DEFINED-ON-LINE '715) 
(PUT 'PPRINTF 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'PPRINTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PPRINTF (EX NEX)
    (PROG (S FIL NAT)
      (TERPRI)
      (SETQ FIL (WRS NIL))
      (COND ((NOT *NAT) (PROGN (SETQ NAT *NAT) (SETQ S (SETQ *NAT T)))))
      (ASSGNPRI EX (LIST NEX) 'LAST)
      (WRS FIL)
      (COND (S (SETQ *NAT NAT))))) 
(PUT 'OPTIMIZELOOP 'NUMBER-OF-ARGS 0) 
(PUT 'OPTIMIZELOOP 'DEFINED-ON-LINE '730) 
(PUT 'OPTIMIZELOOP 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'OPTIMIZELOOP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE OPTIMIZELOOP NIL
    (PROG (B1 B2 B3 B4)
      (PROG ()
       REPEATLABEL
        (PROGN
         (EXTBRSEA)
         (SETQ B1 (IMPROVELAYOUT))
         (SETQ B2 (TCHSCHEME))
         (SETQ B3 (CODFAC))
         (SETQ B4 (SEARCHCSEQUOTIENTS))
         NIL)
        (COND ((NOT (NOT (OR B1 B2 B3 B4))) (GO REPEATLABEL)))))) 
(PUT 'COUNTNOP 'NUMBER-OF-ARGS 2) 
(PUT 'COUNTNOP 'DEFINED-ON-LINE '768) 
(PUT 'COUNTNOP 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'COUNTNOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COUNTNOP (PREFIXLST IO)
    (PROG (TOTCTS)
      (SETQ TOTCTS '(0 0 0 0 0 0))
      (PROG (ITEM)
        (SETQ ITEM PREFIXLST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (COND
             ((PAIRP (CAR ITEM)) (SETQ TOTCTS (COUNTS (CAR ITEM) TOTCTS NIL))))
            (SETQ TOTCTS (COUNTS (CDR ITEM) TOTCTS NIL))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (TERPRI)
      (COND
       ((EQ IO 'INPUT)
        (PROGN (PRIN2 "Number of operations in the input is: ") NIL))
       (T (PROGN (PRIN2 "Number of operations after optimization is:") NIL)))
      (TERPRI)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of (+/-) operations      : ")
       (PRIN2 (CAR TOTCTS))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of unary - operations    : ")
       (PRIN2 (CADR TOTCTS))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of * operations          : ")
       (PRIN2 (CADDR TOTCTS))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of integer ^ operations  : ")
       (PRIN2 (CADDDR TOTCTS))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of / operations          : ")
       (PRIN2 (CAR (CDDDDR TOTCTS)))
       NIL)
      (TERPRI)
      (PROGN
       (PRIN2 "Number of function applications : ")
       (PRIN2 (CAR (REVERSE TOTCTS)))
       NIL)
      (TERPRI))) 
(PUT 'COUNTS 'NUMBER-OF-ARGS 3) 
(PUT 'COUNTS 'DEFINED-ON-LINE '797) 
(PUT 'COUNTS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'COUNTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COUNTS (EXPRESSION LOCS ROOT)
    (PROG (N+ N- N* N^ N/ NF TLOCS LOPER OPERANDS)
      (COND ((OR (IDP EXPRESSION) (CONSTP EXPRESSION)) (SETQ TLOCS LOCS))
            (T
             (PROGN
              (SETQ N+ (CAR LOCS))
              (SETQ N- (CADR LOCS))
              (SETQ N* (CADDR LOCS))
              (SETQ N^ (CADDDR LOCS))
              (SETQ N/ (CAR (CDDDDR LOCS)))
              (SETQ NF (CAR (REVERSE LOCS)))
              (SETQ LOPER (CAR EXPRESSION))
              (SETQ OPERANDS (CDR EXPRESSION))
              (COND
               ((MEMQ LOPER '(PLUS DIFFERENCE))
                (SETQ N+ (PLUS (DIFFERENCE (LENGTH OPERANDS) 1) N+)))
               ((EQ LOPER 'MINUS)
                (COND ((NEQ ROOT 'PLUS) (SETQ N- (PLUS 1 N-)))))
               ((EQ LOPER 'TIMES)
                (SETQ N* (PLUS (DIFFERENCE (LENGTH OPERANDS) 1) N*)))
               ((EQ LOPER 'EXPT)
                (COND ((INTEGERP (CADR OPERANDS)) (SETQ N^ (PLUS 1 N^)))
                      (T (SETQ NF (PLUS 1 NF)))))
               ((EQ LOPER 'QUOTIENT) (SETQ N/ (PLUS 1 N/)))
               ((NOT (SUBSCRIPTEDVARP LOPER)) (SETQ NF (PLUS 1 NF))))
              (SETQ TLOCS (LIST N+ N- N* N^ N/ NF))
              (PROG (OP)
                (SETQ OP OPERANDS)
               LAB
                (COND ((NULL OP) (RETURN NIL)))
                ((LAMBDA (OP) (SETQ TLOCS (COUNTS OP TLOCS LOPER))) (CAR OP))
                (SETQ OP (CDR OP))
                (GO LAB)))))
      (RETURN TLOCS))) 
(PUT 'COMPLEX-I-INIT-STATEMENT 'NUMBER-OF-ARGS 1) 
(PUT 'COMPLEX-I-INIT-STATEMENT 'DEFINED-ON-LINE '835) 
(PUT 'COMPLEX-I-INIT-STATEMENT 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'COMPLEX-I-INIT-STATEMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPLEX-I-INIT-STATEMENT (ST)
    (PROG (TL RES)
      (SETQ TL (FORMTYPELISTS (SYMTABGET '*MAIN* '*DECS*)))
      (PROG (EL)
        (SETQ EL TL)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (COND
             ((AND
               (MEMBER (CAR EL)
                       '(COMPLEX |IMPLICIT COMPLEX| |IMPLICIT COMPLEX*16|))
               (MEMBER 'I EL))
              (SETQ RES
                      (COND
                       (*DOUBLE
                        (COND (ST "i=(0.0D0, 1.0D0)")
                              (T '((LITERAL TAB* "I=(0.0D0, 1.0D0)" CR*)))))
                       (ST "i=(0.0, 1.0)")
                       (T '((LITERAL TAB* "I=(0.0, 1.0)" CR*)))))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN RES))) 
(PUT 'PRIRESULT 'NUMBER-OF-ARGS 1) 
(PUT 'PRIRESULT 'DEFINED-ON-LINE '855) 
(PUT 'PRIRESULT 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'PRIRESULT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIRESULT (PREFIXLIST)
    (PROG (PFL NAT ISTAT)
      (COND (*OPTDECS (TYPEALL PREFIXLIST)))
      (COND
       (OPTLANG*
        (PROGN
         (COND ((NULL (ASSOC 'E PREFIXLIST)) (SYMTABREM NIL 'E)))
         (SETQ PFL
                 (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                   (SETQ PR PREFIXLIST)
                   (COND ((NULL PR) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (PR)
                                       (LIST 'SETQ (CAR PR)
                                             (LISPCODEEXP (CDR PR) *PERIOD)))
                                     (CAR PR))
                                    NIL)))
                  LOOPLABEL
                   (SETQ PR (CDR PR))
                   (COND ((NULL PR) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (PR)
                               (LIST 'SETQ (CAR PR)
                                     (LISPCODEEXP (CDR PR) *PERIOD)))
                             (CAR PR))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          ((SETQ ISTAT (COMPLEX-I-INIT-STATEMENT NIL))
           (SETQ PFL (APPEND ISTAT PFL))))
         (SETQ PFL (LIST (MKSTMTGP 0 PFL)))
         (APPLY1 (GET OPTLANG* 'FORMATTER)
                 (APPLY1 (GET OPTLANG* 'CODEGEN) PFL))
         NIL))
       (*PREFIX
        (PROGN
         (PROGN (PRIN2 "Prefixlist:=") NIL)
         (TERPRI)
         (PRETTYPRINT PREFIXLIST)))
       (T
        (PROGN
         (COND (*OPTDECS (PRINTDECS)))
         (COND
          ((SETQ ISTAT (COMPLEX-I-INIT-STATEMENT 'T))
           (PROGN (PROGN (PRIN2 (CADDAR ISTAT)) NIL) (TERPRI))))
         (COND
          ((NOT *AGAIN)
           (PROG (ITEM)
             (SETQ ITEM PREFIXLIST)
            LAB
             (COND ((NULL ITEM) (RETURN NIL)))
             ((LAMBDA (ITEM) (ASSGNPRI (CDR ITEM) (LIST (CAR ITEM)) 'LAST))
              (CAR ITEM))
             (SETQ ITEM (CDR ITEM))
             (GO LAB)))
          (T
           (PROGN
            (SETQ NAT *NAT)
            (SETQ *NAT NIL)
            (ASSGNPRI
             (APPEND (LIST 'LIST)
                     (PROG (ITEM FORALL-RESULT FORALL-ENDPTR)
                       (SETQ ITEM PREFIXLIST)
                       (COND ((NULL ITEM) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (ITEM)
                                           (LIST 'SETQ (CAR ITEM) (CDR ITEM)))
                                         (CAR ITEM))
                                        NIL)))
                      LOOPLABEL
                       (SETQ ITEM (CDR ITEM))
                       (COND ((NULL ITEM) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ITEM)
                                   (LIST 'SETQ (CAR ITEM) (CDR ITEM)))
                                 (CAR ITEM))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             NIL 'LAST)
            (SETQ *NAT NAT)
            (TERPRI)
            NIL)))))))) 
(PUT 'PRINTDECS 'NUMBER-OF-ARGS 0) 
(PUT 'PRINTDECS 'DEFINED-ON-LINE '897) 
(PUT 'PRINTDECS 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'PRINTDECS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PRINTDECS NIL
    (PROG (TYP)
      (TERPRI* T)
      (PROG (TYPELIST)
        (SETQ TYPELIST (FORMTYPELISTS (SYMTABGET '*MAIN* '*DECS*)))
       LAB
        (COND ((NULL TYPELIST) (RETURN NIL)))
        ((LAMBDA (TYPELIST)
           (PROGN
            (COND
             (*DOUBLE
              (PROGN
               (SETQ TYP
                       (ASSOC (CAR TYPELIST)
                              '((REAL . |DOUBLE PRECISION|)
                                (COMPLEX . COMPLEX*16)
                                (|IMPLICIT REAL| . |IMPLICIT DOUBLE PRECISION|)
                                (|IMPLICIT COMPLEX| . |IMPLICIT COMPLEX*16|))))
               (SETQ TYP (COND ((NULL TYP) (CAR TYPELIST)) (T (CDR TYP))))))
             (T (SETQ TYP (CAR TYPELIST))))
            (PRIN2* TYP)
            (PRIN2* " ")
            (INPRINT '*COMMA* 0 (CDR TYPELIST))
            (TERPRI* T)))
         (CAR TYPELIST))
        (SETQ TYPELIST (CDR TYPELIST))
        (GO LAB)))) 
(GLOBAL '(*FTCH)) 
(SWITCH (LIST 'FTCH)) 
(SETQ *FTCH 'T) 
(PUT 'MAKEPREFIXL 'NUMBER-OF-ARGS 2) 
(PUT 'MAKEPREFIXL 'DEFINED-ON-LINE '925) 
(PUT 'MAKEPREFIXL 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'MAKEPREFIXL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKEPREFIXL (PP PREFIXLIST)
    (PROG (B KVL NEX XX)
      (COND ((NOT *AGAIN) (PREPFINALPLST)))
      (PROG (X)
        (SETQ X 0)
       LAB
        (COND ((MINUSP (DIFFERENCE ROWMAX X)) (RETURN NIL)))
        (PUTV (GETV CODMAT (PLUS MAXVAR X)) 0 T)
        (SETQ X (PLUS2 X 1))
        (GO LAB))
      (SETQ KVL KVARLST)
      (PROG (BEX)
        (SETQ BEX (REVERSE CODBEXL*))
       LAB
        (COND ((NULL BEX) (RETURN NIL)))
        ((LAMBDA (BEX)
           (PROGN
            (COND ((NUMBERP BEX) (SETQ PREFIXLIST (PRFEXP BEX PREFIXLIST)))
                  (T (SETQ PREFIXLIST (PRFKVAR BEX PREFIXLIST))))
            NIL))
         (CAR BEX))
        (SETQ BEX (CDR BEX))
        (GO LAB))
      (SETQ KVARLST KVL)
      (SETQ PREFIXLIST (REVERSE PREFIXLIST))
      (COND
       ((OR *OPTDECS *GENTRANOPT)
        (SETQ PREFIXLIST (REMOVEARRAYSUBSTITUTES PREFIXLIST))))
      (SETQ PREFIXLIST (CLEANUPPREFIXLIST PREFIXLIST))
      (COND (*SIDREL (SETQ PREFIXLIST (EVALPARTPREFIXLIST PREFIXLIST))))
      (COND (*AGAIN (SETQ PREFIXLIST (SAVECSEINFO PREFIXLIST))))
      (RETURN PREFIXLIST))) 
(GLOBAL '(*MIN-EXPR-LENGTH*)) 
(SETQ *MIN-EXPR-LENGTH* NIL) 
(PUT 'PREPFINALPLST 'NUMBER-OF-ARGS 0) 
(PUT 'PREPFINALPLST 'DEFINED-ON-LINE '965) 
(PUT 'PREPFINALPLST 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'PREPFINALPLST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PREPFINALPLST NIL
    (PROG (N)
      (COND
       ((OR *VECTORC *SIDREL (NOT *FTCH) (NOT (NULL MIN-EXPR-LENGTH*)))
        (CODGCD))
       (T
        (PROGN
         (PROG ()
          REPEATLABEL
           (PROGN
            (SETQ N ROWMAX)
            (POWEROFSUMS)
            (REMREPMULTVARS)
            (UPDATEMONOMIALS)
            (CODGCD)
            (COND ((NOT (EQUAL N ROWMAX)) (OPTIMIZELOOP))))
           (COND ((NOT (EQUAL N ROWMAX)) (GO REPEATLABEL))))
         (PREPPOWLS))))
      (COND ((AND (NOT *FTCH) (EQUAL OPTLANG* 'C)) (PREPPOWLS))))) 
(PUT 'SAVECSEINFO 'NUMBER-OF-ARGS 1) 
(PUT 'SAVECSEINFO 'DEFINED-ON-LINE '1005) 
(PUT 'SAVECSEINFO 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'SAVECSEINFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SAVECSEINFO (PREFIXLIST)
    (PROG (CSES GSYM BINF)
      (PROG (ITEM)
        (SETQ ITEM PREFIXLIST)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (COND
            ((AND (PAIRP ITEM) (FLAGP (CAR ITEM) 'NEWSYM))
             (SETQ CSES (CONS (CAR ITEM) CSES)))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (COND
       ((PAIRP CSES)
        (COND ((CDR CSES) (SETQ CSES (CONS 'PLUS CSES)))
              (T (SETQ CSES (CAR CSES))))))
      (SETQ PREFIXLIST (CONS (CONS 'CSES CSES) PREFIXLIST))
      (RETURN
       (COND (CSES (CONS (CONS 'GSYM (FNEWSYM)) PREFIXLIST))
             (T (CONS (CONS 'GSYM (GENSYM)) PREFIXLIST)))))) 
(FLAG '(INAME) 'OPFN) 
(PUT 'INAME 'NUMBER-OF-ARGS 1) 
(PUT 'INAME 'DEFINED-ON-LINE '1034) 
(PUT 'INAME 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'INAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INAME (NM)
    (PROG (DIGITL DLST NB DG INITNAME)
      (SETQ DIGITL
              '((|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
                (|7| . 7) (|8| . 8) (|9| . 9) (|0| . 0)))
      (SETQ CNAME* NIL)
      (SETQ DLST (REVERSE (EXPLODE NM)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((AND (SETQ DG (ASSOC (CAR DLST) DIGITL))
                (NUMBERP (SETQ DG (CDR DG))))
           (PROGN (SETQ DLST (CDR DLST)) (SETQ NB (CONS DG NB))))
          (T
           (PROGN
            (SETQ CNAME* (REVERSE DLST))
            (SETQ CINDEX* 0)
            (SETQ DG (LENGTH NB))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE DG I)) (RETURN NIL)))
              (PROGN
               (SETQ CINDEX* (PLUS (TIMES 10 CINDEX*) (CAR NB)))
               (SETQ NB (CDR NB)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))))))
        (COND ((NOT (OR CNAME* (NULL DLST))) (GO REPEATLABEL))))
      (COND ((NOT (GETD 'NEWSYM)) (MOVD 'NEWSYM 'NEWSYM1)))
      (SETQ INITNAME (NEWSYM))
      (SETQ CINDEX* (DIFFERENCE CINDEX* 1)))) 
(PUT 'MOVD 'NUMBER-OF-ARGS 2) 
(PUT 'MOVD 'DEFINED-ON-LINE '1068) 
(PUT 'MOVD 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'MOVD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MOVD (TOD FROMD)
    (PROG (S) (SETQ S (GETD FROMD)) (PUTD TOD (CAR S) (CDR S)))) 
(PUT 'NEWSYM1 'NUMBER-OF-ARGS 0) 
(PUT 'NEWSYM1 'DEFINED-ON-LINE '1077) 
(PUT 'NEWSYM1 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'NEWSYM1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEWSYM1 NIL
    (PROG (X)
      (SETQ X (EXPLODE CINDEX*))
      (SETQ CINDEX* (PLUS CINDEX* 1))
      (RETURN (COMPRESS (APPEND CNAME* X))))) 
(PUT 'FNEWSYM 'NUMBER-OF-ARGS 0) 
(PUT 'FNEWSYM 'DEFINED-ON-LINE '1089) 
(PUT 'FNEWSYM 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'FNEWSYM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FNEWSYM NIL
    (PROG (X)
      (COND ((GETD 'NEWSYM) (SETQ X (NEWSYM)))
            (T
             (PROGN
              (SETQ X (GENSYM))
              (SETQ X
                      (COMPRESS
                       (APPEND (EXPLODE (LETTERPART X))
                               (EXPLODE (DIGITPART X))))))))
      (SETQ X (INTERN X))
      (FLAG (LIST X) 'NEWSYM)
      (RETURN X))) 
(PUT 'LETTERPART 'NUMBER-OF-ARGS 1) 
(PUT 'LETTERPART 'DEFINED-ON-LINE '1102) 
(PUT 'LETTERPART 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'LETTERPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LETTERPART (NAME)
    (PROG (DIGITL LETTERS EL)
      (SETQ DIGITL
              '((|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
                (|7| . 7) (|8| . 8) (|9| . 9) (|0| . 0)))
      (SETQ LETTERS (REVERSE (EXPLODE NAME)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (SETQ EL (ASSOC (CAR LETTERS) DIGITL)) (NUMBERP (CDR EL))))
          (RETURN NIL)))
        (PROGN (SETQ LETTERS (CDR LETTERS)))
        (GO WHILELABEL))
      (RETURN (INTERN (COMPRESS (REVERSE LETTERS)))))) 
(PUT 'DIGITPART 'NUMBER-OF-ARGS 1) 
(PUT 'DIGITPART 'DEFINED-ON-LINE '1115) 
(PUT 'DIGITPART 'DEFINED-IN-FILE 'SCOPE/CODCTL.RED) 
(PUT 'DIGITPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIGITPART (NAME)
    (PROG (DIGITL NB DG DLST)
      (SETQ DIGITL
              '((|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
                (|7| . 7) (|8| . 8) (|9| . 9) (|0| . 0)))
      (SETQ DLST (REVERSE (EXPLODE NAME)))
      (SETQ NB NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (SETQ DG (ASSOC (CAR DLST) DIGITL))
                (NUMBERP (SETQ DG (CDR DG)))))
          (RETURN NIL)))
        (PROGN (SETQ DLST (CDR DLST)) (SETQ NB (CONS DG NB)))
        (GO WHILELABEL))
      (SETQ DG 0)
      (PROG (DIGIT)
        (SETQ DIGIT NB)
       LAB
        (COND ((NULL DIGIT) (RETURN NIL)))
        ((LAMBDA (DIGIT) (SETQ DG (PLUS (TIMES 10 DG) DIGIT))) (CAR DIGIT))
        (SETQ DIGIT (CDR DIGIT))
        (GO LAB))
      (RETURN DG))) 
(ENDMODULE) 