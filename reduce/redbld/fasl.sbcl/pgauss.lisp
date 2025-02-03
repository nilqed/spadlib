(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID '(PG_RCSID* PG_COPYRIGHT*)) 
(SETQ PG_RCSID* "$Id: pgauss.red 6668 2023-12-29 14:53:18Z arthurcnorman $") 
(SETQ PG_COPYRIGHT* "(c) 1998 A. Dolzmann, A. Seidl, T. Sturm, 2012 T. Sturm") 
(MODULE (LIST 'GAUSS)) 
(CREATE-PACKAGE '(PGAUSS PGCOND PGSM) NIL) 
(BOTHTIMES (LOAD-PACKAGE 'REDLOG)) 
(LOADTIME (RL_SET '(OFSF))) 
(EXPORTS (LIST 'PG_GAUSS 'PG_PRINT 'PG_GAUSS-SYM 'PG_PRINT-SYM)) 
(AEVAL (OPERATOR (LIST 'AND))) 
(AEVAL (OPERATOR (LIST 'NOT))) 
((LAMBDA (*MSG) (AEVAL (OPERATOR (LIST 'ARBCOMPLEX)))) NIL) 
(FLAG '(PG_GAUSS) 'OPFN) 
(FLAG '(PG_PRINT) 'OPFN) 
(FLAG '(PG_C) 'OPFN) 
(FLAG '(PG_SWITCHINFO) 'OPFN) 
(SWITCH (LIST 'PGGENERIC)) 
(SWITCH (LIST 'PGINFOSM)) 
(SWITCH (LIST 'PGINFOSOL)) 
(SWITCH (LIST 'PGSOURCEDIRECT)) 
(SWITCH (LIST 'PGSPSIMPL)) 
(SWITCH (LIST 'PGSPQE)) 
(SWITCH (LIST 'PGVERBOSE)) 
(SWITCH (LIST 'PGAFTERMATH)) 
(SWITCH (LIST 'NOPROPZERO)) 
(SWITCH (LIST 'PGNOARBCOMPLEX)) 
(FLUID
 '(*PGGENERIC *PGINFOSM *PGINFOSOL *PGSOURCEDIRECT *PGSPSIMPL *PGSPQE
   *PGVERBOSE *PGAFTERMATH *NOPROPZERO)) 
(PUT 'PG_GAUSS 'NUMBER-OF-ARGS 2) 
(PUT 'PG_GAUSS 'DEFINED-ON-LINE '147) 
(PUT 'PG_GAUSS 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_GAUSS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PG_GAUSS (EQTL VARL)
    (PROG (ELVL)
      (SETQ ELVL (PG_A2S EQTL VARL))
      (RETURN (PG_S2A (PG_GAUSS-SYM (CAR ELVL) (CDR ELVL)))))) 
(PUT 'PG_A2S 'NUMBER-OF-ARGS 2) 
(PUT 'PG_A2S 'DEFINED-ON-LINE '158) 
(PUT 'PG_A2S 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_A2S 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PG_A2S (EQTL VARL) (CONS (CDR EQTL) (CDR VARL))) 
(PUT 'PG_S2A 'NUMBER-OF-ARGS 1) 
(PUT 'PG_S2A 'DEFINED-ON-LINE '165) 
(PUT 'PG_S2A 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_S2A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_S2A (SOLSET)
    (PROG (NCOND NVALS)
      (RETURN
       (CONS 'LIST
             (PROG (S FORALL-RESULT FORALL-ENDPTR)
               (SETQ S SOLSET)
               (COND ((NULL S) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (S)
                                   (PROGN
                                    (SETQ NCOND
                                            (PROG (C FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ C (CAR S))
                                              (COND ((NULL C) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (C)
                                                                  (RL_PREPFOF
                                                                   C))
                                                                (CAR C))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ C (CDR C))
                                              (COND
                                               ((NULL C)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (C)
                                                          (RL_PREPFOF C))
                                                        (CAR C))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))
                                    (SETQ NVALS
                                            (PROG (V FORALL-RESULT
                                                   FORALL-ENDPTR)
                                              (SETQ V (CDR S))
                                              (COND ((NULL V) (RETURN NIL)))
                                              (SETQ FORALL-RESULT
                                                      (SETQ FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (V)
                                                                  (EQT_MAKEEQT
                                                                   (CAR V)
                                                                   (PREPSQ
                                                                    (CDR V))))
                                                                (CAR V))
                                                               NIL)))
                                             LOOPLABEL
                                              (SETQ V (CDR V))
                                              (COND
                                               ((NULL V)
                                                (RETURN FORALL-RESULT)))
                                              (RPLACD FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (V)
                                                          (EQT_MAKEEQT (CAR V)
                                                           (PREPSQ (CDR V))))
                                                        (CAR V))
                                                       NIL))
                                              (SETQ FORALL-ENDPTR
                                                      (CDR FORALL-ENDPTR))
                                              (GO LOOPLABEL)))
                                    (LIST 'LIST
                                          (COND
                                           ((CAR S)
                                            (COND ((CDR S) (CONS 'AND NCOND))
                                                  (T NCOND)))
                                           (T 'TRUE))
                                          (CONS 'LIST NVALS))))
                                 (CAR S))
                                NIL)))
              LOOPLABEL
               (SETQ S (CDR S))
               (COND ((NULL S) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ NCOND
                                    (PROG (C FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ C (CAR S))
                                      (COND ((NULL C) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (C)
                                                          (RL_PREPFOF C))
                                                        (CAR C))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ C (CDR C))
                                      (COND ((NULL C) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (C) (RL_PREPFOF C))
                                                (CAR C))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (SETQ NVALS
                                    (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ V (CDR S))
                                      (COND ((NULL V) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (V)
                                                          (EQT_MAKEEQT (CAR V)
                                                           (PREPSQ (CDR V))))
                                                        (CAR V))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ V (CDR V))
                                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (V)
                                                  (EQT_MAKEEQT (CAR V)
                                                   (PREPSQ (CDR V))))
                                                (CAR V))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                            (LIST 'LIST
                                  (COND
                                   ((CAR S)
                                    (COND ((CDR S) (CONS 'AND NCOND))
                                          (T NCOND)))
                                   (T 'TRUE))
                                  (CONS 'LIST NVALS))))
                         (CAR S))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'PG_GAUSS-SYM 'NUMBER-OF-ARGS 2) 
(PUT 'PG_GAUSS-SYM 'DEFINED-ON-LINE '184) 
(PUT 'PG_GAUSS-SYM 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_GAUSS-SYM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PG_GAUSS-SYM (EL VL)
    (PROG (SM SOLUTIONS)
      (SETQ SM (SM_CREATE EL VL))
      (COND ((OR *PGINFOSM *PGVERBOSE) (PROGN (PRIN2T "") (SM_PRINT SM) NIL)))
      (SETQ SOLUTIONS (PG_KERN (CSM_CREATE NIL (SM_COPY SM 0 0))))
      (COND
       ((NOT *PGSOURCEDIRECT) (SETQ SOLUTIONS (PG_SOLREVERSIP SOLUTIONS))))
      (COND (*PGINFOSOL (PG_PRINT-SYM SOLUTIONS)))
      (RETURN (COND ((NOT (OR *PGAFTERMATH *PGINFOSOL)) SOLUTIONS))))) 
(PUT 'PG_SOLREVERSIP 'NUMBER-OF-ARGS 1) 
(PUT 'PG_SOLREVERSIP 'DEFINED-ON-LINE '205) 
(PUT 'PG_SOLREVERSIP 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_SOLREVERSIP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_SOLREVERSIP (SL)
    (PROGN
     (PROG (S)
       (SETQ S SL)
      LAB
       (COND ((NULL S) (RETURN NIL)))
       ((LAMBDA (S) (SETCAR S (REVERSE (CAR S)))) (CAR S))
       (SETQ S (CDR S))
       (GO LAB))
     (REVERSIP SL))) 
(PUT 'PG_PRINT-SYM 'NUMBER-OF-ARGS 1) 
(PUT 'PG_PRINT-SYM 'DEFINED-ON-LINE '215) 
(PUT 'PG_PRINT-SYM 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_PRINT-SYM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_PRINT-SYM (SOLSET)
    (PROG (TMP I NUMOFFALSE)
      (SETQ I 0)
      (SETQ NUMOFFALSE 0)
      (PROG (SOL)
        (SETQ SOL SOLSET)
       LAB
        (COND ((NULL SOL) (RETURN NIL)))
        ((LAMBDA (SOL)
           (PROGN
            (PRIN2T "")
            (SETQ I (PLUS I 1))
            (PRIN2 "SOLUTION ")
            (PRIN2T I)
            (PRIN2T "")
            (PRIN2 "Conditions: ")
            (COND_PRIN2L (CAR SOL))
            (PRIN2 "simpl: these conditions are equiv. to")
            (COND_PRIN2 (COND_SIMPL (CAR SOL)))
            (PRIN2
             "qe: the existentional closure of these conditions is equiv. to")
            (COND_PRIN2 (SETQ TMP (COND_QE (CAR SOL))))
            (COND ((EQ TMP 'FALSE) (SETQ NUMOFFALSE (PLUS NUMOFFALSE 1))))
            (PRIN2T "")
            (PRIN2 "Values:     ")
            (PROG (VAL)
              (SETQ VAL (CDR SOL))
             LAB
              (COND ((NULL VAL) (RETURN NIL)))
              ((LAMBDA (VAL)
                 (MATHPRINT (EQT_MAKEEQT (CAR VAL) (PREPSQ (CDR VAL)))))
               (CAR VAL))
              (SETQ VAL (CDR VAL))
              (GO LAB))
            NIL))
         (CAR SOL))
        (SETQ SOL (CDR SOL))
        (GO LAB))
      (PRIN2 "Number of unsatisfiabel conditions: ")
      (PRIN2T NUMOFFALSE))) 
(PUT 'PG_KERN 'NUMBER-OF-ARGS 1) 
(PUT 'PG_KERN 'DEFINED-ON-LINE '241) 
(PUT 'PG_KERN 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_KERN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_KERN (CSM)
    (PROG (SOLUTIONS CSOLS LC CASE PREMISES SM COND WORKCOND)
      (SETQ SM (CSM_GETSM CSM))
      (SETQ PREMISES (CSM_GETC CSM))
      (SETQ CASE (SM_GETCASE SM))
      (COND
       (*PGVERBOSE (PROGN (PRIN2 "--entering p&b. case: ") (PRIN2T CASE))))
      (COND
       ((SETQ LC (SM_REDCOEFFICIENT PREMISES SM))
        (PROGN
         (SM_SWAP SM (CAR LC) (CDR LC))
         (COND
          (*PGVERBOSE
           (PROGN
            (PRIN2T "--p&b, red, have a look at our new system matrix")
            (SM_PRINT SM))))
         (RETURN (APPLY CASE (LIST CSM)))))
       ((NOT *PGGENERIC)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (SETQ LC (SM_NONGREENCOEFFICIENT PREMISES SM)))
             (RETURN NIL)))
           (PROGN
            (SM_SWAP SM (CAR LC) (CDR LC))
            (COND
             (*PGVERBOSE
              (PROGN
               (PRIN2T "--p&b, nongreen, have a look at our new system matrix")
               (SM_PRINT SM))))
            (SETQ WORKCOND
                    (CONS (COND_0MKUNEQT (CAR (SM_GET SM 0 0))) PREMISES))
            (COND (*PGVERBOSE (PRIN2T "--p&b: now case is applied")))
            (SETQ CSOLS
                    (APPLY CASE (LIST (CSM_CREATE WORKCOND (SM_COPY SM 0 0)))))
            (SETQ SOLUTIONS (NCONC CSOLS SOLUTIONS))
            (SETQ PREMISES (CONS (COND_0MKEQT (CAR (SM_GET SM 0 0))) PREMISES))
            (COND ((NOT *NOPROPZERO) (SM_PUT SM 0 0 (SIMP 0)))))
           (GO WHILELABEL))
         (COND
          (*PGVERBOSE
           (PROGN
            (PRIN2T "--have a look at our poor system matrix")
            (SM_PRINT SM))))
         (SETQ CSOLS (PG_NULLCASE (CSM_CREATE PREMISES SM)))
         NIL
         (SETQ SOLUTIONS (NCONC CSOLS SOLUTIONS))
         (RETURN SOLUTIONS)))
       ((SETQ LC (SM_NONGREENCOEFFICIENT PREMISES SM))
        (PROGN
         (SM_SWAP SM (CAR LC) (CDR LC))
         (COND
          (*PGVERBOSE
           (PROGN
            (PRIN2T "--have a look at our new system matrix")
            (SM_PRINT SM))))
         (SETQ WORKCOND
                 (CONS (COND_0MKUNEQT (CAR (SM_GET SM 0 0))) (CSM_GETC CSM)))
         (SETQ CSOLS (APPLY CASE (LIST (CSM_CREATE WORKCOND SM))))
         (RETURN CSOLS)))
       (T (RETURN (PG_NULLCASE CSM)))))) 
(PUT 'PG_LCASE 'NUMBER-OF-ARGS 1) 
(PUT 'PG_LCASE 'DEFINED-ON-LINE '303) 
(PUT 'PG_LCASE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_LCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_LCASE (CSM)
    (PROG (X0VALUE AC TMP IDVALUES SM M N)
      (SETQ M 0)
      (SETQ N 0)
      (PG_VERBOSE (LIST "----entering lcase"))
      (SETQ SM (CSM_GETSM CSM))
      (SETQ M (SM_M SM))
      (SETQ N (SM_N SM))
      (SETQ TMP (SIMP 0))
      (PROG (C)
        (SETQ C 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) C)) (RETURN NIL)))
        (PROGN
         (SETQ AC
                 (COND
                  (*PGNOARBCOMPLEX
                   (CONS (LIST (CONS (CONS (SM_GET SM 1 C) 1) 1)) 1))
                  (T (CONS (MAKEARBCOMPLEX) 1))))
         (SETQ TMP (ADDSQ TMP (MULTSQ (SM_GET SM 0 C) AC)))
         (SETQ IDVALUES (CONS (CONS (SM_GET SM 1 C) AC) IDVALUES)))
        (SETQ C (PLUS2 C 1))
        (GO LAB))
      (SETQ X0VALUE
              (MULTSQ (ADDSQ (NEGSQ TMP) (SM_GET SM 0 N))
                      (INVSQ (SM_GET SM 0 0))))
      (SETQ IDVALUES (CONS (CONS (SM_GET SM 1 0) X0VALUE) IDVALUES))
      (PG_VERBOSE (LIST "----leaving lcase"))
      (RETURN (LIST (CONS (CSM_GETC CSM) IDVALUES))))) 
(PUT 'PG_CCASE 'NUMBER-OF-ARGS 1) 
(PUT 'PG_CCASE 'DEFINED-ON-LINE '328) 
(PUT 'PG_CCASE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_CCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_CCASE (CSM)
    (PROG (NEWCOND RETCOND VALUE TMP SM M N)
      (SETQ M 0)
      (SETQ N 0)
      (PG_VERBOSE (LIST "[entering ccase"))
      (SETQ SM (CSM_GETSM CSM))
      (COND (*PGVERBOSE (SM_PRINT SM)))
      (SETQ M (SM_M SM))
      (SETQ N (SM_N SM))
      (SM_FIRSTSTEP SM)
      (COND
       ((SM_RIGHTSIDESARENOTREDP (CSM_GETC CSM) SM 1)
        (PROGN
         (SETQ NEWCOND
                 (PROG (L FORALL-RESULT FORALL-ENDPTR)
                   (SETQ L 1)
                  STARTOVER
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE M 1) L)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (COND
                            ((NOT
                              (SM_GREENP (CSM_GETC CSM)
                               (SETQ TMP (CAR (SM_GET SM L N)))))
                             (LIST (COND_0MKEQT TMP)))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ L (PLUS2 L 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE M 1) L))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (COND
                            ((NOT
                              (SM_GREENP (CSM_GETC CSM)
                               (SETQ TMP (CAR (SM_GET SM L N)))))
                             (LIST (COND_0MKEQT TMP)))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ L (PLUS2 L 1))
                   (GO LOOPLABEL)))
         (SETQ RETCOND (NCONC NEWCOND (CSM_GETC CSM)))
         (SETQ VALUE
                 (CONS (SM_GET SM M 0)
                       (MULTSQ (SM_GET SM 0 N) (INVSQ (SM_GET SM 0 0)))))
         (PG_VERBOSE (LIST "leaving ccase successfull]"))
         (RETURN (LIST (CONS RETCOND (LIST VALUE))))))
       (T
        (PROGN
         (PG_VERBOSE (LIST "leaving ccase without success]"))
         (RETURN NIL)
         NIL))))) 
(PUT 'PG_RCASE 'NUMBER-OF-ARGS 1) 
(PUT 'PG_RCASE 'DEFINED-ON-LINE '360) 
(PUT 'PG_RCASE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_RCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_RCASE (CSM)
    (PROG (SUBSOLUTIONS SOLUTIONS TMP X0VALUE SM M N)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ SM (CSM_GETSM CSM))
      (SETQ M (SM_M SM))
      (SETQ N (SM_N SM))
      (SM_FIRSTSTEP SM)
      (COND
       (*PGVERBOSE
        (PROGN
         (PRIN2T "----entered rcase. first coulumn done: ")
         (SM_PRINT SM))))
      (SETQ SUBSOLUTIONS (PG_KERN (CSM_COPY CSM 1 1)))
      (PROG (S)
        (SETQ S SUBSOLUTIONS)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ TMP (SIMP 0))
            (PROG (C)
              (SETQ C 1)
             LAB
              (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) C)) (RETURN NIL)))
              (SETQ TMP
                      (ADDSQ TMP
                             (MULTSQ (SM_GET SM 0 C)
                                     (CDR (ATSOC (SM_GET SM M C) (CDR S))))))
              (SETQ C (PLUS2 C 1))
              (GO LAB))
            (SETQ X0VALUE
                    (MULTSQ (ADDSQ (NEGSQ TMP) (SM_GET SM 0 N))
                            (INVSQ (SM_GET SM 0 0))))
            (SETQ SOLUTIONS
                    (CONS
                     (CONS (CAR S)
                           (CONS (CONS (SM_GET SM M 0) X0VALUE) (CDR S)))
                     SOLUTIONS))
            NIL))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (PG_VERBOSE (LIST "----leaving rcase. solutions: " (LENGTH SOLUTIONS)))
      (RETURN (REVERSIP SOLUTIONS)))) 
(PUT 'PG_NULLCASE 'NUMBER-OF-ARGS 1) 
(PUT 'PG_NULLCASE 'DEFINED-ON-LINE '386) 
(PUT 'PG_NULLCASE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_NULLCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_NULLCASE (CSM)
    (PROG (NEWCOND RETCOND VALUES TMP SM AC M N)
      (SETQ M 0)
      (SETQ N 0)
      (PG_VERBOSE (LIST "----nullcase"))
      (SETQ SM (CSM_GETSM CSM))
      (SETQ M (SM_M SM))
      (SETQ N (SM_N SM))
      (COND
       ((SM_RIGHTSIDESARENOTREDP (CSM_GETC CSM) SM 0)
        (PROGN
         (SETQ NEWCOND
                 (PROG (L FORALL-RESULT FORALL-ENDPTR)
                   (SETQ L 0)
                  STARTOVER
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE M 1) L)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (COND
                            ((NOT
                              (SM_GREENP (CSM_GETC CSM)
                               (SETQ TMP (CAR (SM_GET SM L N)))))
                             (LIST (COND_0MKEQT TMP)))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ L (PLUS2 L 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE M 1) L))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (COND
                            ((NOT
                              (SM_GREENP (CSM_GETC CSM)
                               (SETQ TMP (CAR (SM_GET SM L N)))))
                             (LIST (COND_0MKEQT TMP)))))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ L (PLUS2 L 1))
                   (GO LOOPLABEL)))
         (SETQ RETCOND (NCONC NEWCOND (CSM_GETC CSM)))
         (SETQ VALUES
                 (PROG (C FORALL-RESULT FORALL-ENDPTR)
                   (SETQ C 0)
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE N 1) C)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (PROGN
                                     (SETQ AC
                                             (COND
                                              (*PGNOARBCOMPLEX
                                               (CONS
                                                (LIST
                                                 (CONS (CONS (SM_GET SM M C) 1)
                                                       1))
                                                1))
                                              (T
                                               (CONS
                                                (LIST
                                                 (CONS
                                                  (CONS (MAKEARBCOMPLEX) 1) 1))
                                                1))))
                                     (CONS (SM_GET SM M C) AC))
                                    NIL)))
                  LOOPLABEL
                   (SETQ C (PLUS2 C 1))
                   (COND
                    ((MINUSP (DIFFERENCE (DIFFERENCE N 1) C))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (PROGN
                             (SETQ AC
                                     (COND
                                      (*PGNOARBCOMPLEX
                                       (CONS
                                        (LIST
                                         (CONS (CONS (SM_GET SM M C) 1) 1))
                                        1))
                                      (T
                                       (CONS
                                        (LIST
                                         (CONS (CONS (MAKEARBCOMPLEX) 1) 1))
                                        1))))
                             (CONS (SM_GET SM M C) AC))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN (LIST (CONS RETCOND VALUES)))))))) 
(PUT 'PG_PRINT 'NUMBER-OF-ARGS 1) 
(PUT 'PG_PRINT 'DEFINED-ON-LINE '412) 
(PUT 'PG_PRINT 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_PRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_PRINT (SOLS)
    (PROG (I)
      (SETQ I 0)
      (PRIN2T "")
      (COND
       (SOLS
        (PROG (S)
          (SETQ S (CDR SOLS))
         LAB
          (COND ((NULL S) (RETURN NIL)))
          ((LAMBDA (S)
             (PROGN
              (SETQ I (PLUS I 1))
              (PRIN2 "SOLUTION ")
              (PRIN2T I)
              (PRIN2T "")
              (PRIN2T "CONDITION:")
              (MATHPRINT (CADR S))
              (PRIN2T "")
              (PRIN2T "VALUES:")
              (PROG (V)
                (SETQ V (CDADDR S))
               LAB
                (COND ((NULL V) (RETURN NIL)))
                ((LAMBDA (V) (MATHPRINT V)) (CAR V))
                (SETQ V (CDR V))
                (GO LAB))))
           (CAR S))
          (SETQ S (CDR S))
          (GO LAB))))
      (PRIN2T ""))) 
(PUT 'PG_C2 'NUMBER-OF-ARGS 2) 
(PUT 'PG_C2 'DEFINED-ON-LINE '431) 
(PUT 'PG_C2 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_C2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PG_C2 (N Z)
    (COND ((EQ N 1) (COND ((EQ Z 0) 2) (T 1)))
          (T
           (PLUS
            (TIMES N
                   (PROG (I FORALL-RESULT)
                     (SETQ I Z)
                     (SETQ FORALL-RESULT 0)
                    LAB1
                     (COND
                      ((MINUSP (DIFFERENCE (DIFFERENCE N 1) I))
                       (RETURN FORALL-RESULT)))
                     (SETQ FORALL-RESULT
                             (PLUS (PG_C2 (DIFFERENCE N 1) I) FORALL-RESULT))
                     (SETQ I (PLUS2 I 1))
                     (GO LAB1)))
            1)))) 
(PUT 'PG_C 'NUMBER-OF-ARGS 1) 
(PUT 'PG_C 'DEFINED-ON-LINE '439) 
(PUT 'PG_C 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_C 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_C (N) (PG_C2 N 0)) 
(PUT 'PG_SWITCHINFO 'NUMBER-OF-ARGS 0) 
(PUT 'PG_SWITCHINFO 'DEFINED-ON-LINE '445) 
(PUT 'PG_SWITCHINFO 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_SWITCHINFO 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PG_SWITCHINFO NIL
    (PROGN
     (PRIN2T "SWITCHES")
     (PRIN2 "pggeneric      ")
     (COND (*PGGENERIC (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pginfosm       ")
     (COND (*PGINFOSM (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pginfosol      ")
     (COND (*PGINFOSOL (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pgsourcedirect ")
     (COND (*PGSOURCEDIRECT (PRIN2T "on")) (T (PRIN2T "off")))
     (PRIN2 "pgspsimpl      ")
     (COND (*PGSPSIMPL (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pgspqe         ")
     (COND (*PGSPQE (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pgverbose      ")
     (COND (*PGVERBOSE (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "pgaftermath    ")
     (COND (*PGAFTERMATH (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2 "nopropzero     ")
     (COND (*NOPROPZERO (PRIN2T " on")) (T (PRIN2T "off")))
     (PRIN2T "")
     NIL)) 
(PUT 'PG_VERBOSE 'NUMBER-OF-ARGS 1) 
(PUT 'PG_VERBOSE 'DEFINED-ON-LINE '471) 
(PUT 'PG_VERBOSE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'PG_VERBOSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PG_VERBOSE (L)
    (COND
     (*PGVERBOSE
      (PROGN
       (PROG (I)
         (SETQ I L)
        LAB
         (COND ((NULL I) (RETURN NIL)))
         ((LAMBDA (I) (PRIN2 I)) (CAR I))
         (SETQ I (CDR I))
         (GO LAB))
       (PRIN2T ""))))) 
(EXPORTS
 (LIST 'EQT_EQTP 'EQT_LEFTSIDE 'EQT_RIGHTSIDE 'EQT_MAKEEQT 'EQT_0MKUNEQT)) 
(PUT 'EQT_EQTP 'NUMBER-OF-ARGS 1) 
(PUT 'EQT_EQTP 'DEFINED-ON-LINE '483) 
(PUT 'EQT_EQTP 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'EQT_EQTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQT_EQTP (EQT) (COND ((EQ (CAR EQT) 'EQUAL) T))) 
(PUT 'EQT_LEFTSIDE 'NUMBER-OF-ARGS 1) 
(PUT 'EQT_LEFTSIDE 'DEFINED-ON-LINE '489) 
(PUT 'EQT_LEFTSIDE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'EQT_LEFTSIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQT_LEFTSIDE (EQT) (CADR EQT)) 
(PUT 'EQT_RIGHTSIDE 'NUMBER-OF-ARGS 1) 
(PUT 'EQT_RIGHTSIDE 'DEFINED-ON-LINE '493) 
(PUT 'EQT_RIGHTSIDE 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'EQT_RIGHTSIDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQT_RIGHTSIDE (EQT) (CADDR EQT)) 
(PUT 'EQT_MAKEEQT 'NUMBER-OF-ARGS 2) 
(PUT 'EQT_MAKEEQT 'DEFINED-ON-LINE '497) 
(PUT 'EQT_MAKEEQT 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'EQT_MAKEEQT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQT_MAKEEQT (LS RS) (CONS 'EQUAL (CONS LS (LIST RS)))) 
(PUT 'EQT_MKUNEQT 'NUMBER-OF-ARGS 2) 
(PUT 'EQT_MKUNEQT 'DEFINED-ON-LINE '502) 
(PUT 'EQT_MKUNEQT 'DEFINED-IN-FILE 'PGAUSS/PGAUSS.RED) 
(PUT 'EQT_MKUNEQT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQT_MKUNEQT (LS RS) (CONS 'NOT (LIST (EQT_MAKEEQT LS RS)))) 
(ENDMODULE) 