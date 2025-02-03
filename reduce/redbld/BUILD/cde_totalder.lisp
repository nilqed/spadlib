(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_TOTALDER)) 
(PUT 'SET_SVF 'NUMBER-OF-ARGS 4) 
(PUT 'SET_SVF 'DEFINED-ON-LINE '38) 
(PUT 'SET_SVF 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'SET_SVF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SET_SVF (COMP I J VAL) (SETK (LIST COMP I J) VAL)) 
(PUT 'COEFF_TOTDER 'NUMBER-OF-ARGS 3) 
(PUT 'COEFF_TOTDER 'DEFINED-ON-LINE '43) 
(PUT 'COEFF_TOTDER 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'COEFF_TOTDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFF_TOTDER (PAR VAR I)
    (PROG (VAR_MIND VARTEMP MIPTEMP)
      (SETQ VAR_MIND (IDTOMIND PAR VAR))
      (SETQ VARTEMP (CAR VAR_MIND))
      (SETQ MIPTEMP (CADR (ASSOC (CADR VAR_MIND) ALL_MIND_TABLE*)))
      (COND
       (MIPTEMP
        (COND
         ((EQUAL PAR 0)
          (RETURN
           (CAR (CDE_LASSOC2 (LIST VARTEMP (NTH MIPTEMP I)) I2M_JETSPACE*))))
         (T
          (RETURN
           (CAR
            (CDE_LASSOC2 (LIST VARTEMP (NTH MIPTEMP I)) I2M_JETSPACE_ODD*))))))
       (T (RETURN 'LETOP))))) 
(PUT 'INITIALIZE_TD_INDEP_VAR 'NUMBER-OF-ARGS 1) 
(PUT 'INITIALIZE_TD_INDEP_VAR 'DEFINED-ON-LINE '62) 
(PUT 'INITIALIZE_TD_INDEP_VAR 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'INITIALIZE_TD_INDEP_VAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INITIALIZE_TD_INDEP_VAR (TD)
    (PROG (DDTEMP)
      (RETURN
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
         (PROGN
          (SETQ DDTEMP (NTH TD I))
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND ((MINUSP (DIFFERENCE N_INDEP_VAR J)) (RETURN NIL)))
            (COND ((EQUAL I J) (SET_SVF DDTEMP 0 J 1))
                  (T (SET_SVF DDTEMP 0 J 0)))
            (SETQ J (PLUS2 J 1))
            (GO LAB)))
         (SETQ I (PLUS2 I 1))
         (GO LAB))))) 
(PUT 'INITIALIZE_TD_EVEN_VAR 'NUMBER-OF-ARGS 3) 
(PUT 'INITIALIZE_TD_EVEN_VAR 'DEFINED-ON-LINE '73) 
(PUT 'INITIALIZE_TD_EVEN_VAR 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'INITIALIZE_TD_EVEN_VAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INITIALIZE_TD_EVEN_VAR (TD PARAM_DER PRINC_DER)
    (PROG (DDTEMP DCON VAR_I J N_PARAM_DER)
      (SETQ J 0)
      (SETQ N_PARAM_DER 0)
      (SETQ N_PARAM_DER (LENGTH PARAM_DER))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROGN
         (SETQ DDTEMP (NTH TD I))
         (SETQ J 0)
         (PROG (EL)
           (SETQ EL PARAM_DER)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (PROGN
               (SETQ J (PLUS J 1))
               (SETQ VAR_I (COEFF_TOTDER 0 EL I))
               (COND
                ((MEMBER VAR_I ALL_PRINCIPAL_DER*)
                 (SETQ DCON
                         (CONS (LIST DDTEMP 0 (PLUS J N_INDEP_VAR) VAR_I)
                               DCON))))
               (SET_SVF DDTEMP 0 (PLUS J N_INDEP_VAR) VAR_I)))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         (SETQ J 0)
         (PROG (EL)
           (SETQ EL PRINC_DER)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (PROGN
               (SETQ J (PLUS J 1))
               (SETQ VAR_I (COEFF_TOTDER 0 EL I))
               (SET_SVF DDTEMP 0 (PLUS J N_INDEP_VAR N_PARAM_DER) VAR_I)))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN DCON))) 
(PUT 'INITIALIZE_TD_ODD_VAR 'NUMBER-OF-ARGS 3) 
(PUT 'INITIALIZE_TD_ODD_VAR 'DEFINED-ON-LINE '99) 
(PUT 'INITIALIZE_TD_ODD_VAR 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'INITIALIZE_TD_ODD_VAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INITIALIZE_TD_ODD_VAR (TD PARAM_EXT PRINC_EXT)
    (PROG (DDTEMP DCON_ODD OVAR_I J N_PARAM_EXT)
      (SETQ J 0)
      (SETQ N_PARAM_EXT 0)
      (SETQ N_PARAM_EXT (LENGTH PARAM_EXT))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROGN
         (SETQ DDTEMP (NTH TD I))
         (SETQ J 0)
         (PROG (EL)
           (SETQ EL PARAM_EXT)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (PROGN
               (SETQ J (PLUS J 1))
               (SETQ OVAR_I (COEFF_TOTDER 1 (EXTODD EL) I))
               (COND
                ((NOT (EQUAL OVAR_I 'LETOP))
                 (PROGN
                  (COND
                   ((MEMBER OVAR_I ALL_PRINCIPAL_ODD*)
                    (SETQ DCON_ODD (CONS (LIST DDTEMP 1 J OVAR_I) DCON_ODD))))
                  (SET_SVF DDTEMP 1 J (ODDEXT OVAR_I))))
                (T (SET_SVF DDTEMP 1 J 'LETOP)))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         (SETQ J 0)
         (PROG (EL)
           (SETQ EL PRINC_EXT)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (PROGN
               (SETQ J (PLUS J 1))
               (SETQ OVAR_I (COEFF_TOTDER 1 (EXTODD EL) I))
               (COND
                ((NOT (EQUAL OVAR_I 'LETOP))
                 (SET_SVF DDTEMP 1 (PLUS J N_PARAM_EXT) (ODDEXT OVAR_I)))
                (T (SET_SVF DDTEMP 1 (PLUS J N_PARAM_EXT) 'LETOP)))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN DCON_ODD))) 
(PUT 'CDE_ODD_DERIVATIVES 'NUMBER-OF-ARGS 1) 
(PUT 'CDE_ODD_DERIVATIVES 'DEFINED-ON-LINE '131) 
(PUT 'CDE_ODD_DERIVATIVES 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'CDE_ODD_DERIVATIVES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CDE_ODD_DERIVATIVES (EXT_VARS)
    (PROG (SVF_TEMP N_EXT)
      (SETQ N_EXT 0)
      (SETQ N_EXT (PLUS N_ALL_PARAMETRIC_EXT N_ALL_PRINCIPAL_EXT))
      (RETURN
       (PROG (I)
         (SETQ I 1)
        LAB
         (COND ((MINUSP (DIFFERENCE N_EXT I)) (RETURN NIL)))
         (PROGN
          (SETQ SVF_TEMP (MKID 'SVF_EXT I))
          (SUPER_VECTORFIELD SVF_TEMP (LIST) EXT_VARS)
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND ((MINUSP (DIFFERENCE N_EXT J)) (RETURN NIL)))
            (SET_SVF SVF_TEMP 1 J (COND ((EQN I J) 1) (T 0)))
            (SETQ J (PLUS2 J 1))
            (GO LAB)))
         (SETQ I (PLUS2 I 1))
         (GO LAB))))) 
(PUT 'DF_ODD 'NUMBER-OF-ARGS 2) 
(PUT 'DF_ODD 'DEFINED-ON-LINE '148) 
(PUT 'DF_ODD 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'DF_ODD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DF_ODD (EXPR_ODD ODDVAR)
    (REVAL1
     (LIST (MKID 'SVF_EXT (CADR (ODDEXT ODDVAR))) (REPLACE_ODDEXT EXPR_ODD))
     NIL)) 
(FLAG '(DF_ODD) 'OPFN) 
(PUT 'DF_EXT 'NUMBER-OF-ARGS 2) 
(PUT 'DF_EXT 'DEFINED-ON-LINE '165) 
(PUT 'DF_EXT 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'DF_EXT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DF_EXT (EXPR_EXT EXTVAR)
    (REVAL1 (LIST (MKID 'SVF_EXT (CADR EXTVAR)) EXPR_EXT) NIL)) 
(FLAG '(DF_EXT) 'OPFN) 
(PUT 'ITER_DD 'NUMBER-OF-ARGS 3) 
(PUT 'ITER_DD 'DEFINED-ON-LINE '178) 
(PUT 'ITER_DD 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'ITER_DD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ITER_DD (DD_NAME ARG_EXT N)
    (COND ((EQN N 0) ARG_EXT)
          (T (LIST DD_NAME (ITER_DD DD_NAME ARG_EXT (DIFFERENCE N 1)))))) 
(PUT 'COMPUTE_TD 'NUMBER-OF-ARGS 1) 
(PUT 'COMPUTE_TD 'DEFINED-ON-LINE '200) 
(PUT 'COMPUTE_TD 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'COMPUTE_TD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COMPUTE_TD (U)
    (PROG (INDVARS TEMPPOS TEMPIVAR TEMPINDEX TDRES DD_NAME)
      (SETQ TDRES (REPLACE_ODDEXT0 (CAR U)))
      (SETQ INDVARS (CDR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL INDVARS))) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT
            (NULL
             (SETQ TEMPPOS
                     (CDE_POSITION
                      (SETQ TEMPIVAR (*A2K (REVAL1 (CAR INDVARS) NIL)))
                      INDEP_VAR*))))
           (PROGN
            (SETQ DD_NAME (NTH TOT_DER* TEMPPOS))
            (COND
             ((AND (NOT (NULL (CDR INDVARS)))
                   (FIXP (SETQ TEMPINDEX (REVAL1 (CADR INDVARS) NIL))))
              (PROGN
               (SETQ TDRES (REVAL1 (ITER_DD DD_NAME TDRES TEMPINDEX) NIL))
               (SETQ INDVARS (CDDR INDVARS))))
             (T
              (PROGN
               (SETQ TDRES (REVAL1 (LIST DD_NAME TDRES) NIL))
               (SETQ INDVARS (CDR INDVARS)))))
            NIL))
          (T (REDERR "Wrong number of arguments to `td'"))))
        (GO WHILELABEL))
      (COND (*CHECKORD (CHECK_LETOP TDRES)))
      (RETURN (SIMP (REPLACE_EXTODD0 TDRES))))) 
(AEVAL (OPERATOR (LIST 'TD))) 
(PUT 'TD 'SIMPFN 'COMPUTE_TD) 
(PUT 'TD_MIND 'NUMBER-OF-ARGS 2) 
(PUT 'TD_MIND 'DEFINED-ON-LINE '234) 
(PUT 'TD_MIND 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'TD_MIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TD_MIND (ARG MIND) (TD_MIND0 ARG (CDR MIND))) 
(PUT 'TD_MIND0 'NUMBER-OF-ARGS 2) 
(PUT 'TD_MIND0 'DEFINED-ON-LINE '238) 
(PUT 'TD_MIND0 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'TD_MIND0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TD_MIND0 (ARG MIND)
    (PROG (U)
      (SETQ U
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (LIST (NTH INDEP_VAR* I) (NTH MIND I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (LIST (NTH INDEP_VAR* I) (NTH MIND I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (SETQ U (CONS ARG U))
      (RETURN (REVAL1 (CONS 'TD U) NIL)))) 
(FLAG '(TD_MIND) 'OPFN) 
(PUT 'EXPAND_TD 'NUMBER-OF-ARGS 0) 
(FLAG '(EXPAND_TD) 'OPFN) 
(PUT 'EXPAND_TD 'DEFINED-ON-LINE '250) 
(PUT 'EXPAND_TD 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'EXPAND_TD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXPAND_TD NIL (PUT 'TD 'SIMPFN 'COMPUTE_TD)) 
(PUT 'NOEXPAND_TD 'NUMBER-OF-ARGS 0) 
(FLAG '(NOEXPAND_TD) 'OPFN) 
(PUT 'NOEXPAND_TD 'DEFINED-ON-LINE '254) 
(PUT 'NOEXPAND_TD 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'NOEXPAND_TD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NOEXPAND_TD NIL (PUT 'TD 'SIMPFN 'SIMPIDEN)) 
(PUT 'CDE_TOTAL_DERIVATIVES 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_TOTAL_DERIVATIVES 'DEFINED-ON-LINE '258) 
(PUT 'CDE_TOTAL_DERIVATIVES 'DEFINED-IN-FILE 'CDE/CDE_TOTALDER.RED) 
(PUT 'CDE_TOTAL_DERIVATIVES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_TOTAL_DERIVATIVES NIL
    (PROG (TEMPPRINC_DER TEMPPRINC_ODD ALL_EXT)
      (SETQ TOT_DER*
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (MKID ID_TOT_DER* (NTH INDEP_VAR* I))
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (MKID ID_TOT_DER* (NTH INDEP_VAR* I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N_ALL_EXT (PLUS N_ALL_PARAMETRIC_EXT N_ALL_PRINCIPAL_EXT))
      (SETQ ALL_EXT
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N_ALL_EXT I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (LIST 'EXT I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE N_ALL_EXT I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (LIST 'EXT I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROGN
         (AEVAL* (DEPEND (LIST 'LETOP (LIST 'PART 'INDEP_VAR I))))
         (SUPER_VECTORFIELD (NTH TOT_DER* I)
                            (APPEND INDEP_VAR*
                                    (APPEND ALL_PARAMETRIC_DER*
                                            ALL_PRINCIPAL_DER*))
                            ALL_EXT))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (INITIALIZE_TD_INDEP_VAR TOT_DER*)
      (SETQ PRIMARY_DIFFCON_DER_TOT*
              (REVERSE
               (INITIALIZE_TD_EVEN_VAR TOT_DER* ALL_PARAMETRIC_DER*
                ALL_PRINCIPAL_DER*)))
      (SETQ PRIMARY_DIFFCON_ODD_TOT*
              (REVERSE
               (INITIALIZE_TD_ODD_VAR TOT_DER*
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I 1)
                  (COND
                   ((MINUSP (DIFFERENCE N_ALL_PARAMETRIC_EXT I)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR (CONS (LIST 'EXT I) NIL)))
                 LOOPLABEL
                  (SETQ I (PLUS2 I 1))
                  (COND
                   ((MINUSP (DIFFERENCE N_ALL_PARAMETRIC_EXT I))
                    (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR (CONS (LIST 'EXT I) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I 1)
                  (COND
                   ((MINUSP (DIFFERENCE N_ALL_PRINCIPAL_EXT I)) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   (LIST 'EXT (PLUS I N_ALL_PARAMETRIC_EXT))
                                   NIL)))
                 LOOPLABEL
                  (SETQ I (PLUS2 I 1))
                  (COND
                   ((MINUSP (DIFFERENCE N_ALL_PRINCIPAL_EXT I))
                    (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS (LIST 'EXT (PLUS I N_ALL_PARAMETRIC_EXT)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ TEMPPRINC_DER
              (CDE_MKSET
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL PRIMARY_DIFFCON_DER_TOT*)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL) (CAR (REVERSE EL))) (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (EL) (CAR (REVERSE EL))) (CAR EL))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ PRIMARY_DIFFCON_DER* (CDE_DIFFSET TEMPPRINC_DER PRINCIPAL_DER*))
      (SETQ TEMPPRINC_ODD
              (CDE_MKSET
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL PRIMARY_DIFFCON_ODD_TOT*)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL) (CAR (REVERSE EL))) (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (EL) (CAR (REVERSE EL))) (CAR EL))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ PRIMARY_DIFFCON_ODD* (CDE_DIFFSET TEMPPRINC_ODD PRINCIPAL_ODD*))
      (CDE_ODD_DERIVATIVES ALL_EXT))) 
(ENDMODULE) 