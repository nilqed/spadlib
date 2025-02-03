(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_CDIFF)) 
(PUT 'MK_CDIFFOP 'NUMBER-OF-ARGS 4) 
(PUT 'MK_CDIFFOP 'DEFINED-ON-LINE '38) 
(PUT 'MK_CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'MK_CDIFFOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_CDIFFOP (CDIFF_OP NUM_ARG LEN_ARG LEN_TARGET)
    (PROG ()
      (MKOP CDIFF_OP)
      (PUT 'CDOP CDIFF_OP T)
      (COND
       ((NOT (NUMBERP NUM_ARG)) (REDERR "Error in the number of arguments")))
      (PUT 'CDNARG CDIFF_OP NUM_ARG)
      (COND
       ((NOT (FIXP NUM_ARG))
        (REDERR "Error: number of arguments must be an integer")))
      (COND
       ((NOT (AND (LISTP LEN_ARG) (EQUAL (CAR LEN_ARG) 'LIST)))
        (REDERR "Error in the list of lengths of arguments")))
      (PUT 'CDLARG CDIFF_OP LEN_ARG)
      (COND
       ((NOT (FIXP LEN_TARGET))
        (REDERR "Error: the length of image vectors must be an integer")))
      (PUT 'CDTARGET CDIFF_OP LEN_TARGET))) 
(FLAG '(MK_CDIFFOP) 'OPFN) 
(PUT 'CDIFFP 'NUMBER-OF-ARGS 1) 
(PUT 'CDIFFP 'DEFINED-ON-LINE '69) 
(PUT 'CDIFFP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CDIFFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CDIFFP (CDIFF_OP) (GET 'CDOP CDIFF_OP)) 
(PUT 'CHECK_CDIFF_ONEARG 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_CDIFF_ONEARG 'DEFINED-ON-LINE '72) 
(PUT 'CHECK_CDIFF_ONEARG 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CHECK_CDIFF_ONEARG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_CDIFF_ONEARG (CDIFF_OP)
    (PROG (LEN_ARG NUM_ARG N_ARG LEN_TARGET)
      (SETQ NUM_ARG 0)
      (SETQ N_ARG 0)
      (SETQ LEN_TARGET 0)
      (COND
       ((NOT (CDIFFP CDIFF_OP))
        (REDERR
         "Error: the first argument must be a declared CDiff operator")))
      (COND
       ((NOT (EQN (SETQ NUM_ARG (GET 'CDNARG CDIFF_OP)) 1))
        (REDERR "Error: the operator must have exactly one argument")))
      (COND
       ((NOT
         ((LAMBDA (L) (AND (LISTP L) (EQUAL (CAR L) 'LIST)))
          (SETQ LEN_ARG (GET 'CDLARG CDIFF_OP))))
        (REDERR "Error: the length of arguments must be an algebraic list")))
      (COND
       ((NOT (EQN (LENGTH LEN_ARG) 2))
        (REDERR "Error: the operator must have exactly one argument")))
      (COND
       ((NOT (FIXP (SETQ N_ARG (CADR LEN_ARG))))
        (REDERR
         "Error: the dimension of the argument space must be an integer")))
      (COND
       ((NOT (FIXP (SETQ LEN_TARGET (GET 'CDTARGET CDIFF_OP))))
        (REDERR "Error: the length of image vectors must be an integer"))))) 
(PUT 'CHECK_CDIFF_SCALAR 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_CDIFF_SCALAR 'DEFINED-ON-LINE '91) 
(PUT 'CHECK_CDIFF_SCALAR 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CHECK_CDIFF_SCALAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_CDIFF_SCALAR (CDIFF_OP)
    (PROG (LEN_ARG LEN_TARGET)
      (SETQ LEN_TARGET 0)
      (COND
       ((NOT (CDIFFP CDIFF_OP))
        (REDERR
         "Error: the first argument must be a declared CDiff operator")))
      (COND
       ((NOT
         ((LAMBDA (L) (AND (LISTP L) (EQUAL (CAR L) 'LIST)))
          (SETQ LEN_ARG (GET 'CDLARG CDIFF_OP))))
        (REDERR "Error: the length of arguments must be an algebraic list")))
      (COND
       ((NOT (EQN (SETQ LEN_TARGET (GET 'CDTARGET CDIFF_OP)) 1))
        (REDERR "Error: the length of image must be 1 for scalar operators"))))) 
(PUT 'CHECK_CDIFF_SAMETYPE 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_CDIFF_SAMETYPE 'DEFINED-ON-LINE '104) 
(PUT 'CHECK_CDIFF_SAMETYPE 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CHECK_CDIFF_SAMETYPE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_CDIFF_SAMETYPE (CD1 CD2)
    (PROG (PARITY1 PARITY2 LARGCD1 LARGCD2 TARGET1 TARGET2)
      (COND
       ((NOT (CDIFFP CD1))
        (REDERR
         "The first argument must be a declared C-Differential operator")))
      (COND
       ((NOT (CDIFFP CD2))
        (REDERR
         "The second argument must be a declared C-Differential operator")))
      (SETQ PARITY1 (GET 'CDNARG CD1))
      (SETQ PARITY2 (GET 'CDNARG CD2))
      (COND
       ((NOT (EQN PARITY1 PARITY2))
        (REDERR "The number of arguments of the operators must be the same")))
      (SETQ LARGCD1 (GET 'CDLARG CD1))
      (SETQ LARGCD2 (GET 'CDLARG CD2))
      (COND
       ((NOT (EQUAL LARGCD1 LARGCD2))
        (REDERR
         "The length of all arguments of the operators must be the same")))
      (SETQ TARGET1 (GET 'CDTARGET CD1))
      (SETQ TARGET2 (GET 'CDTARGET CD2))
      (COND
       ((NOT (EQN TARGET1 TARGET2))
        (REDERR
         "The length of vectors in the target space must be the same"))))) 
(AEVAL (OPERATOR (LIST 'ODDPROD*))) 
(PUT 'ODDPROD* 'SIMPFN 'EV_ODD_PRODUCT) 
(PUT 'EV_ODD_PRODUCT 'NUMBER-OF-ARGS 1) 
(PUT 'EV_ODD_PRODUCT 'DEFINED-ON-LINE '136) 
(PUT 'EV_ODD_PRODUCT 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'EV_ODD_PRODUCT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EV_ODD_PRODUCT (U) (SIMP (ODD_PRODUCT (CAR U) (CADR U)))) 
(PUT 'LOAD_CDIFFOP0 'NUMBER-OF-ARGS 3) 
(PUT 'LOAD_CDIFFOP0 'DEFINED-ON-LINE '139) 
(PUT 'LOAD_CDIFFOP0 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'LOAD_CDIFFOP0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOAD_CDIFFOP0 (OPNAME INDS L_COEFF)
    (PROG (ARGPROP NARG L_PHI EXPROP TEMPEXP TEMPCF TEMPLMIND CHANGE_EXPAND_TD)
      (COND ((CDIFFP OPNAME) (SETQ ARGPROP 'CDNARG))
            ((SCDIFFP OPNAME) (SETQ ARGPROP 'SCDNARG)))
      (SETQ NARG (GET ARGPROP OPNAME))
      (SETQ L_PHI
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE NARG I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (GENSYM) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE NARG I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (GENSYM) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ EXPROP 0)
      (SETQ CHANGE_EXPAND_TD 0)
      (COND
       ((EQ (GET 'TD 'SIMPFN) 'COMPUTE_TD)
        (PROGN (SETQ CHANGE_EXPAND_TD 1) (NOEXPAND_TD))))
      (PUT 'ODDPROD* 'SIMPFN 'SIMPIDEN)
      (PROG (EL)
        (SETQ EL L_COEFF)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ TEMPEXP 1)
            (SETQ TEMPCF (CADR EL))
            (COND
             ((NOT (EQ TEMPCF 0))
              (PROGN
               (SETQ TEMPLMIND (CDDR EL))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE NARG I)) (RETURN NIL)))
                 (SETQ TEMPEXP
                         (REVAL1
                          (LIST 'ODDPROD*
                                (LIST 'TD_MIND (NTH L_PHI I) (NTH TEMPLMIND I))
                                TEMPEXP)
                          NIL))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (SETQ TEMPEXP (REVAL1 (LIST 'ODDPROD* TEMPCF TEMPEXP) NIL))
               (SETQ EXPROP (REVAL1 (LIST 'PLUS TEMPEXP EXPROP) NIL))
               NIL)))
            NIL))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (CDE_EV_FORALL (CDE_FORALL_FORM OPNAME INDS L_PHI EXPROP))
      (PUT 'ODDPROD* 'SIMPFN 'EV_ODD_PRODUCT)
      (RETURN (COND ((EQN CHANGE_EXPAND_TD 1) (EXPAND_TD)))))) 
(PUT 'LOAD_CDIFFOP 'NUMBER-OF-ARGS 3) 
(PUT 'LOAD_CDIFFOP 'DEFINED-ON-LINE '182) 
(PUT 'LOAD_CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'LOAD_CDIFFOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOAD_CDIFFOP (OPNAME INDS L_COEFF)
    (PROG (NARG)
      (COND
       ((NOT (CDIFFP OPNAME))
        (REDERR
         "The first argument must be a declared C-Differential operator")))
      (SETQ NARG (GET 'CDNARG OPNAME))
      (COND
       ((NOT (EQN NARG (DIFFERENCE (LENGTH INDS) 2)))
        (REDERR "Second argument: wrong number of indices")))
      (PROG (EL)
        (SETQ EL (CDR L_COEFF))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((NOT (EQN NARG (DIFFERENCE (LENGTH EL) 2)))
             (REDERR "Third argument: wrong number of arguments"))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (LOAD_CDIFFOP0 OPNAME (CDR INDS) (CDR (REPLACE_ODDEXT L_COEFF))))) 
(FLAG '(LOAD_CDIFFOP) 'OPFN) 
(PUT 'COEFF_CDIFFOP0 'NUMBER-OF-ARGS 3) 
(PUT 'COEFF_CDIFFOP0 'DEFINED-ON-LINE '209) 
(PUT 'COEFF_CDIFFOP0 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'COEFF_CDIFFOP0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFF_CDIFFOP0 (CDOP L_IND MIND)
    (PROG (TEMPMON ZERO_INDEP_VAR)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (SETQ TEMPMON
                (CONS (REVAL1 (LIST 'EXPT (NTH INDEP_VAR* I) (NTH MIND I)) NIL)
                      TEMPMON))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ TEMPMON
              (CONS
               (REVAL1 (LIST 'EXPT (FACTORIAL_MULTIINDEX MIND) (MINUS 1)) NIL)
               TEMPMON))
      (SETQ TEMPMON (REVAL1 (CONS 'TIMES TEMPMON) NIL))
      (SETQ ZERO_INDEP_VAR
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL INDEP_VAR*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL) (LIST 'REPLACEBY EL 0))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (LIST 'REPLACEBY EL 0)) (CAR EL))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CADR
        (EVALWHEREEXP
         (LIST (CONS 'LIST ZERO_INDEP_VAR)
               (LIST 'LIST
                     (REVAL1 (APPEND (CONS CDOP L_IND) (LIST TEMPMON))
                             NIL)))))))) 
(PUT 'COEFF_CDIFFOP 'NUMBER-OF-ARGS 3) 
(PUT 'COEFF_CDIFFOP 'DEFINED-ON-LINE '230) 
(PUT 'COEFF_CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'COEFF_CDIFFOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COEFF_CDIFFOP (CDOP L_IND MIND)
    (REPLACE_EXTODD (COEFF_CDIFFOP0 CDOP (CDR L_IND) (CDR MIND)))) 
(FLAG '(COEFF_CDIFFOP) 'OPFN) 
(PUT 'ALL_COEFF_CDIFFOP0 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_COEFF_CDIFFOP0 'DEFINED-ON-LINE '235) 
(PUT 'ALL_COEFF_CDIFFOP0 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'ALL_COEFF_CDIFFOP0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_COEFF_CDIFFOP0 (CDOP)
    (PROG (LISTCOEFF0 L_ARG L_TAR TEMPCF TLIST)
      (SETQ LISTCOEFF0 (LIST))
      (CHECK_CDIFF_ONEARG CDOP)
      (SETQ L_ARG (CADR (GET 'CDLARG CDOP)))
      (SETQ L_TAR (GET 'CDTARGET CDOP))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L_TAR I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE L_ARG J)) (RETURN NIL)))
          (PROGN
           (SETQ TLIST (LIST))
           (PROG (MIND)
             (SETQ MIND ALL_MIND*)
            LAB
             (COND ((NULL MIND) (RETURN NIL)))
             ((LAMBDA (MIND)
                (COND
                 ((NOT
                   (EQN (SETQ TEMPCF (COEFF_CDIFFOP0 CDOP (LIST I J) MIND)) 0))
                  (SETQ TLIST
                          (CONS (CONS 'LIST (LIST TEMPCF (CONS 'LIST MIND)))
                                TLIST)))))
              (CAR MIND))
             (SETQ MIND (CDR MIND))
             (GO LAB))
           (COND
            ((NOT TLIST)
             (SETQ TLIST
                     (LIST 'LIST
                           (CONS 'LIST
                                 (LIST 0 (CONS 'LIST (NTH ALL_MIND* 1)))))))
            (T (SETQ TLIST (CONS 'LIST (CONS (CONS 'LIST (LIST I J)) TLIST)))))
           (SETQ LISTCOEFF0 (CONS TLIST LISTCOEFF0)))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS 'LIST LISTCOEFF0)))) 
(PUT 'ALL_COEFF_CDIFFOP 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_COEFF_CDIFFOP 'DEFINED-ON-LINE '258) 
(PUT 'ALL_COEFF_CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'ALL_COEFF_CDIFFOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_COEFF_CDIFFOP (CDOP) (ALL_COEFF_CDIFFOP0 CDOP)) 
(FLAG '(ALL_COEFF_CDIFFOP) 'OPFN) 
(PUT 'COEFF2CDIFFOP_ENTRY 'NUMBER-OF-ARGS 2) 
(PUT 'COEFF2CDIFFOP_ENTRY 'DEFINED-ON-LINE '263) 
(PUT 'COEFF2CDIFFOP_ENTRY 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'COEFF2CDIFFOP_ENTRY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFF2CDIFFOP_ENTRY (CDOP IND_PAR)
    (PROG (IND_PAR0 EXPROP PHI T_IND T_ALLCOEFF T_COEFF0)
      (SETQ IND_PAR0 (CDR IND_PAR))
      (SETQ PHI (GENSYM))
      (SETQ EXPROP 0)
      (SETQ T_IND (CDAR IND_PAR0))
      (SETQ T_ALLCOEFF (CDR IND_PAR0))
      (PROG (T_COEFF)
        (SETQ T_COEFF T_ALLCOEFF)
       LAB
        (COND ((NULL T_COEFF) (RETURN NIL)))
        ((LAMBDA (T_COEFF)
           (PROGN
            (SETQ T_COEFF0 (CDR T_COEFF))
            (SETQ EXPROP
                    (REVAL1
                     (LIST 'PLUS
                           (LIST 'ODDPROD* (CAR T_COEFF0)
                                 (REVAL1 (LIST 'TD_MIND PHI (CADR T_COEFF0))
                                         NIL))
                           EXPROP)
                     NIL))
            NIL))
         (CAR T_COEFF))
        (SETQ T_COEFF (CDR T_COEFF))
        (GO LAB))
      (CDE_EV_FORALL (CDE_FORALL_FORM CDOP T_IND (LIST PHI) EXPROP)))) 
(PUT 'COEFF2CDIFFOP 'NUMBER-OF-ARGS 2) 
(PUT 'COEFF2CDIFFOP 'DEFINED-ON-LINE '293) 
(PUT 'COEFF2CDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'COEFF2CDIFFOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFF2CDIFFOP (CDOP L_PAR)
    (PROG (L_PAR0 CHANGE_EXPAND_TD)
      (CHECK_CDIFF_ONEARG CDOP)
      (SETQ L_PAR0 (CDR (REPLACE_ODDEXT L_PAR)))
      (SETQ CHANGE_EXPAND_TD 0)
      (COND
       ((EQ (GET 'TD 'SIMPFN) 'COMPUTE_TD)
        (PROGN (SETQ CHANGE_EXPAND_TD 1) (NOEXPAND_TD))))
      (PUT 'ODDPROD* 'SIMPFN 'SIMPIDEN)
      (PROG (IND_PAR)
        (SETQ IND_PAR L_PAR0)
       LAB
        (COND ((NULL IND_PAR) (RETURN NIL)))
        ((LAMBDA (IND_PAR) (COEFF2CDIFFOP_ENTRY CDOP IND_PAR)) (CAR IND_PAR))
        (SETQ IND_PAR (CDR IND_PAR))
        (GO LAB))
      (PUT 'ODDPROD* 'SIMPFN 'EV_ODD_PRODUCT)
      (COND ((EQN CHANGE_EXPAND_TD 1) (EXPAND_TD))))) 
(FLAG '(COEFF2CDIFFOP) 'OPFN) 
(PUT 'CONV_CDIFF2SUPERFUN 'NUMBER-OF-ARGS 2) 
(PUT 'CONV_CDIFF2SUPERFUN 'DEFINED-ON-LINE '317) 
(PUT 'CONV_CDIFF2SUPERFUN 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CONV_CDIFF2SUPERFUN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CONV_CDIFF2SUPERFUN (CDIFF_OP SUPERFUN)
    (PROG (LEN_ARG TEMPSUPERFUN NUM_ARG N_ARG LEN_TARGET)
      (SETQ NUM_ARG 0)
      (SETQ N_ARG 0)
      (SETQ LEN_TARGET 0)
      (CHECK_CDIFF_ONEARG CDIFF_OP)
      (SETQ NUM_ARG (GET 'CDNARG CDIFF_OP))
      (SETQ LEN_ARG (GET 'CDLARG CDIFF_OP))
      (SETQ N_ARG (CADR LEN_ARG))
      (COND
       ((NOT (EQN N_ARG (LENGTH ODD_VAR*)))
        (REDERR
         "dimension of the arguments different from the number of odd variables")))
      (SETQ LEN_TARGET (GET 'CDTARGET CDIFF_OP))
      (MK_SUPERFUN SUPERFUN 1 LEN_TARGET)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN_TARGET I)) (RETURN NIL)))
        (SETK (LIST SUPERFUN I)
              (PROGN
               (SETQ TEMPSUPERFUN
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J 1)
                         (COND ((MINUSP (DIFFERENCE N_ARG J)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          (REVAL1
                                           (LIST CDIFF_OP I J (NTH ODD_VAR* J))
                                           NIL)
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (PLUS2 J 1))
                         (COND
                          ((MINUSP (DIFFERENCE N_ARG J))
                           (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  (REVAL1 (LIST CDIFF_OP I J (NTH ODD_VAR* J))
                                          NIL)
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
               (COND (*CHECKORD (CHECK_LETOP TEMPSUPERFUN)))
               (REVAL1 (CONS 'PLUS TEMPSUPERFUN) NIL)))
        (SETQ I (PLUS2 I 1))
        (GO LAB)))) 
(FLAG '(CONV_CDIFF2SUPERFUN) 'OPFN) 
(PUT 'AL_NUMBERP 'NUMBER-OF-ARGS 1) 
(PUT 'AL_NUMBERP 'DEFINED-ON-LINE '357) 
(PUT 'AL_NUMBERP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'AL_NUMBERP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AL_NUMBERP (L)
    (PROG (EL)
      (SETQ EL (CDR L))
     LAB
      (COND ((NULL EL) (RETURN NIL)))
      ((LAMBDA (EL)
         (COND
          ((NOT (NUMBERP EL))
           (REDERR "Error: the argument is not a list of integers"))))
       (CAR EL))
      (SETQ EL (CDR EL))
      (GO LAB))) 
(PUT 'MK_SCDIFFOP 'NUMBER-OF-ARGS 5) 
(PUT 'MK_SCDIFFOP 'DEFINED-ON-LINE '361) 
(PUT 'MK_SCDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'MK_SCDIFFOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_SCDIFFOP (SCDIFF_OP PAR NUM_ARG LEN_ARG LEN_TARGET)
    (PROG ()
      (MKOP SCDIFF_OP)
      (PUT 'SCDOP SCDIFF_OP T)
      (COND
       ((NOT (NUMBERP PAR)) (REDERR "Error: the parity must be an integer")))
      (PUT 'SCDPAR SCDIFF_OP PAR)
      (COND
       ((NOT (NUMBERP NUM_ARG))
        (REDERR "Error: the number of arguments must be an integer")))
      (PUT 'SCDNARG SCDIFF_OP NUM_ARG)
      (PROG (EL)
        (SETQ EL (CDR LEN_ARG))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL) (AL_NUMBERP EL)) (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (PUT 'SCDLARG SCDIFF_OP LEN_ARG)
      (AL_NUMBERP LEN_TARGET)
      (PUT 'SCDTARGET SCDIFF_OP LEN_TARGET))) 
(FLAG '(MK_SCDIFFOP) 'OPFN) 
(PUT 'SCDIFFP 'NUMBER-OF-ARGS 1) 
(PUT 'SCDIFFP 'DEFINED-ON-LINE '399) 
(PUT 'SCDIFFP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'SCDIFFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCDIFFP (SCDIFF_OP) (GET 'SCDOP SCDIFF_OP)) 
(PUT 'CHECK_SCDIFF_ONEARG 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK_SCDIFF_ONEARG 'DEFINED-ON-LINE '402) 
(PUT 'CHECK_SCDIFF_ONEARG 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CHECK_SCDIFF_ONEARG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK_SCDIFF_ONEARG (SCDIFF_OP)
    (PROG (NUM_ARG)
      (COND
       ((NOT (SCDIFFP SCDIFF_OP))
        (REDERR
         "Error: the first argument must be a declared s-CDiff operator")))
      (COND
       ((NOT (EQN (SETQ NUM_ARG (GET 'SCDNARG SCDIFF_OP)) 1))
        (REDERR "Error: the operator must have exactly one argument"))))) 
(PUT 'LOAD_SCDIFFOP 'NUMBER-OF-ARGS 3) 
(PUT 'LOAD_SCDIFFOP 'DEFINED-ON-LINE '412) 
(PUT 'LOAD_SCDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'LOAD_SCDIFFOP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOAD_SCDIFFOP (OPNAME INDS L_COEFF)
    (PROG (NARG)
      (COND
       ((NOT (SCDIFFP OPNAME))
        (REDERR
         "The first argument must be a declared super-C-Differential operator")))
      (SETQ NARG (GET 'SCDNARG OPNAME))
      (COND
       ((NOT (EQN NARG (DIFFERENCE (LENGTH INDS) 2)))
        (REDERR "Second argument: wrong number of indices")))
      (PROG (EL)
        (SETQ EL (CDR L_COEFF))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((NOT (EQN NARG (DIFFERENCE (LENGTH EL) 2)))
             (REDERR "Third argument: wrong number of arguments"))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (LOAD_CDIFFOP0 OPNAME (CDR INDS) (CDR (REPLACE_ODDEXT L_COEFF))))) 
(FLAG '(LOAD_SCDIFFOP) 'OPFN) 
(PUT 'ALL_COEFF_SCDIFFOP0 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_COEFF_SCDIFFOP0 'DEFINED-ON-LINE '438) 
(PUT 'ALL_COEFF_SCDIFFOP0 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'ALL_COEFF_SCDIFFOP0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_COEFF_SCDIFFOP0 (SCDOP)
    (PROG (LISTCOEFF0 L_ARG L_TAR L_ARG_TOT L_TAR_TOT TEMPCF TLIST)
      (SETQ LISTCOEFF0 (LIST))
      (CHECK_SCDIFF_ONEARG SCDOP)
      (SETQ L_ARG (CADR (GET 'SCDLARG SCDOP)))
      (SETQ L_TAR (GET 'SCDTARGET SCDOP))
      (SETQ L_ARG_TOT
              (PROG (EL FORALL-RESULT)
                (SETQ EL (CDR L_ARG))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (EL) EL) (CAR EL)) FORALL-RESULT))
                (SETQ EL (CDR EL))
                (GO LAB1)))
      (SETQ L_TAR_TOT
              (PROG (EL FORALL-RESULT)
                (SETQ EL (CDR L_TAR))
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (EL) EL) (CAR EL)) FORALL-RESULT))
                (SETQ EL (CDR EL))
                (GO LAB1)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE L_TAR_TOT I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE L_ARG_TOT J)) (RETURN NIL)))
          (PROGN
           (SETQ TLIST (LIST))
           (PROG (MIND)
             (SETQ MIND ALL_MIND*)
            LAB
             (COND ((NULL MIND) (RETURN NIL)))
             ((LAMBDA (MIND)
                (COND
                 ((NOT
                   (EQN (SETQ TEMPCF (COEFF_CDIFFOP0 SCDOP (LIST I J) MIND))
                        0))
                  (SETQ TLIST
                          (CONS (CONS 'LIST (LIST TEMPCF (CONS 'LIST MIND)))
                                TLIST)))))
              (CAR MIND))
             (SETQ MIND (CDR MIND))
             (GO LAB))
           (COND
            ((NOT TLIST)
             (SETQ TLIST
                     (LIST
                      (CONS 'LIST (LIST 0 (CONS 'LIST (NTH ALL_MIND* 1))))))))
           (SETQ TLIST (CONS 'LIST (CONS (CONS 'LIST (LIST I J)) TLIST)))
           (SETQ LISTCOEFF0 (CONS TLIST LISTCOEFF0)))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (CONS 'LIST LISTCOEFF0)))) 
(PUT 'ALL_COEFF_SCDIFFOP 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_COEFF_SCDIFFOP 'DEFINED-ON-LINE '463) 
(PUT 'ALL_COEFF_SCDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'ALL_COEFF_SCDIFFOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_COEFF_SCDIFFOP (CDOP) (REPLACE_EXTODD (ALL_COEFF_SCDIFFOP0 CDOP))) 
(FLAG '(ALL_COEFF_SCDIFFOP) 'OPFN) 
(PUT 'COEFF2SCDIFFOP 'NUMBER-OF-ARGS 2) 
(PUT 'COEFF2SCDIFFOP 'DEFINED-ON-LINE '468) 
(PUT 'COEFF2SCDIFFOP 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'COEFF2SCDIFFOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFF2SCDIFFOP (CDOP L_PAR)
    (PROG (L_PAR0 CHANGE_EXPAND_TD)
      (CHECK_SCDIFF_ONEARG CDOP)
      (SETQ L_PAR0 (CDR (REPLACE_ODDEXT L_PAR)))
      (SETQ CHANGE_EXPAND_TD 0)
      (COND
       ((EQ (GET 'TD 'SIMPFN) 'COMPUTE_TD)
        (PROGN (SETQ CHANGE_EXPAND_TD 1) (NOEXPAND_TD))))
      (PUT 'ODDPROD* 'SIMPFN 'SIMPIDEN)
      (PROG (IND_PAR)
        (SETQ IND_PAR L_PAR0)
       LAB
        (COND ((NULL IND_PAR) (RETURN NIL)))
        ((LAMBDA (IND_PAR) (COEFF2CDIFFOP_ENTRY CDOP IND_PAR)) (CAR IND_PAR))
        (SETQ IND_PAR (CDR IND_PAR))
        (GO LAB))
      (PUT 'ODDPROD* 'SIMPFN 'EV_ODD_PRODUCT)
      (COND ((EQN CHANGE_EXPAND_TD 1) (EXPAND_TD))))) 
(FLAG '(COEFF2SCDIFFOP) 'OPFN) 
(PUT 'CDE_CDIFF 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_CDIFF 'DEFINED-ON-LINE '492) 
(PUT 'CDE_CDIFF 'DEFINED-IN-FILE 'CDE/CDE_CDIFF.RED) 
(PUT 'CDE_CDIFF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_CDIFF NIL (PRIN2 "")) 
(ENDMODULE) 