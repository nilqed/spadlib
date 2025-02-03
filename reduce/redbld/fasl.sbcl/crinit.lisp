(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CRACKINIT)) 
(SETQ NOT_PASSED_BACK
        '(ASYMPLIS* *BATCH_MODE *COMPLEX *ICONIC ADJUST_FNC ALG_POLY ALLFLAGS_
          AUTO_PARA_MODE BATCHCOUNT_ BATCH_MODE_SUB CALL_CRACK_OUT CASE_LIST
          CHOOSE_6_20_MAX_FTEM CHOOSE_6_20_MAX_TERMS CHOOSE_27_8_16_MAX
          CHOOSE_30_47_21_MAX CHOOSE_70_65_8_47_ORIGMEM
          CHOOSE_70_65_8_47_ORIGTERMS CHOOSE_70_65_8_47_RATIOMEM
          CHOOSE_70_65_8_47_RATIOTERMS CHOOSE_30_47_72_EQN CHOOSE_11_30_MAX_11
          CHOOSE_11_30_MAX_30 COLLECT_SOL CONFIRM_SUBST CONT_ CONTRADICTION_
          COST_LIMIT5 DEPL* CRACK_DOWNLOAD_ADD CRACK_INI_FILE
          CRACK_LOAD_COMMAND CURRENTLY_TO_BE_SUBSTITUTED_IN
          DEC_DEPTH_FIRST_SEARCH DIFFELIM_STEPS EQN_INPUT EQN_NO EQNAME_
          EQN_TO_BE_GEN EQUATIONS_FILE EXPERT_MODE EXPLOG_ FACINT_ FHOM_ FLIN_
          FNAME_ FNEW_ FORM_COMP FORM_EQN_IDX FORM_EQN_ON_DISK FORM_MAX_READ
          FREEABS_ FREEINT_ FSUB_ FTEM_ GENINT_ GROEB_DIFF_MAX GROEB_SOLVE
          HIGH_GENSEP IDNTIES_ INDEPENDENCE_ INEQ_ INEQ_OR INTER_DIVINT
          KEEP_CASE_TREE KEEP_PARTI KORD* LARGEST_FULLY_SHORTENED
          LAST_FREE_CELLS LAST_STEPS LAZY_EVAL LENGTH_INC_ALG LENGTH_INC_DEC
          LEX_DF LEX_FC LIN_PROBLEM LOGOPRINT_ LOW_GENSEP MAX_COEFF_LEN
          MAX_GC_ELIMIN MAX_GC_FAC MAX_GC_GB MAX_GC_INT MAX_GC_MINSUB
          MAX_GC_ODE MAX_GC_RED_LEN MAX_GC_REVAL MAX_GC_SHORT
          MAX_GC_SPEC_ALG_SOL MAX_GC_SS MAX_PROC_NO MAX_RED_LEN MAXALGSYS_
          MAX_TERM_TO_FAC_COMPLEX MAX_TERM_TO_FAC_REAL MAX_TERM_TO_PRED NEQU_
          MODULAR_COMP NEW_GENSEP ODESOLVE_ PARACRACK_INITIALIZED POLY_ONLY
          POTINT_ PRINT_ PRINT_ALL PRINT_MORE PROC_LIST_ PROCESS_COUNTER
          PVM_ABLE QUICK_DECOUP RECORD_HIST RECYCLE_EQNS RECYCLE_FCTS
          REDUCE_CALL REPEAT_MODE SAFEINT_ SESSION_ SINGULAR_CALL SINGULAR_LIB
          SINGULAR_TIME SIZE_WATCH SOLVEALG_ STOP_ STRUC_EQN SUBST_0 SUBST_1
          SUBST_2 SUBST_3 SUBST_4 TARGET_LIMIT_0 TARGET_LIMIT_1 TARGET_LIMIT_2
          TARGET_LIMIT_3 TIME_ TO_DO_LIST TR_DECOUPLE TR_GENINT TR_GENSEP
          TR_MAIN TR_ORDERINGS TR_SHORT TR_REDLENGTH UD_1 UD_2 UD_3 UD_4
          USERRULES_ VERIFY_END_OF_PARALLEL_RUN VL_)) 
(SETQ PASSED_BACK
        '(DONE_TRAFO HISTORY_ LIMIT_TIME LEVEL_ NFCT_ SIZE_HIST SOL_LIST
          STEPCOUNTER_ TIME_LIMIT INVERSE_TRAFO_LIST_INCOMPLETE
          SOL_LIST_FILE_CREATED)) 
(SETQ GLOBAL_LIST_INTEGER
        '(ODESOLVE_ SUBST_0 SUBST_1 SUBST_2 SUBST_3 TARGET_LIMIT_0
          TARGET_LIMIT_1 TARGET_LIMIT_2 COST_LIMIT5 MAX_GC_FAC MAX_GC_GB
          MAX_GC_ODE MAX_GC_RED_LEN MAX_GC_REVAL MAX_GC_SHORT MAX_GC_SS
          MAXALGSYS_ MAX_EQN_TO_CONTI NFCT_ NEQU_ LOW_GENSEP HIGH_GENSEP)) 
(SETQ GLOBAL_LIST_NINTEGER
        '(GENINT_ FACINT_ NEW_GENSEP TARGET_LIMIT_0 TARGET_LIMIT_1
          TARGET_LIMIT_2 TARGET_LIMIT_3 PRINT_)) 
(SETQ GLOBAL_LIST_FLOAT '(LENGTH_INC_ALG LENGTH_INC_DEC)) 
(SWITCH (LIST 'BATCH_MODE)) 
(COMPILETIME (GLOBAL '(GROEBRESMAX PLOTHEADER*))) 
(PUT 'NAME_SESSION 'NUMBER-OF-ARGS 0) 
(PUT 'NAME_SESSION 'DEFINED-ON-LINE '117) 
(PUT 'NAME_SESSION 'DEFINED-IN-FILE 'CRACK/CRINIT.RED) 
(PUT 'NAME_SESSION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NAME_SESSION NIL
    (PROG ()
      (RANDOM_INIT)
      (SETQ SESSION_ (BLDMSG_INTERNAL "%w%d-" (LIST "bu" (RANDOM 1000000))))
      (SETQ SOL_LIST_FILE_CREATED NIL))) 
(PUT 'START_SOL_LIST_FILE 'NUMBER-OF-ARGS 0) 
(PUT 'START_SOL_LIST_FILE 'DEFINED-ON-LINE '125) 
(PUT 'START_SOL_LIST_FILE 'DEFINED-IN-FILE 'CRACK/CRINIT.RED) 
(PUT 'START_SOL_LIST_FILE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE START_SOL_LIST_FILE NIL
    (COND
     ((NULL SOL_LIST_FILE_CREATED)
      (PROG ()
        (SETQ SOL_LIST_FILE_CREATED T)
        (SETQ SOL_LIST NIL)
        (SAVE_SOL_LIST))))) 
(FLAG '(SETCRACKFLAGS) 'OPFN) 
(PUT 'SETCRACKFLAGS 'NUMBER-OF-ARGS 0) 
(PUT 'SETCRACKFLAGS 'DEFINED-ON-LINE '140) 
(PUT 'SETCRACKFLAGS 'DEFINED-IN-FILE 'CRACK/CRINIT.RED) 
(PUT 'SETCRACKFLAGS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SETCRACKFLAGS NIL
    (PROGN
     (SETQ ONE_ARGUMENT_FUNCTIONS_
             '(ABS ACOS ACOSD ACOSH ACOT ACOTD ACOTH ACSC ACSCD ACSCH ASEC
                   ASECD ASECH ASIN ASIND ASINH ATAN ATAND ATANH CBRT COS COSD
                   COSH COT COTD COTH CSC CSCD CSCH EXP HYPOT LN LOG LOGB LOG10
                   SEC SECD SECH SIN SIND SINH SQRT TAN TAND TANH MINUS))
     (SETQ REDUCEFUNCTIONS_
             (APPEND ONE_ARGUMENT_FUNCTIONS_
                     '(ATAN2 ATAN2D FACTORIAL PLUS DIFFERENCE DF TIMES QUOTIENT
                             EXPT INT)))
     (SETQ ALLFLAGS_
             '(TO_EVAL TO_FULLINT TO_INT TO_SEP TO_CASESEP TO_GENSEP
               TO_CASEGENSEP TO_DECOUP TO_DIFF TO_UNDER TO_SEPARANT))
     (SETQ PROP_LIST
             '(SQVAL FAC PVAL PARTITIONED KERN NON_RAT_KERN FCT_KERN_LIN
               FCT_KERN_NLI FCTS FCT_HOM NFCT_LIN VARS NVARS LEVEL DERIVS
               NO_DERIVS FCTEVAL_LIN FCTEVAL_NCA FCTEVAL_NLI FCTEVAL_N2L
               FCT_NLI_LIN FCT_NLI_NCA FCT_NLI_NLI FCT_NLI_NUS TERMS LENGTH
               PRINTLENGTH RATIONAL NONRATIONAL ALLVARFCTS STARDE DEC_WITH
               DEC_WITH_RL RL_WITH RES_WITH HOM_DEG SPLIT_TEST LINEAR_ HISTRY_
               NOT_TO_EVAL CASE2SEP))
     (SETQ FULL_PROC_LIST_
             '(TO_DO SEPARATION SUBST_LEVEL_0 SUBST_LEVEL_03 SUBST_LEVEL_05
               SUBST_LEVEL_45 QUICK_INTEGRATION FACTORIZE_TO_SUBSTITUTE
               SUBST_DERIVATIVE QUICK_GEN_SEPARATION ALG_LENGTH_REDUCTION
               DROP_LIN_DEP FIND_1_TERM_EQN TRIAN_LIN_ALG SUBST_LEVEL_1
               SUBST_LEVEL_3 SUBST_LEVEL_5 SUBST_LEVEL_2 SUBST_LEVEL_33
               SUBST_LEVEL_35 SUBST_LEVEL_4 UNDETLINODE UNDETLINPDE
               FULL_INTEGRATION INTEGRATION GEN_SEPARATION
               DIFF_LENGTH_REDUCTION DEL_REDUNDANT_DE IDTY_INTEGRATION
               DECOUPLING ADD_DIFFERENTIATED_PDES ADD_DIFF_ISE MULTINTFAC
               ALG_SOLVE_SINGLE ALG_SOLVE_SYSTEM UNDO_SUBST_DERIVATIVE
               CHANGE_PROC_LIST STOP_BATCH FIND_TRAFO DEL_REDUNDANT_FC
               SUB_PROBLEM DROP_DEP_BI_LIN FIND_FACTOR_BI_LIN SPLIT_INTO_CASES
               SUBST_LEVEL_04 FIRST_INT_FOR_ODE FACTORIZE_ANY GEN_SEPARATION2
               FIND_AND_USE_SUB_SYSTEMS12 FIND_AND_USE_SUB_SYSTEMS13
               FIND_AND_USE_SUB_SYSTEMS14 FIND_AND_USE_SUB_SYSTEMS15
               FIND_AND_USE_SUB_SYSTEMS22 FIND_AND_USE_SUB_SYSTEMS23
               FIND_AND_USE_SUB_SYSTEMS24 FIND_AND_USE_SUB_SYSTEMS25
               HIGH_PRIO_DECOUPLING USER_DEFINED EXTERNAL_GROEBNER
               SOLUTION_CHECK1 CHOOSE_6_20 CHOOSE_27_8_16 CHOOSE_30_47_21
               SOLUTION_CHECK2 CASE_ON_MOST_FREQU_FNC BACK_UP
               ALG_LENGTH_REDUCE_1 CHECK_INEQ CASE_SEPARATION
               PRE_DETERMINED_CASE_SPLITS CHOOSE_70_65_8_47 READ_EQUATION
               CHOOSE_30_47_72 CASE_ON_MOST_FREQU_FACTORS CHOOSE_11_30
               TRY_OTHER_ORDERING FIND_FACTORIZATION PARA_SOLVE_HOM_EQU
               UNDETALG SUBST_POWER BOTTOM_UP_SUBST SOLUTION_CHECK3 SUBLINFCT
               CASE_GEN_SEPARATION REPLACE_EQUATION DO_ONE_RESULTANT
               INHOM_DECOUPLING LINEARIZE_BI_LIN QUIT_IF_NO_ALG_SOL
               GET_SPECIAL_ALG_SOL1 GET_SPECIAL_ALG_SOL2))
     (PROG (H)
       (SETQ H 1)
      LAB
       (COND ((MINUSP (DIFFERENCE (LENGTH FULL_PROC_LIST_) H)) (RETURN NIL)))
       (PUT (NTH FULL_PROC_LIST_ H) 'NO H)
       (SETQ H (PLUS2 H 1))
       (GO LAB))
     (SETQ DEFAULT_PROC_LIST_
             '(TO_DO SEPARATION SUBST_LEVEL_0 SUBST_LEVEL_03 QUICK_INTEGRATION
               FIND_FACTORIZATION FACTORIZE_TO_SUBSTITUTE FACTORIZE_ANY
               SUBST_DERIVATIVE SUBST_LEVEL_1 FULL_INTEGRATION SUBST_LEVEL_3
               SUBST_LEVEL_2 SUBST_LEVEL_33 SUBST_LEVEL_35 SUBST_LEVEL_4
               GEN_SEPARATION CASE_SEPARATION CASE_GEN_SEPARATION INTEGRATION
               DIFF_LENGTH_REDUCTION DECOUPLING UNDETLINODE TRY_OTHER_ORDERING
               ALG_SOLVE_SINGLE UNDO_SUBST_DERIVATIVE))
     (PUT '*ALLOWDFINT_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*DFPRINT_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*DIFFELIMVERBOSITY* 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*EXP_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*EZGCD_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*FULLROOTS_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*GCD_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*MCD_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*NOPOWERS_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*RATARG_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*RATIONAL_BAK 'DESCRIPTION (LIST "backup value of switch"))
     (PUT '*BATCH_MODE 'DESCRIPTION (LIST "backup value of switch"))
     (PUT 'ALLFLAGS_ 'DESCRIPTION
          (LIST "list of all flags characterizing each equations"))
     (PUT 'BATCHCOUNT_ 'DESCRIPTION
          (LIST "counter of steps in automatic mode"))
     (PUT 'BACKUP_ 'DESCRIPTION
          (LIST "mainly used to read expressions from files"))
     (PUT 'CONTRADICTION_ 'DESCRIPTION
          (LIST
           "variable to communicate to calling routines that a contradiction resulted forbidding any solutions"))
     (PUT 'FNEW_ 'DESCRIPTION
          (LIST "temporary list of new functions/constants in crint.red"))
     (PUT 'FSUB_ 'DESCRIPTION
          (LIST
           "if lazy_eval=t then the list of substitutions still to be done in forg"))
     (PUT 'FTEM_ 'DESCRIPTION
          (LIST
           "current list of unknown functions and constants to be determined"))
     (PUT 'FULL_PROC_LIST_ 'DESCRIPTION
          (LIST "complete list of (automatic) modules"))
     (PUT 'GCFREE* 'DESCRIPTION (LIST "number of free cells"))
     (PUT 'GLOBAL_LIST_INTEGER 'DESCRIPTION
          (LIST "list of variables that are integers"))
     (PUT 'GLOBAL_LIST_NINTEGER 'DESCRIPTION
          (LIST "list of variables that are integers or nil"))
     (PUT 'GLOBAL_LIST_FLOAT 'DESCRIPTION
          (LIST "list of floating point variables"))
     (PUT 'HISTORY_ 'DESCRIPTION (LIST "list of interactive input"))
     (PUT 'INEQ_ 'DESCRIPTION (LIST "list of inequalities in sq-form"))
     (PUT 'INEQ_OR 'DESCRIPTION
          (LIST "list of OR-inequalities each being a list"))
     (PUT 'LIN_TEST_CONST 'DESCRIPTION
          (LIST "a global variable used to determine linearity of equations"))
     (PUT 'MAX_EQN_TO_CONTI 'DESCRIPTION
          (LIST
           "the maximal number of equations to continue in batch_mode (automatic execution)"))
     (PUT 'ONE_ARGUMENT_FUNCTIONS 'DESCRIPTION
          (LIST "list of functions iin REDUCE with one argument"))
     (PUT 'PARACRACK_INITIALIZED 'DESCRIPTION
          (LIST "=t if file .reducerc initializes parallelization"))
     (PUT 'NOT_PASSED_BACK 'DESCRIPTION
          (LIST
           "variable names whos values are only passed on to subcalls of crackmain"))
     (PUT 'PASSED_BACK 'DESCRIPTION
          (LIST
           "variable names with values passed back from subcalls of crackmain"))
     (PUT 'PROP_LIST 'DESCRIPTION (LIST "list of properties of equations"))
     (PUT 'PVM_ABLE 'DESCRIPTION
          (LIST "flag whether current session runs in Parallel-Reduce"))
     (PUT 'RECYCLE_EQNS 'DESCRIPTION
          (LIST "list of re-usable equation names previously in use"))
     (PUT 'RECYCLE_FCTS 'DESCRIPTION
          (LIST "list of re-usable function names previously in use"))
     (PUT 'RECYCLE_IDS 'DESCRIPTION
          (LIST "list of re-usable identity names previously in use"))
     (PUT 'REDUCEFUNCTIONS 'DESCRIPTION (LIST "the list of REDUCE functions"))
     (PUT 'REDUCERC_INITIALIZED 'DESCRIPTION
          (LIST
           "used in crpvm to check whether the file .reducerc with pvm initializations was loaded at the start of REDUCE"))
     (PUT 'SESSION_ 'DESCRIPTION (LIST "the name of the session"))
     (PUT 'SIZE_HIST 'DESCRIPTION
          (LIST
           "list of data about the history of the computation, see size_watch whih determines whether size_hist is created during computation"))
     (PUT 'STEPCOUNTER_ 'DESCRIPTION (LIST "counter od steps"))
     (PUT 'TO_DO_LIST 'DESCRIPTION
          (LIST "list of most urgent steps done next"))
     (PUT 'USERRULES_ 'DESCRIPTION (LIST "list of user defined LET-rules"))
     (PUT 'VL_ 'DESCRIPTION (LIST "list of independent variables"))
     (PUT 'DEFAULT_PROC_LIST_ 'DESCRIPTION
          (LIST "default priority list of procedures"))
     (SETQ VERIFY_END_OF_PARALLEL_RUN T)
     (PUT 'VERIFY_END_OF_PARALLEL_RUN 'DESCRIPTION
          (LIST
           "whether at the end a parallel run shall stop to verify the that all went well"))
     (SETQ PROC_LIST_ DEFAULT_PROC_LIST_)
     (PUT 'PROC_LIST_ 'DESCRIPTION (LIST "priority list of procedure in use"))
     (SETQ *UNCACHED T)
     (PUT '*UNCACHED 'DESCRIPTION
          (LIST
           "if nil then any long computations with >10,000 algebraic mode steps or expressions read from files become extremely slow"))
     (SETQ *BATCH_MODE T)
     (PUT '*BATCH_MODE 'DESCRIPTION (LIST "running crack in batchmode"))
     (SETQ EXPERT_MODE NIL)
     (PUT 'EXPERT_MODE 'DESCRIPTION
          (LIST
           "is set with command 't', if expert_mode=t then user has to select equations to be worked with in all steps"))
     (SETQ REPEAT_MODE NIL)
     (PUT 'REPEAT_MODE 'DESCRIPTION
          (LIST
           "is set in non batch mode, counter for number of times a step is to be repeated"))
     (COND ((NOT (FIXP NFCT_)) (SETQ NFCT_ 1)))
     (PUT 'NFCT_ 'DESCRIPTION
          (LIST "index of next new functions and constants to be initialized"))
     (SETQ NEQU_ 1)
     (PUT 'NEQU_ 'DESCRIPTION
          (LIST "index of next new equation to be initialized"))
     (SETQ NID_ 1)
     (PUT 'NID_ 'DESCRIPTION
          (LIST "index of next new identity to be initialized"))
     (SETQ FNAME_ 'C_)
     (PUT 'FNAME_ 'DESCRIPTION
          (LIST
           "name of new functions and constants generated, e.g.. in integrations"))
     (SETQ EQNAME_ 'E_)
     (PUT 'EQNAME_ 'DESCRIPTION (LIST "name of new equations"))
     (SETQ IDNAME_ 'ID_)
     (PUT 'IDNAME_ 'DESCRIPTION (LIST "name of new identities"))
     (SETQ LEVEL_ NIL)
     (PUT 'LEVEL_ 'DESCRIPTION (LIST "actual level of crack recursion"))
     (SETQ CONT_ NIL)
     (PUT 'CONT_ 'DESCRIPTION
          (LIST
           "interactive user control for integration or substitution of large expressions is disabled"))
     (SETQ INDEPENDENCE_ NIL)
     (PUT 'INDEPENDENCE_ 'DESCRIPTION
          (LIST "interactive control of linear independence disabled"))
     (SETQ GENINT_ 15)
     (PUT 'GENINT_ 'DESCRIPTION
          (LIST
           "if =nil then generalized integration disabled else the maximal number of new functions and extra equations due to generalized integration"))
     (SETQ FACINT_ 1000)
     (PUT 'FACINT_ 'DESCRIPTION
          (LIST
           "=nil then no search for integrating factors otherwise max product terms*kernels for investigation"))
     (SETQ POTINT_ T)
     (PUT 'POTINT_ 'DESCRIPTION (LIST "allowing `potential integration'"))
     (SETQ SAFEINT_ T)
     (PUT 'SAFEINT_ 'DESCRIPTION
          (LIST "uses only solutions of ODEs with non-vanishing denom."))
     (SETQ FREEINT_ T)
     (PUT 'FREEINT_ 'DESCRIPTION
          (LIST "Do only integrations if expl. part is integrable"))
     (SETQ FREEABS_ T)
     (PUT 'FREEABS_ 'DESCRIPTION
          (LIST "Allow only solutions of ODEs without ABS()"))
     (SETQ ODESOLVE_ 100)
     (PUT 'ODESOLVE_ 'DESCRIPTION
          (LIST
           "maximal length of a de (number of terms) to be integrated as ode"))
     (SETQ LOW_GENSEP 6)
     (PUT 'LOW_GENSEP 'DESCRIPTION
          (LIST
           "max. size of expressions to separate in a generalized way with higher priority"))
     (SETQ HIGH_GENSEP 300)
     (PUT 'HIGH_GENSEP 'DESCRIPTION
          (LIST
           "min. size of expressions to separate in a generalized way with higher priority"))
     (SETQ NEW_GENSEP NIL)
     (PUT 'NEW_GENSEP 'DESCRIPTION
          (LIST "whether or not a new form of gensep should be used"))
     (SETQ MY_GC_COUNTER 0)
     (PUT 'MY_GC_COUNTER 'DESCRIPTION (LIST "initialization of my_gc_counter"))
     (SETQ MAX_GC_COUNTER 100000000)
     (PUT 'MAX_GC_COUNTER 'DESCRIPTION
          (LIST "max. number of garbage collections"))
     (SETQ MAX_GC_REVAL 1)
     (PUT 'MY_GC_REVAL 'DESCRIPTION
          (LIST "maximal number of garbage collections for simplification"))
     (SETQ MAX_GC_SHORT 40)
     (PUT 'MAX_GC_SHORT 'DESCRIPTION
          (LIST "maximal number of garbage collections during shortening"))
     (SETQ MAX_GC_SPEC_ALG_SOL 30)
     (PUT 'MAX_GC_SPEC_ALG_SOL 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during get_special_alg_sol"))
     (SETQ MAX_GC_SS 10)
     (PUT 'MAX_GC_SS 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during search of sub_systems"))
     (SETQ MAX_GC_RED_LEN 30)
     (PUT 'MAX_GC_RED_LEN 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during length reduction"))
     (SETQ MAX_GC_FAC 15)
     (PUT 'MAX_GC_FAC 'DESCRIPTION
          (LIST "maximal number of garbage collections during factorization"))
     (SETQ MAX_GC_ELIMIN 20)
     (PUT 'MAX_GC_ELIMIN 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during elimination in decoupling"))
     (SETQ MAX_GC_GB 100)
     (PUT 'MAX_GC_GB 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during Groebner Base computation, this is only for the time reading in the result of the successful external GB computation so once, e.g. Singular has computed the GB, there should be enough time given to read the result into CRACK"))
     (SETQ MAX_GC_MINSUB 10)
     (PUT 'MAX_GC_MINSUB 'DESCRIPTION
          (LIST
           "maximal number of garbage collections during search for minimal growth substitution"))
     (SETQ MAX_GC_ODE 1)
     (PUT 'MAX_GC_ODE 'DESCRIPTION
          (LIST
           "the maximal number of garbage collections during a call of ODESOLVE (just to avoid hanging)"))
     (SETQ MAX_RED_LEN 1000000)
     (PUT 'MAX_RED_LEN 'DESCRIPTION
          (LIST
           "max product of lengths of equations to be length reduced with the decouling procedure"))
     (SETQ MAX_EQN_TO_CONTI 0)
     (PUT 'MAX_EQN_TO_CONT 'DESCRIPTION
          (LIST
           "used in stop_batch() to stop only if more than max_eqn_to_conti equations are unsolved"))
     (SETQ MAX_GC_INT 1)
     (PUT 'MAX_GC_INT 'DESCRIPTION
          (LIST "maximal number of garbage collections during integration"))
     (SETQ MODULAR_COMP NIL)
     (PUT 'MODULAR_COMP 'DESCRIPTION
          (LIST
           "whether numerical coefficients in equations shall be computed modulo a prime number"))
     (SETQ SUBST_0 2)
     (PUT 'SUBST_0 'DESCRIPTION
          (LIST
           "maximal printlength of an expression to be substituted in substitutions using this parameter"))
     (SETQ SUBST_1 8)
     (PUT 'SUBST_1 'DESCRIPTION
          (LIST
           "maximal printlength of an expression to be substituted in substitutions using this parameter"))
     (SETQ SUBST_2 20)
     (PUT 'SUBST_2 'DESCRIPTION
          (LIST
           "maximal printlength of an expression to be substituted in substitutions using this parameter"))
     (SETQ SUBST_3 (EXPT 10 3))
     (PUT 'SUBST_3 'DESCRIPTION
          (LIST
           "maximal printlength of an expression to be substituted in substitutions using this parameter"))
     (SETQ SUBST_4 NIL)
     (PUT 'SUBST_4 'DESCRIPTION
          (LIST
           "maximal printlength of an expression to be substituted in substitutions using this parameter"))
     (SETQ COST_LIMIT5 100)
     (PUT 'COST_LIMIT5 'DESCRIPTION
          (LIST
           "maximal number of extra terms generated by a substitution using this parameter"))
     (SETQ TARGET_LIMIT_0 (EXPT 10 2))
     (PUT 'TARGET_LIMIT_0 'DESCRIPTION
          (LIST
           "maximal product length(pde)*length(subst_expr) in substitutions using this parameter"))
     (SETQ TARGET_LIMIT_1 (EXPT 10 3))
     (PUT 'TARGET_LIMIT_1 'DESCRIPTION
          (LIST
           "maximal product length(pde)*length(subst_expr) in substitutions using this parameter, if nil then no length limit"))
     (SETQ TARGET_LIMIT_2 (EXPT 10 5))
     (PUT 'TARGET_LIMIT_2 'DESCRIPTION
          (LIST
           "maximal product length(pde)*length(subst_expr) in substitutions using this parameter, if nil then no length limit"))
     (SETQ TARGET_LIMIT_3 NIL)
     (PUT 'TARGET_LIMIT_3 'DESCRIPTION
          (LIST
           "maximal product length(pde)*length(subst_expr) in substitutions using this parameter, if nil then no length limit"))
     (SETQ LENGTH_INC_DEC 1.0)
     (PUT 'LENGTH_INC_DEC 'DESCRIPTION
          (LIST
           "factor by which the length of an expression may grow during decoupling"))
     (SETQ LENGTH_INC_ALG 1.0)
     (PUT 'LENGTH_INC_ALG 'DESCRIPTION
          (LIST
           "factor by which the length of an expression may grow during shortening compared to the shorter one used"))
     (SETQ TR_MAIN NIL)
     (PUT 'TR_MAIN 'DESCRIPTION (LIST "Trace main procedure"))
     (SETQ TR_GENSEP NIL)
     (PUT 'TR_GENSEP 'DESCRIPTION (LIST "Trace generalized separation"))
     (SETQ TR_GENINT NIL)
     (PUT 'TR_GENINT 'DESCRIPTION (LIST "Trace generalized integration"))
     (SETQ TR_DECOUPLE NIL)
     (PUT 'TR_DECOUPLE 'DESCRIPTION (LIST "Trace decoupling process"))
     (SETQ TR_REDLENGTH NIL)
     (PUT 'TR_REDLENGTH 'DESCRIPTION (LIST "Trace length reduction"))
     (SETQ TR_ORDERINGS NIL)
     (PUT 'TR_ORDERINGS 'DESCRIPTION (LIST "Trace orderings stuff"))
     (SETQ TR_SHORT NIL)
     (PUT 'TR_SHORT 'DESCRIPTION (LIST "Trace the algebraic shortening"))
     (SETQ SOLVEALG_ NIL)
     (PUT 'SOLVEALG_ 'DESCRIPTION (LIST "Use SOLVE for algebraic equations"))
     (SETQ PRINT_MORE NIL)
     (PUT 'PRINT_MORE 'DESCRIPTION
          (LIST "Print more informations about the pdes"))
     (SETQ PRINT_ALL NIL)
     (PUT 'PRINT_ALL 'DESCRIPTION
          (LIST "Print all informations about the pdes"))
     (SETQ LOGOPRINT_ T)
     (PUT 'LOGOPRINT_ 'DESCRIPTION (LIST "print logo for crack call"))
     (SETQ POLY_ONLY NIL)
     (PUT 'POLY_ONLY 'DESCRIPTION (LIST "all equations are polynomials only "))
     (SETQ TIME_ NIL)
     (PUT 'TIME_ 'DESCRIPTION
          (LIST "print the time needed between top level commands"))
     (SETQ PRINT_ 12)
     (PUT 'PRINT_ 'DESCRIPTION
          (LIST "maximal length of an expression to be printed"))
     (SETQ MAXALGSYS_ 20)
     (PUT 'MAXALGSYS_ 'DESCRIPTION
          (LIST "max. number of equations to be solved in specialsol"))
     (SETQ ADJUST_FNC NIL)
     (PUT 'ADJUST_FNC 'DESCRIPTION
          (LIST
           "if t then free constants/functions are scaled and redundant ones are droped to simplify the result"))
     (SETQ LEX_DF NIL)
     (PUT 'LEX_DF 'DESCRIPTION
          (LIST
           "if t then use lex. instead of tot. degree ordering of derivatives"))
     (SETQ LEX_FC T)
     (PUT 'LEX_FC 'DESCRIPTION
          (LIST
           "if t then lex. ordering of functions has higher priority than any ordering of derivatives, even if lex_fc=nil then still algebraic powers have no influence, i.e. for algebraic problems both are lexicographical ordering"))
     (SETQ COLLECT_SOL T)
     (PUT 'COLLECT_SOL 'DESCRIPTION
          (LIST
           "whether solutions found shall be collected and returned together at the end or not (to save memory)"))
     (SETQ STRUC_EQN NIL)
     (PUT 'STRUC_EQN 'DESCRIPTION
          (LIST "whether the equations has the form of structural eqn."))
     (SETQ QUICK_DECOUP NIL)
     (PUT 'QUICK_DECOUP 'DESCRIPTION
          (LIST
           "whether decoupling should be done faster with less care for saving memory"))
     (SETQ IDNTIES_ NIL)
     (PUT 'IDNTIES_ 'DESCRIPTION
          (LIST
           "list of identities resulting from reductions and integrability conditions"))
     (SETQ RECORD_HIST NIL)
     (PUT 'RECORD_HIST 'DESCRIPTION
          (LIST
           "whether the history of equations is to be recorded. This slows down large problems because of the prefix form of the history expressions which can become large too if coefficients are large."))
     (SETQ KEEP_PARTI NIL)
     (PUT 'KEEP_PARTI 'DESCRIPTION
          (LIST
           "whether for each equation a copy in partitioned form is to be stored to speed up several simplifications"))
     (SETQ SIZE_WATCH NIL)
     (PUT 'SIZE_WATCH 'DESCRIPTION
          (LIST
           "whether before each computational step the size of the system shall be recorded in size_hist
=nil: not, --> plot_stat() and print_tree() are not applicable. In history_ the start and end of cases are not stored as comments.
=#: then only the last # lines are stored --> warning this affects plot_stat(), print_tree() and possibly the procedures choose_...(). In history_ the start and end of cases are not stored as comments.
=t: yes --> warning: size_hist can grow large with 10,000's of lines"))
     (SETQ INTER_DIVINT NIL)
     (PUT 'INTER_DIVINT 'DESCRIPTION
          (LIST
           "whether the integration of divergence identities with more than 2 differentiation variables shall be confirmed interactively"))
     (SETQ DO_RECYCLE_EQN T)
     (PUT 'DO_RECYCLE_EQN 'DESCRIPTION
          (LIST
           "whether equation names shall be recycled or not (saves memory but is less clear when determining histry_ in terms of original equations) recycle_eqns is a pair of 2 lists: (ready to use eqn. names) . (free eqn. names which still may occur in prob_list)"))
     (SETQ DO_RECYCLE_FNC NIL)
     (PUT 'DO_RECYCLE_FNC 'DESCRIPTION
          (LIST
           "whether function names shall be recycled or not (saves memory but is less clear to follow) for nonlinear problems with different solution branches and free/unsolved functions of different numbers of arguments it must be nil as otherwise clash of same function names of different indep. var."))
     (SETQ OLD_HISTORY NIL)
     (PUT 'OLD_HISTORY 'DESCRIPTION
          (LIST "old_history is interactive input to be read from this list"))
     (SETQ CONFIRM_SUBST NIL)
     (PUT 'CONFIRM_SUBST 'DESCRIPTION
          (LIST
           "whether substitutions and the order of subcase investigations has to be confirmed"))
     (SETQ MEM_EFF T)
     (PUT 'MEM_EFF 'DESCRIPTION
          (LIST "whether to be memory efficient even if slower"))
     (SETQ FORCE_SEP NIL)
     (PUT 'FORCE_SEP 'DESCRIPTION
          (LIST
           "whether direct separation should be forced even if functions occur in the supposed to be linear independent explicit expressions (for non-lin. prob.)"))
     (SETQ FLIN_ NIL)
     (PUT 'FLIN_ 'DESCRIPTION
          (LIST
           "a list of functions occuring only linearly in an otherwise non-linear problem. This matters in a factorization when factors with functions of flin_
 are considered last."))
     (SETQ LAST_STEPS NIL)
     (PUT 'LAST_STEPS 'DESCRIPTION
          (LIST "a list of the last steps to avoid cycles"))
     (COND ((NULL LIN_TEST_CONST) (SETQ LIN_TEST_CONST (GENSYM))))
     (PUT 'LIN_TEST_CONS 'DESCRIPTION
          (LIST "a global fixed constant to check linearity"))
     (SETQ LIN_PROBLEM NIL)
     (PUT 'LIN_PROBLEM 'DESCRIPTION
          (LIST "whether the full problem is linear or not"))
     (SETQ TIME_LIMIT NIL)
     (PUT 'TIME_LIMIT 'DESCRIPTION
          (LIST "whether a time limit limit_time is set after
		   which batch-mode is interrupted to interactive mode"))
     (SETQ LIMIT_TIME 0)
     (PUT 'LIMIT_TIME 'DESCRIPTION
          (LIST "= time()+how-much-more-time-allowed-in-batch-mode"))
     (SETQ GROEBRESMAX 2000)
     (PUT 'GROEBRESMAX 'DESCRIPTION
          (LIST "parameter for the REDUCE Groebner Basis proram"))
     (SETQ *ICONIC NIL)
     (PUT '*ICONIC 'DESCRIPTION
          (LIST
           "whether new processes in parallelization should appear as icons"))
     (SETQ DONE_TRAFO (LIST 'LIST))
     (PUT 'DONE_TRAFO 'DESCRIPTION
          (LIST "a list of backtransformations of done transformations"))
     (SETQ INVERSE_TRAFO_LIST_INCOMPLETE NIL)
     (PUT 'INVERSE_TRAFO_LIST_INCOMPLETE 'DESCRIPTION (LIST ""))
     (SETQ GROEB_SOLVE 'REDUCE)
     (PUT 'GROEB_SOLVE 'DESCRIPTION
          (LIST "which Groebner package to use and how, options:
 'SL_LEX  for Singular in mode 'lex
 'SL_GRAD for Singular in mode 'gradlex
 'SL_REVGRAD for Singular in mode 'revgradlex
 'GB_LEX  for Faugere's GB in mode 'lex
 'GB_REVGRAD for Faugere's GB in mode 'revgradlex
 'REDUCE  for the REDUCE package groebnerf"))
     (SETQ LAST_FREE_CELLS 1000000)
     (PUT 'LAST_FREE_CELLS 'DESCRIPTION
          (LIST "free cells after last garbage collections"))
     (SETQ CHOOSE_6_20_MAX_FTEM 20)
     (PUT 'CHOOSE_6_20_MAX_FTEM 'DESCRIPTION
          (LIST "parameter in choose_6_20 when to switch between 6 and 20"))
     (SETQ CHOOSE_6_20_MAX_TERMS 4000)
     (PUT 'CHOOSE_6_20_MAX_TERMS 'DESCRIPTION
          (LIST "parameter in choose_6_20 when to switch between 6 and 20"))
     (SETQ CHOOSE_27_8_16_MAX 15)
     (PUT 'CHOOSE_27_8_16_MAX 'DESCRIPTION
          (LIST
           "parameter in choose_27_8_16 when to switch between 27 and 8 and 16"))
     (SETQ CHOOSE_30_47_21_MAX 10)
     (PUT 'CHOOSE_30_47_21_MAX 'DESCRIPTION
          (LIST
           "parameter in choose_30_47_21 when to switch between 30 and 47 and 21"))
     (SETQ CHOOSE_70_65_8_47_ORIGTERMS 1000000000)
     (PUT 'CHOOSE_70_65_8_47_ORIGTERMS 'DESCRIPTION
          (LIST
           "parameter in choose_70_65_8_47_origterms: the original number of terms"))
     (SETQ CHOOSE_70_65_8_47_ORIGMEM LAST_FREE_CELLS)
     (PUT 'CHOOSE_70_65_8_47_ORIGMEM 'DESCRIPTION
          (LIST "parameter in choose_70_65_8_47: the original free cells"))
     (SETQ CHOOSE_70_65_8_47_RATIOTERMS 200)
     (PUT 'CHOOSE_70_65_8_47_RATIOTERMS 'DESCRIPTION
          (LIST
           "parameter in choose_70_65_8_47: percentage of increase of terms when to split into cases"))
     (SETQ CHOOSE_70_65_8_47_RATIOMEM 50)
     (PUT 'CHOOSE_70_65_8_47_RATIOMEM 'DESCRIPTION
          (LIST
           "parameter in choose_70_65_8_47: percentage of left free mem when to split"))
     (SETQ CHOOSE_30_47_72_EQN 50)
     (PUT 'CHOOSE_30_47_72_EQN 'DESCRIPTION
          (LIST
           "parameter in choose_30_47_72: no of eqn when decoupling and factorizing become more important than reading more equations"))
     (SETQ CHOOSE_11_30_MAX_11 10)
     (PUT 'CHOOSE_11_30_MAX_11 'DESCRIPTION
          (LIST
           "parameter in choose_11_30: max no of steps 11 before 72 (read in equation)"))
     (SETQ CHOOSE_11_30_MAX_30 3)
     (PUT 'CHOOSE_11_30_MAX_30 'DESCRIPTION
          (LIST
           "parameter in choose_11_30: max no of steps 30 before 72 (read in equation)"))
     (PUT 'CHOOSE_70_65_8_47_RATIOMEM 'DESCRIPTION
          (LIST "parameter in choose_70_65_8_47"))
     (SETQ GROEB_DIFF_MAX 15)
     (PUT 'GROEB_DIFF_MAX 'DESCRIPTION
          (LIST
           "a measure of the difficulty of a system such that it is worth to try a groebner package"))
     (SETQ ALG_POLY NIL)
     (PUT 'ALG_POLY 'DESCRIPTION
          (LIST "=t if the system is algebraic and polynomial"))
     (SETQ CASE_LIST (LIST 'LIST))
     (PUT 'CASE_LIST 'DESCRIPTION
          (LIST
           "an algebraic list of expressions which should be considered =0/<>0 in this order"))
     (SETQ EQUATIONS_FILE "")
     (PUT 'EQUATIONS_FILE 'DESCRIPTION
          (LIST "name of the file from which equations are read and added"))
     (SETQ EQN_INPUT NIL)
     (PUT 'EQN_INPUT 'DESCRIPTION
          (LIST
           "global pointer remembering whether the file is still closed (=nil), open (<>nil and <>'done) or already fully read (='done)"))
     (SETQ EQN_TO_BE_GEN NIL)
     (PUT 'EQN_TO_BE_GEN 'DESCRIPTION
          (LIST
           "=t initially if equations are generated and later set =nil when no more generations can be generated"))
     (SETQ EQN_NO 0)
     (PUT 'EQN_NO 'DESCRIPTION
          (LIST "the number of equations already read in from file"))
     (SETQ CRACK_INI_FILE NIL)
     (PUT 'CRACK_INI_FILE 'DESCRIPTION
          (LIST
           "the name of the compiled initialization file without the suffix .b;  it is a string, like \" /scratch/..\" and is loaded into remote processes in crpvm.red"))
     (SETQ UD_1 NIL)
     (PUT 'UD_1 'DESCRIPTION
          (LIST
           "automatically backed up parameters to be used in user_defined()"))
     (SETQ UD_2 NIL)
     (PUT 'UD_2 'DESCRIPTION
          (LIST
           "automatically backed up parameters to be used in user_defined()"))
     (SETQ UD_3 NIL)
     (PUT 'UD_3 'DESCRIPTION
          (LIST
           "automatically backed up parameters to be used in user_defined()"))
     (SETQ UD_4 NIL)
     (PUT 'UD_4 'DESCRIPTION
          (LIST
           "automatically backed up parameters to be used in user_defined()"))
     (SETQ CRACK_LOAD_COMMAND NIL)
     (PUT 'CRACK_LOAD_COMMAND 'DESCRIPTION
          (LIST
           "determined through a call to crack_load_cmd(), needed for calling external procedures DiffElim, Form and in parallel computations to load automatically exactly the same CRACK as in the initial load. This can not be computed at the time of loading CRACK because the information is taken from the global variable options!* which is updated only at the end of each loading.
"))
     (SETQ CRACK_DOWNLOAD_ADD "http://lie.math.brocku.ca/twolf/crack/")
     (PUT 'CRACK_DOWNLOAD_ADD 'DESCRIPTION
          (LIST "The web address for downloading the latest version of CRACK"))
     (SETQ AUTO_PARA_MODE NIL)
     (PUT 'AUTO_PARA_MODE 'DESCRIPTION
          (LIST
           "determines whether and how to add automatically extra interactive parallel processes:  
=nil: automatic parallel case solution either not specified yet, or not possible
=1: as xterm, =2: as screen, =3: as sqsub job, =4: PVM "))
     (SETQ PROCESS_COUNTER "no_of_processes")
     (PUT 'PROCESS_COUNTER 'DESCRIPTION
          (LIST
           "name of file that holds the number of processes in a parallel computation, its content is initialized together with auto_para_mode in crmain.red and updated in proczaehler() in crpvm.red"))
     (SETQ MAX_PROC_NO 5)
     (PUT 'MAX_PROC_NO 'DESCRIPTION
          (LIST "maximum number of parallel processes"))
     (SETQ BATCH_MODE_SUB T)
     (PUT 'BATCH_MODE_SUB 'DESCRIPTION
          (LIST
           "whether sub-calls of crack() should have batch_mode=t or nil. Such calls occur in find_trafo()/crtrafo.red, in dropredund(0)/crsimpso.red and solution_check1"))
     (SETQ MAX_TERM_TO_FAC_COMPLEX 8)
     (PUT 'MAX_TERM_TO_FAC_COMPLEX 'DESCRIPTION
          (LIST
           "maximal number of terms to be factorized properly and automatically in updatesq() and simplifySQ()"))
     (SETQ MAX_TERM_TO_FAC_REAL 100)
     (PUT 'MAX_TERM_TO_FAC_REAL 'DESCRIPTION
          (LIST
           "maximal number of terms to be factorized properly and automatically in updatesq() and simplifySQ()"))
     (SETQ MAX_TERM_TO_PRED 10)
     (PUT 'MAX_TERM_TO_PRED 'DESCRIPTION
          (LIST "maximal number of terms to be a parameter to preduce()"))
     (SETQ FORM_COMP NIL)
     (PUT 'FORM_COMP 'DESCRIPTION
          (LIST
           "whether the S-polynomial shall be computed by FORM in crdec.red and substitutions done by FORM could be initialized automatically if system \"which form\"; finds the file, e.g. under: /usr/local/bin/form"))
     (SETQ FORM_PIPE NIL)
     (PUT 'FORM_PIPE 'DESCRIPTION
          (LIST
           "whether the communication REDUCE --> FORM should go through a pipe. If nil then a file interface is used. If it goes through a pipe then FORM is started through 
./form_start < formin > formout &
at the start of the sq!*crack() procedure in crmain.red"))
     (SETQ FORM_EQN_IDX 0)
     (PUT 'FORM_EQN_IDX 'DESCRIPTION
          (LIST "the index of the last equation generated by form"))
     (SETQ FORM_EQN_ON_DISK NIL)
     (PUT 'FORM_EQN_ON_DISK 'DESCRIPTION
          (LIST
           "a list {{# of terms, file name},...} of equations that have been computed by form"))
     (SETQ FORM_MAX_READ 10000)
     (PUT 'FORM_MAX_READ 'DESCRIPTION
          (LIST
           "the maximal number of terms of an expression computed by FORM that shall be read into REDUCE"))
     (SETQ FORM_TMP_DIR "/tmp/")
     (PUT 'FORM_TMP_DIR 'DESCRIPTION
          (LIST
           "the directory in which FORM shall open temporary files = \"/tmp/\" on sharcnet"))
     (SETQ LARGEST_FULLY_SHORTENED NIL)
     (PUT 'LARGEST_FULLY_SHORTENED 'DESCRIPTION
          (LIST
           "The latest equation in the list pdes of equations (when starting to count from the first) that is fully shortened wrt to all earlier equations on this list"))
     (SETQ MAX_COEFF_LEN 100000000000000000)
     (PUT 'MAX_COEFF_LEN 'DESCRIPTION
          (LIST
           "the max length of coefficients for shortening to be considered in crshort.red"))
     (SETQ CALL_CRACK_OUT T)
     (PUT 'CALL_CRACK_OUT 'DESCRIPTION
          (LIST
           "If in a user defined procedure crack_out a call of crack is made then set call_crack_out:=nil to avoid a loop"))
     (SETQ SWITCH_LIST NIL)
     (PUT 'SWITCH_LIST 'DESCRIPTION
          (LIST
           "a list of switches changed, i.e. a list of lists {level_ when change was made, name of switch, value before change}"))
     (SETQ PARA_CASE_DIR "")
     (PUT 'PARA_CASE_DIR 'DESCRIPTION
          (LIST
           "the directory in which the initialization files for all cases of a parallel computation shall be stored, this should be long enough until all cases are solved, if necessary in later runs, \"\" = current directory, =\"/scratch/twolf/\"$  on SharcNet"))
     (SETQ *NOTSEPARATE T)
     (PUT '*NOTSEPARATE 'DESCRIPTION
          (LIST
           "This is a REDUCE flag which when =t prevents the simplification of x**(5/4) to x**(1/4)*x. The default value of !*notseparate in REDUCE is nil. !*notseparate=t is necessary for simplifications of, for example, (x**(5/4))**(4/5) to abs(x) {or x if let rule abs_ is active}. The disadvantage of !*notseparate:=t is that factorize (a*x**(5/4)+b*x); would not get x as a factor, but on combineexpt; does get the factorization right. !*notseparate=t has the disadvantage that together with OFF COMBINEEXPT it will not recognize u**(3/2)-u*sqrt(u) as zero which can have catastrophic consequences."))
     (SETQ VERSION_DATE "2016-04-07")
     (PUT '*NOTSEPARATE 'DESCRIPTION (LIST "Date of last change."))
     (AEVAL (OFF (LIST 'COMBINEEXPT)))
     (SETQ CURRENTLY_TO_BE_SUBSTITUTED_IN NIL)
     (PUT 'CURRENTLY_TO_BE_SUBSTITUTED_IN 'DESCRIPTION
          (LIST
           "The latest/largest equation in the list pdes of equations (when starting to count from the shortest) in which all possible substitutions of shorter equations have been done that do not lead to case distinctions (this variable was previously not in the list not_passed_back.)"))
     (PUT 'SOL_LIST 'DESCRIPTION
          (LIST "list of file names each containing a solution"))
     (PUT 'SOL_LIST_FILE_CREATED 'DESCRIPTION
          (LIST "Whether a file listing all solution names has been created"))
     (SETQ REDUCE_CALL NIL)
     (PUT 'REDUCE_CALL 'DESCRIPTION
          (LIST
           "The call of the current REDUCE session. In PSL-REDUCE this is stored in unixargs!* but not in CSL-REDUCE. It is needed to start parallel sessions."))
     (SETQ SINGULAR_CALL NIL)
     (PUT 'SINGULAR_CALL 'DESCRIPTION
          (LIST
           "The call of Singular including the pathname, determined in crgb.red, possible default value: \"Singular\"$ "))
     (SETQ SINGULAR_LIB NIL)
     (PUT 'SINGULAR_LIB 'DESCRIPTION
          (LIST
           "the directory name of the Singular library, determined in crgb.red, possible default value: \"\"$ "))
     (SETQ SINGULAR_TIME 20)
     (PUT 'SINGULAR_TIME 'DESCRIPTION
          (LIST "CPU time limit for a Singular call"))
     (SETQ DIFFELIM_STEPS 10000)
     (PUT 'DIFFELIM_STEPS 'DESCRIPTION
          (LIST "number of steps to be done in DiffElim"))
     (SETQ LAZY_EVAL NIL)
     (PUT 'LAZY_EVAL 'DESCRIPTION
          (LIST
           "In large non-linear calculations with many variables and many sub-cases which mostly have no solution a lot of time is wasted by substitutions being performed in forg when the case afterall has no solution. If lazy_eval=t then these substitutions are collected in fsub_ until a solution has been found and then they are performed. fsub_ has the form {(f . {'!*sq,SQ,t})[,..]} Before a call of dropredundant() substitutions should have been made for dropredundant() to be efficient."))
     (SETQ KEEP_CASE_TREE T)
     (PUT 'KEEP_CASE_TREE 'DESCRIPTION
          (LIST
           "whether the procedure CaseTree in crutil.red shall maintain a file ct??????- which stores and updates the tree of cases for re-starts."))
     (SETQ DEC_DEPTH_FIRST_SEARCH T)
     (PUT 'DEC_DEPTH_FIRST_SEARCH 'DESCRIPTION
          (LIST
           "in decoupling: if t then the strategy is to make progress in lowering derivative no matter what the cost which is good if there are non-trivial solutions, i.e. there is a non-trivial differential Groebner Basis. Then adding more and more equations would not help. dec_depth_first_search=t alsso implies that any case distinctions are made to pair the 2 equations with the lowest degree of the least priority leading derivatives of the highest ranked functions."))
     (SETQ REAL_VALUED NIL)
     (PUT 'REAL_VALUED 'DESCRIPTION
          (LIST
           "=t iff all unknowns and paprmeters and variables are real valued. Then all terms of a polynomial are set to zero if all exponents are even and coefficients are positive."))
     (PUT 'TO_DO 'DESCRIPTION (LIST "Hot list of urgent steps"))
     (PUT 'SUBST_LEVEL_0 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_0 SUBST_0) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     only fcts. of less vars., no cases"))
     (PUT 'SUBST_LEVEL_03 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_0 SUBST_0) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     alg. expressions, no cases"))
     (PUT 'SUBST_LEVEL_04 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_1 SUBST_1) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_1 TARGET_LIMIT_1) (T "any number of"))
                " terms," "     alg. expressions, no cases"))
     (PUT 'SUBST_LEVEL_05 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     alg. expressions, no cases"))
     (PUT 'SUBST_LEVEL_1 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_1 SUBST_1) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_2 TARGET_LIMIT_2) (T "any number of"))
                " terms," "     fcts. of less vars."))
     (PUT 'SUBST_LEVEL_2 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     fcts. of less vars., no cases"))
     (PUT 'SUBST_LEVEL_3 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_2 SUBST_2) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_1 TARGET_LIMIT_1) (T "any number of"))
                " terms"))
     (PUT 'SUBST_LEVEL_33 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     only linear expressions, f-indep. coeff."))
     (PUT 'SUBST_LEVEL_35 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     no cases"))
     (PUT 'SUBST_LEVEL_4 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms"))
     (PUT 'SUBST_LEVEL_45 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "     no cases, minimal growth"
                (COND (COST_LIMIT5 ", with max "))
                (COND (COST_LIMIT5 COST_LIMIT5))
                (COND (COST_LIMIT5 " add. terms"))))
     (PUT 'SUBST_LEVEL_5 'DESCRIPTION
          (LIST "Substitution of max "
                (COND (SUBST_3 SUBST_3) (T "any number of")) " terms in "
                (COND (TARGET_LIMIT_3 TARGET_LIMIT_3) (T "any number of"))
                " terms," "    minimal growth"))
     (PUT 'SUBST_DERIVATIVE 'DESCRIPTION
          (LIST "Substitution of derivatives by new functions"))
     (PUT 'UNDO_SUBST_DERIVATIVE 'DESCRIPTION
          (LIST "Undo Substitutions of derivatives by new functions"))
     (PUT 'FACTORIZE_TO_SUBSTITUTE 'DESCRIPTION
          (LIST "Factorization to subcases leading to substitutions"))
     (PUT 'FACTORIZE_ANY 'DESCRIPTION (LIST "Any factorization"))
     (PUT 'SEPARATION 'DESCRIPTION (LIST "Direct separation"))
     (PUT 'QUICK_INTEGRATION 'DESCRIPTION
          (LIST "Integration of a first order de with at" " most one term."))
     (PUT 'FULL_INTEGRATION 'DESCRIPTION
          (LIST "Integration of a pde such that" " a function can be subst."))
     (PUT 'INTEGRATION 'DESCRIPTION (LIST "Any integration"))
     (PUT 'MULTINTFAC 'DESCRIPTION
          (LIST "Find an integrating factor for a set of pde's"))
     (PUT 'DIFF_LENGTH_REDUCTION 'DESCRIPTION
          (LIST "Length reducing decoupling steps"))
     (PUT 'DECOUPLING 'DESCRIPTION (LIST "Do one decoupling step"))
     (PUT 'QUICK_GEN_SEPARATION 'DESCRIPTION
          (LIST "Indirect separation of <" LOW_GENSEP " or >" HIGH_GENSEP
                " terms"))
     (PUT 'GEN_SEPARATION 'DESCRIPTION
          (LIST "Indirect separation of equations of any size"))
     (PUT 'GEN_SEPARATION2 'DESCRIPTION
          (LIST "Alternative indirect separation of non-lin equations"))
     (PUT 'ADD_DIFFERENTIATED_PDES 'DESCRIPTION
          (LIST "Differentiate pdes with nonlinear leading derivs"))
     (PUT 'ALG_LENGTH_REDUCTION 'DESCRIPTION
          (LIST "Algebraic length reduction of equations"))
     (PUT 'ALG_SOLVE_SINGLE 'DESCRIPTION
          (LIST "Solving an algebraic equation"))
     (PUT 'ALG_SOLVE_SYSTEM 'DESCRIPTION
          (LIST "Solving equations for fnct.s or deriv.s algebraically"))
     (PUT 'STOP_BATCH 'DESCRIPTION (LIST "Stop batch mode"))
     (PUT 'UNDETLINODE 'DESCRIPTION
          (LIST "The parametric solution of underdetermined ODE"))
     (PUT 'UNDETLINPDE 'DESCRIPTION
          (LIST "The parametric solution of underdetermined PDE"))
     (PUT 'CHANGE_PROC_LIST 'DESCRIPTION
          (LIST "Changing the list of priorities"))
     (PUT 'DROP_LIN_DEP 'DESCRIPTION
          (LIST "Find and drop linear dependent general equations"))
     (PUT 'DROP_DEP_BI_LIN 'DESCRIPTION
          (LIST "Find and drop linear dependent bi-linear equations"))
     (PUT 'FIND_FACTOR_BI_LIN 'DESCRIPTION
          (LIST "Find factorizable bi-linear equations"))
     (PUT 'FIND_1_TERM_EQN 'DESCRIPTION
          (LIST "Find a linear dependent equation with only 1 term"))
     (PUT 'TRIAN_LIN_ALG 'DESCRIPTION
          (LIST "Triangularize a linear algebraic system"))
     (PUT 'DEL_REDUNDANT_FC 'DESCRIPTION
          (LIST "Drop redundant functions and constants"))
     (PUT 'SUB_PROBLEM 'DESCRIPTION (LIST "Solve a subset of equations first"))
     (PUT 'DEL_REDUNDANT_DE 'DESCRIPTION (LIST "Delete redundant equations"))
     (PUT 'IDTY_INTEGRATION 'DESCRIPTION (LIST "Integrate an identity"))
     (PUT 'ADD_DIFF_ISE 'DESCRIPTION
          (LIST "Differentiate indirectly separable equations"))
     (PUT 'SPLIT_INTO_CASES 'DESCRIPTION
          (LIST "Consider a given expression to be zero and non-zero"))
     (PUT 'FIRST_INT_FOR_ODE 'DESCRIPTION
          (LIST "Find symmetries and then first integrals for an ODE"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS12 'DESCRIPTION
          (LIST "Find sub-systems with 2 non-flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS13 'DESCRIPTION
          (LIST "Find sub-systems with 3 non-flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS14 'DESCRIPTION
          (LIST "Find sub-systems with 4 non-flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS15 'DESCRIPTION
          (LIST "Find sub-systems with 5 non-flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS22 'DESCRIPTION
          (LIST "Find sub-systems with 2 flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS23 'DESCRIPTION
          (LIST "Find sub-systems with 3 flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS24 'DESCRIPTION
          (LIST "Find sub-systems with 4 flin_ functions"))
     (PUT 'FIND_AND_USE_SUB_SYSTEMS25 'DESCRIPTION
          (LIST "Find sub-systems with 5 flin_ functions"))
     (PUT 'HIGH_PRIO_DECOUPLING 'DESCRIPTION
          (LIST "Do one high priority decoupling step"))
     (PUT 'USER_DEFINE 'DESCRIPTION (LIST "Perform a user defined operation"))
     (PUT 'EXTERNAL_GROEBNER 'DESCRIPTION
          (LIST "Computation of the algebraic Groebner basis"))
     (PUT 'SOLUTION_CHECK1 'DESCRIPTION
          (LIST "Check whether a given solution is contained"))
     (PUT 'FIND_TRAFO 'DESCRIPTION
          (LIST "Find a transformation to integrate a 1st order PDE"))
     (PUT 'CHOOSE_6_20 'DESCRIPTION (LIST "Reorder modules 6 and 20"))
     (PUT 'CHOOSE_27_8_16 'DESCRIPTION (LIST "Reorder modules 27, 8 and 16"))
     (PUT 'CHOOSE_30_47_21 'DESCRIPTION (LIST "Reorder modules 30, 47 and 21"))
     (PUT 'CASE_ON_MOST_FREQU_FNC 'DESCRIPTION
          (LIST "Split into cases whether most frequ. function vanishes"))
     (PUT 'BACK_UP 'DESCRIPTION (LIST "Save backup"))
     (PUT 'ALG_LENGTH_REDUCE_1 'DESCRIPTION
          (LIST "Algebraic length reduction of a single equation"))
     (PUT 'CHECK_INEQ 'DESCRIPTION
          (LIST "Checking whether inequalities are contradicted"))
     (PUT 'CASE_SEPARATION 'DESCRIPTION
          (LIST "Separation that may lead to case distinctions"))
     (PUT 'PRE_DETERMINED_CASE_SPLITS 'DESCRIPTION
          (LIST "Doing a pre-determined case splitting"))
     (PUT 'CHOOSE_70_65_8_47 'DESCRIPTION
          (LIST "Inserting a case-splitting if system too big"))
     (PUT 'SOLUTION_CHECK2 'DESCRIPTION
          (LIST "Check whether a given solution contradicts inequalities"))
     (PUT 'READ_EQUATION 'DESCRIPTION
          (LIST "Read an additional equation from a file"))
     (PUT 'CHOOSE_30_47_72 'DESCRIPTION
          (LIST "Reading new equation if not too many"))
     (PUT 'CASE_ON_MOST_FREQU_FACTORS 'DESCRIPTION
          (LIST "Split into cases whether most frequ. factor vanishes"))
     (PUT 'CHOOSE_11_30 'DESCRIPTION (LIST "Limited shortenings"))
     (PUT 'TRY_OTHER_ORDERING 'DESCRIPTION (LIST "Try other ordering"))
     (PUT 'FIND_FACTORIZATION 'DESCRIPTION
          (LIST "Perform a thorough factorization"))
     (PUT 'PARA_SOLVE_HOM_EQU 'DESCRIPTION
          (LIST "Solve a homogeneous alg. equation in two variables"))
     (PUT 'UNDETALG 'DESCRIPTION (LIST "Solve nested polynomial"))
     (PUT 'SUBST_POWER 'DESCRIPTION (LIST "Replace powers by new unknowns"))
     (PUT 'BOTTOM_UP_SUBST 'DESCRIPTION (LIST "Do a single substitution"))
     (PUT 'SOLUTION_CHECK3 'DESCRIPTION
          (LIST "Fast check of a special solution based on substitutions"))
     (PUT 'SUBLINFCT 'DESCRIPTION
          (LIST "Absorb non-linear factors in linear unknowns"))
     (PUT 'CASE_GEN_SEPARATION 'DESCRIPTION
          (LIST
           "Normal generalized separation that may lead to case distinctions"))
     (PUT 'REPLACE_EQUATION 'DESCRIPTION
          (LIST "Module induced replacement or adding of an equation"))
     (PUT 'DO_ONE_RESULTANT 'DESCRIPTION (LIST "Compute a resultant"))
     (PUT 'INHOM_DECOUPLING 'DESCRIPTION
          (LIST "Decoupling of inhom. equations in otherwise homog. system"))
     (PUT 'LINEARIZE_BI_LIN 'DESCRIPTION
          (LIST
           "Converting a homog. quadr. system into a lin. one of all products"))
     (PUT 'QUIT_IF_NO_ALG_SOL 'DESCRIPTION
          (LIST
           "Stop case if there is an alg. poly. eqn of degree>1 in only 1 fct."))
     (PUT 'GET_SPECIAL_ALG_SOL1 'DESCRIPTION
          (LIST
           "Set coeff. of higher powers to zero to linearize and get spec. sol."))
     (PUT 'GET_SPECIAL_ALG_SOL2 'DESCRIPTION
          (LIST "Set linear terms to zero and get spec. sol."))
     (PUT 'I_HD 'DESCRIPTION (LIST "Help to inspect data"))
     (PUT 'I_HP 'DESCRIPTION (LIST "Help to proceed"))
     (PUT 'I_HF 'DESCRIPTION (LIST "Help to change flags & parameters"))
     (PUT 'I_HC 'DESCRIPTION (LIST "Help to change data of equations"))
     (PUT 'I_HI 'DESCRIPTION (LIST "Help to work with identities"))
     (PUT 'I_HB 'DESCRIPTION (LIST "Help to trace, debug and configure"))
     (PUT 'I_HL 'DESCRIPTION (LIST "Help to duplicate and parallelize runs"))
     (PUT 'I_HE 'DESCRIPTION (LIST "Help to use external systems"))
     (PUT 'I_E 'DESCRIPTION (LIST "Print equations"))
     (PUT 'I_EO 'DESCRIPTION (LIST "Print overview of functions in equations"))
     (PUT 'I_PI 'DESCRIPTION (LIST "Print inequalities"))
     (PUT 'I_F 'DESCRIPTION (LIST "Print functions and variables"))
     (PUT 'I_V 'DESCRIPTION (LIST "Print all derivatives of all functions"))
     (PUT 'I_S 'DESCRIPTION (LIST "Print statistics"))
     (PUT 'I_FC 'DESCRIPTION (LIST "Print no of free cells"))
     (PUT 'I_PE 'DESCRIPTION (LIST "Print an algebraic expression"))
     (PUT 'I_PH 'DESCRIPTION (LIST "Print history of interactive input"))
     (PUT 'I_PV 'DESCRIPTION (LIST "Print value of any lisp variable"))
     (PUT 'I_PF 'DESCRIPTION (LIST "Print no of occurences of each function"))
     (PUT 'I_PO 'DESCRIPTION (LIST "Print no of occurences of each factor"))
     (PUT 'I_PU 'DESCRIPTION
          (LIST "Print no of occurences of each coefficient"))
     (PUT 'I_PR 'DESCRIPTION (LIST "Print active substitution rules"))
     (PUT 'I_PD 'DESCRIPTION
          (LIST "Plot the occurence of functions in equations"))
     (PUT 'I_PS 'DESCRIPTION (LIST "Plot a statistical history"))
     (PUT 'I_LC 'DESCRIPTION (LIST "List all case distinctions"))
     (PUT 'I_CA 'DESCRIPTION (LIST "List all current case assumptions"))
     (PUT 'I_WS 'DESCRIPTION (LIST "Write statistical history in file"))
     (PUT 'I_SN 'DESCRIPTION (LIST "Show name of session"))
     (PUT 'I_SS 'DESCRIPTION (LIST "Find and print sub-systems"))
     (PUT 'I_W 'DESCRIPTION (LIST "Write equations into a file"))
     (PUT 'I_A 'DESCRIPTION (LIST "Do one step automatically"))
     (PUT 'I_G 'DESCRIPTION (LIST "Go on for a number of steps automatically"))
     (PUT 'I_T 'DESCRIPTION
          (LIST "Toggle equation selection between AUTOMATIC and USER"))
     (PUT 'I_P1 'DESCRIPTION
          (LIST "Print a list of all modules in batch mode"))
     (PUT 'I_P2 'DESCRIPTION (LIST "Print a complete list of all modules"))
     (PUT '|I_#| 'DESCRIPTION
          (LIST "Execute the module with the number # once"))
     (PUT 'I_L 'DESCRIPTION (LIST "Execute a specific module repeatedly"))
     (PUT 'I_SB 'DESCRIPTION (LIST "Save complete backup to file"))
     (PUT 'I_RB 'DESCRIPTION (LIST "Read backup from file"))
     (PUT 'I_BM 'DESCRIPTION (LIST "Remove file stop_now to allow batch mode"))
     (PUT 'I_AN 'DESCRIPTION
          (LIST "Add non-triviality conditions for param. lin. sys."))
     (PUT 'I_RS 'DESCRIPTION (LIST "Compute resultant of two polynomials"))
     (PUT 'I_X 'DESCRIPTION (LIST "Exit interactive mode for good"))
     (PUT 'I_Q 'DESCRIPTION (LIST "Quit level or quit crack if in level 0"))
     (PUT 'I_QH 'DESCRIPTION (LIST "Quit level hard and drop solutions"))
     (PUT 'I_QQ 'DESCRIPTION (LIST "Quit the whole CRACK run with all levels"))
     (PUT 'I_PL 'DESCRIPTION
          (LIST "Set maximal length of an expression to be printed"))
     (PUT 'I_PM 'DESCRIPTION
          (LIST
           "Toggle extended printing about the success of methods ON/OFF"))
     (PUT 'I_PA 'DESCRIPTION
          (LIST
           "Toggle full printing of properties of equations with e-command ON/OFF"))
     (PUT 'I_CP 'DESCRIPTION (LIST "Change the priorities of procedures"))
     (PUT 'I_OG 'DESCRIPTION
          (LIST "Toggle decoupling priority between functions and derivativs"))
     (PUT 'I_OD 'DESCRIPTION
          (LIST
           "Toggle decoupling priority between total and lex. ordering of derivatives"))
     (PUT 'I_OI 'DESCRIPTION
          (LIST "Interactive change of lex. ordering on variables"))
     (PUT 'I_OR 'DESCRIPTION
          (LIST "Reverse the lex. ordering of independent variables"))
     (PUT 'I_OM 'DESCRIPTION
          (LIST "Mix randomly lex. ordering of independent variables"))
     (PUT 'I_OF 'DESCRIPTION
          (LIST "Interactive change of ordering on functions"))
     (PUT 'I_OP 'DESCRIPTION (LIST "Print current ordering"))
     (PUT 'I_NE 'DESCRIPTION
          (LIST "Set root of the name of newly generated equations"))
     (PUT 'I_NF 'DESCRIPTION
          (LIST "Set root of the name of newly generated functions/constants"))
     (PUT 'I_NI 'DESCRIPTION
          (LIST "Set root of the name of newly generated identities"))
     (PUT 'I_NA 'DESCRIPTION
          (LIST "Toggle output between natural and machine readable"))
     (PUT 'I_AS 'DESCRIPTION (LIST "Making an assignment"))
     (PUT 'I_KE 'DESCRIPTION
          (LIST
           "Toggle between keeping and not keeping a partitioned copy of each equation"))
     (PUT 'I_FI 'DESCRIPTION
          (LIST "Toggle between allowing and forbidding unresolved integrals"))
     (PUT 'I_FA 'DESCRIPTION
          (LIST
           "Toggle between allowing and forbidding solutions of ODEs with ABS()"))
     (PUT 'I_CS 'DESCRIPTION
          (LIST
           "Toggle between interactive and automatic selection of substitutions"))
     (PUT 'I_FS 'DESCRIPTION
          (LIST
           "Toggle between enforcing and not enforcing direct separation"))
     (PUT 'I_LL 'DESCRIPTION (LIST "Changing the line length"))
     (PUT 'I_RE 'DESCRIPTION
          (LIST "Toggle between re-cycling and not re-cycling equation names"))
     (PUT 'I_RF 'DESCRIPTION
          (LIST "Toggle between re-cycling and not re-cycling function names"))
     (PUT 'I_ST 'DESCRIPTION
          (LIST "Setting a CPU time limit for un-interrupted run"))
     (PUT 'I_CM 'DESCRIPTION (LIST "Adding a comment to the history_ list"))
     (PUT 'I_LR 'DESCRIPTION (LIST "Adding a LET-rule"))
     (PUT 'I_CR 'DESCRIPTION (LIST "Clearing a LET-rule"))
     (PUT 'I_MO 'DESCRIPTION (LIST "Starting/stopping modulo computations"))
     (PUT 'I_AP 'DESCRIPTION
          (LIST
           "Adapting the priority of modules to the type of system to be solved"))
     (PUT 'I_HO 'DESCRIPTION (LIST "Find all homogeneities of the system"))
     (PUT 'I_R 'DESCRIPTION (LIST "Replace or add one equation"))
     (PUT 'I_RD 'DESCRIPTION (LIST "Reduce an equation modulo LET rules"))
     (PUT 'I_N 'DESCRIPTION (LIST "Add one inequality"))
     (PUT 'I_DE 'DESCRIPTION (LIST "Delete equations"))
     (PUT 'I_DI 'DESCRIPTION (LIST "Delete one inequality"))
     (PUT 'I_C 'DESCRIPTION (LIST "Change a flag or property of one pde"))
     (PUT 'I_PT 'DESCRIPTION
          (LIST "Perform a transformation of functions and variables"))
     (PUT 'I_I 'DESCRIPTION (LIST "Print identities between equations"))
     (PUT 'I_ID 'DESCRIPTION (LIST "Delete redundand equations"))
     (PUT 'I_IW 'DESCRIPTION (LIST "Write identities to a file"))
     (PUT 'I_IR 'DESCRIPTION (LIST "Remove list of identities"))
     (PUT 'I_IA 'DESCRIPTION (LIST "Add or replace an identity"))
     (PUT 'I_IH 'DESCRIPTION (LIST "Start recording histories and identities"))
     (PUT 'I_IS 'DESCRIPTION (LIST "Stop recording histories and identities"))
     (PUT 'I_II 'DESCRIPTION (LIST "Integrate an identity"))
     (PUT 'I_IC 'DESCRIPTION (LIST "Check the consistency of identity data"))
     (PUT 'I_IY 'DESCRIPTION (LIST "Print the history of equations"))
     (PUT 'I_TM 'DESCRIPTION (LIST "Toggle tracing of main procedure"))
     (PUT 'I_TG 'DESCRIPTION (LIST "Toggle tracing of generalized separation"))
     (PUT 'I_TI 'DESCRIPTION
          (LIST "Toggle tracing of generalized integration"))
     (PUT 'I_TD 'DESCRIPTION (LIST "Toggle tracing of decoupling process"))
     (PUT 'I_TL 'DESCRIPTION
          (LIST "Toggle tracing of length reducing decoupling"))
     (PUT 'I_TS 'DESCRIPTION
          (LIST "Toggle tracing of algebraic length reduction"))
     (PUT 'I_TO 'DESCRIPTION (LIST "Toggle tracing of ordering process"))
     (PUT 'I_UT 'DESCRIPTION (LIST "Untrace a procedure"))
     (PUT 'I_BR 'DESCRIPTION (LIST "Break"))
     (PUT 'I_PC 'DESCRIPTION (LIST "Do a function call"))
     (PUT 'I_IN 'DESCRIPTION (LIST "Reading in a REDUCE file"))
     (PUT 'I_CU 'DESCRIPTION
          (LIST "Check uniqueness of all standard quotients"))
     (PUT 'I_QT 'DESCRIPTION (LIST "Choose functions to profile"))
     (PUT 'I_PQ 'DESCRIPTION (LIST "Show profiling result"))
     (PUT 'I_SO 'DESCRIPTION (LIST "Turn a switch ON"))
     (PUT 'I_SF 'DESCRIPTION (LIST "Turn a switch OFF"))
     (PUT 'I_LS 'DESCRIPTION (LIST "List REDUCE switch settings"))
     (PUT 'I_LG 'DESCRIPTION (LIST "List global CRACK variables"))
     (PUT 'I_DC 'DESCRIPTION (LIST "Describe command|module|global variable"))
     (PUT 'I_XP 'DESCRIPTION (LIST "Duplicate process under new xterm"))
     (PUT 'I_SP 'DESCRIPTION (LIST "Duplicate process under new screen"))
     (PUT 'I_JP 'DESCRIPTION
          (LIST "Duplicate process submitted as offline job"))
     (PUT 'I_PP 'DESCRIPTION (LIST "Duplicate process under PVM"))
     (PUT 'I_WP 'DESCRIPTION
          (LIST "Perform parallel case solving with xterm's"))
     (PUT 'I_YP 'DESCRIPTION
          (LIST "Perform parallel case solving with screen's"))
     (PUT 'I_ZP 'DESCRIPTION (LIST "Perform parallel case solving with jobs"))
     (PUT 'I_VP 'DESCRIPTION (LIST "Perform parallel case solving under PVM"))
     (PUT 'I_NP 'DESCRIPTION
          (LIST "Set counter of parallel processes to zero"))
     (PUT 'I_MP 'DESCRIPTION
          (LIST "Specify max. number of parallel processes"))
     (PUT 'I_TP 'DESCRIPTION (LIST "Specify directory for storing cases"))
     (PUT 'I_DP 'DESCRIPTION (LIST "Disable parallel case solving"))
     (PUT 'I_FO 'DESCRIPTION (LIST "Select FORM for computing S-polynomials"))
     (PUT 'I_FF 'DESCRIPTION (LIST "Do not use FORM"))
     (PUT 'I_GS 'DESCRIPTION (LIST "Select Singular for Groebner bases"))
     (PUT 'I_GG 'DESCRIPTION
          (LIST "Select GB package of J.C.Faugere for Groebner b."))
     (PUT 'I_GR 'DESCRIPTION (LIST "Select REDUCE package for Groebner bases"))
     (PUT 'I_DF 'DESCRIPTION (LIST "Select DiffElim for diff. Groebner bases"))
     (INI_LET_RULES))) 
(PUT 'INI_LET_RULES 'NUMBER-OF-ARGS 0) 
(FLAG '(INI_LET_RULES) 'OPFN) 
(PUT 'INI_LET_RULES 'DEFINED-ON-LINE '1188) 
(PUT 'INI_LET_RULES 'DEFINED-IN-FILE 'CRACK/CRINIT.RED) 
(PUT 'INI_LET_RULES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INI_LET_RULES NIL
    (PROG ()
      (SETQ USERRULES_ (LIST 'LIST))
      (SETK 'EXPLOG_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'COT (LIST '~ 'X))
                         (LIST 'QUOTIENT 1 (LIST 'TAN 'X)))
                   (LIST 'REPLACEBY
                         (LIST 'TIMES (LIST 'EXPT 'E (LIST '~ 'X))
                               (LIST 'EXPT 'E (LIST '~ 'Y)))
                         (LIST 'EXPT 'E (LIST 'PLUS 'X 'Y)))
                   (LIST 'REPLACEBY
                         (LIST 'TIMES (LIST 'EXPT (LIST 'SQRT 'E) (LIST '~ 'X))
                               (LIST 'EXPT (LIST 'SQRT 'E) (LIST '~ 'Y)))
                         (LIST 'EXPT (LIST 'SQRT 'E) (LIST 'PLUS 'X 'Y)))
                   (LIST 'REPLACEBY
                         (LIST 'TIMES
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'Z)))
                               (LIST 'EXPT 'E
                                     (LIST 'QUOTIENT (LIST '~ 'Y)
                                           (LIST '~ 'Z))))
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT (LIST 'PLUS 'X 'Y) 'Z)))
                   (LIST 'REPLACEBY
                         (LIST 'TIMES
                               (LIST 'EXPT (LIST 'SQRT 'E)
                                     (LIST 'QUOTIENT (LIST '~ 'X)
                                           (LIST '~ 'Z)))
                               (LIST 'EXPT (LIST 'SQRT 'E)
                                     (LIST 'QUOTIENT (LIST '~ 'Y)
                                           (LIST '~ 'Z))))
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'QUOTIENT (LIST 'PLUS 'X 'Y) 'Z)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'Y))
                                     (LIST '~ 'X)))
                         (LIST 'EXPT 'Y
                               (LIST 'QUOTIENT (LIST 'QUOTIENT 1 'X) 2)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'Y))
                                           (LIST '~ 'X))))
                         (LIST 'EXPT 'Y
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (LIST 'QUOTIENT 1 'X)
                                           2))))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'TIMES (LIST '~ 'X)
                                     (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'Y))
                                           (LIST '~ 'Z))))
                         (LIST 'EXPT 'Y
                               (LIST 'QUOTIENT (LIST 'QUOTIENT 'X 'Z) 2)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST '~ 'X)
                                           (LIST 'QUOTIENT
                                                 (LIST 'LOG (LIST '~ 'Y))
                                                 (LIST '~ 'Z)))))
                         (LIST 'EXPT 'Y
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT (LIST 'QUOTIENT 'X 'Z)
                                           2))))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT (LIST 'SQRT 'E)
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES (LIST '~ 'X)
                                           (LIST 'LOG (LIST '~ 'Y)))
                                     (LIST '~ 'Z)))
                         (LIST 'EXPT 'Y
                               (LIST 'QUOTIENT (LIST 'QUOTIENT 'X 'Z) 2)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'Y))
                                     (LIST '~ 'X)))
                         (LIST 'EXPT 'Y (LIST 'QUOTIENT 1 'X)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT 'E
                               (LIST 'TIMES (LIST '~ 'X)
                                     (LIST 'QUOTIENT (LIST 'LOG (LIST '~ 'Y))
                                           (LIST '~ 'Z))))
                         (LIST 'EXPT 'Y (LIST 'QUOTIENT 'X 'Z)))
                   (LIST 'REPLACEBY
                         (LIST 'EXPT 'E
                               (LIST 'QUOTIENT
                                     (LIST 'TIMES (LIST '~ 'X)
                                           (LIST 'LOG (LIST '~ 'Y)))
                                     (LIST '~ 'Z)))
                         (LIST 'EXPT 'Y (LIST 'QUOTIENT 'X 'Z)))
                   (LIST 'REPLACEBY
                         (LIST 'INT
                               (LIST 'QUOTIENT
                                     (LIST 'DF (LIST '~ 'Y) (LIST '~ 'X))
                                     (LIST '~ 'Y))
                               (LIST '~ 'X))
                         (LIST 'LOG 'Y)))))
      (SETK 'ABS_
            (AEVAL (LIST 'LIST (LIST 'REPLACEBY (LIST 'ABS (LIST '~ 'X)) 'X))))
      (SETK 'TRIG1_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SIN (LIST '~ 'X)) 2)
                         (LIST 'DIFFERENCE 1 (LIST 'EXPT (LIST 'COS 'X) 2))))))
      (SETK 'TRIG2_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'EXPT (LIST 'SINH (LIST '~ 'X)) 2)
                         (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'COSH 'X) 2)
                               1)))))
      (SETK 'TRIG3_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'TAN (LIST 'QUOTIENT (LIST '~ 'X) 2))
                         (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 (LIST 'COS 'X))
                               (LIST 'SIN 'X))))))
      (SETK 'TRIG4_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'COT (LIST 'QUOTIENT (LIST '~ 'X) 2))
                         (LIST 'QUOTIENT (LIST 'PLUS 1 (LIST 'COS 'X))
                               (LIST 'SIN 'X))))))
      (SETK 'TRIG5_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'COS (LIST 'TIMES 2 (LIST '~ 'X)))
                         (LIST 'DIFFERENCE 1
                               (LIST 'TIMES 2
                                     (LIST 'EXPT (LIST 'SIN 'X) 2)))))))
      (SETK 'TRIG6_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'SIN (LIST 'TIMES 2 (LIST '~ 'X)))
                         (LIST 'TIMES 2 (LIST 'COS 'X) (LIST 'SIN 'X))))))
      (SETK 'TRIG7_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'SINH (LIST 'TIMES 2 (LIST '~ 'X)))
                         (LIST 'TIMES 2 (LIST 'SINH 'X) (LIST 'COSH 'X))))))
      (SETK 'TRIG8_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY (LIST 'COSH (LIST 'TIMES 2 (LIST '~ 'X)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2 (LIST 'EXPT (LIST 'COSH 'X) 2))
                               1)))))
      (SETK 'SQRT1_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY
                         (LIST 'SQRT (LIST 'TIMES (LIST '~ 'X) (LIST '~ 'Y)))
                         (LIST 'TIMES (LIST 'SQRT 'X) (LIST 'SQRT 'Y))))))
      (SETK 'SQRT2_
            (AEVAL
             (LIST 'LIST
                   (LIST 'REPLACEBY
                         (LIST 'SQRT
                               (LIST 'QUOTIENT (LIST '~ 'X) (LIST '~ 'Y)))
                         (LIST 'QUOTIENT (LIST 'SQRT 'X) (LIST 'SQRT 'Y)))))))) 
(ENDMODULE) 