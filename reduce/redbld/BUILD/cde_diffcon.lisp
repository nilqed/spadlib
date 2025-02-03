(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_DIFFCON)) 
(PUT 'ORDER_DIFFCON 'NUMBER-OF-ARGS 2) 
(PUT 'ORDER_DIFFCON 'DEFINED-ON-LINE '45) 
(PUT 'ORDER_DIFFCON 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'ORDER_DIFFCON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDER_DIFFCON (PAR DCON_TEMP)
    (PROG (LOCLESS LOCEQUAL LOCGREATER LOCPIVOT)
      (COND ((NOT DCON_TEMP) (RETURN NIL))
            (T
             (RETURN
              (PROGN
               (SETQ LOCPIVOT (CAR DCON_TEMP))
               (PROG (EL)
                 (SETQ EL DCON_TEMP)
                LAB
                 (COND ((NULL EL) (RETURN NIL)))
                 ((LAMBDA (EL)
                    (COND
                     ((LESSP (ORDER_OF_DER PAR (CAR EL))
                             (ORDER_OF_DER PAR (CAR LOCPIVOT)))
                      (SETQ LOCLESS (CONS EL LOCLESS)))
                     ((EQN (ORDER_OF_DER PAR (CAR EL))
                           (ORDER_OF_DER PAR (CAR LOCPIVOT)))
                      (SETQ LOCEQUAL (CONS EL LOCEQUAL)))
                     (T (SETQ LOCGREATER (CONS EL LOCGREATER)))))
                  (CAR EL))
                 (SETQ EL (CDR EL))
                 (GO LAB))
               (APPEND (APPEND LOCEQUAL (ORDER_DIFFCON PAR LOCGREATER))
                       (ORDER_DIFFCON PAR LOCLESS)))))))) 
(PUT 'DIFF_VARS 'NUMBER-OF-ARGS 2) 
(PUT 'DIFF_VARS 'DEFINED-ON-LINE '69) 
(PUT 'DIFF_VARS 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'DIFF_VARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFF_VARS (VAR1 VAR2)
    (PROG (M_N MMN MMNI)
      (SETQ MMNI 0)
      (COND ((NEQ (CAR VAR1) (CAR VAR2)) (RETURN (MINUS 1))))
      (SETQ M_N (PAIR (CADR VAR1) (CADR VAR2)))
      (PROG (EL)
        (SETQ EL M_N)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ MMNI (DIFFERENCE (CAR EL) (CDR EL)))
            (COND ((GEQ MMNI 0) (SETQ MMN (CONS MMNI MMN))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (COND ((LESSP (LENGTH MMN) N_INDEP_VAR) (RETURN (MINUS 1)))
            (T (RETURN (REVERSE MMN)))))) 
(PUT 'ADD_DIFFCON 'NUMBER-OF-ARGS 2) 
(PUT 'ADD_DIFFCON 'DEFINED-ON-LINE '86) 
(PUT 'ADD_DIFFCON 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'ADD_DIFFCON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD_DIFFCON (PAR DCON)
    (PROG (DIFFCONTEMP VARTEMP PDER EXPRTEMP ORDTEMP DMIND CNT)
      (SETQ ORDTEMP 0)
      (SETQ DMIND 0)
      (SETQ CNT 0)
      (COND ((EQUAL PAR 0) (SETQ DIFFCONTEMP DIFFCON_DER*))
            (T (SETQ DIFFCONTEMP DIFFCON_ODD*)))
      (PROG (EL)
        (SETQ EL DCON)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ VARTEMP (IDTOMIND PAR EL))
            (SETQ ORDTEMP (ORDER_OF_DER PAR EL))
            (SETQ DMIND (MINUS 1))
            (SETQ CNT 1)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (GEQ (ORDER_OF_DER PAR (CAR (NTH DIFFCONTEMP CNT))) ORDTEMP))
                (RETURN NIL)))
              (SETQ CNT (PLUS CNT 1))
              (GO WHILELABEL))
            (PROG ()
             WHILELABEL
              (COND ((NOT (EQUAL DMIND (MINUS 1))) (RETURN NIL)))
              (PROGN
               (SETQ PDER (CAR (NTH DIFFCONTEMP CNT)))
               (SETQ DMIND (DIFF_VARS VARTEMP (IDTOMIND PAR PDER)))
               (SETQ CNT (PLUS CNT 1)))
              (GO WHILELABEL))
            (SETQ EXPRTEMP (CADR (NTH DIFFCONTEMP (DIFFERENCE CNT 1))))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
              (PROG (J)
                (SETQ J 1)
               LAB
                (COND ((MINUSP (DIFFERENCE (NTH DMIND I) J)) (RETURN NIL)))
                (SETQ EXPRTEMP (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP) NIL))
                (SETQ J (PLUS2 J 1))
                (GO LAB))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND
             ((NOT (FREEOF EXPRTEMP 'LETOP))
              (SETQ EXPRTEMP (REVAL1 'LETOP NIL))))
            (SETQ CNT 1)
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (LESSP (ORDER_OF_DER PAR EL)
                        (ORDER_OF_DER PAR (CAR (NTH DIFFCONTEMP CNT)))))
                (RETURN NIL)))
              (SETQ CNT (PLUS CNT 1))
              (GO WHILELABEL))
            (SETQ DIFFCONTEMP (CDE_INSERT (LIST EL EXPRTEMP) DIFFCONTEMP CNT))
            NIL))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (COND ((EQUAL PAR 0) (SETQ DIFFCON_DER* DIFFCONTEMP))
            (T (SETQ DIFFCON_ODD* DIFFCONTEMP)))
      (COND
       ((EQUAL PAR 0)
        (SETQ DIFFCON_COMP_DER*
                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EL DIFFCON_DER*)
                  (COND ((NULL EL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL))
                                        NIL)))
                 LOOPLABEL
                  (SETQ EL (CDR EL))
                  (COND ((NULL EL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       (T
        (SETQ DIFFCON_COMP_ODD*
                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EL DIFFCON_ODD*)
                  (COND ((NULL EL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL))
                                        NIL)))
                 LOOPLABEL
                  (SETQ EL (CDR EL))
                  (COND ((NULL EL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))))) 
(PUT 'CHECK_DIFFCON 'NUMBER-OF-ARGS 2) 
(PUT 'CHECK_DIFFCON 'DEFINED-ON-LINE '132) 
(PUT 'CHECK_DIFFCON 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'CHECK_DIFFCON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECK_DIFFCON (DC_DER DC_ODD)
    (PROG (DC_EXTRA_DER DC_EXTRA_ODD DC_NEW_DER DC_NEW_ODD ELL_EXT EXPRTEMP)
      (SETQ DC_EXTRA_DER (CDE_DIFFSET ALL_PRINCIPAL_DER* DIFFCON_COMP_DER*))
      (SETQ DC_EXTRA_ODD (CDE_DIFFSET ALL_PRINCIPAL_ODD* DIFFCON_COMP_ODD*))
      (PROG (EL)
        (SETQ EL DC_DER)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ EXPRTEMP (CADR (ASSOC EL DIFFCON_DER*)))
            (PROG (ELL)
              (SETQ ELL DC_EXTRA_DER)
             LAB
              (COND ((NULL ELL) (RETURN NIL)))
              ((LAMBDA (ELL)
                 (COND
                  ((NOT (FREEOF EXPRTEMP ELL))
                   (SETQ DC_NEW_DER (CONS ELL DC_NEW_DER)))))
               (CAR ELL))
              (SETQ ELL (CDR ELL))
              (GO LAB))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (PROG (EL)
        (SETQ EL DC_ODD)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ EXPRTEMP (CADR (ASSOC EL DIFFCON_ODD*)))
            (PROG (ELL)
              (SETQ ELL DC_EXTRA_DER)
             LAB
              (COND ((NULL ELL) (RETURN NIL)))
              ((LAMBDA (ELL)
                 (COND
                  ((NOT (FREEOF EXPRTEMP ELL))
                   (SETQ DC_NEW_DER (CONS ELL DC_NEW_DER)))))
               (CAR ELL))
              (SETQ ELL (CDR ELL))
              (GO LAB))
            (PROG (ELL)
              (SETQ ELL DC_EXTRA_ODD)
             LAB
              (COND ((NULL ELL) (RETURN NIL)))
              ((LAMBDA (ELL)
                 (PROGN
                  (SETQ ELL_EXT (ODDEXT ELL))
                  (COND
                   ((NOT (FREEOF EXPRTEMP ELL_EXT))
                    (SETQ DC_NEW_ODD (CONS ELL DC_NEW_ODD))))))
               (CAR ELL))
              (SETQ ELL (CDR ELL))
              (GO LAB))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN (LIST (CDE_MKSET DC_NEW_DER) (CDE_MKSET DC_NEW_ODD))))) 
(PUT 'GENERATE_DIFFCON 'NUMBER-OF-ARGS 0) 
(PUT 'GENERATE_DIFFCON 'DEFINED-ON-LINE '161) 
(PUT 'GENERATE_DIFFCON 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'GENERATE_DIFFCON 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENERATE_DIFFCON NIL
    (PROG (DCON_DER DCON_ODD TEMPDER DEPENDENCY N_DCON)
      (SETQ DEPENDENCY 0)
      (SETQ N_DCON 0)
      (SETQ DIFFCON_DER*
              (ORDER_DIFFCON 0
               (PAIR PRINCIPAL_DER*
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL DE*)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (EL) (LIST EL)) (CAR EL))
                                             NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (EL) (LIST EL)) (CAR EL)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ DIFFCON_ODD*
              (ORDER_DIFFCON 1
               (PAIR PRINCIPAL_ODD*
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL DE_ODD*)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (LIST (REPLACE_ODDEXT EL)))
                                         (CAR EL))
                                        NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (LIST (REPLACE_ODDEXT EL)))
                                 (CAR EL))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ DIFFCON_COMP_DER*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL DIFFCON_DER*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIFFCON_COMP_ODD*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL DIFFCON_ODD*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (CAR EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DEPENDENCY 1)
      (SETQ DCON_DER PRIMARY_DIFFCON_DER*)
      (SETQ DCON_ODD PRIMARY_DIFFCON_ODD*)
      (SETQ N_DCON 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL DEPENDENCY 1)) (RETURN NIL)))
        (PROGN
         (COND (DCON_DER (ADD_DIFFCON 0 DCON_DER)))
         (COND (DCON_ODD (ADD_DIFFCON 1 DCON_ODD)))
         (SETQ TEMPDER (CHECK_DIFFCON DCON_DER DCON_ODD))
         (SETQ DCON_DER (CAR TEMPDER))
         (SETQ DCON_ODD (CADR TEMPDER))
         (COND ((AND (NOT DCON_DER) (NOT DCON_ODD)) (SETQ DEPENDENCY 0))
               (T
                (PROGN
                 (SETQ N_DCON (PLUS N_DCON 1))
                 (PRIN2 "Presence of ")
                 (PRIN1 N_DCON)
                 (PRIN2T "-ary differential consequences")
                 NIL))))
        (GO WHILELABEL)))) 
(PUT 'REPLACE_DIFFCON_TOTDER 'NUMBER-OF-ARGS 4) 
(PUT 'REPLACE_DIFFCON_TOTDER 'DEFINED-ON-LINE '204) 
(PUT 'REPLACE_DIFFCON_TOTDER 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'REPLACE_DIFFCON_TOTDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPLACE_DIFFCON_TOTDER
    (EVEN_DIFFCON_TOT ODD_DIFFCON_TOT PRIN_PARAM_DER PRIN_PARAM_EXT)
    (PROG ()
      (PROG (EL)
        (SETQ EL EVEN_DIFFCON_TOT)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (SET_SVF (CAR EL) 0 (CADDR EL)
            (CDR (ASSOC (CAR (REVERSE EL)) PRIN_PARAM_DER))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (PROG (EL)
        (SETQ EL ODD_DIFFCON_TOT)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (SET_SVF (CAR EL) 1 (CADDR EL)
            (CDR (ASSOC (ODDEXT (CAR (REVERSE EL))) PRIN_PARAM_EXT))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB)))) 
(PUT 'CDE_DIFFERENTIAL_CONSEQUENCES 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_DIFFERENTIAL_CONSEQUENCES 'DEFINED-ON-LINE '217) 
(PUT 'CDE_DIFFERENTIAL_CONSEQUENCES 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'CDE_DIFFERENTIAL_CONSEQUENCES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_DIFFERENTIAL_CONSEQUENCES NIL
    (PROG (TEMPLDCON DIFFCON_COMP_DER_PARAMEXPR DIFFCON_COMP_EXT_EXPR
           DIFFCON_COMP_EXT_EVENPARAMEXPR DIFFCON_COMP_EXT_PARAMEXPR)
      (PRIN2T "   a - Calculating differential consequences ...")
      (GENERATE_DIFFCON)
      (PRIN2T "   b - Solving the system of differential consequences ...")
      (SETQ DIFFCON_COMP_EXT*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL DIFFCON_COMP_ODD*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (ODDEXT EL)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (ODDEXT EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIFFCON_COMP_EXT_EXPR
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL DIFFCON_ODD*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (CADR EL)) (CAR EL)) NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (CADR EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ REPPRINCPARAM_DER
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL DIFFCON_DER*)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL)
                                     (LIST 'REPLACEBY (CAR EL) (CADR EL)))
                                   (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (EL) (LIST 'REPLACEBY (CAR EL) (CADR EL)))
                           (CAR EL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ DIFFCON_COMP_DER_PARAMEXPR
              (CDR
               (EVALWHEREEXP
                (LIST (CONS 'LIST REPPRINCPARAM_DER)
                      (CONS 'LIST DIFFCON_COMP_DER*)))))
      (SETQ DIFFCON_PARAM_DER*
              (PAIR DIFFCON_COMP_DER* DIFFCON_COMP_DER_PARAMEXPR))
      (SETQ REPPRINCPARAM_DER
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL DIFFCON_PARAM_DER*)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL)
                                     (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                                   (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (EL) (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                           (CAR EL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ DIFFCON_COMP_EXT_EVENPARAMEXPR
              (CDR
               (EVALWHEREEXP
                (LIST (CONS 'LIST REPPRINCPARAM_DER)
                      (CONS 'LIST DIFFCON_COMP_EXT_EXPR)))))
      (SETQ TEMPLDCON (PAIR DIFFCON_COMP_EXT* DIFFCON_COMP_EXT_EVENPARAMEXPR))
      (SETQ REPPRINCPARAM_EXT
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL TEMPLDCON)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL) (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ DIFFCON_COMP_EXT_PARAMEXPR
              (CDR
               (EVALWHEREEXP
                (LIST (CONS 'LIST REPPRINCPARAM_EXT)
                      (CONS 'LIST DIFFCON_COMP_EXT*)))))
      (SETQ DIFFCON_PARAM_EXT*
              (PAIR DIFFCON_COMP_EXT* DIFFCON_COMP_EXT_PARAMEXPR))
      (SETQ REPPRINCPARAM_EXT
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL DIFFCON_PARAM_EXT*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL) (LIST 'REPLACEBY (CAR EL) (CDR EL)))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ REPPRINCPARAM_ODD
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL DIFFCON_PARAM_EXT*)
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (EL)
                                     (LIST 'REPLACEBY (EXTODD (CAR EL))
                                           (REPLACE_EXTODD (CDR EL))))
                                   (CAR EL))
                                  NIL)))
                LOOPLABEL
                 (SETQ EL (CDR EL))
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (EL)
                             (LIST 'REPLACEBY (EXTODD (CAR EL))
                                   (REPLACE_EXTODD (CDR EL))))
                           (CAR EL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ REPPRINCPARAM_DER
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST REPPRINCPARAM_DER)))
      (SETQ REPPRINCPARAM_ODD
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST REPPRINCPARAM_ODD)))
      (PRIN2T "   c - Replacing differential cons. in total derivatives ...")
      (REPLACE_DIFFCON_TOTDER PRIMARY_DIFFCON_DER_TOT* PRIMARY_DIFFCON_ODD_TOT*
       DIFFCON_PARAM_DER* DIFFCON_PARAM_EXT*))) 
(PUT 'RESTRICT_TO_EQUATION 'NUMBER-OF-ARGS 1) 
(FLAG '(RESTRICT_TO_EQUATION) 'OPFN) 
(PUT 'RESTRICT_TO_EQUATION 'DEFINED-ON-LINE '286) 
(PUT 'RESTRICT_TO_EQUATION 'DEFINED-IN-FILE 'CDE/CDE_DIFFCON.RED) 
(PUT 'RESTRICT_TO_EQUATION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RESTRICT_TO_EQUATION (FCT)
    (PROG ()
      (RETURN (AEVAL (LIST 'WHEREEXP (LIST 'LIST REPPRINCPARAM_DER) FCT))))) 
(ENDMODULE) 