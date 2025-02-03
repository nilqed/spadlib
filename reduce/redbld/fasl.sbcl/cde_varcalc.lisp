(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_VARCALC)) 
(PUT 'PVAR_DF 'NUMBER-OF-ARGS 3) 
(PUT 'PVAR_DF 'DEFINED-ON-LINE '41) 
(PUT 'PVAR_DF 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'PVAR_DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PVAR_DF (PAR DENSITY_ODD COMPONENT)
    (PROG (RESULT)
      (COND ((EQUAL PAR 0) (SETQ RESULT (PVAR_DF0 DENSITY_ODD COMPONENT)))
            (T (SETQ RESULT (PVAR_DF1 DENSITY_ODD COMPONENT))))
      (RETURN (REPLACE_EXTODD (REVAL1 (CONS 'PLUS RESULT) NIL))))) 
(PUT 'PVAR_DF0 'NUMBER-OF-ARGS 2) 
(PUT 'PVAR_DF0 'DEFINED-ON-LINE '58) 
(PUT 'PVAR_DF0 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'PVAR_DF0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PVAR_DF0 (DENSITY_ODD COMPONENT)
    (PROG (DENSITY_EXT TEMPVAR TEMPMIND EXPRTEMP RESULT)
      (SETQ DENSITY_EXT (REPLACE_ODDEXT DENSITY_ODD))
      (PROG (EL)
        (SETQ EL ALL_PARAMETRIC_DER*)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ TEMPVAR (IDTOMIND 0 EL))
            (COND
             ((EQUAL (CAR TEMPVAR) COMPONENT)
              (PROGN
               (SETQ TEMPMIND (CADR TEMPVAR))
               (SETQ EXPRTEMP
                       (REVAL1
                        (LIST 'TIMES
                              (LIST 'EXPT (MINUS 1)
                                    (LENGTH_MULTIINDEX TEMPMIND))
                              (LIST 'DF DENSITY_EXT EL))
                        NIL))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                 (PROG (J)
                   (SETQ J 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
                   (SETQ EXPRTEMP
                           (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP) NIL))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (COND (*CHECKORD (CHECK_LETOP EXPRTEMP)))
               (SETQ RESULT (CONS EXPRTEMP RESULT)))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN RESULT))) 
(PUT 'PVAR_DF1 'NUMBER-OF-ARGS 2) 
(PUT 'PVAR_DF1 'DEFINED-ON-LINE '82) 
(PUT 'PVAR_DF1 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'PVAR_DF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PVAR_DF1 (DENSITY_ODD COMPONENT)
    (PROG (TEMPVAR TEMPMIND EXPRTEMP DENSITY_EXT RESULT)
      (SETQ DENSITY_EXT (REPLACE_ODDEXT DENSITY_ODD))
      (PROG (EL)
        (SETQ EL ALL_PARAMETRIC_ODD*)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ TEMPVAR (IDTOMIND 1 EL))
            (COND
             ((EQUAL (CAR TEMPVAR) COMPONENT)
              (PROGN
               (SETQ TEMPMIND (CADR TEMPVAR))
               (SETQ EXPRTEMP
                       (REVAL1
                        (LIST 'TIMES
                              (LIST 'EXPT (MINUS 1)
                                    (LENGTH_MULTIINDEX TEMPMIND))
                              (LIST 'DF_EXT DENSITY_EXT (ODDEXT EL)))
                        NIL))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                 (PROG (J)
                   (SETQ J 1)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
                   (SETQ EXPRTEMP
                           (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP) NIL))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (COND (*CHECKORD (CHECK_LETOP EXPRTEMP)))
               (SETQ RESULT (CONS EXPRTEMP RESULT))
               NIL)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN RESULT))) 
(FLAG '(PVAR_DF) 'OPFN) 
(PUT 'EULER_DF 'NUMBER-OF-ARGS 1) 
(PUT 'EULER_DF 'DEFINED-ON-LINE '108) 
(PUT 'EULER_DF 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'EULER_DF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EULER_DF (DENSITY)
    (PROG (TEMPLIST1 TEMPLIST2)
      (SETQ TEMPLIST1
              (CONS 'LIST
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL DEP_VAR*)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (PVAR_DF 0 DENSITY EL))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EL) (PVAR_DF 0 DENSITY EL)) (CAR EL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ TEMPLIST2
              (CONS 'LIST
                    (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELL ODD_VAR*)
                      (COND ((NULL ELL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELL) (PVAR_DF 1 DENSITY ELL))
                                        (CAR ELL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELL (CDR ELL))
                      (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELL) (PVAR_DF 1 DENSITY ELL))
                                (CAR ELL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN (CONS 'LIST (LIST TEMPLIST1 TEMPLIST2))))) 
(FLAG '(EULER_DF) 'OPFN) 
(PUT 'SCHOUTEN_BRACKET_EXPR 'NUMBER-OF-ARGS 4) 
(PUT 'SCHOUTEN_BRACKET_EXPR 'DEFINED-ON-LINE '121) 
(PUT 'SCHOUTEN_BRACKET_EXPR 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'SCHOUTEN_BRACKET_EXPR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SCHOUTEN_BRACKET_EXPR (MVE1 PARITY1 MVE2 PARITY2)
    (PROG (TEMPTERM1 TEMPTERM2 TEMPSIGN N_DEP_VAR)
      (SETQ N_DEP_VAR (LENGTH DEP_VAR*))
      (COND
       ((NEQ N_DEP_VAR (LENGTH ODD_VAR*))
        (REDERR "Error: number of even and odd variables mismatch!")))
      (SETQ TEMPTERM1
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N_DEP_VAR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (SUPER_PRODUCT
                                  (REVAL1
                                   (CONS 'PLUS
                                         (PVAR_DF0 MVE2 (NTH DEP_VAR* I)))
                                   NIL)
                                  (REVAL1
                                   (CONS 'PLUS
                                         (PVAR_DF1 MVE1 (NTH ODD_VAR* I)))
                                   NIL))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE N_DEP_VAR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (SUPER_PRODUCT
                          (REVAL1 (CONS 'PLUS (PVAR_DF0 MVE2 (NTH DEP_VAR* I)))
                                  NIL)
                          (REVAL1 (CONS 'PLUS (PVAR_DF1 MVE1 (NTH ODD_VAR* I)))
                                  NIL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TEMPSIGN
              (REVAL1
               (LIST 'TIMES (MINUS 1)
                     (LIST 'EXPT (MINUS 1)
                           (LIST 'TIMES (PLUS PARITY1 1) (PLUS PARITY2 1))))
               NIL))
      (SETQ TEMPTERM2
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N_DEP_VAR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (REVAL1
                                  (LIST 'TIMES TEMPSIGN
                                        (SUPER_PRODUCT
                                         (REVAL1
                                          (CONS 'PLUS
                                                (PVAR_DF0 MVE1
                                                 (NTH DEP_VAR* I)))
                                          NIL)
                                         (REVAL1
                                          (CONS 'PLUS
                                                (PVAR_DF1 MVE2
                                                 (NTH ODD_VAR* I)))
                                          NIL)))
                                  NIL)
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE N_DEP_VAR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (REVAL1
                          (LIST 'TIMES TEMPSIGN
                                (SUPER_PRODUCT
                                 (REVAL1
                                  (CONS 'PLUS (PVAR_DF0 MVE1 (NTH DEP_VAR* I)))
                                  NIL)
                                 (REVAL1
                                  (CONS 'PLUS (PVAR_DF1 MVE2 (NTH ODD_VAR* I)))
                                  NIL)))
                          NIL)
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*CHECKORD (PROGN (CHECK_LETOP TEMPTERM1) (CHECK_LETOP TEMPTERM2))))
      (RETURN
       (REPLACE_EXTODD
        (REVAL1 (CONS 'PLUS (APPEND TEMPTERM1 TEMPTERM2)) NIL))))) 
(PUT 'SCHOUTEN_BRACKET 'NUMBER-OF-ARGS 3) 
(PUT 'SCHOUTEN_BRACKET 'DEFINED-ON-LINE '151) 
(PUT 'SCHOUTEN_BRACKET 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'SCHOUTEN_BRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SCHOUTEN_BRACKET (MV1 MV2 MV3)
    (PROG (PARITY1 PARITY2 N_ODD_VAR)
      (SETQ N_ODD_VAR (LENGTH ODD_VAR*))
      (SETQ PARITY1 (GET 'SFNARG MV1))
      (SETQ PARITY2 (GET 'SFNARG MV2))
      (CHECK_SUPERFUN_SCALAR MV1)
      (CHECK_SUPERFUN_SCALAR MV2)
      (MK_SUPERFUN MV3 (PLUS PARITY1 (DIFFERENCE PARITY2 1)) 1)
      (SETK (LIST MV3 1)
            (SCHOUTEN_BRACKET_EXPR (REVAL1 (LIST MV1 1) NIL) PARITY1
             (REVAL1 (LIST MV2 1) NIL) PARITY2)))) 
(FLAG '(SCHOUTEN_BRACKET) 'OPFN) 
(PUT 'ISZERO_SCHOUTEN_BRACKET 'NUMBER-OF-ARGS 3) 
(PUT 'ISZERO_SCHOUTEN_BRACKET 'DEFINED-ON-LINE '175) 
(PUT 'ISZERO_SCHOUTEN_BRACKET 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'ISZERO_SCHOUTEN_BRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ISZERO_SCHOUTEN_BRACKET (MV1 MV2 MV3)
    (PROG ()
      (SCHOUTEN_BRACKET MV1 MV2 MV3)
      (RETURN (EULER_DF (REVAL1 (LIST MV3 1) NIL))))) 
(FLAG '(ISZERO_SCHOUTEN_BRACKET) 'OPFN) 
(PUT 'CARTAN_DF_EXPR 'NUMBER-OF-ARGS 1) 
(PUT 'CARTAN_DF_EXPR 'DEFINED-ON-LINE '211) 
(PUT 'CARTAN_DF_EXPR 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'CARTAN_DF_EXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CARTAN_DF_EXPR (SFUN_EXPR)
    (PROG (SFUN_OUT_LIST TEMPVAR_MIND TEMPEVAR TEMPMIND TEMPOVAR)
      (SETQ SFUN_OUT_LIST
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_PARAMETRIC_DER*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (PROGN
                                     (SETQ TEMPVAR_MIND (IDTOMIND 0 EL))
                                     (SETQ TEMPEVAR (CAR TEMPVAR_MIND))
                                     (SETQ TEMPMIND (CADR TEMPVAR_MIND))
                                     (SETQ TEMPOVAR
                                             (NTH ODD_VAR*
                                                  (CDE_POSITION TEMPEVAR
                                                   DEP_VAR*)))
                                     (ODD_PRODUCT
                                      (MIND_TO_EID (LIST TEMPOVAR TEMPMIND))
                                      (REVAL1 (LIST 'DF SFUN_EXPR EL) NIL))))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (PROGN
                             (SETQ TEMPVAR_MIND (IDTOMIND 0 EL))
                             (SETQ TEMPEVAR (CAR TEMPVAR_MIND))
                             (SETQ TEMPMIND (CADR TEMPVAR_MIND))
                             (SETQ TEMPOVAR
                                     (NTH ODD_VAR*
                                          (CDE_POSITION TEMPEVAR DEP_VAR*)))
                             (ODD_PRODUCT
                              (MIND_TO_EID (LIST TEMPOVAR TEMPMIND))
                              (REVAL1 (LIST 'DF SFUN_EXPR EL) NIL))))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (REVAL1 (CONS 'PLUS SFUN_OUT_LIST) NIL)))) 
(PUT 'CARTAN_DF 'NUMBER-OF-ARGS 2) 
(PUT 'CARTAN_DF 'DEFINED-ON-LINE '229) 
(PUT 'CARTAN_DF 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'CARTAN_DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CARTAN_DF (SFUN SFUN_OUT)
    (PROG ()
      (CHECK_SUPERFUN_SCALAR SFUN)
      (MK_SUPERFUN SFUN_OUT (PLUS 1 (GET 'SFNARG SFUN)) 1)
      (SETK (LIST SFUN_OUT 1) (CARTAN_DF_EXPR (REVAL1 (LIST SFUN 1) NIL))))) 
(FLAG '(CARTAN_DF) 'OPFN) 
(PUT 'VARIATIONAL_DF_EXPR 'NUMBER-OF-ARGS 1) 
(PUT 'VARIATIONAL_DF_EXPR 'DEFINED-ON-LINE '247) 
(PUT 'VARIATIONAL_DF_EXPR 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'VARIATIONAL_DF_EXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VARIATIONAL_DF_EXPR (SFUN_EXPR)
    (PROG (SFUN_OUT_LIST TEMPVAR_MIND TEMPVARS TEMPMIND TEMPOVAR EXPRTEMP)
      (SETQ SFUN_OUT_LIST
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ODD_VAR*)
               STARTOVER
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (EL)
                           (PROGN
                            (SETQ TEMPVARS
                                    (SELECT_ALL_DERS 1 EL ALL_PARAMETRIC_ODD*))
                            (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                              (SETQ ELL TEMPVARS)
                              (COND ((NULL ELL) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELL)
                                                  (PROGN
                                                   (SETQ TEMPVAR_MIND
                                                           (IDTOMIND 1 ELL))
                                                   (SETQ TEMPOVAR
                                                           (CAR TEMPVAR_MIND))
                                                   (SETQ TEMPMIND
                                                           (CADR TEMPVAR_MIND))
                                                   (SETQ EXPRTEMP
                                                           (REVAL1
                                                            (LIST 'TIMES
                                                                  (LIST 'EXPT
                                                                        (MINUS
                                                                         1)
                                                                        (LENGTH_MULTIINDEX
                                                                         TEMPMIND))
                                                                  (DF_ODD
                                                                   SFUN_EXPR
                                                                   ELL))
                                                            NIL))
                                                   (PROG (I)
                                                     (SETQ I 1)
                                                    LAB
                                                     (COND
                                                      ((MINUSP
                                                        (DIFFERENCE N_INDEP_VAR
                                                                    I))
                                                       (RETURN NIL)))
                                                     (PROG (J)
                                                       (SETQ J 1)
                                                      LAB
                                                       (COND
                                                        ((MINUSP
                                                          (DIFFERENCE
                                                           (NTH TEMPMIND I) J))
                                                         (RETURN NIL)))
                                                       (SETQ EXPRTEMP
                                                               (REVAL1
                                                                (LIST
                                                                 (NTH TOT_DER*
                                                                      I)
                                                                 EXPRTEMP)
                                                                NIL))
                                                       (SETQ J (PLUS2 J 1))
                                                       (GO LAB))
                                                     (SETQ I (PLUS2 I 1))
                                                     (GO LAB))
                                                   (COND
                                                    (*CHECKORD
                                                     (CHECK_LETOP EXPRTEMP)))
                                                   (ODD_PRODUCT TEMPOVAR
                                                    EXPRTEMP)))
                                                (CAR ELL))
                                               NIL)))
                             LOOPLABEL
                              (SETQ ELL (CDR ELL))
                              (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELL)
                                          (PROGN
                                           (SETQ TEMPVAR_MIND (IDTOMIND 1 ELL))
                                           (SETQ TEMPOVAR (CAR TEMPVAR_MIND))
                                           (SETQ TEMPMIND (CADR TEMPVAR_MIND))
                                           (SETQ EXPRTEMP
                                                   (REVAL1
                                                    (LIST 'TIMES
                                                          (LIST 'EXPT (MINUS 1)
                                                                (LENGTH_MULTIINDEX
                                                                 TEMPMIND))
                                                          (DF_ODD SFUN_EXPR
                                                           ELL))
                                                    NIL))
                                           (PROG (I)
                                             (SETQ I 1)
                                            LAB
                                             (COND
                                              ((MINUSP
                                                (DIFFERENCE N_INDEP_VAR I))
                                               (RETURN NIL)))
                                             (PROG (J)
                                               (SETQ J 1)
                                              LAB
                                               (COND
                                                ((MINUSP
                                                  (DIFFERENCE (NTH TEMPMIND I)
                                                              J))
                                                 (RETURN NIL)))
                                               (SETQ EXPRTEMP
                                                       (REVAL1
                                                        (LIST (NTH TOT_DER* I)
                                                              EXPRTEMP)
                                                        NIL))
                                               (SETQ J (PLUS2 J 1))
                                               (GO LAB))
                                             (SETQ I (PLUS2 I 1))
                                             (GO LAB))
                                           (COND
                                            (*CHECKORD (CHECK_LETOP EXPRTEMP)))
                                           (ODD_PRODUCT TEMPOVAR EXPRTEMP)))
                                        (CAR ELL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EL (CDR EL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (EL)
                           (PROGN
                            (SETQ TEMPVARS
                                    (SELECT_ALL_DERS 1 EL ALL_PARAMETRIC_ODD*))
                            (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                              (SETQ ELL TEMPVARS)
                              (COND ((NULL ELL) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELL)
                                                  (PROGN
                                                   (SETQ TEMPVAR_MIND
                                                           (IDTOMIND 1 ELL))
                                                   (SETQ TEMPOVAR
                                                           (CAR TEMPVAR_MIND))
                                                   (SETQ TEMPMIND
                                                           (CADR TEMPVAR_MIND))
                                                   (SETQ EXPRTEMP
                                                           (REVAL1
                                                            (LIST 'TIMES
                                                                  (LIST 'EXPT
                                                                        (MINUS
                                                                         1)
                                                                        (LENGTH_MULTIINDEX
                                                                         TEMPMIND))
                                                                  (DF_ODD
                                                                   SFUN_EXPR
                                                                   ELL))
                                                            NIL))
                                                   (PROG (I)
                                                     (SETQ I 1)
                                                    LAB
                                                     (COND
                                                      ((MINUSP
                                                        (DIFFERENCE N_INDEP_VAR
                                                                    I))
                                                       (RETURN NIL)))
                                                     (PROG (J)
                                                       (SETQ J 1)
                                                      LAB
                                                       (COND
                                                        ((MINUSP
                                                          (DIFFERENCE
                                                           (NTH TEMPMIND I) J))
                                                         (RETURN NIL)))
                                                       (SETQ EXPRTEMP
                                                               (REVAL1
                                                                (LIST
                                                                 (NTH TOT_DER*
                                                                      I)
                                                                 EXPRTEMP)
                                                                NIL))
                                                       (SETQ J (PLUS2 J 1))
                                                       (GO LAB))
                                                     (SETQ I (PLUS2 I 1))
                                                     (GO LAB))
                                                   (COND
                                                    (*CHECKORD
                                                     (CHECK_LETOP EXPRTEMP)))
                                                   (ODD_PRODUCT TEMPOVAR
                                                    EXPRTEMP)))
                                                (CAR ELL))
                                               NIL)))
                             LOOPLABEL
                              (SETQ ELL (CDR ELL))
                              (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELL)
                                          (PROGN
                                           (SETQ TEMPVAR_MIND (IDTOMIND 1 ELL))
                                           (SETQ TEMPOVAR (CAR TEMPVAR_MIND))
                                           (SETQ TEMPMIND (CADR TEMPVAR_MIND))
                                           (SETQ EXPRTEMP
                                                   (REVAL1
                                                    (LIST 'TIMES
                                                          (LIST 'EXPT (MINUS 1)
                                                                (LENGTH_MULTIINDEX
                                                                 TEMPMIND))
                                                          (DF_ODD SFUN_EXPR
                                                           ELL))
                                                    NIL))
                                           (PROG (I)
                                             (SETQ I 1)
                                            LAB
                                             (COND
                                              ((MINUSP
                                                (DIFFERENCE N_INDEP_VAR I))
                                               (RETURN NIL)))
                                             (PROG (J)
                                               (SETQ J 1)
                                              LAB
                                               (COND
                                                ((MINUSP
                                                  (DIFFERENCE (NTH TEMPMIND I)
                                                              J))
                                                 (RETURN NIL)))
                                               (SETQ EXPRTEMP
                                                       (REVAL1
                                                        (LIST (NTH TOT_DER* I)
                                                              EXPRTEMP)
                                                        NIL))
                                               (SETQ J (PLUS2 J 1))
                                               (GO LAB))
                                             (SETQ I (PLUS2 I 1))
                                             (GO LAB))
                                           (COND
                                            (*CHECKORD (CHECK_LETOP EXPRTEMP)))
                                           (ODD_PRODUCT TEMPOVAR EXPRTEMP)))
                                        (CAR ELL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))))
                         (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EL (CDR EL))
                (GO LOOPLABEL)))
      (RETURN (REVAL1 (CONS 'PLUS SFUN_OUT_LIST) NIL)))) 
(PUT 'VARIATIONAL_DF 'NUMBER-OF-ARGS 2) 
(PUT 'VARIATIONAL_DF 'DEFINED-ON-LINE '273) 
(PUT 'VARIATIONAL_DF 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'VARIATIONAL_DF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VARIATIONAL_DF (SFUN SFUN_OUT)
    (PROG ()
      (CHECK_SUPERFUN_SCALAR SFUN)
      (MK_SUPERFUN SFUN_OUT (PLUS 1 (GET 'SFNARG SFUN)) 1)
      (SETK (LIST SFUN_OUT 1)
            (VARIATIONAL_DF_EXPR (CARTAN_DF_EXPR (REVAL1 (LIST SFUN 1) NIL)))))) 
(FLAG '(VARIATIONAL_DF) 'OPFN) 
(PUT 'EV_SUPERFUN_EVEN 'NUMBER-OF-ARGS 2) 
(PUT 'EV_SUPERFUN_EVEN 'DEFINED-ON-LINE '289) 
(PUT 'EV_SUPERFUN_EVEN 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'EV_SUPERFUN_EVEN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EV_SUPERFUN_EVEN (SUPERF1 SUPERF_SCAL)
    (PROG (TEMPVAR TEMPMIND TEMPDVAR NTEMPDVAR TEMPRES DER_EVEN EXPRTEMP)
      (SETQ TEMPRES
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_PARAMETRIC_DER*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (PROGN
                                     (SETQ DER_EVEN
                                             (REVAL1
                                              (LIST 'DF
                                                    (REPLACE_ODDEXT
                                                     SUPERF_SCAL)
                                                    EL)
                                              NIL))
                                     (COND ((EQUAL DER_EVEN 0) 0)
                                           (T
                                            (PROGN
                                             (SETQ TEMPVAR (IDTOMIND 0 EL))
                                             (SETQ TEMPMIND (CADR TEMPVAR))
                                             (SETQ TEMPDVAR (CAR TEMPVAR))
                                             (SETQ NTEMPDVAR
                                                     (CDE_POSITION TEMPDVAR
                                                      DEP_VAR*))
                                             (SETQ EXPRTEMP
                                                     (REPLACE_ODDEXT
                                                      (REVAL1
                                                       (LIST SUPERF1 NTEMPDVAR)
                                                       NIL)))
                                             (PROG (I)
                                               (SETQ I 1)
                                              LAB
                                               (COND
                                                ((MINUSP
                                                  (DIFFERENCE N_INDEP_VAR I))
                                                 (RETURN NIL)))
                                               (PROG (J)
                                                 (SETQ J 1)
                                                LAB
                                                 (COND
                                                  ((MINUSP
                                                    (DIFFERENCE
                                                     (NTH TEMPMIND I) J))
                                                   (RETURN NIL)))
                                                 (SETQ EXPRTEMP
                                                         (REVAL1
                                                          (LIST
                                                           (NTH TOT_DER* I)
                                                           EXPRTEMP)
                                                          NIL))
                                                 (SETQ J (PLUS2 J 1))
                                                 (GO LAB))
                                               (SETQ I (PLUS2 I 1))
                                               (GO LAB))
                                             (COND
                                              (*CHECKORD
                                               (CHECK_LETOP EXPRTEMP)))
                                             (SUPER_PRODUCT EXPRTEMP
                                                            DER_EVEN))))))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (PROGN
                             (SETQ DER_EVEN
                                     (REVAL1
                                      (LIST 'DF (REPLACE_ODDEXT SUPERF_SCAL)
                                            EL)
                                      NIL))
                             (COND ((EQUAL DER_EVEN 0) 0)
                                   (T
                                    (PROGN
                                     (SETQ TEMPVAR (IDTOMIND 0 EL))
                                     (SETQ TEMPMIND (CADR TEMPVAR))
                                     (SETQ TEMPDVAR (CAR TEMPVAR))
                                     (SETQ NTEMPDVAR
                                             (CDE_POSITION TEMPDVAR DEP_VAR*))
                                     (SETQ EXPRTEMP
                                             (REPLACE_ODDEXT
                                              (REVAL1 (LIST SUPERF1 NTEMPDVAR)
                                                      NIL)))
                                     (PROG (I)
                                       (SETQ I 1)
                                      LAB
                                       (COND
                                        ((MINUSP (DIFFERENCE N_INDEP_VAR I))
                                         (RETURN NIL)))
                                       (PROG (J)
                                         (SETQ J 1)
                                        LAB
                                         (COND
                                          ((MINUSP
                                            (DIFFERENCE (NTH TEMPMIND I) J))
                                           (RETURN NIL)))
                                         (SETQ EXPRTEMP
                                                 (REVAL1
                                                  (LIST (NTH TOT_DER* I)
                                                        EXPRTEMP)
                                                  NIL))
                                         (SETQ J (PLUS2 J 1))
                                         (GO LAB))
                                       (SETQ I (PLUS2 I 1))
                                       (GO LAB))
                                     (COND (*CHECKORD (CHECK_LETOP EXPRTEMP)))
                                     (SUPER_PRODUCT EXPRTEMP DER_EVEN))))))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (REVAL1 (CONS 'PLUS TEMPRES) NIL)))) 
(PUT 'EV_SUPERFUN_ODD 'NUMBER-OF-ARGS 2) 
(PUT 'EV_SUPERFUN_ODD 'DEFINED-ON-LINE '316) 
(PUT 'EV_SUPERFUN_ODD 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'EV_SUPERFUN_ODD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EV_SUPERFUN_ODD (SUPERF1 SUPERF_SCAL)
    (PROG (TEMPOVAR TEMPMIND NTEMPOVAR TEMPRES DER_ODD EXPRTEMP)
      (SETQ TEMPRES
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_PARAMETRIC_ODD*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL)
                                    (PROGN
                                     (SETQ DER_ODD (COEFFN SUPERF_SCAL EL 1))
                                     (COND ((EQUAL DER_ODD 0) 0)
                                           (T
                                            (PROGN
                                             (SETQ TEMPOVAR (IDTOMIND 1 EL))
                                             (SETQ TEMPMIND (CADR TEMPOVAR))
                                             (SETQ TEMPOVAR (CAR TEMPOVAR))
                                             (SETQ NTEMPOVAR
                                                     (CDE_POSITION TEMPOVAR
                                                      ODD_VAR*))
                                             (SETQ EXPRTEMP
                                                     (REPLACE_ODDEXT
                                                      (CARTAN_DF_EXPR
                                                       (REVAL1
                                                        (LIST SUPERF1
                                                              NTEMPOVAR)
                                                        NIL))))
                                             (PROG (I)
                                               (SETQ I 1)
                                              LAB
                                               (COND
                                                ((MINUSP
                                                  (DIFFERENCE N_INDEP_VAR I))
                                                 (RETURN NIL)))
                                               (PROG (J)
                                                 (SETQ J 1)
                                                LAB
                                                 (COND
                                                  ((MINUSP
                                                    (DIFFERENCE
                                                     (NTH TEMPMIND I) J))
                                                   (RETURN NIL)))
                                                 (SETQ EXPRTEMP
                                                         (REVAL1
                                                          (LIST
                                                           (NTH TOT_DER* I)
                                                           EXPRTEMP)
                                                          NIL))
                                                 (SETQ J (PLUS2 J 1))
                                                 (GO LAB))
                                               (SETQ I (PLUS2 I 1))
                                               (GO LAB))
                                             (COND
                                              (*CHECKORD
                                               (CHECK_LETOP EXPRTEMP)))
                                             (REVAL1
                                              (LIST 'TIMES DER_ODD EXPRTEMP)
                                              NIL))))))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL)
                            (PROGN
                             (SETQ DER_ODD (COEFFN SUPERF_SCAL EL 1))
                             (COND ((EQUAL DER_ODD 0) 0)
                                   (T
                                    (PROGN
                                     (SETQ TEMPOVAR (IDTOMIND 1 EL))
                                     (SETQ TEMPMIND (CADR TEMPOVAR))
                                     (SETQ TEMPOVAR (CAR TEMPOVAR))
                                     (SETQ NTEMPOVAR
                                             (CDE_POSITION TEMPOVAR ODD_VAR*))
                                     (SETQ EXPRTEMP
                                             (REPLACE_ODDEXT
                                              (CARTAN_DF_EXPR
                                               (REVAL1 (LIST SUPERF1 NTEMPOVAR)
                                                       NIL))))
                                     (PROG (I)
                                       (SETQ I 1)
                                      LAB
                                       (COND
                                        ((MINUSP (DIFFERENCE N_INDEP_VAR I))
                                         (RETURN NIL)))
                                       (PROG (J)
                                         (SETQ J 1)
                                        LAB
                                         (COND
                                          ((MINUSP
                                            (DIFFERENCE (NTH TEMPMIND I) J))
                                           (RETURN NIL)))
                                         (SETQ EXPRTEMP
                                                 (REVAL1
                                                  (LIST (NTH TOT_DER* I)
                                                        EXPRTEMP)
                                                  NIL))
                                         (SETQ J (PLUS2 J 1))
                                         (GO LAB))
                                       (SETQ I (PLUS2 I 1))
                                       (GO LAB))
                                     (COND (*CHECKORD (CHECK_LETOP EXPRTEMP)))
                                     (REVAL1 (LIST 'TIMES DER_ODD EXPRTEMP)
                                             NIL))))))
                          (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (REVAL1 (LIST 'TIMES (MINUS 1) (CONS 'PLUS TEMPRES)) NIL)))) 
(PUT 'EV_SUPERFUN 'NUMBER-OF-ARGS 2) 
(PUT 'EV_SUPERFUN 'DEFINED-ON-LINE '347) 
(PUT 'EV_SUPERFUN 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'EV_SUPERFUN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EV_SUPERFUN (SUPERF1 SUPERF_SCAL)
    (REVAL1
     (LIST 'PLUS (EV_SUPERFUN_EVEN SUPERF1 SUPERF_SCAL)
           (EV_SUPERFUN_ODD SUPERF1 SUPERF_SCAL))
     NIL)) 
(PUT 'NIJENHUIS_BRACKET 'NUMBER-OF-ARGS 3) 
(PUT 'NIJENHUIS_BRACKET 'DEFINED-ON-LINE '356) 
(PUT 'NIJENHUIS_BRACKET 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'NIJENHUIS_BRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NIJENHUIS_BRACKET (SUPERF1 SUPERF2 SUPERF3)
    (PROG (N_DEP_VAR)
      (COND
       ((NOT (EQUAL (LENGTH DEP_VAR*) (LENGTH ODD_VAR*)))
        (REDERR "The bracket can be computed only if even and odd variables
        are in equal number")))
      (SETQ N_DEP_VAR (LENGTH DEP_VAR*))
      (COND
       ((NOT (SUPERFUNP SUPERF1))
        (REDERR "The first argument must be a declared superfunction")))
      (COND
       ((NOT (EQUAL (GET 'SFNARG SUPERF1) 1))
        (REDERR
         "The first argument must be a linear function of odd variables")))
      (COND
       ((NOT (EQUAL (GET 'SFTARGET SUPERF1) (LENGTH DEP_VAR*)))
        (REDERR
         "The first argument must be a vector-valued function with m args")))
      (COND
       ((NOT (SUPERFUNP SUPERF2))
        (REDERR "The second argument must be a declared superfunction")))
      (COND
       ((NOT (EQUAL (GET 'SFNARG SUPERF2) 1))
        (REDERR
         "The second argument must be a linear function of odd variables")))
      (COND
       ((NOT (EQUAL (GET 'SFTARGET SUPERF2) (LENGTH DEP_VAR*)))
        (REDERR
         "The second argument must be a vector-valued function with m args")))
      (MK_SUPERFUN SUPERF3 2 N_DEP_VAR)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_DEP_VAR J)) (RETURN NIL)))
        (SETK (LIST SUPERF3 J)
              (REPLACE_EXTODD
               (REVAL1
                (LIST 'PLUS (EV_SUPERFUN SUPERF1 (REVAL1 (LIST SUPERF2 J) NIL))
                      (EV_SUPERFUN SUPERF2 (REVAL1 (LIST SUPERF1 J) NIL)))
                NIL)))
        (SETQ J (PLUS2 J 1))
        (GO LAB)))) 
(FLAG '(NIJENHUIS_BRACKET) 'OPFN) 
(PUT 'CDE_VARCALC 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_VARCALC 'DEFINED-ON-LINE '390) 
(PUT 'CDE_VARCALC 'DEFINED-IN-FILE 'CDE/CDE_VARCALC.RED) 
(PUT 'CDE_VARCALC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_VARCALC NIL (PRIN2 "")) 
(ENDMODULE) 