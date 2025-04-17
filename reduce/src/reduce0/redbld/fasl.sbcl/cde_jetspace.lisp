(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_JETSPACE)) 
(PUT 'JET_FIBER_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'JET_FIBER_DIM 'DEFINED-ON-LINE '57) 
(PUT 'JET_FIBER_DIM 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'JET_FIBER_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE JET_FIBER_DIM (JORDER)
    (PROG (N_IVAR N_DVAR)
      (SETQ N_DVAR (LENGTH DEP_VAR*))
      (SETQ N_IVAR (LENGTH INDEP_VAR*))
      (COND ((EQN JORDER 0) (RETURN N_DVAR))
            (T
             (RETURN
              (TIMES N_DVAR
                     (QUOTIENT (FACTORIAL (PLUS N_IVAR (DIFFERENCE JORDER 1)))
                               (TIMES (FACTORIAL JORDER)
                                      (FACTORIAL (DIFFERENCE N_IVAR 1)))))))))) 
(FLAG '(JET_FIBER_DIM) 'OPFN) 
(PUT 'JET_DIM 'NUMBER-OF-ARGS 1) 
(PUT 'JET_DIM 'DEFINED-ON-LINE '70) 
(PUT 'JET_DIM 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'JET_DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE JET_DIM (JORDER)
    (PROG (N_IVAR N_DVAR)
      (SETQ N_DVAR (LENGTH DEP_VAR*))
      (SETQ N_IVAR (LENGTH INDEP_VAR*))
      (COND ((EQN JORDER 0) (RETURN (PLUS N_IVAR N_DVAR)))
            (T
             (RETURN
              (PLUS N_IVAR
                    (TIMES N_DVAR
                           (QUOTIENT (FACTORIAL (PLUS N_IVAR JORDER))
                                     (TIMES (FACTORIAL N_IVAR)
                                            (FACTORIAL JORDER)))))))))) 
(FLAG '(JET_DIM) 'OPFN) 
(PUT 'LENGTH_MULTIINDEX 'NUMBER-OF-ARGS 1) 
(PUT 'LENGTH_MULTIINDEX 'DEFINED-ON-LINE '84) 
(PUT 'LENGTH_MULTIINDEX 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'LENGTH_MULTIINDEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LENGTH_MULTIINDEX (LM)
    (PROG (EL FORALL-RESULT)
      (SETQ EL LM)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL EL) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT (PLUS ((LAMBDA (EL) EL) (CAR EL)) FORALL-RESULT))
      (SETQ EL (CDR EL))
      (GO LAB1))) 
(PUT 'FACTORIAL_MULTIINDEX 'NUMBER-OF-ARGS 1) 
(PUT 'FACTORIAL_MULTIINDEX 'DEFINED-ON-LINE '88) 
(PUT 'FACTORIAL_MULTIINDEX 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'FACTORIAL_MULTIINDEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FACTORIAL_MULTIINDEX (LM)
    (PROG (EL FORALL-RESULT)
      (SETQ EL LM)
      (SETQ FORALL-RESULT 1)
     LAB1
      (COND ((NULL EL) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (TIMES ((LAMBDA (EL) (FACTORIAL EL)) (CAR EL)) FORALL-RESULT))
      (SETQ EL (CDR EL))
      (GO LAB1))) 
(PUT 'GENERATE_MULTIINDEX 'NUMBER-OF-ARGS 1) 
(PUT 'GENERATE_MULTIINDEX 'DEFINED-ON-LINE '92) 
(PUT 'GENERATE_MULTIINDEX 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'GENERATE_MULTIINDEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENERATE_MULTIINDEX (M)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 1)
      (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS (CDE_REPLACE_NTH M I (PLUS (NTH M I) 1)) NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS (CDE_REPLACE_NTH M I (PLUS (NTH M I) 1)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'GENERATE_PROLONGATION_TABLE 'NUMBER-OF-ARGS 2) 
(PUT 'GENERATE_PROLONGATION_TABLE 'DEFINED-ON-LINE '100) 
(PUT 'GENERATE_PROLONGATION_TABLE 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'GENERATE_PROLONGATION_TABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENERATE_PROLONGATION_TABLE (M TOTAL_ORDER)
    (PROG (TEMP_M_LIST TEMP_TABLE_ROWS ALL_TABLE_ROWS)
      (SETQ TEMP_M_LIST (COPY (LIST M)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (LESSP (LENGTH_MULTIINDEX (CAR TEMP_M_LIST)) TOTAL_ORDER))
          (RETURN NIL)))
        (PROGN
         (SETQ TEMP_TABLE_ROWS
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL TEMP_M_LIST)
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL)
                                       (LIST EL (GENERATE_MULTIINDEX EL)))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL) (LIST EL (GENERATE_MULTIINDEX EL)))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ TEMP_M_LIST
                 (CDE_MKSET
                  (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ ELL TEMP_TABLE_ROWS)
                   STARTOVER
                    (COND ((NULL ELL) (RETURN NIL)))
                    (SETQ FORALL-RESULT ((LAMBDA (ELL) (CADR ELL)) (CAR ELL)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ ELL (CDR ELL))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (ELL) (CADR ELL)) (CAR ELL)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ ELL (CDR ELL))
                    (GO LOOPLABEL))))
         (SETQ ALL_TABLE_ROWS (APPEND ALL_TABLE_ROWS TEMP_TABLE_ROWS))
         NIL)
        (GO WHILELABEL))
      (SETQ TEMP_M_LIST
              (PROG (ELLL FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELLL TEMP_M_LIST)
                (COND ((NULL ELLL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELLL) (LIST ELLL NIL)) (CAR ELLL))
                                 NIL)))
               LOOPLABEL
                (SETQ ELLL (CDR ELLL))
                (COND ((NULL ELLL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELLL) (LIST ELLL NIL)) (CAR ELLL))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (APPEND ALL_TABLE_ROWS TEMP_M_LIST)))) 
(PUT 'JOIN_DVAR_MIND 'NUMBER-OF-ARGS 2) 
(PUT 'JOIN_DVAR_MIND 'DEFINED-ON-LINE '119) 
(PUT 'JOIN_DVAR_MIND 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'JOIN_DVAR_MIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JOIN_DVAR_MIND (DVAR TM)
    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
      (SETQ EL TM)
     STARTOVER
      (COND ((NULL EL) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (EL)
                 (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELL DVAR)
                   (COND ((NULL ELL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELL) (LIST ELL (CAR EL)))
                                     (CAR ELL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELL (CDR ELL))
                   (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (ELL) (LIST ELL (CAR EL))) (CAR ELL))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
               (CAR EL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ EL (CDR EL))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL EL) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (EL)
                 (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ ELL DVAR)
                   (COND ((NULL ELL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELL) (LIST ELL (CAR EL)))
                                     (CAR ELL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ ELL (CDR ELL))
                   (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (ELL) (LIST ELL (CAR EL))) (CAR ELL))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
               (CAR EL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ EL (CDR EL))
      (GO LOOPLABEL))) 
(PUT 'MIND_TO_EID 'NUMBER-OF-ARGS 1) 
(PUT 'MIND_TO_EID 'DEFINED-ON-LINE '126) 
(PUT 'MIND_TO_EID 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'MIND_TO_EID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MIND_TO_EID (DER)
    (PROG (MIP TEMPIND)
      (SETQ MIP
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROGN
                         (SETQ TEMPIND (NTH (CADR DER) I))
                         (COND ((EQUAL TEMPIND 0) NIL)
                               ((EQUAL TEMPIND 1) (LIST (NTH INDEP_VAR* I)))
                               (T (LIST TEMPIND (NTH INDEP_VAR* I))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROGN
                         (SETQ TEMPIND (NTH (CADR DER) I))
                         (COND ((EQUAL TEMPIND 0) NIL)
                               ((EQUAL TEMPIND 1) (LIST (NTH INDEP_VAR* I)))
                               (T (LIST TEMPIND (NTH INDEP_VAR* I))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (RETURN
       (COND ((NOT MIP) (CAR DER))
             (T (CDE_LIST2ID (CONS (CAR DER) (CONS '_ MIP)))))))) 
(PUT 'IDTOMIND 'NUMBER-OF-ARGS 2) 
(PUT 'IDTOMIND 'DEFINED-ON-LINE '141) 
(PUT 'IDTOMIND 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'IDTOMIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDTOMIND (PAR DER_ID)
    (COND ((EQUAL PAR 0) (CDR (ASSOC DER_ID I2M_JETSPACE*)))
          (T (CDR (ASSOC DER_ID I2M_JETSPACE_ODD*))))) 
(PUT 'ORDER_OF_DER_MIND 'NUMBER-OF-ARGS 1) 
(PUT 'ORDER_OF_DER_MIND 'DEFINED-ON-LINE '148) 
(PUT 'ORDER_OF_DER_MIND 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'ORDER_OF_DER_MIND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ORDER_OF_DER_MIND (DER_MIND) (LENGTH_MULTIINDEX (CADR DER_MIND))) 
(PUT 'ORDER_OF_DER 'NUMBER-OF-ARGS 2) 
(PUT 'ORDER_OF_DER 'DEFINED-ON-LINE '153) 
(PUT 'ORDER_OF_DER 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'ORDER_OF_DER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDER_OF_DER (PAR DER_ID)
    (COND ((EQUAL PAR 0) (CDR (ASSOC DER_ID I2O_JETSPACE*)))
          (T (CDR (ASSOC DER_ID I2O_JETSPACE_ODD*))))) 
(FLAG '(ORDER_OF_DER) 'OPFN) 
(PUT 'SELECT_ALL_DERS 'NUMBER-OF-ARGS 3) 
(PUT 'SELECT_ALL_DERS 'DEFINED-ON-LINE '160) 
(PUT 'SELECT_ALL_DERS 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'SELECT_ALL_DERS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SELECT_ALL_DERS (PAR VAR L_VARS)
    (PROG (TEMPODDVARS)
      (PROG (EL)
        (SETQ EL L_VARS)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((EQ (CAR (IDTOMIND PAR EL)) VAR)
             (SETQ TEMPODDVARS (CONS EL TEMPODDVARS)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN TEMPODDVARS))) 
(PUT 'SELECTVARS0 'NUMBER-OF-ARGS 4) 
(PUT 'SELECTVARS0 'DEFINED-ON-LINE '170) 
(PUT 'SELECTVARS0 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'SELECTVARS0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SELECTVARS0 (PAR ORDEROFDER SDEPVARS SVARS)
    (PROG (FDO)
      (PROG (EL)
        (SETQ EL SVARS)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((AND (MEMBER (CAR (IDTOMIND PAR EL)) SDEPVARS)
                  (EQN (ORDER_OF_DER PAR EL) ORDEROFDER))
             (SETQ FDO (CONS EL FDO)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN (REVERSE FDO)))) 
(PUT 'SELECTVARS 'NUMBER-OF-ARGS 4) 
(PUT 'SELECTVARS 'DEFINED-ON-LINE '180) 
(PUT 'SELECTVARS 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'SELECTVARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SELECTVARS (PAR ORDEROFDER DEPVARS VARS)
    (CONS 'LIST (SELECTVARS0 PAR ORDEROFDER (CDR DEPVARS) (CDR VARS)))) 
(FLAG '(SELECTVARS) 'OPFN) 
(PUT 'CDE_JETSPACE 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_JETSPACE 'DEFINED-ON-LINE '188) 
(PUT 'CDE_JETSPACE 'DEFINED-IN-FILE 'CDE/CDE_JETSPACE.RED) 
(PUT 'CDE_JETSPACE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_JETSPACE NIL
    (PROG ()
      (SETQ N_INDEP_VAR (LENGTH INDEP_VAR*))
      (COND
       ((EQUAL N_INDEP_VAR 0) (REDERR "Error: missing dependent variables")))
      (SETQ ALL_MIND_TABLE*
              (GENERATE_PROLONGATION_TABLE (CDE_MKZERO (LENGTH INDEP_VAR*))
               TOTAL_ORDER))
      (SETQ ALL_DER_MIND* (JOIN_DVAR_MIND DEP_VAR* ALL_MIND_TABLE*))
      (SETQ ALL_ODD_MIND* (JOIN_DVAR_MIND ODD_VAR* ALL_MIND_TABLE*))
      (SETQ ALL_MIND*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_MIND_TABLE*)
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
      (SETQ ALL_DER_ID*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_DER_MIND*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (MIND_TO_EID EL)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (MIND_TO_EID EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALL_DER_ID
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST ALL_DER_ID*)))
      (SETQ ALL_ODD_ID*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_ODD_MIND*)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (EL) (MIND_TO_EID EL)) (CAR EL))
                                      NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (MIND_TO_EID EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALL_ODD_ID
              (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (CONS 'LIST ALL_ODD_ID*)))
      (SETQ N_ALL_EXT (LENGTH ALL_ODD_ID*))
      (SETQ I2M_JETSPACE* (PAIR ALL_DER_ID* ALL_DER_MIND*))
      (SETQ I2M_JETSPACE_ODD* (PAIR ALL_ODD_ID* ALL_ODD_MIND*))
      (SETQ I2O_JETSPACE*
              (PAIR ALL_DER_ID*
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL ALL_DER_MIND*)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (ORDER_OF_DER_MIND EL))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EL) (ORDER_OF_DER_MIND EL)) (CAR EL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ I2O_JETSPACE_ODD*
              (PAIR ALL_ODD_ID*
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL ALL_ODD_MIND*)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL) (ORDER_OF_DER_MIND EL))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EL) (ORDER_OF_DER_MIND EL)) (CAR EL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))) 
(ENDMODULE) 