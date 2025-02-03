(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_PARAMETRIC)) 
(PUT 'PROCESS_PRINCIPAL_DER 'NUMBER-OF-ARGS 2) 
(PUT 'PROCESS_PRINCIPAL_DER 'DEFINED-ON-LINE '38) 
(PUT 'PROCESS_PRINCIPAL_DER 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'PROCESS_PRINCIPAL_DER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROCESS_PRINCIPAL_DER (PAR L_VAR)
    (PROG (L_MIND DEP_VAR_TEMP L_TEMP L_VAR_PROCESSED)
      (SETQ L_MIND
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL L_VAR)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL) (IDTOMIND PAR EL)) (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (EL) (IDTOMIND PAR EL)) (CAR EL)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((EQUAL PAR 0) (SETQ DEP_VAR_TEMP DEP_VAR*))
            (T (SETQ DEP_VAR_TEMP ODD_VAR*)))
      (PROG (EL)
        (SETQ EL DEP_VAR_TEMP)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ L_TEMP (LIST))
            (PROG (ELL)
              (SETQ ELL L_MIND)
             LAB
              (COND ((NULL ELL) (RETURN NIL)))
              ((LAMBDA (ELL)
                 (COND ((EQUAL EL (CAR ELL)) (SETQ L_TEMP (CONS ELL L_TEMP)))))
               (CAR ELL))
              (SETQ ELL (CDR ELL))
              (GO LAB))
            (COND
             (L_TEMP
              (PROGN
               (SETQ L_TEMP (REVERSE L_TEMP))
               (SETQ L_VAR_PROCESSED (CONS L_TEMP L_VAR_PROCESSED)))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN (REVERSE L_VAR_PROCESSED)))) 
(PUT 'GENERATE_PRINCIPAL_SUBTABLE 'NUMBER-OF-ARGS 2) 
(PUT 'GENERATE_PRINCIPAL_SUBTABLE 'DEFINED-ON-LINE '61) 
(PUT 'GENERATE_PRINCIPAL_SUBTABLE 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'GENERATE_PRINCIPAL_SUBTABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENERATE_PRINCIPAL_SUBTABLE (LIST_PD TOTAL_ORDER)
    (PROG (DEP_VAR_TEMP PRINCIPAL_MTABLE_TEMP PRINCIPAL_MTABLE_ORD ORD_EL)
      (SETQ DEP_VAR_TEMP (CAAR LIST_PD))
      (SETQ PRINCIPAL_MTABLE_TEMP
              (CDE_MKSET
               (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ EL LIST_PD)
                STARTOVER
                 (COND ((NULL EL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (EL)
                            (GENERATE_PROLONGATION_TABLE (CADR EL)
                             TOTAL_ORDER))
                          (CAR EL)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ EL (CDR EL))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL EL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (EL)
                            (GENERATE_PROLONGATION_TABLE (CADR EL)
                             TOTAL_ORDER))
                          (CAR EL)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ EL (CDR EL))
                 (GO LOOPLABEL))))
      (SETQ PRINCIPAL_MTABLE_ORD
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (LIST) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (LIST) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (EL)
        (SETQ EL PRINCIPAL_MTABLE_TEMP)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ ORD_EL (PLUS (LENGTH_MULTIINDEX (CAR EL)) 1))
            (SETQ PRINCIPAL_MTABLE_ORD
                    (CDE_REPLACE_NTH PRINCIPAL_MTABLE_ORD ORD_EL
                     (CONS EL (NTH PRINCIPAL_MTABLE_ORD ORD_EL))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (RETURN (LIST DEP_VAR_TEMP PRINCIPAL_MTABLE_ORD)))) 
(PUT 'GENERATE_ALL_PRINCIPAL_SUBTABLES 'NUMBER-OF-ARGS 3) 
(PUT 'GENERATE_ALL_PRINCIPAL_SUBTABLES 'DEFINED-ON-LINE '83) 
(PUT 'GENERATE_ALL_PRINCIPAL_SUBTABLES 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'GENERATE_ALL_PRINCIPAL_SUBTABLES 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GENERATE_ALL_PRINCIPAL_SUBTABLES (PAR PRIN_DER TOTAL_ORDER)
    (PROG (PRINCIPAL_DER_SPLIT)
      (SETQ PRINCIPAL_DER_SPLIT (PROCESS_PRINCIPAL_DER PAR PRIN_DER))
      (RETURN
       (PROG (EL FORALL-RESULT FORALL-ENDPTR)
         (SETQ EL PRINCIPAL_DER_SPLIT)
         (COND ((NULL EL) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (EL)
                             (GENERATE_PRINCIPAL_SUBTABLE EL TOTAL_ORDER))
                           (CAR EL))
                          NIL)))
        LOOPLABEL
         (SETQ EL (CDR EL))
         (COND ((NULL EL) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (EL) (GENERATE_PRINCIPAL_SUBTABLE EL TOTAL_ORDER))
                   (CAR EL))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'GENERATE_ALL_PRINCIPAL_DER 'NUMBER-OF-ARGS 3) 
(PUT 'GENERATE_ALL_PRINCIPAL_DER 'DEFINED-ON-LINE '92) 
(PUT 'GENERATE_ALL_PRINCIPAL_DER 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'GENERATE_ALL_PRINCIPAL_DER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GENERATE_ALL_PRINCIPAL_DER (PAR SPLIT_SUBTABLE TOTAL_ORDER)
    (PROG (DEP_VAR_TEMP PRINCIPAL_MTABLE_TEMP PRINCIPAL_DER_TEMP I2M_TEMP APD
           APD_TOT)
      (COND ((EQUAL PAR 0) (SETQ I2M_TEMP I2M_JETSPACE*))
            (T (SETQ I2M_TEMP I2M_JETSPACE_ODD*)))
      (PROG (EL)
        (SETQ EL SPLIT_SUBTABLE)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (PROGN
            (SETQ DEP_VAR_TEMP (CAR EL))
            (SETQ PRINCIPAL_MTABLE_TEMP (CADR EL))
            (SETQ PRINCIPAL_DER_TEMP
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 0)
                      (COND ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR (CONS (LIST) NIL)))
                     LOOPLABEL
                      (SETQ I (PLUS2 I 1))
                      (COND
                       ((MINUSP (DIFFERENCE TOTAL_ORDER I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR (CONS (LIST) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (PROG (I)
              (SETQ I 0)
             LAB
              (COND ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN NIL)))
              (COND
               ((NTH PRINCIPAL_MTABLE_TEMP (PLUS I 1))
                (SETQ PRINCIPAL_DER_TEMP
                        (CDE_REPLACE_NTH PRINCIPAL_DER_TEMP (PLUS I 1)
                         (PROG (ELL FORALL-RESULT FORALL-ENDPTR)
                           (SETQ ELL (NTH PRINCIPAL_MTABLE_TEMP (PLUS I 1)))
                           (COND ((NULL ELL) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (ELL)
                                               (CAR
                                                (CDE_LASSOC2
                                                 (LIST DEP_VAR_TEMP (CAR ELL))
                                                 I2M_TEMP)))
                                             (CAR ELL))
                                            NIL)))
                          LOOPLABEL
                           (SETQ ELL (CDR ELL))
                           (COND ((NULL ELL) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ELL)
                                       (CAR
                                        (CDE_LASSOC2
                                         (LIST DEP_VAR_TEMP (CAR ELL))
                                         I2M_TEMP)))
                                     (CAR ELL))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ APD (CONS PRINCIPAL_DER_TEMP APD))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (SETQ APD (REVERSE APD))
      (SETQ APD_TOT
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (LIST) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (LIST) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE TOTAL_ORDER I)) (RETURN NIL)))
        (SETQ APD_TOT
                (CDE_REPLACE_NTH APD_TOT (PLUS I 1)
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL APD)
                  STARTOVER
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (EL) (NTH EL (PLUS I 1))) (CAR EL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ EL (CDR EL))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (EL) (NTH EL (PLUS I 1))) (CAR EL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ EL (CDR EL))
                   (GO LOOPLABEL))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN APD_TOT))) 
(PUT 'CDE_PRINCIPAL_PARAMETRIC_DERIVATIVES 'NUMBER-OF-ARGS 1) 
(PUT 'CDE_PRINCIPAL_PARAMETRIC_DERIVATIVES 'DEFINED-ON-LINE '118) 
(PUT 'CDE_PRINCIPAL_PARAMETRIC_DERIVATIVES 'DEFINED-IN-FILE
     'CDE/CDE_PARAMETRIC.RED) 
(PUT 'CDE_PRINCIPAL_PARAMETRIC_DERIVATIVES 'PROCEDURE_TYPE
     '(ARROW GENERAL GENERAL)) 
(DE CDE_PRINCIPAL_PARAMETRIC_DERIVATIVES (TOTAL_ORDER)
    (PROG (ALL_PRINCIPAL_DER_SUBTABLES ALL_PRINCIPAL_DER_SPLIT
           ALL_PRINCIPAL_ODD_SUBTABLES ALL_PRINCIPAL_ODD_SPLIT)
      (SETQ ALL_PRINCIPAL_DER_SUBTABLES
              (GENERATE_ALL_PRINCIPAL_SUBTABLES 0 PRINCIPAL_DER* TOTAL_ORDER))
      (SETQ ALL_PRINCIPAL_DER_SPLIT
              (GENERATE_ALL_PRINCIPAL_DER 0 ALL_PRINCIPAL_DER_SUBTABLES
               TOTAL_ORDER))
      (SETQ ALL_PRINCIPAL_DER*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_PRINCIPAL_DER_SPLIT)
               STARTOVER
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (EL) EL) (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EL (CDR EL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (EL) EL) (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EL (CDR EL))
                (GO LOOPLABEL)))
      (SETQ ALL_PRINCIPAL_DER
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST ALL_PRINCIPAL_DER*)))
      (SETQ ALL_PARAMETRIC_DER* (CDE_DIFFSET ALL_DER_ID* ALL_PRINCIPAL_DER*))
      (SETQ ALL_PARAMETRIC_DER
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST ALL_PARAMETRIC_DER*)))
      (SETQ ALL_PRINCIPAL_ODD_SUBTABLES
              (GENERATE_ALL_PRINCIPAL_SUBTABLES 1 PRINCIPAL_ODD* TOTAL_ORDER))
      (SETQ ALL_PRINCIPAL_ODD_SPLIT
              (GENERATE_ALL_PRINCIPAL_DER 1 ALL_PRINCIPAL_ODD_SUBTABLES
               TOTAL_ORDER))
      (SETQ ALL_PRINCIPAL_ODD*
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL ALL_PRINCIPAL_ODD_SPLIT)
               STARTOVER
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT ((LAMBDA (EL) EL) (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ EL (CDR EL))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR ((LAMBDA (EL) EL) (CAR EL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ EL (CDR EL))
                (GO LOOPLABEL)))
      (SETQ ALL_PRINCIPAL_ODD
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST ALL_PRINCIPAL_ODD*)))
      (SETQ ALL_PARAMETRIC_ODD* (CDE_DIFFSET ALL_ODD_ID* ALL_PRINCIPAL_ODD*))
      (SETQ ALL_PARAMETRIC_ODD
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST ALL_PARAMETRIC_ODD*)))
      (SETQ N_ALL_PARAMETRIC_EXT (LENGTH ALL_PARAMETRIC_ODD*))
      (SETQ N_ALL_PRINCIPAL_EXT (LENGTH ALL_PRINCIPAL_ODD*)))) 
(PUT 'EXTODD 'NUMBER-OF-ARGS 1) 
(PUT 'EXTODD 'DEFINED-ON-LINE '147) 
(PUT 'EXTODD 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'EXTODD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXTODD (EXT_VAR)
    (PROG (N_EXT)
      (SETQ N_EXT 0)
      (COND
       ((NEQ (CAR EXT_VAR) 'EXT)
        (REDERR "the argument of extodd must be an ext variable")))
      (SETQ N_EXT (CADR EXT_VAR))
      (COND
       ((NUMBERP N_EXT)
        (RETURN
         (COND
          ((GREATERP N_EXT N_ALL_PARAMETRIC_EXT)
           (NTH ALL_PRINCIPAL_ODD* (DIFFERENCE N_EXT N_ALL_PARAMETRIC_EXT)))
          (T (NTH ALL_PARAMETRIC_ODD* N_EXT))))))
      (COND
       ((OR (MEMBER N_EXT ALL_PARAMETRIC_ODD*)
            (MEMBER N_EXT ALL_PRINCIPAL_ODD*))
        (RETURN N_EXT))
       (T (REDERR "the argument of extodd must be an ext variable"))))) 
(PUT 'LONG_EXTODD 'NUMBER-OF-ARGS 1) 
(PUT 'LONG_EXTODD 'DEFINED-ON-LINE '162) 
(PUT 'LONG_EXTODD 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'LONG_EXTODD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LONG_EXTODD (LONG_EXT_VAR)
    (CONS 'EXT
          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
            (SETQ EL (CDR LONG_EXT_VAR))
            (COND ((NULL EL) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (COND ((NUMBERP EL) (EXTODD (LIST 'EXT EL)))
                                      ((OR (MEMBER EL ALL_PARAMETRIC_ODD*)
                                           (MEMBER EL ALL_PRINCIPAL_ODD*))
                                       EL)
                                      (T
                                       (REDERR
                                        "ext must contain only numbers or odd variables"))))
                              (CAR EL))
                             NIL)))
           LOOPLABEL
            (SETQ EL (CDR EL))
            (COND ((NULL EL) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (EL)
                        (COND ((NUMBERP EL) (EXTODD (LIST 'EXT EL)))
                              ((OR (MEMBER EL ALL_PARAMETRIC_ODD*)
                                   (MEMBER EL ALL_PRINCIPAL_ODD*))
                               EL)
                              (T
                               (REDERR
                                "ext must contain only numbers or odd variables"))))
                      (CAR EL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'REPLACE_EXTODD0 'NUMBER-OF-ARGS 1) 
(PUT 'REPLACE_EXTODD0 'DEFINED-ON-LINE '169) 
(PUT 'REPLACE_EXTODD0 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'REPLACE_EXTODD0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPLACE_EXTODD0 (LISTEXPR)
    (PROG (TEMPLIST TEMPLIST2 TEMP_REPEXTODD)
      (SETQ TEMPLIST (CDR (CDIFF_GET_KERNELS LISTEXPR 'EXT)))
      (PROG (EL)
        (SETQ EL TEMPLIST)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((EQN (LENGTH EL) 2)
             (SETQ TEMP_REPEXTODD
                     (CONS (LIST 'REPLACEBY EL (EXTODD EL)) TEMP_REPEXTODD)))
            (T (SETQ TEMPLIST2 (CONS EL TEMPLIST2)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (SETQ TEMP_REPEXTODD
              (CONS 'LIST
                    (APPEND
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL TEMPLIST2)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (LIST 'REPLACEBY EL
                                                 (LONG_EXTODD EL)))
                                         (CAR EL))
                                        NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL)
                                   (LIST 'REPLACEBY EL (LONG_EXTODD EL)))
                                 (CAR EL))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     TEMP_REPEXTODD)))
      (RETURN (AEVAL (LIST 'SUB TEMP_REPEXTODD LISTEXPR))))) 
(PUT 'REPLACE_EXTODD 'NUMBER-OF-ARGS 1) 
(PUT 'REPLACE_EXTODD 'DEFINED-ON-LINE '185) 
(PUT 'REPLACE_EXTODD 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'REPLACE_EXTODD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPLACE_EXTODD (LISTEXPR)
    (COND ((ATOM LISTEXPR) (REPLACE_EXTODD0 LISTEXPR))
          ((AND (LISTP LISTEXPR) (EQUAL (CAR LISTEXPR) 'LIST))
           (CONS 'LIST
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL (CDR LISTEXPR))
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL) (REPLACE_EXTODD EL))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (EL) (REPLACE_EXTODD EL)) (CAR EL))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (REPLACE_EXTODD0 LISTEXPR)))) 
(FLAG '(REPLACE_EXTODD) 'OPFN) 
(PUT 'ODDEXT 'NUMBER-OF-ARGS 1) 
(PUT 'ODDEXT 'DEFINED-ON-LINE '193) 
(PUT 'ODDEXT 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'ODDEXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODDEXT (ODD_VAR)
    (COND
     ((MEMBER ODD_VAR ALL_PARAMETRIC_ODD*)
      (LIST 'EXT (CDE_POSITION ODD_VAR ALL_PARAMETRIC_ODD*)))
     ((MEMBER ODD_VAR ALL_PRINCIPAL_ODD*)
      (LIST 'EXT
            (PLUS N_ALL_PARAMETRIC_EXT
                  (CDE_POSITION ODD_VAR ALL_PRINCIPAL_ODD*))))
     (T (REDERR "the argument of oddext must be an odd variable")))) 
(PUT 'LONG_ODDEXT 'NUMBER-OF-ARGS 1) 
(PUT 'LONG_ODDEXT 'DEFINED-ON-LINE '202) 
(PUT 'LONG_ODDEXT 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'LONG_ODDEXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LONG_ODDEXT (EXT_ODD_VAR)
    (CONS 'EXT
          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
            (SETQ EL (CDR EXT_ODD_VAR))
            (COND ((NULL EL) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (COND ((NUMBERP EL) EL)
                                      ((MEMBER EL ALL_PARAMETRIC_ODD*)
                                       (CDE_POSITION2 EL ALL_PARAMETRIC_ODD*))
                                      ((MEMBER EL ALL_PRINCIPAL_ODD*)
                                       (PLUS N_ALL_PARAMETRIC_EXT
                                             (CDE_POSITION2 EL
                                              ALL_PRINCIPAL_ODD*)))
                                      (T
                                       (REDERR
                                        "ext must contain only numbers or odd variables"))))
                              (CAR EL))
                             NIL)))
           LOOPLABEL
            (SETQ EL (CDR EL))
            (COND ((NULL EL) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (EL)
                        (COND ((NUMBERP EL) EL)
                              ((MEMBER EL ALL_PARAMETRIC_ODD*)
                               (CDE_POSITION2 EL ALL_PARAMETRIC_ODD*))
                              ((MEMBER EL ALL_PRINCIPAL_ODD*)
                               (PLUS N_ALL_PARAMETRIC_EXT
                                     (CDE_POSITION2 EL ALL_PRINCIPAL_ODD*)))
                              (T
                               (REDERR
                                "ext must contain only numbers or odd variables"))))
                      (CAR EL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'REPLACE_ODDEXT0 'NUMBER-OF-ARGS 1) 
(PUT 'REPLACE_ODDEXT0 'DEFINED-ON-LINE '212) 
(PUT 'REPLACE_ODDEXT0 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'REPLACE_ODDEXT0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPLACE_ODDEXT0 (LISTEXPR)
    (PROG (TEMPLIST TEMPLIST_ODD TEMPLIST2 TEMPLIST3 TEMP_REPODDEXT)
      (SETQ TEMPLIST (KERNELS (CAR (SIMP LISTEXPR))))
      (PROG (EL)
        (SETQ EL TEMPLIST)
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((AND (LISTP EL) (EQUAL (CAR EL) 'EXT) (NOT (FIXP (CADR EL))))
             (SETQ TEMPLIST2 (CONS EL TEMPLIST2)))
            ((MEMBER EL ALL_ODD_ID*)
             (SETQ TEMPLIST_ODD (CONS EL TEMPLIST_ODD)))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (COND
       (TEMPLIST2
        (PROGN
         (PROG (EL)
           (SETQ EL TEMPLIST2)
          LAB
           (COND ((NULL EL) (RETURN NIL)))
           ((LAMBDA (EL)
              (COND
               ((EQN (LENGTH EL) 2)
                (SETQ TEMP_REPODDEXT
                        (CONS (LIST 'REPLACEBY EL (ODDEXT (CADR EL)))
                              TEMP_REPODDEXT)))
               (T (SETQ TEMPLIST3 (CONS EL TEMPLIST3)))))
            (CAR EL))
           (SETQ EL (CDR EL))
           (GO LAB))
         (SETQ TEMP_REPODDEXT
                 (APPEND
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL TEMPLIST3)
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL)
                                        (LIST 'REPLACEBY EL (LONG_ODDEXT EL)))
                                      (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (LIST 'REPLACEBY EL (LONG_ODDEXT EL)))
                              (CAR EL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  TEMP_REPODDEXT))
         NIL)))
      (COND
       (TEMPLIST_ODD
        (SETQ TEMP_REPODDEXT
                (APPEND
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL TEMPLIST_ODD)
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL)
                                       (LIST 'REPLACEBY EL (ODDEXT EL)))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (EL) (LIST 'REPLACEBY EL (ODDEXT EL)))
                             (CAR EL))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 TEMP_REPODDEXT))))
      (COND
       (TEMP_REPODDEXT
        (RETURN (AEVAL (LIST 'SUB (CONS 'LIST TEMP_REPODDEXT) LISTEXPR))))
       (T (RETURN LISTEXPR))))) 
(PUT 'REPLACE_ODDEXT 'NUMBER-OF-ARGS 1) 
(PUT 'REPLACE_ODDEXT 'DEFINED-ON-LINE '246) 
(PUT 'REPLACE_ODDEXT 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'REPLACE_ODDEXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REPLACE_ODDEXT (LISTEXPR)
    (COND ((ATOM LISTEXPR) (REPLACE_ODDEXT0 LISTEXPR))
          ((AND (LISTP LISTEXPR) (EQUAL (CAR LISTEXPR) 'LIST))
           (CONS 'LIST
                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ EL (CDR LISTEXPR))
                   (COND ((NULL EL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (EL) (REPLACE_ODDEXT EL))
                                     (CAR EL))
                                    NIL)))
                  LOOPLABEL
                   (SETQ EL (CDR EL))
                   (COND ((NULL EL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (EL) (REPLACE_ODDEXT EL)) (CAR EL))
                                 NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T (REPLACE_ODDEXT0 LISTEXPR)))) 
(FLAG '(REPLACE_ODDEXT) 'OPFN) 
(PUT 'ODD_PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'ODD_PRODUCT 'DEFINED-ON-LINE '254) 
(PUT 'ODD_PRODUCT 'DEFINED-IN-FILE 'CDE/CDE_PARAMETRIC.RED) 
(PUT 'ODD_PRODUCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODD_PRODUCT (ARG1 ARG2)
    (REPLACE_EXTODD
     (SUPER_PRODUCT (REPLACE_ODDEXT ARG1) (REPLACE_ODDEXT ARG2)))) 
(FLAG '(ODD_PRODUCT) 'OPFN) 
(ENDMODULE) 