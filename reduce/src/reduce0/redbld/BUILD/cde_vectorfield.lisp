(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CDE_VECTORFIELD)) 
(PUT 'MK_EVFIELD 'NUMBER-OF-ARGS 1) 
(PUT 'MK_EVFIELD 'DEFINED-ON-LINE '39) 
(PUT 'MK_EVFIELD 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'MK_EVFIELD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_EVFIELD (EV_FIELD)
    (PROG ()
      (SUPER_VECTORFIELD EV_FIELD DEP_VAR (REPLACE_ODDEXT ODD_VAR))
      (PUT 'EVFIELD EV_FIELD T))) 
(FLAG '(MK_EVFIELD) 'OPFN) 
(PUT 'EVFIELDP 'NUMBER-OF-ARGS 1) 
(PUT 'EVFIELDP 'DEFINED-ON-LINE '47) 
(PUT 'EVFIELDP 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'EVFIELDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EVFIELDP (EV_FIELD) (GET 'EVFIELD EV_FIELD)) 
(PUT 'PARTIAL_EVEN_JBRACKET 'NUMBER-OF-ARGS 5) 
(PUT 'PARTIAL_EVEN_JBRACKET 'DEFINED-ON-LINE '50) 
(PUT 'PARTIAL_EVEN_JBRACKET 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'PARTIAL_EVEN_JBRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARTIAL_EVEN_JBRACKET (EVFIELD1 EVFIELD2 PAR I EVENVAR)
    (PROG (TEMPVAR TEMPMIND TEMPDVAR NTEMPDVAR EXPRTEMP1 EXPRTEMP2)
      (SETQ TEMPVAR (IDTOMIND 0 EVENVAR))
      (SETQ TEMPMIND (CADR TEMPVAR))
      (SETQ TEMPDVAR (CAR TEMPVAR))
      (SETQ NTEMPDVAR (CDE_POSITION TEMPDVAR DEP_VAR*))
      (SETQ EXPRTEMP1 (REVAL1 (LIST EVFIELD1 0 NTEMPDVAR) NIL))
      (SETQ EXPRTEMP2 (REVAL1 (LIST EVFIELD2 0 NTEMPDVAR) NIL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
          (SETQ EXPRTEMP1 (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP1) NIL))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
          (SETQ EXPRTEMP2 (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP2) NIL))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ EXPRTEMP1
              (REVAL1
               (LIST 'TIMES EXPRTEMP1 (LIST 'DF (LIST EVFIELD2 PAR I) EVENVAR))
               NIL))
      (COND (*CHECKORD (CHECK_LETOP EXPRTEMP1)))
      (SETQ EXPRTEMP2
              (REVAL1
               (LIST 'TIMES EXPRTEMP2 (LIST 'DF (LIST EVFIELD1 PAR I) EVENVAR))
               NIL))
      (COND (*CHECKORD (CHECK_LETOP EXPRTEMP2)))
      (RETURN
       (REVAL1
        (LIST 'PLUS EXPRTEMP1 (REVAL1 (LIST 'TIMES (MINUS 1) EXPRTEMP2) NIL))
        NIL)))) 
(PUT 'PARTIAL_ODD_JBRACKET 'NUMBER-OF-ARGS 5) 
(PUT 'PARTIAL_ODD_JBRACKET 'DEFINED-ON-LINE '76) 
(PUT 'PARTIAL_ODD_JBRACKET 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'PARTIAL_ODD_JBRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARTIAL_ODD_JBRACKET (EVFIELD1 EVFIELD2 PAR I ODDVAR)
    (PROG (TEMPVAR TEMPMIND TEMPDVAR NTEMPDVAR EXPRTEMP1 EXPRTEMP2)
      (SETQ TEMPVAR (IDTOMIND 1 ODDVAR))
      (SETQ TEMPMIND (CADR TEMPVAR))
      (SETQ TEMPDVAR (CAR TEMPVAR))
      (SETQ NTEMPDVAR (CDE_POSITION TEMPDVAR ODD_VAR*))
      (SETQ EXPRTEMP1 (REVAL1 (LIST EVFIELD1 1 NTEMPDVAR) NIL))
      (SETQ EXPRTEMP2 (REVAL1 (LIST EVFIELD2 1 NTEMPDVAR) NIL))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
          (SETQ EXPRTEMP1 (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP1) NIL))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N_INDEP_VAR I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE (NTH TEMPMIND I) J)) (RETURN NIL)))
          (SETQ EXPRTEMP2 (REVAL1 (LIST (NTH TOT_DER* I) EXPRTEMP2) NIL))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ EXPRTEMP1
              (REVAL1
               (LIST 'TIMES EXPRTEMP1
                     (LIST 'DF_EXT (LIST EVFIELD2 PAR I) (ODDEXT ODDVAR)))
               NIL))
      (COND (*CHECKORD (CHECK_LETOP EXPRTEMP1)))
      (SETQ EXPRTEMP2
              (REVAL1
               (LIST 'TIMES EXPRTEMP2
                     (LIST 'DF_EXT (LIST EVFIELD1 PAR I) (ODDEXT ODDVAR)))
               NIL))
      (COND (*CHECKORD (CHECK_LETOP EXPRTEMP2)))
      (RETURN
       (REVAL1
        (LIST 'PLUS EXPRTEMP1 (REVAL1 (LIST 'TIMES (MINUS 1) EXPRTEMP2) NIL))
        NIL)))) 
(PUT 'JACOBI_BRACKET 'NUMBER-OF-ARGS 3) 
(PUT 'JACOBI_BRACKET 'DEFINED-ON-LINE '102) 
(PUT 'JACOBI_BRACKET 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'JACOBI_BRACKET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JACOBI_BRACKET (EV_FIELD1 EV_FIELD2 EV_FIELD3)
    (PROG (N_DEP_VAR N_ODD_VAR CNT TEMPEVEN TEMPODD)
      (SETQ N_DEP_VAR (LENGTH DEP_VAR*))
      (SETQ N_ODD_VAR (LENGTH ODD_VAR*))
      (COND
       ((NOT (EVFIELDP EV_FIELD1))
        (REDERR
         "Error: the first argument must be a declared evolutionary field")))
      (COND
       ((NOT (EVFIELDP EV_FIELD2))
        (REDERR
         "Error: the second argument must be a declared evolutionary field")))
      (MK_EVFIELD EV_FIELD3)
      (PROG (PAR)
        (SETQ PAR 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 1 PAR)) (RETURN NIL)))
        (PROGN
         (COND ((EQN PAR 0) (SETQ CNT N_DEP_VAR)) (T (SETQ CNT N_ODD_VAR)))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE CNT I)) (RETURN NIL)))
           (PROGN
            (SETQ TEMPEVEN
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL ALL_PARAMETRIC_DER*)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL)
                                          (PARTIAL_EVEN_JBRACKET EV_FIELD1
                                           EV_FIELD2 PAR I EL))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EL)
                                  (PARTIAL_EVEN_JBRACKET EV_FIELD1 EV_FIELD2
                                   PAR I EL))
                                (CAR EL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ TEMPODD
                    (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                      (SETQ EL ALL_PARAMETRIC_ODD*)
                      (COND ((NULL EL) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (EL)
                                          (PARTIAL_ODD_JBRACKET EV_FIELD1
                                           EV_FIELD2 PAR I EL))
                                        (CAR EL))
                                       NIL)))
                     LOOPLABEL
                      (SETQ EL (CDR EL))
                      (COND ((NULL EL) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (EL)
                                  (PARTIAL_ODD_JBRACKET EV_FIELD1 EV_FIELD2 PAR
                                   I EL))
                                (CAR EL))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SET_SVF EV_FIELD3 PAR I
             (REVAL1 (CONS 'PLUS (APPEND TEMPEVEN TEMPODD)) NIL)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         NIL)
        (SETQ PAR (PLUS2 PAR 1))
        (GO LAB)))) 
(FLAG '(JACOBI_BRACKET) 'OPFN) 
(PUT 'CDE_VECTORFIELD 'NUMBER-OF-ARGS 0) 
(PUT 'CDE_VECTORFIELD 'DEFINED-ON-LINE '130) 
(PUT 'CDE_VECTORFIELD 'DEFINED-IN-FILE 'CDE/CDE_VECTORFIELD.RED) 
(PUT 'CDE_VECTORFIELD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CDE_VECTORFIELD NIL (PRIN2 "")) 
(ENDMODULE) 