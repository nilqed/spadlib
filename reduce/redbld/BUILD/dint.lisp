(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DINT)) 
(FLUID '(*HOLD-INT* *PRECISE)) 
(SWITCH (LIST 'ACN)) 
(PUT 'SIMPDINT 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDINT 'DEFINED-ON-LINE '40) 
(PUT 'SIMPDINT 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'SIMPDINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDINT (U)
    (PROG (CFLAG DMOD RESULT CEFLG)
     *COMBINEEXPT
      (COND
       ((NEQ (LENGTH U) 4)
        (RERROR 'INT 2 "Improper number of arguments to INT")))
      (COND
       (DMODE*
        ((LAMBDA (*MSG)
           (PROGN
            (COND ((SETQ CFLAG (GET DMODE* 'CMPXFN)) (ONOFF 'COMPLEX NIL)))
            (COND ((SETQ DMOD (GET DMODE* 'DNAME)) (ONOFF DMOD NIL)))))
         NIL)))
      (SETQ RESULT (SIMPDINT1 U))
      ((LAMBDA (*MSG)
         (PROGN
          (COND (DMOD (ONOFF DMOD T)))
          (COND (CFLAG (ONOFF 'COMPLEX T)))))
       NIL)
      (COND
       ((OR DMOD CFLAG)
        ((LAMBDA (*HOLD-INT*) (SETQ RESULT (RESIMP RESULT))) T)))
      (RETURN RESULT))) 
(PUT 'SIMPDINT1 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDINT1 'DEFINED-ON-LINE '64) 
(PUT 'SIMPDINT1 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'SIMPDINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDINT1 (U)
    (PROG (LOW UPP FN VAR X Y RESULT)
      (SETQ FN (CAR U))
      (SETQ VAR (CADR U))
      (SETQ LOW (CADDR U))
      (SETQ UPP (CADDDR U))
      (SETQ LOW (REVAL1 LOW T))
      (SETQ UPP (REVAL1 UPP T))
      (COND
       ((AND (NEQ LOW 'INFINITY) (NEQ LOW '(MINUS INFINITY))
             (NEQ UPP 'INFINITY) (NEQ UPP '(MINUS INFINITY)) (IDP VAR) *ACN)
        (PROGN
         (SETQ RESULT (SIMPINT (LIST FN VAR)))
         (COND
          ((NOT (SMEMQ 'INT RESULT))
           (PROGN
            (SETQ X (SUBSQ RESULT (LIST (CONS VAR LOW))))
            (SETQ Y (SUBSQ RESULT (LIST (CONS VAR UPP))))
            (RETURN (ADDSQ (NEGSQ X) Y))))))))
      (COND ((EQUAL LOW UPP) (RETURN (CONS NIL 1)))
            ((NULL (GETD 'NEW_DEFINT)) NIL)
            ((EQUAL UPP 'INFINITY)
             (COND
              ((EQUAL LOW 0)
               (COND
                ((NOT
                  (SMEMQL '(INFINITY UNKNOWN FAIL)
                          (SETQ X (DEFINT* (LIST FN VAR)))))
                 (RETURN (SIMP* X)))
                (T NIL)))
              ((EQUAL LOW '(MINUS INFINITY)) (RETURN (MKINFINT FN VAR)))
              ((FREEOF VAR LOW)
               (COND
                ((AND
                  (NOT
                   (SMEMQL '(INFINITY UNKNOWN FAIL)
                           (SETQ X (DEFINT* (LIST FN VAR)))))
                  (NOT
                   (SMEMQL '(INFINITY UNKNOWN FAIL)
                           (SETQ Y (INDEFINT* (LIST FN VAR LOW))))))
                 (RETURN (SIMP* (LIST 'DIFFERENCE X Y))))
                (T NIL)))
              (T NIL)))
            ((OR (EQUAL UPP '(MINUS INFINITY)) (EQUAL LOW 'INFINITY))
             (RETURN (NEGSQ (SIMPDINT (LIST FN VAR UPP LOW)))))
            ((EQUAL LOW '(MINUS INFINITY))
             (RETURN
              (SIMPDINT
               (LIST
                (PREPSQ
                 (SIMP (LIST 'SUB (LIST 'EQUAL VAR (LIST 'MINUS VAR)) FN)))
                VAR (LIST 'MINUS UPP) 'INFINITY))))
            ((EQUAL LOW 0)
             (COND
              ((AND (FREEOF VAR UPP)
                    (NOT
                     (SMEMQL '(INFINITY UNKNOWN FAIL)
                             (SETQ X (INDEFINT* (LIST FN VAR UPP))))))
               (RETURN (SIMP* X)))
              (T NIL)))
            ((AND (FREEOF VAR UPP) (FREEOF VAR LOW)
                  (NOT
                   (SMEMQL '(INFINITY UNKNOWN FAIL)
                           (SETQ X (INDEFINT* (LIST FN VAR UPP)))))
                  (NOT
                   (SMEMQL '(INFINITY UNKNOWN FAIL)
                           (SETQ Y (INDEFINT* (LIST FN VAR LOW))))))
             (RETURN (SIMP* (LIST 'DIFFERENCE X Y)))))
      (SETQ RESULT (MKDINT FN VAR LOW UPP))
      (RETURN RESULT))) 
(PUT 'DEFINT* 'NUMBER-OF-ARGS 1) 
(PUT 'DEFINT* 'DEFINED-ON-LINE '122) 
(PUT 'DEFINT* 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'DEFINT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEFINT* (U)
    ((LAMBDA (X) (COND ((ERRORP X) 'UNKNOWN) (T (CAR X))))
     (ERRORSET2 (LIST 'NEW_DEFINT (MKQUOTE U))))) 
(PUT 'INDEFINT* 'NUMBER-OF-ARGS 1) 
(PUT 'INDEFINT* 'DEFINED-ON-LINE '126) 
(PUT 'INDEFINT* 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'INDEFINT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEFINT* (U)
    ((LAMBDA (X)
       (COND ((OR (ERRORP X) (SMEMQ 'INDEFINT2 (CAR X))) 'UNKNOWN)
             (T (CAR X))))
     (ERRORSET2 (LIST 'NEW_INDEFINT (MKQUOTE U))))) 
(PUT 'MKDINT 'NUMBER-OF-ARGS 4) 
(PUT 'MKDINT 'DEFINED-ON-LINE '130) 
(PUT 'MKDINT 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'MKDINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKDINT (FN VAR LOW UPP)
    (PROG (X *PRECISE)
      (COND
       ((AND (GETD 'DEFINT0)
             (NOT (EQ (SETQ X (DEFINT0 (LIST FN VAR LOW UPP))) 'FAILED)))
        (RETURN (SIMP X)))
       ((AND (NOT (SMEMQ 'INFINITY LOW)) (NOT (SMEMQ 'INFINITY UPP)))
        (PROGN
         (SETQ X (PREPSQ* (SIMPINT (LIST FN VAR))))
         (COND
          ((NOT (EQCAR X 'INT))
           (RETURN
            (SIMP*
             (LIST 'DIFFERENCE (SUBEVAL (LIST (LIST 'EQUAL VAR UPP) X))
                   (SUBEVAL (LIST (LIST 'EQUAL VAR LOW) X))))))))))
      (RETURN (MKSQ (LIST 'INT FN VAR LOW UPP) 1)))) 
(PUT 'MKINFINT 'NUMBER-OF-ARGS 2) 
(PUT 'MKINFINT 'DEFINED-ON-LINE '146) 
(PUT 'MKINFINT 'DEFINED-IN-FILE 'INT/DINT.RED) 
(PUT 'MKINFINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKINFINT (FN VAR)
    (PROG (X Y)
      (COND
       ((AND (GETD 'DEFINT0)
             (NOT
              (EQ (SETQ X (DEFINT0 (LIST FN VAR '(MINUS INFINITY) 'INFINITY)))
                  'FAILED)))
        (RETURN (SIMP X))))
      (SETQ X (SIMPDINT (LIST FN VAR 0 'INFINITY)))
      (SETQ Y (SIMPDINT (LIST FN VAR '(MINUS INFINITY) 0)))
      (COND
       ((AND (KERNP X) (EQCAR (CAAAR (CAR X)) 'INT) (KERNP Y)
             (EQCAR (CAAAR (CAR Y)) 'INT))
        (RETURN (MKDINT FN VAR '(MINUS INFINITY) 'INFINITY)))
       (T (RETURN (ADDSQ X Y)))))) 
(ENDMODULE) 