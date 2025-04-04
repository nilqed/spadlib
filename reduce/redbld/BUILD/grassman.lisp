(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GRASSMAN)) 
(PUT 'PUTGRASS 'NUMBER-OF-ARGS 1) 
(PUT 'PUTGRASS 'DEFINED-ON-LINE '32) 
(PUT 'PUTGRASS 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'PUTGRASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PUTGRASS (U)
    (PROG (I)
      (SETQ I U)
     LAB
      (COND ((NULL I) (RETURN NIL)))
      ((LAMBDA (I)
         (COND ((NOT (IDP I)) (TYPERR I "grassman variable"))
               (T
                (PROGN
                 (FLAG (LIST I) 'NONCOM)
                 (PUT I 'SIMPFN 'SIMPIDEN)
                 (FLAG (LIST I) 'GRASSMAN)
                 NIL))))
       (CAR I))
      (SETQ I (CDR I))
      (GO LAB))) 
(RLISTAT '(PUTGRASS REMGRASS)) 
(PUT 'REMGRASS 'NUMBER-OF-ARGS 1) 
(PUT 'REMGRASS 'DEFINED-ON-LINE '41) 
(PUT 'REMGRASS 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'REMGRASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMGRASS (U)
    (PROG (I)
      (SETQ I U)
     LAB
      (COND ((NULL I) (RETURN NIL)))
      ((LAMBDA (I)
         (COND
          ((FLAGP I 'GRASSMAN)
           (PROGN
            (REMFLAG (LIST I) 'GRASSMAN)
            (REMFLAG (LIST I) 'NONCOM)
            (CLEAROP I)
            NIL))))
       (CAR I))
      (SETQ I (CDR I))
      (GO LAB))) 
(PUT 'GRASSP 'NUMBER-OF-ARGS 1) 
(PUT 'GRASSP 'DEFINED-ON-LINE '48) 
(PUT 'GRASSP 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'GRASSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRASSP (U) (AND (NOT (ATOM U)) (FLAGP (CAR U) 'GRASSMAN))) 
(FLAG '(GRASSP) 'BOOLEAN) 
(PUT 'GRASSPARITYINI 'NUMBER-OF-ARGS 1) 
(PUT 'GRASSPARITYINI 'DEFINED-ON-LINE '53) 
(PUT 'GRASSPARITYINI 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'GRASSPARITYINI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRASSPARITYINI (U) (COND ((GRASSP U) 1) (T 0))) 
(PUT 'GRASSPARITY 'NUMBER-OF-ARGS 1) 
(PUT 'GRASSPARITY 'DEFINED-ON-LINE '57) 
(PUT 'GRASSPARITY 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'GRASSPARITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRASSPARITY (U)
    (COND ((ATOM U) 0) ((FLAGP (CAR U) 'GRASSMAN) 1)
          ((EQ (CAR U) 'PLUS) "parity undefined")
          ((EQ (CAR U) 'MINUS) (GRASSPARITY (CADR U)))
          ((EQ (CAR U) 'TIMES)
           (REMAINDER
            (PROG (I FORALL-RESULT)
              (SETQ I (CDR U))
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND ((NULL I) (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (PLUS ((LAMBDA (I) (GRASSPARITY I)) (CAR I))
                            FORALL-RESULT))
              (SETQ I (CDR I))
              (GO LAB1))
            2))
          ((EQ (CAR U) 'EXPT)
           (COND ((ODDP (CADDR U)) (GRASSPARITY (CADR U))) (T 0)))
          ((EQ (CAR U) 'QUOTIENT) (GRASSPARITY (CADR U))) (T 0))) 
(FLAG '(GRASSPARITY GHOSTFACTOR) 'OPFN) 
(PUT 'GHOSTFACTOR 'NUMBER-OF-ARGS 2) 
(PUT 'GHOSTFACTOR 'DEFINED-ON-LINE '71) 
(PUT 'GHOSTFACTOR 'DEFINED-IN-FILE 'ASSIST/GRASSMAN.RED) 
(PUT 'GHOSTFACTOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GHOSTFACTOR (U V)
    (COND
     ((EQUAL (REVAL1 (LIST 'TIMES (GRASSPARITY U) (GRASSPARITY V)) T) 0) 1)
     (T (MINUS 1)))) 
(ENDMODULE) 