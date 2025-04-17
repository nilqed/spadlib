(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATARG)) 
(PUT 'MKMATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'MKMATRIX 'DEFINED-ON-LINE '36) 
(PUT 'MKMATRIX 'DEFINED-IN-FILE 'NORMFORM/MATARG.RED) 
(PUT 'MKMATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKMATRIX (N M)
    (CONS 'MAT
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I 1)
            (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J 1)
                               (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                              LOOPLABEL
                               (SETQ J (PLUS2 J 1))
                               (COND
                                ((MINUSP (DIFFERENCE M J))
                                 (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             NIL)))
           LOOPLABEL
            (SETQ I (PLUS2 I 1))
            (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J 1)
                       (COND ((MINUSP (DIFFERENCE M J)) (RETURN NIL)))
                       (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                      LOOPLABEL
                       (SETQ J (PLUS2 J 1))
                       (COND
                        ((MINUSP (DIFFERENCE M J)) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'SETMAT 'NUMBER-OF-ARGS 4) 
(PUT 'SETMAT 'DEFINED-ON-LINE '40) 
(PUT 'SETMAT 'DEFINED-IN-FILE 'NORMFORM/MATARG.RED) 
(PUT 'SETMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETMAT (MATRI I J VAL)
    (PROGN
     (COND
      (*MODULAR (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
     (SETQ I (REVAL1 I T))
     (SETQ J (REVAL1 J T))
     (SETQ VAL (MK*SQ (SIMP (REVAL1 VAL T))))
     (LETMTR (LIST MATRI I J) VAL MATRI)
     (COND
      (*MOD_WAS_ON (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
     MATRI)) 
(PUT 'LETMTR 'NUMBER-OF-ARGS 3) 
(PUT 'LETMTR 'DEFINED-ON-LINE '49) 
(PUT 'LETMTR 'DEFINED-IN-FILE 'NORMFORM/MATARG.RED) 
(PUT 'LETMTR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LETMTR (U V Y)
    (PROG (Z)
      (COND
       ((NOT (EQCAR Y 'MAT))
        (RERROR 'MATRIX 10 (LIST "Matrix" (CAR U) "not set")))
       ((OR (NOT (NUMLIS (SETQ Z (REVLIS (CDR U))))) (NEQ (LENGTH Z) 2))
        (RETURN (ERRPRI2 U 'HOLD))))
      (RPLACA (PNTH (NTH (CDR Y) (CAR Z)) (CADR Z)) V))) 
(PUT 'GETMAT 'NUMBER-OF-ARGS 3) 
(PUT 'GETMAT 'DEFINED-ON-LINE '59) 
(PUT 'GETMAT 'DEFINED-IN-FILE 'NORMFORM/MATARG.RED) 
(PUT 'GETMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETMAT (MATRI I J)
    (PROGN
     (SETQ I (OFF_MOD_REVAL I))
     (SETQ J (OFF_MOD_REVAL J))
     (UNCHECKED_GETMATELEM (LIST MATRI I J)))) 
(PUT 'UNCHECKED_GETMATELEM 'NUMBER-OF-ARGS 1) 
(PUT 'UNCHECKED_GETMATELEM 'DEFINED-ON-LINE '64) 
(PUT 'UNCHECKED_GETMATELEM 'DEFINED-IN-FILE 'NORMFORM/MATARG.RED) 
(PUT 'UNCHECKED_GETMATELEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNCHECKED_GETMATELEM (U)
    (PROG (X)
      (COND
       ((NOT (EQCAR (SETQ X (CAR U)) 'MAT))
        (RERROR 'MATRIX 1 (LIST "Matrix" (CAR U) "not set")))
       (T (RETURN (NTH (NTH (CDR X) (CADR U)) (CADDR U))))))) 
(FLAG '(SETMAT GETMAT MKMATRIX LETMTR) 'OPFN) 
(ENDMODULE) 