(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LMATRIX)) 
(SWITCH (LIST 'MOD_WAS_ON)) 
(PUT 'MKMATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'MKMATRIX 'DEFINED-ON-LINE '40) 
(PUT 'MKMATRIX 'DEFINED-IN-FILE 'LINALG/LAMATRIX.RED) 
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
(PUT 'SETMAT 'DEFINED-ON-LINE '49) 
(PUT 'SETMAT 'DEFINED-IN-FILE 'LINALG/LAMATRIX.RED) 
(PUT 'SETMAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETMAT (MATRI I J VAL)
    (PROGN
     (COND
      (*MODULAR (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
     (SETQ I (COND ((FIXP I) I) (T (REVAL1 I T))))
     (SETQ J (COND ((FIXP J) J) (T (REVAL1 J T))))
     (MY_LETMTR (LIST MATRI I J) VAL MATRI)
     (COND
      (*MOD_WAS_ON (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
     MATRI)) 
(PUT 'GETMAT 'NUMBER-OF-ARGS 3) 
(PUT 'GETMAT 'DEFINED-ON-LINE '62) 
(PUT 'GETMAT 'DEFINED-IN-FILE 'LINALG/LAMATRIX.RED) 
(PUT 'GETMAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GETMAT (MATRI I J)
    (PROGN
     (COND
      (*MODULAR (PROGN (OFF (LIST 'MODULAR)) (ON (LIST 'MOD_WAS_ON)) NIL)))
     (SETQ I (COND ((FIXP I) I) (T (REVAL1 I T))))
     (SETQ J (COND ((FIXP J) J) (T (REVAL1 J T))))
     (COND
      (*MOD_WAS_ON (PROGN (ON (LIST 'MODULAR)) (OFF (LIST 'MOD_WAS_ON)) NIL)))
     (UNCHECKED_GETMATELEM (LIST MATRI I J)))) 
(PUT 'UNCHECKED_GETMATELEM 'NUMBER-OF-ARGS 1) 
(PUT 'UNCHECKED_GETMATELEM 'DEFINED-ON-LINE '74) 
(PUT 'UNCHECKED_GETMATELEM 'DEFINED-IN-FILE 'LINALG/LAMATRIX.RED) 
(PUT 'UNCHECKED_GETMATELEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNCHECKED_GETMATELEM (U)
    (PROG (X)
      (COND
       ((NOT (EQCAR (SETQ X (CAR U)) 'MAT))
        (RERROR 'MATRIX 1 (LIST "Matrix" (CAR U) "not set")))
       (T (RETURN (NTH (NTH (CDR X) (CADR U)) (CADDR U))))))) 
(PUT 'MY_LETMTR 'NUMBER-OF-ARGS 3) 
(PUT 'MY_LETMTR 'DEFINED-ON-LINE '83) 
(PUT 'MY_LETMTR 'DEFINED-IN-FILE 'LINALG/LAMATRIX.RED) 
(PUT 'MY_LETMTR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MY_LETMTR (U V Y)
    (PROG (Z)
      (COND
       ((NOT (EQCAR Y 'MAT))
        (RERROR 'MATRIX 10 (LIST "Matrix" (CAR U) "not set")))
       ((OR
         (NOT
          (NUMLIS
           (SETQ Z
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J (CDR U))
                     (COND ((NULL J) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (J)
                                         (COND ((FIXP J) J) (T (REVAL1 J T))))
                                       (CAR J))
                                      NIL)))
                    LOOPLABEL
                     (SETQ J (CDR J))
                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (J)
                                 (COND ((FIXP J) J) (T (REVAL1 J T))))
                               (CAR J))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))
         (NEQ (LENGTH Z) 2))
        (RETURN (ERRPRI2 U 'HOLD))))
      (RPLACA (PNTH (NTH (CDR Y) (CAR Z)) (CADR Z)) V))) 
(ENDMODULE) 