(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GUARDIANPRINT)) 
(FLUID '(GD_OMODE*)) 
(SETQ GD_OMODE* 'MATRIX) 
(FLAG '(GEX) 'SPRIFN) 
(PUT 'GEX 'TAG 'GE) 
(PUT 'GE 'SETPRIFN 'GD_SETGEPRI) 
(PUT 'GE 'PRIFN 'GD_GEPRI) 
(PUT 'GE 'FANCY-SETPRIFN 'GD_FANCY-SETGEPRI) 
(PUT 'GE 'FANCY-PRIFN 'GD_FANCY-GEPRI) 
(PUT 'GDOMODE 'PSOPFN 'GD_OMODE) 
(PUT 'GD_OMODE 'NUMBER-OF-ARGS 1) 
(PUT 'GD_OMODE 'DEFINED-ON-LINE '46) 
(PUT 'GD_OMODE 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_OMODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_OMODE (ARGL)
    (PROG (W) (SETQ W GD_OMODE*) (SETQ GD_OMODE* (CAR ARGL)) (RETURN W))) 
(PUT 'GD_SETGEPRI 'NUMBER-OF-ARGS 2) 
(PUT 'GD_SETGEPRI 'DEFINED-ON-LINE '53) 
(PUT 'GD_SETGEPRI 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_SETGEPRI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_SETGEPRI (V U)
    ((LAMBDA (*GUARDIAN)
       (APPLY
        (INTERN (COMPRESS (APPEND (EXPLODE 'GD_SETGEPRI) (EXPLODE GD_OMODE*))))
        (LIST V U)))
     NIL)) 
(PUT 'GD_GEPRI 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GEPRI 'DEFINED-ON-LINE '57) 
(PUT 'GD_GEPRI 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_GEPRI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GEPRI (U)
    ((LAMBDA (*GUARDIAN)
       (APPLY
        (INTERN (COMPRESS (APPEND (EXPLODE 'GD_GEPRI) (EXPLODE GD_OMODE*))))
        (LIST U)))
     NIL)) 
(PUT 'GD_SETGEPRIDEBUG 'NUMBER-OF-ARGS 2) 
(PUT 'GD_SETGEPRIDEBUG 'DEFINED-ON-LINE '61) 
(PUT 'GD_SETGEPRIDEBUG 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_SETGEPRIDEBUG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_SETGEPRIDEBUG (V U) (COND ((CDR U) (SETMATPRI V U)))) 
(PUT 'GD_GEPRIDEBUG 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GEPRIDEBUG 'DEFINED-ON-LINE '64) 
(PUT 'GD_GEPRIDEBUG 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_GEPRIDEBUG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GEPRIDEBUG (U) (COND ((CDR U) (MATPRI U)))) 
(PUT 'GD_SETGEPRIMATRIX 'NUMBER-OF-ARGS 2) 
(PUT 'GD_SETGEPRIMATRIX 'DEFINED-ON-LINE '67) 
(PUT 'GD_SETGEPRIMATRIX 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_SETGEPRIMATRIX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_SETGEPRIMATRIX (V U)
    (GD_SETGEPRIDEBUG V
     (CONS 'GE
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X (CDR U))
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GD_GEPRIMATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GEPRIMATRIX 'DEFINED-ON-LINE '70) 
(PUT 'GD_GEPRIMATRIX 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_GEPRIMATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GEPRIMATRIX (U)
    (GD_GEPRIDEBUG
     (CONS 'GE
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X (CDR U))
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (CDR X)) (CAR X)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'GD_SETGEPRIGCASE 'NUMBER-OF-ARGS 2) 
(PUT 'GD_SETGEPRIGCASE 'DEFINED-ON-LINE '73) 
(PUT 'GD_SETGEPRIGCASE 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_SETGEPRIGCASE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_SETGEPRIGCASE (V U) (GD_SETGEPRIDEBUG V (LIST 'GE (CDAR (CDR U))))) 
(PUT 'GD_GEPRIGCASE 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GEPRIGCASE 'DEFINED-ON-LINE '76) 
(PUT 'GD_GEPRIGCASE 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_GEPRIGCASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GEPRIGCASE (U) (GD_GEPRIDEBUG (LIST 'GE (CDAR (CDR U))))) 
(PUT 'GD_SETGEPRIGTERM 'NUMBER-OF-ARGS 2) 
(PUT 'GD_SETGEPRIGTERM 'DEFINED-ON-LINE '79) 
(PUT 'GD_SETGEPRIGTERM 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_SETGEPRIGTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GD_SETGEPRIGTERM (V U)
    (PROGN
     (COND
      ((EQUAL (CADR (CAR (CDR U))) 'FALSE) (LPRIM "contradictive situation")))
     (ASSGNPRI (MK*SQ (SIMP (CADDR (CAR (CDR U))))) (LIST V) 'ONLY))) 
(PUT 'GD_GEPRIGTERM 'NUMBER-OF-ARGS 1) 
(PUT 'GD_GEPRIGTERM 'DEFINED-ON-LINE '86) 
(PUT 'GD_GEPRIGTERM 'DEFINED-IN-FILE 'GUARDIAN/GUARDIANPRINT.RED) 
(PUT 'GD_GEPRIGTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GD_GEPRIGTERM (U)
    (PROGN
     (COND
      ((EQUAL (CADR (CAR (CDR U))) 'FALSE) (LPRIM "contradictive situation")))
     (ASSGNPRI (MK*SQ (SIMP (CADDR (CAR (CDR U))))) NIL 'ONLY))) 
(ENDMODULE) 