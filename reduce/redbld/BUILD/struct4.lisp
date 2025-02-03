(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'STRUCTURE)) 
(PUT 'N_FORMSTRUCTURE 'NUMBER-OF-ARGS 3) 
(PUT 'N_FORMSTRUCTURE 'DEFINED-ON-LINE '33) 
(PUT 'N_FORMSTRUCTURE 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'N_FORMSTRUCTURE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_FORMSTRUCTURE (U VARS TYPE)
    (PROG (X)
      (SETQ U (CADR U))
      (SETQ X
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR U))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (N_FORMSTRUCTURE1 X VARS TYPE))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (N_FORMSTRUCTURE1 X VARS TYPE)) (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MKOBJECT (N_STRUCTUREFN X TYPE) 'NOVAL)))) 
(PUT 'N_FORMSTRUCTURE1 'NUMBER-OF-ARGS 3) 
(PUT 'N_FORMSTRUCTURE1 'DEFINED-ON-LINE '40) 
(PUT 'N_FORMSTRUCTURE1 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'N_FORMSTRUCTURE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_FORMSTRUCTURE1 (U VARS TYPE)
    (PROG (X)
      (COND
       ((NOT (IDP (CAR U)))
        (TYPERR (CAR U)
                (COMPRESS (APPEND (EXPLODE TYPE) (EXPLODE2 "! name"))))))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (CDR U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (LISPEVAL (CADR (N_FORM1 J VARS))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (LISPEVAL (CADR (N_FORM1 J VARS))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((COERCABLE (TYPE J) 'INT) (VALUE J))
                                          (T (TYPERR (VALUE J) 'INT))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND ((COERCABLE (TYPE J) 'INT) (VALUE J))
                                  (T (TYPERR (VALUE J) 'INT))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS (CAR U) X)))) 
(PUT 'N_STRUCTUREFN 'NUMBER-OF-ARGS 2) 
(PUT 'N_STRUCTUREFN 'DEFINED-ON-LINE '53) 
(PUT 'N_STRUCTUREFN 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'N_STRUCTUREFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_STRUCTUREFN (U TYPE)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X) (N_STRUCTUREFN1 X TYPE)) (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (MKQUOTE (MKNOVALOBJ)))) 
(PUT 'N_STRUCTUREFN1 'NUMBER-OF-ARGS 2) 
(PUT 'N_STRUCTUREFN1 'DEFINED-ON-LINE '56) 
(PUT 'N_STRUCTUREFN1 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'N_STRUCTUREFN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_STRUCTUREFN1 (U TYPE)
    (PROG (Y)
      (COND
       ((FLAGP TYPE 'ZEROELEMENTP)
        (SETQ Y
                (ADD1LIS
                 (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Z (CDR U))
                   (COND ((NULL Z) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (Z) (LISPEVAL Z)) (CAR Z))
                                         NIL)))
                  LOOPLABEL
                   (SETQ Z (CDR Z))
                   (COND ((NULL Z) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (Z) (LISPEVAL Z)) (CAR Z)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))
       (T
        (SETQ Y
                (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Z (CDR U))
                  (COND ((NULL Z) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Z) (LISPEVAL Z)) (CAR Z))
                                        NIL)))
                 LOOPLABEL
                  (SETQ Z (CDR Z))
                  (COND ((NULL Z) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Z) (LISPEVAL Z)) (CAR Z)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (PUTOBJECT (CAR U) (MKN_STRUCTURE Y TYPE) TYPE)
      (PUT (CAR U) 'DIMENSION Y))) 
(PUT 'MKN_STRUCTURE 'NUMBER-OF-ARGS 2) 
(PUT 'MKN_STRUCTURE 'DEFINED-ON-LINE '65) 
(PUT 'MKN_STRUCTURE 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'MKN_STRUCTURE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKN_STRUCTURE (U TYPE)
    (COND ((NULL U) (MKOBJECT 0 'ZERO))
          (T
           (PROG (N X)
             (SETQ N 0)
             (SETQ N (DIFFERENCE (CAR U) 1))
             (SETQ X (MKVECT N))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
               (PUTV X I (MKN_STRUCTURE (CDR U) TYPE))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN X))))) 
(PUT 'GETELL 'NUMBER-OF-ARGS 2) 
(PUT 'GETELL 'DEFINED-ON-LINE '77) 
(PUT 'GETELL 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'GETELL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETELL (U V)
    (GETELL1 (VALUE U)
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X V)
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (VALUE X)) (CAR X)) NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (VALUE X)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'GETELL1 'NUMBER-OF-ARGS 2) 
(PUT 'GETELL1 'DEFINED-ON-LINE '80) 
(PUT 'GETELL1 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'GETELL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETELL1 (U V) (COND ((NULL V) U) (T (GETELL1 (GETV U (CAR V)) (CDR V))))) 
(PUT 'SETELL 'NUMBER-OF-ARGS 3) 
(PUT 'SETELL 'DEFINED-ON-LINE '83) 
(PUT 'SETELL 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'SETELL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETELL (U V W) (SETELL1 (VALUE U) V W)) 
(PUT 'SETELL1 'NUMBER-OF-ARGS 3) 
(PUT 'SETELL1 'DEFINED-ON-LINE '86) 
(PUT 'SETELL1 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'SETELL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SETELL1 (U V W)
    (COND ((NULL V) (REDERR "Structure confusion"))
          ((NULL (CDR V)) (PUTV U (INT_CHECK (CAR V)) W))
          (T (SETELL1 (GETV U (INT_CHECK (CAR V))) (CDR V) W)))) 
(PUT 'INT_CHECK 'NUMBER-OF-ARGS 1) 
(PUT 'INT_CHECK 'DEFINED-ON-LINE '91) 
(PUT 'INT_CHECK 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'INT_CHECK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT_CHECK (U)
    (COND ((COERCABLE (TYPE U) 'INT) (VALUE U)) (T (TYPERR (VALUE U) 'INT)))) 
(FLAG '(ARRAY) 'ZEROELEMENTP) 
(PUT 'ARRAY 'N_FORMFN 'N_FORMARRAY) 
(PUT 'N_FORMARRAY 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMARRAY 'DEFINED-ON-LINE '100) 
(PUT 'N_FORMARRAY 'DEFINED-IN-FILE 'REDUCE4/STRUCT4.RED) 
(PUT 'N_FORMARRAY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMARRAY (U VARS) (N_FORMSTRUCTURE U VARS 'ARRAY)) 
(PUT 'ARRAY 'GETFN 'GETELL) 
(PUT 'ARRAY 'PUTFN 'SETELL) 
(ENDMODULE) 