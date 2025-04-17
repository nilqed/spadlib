(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'WEIGHT)) 
(FLUID '(ASYMPLIS* WTL*)) 
(FLAG '(K*) 'RESERVED) 
(PUT 'WEIGHT 'NUMBER-OF-ARGS 1) 
(PUT 'WEIGHT 'DEFINED-ON-LINE '41) 
(PUT 'WEIGHT 'DEFINED-IN-FILE 'ALG/WEIGHT.RED) 
(PUT 'WEIGHT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WEIGHT (U)
    (CONS 'LIST
          (COND
           ((NULL (CAR U))
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X WTL*)
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X) (LIST 'EQUAL (CAR X) (CDR X)))
                                (CAR X))
                               NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X) (LIST 'EQUAL (CAR X) (CDR X))) (CAR X))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
           (T
            (PROGN
             (COND
              ((NULL (ATSOC 'K* ASYMPLIS*))
               (SETQ ASYMPLIS* (CONS '(K* . 2) ASYMPLIS*))))
             (RMSUBS)
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X U)
              STARTOVER
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (X)
                          (PROG (Y Z)
                            (COND
                             ((EQEXPR X)
                              (PROGN
                               (SETQ Z (REVAL1 (CADDR X) T))
                               (COND
                                ((OR (NOT (FIXP Z)) (LEQ Z 0))
                                 (TYPERR Z "weight")))
                               (SETQ X (CADR X)))))
                            (SETQ Y (*A2KWOWEIGHT X))
                            (SETQ X
                                    (COND
                                     ((SETQ X (ATSOC Y WTL*))
                                      (LIST (LIST 'EQUAL (CAR X) (CDR X))))))
                            (COND
                             (Z (SETQ WTL* (CONS (CONS Y Z) (DELASC Y WTL*)))))
                            (RETURN X)))
                        (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ X (CDR X))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (X)
                          (PROG (Y Z)
                            (COND
                             ((EQEXPR X)
                              (PROGN
                               (SETQ Z (REVAL1 (CADDR X) T))
                               (COND
                                ((OR (NOT (FIXP Z)) (LEQ Z 0))
                                 (TYPERR Z "weight")))
                               (SETQ X (CADR X)))))
                            (SETQ Y (*A2KWOWEIGHT X))
                            (SETQ X
                                    (COND
                                     ((SETQ X (ATSOC Y WTL*))
                                      (LIST (LIST 'EQUAL (CAR X) (CDR X))))))
                            (COND
                             (Z (SETQ WTL* (CONS (CONS Y Z) (DELASC Y WTL*)))))
                            (RETURN X)))
                        (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ X (CDR X))
               (GO LOOPLABEL))))))) 
(PUT 'WTLEVEL 'NUMBER-OF-ARGS 1) 
(PUT 'WTLEVEL 'DEFINED-ON-LINE '69) 
(PUT 'WTLEVEL 'DEFINED-IN-FILE 'ALG/WEIGHT.RED) 
(PUT 'WTLEVEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WTLEVEL (N)
    (PROG (OLDN)
      ((LAMBDA (X) (SETQ OLDN (COND (X (DIFFERENCE (CDR X) 1)) (T 1))))
       (ATSOC 'K* ASYMPLIS*))
      (COND
       ((CAR N)
        (PROGN
         (SETQ N (REVAL1 (CAR N) T))
         (COND ((OR (NOT (FIXP N)) (LESSP N 0)) (TYPERR N "weight level")))
         (COND ((LESSP N OLDN) (RMSUBS)))
         (COND
          ((NEQ N OLDN)
           (SETQ ASYMPLIS*
                   (CONS (CONS 'K* (PLUS N 1)) (DELASC 'K* ASYMPLIS*))))))))
      (RETURN OLDN))) 
(RLISTAT '(WEIGHT WTLEVEL)) 
(FLAG '(WEIGHT WTLEVEL) 'NOCHANGE) 
(ENDMODULE) 