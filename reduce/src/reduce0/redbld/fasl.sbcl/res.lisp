(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RES)) 
(PUT 'RESOLVE* 'NUMBER-OF-ARGS 2) 
(PUT 'RESOLVE* 'DEFINED-ON-LINE '57) 
(PUT 'RESOLVE* 'DEFINED-IN-FILE 'CALI/RES.RED) 
(PUT 'RESOLVE* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RESOLVE* (M D)
    ((LAMBDA (CALI=DEGREES)
       (PROG (A U)
         (COND
          ((EQUAL (DPMAT_COLS M) 0)
           (PROGN (SETQ CALI=DEGREES NIL) (SETQ M (IDEAL2MAT* M))))
          (T (SETQ CALI=DEGREES (DPMAT_COLDEGS M))))
         (SETQ A (LIST M))
         (SETQ U (SYZYGIES* M))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND (NOT (DPMAT_ZERO? U)) (GREATERP D 1))) (RETURN NIL)))
           (PROGN
            (SETQ M U)
            (SETQ U (SYZYGIES* M))
            (SETQ D (DIFFERENCE D 1))
            (SETQ U (GROEB_MINIMIZE M U))
            (SETQ M (CAR U))
            (SETQ U (CDR U))
            (SETQ A (CONS M A))
            NIL)
           (GO WHILELABEL))
         (RETURN (REVERSIP (CONS U A)))))
     CALI=DEGREES)) 
(PUT 'BETTINUMBERS* 'NUMBER-OF-ARGS 1) 
(PUT 'BETTINUMBERS* 'DEFINED-ON-LINE '75) 
(PUT 'BETTINUMBERS* 'DEFINED-IN-FILE 'CALI/RES.RED) 
(PUT 'BETTINUMBERS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BETTINUMBERS* (C)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X C)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (DPMAT_COLS X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (DPMAT_COLS X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'GRADEDBETTINUMBERS* 'NUMBER-OF-ARGS 1) 
(PUT 'GRADEDBETTINUMBERS* 'DEFINED-ON-LINE '79) 
(PUT 'GRADEDBETTINUMBERS* 'DEFINED-IN-FILE 'CALI/RES.RED) 
(PUT 'GRADEDBETTINUMBERS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRADEDBETTINUMBERS* (C)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X C)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (PROG (I D)
                            (SETQ D (DPMAT_COLDEGS X))
                            (RETURN
                             (COND
                              (D
                               (SORT
                                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ Y D)
                                  (COND ((NULL Y) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (Y)
                                                      (MO_ECART (CDR Y)))
                                                    (CAR Y))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ Y (CDR Y))
                                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (Y) (MO_ECART (CDR Y)))
                                            (CAR Y))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))
                                'LEQ))
                              (T
                               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ I 1)
                                 (COND
                                  ((MINUSP (DIFFERENCE (DPMAT_COLS X) I))
                                   (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                                LOOPLABEL
                                 (SETQ I (PLUS2 I 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE (DPMAT_COLS X) I))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (PROG (I D)
                    (SETQ D (DPMAT_COLDEGS X))
                    (RETURN
                     (COND
                      (D
                       (SORT
                        (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                          (SETQ Y D)
                          (COND ((NULL Y) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (Y) (MO_ECART (CDR Y)))
                                            (CAR Y))
                                           NIL)))
                         LOOPLABEL
                          (SETQ Y (CDR Y))
                          (COND ((NULL Y) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (Y) (MO_ECART (CDR Y))) (CAR Y))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))
                        'LEQ))
                      (T
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND
                          ((MINUSP (DIFFERENCE (DPMAT_COLS X) I))
                           (RETURN NIL)))
                         (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS 0 NIL)))
                        LOOPLABEL
                         (SETQ I (PLUS2 I 1))
                         (COND
                          ((MINUSP (DIFFERENCE (DPMAT_COLS X) I))
                           (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR (CONS 0 NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(ENDMODULE) 