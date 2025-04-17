(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NUMFIT)) 
(FLUID '(*NOEQUIV ACCURACY* SINGULARITIES*)) 
(GLOBAL '(ITERATIONS *TRNUMERIC NUMFIT_COUNT*)) 
(SETQ NUMFIT_COUNT* 0) 
(PUT 'NUMFIT_GENSYM 'NUMBER-OF-ARGS 0) 
(PUT 'NUMFIT_GENSYM 'DEFINED-ON-LINE '37) 
(PUT 'NUMFIT_GENSYM 'DEFINED-IN-FILE 'NUMERIC/NUMFIT.RED) 
(PUT 'NUMFIT_GENSYM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NUMFIT_GENSYM NIL
    (COMPRESS
     (APPEND (EXPLODE '|NUMFIT_GENSYM:|)
             (EXPLODE (SETQ NUMFIT_COUNT* (PLUS NUMFIT_COUNT* 1)))))) 
(PUT 'FITEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'FITEVAL 'DEFINED-ON-LINE '41) 
(PUT 'FITEVAL 'DEFINED-IN-FILE 'NUMERIC/NUMFIT.RED) 
(PUT 'FITEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FITEVAL (U)
    (PROG (A B E R L Q X V VAR PARS FCN FL BASIS PTS GRAD OLDMODE *NOEQUIV N I)
      (SETQ N 0)
      (SETQ I 0)
      (COND ((NOT (EQUAL (LENGTH U) 3)) (GO SYNERR)))
      (SETQ U
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X U)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                      NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (ACCURACYCONTROL U 6 200))
      (SETQ FCN (CAR U))
      (SETQ U (CDR U))
      (COND ((EQCAR FCN 'LIST) (SETQ FL (CDR FCN))))
      (SETQ BASIS (CAR U))
      (SETQ U (CDR U))
      (COND ((NOT (EQCAR BASIS 'LIST)) (GO SYNERR)))
      (SETQ BASIS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR BASIS))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (SIMP (REVAL1 X T))) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VAR (CAR U))
      (SETQ U (CDR U))
      (COND ((NOT (EQCAR VAR 'EQUAL)) (GO SYNERR)))
      (COND ((NOT (EQCAR (SETQ PTS (CADDR VAR)) 'LIST)) (GO SYNERR)))
      (SETQ VAR (CADR VAR))
      (SETQ PTS (CDR PTS))
      (SETQ N (LENGTH PTS))
      (COND (*TRNUMERIC (PRIN2T "build generic approximation function")))
      (SETQ A (CONS NIL 1))
      (PROG (B)
        (SETQ B BASIS)
       LAB
        (COND ((NULL B) (RETURN NIL)))
        ((LAMBDA (B)
           (PROGN
            (SETQ V (NUMFIT_GENSYM))
            (SETQ PARS (CONS V PARS))
            (SETQ A (ADDSQ (MULTSQ (SIMP V) B) A))))
         (CAR B))
        (SETQ B (CDR B))
        (GO LAB))
      (SETQ OLDMODE (SWITCH-MODE-RD NIL))
      (SETQ *NOEQUIV NIL)
      (SETQ B (SETQ A (SIMP (PREPSQ A))))
      (SETQ FCN (SIMP (COND ((NULL FL) FCN) (T 'DUMMY))))
      (SETQ E (CONS NIL 1))
      (PROG (P)
        (SETQ P PTS)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ I (PLUS I 1))
            (SETQ L (LIST (CONS VAR P)))
            (COND (FL (SETQ L (CONS (CONS 'DUMMY (REVAL1 (NTH FL I) T)) L))))
            (SETQ Q (ADDSQ (SUBSQ FCN L) (NEGSQ (SUBSQ B L))))
            (SETQ E (ADDSQ E (MULTSQ Q Q)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ E (PREPSQ E))
      (COND
       (*TRNUMERIC
        (PROGN (LPRIM "error function is:") (WRITEPRI (MKQUOTE E) 'ONLY))))
      (SETQ GRAD
              (CONS 'LIST
                    (PROG (V FORALL-RESULT FORALL-ENDPTR)
                      (SETQ V PARS)
                      (COND ((NULL V) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (V) (REVAL1 (LIST 'DF E V) T))
                                        (CAR V))
                                       NIL)))
                     LOOPLABEL
                      (SETQ V (CDR V))
                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (V) (REVAL1 (LIST 'DF E V) T)) (CAR V))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (SETQ R
              (RDSOLVEEVAL
               (LIST GRAD
                     (CONS 'LIST
                           (PROG (P FORALL-RESULT FORALL-ENDPTR)
                             (SETQ P PARS)
                             (COND ((NULL P) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (P) (LIST 'EQUAL P 0))
                                               (CAR P))
                                              NIL)))
                            LOOPLABEL
                             (SETQ P (CDR P))
                             (COND ((NULL P) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (P) (LIST 'EQUAL P 0)) (CAR P))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                     (LIST 'EQUAL 'ACCURACY ACCURACY*)
                     (LIST 'EQUAL 'ITERATIONS ITERATIONS*))))
      (COND (*TRNUMERIC (LPRIM "resubstituting in approximating form")))
      (SETQ L NIL)
      (SETQ PARS NIL)
      (PROG (P FORALL-RESULT FORALL-ENDPTR)
        (SETQ P (CDR R))
        (COND ((NULL P) (RETURN NIL)))
        (SETQ FORALL-RESULT
                (SETQ FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ X (CADDR P))
                             (SETQ PARS (CONS X PARS))
                             (SETQ L (CONS (CONS (CADR P) X) L))))
                          (CAR P))
                         NIL)))
       LOOPLABEL
        (SETQ P (CDR P))
        (COND ((NULL P) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                (CONS
                 ((LAMBDA (P)
                    (PROGN
                     (SETQ X (CADDR P))
                     (SETQ PARS (CONS X PARS))
                     (SETQ L (CONS (CONS (CADR P) X) L))))
                  (CAR P))
                 NIL))
        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
        (GO LOOPLABEL))
      (SETQ A (PREPSQ (SUBSQ A L)))
      (SWITCH-MODE-RD OLDMODE)
      (RETURN (LIST 'LIST A (CONS 'LIST PARS)))
     SYNERR
      (REDERR "illegal parameters in fit"))) 
(PUT 'NUM_FIT 'PSOPFN 'FITEVAL) 
(ENDMODULE) 