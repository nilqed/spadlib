(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FORSTAT4)) 
(PUT 'FOR 'N_FORMFN 'N_FORMFOR) 
(FLAG '(GO) 'NON_FORM) 
(PUT 'TOP_TYPE 'NUMBER-OF-ARGS 1) 
(PUT 'TOP_TYPE 'DEFINED-ON-LINE '36) 
(PUT 'TOP_TYPE 'DEFINED-IN-FILE 'REDUCE4/FORSTAT4.RED) 
(PUT 'TOP_TYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TOP_TYPE (U)
    (PROG (V W)
      (SETQ V (TYPE (CAR U)))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN V)))
      (SETQ W (TYPE (CAR U)))
      (COND ((XTYPE1 W V) (GO A)) ((XTYPE1 V W) (PROGN (SETQ V W) (GO A)))
            (T (REDERR "ugh"))))) 
(PUT 'N_FORMFOR 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMFOR 'DEFINED-ON-LINE '48) 
(PUT 'N_FORMFOR 'DEFINED-IN-FILE 'REDUCE4/FORSTAT4.RED) 
(PUT 'N_FORMFOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMFOR (U VARS)
    (PROG (ACTION BODY ENDVAL INCR INITVAL RESULT TESTEXP VAR X INCRTYPE)
      (SETQ VAR (CADR U))
      (SETQ INCR (CADDR U))
      (SETQ INCR (LIST (CAR INCR) (CADR INCR) (CADDR INCR)))
      (SETQ INCRTYPE
              (TOP_TYPE
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J INCR)
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J))
                                       NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (N_FORM1 J VARS)) (CAR J)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ ACTION (CADDDR U))
      (SETQ BODY (CAR (CDDDDR U)))
      (SETQ INITVAL (CAR INCR))
      (SETQ ENDVAL (CADDR INCR))
      (SETQ INCR (CADR INCR))
      (SETQ X (LIST 'DIFFERENCE ENDVAL VAR))
      (COND ((NEQ INCR 1) (SETQ X (LIST 'TIMES INCR X))))
      (SETQ X (LIST 'LESSP X 0))
      (SETQ TESTEXP X)
      (SETQ RESULT (GENSYM))
      (SETQ X
              (SUBLIS
               (LIST (CONS 'BODY2 (LIST (GET ACTION 'BIN) BODY RESULT))
                     (CONS 'BODY3 BODY) (CONS 'BODY BODY)
                     (CONS 'INITVAL INITVAL) (CONS 'NILLIST NIL)
                     (CONS 'RESULT RESULT) (CONS 'INCRTYPE INCRTYPE)
                     (CONS 'INITRESULT (GET ACTION 'INITVAL))
                     (CONS 'RESULTLIST RESULT) (CONS 'TESTEXP TESTEXP)
                     (CONS 'UPDFN 'PLUS) (CONS 'UPDVAL INCR) (CONS 'VAR VAR))
               (COND
                ((EQ ACTION 'DO)
                 '(RBLOCK ((VAR . INCRTYPE)) (SETQ VAR INITVAL) LAB
                          (COND (TESTEXP (RETURN NIL))) BODY
                          (SETQ VAR (UPDFN VAR UPDVAL)) (GO LAB)))
                ((EQ ACTION 'COLLECT)
                 '(RBLOCK
                   ((VAR . INCRTYPE) (RESULT . GENERIC) (ENDPTR . GENERIC))
                   (SETQ VAR INITVAL) (COND (TESTEXP (RETURN NILLIST)))
                   (SETQ RESULT (SETQ ENDPTR (CONS BODY NIL))) LOOPLABEL
                   (SETQ VAR (UPDFN VAR UPDVAL))
                   (COND (TESTEXP (RETURN RESULTLIST)))
                   (RPLACD ENDPTR (CONS BODY NIL)) (SETQ ENDPTR (CDR ENDPTR))
                   (GO LOOPLABEL)))
                ((EQ ACTION 'CONC)
                 '(RBLOCK
                   ((VAR . INCRTYPE) (RESULT . GENERIC) (ENDPTR . GENERIC))
                   (SETQ VAR INITVAL) STARTOVER
                   (COND (TESTEXP (RETURN NILLIST))) (SETQ RESULT BODY)
                   (SETQ ENDPTR (LASTPAIR RESULTLIST))
                   (SETQ VAR (UPDFN VAR UPDVAL))
                   (COND ((ATOM ENDPTR) (GO STARTOVER))) LOOPLABEL
                   (COND (TESTEXP (RETURN RESULT))) (RPLACD ENDPTR BODY3)
                   (SETQ ENDPTR (LASTPAIR ENDPTR))
                   (SETQ VAR (UPDFN VAR UPDVAL)) (GO LOOPLABEL)))
                (T
                 '(RBLOCK ((VAR . INCRTYPE) (RESULT . GENERIC))
                          (SETQ VAR INITVAL) (SETQ RESULT INITRESULT) LAB1
                          (COND (TESTEXP (RETURN RESULT))) (SETQ RESULT BODY2)
                          (SETQ VAR (UPDFN VAR UPDVAL)) (GO LAB1))))))
      (RETURN (N_FORM1 X VARS)))) 
(ENDMODULE) 