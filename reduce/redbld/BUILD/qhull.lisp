(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'QHULL)) 
(CREATE-PACKAGE '(QHULL) NIL) 
(LOAD-PACKAGE 'ASSERT) 
(SWITCH (LIST 'QHULLKEEPFILES)) 
(FLUID '(QHULL_CALL* QHULL_WD*)) 
(SETQ QHULL_CALL* "qhull") 
(SETQ QHULL_WD* "/tmp/") 
(FLAG '(QHULL) 'OPFN) 
(PUT 'QHULL 'NUMBER-OF-ARGS 1) 
(DE QHULL (L) (QHULL_S2ALIST (QHULL_QHULL (QHULL_A2SLIST L)))) 
(PUT 'QHULL_A2SLIST 'NUMBER-OF-ARGS 1) 
(DE QHULL_A2SLIST (L)
    (PROG (PT FORALL-RESULT FORALL-ENDPTR)
      (SETQ PT (CDR L))
      (COND ((NULL PT) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (PT) (CDR PT)) (CAR PT)) NIL)))
     LOOPLABEL
      (SETQ PT (CDR PT))
      (COND ((NULL PT) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PT) (CDR PT)) (CAR PT)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'QHULL_S2ALIST 'NUMBER-OF-ARGS 1) 
(DE QHULL_S2ALIST (L)
    (CONS 'LIST
          (PROG (PT FORALL-RESULT FORALL-ENDPTR)
            (SETQ PT L)
            (COND ((NULL PT) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (PT) (CONS 'LIST PT)) (CAR PT))
                                  NIL)))
           LOOPLABEL
            (SETQ PT (CDR PT))
            (COND ((NULL PT) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (PT) (CONS 'LIST PT)) (CAR PT)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'QHULL_QHULL 'NUMBER-OF-ARGS 1) 
(DE QHULL_QHULL (L)
    (PROG (FN1 FN2 QHULL CALL W RND D N)
      (SETQ RND 0)
      (SETQ D 0)
      (SETQ N 0)
      (COND ((NULL L) (RETURN NIL)))
      (SETQ D (LENGTH (CAR L)))
      (SETQ N (LENGTH L))
      (SETQ RND (LTO_AT2STR (RANDOM (EXPT 10 5))))
      (SETQ FN1 (LTO_SCONCAT (LIST QHULL_WD* (GETENV "USER") RND "-qhull.in")))
      (SETQ FN2
              (LTO_SCONCAT (LIST QHULL_WD* (GETENV "USER") RND "-qhull.out")))
      (SETQ QHULL (OR (GETENV "QHULL") QHULL_CALL*))
      (SETQ CALL (LTO_SCONCAT (LIST QHULL " p < " FN1 " > " FN2)))
      (QHULL_CREATEINFILE L D N FN1)
      (SETQ W (SYSTEM CALL))
      (COND
       ((NOT (EQN W 0))
        (PROGN
         (COND ((NOT *QHULLKEEPFILES) (SYSTEM (LTO_SCONCAT (LIST "rm " FN1)))))
         (REDERR "qhull call failed"))))
      (SETQ W (QHULL_PARSEOUTFILE FN2))
      (COND
       ((NOT *QHULLKEEPFILES) (SYSTEM (LTO_SCONCAT (LIST "rm " FN1 " " FN2)))))
      (RETURN W))) 
(PUT 'QHULL_CREATEINFILE 'NUMBER-OF-ARGS 4) 
(DE QHULL_CREATEINFILE (L D N FN1)
    (PROGN
     (OUT (LIST FN1))
     (PRIN2T D)
     (PRIN2T N)
     (PROG (PT)
       (SETQ PT L)
      LAB
       (COND ((NULL PT) (RETURN NIL)))
       ((LAMBDA (PT)
          (PROGN
           (PROG (RPTL)
             (SETQ RPTL PT)
            LAB
             (COND ((NULL RPTL) (RETURN NIL)))
             (PROGN (PRIN2 (CAR RPTL)) (COND ((CDR RPTL) (PRIN2 " "))))
             (SETQ RPTL (CDR RPTL))
             (GO LAB))
           (TERPRI)))
        (CAR PT))
       (SETQ PT (CDR PT))
       (GO LAB))
     (SHUT (LIST FN1)))) 
(PUT 'QHULL_PARSEOUTFILE 'NUMBER-OF-ARGS 1) 
(DE QHULL_PARSEOUTFILE (FN2)
    (PROG (CH RES D N)
      (SETQ D 0)
      (SETQ N 0)
      (SETQ CH (OPEN FN2 'INPUT))
      (RDS CH)
      (SETQ D (READ))
      (SETQ N (READ))
      (SETQ RES
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ J 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS (READ) NIL)))
                                  LOOPLABEL
                                   (SETQ J (PLUS2 J 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE D J))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR (CONS (READ) NIL))
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
                           (COND ((MINUSP (DIFFERENCE D J)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS (READ) NIL)))
                          LOOPLABEL
                           (SETQ J (PLUS2 J 1))
                           (COND
                            ((MINUSP (DIFFERENCE D J)) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS (READ) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RDS NIL)
      (CLOSE CH)
      (RETURN RES))) 
(ENDMODULE) 