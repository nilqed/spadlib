(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTFAC)) 
(FLUID '(*INTFAC *SURDS KORD* ZLIST)) 
(EXPORTS (LIST 'INT-FAC)) 
(PUT 'INT-FAC 'NUMBER-OF-ARGS 1) 
(PUT 'INT-FAC 'DEFINED-ON-LINE '37) 
(PUT 'INT-FAC 'DEFINED-IN-FILE 'INT/INTFAC.RED) 
(PUT 'INT-FAC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT-FAC (X)
    (PROG (*INTFAC *SURDS)
      (SETQ *INTFAC (SETQ *SURDS T))
      (RETURN (INT-FAC-INNER X)))) 
(PUT 'INT-FAC-INNER 'NUMBER-OF-ARGS 1) 
(PUT 'INT-FAC-INNER 'DEFINED-ON-LINE '49) 
(PUT 'INT-FAC-INNER 'DEFINED-IN-FILE 'INT/INTFAC.RED) 
(PUT 'INT-FAC-INNER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INT-FAC-INNER (X)
    (PROG (FACTORS)
      (SETQ FACTORS (FCTRF X))
      (SETQ FACTORS (CDR FACTORS))
      (SETQ FACTORS
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U FACTORS)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (COND ((EQUAL (CDR U) 1) (CAR U))
                                          (T
                                           (INTERR
                                            (LIST X "not square free")))))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (COND ((EQUAL (CDR U) 1) (CAR U))
                                  (T (INTERR (LIST X "not square free")))))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (REVERSIP
        (PROG (U FORALL-RESULT FORALL-ENDPTR)
          (SETQ U FACTORS)
         STARTOVER
          (COND ((NULL U) (RETURN NIL)))
          (SETQ FORALL-RESULT ((LAMBDA (U) (FAC2INT U)) (CAR U)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
          (SETQ U (CDR U))
          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
         LOOPLABEL
          (COND ((NULL U) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR ((LAMBDA (U) (FAC2INT U)) (CAR U)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
          (SETQ U (CDR U))
          (GO LOOPLABEL)))))) 
(PUT 'FAC2INT 'NUMBER-OF-ARGS 1) 
(PUT 'FAC2INT 'DEFINED-ON-LINE '63) 
(PUT 'FAC2INT 'DEFINED-IN-FILE 'INT/INTFAC.RED) 
(PUT 'FAC2INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FAC2INT (U)
    (PROG (DEGREES X)
      (SETQ DEGREES
              (PROG (W FORALL-RESULT FORALL-ENDPTR)
                (SETQ W ZLIST)
                (COND ((NULL W) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (W) (CONS (DEGREEF U W) W)) (CAR W))
                                 NIL)))
               LOOPLABEL
                (SETQ W (CDR W))
                (COND ((NULL W) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (W) (CONS (DEGREEF U W) W)) (CAR W))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((ASSOC 1 DEGREES) (RETURN (LIST (CONS 'LOG (CONS U 1)))))
            ((SETQ X (ASSOC 2 DEGREES)) (RETURN (INT-QUADTERM U (CDR X))))
            ((ASSOC 0 DEGREES) (RETURN (LIST (CONS 'LOG (CONS U 1))))))
      (COND
       (*TRINT
        (PROGN
         (PROGN (PRIN2 "*** Polynomial") (TERPRI) "*** Polynomial")
         (PRINTSF U)
         (PROGN
          (PRIN2 "has not been completely factored")
          (TERPRI)
          "has not been completely factored"))))
      (RETURN (LIST (CONS 'LOG (CONS U 1)))))) 
(PUT 'INT-QUADTERM 'NUMBER-OF-ARGS 2) 
(PUT 'INT-QUADTERM 'DEFINED-ON-LINE '81) 
(PUT 'INT-QUADTERM 'DEFINED-IN-FILE 'INT/INTFAC.RED) 
(PUT 'INT-QUADTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INT-QUADTERM (POL VAR)
    (PROG (A B C DISCRIM KORD RES W)
      (SETQ KORD (SETKORDER (CONS VAR KORD*)))
      (SETQ C (REORDER POL))
      (COND
       ((NEQ (CDAAR C) 2)
        (PROGN
         (SETKORDER KORD)
         (RERROR 'INT 5 "Invalid polynomial in int-quadterm"))))
      (SETQ A (CDAR C))
      (SETQ C (CDR C))
      (COND
       ((AND (NOT (OR (ATOM C) (ATOM (CAR C)))) (EQUAL (CAAAR C) VAR)
             (EQUAL (CDAAR C) 1))
        (PROGN (SETQ B (CDAR C)) (SETQ C (CDR C)))))
      (SETKORDER KORD)
      (SETQ DISCRIM
              (POWSUBSF
               (ADDF
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF B B)) (T (POLY-MULTF B B)))
                (MULTD (MINUS 4)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF A C))
                             (T (POLY-MULTF A C)))))))
      (COND ((NULL DISCRIM) (INTERR "discrim is zero in quadterm")))
      (SETQ W (ROOTXF* (NEGF DISCRIM) 2))
      (COND ((NOT (EQ W 'FAILED)) (GO ATANCASE)))
      (SETQ W (ROOTXF* DISCRIM 2))
      (COND ((EQ W 'FAILED) (RETURN (LIST (CONS 'LOG (CONS POL 1))))))
      (SETQ DISCRIM W)
      (SETQ W
              ((LAMBDA (G544)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 A))
                       (T (POLY-MULTF G544 A))))
               (LIST (CONS (GETPOWER (FKERN VAR) 1) 1))))
      (SETQ W (ADDF (MULTD 2 W) B))
      (SETQ A (ADDF W DISCRIM))
      (SETQ B (ADDF W (NEGF DISCRIM)))
      (SETQ A ((LAMBDA (*EXP) (QUOTF1 A (CDR (COMFAC A)))) T))
      (SETQ B ((LAMBDA (*EXP) (QUOTF1 B (CDR (COMFAC B)))) T))
      (RETURN (CONS (CONS 'LOG (CONS A 1)) (CONS (CONS 'LOG (CONS B 1)) RES)))
     ATANCASE
      (SETQ RES (CONS (CONS 'LOG (CONS POL 1)) RES))
      (SETQ A
              ((LAMBDA (G544)
                 (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 A))
                       (T (POLY-MULTF G544 A))))
               (LIST (CONS (GETPOWER (FKERN VAR) 1) 1))))
      (SETQ A (ADDF B (MULTD 2 A)))
      (SETQ A (FQUOTF A W))
      (RETURN (CONS (CONS 'ATAN A) RES)))) 
(PUT 'ROOTXF* 'NUMBER-OF-ARGS 2) 
(PUT 'ROOTXF* 'DEFINED-ON-LINE '127) 
(PUT 'ROOTXF* 'DEFINED-IN-FILE 'INT/INTFAC.RED) 
(PUT 'ROOTXF* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROOTXF* (U N)
    ((LAMBDA (X)
       (COND
        ((OR (EQ X 'FAILED) (AND (SMEMQ 'I X) (NOT (SMEMQ 'I U))))
         ((LAMBDA (*SURDS) (ROOTXF U N)) NIL))
        (T X)))
     (ROOTXF U N))) 
(ENDMODULE) 