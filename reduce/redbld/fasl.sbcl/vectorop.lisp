(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'VECTOROP)) 
(PUT '|DEPTHL1:| 'NUMBER-OF-ARGS 1) 
(PUT '|DEPTHL1:| 'DEFINED-ON-LINE '33) 
(PUT '|DEPTHL1:| 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT '|DEPTHL1:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |DEPTHL1:| (U)
    (COND ((NULL U) T) (T (AND (NEQ (CAAR U) 'LIST) (|DEPTHL1:| (CDR U)))))) 
(PUT 'DEPTHL1 'NUMBER-OF-ARGS 1) 
(PUT 'DEPTHL1 'DEFINED-ON-LINE '36) 
(PUT 'DEPTHL1 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'DEPTHL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEPTHL1 (U) (AND (NOT (NULL (GETRTYPE U))) (|DEPTHL1:| (CDR U)))) 
(PUT '|:VECT| 'NUMBER-OF-ARGS 3) 
(PUT '|:VECT| 'DEFINED-ON-LINE '39) 
(PUT '|:VECT| 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT '|:VECT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |:VECT| (U V BOOL)
    (COND ((NULL U) NIL)
          (T
           (CONS
            (ADDSQ (CAR U) (COND ((NULL BOOL) (CAR V)) (T (NEGSQ (CAR V)))))
            (|:VECT| (CDR U) (CDR V) BOOL))))) 
(PUT 'RSUMVECT 'NUMBER-OF-ARGS 1) 
(PUT 'RSUMVECT 'DEFINED-ON-LINE '45) 
(PUT 'RSUMVECT 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'RSUMVECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RSUMVECT (U)
    (PROG (X Y PRF)
      (SETQ X (REVAL1 (CAR U) T))
      (SETQ Y (REVAL1 (CADR U) T))
      (SETQ PRF (CAR X))
      (COND
       ((OR (EQUAL (RDEPTH (LIST X)) 0) (EQUAL (RDEPTH (LIST Y)) 0))
        (REDERR " both arguments must be of depth 1 "))
       (T (SETQ X (CDR X))))
      (SETQ Y (CDR Y))
      (COND ((NEQ (LENGTH X) (LENGTH Y)) (REDERR "vector mismatch")))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Y
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J Y)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS PRF
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J (|:VECT| X Y NIL))
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'SUMVECT 'PSOPFN 'RSUMVECT) 
(PUT 'RMINVECT 'NUMBER-OF-ARGS 1) 
(PUT 'RMINVECT 'DEFINED-ON-LINE '58) 
(PUT 'RMINVECT 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'RMINVECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RMINVECT (U)
    (PROG (X Y PRF)
      (SETQ X (REVAL1 (CAR U) T))
      (SETQ Y (REVAL1 (CADR U) T))
      (SETQ PRF (CAR X))
      (COND
       ((OR (EQUAL (RDEPTH (LIST X)) 0) (EQUAL (RDEPTH (LIST Y)) 0))
        (REDERR " both arguments must be of depth 1 "))
       (T (SETQ X (CDR X))))
      (SETQ Y (CDR Y))
      (COND ((NEQ (LENGTH X) (LENGTH Y)) (REDERR "vector mismatch")))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Y
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J Y)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (CONS PRF
             (PROG (J FORALL-RESULT FORALL-ENDPTR)
               (SETQ J (|:VECT| X Y 'MINUS))
               (COND ((NULL J) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL)))
              LOOPLABEL
               (SETQ J (CDR J))
               (COND ((NULL J) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'MINVECT 'PSOPFN 'RMINVECT) 
(PUT '|:SCALPRD| 'NUMBER-OF-ARGS 2) 
(PUT '|:SCALPRD| 'DEFINED-ON-LINE '71) 
(PUT '|:SCALPRD| 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT '|:SCALPRD| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |:SCALPRD| (U V)
    (COND ((AND (NULL U) (NULL V)) (CONS NIL 1))
          (T (ADDSQ (MULTSQ (CAR U) (CAR V)) (|:SCALPRD| (CDR U) (CDR V)))))) 
(PUT 'SSCALVECT 'NUMBER-OF-ARGS 1) 
(PUT 'SSCALVECT 'DEFINED-ON-LINE '76) 
(PUT 'SSCALVECT 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'SSCALVECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SSCALVECT (U)
    (PROG (X Y)
      (SETQ X (REVAL1 (CAR U) T))
      (SETQ Y (REVAL1 (CADR U) T))
      (COND
       ((OR (EQUAL (RDEPTH (LIST X)) 0) (EQUAL (RDEPTH (LIST Y)) 0))
        (REDERR " both arguments must be of depth 1 "))
       ((NEQ (LENGTH X) (LENGTH Y)) (REDERR "vector mismatch")))
      (SETQ X (CDR X))
      (SETQ Y (CDR Y))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Y
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J Y)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK*SQ (|:SCALPRD| X Y))))) 
(PUT 'SCALVECT 'PSOPFN 'SSCALVECT) 
(PUT '|:PVECT3| 'NUMBER-OF-ARGS 1) 
(PUT '|:PVECT3| 'DEFINED-ON-LINE '90) 
(PUT '|:PVECT3| 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT '|:PVECT3| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |:PVECT3| (U)
    (PROG (X Y XL)
      (SETQ XL 0)
      (COND
       ((OR (EQUAL (RDEPTH (LIST (CAR U))) 0) (EQUAL (RDEPTH (CDR U)) 0))
        (REDERR " both arguments must be of depth 1 "))
       (T (SETQ X (REVAL1 (CAR U) T))))
      (SETQ Y (REVAL1 (CADR U) T))
      (COND ((NEQ (SETQ XL (LENGTH X)) 4) (REDERR "not 3-space vectors"))
            ((NEQ XL (LENGTH Y)) (REDERR "vector mismatch")))
      (SETQ X (CDR X))
      (SETQ Y (CDR Y))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Y
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J Y)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (LIST
        (ADDSQ (MULTSQ (CADR X) (CADDR Y)) (NEGSQ (MULTSQ (CADDR X) (CADR Y))))
        (ADDSQ (MULTSQ (CADDR X) (CAR Y)) (NEGSQ (MULTSQ (CAR X) (CADDR Y))))
        (ADDSQ (MULTSQ (CAR X) (CADR Y)) (NEGSQ (MULTSQ (CADR X) (CAR Y)))))))) 
(PUT 'RCROSSVECT 'NUMBER-OF-ARGS 1) 
(PUT 'RCROSSVECT 'DEFINED-ON-LINE '106) 
(PUT 'RCROSSVECT 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'RCROSSVECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RCROSSVECT (U)
    (CONS 'LIST
          (PROG (J FORALL-RESULT FORALL-ENDPTR)
            (SETQ J (|:PVECT3| U))
            (COND ((NULL J) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL)))
           LOOPLABEL
            (SETQ J (CDR J))
            (COND ((NULL J) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'CROSSVECT 'PSOPFN 'RCROSSVECT) 
(PUT 'SMPVECT 'NUMBER-OF-ARGS 1) 
(PUT 'SMPVECT 'DEFINED-ON-LINE '112) 
(PUT 'SMPVECT 'DEFINED-IN-FILE 'ASSIST/VECTOROP.RED) 
(PUT 'SMPVECT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SMPVECT (U)
    (PROG (X)
      (COND
       ((EQUAL (RDEPTH (LIST (CAR U))) 0)
        (REDERR " arguments must be of depth 1 "))
       (T (SETQ X (REVAL1 (CAR U) T))))
      (SETQ U (CDR U))
      (SETQ X (CDR X))
      (COND ((NEQ (LENGTH X) 3) (REDERR " not 3-space vector")))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (SIMP* J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (MK*SQ (|:SCALPRD| X (|:PVECT3| U)))))) 
(PUT 'MPVECT 'PSOPFN 'SMPVECT) 
(ENDMODULE) 