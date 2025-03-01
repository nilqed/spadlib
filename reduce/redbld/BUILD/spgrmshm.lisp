(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPGRMSHM)) 
(PUT 'SPGRAM_SCHMIDT 'NUMBER-OF-ARGS 1) 
(PUT 'SPGRAM_SCHMIDT 'DEFINED-ON-LINE '42) 
(PUT 'SPGRAM_SCHMIDT 'DEFINED-IN-FILE 'SPARSE/SPGRMSHM.RED) 
(PUT 'SPGRAM_SCHMIDT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPGRAM_SCHMIDT (VEC_LIST)
    (PROG (GS_LIST)
      (SETQ VEC_LIST (CDR VEC_LIST))
      (COND
       ((AND (PAIRP VEC_LIST) (PAIRP (CAR VEC_LIST))
             (EQUAL (CAAR VEC_LIST) 'LIST) (PAIRP (CDAR VEC_LIST))
             (PAIRP (CADAR VEC_LIST)) (EQUAL (CAADAR VEC_LIST) 'LIST))
        (SETQ VEC_LIST (CDAR VEC_LIST))))
      (SETQ VEC_LIST (SPCONVERT_TO_SQ VEC_LIST))
      (SETQ GS_LIST (GRAM+SCHMID VEC_LIST))
      (RETURN (SPCONVERT_FROM_SQ GS_LIST)))) 
(FLAG '(SPGRAM_SCHMIDT) 'OPFN) 
(PUT 'SPCONVERT_TO_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'SPCONVERT_TO_SQ 'DEFINED-ON-LINE '67) 
(PUT 'SPCONVERT_TO_SQ 'DEFINED-IN-FILE 'SPARSE/SPGRMSHM.RED) 
(PUT 'SPCONVERT_TO_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCONVERT_TO_SQ (VEC_LIST)
    (PROG (SQ_LIST VAL RES)
      (PROG (LIST)
        (SETQ LIST VEC_LIST)
       LAB
        (COND ((NULL LIST) (RETURN NIL)))
        ((LAMBDA (LIST)
           (PROGN
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND
               ((MINUSP (DIFFERENCE (SPROW_DIM (CADR LIST)) I)) (RETURN NIL)))
              (PROGN
               (PROG (J)
                 (SETQ J 1)
                LAB
                 (COND
                  ((MINUSP (DIFFERENCE (SPCOL_DIM (CADR LIST)) J))
                   (RETURN NIL)))
                 (PROGN
                  (SETQ VAL (SIMP* (FINDELEM2 (CADR LIST) I J)))
                  (SETQ RES (CONS VAL RES))
                  NIL)
                 (SETQ J (PLUS2 J 1))
                 (GO LAB))
               NIL)
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ SQ_LIST (APPEND SQ_LIST (LIST (REVERSE RES))))
            (SETQ RES NIL)
            NIL))
         (CAR LIST))
        (SETQ LIST (CDR LIST))
        (GO LAB))
      (RETURN SQ_LIST))) 
(PUT 'SPCONVERT_FROM_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'SPCONVERT_FROM_SQ 'DEFINED-ON-LINE '88) 
(PUT 'SPCONVERT_FROM_SQ 'DEFINED-IN-FILE 'SPARSE/SPGRMSHM.RED) 
(PUT 'SPCONVERT_FROM_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPCONVERT_FROM_SQ (SQ_LIST)
    (PROG (GS_LIST CNT RES VAL LEN)
      (PROG (ELT1)
        (SETQ ELT1 SQ_LIST)
       LAB
        (COND ((NULL ELT1) (RETURN NIL)))
        ((LAMBDA (ELT1)
           (PROGN
            (SETQ CNT 0)
            (SETQ LEN (LENGTH ELT1))
            (SETQ RES (MKEMPSPMAT LEN (LIST 'SPM LEN 1)))
            (PROG (ELT)
              (SETQ ELT ELT1)
             LAB
              (COND ((NULL ELT) (RETURN NIL)))
              ((LAMBDA (ELT)
                 (PROGN
                  (SETQ VAL (PREPSQ ELT))
                  (COND
                   ((NOT (EQUAL VAL 0))
                    (LETMTR3 (LIST RES (SETQ CNT (PLUS CNT 1)))
                     (CONS (LIST NIL) (LIST (CONS 1 VAL))) RES NIL))
                   (T (SETQ CNT (PLUS CNT 1))))
                  NIL))
               (CAR ELT))
              (SETQ ELT (CDR ELT))
              (GO LAB))
            (SETQ GS_LIST (APPEND GS_LIST (LIST RES)))
            (SETQ RES NIL)
            NIL))
         (CAR ELT1))
        (SETQ ELT1 (CDR ELT1))
        (GO LAB))
      (RETURN (CONS 'LIST GS_LIST)))) 
(ENDMODULE) 