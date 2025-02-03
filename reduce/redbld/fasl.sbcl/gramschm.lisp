(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GRAMCHMD)) 
(PUT 'GRAM_SCHMIDT 'PSOPFN 'GRAM_SCHMIDT1) 
(PUT 'GRAM_SCHMIDT1 'NUMBER-OF-ARGS 1) 
(PUT 'GRAM_SCHMIDT1 'DEFINED-ON-LINE '42) 
(PUT 'GRAM_SCHMIDT1 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'GRAM_SCHMIDT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRAM_SCHMIDT1 (VEC_LIST)
    (PROG (GS_LIST)
      (COND
       ((AND (PAIRP VEC_LIST) (PAIRP (CAR VEC_LIST))
             (EQUAL (CAAR VEC_LIST) 'LIST) (PAIRP (CDAR VEC_LIST))
             (PAIRP (CADAR VEC_LIST)) (EQUAL (CAADAR VEC_LIST) 'LIST))
        (SETQ VEC_LIST (CDAR VEC_LIST))))
      (SETQ VEC_LIST (CONVERT_TO_SQ VEC_LIST))
      (SETQ GS_LIST (GRAM+SCHMID VEC_LIST))
      (RETURN (CONVERT_FROM_SQ GS_LIST)))) 
(PUT 'CONVERT_TO_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT_TO_SQ 'DEFINED-ON-LINE '66) 
(PUT 'CONVERT_TO_SQ 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'CONVERT_TO_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT_TO_SQ (VEC_LIST)
    (PROG (SQ_LIST)
      (SETQ SQ_LIST
              (PROG (LIST FORALL-RESULT FORALL-ENDPTR)
                (SETQ LIST VEC_LIST)
                (COND ((NULL LIST) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (LIST)
                                    (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ ELT (CDR LIST))
                                      (COND ((NULL ELT) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (ELT)
                                                          (SIMP* ELT))
                                                        (CAR ELT))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ ELT (CDR ELT))
                                      (COND
                                       ((NULL ELT) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELT) (SIMP* ELT))
                                                (CAR ELT))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR LIST))
                                 NIL)))
               LOOPLABEL
                (SETQ LIST (CDR LIST))
                (COND ((NULL LIST) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (LIST)
                            (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                              (SETQ ELT (CDR LIST))
                              (COND ((NULL ELT) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (ELT) (SIMP* ELT))
                                                (CAR ELT))
                                               NIL)))
                             LOOPLABEL
                              (SETQ ELT (CDR ELT))
                              (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELT) (SIMP* ELT)) (CAR ELT))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR LIST))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN SQ_LIST))) 
(PUT 'CONVERT_FROM_SQ 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT_FROM_SQ 'DEFINED-ON-LINE '80) 
(PUT 'CONVERT_FROM_SQ 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'CONVERT_FROM_SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT_FROM_SQ (SQ_LIST)
    (PROG (GS_LIST)
      (SETQ GS_LIST
              (CONS 'LIST
                    (PROG (ELT1 FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ELT1 SQ_LIST)
                      (COND ((NULL ELT1) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (ELT1)
                                          (CONS 'LIST
                                                (PROG (ELT FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ ELT ELT1)
                                                  (COND
                                                   ((NULL ELT) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA
                                                                        (ELT)
                                                                      (PREPSQ
                                                                       ELT))
                                                                    (CAR ELT))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ ELT (CDR ELT))
                                                  (COND
                                                   ((NULL ELT)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (ELT)
                                                              (PREPSQ ELT))
                                                            (CAR ELT))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL))))
                                        (CAR ELT1))
                                       NIL)))
                     LOOPLABEL
                      (SETQ ELT1 (CDR ELT1))
                      (COND ((NULL ELT1) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (ELT1)
                                  (CONS 'LIST
                                        (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ ELT ELT1)
                                          (COND ((NULL ELT) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (ELT)
                                                              (PREPSQ ELT))
                                                            (CAR ELT))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ ELT (CDR ELT))
                                          (COND
                                           ((NULL ELT) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (ELT) (PREPSQ ELT))
                                                    (CAR ELT))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))
                                (CAR ELT1))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (RETURN GS_LIST))) 
(PUT 'VECTOR+P 'NUMBER-OF-ARGS 1) 
(PUT 'VECTOR+P 'DEFINED-ON-LINE '96) 
(PUT 'VECTOR+P 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'VECTOR+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTOR+P (VECTOR1)
    (PROG () (COND ((GREATERP (LENGTH VECTOR1) 0) (RETURN T))))) 
(PUT 'GET+VEC+DIM 'NUMBER-OF-ARGS 1) 
(PUT 'GET+VEC+DIM 'DEFINED-ON-LINE '107) 
(PUT 'GET+VEC+DIM 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'GET+VEC+DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET+VEC+DIM (VECTOR1) (PROG () (RETURN (LENGTH VECTOR1)))) 
(PUT 'GET+VEC+ENTRY 'NUMBER-OF-ARGS 2) 
(PUT 'GET+VEC+ENTRY 'DEFINED-ON-LINE '118) 
(PUT 'GET+VEC+ENTRY 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'GET+VEC+ENTRY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET+VEC+ENTRY (VECTOR1 ELEM) (PROG () (RETURN (NTH VECTOR1 ELEM)))) 
(PUT 'MK+VEC+ADD+VEC 'NUMBER-OF-ARGS 2) 
(PUT 'MK+VEC+ADD+VEC 'DEFINED-ON-LINE '129) 
(PUT 'MK+VEC+ADD+VEC 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+VEC+ADD+VEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+VEC+ADD+VEC (VECTOR1 VECTOR2)
    (PROG (ENT RES H)
      (SETQ RES
              (PROG (ENT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ENT 1)
                (COND
                 ((MINUSP (DIFFERENCE (GET+VEC+DIM VECTOR1) ENT))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROGN
                                  (SETQ H
                                          (ADDSQ (GET+VEC+ENTRY VECTOR1 ENT)
                                                 (GET+VEC+ENTRY VECTOR2 ENT)))
                                  ((LAMBDA (*SUB2) (SETQ H (SUBS2 H))) T)
                                  H)
                                 NIL)))
               LOOPLABEL
                (SETQ ENT (PLUS2 ENT 1))
                (COND
                 ((MINUSP (DIFFERENCE (GET+VEC+DIM VECTOR1) ENT))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROGN
                          (SETQ H
                                  (ADDSQ (GET+VEC+ENTRY VECTOR1 ENT)
                                         (GET+VEC+ENTRY VECTOR2 ENT)))
                          ((LAMBDA (*SUB2) (SETQ H (SUBS2 H))) T)
                          H)
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+SQUARED+NORM 'NUMBER-OF-ARGS 1) 
(PUT 'MK+SQUARED+NORM 'DEFINED-ON-LINE '147) 
(PUT 'MK+SQUARED+NORM 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+SQUARED+NORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+SQUARED+NORM (VECTOR1)
    (PROG () (RETURN (MK+INNER+PRODUCT VECTOR1 VECTOR1)))) 
(PUT 'MY+NULLSQ+P 'NUMBER-OF-ARGS 1) 
(PUT 'MY+NULLSQ+P 'DEFINED-ON-LINE '157) 
(PUT 'MY+NULLSQ+P 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MY+NULLSQ+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MY+NULLSQ+P (SCAL) (PROG () (COND ((NULL (CAR SCAL)) (RETURN T))))) 
(PUT 'MK+NULL+VEC 'NUMBER-OF-ARGS 1) 
(PUT 'MK+NULL+VEC 'DEFINED-ON-LINE '167) 
(PUT 'MK+NULL+VEC 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+NULL+VEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+NULL+VEC (DIMEN)
    (PROG (NULLSQ I RES)
      (SETQ NULLSQ (CONS NIL 1))
      (SETQ RES
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIMEN I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NULLSQ NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE DIMEN I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS NULLSQ NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'NULL+VEC+P 'NUMBER-OF-ARGS 1) 
(PUT 'NULL+VEC+P 'DEFINED-ON-LINE '180) 
(PUT 'NULL+VEC+P 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'NULL+VEC+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULL+VEC+P (VECTOR1)
    (PROG () (COND ((MY+NULLSQ+P (MK+SQUARED+NORM VECTOR1)) (RETURN T))))) 
(PUT 'MK+NORMALIZE+VECTOR 'NUMBER-OF-ARGS 1) 
(PUT 'MK+NORMALIZE+VECTOR 'DEFINED-ON-LINE '189) 
(PUT 'MK+NORMALIZE+VECTOR 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+NORMALIZE+VECTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+NORMALIZE+VECTOR (VECTOR1)
    (PROG (SCALO VECRES)
      (SETQ SCALO (SIMP* (LIST 'SQRT (MK*SQ (MK+SQUARED+NORM VECTOR1)))))
      (COND
       ((MY+NULLSQ+P SCALO) (SETQ VECRES (MK+NULL+VEC (GET+VEC+DIM VECTOR1))))
       (T
        (PROGN
         (SETQ SCALO (SIMP (PREPSQ SCALO)))
         (SETQ SCALO (MULTSQ (CONS 1 1) (INVSQ SCALO)))
         (SETQ VECRES (MK+SCAL+MULT+VEC SCALO VECTOR1))
         NIL)))
      (RETURN VECRES))) 
(PUT 'MK+GRAM+SCHMID 'NUMBER-OF-ARGS 2) 
(PUT 'MK+GRAM+SCHMID 'DEFINED-ON-LINE '209) 
(PUT 'MK+GRAM+SCHMID 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+GRAM+SCHMID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+GRAM+SCHMID (VECTORLIST VECTOR1)
    (PROG (I ORTHOVEC SCALO VECTORS)
      (SETQ ORTHOVEC VECTOR1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VECTORLIST) I)) (RETURN NIL)))
        (PROGN
         (SETQ SCALO (NEGSQ (MK+INNER+PRODUCT ORTHOVEC (NTH VECTORLIST I))))
         (SETQ ORTHOVEC
                 (MK+VEC+ADD+VEC ORTHOVEC
                  (MK+SCAL+MULT+VEC SCALO (NTH VECTORLIST I))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ ORTHOVEC (MK+NORMALIZE+VECTOR ORTHOVEC))
      (COND ((NULL+VEC+P ORTHOVEC) (SETQ VECTORS VECTORLIST))
            (T (SETQ VECTORS (APPEND VECTORLIST (LIST ORTHOVEC)))))
      (RETURN VECTORS))) 
(PUT 'GRAM+SCHMID 'NUMBER-OF-ARGS 1) 
(PUT 'GRAM+SCHMID 'DEFINED-ON-LINE '231) 
(PUT 'GRAM+SCHMID 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'GRAM+SCHMID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRAM+SCHMID (VECTORLIST)
    (PROG (ORTHOLIST I)
      (COND
       ((LESSP (LENGTH VECTORLIST) 1)
        (REDERR "Error in Gram Schmidt: no input.")))
      (COND ((VECTOR+P (CAR VECTORLIST)) (SETQ ORTHOLIST NIL))
            (T (REDERR "Error in Gram_schmidt: empty input.")))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VECTORLIST) I)) (RETURN NIL)))
        (SETQ ORTHOLIST (MK+GRAM+SCHMID ORTHOLIST (NTH VECTORLIST I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ORTHOLIST))) 
(PUT 'MK+INNER+PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+INNER+PRODUCT 'DEFINED-ON-LINE '260) 
(PUT 'MK+INNER+PRODUCT 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+INNER+PRODUCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+INNER+PRODUCT (VECTOR1 VECTOR2)
    (PROG (Z SUM VEC2)
      (COND
       ((NOT (EQUAL (GET+VEC+DIM VECTOR1) (GET+VEC+DIM VECTOR2)))
        (REDERR
         "Error in Gram_schmidt: each list in input must be the same length.")))
      (SETQ SUM (CONS NIL 1))
      (COND (*COMPLEX (SETQ VEC2 (MK+CONJUGATE+VEC VECTOR2)))
            (T (SETQ VEC2 VECTOR2)))
      (PROG (Z)
        (SETQ Z 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (GET+VEC+DIM VECTOR1) Z)) (RETURN NIL)))
        (SETQ SUM
                (ADDSQ SUM
                       (MULTSQ (GET+VEC+ENTRY VECTOR1 Z)
                               (GET+VEC+ENTRY VEC2 Z))))
        (SETQ Z (PLUS2 Z 1))
        (GO LAB))
      ((LAMBDA (*SUB2) (SETQ SUM (SUBS2 SUM))) T)
      (RETURN SUM))) 
(PUT 'MK+SCAL+MULT+VEC 'NUMBER-OF-ARGS 2) 
(PUT 'MK+SCAL+MULT+VEC 'DEFINED-ON-LINE '284) 
(PUT 'MK+SCAL+MULT+VEC 'DEFINED-IN-FILE 'LINALG/GRAMSCHM.RED) 
(PUT 'MK+SCAL+MULT+VEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+SCAL+MULT+VEC (SCAL1 VECTOR1)
    (PROG (ENTRY RES H)
      (SETQ RES
              (PROG (ENTRY FORALL-RESULT FORALL-ENDPTR)
                (SETQ ENTRY VECTOR1)
                (COND ((NULL ENTRY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ENTRY)
                                    (PROGN
                                     (SETQ H (MULTSQ SCAL1 ENTRY))
                                     ((LAMBDA (*SUB2) (SETQ H (SUBS2 H))) T)
                                     H))
                                  (CAR ENTRY))
                                 NIL)))
               LOOPLABEL
                (SETQ ENTRY (CDR ENTRY))
                (COND ((NULL ENTRY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ENTRY)
                            (PROGN
                             (SETQ H (MULTSQ SCAL1 ENTRY))
                             ((LAMBDA (*SUB2) (SETQ H (SUBS2 H))) T)
                             H))
                          (CAR ENTRY))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(ENDMODULE) 