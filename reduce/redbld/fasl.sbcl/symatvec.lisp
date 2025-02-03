(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SYMATVEC)) 
(PUT 'GEN+CAN+BAS 'NUMBER-OF-ARGS 1) 
(PUT 'GEN+CAN+BAS 'DEFINED-ON-LINE '47) 
(PUT 'GEN+CAN+BAS 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GEN+CAN+BAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GEN+CAN+BAS (DIMENSION)
    (PROG (EINS NULLSQ I J LL)
      (SETQ EINS (CONS 1 1))
      (SETQ NULLSQ (CONS NIL 1))
      (SETQ LL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE DIMENSION I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ J 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMENSION J))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (COND ((EQUAL I J) EINS)
                                                          (T NULLSQ))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ J (PLUS2 J 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMENSION J))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (COND ((EQUAL I J) EINS)
                                                  (T NULLSQ))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE DIMENSION I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (J FORALL-RESULT FORALL-ENDPTR)
                           (SETQ J 1)
                           (COND
                            ((MINUSP (DIFFERENCE DIMENSION J)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (COND ((EQUAL I J) EINS)
                                                  (T NULLSQ))
                                            NIL)))
                          LOOPLABEL
                           (SETQ J (PLUS2 J 1))
                           (COND
                            ((MINUSP (DIFFERENCE DIMENSION J))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS (COND ((EQUAL I J) EINS) (T NULLSQ))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN LL))) 
(PUT 'ALG+MATRIX+P 'NUMBER-OF-ARGS 1) 
(PUT 'ALG+MATRIX+P 'DEFINED-ON-LINE '65) 
(PUT 'ALG+MATRIX+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'ALG+MATRIX+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALG+MATRIX+P (MAT1)
    (PROG (LEN ELEM)
      (COND ((LESSP (LENGTH MAT1) 1) (REDERR "should be a matrix")))
      (COND ((NOT (EQUAL (CAR MAT1) 'MAT)) (REDERR "should be a matrix")))
      (SETQ MAT1 (CDR MAT1))
      (COND ((LESSP (LENGTH MAT1) 1) (REDERR "should be a matrix")))
      (SETQ LEN (LENGTH (CAR MAT1)))
      (PROG (ELEM)
        (SETQ ELEM (CDR MAT1))
       LAB
        (COND ((NULL ELEM) (RETURN NIL)))
        ((LAMBDA (ELEM)
           (COND
            ((NOT (EQUAL (LENGTH ELEM) LEN)) (REDERR "should be a matrix"))))
         (CAR ELEM))
        (SETQ ELEM (CDR ELEM))
        (GO LAB))
      (RETURN T))) 
(PUT 'MATRIX+P 'NUMBER-OF-ARGS 1) 
(PUT 'MATRIX+P 'DEFINED-ON-LINE '79) 
(PUT 'MATRIX+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MATRIX+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MATRIX+P (MAT1)
    (PROG (DIMENSION Z RES)
      (COND ((LESSP (LENGTH MAT1) 1) (RETURN NIL)))
      (SETQ DIMENSION (LENGTH (CAR MAT1)))
      (SETQ RES T)
      (PROG (Z)
        (SETQ Z (CDR MAT1))
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND ((NOT (EQUAL DIMENSION (LENGTH Z))) (SETQ RES NIL))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN RES))) 
(PUT 'SQUARED+MATRIX+P 'NUMBER-OF-ARGS 1) 
(PUT 'SQUARED+MATRIX+P 'DEFINED-ON-LINE '91) 
(PUT 'SQUARED+MATRIX+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'SQUARED+MATRIX+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQUARED+MATRIX+P (MAT1)
    (PROG ()
      (COND
       ((AND (MATRIX+P MAT1) (EQUAL (GET+ROW+NR MAT1) (GET+COL+NR MAT1)))
        (RETURN T))))) 
(PUT 'EQUAL+MATRICES+P 'NUMBER-OF-ARGS 2) 
(PUT 'EQUAL+MATRICES+P 'DEFINED-ON-LINE '98) 
(PUT 'EQUAL+MATRICES+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'EQUAL+MATRICES+P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUAL+MATRICES+P (MAT1 MAT2)
    (PROG (S Z HELPP MATHELP SUM RULESUM RULE1 RULE2)
      (COND
       ((SAME+DIM+SQUARED+P MAT1 MAT2)
        (PROGN
         (SETQ MATHELP
                 (MK+MAT+PLUS+MAT MAT1
                  (MK+SCAL+MULT+MAT (CONS (MINUS 1) 1) MAT2)))
         (SETQ SUM (CONS NIL 1))
         (PROG (Z)
           (SETQ Z MATHELP)
          LAB
           (COND ((NULL Z) (RETURN NIL)))
           ((LAMBDA (Z)
              (PROG (S)
                (SETQ S Z)
               LAB
                (COND ((NULL S) (RETURN NIL)))
                ((LAMBDA (S)
                   (COND
                    (*COMPLEX
                     (SETQ SUM (ADDSQ SUM (MULTSQ S (MK+CONJUGATE+SQ S)))))
                    (T (SETQ SUM (ADDSQ SUM (MULTSQ S S))))))
                 (CAR S))
                (SETQ S (CDR S))
                (GO LAB)))
            (CAR Z))
           (SETQ Z (CDR Z))
           (GO LAB))
         (SETQ RULESUM (CHANGE+SQ+TO+ALGNULL SUM))
         (COND ((EQUAL RULESUM 0) (SETQ HELPP T)) (T (SETQ HELPP NIL)))
         NIL))
       (T (SETQ HELPP NIL)))
      (RETURN HELPP))) 
(PUT 'GET+ROW+NR 'NUMBER-OF-ARGS 1) 
(PUT 'GET+ROW+NR 'DEFINED-ON-LINE '123) 
(PUT 'GET+ROW+NR 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GET+ROW+NR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET+ROW+NR (MAT1) (PROG () (RETURN (LENGTH MAT1)))) 
(PUT 'GET+COL+NR 'NUMBER-OF-ARGS 1) 
(PUT 'GET+COL+NR 'DEFINED-ON-LINE '129) 
(PUT 'GET+COL+NR 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GET+COL+NR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET+COL+NR (MAT1) (PROG () (RETURN (LENGTH (CAR MAT1))))) 
(PUT 'GET+MAT+ENTRY 'NUMBER-OF-ARGS 3) 
(PUT 'GET+MAT+ENTRY 'DEFINED-ON-LINE '135) 
(PUT 'GET+MAT+ENTRY 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GET+MAT+ENTRY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GET+MAT+ENTRY (MAT1 Z S) (PROG () (RETURN (NTH (NTH MAT1 Z) S)))) 
(PUT 'SAME+DIM+SQUARED+P 'NUMBER-OF-ARGS 2) 
(PUT 'SAME+DIM+SQUARED+P 'DEFINED-ON-LINE '141) 
(PUT 'SAME+DIM+SQUARED+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'SAME+DIM+SQUARED+P 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SAME+DIM+SQUARED+P (MAT1 MAT2)
    (PROG ()
      (COND
       ((AND (SQUARED+MATRIX+P MAT1) (SQUARED+MATRIX+P MAT2)
             (EQUAL (GET+ROW+NR MAT1) (GET+ROW+NR MAT1)))
        (RETURN T))))) 
(PUT 'MK+TRANSPOSE+MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'MK+TRANSPOSE+MATRIX 'DEFINED-ON-LINE '151) 
(PUT 'MK+TRANSPOSE+MATRIX 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+TRANSPOSE+MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+TRANSPOSE+MATRIX (MAT1)
    (PROG (Z S TPMAT1)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in transpose")))
      (SETQ TPMAT1
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ S 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) S))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (GET+MAT+ENTRY MAT1 S Z)
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ S (PLUS2 S 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) S))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS (GET+MAT+ENTRY MAT1 S Z) NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND
                 ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) Z))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S 1)
                           (COND
                            ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) S))
                             (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS (GET+MAT+ENTRY MAT1 S Z)
                                                 NIL)))
                          LOOPLABEL
                           (SETQ S (PLUS2 S 1))
                           (COND
                            ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) S))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS (GET+MAT+ENTRY MAT1 S Z) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN TPMAT1))) 
(PUT 'MK+CONJUGATE+MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'MK+CONJUGATE+MATRIX 'DEFINED-ON-LINE '162) 
(PUT 'MK+CONJUGATE+MATRIX 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+CONJUGATE+MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+CONJUGATE+MATRIX (MAT1)
    (PROG (Z S TPMAT1)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in conjugate matrix")))
      (SETQ TPMAT1
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ S 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) S))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (MK+CONJUGATE+SQ
                                                     (GET+MAT+ENTRY MAT1 Z S))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ S (PLUS2 S 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) S))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (MK+CONJUGATE+SQ
                                             (GET+MAT+ENTRY MAT1 Z S))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND
                 ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) Z))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S 1)
                           (COND
                            ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) S))
                             (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (MK+CONJUGATE+SQ
                                             (GET+MAT+ENTRY MAT1 Z S))
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (PLUS2 S 1))
                           (COND
                            ((MINUSP (DIFFERENCE (GET+COL+NR MAT1) S))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (MK+CONJUGATE+SQ (GET+MAT+ENTRY MAT1 Z S))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN TPMAT1))) 
(PUT 'MK+HERMITEAN+MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'MK+HERMITEAN+MATRIX 'DEFINED-ON-LINE '173) 
(PUT 'MK+HERMITEAN+MATRIX 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+HERMITEAN+MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+HERMITEAN+MATRIX (MAT1)
    (PROG ()
      (COND
       (*COMPLEX (RETURN (MK+CONJUGATE+MATRIX (MK+TRANSPOSE+MATRIX MAT1))))
       (T (RETURN (MK+TRANSPOSE+MATRIX MAT1)))))) 
(PUT 'UNITARIAN+P 'NUMBER-OF-ARGS 1) 
(PUT 'UNITARIAN+P 'DEFINED-ON-LINE '181) 
(PUT 'UNITARIAN+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'UNITARIAN+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNITARIAN+P (MAT1)
    (PROG (MATHERMIT UNITMAT1)
      (SETQ MATHERMIT (MK+MAT+MULT+MAT (MK+HERMITEAN+MATRIX MAT1) MAT1))
      (SETQ UNITMAT1 (MK+UNIT+MAT (GET+ROW+NR MAT1)))
      (COND ((EQUAL+MATRICES+P MATHERMIT UNITMAT1) (RETURN T))))) 
(PUT 'MK+MAT+MULT+MAT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+MAT+MULT+MAT 'DEFINED-ON-LINE '190) 
(PUT 'MK+MAT+MULT+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+MAT+MULT+MAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+MAT+MULT+MAT (MAT1 MAT2)
    (PROG (DIMS1 DIMZ1 DIMS2 S Z RES SUM K)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in mult")))
      (COND ((NOT (MATRIX+P MAT2)) (REDERR "no matrix in mult")))
      (SETQ DIMS1 (GET+COL+NR MAT1))
      (SETQ DIMZ1 (GET+ROW+NR MAT1))
      (SETQ DIMS2 (GET+COL+NR MAT2))
      (COND
       ((NOT (EQUAL DIMS1 (GET+ROW+NR MAT2)))
        (REDERR "matrices can not be multiplied")))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE DIMZ1 Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ S 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS2 S))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (PROGN
                                                     (SETQ SUM (CONS NIL 1))
                                                     (PROG (K)
                                                       (SETQ K 1)
                                                      LAB
                                                       (COND
                                                        ((MINUSP
                                                          (DIFFERENCE DIMS1 K))
                                                         (RETURN NIL)))
                                                       (SETQ SUM
                                                               (ADDSQ SUM
                                                                      (MULTSQ
                                                                       (GET+MAT+ENTRY
                                                                        MAT1 Z
                                                                        K)
                                                                       (GET+MAT+ENTRY
                                                                        MAT2 K
                                                                        S))))
                                                       (SETQ K (PLUS2 K 1))
                                                       (GO LAB))
                                                     ((LAMBDA (*SUB2)
                                                        (SETQ SUM (SUBS2 SUM)))
                                                      T)
                                                     SUM)
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ S (PLUS2 S 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS2 S))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ SUM (CONS NIL 1))
                                             (PROG (K)
                                               (SETQ K 1)
                                              LAB
                                               (COND
                                                ((MINUSP (DIFFERENCE DIMS1 K))
                                                 (RETURN NIL)))
                                               (SETQ SUM
                                                       (ADDSQ SUM
                                                              (MULTSQ
                                                               (GET+MAT+ENTRY
                                                                MAT1 Z K)
                                                               (GET+MAT+ENTRY
                                                                MAT2 K S))))
                                               (SETQ K (PLUS2 K 1))
                                               (GO LAB))
                                             ((LAMBDA (*SUB2)
                                                (SETQ SUM (SUBS2 SUM)))
                                              T)
                                             SUM)
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND ((MINUSP (DIFFERENCE DIMZ1 Z)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S 1)
                           (COND ((MINUSP (DIFFERENCE DIMS2 S)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ SUM (CONS NIL 1))
                                             (PROG (K)
                                               (SETQ K 1)
                                              LAB
                                               (COND
                                                ((MINUSP (DIFFERENCE DIMS1 K))
                                                 (RETURN NIL)))
                                               (SETQ SUM
                                                       (ADDSQ SUM
                                                              (MULTSQ
                                                               (GET+MAT+ENTRY
                                                                MAT1 Z K)
                                                               (GET+MAT+ENTRY
                                                                MAT2 K S))))
                                               (SETQ K (PLUS2 K 1))
                                               (GO LAB))
                                             ((LAMBDA (*SUB2)
                                                (SETQ SUM (SUBS2 SUM)))
                                              T)
                                             SUM)
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (PLUS2 S 1))
                           (COND
                            ((MINUSP (DIFFERENCE DIMS2 S))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (PROGN
                                     (SETQ SUM (CONS NIL 1))
                                     (PROG (K)
                                       (SETQ K 1)
                                      LAB
                                       (COND
                                        ((MINUSP (DIFFERENCE DIMS1 K))
                                         (RETURN NIL)))
                                       (SETQ SUM
                                               (ADDSQ SUM
                                                      (MULTSQ
                                                       (GET+MAT+ENTRY MAT1 Z K)
                                                       (GET+MAT+ENTRY MAT2 K
                                                        S))))
                                       (SETQ K (PLUS2 K 1))
                                       (GO LAB))
                                     ((LAMBDA (*SUB2) (SETQ SUM (SUBS2 SUM)))
                                      T)
                                     SUM)
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+MAT+PLUS+MAT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+MAT+PLUS+MAT 'DEFINED-ON-LINE '218) 
(PUT 'MK+MAT+PLUS+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+MAT+PLUS+MAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+MAT+PLUS+MAT (MAT1 MAT2)
    (PROG (DIMS DIMZ S Z RES SUM)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in add")))
      (COND ((NOT (MATRIX+P MAT2)) (REDERR "no matrix in add")))
      (SETQ DIMS (GET+COL+NR MAT1))
      (SETQ DIMZ (GET+ROW+NR MAT1))
      (COND
       ((NOT (EQUAL DIMS (GET+COL+NR MAT2)))
        (REDERR "wrong dimensions in add")))
      (COND
       ((NOT (EQUAL DIMZ (GET+ROW+NR MAT2)))
        (REDERR "wrong dimensions in add")))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ S 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS S))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    (PROGN
                                                     (SETQ SUM
                                                             (ADDSQ
                                                              (GET+MAT+ENTRY
                                                               MAT1 Z S)
                                                              (GET+MAT+ENTRY
                                                               MAT2 Z S)))
                                                     ((LAMBDA (*SUB2)
                                                        (SETQ SUM (SUBS2 SUM)))
                                                      T)
                                                     SUM)
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ S (PLUS2 S 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS S))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ SUM
                                                     (ADDSQ
                                                      (GET+MAT+ENTRY MAT1 Z S)
                                                      (GET+MAT+ENTRY MAT2 Z
                                                       S)))
                                             ((LAMBDA (*SUB2)
                                                (SETQ SUM (SUBS2 SUM)))
                                              T)
                                             SUM)
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S 1)
                           (COND ((MINUSP (DIFFERENCE DIMS S)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (PROGN
                                             (SETQ SUM
                                                     (ADDSQ
                                                      (GET+MAT+ENTRY MAT1 Z S)
                                                      (GET+MAT+ENTRY MAT2 Z
                                                       S)))
                                             ((LAMBDA (*SUB2)
                                                (SETQ SUM (SUBS2 SUM)))
                                              T)
                                             SUM)
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (PLUS2 S 1))
                           (COND
                            ((MINUSP (DIFFERENCE DIMS S))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (PROGN
                                     (SETQ SUM
                                             (ADDSQ (GET+MAT+ENTRY MAT1 Z S)
                                                    (GET+MAT+ENTRY MAT2 Z S)))
                                     ((LAMBDA (*SUB2) (SETQ SUM (SUBS2 SUM)))
                                      T)
                                     SUM)
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+MAT*MAT*MAT 'NUMBER-OF-ARGS 3) 
(PUT 'MK+MAT*MAT*MAT 'DEFINED-ON-LINE '243) 
(PUT 'MK+MAT*MAT*MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+MAT*MAT*MAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK+MAT*MAT*MAT (MAT1 MAT2 MAT3)
    (PROG (RES)
      (SETQ RES (MK+MAT+MULT+MAT MAT1 MAT2))
      (RETURN (MK+MAT+MULT+MAT RES MAT3)))) 
(PUT 'ADD+TWO+MATS 'NUMBER-OF-ARGS 2) 
(PUT 'ADD+TWO+MATS 'DEFINED-ON-LINE '251) 
(PUT 'ADD+TWO+MATS 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'ADD+TWO+MATS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADD+TWO+MATS (MAT1 MAT2)
    (PROG (DIMZ Z RES)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in add")))
      (COND ((NOT (MATRIX+P MAT2)) (REDERR "no matrix in add")))
      (SETQ DIMZ (GET+ROW+NR MAT1))
      (COND ((NOT (EQUAL DIMZ (GET+ROW+NR MAT2))) (REDERR "wrong dim in add")))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (APPEND (NTH MAT1 Z) (NTH MAT2 Z)) NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (APPEND (NTH MAT1 Z) (NTH MAT2 Z)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+SCAL+MULT+MAT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+SCAL+MULT+MAT 'DEFINED-ON-LINE '264) 
(PUT 'MK+SCAL+MULT+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+SCAL+MULT+MAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+SCAL+MULT+MAT (SCAL1 MAT1)
    (PROG (RES Z S PROD)
      (COND ((NOT (MATRIX+P MAT1)) (REDERR "no matrix in add")))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z MAT1)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z)
                                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ S Z)
                                      (COND ((NULL S) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (S)
                                                          (PROGN
                                                           (SETQ PROD
                                                                   (MULTSQ
                                                                    SCAL1 S))
                                                           ((LAMBDA (*SUB2)
                                                              (SETQ PROD
                                                                      (SUBS2
                                                                       PROD)))
                                                            T)
                                                           PROD))
                                                        (CAR S))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ S (CDR S))
                                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S)
                                                  (PROGN
                                                   (SETQ PROD (MULTSQ SCAL1 S))
                                                   ((LAMBDA (*SUB2)
                                                      (SETQ PROD (SUBS2 PROD)))
                                                    T)
                                                   PROD))
                                                (CAR S))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Z)
                            (PROG (S FORALL-RESULT FORALL-ENDPTR)
                              (SETQ S Z)
                              (COND ((NULL S) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S)
                                                  (PROGN
                                                   (SETQ PROD (MULTSQ SCAL1 S))
                                                   ((LAMBDA (*SUB2)
                                                      (SETQ PROD (SUBS2 PROD)))
                                                    T)
                                                   PROD))
                                                (CAR S))
                                               NIL)))
                             LOOPLABEL
                              (SETQ S (CDR S))
                              (COND ((NULL S) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (S)
                                          (PROGN
                                           (SETQ PROD (MULTSQ SCAL1 S))
                                           ((LAMBDA (*SUB2)
                                              (SETQ PROD (SUBS2 PROD)))
                                            T)
                                           PROD))
                                        (CAR S))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Z))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+TRACE 'NUMBER-OF-ARGS 1) 
(PUT 'MK+TRACE 'DEFINED-ON-LINE '279) 
(PUT 'MK+TRACE 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+TRACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+TRACE (MAT1)
    (PROG (SPURX S)
      (COND ((NOT (SQUARED+MATRIX+P MAT1)) (REDERR "no square matrix in add")))
      (SETQ SPURX (CONS NIL 1))
      (PROG (S)
        (SETQ S 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (GET+ROW+NR MAT1) S)) (RETURN NIL)))
        (SETQ SPURX (ADDSQ SPURX (GET+MAT+ENTRY MAT1 S S)))
        (SETQ S (PLUS2 S 1))
        (GO LAB))
      ((LAMBDA (*SUB2) (SETQ SPURX (SUBS2 SPURX))) T)
      (RETURN SPURX))) 
(PUT 'MK+BLOCK+DIAGONAL+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+BLOCK+DIAGONAL+MAT 'DEFINED-ON-LINE '292) 
(PUT 'MK+BLOCK+DIAGONAL+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+BLOCK+DIAGONAL+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+BLOCK+DIAGONAL+MAT (MATS)
    (PROG ()
      (COND ((LESSP (LENGTH MATS) 1) (REDERR "no list in mkdiagonalmats")))
      (COND ((EQUAL (LENGTH MATS) 1) (RETURN (CAR MATS)))
            (T
             (RETURN
              (FILL+ZEROS (CAR MATS) (MK+BLOCK+DIAGONAL+MAT (CDR MATS)))))))) 
(PUT 'FILL+ZEROS 'NUMBER-OF-ARGS 2) 
(PUT 'FILL+ZEROS 'DEFINED-ON-LINE '301) 
(PUT 'FILL+ZEROS 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'FILL+ZEROS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FILL+ZEROS (MAT1 MAT2)
    (PROG (NULLMAT1 NULLMAT2)
      (SETQ NULLMAT1 (MK+NULL+MAT (GET+ROW+NR MAT2) (GET+COL+NR MAT1)))
      (SETQ NULLMAT2 (MK+NULL+MAT (GET+ROW+NR MAT1) (GET+COL+NR MAT2)))
      (RETURN
       (APPEND (ADD+TWO+MATS MAT1 NULLMAT2) (ADD+TWO+MATS NULLMAT1 MAT2))))) 
(PUT 'MK+OUTER+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+OUTER+MAT 'DEFINED-ON-LINE '311) 
(PUT 'MK+OUTER+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+OUTER+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+OUTER+MAT (INNERMAT)
    (PROG (RES S Z)
      (COND ((NOT (MATRIX+P INNERMAT)) (REDERR "no matrix in mkoutermat")))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z INNERMAT)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z)
                                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ S Z)
                                      (COND ((NULL S) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (S) (PREPSQ S))
                                                        (CAR S))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ S (CDR S))
                                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (PREPSQ S))
                                                (CAR S))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Z)
                            (PROG (S FORALL-RESULT FORALL-ENDPTR)
                              (SETQ S Z)
                              (COND ((NULL S) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (PREPSQ S))
                                                (CAR S))
                                               NIL)))
                             LOOPLABEL
                              (SETQ S (CDR S))
                              (COND ((NULL S) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (S) (PREPSQ S)) (CAR S))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Z))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (APPEND (LIST 'MAT) RES)))) 
(PUT 'MK+INNER+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+INNER+MAT 'DEFINED-ON-LINE '322) 
(PUT 'MK+INNER+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+INNER+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+INNER+MAT (OUTERMAT)
    (PROG (RES S Z)
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z (CDR OUTERMAT))
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z)
                                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ S Z)
                                      (COND ((NULL S) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (S) (SIMP S))
                                                        (CAR S))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ S (CDR S))
                                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (SIMP S)) (CAR S))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Z)
                            (PROG (S FORALL-RESULT FORALL-ENDPTR)
                              (SETQ S Z)
                              (COND ((NULL S) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (SIMP S)) (CAR S))
                                               NIL)))
                             LOOPLABEL
                              (SETQ S (CDR S))
                              (COND ((NULL S) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (S) (SIMP S)) (CAR S))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Z))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((MATRIX+P RES) (RETURN RES))
            (T (REDERR "incorrect input in mkinnermat"))))) 
(PUT 'MK+RESIMP+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+RESIMP+MAT 'DEFINED-ON-LINE '333) 
(PUT 'MK+RESIMP+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+RESIMP+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+RESIMP+MAT (INNERMAT)
    (PROG (RES S Z)
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z INNERMAT)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z)
                                    (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ S Z)
                                      (COND ((NULL S) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (S) (RESIMP S))
                                                        (CAR S))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ S (CDR S))
                                      (COND ((NULL S) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (RESIMP S))
                                                (CAR S))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Z)
                            (PROG (S FORALL-RESULT FORALL-ENDPTR)
                              (SETQ S Z)
                              (COND ((NULL S) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (S) (RESIMP S))
                                                (CAR S))
                                               NIL)))
                             LOOPLABEL
                              (SETQ S (CDR S))
                              (COND ((NULL S) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS ((LAMBDA (S) (RESIMP S)) (CAR S))
                                            NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Z))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+NULL+MAT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+NULL+MAT 'DEFINED-ON-LINE '343) 
(PUT 'MK+NULL+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+NULL+MAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+NULL+MAT (DIMZ DIMS)
    (PROG (NULLSQ S Z RES)
      (SETQ NULLSQ (CONS NIL 1))
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z 1)
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ S 1)
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS S))
                                     (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS NULLSQ NIL)))
                                  LOOPLABEL
                                   (SETQ S (PLUS2 S 1))
                                   (COND
                                    ((MINUSP (DIFFERENCE DIMS S))
                                     (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR (CONS NULLSQ NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (PLUS2 Z 1))
                (COND ((MINUSP (DIFFERENCE DIMZ Z)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S 1)
                           (COND ((MINUSP (DIFFERENCE DIMS S)) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR (CONS NULLSQ NIL)))
                          LOOPLABEL
                           (SETQ S (PLUS2 S 1))
                           (COND
                            ((MINUSP (DIFFERENCE DIMS S))
                             (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR (CONS NULLSQ NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'MK+UNIT+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+UNIT+MAT 'DEFINED-ON-LINE '353) 
(PUT 'MK+UNIT+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+UNIT+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+UNIT+MAT (DIMENSION) (PROG () (RETURN (GEN+CAN+BAS DIMENSION)))) 
(PUT 'VECTOR+P 'NUMBER-OF-ARGS 1) 
(PUT 'VECTOR+P 'DEFINED-ON-LINE '365) 
(PUT 'VECTOR+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'VECTOR+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VECTOR+P (VECTOR1)
    (PROG () (COND ((GREATERP (LENGTH VECTOR1) 0) (RETURN T))))) 
(PUT 'GET+VEC+DIM 'NUMBER-OF-ARGS 1) 
(PUT 'GET+VEC+DIM 'DEFINED-ON-LINE '372) 
(PUT 'GET+VEC+DIM 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GET+VEC+DIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET+VEC+DIM (VECTOR1) (PROG () (RETURN (LENGTH VECTOR1)))) 
(PUT 'GET+VEC+ENTRY 'NUMBER-OF-ARGS 2) 
(PUT 'GET+VEC+ENTRY 'DEFINED-ON-LINE '379) 
(PUT 'GET+VEC+ENTRY 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GET+VEC+ENTRY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GET+VEC+ENTRY (VECTOR1 ELEM) (PROG () (RETURN (NTH VECTOR1 ELEM)))) 
(PUT 'MK+MAT+MULT+VEC 'NUMBER-OF-ARGS 2) 
(PUT 'MK+MAT+MULT+VEC 'DEFINED-ON-LINE '386) 
(PUT 'MK+MAT+MULT+VEC 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+MAT+MULT+VEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+MAT+MULT+VEC (MAT1 VECTOR1)
    (PROG (Z)
      (RETURN
       (PROG (Z FORALL-RESULT FORALL-ENDPTR)
         (SETQ Z MAT1)
         (COND ((NULL Z) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (Z) (MK+REAL+INNER+PRODUCT Z VECTOR1))
                           (CAR Z))
                          NIL)))
        LOOPLABEL
         (SETQ Z (CDR Z))
         (COND ((NULL Z) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (Z) (MK+REAL+INNER+PRODUCT Z VECTOR1)) (CAR Z))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MK+SCAL+MULT+VEC 'NUMBER-OF-ARGS 2) 
(PUT 'MK+SCAL+MULT+VEC 'DEFINED-ON-LINE '394) 
(PUT 'MK+SCAL+MULT+VEC 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
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
(PUT 'MK+VEC+ADD+VEC 'NUMBER-OF-ARGS 2) 
(PUT 'MK+VEC+ADD+VEC 'DEFINED-ON-LINE '407) 
(PUT 'MK+VEC+ADD+VEC 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
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
(PUT 'MK+SQUARED+NORM 'DEFINED-ON-LINE '421) 
(PUT 'MK+SQUARED+NORM 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+SQUARED+NORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+SQUARED+NORM (VECTOR1)
    (PROG () (RETURN (MK+INNER+PRODUCT VECTOR1 VECTOR1)))) 
(PUT 'MY+NULLSQ+P 'NUMBER-OF-ARGS 1) 
(PUT 'MY+NULLSQ+P 'DEFINED-ON-LINE '427) 
(PUT 'MY+NULLSQ+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MY+NULLSQ+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MY+NULLSQ+P (SCAL) (PROG () (COND ((NULL (CAR SCAL)) (RETURN T))))) 
(PUT 'MK+NULL+VEC 'NUMBER-OF-ARGS 1) 
(PUT 'MK+NULL+VEC 'DEFINED-ON-LINE '433) 
(PUT 'MK+NULL+VEC 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
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
(PUT 'MK+CONJUGATE+VEC 'NUMBER-OF-ARGS 1) 
(PUT 'MK+CONJUGATE+VEC 'DEFINED-ON-LINE '442) 
(PUT 'MK+CONJUGATE+VEC 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+CONJUGATE+VEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+CONJUGATE+VEC (VECTOR1)
    (PROG (Z RES)
      (SETQ RES
              (PROG (Z FORALL-RESULT FORALL-ENDPTR)
                (SETQ Z VECTOR1)
                (COND ((NULL Z) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Z) (MK+CONJUGATE+SQ Z)) (CAR Z))
                                 NIL)))
               LOOPLABEL
                (SETQ Z (CDR Z))
                (COND ((NULL Z) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Z) (MK+CONJUGATE+SQ Z)) (CAR Z)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RES))) 
(PUT 'NULL+VEC+P 'NUMBER-OF-ARGS 1) 
(PUT 'NULL+VEC+P 'DEFINED-ON-LINE '450) 
(PUT 'NULL+VEC+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'NULL+VEC+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULL+VEC+P (VECTOR1)
    (PROG () (COND ((MY+NULLSQ+P (MK+SQUARED+NORM VECTOR1)) (RETURN T))))) 
(PUT 'MK+NORMALIZE+VECTOR 'NUMBER-OF-ARGS 1) 
(PUT 'MK+NORMALIZE+VECTOR 'DEFINED-ON-LINE '457) 
(PUT 'MK+NORMALIZE+VECTOR 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
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
(PUT 'MK+INNER+PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+INNER+PRODUCT 'DEFINED-ON-LINE '472) 
(PUT 'MK+INNER+PRODUCT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+INNER+PRODUCT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+INNER+PRODUCT (VECTOR1 VECTOR2)
    (PROG (Z SUM VEC2)
      (COND
       ((NOT (EQUAL (GET+VEC+DIM VECTOR1) (GET+VEC+DIM VECTOR2)))
        (REDERR "wrong dimensions in innerproduct")))
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
(PUT 'MK+REAL+INNER+PRODUCT 'NUMBER-OF-ARGS 2) 
(PUT 'MK+REAL+INNER+PRODUCT 'DEFINED-ON-LINE '491) 
(PUT 'MK+REAL+INNER+PRODUCT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+REAL+INNER+PRODUCT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+REAL+INNER+PRODUCT (VECTOR1 VECTOR2)
    (PROG (Z SUM)
      (COND
       ((NOT (EQUAL (GET+VEC+DIM VECTOR1) (GET+VEC+DIM VECTOR2)))
        (REDERR "wrong dimensions in innerproduct")))
      (SETQ SUM (CONS NIL 1))
      (PROG (Z)
        (SETQ Z 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (GET+VEC+DIM VECTOR1) Z)) (RETURN NIL)))
        (SETQ SUM
                (ADDSQ SUM
                       (MULTSQ (GET+VEC+ENTRY VECTOR1 Z)
                               (GET+VEC+ENTRY VECTOR2 Z))))
        (SETQ Z (PLUS2 Z 1))
        (GO LAB))
      ((LAMBDA (*SUB2) (SETQ SUM (SUBS2 SUM))) T)
      (RETURN SUM))) 
(PUT 'MK+GRAM+SCHMID 'NUMBER-OF-ARGS 2) 
(PUT 'MK+GRAM+SCHMID 'DEFINED-ON-LINE '508) 
(PUT 'MK+GRAM+SCHMID 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+GRAM+SCHMID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+GRAM+SCHMID (VECTORLIST VECTOR1)
    (PROG (I ORTHOVEC SCALO VECTORS1)
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
      (COND ((NULL+VEC+P ORTHOVEC) (SETQ VECTORS1 VECTORLIST))
            (T (SETQ VECTORS1 (APPEND VECTORLIST (LIST ORTHOVEC)))))
      (RETURN VECTORS1))) 
(PUT 'GRAM+SCHMID 'NUMBER-OF-ARGS 1) 
(PUT 'GRAM+SCHMID 'DEFINED-ON-LINE '527) 
(PUT 'GRAM+SCHMID 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'GRAM+SCHMID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GRAM+SCHMID (VECTORLIST)
    (PROG (ORTHOLIST I)
      (COND ((LESSP (LENGTH VECTORLIST) 1) (REDERR "error in Gram Schmid")))
      (COND ((VECTOR+P (CAR VECTORLIST)) (SETQ ORTHOLIST NIL))
            (T (REDERR "strange in Gram-Schmid")))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE (LENGTH VECTORLIST) I)) (RETURN NIL)))
        (SETQ ORTHOLIST (MK+GRAM+SCHMID ORTHOLIST (NTH VECTORLIST I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ORTHOLIST))) 
(PUT 'MK+INTERNAL+MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MK+INTERNAL+MAT 'DEFINED-ON-LINE '550) 
(PUT 'MK+INTERNAL+MAT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+INTERNAL+MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+INTERNAL+MAT (VECTORLIST)
    (PROG () (RETURN (MK+TRANSPOSE+MATRIX VECTORLIST)))) 
(PUT 'MAT+VECLIST 'NUMBER-OF-ARGS 1) 
(PUT 'MAT+VECLIST 'DEFINED-ON-LINE '558) 
(PUT 'MAT+VECLIST 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MAT+VECLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAT+VECLIST (MAT1) (PROG () (RETURN (MK+TRANSPOSE+MATRIX MAT1)))) 
(PUT 'CHANGE+SQ+TO+INT 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE+SQ+TO+INT 'DEFINED-ON-LINE '571) 
(PUT 'CHANGE+SQ+TO+INT 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'CHANGE+SQ+TO+INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE+SQ+TO+INT (SCAL1)
    (PROG (NR)
      (SETQ NR (SIMP* (PREPSQ SCAL1)))
      (COND ((EQUAL (CDR NR) 1) (RETURN (CAR NR)))
            (T (REDERR "no integer in change!+sq!+to!+int"))))) 
(PUT 'CHANGE+INT+TO+SQ 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE+INT+TO+SQ 'DEFINED-ON-LINE '581) 
(PUT 'CHANGE+INT+TO+SQ 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'CHANGE+INT+TO+SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE+INT+TO+SQ (SCAL1) (PROG () (RETURN (CONS SCAL1 1)))) 
(PUT 'CHANGE+SQ+TO+ALGNULL 'NUMBER-OF-ARGS 1) 
(PUT 'CHANGE+SQ+TO+ALGNULL 'DEFINED-ON-LINE '588) 
(PUT 'CHANGE+SQ+TO+ALGNULL 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'CHANGE+SQ+TO+ALGNULL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHANGE+SQ+TO+ALGNULL (SCAL1)
    (PROG (RULESUM STORECOMP)
      (COND (*COMPLEX (PROGN (SETQ STORECOMP T) (OFF (LIST 'COMPLEX)) NIL))
            (T (PROGN (SETQ STORECOMP NIL) NIL)))
      (SETQ RULESUM
              (EVALWHEREEXP
               (LIST
                '(LIST
                  (LIST
                   (REPLACEBY (COS (~ X))
                    (TIMES (QUOTIENT 1 2)
                           (PLUS (EXPT E (TIMES I (~ X)))
                                 (EXPT E (MINUS (TIMES I (~ X)))))))
                   (REPLACEBY (SIN (~ X))
                    (TIMES (QUOTIENT 1 (TIMES 2 I))
                           (DIFFERENCE (EXPT E (TIMES I (~ X)))
                                       (EXPT E (MINUS (TIMES I (~ X)))))))))
                (PREPSQ SCAL1))))
      (SETQ RULESUM (REVAL1 RULESUM T))
      (COND (STORECOMP (ON (LIST 'COMPLEX))))
      (RETURN RULESUM))) 
(PUT 'MK+CONJUGATE+SQ 'NUMBER-OF-ARGS 1) 
(PUT 'MK+CONJUGATE+SQ 'DEFINED-ON-LINE '620) 
(PUT 'MK+CONJUGATE+SQ 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+CONJUGATE+SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+CONJUGATE+SQ (MYSQ) (PROG () (RETURN (CONJSQ MYSQ)))) 
(PUT 'MK+EQUATION 'NUMBER-OF-ARGS 2) 
(PUT 'MK+EQUATION 'DEFINED-ON-LINE '626) 
(PUT 'MK+EQUATION 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+EQUATION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MK+EQUATION (ARG1 ARG2) (PROG () (RETURN (LIST 'EQUAL ARG1 ARG2)))) 
(PUT 'OUTER+EQUATION+P 'NUMBER-OF-ARGS 1) 
(PUT 'OUTER+EQUATION+P 'DEFINED-ON-LINE '631) 
(PUT 'OUTER+EQUATION+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'OUTER+EQUATION+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTER+EQUATION+P (OUTERLIST)
    (PROG () (COND ((EQCAR OUTERLIST 'EQUAL) (RETURN T))))) 
(PUT 'MK+OUTER+LIST 'NUMBER-OF-ARGS 1) 
(PUT 'MK+OUTER+LIST 'DEFINED-ON-LINE '636) 
(PUT 'MK+OUTER+LIST 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+OUTER+LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+OUTER+LIST (INNERLIST)
    (PROG () (RETURN (APPEND (LIST 'LIST) INNERLIST)))) 
(PUT 'MK+INNER+LIST 'NUMBER-OF-ARGS 1) 
(PUT 'MK+INNER+LIST 'DEFINED-ON-LINE '641) 
(PUT 'MK+INNER+LIST 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'MK+INNER+LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK+INNER+LIST (OUTERLIST)
    (PROG () (COND ((OUTER+LIST+P OUTERLIST) (RETURN (CDR OUTERLIST)))))) 
(PUT 'OUTER+LIST+P 'NUMBER-OF-ARGS 1) 
(PUT 'OUTER+LIST+P 'DEFINED-ON-LINE '646) 
(PUT 'OUTER+LIST+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'OUTER+LIST+P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTER+LIST+P (OUTERLIST)
    (PROG () (COND ((EQCAR OUTERLIST 'LIST) (RETURN T))))) 
(PUT 'EQUAL+LISTS+P 'NUMBER-OF-ARGS 2) 
(PUT 'EQUAL+LISTS+P 'DEFINED-ON-LINE '651) 
(PUT 'EQUAL+LISTS+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'EQUAL+LISTS+P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUAL+LISTS+P (LL1 LL2)
    (PROG () (RETURN (AND (LIST+IN+LIST+P LL1 LL2) (LIST+IN+LIST+P LL2 LL1))))) 
(PUT 'LIST+IN+LIST+P 'NUMBER-OF-ARGS 2) 
(PUT 'LIST+IN+LIST+P 'DEFINED-ON-LINE '656) 
(PUT 'LIST+IN+LIST+P 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'LIST+IN+LIST+P 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIST+IN+LIST+P (LL1 LL2)
    (PROG ()
      (COND ((EQUAL (LENGTH LL1) 0) (RETURN T))
            (T
             (RETURN
              (AND (MEMQ (CAR LL1) LL2) (LIST+IN+LIST+P (CDR LL1) LL2))))))) 
(PUT 'PRINT-MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT-MATRIX 'DEFINED-ON-LINE '662) 
(PUT 'PRINT-MATRIX 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'PRINT-MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT-MATRIX (MAT1)
    (PROG () (WRITEPRI (MKQUOTE (MK+OUTER+MAT MAT1)) 'ONLY))) 
(PUT 'PRINT-SQ 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT-SQ 'DEFINED-ON-LINE '667) 
(PUT 'PRINT-SQ 'DEFINED-IN-FILE 'SYMMETRY/SYMATVEC.RED) 
(PUT 'PRINT-SQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT-SQ (MYSQ) (PROG () (WRITEPRI (MKQUOTE (PREPSQ MYSQ)) 'ONLY))) 
(ENDMODULE) 