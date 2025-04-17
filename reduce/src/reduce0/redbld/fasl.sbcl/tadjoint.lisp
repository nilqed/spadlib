(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TADJOINT)) 
(DE MKSQ*MAT (IN_MAT)
    (PROG (TMP_MAT OUT_MAT)
      (SETQ TMP_MAT (CDR IN_MAT))
      (SETQ OUT_MAT
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U TMP_MAT)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ V U)
                                      (COND ((NULL V) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (V)
                                                          (COND ((ATOM V) V)
                                                                (T (MK*SQ V))))
                                                        (CAR V))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ V (CDR V))
                                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (V)
                                                  (COND ((ATOM V) V)
                                                        (T (MK*SQ V))))
                                                (CAR V))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (PROG (V FORALL-RESULT FORALL-ENDPTR)
                              (SETQ V U)
                              (COND ((NULL V) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (V)
                                                  (COND ((ATOM V) V)
                                                        (T (MK*SQ V))))
                                                (CAR V))
                                               NIL)))
                             LOOPLABEL
                              (SETQ V (CDR V))
                              (COND ((NULL V) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (V)
                                          (COND ((ATOM V) V) (T (MK*SQ V))))
                                        (CAR V))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'MAT OUT_MAT)))) 
(PUT 'MKSQ*MAT 'NUMBER-OF-ARGS 1) 
(PUT 'MKSQ*MAT 'DEFINED-ON-LINE '42) 
(PUT 'MKSQ*MAT 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'MKSQ*MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'MKSQ*MAT 'INLINE
      '(LAMBDA (IN_MAT)
         (PROG (TMP_MAT OUT_MAT)
           (SETQ TMP_MAT (CDR IN_MAT))
           (SETQ OUT_MAT
                   (PROG (U FORALL-RESULT FORALL-ENDPTR)
                     (SETQ U TMP_MAT)
                     (COND ((NULL U) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (U)
                                         (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ V U)
                                           (COND ((NULL V) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (V)
                                                               (COND
                                                                ((ATOM V) V)
                                                                (T (MK*SQ V))))
                                                             (CAR V))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ V (CDR V))
                                           (COND
                                            ((NULL V) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (COND ((ATOM V) V)
                                                             (T (MK*SQ V))))
                                                     (CAR V))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (CAR U))
                                      NIL)))
                    LOOPLABEL
                     (SETQ U (CDR U))
                     (COND ((NULL U) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (U)
                                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ V U)
                                   (COND ((NULL V) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (COND ((ATOM V) V)
                                                             (T (MK*SQ V))))
                                                     (CAR V))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ V (CDR V))
                                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V)
                                               (COND ((ATOM V) V)
                                                     (T (MK*SQ V))))
                                             (CAR V))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR U))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN (CONS 'MAT OUT_MAT))))) 
(DE REVAL*MAT (IN_MAT)
    (PROG (TMP_MAT OUT_MAT)
      (SETQ TMP_MAT (CDR IN_MAT))
      (SETQ OUT_MAT
              (PROG (U FORALL-RESULT FORALL-ENDPTR)
                (SETQ U TMP_MAT)
                (COND ((NULL U) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (U)
                                    (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ V U)
                                      (COND ((NULL V) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (V)
                                                          (COND ((FIXP V) V)
                                                                (T
                                                                 (REVAL1 V
                                                                         T))))
                                                        (CAR V))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ V (CDR V))
                                      (COND ((NULL V) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (V)
                                                  (COND ((FIXP V) V)
                                                        (T (REVAL1 V T))))
                                                (CAR V))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR U))
                                 NIL)))
               LOOPLABEL
                (SETQ U (CDR U))
                (COND ((NULL U) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (U)
                            (PROG (V FORALL-RESULT FORALL-ENDPTR)
                              (SETQ V U)
                              (COND ((NULL V) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (V)
                                                  (COND ((FIXP V) V)
                                                        (T (REVAL1 V T))))
                                                (CAR V))
                                               NIL)))
                             LOOPLABEL
                              (SETQ V (CDR V))
                              (COND ((NULL V) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (V)
                                          (COND ((FIXP V) V) (T (REVAL1 V T))))
                                        (CAR V))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR U))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'MAT OUT_MAT)))) 
(PUT 'REVAL*MAT 'NUMBER-OF-ARGS 1) 
(PUT 'REVAL*MAT 'DEFINED-ON-LINE '56) 
(PUT 'REVAL*MAT 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'REVAL*MAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'REVAL*MAT 'INLINE
      '(LAMBDA (IN_MAT)
         (PROG (TMP_MAT OUT_MAT)
           (SETQ TMP_MAT (CDR IN_MAT))
           (SETQ OUT_MAT
                   (PROG (U FORALL-RESULT FORALL-ENDPTR)
                     (SETQ U TMP_MAT)
                     (COND ((NULL U) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (U)
                                         (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ V U)
                                           (COND ((NULL V) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (V)
                                                               (COND
                                                                ((FIXP V) V)
                                                                (T
                                                                 (REVAL1 V
                                                                         T))))
                                                             (CAR V))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ V (CDR V))
                                           (COND
                                            ((NULL V) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (COND ((FIXP V) V)
                                                             (T (REVAL1 V T))))
                                                     (CAR V))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                       (CAR U))
                                      NIL)))
                    LOOPLABEL
                     (SETQ U (CDR U))
                     (COND ((NULL U) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (U)
                                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ V U)
                                   (COND ((NULL V) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (COND ((FIXP V) V)
                                                             (T (REVAL1 V T))))
                                                     (CAR V))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ V (CDR V))
                                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V)
                                               (COND ((FIXP V) V)
                                                     (T (REVAL1 V T))))
                                             (CAR V))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR U))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (RETURN (CONS 'MAT OUT_MAT))))) 
(PUT 'TRIANG_ADJOINT 'NUMBER-OF-ARGS 1) 
(PUT 'TRIANG_ADJOINT 'DEFINED-ON-LINE '69) 
(PUT 'TRIANG_ADJOINT 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'TRIANG_ADJOINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRIANG_ADJOINT (IN_MAT)
    (PROG (U)
      (COND ((EQCAR (SETQ U (REVAL1 IN_MAT NIL)) 'MATRIX) (SETQ U (CADR U)))
            (T (SETQ U (REVAL1 IN_MAT T))))
      (COND
       ((NOT (MATRIXP U))
        (REDERR "Error in triang_adjoint: non matrix input."))
       ((NOT (SQUAREP U))
        (REDERR "Error in triang_adjoint: input matrix should be square."))
       (T (RETURN (MATSM (ADTRIANG* U))))))) 
(PUT 'TRIANG_ADJOINT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'ADTRIANG* 'NUMBER-OF-ARGS 1) 
(PUT 'ADTRIANG* 'DEFINED-ON-LINE '88) 
(PUT 'ADTRIANG* 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'ADTRIANG* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADTRIANG* (IN_MAT)
    ((LAMBDA (*FAST_LA)
       (PROG (MAT_DIM TMP_MAT L BASE DIM)
         (SETQ DIM 0)
         (ON (LIST 'FAST_LA))
         (SETQ DIM (CADR (MATLENGTH IN_MAT)))
         (SETQ BASE (LOGB DIM 2))
         (SETQ MAT_DIM
                 (COND ((EQUAL (DIFFERENCE BASE (FLOOR BASE)) 0) (FIX BASE))
                       (T (PLUS (FLOOR BASE) 1))))
         (SETQ MAT_DIM (EXPT 2 MAT_DIM))
         (COND
          ((GREATERP MAT_DIM DIM)
           (SETQ TMP_MAT
                   (EXTEND IN_MAT (DIFFERENCE MAT_DIM DIM)
                           (DIFFERENCE MAT_DIM DIM) 0)))
          (T (SETQ TMP_MAT IN_MAT)))
         (SETQ TMP_MAT (ADJOINT*LU TMP_MAT))
         (SETQ L
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                   (COND ((MINUSP (DIFFERENCE DIM I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND ((MINUSP (DIFFERENCE DIM I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS I NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ TMP_MAT (SUB_MATRIX TMP_MAT L L))
         (RETURN TMP_MAT)))
     *FAST_LA)) 
(PUT 'ADJOINT*LU 'NUMBER-OF-ARGS 1) 
(PUT 'ADJOINT*LU 'DEFINED-ON-LINE '112) 
(PUT 'ADJOINT*LU 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'ADJOINT*LU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADJOINT*LU (IN_MAT)
    (PROG (A1 A_TMP A_TMP1 F1 A4* F4* SUBDIM0 SUBDIM1 L DETERMINANT DIM
           CRRNT_DIM)
      (SETQ DETERMINANT 0)
      (SETQ DIM 0)
      (SETQ CRRNT_DIM 0)
      (SETQ DIM (CADR (MATLENGTH IN_MAT)))
      (COND ((LESSP DIM 2) (RETURN (CONS 'MAT (LIST (LIST 1))))))
      (SETQ CRRNT_DIM 1)
      (SETQ F1 (LIST 'MAT (LIST 1)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LESSP CRRNT_DIM DIM)) (RETURN NIL)))
        (PROG ()
          (SETQ SUBDIM0
                  (PROG (I FORALL-RESULT FORALL-ENDPTR)
                    (SETQ I 1)
                    (COND ((MINUSP (DIFFERENCE CRRNT_DIM I)) (RETURN NIL)))
                    (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                   LOOPLABEL
                    (SETQ I (PLUS2 I 1))
                    (COND
                     ((MINUSP (DIFFERENCE CRRNT_DIM I))
                      (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (CONS I NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ SUBDIM1
                  (PROG (I FORALL-RESULT FORALL-ENDPTR)
                    (SETQ I (PLUS CRRNT_DIM 1))
                    (COND
                     ((MINUSP (DIFFERENCE (TIMES 2 CRRNT_DIM) I))
                      (RETURN NIL)))
                    (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                   LOOPLABEL
                    (SETQ I (PLUS2 I 1))
                    (COND
                     ((MINUSP (DIFFERENCE (TIMES 2 CRRNT_DIM) I))
                      (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (CONS I NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ A1 (SUB_MATRIX IN_MAT SUBDIM0 SUBDIM0))
          (SETQ A1
                  (PROG (TMP_MAT OUT_MAT)
                    (SETQ TMP_MAT (CDR A1))
                    (SETQ OUT_MAT
                            (PROG (U FORALL-RESULT FORALL-ENDPTR)
                              (SETQ U TMP_MAT)
                              (COND ((NULL U) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (U)
                                                  (PROG (V FORALL-RESULT
                                                         FORALL-ENDPTR)
                                                    (SETQ V U)
                                                    (COND
                                                     ((NULL V) (RETURN NIL)))
                                                    (SETQ FORALL-RESULT
                                                            (SETQ FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (V)
                                                                        (COND
                                                                         ((FIXP
                                                                           V)
                                                                          V)
                                                                         (T
                                                                          (REVAL1
                                                                           V
                                                                           T))))
                                                                      (CAR V))
                                                                     NIL)))
                                                   LOOPLABEL
                                                    (SETQ V (CDR V))
                                                    (COND
                                                     ((NULL V)
                                                      (RETURN FORALL-RESULT)))
                                                    (RPLACD FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (V)
                                                                (COND
                                                                 ((FIXP V) V)
                                                                 (T
                                                                  (REVAL1 V
                                                                          T))))
                                                              (CAR V))
                                                             NIL))
                                                    (SETQ FORALL-ENDPTR
                                                            (CDR
                                                             FORALL-ENDPTR))
                                                    (GO LOOPLABEL)))
                                                (CAR U))
                                               NIL)))
                             LOOPLABEL
                              (SETQ U (CDR U))
                              (COND ((NULL U) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (U)
                                          (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ V U)
                                            (COND ((NULL V) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (V)
                                                                (COND
                                                                 ((FIXP V) V)
                                                                 (T
                                                                  (REVAL1 V
                                                                          T))))
                                                              (CAR V))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ V (CDR V))
                                            (COND
                                             ((NULL V) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (V)
                                                        (COND ((FIXP V) V)
                                                              (T
                                                               (REVAL1 V T))))
                                                      (CAR V))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))
                                        (CAR U))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                    (RETURN (CONS 'MAT OUT_MAT))))
          (SETQ DETERMINANT 0)
          (PROG (J)
            (SETQ J 1)
           LAB
            (COND ((MINUSP (DIFFERENCE CRRNT_DIM J)) (RETURN NIL)))
            (SETQ DETERMINANT
                    (LIST 'PLUS
                          (COND ((FIXP DETERMINANT) DETERMINANT)
                                (T (REVAL1 DETERMINANT T)))
                          ((LAMBDA (N) (COND ((FIXP N) N) (T (REVAL1 N T))))
                           (LIST 'TIMES (GETMAT F1 CRRNT_DIM J)
                                 (GETMAT A1 J CRRNT_DIM)))))
            (SETQ J (PLUS2 J 1))
            (GO LAB))
          (COND
           ((EQUAL
             (COND ((FIXP DETERMINANT) DETERMINANT) (T (REVAL1 DETERMINANT T)))
             0)
            (PROGN
             (SETQ A_TMP (SUB_MATRIX IN_MAT (APPEND SUBDIM0 SUBDIM1) SUBDIM0))
             (COND
              ((LESSP (RANK-EVAL (LIST A_TMP)) CRRNT_DIM)
               (PROGN
                (SETQ F1 (EXTEND F1 CRRNT_DIM CRRNT_DIM 0))
                (SETQ CRRNT_DIM (TIMES 2 CRRNT_DIM))
                NIL))
              (T
               (PROGN
                (COND
                 ((EQUAL CRRNT_DIM 1)
                  (PROGN
                   (SETQ F1 (EXTEND F1 1 1 0))
                   (SETMAT F1 2 1 (LIST 'TIMES (MINUS 1) (GETMAT IN_MAT 2 1)))
                   (SETQ CRRNT_DIM (TIMES 2 CRRNT_DIM))
                   NIL))
                 (T
                  (PROGN
                   (SETQ F1 (COMPOSE*MAT IN_MAT F1 SUBDIM0 CRRNT_DIM))
                   (SETQ CRRNT_DIM (TIMES 2 CRRNT_DIM))
                   NIL)))
                NIL)))
             NIL))
           (T
            (PROGN
             (SETQ A4* (MATINVERSE (MATSM A1)))
             (SETQ A_TMP (SUB_MATRIX IN_MAT SUBDIM1 SUBDIM0))
             (SETQ A4* (MULTM (MATSM A_TMP) A4*))
             (SETQ A4*
                     (PROG (U FORALL-RESULT FORALL-ENDPTR)
                       (SETQ U A4*)
                       (COND ((NULL U) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (U)
                                           (PROG (V FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ V U)
                                             (COND ((NULL V) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (V)
                                                                 (SETQ V
                                                                         (NEGSQ
                                                                          V)))
                                                               (CAR V))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ V (CDR V))
                                             (COND
                                              ((NULL V)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (V)
                                                         (SETQ V (NEGSQ V)))
                                                       (CAR V))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))
                                         (CAR U))
                                        NIL)))
                      LOOPLABEL
                       (SETQ U (CDR U))
                       (COND ((NULL U) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (U)
                                   (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ V U)
                                     (COND ((NULL V) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (V)
                                                         (SETQ V (NEGSQ V)))
                                                       (CAR V))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ V (CDR V))
                                     (COND ((NULL V) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (V) (SETQ V (NEGSQ V)))
                                               (CAR V))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL)))
                                 (CAR U))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ A_TMP1 (CONS 'MAT A4*))
             (SETQ A_TMP (SUB_MATRIX IN_MAT SUBDIM0 SUBDIM1))
             (SETQ A4* (MULTM A4* (MATSM A_TMP)))
             (SETQ A4* (ADDM A4* (MATSM (SUB_MATRIX IN_MAT SUBDIM1 SUBDIM1))))
             (SETQ A4* (CONS 'MAT A4*))
             (SETQ F4*
                     (ADJOINT*LU
                      (PROG (TMP_MAT OUT_MAT)
                        (SETQ TMP_MAT
                                (CDR
                                 (PROG (TMP_MAT OUT_MAT)
                                   (SETQ TMP_MAT (CDR A4*))
                                   (SETQ OUT_MAT
                                           (PROG (U FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ U TMP_MAT)
                                             (COND ((NULL U) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (U)
                                                                 (PROG (V
                                                                        FORALL-RESULT
                                                                        FORALL-ENDPTR)
                                                                   (SETQ V U)
                                                                   (COND
                                                                    ((NULL V)
                                                                     (RETURN
                                                                      NIL)))
                                                                   (SETQ FORALL-RESULT
                                                                           (SETQ FORALL-ENDPTR
                                                                                   (CONS
                                                                                    ((LAMBDA
                                                                                         (
                                                                                          V)
                                                                                       (COND
                                                                                        ((ATOM
                                                                                          V)
                                                                                         V)
                                                                                        (T
                                                                                         (MK*SQ
                                                                                          V))))
                                                                                     (CAR
                                                                                      V))
                                                                                    NIL)))
                                                                  LOOPLABEL
                                                                   (SETQ V
                                                                           (CDR
                                                                            V))
                                                                   (COND
                                                                    ((NULL V)
                                                                     (RETURN
                                                                      FORALL-RESULT)))
                                                                   (RPLACD
                                                                    FORALL-ENDPTR
                                                                    (CONS
                                                                     ((LAMBDA
                                                                          (V)
                                                                        (COND
                                                                         ((ATOM
                                                                           V)
                                                                          V)
                                                                         (T
                                                                          (MK*SQ
                                                                           V))))
                                                                      (CAR V))
                                                                     NIL))
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CDR
                                                                            FORALL-ENDPTR))
                                                                   (GO
                                                                    LOOPLABEL)))
                                                               (CAR U))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ U (CDR U))
                                             (COND
                                              ((NULL U)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (U)
                                                         (PROG (V FORALL-RESULT
                                                                FORALL-ENDPTR)
                                                           (SETQ V U)
                                                           (COND
                                                            ((NULL V)
                                                             (RETURN NIL)))
                                                           (SETQ FORALL-RESULT
                                                                   (SETQ FORALL-ENDPTR
                                                                           (CONS
                                                                            ((LAMBDA
                                                                                 (
                                                                                  V)
                                                                               (COND
                                                                                ((ATOM
                                                                                  V)
                                                                                 V)
                                                                                (T
                                                                                 (MK*SQ
                                                                                  V))))
                                                                             (CAR
                                                                              V))
                                                                            NIL)))
                                                          LOOPLABEL
                                                           (SETQ V (CDR V))
                                                           (COND
                                                            ((NULL V)
                                                             (RETURN
                                                              FORALL-RESULT)))
                                                           (RPLACD
                                                            FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (V)
                                                                (COND
                                                                 ((ATOM V) V)
                                                                 (T
                                                                  (MK*SQ V))))
                                                              (CAR V))
                                                             NIL))
                                                           (SETQ FORALL-ENDPTR
                                                                   (CDR
                                                                    FORALL-ENDPTR))
                                                           (GO LOOPLABEL)))
                                                       (CAR U))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))
                                   (RETURN (CONS 'MAT OUT_MAT)))))
                        (SETQ OUT_MAT
                                (PROG (U FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ U TMP_MAT)
                                  (COND ((NULL U) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (U)
                                                      (PROG (V FORALL-RESULT
                                                             FORALL-ENDPTR)
                                                        (SETQ V U)
                                                        (COND
                                                         ((NULL V)
                                                          (RETURN NIL)))
                                                        (SETQ FORALL-RESULT
                                                                (SETQ FORALL-ENDPTR
                                                                        (CONS
                                                                         ((LAMBDA
                                                                              (
                                                                               V)
                                                                            (COND
                                                                             ((FIXP
                                                                               V)
                                                                              V)
                                                                             (T
                                                                              (REVAL1
                                                                               V
                                                                               T))))
                                                                          (CAR
                                                                           V))
                                                                         NIL)))
                                                       LOOPLABEL
                                                        (SETQ V (CDR V))
                                                        (COND
                                                         ((NULL V)
                                                          (RETURN
                                                           FORALL-RESULT)))
                                                        (RPLACD FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (V)
                                                                    (COND
                                                                     ((FIXP V)
                                                                      V)
                                                                     (T
                                                                      (REVAL1 V
                                                                              T))))
                                                                  (CAR V))
                                                                 NIL))
                                                        (SETQ FORALL-ENDPTR
                                                                (CDR
                                                                 FORALL-ENDPTR))
                                                        (GO LOOPLABEL)))
                                                    (CAR U))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ U (CDR U))
                                  (COND ((NULL U) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (U)
                                              (PROG (V FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ V U)
                                                (COND ((NULL V) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (V)
                                                                    (COND
                                                                     ((FIXP V)
                                                                      V)
                                                                     (T
                                                                      (REVAL1 V
                                                                              T))))
                                                                  (CAR V))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ V (CDR V))
                                                (COND
                                                 ((NULL V)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (V)
                                                            (COND ((FIXP V) V)
                                                                  (T
                                                                   (REVAL1 V
                                                                           T))))
                                                          (CAR V))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL)))
                                            (CAR U))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                        (RETURN (CONS 'MAT OUT_MAT)))))
             (SETQ L
                     (PROG (I FORALL-RESULT FORALL-ENDPTR)
                       (SETQ I 1)
                       (COND ((MINUSP (DIFFERENCE CRRNT_DIM I)) (RETURN NIL)))
                       (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                      LOOPLABEL
                       (SETQ I (PLUS2 I 1))
                       (COND
                        ((MINUSP (DIFFERENCE CRRNT_DIM I))
                         (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS I NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ A_TMP (MULT_ROWS F4* L DETERMINANT))
             (SETQ A_TMP
                     (PROG (TMP_MAT OUT_MAT)
                       (SETQ TMP_MAT (CDR A_TMP))
                       (SETQ OUT_MAT
                               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ U TMP_MAT)
                                 (COND ((NULL U) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (U)
                                                     (PROG (V FORALL-RESULT
                                                            FORALL-ENDPTR)
                                                       (SETQ V U)
                                                       (COND
                                                        ((NULL V)
                                                         (RETURN NIL)))
                                                       (SETQ FORALL-RESULT
                                                               (SETQ FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              V)
                                                                           (COND
                                                                            ((FIXP
                                                                              V)
                                                                             V)
                                                                            (T
                                                                             (REVAL1
                                                                              V
                                                                              T))))
                                                                         (CAR
                                                                          V))
                                                                        NIL)))
                                                      LOOPLABEL
                                                       (SETQ V (CDR V))
                                                       (COND
                                                        ((NULL V)
                                                         (RETURN
                                                          FORALL-RESULT)))
                                                       (RPLACD FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (V)
                                                                   (COND
                                                                    ((FIXP V)
                                                                     V)
                                                                    (T
                                                                     (REVAL1 V
                                                                             T))))
                                                                 (CAR V))
                                                                NIL))
                                                       (SETQ FORALL-ENDPTR
                                                               (CDR
                                                                FORALL-ENDPTR))
                                                       (GO LOOPLABEL)))
                                                   (CAR U))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ U (CDR U))
                                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (U)
                                             (PROG (V FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ V U)
                                               (COND ((NULL V) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (V)
                                                                   (COND
                                                                    ((FIXP V)
                                                                     V)
                                                                    (T
                                                                     (REVAL1 V
                                                                             T))))
                                                                 (CAR V))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ V (CDR V))
                                               (COND
                                                ((NULL V)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (V)
                                                           (COND ((FIXP V) V)
                                                                 (T
                                                                  (REVAL1 V
                                                                          T))))
                                                         (CAR V))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                           (CAR U))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (RETURN (CONS 'MAT OUT_MAT))))
             (SETQ F1 (EXTEND F1 CRRNT_DIM CRRNT_DIM 0))
             (SETQ F1
                     (COPY_INTO A_TMP F1 (PLUS CRRNT_DIM 1)
                                (PLUS CRRNT_DIM 1)))
             (SETQ A_TMP
                     (CONS 'MAT
                           (MULTM (MATSM A_TMP)
                                  (MATSM
                                   (PROG (TMP_MAT OUT_MAT)
                                     (SETQ TMP_MAT (CDR A_TMP1))
                                     (SETQ OUT_MAT
                                             (PROG (U FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ U TMP_MAT)
                                               (COND ((NULL U) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (U)
                                                                   (PROG (V
                                                                          FORALL-RESULT
                                                                          FORALL-ENDPTR)
                                                                     (SETQ V U)
                                                                     (COND
                                                                      ((NULL V)
                                                                       (RETURN
                                                                        NIL)))
                                                                     (SETQ FORALL-RESULT
                                                                             (SETQ FORALL-ENDPTR
                                                                                     (CONS
                                                                                      ((LAMBDA
                                                                                           (
                                                                                            V)
                                                                                         (COND
                                                                                          ((ATOM
                                                                                            V)
                                                                                           V)
                                                                                          (T
                                                                                           (MK*SQ
                                                                                            V))))
                                                                                       (CAR
                                                                                        V))
                                                                                      NIL)))
                                                                    LOOPLABEL
                                                                     (SETQ V
                                                                             (CDR
                                                                              V))
                                                                     (COND
                                                                      ((NULL V)
                                                                       (RETURN
                                                                        FORALL-RESULT)))
                                                                     (RPLACD
                                                                      FORALL-ENDPTR
                                                                      (CONS
                                                                       ((LAMBDA
                                                                            (V)
                                                                          (COND
                                                                           ((ATOM
                                                                             V)
                                                                            V)
                                                                           (T
                                                                            (MK*SQ
                                                                             V))))
                                                                        (CAR
                                                                         V))
                                                                       NIL))
                                                                     (SETQ FORALL-ENDPTR
                                                                             (CDR
                                                                              FORALL-ENDPTR))
                                                                     (GO
                                                                      LOOPLABEL)))
                                                                 (CAR U))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ U (CDR U))
                                               (COND
                                                ((NULL U)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (U)
                                                           (PROG (V
                                                                  FORALL-RESULT
                                                                  FORALL-ENDPTR)
                                                             (SETQ V U)
                                                             (COND
                                                              ((NULL V)
                                                               (RETURN NIL)))
                                                             (SETQ FORALL-RESULT
                                                                     (SETQ FORALL-ENDPTR
                                                                             (CONS
                                                                              ((LAMBDA
                                                                                   (
                                                                                    V)
                                                                                 (COND
                                                                                  ((ATOM
                                                                                    V)
                                                                                   V)
                                                                                  (T
                                                                                   (MK*SQ
                                                                                    V))))
                                                                               (CAR
                                                                                V))
                                                                              NIL)))
                                                            LOOPLABEL
                                                             (SETQ V (CDR V))
                                                             (COND
                                                              ((NULL V)
                                                               (RETURN
                                                                FORALL-RESULT)))
                                                             (RPLACD
                                                              FORALL-ENDPTR
                                                              (CONS
                                                               ((LAMBDA (V)
                                                                  (COND
                                                                   ((ATOM V) V)
                                                                   (T
                                                                    (MK*SQ
                                                                     V))))
                                                                (CAR V))
                                                               NIL))
                                                             (SETQ FORALL-ENDPTR
                                                                     (CDR
                                                                      FORALL-ENDPTR))
                                                             (GO LOOPLABEL)))
                                                         (CAR U))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                     (RETURN (CONS 'MAT OUT_MAT)))))))
             (SETQ A_TMP
                     (PROG (TMP_MAT OUT_MAT)
                       (SETQ TMP_MAT (CDR A_TMP))
                       (SETQ OUT_MAT
                               (PROG (U FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ U TMP_MAT)
                                 (COND ((NULL U) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (U)
                                                     (PROG (V FORALL-RESULT
                                                            FORALL-ENDPTR)
                                                       (SETQ V U)
                                                       (COND
                                                        ((NULL V)
                                                         (RETURN NIL)))
                                                       (SETQ FORALL-RESULT
                                                               (SETQ FORALL-ENDPTR
                                                                       (CONS
                                                                        ((LAMBDA
                                                                             (
                                                                              V)
                                                                           (COND
                                                                            ((ATOM
                                                                              V)
                                                                             V)
                                                                            (T
                                                                             (MK*SQ
                                                                              V))))
                                                                         (CAR
                                                                          V))
                                                                        NIL)))
                                                      LOOPLABEL
                                                       (SETQ V (CDR V))
                                                       (COND
                                                        ((NULL V)
                                                         (RETURN
                                                          FORALL-RESULT)))
                                                       (RPLACD FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (V)
                                                                   (COND
                                                                    ((ATOM V)
                                                                     V)
                                                                    (T
                                                                     (MK*SQ
                                                                      V))))
                                                                 (CAR V))
                                                                NIL))
                                                       (SETQ FORALL-ENDPTR
                                                               (CDR
                                                                FORALL-ENDPTR))
                                                       (GO LOOPLABEL)))
                                                   (CAR U))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ U (CDR U))
                                 (COND ((NULL U) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (U)
                                             (PROG (V FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ V U)
                                               (COND ((NULL V) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (V)
                                                                   (COND
                                                                    ((ATOM V)
                                                                     V)
                                                                    (T
                                                                     (MK*SQ
                                                                      V))))
                                                                 (CAR V))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ V (CDR V))
                                               (COND
                                                ((NULL V)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (V)
                                                           (COND ((ATOM V) V)
                                                                 (T
                                                                  (MK*SQ V))))
                                                         (CAR V))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL)))
                                           (CAR U))
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                       (RETURN (CONS 'MAT OUT_MAT))))
             (SETQ F1 (COPY_INTO A_TMP F1 (PLUS CRRNT_DIM 1) 1))
             (SETQ CRRNT_DIM (TIMES CRRNT_DIM 2))
             NIL))))
        (GO WHILELABEL))
      (RETURN F1))) 
(PUT 'COMPOSE*MAT 'NUMBER-OF-ARGS 4) 
(PUT 'COMPOSE*MAT 'DEFINED-ON-LINE '177) 
(PUT 'COMPOSE*MAT 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'COMPOSE*MAT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COMPOSE*MAT (IN_MAT F1 SUBDIM0 CRRNT_DIM)
    (PROG (TMP_MAT TMP_ROW K)
      (PROG (I)
        (SETQ I (TIMES 2 CRRNT_DIM))
       LAB
        (COND
         ((MINUSP (TIMES (MINUS 1) (DIFFERENCE CRRNT_DIM I))) (RETURN NIL)))
        (PROG ()
          (SETQ K
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J 1)
                    (COND ((MINUSP (DIFFERENCE I J)) (RETURN NIL)))
                    (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS J NIL)))
                   LOOPLABEL
                    (SETQ J (PLUS2 J 1))
                    (COND ((MINUSP (DIFFERENCE I J)) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (CONS J NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (COND
           ((LESSP (RANK-EVAL (LIST (SUB_MATRIX IN_MAT K SUBDIM0))) CRRNT_DIM)
            (PROGN
             (SETQ F1 (EXTEND F1 CRRNT_DIM CRRNT_DIM 0))
             (PROG (J)
               (SETQ J (PLUS I 1))
              LAB
               (COND
                ((MINUSP (DIFFERENCE (TIMES 2 CRRNT_DIM) J)) (RETURN NIL)))
               (PROG ()
                 (SETQ K (APPEND K (LIST J)))
                 (SETQ TMP_MAT (SUB_MATRIX IN_MAT K K))
                 (SETQ TMP_ROW (ADJOINT_LST_ROW* TMP_MAT J))
                 (PROG (L)
                   (SETQ L 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE J L)) (RETURN NIL)))
                   (SETMAT F1 J L (NTH (CADR TMP_ROW) L))
                   (SETQ L (PLUS2 L 1))
                   (GO LAB)))
               (SETQ J (PLUS2 J 1))
               (GO LAB))
             (SETQ I (DIFFERENCE CRRNT_DIM 1))
             NIL))))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN F1))) 
(PUT 'ADJOINT_LST_ROW* 'NUMBER-OF-ARGS 2) 
(PUT 'ADJOINT_LST_ROW* 'DEFINED-ON-LINE '204) 
(PUT 'ADJOINT_LST_ROW* 'DEFINED-IN-FILE 'LINALG/TADJOINT.RED) 
(PUT 'ADJOINT_LST_ROW* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADJOINT_LST_ROW* (IN_MAT LEN)
    (PROG (TMP_MAT ADJ_ROW DET SIGN)
      (SETQ SIGN 0)
      (COND ((EQUAL LEN 1) (RETURN IN_MAT)))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE LEN J)) (RETURN NIL)))
        (PROG ()
          (SETQ SIGN (EXPT (MINUS 1) (PLUS LEN J)))
          (SETQ TMP_MAT (MINOR IN_MAT J LEN))
          (COND
           ((EQUAL SIGN (MINUS 1))
            (SETQ DET (MK*SQ (NEGSQ (SIMPDET (LIST TMP_MAT))))))
           (T (SETQ DET (MK*SQ (SIMPDET (LIST TMP_MAT))))))
          (SETQ ADJ_ROW (APPEND ADJ_ROW (LIST DET))))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN (CONS 'MAT (LIST ADJ_ROW))))) 
(ENDMODULE) 