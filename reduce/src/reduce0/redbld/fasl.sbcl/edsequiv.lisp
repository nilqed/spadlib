(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSEQUIV)) 
(FLUID '(XTRUNCATE*)) 
(INFIX (LIST 'EQUIV)) 
(PRECEDENCE (LIST 'EQUIV 'EQUAL)) 
(FLAG '(EQUIV) 'OPFN) 
(PUT 'EQUIV 'NUMBER-OF-ARGS 2) 
(PUT 'EQUIV 'DEFINED-ON-LINE '39) 
(PUT 'EQUIV 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUIV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUIV (U V)
    (COND ((CFRMP U) (AND (CFRMP V) (EQUALCFRM U V)))
          ((EDSP U) (AND (EDSP V) (EDSPROTECT (LIST 'EQUALEDS U V))))
          (T (RERROR 'EDS 0 "Don't know how to test equivalence")))) 
(PUT 'EQUALCFRM 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALCFRM 'DEFINED-ON-LINE '45) 
(PUT 'EQUALCFRM 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUALCFRM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALCFRM (M N)
    (AND (EQUALL (CADR M) (CADR N)) (EQUALL (CADDR M) (CADDR N))
         (EQUALDRV (CADDR (CDR M)) (CADDR (CDR N)))
         (EQUALRSX (CADDR (CDDR M)) (CADDR (CDDR N))))) 
(PUT 'EQUALL 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALL 'DEFINED-ON-LINE '53) 
(PUT 'EQUALL 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALL (U V) (AND (EQUAL (LENGTH U) (LENGTH V)) (SUBSETP U V))) 
(PUT 'EQUALDRV 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALDRV 'DEFINED-ON-LINE '58) 
(PUT 'EQUALDRV 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUALDRV 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALDRV (D1 D2)
    (OR (EQUALL D1 D2)
        (AND
         (EQUALL
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R D1)
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (R) (CADR R)) (CAR R)) NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (R) (CADR R)) (CAR R)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R D2)
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (R) (CADR R)) (CAR R)) NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (R) (CADR R)) (CAR R)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))
         (EQUALL
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R D1)
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (R) (RESIMP (SIMP* (CADDR R)))) (CAR R))
                             NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (R) (RESIMP (SIMP* (CADDR R)))) (CAR R))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))
          (PROG (R FORALL-RESULT FORALL-ENDPTR)
            (SETQ R D2)
            (COND ((NULL R) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (R) (RESIMP (SIMP* (CADDR R)))) (CAR R))
                             NIL)))
           LOOPLABEL
            (SETQ R (CDR R))
            (COND ((NULL R) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (R) (RESIMP (SIMP* (CADDR R)))) (CAR R))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))))) 
(PUT 'EQUALRSX 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALRSX 'DEFINED-ON-LINE '67) 
(PUT 'EQUALRSX 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUALRSX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALRSX (R1 R2)
    (OR (EQUALL R1 R2)
        (EQUALL
         (PROG (R FORALL-RESULT FORALL-ENDPTR)
           (SETQ R R1)
           (COND ((NULL R) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (R) (ABSF (CAR (SIMP* R)))) (CAR R))
                                 NIL)))
          LOOPLABEL
           (SETQ R (CDR R))
           (COND ((NULL R) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (R) (ABSF (CAR (SIMP* R)))) (CAR R)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))
         (PROG (R FORALL-RESULT FORALL-ENDPTR)
           (SETQ R R2)
           (COND ((NULL R) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (R) (ABSF (CAR (SIMP* R)))) (CAR R))
                                 NIL)))
          LOOPLABEL
           (SETQ R (CDR R))
           (COND ((NULL R) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (R) (ABSF (CAR (SIMP* R)))) (CAR R)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))) 
(PUT 'EQUALEDS 'NUMBER-OF-ARGS 2) 
(PUT 'EQUALEDS 'DEFINED-ON-LINE '74) 
(PUT 'EQUALEDS 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUALEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUALEDS (S1 S2)
    (AND (EQUALCFRM (CADDR (CDR S1)) (CADDR (CDR S2)))
         (EQUIVSYS (CADR S1) (CADR S2)) (EQUIVSYS (CADDR S1) (CADDR S2)))) 
(PUT 'EQUIVSYS 'NUMBER-OF-ARGS 2) 
(PUT 'EQUIVSYS 'DEFINED-ON-LINE '81) 
(PUT 'EQUIVSYS 'DEFINED-IN-FILE 'EDS/EDSEQUIV.RED) 
(PUT 'EQUIVSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQUIVSYS (P Q)
    (OR (EQUALL (SETQ P (XREORDERSYS P)) (SETQ Q (XREORDERSYS Q)))
        (PROG (P1 Q1 G XTRUNCATE* D)
          (SETQ D 0)
          (SETQ P1
                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                    (SETQ F (SETDIFF P Q))
                   STARTOVER
                    (COND ((NULL F) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (F)
                               (COND ((SETQ F (XREDUCE F Q)) (LIST F))))
                             (CAR F)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ F (CDR F))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (F)
                               (COND ((SETQ F (XREDUCE F Q)) (LIST F))))
                             (CAR F)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ F (CDR F))
                    (GO LOOPLABEL)))
          (SETQ Q1
                  (PROG (F FORALL-RESULT FORALL-ENDPTR)
                    (SETQ F (SETDIFF Q P))
                   STARTOVER
                    (COND ((NULL F) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (F)
                               (COND ((SETQ F (XREDUCE F P)) (LIST F))))
                             (CAR F)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ F (CDR F))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL F) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (F)
                               (COND ((SETQ F (XREDUCE F P)) (LIST F))))
                             (CAR F)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ F (CDR F))
                    (GO LOOPLABEL)))
          (COND ((AND (NULL P1) (NULL Q1)) (RETURN T)))
          (COND
           ((OR (SCALARPART P1) (SCALARPART Q1))
            (RERROR 'EDS 0 "Can't compare systems with 0-forms")))
          (COND
           (P1
            (PROGN
             (SETQ D 0)
             (PROG (F)
               (SETQ F P1)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F) (SETQ D (MAX D (DEGREEPF F)))) (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ XTRUNCATE* D)
             (SETQ G (XIDEALPF Q))
             (SETQ P1
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F P1)
                      STARTOVER
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (F)
                                  (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ F (CDR F))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (F)
                                  (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ F (CDR F))
                       (GO LOOPLABEL))))))
          (COND (P1 (RETURN NIL)))
          (COND
           (Q1
            (PROGN
             (SETQ D 0)
             (PROG (F)
               (SETQ F Q1)
              LAB
               (COND ((NULL F) (RETURN NIL)))
               ((LAMBDA (F) (SETQ D (MAX D (DEGREEPF F)))) (CAR F))
               (SETQ F (CDR F))
               (GO LAB))
             (SETQ XTRUNCATE* D)
             (SETQ G (XIDEALPF P))
             (SETQ Q1
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F Q1)
                      STARTOVER
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (F)
                                  (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ F (CDR F))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (F)
                                  (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                                (CAR F)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ F (CDR F))
                       (GO LOOPLABEL))))))
          (RETURN (NULL Q1))))) 
(ENDMODULE) 