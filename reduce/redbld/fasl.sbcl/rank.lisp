(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RANK)) 
(PUT 'RANK-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'RANK-EVAL 'DEFINED-ON-LINE '34) 
(PUT 'RANK-EVAL 'DEFINED-IN-FILE 'MATRIX/RANK.RED) 
(PUT 'RANK-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANK-EVAL (U)
    (PROG (N)
      (COND ((CDR U) (RERROR 'MATRIX 17 "Wrong number of arguments"))
            ((EQ (GETRTYPE (SETQ U (CAR U))) 'MATRIX)
             (RETURN (RANK-MATRIX (MATSM U))))
            ((NULL (EQCAR (SETQ U (REVAL1 U NIL)) 'LIST)) (TYPERR U "matrix"))
            (T
             (RETURN
              (RANK-MATRIX
               (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ROW (CDR U))
                 (COND ((NULL ROW) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ROW)
                                     (COND
                                      ((NOT (EQCAR ROW 'LIST))
                                       (RERROR 'MATRIX 15
                                               "list not in matrix shape"))
                                      (T
                                       (PROGN
                                        (SETQ ROW (CDR ROW))
                                        (COND ((NULL N) (SETQ N (LENGTH ROW)))
                                              ((NEQ N (LENGTH ROW))
                                               (RERROR 'MATRIX 151
                                                       "list not in matrix shape")))
                                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ J ROW)
                                          (COND ((NULL J) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (J)
                                                              (SIMP J))
                                                            (CAR J))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ J (CDR J))
                                          (COND
                                           ((NULL J) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J) (SIMP J))
                                                    (CAR J))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))))
                                   (CAR ROW))
                                  NIL)))
                LOOPLABEL
                 (SETQ ROW (CDR ROW))
                 (COND ((NULL ROW) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ROW)
                             (COND
                              ((NOT (EQCAR ROW 'LIST))
                               (RERROR 'MATRIX 15 "list not in matrix shape"))
                              (T
                               (PROGN
                                (SETQ ROW (CDR ROW))
                                (COND ((NULL N) (SETQ N (LENGTH ROW)))
                                      ((NEQ N (LENGTH ROW))
                                       (RERROR 'MATRIX 151
                                               "list not in matrix shape")))
                                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ J ROW)
                                  (COND ((NULL J) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (J) (SIMP J))
                                                    (CAR J))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ J (CDR J))
                                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J))
                                                NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL))))))
                           (CAR ROW))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))))))) 
(PUT 'RANK 'PSOPFN 'RANK-EVAL) 
(PUT 'RANK-MATRIX 'NUMBER-OF-ARGS 1) 
(PUT 'RANK-MATRIX 'DEFINED-ON-LINE '54) 
(PUT 'RANK-MATRIX 'DEFINED-IN-FILE 'MATRIX/RANK.RED) 
(PUT 'RANK-MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RANK-MATRIX (U)
    (PROG (X Y Z M N)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ Z 1)
      (PROG (V)
        (SETQ V U)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V)
           (PROGN
            (SETQ Y 1)
            (PROG (W)
              (SETQ W V)
             LAB
              (COND ((NULL W) (RETURN NIL)))
              ((LAMBDA (W) (SETQ Y (LCM Y (CDR W)))) (CAR W))
              (SETQ W (CDR W))
              (GO LAB))
            (SETQ M 1)
            (SETQ X NIL)
            (PROG (J)
              (SETQ J V)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J)
                 (PROGN
                  (COND
                   ((CAR J)
                    (SETQ X
                            (CONS
                             (CONS (LIST M)
                                   ((LAMBDA (G566)
                                      (COND
                                       (*PHYSOP-LOADED
                                        (PHYSOP-MULTF (CAR J) G566))
                                       (T (POLY-MULTF (CAR J) G566))))
                                    ((LAMBDA (*EXP) (QUOTF1 Y (CDR J))) T)))
                             X))))
                  (SETQ M (PLUS M 1))))
               (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (COND
             ((SETQ Y (|C:EXTMULT| X Z))
              (PROGN (SETQ Z Y) (SETQ N (PLUS N 1)))))))
         (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (RETURN N))) 
(ENDMODULE) 