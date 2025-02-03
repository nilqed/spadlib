(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SIMP4)) 
(FLUID '(ZERO)) 
(PUT 'IDEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'IDEVAL 'DEFINED-ON-LINE '32) 
(PUT 'IDEVAL 'DEFINED-IN-FILE 'REDUCE4/SIMP4.RED) 
(PUT 'IDEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDEVAL (U) (KERNELVALUE U)) 
(PUT 'KERNELVALUE 'NUMBER-OF-ARGS 1) 
(PUT 'KERNELVALUE 'DEFINED-ON-LINE '37) 
(PUT 'KERNELVALUE 'DEFINED-IN-FILE 'REDUCE4/SIMP4.RED) 
(PUT 'KERNELVALUE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KERNELVALUE (U)
    (PROG (X)
      (COND ((NULL SUBFG*) (RETURN (TYPE_REDUCE U 'KERNEL)))
            ((SETQ X (ASSOC U WTL*))
             (RETURN
              (COND ((NULL (CAR (SETQ X (MKSQ 'K* (CDR X))))) X)
                    (T
                     (TYPE_REDUCE
                      (MULTSQ X
                              (CONS
                               (LIST (CONS (GETPOWER (CAR (FKERN U)) 1) 1)) 1))
                      'RATPOL)))))
            ((ATOM U)
             (PROGN
              (COND
               ((AND (NULL *NOSUBS) (SETQ X (GET U 'AVALUE)))
                (RETURN
                 (COND ((MEMQ (TYPE X) '(GENERIC SCALAR)) (SIMP4 (VALUE X)))
                       (T X)))))
              (COND ((IDP U) (FLAG (LIST U) 'USED*)))
              (RETURN (MKOBJECT U 'VARIABLE))))
            ((AND (NULL *NOSUBS) (SETQ X (ASSOC U (GET (CAR U) 'KVALUE))))
             (RETURN (SIMP4 (CADR X))))
            ((NOT (MEMQ 'USED* (CDDR (SETQ X (FKERN U))))) (ACONC X 'USED*)))
      (RETURN (MKOBJECT (CAR X) 'XKERNEL)))) 
(PUT 'EVAL_GENERIC 'NUMBER-OF-ARGS 2) 
(PUT 'EVAL_GENERIC 'DEFINED-ON-LINE '60) 
(PUT 'EVAL_GENERIC 'DEFINED-IN-FILE 'REDUCE4/SIMP4.RED) 
(PUT 'EVAL_GENERIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVAL_GENERIC (FN U)
    (PROG (X Y Z)
      (SETQ U
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J U)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND
                                     ((SETQ X (GET (TYPE J) 'PREFIX_CONVERT))
                                      (APPLY1 X (VALUE J)))
                                     (T (VALUE J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (COND
                             ((SETQ X (GET (TYPE J) 'PREFIX_CONVERT))
                              (APPLY1 X (VALUE J)))
                             (T (VALUE J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND U (EQUAL (CAR U) 0) (FLAGP FN 'ODD) (NOT (FLAGP FN 'NONZERO)))
        (RETURN ZERO)))
      (SETQ U (CONS FN U))
      (COND ((FLAGP FN 'NONCOM) (SETQ NCMP* T)))
      (COND ((NULL SUBFG*) (GO C))
            ((NEQ (SETQ Z (VALUE (SETQ X (KERNELVALUE U)))) U) (RETURN X)))
      (SETQ U Z)
      (COND
       ((AND (FLAGP FN 'LINEAR) (NEQ (SETQ Z (FORMLNR U)) U))
        (RETURN (SIMP4 Z)))
       ((SETQ Z (OPMTCH U)) (RETURN (SIMP4 Z))))
     C
      (COND ((FLAGP FN 'SYMMETRIC) (SETQ U (CONS FN (ORDN (CDR U)))))
            ((FLAGP FN 'ANTISYMMETRIC)
             (PROGN
              (COND ((REPEATS (CDR U)) (RETURN (CONS NIL 1)))
                    ((NOT (PERMP (SETQ Z (ORDN (CDR U))) (CDR U))) (SETQ Y T)))
              (SETQ FN (CONS (CAR U) Z))
              (COND
               ((AND (NEQ Z (CDR U)) (SETQ Z (OPMTCH FN)))
                (RETURN (COND (Y (NEGSQ (SIMP4 Z))) (T (SIMP4 Z))))))
              (SETQ U FN))))
      (COND
       ((AND (OR (FLAGP FN 'EVEN) (FLAGP FN 'ODD)) X
             (MINUSF (CAR (SETQ X (SIMP (CAR X))))))
        (PROGN
         (COND ((FLAGP FN 'ODD) (SETQ Y (NOT Y))))
         (SETQ U (CONS FN (CONS (PREPSQXX (NEGSQ X)) (CDDR U))))
         (COND
          ((SETQ Z (OPMTCH U))
           (RETURN (COND (Y (NEGSQ (SIMP Z))) (T (SIMP Z)))))))))
      (SETQ U (MKSQ U 1))
      (COND (Y (SETQ U (NEGSQ U))))
      (RETURN (TYPE_REDUCE U 'RATPOL)))) 
(PUT 'SIMP4* 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP4* 'DEFINED-ON-LINE '100) 
(PUT 'SIMP4* 'DEFINED-IN-FILE 'REDUCE4/SIMP4.RED) 
(PUT 'SIMP4* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP4* (U)
    (PROG (*ASYMP* X)
      (COND ((MEMQ (SETQ X (TYPE U)) '(NZINT VARIABLE ZERO)) (RETURN U))
            ((EQ X 'XPOLY) (SETQ U (CONS (VALUE U) 1)))
            ((EQ X 'XRATPOL) (SETQ U (VALUE U)))
            (T (REDERR (LIST "No simplification for type" X))))
      (SETQ U (SUBS2 U))
      (COND (*COMBINELOGS (SETQ U (CLOGSQ* U))))
      (COND
       ((AND (EQ DMODE* '|:GI:|) (NOT *NORATIONALGI))
        (SETQ U (|GIRATIONALIZE:| U)))
       (*RATIONALIZE (SETQ U (RATIONALIZESQ U))) (T (SETQ U (RATIONALIZEI U))))
      (COND ((AND *ASYMP* *RATIONALIZE) (SETQ U (GCDCHK U))))
      (RETURN (TYPE_REDUCE U 'RATPOL)))) 
(PUT 'SIMP4 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP4 'DEFINED-ON-LINE '122) 
(PUT 'SIMP4 'DEFINED-IN-FILE 'REDUCE4/SIMP4.RED) 
(PUT 'SIMP4 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP4 (U) (TYPE_REDUCE (SIMP U) 'RATPOL)) 
(PUT 'XPOLY 'PREFIX_CONVERT 'PREPF) 
(PUT 'XRATPOL 'PREFIX_CONVERT 'PREPSQXX) 
(FLAG '(COS SIN) 'OPR) 
(ENDMODULE) 