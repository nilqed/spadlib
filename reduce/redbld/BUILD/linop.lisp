(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LINOP)) 
(FLUID '(*INTSTR)) 
(PUT 'LINEAR 'NUMBER-OF-ARGS 1) 
(PUT 'LINEAR 'DEFINED-ON-LINE '34) 
(PUT 'LINEAR 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'LINEAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINEAR (U)
    (PROGN
     (PROG (X)
       (SETQ X U)
      LAB
       (COND ((NULL X) (RETURN NIL)))
       ((LAMBDA (X)
          (COND ((NOT (IDP X)) (TYPERR X 'OPERATOR))
                (T (FLAG (LIST X) 'LINEAR))))
        (CAR X))
       (SETQ X (CDR X))
       (GO LAB))
     (RMSUBS))) 
(RLISTAT '(LINEAR)) 
(PUT 'FORMLNR 'NUMBER-OF-ARGS 1) 
(PUT 'FORMLNR 'DEFINED-ON-LINE '43) 
(PUT 'FORMLNR 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'FORMLNR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORMLNR (U)
    (PROG (X Y Z)
      (SETQ X (CAR U))
      (COND
       ((OR (NULL (CDR U)) (NULL (CDDR U)))
        (RERROR 'ALG 29
                (LIST "Linear operator" X "called with too few arguments"))))
      (SETQ Y (CADR U))
      (SETQ Z (CONS (*A2K (CADDR U)) (CDDDR U)))
      (RETURN
       (COND ((EQUAL Y 1) U)
             ((NOT (DEPENDS Y (CAR Z))) (LIST 'TIMES Y (CONS X (CONS 1 Z))))
             ((ATOM Y) U)
             ((EQ (CAR Y) 'PLUS)
              (CONS 'PLUS
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDR Y))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J)
                                          (FORMLNR (CONS X (CONS J Z))))
                                        (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (J) (FORMLNR (CONS X (CONS J Z))))
                                (CAR J))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
             ((EQ (CAR Y) 'MINUS)
              (LIST 'MINUS (FORMLNR (CONS X (CONS (CADR Y) Z)))))
             ((EQ (CAR Y) 'DIFFERENCE)
              (LIST 'DIFFERENCE (FORMLNR (CONS X (CONS (CADR Y) Z)))
                    (FORMLNR (CONS X (CONS (CADDR Y) Z)))))
             ((EQ (CAR Y) 'TIMES) (FORMLNTMS X (CDR Y) Z U))
             ((EQ (CAR Y) 'QUOTIENT) (FORMLNQUOT X (CDR Y) Z U))
             ((EQ (CAR Y) 'RECIP) (FORMLNRECIP X (CARX (CDR Y) 'RECIP) Z U))
             ((SETQ Y (EXPT-SEPARATE Y (CAR Z)))
              (LIST 'TIMES (CAR Y) (CONS X (CONS (CDR Y) Z))))
             (T U))))) 
(PUT 'FORMSEPARATE 'NUMBER-OF-ARGS 2) 
(PUT 'FORMSEPARATE 'DEFINED-ON-LINE '71) 
(PUT 'FORMSEPARATE 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'FORMSEPARATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FORMSEPARATE (U V)
    (PROG (W X Y)
      (PROG (Z)
        (SETQ Z U)
       LAB
        (COND ((NULL Z) (RETURN NIL)))
        ((LAMBDA (Z)
           (COND
            ((AND (NOT (AND *NCMP (NONCOMP1 Z))) (NOT (DEPENDS Z V)))
             (SETQ X (CONS Z X)))
            ((SETQ W (EXPT-SEPARATE Z V))
             (PROGN (SETQ X (CONS (CAR W) X)) (SETQ Y (CONS (CDR W) Y))))
            (T (SETQ Y (CONS Z Y)))))
         (CAR Z))
        (SETQ Z (CDR Z))
        (GO LAB))
      (RETURN (CONS (REVERSIP* X) (REVERSIP* Y))))) 
(PUT 'EXPT-SEPARATE 'NUMBER-OF-ARGS 2) 
(PUT 'EXPT-SEPARATE 'DEFINED-ON-LINE '83) 
(PUT 'EXPT-SEPARATE 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'EXPT-SEPARATE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPT-SEPARATE (U V)
    (COND
     ((OR (NOT (EQCAR U 'EXPT)) (DEPENDS (CADR U) V)
          (NOT (EQCAR (CADDR U) 'PLUS)))
      NIL)
     (T (EXPT-SEPARATE1 (CDADDR U) (CADR U) V)))) 
(PUT 'EXPT-SEPARATE1 'NUMBER-OF-ARGS 3) 
(PUT 'EXPT-SEPARATE1 'DEFINED-ON-LINE '92) 
(PUT 'EXPT-SEPARATE1 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'EXPT-SEPARATE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPT-SEPARATE1 (U V W)
    (PROG (X)
      (SETQ X (FORMSEPARATE U W))
      (RETURN
       (COND ((NULL (CAR X)) NIL)
             (T
              (CONS (LIST 'EXPT V (REPLUS (CAR X)))
                    (COND ((NULL (CDR X)) 1)
                          (T (LIST 'EXPT V (REPLUS (CDR X))))))))))) 
(PUT 'FORMLNTMS 'NUMBER-OF-ARGS 4) 
(PUT 'FORMLNTMS 'DEFINED-ON-LINE '100) 
(PUT 'FORMLNTMS 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'FORMLNTMS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMLNTMS (U V W X)
    (PROG (Y)
      (SETQ Y (FORMSEPARATE V (CAR W)))
      (RETURN
       (COND ((NULL (CAR Y)) X)
             (T
              (CONS 'TIMES
                    (ACONC* (CAR Y)
                            (COND
                             ((NULL (CDDR Y))
                              (FORMLNR (CONS U (CONS (CADR Y) W))))
                             (T (CONS U (CONS (CONS 'TIMES (CDR Y)) W))))))))))) 
(PUT 'FORMLNQUOT 'NUMBER-OF-ARGS 4) 
(PUT 'FORMLNQUOT 'DEFINED-ON-LINE '112) 
(PUT 'FORMLNQUOT 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'FORMLNQUOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMLNQUOT (FN QUOTARGS REST WHOLE)
    (PROG (X)
      (RETURN
       (COND
        ((NOT (DEPENDS (CADR QUOTARGS) (CAR REST)))
         (LIST 'QUOTIENT (FORMLNR (CONS FN (CONS (CAR QUOTARGS) REST)))
               (CADR QUOTARGS)))
        ((AND (NOT (DEPENDS (CAR QUOTARGS) (CAR REST))) (NEQ (CAR QUOTARGS) 1))
         (LIST 'TIMES (CAR QUOTARGS)
               (FORMLNR (CONS FN (CONS (LIST 'RECIP (CADR QUOTARGS)) REST)))))
        ((EQCAR (CAR QUOTARGS) 'PLUS)
         (CONS 'PLUS
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDAR QUOTARGS))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (FORMLNR
                                      (CONS FN
                                            (CONS
                                             (CONS 'QUOTIENT
                                                   (CONS J (CDR QUOTARGS)))
                                             REST))))
                                   (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (J)
                             (FORMLNR
                              (CONS FN
                                    (CONS
                                     (CONS 'QUOTIENT (CONS J (CDR QUOTARGS)))
                                     REST))))
                           (CAR J))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
        ((EQCAR (CAR QUOTARGS) 'MINUS)
         (LIST 'MINUS
               (FORMLNR
                (CONS FN
                      (CONS
                       (CONS 'QUOTIENT (CONS (CADAR QUOTARGS) (CDR QUOTARGS)))
                       REST)))))
        ((AND (EQCAR (CAR QUOTARGS) 'TIMES)
              (CAR (SETQ X (FORMSEPARATE (CDAR QUOTARGS) (CAR REST)))))
         (CONS 'TIMES
               (ACONC* (CAR X)
                       (FORMLNR
                        (CONS FN
                              (CONS
                               (LIST 'QUOTIENT (MKTIMES (CDR X))
                                     (CADR QUOTARGS))
                               REST))))))
        ((AND (EQCAR (CADR QUOTARGS) 'TIMES)
              (CAR (SETQ X (FORMSEPARATE (CDADR QUOTARGS) (CAR REST)))))
         (LIST 'TIMES (LIST 'RECIP (MKTIMES (CAR X)))
               (FORMLNR
                (CONS FN
                      (CONS (LIST 'QUOTIENT (CAR QUOTARGS) (MKTIMES (CDR X)))
                            REST)))))
        ((SETQ X (EXPT-SEPARATE (CAR QUOTARGS) (CAR REST)))
         (LIST 'TIMES (CAR X)
               (FORMLNR
                (CONS FN
                      (CONS (LIST 'QUOTIENT (CDR X) (CADR QUOTARGS)) REST)))))
        ((SETQ X (EXPT-SEPARATE (CADR QUOTARGS) (CAR REST)))
         (LIST 'TIMES (LIST 'RECIP (CAR X))
               (FORMLNR
                (CONS FN
                      (CONS (LIST 'QUOTIENT (CAR QUOTARGS) (CDR X)) REST)))))
        ((NEQ (SETQ X (REVAL* (CADR QUOTARGS))) (CADR QUOTARGS))
         (FORMLNQUOT FN (LIST (CAR QUOTARGS) X) REST WHOLE))
        (T WHOLE))))) 
(PUT 'FORMLNRECIP 'NUMBER-OF-ARGS 4) 
(PUT 'FORMLNRECIP 'DEFINED-ON-LINE '154) 
(PUT 'FORMLNRECIP 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'FORMLNRECIP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMLNRECIP (FN RECIPARG REST WHOLE)
    (PROG (X)
      (RETURN
       (COND
        ((NOT (DEPENDS RECIPARG (CAR REST)))
         (LIST 'QUOTIENT (CONS FN (CONS 1 REST)) RECIPARG))
        ((EQCAR RECIPARG 'MINUS)
         (LIST 'MINUS
               (FORMLNR (CONS FN (CONS (CONS 'RECIP (CDR RECIPARG)) REST)))))
        ((AND (EQCAR RECIPARG 'TIMES)
              (CAR (SETQ X (FORMSEPARATE (CDR RECIPARG) (CAR REST)))))
         (LIST 'TIMES (LIST 'RECIP (MKTIMES (CAR X)))
               (FORMLNR
                (CONS FN (CONS (LIST 'RECIP (MKTIMES (CDR X))) REST)))))
        ((SETQ X (EXPT-SEPARATE RECIPARG (CAR REST)))
         (LIST 'TIMES (LIST 'RECIP (CAR X))
               (FORMLNR (CONS FN (CONS (LIST 'RECIP (CDR X)) REST)))))
        ((NEQ (SETQ X (REVAL* RECIPARG)) RECIPARG)
         (FORMLNRECIP FN X REST WHOLE))
        (T WHOLE))))) 
(PUT 'MKTIMES 'NUMBER-OF-ARGS 1) 
(PUT 'MKTIMES 'DEFINED-ON-LINE '177) 
(PUT 'MKTIMES 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'MKTIMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKTIMES (U) (COND ((NULL (CDR U)) (CAR U)) (T (CONS 'TIMES U)))) 
(PUT 'REVAL* 'NUMBER-OF-ARGS 1) 
(PUT 'REVAL* 'DEFINED-ON-LINE '180) 
(PUT 'REVAL* 'DEFINED-IN-FILE 'ALG/LINOP.RED) 
(PUT 'REVAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVAL* (U) (PROG (*INTSTR) (SETQ *INTSTR T) (RETURN (REVAL1 U T)))) 
(ENDMODULE) 