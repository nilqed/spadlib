(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYPRINT)) 
(EXPORTS (LIST 'TAYLOR*PRINT 'TAYLOR*PRINT1)) 
(IMPORTS
 (LIST 'DENR 'EQCAR 'FMPRINT 'KERNP 'LASTPAIR 'LIST2STRING 'MAPRINT 'MVAR 'NUMR
       'PREPSQ 'SIMP* 'SMEMQ 'TYPERR 'TAYCFSQ 'TAYCOEFFLIST 'TAYORIG
       'TAYTEMPLATE 'TAYTPELORDER 'TAYTPELPOINT 'TAYTPELVARS 'PREPTAYLOR*
       'PREPTAYLOR*1 'TAYLOR-GEN-BIG-O)) 
(FLUID '(*FORT *NAT *TAYLORPRINTORDER TAYLOR-TRUNCATION-FLAG TAYLORPRINTTERMS)) 
(PUT 'CHECK-PRINT-TERMS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-PRINT-TERMS 'DEFINED-ON-LINE '54) 
(PUT 'CHECK-PRINT-TERMS 'DEFINED-IN-FILE 'TAYLOR/TAYPRINT.RED) 
(PUT 'CHECK-PRINT-TERMS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-PRINT-TERMS (U)
    (PROG (X)
      (SETQ X (SIMP* U))
      (COND ((AND (KERNP X) (EQ (CAAAR (CAR X)) 'ALL)) (RETURN NIL))
            ((AND (EQUAL (CDR X) 1) (FIXP (CAR X))) (RETURN (CAR X)))
            (T (TYPERR X "value of TaylorPrintTerms"))))) 
(PUT 'TAYLOR*PRINT1 'NUMBER-OF-ARGS 1) 
(PUT 'TAYLOR*PRINT1 'DEFINED-ON-LINE '63) 
(PUT 'TAYLOR*PRINT1 'DEFINED-IN-FILE 'TAYLOR/TAYPRINT.RED) 
(PUT 'TAYLOR*PRINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYLOR*PRINT1 (U)
    (COND
     ((OR (SMEMQ '~ U) (AND (ATOM (CADR U)) (NOT (NULL (CADR U)))))
      (CONS 'TAYLOR (CDR U)))
     (T
      (PROG (TAYLOR-TRUNCATION-FLAG PREPEXPR REST NTERMS)
        (SETQ NTERMS
                (COND (*TAYLORPRINTORDER (CHECK-PRINT-TERMS TAYLORPRINTTERMS))
                      (T NIL)))
        (SETQ PREPEXPR (PREPTAYLOR*1 (CADR U) (CADDR U) NTERMS))
        (COND
         (*TAYLORPRINTORDER
          (PROGN
           (SETQ REST (LIST (TAYLOR-GEN-BIG-O (CADDR U))))
           (COND
            (TAYLOR-TRUNCATION-FLAG
             (PROG (NOTPRINTED)
               (SETQ NOTPRINTED 0)
               (SETQ NOTPRINTED (MINUS NTERMS))
               (PROG (PP)
                 (SETQ PP (CADR U))
                LAB
                 (COND ((NULL PP) (RETURN NIL)))
                 ((LAMBDA (PP)
                    (COND
                     ((NOT (NULL (CAR (CDR PP))))
                      (SETQ NOTPRINTED (PLUS NOTPRINTED 1)))))
                  (CAR PP))
                 (SETQ PP (CDR PP))
                 (GO LAB))
               (COND ((EQUAL NOTPRINTED 1) (SETQ REST (CONS "(1 term)" REST)))
                     (T
                      (SETQ REST
                              (CONS
                               (LIST2STRING
                                (CONS '|(|
                                      (NCONC (EXPLODE NOTPRINTED)
                                             '(| | T E R M S |)|))))
                               REST)))))))))
         (T (SETQ REST (LIST '|...|))))
        (RETURN
         (COND
          ((NOT (EQCAR PREPEXPR 'PLUS))
           (CONS 'PLUS (CONS (OR PREPEXPR 0) REST)))
          (T (NCONC PREPEXPR REST)))))))) 
(PUT 'TAYLOR* 'FANCY-REFORM 'TAYLOR*PRINT1) 
(PUT 'TAYLOR*PRINT 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOR*PRINT 'DEFINED-ON-LINE '100) 
(PUT 'TAYLOR*PRINT 'DEFINED-IN-FILE 'TAYLOR/TAYPRINT.RED) 
(PUT 'TAYLOR*PRINT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOR*PRINT (U P)
    (COND
     ((OR (SMEMQ '~ U) (AND (ATOM (CADR U)) (NOT (NULL (CADR U)))))
      (MAPRINT (CONS 'TAYLOR (CDR U)) P))
     (*FORT (FMPRINT (PREPTAYLOR* U) 0))
     ((NULL *NAT)
      (MAPRINT
       (CONS 'TAYLOR
             (CONS (COND ((CADDDR U) (PREPSQ (CADDDR U))) (T (PREPTAYLOR* U)))
                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                     (SETQ EL (CADDR U))
                    STARTOVER
                     (COND ((NULL EL) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             ((LAMBDA (EL)
                                (LIST
                                 (COND ((NULL (CDR (CAR EL))) (CAR (CAR EL)))
                                       (T (CONS 'LIST (CAR EL))))
                                 (CADR EL) (CADDR EL)))
                              (CAR EL)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                     (SETQ EL (CDR EL))
                     (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                    LOOPLABEL
                     (COND ((NULL EL) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             ((LAMBDA (EL)
                                (LIST
                                 (COND ((NULL (CDR (CAR EL))) (CAR (CAR EL)))
                                       (T (CONS 'LIST (CAR EL))))
                                 (CADR EL) (CADDR EL)))
                              (CAR EL)))
                     (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                     (SETQ EL (CDR EL))
                     (GO LOOPLABEL))))
       P))
     (T (MAPRINT (TAYLOR*PRINT1 U) P)))) 
(PUT 'TAYLOR* 'PPRIFN 'TAYLOR*PRINT) 
(ENDMODULE) 