(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INEQ)) 
(CREATE-PACKAGE '(INEQ LININEQ LIQSIMP1 LIQSIMP2 POLINEQ) '(SOLVE)) 
(LOAD-PACKAGE 'SOLVE) 
(FLUID '(SOLVEMETHODS*)) 
(COND
 ((NOT (MEMQ 'INEQSEVAL SOLVEMETHODS*))
  (SETQ SOLVEMETHODS* (CONS 'INEQSEVAL** SOLVEMETHODS*)))) 
(COND
 ((NOT (GET 'GEQ 'SIMPFN))
  (PROGN (MKOP 'LEQ) (MKOP 'GEQ) (MKOP 'LESSP) (MKOP 'GREATERP)))) 
(COND
 ((NOT (GET '*INTERVAL* 'SIMPFN))
  (PROGN
   (MKOP '*INTERVAL*)
   (INFIX (LIST '*INTERVAL*))
   (PUT '*INTERVAL* 'PRTCH " .. ")))) 
(PUT 'INEQSEVAL** 'NUMBER-OF-ARGS 1) 
(PUT 'INEQSEVAL** 'DEFINED-ON-LINE '61) 
(PUT 'INEQSEVAL** 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'INEQSEVAL** 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INEQSEVAL** (U)
    ((LAMBDA (W)
       (COND ((NULL W) NIL)
             ((EQUAL W '(FAILED))
              (COND ((SMEMQL '(LEQ GEQ LESSP GREATERP) U) W) (T NIL)))
             (T W)))
     (INEQSEVAL U))) 
(PUT 'INEQSEVAL* 'NUMBER-OF-ARGS 1) 
(PUT 'INEQSEVAL* 'DEFINED-ON-LINE '67) 
(PUT 'INEQSEVAL* 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'INEQSEVAL* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INEQSEVAL* (U)
    ((LAMBDA (W) (COND ((OR (NULL W) (EQUAL W '(FAILED))) (CAR U)) (T W)))
     (INEQSEVAL U))) 
(PUT 'INEQ_SOLVE 'PSOPFN 'INEQSEVAL*) 
(PUT 'INEQSEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'INEQSEVAL 'DEFINED-ON-LINE '73) 
(PUT 'INEQSEVAL 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'INEQSEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INEQSEVAL (U)
    (PROG (S S1 V V1 L W1 W2 ERR INEQP STR N)
      (SETQ N 0)
      (SETQ S (REVAL1 (CAR U) T))
      (SETQ S (COND ((EQCAR S 'LIST) (CDR S)) (T (LIST S))))
      (COND
       ((CDR U)
        (PROGN
         (SETQ V (REVAL1 (CADR U) T))
         (SETQ V (COND ((EQCAR V 'LIST) (CDR V)) (T (LIST V))))))
       (T (SETQ U (APPEND U (LIST (GGVARS S))))))
      (SETQ L T)
      (SETQ S1
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q S)
               STARTOVER
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (Q)
                           (COND
                            ((NOT ERR)
                             (PROGN
                              (COND
                               ((OR (ATOM Q)
                                    (NOT
                                     (MEMQ (CAR Q)
                                           '(LEQ GEQ LESSP GREATERP EQUAL))))
                                (SETQ ERR T))
                               (T
                                (PROGN
                                 (COND
                                  ((NOT (EQ (CAR Q) 'EQUAL)) (SETQ INEQP T)))
                                 (SETQ N (IPLUS2 N 1))
                                 (SETQ STR
                                         (OR STR
                                             (MEMQ (CAR Q) '(LESSP GREATERP))))
                                 (SETQ W1 (SIMP (CADR Q)))
                                 (SETQ W2 (SIMP (CADDR Q)))
                                 (SETQ V1 (UNION V1 (SOLVEVARS (LIST W1 W2))))
                                 (COND
                                  ((OR
                                    (NOT
                                     (OR (ATOM (CDR W1))
                                         (ATOM (CAR (CDR W1)))))
                                    (NOT
                                     (OR (ATOM (CDR W2))
                                         (ATOM (CAR (CDR W2))))))
                                   (SETQ L NIL)))
                                 (LIST (CAR W1) (CDR W1) (CAR W2)
                                       (CDR W2)))))))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ Q (CDR Q))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (Q)
                           (COND
                            ((NOT ERR)
                             (PROGN
                              (COND
                               ((OR (ATOM Q)
                                    (NOT
                                     (MEMQ (CAR Q)
                                           '(LEQ GEQ LESSP GREATERP EQUAL))))
                                (SETQ ERR T))
                               (T
                                (PROGN
                                 (COND
                                  ((NOT (EQ (CAR Q) 'EQUAL)) (SETQ INEQP T)))
                                 (SETQ N (IPLUS2 N 1))
                                 (SETQ STR
                                         (OR STR
                                             (MEMQ (CAR Q) '(LESSP GREATERP))))
                                 (SETQ W1 (SIMP (CADR Q)))
                                 (SETQ W2 (SIMP (CADDR Q)))
                                 (SETQ V1 (UNION V1 (SOLVEVARS (LIST W1 W2))))
                                 (COND
                                  ((OR
                                    (NOT
                                     (OR (ATOM (CDR W1))
                                         (ATOM (CAR (CDR W1)))))
                                    (NOT
                                     (OR (ATOM (CDR W2))
                                         (ATOM (CAR (CDR W2))))))
                                   (SETQ L NIL)))
                                 (LIST (CAR W1) (CDR W1) (CAR W2)
                                       (CDR W2)))))))))
                         (CAR Q)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ Q (CDR Q))
                (GO LOOPLABEL)))
      (COND ((OR ERR (NOT INEQP)) (RETURN NIL)))
      (COND ((NULL V) (SETQ V V1)))
      (SETQ L (AND L (NOT (NONLNRSYS S1 V))))
      (COND
       ((OR (GREATERP (LENGTH V1) (LENGTH V)) (NOT (SUBSETP V V1))
            (AND (NOT L) (CDR V1)))
        (RETURN '(FAILED))))
      (COND ((AND L STR) (RETURN '(FAILED))))
      (SETQ U (COND (L (LININEQSEVAL U)) (T (POLINEQEVAL U))))
      (COND ((NULL (CDR U)) (SETQ U (LIST 'LIST)))
            ((NULL (CDDR U)) (SETQ U (CADR U))))
      (RETURN U))) 
(PUT 'GGVARS 'NUMBER-OF-ARGS 1) 
(PUT 'GGVARS 'DEFINED-ON-LINE '104) 
(PUT 'GGVARS 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'GGVARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GGVARS (S)
    (PROG (V)
      (PROG (U)
        (SETQ U S)
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U) (SETQ V (GGVARS1 U V))) (CAR U))
        (SETQ U (CDR U))
        (GO LAB))
      (COND (V (SETQ V (COND ((NULL (CDR V)) (CAR V)) (T (CONS 'LIST V))))))
      (RETURN V))) 
(PUT 'GGVARS1 'NUMBER-OF-ARGS 2) 
(PUT 'GGVARS1 'DEFINED-ON-LINE '110) 
(PUT 'GGVARS1 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'GGVARS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GGVARS1 (U V)
    (COND
     ((AND (NOT (ATOM U)) (MEMBER (CAR U) '(LEQ GEQ LESSP GREATERP EQUAL)))
      (GGVARS2 (CADR U) (GGVARS2 (CADDR U) V)))
     (T NIL))) 
(PUT 'GGVARS2 'NUMBER-OF-ARGS 2) 
(PUT 'GGVARS2 'DEFINED-ON-LINE '115) 
(PUT 'GGVARS2 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'GGVARS2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GGVARS2 (U V)
    (COND ((OR (NULL U) (NUMBERP U) (AND (EQ U 'I) *COMPLEX)) V)
          ((ATOM U) (COND ((MEMBER U V) V) (T (CONS U V))))
          ((MEMQ (CAR U) '(PLUS TIMES EXPT DIFFERENCE MINUS QUOTIENT))
           (GGVARS3 (CDR U) V))
          ((MEMBER U V) V) (T (CONS U V)))) 
(PUT 'GGVARS3 'NUMBER-OF-ARGS 2) 
(PUT 'GGVARS3 'DEFINED-ON-LINE '122) 
(PUT 'GGVARS3 'DEFINED-IN-FILE 'SOLVE/INEQ.RED) 
(PUT 'GGVARS3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GGVARS3 (U V) (COND ((NULL U) V) (T (GGVARS3 (CDR U) (GGVARS2 (CAR U) V))))) 
(ENDMODULE) 