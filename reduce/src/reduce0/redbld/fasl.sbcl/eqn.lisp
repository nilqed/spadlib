(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EQN)) 
(FLUID '(*EVALLHSEQP)) 
(SWITCH (LIST (LIST 'EQUAL 'EVALLHSEQP 'ON))) 
(PUT 'EQUALREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'EQUALREVAL 'DEFINED-ON-LINE '42) 
(PUT 'EQUALREVAL 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EQUALREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQUALREVAL (U)
    ((LAMBDA (Y)
       ((LAMBDA (X)
          (COND
           ((OR *EVALLHSEQP
                (AND (NOT (ATOM (CAR U))) (FLAGP (CAAR U) 'IMMEDIATE)))
            (LIST 'EQUAL (REVAL1 (CAR U) T) X))
           (T (LIST 'EQUAL (CAR U) X))))
        (REVAL1 Y T)))
     (CADR U))) 
(PUT 'EQUAL 'PSOPFN 'EQUALREVAL) 
(PUT 'EQUAL 'RTYPEFN 'QUOTEEQUATION) 
(PUT 'EQUAL 'I2D 'EQNERR) 
(PUT 'EQNERR 'NUMBER-OF-ARGS 1) 
(PUT 'EQNERR 'DEFINED-ON-LINE '59) 
(PUT 'EQNERR 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EQNERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQNERR (U) (TYPERR U "equation")) 
(PUT 'EQUATION 'EVFN 'EVALEQN) 
(PUT 'EVALEQN 'NUMBER-OF-ARGS 2) 
(PUT 'EVALEQN 'DEFINED-ON-LINE '79) 
(PUT 'EVALEQN 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EVALEQN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALEQN (U V)
    (PROG (E L R W OP X FOUND)
      (COND ((SETQ X (GET U 'AVALUE)) (SETQ U (CADR X))))
      (COND
       ((NOT *EVALLHSEQP)
        (PROGN
         (COND ((EQCAR U 'EQUAL) (RETURN (EQUALREVAL (CDR U))))
               (T (TYPERR U "algebraic expression when evallhseqp is off"))))))
      (SETQ OP (CAR U))
      (SETQ W (CDR U))
      (COND
       ((OR (EQUAL OP 'PLUS) (EQUAL OP 'DIFFERENCE) (EQUAL OP 'MINUS))
        (PROGN
         (PROG (Q)
           (SETQ Q W)
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q)
              (PROGN
               (SETQ Q (REVAL1 Q T))
               (COND
                ((EQCAR Q 'EQUAL)
                 (PROGN
                  (SETQ L (CONS (CADR Q) L))
                  (SETQ R (CONS (CADDR Q) R))
                  (SETQ FOUND T)))
                (T (PROGN (SETQ L (CONS Q L)) (SETQ R (CONS Q R)))))
               NIL))
            (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB))
         (SETQ R (CONS OP (REVERSE R)))
         (SETQ L (CONS OP (REVERSE L)))
         NIL))
       (T
        (PROGN
         (SETQ U
                 (CONS OP
                       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                         (SETQ Q W)
                         (COND ((NULL Q) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (Q) (REVAL1 Q T)) (CAR Q))
                                          NIL)))
                        LOOPLABEL
                         (SETQ Q (CDR Q))
                         (COND ((NULL Q) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (Q) (REVAL1 Q T)) (CAR Q))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ E (EVALEQN1 U U NIL))
         (COND
          (E
           (PROGN
            (SETQ L (SUBST (CADR E) E U))
            (SETQ R (SUBST (CADDR E) E U))
            (SETQ FOUND T))))
         NIL)))
      (COND
       ((NOT FOUND)
        (REDERR "failed to locate equal sign in equation processing")))
      (RETURN (LIST 'EQUAL (REVAL1 L V) (REVAL1 R V))))) 
(PUT 'EVALEQN1 'NUMBER-OF-ARGS 3) 
(PUT 'EVALEQN1 'DEFINED-ON-LINE '110) 
(PUT 'EVALEQN1 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EVALEQN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EVALEQN1 (U U0 E)
    (COND ((ATOM U) E)
          ((EQUAL (CAR U) 'EQUAL)
           (COND (E (TYPERR U0 "equation expression")) (T U)))
          (T (EVALEQN1 (CDR U) U0 (EVALEQN1 (CAR U) U0 E))))) 
(PUT 'LHS 'NUMBER-OF-ARGS 1) 
(PUT 'LHS 'DEFINED-ON-LINE '121) 
(PUT 'LHS 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'LHS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LHS (U) (LHS-RHS U 'CADR)) 
(PUT 'RHS 'NUMBER-OF-ARGS 1) 
(PUT 'RHS 'DEFINED-ON-LINE '125) 
(PUT 'RHS 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'RHS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RHS (U) (LHS-RHS U 'CADDR)) 
(PUT 'LHS-RHS 'NUMBER-OF-ARGS 2) 
(PUT 'LHS-RHS 'DEFINED-ON-LINE '129) 
(PUT 'LHS-RHS 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'LHS-RHS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LHS-RHS (U OP)
    (PROGN
     (COND
      ((NOT
        (AND (PAIRP U) (GET (CAR U) 'INFIX) (CDR U) (CDDR U) (NULL (CDDDR U))))
       (TYPERR U "argument for LHS or RHS")))
     (APPLY1 OP U))) 
(FLAG '(LHS RHS) 'OPFN) 
(PUT 'EQNSUB 'NUMBER-OF-ARGS 2) 
(PUT 'EQNSUB 'DEFINED-ON-LINE '140) 
(PUT 'EQNSUB 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EQNSUB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQNSUB (U V)
    (COND
     ((OR *EVALLHSEQP (AND (NOT (ATOM (CAR U))) (FLAGP (CAAR U) 'IMMEDIATE)))
      (CONS 'EQUAL
            (PROG (X FORALL-RESULT FORALL-ENDPTR)
              (SETQ X (CDR V))
              (COND ((NULL X) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (SUBEVAL1 U X)) (CAR X))
                                    NIL)))
             LOOPLABEL
              (SETQ X (CDR X))
              (COND ((NULL X) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (SUBEVAL1 U X)) (CAR X)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T (LIST 'EQUAL (CADR V) (SUBEVAL1 U (CADDR V)))))) 
(PUT 'EQUATION 'SUBFN 'EQNSUB) 
(PUT 'EQUATION 'LENGTHFN 'EQNLENGTH) 
(PUT 'EQNLENGTH 'NUMBER-OF-ARGS 1) 
(PUT 'EQNLENGTH 'DEFINED-ON-LINE '149) 
(PUT 'EQNLENGTH 'DEFINED-IN-FILE 'ALG/EQN.RED) 
(PUT 'EQNLENGTH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQNLENGTH (U) (LENGTH (CDR U))) 
(ENDMODULE) 