(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GREDUO)) 
(GLOBAL '(GORDER GORDERS GREDUCE_RESULT)) 
(SHARE (LIST 'GORDER)) 
(SHARE (LIST 'GORDERS)) 
(SHARE (LIST 'GREDUCE_RESULT)) 
(COND
 ((NULL GORDERS)
  (SETQ GORDERS
          (PROGN
           (SETQ ALGLIST* (CONS NIL NIL))
           '(LIST REVGRADLEX GRADLEX LEX))))) 
(PUT 'GREDUCE-ORDERS-EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GREDUCE-ORDERS-EVAL 'DEFINED-ON-LINE '39) 
(PUT 'GREDUCE-ORDERS-EVAL 'DEFINED-IN-FILE 'GROEBNER/GREDUO.RED) 
(PUT 'GREDUCE-ORDERS-EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GREDUCE-ORDERS-EVAL (U)
    (PROG (B G L O P R RR S SS V X)
      (SETQ L (LENGTH U))
      (COND ((OR (GREATERP 2 L) (LESSP 3 L)) (REDERR 'GROE4)))
      (SETQ P (REVAL1 (CAR U) T))
      (SETQ U (CDR U))
      (COND ((EQEXPR P) (SETQ P (*EQN2A P))))
      (SETQ G (REVAL1 (CAR U) T))
      (SETQ U (CDR U))
      (COND ((NOT (EQCAR G 'LIST)) (REDERR 'GROE4)))
      (PROG (GG)
        (SETQ GG (CDR G))
       LAB
        (COND ((NULL GG) (RETURN NIL)))
        ((LAMBDA (GG) (COND ((AND (NULL X) (EQEXPR GG)) (SETQ X T)))) (CAR GG))
        (SETQ GG (CDR GG))
        (GO LAB))
      (COND
       (X
        (SETQ G
                (CONS 'LIST
                      (PROG (GG FORALL-RESULT FORALL-ENDPTR)
                        (SETQ GG (CDR G))
                        (COND ((NULL GG) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (GG)
                                            (COND ((EQEXPR GG) (*EQN2A GG))
                                                  (T GG)))
                                          (CAR GG))
                                         NIL)))
                       LOOPLABEL
                        (SETQ GG (CDR GG))
                        (COND ((NULL GG) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (GG)
                                    (COND ((EQEXPR GG) (*EQN2A GG)) (T GG)))
                                  (CAR GG))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))
      (COND
       (U
        (PROGN
         (SETQ V (REVAL1 (CAR U) T))
         (COND ((NOT (EQCAR V 'LIST)) (REDERR 'GROE4))))))
      (SETQ V (CONS 'LIST (GROEBNERVARS (CDR G) V)))
      (PROG (OO)
        (SETQ OO (CDR GORDERS))
       LAB
        (COND ((NULL OO) (RETURN NIL)))
        ((LAMBDA (OO)
           (COND
            ((NULL B)
             (PROGN
              (SETQ O OO)
              (SETQ OO (COND ((EQCAR OO 'LIST) (CDR OO)) (T (CONS OO NIL))))
              (TORDER (CONS V OO))
              (SETQ RR (GREDUCEEVAL (LIST P G)))
              (SETQ SS (GREDUCE-ORDERS-SIZE RR))
              (COND
               ((OR (NULL R) (LESSP SS S))
                (PROGN
                 (SETQ GORDER (PROGN (SETQ ALGLIST* (CONS NIL NIL)) O))
                 (SETQ R RR)
                 (SETQ S SS)
                 (SETQ GREDUCE_RESULT
                         (PROGN (SETQ ALGLIST* (CONS NIL NIL)) RR)))))
              (COND ((EQUAL RR 0) (SETQ B T)))))))
         (CAR OO))
        (SETQ OO (CDR OO))
        (GO LAB))
      (RETURN R))) 
(PUT 'GREDUCE_ORDERS 'PSOPFN 'GREDUCE-ORDERS-EVAL) 
(PUT 'GREDUCE-ORDERS-SIZE 'NUMBER-OF-ARGS 1) 
(PUT 'GREDUCE-ORDERS-SIZE 'DEFINED-ON-LINE '69) 
(PUT 'GREDUCE-ORDERS-SIZE 'DEFINED-IN-FILE 'GROEBNER/GREDUO.RED) 
(PUT 'GREDUCE-ORDERS-SIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GREDUCE-ORDERS-SIZE (P)
    (COND ((ATOM P) 1)
          ((EQCAR P 'EXPT)
           ((LAMBDA (X) (PLUS 1 (GREDUCE-ORDERS-SIZE (CADR P)) (TIMES 2 X)))
            (COND
             ((AND (FIXP (CADDR P)) (GREATERP (CADDR P) 1)
                   (LESSP (CADDR P) 30))
              (CADDR P))
             (T (TIMES 5 (GREDUCE-ORDERS-SIZE (CADDR P)))))))
          (T
           (PROG (X FORALL-RESULT)
             (SETQ X P)
             (SETQ FORALL-RESULT 0)
            LAB1
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (SETQ FORALL-RESULT
                     (PLUS ((LAMBDA (X) (GREDUCE-ORDERS-SIZE X)) (CAR X))
                           FORALL-RESULT))
             (SETQ X (CDR X))
             (GO LAB1))))) 
(ENDMODULE) 