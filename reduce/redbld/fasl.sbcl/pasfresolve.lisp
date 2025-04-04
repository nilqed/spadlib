(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PASFRESOLVE)) 
(REVISION 'PASFRESOLVE
          "$Id: pasfresolve.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'PASFRESOLVE "(c) 2013 M. Kosta, T. Sturm") 
(MKOP 'DIVC) 
(INFIX (LIST 'DIVC)) 
(MKOP 'MODC) 
(INFIX (LIST 'MODC)) 
(PRECEDENCE (LIST 'MODC 'TIMES)) 
(PRECEDENCE (LIST 'DIVC 'MODC)) 
(FLAG '(RLELIMMOD) 'OPFN) 
(FLAG '(RLELIMCONG) 'OPFN) 
(PUT 'RLELIMMOD 'NUMBER-OF-ARGS 1) 
(PUT 'RLELIMMOD 'DEFINED-ON-LINE '42) 
(PUT 'RLELIMMOD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'RLELIMMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLELIMMOD (F) (RL_PREPFOF (PASF_ELIMMOD (RL_SIMP F)))) 
(PUT 'RLELIMCONG 'NUMBER-OF-ARGS 1) 
(PUT 'RLELIMCONG 'DEFINED-ON-LINE '45) 
(PUT 'RLELIMCONG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'RLELIMCONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLELIMCONG (F) (RL_PREPFOF (PASF_ELIMCONG (RL_SIMP F)))) 
(SWITCH (LIST 'RLRESOLVEUNIVERSAL)) 
(OFF1 'RLRESOLVEUNIVERSAL) 
(PUT 'PASF_RESOLVE 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_RESOLVE 'DEFINED-ON-LINE '55) 
(PUT 'PASF_RESOLVE 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RESOLVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_RESOLVE (F) (CL_APPLY2ATS F 'PASF_RESOLVEAT)) 
(PUT 'PASF_RESOLVEAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_RESOLVEAT 'DEFINED-ON-LINE '60) 
(PUT 'PASF_RESOLVEAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RESOLVEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_RESOLVEAT (ATF)
    (PROG (W)
      (SETQ W (RL_SIMP (PASF_RESOLVEAT1 (RL_PREPFOF ATF))))
      (RETURN (COND (*RLRESI (CL_SIMPL W NIL (MINUS 1))) (T W))))) 
(PUT 'PASF_RESOLVEAT1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_RESOLVEAT1 'DEFINED-ON-LINE '68) 
(PUT 'PASF_RESOLVEAT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RESOLVEAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_RESOLVEAT1 (LPF)
    (PROG (OP LHS RRV)
      (SETQ OP (CAR LPF))
      (SETQ LHS (CADR LPF))
      (SETQ RRV (PASF_RESOLVETERM LHS))
      (COND ((MEMQ OP '(NCONG CONG)) (SETQ OP (CONS OP (CADDDR LPF)))))
      (COND
       ((CDR RRV)
        (RETURN
         (CONS 'OR
               (PROG (RC FORALL-RESULT FORALL-ENDPTR)
                 (SETQ RC RRV)
                 (COND ((NULL RC) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (RC) (PASF_TRANSRC OP RC)) (CAR RC))
                                  NIL)))
                LOOPLABEL
                 (SETQ RC (CDR RC))
                 (COND ((NULL RC) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (RC) (PASF_TRANSRC OP RC)) (CAR RC))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (PASF_TRANSRC OP (CAR RRV))))) 
(PUT 'PASF_RESOLVETERM 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_RESOLVETERM 'DEFINED-ON-LINE '84) 
(PUT 'PASF_RESOLVETERM 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RESOLVETERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_RESOLVETERM (LPF)
    (PROG (OP FN CPRODL)
      (COND ((ATOM LPF) (RETURN (LIST (RC_MK LPF 'TRUE NIL)))))
      (SETQ OP (CAR LPF))
      (SETQ CPRODL
              (CL_CARTPROD
               (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                 (SETQ ARG (CDR LPF))
                 (COND ((NULL ARG) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (ARG) (PASF_RESOLVETERM ARG))
                                   (CAR ARG))
                                  NIL)))
                LOOPLABEL
                 (SETQ ARG (CDR ARG))
                 (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (ARG) (PASF_RESOLVETERM ARG)) (CAR ARG))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ FN (RL_RXFFN OP))
      (COND ((NOT FN) (RETURN (CL_RESOLVE-SIMPLE OP CPRODL))))
      (RETURN (CL_RESOLVE-XFN OP FN CPRODL)))) 
(PUT 'PASF_TRANSRC 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_TRANSRC 'DEFINED-ON-LINE '99) 
(PUT 'PASF_TRANSRC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_TRANSRC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_TRANSRC (OP RC)
    (PROG (W)
      (SETQ W (PASF_TRANSRC1 OP RC))
      (PROG (P)
        (SETQ P (REVERSE (RC_QL RC)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ W (LIST (CAR P) (CDR P) W))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN W))) 
(PUT 'PASF_TRANSRC1 'NUMBER-OF-ARGS 2) 
(PUT 'PASF_TRANSRC1 'DEFINED-ON-LINE '107) 
(PUT 'PASF_TRANSRC1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_TRANSRC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PASF_TRANSRC1 (OP RC)
    (COND
     ((MEMQ OP '(EQUAL NEQ))
      (LIST (COND (*RLRESOLVEUNIVERSAL 'IMPL) (T 'AND)) (RC_GUARD RC)
            (LIST OP (RC_TERM RC) 0)))
     (T
      (LIST (COND (*RLRESOLVEUNIVERSAL 'IMPL) (T 'AND)) (RC_GUARD RC)
            (COND ((ATOM OP) (LIST OP (RC_TERM RC) 0))
                  (T (LIST (CAR OP) (RC_TERM RC) 0 (CDR OP)))))))) 
(PUT 'PASF_RXFFN 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_RXFFN 'DEFINED-ON-LINE '120) 
(PUT 'PASF_RXFFN 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RXFFN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_RXFFN (OP)
    (COND ((EQ OP 'MAX) 'CL_RXFFN-MAX) ((EQ OP 'MIN) 'CL_RXFFN-MAX)
          ((EQ OP 'ABS) 'CL_RXFFN-ABS) ((EQ OP 'SIGN) 'CL_RXFFN-SIGN)
          ((EQ OP 'SQRT) 'CL_RXFFN-SQRT) ((EQ OP 'DIVC) 'PASF_RXFFN-DIVC)
          ((EQ OP 'MODC) 'PASF_RXFFN-MODC) (T NIL))) 
(PUT 'PASF_RXFFN-MODC 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_RXFFN-MODC 'DEFINED-ON-LINE '138) 
(PUT 'PASF_RXFFN-MODC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RXFFN-MODC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_RXFFN-MODC (OP ARGL CONDL QLL)
    (PROG (W A K QUANT)
      (SETQ W (GENSYM))
      (SETQ A (CAR ARGL))
      (SETQ K (CADR ARGL))
      (SETQ QUANT (COND (*RLRESOLVEUNIVERSAL (CONS 'ALL W)) (T (CONS 'EX W))))
      (RETURN
       (LIST
        (RC_MK W
               (CONS 'AND
                     (CONS (LIST 'GREATERP K 0)
                           (CONS (LIST 'CONG (LIST 'DIFFERENCE W A) 0 K)
                                 (CONS (LIST 'GEQ W 0)
                                       (CONS
                                        (LIST 'LESSP (LIST 'DIFFERENCE W K) 0)
                                        CONDL)))))
               (CONS QUANT (LTO_APPENDN QLL))))))) 
(PUT 'PASF_RXFFN-DIVC 'NUMBER-OF-ARGS 4) 
(PUT 'PASF_RXFFN-DIVC 'DEFINED-ON-LINE '152) 
(PUT 'PASF_RXFFN-DIVC 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_RXFFN-DIVC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PASF_RXFFN-DIVC (OP ARGL CONDL QLL)
    (PROG (W A K QUANT)
      (SETQ W (GENSYM))
      (SETQ A (CAR ARGL))
      (SETQ K (CADR ARGL))
      (SETQ QUANT (COND (*RLRESOLVEUNIVERSAL (CONS 'ALL W)) (T (CONS 'EX W))))
      (RETURN
       (LIST
        (RC_MK W
               (CONS 'AND
                     (CONS (LIST 'GREATERP K 0)
                           (CONS
                            (LIST 'LEQ (LIST 'DIFFERENCE (LIST 'TIMES W K) A)
                                  0)
                            (CONS
                             (LIST 'GREATERP
                                   (LIST 'PLUS K (LIST 'TIMES W K)
                                         (LIST 'MINUS A))
                                   0)
                             CONDL))))
               (CONS QUANT (LTO_APPENDN QLL))))))) 
(PUT 'PASF_ELIMMOD 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMMOD 'DEFINED-ON-LINE '166) 
(PUT 'PASF_ELIMMOD 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMMOD (F) (CL_APPLY2ATS F 'PASF_ELIMMODAT)) 
(PUT 'PASF_ELIMMODAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMMODAT 'DEFINED-ON-LINE '171) 
(PUT 'PASF_ELIMMODAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMMODAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMMODAT (ATF)
    (PROG (W)
      (SETQ W (RL_SIMP (PASF_ELIMMODAT1 (RL_PREPFOF ATF))))
      (RETURN (COND (*RLRESI (CL_SIMPL W NIL (MINUS 1))) (T W))))) 
(PUT 'PASF_ELIMMODAT1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMMODAT1 'DEFINED-ON-LINE '179) 
(PUT 'PASF_ELIMMODAT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMMODAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMMODAT1 (LPF)
    (PROG (OP NLHS)
      (SETQ OP (CAR LPF))
      (SETQ NLHS (PASF_ELIMMODTERM (CADR LPF)))
      (COND ((MEMQ OP '(NCONG CONG)) (RETURN (LIST OP NLHS 0 (CADDDR LPF)))))
      (RETURN (LIST OP NLHS 0)))) 
(PUT 'PASF_ELIMMODTERM 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMMODTERM 'DEFINED-ON-LINE '190) 
(PUT 'PASF_ELIMMODTERM 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMMODTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMMODTERM (PF)
    (PROG (OP ARGL NARGL RES)
      (COND ((ATOM PF) (RETURN PF)))
      (SETQ OP (CAR PF))
      (SETQ ARGL (CDR PF))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL ARGL))) (RETURN NIL)))
        (PROGN
         (SETQ NARGL (CONS (PASF_ELIMMODTERM (CAR ARGL)) NARGL))
         (SETQ ARGL (CDR ARGL)))
        (GO WHILELABEL))
      (SETQ NARGL (REVERSE NARGL))
      (COND
       ((EQ OP 'MODC)
        (PROGN
         (SETQ RES
                 (CONS 'TIMES
                       (CONS (CADR NARGL)
                             (LIST (LIST 'DIVC (CAR NARGL) (CADR NARGL))))))
         (SETQ RES (CONS 'DIFFERENCE (CONS (CAR NARGL) (LIST RES))))
         (RETURN RES))))
      (RETURN (CONS OP NARGL)))) 
(PUT 'PASF_ELIMCONG 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMCONG 'DEFINED-ON-LINE '213) 
(PUT 'PASF_ELIMCONG 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMCONG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMCONG (F) (CL_APPLY2ATS F 'PASF_ELIMCONGAT)) 
(PUT 'PASF_ELIMCONGAT 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMCONGAT 'DEFINED-ON-LINE '218) 
(PUT 'PASF_ELIMCONGAT 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMCONGAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMCONGAT (ATF)
    (PROG (W)
      (SETQ W (PASF_ELIMCONGAT1 ATF))
      (RETURN (COND (*RLRESI (CL_SIMPL W NIL (MINUS 1))) (T W))))) 
(PUT 'PASF_ELIMCONGAT1 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMCONGAT1 'DEFINED-ON-LINE '226) 
(PUT 'PASF_ELIMCONGAT1 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMCONGAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMCONGAT1 (ATF)
    (PROG (W NLHS M RES)
      (COND
       ((NOT
         (AND (PAIRP ATF) (PAIRP (CAR ATF)) (MEMQ (CAAR ATF) '(CONG NCONG))))
        (RETURN ATF)))
      (SETQ W (GENSYM))
      (SETQ M (CDAR ATF))
      (SETQ NLHS
              (ADDF (CADR ATF)
                    (NEGF
                     ((LAMBDA (G207)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF M G207))
                              (T (POLY-MULTF M G207))))
                      (LIST (CONS (CONS W 1) 1))))))
      (COND
       ((EQ
         (COND ((OR (EQ ATF 'TRUE) (EQ ATF 'FALSE)) ATF)
               ((PAIRP (CAR ATF)) (CAAR ATF)) (T (CAR ATF)))
         'CONG)
        (RETURN (LIST 'EX W (LIST 'EQUAL NLHS NIL)))))
      (RETURN (LIST 'ALL W (LIST 'NEQ NLHS NIL))))) 
(PUT 'PASF_ELIMCONGAT2 'NUMBER-OF-ARGS 1) 
(PUT 'PASF_ELIMCONGAT2 'DEFINED-ON-LINE '240) 
(PUT 'PASF_ELIMCONGAT2 'DEFINED-IN-FILE 'REDLOG/PASF/PASFRESOLVE.RED) 
(PUT 'PASF_ELIMCONGAT2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PASF_ELIMCONGAT2 (LPF)
    (PROG (OP M VAR NLHS)
      (SETQ OP (CAR LPF))
      (COND ((NOT (MEMQ OP '(CONG NCONG))) (RETURN LPF)))
      (SETQ M (CADDDR LPF))
      (SETQ VAR (GENSYM))
      (SETQ NLHS (LIST 'DIFFERENCE (CADR LPF) (CADDR LPF)))
      (COND
       ((EQ OP 'CONG)
        (RETURN (LIST 'EX VAR (LIST 'EQUAL NLHS (LIST 'TIMES VAR M))))))
      (RETURN (LIST 'ALL VAR (LIST 'NEQ NLHS (LIST 'TIMES VAR M)))))) 
(ENDMODULE) 