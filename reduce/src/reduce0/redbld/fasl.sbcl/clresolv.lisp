(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLRESOLVE)) 
(REVISION 'CLRESOLV
          "$Id: clresolv.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'CLRESOLV "(c) 2006-2009 A. Dolzmann, T. Sturm, 2017 T. Sturm") 
(RL_PROVIDESERVICE 'RL_RESOLVE 'CL_RESOLVE '(RL_RXFFN)) 
(PUT 'RC_TERM 'NUMBER-OF-ARGS 1) 
(PUT 'RC_TERM 'DEFINED-ON-LINE '38) 
(PUT 'RC_TERM 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'RC_TERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RC_TERM (RC) (CAR RC)) 
(PUT 'RC_GUARD 'NUMBER-OF-ARGS 1) 
(PUT 'RC_GUARD 'DEFINED-ON-LINE '41) 
(PUT 'RC_GUARD 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'RC_GUARD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RC_GUARD (RC) (CADR RC)) 
(PUT 'RC_QL 'NUMBER-OF-ARGS 1) 
(PUT 'RC_QL 'DEFINED-ON-LINE '44) 
(PUT 'RC_QL 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'RC_QL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RC_QL (RC) (CADDR RC)) 
(PUT 'RC_MK 'NUMBER-OF-ARGS 3) 
(PUT 'RC_MK 'DEFINED-ON-LINE '47) 
(PUT 'RC_MK 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'RC_MK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RC_MK (TERM GUARD QL) (LIST TERM GUARD QL)) 
(PUT 'CL_RESOLVE 'NUMBER-OF-ARGS 1) 
(PUT 'CL_RESOLVE 'DEFINED-ON-LINE '50) 
(PUT 'CL_RESOLVE 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_RESOLVE (F) (CL_APPLY2ATS F 'CL_RESOLVEAT)) 
(PUT 'CL_RESOLVEAT 'NUMBER-OF-ARGS 1) 
(PUT 'CL_RESOLVEAT 'DEFINED-ON-LINE '53) 
(PUT 'CL_RESOLVEAT 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_RESOLVEAT (ATF)
    (PROG (W)
      (SETQ W (RL_SIMP (CL_RESOLVE1 (RL_PREPFOF ATF))))
      (RETURN (COND (*RLRESI (CL_SIMPL W NIL (MINUS 1))) (T W))))) 
(PUT 'CL_RESOLVE1 'NUMBER-OF-ARGS 1) 
(PUT 'CL_RESOLVE1 'DEFINED-ON-LINE '65) 
(PUT 'CL_RESOLVE1 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_RESOLVE1 (LPF)
    (PROG (OP LHS RRV)
      (SETQ OP (CAR LPF))
      (SETQ LHS (CADR LPF))
      (SETQ RRV (CL_RESOLVE2 LHS))
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
                                  ((LAMBDA (RC) (CL_TRANSRC OP RC)) (CAR RC))
                                  NIL)))
                LOOPLABEL
                 (SETQ RC (CDR RC))
                 (COND ((NULL RC) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (RC) (CL_TRANSRC OP RC)) (CAR RC))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (CL_TRANSRC OP (CAR RRV))))) 
(PUT 'CL_TRANSRC 'NUMBER-OF-ARGS 2) 
(PUT 'CL_TRANSRC 'DEFINED-ON-LINE '78) 
(PUT 'CL_TRANSRC 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_TRANSRC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_TRANSRC (OP RC)
    (PROG (W)
      (SETQ W (CL_TRANSRC1 OP RC))
      (PROG (P)
        (SETQ P (REVERSE (RC_QL RC)))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ W (LIST (CAR P) (CDR P) W))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN W))) 
(PUT 'CL_TRANSRC1 'NUMBER-OF-ARGS 2) 
(PUT 'CL_TRANSRC1 'DEFINED-ON-LINE '86) 
(PUT 'CL_TRANSRC1 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_TRANSRC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_TRANSRC1 (OP RC)
    (COND
     ((OR (EQ OP 'EQUAL) (EQ OP 'NEQ))
      (LIST 'AND (RC_GUARD RC) (LIST OP (RC_TERM RC) 0)))
     (T (LIST 'AND (RC_GUARD RC) (LIST OP (RC_TERM RC) 0))))) 
(PUT 'CL_RESOLVE2 'NUMBER-OF-ARGS 1) 
(PUT 'CL_RESOLVE2 'DEFINED-ON-LINE '92) 
(PUT 'CL_RESOLVE2 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVE2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_RESOLVE2 (LPF)
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
                                  ((LAMBDA (ARG) (CL_RESOLVE2 ARG)) (CAR ARG))
                                  NIL)))
                LOOPLABEL
                 (SETQ ARG (CDR ARG))
                 (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (ARG) (CL_RESOLVE2 ARG)) (CAR ARG))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ FN (RL_RXFFN OP))
      (COND ((NOT FN) (RETURN (CL_RESOLVE-SIMPLE OP CPRODL))))
      (RETURN (CL_RESOLVE-XFN OP FN CPRODL)))) 
(PUT 'CL_CARTPROD 'NUMBER-OF-ARGS 1) 
(PUT 'CL_CARTPROD 'DEFINED-ON-LINE '106) 
(PUT 'CL_CARTPROD 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_CARTPROD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CL_CARTPROD (RRVL)
    (PROG (W)
      (COND
       ((NULL (CDR RRVL))
        (RETURN
         (PROG (RC FORALL-RESULT FORALL-ENDPTR)
           (SETQ RC (CAR RRVL))
           (COND ((NULL RC) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (RC)
                               (LIST (LIST (RC_TERM RC)) (LIST (RC_GUARD RC))
                                     (LIST (RC_QL RC))))
                             (CAR RC))
                            NIL)))
          LOOPLABEL
           (SETQ RC (CDR RC))
           (COND ((NULL RC) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (RC)
                       (LIST (LIST (RC_TERM RC)) (LIST (RC_GUARD RC))
                             (LIST (RC_QL RC))))
                     (CAR RC))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (SETQ W (CL_CARTPROD (CDR RRVL)))
      (RETURN
       (PROG (RC FORALL-RESULT FORALL-ENDPTR)
         (SETQ RC (CAR RRVL))
        STARTOVER
         (COND ((NULL RC) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (RC)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X W)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (LIST (CONS (RC_TERM RC) (CAR X))
                                                (CONS (RC_GUARD RC) (CADR X))
                                                (CONS (RC_QL RC) (CADDR X))))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (LIST (CONS (RC_TERM RC) (CAR X))
                                        (CONS (RC_GUARD RC) (CADR X))
                                        (CONS (RC_QL RC) (CADDR X))))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR RC)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ RC (CDR RC))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL RC) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (RC)
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X W)
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (X)
                                          (LIST (CONS (RC_TERM RC) (CAR X))
                                                (CONS (RC_GUARD RC) (CADR X))
                                                (CONS (RC_QL RC) (CADDR X))))
                                        (CAR X))
                                       NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (X)
                                  (LIST (CONS (RC_TERM RC) (CAR X))
                                        (CONS (RC_GUARD RC) (CADR X))
                                        (CONS (RC_QL RC) (CADDR X))))
                                (CAR X))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
                  (CAR RC)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ RC (CDR RC))
         (GO LOOPLABEL))))) 
(PUT 'CL_RESOLVE-SIMPLE 'NUMBER-OF-ARGS 2) 
(PUT 'CL_RESOLVE-SIMPLE 'DEFINED-ON-LINE '117) 
(PUT 'CL_RESOLVE-SIMPLE 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVE-SIMPLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CL_RESOLVE-SIMPLE (OP CPRODL)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X CPRODL)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (RC_MK (CONS OP (CAR X)) (CONS 'AND (CADR X))
                           (LTO_APPENDN (CADDR X))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (RC_MK (CONS OP (CAR X)) (CONS 'AND (CADR X))
                   (LTO_APPENDN (CADDR X))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CL_RESOLVE-XFN 'NUMBER-OF-ARGS 3) 
(PUT 'CL_RESOLVE-XFN 'DEFINED-ON-LINE '124) 
(PUT 'CL_RESOLVE-XFN 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RESOLVE-XFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_RESOLVE-XFN (OP FN CPRODL)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X CPRODL)
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT ((LAMBDA (X) (APPLY FN (CONS OP X))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR ((LAMBDA (X) (APPLY FN (CONS OP X))) (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'CL_RXFFN-MAX 'NUMBER-OF-ARGS 4) 
(PUT 'CL_RXFFN-MAX 'DEFINED-ON-LINE '128) 
(PUT 'CL_RXFFN-MAX 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RXFFN-MAX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_RXFFN-MAX (OP ARGL CONDL QLL)
    (PROG (REL)
      (SETQ REL (COND ((EQ OP 'MAX) 'GEQ) (T 'LEQ)))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X ARGL)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          (RC_MK (CAR X) (CL_MAXCOND REL ARGL X CONDL)
                           (LTO_APPENDN QLL))
                          NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  (RC_MK (CAR X) (CL_MAXCOND REL ARGL X CONDL)
                   (LTO_APPENDN QLL))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'CL_MAXCOND 'NUMBER-OF-ARGS 4) 
(PUT 'CL_MAXCOND 'DEFINED-ON-LINE '135) 
(PUT 'CL_MAXCOND 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_MAXCOND 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_MAXCOND (REL ARGL RESTARGL CONDL)
    (PROG (W)
      (SETQ W
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y ARGL)
               STARTOVER
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (COND
                         ((NOT (EQ Y RESTARGL))
                          (LIST
                           (LIST REL (LIST 'DIFFERENCE (CAR RESTARGL) (CAR Y))
                                 0)))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ Y (CDR Y))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (COND
                         ((NOT (EQ Y RESTARGL))
                          (LIST
                           (LIST REL (LIST 'DIFFERENCE (CAR RESTARGL) (CAR Y))
                                 0)))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ Y (CDR Y))
                (GO LOOPLABEL)))
      (RETURN (CONS 'AND (NCONC W CONDL))))) 
(PUT 'CL_RXFFN-ABS 'NUMBER-OF-ARGS 4) 
(PUT 'CL_RXFFN-ABS 'DEFINED-ON-LINE '143) 
(PUT 'CL_RXFFN-ABS 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RXFFN-ABS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_RXFFN-ABS (OP ARGL CONDL QLL)
    (COND
     ((CDR ARGL) (REDERR (LIST "cl_rxffn!-abs: length(argl)=" (LENGTH ARGL))))
     (T
      (LIST
       (RC_MK (CAR ARGL) (CONS 'AND (CONS (LIST 'GEQ (CAR ARGL) 0) CONDL))
        (LTO_APPENDN QLL))
       (RC_MK (LIST 'MINUS (CAR ARGL))
        (CONS 'AND (CONS (LIST 'LEQ (CAR ARGL) 0) CONDL)) (LTO_APPENDN QLL)))))) 
(PUT 'CL_RXFFN-SIGN 'NUMBER-OF-ARGS 4) 
(PUT 'CL_RXFFN-SIGN 'DEFINED-ON-LINE '156) 
(PUT 'CL_RXFFN-SIGN 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RXFFN-SIGN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_RXFFN-SIGN (OP ARGL CONDL QLL)
    (COND
     ((CDR ARGL) (REDERR (LIST "cl_rxffn!-abs: length(argl)=" (LENGTH ARGL))))
     (T
      (LIST
       (RC_MK 1 (CONS 'AND (CONS (LIST 'GREATERP (CAR ARGL) 0) CONDL))
        (LTO_APPENDN QLL))
       (RC_MK (MINUS 1) (CONS 'AND (CONS (LIST 'LESSP (CAR ARGL) 0) CONDL))
        (LTO_APPENDN QLL))
       (RC_MK 0 (CONS 'AND (CONS (LIST 'EQUAL (CAR ARGL) 0) CONDL))
        (LTO_APPENDN QLL)))))) 
(PUT 'CL_RXFFN-SQRT 'NUMBER-OF-ARGS 4) 
(PUT 'CL_RXFFN-SQRT 'DEFINED-ON-LINE '173) 
(PUT 'CL_RXFFN-SQRT 'DEFINED-IN-FILE 'REDLOG/CL/CLRESOLV.RED) 
(PUT 'CL_RXFFN-SQRT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CL_RXFFN-SQRT (OP ARGL CONDL QLL)
    (PROG (W)
      (SETQ W (INTERN (GENSYM)))
      (RETURN
       (LIST
        (RC_MK W
         (CONS 'AND
               (CONS
                (LIST 'EQUAL (LIST 'DIFFERENCE (LIST 'EXPT W 2) (CAR ARGL)) 0)
                (CONS (LIST 'GEQ W 0) CONDL)))
         (CONS (CONS 'EX W) (LTO_APPENDN QLL))))))) 
(ENDMODULE) 