(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CLQENODE)) 
(REVISION 'CLQENODE
          "$Id: clqenode.red 6054 2021-09-23 12:34:07Z thomas-sturm $") 
(COPYRIGHT 'CLQENODE "(c) 2021 A. Dolzmann, T. Sturm") 
(PUT 'QENODE_NEW 'NUMBER-OF-ARGS 3) 
(DE QENODE_NEW (VARIABLES F ANSWER) (LIST 'QENODE VARIABLES F NIL NIL ANSWER)) 
(PUT 'QENODE_PRINT 'NUMBER-OF-ARGS 2) 
(DE QENODE_PRINT (NODE EXTERNALINDENT)
    (PROG (*NAT INDENT)
      (SETQ INDENT "  ")
      (IOTO_TPRIN2
       (LIST EXTERNALINDENT INDENT "{" (NTH NODE 1) ", " (NTH NODE 2) ", "
             (IOTO_SMAPRIN (RL_PREPFOF (NTH NODE 3))) ", " (NTH NODE 6) "}"))
      (RETURN NODE))) 
(PUT 'QENODE_PRINTLIST 'NUMBER-OF-ARGS 2) 
(DE QENODE_PRINTLIST (NODES EXTERNALINDENT)
    (PROG (INDENT NEXTINDENT)
      (COND ((NULL NODES) (PROGN (IOTO_PRIN2 (LIST NIL)) (RETURN NODES))))
      (SETQ INDENT "  ")
      (SETQ NEXTINDENT (LTO_SCONCAT2 EXTERNALINDENT INDENT))
      (IOTO_TPRIN2T (LIST EXTERNALINDENT "{"))
      (PROG (NODE)
        (SETQ NODE NODES)
       LAB
        (COND ((NULL NODE) (RETURN NIL)))
        ((LAMBDA (NODE) (QENODE_PRINT NODE NEXTINDENT)) (CAR NODE))
        (SETQ NODE (CDR NODE))
        (GO LAB))
      (IOTO_TPRIN2T (LIST EXTERNALINDENT "}"))
      (RETURN NODES))) 
(PUT 'QENODE_GETVARIABLES 'NUMBER-OF-ARGS 1) 
(DE QENODE_GETVARIABLES (NODE) (NTH NODE 2)) 
(PUT 'QENODE_GETFORMULA 'NUMBER-OF-ARGS 1) 
(DE QENODE_GETFORMULA (NODE) (NTH NODE 3)) 
(PUT 'QENODE_GETANSWER 'NUMBER-OF-ARGS 1) 
(DE QENODE_GETANSWER (NODE) (NTH NODE 6)) 
(ENDMODULE) 