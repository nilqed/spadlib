(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LIEVALFM)) 
(PUT 'LIEBRACKSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'LIEBRACKSTAT 'DEFINED-ON-LINE '30) 
(PUT 'LIEBRACKSTAT 'DEFINED-IN-FILE 'EXCALC/LIEVALFM.RED) 
(PUT 'LIEBRACKSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIEBRACKSTAT NIL
    (PROG (X) (SETQ X (XREAD NIL)) (SCAN) (RETURN (CONS 'LIE (CDR X))))) 
(FLAG (LIST '}) 'DELIM) 
(PUT '{ 'STAT 'LIEBRACKSTAT) 
(PUT 'LIE 'PRIFN 'LIEPRN) 
(PUT 'LIEPRN 'NUMBER-OF-ARGS 1) 
(PUT 'LIEPRN 'DEFINED-ON-LINE '46) 
(PUT 'LIEPRN 'DEFINED-IN-FILE 'EXCALC/LIEVALFM.RED) 
(PUT 'LIEPRN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIEPRN (U) (PROGN (PRIN2* "{") (INPRINT '*COMMA* 0 U) (PRIN2* "}"))) 
(ENDMODULE) 