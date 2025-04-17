(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RUBI-RED)) 
(CREATE-PACKAGE '(RUBI_RED RUBI_PARSE RUBI_RULES) NIL) 
(PUT 'READ_RUBI_RULES 'NUMBER-OF-ARGS 0) 
(SAFE-PUTD 'READ_RUBI_RULES 'EXPR
           (FUNCTION
            (LAMBDA () (DO-AUTOLOAD 'READ_RUBI_RULES (LIST) '(RUBI_PARSE))))) 
(ENDMODULE) 