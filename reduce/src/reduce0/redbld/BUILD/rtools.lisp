(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RTOOLS)) 
(CREATE-PACKAGE
 '(RTOOLS GENERAL RPRINTF RANDOM GENMOD SMALLMOD SORT SIMPLERTRACE) NIL) 
(FLAG '(RTOOLS) 'CORE_PACKAGE) 
(ENDMODULE) 