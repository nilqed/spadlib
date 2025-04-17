(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ASSIST)) 
(CREATE-PACKAGE
 '(ASSIST SL2PSL SWITCHXT BAGLIST HCVCTORS GENPURFN CONTROL POLYEXNS TRANSFNS
   VECTOROP GRASSMAN MATREXT HELPASST)
 '(CONTRIB ASSIST)) 
(FLUID '(*NCMP)) 
(SETQ *NCMP T) 
(ENDMODULE) 