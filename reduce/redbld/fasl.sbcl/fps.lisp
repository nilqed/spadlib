(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FPS)) 
(CREATE-PACKAGE '(FPS SIMPLEDE SUBSTEXP LINREC HGRSOLVE CONSTRE RATALGO)
                '(CONTRIB SPECFN)) 
(PACKAGES_TO_LOAD (LIST 'LIMITS 'FACTOR 'SPECFN 'SFGAMMA)) 
(FLUID '(|PS:ORDER-LIMIT|)) 
(SETQ |PS:ORDER-LIMIT| 30) 
(AEVAL (FACTOR (LIST 'FACTORIAL))) 
(ENDMODULE) 