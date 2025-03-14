(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DEFINT)) 
(LOAD_PACKAGE (LIST 'LIMITS 'SPECFN2)) 
(AEVAL
 (OPERATOR
  (LIST 'M_JACOBIP 'M_GEGENBAUERP 'M_LAGUERREP 'M_HERMITEP 'M_CHEBYSHEVU
        'M_CHEBYSHEVT 'M_LEGENDREP 'STRUVEH2 'MYCOSH 'MYSINH))) 
(GLOBAL '(SPEC_COND UNKNOWN_TST PRODUCT_TST TRANSFORM_TST TRANSFORM_LST)) 
(CREATE-PACKAGE
 '(DEFINT DEFINTA DEFINTC DEFINTF DEFINTI DEFINT0 DEFINTD DEFINTG DEFINTJ
   DEFINTB DEFINTE DEFINTH DEFINTK DEFINTX)
 '(CONTRIB DEFINT)) 
(FLUID '(MELLINCOEF)) 
(SHARE (LIST 'MELLINCOEF)) 
(ENDMODULE) 