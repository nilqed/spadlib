(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SPARSE)) 
(LOAD_PACKAGE (LIST 'LINALG)) 
(CREATE-PACKAGE
 '(SPARSE SPARSMAT SPMATEIG SPLINALG SPLUDCMP SPCHLSKY SPSVD SPGRMSHM)
 '(CONTRIB LINALG)) 
(ENDMODULE) 