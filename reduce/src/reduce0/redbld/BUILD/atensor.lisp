(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ATENSOR)) 
(CREATE-PACKAGE
 '(ATENSOR PERM1 PVECTOR BASIS DUMMY1 DUMMY2 TENSOR1 TENSOR TENSORIO)
 '(CONTRIB ATENSOR)) 
(ENDMODULE) 