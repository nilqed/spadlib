(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MATRICES)) 
(PUT 'MKN_MATRIX 'DEFINED-ON-LINE '32) 
(PUT 'MKN_MATRIX 'DEFINED-IN-FILE 'REDUCE4/MATRIX4.RED) 
(PUT 'MKN_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKN_MATRIX (U)
    (COND ((NEQ (LENGTH U) 2) (REDERR "Incorrect matrix arguments"))
          (T (NLIST (NLIST (MKOBJECT 0 'ZERO) (CADR U)) (CAR U))))) 
(REMPROP 'MATRIX 'STAT) 
(ADDRANK0 'PRINT '(MATRIX) '((X1) (T '(PRINT_MATRIX MATRIX)))) 
(DE PRINT_MATRIX (MATRIX1) (MKOBJECT (PRINT (VALUE MATRIX1)) 'MATRIX)) 
'(NOVAL NIL) 
(PUT 'PRINT_MATRIX 'DEFINED-ON-LINE '40) 
(PUT 'PRINT_MATRIX 'DEFINED-IN-FILE 'REDUCE4/MATRIX4.RED) 
(PUT 'PRINT_MATRIX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_MATRIX (U) (MATPRI U)) 
(PUT 'MATRIX 'STAT 'RLIS) 
(PUT 'MATRIX 'N_FORMFN 'N_FORMMATR) 
(PUT 'N_FORMMATR 'DEFINED-ON-LINE '47) 
(PUT 'N_FORMMATR 'DEFINED-IN-FILE 'REDUCE4/MATRIX4.RED) 
(PUT 'N_FORMMATR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMMATR (U VARS) (N_FORMSTRUCTURE U VARS 'MATRIX)) 
(PUT 'MATRIX 'GETFN 'GETELL) 
(PUT 'MATRIX 'PUTFN 'SETELL) 
(ENDMODULE) 