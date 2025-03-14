(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MRI_PASF)) 
(REVISION 'MRI_PASF
          "$Id: mri_pasf.red 6013 2021-09-09 08:22:53Z thomas-sturm $") 
(COPYRIGHT 'MRI_PASF "Copyright (c) 2008-2021 T. Sturm") 
(LOAD-PACKAGE 'REDLOG) 
(LOAD-PACKAGE 'PASF) 
(RL_COPYC 'MRI_PASF 'PASF) 
(RL_BBIADD 'MRI_PASF 'RL_SIMPLAT1* 'MRI_SIMPLAT1) 
(RL_BBIADD 'MRI_PASF 'RL_NEGATEAT* 'MRI_NEGATEAT) 
(ENDMODULE) 