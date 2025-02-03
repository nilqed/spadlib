(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SWITCHXT)) 
(SWITCH (LIST 'DISTRIBUTE)) 
(FLAG
 '(*FACTOR *MCD *DIV *EXP *GCD *RAT *RATIONAL *RATIONALIZE *INTSTR *REDUCED
   *RATPRI *REVPRI *DISTRIBUTE *EZGCD *COMPLEX *REDUCED *LCM *PRECISE)
 'SHARE) 
(ENDMODULE) 