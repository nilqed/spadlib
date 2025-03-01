(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HORNER)) 
(FLUID '(*EXP *DIV)) 
(PUT 'HORNERSQ 'NUMBER-OF-ARGS 1) 
(PUT 'HORNERSQ 'DEFINED-ON-LINE '32) 
(PUT 'HORNERSQ 'DEFINED-IN-FILE 'POLY/HORNER.RED) 
(PUT 'HORNERSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNERSQ (U)
    (COND
     ((AND *DIV (NULL DMODE*) (NUMBERP (CDR U)) (NEQ (CDR U) 1))
      (PROGN
       (SETDMODE 'RATIONAL T)
       (SETQ U (HORNERF ((LAMBDA (*EXP) (QUOTF1 (CAR U) (CDR U))) T)))
       (SETDMODE 'RATIONAL NIL)
       (CONS U 1)))
     (T (CONS (HORNERF (CAR U)) (HORNERF (CDR U)))))) 
(PUT 'HORNERF 'NUMBER-OF-ARGS 1) 
(PUT 'HORNERF 'DEFINED-ON-LINE '40) 
(PUT 'HORNERF 'DEFINED-IN-FILE 'POLY/HORNER.RED) 
(PUT 'HORNERF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNERF (U)
    (PROGN
     ((LAMBDA (*EXP) (SETQ U (EXPND U))) T)
     ((LAMBDA (*EXP) (HORNERF1 U)) NIL))) 
(PUT 'HORNERF1 'NUMBER-OF-ARGS 1) 
(PUT 'HORNERF1 'DEFINED-ON-LINE '43) 
(PUT 'HORNERF1 'DEFINED-IN-FILE 'POLY/HORNER.RED) 
(PUT 'HORNERF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HORNERF1 (U)
    (PROG (X A B C N M)
      (SETQ N 0)
      (SETQ M 0)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN U)))
      (COND ((OR (ATOM (CDR U)) (ATOM (CAR (CDR U)))) (GO Q)))
      (COND
       ((EQUAL (SETQ X (CAAAR U)) (CAAAR (CDR U)))
        (PROGN
         (SETQ N (CDAAR U))
         (SETQ A (HORNERF1 (CDAR U)))
         (SETQ U (CDR U))
         (SETQ M (CDAAR U))
         (SETQ B (HORNERF1 (CDAR U)))
         (SETQ C (CDR U))))
       ((AND (SFP (CAAAR U)) (NOT (OR (ATOM (CDAR U)) (ATOM (CAR (CDAR U)))))
             (EQUAL (SETQ X (CAAAR (CDAR U))) (CAAAR (CDR U)))
             (GREATERP (SETQ N (CDAAR (CDAR U))) (SETQ M (CDAAR (CDR U)))))
        (PROGN
         (SETQ A
                 ((LAMBDA (G701 G702)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF G701 G702))
                          (T (POLY-MULTF G701 G702))))
                  (CAAAR U) (CDAR (CDAR U))))
         (SETQ U (CDR U))
         (SETQ B (HORNERF1 (CDAR U)))
         (SETQ C (CDR U))))
       (T (GO Q)))
      (RETURN
       (HORNERF1
        (ADDF
         ((LAMBDA (G705 G706)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G705 G706))
                  (T (POLY-MULTF G705 G706))))
          (EXPTF (LIST (CONS (CONS X 1) 1)) M)
          (ADDF
           ((LAMBDA (G703)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF G703 A))
                    (T (POLY-MULTF G703 A))))
            (EXPTF (LIST (CONS (CONS X 1) 1)) (DIFFERENCE N M)))
           B))
         C)))
     Q
      (RETURN
       (ADDF
        ((LAMBDA (G707 G708)
           (COND (*PHYSOP-LOADED (PHYSOP-MULTF G707 G708))
                 (T (POLY-MULTF G707 G708))))
         (LIST (CONS (CAAR U) 1)) (HORNERF1 (CDAR U)))
        (HORNERF1 (CDR U)))))) 
(PUT 'HORNER 'POLYFN 'HORNERF) 
(ENDMODULE) 