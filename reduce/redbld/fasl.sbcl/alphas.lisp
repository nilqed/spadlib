(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ALPHAS)) 
(FLUID '(ALPHALIST CURRENT-MODULUS HENSEL-GROWTH-SIZE NUMBER-OF-FACTORS)) 
(PUT 'GET-ALPHA 'NUMBER-OF-ARGS 1) 
(PUT 'GET-ALPHA 'DEFINED-ON-LINE '39) 
(PUT 'GET-ALPHA 'DEFINED-IN-FILE 'FACTOR/ALPHAS.RED) 
(PUT 'GET-ALPHA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GET-ALPHA (POLY)
    (PROG (W)
      (SETQ W (ASSOC-ALPHA POLY ALPHALIST))
      (COND
       ((NULL W) (ERRORF (LIST "Alpha not found for " POLY " in " ALPHALIST))))
      (RETURN W))) 
(PUT 'DIVIDE-ALL-ALPHAS 'NUMBER-OF-ARGS 1) 
(PUT 'DIVIDE-ALL-ALPHAS 'DEFINED-ON-LINE '49) 
(PUT 'DIVIDE-ALL-ALPHAS 'DEFINED-IN-FILE 'FACTOR/ALPHAS.RED) 
(PUT 'DIVIDE-ALL-ALPHAS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIVIDE-ALL-ALPHAS (N)
    (PROG (OM M NN)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ NN (MODULAR-NUMBER N))
      (SETQ M
              (MODULAR-EXPT (MODULAR-RECIPROCAL NN)
                            (IDIFFERENCE NUMBER-OF-FACTORS 1)))
      (SETQ ALPHALIST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A ALPHALIST)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (CONS (TIMES-MOD-P NN (CAR A))
                                          (TIMES-MOD-P M (CDR A))))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A)
                            (CONS (TIMES-MOD-P NN (CAR A))
                                  (TIMES-MOD-P M (CDR A))))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SET-MODULUS OM))) 
(PUT 'MULTIPLY-ALPHAS 'NUMBER-OF-ARGS 3) 
(PUT 'MULTIPLY-ALPHAS 'DEFINED-ON-LINE '62) 
(PUT 'MULTIPLY-ALPHAS 'DEFINED-IN-FILE 'FACTOR/ALPHAS.RED) 
(PUT 'MULTIPLY-ALPHAS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTIPLY-ALPHAS (N OLDPOLY NEWPOLY)
    (PROG (OM FACA)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ N (MODULAR-NUMBER N))
      (SETQ OLDPOLY (REDUCE-MOD-P OLDPOLY))
      (SETQ FACA (GET-ALPHA OLDPOLY))
      (SETQ ALPHALIST (DELETE FACA ALPHALIST))
      (SETQ ALPHALIST
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A ALPHALIST)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A)
                                    (CONS (CAR A) (TIMES-MOD-P (CDR A) N)))
                                  (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (A) (CONS (CAR A) (TIMES-MOD-P (CDR A) N)))
                          (CAR A))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ALPHALIST
              (CONS (CONS (REDUCE-MOD-P NEWPOLY) (CDR FACA)) ALPHALIST))
      (SET-MODULUS OM))) 
(PUT 'MULTIPLY-ALPHAS-RECIP 'NUMBER-OF-ARGS 3) 
(PUT 'MULTIPLY-ALPHAS-RECIP 'DEFINED-ON-LINE '77) 
(PUT 'MULTIPLY-ALPHAS-RECIP 'DEFINED-IN-FILE 'FACTOR/ALPHAS.RED) 
(PUT 'MULTIPLY-ALPHAS-RECIP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTIPLY-ALPHAS-RECIP (N OLDPOLY NEWPOLY)
    (PROG (OM W)
      (SETQ OM (SET-MODULUS HENSEL-GROWTH-SIZE))
      (SETQ N (MODULAR-RECIPROCAL (MODULAR-NUMBER N)))
      (SETQ W (MULTIPLY-ALPHAS N OLDPOLY NEWPOLY))
      (SET-MODULUS OM)
      (RETURN W))) 
(ENDMODULE) 