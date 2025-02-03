(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TPSSUM)) 
(PUT '|PS:SUMMATION-ERULE| 'NUMBER-OF-ARGS 2) 
(PUT '|PS:SUMMATION-ERULE| 'DEFINED-ON-LINE '44) 
(PUT '|PS:SUMMATION-ERULE| 'DEFINED-IN-FILE 'TPS/TPSSUM.RED) 
(PUT '|PS:SUMMATION-ERULE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |PS:SUMMATION-ERULE| (A N)
    (PROG (POWER COEFF SUMVAR CURRENT-INDEX LAST-EXP CURRENT-EXP)
      (SETQ CURRENT-INDEX (CADDR A))
      (SETQ SUMVAR (CADR A))
      (SETQ COEFF (CDDDR A))
      (SETQ POWER (CADR COEFF))
      (SETQ COEFF (CAR COEFF))
      (SETQ LAST-EXP (IEVAL (REVAL1 (SUBST CURRENT-INDEX SUMVAR POWER) T)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ CURRENT-INDEX (PLUS CURRENT-INDEX 1))
         (SETQ CURRENT-EXP
                 (IEVAL (REVAL1 (SUBST CURRENT-INDEX SUMVAR POWER) T)))
         (COND
          ((LEQ CURRENT-EXP LAST-EXP)
           (RERROR 'TPS 39 "Exponent not strictly increasing: ps:summation")))
         (COND
          ((LESSP CURRENT-EXP N)
           (PROGN
            (|PS:SET-TERM| PS CURRENT-EXP
                           (SIMP* (SUBST CURRENT-INDEX SUMVAR COEFF)))
            (RPLACA (CDDR A) CURRENT-INDEX))))
         (SETQ LAST-EXP CURRENT-EXP))
        (COND ((NOT (GEQ CURRENT-EXP N)) (GO REPEATLABEL))))
      (RETURN
       (COND
        ((EQUAL CURRENT-EXP N)
         (PROGN
          (RPLACA (CDDR A) CURRENT-INDEX)
          (SIMP* (SUBST CURRENT-INDEX SUMVAR COEFF))))
        (T (CONS NIL 1)))))) 
(PUT '|PS:SUMMATION| '|PS:ERULE| '|PS:SUMMATION-ERULE|) 
(PUT '|PS:SUMMATION| 'SIMPFN 'SIMPIDEN) 
(PUT 'PSSUM 'SIMPFN 'SIMPPSSUM) 
(PUT 'SIMPPSSUM 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPPSSUM 'DEFINED-ON-LINE '71) 
(PUT 'SIMPPSSUM 'DEFINED-IN-FILE 'TPS/TPSSUM.RED) 
(PUT 'SIMPPSSUM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPPSSUM (A)
    (PROG (*NOSUBS FROM SUMVAR LOWLIM COEFF POWER DEPVAR ABOUT PSORD TERM)
      (COND
       ((NEQ (LENGTH A) 5)
        (RERROR 'TPS 40
                "Args should be <FROM>,<coeff>,<depvar>,<point>,<power>: simppssum")))
      (SETQ *NOSUBS T)
      (SETQ FROM (REVAL1 (CAR A) T))
      (SETQ *NOSUBS NIL)
      (COND ((NOT (EQEXPR FROM)) (ERRPRI2 (CAR A) T))
            (T
             (PROGN
              (SETQ SUMVAR (CADR FROM))
              (COND
               ((NOT (KERNP (SIMP* SUMVAR)))
                (TYPERR SUMVAR "kernel:  simppssum")))
              (SETQ LOWLIM (IEVAL (CADDR FROM))))))
      (SETQ COEFF (PREPSQXX (SIMP* (CADR A))))
      (SETQ A (CDDR A))
      (SETQ DEPVAR (CAR A))
      (SETQ ABOUT (PREPSQXX (SIMP* (CADR A))))
      (COND ((EQUAL ABOUT 'INFINITY) (SETQ ABOUT '|PS:INF|)))
      (SETQ POWER (PREPSQXX (SIMP* (CADDR A))))
      (COND ((NOT (KERNP (SIMP* DEPVAR))) (TYPERR DEPVAR "kernel:  simppssum"))
            ((EQUAL DEPVAR SUMVAR)
             (RERROR 'TPS 41
                     "Summation and expansion variables are the same:  simppssum"))
            ((SMEMBER DEPVAR ABOUT)
             (RERROR 'TPS 42 "Expansion point depends on depvar:  simppssum"))
            ((SMEMBER SUMVAR ABOUT)
             (RERROR 'TPS 43
                     "Expansion point depends on summation var:  simppssum"))
            ((NOT (SMEMBER SUMVAR POWER))
             (RERROR 'TPS 44
                     "Exponent does not depend on summation variable: simppssum")))
      (SETQ LOWLIM (DIFFERENCE LOWLIM 1))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ LOWLIM (PLUS LOWLIM 1))
         (SETQ PSORD (IEVAL (REVAL1 (SUBST LOWLIM SUMVAR POWER) T))))
        (COND
         ((NOT
           (NEQ (SETQ TERM (SIMP* (SUBST LOWLIM SUMVAR COEFF))) '(NIL . 1)))
          (GO REPEATLABEL))))
      (SETQ PS
              (MAKE-PS (LIST '|PS:SUMMATION| SUMVAR LOWLIM COEFF POWER)
               (LIST '|PS:SUMMATION| FROM COEFF DEPVAR ABOUT POWER) DEPVAR
               ABOUT))
      (|PS:PUTV| PS 0 PSORD)
      (|PS:SET-TERM| PS PSORD TERM)
      (RETURN (CONS PS 1)))) 
(ENDMODULE) 