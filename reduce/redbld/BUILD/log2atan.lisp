(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LOG2ATAN)) 
(FLUID '(*RATIONALIZE *TRA GAUSSIANI *TRMIN INTVAR)) 
(EXPORTS (LIST 'COMBINE-LOGS)) 
(PUT 'COMBINE-LOGS 'NUMBER-OF-ARGS 2) 
(PUT 'COMBINE-LOGS 'DEFINED-ON-LINE '36) 
(PUT 'COMBINE-LOGS 'DEFINED-IN-FILE 'ALGINT/LOG2ATAN.RED) 
(PUT 'COMBINE-LOGS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COMBINE-LOGS (COEF LOGARG)
    (PROG (ANS DENCOEF PARTS LOGS LPARTS *RATIONALIZE TRUEIMAG)
      (SETQ *RATIONALIZE T)
      (SETQ COEF (SIMP* COEF))
      (COND ((NULL (CAR LOGARG)) (RETURN COEF)))
      (SETQ PARTS (SPLIT-REAL-IMAG (CAR COEF)))
      (COND ((NULL (CAR (CDR PARTS))) (RETURN (MULTSQ COEF LOGARG))))
      (SETQ DENCOEF
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CDR COEF) (CDR LOGARG)))
                    (T (POLY-MULTF (CDR COEF) (CDR LOGARG)))))
      (COND
       (*TRA
        (PROGN
         (PROGN
          (PRIN2 "attempting to find 'real' form for")
          (TERPRI)
          "attempting to find 'real' form for")
         (MATHPRINT
          (LIST 'TIMES
                (LIST 'PLUS (PREPSQ (CAR PARTS))
                      (LIST 'TIMES (PREPSQ (CDR PARTS)) 'I))
                (PREPSQ LOGARG))))))
      (SETQ LOGARG (CAR LOGARG))
      (SETQ LOGS (CONS 1 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (PAIRP LOGARG)) (RETURN NIL)))
        (PROGN
         (COND ((NEQ (CDAAR LOGARG) 1) (INTERR "what a log")))
         (COND ((ATOM (CAAAR LOGARG)) (INTERR "what a log")))
         (COND ((NEQ (CAR (CAAAR LOGARG)) 'LOG) (INTERR "what a log")))
         (SETQ LOGS
                 (*MULTSQ LOGS
                          (*EXPTSQ (SIMP* (CADR (CAAAR LOGARG)))
                                   (CDAR LOGARG))))
         (SETQ LOGARG (CDR LOGARG)))
        (GO WHILELABEL))
      (SETQ LOGS (RATIONALIZESQ LOGS))
      (SETQ ANS (MULTSQ (*MULTSQ (CAR PARTS) LOGS) (CONS 1 DENCOEF)))
      (SETQ LPARTS (SPLIT-REAL-IMAG (CAR LOGS)))
      (COND
       ((CAR (DIFFF (CDR (CDR LPARTS)) INTVAR))
        (INTERR "unexpected denominator"))
       ((NULL (CAR (CDR LPARTS))) (RETURN (MULTSQ COEF LOGARG))))
      (SETQ LPARTS
              (CONS (*MULTSQ (CONS (CDR (CDR LPARTS)) 1) (CAR LPARTS))
                    (CDR LPARTS)))
      (COND
       ((NOT (ONEP (CDR (CAR LPARTS)))) (INTERR "unexpected denominator")))
      (SETQ TRUEIMAG
              (MULTSQ
               (CONS
                (ADDF (*EXPTF (CAR (CAR LPARTS)) 2)
                      (*EXPTF (CAR (CDR LPARTS)) 2))
                1)
               (INVSQ (CONS (*EXPTF (CDR LOGS) 2) 1))))
      (COND
       ((CAR (DIFFSQ TRUEIMAG INTVAR))
        (SETQ ANS
                (*ADDSQ ANS
                        (*MULTSQ
                         (CONS GAUSSIANI
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF 2 DENCOEF))
                                     (T (POLY-MULTF 2 DENCOEF))))
                         (*MULTSQ (SIMPLOGSQ TRUEIMAG) (CDR PARTS)))))))
      (SETQ TRUEIMAG
              (*MULTSQ (CAR LPARTS) (*INVSQ (CONS (CAR (CDR LPARTS)) 1))))
      (COND
       ((CAR (DIFFSQ TRUEIMAG INTVAR))
        (SETQ ANS
                (*ADDSQ ANS
                        (*MULTSQ (*MULTSQ (CDR PARTS) (CONS 1 DENCOEF))
                                 (CONS
                                  (LIST
                                   (CONS
                                    (CONS (LIST 'ATAN (PREPSQ* TRUEIMAG)) 1)
                                    1))
                                  1))))))
      (RETURN ANS))) 
(PUT 'SPLIT-REAL-IMAG 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT-REAL-IMAG 'DEFINED-ON-LINE '92) 
(PUT 'SPLIT-REAL-IMAG 'DEFINED-IN-FILE 'ALGINT/LOG2ATAN.RED) 
(PUT 'SPLIT-REAL-IMAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT-REAL-IMAG (SF)
    (COND ((NULL SF) ((LAMBDA (Z) (CONS Z Z)) (CONS NIL 1)))
          ((NUMBERP SF) (CONS (CONS SF 1) (CONS NIL 1)))
          ((OR (ATOM SF) (ATOM (CAR SF)))
           (INTERR "can't handle arbitrary domains"))
          (T
           (PROG (CPARTS RPARTS MV TMP)
             (SETQ CPARTS (SPLIT-REAL-IMAG (CDAR SF)))
             (SETQ RPARTS (SPLIT-REAL-IMAG (CDR SF)))
             (SETQ MV (SPLIT-REAL-IMAGVAR (CAAAR SF)))
             (COND
              ((NULL (CAR (CDR MV)))
               (PROGN
                (SETQ TMP (CONS (CONS (CONS (CAAR SF) 1) NIL) 1))
                (RETURN
                 (CONS (*ADDSQ (*MULTSQ (CAR CPARTS) TMP) (CAR RPARTS))
                       (*ADDSQ (*MULTSQ (CDR CPARTS) TMP) (CDR RPARTS)))))))
             (COND
              ((NULL (CAR (CAR MV)))
               (PROGN
                (SETQ MV (*EXPTSQ (CDR MV) (CDAAR SF)))
                (COND
                 ((NOT (EVENP (QUOTIENT (CDAAR SF) 2))) (SETQ MV (NEGSQ MV))))
                (COND
                 ((NOT (EVENP (CDAAR SF)))
                  (RETURN
                   (CONS
                    (*ADDSQ (*MULTSQ (NEGSQ (CDR CPARTS)) MV) (CAR RPARTS))
                    (*ADDSQ (*MULTSQ (CAR CPARTS) MV) (CDR RPARTS)))))
                 (T
                  (RETURN
                   (CONS (*ADDSQ (*MULTSQ (CAR CPARTS) MV) (CAR RPARTS))
                         (*ADDSQ (*MULTSQ (CDR CPARTS) MV) (CDR RPARTS)))))))))
             (SETQ CPARTS (MUL-COMPLEX CPARTS (EXP-COMPLEX MV (CDAAR SF))))
             (RETURN
              (CONS (*ADDSQ (CAR CPARTS) (CAR RPARTS))
                    (*ADDSQ (CDR CPARTS) (CDR RPARTS)))))))) 
(PUT 'MUL-COMPLEX 'NUMBER-OF-ARGS 2) 
(PUT 'MUL-COMPLEX 'DEFINED-ON-LINE '122) 
(PUT 'MUL-COMPLEX 'DEFINED-IN-FILE 'ALGINT/LOG2ATAN.RED) 
(PUT 'MUL-COMPLEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MUL-COMPLEX (A B)
    (CONS (*ADDSQ (*MULTSQ (NEGSQ (CDR A)) (CDR B)) (*MULTSQ (CAR A) (CAR B)))
          (*ADDSQ (*MULTSQ (CAR A) (CDR B)) (*MULTSQ (CDR A) (CAR B))))) 
(PUT 'EXP-COMPLEX 'NUMBER-OF-ARGS 2) 
(PUT 'EXP-COMPLEX 'DEFINED-ON-LINE '126) 
(PUT 'EXP-COMPLEX 'DEFINED-IN-FILE 'ALGINT/LOG2ATAN.RED) 
(PUT 'EXP-COMPLEX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXP-COMPLEX (A N)
    (COND ((EQUAL N 1) A)
          ((EVENP N) (EXP-COMPLEX (MUL-COMPLEX A A) (QUOTIENT N 2)))
          (T (MUL-COMPLEX A (EXP-COMPLEX (MUL-COMPLEX A A) (QUOTIENT N 2)))))) 
(PUT 'SPLIT-REAL-IMAGVAR 'NUMBER-OF-ARGS 1) 
(PUT 'SPLIT-REAL-IMAGVAR 'DEFINED-ON-LINE '131) 
(PUT 'SPLIT-REAL-IMAGVAR 'DEFINED-IN-FILE 'ALGINT/LOG2ATAN.RED) 
(PUT 'SPLIT-REAL-IMAGVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPLIT-REAL-IMAGVAR (MV)
    (COND ((EQ MV 'I) (CONS (CONS NIL 1) (CONS 1 1)))
          ((ATOM MV)
           (CONS (CONS (CONS (CONS (CONS MV 1) 1) NIL) 1) (CONS NIL 1)))
          ((EQ (CAR MV) 'SQRT)
           (PROG (N RP INNERSQRT C)
             (SETQ N (SIMP* (CADR MV)))
             (COND ((NEQ (CDR N) 1) (INTERR "unexpected denominator")))
             (SETQ RP (SPLIT-REAL-IMAG (CAR N)))
             (COND
              ((AND (NULL (CAR (CDR RP))) (MINUSF (CAR (CAR RP)))
                    (NULL (INVOLVESF (CAR (CAR RP)) INTVAR)))
               (RETURN (CONS (CONS NIL 1) (SIMPSQRTSQ (NEGSQ (CAR RP)))))))
             (COND
              ((NULL (CAR (CDR RP)))
               (RETURN
                (CONS (CONS (CONS (CONS (CONS MV 1) 1) NIL) 1) (CONS NIL 1)))))
             (COND
              ((CAR (CAR RP))
               (SETQ INNERSQRT
                       (SIMPSQRTSQ
                        (*ADDSQ (*EXPTSQ (CAR RP) 2) (*EXPTSQ (CDR RP) 2)))))
              (T (SETQ INNERSQRT (CDR RP))))
             (SETQ C
                     (SIMPSQRTSQ
                      (MULTSQ (*ADDSQ (CAR RP) INNERSQRT) (CONS 1 2))))
             (RETURN
              (CONS C
                    (*MULTSQ (*MULTSQ C (*INVSQ (CDR RP)))
                             (*ADDSQ INNERSQRT (NEGSQ (CAR RP))))))))
          (T (CONS (CONS (CONS (CONS (CONS MV 1) 1) NIL) 1) (CONS NIL 1))))) 
(ENDMODULE) 