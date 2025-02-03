(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'DILOG)) 
(PUT '|COMPUTE:DILOG| 'NUMBER-OF-ARGS 1) 
(FLAG '(|COMPUTE:DILOG|) 'OPFN) 
(PUT '|COMPUTE:DILOG| 'DEFINED-ON-LINE '97) 
(PUT '|COMPUTE:DILOG| 'DEFINED-IN-FILE 'SPECFN/DILOG.RED) 
(PUT '|COMPUTE:DILOG| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |COMPUTE:DILOG| (X)
    (COND
     ((EVALEQUAL (AEVAL X) (AEVAL '(|:DN:| 0 . -1)))
      (AEVAL (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 6)))
     ((EVALEQUAL (AEVAL X) (AEVAL '(|:DN:| 10 . -1))) 0)
     ((EVALEQUAL (AEVAL X) (AEVAL '(|:DN:| 20 . -1)))
      (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 12))))
     ((AND (EVALGEQ (AEVAL X) (AEVAL '(|:DN:| 19 . -1)))
           (EVALLESSP (AEVAL X) (AEVAL '(|:DN:| 20 . -1))))
      (AEVAL
       (LIST 'DIFFERENCE
             (LIST 'QUOTIENT
                   (LIST '|COMPUTE:DILOG|
                         (LIST 'DIFFERENCE 1
                               (LIST 'EXPT (LIST 'DIFFERENCE X 1) 2)))
                   2)
             (LIST '|COMPUTE:DILOG|
                   (LIST 'DIFFERENCE 1 (LIST 'DIFFERENCE X 1))))))
     ((OR (EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 19 . -1)))
          (EVALLESSP (AEVAL X) (AEVAL (LIST 'MINUS '(|:DN:| 10 . -1)))))
      (AEVAL
       (LIST 'DIFFERENCE
             (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT (LIST 'LOG X) 2) 2))
             (LIST '|COMPUTE:DILOG| (LIST 'QUOTIENT 1 X)))))
     ((AND (EVALLESSP (AEVAL X) (AEVAL '(|:DN:| 5 . -1)))
           (EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 0 . -1))))
      (AEVAL
       (LIST 'PLUS
             (LIST 'MINUS
                   (LIST 'TIMES (LIST 'LOG (LIST 'DIFFERENCE 1 X))
                         (LIST 'LOG X)))
             (LIST 'DIFFERENCE (LIST 'QUOTIENT (LIST 'EXPT 'PI 2) 6)
                   (LIST '|COMPUTE:DILOG| (LIST 'DIFFERENCE 1 X))))))
     ((AND (EVALGREATERP (AEVAL X) (AEVAL '(|:DN:| 5 . -1)))
           (EVALLESSP (AEVAL X) (AEVAL '(|:DN:| 10 . -1))))
      (AEVAL
       (LIST 'DIFFERENCE
             (LIST 'MINUS (LIST 'QUOTIENT (LIST 'EXPT (LIST 'LOG X) 2) 2))
             (LIST '|COMPUTE:DILOG| (LIST 'QUOTIENT 1 X)))))
     (T
      (PROG (*UNCACHED YY SUMMA II TERM ALTERN XM1 XM1^II)
        (SETQ *UNCACHED (AEVAL 'T))
        (SETQ YY (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
        (SETQ SUMMA (AEVAL 0))
        (SETQ XM1 (AEVAL (LIST 'DIFFERENCE X '(|:DN:| 10 . -1))))
        (SETQ XM1^II (AEVAL XM1))
        (SETQ II (AEVAL 1))
        (SETQ ALTERN (AEVAL (MINUS 1)))
        (WHILE
         (EVALGREATERP
          (AEVAL*
           (LIST 'ABS
                 (SETQ TERM
                         (AEVAL*
                          (LIST 'TIMES ALTERN
                                (LIST 'QUOTIENT XM1^II
                                      (LIST 'TIMES II II)))))))
          (AEVAL* YY))
         (PROGN
          (SETQ SUMMA (AEVAL* (LIST 'PLUS SUMMA TERM)))
          (SETQ II (AEVAL* (LIST 'PLUS II 1)))
          (SETQ ALTERN (AEVAL* (LIST 'MINUS (LIST 'TIMES 1 ALTERN))))
          (SETQ XM1^II (AEVAL* (LIST 'TIMES XM1^II XM1)))))
        (RETURN (AEVAL SUMMA)))))) 
(PUT '|COMPUTE:LERCH_PHI| 'NUMBER-OF-ARGS 3) 
(FLAG '(|COMPUTE:LERCH_PHI|) 'OPFN) 
(PUT '|COMPUTE:LERCH_PHI| 'DEFINED-ON-LINE '120) 
(PUT '|COMPUTE:LERCH_PHI| 'DEFINED-IN-FILE 'SPECFN/DILOG.RED) 
(PUT '|COMPUTE:LERCH_PHI| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |COMPUTE:LERCH_PHI| (Z S A)
    (PROG (*UNCACHED YY SUMMA K TERM POW)
      (SETQ *UNCACHED (AEVAL 'T))
      (SETQ TERM (AEVAL 1))
      (SETQ POW (AEVAL 1))
      (SETQ YY
              (AEVAL
               (LIST 'EXPT 10 (LIST 'DIFFERENCE (LIST 'MINUS |:PREC:|) 3))))
      (SETQ K (AEVAL 0))
      (SETQ SUMMA (AEVAL 0))
      (WHILE (EVALGREATERP (AEVAL* TERM) (AEVAL* YY))
             (PROGN
              (COND
               ((EVALNEQ (AEVAL* (LIST 'PLUS A K)) 0)
                (PROGN
                 (SETQ TERM
                         (AEVAL*
                          (LIST 'QUOTIENT POW
                                (LIST 'EXPT (LIST 'PLUS A K) S))))
                 (SETQ SUMMA (AEVAL* (LIST 'PLUS SUMMA TERM))))))
              (SETQ POW (AEVAL* (LIST 'TIMES POW Z)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMMA)))) 
(FLAG
 '(GAMMA BETA IGAMMA IBETA M_GAMMA POLYGAMMA PSI ZETA POCHHAMMER DILOG POLYLOG
   LERCH_PHI)
 'SPECFN) 
(DEFLIST
 '((GAMMA 1) (BETA 2) (IGAMMA 2) (IBETA 3) (M_GAMMA 2) (POLYGAMMA 2) (PSI 1)
   (ZETA 1) (POCHHAMMER 2) (DILOG 1) (POLYLOG 2) (LERCH_PHI 3))
 'NUMBER-OF-ARGS) 
(ENDMODULE) 