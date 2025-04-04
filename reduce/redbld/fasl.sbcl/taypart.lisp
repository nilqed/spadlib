(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYPART)) 
(EXPORTS (LIST 'TAYLOR*PART)) 
(IMPORTS (LIST 'NTH 'PARTERR 'PREPTAYLOR*)) 
(PUT 'TAYLOR*PART 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOR*PART 'DEFINED-ON-LINE '49) 
(PUT 'TAYLOR*PART 'DEFINED-IN-FILE 'TAYLOR/TAYPART.RED) 
(PUT 'TAYLOR*PART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOR*PART (TAY N)
    (PROG (PREP)
      (SETQ PREP (PREPTAYLOR* TAY))
      (COND ((ATOM PREP) (PARTERR PREP N)))
      (COND ((EQUAL N 0) (RETURN (CAR PREP))))
      (SETQ PREP (CDR PREP))
      (COND
       ((LESSP N 0) (PROGN (SETQ N (MINUS N)) (SETQ PREP (REVERSE PREP)))))
      (COND ((LESSP (LENGTH PREP) N) (PARTERR TAY N)))
      (RETURN (NTH PREP N)))) 
(PUT 'TAYLOR* 'PARTOP 'TAYLOR*PART) 
(ENDMODULE) 