(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SUSY2)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR
 (LIST '@F_F '@G_G 'SU_NEWTON 'DELTA 'B_PART 'BF_PART 'PG 'CHAN 'S_PART 'PRYKR
       'PRYKL)) 
(OPERATOR
 (LIST 'BOS 'FER 'DER 'DEL 'D 'AXP 'AXX 'ZAN 'ZEN 'FUN 'TET 'GRAS 'BER 'FIR
       'BERZ 'FIRR 'DR 'STP 'BYK 'R_R '&A 'P_P 'S_S 'WAGA)) 
(NONCOM
 (LIST 'BOS 'FER 'DER 'DEL 'D 'AXP 'AXX 'ZAN 'ZEN 'FUN 'TET 'GRAS 'BER 'FIR
       'BERZ 'FIRR 'DR 'STP 'BYK 'R_R)) 
(FACTOR (LIST '&A 'BYK)) 
(FACTOR (LIST 'FER 'BOS 'FUN 'GRAS)) 
(SETK 'CHIRAL
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY 'ABRA_KADABRA 1)
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DER (LIST '~ 'N)) 2) 0)
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DEL (LIST '~ 'N)) 2) 0)
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 2) (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'DEL 2))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DER 1))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER (LIST '~ 'N))
                         (LIST 'DEL (LIST '~ 'N)))
                   0)
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DEL (LIST '~ 'N))
                         (LIST 'DER (LIST '~ 'N)))
                   0)
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL 2))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'DER 1))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY 'B_CHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'F_CHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'B_ANTYCHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'F_ANTYCHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'K)
                               (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1) 'K))
                         (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)))
                               (LIST 'BOS 'F 3 'M))
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)))
                               (LIST 'BOS 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 3 'M)
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                               (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'FER 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                               (LIST 'BOS 'F 2 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M 'L)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                                     (LIST 'BOS 'F 2 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M 'L)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'PLUS
                                           (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                           (LIST 'FER 'F 3 'M))
                                     (LIST 'BOS 'F 1 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 3 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L
                               (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                     (LIST 'FER 'F 3 'M))
                               (LIST 'BOS 'F 1 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M 'L)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 3 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M 'L))))))) 
(SETK 'CHIRAL1
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY 'ABRA_KADABRA 3)
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DER (LIST '~ 'N)) 2)
                   (LIST 'WHEN 0 (LIST 'NEQ 'N 3)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DEL (LIST '~ 'N)) 2)
                   (LIST 'WHEN 0 (LIST 'NEQ 'N 3)))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DER 3) 2)
                   (LIST 'EXPT (LIST 'D 1) 2))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DEL 3) 2)
                   (LIST 'EXPT (LIST 'D 1) 2))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DER 1))
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST 'D 1) (LIST 'DER 3))
                               2)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 2))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'MINUS (LIST 'D 1)) (LIST 'DER 3))
                         2))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 2) (LIST 'DEL 1))
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'PLUS (LIST 'D 1) (LIST 'DEL 3))
                               2)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 1) (LIST 'DEL 2))
                   (LIST 'QUOTIENT
                         (LIST 'PLUS (LIST 'MINUS (LIST 'D 1)) (LIST 'DEL 3))
                         2))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 3))
                   (LIST 'TIMES (LIST 'D 1) (LIST 'DER 1)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DER 3))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'D 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 3) (LIST 'DER 1))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'D 1) (LIST 'DER 1))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 3) (LIST 'DER 2))
                   (LIST 'TIMES (LIST 'D 1) (LIST 'DER 2)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 1) (LIST 'DEL 3))
                   (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 1)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 2) (LIST 'DEL 3))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 2))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 3) (LIST 'DEL 1))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 1))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 3) (LIST 'DEL 2))
                   (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER (LIST '~ 'N))
                         (LIST 'DEL (LIST '~ 'N)))
                   (LIST 'COND (LIST (LIST 'EVALNEQ (LIST 'AEVAL ''N) 3) 0)
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                           2)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DEL (LIST '~ 'N))
                         (LIST 'DER (LIST '~ 'N)))
                   (LIST 'COND (LIST (LIST 'EVALNEQ (LIST 'AEVAL ''N) 3) 0)
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                           2)))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL 2))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'DER 1))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'D 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL 3))
                   (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 1)))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL 3))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'D 1) (LIST 'DEL 2))))
             (LIST 'REPLACEBY 'B_CHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'F_CHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'B_ANTYCHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY 'F_ANTYCHIRAL (LIST 'LIST))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'K)
                               (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1) 'K))
                         (LIST 'NOT (LIST 'FREEOF 'B_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'MINUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'NOT (LIST 'FREEOF 'F_CHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1) 'K)
                         (LIST 'NOT (LIST 'FREEOF 'B_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'K))
                   (LIST 'WHEN 0 (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'WHEN (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'NOT (LIST 'FREEOF 'F_ANTYCHIRAL 'F))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT
                                     (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)) 2))
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT (LIST 'BOS 'F 3 'M) 2)
                               (LIST 'TIMES (LIST 'FER 'F 2 'M)
                                     (LIST 'DER 1)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT
                                     (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)) 2))
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT (LIST 'BOS 'F 3 'M) 2)
                               (LIST 'TIMES (LIST 'DEL 1)
                                     (LIST 'FER 'F 2 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT
                                           (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                                           2))
                               (LIST 'QUOTIENT (LIST 'BOS 'F 3 'M) 2))
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT
                                           (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                                           2))
                               (LIST 'QUOTIENT (LIST 'BOS 'F 3 'M) 2))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'FER (LIST '~ 'F) 0 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 3 'M)
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'FER 'F 0 'M) (LIST 'DER 3))
                               (LIST 'TIMES 2 (LIST 'BOS 'F 1 'M)
                                     (LIST 'DER 2)))
                         (LIST 'TIMES 2 (LIST 'BOS 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'MINUS
                                           (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                                     (LIST 'TIMES
                                           (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                                           (LIST 'DER 1)))
                               (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 1)))
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 3))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                                     (LIST 'DER 2))
                               (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 2)))
                         (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 3))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 2))
                         (LIST 'DIFFERENCE
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'FER 'F 3 'M)
                                           (LIST 'DER 3))
                                     (LIST 'TIMES 2
                                           (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1))
                                           (LIST 'DER 2)))
                               (LIST 'TIMES 2
                                     (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1))
                                     (LIST 'DER 1)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 0 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS (LIST 'FER 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'FER 'F 0 'M))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2 (LIST 'DEL 2)
                                     (LIST 'BOS 'F 1 'M))
                               (LIST 'TIMES 2 (LIST 'DEL 1)
                                     (LIST 'BOS 'F 2 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 1)
                               (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'DEL 2)
                                     (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M))
                         (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 2))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES 2 (LIST 'DEL 2)
                               (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES 2 (LIST 'DEL 1)
                               (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'QUOTIENT
                                     (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)) 2))
                         (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2)
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'QUOTIENT
                                     (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)) 2)
                               (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'QUOTIENT
                                           (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                           2))
                               (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)) 2)
                         (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2)
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'BOS 'F 0 'M) (LIST 'DER 3))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2 (LIST 'FER 'F 1 'M)
                                     (LIST 'DER 2))
                               (LIST 'TIMES 2 (LIST 'FER 'F 2 'M)
                                     (LIST 'DER 1)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                               (LIST 'DER 1))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 1))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 3))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                     (LIST 'DER 2)))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 2))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 3))
                         (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 2))
                         (LIST 'TIMES 2 (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                               (LIST 'DER 1))
                         (LIST 'TIMES 2 (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))
                               (LIST 'DER 2))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 3))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'BOS 'F 0 'M))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES 2 (LIST 'DEL 2)
                                     (LIST 'FER 'F 1 'M))
                               (LIST 'TIMES 2 (LIST 'DEL 1)
                                     (LIST 'FER 'F 2 'M)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DEL 1)
                               (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 3 'M))
                               (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'BOS 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'DEL 2)
                                     (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'BOS 'F 2 'M))
                         (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 3))
                   (LIST 'PLUS (LIST 'BOS 'F 0 (LIST 'PLUS 'M 2))
                         (LIST 'TIMES 2 (LIST 'DEL 1)
                               (LIST 'FER 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES 2 (LIST 'DEL 2)
                               (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 3) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 1)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L
                               (LIST 'BOS 'F 2 'M (LIST 'DIFFERENCE 'L 1))
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'QUOTIENT
                                                 (LIST 'FER 'F 0
                                                       (LIST 'PLUS 'M 1))
                                                 2))
                                     (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2)))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L
                               (LIST 'BOS 'F 3 'M (LIST 'DIFFERENCE 'L 1))
                               (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M 'L)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'BOS 'F 2 'M
                                           (LIST 'DIFFERENCE 'L 1))
                                     (LIST 'PLUS
                                           (LIST 'MINUS
                                                 (LIST 'QUOTIENT
                                                       (LIST 'FER 'F 0
                                                             (LIST 'PLUS 'M 1))
                                                       2))
                                           (LIST 'QUOTIENT (LIST 'FER 'F 3 'M)
                                                 2))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'BOS 'F 3 'M
                                           (LIST 'DIFFERENCE 'L 1))
                                     (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'BOS 'F 1 'M
                                           (LIST 'DIFFERENCE 'L 1))
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT
                                                 (LIST 'FER 'F 0
                                                       (LIST 'PLUS 'M 1))
                                                 2)
                                           (LIST 'QUOTIENT (LIST 'FER 'F 3 'M)
                                                 2))))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 2)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 3 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L
                               (LIST 'PLUS
                                     (LIST 'QUOTIENT
                                           (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                           2)
                                     (LIST 'QUOTIENT (LIST 'FER 'F 3 'M) 2))
                               (LIST 'BOS 'F 1 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M 'L)))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 3 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 3)
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'M)
                               (LIST '~ 'L)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'DER 1)
                               (LIST 'PRYKR (LIST 'BOS 'F 'K 'M 'L) 2))
                         (LIST 'TIMES (LIST 'DER 2)
                               (LIST 'PRYKR (LIST 'BOS 'F 'K 'M 'L) 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'M)
                               (LIST '~ 'L))
                         (LIST 'DEL 3))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'PRYKL (LIST 'BOS 'F 'K 'M 'L) 2)
                                     (LIST 'DEL 1)))
                         (LIST 'TIMES (LIST 'PRYKL (LIST 'BOS 'F 'K 'M 'L) 1)
                               (LIST 'DEL 2))))))) 
(SETK 'TRAD
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY 'ABRA_KADABRA 2)
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DER (LIST '~ 'N)) 2)
                   (LIST 'D 1))
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DEL (LIST '~ 'N)) 2)
                   (LIST 'D 1))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL 2) (LIST 'DEL 1))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'DEL 1) (LIST 'DEL 2))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DER 1))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER (LIST '~ 'N))
                         (LIST 'DEL (LIST '~ 'N)))
                   (LIST 'D 1))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DEL (LIST '~ 'N))
                         (LIST 'DER (LIST '~ 'N)))
                   (LIST 'D 1))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL 2))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'DEL 2) (LIST 'DER 1))))
             (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL 1))
                   (LIST 'MINUS (LIST 'TIMES (LIST 'DEL 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'BOS 'F 3 'M))
                         (LIST 'TIMES (LIST 'FER 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'FER 'F 2 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'FER 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'BOS 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE (LIST 'BOS 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'DIFFERENCE
                         (LIST 'MINUS (LIST 'BOS 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'FER 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 3 'M)
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 1))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 2 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 3 'M))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 1 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'FER 'F 3 'M)
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'MINUS (LIST 'FER 'F 0 (LIST 'PLUS 'M 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'DEL 2))
                   (LIST 'PLUS (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 1 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                               (LIST 'BOS 'F 2 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 1)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 3 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 1 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 1 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                                     (LIST 'BOS 'F 2 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 2 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 1))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 2 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 3 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'DEL 1) (LIST 'BOS 'F 3 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                                     (LIST 'BOS 'F 1 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'BOS 'F 1 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 2 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'F 2 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'DER 2)
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 3 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'BOS 'F 3 'M 'L) (LIST 'DER 2))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 3 'M)
                               (LIST 'BOS 'F 1 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 1 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES 'L
                                     (LIST 'FER 'F 0 (LIST 'PLUS 'M 1))
                                     (LIST 'BOS 'F 2 'M
                                           (LIST 'DIFFERENCE 'L 1))))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 2 'M 'L))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M) (LIST '~ 'L))
                         (LIST 'DEL 2))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'FER 'F 1 (LIST 'PLUS 'M 1))
                               (LIST 'BOS 'F 3 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'DEL 2) (LIST 'BOS 'F 3 'M 'L))))))) 
(SETK 'DRR
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'EXPT (LIST 'D (MINUS 1)) (LIST '~ 'N))
                   (LIST 'WHEN (LIST 'DR (LIST 'MINUS 'N)) (LIST 'NEQ 'N 1)))
             (LIST 'REPLACEBY (LIST 'D (MINUS 1)) (LIST 'DR (MINUS 1)))))) 
(SETK 'NODRR
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'DR (LIST 'MINUS (LIST '~ 'N)))
                   (LIST 'WHEN (LIST 'EXPT (LIST 'D (MINUS 1)) 'N)
                         (LIST 'NEQ 'N 1)))
             (LIST 'REPLACEBY (LIST 'DR (MINUS 1)) (LIST 'D (MINUS 1)))))) 
(SETK 'CUTOFF
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'DR (LIST '~ 'N))
                   (LIST 'WHEN 0 (LIST 'LESSP 'N (LIST 'MINUS 'CUT))))))) 
(SETK 'INVERSE
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'BOS 'F 'N 'M 1))
             (LIST 'REPLACEBY (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N))
                   (LIST 'FUN 'F 'N 1))))) 
(LET
 '((LIST
    (REPLACEBY
     (TIMES (BOS (~ F) (~ N) (~ M) (~ K)) (BOS (~ G) (~ X) (~ Z) (~ V)))
     (WHEN (TIMES (BOS G X Z V) (BOS F N M K))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N X))
          (AND (EQUAL F G) (EQUAL N X) (LESSP M Z)))))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M)) (BOS (~ G) (~ X) (~ Z)))
     (WHEN (TIMES (BOS G X Z) (BOS F N M))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N X))
          (AND (EQUAL F G) (EQUAL N X) (LESSP M Z)))))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M) (~ K)) (BOS (~ G) (~ X) (~ Z)))
     (WHEN (TIMES (BOS G X Z) (BOS F N M K))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N X))
          (AND (EQUAL F G) (EQUAL N X) (LESSP M Z)))))
    (REPLACEBY (TIMES (BOS (~ G) (~ X) (~ Z)) (BOS (~ F) (~ N) (~ M) (~ K)))
     (WHEN (TIMES (BOS F N M K) (BOS G X Z))
      (OR (AND (ORDP G F) (NEQ F G)) (AND (EQUAL F G) (GREATERP N X))
          (AND (EQUAL F G) (EQUAL N X) (GREATERP M Z)))))
    (REPLACEBY
     (TIMES (BOS (~ F) (~ N) (~ M) (~ K)) (BOS (~ F) (~ N) (~ M) (~ L)))
     (BOS F N M (PLUS K L)))
    (REPLACEBY (EXPT (BOS (~ F) (~ N) (~ M) (~ K)) 2) (BOS F N M (TIMES 2 K)))
    (REPLACEBY (BOS (~ F) (~ N) (~ M) 0) 1)
    (REPLACEBY (BOS 0 (~ F) (~ N) (~ M)) 0) (REPLACEBY (BOS 0 (~ F) (~ N)) 0)
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M) (~ K)) (BOS (~ F) (~ N) (~ M)))
     (BOS F N M (PLUS K 1)))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M)) (BOS (~ F) (~ N) (~ M) (~ K)))
     (BOS F N M (PLUS K 1)))
    (REPLACEBY (TIMES (BER (~ F) (~ N) (~ M)) (BOS (~ G) (~ K) (~ X) (~ L)))
     (TIMES (BOS G K X L) (BER F N M)))
    (REPLACEBY (TIMES (FIR (~ F) (~ N) (~ M)) (BOS (~ G) (~ K) (~ X) (~ L)))
     (TIMES (BOS G K X L) (FIR F N M)))
    (REPLACEBY (TIMES (BER (~ F) (~ N) (~ M)) (BOS (~ G) (~ K) (~ L)))
     (TIMES (BOS G K L) (BER F N M)))
    (REPLACEBY (TIMES (BER (~ F) (~ N) (~ M)) (FER (~ G) (~ K) (~ L)))
     (TIMES (FER G K L) (BER F N M)))
    (REPLACEBY (TIMES (FIR (~ F) (~ N) (~ M)) (BOS (~ G) (~ K) (~ L)))
     (TIMES (BOS G K L) (FIR F N M)))
    (REPLACEBY (TIMES (FIR (~ F) (~ N) (~ M)) (FER (~ G) (~ K) (~ L)))
     (MINUS (TIMES (FER G K L) (FIR F N M))))
    (REPLACEBY (FER 0 (~ N) (~ M)) 0)
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M) (~ Y)) (FER (~ G) (~ X) (~ H)))
     (TIMES (FER G X H) (BOS F N M Y)))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M)) (FER (~ G) (~ X) (~ H)))
     (TIMES (FER G X H) (BOS F N M)))
    (REPLACEBY (EXPT (FER (~ F) (~ N) (~ M)) 2) 0)
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (FER (~ G) (~ K) (~ L)))
     (WHEN (MINUS (TIMES (FER G K L) (FER F N M)))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N K))
          (AND (EQUAL F G) (EQUAL N K) (LESSP M L)))))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (FUN (~ G) (~ K) (~ L)))
     (WHEN (TIMES (FUN G K L) (FUN F N M))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N K))
          (AND (EQUAL F G) (EQUAL N K) (LESSP M L)))))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (FUN (~ G) (~ X)))
     (WHEN (TIMES (FUN G X) (FUN F N M))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N X)))))
    (REPLACEBY (TIMES (FUN (~ G) (~ X)) (FUN (~ F) (~ N) (~ M)))
     (WHEN (TIMES (FUN F N M) (FUN G X))
      (OR (AND (ORDP G F) (NEQ F G)) (AND (EQUAL F G) (GREATERP N X)))))
    (REPLACEBY (TIMES (FUN (~ F) (~ N)) (FUN (~ G) (~ M)))
     (WHEN (TIMES (FUN G M) (FUN F N))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N M)))))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M) (~ K) (~ L)) (FUN (~ S) (~ X)))
     (TIMES (FUN S X) (FUN F N M K L)))
    (REPLACEBY
     (TIMES (FUN (~ F) (~ N) (~ M) (~ K) (~ L)) (FUN (~ S) (~ X) (~ Z)))
     (TIMES (FUN S X Z) (FUN F N M K L)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M) (~ K) (~ L)) (GRAS (~ S) (~ X)))
     (TIMES (GRAS S X) (FUN F N M K L)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M) (~ K) (~ L)) (TET (~ S)))
     (TIMES (TET S) (FUN F N M K L)))
    (REPLACEBY
     (TIMES (FUN (~ F) (~ N) (~ M) (~ K) (~ L)) (FUN (~ S) (~ X) (~ Z)))
     (TIMES (FUN S X Z) (FUN F N M K L)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (GRAS (~ G) (~ X)))
     (TIMES (GRAS G X) (FUN F N M)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N)) (GRAS (~ G) (~ X)))
     (TIMES (GRAS G X) (FUN F N)))
    (REPLACEBY (TIMES (GRAS (~ F) (~ N)) (GRAS (~ G) (~ M)))
     (WHEN (MINUS (TIMES (GRAS G M) (GRAS F N)))
      (OR (AND (ORDP F G) (NEQ F G)) (AND (EQUAL F G) (LESSP N M)))))
    (REPLACEBY (TIMES (BER (~ F) (~ N)) (FUN (~ G) (~ M)))
     (TIMES (FUN G M) (FUN F N)))
    (REPLACEBY (TIMES (BER (~ F) (~ N)) (GRAS (~ G) (~ M)))
     (TIMES (GRAS G M) (FUN F N)))
    (REPLACEBY (TIMES (FIR (~ F) (~ N)) (FUN (~ G) (~ M)))
     (TIMES (FUN G M) (FIR F N)))
    (REPLACEBY (TIMES (FIR (~ F) (~ N)) (GRAS (~ G) (~ M)))
     (MINUS (TIMES (FIR G M) (GRAS F N))))
    (REPLACEBY (EXPT (GRAS (~ F) (~ N)) 2) 0) (REPLACEBY (FUN (~ F) (~ N) 0) 1)
    (REPLACEBY (FUN 0 (~ N) (~ M)) 0) (REPLACEBY (FUN 0 (~ N)) 0)
    (REPLACEBY (GRAS 0 (~ N)) 0)
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (FUN (~ F) (~ N) (~ K)))
     (FUN F N (PLUS M K)))
    (REPLACEBY (EXPT (FUN (~ F) (~ N) (~ M)) 2) (FUN F N (TIMES 2 M)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (TET (~ K)))
     (TIMES (TET K) (FUN F N M)))
    (REPLACEBY (TIMES (FUN (~ F) (~ N)) (TET (~ K))) (TIMES (TET K) (FUN F N)))
    (REPLACEBY (TIMES (GRAS (~ F) (~ N)) (TET (~ K)))
     (MINUS (TIMES (TET K) (GRAS F N))))
    (REPLACEBY (TIMES (AXX (~ F)) (FUN (~ G) (~ N))) (TIMES (FUN G N) (AXX F)))
    (REPLACEBY (TIMES (AXX (~ F)) (GRAS (~ G) (~ N)))
     (TIMES (GRAS G N) (AXX F)))
    (REPLACEBY (TIMES (AXX (~ F)) (FUN (~ G) (~ N) (~ M)))
     (TIMES (FUN G N M) (AXX F)))
    (REPLACEBY (TIMES (AXX (~ F)) (FUN (~ G) (~ N) (~ M) (~ K) (~ L)))
     (TIMES (FUN G N M (~ K) (~ L)) (AXX F)))
    (REPLACEBY (FUN (~ F) (~ N) (~ G) (~ M) (~ K))
     (WHEN
      (PROG (S FORALL-RESULT)
        (SETQ S 0)
        (SETQ FORALL-RESULT 0)
       LAB1
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'K) S))
          (RETURN FORALL-RESULT)))
        (SETQ FORALL-RESULT
                (AEVAL*
                 (LIST 'PLUS
                       (AEVAL*
                        (LIST 'TIMES (LIST 'EXPT (MINUS 1) S)
                              (LIST 'SU_NEWTON 'K S)
                              (LIST 'FUN 'F 'N (LIST 'DIFFERENCE 'K S))
                              (LIST 'QUOTIENT (LIST 'FUN 'G 'M S)
                                    (LIST 'EXPT 2 S))))
                       FORALL-RESULT)))
        (SETQ S
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 S))
        (GO LAB1))
      (AND (NUMBERP K) (GEQ K 0))))
    (REPLACEBY (TIMES (BOS (~ G) (~ X) (~ H)) (ZAN (~ F) (~ N) (~ M)))
     (TIMES (ZAN F N M) (BOS G X H)))
    (REPLACEBY (TIMES (BOS (~ G) (~ X) (~ H) (~ L)) (ZAN (~ F) (~ N) (~ M)))
     (TIMES (ZAN F N M) (BOS G X H L)))
    (REPLACEBY (TIMES (FER (~ G) (~ X) (~ H)) (ZAN (~ F) (~ N) (~ M)))
     (TIMES (ZAN F N M) (FER G X H)))
    (REPLACEBY (TIMES (BOS (~ G) (~ X) (~ H)) (ZEN (~ F) (~ N) (~ M)))
     (TIMES (ZEN F N M) (BOS G X H)))
    (REPLACEBY (TIMES (BOS (~ G) (~ X) (~ H) (~ L)) (ZEN (~ F) (~ N) (~ M)))
     (TIMES (ZEN F N M) (BOS G X H L)))
    (REPLACEBY (TIMES (FER (~ G) (~ X) (~ H)) (ZEN (~ F) (~ N) (~ M)))
     (MINUS (TIMES (ZEN F N M) (FER G X H))))
    (REPLACEBY (TIMES (AXP (~ G)) (ZAN (~ F) (~ N) (~ M)))
     (TIMES (ZAN F N M) (AXP G)))
    (REPLACEBY (TIMES (AXP (~ G)) (ZEN (~ F) (~ N) (~ M)))
     (TIMES (ZEN F N M) (AXP G)))
    (REPLACEBY (TIMES (AXP (~ G)) (BOS (~ F) (~ N) (~ M)))
     (TIMES (BOS F N M) (AXP G)))
    (REPLACEBY (TIMES (AXP (~ G)) (BOS (~ F) (~ N) (~ M) (~ L)))
     (TIMES (BOS F N M L) (AXP G)))
    (REPLACEBY (TIMES (AXP (~ G)) (FER (~ F) (~ N) (~ M)))
     (TIMES (FER F N M) (AXP G)))
    (REPLACEBY (TIMES (AXP (~ F)) (AXP (~ G))) (AXP (PLUS F G)))
    (REPLACEBY (EXPT (AXP (~ F)) (~ N)) (AXP (TIMES N F)))))) 
(LET
 (LIST
  (LIST 'LIST
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'N)) (LIST 'DR (LIST '~ 'M)))
              (LIST 'DR (LIST 'PLUS 'N 'M)))
        (LIST 'REPLACEBY (LIST 'EXPT (LIST 'TET (LIST '~ 'N)) 2) 0)
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'TET (LIST '~ 'N)) (LIST 'TET (LIST '~ 'M)))
              (LIST 'WHEN
                    (LIST 'MINUS (LIST 'TIMES (LIST 'TET 'M) (LIST 'TET 'N)))
                    (LIST 'LESSP 'N 'M)))
        (LIST 'REPLACEBY
              (LIST 'EXPT (LIST '@G_G (LIST '~ 'F) (LIST '~ 'M) (LIST '~ 'N))
                    2)
              (LIST 'WHEN 0 (LIST 'OR (LIST 'EQUAL 'M 0) (LIST 'EQUAL 'M 3))))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DR (LIST '~ 'N)) (LIST 'D 1))
              (LIST 'DR (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'DR (LIST '~ 'N)))
              (LIST 'DR (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DR (LIST '~ 'N)) (LIST 'D 2))
              (LIST 'DR (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 2) (LIST 'DR (LIST '~ 'N)))
              (LIST 'DR (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DER (LIST '~ 'M)) (LIST 'DR (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DR 'N) (LIST 'DER 'M)))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'N)) (LIST 'DEL (LIST '~ 'M)))
              (LIST 'TIMES (LIST 'DEL 'M) (LIST 'DR 'N)))
        (LIST 'REPLACEBY (LIST 'EXPT (LIST 'DR (LIST '~ 'N)) 2)
              (LIST 'DR (LIST 'TIMES 2 'N)))
        (LIST 'REPLACEBY (LIST 'AXP 0) 1) (LIST 'REPLACEBY (LIST 'AXX 0) 1)
        (LIST 'REPLACEBY (LIST 'DR 0) 1) (LIST 'REPLACEBY (LIST 'DER 0) 1)
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DER (LIST '~ 'N)) (LIST 'D (LIST '~ 'M)))
              (LIST 'WHEN (LIST 'TIMES (LIST 'D 'M) (LIST 'DER 'N))
                    (LIST 'NEQ 'M 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'DEL (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DEL 'N) (LIST 'D 1)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DEL (LIST '~ 'N)) (LIST 'D 2))
              (LIST 'TIMES (LIST 'D 2) (LIST 'DEL 'N)))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'DEL (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DEL 'N) (LIST 'D (MINUS 1))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DEL (LIST '~ 'N)) (LIST 'D (MINUS 2)))
              (LIST 'TIMES (LIST 'D (MINUS 2)) (LIST 'DEL 'N)))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 3)) (LIST 'DEL (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DEL 'N) (LIST 'D (MINUS 3))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DEL (LIST '~ 'N)) (LIST 'D (MINUS 4)))
              (LIST 'TIMES (LIST 'D (MINUS 4)) (LIST 'DEL 'N)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'D (MINUS 1))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'D 1)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'D (MINUS 2))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 2)) (LIST 'D 1)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'D (MINUS 3))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 3)) (LIST 'D 1)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'D (MINUS 4))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 4)) (LIST 'D 1)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 3) (LIST 'D (MINUS 1))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'D 3)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 3) (LIST 'D (MINUS 2))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 2)) (LIST 'D 3)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 3) (LIST 'D (MINUS 4))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 4)) (LIST 'D 3)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 2) (LIST 'D (MINUS 1))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'D 2)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 2) (LIST 'D (MINUS 2))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 2)) (LIST 'D 2)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 2) (LIST 'D (MINUS 3))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 3)) (LIST 'D 2)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 2) (LIST 'D (MINUS 4))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 4)) (LIST 'D 2)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 3) (LIST 'D (MINUS 3))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 1) (LIST 'D 3))
              (LIST 'TIMES (LIST 'D 3) (LIST 'D 1)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D (MINUS 3)) (LIST 'D 3)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D 1))
              (LIST 'TIMES (LIST 'D 1) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D 2))
              (LIST 'TIMES (LIST 'D 2) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'DER (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DER 'N) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'DEL (LIST '~ 'N)))
              (LIST 'TIMES (LIST 'DEL 'N) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D (MINUS 1)))
              (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D (MINUS 2)))
              (LIST 'TIMES (LIST 'D (MINUS 2)) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'EXPT '@X_Y 2) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D (MINUS 3)))
              (LIST 'TIMES (LIST 'D (MINUS 3)) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'D 'T) (LIST 'D (MINUS 4)))
              (LIST 'TIMES (LIST 'D (MINUS 4)) (LIST 'D 'T)))
        (LIST 'REPLACEBY (LIST 'ABS '|#LL|) 1)
        (LIST 'REPLACEBY (LIST 'DELTA (LIST '~ 'F) (LIST '~ 'G))
              (LIST 'COND
                    (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''F) (LIST 'AEVAL ''G))
                          1)
                    (LIST 'T 0)))
        (LIST 'REPLACEBY (LIST 'BF_PART (LIST '~ 'WX) (LIST '~ 'N))
              (LIST 'PART (LIST 'FPART 'WX) (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY (LIST 'B_PART (LIST '~ 'WX) (LIST '~ 'N))
              (LIST 'PART (LIST 'BPART 'WX) (LIST 'PLUS 'N 1)))
        (LIST 'REPLACEBY (LIST 'PG (LIST '~ 'N) (LIST '~ 'X))
              (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                    (LIST 'TIMES (LIST 'EXPT (LIST 'D 1) 'N) 'X)))
        (LIST 'REPLACEBY (LIST 'CHAN (LIST '~ 'X))
              (LIST 'SUB (LIST 'EQUAL (LIST 'D 2) (LIST 'D 1))
                    (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) (LIST 'D 2)) 'X)))
        (LIST 'REPLACEBY (LIST 'S_PART (LIST '~ 'X) (LIST '~ 'N))
              (LIST 'COEFFN
                    (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) '@K)
                          (LIST 'EQUAL (LIST 'DER 2) (LIST 'EXPT '@K 2))
                          (LIST 'EQUAL (LIST 'DER 3) (LIST 'EXPT '@K 3))
                          (LIST 'EQUAL (LIST 'DEL 1) '@K)
                          (LIST 'EQUAL (LIST 'DEL 2) (LIST 'EXPT '@K 2))
                          (LIST 'EQUAL (LIST 'DER 3) (LIST 'EXPT '@K 3)) 'X)
                    '@K 'N))
        (LIST 'REPLACEBY (LIST 'SU_NEWTON (LIST '~ 'N) (LIST '~ 'M))
              (LIST 'QUOTIENT (LIST 'FACTORIAL 'N)
                    (LIST 'TIMES (LIST 'FACTORIAL 'M)
                          (LIST 'FACTORIAL (LIST 'DIFFERENCE 'N 'M)))))
        (LIST 'REPLACEBY (LIST 'PRYKR (LIST '~ 'F) (LIST '~ 'G))
              (LIST 'COND
                    (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''G) 1)
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''DER 1) ''F)))
                    (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''G) 2)
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''DER 2)
                                      ''F)))))
        (LIST 'REPLACEBY (LIST 'PRYKL (LIST '~ 'F) (LIST '~ 'G))
              (LIST 'COND
                    (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''G) 1)
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES ''F (LIST 'LIST ''DEL 1))))
                    (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''G) 2)
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES ''F
                                      (LIST 'LIST ''DEL 2))))))))) 
(LET
 '((LIST
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M)) (STP (~ X)))
     (TIMES (STP X) (BOS F N M)))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M) (~ L)) (STP (~ X)))
     (TIMES (STP X) (BOS F N M L)))
    (REPLACEBY (TIMES (AXP (~ F)) (STP (~ X))) (TIMES (STP X) (AXP F)))
    (REPLACEBY (TIMES (D (~ N)) (STP (~ X))) (MINUS (TIMES (STP X) (D N))))
    (REPLACEBY (TIMES (DER (~ K)) (STP 1))
     (WHEN (TIMES (STP 2) (DER K)) (NEQ K 3)))
    (REPLACEBY (TIMES (DER (~ K)) (STP 2))
     (WHEN (MINUS (TIMES (STP 1) (DER K))) (NEQ K 3)))
    (REPLACEBY (TIMES (DEL (~ K)) (STP 1)) (WHEN (MINUS (DEL K)) (NEQ K 3)))
    (REPLACEBY (TIMES (DEL (~ K)) (STP 2))
     (WHEN (MINUS (TIMES (STP 1) (DEL K))) (NEQ K 3)))
    (REPLACEBY (TIMES (DER 3) (STP (~ X))) (TIMES (STP X) (DER 3)))
    (REPLACEBY (TIMES (DEL 3) (STP (~ X))) (TIMES (STP X) (DER 3)))
    (REPLACEBY (TIMES (DEL (~ X)) (STP 10))
     (WHEN (TIMES (STP 20) (DEL X)) (NEQ X 3)))
    (REPLACEBY (TIMES (DEL (~ X)) (STP 20))
     (WHEN (MINUS (TIMES (STP 10) (DEL X))) (NEQ X 3)))
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (STP 1))
     (TIMES (STP 10) (FER F N M)))
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (STP 10))
     (MINUS (TIMES (STP 20) (FER F N M))))
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (STP 20))
     (TIMES (STP 10) (FER F N M)))
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (STP 2))
     (TIMES (STP 10) (FER F N M)))))) 
(SETK 'TRYK
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F))
                         (LIST 'FER (LIST '~ 'G) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DELTA 'F 'G) (LIST 'ZEN 'G 'N 'M))
                         (LIST 'TIMES (LIST 'FER 'G 'N 'M) (LIST 'D 'F))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F))
                         (LIST 'BOS (LIST '~ 'G) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'DELTA 'F 'G) (LIST 'ZAN 'G 'N 'M))
                         (LIST 'TIMES (LIST 'BOS 'G 'N 'M) (LIST 'D 'F))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F))
                         (LIST 'BOS (LIST '~ 'G) (LIST '~ 'N) (LIST '~ 'M)
                               (LIST '~ 'L)))
                   (LIST 'PLUS
                         (LIST 'TIMES 'L (LIST 'DELTA 'F 'G)
                               (LIST 'ZAN 'G 'N 'M)
                               (LIST 'BOS 'G 'N 'M (LIST 'DIFFERENCE 'L 1)))
                         (LIST 'TIMES (LIST 'BOS 'G 'N 'M 'L) (LIST 'D 'F))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F))
                         (LIST 'AXP (LIST '~ 'G)))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'AXP 'G)
                               (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'D 'F) 'G)
                                     (LIST 'TIMES 'G (LIST 'D 'F))))
                         (LIST 'TIMES (LIST 'AXP 'G) (LIST 'D 'F))))))) 
(SETK 'TRYK1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 0 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''M)
                                           (LIST 'LIST ''DER 1)
                                           (LIST 'LIST ''DER 2)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 (LIST 'LIST ''PLUS ''M 1))
                                           (LIST 'LIST ''DER 2)
                                           (LIST 'LIST ''DER 1)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''M)
                                           (LIST 'LIST ''DER 3)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'M) (LIST 'DER 1)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'M) (LIST 'DER 2)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'PLUS 'M 1))
                         (LIST 'DER 1) (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'PLUS 'M 1))
                         (LIST 'DER 2) (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 0 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''M)
                                           (LIST 'LIST ''DER 1)
                                           (LIST 'LIST ''DER 2)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 (LIST 'LIST ''PLUS ''M 1))
                                           (LIST 'LIST ''DER 2)
                                           (LIST 'LIST ''DER 1)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''M)
                                           (LIST 'LIST ''DER 3)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''M))))))))) 
(SETK 'TRYK2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 0 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'BOS 'F 0 0) (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'BOS 'F 0 0)
                         (LIST 'COND
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      2)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''M)
                                            (LIST 'LIST ''DER 1)
                                            (LIST 'LIST ''DER 2)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1) ''M))))
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      1)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  (LIST 'LIST ''PLUS ''M 1))
                                            (LIST 'LIST ''DER 2)
                                            (LIST 'LIST ''DER 1)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1) ''M))))
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      3)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''M)
                                            (LIST 'LIST ''DER 3)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1) ''M)))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'FER 'F 0 0) (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'DER 1) (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'FER 'F 0 0) (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'DER 2) (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 1 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'BOS 'F 0 0)
                         (LIST 'EXPT (MINUS 1) (LIST 'PLUS 'M 1)) (LIST 'DER 1)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 2 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'BOS 'F 0 0)
                         (LIST 'EXPT (MINUS 1) (LIST 'PLUS 'M 1)) (LIST 'DER 2)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 0 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'FER 'F 0 0) (LIST 'EXPT (MINUS 1) 'M)
                         (LIST 'EXPT (LIST 'D 1) 'M)))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 3 (LIST '~ 'M))
                   (LIST 'TIMES (LIST 'FER 'F 0 0)
                         (LIST 'COND
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      2)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''M)
                                            (LIST 'LIST ''DER 1)
                                            (LIST 'LIST ''DER 2)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1) ''M))))
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      1)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  (LIST 'LIST ''PLUS ''M 1))
                                            (LIST 'LIST ''DER 2)
                                            (LIST 'LIST ''DER 1)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1) ''M))))
                               (LIST
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      3)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''M)
                                            (LIST 'LIST ''DER 3)
                                            (LIST 'LIST ''EXPT
                                                  (LIST 'LIST ''D 1)
                                                  ''M)))))))))) 
(SETK 'TRYK3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                         (LIST '~ 'L))
                   1)
             (LIST 'REPLACEBY (LIST 'AXP (LIST '~ 'F)) 1)))) 
(SETK 'TRYK4
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)) 1)
             (LIST 'REPLACEBY (LIST 'AXP (LIST '~ 'F)) 1)
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                         (LIST '~ 'L))
                   1)
             (LIST 'REPLACEBY (LIST 'DER (LIST '~ 'N)) 1)
             (LIST 'REPLACEBY (LIST 'D (LIST '~ 'N)) 1)
             (LIST 'REPLACEBY (LIST 'DEL (LIST '~ 'N)) 1)))) 
(SETK 'TRYK5
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'M) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 0)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N)))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             ''N)))
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         1))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 1) ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       (LIST 'LIST ''PLUS ''N
                                                             1)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       (LIST 'LIST ''PLUS ''N
                                                             1))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       (LIST 'LIST ''PLUS ''N
                                                             1)))
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             ''N)
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         2))))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         1))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'REDERR
                                           (LIST 'REVALX
                                                 " wrong values of arguments"))))))
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'M) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 0)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''GRAS
                                                 (LIST 'LIST ''MKID ''F
                                                       (LIST 'LIST ''MKID ''F
                                                             1))
                                                 ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N)))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       ''N)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             ''N)))
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 0)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''GRAS
                                                 (LIST 'LIST ''MKID ''F
                                                       (LIST 'LIST ''MKID ''F
                                                             2))
                                                 ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       (LIST 'LIST ''PLUS ''N
                                                             1)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       (LIST 'LIST ''PLUS ''N
                                                             1))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       (LIST 'LIST ''PLUS ''N
                                                             1)))
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             ''N)
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         2))))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 0)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)))))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'REDERR
                                           (LIST 'REVALX
                                                 "wrong values of arguments"))))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'M) (LIST '~ 'N)
                         (LIST '~ 'L))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 0)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N
                                                 ''L)
                                           (LIST 'LIST ''TIMES ''L
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''L 1))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 1)
                                                                   ''N))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''G 2)
                                                                   ''N))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''PLUS
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         ''N 1)
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''L
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           0)
                                                                          ''N
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           1)
                                                                          ''N)
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           2)
                                                                          ''N)))))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N
                                                 ''L)
                                           (LIST 'LIST ''TIMES ''L
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''L 1))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''G
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''G
                                                                          2)
                                                                         ''N)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''L
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           0)
                                                                          ''N
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           2)
                                                                          ''N)))))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 1) ''N
                                                 ''L)
                                           (LIST 'LIST ''TIMES ''L
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''L 1))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''G 2)
                                                                   ''N))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''G 1)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''L
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1)
                                                                          ''N
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           2)
                                                                          ''N)))))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''M) 3)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 1) ''N
                                                 ''L)
                                           (LIST 'LIST ''TIMES ''L
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''L 1))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''G
                                                                          2)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''G
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''PLUS
                                                                   (LIST 'LIST
                                                                         ''MINUS
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           0)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1)
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''L
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1)
                                                                          ''N
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''G
                                                                           2)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))))))))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'REDERR
                                           (LIST 'REVALX
                                                 "wrong values of arguments"))))))
             (LIST 'REPLACEBY (LIST 'AXP (LIST '~ 'F))
                   (LIST 'TIMES (LIST 'AXX (LIST 'BF_PART 'F 0))
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'TET 1) (LIST 'BF_PART 'F 1))
                               (LIST 'TIMES (LIST 'TET 2) (LIST 'BF_PART 'F 2))
                               (LIST 'TIMES (LIST 'TET 2) (LIST 'TET 1)
                                     (LIST 'PLUS (LIST 'BF_PART 'F 3)
                                           (LIST 'TIMES 2 (LIST 'BF_PART 'F 1)
                                                 (LIST 'BF_PART 'F 2)))))))))) 
(SETK 'TRYK6
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST 'GRAS (LIST '~ 'F) (LIST '~ 'N)) 0)))) 
(SETK 'TRYK7
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST '@F_F (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'BOS 'F 0 'N))
             (LIST 'REPLACEBY (LIST '@F_F (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''BOS ''F 1 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''BOS ''F 1 ''N)))))
             (LIST 'REPLACEBY (LIST '@F_F (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''BOS ''F 2 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''BOS ''F 2 ''N)))))
             (LIST 'REPLACEBY (LIST '@F_F (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''BOS ''F 3 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''BOS ''F 0
                                            (LIST 'LIST ''PLUS ''N 1)))))
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      1)
                                (LIST 'NOT
                                      (LIST 'FREEOF
                                            (LIST 'REVALX ''B_ANTYCHIRAL)
                                            (LIST 'REVALX ''F))))
                          0)
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      3)
                                (LIST 'NOT
                                      (LIST 'FREEOF
                                            (LIST 'REVALX ''B_ANTYCHIRAL)
                                            (LIST 'REVALX ''F))))
                          (LIST 'AEVAL
                                (LIST 'LIST ''BOS ''F 0
                                      (LIST 'LIST ''PLUS ''N 1))))
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''BOS ''F 3 ''N)))))
             (LIST 'REPLACEBY (LIST '@G_G (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'FER 'F 0 'N))
             (LIST 'REPLACEBY (LIST '@G_G (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 1 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''FER ''F 1 ''N)))))
             (LIST 'REPLACEBY (LIST '@G_G (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 2 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''FER ''F 2 ''N)))))
             (LIST 'REPLACEBY (LIST '@G_G (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 3 ''N)))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''FER ''F 0
                                            (LIST 'LIST ''PLUS ''N 1)))))
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      1)
                                (LIST 'NOT
                                      (LIST 'FREEOF
                                            (LIST 'REVALX ''F_ANTYCHIRAL)
                                            (LIST 'REVALX ''F))))
                          0)
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA)
                                      3)
                                (LIST 'NOT
                                      (LIST 'FREEOF
                                            (LIST 'REVALX ''F_ANTYCHIRAL)
                                            (LIST 'REVALX ''F))))
                          (LIST 'AEVAL
                                (LIST 'LIST ''FER ''F 0
                                      (LIST 'LIST ''PLUS ''N 1))))
                         (LIST 'T
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 3 ''N)))))))) 
(SETK 'TRYK8
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'PLUS (LIST 'BERZ 'F 'N 'M)
                         (LIST 'TIMES 'EPS (LIST 'BER 'F 'N 'M))))
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'PLUS (LIST 'FIRR 'F 'N 'M)
                         (LIST 'TIMES 'EPS (LIST 'FIR 'F 'N 'M))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                         (LIST '~ 'L))
                   (LIST 'PLUS (LIST 'BERZ 'F 'N 'M 'L)
                         (LIST 'TIMES 'L 'EPS
                               (LIST 'BERZ 'F 'N 'M (LIST 'DIFFERENCE 'L 1))
                               (LIST 'BER 'F 'N 'M))))))) 
(SETK 'TRYK9
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BERZ (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'BOS 'F 'N 'M))
             (LIST 'REPLACEBY
                   (LIST 'FIRR (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'FER 'F 'N 'M))
             (LIST 'REPLACEBY
                   (LIST 'BERZ (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                         (LIST '~ 'L))
                   (LIST 'BOS 'F 'N 'M 'L))))) 
(SETK 'TRYK10
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'FIR (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'PG 'M (LIST 'PR 'N (LIST 'BOS 'F))))
             (LIST 'REPLACEBY
                   (LIST 'BER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'PG 'M (LIST 'PR 'N (LIST 'BOS 'F))))))) 
(SETK 'TRYK11
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST '|#A| (LIST '~ 'N)) (LIST '|#AA| 'N))
             (LIST 'REPLACEBY (LIST '|#B| (LIST '~ 'N)) (LIST '|#BB| 'N))
             (LIST 'REPLACEBY (LIST '|#C| (LIST '~ 'N)) (LIST '|#CC| 'N))))) 
(SETK 'TRYK12
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST '|#AA| (LIST '~ 'N)) (LIST '|#B| 'N))
             (LIST 'REPLACEBY (LIST '|#BB| (LIST '~ 'N)) (LIST '|#C| 'N))
             (LIST 'REPLACEBY (LIST '|#CC| (LIST '~ 'N)) (LIST '|#A| 'N))))) 
(SETK 'TRYK13
      (AEVAL
       (LIST 'LIST (LIST 'REPLACEBY (LIST '|#AA| (LIST '~ 'N)) (LIST '|#C| 'N))
             (LIST 'REPLACEBY (LIST '|#BB| (LIST '~ 'N)) (LIST '|#A| 'N))
             (LIST 'REPLACEBY (LIST '|#CC| (LIST '~ 'N)) (LIST '|#B| 'N))))) 
(SETK 'TRYK14
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M) 'T 'T)
                   (LIST 'PG 'M (LIST 'PR 'N (LIST 'BOS 'F 'T))))
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M) 'T)
                   (LIST 'PG 'M (LIST 'PR 'N (LIST 'BOS 'F 'T))))))) 
(SETK 'TRYK15
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                         (LIST '~ 'L))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 0)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 3))
                          (LIST 'AEVAL (LIST 'LIST ''BERZ ''F ''N ''M ''L)))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''L)
                                           (LIST 'LIST ''BERZ ''F 2 ''M ''L))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''BERZ ''F 1 ''M ''L)))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 0)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 3))
                          (LIST 'AEVAL (LIST 'LIST ''BERZ ''F ''N ''M)))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''BERZ ''F 2 ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 2)
                               (LIST 'AEVAL (LIST 'LIST ''BERZ ''F 1 ''M)))))
             (LIST 'REPLACEBY
                   (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 0)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 3))
                          (LIST 'AEVAL (LIST 'LIST ''FIRR ''F ''N ''M)))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''FIRR ''F 2 ''M))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''N) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FIRR ''F 1 ''M)))))))) 
(SETK 'TRYK16
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N)
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    2))
                                                        ''N))
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''TET 1)
                                                  (LIST 'LIST ''QUOTIENT
                                                        (LIST 'LIST ''FUN
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    0)
                                                              (LIST 'LIST
                                                                    ''PLUS ''N
                                                                    1))
                                                        2))))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N)
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''GRAS
                                                  (LIST 'LIST ''MKID ''F
                                                        (LIST 'LIST ''MKID ''F
                                                              1))
                                                  ''N))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              0)
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N)))))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''DIFFERENCE
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0)
                                                  ''N)
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              0)
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''DIFFERENCE
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 0)
                                                             ''N)
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          2))
                                                                   ''N)))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1))
                                                             2)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''QUOTIENT
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 0)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1))
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''DIFFERENCE
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0)
                                                  ''N)
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              0)
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 1) ''N)
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             ''N))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         1))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1))))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''QUOTIENT
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1))
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 1)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''MINUS
                                                  (LIST 'LIST ''BOS ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      0)
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1)
                                                                          ''N)
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          (LIST
                                                                           'LIST
                                                                           ''FUN
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            0)
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            ''N
                                                                            1))
                                                                          2))
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''TET
                                                                          2)
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            2))
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))
                                                                         2)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          2))
                                                                   4)))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 3)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''MINUS
                                                  (LIST 'LIST ''BOS ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''BOS ''F 0
                                                  (LIST 'LIST ''PLUS ''N 1))))
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         ''N))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           2))
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          2))
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N ''K)
                                      (LIST 'LIST ''TIMES ''K
                                            (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0) ''N
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1))
                                            (LIST 'LIST ''DIFFERENCE
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    2))
                                                        ''N)
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''TET 1)
                                                        (LIST 'LIST ''QUOTIENT
                                                              (LIST 'LIST ''FUN
                                                                    (LIST 'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                    1)
                                                              2)))))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N ''K)
                                      (LIST 'LIST ''TIMES ''K
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0) ''N
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1))
                                            (LIST 'LIST ''DIFFERENCE
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        ''N)
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''TET 2)
                                                        (LIST 'LIST ''QUOTIENT
                                                              (LIST 'LIST ''FUN
                                                                    (LIST 'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                    1)
                                                              2)))))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N
                                                 ''K)
                                           (LIST 'LIST ''TIMES ''K
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1))
                                                       ''N)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''K 1)))
                                           (LIST 'LIST ''TIMES ''K
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''K 1)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''TIMES ''K
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 1)
                                                                   ''N 1)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   ''N
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         ''K
                                                                         1)))
                                                       (LIST 'LIST ''TIMES ''K
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   ''K 1)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   ''N)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          2))
                                                                   ''N)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   ''N
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         ''K
                                                                         2))))))))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''DIFFERENCE
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N ''K)
                                      (LIST 'LIST ''TIMES ''K
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0) ''N
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1))
                                            (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''TET 1)
                                                        (LIST 'LIST ''QUOTIENT
                                                              (LIST 'LIST ''FUN
                                                                    (LIST 'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                    1)
                                                              2)))))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''DIFFERENCE
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 0) ''N
                                                 ''K)
                                           (LIST 'LIST ''TIMES ''K
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''K 1))
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             ''N)
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1))
                                                             2)
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                         1)
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''DIFFERENCE
                                      (LIST 'LIST ''FUN
                                            (LIST 'LIST ''MKID ''F 0) ''N ''K)
                                      (LIST 'LIST ''TIMES ''K
                                            (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0) ''N
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1))
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''TET 1)
                                                        (LIST 'LIST ''QUOTIENT
                                                              (LIST 'LIST ''FUN
                                                                    (LIST 'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          0)
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))
                                                              2)))))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''FUN
                                                 (LIST 'LIST ''MKID ''F 1) ''N
                                                 ''K)
                                           (LIST 'LIST ''TIMES ''K
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N
                                                       (LIST 'LIST ''DIFFERENCE
                                                             ''K 1))
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          2))
                                                                   ''N)
                                                             (LIST 'LIST ''GRAS
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)
                                                                         1)
                                                                   2)))))))))
             (LIST 'REPLACEBY
                   (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'N) (LIST '~ 'K))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 1)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''EXPT
                                                        (LIST 'MINUS 1) ''K)
                                                  (LIST 'LIST ''BOS ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)
                                                        ''K))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      0)
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             ''N
                                                             (LIST 'LIST ''MKID
                                                                   ''F 0)
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1)
                                                             ''K)
                                                       (LIST 'LIST ''TIMES ''K
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 1)
                                                                   ''N
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N 1)
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         ''K
                                                                         1))
                                                             (LIST 'LIST ''PLUS
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''TET
                                                                          2)
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            2))
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1)))
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         (LIST
                                                                          'LIST
                                                                          ''TIMES
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           2)
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUOTIENT
                                                                           (LIST
                                                                            'LIST
                                                                            ''FUN
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             1)
                                                                            (LIST
                                                                             'LIST
                                                                             ''PLUS
                                                                             ''N
                                                                             1)
                                                                            1)
                                                                           2))
                                                                         (LIST
                                                                          'LIST
                                                                          ''TIMES
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           2)
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUOTIENT
                                                                           (LIST
                                                                            'LIST
                                                                            ''FUN
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             0)
                                                                            (LIST
                                                                             'LIST
                                                                             ''PLUS
                                                                             ''N
                                                                             2)
                                                                            1)
                                                                           4))))))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 3)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''EXPT
                                                        (LIST 'MINUS 1) ''K)
                                                  (LIST 'LIST ''BOS ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)
                                                        ''K))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''B_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''BOS ''F 0
                                                  (LIST 'LIST ''PLUS ''N 1)
                                                  ''K)))
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''EXPT
                                                                   2 ''K)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 1)
                                                                   ''N ''K))
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''TIMES ''K
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         2
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''K
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         ''N
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''K
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           (LIST
                                                                            'LIST
                                                                            ''TET
                                                                            2)
                                                                           (LIST
                                                                            'LIST
                                                                            ''GRAS
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             (LIST
                                                                              'LIST
                                                                              ''MKID
                                                                              ''F
                                                                              2))
                                                                            (LIST
                                                                             'LIST
                                                                             ''PLUS
                                                                             ''N
                                                                             1))))
                                                                         (LIST
                                                                          'LIST
                                                                          ''TIMES
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''GRAS
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             1))
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            ''N
                                                                            1)))
                                                                         (LIST
                                                                          'LIST
                                                                          ''TIMES
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           2)
                                                                          (LIST
                                                                           'LIST
                                                                           ''TET
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUOTIENT
                                                                           (LIST
                                                                            'LIST
                                                                            ''FUN
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             0)
                                                                            (LIST
                                                                             'LIST
                                                                             ''PLUS
                                                                             ''N
                                                                             2)
                                                                            1)
                                                                           2))))
                                                             (LIST 'LIST
                                                                   ''TIMES ''K
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         ''K 1)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         2
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''K
                                                                          2))
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         ''N
                                                                         (LIST
                                                                          'LIST
                                                                          ''DIFFERENCE
                                                                          ''K
                                                                          2))
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           2))
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1)))))))))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''GRAS
                                            (LIST 'LIST ''MKID ''F
                                                  (LIST 'LIST ''MKID ''F 1))
                                            ''N)
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              1)
                                                        ''N))
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''TET 1)
                                                  (LIST 'LIST ''QUOTIENT
                                                        (LIST 'LIST ''GRAS
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    (LIST 'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1))
                                                              (LIST 'LIST
                                                                    ''PLUS ''N
                                                                    1))
                                                        2))))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''F_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''GRAS
                                            (LIST 'LIST ''MKID ''F
                                                  (LIST 'LIST ''MKID ''F 1))
                                            ''N)
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''FUN
                                                  (LIST 'LIST ''MKID ''F 0)
                                                  ''N))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''GRAS
                                                 (LIST 'LIST ''MKID ''F
                                                       (LIST 'LIST ''MKID ''F
                                                             1))
                                                 ''N)
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             0)
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''FUN
                                                       (LIST 'LIST ''MKID ''F
                                                             1)
                                                       ''N))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''GRAS
                                                       (LIST 'LIST ''MKID ''F
                                                             (LIST 'LIST ''MKID
                                                                   ''F 2))
                                                       ''N)))))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''DIFFERENCE
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''GRAS
                                                  (LIST 'LIST ''MKID ''F
                                                        (LIST 'LIST ''MKID ''F
                                                              1))
                                                  ''N)
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 2)
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              0)
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    1))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''DIFFERENCE
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         1))
                                                             ''N)
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 1)
                                                                   ''N)))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 2)
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1))
                                                             2)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''QUOTIENT
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         1))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1))
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_CHIRAL)
                                      (LIST 'REVALX ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''PLUS
                                      (LIST 'LIST ''DIFFERENCE
                                            (LIST 'LIST ''GRAS
                                                  (LIST 'LIST ''MKID ''F
                                                        (LIST 'LIST ''MKID ''F
                                                              2))
                                                  ''N)
                                            (LIST 'LIST ''TIMES
                                                  (LIST 'LIST ''TET 1)
                                                  (LIST 'LIST ''FUN
                                                        (LIST 'LIST ''MKID ''F
                                                              0)
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))))
                                      (LIST 'LIST ''TIMES (LIST 'LIST ''TET 2)
                                            (LIST 'LIST ''TET 1)
                                            (LIST 'LIST ''QUOTIENT
                                                  (LIST 'LIST ''GRAS
                                                        (LIST 'LIST ''MKID ''F
                                                              (LIST 'LIST
                                                                    ''MKID ''F
                                                                    2))
                                                        (LIST 'LIST ''PLUS ''N
                                                              1))
                                                  2)))))
                         (LIST
                          (LIST 'NOT
                                (LIST 'FREEOF (LIST 'REVALX ''B_ANTYCHIRAL)
                                      (LIST 'REVALX ''F)))
                          0)
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''PLUS
                                           (LIST 'LIST ''GRAS
                                                 (LIST 'LIST ''MKID ''F
                                                       (LIST 'LIST ''MKID ''F
                                                             2))
                                                 ''N)
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''FUN
                                                             (LIST 'LIST ''MKID
                                                                   ''F 1)
                                                             ''N))
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''TET 1)
                                                       (LIST 'LIST ''QUOTIENT
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1))
                                                             2)))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''TET 2)
                                                 (LIST 'LIST ''TET 1)
                                                 (LIST 'LIST ''QUOTIENT
                                                       (LIST 'LIST ''GRAS
                                                             (LIST 'LIST ''MKID
                                                                   ''F
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F
                                                                         2))
                                                             (LIST 'LIST ''PLUS
                                                                   ''N 1))
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABRA) 1)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''F_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''MINUS
                                                  (LIST 'LIST ''FER ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''F_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      0)
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            2))
                                                                          ''N)
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          (LIST
                                                                           'LIST
                                                                           ''GRAS
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            (LIST
                                                                             'LIST
                                                                             ''MKID
                                                                             ''F
                                                                             1))
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            ''N
                                                                            1))
                                                                          2))
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''TET
                                                                          2)
                                                                         (LIST
                                                                          'LIST
                                                                          ''FUN
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1)
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           1))))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         1)
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         (LIST
                                                                          'LIST
                                                                          ''GRAS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           (LIST
                                                                            'LIST
                                                                            ''MKID
                                                                            ''F
                                                                            1))
                                                                          (LIST
                                                                           'LIST
                                                                           ''PLUS
                                                                           ''N
                                                                           2))
                                                                         4)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           2))
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))
                                                                   2)))))))
                         (LIST (LIST 'EVALEQUAL (LIST 'AEVAL ''ABRA_KADABAR) 3)
                               (LIST 'COND
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''F_CHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''MINUS
                                                  (LIST 'LIST ''FER ''F 0
                                                        (LIST 'LIST ''PLUS ''N
                                                              1)))))
                                     (LIST
                                      (LIST 'NOT
                                            (LIST 'FREEOF
                                                  (LIST 'REVALX ''F_ANTYCHIRAL)
                                                  (LIST 'REVALX ''F)))
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''FER ''F 0
                                                  (LIST 'LIST ''PLUS ''N 1))))
                                     (LIST 'T
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''TIMES 2
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKIF
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           2))
                                                                         ''N))
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''TET
                                                                         2)
                                                                   (LIST 'LIST
                                                                         ''FUN
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          1)
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          1))))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST ''FUN
                                                                   (LIST 'LIST
                                                                         ''MKID
                                                                         ''F 0)
                                                                   (LIST 'LIST
                                                                         ''PLUS
                                                                         ''N
                                                                         1)))
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''TET
                                                                   2)
                                                             (LIST 'LIST ''TET
                                                                   1)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''GRAS
                                                                         (LIST
                                                                          'LIST
                                                                          ''MKID
                                                                          ''F
                                                                          (LIST
                                                                           'LIST
                                                                           ''MKID
                                                                           ''F
                                                                           1))
                                                                         (LIST
                                                                          'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          2))
                                                                   2)))))))))
             (LIST 'REPLACEBY (LIST 'AXP (LIST '~ 'F))
                   (LIST 'TIMES (LIST 'AXX (LIST 'BF_PART 'F 0))
                         (LIST 'PLUS 1
                               (LIST 'TIMES (LIST 'TET 1) (LIST 'BF_PART 'F 1))
                               (LIST 'TIMES (LIST 'TET 2) (LIST 'BF_PART 'F 2))
                               (LIST 'TIMES (LIST 'TET 2) (LIST 'TET 1)
                                     (LIST 'PLUS (LIST 'BF_PART 'F 3)
                                           (LIST 'TIMES 2 (LIST 'BF_PART 'F 1)
                                                 (LIST 'BF_PART 'F 2)))))))))) 
(LET
 '((LIST
    (REPLACEBY (TIMES (D 1) (FER (~ F) (~ N) (~ M)))
     (PLUS (FER F N (PLUS M 1)) (TIMES (FER F N M) (D 1))))
    (REPLACEBY (TIMES (D 1) (BOS (~ F) (~ N) (~ M)))
     (PLUS (BOS F N (PLUS M 1)) (TIMES (BOS F N M) (D 1))))
    (REPLACEBY (TIMES (FER (~ F) (~ N) (~ M)) (D 2))
     (PLUS (MINUS (FER F N (PLUS M 1))) (TIMES (D 2) (FER F N M))))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M)) (D 2))
     (PLUS (MINUS (BOS F N (PLUS M 1))) (TIMES (D 2) (BOS F N M))))
    (REPLACEBY (TIMES (D 1) (BOS (~ F) (~ N) (~ M) (~ L)))
     (PLUS (TIMES L (BOS F N (PLUS M 1) 1) (BOS F N M (DIFFERENCE L 1)))
           (TIMES (BOS F N M L) (D 1))))
    (REPLACEBY (TIMES (BOS (~ F) (~ N) (~ M) (~ L)) (D 2))
     (PLUS
      (MINUS (TIMES L (BOS F N (PLUS M 1) 1) (BOS F N M (DIFFERENCE L 1))))
      (TIMES (D 2) (BOS F N M L))))
    (REPLACEBY (TIMES (DER (~ K)) (FER (~ F) 0 (~ M)))
     (WHEN (DIFFERENCE (BOS F K M) (TIMES (FER F 0 M) (DER K)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (DER (~ K)) (BOS (~ F) 0 (~ M)))
     (WHEN (PLUS (FER F K M) (TIMES (BOS F 0 M) (DER K)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (FER (~ F) 0 (~ M)) (DEL (~ K)))
     (WHEN (DIFFERENCE (BOS F K M) (TIMES (DEL K) (FER F 0 M)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (BOS (~ F) 0 (~ M)) (DEL (~ K)))
     (WHEN (PLUS (MINUS (FER F K M)) (TIMES (DEL K) (BOS F 0 M)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (DER (~ K)) (BOS (~ F) 0 (~ M) (~ L)))
     (WHEN
      (PLUS (TIMES L (FER F K M) (BOS F 0 M (DIFFERENCE L 1)))
            (TIMES (BOS F 0 M L) (DER K)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (BOS (~ F) 0 (~ M) (~ L)) (DEL (~ K)))
     (WHEN
      (PLUS (MINUS (TIMES L (FER F K M) (BOS F 0 M (DIFFERENCE L 1))))
            (TIMES (DEL K) (BOS F 0 M L)))
      (AND (NUMBERP K) (LESSP K 3))))
    (REPLACEBY (TIMES (D 1) (AXP (~ G)))
     (PLUS (TIMES (PG 1 G) (AXP G)) (TIMES (AXP G) (D 1))))
    (REPLACEBY (TIMES (DER 1) (AXP (~ G)))
     (PLUS (TIMES (PR 1 G) (AXP G)) (TIMES (AXP G) (DER 1))))
    (REPLACEBY (TIMES (DER 2) (AXP (~ G)))
     (PLUS (TIMES (PR 2 G) (AXP G)) (TIMES (AXP G) (DER 2))))
    (REPLACEBY (TIMES (AXP (~ G)) (D 2))
     (PLUS (MINUS (TIMES (PG 1 G) (AXP G))) (TIMES (D 2) (AXP G))))
    (REPLACEBY (TIMES (AXP (~ G)) (DEL 1))
     (PLUS (MINUS (TIMES (PR 1 G) (AXP G))) (TIMES (DEL 1) (AXP G))))
    (REPLACEBY (TIMES (AXP (~ G)) (DEL 2))
     (PLUS (MINUS (TIMES (PR 2 G) (AXP G))) (TIMES (DEL 2) (AXP G))))
    (REPLACEBY (TIMES (D 1) (FUN (~ F) (~ M)))
     (PLUS (FUN F (PLUS M 1)) (TIMES (FUN F M) (D 1))))
    (REPLACEBY (TIMES (FUN (~ F) (~ M)) (D 2))
     (PLUS (MINUS (FUN F (PLUS M 1))) (TIMES (D 2) (FUN F M))))
    (REPLACEBY (TIMES (D 1) (FUN (~ F) (~ N) (~ M)))
     (PLUS (TIMES M (FUN F (PLUS N 1) 1) (FUN F N (DIFFERENCE M 1)))
           (TIMES (FUN F N M) (D 1))))
    (REPLACEBY (TIMES (FUN (~ F) (~ N) (~ M)) (D 2))
     (PLUS (MINUS (TIMES M (FUN F (PLUS N 1) 1) (FUN F N (DIFFERENCE M 1))))
           (TIMES (D 2) (FUN F N M))))
    (REPLACEBY (TIMES (GRAS (~ F) (~ M)) (D 2))
     (PLUS (MINUS (GRAS F (PLUS M 1))) (TIMES (D 2) (GRAS F M))))
    (REPLACEBY (TIMES (D 1) (GRAS (~ F) (~ M)))
     (PLUS (GRAS F (PLUS M 1)) (TIMES (GRAS F M) (D 1))))
    (REPLACEBY (TIMES (D 1) (AXX (~ F)))
     (PLUS (TIMES (PG 1 F) (AXX F)) (TIMES (AXX F) (D 1))))
    (REPLACEBY (TIMES (AXX (~ F)) (D 2))
     (PLUS (MINUS (TIMES (PG 1 F) (AXX F))) (TIMES (D 2) (AXX F))))))) 
(LET
 (LIST
  (LIST 'LIST
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''FER
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'K))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''FER
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'K))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''BOS
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'K))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''BOS
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'K))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                          (LIST '~ 'L)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''BOS
                                                                          ''F
                                                                          ''N
                                                                          ''M
                                                                          ''L))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES
                    (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                          (LIST '~ 'L))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''BOS
                                                                          ''F
                                                                          ''N
                                                                          ''M
                                                                          ''L))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'AXP (LIST '~ 'F)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''AXP
                                                                          ''F))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'AXP (LIST '~ 'F)) (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''AXP
                                                                          ''F))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST ''BOS
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'S))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST ''FER
                                                                    ''F ''N
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''M
                                                                          'S))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)
                          (LIST '~ 'L)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'S
                                                                    (LIST 'LIST
                                                                          ''BOS
                                                                          ''F
                                                                          ''N
                                                                          ''M
                                                                          ''L))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST ''FUN
                                                                    ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'S))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'GRAS (LIST '~ 'F) (LIST '~ 'N)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST
                                                                    ''GRAS ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'S))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DR (LIST '~ 'X))
                    (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'L)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'S 'FORALL-RESULT)
                                (LIST 'SETQ 'S 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL* ''WW) 'S))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'S)
                                                              (LIST 'LIST
                                                                    ''SU_NEWTON
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          (LIST
                                                                           'DIFFERENCE
                                                                           'S
                                                                           1))
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          (LIST
                                                                           'LIST
                                                                           ''MINUS
                                                                           ''X)
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'S
                                                                    (LIST 'LIST
                                                                          ''FUN
                                                                          ''F
                                                                          ''N
                                                                          ''L))
                                                              (LIST 'LIST ''DR
                                                                    (LIST 'LIST
                                                                          ''DIFFERENCE
                                                                          ''X
                                                                          'S))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'S
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'S))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''FUN
                                                                          ''F
                                                                          ''N
                                                                          ''M))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''FUN
                                                                          ''F
                                                                          ''N
                                                                          ''M))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''FUN
                                                                    ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'K))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'FUN (LIST '~ 'F) (LIST '~ 'N))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''FUN
                                                                    ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'K))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1))
                    (LIST 'GRAS (LIST '~ 'F) (LIST '~ 'N)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST
                                                                    ''GRAS ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'K))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'GRAS (LIST '~ 'F) (LIST '~ 'N))
                    (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST
                                                                    ''GRAS ''F
                                                                    (LIST 'LIST
                                                                          ''PLUS
                                                                          ''N
                                                                          'K))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'D (MINUS 1)) (LIST 'AXX (LIST '~ 'F)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'K)
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''AXX
                                                                          ''F))
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           1))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'AXX (LIST '~ 'F)) (LIST 'D (MINUS 2)))
              (LIST 'COND
                    (LIST (LIST 'EVALNUMBERP (LIST 'AEVAL ''WW))
                          (LIST 'PROG (LIST 'K 'FORALL-RESULT)
                                (LIST 'SETQ 'K 0) (LIST 'SETQ 'FORALL-RESULT 0)
                                'LAB1
                                (LIST 'COND
                                      (LIST
                                       (LIST '|AMINUSP:|
                                             (LIST 'LIST ''DIFFERENCE
                                                   (LIST 'AEVAL*
                                                         (LIST 'LIST
                                                               ''DIFFERENCE
                                                               ''WW 1))
                                                   'K))
                                       (LIST 'RETURN 'FORALL-RESULT)))
                                (LIST 'SETQ 'FORALL-RESULT
                                      (LIST 'AEVAL*
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'AEVAL*
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST 'LIST
                                                                          ''D
                                                                          (LIST
                                                                           'MINUS
                                                                           2))
                                                                    (LIST 'PLUS
                                                                          'K
                                                                          1))
                                                              (LIST 'LIST ''PG
                                                                    'K
                                                                    (LIST 'LIST
                                                                          ''AXX
                                                                          ''F))))
                                                  'FORALL-RESULT)))
                                (LIST 'SETQ 'K
                                      (LIST
                                       (LIST 'LAMBDA (LIST 'FORALL-RESULT)
                                             (LIST 'AEVAL*
                                                   (LIST 'LIST ''PLUS
                                                         'FORALL-RESULT 1)))
                                       'K))
                                (LIST 'GO 'LAB1)))
                    (LIST 'T
                          (LIST 'AEVAL
                                (LIST 'REDERR
                                      (LIST 'REVALX
                                            "introduce the precision e.g. give the value of ww > 0"))))))))) 
(LET
 '((LIST (REPLACEBY (TIMES (D T) (AXP (~ F))) (TIMES (AXP F) (D T) F))
         (REPLACEBY (TIMES (D T) (BOS (~ F) (~ N) (~ M)))
          (PLUS (BOS F N M T T) (TIMES (BOS F N M) (D T))))
         (REPLACEBY (TIMES (D T) (FER (~ F) (~ N) (~ M)))
          (PLUS (FER F N M T) (TIMES (FER F N M) (D T))))
         (REPLACEBY (TIMES (D T) (BOS (~ F) (~ N) (~ M) (~ L)))
          (PLUS (TIMES L (BOS F N M (DIFFERENCE L 1)) (BOS F N M T T))
                (TIMES (BOS F N M L) (D T))))))) 
(PUT 'RZUT 'NUMBER-OF-ARGS 2) 
(FLAG '(RZUT) 'OPFN) 
(PUT 'RZUT 'DEFINED-ON-LINE '824) 
(PUT 'RZUT 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'RZUT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RZUT (RR N)
    (PROG (OLA EWA)
      (SETQ OLA (AEVAL (LIST 'CHAN RR)))
      (SETQ EWA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D (MINUS 1)) 0)
                     (LIST 'EQUAL (LIST 'D (MINUS 2)) 0)
                     (LIST 'EQUAL (LIST 'D (MINUS 3)) 0)
                     (LIST 'EQUAL (LIST 'D (MINUS 4)) 0) OLA)))
      (COND ((EVALEQUAL (AEVAL N) 0) (RETURN (AEVAL EWA))))
      (COND
       ((EVALEQUAL (AEVAL N) 1)
        (SETQ EWA
                (AEVAL
                 (LIST 'DIFFERENCE EWA
                       (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) 0)
                             (LIST 'EQUAL (LIST 'DER 2) 0)
                             (LIST 'EQUAL (LIST 'DER 3) 0)
                             (LIST 'EQUAL (LIST 'D 1) 0) EWA))))))
      (COND
       ((EVALEQUAL (AEVAL N) 2)
        (SETQ EWA
                (AEVAL
                 (LIST 'DIFFERENCE
                       (LIST 'DIFFERENCE
                             (LIST 'DIFFERENCE
                                   (LIST 'DIFFERENCE EWA
                                         (LIST 'SD_PART EWA 0 0))
                                   (LIST 'TIMES (LIST 'SD_PART EWA 1 0)
                                         (LIST 'DER 1)))
                             (LIST 'TIMES (LIST 'SD_PART EWA 2 0)
                                   (LIST 'DER 2)))
                       (LIST 'TIMES (LIST 'SD_PART EWA 0 1) (LIST 'D 1)))))))
      (RETURN (AEVAL EWA)))) 
(PUT 'SD_PART 'NUMBER-OF-ARGS 3) 
(FLAG '(SD_PART) 'OPFN) 
(PUT 'SD_PART 'DEFINED-ON-LINE '832) 
(PUT 'SD_PART 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'SD_PART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SD_PART (WR N M)
    (PROG (EWA OLA)
      (SETQ EWA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) '@KK)
                     (LIST 'EQUAL (LIST 'D 2) '@KK)
                     (LIST 'EQUAL (LIST 'D (MINUS 2)) '@SS)
                     (LIST 'EQUAL (LIST 'D (MINUS 1)) '@SS)
                     (LIST 'EQUAL (LIST 'D (MINUS 3))
                           (LIST 'TIMES '@SS (LIST 'D (MINUS 33))))
                     (LIST 'EQUAL (LIST 'D (MINUS 4))
                           (LIST 'TIMES '@SS (LIST 'D (MINUS 44))))
                     WR)))
      (SETQ OLA
              (COND
               ((EVALGREATERP (AEVAL M) 0) (AEVAL (LIST 'COEFFN EWA '@KK M)))
               ((EVALEQUAL (AEVAL M) 0)
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL '@SS 0) (LIST 'EQUAL '@KK 0) EWA)))
               (T (AEVAL (LIST 'COEFFN EWA '@SS (LIST 'MINUS M))))))
      (RETURN
       (AEVAL
        (LIST 'S_PART
              (LIST 'SUB (LIST 'EQUAL '@SS 1) (LIST 'EQUAL '@KK 1)
                    (LIST 'EQUAL (LIST 'D (MINUS 33)) (LIST 'D (MINUS 3)))
                    (LIST 'EQUAL (LIST 'D (MINUS 44)) (LIST 'D (MINUS 4))) OLA)
              N))))) 
(PUT 'D_PART 'NUMBER-OF-ARGS 2) 
(FLAG '(D_PART) 'OPFN) 
(PUT 'D_PART 'DEFINED-ON-LINE '839) 
(PUT 'D_PART 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'D_PART 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE D_PART (WW N)
    (PROG (EWA OLA)
      (SETQ EWA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) '@KK)
                     (LIST 'EQUAL (LIST 'D 2) '@KK)
                     (LIST 'EQUAL (LIST 'D (MINUS 1)) '@SS)
                     (LIST 'EQUAL (LIST 'D (MINUS 2)) '@SS)
                     (LIST 'EQUAL (LIST 'D (MINUS 3))
                           (LIST 'TIMES '@SS (LIST 'D (MINUS 33))))
                     (LIST 'EQUAL (LIST 'D (MINUS 4))
                           (LIST 'TIMES (LIST 'D (MINUS 44)) '@SS))
                     WW)))
      (SETQ OLA
              (COND
               ((EVALGREATERP (AEVAL N) 0) (AEVAL (LIST 'COEFFN EWA '@KK N)))
               ((EVALEQUAL (AEVAL N) 0)
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL '@KK 0) (LIST 'EQUAL '@SS 0) EWA)))
               (T (AEVAL (LIST 'COEFFN EWA '@SS (LIST 'MINUS N))))))
      (RETURN
       (AEVAL
        (LIST 'SUB (LIST 'EQUAL (LIST 'D (MINUS 33)) (LIST 'D (MINUS 3)))
              (LIST 'EQUAL (LIST 'D (MINUS 44)) (LIST 'D (MINUS 4))) OLA))))) 
(PUT 'PR 'NUMBER-OF-ARGS 2) 
(FLAG '(PR) 'OPFN) 
(PUT 'PR 'DEFINED-ON-LINE '846) 
(PUT 'PR 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'PR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PR (N WW)
    (PROG (EWA)
      (COND ((EVALEQUAL (AEVAL N) 0) (SETQ EWA (AEVAL WW))))
      (COND
       ((EVALEQUAL (AEVAL N) 1)
        (SETQ EWA (AEVAL (LIST 'TIMES (LIST 'DER 1) WW)))))
      (COND
       ((EVALEQUAL (AEVAL N) 2)
        (SETQ EWA (AEVAL (LIST 'TIMES (LIST 'DER 2) WW)))))
      (COND
       ((EVALEQUAL (AEVAL N) 3)
        (COND
         ((EVALEQUAL (AEVAL 'ABRA_KADABRA) 3)
          (SETQ EWA (AEVAL (LIST 'TIMES (LIST 'DER 3) WW))))
         (T (SETQ EWA (AEVAL (LIST 'TIMES (LIST 'DER 1) (LIST 'PR 2 WW))))))))
      (RETURN
       (AEVAL
        (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) 0) (LIST 'EQUAL (LIST 'DER 2) 0)
              (LIST 'EQUAL (LIST 'DER 3) 0) EWA))))) 
(SETK '@RAK
      (AEVAL
       (LIST 'LIST (LIST 'EQUAL (LIST 'STP 1) 1) (LIST 'EQUAL (LIST 'STP 10) 1)
             (LIST 'EQUAL (LIST 'STP 20) 1)
             (LIST 'EQUAL (LIST 'STP 2) (MINUS 1))))) 
(PUT 'CP 'NUMBER-OF-ARGS 1) 
(FLAG '(CP) 'OPFN) 
(PUT 'CP 'DEFINED-ON-LINE '859) 
(PUT 'CP 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'CP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CP (XWX)
    (PROG (KAP KAP1 ESS K L)
      (COND ((EVALEQUAL (AEVAL XWX) 0) (RETURN 0)))
      (SETQ KAP (AEVAL (LIST 'LENGTH XWX)))
      (COND ((EVALNUMBERP (AEVAL KAP)) (RETURN (AEVAL (LIST 'CP1 XWX)))))
      (SETQ KAP1 (AEVAL (LIST 'FIRST KAP)))
      (AEVAL (MATRIX (LIST (LIST '@Z_Z KAP1 KAP1))))
      (AEVAL (MATRIX (LIST (LIST '@S_S KAP1 KAP1))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP1) K)) (RETURN NIL)))
        (PROG (L)
          (SETQ L 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP1) L)) (RETURN NIL)))
          (PROGN
           (SETQ ESS
                   (AEVAL*
                    (LIST 'SUB (LIST 'EQUAL '@KRR 1)
                          (LIST 'TIMES '@KRR (LIST XWX K L)))))
           (SETK (LIST '@Z_Z K L) (AEVAL* (LIST 'CP1 ESS)))
           (AEVAL* (CLEAR (LIST ESS)))
           (AEVAL* 'NIL))
          (SETQ L
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   L))
          (GO LAB))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (AEVAL (CLEAR (LIST '@KRR)))
      (SETK '@S_S (AEVAL (LIST 'TP '@Z_Z)))
      (AEVAL (CLEAR (LIST '@Z_Z)))
      (RETURN (AEVAL '@S_S)))) 
(PUT 'CP1 'NUMBER-OF-ARGS 1) 
(FLAG '(CP1) 'OPFN) 
(PUT 'CP1 'DEFINED-ON-LINE '868) 
(PUT 'CP1 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'CP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CP1 (YYZ)
    (PROG (EWA OLA XX YYY)
      (COND ((EVALEQUAL (AEVAL YYZ) 0) (RETURN 0)))
      (SETQ YYY
              (COND
               ((AND (EVALEQUAL (AEVAL (LIST 'LENGTH YYZ)) 1)
                     (EVALEQUAL (AEVAL (LIST 'ARGLENGTH YYZ)) (MINUS 1)))
                (AEVAL (LIST 'TIMES '@ YYZ)))
               (T (AEVAL YYZ))))
      (AEVAL (FACTOR (LIST 'D 'DER 'DEL)))
      (SETQ EWA (AEVAL (LIST 'LYST YYY)))
      (SETQ OLA
              (PROG (XX FORALL-RESULT FORALL-ENDPTR)
                (SETQ XX (GETRLIST (AEVAL EWA)))
                (COND ((NULL XX) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (XX)
                                    (PROG (MEW WEM EM1 EM2 EM EM3 LICZ MIAN)
                                      (SETQ LICZ (AEVAL (LIST 'NUM XX)))
                                      (SETQ MIAN (AEVAL (LIST 'DEN XX)))
                                      (COND
                                       ((EVALNUMBERP (AEVAL LICZ))
                                        (RETURN (AEVAL XX))))
                                      (SETQ MEW
                                              (AEVAL
                                               (LIST 'TIMES LICZ
                                                     (LIST 'STP 1))))
                                      (SETQ WEM (AEVAL (LIST 'SUB '@RAK MEW)))
                                      (SETQ EM
                                              (COND
                                               ((EVALEQUAL
                                                 (AEVAL (LIST 'PART WEM 0))
                                                 (AEVAL 'MINUS))
                                                (MINUS 1))
                                               (T 1)))
                                      (SETQ EM1
                                              (AEVAL
                                               (LIST 'CP2
                                                     (LIST 'TIMES EM WEM))))
                                      (SETQ EM2
                                              (AEVAL
                                               (LIST 'SETPART*
                                                     (LIST 'REVERSE EM1) 0
                                                     (AEVAL 'TIMES))))
                                      (RETURN
                                       (AEVAL
                                        (LIST 'TIMES EM2
                                              (LIST 'QUOTIENT EM MIAN))))))
                                  (CAR XX))
                                 NIL)))
               LOOPLABEL
                (SETQ XX (CDR XX))
                (COND ((NULL XX) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (XX)
                            (PROG (MEW WEM EM1 EM2 EM EM3 LICZ MIAN)
                              (SETQ LICZ (AEVAL (LIST 'NUM XX)))
                              (SETQ MIAN (AEVAL (LIST 'DEN XX)))
                              (COND
                               ((EVALNUMBERP (AEVAL LICZ))
                                (RETURN (AEVAL XX))))
                              (SETQ MEW
                                      (AEVAL (LIST 'TIMES LICZ (LIST 'STP 1))))
                              (SETQ WEM (AEVAL (LIST 'SUB '@RAK MEW)))
                              (SETQ EM
                                      (COND
                                       ((EVALEQUAL (AEVAL (LIST 'PART WEM 0))
                                                   (AEVAL 'MINUS))
                                        (MINUS 1))
                                       (T 1)))
                              (SETQ EM1
                                      (AEVAL (LIST 'CP2 (LIST 'TIMES EM WEM))))
                              (SETQ EM2
                                      (AEVAL
                                       (LIST 'SETPART* (LIST 'REVERSE EM1) 0
                                             (AEVAL 'TIMES))))
                              (RETURN
                               (AEVAL
                                (LIST 'TIMES EM2 (LIST 'QUOTIENT EM MIAN))))))
                          (CAR XX))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (AEVAL (REMFAC (LIST 'D 'DER 'DEL)))
      (RETURN
       (AEVAL
        (LIST 'SUB (LIST 'EQUAL '@ 1) (LIST 'SETPART* OLA 0 (AEVAL 'PLUS))))))) 
(PUT 'CP2 'NUMBER-OF-ARGS 1) 
(FLAG '(CP2) 'OPFN) 
(PUT 'CP2 'DEFINED-ON-LINE '881) 
(PUT 'CP2 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'CP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CP2 (ZZ)
    (PROG (EWA OLA ELA EL1)
      (COND
       ((EVALEQUAL (AEVAL (LIST 'ARGLENGTH (LIST 'TIMES 'PARTDF ZZ))) 2)
        (RETURN (AEVAL (LIST 'LIST ZZ)))))
      (SETQ EWA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK4) ZZ)))
      (SETQ OLA (AEVAL (LIST 'QUOTIENT ZZ EWA)))
      (SETQ ELA
              (COND
               ((EVALEQUAL (AEVAL (LIST 'ARGLENGTH (LIST 'TIMES '@ OLA))) 2)
                (AEVAL (LIST 'LIST OLA)))
               (T (AEVAL (LIST 'SETPART* OLA 0 (AEVAL 'LIST))))))
      (SETQ EL1 (AEVAL (LIST 'APPEND (LIST 'LIST EWA) ELA)))
      (RETURN (AEVAL EL1)))) 
(PUT 'ODWA 'NUMBER-OF-ARGS 1) 
(FLAG '(ODWA) 'OPFN) 
(PUT 'ODWA 'DEFINED-ON-LINE '892) 
(PUT 'ODWA 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'ODWA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODWA (WX)
    (PROG (EWA OLA)
      (AEVAL (LET '(TRYK15)))
      (SETQ EWA
              (AEVAL
               (LIST 'SUB
                     (LIST 'EQUAL (LIST 'DER 1) (LIST 'MINUS (LIST 'DER 20)))
                     (LIST 'EQUAL (LIST 'DER 2) (LIST 'DER 10))
                     (LIST 'EQUAL (LIST 'DEL 1) (LIST 'MINUS (LIST 'DEL 20)))
                     (LIST 'EQUAL (LIST 'DEL 2) (LIST 'DEL 10)) WX)))
      (AEVAL (CLEARRULES (LIST 'TRYK15)))
      (AEVAL (LET '(TRYK9)))
      (SETQ OLA (AEVAL EWA))
      (AEVAL (CLEARRULES (LIST 'TRYK9)))
      (RETURN
       (AEVAL
        (LIST 'SUB (LIST 'EQUAL (LIST 'DER 10) (LIST 'DER 1))
              (LIST 'EQUAL (LIST 'DER 20) (LIST 'DER 2))
              (LIST 'EQUAL (LIST 'DEL 10) (LIST 'DEL 1))
              (LIST 'EQUAL (LIST 'DEL 20) (LIST 'DEL 2)) OLA))))) 
(PUT 'LYST 'NUMBER-OF-ARGS 1) 
(FLAG '(LYST) 'OPFN) 
(PUT 'LYST 'DEFINED-ON-LINE '901) 
(PUT 'LYST 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'LYST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LYST (WX)
    (PROG (EWA OLA KAP KAP1 ADAM)
      (COND ((EVALEQUAL (AEVAL WX) 0) (RETURN (AEVAL (LIST 'LIST 0)))))
      (AEVAL (FACTOR (LIST 'D 'DER 'DEL)))
      (SETQ KAP (AEVAL (LIST 'LENGTH WX)))
      (SETQ KAP1 (AEVAL (LIST 'ARGLENGTH WX)))
      (COND
       ((AND (EVALEQUAL (AEVAL KAP) 1) (EVALEQUAL (AEVAL KAP1) (MINUS 1)))
        (RETURN (AEVAL (LIST 'LIST WX)))))
      (COND
       ((EVALGREATERP (AEVAL KAP1) (AEVAL KAP))
        (RETURN (AEVAL (LIST 'LIST WX)))))
      (AEVAL (ON (LIST 'DIV)))
      (SETQ EWA (AEVAL WX))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART EWA 0)) (AEVAL 'PLUS))
        (SETQ ADAM (AEVAL (LIST 'SETPART* EWA 0 (AEVAL 'LIST)))))
       (T (SETQ ADAM (AEVAL (LIST 'LIST EWA)))))
      (AEVAL (OFF (LIST 'DIV)))
      (AEVAL (REMFAC (LIST 'D 'DER 'DEL)))
      (RETURN (AEVAL ADAM)))) 
(PUT 'LYST1 'NUMBER-OF-ARGS 1) 
(FLAG '(LYST1) 'OPFN) 
(PUT 'LYST1 'DEFINED-ON-LINE '909) 
(PUT 'LYST1 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'LYST1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LYST1 (WY)
    (PROG (EWA OLA)
      (SETQ EWA (AEVAL (LIST 'LYST WY)))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK3) EWA)))
      (RETURN (AEVAL OLA)))) 
(PUT 'LYST2 'NUMBER-OF-ARGS 1) 
(FLAG '(LYST2) 'OPFN) 
(PUT 'LYST2 'DEFINED-ON-LINE '912) 
(PUT 'LYST2 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'LYST2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LYST2 (WY)
    (PROG (EWA OLA)
      (SETQ EWA (AEVAL (LIST 'LYST WY)))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK4) EWA)))
      (RETURN (AEVAL OLA)))) 
(PUT 'WAR 'NUMBER-OF-ARGS 2) 
(FLAG '(WAR) 'OPFN) 
(PUT 'WAR 'DEFINED-ON-LINE '918) 
(PUT 'WAR 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'WAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE WAR (WA F)
    (PROG (EWA OLA ADAM MEW)
      (AEVAL (LET '(TRYK)))
      (SETQ EWA
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'D F) WA)
                     (LIST 'TIMES WA (LIST 'D F)))))
      (AEVAL (CLEARRULES (LIST 'TRYK)))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK1) EWA)))
      (SETQ EWA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                     (LIST 'EQUAL (LIST 'DER 1) 0)
                     (LIST 'EQUAL (LIST 'DER 2) 0)
                     (LIST 'EQUAL (LIST 'DER 3) 0) OLA)))
      (COND ((EVALEQUAL (AEVAL EWA) 0) (RETURN 0)))
      (SETQ ADAM (AEVAL (LIST 'LYST EWA)))
      (SETQ MEW (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK3) ADAM)))
      (RETURN
       (COND ((EVALEQUAL (AEVAL MEW) 0) (AEVAL (LIST 'LIST)))
             (T (AEVAL MEW)))))) 
(PUT 'DYW 'NUMBER-OF-ARGS 2) 
(FLAG '(DYW) 'OPFN) 
(PUT 'DYW 'DEFINED-ON-LINE '925) 
(PUT 'DYW 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'DYW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DYW (WA F)
    (PROG (EWA OLA)
      (SETQ EWA
              (AEVAL
               (LIST 'WHEREEXP (LIST 'LIST 'TRYK)
                     (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'D F) WA)
                           (LIST 'TIMES WA (LIST 'D F))))))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK2) EWA)))
      (SETQ EWA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                     (LIST 'EQUAL (LIST 'DER 1) 0)
                     (LIST 'EQUAL (LIST 'DER 2) 0)
                     (LIST 'EQUAL (LIST 'DER 3) 0) OLA)))
      (COND ((EVALEQUAL (AEVAL EWA) 0) (RETURN 0)))
      (RETURN (AEVAL (LIST 'LYST EWA))))) 
(PUT 'GRA 'NUMBER-OF-ARGS 2) 
(FLAG '(GRA) 'OPFN) 
(PUT 'GRA 'DEFINED-ON-LINE '933) 
(PUT 'GRA 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'GRA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GRA (WA F)
    (PROG (EWA OLA)
      (SETQ EWA
              (AEVAL
               (LIST 'WHEREEXP (LIST 'LIST 'TRYK)
                     (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'D F) WA)
                           (LIST 'TIMES WA (LIST 'D F))))))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK1) EWA)))
      (RETURN
       (AEVAL
        (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0) (LIST 'EQUAL (LIST 'DER 1) 0)
              (LIST 'EQUAL (LIST 'DER 2) 0) (LIST 'EQUAL (LIST 'DER 3) 0)
              OLA))))) 
(PUT 'FPART 'NUMBER-OF-ARGS 1) 
(FLAG '(FPART) 'OPFN) 
(PUT 'FPART 'DEFINED-ON-LINE '940) 
(PUT 'FPART 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'FPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FPART (WX)
    (PROG (EWA OLA ADAM)
      (SETQ EWA
              (COND
               ((EVALEQUAL (AEVAL 'ABRA_KADABRA) 2)
                (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK5) WX)))
               (T (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK16) WX)))))
      (SETQ OLA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'TET 1) '|#QW|)
                     (LIST 'EQUAL (LIST 'TET 2) '|#QQ|) EWA)))
      (SETQ ADAM
              (AEVAL
               (LIST 'LIST (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 0) '|#QQ| 0)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 1) '|#QQ| 0)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 0) '|#QQ| 1)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 1) '|#QQ| 1))))
      (RETURN (AEVAL ADAM)))) 
(PUT 'BPART 'NUMBER-OF-ARGS 1) 
(FLAG '(BPART) 'OPFN) 
(PUT 'BPART 'DEFINED-ON-LINE '948) 
(PUT 'BPART 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'BPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BPART (WX)
    (PROG (EWA OLA ADAM)
      (SETQ EWA
              (COND
               ((EVALEQUAL (AEVAL 'ABRA_KADABRA) 2)
                (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK5) WX)))
               (T (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK16) WX)))))
      (AEVAL (LET '(TRYK6)))
      (SETQ OLA
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'TET 1) '|#QW|)
                     (LIST 'EQUAL (LIST 'TET 2) '|#QQ|) EWA)))
      (AEVAL (CLEARRULES (LIST 'TRYK6)))
      (SETQ ADAM
              (AEVAL
               (LIST 'LIST (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 0) '|#QQ| 0)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 1) '|#QQ| 0)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 0) '|#QQ| 1)
                     (LIST 'COEFFN (LIST 'COEFFN OLA '|#QW| 1) '|#QQ| 1))))
      (RETURN (AEVAL ADAM)))) 
(PUT 'KOZA 'NUMBER-OF-ARGS 3) 
(FLAG '(KOZA) 'OPFN) 
(PUT 'KOZA 'DEFINED-ON-LINE '959) 
(PUT 'KOZA 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'KOZA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE KOZA (WX WI WN)
    (PROG (EW1 EW2 AM)
      (SETK 'EW3 (AEVAL (LIST 'PART WX 3)))
      (SETQ EW1 (AEVAL (LIST 'PART WX 1)))
      (SETQ AM
              (COND
               ((OR
                 (AND (EQ (REVALX 'EW3) (REVALX 'F)) (EVALEQUAL (AEVAL WI) 0))
                 (AND (EQ (REVALX 'EW3) (REVALX 'F)) (EVALEQUAL (AEVAL WI) 3)))
                (AEVAL (LIST 'TIMES '@X_Y (LIST '@G_G EW1 WI WN))))
               ((OR
                 (AND (EQ (REVALX 'EW3) (REVALX 'F)) (EVALEQUAL (AEVAL WI) 1))
                 (AND (EQ (REVALX 'EW3) (REVALX 'F)) (EVALEQUAL (AEVAL WI) 2)))
                (AEVAL
                 (LIST 'TIMES (LIST 'EXPT '@X_Y 2) (LIST '@F_F EW1 WI WN))))
               ((OR
                 (AND (EQ (REVALX 'EW3) (REVALX 'B)) (EVALEQUAL (AEVAL WI) 0))
                 (AND (EQ (REVALX 'EW3) (REVALX 'B)) (EVALEQUAL (AEVAL WI) 3)))
                (AEVAL
                 (LIST 'TIMES (LIST 'EXPT '@X_Y 2) (LIST '@F_F EW1 WI WN))))
               ((OR
                 (AND (EQ (REVALX 'EW3) (REVALX 'B)) (EVALEQUAL (AEVAL WI) 1))
                 (AND (EQ (REVALX 'EW3) (REVALX 'B)) (EVALEQUAL (AEVAL WI) 2)))
                (AEVAL (LIST 'TIMES '@X_Y (LIST '@G_G EW1 WI WN))))))
      (RETURN (AEVAL AM)))) 
(PUT 'W_COMB 'NUMBER-OF-ARGS 4) 
(FLAG '(W_COMB) 'OPFN) 
(PUT 'W_COMB 'DEFINED-ON-LINE '971) 
(PUT 'W_COMB 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'W_COMB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE W_COMB (AS M A BB)
    (PROG (KAP EWA OLA WIC WID WOD WOD1 WX K S *PRECISE)
      (SETQ KAP (AEVAL (LIST 'LENGTH AS)))
      (COND ((EVALEQUAL (AEVAL M) 0) (RETURN 0)))
      (COND
       ((EVALEQUAL (AEVAL M) (AEVAL '(|:DN:| 5 . -1)))
        (RETURN (AEVAL (LIST 'W_COMB1 AS A BB)))))
      (SETK (LIST 'EXPT '|#L| (LIST 'PLUS M 1)) (AEVAL 0))
      (SETK (LIST 'EXPT '|#L| (LIST 'PLUS M (LIST 'QUOTIENT 1 2))) (AEVAL 0))
      (SETQ EWA
              (PROG (S FORALL-RESULT)
                (SETQ S 0)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'FLOOR M)) S))
                  (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (PROG (K FORALL-RESULT)
                                 (SETQ K 1)
                                 (SETQ FORALL-RESULT 0)
                                LAB1
                                 (COND
                                  ((|AMINUSP:|
                                    (LIST 'DIFFERENCE (AEVAL* KAP) K))
                                   (RETURN FORALL-RESULT)))
                                 (SETQ FORALL-RESULT
                                         (AEVAL*
                                          (LIST 'PLUS
                                                (AEVAL*
                                                 (LIST 'TIMES
                                                       (LIST 'EXPT '|#L|
                                                             (LIST 'PLUS
                                                                   (LIST 'PART
                                                                         AS K
                                                                         2)
                                                                   S))
                                                       (LIST 'PLUS
                                                             (LIST 'TIMES
                                                                   (LIST 'KOZA
                                                                         (LIST
                                                                          'PART
                                                                          AS K)
                                                                         0 S)
                                                                   2)
                                                             (LIST 'TIMES '|#L|
                                                                   (LIST 'KOZA
                                                                         (LIST
                                                                          'PART
                                                                          AS K)
                                                                         3 S))
                                                             (LIST 'TIMES
                                                                   (LIST 'EXPT
                                                                         '|#L|
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          1 2))
                                                                   (LIST 'KOZA
                                                                         (LIST
                                                                          'PART
                                                                          AS K)
                                                                         1 S))
                                                             (LIST 'TIMES
                                                                   (LIST 'EXPT
                                                                         '|#L|
                                                                         (LIST
                                                                          'QUOTIENT
                                                                          1 2))
                                                                   (LIST 'KOZA
                                                                         (LIST
                                                                          'PART
                                                                          AS K)
                                                                         2
                                                                         S)))))
                                                FORALL-RESULT)))
                                 (SETQ K
                                         ((LAMBDA (FORALL-RESULT)
                                            (AEVAL*
                                             (LIST 'PLUS FORALL-RESULT 1)))
                                          K))
                                 (GO LAB1))
                               FORALL-RESULT)))
                (SETQ S
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         S))
                (GO LAB1)))
      (SETQ OLA (AEVAL EWA))
      (SETQ WIC (AEVAL EWA))
      (PROG (K)
        (SETQ K 0)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'FLOOR M)) K))
          (RETURN NIL)))
        (PROGN
         (SETQ OLA (AEVAL* (LIST 'TIMES EWA OLA)))
         (SETQ EWA
                 (PROG (S FORALL-RESULT)
                   (SETQ S 0)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND
                    ((|AMINUSP:|
                      (LIST 'DIFFERENCE
                            (AEVAL* (LIST 'PLUS (LIST 'DIFFERENCE M K) 1)) S))
                     (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (PROG (R FORALL-RESULT)
                                    (SETQ R 1)
                                    (SETQ FORALL-RESULT 0)
                                   LAB1
                                    (COND
                                     ((|AMINUSP:|
                                       (LIST 'DIFFERENCE (AEVAL* KAP) R))
                                      (RETURN FORALL-RESULT)))
                                    (SETQ FORALL-RESULT
                                            (AEVAL*
                                             (LIST 'PLUS
                                                   (AEVAL*
                                                    (LIST 'TIMES
                                                          (LIST 'EXPT '|#L|
                                                                (LIST 'PLUS
                                                                      (LIST
                                                                       'PART AS
                                                                       R 2)
                                                                      S))
                                                          (LIST 'PLUS
                                                                (LIST 'TIMES
                                                                      (LIST
                                                                       'KOZA
                                                                       (LIST
                                                                        'PART
                                                                        AS R)
                                                                       0 S)
                                                                      2)
                                                                (LIST 'TIMES
                                                                      '|#L|
                                                                      (LIST
                                                                       'KOZA
                                                                       (LIST
                                                                        'PART
                                                                        AS R)
                                                                       3 S))
                                                                (LIST 'TIMES
                                                                      (LIST
                                                                       'EXPT
                                                                       '|#L|
                                                                       (LIST
                                                                        'QUOTIENT
                                                                        1 2))
                                                                      (LIST
                                                                       'KOZA
                                                                       (LIST
                                                                        'PART
                                                                        AS R)
                                                                       1 S))
                                                                (LIST 'TIMES
                                                                      (LIST
                                                                       'EXPT
                                                                       '|#L|
                                                                       (LIST
                                                                        'QUOTIENT
                                                                        1 2))
                                                                      (LIST
                                                                       'KOZA
                                                                       (LIST
                                                                        'PART
                                                                        AS R)
                                                                       2 S)))))
                                                   FORALL-RESULT)))
                                    (SETQ R
                                            ((LAMBDA (FORALL-RESULT)
                                               (AEVAL*
                                                (LIST 'PLUS FORALL-RESULT 1)))
                                             R))
                                    (GO LAB1))
                                  FORALL-RESULT)))
                   (SETQ S
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            S))
                   (GO LAB1)))
         (SETQ WIC (AEVAL* (LIST 'PLUS WIC OLA)))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ WID
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL '|#L| (LIST 'EXPT '|#LL| 2)) WIC)))
      (SETQ WOD (AEVAL (LIST 'COEFFN WID '|#LL| (LIST 'TIMES 2 M))))
      (SETQ WOD
              (COND
               ((EQ (REVALX BB) (REVALX 'B))
                (AEVAL (LIST 'SUB (LIST 'EQUAL '@X_Y 0) WOD)))
               ((EQ (REVALX BB) (REVALX 'F))
                (AEVAL (LIST 'COEFFN WOD '@X_Y 1)))))
      (SETQ WOD1 (AEVAL (LIST 'LYST (LIST 'WHEREEXP (LIST 'LIST 'TRYK7) WOD))))
      (AEVAL
       (CLEAR
        (LIST (LIST 'EXPT '|#L| (LIST 'PLUS M 1))
              (LIST 'EXPT '|#L| (LIST 'PLUS M (LIST 'QUOTIENT 1 2))))))
      (SETQ KAP (AEVAL (LIST 'LENGTH WOD1)))
      (SETQ EWA (AEVAL 0))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) K)) (RETURN NIL)))
        (PROGN
         (SETK 'ADAM
               (COND ((EVALEQUAL (AEVAL* WOD1) 0) 0)
                     (T (AEVAL* (LIST 'PART WOD1 K)))))
         (SETQ OLA
                 (COND ((EVALEQUAL (AEVAL* 'ADAM) 0) 0)
                       ((EVALEQUAL (AEVAL* (LIST 'PART 'ADAM 0))
                                   (AEVAL* 'MINUS))
                        (AEVAL* (LIST 'MINUS 'ADAM)))
                       (T (AEVAL* 'ADAM))))
         (SETQ WX
                 (COND ((EVALEQUAL (AEVAL* OLA) 0) 1)
                       ((EVALEQUAL (AEVAL* (LIST 'PART OLA 0)) (AEVAL* 'TIMES))
                        (COND
                         ((FIXP (REVALX (LIST 'PART OLA 1)))
                          (AEVAL* (LIST 'PART OLA 1)))
                         (T 1)))
                       (T 1)))
         (SETQ EWA
                 (AEVAL*
                  (LIST 'PLUS EWA
                        (LIST 'TIMES (LIST 'MKID A K)
                              (LIST 'QUOTIENT OLA WX)))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL EWA)))) 
(PUT 'W_COMB1 'NUMBER-OF-ARGS 3) 
(FLAG '(W_COMB1) 'OPFN) 
(PUT 'W_COMB1 'DEFINED-ON-LINE '1003) 
(PUT 'W_COMB1 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'W_COMB1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE W_COMB1 (AS A BB)
    (PROG (EW KAP1 EW1 KAP)
      (SETQ KAP (AEVAL (LIST 'LENGTH AS)))
      (SETQ EW
              (PROG (N FORALL-RESULT FORALL-ENDPTR)
                (SETQ N 1)
               STARTOVER
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) N))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (COND
                         ((EVALNEQ (AEVAL* (LIST 'PART AS N 2))
                                   (AEVAL* (LIST 'QUOTIENT 1 2)))
                          (AEVAL* (LIST 'LIST)))
                         ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'F))
                               (EQ (REVALX BB) (REVALX 'F)))
                          (AEVAL*
                           (LIST 'LIST (LIST 'FER (LIST 'PART AS N 1) 0 0))))
                         ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'F))
                               (EQ (REVALX BB) (REVALX 'B)))
                          (AEVAL* (LIST 'LIST)))
                         ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'B))
                               (EQ (REVALX BB) (REVALX 'F)))
                          (AEVAL* (LIST 'LIST)))
                         ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'B))
                               (EQ (REVALX BB) (REVALX 'B)))
                          (AEVAL*
                           (LIST 'LIST (LIST 'BOS (LIST 'PART AS N 1) 0 0))))))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ N
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         N))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) N))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         (COND
                          ((EVALNEQ (AEVAL* (LIST 'PART AS N 2))
                                    (AEVAL* (LIST 'QUOTIENT 1 2)))
                           (AEVAL* (LIST 'LIST)))
                          ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'F))
                                (EQ (REVALX BB) (REVALX 'F)))
                           (AEVAL*
                            (LIST 'LIST (LIST 'FER (LIST 'PART AS N 1) 0 0))))
                          ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'F))
                                (EQ (REVALX BB) (REVALX 'B)))
                           (AEVAL* (LIST 'LIST)))
                          ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'B))
                                (EQ (REVALX BB) (REVALX 'F)))
                           (AEVAL* (LIST 'LIST)))
                          ((AND (EQ (REVALX (LIST 'PART AS N 3)) (REVALX 'B))
                                (EQ (REVALX BB) (REVALX 'B)))
                           (AEVAL*
                            (LIST 'LIST
                                  (LIST 'BOS (LIST 'PART AS N 1) 0 0)))))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ N
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         N))
                (GO LOOPLABEL)))
      (SETQ KAP1 (AEVAL (LIST 'LENGTH EW)))
      (SETQ EW1
              (COND ((EVALEQUAL (AEVAL KAP1) 0) 0)
                    (T
                     (PROG (N FORALL-RESULT)
                       (SETQ N 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP1) N))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'MKID A N)
                                             (LIST 'PART EW N)))
                                      FORALL-RESULT)))
                       (SETQ N
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                N))
                       (GO LAB1)))))
      (RETURN (AEVAL EW1)))) 
(PUT 'FCOMB 'NUMBER-OF-ARGS 4) 
(FLAG '(FCOMB) 'OPFN) 
(PUT 'FCOMB 'DEFINED-ON-LINE '1017) 
(PUT 'FCOMB 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'FCOMB 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE FCOMB (AS N B BB)
    (PROG (EWA OLA ALA K KAP WX WY KAP1 WOD WOD1 EMA WZ WZ1)
      (AEVAL (OPERATOR (LIST B)))
      (SETQ EWA (AEVAL (LIST 'W_COMB AS N 'A BB)))
      (SETQ KAP (AEVAL (LIST 'LENGTH AS)))
      (SETQ ALA (AEVAL (LIST 'LIST)))
      (SETQ WZ (AEVAL EWA))
      (SETQ WZ1 (AEVAL EWA))
      (SETQ EMA (AEVAL (LIST 'LIST)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) K)) (RETURN NIL)))
        (PROGN
         (SETQ WZ (AEVAL* (LIST 'SUB (LIST 'EQUAL (LIST 'PART AS K 1) 0) WZ)))
         (SETQ WX (AEVAL* (LIST 'DIFFERENCE WZ1 WZ)))
         (SETQ WZ1 (AEVAL* WZ))
         (SETQ EMA (AEVAL* (LIST 'APPEND EMA (LIST 'LIST (LIST 'LIST WX)))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) K)) (RETURN NIL)))
        (PROGN
         (SETQ WX
                 (AEVAL* (LIST 'DYW (LIST 'PART EMA K 1) (LIST 'PART AS K 1))))
         (SETQ WY
                 (COND ((EVALEQUAL (AEVAL* WX) 0) (AEVAL* (LIST 'LIST)))
                       (T (AEVAL* WX))))
         (SETQ ALA (AEVAL* (LIST 'APPEND ALA WY)))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ KAP1 (AEVAL (LIST 'LENGTH ALA)))
      (SETQ EWA (AEVAL 0))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP1) K)) (RETURN NIL)))
        (PROGN
         (SETQ WOD (AEVAL* (LIST 'PART ALA K)))
         (SETQ WOD1 (AEVAL* (LIST 'WHEREEXP (LIST 'LIST 'TRYK3) WOD)))
         (SETQ EWA
                 (AEVAL*
                  (LIST 'PLUS EWA
                        (LIST 'TIMES (LIST B K) (LIST 'QUOTIENT WOD WOD1)))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL EWA)))) 
(PUT 'PSE_ELE 'NUMBER-OF-ARGS 3) 
(FLAG '(PSE_ELE) 'OPFN) 
(PUT 'PSE_ELE 'DEFINED-ON-LINE '1029) 
(PUT 'PSE_ELE 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'PSE_ELE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PSE_ELE (N WW SS)
    (PROG (EWA OLA KAP K MAJ MAJ1 ELA)
      (SETQ EWA (AEVAL 0))
      (AEVAL (OPERATOR (LIST SS)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) K)) (RETURN NIL)))
        (PROGN
         (SETQ EWA
                 (AEVAL*
                  (LIST 'PLUS EWA
                        (LIST 'TIMES
                              (LIST 'PLUS
                                    (LIST 'W_COMB WW K
                                          (LIST 'MKID (LIST 'MKID 'A K) 'A) 'B)
                                    (LIST 'TIMES
                                          (LIST 'W_COMB WW
                                                (LIST 'DIFFERENCE K
                                                      (LIST 'QUOTIENT 1 2))
                                                (LIST 'MKID (LIST 'MKID 'A K)
                                                      'B)
                                                'F)
                                          (LIST 'DER 1))
                                    (LIST 'TIMES
                                          (LIST 'W_COMB WW
                                                (LIST 'DIFFERENCE K
                                                      (LIST 'QUOTIENT 1 2))
                                                (LIST 'MKID (LIST 'MKID 'A K)
                                                      'C)
                                                'F)
                                          (LIST 'DER 2))
                                    (LIST 'TIMES
                                          (LIST 'W_COMB WW
                                                (LIST 'DIFFERENCE K 1)
                                                (LIST 'MKID (LIST 'MKID 'A K)
                                                      'D)
                                                'B)
                                          (COND
                                           ((EVALEQUAL (AEVAL* 'ABRA_KADABRA)
                                                       3)
                                            (AEVAL* (LIST 'DER 3)))
                                           (T
                                            (AEVAL*
                                             (LIST 'TIMES (LIST 'DER 1)
                                                   (LIST 'DER 2)))))))
                              (LIST 'EXPT (LIST 'D 1)
                                    (LIST 'DIFFERENCE N K))))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (AEVAL (REMFAC (LIST 'FER 'BOS)))
      (SETQ KAP (AEVAL (LIST 'LENGTH EWA)))
      (SETQ OLA (AEVAL 0))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) K)) (RETURN NIL)))
        (PROGN
         (SETQ MAJ
                 (COND ((EVALEQUAL (AEVAL* EWA) 0) 0)
                       ((EVALEQUAL (AEVAL* KAP) 1) (AEVAL* EWA))
                       (T (AEVAL* (LIST 'PART EWA K)))))
         (SETQ MAJ1
                 (COND ((EVALEQUAL (AEVAL* MAJ) 0) 1)
                       (T (AEVAL* (LIST 'WHEREEXP (LIST 'LIST 'TRYK4) MAJ)))))
         (SETQ OLA
                 (AEVAL*
                  (LIST 'PLUS OLA
                        (LIST 'TIMES (LIST SS (LIST 'PLUS K 1))
                              (LIST 'QUOTIENT MAJ MAJ1)))))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ ELA
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES (LIST SS 0) (LIST 'EXPT (LIST 'D 1) N))
                     (LIST 'TIMES (LIST SS 1)
                           (COND
                            ((EVALEQUAL (AEVAL 'ABRA_KADABRA) 3)
                             (AEVAL (LIST 'DER 3)))
                            (T
                             (AEVAL
                              (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 2)))))
                           (LIST 'EXPT (LIST 'D 1) (LIST 'DIFFERENCE N 1)))
                     OLA)))
      (AEVAL (FACTOR (LIST 'FER 'BOS)))
      (COND
       ((EVALEQUAL (AEVAL 'ABRA_KADABRA) 3)
        (SETQ ELA
                (AEVAL
                 (LIST 'WHEREEXP
                       (LIST 'LIST
                             (LIST 'LIST
                                   (LIST 'REPLACEBY
                                         (LIST 'TIMES (LIST 'DER 1)
                                               (LIST 'DER 2))
                                         (LIST 'DER 3))))
                       ELA)))))
      (RETURN (AEVAL ELA)))) 
(PUT 'N_GAT 'NUMBER-OF-ARGS 2) 
(FLAG '(N_GAT) 'OPFN) 
(PUT 'N_GAT 'DEFINED-ON-LINE '1052) 
(PUT 'N_GAT 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'N_GAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_GAT (PP WIM)
    (PROG (KAP NIECH ZYJE)
      (SETQ KAP (AEVAL (LIST 'LENGTH WIM)))
      (SETQ NIECH (AEVAL (LIST 'GATO PP)))
      (AEVAL (LET (LIST WIM)))
      (SETQ ZYJE (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK10) NIECH)))
      (AEVAL (CLEARRULES (LIST WIM)))
      (RETURN (AEVAL ZYJE)))) 
(PUT 'GATO 'NUMBER-OF-ARGS 1) 
(FLAG '(GATO) 'OPFN) 
(PUT 'GATO 'DEFINED-ON-LINE '1058) 
(PUT 'GATO 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'GATO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GATO (P)
    (PROG (AS ESS MESS)
      (COND
       ((EVALNUMBERP (AEVAL (LIST 'LENGTH P)))
        (RETURN (AEVAL (LIST 'GAT1 P)))))
      (SETQ AS (AEVAL (LIST 'FIRST (LIST 'LENGTH P))))
      (AEVAL (MATRIX (LIST (LIST '|#ZZ| AS AS))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K)) (RETURN NIL)))
        (PROG (L)
          (SETQ L 1)
         LAB
          (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) L)) (RETURN NIL)))
          (PROGN
           (SETQ ESS
                   (AEVAL*
                    (LIST 'WHEREEXP (LIST 'LIST 'TRYK8)
                          (LIST 'TIMES '|#ZAB| (LIST P K L)))))
           (SETQ MESS
                   (AEVAL*
                    (LIST 'WHEREEXP (LIST 'LIST 'TRYK9)
                          (LIST 'QUOTIENT ESS '|#ZAB|))))
           (SETK (LIST '|#ZZ| K L)
                 (AEVAL*
                  (LIST 'SUB (LIST 'EQUAL 'EPS 0) (LIST 'DF MESS 'EPS))))
           (AEVAL* 'NIL))
          (SETQ L
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   L))
          (GO LAB))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL '|#ZZ|)))) 
(PUT 'GAT1 'NUMBER-OF-ARGS 1) 
(FLAG '(GAT1) 'OPFN) 
(PUT 'GAT1 'DEFINED-ON-LINE '1066) 
(PUT 'GAT1 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'GAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GAT1 (P)
    (PROG (ESS ZZ MESS)
      (SETQ ESS (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK8) P)))
      (SETQ MESS (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK9) ESS)))
      (SETQ ZZ (AEVAL (LIST 'SUB (LIST 'EQUAL 'EPS 0) (LIST 'DF MESS 'EPS))))
      (RETURN (AEVAL ZZ)))) 
(PUT 'FJACOB 'NUMBER-OF-ARGS 2) 
(FLAG '(FJACOB) 'OPFN) 
(PUT 'FJACOB 'DEFINED-ON-LINE '1074) 
(PUT 'FJACOB 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'FJACOB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FJACOB (P W)
    (PROG (AS AS1 ES1 WOD0 WOD1 WOD2 WOD3 WODX)
      (COND
       ((EVALNUMBERP (AEVAL (LIST 'LENGTH P)))
        (RETURN (AEVAL (LIST 'JACOB1 P W)))))
      (SETQ AS (AEVAL (LIST 'FIRST (LIST 'LENGTH P))))
      (AEVAL (MATRIX (LIST (LIST '|#ALA| AS AS) (LIST '|#ELA| AS AS))))
      (SETK '|#ALA| (AEVAL (LIST 'GATO P)))
      (AEVAL (OPERATOR (LIST '|#A| '|#B| '|#C|)))
      (SETQ AS1
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (L FORALL-RESULT)
                                   (SETQ L 1)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE (AEVAL* AS) L))
                                     (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (AEVAL*
                                            (LIST 'PLUS
                                                  (AEVAL*
                                                   (LIST 'SUB
                                                         (LIST 'EQUAL
                                                               (LIST 'D 1) 0)
                                                         (LIST 'EQUAL
                                                               (LIST 'DER 1) 0)
                                                         (LIST 'EQUAL
                                                               (LIST 'DER 2) 0)
                                                         (LIST 'TIMES
                                                               (LIST P K L)
                                                               (LIST 'BOS
                                                                     (LIST
                                                                      '|#B| L)
                                                                     0 0))))
                                                  FORALL-RESULT)))
                                   (SETQ L
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            L))
                                   (GO LAB1))
                                 NIL)))
               LOOPLABEL
                (SETQ K
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         K))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (L FORALL-RESULT)
                           (SETQ L 1)
                           (SETQ FORALL-RESULT 0)
                          LAB1
                           (COND
                            ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) L))
                             (RETURN FORALL-RESULT)))
                           (SETQ FORALL-RESULT
                                   (AEVAL*
                                    (LIST 'PLUS
                                          (AEVAL*
                                           (LIST 'SUB
                                                 (LIST 'EQUAL (LIST 'D 1) 0)
                                                 (LIST 'EQUAL (LIST 'DER 1) 0)
                                                 (LIST 'EQUAL (LIST 'DER 2) 0)
                                                 (LIST 'TIMES (LIST P K L)
                                                       (LIST 'BOS
                                                             (LIST '|#B| L) 0
                                                             0))))
                                          FORALL-RESULT)))
                           (SETQ L
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    L))
                           (GO LAB1))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'BOS (LIST 'PART W K)) (AEVAL* (LIST 'PART AS1 K)))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETK '|#ELA| (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK10) '|#ALA|)))
      (SETK 'WOD
            (PROG (K FORALL-RESULT)
              (SETQ K 1)
              (SETQ FORALL-RESULT 0)
             LAB1
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K))
                (RETURN FORALL-RESULT)))
              (SETQ FORALL-RESULT
                      (AEVAL*
                       (LIST 'PLUS
                             (PROG (L FORALL-RESULT)
                               (SETQ L 1)
                               (SETQ FORALL-RESULT 0)
                              LAB1
                               (COND
                                ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) L))
                                 (RETURN FORALL-RESULT)))
                               (SETQ FORALL-RESULT
                                       (AEVAL*
                                        (LIST 'PLUS
                                              (AEVAL*
                                               (LIST 'TIMES
                                                     (LIST 'BOS (LIST '|#C| K)
                                                           0 0)
                                                     (LIST '|#ELA| K L)
                                                     (LIST 'BOS (LIST '|#A| L)
                                                           0 0)))
                                              FORALL-RESULT)))
                               (SETQ L
                                       ((LAMBDA (FORALL-RESULT)
                                          (AEVAL*
                                           (LIST 'PLUS FORALL-RESULT 1)))
                                        L))
                               (GO LAB1))
                             FORALL-RESULT)))
              (SETQ K
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       K))
              (GO LAB1)))
      (SETQ WOD1
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                     (LIST 'EQUAL (LIST 'DER 1) 0)
                     (LIST 'EQUAL (LIST 'DER 2) 0) 'WOD)))
      (SETQ WODX (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK11) WOD1)))
      (SETQ WOD2 (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK12) WODX)))
      (SETQ WOD3 (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK13) WODX)))
      (RETURN (AEVAL (LIST 'PLUS WOD1 WOD2 WOD3))))) 
(PUT 'JACOB 'NUMBER-OF-ARGS 3) 
(FLAG '(JACOB) 'OPFN) 
(PUT 'JACOB 'DEFINED-ON-LINE '1091) 
(PUT 'JACOB 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'JACOB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE JACOB (P W M)
    (PROG (WODA)
      (SETQ WODA
              (PROG (WX FORALL-RESULT)
                (SETQ WX 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 3 WX)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (PROG (TRYS AS1 AS WOD)
                                 (AEVAL*
                                  (OPERATOR
                                   (LIST '|#A| '|#B| '|#C| '@A '@B '@C)))
                                 (SETQ AS
                                         (AEVAL*
                                          (LIST 'FIRST (LIST 'LENGTH P))))
                                 (SETQ TRYS
                                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ K 1)
                                          STARTOVER
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE (AEVAL* AS) K))
                                             (RETURN (MAKELIST NIL))))
                                           (SETQ FORALL-RESULT
                                                   (AEVAL*
                                                    (LIST 'LIST
                                                          (LIST 'EQUAL
                                                                (LIST '@A K)
                                                                (COND
                                                                 ((EVALEQUAL K
                                                                             (AEVAL*
                                                                              (LIST
                                                                               'FIRST
                                                                               M)))
                                                                  (AEVAL*
                                                                   (LIST '|#A|
                                                                         K)))
                                                                 (T 0)))
                                                          (LIST 'EQUAL
                                                                (LIST '@B K)
                                                                (COND
                                                                 ((EVALEQUAL K
                                                                             (AEVAL*
                                                                              (LIST
                                                                               'SECOND
                                                                               M)))
                                                                  (AEVAL*
                                                                   (LIST '|#B|
                                                                         K)))
                                                                 (T 0)))
                                                          (LIST 'EQUAL
                                                                (LIST '@C K)
                                                                (COND
                                                                 ((EVALEQUAL K
                                                                             (AEVAL*
                                                                              (LIST
                                                                               'THIRD
                                                                               M)))
                                                                  (AEVAL*
                                                                   (LIST '|#C|
                                                                         K)))
                                                                 (T 0))))))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR
                                                    (CONS 'LIST
                                                          FORALL-RESULT)))
                                           (SETQ K
                                                   ((LAMBDA (FORALL-RESULT)
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             FORALL-RESULT 1)))
                                                    K))
                                           (COND
                                            ((ATOM FORALL-ENDPTR)
                                             (GO STARTOVER)))
                                          LOOPLABEL
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE (AEVAL* AS) K))
                                             (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (GETRLIST
                                                    (AEVAL*
                                                     (LIST 'LIST
                                                           (LIST 'EQUAL
                                                                 (LIST '@A K)
                                                                 (COND
                                                                  ((EVALEQUAL K
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'FIRST
                                                                                M)))
                                                                   (AEVAL*
                                                                    (LIST '|#A|
                                                                          K)))
                                                                  (T 0)))
                                                           (LIST 'EQUAL
                                                                 (LIST '@B K)
                                                                 (COND
                                                                  ((EVALEQUAL K
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'SECOND
                                                                                M)))
                                                                   (AEVAL*
                                                                    (LIST '|#B|
                                                                          K)))
                                                                  (T 0)))
                                                           (LIST 'EQUAL
                                                                 (LIST '@C K)
                                                                 (COND
                                                                  ((EVALEQUAL K
                                                                              (AEVAL*
                                                                               (LIST
                                                                                'THIRD
                                                                                M)))
                                                                   (AEVAL*
                                                                    (LIST '|#C|
                                                                          K)))
                                                                  (T 0)))))))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR FORALL-ENDPTR))
                                           (SETQ K
                                                   ((LAMBDA (FORALL-RESULT)
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             FORALL-RESULT 1)))
                                                    K))
                                           (GO LOOPLABEL)))
                                 (AEVAL* (LET (LIST TRYS)))
                                 (AEVAL*
                                  (MATRIX
                                   (LIST (LIST '@ALA AS AS)
                                         (LIST '@ELA AS AS))))
                                 (SETK '@ALA (AEVAL* (LIST 'GATO P)))
                                 (SETQ AS1
                                         (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ K 1)
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE (AEVAL* AS) K))
                                             (RETURN (MAKELIST NIL))))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            (PROG (L
                                                                   FORALL-RESULT)
                                                              (SETQ L 1)
                                                              (SETQ FORALL-RESULT
                                                                      0)
                                                             LAB1
                                                              (COND
                                                               ((|AMINUSP:|
                                                                 (LIST
                                                                  'DIFFERENCE
                                                                  (AEVAL* AS)
                                                                  L))
                                                                (RETURN
                                                                 FORALL-RESULT)))
                                                              (SETQ FORALL-RESULT
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'PLUS
                                                                        (AEVAL*
                                                                         (LIST
                                                                          'SUB
                                                                          (LIST
                                                                           'EQUAL
                                                                           (LIST
                                                                            'D
                                                                            1)
                                                                           0)
                                                                          (LIST
                                                                           'EQUAL
                                                                           (LIST
                                                                            'DER
                                                                            1)
                                                                           0)
                                                                          (LIST
                                                                           'EQUAL
                                                                           (LIST
                                                                            'DER
                                                                            2)
                                                                           0)
                                                                          (LIST
                                                                           'TIMES
                                                                           (LIST
                                                                            P K
                                                                            L)
                                                                           (LIST
                                                                            'BOS
                                                                            (COND
                                                                             ((EQUAL
                                                                               WX
                                                                               1)
                                                                              (AEVAL*
                                                                               (LIST
                                                                                '@B
                                                                                L)))
                                                                             ((EQUAL
                                                                               WX
                                                                               2)
                                                                              (AEVAL*
                                                                               (LIST
                                                                                '@C
                                                                                L)))
                                                                             ((EQUAL
                                                                               WX
                                                                               3)
                                                                              (AEVAL*
                                                                               (LIST
                                                                                '@A
                                                                                L))))
                                                                            0
                                                                            0))))
                                                                        FORALL-RESULT)))
                                                              (SETQ L
                                                                      ((LAMBDA
                                                                           (
                                                                            FORALL-RESULT)
                                                                         (AEVAL*
                                                                          (LIST
                                                                           'PLUS
                                                                           FORALL-RESULT
                                                                           1)))
                                                                       L))
                                                              (GO LAB1))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ K
                                                   ((LAMBDA (FORALL-RESULT)
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             FORALL-RESULT 1)))
                                                    K))
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE (AEVAL* AS) K))
                                             (RETURN
                                              (CONS 'LIST FORALL-RESULT))))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    (PROG (L FORALL-RESULT)
                                                      (SETQ L 1)
                                                      (SETQ FORALL-RESULT 0)
                                                     LAB1
                                                      (COND
                                                       ((|AMINUSP:|
                                                         (LIST 'DIFFERENCE
                                                               (AEVAL* AS) L))
                                                        (RETURN
                                                         FORALL-RESULT)))
                                                      (SETQ FORALL-RESULT
                                                              (AEVAL*
                                                               (LIST 'PLUS
                                                                     (AEVAL*
                                                                      (LIST
                                                                       'SUB
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'D 1)
                                                                        0)
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'DER
                                                                         1)
                                                                        0)
                                                                       (LIST
                                                                        'EQUAL
                                                                        (LIST
                                                                         'DER
                                                                         2)
                                                                        0)
                                                                       (LIST
                                                                        'TIMES
                                                                        (LIST P
                                                                              K
                                                                              L)
                                                                        (LIST
                                                                         'BOS
                                                                         (COND
                                                                          ((EQUAL
                                                                            WX
                                                                            1)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@B
                                                                             L)))
                                                                          ((EQUAL
                                                                            WX
                                                                            2)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@C
                                                                             L)))
                                                                          ((EQUAL
                                                                            WX
                                                                            3)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@A
                                                                             L))))
                                                                         0
                                                                         0))))
                                                                     FORALL-RESULT)))
                                                      (SETQ L
                                                              ((LAMBDA
                                                                   (
                                                                    FORALL-RESULT)
                                                                 (AEVAL*
                                                                  (LIST 'PLUS
                                                                        FORALL-RESULT
                                                                        1)))
                                                               L))
                                                      (GO LAB1))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL)))
                                 (PROG (K)
                                   (SETQ K 1)
                                  LAB
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE (AEVAL* AS) K))
                                     (RETURN NIL)))
                                   (PROGN
                                    (SETK (LIST 'BOS (LIST 'PART W K))
                                          (AEVAL* (LIST 'PART AS1 K)))
                                    (AEVAL* 'NIL))
                                   (SETQ K
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            K))
                                   (GO LAB))
                                 (SETK '@ELA
                                       (AEVAL*
                                        (LIST 'WHEREEXP (LIST 'LIST 'TRYK10)
                                              '@ALA)))
                                 (SETQ WOD
                                         (PROG (K FORALL-RESULT)
                                           (SETQ K 1)
                                           (SETQ FORALL-RESULT 0)
                                          LAB1
                                           (COND
                                            ((|AMINUSP:|
                                              (LIST 'DIFFERENCE (AEVAL* AS) K))
                                             (RETURN FORALL-RESULT)))
                                           (SETQ FORALL-RESULT
                                                   (AEVAL*
                                                    (LIST 'PLUS
                                                          (PROG (L
                                                                 FORALL-RESULT)
                                                            (SETQ L 1)
                                                            (SETQ FORALL-RESULT
                                                                    0)
                                                           LAB1
                                                            (COND
                                                             ((|AMINUSP:|
                                                               (LIST
                                                                'DIFFERENCE
                                                                (AEVAL* AS) L))
                                                              (RETURN
                                                               FORALL-RESULT)))
                                                            (SETQ FORALL-RESULT
                                                                    (AEVAL*
                                                                     (LIST
                                                                      'PLUS
                                                                      (AEVAL*
                                                                       (LIST
                                                                        'TIMES
                                                                        (LIST
                                                                         'BOS
                                                                         (COND
                                                                          ((EQUAL
                                                                            WX
                                                                            1)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@C
                                                                             K)))
                                                                          ((EQUAL
                                                                            WX
                                                                            2)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@A
                                                                             K)))
                                                                          ((EQUAL
                                                                            WX
                                                                            3)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@B
                                                                             K))))
                                                                         0 0)
                                                                        (LIST
                                                                         '@ELA
                                                                         K L)
                                                                        (LIST
                                                                         'BOS
                                                                         (COND
                                                                          ((EQUAL
                                                                            WX
                                                                            1)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@A
                                                                             L)))
                                                                          ((EQUAL
                                                                            WX
                                                                            2)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@B
                                                                             L)))
                                                                          ((EQUAL
                                                                            WX
                                                                            3)
                                                                           (AEVAL*
                                                                            (LIST
                                                                             '@C
                                                                             L))))
                                                                         0 0)))
                                                                      FORALL-RESULT)))
                                                            (SETQ L
                                                                    ((LAMBDA
                                                                         (
                                                                          FORALL-RESULT)
                                                                       (AEVAL*
                                                                        (LIST
                                                                         'PLUS
                                                                         FORALL-RESULT
                                                                         1)))
                                                                     L))
                                                            (GO LAB1))
                                                          FORALL-RESULT)))
                                           (SETQ K
                                                   ((LAMBDA (FORALL-RESULT)
                                                      (AEVAL*
                                                       (LIST 'PLUS
                                                             FORALL-RESULT 1)))
                                                    K))
                                           (GO LAB1)))
                                 (PROG (K)
                                   (SETQ K 1)
                                  LAB
                                   (COND
                                    ((|AMINUSP:|
                                      (LIST 'DIFFERENCE (AEVAL* AS) K))
                                     (RETURN NIL)))
                                   (AEVAL*
                                    (CLEAR
                                     (LIST (LIST '@A K) (LIST '@B K)
                                           (LIST '@C K)
                                           (LIST 'BOS (LIST 'PART W K)))))
                                   (SETQ K
                                           ((LAMBDA (FORALL-RESULT)
                                              (AEVAL*
                                               (LIST 'PLUS FORALL-RESULT 1)))
                                            K))
                                   (GO LAB))
                                 (RETURN
                                  (AEVAL*
                                   (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                                         (LIST 'EQUAL (LIST 'DER 1) 0)
                                         (LIST 'EQUAL (LIST 'DER 2) 0) WOD))))
                               FORALL-RESULT)))
                (SETQ WX (PLUS2 WX 1))
                (GO LAB1)))
      (RETURN (AEVAL WODA)))) 
(PUT 'JACOB1 'NUMBER-OF-ARGS 2) 
(FLAG '(JACOB1) 'OPFN) 
(PUT 'JACOB1 'DEFINED-ON-LINE '1119) 
(PUT 'JACOB1 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'JACOB1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE JACOB1 (P W)
    (PROG (ALA ELA WOD WOD1 WOD2 WODX EWA)
      (SETQ ALA (AEVAL (LIST 'GAT1 P)))
      (SETK (LIST 'BOS W)
            (AEVAL
             (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                   (LIST 'EQUAL (LIST 'DER 1) 0) (LIST 'EQUAL (LIST 'DER 2) 0)
                   (LIST 'TIMES P (LIST 'BOS '|#B| 0 0)))))
      (AEVAL (LET '(TRYK10)))
      (SETQ ELA (AEVAL ALA))
      (AEVAL (CLEARRULES (LIST 'TRYK10)))
      (SETQ WOD
              (AEVAL
               (LIST 'TIMES (LIST 'BOS '|#C| 0 0) ELA (LIST 'BOS '|#A| 0 0))))
      (SETQ WOD1
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL (LIST 'D 1) 0)
                     (LIST 'EQUAL (LIST 'DER 1) 0)
                     (LIST 'EQUAL (LIST 'DER 2) 0) WOD)))
      (SETQ WODX
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL '|#A| '|#AA|) (LIST 'EQUAL '|#B| '|#BB|)
                     (LIST 'EQUAL '|#C| '|#CC|) WOD1)))
      (SETQ WOD2
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL '|#AA| '|#B|) (LIST 'EQUAL '|#BB| '|#C|)
                     (LIST 'EQUAL '|#CC| '|#A|) WODX)))
      (SETK 'WOD3
            (AEVAL
             (LIST 'SUB (LIST 'EQUAL '|#AA| '|#C|) (LIST 'EQUAL '|#BB| '|#A|)
                   (LIST 'EQUAL '|#CC| '|#B|) WODX)))
      (AEVAL (CLEAR (LIST (LIST 'BOS W))))
      (RETURN (AEVAL (LIST 'PLUS WOD1 WOD2 'WOD3))))) 
(PUT 'MACIERZ 'NUMBER-OF-ARGS 3) 
(FLAG '(MACIERZ) 'OPFN) 
(PUT 'MACIERZ 'DEFINED-ON-LINE '1136) 
(PUT 'MACIERZ 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'MACIERZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MACIERZ (WX XX YY)
    (PROG (EWA OLA EW1 EW2)
      (AEVAL (MATRIX (LIST (LIST '@Z_Z_X 4 4))))
      (SETQ EWA
              (COND
               ((EQ (REVALX XX) (REVALX 'F))
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) 0)
                       (LIST 'EQUAL (LIST 'DER 2) 0)
                       (LIST 'EQUAL (LIST 'DER 3) 0)
                       (LIST 'EQUAL (LIST 'D 1) 0)
                       (LIST 'TIMES WX (LIST 'FER '|#Z_Z| 0 0)))))
               ((EQ (REVALX XX) (REVALX 'B))
                (AEVAL
                 (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) 0)
                       (LIST 'EQUAL (LIST 'DER 2) 0)
                       (LIST 'EQUAL (LIST 'DER 3) 0)
                       (LIST 'EQUAL (LIST 'D 1) 0)
                       (LIST 'TIMES WX (LIST 'BOS '|#Z_Z| 0 0)))))
               (T
                (AEVAL
                 (REDERR
                  (REVALX
                   "wrong value of second  argument which should be b or f"))))))
      (SETQ OLA
              (COND ((EQ (REVALX YY) (REVALX 'B)) (AEVAL (LIST 'BPART EWA)))
                    ((EQ (REVALX YY) (REVALX 'F)) (AEVAL (LIST 'FPART EWA)))
                    (T
                     (AEVAL
                      (REDERR
                       (REVALX
                        "wrong value of third  argument which should be b or f"))))))
      (SETQ EW1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'REPLACEBY
                                       (LIST 'FUN '|#Z_Z0| (LIST '~ 'N))
                                       (LIST 'BER 1 'N))
                                 (LIST 'REPLACEBY
                                       (LIST 'FUN '|#Z_Z1| (LIST '~ 'N))
                                       (LIST 'BER 2 'N))
                                 (LIST 'REPLACEBY
                                       (LIST 'GRAS '|#Z_Z#Z_Z1| (LIST '~ 'N))
                                       (LIST 'FIR 1 'N))
                                 (LIST 'REPLACEBY
                                       (LIST 'GRAS '|#Z_Z#Z_Z2| (LIST '~ 'N))
                                       (LIST 'FIR 2 'N))))
                     OLA)))
      (SETQ EW2
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'REPLACEBY (LIST 'BER 1 (LIST '~ 'N))
                                       (LIST 'TIMES '|#S_S|
                                             (LIST 'EXPT (LIST 'D 1) 'N)))
                                 (LIST 'REPLACEBY (LIST 'BER 2 (LIST '~ 'N))
                                       (LIST 'TIMES (LIST 'EXPT '|#S_S| 4)
                                             (LIST 'EXPT (LIST 'D 1) 'N)))
                                 (LIST 'REPLACEBY (LIST 'FIR 1 (LIST '~ 'N))
                                       (LIST 'TIMES (LIST 'EXPT '|#S_S| 2)
                                             (LIST 'EXPT (LIST 'D 1) 'N)))
                                 (LIST 'REPLACEBY (LIST 'FIR 2 (LIST '~ 'N))
                                       (LIST 'TIMES (LIST 'EXPT '|#S_S| 3)
                                             (LIST 'EXPT (LIST 'D 1) 'N)))))
                     EW1)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 K)) (RETURN NIL)))
        (PROG (L)
          (SETQ L 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 4 L)) (RETURN NIL)))
          (SETK (LIST '@Z_Z_X K L)
                (AEVAL*
                 (LIST 'CHAN (LIST 'COEFFN (LIST 'PART EW2 K) '|#S_S| L))))
          (SETQ L (PLUS2 L 1))
          (GO LAB))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (RETURN (AEVAL '@Z_Z_X)))) 
(PUT 'DOT_HAM 'NUMBER-OF-ARGS 2) 
(FLAG '(DOT_HAM) 'OPFN) 
(PUT 'DOT_HAM 'DEFINED-ON-LINE '1156) 
(PUT 'DOT_HAM 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'DOT_HAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DOT_HAM (WW MM)
    (PROG (EWA OLA ALA AS)
      (SETQ AS (AEVAL (LIST 'LENGTH WW)))
      (SETQ EWA
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'D 'T) MM)
                     (LIST 'TIMES MM (LIST 'D 'T)))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K)) (RETURN NIL)))
        (SETK (LIST 'BOS (LIST 'PART (LIST 'PART WW K) 1) 'T)
              (AEVAL* (LIST 'PART (LIST 'PART WW K) 2)))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (SETQ OLA (AEVAL (LIST 'WHEREEXP (LIST 'LIST 'TRYK14) EWA)))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* AS) K)) (RETURN NIL)))
        (AEVAL* (CLEAR (LIST (LIST 'BOS (LIST 'PART (LIST 'PART WW K) 1) 'T))))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN (AEVAL OLA)))) 
(LET
 (LIST
  (LIST 'LIST
        (LIST 'REPLACEBY (LIST 'WAGA (LIST '~ 'K) (LIST '~ 'S) (LIST '~ 'M))
              (LIST 'PLUS (LIST 'TIMES 2 'M) (LIST 'DELTA 'K 'S)
                    (LIST 'TIMES 2 (LIST 'DELTA 3 'S))
                    (LIST 'TIMES (LIST 'DELTA (LIST 'DIFFERENCE 3 'K) 'S)
                          (LIST 'COND
                                (LIST (LIST 'EVALGREATERP (LIST 'AEVAL ''M) 0)
                                      1)
                                (LIST 'T 0)))))
        (LIST 'REPLACEBY (LIST 'S_S (LIST '~ 'F) (LIST '~ 'N)) 1)
        (LIST 'REPLACEBY (LIST 'S_S 1 (LIST '~ 'F) (LIST '~ 'N)) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL (MINUS 1))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL (MINUS 2))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 3) (LIST 'DEL (MINUS 3))) 1)
        (LIST 'REPLACEBY (LIST 'DEL 0) (LIST 'D (MINUS 3)))
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 1) (LIST 'DEL (MINUS 3))) 1)
        (LIST 'REPLACEBY (LIST 'TIMES (LIST 'DER 2) (LIST 'DEL (MINUS 3)))
              (LIST 'DEL (MINUS 3)))
        (LIST 'REPLACEBY
              (LIST 'TIMES (LIST 'DER 1) (LIST 'DER 2) (LIST 'DEL (MINUS 3)))
              1)))) 
(SETK 'DRYK
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F) (LIST '~ 'N))
                         (LIST 'FER (LIST '~ 'G) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'FER 'G 'K 'M)
                         (LIST 'D 'F (LIST 'PLUS 'N (LIST 'DELTA 'F 'G)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F) (LIST '~ 'N))
                         (LIST 'BOS (LIST '~ 'G) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'BOS 'G 'K 'M)
                         (LIST 'D 'F (LIST 'PLUS 'N (LIST 'DELTA 'F 'G)))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F) (LIST '~ 'N))
                         (LIST 'ZAN (LIST '~ 'G) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'ZAN 'G 'K 'M)
                         (LIST 'D 'F (LIST 'PLUS 'N 1))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'D (LIST '~ 'F) (LIST '~ 'N))
                         (LIST 'ZEN (LIST '~ 'G) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'TIMES (LIST 'ZEN 'G 'K 'M)
                         (LIST 'D 'F (LIST 'PLUS 'N 1))))))) 
(SETK 'WARIAT_0
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N) (LIST 'FER 'F 'K 0)
                         (LIST 'EXPT (LIST 'D 1) 'N)))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N) (LIST 'BOS 'F 'K 0)
                         (LIST 'EXPT (LIST 'D 1) 'N)))))) 
(SETK 'WARIAT_1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                            (LIST 'LIST ''PLUS ''N 1))
                                      (LIST 'LIST ''BOS ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 1) 0)
                                      (LIST 'LIST ''DER 1)
                                      (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                            ''N))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''N)
                                           (LIST 'LIST ''FER ''F ''K 0)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''N))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''N)
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 1) 0)
                                      (LIST 'LIST ''DER 1)
                                      (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                            ''N))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''N)
                                           (LIST 'LIST ''BOS ''F ''K 0)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''N))))))))) 
(SETK 'WARIAT_2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1)
                                                  ''N))
                                      (LIST 'LIST ''BOS ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 2) 0)
                                      (LIST 'LIST ''DER 2)
                                      (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                            ''N))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''N)
                                           (LIST 'LIST ''FER ''F ''K 0)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''N))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                            (LIST 'LIST ''PLUS ''K ''N))
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 2) 0)
                                      (LIST 'LIST ''DER 2)
                                      (LIST 'LIST ''EXPT (LIST 'LIST ''D 1)
                                            ''N))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 ''N)
                                           (LIST 'LIST ''BOS ''F ''K 0)
                                           (LIST 'LIST ''EXPT
                                                 (LIST 'LIST ''D 1) ''N))))))))) 
(SETK 'WARIAT_3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       1))
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''MINUS
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''FER
                                                                   ''F 0 0)
                                                             (LIST 'LIST ''EXPT
                                                                   (LIST 'LIST
                                                                         ''D 1)
                                                                   ''N)))
                                                 (LIST 'LIST ''TIMES ''N
                                                       (LIST 'LIST ''D 1)
                                                       (LIST 'LIST ''FER ''F 0
                                                             0)
                                                       (LIST 'LIST ''EXPT
                                                             (LIST 'LIST ''D 1)
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   ''N 1)))))))
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''FER ''F 0 ''N)))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'FER 'F 1 0)
                                     (LIST 'EXPT (LIST 'D 1) 'N))
                               (LIST 'TIMES 'N (LIST 'D 1) (LIST 'BOS 'F 0 0)
                                     (LIST 'DER 1)
                                     (LIST 'EXPT (LIST 'D 1)
                                           (LIST 'DIFFERENCE 'N 1))))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                         (LIST 'PLUS
                               (LIST 'TIMES (LIST 'FER 'F 2 0)
                                     (LIST 'EXPT (LIST 'D 1) 'N))
                               (LIST 'TIMES 'N (LIST 'D 1) (LIST 'BOS 'F 0 0)
                                     (LIST 'DER 2)
                                     (LIST 'EXPT (LIST 'D 1)
                                           (LIST 'DIFFERENCE 'N 1))))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'ZEN 'F 0 'N) (LIST 'DER 1)
                                     (LIST 'DER 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'ZAN 'F 1 'N) (LIST 'DER 2))
                               (LIST 'TIMES (LIST 'ZAN 'F 2 'N)
                                     (LIST 'DER 1)))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       1))
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''MINUS
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''BOS
                                                                   ''F 0 0)
                                                             (LIST 'LIST ''EXPT
                                                                   (LIST 'LIST
                                                                         ''D 1)
                                                                   ''N)))
                                                 (LIST 'LIST ''TIMES ''N
                                                       (LIST 'LIST ''D 1)
                                                       (LIST 'LIST ''BOS ''F 0
                                                             0)
                                                       (LIST 'LIST ''EXPT
                                                             (LIST 'LIST ''D 1)
                                                             (LIST 'LIST
                                                                   ''DIFFERENCE
                                                                   ''N 1)))))))
                         (LIST 'T (LIST 'AEVAL (LIST 'LIST ''BOS ''F 0 ''N)))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'BOS 'F 1 0)
                                     (LIST 'EXPT (LIST 'D 1) 'N))
                               (LIST 'TIMES 'N (LIST 'D 1) (LIST 'FER 'F 0 0)
                                     (LIST 'DER 1)
                                     (LIST 'EXPT (LIST 'D 1)
                                           (LIST 'DIFFERENCE 'N 1))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) 'N)
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'BOS 'F 2 0)
                                     (LIST 'EXPT (LIST 'D 1) 'N))
                               (LIST 'TIMES 'N (LIST 'D 1) (LIST 'FER 'F 0 0)
                                     (LIST 'DER 2)
                                     (LIST 'EXPT (LIST 'D 1)
                                           (LIST 'DIFFERENCE 'N 1))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'ZAN 'F 0 'N)
                                           (LIST 'DER 1) (LIST 'DER 2)))
                               (LIST 'TIMES (LIST 'ZEN 'F 1 'N) (LIST 'DER 2)))
                         (LIST 'TIMES (LIST 'ZEN 'F 2 'N) (LIST 'DER 1))))))) 
(SETK 'SZUKAJ0
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'N) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''N)
                                (LIST 'AEVAL (LIST 'LIST ''TIMES 2 ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F ''K ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''TIMES 2 ''M) ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F ''K ''M)
                                           (LIST 'LIST ''BYK ''N ''G))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'N) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''N)
                                (LIST 'AEVAL (LIST 'LIST ''TIMES 2 ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''BOS ''F ''K ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''TIMES 2 ''M) ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F ''K ''M)
                                           (LIST 'LIST ''BYK ''N ''G))))))))) 
(SETK 'SZUKAJ1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL (LIST 'LIST ''WAGA 1 ''N ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F ''N ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''WAGA 1 ''N ''M)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F ''N ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL (LIST 'LIST ''WAGA 1 ''N ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''BOS ''F ''N ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''WAGA 1 ''N ''M)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F ''N ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))))) 
(SETK 'SZUKAJ2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL (LIST 'LIST ''WAGA 2 ''N ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F ''N ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''WAGA 2 ''N ''M)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F ''N ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL (LIST 'LIST ''WAGA 2 ''N ''M)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''BOS ''F ''N ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''WAGA 2 ''N ''M)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F ''N ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))))) 
(SETK 'SZUKAJ3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) 0 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F 0 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 2
                                                              ''M)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''FER ''F 0
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''FER
                                                                   ''F 0 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F 1 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              1)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''FER ''F 1
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''FER
                                                                   ''F 1 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F 2 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              1)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''FER ''F 2
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''FER
                                                                   ''F 2 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''PLUS
                                            (LIST 'LIST ''TIMES 2 ''M) 2)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F 3 ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M) 2)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''FER ''F 3 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F 0 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 2
                                                              ''M)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''BOS ''F 0
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''BOS
                                                                   ''F 0 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F 1 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              1)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''BOS ''F 1
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''BOS
                                                                   ''F 1 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F 2 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              1)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''BOS ''F 2
                                                              ''M)
                                                        (LIST 'LIST ''BYK
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''BOS
                                                                   ''F 2 ''M)
                                                             (LIST 'LIST ''BYK
                                                                   ''K
                                                                   ''G)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BYK (LIST '~ 'K) (LIST '~ 'G))
                         (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M)))
                   (LIST 'COND
                         (LIST
                          (LIST 'EVALLEQ (LIST 'AEVAL ''K)
                                (LIST 'AEVAL
                                      (LIST 'LIST ''PLUS
                                            (LIST 'LIST ''TIMES 2 ''M) 2)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''BOS ''F 3 ''M)
                                      (LIST 'LIST ''BYK
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M) 2)
                                            ''F))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''BOS ''F 3 ''M)
                                           (LIST 'LIST ''BYK ''K ''G))))))))) 
(SETK 'POSZUKAJ0
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL (LIST 'LIST ''TIMES 2 ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''R_R
                                            (LIST 'LIST ''TIMES 2 ''M) ''F)
                                      (LIST 'LIST ''ZEN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F ''S ''M))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL (LIST 'LIST ''TIMES 2 ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''R_R
                                            (LIST 'LIST ''TIMES 2 ''M) ''F)
                                      (LIST 'LIST ''ZAN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F ''S
                                                 ''M))))))))) 
(SETK 'POSZUKAJ1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''WAGA 1 ''S ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''R_R ''K ''F)
                                      (LIST 'LIST ''ZEN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F ''S ''M))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''WAGA 1 ''S ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''R_R ''K ''F)
                                      (LIST 'LIST ''ZAN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F ''S
                                                 ''M))))))))) 
(SETK 'POSZUKAJ2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'FER (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''WAGA 2 ''S ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''R_R ''K ''F)
                                      (LIST 'LIST ''ZEN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F ''S ''M))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES
                         (LIST 'BOS (LIST '~ 'F) (LIST '~ 'S) (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''WAGA 2 ''S ''M)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''G)
                                      (LIST 'AEVAL ''F)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES (LIST 'LIST ''R_R ''K ''F)
                                      (LIST 'LIST ''ZAN ''F ''S ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F ''S
                                                 ''M))))))))) 
(SETK 'POSZUKAJ3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 0 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F 0 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)))
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''F)
                                                        (LIST 'AEVAL ''G)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              ''F)
                                                        (LIST 'LIST ''ZEN ''F 0
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''FER
                                                                   ''F 0
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F 1 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)))
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''F)
                                                        (LIST 'AEVAL ''G)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F)
                                                        (LIST 'LIST ''ZEN ''F 1
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''FER
                                                                   ''F 1
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F 2 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)))
                                                  (LIST 'BOOLVALUE*
                                                        (LIST 'REVALX
                                                              (LIST 'LIST ''F
                                                                    (LIST 'LIST
                                                                          ''E
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUAL
                                                                           ''G))))))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F)
                                                        (LIST 'LIST ''ZEN ''F 2
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''FER
                                                                   ''F 2
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'FER (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M)
                                                  2)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''F)
                                      (LIST 'AEVAL ''G)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''R_R 1
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M) 2)
                                            ''F)
                                      (LIST 'LIST ''ZEN ''F 3 ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''FER ''F 3 ''M))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 0 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 2)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F 0 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)))
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''F)
                                                        (LIST 'AEVAL ''G)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''TIMES 2
                                                                    ''M)
                                                              ''F)
                                                        (LIST 'LIST ''ZAN ''F 0
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''BOS
                                                                   ''F 0
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 1 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F 1 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)))
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''F)
                                                        (LIST 'AEVAL ''G)))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F)
                                                        (LIST 'LIST ''ZAN ''F 1
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''BOS
                                                                   ''F 1
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 2 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''M) 1)
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F 2 ''M))))
                         (LIST 'T
                               (LIST 'PROGN
                                     (LIST 'COND
                                           (LIST
                                            (LIST 'AND
                                                  (LIST 'EVALEQUAL
                                                        (LIST 'AEVAL ''K)
                                                        (LIST 'AEVAL
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)))
                                                  (LIST 'BOOLVALUE*
                                                        (LIST 'REVALX
                                                              (LIST 'LIST ''F
                                                                    (LIST 'LIST
                                                                          ''E
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUAL
                                                                           ''G))))))
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES
                                                        (LIST 'LIST ''R_R 1
                                                              (LIST 'LIST
                                                                    ''PLUS
                                                                    (LIST 'LIST
                                                                          ''TIMES
                                                                          2
                                                                          ''M)
                                                                    1)
                                                              ''F)
                                                        (LIST 'LIST ''ZAN ''F 2
                                                              ''M))))
                                           (LIST 'T
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES
                                                             (LIST 'LIST ''R_R
                                                                   ''K ''G)
                                                             (LIST 'LIST ''BOS
                                                                   ''F 2
                                                                   ''M)))))))))
             (LIST 'REPLACEBY
                   (LIST 'TIMES (LIST 'BOS (LIST '~ 'F) 3 (LIST '~ 'M))
                         (LIST 'R_R (LIST '~ 'K) (LIST '~ 'G)))
                   (LIST 'COND
                         (LIST
                          (LIST 'AND
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K)
                                      (LIST 'AEVAL
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M)
                                                  2)))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''F)
                                      (LIST 'AEVAL ''G)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''R_R 1
                                            (LIST 'LIST ''PLUS
                                                  (LIST 'LIST ''TIMES 2 ''M) 2)
                                            ''F)
                                      (LIST 'LIST ''ZAN ''F 3 ''M))))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''TIMES
                                           (LIST 'LIST ''R_R ''K ''G)
                                           (LIST 'LIST ''BOS ''F 3 ''M))))))))) 
(SETK 'CALKUJ0
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'FER 'F 'N (LIST 'DIFFERENCE 'M 1)))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'BOS 'F 'N (LIST 'DIFFERENCE 'M 1)))))) 
(SETK 'POCALKUJ0
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'FER 'F 'N (LIST 'DIFFERENCE 'M 1))
                               (LIST 'D 1))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'N) (LIST '~ 'M))
                   (LIST 'MINUS
                         (LIST 'TIMES (LIST 'BOS 'F 'N (LIST 'DIFFERENCE 'M 1))
                               (LIST 'D 1))))))) 
(SETK 'CALKUJ1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''BOS ''F
                                      (LIST 'LIST ''DIFFERENCE ''K 1) ''N)))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''BOS ''F (LIST 'LIST ''PLUS ''K 1)
                                      (LIST 'LIST ''DIFFERENCE ''N 1))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''FER ''F
                                      (LIST 'LIST ''DIFFERENCE ''K 1) ''N)))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''FER ''F (LIST 'LIST ''PLUS ''K 1)
                                      (LIST 'LIST ''DIFFERENCE ''N 1))))))))) 
(SETK 'POCALKUJ1
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''BOS ''F
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        1)
                                                  ''N)
                                            (LIST 'LIST ''DER 1)))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''BOS ''F
                                                  (LIST 'LIST ''PLUS ''K 1)
                                                  (LIST 'LIST ''DIFFERENCE ''N
                                                        1))
                                            (LIST 'LIST ''DER 1)))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 1)
                                            ''N)
                                      (LIST 'LIST ''DER 1))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''PLUS ''K 1)
                                            (LIST 'LIST ''DIFFERENCE ''N 1))
                                      (LIST 'LIST ''DER 1))))))))) 
(SETK 'CALKUJ2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''BOS ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 2)
                                            ''N))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''BOS ''F
                                            (LIST 'LIST ''PLUS ''K 2)
                                            (LIST 'LIST ''DIFFERENCE ''N
                                                  1)))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 2)
                                            ''N))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''PLUS ''K 2)
                                            (LIST 'LIST ''DIFFERENCE ''N
                                                  1)))))))))) 
(SETK 'POCALKUJ2
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ZEN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''K)
                                            (LIST 'LIST ''BOS ''F
                                                  (LIST 'LIST ''DIFFERENCE ''K
                                                        2)
                                                  ''N)
                                            (LIST 'LIST ''DER 2)))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''MINUS
                                      (LIST 'LIST ''TIMES
                                            (LIST 'LIST ''EXPT (LIST 'MINUS 1)
                                                  ''K)
                                            (LIST 'LIST ''BOS ''F
                                                  (LIST 'LIST ''PLUS ''K 2)
                                                  (LIST 'LIST ''DIFFERENCE ''N
                                                        1))
                                            (LIST 'LIST ''DER 2)))))))
             (LIST 'REPLACEBY
                   (LIST 'ZAN (LIST '~ 'F) (LIST '~ 'K) (LIST '~ 'N))
                   (LIST 'COND
                         (LIST
                          (LIST 'OR (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 3)
                                (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 2))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''DIFFERENCE ''K 2)
                                            ''N)
                                      (LIST 'LIST ''DER 2))))
                         (LIST
                          (LIST 'OR
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 0)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N) 0))
                                (LIST 'AND
                                      (LIST 'EVALEQUAL (LIST 'AEVAL ''K) 1)
                                      (LIST 'EVALGREATERP (LIST 'AEVAL ''N)
                                            0)))
                          (LIST 'AEVAL
                                (LIST 'LIST ''TIMES
                                      (LIST 'LIST ''EXPT (LIST 'MINUS 1) ''K)
                                      (LIST 'LIST ''FER ''F
                                            (LIST 'LIST ''PLUS ''K 2)
                                            (LIST 'LIST ''DIFFERENCE ''N 1))
                                      (LIST 'LIST ''DER 2))))))))) 
(SETK 'CALKUJ3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 0 ''N)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''FER ''F 3
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 1 0)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''FER ''F 2
                                           (LIST 'LIST ''DIFFERENCE ''N 1))))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 2 0)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''FER ''F 1
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       1)))))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'FER 'F 0 'N))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 2)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 0 ''N)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''BOS ''F 3
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       2)))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 1 0)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''BOS ''F 2
                                           (LIST 'LIST ''DIFFERENCE ''N 1))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'COND
                         (LIST (LIST 'EVALLESSP (LIST 'AEVAL ''N) 1)
                               (LIST 'AEVAL (LIST 'LIST ''FER ''F 2 0)))
                         (LIST 'T
                               (LIST 'AEVAL
                                     (LIST 'LIST ''MINUS
                                           (LIST 'LIST ''BOS ''F 1
                                                 (LIST 'LIST ''DIFFERENCE ''N
                                                       1)))))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'BOS 'F 0 'N))))) 
(SETK 'POCALKUJ3
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES
                                           (LIST 'BOS 'F 2
                                                 (LIST 'DIFFERENCE 'N 1))
                                           (LIST 'DER 2)))
                               (LIST 'TIMES
                                     (LIST 'BOS 'F 1 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'DER 1)))
                         (LIST 'TIMES (LIST 'FER 'F 3 (LIST 'DIFFERENCE 'N 2))
                               (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES
                                     (LIST 'BOS 'F 3 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'DER 2))
                               (LIST 'TIMES (LIST 'BOS 'F 0 'N) (LIST 'DER 1)))
                         (LIST 'TIMES (LIST 'FER 'F 2 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'BOS 'F 0 'N)
                                           (LIST 'DER 2)))
                               (LIST 'TIMES
                                     (LIST 'BOS 'F 3 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'DER 1)))
                         (LIST 'TIMES (LIST 'FER 'F 1 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'ZEN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'FER 'F 0 'N) (LIST 'DER 1)
                                     (LIST 'DER 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'BOS 'F 1 'N) (LIST 'DER 2))
                               (LIST 'TIMES (LIST 'BOS 'F 2 'N)
                                     (LIST 'DER 1)))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 0 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'BOS 'F 3 (LIST 'DIFFERENCE 'N 2))
                               (LIST 'DER 1) (LIST 'DER 2))
                         (LIST 'TIMES (LIST 'FER 'F 2 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 2))
                         (LIST 'TIMES (LIST 'FER 'F 1 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 1))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 1 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'MINUS
                               (LIST 'TIMES
                                     (LIST 'FER 'F 3 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'DER 2)))
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'FER 'F 0 'N) (LIST 'DER 1))
                               (LIST 'TIMES
                                     (LIST 'BOS 'F 2 (LIST 'DIFFERENCE 'N 1))
                                     (LIST 'DER 1) (LIST 'DER 2)))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 2 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'TIMES (LIST 'FER 'F 3 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 1))
                         (LIST 'TIMES (LIST 'FER 'F 0 'N) (LIST 'DER 2))
                         (LIST 'TIMES (LIST 'BOS 'F 1 (LIST 'DIFFERENCE 'N 1))
                               (LIST 'DER 1) (LIST 'DER 2))))
             (LIST 'REPLACEBY (LIST 'ZAN (LIST '~ 'F) 3 (LIST '~ 'N))
                   (LIST 'PLUS
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'BOS 'F 0 'N)
                                           (LIST 'DER 1) (LIST 'DER 2)))
                               (LIST 'TIMES (LIST 'FER 'F 1 'N) (LIST 'DER 2)))
                         (LIST 'TIMES (LIST 'FER 'F 2 'N) (LIST 'DER 1))))))) 
(PUT 'MAXI 'NUMBER-OF-ARGS 2) 
(FLAG '(MAXI) 'OPFN) 
(PUT 'MAXI 'DEFINED-ON-LINE '1402) 
(PUT 'MAXI 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'MAXI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXI (WRT WX)
    (PROG (KR KR1 EW1 EW2 EW3 EW4 EW5)
      (COND ((EVALEQUAL (AEVAL WX) 0) (RETURN (AEVAL (LIST 'LIST 0 0)))))
      (SETQ KR (AEVAL (LIST 'NUM WX)))
      (SETQ KR1 (AEVAL (LIST 'DEN WX)))
      (SETQ EW1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (SETK 'HELP* (AEVAL (LIST 'MKID 'SZUKAJ WRT))))
                     (LIST 'TIMES (LIST 'BYK 0 0) KR))))
      (SETQ EW2
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'REPLACEBY
                                       (LIST 'BYK (LIST '~ 'N) (LIST '~ 'F))
                                       (LIST 'TIMES (LIST 'EXPT 'L_A_@M 'N)
                                             (LIST 'P_P 'N 'F)))))
                     EW1)))
      (SETQ EW2
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL 'P_P 'R_R) (LIST 'LCOF EW2 'L_A_@M))))
      (SETQ EW2
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (SETK 'HELP* (AEVAL (LIST 'MKID 'POSZUKAJ WRT))))
                     EW2)))
      (SETQ EW3 (AEVAL (LIST 'SUB (LIST 'EQUAL 'R_R 'S_S) EW2)))
      (SETQ EW1
              (COND
               ((EVALEQUAL (AEVAL (LIST 'PART EW3 0)) (AEVAL 'MINUS))
                (MINUS 1))
               (T 1)))
      (SETQ EW4
              (AEVAL
               (LIST 'SUB (LIST 'EQUAL 'X_X 0)
                     (COND
                      ((EVALLESSP (AEVAL (LIST 'LENGTH (LIST 'TIMES EW1 EW3)))
                                  (AEVAL
                                   (LIST 'ARGLENGTH (LIST 'TIMES EW1 EW3))))
                       (AEVAL (LIST 'TIMES EW1 EW3)))
                      (T
                       (AEVAL
                        (LIST 'PART (LIST 'PLUS (LIST 'TIMES EW1 EW3) 'X_X)
                              1)))))))
      (SETQ EW5
              (AEVAL
               (LIST 'DIFFERENCE KR
                     (LIST 'SUB (LIST 'EQUAL 'ZEN 'FER) (LIST 'EQUAL 'ZAN 'BOS)
                           (LIST 'TIMES EW1 EW4)))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'TIMES EW1 (LIST 'QUOTIENT EW4 KR1))
              (LIST 'QUOTIENT EW5 KR1)))))) 
(PUT 'N_DYW 'NUMBER-OF-ARGS 3) 
(FLAG '(N_DYW) 'OPFN) 
(PUT 'N_DYW 'DEFINED-ON-LINE '1419) 
(PUT 'N_DYW 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'N_DYW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_DYW (WRT WX WZ)
    (PROG (EKS0 EKS EKS1 EKS2 OSA1 OSA2 OSA3 OSA4)
      (SETK 'KAP (AEVAL (LIST 'LENGTH WZ)))
      (SETQ EKS (AEVAL (LIST 'NUM WX)))
      (SETQ EKS0 (AEVAL (LIST 'DEN WX)))
      (SETQ EKS1
              (COND
               ((EVALEQUAL (AEVAL (LIST 'PART EKS 0)) (AEVAL 'MINUS))
                (MINUS 1))
               (T 1)))
      (SETQ EKS2 (AEVAL (LIST 'TIMES EKS1 EKS)))
      (SETQ OSA4 (AEVAL 0))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* 'KAP) K)) (RETURN NIL)))
        (PROGN
         (SETQ OSA1
                 (AEVAL*
                  (LIST 'DIFFERENCE EKS2
                        (LIST 'SUB (LIST 'EQUAL (LIST 'PART WZ K) 0) EKS2))))
         (SETQ EKS2 (AEVAL* (LIST 'DIFFERENCE EKS2 OSA1)))
         (SETQ OSA2
                 (AEVAL*
                  (LIST 'SUB (LIST 'EQUAL (LIST 'D (LIST 'PART WZ K)) 0)
                        (LIST 'WHEREEXP (LIST 'LIST 'TRYK)
                              (LIST 'TIMES (LIST 'D (LIST 'PART WZ K))
                                    OSA1)))))
         (SETQ OSA3
                 (AEVAL*
                  (LIST 'WHEREEXP (LIST 'LIST 'DRYK)
                        (LIST 'TIMES (LIST 'D (LIST 'PART WZ K) 0) OSA2))))
         (SETQ OSA3
                 (AEVAL*
                  (LIST 'WHEREEXP
                        (LIST 'LIST
                              (LIST 'LIST
                                    (LIST 'REPLACEBY
                                          (LIST 'D (LIST '~ 'F) (LIST '~ 'N))
                                          (LIST 'WHEN (LIST 'QUOTIENT 1 'N)
                                                (LIST 'GREATERP 'N 0)))))
                        OSA3)))
         (SETQ OSA3
                 (AEVAL*
                  (LIST 'SUB (LIST 'EQUAL (LIST 'DER 1) 0)
                        (LIST 'EQUAL (LIST 'D 1) 0)
                        (LIST 'EQUAL (LIST 'DER 3) 0)
                        (LIST 'EQUAL (LIST 'DER 2) 0)
                        (LIST 'WHEREEXP
                              (LIST 'LIST
                                    (SETK 'HELP*
                                          (AEVAL* (LIST 'MKID 'WARIAT_ WRT))))
                              OSA3))))
         (SETQ OSA4 (AEVAL* (LIST 'PLUS OSA4 OSA3)))
         (AEVAL* 'NIL))
        (SETQ K
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 K))
        (GO LAB))
      (RETURN
       (AEVAL
        (LIST 'LIST
              (LIST 'DIFFERENCE WX
                    (LIST 'TIMES EKS1 (LIST 'QUOTIENT OSA4 EKS0)))
              (LIST 'TIMES EKS1 (LIST 'QUOTIENT OSA4 EKS0))))))) 
(PUT 'S_INT 'NUMBER-OF-ARGS 3) 
(FLAG '(S_INT) 'OPFN) 
(PUT 'S_INT 'DEFINED-ON-LINE '1443) 
(PUT 'S_INT 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'S_INT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE S_INT (WRT WX WZ)
    (PROG (KAK KAK1 KAK2 PAK PAK1 PAK2 KAP PAK3 PAK4)
      (COND ((EVALEQUAL (AEVAL WX) 0) (RETURN 0)))
      (COND
       ((OR
         (AND (EVALEQUAL (AEVAL 'ABRA_KADABRA) 1) (EVALGREATERP (AEVAL WRT) 0))
         (AND (EVALEQUAL (AEVAL 'ABRA_KADABRA) 3)
              (EVALGREATERP (AEVAL WRT) 0)))
        (AEVAL
         (REDERR
          (REVALX " **** I T is Impossible to define in a proper
manner this integral => use trad representation for computation only")))))
      (SETQ KAK (AEVAL (LIST 'N_DYW WRT WX WZ)))
      (SETQ KAK1 (AEVAL (LIST 'FIRST KAK)))
      (SETQ KAK2 (AEVAL (LIST 'SECOND KAK)))
      (SETQ PAK (AEVAL (LIST 'HOM KAK1)))
      (SETQ PAK1 (AEVAL (LIST 'FIRST (LIST 'FIRST PAK))))
      (SETQ PAK2 (AEVAL (LIST 'SECOND PAK)))
      (SETQ KAP (AEVAL (LIST 'LENGTH PAK2)))
      (SETQ PAK3
              (COND ((EVALEQUAL (AEVAL PAK2) 0) 0)
                    (T
                     (PROG (K FORALL-RESULT)
                       (SETQ K 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* KAP) K))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'CAL WRT (LIST 'PART PAK2 K)))
                                      FORALL-RESULT)))
                       (SETQ K
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                K))
                       (GO LAB1)))))
      (SETQ PAK4
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES PAK1 PAK3)
                     (LIST 'TIMES (LIST 'DEL (LIST 'MINUS WRT)) KAK2))))
      (RETURN (AEVAL PAK4)))) 
(PUT 'CAL 'NUMBER-OF-ARGS 2) 
(FLAG '(CAL) 'OPFN) 
(PUT 'CAL 'DEFINED-ON-LINE '1460) 
(PUT 'CAL 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'CAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CAL (WRT WX)
    (PROG (WEM WEM1 WEM2 WEM3 WEM4 WEM5 WEM6 Z_Z_Z)
      (COND ((EVALEQUAL (AEVAL WX) 0) (RETURN 0)))
      (SETQ WEM (AEVAL (LIST 'MAXI WRT WX)))
      (SETQ WEM1 (AEVAL (LIST 'FIRST WEM)))
      (SETQ WEM2 (AEVAL (LIST 'SECOND WEM)))
      (SETQ Z_Z_Z (AEVAL 0))
      (WHILE (EVALNEQ (AEVAL* WEM1) 0)
             (PROGN
              (SETQ WEM3
                      (AEVAL*
                       (LIST 'SUB (LIST 'EQUAL 'ZEN 'FER)
                             (LIST 'EQUAL 'ZAN 'BOS) WEM1)))
              (SETQ WEM4
                      (AEVAL*
                       (LIST 'WHEREEXP
                             (LIST 'LIST
                                   (SETK 'HELP*
                                         (AEVAL* (LIST 'MKID 'CALKUJ WRT))))
                             WEM1)))
              (SETQ WEM5
                      (AEVAL*
                       (LIST 'SUB (LIST 'EQUAL (LIST 'DER 3) 0)
                             (LIST 'EQUAL (LIST 'DER 1) 0)
                             (LIST 'EQUAL (LIST 'D 1) 0)
                             (LIST 'EQUAL (LIST 'DER 2) 0)
                             (LIST 'WHEREEXP
                                   (LIST 'LIST
                                         (SETK 'HELP*
                                               (AEVAL*
                                                (LIST 'MKID 'POCALKUJ WRT))))
                                   (LIST 'MINUS WEM1)))))
              (COND
               ((EVALEQUAL (AEVAL* WEM4) 0)
                (SETQ Z_Z_Z
                        (AEVAL*
                         (LIST 'PLUS Z_Z_Z
                               (LIST 'TIMES (LIST 'DEL (LIST 'MINUS WRT))
                                     WEM3)))))
               (T
                (PROGN
                 (SETK 'XXX
                       (AEVAL*
                        (LIST 'WHEREEXP
                              (LIST 'LIST
                                    (LIST 'LIST (LIST 'REPLACEBY WEM3 'KOZ)))
                              (LIST 'PLUS
                                    (LIST 'MINUS (LIST 'TIMES 'L_A_@M WEM4))
                                    WEM3 WEM5))))
                 (SETQ WEM6
                         (AEVAL*
                          (LIST 'RHS (LIST 'FIRST (LIST 'SOLVE 'XXX 'KOZ)))))
                 (AEVAL* (CLEAR (LIST 'XXX)))
                 (SETQ Z_Z_Z
                         (AEVAL*
                          (LIST 'PLUS Z_Z_Z (LIST 'COEFFN WEM6 'L_A_@M 1))))
                 (SETQ WEM6 (AEVAL* (LIST 'SUB (LIST 'EQUAL 'L_A_@M 0) WEM6)))
                 (SETQ WEM2 (AEVAL* (LIST 'PLUS WEM2 WEM6))))))
              (SETQ WEM6 (AEVAL* (LIST 'MAXI WRT WEM2)))
              (SETQ WEM1 (AEVAL* (LIST 'FIRST WEM6)))
              (SETQ WEM2 (AEVAL* (LIST 'SECOND WEM6)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL Z_Z_Z)))) 
(PUT 'HOM 'NUMBER-OF-ARGS 1) 
(FLAG '(HOM) 'OPFN) 
(PUT 'HOM 'DEFINED-ON-LINE '1481) 
(PUT 'HOM 'DEFINED-IN-FILE 'SUSY2/SUSY2.RED) 
(PUT 'HOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HOM (WX)
    (PROG (ZET1 ZET2 IKS IKS1 IKS2)
      (COND
       ((EVALEQUAL (AEVAL WX) 0)
        (RETURN (AEVAL (LIST 'LIST (LIST 'LIST 0) 0)))))
      (SETQ IKS (AEVAL (LIST 'NUM WX)))
      (SETQ IKS2 (AEVAL (LIST 'DEN WX)))
      (SETQ IKS1
              (COND
               ((EVALEQUAL (AEVAL (LIST 'PART IKS 0)) (AEVAL 'MINUS))
                (MINUS 1))
               (T 1)))
      (SETQ IKS (AEVAL (LIST 'TIMES IKS1 IKS)))
      (SETQ ZET1
              (AEVAL
               (LIST 'WHEREEXP
                     (LIST 'LIST
                           (LIST 'LIST
                                 (LIST 'REPLACEBY
                                       (LIST 'FER (LIST '~ 'F) (LIST '~ 'K)
                                             (LIST '~ 'N))
                                       (LIST 'TIMES (LIST '&A 'F)
                                             (LIST 'EXPT (LIST '&A '@)
                                                   (LIST 'PLUS
                                                         (LIST 'TIMES 2 'N)
                                                         (LIST 'COND
                                                               (LIST
                                                                (LIST 'OR
                                                                      (LIST
                                                                       'EVALEQUAL
                                                                       (LIST
                                                                        'AEVAL
                                                                        ''K)
                                                                       1)
                                                                      (LIST
                                                                       'EVALEQUAL
                                                                       (LIST
                                                                        'AEVAL
                                                                        ''K)
                                                                       2))
                                                                1)
                                                               (LIST
                                                                (LIST
                                                                 'EVALEQUAL
                                                                 (LIST 'AEVAL
                                                                       ''K)
                                                                 3)
                                                                2)
                                                               (LIST 'T 0))))
                                             (LIST 'ZEN 'F 'K 'N)))
                                 (LIST 'REPLACEBY
                                       (LIST 'BOS (LIST '~ 'F) (LIST '~ 'K)
                                             (LIST '~ 'N))
                                       (LIST 'TIMES (LIST '&A 'F)
                                             (LIST 'EXPT (LIST '&A '@)
                                                   (LIST 'PLUS
                                                         (LIST 'TIMES 2 'N)
                                                         (LIST 'COND
                                                               (LIST
                                                                (LIST 'OR
                                                                      (LIST
                                                                       'EVALEQUAL
                                                                       (LIST
                                                                        'AEVAL
                                                                        ''K)
                                                                       1)
                                                                      (LIST
                                                                       'EVALEQUAL
                                                                       (LIST
                                                                        'AEVAL
                                                                        ''K)
                                                                       2))
                                                                1)
                                                               (LIST
                                                                (LIST
                                                                 'EVALEQUAL
                                                                 (LIST 'AEVAL
                                                                       ''K)
                                                                 3)
                                                                2)
                                                               (LIST 'T 0))))
                                             (LIST 'ZAN 'F 'K 'N)))))
                     IKS)))
      (SETQ ZET2
              (AEVAL (LIST 'SETPART* (LIST 'PLUS ZET1 'X_X) 0 (AEVAL 'LIST))))
      (SETQ ZET1 (AEVAL (LIST 'REVERSE (LIST 'REST (LIST 'REVERSE ZET2)))))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'LIST (LIST 'QUOTIENT IKS1 IKS2))
              (LIST 'SUB (LIST 'EQUAL 'ZEN 'FER) (LIST 'EQUAL 'ZAN 'BOS)
                    (LIST 'WHEREEXP
                          (LIST 'LIST
                                (LIST 'LIST
                                      (LIST 'REPLACEBY (LIST '&A (LIST '~ 'F))
                                            1)
                                      (LIST 'REPLACEBY (LIST '&A '@) 1)))
                          ZET1))))))) 
(LET '(TRAD)) 
(ENDMODULE) 