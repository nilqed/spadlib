(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EFTHETA)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(PUT 'SHIFT-TAU 'NUMBER-OF-ARGS 1) 
(FLAG '(SHIFT-TAU) 'OPFN) 
(PUT 'SHIFT-TAU 'DEFINED-ON-LINE '92) 
(PUT 'SHIFT-TAU 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'SHIFT-TAU 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SHIFT-TAU (TAU)
    (PROG (NT)
      (SETQ NT (AEVAL (LIST 'FIX (LIST 'REPART TAU))))
      (SETQ TAU (AEVAL (LIST 'DIFFERENCE TAU NT)))
      (SETQ NT (AEVAL (LIST 'MOD NT 8)))
      (RETURN (AEVAL (LIST 'LIST TAU NT))))) 
(PUT 'SHIFT-ARG 'NUMBER-OF-ARGS 2) 
(FLAG '(SHIFT-ARG) 'OPFN) 
(PUT 'SHIFT-ARG 'DEFINED-ON-LINE '101) 
(PUT 'SHIFT-ARG 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'SHIFT-ARG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SHIFT-ARG (Z TAU)
    (PROG (QUASIP RPQ NUMHQP NUMHRP)
      (SETQ QUASIP (AEVAL (LIST 'TIMES 'PI TAU)))
      (SETQ RPQ (AEVAL (LIST 'IMPART QUASIP)))
      (SETQ NUMHQP
              (AEVAL
               (LIST 'FIX
                     (LIST 'TIMES 2 (LIST 'QUOTIENT (LIST 'IMPART Z) RPQ)))))
      (SETQ Z
              (AEVAL
               (LIST 'DIFFERENCE Z
                     (LIST 'TIMES NUMHQP (LIST 'QUOTIENT QUASIP 2)))))
      (SETQ NUMHRP
              (AEVAL
               (LIST 'FIX
                     (LIST 'TIMES 2 (LIST 'QUOTIENT (LIST 'REPART Z) 'PI)))))
      (SETQ Z
              (AEVAL
               (LIST 'DIFFERENCE Z
                     (LIST 'TIMES NUMHRP (LIST 'QUOTIENT 'PI 2)))))
      (RETURN (AEVAL (LIST 'LIST Z NUMHRP NUMHQP TAU 'NUMTAU))))) 
(PUT 'N_THETA1 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA1) 'OPFN) 
(PUT 'N_THETA1 'DEFINED-ON-LINE '114) 
(PUT 'N_THETA1 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA1 (Z Q TAU)
    (PROG (N POW TERM TOTAL TOL M BOUND F)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TOTAL (AEVAL 0))
      (SETQ N (AEVAL 0))
      (SETQ BOUND (AEVAL (LIST 'EXP (LIST 'ABS (LIST 'IMPART Z)))))
      (SETQ F (AEVAL (LIST 'EXPT BOUND 2)))
      (REPEAT
       (PROGN
        (SETQ POW
                (AEVAL*
                 (LIST 'TIMES (LIST 'EXPT (MINUS 1) N)
                       (LIST 'EXPT Q (LIST 'TIMES N (LIST 'PLUS N 1))))))
        (SETQ TERM
                (AEVAL*
                 (LIST 'TIMES POW
                       (LIST 'SIN
                             (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                   Z)))))
        (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))
        (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
        (SETQ M (AEVAL* (LIST 'TIMES (LIST 'ABS POW) BOUND)))
        (AEVAL* 'NIL))
       (OR
        (AND (EVALEQUAL (AEVAL* TOTAL) 0) (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
        (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
      (RETURN
       (AEVAL
        (LIST 'TIMES 2 (LIST 'EXP (LIST 'TIMES 'I 'PI (LIST 'QUOTIENT TAU 4)))
              TOTAL))))) 
(PUT 'NUM_THETA1 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_THETA1) 'OPFN) 
(PUT 'NUM_THETA1 'DEFINED-ON-LINE '133) 
(PUT 'NUM_THETA1 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM_THETA1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_THETA1 (Z Q TAU)
    (PROG (X M NT)
      (COND
       ((EVALGREATERP (AEVAL (LIST 'IMPART TAU)) (AEVAL '(|:DN:| 6 . -1)))
        (RETURN (AEVAL (LIST 'N_THETA1 Z Q TAU)))))
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (SETQ M (AEVAL (LIST 'EXP (LIST 'TIMES 'I NT (LIST 'QUOTIENT 'PI 4)))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART Q)) 0)
        (PROGN
         (COND
          ((EVALLESSP (AEVAL (LIST 'IMPART Q)) 0)
           (PROGN
            (SETQ TAU (AEVAL (LIST 'PLUS TAU 1)))
            (SETQ M
                    (AEVAL
                     (LIST 'TIMES M
                           (LIST 'EXP
                                 (LIST 'MINUS
                                       (LIST 'TIMES 'I
                                             (LIST 'QUOTIENT 'PI 4)))))))))
          (T
           (PROGN
            (SETQ TAU (AEVAL (LIST 'DIFFERENCE TAU 1)))
            (SETQ M
                    (AEVAL
                     (LIST 'TIMES M
                           (LIST 'EXP
                                 (LIST 'TIMES 'I
                                       (LIST 'QUOTIENT 'PI 4))))))))))))
      (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
      (RETURN
       (AEVAL
        (LIST 'MINUS
              (LIST 'TIMES 'I M
                    (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                          (LIST 'QUOTIENT 1 2))
                    (LIST 'EXP
                          (LIST 'TIMES 'I X
                                (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI)))
                    (LIST 'NUM_THETA1 (LIST 'TIMES X Z) Q X))))))) 
(PUT 'NUM1_THETA1 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_THETA1) 'OPFN) 
(PUT 'NUM1_THETA1 'DEFINED-ON-LINE '161) 
(PUT 'NUM1_THETA1 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM1_THETA1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_THETA1 (Z TAU)
    (PROG (NR NQ M NT Q)
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (SETQ M (AEVAL (LIST 'SHIFT-ARG Z TAU)))
      (SETQ Z (AEVAL (LIST 'FIRST M)))
      (SETQ NR (AEVAL (LIST 'SECOND M)))
      (SETQ NQ (AEVAL (LIST 'THIRD M)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ M (AEVAL (LIST 'EXP (LIST 'TIMES 'I NT (LIST 'QUOTIENT 'PI 4)))))
      (SETQ NR (AEVAL (LIST 'MOD NR 4)))
      (COND
       ((OR (EVALEQUAL (AEVAL NR) 2) (EVALEQUAL (AEVAL NR) 3))
        (SETQ M (AEVAL (LIST 'MINUS M)))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NR)))
        (COND
         ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES M (LIST 'EXPT (MINUS 1) NQ)
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                                (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA1 Z Q TAU))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES M (LIST 'EXPT (MINUS 1) NQ) 'I
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS
                                            (LIST 'TIMES
                                                  (LIST 'PLUS
                                                        (LIST 'TIMES 2 NQ) 1)
                                                  'I Z)))
                                (LIST 'EXPT Q
                                      (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                          (LIST 'EXP
                                (LIST 'MINUS
                                      (LIST 'TIMES 'I 'PI
                                            (LIST 'QUOTIENT TAU 4)))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA4 Z Q TAU))))
           (AEVAL 'NIL)))))
       ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES M
                        (LIST 'QUOTIENT
                              (LIST 'EXP (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                              (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA2 Z Q TAU))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES M
                        (LIST 'QUOTIENT
                              (LIST 'EXP
                                    (LIST 'MINUS
                                          (LIST 'TIMES
                                                (LIST 'PLUS (LIST 'TIMES 2 NQ)
                                                      1)
                                                'I Z)))
                              (LIST 'EXPT Q (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                        (LIST 'EXP
                              (LIST 'MINUS
                                    (LIST 'TIMES 'I 'PI
                                          (LIST 'QUOTIENT TAU 4)))))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA3 Z Q TAU))))
         (AEVAL 'NIL)))))) 
(PUT 'N_THETA2 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA2) 'OPFN) 
(PUT 'N_THETA2 'DEFINED-ON-LINE '206) 
(PUT 'N_THETA2 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA2 (Z Q TAU)
    (PROG (N POW TERM TOTAL TOL M BOUND F)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TOTAL (AEVAL 0))
      (SETQ N (AEVAL 0))
      (SETQ BOUND (AEVAL (LIST 'EXP (LIST 'ABS (LIST 'IMPART Z)))))
      (SETQ F (AEVAL (LIST 'EXPT BOUND 2)))
      (REPEAT
       (PROGN
        (SETQ POW (AEVAL* (LIST 'EXPT Q (LIST 'TIMES N (LIST 'PLUS N 1)))))
        (SETQ TERM
                (AEVAL*
                 (LIST 'TIMES POW
                       (LIST 'COS
                             (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                   Z)))))
        (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))
        (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
        (SETQ M (AEVAL* (LIST 'TIMES (LIST 'ABS POW) BOUND)))
        (AEVAL* 'NIL))
       (OR
        (AND (EVALEQUAL (AEVAL* TOTAL) 0) (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
        (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
      (RETURN
       (AEVAL
        (LIST 'TIMES 2 (LIST 'EXP (LIST 'TIMES 'I 'PI (LIST 'QUOTIENT TAU 4)))
              TOTAL))))) 
(PUT 'NUM_THETA2 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_THETA2) 'OPFN) 
(PUT 'NUM_THETA2 'DEFINED-ON-LINE '226) 
(PUT 'NUM_THETA2 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM_THETA2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_THETA2 (Z Q TAU)
    (PROG (X M NT)
      (COND
       ((EVALGREATERP (AEVAL (LIST 'IMPART TAU)) (AEVAL '(|:DN:| 6 . -1)))
        (RETURN (AEVAL (LIST 'N_THETA2 Z Q TAU)))))
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (SETQ M (AEVAL (LIST 'EXP (LIST 'TIMES 'I NT (LIST 'QUOTIENT 'PI 4)))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'REPART Q)) 0)
        (PROGN
         (COND
          ((EVALLESSP (AEVAL (LIST 'IMPART Q)) 0)
           (PROGN
            (SETQ TAU (AEVAL (LIST 'PLUS TAU 1)))
            (SETQ M
                    (AEVAL
                     (LIST 'TIMES M
                           (LIST 'EXP
                                 (LIST 'MINUS
                                       (LIST 'TIMES 'I
                                             (LIST 'QUOTIENT 'PI 4)))))))))
          (T
           (PROGN
            (SETQ TAU (AEVAL (LIST 'DIFFERENCE TAU 1)))
            (SETQ M
                    (AEVAL
                     (LIST 'TIMES M
                           (LIST 'EXP
                                 (LIST 'TIMES 'I
                                       (LIST 'QUOTIENT 'PI 4))))))))))))
      (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
      (RETURN
       (AEVAL
        (LIST 'TIMES M
              (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                    (LIST 'QUOTIENT 1 2))
              (LIST 'EXP
                    (LIST 'TIMES 'I X (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI)))
              (LIST 'NUM_THETA4 (LIST 'TIMES X Z) Q X)))))) 
(PUT 'NUM1_THETA2 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_THETA2) 'OPFN) 
(PUT 'NUM1_THETA2 'DEFINED-ON-LINE '254) 
(PUT 'NUM1_THETA2 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM1_THETA2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_THETA2 (Z TAU)
    (PROG (NR NQ M NT Q)
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (SETQ M (AEVAL (LIST 'SHIFT-ARG Z TAU)))
      (SETQ Z (AEVAL (LIST 'FIRST M)))
      (SETQ NR (AEVAL (LIST 'SECOND M)))
      (SETQ NQ (AEVAL (LIST 'THIRD M)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (SETQ M (AEVAL (LIST 'EXP (LIST 'TIMES 'I NT (LIST 'QUOTIENT 'PI 4)))))
      (SETQ NR (AEVAL (LIST 'MOD NR 4)))
      (COND
       ((OR (EVALEQUAL (AEVAL NR) 2) (EVALEQUAL (AEVAL NR) 3))
        (SETQ M (AEVAL (LIST 'MINUS M)))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NR)))
        (COND
         ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES M
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                                (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA2 Z Q TAU))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES M
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS
                                            (LIST 'TIMES
                                                  (LIST 'PLUS
                                                        (LIST 'TIMES 2 NQ) 1)
                                                  'I Z)))
                                (LIST 'EXPT Q
                                      (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                          (LIST 'EXP
                                (LIST 'MINUS
                                      (LIST 'TIMES 'I 'PI
                                            (LIST 'QUOTIENT TAU 4)))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA3 Z Q TAU))))
           (AEVAL 'NIL)))))
       ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES M (LIST 'EXPT (MINUS 1) NQ)
                        (LIST 'QUOTIENT
                              (LIST 'EXP (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                              (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
         (RETURN
          (AEVAL (LIST 'MINUS (LIST 'TIMES M (LIST 'NUM_THETA1 Z Q TAU)))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES M (LIST 'EXPT (MINUS 1) NQ) 'I
                        (LIST 'QUOTIENT
                              (LIST 'EXP
                                    (LIST 'MINUS
                                          (LIST 'TIMES
                                                (LIST 'PLUS (LIST 'TIMES 2 NQ)
                                                      1)
                                                'I Z)))
                              (LIST 'EXPT Q (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                        (LIST 'EXP
                              (LIST 'MINUS
                                    (LIST 'TIMES 'I 'PI
                                          (LIST 'QUOTIENT TAU 4)))))))
         (RETURN
          (AEVAL (LIST 'MINUS (LIST 'TIMES M (LIST 'NUM_THETA4 Z Q TAU)))))
         (AEVAL 'NIL)))))) 
(PUT 'N_THETA3 'NUMBER-OF-ARGS 2) 
(FLAG '(N_THETA3) 'OPFN) 
(PUT 'N_THETA3 'DEFINED-ON-LINE '299) 
(PUT 'N_THETA3 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_THETA3 (Z Q)
    (PROG (N POW TERM TOTAL TOL M BOUND F)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TOTAL (AEVAL 0))
      (SETQ N (AEVAL 1))
      (SETQ F (AEVAL (LIST 'EXP (LIST 'TIMES 2 (LIST 'ABS (LIST 'IMPART Z))))))
      (SETQ BOUND (AEVAL 1))
      (REPEAT
       (PROGN
        (SETQ POW (AEVAL* (LIST 'EXPT Q (LIST 'TIMES N N))))
        (SETQ TERM (AEVAL* (LIST 'TIMES POW (LIST 'COS (LIST 'TIMES 2 N Z)))))
        (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))
        (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
        (SETQ M (AEVAL* (LIST 'TIMES (LIST 'ABS POW) BOUND)))
        (AEVAL* 'NIL))
       (OR
        (AND (EVALEQUAL (AEVAL* TOTAL) 0) (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
        (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
      (RETURN (AEVAL (LIST 'PLUS 1 (LIST 'TIMES 2 TOTAL)))))) 
(PUT 'NUM_THETA3 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_THETA3) 'OPFN) 
(PUT 'NUM_THETA3 'DEFINED-ON-LINE '319) 
(PUT 'NUM_THETA3 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM_THETA3 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_THETA3 (Z Q TAU)
    (PROG (X M NT)
      (COND
       ((EVALGREATERP (AEVAL (LIST 'IMPART TAU)) (AEVAL '(|:DN:| 6 . -1)))
        (RETURN (AEVAL (LIST 'N_THETA3 Z Q)))))
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (COND
       ((EVALGEQ (AEVAL (LIST 'REPART Q)) 0)
        (PROGN
         (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
         (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                              (LIST 'QUOTIENT 1 2))
                        (LIST 'EXP
                              (LIST 'TIMES 'I X
                                    (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI))))))
         (RETURN
          (AEVAL
           (LIST 'TIMES M
                 (COND
                  ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
                   (AEVAL (LIST 'NUM_THETA3 (LIST 'TIMES X Z) Q X)))
                  (T (AEVAL (LIST 'NUM_THETA2 (LIST 'TIMES X Z) Q X)))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART Q)) 0)
        (SETQ TAU (AEVAL (LIST 'PLUS TAU 1))))
       (T (SETQ TAU (AEVAL (LIST 'DIFFERENCE TAU 1)))))
      (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
      (SETQ M
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                           (LIST 'QUOTIENT 1 2))
                     (LIST 'EXP
                           (LIST 'TIMES 'I X
                                 (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES M
              (COND
               ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
                (AEVAL (LIST 'NUM_THETA2 (LIST 'TIMES X Z) Q X)))
               (T (AEVAL (LIST 'NUM_THETA3 (LIST 'TIMES X Z) Q X))))))))) 
(PUT 'NUM1_THETA3 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_THETA3) 'OPFN) 
(PUT 'NUM1_THETA3 'DEFINED-ON-LINE '355) 
(PUT 'NUM1_THETA3 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM1_THETA3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_THETA3 (Z TAU)
    (PROG (M NT)
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
        (RETURN (AEVAL (LIST 'NUM2_THETA3 Z TAU))))
       (T (RETURN (AEVAL (LIST 'NUM2_THETA4 Z TAU))))))) 
(PUT 'NUM2_THETA3 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM2_THETA3) 'OPFN) 
(PUT 'NUM2_THETA3 'DEFINED-ON-LINE '368) 
(PUT 'NUM2_THETA3 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM2_THETA3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM2_THETA3 (Z TAU)
    (PROG (NR NQ M Q)
      (SETQ M (AEVAL (LIST 'SHIFT-ARG Z TAU)))
      (SETQ Z (AEVAL (LIST 'FIRST M)))
      (SETQ NR (AEVAL (LIST 'SECOND M)))
      (SETQ NQ (AEVAL (LIST 'THIRD M)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NR)))
        (COND
         ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'QUOTIENT
                          (LIST 'EXP (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                          (LIST 'EXPT Q (LIST 'EXPT NQ 2)))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA3 Z Q TAU))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS
                                            (LIST 'TIMES
                                                  (LIST 'PLUS
                                                        (LIST 'TIMES 2 NQ) 1)
                                                  'I Z)))
                                (LIST 'EXPT Q
                                      (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                          (LIST 'EXP
                                (LIST 'MINUS
                                      (LIST 'TIMES 'I 'PI
                                            (LIST 'QUOTIENT TAU 4)))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA2 Z Q TAU))))
           (AEVAL 'NIL)))))
       ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT (MINUS 1) NQ)
                        (LIST 'QUOTIENT
                              (LIST 'EXP (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                              (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA4 Z Q TAU))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES (LIST 'EXPT (MINUS 1) NQ) 'I
                        (LIST 'QUOTIENT
                              (LIST 'EXP
                                    (LIST 'MINUS
                                          (LIST 'TIMES
                                                (LIST 'PLUS (LIST 'TIMES 2 NQ)
                                                      1)
                                                'I Z)))
                              (LIST 'EXPT Q (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                        (LIST 'EXP
                              (LIST 'MINUS
                                    (LIST 'TIMES 'I 'PI
                                          (LIST 'QUOTIENT TAU 4)))))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA1 Z Q TAU))))
         (AEVAL 'NIL)))))) 
(PUT 'N_THETA4 'NUMBER-OF-ARGS 2) 
(FLAG '(N_THETA4) 'OPFN) 
(PUT 'N_THETA4 'DEFINED-ON-LINE '405) 
(PUT 'N_THETA4 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_THETA4 (Z Q)
    (PROG (N POW TERM TOTAL TOL M BOUND F)
      (SETQ TOL (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
      (SETQ TOTAL (AEVAL 0))
      (SETQ N (AEVAL 1))
      (SETQ F (AEVAL (LIST 'EXP (LIST 'TIMES 2 (LIST 'ABS (LIST 'IMPART Z))))))
      (SETQ BOUND (AEVAL 1))
      (REPEAT
       (PROGN
        (SETQ POW
                (AEVAL*
                 (LIST 'TIMES (LIST 'EXPT (MINUS 1) N)
                       (LIST 'EXPT Q (LIST 'TIMES N N)))))
        (SETQ TERM (AEVAL* (LIST 'TIMES POW (LIST 'COS (LIST 'TIMES 2 N Z)))))
        (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
        (SETQ N (AEVAL* (LIST 'PLUS N 1)))
        (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
        (SETQ M (AEVAL* (LIST 'TIMES (LIST 'ABS POW) BOUND)))
        (AEVAL* 'NIL))
       (OR
        (AND (EVALEQUAL (AEVAL* TOTAL) 0) (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
        (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
      (RETURN (AEVAL (LIST 'PLUS 1 (LIST 'TIMES 2 TOTAL)))))) 
(PUT 'NUM_THETA4 'NUMBER-OF-ARGS 3) 
(FLAG '(NUM_THETA4) 'OPFN) 
(PUT 'NUM_THETA4 'DEFINED-ON-LINE '425) 
(PUT 'NUM_THETA4 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM_THETA4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NUM_THETA4 (Z Q TAU)
    (PROG (X M NT)
      (COND
       ((EVALGREATERP (AEVAL (LIST 'IMPART TAU)) (AEVAL '(|:DN:| 6 . -1)))
        (RETURN (AEVAL (LIST 'N_THETA4 Z Q)))))
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (COND
       ((EVALGEQ (AEVAL (LIST 'REPART Q)) 0)
        (PROGN
         (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
         (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                              (LIST 'QUOTIENT 1 2))
                        (LIST 'EXP
                              (LIST 'TIMES 'I X
                                    (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI))))))
         (RETURN
          (AEVAL
           (LIST 'TIMES M
                 (COND
                  ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
                   (AEVAL (LIST 'NUM_THETA2 (LIST 'TIMES X Z) Q X)))
                  (T (AEVAL (LIST 'NUM_THETA3 (LIST 'TIMES X Z) Q X)))))))
         (AEVAL 'NIL))))
      (COND
       ((EVALLESSP (AEVAL (LIST 'IMPART Q)) 0)
        (SETQ TAU (AEVAL (LIST 'PLUS TAU 1))))
       (T (SETQ TAU (AEVAL (LIST 'DIFFERENCE TAU 1)))))
      (SETQ X (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 TAU))))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI X))))
      (SETQ M
              (AEVAL
               (LIST 'TIMES
                     (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I X))
                           (LIST 'QUOTIENT 1 2))
                     (LIST 'EXP
                           (LIST 'TIMES 'I X
                                 (LIST 'QUOTIENT (LIST 'EXPT Z 2) 'PI))))))
      (RETURN
       (AEVAL
        (LIST 'TIMES M
              (COND
               ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
                (AEVAL (LIST 'NUM_THETA3 (LIST 'TIMES X Z) Q X)))
               (T (AEVAL (LIST 'NUM_THETA2 (LIST 'TIMES X Z) Q X))))))))) 
(PUT 'NUM1_THETA4 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM1_THETA4) 'OPFN) 
(PUT 'NUM1_THETA4 'DEFINED-ON-LINE '458) 
(PUT 'NUM1_THETA4 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM1_THETA4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM1_THETA4 (Z TAU)
    (PROG (M NT)
      (SETQ M (AEVAL (LIST 'SHIFT-TAU TAU)))
      (SETQ TAU (AEVAL (LIST 'FIRST M)))
      (SETQ NT (AEVAL (LIST 'SECOND M)))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NT)))
        (RETURN (AEVAL (LIST 'NUM2_THETA4 Z TAU))))
       (T (RETURN (AEVAL (LIST 'NUM2_THETA3 Z TAU))))))) 
(PUT 'NUM2_THETA4 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM2_THETA4) 'OPFN) 
(PUT 'NUM2_THETA4 'DEFINED-ON-LINE '469) 
(PUT 'NUM2_THETA4 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'NUM2_THETA4 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM2_THETA4 (Z TAU)
    (PROG (NR NQ M Q)
      (SETQ M (AEVAL (LIST 'SHIFT-ARG Z TAU)))
      (SETQ Z (AEVAL (LIST 'FIRST M)))
      (SETQ NR (AEVAL (LIST 'SECOND M)))
      (SETQ NQ (AEVAL (LIST 'THIRD M)))
      (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
      (COND
       ((BOOLVALUE* (REVALX (LIST 'EVENP NR)))
        (COND
         ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT (MINUS 1) NQ)
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                                (LIST 'EXPT Q (LIST 'EXPT NQ 2))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA4 Z Q TAU))))
           (AEVAL 'NIL)))
         (T
          (PROGN
           (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
           (SETQ M
                   (AEVAL
                    (LIST 'TIMES (LIST 'EXPT (MINUS 1) NQ) 'I
                          (LIST 'QUOTIENT
                                (LIST 'EXP
                                      (LIST 'MINUS
                                            (LIST 'TIMES
                                                  (LIST 'PLUS
                                                        (LIST 'TIMES 2 NQ) 1)
                                                  'I Z)))
                                (LIST 'EXPT Q
                                      (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                          (LIST 'EXP
                                (LIST 'MINUS
                                      (LIST 'TIMES 'I 'PI
                                            (LIST 'QUOTIENT TAU 4)))))))
           (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA1 Z Q TAU))))
           (AEVAL 'NIL)))))
       ((BOOLVALUE* (REVALX (LIST 'EVENP NQ)))
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT NQ 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'EXP (LIST 'MINUS (LIST 'TIMES 2 'I NQ Z)))
                        (LIST 'EXPT Q (LIST 'EXPT NQ 2)))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA3 Z Q TAU))))
         (AEVAL 'NIL)))
       (T
        (PROGN
         (SETQ NQ (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE NQ 1) 2)))
         (SETQ M
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'QUOTIENT
                              (LIST 'EXP
                                    (LIST 'MINUS
                                          (LIST 'TIMES
                                                (LIST 'PLUS (LIST 'TIMES 2 NQ)
                                                      1)
                                                'I Z)))
                              (LIST 'EXPT Q (LIST 'PLUS (LIST 'EXPT NQ 2) NQ)))
                        (LIST 'EXP
                              (LIST 'MINUS
                                    (LIST 'TIMES 'I 'PI
                                          (LIST 'QUOTIENT TAU 4)))))))
         (RETURN (AEVAL (LIST 'TIMES M (LIST 'NUM_THETA2 Z Q TAU))))
         (AEVAL 'NIL)))))) 
(OPERATOR (LIST 'ELLIPTICTHETA1)) 
(OPERATOR (LIST 'ELLIPTICTHETA2)) 
(OPERATOR (LIST 'ELLIPTICTHETA3)) 
(OPERATOR (LIST 'ELLIPTICTHETA4)) 
(OPERATOR (LIST 'NOME2MOD)) 
(OPERATOR (LIST '|NOME2MOD'|)) 
(OPERATOR (LIST '|NOME2k|)) 
(OPERATOR (LIST '|NOME2k'|)) 
(SETK 'ELLIPTICTHETARULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'TAU))
                   (LIST 'MINUS (LIST 'ELLIPTICTHETA1 'U 'TAU)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'TAU))
                   (LIST 'ELLIPTICTHETA2 'U 'TAU))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'TAU))
                   (LIST 'ELLIPTICTHETA3 'U 'TAU))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4 (LIST 'MINUS (LIST '~ 'U))
                         (LIST '~ 'TAU))
                   (LIST 'ELLIPTICTHETA4 'U 'TAU))
             (LIST 'REPLACEBY (LIST 'ELLIPTICTHETA1 0 (LIST '~ 'TAU)) 0)
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1 (LIST '~ 'U) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETA 'NUM1_THETA1 'U 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2 (LIST '~ 'U) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETA 'NUM1_THETA2 'U 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3 (LIST '~ 'U) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETA 'NUM1_THETA3 'U 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4 (LIST '~ 'U) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETA 'NUM1_THETA4 'U 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'R 'S)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   ''D))))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'N 4)))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST
                                                              ''ELLIPTICTHETA1
                                                              'ARG ''TAU)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''TIMES 'S
                                                             (LIST 'LIST
                                                                   ''ELLIPTICTHETA2
                                                                   'ARG
                                                                   ''TAU)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K 'D)))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'R 'S)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   ''D))))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'N 4)))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'OR
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  2)
                                            (LIST 'EVALEQUAL (LIST 'AEVAL 'R)
                                                  3))
                                      (LIST 'SETQ 'S
                                            (LIST 'AEVAL (LIST 'MINUS 1))))
                                     (LIST 'T (LIST 'SETQ 'S (LIST 'AEVAL 1))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''TIMES 'S
                                                        (LIST 'LIST
                                                              ''ELLIPTICTHETA2
                                                              'ARG ''TAU)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''TIMES 'S
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICTHETA1
                                                                         'ARG
                                                                         ''TAU))))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K 'D)))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''ELLIPTICTHETA3
                                                        'ARG ''TAU))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST
                                                             ''ELLIPTICTHETA4
                                                             'ARG ''TAU))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K 'D)))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K)) 'PI))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   ''D))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K ''D)
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''ELLIPTICTHETA4
                                                        'ARG ''TAU))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST
                                                             ''ELLIPTICTHETA3
                                                             'ARG ''TAU))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K 'D)))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '~ 'TAU)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'F)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D)))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D))
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI ''TAU))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'PROGN
                                            (LIST 'SETQ 'N
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''QUOTIENT
                                                              'N 2)))
                                            (LIST 'SETQ 'F
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'N)
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           2
                                                                           ''I
                                                                           'N
                                                                           'ARG)))
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''EXPT
                                                                            'N
                                                                            2)))))))
                                            (LIST 'RETURN
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 'F
                                                              (LIST 'LIST
                                                                    ''ELLIPTICTHETA1
                                                                    'ARG
                                                                    ''TAU))))
                                            (LIST 'AEVAL ''NIL)))
                                     (LIST 'T
                                           (LIST 'PROGN
                                                 (LIST 'SETQ 'N
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         'N 1)
                                                                   2)))
                                                 (LIST 'SETQ 'F
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         (LIST
                                                                          'MINUS
                                                                          1)
                                                                         'N)
                                                                   ''I
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''TIMES
                                                                             2
                                                                             'N)
                                                                            1)
                                                                           ''I
                                                                           'ARG)))
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''EXPT
                                                                             'N
                                                                             2)
                                                                            'N
                                                                            (LIST
                                                                             'LIST
                                                                             ''QUOTIENT
                                                                             1
                                                                             4))))))))
                                                 (LIST 'RETURN
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES 'F
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICTHETA4
                                                                         'ARG
                                                                         ''TAU))))
                                                 (LIST 'AEVAL ''NIL)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K
                                                       (LIST 'TIMES 'PI
                                                             'D))))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '~ 'TAU)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'F)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D)))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D))
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI ''TAU))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'PROGN
                                            (LIST 'SETQ 'N
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''QUOTIENT
                                                              'N 2)))
                                            (LIST 'SETQ 'F
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           2
                                                                           ''I
                                                                           'N
                                                                           'ARG)))
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''EXPT
                                                                            'N
                                                                            2)))))))
                                            (LIST 'RETURN
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 'F
                                                              (LIST 'LIST
                                                                    ''ELLIPTICTHETA2
                                                                    'ARG
                                                                    ''TAU))))
                                            (LIST 'AEVAL ''NIL)))
                                     (LIST 'T
                                           (LIST 'PROGN
                                                 (LIST 'SETQ 'N
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         'N 1)
                                                                   2)))
                                                 (LIST 'SETQ 'F
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''TIMES
                                                                             2
                                                                             'N)
                                                                            1)
                                                                           ''I
                                                                           'ARG)))
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''EXPT
                                                                             'N
                                                                             2)
                                                                            'N
                                                                            (LIST
                                                                             'LIST
                                                                             ''QUOTIENT
                                                                             1
                                                                             4))))))))
                                                 (LIST 'RETURN
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES 'F
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICTHETA3
                                                                         'ARG
                                                                         ''TAU))))
                                                 (LIST 'AEVAL ''NIL)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K
                                                       (LIST 'TIMES 'PI
                                                             'D))))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '~ 'TAU)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'F)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D)))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D))
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI ''TAU))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'PROGN
                                            (LIST 'SETQ 'N
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''QUOTIENT
                                                              'N 2)))
                                            (LIST 'SETQ 'F
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           2
                                                                           ''I
                                                                           'N
                                                                           'ARG)))
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''EXPT
                                                                            'N
                                                                            2)))))))
                                            (LIST 'RETURN
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 'F
                                                              (LIST 'LIST
                                                                    ''ELLIPTICTHETA3
                                                                    'ARG
                                                                    ''TAU))))
                                            (LIST 'AEVAL ''NIL)))
                                     (LIST 'T
                                           (LIST 'PROGN
                                                 (LIST 'SETQ 'N
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         'N 1)
                                                                   2)))
                                                 (LIST 'SETQ 'F
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''TIMES
                                                                             2
                                                                             'N)
                                                                            1)
                                                                           ''I
                                                                           'ARG)))
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''EXPT
                                                                             'N
                                                                             2)
                                                                            'N
                                                                            (LIST
                                                                             'LIST
                                                                             ''QUOTIENT
                                                                             1
                                                                             4))))))))
                                                 (LIST 'RETURN
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES 'F
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICTHETA2
                                                                         'ARG
                                                                         ''TAU))))
                                                 (LIST 'AEVAL ''NIL)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K
                                                       (LIST 'TIMES 'PI
                                                             'D))))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ (LIST '~ 'W))
                                     (LIST 'TIMES (LIST '~ (LIST '~ 'K))
                                           (LIST '~ 'TAU)))
                               (LIST '~ (LIST '~ 'D)))
                         (LIST '~ 'TAU))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'ARG 'F)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''TIMES 2
                                                       (LIST 'LIST ''REPART
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D)))))))
                               (LIST 'SETQ 'ARG
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''QUOTIENT ''W
                                                       ''D)
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''K
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         ''PI
                                                                         ''D))
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   'N 2))
                                                       ''PI ''TAU))))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'PROGN
                                            (LIST 'SETQ 'N
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''QUOTIENT
                                                              'N 2)))
                                            (LIST 'SETQ 'F
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES
                                                              (LIST 'LIST
                                                                    ''EXPT
                                                                    (LIST
                                                                     'MINUS 1)
                                                                    'N)
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           2
                                                                           ''I
                                                                           'N
                                                                           'ARG)))
                                                              (LIST 'LIST ''EXP
                                                                    (LIST 'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''EXPT
                                                                            'N
                                                                            2)))))))
                                            (LIST 'RETURN
                                                  (LIST 'AEVAL
                                                        (LIST 'LIST ''TIMES 'F
                                                              (LIST 'LIST
                                                                    ''ELLIPTICTHETA4
                                                                    'ARG
                                                                    ''TAU))))
                                            (LIST 'AEVAL ''NIL)))
                                     (LIST 'T
                                           (LIST 'PROGN
                                                 (LIST 'SETQ 'N
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   (LIST 'LIST
                                                                         ''DIFFERENCE
                                                                         'N 1)
                                                                   2)))
                                                 (LIST 'SETQ 'F
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         (LIST
                                                                          'MINUS
                                                                          1)
                                                                         'N)
                                                                   ''I
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''TIMES
                                                                             2
                                                                             'N)
                                                                            1)
                                                                           ''I
                                                                           'ARG)))
                                                                   (LIST 'LIST
                                                                         ''EXP
                                                                         (LIST
                                                                          'LIST
                                                                          ''MINUS
                                                                          (LIST
                                                                           'LIST
                                                                           ''TIMES
                                                                           ''I
                                                                           ''PI
                                                                           ''TAU
                                                                           (LIST
                                                                            'LIST
                                                                            ''PLUS
                                                                            (LIST
                                                                             'LIST
                                                                             ''EXPT
                                                                             'N
                                                                             2)
                                                                            'N
                                                                            (LIST
                                                                             'LIST
                                                                             ''QUOTIENT
                                                                             1
                                                                             4))))))))
                                                 (LIST 'RETURN
                                                       (LIST 'AEVAL
                                                             (LIST 'LIST
                                                                   ''TIMES 'F
                                                                   (LIST 'LIST
                                                                         ''ELLIPTICTHETA1
                                                                         'ARG
                                                                         ''TAU))))
                                                 (LIST 'AEVAL ''NIL)))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'TIMES 2
                                           (LIST 'REPART
                                                 (LIST 'QUOTIENT 'K
                                                       (LIST 'TIMES 'PI
                                                             'D))))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1 (LIST '~ 'U)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'TAU) (LIST '~ 'K))
                               (LIST '~ (LIST '~ 'D))))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'R)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''QUOTIENT ''K
                                                       ''D)
                                                 'N)))
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'N 8)))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES ''I
                                                             'N
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''PI 4)))
                                                 (LIST 'LIST ''ELLIPTICTHETA1
                                                       ''U
                                                       (LIST 'LIST ''PLUS
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''TAU ''D)
                                                             'R))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2 (LIST '~ 'U)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'TAU) (LIST '~ 'K))
                               (LIST '~ (LIST '~ 'D))))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'R)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''QUOTIENT ''K
                                                       ''D)
                                                 'N)))
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL (LIST 'LIST ''MOD 'N 8)))
                               (LIST 'RETURN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''EXP
                                                       (LIST 'LIST ''TIMES ''I
                                                             'N
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''PI 4)))
                                                 (LIST 'LIST ''ELLIPTICTHETA2
                                                       ''U
                                                       (LIST 'LIST ''PLUS
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''TAU ''D)
                                                             'R))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3 (LIST '~ 'U)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'TAU) (LIST '~ 'K))
                               (LIST '~ (LIST '~ 'D))))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'R)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''QUOTIENT ''K
                                                       ''D)
                                                 'N)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''ELLIPTICTHETA3
                                                        ''U
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''QUOTIENT
                                                                    ''TAU ''D)
                                                              'R)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST
                                                             ''ELLIPTICTHETA4
                                                             ''U
                                                             (LIST 'LIST ''PLUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         ''TAU
                                                                         ''D)
                                                                   'R)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4 (LIST '~ 'U)
                         (LIST 'QUOTIENT
                               (LIST 'PLUS (LIST '~ 'TAU) (LIST '~ 'K))
                               (LIST '~ (LIST '~ 'D))))
                   (LIST 'WHEN
                         (LIST 'PROG (LIST 'N 'R)
                               (LIST 'SETQ 'N
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''FIX
                                                 (LIST 'LIST ''REPART
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''K ''D)))))
                               (LIST 'SETQ 'R
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''DIFFERENCE
                                                 (LIST 'LIST ''QUOTIENT ''K
                                                       ''D)
                                                 'N)))
                               (LIST 'COND
                                     (LIST
                                      (LIST 'BOOLVALUE*
                                            (LIST 'REVALX
                                                  (LIST 'LIST ''EVENP 'N)))
                                      (LIST 'RETURN
                                            (LIST 'AEVAL
                                                  (LIST 'LIST ''ELLIPTICTHETA4
                                                        ''U
                                                        (LIST 'LIST ''PLUS
                                                              (LIST 'LIST
                                                                    ''QUOTIENT
                                                                    ''TAU ''D)
                                                              'R)))))
                                     (LIST 'T
                                           (LIST 'RETURN
                                                 (LIST 'AEVAL
                                                       (LIST 'LIST
                                                             ''ELLIPTICTHETA3
                                                             ''U
                                                             (LIST 'LIST ''PLUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         ''TAU
                                                                         ''D)
                                                                   'R)))))))
                         (LIST 'WHERE
                               (LIST 'AND (LIST 'RATNUMP 'RP)
                                     (LIST 'GEQ (LIST 'ABS 'RP) 1))
                               (LIST 'REPLACEBY 'RP
                                     (LIST 'REPART (LIST 'QUOTIENT 'K 'D))))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA1 (LIST '~ 'U)
                         (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST '~ 'TAU))))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I 'TAU))
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 'EXP
                                     (LIST 'TIMES 'I 'TAU
                                           (LIST 'QUOTIENT (LIST 'EXPT 'U 2)
                                                 'PI)))
                               (LIST 'ELLIPTICTHETA1 (LIST 'TIMES 'TAU 'U)
                                     'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA2 (LIST '~ 'U)
                         (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST '~ 'TAU))))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I 'TAU))
                               (LIST 'QUOTIENT 1 2))
                         (LIST 'EXP
                               (LIST 'TIMES 'I 'TAU
                                     (LIST 'QUOTIENT (LIST 'EXPT 'U 2) 'PI)))
                         (LIST 'ELLIPTICTHETA4 (LIST 'TIMES 'TAU 'U) 'TAU)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA3 (LIST '~ 'U)
                         (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST '~ 'TAU))))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I 'TAU))
                               (LIST 'QUOTIENT 1 2))
                         (LIST 'EXP
                               (LIST 'TIMES 'I 'TAU
                                     (LIST 'QUOTIENT (LIST 'EXPT 'U 2) 'PI)))
                         (LIST 'ELLIPTICTHETA3 (LIST 'TIMES 'TAU 'U) 'TAU)))
             (LIST 'REPLACEBY
                   (LIST 'ELLIPTICTHETA4 (LIST '~ 'U)
                         (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST '~ 'TAU))))
                   (LIST 'TIMES
                         (LIST 'EXPT (LIST 'MINUS (LIST 'TIMES 'I 'TAU))
                               (LIST 'QUOTIENT 1 2))
                         (LIST 'EXP
                               (LIST 'TIMES 'I 'TAU
                                     (LIST 'QUOTIENT (LIST 'EXPT 'U 2) 'PI)))
                         (LIST 'ELLIPTICTHETA2 (LIST 'TIMES 'TAU 'U) 'TAU)))
             (LIST 'REPLACEBY (LIST 'NOME2MOD (LIST '~ 'Q))
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY 'TAU
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT (LIST 'LOG 'Q)
                                                       'PI)))))
                         (LIST 'EXPT
                               (LIST 'QUOTIENT (LIST 'ELLIPTICTHETA2 0 'TAU)
                                     (LIST 'ELLIPTICTHETA3 0 'TAU))
                               2)))
             (LIST 'REPLACEBY (LIST '|NOME2MOD'| (LIST '~ 'Q))
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY 'TAU
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT (LIST 'LOG 'Q)
                                                       'PI)))))
                         (LIST 'EXPT
                               (LIST 'QUOTIENT (LIST 'ELLIPTICTHETA4 'TAU)
                                     (LIST 'ELLIPTICTHETA3 0 'TAU))
                               2)))
             (LIST 'REPLACEBY (LIST '|NOME2k| (LIST '~ 'Q))
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY 'TAU
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT (LIST 'LOG 'Q)
                                                       'PI)))))
                         (LIST 'TIMES 'PI
                               (LIST 'QUOTIENT
                                     (LIST 'EXPT (LIST 'ELLIPTICTHETA3 0 'TAU)
                                           2)
                                     2))))
             (LIST 'REPLACEBY (LIST '|NOME2k'| (LIST '~ 'Q))
                   (LIST 'WHEREEXP
                         (LIST 'LIST
                               (LIST 'REPLACEBY 'TAU
                                     (LIST 'MINUS
                                           (LIST 'TIMES 'I
                                                 (LIST 'QUOTIENT (LIST 'LOG 'Q)
                                                       'PI)))))
                         (LIST 'MINUS
                               (LIST 'TIMES 'I 'TAU 'PI
                                     (LIST 'QUOTIENT
                                           (LIST 'EXPT
                                                 (LIST 'ELLIPTICTHETA3 0 'TAU)
                                                 2)
                                           2)))))))) 
(LET '(ELLIPTICTHETARULES)) 
(PUT 'ELLIPTICTHETA1 'FANCY-FUNCTIONSYMBOL "\\vartheta_1") 
(PUT 'ELLIPTICTHETA2 'FANCY-FUNCTIONSYMBOL "\\vartheta_2") 
(PUT 'ELLIPTICTHETA3 'FANCY-FUNCTIONSYMBOL "\\vartheta_3") 
(PUT 'ELLIPTICTHETA4 'FANCY-FUNCTIONSYMBOL "\\vartheta_4") 
(PUT 'ELLIPTICTHETA1 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ELLIPTICTHETA2 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ELLIPTICTHETA3 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ELLIPTICTHETA4 'FANCY-SYMBOL-LENGTH 4) 
(PUT 'ELLIPTICTHETA1 'PLAIN-FUNCTIONSYMBOL 'THETA1) 
(PUT 'ELLIPTICTHETA2 'PLAIN-FUNCTIONSYMBOL 'THETA2) 
(PUT 'ELLIPTICTHETA3 'PLAIN-FUNCTIONSYMBOL 'THETA3) 
(PUT 'ELLIPTICTHETA4 'PLAIN-FUNCTIONSYMBOL 'THETA4) 
(PUT 'ELLIPTICTHETA1 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICTHETA2 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICTHETA3 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICTHETA4 'PRIFN 'PLAIN-SYMBOL) 
(PUT 'ELLIPTICTAU 'PRIFN 'PLAIN-SYMBOL) 
(FLAG '(ELLIPTICTHETA1 ELLIPTICTHETA2 ELLIPTICTHETA3 ELLIPTICTHETA4) 'SPECFN) 
(DEFLIST
 '((ELLIPTICTHETA1 2) (ELLIPTICTHETA2 2) (ELLIPTICTHETA3 2) (ELLIPTICTHETA4 2))
 'NUMBER-OF-ARGS) 
(PUT 'N_THETA1D 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA1D) 'OPFN) 
(PUT 'N_THETA1D 'DEFINED-ON-LINE '763) 
(PUT 'N_THETA1D 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA1D 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA1D (Z D TAU)
    (COND
     ((NOT (AND (FIXP (REVALX D)) (EVALGREATERP (AEVAL D) 0)))
      (AEVAL (REDERR (REVALX "d must be a positve integer: n_theta1d"))))
     (T
      (PROG (N Q POW TERM TOTAL TOL M BOUND F)
        (SETQ TOL
                (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
        (SETQ TOTAL (AEVAL 0))
        (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
        (SETQ N (AEVAL 0))
        (SETQ BOUND (AEVAL (LIST 'EXP (LIST 'ABS (LIST 'IMPART Z)))))
        (SETQ F (AEVAL (LIST 'EXPT BOUND 2)))
        (REPEAT
         (PROGN
          (SETQ POW
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) N)
                         (LIST 'EXPT Q (LIST 'TIMES N (LIST 'PLUS N 1))))))
          (COND
           ((BOOLVALUE* (REVALX (LIST 'EVENP D)))
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'QUOTIENT D 2))
                           (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D) POW
                           (LIST 'SIN
                                 (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                       Z))))))
           (T
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES
                           (LIST 'EXPT (MINUS 1)
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE D 1) 2))
                           (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D) POW
                           (LIST 'COS
                                 (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                       Z)))))))
          (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
          (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
          (SETQ M
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D)
                         (LIST 'ABS POW) BOUND)))
          (SETQ N (AEVAL* (LIST 'PLUS N 1)))
          (AEVAL* 'NIL))
         (OR
          (AND (EVALEQUAL (AEVAL* TOTAL) 0)
               (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
          (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
        (RETURN
         (AEVAL
          (LIST 'TIMES 2
                (LIST 'EXP (LIST 'TIMES 'I 'PI (LIST 'QUOTIENT TAU 4)))
                TOTAL))))))) 
(PUT 'N_THETA2D 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA2D) 'OPFN) 
(PUT 'N_THETA2D 'DEFINED-ON-LINE '791) 
(PUT 'N_THETA2D 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA2D 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA2D (Z D TAU)
    (COND
     ((NOT (AND (FIXP (REVALX D)) (EVALGREATERP (AEVAL D) 0)))
      (AEVAL (REDERR (REVALX "d must be a positve integer: n_theta2d"))))
     (T
      (PROG (N Q POW TERM TOTAL TOL M BOUND F)
        (SETQ TOL
                (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
        (SETQ TOTAL (AEVAL 0))
        (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
        (SETQ BOUND (AEVAL (LIST 'EXP (LIST 'ABS (LIST 'IMPART Z)))))
        (SETQ F (AEVAL (LIST 'EXPT BOUND 2)))
        (SETQ N (AEVAL 0))
        (REPEAT
         (PROGN
          (SETQ POW (AEVAL* (LIST 'EXPT Q (LIST 'TIMES N (LIST 'PLUS N 1)))))
          (COND
           ((BOOLVALUE* (REVALX (LIST 'EVENP D)))
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'QUOTIENT D 2))
                           (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D) POW
                           (LIST 'COS
                                 (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                       Z))))))
           (T
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES
                           (LIST 'EXPT (MINUS 1)
                                 (LIST 'QUOTIENT (LIST 'PLUS D 1) 2))
                           (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D) POW
                           (LIST 'SIN
                                 (LIST 'TIMES (LIST 'PLUS (LIST 'TIMES 2 N) 1)
                                       Z)))))))
          (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
          (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
          (SETQ M
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (LIST 'PLUS (LIST 'TIMES 2 N) 1) D)
                         (LIST 'ABS POW) BOUND)))
          (SETQ N (AEVAL* (LIST 'PLUS N 1)))
          (AEVAL* 'NIL))
         (OR
          (AND (EVALEQUAL (AEVAL* TOTAL) 0)
               (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
          (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
        (RETURN
         (AEVAL
          (LIST 'TIMES 2
                (LIST 'EXP (LIST 'TIMES 'I 'PI (LIST 'QUOTIENT TAU 4)))
                TOTAL))))))) 
(PUT 'N_THETA3D 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA3D) 'OPFN) 
(PUT 'N_THETA3D 'DEFINED-ON-LINE '818) 
(PUT 'N_THETA3D 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA3D 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA3D (Z D TAU)
    (COND
     ((NOT (AND (FIXP (REVALX D)) (EVALGREATERP (AEVAL D) 0)))
      (AEVAL (REDERR (REVALX "d must be a positve integer: n_theta3d"))))
     (T
      (PROG (N Q POW TERM TOTAL TOL M BOUND F)
        (SETQ TOL
                (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
        (SETQ TOTAL (AEVAL 0))
        (SETQ N (AEVAL 1))
        (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
        (SETQ F
                (AEVAL
                 (LIST 'EXP (LIST 'TIMES 2 (LIST 'ABS (LIST 'IMPART Z))))))
        (SETQ BOUND (AEVAL 1))
        (REPEAT
         (PROGN
          (SETQ POW (AEVAL* (LIST 'EXPT Q (LIST 'TIMES N N))))
          (COND
           ((BOOLVALUE* (REVALX (LIST 'EVENP D)))
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'QUOTIENT D 2))
                           (LIST 'EXPT (LIST 'TIMES 2 N) D) POW
                           (LIST 'COS (LIST 'TIMES 2 N Z))))))
           (T
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES
                           (LIST 'EXPT (MINUS 1)
                                 (LIST 'QUOTIENT (LIST 'PLUS D 1) 2))
                           (LIST 'EXPT (LIST 'TIMES 2 N) D) POW
                           (LIST 'SIN (LIST 'TIMES 2 N Z)))))))
          (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
          (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
          (SETQ M
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (LIST 'TIMES 2 N) D)
                         (LIST 'ABS POW) BOUND)))
          (SETQ N (AEVAL* (LIST 'PLUS N 1)))
          (AEVAL* 'NIL))
         (OR
          (AND (EVALEQUAL (AEVAL* TOTAL) 0)
               (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
          (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
        (RETURN (AEVAL (LIST 'TIMES 2 TOTAL))))))) 
(PUT 'N_THETA4D 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA4D) 'OPFN) 
(PUT 'N_THETA4D 'DEFINED-ON-LINE '843) 
(PUT 'N_THETA4D 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA4D 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA4D (Z D TAU)
    (COND
     ((NOT (AND (FIXP (REVALX D)) (EVALGREATERP (AEVAL D) 0)))
      (AEVAL (REDERR (REVALX "d must be a positve integer: n_theta4d"))))
     (T
      (PROG (N Q POW TERM TOTAL TOL M BOUND F)
        (SETQ TOL
                (AEVAL (LIST 'EXPT '(|:DN:| 100 . -1) (LIST 'MINUS |:PREC:|))))
        (SETQ TOTAL (AEVAL 0))
        (SETQ N (AEVAL 1))
        (SETQ Q (AEVAL (LIST 'EXP (LIST 'TIMES 'I 'PI TAU))))
        (SETQ F
                (AEVAL
                 (LIST 'EXP (LIST 'TIMES 2 (LIST 'ABS (LIST 'IMPART Z))))))
        (SETQ BOUND (AEVAL 1))
        (REPEAT
         (PROGN
          (SETQ POW
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (MINUS 1) N)
                         (LIST 'EXPT Q (LIST 'TIMES N N)))))
          (COND
           ((BOOLVALUE* (REVALX (LIST 'EVENP D)))
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES (LIST 'EXPT (MINUS 1) (LIST 'QUOTIENT D 2))
                           (LIST 'EXPT (LIST 'TIMES 2 N) D) POW
                           (LIST 'COS (LIST 'TIMES 2 N Z))))))
           (T
            (SETQ TERM
                    (AEVAL*
                     (LIST 'TIMES
                           (LIST 'EXPT (MINUS 1)
                                 (LIST 'QUOTIENT (LIST 'PLUS D 1) 2))
                           (LIST 'EXPT (LIST 'TIMES 2 N) D) POW
                           (LIST 'SIN (LIST 'TIMES 2 N Z)))))))
          (SETQ TOTAL (AEVAL* (LIST 'PLUS TOTAL TERM)))
          (SETQ BOUND (AEVAL* (LIST 'TIMES BOUND F)))
          (SETQ M
                  (AEVAL*
                   (LIST 'TIMES (LIST 'EXPT (LIST 'TIMES 2 N) D)
                         (LIST 'ABS POW) BOUND)))
          (SETQ N (AEVAL* (LIST 'PLUS N 1)))
          (AEVAL* 'NIL))
         (OR
          (AND (EVALEQUAL (AEVAL* TOTAL) 0)
               (EVALLESSP (AEVAL* M) (AEVAL* TOL)))
          (EVALLESSP (AEVAL* M) (AEVAL* (LIST 'TIMES (LIST 'ABS TOTAL) TOL)))))
        (RETURN (AEVAL (LIST 'TIMES 2 TOTAL))))))) 
(PUT 'N_THETA 'NUMBER-OF-ARGS 3) 
(FLAG '(N_THETA) 'OPFN) 
(PUT 'N_THETA 'DEFINED-ON-LINE '869) 
(PUT 'N_THETA 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETA (F U TAU)
    (COND
     ((EVALLEQ (AEVAL (LIST 'IMPART TAU)) 0)
      (AEVAL
       (REDERR
        (REVALX
         "2nd parameter of the theta functions must have positive imaginary part"))))
     (T (AEVAL (LIST 'NUM_ELLIPTIC F U TAU))))) 
(PUT 'N_THETAD 'NUMBER-OF-ARGS 4) 
(FLAG '(N_THETAD) 'OPFN) 
(PUT 'N_THETAD 'DEFINED-ON-LINE '874) 
(PUT 'N_THETAD 'DEFINED-IN-FILE 'ELLIPFN/EFTHETA.RED) 
(PUT 'N_THETAD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_THETAD (F U D TAU)
    (COND
     ((EVALLEQ (AEVAL (LIST 'IMPART TAU)) 0)
      (AEVAL
       (REDERR
        (REVALX
         "3rd parameter of the theta derivs must have positive imaginary part"))))
     (T (AEVAL (LIST 'NUM_ELLIPTIC F U D TAU))))) 
(OPERATOR (LIST 'THETA1D 'THETA2D 'THETA3D 'THETA4D)) 
(FLAG '(N_THETA1D N_THETA2D N_THETA3D N_THETA4D) 'SPECFN) 
(DEFLIST '((N_THETA1D 3) (N_THETA2D 3) (N_THETA3D 3) (N_THETA4D 3))
         'NUMBER-OF-ARGS) 
(SETK 'THETA_DERIV_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY
                   (LIST 'THETA1D (LIST '~ 'U) (LIST '~ 'D) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETAD 'N_THETA1D 'U 'D 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'THETA2D (LIST '~ 'U) (LIST '~ 'D) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETAD 'N_THETA2D 'U 'D 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'THETA3D (LIST '~ 'U) (LIST '~ 'D) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETAD 'N_THETA3D 'U 'D 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))
             (LIST 'REPLACEBY
                   (LIST 'THETA4D (LIST '~ 'U) (LIST '~ 'D) (LIST '~ 'TAU))
                   (LIST 'WHEN (LIST 'N_THETAD 'N_THETA4D 'U 'D 'TAU)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'SYMBOLIC '*COMPLEX) (LIST 'NUMBERP 'U)
                               (LIST 'NUMBERP 'TAU))))))) 
(LET '(THETA_DERIV_RULES)) 
(ENDMODULE) 