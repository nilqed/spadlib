(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LIE1234)) 
(AEVAL (NULL (SETQ *MODE 'ALGEBRAIC))) 
(OPERATOR (LIST 'LIEALG 'COMTAB)) 
(PUT 'LIECLASS 'NUMBER-OF-ARGS 1) 
(FLAG '(LIECLASS) 'OPFN) 
(PUT 'LIECLASS 'DEFINED-ON-LINE '36) 
(PUT 'LIECLASS 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIECLASS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIECLASS (DIM)
    (PROG ()
      (COND
       ((NOT
         (OR (EVALEQUAL (AEVAL DIM) 1) (EVALEQUAL (AEVAL DIM) 2)
             (EVALEQUAL (AEVAL DIM) 3) (EVALEQUAL (AEVAL DIM) 4)))
        (AEVAL (REDERR "dimension out of range"))))
      (COND ((NEQ (GETTYPE 'LIESTRIN) 'ARRAY) (REDERR "liestrin not array")))
      (COND
       ((EVALNEQ (AEVAL (LIST 'LENGTH 'LIESTRIN))
                 (AEVAL
                  (LIST 'LIST (LIST 'PLUS DIM 1) (LIST 'PLUS DIM 1)
                        (LIST 'PLUS DIM 1))))
        (AEVAL (REDERR "dimension of liestrin out of range"))))
      (COND
       ((EVALEQUAL (AEVAL DIM) 1)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "one-dimensional Lie algebra") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 1) (LIST 'COMTAB 0))))))
       ((EVALEQUAL (AEVAL DIM) 2)
        (AEVAL (LIST 'LIE2 (LIST 'LIESTRIN 1 2 1) (LIST 'LIESTRIN 1 2 2))))
       ((EVALEQUAL (AEVAL DIM) 3)
        (PROGN
         (AEVAL (MATRIX (LIST (LIST 'LIE3_FF 3 3))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
           (PROGN
            (SETK (LIST 'LIE3_FF 1 I) (AEVAL* (LIST 'LIESTRIN 1 2 I)))
            (SETK (LIST 'LIE3_FF 2 I) (AEVAL* (LIST 'LIESTRIN 1 3 I)))
            (SETK (LIST 'LIE3_FF 3 I) (AEVAL* (LIST 'LIESTRIN 2 3 I))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (AEVAL (LIST 'LIE3 'LIE3_FF))
         (AEVAL (CLEAR (LIST 'LIE3_FF)))))
       (T
        (PROGN
         (ARRAYFN 'ALGEBRAIC (LIST (LIST 'CC 4 4 4)))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
             (PROG (K)
               (SETQ K 1)
              LAB
               (COND ((MINUSP (DIFFERENCE 4 K)) (RETURN NIL)))
               (SETK (LIST 'CC I J K) (AEVAL* (LIST 'LIESTRIN I J K)))
               (SETQ K (PLUS2 K 1))
               (GO LAB))
             (SETQ J (PLUS2 J 1))
             (GO LAB))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (AEVAL (LIST 'LIE4))
         (AEVAL (CLEAR (LIST 'CC))))))
      (RETURN (AEVAL 'LIE_CLASS)))) 
(PUT 'LIE2 'NUMBER-OF-ARGS 2) 
(FLAG '(LIE2) 'OPFN) 
(PUT 'LIE2 'DEFINED-ON-LINE '59) 
(PUT 'LIE2 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIE2 (F G)
    (PROG ()
      (COND
       ((EVALEQUAL (AEVAL G) 0)
        (COND
         ((EVALEQUAL (AEVAL F) 0)
          (SETK 'LIEMAT (AEVAL (LIST 'MAT (LIST 1 0) (LIST 0 1)))))
         (T
          (SETK 'LIEMAT
                (AEVAL
                 (LIST 'MAT (LIST 0 (LIST 'MINUS (LIST 'QUOTIENT 1 F)))
                       (LIST F 0)))))))
       (T
        (SETK 'LIEMAT
              (AEVAL (LIST 'MAT (LIST (LIST 'QUOTIENT 1 G) 0) (LIST F G))))))
      (COND
       ((AND (EVALEQUAL (AEVAL F) 0) (EVALEQUAL (AEVAL G) 0))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "The given Lie algebra is commutative") NIL
                     'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 2) (LIST 'COMTAB 0))))))
       (T
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Y]=Y") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 2) (LIST 'COMTAB 1))))))))) 
(PUT 'LIE3 'NUMBER-OF-ARGS 1) 
(FLAG '(LIE3) 'OPFN) 
(PUT 'LIE3 'DEFINED-ON-LINE '72) 
(PUT 'LIE3 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIE3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIE3 (FF)
    (PROG ()
      (AEVAL (MATRIX (LIST (LIST 'LIEMAT 3 3) (LIST 'L_F 3 3))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'L_JJ 3)))
      (SETK 'L_F (AEVAL FF))
      (PROG (N)
        (SETQ N 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 N)) (RETURN NIL)))
        (SETK (LIST 'L_JJ N)
              (AEVAL*
               (LIST 'PLUS
                     (LIST 'TIMES (LIST 'L_F 1 N)
                           (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'L_F 2 1))
                                 (LIST 'L_F 3 2)))
                     (LIST 'TIMES (LIST 'L_F 2 N)
                           (LIST 'DIFFERENCE (LIST 'L_F 1 1) (LIST 'L_F 3 3)))
                     (LIST 'TIMES (LIST 'L_F 3 N)
                           (LIST 'PLUS (LIST 'L_F 1 2) (LIST 'L_F 2 3))))))
        (SETQ N (PLUS2 N 1))
        (GO LAB))
      (COND
       ((NOT
         (AND (EVALEQUAL (AEVAL (LIST 'L_JJ 1)) 0)
              (EVALEQUAL (AEVAL (LIST 'L_JJ 2)) 0)
              (EVALEQUAL (AEVAL (LIST 'L_JJ 3)) 0)))
        (PROGN
         (AEVAL (CLEAR (LIST 'LIE3_FF 'LIEMAT 'L_F 'L_JJ)))
         (AEVAL (REDERR "not a Lie algebra")))))
      (COND
       ((EVALEQUAL (AEVAL 'L_F)
                   (AEVAL (LIST 'MAT (LIST 0 0 0) (LIST 0 0 0) (LIST 0 0 0))))
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "Your Lie algebra is commutative") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 0))))
         (SETK 'LIEMAT (AEVAL (LIST 'EXPT 'LIEMAT 0)))))
       ((EVALNEQ (AEVAL (LIST 'DET 'L_F)) 0) (AEVAL (LIST 'COM3 FF)))
       ((EVALEQUAL (AEVAL (LIST 'INDEPEND 1 2 FF)) 1)
        (AEVAL (LIST 'COM2 FF 1 2)))
       ((EVALEQUAL (AEVAL (LIST 'INDEPEND 1 3 FF)) 1)
        (AEVAL (LIST 'COM2 FF 1 3)))
       ((EVALEQUAL (AEVAL (LIST 'INDEPEND 2 3 FF)) 1)
        (AEVAL (LIST 'COM2 FF 2 3)))
       (T (AEVAL (LIST 'COM1 FF))))
      (AEVAL (CLEAR (LIST 'L_JJ 'L_F))))) 
(PUT 'INDEPEND 'NUMBER-OF-ARGS 3) 
(FLAG '(INDEPEND) 'OPFN) 
(PUT 'INDEPEND 'DEFINED-ON-LINE '96) 
(PUT 'INDEPEND 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'INDEPEND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INDEPEND (I J F0)
    (PROG ()
      (AEVAL (MATRIX (LIST (LIST 'F1 3 3))))
      (SETK 'F1 (AEVAL F0))
      (COND
       ((AND
         (EVALEQUAL
          (AEVAL
           (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'F1 I 1) (LIST 'F1 J 2))
                 (LIST 'TIMES (LIST 'F1 I 2) (LIST 'F1 J 1))))
          0)
         (EVALEQUAL
          (AEVAL
           (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'F1 I 2) (LIST 'F1 J 3))
                 (LIST 'TIMES (LIST 'F1 I 3) (LIST 'F1 J 2))))
          0)
         (EVALEQUAL
          (AEVAL
           (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'F1 I 1) (LIST 'F1 J 3))
                 (LIST 'TIMES (LIST 'F1 I 3) (LIST 'F1 J 1))))
          0))
        (RETURN 0))
       (T (RETURN 1))))) 
(PUT 'COM1 'NUMBER-OF-ARGS 1) 
(FLAG '(COM1) 'OPFN) 
(PUT 'COM1 'DEFINED-ON-LINE '105) 
(PUT 'COM1 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COM1 (F2)
    (PROG (ALPHA AA BB R I J M N Z1)
      (SETQ R 0)
      (SETQ I 0)
      (SETQ J 0)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ Z1 0)
      (AEVAL (MATRIX (LIST (LIST 'F3 3 3))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'L_C 3 3 3)))
      (SETK 'F3 (AEVAL F2))
      (PROG (M)
        (SETQ M 3)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 M))) (RETURN NIL)))
        (PROG (N)
          (SETQ N 3)
         LAB
          (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 N))) (RETURN NIL)))
          (COND ((EVALNEQ (AEVAL* (LIST 'F3 M N)) 0) (SETQ I (AEVAL* M))))
          (SETQ N (PLUS2 N (MINUS 1)))
          (GO LAB))
        (SETQ M (PLUS2 M (MINUS 1)))
        (GO LAB))
      (COND ((EQUAL I 1) (PROGN (SETQ I (AEVAL 1)) (SETQ J (AEVAL 2))))
            ((EQUAL I 2) (PROGN (SETQ I (AEVAL 1)) (SETQ J (AEVAL 3))))
            (T (PROGN (SETQ I (AEVAL 2)) (SETQ J (AEVAL 3)))))
      (PROG (K)
        (SETQ K 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 K)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'L_C 1 2 K) (AEVAL* (LIST 'F3 1 K)))
         (SETK (LIST 'L_C 2 1 K) (AEVAL* (LIST 'MINUS (LIST 'F3 1 K))))
         (SETK (LIST 'L_C 1 3 K) (AEVAL* (LIST 'F3 2 K)))
         (SETK (LIST 'L_C 3 1 K) (AEVAL* (LIST 'MINUS (LIST 'F3 2 K))))
         (SETK (LIST 'L_C 2 3 K) (AEVAL* (LIST 'F3 3 K)))
         (SETK (LIST 'L_C 3 2 K) (AEVAL* (LIST 'MINUS (LIST 'F3 3 K)))))
        (SETQ K (PLUS2 K 1))
        (GO LAB))
      (SETQ Z1 (AEVAL 0))
      (PROG (U)
        (SETQ U 3)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 U))) (RETURN NIL)))
        (PROG (V)
          (SETQ V 3)
         LAB
          (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 1 V))) (RETURN NIL)))
          (COND
           ((EVALNEQ
             (AEVAL*
              (LIST 'PLUS (LIST 'TIMES (LIST 'L_C I J 1) (LIST 'L_C V 1 U))
                    (LIST 'TIMES (LIST 'L_C I J 2) (LIST 'L_C V 2 U))
                    (LIST 'TIMES (LIST 'L_C I J 3) (LIST 'L_C V 3 U))))
             0)
            (PROGN
             (SETQ M (AEVAL* U))
             (SETQ N (AEVAL* V))
             (SETQ Z1 (AEVAL* 1)))))
          (SETQ V (PLUS2 V (MINUS 1)))
          (GO LAB))
        (SETQ U (PLUS2 U (MINUS 1)))
        (GO LAB))
      (COND
       ((EQUAL Z1 0)
        (PROGN
         (SETK 'A1
               (AEVAL
                (LIST 'MAT (LIST 1 0 0) (LIST 0 1 0)
                      (LIST (LIST 'L_C 1 2 1) (LIST 'L_C 1 2 2)
                            (LIST 'L_C 1 2 3)))))
         (SETK 'A2
               (AEVAL
                (LIST 'MAT (LIST 1 0 0) (LIST 0 0 1)
                      (LIST (LIST 'L_C 1 3 1) (LIST 'L_C 1 3 2)
                            (LIST 'L_C 1 3 3)))))
         (SETK 'A3
               (AEVAL
                (LIST 'MAT (LIST 0 1 0) (LIST 0 0 1)
                      (LIST (LIST 'L_C 2 3 1) (LIST 'L_C 2 3 2)
                            (LIST 'L_C 2 3 3)))))
         (COND ((EVALNEQ (AEVAL (LIST 'DET 'A1)) 0) (SETK 'LIEMAT (AEVAL 'A1)))
               ((EVALNEQ (AEVAL (LIST 'DET 'A2)) 0) (SETK 'LIEMAT (AEVAL 'A2)))
               (T (SETK 'LIEMAT (AEVAL 'A3))))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Y]=Z") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 1))))))
       (T
        (PROGN
         (SETQ ALPHA
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS
                              (LIST 'TIMES (LIST 'L_C I J 1) (LIST 'L_C N 1 M))
                              (LIST 'TIMES (LIST 'L_C I J 2) (LIST 'L_C N 2 M))
                              (LIST 'TIMES (LIST 'L_C I J 3)
                                    (LIST 'L_C N 3 M)))
                        (LIST 'L_C I J M))))
         (SETK 'A1
               (AEVAL
                (LIST 'MAT (LIST 0 0 0) (LIST 0 0 0)
                      (LIST (LIST 'L_C I J 1) (LIST 'L_C I J 2)
                            (LIST 'L_C I J 3)))))
         (SETK (LIST 'A1 1 N) (AEVAL (LIST 'QUOTIENT 1 ALPHA)))
         (SETK (LIST 'A1 2 1) (AEVAL 1))
         (COND ((EVALNEQ (AEVAL (LIST 'DET 'A1)) 0) (SETQ R (AEVAL 1)))
               (T
                (PROGN
                 (SETK (LIST 'A1 2 1) (AEVAL 0))
                 (SETK (LIST 'A1 2 2) (AEVAL 1))
                 (COND ((EVALNEQ (AEVAL (LIST 'DET 'A1)) 0) (SETQ R (AEVAL 2)))
                       (T
                        (PROGN
                         (SETK (LIST 'A1 2 2) (AEVAL 0))
                         (SETK (LIST 'A1 2 3) (AEVAL 1))
                         (SETQ R (AEVAL 3))))))))
         (SETQ AA
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'L_C N R M)
                        (LIST 'TIMES ALPHA (LIST 'L_C I J M)))))
         (SETQ BB
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS
                              (LIST 'TIMES (LIST 'L_C I J 1) (LIST 'L_C R 1 M))
                              (LIST 'TIMES (LIST 'L_C I J 2) (LIST 'L_C R 2 M))
                              (LIST 'TIMES (LIST 'L_C I J 3)
                                    (LIST 'L_C R 3 M)))
                        (LIST 'L_C I J M))))
         (COND
          ((EVALEQUAL (AEVAL AA) 0)
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0) (LIST (LIST 'MINUS BB) 1 0)
                              (LIST 0 0 1))
                        'A1))))
          (T
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0)
                              (LIST (LIST 'QUOTIENT BB AA)
                                    (LIST 'MINUS (LIST 'QUOTIENT 1 AA)) 1)
                              (LIST 0 0 1))
                        'A1)))))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Z]=Z") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 2)))))))
      (AEVAL (CLEAR (LIST 'A1 'A2 'A3 'L_C 'F3))))) 
(PUT 'COM2 'NUMBER-OF-ARGS 3) 
(FLAG '(COM2) 'OPFN) 
(PUT 'COM2 'DEFINED-ON-LINE '153) 
(PUT 'COM2 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COM2 (F2 M N)
    (PROG (Z1 ALPHA ALPHA1 ALPHA2 BETA BETA1 BETA2)
      (AEVAL (MATRIX (LIST (LIST 'F3 3 3))))
      (SETK 'F3 (AEVAL F2))
      (SETK 'A1
            (AEVAL
             (LIST 'MAT (LIST (LIST 'F3 M 1) (LIST 'F3 M 2) (LIST 'F3 M 3))
                   (LIST (LIST 'F3 N 1) (LIST 'F3 N 2) (LIST 'F3 N 3))
                   (LIST 0 0 0))))
      (SETK (LIST 'A1 3 1) (AEVAL 1))
      (SETQ Z1 (AEVAL (LIST 'DET 'A1)))
      (COND
       ((EVALNEQ (AEVAL Z1) 0)
        (PROGN
         (SETQ ALPHA1
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS
                              (LIST 'MINUS
                                    (LIST 'TIMES (LIST 'F3 N 3)
                                          (LIST 'PLUS
                                                (LIST 'TIMES (LIST 'F3 M 2)
                                                      (LIST 'F3 1 2))
                                                (LIST 'TIMES (LIST 'F3 M 3)
                                                      (LIST 'F3 2 2)))))
                              (LIST 'TIMES (LIST 'F3 N 2)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 M 2)
                                                (LIST 'F3 1 3))
                                          (LIST 'TIMES (LIST 'F3 M 3)
                                                (LIST 'F3 2 3)))))
                        Z1)))
         (SETQ ALPHA2
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'PLUS
                              (LIST 'MINUS
                                    (LIST 'TIMES (LIST 'F3 N 3)
                                          (LIST 'PLUS
                                                (LIST 'TIMES (LIST 'F3 N 2)
                                                      (LIST 'F3 1 2))
                                                (LIST 'TIMES (LIST 'F3 N 3)
                                                      (LIST 'F3 2 2)))))
                              (LIST 'TIMES (LIST 'F3 N 2)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 N 2)
                                                (LIST 'F3 1 3))
                                          (LIST 'TIMES (LIST 'F3 N 3)
                                                (LIST 'F3 2 3)))))
                        Z1)))
         (SETQ BETA1
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES (LIST 'F3 M 3)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 M 2)
                                                (LIST 'F3 1 2))
                                          (LIST 'TIMES (LIST 'F3 M 3)
                                                (LIST 'F3 2 2))))
                              (LIST 'TIMES (LIST 'F3 M 2)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 M 2)
                                                (LIST 'F3 1 3))
                                          (LIST 'TIMES (LIST 'F3 M 3)
                                                (LIST 'F3 2 3)))))
                        Z1)))
         (SETQ BETA2
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'DIFFERENCE
                              (LIST 'TIMES (LIST 'F3 M 3)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 N 2)
                                                (LIST 'F3 1 2))
                                          (LIST 'TIMES (LIST 'F3 N 3)
                                                (LIST 'F3 2 2))))
                              (LIST 'TIMES (LIST 'F3 M 2)
                                    (LIST 'PLUS
                                          (LIST 'TIMES (LIST 'F3 N 2)
                                                (LIST 'F3 1 3))
                                          (LIST 'TIMES (LIST 'F3 N 3)
                                                (LIST 'F3 2 3)))))
                        Z1)))))
       (T
        (PROGN
         (SETK (LIST 'A1 3 1) (AEVAL 0))
         (SETK (LIST 'A1 3 2) (AEVAL 1))
         (SETQ Z1 (AEVAL (LIST 'DET 'A1)))
         (COND
          ((EVALNEQ (AEVAL Z1) 0)
           (PROGN
            (SETQ ALPHA1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'MINUS
                                       (LIST 'TIMES (LIST 'F3 N 3)
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES (LIST 'F3 M 1)
                                                         (LIST 'F3 1 1))
                                                   (LIST 'TIMES (LIST 'F3 M 3)
                                                         (LIST 'F3 3 1)))))
                                 (LIST 'TIMES (LIST 'F3 N 1)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 1 3))
                                             (LIST 'TIMES (LIST 'F3 M 3)
                                                   (LIST 'F3 3 3)))))
                           Z1)))
            (SETQ ALPHA2
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'MINUS
                                       (LIST 'TIMES (LIST 'F3 N 3)
                                             (LIST 'DIFFERENCE
                                                   (LIST 'TIMES (LIST 'F3 N 1)
                                                         (LIST 'F3 1 1))
                                                   (LIST 'TIMES (LIST 'F3 N 3)
                                                         (LIST 'F3 3 1)))))
                                 (LIST 'TIMES (LIST 'F3 N 1)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 1 3))
                                             (LIST 'TIMES (LIST 'F3 N 3)
                                                   (LIST 'F3 3 3)))))
                           Z1)))
            (SETQ BETA1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES (LIST 'F3 M 3)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 1 1))
                                             (LIST 'TIMES (LIST 'F3 M 3)
                                                   (LIST 'F3 3 1))))
                                 (LIST 'TIMES (LIST 'F3 M 1)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 1 3))
                                             (LIST 'TIMES (LIST 'F3 M 3)
                                                   (LIST 'F3 3 3)))))
                           Z1)))
            (SETQ BETA2
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES (LIST 'F3 M 3)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 1 1))
                                             (LIST 'TIMES (LIST 'F3 N 3)
                                                   (LIST 'F3 3 1))))
                                 (LIST 'TIMES (LIST 'F3 M 1)
                                       (LIST 'DIFFERENCE
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 1 3))
                                             (LIST 'TIMES (LIST 'F3 N 3)
                                                   (LIST 'F3 3 3)))))
                           Z1)))))
          (T
           (PROGN
            (SETK (LIST 'A1 3 2) (AEVAL 0))
            (SETK (LIST 'A1 3 3) (AEVAL 1))
            (SETQ Z1 (AEVAL (LIST 'DET 'A1)))
            (SETQ ALPHA1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES (LIST 'F3 N 2)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 2 1))
                                             (LIST 'TIMES (LIST 'F3 M 2)
                                                   (LIST 'F3 3 1))))
                                 (LIST 'TIMES (LIST 'F3 N 1)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 2 2))
                                             (LIST 'TIMES (LIST 'F3 M 2)
                                                   (LIST 'F3 3 2)))))
                           Z1)))
            (SETQ ALPHA2
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES (LIST 'F3 N 2)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 2 1))
                                             (LIST 'TIMES (LIST 'F3 N 2)
                                                   (LIST 'F3 3 1))))
                                 (LIST 'TIMES (LIST 'F3 N 1)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 2 2))
                                             (LIST 'TIMES (LIST 'F3 N 2)
                                                   (LIST 'F3 3 2)))))
                           Z1)))
            (SETQ BETA1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'MINUS
                                       (LIST 'TIMES (LIST 'F3 M 2)
                                             (LIST 'PLUS
                                                   (LIST 'TIMES (LIST 'F3 M 1)
                                                         (LIST 'F3 2 1))
                                                   (LIST 'TIMES (LIST 'F3 M 2)
                                                         (LIST 'F3 3 1)))))
                                 (LIST 'TIMES (LIST 'F3 M 1)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 M 1)
                                                   (LIST 'F3 2 2))
                                             (LIST 'TIMES (LIST 'F3 M 2)
                                                   (LIST 'F3 3 2)))))
                           Z1)))
            (SETQ BETA2
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'MINUS
                                       (LIST 'TIMES (LIST 'F3 M 2)
                                             (LIST 'PLUS
                                                   (LIST 'TIMES (LIST 'F3 N 1)
                                                         (LIST 'F3 2 1))
                                                   (LIST 'TIMES (LIST 'F3 N 2)
                                                         (LIST 'F3 3 1)))))
                                 (LIST 'TIMES (LIST 'F3 M 1)
                                       (LIST 'PLUS
                                             (LIST 'TIMES (LIST 'F3 N 1)
                                                   (LIST 'F3 2 2))
                                             (LIST 'TIMES (LIST 'F3 N 2)
                                                   (LIST 'F3 3 2)))))
                           Z1)))))))))
      (COND
       ((AND (EVALEQUAL (AEVAL ALPHA2) 0) (EVALEQUAL (AEVAL BETA1) 0)
             (EVALEQUAL (AEVAL ALPHA1) (AEVAL BETA2)))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0) (LIST 0 1 0)
                            (LIST 0 0 (LIST 'QUOTIENT 1 ALPHA1)))
                      'A1)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Z]=X, [Y,Z]=Y") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 3))))))
       (T
        (PROGN
         (COND
          ((EVALNEQ (AEVAL ALPHA2) 0)
           (PROGN
            (SETQ ALPHA (AEVAL (LIST 'PLUS ALPHA1 BETA2)))
            (SETQ BETA
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'TIMES ALPHA2 BETA1)
                           (LIST 'TIMES ALPHA1 BETA2))))
            (SETK 'A2
                  (AEVAL
                   (LIST 'MAT
                         (LIST 0
                               (LIST 'DIFFERENCE BETA1
                                     (LIST 'TIMES ALPHA1
                                           (LIST 'QUOTIENT BETA2 ALPHA2)))
                               0)
                         (LIST 1 (LIST 'MINUS (LIST 'QUOTIENT ALPHA1 ALPHA2))
                               0)
                         (LIST 0 0 1))))))
          ((EVALNEQ (AEVAL BETA1) 0)
           (PROGN
            (SETQ ALPHA (AEVAL (LIST 'PLUS 1 (LIST 'QUOTIENT ALPHA1 BETA2))))
            (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'QUOTIENT ALPHA1 BETA2))))
            (SETK 'A2
                  (AEVAL
                   (LIST 'MAT
                         (LIST
                          (LIST 'MINUS
                                (LIST 'TIMES ALPHA1
                                      (LIST 'QUOTIENT BETA2 BETA1)))
                          0 0)
                         (LIST
                          (LIST 'MINUS
                                (LIST 'QUOTIENT (LIST 'EXPT BETA2 2) BETA1))
                          BETA2 0)
                         (LIST 0 0 (LIST 'QUOTIENT 1 BETA2)))))))
          (T
           (PROGN
            (SETQ ALPHA (AEVAL (LIST 'PLUS ALPHA1 BETA2)))
            (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'TIMES ALPHA1 BETA2))))
            (SETK 'A2
                  (AEVAL
                   (LIST 'MAT (LIST 1 1 0)
                         (LIST (LIST 'QUOTIENT 1 ALPHA1)
                               (LIST 'QUOTIENT 1 BETA2) 0)
                         (LIST 0 0 1)))))))
         (COND
          ((EVALEQUAL (AEVAL ALPHA) 0)
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0)
                               (LIST 0 (LIST 'SQRT (LIST 'ABS BETA)) 0)
                               (LIST 0 0
                                     (LIST 'QUOTIENT 1
                                           (LIST 'SQRT (LIST 'ABS BETA)))))
                         'A2 'A1)))
            (COND
             ((BOOLVALUE* (REVALX *TR_LIE))
              (PROGN
               (ASSGNPRI (AEVAL "[X,Z]=") NIL 'FIRST)
               (ASSGNPRI (AEVAL (LIST 'QUOTIENT BETA (LIST 'ABS BETA))) NIL
                         NIL)
               (ASSGNPRI (AEVAL "Y, [Y,Z]=X") NIL 'LAST))))
            (COND
             ((EVALGREATERP (AEVAL BETA) 0)
              (SETK 'LIE_CLASS
                    (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 4)))))
             (T
              (SETK 'LIE_CLASS
                    (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 5))))))))
          (T
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0) (LIST 0 (LIST 'MINUS ALPHA) 0)
                               (LIST 0 0
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 ALPHA))))
                         'A2 'A1)))
            (COND
             ((BOOLVALUE* (REVALX *TR_LIE))
              (PROGN
               (ASSGNPRI (AEVAL "[X,Z]=-X+") NIL 'FIRST)
               (ASSGNPRI (AEVAL (LIST 'QUOTIENT BETA (LIST 'EXPT ALPHA 2))) NIL
                         NIL)
               (ASSGNPRI (AEVAL "Y, [Y,Z]=X") NIL 'LAST))))
            (SETK 'LIE_CLASS
                  (AEVAL
                   (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 6)
                         (LIST 'QUOTIENT BETA (LIST 'EXPT ALPHA 2)))))))))))
      (AEVAL (CLEAR (LIST 'A1 'A2 'F3))))) 
(PUT 'COM3 'NUMBER-OF-ARGS 1) 
(FLAG '(COM3) 'OPFN) 
(PUT 'COM3 'DEFINED-ON-LINE '219) 
(PUT 'COM3 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COM3 (F2)
    (PROG ()
      (AEVAL (MATRIX (LIST (LIST 'L_K 3 3) (LIST 'F3 3 3))))
      (SETK 'F3 (AEVAL F2))
      (SETK (LIST 'L_K 1 1)
            (AEVAL
             (LIST 'PLUS (LIST 'EXPT (LIST 'F3 1 2) 2)
                   (LIST 'TIMES 2 (LIST 'F3 1 3) (LIST 'F3 2 2))
                   (LIST 'EXPT (LIST 'F3 2 3) 2))))
      (SETK (LIST 'L_K 1 2)
            (AEVAL
             (LIST 'PLUS
                   (LIST 'MINUS (LIST 'TIMES (LIST 'F3 1 1) (LIST 'F3 1 2)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'F3 1 3) (LIST 'F3 3 2))
                         (LIST 'TIMES (LIST 'F3 2 1) (LIST 'F3 1 3)))
                   (LIST 'TIMES (LIST 'F3 2 3) (LIST 'F3 3 3)))))
      (SETK (LIST 'L_K 1 3)
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'F3 1 1)
                                           (LIST 'F3 2 2)))
                               (LIST 'TIMES (LIST 'F3 1 2) (LIST 'F3 3 2)))
                         (LIST 'TIMES (LIST 'F3 2 1) (LIST 'F3 2 3)))
                   (LIST 'TIMES (LIST 'F3 2 2) (LIST 'F3 3 3)))))
      (SETK (LIST 'L_K 2 1) (AEVAL (LIST 'L_K 1 2)))
      (SETK (LIST 'L_K 2 2)
            (AEVAL
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'F3 1 1) 2)
                         (LIST 'TIMES 2 (LIST 'F3 1 3) (LIST 'F3 3 1)))
                   (LIST 'EXPT (LIST 'F3 3 3) 2))))
      (SETK (LIST 'L_K 2 3)
            (AEVAL
             (LIST 'PLUS (LIST 'TIMES (LIST 'F3 1 1) (LIST 'F3 2 1))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'F3 1 2) (LIST 'F3 3 1))
                               (LIST 'TIMES (LIST 'F3 3 1) (LIST 'F3 2 3)))
                         (LIST 'TIMES (LIST 'F3 3 2) (LIST 'F3 3 3))))))
      (SETK (LIST 'L_K 3 1) (AEVAL (LIST 'L_K 1 3)))
      (SETK (LIST 'L_K 3 2) (AEVAL (LIST 'L_K 2 3)))
      (SETK (LIST 'L_K 3 3)
            (AEVAL
             (LIST 'PLUS (LIST 'EXPT (LIST 'F3 2 1) 2)
                   (LIST 'TIMES 2 (LIST 'F3 2 2) (LIST 'F3 3 1))
                   (LIST 'EXPT (LIST 'F3 3 2) 2))))
      (COND
       ((NOT
         (AND (EVALNUMBERP (AEVAL (LIST 'L_K 1 1)))
              (EVALNUMBERP
               (AEVAL
                (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                      (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1)))))
              (EVALNUMBERP (AEVAL (LIST 'DET 'L_K)))))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'MINUS (LIST 'L_K 1 1))) NIL NIL)
          (ASSGNPRI (AEVAL ">0 and ") NIL NIL)
          (ASSGNPRI
           (AEVAL
            (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                  (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1))))
           NIL NIL)
          (ASSGNPRI (AEVAL ">0 and ") NIL NIL)
          (ASSGNPRI (AEVAL (LIST 'MINUS (LIST 'DET 'L_K))) NIL NIL)
          (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
         (SETK 'HE (AEVAL (READ)))
         (COND ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y)) (AEVAL (LIST 'SO3 F2)))
               (T (AEVAL (LIST 'SO21 F2))))))
       ((AND (EVALGREATERP (AEVAL (LIST 'MINUS (LIST 'L_K 1 1))) 0)
             (EVALGREATERP
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                     (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1))))
              0)
             (EVALGREATERP (AEVAL (LIST 'MINUS (LIST 'DET 'L_K))) 0))
        (AEVAL (LIST 'SO3 F2)))
       (T (AEVAL (LIST 'SO21 F2))))
      (AEVAL (CLEAR (LIST 'L_K 'F3))))) 
(PUT 'SO3 'NUMBER-OF-ARGS 1) 
(FLAG '(SO3) 'OPFN) 
(PUT 'SO3 'DEFINED-ON-LINE '247) 
(PUT 'SO3 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'SO3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SO3 (F4)
    (PROG (S TT Q R ALPHA)
      (AEVAL (MATRIX (LIST (LIST 'F5 3 3))))
      (SETK 'F5 (AEVAL F4))
      (SETQ S
              (AEVAL
               (LIST 'QUOTIENT (LIST 'F5 2 2) (LIST 'ABS (LIST 'F5 2 2)))))
      (SETQ TT
              (AEVAL
               (LIST 'ABS
                     (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                           (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))))
      (SETQ R
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'F5 1 1)
                     (LIST 'TIMES (LIST 'F5 1 2)
                           (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2))))))
      (SETQ ALPHA
              (AEVAL
               (LIST 'TIMES TT
                     (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                           (LIST 'TIMES
                                 (LIST 'PLUS
                                       (LIST 'EXPT
                                             (LIST 'QUOTIENT (LIST 'F5 2 1)
                                                   (LIST 'F5 2 2))
                                             2)
                                       (LIST 'QUOTIENT (LIST 'F5 3 1)
                                             (LIST 'F5 2 2)))
                                 TT)))))
      (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT ALPHA))))
      (SETK (LIST 'LIEMAT 1 1)
            (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
      (SETK (LIST 'LIEMAT 1 2) (AEVAL 0))
      (SETK (LIST 'LIEMAT 1 3) (AEVAL 0))
      (SETK (LIST 'LIEMAT 2 1) (AEVAL (LIST 'TIMES Q R)))
      (SETK (LIST 'LIEMAT 2 2) (AEVAL 0))
      (SETK (LIST 'LIEMAT 2 3)
            (AEVAL
             (LIST 'MINUS (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2))))))
      (SETK (LIST 'LIEMAT 3 1)
            (AEVAL
             (LIST 'MINUS
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2))))))
      (SETK (LIST 'LIEMAT 3 2)
            (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
      (SETK (LIST 'LIEMAT 3 3)
            (AEVAL
             (LIST 'TIMES Q S (LIST 'SQRT TT)
                   (LIST 'QUOTIENT (LIST 'F5 1 2) (LIST 'F5 2 2)))))
      (COND
       ((BOOLVALUE* (REVALX *TR_LIE))
        (ASSGNPRI (AEVAL "[X,Y]=Z, [X,Z]=-Y, [Y,Z]=X") NIL 'ONLY)))
      (SETK 'LIE_CLASS (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 7))))
      (AEVAL (CLEAR (LIST 'F5))))) 
(PUT 'SO21 'NUMBER-OF-ARGS 1) 
(FLAG '(SO21) 'OPFN) 
(PUT 'SO21 'DEFINED-ON-LINE '270) 
(PUT 'SO21 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'SO21 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SO21 (F4)
    (PROG (GAM EPS S TT Q R ALPHA)
      (AEVAL (MATRIX (LIST (LIST 'L_G 3 3) (LIST 'F5 3 3))))
      (SETK 'F5 (AEVAL F4))
      (SETK 'LIEMAT (AEVAL (LIST 'MAT (LIST 1 0 0) (LIST 0 1 0) (LIST 0 0 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'F5 2 2)) 0)
        (COND
         ((EVALNEQ (AEVAL (LIST 'F5 1 3)) 0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL (LIST 'MAT (LIST 1 0 0) (LIST 0 0 1) (LIST 0 1 0))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'F5 2 1)))
           (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'F5 2 3)))
           (SETK (LIST 'L_G 1 3) (AEVAL (LIST 'F5 2 2)))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'F5 1 1)))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'F5 1 3)))
           (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'F5 1 2)))
           (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'MINUS (LIST 'F5 3 1))))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'MINUS (LIST 'F5 3 3))))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'MINUS (LIST 'F5 3 2))))
           (SETK 'F5 (AEVAL 'L_G))))
         ((EVALNEQ (AEVAL (LIST 'F5 3 1)) 0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL (LIST 'MAT (LIST 0 1 0) (LIST 1 0 0) (LIST 0 0 1))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'MINUS (LIST 'F5 1 2))))
           (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'MINUS (LIST 'F5 1 1))))
           (SETK (LIST 'L_G 1 3) (AEVAL (LIST 'MINUS (LIST 'F5 1 3))))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'F5 3 2)))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'F5 3 1)))
           (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'F5 3 3)))
           (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'F5 2 2)))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'F5 2 1)))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'F5 2 3)))
           (SETK 'F5 (AEVAL 'L_G))))
         (T
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL (LIST 'MAT (LIST 1 0 1) (LIST 1 0 0) (LIST 0 1 0))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'MINUS (LIST 'F5 2 3))))
           (SETK (LIST 'L_G 1 2)
                 (AEVAL (LIST 'DIFFERENCE (LIST 'F5 2 3) (LIST 'F5 2 1))))
           (SETK (LIST 'L_G 1 3) (AEVAL 0))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'MINUS (LIST 'F5 3 3))))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES 2 (LIST 'F5 1 1))))
           (SETK (LIST 'L_G 2 3)
                 (AEVAL (LIST 'DIFFERENCE (LIST 'F5 1 2) (LIST 'F5 3 2))))
           (SETK (LIST 'L_G 3 1) (AEVAL 0))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'F5 1 1)))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'F5 1 2)))
           (SETK 'F5 (AEVAL 'L_G)))))))
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))
         0)
        (PROGN
         (SETQ GAM
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'F5 1 2) (LIST 'F5 2 2)))))
         (SETQ EPS
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'F5 1 1)
                        (LIST 'TIMES (LIST 'F5 1 2)
                              (LIST 'QUOTIENT (LIST 'F5 2 1)
                                    (LIST 'F5 2 2))))))
         (COND
          ((EVALEQUAL
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                         (LIST 'PLUS (LIST 'EXPT (LIST 'F5 3 2) 2)
                               (LIST 'TIMES (LIST 'F5 3 1) (LIST 'F5 2 2))))
                   (LIST 'TIMES EPS (LIST 'QUOTIENT (LIST 'F5 2 2) 2))))
            0)
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 0 0 1)
                               (LIST 0 (LIST 'QUOTIENT 2 EPS)
                                     (LIST 'TIMES 2 (LIST 'QUOTIENT GAM EPS)))
                               (LIST 1 0 0))
                         'LIEMAT)))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES 2 GAM
                               (LIST 'QUOTIENT (LIST 'F5 3 2) EPS))
                         (LIST 'F5 3 3))))
            (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'MINUS (LIST 'F5 3 2))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES 2 (LIST 'QUOTIENT (LIST 'F5 3 1) EPS)))))
            (SETK (LIST 'L_G 2 1) (AEVAL 0))
            (SETK (LIST 'L_G 2 2)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES EPS (LIST 'QUOTIENT (LIST 'F5 2 2) 2)))))
            (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'MINUS (LIST 'F5 2 1))))
            (SETK (LIST 'L_G 3 1) (AEVAL 0))
            (SETK (LIST 'L_G 3 2) (AEVAL 0))
            (SETK (LIST 'L_G 3 3) (AEVAL (MINUS 2)))
            (SETK 'F5 (AEVAL 'L_G))))
          (T
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT
                               (LIST (LIST 'QUOTIENT 1 2) 0
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 0 (LIST 'QUOTIENT 1 EPS)
                                     (LIST 'QUOTIENT GAM EPS))
                               (LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 0
                                     (LIST 'QUOTIENT 1 2)))
                         'LIEMAT)))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'F5 3 1) (LIST 'TIMES 2 EPS)))))
            (SETK (LIST 'L_G 1 2)
                  (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'F5 3 2) 2))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'DIFFERENCE
                         (LIST 'QUOTIENT (LIST 'F5 3 1) (LIST 'TIMES 2 EPS))
                         1)))
            (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'QUOTIENT (LIST 'F5 2 1) 2)))
            (SETK (LIST 'L_G 2 2)
                  (AEVAL (LIST 'TIMES (LIST 'F5 2 2) (LIST 'QUOTIENT EPS 2))))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'F5 2 1) 2))))
            (SETK (LIST 'L_G 3 1)
                  (AEVAL
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'F5 3 1) (LIST 'TIMES 2 EPS))
                         1)))
            (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'QUOTIENT (LIST 'F5 3 2) 2)))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'F5 3 1) (LIST 'TIMES 2 EPS)))))
            (SETK 'F5 (AEVAL 'L_G))))))))
      (COND
       ((NOT
         (EVALNUMBERP
          (AEVAL
           (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                 (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
          (ASSGNPRI
           (AEVAL
            (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                  (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))
           NIL NIL)
          (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL 'LAST))
         (SETK 'HE (AEVAL (READ)))
         (COND
          ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
           (PROGN
            (SETQ S
                    (AEVAL
                     (LIST 'QUOTIENT (LIST 'F5 2 2)
                           (LIST 'ABS (LIST 'F5 2 2)))))
            (SETQ TT
                    (AEVAL
                     (LIST 'ABS
                           (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                                 (LIST 'TIMES (LIST 'F5 1 3)
                                       (LIST 'F5 2 2))))))
            (SETQ R
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'F5 1 1)
                           (LIST 'TIMES (LIST 'F5 1 2)
                                 (LIST 'QUOTIENT (LIST 'F5 2 1)
                                       (LIST 'F5 2 2))))))
            (SETQ ALPHA
                    (AEVAL
                     (LIST 'TIMES TT
                           (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                                 (LIST 'TIMES
                                       (LIST 'PLUS
                                             (LIST 'EXPT
                                                   (LIST 'QUOTIENT
                                                         (LIST 'F5 2 1)
                                                         (LIST 'F5 2 2))
                                                   2)
                                             (LIST 'QUOTIENT (LIST 'F5 3 1)
                                                   (LIST 'F5 2 2)))
                                       TT)))))
            (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'F5 2 1)
                                     (LIST 'F5 2 2))))))
            (SETK (LIST 'L_G 1 2)
                  (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'F5 1 2) (LIST 'F5 2 2)))))
            (SETK (LIST 'L_G 2 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 2 2) (AEVAL 0))
            (SETK (LIST 'L_G 2 3) (AEVAL 0))
            (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 3 2) (AEVAL 0))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2))))))
            (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
          (T
           (PROGN
            (SETQ S
                    (AEVAL
                     (LIST 'QUOTIENT (LIST 'F5 2 2)
                           (LIST 'ABS (LIST 'F5 2 2)))))
            (SETQ TT
                    (AEVAL
                     (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                           (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2)))))
            (SETQ R
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'F5 1 1)
                           (LIST 'TIMES (LIST 'F5 1 2)
                                 (LIST 'QUOTIENT (LIST 'F5 2 1)
                                       (LIST 'F5 2 2))))))
            (SETQ ALPHA
                    (AEVAL
                     (LIST 'TIMES TT
                           (LIST 'DIFFERENCE (LIST 'TIMES R R)
                                 (LIST 'TIMES
                                       (LIST 'PLUS
                                             (LIST 'EXPT
                                                   (LIST 'QUOTIENT
                                                         (LIST 'F5 2 1)
                                                         (LIST 'F5 2 2))
                                                   2)
                                             (LIST 'QUOTIENT (LIST 'F5 3 1)
                                                   (LIST 'F5 2 2)))
                                       TT)))))
            (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
            (COND
             ((NOT (EVALNUMBERP (AEVAL ALPHA)))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                (ASSGNPRI (AEVAL ALPHA) NIL NIL)
                (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
               (SETK 'HE (AEVAL (READ)))
               (COND
                ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
                 (PROGN
                  (SETK (LIST 'L_G 1 1)
                        (AEVAL
                         (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
                  (SETK (LIST 'L_G 1 2) (AEVAL 0))
                  (SETK (LIST 'L_G 1 3) (AEVAL 0))
                  (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
                  (SETK (LIST 'L_G 2 2) (AEVAL 0))
                  (SETK (LIST 'L_G 2 3)
                        (AEVAL
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
                  (SETK (LIST 'L_G 3 1)
                        (AEVAL
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'F5 2 1)
                                     (LIST 'F5 2 2)))))
                  (SETK (LIST 'L_G 3 2)
                        (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
                  (SETK (LIST 'L_G 3 3)
                        (AEVAL
                         (LIST 'MINUS
                               (LIST 'TIMES Q S (LIST 'SQRT TT)
                                     (LIST 'QUOTIENT (LIST 'F5 1 2)
                                           (LIST 'F5 2 2))))))
                  (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
                (T
                 (PROGN
                  (SETK (LIST 'L_G 1 1)
                        (AEVAL
                         (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
                  (SETK (LIST 'L_G 1 2) (AEVAL 0))
                  (SETK (LIST 'L_G 1 3) (AEVAL 0))
                  (SETK (LIST 'L_G 2 1)
                        (AEVAL
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'F5 2 1)
                                     (LIST 'F5 2 2)))))
                  (SETK (LIST 'L_G 2 2)
                        (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
                  (SETK (LIST 'L_G 2 3)
                        (AEVAL
                         (LIST 'MINUS
                               (LIST 'TIMES Q S (LIST 'SQRT TT)
                                     (LIST 'QUOTIENT (LIST 'F5 1 2)
                                           (LIST 'F5 2 2))))))
                  (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
                  (SETK (LIST 'L_G 3 2) (AEVAL 0))
                  (SETK (LIST 'L_G 3 3)
                        (AEVAL
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
                  (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT))))))))
             ((EVALGREATERP (AEVAL ALPHA) 0)
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (AEVAL 0))
               (SETK (LIST 'L_G 1 3) (AEVAL 0))
               (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 2 2) (AEVAL 0))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 3 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'F5 1 2)
                                        (LIST 'F5 2 2))))))
               (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
             (T
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (AEVAL 0))
               (SETK (LIST 'L_G 1 3) (AEVAL 0))
               (SETK (LIST 'L_G 2 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'F5 1 2)
                                        (LIST 'F5 2 2))))))
               (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 3 2) (AEVAL 0))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
               (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))))))))
       ((EVALLESSP
         (AEVAL
          (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))
         0)
        (PROGN
         (SETQ S
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'F5 2 2) (LIST 'ABS (LIST 'F5 2 2)))))
         (SETQ TT
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                              (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2))))))
         (SETQ R
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'F5 1 1)
                        (LIST 'TIMES (LIST 'F5 1 2)
                              (LIST 'QUOTIENT (LIST 'F5 2 1)
                                    (LIST 'F5 2 2))))))
         (SETQ ALPHA
                 (AEVAL
                  (LIST 'TIMES TT
                        (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                              (LIST 'TIMES
                                    (LIST 'PLUS
                                          (LIST 'EXPT
                                                (LIST 'QUOTIENT (LIST 'F5 2 1)
                                                      (LIST 'F5 2 2))
                                                2)
                                          (LIST 'QUOTIENT (LIST 'F5 3 1)
                                                (LIST 'F5 2 2)))
                                    TT)))))
         (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
         (SETK (LIST 'L_G 1 1)
               (AEVAL
                (LIST 'MINUS
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2))))))
         (SETK (LIST 'L_G 1 2)
               (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
         (SETK (LIST 'L_G 1 3)
               (AEVAL
                (LIST 'TIMES Q S (LIST 'SQRT TT)
                      (LIST 'QUOTIENT (LIST 'F5 1 2) (LIST 'F5 2 2)))))
         (SETK (LIST 'L_G 2 1)
               (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
         (SETK (LIST 'L_G 2 2) (AEVAL 0))
         (SETK (LIST 'L_G 2 3) (AEVAL 0))
         (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
         (SETK (LIST 'L_G 3 2) (AEVAL 0))
         (SETK (LIST 'L_G 3 3)
               (AEVAL
                (LIST 'MINUS
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2))))))
         (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
       (T
        (PROGN
         (SETQ S
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'F5 2 2) (LIST 'ABS (LIST 'F5 2 2)))))
         (SETQ TT
                 (AEVAL
                  (LIST 'PLUS (LIST 'EXPT (LIST 'F5 1 2) 2)
                        (LIST 'TIMES (LIST 'F5 1 3) (LIST 'F5 2 2)))))
         (SETQ R
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'F5 1 1)
                        (LIST 'TIMES (LIST 'F5 1 2)
                              (LIST 'QUOTIENT (LIST 'F5 2 1)
                                    (LIST 'F5 2 2))))))
         (SETQ ALPHA
                 (AEVAL
                  (LIST 'TIMES TT
                        (LIST 'DIFFERENCE (LIST 'TIMES R R)
                              (LIST 'TIMES
                                    (LIST 'PLUS
                                          (LIST 'EXPT
                                                (LIST 'QUOTIENT (LIST 'F5 2 1)
                                                      (LIST 'F5 2 2))
                                                2)
                                          (LIST 'QUOTIENT (LIST 'F5 3 1)
                                                (LIST 'F5 2 2)))
                                    TT)))))
         (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
         (COND
          ((NOT (EVALNUMBERP (AEVAL ALPHA)))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
             (ASSGNPRI (AEVAL ALPHA) NIL NIL)
             (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
            (SETK 'HE (AEVAL (READ)))
            (COND
             ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (AEVAL 0))
               (SETK (LIST 'L_G 1 3) (AEVAL 0))
               (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 2 2) (AEVAL 0))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 3 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'F5 1 2)
                                        (LIST 'F5 2 2))))))
               (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
             (T
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (AEVAL 0))
               (SETK (LIST 'L_G 1 3) (AEVAL 0))
               (SETK (LIST 'L_G 2 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
               (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'F5 1 2)
                                        (LIST 'F5 2 2))))))
               (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 3 2) (AEVAL 0))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
               (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT))))))))
          ((EVALGREATERP (AEVAL ALPHA) 0)
           (PROGN
            (SETK (LIST 'L_G 1 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 2) (AEVAL 0))
            (SETK (LIST 'L_G 1 3) (AEVAL 0))
            (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 2 2) (AEVAL 0))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
            (SETK (LIST 'L_G 3 1)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
            (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'F5 1 2)
                                     (LIST 'F5 2 2))))))
            (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))
          (T
           (PROGN
            (SETK (LIST 'L_G 1 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 2) (AEVAL 0))
            (SETK (LIST 'L_G 1 3) (AEVAL 0))
            (SETK (LIST 'L_G 2 1)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'F5 2 1) (LIST 'F5 2 2)))))
            (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'F5 1 2)
                                     (LIST 'F5 2 2))))))
            (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 3 2) (AEVAL 0))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'F5 2 2)))))
            (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'L_G 'LIEMAT)))))))))
      (COND
       ((BOOLVALUE* (REVALX *TR_LIE))
        (ASSGNPRI (AEVAL "[X,Y]=Z, [X,Z]=Y, [Y,Z]=X") NIL 'ONLY)))
      (SETK 'LIE_CLASS (AEVAL (LIST 'LIST (LIST 'LIEALG 3) (LIST 'COMTAB 8))))
      (AEVAL (CLEAR (LIST 'L_G 'F5))))) 
(PUT 'LIE4 'NUMBER-OF-ARGS 0) 
(FLAG '(LIE4) 'OPFN) 
(PUT 'LIE4 'DEFINED-ON-LINE '447) 
(PUT 'LIE4 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIE4 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIE4 NIL
    (PROG (LAM JAC1 JAC2 JAC3 JAC4 P1 M1 M2 M3 DIML1)
      (SETQ P1 0)
      (SETQ M1 0)
      (SETQ M2 0)
      (SETQ M3 0)
      (SETQ DIML1 0)
      (AEVAL (MATRIX (LIST (LIST 'L_F 6 4))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'ORDV 12)))
      (SETK (LIST 'ORDV 1)
            (SETK (LIST 'ORDV 3) (SETK (LIST 'ORDV 7) (AEVAL 1))))
      (SETK (LIST 'ORDV 2)
            (SETK (LIST 'ORDV 5) (SETK (LIST 'ORDV 9) (AEVAL 2))))
      (SETK (LIST 'ORDV 4)
            (SETK (LIST 'ORDV 6) (SETK (LIST 'ORDV 11) (AEVAL 3))))
      (SETK (LIST 'ORDV 8)
            (SETK (LIST 'ORDV 10) (SETK (LIST 'ORDV 12) (AEVAL 4))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'L_F 1 I) (AEVAL* (LIST 'CC 1 2 I)))
         (SETK (LIST 'L_F 2 I) (AEVAL* (LIST 'CC 1 3 I)))
         (SETK (LIST 'L_F 3 I) (AEVAL* (LIST 'CC 2 3 I)))
         (SETK (LIST 'L_F 4 I) (AEVAL* (LIST 'CC 1 4 I)))
         (SETK (LIST 'L_F 5 I) (AEVAL* (LIST 'CC 2 4 I)))
         (SETK (LIST 'L_F 6 I) (AEVAL* (LIST 'CC 3 4 I)))
         (SETK (LIST 'CC 1 1 I)
               (SETK (LIST 'CC 2 2 I)
                     (SETK (LIST 'CC 3 3 I)
                           (SETK (LIST 'CC 4 4 I) (AEVAL* 0)))))
         (SETK (LIST 'CC 2 1 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 1 I))))
         (SETK (LIST 'CC 3 1 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 2 I))))
         (SETK (LIST 'CC 3 2 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 3 I))))
         (SETK (LIST 'CC 4 1 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 4 I))))
         (SETK (LIST 'CC 4 2 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 5 I))))
         (SETK (LIST 'CC 4 3 I) (AEVAL* (LIST 'MINUS (LIST 'L_F 6 I)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (S)
        (SETQ S 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 S)) (RETURN NIL)))
        (PROGN
         (SETQ JAC1
                 (PROG (R FORALL-RESULT)
                   (SETQ R 1)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'CC 1 2 R)
                                               (LIST 'CC R 3 S))
                                         (LIST 'TIMES (LIST 'CC 2 3 R)
                                               (LIST 'CC R 1 S))
                                         (LIST 'TIMES (LIST 'CC 3 1 R)
                                               (LIST 'CC R 2 S))))
                                  FORALL-RESULT)))
                   (SETQ R (PLUS2 R 1))
                   (GO LAB1)))
         (SETQ JAC2
                 (PROG (R FORALL-RESULT)
                   (SETQ R 1)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'CC 1 2 R)
                                               (LIST 'CC R 4 S))
                                         (LIST 'TIMES (LIST 'CC 2 4 R)
                                               (LIST 'CC R 1 S))
                                         (LIST 'TIMES (LIST 'CC 4 1 R)
                                               (LIST 'CC R 2 S))))
                                  FORALL-RESULT)))
                   (SETQ R (PLUS2 R 1))
                   (GO LAB1)))
         (SETQ JAC3
                 (PROG (R FORALL-RESULT)
                   (SETQ R 1)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'CC 1 3 R)
                                               (LIST 'CC R 4 S))
                                         (LIST 'TIMES (LIST 'CC 3 4 R)
                                               (LIST 'CC R 1 S))
                                         (LIST 'TIMES (LIST 'CC 4 1 R)
                                               (LIST 'CC R 3 S))))
                                  FORALL-RESULT)))
                   (SETQ R (PLUS2 R 1))
                   (GO LAB1)))
         (SETQ JAC4
                 (PROG (R FORALL-RESULT)
                   (SETQ R 1)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (AEVAL*
                            (LIST 'PLUS
                                  (AEVAL*
                                   (LIST 'PLUS
                                         (LIST 'TIMES (LIST 'CC 2 3 R)
                                               (LIST 'CC R 4 S))
                                         (LIST 'TIMES (LIST 'CC 3 4 R)
                                               (LIST 'CC R 2 S))
                                         (LIST 'TIMES (LIST 'CC 4 2 R)
                                               (LIST 'CC R 3 S))))
                                  FORALL-RESULT)))
                   (SETQ R (PLUS2 R 1))
                   (GO LAB1)))
         (COND
          ((OR (EVALNEQ (AEVAL* JAC1) 0) (EVALNEQ (AEVAL* JAC2) 0)
               (EVALNEQ (AEVAL* JAC3) 0) (EVALNEQ (AEVAL* JAC4) 0))
           (SETQ S (AEVAL* 4)))))
        (SETQ S (PLUS2 S 1))
        (GO LAB))
      (COND
       ((OR (EVALNEQ (AEVAL JAC1) 0) (EVALNEQ (AEVAL JAC2) 0)
            (EVALNEQ (AEVAL JAC3) 0) (EVALNEQ (AEVAL JAC4) 0))
        (PROGN
         (AEVAL (CLEAR (LIST 'L_F 'ORDV 'CC)))
         (AEVAL (REDERR "not a Lie algebra")))))
      (SETQ M1 (AEVAL 0))
      (PROG (S)
        (SETQ S 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 6 S)) (RETURN NIL)))
        (PROG (TT)
          (SETQ TT 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 4 TT)) (RETURN NIL)))
          (COND
           ((EVALNEQ (AEVAL* (LIST 'L_F S TT)) 0)
            (PROGN
             (SETQ M1 (AEVAL* S))
             (SETQ P1 (AEVAL* TT))
             (SETQ S (AEVAL* 6))
             (SETQ TT (AEVAL* 4)))))
          (SETQ TT (PLUS2 TT 1))
          (GO LAB))
        (SETQ S (PLUS2 S 1))
        (GO LAB))
      (COND ((EQUAL M1 0) (SETQ DIML1 (AEVAL 0)))
            ((EQUAL M1 6) (SETQ DIML1 (AEVAL 1)))
            (T
             (PROGN
              (SETQ M2 (AEVAL 0))
              (PROG (S)
                (SETQ S (PLUS M1 1))
               LAB
                (COND ((MINUSP (DIFFERENCE 6 S)) (RETURN NIL)))
                (PROGN
                 (SETQ LAM
                         (AEVAL*
                          (LIST 'QUOTIENT (LIST 'L_F S P1) (LIST 'L_F M1 P1))))
                 (PROG (TT)
                   (SETQ TT 1)
                  LAB
                   (COND ((MINUSP (DIFFERENCE 4 TT)) (RETURN NIL)))
                   (COND
                    ((EVALNEQ (AEVAL* (LIST 'L_F S TT))
                              (AEVAL* (LIST 'TIMES LAM (LIST 'L_F M1 TT))))
                     (PROGN
                      (SETQ M2 (AEVAL* S))
                      (SETQ S (AEVAL* 6))
                      (SETQ TT (AEVAL* 4)))))
                   (SETQ TT (PLUS2 TT 1))
                   (GO LAB)))
                (SETQ S (PLUS2 S 1))
                (GO LAB))
              (COND ((EQUAL M2 0) (SETQ DIML1 (AEVAL 1)))
                    ((EQUAL M2 6) (SETQ DIML1 (AEVAL 2)))
                    (T
                     (PROGN
                      (SETQ M3 (AEVAL 0))
                      (PROG (S)
                        (SETQ S (PLUS M2 1))
                       LAB
                        (COND ((MINUSP (DIFFERENCE 6 S)) (RETURN NIL)))
                        (COND
                         ((NOT
                           (AND
                            (EVALEQUAL
                             (AEVAL*
                              (LIST 'DET
                                    (LIST 'MAT
                                          (LIST (LIST 'L_F M1 2)
                                                (LIST 'L_F M1 3)
                                                (LIST 'L_F M1 4))
                                          (LIST (LIST 'L_F M2 2)
                                                (LIST 'L_F M2 3)
                                                (LIST 'L_F M2 4))
                                          (LIST (LIST 'L_F S 2) (LIST 'L_F S 3)
                                                (LIST 'L_F S 4)))))
                             0)
                            (EVALEQUAL
                             (AEVAL*
                              (LIST 'DET
                                    (LIST 'MAT
                                          (LIST (LIST 'L_F M1 1)
                                                (LIST 'L_F M1 3)
                                                (LIST 'L_F M1 4))
                                          (LIST (LIST 'L_F M2 1)
                                                (LIST 'L_F M2 3)
                                                (LIST 'L_F M2 4))
                                          (LIST (LIST 'L_F S 1) (LIST 'L_F S 3)
                                                (LIST 'L_F S 4)))))
                             0)
                            (EVALEQUAL
                             (AEVAL*
                              (LIST 'DET
                                    (LIST 'MAT
                                          (LIST (LIST 'L_F M1 1)
                                                (LIST 'L_F M1 2)
                                                (LIST 'L_F M1 4))
                                          (LIST (LIST 'L_F M2 1)
                                                (LIST 'L_F M2 2)
                                                (LIST 'L_F M2 4))
                                          (LIST (LIST 'L_F S 1) (LIST 'L_F S 2)
                                                (LIST 'L_F S 4)))))
                             0)
                            (EVALEQUAL
                             (AEVAL*
                              (LIST 'DET
                                    (LIST 'MAT
                                          (LIST (LIST 'L_F M1 1)
                                                (LIST 'L_F M1 2)
                                                (LIST 'L_F M1 3))
                                          (LIST (LIST 'L_F M2 1)
                                                (LIST 'L_F M2 2)
                                                (LIST 'L_F M2 3))
                                          (LIST (LIST 'L_F S 1) (LIST 'L_F S 2)
                                                (LIST 'L_F S 3)))))
                             0)))
                          (PROGN (SETQ M3 (AEVAL* S)) (SETQ S (AEVAL* 6)))))
                        (SETQ S (PLUS2 S 1))
                        (GO LAB))
                      (COND ((EQUAL M3 0) (SETQ DIML1 (AEVAL 2)))
                            (T (SETQ DIML1 (AEVAL 3))))))))))
      (COND
       ((EQUAL DIML1 0)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "Your Lie algebra is commutative") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 0))))
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                      (LIST 0 0 0 1))))))
       ((EQUAL DIML1 3)
        (AEVAL
         (LIST 'COM43 (LIST 'ORDV (DIFFERENCE (TIMES 2 M1) 1))
               (LIST 'ORDV (TIMES 2 M1))
               (LIST 'ORDV (DIFFERENCE (TIMES 2 M2) 1))
               (LIST 'ORDV (TIMES 2 M2))
               (LIST 'ORDV (DIFFERENCE (TIMES 2 M3) 1))
               (LIST 'ORDV (TIMES 2 M3)))))
       ((EQUAL DIML1 1)
        (AEVAL
         (LIST 'COM41 (LIST 'ORDV (DIFFERENCE (TIMES 2 M1) 1))
               (LIST 'ORDV (TIMES 2 M1)) P1)))
       (T
        (AEVAL
         (LIST 'COM42 (LIST 'ORDV (DIFFERENCE (TIMES 2 M1) 1))
               (LIST 'ORDV (TIMES 2 M1))
               (LIST 'ORDV (DIFFERENCE (TIMES 2 M2) 1))
               (LIST 'ORDV (TIMES 2 M2))))))
      (AEVAL (CLEAR (LIST 'ORDV 'L_F))))) 
(PUT 'COM41 'NUMBER-OF-ARGS 3) 
(FLAG '(COM41) 'OPFN) 
(PUT 'COM41 'DEFINED-ON-LINE '516) 
(PUT 'COM41 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM41 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COM41 (I1 J1 P1)
    (PROG (Y1 Y2 Y3 BETA1 BETA2 BETA3 BETA4 BETA5 BETA6)
      (AEVAL (MATRIX (LIST (LIST 'LIEMAT 4 4))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
        (SETK (LIST 'LIEMAT 1 I) (AEVAL* (LIST 'CC I1 J1 I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL P1) 1)
        (PROGN (SETQ Y1 (AEVAL 2)) (SETQ Y2 (AEVAL 3)) (SETQ Y3 (AEVAL 4))))
       ((EVALEQUAL (AEVAL P1) 2)
        (PROGN (SETQ Y1 (AEVAL 1)) (SETQ Y2 (AEVAL 3)) (SETQ Y3 (AEVAL 4))))
       ((EVALEQUAL (AEVAL P1) 3)
        (PROGN (SETQ Y1 (AEVAL 1)) (SETQ Y2 (AEVAL 2)) (SETQ Y3 (AEVAL 4))))
       (T (PROGN (SETQ Y1 (AEVAL 1)) (SETQ Y2 (AEVAL 2)) (SETQ Y3 (AEVAL 3)))))
      (SETK (LIST 'LIEMAT 2 Y1)
            (SETK (LIST 'LIEMAT 3 Y2) (SETK (LIST 'LIEMAT 4 Y3) (AEVAL 1))))
      (SETQ BETA1
              (AEVAL
               (LIST 'QUOTIENT
                     (PROG (L FORALL-RESULT)
                       (SETQ L 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((MINUSP (DIFFERENCE 4 L)) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'CC I1 J1 L)
                                             (LIST 'CC L Y1 P1)))
                                      FORALL-RESULT)))
                       (SETQ L (PLUS2 L 1))
                       (GO LAB1))
                     (LIST 'CC I1 J1 P1))))
      (SETQ BETA2
              (AEVAL
               (LIST 'QUOTIENT
                     (PROG (L FORALL-RESULT)
                       (SETQ L 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((MINUSP (DIFFERENCE 4 L)) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'CC I1 J1 L)
                                             (LIST 'CC L Y2 P1)))
                                      FORALL-RESULT)))
                       (SETQ L (PLUS2 L 1))
                       (GO LAB1))
                     (LIST 'CC I1 J1 P1))))
      (SETQ BETA3
              (AEVAL (LIST 'QUOTIENT (LIST 'CC Y1 Y2 P1) (LIST 'CC I1 J1 P1))))
      (SETQ BETA4
              (AEVAL
               (LIST 'QUOTIENT
                     (PROG (L FORALL-RESULT)
                       (SETQ L 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((MINUSP (DIFFERENCE 4 L)) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST 'CC I1 J1 L)
                                             (LIST 'CC L Y3 P1)))
                                      FORALL-RESULT)))
                       (SETQ L (PLUS2 L 1))
                       (GO LAB1))
                     (LIST 'CC I1 J1 P1))))
      (SETQ BETA5
              (AEVAL (LIST 'QUOTIENT (LIST 'CC Y1 Y3 P1) (LIST 'CC I1 J1 P1))))
      (SETQ BETA6
              (AEVAL (LIST 'QUOTIENT (LIST 'CC Y2 Y3 P1) (LIST 'CC I1 J1 P1))))
      (COND
       ((AND (EVALEQUAL (AEVAL BETA1) 0) (EVALEQUAL (AEVAL BETA2) 0)
             (EVALEQUAL (AEVAL BETA3) 0) (EVALEQUAL (AEVAL BETA4) 0)
             (EVALEQUAL (AEVAL BETA5) 0))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 0 0 1) (LIST 0 0 1 0)
                            (LIST 0 1 0 0))
                      'LIEMAT)))
         (SETQ BETA3 (AEVAL (LIST 'MINUS BETA6)))
         (SETQ BETA6 (AEVAL 0))))
       ((AND (EVALEQUAL (AEVAL BETA1) 0) (EVALEQUAL (AEVAL BETA2) 0)
             (EVALEQUAL (AEVAL BETA3) 0))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 0 1)
                            (LIST 0 0 1 0))
                      'LIEMAT)))
         (SETQ BETA2 (AEVAL BETA4))
         (SETQ BETA3 (AEVAL BETA5))
         (SETQ BETA4 (SETQ BETA5 (AEVAL 0)))
         (SETQ BETA6 (AEVAL (LIST 'MINUS BETA6))))))
      (COND
       ((AND (EVALEQUAL (AEVAL BETA1) 0) (EVALEQUAL (AEVAL BETA2) 0))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST BETA3 0 0 0) (LIST 0 1 0 0)
                            (LIST 0 0 1 0) (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ Y1 (AEVAL BETA4))
         (SETQ Y2 (AEVAL (LIST 'QUOTIENT BETA5 BETA3)))
         (SETQ Y3 (AEVAL (LIST 'QUOTIENT BETA6 BETA3)))))
       ((EVALEQUAL (AEVAL BETA1) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0)
                            (LIST (LIST 'MINUS (LIST 'QUOTIENT BETA3 BETA2)) 1
                                  0 0)
                            (LIST 0 0 (LIST 'QUOTIENT 1 BETA2) 0)
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ Y1 (AEVAL BETA4))
         (SETQ Y2
                 (AEVAL
                  (LIST 'DIFFERENCE BETA5
                        (LIST 'TIMES BETA3 (LIST 'QUOTIENT BETA4 BETA2)))))
         (SETQ Y3 (AEVAL (LIST 'QUOTIENT BETA6 BETA2)))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0)
                            (LIST (LIST 'QUOTIENT BETA3 BETA1)
                                  (LIST 'MINUS (LIST 'QUOTIENT BETA2 BETA1)) 1
                                  0)
                            (LIST 0 (LIST 'QUOTIENT 1 BETA1) 0 0)
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ Y1 (AEVAL BETA4))
         (SETQ Y2
                 (AEVAL
                  (LIST 'QUOTIENT
                        (LIST 'DIFFERENCE (LIST 'TIMES BETA3 BETA4)
                              (LIST 'TIMES BETA2 BETA5))
                        BETA1)))
         (SETQ Y3 (AEVAL (LIST 'QUOTIENT BETA5 BETA1))))))
      (COND
       ((AND (EVALEQUAL (AEVAL BETA1) 0) (EVALEQUAL (AEVAL BETA2) 0))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST 0 Y3 (LIST 'MINUS Y2) 1) (LIST 0 0 1 0))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Z]=W") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 2))))))
       (T
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL Y1) 0)
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                              (LIST (LIST 'MINUS Y3) 0 0 (MINUS 1))
                              (LIST 0 0 1 1))
                        'LIEMAT))))
          (T
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                              (LIST (LIST 'MINUS (LIST 'QUOTIENT Y3 Y1)) 0 1
                                    (LIST 'MINUS (LIST 'QUOTIENT 1 Y1)))
                              (LIST 0 0 0 (LIST 'QUOTIENT 1 Y1)))
                        'LIEMAT)))))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[W,Z]=W") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 1))))))))) 
(PUT 'COM42 'NUMBER-OF-ARGS 4) 
(FLAG '(COM42) 'OPFN) 
(PUT 'COM42 'DEFINED-ON-LINE '561) 
(PUT 'COM42 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM42 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COM42 (I1 J1 I2 J2)
    (PROG (D D1 D2 D3 D4 A1 A2 A3 A4 A5 B1 B2 B3 B4 B5)
      (AEVAL (MATRIX (LIST (LIST 'LIEMAT 4 4))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'SOL 1 4)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'LIEMAT 1 I) (AEVAL* (LIST 'CC I1 J1 I)))
         (SETK (LIST 'LIEMAT 2 I) (AEVAL* (LIST 'CC I2 J2 I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETK (LIST 'LIEMAT 3 1) (SETK (LIST 'LIEMAT 4 2) (AEVAL 1)))
      (COND
       ((EVALNEQ (SETQ D (AEVAL (LIST 'DET 'LIEMAT))) 0)
        (PROGN
         (SETQ D1 (AEVAL 1))
         (SETQ D2 (AEVAL 2))
         (SETQ D3 (AEVAL 3))
         (SETQ D4 (AEVAL 4))))
       (T
        (PROGN
         (SETK (LIST 'LIEMAT 4 2) (AEVAL 0))
         (SETK (LIST 'LIEMAT 4 3) (AEVAL 1))
         (COND
          ((EVALNEQ (SETQ D (AEVAL (LIST 'DET 'LIEMAT))) 0)
           (PROGN
            (SETQ D1 (AEVAL 1))
            (SETQ D2 (AEVAL 3))
            (SETQ D3 (AEVAL 2))
            (SETQ D4 (AEVAL 4))
            (SETQ D (AEVAL (LIST 'MINUS D)))))
          (T
           (PROGN
            (SETK (LIST 'LIEMAT 3 1) (AEVAL 0))
            (SETK (LIST 'LIEMAT 3 2) (AEVAL 1))
            (COND
             ((EVALNEQ (SETQ D (AEVAL (LIST 'DET 'LIEMAT))) 0)
              (PROGN
               (SETQ D1 (AEVAL 2))
               (SETQ D2 (AEVAL 3))
               (SETQ D3 (AEVAL 1))
               (SETQ D4 (AEVAL 4))))
             (T
              (PROGN
               (SETK (LIST 'LIEMAT 3 2) (SETK (LIST 'LIEMAT 4 3) (AEVAL 0)))
               (SETK (LIST 'LIEMAT 3 1) (SETK (LIST 'LIEMAT 4 4) (AEVAL 1)))
               (COND
                ((EVALNEQ (SETQ D (AEVAL (LIST 'DET 'LIEMAT))) 0)
                 (PROGN
                  (SETQ D1 (AEVAL 1))
                  (SETQ D2 (AEVAL 4))
                  (SETQ D3 (AEVAL 2))
                  (SETQ D4 (AEVAL 3))))
                (T
                 (PROGN
                  (SETK (LIST 'LIEMAT 3 1) (AEVAL 0))
                  (SETK (LIST 'LIEMAT 3 2) (AEVAL 1))
                  (COND
                   ((EVALNEQ (SETQ D (AEVAL (LIST 'DET 'LIEMAT))) 0)
                    (PROGN
                     (SETQ D1 (AEVAL 2))
                     (SETQ D2 (AEVAL 4))
                     (SETQ D3 (AEVAL 1))
                     (SETQ D4 (AEVAL 3))
                     (SETQ D (AEVAL (LIST 'MINUS D)))))
                   (T
                    (PROGN
                     (SETK (LIST 'LIEMAT 3 2) (AEVAL 0))
                     (SETK (LIST 'LIEMAT 3 3) (AEVAL 1))
                     (SETQ D (AEVAL (LIST 'DET 'LIEMAT)))
                     (SETQ D1 (AEVAL 3))
                     (SETQ D2 (AEVAL 4))
                     (SETQ D3 (AEVAL 1))
                     (SETQ D4 (AEVAL 2)))))))))))))))))
      (SETQ A1
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'DIFFERENCE
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D1 D3)
                                                  (LIST 'CC I2 J2 D4))
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D1 D4)
                                                  (LIST 'CC I2 J2 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ B1
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'PLUS
                                            (LIST 'MINUS
                                                  (LIST 'TIMES
                                                        (LIST 'CC I1 J1 R)
                                                        (LIST 'CC R D1 D3)
                                                        (LIST 'CC I1 J1 D4)))
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D1 D4)
                                                  (LIST 'CC I1 J1 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ A2
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'DIFFERENCE
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D1 D3)
                                                  (LIST 'CC I2 J2 D4))
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D1 D4)
                                                  (LIST 'CC I2 J2 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ B2
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'PLUS
                                            (LIST 'MINUS
                                                  (LIST 'TIMES
                                                        (LIST 'CC I2 J2 R)
                                                        (LIST 'CC R D1 D3)
                                                        (LIST 'CC I1 J1 D4)))
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D1 D4)
                                                  (LIST 'CC I1 J1 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ A3
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'DIFFERENCE
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D2 D3)
                                                  (LIST 'CC I2 J2 D4))
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D2 D4)
                                                  (LIST 'CC I2 J2 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ B3
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'PLUS
                                            (LIST 'MINUS
                                                  (LIST 'TIMES
                                                        (LIST 'CC I1 J1 R)
                                                        (LIST 'CC R D2 D3)
                                                        (LIST 'CC I1 J1 D4)))
                                            (LIST 'TIMES (LIST 'CC I1 J1 R)
                                                  (LIST 'CC R D2 D4)
                                                  (LIST 'CC I1 J1 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ A4
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'DIFFERENCE
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D2 D3)
                                                  (LIST 'CC I2 J2 D4))
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D2 D4)
                                                  (LIST 'CC I2 J2 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ B4
              (PROG (R FORALL-RESULT)
                (SETQ R 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (AEVAL*
                                (LIST 'QUOTIENT
                                      (LIST 'PLUS
                                            (LIST 'MINUS
                                                  (LIST 'TIMES
                                                        (LIST 'CC I2 J2 R)
                                                        (LIST 'CC R D2 D3)
                                                        (LIST 'CC I1 J1 D4)))
                                            (LIST 'TIMES (LIST 'CC I2 J2 R)
                                                  (LIST 'CC R D2 D4)
                                                  (LIST 'CC I1 J1 D3)))
                                      D))
                               FORALL-RESULT)))
                (SETQ R (PLUS2 R 1))
                (GO LAB1)))
      (SETQ A5
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'DIFFERENCE
                           (LIST 'TIMES (LIST 'CC D1 D2 D3)
                                 (LIST 'CC I2 J2 D4))
                           (LIST 'TIMES (LIST 'CC D1 D2 D4)
                                 (LIST 'CC I2 J2 D3)))
                     D)))
      (SETQ B5
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'PLUS
                           (LIST 'MINUS
                                 (LIST 'TIMES (LIST 'CC D1 D2 D3)
                                       (LIST 'CC I1 J1 D4)))
                           (LIST 'TIMES (LIST 'CC D1 D2 D4)
                                 (LIST 'CC I1 J1 D3)))
                     D)))
      (AEVAL (LIST 'FINDCENTRE A1 A2 A3 A4 A5 B1 B2 B3 B4 B5))
      (COND
       ((EVALEQUAL (AEVAL 'NOTTRIV) 0)
        (AEVAL (LIST 'TRIVCENT A1 A2 A3 A4 A5 B1 B2 B3 B4 B5)))
       ((AND (EVALEQUAL (AEVAL (LIST 'SOL 1 3)) 0)
             (EVALEQUAL (AEVAL (LIST 'SOL 1 4)) 0))
        (COND
         ((EVALEQUAL (AEVAL (LIST 'SOL 1 1)) 0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 0 1 0 0) (LIST 1 0 0 0) (LIST 0 0 1 0)
                              (LIST 0 0 0 1))
                        'LIEMAT)))
           (AEVAL (LIST 'CENTINCOM B1 B3 B5 A1 A3 A5))))
         (T
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT
                              (LIST 1
                                    (LIST 'QUOTIENT (LIST 'SOL 1 2)
                                          (LIST 'SOL 1 1))
                                    0 0)
                              (LIST 0 1 0 0) (LIST 0 0 1 0) (LIST 0 0 0 1))
                        'LIEMAT)))
           (AEVAL
            (LIST 'CENTINCOM A2 A4 A5
                  (LIST 'DIFFERENCE B2
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'SOL 1 2) (LIST 'SOL 1 1))
                              A2))
                  (LIST 'DIFFERENCE B4
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'SOL 1 2) (LIST 'SOL 1 1))
                              A4))
                  (LIST 'DIFFERENCE B5
                        (LIST 'TIMES
                              (LIST 'QUOTIENT (LIST 'SOL 1 2) (LIST 'SOL 1 1))
                              A5))))))))
       ((EVALEQUAL
         (AEVAL
          (LIST 'DET
                (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                      (LIST (LIST 'SOL 1 1) (LIST 'SOL 1 2) (LIST 'SOL 1 3)
                            (LIST 'SOL 1 4))
                      (LIST 0 0 0 1))))
         0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST (LIST 'SOL 1 1) (LIST 'SOL 1 2)
                                  (LIST 'SOL 1 3) (LIST 'SOL 1 4))
                            (LIST 0 0 1 0))
                      'LIEMAT)))
         (AEVAL (LIST 'CENTOUTCOM A1 A2 B1 B2))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST (LIST 'SOL 1 1) (LIST 'SOL 1 2)
                                  (LIST 'SOL 1 3) (LIST 'SOL 1 4))
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (AEVAL (LIST 'CENTOUTCOM A3 A4 B3 B4)))))
      (AEVAL (CLEAR (LIST 'SOL 'NOTTRIV))))) 
(PUT 'FINDCENTRE 'NUMBER-OF-ARGS 10) 
(FLAG '(FINDCENTRE) 'OPFN) 
(PUT 'FINDCENTRE 'DEFINED-ON-LINE '620) 
(PUT 'FINDCENTRE 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'FINDCENTRE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE FINDCENTRE (A1 A2 A3 A4 A5 B1 B2 B3 B4 B5)
    (PROG (FLAG HELP)
      (SETQ FLAG 0)
      (SETK 'NOTTRIV (AEVAL 0))
      (SETQ FLAG (AEVAL 0))
      (SETK 'CENT
            (AEVAL
             (LIST 'MAT (LIST A1 A2 0 (LIST 'MINUS A5)) (LIST A3 A4 A5 0)
                   (LIST B1 B2 0 (LIST 'MINUS B5)) (LIST B3 B4 B5 0)
                   (LIST 0 0 A1 A3) (LIST 0 0 A2 A4) (LIST 0 0 B1 B3)
                   (LIST 0 0 B2 B4))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
        (COND
         ((AND (EVALNEQ (AEVAL* (LIST 'CENT I 1)) 0) (EQUAL FLAG 0))
          (PROGN
           (SETQ FLAG (AEVAL* 1))
           (PROG (J)
             (SETQ J 1)
            LAB
             (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
             (PROGN
              (SETQ HELP (AEVAL* (LIST 'CENT 1 J)))
              (SETK (LIST 'CENT 1 J) (AEVAL* (LIST 'CENT I J)))
              (SETK (LIST 'CENT I J) (AEVAL* HELP)))
             (SETQ J (PLUS2 J 1))
             (GO LAB)))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((EQUAL FLAG 0)
        (PROGN (SETK 'NOTTRIV (AEVAL 1)) (SETK (LIST 'SOL 1 1) (AEVAL 1))))
       (T
        (PROGN
         (PROG (I)
           (SETQ I 2)
          LAB
           (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
           (PROGN
            (SETQ HELP
                    (AEVAL*
                     (LIST 'QUOTIENT (LIST 'CENT I 1) (LIST 'CENT 1 1))))
            (PROG (J)
              (SETQ J 1)
             LAB
              (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
              (SETK (LIST 'CENT I J)
                    (AEVAL*
                     (LIST 'DIFFERENCE (LIST 'CENT I J)
                           (LIST 'TIMES HELP (LIST 'CENT 1 J)))))
              (SETQ J (PLUS2 J 1))
              (GO LAB)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ FLAG (AEVAL 0))
         (PROG (I)
           (SETQ I 2)
          LAB
           (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
           (COND
            ((AND (EVALNEQ (AEVAL* (LIST 'CENT I 2)) 0) (EQUAL FLAG 0))
             (PROGN
              (SETQ FLAG (AEVAL* 1))
              (PROG (J)
                (SETQ J 2)
               LAB
                (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
                (PROGN
                 (SETQ HELP (AEVAL* (LIST 'CENT 2 J)))
                 (SETK (LIST 'CENT 2 J) (AEVAL* (LIST 'CENT I J)))
                 (SETK (LIST 'CENT I J) (AEVAL* HELP)))
                (SETQ J (PLUS2 J 1))
                (GO LAB)))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (COND
          ((EQUAL FLAG 0)
           (PROGN
            (SETK 'NOTTRIV (AEVAL 1))
            (SETK (LIST 'SOL 1 1) (AEVAL (LIST 'MINUS (LIST 'CENT 1 2))))
            (SETK (LIST 'SOL 1 2) (AEVAL (LIST 'CENT 1 1)))))
          (T
           (PROGN
            (PROG (I)
              (SETQ I 3)
             LAB
              (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
              (PROGN
               (SETQ HELP
                       (AEVAL*
                        (LIST 'QUOTIENT (LIST 'CENT I 2) (LIST 'CENT 2 2))))
               (PROG (J)
                 (SETQ J 2)
                LAB
                 (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
                 (SETK (LIST 'CENT I J)
                       (AEVAL*
                        (LIST 'DIFFERENCE (LIST 'CENT I J)
                              (LIST 'TIMES HELP (LIST 'CENT 2 J)))))
                 (SETQ J (PLUS2 J 1))
                 (GO LAB)))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (SETQ FLAG (AEVAL 0))
            (PROG (I)
              (SETQ I 3)
             LAB
              (COND ((MINUSP (DIFFERENCE 8 I)) (RETURN NIL)))
              (COND
               ((AND (EVALNEQ (AEVAL* (LIST 'CENT I 3)) 0) (EQUAL FLAG 0))
                (PROGN
                 (SETQ FLAG (AEVAL* 1))
                 (PROG (J)
                   (SETQ J 3)
                  LAB
                   (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
                   (PROGN
                    (SETQ HELP (AEVAL* (LIST 'CENT 3 J)))
                    (SETK (LIST 'CENT 3 J) (AEVAL* (LIST 'CENT I J)))
                    (SETK (LIST 'CENT I J) (AEVAL* HELP)))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB)))))
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND
             ((EQUAL FLAG 0)
              (PROGN
               (SETK 'NOTTRIV (AEVAL 1))
               (SETK (LIST 'SOL 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT
                            (LIST 'DIFFERENCE
                                  (LIST 'TIMES (LIST 'CENT 1 2)
                                        (LIST 'QUOTIENT (LIST 'CENT 2 3)
                                              (LIST 'CENT 2 2)))
                                  (LIST 'CENT 1 3))
                            (LIST 'CENT 1 1))))
               (SETK (LIST 'SOL 1 2)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'QUOTIENT (LIST 'CENT 2 3)
                                  (LIST 'CENT 2 2)))))
               (SETK (LIST 'SOL 1 3) (AEVAL 1))))
             (T
              (PROGN
               (PROG (I)
                 (SETQ I 4)
                LAB
                 (COND ((MINUSP (DIFFERENCE 8 I)) (RETURN NIL)))
                 (PROGN
                  (SETQ HELP
                          (AEVAL*
                           (LIST 'QUOTIENT (LIST 'CENT I 3) (LIST 'CENT 3 3))))
                  (PROG (J)
                    (SETQ J 3)
                   LAB
                    (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
                    (SETK (LIST 'CENT I J)
                          (AEVAL*
                           (LIST 'DIFFERENCE (LIST 'CENT I J)
                                 (LIST 'TIMES HELP (LIST 'CENT 3 J)))))
                    (SETQ J (PLUS2 J 1))
                    (GO LAB)))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (SETQ FLAG (AEVAL 0))
               (PROG (I)
                 (SETQ I 4)
                LAB
                 (COND ((MINUSP (DIFFERENCE 8 I)) (RETURN NIL)))
                 (COND
                  ((AND (EVALNEQ (AEVAL* (LIST 'CENT I 4)) 0) (EQUAL FLAG 0))
                   (PROGN
                    (SETQ FLAG (AEVAL* 1))
                    (SETK (LIST 'CENT 4 4) (AEVAL* (LIST 'CENT I 4))))))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (COND
                ((EQUAL FLAG 0)
                 (PROGN
                  (SETK 'NOTTRIV (AEVAL 1))
                  (SETK (LIST 'SOL 1 1)
                        (AEVAL
                         (LIST 'QUOTIENT
                               (LIST 'PLUS
                                     (LIST 'MINUS
                                           (LIST 'TIMES
                                                 (LIST 'DIFFERENCE
                                                       (LIST 'TIMES
                                                             (LIST 'CENT 2 3)
                                                             (LIST 'QUOTIENT
                                                                   (LIST 'CENT
                                                                         3 4)
                                                                   (LIST 'CENT
                                                                         3 3)))
                                                       (LIST 'CENT 2 4))
                                                 (LIST 'QUOTIENT
                                                       (LIST 'CENT 1 2)
                                                       (LIST 'CENT 2 2))))
                                     (LIST 'DIFFERENCE
                                           (LIST 'TIMES (LIST 'CENT 3 4)
                                                 (LIST 'QUOTIENT
                                                       (LIST 'CENT 1 3)
                                                       (LIST 'CENT 3 3)))
                                           (LIST 'CENT 1 4)))
                               (LIST 'CENT 1 1))))
                  (SETK (LIST 'SOL 1 2)
                        (AEVAL
                         (LIST 'QUOTIENT
                               (LIST 'DIFFERENCE
                                     (LIST 'TIMES (LIST 'CENT 2 3)
                                           (LIST 'QUOTIENT (LIST 'CENT 3 4)
                                                 (LIST 'CENT 3 3)))
                                     (LIST 'CENT 2 4))
                               (LIST 'CENT 2 2))))
                  (SETK (LIST 'SOL 1 3)
                        (AEVAL
                         (LIST 'MINUS
                               (LIST 'QUOTIENT (LIST 'CENT 3 4)
                                     (LIST 'CENT 3 3)))))
                  (SETK (LIST 'SOL 1 4) (AEVAL 1))))))))))))))
      (AEVAL (CLEAR (LIST 'CENT))))) 
(PUT 'CENTINCOM 'NUMBER-OF-ARGS 6) 
(FLAG '(CENTINCOM) 'OPFN) 
(PUT 'CENTINCOM 'DEFINED-ON-LINE '668) 
(PUT 'CENTINCOM 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'CENTINCOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CENTINCOM (A C E B D F)
    (PROG (V1 W1 V2 W2)
      (COND
       ((EVALEQUAL (AEVAL C) 0)
        (COND
         ((EVALEQUAL (AEVAL D) 0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 0 1)
                              (LIST 0 0 1 0))
                        'LIEMAT)))
           (SETQ V1 (AEVAL A))
           (SETQ V2 (AEVAL (LIST 'MINUS E)))
           (SETQ W1 (AEVAL B))
           (SETQ W2 (AEVAL (LIST 'MINUS F)))))
         (T
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                              (LIST 0 0 1 (LIST 'MINUS (LIST 'QUOTIENT B D)))
                              (LIST 0 0 0 1))
                        'LIEMAT)))
           (SETQ V1 (AEVAL C))
           (SETQ V2 (AEVAL E))
           (SETQ W1 (AEVAL D))
           (SETQ W2 (AEVAL F))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST 0 0 1 (LIST 'MINUS (LIST 'QUOTIENT A C)))
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ V1 (AEVAL C))
         (SETQ V2 (AEVAL E))
         (SETQ W1 (AEVAL D))
         (SETQ W2 (AEVAL F)))))
      (COND
       ((EVALEQUAL (AEVAL W1) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST 0 (LIST 'MINUS (LIST 'QUOTIENT V2 W2))
                                  (LIST 'QUOTIENT V1 W2) 0)
                            (LIST 0 0 0 (LIST 'QUOTIENT 1 V1)))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Z]=w, [Y,Z]=X") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 6))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST 0
                                  (LIST 'MINUS
                                        (LIST 'QUOTIENT W2
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES W1 V2)
                                                    (LIST 'TIMES W2 V1))))
                                  (LIST 'TIMES W1
                                        (LIST 'QUOTIENT W1
                                              (LIST 'DIFFERENCE
                                                    (LIST 'TIMES W1 V2)
                                                    (LIST 'TIMES W2 V1))))
                                  0)
                            (LIST 0 0 0 (LIST 'QUOTIENT 1 W1)))
                      (LIST 'MAT (LIST 1 0 0 0) (LIST V1 W1 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Z]=X, [Y,Z]=w") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 7))))))))) 
(PUT 'CENTOUTCOM 'NUMBER-OF-ARGS 4) 
(FLAG '(CENTOUTCOM) 'OPFN) 
(PUT 'CENTOUTCOM 'DEFINED-ON-LINE '689) 
(PUT 'CENTOUTCOM 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'CENTOUTCOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CENTOUTCOM (A C B D)
    (PROG (FLAG ALPHA BETA)
      (SETQ FLAG 0)
      (SETQ FLAG (AEVAL 0))
      (COND
       ((EVALNEQ (AEVAL C) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT
                            (LIST 0
                                  (LIST 'DIFFERENCE B
                                        (LIST 'TIMES A (LIST 'QUOTIENT D C)))
                                  0 0)
                            (LIST 1 (LIST 'MINUS (LIST 'QUOTIENT A C)) 0 0)
                            (LIST 0 0 1 0) (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ ALPHA (AEVAL (LIST 'PLUS A D)))
         (SETQ BETA
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'TIMES B C) (LIST 'TIMES A D))))))
       ((EVALNEQ (AEVAL B) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT
                            (LIST
                             (LIST 'MINUS (LIST 'TIMES A (LIST 'QUOTIENT D B)))
                             0 0 0)
                            (LIST
                             (LIST 'MINUS (LIST 'TIMES D (LIST 'QUOTIENT D B)))
                             D 0 0)
                            (LIST 0 0 1 0) (LIST 0 0 0 (LIST 'QUOTIENT 1 D)))
                      'LIEMAT)))
         (SETQ ALPHA (AEVAL (LIST 'PLUS 1 (LIST 'QUOTIENT A D))))
         (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'QUOTIENT A D))))))
       ((EVALNEQ (AEVAL A) (AEVAL D))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 1 0 0)
                            (LIST (LIST 'QUOTIENT 1 A) (LIST 'QUOTIENT 1 D) 0
                                  0)
                            (LIST 0 0 1 0) (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ ALPHA (AEVAL (LIST 'PLUS A D)))
         (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'TIMES A D))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0 (LIST 'QUOTIENT 1 A)))
                      'LIEMAT)))
         (SETQ FLAG (AEVAL 1)))))
      (COND
       ((EQUAL FLAG 1)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[W,Z]=W, [X,Z]=X") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 10))))))
       ((EVALEQUAL (AEVAL ALPHA) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0)
                            (LIST 0 (LIST 'SQRT (LIST 'ABS BETA)) 0 0)
                            (LIST 0 0 1 0)
                            (LIST 0 0 0
                                  (LIST 'QUOTIENT 1
                                        (LIST 'SQRT (LIST 'ABS BETA)))))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (PROGN
            (ASSGNPRI (AEVAL "[W,Z]=") NIL 'FIRST)
            (ASSGNPRI (AEVAL (LIST 'QUOTIENT BETA (LIST 'ABS BETA))) NIL NIL)
            (ASSGNPRI (AEVAL "X, [X,Z]=W") NIL 'LAST))))
         (COND
          ((EVALGREATERP (AEVAL BETA) 0)
           (SETK 'LIE_CLASS
                 (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 11)))))
          (T
           (SETK 'LIE_CLASS
                 (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 8))))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0)
                            (LIST 0 (LIST 'MINUS ALPHA) 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0
                                  (LIST 'MINUS (LIST 'QUOTIENT 1 ALPHA))))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (PROGN
            (ASSGNPRI (AEVAL "[W,Z]=-W+") NIL 'FIRST)
            (ASSGNPRI (AEVAL (LIST 'QUOTIENT BETA (LIST 'EXPT ALPHA 2))) NIL
                      NIL)
            (ASSGNPRI (AEVAL "X, [X,Z]=W") NIL 'LAST))))
         (SETK 'LIE_CLASS
               (AEVAL
                (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 9)
                      (LIST 'QUOTIENT BETA (LIST 'EXPT ALPHA 2)))))))))) 
(PUT 'TRIVCENT 'NUMBER-OF-ARGS 10) 
(FLAG '(TRIVCENT) 'OPFN) 
(PUT 'TRIVCENT 'DEFINED-ON-LINE '722) 
(PUT 'TRIVCENT 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'TRIVCENT 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL GENERAL)
       GENERAL)) 
(DE TRIVCENT (A1 A2 A3 A4 A5 B1 B2 B3 B4 B5)
    (PROG (FLAG HE HELP ALPHA BETA C1 C2 C3 C4 C5 D1 D2 D3 D4 D5 P E1 E2 E3 E4
           E5 E6)
      (SETQ FLAG 0)
      (COND
       ((EVALEQUAL
         (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES A1 B2) (LIST 'TIMES A2 B1))) 0)
        (COND
         ((EVALEQUAL
           (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES A3 B4) (LIST 'TIMES A4 B3)))
           0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 1)
                              (LIST 0 0 0 1))
                        'LIEMAT)))
           (SETQ A1 (AEVAL (LIST 'PLUS A1 A3)))
           (SETQ B1 (AEVAL (LIST 'PLUS B1 B3)))
           (SETQ A2 (AEVAL (LIST 'PLUS A2 A4)))
           (SETQ B2 (AEVAL (LIST 'PLUS B2 B4)))))
         (T
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 0 1)
                              (LIST 0 0 1 0))
                        'LIEMAT)))
           (SETQ HELP (AEVAL A1))
           (SETQ A1 (AEVAL A3))
           (SETQ A3 (AEVAL HELP))
           (SETQ HELP (AEVAL A2))
           (SETQ A2 (AEVAL A4))
           (SETQ A4 (AEVAL HELP))
           (SETQ HELP (AEVAL B1))
           (SETQ B1 (AEVAL B3))
           (SETQ B3 (AEVAL HELP))
           (SETQ HELP (AEVAL B2))
           (SETQ B2 (AEVAL B4))
           (SETQ B4 (AEVAL HELP))
           (SETQ A5 (AEVAL (LIST 'MINUS A5)))
           (SETQ B5 (AEVAL (LIST 'MINUS B5))))))))
      (COND
       ((EVALNEQ (AEVAL A2) 0)
        (PROGN
         (SETQ ALPHA (AEVAL (LIST 'PLUS A1 B2)))
         (SETQ BETA
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'TIMES A2 B1) (LIST 'TIMES A1 B2))))
         (COND
          ((EVALEQUAL (AEVAL ALPHA) 0)
           (PROGN
            (SETQ C1 (AEVAL 0))
            (SETQ C2
                    (AEVAL
                     (LIST 'DIFFERENCE B1
                           (LIST 'TIMES A1 (LIST 'QUOTIENT B2 A2)))))
            (SETQ C3 (AEVAL (LIST 'SQRT (LIST 'ABS BETA))))
            (SETQ C4
                    (AEVAL
                     (LIST 'MINUS (LIST 'TIMES C3 (LIST 'QUOTIENT A1 A2)))))
            (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C3)))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT A1 (LIST 'TIMES A2 C2))))
            (SETQ D2 (AEVAL C5))
            (SETQ D3 (AEVAL (LIST 'QUOTIENT 1 C2)))
            (SETQ D4 (AEVAL 0))
            (SETQ D5 (AEVAL C3))
            (COND
             ((NOT (EVALNUMBERP (AEVAL BETA)))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                (ASSGNPRI (AEVAL BETA) NIL NIL)
                (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
               (SETQ HE (AEVAL (READ)))
               (COND ((EVALEQUAL (AEVAL HE) (AEVAL 'Y)) (SETQ FLAG (AEVAL 2)))
                     (T (SETQ FLAG (AEVAL 3))))))
             ((EVALGREATERP (AEVAL BETA) 0) (SETQ FLAG (AEVAL 2)))
             (T (SETQ FLAG (AEVAL 3))))))
          (T
           (PROGN
            (SETQ C1 (AEVAL 0))
            (SETQ C2
                    (AEVAL
                     (LIST 'DIFFERENCE B1
                           (LIST 'TIMES A1 (LIST 'QUOTIENT B2 A2)))))
            (SETQ C3 (AEVAL (LIST 'MINUS ALPHA)))
            (SETQ C4 (AEVAL (LIST 'TIMES ALPHA (LIST 'QUOTIENT A1 A2))))
            (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C3)))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT A1 (LIST 'TIMES A2 C2))))
            (SETQ D2 (AEVAL C5))
            (SETQ D3 (AEVAL (LIST 'QUOTIENT 1 C2)))
            (SETQ D4 (AEVAL 0))
            (SETQ D5 (AEVAL C3))
            (SETQ FLAG (AEVAL 4))
            (SETQ P
                    (AEVAL
                     (LIST 'QUOTIENT BETA (LIST 'TIMES ALPHA ALPHA)))))))))
       ((EVALNEQ (AEVAL B1) 0)
        (PROGN
         (SETQ ALPHA (AEVAL (LIST 'PLUS 1 (LIST 'QUOTIENT A1 B2))))
         (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'QUOTIENT A1 B2))))
         (COND
          ((EVALEQUAL (AEVAL ALPHA) 0)
           (PROGN
            (SETQ C1
                    (AEVAL
                     (LIST 'MINUS (LIST 'TIMES A1 (LIST 'QUOTIENT B2 B1)))))
            (SETQ C2 (AEVAL 0))
            (SETQ C3
                    (AEVAL
                     (LIST 'MINUS
                           (LIST 'TIMES (LIST 'SQRT (LIST 'ABS BETA))
                                 (LIST 'QUOTIENT B2 B1)))))
            (SETQ C4 (AEVAL (LIST 'MINUS (LIST 'TIMES C3 B1))))
            (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C4)))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 C1)))
            (SETQ D2 (AEVAL 0))
            (SETQ D3
                    (AEVAL
                     (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 B2)))))
            (SETQ D4 (AEVAL C5))
            (SETQ D5 (AEVAL C4))
            (COND
             ((NOT (EVALNUMBERP (AEVAL BETA)))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                (ASSGNPRI (AEVAL BETA) NIL NIL)
                (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
               (SETQ HE (AEVAL (READ)))
               (COND ((EVALEQUAL (AEVAL HE) (AEVAL 'Y)) (SETQ FLAG (AEVAL 2)))
                     (T (SETQ FLAG (AEVAL 3))))))
             ((EVALGREATERP (AEVAL BETA) 0) (SETQ FLAG (AEVAL 2)))
             (T (SETQ FLAG (AEVAL 3))))))
          (T
           (PROGN
            (SETQ C1
                    (AEVAL
                     (LIST 'MINUS (LIST 'TIMES A1 (LIST 'QUOTIENT B2 B1)))))
            (SETQ C2 (AEVAL 0))
            (SETQ C3 (AEVAL (LIST 'TIMES ALPHA (LIST 'QUOTIENT B2 B1))))
            (SETQ C4 (AEVAL (LIST 'MINUS (LIST 'TIMES ALPHA B2))))
            (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C4)))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 C1)))
            (SETQ D2 (AEVAL 0))
            (SETQ D3
                    (AEVAL
                     (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 B2)))))
            (SETQ D4 (AEVAL C5))
            (SETQ D5 (AEVAL C4))
            (SETQ FLAG (AEVAL 4))
            (SETQ P
                    (AEVAL
                     (LIST 'QUOTIENT BETA (LIST 'TIMES ALPHA ALPHA)))))))))
       ((EVALNEQ (AEVAL A1) (AEVAL B2))
        (PROGN
         (SETQ ALPHA (AEVAL (LIST 'PLUS A1 B2)))
         (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'TIMES A1 B2))))
         (COND
          ((EVALEQUAL (AEVAL ALPHA) 0)
           (PROGN
            (SETQ C1 (AEVAL 1))
            (SETQ C2 (AEVAL 1))
            (SETQ C3 (AEVAL (LIST 'QUOTIENT (LIST 'SQRT (LIST 'ABS BETA)) A1)))
            (SETQ C4 (AEVAL (LIST 'QUOTIENT (LIST 'SQRT (LIST 'ABS BETA)) B2)))
            (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS BETA)))))
            (SETQ HELP
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 B2)
                           (LIST 'QUOTIENT 1 A1))))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES B2 HELP))))
            (SETQ D2 (AEVAL (LIST 'MINUS (LIST 'QUOTIENT C5 HELP))))
            (SETQ D3
                    (AEVAL
                     (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 HELP)))))
            (SETQ D4 (AEVAL (LIST 'MINUS D2)))
            (SETQ D5 (AEVAL (LIST 'QUOTIENT 1 C5)))
            (COND
             ((NOT (EVALNUMBERP (AEVAL BETA)))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                (ASSGNPRI (AEVAL BETA) NIL NIL)
                (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
               (SETQ HE (AEVAL (READ)))
               (COND ((EVALEQUAL (AEVAL HE) (AEVAL 'Y)) (SETQ FLAG (AEVAL 2)))
                     (T (SETQ FLAG (AEVAL 3))))))
             ((EVALGREATERP (AEVAL BETA) 0) (SETQ FLAG (AEVAL 2)))
             (T (SETQ FLAG (AEVAL 3))))))
          (T
           (PROGN
            (SETQ C1 (AEVAL 1))
            (SETQ C2 (AEVAL 1))
            (SETQ C3 (AEVAL (LIST 'MINUS (LIST 'QUOTIENT ALPHA A1))))
            (SETQ C4 (AEVAL (LIST 'MINUS (LIST 'QUOTIENT ALPHA B2))))
            (SETQ C5 (AEVAL (LIST 'MINUS (LIST 'QUOTIENT 1 ALPHA))))
            (SETQ HELP
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 B2)
                           (LIST 'QUOTIENT 1 A1))))
            (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES B2 HELP))))
            (SETQ D2 (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES ALPHA HELP))))
            (SETQ D3
                    (AEVAL
                     (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 HELP)))))
            (SETQ D4 (AEVAL (LIST 'MINUS D2)))
            (SETQ D5 (AEVAL (LIST 'MINUS ALPHA)))
            (SETQ FLAG (AEVAL 4))
            (SETQ P
                    (AEVAL
                     (LIST 'QUOTIENT BETA (LIST 'TIMES ALPHA ALPHA)))))))))
       (T
        (PROGN
         (SETQ C1 (AEVAL 1))
         (SETQ C2 (AEVAL 0))
         (SETQ C3 (AEVAL 0))
         (SETQ C4 (AEVAL 1))
         (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 A1)))
         (SETQ D1 (AEVAL 1))
         (SETQ D2 (AEVAL 0))
         (SETQ D3 (AEVAL 0))
         (SETQ D4 (AEVAL 1))
         (SETQ D5 (AEVAL A1))
         (SETQ FLAG (AEVAL 1)))))
      (SETK 'LIEMAT
            (AEVAL
             (LIST 'TIMES
                   (LIST 'MAT (LIST C1 C2 0 0) (LIST C3 C4 0 0) (LIST 0 0 C5 0)
                         (LIST 0 0 0 1))
                   'LIEMAT)))
      (SETQ E1
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES D1
                           (LIST 'PLUS (LIST 'TIMES C1 A3)
                                 (LIST 'TIMES C2 A4)))
                     (LIST 'TIMES D3
                           (LIST 'PLUS (LIST 'TIMES C1 B3)
                                 (LIST 'TIMES C2 B4))))))
      (SETQ E2
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES D2
                           (LIST 'PLUS (LIST 'TIMES C1 A3)
                                 (LIST 'TIMES C2 A4)))
                     (LIST 'TIMES D4
                           (LIST 'PLUS (LIST 'TIMES C1 B3)
                                 (LIST 'TIMES C2 B4))))))
      (SETQ E3
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES D1
                           (LIST 'PLUS (LIST 'TIMES C3 A3)
                                 (LIST 'TIMES C4 A4)))
                     (LIST 'TIMES D3
                           (LIST 'PLUS (LIST 'TIMES C3 B3)
                                 (LIST 'TIMES C4 B4))))))
      (SETQ E4
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES D2
                           (LIST 'PLUS (LIST 'TIMES C3 A3)
                                 (LIST 'TIMES C4 A4)))
                     (LIST 'TIMES D4
                           (LIST 'PLUS (LIST 'TIMES C3 B3)
                                 (LIST 'TIMES C4 B4))))))
      (SETQ E5
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES C5 A5 D1) (LIST 'TIMES C5 B5 D3))))
      (SETQ E6
              (AEVAL
               (LIST 'PLUS (LIST 'TIMES C5 A5 D2) (LIST 'TIMES C5 B5 D4))))
      (COND
       ((EQUAL FLAG 4)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST 0 0 (LIST 'PLUS E1 E4) 1) (LIST 0 0 1 0))
                      'LIEMAT)))
         (SETQ A1 (AEVAL (LIST 'MINUS E4)))
         (SETQ A2 (AEVAL (LIST 'PLUS E1 E3 E4)))
         (SETQ A3 (AEVAL (MINUS 1)))
         (SETQ A4 (AEVAL 1))
         (SETQ A5 (AEVAL (LIST 'MINUS E5)))
         (SETQ B1 (AEVAL (LIST 'PLUS (LIST 'TIMES P (LIST 'PLUS E1 E4)) E2)))
         (SETQ B2 (AEVAL E4))
         (SETQ B3 (AEVAL P))
         (SETQ B4 (AEVAL 0))
         (SETQ B5 (AEVAL (LIST 'MINUS E6)))))
       ((EQUAL FLAG 1)
        (COND
         ((EVALEQUAL (AEVAL (LIST 'PLUS E1 E4)) 0)
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 0 1)
                              (LIST 0 0 1 0))
                        'LIEMAT)))
           (SETQ A1 (AEVAL E1))
           (SETQ A2 (AEVAL E3))
           (SETQ A3 (AEVAL 1))
           (SETQ A4 (AEVAL 0))
           (SETQ A5 (AEVAL (LIST 'MINUS E5)))
           (SETQ B1 (AEVAL E2))
           (SETQ B2 (AEVAL E4))
           (SETQ B3 (AEVAL 0))
           (SETQ B4 (AEVAL 1))
           (SETQ B5 (AEVAL (LIST 'MINUS E6)))))
         (T
          (PROGN
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                              (LIST 0 0 (LIST 'PLUS E1 E4) (MINUS 2))
                              (LIST 0 0 0 1))
                        'LIEMAT)))
           (SETQ A1 (AEVAL (LIST 'DIFFERENCE E4 E1)))
           (SETQ A2 (AEVAL (LIST 'MINUS (LIST 'TIMES 2 E3))))
           (SETQ A3 (AEVAL E1))
           (SETQ A4 (AEVAL E3))
           (SETQ A5 (AEVAL (LIST 'TIMES E5 (LIST 'PLUS E1 E4))))
           (SETQ B1 (AEVAL (LIST 'MINUS (LIST 'TIMES 2 E2))))
           (SETQ B2 (AEVAL (LIST 'DIFFERENCE E1 E4)))
           (SETQ B3 (AEVAL E2))
           (SETQ B4 (AEVAL E4))
           (SETQ B5 (AEVAL (LIST 'TIMES E6 (LIST 'PLUS E1 E4)))))))))
      (COND
       ((OR (EQUAL FLAG 1) (EQUAL FLAG 4))
        (COND
         ((EVALEQUAL
           (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES A1 B2) (LIST 'TIMES A2 B1)))
           0)
          (COND
           ((EVALEQUAL (AEVAL B1) 0)
            (PROGN
             (SETK 'LIEMAT
                   (AEVAL
                    (LIST 'TIMES
                          (LIST 'MAT (LIST A2 0 0 0) (LIST 0 1 0 0)
                                (LIST 0 0 1 0) (LIST 0 0 0 1))
                          'LIEMAT)))
             (SETQ FLAG (AEVAL 5))
             (SETQ E1 (AEVAL A3))
             (SETQ E2 (AEVAL (LIST 'TIMES B3 A2)))
             (SETQ E3 (AEVAL (LIST 'QUOTIENT A4 A2)))
             (SETQ E4 (AEVAL B4))
             (SETQ E5 (AEVAL (LIST 'QUOTIENT A5 A2)))
             (SETQ E6 (AEVAL B5))))
           (T
            (PROGN
             (SETK 'LIEMAT
                   (AEVAL
                    (LIST 'TIMES
                          (LIST 'MAT (LIST A1 B1 0 0) (LIST 1 0 0 0)
                                (LIST 0 0 1 0) (LIST 0 0 0 1))
                          'LIEMAT)))
             (SETQ FLAG (AEVAL 5))
             (SETQ E1
                     (AEVAL
                      (LIST 'QUOTIENT
                            (LIST 'PLUS (LIST 'TIMES A1 B3)
                                  (LIST 'TIMES B1 B4))
                            B1)))
             (SETQ E2
                     (AEVAL
                      (LIST 'PLUS (LIST 'TIMES A1 A3)
                            (LIST 'DIFFERENCE (LIST 'TIMES B1 A4)
                                  (LIST 'TIMES A1
                                        (LIST 'QUOTIENT
                                              (LIST 'PLUS (LIST 'TIMES A1 B3)
                                                    (LIST 'TIMES B1 B4))
                                              B1))))))
             (SETQ E3 (AEVAL (LIST 'QUOTIENT B3 B1)))
             (SETQ E4
                     (AEVAL
                      (LIST 'DIFFERENCE A3
                            (LIST 'TIMES A1 (LIST 'QUOTIENT B3 B1)))))
             (SETQ E5 (AEVAL (LIST 'QUOTIENT B5 B1)))
             (SETQ E6
                     (AEVAL
                      (LIST 'DIFFERENCE A5
                            (LIST 'TIMES B5 (LIST 'QUOTIENT A1 B1)))))))))
         (T
          (PROGN
           (COND
            ((EVALNEQ (AEVAL A2) 0)
             (PROGN
              (SETQ BETA
                      (AEVAL
                       (LIST 'DIFFERENCE (LIST 'TIMES A2 B1)
                             (LIST 'TIMES A1 B2))))
              (SETQ C1 (AEVAL 0))
              (SETQ C2
                      (AEVAL
                       (LIST 'DIFFERENCE B1
                             (LIST 'TIMES A1 (LIST 'QUOTIENT B2 A2)))))
              (SETQ C3 (AEVAL (LIST 'SQRT (LIST 'ABS BETA))))
              (SETQ C4
                      (AEVAL
                       (LIST 'MINUS (LIST 'TIMES C3 (LIST 'QUOTIENT A1 A2)))))
              (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C3)))
              (SETQ D1 (AEVAL (LIST 'QUOTIENT A1 (LIST 'TIMES A2 C2))))
              (SETQ D2 (AEVAL C5))
              (SETQ D3 (AEVAL (LIST 'QUOTIENT 1 C2)))
              (SETQ D4 (AEVAL 0))
              (SETQ D5 (AEVAL C3))))
            ((EVALNEQ (AEVAL B1) 0)
             (PROGN
              (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'QUOTIENT A1 B2))))
              (SETQ C1
                      (AEVAL
                       (LIST 'MINUS (LIST 'TIMES A1 (LIST 'QUOTIENT B2 B1)))))
              (SETQ C2 (AEVAL 0))
              (SETQ C3
                      (AEVAL
                       (LIST 'MINUS
                             (LIST 'TIMES (LIST 'SQRT (LIST 'ABS BETA))
                                   (LIST 'QUOTIENT B2 B1)))))
              (SETQ C4 (AEVAL (LIST 'MINUS (LIST 'TIMES C3 B1))))
              (SETQ C5 (AEVAL (LIST 'QUOTIENT 1 C4)))
              (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 C1)))
              (SETQ D2 (AEVAL 0))
              (SETQ D3
                      (AEVAL
                       (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 B2)))))
              (SETQ D4 (AEVAL C5))
              (SETQ D5 (AEVAL C4))))
            (T
             (PROGN
              (SETQ BETA (AEVAL (LIST 'MINUS (LIST 'TIMES A1 B2))))
              (SETQ C1 (AEVAL 1))
              (SETQ C2 (AEVAL 1))
              (SETQ C3
                      (AEVAL
                       (LIST 'QUOTIENT (LIST 'SQRT (LIST 'ABS BETA)) A1)))
              (SETQ C4
                      (AEVAL
                       (LIST 'QUOTIENT (LIST 'SQRT (LIST 'ABS BETA)) B2)))
              (SETQ C5
                      (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS BETA)))))
              (SETQ HELP
                      (AEVAL
                       (LIST 'DIFFERENCE (LIST 'QUOTIENT 1 B2)
                             (LIST 'QUOTIENT 1 A1))))
              (SETQ D1 (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES B2 HELP))))
              (SETQ D2 (AEVAL (LIST 'MINUS (LIST 'QUOTIENT C5 HELP))))
              (SETQ D3
                      (AEVAL
                       (LIST 'MINUS (LIST 'QUOTIENT 1 (LIST 'TIMES A1 HELP)))))
              (SETQ D4 (AEVAL (LIST 'MINUS D2)))
              (SETQ D5 (AEVAL (LIST 'QUOTIENT 1 C5))))))
           (COND
            ((NOT (EVALNUMBERP (AEVAL BETA)))
             (PROGN
              (PROGN
               (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
               (ASSGNPRI (AEVAL BETA) NIL NIL)
               (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
              (SETQ HE (AEVAL (READ)))
              (COND ((EVALEQUAL (AEVAL HE) (AEVAL 'Y)) (SETQ FLAG (AEVAL 2)))
                    (T (SETQ FLAG (AEVAL 3))))))
            ((EVALGREATERP (AEVAL BETA) 0) (SETQ FLAG (AEVAL 2)))
            (T (SETQ FLAG (AEVAL 3))))
           (SETK 'LIEMAT
                 (AEVAL
                  (LIST 'TIMES
                        (LIST 'MAT (LIST C1 C2 0 0) (LIST C3 C4 0 0)
                              (LIST 0 0 C5 0) (LIST 0 0 0 1))
                        'LIEMAT)))
           (SETQ E1
                   (AEVAL
                    (LIST 'PLUS
                          (LIST 'TIMES D1
                                (LIST 'PLUS (LIST 'TIMES C1 A3)
                                      (LIST 'TIMES C2 A4)))
                          (LIST 'TIMES D3
                                (LIST 'PLUS (LIST 'TIMES C1 B3)
                                      (LIST 'TIMES C2 B4))))))
           (SETQ E2
                   (AEVAL
                    (LIST 'PLUS
                          (LIST 'TIMES D2
                                (LIST 'PLUS (LIST 'TIMES C1 A3)
                                      (LIST 'TIMES C2 A4)))
                          (LIST 'TIMES D4
                                (LIST 'PLUS (LIST 'TIMES C1 B3)
                                      (LIST 'TIMES C2 B4))))))
           (SETQ E3
                   (AEVAL
                    (LIST 'PLUS
                          (LIST 'TIMES D1
                                (LIST 'PLUS (LIST 'TIMES C3 A3)
                                      (LIST 'TIMES C4 A4)))
                          (LIST 'TIMES D3
                                (LIST 'PLUS (LIST 'TIMES C3 B3)
                                      (LIST 'TIMES C4 B4))))))
           (SETQ E4
                   (AEVAL
                    (LIST 'PLUS
                          (LIST 'TIMES D2
                                (LIST 'PLUS (LIST 'TIMES C3 A3)
                                      (LIST 'TIMES C4 A4)))
                          (LIST 'TIMES D4
                                (LIST 'PLUS (LIST 'TIMES C3 B3)
                                      (LIST 'TIMES C4 B4))))))
           (SETQ E5
                   (AEVAL
                    (LIST 'PLUS (LIST 'TIMES C5 A5 D1)
                          (LIST 'TIMES C5 B5 D3))))
           (SETQ E6
                   (AEVAL
                    (LIST 'PLUS (LIST 'TIMES C5 A5 D2)
                          (LIST 'TIMES C5 B5 D4)))))))))
      (COND
       ((EQUAL FLAG 2)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST (LIST 'MINUS (LIST 'QUOTIENT E5 E1))
                                  (LIST 'MINUS (LIST 'QUOTIENT E6 E1)) 1 0)
                            (LIST 0 0 (LIST 'MINUS (LIST 'QUOTIENT E2 E1))
                                  (LIST 'QUOTIENT 1 E1)))
                      'LIEMAT)))
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT
                            (LIST (LIST 'QUOTIENT 1 2) (LIST 'QUOTIENT 1 2) 0
                                  0)
                            (LIST (LIST 'QUOTIENT 1 2)
                                  (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 0 0)
                            (LIST 0 0 (LIST 'QUOTIENT 1 2)
                                  (LIST 'QUOTIENT 1 2))
                            (LIST 0 0 (LIST 'MINUS (LIST 'QUOTIENT 1 2))
                                  (LIST 'QUOTIENT 1 2)))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[W,Y]=W, [X,Z]=X") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 3))))))
       ((EQUAL FLAG 3)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST (LIST 'MINUS (LIST 'QUOTIENT E5 E1))
                                  (LIST 'MINUS (LIST 'QUOTIENT E6 E1)) 1 0)
                            (LIST 0 0 (LIST 'QUOTIENT E2 E1)
                                  (LIST 'QUOTIENT 1 E1)))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "-[W,Y]=[X,Z]=X, [X,Y]=[W,Z]=W") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 4))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                            (LIST (LIST 'MINUS (LIST 'QUOTIENT E5 E1))
                                  (LIST 'MINUS (LIST 'QUOTIENT E6 E1)) 1 0)
                            (LIST 0 0 (LIST 'MINUS (LIST 'QUOTIENT E3 E1))
                                  (LIST 'QUOTIENT 1 E1)))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[X,Y]=[W,Z]=W, [X,Z]=X") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 5))))))))) 
(PUT 'COM43 'NUMBER-OF-ARGS 6) 
(FLAG '(COM43) 'OPFN) 
(PUT 'COM43 'DEFINED-ON-LINE '846) 
(PUT 'COM43 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COM43 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE COM43 (I1 J1 I2 J2 I3 J3)
    (PROG (LL)
      (SETQ LL 0)
      (AEVAL (MATRIX (LIST (LIST 'LIEMAT 4 4) (LIST 'BB 4 4) (LIST 'FF 3 3))))
      (ARRAYFN 'ALGEBRAIC (LIST (LIST 'L_Z 4 4 3)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'CC 2 1 I) (AEVAL* (LIST 'MINUS (LIST 'CC 1 2 I))))
         (SETK (LIST 'CC 3 1 I) (AEVAL* (LIST 'MINUS (LIST 'CC 1 3 I))))
         (SETK (LIST 'CC 3 2 I) (AEVAL* (LIST 'MINUS (LIST 'CC 2 3 I))))
         (SETK (LIST 'CC 4 1 I) (AEVAL* (LIST 'MINUS (LIST 'CC 1 4 I))))
         (SETK (LIST 'CC 4 2 I) (AEVAL* (LIST 'MINUS (LIST 'CC 2 4 I))))
         (SETK (LIST 'CC 4 3 I) (AEVAL* (LIST 'MINUS (LIST 'CC 3 4 I))))
         (SETK (LIST 'CC 1 1 I)
               (SETK (LIST 'CC 2 2 I)
                     (SETK (LIST 'CC 3 3 I)
                           (SETK (LIST 'CC 4 4 I) (AEVAL* 0)))))
         (SETK (LIST 'LIEMAT 1 I) (AEVAL* (LIST 'CC I1 J1 I)))
         (SETK (LIST 'LIEMAT 2 I) (AEVAL* (LIST 'CC I2 J2 I)))
         (SETK (LIST 'LIEMAT 3 I) (AEVAL* (LIST 'CC I3 J3 I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETK (LIST 'LIEMAT 4 1) (AEVAL 1))
      (COND ((EVALNEQ (AEVAL (LIST 'DET 'LIEMAT)) 0) (SETQ LL (AEVAL 1)))
            (T
             (PROG (J)
               (SETQ J 2)
              LAB
               (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN NIL)))
               (PROGN
                (SETK (LIST 'LIEMAT 4 (DIFFERENCE J 1)) (AEVAL* 0))
                (SETK (LIST 'LIEMAT 4 J) (AEVAL* 1))
                (COND
                 ((EVALNEQ (AEVAL* (LIST 'DET 'LIEMAT)) 0)
                  (PROGN (SETQ LL (AEVAL* J)) (SETQ J (AEVAL* 4))))))
               (SETQ J (PLUS2 J 1))
               (GO LAB))))
      (SETK 'BB (AEVAL (LIST 'QUOTIENT 1 'LIEMAT)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'L_Z 1 2 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (S FORALL-RESULT)
                                  (SETQ S 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 S))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (PROG (TT FORALL-RESULT)
                                                   (SETQ TT 1)
                                                   (SETQ FORALL-RESULT 0)
                                                  LAB1
                                                   (COND
                                                    ((MINUSP (DIFFERENCE 4 TT))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'PLUS
                                                                  (AEVAL*
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'LIEMAT
                                                                          1 R)
                                                                         (LIST
                                                                          'LIEMAT
                                                                          2 S)
                                                                         (LIST
                                                                          'CC R
                                                                          S TT)
                                                                         (LIST
                                                                          'BB
                                                                          TT
                                                                          I)))
                                                                  FORALL-RESULT)))
                                                   (SETQ TT (PLUS2 TT 1))
                                                   (GO LAB1))
                                                 FORALL-RESULT)))
                                  (SETQ S (PLUS2 S 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1)))
         (SETK (LIST 'L_Z 1 3 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (S FORALL-RESULT)
                                  (SETQ S 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 S))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (PROG (TT FORALL-RESULT)
                                                   (SETQ TT 1)
                                                   (SETQ FORALL-RESULT 0)
                                                  LAB1
                                                   (COND
                                                    ((MINUSP (DIFFERENCE 4 TT))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'PLUS
                                                                  (AEVAL*
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'LIEMAT
                                                                          1 R)
                                                                         (LIST
                                                                          'LIEMAT
                                                                          3 S)
                                                                         (LIST
                                                                          'CC R
                                                                          S TT)
                                                                         (LIST
                                                                          'BB
                                                                          TT
                                                                          I)))
                                                                  FORALL-RESULT)))
                                                   (SETQ TT (PLUS2 TT 1))
                                                   (GO LAB1))
                                                 FORALL-RESULT)))
                                  (SETQ S (PLUS2 S 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1)))
         (SETK (LIST 'L_Z 2 3 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (S FORALL-RESULT)
                                  (SETQ S 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 S))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (PROG (TT FORALL-RESULT)
                                                   (SETQ TT 1)
                                                   (SETQ FORALL-RESULT 0)
                                                  LAB1
                                                   (COND
                                                    ((MINUSP (DIFFERENCE 4 TT))
                                                     (RETURN FORALL-RESULT)))
                                                   (SETQ FORALL-RESULT
                                                           (AEVAL*
                                                            (LIST 'PLUS
                                                                  (AEVAL*
                                                                   (LIST 'TIMES
                                                                         (LIST
                                                                          'LIEMAT
                                                                          2 R)
                                                                         (LIST
                                                                          'LIEMAT
                                                                          3 S)
                                                                         (LIST
                                                                          'CC R
                                                                          S TT)
                                                                         (LIST
                                                                          'BB
                                                                          TT
                                                                          I)))
                                                                  FORALL-RESULT)))
                                                   (SETQ TT (PLUS2 TT 1))
                                                   (GO LAB1))
                                                 FORALL-RESULT)))
                                  (SETQ S (PLUS2 S 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1)))
         (SETK (LIST 'L_Z 1 4 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (TT FORALL-RESULT)
                                  (SETQ TT 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 TT))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (AEVAL*
                                                  (LIST 'TIMES
                                                        (LIST 'LIEMAT 1 R)
                                                        (LIST 'CC R LL TT)
                                                        (LIST 'BB TT I)))
                                                 FORALL-RESULT)))
                                  (SETQ TT (PLUS2 TT 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1)))
         (SETK (LIST 'L_Z 2 4 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (TT FORALL-RESULT)
                                  (SETQ TT 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 TT))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (AEVAL*
                                                  (LIST 'TIMES
                                                        (LIST 'LIEMAT 2 R)
                                                        (LIST 'CC R LL TT)
                                                        (LIST 'BB TT I)))
                                                 FORALL-RESULT)))
                                  (SETQ TT (PLUS2 TT 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1)))
         (SETK (LIST 'L_Z 3 4 I)
               (PROG (R FORALL-RESULT)
                 (SETQ R 1)
                 (SETQ FORALL-RESULT 0)
                LAB1
                 (COND ((MINUSP (DIFFERENCE 4 R)) (RETURN FORALL-RESULT)))
                 (SETQ FORALL-RESULT
                         (AEVAL*
                          (LIST 'PLUS
                                (PROG (TT FORALL-RESULT)
                                  (SETQ TT 1)
                                  (SETQ FORALL-RESULT 0)
                                 LAB1
                                  (COND
                                   ((MINUSP (DIFFERENCE 4 TT))
                                    (RETURN FORALL-RESULT)))
                                  (SETQ FORALL-RESULT
                                          (AEVAL*
                                           (LIST 'PLUS
                                                 (AEVAL*
                                                  (LIST 'TIMES
                                                        (LIST 'LIEMAT 3 R)
                                                        (LIST 'CC R LL TT)
                                                        (LIST 'BB TT I)))
                                                 FORALL-RESULT)))
                                  (SETQ TT (PLUS2 TT 1))
                                  (GO LAB1))
                                FORALL-RESULT)))
                 (SETQ R (PLUS2 R 1))
                 (GO LAB1))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
        (PROGN
         (SETK (LIST 'FF 1 I) (AEVAL* (LIST 'L_Z 1 2 I)))
         (SETK (LIST 'FF 2 I) (AEVAL* (LIST 'L_Z 1 3 I)))
         (SETK (LIST 'FF 3 I) (AEVAL* (LIST 'L_Z 2 3 I))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ LL (AEVAL 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 I)) (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 3 J)) (RETURN NIL)))
          (COND
           ((EVALNEQ (AEVAL* (LIST 'FF I J)) 0)
            (PROGN
             (SETQ LL (AEVAL* 1))
             (SETQ I (AEVAL* 3))
             (SETQ J (AEVAL* 3)))))
          (SETQ J (PLUS2 J 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND ((EQUAL LL 0) (AEVAL (LIST 'COMCOM0)))
            ((EVALEQUAL (AEVAL (LIST 'DET 'FF)) 0) (AEVAL (LIST 'COMCOM1)))
            (T (AEVAL (LIST 'COMCOM3))))
      (AEVAL (CLEAR (LIST 'BB 'FF 'L_Z))))) 
(PUT 'COMCOM0 'NUMBER-OF-ARGS 0) 
(FLAG '(COMCOM0) 'OPFN) 
(PUT 'COMCOM0 'DEFINED-ON-LINE '884) 
(PUT 'COMCOM0 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COMCOM0 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COMCOM0 NIL
    (PROG (HE A1 B1 C1 A2 B2 C2 A3 B3 C3 AA1 BB1 CC1 AA2 BB2 CC2 AL1 BE1 GA1
           AL2 BE2 GA2 R S P Q)
      (SETQ A1 (AEVAL (LIST 'L_Z 1 4 1)))
      (SETQ B1 (AEVAL (LIST 'L_Z 1 4 2)))
      (SETQ C1 (AEVAL (LIST 'L_Z 1 4 3)))
      (SETQ A2 (AEVAL (LIST 'L_Z 2 4 1)))
      (SETQ B2 (AEVAL (LIST 'L_Z 2 4 2)))
      (SETQ C2 (AEVAL (LIST 'L_Z 2 4 3)))
      (SETQ A3 (AEVAL (LIST 'L_Z 3 4 1)))
      (SETQ B3 (AEVAL (LIST 'L_Z 3 4 2)))
      (SETQ C3 (AEVAL (LIST 'L_Z 3 4 3)))
      (COND
       ((AND (EVALEQUAL (AEVAL A3) 0) (EVALEQUAL (AEVAL B3) 0))
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0 (LIST 'QUOTIENT 1 C3)))
                      'LIEMAT)))
         (SETQ AL1 (AEVAL (LIST 'QUOTIENT A1 C3)))
         (SETQ BE1 (AEVAL (LIST 'QUOTIENT B1 C3)))
         (SETQ GA1 (AEVAL (LIST 'QUOTIENT C1 C3)))
         (SETQ AL2 (AEVAL (LIST 'QUOTIENT A2 C3)))
         (SETQ BE2 (AEVAL (LIST 'QUOTIENT B2 C3)))
         (SETQ GA2 (AEVAL (LIST 'QUOTIENT C2 C3)))))
       (T
        (PROGN
         (COND
          ((AND (EVALEQUAL (AEVAL A3) 0) (EVALNEQ (AEVAL B3) 0))
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 0 B3 C3 0) (LIST 1 0 0 0)
                               (LIST 0 0 1 0) (LIST 0 0 0 1))
                         'LIEMAT)))
            (SETQ AA1 (AEVAL (LIST 'PLUS B2 C3)))
            (SETQ BB1 (AEVAL (LIST 'TIMES B3 A2)))
            (SETQ CC1
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'TIMES B3 C2)
                           (LIST 'TIMES B2 C3))))
            (SETQ AA2 (AEVAL (LIST 'QUOTIENT B1 B3)))
            (SETQ BB2 (AEVAL A1))
            (SETQ CC2
                    (AEVAL
                     (LIST 'DIFFERENCE C1
                           (LIST 'TIMES B1 (LIST 'QUOTIENT C3 B3)))))))
          (T
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST A3 B3 C3 0) (LIST 0 1 0 0)
                               (LIST 0 0 1 0) (LIST 0 0 0 1))
                         'LIEMAT)))
            (SETQ AA1
                    (AEVAL
                     (LIST 'PLUS A1 (LIST 'TIMES B3 (LIST 'QUOTIENT A2 A3))
                           C3)))
            (SETQ BB1
                    (AEVAL
                     (LIST 'PLUS
                           (LIST 'DIFFERENCE
                                 (LIST 'DIFFERENCE (LIST 'TIMES A3 B1)
                                       (LIST 'TIMES A1 B3))
                                 (LIST 'TIMES B3 B3 (LIST 'QUOTIENT A2 A3)))
                           (LIST 'TIMES B3 B2))))
            (SETQ CC1
                    (AEVAL
                     (LIST 'PLUS
                           (LIST 'DIFFERENCE
                                 (LIST 'DIFFERENCE (LIST 'TIMES A3 C1)
                                       (LIST 'TIMES A1 C3))
                                 (LIST 'TIMES B3 A2 (LIST 'QUOTIENT C3 A3)))
                           (LIST 'TIMES B3 C2))))
            (SETQ AA2 (AEVAL (LIST 'QUOTIENT A2 A3)))
            (SETQ BB2
                    (AEVAL
                     (LIST 'DIFFERENCE B2
                           (LIST 'TIMES A2 (LIST 'QUOTIENT B3 A3)))))
            (SETQ CC2
                    (AEVAL
                     (LIST 'DIFFERENCE C2
                           (LIST 'TIMES A2 (LIST 'QUOTIENT C3 A3))))))))
         (PROGN
          (SETK 'LIEMAT
                (AEVAL
                 (LIST 'TIMES
                       (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 (LIST 'MINUS AA2) 0)
                             (LIST 0 0 1 0) (LIST 0 0 0 1))
                       'LIEMAT)))
          (SETQ CC1 (AEVAL (LIST 'PLUS CC1 (LIST 'TIMES BB1 AA2))))
          (SETQ CC2 (AEVAL (LIST 'PLUS CC2 (LIST 'TIMES BB2 AA2))))
          (SETQ AA2 (AEVAL 0)))
         (COND
          ((AND (EVALEQUAL (AEVAL BB1) 0) (EVALEQUAL (AEVAL AA1) (AEVAL BB2))
                (EVALNEQ (AEVAL CC2) 0))
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 0 0 1 0) (LIST 0 1 0 0)
                               (LIST 1 (LIST 'MINUS (LIST 'QUOTIENT CC1 CC2)) 0
                                     0)
                               (LIST 0 0 0 (LIST 'QUOTIENT 1 AA1)))
                         'LIEMAT)))
            (SETQ AL1 (AEVAL 0))
            (SETQ BE1 (AEVAL (LIST 'QUOTIENT CC1 (LIST 'TIMES AA1 CC2))))
            (SETQ GA1 (AEVAL (LIST 'QUOTIENT 1 AA1)))
            (SETQ AL2 (AEVAL (LIST 'QUOTIENT CC2 AA1)))
            (SETQ BE2 (AEVAL 1))
            (SETQ GA2 (AEVAL 0))))
          ((AND (EVALEQUAL (AEVAL BB1) 0) (EVALNEQ (AEVAL AA1) (AEVAL BB2))
                (EVALNEQ (AEVAL CC2) 0))
           (PROGN
            (SETQ A1 (AEVAL (LIST 'QUOTIENT 1 (LIST 'DIFFERENCE BB2 AA1))))
            (SETQ B1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'PLUS
                                 (LIST 'DIFFERENCE (LIST 'TIMES BB2 AA1)
                                       (LIST 'TIMES BB2 BB2))
                                 CC1)
                           (LIST 'TIMES CC2 (LIST 'DIFFERENCE AA1 BB2)))))
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                               (LIST A1 B1 1 0)
                               (LIST 0 0 0 (LIST 'QUOTIENT 1 BB2)))
                         'LIEMAT)))
            (SETQ AL1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE AA1 (LIST 'TIMES CC1 A1)) BB2)))
            (SETQ BE1
                    (AEVAL
                     (LIST 'MINUS (LIST 'TIMES B1 (LIST 'QUOTIENT CC1 BB2)))))
            (SETQ GA1 (AEVAL (LIST 'QUOTIENT CC1 BB2)))
            (SETQ AL2
                    (AEVAL
                     (LIST 'MINUS (LIST 'TIMES CC2 (LIST 'QUOTIENT A1 BB2)))))
            (SETQ BE2
                    (AEVAL
                     (LIST 'DIFFERENCE 1
                           (LIST 'TIMES B1 (LIST 'QUOTIENT CC2 BB2)))))
            (SETQ GA2 (AEVAL (LIST 'QUOTIENT CC2 BB2)))))
          ((AND (EVALEQUAL (AEVAL BB1) 0) (EVALEQUAL (AEVAL CC2) 0))
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0) (LIST 0 0 1 0)
                               (LIST 0 1 0 0)
                               (LIST 0 0 0 (LIST 'QUOTIENT 1 BB2)))
                         'LIEMAT)))
            (SETQ AL1 (AEVAL (LIST 'QUOTIENT AA1 BB2)))
            (SETQ BE1 (AEVAL (LIST 'QUOTIENT CC1 BB2)))
            (SETQ AL2 (AEVAL (LIST 'QUOTIENT 1 BB2)))
            (SETQ GA1 (SETQ BE2 (SETQ GA2 (AEVAL 0))))))
          (T
           (PROGN
            (SETQ R (AEVAL (LIST 'DIFFERENCE (LIST 'MINUS AA1) BB2)))
            (SETQ S (AEVAL (LIST 'DIFFERENCE (LIST 'TIMES AA1 BB2) CC1)))
            (SETQ P
                    (AEVAL
                     (LIST 'DIFFERENCE S
                           (LIST 'TIMES R (LIST 'QUOTIENT R 3)))))
            (SETQ Q
                    (AEVAL
                     (LIST 'PLUS
                           (LIST 'DIFFERENCE
                                 (LIST 'TIMES 2 R R (LIST 'QUOTIENT R 27))
                                 (LIST 'TIMES S (LIST 'QUOTIENT R 3)))
                           (LIST 'DIFFERENCE (LIST 'TIMES BB2 CC1)
                                 (LIST 'TIMES BB1 CC2)))))
            (SETQ C1
                    (AEVAL
                     (LIST 'PLUS
                           (LIST 'EXPT
                                 (LIST 'PLUS (LIST 'MINUS (LIST 'QUOTIENT Q 2))
                                       (LIST 'SQRT
                                             (LIST 'PLUS
                                                   (LIST 'TIMES Q
                                                         (LIST 'QUOTIENT Q 4))
                                                   (LIST 'TIMES P P
                                                         (LIST 'QUOTIENT P
                                                               27)))))
                                 (LIST 'QUOTIENT 1 3))
                           (LIST 'DIFFERENCE
                                 (LIST 'EXPT
                                       (LIST 'DIFFERENCE
                                             (LIST 'MINUS (LIST 'QUOTIENT Q 2))
                                             (LIST 'SQRT
                                                   (LIST 'PLUS
                                                         (LIST 'TIMES Q
                                                               (LIST 'QUOTIENT
                                                                     Q 4))
                                                         (LIST 'TIMES P P
                                                               (LIST 'QUOTIENT
                                                                     P 27)))))
                                       (LIST 'QUOTIENT 1 3))
                                 (LIST 'QUOTIENT R 3)))))
            (SETQ A1 (AEVAL (LIST 'QUOTIENT (LIST 'DIFFERENCE C1 BB2) BB1)))
            (SETQ B1
                    (AEVAL
                     (LIST 'TIMES (LIST 'DIFFERENCE C1 BB2)
                           (LIST 'QUOTIENT (LIST 'DIFFERENCE C1 AA1) BB1))))
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0) (LIST 0 0 1 0)
                               (LIST A1 1 B1 0)
                               (LIST 0 0 0 (LIST 'QUOTIENT 1 C1)))
                         'LIEMAT)))
            (SETQ AL1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE AA1 (LIST 'TIMES A1 BB1)) C1)))
            (SETQ BE1
                    (AEVAL
                     (LIST 'QUOTIENT
                           (LIST 'DIFFERENCE CC1 (LIST 'TIMES B1 BB1)) C1)))
            (SETQ GA1 (AEVAL (LIST 'QUOTIENT BB1 C1)))
            (SETQ AL2 (AEVAL (LIST 'QUOTIENT 1 C1)))
            (SETQ BE2 (SETQ GA2 (AEVAL 0)))))))))
      (COND
       ((EVALNEQ (AEVAL GA2) 0)
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT
                            (LIST 1 (LIST 'MINUS (LIST 'QUOTIENT GA1 GA2)) 0 0)
                            (LIST 0 1 0 0) (LIST 0 0 1 0) (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ AA1
                 (AEVAL
                  (LIST 'DIFFERENCE AL1
                        (LIST 'TIMES GA1 (LIST 'QUOTIENT AL2 GA2)))))
         (SETQ BB1
                 (AEVAL
                  (LIST 'PLUS BE1
                        (LIST 'DIFFERENCE
                              (LIST 'DIFFERENCE
                                    (LIST 'TIMES AL1 (LIST 'QUOTIENT GA1 GA2))
                                    (LIST 'TIMES AL2 GA1
                                          (LIST 'QUOTIENT GA1
                                                (LIST 'TIMES GA2 GA2))))
                              (LIST 'TIMES GA1 (LIST 'QUOTIENT BE2 GA2))))))
         (SETQ AA2 (AEVAL AL2))
         (SETQ BB2
                 (AEVAL
                  (LIST 'PLUS BE2 (LIST 'TIMES AL2 (LIST 'QUOTIENT GA1 GA2)))))
         (SETQ CC2 (AEVAL GA2))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 0 1 0 0) (LIST 1 0 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ AA1 (AEVAL BE2))
         (SETQ BB1 (AEVAL AL2))
         (SETQ AA2 (AEVAL BE1))
         (SETQ BB2 (AEVAL AL1))
         (SETQ CC2 (AEVAL GA1)))))
      (COND
       ((AND (EVALEQUAL (AEVAL AA2) 0)
             (EVALEQUAL
              (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE AA1 BB1) BB2)) 0)
             (EVALEQUAL
              (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'MINUS AA1) BB1) BB2))
              0)
             (EVALEQUAL (AEVAL CC2) 0))
        (AEVAL (LIST 'C0111 AA1 AA1)))
       (T
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL AA2) 0)
           (COND
            ((EVALNEQ (AEVAL (LIST 'DIFFERENCE (LIST 'DIFFERENCE AA1 BB1) BB2))
                      0)
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 1 0 0 0) (LIST 1 1 0 0)
                                 (LIST 0 0 1 0) (LIST 0 0 0 1))
                           'LIEMAT)))
              (SETQ AA2
                      (AEVAL
                       (LIST 'DIFFERENCE (LIST 'DIFFERENCE AA1 BB1) BB2)))
              (SETQ BB2 (AEVAL (LIST 'PLUS BB1 BB2)))
              (SETQ AA1 (AEVAL (LIST 'DIFFERENCE AA1 BB1)))))
            ((EVALNEQ
              (AEVAL (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'MINUS AA1) BB1) BB2))
              0)
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 1 0 0 0) (LIST (MINUS 1) 1 0 0)
                                 (LIST 0 0 1 0) (LIST 0 0 0 1))
                           'LIEMAT)))
              (SETQ AA2
                      (AEVAL
                       (LIST 'PLUS (LIST 'DIFFERENCE (LIST 'MINUS AA1) BB1)
                             BB2)))
              (SETQ BB2 (AEVAL (LIST 'DIFFERENCE BB2 BB1)))
              (SETQ AA1 (AEVAL (LIST 'PLUS AA1 BB1)))))
            (T
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 0 0 1 0) (LIST 0 1 0 0)
                                 (LIST 1 0 0 0)
                                 (LIST 0 0 0 (LIST 'QUOTIENT 1 AA1)))
                           'LIEMAT)))
              (SETQ AA2 (AEVAL (LIST 'QUOTIENT CC2 AA1)))
              (SETQ BB2 (AEVAL 1))
              (SETQ CC2 (AEVAL 0))
              (SETQ AA1 (AEVAL (LIST 'QUOTIENT 1 AA1))))))))
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT
                            (LIST 1 (LIST 'MINUS (LIST 'QUOTIENT AA1 AA2))
                                  (LIST 'TIMES AA1 (LIST 'QUOTIENT CC2 AA2)) 0)
                            (LIST 0 1 0 0) (LIST 0 0 1 0) (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ BE1
                 (AEVAL
                  (LIST 'DIFFERENCE BB1
                        (LIST 'TIMES AA1 (LIST 'QUOTIENT BB2 AA2)))))
         (SETQ AL2 (AEVAL AA2))
         (SETQ BE2 (AEVAL (LIST 'PLUS AA1 BB2)))
         (SETQ GA2 (AEVAL (LIST 'DIFFERENCE CC2 (LIST 'TIMES AA1 CC2))))
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0)
                            (LIST (LIST 'MINUS BE2) BE1 0 0) (LIST 0 0 1 0)
                            (LIST 0 0 0 1))
                      'LIEMAT)))
         (SETQ AA1 (AEVAL BE2))
         (SETQ AA2 (AEVAL (LIST 'TIMES AL2 BE1)))
         (SETQ CC2 (AEVAL (LIST 'TIMES GA2 BE1)))
         (COND
          ((AND (EVALNEQ (AEVAL CC2) 0)
                (EVALEQUAL (AEVAL AA2) (AEVAL (LIST 'DIFFERENCE 1 AA1))))
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0) (LIST 1 1 0 0)
                               (LIST 0 0 CC2 0) (LIST 0 0 0 1))
                         'LIEMAT)))
            (SETQ AL1 (AEVAL (LIST 'DIFFERENCE AA1 1)))
            (COND
             ((EVALEQUAL (AEVAL AL1) 1)
              (PROGN
               (COND
                ((BOOLVALUE* (REVALX *TR_LIE))
                 (ASSGNPRI (AEVAL "[W,Z]=W+X, [X,Z]=X+Y, [Y,Z]=Y") NIL 'ONLY)))
               (SETK 'LIE_CLASS
                     (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 12))))))
             (T
              (PROGN
               (SETK 'LIEMAT
                     (AEVAL
                      (LIST 'TIMES
                            (LIST 'MAT (LIST 0 0 1 0) (LIST 0 1 0 0)
                                  (LIST 1
                                        (LIST 'QUOTIENT 1
                                              (LIST 'DIFFERENCE AL1 1))
                                        (LIST 'QUOTIENT 1
                                              (LIST 'TIMES
                                                    (LIST 'DIFFERENCE AL1 1)
                                                    (LIST 'DIFFERENCE AL1 1)))
                                        0)
                                  (LIST 0 0 0 (LIST 'QUOTIENT 1 AL1)))
                            'LIEMAT)))
               (SETK 'LIEMAT
                     (AEVAL
                      (LIST 'TIMES
                            (LIST 'MAT (LIST 0 1 0 0)
                                  (LIST (LIST 'QUOTIENT 1 AL1) 0 0 0)
                                  (LIST 0 0 1 0) (LIST 0 0 0 1))
                            'LIEMAT)))
               (COND
                ((BOOLVALUE* (REVALX *TR_LIE))
                 (PROGN
                  (ASSGNPRI (AEVAL "[W,Z]=") NIL 'FIRST)
                  (ASSGNPRI (AEVAL (LIST 'QUOTIENT 1 AL1)) NIL NIL)
                  (ASSGNPRI (AEVAL "W+X, [X,Z]=") NIL NIL)
                  (ASSGNPRI (AEVAL (LIST 'QUOTIENT 1 AL1)) NIL NIL)
                  (ASSGNPRI (AEVAL "X, [Y,Z]=Y") NIL 'LAST))))
               (SETK 'LIE_CLASS
                     (AEVAL
                      (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 15)
                            (LIST 'QUOTIENT 1 AL1)))))))))
          (T
           (PROGN
            (COND
             ((EVALNEQ (AEVAL CC2) 0)
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT
                                 (LIST 1 0
                                       (LIST 'MINUS
                                             (LIST 'QUOTIENT CC2
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE 1
                                                               AA2)
                                                         AA1)))
                                       0)
                                 (LIST 0 1
                                       (LIST 'TIMES (LIST 'PLUS (MINUS 1) AA1)
                                             (LIST 'QUOTIENT CC2
                                                   (LIST 'DIFFERENCE
                                                         (LIST 'DIFFERENCE 1
                                                               AA2)
                                                         AA1)))
                                       0)
                                 (LIST 0 0
                                       (LIST 'QUOTIENT CC2
                                             (LIST 'DIFFERENCE
                                                   (LIST 'DIFFERENCE 1 AA2)
                                                   AA1))
                                       0)
                                 (LIST 0 0 0 1))
                           'LIEMAT)))))
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0)
                               (LIST (LIST 'QUOTIENT AA1 2) 1 0 0)
                               (LIST 0 0 1 0) (LIST 0 0 0 1))
                         'LIEMAT)))
            (SETQ R
                    (AEVAL
                     (LIST 'PLUS (LIST 'TIMES AA1 (LIST 'QUOTIENT AA1 4))
                           AA2)))
            (COND
             ((EVALEQUAL (AEVAL R) 0)
              (PROGN
               (COND
                ((BOOLVALUE* (REVALX *TR_LIE))
                 (PROGN
                  (ASSGNPRI (AEVAL "[W,Z]=") NIL 'FIRST)
                  (ASSGNPRI (AEVAL (LIST 'QUOTIENT AA1 2)) NIL NIL)
                  (ASSGNPRI (AEVAL "W+X, [X,Z]=") NIL NIL)
                  (ASSGNPRI (AEVAL (LIST 'QUOTIENT AA1 2)) NIL NIL)
                  (ASSGNPRI (AEVAL "X, [Y,Z]=Y") NIL 'LAST))))
               (SETK 'LIE_CLASS
                     (AEVAL
                      (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 15)
                            (LIST 'QUOTIENT AA1 2))))))
             (T
              (PROGN
               (SETK 'LIEMAT
                     (AEVAL
                      (LIST 'TIMES
                            (LIST 'MAT (LIST (LIST 'SQRT (LIST 'ABS R)) 0 0 0)
                                  (LIST 0 1 0 0) (LIST 0 0 1 0) (LIST 0 0 0 1))
                            'LIEMAT)))
               (COND
                ((NOT (EVALNUMBERP (AEVAL R)))
                 (PROGN
                  (PROGN
                   (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                   (ASSGNPRI (AEVAL R) NIL NIL)
                   (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL
                             'LAST))
                  (SETQ HE (AEVAL (READ)))
                  (COND
                   ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
                    (PROGN
                     (SETK 'LIEMAT
                           (AEVAL
                            (LIST 'TIMES
                                  (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                                        (LIST 0 0 1 0)
                                        (LIST 0 0 0
                                              (LIST 'SQRT
                                                    (LIST 'ABS
                                                          (LIST 'QUOTIENT 1
                                                                R)))))
                                  'LIEMAT)))
                     (SETQ S
                             (AEVAL
                              (LIST 'QUOTIENT AA1
                                    (LIST 'TIMES 2
                                          (LIST 'SQRT (LIST 'ABS R))))))
                     (COND
                      ((BOOLVALUE* (REVALX *TR_LIE))
                       (PROGN
                        (ASSGNPRI (AEVAL "[W,Z]=") NIL 'FIRST)
                        (ASSGNPRI (AEVAL S) NIL NIL)
                        (ASSGNPRI (AEVAL "W+X, [X,Z]=-W+") NIL NIL)
                        (ASSGNPRI (AEVAL S) NIL NIL)
                        (ASSGNPRI (AEVAL "X, [Y,Z]=") NIL NIL)
                        (ASSGNPRI
                         (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'QUOTIENT 1 R))))
                         NIL NIL)
                        (ASSGNPRI (AEVAL "Y") NIL 'LAST))))
                     (SETK 'LIE_CLASS
                           (AEVAL
                            (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 14) S
                                  (LIST 'SQRT
                                        (LIST 'ABS (LIST 'QUOTIENT 1 R))))))))
                   (T
                    (PROGN
                     (SETK 'LIEMAT
                           (AEVAL
                            (LIST 'TIMES
                                  (LIST 'MAT (LIST 1 0 0 0) (LIST 1 1 0 0)
                                        (LIST 0 0 1 0) (LIST 0 0 0 1))
                                  'LIEMAT)))
                     (SETK 'LIEMAT
                           (AEVAL
                            (LIST 'TIMES
                                  (LIST 'MAT
                                        (LIST
                                         (LIST 'MINUS
                                               (LIST 'TIMES 2
                                                     (LIST 'SQRT
                                                           (LIST 'ABS R))))
                                         (LIST 'SQRT (LIST 'ABS R)) 0 0)
                                        (LIST 0 (LIST 'SQRT (LIST 'ABS R)) 0 0)
                                        (LIST 0 0 1 0) (LIST 0 0 0 1))
                                  'LIEMAT)))
                     (PROGN
                      (AEVAL
                       (LIST 'C0111
                             (LIST 'DIFFERENCE (LIST 'QUOTIENT AA1 2)
                                   (LIST 'SQRT (LIST 'ABS R)))
                             (LIST 'PLUS (LIST 'QUOTIENT AA1 2)
                                   (LIST 'SQRT (LIST 'ABS R)))))))))))
                ((EVALLESSP (AEVAL R) 0)
                 (PROGN
                  (SETK 'LIEMAT
                        (AEVAL
                         (LIST 'TIMES
                               (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                                     (LIST 0 0 1 0)
                                     (LIST 0 0 0
                                           (LIST 'SQRT
                                                 (LIST 'ABS
                                                       (LIST 'QUOTIENT 1 R)))))
                               'LIEMAT)))
                  (SETQ S
                          (AEVAL
                           (LIST 'QUOTIENT AA1
                                 (LIST 'TIMES 2 (LIST 'SQRT (LIST 'ABS R))))))
                  (COND
                   ((BOOLVALUE* (REVALX *TR_LIE))
                    (PROGN
                     (ASSGNPRI (AEVAL "[W,Z]=") NIL 'FIRST)
                     (ASSGNPRI (AEVAL S) NIL NIL)
                     (ASSGNPRI (AEVAL "W+X, [X,Z]=-W+") NIL NIL)
                     (ASSGNPRI (AEVAL S) NIL NIL)
                     (ASSGNPRI (AEVAL "X, [Y,Z]=") NIL NIL)
                     (ASSGNPRI
                      (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'QUOTIENT 1 R)))) NIL
                      NIL)
                     (ASSGNPRI (AEVAL "Y") NIL 'LAST))))
                  (SETK 'LIE_CLASS
                        (AEVAL
                         (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 14) S
                               (LIST 'SQRT
                                     (LIST 'ABS (LIST 'QUOTIENT 1 R))))))))
                (T
                 (PROGN
                  (SETK 'LIEMAT
                        (AEVAL
                         (LIST 'TIMES
                               (LIST 'MAT (LIST 1 0 0 0) (LIST 1 1 0 0)
                                     (LIST 0 0 1 0) (LIST 0 0 0 1))
                               'LIEMAT)))
                  (SETK 'LIEMAT
                        (AEVAL
                         (LIST 'TIMES
                               (LIST 'MAT
                                     (LIST
                                      (LIST 'MINUS
                                            (LIST 'TIMES 2
                                                  (LIST 'SQRT (LIST 'ABS R))))
                                      (LIST 'SQRT (LIST 'ABS R)) 0 0)
                                     (LIST 0 (LIST 'SQRT (LIST 'ABS R)) 0 0)
                                     (LIST 0 0 1 0) (LIST 0 0 0 1))
                               'LIEMAT)))
                  (AEVAL
                   (LIST 'C0111
                         (LIST 'DIFFERENCE (LIST 'QUOTIENT AA1 2)
                               (LIST 'SQRT (LIST 'ABS R)))
                         (LIST 'PLUS (LIST 'QUOTIENT AA1 2)
                               (LIST 'SQRT (LIST 'ABS R))))))))))))))))))) 
(PUT 'C0111 'NUMBER-OF-ARGS 2) 
(FLAG '(C0111) 'OPFN) 
(PUT 'C0111 'DEFINED-ON-LINE '1006) 
(PUT 'C0111 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'C0111 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE C0111 (MY NY)
    (PROG ()
      (SETK 'LIEMAT
            (AEVAL
             (LIST 'TIMES
                   (LIST 'MAT (LIST 0 0 1 0) (LIST 1 0 0 0) (LIST 0 1 0 0)
                         (LIST 0 0 0 1))
                   'LIEMAT)))
      (COND
       ((BOOLVALUE* (REVALX *TR_LIE))
        (PROGN
         (ASSGNPRI (AEVAL "[W,Z]=W, [X,Z]=") NIL 'FIRST)
         (ASSGNPRI (AEVAL MY) NIL NIL)
         (ASSGNPRI (AEVAL "X, [Y,Z]=") NIL NIL)
         (ASSGNPRI (AEVAL NY) NIL NIL)
         (ASSGNPRI (AEVAL "Y") NIL 'LAST))))
      (SETK 'LIE_CLASS
            (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 13) MY NY))))) 
(PUT 'COMCOM1 'NUMBER-OF-ARGS 0) 
(FLAG '(COMCOM1) 'OPFN) 
(PUT 'COMCOM1 'DEFINED-ON-LINE '1014) 
(PUT 'COMCOM1 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COMCOM1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COMCOM1 NIL
    (PROG (II HE A1 A2 A3 B2 B3 C2 C3 HELP)
      (SETQ II 0)
      (AEVAL
       (MATRIX
        (LIST (LIST 'A11 4 4) (LIST 'A22 4 4) (LIST 'A33 4 4)
              (LIST 'CCC 3 3))))
      (SETQ HELP (AEVAL 0))
      (PROG (M)
        (SETQ M 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 M)) (RETURN NIL)))
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 3 N)) (RETURN NIL)))
          (COND
           ((EVALNEQ (AEVAL* (LIST 'FF M N)) 0)
            (PROGN
             (SETQ II (AEVAL* M))
             (SETQ M (AEVAL* 3))
             (SETQ N (AEVAL* 3)))))
          (SETQ N (PLUS2 N 1))
          (GO LAB))
        (SETQ M (PLUS2 M 1))
        (GO LAB))
      (SETK 'A11
            (AEVAL
             (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                   (LIST (LIST 'FF II 1) (LIST 'FF II 2) (LIST 'FF II 3) 0)
                   (LIST 0 0 0 1))))
      (SETK 'A22
            (AEVAL
             (LIST 'MAT (LIST 1 0 0 0) (LIST 0 0 1 0)
                   (LIST (LIST 'FF II 1) (LIST 'FF II 2) (LIST 'FF II 3) 0)
                   (LIST 0 0 0 1))))
      (SETK 'A33
            (AEVAL
             (LIST 'MAT (LIST 0 1 0 0) (LIST 0 0 1 0)
                   (LIST (LIST 'FF II 1) (LIST 'FF II 2) (LIST 'FF II 3) 0)
                   (LIST 0 0 0 1))))
      (COND
       ((EVALNEQ (AEVAL (LIST 'DET 'A11)) 0)
        (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'A11 'LIEMAT))))
       ((EVALNEQ (AEVAL (LIST 'DET 'A22)) 0)
        (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'A22 'LIEMAT))))
       (T (SETK 'LIEMAT (AEVAL (LIST 'TIMES 'A33 'LIEMAT)))))
      (SETK 'LIEMAT
            (AEVAL
             (LIST 'TIMES
                   (LIST 'MAT (LIST 0 0 1 0) (LIST 1 0 0 0) (LIST 0 1 0 0)
                         (LIST 0 0 0 1))
                   'LIEMAT)))
      (SETK 'A11 (AEVAL (LIST 'QUOTIENT 1 'LIEMAT)))
      (PROG (M)
        (SETQ M 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 3 M)) (RETURN NIL)))
        (PROG (N)
          (SETQ N 1)
         LAB
          (COND ((MINUSP (DIFFERENCE 3 N)) (RETURN NIL)))
          (SETK (LIST 'CCC M N)
                (PROG (I FORALL-RESULT)
                  (SETQ I 1)
                  (SETQ FORALL-RESULT 0)
                 LAB1
                  (COND ((MINUSP (DIFFERENCE 4 I)) (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (AEVAL*
                           (LIST 'PLUS
                                 (PROG (J FORALL-RESULT)
                                   (SETQ J 1)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND
                                    ((MINUSP (DIFFERENCE 4 J))
                                     (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (AEVAL*
                                            (LIST 'PLUS
                                                  (PROG (K FORALL-RESULT)
                                                    (SETQ K 1)
                                                    (SETQ FORALL-RESULT 0)
                                                   LAB1
                                                    (COND
                                                     ((MINUSP (DIFFERENCE 4 K))
                                                      (RETURN FORALL-RESULT)))
                                                    (SETQ FORALL-RESULT
                                                            (AEVAL*
                                                             (LIST 'PLUS
                                                                   (AEVAL*
                                                                    (LIST
                                                                     'TIMES
                                                                     (LIST
                                                                      'LIEMAT M
                                                                      I)
                                                                     (LIST
                                                                      'LIEMAT 4
                                                                      J)
                                                                     (LIST 'CC
                                                                           I J
                                                                           K)
                                                                     (LIST 'A11
                                                                           K
                                                                           N)))
                                                                   FORALL-RESULT)))
                                                    (SETQ K (PLUS2 K 1))
                                                    (GO LAB1))
                                                  FORALL-RESULT)))
                                   (SETQ J (PLUS2 J 1))
                                   (GO LAB1))
                                 FORALL-RESULT)))
                  (SETQ I (PLUS2 I 1))
                  (GO LAB1)))
          (SETQ N (PLUS2 N 1))
          (GO LAB))
        (SETQ M (PLUS2 M 1))
        (GO LAB))
      (SETQ A1 (AEVAL (LIST 'CCC 1 1)))
      (SETQ A2 (AEVAL (LIST 'CCC 2 1)))
      (SETQ A3 (AEVAL (LIST 'CCC 3 1)))
      (SETQ B2 (AEVAL (LIST 'CCC 2 2)))
      (SETQ B3 (AEVAL (LIST 'CCC 3 2)))
      (SETQ C2 (AEVAL (LIST 'CCC 2 3)))
      (SETQ C3 (AEVAL (LIST 'CCC 3 3)))
      (COND
       ((EVALEQUAL (AEVAL A1) 0)
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL C2) 0)
           (COND
            ((EVALEQUAL (AEVAL B3) 0)
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 1 0)
                                 (LIST 0 0 1 0) (LIST 0 0 0 1))
                           'LIEMAT)))
              (SETQ A2 (AEVAL (LIST 'PLUS A2 A3)))
              (SETQ C2 (AEVAL (LIST 'MINUS (LIST 'TIMES 2 B2))))))
            (T
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 1 0 0 0)
                                 (LIST 0 1 (LIST 'QUOTIENT B2 B3) 0)
                                 (LIST 0 0 1 0) (LIST 0 0 0 1))
                           'LIEMAT)))
              (SETQ A2
                      (AEVAL
                       (LIST 'PLUS A2
                             (LIST 'TIMES A3 (LIST 'QUOTIENT B2 B3)))))
              (SETQ C2
                      (AEVAL
                       (LIST 'MINUS
                             (LIST 'TIMES 3 B2 (LIST 'QUOTIENT B2 B3)))))
              (SETQ B2 (AEVAL (LIST 'TIMES 2 B2))))))))
         (SETQ HELP
                 (AEVAL (LIST 'PLUS (LIST 'TIMES B2 B2) (LIST 'TIMES C2 B3))))
         (SETQ C3 (AEVAL (LIST 'SQRT (LIST 'ABS HELP))))
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST (LIST 'QUOTIENT C2 C3) 0 0 0)
                            (LIST 0 1 0 0)
                            (LIST 0 (LIST 'QUOTIENT B2 C3)
                                  (LIST 'QUOTIENT C2 C3) 0)
                            (LIST 0 (LIST 'TIMES A3 (LIST 'QUOTIENT C3 HELP))
                                  (LIST 'MINUS
                                        (LIST 'TIMES A2
                                              (LIST 'QUOTIENT C3 HELP)))
                                  (LIST 'QUOTIENT C3 HELP)))
                      'LIEMAT)))
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (PROGN
            (ASSGNPRI (AEVAL "[X,Y]=W, [X,Z]=") NIL 'FIRST)
            (ASSGNPRI (AEVAL (LIST 'QUOTIENT HELP (LIST 'ABS HELP))) NIL NIL)
            (ASSGNPRI (AEVAL "Y, [Y,Z]=X") NIL 'LAST))))
         (COND
          ((EVALGREATERP (AEVAL HELP) 0)
           (SETK 'LIE_CLASS
                 (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 19)))))
          (T
           (SETK 'LIE_CLASS
                 (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 20))))))))
       (T
        (PROGN
         (SETK 'LIEMAT
               (AEVAL
                (LIST 'TIMES
                      (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                            (LIST 0 (LIST 'TIMES 2 (LIST 'QUOTIENT A3 A1))
                                  (LIST 'MINUS
                                        (LIST 'TIMES 2 (LIST 'QUOTIENT A2 A1)))
                                  (LIST 'QUOTIENT 2 A1)))
                      'LIEMAT)))
         (SETQ B2 (AEVAL (LIST 'TIMES 2 (LIST 'QUOTIENT B2 A1))))
         (SETQ C2 (AEVAL (LIST 'TIMES 2 (LIST 'QUOTIENT C2 A1))))
         (SETQ B3 (AEVAL (LIST 'TIMES 2 (LIST 'QUOTIENT B3 A1))))
         (SETQ C3 (AEVAL (LIST 'TIMES 2 (LIST 'QUOTIENT C3 A1))))
         (COND
          ((EVALNEQ (AEVAL B3) 0)
           (PROGN
            (SETK 'LIEMAT
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 1 0 0 0)
                               (LIST 0 1
                                     (LIST 'QUOTIENT (LIST 'DIFFERENCE 1 B2)
                                           B3)
                                     0)
                               (LIST 0 0 1 0) (LIST 0 0 0 1))
                         'LIEMAT)))
            (SETQ C2
                    (AEVAL
                     (LIST 'PLUS C2
                           (LIST 'TIMES (LIST 'DIFFERENCE 1 B2)
                                 (LIST 'QUOTIENT (LIST 'DIFFERENCE C3 1)
                                       B3)))))
            (SETQ B2 (SETQ C3 (AEVAL 1)))
            (COND
             ((EVALEQUAL (AEVAL C2) 0)
              (PROGN
               (SETK 'LIEMAT
                     (AEVAL
                      (LIST 'TIMES
                            (LIST 'MAT (LIST (MINUS 1) 0 0 0) (LIST 0 0 1 0)
                                  (LIST 0 1 0 0) (LIST 0 0 0 1))
                            'LIEMAT)))
               (SETQ C2 (AEVAL B3))))
             (T
              (PROGN
               (SETQ A1 (AEVAL (LIST 'QUOTIENT B3 (LIST 'ABS B3))))
               (SETQ A2 (AEVAL (LIST 'QUOTIENT C2 (LIST 'ABS C2))))
               (SETQ A3 (AEVAL (LIST 'SQRT (LIST 'ABS (LIST 'TIMES B3 C2)))))
               (SETK 'LIEMAT
                     (AEVAL
                      (LIST 'TIMES
                            (LIST 'MAT (LIST 1 0 0 0)
                                  (LIST 0
                                        (LIST 'EXPT
                                              (LIST 'ABS
                                                    (LIST 'QUOTIENT B3 C2))
                                              (LIST 'QUOTIENT 1 4))
                                        0 0)
                                  (LIST 0 0
                                        (LIST 'EXPT
                                              (LIST 'ABS
                                                    (LIST 'QUOTIENT C2 B3))
                                              (LIST 'QUOTIENT 1 4))
                                        0)
                                  (LIST 0 0 0 1))
                            'LIEMAT)))
               (COND
                ((EVALEQUAL (AEVAL A1) (AEVAL A2))
                 (PROGN
                  (COND
                   ((NOT (EVALNUMBERP (AEVAL A1)))
                    (PROGN
                     (PROGN
                      (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                      (ASSGNPRI (AEVAL A1) NIL NIL)
                      (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL
                                'LAST))
                     (SETQ HE (AEVAL (READ)))
                     (COND
                      ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
                       (SETQ A3 (AEVAL (LIST 'MINUS A3)))))))
                   ((EVALLESSP (AEVAL A1) 0)
                    (SETQ A3 (AEVAL (LIST 'MINUS A3)))))
                  (SETK 'LIEMAT
                        (AEVAL
                         (LIST 'TIMES
                               (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0)
                                     (LIST 0 1 1 0) (LIST 0 0 0 1))
                               'LIEMAT)))
                  (SETQ B2 (AEVAL (LIST 'DIFFERENCE 1 A3)))
                  (SETQ C2 (AEVAL A3))
                  (SETQ C3 (AEVAL (LIST 'PLUS A3 1)))))
                (T
                 (PROGN
                  (SETQ HELP (AEVAL 1))
                  (COND
                   ((NOT (EVALNUMBERP (AEVAL A1)))
                    (PROGN
                     (PROGN
                      (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                      (ASSGNPRI (AEVAL A1) NIL NIL)
                      (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL
                                'LAST))
                     (SETQ HE (AEVAL (READ)))
                     (COND
                      ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
                       (SETK 'LIEMAT
                             (AEVAL
                              (LIST 'TIMES
                                    (LIST 'MAT (LIST (MINUS 1) 0 0 0)
                                          (LIST 0 0 1 0) (LIST 0 1 0 0)
                                          (LIST 0 0 0 1))
                                    'LIEMAT)))))))
                   ((EVALLESSP (AEVAL A1) 0)
                    (SETK 'LIEMAT
                          (AEVAL
                           (LIST 'TIMES
                                 (LIST 'MAT (LIST (MINUS 1) 0 0 0)
                                       (LIST 0 0 1 0) (LIST 0 1 0 0)
                                       (LIST 0 0 0 1))
                                 'LIEMAT)))))
                  (COND
                   ((BOOLVALUE* (REVALX *TR_LIE))
                    (PROGN
                     (ASSGNPRI (AEVAL "[W,Z]=2W, [X,Y]=W, [X,Z]=X-") NIL
                               'FIRST)
                     (ASSGNPRI (AEVAL A3) NIL NIL)
                     (ASSGNPRI (AEVAL "Y, ") NIL NIL)
                     (ASSGNPRI (AEVAL "[Y,Z]=") NIL NIL)
                     (ASSGNPRI (AEVAL A3) NIL NIL)
                     (ASSGNPRI (AEVAL "X+Y") NIL 'LAST))))
                  (SETK 'LIE_CLASS
                        (AEVAL
                         (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 17)
                               A3))))))))))))
         (COND
          ((EVALNEQ (AEVAL HELP) 1)
           (COND
            ((OR (EVALEQUAL (AEVAL C2) 0) (EVALNEQ (AEVAL B2) (AEVAL C3)))
             (PROGN
              (COND
               ((EVALNEQ (AEVAL B2) (AEVAL C3))
                (SETK 'LIEMAT
                      (AEVAL
                       (LIST 'TIMES
                             (LIST 'MAT (LIST 1 0 0 0)
                                   (LIST 0 1
                                         (LIST 'QUOTIENT C2
                                               (LIST 'DIFFERENCE B2 C3))
                                         0)
                                   (LIST 0 0 1 0) (LIST 0 0 0 1))
                             'LIEMAT)))))
              (COND
               ((NOT (EVALNUMBERP (AEVAL B2)))
                (PROGN
                 (PROGN
                  (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                  (ASSGNPRI (AEVAL B2) NIL NIL)
                  (ASSGNPRI (AEVAL "<1 ? (y/n) and press <RETURN>") NIL 'LAST))
                 (SETQ HE (AEVAL (READ)))
                 (COND
                  ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
                   (SETK 'LIEMAT
                         (AEVAL
                          (LIST 'TIMES
                                (LIST 'MAT (LIST (MINUS 1) 0 0 0)
                                      (LIST 0 0 1 0) (LIST 0 1 0 0)
                                      (LIST 0 0 0 1))
                                'LIEMAT)))))
                 (SETQ HELP (AEVAL B2))
                 (SETQ B2 (AEVAL C3))
                 (SETQ C3 (AEVAL HELP))))
               ((EVALLESSP (AEVAL B2) 1)
                (PROGN
                 (SETK 'LIEMAT
                       (AEVAL
                        (LIST 'TIMES
                              (LIST 'MAT (LIST (MINUS 1) 0 0 0) (LIST 0 0 1 0)
                                    (LIST 0 1 0 0) (LIST 0 0 0 1))
                              'LIEMAT)))
                 (SETQ HELP (AEVAL B2))
                 (SETQ B2 (AEVAL C3))
                 (SETQ C3 (AEVAL HELP)))))
              (COND
               ((BOOLVALUE* (REVALX *TR_LIE))
                (PROGN
                 (ASSGNPRI (AEVAL "[W,Z]=2W, [X,Y]=W, [X,Z]=") NIL 'FIRST)
                 (ASSGNPRI (AEVAL B2) NIL NIL)
                 (ASSGNPRI (AEVAL "X, [Y,Z]=") NIL NIL)
                 (ASSGNPRI (AEVAL C3) NIL NIL)
                 (ASSGNPRI (AEVAL "Y") NIL 'LAST))))
              (SETK 'LIE_CLASS
                    (AEVAL
                     (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 16)
                           (LIST 'DIFFERENCE B2 1))))))
            (T
             (PROGN
              (SETK 'LIEMAT
                    (AEVAL
                     (LIST 'TIMES
                           (LIST 'MAT (LIST 1 0 0 0)
                                 (LIST 0
                                       (LIST 'QUOTIENT 1
                                             (LIST 'SQRT (LIST 'ABS C2)))
                                       0 0)
                                 (LIST 0 0 (LIST 'SQRT (LIST 'ABS C2)) 0)
                                 (LIST 0 0 0 1))
                           'LIEMAT)))
              (COND
               ((NOT (EVALNUMBERP (AEVAL C2)))
                (PROGN
                 (PROGN
                  (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                  (ASSGNPRI (AEVAL C2) NIL NIL)
                  (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL 'LAST))
                 (SETQ HE (AEVAL (READ)))
                 (COND
                  ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
                   (SETK 'LIEMAT
                         (AEVAL
                          (LIST 'TIMES
                                (LIST 'MAT (LIST (MINUS 1) 0 0 0)
                                      (LIST 0 1 0 0) (LIST 0 0 (MINUS 1) 0)
                                      (LIST 0 0 0 1))
                                'LIEMAT)))))))
               ((EVALLESSP (AEVAL C2) 0)
                (SETK 'LIEMAT
                      (AEVAL
                       (LIST 'TIMES
                             (LIST 'MAT (LIST (MINUS 1) 0 0 0) (LIST 0 1 0 0)
                                   (LIST 0 0 (MINUS 1) 0) (LIST 0 0 0 1))
                             'LIEMAT)))))
              (COND
               ((BOOLVALUE* (REVALX *TR_LIE))
                (ASSGNPRI (AEVAL "[W,Z]=2W, [X,Y]=W, [X,Z]=X+Y, [Y,Z]=Y") NIL
                          'ONLY)))
              (SETK 'LIE_CLASS
                    (AEVAL
                     (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 18))))))))))))
      (AEVAL (CLEAR (LIST 'A11 'A22 'A33 'CCC))))) 
(PUT 'COMCOM3 'NUMBER-OF-ARGS 0) 
(FLAG '(COMCOM3) 'OPFN) 
(PUT 'COMCOM3 'DEFINED-ON-LINE '1116) 
(PUT 'COMCOM3 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'COMCOM3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE COMCOM3 NIL
    (PROG (HELP HE AL BE GA)
      (SETQ HELP 0)
      (AEVAL (MATRIX (LIST (LIST 'L_K 3 3) (LIST 'L_A 3 3))))
      (SETQ HELP (AEVAL 0))
      (SETK (LIST 'L_K 1 1)
            (AEVAL
             (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                   (LIST 'TIMES 2 (LIST 'FF 1 3) (LIST 'FF 2 2))
                   (LIST 'EXPT (LIST 'FF 2 3) 2))))
      (SETK (LIST 'L_K 1 2)
            (AEVAL
             (LIST 'PLUS
                   (LIST 'MINUS (LIST 'TIMES (LIST 'FF 1 1) (LIST 'FF 1 2)))
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 3 2))
                         (LIST 'TIMES (LIST 'FF 2 1) (LIST 'FF 1 3)))
                   (LIST 'TIMES (LIST 'FF 2 3) (LIST 'FF 3 3)))))
      (SETK (LIST 'L_K 1 3)
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'MINUS
                                     (LIST 'TIMES (LIST 'FF 1 1)
                                           (LIST 'FF 2 2)))
                               (LIST 'TIMES (LIST 'FF 1 2) (LIST 'FF 3 2)))
                         (LIST 'TIMES (LIST 'FF 2 1) (LIST 'FF 2 3)))
                   (LIST 'TIMES (LIST 'FF 2 2) (LIST 'FF 3 3)))))
      (SETK (LIST 'L_K 2 1) (AEVAL (LIST 'L_K 1 2)))
      (SETK (LIST 'L_K 2 2)
            (AEVAL
             (LIST 'PLUS
                   (LIST 'DIFFERENCE (LIST 'EXPT (LIST 'FF 1 1) 2)
                         (LIST 'TIMES 2 (LIST 'FF 1 3) (LIST 'FF 3 1)))
                   (LIST 'EXPT (LIST 'FF 3 3) 2))))
      (SETK (LIST 'L_K 2 3)
            (AEVAL
             (LIST 'PLUS (LIST 'TIMES (LIST 'FF 1 1) (LIST 'FF 2 1))
                   (LIST 'DIFFERENCE
                         (LIST 'DIFFERENCE
                               (LIST 'TIMES (LIST 'FF 1 2) (LIST 'FF 3 1))
                               (LIST 'TIMES (LIST 'FF 3 1) (LIST 'FF 2 3)))
                         (LIST 'TIMES (LIST 'FF 3 2) (LIST 'FF 3 3))))))
      (SETK (LIST 'L_K 3 1) (AEVAL (LIST 'L_K 1 3)))
      (SETK (LIST 'L_K 3 2) (AEVAL (LIST 'L_K 2 3)))
      (SETK (LIST 'L_K 3 3)
            (AEVAL
             (LIST 'PLUS (LIST 'EXPT (LIST 'FF 2 1) 2)
                   (LIST 'TIMES 2 (LIST 'FF 2 2) (LIST 'FF 3 1))
                   (LIST 'EXPT (LIST 'FF 3 2) 2))))
      (COND
       ((NOT
         (AND (EVALNUMBERP (AEVAL (LIST 'L_K 1 1)))
              (EVALNUMBERP
               (AEVAL
                (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                      (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1)))))
              (EVALNUMBERP (AEVAL (LIST 'DET 'L_K)))))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
          (ASSGNPRI (AEVAL (LIST 'MINUS (LIST 'L_K 1 1))) NIL NIL)
          (ASSGNPRI (AEVAL ">0 and ") NIL NIL)
          (ASSGNPRI
           (AEVAL
            (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                  (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1))))
           NIL NIL)
          (ASSGNPRI (AEVAL ">0 and ") NIL NIL)
          (ASSGNPRI (AEVAL (LIST 'MINUS (LIST 'DET 'L_K))) NIL NIL)
          (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
         (SETQ HE (AEVAL (READ)))
         (COND
          ((EVALEQUAL (AEVAL HE) (AEVAL 'Y))
           (PROGN (SETQ HELP (AEVAL 1)) (AEVAL (LIST 'LIE4SO3))))
          (T (AEVAL (LIST 'LIE4SO21))))))
       ((AND (EVALGREATERP (AEVAL (LIST 'MINUS (LIST 'L_K 1 1))) 0)
             (EVALGREATERP
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'TIMES (LIST 'L_K 1 1) (LIST 'L_K 2 2))
                     (LIST 'TIMES (LIST 'L_K 1 2) (LIST 'L_K 2 1))))
              0)
             (EVALGREATERP (AEVAL (LIST 'MINUS (LIST 'DET 'L_K))) 0))
        (PROGN (SETQ HELP (AEVAL 1)) (AEVAL (LIST 'LIE4SO3))))
       (T (AEVAL (LIST 'LIE4SO21))))
      (SETK 'LIEMAT
            (AEVAL
             (LIST 'TIMES
                   (LIST 'MAT
                         (LIST (LIST 'L_A 1 1) (LIST 'L_A 1 2) (LIST 'L_A 1 3)
                               0)
                         (LIST (LIST 'L_A 2 1) (LIST 'L_A 2 2) (LIST 'L_A 2 3)
                               0)
                         (LIST (LIST 'L_A 3 1) (LIST 'L_A 3 2) (LIST 'L_A 3 3)
                               0)
                         (LIST 0 0 0 1))
                   'LIEMAT)))
      (SETK 'BB (AEVAL (LIST 'QUOTIENT 1 'LIEMAT)))
      (SETQ AL
              (PROG (J FORALL-RESULT)
                (SETQ J 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (PROG (K FORALL-RESULT)
                                 (SETQ K 1)
                                 (SETQ FORALL-RESULT 0)
                                LAB1
                                 (COND
                                  ((MINUSP (DIFFERENCE 4 K))
                                   (RETURN FORALL-RESULT)))
                                 (SETQ FORALL-RESULT
                                         (AEVAL*
                                          (LIST 'PLUS
                                                (PROG (L FORALL-RESULT)
                                                  (SETQ L 1)
                                                  (SETQ FORALL-RESULT 0)
                                                 LAB1
                                                  (COND
                                                   ((MINUSP (DIFFERENCE 4 L))
                                                    (RETURN FORALL-RESULT)))
                                                  (SETQ FORALL-RESULT
                                                          (AEVAL*
                                                           (LIST 'PLUS
                                                                 (AEVAL*
                                                                  (LIST 'TIMES
                                                                        (LIST
                                                                         'LIEMAT
                                                                         1 J)
                                                                        (LIST
                                                                         'LIEMAT
                                                                         4 K)
                                                                        (LIST
                                                                         'CC J
                                                                         K L)
                                                                        (LIST
                                                                         'BB L
                                                                         2)))
                                                                 FORALL-RESULT)))
                                                  (SETQ L (PLUS2 L 1))
                                                  (GO LAB1))
                                                FORALL-RESULT)))
                                 (SETQ K (PLUS2 K 1))
                                 (GO LAB1))
                               FORALL-RESULT)))
                (SETQ J (PLUS2 J 1))
                (GO LAB1)))
      (SETQ BE
              (PROG (J FORALL-RESULT)
                (SETQ J 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (PROG (K FORALL-RESULT)
                                 (SETQ K 1)
                                 (SETQ FORALL-RESULT 0)
                                LAB1
                                 (COND
                                  ((MINUSP (DIFFERENCE 4 K))
                                   (RETURN FORALL-RESULT)))
                                 (SETQ FORALL-RESULT
                                         (AEVAL*
                                          (LIST 'PLUS
                                                (PROG (L FORALL-RESULT)
                                                  (SETQ L 1)
                                                  (SETQ FORALL-RESULT 0)
                                                 LAB1
                                                  (COND
                                                   ((MINUSP (DIFFERENCE 4 L))
                                                    (RETURN FORALL-RESULT)))
                                                  (SETQ FORALL-RESULT
                                                          (AEVAL*
                                                           (LIST 'PLUS
                                                                 (AEVAL*
                                                                  (LIST 'TIMES
                                                                        (LIST
                                                                         'LIEMAT
                                                                         1 J)
                                                                        (LIST
                                                                         'LIEMAT
                                                                         4 K)
                                                                        (LIST
                                                                         'CC J
                                                                         K L)
                                                                        (LIST
                                                                         'BB L
                                                                         3)))
                                                                 FORALL-RESULT)))
                                                  (SETQ L (PLUS2 L 1))
                                                  (GO LAB1))
                                                FORALL-RESULT)))
                                 (SETQ K (PLUS2 K 1))
                                 (GO LAB1))
                               FORALL-RESULT)))
                (SETQ J (PLUS2 J 1))
                (GO LAB1)))
      (SETQ GA
              (PROG (J FORALL-RESULT)
                (SETQ J 1)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((MINUSP (DIFFERENCE 4 J)) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (AEVAL*
                         (LIST 'PLUS
                               (PROG (K FORALL-RESULT)
                                 (SETQ K 1)
                                 (SETQ FORALL-RESULT 0)
                                LAB1
                                 (COND
                                  ((MINUSP (DIFFERENCE 4 K))
                                   (RETURN FORALL-RESULT)))
                                 (SETQ FORALL-RESULT
                                         (AEVAL*
                                          (LIST 'PLUS
                                                (PROG (L FORALL-RESULT)
                                                  (SETQ L 1)
                                                  (SETQ FORALL-RESULT 0)
                                                 LAB1
                                                  (COND
                                                   ((MINUSP (DIFFERENCE 4 L))
                                                    (RETURN FORALL-RESULT)))
                                                  (SETQ FORALL-RESULT
                                                          (AEVAL*
                                                           (LIST 'PLUS
                                                                 (AEVAL*
                                                                  (LIST 'TIMES
                                                                        (LIST
                                                                         'LIEMAT
                                                                         2 J)
                                                                        (LIST
                                                                         'LIEMAT
                                                                         4 K)
                                                                        (LIST
                                                                         'CC J
                                                                         K L)
                                                                        (LIST
                                                                         'BB L
                                                                         3)))
                                                                 FORALL-RESULT)))
                                                  (SETQ L (PLUS2 L 1))
                                                  (GO LAB1))
                                                FORALL-RESULT)))
                                 (SETQ K (PLUS2 K 1))
                                 (GO LAB1))
                               FORALL-RESULT)))
                (SETQ J (PLUS2 J 1))
                (GO LAB1)))
      (COND
       ((EQUAL HELP 1)
        (SETK 'LIEMAT
              (AEVAL
               (LIST 'TIMES
                     (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                           (LIST GA (LIST 'MINUS BE) AL 1))
                     'LIEMAT))))
       (T
        (SETK 'LIEMAT
              (AEVAL
               (LIST 'TIMES
                     (LIST 'MAT (LIST 1 0 0 0) (LIST 0 1 0 0) (LIST 0 0 1 0)
                           (LIST GA (LIST 'MINUS BE) (LIST 'MINUS AL) 1))
                     'LIEMAT)))))
      (COND
       ((EQUAL HELP 1)
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[W,X]=Y, [W,Y]=-X, [X,Y]=W") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 21))))))
       (T
        (PROGN
         (COND
          ((BOOLVALUE* (REVALX *TR_LIE))
           (ASSGNPRI (AEVAL "[W,X]=Y, [W,Y]=X, [X,Y]=W") NIL 'ONLY)))
         (SETK 'LIE_CLASS
               (AEVAL (LIST 'LIST (LIST 'LIEALG 4) (LIST 'COMTAB 22)))))))
      (AEVAL (CLEAR (LIST 'L_K 'L_A))))) 
(PUT 'LIE4SO3 'NUMBER-OF-ARGS 0) 
(FLAG '(LIE4SO3) 'OPFN) 
(PUT 'LIE4SO3 'DEFINED-ON-LINE '1165) 
(PUT 'LIE4SO3 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIE4SO3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIE4SO3 NIL
    (PROG (S TT Q R ALPHA)
      (SETQ S
              (AEVAL
               (LIST 'QUOTIENT (LIST 'FF 2 2) (LIST 'ABS (LIST 'FF 2 2)))))
      (SETQ TT
              (AEVAL
               (LIST 'ABS
                     (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                           (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))))
      (SETQ R
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'FF 1 1)
                     (LIST 'TIMES (LIST 'FF 1 2)
                           (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2))))))
      (SETQ ALPHA
              (AEVAL
               (LIST 'TIMES TT
                     (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                           (LIST 'TIMES
                                 (LIST 'PLUS
                                       (LIST 'EXPT
                                             (LIST 'QUOTIENT (LIST 'FF 2 1)
                                                   (LIST 'FF 2 2))
                                             2)
                                       (LIST 'QUOTIENT (LIST 'FF 3 1)
                                             (LIST 'FF 2 2)))
                                 TT)))))
      (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT ALPHA))))
      (SETK (LIST 'L_A 1 1)
            (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
      (SETK (LIST 'L_A 1 2)
            (SETK (LIST 'L_A 1 3) (SETK (LIST 'L_A 2 2) (AEVAL 0))))
      (SETK (LIST 'L_A 2 1) (AEVAL (LIST 'TIMES Q R)))
      (SETK (LIST 'L_A 2 3)
            (AEVAL
             (LIST 'MINUS (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2))))))
      (SETK (LIST 'L_A 3 1)
            (AEVAL
             (LIST 'MINUS
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2))))))
      (SETK (LIST 'L_A 3 2)
            (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
      (SETK (LIST 'L_A 3 3)
            (AEVAL
             (LIST 'TIMES Q S (LIST 'SQRT TT)
                   (LIST 'QUOTIENT (LIST 'FF 1 2) (LIST 'FF 2 2))))))) 
(PUT 'LIE4SO21 'NUMBER-OF-ARGS 0) 
(FLAG '(LIE4SO21) 'OPFN) 
(PUT 'LIE4SO21 'DEFINED-ON-LINE '1177) 
(PUT 'LIE4SO21 'DEFINED-IN-FILE 'MISC/LIE1234.RED) 
(PUT 'LIE4SO21 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIE4SO21 NIL
    (PROG (GAM EPS S TT Q R ALPHA)
      (AEVAL (MATRIX (LIST (LIST 'L_G 3 3))))
      (SETK 'L_A (AEVAL (LIST 'MAT (LIST 1 0 0) (LIST 0 1 0) (LIST 0 0 1))))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'FF 2 2)) 0)
        (COND
         ((EVALNEQ (AEVAL (LIST 'FF 1 3)) 0)
          (PROGN
           (SETK 'L_A
                 (AEVAL (LIST 'MAT (LIST 1 0 0) (LIST 0 0 1) (LIST 0 1 0))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'FF 2 1)))
           (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'FF 2 3)))
           (SETK (LIST 'L_G 1 3) (AEVAL (LIST 'FF 2 2)))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'FF 1 1)))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'FF 1 3)))
           (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'FF 1 2)))
           (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'MINUS (LIST 'FF 3 1))))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'MINUS (LIST 'FF 3 3))))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'MINUS (LIST 'FF 3 2))))
           (SETK 'FF (AEVAL 'L_G))))
         ((EVALNEQ (AEVAL (LIST 'FF 3 1)) 0)
          (PROGN
           (SETK 'L_A
                 (AEVAL (LIST 'MAT (LIST 0 1 0) (LIST 1 0 0) (LIST 0 0 1))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'MINUS (LIST 'FF 1 2))))
           (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'MINUS (LIST 'FF 1 1))))
           (SETK (LIST 'L_G 1 3) (AEVAL (LIST 'MINUS (LIST 'FF 1 3))))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'FF 3 2)))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'FF 3 1)))
           (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'FF 3 3)))
           (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'FF 2 2)))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'FF 2 1)))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'FF 2 3)))
           (SETK 'FF (AEVAL 'L_G))))
         (T
          (PROGN
           (SETK 'L_A
                 (AEVAL (LIST 'MAT (LIST 1 0 1) (LIST 1 0 0) (LIST 0 1 0))))
           (SETK (LIST 'L_G 1 1) (AEVAL (LIST 'MINUS (LIST 'FF 2 3))))
           (SETK (LIST 'L_G 1 2)
                 (AEVAL (LIST 'DIFFERENCE (LIST 'FF 2 3) (LIST 'FF 2 1))))
           (SETK (LIST 'L_G 1 3) (AEVAL 0))
           (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'MINUS (LIST 'FF 3 3))))
           (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES 2 (LIST 'FF 1 1))))
           (SETK (LIST 'L_G 2 3)
                 (AEVAL (LIST 'DIFFERENCE (LIST 'FF 1 2) (LIST 'FF 3 2))))
           (SETK (LIST 'L_G 3 1) (AEVAL 0))
           (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'FF 1 1)))
           (SETK (LIST 'L_G 3 3) (AEVAL (LIST 'FF 1 2)))
           (SETK 'FF (AEVAL 'L_G)))))))
      (COND
       ((EVALEQUAL
         (AEVAL
          (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))
         0)
        (PROGN
         (SETQ GAM
                 (AEVAL
                  (LIST 'MINUS
                        (LIST 'QUOTIENT (LIST 'FF 1 2) (LIST 'FF 2 2)))))
         (SETQ EPS
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'FF 1 1)
                        (LIST 'TIMES (LIST 'FF 1 2)
                              (LIST 'QUOTIENT (LIST 'FF 2 1)
                                    (LIST 'FF 2 2))))))
         (COND
          ((EVALEQUAL
            (AEVAL
             (LIST 'DIFFERENCE
                   (LIST 'TIMES (LIST 'QUOTIENT 1 4)
                         (LIST 'PLUS (LIST 'EXPT (LIST 'FF 3 2) 2)
                               (LIST 'TIMES (LIST 'FF 3 1) (LIST 'FF 2 2))))
                   (LIST 'TIMES EPS (LIST 'QUOTIENT (LIST 'FF 2 2) 2))))
            0)
           (PROGN
            (SETK 'L_A
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT (LIST 0 0 1)
                               (LIST 0 (LIST 'QUOTIENT 2 EPS)
                                     (LIST 'TIMES 2 (LIST 'QUOTIENT GAM EPS)))
                               (LIST 1 0 0))
                         'L_A)))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'DIFFERENCE
                         (LIST 'TIMES 2 GAM
                               (LIST 'QUOTIENT (LIST 'FF 3 2) EPS))
                         (LIST 'FF 3 3))))
            (SETK (LIST 'L_G 1 2) (AEVAL (LIST 'MINUS (LIST 'FF 3 2))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES 2 (LIST 'QUOTIENT (LIST 'FF 3 1) EPS)))))
            (SETK (LIST 'L_G 2 1) (AEVAL 0))
            (SETK (LIST 'L_G 2 2)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES EPS (LIST 'QUOTIENT (LIST 'FF 2 2) 2)))))
            (SETK (LIST 'L_G 2 3) (AEVAL (LIST 'MINUS (LIST 'FF 2 1))))
            (SETK (LIST 'L_G 3 1) (SETK (LIST 'L_G 3 2) (AEVAL 0)))
            (SETK (LIST 'L_G 3 3) (AEVAL (MINUS 2)))
            (SETK 'FF (AEVAL 'L_G))))
          (T
           (PROGN
            (SETK 'L_A
                  (AEVAL
                   (LIST 'TIMES
                         (LIST 'MAT
                               (LIST (LIST 'QUOTIENT 1 2) 0
                                     (LIST 'QUOTIENT 1 2))
                               (LIST 0 (LIST 'QUOTIENT 1 EPS)
                                     (LIST 'QUOTIENT GAM EPS))
                               (LIST (LIST 'MINUS (LIST 'QUOTIENT 1 2)) 0
                                     (LIST 'QUOTIENT 1 2)))
                         'L_A)))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'FF 3 1) (LIST 'TIMES 2 EPS)))))
            (SETK (LIST 'L_G 1 2)
                  (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'FF 3 2) 2))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'DIFFERENCE
                         (LIST 'QUOTIENT (LIST 'FF 3 1) (LIST 'TIMES 2 EPS))
                         1)))
            (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'QUOTIENT (LIST 'FF 2 1) 2)))
            (SETK (LIST 'L_G 2 2)
                  (AEVAL (LIST 'TIMES (LIST 'FF 2 2) (LIST 'QUOTIENT EPS 2))))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL (LIST 'MINUS (LIST 'QUOTIENT (LIST 'FF 2 1) 2))))
            (SETK (LIST 'L_G 3 1)
                  (AEVAL
                   (LIST 'PLUS
                         (LIST 'QUOTIENT (LIST 'FF 3 1) (LIST 'TIMES 2 EPS))
                         1)))
            (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'QUOTIENT (LIST 'FF 3 2) 2)))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'QUOTIENT (LIST 'FF 3 1) (LIST 'TIMES 2 EPS)))))
            (SETK 'FF (AEVAL 'L_G))))))))
      (COND
       ((NOT
         (EVALNUMBERP
          (AEVAL
           (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                 (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))))
        (PROGN
         (PROGN
          (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
          (ASSGNPRI
           (AEVAL
            (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                  (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))
           NIL NIL)
          (ASSGNPRI (AEVAL "<0 ? (y/n) and press <RETURN>") NIL 'LAST))
         (SETK 'HE (AEVAL (READ)))
         (COND
          ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
           (PROGN
            (SETQ S
                    (AEVAL
                     (LIST 'QUOTIENT (LIST 'FF 2 2)
                           (LIST 'ABS (LIST 'FF 2 2)))))
            (SETQ TT
                    (AEVAL
                     (LIST 'ABS
                           (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                                 (LIST 'TIMES (LIST 'FF 1 3)
                                       (LIST 'FF 2 2))))))
            (SETQ R
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'FF 1 1)
                           (LIST 'TIMES (LIST 'FF 1 2)
                                 (LIST 'QUOTIENT (LIST 'FF 2 1)
                                       (LIST 'FF 2 2))))))
            (SETQ ALPHA
                    (AEVAL
                     (LIST 'TIMES TT
                           (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                                 (LIST 'TIMES
                                       (LIST 'PLUS
                                             (LIST 'EXPT
                                                   (LIST 'QUOTIENT
                                                         (LIST 'FF 2 1)
                                                         (LIST 'FF 2 2))
                                                   2)
                                             (LIST 'QUOTIENT (LIST 'FF 3 1)
                                                   (LIST 'FF 2 2)))
                                       TT)))))
            (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
            (SETK (LIST 'L_G 1 1)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'FF 2 1)
                                     (LIST 'FF 2 2))))))
            (SETK (LIST 'L_G 1 2)
                  (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 3)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'FF 1 2) (LIST 'FF 2 2)))))
            (SETK (LIST 'L_G 2 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 2 2) (SETK (LIST 'L_G 2 3) (AEVAL 0)))
            (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 3 2) (AEVAL 0))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2))))))
            (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
          (T
           (PROGN
            (SETQ S
                    (AEVAL
                     (LIST 'QUOTIENT (LIST 'FF 2 2)
                           (LIST 'ABS (LIST 'FF 2 2)))))
            (SETQ TT
                    (AEVAL
                     (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                           (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2)))))
            (SETQ R
                    (AEVAL
                     (LIST 'DIFFERENCE (LIST 'FF 1 1)
                           (LIST 'TIMES (LIST 'FF 1 2)
                                 (LIST 'QUOTIENT (LIST 'FF 2 1)
                                       (LIST 'FF 2 2))))))
            (SETQ ALPHA
                    (AEVAL
                     (LIST 'TIMES TT
                           (LIST 'DIFFERENCE (LIST 'TIMES R R)
                                 (LIST 'TIMES
                                       (LIST 'PLUS
                                             (LIST 'EXPT
                                                   (LIST 'QUOTIENT
                                                         (LIST 'FF 2 1)
                                                         (LIST 'FF 2 2))
                                                   2)
                                             (LIST 'QUOTIENT (LIST 'FF 3 1)
                                                   (LIST 'FF 2 2)))
                                       TT)))))
            (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
            (COND
             ((NOT (EVALNUMBERP (AEVAL ALPHA)))
              (PROGN
               (PROGN
                (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
                (ASSGNPRI (AEVAL ALPHA) NIL NIL)
                (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
               (SETK 'HE (AEVAL (READ)))
               (COND
                ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
                 (PROGN
                  (SETK (LIST 'L_G 1 1)
                        (AEVAL
                         (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
                  (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
                  (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
                  (SETK (LIST 'L_G 2 2) (AEVAL 0))
                  (SETK (LIST 'L_G 2 3)
                        (AEVAL
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
                  (SETK (LIST 'L_G 3 1)
                        (AEVAL
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'FF 2 1)
                                     (LIST 'FF 2 2)))))
                  (SETK (LIST 'L_G 3 2)
                        (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
                  (SETK (LIST 'L_G 3 3)
                        (AEVAL
                         (LIST 'MINUS
                               (LIST 'TIMES Q S (LIST 'SQRT TT)
                                     (LIST 'QUOTIENT (LIST 'FF 1 2)
                                           (LIST 'FF 2 2))))))
                  (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
                (T
                 (PROGN
                  (SETK (LIST 'L_G 1 1)
                        (AEVAL
                         (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
                  (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
                  (SETK (LIST 'L_G 2 1)
                        (AEVAL
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'FF 2 1)
                                     (LIST 'FF 2 2)))))
                  (SETK (LIST 'L_G 2 2)
                        (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
                  (SETK (LIST 'L_G 2 3)
                        (AEVAL
                         (LIST 'MINUS
                               (LIST 'TIMES Q S (LIST 'SQRT TT)
                                     (LIST 'QUOTIENT (LIST 'FF 1 2)
                                           (LIST 'FF 2 2))))))
                  (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
                  (SETK (LIST 'L_G 3 2) (AEVAL 0))
                  (SETK (LIST 'L_G 3 3)
                        (AEVAL
                         (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
                  (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A))))))))
             ((EVALGREATERP (AEVAL ALPHA) 0)
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
               (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 2 2) (AEVAL 0))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 3 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'FF 1 2)
                                        (LIST 'FF 2 2))))))
               (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
             (T
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
               (SETK (LIST 'L_G 2 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'FF 1 2)
                                        (LIST 'FF 2 2))))))
               (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 3 2) (AEVAL 0))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
               (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))))))))
       ((EVALLESSP
         (AEVAL
          (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))
         0)
        (PROGN
         (SETQ S
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'FF 2 2) (LIST 'ABS (LIST 'FF 2 2)))))
         (SETQ TT
                 (AEVAL
                  (LIST 'ABS
                        (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                              (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2))))))
         (SETQ R
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'FF 1 1)
                        (LIST 'TIMES (LIST 'FF 1 2)
                              (LIST 'QUOTIENT (LIST 'FF 2 1)
                                    (LIST 'FF 2 2))))))
         (SETQ ALPHA
                 (AEVAL
                  (LIST 'TIMES TT
                        (LIST 'DIFFERENCE (LIST 'MINUS (LIST 'TIMES R R))
                              (LIST 'TIMES
                                    (LIST 'PLUS
                                          (LIST 'EXPT
                                                (LIST 'QUOTIENT (LIST 'FF 2 1)
                                                      (LIST 'FF 2 2))
                                                2)
                                          (LIST 'QUOTIENT (LIST 'FF 3 1)
                                                (LIST 'FF 2 2)))
                                    TT)))))
         (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
         (SETK (LIST 'L_G 1 1)
               (AEVAL
                (LIST 'MINUS
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2))))))
         (SETK (LIST 'L_G 1 2)
               (AEVAL (LIST 'MINUS (LIST 'TIMES Q S (LIST 'SQRT TT)))))
         (SETK (LIST 'L_G 1 3)
               (AEVAL
                (LIST 'TIMES Q S (LIST 'SQRT TT)
                      (LIST 'QUOTIENT (LIST 'FF 1 2) (LIST 'FF 2 2)))))
         (SETK (LIST 'L_G 2 1)
               (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
         (SETK (LIST 'L_G 2 2) (SETK (LIST 'L_G 2 3) (AEVAL 0)))
         (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
         (SETK (LIST 'L_G 3 2) (AEVAL 0))
         (SETK (LIST 'L_G 3 3)
               (AEVAL
                (LIST 'MINUS
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2))))))
         (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
       (T
        (PROGN
         (SETQ S
                 (AEVAL
                  (LIST 'QUOTIENT (LIST 'FF 2 2) (LIST 'ABS (LIST 'FF 2 2)))))
         (SETQ TT
                 (AEVAL
                  (LIST 'PLUS (LIST 'EXPT (LIST 'FF 1 2) 2)
                        (LIST 'TIMES (LIST 'FF 1 3) (LIST 'FF 2 2)))))
         (SETQ R
                 (AEVAL
                  (LIST 'DIFFERENCE (LIST 'FF 1 1)
                        (LIST 'TIMES (LIST 'FF 1 2)
                              (LIST 'QUOTIENT (LIST 'FF 2 1)
                                    (LIST 'FF 2 2))))))
         (SETQ ALPHA
                 (AEVAL
                  (LIST 'TIMES TT
                        (LIST 'DIFFERENCE (LIST 'TIMES R R)
                              (LIST 'TIMES
                                    (LIST 'PLUS
                                          (LIST 'EXPT
                                                (LIST 'QUOTIENT (LIST 'FF 2 1)
                                                      (LIST 'FF 2 2))
                                                2)
                                          (LIST 'QUOTIENT (LIST 'FF 3 1)
                                                (LIST 'FF 2 2)))
                                    TT)))))
         (SETQ Q (AEVAL (LIST 'QUOTIENT 1 (LIST 'SQRT (LIST 'ABS ALPHA)))))
         (COND
          ((NOT (EVALNUMBERP (AEVAL ALPHA)))
           (PROGN
            (PROGN
             (ASSGNPRI (AEVAL "Is ") NIL 'FIRST)
             (ASSGNPRI (AEVAL ALPHA) NIL NIL)
             (ASSGNPRI (AEVAL ">0 ? (y/n) and press <RETURN>") NIL 'LAST))
            (SETK 'HE (AEVAL (READ)))
            (COND
             ((EVALEQUAL (AEVAL 'HE) (AEVAL 'Y))
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
               (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 2 2) (AEVAL 0))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 3 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'FF 1 2)
                                        (LIST 'FF 2 2))))))
               (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
             (T
              (PROGN
               (SETK (LIST 'L_G 1 1)
                     (AEVAL
                      (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
               (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
               (SETK (LIST 'L_G 2 1)
                     (AEVAL
                      (LIST 'TIMES Q S (LIST 'SQRT TT)
                            (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
               (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
               (SETK (LIST 'L_G 2 3)
                     (AEVAL
                      (LIST 'MINUS
                            (LIST 'TIMES Q S (LIST 'SQRT TT)
                                  (LIST 'QUOTIENT (LIST 'FF 1 2)
                                        (LIST 'FF 2 2))))))
               (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
               (SETK (LIST 'L_G 3 2) (AEVAL 0))
               (SETK (LIST 'L_G 3 3)
                     (AEVAL
                      (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
               (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A))))))))
          ((EVALGREATERP (AEVAL ALPHA) 0)
           (PROGN
            (SETK (LIST 'L_G 1 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
            (SETK (LIST 'L_G 2 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 2 2) (AEVAL 0))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
            (SETK (LIST 'L_G 3 1)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
            (SETK (LIST 'L_G 3 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'FF 1 2)
                                     (LIST 'FF 2 2))))))
            (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))
          (T
           (PROGN
            (SETK (LIST 'L_G 1 1)
                  (AEVAL (LIST 'QUOTIENT 1 (LIST 'TIMES S (LIST 'SQRT TT)))))
            (SETK (LIST 'L_G 1 2) (SETK (LIST 'L_G 1 3) (AEVAL 0)))
            (SETK (LIST 'L_G 2 1)
                  (AEVAL
                   (LIST 'TIMES Q S (LIST 'SQRT TT)
                         (LIST 'QUOTIENT (LIST 'FF 2 1) (LIST 'FF 2 2)))))
            (SETK (LIST 'L_G 2 2) (AEVAL (LIST 'TIMES Q S (LIST 'SQRT TT))))
            (SETK (LIST 'L_G 2 3)
                  (AEVAL
                   (LIST 'MINUS
                         (LIST 'TIMES Q S (LIST 'SQRT TT)
                               (LIST 'QUOTIENT (LIST 'FF 1 2)
                                     (LIST 'FF 2 2))))))
            (SETK (LIST 'L_G 3 1) (AEVAL (LIST 'TIMES Q R)))
            (SETK (LIST 'L_G 3 2) (AEVAL 0))
            (SETK (LIST 'L_G 3 3)
                  (AEVAL (LIST 'TIMES Q (LIST 'QUOTIENT TT (LIST 'FF 2 2)))))
            (SETK 'L_A (AEVAL (LIST 'TIMES 'L_G 'L_A)))))))))
      (AEVAL (CLEAR (LIST 'L_G))))) 
(ENDMODULE) 