(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'VECPOLY)) 
(FLUID '(CURRENT-MODULUS SAFE-FLAG)) 
(SETQ SAFE-FLAG (CARCHECK 0)) 
(PUT 'COPY-VECTOR 'NUMBER-OF-ARGS 3) 
(PUT 'COPY-VECTOR 'DEFINED-ON-LINE '42) 
(PUT 'COPY-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'COPY-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE COPY-VECTOR (A DA B)
    (PROGN
     (PROG (I)
       (SETQ I 0)
      LAB
       (COND ((MINUSP (DIFFERENCE DA I)) (RETURN NIL)))
       (PUTV B I (GETV A I))
       (SETQ I (PLUS2 I 1))
       (GO LAB))
     DA)) 
(PUT 'TIMES-IN-VECTOR 'NUMBER-OF-ARGS 5) 
(PUT 'TIMES-IN-VECTOR 'DEFINED-ON-LINE '48) 
(PUT 'TIMES-IN-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'TIMES-IN-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TIMES-IN-VECTOR (A DA B DB C)
    (PROG (DC IC W)
      (COND ((OR (ILESSP DA 0) (ILESSP DB 0)) (RETURN -1)))
      (SETQ DC (IPLUS2 DA DB))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE DC I)) (RETURN NIL)))
        (PUTV C I 0)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (IA)
        (SETQ IA 0)
       LAB
        (COND ((MINUSP (DIFFERENCE DA IA)) (RETURN NIL)))
        (PROGN
         (SETQ W (GETV A IA))
         (PROG (IB)
           (SETQ IB 0)
          LAB
           (COND ((MINUSP (DIFFERENCE DB IB)) (RETURN NIL)))
           (PROGN
            (SETQ IC (IPLUS2 IA IB))
            (PUTV C IC
                  (PROG (RESULT)
                    (SETQ RESULT
                            (IPLUS2 (GETV C IC)
                                    (REMAINDER (TIMES W (GETV B IB))
                                               CURRENT-MODULUS)))
                    (COND
                     ((NOT (ILESSP RESULT CURRENT-MODULUS))
                      (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                    (RETURN RESULT))))
           (SETQ IB (PLUS2 IB 1))
           (GO LAB)))
        (SETQ IA (PLUS2 IA 1))
        (GO LAB))
      (RETURN DC))) 
(PUT 'QUOTFAIL-IN-VECTOR 'NUMBER-OF-ARGS 4) 
(PUT 'QUOTFAIL-IN-VECTOR 'DEFINED-ON-LINE '66) 
(PUT 'QUOTFAIL-IN-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'QUOTFAIL-IN-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUOTFAIL-IN-VECTOR (A DA B DB)
    (COND ((ILESSP DA 0) DA)
          ((ILESSP DB 0) (ERRORF "Attempt to divide by zero"))
          ((ILESSP DA DB) (ERRORF "Bad degrees in quotfail-in-vector"))
          (T
           (PROG (DC)
             (SETQ DC (IDIFFERENCE DA DB))
             (PROG (I)
               (SETQ I DC)
              LAB
               (COND
                ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
               (PROG (Q)
                 (SETQ Q
                         (REMAINDER
                          (TIMES (GETV A (IPLUS2 DB I))
                                 (MODULAR-RECIPROCAL (GETV B DB)))
                          CURRENT-MODULUS))
                 (PROG (J)
                   (SETQ J 0)
                  LAB
                   (COND
                    ((MINUSP (DIFFERENCE (IDIFFERENCE DB 1) J)) (RETURN NIL)))
                   (PUTV A (IPLUS2 I J)
                         (PROG (RESULT)
                           (SETQ RESULT
                                   (IDIFFERENCE (GETV A (IPLUS2 I J))
                                                (REMAINDER (TIMES Q (GETV B J))
                                                           CURRENT-MODULUS)))
                           (COND
                            ((IMINUSP RESULT)
                             (SETQ RESULT (IPLUS2 RESULT CURRENT-MODULUS))))
                           (RETURN RESULT)))
                   (SETQ J (PLUS2 J 1))
                   (GO LAB))
                 (PUTV A (IPLUS2 DB I) Q))
               (SETQ I (PLUS2 I (MINUS 1)))
               (GO LAB))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE (IDIFFERENCE DB 1) I)) (RETURN NIL)))
               (COND
                ((NEQ (GETV A I) 0)
                 (ERRORF "Quotient not exact in quotfail!-in!-vector")))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE DC I)) (RETURN NIL)))
               (PUTV A I (GETV A (IPLUS2 DB I)))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN DC))))) 
(PUT 'REMAINDER-IN-VECTOR 'NUMBER-OF-ARGS 4) 
(PUT 'REMAINDER-IN-VECTOR 'DEFINED-ON-LINE '91) 
(PUT 'REMAINDER-IN-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'REMAINDER-IN-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE REMAINDER-IN-VECTOR (A DA B DB)
    (PROG (DELTA DB-1 RECIP-LC-B W)
      (COND ((EQUAL DB 0) (RETURN -1))
            ((EQUAL DB -1) (ERRORF "Attempt to divide by zero")))
      (SETQ RECIP-LC-B
              ((LAMBDA (A)
                 (COND ((EQUAL A 0) A) (T (IDIFFERENCE CURRENT-MODULUS A))))
               (MODULAR-RECIPROCAL (GETV B DB))))
      (SETQ DB-1 (IDIFFERENCE DB 1))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NOT (ILESSP (SETQ DELTA (IDIFFERENCE DA DB)) 0)))
          (RETURN NIL)))
        (PROGN
         (SETQ W (REMAINDER (TIMES RECIP-LC-B (GETV A DA)) CURRENT-MODULUS))
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((MINUSP (DIFFERENCE DB-1 I)) (RETURN NIL)))
           (PUTV A (IPLUS2 I DELTA)
                 (PROG (RESULT)
                   (SETQ RESULT
                           (IPLUS2 (GETV A (IPLUS2 I DELTA))
                                   (REMAINDER (TIMES (GETV B I) W)
                                              CURRENT-MODULUS)))
                   (COND
                    ((NOT (ILESSP RESULT CURRENT-MODULUS))
                     (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                   (RETURN RESULT)))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ DA (IDIFFERENCE DA 1))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND (NOT (ILESSP DA 0)) (EQUAL (GETV A DA) 0)))
             (RETURN NIL)))
           (SETQ DA (IDIFFERENCE DA 1))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN DA))) 
(PUT 'EVALUATE-IN-VECTOR 'NUMBER-OF-ARGS 3) 
(PUT 'EVALUATE-IN-VECTOR 'DEFINED-ON-LINE '110) 
(PUT 'EVALUATE-IN-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'EVALUATE-IN-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EVALUATE-IN-VECTOR (A DA N)
    (PROG (R)
      (SETQ R (GETV A DA))
      (PROG (I)
        (SETQ I (IDIFFERENCE DA 1))
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 0 I))) (RETURN NIL)))
        (SETQ R
                (PROG (RESULT)
                  (SETQ RESULT
                          (IPLUS2 (GETV A I)
                                  (REMAINDER (TIMES R N) CURRENT-MODULUS)))
                  (COND
                   ((NOT (ILESSP RESULT CURRENT-MODULUS))
                    (SETQ RESULT (IDIFFERENCE RESULT CURRENT-MODULUS))))
                  (RETURN RESULT)))
        (SETQ I (PLUS2 I (MINUS 1)))
        (GO LAB))
      (RETURN R))) 
(PUT 'GCD-IN-VECTOR 'NUMBER-OF-ARGS 4) 
(PUT 'GCD-IN-VECTOR 'DEFINED-ON-LINE '121) 
(PUT 'GCD-IN-VECTOR 'DEFINED-IN-FILE 'FACTOR/VECPOLY.RED) 
(PUT 'GCD-IN-VECTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GCD-IN-VECTOR (A DA B DB)
    (PROG (W)
      (COND ((OR (EQUAL DA 0) (EQUAL DB 0)) (PROGN (PUTV A 0 1) (RETURN 0)))
            ((OR (ILESSP DA 0) (ILESSP DB 0))
             (ERRORF "GCD with zero not allowed")))
     TOP
      (SETQ DA (REMAINDER-IN-VECTOR A DA B DB))
      (COND ((EQUAL DA 0) (PROGN (PUTV A 0 1) (RETURN 0)))
            ((EQUAL DA -1)
             (PROGN
              (SETQ W (MODULAR-RECIPROCAL (GETV B DB)))
              (PROG (I)
                (SETQ I 0)
               LAB
                (COND ((MINUSP (DIFFERENCE DB I)) (RETURN NIL)))
                (PUTV A I (REMAINDER (TIMES (GETV B I) W) CURRENT-MODULUS))
                (SETQ I (PLUS2 I 1))
                (GO LAB))
              (RETURN DB))))
      (SETQ DB (REMAINDER-IN-VECTOR B DB A DA))
      (COND ((EQUAL DB 0) (PROGN (PUTV A 0 1) (RETURN 0)))
            ((EQUAL DB -1)
             (PROGN
              (SETQ W (MODULAR-RECIPROCAL (GETV A DA)))
              (COND
               ((NOT (EQUAL W 1))
                (PROG (I)
                  (SETQ I 0)
                 LAB
                  (COND ((MINUSP (DIFFERENCE DA I)) (RETURN NIL)))
                  (PUTV A I (REMAINDER (TIMES (GETV A I) W) CURRENT-MODULUS))
                  (SETQ I (PLUS2 I 1))
                  (GO LAB))))
              (RETURN DA))))
      (GO TOP))) 
(CARCHECK SAFE-FLAG) 
(ENDMODULE) 