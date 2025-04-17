(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MODROOTS)) 
(PUT 'MODROOTS0 'NUMBER-OF-ARGS 2) 
(PUT 'MODROOTS0 'DEFINED-ON-LINE '33) 
(PUT 'MODROOTS0 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODROOTS0 (F M)
    (PROG (ML)
      (SETQ ML
              (SORT
               (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                 (SETQ Q (ZFACTOR M))
                STARTOVER
                 (COND ((NULL Q) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (Q)
                            (PROG (I FORALL-RESULT FORALL-ENDPTR)
                              (SETQ I 1)
                              (COND
                               ((MINUSP (DIFFERENCE (CDR Q) I)) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR (CONS (CAR Q) NIL)))
                             LOOPLABEL
                              (SETQ I (PLUS2 I 1))
                              (COND
                               ((MINUSP (DIFFERENCE (CDR Q) I))
                                (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR (CONS (CAR Q) NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Q)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ Q (CDR Q))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL Q) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (Q)
                            (PROG (I FORALL-RESULT FORALL-ENDPTR)
                              (SETQ I 1)
                              (COND
                               ((MINUSP (DIFFERENCE (CDR Q) I)) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR (CONS (CAR Q) NIL)))
                             LOOPLABEL
                              (SETQ I (PLUS2 I 1))
                              (COND
                               ((MINUSP (DIFFERENCE (CDR Q) I))
                                (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR (CONS (CAR Q) NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR Q)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ Q (CDR Q))
                 (GO LOOPLABEL))
               'LESSP))
      (RETURN (SORT (MODROOTS1 F ML) 'LESSP)))) 
(PUT 'MODROOTS1 'NUMBER-OF-ARGS 2) 
(PUT 'MODROOTS1 'DEFINED-ON-LINE '45) 
(PUT 'MODROOTS1 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODROOTS1 (F ML)
    (COND ((NULL (CDR ML)) (MODROOTS2 F (CAR ML) NIL))
          (T
           (PROG (F1 P Q PQ R S X Y)
             (SETQ P (CAR ML))
             (SETQ ML (CDR ML))
             (SETQ R (MODROOTS1 F ML))
             (COND ((NULL R) (RETURN NIL)))
             (SETQ X (CAAAR F))
             (SETQ Y (GENSYM))
             (SETQ Q
                     (PROG (M FORALL-RESULT)
                       (SETQ M ML)
                       (SETQ FORALL-RESULT 1)
                      LAB1
                       (COND ((NULL M) (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (TIMES ((LAMBDA (M) M) (CAR M)) FORALL-RESULT))
                       (SETQ M (CDR M))
                       (GO LAB1)))
             (SETQ PQ (TIMES P Q))
             (PROG (W)
               (SETQ W R)
              LAB
               (COND ((NULL W) (RETURN NIL)))
               ((LAMBDA (W)
                  (PROGN
                   (SETQ F1
                           (CAR
                            (SUBF F
                                  (LIST
                                   (CONS X
                                         (LIST 'PLUS (LIST 'TIMES Y Q) W))))))
                   (PROG (Y)
                     (SETQ Y (MODROOTS2 (REDUCE-MOD-P* F1 P) P T))
                    LAB
                     (COND ((NULL Y) (RETURN NIL)))
                     ((LAMBDA (Y)
                        (PROGN
                         (SETQ Y (MODP (PLUS (TIMES Y Q) W) PQ))
                         (COND
                          ((AND
                            (NULL
                             (REDUCE-MOD-P* (CAR (SUBF F (LIST (CONS X Y))))
                                            PQ))
                            (NOT (MEMBER Y S)))
                           (SETQ S (CONS Y S))))))
                      (CAR Y))
                     (SETQ Y (CDR Y))
                     (GO LAB))
                   NIL))
                (CAR W))
               (SETQ W (CDR W))
               (GO LAB))
             (RETURN S))))) 
(PUT 'MODROOTS2 'NUMBER-OF-ARGS 3) 
(PUT 'MODROOTS2 'DEFINED-ON-LINE '66) 
(PUT 'MODROOTS2 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODROOTS2 (F P REC)
    (COND ((AND (OR (ATOM F) (ATOM (CAR F))) F) NIL)
          ((NULL F)
           (COND ((AND (EQUAL P 2) REC) '(-1 0 1))
                 (T
                  (PROG (I FORALL-RESULT FORALL-ENDPTR)
                    (SETQ I 0)
                    (COND
                     ((MINUSP (DIFFERENCE (DIFFERENCE P 1) I)) (RETURN NIL)))
                    (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                   LOOPLABEL
                    (SETQ I (PLUS2 I 1))
                    (COND
                     ((MINUSP (DIFFERENCE (DIFFERENCE P 1) I))
                      (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR (CONS I NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
          ((EQUAL P 2) (MODROOTS4 F T REC)) (T (MODROOTS3 F P)))) 
(PUT 'X**P-W 'NUMBER-OF-ARGS 3) 
(PUT 'X**P-W 'DEFINED-ON-LINE '74) 
(PUT 'X**P-W 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'X**P-W 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE X**P-W (X P W) (GENERAL-DIFFERENCE-MOD-P (CONS (CONS (CONS X P) 1) NIL) W)) 
(PUT 'MODROOTS3 'NUMBER-OF-ARGS 2) 
(PUT 'MODROOTS3 'DEFINED-ON-LINE '78) 
(PUT 'MODROOTS3 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MODROOTS3 (F CURRENT-MODULUS)
    (PROG (A D P R X N)
      (SETQ N 0)
      (SETQ P CURRENT-MODULUS)
      (SETQ F (GENERAL-REDUCE-MOD-P F))
      (SETQ X (CAAAR F))
      (SETQ A (GENERAL-GCD-MOD-P F (X**P-W X P (LIST (CONS (CONS X 1) 1)))))
      (SETQ D (CDAAR A))
      (SETQ N (LOWESTDEG1 A X 0))
      (COND
       ((GREATERP N 0)
        (PROGN
         (SETQ R '(0))
         (SETQ A (GENERAL-QUOTIENT-MOD-P A (X**P-W X N NIL))))))
      (RETURN (APPEND R (MODROOTS31 A X P))))) 
(PUT 'MODROOTS31 'NUMBER-OF-ARGS 3) 
(PUT 'MODROOTS31 'DEFINED-ON-LINE '99) 
(PUT 'MODROOTS31 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS31 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODROOTS31 (A X P)
    (PROG (A0 A1 A2 B D E S W)
     S2
      (COND ((OR (ATOM A) (ATOM (CAR A))) (RETURN NIL)))
      (COND
       ((EQUAL (CDAAR A) 1)
        (RETURN
         (LIST
          (GENERAL-MODULAR-QUOTIENT
           (COND ((CDR A) (GENERAL-MODULAR-MINUS (CDR A))) (T 0)) (CDAR A))))))
      (COND
       ((EQUAL (CDAAR A) 2)
        (PROGN
         (SETQ A2 (CDAR A))
         (SETQ A (CDR A))
         (COND
          ((NOT (OR (ATOM A) (ATOM (CAR A))))
           (PROGN (SETQ A1 (CDAR A)) (SETQ A (CDR A))))
          (T (SETQ A1 0)))
         (SETQ A0 (COND ((NULL A) 0) (T A)))
         (SETQ D
                 (GENERAL-MODULAR-DIFFERENCE (GENERAL-MODULAR-TIMES A1 A1)
                                             (GENERAL-MODULAR-TIMES 4
                                                                    (GENERAL-MODULAR-TIMES
                                                                     A0 A2))))
         (SETQ S (LEGENDRE-SYMBOL D P))
         (COND ((EQUAL S (MINUS 1)) (RETURN NIL)))
         (SETQ E (MODSQRT D P))
         (SETQ A2 (GENERAL-MODULAR-RECIPROCAL (GENERAL-MODULAR-PLUS A2 A2)))
         (SETQ A1 (GENERAL-MODULAR-MINUS A1))
         (RETURN
          (LIST (GENERAL-MODULAR-TIMES (GENERAL-MODULAR-PLUS A1 E) A2)
                (GENERAL-MODULAR-TIMES (GENERAL-MODULAR-DIFFERENCE A1 E) A2)))
         NIL)))
     S3
      (SETQ E (RANDOM P))
      (SETQ W (X**P-W X (QUOTIENT (DIFFERENCE P 1) 2) 1))
      (SETQ A1
              (GENERAL-REDUCE-MOD-P
               (CAR (SUBF A (LIST (CONS X (LIST 'DIFFERENCE X E)))))))
      (SETQ B (GENERAL-GCD-MOD-P W A1))
      (COND
       ((OR (OR (ATOM B) (ATOM (CAR B))) (EQUAL (CDAAR B) (CDAAR A))) (GO S3)))
     S4
      (RETURN
       (PROG (W FORALL-RESULT FORALL-ENDPTR)
         (SETQ W
                 (UNION (MODROOTS31 (GENERAL-QUOTIENT-MOD-P A1 B) X P)
                        (MODROOTS31 B X P)))
         (COND ((NULL W) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (W) (GENERAL-MODULAR-DIFFERENCE W E))
                           (CAR W))
                          NIL)))
        LOOPLABEL
         (SETQ W (CDR W))
         (COND ((NULL W) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (W) (GENERAL-MODULAR-DIFFERENCE W E)) (CAR W))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MODROOTS4 'NUMBER-OF-ARGS 3) 
(PUT 'MODROOTS4 'DEFINED-ON-LINE '143) 
(PUT 'MODROOTS4 'DEFINED-IN-FILE 'SOLVE/MODROOTS.RED) 
(PUT 'MODROOTS4 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MODROOTS4 (F W REC)
    (COND
     ((OR (ATOM F) (ATOM (CAR F)))
      (PROGN
       (COND (F (SETQ W (NOT W))))
       (APPEND (COND ((NULL F) '(0)))
               (COND (W (COND (REC '(-1 1)) (T '(1))))))))
     (T (MODROOTS4 (CDR F) (NOT W) REC)))) 
(PUT 'M_ROOTS 'PSOPFN
     (FUNCTION
      (LAMBDA (U)
        (CONS 'LIST (MODROOTS0 (CAR (SIMP (CAR U))) (REVAL1 (CADR U) T)))))) 
(ENDMODULE) 