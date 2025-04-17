(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'MODSOLVE)) 
(FLUID '(*TRNONLNR CURRENT-MODULUS)) 
(LOAD-PACKAGE 'SOLVE) 
(LOAD-PACKAGE 'FACTOR) 
(PUT 'M_SOLVE 'PSOPFN 'MSOLVE) 
(PUT 'MSOLVE 'NUMBER-OF-ARGS 1) 
(PUT 'MSOLVE 'DEFINED-ON-LINE '46) 
(PUT 'MSOLVE 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MSOLVE (U)
    (PROG (S S1 V V1 W)
      (SETQ S (REVAL1 (CAR U) T))
      (SETQ S (COND ((EQCAR S 'LIST) (CDR S)) (T (LIST S))))
      (COND
       ((CDR U)
        (PROGN
         (SETQ V (REVAL1 (CADR U) T))
         (SETQ V (COND ((EQCAR V 'LIST) (CDR V)) (T (LIST V)))))))
      (SETQ S1
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q S)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Q)
                                    (PROGN
                                     (COND
                                      ((EQCAR Q 'EQUAL)
                                       (SETQ Q (CONS 'DIFFERENCE (CDR Q)))))
                                     (SETQ W (CONS (CAR (SIMP Q)) 1))
                                     (SETQ V1 (UNION V1 (SOLVEVARS (LIST W))))
                                     (CAR W)))
                                  (CAR Q))
                                 NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Q)
                            (PROGN
                             (COND
                              ((EQCAR Q 'EQUAL)
                               (SETQ Q (CONS 'DIFFERENCE (CDR Q)))))
                             (SETQ W (CONS (CAR (SIMP Q)) 1))
                             (SETQ V1 (UNION V1 (SOLVEVARS (LIST W))))
                             (CAR W)))
                          (CAR Q))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL V) (SETQ V V1)))
      (RETURN
       (MSOLVE-RESULT
        (COND ((NULL (CDR S1)) (MSOLVE-POLY (CAR S1) V))
              (T (MSOLVE-PSYS S1 V))))))) 
(PUT 'MSOLVE-RESULT 'NUMBER-OF-ARGS 1) 
(PUT 'MSOLVE-RESULT 'DEFINED-ON-LINE '64) 
(PUT 'MSOLVE-RESULT 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-RESULT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MSOLVE-RESULT (U)
    (COND ((EQUAL U 'FAILED) U)
          (T
           (CONS 'LIST
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V U)
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V)
                                       (CONS 'LIST
                                             (PROG (W FORALL-RESULT
                                                    FORALL-ENDPTR)
                                               (SETQ W V)
                                               (COND ((NULL W) (RETURN NIL)))
                                               (SETQ FORALL-RESULT
                                                       (SETQ FORALL-ENDPTR
                                                               (CONS
                                                                ((LAMBDA (W)
                                                                   (LIST 'EQUAL
                                                                         (CAR
                                                                          W)
                                                                         (CDR
                                                                          W)))
                                                                 (CAR W))
                                                                NIL)))
                                              LOOPLABEL
                                               (SETQ W (CDR W))
                                               (COND
                                                ((NULL W)
                                                 (RETURN FORALL-RESULT)))
                                               (RPLACD FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (W)
                                                           (LIST 'EQUAL (CAR W)
                                                                 (CDR W)))
                                                         (CAR W))
                                                        NIL))
                                               (SETQ FORALL-ENDPTR
                                                       (CDR FORALL-ENDPTR))
                                               (GO LOOPLABEL))))
                                     (CAR V))
                                    NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (V)
                               (CONS 'LIST
                                     (PROG (W FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ W V)
                                       (COND ((NULL W) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (W)
                                                           (LIST 'EQUAL (CAR W)
                                                                 (CDR W)))
                                                         (CAR W))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ W (CDR W))
                                       (COND ((NULL W) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (W)
                                                   (LIST 'EQUAL (CAR W)
                                                         (CDR W)))
                                                 (CAR W))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))))
                             (CAR V))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'MSOLVESYS 'NUMBER-OF-ARGS 3) 
(PUT 'MSOLVESYS 'DEFINED-ON-LINE '69) 
(PUT 'MSOLVESYS 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVESYS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MSOLVESYS (S1 V TG)
    (PROG (W FAIL)
      (COND
       ((NULL (CDR S1)) (PROGN (SETQ W (MSOLVE-POLY (CAR S1) V)) (GO DONE))))
      (PROG (P)
        (SETQ P S1)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROG (X)
             (SETQ X (KERNELS P))
            LAB
             (COND ((NULL X) (RETURN NIL)))
             ((LAMBDA (X) (COND ((NOT (MEMBER X V)) (SETQ FAIL T)))) (CAR X))
             (SETQ X (CDR X))
             (GO LAB)))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND
       (FAIL
        (PROGN
         (COND (*TRNONLNR (LPRIM "Cannot solve parametric modular system")))
         (GO FAILED))))
      (SETQ W (MSOLVE-PSYS S1 V))
     DONE
      (COND ((EQUAL W 'FAILED) (GO FAILED)))
      (SETQ W
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q W)
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Q)
                                    (LIST
                                     (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ R Q)
                                       (COND ((NULL R) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (R)
                                                           (SIMP (CDR R)))
                                                         (CAR R))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ R (CDR R))
                                       (COND ((NULL R) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (R) (SIMP (CDR R)))
                                                 (CAR R))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))
                                     (PROG (R FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ R Q)
                                       (COND ((NULL R) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (R) (CAR R))
                                                         (CAR R))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ R (CDR R))
                                       (COND ((NULL R) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (R) (CAR R)) (CAR R))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL))
                                     1))
                                  (CAR Q))
                                 NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Q)
                            (LIST
                             (PROG (R FORALL-RESULT FORALL-ENDPTR)
                               (SETQ R Q)
                               (COND ((NULL R) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (R) (SIMP (CDR R)))
                                                 (CAR R))
                                                NIL)))
                              LOOPLABEL
                               (SETQ R (CDR R))
                               (COND ((NULL R) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (R) (SIMP (CDR R))) (CAR R))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             (PROG (R FORALL-RESULT FORALL-ENDPTR)
                               (SETQ R Q)
                               (COND ((NULL R) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (R) (CAR R)) (CAR R))
                                                NIL)))
                              LOOPLABEL
                               (SETQ R (CDR R))
                               (COND ((NULL R) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS ((LAMBDA (R) (CAR R)) (CAR R))
                                             NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             1))
                          (CAR Q))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (COND (TG (CONS T W)) (T W)))
     FAILED
      (RETURN
       (COND
        ((AND (NULL (CDR S1)) (NULL (CDR V)) (NULL TG))
         (MKROOTSOF (CONS (CAR S1) 1) (CAR V) 1))
        (TG '(FAILED)) (T 'FAILED))))) 
(PUT 'MSOLVE-POLY1 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-POLY1 'DEFINED-ON-LINE '95) 
(PUT 'MSOLVE-POLY1 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-POLY1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-POLY1 (F X)
    (PROG (W L)
      (COND ((OR (ATOM F) (ATOM (CAR F))) NIL)
            ((EQUAL (CDAAR F) 1)
             (PROGN
              (SETQ W (SAFE-MODRECIP (CDAR F)))
              (SETQ ERFG* NIL)
              (COND ((NULL W) (GO ENUM)))
              (SETQ W
                      (MODUNTAG
                       ((LAMBDA (G549)
                          (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G549))
                                (T (POLY-MULTF W G549))))
                        (NEGF (CDR F)))))
              (COND
               ((AND W (OR (LESSP W 0) (GREATERP W CURRENT-MODULUS)))
                (SETQ W (GENERAL-MODULAR-NUMBER W))))
              (SETQ W (LIST W))
              (GO DONE))))
     ENUM
      (SETQ L (LOWESTDEG1 F X 0))
      (COND
       ((GREATERP L 0)
        (SETQ F ((LAMBDA (*EXP) (QUOTF1 F (CAR (SIMP (LIST 'EXPT X L))))) T))))
      (SETQ F (GENERAL-REDUCE-MOD-P (MODUNTAG F)))
      (SETQ W
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE CURRENT-MODULUS 1) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (COND
                         ((NULL (GENERAL-EVALUATE-MOD-P F X I)) (LIST I))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE CURRENT-MODULUS 1) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (COND
                         ((NULL (GENERAL-EVALUATE-MOD-P F X I)) (LIST I))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (COND ((GREATERP L 0) (SETQ W (APPEND W (LIST NIL)))))
     DONE
      (RETURN
       (PROG (Q FORALL-RESULT FORALL-ENDPTR)
         (SETQ Q W)
         (COND ((NULL Q) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (Q) (LIST (CONS X (PREPF Q)))) (CAR Q))
                               NIL)))
        LOOPLABEL
         (SETQ Q (CDR Q))
         (COND ((NULL Q) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (Q) (LIST (CONS X (PREPF Q)))) (CAR Q)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'MSOLVE-POLY 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-POLY 'DEFINED-ON-LINE '119) 
(PUT 'MSOLVE-POLY 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-POLY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-POLY (F L)
    (PROG (X VL LIMIT)
      (SETQ LIMIT 10000000)
      (COND
       ((GREATERP CURRENT-MODULUS LIMIT)
        (PROGN
         (COND (*TRNONLNR (LPRIM (LIST "Current modulus larger than" LIMIT))))
         (RETURN 'FAILED))))
      (SETQ VL (KERNELS F))
      (PROG (X)
        (SETQ X L)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND ((NOT (MEMBER X VL)) (SETQ L (DELETE X L))))
            (SETQ VL (DELETE X VL))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND ((NULL L) (RETURN NIL)))
      (RETURN (COND (VL (MSOLVE-POLYA F L)) (T (MSOLVE-POLYN F L)))))) 
(PUT 'MSOLVE-POLYN 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-POLYN 'DEFINED-ON-LINE '134) 
(PUT 'MSOLVE-POLYN 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-POLYN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-POLYN (F L)
    ((LAMBDA (X)
       (COND ((NULL (CDR L)) (MSOLVE-POLY1 F (CAR L)))
             (T
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
               STARTOVER
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE CURRENT-MODULUS 1) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                          (SETQ S
                                  (MSOLVE-POLYN
                                   (CAR (SUBF F (LIST (CONS X I)))) (CDR L)))
                          (COND ((NULL S) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S) (CONS (CONS X I) S))
                                            (CAR S))
                                           NIL)))
                         LOOPLABEL
                          (SETQ S (CDR S))
                          (COND ((NULL S) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (S) (CONS (CONS X I) S)) (CAR S))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((MINUSP (DIFFERENCE (DIFFERENCE CURRENT-MODULUS 1) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                          (SETQ S
                                  (MSOLVE-POLYN
                                   (CAR (SUBF F (LIST (CONS X I)))) (CDR L)))
                          (COND ((NULL S) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S) (CONS (CONS X I) S))
                                            (CAR S))
                                           NIL)))
                         LOOPLABEL
                          (SETQ S (CDR S))
                          (COND ((NULL S) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (S) (CONS (CONS X I) S)) (CAR S))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))))
     (CAR L))) 
(PUT 'MSOLVE-POLYA 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-POLYA 'DEFINED-ON-LINE '140) 
(PUT 'MSOLVE-POLYA 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-POLYA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-POLYA (F L)
    (PROG (X C W)
      (PROG (Y)
        (SETQ Y L)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (COND
            ((NULL X)
             (COND
              ((EQUAL 1
                      (CDAAR ((LAMBDA (KORD*) (SETQ W (REORDER F))) (LIST Y))))
               (SETQ X Y))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (COND ((NULL X) (GO NONE)))
      (SETQ C (CDAR W))
      (SETQ W (CDR W))
      (COND ((NOT (OR (ATOM C) (ATOM (CAR C)))) (GO NONE)))
      (SETQ C (SAFE-MODRECIP C))
      (COND ((NULL C) (GO NONE)))
      (RETURN
       (LIST
        (LIST
         (CONS X
               (PREPF
                ((LAMBDA (G550)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G550 C))
                         (T (POLY-MULTF G550 C))))
                 (NEGF W)))))))
     NONE
      (RETURN
       (LIST
        (LIST
         (CONS (CAR L) (MK*SQ (CAAAR (MKROOTSOF (CONS F 1) (CAR L) 1))))))))) 
(PUT 'MSOLVE-PSYS 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-PSYS 'DEFINED-ON-LINE '157) 
(PUT 'MSOLVE-PSYS 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-PSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-PSYS (S V)
    (PROG (B O Z W)
      (COND
       ((AND (GREATERP (TIMES CURRENT-MODULUS (LENGTH S)) 1000)
             (PRIMEP CURRENT-MODULUS))
        (PROGN
         (LOAD-PACKAGE 'GROEBNER)
         (LOAD-PACKAGE 'GROEBNR2)
         (SETQ O (APPLY1 'TORDER (LIST (CONS 'LIST V) 'LEX)))
         (SETQ B
                 (GROEBNEREVAL
                  (LIST
                   (CONS 'LIST
                         (PROG (P FORALL-RESULT FORALL-ENDPTR)
                           (SETQ P S)
                           (COND ((NULL P) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (P) (PREPF P)) (CAR P))
                                            NIL)))
                          LOOPLABEL
                           (SETQ P (CDR P))
                           (COND ((NULL P) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (P) (PREPF P)) (CAR P)) NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))
         (SETQ Z (GZERODIMEVAL (LIST B)))
         (SETQ S
                 (REVERSIP
                  (PROG (P FORALL-RESULT FORALL-ENDPTR)
                    (SETQ P (CDR B))
                    (COND ((NULL P) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (P) (CAR (SIMP P))) (CAR P))
                                          NIL)))
                   LOOPLABEL
                    (SETQ P (CDR P))
                    (COND ((NULL P) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (P) (CAR (SIMP P))) (CAR P)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
         (APPLY1 'TORDER (CDR O))))
       (T
        (PROGN
         (SETQ W
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P S)
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P)
                                       (CONS
                                        (LENGTH
                                         (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ X V)
                                          STARTOVER
                                           (COND ((NULL X) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   ((LAMBDA (X)
                                                      (COND
                                                       ((SMEMQ X P) (LIST X))))
                                                    (CAR X)))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR FORALL-RESULT))
                                           (SETQ X (CDR X))
                                           (COND
                                            ((ATOM FORALL-ENDPTR)
                                             (GO STARTOVER)))
                                          LOOPLABEL
                                           (COND
                                            ((NULL X) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   ((LAMBDA (X)
                                                      (COND
                                                       ((SMEMQ X P) (LIST X))))
                                                    (CAR X)))
                                           (SETQ FORALL-ENDPTR
                                                   (LASTPAIR FORALL-ENDPTR))
                                           (SETQ X (CDR X))
                                           (GO LOOPLABEL)))
                                        P))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P)
                               (CONS
                                (LENGTH
                                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ X V)
                                  STARTOVER
                                   (COND ((NULL X) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           ((LAMBDA (X)
                                              (COND ((SMEMQ X P) (LIST X))))
                                            (CAR X)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-RESULT))
                                   (SETQ X (CDR X))
                                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                  LOOPLABEL
                                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           ((LAMBDA (X)
                                              (COND ((SMEMQ X P) (LIST X))))
                                            (CAR X)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-ENDPTR))
                                   (SETQ X (CDR X))
                                   (GO LOOPLABEL)))
                                P))
                             (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ W
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (SORT W 'LESSPCAR))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (P) (CDR P)) (CAR P)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (RETURN (MSOLVE-PSYS1 S V)))) 
(PUT 'MSOLVE-PSYS1 'NUMBER-OF-ARGS 2) 
(PUT 'MSOLVE-PSYS1 'DEFINED-ON-LINE '179) 
(PUT 'MSOLVE-PSYS1 'DEFINED-IN-FILE 'SOLVE/MODSOLVE.RED) 
(PUT 'MSOLVE-PSYS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MSOLVE-PSYS1 (S V)
    (PROG (W W1 F F1)
      (SETQ W (LIST NIL))
      (PROG (F)
        (SETQ F S)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ W1 NIL)
            (PROG (S)
              (SETQ S W)
             LAB
              (COND ((NULL S) (RETURN NIL)))
              ((LAMBDA (S)
                 (PROGN
                  (SETQ F1 (GENERAL-REDUCE-MOD-P (MODUNTAG (CAR (SUBF F S)))))
                  (COND ((NULL F1) (SETQ W1 (CONS S W1)))
                        ((OR (ATOM F1) (ATOM (CAR F1))) NIL)
                        (T
                         (PROG (NS)
                           (SETQ NS (MSOLVE-POLY F1 V))
                          LAB
                           (COND ((NULL NS) (RETURN NIL)))
                           ((LAMBDA (NS) (SETQ W1 (CONS (APPEND S NS) W1)))
                            (CAR NS))
                           (SETQ NS (CDR NS))
                           (GO LAB))))))
               (CAR S))
              (SETQ S (CDR S))
              (GO LAB))
            (SETQ W W1)))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN W))) 
(ENDMODULE) 