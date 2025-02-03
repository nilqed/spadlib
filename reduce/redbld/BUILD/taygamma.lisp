(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYGAMMA)) 
(EXPORTS (LIST 'TAYSIMPGAMMA 'TAYSIMPPSI 'TAYSIMPPOLYGAMMA)) 
(IMPORTS
 (LIST '*F2Q 'DENR 'EQCAR 'MK*SQ 'MKSQ 'MVAR 'NFACTORIAL 'NUMR 'OVER 'SIMP*
       '*TAY2Q 'CST-TAYLOR* 'MAKE-CST-POWERLIST 'PRUNE-COEFFLIST 'SET-TAYORIG
       'TAYCFPL 'TAYCOEFFLIST 'TAYLOR-KERNEL-SQ-P 'TAYORIG 'TAYTEMPLATE
       'CONFUSION 'TAYLOR-ERROR 'GET-CST-COEFF 'IS-NEG-PL 'ADDTAYLOR 'INVTAYLOR
       'MULTTAYLORSQ 'NEGTAYLOR 'QUOTTAYLOR 'TAYLOREXPAND-DIFF 'EXPTTAYRAT
       'TAYSIMPSQ)) 
(FLUID '(*TAYLORKEEPORIGINAL **TAYLOR-EPSILON** FRLIS*)) 
(PUT 'TAYSIMPGAMMA 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPGAMMA 'DEFINED-ON-LINE '89) 
(PUT 'TAYSIMPGAMMA 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPGAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPGAMMA (U)
    (COND ((OR (NOT (EQCAR U 'GAMMA)) (CDDR U)) (CONFUSION 'TAYSIMPGAMMA))
          (T
           (PROG (L L0 C0 TAY TP)
             (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
             (COND
              ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
               (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
             (SETQ TAY (CAAAR (CAR TAY)))
             (SETQ TP (CADDR TAY))
             (SETQ L0
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL TP)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                 (CAR EL))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ L
                     ((LAMBDA (CFLIS)
                        (PROGN
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT
                              (AND (NOT (NULL CFLIS))
                                   (NULL (CAR (CDR (CAR CFLIS))))))
                             (RETURN NIL)))
                           (SETQ CFLIS (CDR CFLIS))
                           (GO WHILELABEL))
                         CFLIS))
                      (CADR TAY)))
             (COND ((NULL L) (RETURN (TAYSIMPGAMMA* TAY 1))))
             (COND
              ((IS-NEG-PL (CAR (CAR L)))
               (TAYLOR-ERROR 'ESSENTIAL-SINGULARITY (CAR U))))
             (SETQ C0 (GET-CST-COEFF TAY))
             (COND
              ((AND (EQUAL (CDR C0) 1)
                    (OR (NULL (CAR C0))
                        (AND (FIXP (CAR C0)) (LESSP (CAR C0) 0))))
               (RETURN
                (TAYSIMPGAMMA* TAY
                 (COND ((NULL (CAR C0)) 1) (T (DIFFERENCE 1 (CAR C0)))))))
              (T (RETURN (TAYLOREXPAND-DIFF U TP T)))))))) 
(PUT 'TAYSIMPGAMMA* 'NUMBER-OF-ARGS 2) 
(PUT 'TAYSIMPGAMMA* 'DEFINED-ON-LINE '117) 
(PUT 'TAYSIMPGAMMA* 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPGAMMA* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPGAMMA* (TAY COUNT)
    (PROG (TAY1 TAY2 TP)
      (SETQ TP (CADDR TAY))
      (SETQ TAY1
              (ADDTAYLOR TAY
               ((LAMBDA (G209)
                  (LIST 'TAYLOR*
                        (LIST
                         (CONS
                          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EL TP)
                            (COND ((NULL EL) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL)
                                                (NLIST 0 (LENGTH (CAR EL))))
                                              (CAR EL))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EL (CDR EL))
                            (COND ((NULL EL) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                      (CAR EL))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          G209))
                        TP G209 NIL))
                (CONS COUNT 1))))
      (SETQ TAY1 (TAYSIMPGAMMA (LIST 'GAMMA TAY1)))
      (COND
       ((NOT (AND (KERNP TAY1) (EQCAR (CAAAR (CAR TAY1)) 'TAYLOR*)))
        (CONFUSION 'TAYSIMPGAMMA)))
      (SETQ TAY1 (CAAAR (CAR TAY1)))
      (SETQ TAY2 TAY)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE COUNT I)) (RETURN NIL)))
        (PROGN
         (SETQ TAY1 (QUOTTAYLOR TAY1 TAY2))
         (SETQ TAY2
                 (ADDTAYLOR TAY2
                  ((LAMBDA (G211)
                     (LIST 'TAYLOR*
                           (LIST
                            (CONS
                             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                               (SETQ EL TP)
                               (COND ((NULL EL) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (EL)
                                                   (NLIST 0 (LENGTH (CAR EL))))
                                                 (CAR EL))
                                                NIL)))
                              LOOPLABEL
                               (SETQ EL (CDR EL))
                               (COND ((NULL EL) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             G211))
                           TP G211 NIL))
                   (CONS 1 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
        (RPLACA (CDDDR TAY1) (LIST 'GAMMA (CADDDR TAY)))))
      (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY1) 1) 1)) 1)))) 
(PUT 'GAMMA 'TAYLORSIMPFN 'TAYSIMPGAMMA) 
(PUT 'TAYSIMPPSI 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPPSI 'DEFINED-ON-LINE '141) 
(PUT 'TAYSIMPPSI 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPPSI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPPSI (U)
    (COND ((OR (NOT (EQCAR U 'PSI)) (CDDR U)) (CONFUSION 'TAYSIMPPSI))
          (T
           (PROG (L L0 C0 TAY TP)
             (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
             (COND
              ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
               (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
             (SETQ TAY (CAAAR (CAR TAY)))
             (SETQ TP (CADDR TAY))
             (SETQ L0
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL TP)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                 (CAR EL))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ L
                     ((LAMBDA (CFLIS)
                        (PROGN
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT
                              (AND (NOT (NULL CFLIS))
                                   (NULL (CAR (CDR (CAR CFLIS))))))
                             (RETURN NIL)))
                           (SETQ CFLIS (CDR CFLIS))
                           (GO WHILELABEL))
                         CFLIS))
                      (CADR TAY)))
             (COND ((NULL L) (RETURN (TAYSIMPPSI* TAY 1))))
             (COND
              ((IS-NEG-PL (CAR (CAR L)))
               (TAYLOR-ERROR 'ESSENTIAL-SINGULARITY (CAR U))))
             (SETQ C0 (GET-CST-COEFF TAY))
             (COND
              ((AND (EQUAL (CDR C0) 1)
                    (OR (NULL (CAR C0))
                        (AND (FIXP (CAR C0)) (LESSP (CAR C0) 0))))
               (RETURN
                (TAYSIMPPSI* TAY
                 (COND ((NULL (CAR C0)) 1) (T (DIFFERENCE 1 (CAR C0)))))))
              (T (RETURN (TAYLOREXPAND-DIFF U TP T)))))))) 
(PUT 'TAYSIMPPSI* 'NUMBER-OF-ARGS 2) 
(PUT 'TAYSIMPPSI* 'DEFINED-ON-LINE '169) 
(PUT 'TAYSIMPPSI* 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPPSI* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPPSI* (TAY COUNT)
    (PROG (TAY1 TAY2 TP)
      (SETQ TP (CADDR TAY))
      (SETQ TAY1
              (ADDTAYLOR TAY
               ((LAMBDA (G213)
                  (LIST 'TAYLOR*
                        (LIST
                         (CONS
                          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EL TP)
                            (COND ((NULL EL) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL)
                                                (NLIST 0 (LENGTH (CAR EL))))
                                              (CAR EL))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EL (CDR EL))
                            (COND ((NULL EL) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                      (CAR EL))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          G213))
                        TP G213 NIL))
                (CONS COUNT 1))))
      (SETQ TAY1 (TAYSIMPPSI (LIST 'PSI TAY1)))
      (COND
       ((NOT (AND (KERNP TAY1) (EQCAR (CAAAR (CAR TAY1)) 'TAYLOR*)))
        (CONFUSION 'TAYSIMPPSI)))
      (SETQ TAY1 (CAAAR (CAR TAY1)))
      (SETQ TAY2 (NEGTAYLOR TAY))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE COUNT I)) (RETURN NIL)))
        (PROGN
         (SETQ TAY1 (ADDTAYLOR TAY1 (INVTAYLOR TAY2)))
         (SETQ TAY2
                 (ADDTAYLOR TAY2
                  ((LAMBDA (G215)
                     (LIST 'TAYLOR*
                           (LIST
                            (CONS
                             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                               (SETQ EL TP)
                               (COND ((NULL EL) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (EL)
                                                   (NLIST 0 (LENGTH (CAR EL))))
                                                 (CAR EL))
                                                NIL)))
                              LOOPLABEL
                               (SETQ EL (CDR EL))
                               (COND ((NULL EL) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             G215))
                           TP G215 NIL))
                   (CONS (MINUS 1) 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
        (RPLACA (CDDDR TAY1) (LIST 'PSI (CADDDR TAY)))))
      (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY1) 1) 1)) 1)))) 
(PUT 'PSI 'TAYLORSIMPFN 'TAYSIMPPSI) 
(PUT 'TAYSIMPPOLYGAMMA 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPPOLYGAMMA 'DEFINED-ON-LINE '193) 
(PUT 'TAYSIMPPOLYGAMMA 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPPOLYGAMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPPOLYGAMMA (U)
    (COND
     ((OR (NOT (EQCAR U 'POLYGAMMA)) (NOT (FIXP (CADR U))) (LESSP (CADR U) 1)
          (CDDDR U))
      (CONFUSION 'TAYSIMPPOLYGAMMA))
     (T
      (PROG (L L0 C0 TAY TP)
        (SETQ TAY (TAYSIMPSQ (SIMP* (CADDR U))))
        (COND
         ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
        (SETQ TAY (CAAAR (CAR TAY)))
        (SETQ TP (CADDR TAY))
        (SETQ L0
                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EL TP)
                  (COND ((NULL EL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                    (CAR EL))
                                   NIL)))
                 LOOPLABEL
                  (SETQ EL (CDR EL))
                  (COND ((NULL EL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (COND ((NULL L) (RETURN (TAYSIMPPOLYGAMMA* TAY (CADR U) 1))))
        (COND
         ((IS-NEG-PL (CAR (CAR L)))
          (TAYLOR-ERROR 'ESSENTIAL-SINGULARITY (CAR U))))
        (SETQ C0 (GET-CST-COEFF TAY))
        (COND
         ((AND (EQUAL (CDR C0) 1)
               (OR (NULL (CAR C0)) (AND (FIXP (CAR C0)) (LESSP (CAR C0) 0))))
          (RETURN
           (TAYSIMPPOLYGAMMA* TAY (CADR U)
            (COND ((NULL (CAR C0)) 1) (T (DIFFERENCE 1 (CAR C0)))))))
         (T (RETURN (TAYLOREXPAND-DIFF U TP T)))))))) 
(PUT 'TAYSIMPPOLYGAMMA* 'NUMBER-OF-ARGS 3) 
(PUT 'TAYSIMPPOLYGAMMA* 'DEFINED-ON-LINE '221) 
(PUT 'TAYSIMPPOLYGAMMA* 'DEFINED-IN-FILE 'TAYLOR/TAYGAMMA.RED) 
(PUT 'TAYSIMPPOLYGAMMA* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPPOLYGAMMA* (TAY N COUNT)
    (PROG (TAY1 TAY2 TP FCT)
      (SETQ TP (CADDR TAY))
      (SETQ TAY1
              (ADDTAYLOR TAY
               ((LAMBDA (G217)
                  (LIST 'TAYLOR*
                        (LIST
                         (CONS
                          (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                            (SETQ EL TP)
                            (COND ((NULL EL) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (EL)
                                                (NLIST 0 (LENGTH (CAR EL))))
                                              (CAR EL))
                                             NIL)))
                           LOOPLABEL
                            (SETQ EL (CDR EL))
                            (COND ((NULL EL) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                      (CAR EL))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          G217))
                        TP G217 NIL))
                (CONS COUNT 1))))
      (SETQ TAY1 (TAYSIMPPOLYGAMMA (LIST 'POLYGAMMA N TAY1)))
      (COND
       ((NOT (AND (KERNP TAY1) (EQCAR (CAAAR (CAR TAY1)) 'TAYLOR*)))
        (CONFUSION 'TAYSIMPPOLYGAMMA)))
      (SETQ TAY1 (CAAAR (CAR TAY1)))
      (SETQ TAY2 (NEGTAYLOR TAY))
      (SETQ FCT (SIMP* (NFACTORIAL N)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE COUNT I)) (RETURN NIL)))
        (PROGN
         (SETQ TAY1
                 (ADDTAYLOR TAY1
                  (MULTTAYLORSQ
                   (INVTAYLOR (EXPTTAYRAT TAY2 (CONS (PLUS N 1) 1))) FCT)))
         (SETQ TAY2
                 (ADDTAYLOR TAY2
                  ((LAMBDA (G219)
                     (LIST 'TAYLOR*
                           (LIST
                            (CONS
                             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                               (SETQ EL TP)
                               (COND ((NULL EL) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (EL)
                                                   (NLIST 0 (LENGTH (CAR EL))))
                                                 (CAR EL))
                                                NIL)))
                              LOOPLABEL
                               (SETQ EL (CDR EL))
                               (COND ((NULL EL) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             G219))
                           TP G219 NIL))
                   (CONS (MINUS 1) 1))))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
        (RPLACA (CDDDR TAY1) (LIST 'POLYGAMMA N (CADDDR TAY)))))
      (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY1) 1) 1)) 1)))) 
(PUT 'POLYGAMMA 'TAYLORSIMPFN 'TAYSIMPPOLYGAMMA) 
(ENDMODULE) 