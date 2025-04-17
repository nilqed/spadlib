(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'GROEBFAC)) 
(IMPORTS (LIST 'FACTOR)) 
(PUT 'GROEBFACTORIZE 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBFACTORIZE 'DEFINED-ON-LINE '30) 
(PUT 'GROEBFACTORIZE 'DEFINED-IN-FILE 'GROEBNER/GROEBFAC.RED) 
(PUT 'GROEBFACTORIZE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBFACTORIZE (H ABORT1 G G99)
    (PROG (R TIM GCTIM H1 GROEBACTUALG99* GROEBFABORT* TEST S)
      (SETQ S (AND *GSUGAR (GSUGAR H)))
      (SETQ GROEBACTUALG99* G99)
      (SETQ GROEBACTUALG* G)
      (SETQ GROEBFABORT* ABORT1)
      (COND ((VDPGETPROP H 'IRREDUCIBLE) (RETURN (GROEBFACTORIZE3 H))))
      (SETQ TIM (TIME))
      (SETQ GCTIM (GCTIME))
      (AND *TRGROEB (GROEBMESS7 H))
      (SETQ R
              (COND ((SETQ R (VDPGETPROP H 'FACTORS)) R)
                    (*GROEBRM (GROEBFACTORIZE1 H))
                    ((NOT *VDPMODULAR) (GROEBFACTORIZE2 H)) (T NIL)))
      (SETQ FACTORTIME*
              (PLUS FACTORTIME*
                    (DIFFERENCE (DIFFERENCE (TIME) TIM)
                                (DIFFERENCE (GCTIME) GCTIM))))
      (COND
       ((NULL R)
        (PROGN (VDPPUTPROP H 'IRREDUCIBLE T) (RETURN (GROEBFACTORIZE3 H)))))
      (COND ((CDR R) (AND *TRGROEB (GROEBMESS14 H R))))
      (VDPPUTPROP H 'FACTORS R)
      (PROG (P)
        (SETQ P R)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (COND ((VDPMEMBER (CAR P) G) (SETQ TEST (CAR P)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND (TEST (PROGN (AND *TRGROEB (GROEBMESS27A H TEST)) (RETURN 'ZERO))))
      (SETQ H1 (CAR R))
      (PROG (P)
        (SETQ P R)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((VDPMEMBER (CAR P) ABORT1)
             (PROGN
              (SETQ R (DELETE P R))
              (AND *TRGROEB (GROEBMESS27 (CAR P)))))
            (T (VDPPUTPROP (CAR P) 'IRREDUCIBLE T))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (COND ((NULL R) (SETQ R (LIST H1))))
      (COND ((NULL (CDR R)) (GROEBFACTORIZE3 (CAAR R))))
      (COND
       (*GSUGAR
        (COND ((NULL (CDR R)) (GSETSUGAR (CAAR R) S))
              (T
               (PROG (P)
                 (SETQ P R)
                LAB
                 (COND ((NULL P) (RETURN NIL)))
                 ((LAMBDA (P) (GSETSUGAR (CAR P) (VDPTDEG (CAR P)))) (CAR P))
                 (SETQ P (CDR P))
                 (GO LAB))))))
      (RETURN (CONS 'FACTOR R)))) 
(PUT 'GROEBFACTORIZE1 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBFACTORIZE1 'DEFINED-ON-LINE '66) 
(PUT 'GROEBFACTORIZE1 'DEFINED-IN-FILE 'GROEBNER/GROEBFAC.RED) 
(PUT 'GROEBFACTORIZE1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBFACTORIZE1 (H)
    (PROG (MONF VP N E H1 H2 VP2)
      (SETQ MONF (VDPGETPROP H 'MONFAC))
      (COND
       ((NULL MONF)
        (RETURN (COND ((NOT *VDPMODULAR) (GROEBFACTORIZE2 H)) (T NIL)))))
      (SETQ H2 (VDPDIVMON H (BCFD 1) MONF))
      (COND
       ((NEQ GROEBMONFAC 0)
        (PROGN
         (SETQ N 0)
         (PROG (X)
           (SETQ X MONF)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ N (IPLUS2 N 1))
               (COND
                ((NEQ X 0)
                 (PROGN
                  (SETQ E (LIST X))
                  (PROG (I)
                    (SETQ I 2)
                   LAB
                    (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                    (SETQ E (CONS 0 E))
                    (SETQ I (PLUS2 I 1))
                    (GO LAB))
                  (SETQ VP (CONS (VDPFMON (A2BC 1) E) VP)))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))))
       (T (AND *TRGROEB (GROEBMESS15 MONF))))
      (COND
       ((AND (NOT (OR (NULL H2) (NULL (CADR (CDDR H2)))))
             (NOT
              (OR (NULL (CADR H2))
                  (AND (EQUAL (CAR (CADR H2)) 0)
                       (VEVZERO?1 (CDR (CADR H2)))))))
        (PROGN
         (COND ((NOT *VDPMODULAR) (SETQ VP2 (GROEBFACTORIZE2 H2))))
         (SETQ VP2
                 (COND ((NOT VP2) (LIST H2))
                       (T
                        (PROG (V FORALL-RESULT FORALL-ENDPTR)
                          (SETQ V VP2)
                          (COND ((NULL V) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS ((LAMBDA (V) (CAR V)) (CAR V))
                                                NIL)))
                         LOOPLABEL
                          (SETQ V (CDR V))
                          (COND ((NULL V) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (V) (CAR V)) (CAR V)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))))
         (SETQ VP (NCONC VP VP2)))))
      (SETQ H1 VP)
      (RETURN
       (REVERSE
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X H1)
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (LIST (VDPENUMERATE X))) (CAR X))
                                NIL)))
         LOOPLABEL
          (SETQ X (CDR X))
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (X) (LIST (VDPENUMERATE X))) (CAR X)) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'GROEBFACTORIZE2 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBFACTORIZE2 'DEFINED-ON-LINE '98) 
(PUT 'GROEBFACTORIZE2 'DEFINED-IN-FILE 'GROEBNER/GROEBFAC.RED) 
(PUT 'GROEBFACTORIZE2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBFACTORIZE2 (H)
    (PROG (H1 H2 *FACTOR)
      (SETQ *FACTOR T)
      (SETQ H1 (GROEFCTRF (DIP2F (CADR (CDDR H)))))
      (COND ((NULL (CDR H1)) (RETURN NIL)))
      (COND ((AND (NULL (CDDR H1)) (EQUAL (CDR (CADR H1)) 1)) (RETURN NIL)))
      (SETQ H2
              (PROG (L FORALL-RESULT FORALL-ENDPTR)
                (SETQ L (CDR H1))
               STARTOVER
                (COND ((NULL L) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (L)
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                             (COND
                              ((MINUSP (DIFFERENCE (CDR L) I)) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR (CONS (CAR L) NIL)))
                            LOOPLABEL
                             (SETQ I (PLUS2 I 1))
                             (COND
                              ((MINUSP (DIFFERENCE (CDR L) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR (CONS (CAR L) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR L)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ L (CDR L))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL L) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (L)
                           (PROG (I FORALL-RESULT FORALL-ENDPTR)
                             (SETQ I 1)
                             (COND
                              ((MINUSP (DIFFERENCE (CDR L) I)) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR (CONS (CAR L) NIL)))
                            LOOPLABEL
                             (SETQ I (PLUS2 I 1))
                             (COND
                              ((MINUSP (DIFFERENCE (CDR L) I))
                               (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR (CONS (CAR L) NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR L)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ L (CDR L))
                (GO LOOPLABEL)))
      (SETQ H2
              (VDPLSORT
               (PROG (P FORALL-RESULT FORALL-ENDPTR)
                 (SETQ P H2)
                 (COND ((NULL P) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (P) (VDPSIMPCONT (F2VDP P)))
                                   (CAR P))
                                  NIL)))
                LOOPLABEL
                 (SETQ P (CDR P))
                 (COND ((NULL P) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (P) (VDPSIMPCONT (F2VDP P))) (CAR P))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X H2)
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (LIST (VDPENUMERATE X))) (CAR X))
                               NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (LIST (VDPENUMERATE X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'GROEFCTRF 'NUMBER-OF-ARGS 1) 
(PUT 'GROEFCTRF 'DEFINED-ON-LINE '111) 
(PUT 'GROEFCTRF 'DEFINED-IN-FILE 'GROEBNER/GROEBFAC.RED) 
(PUT 'GROEFCTRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEFCTRF (P)
    ((LAMBDA (*FACTOR CURRENT-MODULUS) (FCTRF P)) T CURRENT-MODULUS)) 
(PUT 'GROEBFACTORIZE3 'NUMBER-OF-ARGS 1) 
(PUT 'GROEBFACTORIZE3 'DEFINED-ON-LINE '114) 
(PUT 'GROEBFACTORIZE3 'DEFINED-IN-FILE 'GROEBNER/GROEBFAC.RED) 
(PUT 'GROEBFACTORIZE3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GROEBFACTORIZE3 (H) (PROGN (SETQ H NIL) NIL)) 
(ENDMODULE) 