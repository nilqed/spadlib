(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HEUGCD)) 
(FLUID '(*HEUGCD REDUCTION-COUNT)) 
(GLOBAL '(!EE)) 
(PUT 'HTC 'NUMBER-OF-ARGS 1) 
(PUT 'HTC 'DEFINED-ON-LINE '73) 
(PUT 'HTC 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HTC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HTC (P) (COND ((ATOM P) P) ((NULL (CDR P)) (CDAR P)) (T (HTC (CDR P))))) 
(PUT 'KONTENT 'NUMBER-OF-ARGS 1) 
(PUT 'KONTENT 'DEFINED-ON-LINE '78) 
(PUT 'KONTENT 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'KONTENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KONTENT (P)
    (COND
     ((OR (ATOM P) (ATOM (CAR P)))
      (COND ((NUMBERP P) P) ((NULL P) 1)
            (T (REDERR "HEUGCD(kontent): unsupported domain element"))))
     ((OR (ATOM (CDR P)) (ATOM (CAR (CDR P))))
      (COND ((NUMBERP (CDR P)) (GCDN (CDAR P) (CDR P)))
            ((NULL (CDR P)) (CDAR P))
            (T (REDERR "HEUGCD(kontent): unsupported domain element"))))
     (T (KONTENT1 (CDR (CDR P)) (GCDN (CDAR P) (CDAR (CDR P))))))) 
(PUT 'KONTENT1 'NUMBER-OF-ARGS 2) 
(PUT 'KONTENT1 'DEFINED-ON-LINE '90) 
(PUT 'KONTENT1 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'KONTENT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KONTENT1 (P A)
    (COND ((EQUAL A 1) 1)
          ((OR (ATOM P) (ATOM (CAR P)))
           (COND ((NUMBERP P) (GCDN P A)) ((NULL P) A)
                 (T (REDERR "HEUGCD(kontent1): unsupported domain element"))))
          (T (KONTENT1 (CDR P) (GCDN (REMAINDER (CDAR P) A) A))))) 
(PUT 'HORNER-EVAL-RAT 'NUMBER-OF-ARGS 2) 
(PUT 'HORNER-EVAL-RAT 'DEFINED-ON-LINE '98) 
(PUT 'HORNER-EVAL-RAT 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-RAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-RAT (P V)
    (COND ((EQUAL (CAR V) 1) (HORNER-EVAL-INTEGER P (CDR V) 1 0))
          ((EQUAL (CDR V) 1) (HORNER-EVAL-RECIPROCAL P (CAR V) 0 0))
          (T (HORNER-EVAL-RATIONAL P (CAR V) (CDR V) 0 1 0)))) 
(PUT 'HORNER-EVAL-RATIONAL 'NUMBER-OF-ARGS 6) 
(PUT 'HORNER-EVAL-RATIONAL 'DEFINED-ON-LINE '115) 
(PUT 'HORNER-EVAL-RATIONAL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-RATIONAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-RATIONAL (P N D M S ANS)
    (COND
     ((OR (ATOM P) (ATOM (CAR P)))
      (COND (P (PLUS (TIMES ANS (EXPT N M)) (TIMES S P))) (T ANS)))
     (T
      (DIFFERENCE
       ((LAMBDA (MP)
          (HORNER-EVAL-RATIONAL (CDR P) N D MP (TIMES S (EXPT D MP))
                                (PLUS (TIMES ANS (EXPT N M))
                                      (TIMES S (CDAR P)))))
        (CDAAR P))
       (COND ((OR (ATOM (CDR P)) (ATOM (CAR (CDR P)))) 0)
             (T (CDAAR (CDR P)))))))) 
(PUT 'HORNER-EVAL-INTEGER 'NUMBER-OF-ARGS 4) 
(PUT 'HORNER-EVAL-INTEGER 'DEFINED-ON-LINE '123) 
(PUT 'HORNER-EVAL-INTEGER 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-INTEGER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-INTEGER (P D S ANS)
    (COND
     ((OR (ATOM P) (ATOM (CAR P))) (COND (P (PLUS ANS (TIMES S P))) (T ANS)))
     (T
      (HORNER-EVAL-INTEGER (CDR P) D
                           (TIMES S
                                  (EXPT D
                                        (DIFFERENCE (CDAAR P)
                                                    (COND
                                                     ((OR (ATOM (CDR P))
                                                          (ATOM (CAR (CDR P))))
                                                      0)
                                                     (T (CDAAR (CDR P)))))))
                           (PLUS ANS (TIMES S (CDAR P))))))) 
(PUT 'HORNER-EVAL-RECIPROCAL 'NUMBER-OF-ARGS 4) 
(PUT 'HORNER-EVAL-RECIPROCAL 'DEFINED-ON-LINE '131) 
(PUT 'HORNER-EVAL-RECIPROCAL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-RECIPROCAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-RECIPROCAL (P N M ANS)
    (COND
     ((OR (ATOM P) (ATOM (CAR P)))
      (COND (P (PLUS (TIMES ANS (EXPT N M)) P)) (T ANS)))
     (T
      (HORNER-EVAL-RECIPROCAL (CDR P) N
                              (DIFFERENCE (CDAAR P)
                                          (COND
                                           ((OR (ATOM (CDR P))
                                                (ATOM (CAR (CDR P))))
                                            0)
                                           (T (CDAAR (CDR P)))))
                              (PLUS (TIMES ANS (EXPT N M)) (CDAR P)))))) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL 'NUMBER-OF-ARGS 2) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL 'DEFINED-ON-LINE '139) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-RAT-AND-GCDL (L V)
    (COND ((NULL (CDR L)) (HORNER-EVAL-RAT (CAR L) V))
          ((NULL (CDDR L))
           (GCDN (HORNER-EVAL-RAT (CAR L) V) (HORNER-EVAL-RAT (CADR L) V)))
          (T
           (HORNER-EVAL-RAT-AND-GCDL1 (CDDR L) V
                                      (GCDN (HORNER-EVAL-RAT (CAR L) V)
                                            (HORNER-EVAL-RAT (CADR L) V)))))) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL1 'NUMBER-OF-ARGS 3) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL1 'DEFINED-ON-LINE '150) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL1 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HORNER-EVAL-RAT-AND-GCDL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HORNER-EVAL-RAT-AND-GCDL1 (L V A)
    (COND ((EQUAL A 1) 1) ((NULL L) A)
          (T
           (HORNER-EVAL-RAT-AND-GCDL1 (CDR L) V
                                      (GCDN (HORNER-EVAL-RAT (CAR L) V) A))))) 
(PUT 'HEU-QUOTFL 'NUMBER-OF-ARGS 2) 
(PUT 'HEU-QUOTFL 'DEFINED-ON-LINE '158) 
(PUT 'HEU-QUOTFL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-QUOTFL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HEU-QUOTFL (L D)
    (COND ((NULL (CDR L)) (HEU-QUOTF (CAR L) D))
          (T (HEU-QUOTFL1 (CDR L) D (HEU-QUOTF (CAR L) D))))) 
(PUT 'HEU-QUOTFL1 'NUMBER-OF-ARGS 3) 
(PUT 'HEU-QUOTFL1 'DEFINED-ON-LINE '163) 
(PUT 'HEU-QUOTFL1 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-QUOTFL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HEU-QUOTFL1 (L D FLAG)
    (COND ((NULL FLAG) NIL) ((NULL (CDR L)) (HEU-QUOTF (CAR L) D))
          (T (HEU-QUOTFL1 (CDR L) D (HEU-QUOTF (CAR L) D))))) 
(PUT 'HEU-QUOTF 'NUMBER-OF-ARGS 2) 
(PUT 'HEU-QUOTF 'DEFINED-ON-LINE '168) 
(PUT 'HEU-QUOTF 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-QUOTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HEU-QUOTF (P Q)
    (COND
     ((OR (ATOM Q) (ATOM (CAR Q)))
      (COND
       ((OR (ATOM P) (ATOM (CAR P)))
        (COND ((NULL P) NIL)
              ((NULL Q) (REDERR "HEUGCD(heu-quotf): division by zero"))
              (T
               ((LAMBDA (TEMP)
                  (COND ((EQUAL (CDR TEMP) 0) (CAR TEMP)) (T NIL)))
                (DIVIDE P Q)))))
       (T ((LAMBDA (*EXP) (QUOTF1 P Q)) T))))
     ((OR (ATOM P) (ATOM (CAR P))) NIL) ((LESSP (CDAAR P) (CDAAR Q)) NIL)
     ((NEQ (CDR (DIVIDE (CDAR P) (CDAR Q))) 0) NIL) ((EQUAL P Q) 1)
     (T
      ((LAMBDA (QV)
         (COND ((EQUAL QV 0) ((LAMBDA (*EXP) (QUOTF1 P Q)) T))
               ((EQUAL (REMAINDER (HORNER-EVAL-RAT P '(2 . 1)) QV) 0)
                ((LAMBDA (*EXP) (QUOTF1 P Q)) T))
               (T NIL)))
       (HORNER-EVAL-RAT Q '(2 . 1)))))) 
(DE XCEILING (N D) (QUOTIENT (PLUS N (DIFFERENCE D 1)) D)) 
(PUT 'XCEILING 'NUMBER-OF-ARGS 2) 
(PUT 'XCEILING 'DEFINED-ON-LINE '190) 
(PUT 'XCEILING 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'XCEILING 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'XCEILING 'INLINE '(LAMBDA (N D) (QUOTIENT (PLUS N (DIFFERENCE D 1)) D))) 
(DE FORCE-EVEN (X) (COND ((EVENP X) X) (T (PLUS X 1)))) 
(PUT 'FORCE-EVEN 'NUMBER-OF-ARGS 1) 
(PUT 'FORCE-EVEN 'DEFINED-ON-LINE '192) 
(PUT 'FORCE-EVEN 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'FORCE-EVEN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FORCE-EVEN 'INLINE '(LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))) 
(DE FORCE-ODD (X) (COND ((EVENP X) (PLUS X 1)) (T X))) 
(PUT 'FORCE-ODD 'NUMBER-OF-ARGS 1) 
(PUT 'FORCE-ODD 'DEFINED-ON-LINE '195) 
(PUT 'FORCE-ODD 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'FORCE-ODD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'FORCE-ODD 'INLINE '(LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))) 
(DE NEXT-EVEN-VALUE (X)
    (COND
     ((EQUAL (CDR X) 1)
      (CONS
       ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
        (FIX (TIMES (CAR X) !EE)))
       1))
     (T
      (CONS 1
            ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
             (FIX (TIMES (CDR X) !EE))))))) 
(PUT 'NEXT-EVEN-VALUE 'NUMBER-OF-ARGS 1) 
(PUT 'NEXT-EVEN-VALUE 'DEFINED-ON-LINE '198) 
(PUT 'NEXT-EVEN-VALUE 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'NEXT-EVEN-VALUE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'NEXT-EVEN-VALUE 'INLINE
      '(LAMBDA (X)
         (COND
          ((EQUAL (CDR X) 1)
           (CONS
            ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
             (FIX (TIMES (CAR X) !EE)))
            1))
          (T
           (CONS 1
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (FIX (TIMES (CDR X) !EE)))))))) 
(DE NEXT-ODD-VALUE (X)
    (COND
     ((EQUAL (CDR X) 1)
      (CONS
       ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
        (FIX (TIMES (CAR X) !EE)))
       1))
     (T
      (CONS 1
            ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
             (FIX (TIMES (CDR X) !EE))))))) 
(PUT 'NEXT-ODD-VALUE 'NUMBER-OF-ARGS 1) 
(PUT 'NEXT-ODD-VALUE 'DEFINED-ON-LINE '202) 
(PUT 'NEXT-ODD-VALUE 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'NEXT-ODD-VALUE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'NEXT-ODD-VALUE 'INLINE
      '(LAMBDA (X)
         (COND
          ((EQUAL (CDR X) 1)
           (CONS
            ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
             (FIX (TIMES (CAR X) !EE)))
            1))
          (T
           (CONS 1
                 ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
                  (FIX (TIMES (CDR X) !EE)))))))) 
(DE FIRST-VALUE (INP INQ LCP LCQ TCP TCQ)
    (COND
     ((LESSP LCP TCP)
      (COND
       ((LESSP LCQ TCQ)
        (COND
         ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
          (CONS 1
                (PLUS 2
                      (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
         (T
          (CONS 1
                (PLUS 2
                      (TIMES 2
                             (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1)) TCQ)))))))
       ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
        (CONS 1
              (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
       (T
        (CONS (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
              1))))
     ((LESSP LCQ TCQ)
      (COND
       ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
        (CONS (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
              1))
       (T
        (CONS 1
              (PLUS 2
                    (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1)) TCQ)))))))
     ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
      (CONS (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP))) 1))
     (T
      (CONS (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
            1)))) 
(PUT 'FIRST-VALUE 'NUMBER-OF-ARGS 6) 
(PUT 'FIRST-VALUE 'DEFINED-ON-LINE '206) 
(PUT 'FIRST-VALUE 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'FIRST-VALUE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(PUTC 'FIRST-VALUE 'INLINE
      '(LAMBDA (INP INQ LCP LCQ TCP TCQ)
         (COND
          ((LESSP LCP TCP)
           (COND
            ((LESSP LCQ TCQ)
             (COND
              ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
               (CONS 1
                     (PLUS 2
                           (TIMES 2
                                  (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                            TCP)))))
              (T
               (CONS 1
                     (PLUS 2
                           (TIMES 2
                                  (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                            TCQ)))))))
            ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
             (CONS 1
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                          TCP)))))
            (T
             (CONS
              (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
              1))))
          ((LESSP LCQ TCQ)
           (COND
            ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
             (CONS
              (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
              1))
            (T
             (CONS 1
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                          TCQ)))))))
          ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
           (CONS
            (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP))) 1))
          (T
           (CONS
            (PLUS 2 (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
            1))))) 
(DE SECOND-VALUE (INP INQ LCP LCQ LGCD TCP TCQ TGCD)
    ((LAMBDA (INP INQ LCP LCQ TCP TCQ)
       (COND
        ((LESSP LCP TCP)
         (COND
          ((LESSP LCQ TCQ)
           (COND
            ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
             (CONS 1
                   ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                    (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
            (T
             (CONS 1
                   ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                    (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1)) TCQ)))))))
          ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
           (CONS 1
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
          (T
           (CONS
            ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
             (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
            1))))
        ((LESSP LCQ TCQ)
         (COND
          ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
           (CONS
            ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
             (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
            1))
          (T
           (CONS 1
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1)) TCQ)))))))
        ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
         (CONS
          ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
           (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
          1))
        (T
         (CONS
          ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
           (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
          1))))
     INP INQ (MAX 2 (QUOTIENT LCP LGCD)) (MAX 2 (QUOTIENT LCQ LGCD))
     (MAX 2 (QUOTIENT TCP TGCD)) (MAX 2 (QUOTIENT TCQ TGCD)))) 
(PUT 'SECOND-VALUE 'NUMBER-OF-ARGS 8) 
(PUT 'SECOND-VALUE 'DEFINED-ON-LINE '221) 
(PUT 'SECOND-VALUE 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'SECOND-VALUE 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL)
       GENERAL)) 
(PUTC 'SECOND-VALUE 'INLINE
      '(LAMBDA (INP INQ LCP LCQ LGCD TCP TCQ TGCD)
         ((LAMBDA (INP INQ LCP LCQ TCP TCQ)
            (COND
             ((LESSP LCP TCP)
              (COND
               ((LESSP LCQ TCQ)
                (COND
                 ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
                  (CONS 1
                        ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                         (PLUS 2
                               (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
                 (T
                  (CONS 1
                        ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                         (PLUS 2
                               (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                         TCQ)))))))
               ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE TCP 1)) TCP)))))
               (T
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                 1))))
             ((LESSP LCQ TCQ)
              (COND
               ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                 1))
               (T
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2
                             (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1)) TCQ)))))))
             ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
              (CONS
               ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
               1))
             (T
              (CONS
               ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
               1))))
          INP INQ (MAX 2 (QUOTIENT LCP LGCD)) (MAX 2 (QUOTIENT LCQ LGCD))
          (MAX 2 (QUOTIENT TCP TGCD)) (MAX 2 (QUOTIENT TCQ TGCD))))) 
(PUT 'HEU-GCD-LIST 'NUMBER-OF-ARGS 1) 
(PUT 'HEU-GCD-LIST 'DEFINED-ON-LINE '243) 
(PUT 'HEU-GCD-LIST 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-GCD-LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HEU-GCD-LIST (L)
    (COND ((NULL (CDR L)) (CAR L)) ((NULL (CDDR L)) (HEU-GCD (CAR L) (CADR L)))
          (T
           (HEU-GCDL
            (SORT L
                  (FUNCTION (LAMBDA (P1 P2) (LESSP (CDAAR P1) (CDAAR P2))))))))) 
(PUT 'HEU-GCDL 'NUMBER-OF-ARGS 1) 
(PUT 'HEU-GCDL 'DEFINED-ON-LINE '248) 
(PUT 'HEU-GCDL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-GCDL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HEU-GCDL (L)
    (PROG (K VALUE DVAL D XSX INP INQ LCP LCQ LGCD TCP TCQ TGCD TMP)
      (COND
       ((EQUAL (CDAAR (CAR L)) 1)
        (RETURN
         ((LAMBDA (PCARL) (COND ((HEU-QUOTFL (CDR L) PCARL) PCARL) (T 1)))
          ((LAMBDA (*EXP) (QUOTF1 (CAR L) (KONTENT (CAR L)))) T)))))
      (SETQ TMP (ANALYSE-POLYNOMIAL (CAR L)))
      (COND
       (TMP
        (PROGN
         (SETQ INP (CAR TMP))
         (SETQ LCP (CDAR (CAR L)))
         (SETQ XSX (CADR TMP))
         (SETQ TCP (CADDR TMP))
         (SETQ TMP (ANALYSE-POLYNOMIAL (CADR L)))
         (COND
          (TMP
           (PROGN
            (SETQ INQ (CAR TMP))
            (SETQ LCQ (CDAR (CADR L)))
            (SETQ XSX (MIN XSX (CADR TMP)))
            (SETQ TCQ (CADDR TMP))))
          (T (RETURN NIL)))))
       (T (RETURN NIL)))
      (SETQ VALUE
              (COND
               ((LESSP LCP TCP)
                (COND
                 ((LESSP LCQ TCQ)
                  (COND
                   ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
                    (CONS 1
                          (PLUS 2
                                (TIMES 2
                                       (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                                 TCP)))))
                   (T
                    (CONS 1
                          (PLUS 2
                                (TIMES 2
                                       (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                                 TCQ)))))))
                 ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
                  (CONS 1
                        (PLUS 2
                              (TIMES 2
                                     (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                               TCP)))))
                 (T
                  (CONS
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                   1))))
               ((LESSP LCQ TCQ)
                (COND
                 ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
                  (CONS
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                   1))
                 (T
                  (CONS 1
                        (PLUS 2
                              (TIMES 2
                                     (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                               TCQ)))))))
               ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
                (CONS
                 (PLUS 2
                       (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                 1))
               (T
                (CONS
                 (PLUS 2
                       (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                 1))))
      (SETQ D
              (GEN-POLY (HORNER-EVAL-RAT-AND-GCDL L VALUE) VALUE
                        (CAAAR (CAR L)) XSX))
      (COND ((HEU-QUOTFL L D) (RETURN D)))
      (SETQ LGCD (GCDN LCP LCQ))
      (PROG (X)
        (SETQ X (CDDR L))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ LGCD (GCDN (CDAR X) LGCD))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ TGCD (GCDN TCP TCQ))
      (PROG (X)
        (SETQ X (CDDR L))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ TGCD (GCDN (HTC X) TGCD))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VALUE
              ((LAMBDA (INP INQ LCP LCQ TCP TCQ)
                 (COND
                  ((LESSP LCP TCP)
                   (COND
                    ((LESSP LCQ TCQ)
                     (COND
                      ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
                       (CONS 1
                             ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                              (PLUS 2
                                    (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                              TCP)))))
                      (T
                       (CONS 1
                             ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                              (PLUS 2
                                    (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                              TCQ)))))))
                    ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
                     (CONS 1
                           ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                            (PLUS 2
                                  (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                            TCP)))))
                    (T
                     (CONS
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                      1))))
                  ((LESSP LCQ TCQ)
                   (COND
                    ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
                     (CONS
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                      1))
                    (T
                     (CONS 1
                           ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                            (PLUS 2
                                  (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                            TCQ)))))))
                  ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
                   (CONS
                    ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                     (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                    1))
                  (T
                   (CONS
                    ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                     (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                    1))))
               INP INQ (MAX 2 (QUOTIENT LCP LGCD)) (MAX 2 (QUOTIENT LCQ LGCD))
               (MAX 2 (QUOTIENT TCP TGCD)) (MAX 2 (QUOTIENT TCQ TGCD))))
     LOOP
      (SETQ D
              (GEN-POLY (HORNER-EVAL-RAT-AND-GCDL L VALUE) VALUE
                        (CAAAR (CAR L)) XSX))
      (COND ((HEU-QUOTFL L D) (RETURN D)))
      (SETQ VALUE
              (COND
               ((EQUAL (CDR VALUE) 1)
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
                  (FIX (TIMES (CAR VALUE) !EE)))
                 1))
               (T
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
                       (FIX (TIMES (CDR VALUE) !EE)))))))
      (SETQ K (PLUS K 1))
      (SETQ D
              (GEN-POLY (HORNER-EVAL-RAT-AND-GCDL L VALUE) VALUE
                        (CAAAR (CAR L)) XSX))
      (COND ((HEU-QUOTFL L D) (RETURN D)))
      (SETQ VALUE
              (COND
               ((EQUAL (CDR VALUE) 1)
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (FIX (TIMES (CAR VALUE) !EE)))
                 1))
               (T
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (FIX (TIMES (CDR VALUE) !EE)))))))
      (SETQ K (PLUS K 1))
      (COND ((LESSP K 10) (GO LOOP)))
      (PRINT "(HEUGCD):heu-gcd-list fails")
      (RETURN NIL))) 
(PUT 'HEU-GCD 'NUMBER-OF-ARGS 2) 
(PUT 'HEU-GCD 'DEFINED-ON-LINE '295) 
(PUT 'HEU-GCD 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'HEU-GCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE HEU-GCD (P Q)
    (PROG (K VALUE D DVAL XSX INP INQ LCP LCQ LGCD TCP TCQ TGCD TMP)
      (COND
       ((OR (EQUAL (CDAAR Q) 1) (EQUAL (CDAAR P) 1))
        (RETURN
         (COND
          ((AND (UNIVARIATEP P) (UNIVARIATEP Q))
           ((LAMBDA (PP PQ)
              (COND
               ((EQUAL (CDAAR PQ) 1)
                ((LAMBDA (H) (COND ((NULL H) 1) (T PQ))) (HEU-QUOTF PP PQ)))
               (T
                ((LAMBDA (H) (COND ((NULL H) 1) (T PP))) (HEU-QUOTF PQ PP)))))
            ((LAMBDA (*EXP) (QUOTF1 P (KONTENT P))) T)
            ((LAMBDA (*EXP) (QUOTF1 Q (KONTENT Q))) T)))
          (T NIL)))))
      (COND ((GREATERP (CDAAR P) (CDAAR Q)) (RETURN (HEU-GCD Q P))))
      (SETQ TMP (ANALYSE-POLYNOMIAL P))
      (COND
       (TMP
        (PROGN
         (SETQ INP (CAR TMP))
         (SETQ LCP (CDAR P))
         (SETQ XSX (CADR TMP))
         (SETQ TCP (CADDR TMP))
         (SETQ TMP (ANALYSE-POLYNOMIAL Q))
         (COND
          (TMP
           (PROGN
            (SETQ INQ (CAR TMP))
            (SETQ LCQ (CDAR Q))
            (SETQ XSX (MIN XSX (CADR TMP)))
            (SETQ TCQ (CADDR TMP))))
          (T (RETURN NIL)))))
       (T (RETURN NIL)))
      (SETQ VALUE
              (COND
               ((LESSP LCP TCP)
                (COND
                 ((LESSP LCQ TCQ)
                  (COND
                   ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
                    (CONS 1
                          (PLUS 2
                                (TIMES 2
                                       (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                                 TCP)))))
                   (T
                    (CONS 1
                          (PLUS 2
                                (TIMES 2
                                       (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                                 TCQ)))))))
                 ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
                  (CONS 1
                        (PLUS 2
                              (TIMES 2
                                     (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                               TCP)))))
                 (T
                  (CONS
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                   1))))
               ((LESSP LCQ TCQ)
                (COND
                 ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
                  (CONS
                   (PLUS 2
                         (TIMES 2
                                (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                   1))
                 (T
                  (CONS 1
                        (PLUS 2
                              (TIMES 2
                                     (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                               TCQ)))))))
               ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
                (CONS
                 (PLUS 2
                       (TIMES 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                 1))
               (T
                (CONS
                 (PLUS 2
                       (TIMES 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                 1))))
      (SETQ DVAL (GCDN (HORNER-EVAL-RAT P VALUE) (HORNER-EVAL-RAT Q VALUE)))
      (SETQ D (GEN-POLY DVAL VALUE (CAAAR P) XSX))
      (COND ((AND (HEU-QUOTF P D) (HEU-QUOTF Q D)) (RETURN D)))
      (SETQ LGCD (GCDN LCP LCQ))
      (SETQ TGCD (GCDN LCP LCQ))
      (SETQ VALUE
              ((LAMBDA (INP INQ LCP LCQ TCP TCQ)
                 (COND
                  ((LESSP LCP TCP)
                   (COND
                    ((LESSP LCQ TCQ)
                     (COND
                      ((LESSP (TIMES INP TCQ) (TIMES INQ TCP))
                       (CONS 1
                             ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                              (PLUS 2
                                    (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                              TCP)))))
                      (T
                       (CONS 1
                             ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                              (PLUS 2
                                    (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                              TCQ)))))))
                    ((LESSP (TIMES INP LCQ) (TIMES INQ TCP))
                     (CONS 1
                           ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                            (PLUS 2
                                  (QUOTIENT (PLUS INP (DIFFERENCE TCP 1))
                                            TCP)))))
                    (T
                     (CONS
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                      1))))
                  ((LESSP LCQ TCQ)
                   (COND
                    ((LESSP (TIMES INP TCQ) (TIMES INQ LCP))
                     (CONS
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                      1))
                    (T
                     (CONS 1
                           ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                            (PLUS 2
                                  (QUOTIENT (PLUS INQ (DIFFERENCE TCQ 1))
                                            TCQ)))))))
                  ((LESSP (TIMES INP LCQ) (TIMES INQ LCP))
                   (CONS
                    ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                     (PLUS 2 (QUOTIENT (PLUS INP (DIFFERENCE LCP 1)) LCP)))
                    1))
                  (T
                   (CONS
                    ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                     (PLUS 2 (QUOTIENT (PLUS INQ (DIFFERENCE LCQ 1)) LCQ)))
                    1))))
               INP INQ (MAX 2 (QUOTIENT LCP LGCD)) (MAX 2 (QUOTIENT LCQ LGCD))
               (MAX 2 (QUOTIENT TCP TGCD)) (MAX 2 (QUOTIENT TCQ TGCD))))
      (SETQ K 0)
     LOOP
      (SETQ DVAL (GCDN (HORNER-EVAL-RAT P VALUE) (HORNER-EVAL-RAT Q VALUE)))
      (SETQ D (GEN-POLY DVAL VALUE (CAAAR P) XSX))
      (COND ((AND (HEU-QUOTF P D) (HEU-QUOTF Q D)) (RETURN D)))
      (SETQ VALUE
              (COND
               ((EQUAL (CDR VALUE) 1)
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
                  (FIX (TIMES (CAR VALUE) !EE)))
                 1))
               (T
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) (PLUS X 1)) (T X)))
                       (FIX (TIMES (CDR VALUE) !EE)))))))
      (SETQ K (PLUS K 1))
      (SETQ DVAL (GCDN (HORNER-EVAL-RAT P VALUE) (HORNER-EVAL-RAT Q VALUE)))
      (SETQ D (GEN-POLY DVAL VALUE (CAAAR P) XSX))
      (COND ((AND (HEU-QUOTF P D) (HEU-QUOTF Q D)) (RETURN D)))
      (SETQ VALUE
              (COND
               ((EQUAL (CDR VALUE) 1)
                (CONS
                 ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                  (FIX (TIMES (CAR VALUE) !EE)))
                 1))
               (T
                (CONS 1
                      ((LAMBDA (X) (COND ((EVENP X) X) (T (PLUS X 1))))
                       (FIX (TIMES (CDR VALUE) !EE)))))))
      (SETQ K (PLUS K 1))
      (COND ((LESSP K 10) (GO LOOP)))
      (COND (*EZGCD (REDERR "heu-gcd failed -- EZGCD ON"))
            (T
             (PROGN (LPRIE "heu-gcd failed -- EZGCD OFF") (RETURN NIL) NIL))))) 
(PUT 'ANALYSE-POLYNOMIAL 'NUMBER-OF-ARGS 1) 
(PUT 'ANALYSE-POLYNOMIAL 'DEFINED-ON-LINE '356) 
(PUT 'ANALYSE-POLYNOMIAL 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'ANALYSE-POLYNOMIAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ANALYSE-POLYNOMIAL (P) (ANALYSE-POLYNOMIAL1 P 1 (CDAR P) 0 (CAAAR P))) 
(PUT 'ANALYSE-POLYNOMIAL1 'NUMBER-OF-ARGS 5) 
(PUT 'ANALYSE-POLYNOMIAL1 'DEFINED-ON-LINE '363) 
(PUT 'ANALYSE-POLYNOMIAL1 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'ANALYSE-POLYNOMIAL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ANALYSE-POLYNOMIAL1 (P INP TCP XSXP MVARP)
    (COND
     ((OR (ATOM P) (ATOM (CAR P)))
      (COND (P (LIST (MAX INP (ABS P)) 0 (ABS P)))
            (T (LIST INP XSXP (ABS TCP)))))
     ((NEQ (CAAAR P) MVARP) NIL)
     ((OR (ATOM (CDAR P)) (ATOM (CAR (CDAR P))))
      (ANALYSE-POLYNOMIAL1 (CDR P) (MAX INP (ABS (CDAR P))) (CDAR P) (CDAAR P)
                           MVARP))
     (T NIL))) 
(DE NEGSHIFTZ (N MODULUS)
    ((LAMBDA (NN MMODULUS)
       (COND ((GREATERP NN (QUOTIENT MMODULUS 2)) (DIFFERENCE NN MMODULUS))
             (T NN)))
     N MODULUS)) 
(PUT 'NEGSHIFTZ 'NUMBER-OF-ARGS 2) 
(PUT 'NEGSHIFTZ 'DEFINED-ON-LINE '377) 
(PUT 'NEGSHIFTZ 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'NEGSHIFTZ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'NEGSHIFTZ 'INLINE
      '(LAMBDA (N MODULUS)
         ((LAMBDA (NN MMODULUS)
            (COND
             ((GREATERP NN (QUOTIENT MMODULUS 2)) (DIFFERENCE NN MMODULUS))
             (T NN)))
          N MODULUS))) 
(PUT 'GEN-POLY 'NUMBER-OF-ARGS 4) 
(PUT 'GEN-POLY 'DEFINED-ON-LINE '382) 
(PUT 'GEN-POLY 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'GEN-POLY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEN-POLY (DVAL VALUE VAR XSX)
    (COND ((EQUAL (CAR VALUE) 1) (GEN-POLY-BACKWARD DVAL (CDR VALUE) VAR XSX))
          ((EQUAL (CDR VALUE) 1) (GEN-POLY-FORWARD DVAL (CAR VALUE) VAR XSX))
          (T (REDERR "HEUGCD(gen-poly):point must be integral or reciprocal")))) 
(PUT 'GEN-POLY-FORWARD 'NUMBER-OF-ARGS 4) 
(PUT 'GEN-POLY-FORWARD 'DEFINED-ON-LINE '387) 
(PUT 'GEN-POLY-FORWARD 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'GEN-POLY-FORWARD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEN-POLY-FORWARD (DVAL VALUE VAR XSX)
    (PROG (I D VAL VAL1 KONT)
      (SETQ KONT 0)
      (SETQ VAL DVAL)
      (SETQ I XSX)
      (COND
       ((ZEROP I)
        (PROGN
         (SETQ VAL1
                 ((LAMBDA (NN MMODULUS)
                    (COND
                     ((GREATERP NN (QUOTIENT MMODULUS 2))
                      (DIFFERENCE NN MMODULUS))
                     (T NN)))
                  (REMAINDER VAL VALUE) VALUE))
         (COND ((NOT (ZEROP VAL1)) (SETQ KONT (SETQ D VAL1))))
         (SETQ VAL (QUOTIENT (DIFFERENCE VAL VAL1) VALUE))
         (SETQ I 1))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ZEROP VAL))) (RETURN NIL)))
        (PROGN
         (SETQ VAL1
                 ((LAMBDA (NN MMODULUS)
                    (COND
                     ((GREATERP NN (QUOTIENT MMODULUS 2))
                      (DIFFERENCE NN MMODULUS))
                     (T NN)))
                  (REMAINDER VAL VALUE) VALUE))
         (COND
          ((NOT (ZEROP VAL1))
           (PROGN
            (SETQ KONT (GCDN VAL1 KONT))
            (SETQ D (CONS (CONS (CONS VAR I) VAL1) D)))))
         (SETQ VAL (QUOTIENT (DIFFERENCE VAL VAL1) VALUE))
         (SETQ I (PLUS 1 I)))
        (GO WHILELABEL))
      (RETURN ((LAMBDA (*EXP) (QUOTF1 D KONT)) T)))) 
(PUT 'GEN-POLY-BACKWARD 'NUMBER-OF-ARGS 4) 
(PUT 'GEN-POLY-BACKWARD 'DEFINED-ON-LINE '413) 
(PUT 'GEN-POLY-BACKWARD 'DEFINED-IN-FILE 'POLY/HEUGCD.RED) 
(PUT 'GEN-POLY-BACKWARD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEN-POLY-BACKWARD (DVAL VALUE VAR XSX)
    (PROG (I D ANS VAL VAL1 KONT)
      (SETQ KONT 0)
      (SETQ VAL DVAL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ZEROP VAL))) (RETURN NIL)))
        (PROGN
         (SETQ VAL1
                 ((LAMBDA (NN MMODULUS)
                    (COND
                     ((GREATERP NN (QUOTIENT MMODULUS 2))
                      (DIFFERENCE NN MMODULUS))
                     (T NN)))
                  (REMAINDER VAL VALUE) VALUE))
         (SETQ D (CONS VAL1 D))
         (SETQ VAL (QUOTIENT (DIFFERENCE VAL VAL1) VALUE)))
        (GO WHILELABEL))
      (SETQ I XSX)
      (COND
       ((AND (ZEROP I) (NOT (ZEROP (CAR D))))
        (PROGN (SETQ KONT (SETQ ANS (CAR D))) (SETQ D (CDR D)) (SETQ I 1))))
      (PROG ()
       WHILELABEL
        (COND ((NOT D) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (ZEROP (CAR D)))
           (PROGN
            (SETQ KONT (GCDN (CAR D) KONT))
            (SETQ ANS (CONS (CONS (CONS VAR I) (CAR D)) ANS)))))
         (SETQ D (CDR D))
         (SETQ I (PLUS I 1)))
        (GO WHILELABEL))
      (RETURN ((LAMBDA (*EXP) (QUOTF1 ANS KONT)) T)))) 
(ENDMODULE) 