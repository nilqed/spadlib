(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFAIRY)) 
(PUT 'MYFSERIES 'NUMBER-OF-ARGS 1) 
(FLAG '(MYFSERIES) 'OPFN) 
(PUT 'MYFSERIES 'DEFINED-ON-LINE '49) 
(PUT 'MYFSERIES 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'MYFSERIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYFSERIES (Z)
    (PROG (SUMM ACCU TERM ZCUBE INT1 INT2)
      (SETQ SUMM (AEVAL 1))
      (SETQ INT1 (AEVAL 2))
      (SETQ INT2 (AEVAL 3))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ ZCUBE (AEVAL (LIST 'EXPT Z 3)))
      (PROG (KK)
        (SETQ KK 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 30 KK)) (RETURN NIL)))
        (PROGN
         (SETQ TERM
                 (AEVAL*
                  (LIST 'TIMES TERM
                        (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
         (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
         (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
         (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
         (AEVAL* 'NIL))
        (SETQ KK (PLUS2 KK 1))
        (GO LAB))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
              (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'MYGSERIES 'NUMBER-OF-ARGS 1) 
(FLAG '(MYGSERIES) 'OPFN) 
(PUT 'MYGSERIES 'DEFINED-ON-LINE '94) 
(PUT 'MYGSERIES 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'MYGSERIES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYGSERIES (Z)
    (PROG (K SUMM ACCU TERM ZCUBE INT1 INT2)
      (SETQ SUMM (AEVAL Z))
      (SETQ INT1 (AEVAL 3))
      (SETQ INT2 (AEVAL 4))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL SUMM))
      (SETQ ZCUBE (AEVAL (LIST 'EXPT Z 3)))
      (PROG (KK)
        (SETQ KK 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 30 KK)) (RETURN NIL)))
        (PROGN
         (SETQ TERM
                 (AEVAL*
                  (LIST 'TIMES TERM
                        (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
         (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
         (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
         (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
         (AEVAL* 'NIL))
        (SETQ KK (PLUS2 KK 1))
        (GO LAB))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
              (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'AIRYA2 'NUMBER-OF-ARGS 2) 
(FLAG '(AIRYA2) 'OPFN) 
(PUT 'AIRYA2 'DEFINED-ON-LINE '129) 
(PUT 'AIRYA2 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'AIRYA2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AIRYA2 (Z PROC)
    (PROG (C1 C2 SUMM OLDPREC)
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'PLUS OLDPREC 10)))
      (SETQ C1
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'EXPT 3 (LIST 'MINUS (LIST 'QUOTIENT 2 3)))
                     (LIST 'GAMMA (LIST 'QUOTIENT 2 3)))))
      (SETQ C2
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'EXPT 3 (LIST 'MINUS (LIST 'QUOTIENT 1 3)))
                     (LIST 'GAMMA (LIST 'QUOTIENT 1 3)))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AI))
        (SETQ SUMM
                (AEVAL
                 (LIST 'DIFFERENCE (LIST 'TIMES C1 (LIST 'MYFSERIES Z))
                       (LIST 'TIMES C2 (LIST 'MYGSERIES Z))))))
       (T
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES (LIST 'SQRT 3)
                       (LIST 'PLUS (LIST 'TIMES C1 (LIST 'MYFSERIES Z))
                             (LIST 'TIMES C2 (LIST 'MYGSERIES Z))))))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL SUMM)))) 
(PUT 'ASUM1 'NUMBER-OF-ARGS 2) 
(FLAG '(ASUM1) 'OPFN) 
(PUT 'ASUM1 'DEFINED-ON-LINE '162) 
(PUT 'ASUM1 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'ASUM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASUM1 (Z PROC)
    (PROG (P K SUMM ACCU TERM ZTERM)
      (SETQ SUMM (AEVAL 1))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ ZTERM
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                     (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT
                                   (LIST 'TIMES
                                         (COND
                                          ((EVALEQUAL (AEVAL* PROC)
                                                      (AEVAL* 'AI))
                                           (MINUS 1))
                                          (T 1))
                                         (LIST 'QUOTIENT
                                               (LIST 'TIMES
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 1
                                                                 2))
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 3
                                                                 2))
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 5
                                                                 2)))
                                               (LIST 'TIMES 54 K
                                                     (LIST 'DIFFERENCE K
                                                           (LIST 'QUOTIENT 1
                                                                 2)))))
                                   ZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'ASUM2 'NUMBER-OF-ARGS 1) 
(FLAG '(ASUM2) 'OPFN) 
(PUT 'ASUM2 'DEFINED-ON-LINE '191) 
(PUT 'ASUM2 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'ASUM2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASUM2 (Z)
    (PROG (P K SUMM ACCU TERM SQZTERM SQNUM)
      (SETQ SUMM (AEVAL 1))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ SQZTERM
              (AEVAL
               (LIST 'EXPT
                     (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                           (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))
                     2)))
      (SETQ SQNUM (AEVAL (LIST 'EXPT 54 2)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT
                                   (LIST 'TIMES (MINUS 1)
                                         (LIST 'DIFFERENCE (LIST 'TIMES 6 K)
                                               '(|:DN:| 55 . -1))
                                         (LIST 'DIFFERENCE (LIST 'TIMES 6 K)
                                               '(|:DN:| 45 . -1))
                                         (LIST 'DIFFERENCE (LIST 'TIMES 6 K)
                                               '(|:DN:| 35 . -1))
                                         (LIST 'DIFFERENCE (LIST 'TIMES 6 K)
                                               '(|:DN:| 25 . -1))
                                         (LIST 'DIFFERENCE (LIST 'TIMES 6 K)
                                               '(|:DN:| 15 . -1))
                                         (LIST 'QUOTIENT
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 5 . -1))
                                               (LIST 'TIMES SQNUM
                                                     (LIST 'TIMES 2 K)
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 K) 1)
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 K)
                                                           '(|:DN:| 15 . -1))
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 K)
                                                           '(|:DN:| 5 . -1)))))
                                   SQZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'ASUM3 'NUMBER-OF-ARGS 1) 
(FLAG '(ASUM3) 'OPFN) 
(PUT 'ASUM3 'DEFINED-ON-LINE '214) 
(PUT 'ASUM3 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'ASUM3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASUM3 (Z)
    (PROG (P K SUMM ACCU TERM ZTERM SQZTERM SQNUM)
      (SETQ ZTERM
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                     (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))))
      (SETQ SQZTERM (AEVAL (LIST 'EXPT ZTERM 2)))
      (SETQ SQNUM (AEVAL (LIST 'EXPT 54 2)))
      (SETQ SUMM
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                           (LIST 'QUOTIENT (LIST 'QUOTIENT 5 2) 54))
                     ZTERM)))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                           (LIST 'QUOTIENT (LIST 'QUOTIENT 5 2) 54))
                     ZTERM)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT
                                   (LIST 'TIMES (MINUS 1)
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 1 2))
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 3 2))
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 5 2))
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 7 2))
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 9 2))
                                         (LIST 'QUOTIENT
                                               (LIST 'DIFFERENCE
                                                     (LIST 'PLUS
                                                           (LIST 'TIMES 6 K) 3)
                                                     (LIST 'QUOTIENT 11 2))
                                               (LIST 'TIMES SQNUM
                                                     (LIST 'TIMES 2 K)
                                                     (LIST 'PLUS
                                                           (LIST 'TIMES 2 K) 1)
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 2 K)
                                                           (LIST 'QUOTIENT 1
                                                                 2))
                                                     (LIST 'PLUS
                                                           (LIST 'TIMES 2 K)
                                                           (LIST 'QUOTIENT 1
                                                                 2)))))
                                   SQZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'ASAIRYAM 'NUMBER-OF-ARGS 2) 
(FLAG '(ASAIRYAM) 'OPFN) 
(PUT 'ASAIRYAM 'DEFINED-ON-LINE '247) 
(PUT 'ASAIRYAM 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'ASAIRYAM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASAIRYAM (MINUSZ PROC)
    (PROG (TT P EE SUMM)
      (SETK 'Z (AEVAL (LIST 'MINUS MINUSZ)))
      (SETQ TT (AEVAL (LIST 'EXPT 'Z (LIST 'MINUS (LIST 'QUOTIENT 1 4)))))
      (SETQ P (AEVAL (LIST 'EXPT 'PI (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))
      (SETQ EE
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                           (LIST 'EXPT 'Z (LIST 'QUOTIENT 3 2)))
                     (LIST 'QUOTIENT 'PI 4))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AI))
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES TT P
                       (LIST 'DIFFERENCE
                             (LIST 'TIMES (LIST 'SIN EE) (LIST 'ASUM2 'Z))
                             (LIST 'TIMES (LIST 'COS EE) (LIST 'ASUM3 'Z)))))))
       (T
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES TT P
                       (LIST 'PLUS
                             (LIST 'TIMES (LIST 'COS EE) (LIST 'ASUM2 'Z))
                             (LIST 'TIMES (LIST 'SIN EE)
                                   (LIST 'ASUM3 'Z))))))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'ASAIRYAP 'NUMBER-OF-ARGS 2) 
(FLAG '(ASAIRYAP) 'OPFN) 
(PUT 'ASAIRYAP 'DEFINED-ON-LINE '263) 
(PUT 'ASAIRYAP 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'ASAIRYAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ASAIRYAP (Z PROC)
    (PROG (TT P EE SUMM)
      (SETQ TT (AEVAL (LIST 'EXPT Z (LIST 'MINUS (LIST 'QUOTIENT 1 4)))))
      (SETQ P (AEVAL (LIST 'EXPT 'PI (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))
      (SETQ EE
              (AEVAL
               (LIST 'EXPT 'E
                     (LIST 'TIMES
                           (COND
                            ((EVALEQUAL (AEVAL PROC) (AEVAL 'AI)) (MINUS 1))
                            (T 1))
                           (LIST 'QUOTIENT 2 3)
                           (LIST 'EXPT Z (LIST 'QUOTIENT 3 2))))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AI))
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES (LIST 'QUOTIENT 1 2) TT P EE
                       (LIST 'ASUM1 Z 'AI)))))
       (T (SETQ SUMM (AEVAL (LIST 'TIMES TT P EE (LIST 'ASUM1 Z 'BI))))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'MYFSERIESP 'NUMBER-OF-ARGS 1) 
(FLAG '(MYFSERIESP) 'OPFN) 
(PUT 'MYFSERIESP 'DEFINED-ON-LINE '282) 
(PUT 'MYFSERIESP 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'MYFSERIESP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYFSERIESP (Z)
    (PROG (K SUMM ACCU TERM ZCUBE INT1 INT2)
      (SETQ SUMM (AEVAL (LIST 'QUOTIENT (LIST 'EXPT Z 2) 2)))
      (SETQ INT1 (AEVAL 3))
      (SETQ INT2 (AEVAL 5))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL (LIST 'QUOTIENT (LIST 'EXPT Z 2) 2)))
      (SETQ ZCUBE (AEVAL (LIST 'EXPT Z 3)))
      (PROG (KK)
        (SETQ KK 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 30 KK)) (RETURN NIL)))
        (PROGN
         (SETQ TERM
                 (AEVAL*
                  (LIST 'TIMES TERM
                        (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
         (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
         (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
         (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
         (AEVAL* 'NIL))
        (SETQ KK (PLUS2 KK 1))
        (GO LAB))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
              (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'MYGSERIESP 'NUMBER-OF-ARGS 1) 
(FLAG '(MYGSERIESP) 'OPFN) 
(PUT 'MYGSERIESP 'DEFINED-ON-LINE '310) 
(PUT 'MYGSERIESP 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'MYGSERIESP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MYGSERIESP (Z)
    (PROG (K SUMM ACCU TERM ZCUBE INT1 INT2)
      (SETQ SUMM (AEVAL 1))
      (SETQ INT1 (AEVAL 3))
      (SETQ INT2 (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ ZCUBE (AEVAL (LIST 'EXPT Z 3)))
      (PROG (KK)
        (SETQ KK 0)
       LAB
        (COND ((MINUSP (DIFFERENCE 30 KK)) (RETURN NIL)))
        (PROGN
         (SETQ TERM
                 (AEVAL*
                  (LIST 'TIMES TERM
                        (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
         (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
         (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
         (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
         (AEVAL* 'NIL))
        (SETQ KK (PLUS2 KK 1))
        (GO LAB))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT ZCUBE (LIST 'TIMES INT1 INT2)))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ INT1 (AEVAL* (LIST 'PLUS INT1 3)))
              (SETQ INT2 (AEVAL* (LIST 'PLUS INT2 3)))
              (AEVAL* 'NIL)))
      (RETURN (AEVAL SUMM)))) 
(PUT 'AIRYAP 'NUMBER-OF-ARGS 2) 
(FLAG '(AIRYAP) 'OPFN) 
(PUT 'AIRYAP 'DEFINED-ON-LINE '340) 
(PUT 'AIRYAP 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'AIRYAP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AIRYAP (Z PROC)
    (PROG (C1 C2 SUMM OLDPREC)
      (SETQ OLDPREC (AEVAL (LIST 'PRECISION 0)))
      (AEVAL (LIST 'PRECISION (LIST 'PLUS OLDPREC 10)))
      (SETQ C1
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'EXPT 3 (LIST 'MINUS (LIST 'QUOTIENT 2 3)))
                     (LIST 'GAMMA (LIST 'QUOTIENT 2 3)))))
      (SETQ C2
              (AEVAL
               (LIST 'QUOTIENT
                     (LIST 'EXPT 3 (LIST 'MINUS (LIST 'QUOTIENT 1 3)))
                     (LIST 'GAMMA (LIST 'QUOTIENT 1 3)))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AIPRIME))
        (SETQ SUMM
                (AEVAL
                 (LIST 'DIFFERENCE (LIST 'TIMES C1 (LIST 'MYFSERIESP Z))
                       (LIST 'TIMES C2 (LIST 'MYGSERIESP Z))))))
       (T
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES (LIST 'SQRT 3)
                       (LIST 'PLUS (LIST 'TIMES C1 (LIST 'MYFSERIESP Z))
                             (LIST 'TIMES C2 (LIST 'MYGSERIESP Z))))))))
      (AEVAL (LIST 'PRECISION OLDPREC))
      (RETURN (AEVAL SUMM)))) 
(PUT 'APSUM1 'NUMBER-OF-ARGS 2) 
(FLAG '(APSUM1) 'OPFN) 
(PUT 'APSUM1 'DEFINED-ON-LINE '357) 
(PUT 'APSUM1 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'APSUM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE APSUM1 (Z PROC)
    (PROG (P K SUMM ACCU TERM ZTERM)
      (SETQ SUMM (AEVAL 1))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ ZTERM
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                     (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'QUOTIENT
                                   (LIST 'TIMES
                                         (COND
                                          ((EVALEQUAL (AEVAL* PROC)
                                                      (AEVAL* 'AIPRIME))
                                           (MINUS 1))
                                          (T 1))
                                         (LIST 'TIMES
                                               (LIST 'QUOTIENT
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 6 K) 7)
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 6 K)
                                                           5))
                                               (LIST 'QUOTIENT
                                                     (LIST 'PLUS
                                                           (LIST 'TIMES 6 K) 1)
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 6 K)
                                                           1)))
                                         (LIST 'QUOTIENT
                                               (LIST 'TIMES
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 1
                                                                 2))
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 3
                                                                 2))
                                                     (LIST 'DIFFERENCE
                                                           (LIST 'TIMES 3 K)
                                                           (LIST 'QUOTIENT 5
                                                                 2)))
                                               (LIST 'TIMES 54 K
                                                     (LIST 'DIFFERENCE K
                                                           (LIST 'QUOTIENT 1
                                                                 2)))))
                                   ZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'APSUM2 'NUMBER-OF-ARGS 1) 
(FLAG '(APSUM2) 'OPFN) 
(PUT 'APSUM2 'DEFINED-ON-LINE '379) 
(PUT 'APSUM2 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'APSUM2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE APSUM2 (Z)
    (PROG (P K SUMM ACCU TERM SQZTERM SQNUM)
      (SETQ SUMM (AEVAL 1))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM (AEVAL 1))
      (SETQ SQZTERM
              (AEVAL
               (LIST 'EXPT
                     (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                           (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))
                     2)))
      (SETQ SQNUM (AEVAL (LIST 'EXPT 54 2)))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM (MINUS 1)
                             (LIST 'TIMES
                                   (LIST 'QUOTIENT
                                         (LIST 'DIFFERENCE (LIST 'TIMES 12 K)
                                               13)
                                         (LIST 'DIFFERENCE (LIST 'TIMES 12 K)
                                               11))
                                   (LIST 'QUOTIENT
                                         (LIST 'PLUS (LIST 'TIMES 12 K) 1)
                                         (LIST 'DIFFERENCE (LIST 'TIMES 12 K)
                                               1)))
                             (LIST 'QUOTIENT
                                   (LIST 'QUOTIENT
                                         (LIST 'TIMES
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 55 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 45 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 35 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 25 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 15 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 6 K)
                                                     '(|:DN:| 5 . -1)))
                                         (LIST 'TIMES SQNUM (LIST 'TIMES 2 K)
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2 K) 1)
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2 K)
                                                     '(|:DN:| 15 . -1))
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2 K)
                                                     '(|:DN:| 5 . -1))))
                                   SQZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'APSUM3 'NUMBER-OF-ARGS 1) 
(FLAG '(APSUM3) 'OPFN) 
(PUT 'APSUM3 'DEFINED-ON-LINE '401) 
(PUT 'APSUM3 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'APSUM3 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE APSUM3 (Z)
    (PROG (P K SUMM ACCU TERM ZTERM SQZTERM SQNUM)
      (SETQ ZTERM
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                     (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))))
      (SETQ SQZTERM (AEVAL (LIST 'EXPT ZTERM 2)))
      (SETQ SQNUM (AEVAL (LIST 'EXPT 54 2)))
      (SETQ SUMM
              (AEVAL
               (LIST 'TIMES (LIST 'MINUS (LIST 'QUOTIENT 7 5))
                     (LIST 'QUOTIENT
                           (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                 (LIST 'QUOTIENT (LIST 'QUOTIENT 5 2) 54))
                           ZTERM))))
      (SETQ K (AEVAL 1))
      (SETQ ACCU (AEVAL (LIST 'EXPT 10 (LIST 'MINUS |:PREC:|))))
      (SETQ TERM
              (AEVAL
               (LIST 'TIMES (LIST 'MINUS (LIST 'QUOTIENT 7 5))
                     (LIST 'QUOTIENT
                           (LIST 'TIMES (LIST 'QUOTIENT 3 2)
                                 (LIST 'QUOTIENT (LIST 'QUOTIENT 5 2) 54))
                           ZTERM))))
      (WHILE (EVALGREATERP (AEVAL* (LIST 'ABS TERM)) (AEVAL* ACCU))
             (PROGN
              (SETQ TERM
                      (AEVAL*
                       (LIST 'TIMES TERM
                             (LIST 'TIMES (MINUS 1)
                                   (LIST 'QUOTIENT
                                         (LIST 'DIFFERENCE (LIST 'TIMES 12 K)
                                               7)
                                         (LIST 'DIFFERENCE (LIST 'TIMES 12 K)
                                               5))
                                   (LIST 'QUOTIENT
                                         (LIST 'PLUS (LIST 'TIMES 12 K) 7)
                                         (LIST 'PLUS (LIST 'TIMES 12 K) 5)))
                             (LIST 'DIFFERENCE (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                   (LIST 'QUOTIENT 1 2))
                             (LIST 'DIFFERENCE (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                   (LIST 'QUOTIENT 3 2))
                             (LIST 'DIFFERENCE (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                   (LIST 'QUOTIENT 5 2))
                             (LIST 'DIFFERENCE (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                   (LIST 'QUOTIENT 7 2))
                             (LIST 'DIFFERENCE (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                   (LIST 'QUOTIENT 9 2))
                             (LIST 'QUOTIENT
                                   (LIST 'QUOTIENT
                                         (LIST 'DIFFERENCE
                                               (LIST 'PLUS (LIST 'TIMES 6 K) 3)
                                               (LIST 'QUOTIENT 11 2))
                                         (LIST 'TIMES SQNUM (LIST 'TIMES 2 K)
                                               (LIST 'PLUS (LIST 'TIMES 2 K) 1)
                                               (LIST 'DIFFERENCE
                                                     (LIST 'TIMES 2 K)
                                                     (LIST 'QUOTIENT 1 2))
                                               (LIST 'PLUS (LIST 'TIMES 2 K)
                                                     (LIST 'QUOTIENT 1 2))))
                                   SQZTERM))))
              (SETQ SUMM (AEVAL* (LIST 'PLUS SUMM TERM)))
              (SETQ K (AEVAL* (LIST 'PLUS K 1)))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'AIRYAPP 'NUMBER-OF-ARGS 2) 
(FLAG '(AIRYAPP) 'OPFN) 
(PUT 'AIRYAPP 'DEFINED-ON-LINE '427) 
(PUT 'AIRYAPP 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'AIRYAPP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AIRYAPP (Z PROC)
    (PROG (TT P)
      (SETQ TT (AEVAL (LIST 'EXPT Z (LIST 'QUOTIENT 1 4))))
      (SETQ P (AEVAL (LIST 'EXPT 'PI (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))
      (SETK 'EE
            (AEVAL
             (LIST 'EXPT 'E
                   (LIST 'TIMES
                         (COND
                          ((EVALEQUAL (AEVAL PROC) (AEVAL 'AIPRIME)) (MINUS 1))
                          (T 1))
                         (LIST 'QUOTIENT 2 3)
                         (LIST 'EXPT Z (LIST 'QUOTIENT 3 2))))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AIPRIME))
        (SETK 'SUMM
              (AEVAL
               (LIST 'TIMES (LIST 'QUOTIENT 1 2) TT P 'EE
                     (LIST 'APSUM1 Z 'AI)))))
       (T (SETK 'SUMM (AEVAL (LIST 'TIMES TT P 'EE (LIST 'APSUM1 Z 'BI))))))
      (RETURN (AEVAL 'SUMM)))) 
(PUT 'AIRYAPM 'NUMBER-OF-ARGS 2) 
(FLAG '(AIRYAPM) 'OPFN) 
(PUT 'AIRYAPM 'DEFINED-ON-LINE '439) 
(PUT 'AIRYAPM 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'AIRYAPM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AIRYAPM (Z PROC)
    (PROG (TT P EE SUMM)
      (SETQ TT (AEVAL (LIST 'EXPT Z (LIST 'QUOTIENT 1 4))))
      (SETQ P (AEVAL (LIST 'EXPT 'PI (LIST 'MINUS (LIST 'QUOTIENT 1 2)))))
      (SETQ EE
              (AEVAL
               (LIST 'PLUS
                     (LIST 'TIMES (LIST 'QUOTIENT 2 3)
                           (LIST 'EXPT Z (LIST 'QUOTIENT 3 2)))
                     (LIST 'QUOTIENT 'PI 4))))
      (COND
       ((EVALEQUAL (AEVAL PROC) (AEVAL 'AIPRIME))
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES TT (LIST 'MINUS P)
                       (LIST 'PLUS
                             (LIST 'TIMES (LIST 'COS EE) (LIST 'APSUM2 Z))
                             (LIST 'TIMES (LIST 'SIN EE) (LIST 'APSUM3 Z)))))))
       (T
        (SETQ SUMM
                (AEVAL
                 (LIST 'TIMES TT P
                       (LIST 'DIFFERENCE
                             (LIST 'TIMES (LIST 'COS EE) (LIST 'APSUM2 Z))
                             (LIST 'TIMES (LIST 'SIN EE)
                                   (LIST 'APSUM3 Z))))))))
      (RETURN (AEVAL SUMM)))) 
(PUT 'AI_ASYMPTOTIC 'NUMBER-OF-ARGS 1) 
(FLAG '(AI_ASYMPTOTIC) 'OPFN) 
(PUT 'AI_ASYMPTOTIC 'DEFINED-ON-LINE '465) 
(PUT 'AI_ASYMPTOTIC 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'AI_ASYMPTOTIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AI_ASYMPTOTIC (ABSZ)
    (PROG (PREC)
      (SETQ PREC (AEVAL |:PREC:|))
      (RETURN
       (COND ((AND (EVALLEQ (AEVAL PREC) 6) (EVALGREATERP (AEVAL ABSZ) 5)) 1)
             ((AND (EVALLEQ (AEVAL PREC) 12) (EVALGREATERP (AEVAL ABSZ) 8)) 1)
             ((AND (EVALLEQ (AEVAL PREC) 16) (EVALGREATERP (AEVAL ABSZ) 10)) 1)
             ((AND (EVALLEQ (AEVAL PREC) 23) (EVALGREATERP (AEVAL ABSZ) 12)) 1)
             ((AND (EVALLEQ (AEVAL PREC) 33) (EVALGREATERP (AEVAL ABSZ) 15)) 1)
             (T 0))))) 
(PUT 'NUM_AIRY 'NUMBER-OF-ARGS 2) 
(FLAG '(NUM_AIRY) 'OPFN) 
(PUT 'NUM_AIRY 'DEFINED-ON-LINE '485) 
(PUT 'NUM_AIRY 'DEFINED-IN-FILE 'SPECFN/SFAIRY.RED) 
(PUT 'NUM_AIRY 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_AIRY (Z FNAME)
    (PROG (SUMM)
      (COND
       ((EVALEQUAL (AEVAL FNAME) (AEVAL 'AI))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'AI_ASYMPTOTIC (LIST 'ABS Z))) 1)
           (PROGN
            (COND
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG (LIST 'MINUS Z))))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 2 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'ASAIRYAM Z 'AI))))
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG Z))) (AEVAL 'PI))
              (SETQ SUMM (AEVAL (LIST 'ASAIRYAP Z 'AI)))))
            (AEVAL 'NIL)))
          (T (SETQ SUMM (AEVAL (LIST 'AIRYA2 Z 'AI)))))
         (RETURN (AEVAL SUMM))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL FNAME) (AEVAL 'BI))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'AI_ASYMPTOTIC (LIST 'ABS Z))) 1)
           (PROGN
            (COND
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG (LIST 'MINUS Z))))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 2 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'ASAIRYAM Z 'BI))))
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG Z)))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 1 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'ASAIRYAP Z 'BI)))))
            (AEVAL 'NIL)))
          (T (SETQ SUMM (AEVAL (LIST 'AIRYA2 Z 'BI)))))
         (RETURN (AEVAL SUMM))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL FNAME) (AEVAL 'AIPRIME))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'AI_ASYMPTOTIC (LIST 'ABS Z))) 1)
           (PROGN
            (COND
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG (LIST 'MINUS Z))))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 2 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'AIRYAPM Z 'AIPRIME))))
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG Z))) (AEVAL 'PI))
              (SETQ SUMM (AEVAL (LIST 'AIRYAPP Z 'AIPRIME)))))
            (AEVAL 'NIL)))
          (T (SETQ SUMM (AEVAL (LIST 'AIRYAP Z 'AIPRIME)))))
         (RETURN (AEVAL SUMM))
         (AEVAL 'NIL)))
       ((EVALEQUAL (AEVAL FNAME) (AEVAL 'BIPRIME))
        (PROGN
         (COND
          ((EVALEQUAL (AEVAL (LIST 'AI_ASYMPTOTIC (LIST 'ABS Z))) 1)
           (PROGN
            (COND
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG (LIST 'MINUS Z))))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 2 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'AIRYAPM Z 'BIPRIME))))
             ((EVALLESSP (AEVAL (LIST 'ABS (LIST 'ARG Z)))
                         (AEVAL (LIST 'TIMES (LIST 'QUOTIENT 1 3) 'PI)))
              (SETQ SUMM (AEVAL (LIST 'AIRYAPP Z 'BIPRIME)))))
            (AEVAL 'NIL)))
          (T (SETQ SUMM (AEVAL (LIST 'AIRYAP Z 'BIPRIME)))))
         (RETURN (AEVAL SUMM))
         (AEVAL 'NIL)))))) 
(FLAG '(AIRY_AI AIRY_BI AIRY_AIPRIME AIRY_BIPRIME) 'SPECFN) 
(DEFLIST '((AIRY_AI 1) (AIRY_BI 1) (AIRY_AIPRIME 1) (AIRY_BIPRIME 1))
         'NUMBER-OF-ARGS) 
(AEVAL (OPERATOR (LIST 'AIRY_AI 'AIRY_BI 'AIRY_AIPRIME 'AIRY_BIPRIME))) 
(SETK 'AIRY_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'AIRY_AI 0)
                   (LIST 'QUOTIENT
                         (LIST 'EXPT 3 (LIST 'MINUS (LIST 'QUOTIENT 2 3)))
                         (LIST 'GAMMA (LIST 'QUOTIENT 2 3))))
             (LIST 'REPLACEBY (LIST 'AIRY_AI (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'NUM_AIRY 'Z 'AI)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'NUMBERP 'Z))))
             (LIST 'REPLACEBY (LIST 'DF (LIST 'AIRY_AI (LIST '~ 'Z)) 'Z)
                   (LIST 'AIRY_AIPRIME 'Z))
             (LIST 'REPLACEBY (LIST 'AIRY_BI 0)
                   (LIST 'TIMES (LIST 'SQRT 3)
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 3
                                     (LIST 'MINUS (LIST 'QUOTIENT 2 3)))
                               (LIST 'GAMMA (LIST 'QUOTIENT 2 3)))))
             (LIST 'REPLACEBY (LIST 'AIRY_BI (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'NUM_AIRY 'Z 'BI)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'NUMBERP 'Z))))
             (LIST 'REPLACEBY (LIST 'DF (LIST 'AIRY_BI (LIST '~ 'Z)) 'Z)
                   (LIST 'AIRY_BIPRIME 'Z))
             (LIST 'REPLACEBY (LIST 'AIRY_AIPRIME 0)
                   (LIST 'MINUS
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 3
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 3)))
                               (LIST 'GAMMA (LIST 'QUOTIENT 1 3)))))
             (LIST 'REPLACEBY (LIST 'AIRY_AIPRIME (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'NUM_AIRY 'Z 'AIPRIME)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'NUMBERP 'Z))))
             (LIST 'REPLACEBY (LIST 'DF (LIST 'AIRY_AIPRIME (LIST '~ 'Z)) 'Z)
                   (LIST 'TIMES 'Z (LIST 'AIRY_AI 'Z)))
             (LIST 'REPLACEBY (LIST 'AIRY_BIPRIME 0)
                   (LIST 'TIMES (LIST 'SQRT 3)
                         (LIST 'QUOTIENT
                               (LIST 'EXPT 3
                                     (LIST 'MINUS (LIST 'QUOTIENT 1 3)))
                               (LIST 'GAMMA (LIST 'QUOTIENT 1 3)))))
             (LIST 'REPLACEBY (LIST 'AIRY_BIPRIME (LIST '~ 'Z))
                   (LIST 'WHEN (LIST 'NUM_AIRY 'Z 'BIPRIME)
                         (LIST 'AND (LIST 'SYMBOLIC '*ROUNDED)
                               (LIST 'NUMBERP 'Z))))
             (LIST 'REPLACEBY (LIST 'DF (LIST 'AIRY_BIPRIME (LIST '~ 'Z)) 'Z)
                   (LIST 'TIMES 'Z (LIST 'AIRY_BI 'Z)))))) 
(AEVAL (LET '(AIRY_RULES))) 
(AEVAL (PUT 'AIRY_AI 'PLAIN-FUNCTIONSYMBOL '|aI|)) 
(AEVAL (PUT 'AIRY_BI 'PLAIN-FUNCTIONSYMBOL '|bI|)) 
(AEVAL (PUT 'AIRY_AIPRIME 'PLAIN-FUNCTIONSYMBOL '|aI'|)) 
(AEVAL (PUT 'AIRY_BIPRIME 'PLAIN-FUNCTIONSYMBOL '|bI'|)) 
(AEVAL (PUT 'AIRY_AI 'PRIFN 'PLAIN-SYMBOL)) 
(AEVAL (PUT 'AIRY_BI 'PRIFN 'PLAIN-SYMBOL)) 
(AEVAL (PUT 'AIRY_AIPRIME 'PRIFN 'PLAIN-SYMBOL)) 
(AEVAL (PUT 'AIRY_BIPRIME 'PRIFN 'PLAIN-SYMBOL)) 
(SETK 'AIRY2BESSEL_RULES
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'AIRY_AI (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 1 3) (LIST 'SQRT 'Z)
                               (LIST 'PROGN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''WHEREEXP
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2)))))
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         1 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 1
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'GEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_AI (LIST '~ 'MINUSZ))
                   (LIST 'WHEN
                         (LIST 'PROGN
                               (LIST 'AEVAL
                                     (LIST 'LIST ''WHEREEXP
                                           (LIST 'LIST ''LIST
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2))))
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''Z
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   ''MINUSZ))))
                                           (LIST 'LIST ''PLUS
                                                 (LIST 'LIST ''TIMES
                                                       (LIST 'LIST ''SQRT
                                                             (LIST 'LIST
                                                                   ''QUOTIENT
                                                                   ''Z 3))
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 1
                                                                   3)
                                                             ''EE))
                                                 (LIST 'LIST ''BESSELJ
                                                       (LIST 'LIST ''MINUS
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 1
                                                                   3))
                                                       ''EE)))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'LEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_AIPRIME (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'MINUS
                               (LIST 'TIMES (LIST 'QUOTIENT 'Z 3)
                                     (LIST 'PROGN
                                           (LIST 'AEVAL
                                                 (LIST 'LIST ''WHEREEXP
                                                       (LIST 'LIST ''LIST
                                                             (LIST 'LIST
                                                                   ''REPLACEBY
                                                                   ''EE
                                                                   (LIST 'LIST
                                                                         ''TIMES
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          2 3)
                                                                         (LIST
                                                                          'LIST
                                                                          ''EXPT
                                                                          ''Z
                                                                          (LIST
                                                                           'LIST
                                                                           ''QUOTIENT
                                                                           3
                                                                           2)))))
                                                       (LIST 'LIST ''DIFFERENCE
                                                             (LIST 'LIST
                                                                   ''BESSELI
                                                                   (LIST 'LIST
                                                                         ''MINUS
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          2 3))
                                                                   ''EE)
                                                             (LIST 'LIST
                                                                   ''BESSELI
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   ''EE)))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'GEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_AIPRIME (LIST '~ 'MINUSZ))
                   (LIST 'WHEN
                         (LIST 'PROGN
                               (LIST 'AEVAL
                                     (LIST 'LIST ''WHEREEXP
                                           (LIST 'LIST ''LIST
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2))))
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''Z
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   ''MINUSZ))))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''MINUS
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''Z 3))
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 2
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'LEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_BI (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'SQRT (LIST 'QUOTIENT 'Z 3))
                               (LIST 'PROGN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''WHEREEXP
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2)))))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         1 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 1
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'GEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_BI (LIST '~ 'MINUSZ))
                   (LIST 'WHEN
                         (LIST 'PROGN
                               (LIST 'AEVAL
                                     (LIST 'LIST ''WHEREEXP
                                           (LIST 'LIST ''LIST
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2))))
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''Z
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   ''MINUSZ))))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''SQRT
                                                       (LIST 'LIST ''QUOTIENT
                                                             ''Z 3))
                                                 (LIST 'LIST ''DIFFERENCE
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         1 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 1
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z) (LIST 'LEQ 'REPART 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_BIPRIME (LIST '~ 'Z))
                   (LIST 'WHEN
                         (LIST 'TIMES (LIST 'QUOTIENT 'Z (LIST 'SQRT 3))
                               (LIST 'PROGN
                                     (LIST 'AEVAL
                                           (LIST 'LIST ''WHEREEXP
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2)))))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELI
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 2
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'GEQ (LIST 'REPART 'Z) 0))))
             (LIST 'REPLACEBY (LIST 'AIRY_BIPRIME (LIST '~ 'MINUSZ))
                   (LIST 'WHEN
                         (LIST 'PROGN
                               (LIST 'AEVAL
                                     (LIST 'LIST ''WHEREEXP
                                           (LIST 'LIST ''LIST
                                                 (LIST 'LIST ''LIST
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''EE
                                                             (LIST 'LIST
                                                                   ''TIMES
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3)
                                                                   (LIST 'LIST
                                                                         ''EXPT
                                                                         ''Z
                                                                         (LIST
                                                                          'LIST
                                                                          ''QUOTIENT
                                                                          3
                                                                          2))))
                                                       (LIST 'LIST ''REPLACEBY
                                                             ''Z
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   ''MINUSZ))))
                                           (LIST 'LIST ''TIMES
                                                 (LIST 'LIST ''QUOTIENT ''Z
                                                       (LIST 'LIST ''SQRT 3))
                                                 (LIST 'LIST ''PLUS
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''MINUS
                                                                   (LIST 'LIST
                                                                         ''QUOTIENT
                                                                         2 3))
                                                             ''EE)
                                                       (LIST 'LIST ''BESSELJ
                                                             (LIST 'LIST
                                                                   ''QUOTIENT 2
                                                                   3)
                                                             ''EE))))))
                         (LIST 'AND (LIST 'NUMBERP 'Z)
                               (LIST 'LEQ (LIST 'REPART 'Z) 0))))))) 
(AEVAL 'NIL) 
(ENDMODULE) 