(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NRSTROOT)) 
(GLOBAL '(BFZ* |CPVAL#|)) 
(FLUID
 '(*MSG *MULTIROOT |CPXT#| |PFACTOR#| NRST$ |INTV#| |PFL#| *BFTAG |SS#| |ACCM#|
   |PRM#| |PREC#| *MB *GFP *KEEPIMP *XO NRST$ |PNN#| |PFX#| 1RP *COMPLEX
   *RESIMP |PGCD#| *XN *XOBF |PREC#| *HARDTST |ACC#| |RR#| |PRX#|)) 
(PUT 'NEARESTROOT 'NUMBER-OF-ARGS 1) 
(PUT 'NEARESTROOT 'DEFINED-ON-LINE '47) 
(PUT 'NEARESTROOT 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'NEARESTROOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEARESTROOT (ARGS)
    ((LAMBDA (P RR) (NRSTROOT P RR NIL)) (CAR ARGS) (CADR ARGS))) 
(PUT 'NEARESTROOT 'PSOPFN 'NEARESTROOT) 
(PUT 'NRSTROOT 'NUMBER-OF-ARGS 3) 
(PUT 'NRSTROOT 'DEFINED-ON-LINE '56) 
(PUT 'NRSTROOT 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'NRSTROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NRSTROOT (P RR TRIB)
    (PROG (X C D DX XM R CP N M S P1 |PRX#| ACC PP M1 *MSG *MULTIROOT |CPXT#|
           |PFACTOR#| NRST$ |INTV#| |PFL#| |ACFL#| *BFTAG |SS#| |ACCM#| |PRM#|
           |PREC#|)
      (SETQ |SS#| 0)
      (SETQ |ACCM#| 0)
      (SETQ |PRM#| 0)
      (SETQ |PREC#| 0)
      (!MFEFIX)
      (SETQ P (CDR (SETQ C (CKPZRO P))))
      (SETQ C (CAR C))
      (SETQ *MB NIL)
      (COND ((ATOM P) (PROGN (COND (C (SETQ C 0))) (GO RET))))
      (COND ((NULL RR) (SETQ RR 0)))
      (COND
       (TRIB
        (PROGN
         (SETQ |ACC#| (MAX |ACC#| (RR2ACC RR)))
         (SETQ *GFP (SETQ P (AUTOMOD P)))
         (SETQ *BFTAG T)
         (COND
          ((ATOM TRIB)
           (PROGN
            (SETQ *KEEPIMP (NUMBERP TRIB))
            (SETQ *XO
                    (COND
                     (*BFTAG
                      (CONS
                       (COND ((FLOATP 0) (FL2BF 0))
                             (T
                              (NORMBF
                               (COND ((NOT (ATOM 0)) 0)
                                     ((FIXP 0) (CONS '|:RD:| (CONS 0 0)))
                                     (T (|READ:NUM| 0))))))
                       BFZ*))
                     (T (CONS (CFLOT 0) 0.0))))
            (SETQ C (GFNEWTON P (A2GF RR) 4))
            (GO RET)))
          (T
           (PROGN
            (SETQ *KEEPIMP (NUMBERP (CAR TRIB)))
            (SETQ C (GFROOTFIND P (A2GF RR)))
            (GO RET)))))))
      (SETQ R (A2GF RR))
      (SETQ ACC (SETQ |ACC#| (MAX |ACC#| (RR2ACC RR))))
      (COND
       (C
        (COND
         ((COND
           ((NOT (ATOM (CAR R)))
            (AND (EQUAL (CADR (CAR R)) 0) (EQUAL (CADR (CDR R)) 0)))
           (T (EQUAL R '(0.0 . 0.0))))
          (PROGN (SETQ C 0) (GO RET)))
         (T
          (PROGN
           (SETQ D
                   ((LAMBDA (X)
                      (COND ((FLOATP X) (FL2BF X))
                            (T
                             (NORMBF
                              (COND ((NOT (ATOM X)) X)
                                    ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                    (T (|READ:NUM| X)))))))
                    (GFRSQ R)))
           (SETQ XM BFZ*))))))
      (SETQ M (POWERCHK P))
      (SETQ NRST$ T)
      (SETQ P (GFSQFRF P))
      (AUTOMOD 1RP)
      (SETQ N |PNN#|)
      (SETQ P1 (BFLOATEM 1RP))
      (COND ((GREATERP (LENGTH P) 1) (SETQ |PFACTOR#| (SETQ |PRX#| N))))
      (COND
       ((NOT
         (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
             (OR (NUMBERP (CDAR P))
                 (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P))))))))
        (SETQ |CPXT#| T)))
      (COND
       (M
        (PROGN
         (SETQ P (GFSQFRF (CDR M)))
         (SETQ M (CAR M))
         (SETQ |SS#| (SETQ S (CEILING (LOG10 (FLOAT M))))))))
     LOOP
      (SETQ PP (AUTOMOD (CAR (SETQ X (CAR P)))))
      (SETQ CP NIL)
      (COND
       ((NOT
         (OR (OR (NUMBERP PP) (AND (EQCAR PP '|:RD:|) (NOT (ATOM (CDR PP)))))
             (OR (NUMBERP (CDAR PP))
                 (AND (EQCAR (CDAR PP) '|:RD:|)
                      (NOT (ATOM (CDR (CDAR PP))))))))
        (PROGN
         (SETQ PP (CAR (SETQ CP (CSEP PP))))
         (SETQ CP (CDR CP))
         (COND ((ATOM PP) (GO CPR)) (T (SETQ |PFACTOR#| (SETQ |PRX#| N)))))))
     MOD
      (SETQ PP (AUTOMOD PP))
      (SETQ R (GF2BF R))
      (COND
       ((SETQ M1 (POWERCHK PP)) (PROGN (SETQ PP (CDR M1)) (SETQ M1 (CAR M1)))))
      (COND
       ((AND (NOT M) (NOT M1)) (PROGN (SETQ X (NRSTRT0 PP R P1)) (GO COL))))
      (SETQ X
              (COND
               (M1
                (NRPASS2 M1 (NRPASS1 PP R (COND (M (TIMES M1 M)) (T M1))) R P1
                 ACC))
               (T (NRPASS1 PP R M))))
      (COND (M (SETQ X (NRPASS2 M X R P1 ACC))))
     COL
      (SETQ X (CDR X))
      (SETQ DX
              (GFRSQ
               (GFDIFFER
                (COND
                 ((OR (NUMBERP X) (AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))))
                  (SETQ X (CONS X BFZ*)))
                 (T X))
                (GF2BF R))))
      (COND
       ((OR (NOT D) (AND D (BFLEQP DX D)))
        (PROGN (SETQ D DX) (SETQ XM (MKDN |CPVAL#|)))))
     CPR
      (COND (CP (PROGN (SETQ PP CP) (SETQ CP NIL) (GO MOD))))
      (COND
       ((AND (SETQ P (CDR P)) (NOT (OR (ATOM (CAAR P)) (ATOM (CAR (CAAR P))))))
        (GO LOOP)))
      (SETQ C XM)
     RET
      (RETURN (ALLOUT (COND (C (LIST (CONS C |ACC#|))) (T NIL)))))) 
(PUT 'RR2ACC 'NUMBER-OF-ARGS 1) 
(PUT 'RR2ACC 'DEFINED-ON-LINE '109) 
(PUT 'RR2ACC 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'RR2ACC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RR2ACC (RR)
    ((LAMBDA (PR)
       (PROG (*MSG C)
         (SETQ C *COMPLEX)
         (ON (LIST 'COMPLEX))
         (PROG (N)
           (SETQ N (RR2NL RR))
          LAB
           (COND ((NULL N) (RETURN NIL)))
           ((LAMBDA (N) (FORM1 N NIL 'ALGEBRAIC)) (CAR N))
           (SETQ N (CDR N))
           (GO LAB))
         ((LAMBDA (*RESIMP) (SIMP* RR)) T)
         (COND ((NOT C) (OFF (LIST 'COMPLEX))))
         (SETQ PR (PRECISION PR))
         (RETURN PR)))
     (PRECISION 6))) 
(PUT 'RR2NL 'NUMBER-OF-ARGS 1) 
(PUT 'RR2NL 'DEFINED-ON-LINE '118) 
(PUT 'RR2NL 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'RR2NL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RR2NL (RR) (RR2NL1 RR NIL)) 
(PUT 'RR2NL1 'NUMBER-OF-ARGS 2) 
(PUT 'RR2NL1 'DEFINED-ON-LINE '120) 
(PUT 'RR2NL1 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'RR2NL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RR2NL1 (RR NL)
    (COND ((NUMBERP RR) (CONS (LIST '|:INT:| RR) NL)) ((ATOM RR) NL)
          ((EQ (CAR RR) '|:DN:|) (CONS (LIST '|:INT:| (CADR RR)) NL))
          (T (RR2NL1 (CAR RR) (RR2NL1 (CDR RR) NL))))) 
(PUT 'NRSTRT0 'NUMBER-OF-ARGS 3) 
(PUT 'NRSTRT0 'DEFINED-ON-LINE '126) 
(PUT 'NRSTRT0 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'NRSTRT0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NRSTRT0 (Q R P1)
    (PROG (RR X B PR PS P2 QF)
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN (PROGN (PRIN2 (PBFPRINT Q)) NIL) (TERPRI))))
      (SETQ B *BFTAG)
      (SETQ PS (PLUS 2 (PRECISION 0)))
      (SETQ PR (MINPREC))
      (SETQ |PGCD#| (NOT P1))
      (SETQ P2
              ((LAMBDA (U)
                 (COND
                  ((NOT (ATOM (CAR U)))
                   (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
                  (T (EQUAL U '(0.0 . 0.0)))))
               (SETQ RR (A2GF R))))
      (SETQ *GFP (SETQ QF Q))
      (COND (B (GO R2)))
      (COND
       ((OR (ERRORP (SETQ Q (ERRORSET* (LIST 'CFLOTEM (MKQUOTE QF)) NIL)))
            (ERRORP (SETQ R (ERRORSET* (LIST 'GF2FL (MKQUOTE RR)) NIL))))
        (GO R1))
       (T (PROGN (SETQ Q (CAR Q)) (SETQ R (CAR R)))))
      (COND
       ((SETQ X (GFROOTSET Q R B))
        (PROGN
         (SETQ Q QF)
         (SETQ *XN (GF2BF *XN))
         (SETQ *XOBF (SETQ *XO (GF2BF *XO)))
         (GO R3))))
     R1
      (SETQ Q QF)
      (SETQ B (SETQ *BFTAG T))
      (SETQ R (GF2BF RR))
     R2
      (SETQ X (GFROOTFIND Q R))
      (SETQ *XOBF (SETQ *XO (GF2BF *XO)))
     R3
      (COND ((NOT *HARDTST) (SETQ X (CKACC Q (COND (P1 P1) (T Q)) *XN))))
      (SETQ X
              (ACCUROOT
               (COND
                ((COND ((ATOM (CDR R)) (ZEROP (CDR R)))
                       (T (EQUAL (CADR (CDR R)) 0)))
                 (CONS (CAR *XN)
                       ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
                        (CDR *XN))))
                (T *XN))
               Q *XO))
      (COND
       ((LESSP |PREC#| (PLUS PR 2))
        (PROGN
         (SETQ *BFTAG B)
         (PRECISION1 (DIFFERENCE PS 2) T)
         (RETURN (CONS |ACC#| X)))))
      (PRECISION1 (DIFFERENCE (SETQ PR |PREC#|) 2) T)
      (COND ((NOT *BFTAG) (SETQ B (SETQ *BFTAG T))))
      (COND (P2 (GO R2)) (T (PROGN (SETQ P2 T) (GO R1)))))) 
(PUT 'NRPASS1 'NUMBER-OF-ARGS 3) 
(PUT 'NRPASS1 'DEFINED-ON-LINE '148) 
(PUT 'NRPASS1 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'NRPASS1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NRPASS1 (PP RR M)
    ((LAMBDA (|SS#|) (NRSTRT0 PP (RRPWR RR M) NIL))
     (CEILING (LOG10 (FLOAT M))))) 
(PUT 'NRPASS2 'NUMBER-OF-ARGS 5) 
(PUT 'NRPASS2 'DEFINED-ON-LINE '151) 
(PUT 'NRPASS2 'DEFINED-IN-FILE 'ROOTS/NRSTROOT.RED) 
(PUT 'NRPASS2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NRPASS2 (M X RR P1 ACC)
    (PROG (S)
      (SETQ S (CEILING (LOG10 (FLOAT M))))
      (RETURN
       ((LAMBDA (|ACC#| |RR#| |SS#| |PFACTOR#|)
          (NRSTRT0 (PCONSTR M (CDR X)) RR P1))
        (MAX ACC (DIFFERENCE (CAR X) S)) 1 0
        (OR |PFACTOR#| (GREATERP (DIFFERENCE (CAR X) S) ACC)))))) 
(ENDMODULE) 