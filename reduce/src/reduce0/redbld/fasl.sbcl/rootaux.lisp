(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ROOTAUX)) 
(GLOBAL '(!NFPD MAX-ACC-INCR |LM#|)) 
(SETQ MAX-ACC-INCR 8) 
(FLUID
 '(*XN 1RP |ACC#| |SS#| |ROOTACC##| |RPREC#| |CPXT#| |PFACTOR#| |PRX#| NRST$
   |INTV#| |RLRT#| |LIMS#| |PNN#| |RR#| *STRM *XNLIST |SH#| *NOSTURM)) 
(FLUID '(*COMPXROOTS *BFTAG *ROUNDBF *MSG)) 
(PUT 'ACCUPR1 'NUMBER-OF-ARGS 2) 
(PUT 'ACCUPR1 'DEFINED-ON-LINE '45) 
(PUT 'ACCUPR1 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'ACCUPR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ACCUPR1 (Y P)
    ((LAMBDA (ACR)
       (PROGN
        (SETQ *XN Y)
        (PROG ()
         WHILELABEL
          (COND
           ((NOT (GREATERP (SETQ ACR (ACCUPR P (BFLOATEM 1RP) *XN)) |ACC#|))
            (RETURN NIL)))
          (PROGN
           (SETQ |ACC#| ACR)
           (SETQ Y
                   (ACCUROOT Y P
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
                     (T (CONS (CFLOT 0) 0.0))))))
          (GO WHILELABEL))
        (CONS Y (PLUS |ACC#| |SS#|))))
     |ACC#|)) 
(PUT 'UNIROOTS 'NUMBER-OF-ARGS 2) 
(PUT 'UNIROOTS 'DEFINED-ON-LINE '53) 
(PUT 'UNIROOTS 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'UNIROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNIROOTS (P RRTS)
    ((LAMBDA (*MSG)
       (PROGN
        (!MFEFIX)
        (COND
         ((EQCAR (REVAL1 P NIL) 'LIST)
          ((LAMBDA (PR *COMPXROOTS) (AEVAL (LIST 'MULTROOT PR P)))
           (COND (|ROOTACC##| |ROOTACC##|) (T (PRECISION 0)))
           (COND ((EQUAL RRTS 0) NIL) (T T))))
         ((EQUAL RRTS 0)
          ((LAMBDA (*BFTAG *ROUNDBF |RPREC#|)
             (PROGN
              (SETQ |RPREC#| (MAX !NFPD (OR |RPREC#| 7)))
              (UNIROOT0 P 0)))
           T T |RPREC#|))
         (T (UNIROOT0 P RRTS)))))
     NIL)) 
(PUT 'UNIROOT0 'NUMBER-OF-ARGS 2) 
(PUT 'UNIROOT0 'DEFINED-ON-LINE '65) 
(PUT 'UNIROOT0 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'UNIROOT0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UNIROOT0 (P RRTS)
    (PROG (C LIM N P1 PP Q R R1 RR X CP M |CPXT#| |PFACTOR#| ACC S |PRX#| M1
           NRST$ |INTV#| |RLRT#| RRC |SS#|)
      (SETQ |SS#| 0)
      (SETQ P (CDR (SETQ C (CKPZRO P))))
      (COND
       ((SETQ C (CAR C))
        (SETQ C (LIST (CONS (LIST (CONS (CAAAR C) 6)) (CDAR C))))))
      (COND
       (|LIMS#|
        (COND
         ((OR (NOT (CDR |LIMS#|))
              (PROGN
               (SETQ R (CAR |LIMS#|))
               (SETQ R1 (CADR |LIMS#|))
               (OR
                (AND (NEQ R 'MINFTY)
                     (COND ((EQCAR R 'LIST) (GEQ (CADR R) 0))
                           (T (GREATERP (CAR R) 0))))
                (AND (NEQ R1 'INFTY)
                     (COND ((EQCAR R1 'LIST) (LEQ (CADR R1) 0))
                           (T (LESSP (CAR R1) 0)))))))
          (SETQ C NIL)))))
      (COND
       ((ATOM P)
        (PROGN (SETQ R (COND (C C) (T (LIST (CONS NIL 1))))) (GO RET))))
      (COND
       ((NOT
         (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
             (OR (NUMBERP (CDAR P))
                 (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P))))))))
        (SETQ |CPXT#| T)))
      (SETQ M (POWERCHK P))
      (SETQ P
              (COND ((AND *NOSTURM (NEQ RRTS 0)) (LIST (CONS (SETQ 1RP P) 1)))
                    (T (GFSQFRF P))))
      (AUTOMOD 1RP)
      (SETQ N |PNN#|)
      (SETQ P1 1RP)
      (COND ((GREATERP (LENGTH P) 1) (SETQ |PFACTOR#| (SETQ |PRX#| N))))
      (COND
       (M
        (PROGN
         (SETQ P
                 (COND
                  ((AND *NOSTURM (NEQ RRTS 0))
                   (LIST (CONS (SETQ 1RP (CDR M)) 1)))
                  (T (GFSQFRF (CDR M)))))
         (SETQ M (CAR M))
         (SETQ |SS#| (SETQ S (CEILING (LOG10 (FLOAT M))))))))
      (SETQ LIM (PLUS |ACC#| MAX-ACC-INCR))
      (SETQ Q P)
      (SETQ R1 NIL)
      (SETQ R C)
      (SETQ ACC |ACC#|)
     LOOP
      (SETQ PP (AUTOMOD (CAR (SETQ X (CAR Q)))))
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
      (COND
       ((SETQ M1 (POWERCHK PP)) (PROGN (SETQ PP (CDR M1)) (SETQ M1 (CAR M1)))))
      (COND
       ((AND (NOT M) (NOT M1)) (PROGN (SETQ RR (DOROOTS PP RRTS T)) (GO COL))))
      (SETQ RR
              (COND
               (M1
                (RTPASS2 M1 (RTPASS1 PP M1 RRTS (COND (M (TIMES M1 M)) (T M1)))
                 RRTS P1 ACC M))
               (T (RTPASS1 PP M RRTS M))))
      (COND (M (SETQ RR (RTPASS2 M RR RRTS P1 ACC NIL))))
     COL
      (SETQ RRC
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y RR)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (CAR Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (Y)
        (SETQ Y RRC)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (COND ((MEMBER Y R1) (ROOTERR Y)))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ R1 (APPEND R1 RRC))
      (SETQ R (APPEND R (LIST (CONS RR (CDR X)))))
     CPR
      (COND
       ((AND CP (GREATERP RRTS 0))
        (PROGN (SETQ PP CP) (SETQ CP NIL) (GO MOD))))
      (COND
       ((AND (SETQ Q (CDR Q)) (NOT (OR (ATOM (CAAR Q)) (ATOM (CAR (CAAR Q))))))
        (GO LOOP)))
     RET
      (RETURN (OUTECHO R)))) 
(PUT 'RTPASS1 'NUMBER-OF-ARGS 4) 
(PUT 'RTPASS1 'DEFINED-ON-LINE '121) 
(PUT 'RTPASS1 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'RTPASS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RTPASS1 (PP M RRTS M2)
    ((LAMBDA (|LIMS#| |SS#|) (DOROOTS PP RRTS NIL)) (LIMADJ M2)
     (CEILING (LOG10 (FLOAT M))))) 
(PUT 'RTPASS2 'NUMBER-OF-ARGS 6) 
(PUT 'RTPASS2 'DEFINED-ON-LINE '124) 
(PUT 'RTPASS2 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'RTPASS2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE RTPASS2 (M RR RRTS P1 ACC M2)
    (PROG (PP S)
      (SETQ S (CEILING (LOG10 (FLOAT M))))
      (RETURN
       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
         (SETQ Y RR)
        STARTOVER
         (COND ((NULL Y) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (Y)
                    ((LAMBDA (1RP |ACC#| |RR#| |SS#| |PFACTOR#| |LIMS#|)
                       (PROGN
                        (SETQ PP (PCONSTR M (CAR Y)))
                        (DOROOTS PP RRTS (NOT M2))))
                     P1 (MAX ACC (DIFFERENCE (CDR Y) S)) 1 0
                     (OR |PFACTOR#| (GREATERP (DIFFERENCE (CDR Y) S) ACC))
                     (LIMADJ M2)))
                  (CAR Y)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ Y (CDR Y))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL Y) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (Y)
                    ((LAMBDA (1RP |ACC#| |RR#| |SS#| |PFACTOR#| |LIMS#|)
                       (PROGN
                        (SETQ PP (PCONSTR M (CAR Y)))
                        (DOROOTS PP RRTS (NOT M2))))
                     P1 (MAX ACC (DIFFERENCE (CDR Y) S)) 1 0
                     (OR |PFACTOR#| (GREATERP (DIFFERENCE (CDR Y) S) ACC))
                     (LIMADJ M2)))
                  (CAR Y)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ Y (CDR Y))
         (GO LOOPLABEL))))) 
(PUT 'DOROOTS 'NUMBER-OF-ARGS 3) 
(PUT 'DOROOTS 'DEFINED-ON-LINE '132) 
(PUT 'DOROOTS 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'DOROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE DOROOTS (PP R S) (COND ((EQUAL R 0) (RTSREAL PP S)) (T (ALLROOTS PP S)))) 
(PUT 'ROOTERR 'NUMBER-OF-ARGS 1) 
(PUT 'ROOTERR 'DEFINED-ON-LINE '135) 
(PUT 'ROOTERR 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'ROOTERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOTERR (Y)
    ((LAMBDA (*MSG)
       (LPRIM (LIST Y "is false repeated root.  Send input to S. L. Kameny")))
     T)) 
(PUT 'SCHINF 'NUMBER-OF-ARGS 1) 
(PUT 'SCHINF 'DEFINED-ON-LINE '139) 
(PUT 'SCHINF 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'SCHINF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCHINF (Z)
    (PROG (V V1 R)
      (SETQ R 0)
      (SETQ V (SCHINF1 (CAR *STRM) (SETQ Z (SGN Z))))
      (COND ((EQUAL V 0) (RETURN (SCHPLUS (REALRAT Z)))))
      (PROG (P)
        (SETQ P (CDR *STRM))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (PROGN
            (SETQ V1 (COND ((ATOM P) P) (T (SCHINF1 P Z))))
            (COND ((LESSP (TIMES V V1) 0) (SETQ R (PLUS R 1))))
            (COND ((NEQ V1 0) (SETQ V V1)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN R))) 
(PUT 'SCHPLUS 'NUMBER-OF-ARGS 1) 
(PUT 'SCHPLUS 'DEFINED-ON-LINE '148) 
(PUT 'SCHPLUS 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'SCHPLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCHPLUS (Z) (SCH (RATPLUS Z (OFFSETR (CAAR *STRM) Z)))) 
(PUT 'SCHINF1 'NUMBER-OF-ARGS 2) 
(PUT 'SCHINF1 'DEFINED-ON-LINE '150) 
(PUT 'SCHINF1 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'SCHINF1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SCHINF1 (P Z)
    (COND ((EQUAL Z 0) (CAR (LASTPAIR P)))
          (T (TIMES (EXPT Z (CAR P)) (SGN (CADR P)))))) 
(PUT 'BFNEWTON 'NUMBER-OF-ARGS 5) 
(PUT 'BFNEWTON 'DEFINED-ON-LINE '153) 
(PUT 'BFNEWTON 'DEFINED-IN-FILE 'ROOTS/ROOTAUX.RED) 
(PUT 'BFNEWTON 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE BFNEWTON (P P1 NX RI KMAX)
    (PROG (PX PF PF0 X0 XE K XK XR LP M)
      (SETQ M 0)
      (SETQ *XNLIST NIL)
      (SETQ |LM#| 0)
      (SETQ |LM#| (NWTERRFX (CAAR (LASTPAIR P)) NIL))
      (GFSTORVAL
       (SETQ PF0
               ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
                (SETQ PX (RLVAL P NX))))
       NX)
      (COND
       ((COND ((ATOM PF0) (ZEROP PF0)) (T (EQUAL (CADR PF0) 0)))
        (PROGN (COND (*TRROOT (TRMSG1A 'NWT NX))) (GO RET))))
     NEWT
      (SETQ X0 NX)
      (COND
       (((LAMBDA (U) (COND ((ATOM U) (ZEROP U)) (T (EQUAL (CADR U) 0))))
         (SETQ XE (RLVAL P1 NX)))
        (GO RET1)))
      (SETQ NX (BFPLUS NX (SETQ XE (BFMINUS (BFDIVIDE PX XE)))))
      (SETQ PX (RLVAL P NX))
      (COND ((NOT RI) (GO TST2)))
      (COND
       ((AND (RATLEQP (CAR RI) (SETQ XR (REALRAT NX))) (RATLEQP XR (CDR RI)))
        (GO TST)))
      (SETQ NX
              (TIGHTEN RI P (COND ((ATOM PX) (ABS PX)) (T (|ABS:| PX))) |SH#|))
      (COND ((NULL *XNLIST) (GO RET2)))
      (MOVEBDS RI (SETQ XR (RATMEAN (CAR RI) (CDR RI))) |SH#|)
      (SETQ PX (RLVAL P (SETQ NX (R2FLBF XR))))
      (SETQ LP (SETQ K (SETQ XK (SETQ PF NIL))))
      (GO NEWT)
     TST
      (MOVEBDS RI XR |SH#|)
      (COND ((BDSTEST RI) (GO RET)))
     TST2
      (SETQ PF0 PF)
      (SETQ PF (COND ((ATOM PX) (ABS PX)) (T (|ABS:| PX))))
      (COND
       ((AND (NOT LP) PF0 (BFLEQP PF0 PF))
        (PROGN (COND ((LESSP KMAX 2) (GO RET3)) (T (SETQ LP T))))))
      (COND (*TRROOT (TRMSG2A (COND (LP 'LOOP) (T 'NWT)) NX PX)))
      (COND
       ((COND ((ATOM PF) (ZEROP PF)) (T (EQUAL (CADR PF) 0)))
        (PROGN (COND (*TRROOT (TRMSG1A 'NWT NX))) (GO RET))))
      (COND
       ((BFEQP NX X0) (PROGN (COND (*TRROOT (TRMSG3A 'NWT NX))) (GO RET))))
      (GFSTORVAL PF NX)
      (COND (K (PROGN (SETQ XK (BFPLUS XK NX)) (SETQ K (PLUS K 1))))
            (LP (PROGN (SETQ K 1) (SETQ XK NX))))
      (COND
       ((EQUAL K KMAX)
        (PROGN
         (SETQ NX
                 ((LAMBDA (G162)
                    (COND ((ATOM XK) (TIMES G162 XK))
                          (T
                           (NORMBF
                            (|ROUND:MT|
                             (|TIMES:|
                              (COND ((FLOATP G162) (FL2BF G162))
                                    (T
                                     (NORMBF
                                      (COND ((NOT (ATOM G162)) G162)
                                            ((FIXP G162)
                                             (CONS '|:RD:| (CONS G162 0)))
                                            (T (|READ:NUM| G162))))))
                              XK)
                             |:BPREC:|)))))
                  (QUOTIENT 1.0 K)))
         (GFSTORVAL
          ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
           (SETQ PX (RLVAL P NX)))
          NX)
         (COND (*TRROOT (TRMSG6A K NX PX)))
         (GO RET3))))
      (NWTERR (SETQ M (PLUS M 1)))
      (GO NEWT)
     RET3
      (SETQ NX (GFGETMIN))
      (COND (*TRROOT (TRMSG7A NX)))
      (GO RET)
     RET2
      (COND (NX (GO RET)))
     RET1
      (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A 'NWT)))
     RET
      (SETQ *XNLIST NIL)
      (RETURN NX))) 
(ENDMODULE) 