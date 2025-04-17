(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ALLROOT)) 
(EXPORTS (LIST 'ACCUROOT 'ALLROOTS 'GFNEWTON 'GFROOTFIND 'SIZATOM)) 
(IMPORTS
 (LIST '!MFEFIX 'A2GF 'ACCUPR 'ALLOUT 'AUTOMOD 'BFABS 'BFDIVIDE 'BFEQP 'BFLEQP
       'BFLESSP 'BFLOAT 'BFLOATEM 'BFMAX 'BFNEWTON 'BFNUMP 'BFNZP '|BFP:|
       'BFPRIM 'BFRLMULT 'BFRNDEM 'BFSQRT 'BFTIMES 'BFZP 'CEILLOG 'CEXPAND
       'CKACC 'CKPZRO 'CPXP 'CSEP 'CVT5 'DECIMAL2INTERNAL 'DEFLATE1 'DEFLATE1C
       'DEFLATE2 'DIVBF 'DOMAINP 'DSPLY '|EP:| 'ERRORP 'ERRORSET* 'GEQ 'GETPREC
       'GF2BF 'GF2FLT 'GFDIFF 'GFDIFFER 'GFDOT 'GFEQP 'GFEXIT 'GFGETMIN 'GFIM
       'GFMINUS 'GFNEWTSET 'GFPLUS 'GFQUOTIENT 'GFRL 'GFRLMULT 'GFROOTSET
       'GFRSQ 'GFRTRND 'GFSHIFT 'GFSQFRF 'GFSQFRF1 'GFSQRT 'GFSTORVAL 'GFTIMES
       'GFVAL 'GFZEROP 'IM2GF 'LASTPAIR 'LEQ 'LPRIM 'MINBND1 'MINPREC 'MKQUOTE
       'NCPXP 'NWTERR 'NWTERRFX 'ORGSHIFT 'PBFPRINT 'PCONSTR 'PFLUPD 'PMSG
       'POWERCHK 'RERROR 'RESTOREFL 'RL2GF 'RLRTNO 'RLVAL 'ROOTRND '|ROUND:MT|
       'RRPWR 'RXGFC 'RXGFRL 'RXRL 'SETEPS 'SETFLBF 'SETPREC 'SMPART 'SQRT
       'TIMBF 'TRMSG1 'TRMSG10 'TRMSG11 'TRMSG12 'TRMSG13 'TRMSG2 'TRMSG4
       'TRMSG6 'TRMSG7 'TRMSG8 'TRMSG9 'UNSHIFT 'XNSHIFT 'XNSIZ 'XOSHIFT)) 
(FLUID
 '(*TRROOT *BFTAG *ROOTMSG *MULTIROOT *POWERGCD *HARDTST *NOSTURM 2LOOP
   *NOINVERT)) 
(SWITCH
 (LIST (LIST 'EQUAL 'TRROOT 'OFF) (LIST 'EQUAL 'ROOTMSG 'OFF)
       (LIST 'EQUAL 'MULTIROOT 'ON) (LIST 'EQUAL 'NOSTURM 'OFF))) 
(FLUID '(*XNLIST *PFSAV *XMAX *XMAX2 *GFP |PGCD#| |ALLRL#|)) 
(FLUID '(*PCMP |PREC#| |ACC#| |SPREC#| *XN |EPS#| |ACCM#| *XOBF |FROOT#|)) 
(FLUID
 '(|NWMAX#| |LGMAX#| *XO *KEEPIMP 1RP *ZX1 *MB |THT#| |PRM#| *BFSH *STRM
   |MLTR#| |EMSG#| |LIMS#| INCMSG$ |CPXT#| |SH#| |PFL#| |ACFL#| |PFACTOR#|
   |RPREC#| |RR#| |SS#| |PRX#| NRST$ *XD *ZP |INTV#| |PNN#|)) 
(GLOBAL '(BFONE* BFHALF* BFZ* !LOG2OF10 |CPVAL#| POLNIX$ POLREM$ |LM#|)) 
(SETQ |NWMAX#| 200) 
(SETQ |LGMAX#| 100) 
(SETQ *MULTIROOT (SETQ *POWERGCD T)) 
(PUT 'GFROOTFIND 'NUMBER-OF-ARGS 2) 
(PUT 'GFROOTFIND 'DEFINED-ON-LINE '70) 
(PUT 'GFROOTFIND 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'GFROOTFIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFROOTFIND (P NX)
    (PROG (P1 P2 PX X1 X2 X3 X0 XD N1 GFQT XZ RSC NJP LGERR PF XN2 T1 T2 *PFSAV
           PF0 PF1 PFN LP XLIM FG FG2 IP IP0 NXL2 N R M NI)
      (SETQ N 0)
      (SETQ R 0)
      (SETQ M 0)
      (SETQ NI 0)
      (SETQ |LM#| 0)
      (SETQ *XNLIST (SETQ |EMSG#| (SETQ *XD NIL)))
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN (PROGN (PRIN2 (PBFPRINT P)) NIL) (TERPRI))))
      (COND (*TRROOT (TRMSG8A)))
      (SETQ *PCMP
              (NOT
               (OR
                (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                (OR (NUMBERP (CDAR P))
                    (AND (EQCAR (CDAR P) '|:RD:|)
                         (NOT (ATOM (CDR (CDAR P)))))))))
      (COND
       ((GREATERP (CAAR P) 0)
        (PROGN
         (RESTOREFL)
         (RERROR 'ROOTS 7 "Roots error: zero root out of sequence!"))))
      (COND
       ((EQUAL (SETQ N (CAAR (LASTPAIR P))) 1)
        (PROGN
         (SETQ P (CDAR (BFPRIM P)))
         (SETQ NX
                 (GFMINUS
                  (COND (*PCMP P)
                        (T
                         (COND
                          (*BFTAG
                           (CONS
                            (COND ((FLOATP P) (FL2BF P))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM P)) P)
                                          ((FIXP P) (CONS '|:RD:| (CONS P 0)))
                                          (T (|READ:NUM| P))))))
                            BFZ*))
                          (T (CONS (CFLOT P) 0.0)))))))
         (GFSHIFT NIL)
         (COND (*TRROOT (TRMSG11A NX 1)))
         (GO RET1))))
      (COND
       ((AND NX (AND (EQCAR (CAR NX) '|:RD:|) (NOT (ATOM (CDR (CAR NX)))))
             (NOT *BFTAG))
        (PROGN (SETQ P *GFP) (SETQ *BFTAG T))))
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
      (SETEPS)
      (SETQ |LM#| (TIMES 2 (NWTERRFX N NIL)))
      (COND ((OR (LESSP N 3) *HARDTST (NOT (XOSHIFT P NX))) (GFSHIFT NIL))
            (T
             (PROGN
              (SETQ P (GFSHIFT P))
              (COND
               ((AND *ROOTMSG *TRROOT)
                (PROGN (PROGN (PRIN2 (CONS 'XO *XO)) NIL) (TERPRI)))))))
      (SETQ NX
              (COND
               ((NOT NX)
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
               (T (XNSHIFT NX))))
      (SETQ P1 (GFDIFF P))
      (COND
       ((COND
         ((NOT (ATOM (CAR NX)))
          (AND (EQUAL (CADR (CAR NX)) 0) (EQUAL (CADR (CDR NX)) 0)))
         (T (EQUAL NX '(0.0 . 0.0))))
        (SETQ XZ T)))
      (SETQ PX (GFVAL P NX))
      (BFMAX P)
      (SETQ *ZP 0)
     STRT
      (SETQ PF (GFRSQ PX))
      (COND (*TRROOT (TRMSG13A N NX PX)))
      (COND
       ((COND ((ATOM PF) (ZEROP PF)) (T (EQUAL (CADR PF) 0)))
        (PROGN (COND (*TRROOT (TRMSG1A 'LAG NX))) (GO RET0))))
      (SETQ X1 (GFVAL P1 NX))
      (COND
       ((SETQ *ZX1
                (AND (NOT *MB)
                     ((LAMBDA (U)
                        (NOT
                         (COND ((ATOM U) (ZEROP U)) (T (EQUAL (CADR U) 0)))))
                      (GFRSQ X1))))
        (GO ST2)))
      (SETQ *MB (SETQ X2 NIL))
      (SETQ X1
              ((LAMBDA (G148)
                 (COND ((ATOM G148) (TIMES 2.0 G148))
                       (T
                        (NORMBF
                         (|ROUND:MT|
                          (|TIMES:|
                           (COND ((FLOATP 2.0) (FL2BF 2.0))
                                 (T
                                  (NORMBF
                                   (COND ((NOT (ATOM 2.0)) 2.0)
                                         ((FIXP 2.0)
                                          (CONS '|:RD:| (CONS 2.0 0)))
                                         (T (|READ:NUM| 2.0))))))
                           G148)
                          |:BPREC:|)))))
               (MINBND1 P NX)))
      (COND
       (*KEEPIMP
        (PROGN
         (COND
          ((OR *PCMP
               (NOT
                (COND ((ATOM (CDR NX)) (ZEROP (CDR NX)))
                      (T (EQUAL (CADR (CDR NX)) 0)))))
           (SETQ X2
                   (GFDIFFER NX
                             (COND
                              (*BFTAG
                               (CONS BFZ*
                                     (COND ((FLOATP X1) (FL2BF X1))
                                           (T
                                            (NORMBF
                                             (COND ((NOT (ATOM X1)) X1)
                                                   ((FIXP X1)
                                                    (CONS '|:RD:| (CONS X1 0)))
                                                   (T (|READ:NUM| X1))))))))
                              (T (CONS 0.0 (CFLOT X1))))))))
         (SETQ PX
                 (GFVAL P
                  (SETQ NX
                          (GFPLUS NX
                                  (COND
                                   (*BFTAG
                                    (CONS BFZ*
                                          (COND ((FLOATP X1) (FL2BF X1))
                                                (T
                                                 (NORMBF
                                                  (COND ((NOT (ATOM X1)) X1)
                                                        ((FIXP X1)
                                                         (CONS '|:RD:|
                                                               (CONS X1 0)))
                                                        (T
                                                         (|READ:NUM| X1))))))))
                                   (T (CONS 0.0 (CFLOT X1))))))))
         (COND ((NOT X2) (GO ST1)))))
       (T
        (PROGN
         (SETQ X2
                 (GFDIFFER NX
                           (COND
                            (*BFTAG
                             (CONS
                              (COND ((FLOATP X1) (FL2BF X1))
                                    (T
                                     (NORMBF
                                      (COND ((NOT (ATOM X1)) X1)
                                            ((FIXP X1)
                                             (CONS '|:RD:| (CONS X1 0)))
                                            (T (|READ:NUM| X1))))))
                              BFZ*))
                            (T (CONS (CFLOT X1) 0.0)))))
         (SETQ PX
                 (GFVAL P
                  (SETQ NX
                          (GFPLUS NX
                                  (COND
                                   (*BFTAG
                                    (CONS
                                     (COND ((FLOATP X1) (FL2BF X1))
                                           (T
                                            (NORMBF
                                             (COND ((NOT (ATOM X1)) X1)
                                                   ((FIXP X1)
                                                    (CONS '|:RD:| (CONS X1 0)))
                                                   (T (|READ:NUM| X1))))))
                                     BFZ*))
                                   (T (CONS (CFLOT X1) 0.0))))))))))
      (COND
       ((BFLESSP (GFRSQ (SETQ P2 (GFVAL P X2))) (GFRSQ PX))
        (PROGN (SETQ NX X2) (SETQ PX P2))))
     ST1
      (SETQ XZ NIL)
      (GO STRT)
     ST2
      (SETQ N1 (DIFFERENCE N 1))
      (SETQ P2 (GFDIFF P1))
      (SETQ XLIM
              ((LAMBDA (G150)
                 (COND ((ATOM G150) (TIMES 100.0 G150))
                       (T
                        (NORMBF
                         (|ROUND:MT|
                          (|TIMES:|
                           (COND ((FLOATP 100.0) (FL2BF 100.0))
                                 (T
                                  (NORMBF
                                   (COND ((NOT (ATOM 100.0)) 100.0)
                                         ((FIXP 100.0)
                                          (CONS '|:RD:| (CONS 100.0 0)))
                                         (T (|READ:NUM| 100.0))))))
                           G150)
                          |:BPREC:|)))))
               *XMAX2))
      (SETQ IP
              (NOT
               (COND ((ATOM (CDR NX)) (ZEROP (CDR NX)))
                     (T (EQUAL (CADR (CDR NX)) 0)))))
      (SETQ NXL2 (LIST NX))
     LAG
      (SETQ X3 (GFVAL P2 NX))
      (COND
       ((OR (NOT FG)
            ((LAMBDA (U) (COND ((ATOM U) (ZEROP U)) (T (EQUAL (CADR U) 0))))
             (GFRSQ X1)))
        (PROGN
         (COND ((AND *ROOTMSG *TRROOT) (PROGN (PROGN (PRIN2 0) NIL) (TERPRI))))
         (GO LAG0))))
      (SETQ GFQT (GFQUOTIENT PX X1))
      (SETQ XN2
              (GFTIMES (GFTIMES GFQT GFQT) (GFRLMULT 0.5 (GFQUOTIENT X3 X1))))
      (SETQ T1
              ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
               (GFDOT NX XN2)))
      (SETQ T2
              ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
               ((LAMBDA (G152)
                  (COND ((ATOM G152) (TIMES 0.002 G152))
                        (T
                         (NORMBF
                          (|ROUND:MT|
                           (|TIMES:|
                            (COND ((FLOATP 0.002) (FL2BF 0.002))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM 0.002)) 0.002)
                                          ((FIXP 0.002)
                                           (CONS '|:RD:| (CONS 0.002 0)))
                                          (T (|READ:NUM| 0.002))))))
                            G152)
                           |:BPREC:|)))))
                (GFDOT NX GFQT))))
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN
         (PROGN
          (PRIN2
           (COND
            ((NOT (COND ((ATOM T2) (ZEROP T2)) (T (EQUAL (CADR T2) 0))))
             (GF2FLT (BFDIVIDE T1 T2)))
            (T "nwt_del->0")))
          NIL)
         (TERPRI))))
      (COND ((BFLESSP T1 T2) (GO RET)))
     LAG0
      (SETQ X2
              (GFRLMULT N1
               (GFDIFFER (GFRLMULT N1 (GFTIMES X1 X1))
                         (GFRLMULT N (GFTIMES PX X3)))))
      (SETQ X2 (GFSQRT X2))
      (SETQ X0 NX)
      (SETQ XD (GFPLUS X1 X2))
      (SETQ X2 (GFDIFFER X1 X2))
      (COND ((BFLESSP (GFRSQ XD) (GFRSQ X2)) (SETQ XD X2)))
      (COND
       (((LAMBDA (U) (COND ((ATOM U) (ZEROP U)) (T (EQUAL (CADR U) 0))))
         (SETQ X2 (GFRSQ XD)))
        (PROGN
         (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A 'LAG)))
         (RETURN (SETQ *XNLIST NIL)))))
      (COND
       ((BFLESSP (BFTIMES XLIM X2)
                 ((LAMBDA (G153 G154)
                    (COND ((ATOM G154) (TIMES G153 G154))
                          (T
                           (NORMBF
                            (|ROUND:MT|
                             (|TIMES:|
                              (COND ((FLOATP G153) (FL2BF G153))
                                    (T
                                     (NORMBF
                                      (COND ((NOT (ATOM G153)) G153)
                                            ((FIXP G153)
                                             (CONS '|:RD:| (CONS G153 0)))
                                            (T (|READ:NUM| G153))))))
                              G154)
                             |:BPREC:|)))))
                  (TIMES N N) (GFRSQ PX)))
        (PROGN (SETQ LP T) (SETQ XN2 (GFRSQ NX)) (GO LAG1))))
      (SETQ XD (GFRLMULT (MINUS N) (GFQUOTIENT PX XD)))
      (SETQ NX (GFPLUS X0 XD))
      (COND ((BFLESSP (SETQ XN2 (GFRSQ NX)) *XMAX2) (GO LAG2)))
     LAG1
      (COND (RSC (GO LAG2)))
      (SETQ PF1 (SETQ PF0 (SETQ FG2 (SETQ *PFSAV (SETQ *XNLIST NIL)))))
      (COND
       (LP
        (PROGN
         (COND (*TRROOT (PROGN (PROGN (PRIN2 "root scaled") NIL) (TERPRI))))
         (SETQ NX
                 (COND
                  (XZ
                   ((LAMBDA (U)
                      (COND
                       (*BFTAG
                        (CONS
                         (COND ((FLOATP U) (FL2BF U))
                               (T
                                (NORMBF
                                 (COND ((NOT (ATOM U)) U)
                                       ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                                       (T (|READ:NUM| U))))))
                         BFZ*))
                       (T (CONS (CFLOT U) 0.0))))
                    ((LAMBDA (G156)
                       (COND ((ATOM G156) (TIMES 0.5 G156))
                             (T
                              (NORMBF
                               (|ROUND:MT|
                                (|TIMES:|
                                 (COND ((FLOATP 0.5) (FL2BF 0.5))
                                       (T
                                        (NORMBF
                                         (COND ((NOT (ATOM 0.5)) 0.5)
                                               ((FIXP 0.5)
                                                (CONS '|:RD:| (CONS 0.5 0)))
                                               (T (|READ:NUM| 0.5))))))
                                 G156)
                                |:BPREC:|)))))
                     *XMAX)))
                  (T
                   (PROGN
                    (SETQ XN2
                            ((LAMBDA (G158)
                               (COND ((ATOM G158) (TIMES 0.5 G158))
                                     (T
                                      (NORMBF
                                       (|ROUND:MT|
                                        (|TIMES:|
                                         (COND ((FLOATP 0.5) (FL2BF 0.5))
                                               (T
                                                (NORMBF
                                                 (COND ((NOT (ATOM 0.5)) 0.5)
                                                       ((FIXP 0.5)
                                                        (CONS '|:RD:|
                                                              (CONS 0.5 0)))
                                                       (T (|READ:NUM| 0.5))))))
                                         G158)
                                        |:BPREC:|)))))
                             (COND ((ATOM XN2) (SQRT (QUOTIENT *XMAX2 XN2)))
                                   (T
                                    (BFSQRT
                                     (NORMBF
                                      (|DIVIDE:| *XMAX2 XN2 |:BPREC:|)))))))
                    (GFRLMULT XN2 NX)))))
         (SETQ RSC T)))
       ((BFLESSP XLIM XN2) (SETQ LP T)))
      (SETQ PF (GFRSQ (SETQ PX (GFVAL P NX))))
      (GO LAG3)
     LAG2
      (COND
       (*XNLIST
        (PROG (Y)
          (SETQ Y *XNLIST)
         LAB
          (COND ((NULL Y) (RETURN NIL)))
          ((LAMBDA (Y) (COND ((EQUAL NX (CDR Y)) (SETQ NJP T)))) (CAR Y))
          (SETQ Y (CDR Y))
          (GO LAB))))
      (SETQ PF (GFRSQ (SETQ PX (GFVAL P NX))))
      (COND ((NOT FG) (SETQ FG T)) (T (PROGN (SETQ PF1 PF0) (SETQ PF0 PFN))))
      (SETQ IP0 IP)
      (SETQ IP
              (NOT
               (COND ((ATOM (CDR NX)) (ZEROP (CDR NX)))
                     (T (EQUAL (CADR (CDR NX)) 0)))))
      (COND
       ((AND IP (NOT IP0))
        (PROGN
         (SETQ PF1 (SETQ PF0 (SETQ FG2 (SETQ *PFSAV NIL))))
         (SETQ NI 1))))
      (SETQ PFN (PFLUPD PF))
      (COND
       (PF1
        (PROGN
         (COND ((NOT FG2) (PROGN (COND ((BFLESSP PFN PF0) (SETQ FG2 T)))))
               (T
                (PROGN
                 (COND
                  ((OR (BFLESSP PF0 PFN) (AND (BFEQP PF0 PF1) (BFEQP PF0 PFN)))
                   (GO NEWT)))))))))
      (COND (XZ (SETQ XZ NIL)) (T (GFSTORVAL PF NX)))
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN
         (PROGN (PRIN2 (MAPCAR *PFSAV (FUNCTION GF2FLT))) NIL)
         (TERPRI))))
     LAG3
      (COND (*TRROOT (TRMSG2A 'LAG NX PX)))
      (SETQ R (PLUS R 1))
      (SETQ NI (PLUS NI 1))
      (COND ((EQUAL (SETQ XD (GFEXIT PF NX X0 'LAG)) T) (GO RET0))
            ((AND XD (GREATERP (SETQ M (PLUS M 1)) 5))
             (PROGN
              (COND ((GFEQP NX XD) (GO RET0))
                    ((GREATERP R 2)
                     (PROGN (SETQ *XD (SETQ NX XD)) (GO RET2))))))
            (NJP (GO NEWT)))
      (COND ((NOT XD) (SETQ M 0)))
      (COND
       ((OR (AND (GREATERP NI 5) (GFEQP (CAR NXL2) NX))
            (SETQ LGERR (GREATERP R (PLUS |LGMAX#| |LM#|))))
        (PROGN
         (LPRIM
          (COND (LGERR (LIST "max LAG limit" (PLUS |LGMAX#| |LM#|) "exceeded"))
                (T "lag loop of length 2 found.")))
         (SETQ 2LOOP T)
         (RETURN (SETQ *XNLIST NIL))))
       ((GREATERP (LENGTH (SETQ NXL2 (NCONC NXL2 (LIST NX)))) 2)
        (SETQ NXL2 (CDR NXL2))))
      (SETQ X1 (GFVAL P1 NX))
      (GO LAG)
     RET1
      (SETQ NX (UNSHIFT NX))
      (GO RET2)
     NEWT
      (SETQ NX (GFGETMIN))
     RET
      (RETURN (GFNEWT2 P P1 NX 4))
     RET0
      (SETQ NX (UNSHIFT NX))
     RET2
      (SETQ *XNLIST NIL)
      (DSPLY NX)
      (RETURN (SETQ *XN NX)))) 
(PUT 'PSHIFT 'NUMBER-OF-ARGS 1) 
(PUT 'PSHIFT 'DEFINED-ON-LINE '187) 
(PUT 'PSHIFT 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'PSHIFT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSHIFT (P)
    (ORGSHIFT P
     (COND
      ((NOT
        (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
            (OR (NUMBERP (CDAR P))
                (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P))))))))
       *XO)
      (T (CAR *XO))))) 
(PUT 'GFNEWTON 'NUMBER-OF-ARGS 3) 
(PUT 'GFNEWTON 'DEFINED-ON-LINE '190) 
(PUT 'GFNEWTON 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'GFNEWTON 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFNEWTON (P NX K)
    (PROGN (SETQ P (PSHIFT P)) (GFNEWT2 P (GFDIFF P) (XNSHIFT NX) K))) 
(PUT 'GFNEWT2 'NUMBER-OF-ARGS 4) 
(PUT 'GFNEWT2 'DEFINED-ON-LINE '193) 
(PUT 'GFNEWT2 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'GFNEWT2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFNEWT2 (P P1 NX KMAX)
    (PROG (PF0 PF K XK LOOP X0 X1 XD PX RL M |THT#|)
      (SETQ M 0)
      (SETQ |THT#| 0)
      (SETQ *XNLIST (SETQ |EMSG#| (SETQ *XD NIL)))
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN (PROGN (PRIN2 (PBFPRINT P)) NIL) (TERPRI))))
      (COND (*TRROOT (TRMSG8A)))
      (COND
       ((AND
         (SETQ RL
                 (COND ((ATOM (CDR NX)) (ZEROP (CDR NX)))
                       (T (EQUAL (CADR (CDR NX)) 0))))
         (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
             (OR (NUMBERP (CDAR P))
                 (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P))))))))
        (PROGN
         (SETQ *BFSH T)
         (SETQ NX
                 ((LAMBDA (U)
                    (COND
                     (*BFTAG
                      (CONS
                       (COND ((FLOATP U) (FL2BF U))
                             (T
                              (NORMBF
                               (COND ((NOT (ATOM U)) U)
                                     ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                                     (T (|READ:NUM| U))))))
                       BFZ*))
                     (T (CONS (CFLOT U) 0.0))))
                  (BFNEWTON P P1 (CAR NX) |INTV#| KMAX)))
         (SETQ *BFSH NIL)
         (GO RET1))))
      (SETEPS)
      (SETQ *ZP (SETQ |LM#| 0))
      (SETQ |LM#| (NWTERRFX (CAAR (LASTPAIR P)) T))
      (COND
       (((LAMBDA (U)
           (COND
            ((NOT (ATOM (CAR U)))
             (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
            (T (EQUAL U '(0.0 . 0.0)))))
         (SETQ PX (GFVAL P NX)))
        (PROGN (COND (*TRROOT (TRMSG1A 'NWT NX))) (GO RET1))))
      (GFSTORVAL (GFRSQ PX) NX)
     NE0
      (SETQ X0 NX)
      (COND
       (((LAMBDA (U)
           (COND
            ((NOT (ATOM (CAR U)))
             (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
            (T (EQUAL U '(0.0 . 0.0)))))
         (SETQ X1 (GFVAL P1 NX)))
        (PROGN (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A 'NWT))) (GO RET1))))
      (SETQ NX (GFDIFFER NX (GFQUOTIENT PX X1)))
      (SETQ PF0 PF)
      (PROG (Y)
        (SETQ Y *XNLIST)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (COND ((EQUAL NX (CDR Y)) (SETQ LOOP T)))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (GFSTORVAL (SETQ PF (GFRSQ (SETQ PX (GFVAL P NX)))) NX)
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN (PROGN (PRIN2 (LIST (GF2FLT PF))) NIL) (TERPRI))))
      (COND
       ((AND PF0 (BFLEQP PF0 PF))
        (PROGN
         (SETQ LOOP T)
         (COND
          ((LESSP KMAX 2)
           (PROGN (COND (*TRROOT (TRMSG2A 'LOOP NX PX))) (GO RET)))))))
      (COND (*TRROOT (TRMSG2A (COND (LOOP 'LOOP) (T 'NWT)) NX PX)))
      (COND
       ((SETQ XD (GFEXIT PF NX X0 'NWT))
        (PROGN
         (COND ((OR (EQUAL XD T) (GFEQP NX XD)) (GO RET1))
               ((GREATERP M 0) (PROGN (SETQ *XD (SETQ NX XD)) (GO RET2)))))))
      (COND ((NOT LOOP) (GO NLP)))
      (COND (K (PROGN (SETQ XK (GFPLUS XK NX)) (SETQ K (PLUS K 1))))
            (T (PROGN (SETQ K 1) (SETQ XK NX))))
      (COND
       ((GEQ K KMAX)
        (PROGN
         (SETQ NX (GFRLMULT (QUOTIENT 1.0 K) XK))
         (GFSTORVAL (GFRSQ (SETQ PX (GFVAL P NX))) NX)
         (COND (*TRROOT (TRMSG6A K NX PX)))
         (GO RET))))
     NLP
      (NWTERR (SETQ M (PLUS M 1)))
      (GO NE0)
     RET
      (SETQ NX (GFGETMIN))
      (COND (*TRROOT (TRMSG7A NX)))
     RET1
      (SETQ NX (UNSHIFT NX))
     RET2
      (SETQ *XNLIST NIL)
      (DSPLY NX)
      (RETURN (SETQ *XN NX)))) 
(PUT 'ACCUROOT 'NUMBER-OF-ARGS 3) 
(PUT 'ACCUROOT 'DEFINED-ON-LINE '233) 
(PUT 'ACCUROOT 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'ACCUROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ACCUROOT (Y P XO)
    (PROG (RPREC B C N RL X PR0 PS Y0)
      (SETQ PS (PLUS 2 (PRECISION 0)))
      (SETQ RL
              ((LAMBDA (P)
                 (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P))))))
               (SETQ Y0 (SETQ Y (GF2BF Y)))))
      (SETQ B *BFTAG)
      (SETQ PR0 (MINPREC))
      (SETQ *XO XO)
      (SETQ *BFTAG T)
      (COND
       ((LESSP (SETQ N (CAAR (LASTPAIR P))) 2)
        (PROGN
         (PRECISION1 (DIFFERENCE (MAX PS (PLUS |ACC#| 2)) 2) T)
         (COND
          (|PRX#|
           (PRECISION1 (DIFFERENCE (MAX (PLUS 2 (PRECISION 0)) |PRX#|) 2) T)))
         (SETQ Y (GFROOTFIND P NIL))
         (GO RET))))
      (SETQ X
              (COND
               (RL
                (CAR
                 (XNSHIFT
                  (SETQ Y
                          (COND
                           (*BFTAG
                            (CONS
                             (COND ((FLOATP Y) (FL2BF Y))
                                   (T
                                    (NORMBF
                                     (COND ((NOT (ATOM Y)) Y)
                                           ((FIXP Y) (CONS '|:RD:| (CONS Y 0)))
                                           (T (|READ:NUM| Y))))))
                             BFZ*))
                           (T (CONS (CFLOT Y) 0.0)))))))
               (T (XNSHIFT Y))))
      (COND
       ((NOT (SETQ RPREC (PRREQ P X RL)))
        (PROGN (SETQ *BFTAG B) (RETURN NIL))))
      (COND
       ((AND (NOT (OR |ALLRL#| RL))
             (OR
              (COND ((ATOM (CDR Y)) (ZEROP (CDR Y)))
                    (T (EQUAL (CADR (CDR Y)) 0)))
              (COND ((ATOM (CAR Y)) (ZEROP (CAR Y)))
                    (T (EQUAL (CADR (CAR Y)) 0)))))
        (SETQ *XD 1)))
      (COND
       ((LEQ RPREC PR0)
        (PROGN
         (PRECISION1 (DIFFERENCE PR0 2) T)
         (COND (*XD (GO BFP)) (T (GO RET))))))
      (PRECISION1 (DIFFERENCE RPREC 2) T)
     BFP
      (SETQ Y
              (COND
               ((OR
                 (AND (NOT RL)
                      (OR (GEQ RPREC (TIMES 2 PR0))
                          (COND ((ATOM (CDR Y)) (ZEROP (CDR Y)))
                                (T (EQUAL (CADR (CDR Y)) 0)))
                          (COND ((ATOM (CAR Y)) (ZEROP (CAR Y)))
                                (T (EQUAL (CADR (CAR Y)) 0)))))
                 *XD)
                (GFROOTFIND P Y))
               (T
                (PROGN
                 (COND (*TRROOT (TRMSG8A)))
                 (GFNEWTON P Y (COND (RL 2) (T 0)))))))
      (COND
       (*XD
        (PROGN
         (PRECISION1
          (DIFFERENCE (QUOTIENT (TIMES 3 (PLUS 2 (PRECISION 0))) 2) 2) T)
         (GO BFP))))
     RET
      (COND
       (|ACFL#|
        (PROGN
         (PRECISION1
          (DIFFERENCE (SETQ |PREC#| (SETQ |PRM#| (MAX RPREC |PRM#|))) 2) T)
         (GO R3))))
      (SETQ |PREC#| (PLUS 2 (PRECISION 0)))
      (COND ((OR RL (LESSP N 2) (NOT (SETQ C (SMPART Y)))) (GO R2)))
      (PRECISION1 (DIFFERENCE (PLUS |PREC#| 1) 2) T)
      (SETQ X (GFNEWTON P (SETQ Y (GF2BF *XN)) 0))
      (SETQ Y
              (SETQ *XN
                      (COND
                       ((EQUAL C T)
                        (COND
                         ((AND (NOT *PCMP) (CVT5 (CAR Y) (CAR X))
                               (NOT (CVT5 (CDR Y) (CDR X))))
                          (COND
                           (*BFTAG
                            (CONS
                             (COND ((FLOATP (CAR Y)) (FL2BF (CAR Y)))
                                   (T
                                    (NORMBF
                                     (COND ((NOT (ATOM (CAR Y))) (CAR Y))
                                           ((FIXP (CAR Y))
                                            (CONS '|:RD:| (CONS (CAR Y) 0)))
                                           (T (|READ:NUM| (CAR Y)))))))
                             BFZ*))
                           (T (CONS (CFLOT (CAR Y)) 0.0))))
                         (T Y)))
                       ((AND (CVT5 (CDR Y) (CDR X))
                             (NOT (CVT5 (CAR Y) (CAR X))))
                        (COND
                         (*BFTAG
                          (CONS BFZ*
                                (COND ((FLOATP (CDR Y)) (FL2BF (CDR Y)))
                                      (T
                                       (NORMBF
                                        (COND ((NOT (ATOM (CDR Y))) (CDR Y))
                                              ((FIXP (CDR Y))
                                               (CONS '|:RD:| (CONS (CDR Y) 0)))
                                              (T (|READ:NUM| (CDR Y)))))))))
                         (T (CONS 0.0 (CFLOT (CDR Y))))))
                       (T Y))))
     R2
      (PRECISION1 (DIFFERENCE PS 2) T)
      (COND
       ((AND (NOT RL)
             (OR
              (AND
               (COND ((ATOM (CAR Y)) (ZEROP (CAR Y)))
                     (T (EQUAL (CADR (CAR Y)) 0)))
               (NOT
                (COND ((ATOM (CAR Y0)) (ZEROP (CAR Y0)))
                      (T (EQUAL (CADR (CAR Y0)) 0)))))
              (AND
               (COND ((ATOM (CDR Y)) (ZEROP (CDR Y)))
                     (T (EQUAL (CADR (CDR Y)) 0)))
               (NOT
                (COND ((ATOM (CDR Y0)) (ZEROP (CDR Y0)))
                      (T (EQUAL (CADR (CDR Y0)) 0)))))))
        (SETQ |ACC#| (MAX |ACC#| (ACCUPR P (COND (|PGCD#| P) (T 1RP)) Y)))))
     R3
      (SETQ Y (COND (RL (ROOTRND (CAR Y))) (T (GFRTRND Y))))
      (COND (*TRROOT (TRMSG12A Y)))
      (SETQ *BFTAG B)
      (SETQ *XN (GF2BF *XN))
      (RETURN Y))) 
(PUT 'PRREQ 'NUMBER-OF-ARGS 3) 
(PUT 'PRREQ 'DEFINED-ON-LINE '278) 
(PUT 'PRREQ 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'PRREQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRREQ (P X RL)
    (PROG (P1 X1 RX)
      (SETQ P1 (GFDIFF (PSHIFT P)))
      (COND
       ((AND RL
             (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                 (OR (NUMBERP (CDAR P))
                     (AND (EQCAR (CDAR P) '|:RD:|)
                          (NOT (ATOM (CDR (CDAR P))))))))
        (PROGN
         (SETQ X1
                 ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
                  (RLVAL P1 X)))
         (SETQ RX (RXRL P1 X))))
       (T
        (PROGN
         (SETQ RX
                 (COND
                  ((NOT
                    (OR
                     (OR (NUMBERP P)
                         (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                     (OR (NUMBERP (CDAR P))
                         (AND (EQCAR (CDAR P) '|:RD:|)
                              (NOT (ATOM (CDR (CDAR P))))))))
                   (RXGFC P1 X))
                  (T (RXGFRL P1 X))))
         (SETQ X1 (BFSQRT (GFRSQ (GFVAL P1 X)))))))
      (RETURN
       (COND ((COND ((ATOM X1) (ZEROP X1)) (T (EQUAL (CADR X1) 0))) NIL)
             (T
              (PROGN
               (SETQ X (PLUS 2 (PRECISION 0)))
               (PRECISION1 (DIFFERENCE 8 2) T)
               (SETQ RL
                       (CDDR
                        (|ROUND:MT|
                         (NORMBF
                          (|DIVIDE:|
                           (NORMBF
                            (|ROUND:MT|
                             (|TIMES:| RX (DECIMAL2INTERNAL 1 (PLUS |ACC#| 2)))
                             |:BPREC:|))
                           X1 |:BPREC:|))
                         1)))
               (PRECISION1 (DIFFERENCE X 2) T)
               (PLUS 1 (CEILING (QUOTIENT RL !LOG2OF10))))))))) 
(PUT 'SIZATOM 'NUMBER-OF-ARGS 1) 
(PUT 'SIZATOM 'DEFINED-ON-LINE '293) 
(PUT 'SIZATOM 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'SIZATOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIZATOM (U)
    (PROG (C X)
      (SETQ C *COMPLEX)
      (ON (LIST 'COMPLEX))
      (SETQ X (PREPSQ (SIMP* U)))
      (COND ((NOT C) (OFF (LIST 'COMPLEX))))
      (COND ((NEQ X U) (RETURN X)) (T (RERROR 'ROOTS 8 "non-numeric value"))))) 
(PUT 'DSPLYRTNO 'NUMBER-OF-ARGS 1) 
(PUT 'DSPLYRTNO 'DEFINED-ON-LINE '300) 
(PUT 'DSPLYRTNO 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'DSPLYRTNO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DSPLYRTNO (M)
    ((LAMBDA (N) (PROGN (PROGN (PRIN2 "rootno. ") (PRIN2 M) NIL) (WRS N)))
     (WRS NIL))) 
(PUT 'ALLROOTS 'NUMBER-OF-ARGS 2) 
(PUT 'ALLROOTS 'DEFINED-ON-LINE '303) 
(PUT 'ALLROOTS 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'ALLROOTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ALLROOTS (P P0)
    (PROG (Q N N0 C CC CPRQ RLN CPN QF AC Y ER RL Z MB INC PREC XO PF XOF QBF
           SPREC B RED SW |PFL#| ACFL |ACFL#| *MSG PRQ |ALLRL#| INVP INVTD*
           PINV 1RPINV 1RP0 NMFG P00 ZI REQ NPI |ACCM#| |PREC#| R15N |PRM#| K
           RTN INVPB)
      (SETQ REQ 0)
      (SETQ NPI 0)
      (SETQ |ACCM#| 0)
      (SETQ |PREC#| 0)
      (SETQ R15N 0)
      (SETQ |PRM#| 0)
      (SETQ K 0)
      (SETQ RTN 0)
      (SETQ INVPB 0)
      (SETQ PREC (PLUS 2 (PRECISION 0)))
      (SETQ POLREM$ (SETQ POLNIX$ NIL))
      (SETQ *MSG T)
      (SETQ AC |ACC#|)
      (SETQ N0 (CAAR (LASTPAIR P)))
      (SETQ |PGCD#| (SETQ RED (NOT P0)))
      (SETQ B *BFTAG)
      (SETQ SPREC (MINPREC))
      (SETQ INVPB (QUOTIENT N0 2))
      (SETQ *PCMP
              (NOT
               (OR
                (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                (OR (NUMBERP (CDAR P))
                    (AND (EQCAR (CDAR P) '|:RD:|)
                         (NOT (ATOM (CDR (CDAR P)))))))))
      (COND (*NOSTURM (SETQ REQ NIL))
            (T
             (PROGN
              (COND
               ((NOT *PCMP) (SETQ REQ (COND ((EQUAL N0 1) 1) (T (RLRTNO P))))))
              (COND ((GREATERP REQ 0) (COND (*TRROOT (TRMSG4A REQ))))))))
      (SETQ RTN
              (DIFFERENCE
               (COND ((OR *PCMP *NOSTURM) N0) (T (QUOTIENT (PLUS N0 REQ) 2)))
               1))
      (SETQ P00 P)
      (SETQ 1RP0 1RP)
      (COND
       ((OR (EQUAL *NOINVERT "test") (AND (GREATERP N0 10) (NOT *NOINVERT)))
        (PROGN
         (SETQ PINV (INVPOLY P))
         (SETQ 1RPINV (INVPOLY 1RP))
         (COND
          ((OR (EQUAL *NOINVERT "test")
               (NOT (BFLEQP (MAXBND1 PINV) (MAXBND1 P))))
           (PROGN
            (COND ((OR *TRROOT *ROOTMSG) (LPRIM "inverting initially!")))
            (SETQ NMFG T)
            (GO INV)))))))
      (GO ST0)
     TLP
      (COND (INVTD* (GO ABRT))
            (T
             (PROGN
              (COND
               ((OR *TRROOT *ROOTMSG) (LPRIM "inverted polynomial tried")))
              (SETQ INVTD* T))))
     INV
      (SETQ K 0)
      (COND
       ((NOT PINV)
        (PROGN (SETQ PINV (INVPOLY P00)) (SETQ 1RPINV (INVPOLY 1RP0)))))
      (COND ((SETQ INVP (NOT INVP)) (PROGN (SETQ P PINV) (SETQ 1RP 1RPINV)))
            (T (PROGN (SETQ P P00) (SETQ 1RP 1RP0))))
      (COND
       ((AND (NOT NMFG) (NEQ INVPB 0))
        (PROGN
         (SETQ PREC (PLUS PREC INVPB))
         (PRECISION1 (DIFFERENCE (DIFFERENCE PREC 2) 2) T)
         (SETQ SPREC (MINPREC))
         (SETQ INVPB 0)
         (COND (*BFTAG (SETQ B T))))))
     STRT
      (COND ((AND PRQ (GREATERP (SETQ K (PLUS K 1)) RTN)) (GO ABRT)))
      (SETQ MB NIL)
      (COND
       ((AND (OR *ROOTMSG *ROOTMSG) (NOT NMFG))
        ((LAMBDA (CH)
           (PROGN
            (PROGN
             (PRIN2 "allroots restart at prec ")
             (PRIN2 (PLUS 2 (PRECISION 0)))
             NIL)
            (TERPRI)
            (WRS CH)))
         (WRS NIL))))
     ST0
      (SETQ N N0)
      (SETQ *GFP (SETQ QBF P))
      (SETQ C (SETQ CC (SETQ PF (SETQ PRQ NIL))))
      (COND
       ((NOT *NOSTURM)
        (PROGN
         (SETQ CPRQ (DIFFERENCE N REQ))
         (COND ((NOT *PCMP) (SETQ CPRQ (QUOTIENT CPRQ 2)))))))
      (SETQ RLN (SETQ CPN (SETQ |PRM#| 0)))
     ROOT
      (SETQ QF (SETQ MB (SETQ *MB (SETQ NMFG NIL))))
      (COND ((NOT *NOSTURM) (SETQ |ALLRL#| (EQUAL CPN CPRQ))))
      (COND (B (PROGN (SETQ Q QBF) (GO R0))))
      (SETQ Q
              (COND
               ((ERRORP (SETQ Q (ERRORSET* (LIST 'CFLOTEM (MKQUOTE QBF)) NIL)))
                (PROGN (SETQ B (SETQ *BFTAG T)) QBF))
               (T (SETQ QF (CAR Q)))))
     R0
      (SETQ |ACC#| AC)
      (COND ((NOT *NOSTURM) (SETQ *KEEPIMP (EQUAL (DIFFERENCE REQ RLN) 0))))
     R1
      (COND (*ROOTMSG (DSPLYRTNO (PLUS 1 (DIFFERENCE N0 N)))))
      (SETQ Y (GFROOTSET Q NIL B))
      (COND (2LOOP (PROGN (SETQ 2LOOP NIL) (GO TLP))))
      (SETQ R15N 0)
      (SETQ ACFL (SETQ |ACFL#| (SETQ |PFL#| NIL)))
      (COND
       ((EQUAL N N0)
        (PROGN
         (SETQ XO (SETQ *XOBF (GF2BF *XO)))
         (SETQ P0 QBF)
         (COND ((NOT B) (PROGN (SETQ XOF *XO) (SETQ PF Q)))))))
      (COND ((NOT Y) (PROGN (SETQ ER T) (GO FL))))
      (COND
       ((NOT (SETQ Y (CKACC QBF (COND (RED P0) (T 1RP)) (GF2BF *XN))))
        (PROGN (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A "ckacc"))) (GO INC0))))
      (COND
       ((PRINCREQ N
         (COND ((ATOM (CDR Y)) (ZEROP (CDR Y))) (T (EQUAL (CADR (CDR Y)) 0)))
         SPREC)
        (PROGN
         (SETQ PRQ T)
         (SETQ SW |PREC#|)
         (COND ((GREATERP N 2) (SETQ SW (PLUS SW N0))))
         (GO FL))))
     R15
      (COND ((GREATERP (SETQ R15N (PLUS R15N 1)) 3) (GO ABRT)))
      (COND
       ((OR INVP (AND (GREATERP N0 2) (GREATERP N0 N)))
        (PROGN
         (COND
          (*TRROOT
           (PROGN
            (PROGN (PRIN2 "q(") (PRIN2 N) (PRIN2 ") -> ") NIL)
            (PRINT_THE_NUMBER (GF2BF Y))
            (TERPRI))))
         (SETQ Y
                 (COND
                  ((OR (NOT PF)
                       ((LAMBDA (X)
                          (AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))))
                        (CAR *XN)))
                   (GFNEWTSET N0 P0 *XN XO B))
                  (T (GFNEWTSET N0 PF *XN XOF B))))
         (COND
          ((NOT Y)
           (PROGN
            (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A "gfnewtset")))
            (GO INC0)))))))
      (COND
       (ACFL
        (PROGN
         (SETQ |PFL#| T)
         (SETQ Y (CKACC QBF (COND (RED P0) (T 1RP)) (GF2BF *XN))))))
      (COND
       (*TRROOT
        (PROGN
         (PROGN (PRIN2 "p(") (PRIN2 N) (PRIN2 ") -> ") NIL)
         (PRINT_THE_NUMBER (GF2BF Y))
         (TERPRI))))
      (COND
       ((COND
         ((NOT (ATOM (CAR Y)))
          (AND (EQUAL (CADR (CAR Y)) 0) (EQUAL (CADR (CDR Y)) 0)))
         (T (EQUAL Y '(0.0 . 0.0))))
        (PROGN (SETQ INCMSG$ "illegal zero root") (GO INCR))))
      (COND
       ((NOT (SETQ Y (ACCUROOT *XN P0 XO)))
        (PROGN
         (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A "accuroot")))
         (GO INC0))))
      (SETQ RL
              (COND ((ATOM (CDR Y)) (ZEROP (CDR Y)))
                    (T (EQUAL (CADR (CDR Y)) 0))))
      (COND
       ((PRINCREQ N RL SPREC)
        (PROGN
         (SETQ PRQ T)
         (SETQ SW |PREC#|)
         (COND ((GREATERP N 3) (SETQ SW (PLUS SW 2))))
         (GO FL))))
     R2
      (COND
       ((NOT *NOSTURM)
        (COND
         (RL
          (PROGN
           (SETQ Y (CAR Y))
           (COND
            ((GREATERP (PLUS RLN 1) REQ)
             (PROGN (SETQ INCMSG$ "excess real root") (GO INCR))))))
         ((GREATERP (PLUS CPN 1) CPRQ)
          (PROGN (SETQ INCMSG$ "excess complex root") (GO INCR))))))
      (SETQ Z (GF2BF (COND (RL (CAR *XN)) (T *XN))))
      (COND
       ((AND (NOT RL) (NOT *PCMP))
        (PROGN
         (SETQ Y
                 (CONS (CAR Y)
                       (COND ((ATOM (CDR Y)) (ABS (CDR Y)))
                             (T (|ABS:| (CDR Y))))))
         (SETQ Z
                 (CONS (CAR Z)
                       (COND ((ATOM (CDR Z)) (ABS (CDR Z)))
                             (T (|ABS:| (CDR Z)))))))))
      (COND
       ((AND C (MEMBER Y C))
        (PROGN (SETQ INCMSG$ "equal roots found") (GO INCR))))
      (COND (RL (SETQ RLN (PLUS RLN 1))) (T (SETQ CPN (PLUS CPN 1))))
      (SETQ C (CONS Y C))
      (COND
       (INVP
        (PROGN
         (SETQ ZI (GF2BF Z))
         (COND
          (RED
           (SETQ ZI
                   (COND (RL (BFQUOTIENT BFONE* ZI))
                         (T (GFQUOTIENT (CONS BFONE* BFZ*) ZI)))))
          (T
           (GFRTRND
            (GFQUOTIENT (CONS BFONE* BFZ*)
                        (COND (RL (CONS ZI BFZ*)) (T ZI)))))))))
      (COND
       ((NOT (OR RL RED *PCMP))
        (SETQ |CPVAL#|
                (CONS (CAR |CPVAL#|)
                      (CONS (ABS (CADR |CPVAL#|)) (CDDR |CPVAL#|))))))
      (SETQ CC
              (CONS
               (CONS (COND (RED (COND (INVP ZI) (T Z))) (T (MKDN |CPVAL#|)))
                     |ACC#|)
               CC))
      (COND (*TRROOT (TERPRI)))
      (COND (|FROOT#| (GO RET)))
      (SETQ Z (GF2BF Z))
      (SETQ Q (BFLOATEM Q))
      (COND
       ((OR
         (AND (OR RL *PCMP) (GREATERP (SETQ N (DIFFERENCE N 1)) 0)
              (SETQ Q (CDR (COND (RL (DEFLATE1 Q Z)) (T (DEFLATE1C Q Z))))))
         (AND (GREATERP (SETQ N (DIFFERENCE N 2)) 0) (SETQ Q (DEFLATE2 Q Z))))
        (PROGN (SETQ QBF (BFRNDEM Q)) (GO ROOT))))
     RET
      (PRECISION1 (DIFFERENCE (MAX PREC (PLUS (SETQ |ACC#| AC) 2)) 2) T)
      (SETQ *BFTAG B)
      (RETURN (CEXPAND CC))
     INCR
      (LPRIM INCMSG$)
      (SETQ POLNIX$ Q)
      (COND (MB (GO TLP))
            (*ZX1
             (PROGN
              (LPRIM "offset iteration attempted")
              (SETQ *MB (SETQ MB T))
              (GO R1))))
     INC0
      (COND ((LEQ (SETQ NPI (PLUS NPI 1)) 3) (GO INC1)))
     ABRT
      (LPRIM (LIST "root finding aborted. Deflate degree = " N))
      (LPRIM (LIST "poly = " Q))
      (TERPRI)
      (COND ((GREATERP N0 N) (SETQ POLREM$ Q)))
      (GO RET)
     INC1
      (SETQ INC (MAX N0 (QUOTIENT SPREC 2)))
      (PRECISION1
       (DIFFERENCE
        (SETQ SPREC (MAX (PLUS SPREC INC) (PLUS 2 (TIMES 2 |ACC#|)))) 2)
       T)
      (TRMSG9 SPREC)
      (COND (B (GO STRT)))
     FL
      (SETQ P P0)
      (SETQ XO (SETQ *XO (GF2BF XO)))
      (SETQ B (SETQ *BFTAG T))
      (SETQ 1RP (BFLOATEM 1RP))
      (COND (ER (PROGN (SETQ ER NIL) (SETQ Q QBF) (GO R1))))
      (SETQ ACFL T)
      (COND
       (SW
        (PROGN
         (PRECISION1 (DIFFERENCE (SETQ SPREC SW) 2) T)
         (SETQ SW NIL)
         (SETQ Q P)
         (COND ((EQUAL N N0) (PROGN (SETQ Y (GF2BF Y)) (GO R15)))))))
      (GO STRT))) 
(PUT 'PRINCREQ 'NUMBER-OF-ARGS 3) 
(PUT 'PRINCREQ 'DEFINED-ON-LINE '466) 
(PUT 'PRINCREQ 'DEFINED-IN-FILE 'ROOTS/ALLROOT.RED) 
(PUT 'PRINCREQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PRINCREQ (N RL SPREC)
    (AND (OR (GREATERP N 2) (AND (OR RL *PCMP) (GREATERP N 1)))
         (GREATERP (MIN |PREC#| (TIMES 2 (PLUS |ACCM#| 1))) SPREC))) 
(ENDMODULE) 