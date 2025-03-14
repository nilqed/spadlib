(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BFAUXIL)) 
(EXPORTS
 (LIST '!SHBINFLP 'BF2FLR 'BFDIFFER 'BFDIVIDE 'BFINVERSE 'BFLESSP 'BFMINUS
       'BFSQRT 'CFLOT '|CONV:BF2I| 'DIFBF 'EXPTBF 'FL2INT 'GF2BF 'GF2FL
       'GFDIFFER 'GFDOT 'GFMINUS 'GFPLUS 'GFQUOTIENT 'GFSQRT 'GFTIMES 'GRPBF
       'ICBRT 'ILOG2 'INVBF 'IROOTN 'ISQRT 'NORMBF 'PLUBF 'R2BF 'R2FL 'REALRAT)) 
(IMPORTS
 (LIST '|ABS:| 'ASHIFT 'BFLERRMSG 'BFLOAT 'BFMINUSP 'BFNZP '|BFP:| '|BFZEROP:|
       'BFZP '|CONV:MT| '|CUT:EP| '|DECPREC:| '|DIFFERENCE:| 'DIVBF '|DIVIDE:|
       '|EP:| 'EQCAR 'ERROR1 'ERRORP 'ERRORSET* 'EVENP 'FL2BF 'GCDN 'GEQ 'GFIM
       'GFRL 'GFZEROP '|GREATERP:| 'HYPOT '|I2BF:| 'LEQ 'LSHIFT '|MAKE:IBF|
       '|MINUS:| '|MINUSP:| '|MSD:| '|MT:| '|ORDER:| '|PLUS:| '|PRECI:|
       '|READ:NUM| 'RNDPWR '|ROUND:MT| 'SGN 'SQRT 'TERRLST 'TIMBF '|TIMES:|
       'TYPERR)) 
(FLUID '(|:PREC:| |:BPREC:|)) 
(GLOBAL '(BFONE* BFHALF* BFZ*)) 
(GLOBAL '(!NFPD !NBFPD !SHBINFL VV! !FLBINT)) 
(GLOBAL '(!MINFLBF !MAXFLBF)) 
(PUT 'NORMBF 'NUMBER-OF-ARGS 1) 
(PUT 'NORMBF 'DEFINED-ON-LINE '60) 
(PUT 'NORMBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'NORMBF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMBF (X)
    (PROG (MT S EP)
      (SETQ EP 0)
      (COND ((EQUAL (SETQ MT (CADR X)) 0) (GO RET)))
      (COND ((LESSP MT 0) (PROGN (SETQ MT (MINUS MT)) (SETQ S T))))
      (SETQ EP (CDDR X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (REMAINDER MT 1073741824) 0)) (RETURN NIL)))
        (PROGN (SETQ MT (ASHIFT MT (MINUS 30))) (SETQ EP (PLUS EP 30)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL (REMAINDER MT 256) 0)) (RETURN NIL)))
        (PROGN (SETQ MT (ASHIFT MT (MINUS 8))) (SETQ EP (PLUS EP 8)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NOT (EVENP MT)))) (RETURN NIL)))
        (PROGN (SETQ MT (ASHIFT MT (MINUS 1))) (SETQ EP (PLUS EP 1)))
        (GO WHILELABEL))
      (COND (S (SETQ MT (MINUS MT))))
     RET
      (RETURN (CONS '|:RD:| (CONS MT EP))))) 
(PUT 'BFDIVIDE 'NUMBER-OF-ARGS 2) 
(PUT 'BFDIVIDE 'DEFINED-ON-LINE '79) 
(PUT 'BFDIVIDE 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFDIVIDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFDIVIDE (U V)
    (COND ((ATOM U) (QUOTIENT U V)) (T (NORMBF (|DIVIDE:| U V |:BPREC:|))))) 
(PUT 'BFTIMES 'NUMBER-OF-ARGS 2) 
(PUT 'BFTIMES 'DEFINED-ON-LINE '84) 
(PUT 'BFTIMES 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFTIMES (U V)
    (COND ((ATOM U) (TIMES U V))
          (T (NORMBF (|ROUND:MT| (|TIMES:| U V) |:BPREC:|))))) 
(PUT 'PLUBF 'NUMBER-OF-ARGS 2) 
(PUT 'PLUBF 'DEFINED-ON-LINE '86) 
(PUT 'PLUBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'PLUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PLUBF (A B)
    (NORMBF
     (|ROUND:MT|
      (PROG (MA MB EA EB D LD P)
        (COND ((EQUAL (SETQ MA (CADR A)) 0) (RETURN B)))
        (COND ((EQUAL (SETQ MB (CADR B)) 0) (RETURN A)))
        (COND
         ((EQUAL (SETQ D (DIFFERENCE (SETQ EA (CDDR A)) (SETQ EB (CDDR B)))) 0)
          (RETURN (CONS '|:RD:| (CONS (PLUS MA MB) EA)))))
        (SETQ LD (PLUS D (DIFFERENCE (|MSD:| (ABS MA)) (|MSD:| (ABS MB)))))
        (SETQ P (PLUS |:BPREC:| 1))
        (COND ((GREATERP LD P) (RETURN A)))
        (COND ((LESSP LD (MINUS P)) (RETURN B)))
        (COND
         ((GREATERP D 0)
          (RETURN (CONS '|:RD:| (CONS (PLUS (ASHIFT MA D) MB) EB))))
         (T
          (RETURN (CONS '|:RD:| (CONS (PLUS MA (ASHIFT MB (MINUS D))) EA))))))
      |:BPREC:|))) 
(PUT 'BFPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'BFPLUS 'DEFINED-ON-LINE '102) 
(PUT 'BFPLUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFPLUS (U V) (COND ((ATOM U) (PLUS U V)) (T (PLUBF U V)))) 
(PUT 'DIFBF 'NUMBER-OF-ARGS 2) 
(PUT 'DIFBF 'DEFINED-ON-LINE '104) 
(PUT 'DIFBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'DIFBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIFBF (A B)
    (NORMBF
     (|ROUND:MT|
      (PROG (MA MB EA EB D LD P)
        (COND ((EQUAL (SETQ MA (CADR A)) 0) (RETURN (|MINUS:| B))))
        (COND ((EQUAL (SETQ MB (CADR B)) 0) (RETURN A)))
        (COND
         ((EQUAL (SETQ D (DIFFERENCE (SETQ EA (CDDR A)) (SETQ EB (CDDR B)))) 0)
          (RETURN (CONS '|:RD:| (CONS (DIFFERENCE MA MB) EA)))))
        (SETQ LD (PLUS D (DIFFERENCE (|MSD:| (ABS MA)) (|MSD:| (ABS MB)))))
        (SETQ P (PLUS |:BPREC:| 1))
        (COND ((GREATERP LD P) (RETURN A)))
        (COND ((LESSP LD (MINUS P)) (RETURN (|MINUS:| B))))
        (COND
         ((GREATERP D 0)
          (RETURN (CONS '|:RD:| (CONS (DIFFERENCE (ASHIFT MA D) MB) EB))))
         (T
          (RETURN
           (CONS '|:RD:| (CONS (DIFFERENCE MA (ASHIFT MB (MINUS D))) EA))))))
      |:BPREC:|))) 
(PUT 'BFDIFFER 'NUMBER-OF-ARGS 2) 
(PUT 'BFDIFFER 'DEFINED-ON-LINE '120) 
(PUT 'BFDIFFER 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFDIFFER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFDIFFER (U V) (COND ((ATOM U) (DIFFERENCE U V)) (T (DIFBF U V)))) 
(PUT 'INVBF 'NUMBER-OF-ARGS 1) 
(PUT 'INVBF 'DEFINED-ON-LINE '122) 
(PUT 'INVBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'INVBF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVBF (U) (NORMBF (|DIVIDE:| BFONE* U |:BPREC:|))) 
(PUT 'BFINVERSE 'NUMBER-OF-ARGS 1) 
(PUT 'BFINVERSE 'DEFINED-ON-LINE '124) 
(PUT 'BFINVERSE 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFINVERSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFINVERSE (U) (COND ((ATOM U) (QUOTIENT 1.0 U)) (T (INVBF U)))) 
(PUT 'BFMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'BFMINUS 'DEFINED-ON-LINE '126) 
(PUT 'BFMINUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFMINUS (U) (COND ((ATOM U) (MINUS U)) (T (|MINUS:| U)))) 
(PUT 'BFLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'BFLESSP 'DEFINED-ON-LINE '128) 
(PUT 'BFLESSP 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFLESSP (A B) (COND ((ATOM A) (LESSP A B)) (T (GRPBF B A)))) 
(PUT 'GRPBF 'NUMBER-OF-ARGS 2) 
(PUT 'GRPBF 'DEFINED-ON-LINE '130) 
(PUT 'GRPBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GRPBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GRPBF (A B)
    ((LAMBDA (MA MB)
       (PROGN
        (COND ((EQUAL MA 0) (LESSP MB 0)) ((EQUAL MB 0) (GREATERP MA 0))
              ((AND (GREATERP MA 0) (LESSP MB 0)) T)
              ((AND (LESSP MA 0) (GREATERP MB 0)) NIL)
              (T
               ((LAMBDA (DO DE)
                  (COND ((GREATERP DO 0) (GREATERP MA 0))
                        ((LESSP DO 0) (LESSP MA 0))
                        ((EQUAL DE 0) (GREATERP MA MB))
                        ((GREATERP DE 0) (GREATERP (ASHIFT MA DE) MB))
                        (T (GREATERP MA (ASHIFT MB (MINUS DE))))))
                (DIFFERENCE (|ORDER:| A) (|ORDER:| B))
                (DIFFERENCE (CDDR A) (CDDR B)))))))
     (CADR A) (CADR B))) 
(PUT '!SHBINFLP 'NUMBER-OF-ARGS 0) 
(PUT '!SHBINFLP 'DEFINED-ON-LINE '157) 
(PUT '!SHBINFLP 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT '!SHBINFLP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE !SHBINFLP NIL
    (PROG (N)
      (SETQ N 0)
      (SETQ VV! 9.0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LESSP N 300) (NOT (ERRORP (ERRORSET* '(VV!*1E10) NIL)))))
          (RETURN NIL)))
        (SETQ N (PLUS N 10))
        (GO WHILELABEL))
      (RETURN (LESSP N 300)))) 
(PUT 'VV!*1E10 'NUMBER-OF-ARGS 0) 
(PUT 'VV!*1E10 'DEFINED-ON-LINE '163) 
(PUT 'VV!*1E10 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'VV!*1E10 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE VV!*1E10 NIL (SETQ VV! (TIMES VV! 1.0e10))) 
(SETQ !SHBINFL (!SHBINFLP)) 
(PUT 'BFSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'BFSQRT 'DEFINED-ON-LINE '167) 
(PUT 'BFSQRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BFSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFSQRT (X)
    (COND ((|MINUSP:| X) (TERRLST X 'BFSQRT))
          (T
           (PROG (NX DX DC K7 NF)
             (COND ((|BFZEROP:| X) (RETURN BFZ*)))
             (SETQ K7 (PLUS |:BPREC:| 7))
             (SETQ DC
                     (CONS '|:RD:|
                           (CONS 1
                                 (PLUS (MINUS K7)
                                       (QUOTIENT (PLUS (|ORDER:| X) 10) 2)))))
             (SETQ NX
                     (COND
                      ((NOT (NOT (EVENP (CDDR (SETQ NX (|CONV:MT| X 2))))))
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 2 (TIMES 3 (CADR NX))) 5)
                                   (QUOTIENT (CDDR NX) 2))))
                      (T
                       (CONS '|:RD:|
                             (CONS (QUOTIENT (PLUS 9 (TIMES 5 (CADR NX))) 10)
                                   (QUOTIENT (DIFFERENCE (CDDR NX) 1) 2))))))
             (SETQ NF 1)
            LOOP
             (COND ((GREATERP (SETQ NF (TIMES 2 NF)) K7) (SETQ NF K7)))
             (SETQ DX (|TIMES:| BFHALF* (|PLUS:| (|DIVIDE:| X NX NF) NX)))
             (COND
              ((AND (GEQ NF K7)
                    (NOT (|GREATERP:| (|ABS:| (|DIFFERENCE:| DX NX)) DC)))
               (RETURN (NORMBF (|ROUND:MT| NX |:BPREC:|)))))
             (SETQ NX DX)
             (GO LOOP))))) 
(PUT 'REALRAT 'NUMBER-OF-ARGS 1) 
(PUT 'REALRAT 'DEFINED-ON-LINE '184) 
(PUT 'REALRAT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'REALRAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REALRAT (X)
    (PROG (D G)
      (COND ((AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))) (GO BF)))
      (COND
       ((EQCAR X 'QUOTIENT)
        (COND
         ((AND (FIXP (CADR X)) (FIXP (CADDR X)))
          (PROGN
           (SETQ X
                   (COND ((LESSP (SETQ D (CADDR X)) 0) (MINUS (CADR X)))
                         (T (CADR X))))
           (SETQ D (ABS D))
           (GO RET)))
         (T (SETQ X (QUOTIENT (CADR X) (CADDR X)))))))
      (COND ((ZEROP X) (RETURN (CONS 0 1))))
      (COND ((NOT (FLOATP X)) (RETURN (CONS X 1))))
      (SETQ X
              (COND ((FLOATP X) (FL2BF X))
                    (T
                     (NORMBF
                      (COND ((NOT (ATOM X)) X)
                            ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                            (T (|READ:NUM| X)))))))
     BF
      (SETQ D (CDDR (SETQ X (NORMBF X))))
      (SETQ X (CADR X))
      (COND ((EQUAL X 0) (RETURN (CONS 0 1))))
      (COND ((LESSP D 0) (SETQ D (ASHIFT 1 (MINUS D))))
            (T (PROGN (SETQ X (ASHIFT X D)) (SETQ D 1))))
     RET
      (SETQ G (GCDN (ABS X) D))
      (RETURN (CONS (QUOTIENT X G) (QUOTIENT D G))))) 
(REMFLAG '(FL2INT) 'LOSE) 
(PUT 'FL2INT 'NUMBER-OF-ARGS 1) 
(PUT 'FL2INT 'DEFINED-ON-LINE '203) 
(PUT 'FL2INT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'FL2INT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FL2INT (X)
    (PROGN
     (SETQ X (FL2BF X))
     ((LAMBDA (M D) (COND ((EQUAL D 0) M) (T (ASHIFT M D)))) (CADR X)
      (CDDR X)))) 
(FLAG '(FL2INT) 'LOSE) 
(PUT 'CFLOT 'NUMBER-OF-ARGS 1) 
(PUT 'CFLOT 'DEFINED-ON-LINE '210) 
(PUT 'CFLOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'CFLOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFLOT (X) (COND ((FLOATP X) X) ((ATOM X) (FLOAT X)) (T (BF2FLR X)))) 
(PUT '|CONV:BF2I| 'NUMBER-OF-ARGS 1) 
(PUT '|CONV:BF2I| 'DEFINED-ON-LINE '213) 
(PUT '|CONV:BF2I| 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT '|CONV:BF2I| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |CONV:BF2I| (NMBR) (ASHIFT (CADR NMBR) (CDDR NMBR))) 
(PUT 'BF2FLR 'NUMBER-OF-ARGS 1) 
(PUT 'BF2FLR 'DEFINED-ON-LINE '224) 
(PUT 'BF2FLR 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'BF2FLR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BF2FLR (U)
    (PROG (EP M Y)
      (COND ((|BFZEROP:| U) (RETURN 0.0)))
      (SETQ EP (CDDR (SETQ U (|ROUND:MT| U !NBFPD))))
      (COND
       ((OR (GRPBF !MINFLBF (SETQ Y (|ABS:| U))) (GRPBF Y !MAXFLBF)) (ERROR1)))
      (COND ((LESSP EP 0) (PROGN (SETQ EP (PLUS EP !NBFPD)) (SETQ M T))))
      (SETQ EP (EXPT 2.0 EP))
      (COND ((EQUAL EP 0.0) (ERROR1)))
      (RETURN
       (COND ((NOT M) (TIMES EP (CADR U)))
             (T (TIMES EP (QUOTIENT (CADR U) !FLBINT))))))) 
(PUT 'GF2FL 'NUMBER-OF-ARGS 1) 
(PUT 'GF2FL 'DEFINED-ON-LINE '240) 
(PUT 'GF2FL 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GF2FL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GF2FL (A)
    (COND ((ATOM A) A)
          ((AND (EQCAR A '|:RD:|) (NOT (ATOM (CDR A)))) (BF2FLR A))
          (T (CONS (GF2FL (CAR A)) (GF2FL (CDR A)))))) 
(PUT 'GF2BF 'NUMBER-OF-ARGS 1) 
(PUT 'GF2BF 'DEFINED-ON-LINE '244) 
(PUT 'GF2BF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GF2BF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GF2BF (A)
    (COND
     (A
      (COND
       ((ATOM A)
        (COND ((FLOATP A) (FL2BF A))
              (T
               (NORMBF
                (COND ((NOT (ATOM A)) A) ((FIXP A) (CONS '|:RD:| (CONS A 0)))
                      (T (|READ:NUM| A)))))))
       ((AND (EQCAR A '|:RD:|) (NOT (ATOM (CDR A)))) A)
       (T (CONS (GF2BF (CAR A)) (GF2BF (CDR A)))))))) 
(PUT 'R2BF 'NUMBER-OF-ARGS 1) 
(PUT 'R2BF 'DEFINED-ON-LINE '248) 
(PUT 'R2BF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'R2BF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2BF (U)
    (COND
     ((ATOM U)
      (COND ((FLOATP U) (FL2BF U))
            (T
             (NORMBF
              (COND ((NOT (ATOM U)) U) ((FIXP U) (CONS '|:RD:| (CONS U 0)))
                    (T (|READ:NUM| U)))))))
     ((AND (EQCAR U '|:RD:|) (NOT (ATOM (CDR U)))) U)
     ((NUMBERP (CAR U))
      (NORMBF
       (|DIVIDE:| (CONS '|:RD:| (CONS (CAR U) 0))
                  (CONS '|:RD:| (CONS (CDR U) 0)) |:BPREC:|)))
     ((EQCAR U 'QUOTIENT)
      (NORMBF
       (|DIVIDE:| (CONS '|:RD:| (CONS (CADR U) 0))
                  (CONS '|:RD:| (CONS (CADDR U) 0)) |:BPREC:|)))
     ((EQCAR U '|:RN:|) (R2BF (CDR U))) (T (R2BF (CADR U))))) 
(PUT 'R2FL 'NUMBER-OF-ARGS 1) 
(PUT 'R2FL 'DEFINED-ON-LINE '258) 
(PUT 'R2FL 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'R2FL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2FL (U)
    (COND ((EQUAL U 0) 0.0) ((ATOM U) (FLOAT U))
          ((NUMBERP (CAR U)) (QUOTIENT (FLOAT (CAR U)) (CDR U)))
          ((EQCAR U 'QUOTIENT) (QUOTIENT (FLOAT (CADR U)) (CADDR U)))
          ((AND (EQCAR U '|:RD:|) (NOT (ATOM (CDR U)))) (BF2FLR U))
          ((EQCAR U '|:RN:|) (R2FL (CDR U))) (T (R2FL (CADR U))))) 
(PUT 'GFPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'GFPLUS 'DEFINED-ON-LINE '268) 
(PUT 'GFPLUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFPLUS (U V) (COND ((ATOM (CAR U)) (GFFPLUS U V)) (T (GBFPLUS U V)))) 
(PUT 'GFFPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'GFFPLUS 'DEFINED-ON-LINE '271) 
(PUT 'GFFPLUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFPLUS (U V) (CONS (PLUS (CAR U) (CAR V)) (PLUS (CDR U) (CDR V)))) 
(PUT 'GBFPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'GBFPLUS 'DEFINED-ON-LINE '273) 
(PUT 'GBFPLUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFPLUS (U V) (CONS (PLUBF (CAR U) (CAR V)) (PLUBF (CDR U) (CDR V)))) 
(PUT 'GFDIFFER 'NUMBER-OF-ARGS 2) 
(PUT 'GFDIFFER 'DEFINED-ON-LINE '276) 
(PUT 'GFDIFFER 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFDIFFER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFDIFFER (U V) (COND ((ATOM (CAR U)) (GFFDIFF U V)) (T (GBFDIFF U V)))) 
(PUT 'GFFDIFF 'NUMBER-OF-ARGS 2) 
(PUT 'GFFDIFF 'DEFINED-ON-LINE '279) 
(PUT 'GFFDIFF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFDIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFDIFF (U V)
    (CONS (DIFFERENCE (CAR U) (CAR V)) (DIFFERENCE (CDR U) (CDR V)))) 
(PUT 'GBFDIFF 'NUMBER-OF-ARGS 2) 
(PUT 'GBFDIFF 'DEFINED-ON-LINE '281) 
(PUT 'GBFDIFF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFDIFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFDIFF (U V) (CONS (DIFBF (CAR U) (CAR V)) (DIFBF (CDR U) (CDR V)))) 
(PUT 'GFTIMES 'NUMBER-OF-ARGS 2) 
(PUT 'GFTIMES 'DEFINED-ON-LINE '284) 
(PUT 'GFTIMES 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFTIMES (U V) (COND ((ATOM (CAR U)) (GFFTIMES U V)) (T (GBFTIMES U V)))) 
(PUT 'GFFTIMES 'NUMBER-OF-ARGS 2) 
(PUT 'GFFTIMES 'DEFINED-ON-LINE '287) 
(PUT 'GFFTIMES 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFTIMES (U V)
    (PROG (RU IU RV IV)
      (SETQ RU (CAR U))
      (SETQ IU (CDR U))
      (SETQ RV (CAR V))
      (SETQ IV (CDR V))
      (RETURN
       (CONS (DIFFERENCE (TIMES RU RV) (TIMES IU IV))
             (PLUS (TIMES RU IV) (TIMES IU RV)))))) 
(PUT 'GBFTIMES 'NUMBER-OF-ARGS 2) 
(PUT 'GBFTIMES 'DEFINED-ON-LINE '292) 
(PUT 'GBFTIMES 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFTIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFTIMES (U V)
    (PROG (RU IU RV IV)
      (SETQ RU (CAR U))
      (SETQ IU (CDR U))
      (SETQ RV (CAR V))
      (SETQ IV (CDR V))
      (RETURN
       (CONS
        (DIFBF (NORMBF (|ROUND:MT| (|TIMES:| RU RV) |:BPREC:|))
               (NORMBF (|ROUND:MT| (|TIMES:| IU IV) |:BPREC:|)))
        (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| RU IV) |:BPREC:|))
               (NORMBF (|ROUND:MT| (|TIMES:| IU RV) |:BPREC:|))))))) 
(PUT 'GFQUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'GFQUOTIENT 'DEFINED-ON-LINE '298) 
(PUT 'GFQUOTIENT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFQUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFQUOTIENT (U V) (COND ((ATOM (CAR U)) (GFFQUOT U V)) (T (GBFQUOT U V)))) 
(PUT 'GFFQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'GFFQUOT 'DEFINED-ON-LINE '301) 
(PUT 'GFFQUOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFQUOT (U V)
    (PROG (RU IU RV IV D)
      (SETQ RU (CAR U))
      (SETQ IU (CDR U))
      (SETQ RV (CAR V))
      (SETQ IV (CDR V))
      (SETQ D (PLUS (TIMES RV RV) (TIMES IV IV)))
      (RETURN
       (CONS (QUOTIENT (PLUS (TIMES RU RV) (TIMES IU IV)) D)
             (QUOTIENT (DIFFERENCE (TIMES IU RV) (TIMES RU IV)) D))))) 
(PUT 'GBFQUOT 'NUMBER-OF-ARGS 2) 
(PUT 'GBFQUOT 'DEFINED-ON-LINE '307) 
(PUT 'GBFQUOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFQUOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFQUOT (U V)
    (PROG (RU IU RV IV D)
      (SETQ RU (CAR U))
      (SETQ IU (CDR U))
      (SETQ RV (CAR V))
      (SETQ IV (CDR V))
      (SETQ D
              (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| RV RV) |:BPREC:|))
                     (NORMBF (|ROUND:MT| (|TIMES:| IV IV) |:BPREC:|))))
      (RETURN
       (CONS
        (NORMBF
         (|DIVIDE:|
          (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| RU RV) |:BPREC:|))
                 (NORMBF (|ROUND:MT| (|TIMES:| IU IV) |:BPREC:|)))
          D |:BPREC:|))
        (NORMBF
         (|DIVIDE:|
          (DIFBF (NORMBF (|ROUND:MT| (|TIMES:| IU RV) |:BPREC:|))
                 (NORMBF (|ROUND:MT| (|TIMES:| RU IV) |:BPREC:|)))
          D |:BPREC:|)))))) 
(PUT 'GFMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'GFMINUS 'DEFINED-ON-LINE '314) 
(PUT 'GFMINUS 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFMINUS (U) (CONS (BFMINUS (CAR U)) (BFMINUS (CDR U)))) 
(PUT 'GFROTATE 'NUMBER-OF-ARGS 1) 
(PUT 'GFROTATE 'DEFINED-ON-LINE '316) 
(PUT 'GFROTATE 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFROTATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFROTATE (U) (CONS (BFMINUS (CDR U)) (CAR U))) 
(PUT 'GFDOT 'NUMBER-OF-ARGS 2) 
(PUT 'GFDOT 'DEFINED-ON-LINE '326) 
(PUT 'GFDOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFDOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFDOT (U V) (COND ((ATOM (CAR U)) (GFFDOT U V)) (T (GBFDOT U V)))) 
(PUT 'GFFDOT 'NUMBER-OF-ARGS 2) 
(PUT 'GFFDOT 'DEFINED-ON-LINE '329) 
(PUT 'GFFDOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFDOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFDOT (U V) (PLUS (TIMES (CAR U) (CAR V)) (TIMES (CDR U) (CDR V)))) 
(PUT 'GBFDOT 'NUMBER-OF-ARGS 2) 
(PUT 'GBFDOT 'DEFINED-ON-LINE '331) 
(PUT 'GBFDOT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFDOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFDOT (U V)
    (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| (CAR U) (CAR V)) |:BPREC:|))
           (NORMBF (|ROUND:MT| (|TIMES:| (CDR U) (CDR V)) |:BPREC:|)))) 
(PUT 'GFRSQ 'NUMBER-OF-ARGS 1) 
(PUT 'GFRSQ 'DEFINED-ON-LINE '334) 
(PUT 'GFRSQ 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFRSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFRSQ (U) (GFDOT U U)) 
(PUT 'GFFRSQ 'NUMBER-OF-ARGS 1) 
(PUT 'GFFRSQ 'DEFINED-ON-LINE '336) 
(PUT 'GFFRSQ 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFRSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFFRSQ (U) (PLUS (TIMES (CAR U) (CAR U)) (TIMES (CDR U) (CDR U)))) 
(PUT 'GBFRSQ 'NUMBER-OF-ARGS 1) 
(PUT 'GBFRSQ 'DEFINED-ON-LINE '338) 
(PUT 'GBFRSQ 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFRSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBFRSQ (U)
    (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| (CAR U) (CAR U)) |:BPREC:|))
           (NORMBF (|ROUND:MT| (|TIMES:| (CDR U) (CDR U)) |:BPREC:|)))) 
(PUT 'GFFMULT 'NUMBER-OF-ARGS 2) 
(PUT 'GFFMULT 'DEFINED-ON-LINE '341) 
(PUT 'GFFMULT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFFMULT (R U) (CONS (TIMES R (CAR U)) (TIMES R (CDR U)))) 
(PUT 'GFFSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'GFFSQRT 'DEFINED-ON-LINE '343) 
(PUT 'GFFSQRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFFSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFFSQRT (X)
    (PROG (X0 NX XD XD0 RL IM)
      (SETQ RL (CAR X))
      (SETQ IM (CDR X))
      (SETQ RL (SQRT (PLUS (QUOTIENT (HYPOT RL IM) 2) (QUOTIENT RL 2))))
      (SETQ IM (QUOTIENT IM (TIMES 2 RL)))
      (SETQ NX (CONS RL IM))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ X0 NX)
         (SETQ NX (GFFMULT 0.5 (GFFPLUS X0 (GFFQUOT X X0))))
         (SETQ XD0 XD)
         (SETQ XD (GFFRSQ (GFFDIFF X (GFFTIMES NX NX)))))
        (COND
         ((NOT (AND XD0 (LEQ (DIFFERENCE XD0 XD) 0.0))) (GO REPEATLABEL))))
      (RETURN X0))) 
(PUT 'GBFMULT 'NUMBER-OF-ARGS 2) 
(PUT 'GBFMULT 'DEFINED-ON-LINE '352) 
(PUT 'GBFMULT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GBFMULT (R U)
    (PROGN
     (SETQ R
             (COND ((FLOATP R) (FL2BF R))
                   (T
                    (NORMBF
                     (COND ((NOT (ATOM R)) R)
                           ((FIXP R) (CONS '|:RD:| (CONS R 0)))
                           (T (|READ:NUM| R)))))))
     (CONS (NORMBF (|ROUND:MT| (|TIMES:| R (CAR U)) |:BPREC:|))
           (NORMBF (|ROUND:MT| (|TIMES:| R (CDR U)) |:BPREC:|))))) 
(PUT 'GBFSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'GBFSQRT 'DEFINED-ON-LINE '355) 
(PUT 'GBFSQRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GBFSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBFSQRT (X)
    (PROG (X0 NX XD XD0 RL)
      (SETQ NX
              (PROGN
               (SETQ RL
                       (BFSQRT
                        (NORMBF
                         (|ROUND:MT|
                          (|TIMES:| BFHALF* (PLUBF (BFSQRT (GFRSQ X)) (CAR X)))
                          |:BPREC:|))))
               (CONS RL
                     (NORMBF
                      (|ROUND:MT|
                       (|TIMES:| BFHALF*
                                 (NORMBF (|DIVIDE:| (CDR X) RL |:BPREC:|)))
                       |:BPREC:|)))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ X0 NX)
         (SETQ NX (GBFMULT BFHALF* (GBFPLUS X0 (GBFQUOT X X0))))
         (SETQ XD0 XD)
         (SETQ XD (GBFRSQ (GBFDIFF X (GBFTIMES NX NX)))))
        (COND
         ((NOT (AND XD0 (LEQ (CADR (DIFBF XD0 XD)) 0))) (GO REPEATLABEL))))
      (RETURN X0))) 
(DE RL2GFC (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*)))) 
(PUT 'RL2GFC 'NUMBER-OF-ARGS 1) 
(PUT 'RL2GFC 'DEFINED-ON-LINE '366) 
(PUT 'RL2GFC 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'RL2GFC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'RL2GFC 'INLINE '(LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))) 
(PUT 'GFSQRT 'NUMBER-OF-ARGS 1) 
(PUT 'GFSQRT 'DEFINED-ON-LINE '369) 
(PUT 'GFSQRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'GFSQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFSQRT (X)
    (PROG (XN NEG NEGI)
      (COND
       ((COND
         ((NOT (ATOM (CAR X)))
          (AND (EQUAL (CADR (CAR X)) 0) (EQUAL (CADR (CDR X)) 0)))
         (T (EQUAL X '(0.0 . 0.0))))
        (RETURN X)))
      (COND
       ((COND ((ATOM (CAR X)) (MINUSP (CAR X))) (T (|MINUSP:| (CAR X))))
        (PROGN
         (SETQ X (GFMINUS X))
         (SETQ NEG T)
         (COND ((SPECIAL_BFMINUSP (CDR X)) NIL) (T (SETQ NEGI T))))))
      (COND
       ((COND ((ATOM (CDR X)) (ZEROP (CDR X))) (T (EQUAL (CADR (CDR X)) 0)))
        (PROGN
         (SETQ X (CAR X))
         (SETQ XN
                 ((LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))
                  (COND ((ATOM X) (SQRT X)) (T (BFSQRT X)))))
         (GO RET))))
      (SETQ XN (COND ((ATOM (CAR X)) (GFFSQRT X)) (T (GBFSQRT X))))
      (COND (NEGI (SETQ XN (GFMINUS XN))))
     RET
      (RETURN (COND (NEG (GFROTATE XN)) (T XN))))) 
(PUT 'SGN 'NUMBER-OF-ARGS 1) 
(PUT 'SGN 'DEFINED-ON-LINE '384) 
(PUT 'SGN 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'SGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SGN (X) (COND ((GREATERP X 0) 1) ((LESSP X 0) (MINUS 1)) (T 0))) 
(PUT 'EXPTBF 'NUMBER-OF-ARGS 3) 
(PUT 'EXPTBF 'DEFINED-ON-LINE '386) 
(PUT 'EXPTBF 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'EXPTBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPTBF (X N A)
    (PROG ()
     LP
      (COND
       ((NOT (EVENP N))
        (SETQ A (NORMBF (|ROUND:MT| (|TIMES:| A X) |:BPREC:|)))))
      (SETQ N (ASHIFT N (MINUS 1)))
      (COND ((EQUAL N 0) (RETURN A)))
      (SETQ X (NORMBF (|ROUND:MT| (|TIMES:| X X) |:BPREC:|)))
      (GO LP))) 
(PUT 'ICBRT 'NUMBER-OF-ARGS 1) 
(PUT 'ICBRT 'DEFINED-ON-LINE '395) 
(PUT 'ICBRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'ICBRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ICBRT (X) (IROOTN (FIX2 X) 3)) 
(PUT 'FIX2 'NUMBER-OF-ARGS 1) 
(PUT 'FIX2 'DEFINED-ON-LINE '400) 
(PUT 'FIX2 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'FIX2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIX2 (X) (COND ((FIXP X) X) (T (FL2INT X)))) 
(PUT 'ILOG2 'NUMBER-OF-ARGS 1) 
(PUT 'ILOG2 'DEFINED-ON-LINE '402) 
(PUT 'ILOG2 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'ILOG2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ILOG2 (N)
    (PROG (ANS POWERS-OF-2 PWR)
      (COND ((LEQ N 0) (TERRLST N 'ILOG2)))
      (SETQ PWR 2)
      (SETQ POWERS-OF-2 (CONS PWR NIL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N PWR)) (RETURN NIL)))
        (PROGN
         (SETQ POWERS-OF-2 (CONS PWR POWERS-OF-2))
         (SETQ PWR (TIMES PWR PWR)))
        (GO WHILELABEL))
      (SETQ ANS 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ (SETQ PWR (CAR POWERS-OF-2)) 2)) (RETURN NIL)))
        (PROGN
         (SETQ POWERS-OF-2 (CDR POWERS-OF-2))
         (COND
          ((GEQ N PWR)
           (PROGN (SETQ N (QUOTIENT N PWR)) (SETQ ANS (PLUS ANS 1)) NIL)))
         (SETQ ANS (TIMES ANS 2)))
        (GO WHILELABEL))
      (COND ((GEQ N 2) (SETQ ANS (PLUS ANS 1))))
      (RETURN ANS))) 
(PUT 'ISQRT 'NUMBER-OF-ARGS 1) 
(PUT 'ISQRT 'DEFINED-ON-LINE '420) 
(PUT 'ISQRT 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'ISQRT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISQRT (X) (COND ((LEQ X 0) (TERRLST X 'ISQRT)) (T (IROOTN (FIX2 X) 2)))) 
(PUT 'QROUNDUP 'NUMBER-OF-ARGS 2) 
(PUT 'QROUNDUP 'DEFINED-ON-LINE '425) 
(PUT 'QROUNDUP 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'QROUNDUP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QROUNDUP (M N)
    (COND ((LESSP M 0) (MINUS (QUOTIENT (MINUS M) N)))
          (T (QUOTIENT (PLUS M (DIFFERENCE N 1)) N)))) 
(PUT 'IROOTN 'NUMBER-OF-ARGS 2) 
(PUT 'IROOTN 'DEFINED-ON-LINE '429) 
(PUT 'IROOTN 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'IROOTN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IROOTN (N R)
    (COND ((NOT (FIXP N)) (TYPERR N "integer"))
          ((OR (NOT (FIXP R)) (LEQ R 0)) (TYPERR R "positive integer"))
          ((LESSP N 0)
           (COND ((EVENP R) (TYPERR R "odd integer"))
                 (T (MINUS (IROOTN (MINUS N) R)))))
          ((EQUAL R 1) N) ((EQUAL N 0) 0)
          (T
           (PROG (ANS)
             (SETQ ANS (IROOTN1 N R (ILOG2 N) (QUOTIENT (ILOG2 R) 2)))
             (COND ((GREATERP (EXPT ANS R) N) (RETURN (DIFFERENCE ANS 1)))
                   (T (RETURN ANS))))))) 
(PUT 'IROOTN-POWER2 'NUMBER-OF-ARGS 2) 
(PUT 'IROOTN-POWER2 'DEFINED-ON-LINE '443) 
(PUT 'IROOTN-POWER2 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'IROOTN-POWER2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IROOTN-POWER2 (P Q)
    (PROG (WHOLE-PART P1 NUMANS DENANS)
      (SETQ WHOLE-PART (QUOTIENT (PLUS P (QUOTIENT Q 2)) Q))
      (SETQ P1 (DIFFERENCE P (TIMES Q WHOLE-PART)))
      (SETQ NUMANS
              (PLUS (QUOTIENT (EXPT Q 3) 100) (TIMES 1000 (EXPT Q 3))
                    (TIMES 693 (EXPT Q 2) P1) (TIMES 243 Q (EXPT P1 2))
                    (TIMES 57 (EXPT P1 3))))
      (SETQ DENANS (TIMES 1000 (EXPT Q 3)))
      (RETURN (PLUS 1 (QUOTIENT (TIMES (EXPT 2 WHOLE-PART) NUMANS) DENANS))))) 
(PUT 'IROOTN1 'NUMBER-OF-ARGS 4) 
(PUT 'IROOTN1 'DEFINED-ON-LINE '457) 
(PUT 'IROOTN1 'DEFINED-IN-FILE 'ARITH/BFAUXIL.RED) 
(PUT 'IROOTN1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE IROOTN1 (N R LOGN XS)
    (PROG (X UPB SIZE TMP)
      (SETQ SIZE (QUOTIENT LOGN R))
      (COND ((LESSP SIZE 17) (SETQ UPB (IROOTN-POWER2 (PLUS 1 LOGN) R)))
            (T
             (PROGN
              (SETQ X (DIFFERENCE (QUOTIENT SIZE 2) XS))
              (SETQ UPB
                      (TIMES
                       (IROOTN1 (QUOTIENT N (EXPT 2 (TIMES X R))) R
                                (DIFFERENCE LOGN (TIMES R X)) XS)
                       (EXPT 2 X)))
              (SETQ TMP (EXPT UPB (DIFFERENCE R 1)))
              (RETURN
               (QUOTIENT (PLUS (TIMES (TIMES (DIFFERENCE R 1) UPB) TMP) N)
                         (TIMES R TMP))))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ X UPB)
         (SETQ UPB
                 (DIFFERENCE X
                             (QROUNDUP
                              (DIFFERENCE X
                                          (QUOTIENT N
                                                    (EXPT X (DIFFERENCE R 1))))
                              R))))
        (COND ((NOT (GEQ UPB X)) (GO REPEATLABEL))))
      (RETURN X))) 
(PUT 'IROOTN 'NUMBER-OF-ARGS 2) 
(ENDMODULE) 