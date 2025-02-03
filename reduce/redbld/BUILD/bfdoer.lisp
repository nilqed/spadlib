(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BFDOER)) 
(EXPORTS
 (LIST 'BFEQP 'BFLEQP 'BFLOATEM 'BFMIN 'BFSGN 'CFLOTEM 'CKACC 'DEFLATE2
       'FIRSTROOT 'GFCONJ 'GFDIFF 'GFEQP 'GFEXIT 'GFNEWT 'GFNEWTSET 'GFPLUSN
       'GFRLMULT 'GFROOT 'GFROOTSET 'GFTIMESN 'GFVAL 'GTAG 'GZERO 'INTDIFF
       'ISOLATER 'LISTECHO 'MAXBND1 'MAXBOUND 'MINBND1 'MINBOUND 'MKRATL
       'NCOEFFS 'NEARESTROOT 'POWERCHK 'PRIMP 'PRIMPN 'R2FLBF 'RATDIF 'RATLEQP
       'RATLESSP 'RATMAX 'RATMEAN 'RATMIN 'RATMINUS 'RATPLUS 'REALROOTS
       'RLROOTNO 'RLRTNO 'RLVAL 'ROOT_VAL 'RTREORDER 'SCH 'SIMPGI 'UNIVAR
       'UNGFFORM 'UNSHIFT 'XNSHIFT)) 
(IMPORTS
 (LIST 'A2GF '|ABS:| 'ACCUPR 'ACCUPR1 'ACCUROOT 'AUTOMOD 'BF2FLR 'BFABS
       'BFINVERSE 'BFLOAT 'BFMINUS 'BFNUMP 'BFNZP '|BFP:| 'BFRLMULT 'BFSQRT
       'BFZP 'CFLOT 'CKPREC 'CPXP 'CVT2 'CVT5 '|DIFFERENCE:| 'DIVBF 'DOMAINP
       '|EP:| 'EQCAR '|EQUAL:| 'ERRORP 'ERRORSET* 'EXP 'EXPTBF 'GBFMULT 'GCDN
       'GETPREC 'GF2BF 'GFDIFFER 'GFFINIT 'GFFMULT 'GFFPLUS 'GFFTIMES 'GFIM
       'GFNEWTON 'GFPLUS 'GFRL 'GFRSQ 'GFRTRND 'GFTIMES 'GFZEROP 'GRPBF 'IM2GF
       'INFINITYP 'INVBF 'INVPOLY 'ISOLATEP 'LASTPAIR 'LEADATOM 'LEQ '|LESSP:|
       'LIMCHK 'LOG '|MAKE:IBF| '|MIN:| '|MINUS:| '|MINUSP:| 'MK*SQ 'MKQUOTE
       'MKXCL '|MSD:| '|MT:| 'NCPXP 'NEQ 'NRSTROOT 'NUMR 'OFF 'ON 'ORGSHIFT
       'OUTMODE 'OVER 'PLUBF '|PLUS:| '|PRECI:| 'PRECISION 'R2BF 'R2FL 'REALRAT
       'RERROR 'RESTOREFL 'RL2GF 'RLRTNO2 'RNDPWR 'ROOTRND 'ROOTS 'SCHINF
       'SCHPLUS 'SETFLBF 'SETPREC 'SGN 'SGN1 'SIMP* 'STURM1 'TIMBF '|TIMES:|
       'TRMSG1 'TRMSG12 'TRMSG13 'TRMSG3 'UNIROOTS 'UNIVARIATEP)) 
(FLUID
 '(*BFTAG *PCMP *RVAR *STRM |LIMS#| |MLTR#| |EMSG#| *XO *GFP *POWERGCD |PFL#|
   |ACFL#| |ACCM#| |FROOT#| *BACKTRACE)) 
(FLUID
 '(|ACC#| |SPREC#| |PFACTOR#| |RR#| |SS#| *XN *ZP *XOBF *NOEQNS *MSG |ROOTACC#|
   |ROOTACC##|)) 
(FLUID '(|INIPREC#|)) 
(GLOBAL '(BFZ* BFONE* BFHALF* BFTWO* PRD% !LOG2 BFEE* !EE)) 
(SETQ BFEE*
        ((LAMBDA (X)
           (COND ((FLOATP X) (FL2BF X))
                 (T
                  (NORMBF
                   (COND ((NOT (ATOM X)) X)
                         ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                         (T (|READ:NUM| X)))))))
         !EE)) 
(DE DNP (X) (EQCAR X '|:DN:|)) 
(PUT 'DNP 'NUMBER-OF-ARGS 1) 
(PUT 'DNP 'DEFINED-ON-LINE '79) 
(PUT 'DNP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'DNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'DNP 'INLINE '(LAMBDA (X) (EQCAR X '|:DN:|))) 
(PUT 'BFLEQP 'NUMBER-OF-ARGS 2) 
(PUT 'BFLEQP 'DEFINED-ON-LINE '81) 
(PUT 'BFLEQP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFLEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFLEQP (A B) (COND ((ATOM A) (LEQ A B)) (T (NOT (GRPBF A B))))) 
(PUT 'BFEQP 'NUMBER-OF-ARGS 2) 
(PUT 'BFEQP 'DEFINED-ON-LINE '83) 
(PUT 'BFEQP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFEQP (A B)
    (COND ((ATOM A) (EQUAL A B))
          (T
           ((LAMBDA (MA MB)
              (OR (AND (ZEROP MA) (ZEROP MB))
                  (AND (EQUAL (CDDR A) (CDDR B)) (EQUAL MA MB))))
            (CADR A) (CADR B))))) 
(PUT 'BFSGN 'NUMBER-OF-ARGS 1) 
(PUT 'BFSGN 'DEFINED-ON-LINE '87) 
(PUT 'BFSGN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFSGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFSGN (U) (COND ((ATOM U) (SGN U)) (T (SGN (CADR U))))) 
(PUT 'BFMIN 'NUMBER-OF-ARGS 2) 
(PUT 'BFMIN 'DEFINED-ON-LINE '90) 
(PUT 'BFMIN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFMIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFMIN (U V) (COND ((ATOM U) (MIN U V)) (T (|MIN2:| U V)))) 
(PUT 'GFCONJ 'NUMBER-OF-ARGS 1) 
(PUT 'GFCONJ 'DEFINED-ON-LINE '93) 
(PUT 'GFCONJ 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFCONJ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFCONJ (U) (CONS (CAR U) (BFMINUS (CDR U)))) 
(PUT 'GFRLMULT 'NUMBER-OF-ARGS 2) 
(PUT 'GFRLMULT 'DEFINED-ON-LINE '95) 
(PUT 'GFRLMULT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFRLMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFRLMULT (R U)
    (COND ((ATOM (CAR U)) (GFFMULT R U))
          (T
           (GBFMULT
            (COND ((FLOATP R) (FL2BF R))
                  (T
                   (NORMBF
                    (COND ((NOT (ATOM R)) R)
                          ((FIXP R) (CONS '|:RD:| (CONS R 0)))
                          (T (|READ:NUM| R))))))
            U)))) 
(PUT 'GFEQP 'NUMBER-OF-ARGS 2) 
(PUT 'GFEQP 'DEFINED-ON-LINE '98) 
(PUT 'GFEQP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFEQP (U V)
    ((LAMBDA (U)
       (COND
        ((NOT (ATOM (CAR U)))
         (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
        (T (EQUAL U '(0.0 . 0.0)))))
     (GFDIFFER U V))) 
(PUT 'NCOEFFS 'NUMBER-OF-ARGS 1) 
(PUT 'NCOEFFS 'DEFINED-ON-LINE '100) 
(PUT 'NCOEFFS 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'NCOEFFS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NCOEFFS (P)
    (PROG (N Q D)
      (SETQ D 0)
      (PROG (I)
        (SETQ I P)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ N (CAR I))
            (PROG ()
             WHILELABEL
              (COND ((NOT (LESSP D N)) (RETURN NIL)))
              (PROGN (SETQ Q (CONS NIL Q)) (SETQ D (PLUS D 1)))
              (GO WHILELABEL))
            (SETQ D (PLUS D 1))
            (SETQ Q (CONS (CDR I) Q))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN (CONS N Q)))) 
(PUT 'RLVAL 'NUMBER-OF-ARGS 2) 
(PUT 'RLVAL 'DEFINED-ON-LINE '108) 
(PUT 'RLVAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RLVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RLVAL (P R)
    (COND ((OR (ATOM P) (ATOM (CAR P))) P)
          ((COND ((ATOM R) (ZEROP R)) (T (EQUAL (CADR R) 0)))
           (COND ((EQUAL (CAAR P) 0) (CDAR P)) (T (R2FLBF 0))))
          (T
           (PROG (C BF)
             (SETQ BF (AND (EQCAR R '|:RD:|) (NOT (ATOM (CDR R)))))
             (SETQ C (CAR (SETQ P (CDR (NCOEFFS P)))))
             (PROG (I)
               (SETQ I (CDR P))
              LAB
               (COND ((NULL I) (RETURN NIL)))
               ((LAMBDA (I)
                  (PROGN
                   (SETQ C (COND (BF (|TIMES:| R C)) (T (TIMES R C))))
                   (COND
                    (I (SETQ C (COND (BF (|PLUS:| I C)) (T (PLUS C I))))))))
                (CAR I))
               (SETQ I (CDR I))
               (GO LAB))
             (RETURN (COND (BF (NORMBF (|ROUND:MT| C |:BPREC:|))) (T C))))))) 
(DE |SQR:| (A) (|TIMES:| A A)) 
(PUT '|SQR:| 'NUMBER-OF-ARGS 1) 
(PUT '|SQR:| 'DEFINED-ON-LINE '120) 
(PUT '|SQR:| 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT '|SQR:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '|SQR:| 'INLINE '(LAMBDA (A) (|TIMES:| A A))) 
(PUT 'DEFLATE2 'NUMBER-OF-ARGS 2) 
(PUT 'DEFLATE2 'DEFINED-ON-LINE '122) 
(PUT 'DEFLATE2 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'DEFLATE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEFLATE2 (P U)
    (PROG (Q N B C F G H J)
      (SETQ B (|TIMES:| BFTWO* (CAR U)))
      (SETQ C
              (|MINUS:|
               (|PLUS:| (|TIMES:| (CAR U) (CAR U))
                        (|TIMES:| (CDR U) (CDR U)))))
      (SETQ G (SETQ H BFZ*))
      (SETQ N (DIFFERENCE (CAR (SETQ P (NCOEFFS P))) 1))
      (SETQ P (CDR P))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP N 0)) (RETURN NIL)))
        (PROGN
         (SETQ N (DIFFERENCE N 1))
         (SETQ F (|PLUS:| (|TIMES:| B G) (|TIMES:| C H)))
         (COND ((SETQ J (CAR P)) (SETQ F (|PLUS:| J F))))
         (COND ((NEQ (CADR F) 0) (SETQ Q (CONS (CONS N F) Q))))
         (SETQ H G)
         (SETQ G F)
         (SETQ P (CDR P)))
        (GO WHILELABEL))
      (RETURN Q))) 
(PUT 'PRIMP 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMP 'DEFINED-ON-LINE '137) 
(PUT 'PRIMP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'PRIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMP (P)
    (COND ((ATOM P) (SGN P))
          (T
           (PROG (D)
             (SETQ D 0)
             (PROG (Y)
               (SETQ Y P)
              LAB
               (COND ((NULL Y) (RETURN NIL)))
               ((LAMBDA (Y) (SETQ D (GCDN D (CDR Y)))) (CAR Y))
               (SETQ Y (CDR Y))
               (GO LAB))
             (RETURN
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y P)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y)
                                    (CONS (CAR Y) (QUOTIENT (CDR Y) D)))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Y) (CONS (CAR Y) (QUOTIENT (CDR Y) D)))
                          (CAR Y))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'PRIMPN 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMPN 'DEFINED-ON-LINE '142) 
(PUT 'PRIMPN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'PRIMPN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMPN (P)
    (PROG (N G)
      (SETQ N (CAR P))
      (SETQ P (CDR P))
      (SETQ G 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P (EQUAL (CAR P) 0))) (RETURN NIL)))
        (PROGN (SETQ P (CDR P)) (SETQ N (DIFFERENCE N 1)))
        (GO WHILELABEL))
      (COND ((LESSP N 0) (RETURN 0)) ((EQUAL N 0) (RETURN (SGN (CAR P)))))
      (PROG (Y)
        (SETQ Y P)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (SETQ G (GCDN Y G))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN
       (CONS N
             (PROG (Y FORALL-RESULT FORALL-ENDPTR)
               (SETQ Y P)
               (COND ((NULL Y) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS ((LAMBDA (Y) (QUOTIENT Y G)) (CAR Y))
                                     NIL)))
              LOOPLABEL
               (SETQ Y (CDR Y))
               (COND ((NULL Y) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS ((LAMBDA (Y) (QUOTIENT Y G)) (CAR Y)) NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'R2FLBF 'NUMBER-OF-ARGS 1) 
(PUT 'R2FLBF 'DEFINED-ON-LINE '149) 
(PUT 'R2FLBF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'R2FLBF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2FLBF (U) (COND (*BFTAG (R2BF U)) (T (R2FL U)))) 
(PUT 'INTDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'INTDIFF 'DEFINED-ON-LINE '152) 
(PUT 'INTDIFF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'INTDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INTDIFF (P)
    (PROGN
     (COND ((EQUAL (CAAR P) 0) (SETQ P (CDR P))))
     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
       (SETQ Y P)
       (COND ((NULL Y) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (Y)
                           (CONS (DIFFERENCE (CAR Y) 1)
                                 (TIMES (CAR Y) (CDR Y))))
                         (CAR Y))
                        NIL)))
      LOOPLABEL
       (SETQ Y (CDR Y))
       (COND ((NULL Y) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (Y)
                   (CONS (DIFFERENCE (CAR Y) 1) (TIMES (CAR Y) (CDR Y))))
                 (CAR Y))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'RATMINUS 'NUMBER-OF-ARGS 1) 
(PUT 'RATMINUS 'DEFINED-ON-LINE '156) 
(PUT 'RATMINUS 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATMINUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATMINUS (R) (CONS (MINUS (CAR R)) (CDR R))) 
(PUT 'RATDIF 'NUMBER-OF-ARGS 2) 
(PUT 'RATDIF 'DEFINED-ON-LINE '158) 
(PUT 'RATDIF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATDIF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATDIF (R S) (RATPLUSM R (RATMINUS S) NIL)) 
(PUT 'RATPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'RATPLUS 'DEFINED-ON-LINE '160) 
(PUT 'RATPLUS 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATPLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATPLUS (R S) (RATPLUSM R S NIL)) 
(PUT 'RATMEAN 'NUMBER-OF-ARGS 2) 
(PUT 'RATMEAN 'DEFINED-ON-LINE '162) 
(PUT 'RATMEAN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATMEAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATMEAN (R S) (RATPLUSM R S T)) 
(PUT 'RATPLUSM 'NUMBER-OF-ARGS 3) 
(PUT 'RATPLUSM 'DEFINED-ON-LINE '164) 
(PUT 'RATPLUSM 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATPLUSM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RATPLUSM (R S M)
    (PROG (RA RD SA SD A D G)
      (SETQ RA (CAR R))
      (SETQ RD (CDR R))
      (SETQ SA (CAR S))
      (SETQ SD (CDR S))
      (COND ((EQUAL RD SD) (PROGN (SETQ A (PLUS RA SA)) (SETQ D RD)))
            (T
             (PROGN
              (SETQ G (GCDN RD SD))
              (SETQ A
                      (PLUS (TIMES (QUOTIENT SD G) RA)
                            (TIMES (QUOTIENT RD G) SA)))
              (SETQ D (TIMES (QUOTIENT RD G) SD)))))
      (COND (M (SETQ D (PLUS D D))))
      (COND ((EQUAL A 0) (RETURN (CONS 0 1))))
      (SETQ G (GCDN A D))
      (RETURN (CONS (QUOTIENT A G) (QUOTIENT D G))))) 
(PUT 'RATMIN 'NUMBER-OF-ARGS 2) 
(PUT 'RATMIN 'DEFINED-ON-LINE '174) 
(PUT 'RATMIN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATMIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATMIN (A B) (COND ((RATLESSP A B) A) (T B))) 
(PUT 'RATMAX 'NUMBER-OF-ARGS 2) 
(PUT 'RATMAX 'DEFINED-ON-LINE '176) 
(PUT 'RATMAX 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATMAX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATMAX (A B) (COND ((RATLESSP A B) B) (T A))) 
(PUT 'RATLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'RATLESSP 'DEFINED-ON-LINE '178) 
(PUT 'RATLESSP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATLESSP (A B) (LESSP (CAR (RATDIF A B)) 0)) 
(PUT 'RATLEQP 'NUMBER-OF-ARGS 2) 
(PUT 'RATLEQP 'DEFINED-ON-LINE '180) 
(PUT 'RATLEQP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RATLEQP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RATLEQP (A B) (LEQ (CAR (RATDIF A B)) 0)) 
(PUT 'LISTECHO 'NUMBER-OF-ARGS 2) 
(PUT 'LISTECHO 'DEFINED-ON-LINE '182) 
(PUT 'LISTECHO 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'LISTECHO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISTECHO (L N)
    (COND ((LESSP N 2) L)
          (T
           (PROG (C)
             (PROG (X)
               (SETQ X L)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X)
                  (PROGN
                   (PROG (I)
                     (SETQ I 1)
                    LAB
                     (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                     (SETQ C (APPEND C (LIST X)))
                     (SETQ I (PLUS2 I 1))
                     (GO LAB))))
                (CAR X))
               (SETQ X (CDR X))
               (GO LAB))
             (RETURN C))))) 
(PUT 'BFLOATEM 'NUMBER-OF-ARGS 1) 
(PUT 'BFLOATEM 'DEFINED-ON-LINE '186) 
(PUT 'BFLOATEM 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFLOATEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFLOATEM (P)
    (COND
     (P
      ((LAMBDA (CP)
         (PROGN
          (PROG (C FORALL-RESULT FORALL-ENDPTR)
            (SETQ C P)
            (COND ((NULL C) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (C)
                                (CONS (CAR C)
                                      (COND
                                       (CP
                                        (CONS
                                         (COND
                                          ((FLOATP (CADR C)) (FL2BF (CADR C)))
                                          (T
                                           (NORMBF
                                            (COND
                                             ((NOT (ATOM (CADR C))) (CADR C))
                                             ((FIXP (CADR C))
                                              (CONS '|:RD:| (CONS (CADR C) 0)))
                                             (T (|READ:NUM| (CADR C)))))))
                                         (COND
                                          ((FLOATP (CDDR C)) (FL2BF (CDDR C)))
                                          (T
                                           (NORMBF
                                            (COND
                                             ((NOT (ATOM (CDDR C))) (CDDR C))
                                             ((FIXP (CDDR C))
                                              (CONS '|:RD:| (CONS (CDDR C) 0)))
                                             (T (|READ:NUM| (CDDR C)))))))))
                                       (T
                                        (COND
                                         ((FLOATP (CDR C)) (FL2BF (CDR C)))
                                         (T
                                          (NORMBF
                                           (COND ((NOT (ATOM (CDR C))) (CDR C))
                                                 ((FIXP (CDR C))
                                                  (CONS '|:RD:|
                                                        (CONS (CDR C) 0)))
                                                 (T
                                                  (|READ:NUM| (CDR C)))))))))))
                              (CAR C))
                             NIL)))
           LOOPLABEL
            (SETQ C (CDR C))
            (COND ((NULL C) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (C)
                        (CONS (CAR C)
                              (COND
                               (CP
                                (CONS
                                 (COND ((FLOATP (CADR C)) (FL2BF (CADR C)))
                                       (T
                                        (NORMBF
                                         (COND ((NOT (ATOM (CADR C))) (CADR C))
                                               ((FIXP (CADR C))
                                                (CONS '|:RD:|
                                                      (CONS (CADR C) 0)))
                                               (T (|READ:NUM| (CADR C)))))))
                                 (COND ((FLOATP (CDDR C)) (FL2BF (CDDR C)))
                                       (T
                                        (NORMBF
                                         (COND ((NOT (ATOM (CDDR C))) (CDDR C))
                                               ((FIXP (CDDR C))
                                                (CONS '|:RD:|
                                                      (CONS (CDDR C) 0)))
                                               (T (|READ:NUM| (CDDR C)))))))))
                               (T
                                (COND ((FLOATP (CDR C)) (FL2BF (CDR C)))
                                      (T
                                       (NORMBF
                                        (COND ((NOT (ATOM (CDR C))) (CDR C))
                                              ((FIXP (CDR C))
                                               (CONS '|:RD:| (CONS (CDR C) 0)))
                                              (T (|READ:NUM| (CDR C)))))))))))
                      (CAR C))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))
       (NOT
        (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
            (OR (NUMBERP (CDAR P))
                (AND (EQCAR (CDAR P) '|:RD:|)
                     (NOT (ATOM (CDR (CDAR P)))))))))))) 
(PUT 'CFLOTEM 'NUMBER-OF-ARGS 1) 
(PUT 'CFLOTEM 'DEFINED-ON-LINE '191) 
(PUT 'CFLOTEM 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'CFLOTEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFLOTEM (P)
    ((LAMBDA (CP)
       (PROGN
        (PROG (C FORALL-RESULT FORALL-ENDPTR)
          (SETQ C P)
          (COND ((NULL C) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (C)
                              (CONS (CAR C)
                                    (COND
                                     (CP
                                      (CONS (CFLOT (CADR C)) (CFLOT (CDDR C))))
                                     (T (CFLOT (CDR C))))))
                            (CAR C))
                           NIL)))
         LOOPLABEL
          (SETQ C (CDR C))
          (COND ((NULL C) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (C)
                      (CONS (CAR C)
                            (COND (CP (CONS (CFLOT (CADR C)) (CFLOT (CDDR C))))
                                  (T (CFLOT (CDR C))))))
                    (CAR C))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))))
     (NOT
      (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
          (OR (NUMBERP (CDAR P))
              (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P)))))))))) 
(PUT 'GFDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'GFDIFF 'DEFINED-ON-LINE '196) 
(PUT 'GFDIFF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFDIFF (P)
    (PROGN
     (COND ((EQUAL (CAAR P) 0) (SETQ P (CDR P))))
     (COND
      ((OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
           (OR (NUMBERP (CDAR P))
               (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P)))))))
       (PROG (A FORALL-RESULT FORALL-ENDPTR)
         (SETQ A P)
         (COND ((NULL A) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A)
                             (CONS (DIFFERENCE (CAR A) 1)
                                   (COND
                                    ((ATOM (CDR A)) (TIMES (CAR A) (CDR A)))
                                    (T
                                     (NORMBF
                                      (|ROUND:MT|
                                       (|TIMES:|
                                        (COND
                                         ((FLOATP (CAR A)) (FL2BF (CAR A)))
                                         (T
                                          (NORMBF
                                           (COND ((NOT (ATOM (CAR A))) (CAR A))
                                                 ((FIXP (CAR A))
                                                  (CONS '|:RD:|
                                                        (CONS (CAR A) 0)))
                                                 (T (|READ:NUM| (CAR A)))))))
                                        (CDR A))
                                       |:BPREC:|))))))
                           (CAR A))
                          NIL)))
        LOOPLABEL
         (SETQ A (CDR A))
         (COND ((NULL A) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (A)
                     (CONS (DIFFERENCE (CAR A) 1)
                           (COND ((ATOM (CDR A)) (TIMES (CAR A) (CDR A)))
                                 (T
                                  (NORMBF
                                   (|ROUND:MT|
                                    (|TIMES:|
                                     (COND ((FLOATP (CAR A)) (FL2BF (CAR A)))
                                           (T
                                            (NORMBF
                                             (COND
                                              ((NOT (ATOM (CAR A))) (CAR A))
                                              ((FIXP (CAR A))
                                               (CONS '|:RD:| (CONS (CAR A) 0)))
                                              (T (|READ:NUM| (CAR A)))))))
                                     (CDR A))
                                    |:BPREC:|))))))
                   (CAR A))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
      (T
       (PROG (A FORALL-RESULT FORALL-ENDPTR)
         (SETQ A P)
         (COND ((NULL A) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (A)
                             (CONS (DIFFERENCE (CAR A) 1)
                                   (GFTIMES
                                    (COND
                                     (*BFTAG
                                      (CONS
                                       (COND ((FLOATP (CAR A)) (FL2BF (CAR A)))
                                             (T
                                              (NORMBF
                                               (COND
                                                ((NOT (ATOM (CAR A))) (CAR A))
                                                ((FIXP (CAR A))
                                                 (CONS '|:RD:|
                                                       (CONS (CAR A) 0)))
                                                (T (|READ:NUM| (CAR A)))))))
                                       BFZ*))
                                     (T (CONS (CFLOT (CAR A)) 0.0)))
                                    (CDR A))))
                           (CAR A))
                          NIL)))
        LOOPLABEL
         (SETQ A (CDR A))
         (COND ((NULL A) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (A)
                     (CONS (DIFFERENCE (CAR A) 1)
                           (GFTIMES
                            (COND
                             (*BFTAG
                              (CONS
                               (COND ((FLOATP (CAR A)) (FL2BF (CAR A)))
                                     (T
                                      (NORMBF
                                       (COND ((NOT (ATOM (CAR A))) (CAR A))
                                             ((FIXP (CAR A))
                                              (CONS '|:RD:| (CONS (CAR A) 0)))
                                             (T (|READ:NUM| (CAR A)))))))
                               BFZ*))
                             (T (CONS (CFLOT (CAR A)) 0.0)))
                            (CDR A))))
                   (CAR A))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'GFVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GFVAL 'DEFINED-ON-LINE '203) 
(PUT 'GFVAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFVAL (P X)
    (PROGN
     (SETQ P
             (COND
              ((NOT
                (OR
                 (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                 (OR (NUMBERP (CDAR P))
                     (AND (EQCAR (CDAR P) '|:RD:|)
                          (NOT (ATOM (CDR (CDAR P))))))))
               (GFCVAL P X))
              (T (GFRVAL P X))))
     (COND
      ((AND (ATOM (CAR P)) (OR (INFINITYP (CAR P)) (INFINITYP (CDR P))))
       (ERROR 0 "gfval -> infinity"))
      (T P)))) 
(DE RNDPWRXC (X C)
    (COND ((ATOM (CAR X)) C)
          (T
           (CONS (NORMBF (|ROUND:MT| (CAR C) |:BPREC:|))
                 (NORMBF (|ROUND:MT| (CDR C) |:BPREC:|)))))) 
(PUT 'RNDPWRXC 'NUMBER-OF-ARGS 2) 
(PUT 'RNDPWRXC 'DEFINED-ON-LINE '208) 
(PUT 'RNDPWRXC 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RNDPWRXC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'RNDPWRXC 'INLINE
      '(LAMBDA (X C)
         (COND ((ATOM (CAR X)) C)
               (T
                (CONS (NORMBF (|ROUND:MT| (CAR C) |:BPREC:|))
                      (NORMBF (|ROUND:MT| (CDR C) |:BPREC:|))))))) 
(PUT 'GFRVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GFRVAL 'DEFINED-ON-LINE '211) 
(PUT 'GFRVAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFRVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFRVAL (P X)
    (COND
     ((COND
       ((NOT (ATOM (CAR X)))
        (AND (EQUAL (CADR (CAR X)) 0) (EQUAL (CADR (CDR X)) 0)))
       (T (EQUAL X '(0.0 . 0.0))))
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
       (COND ((EQUAL (CAAR P) 0) (CDAR P)) (T 0))))
     ((COND ((ATOM (CDR X)) (ZEROP (CDR X))) (T (EQUAL (CADR (CDR X)) 0)))
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
       (RLVAL P (CAR X))))
     (T
      (PROG (C)
        (SETQ C
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
                 (CAR (SETQ P (CDR (NCOEFFS P))))))
        (PROG (I)
          (SETQ I (CDR P))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ C (GFTIMESN X C))
              (COND (I (SETQ C (CONS (BFPLUSN I (CAR C)) (CDR C)))))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        (RETURN
         (COND ((ATOM (CAR X)) C)
               (T
                (CONS (NORMBF (|ROUND:MT| (CAR C) |:BPREC:|))
                      (NORMBF (|ROUND:MT| (CDR C) |:BPREC:|)))))))))) 
(PUT 'GFCVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GFCVAL 'DEFINED-ON-LINE '222) 
(PUT 'GFCVAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFCVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFCVAL (P X)
    (COND
     ((COND
       ((NOT (ATOM (CAR X)))
        (AND (EQUAL (CADR (CAR X)) 0) (EQUAL (CADR (CDR X)) 0)))
       (T (EQUAL X '(0.0 . 0.0))))
      (COND ((EQUAL (CAAR P) 0) (CDAR P))
            (T
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
     (T
      (PROG (C)
        (SETQ C (CAR (SETQ P (CDR (NCOEFFS P)))))
        (PROG (I)
          (SETQ I (CDR P))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN (SETQ C (GFTIMESN X C)) (COND (I (SETQ C (GFPLUSN I C))))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        (RETURN
         (COND ((ATOM (CAR X)) C)
               (T
                (CONS (NORMBF (|ROUND:MT| (CAR C) |:BPREC:|))
                      (NORMBF (|ROUND:MT| (CDR C) |:BPREC:|)))))))))) 
(PUT 'BFPLUSN 'NUMBER-OF-ARGS 2) 
(PUT 'BFPLUSN 'DEFINED-ON-LINE '231) 
(PUT 'BFPLUSN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFPLUSN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFPLUSN (U V) (COND ((ATOM U) (PLUS U V)) (T (|PLUS:| U V)))) 
(PUT 'GFPLUSN 'NUMBER-OF-ARGS 2) 
(PUT 'GFPLUSN 'DEFINED-ON-LINE '233) 
(PUT 'GFPLUSN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFPLUSN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFPLUSN (U V)
    (COND ((ATOM (CAR U)) (GFFPLUS U V))
          (T (CONS (|PLUS:| (CAR U) (CAR V)) (|PLUS:| (CDR U) (CDR V)))))) 
(PUT 'GFTIMESN 'NUMBER-OF-ARGS 2) 
(PUT 'GFTIMESN 'DEFINED-ON-LINE '237) 
(PUT 'GFTIMESN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFTIMESN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFTIMESN (U V)
    (COND ((ATOM (CAR U)) (GFFTIMES U V))
          (T
           (PROG (RU IU RV IV)
             (SETQ RU (CAR U))
             (SETQ IU (CDR U))
             (SETQ RV (CAR V))
             (SETQ IV (CDR V))
             (RETURN
              (CONS (|DIFFERENCE:| (|TIMES:| RU RV) (|TIMES:| IU IV))
                    (|PLUS:| (|TIMES:| RU IV) (|TIMES:| IU RV)))))))) 
(PUT 'MINBOUND 'NUMBER-OF-ARGS 2) 
(PUT 'MINBOUND 'DEFINED-ON-LINE '244) 
(PUT 'MINBOUND 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'MINBOUND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MINBOUND (P O)
    ((LAMBDA (BF |ACC#|)
       (PROGN
        (SETQ P
                (COND ((ATOM (SETQ P (AUTOMOD (CKPREC P)))) NIL)
                      (T (MINBND1 P O))))
        (SETQ *BFTAG BF)
        (RESTOREFL)
        (OUTMODE P)))
     *BFTAG 6)) 
(FLAG '(MAXBOUND MINBOUND) 'OPFN) 
(PUT 'MAXBOUND 'NUMBER-OF-ARGS 1) 
(PUT 'MAXBOUND 'DEFINED-ON-LINE '251) 
(PUT 'MAXBOUND 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'MAXBOUND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAXBOUND (P) (PROGN (SETQ P (MAXBND1 (CKPREC P))) (RESTOREFL) (OUTMODE P))) 
(PUT 'MAXBND1 'NUMBER-OF-ARGS 1) 
(PUT 'MAXBND1 'DEFINED-ON-LINE '254) 
(PUT 'MAXBND1 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'MAXBND1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAXBND1 (P)
    (PROG (NC BF M PR)
      (SETQ BF *BFTAG)
      (SETQ PR (PLUS 2 (PRECISION 0)))
      (COND ((ATOM (SETQ P (GFFINIT P))) (RETURN NIL)))
      (PRECISION1 (DIFFERENCE 8 2) T)
      (SETQ P (BFLOATRD P))
      (SETQ NC
              (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                  (OR (NUMBERP (CDAR P))
                      (AND (EQCAR (CDAR P) '|:RD:|)
                           (NOT (ATOM (CDR (CDAR P))))))))
      (SETQ P (REVERSE P))
      (SETQ *BFTAG BF)
      (SETQ M
              ((LAMBDA (G130)
                 (COND ((ATOM G130) (TIMES 2 G130))
                       (T
                        (NORMBF
                         (|ROUND:MT|
                          (|TIMES:|
                           (COND ((FLOATP 2) (FL2BF 2))
                                 (T
                                  (NORMBF
                                   (COND ((NOT (ATOM 2)) 2)
                                         ((FIXP 2) (CONS '|:RD:| (CONS 2 0)))
                                         (T (|READ:NUM| 2))))))
                           G130)
                          |:BPREC:|)))))
               (MAXBDBF P NC)))
      (PRECISION1 (DIFFERENCE PR 2) T)
      (RETURN M))) 
(PUT 'MINBND1 'NUMBER-OF-ARGS 2) 
(PUT 'MINBND1 'DEFINED-ON-LINE '267) 
(PUT 'MINBND1 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'MINBND1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MINBND1 (P ORG)
    (PROG (B C)
      (SETQ B *BFTAG)
      (SETQ *BFTAG
              ((LAMBDA (X) (AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))))
               (COND
                ((NOT
                  ((LAMBDA (P)
                     (OR (NUMBERP P)
                         (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P))))))
                   (SETQ C (CDAR P))))
                 (CAR C))
                (T C))))
      (SETQ ORG (A2GF ORG))
      (COND
       ((OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
            (OR (NUMBERP (CDAR P))
                (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P)))))))
        (COND
         ((COND ((ATOM (CDR ORG)) (ZEROP (CDR ORG)))
                (T (EQUAL (CADR (CDR ORG)) 0)))
          (SETQ ORG (CAR ORG)))
         (T
          (SETQ P
                  (PROG (R FORALL-RESULT FORALL-ENDPTR)
                    (SETQ R P)
                    (COND ((NULL R) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (R)
                                        (CONS (CAR R)
                                              (COND
                                               (*BFTAG
                                                (CONS
                                                 (COND
                                                  ((FLOATP (CDR R))
                                                   (FL2BF (CDR R)))
                                                  (T
                                                   (NORMBF
                                                    (COND
                                                     ((NOT (ATOM (CDR R)))
                                                      (CDR R))
                                                     ((FIXP (CDR R))
                                                      (CONS '|:RD:|
                                                            (CONS (CDR R) 0)))
                                                     (T
                                                      (|READ:NUM| (CDR R)))))))
                                                 BFZ*))
                                               (T
                                                (CONS (CFLOT (CDR R)) 0.0)))))
                                      (CAR R))
                                     NIL)))
                   LOOPLABEL
                    (SETQ R (CDR R))
                    (COND ((NULL R) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (R)
                                (CONS (CAR R)
                                      (COND
                                       (*BFTAG
                                        (CONS
                                         (COND
                                          ((FLOATP (CDR R)) (FL2BF (CDR R)))
                                          (T
                                           (NORMBF
                                            (COND
                                             ((NOT (ATOM (CDR R))) (CDR R))
                                             ((FIXP (CDR R))
                                              (CONS '|:RD:| (CONS (CDR R) 0)))
                                             (T (|READ:NUM| (CDR R)))))))
                                         BFZ*))
                                       (T (CONS (CFLOT (CDR R)) 0.0)))))
                              (CAR R))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))))
      (SETQ P (BFINVERSE (MAXBND1 (INVPOLY (ORGSHIFT P ORG)))))
      (SETQ *BFTAG B)
      (RETURN P))) 
(PUT 'MAXBDBF 'NUMBER-OF-ARGS 2) 
(PUT 'MAXBDBF 'DEFINED-ON-LINE '276) 
(PUT 'MAXBDBF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'MAXBDBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXBDBF (P NC)
    (PROG (AN AL M MM N)
      (SETQ N (CAR (SETQ AN (CAR P))))
      (SETQ AN
              (COND
               (NC
                (COND ((ATOM (CDR AN)) (ABS (CDR AN))) (T (|ABS:| (CDR AN)))))
               (T (BFSQRT (GFRSQ (CDR AN))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ P (CDR P))) (RETURN NIL)))
        (PROGN
         (SETQ AL
                 (COND
                  (NC
                   (COND ((ATOM (CDAR P)) (ABS (CDAR P)))
                         (T (|ABS:| (CDAR P)))))
                  (T (BFSQRT (GFRSQ (CDAR P))))))
         (SETQ MM
                 (AND (NOT (ZEROP (CADR AL)))
                      (LOGRTN (NORMBF (|DIVIDE:| AL AN |:BPREC:|))
                       (DIFFERENCE N (CAAR P)))))
         (COND ((OR (NOT M) (AND MM (GREATERP MM M))) (SETQ M MM))))
        (GO WHILELABEL))
      (SETQ M (FL2BFEXP M))
      (RETURN M))) 
(PUT 'BFLOATRD 'NUMBER-OF-ARGS 1) 
(PUT 'BFLOATRD 'DEFINED-ON-LINE '289) 
(PUT 'BFLOATRD 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFLOATRD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFLOATRD (P)
    ((LAMBDA (CP)
       (PROGN
        (PROG (C FORALL-RESULT FORALL-ENDPTR)
          (SETQ C P)
          (COND ((NULL C) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (C)
                              (CONS (CAR C)
                                    (COND
                                     (CP
                                      (CONS
                                       (NORMBF
                                        (|ROUND:MT|
                                         (COND
                                          ((FLOATP (CADR C)) (FL2BF (CADR C)))
                                          (T
                                           (NORMBF
                                            (COND
                                             ((NOT (ATOM (CADR C))) (CADR C))
                                             ((FIXP (CADR C))
                                              (CONS '|:RD:| (CONS (CADR C) 0)))
                                             (T (|READ:NUM| (CADR C)))))))
                                         |:BPREC:|))
                                       (NORMBF
                                        (|ROUND:MT|
                                         (COND
                                          ((FLOATP (CDDR C)) (FL2BF (CDDR C)))
                                          (T
                                           (NORMBF
                                            (COND
                                             ((NOT (ATOM (CDDR C))) (CDDR C))
                                             ((FIXP (CDDR C))
                                              (CONS '|:RD:| (CONS (CDDR C) 0)))
                                             (T (|READ:NUM| (CDDR C)))))))
                                         |:BPREC:|))))
                                     (T
                                      (NORMBF
                                       (|ROUND:MT|
                                        (COND
                                         ((FLOATP (CDR C)) (FL2BF (CDR C)))
                                         (T
                                          (NORMBF
                                           (COND ((NOT (ATOM (CDR C))) (CDR C))
                                                 ((FIXP (CDR C))
                                                  (CONS '|:RD:|
                                                        (CONS (CDR C) 0)))
                                                 (T (|READ:NUM| (CDR C)))))))
                                        |:BPREC:|))))))
                            (CAR C))
                           NIL)))
         LOOPLABEL
          (SETQ C (CDR C))
          (COND ((NULL C) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (C)
                      (CONS (CAR C)
                            (COND
                             (CP
                              (CONS
                               (NORMBF
                                (|ROUND:MT|
                                 (COND ((FLOATP (CADR C)) (FL2BF (CADR C)))
                                       (T
                                        (NORMBF
                                         (COND ((NOT (ATOM (CADR C))) (CADR C))
                                               ((FIXP (CADR C))
                                                (CONS '|:RD:|
                                                      (CONS (CADR C) 0)))
                                               (T (|READ:NUM| (CADR C)))))))
                                 |:BPREC:|))
                               (NORMBF
                                (|ROUND:MT|
                                 (COND ((FLOATP (CDDR C)) (FL2BF (CDDR C)))
                                       (T
                                        (NORMBF
                                         (COND ((NOT (ATOM (CDDR C))) (CDDR C))
                                               ((FIXP (CDDR C))
                                                (CONS '|:RD:|
                                                      (CONS (CDDR C) 0)))
                                               (T (|READ:NUM| (CDDR C)))))))
                                 |:BPREC:|))))
                             (T
                              (NORMBF
                               (|ROUND:MT|
                                (COND ((FLOATP (CDR C)) (FL2BF (CDR C)))
                                      (T
                                       (NORMBF
                                        (COND ((NOT (ATOM (CDR C))) (CDR C))
                                              ((FIXP (CDR C))
                                               (CONS '|:RD:| (CONS (CDR C) 0)))
                                              (T (|READ:NUM| (CDR C)))))))
                                |:BPREC:|))))))
                    (CAR C))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))))
     (NOT
      (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
          (OR (NUMBERP (CDAR P))
              (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P)))))))))) 
(PUT 'LOGRTN 'NUMBER-OF-ARGS 2) 
(PUT 'LOGRTN 'DEFINED-ON-LINE '295) 
(PUT 'LOGRTN 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'LOGRTN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOGRTN (X N)
    ((LAMBDA (M)
       ((LAMBDA (P)
          ((LAMBDA (Y) (QUOTIENT Y N))
           (PLUS (LOG (QUOTIENT M (EXPT 2.0 P)))
                 (TIMES (PLUS P (CDDR X)) !LOG2))))
        (DIFFERENCE (|MSD:| M) 1)))
     (CADR X))) 
(PUT 'FL2BFEXP 'NUMBER-OF-ARGS 1) 
(PUT 'FL2BFEXP 'DEFINED-ON-LINE '300) 
(PUT 'FL2BFEXP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'FL2BFEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FL2BFEXP (M) (COND (*BFTAG (EXPFL2BF M)) (T (EXP M)))) 
(PUT 'EXPFL2BF 'NUMBER-OF-ARGS 1) 
(PUT 'EXPFL2BF 'DEFINED-ON-LINE '302) 
(PUT 'EXPFL2BF 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'EXPFL2BF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPFL2BF (M)
    (COND ((LESSP M 0) (INVBF (EXPFL2BF (MINUS M))))
          (T
           ((LAMBDA (MI)
              ((LAMBDA (MF)
                 (EXPTBF BFEE* MI
                         ((LAMBDA (X)
                            (COND ((FLOATP X) (FL2BF X))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM X)) X)
                                          ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                          (T (|READ:NUM| X)))))))
                          (EXP MF))))
               (DIFFERENCE M MI)))
            (FIX M))))) 
(PUT 'UNGFFORM 'NUMBER-OF-ARGS 1) 
(PUT 'UNGFFORM 'DEFINED-ON-LINE '305) 
(PUT 'UNGFFORM 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'UNGFFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNGFFORM (P)
    (PROG (R)
      (COND
       ((EQUAL (CAAR P) 0)
        (PROGN
         (COND
          ((NOT
            (COND ((ATOM (CDAR P)) (ZEROP (CDAR P)))
                  (T (EQUAL (CADR (CDAR P)) 0))))
           (SETQ R (CDAR P))))
         (SETQ P (CDR P)))))
      (PROG (I)
        (SETQ I P)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (COND
            ((NOT
              (COND ((ATOM (CDR I)) (ZEROP (CDR I)))
                    (T (EQUAL (CADR (CDR I)) 0))))
             (SETQ R (CONS (CONS (CONS (OR *RVAR 'X) (CAR I)) (CDR I)) R)))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN R))) 
(PUT 'GTAG 'NUMBER-OF-ARGS 1) 
(PUT 'GTAG 'DEFINED-ON-LINE '312) 
(PUT 'GTAG 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GTAG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GTAG (C) (COND ((FIXP C) '|:GI:|) (T '|:CR:|))) 
(PUT 'GZERO 'NUMBER-OF-ARGS 1) 
(PUT 'GZERO 'DEFINED-ON-LINE '314) 
(PUT 'GZERO 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GZERO (C) (COND ((FIXP C) 0) ((FLOATP C) 0.0) (T BFZ*))) 
(PUT 'SIMPGI 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPGI 'DEFINED-ON-LINE '317) 
(PUT 'SIMPGI 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'SIMPGI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPGI (U) (CONS (CONS '|:GI:| U) 1)) 
(PUT '|:GI:| 'SIMPFN 'SIMPGI) 
(PUT 'RLRTNO 'NUMBER-OF-ARGS 1) 
(PUT 'RLRTNO 'DEFINED-ON-LINE '321) 
(PUT 'RLRTNO 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RLRTNO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLRTNO (P)
    (PROGN
     (STURM1 P)
     (SETQ P (DIFFERENCE (SCHINF (MINUS 1)) (SCHINF 1)))
     (SETQ *STRM NIL)
     P)) 
(PUT 'ROOTS 'NUMBER-OF-ARGS 1) 
(PUT 'ROOTS 'DEFINED-ON-LINE '324) 
(PUT 'ROOTS 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'ROOTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOTS (P)
    ((LAMBDA (|FROOT#|) (PROGN (SETQ |LIMS#| NIL) (UNIROOTS (CAR P) 1))) NIL)) 
(PUT 'FIRSTROOT 'NUMBER-OF-ARGS 1) 
(PUT 'FIRSTROOT 'DEFINED-ON-LINE '327) 
(PUT 'FIRSTROOT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'FIRSTROOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIRSTROOT (P)
    ((LAMBDA (|FROOT#|) (PROGN (SETQ |LIMS#| NIL) (UNIROOTS (CAR P) 1))) T)) 
(PUT 'ROOT_VAL 'NUMBER-OF-ARGS 1) 
(PUT 'ROOT_VAL 'DEFINED-ON-LINE '330) 
(PUT 'ROOT_VAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'ROOT_VAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOT_VAL (X)
    ((LAMBDA (*MSG P) ((LAMBDA (|ROOTACC##| |INIPREC#|) (ROOTS X)) P P)) NIL
     (PRECISION 0))) 
(PROG (N)
  (SETQ N '(ROOTS FIRSTROOT ROOT_VAL))
 LAB
  (COND ((NULL N) (RETURN NIL)))
  ((LAMBDA (N) (PUT N 'PSOPFN N)) (CAR N))
  (SETQ N (CDR N))
  (GO LAB)) 
(PUT 'OUTRIL 'NUMBER-OF-ARGS 1) 
(PUT 'OUTRIL 'DEFINED-ON-LINE '338) 
(PUT 'OUTRIL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'OUTRIL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTRIL (P)
    (CONS 'LIST
          (PROG (I FORALL-RESULT FORALL-ENDPTR)
            (SETQ I P)
            (COND ((NULL I) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (I)
                                (CONS 'LIST
                                      (LIST (MK*SQ (CAR I)) (MK*SQ (CDR I)))))
                              (CAR I))
                             NIL)))
           LOOPLABEL
            (SETQ I (CDR I))
            (COND ((NULL I) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (I)
                        (CONS 'LIST (LIST (MK*SQ (CAR I)) (MK*SQ (CDR I)))))
                      (CAR I))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'GFROOTSET 'NUMBER-OF-ARGS 3) 
(PUT 'GFROOTSET 'DEFINED-ON-LINE '341) 
(PUT 'GFROOTSET 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFROOTSET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFROOTSET (P R B)
    (COND
     ((ERRORP
       (SETQ R
               (ERRORSET* (LIST 'GFROOTFIND (MKQUOTE P) (MKQUOTE R))
                          *BACKTRACE)))
      (GFSETMSG R B 'GFROOTFIND))
     (T (CAR R)))) 
(PUT 'GFSETMSG 'NUMBER-OF-ARGS 3) 
(PUT 'GFSETMSG 'DEFINED-ON-LINE '346) 
(PUT 'GFSETMSG 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFSETMSG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFSETMSG (R B N)
    (COND ((SETQ R |EMSG#|) (PROGN (SETQ |EMSG#| NIL) (RERROR 'ROOTS 1 R)))
          (B (RERROR 'ROOTS 2 (LIST N ": error in bfloat computation")))
          (T NIL))) 
(PUT 'SCH 'NUMBER-OF-ARGS 1) 
(PUT 'SCH 'DEFINED-ON-LINE '352) 
(PUT 'SCH 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'SCH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCH (Z)
    (PROG (V V1 R)
      (SETQ R 0)
      (SETQ V (SGN1 (CAR *STRM) Z))
      (COND ((AND (EQUAL V 0) |MLTR#|) (RETURN (SCHPLUS Z))))
      (PROG (Q)
        (SETQ Q (CDR *STRM))
       LAB
        (COND ((NULL Q) (RETURN NIL)))
        ((LAMBDA (Q)
           (PROGN
            (COND
             ((LESSP (TIMES V (SETQ V1 (SGN1 Q Z))) 0) (SETQ R (PLUS R 1))))
            (COND ((NEQ V1 0) (SETQ V V1)))))
         (CAR Q))
        (SETQ Q (CDR Q))
        (GO LAB))
      (RETURN R))) 
(PUT 'GFNEWTSET 'NUMBER-OF-ARGS 5) 
(PUT 'GFNEWTSET 'DEFINED-ON-LINE '361) 
(PUT 'GFNEWTSET 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFNEWTSET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFNEWTSET (N P Y XO B)
    (PROG (Y1)
      (COND ((SETQ B *BFTAG) (GO RET)))
      (COND ((NOT (ATOM (CAR Y))) (GO MBF)))
      (COND
       ((NOT
         (ERRORP
          (SETQ Y1
                  (ERRORSET*
                   (LIST 'GFNS1 N (MKQUOTE P) (MKQUOTE Y) (MKQUOTE XO))
                   *BACKTRACE))))
        (RETURN (CAR Y1))))
     MBF
      (GFSETMSG Y1 B 'GFNEWTON)
      (SETQ P *GFP)
      (SETQ *XO (SETQ XO (GF2BF XO)))
      (SETQ Y (GF2BF Y))
      (SETQ *BFTAG T)
     RET
      (SETQ Y (GFNS1 N P Y XO))
      (SETQ *BFTAG B)
      (RETURN Y))) 
(PUT 'GFNS1 'NUMBER-OF-ARGS 4) 
(PUT 'GFNS1 'DEFINED-ON-LINE '372) 
(PUT 'GFNS1 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFNS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFNS1 (N P Y XO)
    (PROGN
     (SETQ *XO XO)
     (COND (*TRROOT (TRMSG13A N (XNSHIFT Y) (GFVAL P Y))))
     (GFNEWTON P Y 4))) 
(PUT 'GFNEWT 'NUMBER-OF-ARGS 1) 
(PUT 'GFNEWT 'DEFINED-ON-LINE '375) 
(PUT 'GFNEWT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFNEWT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFNEWT (ARGS)
    ((LAMBDA (P R CPX PR)
       ((LAMBDA (|ROOTACC##| |RPREC#|)
          (NRSTROOT (GFFINIT P) R (COND (CPX 0) (T T))))
        PR PR))
     (CAR ARGS) (CADR ARGS) (CADDR ARGS) (PRECISION 0))) 
(PUT 'GFROOT 'NUMBER-OF-ARGS 1) 
(PUT 'GFROOT 'DEFINED-ON-LINE '382) 
(PUT 'GFROOT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFROOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFROOT (ARGS)
    ((LAMBDA (P R CPX PR)
       ((LAMBDA (|ROOTACC##| |RPREC#|)
          (NRSTROOT (GFFINIT P) R (LIST (COND (CPX 0) (T T)))))
        PR PR))
     (CAR ARGS) (CADR ARGS) (CADDR ARGS) (PRECISION 0))) 
(PROG (N)
  (SETQ N '(GFNEWT GFROOT))
 LAB
  (COND ((NULL N) (RETURN NIL)))
  ((LAMBDA (N) (PUT N 'PSOPFN N)) (CAR N))
  (SETQ N (CDR N))
  (GO LAB)) 
(PUT 'UNIVAR 'NUMBER-OF-ARGS 1) 
(PUT 'UNIVAR 'DEFINED-ON-LINE '391) 
(PUT 'UNIVAR 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'UNIVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNIVAR (Y)
    ((LAMBDA (P *MSG)
       (COND (((LAMBDA (U) (OR (ATOM U) (ATOM (CAR U)))) (SETQ Y (CAR P))) 0)
             ((OR (UNIVARIATEP Y)
                  (PROGN
                   (ON (LIST 'COMPLEX))
                   (UNIVARIATEP (SETQ Y (CAR (RESIMP P)))))
                  (PROGN
                   (ON (LIST 'ROUNDED))
                   (UNIVARIATEP (SETQ Y (CAR (RESIMP P))))))
              Y)))
     (SIMP* Y) NIL)) 
(PUT 'CKACC 'NUMBER-OF-ARGS 3) 
(PUT 'CKACC 'DEFINED-ON-LINE '398) 
(PUT 'CKACC 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'CKACC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CKACC (Q P R)
    (COND
     ((NOT
       (AND R (GREATERP (CAAR (LASTPAIR P)) 1)
            (OR (GREATERP |RR#| 1) |PFACTOR#|)))
      R)
     ((EQUAL (CAAR (LASTPAIR Q)) 1)
      (PROGN (SETQ |ACC#| (CDR (SETQ R (ACCUPR1 R Q)))) (CAR R)))
     (T
      (PROG (AC RL S NX)
        (SETQ RL
                (COND ((ATOM (CDR R)) (ZEROP (CDR R)))
                      (T (EQUAL (CADR (CDR R)) 0))))
        (SETQ R (PROGN (COND ((NOT |PFL#|) (ACCUROOT R Q *XOBF))) *XN))
       LOOP
        (SETQ AC (ACCUPR Q P R))
        (COND
         (|PFL#|
          (PROGN
           (COND
            ((GREATERP (SETQ S (DIFFERENCE |ACC#| (SETQ AC (PLUS AC |SS#|))))
                       0)
             (PROGN (SETQ |ACFL#| T) (SETQ |ACCM#| (DIFFERENCE |ACCM#| S)))))
           (SETQ |ACC#| AC)
           (SETQ R (COND (RL (ROOTRND (CAR R))) (T (GFRTRND R))))
           (COND (*TRROOT (TRMSG12A R)))
           (RETURN R)))
         ((GREATERP AC |ACC#|) (PROGN (SETQ |ACC#| AC) (GO GFR))))
        (COND ((OR S (EQUAL |SS#| 0)) (RETURN R)))
        (SETQ S T)
        (SETQ |ACC#| (PLUS |ACC#| |SS#|))
       GFR
        (SETQ NX R)
        (SETQ R (PROGN (ACCUROOT R Q *XOBF) *XN))
        (COND
         ((OR (GFEQP NX R)
              (AND S
                   (NOT
                    (AND RL
                         (NOT
                          (SETQ RL
                                  (COND ((ATOM (CDR R)) (ZEROP (CDR R)))
                                        (T (EQUAL (CADR (CDR R)) 0)))))))))
          (RETURN R))
         (T (GO LOOP))))))) 
(PUT 'GFADJUST 'NUMBER-OF-ARGS 1) 
(PUT 'GFADJUST 'DEFINED-ON-LINE '420) 
(PUT 'GFADJUST 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFADJUST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFADJUST (X)
    (COND
     ((OR *PCMP (NOT *BFTAG) (NOT (|LESSP:| (|ABS:| (CAR X)) |SPREC#|))) X)
     (T
      (COND
       (*BFTAG
        (CONS BFZ*
              (COND ((FLOATP (CDR X)) (FL2BF (CDR X)))
                    (T
                     (NORMBF
                      (COND ((NOT (ATOM (CDR X))) (CDR X))
                            ((FIXP (CDR X)) (CONS '|:RD:| (CONS (CDR X) 0)))
                            (T (|READ:NUM| (CDR X)))))))))
       (T (CONS 0.0 (CFLOT (CDR X)))))))) 
(PUT 'XNSHIFT 'NUMBER-OF-ARGS 1) 
(PUT 'XNSHIFT 'DEFINED-ON-LINE '423) 
(PUT 'XNSHIFT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'XNSHIFT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XNSHIFT (X)
    (COND
     (((LAMBDA (U)
         (COND
          ((NOT (ATOM (CAR U)))
           (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
          (T (EQUAL U '(0.0 . 0.0)))))
       *XO)
      X)
     (T (GFADJUST (GFDIFFER X *XO))))) 
(PUT 'UNSHIFT 'NUMBER-OF-ARGS 1) 
(PUT 'UNSHIFT 'DEFINED-ON-LINE '426) 
(PUT 'UNSHIFT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'UNSHIFT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNSHIFT (X)
    (COND
     (((LAMBDA (U)
         (COND
          ((NOT (ATOM (CAR U)))
           (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
          (T (EQUAL U '(0.0 . 0.0)))))
       *XO)
      X)
     (T (GFADJUST (GFPLUS X *XO))))) 
(PUT 'GFEXIT 'NUMBER-OF-ARGS 4) 
(PUT 'GFEXIT 'DEFINED-ON-LINE '429) 
(PUT 'GFEXIT 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'GFEXIT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFEXIT (PF NX X0 M)
    (COND
     ((COND ((ATOM PF) (ZEROP PF)) (T (EQUAL (CADR PF) 0)))
      (PROGN (COND (*TRROOT (TRMSG1A M NX))) T))
     ((GFEQP (UNSHIFT NX) (UNSHIFT X0))
      (PROGN (COND (*TRROOT (TRMSG3A M NX))) NX))
     (T
      (PROG (RL R1 R0 IM I1 I0)
        (SETQ R1
                ((LAMBDA (X)
                   (COND ((FLOATP X) (FL2BF X))
                         (T
                          (NORMBF
                           (COND ((NOT (ATOM X)) X)
                                 ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                 (T (|READ:NUM| X)))))))
                 (SETQ RL (CAR (SETQ NX (UNSHIFT NX))))))
        (SETQ I1
                ((LAMBDA (X)
                   (COND ((FLOATP X) (FL2BF X))
                         (T
                          (NORMBF
                           (COND ((NOT (ATOM X)) X)
                                 ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                 (T (|READ:NUM| X)))))))
                 (SETQ IM (CDR NX))))
        (SETQ R0
                ((LAMBDA (X)
                   (COND ((FLOATP X) (FL2BF X))
                         (T
                          (NORMBF
                           (COND ((NOT (ATOM X)) X)
                                 ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                 (T (|READ:NUM| X)))))))
                 (CAR (SETQ X0 (UNSHIFT X0)))))
        (SETQ I0
                (COND ((FLOATP (CDR X0)) (FL2BF (CDR X0)))
                      (T
                       (NORMBF
                        (COND ((NOT (ATOM (CDR X0))) (CDR X0))
                              ((FIXP (CDR X0))
                               (CONS '|:RD:| (CONS (CDR X0) 0)))
                              (T (|READ:NUM| (CDR X0))))))))
        (RETURN
         (COND
          ((EQPRTS R1 R0)
           (COND
            ((LESSP (TIMES (CADR I1) (CADR I0)) 0)
             (COND
              (*BFTAG
               (CONS
                (COND ((FLOATP RL) (FL2BF RL))
                      (T
                       (NORMBF
                        (COND ((NOT (ATOM RL)) RL)
                              ((FIXP RL) (CONS '|:RD:| (CONS RL 0)))
                              (T (|READ:NUM| RL))))))
                BFZ*))
              (T (CONS (CFLOT RL) 0.0))))
            ((CVT2 I1 I0)
             (ZPTST
              (COND
               (*BFTAG
                (CONS
                 (COND ((FLOATP RL) (FL2BF RL))
                       (T
                        (NORMBF
                         (COND ((NOT (ATOM RL)) RL)
                               ((FIXP RL) (CONS '|:RD:| (CONS RL 0)))
                               (T (|READ:NUM| RL))))))
                 BFZ*))
               (T (CONS (CFLOT RL) 0.0)))))
            (T NIL)))
          ((EQPRTS I1 I0)
           (COND
            ((LESSP (TIMES (CADR R1) (CADR R0)) 0)
             (COND
              (*BFTAG
               (CONS BFZ*
                     (COND ((FLOATP IM) (FL2BF IM))
                           (T
                            (NORMBF
                             (COND ((NOT (ATOM IM)) IM)
                                   ((FIXP IM) (CONS '|:RD:| (CONS IM 0)))
                                   (T (|READ:NUM| IM))))))))
              (T (CONS 0.0 (CFLOT IM)))))
            ((CVT2 R1 R0)
             (ZPTST
              (COND
               (*BFTAG
                (CONS BFZ*
                      (COND ((FLOATP IM) (FL2BF IM))
                            (T
                             (NORMBF
                              (COND ((NOT (ATOM IM)) IM)
                                    ((FIXP IM) (CONS '|:RD:| (CONS IM 0)))
                                    (T (|READ:NUM| IM))))))))
               (T (CONS 0.0 (CFLOT IM))))))
            (T NIL)))
          (T (PROGN (SETQ *ZP 0) NIL)))))))) 
(PUT 'ZPTST 'NUMBER-OF-ARGS 1) 
(PUT 'ZPTST 'DEFINED-ON-LINE '445) 
(PUT 'ZPTST 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'ZPTST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZPTST (X)
    (COND ((GREATERP *ZP 4) X) (T (PROGN (SETQ *ZP (PLUS *ZP 1)) NIL)))) 
(PUT 'EQPRTS 'NUMBER-OF-ARGS 2) 
(PUT 'EQPRTS 'DEFINED-ON-LINE '448) 
(PUT 'EQPRTS 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'EQPRTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQPRTS (A B)
    (AND (NOT (COND ((ATOM A) (ZEROP A)) (T (EQUAL (CADR A) 0))))
         (OR (|EQUAL:| A B) (CVT5 A B)))) 
(PUT 'POWERCHK 'NUMBER-OF-ARGS 1) 
(PUT 'POWERCHK 'DEFINED-ON-LINE '450) 
(PUT 'POWERCHK 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'POWERCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POWERCHK (P)
    ((LAMBDA (G)
       (PROGN
        (COND
         ((AND *POWERGCD (GREATERP (LENGTH P) 2))
          (PROG (X)
            (SETQ X (CDR P))
           LAB
            (COND ((NULL X) (RETURN NIL)))
            ((LAMBDA (X) (SETQ G (GCDN G (CAR X)))) (CAR X))
            (SETQ X (CDR X))
            (GO LAB))))
        (COND
         ((GREATERP G 1)
          (CONS G
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X P)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (CONS (QUOTIENT (CAR X) G) (CDR X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X) (CONS (QUOTIENT (CAR X) G) (CDR X)))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
     0)) 
(PUT 'RTREORDER 'NUMBER-OF-ARGS 1) 
(PUT 'RTREORDER 'DEFINED-ON-LINE '457) 
(PUT 'RTREORDER 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'RTREORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RTREORDER (CC)
    (COND
     (CC
      (COND
       ((OR (EQCAR (CAAR CC) '|:DN:|)
            (AND
             (NOT
              (OR (NUMBERP (CAAR CC))
                  (AND (EQCAR (CAAR CC) '|:RD:|)
                       (NOT (ATOM (CDR (CAAR CC)))))))
             (EQCAR (CAAAR CC) '|:DN:|)))
        ((LAMBDA (P)
           (PROGN
            (PROG (J)
              (SETQ J CC)
             LAB
              (COND ((NULL J) (RETURN NIL)))
              ((LAMBDA (J) (SETQ P (MAX P (RRSIZ (CAR J))))) (CAR J))
              (SETQ J (CDR J))
              (GO LAB))
            (SETQ PRD% (TIMES 2 P))
            (SORT CC (FUNCTION DNAFTERP))))
         0))
       (T (SORT CC (FUNCTION BFNAFTERP))))))) 
(PUT 'BFNAFTERP 'NUMBER-OF-ARGS 2) 
(PUT 'BFNAFTERP 'DEFINED-ON-LINE '463) 
(PUT 'BFNAFTERP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'BFNAFTERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BFNAFTERP (A B)
    ((LAMBDA (CA CB)
       (COND
        ((OR (NUMBERP CA) (AND (EQCAR CA '|:RD:|) (NOT (ATOM (CDR CA)))))
         (COND
          ((OR (NUMBERP CB) (AND (EQCAR CB '|:RD:|) (NOT (ATOM (CDR CB)))))
           (|RD:MINUSP| (|RD:DIFFERENCE| CB CA)))
          (T
           ((LAMBDA (D)
              (COND ((|RD:ZEROP| D) (|RD:MINUSP| (CDR CB)))
                    (T (|RD:MINUSP| D))))
            (|RD:DIFFERENCE| (CAR CB) CA)))))
        ((OR (NUMBERP CB) (AND (EQCAR CB '|:RD:|) (NOT (ATOM (CDR CB)))))
         ((LAMBDA (D)
            (COND ((|RD:ZEROP| D) (NOT (|RD:MINUSP| (CDR CA))))
                  (T (|RD:MINUSP| D))))
          (|RD:DIFFERENCE| CB (CAR CA))))
        (T
         ((LAMBDA (D)
            (COND
             ((|RD:ZEROP| D) (|RD:MINUSP| (|RD:DIFFERENCE| (CDR CB) (CDR CA))))
             (T (|RD:MINUSP| D))))
          (|RD:DIFFERENCE| (CAR CB) (CAR CA))))))
     (CAR A) (CAR B))) 
(PUT 'DNAFTERP 'NUMBER-OF-ARGS 2) 
(PUT 'DNAFTERP 'DEFINED-ON-LINE '476) 
(PUT 'DNAFTERP 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'DNAFTERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DNAFTERP (A B)
    ((LAMBDA (CA CB)
       (COND
        ((EQCAR CA '|:DN:|)
         (COND ((EQCAR CB '|:DN:|) (DNAFTERP1 CA CB))
               ((DNEQUAL CA (CAR CB)) (LESSP (CADR (CDR CB)) 0))
               (T (DNAFTERP1 CA (CAR CB)))))
        ((EQCAR CB '|:DN:|)
         (COND ((DNEQUAL (CAR CA) CB) (GREATERP (CADR (CDR CA)) 0))
               (T (DNAFTERP1 (CAR CA) CB))))
        (T
         ((LAMBDA (CCA CCB)
            (COND ((DNEQUAL CCA CCB) (DNAFTERP1 (CDR CA) (CDR CB)))
                  (T (DNAFTERP1 CCA CCB))))
          (CAR CA) (CAR CB)))))
     (CAR A) (CAR B))) 
(PUT 'DNEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'DNEQUAL 'DEFINED-ON-LINE '489) 
(PUT 'DNEQUAL 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'DNEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DNEQUAL (A B)
    (OR (AND (EQUAL (CADR A) 0) (EQUAL (CADR B) 0))
        (AND (EQUAL (CDDR A) (CDDR B)) (EQUAL (CADR A) (CADR B))))) 
(PUT 'DNAFTERP1 'NUMBER-OF-ARGS 2) 
(PUT 'DNAFTERP1 'DEFINED-ON-LINE '492) 
(PUT 'DNAFTERP1 'DEFINED-IN-FILE 'ROOTS/BFDOER.RED) 
(PUT 'DNAFTERP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DNAFTERP1 (A B)
    (COND ((EQUAL (CDDR A) (CDDR B)) (GREATERP (CADR A) (CADR B)))
          (T
           ((LAMBDA (D MA MB)
              (COND ((EQUAL D 0) (GREATERP MA MB))
                    ((GREATERP D PRD%) (GREATERP MA 0))
                    ((LESSP D (MINUS PRD%)) (LESSP MB 0))
                    ((GREATERP D 0) (GREATERP (TIMES MA (EXPT 10 D)) MB))
                    (T (GREATERP MA (TIMES MB (EXPT 10 (MINUS D)))))))
            (DIFFERENCE (CDDR A) (CDDR B)) (CADR A) (CADR B))))) 
(ENDMODULE) 