(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BFDOER2)) 
(EXPORTS
 (LIST 'ALLOUT 'AUTOMOD 'BFIXUP 'BFMAX 'CEXPAND 'CKPREC 'CKPZRO 'CSIZE 'FTOUT
       'GF2FLT 'GFFINIT 'GFRTRND 'GFSIMP 'GFSQFRF 'MKDN 'N2GF 'NBFOUT 'NWTERR
       'NWTERRFX 'OUTECHO 'OUTMODE 'PBFPRINT 'PCONSTR 'PFLUPD 'RESTOREFL
       'ROOTPREC 'ROOTRND 'SETEPS 'SMPART 'STUFFR 'TRMSG10A 'TRMSG11A 'TRMSG12A
       'TRMSG13A 'TRMSG1A 'TRMSG2A 'TRMSG3A 'TRMSG4A 'TRMSG6A 'TRMSG7A 'TRMSG8A
       'TRMSG9 'TRMSG17 'XNSIZ)) 
(IMPORTS
 (LIST '*CRN2CR '*F2Q '|ABS:| 'BFLOAT 'BFLOATEM 'BFMIN 'BFMINUS 'BFNUMP 'BFNZP
       '|BFP:| 'BFPLUS 'BFPRIN0 'BFRNDEM 'BFSIZ 'BFTIMES 'BFZP 'CALCPREC
       'CEILING 'CEILLOG 'CFLOT 'CONV2GI2 'CONV2GID 'CPXP 'CRP 'DIVBF 'DMCONV0
       'DMCONV1 'DOMAINP '|EP:| 'EQCAR 'ERRORP 'ERRORSET* 'FIND!NFPD 'FLOOR
       'GETPREC 'GF2BF 'GF2FL 'GFCONJ 'GFFINITR 'GFIM 'GFMINUS 'GFRL 'GZERO
       '|I2BF:| 'LASTPAIR '|LESSP:| 'LISPEVAL 'LISTECHO 'LOG10 'LPRIM
       '|MAKE:IBF| '|MAKE:RD| 'MAXBND1 'MINPREC 'MK*SQ 'MKGIRAT 'MKINTEG
       'MKQUOTE '|MT:| 'MVAR 'NEQ 'NORMBF 'NUMR 'OFF 'ON 'P1RMULT 'PMSG
       '|PRECI:| 'PRECISION 'PRECMSG 'R2BF 'RDP 'RERROR 'REVERSIP 'RL2GF
       '|ROUND:MT| 'RTREORDER 'SETFLBF 'SETPREC 'SIMP* 'SIZATOM 'SQFRF 'SQRT
       'TAGIM 'TAGRL '|TIMES:| 'UNGFFC 'UNGFFORM 'UNIVAR 'UNSHIFT)) 
(FLUID
 '(*TRROOT *ROOTMSG *MULTIROOT *ROUNDBF |:BPREC:| *COMPLEX *MSG *BFTAG *SQFREE
   *RATROOT *NOSQFR |ROOTACC#| |INIPREC#|)) 
(SWITCH (LIST 'TRROOT 'ROOTMSG 'MULTIROOT 'RATROOT)) 
(GLOBAL '(!NFPD !NBFPD !FLIM BFZ* !LOG2OF10 |RLVAL#| |CPVAL#|)) 
(GLOBAL '(!SHBINFL ROOTSREAL ROOTSCOMPLEX DEN* |LM#| !FLPREC)) 
(FLAG '(ROOTSREAL ROOTSCOMPLEX) 'SHARE) 
(FLUID '(*PFSAV |PR#| |ACC#| |BFL#| |EMSG#| |EPS#| |ROOTACC##|)) 
(FLUID '(*XMAX *XMAX2 *RVAR |NWMAX#| |LGMAX#| |NOZRO#| 1RP)) 
(FLUID '(*XO *EXP *PCMP *MULTRT |LGMAX#| |NWMAX#| |RND#| |RNDL#|)) 
(FLUID
 '(*KEEPIMP |SQF#| |EXP#| |SPREC#| |RPREC#| *BFSH INCMSG$ |CPX#| |PFACTOR#|
   |RR#| |PNN#| |NN#| |PRX#| |RLRT#| *NOEQNS)) 
(GLOBAL '(*ROOTORDER)) 
(SETQ *ROOTORDER T) 
(PUT 'GF2FLT 'NUMBER-OF-ARGS 1) 
(PUT 'GF2FLT 'DEFINED-ON-LINE '75) 
(PUT 'GF2FLT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GF2FLT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GF2FLT (A)
    (COND ((OR *ROUNDBF !SHBINFL) A)
          (T
           (PROGN
            ((LAMBDA (GX) (COND ((ERRORP GX) A) (T (CAR GX))))
             (ERRORSET* (LIST 'GF2FL (MKQUOTE A)) NIL)))))) 
(PUT 'GFBFXN 'NUMBER-OF-ARGS 1) 
(PUT 'GFBFXN 'DEFINED-ON-LINE '81) 
(PUT 'GFBFXN 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFBFXN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFBFXN (NX)
    (GF2BF
     (COND
      (*BFSH
       (COND
        ((OR (NUMBERP NX) (AND (EQCAR NX '|:RD:|) (NOT (ATOM (CDR NX)))))
         (BFPLUS NX (CAR *XO)))
        (T (CAR (UNSHIFT NX)))))
      ((OR (NUMBERP NX) (AND (EQCAR NX '|:RD:|) (NOT (ATOM (CDR NX))))) NX)
      (T (UNSHIFT NX))))) 
(PUT 'PRINT_THE_NUMBER 'NUMBER-OF-ARGS 1) 
(PUT 'PRINT_THE_NUMBER 'DEFINED-ON-LINE '86) 
(PUT 'PRINT_THE_NUMBER 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'PRINT_THE_NUMBER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRINT_THE_NUMBER (XN)
    (COND ((ATOM XN) (PROGN (PRIN2 XN) NIL))
          ((NUMBERP (CAR XN))
           (PROGN
            (PROGN (PRIN2 (CAR XN)) NIL)
            (COND ((GEQ (CDR XN) 0) (PROGN (PRIN2 "+") NIL)))
            (PROGN (PRIN2 (CDR XN)) NIL)
            (PROGN (PRIN2 "*I") NIL)))
          ((EQCAR XN '|:RD:|) (BFPRIN0A XN))
          (T
           (PROGN
            (BFPRIN0A (CAR XN))
            (COND ((GEQ (CADR (CDR XN)) 0) (PROGN (PRIN2 "+") NIL)))
            (BFPRIN0A (CDR XN))
            (PROGN (PRIN2 "*I") NIL))))) 
(PUT 'BFPRIN0A 'NUMBER-OF-ARGS 1) 
(PUT 'BFPRIN0A 'DEFINED-ON-LINE '98) 
(PUT 'BFPRIN0A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'BFPRIN0A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFPRIN0A (U) ((LAMBDA (*NAT) (BFPRIN0 U)) NIL)) 
(PUT 'TRMSG1A 'NUMBER-OF-ARGS 2) 
(PUT 'TRMSG1A 'DEFINED-ON-LINE '101) 
(PUT 'TRMSG1A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG1A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRMSG1A (A NX)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 A) (PRIN2 ",px=0 => ") NIL)
       (PRINT_THE_NUMBER (GFBFXN NX))
       (TERPRI))))) 
(PUT 'TRMSG2A 'NUMBER-OF-ARGS 3) 
(PUT 'TRMSG2A 'DEFINED-ON-LINE '108) 
(PUT 'TRMSG2A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG2A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRMSG2A (A XN PX)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 A) (PRIN2 " -> xn=") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TRMSG5 XN PX))))) 
(PUT 'TRMSG3A 'NUMBER-OF-ARGS 2) 
(PUT 'TRMSG3A 'DEFINED-ON-LINE '115) 
(PUT 'TRMSG3A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG3A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRMSG3A (A XN)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 A) (PRIN2 ",xn=x0 => ") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TERPRI))))) 
(PUT 'TRMSG4A 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG4A 'DEFINED-ON-LINE '122) 
(PUT 'TRMSG4A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG4A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG4A (REQ)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "number of ") NIL)
       (COND (|NOZRO#| (PROGN (PRIN2 "nonzero ") NIL)))
       (PROGN (PRIN2 "real roots = ") (PRIN2 REQ) NIL)
       (TERPRI))))) 
(PUT 'TRMSG5 'NUMBER-OF-ARGS 2) 
(PUT 'TRMSG5 'DEFINED-ON-LINE '126) 
(PUT 'TRMSG5 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG5 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRMSG5 (NX PX)
    (PROGN
     (TERPRI)
     (PROGN (PRIN2 "      ") (PRIN2 " px=") NIL)
     (PRINT_THE_NUMBER PX)
     (TERPRI))) 
(PUT 'TRMSG6A 'NUMBER-OF-ARGS 3) 
(PUT 'TRMSG6A 'DEFINED-ON-LINE '129) 
(PUT 'TRMSG6A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG6A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRMSG6A (K XN PX)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "mean of ") (PRIN2 K) (PRIN2 " xn->") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TRMSG5 XN PX))))) 
(PUT 'TRMSG7A 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG7A 'DEFINED-ON-LINE '134) 
(PUT 'TRMSG7A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG7A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG7A (XN)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "best value =") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TERPRI))))) 
(PUT 'TRMSG8A 'NUMBER-OF-ARGS 0) 
(PUT 'TRMSG8A 'DEFINED-ON-LINE '138) 
(PUT 'TRMSG8A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG8A 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TRMSG8A NIL
    (COND
     (*TRROOT
      (PROGN
       (COND
        (*BFTAG
         (PROGN (PRIN2 "Precision is ") (PRIN2 (PLUS 2 (PRECISION 0))) NIL))
        (T (PROGN (PRIN2 "!nfpd = ") (PRIN2 !NFPD) NIL)))
       (TERPRI))))) 
(PUT 'TRMSG9 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG9 'DEFINED-ON-LINE '143) 
(PUT 'TRMSG9 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG9 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG9 (A) (LPRIM (LIST "Roots precision increase to " A))) 
(PUT 'TRMSG10A 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG10A 'DEFINED-ON-LINE '146) 
(PUT 'TRMSG10A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG10A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG10A (A)
    (PROGN
     (LPRIM (LIST "restart at higher precision called by function " A))
     (TERPRI))) 
(PUT 'TRMSG11A 'NUMBER-OF-ARGS 2) 
(PUT 'TRMSG11A 'DEFINED-ON-LINE '150) 
(PUT 'TRMSG11A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG11A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRMSG11A (XN N)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "n=") (PRIN2 N) (PRIN2 " -> ") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TERPRI))))) 
(PUT 'TRMSG12A 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG12A 'DEFINED-ON-LINE '154) 
(PUT 'TRMSG12A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG12A 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG12A (Z)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "acc(") (PRIN2 |ACC#|) (PRIN2 ") ->") NIL)
       (PRINT_THE_NUMBER (OUTTRIM Z))
       (TERPRI))))) 
(PUT 'TRMSG13A 'NUMBER-OF-ARGS 3) 
(PUT 'TRMSG13A 'DEFINED-ON-LINE '158) 
(PUT 'TRMSG13A 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG13A 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TRMSG13A (N XN PX)
    (COND
     (*TRROOT
      (PROGN
       (PROGN (PRIN2 "n=") (PRIN2 N) (PRIN2 ",xn=") NIL)
       (PRINT_THE_NUMBER (GFBFXN XN))
       (TRMSG5 XN PX))))) 
(PUT 'TRMSG17 'NUMBER-OF-ARGS 1) 
(PUT 'TRMSG17 'DEFINED-ON-LINE '163) 
(PUT 'TRMSG17 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'TRMSG17 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRMSG17 (Y) (LPRIM (LIST "Roots " Y " equal to acc " |ACC#|))) 
(PUT 'NWTERR 'NUMBER-OF-ARGS 1) 
(PUT 'NWTERR 'DEFINED-ON-LINE '166) 
(PUT 'NWTERR 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'NWTERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NWTERR (M)
    (COND
     ((GREATERP M (PLUS |NWMAX#| |LM#|))
      (PROGN
       (RESTOREFL)
       (SETQ |EMSG#| (LIST "max NWT limit" (PLUS |NWMAX#| |LM#|) "exceeded"))
       (ERROR 0 |EMSG#|))))) 
(PUT 'NWTERRFX 'NUMBER-OF-ARGS 2) 
(PUT 'NWTERRFX 'DEFINED-ON-LINE '172) 
(PUT 'NWTERRFX 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'NWTERRFX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NWTERRFX (N CP)
    (COND ((LESSP N 3) 0)
          (T
           (FIX
            (TIMES (DIFFERENCE N 2)
                   (SQRT (MAX 0 (PLUS (DIFFERENCE 0 15) (MINPREC))))
                   (COND (CP 4) (T 1))))))) 
(PUT 'SETEPS 'NUMBER-OF-ARGS 0) 
(PUT 'SETEPS 'DEFINED-ON-LINE '176) 
(PUT 'SETEPS 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'SETEPS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SETEPS NIL
    (SETQ |EPS#|
            (CONS '|:RD:|
                  (CONS 1 (MINUS (COND (*BFTAG |:BPREC:|) (T !NBFPD))))))) 
(PUT 'PCONSTR 'NUMBER-OF-ARGS 2) 
(PUT 'PCONSTR 'DEFINED-ON-LINE '179) 
(PUT 'PCONSTR 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'PCONSTR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PCONSTR (M R)
    ((LAMBDA (RL AC)
       (BFLOATEM
        (PRXFLOT (COND (|PRX#| (MAX |PRX#| AC)) (T AC))
         (LIST
          (CONS 0
                (COND
                 (RL
                  (BFMINUS
                   (COND
                    ((OR (NUMBERP R)
                         (AND (EQCAR R '|:RD:|) (NOT (ATOM (CDR R)))))
                     R)
                    (T (CAR R)))))
                 (T (GFMINUS R))))
          (CONS M
                (COND (RL 1.0)
                      (T
                       (COND
                        (*BFTAG
                         (CONS
                          (COND ((FLOATP 1.0) (FL2BF 1.0))
                                (T
                                 (NORMBF
                                  (COND ((NOT (ATOM 1.0)) 1.0)
                                        ((FIXP 1.0)
                                         (CONS '|:RD:| (CONS 1.0 0)))
                                        (T (|READ:NUM| 1.0))))))
                          BFZ*))
                        (T (CONS (CFLOT 1.0) 0.0))))))))))
     (OR (OR (NUMBERP R) (AND (EQCAR R '|:RD:|) (NOT (ATOM (CDR R)))))
         (COND ((ATOM (CDR R)) (ZEROP (CDR R))) (T (EQUAL (CADR (CDR R)) 0))))
     (PLUS |ACC#| 2 (CEILING (LOG10 (FLOAT M)))))) 
(PUT 'PRXFLOT 'NUMBER-OF-ARGS 2) 
(PUT 'PRXFLOT 'DEFINED-ON-LINE '186) 
(PUT 'PRXFLOT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'PRXFLOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PRXFLOT (PR P)
    (PROGN
     (PRECISION1
      (DIFFERENCE
       (COND ((SETQ *BFTAG (OR |RLRT#| (GREATERP PR !NFPD))) PR) (T !NFPD)) 2)
      T)
     (BFRNDEM (BFLOATEM P)))) 
(PUT 'SMPART 'NUMBER-OF-ARGS 1) 
(PUT 'SMPART 'DEFINED-ON-LINE '190) 
(PUT 'SMPART 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'SMPART 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SMPART (Y)
    ((LAMBDA (A B C)
       (COND
        ((AND (GREATERP (CADR A) 0) (GREATERP (CADR B) 0))
         (COND ((|LESSP:| B (|TIMES:| A C)) T)
               ((|LESSP:| A (|TIMES:| B C)) 0)))))
     (|ABS:| (|ROUND:MT| (CAR Y) 20)) (|ABS:| (|ROUND:MT| (CDR Y) 20))
     (CONS '|:RD:| (CONS 1 (MINUS |:BPREC:|))))) 
(PUT 'STUFFR 'NUMBER-OF-ARGS 3) 
(PUT 'STUFFR 'DEFINED-ON-LINE '197) 
(PUT 'STUFFR 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'STUFFR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE STUFFR (N V AR)
    ((LAMBDA (PT)
       (PROGN
        (COND ((AND *ROOTMSG *TRROOT) (PROGN (PROGN (PRIN2 V) NIL) (TERPRI))))
        (PROG ()
         WHILELABEL
          (COND ((NOT (GREATERP N 0)) (RETURN NIL)))
          (PROGN (SETQ PT (CDR PT)) (SETQ N (DIFFERENCE N 1)))
          (GO WHILELABEL))
        (RPLACA PT V)
        AR))
     AR)) 
(PUT 'N2GF 'NUMBER-OF-ARGS 1) 
(PUT 'N2GF 'DEFINED-ON-LINE '201) 
(PUT 'N2GF 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'N2GF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE N2GF (P)
    (COND ((ATOM P) P)
          (T
           (PROG (F N)
             (SETQ N (CAR P))
             (PROG (Y)
               (SETQ Y (CDR P))
              LAB
               (COND ((NULL Y) (RETURN NIL)))
               ((LAMBDA (Y)
                  (PROGN
                   (COND ((AND Y (NEQ Y 0)) (SETQ F (CONS (CONS N Y) F))))
                   (SETQ N (DIFFERENCE N 1))))
                (CAR Y))
               (SETQ Y (CDR Y))
               (GO LAB))
             (RETURN F))))) 
(PUT 'PBFPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'PBFPRINT 'DEFINED-ON-LINE '207) 
(PUT 'PBFPRINT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'PBFPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PBFPRINT (X)
    (PROG (N D C)
      (COND
       ((NOT
         (OR (ATOM (SETQ D (CDAR X)))
             (AND (EQCAR D '|:RD:|) (NOT (ATOM (CDR D))))))
        (PROGN (SETQ C 'COMPLEX) (SETQ D (CAR D)))))
      (COND ((FLOATP D) (SETQ N 'FLOAT)) ((FIXP D) (SETQ N 'FIX)))
      (SETQ X
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P X)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P) (CONS (CAR P) (GF2BF (CDR P))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P) (CONS (CAR P) (GF2BF (CDR P)))) (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (N (SETQ X (CONS N X))))
      (COND (C (SETQ X (CONS C X))))
      (RETURN X))) 
(PUT 'PFLUPD 'NUMBER-OF-ARGS 1) 
(PUT 'PFLUPD 'DEFINED-ON-LINE '215) 
(PUT 'PFLUPD 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'PFLUPD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFLUPD (PF)
    (PROGN
     (SETQ *PFSAV (APPEND (LASTPAIR *PFSAV) (LIST PF)))
     (BFMIN PF (CAR *PFSAV)))) 
(PUT 'ROOTACC 'NUMBER-OF-ARGS 1) 
(PUT 'ROOTACC 'DEFINED-ON-LINE '219) 
(PUT 'ROOTACC 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'ROOTACC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOTACC (N)
    (COND
     ((OR (NULL N) (AND (NUMBERP N) (GEQ (SETQ N (ABS (FIX N))) 6)))
      (SETQ |ROOTACC##| N))
     (T (RERROR 'ROOTS 1 (LIST "nil or numeric input >= 6 required."))))) 
(PUT 'ROOTPREC 'NUMBER-OF-ARGS 1) 
(PUT 'ROOTPREC 'DEFINED-ON-LINE '224) 
(PUT 'ROOTPREC 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'ROOTPREC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOTPREC (N)
    (COND
     ((OR (NULL N)
          (AND (NUMBERP N) (GEQ (SETQ N (ABS (FIX N))) (PLUS !FLPREC 4))))
      (SETQ |RPREC#| N))
     (T
      (RERROR 'ROOTS 2
              (LIST "nil or numeric input >=" (PLUS !FLPREC 4) "required."))))) 
(PUT 'CSIZE 'NUMBER-OF-ARGS 1) 
(PUT 'CSIZE 'DEFINED-ON-LINE '229) 
(PUT 'CSIZE 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CSIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CSIZE (P)
    (PROG (N)
      (SETQ N 0)
      (PROG (C)
        (SETQ C (GFFINITR P))
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C) (SETQ N (MAX N (XNSIZ (CDR C))))) (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (RETURN N))) 
(FLAG '(ROOTACC ROOTPREC CSIZE) 'OPFN) 
(PUT 'CKPZRO 'NUMBER-OF-ARGS 1) 
(PUT 'CKPZRO 'DEFINED-ON-LINE '235) 
(PUT 'CKPZRO 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CKPZRO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CKPZRO (P)
    (PROG (C N)
      (SETQ P (CKPREC P))
      (COND ((NUMBERP (SETQ P (GFFINIT P))) (SETQ P NIL))
            ((ATOM P) (PROGN (SETQ C '(((0) . 1))) (SETQ P NIL)))
            ((ATOM (CAR P)) (SETQ P NIL))
            ((GREATERP (SETQ N (CAAR P)) 0) (GO ZRT)))
      (GO RET)
     ZRT
      (SETQ C (LIST (CONS (LIST 0) N)))
      (COND ((OR (NOT P) (NULL (CDR P))) (PROGN (SETQ P NIL) (GO RET))))
      (SETQ P
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J P)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS (DIFFERENCE (CAR J) N) (CDR J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (CONS (DIFFERENCE (CAR J) N) (CDR J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
     RET
      (SETQ |NOZRO#| T)
      (RETURN (CONS C P)))) 
(PUT 'CKPREC 'NUMBER-OF-ARGS 1) 
(PUT 'CKPREC 'DEFINED-ON-LINE '250) 
(PUT 'CKPREC 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CKPREC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CKPREC (P)
    (PROGN
     (FIND!FLIM)
     (SETQ *RVAR NIL)
     (SETQ ROOTSREAL
             (PROGN
              (SETQ ALGLIST* (CONS NIL NIL))
              (SETQ ROOTSCOMPLEX (PROGN (SETQ ALGLIST* (CONS NIL NIL)) NIL))))
     (SETQ |CPX#| *COMPLEX)
     (SETQ |BFL#| *BFTAG)
     (SETQ |ACC#| (SETQ |ROOTACC#| (OR |ROOTACC##| 6)))
     (SETQ |PR#| (PLUS 2 (PRECISION 0)))
     ((LAMBDA (*MSG)
        (PROGN
         (COND ((SETQ |RND#| *ROUNDED) (OFF (LIST 'ROUNDED))))
         (COND ((SETQ |RNDL#| *ROUNDALL) (OFF (LIST 'ROUNDALL))))))
      NIL)
     (SETQ |SQF#| *SQFREE)
     (SETQ |EXP#| *EXP)
     (SETQ *SQFREE (SETQ *EXP T))
     (COND
      ((NULL (SETQ P (UNIVAR P)))
       (PROGN (RESTOREFL) (RERROR 'ROOTS 3 "Univariate polynomial required"))))
     (COND
      (*ROUNDED
       ((LAMBDA (*MSG)
          (MSGPRI NIL "Polynomial simplified in ROUNDED mode at precision"
                  (PRECISION 0)
                  ":       root locations depend on system precision." NIL))
        T)))
     (PRECISION1
      (DIFFERENCE (MAX (OR |RPREC#| (PLUS !NFPD 2)) (PLUS |ACC#| 2)) 2) T)
     P)) 
(PUT 'RESTOREFL 'NUMBER-OF-ARGS 0) 
(PUT 'RESTOREFL 'DEFINED-ON-LINE '275) 
(PUT 'RESTOREFL 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'RESTOREFL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RESTOREFL NIL
    (PROGN
     (PRECISION1 (DIFFERENCE |PR#| 2) T)
     (SETQ *SQFREE |SQF#|)
     (SETQ *EXP |EXP#|)
     ((LAMBDA (*MSG)
        (PROGN
         (COND ((AND *COMPLEX (NOT |CPX#|)) (OFF (LIST 'COMPLEX))))
         (COND (|RND#| (ON (LIST 'ROUNDED))) (*ROUNDED (OFF (LIST 'ROUNDED))))
         (COND (|RNDL#| (ON (LIST 'ROUNDALL))))))
      NIL)
     (SETQ |NOZRO#|
             (SETQ |PR#|
                     (SETQ |SQF#|
                             (SETQ |EXP#|
                                     (SETQ |CPX#|
                                             (SETQ |RND#|
                                                     (SETQ |RNDL#| NIL))))))))) 
(PUT 'MKEQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'MKEQUAL 'DEFINED-ON-LINE '286) 
(PUT 'MKEQUAL 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'MKEQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKEQUAL (L)
    (CONS 'LIST
          (PROG (Y FORALL-RESULT FORALL-ENDPTR)
            (SETQ Y L)
            (COND ((NULL Y) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (Y)
                                (LIST 'EQUAL (OR *RVAR 'X) (OUTMODE Y)))
                              (CAR Y))
                             NIL)))
           LOOPLABEL
            (SETQ Y (CDR Y))
            (COND ((NULL Y) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (Y) (LIST 'EQUAL (OR *RVAR 'X) (OUTMODE Y)))
                      (CAR Y))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'OUTMODE 'NUMBER-OF-ARGS 1) 
(PUT 'OUTMODE 'DEFINED-ON-LINE '289) 
(PUT 'OUTMODE 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'OUTMODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTMODE (J)
    (COND ((NULL J) J)
          ((AND (OR (NUMBERP J) (AND (EQCAR J '|:RD:|) (NOT (ATOM (CDR J)))))
                (COND ((ATOM J) (ZEROP J)) (T (EQUAL (CADR J) 0))))
           0)
          ((FIXP J) J)
          (T
           (MK*SQ
            (COND (*RATROOT (MKGIRAT J))
                  (T
                   (CONS
                    (COND ((FLOATP J) (CONS '|:RD:| J))
                          ((EQCAR J '|:DN:|)
                           (DECIMAL2INTERNAL (CADR J) (CDDR J)))
                          ((OR (ATOM J) (ATOM (CAR J))) J)
                          ((EQCAR (CAR J) '|:DN:|)
                           (CONS '|:CR:|
                                 (CONS
                                  (CDR (DECIMAL2INTERNAL (CADAR J) (CDDAR J)))
                                  (CDR
                                   (DECIMAL2INTERNAL (CADDR J) (CDDDR J))))))
                          (T
                           (CONS '|:CR:|
                                 (COND
                                  ((AND (EQCAR (CAR J) '|:RD:|)
                                        (NOT (ATOM (CDR (CAR J)))))
                                   (CONS (CDAR J) (CDDR J)))
                                  (T J)))))
                    1))))))) 
(PUT 'ALLOUT 'NUMBER-OF-ARGS 1) 
(PUT 'ALLOUT 'DEFINED-ON-LINE '301) 
(PUT 'ALLOUT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'ALLOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLOUT (C)
    (PROG (RL CMP A)
      (SETQ A 0)
      (SETQ C
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J C)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((AND C (NOT *RATROOT)
             ((LAMBDA (R)
                (AND (PAIRP R)
                     (OR
                      (AND
                       (NOT
                        (OR (NUMBERP R)
                            (AND (EQCAR R '|:RD:|) (NOT (ATOM (CDR R))))))
                       (EQ (CAR R) '|:DN:|))
                      (AND
                       (NOT
                        (OR (NUMBERP (CAR R))
                            (AND (EQCAR (CAR R) '|:RD:|)
                                 (NOT (ATOM (CDR (CAR R)))))))
                       (EQ (CAAR R) '|:DN:|)))))
              (CAR C)))
        (PROG (J)
          (SETQ J C)
         LAB
          (COND ((NULL J) (RETURN NIL)))
          ((LAMBDA (J) (SETQ A (MAX A (RRSIZ J)))) (CAR J))
          (SETQ J (CDR J))
          (GO LAB))))
      (RESTOREFL)
      (PROG (X)
        (SETQ X C)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((OR (ATOM X) (EQCAR X '|:DN:|)) (SETQ RL (CONS X RL)))
                 (T (SETQ CMP (CONS X CMP)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ *MSG T)
      (PRECMSG A)
      (COND
       ((AND (LESSP A (PRECISION 0))
             (GREATERP A (MAX (OR |ROOTACC##| 6) !FLPREC)))
        (MSGPRI NIL "input of these values may require precision >= " A NIL
                NIL)))
      (SETQ *MSG NIL)
      (SETQ C
              (COND
               (*NOEQNS
                (PROGN
                 (SETQ ROOTSCOMPLEX
                         (PROGN
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (SETQ ROOTSREAL
                                  (PROGN (SETQ ALGLIST* (CONS NIL NIL)) NIL))))
                 (CONS 'LIST
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J C)
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (OUTMODE J)) (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (OUTMODE J)) (CAR J)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))
               (T
                (PROGN
                 (SETQ ROOTSCOMPLEX
                         (PROGN
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (MKEQUAL (REVERSIP CMP))))
                 (SETQ ROOTSREAL
                         (PROGN
                          (SETQ ALGLIST* (CONS NIL NIL))
                          (MKEQUAL (REVERSIP RL))))
                 (MKEQUAL C)))))
      (RETURN C))) 
(PUT 'RRSIZ 'NUMBER-OF-ARGS 1) 
(PUT 'RRSIZ 'DEFINED-ON-LINE '329) 
(PUT 'RRSIZ 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'RRSIZ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RRSIZ (U)
    (COND ((NUMBERP U) (LENGTH (EXPLODE (ABS U)))) ((EQ U 'I) 0)
          ((ATOM U) (RRSIZ (SIZATOM U))) ((EQCAR U 'MINUS) (RRSIZ (CADR U)))
          (T
           ((LAMBDA (Y)
              (COND
               ((NOT (ATOM Y))
                (COND ((EQCAR Y '|:DN:|) (MAX (RRSIZ (CAR U)) (RRSIZ (CDR U))))
                      (T (RERROR 'ROOTS 7 "unknown structure"))))
               ((MEMQ Y '(PLUS DIFFERENCE))
                (PROG (R)
                  (SETQ R 0)
                  (PROG (N)
                    (SETQ N (CDR U))
                   LAB
                    (COND ((NULL N) (RETURN NIL)))
                    ((LAMBDA (N) (SETQ R (MAX R (RRSIZ N)))) (CAR N))
                    (SETQ N (CDR N))
                    (GO LAB))
                  (RETURN R)))
               ((MEMQ Y '(TIMES QUOTIENT))
                (PROG (N FORALL-RESULT)
                  (SETQ N (CDR U))
                  (SETQ FORALL-RESULT 0)
                 LAB1
                  (COND ((NULL N) (RETURN FORALL-RESULT)))
                  (SETQ FORALL-RESULT
                          (PLUS ((LAMBDA (N) (RRSIZ N)) (CAR N))
                                FORALL-RESULT))
                  (SETQ N (CDR N))
                  (GO LAB1)))
               ((EQ Y '|:DN:|)
                (LENGTH (EXPLODE (ABS (CAR (NORMDEC (CDR U)))))))
               (T (RERROR 'ROOTS 7 "unknown structure"))))
            (CAR U))))) 
(PUT 'OUTECHO 'NUMBER-OF-ARGS 1) 
(PUT 'OUTECHO 'DEFINED-ON-LINE '347) 
(PUT 'OUTECHO 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'OUTECHO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTECHO (R)
    (ALLOUT
     (PROG (C FORALL-RESULT FORALL-ENDPTR)
       (SETQ C R)
      STARTOVER
       (COND ((NULL C) (RETURN NIL)))
       (SETQ FORALL-RESULT
               ((LAMBDA (C)
                  (LISTECHO (CAR C) (COND (*MULTIROOT (CDR C)) (T 1))))
                (CAR C)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ C (CDR C))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL C) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               ((LAMBDA (C)
                  (LISTECHO (CAR C) (COND (*MULTIROOT (CDR C)) (T 1))))
                (CAR C)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ C (CDR C))
       (GO LOOPLABEL)))) 
(PUT 'FIND!FLIM 'NUMBER-OF-ARGS 0) 
(PUT 'FIND!FLIM 'DEFINED-ON-LINE '351) 
(PUT 'FIND!FLIM 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'FIND!FLIM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FIND!FLIM NIL
    ((LAMBDA (N)
       (PROGN
        (SETQ !FLIM 0)
        (PROG ()
         REPEATLABEL
          (PROGN (SETQ N (QUOTIENT N 10)) (SETQ !FLIM (PLUS !FLIM 1)))
          (COND
           ((NOT (EQUAL (EXPLODE (PLUS 1.0 N)) (EXPLODE 1.0)))
            (GO REPEATLABEL))))
        !FLIM))
     1.0)) 
(PUT 'XNSIZ 'NUMBER-OF-ARGS 1) 
(PUT 'XNSIZ 'DEFINED-ON-LINE '355) 
(PUT 'XNSIZ 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'XNSIZ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XNSIZ (X) (CEILING (QUOTIENT (XNSIZ1 X) !LOG2OF10))) 
(PUT 'XNSIZ1 'NUMBER-OF-ARGS 1) 
(PUT 'XNSIZ1 'DEFINED-ON-LINE '358) 
(PUT 'XNSIZ1 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'XNSIZ1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XNSIZ1 (X)
    (COND
     ((OR (NUMBERP X) (AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))))
      (|MSD:|
       (ABS
        (CADR
         (COND ((FLOATP X) (FL2BF X))
               (T
                (NORMBF
                 (COND ((NOT (ATOM X)) X) ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                       (T (|READ:NUM| X))))))))))
     ((COND ((ATOM (CDR X)) (ZEROP (CDR X))) (T (EQUAL (CADR (CDR X)) 0)))
      (|MSD:|
       (ABS
        (CADR
         (COND ((FLOATP (CAR X)) (FL2BF (CAR X)))
               (T
                (NORMBF
                 (COND ((NOT (ATOM (CAR X))) (CAR X))
                       ((FIXP (CAR X)) (CONS '|:RD:| (CONS (CAR X) 0)))
                       (T (|READ:NUM| (CAR X)))))))))))
     ((COND ((ATOM (CAR X)) (ZEROP (CAR X))) (T (EQUAL (CADR (CAR X)) 0)))
      (|MSD:|
       (ABS
        (CADR
         (COND ((FLOATP (CDR X)) (FL2BF (CDR X)))
               (T
                (NORMBF
                 (COND ((NOT (ATOM (CDR X))) (CDR X))
                       ((FIXP (CDR X)) (CONS '|:RD:| (CONS (CDR X) 0)))
                       (T (|READ:NUM| (CDR X)))))))))))
     (T
      (PROGN
       (SETQ X (GF2BF X))
       ((LAMBDA (E1 E2)
          (DIFFERENCE
           (MAX (PLUS (|MSD:| (ABS (CADR (CAR X)))) E1)
                (PLUS (|MSD:| (ABS (CADR (CDR X)))) E2))
           (MIN E1 E2)))
        (CDDR (CAR X)) (CDDR (CDR X))))))) 
(PUT 'OUTTRIM 'NUMBER-OF-ARGS 1) 
(PUT 'OUTTRIM 'DEFINED-ON-LINE '366) 
(PUT 'OUTTRIM 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'OUTTRIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OUTTRIM (J)
    (COND ((OR *ROUNDBF (GREATERP |ACC#| !FLIM)) (GF2BF J))
          (T
           ((LAMBDA (D) (COND ((ERRORP D) (GF2BF J)) (T (CAR D))))
            (ERRORSET* (LIST 'GF2FL (MKQUOTE J)) NIL))))) 
(PUT 'BFMAX 'NUMBER-OF-ARGS 1) 
(PUT 'BFMAX 'DEFINED-ON-LINE '371) 
(PUT 'BFMAX 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'BFMAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFMAX (P)
    (PROGN
     (SETQ *XMAX (MAXBND1 P))
     ((LAMBDA (M) (SETQ *XMAX2 (BFTIMES M M)))
      (COND
       (*BFTAG
        ((LAMBDA (X)
           (COND ((FLOATP X) (FL2BF X))
                 (T
                  (NORMBF
                   (COND ((NOT (ATOM X)) X)
                         ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                         (T (|READ:NUM| X)))))))
         *XMAX))
       (T (CFLOT *XMAX))))
     *XMAX)) 
(PUT 'NBFOUT 'NUMBER-OF-ARGS 1) 
(PUT 'NBFOUT 'DEFINED-ON-LINE '376) 
(PUT 'NBFOUT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'NBFOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NBFOUT (X)
    ((LAMBDA (X)
       (COND ((FLOATP X) (FL2BF X))
             (T
              (NORMBF
               (COND ((NOT (ATOM X)) X) ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                     (T (|READ:NUM| X)))))))
     (FTOUT X))) 
(PUT 'BFIXUP 'NUMBER-OF-ARGS 1) 
(PUT 'BFIXUP 'DEFINED-ON-LINE '378) 
(PUT 'BFIXUP 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'BFIXUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFIXUP (X) (COND (*BFTAG (GF2BF X)) (T (GF2FL X)))) 
(PUT 'FTOUT 'NUMBER-OF-ARGS 1) 
(PUT 'FTOUT 'DEFINED-ON-LINE '380) 
(PUT 'FTOUT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'FTOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FTOUT (X) (COND ((ATOM X) (CFLOT X)) ((EQCAR X '|:RD:|) (CDR X)) (T X))) 
(FIND!FLIM) 
(PUT 'CEXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'CEXPAND 'DEFINED-ON-LINE '386) 
(PUT 'CEXPAND 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEXPAND (CC)
    (PROG (C)
      (COND (*ROOTORDER (SETQ CC (RTREORDER CC))))
      (PROG (R)
        (SETQ R CC)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (PROGN
            (COND
             ((NOT *PCMP)
              (COND
               ((AND
                 (NOT
                  (OR (NUMBERP (CAR R))
                      (AND (EQCAR (CAR R) '|:RD:|)
                           (NOT (ATOM (CDR (CAR R)))))))
                 (OR (NUMBERP (CAAR R))
                     (AND (EQCAR (CAAR R) '|:RD:|)
                          (NOT (ATOM (CDR (CAAR R)))))))
                (SETQ C (CONS (CONS (GFCONJ (CAR R)) (CDR R)) C)))
               ((AND (NOT (EQCAR (CAR R) '|:DN:|)) (EQCAR (CAAR R) '|:DN:|))
                (SETQ C (CONS (CONS (CDNCONJ (CAR R)) (CDR R)) C))))))
            (SETQ C (CONS R C))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (RETURN C))) 
(PUT 'CDNCONJ 'NUMBER-OF-ARGS 1) 
(PUT 'CDNCONJ 'DEFINED-ON-LINE '398) 
(PUT 'CDNCONJ 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CDNCONJ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CDNCONJ (U)
    (CONS (CAR U) (CONS (CADR U) (CONS (MINUS (CADDR U)) (CDDDR U))))) 
(PUT 'MKDN 'NUMBER-OF-ARGS 1) 
(PUT 'MKDN 'DEFINED-ON-LINE '401) 
(PUT 'MKDN 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'MKDN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKDN (U)
    (COND ((ATOM (CAR U)) (CONS '|:DN:| (NORMDEC U)))
          (T (CONS (MKDN (CAR U)) (MKDN (CDR U)))))) 
(PUT 'NORMDEC 'NUMBER-OF-ARGS 1) 
(PUT 'NORMDEC 'DEFINED-ON-LINE '404) 
(PUT 'NORMDEC 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'NORMDEC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NORMDEC (X)
    (PROG (MT S EP)
      (SETQ EP 0)
      (COND ((EQUAL (SETQ MT (CAR X)) 0) (GO RET)))
      (COND ((LESSP MT 0) (PROGN (SETQ MT (MINUS MT)) (SETQ S T))))
      (SETQ EP (CDR X))
      (SETQ MT (REVERSIP (EXPLODE MT)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQ (CAR MT) '|0|)) (RETURN NIL)))
        (PROGN (SETQ MT (CDR MT)) (SETQ EP (PLUS EP 1)))
        (GO WHILELABEL))
      (SETQ MT (COMPRESS (REVERSIP MT)))
      (COND (S (SETQ MT (MINUS MT))))
     RET
      (RETURN (CONS MT EP)))) 
(PUT 'ROOTRND 'NUMBER-OF-ARGS 1) 
(PUT 'ROOTRND 'DEFINED-ON-LINE '415) 
(PUT 'ROOTRND 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'ROOTRND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ROOTRND (Y) (RTRNDA Y |ACC#|)) 
(PUT 'RTRNDA 'NUMBER-OF-ARGS 2) 
(PUT 'RTRNDA 'DEFINED-ON-LINE '417) 
(PUT 'RTRNDA 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'RTRNDA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RTRNDA (R A)
    (COND
     ((COND ((ATOM R) (ZEROP R)) (T (EQUAL (CADR R) 0)))
      (PROGN (SETQ |RLVAL#| (CONS 0 0)) R))
     (T
      ((LAMBDA (U) (DECIMAL2INTERNAL (CAR (SETQ |RLVAL#| U)) (CDR U)))
       (|ROUND:DEC1| R A))))) 
(PUT 'GFRTRND 'NUMBER-OF-ARGS 1) 
(PUT 'GFRTRND 'DEFINED-ON-LINE '422) 
(PUT 'GFRTRND 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFRTRND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFRTRND (Y)
    ((LAMBDA (A)
       (PROG (RL RLD IM)
         (SETQ Y (CDR Y))
         (SETQ RL (RTRNDA A |ACC#|))
         (SETQ RLD |RLVAL#|)
         (SETQ IM (RTRNDA Y |ACC#|))
         (SETQ |CPVAL#|
                 (COND ((EQUAL (CAR |RLVAL#|) 0) RLD) (T (CONS RLD |RLVAL#|))))
         (RETURN (CONS RL IM))))
     (CAR Y))) 
(PUT 'GFSQFRF 'NUMBER-OF-ARGS 1) 
(PUT 'GFSQFRF 'DEFINED-ON-LINE '430) 
(PUT 'GFSQFRF 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFSQFRF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFSQFRF (P)
    (PROG (M CP Q DMD)
      (COND ((OR (EQUAL (CAAR (LASTPAIR P)) 1) *NOSQFR) (GO NOF)))
      (SETQ CP
              ((LAMBDA (P)
                 (NOT
                  (OR
                   (OR (NUMBERP P)
                       (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                   (OR (NUMBERP (CDAR P))
                       (AND (EQCAR (CDAR P) '|:RD:|)
                            (NOT (ATOM (CDR (CDAR P)))))))))
               (SETQ Q (MKINTEG P))))
      (SETQ DMD DMODE*)
      (COND (*COMPLEX (SETQ DMD (GET DMD 'REALTYPE))))
      (SETQ M *MSG)
      (OFF (LIST 'MSG))
      (COND
       (DMD
        (LISPEVAL (LIST 'OFF (MKQUOTE (LIST (SETQ DMD (GET DMD 'DNAME))))))))
      (SETQ Q (SQFRF (COND (CP (UNGFFC Q)) (T (UNGFFORM Q)))))
      (COND (DMD (LISPEVAL (LIST 'ON (MKQUOTE (LIST DMD))))))
      (COND (M (ON (LIST 'MSG))))
      (COND ((CDR Q) (SETQ |PFACTOR#| T)) ((EQUAL (CDAR Q) 1) (GO NOF)))
      (SETQ 1RP (P1RMULT Q))
      (RETURN Q)
     NOF
      (SETQ Q (LIST (CONS P 1)))
      (SETQ 1RP P)
      (RETURN Q))) 
(PUT 'AUTOMOD 'NUMBER-OF-ARGS 1) 
(PUT 'AUTOMOD 'DEFINED-ON-LINE '443) 
(PUT 'AUTOMOD 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'AUTOMOD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE AUTOMOD (P)
    (COND
     (((LAMBDA (P)
         (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P))))))
       (SETQ P (GFFINIT P)))
      P)
     (T
      (PROG (N S S2 A D M NL PR NC DD)
        (SETQ N 0)
        (SETQ S 0)
        (SETQ S2 0)
        (SETQ |RR#| 0)
        (COND
         ((NULL (CDR P)) (PROGN (SETQ N (PLUS 2 (PRECISION 0))) (GO SEL))))
        (SETQ M (CAR (SETQ D (CAR (LASTPAIR (SETQ P (BFLOATEM P)))))))
        (SETQ D (CDR D))
        (PROG (C)
          (SETQ C (CDR (REVERSE P)))
         LAB
          (COND ((NULL C) (RETURN NIL)))
          ((LAMBDA (C) (SETQ N (MAX N (XNSIZ (CDR C))))) (CAR C))
          (SETQ C (CDR C))
          (GO LAB))
        (SETQ PR (PLUS 2 (PRECISION 0)))
        (PRECISION1
         (DIFFERENCE
          (COND
           ((OR
             (AND
              (SETQ NC
                      (OR (NUMBERP D)
                          (AND (EQCAR D '|:RD:|) (NOT (ATOM (CDR D))))))
              (EQUAL (ABS (CADR D)) 1))
             (AND (NOT NC)
                  (OR
                   (AND (EQUAL (SETQ A (CADR (CAR D))) 0)
                        (EQUAL (ABS (SETQ DD (CADR (CDR D)))) 1))
                   (AND (EQUAL DD 0) (EQUAL (ABS A) 1)))))
            N)
           (T (PLUS 2 (MAX N (XNSIZ D)))))
          2)
         T)
        (SETQ N 0)
        (SETQ NL
                (PROG (C FORALL-RESULT FORALL-ENDPTR)
                  (SETQ C (CDR (REVERSE P)))
                  (COND ((NULL C) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (C) (XNSIZ (CDR C))) (CAR C))
                                        NIL)))
                 LOOPLABEL
                  (SETQ C (CDR C))
                  (COND ((NULL C) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (C) (XNSIZ (CDR C))) (CAR C)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROG (C)
          (SETQ C NL)
         LAB
          (COND ((NULL C) (RETURN NIL)))
          ((LAMBDA (C)
             (PROGN
              (SETQ |RR#| (PLUS |RR#| 1))
              (SETQ S (PLUS S C))
              (SETQ S2 (PLUS S2 (TIMES C C)))
              (SETQ N (MAX N C))))
           (CAR C))
          (SETQ C (CDR C))
          (GO LAB))
        (SETQ N
                (CALCPREC M (SETQ |NN#| N) |RR#| (QUOTIENT (FLOAT S) |RR#|)
                 (COND
                  ((GREATERP N 1)
                   (QUOTIENT (FLOAT S2) (TIMES 2 N (DIFFERENCE N 1))))
                  (T 0))))
        (COND (|RPREC#| (SETQ N (MAX N |RPREC#|))))
        (SETQ |PNN#| N)
        (COND ((OR (GREATERP N !NFPD) *ROUNDBF) (GO BFL)))
        (SETQ *BFTAG NIL)
        (PRECISION1 (DIFFERENCE PR 2) T)
       CFL
        (COND ((ERRORP (ERRORSET* (LIST 'CFLOTEM (MKQUOTE P)) NIL)) (GO BFL))
              (T (RETURN P)))
       SEL
        (COND ((NOT *BFTAG) (GO CFL)))
       BFL
        (SETQ *BFTAG T)
        (PRECISION1 (DIFFERENCE N 2) T)
        (RETURN P))))) 
(PUT 'GFFINIT 'NUMBER-OF-ARGS 1) 
(PUT 'GFFINIT 'DEFINED-ON-LINE '472) 
(PUT 'GFFINIT 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFFINIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFFINIT (P)
    (COND ((AND (NOT (OR (ATOM P) (ATOM (CAR P)))) (NUMBERP (CAAR P))) P)
          ((OR (NUMBERP P) (AND (NOT (ATOM P)) (MEMBER (CAR P) DOMAINLIST*)))
           0)
          (T
           (PROG (*MSG CP)
             (SETQ CP *COMPLEX)
             (ON (LIST 'COMPLEX))
             (SETQ P (GFFORM P))
             (COND ((NOT CP) (OFF (LIST 'COMPLEX))))
             (RETURN (REFORMUP P)))))) 
(PUT 'CLRDENOM 'NUMBER-OF-ARGS 1) 
(PUT 'CLRDENOM 'DEFINED-ON-LINE '482) 
(PUT 'CLRDENOM 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'CLRDENOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLRDENOM (P)
    (PROGN
     (DMCONV0 (COND (*COMPLEX '|:CRN:|) (T '|:RN:|)))
     (SETQ DEN* (CONV2GID (SETQ P (DMCONV1 P)) 1))
     (CONV2GI2 P))) 
(PUT 'GFFORM 'NUMBER-OF-ARGS 1) 
(PUT 'GFFORM 'DEFINED-ON-LINE '486) 
(PUT 'GFFORM 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFFORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFFORM (P)
    (COND ((OR (ATOM P) (ATOM (CAR P))) 0) ((ATOM (CAAR P)) P)
          (T
           (PROG (Q)
             (SETQ *RVAR (CAAAR P))
             (SETQ P (CLRDENOM P))
            LOOP
             (COND
              ((CDAR P) (SETQ Q (CONS (CONS (CDAAR P) (GFSIMP (CDAR P))) Q))))
             (COND ((NULL (SETQ P (CDR P))) (RETURN Q))
                   ((OR (ATOM P) (ATOM (CAR P)))
                    (PROGN (SETQ Q (CONS (CONS 0 (GFSIMP P)) Q)) (RETURN Q)))
                   (T (GO LOOP))))))) 
(PUT 'GFSIMP 'NUMBER-OF-ARGS 1) 
(PUT 'GFSIMP 'DEFINED-ON-LINE '494) 
(PUT 'GFSIMP 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'GFSIMP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFSIMP (U)
    (COND
     ((OR (OR (NUMBERP U) (AND (EQCAR U '|:RD:|) (NOT (ATOM (CDR U)))))
          (EQCAR U '|:RD:|))
      U)
     ((EQCAR U '|:RN:|) (R2BF (CDR U)))
     (T
      (PROGN
       (COND ((EQCAR U '|:CRN:|) (SETQ U (*CRN2CR U))))
       (SETQ U
               (COND
                ((EQCAR U '|:CR:|)
                 (CONS (CONS '|:RD:| (CADR U)) (CONS '|:RD:| (CDDR U))))
                ((EQCAR U '|:GI:|)
                 (CONS (NORMBF (CONS '|:RD:| (CONS (CADR U) 0)))
                       (NORMBF (CONS '|:RD:| (CONS (CDDR U) 0)))))
                (T (CDR U))))
       (COND
        ((COND ((ATOM (CDR U)) (ZEROP (CDR U))) (T (EQUAL (CADR (CDR U)) 0)))
         (CAR U))
        (T U)))))) 
(PUT 'REFORMUP 'NUMBER-OF-ARGS 1) 
(PUT 'REFORMUP 'DEFINED-ON-LINE '507) 
(PUT 'REFORMUP 'DEFINED-IN-FILE 'ROOTS/BFDOER2.RED) 
(PUT 'REFORMUP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REFORMUP (Q)
    (COND ((OR (ATOM Q) (ATOM (CAR Q))) Q)
          (T
           (PROG (C FG D N)
             (SETQ N 0)
             (PROG (V)
               (SETQ V Q)
              LAB
               (COND ((NULL V) (RETURN NIL)))
               ((LAMBDA (V)
                  (PROGN
                   (SETQ V (CDR V))
                   (SETQ N (MAX N (XNSIZ V)))
                   (COND
                    ((OR (FLOATP V)
                         (AND (EQCAR V '|:RD:|) (NOT (ATOM (CDR V)))))
                     (SETQ C (GZERO V)))
                    ((NOT (ATOM V))
                     (PROGN
                      (SETQ FG T)
                      (COND
                       ((NOT (FIXP (SETQ V (CAR V)))) (SETQ C (GZERO V)))))))))
                (CAR V))
               (SETQ V (CDR V))
               (GO LAB))
             (COND
              (FG
               (PROGN
                (SETQ Q
                        (PROG (V FORALL-RESULT FORALL-ENDPTR)
                          (SETQ V Q)
                          (COND ((NULL V) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (V)
                                              (CONS (CAR V)
                                                    (PROGN
                                                     (SETQ D (CDR V))
                                                     (COND
                                                      ((OR (NUMBERP D)
                                                           (AND
                                                            (EQCAR D '|:RD:|)
                                                            (NOT
                                                             (ATOM (CDR D)))))
                                                       (CONS D 0))
                                                      (T D)))))
                                            (CAR V))
                                           NIL)))
                         LOOPLABEL
                          (SETQ V (CDR V))
                          (COND ((NULL V) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (V)
                                      (CONS (CAR V)
                                            (PROGN
                                             (SETQ D (CDR V))
                                             (COND
                                              ((OR (NUMBERP D)
                                                   (AND (EQCAR D '|:RD:|)
                                                        (NOT (ATOM (CDR D)))))
                                               (CONS D 0))
                                              (T D)))))
                                    (CAR V))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ D Q)
                (PROG ()
                 REPEATLABEL
                  (COND
                   (((LAMBDA (U)
                       (NOT
                        (COND ((ATOM U) (ZEROP U)) (T (EQUAL (CADR U) 0)))))
                     (CADAR D))
                    (SETQ FG NIL)))
                  (COND
                   ((NOT (OR (NOT FG) (NULL (SETQ D (CDR D)))))
                    (GO REPEATLABEL))))
                (COND
                 (FG
                  (SETQ Q
                          (PROG (V FORALL-RESULT FORALL-ENDPTR)
                            (SETQ V Q)
                            (COND ((NULL V) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (V)
                                                (CONS (CAR V) (CDDR V)))
                                              (CAR V))
                                             NIL)))
                           LOOPLABEL
                            (SETQ V (CDR V))
                            (COND ((NULL V) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (V) (CONS (CAR V) (CDDR V)))
                                      (CAR V))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))))))
             (COND
              ((OR (AND (EQCAR C '|:RD:|) (NOT (ATOM (CDR C))))
                   (GREATERP N !NFPD))
               (PROGN (SETQ Q (BFLOATEM Q)) (SETQ *BFTAG T)))
              ((FLOATP C) (PROGN (SETQ Q (BFLOATEM Q)) (SETQ *BFTAG NIL))))
             (COND
              ((GREATERP (PLUS N 2) (PLUS 2 (PRECISION 0)))
               (PRECISION1 (DIFFERENCE (PLUS N 2) 2) T)))
             (RETURN Q))))) 
(ENDMODULE) 