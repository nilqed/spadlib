(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REALROOT)) 
(EXPORTS
 (LIST 'ACCUPR1 'BFNEWTON 'ISOLATEP 'SCHINF 'SCHPLUS 'SGN1 'STURM 'STURM0
       'UNIROOTS)) 
(IMPORTS
 (LIST '!MFEFIX '|ABS:| 'ACCUPR 'ACCUROOT 'ALLROOTS 'AUTOMOD 'BDSTEST 'BFABS
       'BFDIVIDE 'BFEQP 'BFLEQP 'BFLOAT 'BFLOATEM 'BFMAX 'BFMINUS 'BFMINUSP
       'BFPLUS 'BFRLMULT 'BFSGN 'BFSQRT 'BFZP 'CEILLOG 'CKPZRO 'CPXP 'CSEP
       'DIFBF 'DIVBF 'DOMAINP 'DSPLY 'EQCAR '|EQUAL:| 'ERRACH 'GEQ 'GETPREC
       'GFDIFF 'GFFINITR 'GFGETMIN 'GFRL 'GFROOTFIND 'GFSQFRF 'GFSTORVAL
       '|GREATERP:| 'LASTPAIR 'LEQ 'LPRIM 'MINBND1 'MINPREC 'MK*SQ 'MULTROOT
       'NEQ 'NWTERR 'NWTERRFX 'OUTECHO 'PCONSTR 'PLUBF 'POWERCHK 'R2BF 'R2FLBF
       'RATDIF 'RATLEQP 'RATLESSP 'RATMAX 'RATMEAN 'RATMIN 'RATMINUS 'RATPLUS
       'REALRAT 'RERROR 'RL2GF 'RLVAL '|ROUND:MT| 'SCH 'SCHNOK 'SETPREC 'SGN
       'STUFFR 'STURM1 'TIMBF 'TRMSG1 'TRMSG10 'TRMSG2 'TRMSG3 'TRMSG4 'TRMSG6
       'TRMSG7 'TRMSG8 'XCLP)) 
(GLOBAL '(!NFPD !FLIM BFHALF* MAX-ACC-INCR BFONE* |RLVAL#|)) 
(FLUID
 '(*GFP *XNLIST *INTP |THT#| *STRM |LIMS#| |MLTR#| |PFACTOR#| |PREC#| *RVAR
   |ACC#| *XO 1RP |ACCM#| *XN |INTV#| |SH#| |RPREC#| |RLRT#| |PRM#| |PFL#|
   |ACFL#| |PGCD#|)) 
(FLUID '(*TRROOT *BFTAG *COMPXROOTS *MSG)) 
(FLAG '(POSITIVE NEGATIVE INFINITY) 'RESERVED) 
(GLOBAL '(|LIMLST#| |LM#|)) 
(SETQ |LIMLST#| '(POSITIVE NEGATIVE INFINITY (MINUS INFINITY))) 
(PUT 'ISOLATEP 'NUMBER-OF-ARGS 1) 
(PUT 'ISOLATEP 'DEFINED-ON-LINE '70) 
(PUT 'ISOLATEP 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'ISOLATEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISOLATEP (P)
    (PROG (N Q ZR A B C RIL VA VB VC V0 W ELEM L U I J LR UR XCLI XCLJ OL OU)
      (COND
       ((OR (NULL (STURM P))
            (EQUAL (DIFFERENCE (SCHINF (MINUS 1)) (SCHINF 1)) 0))
        (GO RET)))
      (SETQ N (CAR (SETQ Q (CAR *STRM))))
      (SETQ L
              (RATMINUS
               (SETQ U
                       (REALRAT
                        ((LAMBDA (G127)
                           (COND ((ATOM G127) (TIMES 1.0001 G127))
                                 (T
                                  (NORMBF
                                   (|ROUND:MT|
                                    (|TIMES:|
                                     (COND ((FLOATP 1.0001) (FL2BF 1.0001))
                                           (T
                                            (NORMBF
                                             (COND ((NOT (ATOM 1.0001)) 1.0001)
                                                   ((FIXP 1.0001)
                                                    (CONS '|:RD:|
                                                          (CONS 1.0001 0)))
                                                   (T (|READ:NUM| 1.0001))))))
                                     G127)
                                    |:BPREC:|)))))
                         (BFMAX P))))))
      (COND ((AND (SETQ ZR (EQUAL L U)) (SETQ LR L) (NOT |LIMS#|)) (GO ZRT)))
      (COND
       (|LIMS#|
        (PROGN
         (SETQ I (CAR |LIMS#|))
         (COND
          ((CDR |LIMS#|)
           (PROGN
            (SETQ J (CADR |LIMS#|))
            (COND ((EQ I 'MINFTY) (SETQ XCLI T))
                  (T
                   (PROGN
                    (COND
                     ((EQCAR I 'LIST) (PROGN (SETQ XCLI T) (SETQ I (CDR I)))))
                    (SETQ L (RATMAX L I)))))
            (COND ((EQ J 'INFTY) (SETQ XCLJ T))
                  (T
                   (PROGN
                    (COND
                     ((EQCAR J 'LIST) (PROGN (SETQ XCLJ T) (SETQ J (CDR J)))))
                    (SETQ U (RATMIN U J)))))
            (COND (ZR (COND ((RATLESSP U L) (GO RET)) (T (GO ZRT)))))
            (COND
             ((EQUAL (SGN1 Q L) 0)
              (PROGN
               (SETQ OL (OFFSETR N L))
               (COND (XCLI (SETQ L (RATPLUS L OL))) (T (SETQ LR L))))))
            (COND
             ((AND (NEQ L U) (EQUAL (SGN1 Q U) 0))
              (PROGN
               (SETQ OU (OFFSETR N U))
               (COND (XCLJ (SETQ U (RATDIF U OU))) (T (SETQ UR U))))))))
          (ZR (GO RET))
          (T
           (PROGN
            (COND
             ((EQUAL (SGN1 Q (SETQ OL (REALRAT 0))) 0)
              (SETQ OL (OFFSETR N OL))))
            (COND ((LESSP I 0) (SETQ U (RATMINUS OL))) (T (SETQ L OL)))))))))
      (SETQ N
              (DIFFERENCE (SETQ VA (PLUS (SCH L) (COND (LR 1) (T 0))))
                          (SETQ VB (SCH U))))
      (COND (*TRROOT (TRMSG4A N)))
      (COND ((EQUAL N 0) (GO RET)))
      (COND ((EQUAL N 1) (SETQ RIL (LIST (LIST L U))))
            (T
             (PROG (J)
               (SETQ J 1)
              LAB
               (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
               (SETQ RIL (CONS NIL RIL))
               (SETQ J (PLUS2 J 1))
               (GO LAB))))
      (SETQ V0 (PLUS VB (DIFFERENCE N 1)))
      (COND
       (LR
        (PROGN
         (STUFFRT L U LR OL V0 VA VB NIL RIL)
         (SETQ L (RATPLUS LR OL))
         (SETQ VA (DIFFERENCE VA 1)))))
      (COND
       (UR
        (PROGN
         (STUFFRT L U UR OU V0 VA VB NIL RIL)
         (SETQ U (RATDIF UR OU))
         (SETQ VB (PLUS VB 1)))))
      (SETQ W (LIST (LIST L U VA VB)))
      (COND
       ((GREATERP N 1)
        (PROG ()
         WHILELABEL
          (COND ((NOT W) (RETURN NIL)))
          (PROGN
           (SETQ ELEM (CAR W))
           (SETQ W (CDR W))
           (SETQ A (CAR ELEM))
           (SETQ B (CADR ELEM))
           (SETQ VA (CADDR ELEM))
           (SETQ VB (CADDDR ELEM))
           (SETQ C (RATMEAN A B))
           (COND
            ((EQUAL (SGN1 Q C) 0)
             (SETQ W (STUFFRT A B C (OFFSETR N C) V0 VA VB W RIL)))
            (T
             (PROGN
              (SETQ VC (SCH C))
              (COND
               ((EQUAL VA (PLUS VC 1))
                (PROGN (STUFFR (DIFFERENCE V0 VC) (LIST A C) RIL))))
              (COND
               ((GREATERP VA (PLUS VC 1)) (SETQ W (CONS (LIST A C VA VC) W))))
              (COND
               ((EQUAL VB (DIFFERENCE VC 1))
                (PROGN (STUFFR (DIFFERENCE V0 VB) (LIST C B) RIL))))
              (COND
               ((LESSP VB (DIFFERENCE VC 1))
                (SETQ W (CONS (LIST C B VC VB) W))))))))
          (GO WHILELABEL))))
      (SETQ RIL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I RIL)
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I) (CONS (CAR I) (CADR I))) (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (I) (CONS (CAR I) (CADR I))) (CAR I))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
     RET
      (RETURN RIL)
     ZRT
      (RETURN (LIST (CONS LR LR))))) 
(PUT 'STUFFRT 'NUMBER-OF-ARGS 9) 
(PUT 'STUFFRT 'DEFINED-ON-LINE '127) 
(PUT 'STUFFRT 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'STUFFRT 'PROCEDURE_TYPE
     '(ARROW
       (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL
              GENERAL)
       GENERAL)) 
(DE STUFFRT (A B C M V0 VA VB W RIL)
    (PROG (VCM VCP)
      (SETQ VCM (PLUS 1 (SETQ VCP (SCH (RATPLUS C M)))))
      (STUFFR (DIFFERENCE V0 VCP) (LIST C C) RIL)
      (COND
       ((EQUAL VA (PLUS VCM 1))
        (STUFFR (DIFFERENCE V0 VCM) (LIST A (RATDIF C M)) RIL)))
      (COND
       ((GREATERP VA (PLUS VCM 1))
        (SETQ W (CONS (LIST A (RATDIF C M) VA VCM) W))))
      (COND
       ((EQUAL VB (DIFFERENCE VCP 1))
        (STUFFR (DIFFERENCE V0 VB) (LIST (RATPLUS C M) B) RIL)))
      (COND
       ((LESSP VB (DIFFERENCE VCP 1))
        (SETQ W (CONS (LIST (RATPLUS C M) B VCP VB) W))))
      (RETURN W))) 
(PUT 'OFFSETR 'NUMBER-OF-ARGS 2) 
(PUT 'OFFSETR 'DEFINED-ON-LINE '137) 
(PUT 'OFFSETR 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'OFFSETR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFFSETR (N R) (REALRAT (COND ((EQUAL N 1) 1) (T (MINBND1 *GFP (MK*SQ R)))))) 
(PUT 'STURM 'NUMBER-OF-ARGS 1) 
(PUT 'STURM 'DEFINED-ON-LINE '140) 
(PUT 'STURM 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'STURM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STURM (P)
    (PROGN
     (COND
      (((LAMBDA (P)
          (NOT
           (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
               (OR (NUMBERP (CDAR P))
                   (AND (EQCAR (CDAR P) '|:RD:|)
                        (NOT (ATOM (CDR (CDAR P)))))))))
        (SETQ P (GFFINITR P)))
       (PROGN
        (SETQ P (CAR (CSEP P)))
        (COND ((NOT (ATOM P)) (SETQ P (BFLOATEM P)))))))
     (COND ((NOT (ATOM P)) (STURM1 (SETQ *GFP P)))))) 
(PUT 'STURM 'PSOPFN 'STURM0) 
(PUT 'STURM0 'NUMBER-OF-ARGS 1) 
(PUT 'STURM0 'DEFINED-ON-LINE '147) 
(PUT 'STURM0 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'STURM0 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STURM0 (P)
    (PROGN
     (SETQ P (STURM (CKPREC (CAR P))))
     (RESTOREFL)
     (CONS 'LIST
           (PROG (A FORALL-RESULT FORALL-ENDPTR)
             (SETQ A P)
             (COND ((NULL A) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (A)
                                 (COND ((ATOM A) A) (T (CONS 'LIST A))))
                               (CAR A))
                              NIL)))
            LOOPLABEL
             (SETQ A (CDR A))
             (COND ((NULL A) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (A) (COND ((ATOM A) A) (T (CONS 'LIST A))))
                       (CAR A))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'SGN1 'NUMBER-OF-ARGS 2) 
(PUT 'SGN1 'DEFINED-ON-LINE '151) 
(PUT 'SGN1 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'SGN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SGN1 (P R)
    (COND ((ATOM P) (SGN P))
          (T
           (PROG (M C U D)
             (SETQ U (CAR R))
             (SETQ D (CDR R))
             (SETQ C 0)
             (SETQ M 1)
             (SETQ P (CDR P))
             (PROG ()
              REPEATLABEL
               (PROGN
                (SETQ C (PLUS (TIMES M (CAR P)) (TIMES U C)))
                (SETQ M (TIMES M D)))
               (COND ((NOT (NULL (SETQ P (CDR P)))) (GO REPEATLABEL))))
             (RETURN (SGN C)))))) 
(PUT 'R2FLIMBF 'NUMBER-OF-ARGS 1) 
(PUT 'R2FLIMBF 'DEFINED-ON-LINE '158) 
(PUT 'R2FLIMBF 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'R2FLIMBF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2FLIMBF (X) (COND ((LEQ |ACC#| !FLIM) (R2FLBF X)) (T (R2BF X)))) 
(PUT 'ROOTFIND 'NUMBER-OF-ARGS 2) 
(PUT 'ROOTFIND 'DEFINED-ON-LINE '161) 
(PUT 'ROOTFIND 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'ROOTFIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ROOTFIND (P I)
    (PROG (P1 P2 PX X1 X2 X3 X0 NX XR FG N S SH XD XE QT XNLIST PF PF0 M
           |THT#|)
      (SETQ M 0)
      (SETQ |THT#| 0)
      (SETQ N (CAAR (LASTPAIR P)))
      (SETQ *XNLIST NIL)
      (COND
       ((EQUAL (CAR I) (CDR I)) (PROGN (SETQ NX (R2FLBF (CDR I))) (GO LG4))))
      (SETQ XR (RATMEAN (CAR I) (CDR I)))
      (COND
       (*TRROOT
        (PROGN
         (PROGN
          (PRIN2 "real root ")
          (PRIN2 (LIST (R2FLIMBF (CAR I)) (R2FLIMBF (CDR I))))
          NIL)
         (TERPRI))))
      (COND (*TRROOT (TRMSG8A)))
      (COND
       ((RATLESSP (CDR I) (CAR I))
        (ERRACH "lx > hx ***error in roots package")))
      (MOVEBDS I XR (SETQ |SH#| (SETQ SH (SGN1 *INTP (CDR I)))))
      (SETQ P2 (GFDIFF (SETQ P1 (GFDIFF P))))
     LAG0
      (COND ((BNDTST (SETQ PX (RLVAL P (SETQ NX (R2FLBF XR))))) (GO THT))
            ((COND ((ATOM PX) (ZEROP PX)) (T (EQUAL (CADR PX) 0))) (GO LG4)))
     LAG
      (COND
       ((OR (BNDTST (SETQ X1 (RLVAL P1 NX))) (NEQ (SETQ S (BFSGN X1)) SH))
        (GO THT)))
      (SETQ PF (COND ((ATOM PX) (ABS PX)) (T (|ABS:| PX))))
      (COND ((AND PF0 (BFLEQP PF0 PF)) (GO NEWT)))
      (GFSTORVAL PF NX)
      (SETQ X1 (COND ((ATOM X1) (ABS X1)) (T (|ABS:| X1))))
      (COND ((BNDTST (SETQ X3 (RLVAL P2 NX))) (GO THT)))
      (COND
       ((AND FG
             (PROGN
              (SETQ QT (NORMBF (|DIVIDE:| PX X1 |:BPREC:|)))
              (SETQ XE
                      (NORMBF
                       (|ROUND:MT|
                        (|TIMES:| QT
                                  (NORMBF
                                   (|ROUND:MT|
                                    (|TIMES:| QT
                                              (NORMBF
                                               (|DIVIDE:| X3 X1 |:BPREC:|)))
                                    |:BPREC:|)))
                        |:BPREC:|)))
              (|EQUAL:| NX
                        (PLUBF NX
                               (NORMBF
                                (|ROUND:MT| (|TIMES:| BFHALF* XE)
                                            |:BPREC:|))))))
        (GO NEWT)))
      (SETQ X2
              (DIFBF
               ((LAMBDA (G128 G129)
                  (COND ((ATOM G129) (TIMES G128 G129))
                        (T
                         (NORMBF
                          (|ROUND:MT|
                           (|TIMES:|
                            (COND ((FLOATP G128) (FL2BF G128))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM G128)) G128)
                                          ((FIXP G128)
                                           (CONS '|:RD:| (CONS G128 0)))
                                          (T (|READ:NUM| G128))))))
                            G129)
                           |:BPREC:|)))))
                (DIFFERENCE N 1.0)
                (NORMBF (|ROUND:MT| (|TIMES:| X1 X1) |:BPREC:|)))
               ((LAMBDA (G131)
                  (COND ((ATOM G131) (TIMES N G131))
                        (T
                         (NORMBF
                          (|ROUND:MT|
                           (|TIMES:|
                            (COND ((FLOATP N) (FL2BF N))
                                  (T
                                   (NORMBF
                                    (COND ((NOT (ATOM N)) N)
                                          ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                                          (T (|READ:NUM| N))))))
                            G131)
                           |:BPREC:|)))))
                (NORMBF (|ROUND:MT| (|TIMES:| PX X3) |:BPREC:|)))))
      (COND ((COND ((ATOM X2) (MINUSP X2)) (T (|MINUSP:| X2))) (GO THT)))
      (SETQ X0 NX)
      (SETQ XD
              (NORMBF
               (|DIVIDE:|
                ((LAMBDA (G132)
                   (COND ((ATOM PX) (TIMES G132 PX))
                         (T
                          (NORMBF
                           (|ROUND:MT|
                            (|TIMES:|
                             (COND ((FLOATP G132) (FL2BF G132))
                                   (T
                                    (NORMBF
                                     (COND ((NOT (ATOM G132)) G132)
                                           ((FIXP G132)
                                            (CONS '|:RD:| (CONS G132 0)))
                                           (T (|READ:NUM| G132))))))
                             PX)
                            |:BPREC:|)))))
                 (MINUS (TIMES N S)))
                (PLUBF X1
                       (BFSQRT
                        ((LAMBDA (G134)
                           (COND ((ATOM X2) (TIMES G134 X2))
                                 (T
                                  (NORMBF
                                   (|ROUND:MT|
                                    (|TIMES:|
                                     (COND ((FLOATP G134) (FL2BF G134))
                                           (T
                                            (NORMBF
                                             (COND ((NOT (ATOM G134)) G134)
                                                   ((FIXP G134)
                                                    (CONS '|:RD:|
                                                          (CONS G134 0)))
                                                   (T (|READ:NUM| G134))))))
                                     X2)
                                    |:BPREC:|)))))
                         (DIFFERENCE N 1))))
                |:BPREC:|)))
      (SETQ NX (PLUBF X0 XD))
     LG3
      (SETQ FG T)
      (COND
       ((OR (RATLESSP (SETQ XR (REALRAT NX)) (CAR I)) (RATLESSP (CDR I) XR))
        (GO THT)))
      (COND ((BNDTST (SETQ PX (RLVAL P NX))) (GO THT)))
      (MOVEBDS I XR SH)
      (COND (*TRROOT (TRMSG2A 'LAG NX PX)))
      (COND ((BDSTEST I) (GO RET)))
      (COND ((COND ((ATOM PX) (ZEROP PX)) (T (EQUAL (CADR PX) 0))) (GO LG4)))
      (COND
       ((BFEQP NX X0) (PROGN (COND (*TRROOT (TRMSG3A 'LAG NX))) (GO RET))))
      (COND ((AND XNLIST (MEMBER NX XNLIST)) (GO NEWT)))
      (SETQ XNLIST (CONS NX XNLIST))
      (SETQ PF0 PF)
      (COND
       ((OR (LESSP (SETQ M (PLUS M 1)) 10)
            (PROGN
             (SETQ M 0)
             (|EQUAL:| BFONE*
                       (|ROUND:MT|
                        (NORMBF
                         (|DIVIDE:|
                          (COND ((FLOATP NX) (FL2BF NX))
                                (T
                                 (NORMBF
                                  (COND ((NOT (ATOM NX)) NX)
                                        ((FIXP NX) (CONS '|:RD:| (CONS NX 0)))
                                        (T (|READ:NUM| NX))))))
                          (COND ((FLOATP X0) (FL2BF X0))
                                (T
                                 (NORMBF
                                  (COND ((NOT (ATOM X0)) X0)
                                        ((FIXP X0) (CONS '|:RD:| (CONS X0 0)))
                                        (T (|READ:NUM| X0))))))
                          |:BPREC:|))
                        13))))
        (GO LAG)))
     THT
      (SETQ NX (TIGHTEN I P PF SH))
      (SETQ M 0)
      (COND
       (*XNLIST
        (PROGN
         (SETQ PF0 NIL)
         (MOVEBDS I (SETQ XR (RATMEAN (CAR I) (CDR I))) SH)
         (GO LAG0))))
     LG4
      (COND (*TRROOT (TRMSG1A 'LAG NX)))
     RET
      (SETQ *XNLIST NIL)
      (COND ((NOT NX) (COND ((OR *TRROOT *ROOTMSG) (TRMSG10A 'LAG)))))
      (GO RET2)
     NEWT
      (SETQ NX (BFNEWTON P P1 (GFGETMIN) I 4))
     RET2
      (SETQ *XN
              (COND
               (*BFTAG
                (CONS
                 (COND ((FLOATP NX) (FL2BF NX))
                       (T
                        (NORMBF
                         (COND ((NOT (ATOM NX)) NX)
                               ((FIXP NX) (CONS '|:RD:| (CONS NX 0)))
                               (T (|READ:NUM| NX))))))
                 BFZ*))
               (T (CONS (CFLOT NX) 0.0))))
      (RETURN NX))) 
(GLOBAL '(TENTOTHETENTH**)) 
(SETQ TENTOTHETENTH** (NORMBF (CONS '|:RD:| (CONS 10000000000 0)))) 
(PUT 'BNDTST 'NUMBER-OF-ARGS 1) 
(PUT 'BNDTST 'DEFINED-ON-LINE '228) 
(PUT 'BNDTST 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'BNDTST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BNDTST (X) (|GREATERP:| (|ABS:| X) TENTOTHETENTH**)) 
(PUT 'MOVEBDS 'NUMBER-OF-ARGS 3) 
(PUT 'MOVEBDS 'DEFINED-ON-LINE '231) 
(PUT 'MOVEBDS 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'MOVEBDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MOVEBDS (I XR SH)
    (COND ((EQUAL (SGN1 *INTP XR) SH) (RPLACD I XR)) (T (RPLACA I XR)))) 
(PUT 'TIGHTEN 'NUMBER-OF-ARGS 4) 
(PUT 'TIGHTEN 'DEFINED-ON-LINE '234) 
(PUT 'TIGHTEN 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'TIGHTEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TIGHTEN (I P PF SH)
    (PROG (J X0 NX PX SN X)
      (SETQ NX (CAR I))
     THT0
      (SETQ J 4)
     THT1
      (SETQ X0 NX)
      (SETQ NX (RATMEAN (CAR I) (CDR I)))
      (COND
       ((EQUAL (SETQ SN (SGN1 *INTP NX)) 0)
        (PROGN
         (SETQ X (R2FLBF NX))
         (COND (*TRROOT (TRMSG1A 'THT X)))
         (GO RET))))
      (COND
       ((EQUAL 0 (CAR (RATDIF NX X0)))
        (PROGN
         (SETQ X (R2FLBF NX))
         (COND (*TRROOT (TRMSG3A 'THT X)))
         (GO RET))))
      (COND ((EQUAL SN SH) (RPLACD I NX)) (T (RPLACA I NX)))
      (COND ((SETQ SN (BDSTEST I)) (PROGN (SETQ X (R2FLBF SN)) (GO RET))))
      (COND ((GREATERP (SETQ J (DIFFERENCE J 1)) 0) (GO THT1)))
      (COND
       ((BNDTST (SETQ PX (RLVAL P (SETQ X (R2FLBF NX)))))
        (PROGN (SETQ J 4) (GO THT1))))
      (GFSTORVAL (COND ((ATOM PX) (ABS PX)) (T (|ABS:| PX))) X)
      (COND (*TRROOT (TRMSG2A 'THT X PX)))
      (COND ((COND ((ATOM PX) (ZEROP PX)) (T (EQUAL (CADR PX) 0))) (GO RET))
            ((AND PF (BFLEQP PF (COND ((ATOM PX) (ABS PX)) (T (|ABS:| PX)))))
             (GO THT0))
            (T (RETURN X)))
     RET
      (SETQ *XNLIST NIL)
      (RETURN X))) 
(PUT 'RTSREAL 'NUMBER-OF-ARGS 2) 
(PUT 'RTSREAL 'DEFINED-ON-LINE '255) 
(PUT 'RTSREAL 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'RTSREAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RTSREAL (P S)
    (PROG (ACR ACS N Q R X Y *STRM PR APR *BFTAG |PFL#| |ACFL#| XOUT X1 ACCN
           ACCN1 |ACCM#| |PREC#| |PRM#|)
      (SETQ ACCN 0)
      (SETQ ACCN1 0)
      (SETQ |ACCM#| 0)
      (SETQ |PREC#| 0)
      (SETQ |PRM#| 0)
      (SETQ PR (PLUS 2 (PRECISION 0)))
      (SETQ *BFTAG (SETQ |RLRT#| T))
      (SETQ |PGCD#| (NOT S))
      (SETQ R (ISOLATEP P))
      (COND ((NULL R) (GO RET)))
      (COND ((GREATERP (SETQ N (CAAR (LASTPAIR P))) 1) (GO GR1)))
      (SETQ Y (ROOTRND (CAR (GFROOTFIND P NIL))))
      (COND
       (|PFACTOR#|
        (PROGN
         (SETQ Y (ACCUPR1 Y P))
         (SETQ Y (CONS (ROOTRND (CAR Y)) (CDR Y))))))
      (SETQ XOUT
              (LIST
               (COND (S (CONS (MKDN |RLVAL#|) |ACC#|)) (|PFACTOR#| Y)
                     (T (CONS Y |ACC#|)))))
      (COND (*TRROOT (TERPRI)))
      (GO RET)
     GR1
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
      (SETQ Q R)
      (SETQ ACS |ACC#|)
     LAG
      (COND
       ((CDR Q)
        (PROGN
         (PRECISION1 (DIFFERENCE ACS 2) T)
         (PROG ()
          WHILELABEL
           (COND ((NOT (SCHNOK Q)) (RETURN NIL)))
           (PRECISION1 (DIFFERENCE (PLUS (PLUS 2 (PRECISION 0)) 1) 2) T)
           (GO WHILELABEL))
         (SETQ ACCN1 (PLUS 2 (PRECISION 0))))))
      (SETQ |ACC#| (MAX ACS ACCN ACCN1))
      (SETQ ACCN (COND ((GREATERP ACCN1 ACS) ACCN1) (T 0)))
      (PRECISION1 (DIFFERENCE (MAX |RPREC#| (PLUS |ACC#| 2)) 2) T)
      (SETQ Y (ROOTFIND P (SETQ |INTV#| (CAR Q))))
      (SETQ APR T)
      (COND ((NULL Y) (RERROR 'ROOTS 8 "Realroots abort")))
     ACC
      (SETQ Y (ACCUROOT (CAR *XN) P *XO))
      (COND
       (APR
        (PROGN
         (COND
          ((GREATERP (SETQ ACR (ACCUPR P 1RP *XN)) |ACC#|) (SETQ |ACC#| ACR))
          ((LEQ ACR |ACC#|) (PROGN (SETQ |ACC#| ACR) (SETQ APR NIL))))
         (GO ACC))))
      (SETQ XOUT
              (CONS (CONS (SETQ X1 (COND (S (MKDN |RLVAL#|)) (T Y))) |ACC#|)
                    XOUT))
      (COND ((AND X (EQUAL X1 (CAR X))) (ROOTERR X1)))
      (SETQ X (CONS X1 X))
      (DSPLY Y)
      (SETQ |ACC#| ACS)
      (COND ((SETQ Q (CDR Q)) (PROGN (SETQ ACCN1 0) (GO LAG))))
     RET
      (PRECISION1 (DIFFERENCE PR 2) T)
      (RETURN (REVERSE XOUT)))) 
(PUT 'LVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LVAL 'DEFINED-ON-LINE '303) 
(PUT 'LVAL 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LVAL (X) (COND ((EQCAR X 'LIST) (CDR X)) (T X))) 
(PUT 'LPWR 'NUMBER-OF-ARGS 2) 
(PUT 'LPWR 'DEFINED-ON-LINE '305) 
(PUT 'LPWR 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LPWR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LPWR (L M)
    (COND ((EQCAR L 'LIST) (CONS 'LIST (LPWR (CDR L) M))) ((ATOM L) L)
          (T (CONS (EXPT (CAR L) M) (EXPT (CDR L) M))))) 
(PUT 'SCHNOK 'NUMBER-OF-ARGS 1) 
(PUT 'SCHNOK 'DEFINED-ON-LINE '309) 
(PUT 'SCHNOK 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'SCHNOK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCHNOK (R)
    ((LAMBDA (L H)
       (AND (NEQ L H)
            (OR (NEQ (SCH L) (SCH (R2FLBF2R L)))
                (NEQ (SCH H) (SCH (R2FLBF2R H))))))
     (CAAR R) (CDAR R))) 
(PUT 'LIMCHK 'NUMBER-OF-ARGS 1) 
(PUT 'LIMCHK 'DEFINED-ON-LINE '314) 
(PUT 'LIMCHK 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LIMCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMCHK (X)
    (PROGN
     (!MFEFIX)
     (COND
      ((NULL
        (SETQ X
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y X)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (Y)
                                      (COND ((MEMBER Y |LIMLST#|) Y)
                                            ((EQCAR (SETQ Y (REVAL1 Y T))
                                                    'LIST)
                                             (CONS 'LIST
                                                   (LIST (LIMCHK1 (CADR Y)))))
                                            (T (LIMCHK1 Y))))
                                    (CAR Y))
                                   NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (Y)
                              (COND ((MEMBER Y |LIMLST#|) Y)
                                    ((EQCAR (SETQ Y (REVAL1 Y T)) 'LIST)
                                     (CONS 'LIST (LIST (LIMCHK1 (CADR Y)))))
                                    (T (LIMCHK1 Y))))
                            (CAR Y))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       NIL)
      ((AND X (NOT (CDR X)))
       (COND ((EQ (CAR X) 'POSITIVE) (LIST 1))
             ((EQ (CAR X) 'NEGATIVE) (LIST (MINUS 1))) (T (LIMERR))))
      (T (PROGN (SETQ X (MKRATL X)) (LIMCHK2 (CAR X) (CADR X))))))) 
(PUT 'LIMCHK1 'NUMBER-OF-ARGS 1) 
(PUT 'LIMCHK1 'DEFINED-ON-LINE '326) 
(PUT 'LIMCHK1 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LIMCHK1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMCHK1 (Y)
    (COND
     ((ERRORP (SETQ Y (ERRORSET* (LIST 'A2RAT (MKQUOTE Y)) NIL)))
      (RERROR 'ROOTS 5 "Real root function limits must be real"))
     (T (CAR Y)))) 
(PUT 'LIMCHK2 'NUMBER-OF-ARGS 2) 
(PUT 'LIMCHK2 'DEFINED-ON-LINE '331) 
(PUT 'LIMCHK2 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LIMCHK2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LIMCHK2 (A B)
    ((LAMBDA (L)
       (PROGN
        (COND
         ((AND (MEMBER A L) (MEMBER B L)) (COND ((NEQ A B) NIL) (T (LIMERR))))
         ((MEMBER A |LIMLST#|)
          (COND ((MEMBER B |LIMLST#|) (LIMERR)) (T (LIMCHK2 B A))))
         ((MEMBER B |LIMLST#|)
          (COND
           ((EQ B 'NEGATIVE)
            (LIST 'MINFTY (COND ((EQCAR A 'LIST) A) (T (CONS 'LIST A)))))
           ((EQ B 'POSITIVE)
            (LIST (COND ((EQCAR A 'LIST) A) (T (CONS 'LIST A))) 'INFTY))
           ((EQ B 'INFINITY) (LIST A 'INFTY)) (T (LIST 'MINFTY A))))
         ((AND (EQUAL (RATV B) (RATV A)) (OR (EQCAR A 'LIST) (EQCAR B 'LIST)))
          T)
         ((RATLESSP (RATV B) (RATV A)) (LIST B A)) (T (LIST A B)))))
     (CDDR |LIMLST#|))) 
(PUT 'LIMERR 'NUMBER-OF-ARGS 0) 
(PUT 'LIMERR 'DEFINED-ON-LINE '343) 
(PUT 'LIMERR 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'LIMERR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LIMERR NIL (RERROR 'ROOTS 6 "Illegal region specification")) 
(PUT 'RATV 'NUMBER-OF-ARGS 1) 
(PUT 'RATV 'DEFINED-ON-LINE '346) 
(PUT 'RATV 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'RATV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RATV (A) (COND ((EQCAR A 'LIST) (CDR A)) (T A))) 
(PUT 'A2RAT 'NUMBER-OF-ARGS 1) 
(PUT 'A2RAT 'DEFINED-ON-LINE '348) 
(PUT 'A2RAT 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'A2RAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE A2RAT (X)
    (COND ((NUMBERP X) (CONS X 1)) ((ATOM X) (LIMERR))
          ((EQCAR X 'QUOTIENT)
           (CONS
            ((LAMBDA (N)
               (COND ((NUMBERP N) N) ((EQCAR N 'MINUS) (MINUS (CADR N)))
                     (T (RERROR 'ROOTS 10 "illegal limit"))))
             (CADR X))
            (CADDR X)))
          ((EQ (CAR X) '|:RN:|) (CDR X))
          (T
           ((LAMBDA (Y)
              (COND ((AND (MEMQ (CAR X) DOMAINLIST*) Y) (CDR (APPLY1 Y X)))
                    (T (LIMERR))))
            (GET (CAR X) '|:RN:|))))) 
(PUT 'RLROOTNO 'NUMBER-OF-ARGS 1) 
(PUT 'RLROOTNO 'DEFINED-ON-LINE '361) 
(PUT 'RLROOTNO 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'RLROOTNO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLROOTNO (A)
    (PROGN
     (SETQ |MLTR#| T)
     (SETQ |LIMS#| (LIMCHK (CDR A)))
     (SETQ A (CKPREC (CAR A)))
     (SETQ A (RLRTNO2 (COND ((EQUAL |LIMS#| T) 0) (T A))))
     (RESTOREFL)
     (SETQ |MLTR#| NIL)
     A)) 
(PUT 'RLROOTNO 'PSOPFN 'RLROOTNO) 
(PUT 'REALROOTS 'NUMBER-OF-ARGS 1) 
(PUT 'REALROOTS 'DEFINED-ON-LINE '368) 
(PUT 'REALROOTS 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'REALROOTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REALROOTS (A)
    (PROGN
     (SETQ |LIMS#| (LIMCHK (CDR A)))
     (UNIROOTS (COND ((EQUAL |LIMS#| T) 0) (T (CAR A))) 0))) 
(PUT 'REALROOTS 'PSOPFN 'REALROOTS) 
(PUT 'ISOLATER 'NUMBER-OF-ARGS 1) 
(PUT 'ISOLATER 'DEFINED-ON-LINE '374) 
(PUT 'ISOLATER 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'ISOLATER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISOLATER (P)
    (PROGN
     (SETQ |MLTR#| T)
     (SETQ |LIMS#| (LIMCHK (CDR P)))
     (SETQ P (CKPREC (CAR P)))
     (SETQ P (ISOLATEP (COND ((EQUAL |LIMS#| T) 0) (T P))))
     (RESTOREFL)
     (SETQ |MLTR#| NIL)
     (OUTRIL P))) 
(PUT 'ISOLATER 'PSOPFN 'ISOLATER) 
(PUT 'MKRATL 'NUMBER-OF-ARGS 1) 
(PUT 'MKRATL 'DEFINED-ON-LINE '381) 
(PUT 'MKRATL 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'MKRATL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKRATL (L)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A L)
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (A)
                          (COND ((MEMBER A |LIMLST#|) A)
                                ((EQCAR A 'LIST)
                                 (COND ((MEMBER (SETQ A (CADR A)) |LIMLST#|) A)
                                       (T
                                        (COND ((EQCAR A 'LIST) A)
                                              (T (CONS 'LIST A))))))
                                (T A)))
                        (CAR A))
                       NIL)))
     LOOPLABEL
      (SETQ A (CDR A))
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (A)
                  (COND ((MEMBER A |LIMLST#|) A)
                        ((EQCAR A 'LIST)
                         (COND ((MEMBER (SETQ A (CADR A)) |LIMLST#|) A)
                               (T
                                (COND ((EQCAR A 'LIST) A)
                                      (T (CONS 'LIST A))))))
                        (T A)))
                (CAR A))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'EXCLUDE 'NUMBER-OF-ARGS 1) 
(PUT 'EXCLUDE 'DEFINED-ON-LINE '387) 
(PUT 'EXCLUDE 'DEFINED-IN-FILE 'ROOTS/REALROOT.RED) 
(PUT 'EXCLUDE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXCLUDE (X) (LIST 'LIST X)) 
(FLAG '(EXCLUDE) 'OPFN) 
(ENDMODULE) 